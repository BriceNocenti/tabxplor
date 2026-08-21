# Phase 13c-i: composite-token alignment, ratio x/div display, ratio tooltip fix.
# (source stays ASCII: the multiply/divide glyphs are written as \u escapes, per the package rule.)

mult_glyph <- intToUtf8(0x00d7)  # multiply sign
div_glyph  <- intToUtf8(0x00f7)  # divide sign

testthat::test_that("composite {} tokens are padded to a uniform width per column", {
  x <- set_display(
    fmt(n = c(17L, 5416L, 743L), scale = "level_pct", pct_type = "row", pct = c(1, 1, 1), display = "pct"),
    "{pct} (n={n})")
  f <- format(x)
  # the {n} field is right-aligned to the column-max width so numbers line up in a monospace font.
  testthat::expect_identical(f, c("100% (n=   17)", "100% (n=5 416)", "100% (n=  743)"))
  testthat::expect_length(unique(nchar(f)), 1L)
})

testthat::test_that("format() stays byte-identical (no primary_nchar attr) when bold_split is off", {
  x <- set_display(fmt(n = c(10L, 20L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.6), display = "pct"),
                   "{pct} (n={n})")
  testthat::expect_null(attr(format(x), "primary_nchar"))
  testthat::expect_identical(format(x), c("40% (n=10)", "60% (n=20)"))
})

testthat::test_that("ratio (rr) display shows the multiplicative x / div sign", {
  x <- set_display(
    fmt(n = rep(1L, 4), scale = "level_pct", pct_type = "row", pct = rep(0.5, 4),
        ratio = c(2, 0.5, 1, 0.998), display = "pct"),
    "rr")
  f <- format(x)
  # >= 1 -> "x<r>"; < 1 -> "/<1/r>"; a value ROUNDING to the neutral takes the over glyph, never the
  # confusing "/1.00". Only a REFERENCE cell loses the glyph (see the next test).
  testthat::expect_identical(
    f, c(paste0(mult_glyph, "2.00"), paste0(div_glyph, "2.00"),
         paste0(mult_glyph, "1.00"), paste0(mult_glyph, "1.00")))
})

testthat::test_that("a REFERENCE cell at the neutral prints a bare 1, a cell that merely equals it does not", {
  t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "ratio", display = "ratio")
  col <- t[["Divorced"]]
  f   <- format(col, special_formatting = TRUE)
  # the Total row IS the reference: no glyph, no decimals, so its row stands out
  testthat::expect_identical(f[is_totrow(col)], "1")
  # a non-reference cell rounding to the neutral keeps the glyph and the decimals
  testthat::expect_true(any(f[!is_totrow(col)] == paste0(mult_glyph, "1.00")))
  # and a regression Constant IS a reference row, but its odds ratio is a real baseline value
  skip_if_not_installed("broom")
  reg <- suppressMessages(tab_reg(gss_cat_data_formatting(), "married", "race",
                                  family = "binomial"))
  cst <- format(reg[["Model_OR"]], special_formatting = TRUE)[as.character(reg$var) == "Constant"]
  testthat::expect_false(cst == "1")
})

testthat::test_that("a multiplicative cell keeps the decimals it ASKS for, and 0 takes the floor", {
  # DISPLAY_TOKENS$min_digits overrides ONLY 0: a ratio read against the x1.2 / x1.5 thresholds is
  # meaningless at "1", but a cell asking for 1 or 3 decimals gets exactly that.
  x <- set_display(
    fmt(n = rep(1L, 3), scale = "level_pct", pct_type = "row", pct = rep(0.5, 3),
        ratio = c(1.5, 0.25, 1.0624), display = "pct"),
    "rr")
  m <- mult_glyph; d <- div_glyph
  testthat::expect_identical(format(set_digits(x, 0L)),
                             c(paste0(m, "1.50"), paste0(d, "4.00"), paste0(m, "1.06")))
  testthat::expect_identical(format(set_digits(x, 1L)),
                             c(paste0(m, "1.5"), paste0(d, "4.0"), paste0(m, "1.1")))
  testthat::expect_identical(format(set_digits(x, 3L)),
                             c(paste0(m, "1.500"), paste0(d, "4.000"), paste0(m, "1.062")))
  # the same rule on the odds-ratio glyph, and the same floor
  y <- fmt(n = rep(1L, 3), scale = "odds_ratio", pct_type = "row",
           or = c(2, 0.5, 1.0624), display = "or")
  testthat::expect_identical(format(set_digits(y, 0L), special_formatting = TRUE),
                             c("2.00", "1/2.00", "1.06"))
  testthat::expect_identical(format(set_digits(y, 1L), special_formatting = TRUE),
                             c("2.0", "1/2.0", "1.1"))
})

# Phase 13c-ii: composite partial bold (first token bold, rest plain).

testthat::test_that("md bolds only the primary field of a composite cell in a bold row", {
  t  <- tab(forcats::gss_cat, marital, race, pct = "row", n = "range")
  md <- tab_md(t, color = FALSE, print = FALSE, css = FALSE)
  # the bold Total row's composite: pct bold, "(...)" plain -> "**100%** (...)". Phase g (A6): the
  # join is a non-breaking space (U+00A0) so html does not wrap the composite.
  nbsp <- intToUtf8(160L)
  testthat::expect_match(md, paste0("\\*\\*100%\\*\\*", nbsp, "\\("))
  # NOT whole-cell bold ("**100% (...)**")
  testthat::expect_false(grepl(paste0("\\*\\*100%", nbsp, "\\([0-9 ]+\\)\\*\\*"), md))
})

testthat::test_that("html bolds only the primary field of a composite bold cell", {
  t <- tab(forcats::gss_cat, marital, race, pct = "row", n = "range")
  h <- as.character(tab_kable(t))
  # a normal-weight span wraps the "(...)" suffix of the bold composite cells
  testthat::expect_true(grepl("font-weight: ?normal", h))
})

testthat::test_that("kable tooltip shows the ratio field (not OR) under a 'ratio:' label", {
  t <- tab(forcats::gss_cat, marital, race, pct = "row", color = c("diff", "ratio"))
  tt <- tabxplor:::tab_tooltip_text(t[[2]])
  testthat::expect_true(any(grepl("ratio:", tt, fixed = TRUE)))
  testthat::expect_false(any(grepl("rr:", tt, fixed = TRUE)))
  # the ratio value is present (x or div sign), not an empty field.
  testthat::expect_true(any(grepl(paste0(mult_glyph, "|", div_glyph), tt)))
})

# Phase 13c-iii: col_var spanning headers + level-name suffix stripping.

testthat::test_that("shared header model: spanning labels + suffix-stripped clean names", {
  d <- forcats::gss_cat
  d$grp <- factor(ifelse(d$age < 40, "Young", "Other"))   # "Other" collides with race "Other"
  t <- tab(d, row_vars = marital, col_vars = c(race, grp), pct = "row")
  cvh <- tabxplor:::tab_export_prep(t, backend = "md", compute = "refs",
                                    list_method = TRUE)$tables[[1]]$col_var_header
  testthat::expect_true("Other_race" %in% names(t))        # stored uniquely
  testthat::expect_false(any(cvh$clean == "Other_race"))   # but shown clean
  testthat::expect_equal(sum(cvh$clean == "Other"), 2L)    # both collisions -> "Other"
  testthat::expect_identical(cvh$label[names(t) == "Total"], "")  # Total stands alone
})

testthat::test_that("md/kable/html show the col_var name spanning header (single col_var too)", {
  t  <- tab(forcats::gss_cat, marital, race, pct = "row")
  md <- tab_md(t, color = FALSE, print = FALSE, css = FALSE)   # css = FALSE: line 3 is the name row
  # Phase 14f: in markdown the name is the first BODY row (line 3: header, delimiter, then it). Above
  # the delimiter it was a second header row, which pandoc does not accept -- it rejected the whole
  # table. Locked by "tab_md() output is valid pandoc" in test-tab_md.R.
  lines <- strsplit(md, "\n")[[1]]
  testthat::expect_match(lines[3], "race")
  testthat::expect_match(lines[3], "[*]race[*]", perl = TRUE)   # italic: it reads as a sub-heading
  testthat::expect_no_match(lines[1], "race")
  hh <- as.character(tab_kable(t))
  testthat::expect_match(hh, 'colspan="3"[^>]*>race<')         # html engine colspan cell
})

# Phase 13c-iv: tabxplor_tabs list class + Viewer routing.

testthat::test_that("tab(output_list = TRUE) returns a tabxplor_tabs that behaves like a list", {
  t <- tab(forcats::gss_cat, c(marital, relig), race, pct = "row", output_list = TRUE)
  testthat::expect_s3_class(t, "tabxplor_tabs")
  testthat::expect_true(is.list(t))
  testthat::expect_length(t, 2L)
  testthat::expect_s3_class(t[[1]], "tabxplor_tab")            # [[ -> bare tab
  testthat::expect_false(inherits(t[[1]], "tabxplor_tabs"))
  testthat::expect_s3_class(t[1], "tabxplor_tabs")             # [ keeps the class
  testthat::expect_length(purrr::map(t, nrow), 2L)            # map / lapply work
})

testthat::test_that("a single tab is returned bare (not wrapped in tabxplor_tabs)", {
  t <- tab(forcats::gss_cat, marital, race, pct = "row")
  testthat::expect_false(inherits(t, "tabxplor_tabs"))
  testthat::expect_s3_class(t, "tabxplor_tab")
})

testthat::test_that("tab_kable(list) routes to the Viewer (kableExtra class) with joined tables", {
  t <- tab(forcats::gss_cat, c(marital, relig), race, pct = "row", output_list = TRUE)
  k <- tab_kable(t)
  testthat::expect_s3_class(k, "kableExtra")                  # print.kableExtra -> Viewer
  testthat::expect_true(grepl("<table", as.character(k)))
})

testthat::test_that("print.tabxplor_tabs honours options(tabxplor.print)", {
  t <- tab(forcats::gss_cat, c(marital, relig), race, pct = "row", output_list = TRUE)
  withr::local_options(tabxplor.print = "kable")
  out <- capture.output(print(t))
  testthat::expect_true(any(grepl("<table", out)))           # kable mode -> html tables
})
