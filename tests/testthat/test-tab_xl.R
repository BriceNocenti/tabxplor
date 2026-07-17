testthat::test_that("tab_xl creates an Excel file", {
  testthat::skip_if_not_installed("openxlsx2")
  tabs <-
    purrr::pmap(
      tibble::tribble(
        ~row_var, ~col_vars       , ~pct , ~filter              , ~subtext               ,
        "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
        "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
        NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
      ),
      .f = tab_many,
      data = forcats::gss_cat, color = "auto", chi2 = TRUE)

  test_path <- file.path(tempdir(), "tab_xl_test.xlsx")

  tabs %>%
    tab_xl(path = test_path, sheets = "unique",
           replace = TRUE, open = FALSE) %>%
    testthat::expect_invisible()

 testthat::expect_true(file.exists(test_path))

 file.remove(test_path)
})

testthat::test_that("tab_xl work with  after_ci", {
  testthat::skip_if_not_installed("openxlsx2")
  withr::local_options(lifecycle_verbosity = "quiet")
  tabs <-tab(forcats::gss_cat, race, marital, pct = "row", color = "after_ci")

  test_path <- file.path(tempdir(), "tab_xl_test.xlsx")

  tabs %>%
    tab_xl(path = test_path, sheets = "unique",
           replace = TRUE, open = FALSE) %>%
    testthat::expect_invisible()

  testthat::expect_true(file.exists(test_path))

  file.remove(test_path)
})

# Phase 10g: read the written workbook back and confirm the raw get_num() values reached the file
# (Excel stores the raw value; the "%" is a display-only numFmt). Closes the "no test inspects the
# written file" gap.
testthat::test_that("tab_xl writes get_num() values that round-trip from the file", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  testthat::expect_true(file.exists(p))

  wb <- openxlsx2::wb_load(p)
  testthat::expect_gte(length(openxlsx2::wb_get_sheet_names(wb)), 1)

  d    <- openxlsx2::wb_to_df(p, sheet = 1, col_names = FALSE, convert = TRUE)
  vals <- round(suppressWarnings(as.numeric(unlist(d, use.names = FALSE))), 6)
  vals <- vals[!is.na(vals)]

  fmt_names <- names(tb)[purrr::map_lgl(tb, is_fmt)]
  want <- round(get_num(tb[[fmt_names[[1]]]]), 6)
  want <- want[!is.na(want)]
  testthat::expect_true(all(want %in% vals))
})

# Phase 10g: a non-tabxplor data.frame degrades gracefully (plain sheet + message, still writes).
testthat::test_that("tab_xl degrades to a plain sheet for a non-tabxplor data.frame", {
  testthat::skip_if_not_installed("openxlsx2")
  p <- withr::local_tempfile(fileext = ".xlsx")
  testthat::expect_message(
    tab_xl(tibble::tibble(a = 1:3, b = letters[1:3]), path = p, open = FALSE),
    "skipped"
  )
  testthat::expect_true(file.exists(p))
})

# Phase 10h / bug-fix: significance stars are folded into the Excel numFmt code (0.0%"***"), keeping
# the cell a real number. STORAGE-driven (like the console): a table built with stars = TRUE carries
# a per-cell pvalue -> star literals; the opt-out default (stars = FALSE) writes none.
testthat::test_that("tab_xl folds significance stars into the numFmt code", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff",
            color_signif = "guaranteed_effect", stars = TRUE)
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  codes <- openxlsx2::wb_load(p)$styles_mgr$styles$numFmts
  testthat::expect_true(any(grepl("\\*", codes)))                 # a code carries the star literal

  # a table built without stars stores no pvalue -> no star literal
  tb2 <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff",
             color_signif = "guaranteed_effect", stars = FALSE)
  p2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb2, path = p2, sheets = "unique", replace = TRUE, open = FALSE))
  codes2 <- openxlsx2::wb_load(p2)$styles_mgr$styles$numFmts
  testthat::expect_false(any(grepl("\\*", codes2)))
})

# Phase 10h: transpose = TRUE exports the transposed table (still a valid, readable workbook).
testthat::test_that("tab_xl(transpose = TRUE) writes a valid workbook", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, replace = TRUE, open = FALSE, transpose = TRUE))
  testthat::expect_true(file.exists(p) && file.size(p) > 0)
})

# Phase 10h: conditional_format is experimental (message + falls back to hard styles, still writes).
testthat::test_that("tab_xl(conditional_format = TRUE) informs and falls back", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  testthat::expect_message(
    tab_xl(tb, path = p, replace = TRUE, open = FALSE, conditional_format = TRUE),
    "experimental"
  )
  testthat::expect_true(file.exists(p))
})

# Phase 13c-v: Excel value/format + col_var spanning header.

testthat::test_that("ci = 'cell' exports the CI text (not the raw proportion)", {
  testthat::skip_if_not_installed("openxlsx2")
  t   <- tab(forcats::gss_cat, marital, race, pct = "row", ci = "cell")
  tmp <- tempfile(fileext = ".xlsx"); tab_xl(t, path = tmp, open = FALSE, replace = TRUE)
  df  <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)
  testthat::expect_true(any(grepl("\\[[0-9]+;[0-9]+\\]", as.matrix(df))))   # a "[lo;hi]" bracket
})

testthat::test_that("OR exports as 1/x text by default, numbers with or_numeric = TRUE", {
  testthat::skip_if_not_installed("openxlsx2")
  testthat::skip_if_not_installed("broom")
  d  <- forcats::gss_cat
  d$married <- factor(ifelse(d$marital == "Married", "yes", "no"))
  tl <- tab_logit(d, "married", c("race", "relig"))
  tmp <- tempfile(fileext = ".xlsx"); tab_xl(tl, path = tmp, open = FALSE, replace = TRUE)
  or_col <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)[[2]]
  testthat::expect_true(any(grepl("1/", or_col, fixed = TRUE)))             # reciprocal text present
  tmp2 <- tempfile(fileext = ".xlsx"); tab_xl(tl, path = tmp2, open = FALSE, replace = TRUE, or_numeric = TRUE)
  or_col2 <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp2), col_names = FALSE)[[2]]
  num <- suppressWarnings(as.numeric(or_col2))
  testthat::expect_true(any(!is.na(num) & num > 0))                        # real numbers now
})

testthat::test_that("numeric vars export a mean + separate sd column, named by the statistic", {
  testthat::skip_if_not_installed("openxlsx2")
  t   <- tab_num(forcats::gss_cat, race, c(age, tvhours), digits = 1L)
  tmp <- tempfile(fileext = ".xlsx"); tab_xl(t, path = tmp, open = FALSE, replace = TRUE)
  df   <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)
  span <- as.character(df[2, ])   # Phase 13c-iii col_var spanning-name row (row 1 = the title)
  hdr  <- as.character(df[3, ])   # level-header row
  # Phase 14d: the variable name is said ONCE, by the span; the level headers say which statistic
  # (they used to repeat it: "age" / "age_sd" under an "age" span).
  testthat::expect_true(all(c("age", "tvhours") %in% span))
  testthat::expect_equal(sum(hdr == "mean", na.rm = TRUE), 2L)
  testthat::expect_equal(sum(hdr == "sd",   na.rm = TRUE), 2L)
  testthat::expect_false(any(c("age_sd", "tvhours_sd") %in% hdr))
})

testthat::test_that("Excel gets a col_var spanning-name row + suffix-stripped level labels", {
  testthat::skip_if_not_installed("openxlsx2")
  d <- forcats::gss_cat
  d$grp <- factor(ifelse(d$age < 40, "Young", "Other"))
  t   <- tab(d, row_vars = marital, col_vars = c(race, grp), pct = "row")
  tmp <- tempfile(fileext = ".xlsx"); tab_xl(t, path = tmp, open = FALSE, replace = TRUE)
  df  <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)
  span_row <- as.character(df[2, ]); hdr_row <- as.character(df[3, ])
  testthat::expect_true(all(c("race", "grp") %in% span_row))               # spanning names row
  testthat::expect_true("Other" %in% hdr_row)                              # suffix stripped
  testthat::expect_false(any(hdr_row == "Other_race", na.rm = TRUE))
})

# === SECTION: the label column -- merge + rotation + the title (Phase 14i) ===

testthat::test_that("tab_xl: a merged table names each row-variable once, merged and rotated", {
  testthat::skip_if_not_installed("openxlsx2")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(
    tab_xl(tab(forcats::gss_cat, c(race, marital), relig, pct = "row"), path = tmp, open = FALSE, replace = TRUE)
  )
  wb <- openxlsx2::wb_load(tmp)
  d  <- openxlsx2::wb_to_df(wb, col_names = FALSE)

  # row 1 = title, row 2 = the col_var span, row 3 = level headers, data from row 4 (Phase 13c-iii)
  # Phase 14i: the title names the SOURCE row_vars -- it read "levels by relig" (the merge's own
  # scaffolding column) because the prep dropped `vars$row_vars`.
  testthat::expect_equal(as.character(d[1, 1]), "race, marital by relig")
  # one merge per block, in column A
  merges <- paste(wb$worksheets[[1]]$mergeCells, collapse = " ")
  testthat::expect_match(merges, 'ref="A4:A7"', fixed = TRUE)
  testthat::expect_match(merges, 'ref="A8:A14"', fixed = TRUE)
  # the name is written once per block, not on every row (Excel keeps only a merge's top-left value,
  # so a repeat below it would be an invisible ghost the user finds again on unmerging)
  testthat::expect_equal(as.character(d[4:14, 1]),
                         c("race", rep(NA, 3), "marital", rep(NA, 6)))
  # rotated 90 degrees, and the column narrowed to match (that is what the rotation buys)
  testthat::expect_true(any(grepl('textRotation="90"', wb$styles_mgr$styles$cellXfs)))
  testthat::expect_match(paste(unlist(wb$worksheets[[1]]$cols_attr), collapse = " "),
                         '<col min="1" max="1"[^/]*width="4', perl = TRUE)
  # the literal "row_var" header is gone
  testthat::expect_true(is.na(d[3, 1]) || !nzchar(as.character(d[3, 1])))
})

testthat::test_that("tab_xl: a one-row block falls back to horizontal, with no merge", {
  testthat::skip_if_not_installed("openxlsx2")
  # Excel rejects a 1-cell "merge", and a rotated 1-row cell would only force a tall row. This also
  # drives label_merges' empty-tibble path (a size-1 `col` recycled to zero rows).
  one <- tab(forcats::gss_cat, c(race, marital), relig, pct = "row") |>
    dplyr::filter(!!rlang::sym("levels") == "Total")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(one, path = tmp, open = FALSE, replace = TRUE))
  wb <- openxlsx2::wb_load(tmp)
  testthat::expect_no_match(paste(wb$worksheets[[1]]$mergeCells, collapse = " "), "A", fixed = TRUE)
  testthat::expect_false(any(grepl('textRotation="90"', wb$styles_mgr$styles$cellXfs)))
  # each block is its own run, so both names are still written
  testthat::expect_equal(as.character(openxlsx2::wb_to_df(wb, col_names = FALSE)[4:5, 1]),
                         c("race", "marital"))
})

testthat::test_that("tab_xl: var_names = 'none' drops both name annotations", {
  testthat::skip_if_not_installed("openxlsx2")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(
    tab_xl(tab(forcats::gss_cat, c(race, marital), relig, pct = "row"), path = tmp, open = FALSE,
           replace = TRUE, var_names = "none")
  )
  wb <- openxlsx2::wb_load(tmp)
  d  <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  # no name column -> no merge at all; and no span row, so the header climbs to row 2
  testthat::expect_length(wb$worksheets[[1]]$mergeCells, 0L)
  testthat::expect_equal(as.character(d[2, 1]), "levels")
  testthat::expect_false(any(grepl('textRotation="90"', wb$styles_mgr$styles$cellXfs)))
})
