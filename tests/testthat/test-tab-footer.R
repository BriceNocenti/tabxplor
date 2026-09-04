# The REGION under a table: the subtext template, the placeholders, the three kinds and where each
# medium puts them. The SENTENCE the `<legend>` placeholder builds is test-tab-color.R's.

testthat::test_that("tab() stores the default template, and it renders today's footer", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  testthat::expect_identical(get_subtext(t),
                             c("<weight>", "<model>", "<interaction>", "<legend>", "<stars>"))

  # a user line takes the `user` row's slot -- last, as it always did
  t2 <- tab(fx_gss(), race, marital, pct = "row", color = "diff", subtext = "Field: GSS")
  testthat::expect_identical(utils::tail(get_subtext(t2), 1L), "Field: GSS")
  testthat::expect_identical(utils::tail(tab_footer_text(t2), 1L), "Field: GSS")
})


testthat::test_that("the template decides the order, and dropping <legend> drops the legend", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff", wt = tvhours)
  has_legend <- function(x) any(grepl("difference", tab_footer_text(x), fixed = TRUE))
  has_weight <- function(x) any(grepl("Weighted by",  tab_footer_text(x), fixed = TRUE))

  testthat::expect_true(has_legend(t) && has_weight(t))

  # re-ordered: the weight line now comes second
  t2 <- set_subtext(t, c("<legend>", "<weight>"))
  testthat::expect_true(grepl("difference",  tab_footer_text(t2)[[1]], fixed = TRUE))
  testthat::expect_true(grepl("Weighted by", tab_footer_text(t2)[[2]], fixed = TRUE))

  # dropped: no legend anywhere, INCLUDING the console, which no argument can do
  t3 <- set_subtext(t, "<weight>")
  testthat::expect_false(has_legend(t3))
  testthat::expect_false(any(grepl("difference", cli::ansi_strip(print(t3, get_text = TRUE)),
                                   fixed = TRUE)))
  testthat::expect_false(any(grepl("difference", tab_md(t3, css = FALSE, print = FALSE),
                                   fixed = TRUE)))
})


testthat::test_that("a subtext naming no placeholder is APPENDED, and unknown <...> claims nothing", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  keeps_footer <- function(x) any(grepl("difference", tab_footer_text(x), fixed = TRUE))

  # the safety net: a raw overwrite (what a hand-built table and jamovi's free-text box do)
  raw <- t; attr(raw, "subtext") <- "my note"
  testthat::expect_true(keeps_footer(raw))
  testthat::expect_identical(utils::tail(tab_footer_text(raw), 1L), "my note")

  # raw html, a comparison and a bracketed level are NOT placeholders: verbatim, and no claim
  odd <- set_subtext(t, c("<b>bold</b>", "n < 30", "<30 ans>"))
  testthat::expect_true(keeps_footer(odd))
  testthat::expect_true(all(c("<b>bold</b>", "n < 30", "<30 ans>") %in% tab_footer_text(odd)))

  # ...and a backslash escapes a literal `<`
  esc <- set_subtext(t, "literal \\<legend>")
  testthat::expect_true("literal <legend>" %in% tab_footer_text(esc))
})


testthat::test_that("the inline placeholders are built from the cells' own facts", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")

  # <breaks> IS the ladder the cells are painted with -- never a pasted copy of it
  brk  <- get_color_breaks()[["pct_diff"]]
  line <- tab_footer_text(set_subtext(t, "ladder: <breaks>"))
  line <- line[grepl("^ladder", line)]
  for (b in brk) testthat::expect_true(grepl(paste0("+", b * 100), line, fixed = TRUE))
  # one side only
  up <- tab_footer_text(set_subtext(t, "up: <breaks:over>"))
  up <- up[grepl("^up:", up)]
  testthat::expect_true(grepl("+5", up, fixed = TRUE))
  testthat::expect_false(grepl("-5", up, fixed = TRUE))

  # <conf> and <cols>, and an inline token NEVER claims the layout
  cf <- set_subtext(t, "at <conf>")
  testthat::expect_true("at 95%" %in% tab_footer_text(cf))
  testthat::expect_true(any(grepl("difference", tab_footer_text(cf), fixed = TRUE)))
  testthat::expect_true(any(grepl("marital", tab_footer_text(set_subtext(t, "on <cols>")),
                                  fixed = TRUE)))
})


testthat::test_that("<legend:terse|prose> pins the register on the table", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  terse <- tab_footer_text(set_subtext(t, "<legend:terse>"))
  prose <- tab_footer_text(set_subtext(t, "<legend:prose>"))
  testthat::expect_true(grepl("^difference", terse[[1]]))
  testthat::expect_true(grepl("^Percentage points", prose[[1]]))
  testthat::expect_gt(nchar(prose[[1]]), nchar(terse[[1]]))
})


testthat::test_that("a table stripped of its attributes keeps what its COLUMNS can say", {
  # the degradation contract: each FOOTER_BLOCKS row is gated on what it `reads`, so a stripped table
  # keeps the column-derived half (the colour legend, the stars key) and drops the table-derived half
  # (weight / Model: / the shape table / the subordinate tables) -- with no exception handling.
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff", wt = tvhours)
  bare <- t
  for (a in c("subtext", "meta")) attr(bare, a) <- NULL

  testthat::expect_true(any(grepl("difference", tab_footer_text(bare), fixed = TRUE)))
  testthat::expect_false(any(grepl("Weighted by", tab_footer_text(bare), fixed = TRUE)))
  for (m in c("plain", "console", "html", "md"))
    testthat::expect_no_error(tab_footer_text(bare, medium = m))
  testthat::expect_no_error(tab_md(bare, css = FALSE, print = FALSE))
  testthat::expect_no_error(tab_html(bare))
})


testthat::test_that("the pre-2.0.1 carrier surface still works (CRAN ggfacto 0.3.2)", {
  # a table hand-built the old way: new_tab(subtext = <character>), a raw attr()<- , and the three
  # meta setters. Nothing here may need the template to exist.
  n <- 3L
  col <- fmt(n = rep(100L, n), pct = c(0.2, 0.5, 0.3), scale = "level_pct", pct_type = "col",
             col_var = "v", color = "no", digits = 0L)
  df  <- tibble::tibble(lv = new_lvl(factor(c("a", "b", "c")), role = "level"), v = col)
  old <- new_tab(df, subtext = "hand-written legend")
  testthat::expect_true(is_tab(old))
  testthat::expect_true("hand-written legend" %in% tab_footer_text(old))

  attr(old, "subtext") <- c("line one", "line two")
  testthat::expect_true(all(c("line one", "line two") %in% tab_footer_text(old)))

  old <- set_footer_tabs(old, list("Base" = new_tab(df)))
  old <- set_bars(old, "v")
  testthat::expect_no_error(tab_kable(old))
  testthat::expect_no_error(tab_md(old, css = FALSE, print = FALSE))
  testthat::expect_no_error(print(old, get_text = TRUE))
})
