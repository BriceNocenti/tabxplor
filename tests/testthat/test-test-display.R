# Phase 16a: the shared summary-statistics framework (R/tab-test-display.R): the formatters, the display
# grid built from the `test` attribute, the console GFM renderer, and the export-side extensions
# (weak-test flag + the tabxplor.test_lines statistic row + split_var kept in exports).

gss <- gss_cat_data_formatting()

# === formatters =====================================================================================

test_that("test_fmt_pvalue / stat / num / is_nonsig / weak-label format as specified", {
  expect_equal(test_fmt_pvalue(c(0.5, 0.001, 1e-6, NA)), c("50%", "0.1%", "<0.01%", NA))
  expect_equal(test_fmt_stat(1911, 6, NA), "1911 (df 6)")
  expect_equal(test_fmt_stat(127, 2, 2029.3), "127 (df 2; 2029)")
  expect_equal(test_fmt_stat(9.78, 8, 486), "9.78 (df 8; 486)")
  expect_equal(test_fmt_num(21483), "21 483")
  expect_equal(test_is_nonsig(c(0.04, 0.06, NA)), c(FALSE, TRUE, FALSE))
  # weak-test flag: min_e < 5 appends " !" (chi2 only; F has NA min_e)
  expect_equal(test_cell_label_weak("chi2", 3),   "Chi2 !")
  expect_equal(test_cell_label_weak("chi2", 10),  "Chi2")
  expect_equal(test_cell_label_weak("F_welch", NA_real_), "F, Welch")
  expect_equal(test_pvalue_label("chi2", 3), "(Chi2 !)")
})

# === the display grid ===============================================================================

test_that("crosstab grid: col_vars are columns, row_vars are groups, N/statistic/pvalue rows", {
  t <- tab(gss, c(race, relig), c(party3, tvhours), pct = "row", test = TRUE)
  g <- test_summary_grid(t)
  expect_equal(g$stat_header, "Tests")
  expect_setequal(g$value_headers, c("party3", "tvhours"))
  expect_length(g$groups, 2L)                              # race, relig
  expect_equal(g$groups[[1]]$label_lines[[1]], "race")
  expect_equal(vapply(g$groups[[1]]$rows, `[[`, character(1), "label"),
               c("N", "statistic", "pvalue"))
})

test_that("comp='tab' adds a tab_var column; comp='all' collapses the group label to row x tabvars", {
  gt <- test_summary_grid(tab(gss, relig, party3, race, pct = "row", test = TRUE, comp = "tab"))
  expect_true("race" %in% gt$label_headers)               # the tab_var column
  expect_length(gt$groups, 3L)                            # White / Black / Other

  ga <- test_summary_grid(tab(gss, relig, party3, race, pct = "row", test = TRUE, comp = "all"))
  expect_length(ga$groups, 1L)
  expect_match(ga$groups[[1]]$label_lines[[1]], "relig .* race")   # "relig x race"
})

test_that("reg grid: Model fit header, dependent-named columns, a shared predictors column", {
  skip_if_not_installed("broom")
  g <- test_summary_grid(tab_reg(gss, c("married", "income25k"), c("relig", "age")))
  expect_equal(g$stat_header, "Model fit")
  expect_setequal(g$value_headers, c("married", "income25k"))
  expect_true("predictors" %in% g$label_headers)
  expect_true(any(vapply(g$groups[[1]]$rows, `[[`, character(1), "label") == "N"))
})

test_that("reg grid: split_var levels become the row groups", {
  skip_if_not_installed("broom")
  g <- test_summary_grid(tab_reg(gss, "married", c("relig", "age"), split_var = "race"))
  expect_length(g$groups, 3L)
  expect_setequal(vapply(g$groups, function(gr) gr$label_lines[[1]], character(1)),
                  c("White", "Black", "Other"))
})

# === console renderer ===============================================================================

test_that("console renderer emits an aligned GFM pipe table with the weak-test flag", {
  withr::local_options(cli.num_colors = 1)               # strip ANSI so widths are exact
  t   <- tab(gss, relig, party3, pct = "row", test = TRUE)   # relig x party3: min_e < 5 -> weak
  out <- capture.output(test_render_console(test_summary_grid(t)))
  expect_true(any(grepl("^\\|.*party3", out)))           # a header row naming the col_var
  expect_true(any(grepl("^\\|:-", out)))                 # the GFM alignment row
  expect_true(any(grepl("pvalue", out)))
  expect_true(any(grepl("\\(Chi2 !\\)", out)))           # weak flag rendered
  # every rendered row is the same visible width (aligned)
  body <- out[grepl("^\\|", out)]
  expect_equal(length(unique(nchar(body))), 1L)
})

test_that("console renderer colours a non-significant p-value red (cli)", {
  withr::local_options(cli.num_colors = 256)
  # a small sample -> some non-significant test
  set.seed(1); d <- gss[sample(nrow(gss), 150), ]
  t   <- suppressWarnings(tab(d, marital, tvhours, pct = "row", test = TRUE))
  out <- paste(capture.output(test_render_console(test_summary_grid(t))), collapse = "\n")
  # if the F test is non-significant, the p-value cell carries an ANSI red escape
  g <- test_summary_grid(t)
  nonsig <- any(vapply(g$groups, function(gr)
    any(gr$rows[[3]]$nonsig), logical(1)))
  if (nonsig) expect_match(out, "\033\\[")               # an ANSI escape is present
})

# === exports: default unchanged, test_lines='stat' adds a statistic row =============================

test_that("export default = one p-value row; test_lines='stat' adds a statistic row above it", {
  t  <- tab(gss, relig, party3, pct = "row", test = TRUE)
  md <- paste(tab_md(t, print = FALSE), collapse = "\n")
  expect_true(grepl("\\| pvalue", md))
  expect_false(grepl("\\| statistic", md))

  withr::local_options(tabxplor.test_lines = "stat")
  md2 <- paste(tab_md(t, print = FALSE), collapse = "\n")
  expect_true(grepl("\\| statistic", md2))
  expect_true(grepl("\\| pvalue",    md2))
})

# === split_var appears in the html/Excel export =====================================================

test_that("a regression split_var renders as a merged, vertical first column in HTML", {
  skip_if_not_installed("broom")
  r <- tab_reg(gss, "married", c("relig", "age"), split_var = "race")
  h <- as.character(tab_kable(r, engine = "html"))
  # each split level is one rowspan cell with the vertical (tx-vname) class
  for (lv in c("White", "Black", "Other"))
    expect_match(h, sprintf('<td[^>]*tx-vname[^>]*rowspan="[0-9]+">%s', lv))
})

# === the test schema no longer carries the vestigial `variance` column ==============================

test_that("the `test` attribute schema dropped the dead `variance` column", {
  t <- tab(gss, marital, race, pct = "row", test = TRUE)
  expect_false("variance" %in% names(get_test(t)))
  expect_true(all(c("statistic", "df1", "df2", "pvalue", "n", "min_e") %in% names(get_test(t))))
})
