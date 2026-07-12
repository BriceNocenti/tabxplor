# Phase 10j: the tab_export() facade + the unified exporter arguments (color / transpose /
# color_legend / caption). The four exporters keep their own snapshot/parity tests; this file locks
# the shared surface.

t_row <- tab(forcats::gss_cat, race, marital, pct = "row", color = c("diff", "ratio"))

testthat::test_that("tab_export() dispatches to each format", {
  testthat::expect_no_error(as.character(tab_export(t_row, "kable")))
  testthat::expect_type(tab_export(t_row, "md", print = FALSE), "character")

  f <- tempfile(fileext = ".xlsx")
  tab_export(t_row, "xl", path = f, open = FALSE, replace = TRUE)
  testthat::expect_true(file.exists(f) && file.size(f) > 0)

  if (requireNamespace("ggpubr", quietly = TRUE) &&
      requireNamespace("cowplot", quietly = TRUE)) {
    testthat::expect_no_error(tab_export(t_row, "plot"))
  }
})

testthat::test_that("color = FALSE renders monochrome (no colour spans)", {
  col <- tab_md(t_row, color = TRUE,  print = FALSE)
  mon <- tab_md(t_row, color = FALSE, print = FALSE)
  testthat::expect_true(grepl("]{.", col, fixed = TRUE))   # coloured pandoc spans present
  testthat::expect_false(grepl("]{.", mon, fixed = TRUE))  # none when monochrome
})

testthat::test_that("transpose swaps the table axes at export", {
  t_col <- tab(forcats::gss_cat, race, marital, pct = "col", color = "diff")
  base  <- tab_md(t_col, print = FALSE)
  trans <- tab_md(t_col, transpose = TRUE, print = FALSE)
  testthat::expect_false(identical(base, trans))
})

testthat::test_that("tab_xl is theme-aware (dark palette differs from light)", {
  fl <- tempfile(fileext = ".xlsx"); fd <- tempfile(fileext = ".xlsx")
  tab_xl(t_row, path = fl, theme = "light", open = FALSE, replace = TRUE)
  tab_xl(t_row, path = fd, theme = "dark",  open = FALSE, replace = TRUE)
  # a coloured table written under two themes produces two different workbooks
  testthat::expect_false(identical(readBin(fl, "raw", file.size(fl)),
                                   readBin(fd, "raw", file.size(fd))))
})

testthat::test_that("deprecated tab_xl(print_color_legend) still feeds color_legend", {
  f <- tempfile(fileext = ".xlsx")
  lifecycle::expect_deprecated(
    tab_xl(t_row, path = f, print_color_legend = FALSE, open = FALSE, replace = TRUE)
  )
})
