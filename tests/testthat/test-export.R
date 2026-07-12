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

testthat::test_that("contrib tables render through every exporter (comp tab/all, +/- tab_vars)", {
  # Regression for two Phase 10j-B crashes: the colour engine (get_mean_contrib size 0 under
  # comp = "all" without a total table) and the kable tooltip (cond_ctr NA on the Total column,
  # which broke ANY contrib table via tab_kable, incl. the default comp = "tab").
  gss <- forcats::gss_cat
  contribs <- list(
    tab     = tab(gss, marital, race, pct = "row", color = "contrib"),
    all     = suppressWarnings(tab(gss, marital, race, pct = "row", color = "contrib", comp = "all")),
    all_tab = tab(gss, marital, race, tab_vars = year, pct = "row", color = "contrib", comp = "all")
  )
  for (nm in names(contribs)) {
    t <- contribs[[nm]]
    testthat::expect_no_error(as.character(tab_kable(t)))                    # crash 2 (tooltip)
    testthat::expect_type(tab_md(t, print = FALSE), "character")
    f <- tempfile(fileext = ".xlsx")
    testthat::expect_no_error(tab_xl(t, path = f, open = FALSE, replace = TRUE))  # crash 1
  }
})

testthat::test_that("tab_plot renders a non-mergeable list as a list of plots (list-method parity)", {
  testthat::skip_if_not_installed("ggpubr")
  testthat::skip_if_not_installed("cowplot")
  testthat::skip_if_not_installed("gtable")
  is_gg <- function(x) inherits(x, "ggplot")            # version-agnostic (is.ggplot() is deprecated)
  t2 <- tab(forcats::gss_cat, race, relig, pct = "row", color = "diff")  # different col_vars
  testthat::expect_true(is_gg(tab_plot(t_row)))                          # single -> one plot
  lst <- tab_plot(list(t_row, t2))                                        # non-mergeable -> list
  testthat::expect_true(is.list(lst) && !is_gg(lst))
  testthat::expect_length(lst, 2L)
  testthat::expect_true(all(vapply(lst, is_gg, logical(1))))
})
