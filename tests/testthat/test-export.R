# Phase 10j: the tab_export() facade + the unified exporter arguments (color / transpose /
# color_legend / caption). The four exporters keep their own snapshot/parity tests; this file locks
# the shared surface.

t_row <- tab(forcats::gss_cat, race, marital, pct = "row", color = c("diff", "ratio"))

testthat::test_that("tab_export() dispatches to each format", {
  testthat::expect_no_error(as.character(tab_export(t_row, "html")))
  testthat::expect_type(tab_export(t_row, "md", print = FALSE), "character")

  f <- tempfile(fileext = ".xlsx")
  tab_export(t_row, "xl", path = f, open = FALSE, replace = TRUE)
  testthat::expect_true(file.exists(f) && file.size(f) > 0)

})

testthat::test_that("color = FALSE renders monochrome (no colour spans)", {
  col <- tab_md(t_row, color = TRUE,  print = FALSE)
  mon <- tab_md(t_row, color = FALSE, print = FALSE)
  # the COLOUR-slot spans (.p1/.m2/.o3/.u4), not the chrome ones a monochrome table still carries
  # (`.tx-unit`, the header's unit line -- Phase 22c-ii).
  slot <- "\\]\\{\\.[pmou][0-9]"
  testthat::expect_true(grepl(slot, col))                  # coloured pandoc spans present
  testthat::expect_false(grepl(slot, mon))                 # none when monochrome
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

# === SECTION: var_names, on all four exporters (Phase 14i) ===================

testthat::test_that("var_names is honoured by every exporter, and defaults to the option", {
  merged <- tab(forcats::gss_cat, c(race, relig), marital, pct = "row")

  # kable (html engine) + md: the row-name column and the col_var span both answer to it
  k_both <- as.character(tab_export(merged, "html", css = FALSE))
  k_none <- as.character(tab_export(merged, "html", css = FALSE,
                                    var_names = "none"))
  testthat::expect_match(k_both, ">race</td>")
  testthat::expect_no_match(k_none, ">race</td>")
  testthat::expect_no_match(k_none, "tx-span", fixed = TRUE)

  m_none <- tab_export(merged, "md", print = FALSE, color = FALSE, var_names = "none")
  testthat::expect_no_match(m_none, "*race*", fixed = TRUE)
  testthat::expect_no_match(m_none, "*marital*", fixed = TRUE)

  # xl
  testthat::skip_if_not_installed("openxlsx2")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_export(merged, "xl", path = tmp, open = FALSE, replace = TRUE,
                              var_names = "none"))
  # `var_names = "none"` drops the name COLUMN and the span row; what is left is the prose merges
  # (the title, each footer line) and the index column's header over the unit row -- none of them a
  # variable name.
  mg <- sub('".*$', "", sub('^.*ref="', "", unlist(openxlsx2::wb_load(tmp)$worksheets[[1]]$mergeCells)))
  testthat::expect_length(grep("^A[0-9]+:A", mg, value = TRUE), 1L)   # the header/unit one only

  # the option is the default
  withr::local_options(tabxplor.var_names = "none")
  testthat::expect_no_match(as.character(tab_export(merged, "html", css = FALSE)),
                            ">race</td>")
})
