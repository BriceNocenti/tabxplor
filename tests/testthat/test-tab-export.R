# PURPOSE: the tab_export() facade and the arguments every backend shares.
# ROLE: the shipped CONTRACT for R/tab-export.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the tab_export() facade =============================================================

t_row <- tab(fx_gss(), race, marital, pct = "row", color = c("diff", "ratio"))



testthat::test_that("tab_export() dispatches to each format", {
  testthat::expect_no_error(as.character(tab_export(t_row, "html")))
  testthat::expect_type(tab_export(t_row, "md", print = FALSE), "character")

  f <- tempfile(fileext = ".xlsx")
  tab_export(t_row, "xl", path = f, open = FALSE, replace = TRUE)
  testthat::expect_true(file.exists(f) && file.size(f) > 0)

})



# === SECTION: var_names, on all four exporters (Phase 14i) ===================

testthat::test_that("var_names is honoured by every exporter, and defaults to the option", {
  merged <- tab(fx_gss(), c(race, relig), marital, pct = "row")

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




# === SECTION: every backend runs ==================================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())




gss <- fx_gss()



# Phase 20h: the prepared starwars fixture, built ONCE at top level -- where the file-level
# lifecycle line above actually bites (testthat re-enables the warning inside every
# test_that()). It was written verbatim in each block below.
sw_prepared <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)



# === SECTION: tab_plot, defunct in 2.0.0 ======================================

testthat::test_that("tab_plot() is defunct and says what replaces it", {
  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  testthat::expect_error(tab_plot(tabs), class = "defunctError")
  # and `format = "plot"` is no longer a value tab_export() accepts
  testthat::expect_error(tab_export(tabs, "plot"), "arg")
})



# === SECTION: tab_xl (extended tests) =========================================

testthat::test_that("tab_xl creates a valid Excel file with multiple color types", {
  testthat::skip_if_not_installed("openxlsx2")

  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "diff")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  tab_xl(tabs, path = tmp, open = FALSE)
  testthat::expect_true(file.exists(tmp))
  testthat::expect_gt(file.size(tmp), 0)
})
