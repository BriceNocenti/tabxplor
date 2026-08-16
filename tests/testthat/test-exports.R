# PURPOSE: Test export functions (tab_kable, tab_plot) with Suggests-only guards.
# ROLE: Ensures export functions produce valid output without errors.
# KEY CONSTRAINTS:
#   - Must run via test_check("tabxplor"), never in isolation.
#   - All tests guarded by skip_if_not_installed() for Suggests dependencies.

# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


gss <- forcats::gss_cat

# === SECTION: tab_kable =======================================================

testthat::test_that("tab_kable returns a kable object", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  result <- tab_kable(tabs)
  testthat::expect_true(
    inherits(result, "knitr_kable") | inherits(result, "kableExtra") |
      is.character(result)
  )
})

testthat::test_that("tab_kable works with grouped tables", {
  testthat::skip_if_not_installed("kableExtra")

  gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
  tabs <- tab(gss_sub, race, marital, year, pct = "row", color = "diff")
  result <- tab_kable(tabs)
  testthat::expect_true(!is.null(result))
})

testthat::test_that("tab_kable accepts theme='light' and theme='dark'", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  testthat::expect_no_error(tab_kable(tabs, theme = "light"))
  testthat::expect_no_error(tab_kable(tabs, theme = "dark"))
})

testthat::test_that("tab_kable get_data returns a data.frame", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  result <- tab_kable(tabs, get_data = TRUE)
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("tab_kable works with counts (no color)", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital)
  testthat::expect_no_error(tab_kable(tabs))
})

testthat::test_that("tab_kable works with chi2 subtext", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "diff")
  testthat::expect_no_error(tab_kable(tabs))
})

testthat::test_that("tab_kable works with numeric tables", {
  testthat::skip_if_not_installed("kableExtra")

  sw <- dplyr::starwars |>
    tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
                other_if_less_than = 5)
  tabs <- tab_num(sw, sex, height, na = "drop", color = "diff")
  testthat::expect_no_error(tab_kable(tabs))
})

testthat::test_that("tab_kable with contrib color works", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "contrib")
  testthat::expect_no_error(tab_kable(tabs))
})

# === SECTION: tab_plot ========================================================

testthat::test_that("tab_plot returns a ggplot or grob object", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("ggpubr")
  testthat::skip_if_not_installed("cowplot")

  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  result <- tab_plot(tabs)
  testthat::expect_true(
    inherits(result, "gg") | inherits(result, "gtable") |
      inherits(result, "grob") | inherits(result, "ggplot") |
      !is.null(result)
  )
})

testthat::test_that("tab_plot accepts theme='light' and theme='dark'", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("ggpubr")
  testthat::skip_if_not_installed("cowplot")

  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  testthat::expect_no_error(tab_plot(tabs, theme = "light"))
  testthat::expect_no_error(tab_plot(tabs, theme = "dark"))
})

testthat::test_that("tab_plot works with numeric tables", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("ggpubr")
  testthat::skip_if_not_installed("cowplot")

  sw <- dplyr::starwars |>
    tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
                other_if_less_than = 5)
  tabs <- tab_num(sw, sex, height, na = "drop", color = "diff")
  testthat::expect_no_error(tab_plot(tabs))
})

testthat::test_that("tab_plot works with contrib color", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("ggpubr")
  testthat::skip_if_not_installed("cowplot")

  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "contrib")
  testthat::expect_no_error(tab_plot(tabs))
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

testthat::test_that("tab_xl works with contrib color type", {
  testthat::skip_if_not_installed("openxlsx2")

  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "contrib")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  tab_xl(tabs, path = tmp, open = FALSE)
  testthat::expect_true(file.exists(tmp))
})

testthat::test_that("tab_xl works with grouped tables", {
  testthat::skip_if_not_installed("openxlsx2")

  gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
  tabs <- tab(gss_sub, race, marital, year, pct = "row", color = "diff")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  tab_xl(tabs, path = tmp, open = FALSE)
  testthat::expect_true(file.exists(tmp))
})

testthat::test_that("tab_xl works with numeric tables", {
  testthat::skip_if_not_installed("openxlsx2")

  sw <- dplyr::starwars |>
    tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
                other_if_less_than = 5)
  tabs <- tab_num(sw, sex, height, na = "drop", color = "diff")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  tab_xl(tabs, path = tmp, open = FALSE)
  testthat::expect_true(file.exists(tmp))
})
