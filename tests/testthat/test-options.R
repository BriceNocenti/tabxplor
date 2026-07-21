# PURPOSE: Coverage for the option-synonym resolver tx_getOption() and the Phase 17j renames/aliases.
# ROLE: Guards the retro-compat contract "old option names keep working" -- each assertion sets the
#        LEGACY / alias name and checks the read site honours it over the seeded canonical.

testthat::test_that("tx_getOption() returns the first name set, else the default", {
  withr::local_options(list(tabxplor._syn_a = NULL, tabxplor._syn_b = NULL))
  # neither set -> default
  testthat::expect_identical(tx_getOption(c("tabxplor._syn_a", "tabxplor._syn_b"), "d"), "d")
  # only the second (canonical/seeded) set -> that value
  withr::with_options(list(tabxplor._syn_b = "B"),
    testthat::expect_identical(tx_getOption(c("tabxplor._syn_a", "tabxplor._syn_b"), "d"), "B"))
  # the first (legacy/alias) set -> it WINS over a later-listed name (the ordering contract)
  withr::with_options(list(tabxplor._syn_a = "A", tabxplor._syn_b = "B"),
    testthat::expect_identical(tx_getOption(c("tabxplor._syn_a", "tabxplor._syn_b"), "d"), "A"))
})

testthat::test_that("tabxplor.kable_css (old name) still drives tab_kable's css, over the seeded tab_kable_css", {
  # the new canonical is seeded TRUE at load
  testthat::expect_true(isTRUE(getOption("tabxplor.tab_kable_css")))
  # old name set FALSE must win over the seeded-TRUE new name
  testthat::expect_false(
    withr::with_options(list(tabxplor.kable_css = FALSE),
      tx_getOption(c("tabxplor.kable_css", "tabxplor.tab_kable_css"), TRUE)))
  # new name honoured directly
  testthat::expect_false(
    withr::with_options(list(tabxplor.tab_kable_css = FALSE),
      tx_getOption(c("tabxplor.kable_css", "tabxplor.tab_kable_css"), TRUE)))
})

testthat::test_that("tabxplor.console_theme aliases the console palette theme", {
  dark  <- get_color_style("color_code", theme = "dark")
  light <- get_color_style("color_code", theme = "light")
  testthat::expect_false(identical(dark, light))                       # non-vacuous
  # setting the alias (with color_style_theme unset) resolves to the dark palette
  got <- withr::with_options(
    list(tabxplor.console_theme = "dark", tabxplor.color_style_theme = NULL),
    get_color_style("color_code"))
  testthat::expect_identical(got, dark)
})

testthat::test_that("tabxplor.export_theme aliases the export theme in resolve_export_opts()", {
  got <- withr::with_options(
    list(tabxplor.export_theme = "dark", tabxplor.theme = NULL),
    resolve_export_opts(allow_auto = TRUE)$theme)
  testthat::expect_identical(got, "dark")
  # the legacy canonical name still works too
  got2 <- withr::with_options(
    list(tabxplor.theme = "dark"),
    resolve_export_opts(allow_auto = TRUE)$theme)
  testthat::expect_identical(got2, "dark")
})
