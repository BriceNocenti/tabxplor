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

# === Phase 22h: the `tabxplor.parallel` worker count =============================================
# The RULE is arithmetic and must be testable without mirai, so it lives in tab_auto_workers();
# tab_parallel_workers() is the option boundary around it and is skipped where mirai is absent.

testthat::test_that("the auto worker count is half the cores, floored at 2 and capped at 4", {
  f <- tab_auto_workers
  testthat::expect_identical(f(1L), 1L)                    # one core: 1 IS serial, 2 would oversubscribe
  testthat::expect_identical(f(2L), 2L)                    # the floor: 2 cores is where 2 workers pay
  testthat::expect_identical(f(3L), 2L)
  testthat::expect_identical(f(4L), 2L)                    # a student all-in-one
  testthat::expect_identical(f(6L), 3L)
  testthat::expect_identical(f(8L), 4L)                    # the cap
  testthat::expect_identical(f(12L), 4L)
  testthat::expect_identical(f(64L), 4L)                   # never more, whatever the machine
})

testthat::test_that("tab_available_cores() answers the OPTIONS before the machine", {
  # `_R_CHECK_LIMIT_CORES_` is CRAN's 2-core rule and wins over everything.
  testthat::expect_identical(
    withr::with_envvar(c("_R_CHECK_LIMIT_CORES_" = "TRUE"), tab_available_cores()), 2L)
  # base R's own convention: a user who set mc.cores has already answered the question.
  testthat::expect_identical(
    withr::with_envvar(c("_R_CHECK_LIMIT_CORES_" = ""),
                       withr::with_options(list(mc.cores = 3L), tab_available_cores())), 3L)
  testthat::expect_gte(withr::with_options(list(mc.cores = NULL), tab_available_cores()), 1L)
})

testthat::test_that("the parallel option resolves off / auto / verbatim, and never over CRAN's cap", {
  testthat::skip_if_not_installed("mirai")
  w <- function(p, ...) withr::with_options(list(tabxplor.parallel = p),
                                            withr::with_envvar(c(...), tab_parallel_workers()))
  for (off in list(FALSE, NULL, "no", 0L, -1L))
    testthat::expect_identical(w(off, "_R_CHECK_LIMIT_CORES_" = ""), 0L)
  # TRUE and "auto" are the same answer, and it is the rule above
  auto <- withr::with_envvar(c("_R_CHECK_LIMIT_CORES_" = ""), tab_auto_workers())
  testthat::expect_identical(w(TRUE,   "_R_CHECK_LIMIT_CORES_" = ""), auto)
  testthat::expect_identical(w("auto", "_R_CHECK_LIMIT_CORES_" = ""), auto)
  # an integer (or its string form, which is what jamovi passes) is taken verbatim
  testthat::expect_identical(w(3L,  "_R_CHECK_LIMIT_CORES_" = ""), 3L)
  testthat::expect_identical(w("3", "_R_CHECK_LIMIT_CORES_" = ""), 3L)
  # ... but never past CRAN's cap, however it was asked for
  testthat::expect_identical(w(8L,     "_R_CHECK_LIMIT_CORES_" = "TRUE"), 2L)
  testthat::expect_identical(w("auto", "_R_CHECK_LIMIT_CORES_" = "TRUE"), 2L)
  # a jmvtab live cache is always serial, whatever the option says
  testthat::expect_identical(
    withr::with_options(list(tabxplor.parallel = "auto"), tab_parallel_workers(new.env())), 0L)
})
