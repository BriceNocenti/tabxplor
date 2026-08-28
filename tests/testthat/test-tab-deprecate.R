# PURPOSE: the 1.x -> 2.0.0 translation layer.
# ROLE: the shipped CONTRACT for R/tab-deprecate.R -- a 1.x call that still works must keep working,
#   and must land on exactly the 2.0.0 value it names.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the retired `type` vocabulary =======================================================

# every 1.x `type`, and the (scale, pct_type) pair it names now. Stated here as the CONTRACT, so a
# change to fmt_type_legacy() that is not a deliberate one fails.
TYPE_MAP <- list(
  row      = c("level_pct" , "row"),
  col      = c("level_pct" , "col"),
  all      = c("level_pct" , "all"),
  all_tabs = c("level_pct" , "all_tabs"),
  mean     = c("level_mean", "none"),
  n        = c("level_n"   , "none"),
  coef     = c("raw_diff"  , "none")
)

testthat::test_that("set_type() lands on the pair that replaced it, and reads back", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- fmt(n = c(10L, 20L), pct = c(0.3, 0.7), scale = "level_pct", pct_type = "row")
  for (tp in names(TYPE_MAP)) {
    y <- set_type(x, tp)
    testthat::expect_identical(get_scale(y)   , TYPE_MAP[[tp]][[1]], info = tp)
    testthat::expect_identical(get_pct_type(y), TYPE_MAP[[tp]][[2]], info = tp)
    testthat::expect_identical(get_type(y)    , tp                 , info = tp)
  }
})

testthat::test_that("the retired spellings of 'no type' all mean a count", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- fmt(n = 1L, scale = "level_pct", pct_type = "row")
  for (tp in list("no", "", NA_character_))
    testthat::expect_identical(get_scale(set_type(x, tp)), "level_n")
})

testthat::test_that("set_type() writes through the validating setters", {
  withr::local_options(lifecycle_verbosity = "quiet")
  testthat::expect_error(set_type(fmt(n = 1L), "bogus"), "must be one of")
})

testthat::test_that("get_type() folds every effect scale back onto 'coef'", {
  withr::local_options(lifecycle_verbosity = "quiet")
  for (sc in c("odds_ratio", "pct_ratio", "mean_diff", "points", "log_coef"))
    testthat::expect_identical(get_type(fmt(n = 1L, scale = sc)), "coef", info = sc)
  # ...and reads a whole table column by column
  t <- tab(fx_gss(), race, marital, pct = "row")
  testthat::expect_identical(unname(get_type(t)[-1]), rep("row", ncol(t) - 1L))
})

testthat::test_that("fmt(type = ) is translated, not refused", {
  withr::local_options(lifecycle_verbosity = "quiet")
  a <- fmt(pct = c(0.1, 0.9), n = 0L, type = "all")
  testthat::expect_identical(get_scale(a)   , "level_pct")
  testthat::expect_identical(get_pct_type(a), "all")
  # the display default is a promise reading `scale`, so it must follow the TRANSLATED scale
  testthat::expect_identical(unique(tabxplor:::get_display(a)), "pct")

  b <- fmt(n = c(30L, 30L), type = "mean", mean = c(1.5, 2.5))
  testthat::expect_identical(get_scale(b), "level_mean")
  testthat::expect_identical(unique(tabxplor:::get_display(b)), "mean")
})

testthat::test_that("fmt() deprecates `type` once, refuses `ci_type` and refuses a conflict", {
  withr::local_options(lifecycle_verbosity = "warning")
  lifecycle::expect_deprecated(fmt(n = 1L, pct = 0.5, type = "row"))
  testthat::expect_error(fmt(n = 1L, ci_type = "diff"), "no longer has")
  testthat::expect_error(fmt(n = 1L, type = "row", scale = "level_pct"), "both")
  testthat::expect_error(fmt(n = 1L, nope = 2), "Unused argument")
})

testthat::test_that("a message bullet cannot be swallowed by the id argument", {
  # `tx_inform_once(id, ...)`: a `"i" = ` bullet used to partial-match `id`, dropping the line and
  # printing the id instead. The formal is dot-prefixed now.
  testthat::expect_identical(names(formals(tabxplor:::tx_inform_once))[[1]], ".id")
})
