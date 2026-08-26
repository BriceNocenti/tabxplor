# PURPOSE: the exported user helpers.
# ROLE: the shipped CONTRACT for R/utils.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: fct_recode_helper() =================================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


testthat::test_that("fct_recode_helper() works with freq = FALSE", {
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  out <- fct_recode_helper(fx_gss(), tidyselect::all_of("marital"),
                           freq = FALSE, cat = FALSE)
  testthat::expect_s3_class(out, "data.frame")
  txt <- paste(unlist(out), collapse = "\n")
  # every recode line is a `"level" = "level"` pair
  testthat::expect_true(grepl("\"Married\"", txt))
  testthat::expect_true(grepl("=", txt))
})

testthat::test_that("fct_recode_helper() works with freq = TRUE (Phase 14p)", {
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  # the reported crash: a single ordered-looking income factor
  testthat::expect_no_error(
    out <- fct_recode_helper(fx_gss(), tidyselect::all_of("rincome"),
                             freq = TRUE, cat = FALSE)
  )
  testthat::expect_s3_class(out, "data.frame")
  txt <- paste(unlist(out), collapse = "\n")
  # a frequency + count comment is emitted beside each level (e.g. "# 34% 7 363")
  testthat::expect_true(grepl("#", txt))
  testthat::expect_true(grepl("\\$25000 or more", txt))
})

testthat::test_that("fct_recode_helper(freq = TRUE) handles several factors at once (Phase 14p)", {
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  testthat::expect_no_error(
    out <- fct_recode_helper(fx_gss(), tidyselect::all_of(c("marital", "race", "relig")),
                             freq = TRUE, cat = FALSE)
  )
  txt <- paste(unlist(out), collapse = "\n")
  testthat::expect_true(grepl("\"Married\"", txt))
  testthat::expect_true(grepl("\"White\"", txt))
})


# === SECTION: score_from_lv1() ====================================================================

testthat::test_that("score_from_lv1() counts factors at their first level", {
  data <- tibble::tibble(
    group = factor(c("G1", "G1", "G2", "G2", "G3", "G3")),
    a = factor(c("Oui", "Oui", "Oui", "Oui", "Non", "Oui")),
    b = factor(c("Oui", "Non", "Non", "Oui", "Non", "Oui")),
    c = factor(c("Oui", "Oui", "Non", "Non", "Oui", "Oui"))
  )
  # first level of each factor is "Non" (alphabetical): count "Non" per row
  out <- score_from_lv1(data, "score", vars_list = c("a", "b", "c"))
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true("score" %in% names(out))
  testthat::expect_type(out$score, "integer")
  # row-by-row: Non count over (a, b, c)
  testthat::expect_identical(out$score, c(0L, 1L, 2L, 1L, 2L, 0L))
})

testthat::test_that("score_from_lv1() folds NA into a non-first level (counts as 0)", {
  data <- tibble::tibble(
    a = factor(c("yes", "no", NA, "yes")),   # first level "no"
    b = factor(c("no",  "no", "no", NA))     # first level "no"
  )
  out <- score_from_lv1(data, "score", vars_list = c("a", "b"))
  # a: no at row 2 only ; b: no at rows 1-3 ; NA never matches the first level
  testthat::expect_identical(out$score, c(1L, 2L, 1L, 0L))
})

testthat::test_that("score_from_lv1() replaces an existing column of the same name", {
  data <- tibble::tibble(
    score = c(99L, 99L),
    a = factor(c("no", "yes")),   # first level "no"
    b = factor(c("no", "no"))     # first level "no"
  )
  out <- score_from_lv1(data, "score", vars_list = c("a", "b"))
  testthat::expect_identical(out$score, c(2L, 1L))
  # exactly one score column, and the vars stay put
  testthat::expect_equal(sum(names(out) == "score"), 1L)
  testthat::expect_true(all(c("a", "b") %in% names(out)))
})

testthat::test_that("score_from_lv1() output feeds tab() cleanly", {
  data <- tibble::tibble(
    group = factor(c("G1", "G1", "G2", "G2")),
    a = factor(c("no", "yes", "no", "no")),
    b = factor(c("no", "no", "yes", "no"))
  )
  out <- score_from_lv1(data, "score", vars_list = c("a", "b"))
  testthat::expect_no_error(t <- tab(out, group, score))
  testthat::expect_s3_class(t, "tabxplor_tab")
})
