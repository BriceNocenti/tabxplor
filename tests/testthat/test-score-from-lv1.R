# Phase 17k: score_from_lv1() is exported but was untested. It counts, per row, how many of the
# listed factors sit at their first (reference) level. These tests lock the counting logic, the
# NA-as-0 rule, the replace-existing-column behaviour, and that the score feeds tab() cleanly.

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
