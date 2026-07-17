# Phase 14p: fct_recode_helper() is exported but was untested. `freq = TRUE` crashed with
# "object 'pct' not found" -- the bare `filter()` (not imported) resolved to stats::filter() and
# evaluated its predicate outside the data mask. These tests lock both the freq and no-freq paths.

testthat::test_that("fct_recode_helper() works with freq = FALSE", {
  out <- fct_recode_helper(forcats::gss_cat, tidyselect::all_of("marital"),
                           freq = FALSE, cat = FALSE)
  testthat::expect_s3_class(out, "data.frame")
  txt <- paste(unlist(out), collapse = "\n")
  # every recode line is a `"level" = "level"` pair
  testthat::expect_true(grepl("\"Married\"", txt))
  testthat::expect_true(grepl("=", txt))
})

testthat::test_that("fct_recode_helper() works with freq = TRUE (Phase 14p)", {
  # the reported crash: a single ordered-looking income factor
  testthat::expect_no_error(
    out <- fct_recode_helper(forcats::gss_cat, tidyselect::all_of("rincome"),
                             freq = TRUE, cat = FALSE)
  )
  testthat::expect_s3_class(out, "data.frame")
  txt <- paste(unlist(out), collapse = "\n")
  # a frequency + count comment is emitted beside each level (e.g. "# 34% 7 363")
  testthat::expect_true(grepl("#", txt))
  testthat::expect_true(grepl("\\$25000 or more", txt))
})

testthat::test_that("fct_recode_helper(freq = TRUE) handles several factors at once (Phase 14p)", {
  testthat::expect_no_error(
    out <- fct_recode_helper(forcats::gss_cat, tidyselect::all_of(c("marital", "race", "relig")),
                             freq = TRUE, cat = FALSE)
  )
  txt <- paste(unlist(out), collapse = "\n")
  testthat::expect_true(grepl("\"Married\"", txt))
  testthat::expect_true(grepl("\"White\"", txt))
})
