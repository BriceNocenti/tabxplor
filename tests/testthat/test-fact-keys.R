# PURPOSE: KEY 2 (Phase 20a) -- the declared fact tables' cross-table foreign keys.
# ROLE: the load-time check in R/zzz-fact-keys.R already runs at R CMD INSTALL, so a green suite is
#       not what proves it works. These tests prove the CHECKER itself catches a dangling key, that
#       the declared edges are complete enough to be worth running, and that no target table has an
#       unreferenced row.
# KEY CONSTRAINTS:
#   - a cached binary install skips the top-level code, so re-running the check here is the belt.
#   - every table must be read with `[[`, never `$` (MEASURES$adjustment has `scale_from`, so
#     `$scale` partial-matches to "gap"). The readers are asserted for that below.

testthat::test_that("every declared foreign key resolves", {
  testthat::expect_no_error(tabxplor:::tx_check_foreign_keys())
})

testthat::test_that("no target table has an unreferenced row", {
  # `orphan = TRUE` edges point at COLOR_SCALES: a break ladder no measure and no scale references
  # is dead weight, and this is where that is noticed.
  testthat::expect_equal(tabxplor:::tx_check_foreign_keys(), list())
})

testthat::test_that("the checker catches a dangling key", {
  broken <- list(tabxplor:::tx_fk("FAKE$key", function() c("difference", "not_a_measure"),
                                  function() names(tabxplor:::MEASURES)))
  testthat::expect_error(tabxplor:::tx_check_foreign_keys(broken), "not_a_measure")
  testthat::expect_error(tabxplor:::tx_check_foreign_keys(broken), "dangling key in FAKE[$]key")
})

testthat::test_that("an `allow` entry is legal but does not open the gate", {
  ok  <- list(tabxplor:::tx_fk("FAKE$key", function() "ci", function() tabxplor:::fmt_field_names,
                               allow = "ci"))
  bad <- list(tabxplor:::tx_fk("FAKE$key", function() c("ci", "nope"),
                               function() tabxplor:::fmt_field_names, allow = "ci"))
  testthat::expect_no_error(tabxplor:::tx_check_foreign_keys(ok))
  testthat::expect_error(tabxplor:::tx_check_foreign_keys(bad), "nope")
})

testthat::test_that("the row readers are exact, never partial-matching", {
  # THE hazard the file header names: MEASURES$adjustment has `scale_from` and no `scale`, so `$`
  # would return "gap" and a generic checker would validate the wrong string.
  tbl <- list(a = list(scale_from = "gap"), b = list(scale = "pct_diff"))
  testthat::expect_identical(tabxplor:::tx_fk_scalar(tbl, "scale"), c(NA_character_, "pct_diff"))
  testthat::expect_identical(tabxplor:::tx_fk_all(tbl, "scale"), "pct_diff")
})

testthat::test_that("the edge inventory covers the tables a rename would break", {
  froms <- vapply(tabxplor:::TAB_FOREIGN_KEYS, function(k) k$from, character(1))
  # the edge 19d actually broke, and the four that were never checked at all
  testthat::expect_true("EST_SCALES$label_meas" %in% froms)
  testthat::expect_true("REG_ESTIMANDS$rows$display" %in% froms)
  testthat::expect_true("REG_ESTIMANDS$rows$crude_fam" %in% froms)
  testthat::expect_true("REG_ESTIMANDS$rows$crude_shape" %in% froms)
  testthat::expect_true("DISPLAY_TOKENS$field" %in% froms)
  testthat::expect_gt(length(froms), 25L)
})
