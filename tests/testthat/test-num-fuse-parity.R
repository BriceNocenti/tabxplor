# PURPOSE: The numeric aggregate-injection seam (Phase 7d-i) -- tab_num(.fine=) adopting a moment-sum
#          aggregate built by tab_aggregate_num() -- must produce byte-identical tables to tab_num()'s
#          own table-by-table scan. Locks: the shared num_moment_scan() math, the moment-column
#          construction order, the Kish _w2 round-trip, that a passed `.fine` is not mutated, and the
#          Phase 6e `ci="cell" + tab_vars` regression.
# NOTE: engine-internal parity test. Drives tab_build() (the `.by_table` entry -> raw scan) and
#       tab_num()/tab_aggregate_num() directly. The numeric fine has NO row floor (it is a
#       per-row_var relocation of the single scan, not a cross-table fusion), so unlike the factor
#       fuse-parity test this needs no `tabxplor.fuse_min_rows`.
# See: tab_aggregate_num()/num_moment_scan() (R/tab-agg.R); tab_num(.fine=) (R/tab.R); CLAUDE.md Phase 7d.

# gss_cat carries two numeric col_vars (age, tvhours, both with real NAs) + a deterministic weight
# and injected row_var NAs.
make_gss_num <- function() {
  gss <- forcats::gss_cat
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss$marital[seq(1L, nrow(gss), by = 500L)] <- NA
  gss
}

testthat::test_that("tab_num(.fine=) adopt == inline scan (unweighted + weighted, na keep/drop)", {
  gss <- make_gss_num()

  fine_u <- tabxplor:::tab_aggregate_num(gss, race, c(age, tvhours), marital, na = "keep")
  testthat::expect_equal(
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, ci = "cell", .fine = fine_u),
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, ci = "cell"))

  fine_w <- tabxplor:::tab_aggregate_num(gss, race, c(age, tvhours), marital, wt = w, na = "drop")
  testthat::expect_equal(
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, wt = w, na = "drop", ci = "cell",
                       .fine = fine_w),
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, wt = w, na = "drop", ci = "cell"))
})

testthat::test_that("tab_build numeric: default (adopt fine) == .by_table (raw scan)", {
  gss <- make_gss_num()

  # unweighted, na = "keep" (default)
  testthat::expect_equal(
    tabxplor:::tab_build(gss, race, c(age, tvhours)),
    tabxplor:::tab_build(gss, race, c(age, tvhours), .by_table = TRUE))

  # weighted, na = "drop"
  testthat::expect_equal(
    tabxplor:::tab_build(gss, race, c(age, tvhours), wt = w, na = "drop"),
    tabxplor:::tab_build(gss, race, c(age, tvhours), wt = w, na = "drop", .by_table = TRUE))

  # tab_vars + comp = "all" (total table via num_rollup)
  testthat::expect_equal(
    tabxplor:::tab_build(gss, race, c(age, tvhours), year, comp = "all"),
    tabxplor:::tab_build(gss, race, c(age, tvhours), year, comp = "all", .by_table = TRUE))

  # ci = "cell" + tab_vars (the previously-crashing config -- H1/H5)
  testthat::expect_equal(
    tabxplor:::tab_build(gss, race, c(age, tvhours), marital, ci = "cell"),
    tabxplor:::tab_build(gss, race, c(age, tvhours), marital, ci = "cell", .by_table = TRUE))

  # ci = "diff" + stars, weighted (Welch-t path)
  testthat::expect_equal(
    tabxplor:::tab_build(gss, race, c(age, tvhours), wt = w, ci = "diff", color = "diff"),
    tabxplor:::tab_build(gss, race, c(age, tvhours), wt = w, ci = "diff", color = "diff",
                         .by_table = TRUE))

  # MIXED table: factor + numeric col_vars coexist (numeric fine + factor path)
  testthat::expect_equal(
    tabxplor:::tab_build(gss, marital, c(race, tvhours), wt = w, pct = "row", na = "drop"),
    tabxplor:::tab_build(gss, marital, c(race, tvhours), wt = w, pct = "row", na = "drop",
                         .by_table = TRUE))

  # several row_vars -> a per-row_var fine each (H1: never fused across row_vars)
  testthat::expect_equal(
    tabxplor:::tab_build(gss, c(marital, relig), c(age, tvhours), na = "drop"),
    tabxplor:::tab_build(gss, c(marital, relig), c(age, tvhours), na = "drop", .by_table = TRUE))
})

testthat::test_that("Kish n_eff (tabxplor.kish_neff): _w2 survives the .fine round-trip", {
  withr::local_options(tabxplor.kish_neff = TRUE)
  gss <- make_gss_num()

  fine <- tabxplor:::tab_aggregate_num(gss, race, c(age, tvhours), marital, wt = w, na = "drop")
  testthat::expect_equal(
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, wt = w, na = "drop", ci = "cell",
                       .fine = fine),
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, wt = w, na = "drop", ci = "cell"))
})

testthat::test_that("a passed `.fine` is not mutated by tab_num (copy on adopt -- H9)", {
  gss <- make_gss_num()
  fine   <- tabxplor:::tab_aggregate_num(gss, race, c(age, tvhours), marital, na = "keep")
  before <- as.data.frame(fine)
  invisible(tabxplor:::tab_num(gss, race, c(age, tvhours), marital, ci = "cell", .fine = fine))
  testthat::expect_equal(as.data.frame(fine), before)
})

testthat::test_that("tab_num(<tab_vars>, ci='cell') does not error (Phase 6e fix, both comp modes)", {
  gss <- make_gss_num()
  testthat::expect_no_error(
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, ci = "cell"))
  testthat::expect_no_error(
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, ci = "cell", comp = "all"))
  testthat::expect_no_error(
    tabxplor:::tab_num(gss, race, c(age, tvhours), marital, ci = "cell", na = "drop"))
  # also via the .by_table raw path
  testthat::expect_no_error(
    tabxplor:::tab_build(gss, race, c(age, tvhours), marital, ci = "cell", .by_table = TRUE))
})
