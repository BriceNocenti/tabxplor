# PURPOSE: The shared finest-grain aggregate ("fused") path in tab_many()/tab_plain() must produce
#          byte-identical tables to the table-by-table path. Fusion is forced on via
#          `tabxplor.fuse_min_rows = 0` so it engages on the small gss_cat fixture (correctness is
#          N-independent; the row floor is only a performance guard).
# See: tab_many()/tab_plain() `.fine`/`.by_table`; CLAUDE.md "Perf findings".

# Real tidyverse factor-rich fixture (NOT pc18) + a deterministic weight and deterministic NAs.
make_gss <- function() {
  gss <- forcats::gss_cat
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss$marital[seq(1L, nrow(gss), by = 500L)] <- NA
  gss$partyid[seq(1L, nrow(gss), by = 700L)] <- NA
  gss
}

testthat::test_that("fused == table-by-table across weighted/na/pct/multi/tab_vars/chi2/ci configs", {
  withr::local_options(tabxplor.fuse_min_rows = 0)   # force fusion on the small fixture
  gss <- make_gss()

  testthat::expect_equal(
    tab_many(gss, marital, race, pct = "row", na = "drop"),
    tab_many(gss, marital, race, pct = "row", na = "drop", .by_table = TRUE))

  testthat::expect_equal(
    tab_many(gss, marital, race, wt = w, pct = "col", na = "drop"),
    tab_many(gss, marital, race, wt = w, pct = "col", na = "drop", .by_table = TRUE))

  testthat::expect_equal(
    tab_many(gss, marital, race, wt = w, pct = "row", na = "keep"),
    tab_many(gss, marital, race, wt = w, pct = "row", na = "keep", .by_table = TRUE))

  testthat::expect_equal(
    tab_many(gss, marital, c(race, partyid), wt = w, pct = "row", na = "drop", chi2 = TRUE),
    tab_many(gss, marital, c(race, partyid), wt = w, pct = "row", na = "drop", chi2 = TRUE,
             .by_table = TRUE))

  testthat::expect_equal(
    tab_many(gss, c(marital, relig), race, wt = w, pct = "row", na = "drop"),
    tab_many(gss, c(marital, relig), race, wt = w, pct = "row", na = "drop", .by_table = TRUE))

  testthat::expect_equal(
    tab_many(gss, marital, race, year, wt = w, pct = "row", na = "drop"),   # tab_vars = year
    tab_many(gss, marital, race, year, wt = w, pct = "row", na = "drop", .by_table = TRUE))

  testthat::expect_equal(
    tab_many(gss, marital, race, wt = w, pct = "row", na = "drop", ci = "cell"),
    tab_many(gss, marital, race, wt = w, pct = "row", na = "drop", ci = "cell", .by_table = TRUE))
})

testthat::test_that("auto-fallback when prod(nlevels) > N still matches table-by-table", {
  withr::local_options(tabxplor.fuse_min_rows = 0)
  gss <- make_gss()
  # denom(30) x relig(16) x partyid(10) x marital(6) -> prod(nlevels) >> N(21483): fusion declines,
  # so the default call must fall back to table-by-table and stay identical to it.
  testthat::expect_equal(
    tab_many(gss, c(denom, relig), c(partyid, marital), wt = w, pct = "row", na = "drop"),
    tab_many(gss, c(denom, relig), c(partyid, marital), wt = w, pct = "row", na = "drop",
             .by_table = TRUE))
})
