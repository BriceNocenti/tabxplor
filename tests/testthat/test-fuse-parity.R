# PURPOSE: The factor tier-1 aggregate-injection seam -- tab_plain(.fine=) adopting a finest-grain
#          count aggregate and marginalising it to its (row_var x col_var) pair -- must produce
#          byte-identical tables to tab_plain()'s own table-by-table raw scan. This is the FACTOR
#          analogue of test-num-fuse-parity.R.
# NOTE (Phase 9c): the former tab()-level opt-in scan-fusion (`options(tabxplor.fuse_min_rows)` + the
#       fused block in tab_aggregate()) was REMOVED -- it was a net-negative once the O(cells) build
#       dominates (dev/tabxplor_2.0.0_decisions.md 30). The `.fine`/fine_for_pair()/use_raw seam in
#       tab_plain() STAYS: it is now EXCLUSIVELY the jmvtab cache seam (jmv_cache_aggregate() injects a
#       per-pair `.fine`; end-to-end == tab() is locked by test-jmvtab-cache.R). This test drives that
#       seam DIRECTLY (build a valid `.fine`, feed tab_plain(.fine=)), so a regression is caught with a
#       focused failure rather than only inside the jmvtab cache suite.
# See: tab_plain() `.fine`/`.by_table` (R/tab.R); fine_for_pair(); jmv_cache_aggregate() (R/jmvtab-cache.R).

# Real tidyverse factor-rich fixture + a deterministic weight and deterministic NAs.
make_gss <- function() {
  gss <- forcats::gss_cat
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss$marital[seq(1L, nrow(gss), by = 500L)] <- NA
  gss$partyid[seq(1L, nrow(gss), by = 700L)] <- NA
  gss
}

# Build a finest-grain count aggregate keyed by `keycols` (NA groups kept, like jmv_cache_aggregate()
# and the removed fused block): the exact input tab_plain(.fine=) marginalises to its pair.
build_fine <- function(data, keycols, wtname = NULL) {
  dt <- data.table::as.data.table(data[c(keycols, wtname)])
  if (is.null(wtname)) {
    dt[, list(n = .N), keyby = keycols]
  } else {
    # Last Phase z16-iiiii: Sigma w^2 alongside Sigma w, exactly as jmv_cache_aggregate() now emits it
    # -- it is what lets a pre-aggregate serve the WEIGHTED inference basis (see the test below).
    dt[, list(n = .N, wn = sum(as.numeric(get(wtname)), na.rm = TRUE),
              w2 = sum(as.numeric(get(wtname))^2, na.rm = TRUE)), keyby = keycols]
  }
}

testthat::test_that("tab_plain(.fine=) adopt == raw scan (weighted/na/pct/tab_vars)", {
  gss <- make_gss()

  # unweighted, pct = "row", na = "drop"
  f1 <- build_fine(gss, c("marital", "race"))
  testthat::expect_equal(
    tabxplor:::tab_plain(gss, marital, race, pct = "row", na = "drop", .fine = f1),
    tabxplor:::tab_plain(gss, marital, race, pct = "row", na = "drop"))

  # weighted, pct = "col", na = "drop"
  f2 <- build_fine(gss, c("marital", "race"), "w")
  testthat::expect_equal(
    tabxplor:::tab_plain(gss, marital, race, wt = w, pct = "col", na = "drop", .fine = f2),
    tabxplor:::tab_plain(gss, marital, race, wt = w, pct = "col", na = "drop"))

  # weighted, pct = "row", na = "keep" (explicit NA level kept)
  testthat::expect_equal(
    tabxplor:::tab_plain(gss, marital, race, wt = w, pct = "row", na = "keep", .fine = f2),
    tabxplor:::tab_plain(gss, marital, race, wt = w, pct = "row", na = "keep"))

  # tab_vars = year (subtables): keyed by c(tab_vars, row_var, col_var)
  f3 <- build_fine(gss, c("year", "marital", "race"), "w")
  testthat::expect_equal(
    tabxplor:::tab_plain(gss, marital, race, year, wt = w, pct = "row", na = "drop", .fine = f3),
    tabxplor:::tab_plain(gss, marital, race, year, wt = w, pct = "row", na = "drop"))
})

testthat::test_that("a FINER joint `.fine` rolls up to the pair identically", {
  gss <- make_gss()
  # a joint aggregate over two factor col_vars; tab_plain must marginalise away `partyid` for the
  # marital x race pair (exercises the keyby-sum rollup, not just an identity regroup).
  fj <- build_fine(gss, c("marital", "race", "partyid"), "w")
  testthat::expect_equal(
    tabxplor:::tab_plain(gss, marital, race, wt = w, pct = "row", na = "drop", .fine = fj),
    tabxplor:::tab_plain(gss, marital, race, wt = w, pct = "row", na = "drop"))
})

testthat::test_that("tab_build factor path: default (no fusion) == .by_table (raw scan)", {
  gss <- make_gss()
  # tab() no longer fuses, so both sides raw-scan; this pins that `.by_table` is a no-op on the
  # factor path now (the numeric `fine_num` is unaffected -- see test-num-fuse-parity.R).
  testthat::expect_equal(
    tabxplor:::tab_build(gss, marital, c(race, partyid), wt = w, pct = "row", na = "drop", chi2 = TRUE),
    tabxplor:::tab_build(gss, marital, c(race, partyid), wt = w, pct = "row", na = "drop", chi2 = TRUE,
                         .by_table = TRUE))
})

testthat::test_that("a `.fine` carrying Sigma w^2 serves the weighted basis, exactly as a raw scan", {
  # Last Phase z16-iiiii, the FACTOR twin of test-num-fuse-parity.R's `_w2` round-trip. Until this
  # phase jmv_cache_aggregate() emitted only (n, wn), so `has_w2` was FALSE on the cache path: in
  # jamovi, ticking `design_effect` widened the MEAN cell intervals and left the PERCENTAGES alone,
  # while the footer denied the one correction that had happened. Failing-first: without the `w2`
  # rollup in plain_core(), `n_eff` here is all-NA and the raw scan's is not.
  withr::local_options(tabxplor.design_effect = TRUE)
  gss  <- make_gss()
  fine <- build_fine(gss, c("marital", "race", "partyid"), "w")
  fused <- tabxplor:::tab_plain(gss, race, partyid, marital, wt = w, pct = "row",
                                na = "drop", .fine = fine)
  raw   <- tabxplor:::tab_plain(gss, race, partyid, marital, wt = w, pct = "row",
                                na = "drop")
  testthat::expect_equal(fused, raw)
  ne <- get_n_eff(raw[[which(purrr::map_lgl(raw, is_fmt))[[1]]]])
  testthat::expect_gt(sum(is.finite(ne)), 0L)                     # non-vacuous
  testthat::expect_identical(tabxplor:::tab_inference_basis(fused), "weights")
})

testthat::test_that("a legacy `.fine` WITHOUT Sigma w^2 still degrades to the raw basis", {
  # The leaf keeps its own guard: tab_plain() is exported and reachable with a hand-built aggregate
  # that predates the w2 contract. It must fall back, not claim a correction it cannot compute.
  withr::local_options(tabxplor.design_effect = TRUE)
  gss  <- make_gss()
  fine <- build_fine(gss, c("marital", "race", "partyid"), "w")
  fine[, "w2" := NULL]
  t <- tabxplor:::tab_plain(gss, race, partyid, marital, wt = w, pct = "row",
                            na = "drop", .fine = fine)
  testthat::expect_true(all(is.na(get_n_eff(t[[which(purrr::map_lgl(t, is_fmt))[[1]]]]))))
  testthat::expect_identical(tabxplor:::tab_inference_basis(t), "n")
})
