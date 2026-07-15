# PURPOSE: lock the SHAPE of the data-free cache-key material emitted by tab_resolve_settings()
#          for the jmvtab cache tiers 0-2 (Phase 7d-ii; dev/tabxplor_jmvtab_cache_design.md §3).
#          These keys are consumed by the Phase 7e module rewrite (which adds the data hashes);
#          this test only guards the symbolic contract so 7e can rely on it.

testthat::test_that("tab_cache_keys emits the tier 0-2 skeleton", {
  keys <- tabxplor:::tab_cache_keys(
    na = "keep", wt_name = "w", other_if_less_than = 5, comp = "tab",
    tab_vars = c("region", "year"), row_vars = "marital", col_vars = c("race", "partyid")
  )
  testthat::expect_named(keys, c("tier0", "tier1_common", "tier2"))
  testthat::expect_named(keys$tier0, c("na", "wt", "filter", "population"))
  testthat::expect_named(keys$tier1_common, c("grain", "wt", "other_if_less_than", "population"))
  testthat::expect_named(keys$tier2, "comp")

  # grain = sorted tab_vars; wt carried on both persisted tiers.
  testthat::expect_identical(keys$tier1_common$grain, c("region", "year"))
  testthat::expect_identical(keys$tier0$wt, "w")
  testthat::expect_identical(keys$tier1_common$wt, "w")
  testthat::expect_identical(keys$tier1_common$other_if_less_than, 5)
  testthat::expect_identical(keys$tier2$comp, "tab")
})

testthat::test_that("population descriptor encodes each na mode (\u00a73.1)", {
  # keep / drop -> full population (per-pair reuse is widest).
  testthat::expect_identical(
    tabxplor:::tab_cache_keys(na = "keep", row_vars = "a", col_vars = "b")$tier0$population,
    "full")
  testthat::expect_identical(
    tabxplor:::tab_cache_keys(na = "drop", row_vars = "a", col_vars = "b")$tier1_common$population,
    "full")

  # drop_all -> listwise on ALL selected vars (sorted, unique).
  pop_da <- tabxplor:::tab_cache_keys(
    na = "drop_all", row_vars = "marital", col_vars = c("race", "partyid"),
    tab_vars = "year")$tier0$population
  testthat::expect_identical(pop_da$mode, "drop_all")
  testthat::expect_identical(pop_da$vars, sort(c("marital", "race", "partyid", "year")))

  # common_base -> row_var + FIRST col_var + tab_vars (secondary col_vars keep their own NAs).
  pop_cb <- tabxplor:::tab_cache_keys(
    na = "common_base", row_vars = "marital", col_vars = c("race", "partyid"),
    tab_vars = "year")$tier1_common$population
  testthat::expect_identical(pop_cb$mode, "common_base")
  testthat::expect_identical(pop_cb$vars, c("marital", "race", "year"))
})

testthat::test_that("tab_resolve_settings returns cache_keys alongside the colour cascade", {
  out <- tabxplor:::tab_resolve_settings(
    color = "diff", OR = "no", ci = "no", chi2 = FALSE, ref = "tot",
    pct_vect = list("row"), col_vars_text = TRUE, totrow = TRUE,
    na = "keep", wt_name = character(), other_if_less_than = 0, comp = "tab",
    tab_vars = character(), row_vars = "marital", col_vars = "race"
  )
  testthat::expect_true("cache_keys" %in% names(out))
  testthat::expect_named(out$cache_keys, c("tier0", "tier1_common", "tier2"))
  # wt absent -> "" (never NA / missing), grain empty for no tab_vars.
  testthat::expect_identical(out$cache_keys$tier0$wt, "")
  testthat::expect_identical(out$cache_keys$tier1_common$grain, character())
})
