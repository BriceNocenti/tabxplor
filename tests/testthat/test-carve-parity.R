# PURPOSE: Phase 7d-ii carved tab_build() into five composable, individually-callable stages
#          (tab_setup -> tab_prepare_pop -> tab_aggregate -> tab_transform -> tab_assemble) that
#          match the jmvtab cache tiers. This test locks (a) that running the stages by hand from a
#          hand-built ctx == the direct tab_build() call (the composition is faithful), and (b) that
#          each stage adds the ctx fields Phase 7e will key its cache on (the seam contract). The
#          byte-identity of tab_build()'s OUTPUT is already locked by test-golden / test-fuse-parity /
#          test-counts-parity; this test guards the SEAM so 7e can drive the stages at cache grain.
# See: R/tab.R (the five stages); dev/tabxplor_jmvtab_cache_design.md §8.

carve_gss <- function() {
  gss <- forcats::gss_cat
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss$marital[seq(1L, nrow(gss), by = 500L)] <- NA
  gss
}

# Build the entry-ctx exactly as tab_build()'s argument surface does, for a set of overrides on top
# of the tab_build() defaults. Mirrors the `ctx <- list(...)` block in tab_build().
carve_ctx <- function(data, row_vars, col_vars, tab_vars = rlang::quo(NULL),
                      wt = rlang::quo(NULL), overrides = list()) {
  base <- list(
    data = data, with_filter = FALSE,
    row_vars_quo = row_vars, col_vars_quo = col_vars, tab_vars_quo = tab_vars,
    wt_quo = wt, na_drop_all_quo = rlang::quo(NULL),
    pct = "no", color = "no", color_signif = "ignore", color_ratio_ci = FALSE,
    OR = "no", chi2 = FALSE,
    na = "keep", levels = "all",
    cleannames = NULL, output = "single", other_if_less_than = 0, other_level = "Others",
    ref = "auto", ref2 = "first", comp = "tab", ci = "no", conf_level = 0.95, stars = NULL,
    method_cell = "wilson", method_diff = "newcombe",
    method_ratio = "katz", method_mean_diff = "welch", method_mean_ratio = "robust",
    totaltab = "line", totaltab_name = "Ensemble", totrow = TRUE, totcol = "last",
    total_names = "Total", add_n = TRUE, add_pct = FALSE, digits = 0, subtext = "",
    by_table = FALSE, spread_vars = character(), names_prefix = NULL, names_sort = FALSE
  )
  utils::modifyList(base, overrides)
}

# Phase 9a: the row axis is an OUTER map. tab_transform()/tab_assemble_tables() are now scalar over ONE
# row_var (driven by tab_build_tables()'s per-row_var units), so the whole-ctx composition ends at
# tab_build_tables() rather than threading tab_transform -> tab_assemble on a multi-row_var ctx.
run_stages <- function(ctx) {
  ctx <- tabxplor:::tab_setup(ctx)
  ctx <- tabxplor:::tab_prepare_pop(ctx)
  ctx <- tabxplor:::tab_aggregate(ctx)
  tabxplor:::tab_build_tables(ctx)
}

testthat::test_that("the five stages compose == tab_build() (factor, numeric, mixed+tab_vars)", {
  gss <- carve_gss()

  # factor x factor
  testthat::expect_equal(
    run_stages(carve_ctx(gss, rlang::quo(marital), rlang::quo(race),
                         overrides = list(pct = "row", color = "diff", ci = "diff"))),
    tabxplor:::tab_build(gss, marital, race, pct = "row", color = "diff", ci = "diff"))

  # factor x numeric (means path)
  testthat::expect_equal(
    run_stages(carve_ctx(gss, rlang::quo(marital), rlang::quo(age),
                         overrides = list(pct = "row", ci = "cell", chi2 = TRUE))),
    tabxplor:::tab_build(gss, marital, age, pct = "row", ci = "cell", chi2 = TRUE))

  # mixed factor+numeric col_vars, weighted, tab_vars, chi2
  testthat::expect_equal(
    run_stages(carve_ctx(gss, rlang::quo(marital), rlang::quo(c(race, age)),
                         tab_vars = rlang::quo(year), wt = rlang::quo(w),
                         overrides = list(pct = "row", chi2 = TRUE))),
    tabxplor:::tab_build(gss, marital, c(race, age), year, wt = w, pct = "row", chi2 = TRUE))
})

testthat::test_that("each stage adds its cache-tier ctx fields (the 7e seam contract)", {
  gss <- carve_gss()
  ctx <- carve_ctx(gss, rlang::quo(marital), rlang::quo(race),
                   overrides = list(pct = "row", color = "diff", ci = "diff"))

  ctx <- tabxplor:::tab_setup(ctx)
  testthat::expect_true(all(c("col_vars_num", "col_vars_text", "tot_cols_type", "pct_vect",
                              "color_diff_OR", "cache_keys") %in% names(ctx)))
  testthat::expect_named(ctx$cache_keys, c("tier0", "tier1_common", "tier2"))

  ctx <- tabxplor:::tab_prepare_pop(ctx)
  testthat::expect_true(all(c("na_text", "na_num", "lv1", "remove_levels") %in% names(ctx)))

  ctx <- tabxplor:::tab_aggregate(ctx)
  testthat::expect_true(all(c("fine_num", "fine_fused") %in% names(ctx)))  # tier 1 (NULL when off)

  # Phase 9a: tab_transform()/tab_assemble_tables() are scalar over ONE row_var -- run them on a
  # per-row_var unit built by tab_rowvar_ctxs() (the outer-map slice), not on the multi-row_var ctx.
  # ctx_update() (single-bracket) so a NULL fine_fused (fuse off) survives as a list element -- a
  # `unit$fine_fused <- NULL` would delete the key and tab_transform's list2env() couldn't find it.
  unit <- tabxplor:::tab_rowvar_ctxs(ctx)[[1]]
  unit <- tabxplor:::ctx_update(unit, list(data = ctx$data, fine_fused = ctx$fine_fused))
  unit <- tabxplor:::tab_transform(unit)
  testthat::expect_true(all(c("tabs_text", "tabs_num", "tests", "chi2_num") %in% names(unit)))
  unit <- tabxplor:::tab_assemble_tables(unit)
  testthat::expect_s3_class(unit$tabs, "tabxplor_tab")

  out <- tabxplor:::tab_build_tables(ctx)
  testthat::expect_s3_class(out, "tabxplor_tab")
})

testthat::test_that("carved stages: default factor path == .by_table raw scan", {
  # Phase 9c: tab() no longer fuses the factor scan (the net-negative opt-in was removed), so the
  # default and `.by_table` paths both raw-scan -- this pins them byte-identical through the CARVED
  # aggregate/transform/assemble boundary (the direct tab_plain(.fine=) seam is test-fuse-parity.R).
  gss <- carve_gss()
  def <- run_stages(carve_ctx(gss, rlang::quo(c(marital, relig)), rlang::quo(race),
                              overrides = list(pct = "row", chi2 = TRUE, output = "list")))
  raw <- run_stages(carve_ctx(gss, rlang::quo(c(marital, relig)), rlang::quo(race),
                              overrides = list(pct = "row", chi2 = TRUE, output = "list",
                                               by_table = TRUE)))
  testthat::expect_equal(def, raw)
})
