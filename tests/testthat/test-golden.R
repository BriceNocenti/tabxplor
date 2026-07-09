# PURPOSE: Characterization guardrail. Locks the CURRENT output of tab()/tab_num()/tab_many()
#          across an argument matrix, so ambitious internal refactors reproduce it exactly
#          unless a change is deliberately accepted (and the fixtures regenerated).
# ROLE: Core retro-compatibility net for tabxplor 1.4.0.
# KEY CONSTRAINTS:
#   - Structural fixtures live in tests/testthat/_golden/*.rds, produced by dev/make_golden.R.
#   - Display snapshots (tab_md) live in tests/testthat/_snaps/golden.md.
#   - A FAILURE here means output changed. If unintended -> a regression to fix. If intended ->
#     rerun dev/make_golden.R + testthat::snapshot_accept("golden"), reviewing the git diff.
# See: helper-golden.R (the shared case matrix) and CLAUDE.md golden regeneration protocol.
#
# ===========================================================================================
# TRIPWIRE LEDGER -- which 1.4.0 phase consciously regenerates which fixture, and why. On the
# current baseline every fixture below is GREEN; these are the DELIBERATE future changes to
# review at regen time (not bugs). [display] = also has a tab_md snapshot in _snaps/golden.md.
#
# Phase 1a (combined field pass, 15 -> 18 fields: +pvalue +tot_n +ci_inf +ci_sup, rr->ratio,
#   drop ci) changes the vctrs RECORD SHAPE, so ALL *.rds are regenerated once; the display
#   _snaps are held byte-identical via the set_ci/get_ci bounds-shim + NA-defaulted new fields.
#   Rows below name the SEMANTIC/display change layered on top of that structural 1a regen.
#
# Phase 5 (ratio-field repoint, §3): for pct/factor columns the `ratio` field now holds the
#   REFERENCE-RELATIVE ratio cell_pct/ref_pct (= the old `mean` x2 overload) instead of the
#   cross-direction tabs_rr it carried before. So EVERY pct fixture's `ratio` field changes
#   (was NA for non-OR tables, tabs_rr for OR tables) -> ALL pct *.rds regenerated once. `mean`
#   still carries the same value during the transition (Step 6 sets it NA for pct); the DISPLAY
#   is byte-identical (no cell displays the ratio field), so the _snaps are untouched. Coloring
#   is byte-identical (the x2 reads the same value from `ratio` now instead of `mean`).
#
# Phase 5 (color_signif attribute, §9.1): EVERY fixture gains a 9th per-column attribute
#   `color_signif` (default "ignore") -> ALL *.rds regenerated once + the fmt-contract snapshot.
#   Display + coloring byte-identical (the default policy reproduces today's behaviour).
#
#   f_row_pct/f_col_pct/f_all_pct/f_counts/f_ref_first/m_multi/totn_keep/w_weighted -> 1a only
#   f_color_diff    -> 1a only (FACTOR diff-color MUST stay byte-identical: the tripwire that
#                      Phase 5 does NOT disturb factor coloring)
#   f_ci_cell       -> DONE Phase 3a (ci half-width -> asymmetric Wilson ci_inf/ci_sup; cell CI
#                      also drawn on the total column, per decisions §1) [display]
#   f_ci_diff       -> DONE Phase 3a (AC -> Newcombe default interval + universal-inclusion stars) [display]
#   f_or            -> Phase 10/tab_logit (rr->ratio done in 1a; empirical log-OR Wald pvalue + 1/OR
#                      display deferred to the tab_logit phase, NOT 3b)
#   f_chi2/f_subtab -> Phase 3b (table attribute chi2 -> test; chi2 pvalue field populated)
#   f_color_afterci -> DONE Phase 3a (after_ci reads the real CI bounds via get_ci upper arm) [struct]
#   f_color_contrib -> Phase 5 (contrib mode reworked in the diff/ratio color overhaul)
#   n_mean          -> Phase 2 (numeric diff flips ratio -> difference: field + display)
#   n_mean_color    -> Phase 2 then 5 (Phase 2 flips diff field; Phase 5 numeric color="diff"
#                      becomes sd-standardized Glass's delta with mean_diff_breaks)
#   n_mean_ci       -> DONE Phase 3a (mean CI z-half-width -> absolute bounds; z stays for cell,
#                      z -> Welch-t under stars for diff) [display: mean bracket byte-identical]
#   totn_drop       -> Phase 2 (THE motivating case: col_vars with differing non-NA totals;
#                      tot_n makes each cell's % base exact; [min;max] total-col range) [display]
#   f_totcol_each   -> Phase 6 (default becomes ONE total column; totcol="each" -> cosmetic)
#   f_selfcross     -> Phase 2c (tot_n field populated); display byte-identical -- locks the
#                      _colvarbis self-crosstab through the factor aggregate-core reorg
#   totn_row_drop   -> Phase 2c (tot_n populated; each col_var already divides by its OWN Total
#                      in the per-(row_var x col_var) forward pass, so DISPLAY stays identical)
#   n_mean_w        -> Phase 2a (weighted ML var rebuilt from moment-sums, within waldo tolerance)
#                      then 2c (numeric diff flips ratio -> difference); display (mean) identical
#   n_mean_sparse   -> Phase 2a (n<=1 / all-NA: the 0/0 moment-sum form must map to the same NA
#                      that stats::var/mean give) then 2c (numeric diff flip); display identical
#   fmt-contract snapshot (test-fmt-contract.R) -> Phase 1a (15 -> 18 field-vector rewrite; the
#                      8 COLUMN attrs are unaffected -- chi2->test is a TABLE attribute, not here)
# ===========================================================================================

cases <- golden_cases()

testthat::test_that("all golden fixtures are present (else run dev/make_golden.R)", {
  testthat::skip_if_not(dir.exists(golden_dir()),
                        "no _golden fixtures yet - run: Rscript dev/make_golden.R")
  files   <- file.path(golden_dir(), paste0(names(cases), ".rds"))
  missing <- names(cases)[!file.exists(files)]
  testthat::expect_identical(
    missing, character(0),
    info = paste("missing goldens (run dev/make_golden.R):", paste(missing, collapse = ", "))
  )
})

# Structural equality: one test per case.
for (nm in names(cases)) {
  local({
    name <- nm
    testthat::test_that(paste0("golden structure unchanged: ", name), {
      fx <- file.path(golden_dir(), paste0(name, ".rds"))
      testthat::skip_if_not(file.exists(fx),
                            paste0("no golden for '", name, "' - run dev/make_golden.R"))
      testthat::expect_equal(cases[[name]](), readRDS(fx))
    })
  })
}

# Display parity: user-facing markdown output for a representative subset.
for (nm in golden_display_cases) {
  local({
    name <- nm
    testthat::test_that(paste0("golden display (tab_md) unchanged: ", name), {
      testthat::expect_snapshot(cat(tab_md(cases[[name]](), print = FALSE)))
    })
  })
}

# tot_n motivation (Phase 2). The two col_vars of the synthetic frame have DIFFERENT
# non-NA totals, so with na = "drop" the shared total/reference count today comes from the
# LAST col_var only -- an approximation. When the `tot_n` field lands and makes each cell's
# % base exact, the `totn_keep`/`totn_drop` goldens above WILL change: regenerate and accept
# them consciously. This test just pins the root-cause asymmetry.
testthat::test_that("tot_n motivation: col_vars have differing non-NA totals", {
  syn <- golden_syn_df()
  testthat::expect_false(sum(!is.na(syn$h)) == sum(!is.na(syn$k)))
})
