#!/usr/bin/env Rscript
# PURPOSE: (Re)generate the structural golden fixtures consumed by tests/testthat/test-golden.R.
# ROLE: Manual, conscious step of the retro-compatibility safety net. NOT run by tests or check.
# USAGE (from package root):  Rscript dev/make_golden.R
#
# CONSCIOUS ACT: only run this when a change to tab()/tab_num()/tab_many() output is INTENTIONAL
# (e.g. `ref_n` making cross-col_var percentages exact). Afterwards:
#   1. review `git diff tests/testthat/_golden/` and accept the changes deliberately;
#   2. for display snapshots, run the tests then `testthat::snapshot_accept("golden")`.
# See: CLAUDE.md > 2.0.0 roadmap > Golden regeneration protocol.
#
# LEDGER
#   Phase 22c-ii: 26 fixtures move, on TWO members and nothing else (verified field by field against
#   `git archive HEAD` on all 36 cases): `or` and, on the two odds-ratio cases, `display`. An odds
#   ratio needs a 2x2, and on the DEGENERATE MARGIN -- the Total column under pct = "row", the Total
#   row under pct = "col" -- one cell of it is the whole block, so the number the sweep divided
#   compared nothing. It is `NA` there now (tab_apply_reference), which is what stops the html render
#   colouring a 100 % Total column, printing a bogus `OR: 1/1.16 [...]` on hover and naming it in the
#   OR legend; the `display` move is the same cell falling back to its column's own token, having no
#   odds ratio left to show. `_color_golden/c_or.rds` moves for the same reason, on its last row only.
#   The `golden.md` and `render-html.md` snapshots are PURELY ADDITIVE (no line removed, none
#   changed): the exports' new unit header row and its two CSS rules.
#   Phase 22c-i: FOUR fixtures move, on the row INDEX COLUMN's level set and nothing else -- an
#   unused "NA" level disappears from the tables whose row axis has no missing value. num_core()
#   appended it whenever `na = "keep"`, gated on nothing, while plain_core() gated on an actual NA;
#   under `na = "drop_all"` (which drops globally and then hands both leaves "keep") the two leaves
#   therefore built DIFFERENT level sets and the full_join() of the text and numeric blocks aborted
#   on an `ordered` row variable. No cell, no field and no attribute moves. The `golden.md` snapshot
#   moves on two `comp = "all"` cases, where the colour legend now names the baseline it actually
#   uses ("the Total Ensemble row" rather than "the Total row").
#   Phase 22b-xvi: FOUR fixtures move, on ONE column ATTRIBUTE and nothing else -- n_mean, n_mean_w,
#   n_mean_sparse, n_mean_tottab (plus n_ci_tabvars / n_ci_tabvars_all, same reason). `color = "auto"`
#   is now `color = TRUE`, and the declared automatic measure of a NUMERIC column is `ratio`, so
#   `tab_num()`'s default stamps `color = "ratio"` where it stamped `"difference"`. Every field is
#   bit-identical (dev/verify_golden_field_delta.R, 1788 cells).
#   Phase 22b-ii: FOUR fixtures move, on the `display` FIELD and nothing else -- f_ci_cell,
#     n_ci_tabvars, n_ci_tabvars_all, n_mean_ci, where `pct_ci` / `mean_ci` became `ci`. Those two
#     pipeline-only tokens were the last composites in the display grammar that were not templates;
#     `ci = "cell"` stamps the ordinary `{ci}` token now, and `options(tabxplor.ci_print =)` is
#     retired (`display = "base_ci"` / `"base_moe"` are its two notations). VERIFIED field by field
#     against `git archive HEAD` on all 36: no other field, no column attribute, no table attribute.
#     The `golden.md` snapshot does NOT move: tab_totcol_sums() reads what a column ESTIMATES
#     (fmt_est_of()) rather than what it prints, so a `{ci}` cell still counts as the share it
#     brackets and the Total keeps its "100%".
#   Phase 22b-i: ALL 36 regenerated, and NOTHING in the tables moved -- dev/verify_golden_field_
#     delta.R reports "no new field" and "attributes unchanged" on all 1788 cells of all 36 cases.
#     The only delta is the TABLE attribute `meta$render_extras`, whose `add_n = TRUE` became
#     `n = "range"`: the base count is a display MODE now (range / min / no), not a logical.
#   Phase 19h: f_totcol_each REGENERATED, and it alone -- verified by dev/verify_golden_field_delta.R
#     that it is the ONLY case whose shape moves and that no other case changes a field, a column
#     attribute, a `test` column or a `meta` sub-field (1655 cells across the other 35). `totcol =
#     "each"` is an accepted SPELLING of "last" now (exactly one total column since Phase 6), so the
#     fixture went from a per-col_var Total_race/Total_relig pair to the single Total every other
#     table has. The classifier it drove had TWO DEAD identical() arms (a character compared against
#     a list of symbols), so "each" was the only way to reach a shape tab() could not produce.
#   Phase 19b: ALL 36 regenerated -- the per-column attribute `type` split into `scale` + `pct_base`
#     and `ci_type` was deleted (KEY 2). NO field and NO cell value moved: dev/verify_golden_field_
#     delta.R proved, on all 1787 cells, that each stored `scale` is exactly what the deleted
#     est_scale_key() dispatch derived from that column's own (type, ci_type) -- except an OR table's
#     REFERENCE column, which now says `odds_ratio` like its siblings instead of "" (D19). The same
#     pass added `ci_method` (which engine built these bounds; "" = none), proved per column against
#     the invariant "names a method iff the old ci_type was not ''/'no'", and DROPPED meta$ci_settings.
#   Phase 18z8: ALL 36 regenerated -- the record gained a 21st per-cell field `gap_se` (the SE of
#     the gap between a cell's estimate and `obs`). dev/verify_golden_field_delta.R -- the proving
#     script, now COMMITTED rather than rewritten each time -- checked 1787 cells and reported the
#     added, entirely all-NA column as the only delta, with every shared field, every column attribute
#     and every table attribute bit-identical. No display / colour snapshot moved.
#   Phase 18z5: ALL 36 regenerated -- the record gained a 20th per-cell field `obs` (the value a
#     tab_reg cell's estimate is compared to). A script proved the ONLY delta is that added, entirely
#     all-NA column (1787 cells checked): a cross-table never fills it, so every displayed value, CI
#     bound and table attribute is byte-identical and NO display / colour snapshot moved.
#   Phase 18z4: f_color_contrib REGENERATED, and it alone -- verified field by field across all 36
#     cases that the ONLY delta is its `pvalue` field, moving from the Pearson residual to the
#     adjusted standardized (Haberman) one. Its RENDERED output is byte-identical (that case uses the
#     default color_signif = "ignore", where the p-value drives nothing), which is why no display
#     snapshot moved with it.

pkg <- normalizePath(".", winslash = "/")
devtools::load_all(pkg, quiet = TRUE)
source(file.path(pkg, "tests", "testthat", "helper-fixtures.R"))
source(file.path(pkg, "tests", "testthat", "helper-golden.R"))

out_dir <- file.path(pkg, "tests", "testthat", "_golden")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cases <- golden_cases()
for (nm in names(cases)) {
  obj <- cases[[nm]]()
  saveRDS(obj, file.path(out_dir, paste0(nm, ".rds")), version = 2)
  cat("wrote", nm, "\n")
}
cat("Done: ", length(cases), " golden fixtures written to ", out_dir, "\n", sep = "")
