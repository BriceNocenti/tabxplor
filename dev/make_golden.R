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
