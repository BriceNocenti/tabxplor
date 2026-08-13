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
