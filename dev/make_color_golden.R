#!/usr/bin/env Rscript
# PURPOSE: (Re)generate the COLOR characterization fixtures consumed by test-color-golden.R.
# ROLE: Manual, conscious step of the Phase 5 color-refactor safety net. NOT run by tests/check.
# USAGE (from package root):  Rscript dev/make_color_golden.R
#
# CONSCIOUS ACT: only run when a change to the RENDERED colors is INTENTIONAL. Phase 5 ledger:
#   LOCKED (do not regenerate lightly): c_syn_diff, c_diff, c_or, c_mean_diff_ci,
#     c_mean_after_ci (mean CI-gated modes are byte-identical on symmetric intervals).
#   Phase 10i-B: c_contrib REGENERATED (p-value rows are now display-only, so the built contrib
#     table loses its "pvalue" body row -> its per-cell hex fixture drops that row's cells; every
#     other cell's colour is byte-identical, contrib being computed at build before the row was ever
#     added). The other data rows are unchanged.
#   Phase 19b: ALL 15 regenerated -- the `type` -> `scale` + `pct_base` attribute split (KEY 2).
#     Colour SLOTS are unchanged by construction (the plan reads the stored scale where it used to
#     re-derive the same facts from `type` / `ci_type`); only the stored attributes differ.
#   CONSCIOUSLY REGENERATED at Step 3: c_mean_diff (numeric diff -> Glass's delta), and the
#     pct CI-gated modes c_diff_ci / c_after_ci / c_ci (asymmetric-interval upper-arm fix).
#   Phase 18z16-iv: c_contrib_wt_grey ADDED (nothing regenerated). It closes the gap that let W-B
#     ship: there was NO weighted colour fixture at all, so a residual blind to the sample design
#     moved no golden. Every pre-existing case is unweighted -> basis "n" -> untouched by W-B.
#   Phase 18z4: c_contrib_grey + c_contrib_guar ADDED (new cases, nothing regenerated). The three
#     pre-existing c_contrib* cases all use the default color_signif = "ignore" -- the CA reading,
#     which z4 left byte-identical -- so they did NOT move, which is the mechanical proof that the
#     new adjusted-residual gate and the absolute `residual` scale touch only the two gated policies.
# Afterwards: review `git diff tests/testthat/_color_golden/` and accept deliberately.
# See: dev/new_colors_UI.md §13 (Step 0) ; CLAUDE.md golden regeneration protocol.

pkg <- normalizePath(".", winslash = "/")
devtools::load_all(pkg, quiet = TRUE)
source(file.path(pkg, "tests", "testthat", "helper-fixtures.R"))
source(file.path(pkg, "tests", "testthat", "helper-color-golden.R"))

out_dir <- file.path(pkg, "tests", "testthat", "_color_golden")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cases <- color_golden_cases()
for (nm in names(cases)) {
  obj <- cases[[nm]]()
  saveRDS(obj, file.path(out_dir, paste0(nm, ".rds")), version = 2)
  cat("wrote", nm, "\n")
}
cat("Done: ", length(cases), " color-golden fixtures written to ", out_dir, "\n", sep = "")
