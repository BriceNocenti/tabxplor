#!/usr/bin/env Rscript
# PURPOSE: (Re)generate the COLOR characterization fixtures consumed by test-color-golden.R.
# ROLE: Manual, conscious step of the Phase 5 color-refactor safety net. NOT run by tests/check.
# USAGE (from package root):  Rscript dev/make_color_golden.R
#
# CONSCIOUS ACT: only run when a change to the RENDERED colors is INTENTIONAL. Phase 5 ledger:
#   LOCKED (do not regenerate lightly): c_syn_diff, c_diff, c_contrib, c_or, c_mean_diff_ci,
#     c_mean_after_ci (mean CI-gated modes are byte-identical on symmetric intervals).
#   CONSCIOUSLY REGENERATED at Step 3: c_mean_diff (numeric diff -> Glass's delta), and the
#     pct CI-gated modes c_diff_ci / c_after_ci / c_ci (asymmetric-interval upper-arm fix).
# Afterwards: review `git diff tests/testthat/_color_golden/` and accept deliberately.
# See: dev/new_colors_UI.md §13 (Step 0) ; CLAUDE.md golden regeneration protocol.

pkg <- normalizePath(".", winslash = "/")
devtools::load_all(pkg, quiet = TRUE)
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
