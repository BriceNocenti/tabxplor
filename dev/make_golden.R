#!/usr/bin/env Rscript
# PURPOSE: (Re)generate the structural golden fixtures consumed by tests/testthat/test-golden.R.
# ROLE: Manual, conscious step of the retro-compatibility safety net. NOT run by tests or check.
# USAGE (from package root):  Rscript dev/make_golden.R
#
# CONSCIOUS ACT: only run this when a change to tab()/tab_num()/tab_many() output is INTENTIONAL
# (e.g. `ref_n` making cross-col_var percentages exact). Afterwards:
#   1. review `git diff tests/testthat/_golden/` and accept the changes deliberately;
#   2. for display snapshots, run the tests then `testthat::snapshot_accept("golden")`.
# See: CLAUDE.md > 1.4.0 roadmap > Golden regeneration protocol.

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
