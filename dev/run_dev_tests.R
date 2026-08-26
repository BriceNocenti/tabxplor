# PURPOSE: run the second test suite -- the tests that are real testthat files but do not belong in
#   the shipped one.
# ROLE: `tests/testthat/` is the package's CONTRACT: it must fail when a user-visible fact changes,
#   must not fail when an internal is redesigned, and must stay fast enough to run on every edit.
#   Everything that fails one of those three lives here instead, unchanged and still runnable:
#     - SOURCE-TREE LINT      reads R/, jamovi/*.yaml, jamovi/js/ -- absent from a built package, so
#                             it can only ever skip under R CMD check.
#     - PHASE-DEFECT SUITES   one historical 2.0.0 bug each; the fact each guards now belongs to the
#                             subsystem file that owns it.
#     - INTERNAL-SEAM PARITY  pins a seam that is free to be redesigned; test-golden.R already locks
#                             the output those seams produce.
#     - SECONDARY PARITY      the exhaustive arm of a statistical engine's parity. The CANONICAL case
#                             per engine stays in the suite -- it is the only assurance of statistical
#                             soundness for future dev. What is here is the sweep around it, plus the
#                             parity whose oracle is no longer a declared dependency (DescTools).
#     - EXHAUSTIVE SWEEPS     every argument value, where the shipped file keeps a representative
#                             slice. Same spirit as dev/verify_reg_invariants.R.
# USAGE: OMP_NUM_THREADS=1 Rscript dev/run_dev_tests.R          (run it alone; see CLAUDE.md Testing)
#   Optional first argument = a testthat `filter =` regex.
#   Exits non-zero on any failure, so it can be dropped into dev/release_checklist.md.
# See: CLAUDE.md section "Testing" for what belongs where.

args <- commandArgs(trailingOnly = TRUE)
# The package root: walk up from the working directory to the DESCRIPTION, so the script runs from
# anywhere inside the checkout.
root <- normalizePath(".")
while (!file.exists(file.path(root, "DESCRIPTION")) && dirname(root) != root) root <- dirname(root)
stopifnot(file.exists(file.path(root, "DESCRIPTION")))

Sys.setenv(NOT_CRAN = "true")
suppressMessages(devtools::load_all(root, quiet = TRUE))

res <- testthat::test_dir(
  file.path(root, "dev", "tests", "testthat"),
  package        = "tabxplor",
  load_package   = "none",
  filter         = if (length(args)) args[[1]] else NULL,
  stop_on_failure = FALSE
)

d <- as.data.frame(res)
bad <- sum(d$failed) + sum(d$error)
cat(sprintf("\ndev suite: FAIL %d | PASS %d | SKIP %d\n", bad, sum(d$passed), sum(d$skipped)))
if (bad > 0) quit(status = 1L)
