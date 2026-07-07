#!/usr/bin/env Rscript
# PURPOSE: (Re)generate the committed small-benchmark baseline that test-benchmark.R compares
#          against in-suite (tests/testthat/benchmark_baseline.csv). Ships with the tests, so it
#          is available under devtools::test() AND devtools::check() (unlike dev/benchmarks/, which is
#          .Rbuildignore'd). Separate from dev/benchmarks/baseline.csv used by dev/benchmarks/run_bench.R.
# USAGE (from package root):  Rscript dev/make_benchmark_baseline.R
#
# CONSCIOUS ACT: run on your reference machine to reset the perf baseline. The in-suite comparison
# is informational (never fails), so regenerate whenever timings have drifted enough that the diffs
# stop being useful, then commit the updated CSV.

pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
source(file.path(pkg, "tests", "testthat", "helper-benchmark.R"))

res <- benchmark_run("small_gss_cat", nrow(forcats::gss_cat), benchmark_small_ops(), iterations = 10L)
out <- res[, c("operation", "median_s", "mem_mb")]

path <- file.path(pkg, "tests", "testthat", "benchmark_baseline.csv")
utils::write.csv(out, path, row.names = FALSE)
cat("wrote ", path, "\n", sep = "")
print(out, row.names = FALSE)
