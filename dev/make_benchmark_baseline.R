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

# jmvtab live-cache baseline (Phase 7e) -- the warm-cache cost of a single option change.
jmv     <- benchmark_run("jmvtab_gss_cat", nrow(forcats::gss_cat), benchmark_jmvtab_ops(), iterations = 10L)
jmv_out <- jmv[, c("operation", "median_s", "mem_mb")]
jmv_path <- file.path(pkg, "tests", "testthat", "jmvtab_benchmark_baseline.csv")
utils::write.csv(jmv_out, jmv_path, row.names = FALSE)
cat("wrote ", jmv_path, "\n", sep = "")
print(jmv_out, row.names = FALSE)

# jmvtab BIG table-of-tables baseline (3 row_vars x 3 col_vars) -- real-world exploratory size.
jmvb     <- benchmark_run("jmvtab_big_gss_cat", nrow(forcats::gss_cat), benchmark_jmvtab_big_ops(), iterations = 10L)
jmvb_out <- jmvb[, c("operation", "median_s", "mem_mb")]
jmvb_path <- file.path(pkg, "tests", "testthat", "jmvtab_big_benchmark_baseline.csv")
utils::write.csv(jmvb_out, jmvb_path, row.names = FALSE)
cat("wrote ", jmvb_path, "\n", sep = "")
print(jmvb_out, row.names = FALSE)
