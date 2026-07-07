#!/usr/bin/env Rscript
# PURPOSE: Deliberate performance harness for the large 8M-row fixture. Times the tab() pipeline
#          and prints a current-vs-baseline comparison in the SAME format as the in-suite small
#          benchmark (test-benchmark.R), via the shared benchmark_print() helper.
# ROLE: Standalone; NOT part of the test suite and NEVER run by R CMD check (dev/benchmarks/ is
#        .Rbuildignore'd). The small fixture is covered in-suite by test-benchmark.R; THIS script
#        is only for the heavy 8M-row run.
#
# USAGE (from package root, e.g. in a dev R session):
#   source("dev/benchmarks/run_bench.R", encoding = "UTF-8")
#   # or: Rscript dev/benchmarks/run_bench.R
#
# Uses bench::mark() if installed (adds memory), else falls back to system.time().
# Baseline is dev/benchmarks/baseline.csv (big_8M rows). To reset it after a deliberate perf change,
# delete that file (or its big_8M rows) and re-run.

pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
# Shared ops/timing/printing helpers (same ones the in-suite benchmark uses).
source(file.path(pkg, "tests", "testthat", "helper-benchmark.R"))
source(file.path(pkg, "dev", "benchmarks", "gen_big_df.R"))

big <- gen_big_df(cache = file.path(pkg, "dev", "benchmarks", "big_df.rds"))
big_ops <- list(
  tab_row_pct    = function() tab(big, region, response, pct = "row"),
  tab_ci         = function() tab(big, region, response, pct = "row", ci = "cell"),
  tab_chi2       = function() tab(big, region, response, pct = "row", chi2 = TRUE),
  tab_num_mean   = function() tab_num(big, region, c(score, income), response, comp = "all"),
  tab_many_multi = function() tab_many(big, region, c(response, age_grp), pct = "row"),
  tab_weighted   = function() tab(big, region, response, wt = weight, pct = "col")
)
big_res <- benchmark_run("big_8M", 8e6L, big_ops, iterations = 3L)

out_dir  <- file.path(pkg, "dev", "benchmarks")
baseline <- file.path(out_dir, "baseline.csv")

# Read the big_8M rows of the baseline, if any.
base_big <- NULL
if (file.exists(baseline)) {
  b <- utils::read.csv(baseline, stringsAsFactors = FALSE)
  if ("dataset" %in% names(b)) b <- b[b$dataset == "big_8M", , drop = FALSE]
  if (nrow(b) > 0) base_big <- b
}

benchmark_print(
  big_res, base_big,
  header = "big tab() benchmark (8,000,000 rows)",
  regen  = "delete dev/benchmarks/baseline.csv, then re-run"
)

# Record this run (git-ignored) and seed the baseline on first big run.
big_res$r_version <- as.character(getRversion())
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
big_res$stamp <- stamp
utils::write.csv(big_res, file.path(out_dir, paste0("results_", stamp, ".csv")), row.names = FALSE)
if (is.null(base_big)) {
  utils::write.csv(big_res, baseline, row.names = FALSE)
  cat("seeded baseline: ", baseline, "\n", sep = "")
}
