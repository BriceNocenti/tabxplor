# PURPOSE: Informational micro-benchmark of the small (gss_cat) tab() pipeline. NEVER fails --
#          it prints current timings alongside a committed baseline (with diffs) so a perf
#          regression is *visible* during normal test runs and `devtools::check()`. A signal to
#          read, not an assertion.
# ROLE: Lightweight, always-green perf lens. The heavy 8M-row benchmark lives in
#        dev/benchmarks/run_bench.R (run deliberately via source()).
# KEY CONSTRAINTS:
#   - Baseline is tests/testthat/benchmark_baseline.csv (ships with the tests, unlike dev/benchmarks/).
#     Regenerate it with dev/make_benchmark_baseline.R.
# See: helper-benchmark.R, dev/make_benchmark_baseline.R, CLAUDE.md > 1.4.0 roadmap.

testthat::test_that("small tab() pipeline timings (informational, never fails)", {
  # skip_on_cran() keeps timings off the CRAN farm (variance/time limits) but STILL runs under a
  # local devtools::check() and devtools::test(), where NOT_CRAN is set. Remove it to run on CRAN too.
  testthat::skip_on_cran()

  n   <- nrow(forcats::gss_cat)
  cur <- benchmark_run("small_gss_cat", n, benchmark_small_ops(), iterations = 3L)

  base_path <- testthat::test_path("benchmark_baseline.csv")
  base <- if (file.exists(base_path)) utils::read.csv(base_path, stringsAsFactors = FALSE) else NULL

  benchmark_print(
    cur, base,
    header = paste0("small tab() benchmark (forcats::gss_cat, ", n, " rows)"),
    regen  = "dev/make_benchmark_baseline.R"
  )

  testthat::succeed("printed small benchmark timings")
})
