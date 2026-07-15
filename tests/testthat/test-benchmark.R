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
  testthat::skip_on_cran()
  skip_unless_benchmarks()   # opt-in: TABXPLOR_BENCH=true (see helper-benchmark.R for why)

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

testthat::test_that("jmvtab live-cache timings (informational, never fails)", {
  # The live-UI cost the user feels when changing one option with the cache WARM (Phase 7e). Baseline
  # partyid x (race, marital, relig), pct = "row", color = "diff" on full gss_cat. Currently dominated
  # by the tab_kable render (jmv_render_kable) -- this benchmark tracks that as it is optimised.
  testthat::skip_on_cran()
  skip_unless_benchmarks()   # opt-in: TABXPLOR_BENCH=true (see helper-benchmark.R for why)

  n   <- nrow(forcats::gss_cat)
  cur <- benchmark_run("jmvtab_gss_cat", n, benchmark_jmvtab_ops(), iterations = 3L)

  base_path <- testthat::test_path("jmvtab_benchmark_baseline.csv")
  base <- if (file.exists(base_path)) utils::read.csv(base_path, stringsAsFactors = FALSE) else NULL

  benchmark_print(
    cur, base,
    header = paste0("jmvtab live-cache benchmark (forcats::gss_cat, ", n, " rows; warm cache)"),
    regen  = "dev/make_benchmark_baseline.R"
  )

  testthat::succeed("printed jmvtab benchmark timings")
})

testthat::test_that("jmvtab BIG table-of-tables timings (informational, never fails)", {
  # Real-world exploratory size: 3 row_vars (partyid + rincome + year) x (race, marital, relig),
  # pct = "row", color = "diff". Warm change ~2s in the Jamovi UI today (improvable). Its own frozen
  # baseline (jmvtab_big_benchmark_baseline.csv) so the small benchmark stays a stable reference.
  testthat::skip_on_cran()
  skip_unless_benchmarks()   # opt-in: TABXPLOR_BENCH=true (see helper-benchmark.R for why)

  n   <- nrow(forcats::gss_cat)
  cur <- benchmark_run("jmvtab_big_gss_cat", n, benchmark_jmvtab_big_ops(), iterations = 3L)

  base_path <- testthat::test_path("jmvtab_big_benchmark_baseline.csv")
  base <- if (file.exists(base_path)) utils::read.csv(base_path, stringsAsFactors = FALSE) else NULL

  benchmark_print(
    cur, base,
    header = paste0("jmvtab BIG benchmark (3 row_vars x 3 col_vars, forcats::gss_cat, ", n, " rows; warm cache)"),
    regen  = "dev/make_benchmark_baseline.R"
  )

  testthat::succeed("printed jmvtab big-table benchmark timings")
})
