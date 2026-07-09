# PURPOSE: Shared small-benchmark ops + timing helper.
# ROLE: Single definition used by BOTH the informational in-suite test (test-benchmark.R)
#        and the standalone big-data harness (dev/benchmarks/run_bench.R). Lives here (not in
#        dev/benchmarks/) because dev/benchmarks/ is .Rbuildignore'd and cannot be sourced from the
#        built/installed package, whereas test helpers ship with the tests.
# KEY CONSTRAINTS:
#   - `bench` is Suggests-only: benchmark_measure() degrades to system.time() without it.
#   - Timings are informational; nothing here should ever fail a test.

# Representative small operations on forcats::gss_cat (deterministic).
benchmark_small_ops <- function() {
  gss <- forcats::gss_cat
  list(
    tab_row_pct    = function() tab(gss, marital, race, pct = "row"),
    tab_ci         = function() tab(gss, marital, race, pct = "row", ci = "cell"),
    tab_chi2       = function() tab(gss, marital, race, pct = "row", chi2 = TRUE),
    tab_num_mean   = function() tab_num(gss, race, c(age, tvhours), marital, comp = "all"),
    tab_many_multi = function() tab(gss, marital, c(race, relig), pct = "row"),  # op label kept for baseline continuity
    tab_kable      = function() tab_kable(tab(gss, marital, race, pct = "row"))
  )
}

# Median wall-clock seconds (+ MB allocated when 'bench' is available) over `iterations` runs.
benchmark_measure <- function(f, iterations = 5L) {
  if (requireNamespace("bench", quietly = TRUE)) {
    b <- bench::mark(f(), iterations = iterations, check = FALSE, filter_gc = FALSE)
    list(median_s = as.numeric(b$median), mem_mb = as.numeric(b$mem_alloc) / 1024^2)
  } else {
    f()  # warm-up
    ts <- replicate(iterations, system.time(f())[["elapsed"]])
    list(median_s = stats::median(ts), mem_mb = NA_real_)
  }
}

# Run a named list of ops and return a tidy timing data.frame.
benchmark_run <- function(dataset, n_rows, ops, iterations = 5L) {
  rows <- lapply(names(ops), function(op) {
    m <- benchmark_measure(ops[[op]], iterations)
    data.frame(
      dataset = dataset, operation = op, n_rows = n_rows,
      median_s = round(m$median_s, 4), mem_mb = round(m$mem_mb, 1),
      iterations = iterations, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Build the canonical current-vs-baseline comparison table (shared column layout). `base` must
# already be scoped to the same fixture as `cur` (merged by operation, in cur's row order).
benchmark_compare <- function(cur, base) {
  m <- merge(cur[, c("operation", "median_s", "mem_mb")],
             base[, c("operation", "median_s", "mem_mb")],
             by = "operation", suffixes = c("", "_base"))
  m <- m[match(cur$operation, m$operation), , drop = FALSE]
  m <- m[!is.na(m$operation), , drop = FALSE]
  data.frame(
    operation   = m$operation,
    median_s    = round(m$median_s, 4),
    base_s      = round(m$median_s_base, 4),
    diff_s      = round(m$median_s - m$median_s_base, 4),
    mem_mb      = round(m$mem_mb, 1),
    base_mem_mb = round(m$mem_mb_base, 1),
    diff_mem_mb = round(m$mem_mb - m$mem_mb_base, 1),
    stringsAsFactors = FALSE
  )
}

# Single printer used by BOTH the in-suite small benchmark and the standalone big harness, so
# their output is byte-for-byte the same layout. Prints a comparison when `base` is available,
# else the current timings alone. `regen` names how to (re)create the baseline.
benchmark_print <- function(cur, base = NULL, header = NULL, regen = NULL) {
  if (!is.null(header)) cat("\n--- ", header, " ---\n", sep = "")
  if (!is.null(base) && nrow(base) > 0) {
    print(benchmark_compare(cur, base), row.names = FALSE)
    cat("(_s = seconds, _mb = MB allocated; diff = now - baseline",
        if (!is.null(regen)) paste0("; regenerate: ", regen) else "", ")\n", sep = "")
  } else {
    print(cur[, c("operation", "median_s", "mem_mb")], row.names = FALSE)
    cat("(no baseline yet",
        if (!is.null(regen)) paste0("; create it with: ", regen) else "", ")\n", sep = "")
  }
}
