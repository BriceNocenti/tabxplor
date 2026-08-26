# PURPOSE: Shared small-benchmark ops + timing helper.
# ROLE: Single definition used by BOTH the informational in-suite test (test-benchmark.R)
#        and the standalone big-data harness (dev/benchmarks/run_bench.R). Lives here (not in
#        dev/benchmarks/) because dev/benchmarks/ is .Rbuildignore'd and cannot be sourced from the
#        built/installed package, whereas test helpers ship with the tests.
# KEY CONSTRAINTS:
#   - `bench` is Suggests-only: benchmark_measure() degrades to system.time() without it.
#   - Timings are informational; nothing here should ever fail a test.

# Gate for the informational benchmark blocks: OPT-IN, off by default.
#
# Why not skip_on_cran() alone (what these used before): NOT_CRAN is set to "true" by BOTH
# devtools::test() and devtools::check() (its literal default is env_vars = c(NOT_CRAN = "true")),
# and by r-lib/actions. So skip_on_cran() only ever skipped on the CRAN farm -- the timings still
# ran on every local test run and every CI job, costing ~46s (21% of the suite) to print numbers
# nobody reads mid-development, and asserting nothing. They are a signal to READ deliberately, so
# they should be requested deliberately. Two further reasons this has to be an explicit gate:
#   - under Config/testthat/parallel, stdout from test files is DISCARDED, so the printed
#     comparison -- the block's entire output -- would silently vanish;
#   - timings from a parallel run are meaningless anyway (workers contend for cores).
# Run them with:  TABXPLOR_BENCH=true (and serial, e.g. TESTTHAT_PARALLEL=false)
#   Sys.setenv(TABXPLOR_BENCH = "true"); devtools::test(filter = "benchmark")
# The heavy 8M-row harness stays where it was: dev/benchmarks/run_bench.R.
skip_unless_benchmarks <- function() {
  testthat::skip_if_not(
    isTRUE(as.logical(Sys.getenv("TABXPLOR_BENCH", "false"))),
    "informational benchmarks are opt-in (set TABXPLOR_BENCH=true)"
  )
}

# Representative small operations on forcats::gss_cat (deterministic).
benchmark_small_ops <- function() {
  gss <- forcats::gss_cat
  list(
    tab_row_pct    = function() tab(gss, marital, race, pct = "row"),
    tab_ci         = function() tab(gss, marital, race, pct = "row", ci = "cell"),
    tab_chi2       = function() tab(gss, marital, race, pct = "row", test = TRUE),
    tab_num_mean   = function() tab_num(gss, race, c(age, tvhours), marital, comp = "all"),
    tab_many_multi = function() tab(gss, marital, c(race, relig), pct = "row"),  # op label kept for baseline continuity
    tab_kable      = function() tab_kable(tab(gss, marital, race, pct = "row"))
  )
}

# Default jmvtab option list (jamovi UI defaults) with overrides -- shared by the benchmark ops.
# ⚠ Keep it in step with `jamovi/jmvtab.a.yaml`: an option this list spells wrongly is simply absent,
# so the benchmark measures a DIFFERENT table without saying so. It had drifted since 19k (`OR` and
# `chi2` were retired options, the four interval methods were renamed in 20g-i) -- which is exactly
# the class of drift test-jamovi-vocabulary.R now checks on the module's own side.
benchmark_jmvtab_opts <- function(...) {
  o <- list(row_vars = character(), col_vars = character(), tab_vars = character(), wt = character(),
            pct = "no", color = "no", color_signif = "ignore", test = FALSE, anova = "welch",
            na = "keep", levels = "all", ref = "auto", ref2 = "first", comp = "tab", ci = "auto",
            conf_level = 0.95, stars = TRUE,
            ci_method_cell = "wilson", ci_method_diff = "newcombe",
            ci_method_mean_diff = "welch", ci_method_mean_ratio = "robust",
            totaltab = "line", digits = 0, other_if_less_than = 0, add_pct = FALSE,
            subtext = "", output_list = FALSE, cleannames = TRUE, display = "auto",
            total_names = c(row = "Total", col = "Total", tab = "Ensemble", other = "Others"))
  utils::modifyList(o, list(...))
}

# jmvtab LIVE-UI ops: the cost the user feels when changing one option with the cache WARM. Baseline
# = partyid x (race, marital, relig), pct = "row", color = "diff" (Phase 7e). Each op is a warm
# jmvtab_build() for a single-argument change, plus the tab_kable() render (currently the dominant
# cost). Tracks the effect of future render / pipeline optimisations. Deterministic (gss_cat + fixed
# opts). The heavy tab_kable render is the reason a warm change is not yet "instant".
benchmark_jmvtab_ops <- function(row_vars = "partyid") {
  d      <- forcats::gss_cat
  base   <- benchmark_jmvtab_opts(row_vars = row_vars, col_vars = c("race", "marital", "relig"),
                                  pct = "row", color = "diff")
  quiet  <- function(x) suppressWarnings(suppressMessages(x))       # perf timing, not a correctness check
  store  <- quiet(jmvtab_build(d, base, NULL))$store                # warm the cache once
  r_base <- quiet(jmvtab_build(d, base, store))$tabs                # a built table for the render op
  vary   <- function(...) { o <- utils::modifyList(base, list(...))
                            function() quiet(jmvtab_build(d, o, store)) }
  list(
    jmv_build_baseline = vary(),
    jmv_change_pct     = vary(pct = "col"),
    jmv_change_color   = vary(color = "ratio"),
    jmv_change_ref     = vary(ref = "1"),
    jmv_change_digits  = vary(digits = 1),
    # Match jmvtab .render_html: tooltips off (the interactive JS is inert in Jamovi and doubles the
    # render time). Still the dominant per-interaction cost; the CSS-only rewrite is Phase 8.
    jmv_render_kable   = function() tab_kable(r_base, position = "left", tooltips = FALSE)
  )
}

# The BIG exploratory table-of-tables (real-world size): 3 row_vars (partyid + rincome + year) x the
# same 3 col_vars. Same interactions; its own frozen baseline so the small one stays a stable
# reference. In Jamovi UI a warm change on this table is currently ~2s (improvable, Phase 8 render).
benchmark_jmvtab_big_ops <- function() benchmark_jmvtab_ops(c("partyid", "rincome", "year"))

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
