# PURPOSE: Phase 8 benchmark — the REAL API. Does tab(data, many_row_vars, col_vars, parallel = W)
#          deliver the §26 survey-range win (~2.5-4x on 10k-60k rows x many tables), byte-identical?
# ROLE: Unlike the §26 PoC (parallel_poc_survey.R, an EXTERNAL hand-dispatched loop), this drives the
#       shipped tab_pmap() seam: ONE tab() call whose per-row_var build fans out over a mirai pool.
# USAGE: source("dev/benchmarks/run_parallel.R")  (.Rbuildignore'd; dev-only).
#        Save output with:  Rscript dev/benchmarks/run_parallel.R > dev/benchmarks/results_2.0.0/phase8_survey.txt
# See: CLAUDE.md Phase 8, dev/tabxplor_2.0.0_decisions.md §26.

PKG <- "d:/Statistiques/github/tabxplor"
suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))
suppressWarnings(suppressMessages({ library(rlang); library(data.table) }))
stopifnot(requireNamespace("mirai", quietly = TRUE))

cat("=== Phase 8 — real tab(parallel=) over many row_vars, survey range (10k-60k) ===\n")
cat("R:", R.version.string, "| physical cores:", parallel::detectCores(logical = FALSE),
    "| logical:", parallel::detectCores(), "\n\n")

gss0 <- as.data.frame(forcats::gss_cat)                          # 21 483 rows
mk_df <- function(n) {
  d <- if (n <= nrow(gss0)) gss0[seq_len(n), , drop = FALSE]
       else gss0[rep_len(seq_len(nrow(gss0)), n), , drop = FALSE]
  # 12 factor row_vars (recycle the 6 real gss_cat factors under fresh names -> the "many
  # exploratory variables" workflow) + a weight. col_vars stay the real race + tvhours.
  facs <- c("marital", "race", "rincome", "partyid", "relig", "denom")
  for (i in seq_len(12)) d[[sprintf("rv%02d", i)]] <- d[[facs[(i - 1L) %% 6L + 1L]]]
  d$w <- 1 + (seq_len(nrow(d)) %% 5L) / 5                         # deterministic weight
  d
}
# row_vars is ONE tidy-select arg -> inject a single c(rv01, ..., rv12) expression (not a `!!!` splice).
RV_SEL <- rlang::expr(c(!!!rlang::syms(sprintf("rv%02d", 1:12))))
build <- function(d, W)
  tab(d, !!RV_SEL, c(race, tvhours), wt = w, pct = "row",
      color = "diff", chi2 = TRUE, parallel = W)

# Dev daemons must run the SAME source (installed tabxplor predates Phase 8). Pre-warm the named pool
# with load_all + single-thread DT; tab_pool_ensure() then reuses it (count matches -> no respawn).
warm <- function(W) {
  t <- system.time({
    mirai::daemons(0, .compute = "tabxplor")
    mirai::daemons(W, .compute = "tabxplor")
    mirai::everywhere(
      { suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))
        data.table::setDTthreads(1L) },
      PKG = PKG, .compute = "tabxplor")
  })[["elapsed"]]
  t
}
timeit <- function(expr, reps = 3L) {
  expr <- substitute(expr); env <- parent.frame(); eval(expr, env)   # warm run (discarded)
  stats::median(replicate(reps, system.time(eval(expr, env))[["elapsed"]]))
}

rows <- list()
for (N in c(10000L, 30000L, 60000L)) {
  d <- mk_df(N)
  data.table::setDTthreads(8L)
  seq_s <- suppressWarnings(timeit(build(d, FALSE)))
  cat(sprintf("\n--- N = %s rows | %.1f MB | serial(DT=8) %.3f s ---\n",
              format(N, big.mark = " "), as.numeric(object.size(d)) / 1024^2, seq_s))

  res <- list()
  for (W in c(4L, 8L)) {
    setup <- warm(W)
    par1  <- suppressWarnings(build(d, W))                    # fresh-call target (pool already warm)
    seq1  <- suppressWarnings(build(d, FALSE))
    ident <- isTRUE(all.equal(par1, seq1))
    batch <- suppressWarnings(timeit(build(d, W)))
    cat(sprintf("  parallel W=%d  warm %.2f s | batch %.3f s | speedup %.2fx | identical %s\n",
                W, setup, batch, seq_s / batch, ident))
    res[[as.character(W)]] <- list(batch = batch, setup = setup, ident = ident)
  }
  mirai::daemons(0, .compute = "tabxplor")
  rows[[length(rows) + 1L]] <- data.frame(
    N = N, serial_s = round(seq_s, 3),
    W4_speedup = round(seq_s / res[["4"]]$batch, 2),
    W8_speedup = round(seq_s / res[["8"]]$batch, 2),
    W4_warm_s  = round(res[["4"]]$setup, 2),
    identical  = res[["4"]]$ident && res[["8"]]$ident)
}
cat("\n=== summary (batch speedup vs serial DT=8) ===\n")
print(do.call(rbind, rows), row.names = FALSE)
cat("\nDONE\n")
