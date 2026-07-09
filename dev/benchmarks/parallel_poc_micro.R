# PURPOSE: Phase 6b research (Layer A) — synthetic micro proof-of-concept for parallelising tab().
# ROLE: Isolate the *mechanics* of process-parallelism vs data.table's own threading, WITHOUT tab(),
#       so the numbers bound what any real implementation could achieve. Companion to
#       parallel_poc_tab.R (Layer B, the realistic end-to-end PoC). Findings -> decisions doc §26.
# KEY CONSTRAINTS:
#   - .Rbuildignore'd standalone script; run in a COLD Rscript (never the warm MCP session).
#   - mirai needs nanonext >= 1.9.0. If the machine's main-lib nanonext is DLL-locked at 1.8.0,
#     install nanonext+mirai into a temp lib and prepend it via R_LIBS_USER (the whole process tree,
#     incl. mirai daemons, then resolves the newer nanonext). future.apply/parallel need no nanonext.
#   - Windows has no fork(): every worker is a separate process; the df must reach it (serialize once
#     into persistent workers, or per task). setDTthreads(1) in workers avoids oversubscription.
# See: CLAUDE.md § 1.4.0 roadmap > Phase 6b research.

suppressWarnings(suppressMessages({
  library(data.table)
}))

BIG_RDS  <- "dev/benchmarks/big_df.rds"
WORKERS  <- c(2L, 4L, 8L, 12L)   # W: worker/core counts to sweep
K        <- 16L                  # number of independent "tables" (grouped scans) in a batch
REPS     <- 3L                   # timing repetitions (median)

n_cores <- parallel::detectCores()
cat("=== Phase 6b Layer A — synthetic parallelism micro-PoC ===\n")
cat("R:", R.version.string, "| logical cores:", n_cores,
    "| DT default threads:", getDTthreads(), "\n\n")

# --- timing helper: median elapsed seconds over `reps` (1 warm-up discarded) -------------------
timeit <- function(expr, reps = REPS) {
  expr <- substitute(expr); env <- parent.frame()
  eval(expr, env)                                   # warm-up
  ts <- replicate(reps, system.time(eval(expr, env))[["elapsed"]])
  stats::median(ts)
}

# --- load fixture -------------------------------------------------------------------------------
stopifnot(file.exists(BIG_RDS))
big <- readRDS(BIG_RDS); setDT(big)
cat("fixture:", format(nrow(big), big.mark = " "), "rows x", ncol(big), "cols;",
    "object.size:", round(as.numeric(object.size(big)) / 1024^2, 1), "MB\n")
ser_mb <- length(serialize(big, NULL)) / 1024^2
cat("serialized size (per-worker transfer):", round(ser_mb, 1), "MB\n\n")

# --- the unit of work: one grouped count/moment scan = one "table" ------------------------------
facs <- c("region", "age_grp", "education", "sex", "response")
grid <- expand.grid(r = facs, c = facs, stringsAsFactors = FALSE)
grid <- grid[grid$r != grid$c, ]
task_specs <- Map(function(r, c) c(r, c), grid$r, grid$c)
task_specs <- task_specs[seq_len(K)]                # 16 distinct (row_var, col_var) pairs

scan_one <- function(DT, spec) {
  DT[, list(n = .N, wn = sum(weight, na.rm = TRUE),
            s1 = sum(score, na.rm = TRUE), s2 = sum(score * score, na.rm = TRUE)),
     keyby = spec]
}
# worker version: reads the pre-loaded DT from the daemon/cluster global, returns a tiny summary
scan_worker <- function(spec) {
  data.table::setDTthreads(1L)
  DT <- get("DT_shared", envir = .GlobalEnv)
  r <- DT[, list(n = .N, wn = sum(weight, na.rm = TRUE),
                 s1 = sum(score, na.rm = TRUE), s2 = sum(score * score, na.rm = TRUE)),
          keyby = spec]
  nrow(r)
}

results <- list()   # collect tidy rows for a final summary

# === A1. data.table thread-scaling curve (one scan) ============================================
cat("--- A1. data.table thread-scaling (single scan: education x age_grp, 48 groups) ---\n")
a1 <- lapply(c(1L, 2L, 4L, 8L, 12L), function(t) {
  setDTthreads(t)
  s <- timeit(scan_one(big, c("education", "age_grp")), reps = 5L)
  data.frame(dt_threads = t, median_s = round(s, 4))
})
a1 <- do.call(rbind, a1)
a1$speedup_vs_1 <- round(a1$median_s[a1$dt_threads == 1] / a1$median_s, 2)
print(a1, row.names = FALSE)
cat("\n")

# === A2. batch of K scans: sequential (DT-threaded) vs process-parallel (1 thread/worker) =======
cat("--- A2. batch of K =", K, "scans: sequential vs process-parallel ---\n")

# sequential baselines
setDTthreads(8L)
seq8 <- timeit(for (sp in task_specs) scan_one(big, sp))
setDTthreads(1L)
seq1 <- timeit(for (sp in task_specs) scan_one(big, sp))
cat(sprintf("sequential DT=8 threads : %.3f s\n", seq8))
cat(sprintf("sequential DT=1 thread  : %.3f s\n", seq1))
results[["seq_dt8"]] <- data.frame(backend = "sequential", workers = 1L, dt_threads = 8L,
                                   setup_s = 0, batch_s = round(seq8, 3))
results[["seq_dt1"]] <- data.frame(backend = "sequential", workers = 1L, dt_threads = 1L,
                                   setup_s = 0, batch_s = round(seq1, 3))

# --- mirai (persistent daemons + df pre-loaded once via everywhere) ---
if (requireNamespace("mirai", quietly = TRUE)) {
  cat("\n[mirai] persistent daemons, df pre-loaded once, setDTthreads(1) per daemon\n")
  for (W in WORKERS) {
    ok <- tryCatch({
      t_setup <- system.time({
        mirai::daemons(W)
        mirai::everywhere({
          suppressWarnings(suppressMessages(library(data.table)))
          data.table::setDTthreads(1L)
          assign("DT_shared", data.table::setDT(x), envir = .GlobalEnv)
        }, x = big)
      })[["elapsed"]]
      bt <- replicate(REPS, system.time(mirai::mirai_map(task_specs, scan_worker)[])[["elapsed"]])
      mirai::daemons(0)
      batch <- stats::median(bt)
      cat(sprintf("  W=%2d  setup(transfer) %.2f s | batch %.3f s | speedup vs seq8 %.2fx\n",
                  W, t_setup, batch, seq8 / batch))
      results[[paste0("mirai_", W)]] <- data.frame(backend = "mirai", workers = W, dt_threads = 1L,
                                                    setup_s = round(t_setup, 2), batch_s = round(batch, 3))
      TRUE
    }, error = function(e) { cat("  mirai W=", W, "ERROR:", conditionMessage(e), "\n"); try(mirai::daemons(0), silent = TRUE); FALSE })
  }
} else cat("\n[mirai] not available — skipped\n")

# --- base parallel (PSOCK cluster + clusterExport once) ---
if (requireNamespace("parallel", quietly = TRUE)) {
  cat("\n[parallel] PSOCK cluster, df exported once, setDTthreads(1) per worker\n")
  for (W in WORKERS) {
    ok <- tryCatch({
      t_setup <- system.time({
        cl <- parallel::makePSOCKcluster(W)
        parallel::clusterEvalQ(cl, { suppressWarnings(suppressMessages(library(data.table))); data.table::setDTthreads(1L) })
        parallel::clusterExport(cl, varlist = "big", envir = environment())
        parallel::clusterEvalQ(cl, assign("DT_shared", data.table::setDT(big), envir = .GlobalEnv))
      })[["elapsed"]]
      bt <- replicate(REPS, system.time(parallel::parLapply(cl, task_specs, scan_worker))[["elapsed"]])
      parallel::stopCluster(cl)
      batch <- stats::median(bt)
      cat(sprintf("  W=%2d  setup(transfer) %.2f s | batch %.3f s | speedup vs seq8 %.2fx\n",
                  W, t_setup, batch, seq8 / batch))
      results[[paste0("parallel_", W)]] <- data.frame(backend = "parallel", workers = W, dt_threads = 1L,
                                                       setup_s = round(t_setup, 2), batch_s = round(batch, 3))
      TRUE
    }, error = function(e) { cat("  parallel W=", W, "ERROR:", conditionMessage(e), "\n"); FALSE })
  }
} else cat("\n[parallel] not available — skipped\n")

# --- future.apply (multisession; globals re-exported per batch call) ---
if (requireNamespace("future.apply", quietly = TRUE)) {
  cat("\n[future.apply] multisession; NB globals re-sent per future_lapply call (not amortised)\n")
  options(parallelly.makeNodePSOCK.rscript_args = "--vanilla")  # clean, fast workers (skip profile)
  for (W in WORKERS) {
    ok <- tryCatch({
      t_setup <- system.time(future::plan(future::multisession, workers = W))[["elapsed"]]
      fworker <- function(spec) { data.table::setDTthreads(1L); nrow(scan_one(big, spec)) }
      bt <- replicate(REPS, system.time(
        future.apply::future_lapply(task_specs, fworker, future.seed = TRUE))[["elapsed"]])
      future::plan(future::sequential)
      batch <- stats::median(bt)
      cat(sprintf("  W=%2d  setup(plan) %.2f s | batch %.3f s | speedup vs seq8 %.2fx\n",
                  W, t_setup, batch, seq8 / batch))
      results[[paste0("future_", W)]] <- data.frame(backend = "future.apply", workers = W, dt_threads = 1L,
                                                     setup_s = round(t_setup, 2), batch_s = round(batch, 3))
      TRUE
    }, error = function(e) { cat("  future W=", W, "ERROR:", conditionMessage(e), "\n"); FALSE })
  }
} else cat("\n[future.apply] not available — skipped\n")

# === A5. oversubscription: mirai W=8, daemon DT threads 1 vs 8 (nested) =========================
if (requireNamespace("mirai", quietly = TRUE)) {
  cat("\n--- A5. oversubscription (W=8 daemons; DT threads 1 vs 8 per daemon) ---\n")
  for (dt_t in c(1L, 8L)) {
    tryCatch({
      mirai::daemons(8L)
      mirai::everywhere({
        suppressWarnings(suppressMessages(library(data.table)))
        data.table::setDTthreads(nt)
        assign("DT_shared", data.table::setDT(x), envir = .GlobalEnv)
      }, x = big, nt = dt_t)
      bt <- replicate(REPS, system.time(mirai::mirai_map(task_specs, function(spec) {
        DT <- get("DT_shared", envir = .GlobalEnv)
        nrow(DT[, list(n = .N, wn = sum(weight, na.rm = TRUE)), keyby = spec])
      })[])[["elapsed"]])
      mirai::daemons(0)
      cat(sprintf("  W=8 x DT=%d threads (=%d logical) : batch %.3f s\n",
                  dt_t, 8L * dt_t, stats::median(bt)))
    }, error = function(e) { cat("  oversub DT=", dt_t, "ERROR:", conditionMessage(e), "\n"); try(mirai::daemons(0), silent = TRUE) })
  }
}

# === A6. memory footprint of persistent worker copies ==========================================
cat("\n--- A6. memory: per-worker df copy footprint ---\n")
obj_mb <- as.numeric(object.size(big)) / 1024^2
cat(sprintf("df in-memory: %.0f MB; serialized: %.0f MB\n", obj_mb, ser_mb))
for (W in WORKERS)
  cat(sprintf("  W=%2d persistent workers each holding a copy -> ~%.1f GB resident (parent + %d x df)\n",
              W, (obj_mb * (W + 1)) / 1024, W))

# === summary table ==============================================================================
cat("\n=== A2 summary (batch of", K, "scans) ===\n")
summ <- do.call(rbind, results)
summ$speedup_vs_seq8 <- round(seq8 / summ$batch_s, 2)
print(summ, row.names = FALSE)
cat("\n(setup_s = one-time worker creation + df transfer; batch_s = median of", REPS,
    "runs of K scans; speedup vs the sequential DT=8 baseline)\n")
cat("\nDONE\n")
