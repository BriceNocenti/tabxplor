# PURPOSE: Phase 6b research (Layer B) — realistic end-to-end PoC for parallelising tab() over row_vars.
# ROLE: Build a BATCH of real, colored tabxplor tables sequentially vs across processes, to measure the
#       true win (scan + the CPU-bound fmt/chi2 overhead that Layer A's pure scan lacked). Byte-identity
#       is asserted BEFORE any timing. Companion to parallel_poc_micro.R. Findings -> decisions doc §26.
# KEY CONSTRAINTS:
#   - .Rbuildignore'd standalone script; COLD Rscript only.
#   - Parent AND workers must run the SAME dev source: both devtools::load_all() the package, so parallel
#     output is byte-identical to sequential (the installed CRAN 1.3.1 would diverge).
#   - mirai needs nanonext >= 1.9.0 (see parallel_poc_micro.R header for the R_LIBS_USER temp-lib trick).
#   - Persistent workers, df pre-loaded once, setDTthreads(1) per worker. setup_s (transfer + load_all) is
#     reported SEPARATELY from batch_s, because the verdict differs for a fresh call vs reused workers.
# See: CLAUDE.md § 1.4.0 roadmap > Phase 6b research.

PKG <- "d:/Statistiques/github/tabxplor"
suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))
suppressWarnings(suppressMessages({ library(data.table); library(rlang) }))

WORKERS <- c(2L, 4L, 8L, 12L)
REPS    <- 2L
cat("=== Phase 6b Layer B — realistic tab() parallelism PoC ===\n")
cat("R:", R.version.string, "| logical cores:", parallel::detectCores(),
    "| DT default threads:", getDTthreads(), "\n\n")

# --- build one real colored table for a (row_var, col_var) pair -------------------------------
# rich settings = the "exploratory tables with color helpers" workflow: pct + diff color + chi2.
build_one <- function(DT, rv, cv) {
  tab(DT, !!rlang::sym(rv), !!rlang::sym(cv), pct = "row", color = "diff", chi2 = TRUE)
}
# worker: read pre-loaded DT from global, cap threads, build the table, return the full tab
build_worker <- function(pair) {
  data.table::setDTthreads(1L)
  DT <- get("DT_shared", envir = .GlobalEnv)
  tabxplor::tab(DT, !!rlang::sym(pair[[1]]), !!rlang::sym(pair[[2]]),
                pct = "row", color = "diff", chi2 = TRUE)
}

timeit <- function(expr, reps = REPS) {
  expr <- substitute(expr); env <- parent.frame()
  eval(expr, env)
  ts <- replicate(reps, system.time(eval(expr, env))[["elapsed"]])
  stats::median(ts)
}

# --- run one dataset through the whole grid ---------------------------------------------------
run_dataset <- function(label, DT, pairs, workers = WORKERS,
                        backends = c("mirai", "parallel", "future.apply")) {
  # NB keep DT a data.frame/tibble: tab() errors on a data.table input (pre-existing tab() bug,
  # tab_num col_vars tidyselect -> "Selections can't have missing values"). tab() setDT's its own
  # narrowed copy internally, so a data.frame is the normal, correct input.
  K <- length(pairs)
  obj_mb <- as.numeric(object.size(DT)) / 1024^2
  cat(sprintf("\n########## dataset: %s | %s rows x %d cols | %.0f MB | K=%d tables ##########\n",
              label, format(nrow(DT), big.mark = " "), ncol(DT), obj_mb, K))

  # canonical sequential result (DT default threads) + byte-identity reference
  setDTthreads(8L)
  seq_list <- lapply(pairs, function(p) build_one(DT, p[[1]], p[[2]]))
  names(seq_list) <- vapply(pairs, function(p) paste(p, collapse = "_x_"), "")
  seq8 <- timeit({ for (p in pairs) build_one(DT, p[[1]], p[[2]]) })
  setDTthreads(1L); seq1 <- timeit({ for (p in pairs) build_one(DT, p[[1]], p[[2]]) })
  cat(sprintf("sequential DT=8 : %.3f s | DT=1 : %.3f s  (%.2fs/table)\n", seq8, seq1, seq8 / K))

  out <- list(data.frame(dataset = label, backend = "sequential", workers = 1L,
                         setup_s = 0, batch_s = round(seq8, 3), speedup = 1))

  # ---- mirai ----
  if ("mirai" %in% backends && requireNamespace("mirai", quietly = TRUE)) {
    for (W in workers) tryCatch({
      t_setup <- system.time({
        mirai::daemons(W)
        mirai::everywhere({
          suppressWarnings(suppressMessages(devtools::load_all(pkg, quiet = TRUE)))
          data.table::setDTthreads(1L)
          assign("DT_shared", x, envir = .GlobalEnv)   # keep as data.frame (see run_dataset note)
        }, x = DT, pkg = PKG)
      })[["elapsed"]]
      # identity gate (first W only)
      if (W == workers[1]) {
        par_list <- mirai::mirai_map(pairs, build_worker)[]
        names(par_list) <- names(seq_list)
        diffs <- sum(vapply(seq_along(pairs), function(i)
          !isTRUE(all.equal(seq_list[[i]], par_list[[i]], check.attributes = TRUE)), logical(1)))
        cat(sprintf("[mirai] byte-identity: %d/%d tables differ from sequential%s\n",
                    diffs, K, if (diffs == 0) "  OK" else "  !! FAIL"))
        if (diffs > 0) print(waldo::compare(seq_list[[1]], par_list[[1]]))
      }
      bt <- replicate(REPS, system.time(mirai::mirai_map(pairs, build_worker)[])[["elapsed"]])
      mirai::daemons(0)
      batch <- stats::median(bt)
      cat(sprintf("[mirai]    W=%2d  setup %.2f s | batch %.3f s | batch-speedup %.2fx | fresh-call(setup+batch) %.2f s\n",
                  W, t_setup, batch, seq8 / batch, t_setup + batch))
      out[[length(out) + 1]] <- data.frame(dataset = label, backend = "mirai", workers = W,
                                            setup_s = round(t_setup, 2), batch_s = round(batch, 3),
                                            speedup = round(seq8 / batch, 2))
    }, error = function(e) { cat("[mirai] W=", W, "ERROR:", conditionMessage(e), "\n"); try(mirai::daemons(0), silent = TRUE) })
  }

  # ---- base parallel ----
  if ("parallel" %in% backends) {
    for (W in workers) tryCatch({
      t_setup <- system.time({
        cl <- parallel::makePSOCKcluster(W)
        parallel::clusterExport(cl, varlist = c("DT", "PKG"), envir = environment())
        parallel::clusterEvalQ(cl, {
          suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))
          data.table::setDTthreads(1L)
          assign("DT_shared", DT, envir = .GlobalEnv)   # keep as data.frame (see run_dataset note)
        })
      })[["elapsed"]]
      bt <- replicate(REPS, system.time(parallel::parLapply(cl, pairs, build_worker))[["elapsed"]])
      parallel::stopCluster(cl)
      batch <- stats::median(bt)
      cat(sprintf("[parallel] W=%2d  setup %.2f s | batch %.3f s | batch-speedup %.2fx | fresh-call %.2f s\n",
                  W, t_setup, batch, seq8 / batch, t_setup + batch))
      out[[length(out) + 1]] <- data.frame(dataset = label, backend = "parallel", workers = W,
                                            setup_s = round(t_setup, 2), batch_s = round(batch, 3),
                                            speedup = round(seq8 / batch, 2))
    }, error = function(e) cat("[parallel] W=", W, "ERROR:", conditionMessage(e), "\n"))
  }

  # ---- future.apply (expected worse: globals resent per call) ----
  if ("future.apply" %in% backends && requireNamespace("future.apply", quietly = TRUE)) {
    options(parallelly.makeNodePSOCK.rscript_args = "--vanilla")
    for (W in workers) tryCatch({
      t_setup <- system.time({
        future::plan(future::multisession, workers = W)
        # warm-up: load_all + pre-load on each persistent worker (untimed batch)
        invisible(future.apply::future_lapply(seq_len(W), function(i) {
          suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))
          data.table::setDTthreads(1L); TRUE
        }, future.seed = TRUE))
      })[["elapsed"]]
      fworker <- function(pair) tabxplor::tab(DT, !!rlang::sym(pair[[1]]), !!rlang::sym(pair[[2]]),
                                              pct = "row", color = "diff", chi2 = TRUE)
      bt <- replicate(REPS, system.time(
        future.apply::future_lapply(pairs, fworker, future.seed = TRUE))[["elapsed"]])
      future::plan(future::sequential)
      batch <- stats::median(bt)
      cat(sprintf("[future]   W=%2d  setup %.2f s | batch %.3f s | batch-speedup %.2fx | fresh-call %.2f s\n",
                  W, t_setup, batch, seq8 / batch, t_setup + batch))
      out[[length(out) + 1]] <- data.frame(dataset = label, backend = "future.apply", workers = W,
                                            setup_s = round(t_setup, 2), batch_s = round(batch, 3),
                                            speedup = round(seq8 / batch, 2))
    }, error = function(e) { cat("[future] W=", W, "ERROR:", conditionMessage(e), "\n"); try(future::plan(future::sequential), silent = TRUE) })
  }

  cat(sprintf("[memory] %.0f MB/df -> W=8 persistent workers ~%.1f GB resident (parent + 8 x df copy)\n",
              obj_mb, obj_mb * 9 / 1024))
  do.call(rbind, out)
}

# --- (row_var, col_var) pair list from a set of factor columns --------------------------------
make_pairs <- function(facs, K = NULL) {
  g <- expand.grid(r = facs, c = facs, stringsAsFactors = FALSE); g <- g[g$r != g$c, ]
  pl <- Map(function(r, c) c(r, c), g$r, g$c)
  if (!is.null(K)) pl <- pl[seq_len(min(K, length(pl)))]
  pl
}

# ============================ BIG df (8M) ============================
big <- readRDS("dev/benchmarks/big_df.rds")
big_facs <- c("region", "age_grp", "education", "sex", "response")
big_pairs <- make_pairs(big_facs, K = 16L)   # 16 real colored tables
res_big <- run_dataset("big_8M", big, big_pairs, workers = WORKERS)

# few-tables case (setup dominates): only 2 tables, big df, mirai/parallel at W=8
res_few <- run_dataset("big_8M_fewtab", big, make_pairs(c("region", "response")),
                       workers = 8L, backends = c("mirai", "parallel"))

# ============================ SMALL df (gss_cat 21k) ============================
gss <- as.data.frame(forcats::gss_cat)
gss_facs <- c("marital", "race", "rincome", "partyid", "relig", "denom")
gss_pairs <- make_pairs(gss_facs, K = 16L)
res_small <- run_dataset("small_gss", gss, gss_pairs, workers = c(4L, 8L),
                         backends = c("mirai", "parallel"))

# ============================ summary ============================
cat("\n\n=== Layer B summary (batch_s = median of", REPS, "runs; speedup = seq8 / batch_s) ===\n")
print(rbind(res_big, res_few, res_small), row.names = FALSE)
cat("\nDONE\n")
