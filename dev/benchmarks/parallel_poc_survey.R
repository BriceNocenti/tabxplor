# PURPOSE: Phase 6b research (confirmation) — does the small-df parallel win hold across tabxplor's
#          ACTUAL target regime: many exploratory tables at once on a typical survey (10k-60k rows)?
# ROLE: Nail the §26 sweet-spot claim (mirai ~3.5x at W=4) at the real survey sizes + commodity W=4,
#       not just gss_cat's 21k. Sequential vs mirai/parallel, batch of 16 colored+chi2 tables.
# See: parallel_poc_tab.R (Layer B), decisions doc §26.

PKG <- "d:/Statistiques/github/tabxplor"
suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))
suppressWarnings(suppressMessages({ library(rlang); library(data.table) }))

cat("=== Phase 6b confirmation — survey-size range (10k-60k), many tables, W=4/8 ===\n")
cat("R:", R.version.string, "| logical cores:", parallel::detectCores(), "\n\n")

gss0 <- as.data.frame(forcats::gss_cat)                 # 21 483 rows
mk_df <- function(n) {                                   # deterministic size via head / row-replication
  if (n <= nrow(gss0)) gss0[seq_len(n), , drop = FALSE]
  else gss0[rep_len(seq_len(nrow(gss0)), n), , drop = FALSE]
}
facs <- c("marital", "race", "rincome", "partyid", "relig", "denom")
g <- expand.grid(r = facs, c = facs, stringsAsFactors = FALSE); g <- g[g$r != g$c, ]
pairs <- Map(function(r, c) c(r, c), g$r, g$c)[1:16]     # 16 real colored tables

build_one <- function(DT, rv, cv)
  tab(DT, !!sym(rv), !!sym(cv), pct = "row", color = "diff", chi2 = TRUE)
build_worker <- function(pair) {
  data.table::setDTthreads(1L)
  DT <- get("DT_shared", envir = .GlobalEnv)
  tabxplor::tab(DT, !!rlang::sym(pair[[1]]), !!rlang::sym(pair[[2]]),
                pct = "row", color = "diff", chi2 = TRUE)
}
timeit <- function(expr, reps = 3L) {
  expr <- substitute(expr); env <- parent.frame(); eval(expr, env)
  stats::median(replicate(reps, system.time(eval(expr, env))[["elapsed"]]))
}

mirai_batch <- function(DT, W) {
  t_setup <- system.time({
    mirai::daemons(W)
    mirai::everywhere({
      suppressWarnings(suppressMessages(devtools::load_all(pkg, quiet = TRUE)))
      data.table::setDTthreads(1L); assign("DT_shared", x, envir = .GlobalEnv)
    }, x = DT, pkg = PKG)
  })[["elapsed"]]
  ident <- {
    par <- mirai::mirai_map(pairs, build_worker)[]
    seql <- lapply(pairs, function(p) build_one(DT, p[[1]], p[[2]]))
    sum(vapply(seq_along(pairs), function(i) !isTRUE(all.equal(seql[[i]], par[[i]])), logical(1)))
  }
  batch <- stats::median(replicate(3, system.time(mirai::mirai_map(pairs, build_worker)[])[["elapsed"]]))
  mirai::daemons(0)
  list(setup = t_setup, batch = batch, ident = ident)
}
parallel_batch <- function(DT, W) {
  t_setup <- system.time({
    cl <- parallel::makePSOCKcluster(W)
    parallel::clusterExport(cl, c("DT", "PKG"), envir = environment())
    parallel::clusterEvalQ(cl, {
      suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))
      data.table::setDTthreads(1L); assign("DT_shared", DT, envir = .GlobalEnv)
    })
  })[["elapsed"]]
  batch <- stats::median(replicate(3, system.time(parallel::parLapply(cl, pairs, build_worker))[["elapsed"]]))
  parallel::stopCluster(cl)
  list(setup = t_setup, batch = batch)
}

rows <- list()
for (N in c(10000L, 30000L, 60000L)) {
  DT <- mk_df(N)
  data.table::setDTthreads(8L)
  seq8 <- timeit({ for (p in pairs) build_one(DT, p[[1]], p[[2]]) })
  cat(sprintf("\n--- N = %s rows | %.1f MB | seq(DT=8) %.3f s (%.3f s/table) ---\n",
              format(N, big.mark = " "), as.numeric(object.size(DT)) / 1024^2, seq8, seq8 / 16))
  m4 <- mirai_batch(DT, 4L)
  cat(sprintf("  mirai    W=4  setup %.2f s | batch %.3f s | speedup %.2fx | fresh %.2f s | ident %d/16\n",
              m4$setup, m4$batch, seq8 / m4$batch, m4$setup + m4$batch, m4$ident))
  p4 <- parallel_batch(DT, 4L)
  cat(sprintf("  parallel W=4  setup %.2f s | batch %.3f s | speedup %.2fx | fresh %.2f s\n",
              p4$setup, p4$batch, seq8 / p4$batch, p4$setup + p4$batch))
  p8 <- parallel_batch(DT, 8L)
  cat(sprintf("  parallel W=8  setup %.2f s | batch %.3f s | speedup %.2fx | fresh %.2f s\n",
              p8$setup, p8$batch, seq8 / p8$batch, p8$setup + p8$batch))
  rows[[length(rows) + 1]] <- data.frame(
    N = N, seq_s = round(seq8, 3),
    mirai_W4 = round(seq8 / m4$batch, 2), par_W4 = round(seq8 / p4$batch, 2),
    par_W8 = round(seq8 / p8$batch, 2), mirai_W4_setup = round(m4$setup, 2))
}
cat("\n=== survey-range summary (batch speedup vs sequential DT=8) ===\n")
print(do.call(rbind, rows), row.names = FALSE)
cat("\nDONE\n")
