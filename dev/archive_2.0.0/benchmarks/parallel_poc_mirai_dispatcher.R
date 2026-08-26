# PURPOSE: Phase 6b research (follow-up) — is mirai's DEFAULT DISPATCHER the reason it plateaus at
#          ~1.25x while base parallel reaches ~2.5x on the same batch of tab() builds?
# ROLE: Compare mirai daemons(W) [dispatcher on] vs daemons(W, dispatcher=FALSE) [direct round-robin],
#       same 16-table batch on the 8M fixture. Findings -> decisions doc §26.
# See: parallel_poc_tab.R (the main Layer B), CLAUDE.md § Phase 6b research.

PKG <- "d:/Statistiques/github/tabxplor"
suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))
suppressWarnings(suppressMessages({ library(mirai); library(rlang); library(data.table) }))

big <- readRDS("dev/benchmarks/big_df.rds")
facs <- c("region", "age_grp", "education", "sex", "response")
g <- expand.grid(r = facs, c = facs, stringsAsFactors = FALSE); g <- g[g$r != g$c, ]
pairs <- Map(function(r, c) c(r, c), g$r, g$c)[1:16]

build_worker <- function(pair) {
  data.table::setDTthreads(1L)
  DT <- get("DT_shared", envir = .GlobalEnv)
  tabxplor::tab(DT, !!rlang::sym(pair[[1]]), !!rlang::sym(pair[[2]]),
                pct = "row", color = "diff", chi2 = TRUE)
}
setDTthreads(8L)
seq8 <- {
  for (p in pairs) tab(big, !!sym(p[[1]]), !!sym(p[[2]]), pct = "row", color = "diff", chi2 = TRUE)
  ts <- replicate(2, system.time(for (p in pairs)
    tab(big, !!sym(p[[1]]), !!sym(p[[2]]), pct = "row", color = "diff", chi2 = TRUE))[["elapsed"]])
  stats::median(ts)
}
cat(sprintf("sequential DT=8 : %.3f s\n", seq8))

run_mirai <- function(W, dispatcher) {
  t_setup <- system.time({
    mirai::daemons(W, dispatcher = dispatcher)
    mirai::everywhere({
      suppressWarnings(suppressMessages(devtools::load_all(pkg, quiet = TRUE)))
      data.table::setDTthreads(1L)
      assign("DT_shared", x, envir = .GlobalEnv)
    }, x = big, pkg = PKG)
  })[["elapsed"]]
  bt <- replicate(2, system.time(mirai::mirai_map(pairs, build_worker)[])[["elapsed"]])
  mirai::daemons(0)
  batch <- stats::median(bt)
  cat(sprintf("[mirai dispatcher=%-5s] W=%2d  setup %.2f s | batch %.3f s | batch-speedup %.2fx\n",
              as.character(dispatcher), W, t_setup, batch, seq8 / batch))
}

for (W in c(4L, 8L, 12L)) {
  run_mirai(W, dispatcher = TRUE)
  run_mirai(W, dispatcher = FALSE)
}
cat("DONE\n")
