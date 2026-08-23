# PURPOSE: The evidence behind the `tabxplor.parallel` default -- what a mirai pool costs to start,
#          what each parallel build costs on top of the work, how the gain scales with the number of
#          TABLES and the number of workers, how much memory a worker holds, and whether jamovi's
#          always-serial rule is still the right one.
# ROLE: The fourth piece of the Phase 22h harness. Answers a different question from
#       phase22h_perf_review.R (which measures defaults) and needs its own design, for two reasons:
#       a daemon pool is process state, and the answer depends on a grid (workers x tables) rather
#       than on one call.
# KEY CONSTRAINTS:
#   - ⚠ ONE worker count per PROCESS. tab_pool_ensure() respawns whenever the count differs, so
#     sweeping W inside one session charges every W after the first with a teardown + spawn.
#   - ⚠ Children run the INSTALLED build (--lib=), never load_all(): under load_all() the pool
#     load_all()s each fresh daemon, which is dev-only cost no user ever pays and would triple the
#     measured spawn time.
#   - Every child stops its pool in on.exit(). An orphaned daemon survives its parent and silently
#     starves every later run.
#   - The fixture has 24 factor columns on purpose: the question is how the gain scales with the
#     number of TABLES, and gss_cat alone cannot make more than ~8.
# USAGE (from the package root):
#   Rscript dev/benchmarks/phase22h_parallel.R                          # 12 cores
#   taskset -c 0-3 Rscript dev/benchmarks/phase22h_parallel.R --tag=4core
#   taskset -c 0,1 Rscript dev/benchmarks/phase22h_parallel.R --tag=2core

args   <- commandArgs(trailingOnly = TRUE)
arg_of <- function(n, d = NULL) { h <- grep(paste0("^--", n, "="), args, value = TRUE)
                                  if (length(h)) sub(paste0("^--", n, "="), "", h[[1L]]) else d }
LIB   <- arg_of("lib", path.expand("~/R/tabxplorhead"))
TAG   <- arg_of("tag", "desktop")
OUT   <- file.path("dev/benchmarks/results_2.0.0", sprintf("phase22h_parallel_%s.csv", TAG))
NGRID <- as.integer(strsplit(arg_of("n", "2,3,4,6,8,12,24"), ",")[[1L]])
WGRID <- as.integer(strsplit(arg_of("w", "0,2,3,4,6,8"), ",")[[1L]])

# --- the fixture: a survey-shaped frame with enough variables to sweep the table count ----------
make_fixture <- function(times = 1L, seed = 22080L) {
  g <- forcats::gss_cat
  if (times > 1L) g <- g[rep(seq_len(nrow(g)), times), , drop = FALSE]
  set.seed(seed); n <- nrow(g)
  extra <- lapply(seq_len(20L), function(i)
    factor(sample.int(3L + (i %% 6L), n, replace = TRUE), labels = paste0("l", seq_len(3L + (i %% 6L)))))
  names(extra) <- paste0("v", sprintf("%02d", seq_len(20L)))
  tibble::as_tibble(cbind(g[, c("marital", "race", "rincome", "relig", "partyid", "year")], extra))
}
ROW_POOL <- c("marital", "rincome", "relig", "partyid", "year", paste0("v", sprintf("%02d", 1:20)))
COL_VARS <- c("race", "marital")

# ================= CHILD: one worker count, one process ========================================
if (!is.null(arg_of("child"))) {
  mode <- arg_of("child")
  # ⚠ R_LIBS, not just .libPaths(): a mirai daemon is a FRESH R process and inherits the environment,
  # not this process' library path. Without it the daemons load whatever tabxplor is in the default
  # library -- a different build from the one under test, which is exactly the version mismatch
  # tab-parallel.R's header warns about, and it surfaces as "could not find function" in a worker.
  Sys.setenv(R_LIBS = paste(c(LIB, .libPaths()), collapse = .Platform$path.sep))
  .libPaths(c(LIB, .libPaths()))
  suppressMessages(library(tabxplor))
  data.table::setDTthreads(1L)
  q <- function(e) suppressWarnings(suppressMessages(e))
  on.exit(try(tabxplor::tab_parallel_stop(), silent = TRUE), add = TRUE)

  # -- 1. cold pool spawn: what the FIRST parallel table pays if nothing pre-warmed it -----------
  if (mode == "spawn") {
    W <- as.integer(arg_of("workers", "4"))
    t <- system.time(tabxplor:::tab_pool_ensure(W))[["elapsed"]]
    cat(sprintf("spawn\t%d\t%.4f\n", W, t))
    quit(save = "no")
  }

  # -- 2. the grid: seconds for N tables at W workers, pool warm ---------------------------------
  if (mode == "grid") {
    W <- as.integer(arg_of("workers", "0"))
    d <- make_fixture(as.integer(arg_of("times", "1")))
    options(tabxplor.parallel = if (W <= 1L) FALSE else W)
    if (W > 1L) tabxplor:::tab_pool_ensure(W)                 # warm: spawn is measured separately
    build <- function(N) function()
      q(tab(d, ROW_POOL[seq_len(N)], COL_VARS, pct = "row", test = TRUE, color = TRUE))
    for (N in NGRID) {
      f <- build(N); f()                                      # warm this shape (ship + compile)
      ts <- replicate(3L, system.time(f())[["elapsed"]])
      cat(sprintf("grid\t%d\t%d\t%.4f\n", W, N, min(ts))); flush.console()
    }
    quit(save = "no")
  }

  # -- 3. the per-call shipping cost: same 4 tables, data of growing size ------------------------
  if (mode == "ship") {
    W <- as.integer(arg_of("workers", "4"))
    for (times in c(1L, 10L, 50L)) {
      d <- make_fixture(times)
      mb <- round(as.numeric(utils::object.size(d)) / 1024^2, 1)
      for (w in c(0L, W)) {
        options(tabxplor.parallel = if (w <= 1L) FALSE else w)
        if (w > 1L) tabxplor:::tab_pool_ensure(w)
        f <- function() q(tab(d, ROW_POOL[1:4], COL_VARS, pct = "row", test = TRUE))
        f(); ts <- replicate(3L, system.time(f())[["elapsed"]])
        cat(sprintf("ship\t%d\t%d\t%.1f\t%.4f\n", w, nrow(d), mb, min(ts))); flush.console()
      }
    }
    quit(save = "no")
  }

  # -- 4. what a pool COSTS in memory: total RSS of the daemons ---------------------------------
  if (mode == "mem") {
    W <- as.integer(arg_of("workers", "4"))
    d <- make_fixture(as.integer(arg_of("times", "10")))
    # ⚠ every R process, not `--ppid`: a mirai daemon is reparented and is NOT a child of this one.
    rss <- function() {
      p <- suppressWarnings(system2("ps", c("-eo", "rss=,args="), stdout = TRUE, stderr = FALSE))
      p <- p[grepl("/exec/R|bin/R ", p)]
      sum(as.numeric(sub("^ *([0-9]+).*", "\\1", p)), na.rm = TRUE) / 1024
    }
    options(tabxplor.parallel = W); tabxplor:::tab_pool_ensure(W)
    Sys.sleep(1)                                            # let the daemons finish coming up
    before <- rss()
    q(tab(d, ROW_POOL[1:8], COL_VARS, pct = "row", test = TRUE))
    cat(sprintf("mem\t%d\t%.1f\t%.1f\t%.1f\n", W,
                as.numeric(utils::object.size(d)) / 1024^2, before, rss()))
    quit(save = "no")
  }

  # -- 6. which jmvtab SHAPE parallelises: row_vars make units, tab_vars do not -------------------
  if (mode == "shapes") {
    W <- as.integer(arg_of("workers", "0"))
    g <- forcats::gss_cat
    options(tabxplor.parallel = if (W <= 1L) FALSE else W)
    if (W > 1L) tabxplor:::tab_pool_ensure(W)
    med <- function(f, k = 4L) { f(); min(replicate(k, system.time(f())[["elapsed"]])) }
    cases <- list(
      `1 row_var, 3 col_vars` = function()
        q(tab(g, "partyid", c("race", "marital", "relig"), pct = "row", color = "diff")),
      `1 row_var, 3 col_vars, tab_vars` = function()
        q(tab(g, "partyid", c("marital", "relig", "year"), tab_vars = "race", pct = "row", color = "diff")),
      `3 row_vars, 3 col_vars` = function()
        q(tab(g, c("partyid", "rincome", "year"), c("race", "marital", "relig"), pct = "row", color = "diff")),
      `3 row_vars, tab_vars` = function()
        q(tab(g, c("partyid", "rincome", "year"), c("marital", "relig"), tab_vars = "race",
              pct = "row", color = "diff")))
    for (nm in names(cases)) cat(sprintf("shape\t%d\t%s\t%.4f\n", W, nm, med(cases[[nm]])))
    quit(save = "no")
  }

  # -- 7. the three tab_reg axes: outcomes, tab_vars groups, model specs -------------------------
  if (mode == "regaxes") {
    W  <- as.integer(arg_of("workers", "0"))
    g  <- tabxplor::gss_cat_data_formatting()
    g10 <- g[rep(seq_len(nrow(g)), 10L), , drop = FALSE]
    options(tabxplor.parallel = if (W <= 1L) FALSE else W)
    if (W > 1L) tabxplor:::tab_pool_ensure(W)
    med <- function(f, k = 3L) { f(); min(replicate(k, system.time(f())[["elapsed"]])) }
    p4 <- c("rincome", "race", "age", "relig")
    cases <- list(
      `1 outcome [21k]` = function()
        q(tab_reg(g, outcome = "married", predictors = p4, family = "binomial", empirical = TRUE)),
      `3 outcomes [21k]` = function()
        q(tab_reg(g, outcome = c("married", "black", "income25k"), predictors = p4,
                  family = "binomial", empirical = TRUE)),
      `3 outcomes [215k]` = function()
        q(tab_reg(g10, outcome = c("married", "black", "income25k"), predictors = p4,
                  family = "binomial", empirical = TRUE)),
      `tab_vars: 3 groups [21k]` = function()
        q(tab_reg(g, outcome = "married", tab_vars = "race",
                  predictors = c("rincome", "age", "relig"), family = "binomial")),
      `tab_vars: 3 groups [215k]` = function()
        q(tab_reg(g10, outcome = "married", tab_vars = "race",
                  predictors = c("rincome", "age", "relig"), family = "binomial")),
      `4 nested models [21k]` = function()
        q(tab_reg(g, outcome = "married", family = "binomial", empirical = TRUE,
                  predictors = list(a = "race", b = c("race", "rincome"),
                                    c = c("race", "rincome", "age"), d = p4))))
    for (nm in names(cases)) cat(sprintf("regax\t%d\t%s\t%.4f\n", W, nm, med(cases[[nm]])))
    quit(save = "no")
  }

  # -- 5. jamovi: how much of a warm interaction is even in the parallelisable map ---------------
  # Rprof over repeated warm jmvtab_build() calls, attributing to the three stages that matter:
  # the tier-1 aggregate (cached on main), the row-axis map (the only parallelisable part) and the
  # rest (resolve + assemble + output shape). The render is timed separately -- it is not in build.
  if (mode == "jmv") {
    if (!file.exists("tests/testthat/helper-benchmark.R")) quit(save = "no")
    source("tests/testthat/helper-benchmark.R")
    d    <- forcats::gss_cat
    opts <- benchmark_jmvtab_opts(row_vars = c("partyid", "rincome", "year"),
                                  col_vars = c("race", "marital", "relig"),
                                  pct = "row", color = "diff")
    store <- q(tabxplor:::jmvtab_build(d, opts, NULL))$store         # warm the cache
    o2 <- utils::modifyList(opts, list(pct = "col"))
    f  <- function() q(tabxplor:::jmvtab_build(d, o2, store))
    f(); ts <- replicate(5L, system.time(f())[["elapsed"]])
    cat(sprintf("jmv\tbuild_warm\t%.4f\t\n", stats::median(ts)))
    pf <- tempfile(fileext = ".Rprof")
    Rprof(pf, interval = 0.005)
    for (i in 1:40) f()
    Rprof(NULL)
    sm <- summaryRprof(pf)$by.total
    # ⚠ summaryRprof() rownames carry the profile file's own double quotes -- strip them, or every
    # lookup silently returns 0 and every stage reads "0 % of the build".
    rn  <- gsub('"', "", rownames(sm), fixed = TRUE)
    tot <- max(sm$total.time)
    grab <- function(nm) { r <- sm[rn == nm, ]
                           if (nrow(r)) round(100 * r$total.time[1] / tot, 1) else 0 }
    for (nm in c("tab_build_tables", "tab_assemble_output", "tab_aggregate",
                 "jmvtab_cache_aggregate", "tab_setup", "tab_prepare_pop", "tab_transform"))
      cat(sprintf("jmv\tshare_%s\t%.1f\t\n", nm, grab(nm)))
    quit(save = "no")
  }

  quit(save = "no")
}

# ================= PARENT: orchestrate the children, print, write ==============================
run <- function(...) {
  o <- system2("Rscript", c("dev/benchmarks/phase22h_parallel.R", paste0("--lib=", LIB), ...),
               stdout = TRUE, stderr = FALSE)
  o[grepl("\t", o)]
}
rows <- c()

cat("== cold pool spawn (installed build, nothing pre-warmed) ==\n")
for (W in setdiff(WGRID, 0L)) rows <- c(rows, run("--child=spawn", paste0("--workers=", W)))

cat("== the grid: N tables x W workers ==\n")
for (W in WGRID) { cat("  workers=", W, "\n", sep = ""); flush.console()
                   rows <- c(rows, run("--child=grid", paste0("--workers=", W),
                                       paste0("--n=", paste(NGRID, collapse = ",")))) }

cat("== per-call shipping cost by data size ==\n")
rows <- c(rows, run("--child=ship", "--workers=4"))

cat("== pool memory ==\n")
for (W in c(2L, 4L, 8L)) rows <- c(rows, run("--child=mem", paste0("--workers=", W)))

cat("== jamovi: where a warm interaction spends its time ==\n")
rows <- c(rows, run("--child=jmv"))

cat("== which jmvtab shape parallelises ==\n")
for (W in c(0L, 3L)) rows <- c(rows, run("--child=shapes", paste0("--workers=", W)))

cat("== the three tab_reg axes ==\n")
for (W in c(0L, 2L, 3L)) rows <- c(rows, run("--child=regaxes", paste0("--workers=", W)))

p  <- strsplit(rows, "\t", fixed = TRUE)
df <- data.frame(kind = vapply(p, `[`, "", 1L),
                 a = vapply(p, function(x) x[2], ""), b = vapply(p, function(x) x[3], ""),
                 c = vapply(p, function(x) x[4], ""), d = vapply(p, function(x) x[5], ""),
                 tag = TAG, stringsAsFactors = FALSE)
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(df, OUT, row.names = FALSE)

sp <- df[df$kind == "spawn", ]
cat("\n--- cold pool spawn ---\n")
print(data.frame(workers = sp$a, seconds = sp$b), row.names = FALSE)

gr <- df[df$kind == "grid", ]
gr$W <- as.integer(gr$a); gr$N <- as.integer(gr$b); gr$s <- as.numeric(gr$c)
m <- reshape(gr[, c("N", "W", "s")], idvar = "N", timevar = "W", direction = "wide")
names(m) <- sub("^s\\.", "W", names(m)); names(m)[names(m) == "W0"] <- "serial"
cat("\n--- seconds: N tables (rows) x workers (cols), pool warm ---\n")
print(m, row.names = FALSE)
cat("\n--- speedup vs serial ---\n")
sp2 <- m; for (j in setdiff(names(m), c("N", "serial"))) sp2[[j]] <- round(m$serial / m[[j]], 2)
print(sp2[, setdiff(names(sp2), "serial")], row.names = FALSE)

sh <- df[df$kind == "ship", ]
cat("\n--- per-call cost, 4 tables, by data size ---\n")
print(data.frame(workers = sh$a, rows = sh$b, data_mb = sh$c, seconds = sh$d), row.names = FALSE)

me <- df[df$kind == "mem", ]
cat("\n--- pool memory (MB of daemon RSS) ---\n")
print(data.frame(workers = me$a, data_mb = me$b, before = me$c, after = me$d), row.names = FALSE)
jm <- df[df$kind == "jmv", ]
if (nrow(jm)) {
  cat("\n--- jamovi warm interaction: total seconds, then % of build spent per stage ---\n")
  print(data.frame(what = jm$a, value = jm$b), row.names = FALSE)
}
cat("\nwritten: ", OUT, "\n", sep = "")
