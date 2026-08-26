#!/usr/bin/env Rscript
# PURPOSE: Manual arbiter for the shared finest-grain aggregate fusion (tab_many `.by_table`).
#          Times fused (default) vs table-by-table (`.by_table = TRUE`) on a big 15M-row fixture that
#          fakes the pc18 example (5 socio-demo row_vars x 3 practice col_vars, weighted, row %) and
#          asserts byte-identical output. The fixture uses ALL level combinations (full Cartesian),
#          so G_obs = prod(nlevels) -- DENSER, i.e. more pessimistic, than real sparse survey data.
# ROLE: Standalone; dev/benchmarks/ is .Rbuildignore'd -> never run by the test suite or R CMD check.
# USAGE (from package root): source("dev/benchmarks/run_fused_vs_bytable.R", encoding = "UTF-8")

pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
source(file.path(pkg, "dev", "tests", "testthat", "helper-benchmark.R"))
suppressMessages(library(data.table))

# --- fixture: full Cartesian of pc18-like factor levels (NA included on DIPLOM/CRITREVENU) ---
gen_pc18_full <- function(n = 15e6L,
                          cache = file.path(pkg, "dev", "benchmarks", "big_pc18_full_15M.rds"),
                          seed = 20260701L) {
  if (!is.null(cache) && file.exists(cache)) {
    message("gen_pc18_full: loading cached fixture (", cache, ")")
    return(readRDS(cache))
  }
  message("gen_pc18_full: building ", format(n, big.mark = ","), "-row full-Cartesian fixture ...")
  lv <- list(
    DIPLOM = c(1:7, NA), CRITREVENU = c(1:5, NA), CRITAGE = 1:5, CSTOTR = 1:6, TELE = 1:3,  # rows
    CONCERTS = 1:4, THEATRE4 = 1:4, JV = 1:3                                                 # cols
  )
  grid <- do.call(data.table::CJ, lv)                       # every combo incl. NA -> 207,360 rows
  big  <- grid[rep(seq_len(nrow(grid)), length.out = n)]    # replicate to n rows
  for (nm in names(lv)) data.table::set(big, j = nm, value = factor(big[[nm]]))
  set.seed(seed)
  data.table::set(big, j = "POND", value = stats::runif(nrow(big), 0.2, 3))
  big <- as.data.frame(big)
  if (!is.null(cache)) {
    dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(big, cache, compress = FALSE)
  }
  big
}

rv <- c("DIPLOM", "CRITREVENU", "CRITAGE", "CSTOTR", "TELE")
cv <- c("CONCERTS", "THEATRE4", "JV")
big <- gen_pc18_full()
g_obs   <- nrow(data.table::as.data.table(big)[, .N, by = c(rv, cv)])
prodlev <- prod(vapply(c(rv, cv), function(k) nlevels(big[[k]]) + as.integer(anyNA(big[[k]])), numeric(1)))
cat(sprintf("\nrows: %s ; prod(nlevels)=%s ; observed cells G_obs=%s\n",
            format(nrow(big), big.mark = ","), format(prodlev, big.mark = ","),
            format(g_obs, big.mark = ",")))

# Fusion is opt-in (default floor Inf); enable it here so the "fused" call actually fuses at 15M.
options(tabxplor.fuse_min_rows = 1e7)
call_fused   <- function() tab_many(big, tidyselect::all_of(rv), tidyselect::all_of(cv),
                                     wt = POND, pct = "row", na = "drop", chi2 = TRUE)
call_bytable <- function() tab_many(big, tidyselect::all_of(rv), tidyselect::all_of(cv),
                                     wt = POND, pct = "row", na = "drop", chi2 = TRUE, .by_table = TRUE)

cat("checking byte-identical output (fused vs table-by-table) ...\n")
identical_out <- isTRUE(all.equal(call_fused(), call_bytable()))
cat(sprintf("  all.equal(fused, by_table): %s\n", identical_out))

ops <- list(fused = call_fused, by_table = call_bytable)
res <- benchmark_run("pc18_full_15M", nrow(big), ops, iterations = 3L)
benchmark_print(res, NULL, header = "15M pc18-like (full Cartesian) fused vs table-by-table")
tf <- res$median_s[res$operation == "fused"]; tb <- res$median_s[res$operation == "by_table"]
cat(sprintf("speedup (by_table / fused): %.2fx\n", tb / tf))
