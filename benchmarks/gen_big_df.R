# PURPOSE: Deterministically generate (and cache) a large data frame for tabxplor benchmarks.
# ROLE: Big-data fixture for benchmarks/run_bench.R. NEVER shipped (benchmarks/ is .Rbuildignore'd)
#        and NEVER touched by the test suite or R CMD check.
# KEY CONSTRAINTS:
#   - Deterministic (fixed seed) so timings are comparable run to run.
#   - Built ONCE, then cached to benchmarks/big_df.rds (git-ignored) and reloaded on every later
#     run -- the build (rgamma/runif/sample over 8M rows) is the slow part and must not repeat.
#   - Cache is stored UNCOMPRESSED (compress = FALSE): larger on disk (~a few hundred MB) but much
#     faster to write and, crucially, to reload. Delete the .rds to force a rebuild.

# Realistic mix: several factor col cardinalities + a weight + numeric cols + injected NAs.
gen_big_df <- function(n = 8e6L,
                       cache = file.path("benchmarks", "big_df.rds"),
                       seed = 20260701L) {
  if (!is.null(cache) && file.exists(cache)) {
    message("gen_big_df: loading cached fixture (", cache, ")")
    return(readRDS(cache))
  }

  message("gen_big_df: building ", format(n, big.mark = ","),
          "-row fixture (one-time, slow) ...")
  set.seed(seed)
  idx <- function(k) sample.int(k, n, replace = TRUE)

  df <- tibble::tibble(
    region    = factor(idx(5), labels = paste0("R", 1:5)),
    age_grp   = factor(idx(6), labels = paste0("A", 1:6)),
    education = factor(idx(8), labels = paste0("E", 1:8)),
    sex       = factor(idx(2), labels = c("F", "M")),
    response  = factor(idx(3), labels = c("no", "maybe", "yes")),
    income    = stats::rgamma(n, shape = 2, rate = 0.001),
    score     = stats::runif(n, 0, 100),
    weight    = stats::runif(n, 0.2, 3)
  )

  # Inject missing values with different densities on two columns.
  df$response[sample.int(n, n %/% 20L)] <- NA   # ~5%
  df$score[sample.int(n, n %/% 10L)]    <- NA   # ~10%

  if (!is.null(cache)) {
    dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(df, cache, compress = FALSE)          # fast reuse; delete the .rds to rebuild
    message("gen_big_df: cached to ", cache, " (reused on future runs; delete it to rebuild)")
  }
  df
}
