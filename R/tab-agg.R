# PURPOSE: The tabxplor 1.4.0 aggregate-core -- sufficient-statistics aggregation and the
#          pure transforms that turn it into fmt fields. This is the single computation core
#          that both tab_plain() (factors) and tab_num() (numerics) route through, replacing
#          the historical duplicated inline data.table math.
# ROLE: Called from R/tab.R (tab_plain/tab_num). Kept in its own file so the core is legible.
# KEY CONSTRAINTS:
#   - data.table non-standard evaluation: keep aggregations GForce-friendly (bare per-column
#     sums), never re-scan N rows for a quantity the aggregate already carries.
#   - Byte-identity: the derived statistics must reproduce the pre-1.4.0 definitions EXACTLY
#     (waldo tolerance for the .rds golden, exact for the rounded tab_md snapshots).
# See: CLAUDE.md > 1.4.0 roadmap > Phase 2; dev/tabxplor_1.4.0_decisions.md (G1, §14).

# === SECTION: numeric sufficient statistics ==========================================

# num_derive_stats() -- derive per-col_var mean and (weighted) variance from the moment-sum
# columns an aggregate carries, and drop the moment-sum scratch columns.
#
# For each numeric col_var `v`, the aggregate carries (produced by the tab_num scans):
#   <v>_n   unweighted count of non-missing values          (= sum(!is.na(x)))
#   <v>_wn  weighted count of non-missing values (weighted)  (= sum(w * !is.na(x)))
#   <v>_s1  first moment sum   Sigma[w] x                     (= sum([w *] x,   na.rm))
#   <v>_s2  second moment sum  Sigma[w] x^2                   (= sum([w *] x^2, na.rm))
# It adds <v>_mean and <v>_var and removes <v>_s1 / <v>_s2, matching the pre-1.4.0
# definitions bit-for-bit (up to floating-point) so the single moment-sum pass replaces the
# old weighted.var() double-scan (which recomputed weighted.mean() per group) without
# changing output:
#   unweighted  mean = mean(x, na.rm)        = s1 / n
#               var  = stats::var(x, na.rm)  = (s2 - s1^2 / n) / (n - 1)
#   weighted    mean = round(weighted.mean, 10) = round(s1 / wn, 10)
#               var  = weighted.var (ML)        = round(s2 / wn - (s1 / wn)^2, 10)
#
# WARNING: the unweighted-vs-weighted variance asymmetry (sample n-1 vs ML /Sigma-w) is
# INTENTIONAL here -- it reproduces the historical stats::var vs weighted.var split. Unifying
# it is deferred to Phase 3 (weighted inference). See dev/tabxplor_1.4.0_decisions.md §14.
#
# NUMERICS: this is the one-pass "sum of squares" form (var = s2/.. - mean^2), so a single
# grouped scan yields both mean and variance (the whole point -- the old two-pass code
# recomputed the mean inside weighted.var). It is marginally less stable than the centred
# two-pass form for data with a very large mean relative to its spread; the moment sums are
# accumulated in double precision (the scans coerce integer col_vars via as.double(), which
# also avoids 32-bit integer overflow on Sigma x^2). Well within the golden waldo tolerance
# and display rounding for real survey data.
#
# DESIGN: degenerate groups must match the OLD functions exactly, and they differ by branch:
#   - unweighted: stats::var() is NA for n <= 1 (and empty); the Sigma-form gives NaN there,
#     so map NaN -> NA. mean(x, na.rm) of an all-NA group is NaN (converted to NA downstream
#     by the existing tabs_mean NaN->NA pass), so mean is left untouched here.
#   - weighted: weighted.var() returns 0 for a single observation (not NA) and NaN for an
#     all-NA group; the Sigma-form already reproduces both (0 and NaN), so var is NOT
#     NaN-scrubbed on the weighted branch.
num_derive_stats <- function(tabs, col_vars, weighted) {
  col_vars <- as.character(col_vars)
  for (v in col_vars) {
    n  <- tabs[[paste0(v, "_n")]]
    s1 <- tabs[[paste0(v, "_s1")]]
    s2 <- tabs[[paste0(v, "_s2")]]
    if (weighted) {
      wn   <- tabs[[paste0(v, "_wn")]]
      mean <- round(s1 / wn, 10)
      var  <- round(s2 / wn - (s1 / wn)^2, 10)
    } else {
      mean <- s1 / n
      var  <- (s2 - s1^2 / n) / (n - 1)
      var[is.nan(var)] <- NA_real_
    }
    data.table::set(tabs, j = paste0(v, "_mean"), value = mean)
    data.table::set(tabs, j = paste0(v, "_var"),  value = var)
    data.table::set(tabs, j = paste0(v, "_s1"),   value = NULL)
    data.table::set(tabs, j = paste0(v, "_s2"),   value = NULL)
  }
  tabs
}

# num_rollup() -- build a totals block (a set of total rows, or the total table) by SUMMING the
# moment-sum columns of the main numeric aggregate `agg`, grouped by `by`, then labeling the
# collapsed keys "Total". Because the moment sums (n, wn, s1, s2) are ADDITIVE, this reproduces
# exactly what re-scanning the microdata grouped by `by` would give -- WITHOUT re-scanning N rows.
# This is the Phase 2 rollup that replaces tab_num()'s two extra total-row / total-table N-scans.
#
#   agg          the main moment-sum aggregate (keyed by tab_row_names, carrying moment cols)
#   by           the surviving key columns (character(0) for the grand total)
#   drop_keys    the tab_row_names collapsed to the "Total" label (tab_vars not in `by`, + row_var)
#   moment_cols  the additive columns to sum (all non-key columns of `agg`)
#   tab_vars_chr the tab_var column names, re-factored after the "Total" relabel (mirrors the old
#                re-scan's `[, tab_vars := as.factor(.)]`; row_var is left for the caller's
#                not-factor pass, exactly as before)
num_rollup <- function(agg, by, drop_keys, moment_cols, tab_vars_chr) {
  roll <- if (length(by) == 0) {
    agg[, lapply(.SD, sum, na.rm = TRUE), .SDcols = moment_cols]
  } else {
    agg[, lapply(.SD, sum, na.rm = TRUE), .SDcols = moment_cols, keyby = by]
  }
  if (length(drop_keys) > 0)    roll[, (drop_keys) := "Total"]
  if (length(tab_vars_chr) > 0) roll[, (tab_vars_chr) := lapply(.SD, as.factor), .SDcols = tab_vars_chr]
  roll
}


# === SECTION: confidence intervals & per-cell significance ============================
#
# The unified CI engine (Phase 3a). PURE, vectorised, dependency-free math -- no dplyr /
# data.table / DescTools inside. tab_ci() (proportions) and tab_num() (means) both route
# through these: they resolve the reference + read base stats once, then call one primitive.
# Every interval is one of TWO shapes:
#   - PIVOT   : estimate +/- q*se, symmetric, with a continuous p = 2*P(T > |est/se|).
#               Serves Agresti-Caffo & Wald (proportion diff) and z / Welch-t (means).
#   - SCORE   : asymmetric, from the score formula -- Wilson (single proportion) and its
#               hybrid Newcombe-10 (proportion difference).
#
# SIGNIFICANCE = universal CI-inclusion: the stored per-cell `pvalue` is the CI-inversion p of
# the SAME method that draws the bracket, so stars (cut(pvalue)) can never contradict the
# interval, for any method. Pivot methods invert in closed form; Newcombe inverts by a
# vectorised bisection on z (its bounds are monotone in z). `pvalue = NA` for cell intervals
# (H0: p=0 / mu=0 is not meaningful) and when `want_p = FALSE` (stars opted out -> one eval).
# Validated against DescTools/prop.test/t.test in dev/verify_ci_inclusion.R.
# See: CLAUDE.md > 1.4.0 roadmap > Phase 3a; dev/tabxplor_1.4.0_decisions.md §20.

# Wilson score bounds at a given z (internal core, reused by the Newcombe inversion).
wilson_bounds <- function(p, n, z) {
  d    <- 1 + z^2 / n
  ctr  <- (p + z^2 / (2 * n)) / d
  half <- (z / d) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  list(inf = ctr - half, sup = ctr + half)
}

# PIVOT shape: symmetric interval `estimate +/- q*se` + continuous inversion p-value.
# df = Inf gives the normal quantile (qt/pt handle Inf as the normal limit), so this covers
# z-based (df = Inf) and t-based (Welch df) means, and AC/Wald proportion diffs (the caller
# builds the adjusted `estimate`/`se`). `want_p = FALSE` skips significance (returns NA).
ci_pivot <- function(estimate, se, df = Inf, conf_level = 0.95, want_p = TRUE) {
  q      <- stats::qt(1 - (1 - conf_level) / 2, df)
  half   <- q * se
  pvalue <- if (want_p) 2 * stats::pt(-abs(estimate / se), df) else NA_real_
  pvalue <- vctrs::vec_recycle(pvalue, length(estimate))
  # df = Inf is the valid normal pivot; only NA/NaN df (and degenerate se) kill the p-value.
  bad    <- !is.finite(se) | se == 0 | is.na(df)
  pvalue[bad] <- NA_real_
  list(inf = estimate - half, sup = estimate + half, pvalue = pvalue)
}

# SCORE shape, single proportion: Wilson interval. Cell CI -> no meaningful H0 -> pvalue NA.
ci_wilson <- function(p, n, conf_level = 0.95) {
  b <- wilson_bounds(p, n, zscore_formula(conf_level))
  list(inf = b$inf, sup = b$sup, pvalue = vctrs::vec_recycle(NA_real_, length(p)))
}

# SCORE shape, proportion difference: Newcombe method 10 (hybrid score, built from the two
# groups' Wilson intervals). Its exact dual test has no closed form, so the inversion p is
# found by a vectorised bisection on z (monotone). want_p = FALSE skips it (one interval eval).
ci_newcombe <- function(p1, n1, p2, n2, conf_level = 0.95, want_p = TRUE) {
  d  <- p1 - p2
  w1 <- wilson_bounds(p1, n1, zscore_formula(conf_level))
  w2 <- wilson_bounds(p2, n2, zscore_formula(conf_level))
  inf <- d - sqrt((p1 - w1$inf)^2 + (w2$sup - p2)^2)
  sup <- d + sqrt((w1$sup - p1)^2 + (p2 - w2$inf)^2)
  pvalue <- if (want_p) newcombe_pvalue(p1, n1, p2, n2) else vctrs::vec_recycle(NA_real_, length(d))
  list(inf = inf, sup = sup, pvalue = pvalue)
}

# Continuous CI-inversion p for Newcombe: the smallest alpha at which the interval excludes 0.
# g(z) = |d| - near_margin(z) is decreasing in z (the interval widens with z); bisect for the
# root z*, then p = 2*(1 - Phi(z*)). By construction cut(p) matches the Newcombe bracket's own
# 0-inclusion at any level. Fully vectorised (fixed iterations, no per-cell root solver).
newcombe_pvalue <- function(p1, n1, p2, n2, steps = 50L) {
  d  <- p1 - p2
  ad <- abs(d)
  margin <- function(z) {
    w1 <- wilson_bounds(p1, n1, z)
    w2 <- wilson_bounds(p2, n2, z)
    ifelse(d >= 0,
           sqrt((p1 - w1$inf)^2 + (w2$sup - p2)^2),   # controls the lower limit (d >= 0)
           sqrt((w1$sup - p1)^2 + (p2 - w2$inf)^2))   # controls the upper limit (d <  0)
  }
  lo <- vctrs::vec_recycle(0,  length(d))
  hi <- vctrs::vec_recycle(40, length(d))
  for (i in seq_len(steps)) {
    mid <- (lo + hi) / 2
    over <- (ad - margin(mid)) <= 0   # margin too wide -> z too high
    hi   <- ifelse(over, mid, hi)
    lo   <- ifelse(over, lo,  mid)
  }
  p <- 2 * stats::pnorm(-(lo + hi) / 2)
  p[!is.finite(d)] <- NA_real_
  p
}

# Proportion-difference CI + inclusion significance, dispatched on method. Newcombe (default)
# is the score hybrid; AC and Wald are pivot-shaped (the caller-free adjusted est/se are built
# here). Weighted rule (§14): the caller passes the WEIGHTED proportions p1/p2 and the
# UNWEIGHTED bases n1/n2 (their cells' tot_n).
ci_prop_diff <- function(p1, n1, p2, n2, conf_level = 0.95, method = "newcombe", want_p = TRUE) {
  switch(
    method,
    "newcombe" = ci_newcombe(p1, n1, p2, n2, conf_level, want_p),
    "ac" = {
      a1 <- (p1 * n1 + 1) / (n1 + 2); a2 <- (p2 * n2 + 1) / (n2 + 2)
      ci_pivot(a1 - a2, sqrt(a1 * (1 - a1) / (n1 + 2) + a2 * (1 - a2) / (n2 + 2)),
               df = Inf, conf_level = conf_level, want_p = want_p)
    },
    "wald" = ci_pivot(p1 - p2, sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2),
                      df = Inf, conf_level = conf_level, want_p = want_p),
    stop("unknown method_diff: ", method)
  )
}

# Mean-difference CI + inclusion significance (Welch-t pivot when stars are on, z otherwise).
# Weighted rule (§14): weighted means/variances, unweighted n1/n2.
ci_mean_diff2 <- function(m1, v1, n1, m2, v2, n2, conf_level = 0.95, want_p = TRUE) {
  se <- sqrt(v1 / n1 + v2 / n2)
  df <- if (want_p) se^4 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1)) else Inf
  ci_pivot(m1 - m2, se, df = df, conf_level = conf_level, want_p = want_p)
}
