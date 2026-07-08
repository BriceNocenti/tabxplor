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
