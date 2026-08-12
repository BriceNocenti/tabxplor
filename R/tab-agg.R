# PURPOSE: The tabxplor 2.0.0 aggregate-core -- sufficient-statistics aggregation and the
#          pure transforms that turn it into fmt fields. This is the single computation core
#          that both tab_plain() (factors) and tab_num() (numerics) route through, replacing
#          the historical duplicated inline data.table math.
# ROLE: Called from R/tab.R (tab_plain/tab_num). Kept in its own file so the core is legible.
#       Phase 7d added the numeric aggregate seam here: num_moment_scan() (the shared O(N) scan)
#       and tab_aggregate_num() (the tier-1 producer tab_num(.fine=) adopts), the numeric analogue
#       of tab_plain()'s `.fine` factor path.
# KEY CONSTRAINTS:
#   - data.table non-standard evaluation: keep aggregations GForce-friendly (bare per-column
#     sums), never re-scan N rows for a quantity the aggregate already carries.
#   - Byte-identity: the derived statistics must reproduce the pre-2.0.0 definitions EXACTLY
#     (waldo tolerance for the .rds golden, exact for the rounded tab_md snapshots).
# See: CLAUDE.md > 2.0.0 roadmap > Phase 2; dev/tabxplor_2.0.0_decisions.md (G1, §14).

# === SECTION: numeric sufficient statistics ==========================================

# num_derive_stats() -- derive per-col_var mean and (weighted) variance from the moment-sum
# columns an aggregate carries, and drop the moment-sum scratch columns.
#
# For each numeric col_var `v`, the aggregate carries (produced by the tab_num scans):
#   <v>_n   unweighted count of non-missing values          (= sum(!is.na(x)))
#   <v>_wn  weighted count of non-missing values (weighted)  (= sum(w * !is.na(x)))
#   <v>_s1  first moment sum   Sigma[w] x                     (= sum([w *] x,   na.rm))
#   <v>_s2  second moment sum  Sigma[w] x^2                   (= sum([w *] x^2, na.rm))
# It adds <v>_mean and <v>_var and removes <v>_s1 / <v>_s2, matching the pre-2.0.0
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
# it is deferred to Phase 3 (weighted inference). See dev/tabxplor_2.0.0_decisions.md §14.
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
  # Last Phase z10: one SHARED ptype per tab_var, taken from the source aggregate. vctrs refuses to
  # combine two ORDERED factors with different level sets, so the map_dfr over the grouping sets
  # (R/tab.R, num_core) died on an ordered tab_var: the sets that keep it carry levels(src), the sets
  # that collapse it to "Total" carried only "Total". Byte-identical unordered -- the first grouping
  # set keeps the full source levels, so vctrs' appearance-order union already yielded
  # c(levels(src), "Total"); a NON-factor source keeps the old as.factor() exactly.
  if (length(tab_vars_chr) > 0) for (v in tab_vars_chr) {
    src <- agg[[v]]
    data.table::set(roll, j = v, value = if (is.factor(src)) {
      factor(as.character(roll[[v]]), levels = unique(c(levels(src), "Total")),
             ordered = is.ordered(src))
    } else as.factor(roll[[v]]))
  }
  roll
}

# num_moment_scan() -- the single O(N) moment-sum scan (the numeric aggregate MATH). Kept in ONE
# place (2.0.0 keystone: no duplicated aggregate math) and shared verbatim by tab_num()'s
# table-by-table path and by tab_aggregate_num() (the Phase 7d producer). Given a prepped
# data.table `data` (columns = `tab_row_names` keys + numeric `col_vars` [+ `wt`]; integer/factor
# col_vars already coerced to numeric) it returns, per numeric col_var `v`, the sufficient moment
# sums keyed by `tab_row_names`:
#   v_n  = sum(!is.na(x))                          v_s1 = sum([w *] x)      v_s2 = sum([w *] x^2)
#   v_wn = sum([w *] !is.na(x))  (weighted only)   v_w2 = sum(w^2 * !is.na(x))  (weighted only)
#   v_w2s1 = sum(w^2 x)  v_w2s2 = sum(w^2 x^2)      (weighted only, Last Phase z16-ii)
# `wt` is the weight SYMBOL (character(0) when unweighted); `eval(wt)` looks the column up inside j.
# WARNING: byte-identity-critical -- the as.double() coercions (32-bit overflow guard on Sigma x^2),
# the weight lookup, the (no) .SDcols on the weighted branch, and the column construction order (all
# _n, then _wn, _s1, _s2, _w2) must match num_derive_stats()'s expectations EXACTLY.
# WARNING (Last Phase a bug-fix): the weight is referenced by the plain string `wt_name` (captured
# OUTSIDE the data.table `[...]` call, where the `wt` argument is un-shadowed) and read with
# get(wt_name) -- never the bare symbol `wt` inside `j`. data.table exposes every column as a `j`
# variable, so a column literally named "wt" (the weight OR a col_var) used to SHADOW the `wt`
# argument: as.character(wt) then returned the column's VALUES, corrupting the scratch column names
# (a leaked garbage column + "does not exist to remove" warnings). Byte-identical for every ordinary
# weight name; get(wt_name) is functionally eval(wt).
num_moment_scan <- function(data, tab_row_names, col_vars, wt) {
  col_vars <- as.character(col_vars)
  wt_name  <- as.character(wt)     # captured here (un-shadowed) -- see the WARNING above
  if (length(wt) == 0) {
    data[,
         c(purrr::set_names(purrr::map(.SD,  ~ sum(!is.na(.))),
                            paste0(col_vars, "_n")),

           purrr::set_names(purrr::map(.SD,  ~ sum(as.double(.), na.rm = TRUE)),
                            paste0(col_vars, "_s1")),

           purrr::set_names(purrr::map(.SD, ~ sum(as.double(.) * as.double(.), na.rm = TRUE)),
                            paste0(col_vars, "_s2"))
         ),
         .SDcols = col_vars,
         keyby = c(tab_row_names)]

  } else {
    data[,
         c(purrr::set_names(purrr::map_if(.SD,
                                          names(.SD) != wt_name,
                                          ~ sum(!is.na(.)),
                                          .else = ~ NA_real_),
                            paste0(c(col_vars, wt_name), "_n")),

           purrr::set_names(purrr::map_if(.SD,
                                          names(.SD) != wt_name,
                                          ~ sum(as.integer(!is.na(.)) * get(wt_name), na.rm = TRUE),
                                          .else = ~ NA_real_),
                            paste0(c(col_vars, wt_name), "_wn")),

           purrr::set_names(purrr::map_if(.SD,
                                          names(.SD) != wt_name,
                                          ~ sum(get(wt_name) * ., na.rm = TRUE),
                                          .else = ~ NA_real_),
                            paste0(c(col_vars, wt_name), "_s1")),

           purrr::set_names(purrr::map_if(.SD,
                                          names(.SD) != wt_name,
                                          ~ sum(get(wt_name) * . * ., na.rm = TRUE),
                                          .else = ~ NA_real_),
                            paste0(c(col_vars, wt_name), "_s2")),

           # G1 (Phase 3a) + Last Phase z16-ii: the THREE extra sufficient statistics the flat-design
           # variance of a mean needs -- Sigma w^2, Sigma w^2 x, Sigma w^2 x^2 (Kish only ever used
           # the first, which is that formula with the outcome discarded). Accumulated whenever the
           # table is WEIGHTED, never on an option (ruling 8): the aggregate then has ONE shape, so
           # toggling tabxplor.design_effect is a jamovi cache HIT instead of a re-aggregate, and
           # num_core() decides from the resolved BASIS whether to use them. All three are ADDITIVE,
           # so num_rollup() gives the total rows their own exact variance by summation.
           purrr::set_names(purrr::map_if(.SD,
                                          names(.SD) != wt_name,
                                          ~ sum(get(wt_name)^2 * as.integer(!is.na(.)), na.rm = TRUE),
                                          .else = ~ NA_real_),
                            paste0(c(col_vars, wt_name), "_w2")),

           purrr::set_names(purrr::map_if(.SD,
                                          names(.SD) != wt_name,
                                          ~ sum(get(wt_name)^2 * ., na.rm = TRUE),
                                          .else = ~ NA_real_),
                            paste0(c(col_vars, wt_name), "_w2s1")),

           purrr::set_names(purrr::map_if(.SD,
                                          names(.SD) != wt_name,
                                          ~ sum(get(wt_name)^2 * . * ., na.rm = TRUE),
                                          .else = ~ NA_real_),
                            paste0(c(col_vars, wt_name), "_w2s2"))
         ),
         keyby = c(tab_row_names)][
           , paste0(wt_name, c("_n", "_wn", "_s1", "_s2", "_w2", "_w2s1", "_w2s2")) := NULL]
  }
}

# tab_aggregate_num() -- the numeric TIER-1 producer (Phase 7d). Prepped microdata -> the
# finest-grain moment-sum aggregate keyed by c(tab_vars, row_var), carrying, per numeric col_var,
# the sufficient statistics num_derive_stats() / num_rollup() consume. It is the numeric analogue of
# the count `.fine` tab_build() builds for factors (tab.R ~L1349), and the aggregate that
# tab_num(.fine=) adopts instead of re-scanning. Byte-identical to tab_num()'s own scan: BOTH route
# through num_moment_scan() (the shared math) -- only the quosure + data-prep plumbing is mirrored.
#
# NA is KEPT here (na = "keep") so the jmvtab cache can collapse NA post-aggregate; na = "drop"
# listwise-removes the row_var/tab_var NAs pre-scan, exactly as tab_num() does. The producer returns
# the RAW scan (the factor-key coercion + na-order normalisation that follow in tab_num() stay in
# the transform, applied unconditionally on both the adopt-.fine and raw paths -> the adopted
# aggregate normalises identically).
tab_aggregate_num <- function(data, row_var, col_vars, tab_vars, wt,
                              na = c("keep", "drop")) {
  row_var_quo <- rlang::enquo(row_var)
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data |> dplyr::mutate(no_row_var = factor("no_row_var"))
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::ensym(row_var)
  }

  col_vars <- rlang::enquo(col_vars)
  if (quo_miss_na_null_empty_no(col_vars)) {
    data     <- data |> dplyr::mutate(no_col_var = factor("n"))
    col_vars <- rlang::syms("no_col_var")
  } else {
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
  }

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character()
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }

  tab_row_names <- purrr::map_chr(c(tab_vars, row_var), rlang::as_name)
  na <- na[1]
  stopifnot(na %in% c("keep", "drop"))

  data <- data |>
    dplyr::select(!!!tab_vars, !!row_var, !!!col_vars, !!wt) |>
    dplyr::mutate(dplyr::across((!!wt | tidyselect::all_of(as.character(col_vars))) &
                                  !where(is.numeric), as.numeric)
    )

  data.table::setDT(data)
  if (na == "drop") data <- stats::na.omit(data, tab_row_names)
  if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop')")

  num_moment_scan(data, tab_row_names, col_vars, wt)
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
# See: CLAUDE.md > 2.0.0 roadmap > Phase 3a; dev/tabxplor_2.0.0_decisions.md §20.

# THE critical value of every interval in the package (Last Phase z16-i, W7): the two-sided Student
# quantile at `df` degrees of freedom, which at `df = Inf` IS the normal quantile -- `qt(p, Inf)` is
# bit-identical to `qnorm(p)`, so the default is byte-identical to the z the engines used before.
# WHY one function: `survey` refers every interval to `t(degf)` where `degf = #PSU - #strata`;
# tabxplor referred proportions to z and means to `t(n_eff - 1)`, i.e. to an EFFECTIVE SAMPLE SIZE,
# which has nothing to do with the design's degrees of freedom -- measured 15 % too narrow on a
# proportion at 10 PSUs. Threading `df` here covers the score intervals too (Wilson / Newcombe / Katz
# / Woolf are z-based by construction; substituting t(degf) for z is survey's own `xlogit` idiom).
#' @keywords internal
conf_level_to_crit <- function(conf_level, df = Inf) {
  stopifnot(conf_level >= 0, conf_level <= 1)
  stats::qt(1 - (1 - conf_level) / 2, df_clean(df))
}

# THE df sanitiser -- "no df here" is Inf, i.e. refer to z. Absent / NA / non-positive all mean the
# same thing (no design, an empty design, a design whose degf could not be had), and every engine that
# takes a `df` needs exactly this line before qt()/pt(). It was written out four times.
#' @keywords internal
df_clean <- function(df) {
  df <- as.double(df)
  if (!length(df)) return(Inf)
  df[is.na(df) | df <= 0] <- Inf
  df
}

# The normal quantile (z-score) for a two-sided confidence level -- conf_level_to_crit() at df = Inf.
# (Moved here from tab.R in Phase 17a: it belongs beside its only callers, the CI engine.)
#' @keywords internal
zscore_formula <- function(conf_level) conf_level_to_crit(conf_level, Inf)


# === SECTION: the CI-method vocabulary ==============================================================
#
# THE interval kinds a tabxplor table can choose a method for, each with its legal values, FIRST = the
# default -- declared once, beside the engines that implement them.
#   cell        a proportion's own interval          (ci = "cell")     ci_wilson / ci_wald / ci_beta
#   diff        a proportion minus its reference     (ci = "diff")     ci_prop_diff
#   mean_diff   a numeric mean minus its reference                     ci_mean_diff2
#   mean_ratio  a numeric mean over its reference    (color = "ratio") ci_mean_ratio
# Last Phase z16-iiiii: this table IS the public grammar. One named vector,
# `ci_method = c(cell = , diff = , mean_diff = , mean_ratio = )`, partial (an unnamed slot keeps its
# default), replaced five parallel `method_*` arguments that had to be listed, validated, threaded,
# cache-keyed and stored one by one across six files. There is no `ratio` slot: a proportion ratio has
# exactly one method (Katz's log risk-ratio), so it is not a choice -- the never-released
# `method_ratio` had one legal value and went with the five.
CI_METHODS <- list(
  cell       = c("wilson", "wald", "beta"),
  diff       = c("newcombe", "ac", "wald"),
  mean_diff  = c("welch", "student"),
  mean_ratio = c("robust", "quasipoisson", "poisson")
)

# The package's own methods -- DERIVED from the table above, so a default cannot drift from the values
# it is chosen among.
#' @keywords internal
default_ci_method <- function() vapply(CI_METHODS, `[[`, character(1), 1L)

# Resolve the public grammar into the full four-slot vector: the defaults, overwritten by the
# soft-deprecated `method_cell` / `method_diff` (released CRAN arguments), then by `ci_method`.
# Validation is one loop over CI_METHODS, so an unknown slot or an illegal value is named the same way
# by every entry point -- tab(), tab_many(), tab_num(), tab_counts() and tab_ci().
#' @keywords internal
resolve_ci_method <- function(ci_method = NULL, method_cell = NULL, method_diff = NULL,
                              fn = "tab") {
  out <- default_ci_method()
  for (s in c("cell", "diff")) {
    v <- if (s == "cell") method_cell else method_diff
    if (is.null(v) || identical(v, out[[s]])) next
    lifecycle::deprecate_soft("2.0.0", paste0(fn, "(method_", s, " = )"),
                              paste0(fn, "(ci_method = )"))
    out[[s]] <- v[[1]]
  }
  if (is.null(ci_method) || !length(ci_method)) ci_method <- character()
  nm <- names(ci_method)
  if (length(ci_method) && (is.null(nm) || !all(nzchar(nm))))
    cli::cli_abort(c("{.arg ci_method} must be named.",
                     "i" = "One entry per interval kind, e.g. {.code ci_method = c(cell = \"beta\")}.",
                     "i" = "Kinds: {.val {names(CI_METHODS)}}."))
  bad <- setdiff(nm, names(CI_METHODS))
  if (length(bad))
    cli::cli_abort(c("Unknown {.arg ci_method} {cli::qty(length(bad))}name{?s} {.val {bad}}.",
                     "i" = "Kinds: {.val {names(CI_METHODS)}}."))
  for (s in nm) out[[s]] <- as.character(ci_method[[s]])[[1]]
  for (s in names(CI_METHODS)) if (!out[[s]] %in% CI_METHODS[[s]])
    cli::cli_abort(c("{.arg ci_method} {.field {s}} must be one of {.val {CI_METHODS[[s]]}}.",
                     "x" = "Got {.val {out[[s]]}}."))
  out
}

#' Convert confidence levels into z thresholds
#'
#' @description Turn one or several confidence levels into the two-sided normal (z) thresholds they
#' correspond to, rounded for readability. It is a convenience for writing the \code{zscore} color
#' break scale (\code{\link{set_color_breaks}}) in the vocabulary you already use elsewhere —
#' confidence levels — instead of remembering that 95 % is 1.96. The scale itself always stores plain
#' z magnitudes, so \code{conf_level_to_z(0.95)} and \code{1.96} are strictly interchangeable.
#'
#' @param conf_level A numeric vector of confidence levels, each between 0 and 1
#'   (e.g. \code{c(0.95, 0.99)}).
#' @param digits Number of digits to round to (default 2). Rounding keeps color legends readable
#'   (\code{"+1.96"} rather than \code{"+1.959964"}); pass \code{Inf} for the exact values.
#'
#' @return A numeric vector of positive z thresholds, the same length as \code{conf_level}.
#' @export
#'
#' @examples
#' conf_level_to_z(c(0.95, 0.99))
#'
#' # the default `zscore` break scale (color = "contrib", color_signif = "guaranteed_effect")
#' conf_level_to_z(c(0.95, 0.99, 0.9999, 1 - 2e-9))
#'
#' \donttest{
#' set_color_breaks(zscore = conf_level_to_z(c(0.95, 0.999)))
#' set_color_breaks(zscore = c(2, 3, 4, 6))  # or plain z values, identically
#' }
conf_level_to_z <- function(conf_level, digits = 2) {
  round(zscore_formula(conf_level), digits)
}

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
  # df <= 0 (e.g. a mean cell with n = 1 -> df = n - 1 = 0) has no defined t interval: coerce to NA so
  # qt/pt return NA cleanly (df = 0 would be NaN + a warning) -> such a cell is left uncoloured/blank.
  df     <- ifelse(df > 0, df, NA_real_)
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
ci_wilson <- function(p, n, conf_level = 0.95, df = Inf) {
  b <- wilson_bounds(p, n, conf_level_to_crit(conf_level, df))
  list(inf = b$inf, sup = b$sup, pvalue = vctrs::vec_recycle(NA_real_, length(p)))
}

# PIVOT counterpart for a single proportion: the WALD normal-approximation interval
# p +/- z*sqrt(p(1-p)/n) (Phase 7g -- an opt-in `method_cell`, commonly taught). It is the
# degenerate one-group version of the Wald arm already in ci_prop_diff(). Cell CI -> pvalue NA.
# WARNING: at p in {0, 1} se = 0 -> a degenerate zero-width interval (Wilson never degenerates);
# bounds can also fall outside [0, 1] (the pct_ci display clamps to [0, 100], same as method_diff
# = "wald"). Kept for teaching parity; wilson stays the default.
ci_wald <- function(p, n, conf_level = 0.95, df = Inf) {
  ci_pivot(p, sqrt(p * (1 - p) / n), df = df, conf_level = conf_level, want_p = FALSE)
}

# KORN-GRAUBARD shape, single proportion (Last Phase z16-iii, ruling 4): a Clopper-Pearson interval
# on the EFFECTIVE sample size -- literally `survey::svyciprop(method = "beta")`, which is defined as
# binom.test "with an effective sample size based on the estimated variance of the proportion". It is
# the textbook design-based cell interval, and it needs nothing new here: `n` is already the base this
# framework computes (n_eff = p(1-p)/Var_design). Opt-in via `method_cell = "beta"`, NOT a default --
# one interval SHAPE at every position keeps the legend, the goldens and cross-table comparability one
# story, and beta is deliberately conservative near 0 and 1 where Wilson is not.
# The SECOND half of Korn-Graubard, and the reason this takes two sample sizes (Last Phase z16-iiiii):
# beta quantiles have no degrees of freedom of their own, so survey carries the design's in by shrinking
# the effective n FIRST -- `n.eff * (qt(a, nrow - 1) / qt(a, degf))^2`, the ratio of the SRS critical
# value to the design's. `n_raw` is that `nrow`: the cell's own unweighted base, which the caller already
# holds (the `tot_n` field). Without it, a design built on few PSUs printed an interval that was measured
# 25 % too short.
# DESIGN: the rescale converts an interval referred to n-1 into one referred to `df`, so where there is
# no design (`df` = Inf, this framework's "refer to z") there is nothing to convert and the factor is 1
# -- which is also what survey itself gives at ids = ~1, where degf IS n-1. That is what keeps the
# weights basis, and every unweighted table, byte-identical.
# WARNING: `df` is the WHOLE design's degf, captured once at the boundary (svy_degf), as it is for every
# other interval here. survey's own call on a domain uses that domain's -- the same number whenever the
# row variable is crossed with the PSUs (the ordinary case), smaller when a domain drops whole PSUs.
ci_beta <- function(p, n, conf_level = 0.95, df = Inf, n_raw = NULL) {
  a  <- (1 - conf_level) / 2
  dfd <- df_clean(df)
  if (!is.null(n_raw) && is.finite(dfd[1])) {
    srs <- as.double(n_raw) - 1
    n   <- n * ifelse(is.finite(srs) & srs > 0,
                      (stats::qt(a, srs) / stats::qt(a, dfd[1]))^2, 1)
  }
  lo <- stats::qbeta(a,     n * p,     n * (1 - p) + 1)
  hi <- stats::qbeta(1 - a, n * p + 1, n * (1 - p))
  bad <- !is.finite(n) | n <= 0 | !is.finite(p)
  lo[bad] <- NA_real_; hi[bad] <- NA_real_
  lo[!is.na(p) & p <= 0] <- 0
  hi[!is.na(p) & p >= 1] <- 1
  list(inf = lo, sup = hi, pvalue = vctrs::vec_recycle(NA_real_, length(p)))
}

# SCORE shape, proportion difference: Newcombe method 10 (hybrid score, built from the two
# groups' Wilson intervals). Its exact dual test has no closed form, so the inversion p is
# found by a vectorised bisection on z (monotone). want_p = FALSE skips it (one interval eval).
ci_newcombe <- function(p1, n1, p2, n2, conf_level = 0.95, want_p = TRUE, df = Inf) {
  d  <- p1 - p2
  z  <- conf_level_to_crit(conf_level, df)
  w1 <- wilson_bounds(p1, n1, z)
  w2 <- wilson_bounds(p2, n2, z)
  inf <- d - sqrt((p1 - w1$inf)^2 + (w2$sup - p2)^2)
  sup <- d + sqrt((w1$sup - p1)^2 + (p2 - w2$inf)^2)
  pvalue <- if (want_p) newcombe_pvalue(p1, n1, p2, n2, df = df)
            else vctrs::vec_recycle(NA_real_, length(d))
  list(inf = inf, sup = sup, pvalue = pvalue)
}

# Continuous CI-inversion p for Newcombe: the smallest alpha at which the interval excludes 0.
# g(z) = |d| - near_margin(z) is decreasing in z (the interval widens with z); bisect for the
# root z*, then p = 2*(1 - Phi(z*)). By construction cut(p) matches the Newcombe bracket's own
# 0-inclusion at any level. Fully vectorised (fixed iterations, no per-cell root solver).
newcombe_pvalue <- function(p1, n1, p2, n2, steps = 50L, df = Inf) {
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
  q <- (lo + hi) / 2
  dfp <- df_clean(df)
  p <- 2 * stats::pt(-q, dfp)
  p[!is.finite(d)] <- NA_real_
  p
}

# Proportion-difference CI + inclusion significance, dispatched on method. Newcombe (default)
# is the score hybrid; AC and Wald are pivot-shaped (the caller-free adjusted est/se are built
# here). Weighted rule (§14): the caller passes the WEIGHTED proportions p1/p2 and the
# UNWEIGHTED bases n1/n2 (their cells' tot_n).
ci_prop_diff <- function(p1, n1, p2, n2, conf_level = 0.95, method = "newcombe", want_p = TRUE,
                         df = Inf) {
  switch(
    method,
    "newcombe" = ci_newcombe(p1, n1, p2, n2, conf_level, want_p, df = df),
    "ac" = {
      a1 <- (p1 * n1 + 1) / (n1 + 2); a2 <- (p2 * n2 + 1) / (n2 + 2)
      ci_pivot(a1 - a2, sqrt(a1 * (1 - a1) / (n1 + 2) + a2 * (1 - a2) / (n2 + 2)),
               df = df, conf_level = conf_level, want_p = want_p)
    },
    "wald" = ci_pivot(p1 - p2, sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2),
                      df = df, conf_level = conf_level, want_p = want_p),
    stop("unknown method_diff: ", method)
  )
}

# MULTIPLICATIVE shape, proportion RATIO: Katz's log-RR interval (Phase 14b) --
# exp(log(p1/p2) +/- z * se), se(log RR) = sqrt((1-p1)/(n1 p1) + (1-p2)/(n2 p2)). The bounds are on
# the RATIO scale (neutral 1), stored as ci_type = "ratio", and its dual is the log-RR Wald test, so
# bracket <-> stars stay exact duals (§20) like every other method here.
#
# Why it exists: `ratio` had no native interval, so a ratio-coloured cell borrowed the DIFFERENCE
# bounds and converted them, holding the reference proportion fixed at its point estimate. That is a
# valid significance test (H0: p1 = p2 is the same null on either scale) but not a ratio interval:
# it ignores the reference's own uncertainty. When the ratio is the measure the reader sees, it now
# owns the stored interval, and any difference channel converts FROM it instead.
# Weighted rule (§14): WEIGHTED proportions p1/p2, UNWEIGHTED bases n1/n2.
# WARNING: undefined at p1 = 0 (log 0) or p2 = 0 (the division) -> NA bounds and NA p, so an empty
# cell or an empty reference is left uncoloured/unstarred rather than +/-Inf. Katz is the standard
# large-sample RR interval and, like every Wald-family method here, wants a few counts per cell.
ci_katz_rr <- function(p1, n1, p2, n2, conf_level = 0.95, want_p = TRUE, df = Inf) {
  rr  <- p1 / p2
  lrr <- log(rr)
  se  <- sqrt((1 - p1) / (n1 * p1) + (1 - p2) / (n2 * p2))
  z   <- conf_level_to_crit(conf_level, df)
  dfp <- df_clean(df)
  inf <- exp(lrr - z * se)
  sup <- exp(lrr + z * se)
  pvalue <- if (want_p) 2 * stats::pt(-abs(lrr / se), dfp)
            else vctrs::vec_recycle(NA_real_, length(rr))
  pvalue <- vctrs::vec_recycle(pvalue, length(rr))
  bad <- !is.finite(lrr) | !is.finite(se) | se == 0
  inf[bad] <- NA_real_; sup[bad] <- NA_real_; pvalue[bad] <- NA_real_
  list(inf = inf, sup = sup, pvalue = pvalue)
}

# Mean-difference CI + inclusion significance. Rule B (14v-ii, decisions §48): the reference
# distribution is a property of the METHOD, not of the stars toggle -- a mean variance is always
# ESTIMATED, so this is a Student t with the method's own df. `method = "welch"` (default) uses each
# group's own variance + the Welch-Satterthwaite df (heteroscedastic, tab()'s assumption-light default);
# `method = "student"` uses the pooled variance + df = n1+n2-2 (homoscedastic = an OLS two-group
# coefficient CI). want_p gates ONLY whether the inversion p-value is computed; the df (hence the
# bracket width) no longer flips with stars. Weighted rule (§14): weighted means/variances, unweighted
# n1/n2.
ci_mean_diff2 <- function(m1, v1, n1, m2, v2, n2, conf_level = 0.95, want_p = TRUE,
                          method = "welch", df_design = Inf) {
  if (identical(method, "student")) {
    sp2 <- ((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2)
    se  <- sqrt(sp2 * (1 / n1 + 1 / n2))
    df  <- n1 + n2 - 2
  } else {
    se <- sqrt(v1 / n1 + v2 / n2)
    df <- se^4 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))
  }
  ci_pivot(m1 - m2, se, df = df_or_design(df, df_design), conf_level = conf_level, want_p = want_p)
}

# Last Phase z16-i (W7): a DESIGN's degrees of freedom REPLACE the sample-based ones -- `survey`
# refers every interval, mean included, to t(degf) = t(#PSU - #strata), which no n_eff can stand in
# for. `df_design` is NA / Inf everywhere else, so the sample-based df is kept unchanged.
df_or_design <- function(df, df_design) {
  d <- as.double(df_design)
  if (!length(d) || anyNA(d) || !all(is.finite(d)) || any(d <= 0)) return(df)
  vctrs::vec_recycle(d, length(df))
}

# MULTIPLICATIVE shape, ratio of MEANS (14v-ii, decisions §48): exp(log(m1/m2) +/- q * se_logR), the
# ratio counterpart of ci_mean_diff2. Three variance assumptions (`method`) spanning the dispersion
# ladder a Poisson / quasi-Poisson regression walks -- all closed-form from the two groups' means /
# variances / bases, reproducing the matching regression exactly:
#   robust       each group's OWN empirical variance (delta method on log = modified/robust Poisson,
#                GEE sandwich): z (asymptotic, no exact small-sample t).
#   poisson      naive Var = mu, so se_logR = sqrt(1/S1 + 1/S2), S = m*n the group total count: z.
#   quasipoisson Poisson se * sqrt(phi), phi the pooled two-group Pearson dispersion (= quasi-Poisson
#                regression's se): Student t(n1+n2-2). Auto-degrades to the naive Poisson when phi ~= 1.
# Rule B (§48): the df is the method's own, not stars-gated. Neutral 1 on the ratio scale
# (ci_type = "ratio"); dual = the log-ratio Wald/t test, so bracket <-> stars stay duals. Weighted
# rule (§14): weighted means/variances, unweighted n1/n2. WARNING: undefined at m <= 0 -> NA bounds/p
# (an empty group is left uncoloured/unstarred).
ci_mean_ratio <- function(m1, v1, n1, m2, v2, n2, conf_level = 0.95, want_p = TRUE,
                          method = "robust", df_design = Inf) {
  lr <- log(m1 / m2)
  se <- switch(
    method,
    "robust"       = sqrt((v1 / n1) / m1^2 + (v2 / n2) / m2^2),
    "poisson"      = sqrt(1 / (m1 * n1) + 1 / (m2 * n2)),
    "quasipoisson" = {
      phi <- ((n1 - 1) * v1 / m1 + (n2 - 1) * v2 / m2) / (n1 + n2 - 2)
      sqrt(1 / (m1 * n1) + 1 / (m2 * n2)) * sqrt(phi)
    },
    stop("unknown method_mean_ratio: ", method)
  )
  df  <- if (identical(method, "quasipoisson")) n1 + n2 - 2 else Inf
  res <- ci_pivot(lr, se, df = df_or_design(df, df_design), conf_level = conf_level,
                  want_p = want_p)
  inf <- exp(res$inf); sup <- exp(res$sup)
  bad <- !is.finite(lr) | !is.finite(se) | se == 0
  inf[bad] <- NA_real_; sup[bad] <- NA_real_; res$pvalue[bad] <- NA_real_
  list(inf = inf, sup = sup, pvalue = res$pvalue)
}

# MULTIPLICATIVE shape, ODDS RATIO from a 2x2 (14v-ii): Woolf's log-OR Wald interval, the crude-OR
# counterpart used by the empirical binomial column (dual = the log-OR Wald test -> bracket <-> stars
# duals). a/b = the level's (positive, negative) counts, c/d = the reference's. Weighted rule (§14):
# the caller builds the cells from the WEIGHTED proportion x the UNWEIGHTED base (a = p1*n1,
# b = (1-p1)*n1, ...). z-based (an OR has no exact small-sample t). WARNING: undefined when any cell
# is 0 (log 0 / 1/0) -> NA bounds/p.
ci_or <- function(a, b, c, d, conf_level = 0.95, want_p = TRUE, df = Inf) {
  lor <- log((a * d) / (b * c))
  se  <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  z   <- conf_level_to_crit(conf_level, df)
  dfp <- df_clean(df)
  inf <- exp(lor - z * se); sup <- exp(lor + z * se)
  pvalue <- if (want_p) 2 * stats::pt(-abs(lor / se), dfp)
            else vctrs::vec_recycle(NA_real_, length(lor))
  pvalue <- vctrs::vec_recycle(pvalue, length(lor))
  bad <- !is.finite(lor) | !is.finite(se) | se == 0
  inf[bad] <- NA_real_; sup[bad] <- NA_real_; pvalue[bad] <- NA_real_
  list(inf = inf, sup = sup, pvalue = pvalue)
}


# === SECTION: whole-table tests (Chi2 + ANOVA), vectorised over all tables ============
#
# The Phase 3b test engine. Whole-table omnibus tests computed for EVERY (sub)table in ONE
# vectorised pass, from the already-aggregated cell statistics (never a raw N-scan). Each
# distinct table is tagged by `table_id` (a (tab_var-group x col_var) key); all tables are
# stacked into one long data.table and the test math is a handful of grouped ops, so the cost
# is O(total cells / groups) and independent of the NUMBER of tables -- the framework for the
# "many tests of the same kind on different tables" case (several row_vars x col_vars, tab_vars
# with comp="tab"). This replaces tab_chi2()'s per-(sub)table group_split() + stats::chisq.test()
# loop (its #1-cost, N-independent, dplyr-overhead-bound path -- see the perf profile).
#
# KEY CONSTRAINTS:
#   - Chi2 must match stats::chisq.test() DEFAULTS EXACTLY, including the Yates continuity
#     correction on 2x2 (G2 parity; test-calculations.R locks it). Chi2 is FULLY UNWEIGHTED
#     (counts and n) -- the one documented exception to the §14 weighted rule.
#   - Welch's F must match stats::oneway.test(var.equal = FALSE); classic F must match
#     oneway.test(var.equal = TRUE). The F follows §14 (weighted group means/variances +
#     unweighted n) -- on unweighted data it reduces to oneway.test, which the parity test pins.
# See: CLAUDE.md > 2.0.0 roadmap > Phase 3b; dev/tabxplor_2.0.0_decisions.md §24, §16, §14.

# agg_chi2() -- Pearson chi2 decomposition for every table at once. Inputs are equal-length
# vectors describing one cell each: `table_id` (which table), `row_id`/`col_id` (the cell's
# row_var level / col_var level within that table) and `o` (the observed count -- UNWEIGHTED n
# for the p-value, or weighted wn for the contribution/variance pass). Returns:
#   $tables : one row per table_id -- statistic (X2), df, n (grand total), min_e (smallest
#             expected count, a cheap "low expected" flag), pvalue.
#   $cells  : the input cells in INPUT ORDER, augmented with e (expected), contrib
#             (Pearson term, 0 for cells in an all-empty row/col), signed_contrib
#             (sign(o-e)*contrib) -- consumed by the color="contrib" write-back.
# Parity with chisq.test(): empty rows/cols are dropped before df / Yates (matching the
# historical pre-chisq drop); df = (r-1)(c-1) on the reduced matrix; Yates uses the per-cell
# pmin(0.5, |o-e|), which on a genuine 2x2 (all |o-e| equal) equals chisq.test()'s scalar
# min(0.5, abs(x-E)); a degenerate reduced table (df < 1) yields pvalue = NA (chisq.test errors
# there, the old path returned NA via possibly()).
agg_chi2 <- function(table_id, row_id, col_id, o, correct = TRUE) {
  DT <- data.table::data.table(table_id = table_id, row_id = row_id,
                               col_id = col_id, o = as.double(o))
  DT[, rowtot := sum(o), by = list(table_id, row_id)]
  DT[, coltot := sum(o), by = list(table_id, col_id)]
  DT[, ok := rowtot > 0 & coltot > 0]
  DT[, grandtot := sum(o[ok]), by = table_id]
  DT[, nr := data.table::uniqueN(row_id[rowtot > 0]), by = table_id]
  DT[, nc := data.table::uniqueN(col_id[coltot > 0]), by = table_id]
  DT[, e := rowtot * coltot / grandtot]
  yates <- if (correct) data.table::fifelse(DT$ok & DT$nr == 2L & DT$nc == 2L,
                                            pmin(0.5, abs(DT$o - DT$e)), 0) else 0
  DT[, contrib := data.table::fifelse(ok, (abs(o - e) - yates)^2 / e, 0)]
  DT[, signed_contrib := sign(o - e) * contrib]
  # Effect size reads the UNCORRECTED Pearson chi2 (the standard Cramer's V / phi convention, matching
  # DescTools::CramerV / vcd::assocstats defaults); the p-value keeps the Yates-corrected `contrib`.
  DT[, contrib_unc := data.table::fifelse(ok, (o - e)^2 / e, 0)]

  tables <- DT[ok == TRUE, {
    nr_ <- data.table::uniqueN(row_id)
    nc_ <- data.table::uniqueN(col_id)
    df_ <- (nr_ - 1L) * (nc_ - 1L)
    st_ <- sum(contrib)
    n_  <- grandtot[1]
    # Cramer's V = sqrt(X2 / (N * (min(r, c) - 1))); for a 2x2 this equals phi = sqrt(X2 / N).
    kdim <- min(nr_, nc_) - 1L
    es_  <- if (kdim >= 1L) sqrt(sum(contrib_unc) / (n_ * kdim)) else NA_real_
    list(statistic = st_, df = df_, n = n_, min_e = min(e),
         pvalue = if (df_ >= 1L) stats::pchisq(st_, df_, lower.tail = FALSE) else NA_real_,
         effect_size = es_,
         es_type = if (nr_ == 2L && nc_ == 2L) "phi" else "cramer_v")
  }, by = table_id]

  list(tables = tables,
       cells  = DT[, list(table_id, row_id, col_id, e, contrib, signed_contrib)])
}

# agg_anova() -- one-way ANOVA (Welch + classic F) for every mean table at once, from per-group
# summary statistics: `table_id`, `group_id` (row_var level), `n` (UNWEIGHTED count), `mean`
# and `var` (WEIGHTED group mean/variance, §14; on unweighted data these are the sample mean and
# n-1 variance, so the tests reduce to stats::oneway.test). Groups with n < 2, non-finite mean/var
# or var <= 0 are dropped (outside the F domain); a table left with k < 2 groups yields NA.
# Returns one row per table_id with both tests. Welch matches oneway.test(var.equal = FALSE);
# classic (pooled) matches oneway.test(var.equal = TRUE) / aov.
agg_anova <- function(table_id, group_id, n, mean, var) {
  DT <- data.table::data.table(table_id = table_id, n = as.double(n),
                               mean = as.double(mean), var = as.double(var))
  DT <- DT[is.finite(mean) & is.finite(var) & var > 0 & n >= 2]
  DT[, w := n / var]
  DT[, {
    k <- .N
    if (k < 2L) {
      list(k = k, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_, pvalue = NA_real_,
           statistic_classic = NA_real_, df1_classic = NA_real_, df2_classic = NA_real_,
           pvalue_classic = NA_real_, n = sum(n), effect_size = NA_real_)
    } else {
      sw    <- sum(w)
      xbarw <- sum(w * mean) / sw
      A     <- sum(w * (mean - xbarw)^2) / (k - 1)
      tmp   <- sum((1 - w / sw)^2 / (n - 1))
      Fw    <- A / (1 + 2 * (k - 2) / (k^2 - 1) * tmp)
      df1w  <- k - 1
      df2w  <- (k^2 - 1) / (3 * tmp)
      pw    <- stats::pf(Fw, df1w, df2w, lower.tail = FALSE)
      N     <- sum(n)
      grand <- sum(n * mean) / N
      ssb   <- sum(n * (mean - grand)^2)
      ssw   <- sum((n - 1) * var)
      df1c  <- k - 1
      df2c  <- N - k
      Fc    <- (ssb / df1c) / (ssw / df2c)
      pc    <- stats::pf(Fc, df1c, df2c, lower.tail = FALSE)
      # eta^2 = SSB / SST = the share of variance explained by the row_var groups (the numeric
      # analogue of Cramer's V). From the same SS the classic F forms; weighted per S14, exact when
      # unweighted. sst = ssb + ssw > 0 here (k >= 2, var > 0 on at least one kept group).
      eta2  <- ssb / (ssb + ssw)
      list(k = k, statistic = Fw, df1 = df1w, df2 = df2w, pvalue = pw,
           statistic_classic = Fc, df1_classic = df1c, df2_classic = df2c,
           pvalue_classic = pc, n = N, effect_size = eta2)
    }
  }, by = table_id]
}

# agg_fisher() -- Fisher's exact test for the SMALL factor tables where the chi2 is unreliable
# (smallest expected count < 5, the standard validity threshold that already drives the "!" weak flag).
# Same long (table_id, row_id, col_id, o = UNWEIGHTED count) inputs as agg_chi2(); `which_ids` bounds the
# work to the flagged tables (Last Phase j -- a per-table loop, so only ever a handful). Each table is
# reshaped to its integer count matrix, empty rows/cols dropped (matching agg_chi2's `ok`), and tested.
# SIZE GUARD: an exact test is meaningful (and feasible) only for a SMALL sample -- a large table with
# one rare category has a low expected count but a fine chi2, and the exact test would blow up FEXACT's
# workspace. So a grid over `max_cells` non-empty cells OR a total over `n_exact` uses a Monte-Carlo p
# (simulate.p.value, B reps); an exact call that still errors falls back to it too. `simulated` flags
# which was used, so the caller can show the EXACT p only (never a silent cap).
agg_fisher <- function(table_id, row_id, col_id, o, which_ids,
                       max_cells = 25L, n_exact = 2000, B = 2000L) {
  DT <- data.table::data.table(table_id = table_id, row_id = row_id,
                               col_id = col_id, o = as.double(o))
  rows <- lapply(which_ids, function(id) {
    d <- DT[table_id == id]
    M <- data.table::dcast(d, row_id ~ col_id, value.var = "o", fill = 0, fun.aggregate = sum)
    M <- as.matrix(M[, -1L])
    M <- M[rowSums(M) > 0, , drop = FALSE]
    M <- M[, colSums(M) > 0, drop = FALSE]
    if (nrow(M) < 2L || ncol(M) < 2L)
      return(list(table_id = id, pvalue = NA_real_, simulated = FALSE))
    M   <- round(M)
    sim <- (nrow(M) * ncol(M)) > max_cells || sum(M) > n_exact
    res <- tryCatch(
      if (sim) stats::fisher.test(M, simulate.p.value = TRUE, B = B)
      else     stats::fisher.test(M),
      error = function(e) tryCatch(stats::fisher.test(M, simulate.p.value = TRUE, B = B),
                                   error = function(e2) NULL))
    if (is.null(res)) list(table_id = id, pvalue = NA_real_, simulated = TRUE)
    else list(table_id = id, pvalue = res$p.value,
              simulated = sim || grepl("simulated", res$method, fixed = TRUE))
  })
  data.table::rbindlist(rows)
}
