# PURPOSE: The aggregate core's arithmetic -- sufficient-statistic aggregation, the confidence-
#   interval engines, and the vectorised whole-table tests.
# ROLE: The single computation core both leaves route through (R/tab-leaf.R): counts and moment
#   sums in, fmt fields out. It owns no table structure and touches no fmt record -- every function
#   here takes plain vectors or a data.table and returns plain vectors.
# KEY CONSTRAINTS:
#   - TWO declared vocabularies, side by side: CI_METHODS says which METHOD a kind of interval may be
#     built with, CI_GEOMS which INTERVAL a column's plan asks for. Read CI_GEOMS only through its
#     accessors, so its three consumers cannot answer the reference-cell question differently.
#   - The engines are ELEMENTWISE over cells and know nothing of a variable's level set, so a method
#     whose variance is pooled over that set (CI_POOLED: "ols", "quasipoisson") takes it as `pool`,
#     computed by the caller through ci_pool_disp(). Never give an engine a grouping key instead: the
#     vectors it receives span total rows and sub-tables, which no key in its argument list separates.
#   - SIGNIFICANCE IS CI-INVERSION. The stored per-cell `pvalue` inverts the SAME interval that drew
#     the bracket, so stars and bounds can never contradict each other, whatever the method.
#   - data.table NSE: keep aggregations GForce-friendly (bare per-column sums), and never re-scan N
#     rows for a quantity the aggregate already carries.
# See: CLAUDE.md § tabxplor architecture (the inference layer).

# === SECTION: numeric sufficient statistics ==========================================

# DESIGN: the unweighted-vs-weighted variance asymmetry (sample n-1 vs ML /Sigma w) is deliberate, and
#   so is each branch's degenerate group: unweighted var is NA at n <= 1 (hence the NaN -> NA map),
#   weighted var 0 for a single observation, NaN for an all-NA group.
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

# Total rows and total tables by SUMMING the moment sums, which are ADDITIVE: exact, no N re-scan.
num_rollup <- function(agg, by, drop_keys, moment_cols, tab_vars_chr) {
  roll <- if (length(by) == 0) {
    agg[, lapply(.SD, sum, na.rm = TRUE), .SDcols = moment_cols]
  } else {
    agg[, lapply(.SD, sum, na.rm = TRUE), .SDcols = moment_cols, keyby = by]
  }
  if (length(drop_keys) > 0)    roll[, (drop_keys) := "Total"]
  # DESIGN: one SHARED factor ptype per tab_var, taken from the source aggregate -- vctrs refuses to
  # combine two ORDERED factors with different level sets, and a collapsed set carries only "Total".
  if (length(tab_vars_chr) > 0) for (v in tab_vars_chr) {
    src <- agg[[v]]
    data.table::set(roll, j = v, value = if (is.factor(src)) {
      factor(as.character(roll[[v]]), levels = unique(c(levels(src), "Total")),
             ordered = is.ordered(src))
    } else as.factor(roll[[v]]))
  }
  roll
}

# WARNING: byte-identity-critical -- the as.double() coercions are a 32-bit overflow guard on Sigma x^2.
# WARNING: inside a data.table `j` the weight must be referenced by the plain string `wt_name` (captured
#   OUTSIDE the call, where the argument is un-shadowed), NEVER the bare symbol `wt`: data.table exposes
#   every column in `j` scope, so a column named "wt" shadows it.
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

           # What the flat-design variance of a mean needs, accumulated whenever the table is
           # WEIGHTED, never on an option: the aggregate then has ONE shape, so toggling
           # tabxplor.design_effect is a cache hit, not a re-aggregate.
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

# The numeric TIER-1 producer returns the RAW scan: `na = "keep"` lets the jmvtab cache collapse NA
# post-aggregate, and normalisation stays in the transform, so an adopted aggregate matches a scan.
tab_aggregate_num <- function(data, row_var, col_vars, tab_vars, wt,
                              na = c("keep", "drop")) {
  .v <- leaf_defuse_vars(data, rlang::enquo(row_var), rlang::enquo(col_vars),
                         rlang::enquo(tab_vars), rlang::enquo(wt), svy = NULL, plural = TRUE)
  data <- .v$data ; row_var <- .v$row_var ; col_vars <- .v$col ; tab_vars <- .v$tab_vars ; wt <- .v$wt

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
# PURE, vectorised, dependency-free math. Every interval is either PIVOT (estimate +/- q*se, inverted in
# closed form) or SCORE (asymmetric: Wilson and its Newcombe-10 hybrid, inverted by bisection).

# THE critical value of every interval here: the two-sided Student quantile at `df` (`qt(p, Inf)` IS
# `qnorm(p)`). ONE function, because `survey` refers every interval to t(degf), and threading `df` here
# covers the score intervals too -- Wilson / Newcombe / Katz / Woolf are z-based, and substituting t for
# z is survey's own idiom.
#' @keywords internal
#' @noRd
conf_level_to_crit <- function(conf_level, df = Inf) {
  stopifnot(conf_level >= 0, conf_level <= 1)
  stats::qt(1 - (1 - conf_level) / 2, df_clean(df))
}

# THE df sanitiser: "no df here" is Inf, i.e. refer to z -- absent / NA / non-positive all mean the same
# thing (no design, an empty design, a degf that could not be had).
#' @keywords internal
#' @noRd
df_clean <- function(df) {
  df <- as.double(df)
  if (!length(df)) return(Inf)
  df[is.na(df) | df <= 0] <- Inf
  df
}

#' @keywords internal
#' @noRd
zscore_formula <- function(conf_level) conf_level_to_crit(conf_level, Inf)


# === SECTION: the CI-method vocabulary ==============================================================
#
# THE interval kinds a tabxplor table can choose a method for, each with its legal values, FIRST = the
# default -- declared once, beside the engines that implement them.
#   cell        a proportion's own interval          (ci = "cell")     ci_wilson / ci_wald / ci_beta
#   diff        a proportion minus its reference     (ci = "diff")     ci_prop_diff
#   mean_diff   a numeric mean minus its reference                     ci_mean_diff2
#   mean_ratio  a numeric mean over its reference    (color = "ratio") ci_mean_ratio
#   model       a regression coefficient's interval  (tab_reg)         reg_fit's Wald / profile
# There is no `ratio` slot: a proportion ratio has exactly one method (Katz's log risk-ratio), so it is
# not a choice.
CI_METHODS <- list(
  cell       = c("wilson", "wald", "beta"),
  diff       = c("newcombe", "ac", "wald"),
  mean_diff  = c("welch", "student", "ols"),
  mean_ratio = c("robust", "quasipoisson", "poisson"),
  model      = c("wald", "profile")
)

# The methods whose variance is POOLED OVER THE WHOLE VARIABLE, not over the two cells being
# compared -- the two that reproduce a MODEL's interval rather than a two-sample test: `ols` is the
# linear model's coefficient interval (one residual variance over all k levels, df = N - k) and
# `quasipoisson` the quasi-Poisson's (one global Pearson dispersion). The engines are vectorised over
# cells and know nothing of the level set, so the CALLER must supply `pool` -- see ci_pool_disp().
# With no `pool` they fall back to the pair, which IS the level set when the variable has 2 levels.
#' @keywords internal
#' @noRd
CI_POOLED <- list(mean_diff = "ols", mean_ratio = "quasipoisson")

# A crosstab has no model interval and a regression no cell one, so a consumer that enumerates the slots
# (the jamovi vocabulary gate) must be able to ask which producer offers each.
#' @keywords internal
#' @noRd
CI_SLOT_PRODUCER <- c(cell = "tab", diff = "tab", mean_diff = "tab", mean_ratio = "tab",
                      model = "reg")
stopifnot(setequal(names(CI_SLOT_PRODUCER), names(CI_METHODS)))

#' @keywords internal
#' @noRd
ci_slots_of <- function(producer) names(CI_METHODS)[CI_SLOT_PRODUCER[names(CI_METHODS)] == producer]

#' @keywords internal
#' @noRd
default_ci_method <- function() vapply(CI_METHODS, `[[`, character(1), 1L)

#' @keywords internal
#' @noRd
resolve_ci_method <- function(ci_method = NULL, method_cell = NULL, method_diff = NULL,
                              fn = "tab", user_env = rlang::caller_env()) {
  out <- default_ci_method()
  for (s in c("cell", "diff")) {
    v <- if (s == "cell") method_cell else method_diff
    if (is.null(v) || identical(v, out[[s]])) next
    # WARNING: `user_env` must be threaded from the PRODUCER -- this resolver runs inside
    # tab_resolve_common_args(), so lifecycle's own default would name a tabxplor frame as the user.
    lifecycle::deprecate_soft("2.0.0", paste0(fn, "(method_", s, " = )"),
                              paste0(fn, "(ci_method = )"), user_env = user_env)
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


# === SECTION: the interval GEOMETRY vocabulary ======================================================
#
# CI_METHODS above says WHICH METHOD a kind of interval may be built with. CI_GEOMS says WHICH INTERVAL
# a column's plan asks for -- the engine that builds it, the CI_METHODS slot that names it, and the
# EST_SCALES key it makes the column ESTIMATE. One row per (kind x var_kind x scale) the package can
# answer, keyed "<kind>.<var_kind>[.<scale>]", read only through the four readers below, so the factor
# leaf, the numeric leaf and the superseded tab_ci() step cannot answer the same question differently.
#
#   kind         "cell" (the cell's own interval) | "diff" (the cell against its reference)
#   var_kind     the column's fmt_var_kind(): "pct" | "mean". A "count"/"coef" column has no row here.
#   scale        the ci_scale the reader sees: "diff" | "ratio". Absent on a cell interval (no contrast).
#   method_slot  the CI_METHODS slot that CHOOSES the engine -- NA where there is no choice
#   method_fixed the stamped ci_method where there is no choice ("student", "katz")
#   scale_key    the EST_SCALES key stamped on the column. NA = "the level scale stands", which is what
#                a cell interval does: a mean with its own interval is still a mean.
#   wants_ref    the caller must supply `ref` / `ref_n` (+ `ref_var` for a mean)
#   wants_p      a p-value exists at all -- a cell interval has no null, so never
#   ref_cell     "keep" | "na" -- does the cell that IS the reference keep its own interval?
#   engine       the call, written out ONCE.
#
# WARNING: the engine call is written out PER ROW, never as one do.call() over a shared argument list.
#   The proportion engines take `df =` (the design df, straight to conf_level_to_crit); the mean ones
#   take `df_design =`, which REPLACES the sample-based df. A shared name list would make that swappable
#   by a typo; a per-row closure cannot.
CI_GEOMS <- list(
  "cell.pct" = list(
    kind = "cell", var_kind = "pct", scale = NA_character_,
    method_slot = "cell", method_fixed = NA_character_,
    scale_key = NA_character_, wants_ref = FALSE, wants_p = FALSE, ref_cell = "keep",
    engine = function(a) switch(
      a$method,
      "wilson" = ci_wilson(a$est, a$base, conf_level = a$conf_level, df = a$degf),
      "wald"   = ci_wald(  a$est, a$base, conf_level = a$conf_level, df = a$degf),
      # Korn-Graubard's df rescale needs the cell's RAW base beside the effective one -- hence `n_raw`,
      # NOT `base` (that one is n_eff-coalesced and NA'd on the reference cell).
      "beta"   = ci_beta(  a$est, a$base, conf_level = a$conf_level,
                           df = a$degf, n_raw = a$n_raw))),
  "cell.mean" = list(
    kind = "cell", var_kind = "mean", scale = NA_character_,
    method_slot = NA_character_, method_fixed = "student",
    scale_key = NA_character_, wants_ref = FALSE, wants_p = FALSE, ref_cell = "keep",
    # One-sample Student t(n-1) cell interval (a variance is estimated).
    engine = function(a) ci_pivot(a$est, sqrt(a$var / a$base),
                                  df = df_or_design(a$base - 1, a$degf),
                                  conf_level = a$conf_level, want_p = FALSE)),
  "diff.pct.diff" = list(
    kind = "diff", var_kind = "pct", scale = "diff",
    method_slot = "diff", method_fixed = NA_character_,
    scale_key = "points", wants_ref = TRUE, wants_p = TRUE, ref_cell = "na",
    engine = function(a) ci_prop_diff(a$est, a$base, a$ref, a$ref_n, conf_level = a$conf_level,
                                      method = a$method, want_p = a$want_p, df = a$degf)),
  "diff.pct.ratio" = list(
    kind = "diff", var_kind = "pct", scale = "ratio",
    method_slot = NA_character_, method_fixed = "katz",
    scale_key = "pct_ratio", wants_ref = TRUE, wants_p = TRUE, ref_cell = "na",
    # Katz is the only proportion-ratio method, so here the method is not a choice.
    engine = function(a) ci_katz_rr(a$est, a$base, a$ref, a$ref_n, conf_level = a$conf_level,
                                    want_p = a$want_p, df = a$degf)),
  "diff.mean.diff" = list(
    kind = "diff", var_kind = "mean", scale = "diff",
    method_slot = "mean_diff", method_fixed = NA_character_,
    scale_key = "mean_diff", wants_ref = TRUE, wants_p = TRUE, ref_cell = "na",
    engine = function(a) ci_mean_diff2(a$est, a$var, a$base, a$ref, a$ref_var, a$ref_n,
                                       method = a$method, conf_level = a$conf_level,
                                       want_p = a$want_p, df_design = a$degf, pool = a$pool)),
  "diff.mean.ratio" = list(
    kind = "diff", var_kind = "mean", scale = "ratio",
    method_slot = "mean_ratio", method_fixed = NA_character_,
    scale_key = "mean_ratio", wants_ref = TRUE, wants_p = TRUE, ref_cell = "na",
    engine = function(a) ci_mean_ratio(a$est, a$var, a$base, a$ref, a$ref_var, a$ref_n,
                                       method = a$method, conf_level = a$conf_level,
                                       want_p = a$want_p, df_design = a$degf, pool = a$pool))
)

stopifnot(all(vapply(CI_GEOMS, function(g) setequal(
  names(g), c("kind", "var_kind", "scale", "method_slot", "method_fixed",
              "scale_key", "wants_ref", "wants_p", "ref_cell", "engine")), logical(1))),
  identical(names(CI_GEOMS),
            vapply(CI_GEOMS, function(g) if (is.na(g$scale)) paste(g$kind, g$var_kind, sep = ".")
                   else paste(g$kind, g$var_kind, g$scale, sep = "."), character(1),
                   USE.NAMES = FALSE)))

# --- the four readers. Nothing else may index CI_GEOMS. -------------------------------------------

#' @keywords internal
#' @noRd
ci_geom <- function(kind, var_kind, ci_scale = "diff") {
  if (length(kind) == 0L || is.na(kind[1]) || identical(kind[1], "no")) return(NULL)
  key <- if (identical(kind[1], "cell")) paste("cell", var_kind[1], sep = ".")
         else paste("diff", var_kind[1], ci_scale[1], sep = ".")
  CI_GEOMS[[key]]
}

#' @keywords internal
#' @noRd
ci_geom_scale <- function(kind, var_kind, ci_scale = "diff") {
  g <- ci_geom(kind, var_kind, ci_scale)
  if (is.null(g)) NA_character_ else g$scale_key
}

#' @keywords internal
#' @noRd
ci_geom_method <- function(kind, var_kind, ci_scale = "diff", method = default_ci_method()) {
  g <- ci_geom(kind, var_kind, ci_scale)
  if (is.null(g)) return("")
  if (is.na(g$method_slot)) g$method_fixed else as.character(method[[g$method_slot]])
}

# THE rule, stated once: a CELL interval has no reference (it compares each cell to 0 % or to a mean of
# 0), so every cell keeps it, INCLUDING the total row; a CONTRAST interval has nothing to say about a
# row compared to itself, so that row is NA.
#' @keywords internal
#' @noRd
ci_geom_ref_cell <- function(kind, var_kind, ci_scale = "diff") {
  g <- ci_geom(kind, var_kind, ci_scale)
  if (is.null(g)) "na" else g$ref_cell
}

# DESIGN: the reference-cell MECHANISM belongs to the CALLER, never here -- the two callers NA different
#   things and are not equivalent on a mean cell. What is NOT the caller's is the DECISION, which is one
#   lookup: ci_geom_ref_cell(). Same for the total / reference COLUMN masking.
#' @keywords internal
#' @noRd
ci_dispatch <- function(kind, var_kind, ci_scale = "diff",
                        est, base, var = NULL,
                        ref = NULL, ref_var = NULL, ref_n = NULL, n_raw = NULL,
                        conf_level = 0.95, want_p = FALSE,
                        method = default_ci_method(), degf = Inf, pool = NULL) {
  g <- ci_geom(kind, var_kind, ci_scale)
  if (is.null(g)) cli::cli_abort(
    "No interval geometry for {.val {kind}} / {.val {var_kind}} / {.val {ci_scale}}.")
  g$engine(list(
    est = est, base = base, var = var, ref = ref, ref_var = ref_var, ref_n = ref_n, n_raw = n_raw,
    conf_level = conf_level, pool = pool,
    # a caller that forgets the kind test still gets FALSE where there is no null to test
    want_p = isTRUE(want_p) && isTRUE(g$wants_p),
    method = if (is.na(g$method_slot)) g$method_fixed else as.character(method[[g$method_slot]]),
    degf = degf))
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

wilson_bounds <- function(p, n, z) {
  d    <- 1 + z^2 / n
  ctr  <- (p + z^2 / (2 * n)) / d
  half <- (z / d) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  list(inf = ctr - half, sup = ctr + half)
}

ci_pivot <- function(estimate, se, df = Inf, conf_level = 0.95, want_p = TRUE) {
  # df <= 0 (a mean cell with n = 1) has no t interval -> NA, so qt/pt return NA rather than NaN.
  df     <- ifelse(df > 0, df, NA_real_)
  q      <- stats::qt(1 - (1 - conf_level) / 2, df)
  half   <- q * se
  pvalue <- if (want_p) 2 * stats::pt(-abs(estimate / se), df) else NA_real_
  pvalue <- vctrs::vec_recycle(pvalue, length(estimate))
  bad    <- !is.finite(se) | se == 0 | is.na(df)
  pvalue[bad] <- NA_real_
  list(inf = estimate - half, sup = estimate + half, pvalue = pvalue)
}

ci_wilson <- function(p, n, conf_level = 0.95, df = Inf) {
  b <- wilson_bounds(p, n, conf_level_to_crit(conf_level, df))
  list(inf = b$inf, sup = b$sup, pvalue = vctrs::vec_recycle(NA_real_, length(p)))
}

# WARNING: the Wald proportion interval degenerates at p in {0, 1} -- se = 0, hence zero width (Wilson
#   never does), and its bounds can fall outside [0, 1] (the display clamps them). Opt-in; not default.
ci_wald <- function(p, n, conf_level = 0.95, df = Inf) {
  ci_pivot(p, sqrt(p * (1 - p) / n), df = df, conf_level = conf_level, want_p = FALSE)
}

# KORN-GRAUBARD: a Clopper-Pearson interval on the EFFECTIVE sample size, i.e. exactly
# `survey::svyciprop(method = "beta")`. Opt-in, never a default.
# DESIGN: beta quantiles have no degrees of freedom of their own, so the design's are carried in by
#   shrinking the effective n by the ratio of the SRS critical value to the design's -- hence the second
#   sample size `n_raw`, the cell's own unweighted base. With no design (`df` = Inf) the factor is 1.
# WARNING: `df` is the WHOLE design's degf, captured once at the boundary, as for every interval here.
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

# The smallest alpha at which the interval excludes 0: monotone in z, so fixed vectorised bisection
# steps replace a per-cell root solver.
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

# The caller passes WEIGHTED proportions p1/p2 and UNWEIGHTED bases n1/n2, as for every engine here.
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

# Katz's log-RR interval: when the ratio is the measure the reader sees, it owns the stored interval
# (converting the DIFFERENCE bounds would hold the reference proportion fixed at its point estimate).
# Its dual is the log-RR Wald test, so bracket and stars stay exact duals, like every method here.
# WARNING: undefined at p1 = 0 (log 0) or p2 = 0 -> NA bounds and NA p, so an empty cell or an empty
#   reference is left uncoloured and unstarred rather than +/-Inf.
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

# A mean variance is always ESTIMATED, so this is a Student t at the METHOD's own df, never one flipped
# by the stars toggle. Three variance assumptions, from the narrowest scope to the widest:
#   welch   (default) each group's own variance, Welch-Satterthwaite df -- a two-sample test.
#   student the two groups POOLED, df = n1 + n2 - 2 -- the two-sample pooled t.
#   ols     the variance pooled over ALL the variable's levels, df = N - k -- the interval a LINEAR
#           MODEL puts on that coefficient. Needs `pool` (see CI_POOLED); without it, the pair.
ci_mean_diff2 <- function(m1, v1, n1, m2, v2, n2, conf_level = 0.95, want_p = TRUE,
                          method = "welch", df_design = Inf, pool = NULL) {
  if (identical(method, "ols") && !is.null(pool)) {
    se <- sqrt(pool$disp * (1 / n1 + 1 / n2))
    df <- pool$df
  } else if (method %in% c("student", "ols")) {
    sp2 <- ((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2)
    se  <- sqrt(sp2 * (1 / n1 + 1 / n2))
    df  <- n1 + n2 - 2
  } else {
    se <- sqrt(v1 / n1 + v2 / n2)
    df <- se^4 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))
  }
  ci_pivot(m1 - m2, se, df = df_or_design(df, df_design), conf_level = conf_level, want_p = want_p)
}

# ci_pool_disp() -- the ONE dispersion a model estimates for a whole variable, which the elementwise
# engines above cannot compute: they see a cell and its reference, never the level set.
#   mean_diff   s_p2 = SUM (n_g - 1) v_g          / (N - k)   the OLS residual variance
#   mean_ratio  phi  = SUM (n_g - 1) v_g / m_g    / (N - k)   the Pearson dispersion
# `by` groups the cells (one variable, one sub-table); `use` marks the rows that ARE levels of it --
# a total row is a mixture, not a level, and would be counted twice. Returns both quantities
# broadcast to every cell of the group, so the engines stay elementwise.
#' @keywords internal
#' @noRd
ci_pool_disp <- function(n, mean, var, by, use, kind) {
  n_r  <- length(n)
  disp <- rep(NA_real_, n_r); df <- rep(NA_real_, n_r)
  use  <- use & is.finite(n) & n > 1 & is.finite(var) &
    (if (identical(kind, "mean_ratio")) is.finite(mean) & mean > 0 else TRUE)
  if (!any(use)) return(list(disp = disp, df = df))
  num <- (n - 1) * var
  if (identical(kind, "mean_ratio")) num <- num / mean
  by  <- as.character(by)
  for (g in unique(by[use])) {
    i  <- use & by == g
    N  <- sum(n[i]); k <- sum(i)
    if (N - k <= 0) next
    disp[by == g] <- sum(num[i]) / (N - k)
    df[by == g]   <- N - k
  }
  list(disp = disp, df = df)
}

# A DESIGN's degrees of freedom REPLACE the sample-based ones: survey refers every interval, mean
# included, to t(#PSU - #strata), which no n_eff can stand in for. NA / Inf elsewhere -> df unchanged.
df_or_design <- function(df, df_design) {
  d <- as.double(df_design)
  if (!length(d) || anyNA(d) || !all(is.finite(d)) || any(d <= 0)) return(df)
  vctrs::vec_recycle(d, length(df))
}

# The ratio counterpart of ci_mean_diff2; dual = the log-ratio Wald/t test. Three variance assumptions,
# spanning the dispersion ladder a Poisson / quasi-Poisson regression walks and reproducing it:
#   robust       each group's OWN empirical variance (delta method on log = robust Poisson / GEE): z.
#   poisson      naive Var = mu, so se_logR = sqrt(1/S1 + 1/S2), S = m*n the group total count: z.
#   quasipoisson the Poisson se * sqrt(phi), phi the Pearson dispersion the quasi-Poisson MODEL
#                estimates -- ONE value for the whole variable, df = N - k. That is what `pool`
#                carries (see CI_POOLED); without it, the pair, which is the same thing at k = 2.
# WARNING: undefined at m <= 0 -> NA bounds and NA p (an empty group is left uncoloured, unstarred).
ci_mean_ratio <- function(m1, v1, n1, m2, v2, n2, conf_level = 0.95, want_p = TRUE,
                          method = "robust", df_design = Inf, pool = NULL) {
  lr    <- log(m1 / m2)
  qpool <- identical(method, "quasipoisson") && !is.null(pool)
  se <- switch(
    method,
    "robust"       = sqrt((v1 / n1) / m1^2 + (v2 / n2) / m2^2),
    "poisson"      = sqrt(1 / (m1 * n1) + 1 / (m2 * n2)),
    "quasipoisson" = {
      phi <- if (qpool) pool$disp
             else ((n1 - 1) * v1 / m1 + (n2 - 1) * v2 / m2) / (n1 + n2 - 2)
      sqrt(1 / (m1 * n1) + 1 / (m2 * n2)) * sqrt(phi)
    },
    stop("unknown method_mean_ratio: ", method)
  )
  df  <- if (qpool) pool$df else if (identical(method, "quasipoisson")) n1 + n2 - 2 else Inf
  res <- ci_pivot(lr, se, df = df_or_design(df, df_design), conf_level = conf_level,
                  want_p = want_p)
  inf <- exp(res$inf); sup <- exp(res$sup)
  bad <- !is.finite(lr) | !is.finite(se) | se == 0
  inf[bad] <- NA_real_; sup[bad] <- NA_real_; res$pvalue[bad] <- NA_real_
  list(inf = inf, sup = sup, pvalue = res$pvalue)
}

# Woolf's log-OR Wald interval on a 2x2, the crude-OR counterpart the empirical binomial column uses;
# a/b are the level's (positive, negative) counts, c/d the reference's. Its dual is the log-OR Wald test,
# so bracket and stars stay duals. WARNING: undefined when any cell is 0 -> NA bounds and NA p.
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
# Whole-table omnibus tests for EVERY (sub)table in ONE pass over the already-aggregated cells -- never
# a raw N-scan. Tables are tagged by `table_id` and tested by grouped ops, so the cost is independent
# of the NUMBER of tables.
#
# KEY CONSTRAINTS:
#   - These engines are AGNOSTIC to weighting: they compute on whatever counts they are handed, and
#     the CALLER decides. chi2_compute_test() (R/tab-chi2.R) hands them the weighted counts rescaled
#     to the raw n; on unweighted data that factor is exactly 1.
#   - Chi2 must match stats::chisq.test() DEFAULTS EXACTLY, including the Yates continuity
#     correction on 2x2 (test-calculations.R locks it) -- which holds because of the rescale above.
#   - Welch's F must match stats::oneway.test(var.equal = FALSE); classic F must match
#     oneway.test(var.equal = TRUE). The F takes weighted group means/variances with an unweighted
#     n -- on unweighted data it reduces to oneway.test, which the parity test pins.

# `o` is the UNWEIGHTED n for the p-value, the weighted wn for the contribution pass. Parity with
# chisq.test(): empty rows/cols are dropped before df and Yates; Yates uses the per-cell
# pmin(0.5, |o-e|), which on a genuine 2x2 equals its scalar min(0.5, |o-E|); df < 1 yields pvalue = NA.
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
  # The effect size reads the UNCORRECTED chi2 (the Cramer's V / phi convention); the p keeps Yates.
  DT[, contrib_unc := data.table::fifelse(ok, (o - e)^2 / e, 0)]

  tables <- DT[ok == TRUE, {
    nr_ <- data.table::uniqueN(row_id)
    nc_ <- data.table::uniqueN(col_id)
    df_ <- (nr_ - 1L) * (nc_ - 1L)
    st_ <- sum(contrib)
    n_  <- grandtot[1]
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

# Per-group summary statistics in: an UNWEIGHTED n with a WEIGHTED mean / var. Groups outside the F
# domain are dropped, and a table left with k < 2 groups yields NA.
agg_anova <- function(table_id, n, mean, var) {
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
      eta2  <- ssb / (ssb + ssw)
      list(k = k, statistic = Fw, df1 = df1w, df2 = df2w, pvalue = pw,
           statistic_classic = Fc, df1_classic = df1c, df2_classic = df2c,
           pvalue_classic = pc, n = N, effect_size = eta2)
    }
  }, by = table_id]
}

# Fisher's exact test for the SMALL factor tables where the chi2 is unreliable (`which_ids` bounds it).
# SIZE GUARD: an exact test is meaningful and feasible only on a SMALL sample -- a large table with one
#   rare category has a low expected count but a fine chi2, and the exact call would blow up FEXACT's
#   workspace. Past `max_cells` cells or `n_exact` observations (or on an error) it falls back to a
#   Monte-Carlo p, and `simulated` flags that, so the caller shows the EXACT p only, never a cap.
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
