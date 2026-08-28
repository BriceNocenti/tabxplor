# PURPOSE: tab_counts() -- the "from-the-middle" constructor: a full tabxplor_tab built from
#   ALREADY-AGGREGATED counts instead of microdata.
# ROLE: A public sibling of tab(), and the thinnest wrapper it can be -- "tab() with the first
#   steps already done". It normalises the supported input shapes (long tidy counts, a wide count
#   matrix or data.frame, a table/xtabs object, frequencies + base N) into the canonical count
#   aggregate, then routes them through the SAME core, argument boundary and colour tail as tab().
#   No math is forked. What stays local is only what is true of THIS producer: the survey-design
#   refusal, the microdata-only `na` refusal, and the inert mean `ci_method` slots.
# KEY CONSTRAINTS:
#   - A real UNWEIGHTED `n` is required. Weighted input carries both a real unweighted count and a
#     weighted one (weighted estimate, unweighted base -- the package's rule everywhere). Counts
#     that are not real (fractional, weighted-only) disable CI and chi2, with a warning.
#   - Feeding the same data as microdata and as counts must give an IDENTICAL fmt table. This is
#     the file's whole contract; test-fuse-parity.R locks it byte-for-byte.
#   - It starts PAST the microdata preparation, so the tab() arguments resolved there are not
#     offered: level selection (levels = "first"/"auto"), rare-level lumping, na = "drop_all" /
#     "common_base", and survey designs. `cleannames` is the exception -- a pure relabel of the
#     aggregate keys, which commutes with the count sum.
# See: CLAUDE.md § tabxplor architecture (how a table is built).
# === SECTION: helpers ================================================================

# DESIGN: largest-remainder (Hamilton) rounding -- the parts sum EXACTLY to `target`, so counts
#   rebuilt from frequencies + base N still form a well-formed contingency table for chi2.
largest_remainder <- function(x, target = round(sum(x, na.rm = TRUE))) {
  x[is.na(x)] <- 0
  fl <- floor(x)
  k  <- as.integer(round(target - sum(fl)))
  if (k > 0L) {
    ord <- order(x - fl, decreasing = TRUE)
    fl[ord[seq_len(k)]] <- fl[ord[seq_len(k)]] + 1
  } else if (k < 0L) {
    ord <- order(x - fl, decreasing = FALSE)
    fl[ord[seq_len(-k)]] <- fl[ord[seq_len(-k)]] - 1
  }
  as.integer(fl)
}


# === SECTION: input reshaping ========================================================

# All shape detection lives here: any supported input -> long tidy counts + the character column
# roles. row_var/col_var/tab_vars/counts/wt_counts/cols/base arrive as QUOSURES.
tab_counts_reshape <- function(data, row_var, col_var, tab_vars, counts, wt_counts,
                               cols, base, col_name, input) {

  if (inherits(data, c("table", "xtabs")) || is.matrix(data) || is.array(data)) {
    # WARNING: a bare matrix/array melts through as.data.frame.matrix (wrong) -- coerce to a table
    #   first, so as.data.frame.table() gives the long [dim1, dim2, ..., .Freq] shape.
    if (!inherits(data, "table")) data <- as.table(data)
    df      <- as.data.frame(data, responseName = ".Freq", stringsAsFactors = TRUE)
    dimvars <- setdiff(names(df), ".Freq")
    if (length(dimvars) < 2)
      cli::cli_abort("A {.cls table}/{.cls matrix} input needs at least 2 dimensions (rows x columns).")
    rv <- if (quo_miss_na_null_empty_no(row_var)) dimvars[1] else rlang::as_name(row_var)
    cv <- if (quo_miss_na_null_empty_no(col_var)) dimvars[2] else rlang::as_name(col_var)
    tv <- if (quo_miss_na_null_empty_no(tab_vars)) setdiff(dimvars, c(rv, cv))
          else names(tidyselect::eval_select(tab_vars, df))
    return(list(data = df, row_var = rv, col_var = cv, tab_vars = tv,
                n_col = ".Freq", wn_col = NULL))
  }

  if (!quo_miss_na_null_empty_no(cols)) {
    if (quo_miss_na_null_empty_no(row_var))
      cli::cli_abort("With {.arg cols}, {.arg row_var} must name the row (label) column.")
    level_cols <- names(tidyselect::eval_select(cols, data))
    rv <- rlang::as_name(row_var)
    tv <- if (quo_miss_na_null_empty_no(tab_vars)) character()
          else names(tidyselect::eval_select(tab_vars, data))

    if (input == "pct") {
      if (quo_miss_na_null_empty_no(base))
        cli::cli_abort("With {.code input = \"pct\"}, {.arg base} must name the column of row sample sizes (N).")
      base_col <- rlang::as_name(base)
      long <- tidyr::pivot_longer(data, tidyselect::all_of(level_cols),
                                  names_to = col_name, values_to = ".pct")
      long <- long |>
        dplyr::group_by(dplyr::across(tidyselect::all_of(c(tv, rv)))) |>
        dplyr::mutate(.n = largest_remainder(
          .data$.pct / sum(.data$.pct) * dplyr::first(.data[[base_col]]))) |>
        dplyr::ungroup() |>
        dplyr::select(-tidyselect::all_of(c(".pct", base_col)))
      n_col <- ".n"
    } else {
      long <- tidyr::pivot_longer(data, tidyselect::all_of(level_cols),
                                  names_to = col_name, values_to = ".n")
      n_col <- ".n"
    }
    long[[col_name]] <- forcats::fct_inorder(as.character(long[[col_name]]))  # keep `cols` order
    return(list(data = long, row_var = rv, col_var = col_name, tab_vars = tv,
                n_col = n_col, wn_col = NULL))
  }

  if (quo_miss_na_null_empty_no(counts))
    cli::cli_abort("For long counts, {.arg counts} must name the column of counts.")
  if (quo_miss_na_null_empty_no(row_var) || quo_miss_na_null_empty_no(col_var))
    cli::cli_abort("{.arg row_var} and {.arg col_var} must be provided.")
  # ⚠ AN INTERACTION HAS NO PARENTS HERE. tab_counts() starts from aggregated counts, so the two
  # columns a cross would combine are gone; a pair must be crossed while the microdata still exists.
  for (q in list(row_var, col_var, tab_vars)) {
    if (quo_miss_na_null_empty_no(q)) next
    e <- rlang::quo_get_expr(q)
    if (reg_cross_is_term(e) || (is.character(e) && any(reg_cross_has_op(e))))
      cli::cli_abort(c(
        "{.fn tab_counts} takes no interaction: it starts from counts already aggregated.",
        "i" = "Cross the pair in {.fn tab}: {.code tab(data, rows, a*b)}."), call = NULL)
  }
  rv     <- rlang::as_name(row_var)
  cv     <- rlang::as_name(col_var)
  tv     <- if (quo_miss_na_null_empty_no(tab_vars)) character()
            else names(tidyselect::eval_select(tab_vars, data))
  n_col  <- rlang::as_name(counts)
  wn_col <- if (quo_miss_na_null_empty_no(wt_counts)) NULL else rlang::as_name(wt_counts)
  list(data = data, row_var = rv, col_var = cv, tab_vars = tv, n_col = n_col, wn_col = wn_col)
}


# Aggregates long tidy counts into the canonical `.fine` aggregate tab_plain rolls up (duplicate
# keys summed); `has_real_n = FALSE` on fractional / weighted-only counts disables CI and chi2.
# DESIGN: `cleannames` strips the keys HERE, pre-aggregate, through the microdata path's own
#   tab_cleannames_relabel() -- a relabel commutes with the count sum (relabel-then-sum ==
#   tab()'s sum-then-relabel): that is why it is the one microdata-prep argument offered.
tab_counts_normalize <- function(data, row_col, col_col, tab_cols, n_col, wn_col,
                                 cleannames = FALSE) {
  keys <- c(tab_cols, row_col, col_col)
  miss <- setdiff(c(keys, n_col, wn_col), names(data))
  if (length(miss) > 0)
    cli::cli_abort("Column{?s} {.field {miss}} not found in {.arg data}.")

  data <- data |> tab_apply_val_labels(keys)

  raw_n      <- suppressWarnings(as.numeric(data[[n_col]]))
  has_real_n <- all(is.na(raw_n) | abs(raw_n - round(raw_n)) < 1e-8)

  d <- data.table::as.data.table(data)
  # WARNING: byte-identity hotspot -- the keys must be factors with the SAME level order a microdata
  #   table would use: keep existing factor levels, else first-appearance order.
  for (k in keys) if (!is.factor(d[[k]]))
    data.table::set(d, j = k, value = forcats::as_factor(d[[k]]))

  if (isTRUE(cleannames)) d <- data.table::as.data.table(tab_cleannames_relabel(d, keys))

  if (is.null(wn_col)) {
    fine <- d[, list(n = as.integer(round(sum(as.numeric(get(n_col)), na.rm = TRUE)))),
              keyby = keys]
    weighted <- FALSE
  } else {
    fine <- d[, list(n  = as.integer(round(sum(as.numeric(get(n_col)), na.rm = TRUE))),
                     wn =            sum(as.numeric(get(wn_col)), na.rm = TRUE)),
              keyby = keys]
    weighted <- TRUE
  }

  # DESIGN: microdata's per-observed-key aggregate NEVER holds a zero cell (dcast(fill = 0) recreates
  #   the empty ones), so dropping the zeros table()/pivot_wider() surface keeps `.fine` byte-identical.
  fine <- fine[fine$n > 0]

  list(fine = fine, weighted = weighted, has_real_n = has_real_n)
}


# === SECTION: public constructor =====================================================

#' Cross-tables from already-aggregated counts
#'
#' @description
#' `tab_counts()` builds the same color-coded cross-table as [tab()], but from data that is
#' **already cross-tabulated** --- a `dplyr::count()` result, a contingency table, or a published
#' table of counts or percentages --- instead of microdata (one row per individual). Percentages,
#' differences, confidence intervals, chi-squared, colors and totals are all computed from the
#' counts, and the result is identical to the table [tab()] would build from the microdata behind
#' them.
#'
#' It accepts four input shapes:
#'
#' * **Long tidy counts** (the default): one row per `row_var` \eqn{\times} `col_var` (\eqn{\times}
#'   `tab_vars`) combination, with the count in `counts` (and the weighted count in `wt_counts`).
#' * **A wide `data.frame`**: a label (`row_var`) column plus one column per `col_var` level ---
#'   select those level columns with `cols` and name the column variable with `col_name`.
#' * **A `table` / `xtabs` / `matrix` object**: melted automatically, the row and column variables
#'   read from the dimnames (or set with `row_var` / `col_var`).
#' * **Frequencies + base N**: the wide shape, plus `input = "pct"` and `base` (the column of row
#'   sample sizes); the integer counts are rebuilt from the percentages and the base.
#'
#' With weighted data, give the real (unweighted) count in `counts` **and** the weighted count in
#' `wt_counts`: estimates use the weighted counts, while confidence intervals and tests use the
#' real unweighted sample size. Counts that are not whole numbers (weighted-only or frequency-only
#' input) disable confidence intervals and chi-squared, with a message.
#'
#' @eval tab_args_rd("tab_counts")
#' @param ... Every other argument of [tab()] -- `pct`, `color`, `ci`, `tot`, ... -- passed
#'   by name. See [tab()]; a typo gets a suggestion.
#'
#' @return A `tabxplor_tab` (or `tabxplor_grouped_tab` when `tab_vars` are provided).
#' @export
#'
#' @examples
#' # Long tidy counts (as from dplyr::count()) reproduce the microdata table :
#' counts <- dplyr::count(forcats::gss_cat, marital, race)
#' tab_counts(counts, marital, race, counts = n, pct = "row")
#' # tab(forcats::gss_cat, marital, race, pct = "row")   # identical
#'
#' # A contingency table object :
#' tab_counts(table(forcats::gss_cat$marital, forcats::gss_cat$race), pct = "row")
#'
#' # A wide data.frame of counts :
#' wide <- tidyr::pivot_wider(counts, names_from = race, values_from = n)
#' tab_counts(wide, row_var = marital, cols = c(Other, Black, White),
#'            col_name = "race", pct = "row")
tab_counts <- function(data, row_var, col_var, tab_vars, counts, wt_counts,
                       cols, col_name = "variable", base, input = c("counts", "pct"), ...) {

  .d <- rlang::list2(...)
  tab_check_dots(.d, "tab_counts")
  list2env(tab_dots_expand(.d, "tab_counts"), environment())

  # DESIGN: the ONE entry point that REFUSES a survey design instead of unwrapping it -- a design's
  #   weights and structure are per-observation facts a count table cannot carry.
  if (svy_is_design(data))
    cli::cli_abort(c(
      "{.fn tab_counts} works on pre-aggregated counts; a survey design carries microdata.",
      "i" = "Pass the design to {.fn tab} instead, or give the weighted counts in {.arg wt_counts}."
    ))
  # DESIGN: refused BEFORE the shared argument boundary so the message can say WHY -- counts cannot
  #   reconstruct who was missing; the shared one would only say the word is not in the vocabulary.
  if (identical(na, "common_base") || identical(na, "drop_all")) {
    cli::cli_abort(c(
      "{.code na = {na}} is only available in {.fn tab} (from microdata).",
      "i" = "Pre-aggregated counts cannot reconstruct who was missing; use {.val keep} or {.val drop}."
    ))
  }
  .a <- tab_resolve_common_args(
    "tab_counts", test = test, chi2 = chi2, color = color, color_signif = color_signif,
    ci = ci, stars = stars, conf_level = conf_level, ci_method = ci_method,
    cleannames = cleannames, OR = OR, display = display, ref = ref, ref2 = ref2,
    tot = tot, na = na, pct = pct, comp = comp,
    total_names   = .d$total_names,
    totaltab_name = .d$totaltab_name,
    other_level   = .d$other_level,
    totaltab = totaltab, n_min = n_min, n = n, add_n = add_n,
    user_env = rlang::caller_env())
  test <- .a$test ; cleannames <- .a$cleannames ; stars <- .a$stars
  display <- .a$display ; ref <- .a$ref ; ref2 <- .a$ref2
  color_spec <- .a$color_spec ; total_names <- .a$total_names
  conf_level <- .a$conf_level ; totaltab_name <- .a$totaltab_name ; base_n <- .a$base_n
  counts_refuse_mean_methods(ci_method)
  ci_method <- .a$ci_method

  input <- rlang::arg_match(input)

  resh <- tab_counts_reshape(
    data,
    row_var   = rlang::enquo(row_var),  col_var   = rlang::enquo(col_var),
    tab_vars  = rlang::enquo(tab_vars), counts    = rlang::enquo(counts),
    wt_counts = rlang::enquo(wt_counts), cols     = rlang::enquo(cols),
    base      = rlang::enquo(base),     col_name  = col_name, input = input)

  # The same two rules as tab(): a spread variable IS a tab variable -- it splits the population and
  # merely shows the split across the page -- and a total LINE cannot become a column block. Applied
  # BEFORE the normalisation, which aggregates away every column no role claims.
  if (is.character(spread_vars) && length(spread_vars)) {
    resh$tab_vars <- c(resh$tab_vars, setdiff(spread_vars, resh$tab_vars))
    if (any(totaltab == "line")) {
      tx_inform_once("spread_totline", c("i" = paste(
        "A total line cannot become a column block: a full total table was added.",
        'Use {.code totaltab = "no"} for no overall column.')))
      totaltab[totaltab == "line"] <- "table"
    }
  }

  norm       <- tab_counts_normalize(resh$data, resh$row_var, resh$col_var, resh$tab_vars,
                                      resh$n_col, resh$wn_col, cleannames = cleannames)
  fine       <- norm$fine
  weighted   <- norm$weighted
  has_real_n <- norm$has_real_n
  row_var    <- rlang::sym(resh$row_var)
  col_var    <- rlang::sym(resh$col_var)
  tab_vars   <- resh$tab_vars

  # No real unweighted n -> inference is undefined; percentages, differences and colors stay.
  if (!has_real_n && (!identical(ci, "no") || !isFALSE(test))) {
    cli::cli_warn(c(
      "!" = "The counts are not whole numbers (weighted or frequency-only): confidence intervals and the test are disabled.",
      "i" = "Provide real unweighted counts in {.arg counts} (with the weighted counts in {.arg wt_counts}) to enable them."
    ))
    ci <- "no"; test <- FALSE
  }

  # DESIGN: already holding its aggregate, tab_counts() bypasses tab_prepare_pop()/tab_aggregate()
  #   but runs the SAME tab_setup() + tab_build_tables(), so the table stays byte-identical to tab().
  data_skel <- as.data.frame(fine)

  totrow <- .a$totrow
  totcol <- .a$totcol

  ctx <- new_ctx(
    data = data_skel,   # (no `filter` here -- the ctx default NA_character_ says so)
    row_vars_quo = rlang::quo(!!row_var), col_vars_quo = rlang::quo(!!col_var),
    tab_vars_quo = if (length(tab_vars) == 0) rlang::quo(NULL)
                   else rlang::quo(c(!!!rlang::syms(tab_vars))),
    wt_quo = if (weighted) rlang::quo(wn) else rlang::quo(NULL),
    na_drop_all_quo = rlang::quo(NULL),
    pct = pct, color = color_spec$legacy, color_signif = color_spec$signif,
    color_ratio_ci = color_pct_text_is_ratio(color_spec),
    display = display, chi2 = test,
    na = na, levels = "all",
    cleannames = cleannames, output = "single",
    ref = ref, ref2 = ref2, comp = comp, ci = ci, conf_level = conf_level, stars = stars,
    ci_method = ci_method,
    totaltab = totaltab, totaltab_name = totaltab_name, totrow = totrow, totcol = totcol,
    total_names = total_names, base_n = base_n, add_pct = add_pct, common_totrow = common_totrow,
    digits = digits, n_min = n_min, subtext = subtext, by_table = FALSE,
    # DESIGN: pre-aggregated counts carry no per-observation Sum(w^2), so the inference basis is
    #   "n" -- declared here rather than discovered in the leaf.
    agg_only = TRUE,
    spread_vars = spread_vars, names_prefix = names_prefix, names_sort = names_sort
  )

  ctx <- tab_setup(ctx)

  # DESIGN: levels = "first"/"auto" would need `remove_levels` from the bypassed tab_prepare_pop(),
  #   so every level is kept; the population/level metadata goes onto the settings spine instead.
  ctx$settings$cols$lvs    <- rep("all", nrow(ctx$settings$cols))
  ctx$settings$cols$lv1    <- FALSE
  ctx$settings$rows$na_num <- ctx$na
  ctx$settings$pairs$na    <- rep(ctx$na, nrow(ctx$settings$pairs))
  ctx <- ctx_update(ctx, list(
    remove_levels = NULL,
    fine_num = NULL, fine_fused = fine
  ))

  result <- tab_build_tables(ctx)

  finalize_color_tail(result, color_spec, color_breaks, display)
}


# `ci_method`'s two MEAN slots have nothing to act on here (a counts table has no mean column), so
# setting one is a refusal rather than a silent no-op; the argument stays one named vector everywhere.
#' @keywords internal
#' @noRd
counts_refuse_mean_methods <- function(ci_method) {
  if (is.null(ci_method) || is.null(names(ci_method))) return(invisible(NULL))
  hit <- intersect(c("mean_diff", "mean_ratio"), names(ci_method))
  if (length(hit) == 0L) return(invisible(NULL))
  cli::cli_abort(c(
    "{.code ci_method = c({hit[[1]]} = )} has no effect in {.fn tab_counts}: a counts table has no
     mean columns.",
    "i" = "The slots that apply here are {.val cell} and {.val diff}."
  ))
}

# codetools: tab_counts() binds every argument riding on `...` as a local, via
# list2env(tab_dots_expand(.d, "tab_counts"), environment()) -- correct at run time, invisible to the
# code walker. DERIVED from the declaration, so a new or retired argument needs no edit here.
utils::globalVariables(tab_args_for("tab_counts"))
