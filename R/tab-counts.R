# PURPOSE: tab_counts() -- the "from-the-middle" constructor. Build a full tabxplor_tab (pct,
#          diff, CI, chi2, colors, totals) from ALREADY-AGGREGATED counts instead of microdata.
# ROLE: A public sibling of tab(). It normalises the supported input shapes (long tidy counts,
#       wide count matrix / data.frame, a table/xtabs/matrix object, frequencies + base N) to the
#       canonical count-aggregate and routes them through the SAME core as tab(): tab_plain()'s
#       `.fine` pre-aggregate entry (the scan-fusion path, locked byte-for-byte by
#       test-fuse-parity.R) + the shared finalize (tab_chi2 / tab_ci / tab_add_n_pct /
#       tab_pvalue_lines). No math is forked.
# KEY CONSTRAINTS:
#   - Require a real unweighted `n`; weighted input carries BOTH a real unweighted count and a
#     weighted count (weighted estimate + unweighted n -- decisions doc §14). Input whose counts
#     are not real (fractional / weighted-only) disables CI/chi2 with a warning.
#   - Feeding the same data as microdata vs as counts must give an IDENTICAL fmt table.
# See: CLAUDE.md > 1.4.0 roadmap > Phase 4; dev/tabxplor_1.4.0_decisions.md §20.

# === SECTION: helpers ================================================================

# Hamilton (largest-remainder) rounding: round `x` to integers summing EXACTLY to `target`
# (default round(sum(x))). Used to rebuild a row's integer counts from frequencies + base N so the
# reconstructed counts sum exactly to N (a well-formed contingency table for chi2).
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

# tab_counts_reshape() -- turn any supported input SHAPE into a canonical long tidy data frame plus
# the resolved character column roles. All shape detection lives here; downstream code sees only
# long tidy counts. Args row_var/col_var/tab_vars/counts/wt_counts/cols/base are QUOSURES.
#   - table / xtabs / matrix / array : melt via as.data.frame.table(); roles from dimnames (or the
#     user's row_var/col_var/tab_vars overrides).
#   - data.frame + `cols` (tidyselect of the level columns) : `input = "counts"` pivots to long
#     counts; `input = "pct"` rebuilds counts as largest_remainder(freq x base) per row.
#   - data.frame + `counts` (the default) : already long tidy.
tab_counts_reshape <- function(data, row_var, col_var, tab_vars, counts, wt_counts,
                               cols, base, col_name, input) {

  # ---- a table / xtabs / matrix / array object ----
  if (inherits(data, c("table", "xtabs")) || is.matrix(data) || is.array(data)) {
    # A bare matrix/array melts via as.data.frame.matrix (wrong) -- coerce to a table first so
    # as.data.frame.table() gives the long [dim1, dim2, ..., .Freq] shape.
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

  # ---- a wide data.frame (cols = the col_var level columns) : wide counts or frequencies ----
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
      # Rebuild each row's integer counts so they sum exactly to its base N (largest-remainder).
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

  # ---- long tidy counts (the default shape) ----
  if (quo_miss_na_null_empty_no(counts))
    cli::cli_abort("For long counts, {.arg counts} must name the column of counts.")
  if (quo_miss_na_null_empty_no(row_var) || quo_miss_na_null_empty_no(col_var))
    cli::cli_abort("{.arg row_var} and {.arg col_var} must be provided.")
  rv     <- rlang::as_name(row_var)
  cv     <- rlang::as_name(col_var)
  tv     <- if (quo_miss_na_null_empty_no(tab_vars)) character()
            else names(tidyselect::eval_select(tab_vars, data))
  n_col  <- rlang::as_name(counts)
  wn_col <- if (quo_miss_na_null_empty_no(wt_counts)) NULL else rlang::as_name(wt_counts)
  list(data = data, row_var = rv, col_var = cv, tab_vars = tv, n_col = n_col, wn_col = wn_col)
}


# tab_counts_normalize() -- aggregate a long tidy data frame into the canonical count-aggregate: a
# keyed data.table `[tab_cols..., row_col, col_col, n, (wn)]` (the exact `.fine` shape tab_plain
# rolls up). `n` is the real UNWEIGHTED count (integer); `wn` the weighted count (double) when
# weighted. Duplicate keys are summed. `has_real_n` is FALSE when the supplied counts are not whole
# numbers (fractional / weighted-only) -- the boundary that disables CI/chi2.
tab_counts_normalize <- function(data, row_col, col_col, tab_cols, n_col, wn_col) {
  keys <- c(tab_cols, row_col, col_col)
  miss <- setdiff(c(keys, n_col, wn_col), names(data))
  if (length(miss) > 0)
    cli::cli_abort("Column{?s} {.field {miss}} not found in {.arg data}.")

  raw_n      <- suppressWarnings(as.numeric(data[[n_col]]))
  has_real_n <- all(is.na(raw_n) | abs(raw_n - round(raw_n)) < 1e-8)

  d <- data.table::as.data.table(data)
  # Byte-identity hotspot: keys must be factors with the SAME level order a microdata table would
  # use -- keep existing factor levels, else first-appearance order (matches tab_plain's L2399).
  for (k in keys) if (!is.factor(d[[k]]))
    data.table::set(d, j = k, value = forcats::as_factor(d[[k]]))

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

  # Match microdata tab() structurally: its aggregate (`.N` per observed key) NEVER contains a
  # zero-count cell -- the empty cells of the cross-table are recreated by dcast(fill = 0). So drop
  # explicit zero cells here (they are surfaced by table()/pivot_wider() for unused factor levels
  # and empty tab_var x row_var combinations, but never by microdata). This makes the `.fine`
  # byte-identical to the one tab_plain() would build from the underlying microdata.
  fine <- fine[fine$n > 0]

  list(fine = fine, weighted = weighted, has_real_n = has_real_n)
}


# === SECTION: public constructor =====================================================

#' Cross-table from already-aggregated counts ("from the middle")
#'
#' @description
#' `tab_counts()` builds the same color-coded cross-table as [tab()], but from data that is
#' **already cross-tabulated** (a table of counts) rather than from microdata (one row per
#' individual). This is the common case when you start from a `dplyr::count()` result, a
#' contingency table, or a published table of counts or percentages. All the usual calculations
#' --- percentages, differences, confidence intervals, chi-squared, colors, totals --- are done on
#' the counts, and the result is identical to the table [tab()] would build from the underlying
#' microdata.
#'
#' It accepts four input shapes:
#'
#' * **Long tidy counts** (the default): one row per `row_var` \eqn{\times} `col_var` (\eqn{\times}
#'   `tab_vars`) combination, with the count in `counts` (and, weighted, the weighted count in
#'   `wt_counts`).
#' * **A wide `data.frame`**: a label (`row_var`) column plus one column per `col_var` level ---
#'   select those level columns with `cols` and name the column variable with `col_name`.
#' * **A `table` / `xtabs` / `matrix` object**: melted automatically; the row/column variables are
#'   read from the dimnames (or set with `row_var` / `col_var`).
#' * **Frequencies + base N**: as the wide shape, plus `input = "pct"` and `base` (the column of
#'   row sample sizes); the integer counts are rebuilt from the percentages and the base.
#'
#' For weighted data, supply the real (unweighted) count in `counts` **and** the weighted count in
#' `wt_counts`: estimates use the weighted counts while confidence intervals and tests use the real
#' unweighted sample size. When the counts are not real whole numbers (a base-less / weighted-only
#' input), confidence intervals and chi-squared are disabled with a message.
#'
#' @param data A data frame of counts, or a `table` / `xtabs` / `matrix` object.
#' @param row_var The row variable (one level per line). For a `table` object it defaults to the
#'   first dimension.
#' @param col_var The column variable (one column per level). For a `table` object it defaults to
#'   the second dimension. Not used with `cols`.
#' @param tab_vars <[`tidy-select`][tidyr::tidyr_tidy_select]> Tab variables: a subtable is made for
#'   each combination of their levels.
#' @param counts The column holding the **unweighted** count for each cell (long tidy shape).
#' @param wt_counts Optional column holding the **weighted** count for each cell. Leave empty for an
#'   unweighted table.
#' @param cols <[`tidy-select`][tidyr::tidyr_tidy_select]> For a wide `data.frame`: the columns
#'   holding the `col_var` levels.
#' @param col_name Name of the (synthesised) column variable when `cols` is used.
#' @param base For `input = "pct"`: the column holding each row's sample size N.
#' @param input `"counts"` (default) or `"pct"` (with `cols` and `base`: the level columns hold
#'   frequencies, and counts are rebuilt from them and `base`).
#' @param pct,color,OR,chi2,na,ref,ref2,comp,ci,conf_level,stars,method_cell,method_diff,totaltab,totaltab_name,tot,total_names,add_n,add_pct,subtext,digits
#'   Same meaning as in [tab()].
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
                       cols, col_name = "variable", base, input = c("counts", "pct"),
                       pct = "no", color = "no", OR = "no", chi2 = FALSE,
                       na = "keep",
                       ref = "auto", ref2 = "first", comp = "tab",
                       ci = "no", conf_level = 0.95, stars = NULL,
                       method_cell = "wilson", method_diff = "newcombe",
                       totaltab = "line", totaltab_name = "Ensemble",
                       tot = c("row", "col"), total_names = "Total",
                       add_n = TRUE, add_pct = FALSE,
                       subtext = "", digits = 0) {

  input <- rlang::arg_match(input)
  vctrs::vec_assert(pct, size = 1); vctrs::vec_assert(color, size = 1)
  vctrs::vec_assert(ref, size = 1); vctrs::vec_assert(na, size = 1)
  # Phase 6g (S3): na = "common_base" is microdata-only -- it fixes the population from who is
  # NA on the row_var/first col_var, which pre-aggregated counts cannot reconstruct.
  if (identical(na, "common_base")) {
    cli::cli_abort(c(
      "{.code na = \"common_base\"} is only available in {.fn tab} (from microdata).",
      "i" = "Pre-aggregated counts cannot reconstruct who was missing; use {.val keep} or {.val drop}."
    ))
  }
  stopifnot(na %in% c("keep", "drop"))
  stopifnot(all(tot %in% c("row", "col", "both", "no", "")))
  if (tot[1] == "both") tot <- c("row", "col")
  total_names <- vctrs::vec_recycle(total_names, 2)

  # -- resolve the input SHAPE to canonical long tidy counts, then to the aggregate (the one
  #    validation boundary) --
  resh <- tab_counts_reshape(
    data,
    row_var   = rlang::enquo(row_var),  col_var   = rlang::enquo(col_var),
    tab_vars  = rlang::enquo(tab_vars), counts    = rlang::enquo(counts),
    wt_counts = rlang::enquo(wt_counts), cols     = rlang::enquo(cols),
    base      = rlang::enquo(base),     col_name  = col_name, input = input)

  norm       <- tab_counts_normalize(resh$data, resh$row_var, resh$col_var, resh$tab_vars,
                                      resh$n_col, resh$wn_col)
  fine       <- norm$fine
  weighted   <- norm$weighted
  has_real_n <- norm$has_real_n
  row_var    <- rlang::sym(resh$row_var)
  col_var    <- rlang::sym(resh$col_var)
  tab_vars   <- resh$tab_vars

  # Base-less input: no real unweighted n -> inference is not defined; keep pct/diff/colors.
  if (!has_real_n && (!identical(ci, "no") || !isFALSE(chi2))) {
    cli::cli_warn(c(
      "!" = "The counts are not whole numbers (weighted or frequency-only): confidence intervals and chi-square are disabled.",
      "i" = "Provide real unweighted counts in {.arg counts} (with the weighted counts in {.arg wt_counts}) to enable them."
    ))
    ci <- "no"; chi2 <- FALSE
  }

  # -- resolve colour: Phase 7b routes tab_counts() through the SAME pure resolver as tab_build()
  #    (tab_resolve_settings(), R/tab-resolve.R) -- auto-resolution, the contrib/diff-family
  #    forcing, and the split into the diff/OR colour (-> tab_plain), the contrib colour
  #    (-> tab_chi2) and the ci colour (-> tab_ci). This is a single row_var x single factor
  #    col_var, so the row-axis inputs are length 1, pct_vect is list(pct) and col_vars_text is
  #    TRUE. totrow = NULL: this constructor drives total rows through its own `tot`, so the
  #    contrib -> totrow forcing is skipped (unchanged from before). --
  .settings     <- tab_resolve_settings(color = color, OR = OR, ci = ci, chi2 = chi2,
                                         ref = ref, pct_vect = list(pct),
                                         col_vars_text = TRUE, totrow = NULL)
  color         <- .settings$color
  chi2          <- .settings$chi2
  ci            <- .settings$ci
  color_diff_OR <- .settings$color_diff_OR
  color_ctr     <- .settings$color_ctr
  color_ci      <- .settings$color_ci

  # -- base table via tab_plain()'s pre-aggregate (`.fine`) entry. `data_skel` only serves the
  #    tidy-select of tab_vars; the aggregation reads `.fine`. `wt` is a weighted/unweighted flag
  #    (never evaluated as a column on the `.fine` path). --
  data_skel <- as.data.frame(fine)
  wt_pass   <- if (weighted) rlang::sym("wn") else character()

  tabs <- tab_plain(
    data_skel, !!row_var, !!col_var, tidyselect::all_of(tab_vars),
    wt            = !!wt_pass,
    na            = na,
    digits        = digits,
    pct           = pct,
    ref           = ref,
    ref2          = ref2,
    comp          = comp,
    OR            = OR,
    color         = color_diff_OR,
    totaltab      = totaltab,
    totaltab_name = totaltab_name,
    tot           = tot,
    total_names   = total_names,
    .fine         = fine
  )

  # -- finalize: the SAME steps tab_many() applies after tab_plain (chi2 -> ci -> add_n ->
  #    rewrap with the test attribute -> p-value lines). Single row_var x col_var, so no
  #    multi-table join / level-drop / totcol-cleanup is needed. The chi2 -> capture test ->
  #    ci block is the shared tab_apply_tests() helper (Phase 6a). --
  applied <- tab_apply_tests(tabs, do_chi2 = !isFALSE(chi2), ci = ci, comp = comp,
                             color_ctr = color_ctr, color_ci = color_ci,
                             conf_level = conf_level, stars = stars,
                             method_cell = method_cell, method_diff = method_diff)
  tabs <- applied$tab
  test <- applied$test

  tabs <- tab_add_n_pct(list(tabs), add_n, add_pct)[[1]]

  # Remove unwanted total rows (keep the total-table line if present) when tot excludes "row".
  if (!"row" %in% tot) {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows
    tabs <- tabs |>
      tibble::add_column(.totrows = totrows, .tottab_line = tottab_line) |>
      dplyr::filter(!.data$.totrows | .data$.tottab_line) |>
      dplyr::select(-".totrows", -".tottab_line")
  }

  # Rewrap so the whole-table `test` attribute survives, choosing plain vs grouped like tab_many().
  if (!lv1_group_vars(tabs)) {
    tabs   <- tabs |> dplyr::group_by(!!!rlang::syms(tab_vars))
    groups <- dplyr::group_data(tabs)
    tabs   <- new_grouped_tab(tabs, groups = groups, subtext = subtext, test = test)
  } else {
    tabs <- new_tab(tabs, subtext = subtext, test = test)
  }

  tab_pvalue_lines(tabs)
}
