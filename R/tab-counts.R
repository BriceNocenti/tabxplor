# PURPOSE: tab_counts() -- the "from-the-middle" constructor. Build a full tabxplor_tab (pct,
#          diff, CI, chi2, colors, totals) from ALREADY-AGGREGATED counts instead of microdata.
# ROLE: A public sibling of tab(), and the THINNEST wrapper it can be -- "tab() with the first steps
#       already done". It normalises the supported input shapes (long tidy counts, wide count matrix /
#       data.frame, a table/xtabs/matrix object, frequencies + base N) to the canonical count-aggregate
#       and routes them through the SAME core as tab(): tab_plain()'s `.fine` pre-aggregate entry (the
#       scan-fusion path, locked byte-for-byte by test-fuse-parity.R) + the shared finalize (tab_chi2 /
#       tab_ci / tab_add_n_pct / tab_pvalue_lines). It ALSO shares tab()'s colour boundary
#       (normalize_color_spec at the front) and its finalize_color_tail() at the back, so every modern
#       colour form (TRUE / two-channel / per-type / color_signif / ratio), `display` and a per-table
#       `color_breaks` behave EXACTLY as in tab(). No math is forked.
# KEY CONSTRAINTS:
#   - Require a real unweighted `n`; weighted input carries BOTH a real unweighted count and a
#     weighted count (weighted estimate + unweighted n -- decisions doc §14). Input whose counts
#     are not real (fractional / weighted-only) disables CI/chi2 with a warning.
#   - Feeding the same data as microdata vs as counts must give an IDENTICAL fmt table.
#   - It starts PAST the microdata prep (tab_prepare_pop), so the tab() arguments resolved there are
#     not offered: level selection (levels = "first"/"auto"), rare-level lumping (other_if_less_than),
#     na = "drop_all"/"common_base", survey design. cleannames is the exception -- a pure relabel run
#     on the aggregate keys (tab_counts_normalize), byte-identical to tab()'s pre-aggregate strip.
# See: CLAUDE.md > 2.0.0 roadmap > Phase 4; dev/tabxplor_2.0.0_decisions.md §20.

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
# numbers (fractional / weighted-only) -- the boundary that disables CI/chi2. `cleannames` (Phase p)
# strips the cleannames regex off the key levels HERE (pre-aggregate) via the SAME
# tab_cleannames_relabel() the microdata path runs in tab_prepare(): a relabel commutes with the count
# sum (relabel-then-sum == tab()'s sum-then-relabel) and the keyby re-aggregation merges any collapsed
# level -- byte-identical to tab(cleannames = TRUE); no forked relabel.
tab_counts_normalize <- function(data, row_col, col_col, tab_cols, n_col, wn_col,
                                 cleannames = FALSE) {
  keys <- c(tab_cols, row_col, col_col)
  miss <- setdiff(c(keys, n_col, wn_col), names(data))
  if (length(miss) > 0)
    cli::cli_abort("Column{?s} {.field {miss}} not found in {.arg data}.")

  # Phase k: convert labelled (haven/labelled) key columns to value-label factors before the
  # factor-order coercion below, so a labelled key reads as a factor (byte-identical for non-labelled).
  data <- data |> tab_apply_val_labels(keys)

  raw_n      <- suppressWarnings(as.numeric(data[[n_col]]))
  has_real_n <- all(is.na(raw_n) | abs(raw_n - round(raw_n)) < 1e-8)

  d <- data.table::as.data.table(data)
  # Byte-identity hotspot: keys must be factors with the SAME level order a microdata table would
  # use -- keep existing factor levels, else first-appearance order (matches tab_plain's L2399).
  for (k in keys) if (!is.factor(d[[k]]))
    data.table::set(d, j = k, value = forcats::as_factor(d[[k]]))

  # cleannames strip on the (now-factor) keys, pre-aggregate -- reuse the microdata helper so there is
  # no forked relabel; the keyby sum below re-merges any level a strip collapses. No-op when off.
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
#' @param pct,color,color_signif,OR,test,na,cleannames,ref,ref2,comp,ci,conf_level,stars,method_cell,method_diff,method_ratio,totaltab,totaltab_name,tot,total_names,add_n,add_pct,common_totrow,subtext,digits,n_min,display,color_breaks,spread_vars,names_prefix,names_sort
#'   Same meaning as in [tab()]. `color` accepts every form [tab()] does (`FALSE` / `TRUE` /
#'   a measure / `c(text, background)` / `list(pct =, mean =)`). Only `na = "keep"` / `"drop"` are
#'   available (`"drop_all"` / `"common_base"` need the microdata). The [tab()] arguments that pick or
#'   collapse levels *during the microdata prep* — which `tab_counts()` starts past — are not offered:
#'   `levels = "first"` / `"auto"` (keeping a subset of levels), `other_if_less_than` / `other_level`
#'   (lumping rare levels counts individual observations); build from microdata with [tab()] for those.
#'   Likewise the microdata-only / numeric-mean-only arguments: `wt` (use `wt_counts`); a survey
#'   design as `data` (per-observation weights and structure a count table cannot carry -- it is
#'   refused with a message); `method_mean_diff`/`method_mean_ratio` (a counts table has no
#'   numeric column); `parallel`; `output_list`; `sup_cols`.
#' @param chi2 `r lifecycle::badge("deprecated")` Renamed to \code{test} in 2.0.0 (see [tab()]).
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
                       pct = "no", color = "no", color_signif = "ignore",
                       OR = "no", test = FALSE,
                       na = "keep", cleannames = NULL,
                       ref = "auto", ref2 = "first", comp = "tab",
                       ci = "no", conf_level = 0.95, stars = NULL,
                       method_cell = "wilson", method_diff = "newcombe",
                       method_ratio = "katz",
                       totaltab = "line", totaltab_name = "Ensemble",
                       tot = c("row", "col"), total_names = "Total",
                       add_n = TRUE, add_pct = FALSE, common_totrow = FALSE,
                       subtext = "", digits = 0, n_min = 0, display = NULL,
                       color_breaks = NULL,
                       spread_vars = character(), names_prefix = NULL, names_sort = FALSE,
                       chi2 = lifecycle::deprecated()) {

  # Phase 14a: `chi2` renamed `test` (see tab()) -- kept working, one soft nudge.
  if (lifecycle::is_present(chi2)) {
    lifecycle::deprecate_soft("2.0.0", "tab_counts(chi2 = )", "tab_counts(test = )")
    test <- chi2
  }

  # Last Phase z14-i: tab_counts() starts from pre-aggregated counts, so it is the ONE entry point
  # that REFUSES a survey design rather than unwrapping it -- a design's weights and structure are
  # per-observation facts that a count table cannot carry. Same svy_is_design() as the four accepting
  # entry points, so "what is a design" has one answer.
  if (svy_is_design(data))
    cli::cli_abort(c(
      "{.fn tab_counts} works on pre-aggregated counts; a survey design carries microdata.",
      "i" = "Pass the design to {.fn tab} instead, or give the weighted counts in {.arg wt_counts}."
    ))
  # `test` is TRUE/FALSE. It used to be forwarded as a truthy `chi2`, so tab_counts(test = "survey")
  # silently produced a CLASSIC test.
  test <- svy_check_test(test)

  input <- rlang::arg_match(input)
  vctrs::vec_assert(pct, size = 1); vctrs::vec_assert(na, size = 1)
  # `color` is NOT size-1-asserted (like tab()): it accepts FALSE/TRUE/scalar/c(text, background)/
  # a per-type list -- parsed by normalize_color_spec() below.
  # Phase 6g (S3): na = "common_base" is microdata-only -- it fixes the population from who is
  # NA on the row_var/first col_var, which pre-aggregated counts cannot reconstruct. na = "drop_all"
  # (drop every row missing on ANY variable) is likewise a whole-DB row drop with no meaning on counts.
  if (identical(na, "common_base") || identical(na, "drop_all")) {
    cli::cli_abort(c(
      "{.code na = {na}} is only available in {.fn tab} (from microdata).",
      "i" = "Pre-aggregated counts cannot reconstruct who was missing; use {.val keep} or {.val drop}."
    ))
  }
  stopifnot(na %in% c("keep", "drop"))
  stopifnot(all(tot %in% c("row", "col", "both", "no", "")))
  if (tot[1] == "both") tot <- c("row", "col")
  total_names <- vctrs::vec_recycle(total_names, 2)
  cleannames <- if (is.null(cleannames)) getOption("tabxplor.cleannames") else cleannames

  # Phase 5: parse `color` (+ `color_signif`) once, exactly as tab()/tab_many() do: the engine runs
  # on the legacy string ($legacy) + the significance policy ($signif) + the ratio-CI flag, then
  # finalize_color_tail() sets the final two-channel colour attributes on the built table.
  color_spec <- normalize_color_spec(color, color_signif)

  # -- resolve the input SHAPE to canonical long tidy counts, then to the aggregate (the one
  #    validation boundary) --
  resh <- tab_counts_reshape(
    data,
    row_var   = rlang::enquo(row_var),  col_var   = rlang::enquo(col_var),
    tab_vars  = rlang::enquo(tab_vars), counts    = rlang::enquo(counts),
    wt_counts = rlang::enquo(wt_counts), cols     = rlang::enquo(cols),
    base      = rlang::enquo(base),     col_name  = col_name, input = input)

  norm       <- tab_counts_normalize(resh$data, resh$row_var, resh$col_var, resh$tab_vars,
                                      resh$n_col, resh$wn_col, cleannames = cleannames)
  fine       <- norm$fine
  weighted   <- norm$weighted
  has_real_n <- norm$has_real_n
  row_var    <- rlang::sym(resh$row_var)
  col_var    <- rlang::sym(resh$col_var)
  tab_vars   <- resh$tab_vars

  # Base-less input: no real unweighted n -> inference is not defined; keep pct/diff/colors.
  if (!has_real_n && (!identical(ci, "no") || !isFALSE(test))) {
    cli::cli_warn(c(
      "!" = "The counts are not whole numbers (weighted or frequency-only): confidence intervals and the test are disabled.",
      "i" = "Provide real unweighted counts in {.arg counts} (with the weighted counts in {.arg wt_counts}) to enable them."
    ))
    ci <- "no"; test <- FALSE
  }

  # -- Phase 7d-ii / 9a: route the single (row_var x col_var) FACTOR pair through the SAME engine stages
  #    tab() uses. tab_counts() already holds its tier-1 aggregate (`fine`), so it BYPASSES
  #    tab_prepare_pop() (no microdata to prep) and tab_aggregate() (nothing to scan): it builds the
  #    ctx, runs tab_setup() (arg resolution -- incl. the SAME `tot` -> totrow/totcol translation +
  #    tot_cols_type + colour cascade tab() uses), injects `fine` as the fused factor aggregate + the
  #    single-pair pop/level metadata, then runs the shared tab_build_tables() (the outer map +
  #    output shape -- one row_var here -> one serial unit). This guarantees byte-identity with tab()
  #    for every `tot`, and deletes the hand-inlined finalize copy. NOTE: contrib colouring now forces a
  #    total row (as tab() does) -- a deliberate convergence (was skipped when totrow was driven by
  #    `tot`). --
  data_skel <- as.data.frame(fine)

  # tot -> (totrow, totcol), exactly as tab()'s wrapper translates it.
  totrow <- "row" %in% tot
  totcol <- if ("col" %in% tot) "last" else "no"

  # Phase 17e: the same typed new_ctx() constructor tab_build() uses. tab_counts() sets the fields it
  # needs and inherits the rest (parallel/cache_env/defer_level_merge/levels_order/method_mean_* -- the
  # mean CI methods are numeric-only and inert on a counts table, but ci_settings expects them) from the
  # ONE defaults list. Colour rides the parsed spec, exactly as tab() does (R/tab.R): the legacy string,
  # the significance policy, and color_pct_text_is_ratio() (whether the reader's pct channel IS the
  # ratio measure -> it owns the stored interval).
  ctx <- new_ctx(
    data = data_skel, with_filter = FALSE,
    row_vars_quo = rlang::quo(!!row_var), col_vars_quo = rlang::quo(!!col_var),
    tab_vars_quo = if (length(tab_vars) == 0) rlang::quo(NULL)
                   else rlang::quo(c(!!!rlang::syms(tab_vars))),
    wt_quo = if (weighted) rlang::quo(wn) else rlang::quo(NULL),
    na_drop_all_quo = rlang::quo(NULL),
    pct = pct, color = color_spec$legacy, color_signif = color_spec$signif,
    color_ratio_ci = color_pct_text_is_ratio(color_spec),
    OR = OR, chi2 = test,
    na = na, levels = "all",
    cleannames = cleannames, output = "single",
    ref = ref, ref2 = ref2, comp = comp, ci = ci, conf_level = conf_level, stars = stars,
    method_cell = method_cell, method_diff = method_diff, method_ratio = method_ratio,
    totaltab = totaltab, totaltab_name = totaltab_name, totrow = totrow, totcol = totcol,
    total_names = total_names, add_n = add_n, add_pct = add_pct, common_totrow = common_totrow,
    digits = digits, n_min = n_min, subtext = subtext, by_table = FALSE,
    # Last Phase z16-iiiii: pre-aggregated counts carry no per-observation Sum(w^2), so this table
    # cannot serve the weighted basis -- declared here rather than discovered in the leaf, so the
    # footer states basis "n" from the start and no design-based omnibus is attempted on aggregate rows.
    agg_only = TRUE,
    spread_vars = spread_vars, names_prefix = names_prefix, names_sort = names_sort
  )

  ctx <- tab_setup(ctx)

  # Set the single-pair population/level metadata (levels = "all" -> lv1 FALSE / no level removal;
  # no NA-drop beyond the `na` policy) and inject the count aggregate as the fused tier-1 (tab_plain
  # (.fine=) adopts it). levels = "first"/"auto" would need remove_levels computed in the bypassed
  # tab_prepare_pop, so tab_counts keeps all levels (see the header KEY CONSTRAINTS).
  ctx <- ctx_update(ctx, list(
    na_text = list(ctx$na), na_num = list(ctx$na),
    lv1 = FALSE, remove_levels = NULL,
    fine_num = NULL, fine_fused = fine
  ))

  result <- tab_build_tables(ctx)

  # The shared wrapper tail (finalize colour spec -> display recipe -> per-table breaks), identical to
  # tab()/tab_many() -> a modern colour (TRUE / two-channel / per-type / color_signif / ratio), a
  # `display` recipe and a per-table `color_breaks` override are all applied here, on the built table.
  finalize_color_tail(result, color_spec, color_breaks, display)
}
