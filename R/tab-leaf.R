# PURPOSE: THE AGGREGATE CORE -- the two leaves that turn microdata into fmt cells.
# ROLE: `tab_plain()` (factor col_vars) and `tab_num()` (numeric col_vars) are the public superseded
#   entry points; `plain_core()` / `num_core()` are their resolved-argument compute cores, which
#   tab_build() calls directly. This is where a percentage, a mean, its interval and the table's
#   test first exist as numbers -- everything downstream only reshapes or renders them.
# KEY CONSTRAINTS:
#   - WRAPPER / CORE split. The public leaf defuses NSE, validates, and normalises the colour spec;
#     a shared `*_resolve()` resolves the arguments; the `*_core()` builds the cells and returns
#     PRE-FINALISE. tab_transform() calls the CORES, so forcing runs once and colour finalises once.
#   - ONE PASS. The leaf computes the cells, THEIR interval (leaf_ci_plain / ci_dispatch) and the
#     whole-table TEST (leaf_chi2 / leaf_chi2_num), because the leaf is where the plan is. Nothing
#     re-reads a built table to add them afterwards.
#   - Both leaves share one head (leaf_inference_setup) and one tail (leaf_finish), so the inference
#     stamp and the row index are written in exactly one place each.
#   - build_total_rows() and num_rollup() are deliberately NOT merged: base::sum over split() vs
#     data.table gforce is a 1-ULP contract on both sides. See their headers before touching either.
# See: CLAUDE.md § tabxplor architecture (the calculation pipeline).

# === SECTION: The factor leaf -- tab_plain() / plain_resolve() / plain_core() =================

#' Plain single cross-table
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' One bare cross-table of counts or percentages, from ONE row variable and ONE column variable.
#' Superseded by [tab()], which does the same and everything around it (several variables, colours,
#' totals, tests) -- but it stays the smallest entry point into the aggregate core, and takes the
#' same `ci` / `ci_method` / `conf_level` / `stars` / `display` arguments, resolved by the same
#' rules, so its numbers agree with `tab()`'s cell for cell.
#' @eval tab_args_rd("tab_plain")
#' @param ... Every other argument of [tab()] -- `pct`, `color`, `ci`, `tot`, ... -- passed
#'   by name. See [tab()]; a typo gets a suggestion.
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}. If \code{...} (\code{tab_vars})
#'  are provided, a \code{tab} of class \code{tabxplor_grouped_tab}.
#' All non-text columns are \code{\link{fmt}} vectors of class \code{tabxplor_fmt},
#' storing all the data necessary to print formats and colors. Columns with \code{row_var}
#' and \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples
#' \donttest{
#' data <- dplyr::starwars |> tab_prepare(sex, hair_color)
#'
#' # the leaf builds the cells AND their intervals (2.0.0): `ci` is resolved here exactly as in
#' # tab(), so tab_plain(ci = "ref") and tab(ci = "ref") agree cell for cell.
#' data |>
#'   tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row",
#'             ci = "ref", color = "difference", color_signif = "grey_non_signif")
#'
#' # the whole-table test is still a step (superseded, but supported)
#' data |>
#'   tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row") |>
#'   tab_chi2()
#' }
tab_plain <- function(data, row_var, col_var, tab_vars, wt, ...,
                      num = FALSE, df = FALSE, .fine = NULL, .by_table = FALSE
) {
  .d <- rlang::list2(...)
  tab_check_dots(.d, "tab_plain")
  list2env(tab_dots_expand(.d, "tab_plain"), environment())

  # A survey design as `data` is unwrapped FIRST -- tidyselect must see a data frame.
  svy   <- svy_unwrap_data(data, "tab_plain")
  if (!is.null(svy)) data <- svy$data
  .a <- tab_resolve_common_args(
    "tab_plain", color = color, color_signif = color_signif, stars = stars,
    conf_level = conf_level, OR = OR, display = display, ref = ref, ref2 = ref2,
    tot = tot, na = na, pct = pct, comp = comp, totaltab = totaltab,
    total_names   = .d$total_names,
    totaltab_name = .d$totaltab_name,
    other_level   = .d$other_level,
    ci = ci, ci_method = ci_method, user_env = rlang::caller_env())
  stars <- .a$stars ; display <- .a$display ; ref <- .a$ref ; ref2 <- .a$ref2
  total_names <- .a$total_names ; ci_method <- .a$ci_method ; conf_level <- .a$conf_level
  totaltab_name <- .a$totaltab_name
  color <- .a$color ; color_spec <- .a$color_spec

  .v <- leaf_defuse_vars(data, rlang::enquo(row_var), rlang::enquo(col_var),
                         rlang::enquo(tab_vars), rlang::enquo(wt), svy = svy, plural = FALSE)
  data <- .v$data ; row_var <- .v$row_var ; col_var <- .v$col ; tab_vars <- .v$tab_vars ; wt <- .v$wt



  comparison <- tab_leaf_comparison(color, display, pct, ref)
  r_ci  <- resolve_leaf_ci(ci, color, color_signif, stars, ref)
  stars <- r_ci$stars ; color_signif <- r_ci$color_signif
  or_ci <- identical(comparison, "odds_ratio") && identical(r_ci$ci, "ref")
  ci_leaf  <- if (or_ci) "no" else if (identical(r_ci$ci, "ref")) "diff" else r_ci$ci
  ci_scale <- if (identical(comparison, "ratio")) "ratio" else "diff"
  r <- plain_resolve(pct, ref, ref2, na, totaltab_name, total_names, tot, comp, color,
                     digits, totaltab, tab_vars, comparison = comparison)
  result <- plain_core(
    data, row_var, col_var, tab_vars, wt,
    pct = r$pct, color = color, na = r$na, ref = r$ref, ref2 = r$ref2, comp = r$comp,
    totaltab = r$totaltab, totaltab_name = totaltab_name, tot = r$tot, total_names = r$total_names,
    subtext = subtext, digits = r$digits, num = num, df = df,
    stars = stars, color_signif = color_signif, .fine = .fine, .by_table = .by_table,
    comparison = comparison, ci = ci_leaf, ci_scale = ci_scale,
    or_ci = or_ci,
    inference = new_inference(wt, svy$spec, conf_level, ci_method, design_effect = design_effect)
  )

  if (df || num) return(result)

  finalize_color_tail(result, color_spec, NULL, display)
}


# The leaf's NSE preamble, shared by plain_core(), num_core() and tab_aggregate_num(): missing / NA
# / NULL / "" / "no" means "the user named nothing" and gets a synthesised constant column. A
# `svy` design's own weight column REPLACES `wt`.
#' @keywords internal
#' @noRd
leaf_defuse_vars <- function(data, row_var_quo, col_quo, tab_vars_quo, wt_quo,
                             svy = NULL, plural = FALSE) {
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data    <- data |> dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::sym(rlang::as_name(row_var_quo))
  }

  pos_col_vars <- NULL
  if (quo_miss_na_null_empty_no(col_quo)) {
    data <- data |> dplyr::mutate(no_col_var = factor("n"))
    col  <- if (plural) rlang::syms("no_col_var") else rlang::sym("no_col_var")
    if (plural) pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else if (plural) {
    pos_col_vars <- tidyselect::eval_select(col_quo, data)
    col          <- rlang::syms(names(pos_col_vars))
  } else {
    col <- rlang::sym(rlang::as_name(col_quo))
  }

  if (quo_miss_na_null_empty_no(tab_vars_quo)) {
    tab_vars <- character()
  } else {
    tab_vars <- rlang::syms(names(tidyselect::eval_select(tab_vars_quo, data)))
  }

  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::sym(rlang::as_name(wt_quo))
  }
  if (!is.null(svy)) {
    svy_abort_wt_design(length(wt) != 0L)
    wt <- rlang::sym(svy$spec$wt)
  }

  # DESIGN: the LEAF's home for the reserved-level check -- a direct leaf call never reaches
  # tab_prepare(), where the pipeline's copy lives.
  lvl_check_reserved(data, c(rlang::as_name(row_var),
                             vapply(if (plural) col else list(col), rlang::as_name, character(1)),
                             vapply(tab_vars, rlang::as_name, character(1))))

  list(data = data, row_var = row_var, col = col, pos_col_vars = pos_col_vars,
       tab_vars = tab_vars, wt = wt)
}


# plain_resolve() -- the factor leaf's FORCING cascade (pct -> tot -> comp -> ref -> digits ->
# totaltab), shared with tab_transform(). Vocabulary checks live at the argument boundary.
#' @keywords internal
#' @noRd
plain_resolve <- function(pct, ref, ref2, na, totaltab_name, total_names, tot, comp, color,
                          digits, totaltab, tab_vars, comparison = NA_character_) {
  vctrs::vec_assert(ref, size = 1)
  ref <- stringi::stri_trim_both(stringi::stri_replace_all_regex(ref, "\\s+", " "))
  vctrs::vec_assert(ref2, size = 1)
  ref2 <- stringi::stri_trim_both(stringi::stri_replace_all_regex(ref2, "\\s+", " "))
  vctrs::vec_assert(totaltab_name, size = 1)

  if (pct == "all_tabs" & length(tab_vars) == 0) pct <- "all"

  if (color != "no" & ref == "no") {
    warning("since color is ", color, " ref can't be `no` and was set to `tot`")
    ref <- "tot"
  }

  if (is.null(tot)) {
    tot <- switch(pct,
                  "no"  = "no",
                  "row" = , #switch(ref, "tot" = c("row", "col"), "col"),
                  "col" = , #switch(ref, "tot" = c("row", "col"), "row"),
                  "all" = ,
                  "all_tabs" = c("row", "col"),
    )

  } else {
    # `both` expands differently on the two leaves: both totals here, the row total on tab_num().
    if (tot[1] == "both") tot <- c("row", "col")

    if (!"col" %in% tot) {
      if (pct == "row") {
        warning("since pct == 'row', a total column was added")
        tot <- c(tot, "col")
      }
      if (color != "no" & pct == "col" & ref == "tot") {
        warning("since color == ", color, " and pct == 'col' and ref == 'tot', a total column was added")
        tot <- c(tot, "col")
      }
      if (pct %in% c("all", "all_tabs")) {
        warning("since pct == 'all' or 'all_tabs', a total column was added")
        tot <- c(tot, "col")
      }
    }

    if (!"row" %in% tot) {
      if (pct == "col") {
        warning("since pct == 'col', total rows were added")
        tot <- c(tot, "row")
      }
      if (color != "no" & pct == "row" & ref == "tot") {
        warning("since color == ", color, " and pct == 'row' and ref == 'tot', total rows were added")
        tot <- c(tot, "row")
      }
      if (pct %in% c("all", "all_tabs")) {
        warning("since pct == 'all' or 'all_tabs', total rows were added")
        tot <- c(tot, "row")
      }
    }
  }

  vctrs::vec_assert(comp, size = 1)
  stopifnot(comp %in% c("tab", "all", "") | is.na(comp) | is.null(comp))

  comp <- force_comp(comp, tab_vars)

  # DESIGN: `ref = "auto"` stays in the LEAF, being type-specific (a mixed table needs a different
  # answer here and in num_resolve()); WHICH reference is the measure's declared `ref_auto`.
  if (ref == "auto") {
    ra  <- measure_ref_auto(if (is.na(comparison) || !nzchar(comparison)) color else comparison)
    ref <- if (!is.na(ra)) ra else "tot"
  }

  vctrs::vec_assert(digits, size = 1)
  digits <- vctrs::vec_cast(digits, integer())

  if (length(tab_vars) == 0) totaltab <- "no"

  if (((comp[1] == "all" & ref == "tot") | pct == "all_tabs") &
      !totaltab %in% c("table", "line")) {
    warning("since comp = 'all', a total table was added to compare with")
    totaltab <-  "line"
  }

  if (comp[1] == "all" & !ref %in% c("tot", "no", "") & totaltab != "table") {
    warning("since comp = 'all', a full total table was added to compare with")
    totaltab <- "table"
  }

  list(pct = pct, ref = ref, ref2 = ref2, na = na, total_names = total_names,
       tot = tot, comp = comp, digits = digits, totaltab = totaltab)
}


# plain_core() -- the factor leaf's compute core. Consumes ALREADY-RESOLVED settings (plain_resolve)
# plus the resolved NSE syms and returns the table PRE-FINALISE; colour is finalised by the caller.
# It runs as twenty numbered blocks, each head naming WHAT IT PRODUCES; the numbers are the order.
#' @keywords internal
#' @noRd
plain_core <- function(data, row_var, col_var, tab_vars, wt, pct, color, na, ref, ref2, comp,
                       totaltab, totaltab_name, tot, total_names, subtext, digits, num, df,
                       stars, color_signif, .fine, .by_table, inference,
                       comparison = NA_character_, or_ci = FALSE, dichotomise = FALSE,
                       ci = "no", ci_scale = "diff", test = "no", deff = NULL) {
  # `comparison` = the geometry this table compares on; `or_ci` = the LEAF owns the Woolf log-OR
  # interval rather than the cell / contrast one (they cannot co-occur); `ref2` picks the OR's 2x2.
  or_compare <- identical(comparison, "odds_ratio")
  # WARNING: `inference` is REQUIRED -- a default would silently re-read the global option instead.
  list2env(leaf_inference_setup(inference, .fine, .by_table), environment())
  des_rows <- NULL

  # 2. DIRECT-ENTRY PREP -> `data` fit for the scan (labelled -> factor, select, weight coercion).
  if (use_raw) {
    # labelled columns -> value-label factors, for the DIRECT leaf entry; a no-op on the tab() path.
    data <- data |> tab_apply_val_labels(as.character(c(tab_vars, row_var, col_var)))
    data <- data |>
      dplyr::select(!!!tab_vars, !!row_var, !!col_var, !!wt,
                    tidyselect::any_of(if (design_on) svy_row_col else character())) |>
      dplyr::mutate(dplyr::across(!!wt & !where(is.numeric), as.numeric)) |>
      relabel_levels_in_varnames(as.character(col_var))
  }






  # 3. THE data.table NAME ROUND-TRIP -> internal names (see the sentinel note below).
  tab_row_names  <- as.character(c(tab_vars, row_var))

  # DESIGN: the data.table name round-trip. The col_var becomes the fixed internal "col_var" so the
  # dcast formula is stable, and a col_var that ALSO appears among the row / tab vars is duplicated
  # as "<var>_colvarbis"; those names and dcast's "n_" / "wn_" prefixes are stripped further down.
  # WARNING: "Total" is a fourth internal name -- the pre-rename key of every total row, tab and
  # column, read as such by leaf_wide_pct(), num_rollup() and the survey variance producers, and
  # swapped for `total_names` only in leaf_rename_totals(); a consumer running after that rename
  # must be handed `totcol_vector` / `totrow_vector` / `tottab_vector` instead.
  col_var_in_row_var <- tab_row_names %in% as.character(col_var)
  if (any(col_var_in_row_var)) {
    in_col_vars <- tab_row_names[col_var_in_row_var]

    data <- data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(in_col_vars), ~ ., .names = "{.col}_colvarbis"))
    tabs_vars2 <-
      if (length(tab_vars) != 0) {
        dplyr::recode(as.character(tab_vars),
                      !!!purrr::set_names(paste0(in_col_vars, "_colvarbis"),
                                          in_col_vars))
      } else {
        character()
      }

    row_var2 <- dplyr::recode(as.character(row_var),
                              !!!purrr::set_names(paste0(in_col_vars, "_colvarbis"),
                                                  in_col_vars))
    tab_row_names2 <- c(tabs_vars2, row_var2)
  } else {
    tab_row_names2 <- tab_row_names
  }



  # 4. THE AGGREGATE -> `long` (one row per cell), from the raw scan or a pre-aggregate...
  if (use_raw) {
    # lift `.svy_row` (the row's position in the ORIGINAL design) out before data.table takes over.
    if (design_on) { des_rows <- data[[svy_row_col]]; data[[svy_row_col]] <- NULL }

    data.table::setDT(data)
    data.table::setnames(data, as.character(col_var), "col_var", skip_absent = TRUE)

    if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop_all')")
  } else if (nrow(.fine) == 0) {
    stop("data is of length 0 (possibly after filter or na = 'drop_all')")
  }

  # DESIGN: two aggregation sources, one dcast -- a raw scan per row_var x col_var (`use_raw`), or a
  # roll-up of the shared finest-grain `.fine`. Sigma w^2, the statistic behind the weighted
  # effective base, is accumulated whenever WEIGHTED; an input that lacks it leaves n_eff NA.
  weighted <- length(wt) != 0
  if (use_raw) {
    long <- data[, list(n  = .N,
                        wn = if(weighted) { sum(eval(wt), na.rm = TRUE) } else {double()},
                        w2 = if(weighted) { sum(eval(wt)^2, na.rm = TRUE) } else {double()}),
                 keyby = eval(c(tab_row_names2, "col_var"))]
  } else {
    ocv  <- as.character(col_var)
    keep_w2 <- weighted && "w2" %in% names(.fine)
    long <- if (keep_w2) {
      .fine[, list(n = as.integer(sum(n)), wn = sum(wn), w2 = sum(w2)),
            keyby = eval(c(tab_row_names, ocv))]
    } else if (weighted) {
      .fine[, list(n = as.integer(sum(n)), wn = sum(wn)), keyby = eval(c(tab_row_names, ocv))]
    } else {
      .fine[, list(n = as.integer(sum(n))),              keyby = eval(c(tab_row_names, ocv))]
    }
    if (ocv != "col_var") data.table::setnames(long, ocv, "col_var")
  }

  # the flat design's nPSU, read off the AGGREGATE so scan and pre-aggregate agree.
  n_obs <- sum(as.double(long$n))

  # WARNING: the unweighted scan produces an EMPTY `w2` column -- gate on weighted AND presence.
  has_w2 <- weighted && "w2" %in% names(long)
  # the weighted basis was asked for but this input cannot serve it -> the table states basis "n".
  unserved <- identical(inference_basis, "weights") && !has_w2
  degraded <- FALSE
  # `want_neff` = the basis asks for an effective base, `can_neff` = this input can supply one.
  can_neff  <- has_w2 || design_on

  # 5. RESHAPE -> `tabs`: one row per (tab_vars x row_var), one column per col_var level.
  tabs <-
    data.table::dcast(
      long,
      formula = ... ~ col_var,
      value.var = if (has_w2) {c("n", "wn", "w2")} else if (length(wt) != 0) {c("n", "wn")} else {"n"},
      fill = 0
    )

  # drop the empty `w2` id column dcast leaks when it is not a value.var (as for `wn` just below).
  if (!has_w2 && "w2" %in% names(tabs)) tabs[, "w2" := NULL]


  if (any(col_var_in_row_var)) {
    colvarbis <- names(tabs)[stringi::stri_detect_regex(names(tabs), "_colvarbis$")]
    data.table::setnames(tabs, colvarbis, stringi::stri_replace_first_regex(colvarbis, "_colvarbis$", ""),
                         skip_absent = TRUE)
  }

  not_fct <- !purrr::map_lgl(dplyr::select(tabs, tidyselect::all_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    tabs[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
         .SDcols = names(not_fct)[not_fct]]
  }


  # 6. THE NA POLICY -> `tabs` with the NA column and rows kept or dropped.
  na_cols <- names(tabs) %in% c("n_NA", "wn_NA", "w2_NA", "NA")
  if (any(na_cols)) {
    if (na == "drop") {
      suppressWarnings(tabs[, `:=`(n_NA = NULL, wn_NA = NULL, w2_NA = NULL, `NA` = NULL)])
    } else {
      data.table::setcolorder(tabs, c(names(tabs)[!na_cols], names(tabs)[na_cols]))
    }
  }

  na_rows <- tabs |>
    dplyr::select(!!!tab_vars, !!row_var) |>
    dplyr::mutate(na_rows = dplyr::if_any(.cols = dplyr::everything(), .fns = is.na)) |>
    dplyr::pull(.data$na_rows)

  if (any(na_rows)) {
    if (na == "drop") {
      tabs <- tabs[-which(na_rows), ]
    } else {
      data.table::setorderv(
        tabs, tab_row_names, na.last = TRUE
      )[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
        .SDcols = tab_row_names]
    }
  }

  # 7. THE TOTAL ROWS AND TABS -> `tabs` with them appended (the two blocks below).
  num_cols <- tabs |> purrr::map_lgl(is.numeric)
  num_cols <- names(num_cols)[num_cols]

  # "table" = one total row per row_var level (tab_vars set to "Total"); "line" = one grand total row.
  if (totaltab %in% c("table", "line")) {
    if (totaltab[1] == "table") { bt_keys <- as.character(row_var); bt_totvars <- as.character(tab_vars) }
    else                        { bt_keys <- character();           bt_totvars <- tab_row_names }
    tabs_totaltab <- build_total_rows(tabs, bt_keys, tab_row_names, num_cols)
    tabs <- finalize_total_rows(tabs, tabs_totaltab, bt_totvars, tab_row_names)
  }



  if ("row" %in% tot) {
    if (length(tab_vars) != 0) {
      group_vars <- rev(purrr::accumulate(as.character(tab_vars) , ~ c(.x, .y)))
      total_vars <- purrr::map(group_vars,
                               ~ c(as.character(tab_vars)[!as.character(tab_vars) %in% .],
                                   as.character(row_var)))
    } else {
      group_vars <- list(character())
      total_vars <- list(as.character(row_var))
    }

    parts    <- purrr::map(group_vars,
                           ~ build_total_rows(tabs, .x, tab_row_names, num_cols))
    tabs_tot <- do.call(rbind, parts)
    tabs_tot <- tabs_tot[do.call(order, tabs_tot[tab_row_names]), , drop = FALSE]
    tabs_tot <- tabs_tot[!duplicated(tabs_tot), , drop = FALSE]

    if (totaltab == "line") {
      keep     <- Reduce(`|`, lapply(tab_row_names, function(v) as.character(tabs_tot[[v]]) != "Total"))
      tabs_tot <- tabs_tot[keep, , drop = FALSE]
    }

    tabs <- finalize_total_rows(tabs, tabs_tot, unique(unlist(total_vars)), tab_row_names)
  }

  # 8. THE ROW ROLES -> totrow_vector / tottab_vector / kind_vector, declared not re-derived.
  tt <- leaf_totrow_tottab(tabs, row_var, tab_vars)
  totrow_vector <- tt$totrow; tottab_vector <- tt$tottab; kind_vector <- tt$kind




  # 9. THE OPTIONAL LEAF TABLES -> nothing; it DECLARES what the phases below may compute.
  tabs_wn <- tabs_w2 <- tabs_pct <- tabs_totn <- tabs_neff <- NULL
  tabs_diff <- tabs_mean <- tabs_rr <- tabs_or <- NULL
  tabs_or_ci_inf <- tabs_or_ci_sup <- tabs_or_pvalue <- NULL
  refcols_vector <- refrows <- NULL

  # 10. THE COUNT TABLES -> tabs_n / tabs_wn / tabs_w2, and `cols` (which columns hold values).
  if (length(wt) == 0) {
    if ("wn" %in% names(tabs)) tabs[, "wn" := NULL]

    text_vars <- !purrr::map_lgl(tabs, is.numeric)
    text_vars <- text_vars[text_vars]

    if ("col" %in% tot) {
      tabs[, "Total" := as.integer(rowSums(tabs[, -text_vars, with = FALSE]))] #Problems if not integer.
    }
    tabs_n <- tabs

  } else {
    text_vars <- !purrr::map_lgl(tabs, is.numeric)
    n_index  <- stringi::stri_detect_regex(names(tabs), "^n_")  | text_vars
    wn_index <- stringi::stri_detect_regex(names(tabs), "^wn_") | text_vars
    w2_index <- stringi::stri_detect_regex(names(tabs), "^w2_") | text_vars

    text_vars <- text_vars[text_vars]

    tabs_n  <- data.table::setnames(tabs[, n_index, with = FALSE] ,
                                    function(.x) stringi::stri_replace_first_regex(.x, "^n_" , ""))
    tabs_wn <- data.table::setnames(tabs[, wn_index, with = FALSE],
                                    function(.x) stringi::stri_replace_first_regex(.x, "^wn_", ""))

    tabs_wn[, (names(tabs_wn)) := purrr::map(.SD, as.double)]

    if (has_w2) {
      tabs_w2 <- data.table::setnames(tabs[, w2_index, with = FALSE],
                                      function(.x) stringi::stri_replace_first_regex(.x, "^w2_", ""))
      tabs_w2[, (names(tabs_w2)) := purrr::map(.SD, as.double)]
    }

    if ("col" %in% tot) {
      tabs_n [, "Total" := as.integer(rowSums(tabs_n[, -names(text_vars), with = FALSE] ))] #Problems if not integer.
      tabs_wn[, "Total" := rowSums(tabs_wn[, -names(text_vars), with = FALSE])]
      if (has_w2) tabs_w2[, "Total" := rowSums(tabs_w2[, -names(text_vars), with = FALSE])]
    }

  }
  tabs_text <- tabs[, names(text_vars), with = FALSE] #tibble::as_tibble()
  cols <- purrr::map_lgl(tabs_n, is.numeric)
  cols <- cols[cols]


  # DESIGN: copy() before each in-place := below -- the aggregate is shared by reference, so a bare
  # := would mutate the source and every other derived table (data.table semantics).
  # THE per-cell inference base is `n_eff` = p(1-p) / Var_design(p), ONE definition whose
  # implementation the resolved basis picks: the closed form on the per-cell Sigma w^2 the aggregate
  # carries, or survey::svyrecvar on each cell's influence function. Writing it into the SAME field
  # makes every interval, star and threshold basis-aware at once; `p` is the DISPLAYED proportion,
  # so the interval inverts exactly what is printed.
  neff_dt <- function(Ne) {
    Ne[!is.finite(Ne)] <- NA_real_
    out <- data.table::copy(tabs_n)
    out[, (names(cols)) := lapply(seq_len(ncol(Ne)), function(j) Ne[, j])]
    out
  }
  leaf_neff <- function(res, base) {
    Pm <- as.matrix(res$pct[, names(cols), with = FALSE]) * 1.0
    if (design_on && !design_flat && !is.null(des_rows)) {
      vres <- svy_var_prop(
        prep      = svy_var_prep(inference$design, des_rows),
        keys      = lapply(tab_row_names,  function(v) svy_key_chr(tabs_n[[v]])),
        n_tab     = length(tab_vars),
        mkeys     = lapply(tab_row_names2, function(v) svy_key_chr(data[[v]])),
        mcol      = svy_key_chr(data[["col_var"]]),
        col_names = names(cols), base = base)
      if (!is.null(vres$v)) {
        Ne <- Pm * (1 - Pm) / vres$v
        Ne[!is.finite(Ne) | Ne <= 0] <- NA_real_
        # A DEGENERATE cell (Var = 0) falls back to its base domain's closed form, as the flat arm does.
        if (has_w2 && anyNA(Ne)) {
          M_w2 <- as.matrix(tabs_w2[, names(cols), with = FALSE]) * 1.0
          fb <- svy_flat_base_neff(res$dmat(res$m_pct), res$dmat(M_w2))
          Ne[is.na(Ne)] <- fb[is.na(Ne)]
        }
        return(neff_dt(Ne))
      }
      # -> basis "design_partial"; the weights still apply, so the cell keeps the flat closed form.
      degraded <<- svy_var_degraded(vres$reason)
    }
    if (!has_w2) return(NULL)
    M_w2 <- as.matrix(tabs_w2[, names(cols), with = FALSE]) * 1.0
    neff_dt(svy_flat_neff_prop(P = Pm, A = M_w2, S = res$dmat(M_w2), B = res$dmat(res$m_pct),
                               n_obs = n_obs))
  }

  if (pct == "no" && want_neff && can_neff) {
    res_0 <- leaf_wide_pct(tabs_n, tabs_wn, "all", as.character(tab_vars), cols)
    ne_0  <- leaf_neff(res_0, "all")
    if (!is.null(ne_0)) tabs_neff <- ne_0
  }

  # 12. THE CELLS AND THE COMPARISON -> tabs_pct / tabs_totn, then tab_apply_reference()'s outputs.
  if (pct != "no") {
    # `tot_n` = each cell's OWN unweighted percentage base, broadcast from tabs_n per `pct`.
    res_e     <- leaf_wide_pct(tabs_n, if (length(wt) == 0) NULL else tabs_wn,
                               pct, as.character(tab_vars), cols)
    tabs_pct  <- res_e$pct
    tabs_totn <- res_e$tot_n
    if (want_neff && can_neff) {
      ne_e <- leaf_neff(res_e, pct)
      if (!is.null(ne_e)) tabs_neff <- ne_e
    }


    if (ref != "no" & pct %in% c("row", "col")) {
      # the leaf owns the Woolf OR interval only when the odds ratio IS the comparison.
      or_want_ci <- isTRUE(or_ci)
      ref_res <- tab_apply_reference(
        tabs = tabs, tabs_pct = tabs_pct, ref = ref, ref2 = ref2, comp = comp,
        or_compare = or_compare, pct = pct, tab_row_names = tab_row_names, tab_vars = tab_vars,
        row_var = row_var, tottab_vector = tottab_vector, totrow_vector = totrow_vector, cols = cols,
        # the leaf MINTS this column above, so here the literal IS the declaration.
        totcol_vector = names(cols) == "Total",
        tabs_totn = if (or_want_ci) tabs_totn else NULL,
        tabs_neff = if (or_want_ci && !is.null(tabs_neff)) tabs_neff else NULL,
        conf_level = conf_level, stars = stars, degf = inference$degf,
        dichotomise = dichotomise
      )
      tabs_diff <- ref_res$diff
      tabs_mean <- ref_res$ratio
      if (!is.null(ref_res$rr))             tabs_rr        <- ref_res$rr
      if (!is.null(ref_res$or))             tabs_or        <- ref_res$or
      if (!is.null(ref_res$or_ci_inf))      tabs_or_ci_inf <- ref_res$or_ci_inf
      if (!is.null(ref_res$or_ci_sup))      tabs_or_ci_sup <- ref_res$or_ci_sup
      if (!is.null(ref_res$or_pvalue))      tabs_or_pvalue <- ref_res$or_pvalue
      if (!is.null(ref_res$refcols_vector)) refcols_vector <- ref_res$refcols_vector
      if (!is.null(ref_res$refrows))        refrows        <- ref_res$refrows
    }
  }



  tabs_n [, names(text_vars) := NULL]
  if (!is.null(tabs_wn)) tabs_wn  [, names(text_vars) := NULL]
  if (!is.null(tabs_pct)) tabs_pct [, names(text_vars) := NULL]
  if (!is.null(tabs_diff)) tabs_diff[, names(text_vars) := NULL]
  if (!is.null(tabs_mean)) tabs_mean[, names(text_vars) := NULL]
  if (!is.null(tabs_rr)) tabs_rr  [, names(text_vars) := NULL]
  if (!is.null(tabs_or)) tabs_or  [, names(text_vars) := NULL]
  if (!is.null(tabs_or_ci_inf)) tabs_or_ci_inf[, names(text_vars) := NULL]
  if (!is.null(tabs_or_ci_sup)) tabs_or_ci_sup[, names(text_vars) := NULL]
  if (!is.null(tabs_or_pvalue)) tabs_or_pvalue[, names(text_vars) := NULL]
  if (!is.null(tabs_totn)) tabs_totn[, names(text_vars) := NULL]
  if (!is.null(tabs_neff)) tabs_neff[, names(text_vars) := NULL]

  # 13. THE COLUMN ROLES and the reference defaults -> totcol_vector / refrows.
  totcol_vector <- names(tabs_n) == "Total"
  NA_reals <- rep(NA_real_, nrow(tabs_n))

  if (ref == "tot") refrows <- rep(FALSE, nrow(tabs_n))

  if (is.null(refrows)) refrows <- rep(FALSE, nrow(tabs_n))

  # 14. THE PER-CELL INTERVAL -> ci_res (cell or contrast; ONE geometry, via ci_dispatch()).
  ci_res <- leaf_ci_plain(
    P     = if (!is.null(tabs_pct))
              as.matrix(tabs_pct)  * 1.0 else matrix(NA_real_, nrow(tabs_n), ncol(tabs_n)),
    tot_n = if (!is.null(tabs_totn))
              as.matrix(tabs_totn) * 1.0 else matrix(NA_real_, nrow(tabs_n), ncol(tabs_n)),
    n_eff = if (!is.null(tabs_neff))
              as.matrix(tabs_neff) * 1.0 else NULL,
    ci = ci, pct = pct, ci_scale = ci_scale,
    # a ROW contrast under comp = "all" is computed ungrouped; a column or cell interval is not.
    grp = if (identical(comp, "all") || length(tab_vars) == 0L) rep(1L, nrow(tabs_n)) else
      do.call(paste, c(lapply(as.character(tab_vars), function(v) as.character(tabs_text[[v]])),
                       sep = "\r")),
    ref_row = if (identical(as.character(ref), "tot")) totrow_vector else refrows,
    totrow  = totrow_vector,
    refcol  = if (!is.null(refcols_vector) &&
                  any(refcols_vector)) which(refcols_vector)[1] else NA_integer_,
    totcol  = totcol_vector,
    conf_level = conf_level, stars = stars,
    ci_method = inference$method, degf = inference$degf)

  # these stamps are column-INVARIANT here, computed ONCE and recycled by new_fmt().
  display_1 <- dplyr::case_when(
    isTRUE(ci_res$visible)                           ~ "ci",
    pct != "no"                                      ~ "pct",
    length(wt) != 0                                  ~ "wn" ,
    TRUE                                             ~ "n"
  )
  color_1 <- dplyr::case_when(
    color %in% c("", "no")                            ~ "",
    is_placeholder_var(row_var) | is_placeholder_var(col_var) ~ "",
    or_compare & pct %in% c("row", "col") & ref != "no" & ref2 != "no" ~ "odds_ratio",
    pct %in% c("row", "col") & ref != "no"            ~ "difference",
    TRUE                                              ~ ""
  )
  # `scale` says which estimate the column's INTERVAL belongs to, not what is displayed: on an
  # odds-ratio table every column estimates an odds ratio; a NA scale key is a cell interval.
  base_1   <- dplyr::if_else(pct != "no", pct, "none")
  scale_1  <- dplyr::case_when(or_compare & pct %in% c("row", "col") & ref != "no" ~ "odds_ratio",
                               !is.na(ci_res$scale)             ~ ci_res$scale,
                               pct != "no"                      ~ "level_pct",
                               TRUE                             ~ "level_n")
  ref_1    <- switch(as.character(ref), "no" = "", "tot" = "tot", as.character(ref))
  comp_1   <- dplyr::if_else(pct != "no" & ref != "no", comp == "all", NA)
  colvar_1 <- rlang::as_name(col_var)
  # 15. THE fmt SPEC -> the per-column scalars every cell below is built with.
  digits_v <- vctrs::vec_recycle(as.integer(digits), nrow(tabs_n))

  # ONE SLOT, ONE INTERVAL: the Woolf log-OR bounds under `or_ci`, this leaf's own otherwise.
  # 16. THE CARRIER BUILD -> one fmt column per value column, from the spec above.
  or_from_leaf <- !is.null(tabs_or_ci_inf)
  mat_cols     <- function(M) lapply(seq_len(ncol(M)), function(j) M[, j])
  ci_inf_1     <- if (or_from_leaf) tabs_or_ci_inf else
                  if (!is.null(ci_res$inf))    mat_cols(ci_res$inf)    else list(NA_reals)
  ci_sup_1     <- if (or_from_leaf) tabs_or_ci_sup else
                  if (!is.null(ci_res$sup))    mat_cols(ci_res$sup)    else list(NA_reals)
  ci_pvalue_1  <- if (or_from_leaf) tabs_or_pvalue else
                  if (!is.null(ci_res$pvalue)) mat_cols(ci_res$pvalue) else list(NA_reals)

  tabs <-
    list(tabs_n,
         if (!is.null(tabs_wn)) { tabs_wn   } else { list(NA_reals) },
         if (!is.null(tabs_pct)) { tabs_pct  } else { list(NA_reals) },
         if (!is.null(tabs_diff)) { tabs_diff } else { list(NA_reals) },
         if (!is.null(tabs_mean)) { tabs_mean } else { list(NA_reals) },
         if (!is.null(tabs_rr)) { tabs_rr   } else { list(NA_reals) },
         if (!is.null(tabs_or)) { tabs_or   } else { list(NA_reals) },

         totcol_vector,
         if (!is.null(refcols_vector)) { refcols_vector } else {
           rep(FALSE, length(cols)) },
         if (!is.null(tabs_totn)) { tabs_totn } else { list(NA_reals) },
         ci_inf_1, ci_sup_1, ci_pvalue_1,
         if (!is.null(tabs_neff)) { tabs_neff } else { list(NA_reals) }
    ) |>
    # `ratio` (..5) is the REFERENCE-RELATIVE ratio the colour engine reads; `mean` is NA on a % column.
    purrr::pmap_dfc(function(...) {
      a <- list(...)
      fmt_materialize_col(
        frame = list(
          n         = as.integer(a[[1]]), display = display_1, digits = digits_v,
          wn        = a[[2]], pct = a[[3]], mean = NA_reals, diff = a[[4]], ratio = a[[5]],
          ctr       = NA_reals, var = NA_reals, ci_inf = a[[11]], ci_sup = a[[12]],
          pvalue    = a[[13]], or = a[[7]], tot_n = a[[10]], n_eff = a[[14]],
          row_kind  = kind_vector, in_tottab = tottab_vector, in_refrow = refrows),
        meta  = list(
          scale     = scale_1, comp_all = comp_1, ref = ref_1,
          # WHICH engine built these bounds. All-NA bounds (a degenerate 2x2) name no method.
          ci_method = if (or_from_leaf) { if (!all(is.na(a[[11]]))) "woolf" else "" }
                      else ci_res$method,
          pct_type  = base_1, col_var = colvar_1,
          totcol    = a[[8]], refcol = a[[9]], color = color_1, color_signif = "ignore")
      )
    })

  # 17. THE LABEL COLUMNS, and the ONE rename "Total" -> total_names (leaf_rename_totals).
  tabs <- dplyr::bind_cols(tibble::as_tibble(tabs_text), tabs)

  tabs <- leaf_rename_totals(tabs, row_var, tab_vars, tot, total_names, totaltab, totaltab_name,
                             tottab_vector, totrow_vector)


  # 18. THE no_col_var SPECIAL CASE -> the lone count/mean column, named as a table of `n`.
  no_col_vars_cols <- get_col_var(tabs) == "no_col_var" #& pct %in% c("row", "col", "all", "all_tabs")
  if (any(no_col_vars_cols) ) {
    tabs <- tabs |>
      dplyr::mutate(n = set_display(.data$n, "n") |> set_count_col() |> as_totcol(FALSE)) |>
      dplyr::relocate("n", .after = tidyselect::last_col())

    if (pct %in% c("row", "col", "all", "all_tabs")) {
      # the total column by its STORED role, never the rendered label -- this runs after the rename.
      tot_nm <- names(tabs)[is_totcol(tabs)]
      tabs <- tabs |>
        dplyr::rename(pct = tidyselect::all_of(tot_nm)) |>
        dplyr::mutate(pct = as_totcol(pct, FALSE))
         } else {
      tabs <- tabs |> dplyr::select(-dplyr::where(is_totcol))
    }

    if (length(wt) != 0) tabs <- tabs |>
        dplyr::mutate(wn = set_display(.data$n, "wn") |> set_count_col()) |>
        dplyr::relocate("wn", .after = tidyselect::last_col() )
  }

  # 19. THE WHOLE-TABLE TEST -> leaf_test (chi2 / ANOVA F, on this leaf's own col_var).
  leaf_test <- NULL
  if (!identical(test, "no")) {
    lt        <- leaf_chi2(tabs, test, comp, row_var, col_var, tab_vars, deff)
    tabs      <- lt$tabs
    leaf_test <- lt$test
  }

  # 20. THE RESULT -> the shared leaf tail (row index, class, inference stamp, df/num extract).
  leaf_finish(tabs, row_var, tab_vars, wt, subtext, inference, unserved, degraded, df, num,
              test = leaf_test)
}


# === SECTION: The leaf's whole-table test, and the tail both leaves share =====================

# leaf_chi2() -- the leaf's own whole-table test + contribution pass, through the SAME
# chi2_write_contrib() / chi2_compute_test() the superseded tab_chi2() step calls: one arithmetic.
# What the leaf saves is the QUESTION -- it knows the metadata the step had to reconstruct.
# WARNING: `comp = "all"` is a LOCAL ungrouping, not a table mutation: a computation step must not
#   decide the table's shape.
#' @keywords internal
#' @noRd
leaf_chi2 <- function(tabs, test, comp, row_var, col_var, tab_vars, deff = NULL) {
  do_ctr  <- identical(test, "ctr")
  calc    <- if (do_ctr) c("ctr", "p") else "p"
  color   <- if (do_ctr) "all" else "no"
  cv      <- rlang::as_name(col_var)
  lev_all <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  if (length(lev_all) == 0L || identical(cv, "no_col_var"))
    return(list(tabs = tabs, test = new_test_tibble()))

  col_vars_levels        <- stats::setNames(list(rlang::syms(lev_all)), cv)
  is_tot                 <- purrr::map_lgl(lev_all, ~ any(is_totcol(tabs[[.x]])))
  col_vars_levels_no_tot <- stats::setNames(list(rlang::syms(lev_all[!is_tot])), cv)
  # `tot_cols` names each level column's TOTAL column (the chi2 marginals); built only when one exists.
  tot_cols               <- if (any(is_tot))
    stats::setNames(rlang::syms(rep(lev_all[is_tot][[1]], length(lev_all))), lev_all) else NULL

  keep  <- dplyr::group_vars(tabs)
  work  <- leaf_test_view(tabs, comp, tab_vars)

  if (do_ctr && any(is_tot))
    work <- chi2_write_contrib(work, calc, comp, color, col_vars_levels,
                               col_vars_levels_no_tot, is_a_mean = FALSE, all_col_tot = FALSE,
                               tot_cols = tot_cols, deff = deff)

  test_tbl <- chi2_compute_test(work, comp, as.character(rlang::as_name(row_var)),
                                col_vars_levels, col_vars_levels_no_tot,
                                is_a_mean = FALSE, all_col_tot = FALSE)

  work <- dplyr::ungroup(work)
  if (length(keep)) work <- dplyr::group_by(work, dplyr::across(dplyr::all_of(keep)))
  list(tabs = work, test = test_tbl)
}


# leaf_test_view() -- the grouping the whole-table test is computed ON: `comp = "all"` drops the
# sub-table grouping, for the COMPUTATION only. Shared, so the leaves cannot answer differently.
#' @keywords internal
#' @noRd
leaf_test_view <- function(tabs, comp, tab_vars) {
  gv <- as.character(tab_vars)
  if (identical(comp, "all") || length(gv) == 0L) dplyr::ungroup(tabs)
  else dplyr::group_by(tabs, dplyr::across(dplyr::all_of(gv)))
}


# leaf_chi2_num() -- the numeric twin: the one-way ANOVA (Welch + classic F) over the mean columns,
# through the same chi2_compute_test(). A numeric col_var is its own single "level".
#' @keywords internal
#' @noRd
leaf_chi2_num <- function(tabs, comp, row_var, col_vars, tab_vars) {
  cvs <- as.character(col_vars)
  cvs <- cvs[cvs %in% names(tabs)]
  if (length(cvs) == 0L) return(new_test_tibble())
  cvl <- stats::setNames(lapply(cvs, function(v) rlang::syms(v)), cvs)
  chi2_compute_test(leaf_test_view(tabs, comp, tab_vars), comp,
                    as.character(rlang::as_name(row_var)), cvl, cvl,
                    is_a_mean = rep(TRUE, length(cvs)), all_col_tot = rep(FALSE, length(cvs)))
}


# leaf_finish() -- the RESULT TAIL both leaves run: declare the row-index columns, decide whether
# the tab_vars survive as groups, wrap in the class with the table's own `spec`, stamp the inference
# facts on every fmt column, and return the fmt table or the raw numbers (`df` / `num`).
#' @keywords internal
#' @noRd
leaf_finish <- function(tabs, row_var, tab_vars, wt, subtext, inference,
                        unserved = FALSE, degraded = FALSE, df = FALSE, num = FALSE,
                        test = NULL, anova = NULL) {
  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                                    ~ length(unique(.)) == 1))

  inf <- leaf_inference(inference, unserved, degraded)

  tabs <- tab_stamp_index(tabs, level = rlang::as_name(row_var),
                          var = rlang::as_name(row_var),
                          tab_vars = purrr::map_chr(tab_vars, rlang::as_name))
  meta <- list(spec = new_spec("crosstab", vars = new_vars_attr(
    wt = if (length(wt) == 0L) NA_character_ else as.character(wt)[1])))
  # `anova` is display intent, so it rides render_extras -- stored only when the caller stated it.
  if (!is.null(anova)) meta$render_extras <- list(anova = as.character(anova)[[1]])

  # WARNING: `test` defaults to new_test_tibble() in new_tab(), never NULL -- a leaf with no test
  # must let that default stand; passing NULL would DROP the empty-tibble attribute every table has.
  tst <- if (is.null(test)) new_test_tibble() else test
  result <- if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext, test = tst, meta = meta) |>
      dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs |> dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext, test = tst, meta = meta)
  }

  result <- tab_stamp_inference(result, inference$conf_level, inf$degf, inf$basis)

  if (df || num) leaf_extract_raw(result, num, row_var) else result
}


# num_total_postprocess() -- the tail num_core() runs after each num_rollup(), in place.
# WARNING: a rollup's key columns come back as plain character and must be coerced in APPEARANCE
#   order with `forcats::as_factor`, never `base::as.factor`, which sorts them and moves "Total".
#   (The MAIN aggregate above DOES use base::as.factor -- its input is already sorted.)
#' @keywords internal
#' @noRd
num_total_postprocess <- function(dt, keys, na, tab_row_names) {
  not_fct <- !purrr::map_lgl(dplyr::select(dt, tidyselect::any_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    dt[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
       .SDcols = names(not_fct)[not_fct]]
  }
  if (identical(na, "keep") && length(keys) != 0) {
    data.table::setorderv(dt, keys, na.last = TRUE
    )[, (keys) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"), .SDcols = keys]
  }
  invisible(dt)
}


# leaf_inference_setup() -- the inference preamble both leaves open with, list2env()'d into each
# core: `conf_level` / `inference_basis`; `design_flat` (a FLAT svydesign(ids = ~1) takes the exact
# algebraic path, no influence matrix); `want_neff`; and `use_raw` -- no `.fine`, or ANY design,
# forces the raw scan, a design-based variance being a function of the OBSERVATIONS.
#' @keywords internal
#' @noRd
leaf_inference_setup <- function(inference, .fine, .by_table) {
  basis     <- inference$basis
  design_on <- identical(basis, "design")
  list(conf_level      = inference$conf_level,
       inference_basis = basis,
       design_on       = design_on,
       design_flat     = design_on && svy_design_is_flat(inference$design),
       want_neff       = !identical(basis, "n"),
       use_raw         = .by_table || is.null(.fine) || design_on)
}


# leaf_inference() -- the inference facts of ONE built table, shared by both leaves and tab_reg().
# The basis is downgraded by what the build FOUND OUT and only it can know: `unserved` (no
# per-observation Sigma w^2) states "n", `degraded` states "design_partial". It feeds
# tab_stamp_inference(), so the facts live on every fmt COLUMN, not in a table attribute.
#' @keywords internal
#' @noRd
leaf_inference <- function(inf, unserved = FALSE, degraded = FALSE) {
  if (!svy_weighted(inf)) return(list(basis = NULL, degf = NULL))
  basis <- inf$basis
  if (identical(basis, "weights") && isTRUE(unserved)) basis <- "n"
  if (identical(basis, "design")  && isTRUE(degraded)) basis <- "design_partial"
  list(basis = basis, degf = inf$degf)
}


# === SECTION: Total rows, and the percentage base =============================================

# leaf_wide_pct() -- the percentages and the `tot_n` base, on numeric matrices: the value matrix
# over leaf_dmat()'s broadcast denominator (then NA/NaN -> 0), and the same broadcast on the
# UNWEIGHTED tabs_n. `dmat` / `grp_last` come back so plain_core reuses the same selector.
#' @keywords internal
#' @noRd
leaf_wide_pct <- function(tabs_n, tabs_wn, pct, tab_vars, cols) {
  nm <- names(cols); n <- nrow(tabs_n); k <- length(nm)
  grp <- if (length(tab_vars) == 0) rep(1L, n) else {
    key <- do.call(paste, c(lapply(tab_vars, function(v) as.character(tabs_n[[v]])), sep = "\r"))
    match(key, unique(key))
  }
  grp_last <- stats::ave(seq_len(n), grp, FUN = max)
  M_pct  <- if (!is.null(tabs_wn)) as.matrix(tabs_wn[, nm, with = FALSE]) else
                                   as.matrix(tabs_n[,  nm, with = FALSE]) * 1.0
  M_totn <- as.matrix(tabs_n[, nm, with = FALSE]) * 1.0
  Dmat <- function(M) leaf_dmat(M, pct, grp_last, n, k)
  P <- M_pct / Dmat(M_pct); P[is.na(P)] <- 0
  Tn <- Dmat(M_totn)
  wb <- function(src, M2) {
    dt <- data.table::copy(src)
    dt[, (nm) := lapply(seq_len(k), function(j) M2[, j])]
    dt
  }
  list(pct   = wb(if (!is.null(tabs_wn)) tabs_wn else tabs_n, P),
       tot_n = wb(tabs_n, Tn),
       grp_last = grp_last, m_pct = M_pct, dmat = Dmat)
}

# THE percentage-base broadcast: which denominator each cell divides by, per `pct` -- row -> the
# row's Total, col -> the group's last (= total) row, all / all_tabs -> that row's / the grand Total.
# Extracted so leaf_wide_pct() and the flat-design variance provably use the SAME base;
# `grp_last <- ave(seq_len(n), grp, max)` reproduces `dplyr::last(.)` = the group's total row.
#' @keywords internal
#' @noRd
leaf_dmat <- function(M, pct, grp_last, n, k) switch(
  pct,
  "row"      = matrix(M[, "Total"],         n, k),
  "col"      = M[grp_last, , drop = FALSE],
  "all"      = matrix(M[grp_last, "Total"], n, k),
  "all_tabs" = matrix(M[n,        "Total"], n, k))


# build_total_rows() / finalize_total_rows() -- the total-TABLE and total-ROW group-sums.
# WARNING: sum with `base::sum()` per split() group, NOT rowsum() or data.table gforce -- their
#   plain-double accumulator drifts 1 ULP from the long-double one, breaking identical().
#   finalize_total_rows() appends the "Total" level to exactly `totvars`, ITS OWN argument, never
#   build_total_rows()'s `keys`, which it only sums by.
#' @keywords internal
#' @noRd
build_total_rows <- function(tabs, keys, tab_row_names, num_cols) {
  n <- nrow(tabs)
  if (length(keys) == 0) { idx <- list(seq_len(n)); kf <- NULL } else {
    key <- do.call(paste, c(lapply(keys, function(v) as.character(tabs[[v]])), sep = "\r"))
    f   <- factor(key, levels = unique(key))
    idx <- split(seq_len(n), f)
    kf  <- as.data.frame(do.call(rbind, strsplit(levels(f), "\r", fixed = TRUE)),
                         stringsAsFactors = FALSE)
    names(kf) <- keys
  }
  summ <- lapply(num_cols, function(cc) {
    col <- tabs[[cc]]; fv <- if (is.integer(col)) integer(1) else numeric(1)
    vapply(idx, function(ii) sum(col[ii], na.rm = TRUE), fv)
  })
  names(summ) <- num_cols
  lab <- lapply(tab_row_names, function(v)
    if (!is.null(kf) && v %in% names(kf)) kf[[v]] else rep("Total", length(idx)))
  names(lab) <- tab_row_names
  # check.names = FALSE: key names carry special chars that as.data.frame() would otherwise mangle.
  out <- cbind(as.data.frame(lab,  stringsAsFactors = FALSE, check.names = FALSE),
               as.data.frame(summ, stringsAsFactors = FALSE, check.names = FALSE))
  out[, c(tab_row_names, num_cols), drop = FALSE]
}

#' @keywords internal
#' @noRd
finalize_total_rows <- function(tabs, extra, cols_get_total, tab_row_names) {
  for (v in cols_get_total) if (v %in% names(tabs))
    tabs[[v]] <- factor(tabs[[v]], levels = unique(c(levels(tabs[[v]]), "Total")))
  for (v in tab_row_names)
    extra[[v]] <- factor(extra[[v]], levels = levels(tabs[[v]]))
  out <- rbind(tabs, data.table::as.data.table(extra))
  data.table::setorderv(out, tab_row_names)
  out[]
}


# === SECTION: The reference, and the fields derived from it ===================================

# tab_apply_reference() -- from the pct data.table and a reference selector, derive diff
# (cell - ref), ratio (cell / ref), rr / or and the ref-row / ref-col markers; what a
# given `pct` does not compute comes back NULL. Shared verbatim with the jamovi tier-3 re-reference.
# DESIGN: the odds ratio is computed UNCONDITIONALLY on a row / col % table -- its 2x2 is four
#   numbers this sweep already holds -- EXCEPT on the degenerate margin, which has no 2x2 at all (see
#   `degen_or` below). `or_compare` says whether it IS the comparison the table is tested on, which is
#   what gates the BASELINE markers. `ref2` picks the 2x2.
#' @keywords internal
#' @noRd
tab_apply_reference <- function(tabs, tabs_pct, ref, ref2, comp, or_compare, pct,
                                tab_row_names, tab_vars, row_var, tottab_vector, totrow_vector, cols,
                                totcol_vector = names(cols) == "Total",
                                tabs_totn = NULL, tabs_neff = NULL, conf_level = 0.95, stars = FALSE,
                                degf = Inf, dichotomise = FALSE) {
  nm <- names(cols)
  n  <- nrow(tabs_pct)
  k  <- length(nm)
  P  <- as.matrix(tabs_pct[, nm, with = FALSE]) * 1.0
  # WARNING: which column is the TOTAL is a DECLARED fact passed in, never the literal `nm ==
  # "Total"` -- the leaf's pre-rename convention, wrong for the jamovi re-reference's names.
  is_tot_col <- as.logical(totcol_vector)
  if (length(is_tot_col) != k) is_tot_col <- rep_len(FALSE, k)

  tabs_diff <- data.table::copy(tabs_pct)
  tabs_mean <- data.table::copy(tabs_pct)
  refrows   <- NULL
  tabs_rr <- NULL; tabs_or <- NULL; or_cells <- NULL; refcols_vector <- NULL; or_refrows <- NULL

  set_cols <- function(dt, M2) dt[, (nm) := lapply(seq_len(k), function(j) M2[, j])]

  # per-comp-group FIRST reference-row absolute index (NA -> P[NA, ] is an all-NA row).
  comp_group <- if (comp == "tab") as.character(tab_vars) else character()
  grp_comp   <- if (length(comp_group) != 0) {
    do.call(paste, c(lapply(comp_group, function(v) as.character(tabs[[v]])), sep = "\r"))
  } else rep(1L, n)
  ref_abs <- function(refr) {
    out <- rep(NA_integer_, n)
    for (rows in split(seq_len(n), grp_comp)) {
      p <- which(refr[rows])[1]
      if (!is.na(p)) out[rows] <- rows[p]
    }
    out
  }

  if (pct == "row") {

    refrows <- tabs |>
      calculate_refrows(ref           = ref,
                        comp          = comp,
                        tab_row_names = tab_row_names,
                        tab_vars      = tab_vars,
                        row_var       = row_var,
                        tottab_vector = tottab_vector,
                        totrow_vector = totrow_vector,
                        num_names     = names(cols)
      )

    ra   <- ref_abs(refrows)
    Pref <- P[ra, , drop = FALSE]
    set_cols(tabs_diff, P - Pref)
    set_cols(tabs_mean, P / Pref)   # with pct, tabs_mean is the *2 rule ratio, not a difference


    # CUMULATIVE odds ratio: one cut point per column ("at or below level j"), row i against the
    # reference row -- k levels give k-1 cuts, so the last column is empty by construction. A new
    # DICHOTOMISATION, not a new measure: ci_or() and the `odds_ratio` break scale are reused.
    # WARNING: the `na = "keep"` column is excluded -- appended AFTER the real levels, and "at or
    # below NA" is not a cut point.
    if (ref2 == "cumulative") {
      lv <- which(!is_tot_col & nm != "NA")
      Pc <- matrix(NA_real_, n, k)
      if (length(lv) >= 2L) {
        U <- upper.tri(matrix(0, length(lv), length(lv)), diag = TRUE) * 1     # the cumulator
        Pc[, lv] <- P[, lv, drop = FALSE] %*% U
        Pc[, lv[length(lv)]] <- NA_real_                       # the degenerate last cut
      }
      Oc <- Pc / (1 - Pc)                                      # cumulative odds
      tabs_rr <- data.table::copy(tabs_pct)
      set_cols(tabs_rr, Oc)
      tabs_or <- data.table::copy(tabs_pct)
      set_cols(tabs_or, Oc / Oc[ra, , drop = FALSE])
      refcols_vector <- rep(FALSE, k)      # no reference COLUMN: every column is its own cut, ref2 unused
      or_cells <- function(N) {
        A <- Pc * N; B <- (1 - Pc) * N
        list(a = A, b = B, c = A[ra, , drop = FALSE], d = B[ra, , drop = FALSE])
      }

    } else {

      # PER-COLUMN reference index: a BINARY col_var takes each level against the OTHER (reciprocals,
      # ref2 unused); with 3+ levels every column references ref2, which then shows OR = 1.
      ridx0   <- diff_index(ref2, row_var = dplyr::pull(tabs_pct, !!row_var),
                            num_names = nm, pct = "col", is_total = is_tot_col)
      ok_ref2 <- length(ridx0) != 0 && !is.na(ridx0) && ridx0 >= 1L && ridx0 <= k
      lv      <- which(!is_tot_col)
      binary  <- length(lv) == 2L
      # `levels = "first"` shows one level against the merged rest, a true dichotomy: tab() merges
      # before the leaf, the jamovi path DEFERS it (the test must see every level).
      dich <- isTRUE(dichotomise) && !binary && length(lv) >= 3L

      if (binary || ok_ref2 || dich) {
        ref_col_idx <- rep(if (ok_ref2) as.integer(ridx0) else NA_integer_, k)
        if (binary) { ref_col_idx[lv[1]] <- lv[2]; ref_col_idx[lv[2]] <- lv[1] }
        Pref_col <- P[, ref_col_idx, drop = FALSE]
        # the merged "rest" column does not exist yet on the deferred path: in a row base it IS 1 - p.
        if (dich) {
          Pref_col[, lv] <- 1 - P[, lv, drop = FALSE]
          ref_col_idx[lv] <- NA_integer_
        }
        RR <- P / Pref_col
        or_cells <- function(N) {
          PN <- P * N; PrefN <- Pref_col * N
          list(a = PN, b = if (dich) PrefN else PN[, ref_col_idx, drop = FALSE],
               c = (P * N)[ra, , drop = FALSE],
               d = if (dich) PrefN[ra, , drop = FALSE]
                   else ((P * N)[ra, , drop = FALSE])[, ref_col_idx, drop = FALSE])
        }
      } else {
        warning(paste0(
          "in ref2 = '", ref2, "' , no columns were found as reference for comparison ; ",
          "to remove this warning, precise the value of ref ",
          "until there is one column matched"
        ))
        ref_col_idx <- rep(NA_integer_, k)
        RR <- matrix(NA_real_, n, k)
      }
      # `refcol` means "THE reference of the comparison in force" -- true of ref2 only under an OR.
      refcols_vector <- or_compare & !is.na(ref_col_idx) & ref_col_idx == seq_len(k)

      tabs_rr <- data.table::copy(tabs_pct)
      set_cols(tabs_rr, RR)

      tabs_or <- data.table::copy(tabs_pct)
      set_cols(tabs_or, RR / RR[ra, , drop = FALSE])
    }

  }


  if (pct == "col") {
    refcols <- dplyr::nth(names(cols), diff_index(ref,
                                                  num_names = nm,
                                                  pct       = pct,
                                                  is_total  = is_tot_col))
    refcols_vector <- names(cols) == refcols

    if (length(refcols) != 0 & !is.na(refcols)) {
      set_cols(tabs_diff, P - P[, refcols])
      set_cols(tabs_mean, P / P[, refcols])   # *2 rule ratio
    } else {
      warning(paste0(
        "in ref = '", ref, "' , no columns were found as reference for comparison ; ",
        "to remove this warning, precise the value of ref ",
        "until there is one column matched"
      ))
      set_cols(tabs_diff, matrix(NA_real_, n, k))
      set_cols(tabs_mean, matrix(NA_real_, n, k))
    }


    {

      # the ref2 ROW is the odds ratio's baseline, exported as `refrows` only when the OR is the
      # comparison -- on a col% DIFFERENCE table the reference is a column.
      or_refrows <- tabs |>
        calculate_refrows(ref           = ref2,
                          comp          = comp,
                          tab_row_names = tab_row_names,
                          tab_vars      = tab_vars,
                          row_var       = row_var,
                          tottab_vector = tottab_vector,
                          totrow_vector = totrow_vector,
                          num_names     = names(cols)
        )
      if (or_compare) refrows <- or_refrows
      ra <- ref_abs(or_refrows)
      RR <- P / P[ra, , drop = FALSE]
      tabs_rr <- data.table::copy(tabs_pct)
      set_cols(tabs_rr, RR)

      tabs_or <- data.table::copy(tabs_pct)
      if (length(refcols) != 0 & !is.na(refcols)) {
        set_cols(tabs_or, RR / RR[, refcols])
      } else {
        set_cols(tabs_or, matrix(NA_real_, n, k))
      }
      ref_col_idx <- rep(which(refcols_vector)[1], k)
      if (!is.na(ref_col_idx[1])) or_cells <- function(N) {
        PN <- P * N
        list(a = PN, b = PN[, ref_col_idx, drop = FALSE],
             c = (P * N)[ra, , drop = FALSE],
             d = ((P * N)[ra, , drop = FALSE])[, ref_col_idx, drop = FALSE])
      }
    }
  }

  # DESIGN: THE DEGENERATE MARGIN HAS NO ODDS RATIO. An odds ratio needs a 2x2, and on the margin the
  #   percentage sums TO -- the Total column under pct = "row", the Total row under pct = "col" -- one
  #   cell of it is the whole block: the odds are 1/0 and what the sweep would divide compares nothing.
  #   Blanked at the one place it is written, so the colour engine, the tooltip, the legend and the
  #   honest-total test all see an empty field rather than a number that means nothing. (The
  #   `ref2 == "cumulative"` arm never filled it, so the three arms now agree.)
  degen_or <- matrix(FALSE, n, k)
  if (identical(pct, "row")) degen_or[, is_tot_col] <- TRUE
  if (identical(pct, "col")) {
    tr <- as.logical(totrow_vector)
    if (length(tr) == n) degen_or[!is.na(tr) & tr, ] <- TRUE
  }
  if (!is.null(tabs_or) && any(degen_or)) {
    O <- as.matrix(tabs_or[, nm, with = FALSE]) * 1.0
    O[degen_or] <- NA_real_
    set_cols(tabs_or, O)
  }

  # Woolf log-OR Wald interval for the empirical odds ratio, only when a colour policy or stars asks
  # for it. Its 2x2 is WEIGHTED P x UNWEIGHTED base N, so the totals cancel; ci_or() is the engine.
  or_ci_inf <- or_ci_sup <- or_pvalue <- NULL
  # `or_refrows` is the odds ratio's OWN row baseline; the exported `refrows` may be NULL.
  if (is.null(or_refrows)) or_refrows <- refrows
  if (!is.null(tabs_totn) && !is.null(tabs_or) && !is.null(or_cells) && !is.null(or_refrows)) {
    N  <- as.matrix(tabs_totn[, nm, with = FALSE]) * 1.0
    if (!is.null(tabs_neff)) {
      Ne <- as.matrix(tabs_neff[, nm, with = FALSE]) * 1.0
      N[is.finite(Ne)] <- Ne[is.finite(Ne)]
    }
    # the 2x2 comes from the ARM that built the odds ratio, so this block has no branch of its own.
    cl <- or_cells(N)
    oc <- ci_or(as.vector(cl$a), as.vector(cl$b), as.vector(cl$c), as.vector(cl$d),
                conf_level = conf_level, want_p = isTRUE(stars), df = degf)
    OINF <- matrix(oc$inf, n, k); OSUP <- matrix(oc$sup, n, k); OPV <- matrix(oc$pvalue, n, k)
    # No interval on a reference position (OR = 1 there): the ref row, and any self-referencing column.
    rrm <- !is.na(or_refrows) & or_refrows
    OINF[rrm, ] <- NA_real_; OSUP[rrm, ] <- NA_real_; OPV[rrm, ] <- NA_real_
    if (!is.null(refcols_vector) && any(refcols_vector)) {
      OINF[, refcols_vector] <- NA_real_; OSUP[, refcols_vector] <- NA_real_; OPV[, refcols_vector] <- NA_real_
    }
    OINF[degen_or] <- NA_real_; OSUP[degen_or] <- NA_real_; OPV[degen_or] <- NA_real_
    or_ci_inf <- data.table::copy(tabs_pct); set_cols(or_ci_inf, OINF)
    or_ci_sup <- data.table::copy(tabs_pct); set_cols(or_ci_sup, OSUP)
    or_pvalue <- data.table::copy(tabs_pct); set_cols(or_pvalue, OPV)
  }

  list(
    diff           = tabs_diff,
    ratio          = tabs_mean,
    rr             = tabs_rr,
    or             = tabs_or,
    or_ci_inf      = or_ci_inf,
    or_ci_sup      = or_ci_sup,
    or_pvalue      = or_pvalue,
    refcols_vector = refcols_vector,
    refrows        = refrows
  )
}


# === SECTION: The factor leaf's confidence interval ===========================================

# leaf_ci_plain() -- THE factor leaf's confidence interval, on matrices, FROM THE PLAN: tab_ci()'s
# per-cell arithmetic with the plan RECONSTRUCTION removed, the leaf already knowing `pct`, `ci`,
# `ci_scale`, `comp`, its reference row and its reference column.
# DESIGN: it lives BESIDE tab_apply_reference(), not inside it -- `ci = "cell"` needs no reference
#   and must run when ref == "no", outside that function's gate. "One cell, one interval" still
#   holds: the RULE is one (CI_GEOMS, R/tab-agg.R), and `or_ci` / `ci` cannot co-occur.
# @param P,tot_n n x k: the WEIGHTED proportion and its RAW unweighted base; `n_eff` the effective
#   base (NULL on basis "n"); `ci` / `ci_scale` / `pct` already resolved; `grp` each row's comparison
#   group; `ref_row` the reference ROW mask; `totrow` the total rows (diff_col reads the reference
#   column's base THERE); `refcol` the reference COLUMN index (NA when none); `totcol` the total cols.
# @return list(kind, inf, sup, pvalue, scale, method, visible) -- three n x k matrices plus three
#   COLUMN-INVARIANT stamps (`scale` NA = the level scale stands).
leaf_ci_plain <- function(P, tot_n, n_eff = NULL, ci, pct, ci_scale = "diff",
                          grp, ref_row, totrow, refcol = NA_integer_, totcol,
                          conf_level = 0.95, stars = FALSE,
                          ci_method = default_ci_method(), degf = Inf) {
  n <- nrow(P); k <- ncol(P)
  none <- list(kind = "no", inf = NULL, sup = NULL, pvalue = NULL,
               scale = NA_character_, method = "", visible = FALSE)

  # (a) THE DIRECTION. `pct_type` / `var_kind` being column-INVARIANT here, tab_ci()'s branches collapse.
  kind <- if (length(ci) == 0L || is.na(ci[1]) || identical(ci[1], "no") || identical(pct, "no")) "no"
          else if (identical(ci[1], "cell"))                                          "cell"
          else if (identical(pct, "row"))                                             "diff_row"
          else if (identical(pct, "col"))                                             "diff_col"
          else                                                                        "no"
  if (identical(kind, "no")) return(none)

  kind_base <- if (identical(kind, "cell")) "cell" else "diff"

  # (b) THE REFERENCE ROW, per comparison group.
  # WARNING: LAST-in-group semantics, deliberately, NOT the FIRST that tab_apply_reference()'s own
  # ref_abs() takes -- they coincide on every reachable shape, but this is how the CI is written.
  grp_last <- function(mask) {
    pos <- rep(NA_integer_, n)
    for (g in unique(grp)) { r <- which(grp == g); w <- which(mask[r])
                             if (length(w)) pos[r] <- r[[w[[length(w)]]]] }
    pos
  }
  rp    <- grp_last(ref_row)
  rtona <- !is.na(rp) & (seq_len(n) == rp)     # the cell's own reference row: no interval against itself

  # (c) THE BASES: the effective n where populated, else the raw base -- which `n_raw` keeps as is.
  B <- tot_n * 1.0
  if (!is.null(n_eff)) { ok <- is.finite(n_eff); B[ok] <- n_eff[ok] }
  # a reference row has no CI against ITSELF, but a `ci = "cell"` interval is not a contrast -- it
  # compares each cell to 0 %, so it stands. ONE declared fact (CI_GEOMS$ref_cell), shared with num_core().
  X <- B
  if (identical(ci_geom_ref_cell(kind_base, "pct", ci_scale[1]), "na")) X[rtona, ] <- NA_real_

  REF <- REF_N <- NULL
  if (identical(kind, "diff_row")) {
    REF   <- P[rp, , drop = FALSE]
    REF_N <- B[rp, , drop = FALSE]
  } else if (identical(kind, "diff_col")) {
    if (is.na(refcol)) refcol <- 1L      # detect_refcol()'s fallback: the group's FIRST column
    # WARNING: the reference column's PROPORTION is read in the cell's own row, but its BASE at the
    # group's TOTAL row. Under pct = "col" the raw base is constant down a column, so getting this
    # backwards is invisible unweighted -- and wrong on every design-based col-% table.
    tr    <- grp_last(totrow)
    REF   <- P[, rep(refcol, k), drop = FALSE]
    REF_N <- B[tr, rep(refcol, k), drop = FALSE]
  }

  res <- ci_dispatch(
    kind = kind_base, var_kind = "pct", ci_scale = ci_scale[1],
    est = as.vector(P), base = as.vector(X),
    ref = if (!is.null(REF)) as.vector(REF), ref_n = if (!is.null(REF_N)) as.vector(REF_N),
    n_raw = as.vector(tot_n),
    conf_level = conf_level, want_p = isTRUE(stars), method = ci_method, degf = degf)

  INF <- matrix(res$inf, n, k); SUP <- matrix(res$sup, n, k); PV <- matrix(res$pvalue, n, k)

  # (d) THE MASKING: NA bounds are the data fact saying "no interval in this cell".
  if (identical(kind, "diff_row") && any(totcol)) {
    INF[, totcol] <- NA_real_; SUP[, totcol] <- NA_real_; PV[, totcol] <- NA_real_
  }
  if (identical(kind, "diff_col")) {
    INF[, refcol] <- NA_real_; SUP[, refcol] <- NA_real_; PV[, refcol] <- NA_real_
  }

  list(kind = kind, inf = INF, sup = SUP, pvalue = PV,
       scale   = ci_geom_scale( kind_base, "pct", ci_scale[1]),
       method  = ci_geom_method(kind_base, "pct", ci_scale[1], ci_method),
       visible = identical(kind, "cell"))
}






# === SECTION: The numeric leaf -- tab_num() / num_resolve() / num_core() ======================

#' Means table
#' @description Cross categorical variables with numeric variables, and get a table
#' of means and standard deviations.
#' @eval tab_args_rd("tab_num")
#' @param ... Every other argument of [tab()] -- `color`, `ci`, `tot`, `digits`, ... -- passed
#'   by name. See [tab()]; a typo gets a suggestion.
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}. If \code{...} (\code{tab_vars})
#'  are provided, a \code{tab} of class \code{tabxplor_grouped_tab}.
#' All non-text columns are \code{\link{fmt}} vectors of class \code{tabxplor_fmt},
#' storing all the data necessary to print formats and colors. Columns with \code{row_var}
#' and \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples
#' \donttest{
#' data <- dplyr::storms |> tab_prepare(category, wind, na_drop_all = wind)
#' tab_num(data, category, wind, tot = "row",
#'         color = "difference", color_signif = "guaranteed_effect")
#' }
tab_num <- function(data, row_var, col_vars, tab_vars, wt, ...,
                    num = FALSE, df = FALSE, .fine = NULL, .by_table = FALSE
) {
  .d <- rlang::list2(...)
  tab_check_dots(.d, "tab_num")
  list2env(tab_dots_expand(.d, "tab_num"), environment())

  svy       <- svy_unwrap_data(data, "tab_num")
  if (!is.null(svy)) data <- svy$data
  .a <- tab_resolve_common_args(
    "tab_num", color = color, color_signif = color_signif, ci = ci, stars = stars,
    conf_level = conf_level, ci_method = ci_method, display = display, ref = ref,
    tot = tot, na = na[1], comp = comp[1], totaltab = totaltab,
    total_names   = .d$total_names,
    totaltab_name = .d$totaltab_name,
    other_level   = .d$other_level,
    anova = anova, user_env = rlang::caller_env())
  ci_method <- .a$ci_method ; stars <- .a$stars ; display <- .a$display ; ref <- .a$ref
  total_names <- .a$total_names ; na <- .a$na ; comp <- .a$comp
  color_spec <- .a$color_spec ; color <- .a$color
  conf_level <- .a$conf_level ; totaltab_name <- .a$totaltab_name

  .v <- leaf_defuse_vars(data, rlang::enquo(row_var), rlang::enquo(col_vars),
                         rlang::enquo(tab_vars), rlang::enquo(wt), svy = svy, plural = TRUE)
  data <- .v$data ; row_var <- .v$row_var ; col_vars <- .v$col ; tab_vars <- .v$tab_vars
  wt   <- .v$wt   ; pos_col_vars <- .v$pos_col_vars


  vctrs::vec_assert(ref, size = 1)
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), length(col_vars))
  vctrs::vec_assert(totaltab_name, size = 1)
  # a mean column can carry only a measure whose declared `applies_to` includes "num".
  stopifnot(color %in% c("auto", "no", "") || measure_applies(color, "num"))

  # DESIGN: the gated forcing (a `color_signif` policy needs its interval) is applied HERE too, the
  # same rule as in tab_resolve_settings(), so the two paths cannot drift.
  r_ci  <- resolve_leaf_ci(ci, color, color_spec$signif, stars, ref)
  stars <- r_ci$stars ; color_spec$signif <- r_ci$color_signif
  ci    <- if (identical(r_ci$ci, "ref")) "diff" else r_ci$ci
  ci_scale <- if (identical(measure_key(color_spec$text), "ratio") ||
                  identical(measure_key(color), "ratio") ||
                  identical(display_comparison(display), "ratio")) "ratio" else "diff"
  r <- num_resolve(color, ref, ci, tot, comp, totaltab, row_var, col_vars, tab_vars)
  result <- num_core(
    data, row_var, col_vars, tab_vars, wt,
    color = r$color, na = na, ref = r$ref, comp = r$comp, ci = r$ci, ci_visible = r$ci_visible,
    stars = stars, ci_scale = ci_scale, totaltab = r$totaltab,
    totaltab_name = totaltab_name, tot = r$tot, total_names = total_names, subtext = subtext,
    digits = digits, num = num, df = df, .fine = .fine, .by_table = .by_table,
    inference = new_inference(wt, svy$spec, conf_level, ci_method, design_effect = design_effect),
    anova = anova
  )

  if (df || num) return(result)

  # the shared colour tail -- the same one tab() / tab_counts() run: one grammar, one applier.
  finalize_color_tail(result, color_spec, color_breaks, display)
}


# num_resolve() -- the numeric leaf's FORCING cascade (colour-auto -> ci -> ref -> tot -> comp ->
# totaltab), shared with tab_transform(). `ref = "auto"` is a mean's total row, always.
#' @keywords internal
#' @noRd
num_resolve <- function(color, ref, ci, tot, comp, totaltab, row_var, col_vars, tab_vars) {
  # the means arm of the shared colour-auto cascade: a mean has no contrib / OR notion.
  color <- resolve_color_auto_num(color, ref, ci, row_var, col_vars)

  if (is_placeholder_var(row_var) | any(is_placeholder_var(col_vars))) color <- ""

  # "a comparison colour needs a reference": this leaf warns and repairs where the pipeline aborts.
  needs_ref <- measure_forces(color, "ref")
  if (needs_ref & ref %in% c("no", "")) {
    warning("since color = 'diff', ref must be provided and was set to 'tot'")
    ref <- "tot"
  }

  if (is.null(ci)) ci <- "no"

  if (ci == "diff" & ref %in% c("no", "")) {
    warning("since ci = 'diff', a diff was added with ref = 'tot'")
    ref <- "tot"
  }

  ci_visible <- ci == "cell"


  if (is.null(tot)) {
    tot <- if (ref == "tot" & needs_ref) {"row"} else {"no"}

  } else {
    # ... and only the total ROW on the numeric leaf, which has no total-column notion.
    if (tot[1] == "both") tot <- "row"

    if ((needs_ref | ref == "tot") & !tot %in% "row") {
      tot <- "row"
    }
  }

  if (ref == "auto") {
    ref <- "tot"
  }

  comp <- force_comp(comp, tab_vars)

  if (length(tab_vars) == 0) totaltab <- "no"

  if (comp[1] == "all" & ref == "tot" & !totaltab %in% c("table", "line")) {
    warning("since comp = 'all', a total table was added to compare with")
    totaltab <-  "line"
  }

  if (comp[1] == "all" & !ref %in% c("tot", "no", "") & totaltab != "table") {
    warning("since comp = 'all', a full total table was added to compare with")
    totaltab <- "table"
  }

  list(color = color, ref = ref, ci = ci, ci_visible = ci_visible,
       tot = tot, comp = comp, totaltab = totaltab)
}


# num_digits_floor() -- THE mean-magnitude digits floor: a column of small means needs more decimals
# than `digits` asks for, or it prints as a wall of zeroes. ONE rule, two callers -- num_core() and
# the jamovi tier-4 re-paint, which must reproduce it on a cached carrier.
# WARNING: an all-NA col_var makes every mean NA, so max(na.rm = TRUE) warns and returns -Inf.
#' @keywords internal
#' @noRd
num_digits_floor <- function(digits, means) {
  m <- suppressWarnings(max(means, na.rm = TRUE))
  if (!is.finite(m)) m <- 0
  if      (m <= 1 ) max(digits, 2L)
  else if (m <= 10) max(digits, 1L)
  else              digits
}


# num_core() -- the numeric leaf's compute core. Consumes ALREADY-RESOLVED settings (num_resolve)
# plus the resolved NSE syms, and returns PRE-FINALISE -- colour is finalised by the caller.
#' @keywords internal
#' @noRd
num_core <- function(data, row_var, col_vars, tab_vars, wt,
                     color, na, ref, comp, ci, ci_visible, stars, ci_scale,
                     totaltab, totaltab_name, tot, total_names,
                     subtext, digits, num, df, .fine, .by_table,
                     inference,                            # REQUIRED -- see plain_core()
                     anova = NULL) {

  list2env(leaf_inference_setup(inference, .fine, .by_table), environment())
  des_rows          <- NULL
  # the DESIGN's df REPLACES the sample-based df of every mean pivot: survey refers a design-based
  # mean interval to t(degf), never to t(n-1).
  degf      <- inference$degf

  tab_row_names <- purrr::map_chr(c(tab_vars, row_var), rlang::as_name)

  # the effective n is the EXACT flat closed form or the design variance, never Kish.
  # WARNING: unlike the factor leaf, `use_raw` under a design is a real change of PATH here (the
  # numeric aggregate is normally adopted) but not of VALUES: both branches call num_moment_scan().

  if (use_raw) {
    data <- data |> tab_apply_val_labels(purrr::map_chr(c(tab_vars, row_var), rlang::as_name))
    data <- data |>
      dplyr::select(!!!tab_vars, !!row_var, !!!col_vars, !!wt,
                    tidyselect::any_of(if (design_on) svy_row_col else character())) |>
      dplyr::mutate(dplyr::across((!!wt | tidyselect::all_of(as.character(col_vars))) &
                                    !where(is.numeric), as.numeric)
      )

    data.table::setDT(data)

    # Remove NA's in factors here, otherwise they are kept in totals after
    if (na == "drop") data <- stats::na.omit(data, tab_row_names) # 0.5 sec

    if (design_on) { des_rows <- data[[svy_row_col]]; data[, (svy_row_col) := NULL] }

    if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop')")
  } else if (nrow(.fine) == 0) {
    stop("data is of length 0 (possibly after filter or na = 'drop')")
  }

  if (!use_raw) {
    # copy(): the coercion and relabel below mutate by reference, so a cached `.fine` must survive.
    tabs <- data.table::copy(.fine)

  } else {
    # sufficient moment sums (n [, wn], s1, s2) in ONE grouped pass; they are ADDITIVE, so the total
    # rows and total table below are num_rollup()s of this aggregate, not extra N-scans.
    tabs <- num_moment_scan(data, tab_row_names, col_vars, wt)
  }

  # the flat design's nPSU PER col_var, read off the AGGREGATE rather than nrow(data), so the raw
  # scan and an adopted `.fine` give the same number.
  n_obs_v <- vapply(as.character(col_vars),
                    function(v) sum(as.double(tabs[[paste0(v, "_n")]]), na.rm = TRUE), numeric(1))

  not_fct <- !purrr::map_lgl(dplyr::select(tabs, tidyselect::any_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    tabs[, names(not_fct)[not_fct] := purrr::map(.SD, as.factor),
         .SDcols = names(not_fct)[not_fct]]
  }

  # WARNING: gated on an ACTUAL missing value, exactly as plain_core() is. fct_na_value_to_level()
  #   appends the "NA" level unconditionally, so an ungated call gave the numeric leaf a level the
  #   text leaf did not have -- and the two blocks are full_join()ed on this very column, which an
  #   `ordered` factor refuses across two level sets (`na = "drop_all"` hands both leaves "keep").
  if (na == "keep" &&
      any(purrr::map_lgl(dplyr::select(tabs, tidyselect::any_of(tab_row_names)), anyNA))) {
    data.table::setorderv(
      tabs, tab_row_names, na.last = TRUE
    )[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
      .SDcols = tab_row_names]
  }







  moment_cols <- setdiff(names(tabs), tab_row_names)
  main_agg    <- data.table::copy(tabs)

  if ("row" %in% tot | totaltab %in% c("line", "table")) {
    if (length(tab_vars) != 0) {
      group_vars <- c(as.character(tab_vars)) |> purrr::accumulate(~ c(.x, .y))
      group_vars <- c(rev(group_vars), list(character()))
    } else {
      group_vars <- list(character())
    }
    # WARNING: when tot = "no" but a total table is still built, keep ONLY the grand total -- as a
    # length-1 LIST, never the bare vector, which makes map_dfr() iterate zero times.
    if (!"row" %in% tot) group_vars <- group_vars[length(group_vars)]
    # ... and symmetrically: the LAST grouping is the roll-up over every tab_var, i.e. the total
    # TABLE's own line. `totaltab = "no"` asked for no total table, so it is not built -- which is
    # what plain_core() already does, and the two leaves are full_join()ed row by row.
    else if (!totaltab %in% c("line", "table") && length(tab_vars) != 0)
      group_vars <- group_vars[-length(group_vars)]


    tabs_tot <- purrr::map_dfr(
      group_vars,
      ~ num_rollup(
        main_agg,
        by           = .,
        drop_keys    = as.character(c(tab_vars[!tab_vars %in% .], row_var)),
        moment_cols  = moment_cols,
        tab_vars_chr = as.character(tab_vars)
      )
    )

    num_total_postprocess(tabs_tot, intersect(as.character(tab_vars), names(tabs_tot)),
                          na, tab_row_names)

    tabs <- rbind(tabs, tabs_tot)
    data.table::setorderv(tabs, tab_row_names)


  }

  if (totaltab == "table") {

    tabs_totaltab <- num_rollup(
      main_agg,
      by           = as.character(row_var),
      drop_keys    = as.character(tab_vars),
      moment_cols  = moment_cols,
      tab_vars_chr = as.character(tab_vars)
    )

    num_total_postprocess(tabs_totaltab, as.character(row_var), na, tab_row_names)

    tabs <- rbind(tabs, tabs_totaltab)
    data.table::setorderv(tabs, tab_row_names)


  }

  tabs <- num_derive_stats(tabs, col_vars, weighted = length(wt) != 0)

  # --- the per-cell effective base `_en` ---------------------------------------------------------
  # `n_eff` is a PROPERTY OF THE CELL, so it is written whenever the basis asks for one, never only
  # inside the `ci` branch -- gating it there once made tab_num(design) |> tab_ci("cell") disagree
  # with its factor twin. Basis "design" -> s^2 / Var_design(mean); "weights" -> the Sigma w^2
  # closed form; "n" -> nothing written.
  cvs_all <- as.character(col_vars)
  # what the table can actually carry: a hand-supplied `.fine` without the moment sums cannot climb.
  num_served <- design_on ||
    (want_neff && all(paste0(rep(cvs_all, each = 3L), c("_w2", "_w2s1", "_w2s2")) %in% names(tabs)))
  unserved <- identical(inference_basis, "weights") && !num_served
  degraded <- FALSE
  if (want_neff) {
    Vres <- if (design_on && !design_flat)
      svy_var_mean(prep  = svy_var_prep(inference$design, des_rows),
                   keys  = lapply(tab_row_names, function(v) svy_key_chr(tabs[[v]])),
                   n_tab = length(tab_vars),
                   mkeys = lapply(tab_row_names, function(v) svy_key_chr(data[[v]])),
                   xs    = stats::setNames(lapply(cvs_all, function(v) data[[v]]), cvs_all))
    Vm <- Vres$v
    if (design_on && !design_flat && is.null(Vm)) degraded <- svy_var_degraded(Vres$reason)
    for (v in cvs_all) {
      raw_n <- as.double(tabs[[paste0(v, "_n")]])
      has_m <- all(paste0(v, c("_w2", "_w2s1", "_w2s2")) %in% names(tabs))
      data.table::set(
        tabs, j = paste0(v, "_en"),
        value = if (!is.null(Vm)) {
          en <- tabs[[paste0(v, "_var")]] / Vm[, match(v, cvs_all)]
          ifelse(is.finite(en) & en > 0, en, raw_n)
        } else if (has_m) {
          # the flat closed form: the cell IS its own domain here, so B = Sum(w) over its own rows.
          en <- svy_flat_neff_mean(M = tabs[[paste0(v, "_mean")]], s2 = tabs[[paste0(v, "_var")]],
                                   W2 = tabs[[paste0(v, "_w2")]], W2X = tabs[[paste0(v, "_w2s1")]],
                                   W2X2 = tabs[[paste0(v, "_w2s2")]], B = tabs[[paste0(v, "_wn")]],
                                   n_obs = n_obs_v[[v]])
          ifelse(is.finite(en) & en > 0, en, raw_n)
        } else {
          raw_n
        })
    }
  }

  tt <- leaf_totrow_tottab(tabs, row_var, tab_vars)
  totrow_vector <- tt$totrow; tottab_vector <- tt$tottab; kind_vector <- tt$kind
  comp_group <- if (comp == "tab") { as.character(tab_vars) } else { character() }

  if (!ref %in% c("no", "") | ci %in% c("cell", "diff")) {

    refrows <- calculate_refrows(
      tabs, ref = ref, comp = comp, tab_row_names = tab_row_names, tab_vars = tab_vars,
      row_var = row_var, tottab_vector = tottab_vector, totrow_vector = totrow_vector,
      num_names = col_vars
    )

    tabs[, "ref_rows___" := refrows]

    # `diff` is a real DIFFERENCE (cell mean - reference mean); the colour engine reads `ratio`.
    if (!ref %in% c("no", "") ) {
      tabs[, paste0(col_vars, "_diff") := purrr::map(
        rlang::syms(paste0(col_vars, "_mean")),
        ~ eval(.) - dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
      ),
      by = eval(comp_group)]
      tabs[, paste0(col_vars, "_ratio") := purrr::map(
        rlang::syms(paste0(col_vars, "_mean")),
        ~ eval(.) / dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
      ),
      by = eval(comp_group)]
    }




    # the pvalue is the interval's own inversion, NA when stars are off.
    if (ci %in% c("cell", "diff")) {
      stars_on <- resolve_stars(stars)
      want_p   <- isTRUE(stars_on) && ci == "diff"
      cvs      <- as.character(col_vars)

      # `_en` is written ABOVE, once, as a property of the cell; on basis "n" the raw count stands in.
      if (!all(paste0(cvs, "_en") %in% names(tabs)))
        for (v in cvs) data.table::set(tabs, j = paste0(v, "_en"),
                                       value = as.double(tabs[[paste0(v, "_n")]]))

      if (ci == "diff") {
        tabs[, paste0(cvs, "_refm") := purrr::map(
          rlang::syms(paste0(cvs, "_mean")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
        tabs[, paste0(cvs, "_refv") := purrr::map(
          rlang::syms(paste0(cvs, "_var")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
        tabs[, paste0(cvs, "_refn") := purrr::map(
          rlang::syms(paste0(cvs, "_en")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
      }

      # a MODEL-based method pools its dispersion over the variable's whole level set, which the
      # elementwise engines cannot see -- so compute it here, where the level set is the rows.
      # Grouped by the comparison scope (+ the total table, which is a table of its own), over the
      # rows that ARE levels: a total row is a mixture of them, and would be counted twice.
      pool_slot <- if (identical(ci_scale[1], "ratio")) "mean_ratio" else "mean_diff"
      pool_of   <- function(v) {
        if (ci != "diff" ||
            !identical(inference$method[[pool_slot]], CI_POOLED[[pool_slot]])) return(NULL)
        grp <- if (length(comp_group))
                 do.call(paste, c(as.list(tabs[, comp_group, with = FALSE]),
                                  list(tottab_vector), sep = "\r"))
               else paste(tottab_vector)
        ci_pool_disp(n = tabs[[paste0(v, "_en")]], mean = tabs[[paste0(v, "_mean")]],
                     var = tabs[[paste0(v, "_var")]], by = grp, use = !totrow_vector,
                     kind = pool_slot)
      }

      for (v in cvs) {
        m  <- tabs[[paste0(v, "_mean")]]
        vv <- tabs[[paste0(v, "_var")]]
        nn <- tabs[[paste0(v, "_en")]]
        # ONE lookup in CI_GEOMS: a mean CELL interval is the one-sample Student t pivot, a CONTRAST
        # follows `ci_scale` -- a real ratio-of-means interval, or the mean-difference one.
        res <- ci_dispatch(
          kind = ci, var_kind = "mean", ci_scale = ci_scale[1],
          est = m, base = nn, var = vv,
          ref     = if (ci == "diff") tabs[[paste0(v, "_refm")]],
          ref_var = if (ci == "diff") tabs[[paste0(v, "_refv")]],
          ref_n   = if (ci == "diff") tabs[[paste0(v, "_refn")]],
          conf_level = conf_level, want_p = want_p, method = inference$method, degf = degf,
          pool = pool_of(v))
        # A reference row has no CI or test AGAINST ITSELF -- but a `ci = "cell"` interval is not a
        # comparison, so it keeps its own. ONE declared fact (CI_GEOMS$ref_cell), shared with
        # leaf_ci_plain(); only the MECHANISM is local (this leaf NAs the RESULTS, not the base).
        if (identical(ci_geom_ref_cell(if (ci == "diff") "diff" else "cell", "mean", ci_scale[1]),
                      "na")) {
          res$inf[refrows] <- NA_real_
          res$sup[refrows] <- NA_real_
          res$pvalue[refrows] <- NA_real_
        }
        data.table::set(tabs, j = paste0(v, "_ci_inf"), value = res$inf)
        data.table::set(tabs, j = paste0(v, "_ci_sup"), value = res$sup)
        data.table::set(tabs, j = paste0(v, "_pvalue"), value = res$pvalue)
      }

      # basis "n" writes no effective base -- drop the raw-count scratch so it never reaches `n_eff`.
      if (!want_neff) data.table::set(tabs, j = paste0(cvs, "_en"), value = NULL)
      if (ci == "diff")
        data.table::set(tabs, j = paste0(rep(cvs, each = 3L),
                                         c("_refm", "_refv", "_refn")), value = NULL)
    }

    tabs[, "ref_rows___" := NULL]
  }

  w2_cols <- names(tabs)[stringi::stri_detect_regex(names(tabs), "_w2$|_w2s1$|_w2s2$")]
  if (length(w2_cols) > 0) data.table::set(tabs, j = w2_cols, value = NULL)






  text_vars <- !purrr::map_lgl(tabs, is.numeric)
  NA_reals <- rep(NA_real_, nrow(tabs))


  tabs_n  <-
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_n$"), with = FALSE] ,
                         function(.x) stringi::stri_replace_first_regex(.x, "_n$" , ""))

  tabs_wn  <-
    if (length(wt) != 0) {
      data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_wn$"), with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_wn$" , ""))
    } else {
      list(NA_reals)
    }

  tabs_mean  <-
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_mean$"), with = FALSE] ,
                         function(.x) stringi::stri_replace_first_regex(.x, "_mean$" , ""))

  tabs_mean <- tibble::as_tibble(tabs_mean) |>
    dplyr::mutate(dplyr::across(
      where(~ any(is.nan(.))),
      ~ dplyr::if_else(is.nan(.), NA_real_, .)
    )) |>
    data.table::as.data.table()


  # WARNING: tab_num reshapes by column-name SUFFIX (_n/_wn/_mean/_var/_diff/_ci) — fragile.
  # "no_row_var" ends in "_var" and would be mis-detected as a variance column, hence the
  # explicit exclusion. A numeric col_var whose name ends in one of these suffixes would
  # likewise be mis-parsed.
  tabs_var  <-
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_var$") &
                                !is_placeholder_var(names(tabs)),
                              with = FALSE],
                         function(.x) stringi::stri_replace_first_regex(.x, "_var$" , ""))


  are_diff <- stringi::stri_detect_regex(names(tabs), "_diff$")
  tabs_diff  <-
    if (any(are_diff)) {
      data.table::setnames(tabs[, are_diff, with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_diff$" , ""))
    } else {
      list(NA_reals)
    }

  are_ratio <- stringi::stri_detect_regex(names(tabs), "_ratio$")
  tabs_ratio  <-
    if (any(are_ratio)) {
      data.table::setnames(tabs[, are_ratio, with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_ratio$" , ""))
    } else {
      list(NA_reals)
    }

  reshape_suffix <- function(sfx) {
    hit <- stringi::stri_detect_regex(names(tabs), paste0(sfx, "$"))
    if (any(hit)) {
      data.table::setnames(tabs[, hit, with = FALSE],
                           function(.x) stringi::stri_replace_first_regex(.x, paste0(sfx, "$"), ""))
    } else {
      list(NA_reals)
    }
  }
  tabs_ci_inf <- reshape_suffix("_ci_inf")
  tabs_ci_sup <- reshape_suffix("_ci_sup")
  tabs_pvalue <- reshape_suffix("_pvalue")
  # surface the kept effective n into `n_eff`, by EXACT scratch names to dodge the
  # reshape-by-suffix collision the WARNING above flags, then dropped.
  tabs_neff <-
    if (want_neff && all(paste0(as.character(col_vars), "_en") %in% names(tabs))) {
      data.table::setnames(tabs[, paste0(as.character(col_vars), "_en"), with = FALSE],
                           as.character(col_vars))
    } else { list(NA_reals) }

  tabs_text <- tabs[, text_vars, with = FALSE]

  if (ref %in% c("tot", "no", "")) refrows <- rep(FALSE, nrow(tabs))


  # display / ref / comp are column-invariant here; `digits` and `col_var` stay per-column.
  display_1 <- if (ci_visible) { "ci" } else { "mean" }
  # what these columns estimate and WHICH engine built their bounds -- both the CI_GEOMS row above.
  scale_num  <- ci_geom_scale(ci, "mean", ci_scale[1])
  if (is.na(scale_num)) scale_num <- "level_mean"
  method_num <- ci_geom_method(ci, "mean", ci_scale[1], inference$method)
  ref_1     <- switch(as.character(ref), "no" = "", "tot" = "tot", as.character(ref))
  comp_1    <- dplyr::if_else(ref != "no" | ci != "no", comp == "all", NA)
  NA_reals  <- rep(NA_real_, nrow(tabs_n))

  tabs <-
    list(tabs_n, tabs_wn, tabs_mean, tabs_var, tabs_diff, tabs_ci_sup, as.character(col_vars),
         digits, tabs_ratio, tabs_ci_inf, tabs_pvalue, tabs_neff) |>
    purrr::pmap_dfc(function(...) {
      a <- list(...)
      digits_col <- vec_recycle(num_digits_floor(a[[8]], a[[3]]), length(a[[1]]))
      fmt_materialize_col(
        frame = list(
          n         = a[[1]], display = display_1, digits = digits_col,
          wn        = a[[2]], pct = NA_reals, mean = a[[3]], diff = a[[5]], ratio = a[[9]],
          ctr       = NA_reals, var = a[[4]], ci_inf = a[[10]], ci_sup = a[[6]],
          pvalue    = a[[11]], or = NA_reals, tot_n = NA_reals, n_eff = a[[12]],
          row_kind  = kind_vector, in_tottab = tottab_vector, in_refrow = refrows),
        meta  = list(
          # a ratio-scale mean interval lives on `mean_ratio` (neutral 1), so format() reads the
          # ratio bounds, not a difference mislabelled as one.
          scale     = scale_num, comp_all = comp_1, ref = ref_1,
          pct_type  = "none", ci_method = method_num,
          col_var   = a[[7]],
          totcol    = FALSE, refcol = FALSE, color = color, color_signif = "ignore")
      )
    })

  tabs <- dplyr::bind_cols(tibble::as_tibble(tabs_text), tabs)


  tabs <- leaf_rename_totals(tabs, row_var, tab_vars, tot, total_names, totaltab, totaltab_name,
                             tottab_vector, totrow_vector)







  # the shared result tail. Its inference stamp is the ONLY one, so a factor block whose design
  # variance succeeded keeps "design" beside a numeric block that fell back.
  leaf_finish(tabs, row_var, tab_vars, wt, subtext, inference, unserved, degraded, df, num,
              anova = anova)
}
