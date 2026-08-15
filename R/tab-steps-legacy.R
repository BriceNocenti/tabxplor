# PURPOSE: The superseded dplyr-era step functions -- tab_pct()/tab_tot()/tab_totaltab() + their
#   trio-exclusive formula helpers pct_formula()/diff_formula(), and (Phase 19j) tab_ci()/tab_chi2().
# ROLE: Quarantined here out of tab.R's live pipeline -- 17f for the trio, 19j (KEY 5) for the two
#   tests. These are the pre-2.0.0 step-by-step API: exported + superseded (badge, no lifecycle
#   warning), still working on an existing tab, but OFF the tab()/tab_many() aggregate-core path.
#   With 19j the WHOLE pre-2.0.0 chain lives here: nothing in tab.R's build calls a step any more.
# KEY CONSTRAINTS:
#   - Exports unchanged (the @export roxygen travels with the functions; document() keeps NAMESPACE).
#   - They call INTO the shared ARITHMETIC, which stays where the build uses it (chi2_compute_test,
#     chi2_write_contrib, var_contrib_ctr_signed, contrib_pvalue in R/tab-chi2.R; ci_dispatch() /
#     CI_GEOMS in R/tab-agg.R; detect_totcols() in R/fmt_class.R, which the exporters read too).
#     Their own MACHINERY -- the six helpers with no caller outside this file -- moved in here in
#     Phase 19l; see the section at the bottom. Nothing here is called BY the core.
#   - WHAT A WRAPPER IS: a step here RECONSTRUCTS a plan from the table's own fmt markers, because it
#     runs on a table it did not build (tab_get_vars / detect_totcols / detect_refcol /
#     detect_firstcol, the eight-branch ci case_when, the second `ci = "ratio"` fold, the
#     `stars`-from-the-option and `degf`-from-the-columns fallbacks, and the four tab_match_* /
#     tab_add_* passes that MUTATE the table to make the step's own preconditions true). That
#     reconstruction is the whole POINT of these functions and is why it did not die with the
#     pipeline copy -- but the ARITHMETIC is shared: tab_ci() calls the same ci_dispatch() / CI_GEOMS
#     (R/tab-agg.R) the two leaves do, and tab_chi2() the same chi2_compute_test() /
#     chi2_write_contrib() leaf_chi2() does, so a step and a build cannot compute two different
#     answers.
# See: CLAUDE.md Repository Map > R/tab-steps-legacy.R.


#' Add total table to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (2.0.0): the total table is built directly by the `totaltab` argument of
#' [tab()] / [tab_plain()] / [tab_num()]. `tab_totaltab()` still works on an existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param totaltab If there are subtables, corresponding to the levels of tab_vars,
#' \code{totaltab = "table"} add a complete total table.
#' \code{totaltab = "line"} add a total table of only one row with the general total.
#' \code{totaltab = "no"} remove any existing total table.
#' @param name The name of the total table, as a single string.
#' @param data The original database used to calculate the \code{tab} : it is only useful
#' for mean columns (of numeric variables), in order to calculate the variances
#' necessary to calculate confidence intervals with \code{\link{tab_ci}}.
#'
#' @return A \code{tibble} of class \code{tab}. Rows belonging to the total table can then
#' be detected using \code{\link{is_tottab}}.
#' @export
#'
#' @examples \donttest{ data <- dplyr::starwars |>
#' tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'             na_drop_all = sex)
#'
#' data |>
#'   tab_plain(sex, hair_color, gender) |>
#'   tab_totaltab("line")
#'   }
tab_totaltab <- function(tabs, totaltab = c("table", "line", "no"),
                         name = "Ensemble", data = NULL) {
  #.Deprecated("tab_plain() and tab_num(), which now have a totaltab argument")

  get_vars  <- tab_get_vars(tabs)

  row_var   <- rlang::sym(get_vars$row_var)
  tab_vars  <- rlang::syms(get_vars$tab_vars)
  mean_vars <- (fmt_var_kind(tabs) == "mean") |> purrr::keep(\(x) x) |> names()


  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  test    <- get_test(tabs)

  if (length(tab_vars) == 0) return(tabs)

  #Remove the existing total table if there is one
  tottab_rows <- is_tottab(tabs)
  if (any(tottab_rows)) tabs <- tabs |>
    tibble::add_column(tottab = tottab_rows) |>
    dplyr::filter(!.data$tottab) |> dplyr::select(-"tottab")

  if (totaltab[1] == "no") return(tabs)

  #Calculate the total table
  totaltable <- switch(
    totaltab[1],
    "table" = tibble::as_tibble(tabs) |>
      (\(d) tibble::add_column(d, totrow = is_totrow(d)))() |>
      dplyr::filter(!.data$totrow) |> dplyr::select(-"totrow") |>
      dplyr::group_by(!!row_var) |>
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_tottab(sum(.) ))),

    "line"  = tibble::as_tibble(tabs) |>
      (\(d) tibble::add_column(d, totrow = is_totrow(d)))() |>
      dplyr::filter(!.data$totrow) |> dplyr::select(-"totrow") |>
      dplyr::group_by(!!row_var) |>
      dplyr::summarise(dplyr::across(where(is_fmt), sum)) |>
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(as_tottab(sum(.))))) |>
      dplyr::mutate(!!row_var := paste("TOTAL", stringi::stri_trans_toupper(name)))
  )

  if (totaltab[1] == "line") {
    tabs <- tabs |>
      dplyr::mutate(!!row_var := forcats::fct_expand(
        !!row_var,
        levels(dplyr::pull(totaltable, !!row_var))
      ))

    totaltable <- totaltable |>
      dplyr::mutate(!!row_var := forcats::fct_expand(
        !!row_var, levels(dplyr::pull(tabs, !!row_var))
      ))
  }

  totaltable <-
    purrr::reduce(tab_vars, .init = totaltable,
                  .f = ~ dplyr::mutate(.x, !!.y := factor(name)))


  # If there are mean columns, the calculation of variances, necessary to
  #  calculate confidence intervals, needs access to the original database
  if (length(mean_vars) != 0 & !is.null(data)) {

    mean_calc <- switch(
      totaltab[1],
      "table" = purrr::map(mean_vars, ~ tab_plain(data, !!row_var,
                                                  col_var = !!rlang::sym(.))),
      "line" = purrr::map(mean_vars, ~tab_plain(data, col_var = !!rlang::sym(.)))
    )
    mean_calc <-
      purrr::reduce(mean_calc,
                    ~ dplyr::full_join(.x, .y, by = switch(totaltab[1],
                                                           "table" = as.character(row_var),
                                                           "line"  =  "no_row_var") ) ) |>
      dplyr::select(-tidyselect::starts_with("no_row_var")) |>
      dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(.)))

    if (totaltab[1] == "line") mean_calc <- mean_calc |>
      dplyr::mutate(dplyr::across(where(is_fmt), ~ as_totrow(.)))

    totaltable <- switch(
      totaltab[1],
      "table" = dplyr::left_join(dplyr::select(totaltable,
                                               -tidyselect::all_of(mean_vars)),
                                 mean_calc, by = as.character(row_var)),
      "line"  = dplyr::left_join(dplyr::select(totaltable,
                                               -tidyselect::all_of(mean_vars)),
                                 mean_calc, by = character())
    )

    totaltable
  }


  #Bind the total table to the tabs
  if (lv1_group_vars(tabs)) {
    tabs |> dplyr::bind_rows(totaltable)
  } else {

    df <- tabs |> dplyr::bind_rows(totaltable)
    groups <- dplyr::group_data(df)
    new_grouped_tab(df, groups = groups, subtext = subtext, test = test)
  }
}




#' Add totals to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (2.0.0): totals are built directly by [tab()] / [tab_plain()] / [tab_num()] (a
#' total row is always computed, one total column shown). `tab_tot()` still works on an
#' existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param tot \code{c("col", "row")} and \code{"both"} print total rows and total columns.
#'  Set to \code{"row"} or \code{"col"} to print only one type.
#'  Set to \code{"no"} to remove all totals.
#' @param name  The names of the totals, as a character vector of length one or two.
#' Use \code{c("Total_row", "Total_column")} to set different names for rows and cols.
#' @param totcol \code{"last"} only prints a total column for the last factor column
#' variable. Set to \code{"each"} to print a total column for each column variable.
#' @param data The original database used to calculate the \code{tab} : it is only useful
#' for mean columns (of numeric variables), in order to calculate the variances of
#' total rows, necessary to calculate confidence intervals with \code{\link{tab_ci}}.
#'
#' @return A \code{tibble} of class \code{tab}. Total rows can then be detected using
#'  \code{\link{is_totrow}}, and total columns using \code{\link{is_totcol}}.
#' @export
#'
#' @examples \donttest{data <- dplyr::starwars |> tab_prepare(sex, hair_color)
#'
#' data |>
#'   tab_plain(sex, hair_color) |>
#'   tab_tot("col", totcol = "each")
#'   }
tab_tot <- function(tabs, tot = c("row", "col"), name = "Total",
                    totcol = "last", data = NULL) {
  #.Deprecated("tab_plain() and tab_num(), which now have a tot argument")

  stopifnot(
    tot %in% c("no", "row", "col", "both"),
    totcol %in% TAB_ARG_VALUES$totcol$values   # 19m-i: declared (this copy had lost "all_col_vars")
  )

  get_vars        <- tab_get_vars(tabs)
  row_var         <- rlang::sym(get_vars$row_var)
  col_vars_levels_mean <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  mean_vars <- fmt_var_kind(tabs) == "mean"
  col_vars_levels <- purrr::discard(col_vars_levels_mean, names(col_vars_levels_mean) %in% names(mean_vars))
  tab_vars        <- rlang::syms(get_vars$tab_vars)

  groups <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  test    <- get_test(tabs)

  if (any("both" %in% tot)) tot <- c("row", "col")
  name <- vctrs::vec_recycle(name, 2)

  if (length(col_vars_levels) == 0 & "col" %in% tot) {
    warning("can't add a total column without at least one non-mean col_var")
    tot <- dplyr::if_else("row" %in% tot, "row", "no")
  }


  #Remove existing totals, except if there is a total table of one line
  if ("row" %in% tot | tot[1] == "no") {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows #& totrows

    if (any(totrows)) tabs <- tabs |>
      tibble::add_column(totrows, tottab_line) |>
      dplyr::filter(!.data$totrows | .data$tottab_line) |>
      dplyr::select(-"totrows", -"tottab_line")
  }

  if ("col" %in% tot | tot[1] == "no") tabs <- tabs |>
    dplyr::select(-where(is_totcol))

  if (tot[1] == "no") return(tabs)


  # Total rows
  if ("row" %in% tot) {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows #& totrows

    tabs <- tabs |> tibble::add_column(tottab_rows, tottab_line)

    if (length(groups) != 0) {
      group_vars_totals <-
        dplyr::group_keys(dplyr::filter(tabs, !.data$tottab_line)) |> #dplyr::mutate(bis = PR0) |>
        tidyr::unite(!!row_var, sep = " / ") |>
        dplyr::mutate(!!row_var := paste(name[1], !!row_var) |>
                        stringi::stri_trans_toupper() |> forcats::as_factor())  #stringi::stri_replace_all_regex(, "")
    } else {
      group_vars_totals <- tibble::tibble(!!row_var := factor(name[1]))
    }
    group_vars_totals_levels <- group_vars_totals |> dplyr::pull(1) |> levels()

    tabs <- tabs |>
      dplyr::mutate(!!row_var := forcats::fct_expand(!!row_var, group_vars_totals_levels))

    row_var_levels <- dplyr::pull(tabs, !!row_var) |> levels()

    totrows <- tabs |> dplyr::filter(!.data$tottab_line) |>
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(sum(.)) ),
                       .groups = "drop") |>
      dplyr::bind_cols(group_vars_totals) |>
      dplyr::mutate(!!row_var := forcats::fct_expand(!!row_var, row_var_levels))

    #For mean vars, calculate variances based on original datas
    # (necessary to calculate confidence intervals)
    if (any(mean_vars) & !is.null(data)) {
      mean_names <- names(mean_vars[mean_vars])

      mean_calc <-
        purrr::map(mean_names, ~ tab_plain(data, row_var = NA_character_,
                                           col_var = !!rlang::sym(.),
                                           purrr::map_chr(tab_vars, as.character))
        )

      mean_calc <-
        purrr::reduce(mean_calc,~ dplyr::full_join(
          .x, .y,
          by = c(purrr::map_chr(tab_vars, as.character))
        ) ) |>
        dplyr::select(-tidyselect::contains("no_row_var")) |>
        dplyr::mutate(dplyr::across(where(is_fmt), ~ as_totrow(.)))

      general_totrow_condition <- any(tabs$tottab_rows) & !any(tabs$tottab_line)

      if (general_totrow_condition) {
        general_totrow <-
          purrr::map(mean_names,
                     ~ tab_plain(data, row_var = NA_character_,
                                 col_var = !!rlang::sym(.))
          )

        general_totrow <-
          purrr::reduce(general_totrow,
                        ~ dplyr::full_join(.x, .y ,by = character() ) ) |>
          dplyr::select(-tidyselect::starts_with("no_row_var")) |>
          dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(as_totrow(.))))

        general_totrow  <- dplyr::group_keys(tabs) |>
          dplyr::slice(dplyr::n_groups(tabs)) |>
          dplyr::bind_cols(general_totrow)

        mean_calc <- dplyr::bind_rows(mean_calc, general_totrow)
      }

      totrows <- dplyr::left_join(
        dplyr::select(totrows,
                      -tidyselect::all_of(mean_names)),
        mean_calc,
        by = purrr::map_chr(tab_vars, as.character)
      )
    }


    tabs <- dplyr::bind_rows(tabs, totrows) |>
      dplyr::arrange(.by_group = TRUE) |>
      dplyr::select(-"tottab_line", -"tottab_rows")
  }


  #Total columns
  if ("col" %in% tot) {
    col_vars_2levels_or_more <-
      col_vars_levels[purrr::map_int(col_vars_levels, length) >= 2]

    if (length(col_vars_2levels_or_more) != 0 | totcol[1] == "last") {
      tabs <- tabs |> dplyr::rowwise()

      if (totcol[1] == "last") {
        # c_across don't work. Workaround with quosures : sum(!!!col_vars_levels)
        tabs <- tabs |>
          dplyr::mutate(
            !!rlang::sym(name[2]) :=
              sum(!!!col_vars_levels[[length(col_vars_levels)]]) |>
              as_totcol() |> set_col_var("all_col_vars"))

      } else if (totcol[1] == "each") {
        totcol_names <- purrr::map(paste0(name[2],"_",
                                          names(col_vars_2levels_or_more)),
                                   rlang::sym)
        tabs <-
          purrr::reduce2(col_vars_2levels_or_more, totcol_names, .init = tabs,
                         function(.tab, .levels, .names)
                           dplyr::mutate(.tab, !!.names := sum(!!!.levels) |>
                                           as_totcol())
          )
        tabs <-
          purrr::reduce(names(col_vars_2levels_or_more), .init = tabs,
                        function(.tab, .var)
                          dplyr::relocate(
                            .tab,
                            where(~ tidyr::replace_na(get_col_var(.) == .var & is_totcol(.),
                                                      FALSE)),
                            .after = where(~ tidyr::replace_na(get_col_var(.) == .var &
                                                                 !is_totcol(.),
                                                               FALSE)
                            ) ) )
      }

      tabs <- tabs |> dplyr::group_by(!!!rlang::syms(groups))
    }
  }

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, test = test)
  } else {

    group_dat <- dplyr::group_data(tabs)
    new_grouped_tab(tabs, groups = group_dat, subtext = subtext, test = test)
  }
}


# WARNING: For type="mean" columns, diff stores a RATIO (cell_mean/ref_mean), not a
#   difference. This is intentional — mean breaks (1.15, 1.5, 2, 4) are ratio thresholds.
#   For pct columns, diff stores an additive difference (cell_pct - ref_pct).
#' Add percentages and diffs to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (2.0.0): percentages, differences and ratios are computed directly by
#' [tab()] / [tab_plain()] via the `pct` / `ref` arguments. `tab_pct()` still works on an
#' existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param pct The type of percentages to calculate. \code{"row"} draw row percentages.
#' Set to \code{"col"} for column percentages. Set to \code{"all"} for frequencies
#' (based on each subtable/group if \code{tab_vars} is provided).
#' Set to \code{"all_tabs"} to calculate frequencies based on the whole (set of) table(s).
#' @param digits The number of digits to print for percentages. As a single integer,
#' or an integer vector the same length than \code{col_vars}.
#' @param ref The reference cell to calculate differences and ratios
#'  (used to print \code{colors}) :
#'  \itemize{
#'   \item \code{"auto"}: by default, cell difference from the corresponding total
#'   (rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
#'   used for `diff` ; cell ratio from the first line (or col) is use for `OR`
#'   (odds ratio/relative risks ratio).
#'   \item \code{"tot"}: totals are always used.
#'   \item \code{"first"}: calculate cell difference or ratio from the first cell
#' of the row or column (useful to color temporal developments).
#'   \item \code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.
#'   \item \code{"regex"}: when `ref` is a string, it it used as a regular expression,
#'   to match with the names of the rows (or columns). Be precise enough to match only one
#'   column or row, otherwise you get a warning message.
#'   \item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
#' }
#' @param comp Comparison level. When \code{tab_vars} are present, should the row
#' differences be calculated for each subtable/group (by default \code{comp = "tab"} :
#' comparison of each cell to the relative total row) ?
#' Should they be calculated for the whole table (\code{comp = "all"} :
#' comparison of each cell to the total row of the total table) ?
#' When \code{comp = "all"} and \code{ref = "first"}, cells are compared to the first
#' cell of the total table instead.
#' This parameter doesn't affect column percentages.
#' \code{comp} must be set once and for all the first time you use \code{\link{tab_chi2}},
#' \code{\link{tab_pct}} with rows, or \code{\link{tab_ci}}.
#' @param color Set to \code{TRUE} to color the resulting tab based on differences (from
#' totals or from the first cell).
#' @param just_diff If percentages are already calculated and you just want
#' to recalculate differences.
#'
#' @return A \code{tibble} of class \code{tab}, with percentages displayed, possibly
#' colored based on differences from totals or first cell.
#' @export
tab_pct <- function(tabs, pct = "row", #c("row", "col", "all", "all_tabs", "no"),
                    digits = NULL, ref = c("tot", "first", "no"),
                    comp = NULL, color = FALSE, just_diff = FALSE) { #Add keep/change grouping ?

  # .Deprecated("tab_plain() and tab_num(), which now have pct and ref arguments")

  #stopifnot(pct[1] %in% c("row", "col", "all", "all_tabs", "no"))
  get_vars         <- tab_get_vars(tabs)
  col_vars_with_all<- rlang::syms(get_vars$col_vars)
  col_vars_no_all  <- col_vars_with_all |> purrr::discard(\(s) as.character(s) == "all_col_vars")
  col_means  <- (fmt_var_kind(tabs) == "mean") |> purrr::keep(\(x) x) |> names()
  tab_vars         <- rlang::syms(get_vars$tab_vars)

  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  test    <- get_test(tabs)

  pct <- vctrs::vec_recycle(pct, length(col_vars_no_all)) |>
    purrr::set_names(col_vars_no_all)
  pct[col_means] <- "no"

  if (just_diff == FALSE) {

    if (all(pct == "no")) {
      tabs <- tabs |> dplyr::mutate(dplyr::across(
        where(~ get_pct_base(.) != "none"),
        ~ set_pct(., NA_real_) |> set_count_col() |>
          set_display("wn")
      ))
      if (length(col_means) == 0) return(tabs)
    }


    #Ready table for percentages (need total rows and cols, compatible grouping)
    if (any(pct == "all_tabs")) {
      if (length(tab_vars) != 0          &
          !(is_tottab(tabs[nrow(tabs),]) &
            is_totrow(tabs[nrow(tabs),]) &
            any(is_totcol(tabs))) ) {
        warning("since percentages are 'all_tabs', a total table ",
                "was added")
        if (!is_tottab(tabs[nrow(tabs),])) {
          tabs <- tabs |> tab_totaltab('line')
        }
        tabs <- tabs |>
          dplyr::with_groups(NULL, ~ tab_match_groups_and_totrows(.) |>
                               tab_add_totcol_if_no()
          )
      }
    }

    if ( any(pct %in% c("col", "all") ) | (any(pct == "row") & ref[1] == "tot") ) {
      tabs <- tabs |> tab_match_groups_and_totrows()
    }

    if ( any(pct %in% c("row", "all")) | (any(pct == "col") & ref[1] == "tot") ) {
      tabs <- tabs |> tab_add_totcol_if_no()
    }

    comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
    tabs <- tabs |> tab_match_comp_and_tottab(comp)

    if (any(pct != "no")){
      pct <- c(pct, all_col_vars = dplyr::last(pct[pct != "no"]))
      pct <- purrr::map_chr(tabs, ~ pct[get_col_var(.)] ) |>
        tidyr::replace_na("no")
      row_pct      <- names(pct)[pct == "row"]
      col_pct      <- names(pct)[pct == "col"]
      all_pct      <- names(pct)[pct == "all"]
      all_tabs_pct <- names(pct)[pct == "all_tabs"]


      tot_cols <- detect_totcols(tabs)


      if (any(pct != "all_tabs")) {
        pct_nat <- pct |> stringi::stri_replace_first_regex("all_tabs", "no") |>
          purrr::set_names(names(pct))

        tabs <- tabs |>
          dplyr::mutate(dplyr::across(
            where(~ is_fmt(.) & !fmt_var_kind(.) == "mean"),
            ~ set_pct(., pct_formula(
              .,
              pct = pct_nat[[dplyr::cur_column()]],
              tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
            )) |>
              set_display(ifelse(pct_nat[[dplyr::cur_column()]] != "no", "pct", "wn")) |>
              set_pct_base(pct_nat[[dplyr::cur_column()]]) |>
                set_scale("level_pct")
          ))
      }

      if (any(pct == "all_tabs")) {
        tabs <- tabs |>
          dplyr::with_groups(
            NULL,
            ~ dplyr::mutate(., dplyr::across(
              tidyselect::all_of(all_tabs_pct),
              ~ set_pct(., pct_formula(
                .,
                pct = "all_tabs",
                tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
              )) |>
                set_display("pct") |> set_scale("level_pct") |> set_pct_base("all_tabs")
            ))
          )
      }

      #Set digits if provided. Always zero digits for the 100% cells
      if (!is.null(digits)) {
        digits <- vctrs::vec_recycle(digits, length(col_vars_with_all)) |>
          purrr::set_names(col_vars_with_all)
        digits <- c(digits, all_col_vars = dplyr::last(digits[!is.na(digits)]))
        digits <- purrr::map_dbl(tabs, ~ digits[get_col_var(.)] )
        digits[pct == "no"] <- NA_real_

        digits_cols <- names(digits)[!is.na(digits)]

        tabs <- tabs |> dplyr::mutate(dplyr::across(
          tidyselect::all_of(digits_cols),
          ~ set_digits(., as.integer(digits[[dplyr::cur_column()]])) ))
      }

      if (length(row_pct     ) != 0) tabs <- tabs |> dplyr::mutate(dplyr::across(
        where(is_totcol) & tidyselect::all_of(row_pct), ~ set_digits(., 0L)))
      if (length(col_pct     ) != 0) tabs <- tabs |> dplyr::mutate(dplyr::across(
        tidyselect::all_of(col_pct),
        ~ dplyr::if_else(is_totrow(.), set_digits(., 0L), .)))
      if (length(all_pct     ) != 0) tabs <- tabs |> dplyr::mutate(dplyr::across(
        where(is_totcol) & tidyselect::all_of(all_pct),
        ~ dplyr::if_else(is_totrow(.), set_digits(., 0L), .)))
      if (length(all_tabs_pct) != 0) tabs <- dplyr::ungroup(tabs) |>
        dplyr::mutate(dplyr::across(
          where(is_totcol) & tidyselect::all_of(all_tabs_pct),
          ~ dplyr::if_else(dplyr::row_number()==dplyr::n(), set_digits(., 0L), .))) |>
        dplyr::group_by(!!!rlang::syms(groups))
    }

  } else {
    comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  }

  type <- fmt_kind_label(tabs)
  #Calculate diffs (used to color pct depending on spread from row or col mean)
  if (ref[1] != "no" & any(type %in% c("row", "col", "mean")) ) {

    if (ref[1] == "tot"  ) reference <- detect_totcols(tabs)
    if (ref[1] == "first") {
      reference <- detect_firstcol(tabs)
      reference_cols <- purrr::map_chr(reference, as.character) |> unique()
      reference_cols <- reference_cols[reference_cols != ""]

      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          where(~ get_pct_base(.) == "col") & tidyselect::all_of(reference_cols),
          as_refcol
        ))
      # is_refcol(tabs)

      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          where(~ get_pct_base(.) == "row" | fmt_var_kind(.) == "mean"),
          ~ as_refrow(., dplyr::row_number() == 1 &
                        (comp == "tab" | (comp == "all" & is_tottab(.)) ) )
        ))
      # is_refrow(tabs)
    }

    if ( comp == "all" & any(type %in% c("row", "mean")) ) {
      tabs <- tabs |>
        dplyr::with_groups(
          NULL,
          ~ dplyr::mutate(., dplyr::across(
            where(~ get_pct_base(.) %in% c("row", "col") | fmt_var_kind(.) == "mean"),
            ~ set_diff(., diff_formula(
              .,
              type = type[[dplyr::cur_column()]],
              ref = ref[1],
              refer  = rlang::eval_tidy(reference[[dplyr::cur_column()]])
            )) |> set_diff_type(ref[1])
          ))
        )

    } else {
      tabs <- tabs |>
        dplyr::mutate(dplyr::across(
          where(~ get_pct_base(.) %in% c("row", "col") | fmt_var_kind(.) == "mean") &
            !( where(is_totcol) &
                 tidyselect::any_of(names(reference)[reference == ""]) ),
          ~ set_diff(., diff_formula(
            .,
            type = type[[dplyr::cur_column()]],
            ref = ref[1],
            refer = rlang::eval_tidy(reference[[dplyr::cur_column()]])
          )) |> set_diff_type(ref[1])
        ))
    }

    if ( any(type %in% c("row", "mean")) ) tabs <- tabs |>
        dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., comp[1] == "all")))

    if (color == TRUE) {
      tabs <- tabs |>
        dplyr::mutate(dplyr::across(
          where(is_fmt),
          ~ set_color(., ifelse(
            type[[dplyr::cur_column()]] %in% c("row", "col", "mean"),
            "diff",
            get_color(.)
          )) ))
    }
  }

  tabs <- tabs |> dplyr::select(-tidyselect::any_of("totrow_groups"))

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, test = test)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext,
                    test = test)
  }
}


#' @keywords internal
pct_formula <- function(x, pct, tot) {
  switch(pct,
         "row"     =  get_wn(x) / get_wn(tot             ),
         "col"     =  get_wn(x) / get_wn(dplyr::last(x)  ),
         "all"     =  get_wn(x) / get_wn(dplyr::last(tot)),
         "all_tabs"=  get_wn(x) / get_wn(dplyr::last(tot)),
         NA_real_)
}

#' @keywords internal
diff_formula <- function(x, type, ref, refer) {
  switch(
    ref,
    "tot"   = switch(type,
                     "row"     =  get_pct(x)  - get_pct(dplyr::last(x  )),
                     "col"     =  get_pct(x)  - get_pct(refer             ),
                     "mean"    =  get_mean(x) / get_mean(dplyr::last(x) ),
                     NA_real_),
    "first" = switch(type,
                     "row"     =  get_pct(x)  - get_pct(dplyr::first(x  )),
                     "col"     =  get_pct(x)  - get_pct(refer              ),
                     "mean"    =  get_mean(x) / get_mean(dplyr::first(x) ),
                     NA_real_)
  )
}



# DESIGN: CI is stored as a half-width (margin of error), not a full interval.
#   The ci field = z * sqrt(variance). For pct, stored as 0-1 (multiplied by 100 in format).
#   method_cell controls the proportion CI formula (wilson default); method_diff controls
#   the difference CI formula (agresti-caffo default). Negative CI values indicate
#   non-significant differences (used by color_formula for diff_ci/after_ci modes).
#Ci spread (negative numbers mean no significant difference)
#' Add confidence intervals to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (2.0.0): confidence intervals are computed by the aggregate core, from the
#' `ci` / `ci_method` / `conf_level` / `stars` arguments of [tab()], [tab_plain()] and
#' [tab_num()] --- where the plan that decides them already lives. `tab_ci()` still works on an
#' existing tab, reconstructing that plan from the table's own markers.
#'
#' @param tabs A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param ci What the interval is anchored on -- \code{"ref"} (the comparison with the reference
#'  cell), \code{"cell"} (the cell's own value), \code{"no"}, or \code{"auto"}. See
#'  \code{\link{tab}}, which is where this is normally set: \code{"auto"} gives a comparison
#'  interval for means and row/column percentages and a cell interval for plain frequencies.
#'  \code{"diff"} and \code{"ratio"} are the soft-deprecated spellings of \code{"ref"}.
#'  With \code{ci = "cell"} the result prints as `[inf;sup]`; set
#'  `options("tabxplor.ci_print" = "moe")` for `pct +- moe`.
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
#' @param stars Logical (opt-in; default \code{FALSE}, or `options("tabxplor.stars")` when \code{NULL}).
#' With \code{ci = "diff"}, store and print per-cell significance stars for the difference from
#' the reference, read from the same interval that is displayed (universal CI-inclusion), so the
#' stars and the bracket never disagree. \code{FALSE} skips the significance computation.
#' @param ci_method The confidence-interval method of each kind of interval, as ONE named vector
#' (\code{c(cell = , diff = , mean_diff = , mean_ratio = )}, partial) -- see \code{\link{tab}}. The
#' \code{cell} slot also takes \code{"beta"} (Korn-Graubard:
#' \code{survey::svyciprop(method = "beta")}'s Clopper-Pearson interval on the effective sample size
#' -- the textbook design-based cell interval, conservative near 0 and 1. Beta quantiles have no
#' degrees of freedom of their own, so under a \code{survey} design the effective base is first
#' rescaled by \code{(qt(a, n - 1) / qt(a, degf))^2}, exactly as \code{survey} does, which refers the
#' interval to the design's own df; \code{degf} is the whole design's, as it is for every other
#' interval here).
#' @param method_cell,method_diff `r lifecycle::badge("deprecated")` Use
#' \code{ci_method = c(cell = , diff = )} instead.
#' @param degf The design's degrees of freedom, the reference distribution of every interval
#' (\code{#PSU - #strata}). \code{NULL} (default) takes the value the table itself stores when it was
#' built from a \code{survey::svydesign}; \code{Inf} is the large-sample normal pivot.
#' @param ci_scale Character string, the scale the \code{ci = "diff"} interval is expressed on:
#' \code{"diff"} (default) for a difference interval (neutral 0, one of the \code{ci_method["diff"]}
#' methods), or \code{"ratio"} for a ratio interval (neutral 1), stored on the column's own
#' \code{scale} attribute (\code{"pct_ratio"} / \code{"mean_ratio"}) and centred on the
#' cell/reference ratio -- Katz's log-risk-ratio for proportions (the only proportion
#' ratio method), or a ratio-of-means interval for numeric means (\code{ci_method["mean_ratio"]}).
#' \code{tab()} sets it from
#' the colour: the measure the reader sees owns the interval, so \code{color = "ratio"} (or
#' \code{c("ratio", "diff")}) asks for the ratio one.
#' @param color The type of colors to print, as a single string: \code{"no"} (the default),
#' \code{"diff_ci"} (colour percentages and means by their difference from the total or first cell,
#' dropping the colour when the interval of that difference is wider than the difference itself) or
#' \code{"after_ci"} (idem, but cutting the interval off the difference first). Those two combined
#' strings are the 1.x spelling of \code{\link{tab}}'s \code{color = "difference"} plus
#' \code{color_signif = "grey_non_signif"} / \code{"guaranteed_effect"}, which is what a table
#' built by \code{tab()} stores and its legend names.
#' @param visible By default confidence intervals are calculated and used to set colors,
#' but not printed. Set to \code{TRUE} to print them in the result.
#'
#' @section Significance stars:
#' With \code{stars = TRUE} and an interval anchored on the comparison (see \code{ci}), each cell
#' shows how sure we can be that its
#' difference from the reference is real and not just sampling noise: \code{*} means significant at
#' the 10\% level (p < 0.10), \code{**} at 5\% (p < 0.05), \code{***} at 1\% (p < 0.01). The exact
#' p-value is stored per cell in the \code{pvalue} field of the \code{fmt} vectors, readable with
#' \code{$pvalue} or \code{get_pvalue()}.
#'
#' There is no separate statistical test run behind the scenes: the significance is read straight
#' from the confidence interval that is displayed. A cell is significant at a given level exactly
#' when its interval at that confidence level no longer contains zero, so the stars and the printed
#' \code{[inf; sup]} bracket can never contradict each other. Which test this amounts to depends on
#' the interval:
#' \itemize{
#'   \item \strong{percentage difference} (default, \code{method_diff = "newcombe"}): inverting the
#'     Newcombe hybrid-score interval. This is, to a very close approximation, the classical
#'     two-sample test of proportions (the score / "N-1" chi-squared test).
#'   \item \strong{percentage difference} with \code{method_diff = "ac"} or \code{"wald"}: inverting
#'     the Agresti-Caffo (adjusted Wald) or the Wald interval -- an (adjusted) two-proportion z-test.
#'   \item \strong{mean difference}: the \strong{Welch two-sample t-test} (for groups with unequal
#'     variances); inverting the Welch t interval is exactly this well-known test.
#'   \item \code{ci = "cell"} (an absolute cell interval, not a difference) is purely descriptive,
#'     so it carries no stars and its \code{pvalue} is \code{NA}.
#' }
#' On weighted data the estimate is weighted but the sample size used is the real (unweighted)
#' number of cases, unless you opt in to the weighting's own design effect with
#' \code{options("tabxplor.design_effect" = TRUE)}.
#'
#' @return A \code{tibble} of class \code{tab}, colored based on differences (from
#' totals/first cells) and confidence intervals.
#' @export
#'
#' @examples # A typical workflow with tabxplor step-by-step functions :
#' \donttest{
#' data <- dplyr::starwars |>
#'   tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'               na_drop_all = sex)
#'
#' data |>
#'   tab_plain(sex, hair_color, gender, tot = c("row", "col"),
#'     pct = "row", comp = "all") |>
#'     tab_ci("diff", color = "after_ci")
#'   }
tab_ci <- function(tabs,
                   ci = "auto",
                   comp = NULL,
                   conf_level = conf_level_default(),
                   color = "no",
                   visible = FALSE,
                   stars = NULL,
                   ci_method = NULL,
                   method_cell = NULL, method_diff = NULL,
                   ci_scale = "diff", degf = NULL) {
  # Phase 18z16-iiiii: the four interval methods are ONE named vector (see CI_METHODS); the
  # released `method_cell` / `method_diff` are soft-deprecated aliases into it, and validation is the
  # shared resolver's, so tab_ci() cannot accept a value tab() rejects.
  # Phase 19j: it travels WHOLE from here on -- CI_GEOMS names the slot each geometry reads, so the
  # four unpacked scalars (which every engine call and every stamping arm had to pick between) are gone.
  ci_method <- resolve_ci_method(ci_method, method_cell, method_diff, "tab_ci")
  # Phase 18z16-i (W7): the DESIGN's degrees of freedom. Taken from the table's own stored
  # inference fact when the caller does not supply one, so the exported STEP path
  # (tab_plain(design) |> tab_ci()) refers its intervals to t(degf) exactly as the pipeline does.
  # Phase 18z16-iiiii: read off the COLUMNS (the smallest design df any of them carries), not off a
  # table attribute -- that is what makes the exported step path, and a table a pipeline has stripped
  # of its metadata, still refer their intervals to t(degf) instead of silently falling back to z.
  if (is.null(degf)) degf <- tab_inference_degf(tabs)
  stopifnot(all(ci_scale %in% c("diff", "ratio")),
            all(comp %in%  c("tab", "all"))
  )
  # Phase 19i: the vocabulary is DECLARED (TAB_CI_STEP_VALUES, beside resolve_ci_value() in
  # R/tab-resolve.R) instead of hand-listed here, and the abort names the valid set.
  # WARNING -- and this is why it is a SEPARATE declaration rather than resolve_ci_value(): this
  # superseded step speaks the COMPUTATIONAL vocabulary ("no"/"cell"/"diff"), in which `"diff"` is
  # its own native word, not the deprecated anchor spelling 19d retired on tab()/tab_num()/
  # tab_counts(). The pipeline itself calls it that way (tab_apply_tests hands it the resolved
  # step value), so routing it through the public resolver would fire a deprecation on tabxplor's
  # own build. `"ref"` -- "the interval of the comparison" -- is the anchor synonym for that branch
  # (the odds-ratio one is the leaf's).
  bad_ci <- !ci %in% TAB_CI_STEP_VALUES
  if (any(bad_ci))
    cli::cli_abort(c("Unknown {.arg ci} value {.val {unique(ci[bad_ci])}}.",
                     "i" = "Valid: {.val {TAB_CI_STEP_VALUES}}."))
  ci[ci == "ref"] <- "diff"
  # Phase 15c: a direct `ci = "ratio"` == a difference CI on the ratio (Katz) scale, independent of
  # colour. Fold it to ci = "diff" + ci_scale = "ratio" (the pipeline already does this via
  # tab_resolve_settings(); this makes tab_ci() a self-contained entry point too).
  if (any(ci == "ratio")) {
    ci_scale <- rep_len(ci_scale, length(ci))
    ci_scale[ci == "ratio"] <- "ratio"
    ci[ci == "ratio"] <- "diff"
  }
  # Phase 3a: significance stars default (universal CI-inclusion). NULL -> option default.
  stars <- resolve_stars(stars)

  subtext <- get_subtext(tabs)
  test    <- get_test(tabs)


  get_vars          <- tab_get_vars(tabs)

  col_vars_with_all <- rlang::syms(get_vars$col_vars)
  col_vars_no_all   <- col_vars_with_all |> purrr::discard(\(s) as.character(s) == "all_col_vars")

  fmtc <- purrr::map_lgl(tabs, is_fmt)
  ci <- vctrs::vec_recycle(ci, length(col_vars_no_all)) |>
    purrr::set_names(col_vars_no_all)
  ci <- c(ci, all_col_vars = dplyr::last(ci[ci != "no"]))
  ci <- purrr::map_chr(tabs, ~ ci[get_col_var(.)] ) |>
    tidyr::replace_na(NA_character_)

  visible <- vctrs::vec_recycle(visible, length(col_vars_no_all)) |>
    purrr::set_names(col_vars_no_all)
  visible <- c(visible, all_col_vars = dplyr::last(visible[visible != "no"]))
  visible <- purrr::map_lgl(tabs, ~ visible[get_col_var(.)] ) |>
    tidyr::replace_na(FALSE)


  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs |> tab_match_comp_and_tottab(comp)

  # Phase 19b: which axis the comparison runs along (`pct_base`) and whether the column summarises a
  # mean -- the two facts this step was reading out of the old `type` attribute.
  base   <- get_pct_base(tabs)
  vkind  <- fmt_var_kind(tabs)
  is_rm  <- base == "row" | vkind == "mean"          # the reference is a ROW
  ci_able <- vkind == "mean" | base != "none"        # a count / a coefficient carries no cell CI
  tot_cols <- detect_totcols(tabs)
  tot_cols[is.na(ci)] <- list(rlang::sym(""))
  names_totcols <- tot_cols |> purrr::map_chr(as.character) |> unique() |>
    purrr::discard(\(s) s == "")

  ref <- get_ref_type(tabs)
  # Phase 7g-iii: the diff-CI reference column must match the diff/colour reference column
  # (detect_refcol = the marked refcol, falling back to the first level -> byte-identical for
  # ref = "first"; ref = "tot" uses tot_cols below, so detect_refcol is not consulted there).
  ref_cols  <- detect_refcol(tabs)
  ref_cols[is.na(ci)] <- list(rlang::sym(""))

  ref_cols <- dplyr::if_else(ref == "tot",
                             true  = tot_cols,
                             false = ref_cols     ) |>
    purrr::set_names(names(ref)) #keep ci_yes ?
  names_refcols <- ref_cols |> purrr::map_chr(as.character) |> unique() |>
    purrr::discard(\(s) s == "")

  ci[fmtc] <- dplyr::case_when(
    !ci_able[fmtc]                                              ~ "no"      ,
    ci[fmtc] == "cell"                                          ~ "cell"    ,
    ci[fmtc] == "diff"   & is_rm[fmtc]                          ~ "diff_row",
    ci[fmtc] == "diff"   & base[fmtc] == "col"                  ~ "diff_col",

    ci[fmtc] == "auto"   & is_rm[fmtc]                          ~ "diff_row",
    ci[fmtc] == "auto"   & base[fmtc] == "col"                  ~ "diff_col",
    ci[fmtc] == "auto"   & base[fmtc] %in% c("all","all_tabs")  ~ "cell"    ,

    TRUE                                                        ~ "no"
  )


  #Depending of ci type, totals and reference cols (for diff), not calculate ci
  ci <- dplyr::if_else(
    condition = !ci_able | (ci %in% c("diff_col", "spread_col") & vkind == "mean"),
    true = "no",
    false = ci
  )
  ci_with_ref <- ci |> purrr::set_names(names(tabs))
  ci <- dplyr::if_else(
    condition = (ci == "diff_col" & names(tabs) %in% names_refcols) |
      (ci == "diff_col" & get_col_var(tabs) == "all_col_vars") |
      (ci == "diff_row" & names(tabs) %in% names_totcols),
    true = "no",
    false = ci
  )
  ci <- ci |> purrr::set_names(names(tabs))
  ci_yes <- !is.na(ci) & ! ci == "no"


  if (any(ci_yes)) {
    #Ready table for percentages (needed totals, compatible grouping)
    if ( any(ci == "diff_col" ) ) tabs <- tabs |> tab_add_totcol_if_no()
    if ( any(ci == "diff_row") ) {
      tabs <- switch(comp[1],
                     "tab" = tabs |> tab_match_groups_and_totrows(),
                     "all" = tabs |> dplyr::ungroup()               )
    }

    # Phase 9b-5 increment 2: reference-row selection + reference stats on PLAIN fields, replacing the
    # ref_rows/tot_rows/ref_to_na grouped transmutes and the x_n/ref/ref_var/ref_n transmutes (each a
    # reconstruction over the fmt columns). Per SUBTABLE, group_last_pos(mask) = the ABSOLUTE index of
    # the group's last masked row, broadcast to that group (NA if none) -- the plain form of
    # `.[dplyr::last(which(<mask>))]` under grouping. The old `tot_rows` was DEAD (computed, never read).
    ci_cols   <- names(ci_yes)[ci_yes]
    diff_cols <- names(ci_yes)[ci %in% c("diff_row", "diff_col")]
    mean_cols <- names(ci_yes)[ci == "diff_row" & vkind == "mean"]

    gid  <- dplyr::group_indices(tabs)
    gids <- unique(gid)
    group_last_pos <- function(mask) {
      pos <- rep(NA_integer_, length(mask))
      for (g in gids) {
        r <- which(gid == g); w <- which(mask[r])
        if (length(w)) pos[r] <- r[[w[[length(w)]]]]
      }
      pos
    }
    # the reference row per cell = last total row (ref = "tot") else last is_refrow row.
    ref_mask <- function(col) if (identical(get_ref_type(col), "tot")) is_totrow(col) else is_refrow(col)

    empty <- stats::setNames(vector("list", length(ci_cols)), ci_cols)
    x_n <- ref <- ref_var <- ref_n <- ci_inf <- ci_sup <- pvalue <- empty
    for (nm in ci_cols) {
      col <- tabs[[nm]]
      tp  <- fmt_var_kind(col)
      rp  <- group_last_pos(ref_mask(col))                     # per-row reference-row index (NA if none)
      rtona <- !is.na(rp) & (seq_along(rp) == rp)              # ref_to_na: the cell's own reference row
      # Phase 6h: each cell's OWN unweighted base (tot_n for proportions, n for means); NA on the
      # reference cell so its own CI is not computed -- but ONLY where the interval is a comparison.
      # Phase 19m-i: that decision is CI_GEOMS$ref_cell (R/tab-agg.R), the same lookup the two leaves
      # make; a `ci = "cell"` interval has no reference, so every cell keeps its own.
      # Phase 18s: the CI base is the effective n (`n_eff`) when populated, else the raw base --
      # Phase 19a folds that coalesce, written out at all five read sites below, into fmt_base().
      ref_na <- identical(ci_geom_ref_cell(if (identical(ci[[nm]], "cell")) "cell" else "diff",
                                           if (identical(tp, "mean")) "mean" else "pct",
                                           ci_scale[1]), "na")
      x_n[[nm]] <- dplyr::if_else(
        rtona & ref_na, NA_integer_,
        # every proportion's base is `tot_n`, a mean's is `n`. (Phase 18z16-ii had to add "all" /
        # "all_tabs" to a hand-written list of percentage types here, which is exactly the kind of
        # omission `var_kind` removes: there is one arm per KIND of column, not one per type value.)
        if (identical(tp, "mean")) fmt_base(col, mean = TRUE) else fmt_base(col))
      if (nm %in% diff_cols) {
        if (ci[[nm]] == "diff_col") {
          rcol        <- tabs[[as.character(ref_cols[[nm]])]]  # the reference COLUMN (its own base)
          ref[[nm]]   <- get_pct(rcol)
          ref_n[[nm]] <- fmt_base(rcol)[group_last_pos(is_totrow(col))]
        } else {                                               # diff_row: the reference ROW cell
          ref[[nm]]   <- if (tp == "mean") get_mean(col)[rp] else get_pct(col)[rp]
          ref_n[[nm]] <- fmt_base(col, mean = tp == "mean")[rp]
        }
        if (nm %in% mean_cols) ref_var[[nm]] <- get_var(col)[rp]
      }

      # Confidence interval + per-cell pvalue via the closed-form engine (R/tab-agg.R). Weighted rule
      # (§14): weighted proportion get_pct() / weighted mean get_mean(), UNWEIGHTED base x_n. Cell CIs
      # carry no pvalue; diff CIs star only when `stars` is on (want_p). The reference cell has
      # x_n = NA (rtona) -> NA bounds, so it is never self-compared.
      # Phase 19j (KEY 5): WHICH interval this column asks for is one lookup in CI_GEOMS
      # (R/tab-agg.R) now -- the engine, its method slot, and the scale it makes the column estimate,
      # read by this step, by the factor leaf and by num_core() alike. The reference cell keeps its
      # NA `x_n` (rtona above), which is this caller's own way of saying "no interval here".
      kind_1 <- if (identical(ci[[nm]], "cell")) "cell" else "diff"
      vk_1   <- if (identical(tp, "mean")) "mean" else "pct"
      res <- ci_dispatch(
        kind = kind_1, var_kind = vk_1, ci_scale = ci_scale[1],
        est = if (vk_1 == "mean") get_mean(col) else get_pct(col),
        base = x_n[[nm]], var = get_var(col),
        ref = ref[[nm]], ref_var = ref_var[[nm]], ref_n = ref_n[[nm]],
        n_raw = get_tot_n(col),
        conf_level = conf_level, want_p = isTRUE(stars),
        method = ci_method, degf = degf)
      ci_inf[[nm]] <- res$inf; ci_sup[[nm]] <- res$sup; pvalue[[nm]] <- res$pvalue

    }

    # Phase 9b-5 increment 2: apply the precomputed CI bounds/pvalue (loop above) + `comp_all` + the
    # `visible` display in ONE mutate over plain vectors (was: a with_groups(NULL) CI mutate, then a
    # mutate for comp_all, then one for display -- 3 fmt reconstructions). All three writes are
    # ROW-WISE, so run ungrouped then restore grouping (matching the with_groups(NULL) the CI used).
    diff_row_any <- any(ci == "diff_row")
    comp_all_val <- comp[1] == "all"
    vis_mask     <- visible & ci != "no"
    visible_cols <- names(visible)[!is.na(vis_mask) & vis_mask]
    display      <- stats::setNames(lapply(visible_cols, function(nm)
      if (ci[[nm]] == "cell") ifelse(vkind[[nm]] == "mean", "mean_ci", "pct_ci") else "ci"), visible_cols)
    # comp_all touches ALL fmt columns (if diff_row); otherwise only the CI + visible columns.
    write_cols   <- if (diff_row_any) names(tabs)[purrr::map_lgl(tabs, is_fmt)]
                    else union(ci_cols, visible_cols)
    grp <- dplyr::group_vars(tabs); drp <- dplyr::group_by_drop_default(tabs)
    tabs <- dplyr::mutate(dplyr::ungroup(tabs), dplyr::across(
      tidyselect::all_of(write_cols),
      function(col) {
        nm <- dplyr::cur_column()
        if (nm %in% ci_cols)
          col <- set_pvalue(set_ci_sup(set_ci_inf(col, ci_inf[[nm]]), ci_sup[[nm]]), pvalue[[nm]])
        if (diff_row_any)         col <- set_comp_all(col, comp_all_val)
        if (nm %in% visible_cols) col <- set_display(col, display[[nm]])
        # Byte-identity quirk (as in chi2_write_contrib): the pre-9b-5 comp_all / visible writes were
        # GROUPED mutates, whose per-group recombine MATERIALISES the `wn` field (NA -> n). Reproduce
        # it for exactly those columns (comp_all = all fmt on diff_row; visible = its own columns) when
        # the table is grouped; a no-op when wn is already set / weighted, or the table is ungrouped.
        if (length(grp) > 0L && (diff_row_any || nm %in% visible_cols))
          col <- set_wn(col, get_wn(col))
        col
      }))
    if (length(grp)) tabs <- dplyr::group_by(tabs, dplyr::across(dplyr::all_of(grp)), .drop = drp)


    #Change the scale and the color, even for totals with no ci result
    ci_with_ref <- stringi::stri_replace_first_regex(ci_with_ref, "_row|_col", "")
    # Phase 19b (KEY 2): this step does not RECORD ITS ARGUMENT any more -- it stamps what the column
    # now estimates. Adding a contrast interval to a percentage column CHANGES WHAT THAT COLUMN IS
    # (`level_pct` -> `points`), and every reader (ci_center(), format()'s bracket, the colour
    # significance gate, the legend, the forest-plot axis) reads that one fact instead of re-deriving
    # a colour spec. A `cell` interval changes nothing: a mean with its own interval is still a mean.
    # 14v-ii: a mean also takes the ratio branch above (ci_mean_ratio), so a ratio mean lands on
    # `mean_ratio` (neutral 1, bare bracket) like a ratio proportion.
    # Phase 19b (D8): WHICH engine built these bounds, stamped where it is known instead of being
    # picked back out of a table-wide vector BY MEASURE (an eight-branch chain that could name a
    # method the bounds were never built with -- most visibly a one-sample cell interval on a mean,
    # announced as "Welch t"). Like the scale it is stamped for the WHOLE col_var, totals and
    # reference columns included: their own bounds are NA by construction, and THAT is the data fact
    # saying "no interval here" -- exactly the rule D19 settled for the odds-ratio scale.
    # Phase 19j: both come from the SAME CI_GEOMS row that chose the engine two blocks above, so the
    # scale, the method name and the bounds cannot describe three different intervals.
    ci_yes_ref  <- !is.na(ci_with_ref) & !ci_with_ref == "no"
    ci_var_kind <- function(col) if (identical(fmt_var_kind(col), "mean")) "mean" else "pct"

    # Phase 17d: `color` may arrive as a legacy combined string -- since 19c that is possible ONLY on
    # the exported step path (`tab_plain() |> tab_ci(color = "after_ci")`), because the pipeline hands
    # this step `color = "no"`: its stamping sub-pass existed to receive a composite the cascade
    # manufactured, and both are gone. Decode it ONCE into the clean (measure, policy) pair so the
    # stored attributes stay clean and the engine never re-parses one.
    col_dec <- color_decode_legacy(color[1])
    set_ci_col <- !is.null(color[1]) && !color[1] %in% c("no", "")
    tabs[ci_yes_ref] <-
      purrr::map2_df(tabs[ci_yes_ref],
                     ci_with_ref[ci_yes_ref],
                     function(col, ci_ref) {
                       vk <- ci_var_kind(col)
                       # NA scale_key = "cell": the level scale stands (a mean with its own interval
                       # is still a mean), which is what the old ci_scale_of() said by returning
                       # get_scale(col).
                       sc <- ci_geom_scale(ci_ref, vk, ci_scale[1])
                       if (!is.na(sc)) col <- set_scale(col, sc)
                       col <- set_ci_method(col, ci_geom_method(ci_ref, vk, ci_scale[1], ci_method))
                       if (set_ci_col) {
                         col <- set_color(col, col_dec$measure)
                         if (!is.null(col_dec$policy)) col <- set_color_signif(col, col_dec$policy)
                       }
                       col
                     })
  }


  # Phase 18z13 (D3): this step COMPUTES the intervals, so it owns their level -- otherwise
  # tab_plain() |> tab_ci(conf_level = 0.99) would store 99 % bounds under the leaf's 95 % stamp and
  # the engine would grey at the wrong level.
  tabs <- tab_stamp_inference(tabs, conf_level)

  # Phase 19a: this IS tab_restore()'s body (same lv1_group_vars() downgrade, same three attributes)
  # -- with one difference that mattered: neither tail passed `meta`, so a direct
  # `tab_plain() |> tab_ci()` on the exported step path silently dropped `vars` / `ci_settings` /
  # `render_extras` / `color_breaks` / `reg_meta`. It survived only by accident of
  # tibble::new_tibble() carrying the incoming attributes through, which the grouped branch does not
  # guarantee. Passing them explicitly removes the whole hazard class from the step path.
  tab_restore(tabs, tabs, attrs = list(subtext = subtext, test = test, meta = get_meta(tabs)))
}



#' Add Chi2 summaries to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (2.0.0): the whole-table test and the per-cell contributions are computed by the
#' aggregate core, from the `test` and `color` arguments of [tab()] --- where the plan that
#' decides them already lives. `tab_chi2()` still works on an existing tab, reconstructing that
#' plan from the table's own markers.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param calc By default all elements of the Chi2 summary are calculated :
#' contributions to variance, pvalue, variance and unweighted count. You can choose which
#' are computed by selecting elements in the vector \code{c("ctr", "p", "var", "counts")}.
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param color The type of colors to print, as a single string.
#' \itemize{
#'   \item \code{"no"}: by default, no colors are printed
#'   \item \code{"all"}: color all cells based on their contribution to variance
#' (except for mean columns, from numeric variables)
#'   \item \code{"all_pct"}: color all percentages cells based on their contribution to
#'   variance
#'   \item \code{"auto"}: only color columns with counts, \code{pct = "all"} or
#'    \code{pct = "all_tabs"}
#' }
#' @param .deff Internal pipeline seam. The design-based omnibus grid (one row per subtable x
#' col_var, carrying Rao-Scott's mean generalized design effect), used as the divisor of the
#' \code{color = "contrib"} residual's base when the table's inference basis is not \code{"n"}.
#' \code{NULL} --- the default, and every direct call --- keeps the unweighted base.
#' @return A \code{tibble} of class \code{tab}, with Chi2 summaries as metadata,
#' possibly colored based on contributions of cells to variance.
#' @export
#'
# @examples # A typical workflow with tabxplor step-by-step functions :
# \donttest{
# data <- dplyr::starwars |>
#   tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#               na_drop_all = sex)
#
# data |>
#   tab_plain(sex, hair_color, gender, tot = c("row", "col")) |>
#   tab_chi2(calc = c("p", "ctr"), color = TRUE)
#   }
tab_chi2 <- function(tabs, calc = c("ctr", "p", "var", "counts"),
                     comp = NULL, color = c("no", "auto", "all", "all_pct"),
                     .deff = NULL
) {
  get_vars        <- tab_get_vars(tabs)
  row_var         <- get_vars$row_var
  col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)

  stopifnot(all(calc %in% c("all", "ctr", "p", "var", "counts")))
  if ("all" %in% calc) calc <- c("ctr", "p", "var", "counts")
  subtext         <- get_subtext(tabs)

  # 19m-i: "no real col_var" through the declared set (R/fmt_class.R), and the row axis through the
  # build-time placeholder-NAME predicate -- two different questions, said as two.
  if (!any(is_real_col_var(get_col_var(tabs))) |
      any(is_placeholder_var(names(tabs)))
  ) return(tabs)

  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs |> tab_match_comp_and_tottab(comp)

  # Phase 10j-B: per col_var, is ANY of its level columns a mean? Read get_type() -- a scalar column
  # attribute -- DIRECTLY off each level column, instead of dplyr::select(ungroup(tabs), <levels>) per
  # col_var (which reconstructed the fmt columns just to read that attribute: ~4.6 % of a chi2 build).
  # Byte-identical (PoC dev/benchmarks/phase10j_tests_parity.R: 26/26 identical over factor/mixed/mean
  # x comp tab/all x 0-2 tab_vars x weighted x a 2x2 Yates).
  is_a_mean <-
    purrr::map_lgl(col_vars_levels, function(levs) {
      cols <- purrr::map_chr(levs, rlang::as_name)
      any(vapply(cols, function(cc) fmt_var_kind(tabs[[cc]]) == "mean", logical(1)))
    })
  # Phase 3b: mean col_vars now get an ANOVA F (the chi2 mirror), so an all-means table is no
  # longer skipped -- only the factor total-row/total-col scaffolding (which is factor-oriented)
  # is skipped for it. The ANOVA runs on the data rows (row_var-level groups) via agg_anova().
  if (!all(is_a_mean)) {
    tabs <- tabs |> tab_match_groups_and_totrows() |> tab_add_totcol_if_no()
  }

  if (comp == "all") tabs <- tabs |> dplyr::ungroup()

  tot_cols <- detect_totcols(tabs)


  all_col_tot <- names(col_vars_levels) == "all_col_vars"

  tot_cols_names <- purrr::map_lgl(tabs, is_totcol) #|>  .[.] |> names()
  tot_cols_names <- tot_cols_names[tot_cols_names] |> names()
  col_vars_levels_no_tot <-
    purrr::map(col_vars_levels,~ purrr::discard(., . %in% tot_cols_names ) )



  # Phase 9b-5: the per-cell contribution-to-variance WRITES (var, ctr) + the comp_all / contrib-color
  # col-meta -- ported to ONE mutate(across()) over plain-precomputed vectors (chi2_write_contrib()),
  # replacing the pre-9b-5 ~6 mutate(across(where(is_fmt), set_*)) passes (each a full fmt-record
  # reconstruction). Byte-identical; the real cost of the contrib color path (+~97% vs a plain build).
  if ("ctr" %in% calc | "var" %in% calc) {
    tabs <- chi2_write_contrib(tabs, calc, comp, color, col_vars_levels,
                               col_vars_levels_no_tot, is_a_mean, all_col_tot, tot_cols,
                               deff = .deff)
  }

  # Phase 9b-5: the whole-table chi2/ANOVA test is a READ-ONLY computation over the cell fields (it
  # builds the tidy `test` tibble, never touches the cells) -- extracted so its plain-field
  # marshalling is isolated from the record-based tab_chi2 orchestration. See chi2_compute_test().
  test_tbl <- chi2_compute_test(tabs, comp, row_var, col_vars_levels,
                                col_vars_levels_no_tot, is_a_mean, all_col_tot)

  tabs <- tabs |> dplyr::select(-tidyselect::any_of("tottabs"))

  # Phase 19a: tab_restore(), carrying `meta` explicitly -- see the twin tail in tab_ci().
  tab_restore(tabs, tabs, attrs = list(subtext = subtext, test = test_tbl, meta = get_meta(tabs)))
}



# === SECTION: the wrappers' own machinery (Phase 19l) =============================================
# These six helpers have NO caller outside this file. Four of them (tab_match_groups_and_totrows /
# tab_add_totcol_if_no / tab_validate_comp / tab_match_comp_and_tottab) MUTATE the table to make a
# step's preconditions true; two (detect_refcol / detect_firstcol) RECONSTRUCT, from the fmt markers,
# which column of each col_var group a step should compare against. Both jobs exist only because a
# step runs on a table it did not build -- the build knows all of it -- so 19l moved them out of
# R/tab.R and R/fmt_class.R and in here, where the reader meets them beside the only functions that
# call them. Nothing in the live pipeline meets them at all now.
# ⚠ detect_totcols() did NOT come with them: it has one live caller, tab_add_n_pct() on the
# EXPORTER path (R/tab.R), so it stays in R/fmt_class.R beside the other fmt-marker readers.

#' @keywords internal
tab_match_groups_and_totrows <- function(tabs) {
  #chi2 : not to match groups and totrows with alltabs ? ----

  groups   <- dplyr::group_vars(tabs)

  #If there is a total_row at the end of each group, keep (un)grouping as is
  ind <- dplyr::group_indices(tabs) # 1 1 1 if data isn't grouped
  end_groups <- append(ind[-length(ind)] != ind[-1], FALSE)
  if (any(is_totrow(tabs)) & all(is_totrow(tabs)[end_groups]) ) {return(tabs)}

  #If there isn't any total row, keep actual (un)grouping and add some
  if ( !any(is_totrow(tabs))) {


    if (length(groups) != 0) {
      #if ( !identical(tab_vars, groups) ) {
      warning("no total row(s) found. Some added based on actual grouping variables : ",
              paste(groups, collapse = ", "))
      return(dplyr::group_by(tabs, !!!rlang::syms(groups)) |> tab_tot("row"))
    } else if ( !any(is_tottab(tabs)) ) { #If there are no groups
      warning("no groups nor total row(s) found. One added for the whole table")
      return(tab_tot(tabs, "row"))
    } else {
      warning("no groups nor total row(s), but total table found. ",
              "Grouped upon tab_vars and total rows added")
      tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
      return(dplyr::group_by(tabs, !!!tab_vars) |> tab_tot("row"))
    }

    #If there is at least one total row, calculate new groups based on them
  } else {
    if (utils::tail(is_totrow(tabs), 1L)) return(dplyr::ungroup(tabs))


    tabs_totrow_groups <- tabs |> dplyr::ungroup() |>
      (\(d) tibble::add_column(d, totrow_groups = as.integer(is_totrow(d))))() |>
      dplyr::mutate(totrow_groups = 1 + cumsum(.data$totrow_groups) - .data$totrow_groups)
    totrow_indices <- tabs_totrow_groups$totrow_groups

    #Control if totrows groups match tab_vars, collectively or individualy, if yes group
    tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
    if ( !identical(tab_vars, groups) ) {
      tabs_tab_vars_groups <- tabs |> dplyr::group_by(!!!tab_vars)
      tab_vars_indices <- dplyr::group_indices(tabs_tab_vars_groups)

      if (all(totrow_indices == tab_vars_indices)) return(tabs_tab_vars_groups)
    }

    each_tab_var_indices <-
      tabs |> dplyr::ungroup() |> dplyr::select(!!!tab_vars) |>
      dplyr::transmute(dplyr::across(dplyr::everything(), as.integer)) |>
      purrr::map(~ .)

    each_tab_var_totrow_comp <-
      purrr::map_lgl(each_tab_var_indices, ~ all(. == totrow_indices))

    if (any(each_tab_var_totrow_comp)) {
      group_var_name <- names(each_tab_var_totrow_comp[each_tab_var_totrow_comp])[1]
      return(dplyr::group_by(tabs, !!rlang::sym(group_var_name)))
    }

    # Otherwise return a df grouped with the total rows groups, in a new variable
    warning("grouping variable(s) not corresponding to total_rows, ",
            "new groups calculated, based on actual total_rows")
    return(dplyr::relocate(tabs_totrow_groups, .data$totrow_groups, .before = 1) |>
             dplyr::group_by(.data$totrow_groups)
    )

  }

}



#' @keywords internal
tab_add_totcol_if_no <- function(tabs) {
  if (!any(is_totcol(tabs)) & ! all(fmt_var_kind(tabs) == "mean")) { # & !only_one_column
    only_one_column <- length(which(purrr::map_lgl(tabs, is_fmt))) == 1L
    tabs <- tabs |> tab_tot("col", totcol = "last")
    if (!only_one_column) warning("no total column, one was added (from the last non-mean column)")
  }
  tabs
}





#' @keywords internal
tab_validate_comp <- function(tabs, comp) {
  comp_all        <- purrr::map_lgl(tabs[purrr::map_lgl(tabs, is_fmt)],
                                    ~ get_comp_all(., replace_na = FALSE))
  comp_all_no_na  <- comp_all[!is.na(comp_all)]

  if (!all(is.na(comp_all))) {
    if(comp == "tab" & any(comp_all_no_na) ) {
      warning("since at least one column already have an element calculated ",
              "with comparison to the total row of the total table (pct or means ",
              "diffs from total, chi2 variances or confidence intervals), ",
              "comp were set to 'all'")
      comp <- "all"
    }
    if (comp == "all" & all(!comp_all_no_na) ) {
      warning("since at least one column already have an element calculated ",
              "with comparison to the total row of each tab_var (pct or means ",
              "diffs from total, chi2 variances or confidence intervals), ",
              "comp were set to 'tab'")
      comp <- "tab"
    }
  }
  if (comp == "null") {
    if ( all(is.na(comp_all)) ) {
      comp <- "tab"
    } else {
      comp <- ifelse(any(comp_all_no_na), "all", "tab")
    }
  }
  comp
}



#' @keywords internal
tab_match_comp_and_tottab <- function(tabs, comp) {
  if(comp == "all" & !any(is_tottab(tabs) & is_totrow(tabs)) ) {
    warning("since 'comp' is 'all', a total table with a ",
            "total row was added")
    tabs <- tabs |> tab_totaltab('line')
  }
  tabs
}


#' @keywords internal
detect_firstcol <- function(tabs) {
  col_vars <- get_col_var(tabs)
  firstcol <- which(col_vars != dplyr::lag(col_vars, default = NA_character_))
  if (any(col_vars == "all_col_vars"))
    firstcol <- purrr::discard(firstcol, names(firstcol) == names(col_vars)[col_vars == "all_col_vars"])

  res <- purrr::map(1:ncol(tabs), function(.i)
    tidyr::replace_na(
      dplyr::last(names(firstcol[firstcol <= .i]) ),
      "")) |>
    rlang::syms() |>
    purrr::set_names(names(tabs))

  if (any(col_vars == "all_col_vars")) {
    res[col_vars == "all_col_vars"] <- rlang::syms("")
  }
  res
}

# For each column, detect the REFERENCE column of its col_var group -- the one marked by the `refcol`
# attribute (is_refcol). Falls back to detect_firstcol()'s first-column-of-group when no reference is
# marked, so it is byte-identical to detect_firstcol() whenever the reference IS the first level (or is
# unmarked). Phase 7g-iii: tab_ci() uses it so the diff-CI reference column matches the diff/colour
# reference column, once a per-col_var reference can be neither the first level nor the total.
#' @keywords internal
detect_refcol <- function(tabs) {
  col_vars  <- get_col_var(tabs)
  refcol    <- is_refcol(tabs)
  nms       <- names(tabs)
  firstcols <- detect_firstcol(tabs)   # per-column sym of each group's first column (fallback + "" edges)
  res <- purrr::map(seq_len(ncol(tabs)), function(.i) {
    in_grp <- which(col_vars == col_vars[.i] & refcol)
    if (length(in_grp) >= 1L) rlang::sym(nms[in_grp[1]]) else firstcols[[.i]]
  }) |>
    purrr::set_names(nms)
  # mirror detect_firstcol: no reference column for the all_col_vars total group
  if (any(col_vars == "all_col_vars")) res[col_vars == "all_col_vars"] <- rlang::syms("")
  res
}
