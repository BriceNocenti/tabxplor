# PURPOSE: The superseded dplyr-era step functions tab_pct()/tab_tot()/tab_totaltab() + their
#   trio-exclusive formula helpers pct_formula()/diff_formula().
# ROLE: Quarantined here in Phase 17f (item 5) out of tab.R's live pipeline. These are the pre-1.4.0
#   step-by-step API: exported + soft-deprecated (superseded badge), still working on an existing tab,
#   but OFF the tab()/tab_many() aggregate-core path (the math is now inline in the leaves).
# KEY CONSTRAINTS:
#   - Exports unchanged (the @export roxygen travels with the functions; document() keeps NAMESPACE).
#   - These call INTO shared helpers that stay in tab.R (tab_match_groups_and_totrows,
#     tab_add_totcol_if_no, tab_validate_comp, tab_match_comp_and_tottab, var_contrib_ctr_signed,
#     contrib_pvalue) + the live tab_ci()/tab_chi2(); nothing here is called BY the core.
# See: CLAUDE.md Repository Map > R/tab-steps-legacy.R.


#' Add total table to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (1.4.0): the total table is built directly by the `totaltab` argument of
#' [tab()] / [tab_plain()] / [tab_num()]. `tab_totaltab()` still works on an existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
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
  mean_vars <- (get_type(tabs) == "mean") |> purrr::keep(\(x) x) |> names()


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
#' Superseded (1.4.0): totals are built directly by [tab()] / [tab_plain()] / [tab_num()] (a
#' total row is always computed, one total column shown). `tab_tot()` still works on an
#' existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
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
    totcol %in% c("last", "each", "no", "")
  )

  get_vars        <- tab_get_vars(tabs)
  row_var         <- rlang::sym(get_vars$row_var)
  #col_vars        <- rlang::sym(get_vars$col_vars)
  col_vars_levels_mean <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  mean_vars <- get_type(tabs) == "mean"
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
#' Superseded (1.4.0): percentages, differences and ratios are computed directly by
#' [tab()] / [tab_plain()] via the `pct` / `ref` arguments. `tab_pct()` still works on an
#' existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
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
  #row_var         <- rlang::sym(get_vars$row_var) #col_var ??
  col_vars_with_all<- rlang::syms(get_vars$col_vars)
  col_vars_no_all  <- col_vars_with_all |> purrr::discard(\(s) as.character(s) == "all_col_vars")
  col_means  <- (get_type(tabs) == "mean") |> purrr::keep(\(x) x) |> names()
  # col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
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
        where(~ get_type(.) %in% c("row", "col", "all", "all_tabs")),
        ~ set_pct(., NA_real_) |> set_type("n") |>
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


      #Calculate percentages
      # pct_formula <- function(x, pct, tot) {
      #   switch(pct,
      #          "row"     =  get_wn(x) / get_wn(tot             ),
      #          "col"     =  get_wn(x) / get_wn(dplyr::last(x)  ),
      #          "all"     =  get_wn(x) / get_wn(dplyr::last(tot)),
      #          "all_tabs"=  get_wn(x) / get_wn(dplyr::last(tot)),
      #          NA_real_)
      # }
      #For each var, the first total column at the right is taken
      tot_cols <- detect_totcols(tabs)


      if (any(pct != "all_tabs")) {
        pct_nat <- pct |> stringi::stri_replace_first_regex("all_tabs", "no") |>
          purrr::set_names(names(pct))

        tabs <- tabs |>
          dplyr::mutate(dplyr::across(
            where(~ is_fmt(.) & !get_type(.) == "mean"),
            ~ set_pct(., pct_formula(
              .,
              pct = pct_nat[[dplyr::cur_column()]],
              tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
            )) |>
              set_display(ifelse(pct_nat[[dplyr::cur_column()]] != "no", "pct", "wn")) |>
              set_type(pct_nat[[dplyr::cur_column()]])
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
                set_display("pct") |> set_type("all_tabs")
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

  type <- get_type(tabs)
  #Calculate diffs (used to color pct depending on spread from row or col mean)
  if (ref[1] != "no" & any(type %in% c("row", "col", "mean")) ) {
    # diff_formula <- function(x, type, dif, refer) {
    #   switch(
    #     ref, #ref[1] before
    #     "tot"   = switch(type,
    #                      "row"     =  get_pct(x)  - get_pct(dplyr::last(x  )),
    #                      "col"     =  get_pct(x)  - get_pct(refer             ),
    #                      "mean"    =  get_mean(x) / get_mean(dplyr::last(x) ),
    #                      NA_real_),
    #     "first" = switch(type,
    #                      "row"     =  get_pct(x)  - get_pct(dplyr::first(x  )),
    #                      "col"     =  get_pct(x)  - get_pct(refer              ),
    #                      "mean"    =  get_mean(x) / get_mean(dplyr::first(x) ),
    #                      NA_real_)
    #   )
    # }

    if (ref[1] == "tot"  ) reference <- detect_totcols(tabs)
    if (ref[1] == "first") {
      reference <- detect_firstcol(tabs)
      reference_cols <- purrr::map_chr(reference, as.character) |> unique()
      reference_cols <- reference_cols[reference_cols != ""]

      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          where(~ get_type(.) == "col") & tidyselect::all_of(reference_cols),
          as_refcol
        ))
      # is_refcol(tabs)

      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          where(~ get_type(.) %in% c("row", "mean")),
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
            where(~ get_type(.) %in% c("row", "col", "mean")),
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
          where(~ get_type(.) %in% c("row", "col", "mean") ) &
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
