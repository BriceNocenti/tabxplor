# PURPOSE: The superseded dplyr-era step functions -- tab_totaltab() / tab_tot() / tab_pct() /
#   tab_ci() / tab_chi2(), plus the machinery only they use.
# ROLE: The pre-2.0.0 step-by-step API, quarantined out of the live pipeline. Exported and still
#   working on an existing tab, hard-deprecated (a lifecycle warning on every call), defunct in
#   2.1.0. Nothing in the build calls a step.
#
#   WHAT IS DEPRECATED IS THE CHAINING API, NOT THE COMPUTATIONS. The arithmetic lives in the
#   aggregate core and is SHARED: tab_ci() calls the same ci_dispatch() / CI_GEOMS (R/tab-agg.R)
#   the two leaves do, and tab_chi2() the same chi2_compute_test() / chi2_write_contrib()
#   (R/tab-chi2.R) leaf_chi2() does -- so a step and a build cannot compute two different answers.
#   Deleting these functions removes a way of ASKING, never a way of computing, and every
#   deprecation message says so: "tab_ci() is going away" would otherwise read as "the confidence
#   interval is".
#
#   WHAT A STEP IS: it RECONSTRUCTS a plan from the table's own fmt markers, because it runs on a
#   table it did not build -- tab_get_vars() / detect_totcols() / detect_refcol() / detect_firstcol()
#   for the structure, the `stars`-from-the-option and `degf`-from-the-columns fallbacks for the
#   inference, and the four tab_match_* / tab_add_* passes that MUTATE the table to make the step's
#   own preconditions true. That reconstruction is the point of these functions, and is why they
#   outlived the pipeline copy.
# KEY CONSTRAINTS:
#   - Exports are unchanged: the @export roxygen travels with the functions.
#   - Nothing here is called BY the core; the shared arithmetic stays where the build uses it.
#   - `degf` is read off the COLUMNS (the smallest design df), never off a table attribute -- that
#     is what lets a table a pipeline has stripped of its metadata still refer its intervals to
#     t(degf) instead of silently falling back to z.
# See: CLAUDE.md § tabxplor architecture (the calculation pipeline).
#' Add total table to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated in 2.0.0, defunct in 2.1.0 -- the total table is built directly by the `totaltab`
#' argument of [tab()]. `tab_totaltab()` still works on an existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param totaltab With subtables (the levels of \code{tab_vars}) : \code{"table"} adds a complete
#' total table, \code{"line"} a total table of a single general-total row, \code{"no"} removes any
#' existing total table.
#' @param name The name of the total table, as a single string.
#' @param data The original database : only useful for mean columns (numeric variables), whose
#' variances --- needed by \code{\link{tab_ci}} --- can only be computed from the microdata.
#'
#' @return A \code{tibble} of class \code{tab}. Total-table rows are then detected with
#' \code{\link{is_tottab}}.
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
  # Hard-deprecated: the message must say the ARITHMETIC is unaffected (see the file header).
  lifecycle::deprecate_warn("2.0.0", "tab_totaltab()", "tab(totaltab = )", details = c(
    "The step-by-step chain is superseded: tab() / tab_num() compute this in one pass.",
    "i" = "The arithmetic is shared, so the numbers are identical -- only the chaining API goes."))

  get_vars  <- tab_get_vars(tabs)

  row_var   <- rlang::sym(get_vars$row_var)
  tab_vars  <- rlang::syms(get_vars$tab_vars)
  mean_vars <- (fmt_var_kind(tabs) == "mean") |> purrr::keep(\(x) x) |> names()


  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  test    <- get_test(tabs)

  if (length(tab_vars) == 0) return(tabs)

  tottab_rows <- is_tottab(tabs)
  if (any(tottab_rows)) tabs <- tabs |>
    tibble::add_column(tottab = tottab_rows) |>
    dplyr::filter(!.data$tottab) |> dplyr::select(-"tottab")

  if (totaltab[1] == "no") return(tabs)

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
      dplyr::mutate(!!row_var := paste("TOTAL", toupper(name)))
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


  # DESIGN: mean columns need the original microdata -- the variances their confidence intervals
  #   rest on cannot be recovered from a finished table.
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
                                                           "table" = vars_chr(row_var),
                                                           "line"  =  "no_row_var") ) ) |>
      dplyr::select(-tidyselect::starts_with("no_row_var")) |>
      dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(.)))

    if (totaltab[1] == "line") mean_calc <- mean_calc |>
      dplyr::mutate(dplyr::across(where(is_fmt), ~ as_totrow(.)))

    totaltable <- switch(
      totaltab[1],
      "table" = dplyr::left_join(dplyr::select(totaltable,
                                               -tidyselect::all_of(mean_vars)),
                                 mean_calc, by = vars_chr(row_var)),
      "line"  = dplyr::left_join(dplyr::select(totaltable,
                                               -tidyselect::all_of(mean_vars)),
                                 mean_calc, by = character())
    )

    totaltable
  }


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
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated in 2.0.0, defunct in 2.1.0 -- totals are built directly by [tab()] (a total row is
#' always computed, one total column shown). `tab_tot()` still works on an existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param tot \code{c("col", "row")} or \code{"both"} print total rows and total columns ;
#'  \code{"row"} or \code{"col"} print only one type ; \code{"no"} removes all totals.
#' @param name  The names of the totals, as a character vector of length one or two
#' (\code{c("Total_row", "Total_column")} to name rows and cols differently).
#' @param totcol \code{"last"} prints a total column for the last factor column variable only ;
#' \code{"each"} prints one for each column variable.
#' @param data The original database : only useful for mean columns, whose total-row variances
#' --- needed by \code{\link{tab_ci}} --- can only be computed from the microdata.
#'
#' @return A \code{tibble} of class \code{tab}. Total rows are then detected with
#'  \code{\link{is_totrow}}, and total columns with \code{\link{is_totcol}}.
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
  # Hard-deprecated: the message must say the ARITHMETIC is unaffected (see the file header).
  lifecycle::deprecate_warn("2.0.0", "tab_tot()", "tab(tot = )", details = c(
    "The step-by-step chain is superseded: tab() / tab_num() compute this in one pass.",
    "i" = "The arithmetic is shared, so the numbers are identical -- only the chaining API goes."))

  stopifnot(
    tot %in% c("no", "row", "col", "both"),
    totcol %in% TAB_ARG_VALUES$totcol$values
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
    cli::cli_warn("A total column needs at least one non-mean col_var; none was added.")
    tot <- dplyr::if_else("row" %in% tot, "row", "no")
  }


  if ("row" %in% tot | tot[1] == "no") {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows

    if (any(totrows)) tabs <- tabs |>
      tibble::add_column(totrows, tottab_line) |>
      dplyr::filter(!.data$totrows | .data$tottab_line) |>
      dplyr::select(-"totrows", -"tottab_line")
  }

  if ("col" %in% tot | tot[1] == "no") tabs <- tabs |>
    dplyr::select(-where(is_totcol))

  if (tot[1] == "no") return(tabs)


  if ("row" %in% tot) {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows

    tabs <- tabs |> tibble::add_column(tottab_rows, tottab_line)

    if (length(groups) != 0) {
      group_vars_totals <-
        dplyr::group_keys(dplyr::filter(tabs, !.data$tottab_line)) |>
        tidyr::unite(!!row_var, sep = " / ") |>
        dplyr::mutate(!!row_var := paste(name[1], !!row_var) |>
                        toupper() |> forcats::as_factor())
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


  if ("col" %in% tot) {
    col_vars_2levels_or_more <-
      col_vars_levels[purrr::map_int(col_vars_levels, length) >= 2]

    if (length(col_vars_2levels_or_more) != 0 | totcol[1] == "last") {
      tabs <- tabs |> dplyr::rowwise()

      if (totcol[1] == "last") {
        # WARNING: dplyr::c_across() does not work here -- splice the level columns instead.
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


# WARNING: on mean columns `diff` stores a RATIO (cell_mean / ref_mean), not a difference --
#   intentional, because the mean breaks (1.15, 1.5, 2, 4) are ratio thresholds. On percentage
#   columns `diff` is the additive difference (cell_pct - ref_pct).
#' Add percentages and diffs to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated in 2.0.0, defunct in 2.1.0 -- percentages, differences and ratios are computed
#' directly by [tab()], through its `pct` / `ref` / `comp` arguments. `tab_pct()` still works on
#' an existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param pct The type of percentages : \code{"row"}, \code{"col"}, \code{"all"} (frequencies of
#' each subtable/group when \code{tab_vars} are provided), or \code{"all_tabs"} (frequencies of
#' the whole set of tables).
#' @param digits The number of digits to print for percentages. As a single integer,
#' or an integer vector the same length than \code{col_vars}.
#' @param ref The reference cell differences and ratios --- and so \code{colors} --- are
#' calculated from : \code{"tot"} (the corresponding total row or column), \code{"first"} (the
#' first cell of the row or column, useful to color temporal developments), an integer (the nth
#' row or column), a string (a regular expression matching one row or column name, precise enough
#' to match only one), or \code{"no"} to skip differences entirely. See \code{\link{tab}} for the
#' full vocabulary.
#' @param comp Comparison level, when \code{tab_vars} are present : \code{"tab"} (the default)
#' compares each cell to the total row of its own subtable, \code{"all"} to the total row of the
#' total table (and, with \code{ref = "first"}, to the first cell of the total table). It doesn't
#' affect column percentages, and must be set once and for all the first time you use
#' \code{\link{tab_pct}} with rows, \code{\link{tab_ci}} or \code{\link{tab_chi2}}.
#' @param color Set to \code{TRUE} to color the resulting tab based on those differences.
#' @param just_diff Set to \code{TRUE} when percentages are already calculated and you only want
#' to recalculate differences.
#'
#' @return A \code{tibble} of class \code{tab}, with percentages displayed, possibly colored based
#' on differences from totals or first cell.
#' @export
tab_pct <- function(tabs, pct = "row",
                    digits = NULL, ref = c("tot", "first", "no"),
                    comp = NULL, color = FALSE, just_diff = FALSE) {
  # Hard-deprecated: the message must say the ARITHMETIC is unaffected (see the file header).
  lifecycle::deprecate_warn("2.0.0", "tab_pct()", "tab(pct = )", details = c(
    "The step-by-step chain is superseded: tab() / tab_num() compute this in one pass.",
    "i" = "The arithmetic is shared, so the numbers are identical -- only the chaining API goes."))

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
        where(~ get_pct_type(.) != "none"),
        ~ set_pct(., NA_real_) |> set_count_col() |>
          set_display("wn")
      ))
      if (length(col_means) == 0) return(tabs)
    }


    if (any(pct == "all_tabs")) {
      if (length(tab_vars) != 0          &
          !(is_tottab(tabs[nrow(tabs),]) &
            is_totrow(tabs[nrow(tabs),]) &
            any(is_totcol(tabs))) ) {
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
        pct_nat <- sub("all_tabs", "no", pct, perl = TRUE) |>
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
              set_pct_type(pct_nat[[dplyr::cur_column()]]) |>
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
                set_display("pct") |> set_scale("level_pct") |> set_pct_type("all_tabs")
            ))
          )
      }

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
  if (ref[1] != "no" & any(type %in% c("row", "col", "mean")) ) {

    if (ref[1] == "tot"  ) reference <- detect_totcols(tabs)
    if (ref[1] == "first") {
      reference <- detect_firstcol(tabs)
      reference_cols <- purrr::map_chr(reference, as.character) |> unique()
      reference_cols <- reference_cols[reference_cols != ""]

      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          where(~ get_pct_type(.) == "col") & tidyselect::all_of(reference_cols),
          as_refcol
        ))

      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          where(~ get_pct_type(.) == "row" | fmt_var_kind(.) == "mean"),
          ~ as_refrow(., dplyr::row_number() == 1 &
                        (comp == "tab" | (comp == "all" & is_tottab(.)) ) )
        ))
    }

    if ( comp == "all" & any(type %in% c("row", "mean")) ) {
      tabs <- tabs |>
        dplyr::with_groups(
          NULL,
          ~ dplyr::mutate(., dplyr::across(
            where(~ get_pct_type(.) %in% c("row", "col") | fmt_var_kind(.) == "mean"),
            ~ set_diff(., diff_formula(
              .,
              type = type[[dplyr::cur_column()]],
              ref = ref[1],
              refer  = rlang::eval_tidy(reference[[dplyr::cur_column()]])
            )) |> set_ref_type(ref[1])
          ))
        )

    } else {
      tabs <- tabs |>
        dplyr::mutate(dplyr::across(
          where(~ get_pct_type(.) %in% c("row", "col") | fmt_var_kind(.) == "mean") &
            !( where(is_totcol) &
                 tidyselect::any_of(names(reference)[reference == ""]) ),
          ~ set_diff(., diff_formula(
            .,
            type = type[[dplyr::cur_column()]],
            ref = ref[1],
            refer = rlang::eval_tidy(reference[[dplyr::cur_column()]])
          )) |> set_ref_type(ref[1])
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



# DESIGN: the interval is stored as a HALF-WIDTH (a margin of error, z * sqrt(variance)), not as a
#   full interval, on the 0-1 scale for percentages (x100 in format()). A NEGATIVE value means the
#   difference is not significant -- that sign is what the colour engine reads.
#' Add confidence intervals to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated in 2.0.0, defunct in 2.1.0 -- confidence intervals are computed directly by [tab()],
#' through its `ci` / `ci_method` / `conf_level` / `stars` arguments. `tab_ci()` still works on an
#' existing tab, reconstructing that plan from the table's own markers.
#'
#' @param tabs A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param ci What the interval is anchored on : \code{"ref"} (the comparison with the reference
#'  cell), \code{"cell"} (the cell's own value), \code{"no"}, or \code{"auto"} --- a comparison
#'  interval for means and row/column percentages, a cell interval for plain frequencies.
#'  \code{"diff"} and \code{"ratio"} are the older spellings of \code{"ref"}. With
#'  \code{ci = "cell"} the result prints as `[inf;sup]`; `display = "base_moe"` writes it as
#'  `pct +- margin of error` instead. See \code{\link{tab}}, which is where this is normally set.
#' @param comp Comparison level, when \code{tab_vars} are present : the interval compares within
#'  each subtable/group (by default, \code{comp = "tab"}) or over the whole set of tables
#'  (\code{comp = "all"}). It must be set once and for all the first time you use
#'  \code{\link{tab_pct}} with rows, \code{\link{tab_ci}} or \code{\link{tab_chi2}}.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
#' @param stars Logical (opt-in; default \code{FALSE}, or `options("tabxplor.stars")` when \code{NULL}).
#' Print per-cell significance stars for the difference from the reference, read from the same
#' interval that is displayed, so the stars and the bracket never disagree.
#' @param ci_method The method of each kind of interval, as ONE named vector
#' (\code{c(cell = , diff = , mean_diff = , mean_ratio = )}, partial) -- see \code{\link{tab}}. The
#' \code{cell} slot also takes \code{"beta"} (Korn-Graubard), the textbook design-based cell
#' interval, conservative near 0 and 1.
#' @param method_cell,method_diff `r lifecycle::badge("deprecated")` Use
#' \code{ci_method = c(cell = , diff = )} instead.
#' @param degf The design's degrees of freedom, the reference distribution of every interval
#' (\code{#PSU - #strata}). \code{NULL} (default) takes the value the table itself carries when it
#' was built from a \code{survey::svydesign}; \code{Inf} is the large-sample normal pivot.
#' @param ci_scale The scale a comparison interval is expressed on: \code{"diff"} (default, a
#' difference interval, neutral 0) or \code{"ratio"} (a ratio interval, neutral 1 --- Katz's
#' log-risk-ratio for proportions, a ratio of means for numeric variables). \code{tab()} sets it
#' from the colour: the measure the reader sees owns the interval.
#' @param color The type of colors to print, as a single string: \code{"no"} (the default),
#' \code{"diff_ci"} (colour percentages and means by their difference from the total or first cell,
#' dropping the colour when the interval of that difference is wider than the difference itself) or
#' \code{"after_ci"} (idem, but cutting the interval off the difference first) --- the 1.x spelling
#' of \code{\link{tab}}'s \code{color = "difference"} plus \code{color_signif} set to
#' \code{"grey_non_signif"} / \code{"guaranteed_effect"}.
#' @param visible By default confidence intervals are calculated and used to set colors,
#' but not printed. Set to \code{TRUE} to print them in the result.
#'
#' @inheritSection tab Significance stars
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
  # Hard-deprecated: the message must say the ARITHMETIC is unaffected (see the file header).
  lifecycle::deprecate_warn("2.0.0", "tab_ci()", "tab(ci = )", details = c(
    "The step-by-step chain is superseded: tab() / tab_num() compute this in one pass.",
    "i" = "The arithmetic is shared, so the numbers are identical -- only the chaining API goes."))
  ci_method <- resolve_ci_method(ci_method, method_cell, method_diff, "tab_ci")
  if (is.null(degf)) degf <- tab_inference_degf(tabs)
  stopifnot(all(ci_scale %in% c("diff", "ratio")),
            all(comp %in%  c("tab", "all"))
  )
  # WARNING: TAB_CI_STEP_VALUES (R/tab-resolve.R) is a SEPARATE declaration from resolve_ci_value()
  #   because this step speaks the COMPUTATIONAL vocabulary, where `"diff"` is a native word and not
  #   the deprecated anchor spelling -- routing it through the public resolver would fire a
  #   deprecation on tabxplor's own build. `"ref"` is the anchor synonym for that branch.
  bad_ci <- !ci %in% TAB_CI_STEP_VALUES
  if (any(bad_ci))
    cli::cli_abort(c("Unknown {.arg ci} value {.val {unique(ci[bad_ci])}}.",
                     "i" = "Valid: {.val {TAB_CI_STEP_VALUES}}."))
  ci[ci == "ref"] <- "diff"
  # `ci = "ratio"` is a comparison interval on the ratio (Katz) scale, independent of colour.
  if (any(ci == "ratio")) {
    ci_scale <- rep_len(ci_scale, length(ci))
    ci_scale[ci == "ratio"] <- "ratio"
    ci[ci == "ratio"] <- "diff"
  }
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

  base   <- get_pct_type(tabs)
  vkind  <- fmt_var_kind(tabs)
  is_rm  <- base == "row" | vkind == "mean"          # the reference is a ROW
  ci_able <- vkind == "mean" | base != "none"        # a count / a coefficient carries no cell CI
  tot_cols <- detect_totcols(tabs)
  tot_cols[is.na(ci)] <- list(rlang::sym(""))
  names_totcols <- tot_cols |> purrr::map_chr(as.character) |> unique() |>
    purrr::discard(\(s) s == "")

  ref <- get_ref_type(tabs)
  # DESIGN: the comparison interval must be taken against the SAME column the difference and the
  #   colour are taken against -- hence detect_refcol(). `ref = "tot"` uses tot_cols instead.
  ref_cols  <- detect_refcol(tabs)
  ref_cols[is.na(ci)] <- list(rlang::sym(""))

  ref_cols <- dplyr::if_else(ref == "tot",
                             true  = tot_cols,
                             false = ref_cols     ) |>
    purrr::set_names(names(ref))
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
    if ( any(ci == "diff_col" ) ) tabs <- tabs |> tab_add_totcol_if_no()
    if ( any(ci == "diff_row") ) {
      tabs <- switch(comp[1],
                     "tab" = tabs |> tab_match_groups_and_totrows(),
                     "all" = tabs |> dplyr::ungroup()               )
    }

    # group_last_pos(mask): per SUBTABLE, the ABSOLUTE index of that group's last masked row,
    # broadcast to the whole group (NA if none) -- `.[dplyr::last(which(<mask>))]` under grouping.
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
      # DESIGN: each cell's OWN unweighted base -- NA'd on the reference cell (so its own interval is
      #   not computed) ONLY where the interval is a comparison, which is CI_GEOMS$ref_cell's call:
      #   a `ci = "cell"` interval has no reference, so every cell keeps its base.
      ref_na <- identical(ci_geom_ref_cell(if (identical(ci[[nm]], "cell")) "cell" else "diff",
                                           if (identical(tp, "mean")) "mean" else "pct",
                                           ci_scale[1]), "na")
      x_n[[nm]] <- dplyr::if_else(
        rtona & ref_na, NA_integer_,
        # every proportion's base is `tot_n`, a mean's is `n` -- one arm per KIND of column.
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

      # WARNING: the weighted rule is a WEIGHTED estimate (get_pct() / get_mean()) on an UNWEIGHTED
      #   base (x_n). The reference cell's x_n is NA, so it is never compared with itself.
      kind_1 <- if (identical(ci[[nm]], "cell")) "cell" else "diff"
      vk_1   <- if (identical(tp, "mean")) "mean" else "pct"
      # a MODEL-based mean method pools one dispersion over the whole variable, which the elementwise
      # engines cannot see: compute it per sub-table, over the rows that are levels (ci_pool_disp()).
      pslot <- if (identical(ci_scale[1], "ratio")) "mean_ratio" else "mean_diff"
      pool  <- if (identical(vk_1, "mean") && identical(kind_1, "diff") &&
                   identical(ci_method[[pslot]], CI_POOLED[[pslot]]))
                 ci_pool_disp(n = x_n[[nm]], mean = get_mean(col), var = get_var(col),
                              by = gid, use = !is_totrow(col), kind = pslot)
      res <- ci_dispatch(
        kind = kind_1, var_kind = vk_1, ci_scale = ci_scale[1],
        est = if (vk_1 == "mean") get_mean(col) else get_pct(col),
        base = x_n[[nm]], var = get_var(col),
        ref = ref[[nm]], ref_var = ref_var[[nm]], ref_n = ref_n[[nm]],
        n_raw = get_tot_n(col),
        conf_level = conf_level, want_p = isTRUE(stars),
        method = ci_method, degf = degf, pool = pool)
      ci_inf[[nm]] <- res$inf; ci_sup[[nm]] <- res$sup; pvalue[[nm]] <- res$pvalue

    }

    # All three writes below are ROW-WISE, so they run ungrouped and the grouping is restored after.
    diff_row_any <- any(ci == "diff_row")
    comp_all_val <- comp[1] == "all"
    vis_mask     <- visible & ci != "no"
    visible_cols <- names(visible)[!is.na(vis_mask) & vis_mask]
    display      <- stats::setNames(rep(list("ci"), length(visible_cols)), visible_cols)
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
        # WARNING: a GROUPED mutate materialises `wn` when it recombines. These writes are done
        #   ungrouped, so reproduce it by hand on the columns a grouped write would have touched.
        if (length(grp) > 0L && (diff_row_any || nm %in% visible_cols))
          col <- fmt_materialize_wn(col)
        col
      }))
    if (length(grp)) tabs <- dplyr::group_by(tabs, dplyr::across(dplyr::all_of(grp)), .drop = drp)


    ci_with_ref <- sub("_row|_col", "", ci_with_ref, perl = TRUE)
    # WARNING: adding a contrast interval to a percentage column CHANGES WHAT THAT COLUMN IS
    #   (`level_pct` -> `points`). This stamps that, not the argument it was asked with, and every
    #   reader (ci_center(), format()'s bracket, the colour significance gate, the legend, the
    #   forest-plot axis) reads the stamp. A `cell` interval changes nothing: a mean with its own
    #   interval is still a mean. Scale and method are stamped for the WHOLE col_var, totals and
    #   reference columns included -- their bounds are NA, and THAT is what says "no interval here".
    ci_yes_ref  <- !is.na(ci_with_ref) & !ci_with_ref == "no"
    ci_var_kind <- function(col) if (identical(fmt_var_kind(col), "mean")) "mean" else "pct"

    # `color` may still arrive as a 1.x combined string here: decode it ONCE into the clean
    # (measure, policy) pair, so the stored attributes stay clean and no reader re-parses one.
    col_dec <- color_decode_legacy(color[1])
    set_ci_col <- !is.null(color[1]) && !color[1] %in% c("no", "")
    tabs[ci_yes_ref] <-
      purrr::map2_df(tabs[ci_yes_ref],
                     ci_with_ref[ci_yes_ref],
                     function(col, ci_ref) {
                       vk <- ci_var_kind(col)
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


  # DESIGN: this step COMPUTES the intervals, so it owns their level -- otherwise
  #   tab_ci(conf_level = 0.99) would store 99 % bounds under the leaf's 95 % stamp.
  tabs <- tab_stamp_inference(tabs, conf_level)

  # WARNING: `meta` must be passed EXPLICITLY -- otherwise a step chain silently drops `vars` /
  #   `ci_settings` / `render_extras` / `color_breaks` / `reg_meta` on the grouped branch.
  tab_restore(tabs, tabs, attrs = list(subtext = subtext, test = test, meta = get_meta(tabs)))
}



#' Add Chi2 summaries to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated in 2.0.0, defunct in 2.1.0 -- the whole-table test and the per-cell contributions are
#' computed directly by [tab()], through its `test` and `color` arguments. `tab_chi2()` still works
#' on an existing tab, reconstructing that plan from the table's own markers.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab}}.
#' @param calc Which elements of the Chi2 summary to compute, as a selection in
#' \code{c("ctr", "p", "var", "counts")} : contributions to variance, pvalue, variance and
#' unweighted count. All of them by default.
#' @param comp Comparison level, when \code{tab_vars} are present : contributions to variance are
#'  calculated for each subtable/group (by default, \code{comp = "tab"}) or for the whole set of
#'  tables (\code{comp = "all"}). It must be set once and for all the first time you use
#'  \code{\link{tab_pct}} with rows, \code{\link{tab_ci}} or \code{\link{tab_chi2}}.
#' @param color The type of colors to print, as a single string: \code{"no"} (the default),
#' \code{"all"} (color all cells by their contribution to variance, except mean columns),
#' \code{"all_pct"} (all percentage cells), or \code{"auto"} (only columns with counts,
#' \code{pct = "all"} or \code{pct = "all_tabs"}).
#' @param .deff Internal pipeline seam. The design-based omnibus grid (one row per subtable x
#' col_var, carrying Rao-Scott's mean generalized design effect), used as the divisor of the
#' \code{color = "contrib"} residual's base when the table's inference basis is not \code{"n"}.
#' \code{NULL} --- the default, and every direct call --- keeps the unweighted base.
#' @return A \code{tibble} of class \code{tab}, with Chi2 summaries as metadata,
#' possibly colored based on contributions of cells to variance.
#' @export
tab_chi2 <- function(tabs, calc = c("ctr", "p", "var", "counts"),
                     comp = NULL, color = c("no", "auto", "all", "all_pct"),
                     .deff = NULL
) {
  # Hard-deprecated: the message must say the ARITHMETIC is unaffected (see the file header).
  lifecycle::deprecate_warn("2.0.0", "tab_chi2()", "tab(test = )", details = c(
    "The step-by-step chain is superseded: tab() / tab_num() compute this in one pass.",
    "i" = "The arithmetic is shared, so the numbers are identical -- only the chaining API goes."))
  get_vars        <- tab_get_vars(tabs)
  row_var         <- get_vars$row_var
  col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)

  stopifnot(all(calc %in% c("all", "ctr", "p", "var", "counts")))
  if ("all" %in% calc) calc <- c("ctr", "p", "var", "counts")
  subtext         <- get_subtext(tabs)

  # DESIGN: "is there a real col_var?" and "is there a row axis?" are two different questions,
  #   asked as two -- the declared col_var set, then the placeholder-name predicate.
  if (!any(is_real_col_var(get_col_var(tabs))) |
      any(is_placeholder_var(names(tabs)))
  ) return(tabs)

  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs |> tab_match_comp_and_tottab(comp)

  is_a_mean <-
    purrr::map_lgl(col_vars_levels, function(levs) {
      cols <- purrr::map_chr(levs, rlang::as_name)
      any(vapply(cols, function(cc) fmt_var_kind(tabs[[cc]]) == "mean", logical(1)))
    })
  # DESIGN: mean col_vars get an ANOVA F (the chi2 mirror), so an all-means table is not skipped --
  #   only the factor-oriented total-row / total-col scaffolding is.
  if (!all(is_a_mean)) {
    tabs <- tabs |> tab_match_groups_and_totrows() |> tab_add_totcol_if_no()
  }

  if (comp == "all") tabs <- tabs |> dplyr::ungroup()

  tot_cols <- detect_totcols(tabs)


  all_col_tot <- names(col_vars_levels) == "all_col_vars"

  tot_cols_names <- purrr::map_lgl(tabs, is_totcol)
  tot_cols_names <- tot_cols_names[tot_cols_names] |> names()
  col_vars_levels_no_tot <-
    purrr::map(col_vars_levels,~ purrr::discard(., . %in% tot_cols_names ) )



  if ("ctr" %in% calc | "var" %in% calc) {
    tabs <- chi2_write_contrib(tabs, calc, comp, color, col_vars_levels,
                               col_vars_levels_no_tot, is_a_mean, all_col_tot, tot_cols,
                               deff = .deff)
  }

  test_tbl <- chi2_compute_test(tabs, comp, row_var, col_vars_levels,
                                col_vars_levels_no_tot, is_a_mean, all_col_tot)

  tabs <- tabs |> dplyr::select(-tidyselect::any_of("tottabs"))

  # `meta` passed explicitly -- see the twin tail in tab_ci().
  tab_restore(tabs, tabs, attrs = list(subtext = subtext, test = test_tbl, meta = get_meta(tabs)))
}



# === SECTION: the steps' own machinery ============================================================
# ⚠ detect_totcols() is deliberately NOT here: it has one live caller, tab_base_n_pct() on the
#   EXPORTER path (R/tab.R), so it stays in R/fmt_class.R beside the other fmt-marker readers.

#' @keywords internal
tab_match_groups_and_totrows <- function(tabs) {
  groups   <- dplyr::group_vars(tabs)

  ind <- dplyr::group_indices(tabs) # 1 1 1 if data isn't grouped
  end_groups <- append(ind[-length(ind)] != ind[-1], FALSE)
  if (any(is_totrow(tabs)) & all(is_totrow(tabs)[end_groups]) ) {return(tabs)}

  if ( !any(is_totrow(tabs))) {


    if (length(groups) != 0) {
      return(dplyr::group_by(tabs, !!!rlang::syms(groups)) |> tab_tot("row"))
    } else if ( !any(is_tottab(tabs)) ) { #If there are no groups
      return(tab_tot(tabs, "row"))
    } else {
      tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
      return(dplyr::group_by(tabs, !!!tab_vars) |> tab_tot("row"))
    }

  } else {
    if (utils::tail(is_totrow(tabs), 1L)) return(dplyr::ungroup(tabs))


    tabs_totrow_groups <- tabs |> dplyr::ungroup() |>
      (\(d) tibble::add_column(d, totrow_groups = as.integer(is_totrow(d))))() |>
      dplyr::mutate(totrow_groups = 1 + cumsum(.data$totrow_groups) - .data$totrow_groups)
    totrow_indices <- tabs_totrow_groups$totrow_groups

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

    return(dplyr::relocate(tabs_totrow_groups, .data$totrow_groups, .before = 1) |>
             dplyr::group_by(.data$totrow_groups)
    )

  }

}



#' @keywords internal
tab_add_totcol_if_no <- function(tabs) {
  if (!any(is_totcol(tabs)) & ! all(fmt_var_kind(tabs) == "mean")) {
    tabs <- tabs |> tab_tot("col", totcol = "last")
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
      comp <- "all"
    }
    if (comp == "all" & all(!comp_all_no_na) ) {
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

# The REFERENCE column of each col_var group -- the one marked `refcol`, falling back to the group's
# first column, so it is identical to detect_firstcol() whenever the reference IS the first level (or
# is unmarked). Only a per-col_var reference that is neither the first level nor the total differs.
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
