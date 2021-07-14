#Ajouter : - Rename levels "Total" and "Ensemble"
#          - like tab and tab_multi, putting NA in a variable set no_var
#' Prepare data for \code{\link{tab_df}}.
#' @param data A dataframe.
#' @param row_var,col_var,tab_vars Variables must be the same that will then pass in
#' \code{\link{tab_df}}.
#' @param show_na When \code{TRUE}, \code{NA} values are replaced by explicit
#' \code{"NA"} values.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param rare_to_other When set to \code{TRUE}, levels with less count
#' than minimum_headcount will be merged into an "Other" level.
#' @param minimum_headcount The minimum unweighted count in each variable.
#'
#' @export
#' @examples
#' \dontrun{
#' forcats::gss_cat %>%
#'   tab_prepare(marital, relig, race, rare_to_other = TRUE) %>%
#'   tab_df(marital, relig, race, perc = "col") %>%
#'   tab_draw(totaltab = "line") %>%
#'   tab_xl(compact = TRUE)
#'   }
tab_prepare <-
  function(data, row_var, col_var, ...,
           show_na = TRUE, cleannames = TRUE,
           rare_to_other = FALSE, minimum_headcount = 30) {

    if (missing(row_var)) {
      data %<>% dplyr::mutate(no_row_var = factor("n"))
      row_var <- rlang::expr(no_row_var)
    } else {
      row_var <- rlang::ensym(row_var)
    }

    if (missing(col_var)) {
      data %<>% dplyr::mutate(no_col_var = factor("n"))
      col_var <- rlang::expr(no_col_var)
    } else {
      col_var <- rlang::ensym(col_var)
    }

    if (missing(...)) {
      data %<>% dplyr::mutate(no_tab_vars = factor(" "))
      tab_vars <- rlang::exprs(no_tab_vars)
    } else {
      tab_vars <- rlang::enquo(tab_vars) %>% ensym_c()
    }

    if (as.character(tab_vars)[[1]] == "no_tab_vars") {no_tab_vars <- TRUE} else {no_tab_vars <- FALSE}


    data <- data %>% dplyr::mutate(dplyr::across(
      !!!tab_vars | ((!!row_var | !!col_var) & !where(is.numeric)),
      as.factor
    ))


    data <- data %>%
      tab_sup_prepare(!!row_var, !!col_var, !!!tab_vars,
                      drop_sup_na = ! show_na, cleannames = cleannames,
                      rare_to_other = rare_to_other,
                      minimum_headcount = minimum_headcount)

    if (rare_to_other == TRUE & no_tab_vars == FALSE) {
      # We only count third variable's minimum headcount for the row variable,
      #  otherwise we get problems.
      levelsrow_var <- dplyr::pull(data, !!row_var) %>%
        levels() %>%
        append("Autres") %>%
        unique()

      data <- data %>% dplyr::group_by(!!!tab_vars) %>%
        dplyr::mutate(!!row_var := forcats::fct_lump_min(!!row_var, minimum_headcount,
                                                      other_level = "Autres")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(!!row_var := forcats::fct_relevel(!!row_var, levelsrow_var))

    }
    return(data)
  }





# Fill base fmt elements while creating the tabs. Fill others using vector attributes for
# rows and cols (digits, type, pct). Save performance and make it easy for tabxl after.
# => Fill digits, type, pct, just before printing (less powerful ?)?

# Not working with list of tabs, but with grouped_df (add empty rows while printing ?).
# => All attributes are on the main level (digits, types, pct are int or char matrixes).

# Group over factors only, to let the opportunity to the user to add character vars ?


#Do tidy-dots work with tribble and pmap ?
# tibble::tribble(~data, ~row_var, ~col_var, ~..., ~wt,
#                 dat_group123, rlang::as_label(row_var), rlang::as_label(col_var), as.character(tab_vars), rlang::as_label(wt)) %>%
#   purrr::pmap(tab_core)
tab_core <- function(data, row_var, col_var, ..., wt,
                     digits = 0, subtext = "", is_grouped = FALSE) {
  if (missing(row_var)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("n"))
    row_var <- rlang::expr(no_row_var)
  } else {
    row_var <- rlang::ensym(row_var)
    if (as.character(row_var) %in% c("NA", "NULL", "no")) {
      data <- data %>% dplyr::mutate(no_row_var = factor("n"))
      var1 <- rlang::expr(no_row_var)
    }
  }

  if (missing(col_var)) {
    data <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_var <- rlang::expr(no_col_var)
  } else {
    col_var <- rlang::ensym(col_var)
    if (as.character(col_var) %in% c("NA", "NULL", "no")) {
      data <- data %>% dplyr::mutate(no_col_var = factor("n"))
      col_var <- rlang::expr(no_col_var)
    }
  }

  if (missing(...)) {
    data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
    tab_vars <- rlang::exprs(no_tab_vars)
  } else {
    tab_vars <- rlang::enquos(...) %>% ensyms_c() #rlang::ensyms(...)
    NA_tab_vars <- purrr::map_lgl(tab_vars, ~ as.character(.) %in% c("NA", "NULL", "no"))
    if (all(NA_tab_vars) ) {
      data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
      tab_vars <- rlang::exprs(no_tab_vars)
    }
  }

  if (missing(wt)) {
    data <- data %>% dplyr::mutate(no_weight = 1)
    wt <- rlang::expr(no_weight)
  } else {
    wt <- rlang::ensym(wt)
    if (as.character(wt) %in% c("NA", "NULL", "no")) {
      data <- data %>% dplyr::mutate(no_weight = 1)
      wt   <- rlang::expr(no_weight)
    }
  }

  #dat_prepare : all filters here (operations on rows copy all columns on memory)
  #remove all unwanted NAs : those we want to keep were turned to explicit before
  data <- data %>% dplyr::select(!!row_var, !!col_var, !!!tab_vars, !!wt) %>%
    dplyr::with_groups(NULL,
                       ~ dplyr::filter(., dplyr::across(c(!!row_var, !!col_var, !!!tab_vars),
                                                        ~ ! is.na(.)))) %>%
    dplyr::mutate(!!wt := as.numeric(!!wt))

  row_var_type <- ifelse ("numeric" %in% class(dplyr::pull(data, !!row_var) ), "numeric", "factor")
  col_var_type <- ifelse ("numeric" %in% class(dplyr::pull(data, !!col_var) ), "numeric", "factor")
  if (row_var_type == "numeric" & col_var_type == "numeric") {
    stop("row_var and col_var are both numeric : only one of them can be")
  }
  type <- ifelse(row_var_type == "numeric" | col_var_type == "numeric", "numeric", "factor")

  if (type == "numeric") {
    num_var <- switch(row_var_type, "numeric" = row_var, "factor" = col_var)
    fct_var <- switch(row_var_type, "numeric" = col_var, "factor" = row_var)
  }

  if (! is_grouped) {
    data <- switch(type,
                  "factor"   = dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var),
                  "numeric"  = dplyr::group_by(data, !!!tab_vars, !!fct_var     ) )
  }

  if (type == "numeric") {
    if (rlang::as_name(num_var) %in% dplyr::group_vars(data)) {
      data <- dplyr::ungroup(data, !!num_var)
    }
  }

  tabs <-
    switch(type,
           "factor"  = data %>%
             dplyr::summarise(nums = new_fmt(type    = "wn",
                                              digits  = as.integer(digits),
                                              n       = dplyr::n(),
                                              wn      = sum(!!wt),
                                              col_var = rlang::as_name(col_var)
             ),
             .groups = 'drop') %>%
             tidyr::pivot_wider(names_from = !!col_var, values_from = nums,
                                values_fill = fmt0("wn", digits)),

           "numeric" = data %>%
             dplyr::summarise(!!num_var := new_fmt(
               type    = "mean",
               digits  = as.integer(digits),
               n       = dplyr::n(),
               wn      = sum(!!wt),
               mean    = weighted.mean(!!num_var, !!wt, na.rm = TRUE),
               var     = weighted.var  (!!num_var, !!wt, na.rm = TRUE),
               col_var = rlang::as_name(col_var)
             ),
             .groups = "drop")
    )

  if (row_var_type == "numeric") tabs <- tabs %>%
    tidyr::pivot_wider(names_from = !!fct_var, values_from = !!num_var,
                       values_fill = fmt0("mean", digits))


  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                   ~ nlevels(forcats::fct_drop(.)) == 1))

    if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext) %>%
        dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs %>% dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext)
  }
}



#To pass a list of arguments in tab_vars : c(tab_row_var, tabcol_var, etc.)
tab_many <- function(data, row_var, ..., tab_vars, wt, levels = "all",
                    digits = 0, drop_sup_na = FALSE, cleannames = FALSE, listed = FALSE,
                    totaltab = "no", tot = "no", pct = "no",
                    ci = "no", conf_level = 0.95, subtext = "") {
stopifnot(levels %in% c("first", "all"),
          is.numeric(digits),
          is.logical(drop_sup_na), is.logical(cleannames), is.logical(listed)
)

  #levels = "first" don’t work anymore ----

  if (missing(row_var)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("n"))
    row_var <- rlang::expr(no_row_var)
  } else {
    row_var <- rlang::ensym(row_var)
    if (as.character(row_var) %in% c("NA", "NULL", "no")) {
      data <- data %>% dplyr::mutate(no_row_var = factor("n"))
      var1 <- rlang::expr(no_row_var)
    }
  }


  sup_cols <- rlang::enquos(...) %>% ensyms_c() #rlang::ensyms(...)
  sup_list <- data %>% tab_make_sup_list(sup_cols)
  sup_cols      <-  sup_list$sup_cols %>% rlang::syms()
  sup_cols_num  <-  sup_list$sup_cols_num
  sup_cols_text <-  sup_list$sup_cols_text

  if (missing(tab_vars)) {
    data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
    tab_vars <- rlang::exprs(no_tab_vars)
  } else {
    tab_vars <- rlang::enquos(tab_vars) %>% ensyms_c()
    NA_tab_vars <- purrr::map_lgl(tab_vars, ~ as.character(.) %in% c("NA", "NULL", "no"))
    if (all(NA_tab_vars) ) {
      data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
      tab_vars <- rlang::exprs(no_tab_vars)
    }
  }

  if (missing(wt)) {
    data <- data %>% dplyr::mutate(no_weight = 1)
    wt <- rlang::expr(no_weight)
  } else {
    wt <- rlang::ensym(wt)
    if (as.character(wt) %in% c("NA", "NULL", "no")) {
      data <- data %>% dplyr::mutate(no_weight = 1)
      wt   <- rlang::expr(no_weight)
    }
    }
  # print(tab_vars) ; print(row_var) ; print(wt) ; print(sup_cols)

  #na : one_by_one of for_all !----

  # Vectorise arguments : there can be either one argument for all tabs, or one for each
  nvars <- length(sup_cols)
  lvs          <- vec_recycle(levels          , nvars, x_arg = "levels"          )
  digits       <- vec_recycle(digits          , nvars, x_arg = "digits"          )
  #drop_sup_na  <- vec_recycle(drop_sup_na     , nvars, x_arg = "drop_sup_na"     )
  #cleannames   <- vec_recycle(cleannames      , nvars, x_arg = "cleannames"      )
  totaltab     <- vec_recycle(totaltab        , nvars, x_arg = "totaltab"        )
  tot          <- vec_recycle(tot             , nvars, x_arg = "tot"             )
  pct          <- vec_recycle(pct             , nvars, x_arg = "pct"             )
  ci           <- vec_recycle(ci              , nvars, x_arg = "ci"              )
  conf_level   <- vec_recycle(conf_level, nvars, x_arg = "conf_level")


  #Prepare the data with explicits NA, etc.
  data <- data %>%
    tab_sup_prepare(!!!sup_cols, drop_sup_na = drop_sup_na, cleannames = cleannames)

  #   Remove all the NAs of explanatory variables
  #if drop_sup_na == FALSE, all factors (not numeric) where made explicit with dat_prepare :
  #print(dplyr::filter_all(data, ~ is.na(.)))
  #data %>% purrr::map(~ is.na(.) %>% which() %>% length()) %>% print()

  data <- data %>% dplyr::select(!!!tab_vars, !!row_var, !!wt, !!!sup_cols)
  data <- data %>% dplyr::filter_if(is.factor, ~ !is.na(.))

  #   Where only first levels are kept, merge others to minimise useless calculations
  lv1 <- lvs == "first" & sup_cols_text
  if (any(lv1)) {
    sup_cols_3levels <-
      purrr::map_lgl(dplyr::select(data, !!!sup_cols),
                     ~ is.factor(.) & nlevels(.) >= 3) & lv1

    remove_levels <- purrr::map(dplyr::select(data, !!!sup_cols[lv1]),
                                ~ levels(.) %>% .[-1])

    if (any(sup_cols_3levels)) {
      data <- data %>%
        dplyr::mutate(dplyr::across(
          !!!sup_cols[sup_cols_3levels],
          ~ forcats::fct_other(., keep = levels(.)[1], other_level = "Autres")
        ))
    }
  }


  #Make the tables and store them in a list
  #dat_group3 <- data %>% dplyr::group_by(!!!tab_vars, .add = TRUE, .drop = FALSE)
  tabs <-
    purrr::pmap(list(sup_cols, digits),
                function(.sup_cols, .digits)
                  tab_core(data, !!row_var, !!.sup_cols, !!!tab_vars, wt = !!wt,
                           digits = .digits)) %>%
    purrr::set_names(sup_cols)


  #Add total table, total rows and cols, percentages, confidence intervals
  # Conditions :
  if (length(tot[sup_cols_num]) != 0) {
    if (tot[sup_cols_num] %in% c("both", "row_col", "col_row")) tot[sup_cols_num] <- "row"
    if (tot[sup_cols_num] == "col")                             tot[sup_cols_num] <- "no"
  }

  ctt  <- totaltab != "no"
  ctot <- tot      != "no"
  cpct <- pct      != "no" & sup_cols_text
  cci  <- ci       != "no" & sup_cols_text

  tabs[ctt ] <- tibble::tibble(tabs, totaltab      ) %>% dplyr::filter(ctt ) %>% purrr::pmap(tab_totaltab)
  tabs[ctot] <- tibble::tibble(tabs, tot           ) %>% dplyr::filter(ctot) %>% purrr::pmap(tab_tot     )
  tabs[cpct] <- tibble::tibble(tabs, pct           ) %>% dplyr::filter(cpct) %>% purrr::pmap(tab_pct     )
  tabs[cci ] <- tibble::tibble(tabs, ci, conf_level) %>% dplyr::filter(cci ) %>% purrr::pmap(tab_ci)


  #Remove unwanted levels (keep only the first when levels = "first")
  if (any(lv1)) tabs[lv1] <- tabs[lv1] %>%
    purrr::map2(remove_levels,
                ~ dplyr::select(.x, -tidyselect::all_of(.y), -tidyselect::any_of("Total")))


  #Join the list of tabs into a single table, managing duplicated levels
  if (listed == FALSE) {
    duplicated_levels <- tabs %>%
      purrr::map(~ names(.) %>% purrr::discard(. %in% c(row_var, tab_vars))) %>%
      purrr::flatten_chr() %>% .[duplicated(.)] %>% unique()

    if (length(duplicated_levels) != 0) {
      # warning(paste0("some levels names are the same for different variables : ",
      #                paste0(duplicated_levels, collapse = ", ")))
      tabs <- tabs %>%
        purrr::imap(~ dplyr::rename_with(.x, function(.names)
          dplyr::if_else(.names %in% duplicated_levels, paste0( .names, "_", .y), .names)))
    }

    join_by <- as.character(c(tab_vars, row_var)) %>% purrr::discard(. == "no_tab_vars")
    tabs <- purrr::reduce(tabs, dplyr::full_join, by = join_by)

    if (! lv1_group_vars(tabs)) {

      tabs <- tabs %>% dplyr::group_by(!!!tab_vars)
      groups <- dplyr::group_data(tabs)
      tabs <- new_grouped_tab(tabs, groups = groups, subtext = subtext)
    } else {
     tabs <- new_tab(tabs, subtext = subtext)
    }

  } else {
    tabs <- purrr::map_if(tabs, map_lgl(tabs, dplyr::is_grouped_df),
                          .f    = ~ new_tab(., subtext = subtext),
                          .else = ~ new_grouped_tab(., dplyr::group_data(.),
                                                    subtext = subtext))
  }

  tabs
}


# Do not depend on groups
tab_totaltab <- function(tabs, totaltab = c("table", "line", "no"), name = "Ensemble") {
  get_vars <- tab_get_vars(tabs)
  row_var  <- rlang::sym(get_vars$row_var)
  tab_vars <- rlang::syms(get_vars$tab_vars)

  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  if (length(tab_vars) == 0) return(tabs)

  #Remove the existing total table if there is one
  tottab_rows <- is_tottab(tabs)
  if (any(tottab_rows)) tabs <- tabs %>%
    tibble::add_column(tottab = tottab_rows) %>%
    dplyr::filter(!tottab) %>% dplyr::select(-tottab)

  if (totaltab[1] == "no") return(tabs)

  #Calculate the total table
  totaltable <- switch(
    totaltab[1],
    "table" = tibble::as_tibble(tabs) %>% tibble::add_column(totrow = is_totrow(.)) %>%
      dplyr::filter(!totrow) %>% dplyr::select(-totrow) %>%
      dplyr::group_by(!!row_var) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_tottab(sum(.) ))),

    "line"  = tibble::as_tibble(tabs) %>% tibble::add_column(totrow = is_totrow(.)) %>%
      dplyr::filter(!totrow) %>% dplyr::select(-totrow) %>%
      dplyr::group_by(!!row_var) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), sum)) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(as_tottab(sum(.))))) %>%
      dplyr::mutate(!!row_var := paste("TOTAL", stringr::str_to_upper(name))) #English trad Overall ?
  )

  if (totaltab[1] == "line") {
    tabs <- tabs %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(
        !!row_var,
        levels(dplyr::pull(totaltable, !!row_var))
      ))

    totaltable <- totaltable %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(
        !!row_var, levels(dplyr::pull(tabs, !!row_var))
        ))
  }


  totaltable <-
    purrr::reduce(tab_vars, .init = totaltable,
                  .f = ~ dplyr::mutate(.x, !!.y := factor(name)))






  #Bind the total table to the tabs
  if (lv1_group_vars(tabs)) {
    tabs %>% dplyr::bind_rows(totaltable)
  } else {

    df <- tabs %>% dplyr::bind_rows(totaltable)
    groups <- dplyr::group_data(df)
    new_grouped_tab(df, groups = groups, subtext = subtext, chi2 = chi2)
  }
}



# Depends on groups
# tab_tot("row") can be done on subgroups of the table independently
tab_tot <- function(tabs, tot = c("row", "col"), name = "Total",
                    totcol = c("only_one", "for_each_col_var")
) {
  stopifnot( tot %in% c("no", "row", "col", "both", "row_col", "col_row")  )


   get_vars        <- tab_get_vars(tabs)
  row_var         <- rlang::sym(get_vars$row_var)
  #col_vars        <- rlang::sym(get_vars$col_vars)
  col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  #tab_vars        <- rlang::syms(get_vars$tab_vars)

  groups <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  if (any(c("both", "row_col", "col_row") %in% tot)) tot <- c("row", "col")
  name <- vec_recycle(name, 2)


  #Remove existing totals, except if there is a total table of one line
  if ("row" %in% tot | tot[1] == "no") {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & totrows

    if (any(totrows)) tabs <- tabs %>%
      tibble::add_column(totrows, tottab_line) %>%
      dplyr::filter(!totrows | tottab_line) %>% dplyr::select(-totrows, -tottab_line)
  }

  if ("col" %in% tot | tot[1] == "no") tabs <- tabs %>% dplyr::select(-where(is_totcol))

  if (tot[1] == "no") return(tabs)


  # Total rows
  if ("row" %in% tot) {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & totrows

    tabs <- tabs %>% tibble::add_column(tottab_line)

    if (length(groups) != 0) {
      group_vars_totals <- dplyr::group_keys(dplyr::filter(tabs, !tottab_line)) %>% #dplyr::mutate(bis = PR0) %>%
        tidyr::unite(!!row_var, sep = " / ") %>%
        dplyr::mutate(!!row_var := paste(name[1], !!row_var) %>%
                        stringr::str_to_upper() %>% forcats::as_factor())  #stringr::str_remove_all()
    } else {
      group_vars_totals <- tibble::tibble(!!row_var := factor(name[1]))
    }
    group_vars_totals_levels <- group_vars_totals %>% dplyr::pull(1) %>% levels()

    tabs <- tabs %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(!!row_var, group_vars_totals_levels))

    row_var_levels <- dplyr::pull(tabs, !!row_var) %>% levels()

    totrows <- tabs %>% dplyr::filter(!tottab_line) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(sum(.)) ),
                       .groups = "keep") %>%
      dplyr::bind_cols(group_vars_totals) %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(!!row_var, row_var_levels))

    tabs <- dplyr::bind_rows(tabs, totrows) %>% dplyr::arrange(.by_group = TRUE) %>%
      dplyr::select(-tottab_line)
  }

  #Total columns
  if ("col" %in% tot) {
    col_vars_2levels_or_more <-
      col_vars_levels[purrr::map_int(col_vars_levels, length) >= 2]

    if (length(col_vars_2levels_or_more) != 0 | totcol[1] == "only_one") {
      tabs <- tabs %>% dplyr::rowwise()

      if (totcol[1] == "only_one") {
        # c_across don’t work. Workaround with quosures : sum(!!!col_vars_levels)
        tabs <- tabs %>%
          dplyr::mutate(!!rlang::sym(name[2]) := sum(!!!col_vars_levels[[1]]) %>%
                          as_totcol() %>% set_col_var("all_col_vars"))

      } else if (totcol[1] == "for_each_col_var") {
        totcol_names <- purrr::map(paste0(name[2],"_", names(col_vars_2levels_or_more)),
                                   rlang::sym)
        tabs <-
          purrr::reduce2(col_vars_2levels_or_more, totcol_names, .init = tabs,
                         function(.tab, .levels, .names)
                           dplyr::mutate(.tab, !!.names := sum(!!!.levels) %>% as_totcol())
          )
        tabs <-
          purrr::reduce(names(col_vars_2levels_or_more), .init = tabs, function(.tab, .var)
            dplyr::relocate(
              .tab,
              where(~ tidyr::replace_na(get_col_var(.) == .var & is_totcol(.), FALSE)),
              .after = where(~ tidyr::replace_na(get_col_var(.) == .var & !is_totcol(.),
                                                 FALSE)
              ) ) )
      }

      tabs <- tabs %>% dplyr::group_by(!!!rlang::syms(groups))
    }
  }

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {

    group_dat <- dplyr::group_data(tabs)
    new_grouped_tab(tabs, groups = group_dat, subtext = subtext, chi2 = chi2)
  }
}


tab_match_groups_and_totrows <- function(tabs) {
  #chi2 : not to match groups and totrows with alltabs ? ----

  #tab_vars <- tab_get_vars(tabs)$tab_vars
  groups   <- dplyr::group_vars(tabs)

  #If there is a total_row at the end of each group, keep (un)grouping as is
  ind <- dplyr::group_indices(tabs) # 1 1 1 if data isn’t grouped
  end_groups <- append(ind[-length(ind)] != ind[-1], FALSE)
  if (any(is_totrow(tabs)) & all(is_totrow(tabs)[end_groups]) ) {return(tabs)}

  #If there isn’t any total row, keep actual (un)grouping and add some
  if ( !any(is_totrow(tabs))) {


    if (length(groups) != 0) {
      #if ( !identical(tab_vars, groups) ) {
      warning("no total row(s) found. Some added based on actual grouping variables : ",
              paste(groups, collapse = ", "))
      return(dplyr::group_by(tabs, !!!rlang::syms(groups)) %>% tab_tot("row"))
      # } else {
      #   tabs <- tabs %>% tab_tot("row")
      #   warning("no total row(s) found. One added for the whole table")
      # }
    } else if ( !any(is_tottab(tabs)) ) { #If there are no groups
      warning("no groups and no total row(s) found. One added for the whole table")
      return(tab_tot(tabs, "row"))
    } else {
      warning("no groups and total row(s), but total table found. ",
              "Grouped upon tab_vars and total rows added")
      tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
      return(dplyr::group_by(tabs, !!!tab_vars) %>% tab_tot("row"))
    }

    #If there is at least one total row, calculate new groups based on them
  } else {
    if (tail(is_totrow(tabs), 1L)) return(dplyr::ungroup(tabs))


    tabs_totrow_groups <- tabs %>% dplyr::ungroup() %>%
      tibble::add_column(totrow_groups = as.integer(is_totrow(.))) %>%
      dplyr::mutate(totrow_groups = 1 + cumsum(totrow_groups) - totrow_groups)
    totrow_indices <- tabs_totrow_groups$totrow_groups

    #Control if totrows groups match tab_vars, collectively or individualy, if yes group
    tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
    if ( !identical(tab_vars, groups) ) {
      tabs_tab_vars_groups <- tabs %>% dplyr::group_by(!!!tab_vars)
      tab_vars_indices <- dplyr::group_indices(tabs_tab_vars_groups)

            if (all(totrow_indices == tab_vars_indices)) return(tabs_tab_vars_groups)
    }

    each_tab_var_indices <-
      tabs %>% dplyr::ungroup() %>% dplyr::select(!!!tab_vars) %>%
      dplyr::transmute(dplyr::across(dplyr::everything(), as.integer)) %>%
      purrr::map(~ .)

    each_tab_var_totrow_comp <-
      purrr::map_lgl(each_tab_var_indices, ~ all(. == totrow_indices))

    if (any(each_tab_var_totrow_comp)) {
      group_var_name <- names(each_tab_var_totrow_comp[each_tab_var_totrow_comp])[1]
      return(dplyr::group_by(tabs, !!rlang::sym(group_var_name)))
    }

    # Otherwise return a df grouped with the total rows groups, in a new variable
    warning("grouping variable(s) not corresponding to total_rows : ",
            "new groups calculated, based on actual total_rows")
    return(dplyr::relocate(tabs_totrow_groups, totrow_groups, .before = 1) %>%
             dplyr::group_by(totrow_groups)
    )

  }

}

tab_add_totcol_if_no <- function(tabs) {
  if (!any(is_totcol(tabs)) &
      ! all( purrr::map_lgl(tabs[purrr::map_lgl(tabs, is_fmt)], ~ all(is_mean(.)) ) )
  ) {
    tabs <- tabs %>% tab_tot("col", totcol = "only_one")
    warning("no total column : one was added (from the first column variable)")
  }
  tabs
}



tab_pct <- function(tabs, pct = c("row", "col", "all", "all_tabs", "no")) { #Add keep/change grouping ?
  #stopifnot(pct[1] %in% c("row", "col", "all", "all_tabs", "no"))


  if (pct[1] == "no") {
    tabs <- tabs %>% dplyr::mutate(dplyr::across(
      where(is_fmt),
      ~ dplyr::if_else(
        condition = is_pct(.),
        true  = set_pct(., NA_real_) %>% set_pct_type(NA_character_) %>%
          set_type("wn"),
        false = .)
    ))
    return(tabs)
  }

  #row perc = if many col vars calculate by totals (or unique total...) ! ----

  get_vars         <- tab_get_vars(tabs)
  #row_var         <- rlang::sym(get_vars$row_var) #col_var ??
  #col_vars        <- rlang::sym(get_vars$col_vars)
  # col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  tab_vars         <- rlang::syms(get_vars$tab_vars)

  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)


  #Ready table for percentages (need total rows and cols, compatible grouping)
  switch(pct[1],

         "row"      = tabs <- tabs %>% tab_add_totcol_if_no(),

         "col"      = tabs <- tabs %>% tab_match_groups_and_totrows(),

         "all"      = tabs <- tabs %>% tab_match_groups_and_totrows() %>%
           tab_add_totcol_if_no(),

         "all_tabs" = {
           if ( !(is_tottab(tabs[nrow(tabs),]) &
                  is_totrow(tabs[nrow(tabs),]) &
                  any(is_totcol(tabs))) ) {
             warning("since percentages are 'all_tabs', a total table (tab_totaltab)",
                     "was added")
             if (!is_tottab(tabs[nrow(tabs),])) {
               tabs <- tabs %>% tab_totaltab('line')
             }

             tabs <- tabs %>% dplyr::ungroup() %>%
               tab_match_groups_and_totrows() %>%
               tab_add_totcol_if_no()
           }

           tabs <- tabs %>% dplyr::ungroup()
         }
  )

  #Calculate percentages
  pct_formula <-
    switch(pct[1],
           "row"     = function(x, tot) get_wn(x) / get_wn(tot             ),
           "col"     = function(x, tot) get_wn(x) / get_wn(dplyr::last(x)  ),
           "all"     = function(x, tot) get_wn(x) / get_wn(dplyr::last(tot)),
           "all_tabs"= function(x, tot) get_wn(x) / get_wn(dplyr::last(tot)) )

  #Calculations by the first total_col variable from the right
  #(one calculation per col variable, or useless since row_var counts are all the same ?)
  if (any(is_totcol(tabs))) {
    totvar <- rlang::sym(tail(names(tabs)[is_totcol(tabs)], 1L))
  } else {
    totvar <- rlang::expr(no_tot)
  }

  tabs <- tabs %>%
    dplyr::mutate(dplyr::across(
      where(is_fmt),
      ~ dplyr::if_else(condition = ! is_mean(.),
                       true      = set_pct(., pct_formula(., !!totvar)) %>%
                         set_type(ifelse(pct[1] != "no", "pct", "wn")) %>%
                         set_pct_type(pct[1]),
                       false     = .)
    )) #%>%
  #dplyr::with_groups(NULL, ~ dplyr::mutate(., dplyr::across(
  #  where(is_fmt),
  #  ~ set_pct_type(., pct[1]))))

  tabs <- tabs %>% dplyr::select(-tidyselect::any_of("totrow_groups"))

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext, chi2 = chi2)
  }
}


# ci_formula_factory <- function(y) {
#   function(x, y, zscore) zscore *
#     sqrt( get_pct(x) * (1 - get_pct(x)) / get_n(x)   +   get_pct(y) * (1 - get_pct(y)) / get_n(y) )
# }
#
# ci_formula_gen <- function(ci) {
#   switch(
#     ci,
#     "col"      = ci_formula_factory(tot),
#     "row"      = ci_formula_factory( dplyr::last(x) ),
#     "abs"      = ci_formula_factory(fmt0(pct)),
#     #"totaltab" = function(x, tot, zscore) ,
#     # "r_to_r"   = function(x, nx, y, ny, zscore) ,
#     # "c_to_c"   = function(x, nx, y, ny, zscore) ,
#     # "tab_to_t" = function(x, nx, y, ny, zscore) ,
#     "no"       = function(x, tot, zscore) NA_real_
#   )
# }

weighted.var <- function(x, wt, na.rm = FALSE) {
  #Nwt_non_zero <- length((wt)[wt != 0])
  sum(wt * (x - weighted.mean(x, wt, na.rm = na.rm))^2,  na.rm = na.rm) /
    ( sum(wt, na.rm = na.rm) )
  #((Nwt_non_zero - 1) / Nwt_non_zero) *
} #Same results as sqrt(Hmisc::wtd.var(!!num_var, !!wt, na.rm = TRUE, method = "ML")


zscore_formula <- function(conf_level) {
  # Calculate the z-score for the given confidence level (thanks to mindcrime) :
  # https://datascience.stackexchange.com/questions/10093/how-to-find-a-confidence-level-given-the-z-value
  stopifnot(conf_level >= 0, conf_level <= 1)
  qnorm((1 - conf_level)/2,lower.tail = FALSE)
}



#On differences, it calculates the ci margin (negative numbers mean no significant difference)
tab_ci <- function(tabs,
                   ci = c("abs", "diff_col", "diff_row",
                          "diff_spread_col", "diff_spread_row", "no"),
                   #"totaltab", "r_to_r", "c_to_c", "tab_to_tab",
                   conf_level = 0.95) {
stopifnot(ci[1] %in% c("abs", "diff_col", "diff_row",
                       "diff_spread_col", "diff_spread_row", "no"))

  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  #Ready table for percentages (need total rows and cols, compatible grouping)
  tabs <-
    switch(ci[1],

           "abs"             = tabs,
           "diff_col"        = ,
           "diff_spread_col" =  tabs %>% tab_add_totcol_if_no(),
           "diff_row"        = ,
           "diff_spread_row" = tabs %>% tab_match_groups_and_totrows(),
           "no"              = tabs
    )

  #Problems with ci = "no" ----
  #ci = "no" get means to pct !

  #Formulas :
  zscore <- zscore_formula(conf_level)

  ci_diff <-  function(x, y, zscore) {
    zscore * sqrt( get_pct(x) * (1 - get_pct(x)) / get_n(x) +
                     get_pct(y) * (1 - get_pct(y)) / get_n(y) )
  }

  ci_diff_spread <-  function(x, y, zscore) {
    diff <-  x - y
    - abs(diff) + zscore * sqrt( get_pct(x) * (1 - get_pct(x)) / get_n(x) +
                                           get_pct(y) * (1 - get_pct(y)) / get_n(y) )
  }

  ci_formula <- switch(
    ci[1],
    "abs" = function(x, tot, zscore) zscore * sqrt(get_pct(x)*(1 - get_pct(x))/get_n(x)),
    "diff_col"        = function(x, tot, zscore) ci_diff(x, tot    , zscore),
    "diff_row"        = function(x, tot, zscore) ci_diff(x, dplyr::last(x), zscore),
    "diff_spread_col" = function(x, tot, zscore) ci_diff_spread(x, tot    , zscore),
    "diff_spread_row" = function(x, tot, zscore) ci_diff_spread(x, dplyr::last(x), zscore),
    "no"              = function(x, tot, zscore) NA_real_
  )

  ci_mean_diff <- function(x, y, zscore) {
    zscore * sqrt( get_var(x) / get_n(x) + get_var(y) / get_n(y) )
  }

  ci_mean_diff_spread <-  function(x, y, zscore) {
    diff <-  x - y
    - abs(diff) + zscore * sqrt( get_var(x) / get_n(x) + get_var(y) / get_n(y) )
  }

  ci_mean_formula <- switch(
    ci[1],
    "abs"             = function(x, zscore) zscore * sqrt(get_var(x)/get_n(x)),
    "diff_col"        = function(x, zscore) NA_real_,
    "diff_row"        = function(x, zscore) ci_mean_diff(x, dplyr::last(x), zscore),
    "diff_spread_col" = function(x, zscore) NA_real_,
    "diff_spread_row" = function(x, zscore) ci_mean_diff_spread(x, dplyr::last(x), zscore),
    "no"              = function(x, zscore) NA_real_
  )


  #Calculate the confidence intervals (and change type to "ci")
  #For "diff_col" : calculations by the first total col variable from the right
  if (any(is_totcol(tabs))) {
    totvar <- rlang::sym(tail(names(tabs)[is_totcol(tabs)], 1L))
  } else {
    totvar <- rlang::expr(no_tot)
  }



  tabs <- tabs %>%
    dplyr::mutate(dplyr::across(where(is_fmt), ~ dplyr::if_else(
      condition = is_mean(.),
      true      = set_ci(., ci_mean_formula(.     , zscore)),
      false     = set_ci(., ci_formula(., !!totvar, zscore))
    ) ))

  #Remove the reference of the calculation (total row or total column), and change type
  tabs <-
    switch(ci[1],
           "abs" = tabs %>% dplyr::mutate(dplyr::across(where(is_fmt), ~ dplyr::if_else(
             condition = is_mean(.),
             true      = set_type(., "mean_ci"),
             false     = set_type(., "pct_ci")
           )  )),

           "diff_spread_col" = ,
           "diff_col" = tabs %>% dplyr::mutate(dplyr::across(!!totvar,
                                                             ~ set_ci(., NA_real_))),
           "diff_spread_row" = ,
           "diff_row" = tabs %>%
             dplyr::mutate(dplyr::across(
               where(is_fmt),
               ~ set_ci(., dplyr::if_else(
                 condition = dplyr::row_number() == dplyr::n(),
                 true      = vec_recycle(NA_real_, length(.)),
                 false     = get_ci(.)  ))
             )),

           "no"  = tabs %>%
             dplyr::mutate(dplyr::across(
               where(is_fmt),
               ~ dplyr::case_when(
                 is_mean(.) ~ set_type(., "mean"),
                 is_pct(.)  ~ set_type(., "pct" ),
                 TRUE       ~ .
               )   ))
    )

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext, chi2 = chi2)
  }
}


var_contrib <- function(x, tot, type = c("ctr", "expected_freq", "spread",
                                                      "binding_ratio",
                                         "ctr_with_sign")) {
  observed_freq     <- get_wn(x) / get_wn(dplyr::last(tot))
  expected_freq     <- get_wn(dplyr::last(x)) * get_wn(tot) / get_wn(dplyr::last(tot))^2
  spread            <- observed_freq - expected_freq
  switch(type[1],
         "ctr"           = spread^2 / expected_freq, # = expected_freq * binding_ratio^2,
         "spread"        = spread                  ,
         "binding_ratio" = spread   / expected_freq,
         "expected_freq" = expected_freq           ,
         "ctr_with_sign" = sign(spread) * spread ^2 / expected_freq
  )
}




tab_chi2 <- function(tabs, calc = c("ctr", "p", "var", "counts")) {
  get_vars        <- tab_get_vars(tabs)
  row_var         <- get_vars$row_var
  #col_vars        <- rlang::sym(get_vars$col_vars)
  col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  groups          <- rlang::syms(dplyr::group_vars(tabs))
  #ngroups         <- dplyr::n_groups(tabs)

  stopifnot(calc %in% c("ctr", "p", "var", "counts"))

  if ("all" %in% calc) calc <- c("ctr", "p", "var", "counts")
  subtext         <- get_subtext(tabs)

  tabs <- tabs %>% tab_match_groups_and_totrows() %>% tab_add_totcol_if_no()
  totvar <- rlang::sym(tail(names(tabs)[is_totcol(tabs)], 1L))


  is_a_mean <- purrr::map_lgl(col_vars_levels,
                              ~ purrr::map_lgl(dplyr::select(dplyr::ungroup(tabs), !!!.),
                                               ~ any(is_mean(.))) %>% any() #1st: any/all?
  )
  all_col_tot <- names(col_vars_levels) == "all_col_vars"



  #Calculate absolute contributions to variance (with spread sign)
  if ("ctr" %in% calc | "var" %in% calc) {
    tabs <- tabs %>%
      dplyr::mutate(dplyr::across(
        where(~ is_fmt(.) & ! all(is_mean(.))),
        ~ set_var(., var_contrib(., !!totvar, type = "ctr_with_sign") )
      ))
    #tabs %>% dplyr::mutate(dplyr::across( where(is_fmt), ~ get_var(.)   ))


    #Calculate variances (per groups and per column variables)
    variances_calc <-
      purrr::map_if(col_vars_levels, !is_a_mean & !all_col_tot,
                    .f    = ~ dplyr::select(tabs, !!!groups, !!!.) %>% #
                      dplyr::select(where(~ !is_totcol(.))) %>%
                      dplyr::mutate(dplyr::across(where(is_fmt), ~ abs(get_var(.)))),
                    .else = ~ NA_real_ #Weighted mean of variances ?
      )

    variances_by_row <-
      purrr::map(variances_calc[!is_a_mean & !all_col_tot],
                 ~ dplyr::mutate(., dplyr::across(where(is.double),
                                                  ~ sum(., na.rm = TRUE))) %>%
                   dplyr::ungroup() %>%
                   dplyr::select(where(is.double)) %>% rowSums(na.rm = TRUE)
      )

    variances_by_group <-
      purrr::map_if(variances_calc[!all_col_tot], !is_a_mean[!all_col_tot],
                    .f    = ~ dplyr::group_split(.[!is_totrow(tabs),], .keep = FALSE) %>%
                      purrr::map_dbl(~ rowSums(., na.rm = TRUE) %>% sum(na.rm = TRUE)),
                    .else = ~ NA_real_ #rep(NA_real_, ngroups) #Weighted mean of variances ?
      )
  }

  #Calculate relative contributions to variance
  if ("ctr" %in% calc) {
    tabs <-
      purrr::reduce2(col_vars_levels[!is_a_mean & !all_col_tot], variances_by_row,
                     .init = tabs, .f = function(.tab, .levels, .variance)
                       tibble::add_column(.tab, .var = .variance) %>%
                       dplyr::mutate(dplyr::across(
                         tidyselect::all_of(purrr::map_chr(.levels, as.character)),
                         ~ set_ctr(., .var))) %>%
                       select(-.var)
      )

    tabs <- tabs %>%
      dplyr::mutate(dplyr::across( where(is_fmt),
                                   ~ set_ctr(., get_var(.) / get_ctr(.))
      ))
    #tabs %>% dplyr::mutate(dplyr::across( where(is_fmt), ~ set_type(., "ctr")  ))


    # #Relative contributions of col_vars levels (on total rows)
    # tabs <- tabs %>%
    #   dplyr::mutate(dplyr::across(
    #     where(is_fmt),
    #     ~ dplyr::if_else(condition = dplyr::row_number() == dplyr::n(),
    #                      true      = set_ctr(., sum(abs(get_ctr(.)))),
    #                      false     = . )
    #   ))
    # #tabs %>%  dplyr::mutate(dplyr::across( where(is_fmt), ~ set_type(., "ctr")  ))


    #mean_contrib <- contrib_no_sign %>% map(~ 1 / ( ncol(.) * nrow(.) ) )
  }


  #Calculate unweighted counts
  if ("counts" %in% calc) {
    counts <-
      purrr::map(col_vars_levels[!all_col_tot],
                    ~ dplyr::select(tabs[!is_totrow(tabs),], !!!groups, !!!.) %>%
                      dplyr::select(where(~ !is_totcol(.))) %>%
                      dplyr::group_split(.keep = FALSE) %>%
                      purrr::map_int(~ dplyr::mutate(., dplyr::across(where(is_fmt),
                                                                      ~ get_n(.))) %>%
                                       rowSums() %>% sum() %>% as.integer()
                      )
      )
  }


  #Calculate pvalue : variance was calculated with weights, and here we want unwtd counts
  if ("p" %in% calc) {
    #zero_n_rows <-  # remove zero n rows from chisq test ?
    no_chisq_cols <- purrr::map_if(tabs[!is_totrow(tabs),], is_fmt,
                                   ~ is_totcol(.) | any(is_mean(.)) |
                                     (sum(get_n(.), na.rm = TRUE) == 0),
                                   .else = ~ FALSE) %>% purrr::flatten_lgl()
    no_chisq_cols <- names(tabs)[no_chisq_cols]

    quiet_chisq_test <- function(tab) {
      quiet_chisq <-
        purrr::possibly(purrr::quietly(~ stats::chisq.test(.)),
                        tibble::tibble(warnings = "", result = tibble::tibble(p.value = NA_real_)) )
      result <- quiet_chisq(tab)

      pvalue_warning <- if (length(result$warnings) != 0) {result$warnings} else {""}
      pvalue         <- result$result$p.value
      df             <- result$result$parameter

      tibble::tibble(pvalue = pvalue, warnings = pvalue_warning, df = df)
    }



    pvalues <-
      purrr::map_if(col_vars_levels[!all_col_tot], !is_a_mean[!all_col_tot],
                    ~ dplyr::select(tabs[!is_totrow(tabs),], !!!groups, !!!.) %>%
                      dplyr::group_split(.keep = FALSE) %>%
                      purrr::map_df(~ dplyr::select(., -tidyselect::any_of(no_chisq_cols) ) %>%
                                      dplyr::mutate(dplyr::across(where(is_fmt), ~ get_n(.))) %>%
                                      quiet_chisq_test())
                    ,

                    .else = ~ tibble::tibble(pvalue = NA_real_, warnings = "",
                                             df = NA_integer_)
      )
    pvalue_p     <- purrr::map(pvalues, ~ dplyr::pull(., pvalue))
    pvalue_w     <- purrr::map(pvalues, ~ dplyr::pull(., warnings))
    pvalue_df    <- purrr::map(pvalues, ~ dplyr::pull(., df) %>% as.integer())
  }


  #Assemble everything and put it into the metadata of the tab
  tables <-  tabs[!is_totrow(tabs),] %>% dplyr::select(!where(is_totcol)) %>%
    dplyr::summarise(.groups = "drop", tables = NA_character_) %>%
    dplyr::mutate(dplyr::across(.fns = ~ stringr::str_remove_all(., "/") ))

    if (length(groups) != 0) tables <- tables %>%
    dplyr::transmute(tables = stringr::str_c(!!!groups, sep = "/"))

  tables <- tables %>%
    dplyr::mutate(tables = dplyr::if_else(
      condition = stringr::str_extract(.data$tables, "^.*(?=/)") ==
        stringr::str_extract(.data$tables, "(?<=/).*$"),
      true      = stringr::str_extract(.data$tables, "^.*(?=/)"),
      false     = .data$tables
    ))

  if (!"p"      %in% calc) {
    pvalue_p  <- NA_real_
    pvalue_w  <- NA_character_
    pvalue_df <- NA_integer_
  }
  if (!"var"    %in% calc) variances_by_group <- NA_real_
  if (!"counts" %in% calc) counts             <- NA_integer_

  chi2 <-
    purrr::pmap(list(tables, pvalue_p, pvalue_w, pvalue_df, variances_by_group, counts),
                ~   tibble::tibble(tables   = ..1,
                                   pvalue   = ..2,
                                   warnings = ..3,
                                   df       = ..4,
                                   variance = ..5,
                                   count    = ..6 )) %>%
    purrr::set_names(stringr::str_c(row_var, " par ",
                                    names(col_vars_levels)[!all_col_tot])) %>%
    #purrr::map(~ dplyr::mutate(., var_per_df = var / df))
    purrr::imap(~ dplyr::mutate(.x, tables = dplyr::if_else(
      condition = stringr::str_detect(.data$warnings, "incorrect"),
      true      = stringr::str_c(.data$tables, " (!)"), #"  ",
      false     = stringr::str_c(.data$tables)          #"  ",
    )) %>% select(-.data$warnings) #%>%
    #dplyr::add_row(tables = stringr::str_c(row_var, " par ", .y, " :"),
    #               .before = 1)
    ) %>%
    #dplyr::bind_rows()
    purrr::imap(~ dplyr::mutate(.x, tables = dplyr::if_else(
      is.na(.data$tables), true = .y, false = .data$tables
    )))

  #nb_cells = df + r + c + 1


  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext, chi2 = chi2)
  }
}



# tablr composition test function
# replace tab_get_vars
tab_get_vars <- function(tabs, vars = c("row_var", "col_vars", "tab_vars")) {
  stopifnot(is.data.frame(tabs))

  if ("col_vars" %in% vars) {
    fmt_cols <- purrr::map_lgl(tabs, is_fmt)
    col_vars       <- get_col_var(tabs[fmt_cols])
    col_vars_names <- col_vars %>% unique()

    col_vars_levels <-
      purrr::map(col_vars_names, ~ names(col_vars[col_vars == .])) %>%
      purrr::set_names(col_vars_names)

    col_vars <- col_vars_names
  }

  fct_cols <- purrr::map_lgl(tabs, is.factor)

  if ("row_var" %in% vars) row_var <- names(tail(fct_cols[fct_cols], 1L))

  if ("tab_vars" %in% vars) tab_vars <-
    names(fct_cols[fct_cols & names(fct_cols) != row_var])



  ls(pattern = "^row_var$|^col_vars$|^col_vars_levels$|^tab_vars$") %>%
    purrr::set_names(.) %>%
    purrr::map(~ rlang::sym(.) %>% rlang::eval_tidy())
}




#Ajouter : - Rename levels "Total" and "Ensemble"
#Bug :     - If drop_sup_na = TRUE, NA for numeric variables are kept
# Prepare data for \code{\link{tab_sup}}.
# @param data A dataframe.
# @param ... \code{\link[dplyr]{dplyr_tidy_select}} One or more unquoted
# expressions separated by commas or strings separated by commas.
# @param drop_sup_na When \code{FALSE}, \code{NA} values are
# replaced by explicit \code{"NA"} values.
# @param cleannames Set to \code{TRUE} to clean levels names, by removing
# prefix numbers like \code{"1-"}, and text in parentheses.
# @param rare_to_other When set to \code{TRUE}, levels with less count
# than minimum_headcount will be merged into an "Other" level.
# @param minimum_headcount The minimum unweighted count in each variable.
#
# @export
# @examples
#' @keywords internal
tab_sup_prepare <-
  function(data, ..., drop_sup_na = FALSE,
           cleannames = TRUE, rare_to_other = FALSE,
           minimum_headcount = 30) {
    variables <- rlang::ensyms(...)

    vars_not_numeric <-
      dplyr::select(data, !!!variables) %>%
      dplyr::select_if(~ ! is.numeric(.)) %>% colnames() %>% rlang::syms()

    data <- data %>%
      dplyr::mutate_at(dplyr::vars(!!!vars_not_numeric), as.factor)

    data <- data %>%
      dplyr::mutate(dplyr::across(
        where(is.ordered), ~ magrittr::set_class(., class(.) %>% .[. != "ordered"])
      ))

    if (drop_sup_na == FALSE) data <- data %>%
      dplyr::mutate_at(dplyr::vars(!!!vars_not_numeric),
                       forcats::fct_explicit_na,
                       na_level = "NA")


    if(rare_to_other == TRUE) {
      data %<>%
        dplyr::mutate_at(dplyr::vars(!!!vars_not_numeric),
                         ~ forcats::fct_lump_min(.,
                                                 minimum_headcount,
                                                 other_level = "Autres")
        )
    }
    data <- data %>%  #Remove unused levels anyway
      dplyr::mutate_at(dplyr::vars(!!!vars_not_numeric), forcats::fct_drop)


    if (cleannames == TRUE)  data <- data %>%
      dplyr::mutate_at(as.character(vars_not_numeric), fct_clean)


    #If sex is in supplementary var, see % of women and not men
    if ("SEXE" %in% names(data)){
      if (!stringr::str_detect(levels(data$SEXE)[1], "f|F")) data %<>%
        dplyr::mutate(SEXE = forcats::fct_rev(SEXE))
    }

    # data %<>% dplyr::select(!!tab_vars, !!row_var, !!col_var, !!wt, tidyselect::all_of(c(sup_cols, sup_rows))) %>%
    #   dplyr::select(where(is.factor), where(is.numeric)) %>%
    #   dplyr::select(!!tab_vars, !!row_var, !!col_var, !!wt, tidyselect::everything())

    data %>%
      dplyr::select(!!!variables, dplyr::everything()) %>%
      dplyr::select(where(is.factor), where(is.numeric), dplyr::everything())
  }



#' @keywords internal
get_orig_name <- function(df) { #Thanks to https://stackoverflow.com/questions/30057278
  i <- 1
  while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  deparse(parent.frame(i)$lhs)  #list(name = deparse(parent.frame(i)$lhs), output = df)
}


# data <- ct2013s %>%
#   dplyr::mutate(no_col_var = "n") %>%
#   dplyr::select(tidyselect::all_of(list_of_vars), EMP2,  no_col_var, PR0, pondcal)
# row_var <- rlang::expr(EMP2)
# col_var <- rlang::expr(no_col_var)
# tab_vars <- rlang::expr(PR0)
# wt <- rlang::expr(pondcal)
#
# perc <- "row"
# tot = c("row", "col")
# digits <- 1
# cleannames <- TRUE
# rare_to_other = FALSE
# minimum_headcount <- 30
# another_total = FALSE
# sup_contrib <- FALSE
# result <- "observed"
# show_na <- FALSE
# no_weight = FALSE
# confidence_intervals = FALSE
# conf_level = 0.95
# totaltab <- "line" # "table"
# keep_unused_levels = FALSE
# drop_sup_na = FALSE
# only_first_level = TRUE
# print_sup = FALSE
# force_unique_table = FALSE
# subtext = ""
#
# no_row_var <- FALSE
# no_col_var <- TRUE
# no_tab_vars <- FALSE
# datbase <- data
#
# sup_cols = list_of_vars
# sup_rows =  NULL
# sort_by = "no"
# datbase <- data
# multicols <- TRUE
# multirows <- FALSE


#Classer les variables supplementaires en texte / numeriques (before that data must be reorder with numeric vars at the end)
#' @keywords internal
tab_make_sup_list <- function(data, sup_cols = NULL, sup_rows = NULL) {
  sup_cols <- names(data) %>% purrr::keep(. %in% sup_cols)
  sup_cols_num  <- sup_cols %>% purrr::map_lgl(~ dplyr::pull(data, !!rlang::sym(.)) %>% is.numeric())
  sup_cols_text <- sup_cols %>% purrr::map_lgl(~ dplyr::pull(data, !!rlang::sym(.)) %>% is.factor() |
                                                 dplyr::pull(data, !!rlang::sym(.)) %>% is.character() )

  sup_rows <- names(data) %>% purrr::keep(. %in% sup_rows)
  sup_rows_num  <- sup_rows %>% purrr::map_lgl(~ dplyr::pull(data, !!rlang::sym(.)) %>% is.numeric())
  sup_rows_text <- sup_rows %>% purrr::map_lgl(~ dplyr::pull(data, !!rlang::sym(.)) %>% is.factor() |
                                                 dplyr::pull(data, !!rlang::sym(.)) %>% is.character() )

  list(sup_cols, sup_cols_num, sup_cols_text, sup_rows, sup_rows_num, sup_rows_text) %>%
    magrittr::set_names(c("sup_cols", "sup_cols_num", "sup_cols_text", "sup_rows", "sup_rows_num", "sup_rows_text"))
}


#To be able to pass many variables in one using c() or list() with quosures
# or use c() or list() with ... in tribbles
#Thanks to Artem Sokolov
# https://stackoverflow.com/questions/52590670/how-to-iterate-inside-the-elements-of-a-quosure-of-rlang-in-r
# get_ast <- function(x) { as.list(x) %>% purrr::map_if(is.call, get_ast) }
ensym_c <- function(quo) {
  rlang::get_expr(quo) %>% as.list() %>%
    as.character() %>% tidyr::replace_na("no") %>%
    purrr::discard(. %in% c("c", "list")) %>%
    rlang::syms()
}
# Need to use rlang:quo or rlang:enquo before to works (not to call directly on args)

ensyms_c <- function(quos) {
  quos <- purrr::map(quos, ensym_c) %>% purrr::flatten()
  NA_quos <- purrr::map_lgl(quos, ~ as.character(.) %in% c("NA", "NULL", "no"))
  quos <- quos[!NA_quos]
}


# Need to use rlang:quos or rlang:enquos before to works (not to call directly on args)

quo_test <- function(vars1, vars2, ...) {
  rlang::enquo(vars1) %>% ensym_c() %>% print()
  #rlang::enquo(vars2) %>% ensym_c() %>% print()
  rlang::enquos(...) %>% ensyms_c() %>% print()

  #length(rlang::enquos(...)) %>% print()
  #rlang::ensyms(...) %>% print()


  # tab_vars <- rlang::enquo(...) %>% ensym_c()
  # NA_tab_vars <- purrr::map_lgl(tab_vars, ~ as.character(.) %in% c("NA", "NULL", "no"))
  # if (all(NA_tab_vars) ) {
  #   tab_vars <- rlang::expr(no_tab_vars)
  # } else if (any(NA_tab_vars)) {
  #   tab_vars <- tab_vars[!NA_tab_vars]
  # }
  # print(NA_tab_vars)
  # print(tab_vars)

  }
# quo_test(c(PR1, PR2), list("PE1", "PE2"), PE3, PE4)
# quo_test(PR1, PR2, PR3)

# quo_test(!!tab_vars)
#
# quo_test(PR1, PR2, c("PR0", "EMP_ADM_ENT"))
# quo_test(PR1, PR2, c("PR0", "EMP_ADM_ENT"), PR3)

# quo_test(... = c(NA_character_, PR1))
#
# rlang::expr(NA_character_) %>% as.list() %>%
#   as.character() %>% tidyr::replace_na("no") %>% purrr::discard(. %in% c("c", "list")) %>%
#   rlang::syms() %>% as.character()
