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
    data
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
    row_var     <- rlang::enquo(row_var)
    NA_row_var <- is.na(as.character(rlang::get_expr(row_var)))
    if (all(NA_row_var)) {
      data <- data %>% dplyr::mutate(no_row_var = factor("n"))
      row_var <- rlang::expr(no_row_var)
    } else {
      pos_row_var <- tidyselect::eval_select(row_var, data)
      row_var     <- rlang::sym(names(pos_row_var))
    }
  }

  if (missing(col_var)) {
    data <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_var <- rlang::expr(no_col_var)
  } else {
    col_var     <- rlang::enquo(col_var)
    NA_col_var <- is.na(as.character(rlang::get_expr(col_var)))
    if (all(NA_col_var)) {
      data <- data %>% dplyr::mutate(no_col_var = factor("n"))
      col_var <- rlang::expr(no_col_var)
    } else {
      pos_col_var <- tidyselect::eval_select(col_var, data)
      col_var     <- rlang::sym(names(pos_col_var))
    }
  }

  if (missing(...)) {
    data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
    tab_vars <- rlang::exprs(no_tab_vars)
  } else {
    tab_vars_quo <- rlang::enquos(...)
    NA_tab_vars  <- purrr::map(tab_vars_quo,
                               ~ is.na(as.character(rlang::get_expr(.)))) %>%
      purrr::flatten_lgl()
    if (all(NA_tab_vars) ) {
      data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
      tab_vars <- rlang::exprs(no_tab_vars)
    } else {
      tab_vars <- rlang::expr(c(...))
      pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
      tab_vars     <- rlang::syms(names(pos_tab_vars))
    }
  }

  if (missing(wt)) {
    data <- data %>% dplyr::mutate(no_weight = 1)
    wt <- rlang::expr(no_weight)
  } else {
    wt    <- rlang::enquo(wt)
    NA_wt <- is.na(as.character(rlang::get_expr(wt)))
    if (all(NA_wt)) {
      data <- data %>% dplyr::mutate(no_weight = 1)
      wt   <- rlang::expr(no_weight)
    } else {
      pos_wt <- tidyselect::eval_select(wt, data)
      wt     <- rlang::sym(names(pos_wt))
    }
  }

  #forbid the level to have the name of the variable, othewise problems ----

  #dat_prepare : all filters here (operations on rows copy all columns on memory)
  #remove all unwanted NAs : those we want to keep were turned to explicit before
  data <- data %>% dplyr::select(!!row_var, !!col_var, !!!tab_vars, !!wt) %>%
    dplyr::with_groups(NULL,
                       ~ dplyr::filter(., dplyr::across(c(!!row_var, !!col_var, !!!tab_vars),
                                                        ~ ! is.na(.)))) %>%
    dplyr::mutate(!!wt := as.numeric(!!wt))

  row_var_type <- ifelse ("numeric" %in% class(dplyr::pull(data, !!row_var) ),
                          "numeric", "factor")
  col_var_type <- ifelse ("numeric" %in% class(dplyr::pull(data, !!col_var) ),
                          "numeric", "factor")
  if (row_var_type == "numeric" & col_var_type == "numeric") {
    stop("row_var and col_var are both numeric : only one of them can be")
  }
  type <- ifelse(row_var_type == "numeric" | col_var_type == "numeric",
                 "numeric", "factor")

  if (type == "numeric") {
    num_var <- switch(row_var_type, "numeric" = row_var, "factor" = col_var)
    fct_var <- switch(row_var_type, "numeric" = col_var, "factor" = row_var)
  }

  if (! is_grouped) {
    data <- switch(type,
                   "factor"   = dplyr::group_by(data, !!!tab_vars, !!row_var,
                                                !!col_var),
                   "numeric"  = dplyr::group_by(data, !!!tab_vars, !!fct_var     ) )
  }

  if (type == "numeric") {
    if (rlang::as_name(num_var) %in% dplyr::group_vars(data)) {
      data <- dplyr::ungroup(data, !!num_var)
    }
  }

  # nlvs <- nlevels(dplyr::pull(data, !!col_var))

  tabs <-
    switch(type,
           "factor"  = data %>%
             dplyr::summarise(nums = new_fmt(display = "wn"                   ,
                                             digits  = as.integer(digits)     ,
                                             n       = dplyr::n()             ,
                                             wn      = sum(!!wt)              ,
                                             type    = "n"                    ,
                                             col_var = rlang::as_name(col_var)
             ),
             .groups = 'drop') %>%
             tidyr::pivot_wider(names_from = !!col_var, values_from = nums,
                                values_fill = fmt0("wn", digits, type = "n")),

           "numeric" = data %>%
             dplyr::summarise(!!num_var := new_fmt(
               display = "mean"                                      ,
               digits  = as.integer(digits)                          ,
               n       = dplyr::n()                                  ,
               wn      = sum(!!wt)                                   ,
               mean    = weighted.mean(!!num_var, !!wt, na.rm = TRUE),
               var     = weighted.var (!!num_var, !!wt, na.rm = TRUE),
               type    = "mean"                                      ,
               col_var = rlang::as_name(col_var)
             ),
             .groups = "drop")
    )

  if (row_var_type == "numeric") tabs <- tabs %>%
    tidyr::pivot_wider(names_from = !!fct_var, values_from = !!num_var,
                       values_fill = fmt0("mean", digits, type = "mean"))

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
tab_many <- function(data, row_var, col_vars, tab_vars, wt, levels = "all",
                     digits = 0, drop_sup_na = FALSE, cleannames = FALSE,
                     listed = FALSE,
                     totaltab = "no", totaltab_name = "Ensemble",
                     totrow = FALSE, totcol = "no", totals_name = "Total",
                     pct = "no", chi2 = FALSE,
                     ci = "no", conf_level = 0.95, ci_visible = FALSE,
                     comp = c("tab", "all"),
                     color = "no",
                     subtext = "") {
  stopifnot(levels %in% c("first", "all"),
            is.numeric(digits),
            is.logical(drop_sup_na), is.logical(cleannames), is.logical(listed)
  )
  lvs <- levels
  #levels = "first" don’t work anymore ----

  if (missing(row_var)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("n"))
    row_var <- rlang::expr(no_row_var)
  } else {
    row_var     <- rlang::enquo(row_var)
    pos_row_var <- tidyselect::eval_select(row_var, data)
    row_var     <- rlang::sym(names(pos_row_var))
    if (as.character(row_var) %in% c("NA", "NULL", "no")) {
      data <- data %>% dplyr::mutate(no_row_var = factor("n"))
      var1 <- rlang::expr(no_row_var)
    }
  }

  if (missing(col_vars)) {
    data     <- data %>% dplyr::mutate(no_col_vars = factor(" "))
    col_vars <- rlang::exprs(no_col_vars)
    pos_col_vars <- tidyselect::eval_select("no_col_vars", data)
  } else {
    col_vars     <- rlang::enquo(col_vars)
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
    NA_col_vars  <- purrr::map_lgl(col_vars,
                                   ~ as.character(.) %in% c("NA", "NULL", "no"))
    if (all(NA_col_vars) ) {
      data     <- data %>% dplyr::mutate(no_col_vars = factor(" "))
      col_vars <- rlang::exprs(no_col_vars)
    }
  }
  col_vars_num  <- purrr::map_lgl(data[pos_col_vars], is.numeric)
  col_vars_text <- purrr::map_lgl(data[pos_col_vars],
                                  ~ is.factor(.) | is.character(.))
  if (missing(tab_vars)) {
    data     <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
    tab_vars <- rlang::exprs(no_tab_vars)
  } else {
    tab_vars     <- rlang::enquo(tab_vars)
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
    NA_tab_vars  <- purrr::map_lgl(tab_vars,
                                   ~ as.character(.) %in% c("NA", "NULL", "no"))
    if (all(NA_tab_vars) ) {
      data     <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
      tab_vars <- rlang::exprs(no_tab_vars)
    }
  }

  if (missing(wt)) {
    data <- data %>% dplyr::mutate(no_weight = 1)
    wt <- rlang::expr(no_weight)
  } else {
    wt     <- rlang::enquo(wt)
    pos_wt <- tidyselect::eval_select(wt, data)
    wt     <- rlang::sym(names(pos_wt))
    if (as.character(wt) %in% c("NA", "NULL", "no")) {
      data <- data %>% dplyr::mutate(no_weight = 1)
      wt   <- rlang::expr(no_weight)
    }
  }
  # print(tab_vars) ; print(row_var) ; print(wt) ; print(col_vars)

  #na : one_by_one of for_all !----

  # Vectorise arguments when possible
  nvars        <- length(col_vars)
  lvs          <- vec_recycle(lvs        , nvars)
  digits       <- vec_recycle(digits     , nvars)
  totals_name  <- vec_recycle(totals_name, 2    )
  chi2         <- vec_recycle(chi2       , 1    )
  pct          <- vec_recycle(pct        , nvars)
  ci           <- vec_recycle(ci         , nvars)
  ci_visible   <- vec_recycle(ci_visible , nvars)

  #drop_sup_na <- vec_recycle(drop_sup_na, nvars, x_arg = "drop_sup_na")
  #cleannames  <- vec_recycle(cleannames , nvars, x_arg = "cleannames" )
  if (listed == FALSE) {
    totaltab   <- vec_recycle(totaltab  , 1     )
    conf_level <- vec_recycle(conf_level, 1     )
    color      <- vec_recycle(color     , 1     )
  } else {
    totaltab   <- vec_recycle(totaltab  , nvars )
    conf_level <- vec_recycle(conf_level, nvars )
    color      <- vec_recycle(color     , nvars )
  }

  if (comp[1] == "all" & totaltab == "no") { # just if tab_vars ?
    warning("comp = 'all' need total table with total row to compare with")
    totaltab <- "line"
  }

  # Manage total rows and cols arguments
  if (totcol %in% c("last", "all_col_vars")) {
    totcol <- col_vars[nvars]
  } else if (totcol == "each") {
    totcol <- col_vars[col_vars_text]
  } else if (all(totcol %in% col_vars)) {
    totcol <- col_vars[col_vars %in% totcol]
  } else if (all(totcol %in% c("col", "no"))) {
    totcol <- col_vars[which(totcol == "col")]
  } else if (is.numeric(totcol)) {
    if (any(totcol > nvars)) stop("some totcol indexes are superior to the",
                                  " number of col_vars")
    totcol <- col_vars[unique(as.integer(totcol))]
  } else {
    stop("totcol must be 'last', 'each', a vector of col_vars names, ",
         "a vector of 'col'/'no', or a vector of col_vars indexes")
  }



  tot_cols_type <- dplyr::case_when(
    identical(totcol, col_vars)                                ~ "each",
    identical(totcol, col_vars[nvars])                         ~ "all_col_vars",
    length(totcol) == 0 &
      (any(chi2 != FALSE) | any(pct != "no") | any(ci != "no"))~ "no_delete",
    length(totcol) == 0                                        ~ "no_no_create",
    TRUE                                                       ~ "some"
  )


  #Prepare the data with explicits NA, etc.
  data <- data %>%
    tab_sup_prepare(!!!col_vars,
                    drop_sup_na = drop_sup_na, cleannames = cleannames)

  #   Remove all the NAs of explanatory variables
  #if drop_sup_na == FALSE, all factors (not numeric) where made explicit with dat_prepare :
  #print(dplyr::filter_all(data, ~ is.na(.)))
  #data %>% purrr::map(~ is.na(.) %>% which() %>% length()) %>% print()

  data <- data %>% dplyr::select(!!!tab_vars, !!row_var, !!wt, !!!col_vars)
  data <- data %>% dplyr::filter_if(is.factor, ~ !is.na(.))

  #   Where only first levels are kept, merge others to minimise useless calculations
  lv1 <- lvs == "first" & col_vars_text
  if (any(lv1)) {
    col_vars_3levels <-
      purrr::map_lgl(dplyr::select(data, !!!col_vars),
                     ~ is.factor(.) & nlevels(.) >= 3) & lv1

    remove_levels <- purrr::map(dplyr::select(data, !!!col_vars[lv1]),
                                ~ levels(.) %>% .[-1])

    if (any(col_vars_3levels)) {
      data <- data %>%
        dplyr::mutate(dplyr::across(
          !!!col_vars[col_vars_3levels],
          ~ forcats::fct_other(., keep = levels(.)[1], other_level = "Autres")
        ))
    }
  }


  #Make the tables and store them in a list
  #dat_group3 <- data %>% dplyr::group_by(!!!tab_vars, .add = TRUE, .drop = FALSE)
  tabs <-
    purrr::pmap(list(col_vars, digits),
                function(.col_vars, .digits)
                  tab_core(data, !!row_var, !!.col_vars, !!!tab_vars, wt = !!wt,
                           digits = .digits)) %>%
    purrr::set_names(col_vars)

  #Add total table, total rows and cols, chi2 stats, pct, confidence intervals
  #  if the result if a list or tables
  if (listed == TRUE) {
    ctt  <- totaltab != "no"
    cchi <- chi2     != "no" & col_vars_text
    cpct <- pct      != "no" & col_vars_text
    cci  <- ci       != "no" & col_vars_text

    color_diff <- color == "diff" | (color == "auto" & ci != "diff")
    color_ctr  <- dplyr::case_when(
      color %in% c("no", "diff", "diff_ci", "after_ci") ~ "no"  ,
      color == "auto"                                   ~ "auto",
      color == "contrib"                                ~ "all" ,
    )
    color_ci <- dplyr::case_when(
      color %in% c("diff_ci", "after_ci")               ~ color     ,
      color == "auto" & ci == "diff"                    ~ "after_ci",
      TRUE                                              ~ "no"      ,
    )

    tabs[ctt] <- tibble::tibble(tabs = tabs[ctt], totaltab = totaltab[ctt],
                                name = totaltab_name) %>%
      purrr::pmap(tab_totaltab)

    if (tot_cols_type != "no_no_create" | totrow == TRUE) {
      tabs <- tibble::tibble(tabs, tot = "both", name = totals_name) %>%
        purrr::pmap(tab_tot, totcol = "each")
    }

    tabs[cchi] <- tibble::tibble(tabs = tabs[cchi], comp = comp[1],
                                 color = color_ctr) %>%
      purrr::pmap(tab_chi2)

    tabs[cpct] <- tibble::tibble(tabs, pct, comp = comp[1],
                                 color = color_diff) %>%
      dplyr::filter(cpct) %>%
      purrr::pmap(tab_pct)

    tabs[cci] <- tibble::tibble(tabs, ci, comp = comp[1],conf_level = conf_level,
                                color = color_ci, visible = ci_visible) %>%
      dplyr::filter(cci) %>%
      purrr::pmap(tab_ci)


    #Remove unwanted levels (keep only the first when levels = "first")
    if (any(lv1)) tabs[lv1] <- tabs[lv1] %>%
      purrr::map2(remove_levels,
                  ~ dplyr::select(.x, -tidyselect::all_of(.y),
                                  -tidyselect::any_of("Total")))
    #  remove unwanted totals for listed result ----
    tabs <- purrr::map_if(tabs, map_lgl(tabs, dplyr::is_grouped_df),
                          .f    = ~ new_tab(., subtext = subtext),
                          .else = ~ new_grouped_tab(., dplyr::group_data(.),
                                                    subtext = subtext))
    return(tabs)
  }

  #If the result is unique table, join the list of tabs into a single table,
  # managing duplicated levels
  duplicated_levels <- tabs %>%
    purrr::map(~ names(.) %>% purrr::discard(. %in% c(row_var, tab_vars))) %>%
    purrr::flatten_chr() %>% .[duplicated(.)] %>% unique()

  if (length(duplicated_levels) != 0) {
    # warning(paste0("some levels names are the same for different variables : ",
    #                paste0(duplicated_levels, collapse = ", ")))
    tabs <- tabs %>%
      purrr::imap(~ dplyr::rename_with(.x, function(.names)
        dplyr::if_else(.names %in% duplicated_levels,
                       paste0(.names, "_", .y), .names)))
  }

  join_by <- as.character(c(tab_vars, row_var)) %>%
    purrr::discard(. == "no_tab_vars")
  tabs <- purrr::reduce(tabs, dplyr::full_join, by = join_by)


  color_diff <- color == "diff" | (color == "auto" & any(ci != "diff"))
  color_ctr  <- switch(color,
                       "no"       = "no"  ,
                       "auto"     = "auto",
                       "diff"     = "no"  ,
                       "diff_ci"  = "no"  ,
                       "after_ci" = "no"  ,
                       "contrib"  = "all"  )
  color_ci   <- switch(
    color,
    "no"       = "no"      ,
    "auto"     = dplyr::if_else(any(ci == "diff"), "after_ci", "no"),
    "diff"     = "no"      ,
    "diff_ci"  = "diff_ci" ,
    "after_ci" = "after_ci",
    "contrib"  = "no"
  )

  if (totaltab != "no") tabs <- tab_totaltab(tabs, totaltab,
                                             name = totaltab_name)

  if (tot_cols_type != "no_no_create" | totrow == TRUE) {
    tabs <- tab_tot(tabs, tot = "both", totcol = "each", name = totals_name)
  }

  if (chi2 == TRUE) tabs <- tab_chi2(tabs, comp = comp[1], color = color_ctr)

  if (any(pct != "no")) tabs <- tab_pct(tabs, pct, comp = comp[1],
                                        color = color_diff)

  if (any(ci != "no")) {
    tabs <- tab_ci(tabs, ci = ci, comp = comp[1], conf_level = conf_level,
                   visible = ci_visible, color = color_ci)
  }

  #Remove unwanted levels (keep only the first when levels = "first")
  if (any(lv1)) {
    remove_levels <-
      purrr::imap(remove_levels, ~ c(.x, paste0(.x, "_", .y))) %>%
      purrr::flatten_chr()
  }

  #Remove unwanted totals
  if (!tot_cols_type %in% c("each", "no_no_create")) {
    if (tot_cols_type == "no_delete") tabs <- tabs %>%
        dplyr::select(-where(is_totcol))
    if (tot_cols_type == "some") tabs <- tabs %>%
        dplyr::select(-(where(~ is_totcol(.) & ! get_col_var(.) %in% totcol) )
        )

    if (tot_cols_type == "all_col_vars") {
      no_last_tot <- is_totcol(tabs) %>% .[.] %>% names()
      last_tot <- dplyr::last(no_last_tot)
      no_last_tot <- no_last_tot %>% purrr::discard(. == last_tot)
      tabs <- tabs %>% dplyr::select(-tidyselect::all_of(no_last_tot)) %>%
        dplyr::relocate(where(is_totcol), .after = tidyselect::last_col()) %>%
        dplyr::rename_with(~ totals_name[2], .cols = last_tot) %>%
        dplyr::mutate(dplyr::across(tidyselect::last_col(),
                                    ~ set_col_var(., "all_col_vars")))
    }
  }
  if (totrow == FALSE) {
    tabs <- tabs %>%
      tibble::add_column(
        totrows = is_totrow(tabs),
        tottab_rows = is_tottab(tabs),
        tottab_line = length(tottab_rows[tottab_rows]) == 1 & totrows) %>%
      dplyr::filter(!totrows | tottab_line) %>%
      dplyr::select(-totrows, -tottab_rows, -tottab_line)
  }


  if (! lv1_group_vars(tabs)) {
    tabs <- tabs %>% dplyr::group_by(!!!tab_vars)
    groups <- dplyr::group_data(tabs)
    tabs <- new_grouped_tab(tabs, groups = groups, subtext = subtext)
  } else {
    tabs <- new_tab(tabs, subtext = subtext)
  }

  tabs
}




# Do not depend on groups
tab_totaltab <- function(tabs, totaltab = c("table", "line", "no"),
                         name = "Ensemble", data = NULL) {
  get_vars  <- tab_get_vars(tabs)
  row_var   <- rlang::sym(get_vars$row_var)
  tab_vars  <- rlang::syms(get_vars$tab_vars)
  mean_vars <- (get_type(tabs) == "mean") %>% purrr::keep(., .) %>% names()


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


  # If there are mean columns, the calculation of variances, necessary to
  #  calculate confidence intervals, needs access to the original database
  if (length(mean_vars) != 0 & !is.null(data)) {

    mean_calc <- switch(
      totaltab[1],
      "table" = purrr::map(mean_vars, ~ tab_core(data, !!row_var,
                                                 col_var = !!rlang::sym(.))),
      "line" = purrr::map(mean_vars, ~tab_core(data, col_var = !!rlang::sym(.)))
    )

    mean_calc <-
      purrr::reduce(mean_calc,
                    ~ dplyr::full_join(.x, .y, by = as.character(row_var)) ) %>%
      dplyr::select(-tidyselect::any_of("no_row_var")) %>%
      dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(.)))

    if (totaltab[1] == "line") mean_calc <- mean_calc %>%
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
                    totcol = c("last", "each"), data = NULL) {
  stopifnot( tot %in% c("no", "row", "col", "both", "row_col", "col_row")  )

  get_vars        <- tab_get_vars(tabs)
  row_var         <- rlang::sym(get_vars$row_var)
  #col_vars        <- rlang::sym(get_vars$col_vars)
  col_vars_levels_mean <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  mean_vars <- get_type(tabs) == "mean"
  col_vars_levels <- col_vars_levels_mean %>%
    purrr::discard(names(.) %in% names(mean_vars))
  tab_vars        <- rlang::syms(get_vars$tab_vars)

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

    tabs <- tabs %>% tibble::add_column(tottab_rows, tottab_line)

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
                       .groups = "drop") %>%
      dplyr::bind_cols(group_vars_totals) %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(!!row_var, row_var_levels))

    #For mean vars, calculate variances based on original datas
    # (necessary to calculate confidence intervals)
    if (any(mean_vars) & !is.null(data)) {
      mean_names <- names(mean_vars[mean_vars])

      mean_calc <-
        purrr::map(mean_names, ~ tab_core(data, row_var = NA_character_,
                                          col_var = tidyselect::all_of(.),
                                          purrr::map_chr(tab_vars, as.character))
        )

      mean_calc <-
        purrr::reduce(mean_calc,~ dplyr::full_join(
          .x, .y,
          by = purrr::map_chr(tab_vars, as.character)
        ) ) %>%
        dplyr::select(-tidyselect::any_of("no_row_var")) %>%
        dplyr::mutate(dplyr::across(where(is_fmt), ~ as_totrow(.)))

      general_totrow_condition <- any(tabs$tottab_rows) & !any(tabs$tottab_line)

      if (general_totrow_condition) {
        general_totrow <-
          purrr::map(mean_names,
                     ~ tab_core(data, row_var = NA_character_,
                                col_var = tidyselect::all_of(.))
          )

        general_totrow <-
          purrr::reduce(general_totrow,
                        ~ dplyr::full_join(.x, .y ,by = character() ) ) %>%
          dplyr::select(-tidyselect::any_of("no_row_var")) %>%
          dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(as_totrow(.))))

        general_totrow  <- dplyr::group_keys(tabs) %>%
          dplyr::slice(dplyr::n_groups(tabs)) %>%
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


    tabs <- dplyr::bind_rows(tabs, totrows) %>%
      dplyr::arrange(.by_group = TRUE) %>%
      dplyr::select(-tottab_line, -tottab_rows)
  }


  #Total columns
  if ("col" %in% tot) {
    col_vars_2levels_or_more <-
      col_vars_levels[purrr::map_int(col_vars_levels, length) >= 2]

    if (length(col_vars_2levels_or_more) != 0 | totcol[1] == "last") {
      tabs <- tabs %>% dplyr::rowwise()

      if (totcol[1] == "last") {
        # c_across don’t work. Workaround with quosures : sum(!!!col_vars_levels)
        tabs <- tabs %>%
          dplyr::mutate(
            !!rlang::sym(name[2]) :=
              sum(!!!col_vars_levels[[length(col_vars_levels)]]) %>%
              as_totcol() %>% set_col_var("all_col_vars"))

      } else if (totcol[1] == "each") {
        totcol_names <- purrr::map(paste0(name[2],"_",
                                          names(col_vars_2levels_or_more)),
                                   rlang::sym)
        tabs <-
          purrr::reduce2(col_vars_2levels_or_more, totcol_names, .init = tabs,
                         function(.tab, .levels, .names)
                           dplyr::mutate(.tab, !!.names := sum(!!!.levels) %>%
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
      warning("no groups nor total row(s) found. One added for the whole table")
      return(tab_tot(tabs, "row"))
    } else {
      warning("no groups nor total row(s), but total table found. ",
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
    warning("grouping variable(s) not corresponding to total_rows, ",
            "new groups calculated, based on actual total_rows")
    return(dplyr::relocate(tabs_totrow_groups, totrow_groups, .before = 1) %>%
             dplyr::group_by(totrow_groups)
    )

  }

}



tab_add_totcol_if_no <- function(tabs) {
  if (!any(is_totcol(tabs)) & ! all(get_type(tabs) == "mean") ) {
    tabs <- tabs %>% tab_tot("col", totcol = "last")
    warning("no total column, one was added (from the last non-mean column)")
  }
  tabs
}

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

tab_match_comp_and_tottab <- function(tabs, comp) {
  if(comp == "all" & !any(is_tottab(tabs) & is_totrow(tabs)) ) {
    warning("since 'comp' is 'all', a total table (tab_totaltab) with a ",
            "total row was added")
    tabs <- tabs %>% tab_totaltab('line')
  }
  tabs
}



tab_pct <- function(tabs, pct = "row", #c("row", "col", "all", "all_tabs", "no"),
                    digits = NULL, diff = TRUE, comp = NULL, color = FALSE) { #Add keep/change grouping ?
  #stopifnot(pct[1] %in% c("row", "col", "all", "all_tabs", "no"))
  get_vars         <- tab_get_vars(tabs)
  #row_var         <- rlang::sym(get_vars$row_var) #col_var ??
  col_vars_with_all<- rlang::syms(get_vars$col_vars)
  col_vars_no_all  <- col_vars_with_all %>% purrr::discard(. == "all_col_vars")
  col_means  <- (get_type(tabs) == "mean") %>% purrr::keep(., .) %>% names()
  # col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  tab_vars         <- rlang::syms(get_vars$tab_vars)

  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)


  pct <- vec_recycle(pct, length(col_vars_no_all)) %>%
    purrr::set_names(col_vars_no_all)
  pct[col_means] <- "no"

  if (all(pct == "no")) {
      tabs <- tabs %>% dplyr::mutate(dplyr::across(
        where(~ get_type(.) %in% c("row", "col", "all", "all_tabs")),
        ~ set_pct(., NA_real_) %>% set_type("n") %>%
          set_display("wn")
      ))
  } else {
    #Ready table for percentages (need total rows and cols, compatible grouping)
    if (any(pct == "all_tabs")) {
      if ( !(is_tottab(tabs[nrow(tabs),]) &
             is_totrow(tabs[nrow(tabs),]) &
             any(is_totcol(tabs))) ) {
        warning("since percentages are 'all_tabs', a total table (tab_totaltab) ",
                "was added")
        if (!is_tottab(tabs[nrow(tabs),])) {
          tabs <- tabs %>% tab_totaltab('line')
        }
        tabs <- tabs %>%
          dplyr::with_groups(NULL, ~ tab_match_groups_and_totrows(.) %>%
                               tab_add_totcol_if_no()
          )
      }
    }

    if ( any(pct %in% c("col", "all") ) | (any(pct == "row") & diff == TRUE) ) {
      tabs <- tabs %>% tab_match_groups_and_totrows()
    }

    if ( any(pct %in% c("row", "all")) | (any(pct == "col") & diff == TRUE) ) {
      tabs <- tabs %>% tab_add_totcol_if_no()
    }


    comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
    tabs <- tabs %>% tab_match_comp_and_tottab(comp)

    pct <- c(pct, all_col_vars = dplyr::last(pct[pct != "no"]))
    pct <- purrr::map_chr(tabs, ~ pct[get_col_var(.)] ) %>%
      tidyr::replace_na("no")
    row_pct      <- names(pct)[pct == "row"]
    col_pct      <- names(pct)[pct == "col"]
    all_pct      <- names(pct)[pct == "all"]
    all_tabs_pct <- names(pct)[pct == "all_tabs"]


    #Calculate percentages
    pct_formula <- function(x, pct, tot) {
      switch(pct,
             "row"     =  get_wn(x) / get_wn(tot             ),
             "col"     =  get_wn(x) / get_wn(dplyr::last(x)  ),
             "all"     =  get_wn(x) / get_wn(dplyr::last(tot)),
             "all_tabs"=  get_wn(x) / get_wn(dplyr::last(tot)),
             NA_real_)
    }
    #For each var, the first total column at the right is taken
    tot_cols <- detect_totcols(tabs)


    if (any(pct != "all_tabs")) {
      pct_nat <- pct %>% stringr::str_replace("all_tabs", "no") %>%
        purrr::set_names(names(pct))

      tabs <- tabs %>%
        dplyr::mutate(dplyr::across(
          where(~ is_fmt(.) & !get_type(.) == "mean"),
          ~ set_pct(., pct_formula(
            .,
            pct = pct_nat[[dplyr::cur_column()]],
            tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
          )) %>%
            set_display(ifelse(pct_nat[[dplyr::cur_column()]] != "no", "pct", "wn")) %>%
            set_type(pct_nat[[dplyr::cur_column()]])
        ))
    }

    if (any(pct == "all_tabs")) {
      tabs <- tabs %>%
        dplyr::with_groups(
          NULL,
          ~ dplyr::mutate(., dplyr::across(
            tidyselect::all_of(all_tabs_pct),
            ~ set_pct(., pct_formula(
              .,
              pct = "all_tabs",
              tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
            )) %>%
              set_display("pct") %>% set_type("all_tabs")
          ))
        )
    }

    #Set digits if provided. Always zero digits for the 100% cells
    if (!is.null(digits)) {
      digits <- vec_recycle(digits, length(col_vars)) %>% purrr::set_names(col_vars)
      digits <- c(digits, all_col_vars = dplyr::last(digits[!is.na(digits)]))
      digits <- purrr::map_dbl(tabs, ~ digits[get_col_var(.)] )
      digits[pct == "no"] <- NA_real_

      digits_cols <- names(digits)[!is.na(digits)]

      tabs <- tabs %>% dplyr::mutate(dplyr::across(
        tidyselect::all_of(digits_cols),
        ~ set_digits(., as.integer(digits[[dplyr::cur_column()]])) ))
    }

    if (length(row_pct     ) != 0) tabs <- tabs %>% dplyr::mutate(dplyr::across(
      where(is_totcol) & tidyselect::all_of(row_pct), ~ set_digits(., 0L)))
    if (length(col_pct     ) != 0) tabs <- tabs %>% dplyr::mutate(dplyr::across(
      tidyselect::all_of(col_pct),
      ~ dplyr::if_else(is_totrow(.), set_digits(., 0L), .)))
    if (length(all_pct     ) != 0) tabs <- tabs %>% dplyr::mutate(dplyr::across(
      where(is_totcol) & tidyselect::all_of(all_pct),
      ~ dplyr::if_else(is_totrow(.), set_digits(., 0L), .)))
    if (length(all_tabs_pct) != 0) tabs <- dplyr::ungroup(tabs) %>%
      dplyr::mutate(., dplyr::across(
        where(is_totcol) & tidyselect::all_of(all_tabs_pct),
        ~ dplyr::if_else(dplyr::row_number()==dplyr::n(), set_digits(., 0L), .))) %>%
      dplyr::group_by(!!!rlang::syms(groups))
  }

  type <- get_type(tabs)
  #Calculate diffs (used to color pct depending on spread from row or col mean)
  if ( diff == TRUE & any(type %in% c("row", "col", "mean")) ) {
    diff_formula <- function(x, type, tot) {
      switch(type,
             "row"     =  get_pct(x)  - get_pct(dplyr::last(x  )),
             "col"     =  get_pct(x)  - get_pct(tot             ),
             "mean"    =  get_mean(x) / get_mean(dplyr::last(x) ),
             NA_real_)
    }

    if ( comp == "all" & any(type %in% c("row", "mean")) ) {
      tabs <- tabs %>%
        dplyr::with_groups(
          NULL,
          ~ dplyr::mutate(., dplyr::across(
            where(is_fmt),
            ~ set_diff(., diff_formula(
              .,
              type = type[[dplyr::cur_column()]],
              tot  = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
            ))
          ))
        )

    } else {
      tabs <- tabs %>%
        dplyr::mutate(dplyr::across(
          where(is_fmt),
          ~ set_diff(., diff_formula(
            .,
            type = type[[dplyr::cur_column()]],
            tot  = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
          ))
        ))
    }

    if ( any(type %in% c("row", "mean")) ) tabs <- tabs %>%
        dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp(., comp[1])))

    if (color == TRUE) {
      tabs <- tabs %>%
        dplyr::mutate(dplyr::across(
          where(is_fmt),
          ~ set_color(., ifelse(
            type[[dplyr::cur_column()]] %in% c("row", "col", "mean"),
            "diff",
            get_color(.)
          )) ))
    }
  }

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
#     "cell"      = ci_formula_factory(fmt0(pct)),
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



#Ci spread (negative numbers mean no significant difference)
tab_ci <- function(tabs,
                   ci = "auto",
                   comp = NULL,
                   conf_level = 0.95, color = "no",
                   visible = FALSE) {
  stopifnot(all(ci %in% c("auto", "cell", "diff", "no")), #"r_to_r", "c_to_c", "tab_to_tab",
            all(comp %in%  c("tab", "all"))
  )

  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  get_vars        <- tab_get_vars(tabs)
  col_vars_with_all <- rlang::syms(get_vars$col_vars)
  col_vars_no_all   <- col_vars_with_all %>% purrr::discard(. == "all_col_vars")

  fmtc <- purrr::map_lgl(tabs, is_fmt)
  ci <- vec_recycle(ci, length(col_vars_no_all)) %>%
    purrr::set_names(col_vars_no_all)
  ci <- c(ci, all_col_vars = dplyr::last(ci[ci != "no"]))
  ci <- purrr::map_chr(tabs, ~ ci[get_col_var(.)] ) %>%
    tidyr::replace_na(NA_character_)

  visible <- vec_recycle(visible, length(col_vars_no_all)) %>%
    purrr::set_names(col_vars_no_all)
  visible <- c(visible, all_col_vars = dplyr::last(visible[visible != "no"]))
  visible <- purrr::map_lgl(tabs, ~ visible[get_col_var(.)] ) %>%
    tidyr::replace_na(FALSE)


  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs %>% tab_match_comp_and_tottab(comp)

  type <- get_type(tabs)

  tot_cols <- detect_totcols(tabs)
  tot_cols[is.na(ci)] <- list(rlang::sym(""))
  names_totcols <- tot_cols %>% purrr::map_chr(as.character) %>% unique() %>%
    purrr::discard(. == "")

  ci[fmtc] <- dplyr::case_when(
    !type[fmtc] %in% c("mean", "row", "col", "all", "all_tabs") ~ "no"    ,
    ci[fmtc] == "cell"                                          ~ "cell"  ,
    ci[fmtc] == "diff"   & type[fmtc] %in% c("row","mean")  ~ "diff_row"  ,
    ci[fmtc] == "diff"   & type[fmtc] == "col"              ~ "diff_col"  ,

    ci[fmtc] == "auto"   & type[fmtc] %in% c("row","mean")  ~ "diff_row"  ,
    ci[fmtc] == "auto"   & type[fmtc] == "col"              ~ "diff_col"  ,
    ci[fmtc] == "auto"   & type[fmtc] %in% c("all","all_tabs") ~ "cell"   ,

    TRUE                                                       ~ "no"
  )


  #Depending of column type and ci type, not calculate ci
  ci <- dplyr::if_else(
    condition = (!type %in% c("row", "col", "all", "all_tabs", "mean")) |
      (ci %in% c("diff_col", "spread_col") & type == "mean"),
    true = "no",
    false = ci
  )
  ci_with_tot <- ci %>% purrr::set_names(names(tabs))
  ci <- dplyr::if_else(condition =  ci %in% c("diff_col", "spread_col",
                                              "diff_row", "spread_row") &
                         names(tabs) %in% names_totcols,
                       true = "no",
                       false = ci                                        )
  ci <- ci %>% purrr::set_names(names(tabs))
  ci_yes <- !is.na(ci) & ! ci == "no"

  if (any(ci_yes)) {
    #Ready table for percentages (needed totals, compatible grouping)
    if ( any(ci == "diff_col" ) ) tabs <- tabs %>% tab_add_totcol_if_no()
    if ( any(ci == "diff_row") ) {
      tabs <- switch(comp[1],
                     "tab" = tabs %>% tab_match_groups_and_totrows(),
                     "all" = tabs %>% dplyr::ungroup()               )
    }
    #comp == "all", test if there is a total table with totalrow ----
    #Problems with ci = "no" ? ----


    tabs_nogroup <- tabs %>% dplyr::ungroup() %>% .[ci_yes]

    #Compute all variables needed to calculate ci in different tabs
    xbase <- tabs_nogroup %>%
      dplyr::mutate(dplyr::across(.fns =  ~ dplyr::if_else(
        condition = get_display(.) == "mean",
        true      = get_mean(.),
        false     = get_pct(.)
      )))

    xvar <- tabs_nogroup %>%
      dplyr::mutate(dplyr::across(.fns = ~ dplyr::if_else(
        condition = get_display(.) == "mean",
        true      = get_var(.),
        false     = NA_real_
      )))

    ybase <-
      tibble::tibble(ci, tot_cols, names = rlang::syms(names(tabs))) %>%
      dplyr::filter(ci_yes) %>%
      purrr::pmap_df(function(ci, tot_cols, names) switch(
        ci,
        "cell"     = NA_real_,
        "diff_col" = dplyr::pull(tabs, !!tot_cols),
        "diff_row" = dplyr::mutate(tabs, comp = dplyr::last(!!names)) %>%
          dplyr::pull(comp)
      ))

    yvar <- ybase %>%
      dplyr::mutate(dplyr::across(where(~ !get_type(.)=="mean"), ~NA_real_)) %>%
      dplyr::mutate(dplyr::across(where(~ get_type(.) =="mean"), get_var))

    ybase <- ybase %>%
      dplyr::mutate(dplyr::across(
        where(~ is_fmt(.) & !get_type(.) == "mean"),
        get_pct
      )) %>%
      dplyr::mutate(dplyr::across( where(~ get_type(.) == "mean"), get_mean))

    xn <-
      tibble::tibble(type, tot_cols, names = rlang::syms(names(tabs))) %>%
      dplyr::filter(ci_yes) %>%
      purrr::pmap_df(function(type, tot_cols, names) switch(
        type,
        "row"      = dplyr::pull(tabs, !!tot_cols) %>% get_n(),
        "mean"     = dplyr::pull(tabs, !!names   ) %>% get_n(),
        "col"      = dplyr::mutate(tabs, xn = dplyr::last(get_n(!!names)) ) %>%
          dplyr::pull(xn),
        "all"      = ,
        "all_tabs" = dplyr::mutate(tabs, xn = dplyr::last(get_n(!!tot_cols)) ) %>%
          dplyr::pull(xn),
        NA_integer_
      ))

    yn <-
      tibble::tibble(ci, type, tot_cols, names = rlang::syms(names(tabs))) %>%
      dplyr::filter(ci_yes) %>%
      purrr::pmap_df(function(ci, type, tot_cols, names) switch(
        ci,
        "cell"       = NA_real_,
        "diff_col"   =
          switch(type,
                 "row"      = dplyr::pull(tabs, !!tot_cols) %>% get_n(),
                 "col"      = ,
                 "all"      = ,
                 "all_tabs" =
                   dplyr::transmute(tabs, yn = dplyr::last(get_n(!!tot_cols)) ) %>%
                   dplyr::pull(yn),
                 NA_real_
          ),
        "diff_row"   =
          switch(type,
                 "mean"     = ,
                 "col"      =
                   dplyr::transmute(tabs, yn = dplyr::last(get_n(!!names)) ) %>%
                   dplyr::pull(yn),
                 "row"      = ,
                 "all"      = ,
                 "all_tabs" =
                   dplyr::transmute(tabs, yn = dplyr::last(get_n(!!tot_cols)) ) %>%
                   dplyr::pull(yn),
                 NA_real_
          )
      ) )


    #Formulas :

    zs <- zscore_formula(conf_level)

    ci_mean      <- function(xvar, xn) {
      zs * sqrt( xvar / xn )
    }

    ci_mean_diff <- function(xvar, xn, yvar, yn) {
      zs * sqrt( xvar/xn + yvar/yn )
    }

    # ci_mean_spread <-  function(xmean, xvar, xn, ymean, yvar, yn) {
    #   diff <-  xmean - ymean
    #   abs(diff) - zs * sqrt( xvar/yn + yvar/yn )
    # }

    ci_base <- function(xpct, xn) {
      zs * sqrt(xpct*(1 - xpct)/xn)
    }

    ci_diff <-  function(xpct, xn, ypct, yn) {
      zs * sqrt( xpct*(1 - xpct)/xn   +   ypct*(1 - ypct)/yn )
    }

    # ci_diff_spread <-  function(xpct, xn, ypct, yn) {
    #   diff <-  abs(xpct - ypct)
    #   diff - zs * sqrt( xpct*(1 - xpct)/xn   +   ypct*(1 - ypct)/yn )
    # }

    #Calculate the confidence intervals
    ci_map <-
      list(xbase = xbase, xvar = xvar,
           ybase = ybase, yvar = yvar,
           xn = xn, yn = yn) %>%
      purrr::map(~purrr::map(., ~ .)) %>%
      purrr::transpose() %>% purrr::map(~ tibble::as_tibble(.)) %>%
      tibble::tibble(.name_repair = ~ "ci_map") %>%
      tibble::add_column(ci = ci[ci_yes], type = type[ci_yes]) %>%
      dplyr::mutate(ci_map = dplyr::if_else(
        ci %in% c("diff_col", "diff_row"),
        true  = purrr::map(ci_map, ~ dplyr::mutate(., xn = dplyr::if_else(
          condition =
            ( comp == "tab" & is_totrow(tabs) ) |
            ( comp == "all" & append(rep(FALSE, nrow(tabs) - 1), TRUE)),
          true      = NA_integer_,
          false     = xn)
        )),
        false = ci_map
      ) %>% purrr::set_names(names(tabs)[ci_yes])
      )

    calculations <- ci_map %>%
      purrr::pmap(function(ci_map, ci, type)
        dplyr::mutate(ci_map, res = switch(
          ci,
          "cell"        = switch(type,
                                 "mean" = ci_mean(xvar = xvar, xn = xn),
                                 ci_base(xpct = xbase, xn = xn)
          ),
          "diff_col"   = ,
          "diff_row"   = switch(type,
                                "mean" = ci_mean_diff(xvar = xvar, xn = xn,
                                                      yvar = yvar, yn = yn),
                                ci_diff(xpct = xbase, xn = xn,
                                        ypct = ybase, yn = yn)
          ),
          # "spread_col" = ,
          # "spread_row" = switch(type,
          #                       "mean" = ci_mean_spread(
          #                         xmean = xbase,  xvar = xvar, xn = xn,
          #                         ymean = ybase,  yvar = yvar, yn = yn
          #                       ),
          #                       ci_diff_spread(xpct = xbase, xn = xn,
          #                                      ypct = ybase, yn = yn)
          # ),
          "no"         = NA_real_,
        ) ) )

    result <- calculations %>% purrr::map_df(~ dplyr::pull(., res))

    tabs[ci_yes] <- purrr::map2_df(tabs[ci_yes], result, ~ set_ci(.x, .y) )

    #Change ci_type and color, even for totals with no ci result
    ci_with_tot <- stringr::str_remove(ci_with_tot, "_row|_col")
    ci_yes_tot  <- !is.na(ci_with_tot) & !ci_with_tot == "no"

    tabs[ci_yes_tot] <-
      purrr::map2_df(tabs[ci_yes_tot],
                     ci_with_tot[ci_yes_tot],
                     ~ set_ci_type(.x, .y) %>%
                       set_color(
                         ifelse(!is.null(color[1]) & ! color[1] %in% c("no", ""),
                                color[1], get_color(.))
                       ))

    if (any(ci == "diff_row")) tabs <- tabs %>%
      dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp(., comp[1])))

    # Change types for columns where visible = TRUE
    if (any(visible)) {
      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          tidyselect::all_of(names(visible)[visible]),
          ~ switch(
            ci[dplyr::cur_column()],
            "cell" = set_display(., ifelse(get_type(.) == "mean",
                                           "mean_ci", "pct_ci")),
            set_display(., "ci")
          ) ) )
    }
  }


  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext, chi2 = chi2)
  }
}



var_contrib <- function(x, tot, calc = c("ctr", "expected_freq", "spread",
                                         "binding_ratio",
                                         "ctr_with_sign"),
                        comp = NULL) {
  # x   <- tabs$Encadrant
  # tot <- tabs$Total
  xout             <- get_wn(x)
  tot <- get_wn(tot)
  if (!is.null(comp)) { if (comp == "all") {
    tot_row_or_tab <- is_totrow(x[-length(x)]) | is_tottab(x[-length(x)])
    xout[-length(x)] <-
      dplyr::if_else(tot_row_or_tab, rep(0, length(x) -1), xout[-length(x)])

    tot[-length(x)] <-
      dplyr::if_else(tot_row_or_tab, rep(0, length(x) -1), tot [-length(x)])
  }}

  observed_freq     <- xout / dplyr::last(tot)
  expected_freq     <- dplyr::last(xout) * tot / dplyr::last(tot)^2
  spread            <- observed_freq - expected_freq
  switch(calc[1],
         "ctr"           = spread^2 / expected_freq, # = expected_freq * binding_ratio^2,
         "spread"        = spread                  ,
         "binding_ratio" = spread   / expected_freq,
         "expected_freq" = expected_freq           ,
         "ctr_with_sign" = sign(spread) * spread ^2 / expected_freq
  )
  #tidyr::replace_na(res, 0)
}




tab_chi2 <- function(tabs, calc = c("ctr", "p", "var", "counts"),
                     comp = NULL, color = c("no", "auto", "all", "all_pct")
) {
  get_vars        <- tab_get_vars(tabs)
  row_var         <- get_vars$row_var
  #col_vars        <- rlang::sym(get_vars$col_vars)
  col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  groups          <- rlang::syms(dplyr::group_vars(tabs))
  #ngroups         <- dplyr::n_groups(tabs)

  stopifnot(all(calc %in% c("all", "ctr", "p", "var", "counts")))
  if ("all" %in% calc) calc <- c("ctr", "p", "var", "counts")
  subtext         <- get_subtext(tabs)

  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs %>% tab_match_comp_and_tottab(comp)
  tabs <- tabs %>% tab_match_groups_and_totrows() %>% tab_add_totcol_if_no()

  if (comp == "all") tabs <- tabs %>% dplyr::ungroup()

  tot_cols <- detect_totcols(tabs)

  is_a_mean <-
    purrr::map_lgl(col_vars_levels,
                   ~ purrr::map_lgl(dplyr::select(dplyr::ungroup(tabs), !!!.),
                                    ~ get_type(.) == "mean") %>% any()
    )
  all_col_tot <- names(col_vars_levels) == "all_col_vars"

  tot_cols_names <- purrr::map_lgl(tabs, is_totcol) %>%  .[.] %>% names()
  col_vars_levels_no_tot <-
    purrr::map(col_vars_levels,~ purrr::discard(., . %in% tot_cols_names ) )



  #Calculate absolute contributions to variance (with spread sign)
  if ("ctr" %in% calc | "var" %in% calc) {
    tabs <- tabs %>%
      dplyr::mutate(dplyr::across(
        where(~ is_fmt(.) & !get_type(.) == "mean"),
        ~ set_var(., var_contrib(
          .,
          tot  = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]]),
          calc = "ctr_with_sign",
          comp = comp
        ) )
      ))
    # tabs %>% dplyr::mutate(dplyr::across( where(is_fmt), ~ get_var(.)   ))


    #Calculate variances (per groups and per column variables)
    variances_calc <-
      purrr::map_if(col_vars_levels, !is_a_mean & !all_col_tot,
                    .f    = ~ dplyr::select(tabs, !!!groups, !!!.) %>%
                      dplyr::select(where(~ !is_totcol(.))) %>%
                      dplyr::mutate(dplyr::across(where(is_fmt),
                                                  ~ abs(get_var(.)))),
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
                    .f    = ~ dplyr::group_split(.[!is_totrow(tabs),]) %>% #.keep = FALSE
                      purrr::map(~ dplyr::select(., where(is.double))) %>%
                      purrr::map_dbl(~ rowSums(., na.rm = TRUE) %>% sum(na.rm = TRUE)),
                    .else = ~ NA_real_ #Weighted mean of variances ?
      )


    cells_calc <- cells_by_group <-
      rlang::rep_along(variances_calc[!all_col_tot], NA_integer_)

    cells_calc[!is_a_mean[!all_col_tot]] <-
      variances_calc[!all_col_tot & !is_a_mean] %>%
      purrr::map(~ tibble::add_column(.x,  totrows = is_totrow(tabs)) %>%
                   dplyr::mutate(dplyr::across(
                     where(is.double), ~ dplyr::if_else(is.na(.), 0, 1) %>%
                       dplyr::if_else(totrows, 0, .)
                   )) %>%
                   dplyr::select(-totrows)
      )

    cells_by_row <- cells_calc[!is_a_mean & !all_col_tot] %>%
      purrr::map2(col_vars_levels_no_tot[!all_col_tot & !is_a_mean],
                  ~ dplyr::mutate(.x, cells = sum(!!!.y), .groups = "drop") %>%
                    dplyr::pull(cells)
      )

    cells_by_group[!is_a_mean[!all_col_tot]] <-
      cells_calc[!is_a_mean[!all_col_tot]] %>%
      purrr::map2(col_vars_levels_no_tot[!all_col_tot & !is_a_mean],
                  ~ dplyr::summarise(.x[!is_totrow(tabs),],
                                     cells = sum(!!!.y), .groups = "drop") %>%
                    dplyr::pull(cells)
      )
  }


  #Calculate relative contributions to variance
  if ("ctr" %in% calc) {
    tabs <-
      purrr::reduce2(col_vars_levels[!is_a_mean & !all_col_tot],
                     purrr::transpose(list(var = variances_by_row,
                                           cell = cells_by_row)),
                     .init = tabs, .f = function(.tab, .levels, .l)
                       tibble::add_column(.tab,
                                          .var  = .l[["var"]],
                                          .cell = .l[["cell"]]) %>%
                       dplyr::mutate(dplyr::across(
                         tidyselect::all_of(purrr::map_chr(.levels, as.character)),
                         ~ dplyr::if_else(condition = is_totrow(.),
                                          true      = set_ctr(., 1/.cell),
                                          false     = set_ctr(., .var   ) )
                       )) %>%
                       select(-.var, -.cell)
      )

    tabs <- tabs %>%
      dplyr::mutate(dplyr::across(
        where(is_fmt),
        ~ dplyr::if_else(condition = (comp == "tab" & is_totrow(.)) |
                           (comp == "all" & is_totrow(.) & is_tottab(.)),
                         true      = .,
                         false     = set_ctr(., get_var(.) / get_ctr(.)) )
      ))

    tabs <- tabs %>%
      dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp(., comp[1])))

    if (color[1] != "no" & !is.na(color[1])) {
      color_condition <-
        switch(color[1],
               "auto"    = c("n", "all", "all_tabs"),
               "all"     = c("n", "row", "col", "all", "all_tabs"),
               "all_pct" = c("all", "all_tabs")
        )

      tabs <- tabs %>% dplyr::mutate(dplyr::across(
        where(~ get_type(.) %in% color_condition),
        ~ set_color(., "contrib")
      ))
    }

    # tabs %>% dplyr::mutate(dplyr::across(where(is_fmt), get_ctr))
    # tabs %>% dplyr::mutate(dplyr::across(where(is_fmt), ~ set_display(., "ctr")))



    # #Relative contributions of col_vars levels (on total rows)
    # tabs <- tabs %>%
    #   dplyr::mutate(dplyr::across(
    #     where(is_fmt),
    #     ~ dplyr::if_else(condition = dplyr::row_number() == dplyr::n(),
    #                      true      = set_ctr(., sum(abs(get_ctr(.)))),
    #                      false     = . )
    #   ))
    # #tabs %>%  dplyr::mutate(dplyr::across( where(is_fmt), ~ set_display(., "ctr")  ))


    #mean_contrib <- contrib_no_sign %>% map(~ 1 / ( ncol(.) * nrow(.) ) )
  }

  tabs2 <- if (comp == "all") {
    tabs[!is_totrow(tabs) & !is_tottab(tabs),]
  } else {
    tabs[!is_totrow(tabs),]
  }


  #Calculate unweighted counts
  if ("counts" %in% calc) {
    counts <-  purrr::map(
      col_vars_levels[!all_col_tot],
      ~ dplyr::select(tabs2, !!!groups, !!!.) %>%
        dplyr::select(where(~ !is_totcol(.))) %>%
        dplyr::group_split() %>%
        purrr::map( ~ dplyr::select(., where(is_fmt)) ) %>%
        purrr::map_int(~ dplyr::mutate(.,dplyr::across(.fns = get_n)) %>%
                         rowSums() %>% sum() %>% as.integer()
        )
    )
  }


  #Calculate pvalue : variance was calculated with weights, and here we want unwtd counts
  if ("p" %in% calc) {

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
                    ~ dplyr::select(tabs2, !!!groups, !!!.) %>%
                      dplyr::group_split() %>%
                      purrr::map( ~ dplyr::select(., where(is_fmt)) ) %>%
                      purrr::map(
                        ~ dplyr::mutate(., dplyr::across(.fns = get_n)) %>%
                          dplyr::select(-where(~ sum(., na.rm = TRUE) == 0)) %>%
                          dplyr::rowwise() %>%
                          dplyr::filter(! sum(dplyr::c_across(),
                                              na.rm = TRUE) == 0 ) %>%
                          dplyr::ungroup()
                      ) %>%
                      purrr::map_if(purrr::map_lgl(., ~ nrow(.) > 0 & ncol(.) > 0),
                                    .f = quiet_chisq_test,
                                    .else = ~ tibble::tibble(pvalue = NA_real_,
                                                             warnings = "",
                                                             df = NA_integer_)
                      ) %>% dplyr::bind_rows(),

                    .else = ~ tibble::tibble(pvalue = NA_real_, warnings = "",
                                             df = NA_integer_)
      )
    pvalue_p     <- purrr::map(pvalues, ~ dplyr::pull(., pvalue))
    #pvalue_w     <- purrr::map(pvalues, ~ dplyr::pull(., warnings))
    pvalue_df    <- purrr::map(pvalues, ~ dplyr::pull(., df) %>% as.integer())
  }


  #Assemble everything and put it into the metadata of the tab
  #if (length(groups) != 0) {
  tables <- tabs[!is_totrow(tabs),] %>% dplyr::select(!where(is_totcol)) %>%
    dplyr::summarise(row_var = factor(row_var), .groups = "drop") #%>%
  #dplyr::mutate(dplyr::across(.fns = ~ stringr::str_remove_all(., "/") ))


  # if (length(groups) != 0) tables <- tables %>%
  #   dplyr::transmute(tables = stringr::str_c(!!!groups, sep = "/"))
  #
  # tables <- tables %>%
  #   dplyr::mutate(tables = dplyr::if_else(
  #     condition = stringr::str_extract(.data$tables, "^.*(?=/)") ==
  #       stringr::str_extract(.data$tables, "(?<=/).*$"),
  #     true      = stringr::str_extract(.data$tables, "^.*(?=/)"),
  #     false     = .data$tables
  #   ))

  if (!"p"      %in% calc) {
    pvalue_p  <- NA_real_
    #pvalue_w  <- NA_character_
    pvalue_df <- NA_integer_
  }
  if (!"var"    %in% calc) {
    cells_by_group     <- NA_integer_
    variances_by_group <- NA_real_
  }
  if (!"counts" %in% calc) counts <- NA_integer_

  purrr::map(counts            , length)
  purrr::map(pvalue_p          , length)
  purrr::map(variances_by_group, length)
  purrr::map(cells_by_group    , length)
  purrr::map(pvalue_df         , length)

  chi2 <-
    purrr::pmap(list(counts, pvalue_p, #pvalue_w,
                     variances_by_group, cells_by_group, pvalue_df),
                ~ dplyr::bind_cols(tables,
                                   tibble::tibble(count    = ..1,
                                                  pvalue   = ..2,
                                                  #warnings = ..3,
                                                  variance = ..3,
                                                  cells    = ..4,
                                                  df       = ..5,
                                   ))
    ) #%>%
  # purrr::map(~ dplyr::mutate(., warnings = dplyr::if_else(
  #   stringr::str_detect(.data$warnings, "incorrect"), "!", ""))
  # )
  chi2 <- chi2 %>% purrr::imap(
    ~ dplyr::mutate(.x, dplyr::across(
      tidyselect::any_of(c("count", "pvalue",
                           "variance", "cells", "df")), as.double)) %>%
      tidyr::pivot_longer(cols = c("cells","df", "variance",
                                   "pvalue", "count"),
                          names_to = "chi2 stats",
                          values_to = .y) %>%
      dplyr::mutate(dplyr::across(
        where(is.double),
        ~ fmt(display = "var", type = "n", n =  0L, var  = .,
              col_var = "chi2_cols")
      ))
  ) %>% purrr::map_if(append(FALSE, rep(TRUE, length(.) - 1)),
                      ~ dplyr::select(., where(is_fmt))
  ) %>% dplyr::bind_cols() %>%
    dplyr::mutate(dplyr::across(where(is_fmt), ~ dplyr::case_when(
      `chi2 stats` == "variance" ~ set_digits(., 4L),
      `chi2 stats` == "pvalue"   ~ set_display(., "pct") %>%
        set_pct(get_var(.)) %>% set_digits(., 2L),
      `chi2 stats` == "count"    ~ as_totrow(.),
      TRUE                       ~ .
    ))) %>% new_tab()


  tabs <- tabs %>% dplyr::select(-tidyselect::any_of("tottabs"))

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
    fmtc <- purrr::map_lgl(tabs, is_fmt)
    col_vars       <- get_col_var(tabs[fmtc]) %>% purrr::discard(is.na(.))
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

    variables     <- rlang::expr(c(...))
    pos_variables <- tidyselect::eval_select(variables, data)
    variables     <- rlang::syms(names(pos_variables))

    vars_not_numeric <-
      dplyr::select(data[pos_variables], where(~!is.numeric(.))) %>% #!!!variables
      colnames() %>% rlang::syms()

    data <- data %>%
      dplyr::mutate_at(dplyr::vars(!!!vars_not_numeric), as.factor)

    data <- data %>%
      dplyr::mutate(dplyr::across(
        where(is.ordered), ~ magrittr::set_class(., class(.) %>% .[. != "ordered"])
      )) #remove class ordered ----

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

    data #%>%
    # dplyr::select(!!!variables, dplyr::everything()) %>%
    # dplyr::select(where(is.factor), where(is.numeric), dplyr::everything())
  }




#' @keywords internal
get_orig_name <- function(df) { #Thanks to https://stackoverflow.com/questions/30057278
  i <- 1
  while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  deparse(parent.frame(i)$lhs)  #list(name = deparse(parent.frame(i)$lhs), output = df)
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
