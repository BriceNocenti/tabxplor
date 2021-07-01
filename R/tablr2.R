#Ajouter : - Rename levels "Total" and "Ensemble"
#          - like tab and tab_multi, putting NA in a variable set no_var
#' Prepare data for \code{\link{tab_df}}.
#' @param data A dataframe.
#' @param var1,var2,var3 Variables must be the same that will then pass in
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
  function(data, var1, var2, ...,
           show_na = TRUE, cleannames = TRUE,
           rare_to_other = FALSE, minimum_headcount = 30) {

    if (missing(var1)) {
      data %<>% dplyr::mutate(no_var1 = factor("n"))
      var1 <- rlang::expr(no_var1)
    } else {
      var1 <- rlang::ensym(var1)
    }

    if (missing(var2)) {
      data %<>% dplyr::mutate(no_var2 = factor("n"))
      var2 <- rlang::expr(no_var2)
    } else {
      var2 <- rlang::ensym(var2)
    }

    if (missing(...)) {
      data %<>% dplyr::mutate(no_var3 = factor(" "))
      var3 <- rlang::exprs(no_var3)
    } else {
      var3 <- rlang::ensyms(...)
    }

    if (as.character(var3)[[1]] == "no_var3") {no_var3 <- TRUE} else {no_var3 <- FALSE}


    data <- data %>% dplyr::mutate(dplyr::across(
      !!!var3 | ((!!var1 | !!var2) & !where(is.numeric)),
      as.factor
    ))


    data <- data %>%
      tab_sup_prepare(!!var1, !!var2, !!!var3,
                      drop_sup_na = ! show_na, cleannames = cleannames,
                      rare_to_other = rare_to_other,
                      minimum_headcount = minimum_headcount)

    if (rare_to_other == TRUE & no_var3 == FALSE) {
      # We only count third variable's minimum headcount for the row variable,
      #  otherwise we get problems.
      levelsvar1 <- dplyr::pull(data, !!var1) %>%
        levels() %>%
        append("Autres") %>%
        unique()

      data <- data %>% dplyr::group_by(!!!var3) %>%
        dplyr::mutate(!!var1 := forcats::fct_lump_min(!!var1, minimum_headcount,
                                                      other_level = "Autres")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(!!var1 := forcats::fct_relevel(!!var1, levelsvar1))

    }
    return(data)
  }




# Fill base fmtn elements while creating the tabs. Fill others using vector attributes for
# rows and cols (digits, type, pct). Save performance and make it easy for tabxl after.
# => Fill digits, type, pct, just before printing (less powerful ?)?

# Not working with list of tabs, but with grouped_df (add empty rows while printing ?).
# => All attributes are on the main level (digits, types, pct are int or char matrixes).

# Group over factors only, to let the opportunity to the user to add character vars ?


#Do tidy-dots work with tribble and pmap ?
# tibble::tribble(~data, ~var1, ~var2, ~..., ~wt,
#                 dat_group123, rlang::as_label(var1), rlang::as_label(var2), as.character(var3), rlang::as_label(wt)) %>%
#   purrr::pmap(tab_core)
tab_core <- function(data, var1, var2, ..., wt, digits = 0, is_grouped = FALSE) {
  if (missing(var1)) {
    data <- data %>% dplyr::mutate(no_var1 = factor("n"))
    var1 <- rlang::expr(no_var1)
  } else {
    var1 <- rlang::ensym(var1)
  }

  if (missing(var2)) {
    data <- data %>% dplyr::mutate(no_var2 = factor("n"))
    var2 <- rlang::expr(no_var2)
  } else {
    var2 <- rlang::ensym(var2)
  }

  if (missing(...)) {
    data <- data %>% dplyr::mutate(no_var3 = factor(" "))
    var3 <- rlang::exprs(no_var3)
  } else {
    var3 <- rlang::ensyms(...)
  }

  if (missing(wt)) {
    data <- data %>% dplyr::mutate(no_weight = 1)
    wt <- rlang::expr(no_weight)
  } else {
    wt <- rlang::ensym(wt)
  }

  #dat_prepare : all filters here (operations on rows copy all columns on memory)
  #remove all unwanted NAs : those we want to keep were turned to explicit before
  data <- data %>% dplyr::select(!!var1, !!var2, !!!var3, !!wt) %>%
    dplyr::with_groups(NULL,
                       ~ dplyr::filter(., dplyr::across(c(!!var1, !!var2, !!!var3),
                                                        ~ ! is.na(.)))) %>%
    dplyr::mutate(!!wt := as.numeric(!!wt))

  var1_type <- ifelse ("numeric" %in% class(dplyr::pull(data, !!var1) ), "numeric", "factor")
  var2_type <- ifelse ("numeric" %in% class(dplyr::pull(data, !!var2) ), "numeric", "factor")
  if (var1_type == "numeric" & var2_type == "numeric") {
    stop("var1 and var2 are both numeric : only one of them can be")
  }
  type <- ifelse(var1_type == "numeric" | var2_type == "numeric", "numeric", "factor")

  if (type == "numeric") {
    num_var <- switch(var1_type, "numeric" = var1, "factor" = var2)
    fct_var <- switch(var1_type, "numeric" = var2, "factor" = var1)
  }

  if (! is_grouped) {
    data <- switch(type,
                  "factor"   = dplyr::group_by(data, !!!var3, !!var1, !!var2),
                  "numeric"  = dplyr::group_by(data, !!!var3, !!fct_var     ) )
  }

  if (type == "numeric") {
    if (rlang::as_name(num_var) %in% dplyr::group_vars(data)) {
      data <- dplyr::ungroup(data, !!num_var)
    }
  }


  tabs <-
    switch(type,
           "factor"  = data %>%
             dplyr::summarise(nums = new_fmtn(type    = "wn",
                                              digits  = as.integer(digits),
                                              n       = dplyr::n(),
                                              wn      = sum(!!wt),
                                              col_var = rlang::as_name(var2)
             ),
             .groups = 'drop_last') %>%
             tidyr::pivot_wider(names_from = !!var2, values_from = nums,
                                values_fill = fmtn0("wn", digits)) %>%
             dplyr::ungroup(!!var1),

           "numeric" = data %>%
             dplyr::summarise(!!num_var := new_fmtn(
               type    = "mean",
               digits  = as.integer(digits),
               n       = dplyr::n(),
               wn      = sum(!!wt),
               mean    = weighted.mean(!!num_var, !!wt, na.rm = TRUE),
               sd      = sqrt(
                 sum(!!wt * (!!num_var - weighted.mean(!!num_var, !!wt, na.rm = T))^2)
                 / !!wt
               )),
               col_var = rlang::as_name(var2),
               .groups = "drop_last")
    )

  if (var1_type == "numeric") tabs <- tabs %>%
    tidyr::pivot_wider(names_from = !!fct_var, values_from = !!num_var,
                       values_fill = fmtn0("mean", digits))

  tabs #%>% dplyr::group_by(!!!var3)
}



#To pass a list of arguments in tab_vars : c(tab_var1, tabvar2, etc.)
tab_many <- function(data, row_var, ..., tab_vars, wt, levels = "all",
                    digits = 0, drop_sup_na = FALSE, cleannames = FALSE, listed = FALSE,
                    totaltab = "no", tot = "no", pct = "no",
                    ci = "no", conf_level = 0.95) {
stopifnot(levels %in% c("first", "all"),
          is.numeric(digits),
          is.logical(drop_sup_na), is.logical(cleannames), is.logical(listed),
          totaltab %in% c("no", "table", "line"),
          tot %in% c("no", "row", "col"),
          pct %in% c("no", "row", "col", "all", "all_tabs"),
          ci %in% c("no", "abs", "col", "row"))

  if (missing(row_var)) {
    data %<>% dplyr::mutate(no_var2 = factor("n"))
    row_var <- rlang::expr(no_var2)
  } else {
    row_var <- rlang::ensym(row_var)
  }

  sup_cols <- rlang::ensyms(...)
  sup_list <- data %>% tab_make_sup_list(sup_cols)
  sup_cols      <-  sup_list$sup_cols %>% rlang::syms()
  sup_cols_num  <-  sup_list$sup_cols_num
  sup_cols_text <-  sup_list$sup_cols_text

  if (missing(tab_vars)) {
    data %<>% dplyr::mutate(no_var3 = factor(" "))
    tab_vars <- rlang::exprs(no_var3)
  } else {
    tab_vars <- rlang::enquo(tab_vars) %>% ensyms_c()
  }

  if (missing(wt)) {
    data %<>% dplyr::mutate(no_weight = 1)
    wt <- rlang::expr(no_weight)
  } else {
    wt <- rlang::ensym(wt)
  }

  # Vectorise arguments : there can be either one argument for all tabs, or one for each
  nvars <- length(sup_cols)
  lvs              <- vec_recycle(levels          , nvars, x_arg = "levels"          )
  digits           <- vec_recycle(digits          , nvars, x_arg = "digits"          )
  #drop_sup_na      <- vec_recycle(drop_sup_na     , nvars, x_arg = "drop_sup_na"     )
  #cleannames       <- vec_recycle(cleannames      , nvars, x_arg = "cleannames"      )
  totaltab         <- vec_recycle(totaltab        , nvars, x_arg = "totaltab"        )
  tot              <- vec_recycle(tot             , nvars, x_arg = "tot"             )
  pct              <- vec_recycle(pct             , nvars, x_arg = "pct"             )
  ci               <- vec_recycle(ci              , nvars, x_arg = "ci"              )
  conf_level <- vec_recycle(conf_level, nvars, x_arg = "conf_level")


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
  if (tot[sup_cols_num] %in% c("both", "row_col", "col_row")) tot[sup_cols_num] <- "row"
  if (tot[sup_cols_num] == "col")                             tot[sup_cols_num] <- "no"
  ctt  <- totaltab != "no"
  ctot <- tot      != "no"
  cpct <- pct      != "no" & sup_cols_text
  cci  <- ci       != "no" & sup_cols_text

  tabs[ctt ] <- tibble::tibble(tabs, totaltab      ) %>% dplyr::filter(ctt ) %>% purrr::pmap(tab_totaltab)
  tabs[ctot] <- tibble::tibble(tabs, tot           ) %>% dplyr::filter(ctot) %>% purrr::pmap(tab_tot     )
  tabs[cpct] <- tibble::tibble(tabs, pct           ) %>% dplyr::filter(cpct) %>% purrr::pmap(tab_pct     )
  tabs[cci ] <- tibble::tibble(tabs, ci, conf_level) %>% dplyr::filter(cci ) %>% purrr::pmap(tab_ci)
  # need a pmodify function to keep attributes, if used ? ----


  #Remove unwanted levels (keep only the first when levels = "first")
  tabs[lv1] <- tabs[lv1] %>%
    purrr::map2(remove_levels,
                ~ dplyr::select(.x, -tidyselect::all_of(.y), -tidyselect::any_of("Total")))


  #Join the list of tabs into a single table, managing duplicated levels
  if (listed == FALSE) {
    duplicated_levels <- tabs %>%
      purrr::map(~ names(.) %>% purrr::discard(. %in% c(row_var, tab_vars))) %>%
      purrr::flatten_chr() %>% .[duplicated(.)] %>% unique()

    if (length(duplicated_levels) != 0) {
      warning(paste0("some levels names are the same for different variables : ",
                     paste0(duplicated_levels, collapse = ", ")))
      tabs <- tabs %>%
        purrr::imap(~ dplyr::rename_with(.x, function(.names)
          dplyr::if_else(.names %in% duplicated_levels, paste0(.y, "_", .names), .names)))
    }

    tabs <-
      purrr::reduce(tabs, dplyr::full_join, by = as.character(c(tab_vars, row_var)))
  }

  tabs
}


tab_totaltab <- function(tabs, totaltab = c("table", "line", "no")) {
  tabs_syms <- tab_cols_syms(tabs)
  var3_names  <- tabs_syms[[1]]
  var2_name   <- tabs_syms[[2]]
  var1_levels <- tabs_syms[[3]]
  # gr <- dplyr::group_vars(tabs)

  if (length(var3_names) == 0) return(tabs)

  # "no" removes all lines starting with "Ensemble" in the grouping variables
  totaltable <- switch(totaltab[1],
                       "no"    = tabs %>%
                         dplyr::with_groups(NULL, ~ dplyr::filter(., dplyr::across(
                           !!!var3_names, ~ ! stringr::str_detect(., "^Ensemble"))
                         )),

                       "table" = tabs %>%
                         dplyr::group_by(!!var2_name) %>%
                         dplyr::summarise(dplyr::across(where(is_fmtn), sum)),

                       "line"  = tabs %>%
                         dplyr::group_by(!!var2_name) %>%
                         dplyr::summarise(dplyr::across(where(is_fmtn), sum)) %>%
                         dplyr::summarise(dplyr::across(where(is_fmtn), sum)) %>%
                         dplyr::mutate(!!var2_name := "TOTAL ENSEMBLE") #English trad Overall ?
  )
  if (totaltab[1] == "no") return(totaltable)

  totaltable <-
    purrr::reduce(var3_names, .init = totaltable,
                  .f = ~ dplyr::mutate(.x, !!.y := factor("Ensemble")))

  tabs %>% dplyr::bind_rows(totaltable)
}

tab_cols_syms <- function(tabs) {
  var1_levels <- names(tabs)[purrr::map_lgl(tabs, is_fmtn)] %>% rlang::syms()
  var2_name   <- names(tabs) %>%
    purrr::discard(. %in% var1_levels) %>% .[length(.)] %>% rlang::sym()
  var3_names  <- names(tabs) %>%
    purrr::discard(. %in% c(var1_levels, var2_name)) %>% rlang::syms()

  list(var3_names, var2_name, var1_levels)
}



tab_tot <- function(tabs,
                    tot = c("row", "col") #, fields = c("num", "type", "digits", "n", "wn")
                    ) {
  # stopifnot(all(fields %in% c("num", "type", "digits", "n", "wn")),
  #           length(fields) > 0)
  tabs_syms <- tab_cols_syms(tabs)
  var3_names  <- tabs_syms[[1]]
  var2_name   <- tabs_syms[[2]]
  var1_levels <- tabs_syms[[3]]
  gr <- dplyr::group_vars(tabs)
  if (any(c("both", "row_col", "col_row") %in% tot)) tot <- c("row", "col")

  # Test and manage grouping variables ----

  #Detect if there is a totaltab of only one line, and don’t apply any total row on it
  #   conditions are created to restraint next operations to the first tabs
  gi <- dplyr::group_indices(tabs)
  ngroups <- dplyr::n_groups(tabs)
  last_group_is_totaltab <- dplyr::select(tabs[gi == ngroups,], !!!var3_names) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(everything(), ~ stringr::str_detect(., "^Ensemble"))) %>%
    dplyr::summarise(dplyr::across(everything(), all)) %>% dplyr::rowwise() %>%
    dplyr::summarise(is_totaltab = all(dplyr::c_across()), .groups = "drop") %>%
    tibble::deframe()
  last_group_size <- dplyr::group_size(tabs) %>% tail(1)
  is_overall_tot <- stringr::str_detect(dplyr::pull(tabs[gi == ngroups,], !!var2_name),
                                        "TOTAL ENSEMBLE") %>% any()
  line_totaltab_condition <- ngroups > 1 & last_group_size == 1 &
    (is_overall_tot | last_group_is_totaltab)
  if (line_totaltab_condition) {
    gis <- gi != ngroups
    gig <- 1:ngroups %>% purrr::discard(. == ngroups)
  } else {
    gis <- rep(TRUE, length(gi))
    gig <- 1:ngroups
  }

  if ("row" %in% tot | is.null(tot)) { #Consider as a total line any var1 beginning by "total"
    tabs[gis,] <- tabs[gis,] %>%
      dplyr::filter_at(2, ~ ! stringr::str_detect(., stringr::regex("^total",
                                                                    ignore_case = TRUE))
      )
  }

  if ("col" %in% tot | is.null(tot)) {
    #previous_totcol <- tabs %>% dplyr::select(1, tidyselect::starts_with("total"))
    tabs <- tabs %>% dplyr::select(- tidyselect::starts_with("total"))
  }

  if ("row" %in% tot){

    if (length(gr) != 0) {
      var3_totals <- dplyr::group_keys(tabs) %>% #dplyr::mutate(bis = PR0) %>%
        tidyr::unite(!!var2_name, sep = "/") %>%
        dplyr::mutate(!!var2_name := paste("Total", !!var2_name) %>%
                        stringr::str_to_upper() %>% as.factor())  #stringr::str_remove_all()
    } else {
      var3_totals <- tibble::tibble(!!var2_name := factor("Total"))
    }
    var3_totals_levels <- var3_totals %>% dplyr::pull(1) %>% levels()

    tabs <- tabs %>%
      dplyr::mutate(!!var2_name := forcats::fct_expand(!!var2_name, var3_totals_levels))

    var2_levels <- dplyr::pull(tabs, !!var2_name) %>% levels()

    totrows <-
      dplyr::summarise(tabs[gis,], dplyr::across(where(is_fmtn), sum)) %>%
      dplyr::bind_cols(var3_totals[gig,]) %>%
      dplyr::mutate(!!var2_name := forcats::fct_expand(!!var2_name, var2_levels)) #%>%
    #dplyr::relocate(!!var2_name, .after = gr[length(gr)])

    tabs <-
      tabs[gis,] %>%
      dplyr::with_groups(NULL, dplyr::bind_rows, totrows) %>%
      dplyr::arrange(.by_group = TRUE) %>%
      dplyr::with_groups(NULL, dplyr::bind_rows, tabs[!gis,])

    # tabs_num <- tabs %>% dplyr::select_if(is_fmtn)
    # previous_totrow <- tabs %>%
    #   dplyr::filter_at(2, ~ stringr::str_detect(., stringr::regex("^total", ignore_case = TRUE)))
    # var2_levels <- levels(dplyr::pull(tabs, 2))
    # var3_levels <- levels(dplyr::pull(tabs, 1))
    # if (nrow(previous_totrow) == 0) { #Not working if more than one grouping variable.
    #   empty_nums <- rep(list(new_fmtn(NA_real_)), length(tabs) - 2) %>%
    #     purrr::set_names(names()) %>%
    #     tibble::as_tibble()
    #   #previous_totrow <-
    #   dplyr::group_keys(tabs) %>% dplyr::mutate(var2 = factor("Total", var2_levels)) %>%
    #     dplyr::bind_cols(empty_nums) %>%
    #     dplyr::group_by(!!!rlang::syms(dplyr::group_vars(tabs)))
    #
    #}
  }

  if ("col" %in% tot) {
    tabs <- tabs %>%
      dplyr::rowwise() %>%  # c_across don’t work : good workaround with quosures
      dplyr::mutate(Total = sum(!!!var1_levels) %>% set_total_col(TRUE)) #%>%
      #dplyr::ungroup() %>% dplyr::mutate(Total = set_total_col(Total, TRUE))
}

  tabs %>% dplyr::group_by(!!!var3_names)



  # #Prepare the data by extracting the needed elements of formatted numbers
  # numbers <- tabs %>% dplyr::group_split() %>%
  #   purrr::map(~ dplyr::select_if(., is_fmtn)) #%>%
  # #purrr::set_names(dplyr::group_keys(tabs) %>% tibble::deframe())
  # if ("num" %in% fields) num <-
  #   purrr::map(numbers, ~ purrr::modify(., ~ field(., "num")))
  # if ("type" %in% fields) {
  #   type <- purrr::map(numbers, ~ purrr::modify(., ~ field(., "type")))
  #   type_n    <- purrr::map(type, ~dplyr::summarise_all(., ~ . == "n"   ))
  #   type_pct  <- purrr::map(type, ~dplyr::summarise_all(., ~ . == "pct" ))
  #   type_mean <- purrr::map(type, ~dplyr::summarise_all(., ~ . == "mean"))
  # }
  # if ("digits" %in% fields) digits <-
  #   purrr::map(numbers, ~ purrr::modify(., ~ field(., "digits")))
  # if ("n"  %in% fields) ns  <- purrr::map(numbers, ~ purrr::modify(., ~ field(., "n" )))
  # if ("wn" %in% fields) wn  <- purrr::map(numbers, ~ purrr::modify(., ~ field(., "wn")))


  # #Total rows
  # if ("row" %in% tot) {
  #   if ("num" %in% fields) r_num <- purrr::map(num, ~ dplyr::summarise_all(., ~ sum(.)) )
  #
  #   if ("type" %in% fields) {
  #     r_type_n <-
  #       purrr::map(type_n, ~ dplyr::summarise_all(., ~ all(.), .groups = "drop") %>%
  #                    dplyr::mutate_all(~ dplyr::if_else(., "n", "")))
  #     r_type_pct <-
  #       purrr::map(type_pct, ~ dplyr::summarise_all(., ~ all(.), .groups = "drop") %>%
  #                    dplyr::mutate_all(~ dplyr::if_else(., "pct", "")))
  #     r_type_mean <-
  #       purrr::map(type_mean, ~ dplyr::summarise_all(., ~ all(.), .groups = "drop") %>%
  #                    dplyr::mutate_all(~ dplyr::if_else(., "mean", "")))
  #     r_type <-
  #       purrr::pmap(list(r_type_n, r_type_pct, r_type_mean),
  #                   ~ dplyr::bind_rows(..1, ..2, ..3) %>%
  #                     dplyr::summarise_all(~ paste0(., collapse = ""), .groups = "drop") )
  #     if( purrr::map(r_type, ~ . == "") %>% purrr::flatten_lgl() %>% any() ) {
  #       warning("Columns with formatted numbers of different types (n, pct, mean).") # To NA ?
  #     }
  #   }
  #
  #   if ("digits" %in% fields) r_digits <-
  #       purrr::map(digits, ~ dplyr::summarise_all(., ~ max(.), .groups = "drop") )
  #   if ("n"  %in% fields) r_ns  <- purrr::map(ns, ~ dplyr::summarise_all(., ~ sum(.)))
  #   if ("wn" %in% fields) r_wn  <- purrr::map(wn, ~ dplyr::summarise_all(., ~ sum(.)))
  #
  #
  #   # if (nrow(previous_totrow) == 0) {
  #   #   #total_row <-
  #   #   rlang::syms(paste0("r_", fields) %>% stringr::str_replace("^r_n$", "r_ns")) %>%
  #   #     purrr::set_names(fields) %>% purrr::map(rlang::eval_tidy) %>%
  #   #     purrr::pmap(
  #   #       ~ dplyr::mutate_all(..1,
  #   #                           ~ new_fmtn(num = ..1, type = ..2, digits = ..3,
  #   #                                      n = ..4, wn = ..5 ))) #%>%
  #   #
  #   #   purrr::set_names(dplyr::group_keys(tabs) %>% tibble::deframe()) %>%
  #   #     purrr::imap(
  #   #       ~ tibble::add_column(.x, !!rlang::sym(names(tabs)[2]) :=
  #   #                              factor("Total"), .before = 1) %>%
  #   #         tibble::add_column(!!rlang::sym(names(tabs)[1]) :=
  #   #                              factor(.y, var3_levels), .before = 1) %>%
  #   #         dplyr::mutate_at(2, ~ forcats::fct_expand(., var2_levels) %>%
  #   #                            forcats::fct_relevel(var2_levels))
  #   #     )
  #   #
  #   #
  #   #     rlang::syms(paste0("r_", fields) %>% stringr::str_replace("^r_n$", "r_ns")) %>%
  #   #       purrr::set_names(fields) %>% purrr::map(rlang::eval_tidy) %>%
  #   #       purrr::map_depth(3, class)
  #   #
  #   #
  #   #
  #   # } else {
  #
  #   #previous_totrow_names <- previous_totrow %>% dplyr::select(1, 2) %>% dplyr::group_split()
  #
  #   total_row <-
  #     purrr::reduce2(
  #       fields,
  #       rlang::syms(paste0("r_", fields) %>% stringr::str_replace("^r_n$", "r_ns")),
  #       .init = previous_totrow %>%
  #         purrr::map(., ~ dplyr::select(., -1, -2)),
  #       .f =
  #         ~ purrr::map2(..1, rlang::eval_tidy(..3),
  #                       function(.prev, .change)
  #                         purrr::modify2(.prev, .change, ~ `field<-`(.x, "wn", .y))
  #         )
  #     ) #%>% .[[1]] %>% dplyr::pull(1) %>% vec_data()
  #
  #   total_row <-
  #     purrr::map2(previous_totrow_names, total_row, ~ dplyr::bind_cols(.x, .y))
  #
  #   #}
  # }



  # #Total columns
  # if ("col" %in% tot) {
  #   if ("num" %in% fields) c_num <- purrr::map(num, ~ rowSums(.) %>% as.double())
  #
  #   if ("type" %in% fields) {
  #   c_type_n <- purrr::map(type_n, ~ dplyr::rowwise(.) %>%
  #                            dplyr::summarise(n    = all(dplyr::c_across()), .groups = "drop") %>%
  #                            dplyr::transmute(n    = dplyr::if_else(n, "n", "")))
  #   c_type_pct <- purrr::map(type_pct, ~ dplyr::rowwise(.) %>%
  #     dplyr::summarise(pct  = all(dplyr::c_across()), .groups = "drop")  %>%
  #     dplyr::transmute(pct  = dplyr::if_else(pct, "pct", "")))
  #   c_type_mean <- purrr::map(type_mean, ~ dplyr::rowwise(.) %>%
  #     dplyr::summarise(mean = all(dplyr::c_across()), .groups = "drop")  %>%
  #     dplyr::transmute(mean = dplyr::if_else(mean, "mean", "")))
  #   c_type <- purrr::pmap(list(c_type_n, c_type_pct, c_type_mean),
  #                         ~ dplyr::bind_cols(..1, ..2, ..3) %>% dplyr::rowwise() %>%
  #                           dplyr::summarise(type = paste0(dplyr::c_across(), collapse = ""), .groups = "drop") %>%
  #                           dplyr::pull(type))
  #   if (any(purrr::flatten_chr(c_type) == "")) {
  #     warning("Rows with formatted numbers of different types (n, pct, mean).") # To NA ?
  #   }
  #   }
  #
  #   if ("digits" %in% fields) c_digits <-
  #       purrr::map(digits, ~ dplyr::rowwise(.) %>%
  #                    dplyr::summarise(digits = max(dplyr::c_across()), .groups = "drop") %>%
  #                    dplyr::pull(digits))
  #   if ("n"  %in% fields) c_ns  <- purrr::map(ns, ~ rowSums(.) %>% as.integer())
  #   if ("wn" %in% fields) c_wn  <- purrr::map(wn, ~ rowSums(.) %>% as.double())
  #
  #   if (! "Total" %in% names(previous_totcol)) {
  #     total_col <-
  #       ls(pattern = "^c_num$|^c_type$|^c_digits$|^c_ns$|^c_wn$") %>% as.list() %>%
  #       purrr::set_names(stringr::str_remove(., "^c_") %>% stringr::str_replace("^ns$", "n")) %>%
  #       purrr::map(~ rlang::sym(.) %>% rlang::eval_tidy()) %>%   #%>% tibble::tibble(All = .) %>%
  #       tibble::as_tibble() %>%
  #       purrr::pmap(new_fmtn) %>%
  #       purrr::map(~ tibble::tibble(Total = .))
  #   } else {
  #     # previous_totcol %>% dplyr::group_split(.keep = FALSE) %>%
  #     #   purrr::map2(c_wn, ~ dplyr::mutate(.x, Total = `field<-`(Total, "wn", .y))) %>% .[[1]] %>%
  #     #   dplyr::pull(Total) %>% vec_data()
  #     total_col <-
  #       purrr::reduce2(
  #         fields,
  #         rlang::syms(paste0("c_", fields) %>% stringr::str_replace("^c_n$", "c_ns")),
  #         .init = dplyr::group_split(previous_totcol,.keep = FALSE),
  #         .f = ~ purrr::map2(..1, rlang::eval_tidy(..3),
  #                            function(.data, .col)
  #                              dplyr::mutate(.data, Total = `field<-`(Total, ..2, .col)))
  #       )
  #   }
  # }

  # tabs <- dplyr::group_split(tabs)
  #
  # if ("col" %in% tot) tabs <- tabs %>%
  #   purrr::map2(total_col, ~ dplyr::bind_cols(.x, .y))
  #
  # if ("row" %in% tot) tabs <- tabs %>%
  #   purrr::map2(total_row, ~ dplyr::bind_rows(.x, .y))
  #
  # tabs %>%
  #   dplyr::bind_rows() %>%
  #   dplyr::group_by(!!rlang::sym(names(.)[1]))
}


pct_formula_gen <- function(pct) {
  switch(pct,
         "row"     = function(x, tot) get_wn(x) / get_wn(tot             ),
         "col"     = function(x, tot) get_wn(x) / get_wn(dplyr::last(x)  ),
         "all"     = function(x, tot) get_wn(x) / get_wn(dplyr::last(tot)),
         "all_tabs"= function(x, tot) get_wn(x) / get_wn(dplyr::last(tot)),
         "no"      = function(x, tot) get_wn(x))
}


tab_pct <- function(tabs, pct = c("row", "col", "all", "all_tabs")) { #Add keep/change grouping ?
  #stopifnot(pct[1] %in% c("row", "col", "all", "all_tabs", "no"))

  # all_tabs : need no test if there is a Total tab ----
  # detect if there are totals, calculate otherwise ----

  if (pct[1] == "all_tabs") {
    tabs <- tabs %>% dplyr::ungroup()

  } else if (pct[1] %in% c("col", "all")) {
    tabs_syms <- tab_cols_syms(tabs)
    var3_names  <- tabs_syms[[1]]
    var1_levels <- tabs_syms[[3]]
    gr <- dplyr::group_vars(tabs)

    if (length(gr) == 0) {
      tabs <- tabs %>% dplyr::group_by(!!!rlang::syms(var3_names))
    } else if  (gr != var3_names) {
      tabs <- tabs %>% dplyr::group_by(!!!rlang::syms(var3_names))
    }
  }

  pct_formula <- pct_formula_gen(pct[1])

  tabs %>%
    dplyr::mutate(dplyr::across(
      where(is_fmtn),
      ~ set_pct(., pct_formula(., .data$Total)) %>%
        set_type("pct") %>% set_pct_type(pct[1])
    )) #%>%
  #dplyr::with_groups(NULL, ~ dplyr::mutate(., dplyr::across(
  #  where(is_fmtn),
  #  ~ set_pct_type(., pct[1]))))
}


# ci_formula_factory <- function(y) {
#   function(x, y, zcore) zcore *
#     sqrt( get_pct(x) * (1 - get_pct(x)) / get_n(x)   +   get_pct(y) * (1 - get_pct(y)) / get_n(y) )
# }
#
# ci_formula_gen <- function(ci) {
#   switch(
#     ci,
#     "col"      = ci_formula_factory(tot),
#     "row"      = ci_formula_factory( dplyr::last(x) ),
#     "abs"      = ci_formula_factory(fmtn0(pct)),
#     #"totaltab" = function(x, tot, zcore) ,
#     # "r_to_r"   = function(x, nx, y, ny, zcore) ,
#     # "c_to_c"   = function(x, nx, y, ny, zcore) ,
#     # "tab_to_t" = function(x, nx, y, ny, zcore) ,
#     "no"       = function(x, tot, zcore) NA_real_
#   )
# }

zscore_formula <- function(conf_level) {
 # Calculate the z-score for the given confidence level (thanks to mindcrime) :
 # https://datascience.stackexchange.com/questions/10093/how-to-find-a-confidence-level-given-the-z-value
  stopifnot(conf_level >= 0, conf_level <= 1)
  qnorm((1 - conf_level)/2,lower.tail = FALSE)
}

ci_formula_gen <- function(ci) {
  switch(
    ci,
    "abs"      = function(x, tot, zcore)
      zcore * sqrt( get_pct(x) * (1 - get_pct(x)) / get_n(x) ),
    "col"      = function(x, tot, zcore)
      zcore * sqrt( get_pct(x) * (1 - get_pct(x)) / get_n(x) +
                      get_pct(tot) * (1 - get_pct(tot)) / get_n(tot) ),
    "row"      = function(x, tot, zcore)
      zcore * sqrt( get_pct(x) * (1 - get_pct(x)) / get_n(x) +
                      get_pct(dplyr::last(x)) * (1 - get_pct(dplyr::last(x))) / get_n(dplyr::last(x)) ),
    "no"       = function(x, tot, zcore) NA_real_
  )
}

#On differences, it calculates the ci margin (negative numbers mean no significant difference)
tab_ci <- function(tabs,
                   ci = c("abs", "col", "row", "no"), #"totaltab", "r_to_r", "c_to_c", "tab_to_tab",
                   conf_level = 0.95) {

  # Test if there is totals, pct, totaltab ----
  # Test grouping variables ----

  zscore <- zscore_formula(conf_level)

  ci_formula <- ci_formula_gen(ci[1])

  #Calculate the confidence intervals (and change type to "ci")
  tabs <- tabs %>%
    dplyr::mutate(dplyr::across(where(is_fmtn),
                                ~ set_ci(., ci_formula(., .data$Total, zscore))
    ))

  #Remove the reference of the calculation (total row or total column), and change type
  tabs <-
    switch(ci[1],
           "col" = tabs %>% dplyr::mutate(dplyr::across(.data$Total,
                                                        ~ set_ci(., NA_real_) )), #%>% set_type("ci")
           "row" = tabs %>%
             dplyr::mutate(dplyr::across(
               where(is_fmtn), ~ set_ci(., dplyr::if_else(dplyr::row_number() == dplyr::n(),
                                                          true = NA_real_,
                                                          false = get_ci(.)  )) )),
           "abs" = tabs %>% dplyr::mutate(dplyr::across(where(is_fmtn),
                                                        ~ set_type(., "pct_ci") )),
           "no"  = tabs %>% dplyr::mutate(dplyr::across(where(is_fmtn),
                                                        ~ set_type(., "pct") ))
    )

  tabs
}




#Ajouter : - Rename levels "Total" and "Ensemble"
#Bug :     - If drop_sup_na = TRUE, NA for numeric variables are kept ----
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

    # data %<>% dplyr::select(!!var3, !!var1, !!var2, !!wt, tidyselect::all_of(c(sup_cols, sup_rows))) %>%
    #   dplyr::select(where(is.factor), where(is.numeric)) %>%
    #   dplyr::select(!!var3, !!var1, !!var2, !!wt, tidyselect::everything())

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
#   dplyr::mutate(no_var2 = "n") %>%
#   dplyr::select(tidyselect::all_of(list_of_vars), EMP2,  no_var2, PR0, pondcal)
# var1 <- rlang::expr(EMP2)
# var2 <- rlang::expr(no_var2)
# var3 <- rlang::expr(PR0)
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
# no_var1 <- FALSE
# no_var2 <- TRUE
# no_var3 <- FALSE
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
#Thanks to Artem Sokolov
# https://stackoverflow.com/questions/52590670/how-to-iterate-inside-the-elements-of-a-quosure-of-rlang-in-r
# get_ast <- function(x) { as.list(x) %>% purrr::map_if(is.call, get_ast) }
ensyms_c <- function(quo) {
  rlang::get_expr(quo) %>% as.list() %>%
    as.character() %>% purrr::discard(. %in% c("c", "list")) %>%
    rlang::syms()
}
# Need to use rlang:quo or rlang:enquo before to works (not to call directly on args)

quo_test <- function(vars1, vars2, ...) {
  rlang::enquo(vars1) %>% ensyms_c() %>% print()
  rlang::enquo(vars2) %>% ensyms_c() %>% print()
  rlang::ensyms(...) %>% print()
}
# quo_test(c(PR1, PR2), list("PE1", "PE2"), PE3, PE4)
# quo_test(PR1, PR2, PR3)


