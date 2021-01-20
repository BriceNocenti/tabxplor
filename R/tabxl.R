
# #tabs <- tabw(FES2017, CSER, PR2017ALL1, SEXE, wt = w6)
# #contrib_calc = "whole", sup_cols = "SD46"

# path = "Tableau"
# replace = FALSE
# open = TRUE
# colnames_rotation = 45
# digits_perc = 3
# digits_no_perc = 3
# digits_quanti = 3
# only_one_sheet = FALSE
# color = "contrib" # "no" #"auto" # c("no", "auto", "contrib")
# hide_near_zero = c("auto", 0.0049, Inf)
# compact = FALSE
# perc_color_breaks = c("+0.05","+0.10","+0.15","+0.25","*2","+0.35",
#                       "-0.05","-0.10","-0.15","-0.25",     "-0.35")
# cell_contrib_color_coeffs = c("1", "2", "5", "-1","-2", "-5")

# #Multicols et multirows :

# dat <- ct2013acm %>% dplyr::rename(`Nombre de contraintes (moy)` = NBCONTR)
# tabs <- dat %>%
#   tabw(EMP, var3 = PR0, sup_cols = list_of_vars, wt = pondcal, cleannames = TRUE, show_na = FALSE, perc = "row", multicols = TRUE) #%>%
#tabw(var2 = EMP, var3 = PR0, sup_rows = list_of_vars, wt = pondcal, cleannames = TRUE, show_na = FALSE, multirows = TRUE) %>%
#tabw(EMP, PR0)

# #Multirows :
# tabs <-
#   tabw(dat, var2 = EMP2, var3 = PR0, sup_rows = list_of_vars, wt = pondcal, cleannames = TRUE, show_na = FALSE, multirows = TRUE)

# tabs <-  ct2013acm %>%
#   tabw(PR0, cah, EMP, wt = pondcal, show_na = FALSE, cleannames = TRUE, perc = "row", tot = "no", sup_cols = c("RWDEMacm", "NBCONTR"), sup_rows = "CONTRADacm", another_total = TRUE) %>%
#   tabw(PE0, cah, wt = pondcal, tot = "all") %>%
#   tabw(EMP, cah, wt = pondcal, tot = "col") %>%
#   tabw(PE0, cah, wt = pondcal, tot = "row", sup_cols = c("RWDEMacm", "NBCONTR"), sup_rows = "CONTRADacm")

# tabs <-  ct2013acm %>%
#   tabw(PR0, cah, EMP, wt = pondcal, show_na = FALSE, cleannames = TRUE, perc = "col", tot = "no",
#        sup_rows = c("RWDEMacm", "NBCONTR"), sup_cols = c("RWDEMacm", "NBCONTR"), another_total = TRUE, sup_contrib = TRUE) %>%
#   tabw(PE0, cah, wt = pondcal, tot = "all", perc = "no", sup_rows = c("RWDEMacm", "NBCONTR"), sup_cols = c("RWDEMacm", "NBCONTR")) %>%
#   tabw(EMP, cah, wt = pondcal, tot = "col", sup_rows = c("RWDEMacm", "NBCONTR"), sup_cols = c("RWDEMacm", "NBCONTR")) %>%
#   tabw(PE0, cah, wt = pondcal, tot = "row", sup_cols = c("RWDEMacm", "NBCONTR"), sup_rows = c("RWDEMacm", "NBCONTR"), perc = "all")


# tabs <-  ct2013acm %>%
#   tabw(PR0, cah, EMP, wt = pondcal, show_na = FALSE, cleannames = TRUE, perc = "row",
#        sup_rows = c("RWDEMacm", "NBCONTR"), sup_cols = c("RWDEMacm", "NBCONTR"), another_total = TRUE, sup_contrib = TRUE)




# Mettre en forme la sortie Excel d'un tableau :
# BUGS :                  -
# Possibilites d'ajouts : - variance, pvalue, etc. a droite et pas dessous.
#                         - only_one_sheet ne fonctionne pas du tout avec des variables differentes en col
#                         - utiliser le format table de Excel ?

#' Crosstabs : Excel output with conditionnal formatting
#'
#' @param tabs Table(s) created with \code{\link{tab}}. Strictly speaking, it
#' can be an object of class \code{\link{single_tab}}, several of them
#' gathered in a \code{\link{tab}}, or a list code{\link{tab}}.
#' @param path,replace,open The name, and possibly the path, of the Excel file
#' to create (without the .xlsx extension). Default path to working directory.
#' Use \code{replace = TRUE} to overwrite existing files. Use \code{open = TRUE}
#' if you don't want to automatically open the tables in Excel (or another
#' software associated with .xlsx files).
#' @param colnames_rotation Rotate the names of columns to an angle (in degrees).
#' @param digits_perc Number of digits to print for percentages.
#' @param digits_no_perc Number of digits to print for counts.
#' @param digits_quanti Number of digits to print for quantitative variables.
#' @param only_one_sheet When \code{TRUE}, all tables are printed on the
#'  same sheet.
#' @param color Color cells, based on their deviation from the mean, using
#' Excel conditional formatting. In rows and cols, attractions /
#' over-represented levels are colored in shades of green,
#' and aversions / under-represented levels are colored in shades of orange.
#' \itemize{
#'    \item \code{"auto"} : tabs with \code{"row"} or \code{"col"} percentages
#'    are colored based on deviations from margins,
#'    other tabs on contributions to variance
#'    \item \code{"contrib"} : for all tabs, color intensity is based on the
#'    relative contribution of cells to variance (Chi2 metric)
#'    \item \code{"no"} : no conditional formatting
#'    }
#' @param hide_near_zero By default, values printed as 0 are automatically
#' colored in light gray in Excel, to help to focus on other ones. Use doubles
#' to set the value under which numbers are colored in gray.
#' \code{hide_near_zero = Inf} if you don't want to use this feature.
#' @param compact Create the more compact table possible by removing
#' unnecessary rows (Chi2, unweighted counts, \code{sup_rows}). Useful if there
#' is \code{var3}/\code{tab_var}.
#' @param perc_color_breaks The values to use to color deviations from the mean,
#' when the table shows \code{"row"} or \code{"col"} percentages and
#' \code{color = "auto"}. They must be numbers between - 1 and 1
#' (i.e. -100% and +100%).
#' @param cell_contrib_color_coeffs The values to use to color cell
#' contributions to variance : \code{1} means above/under the average
#' contribution of cells to variance, \code{10} means 10 times the average
#' contribution. Must be positive numbers for
#' attractions (green), negative numbers for repulsions (orange).
#'
#' @return An Excel file with the tables as a side-effect. Invisibly returns
#' \code{tabs}.
#' @export
#'
#' @examples
#' \dontrun{
#' forcats::gss_cat %>%
#' tabw(marital, race, perc = "row") %>%
#'   tab_xl()
#'
#' forcats::gss_cat %>%
#'   tabw(marital, race) %>%
#'   tab_xl()
#'   }
tab_xl <-
  function(tabs, path = "Tabs\\Tab", replace = FALSE, open = rlang::is_interactive(),
           colnames_rotation = 45,
           digits_perc = 0, digits_no_perc = 0, digits_quanti = 0, only_one_sheet = FALSE,
           color = c("auto", "contrib", "no"),
           hide_near_zero = c("auto", 0.0049, Inf),
           compact = FALSE,
           #no_contribs = FALSE, no_sup = FALSE, no_another_tot = FALSE, no_sup = FALSE, #title
           perc_color_breaks = c("+0.05","+0.10","+0.15","+0.25","*2","+0.35",
                                 "-0.05","-0.10","-0.15","-0.25",     "-0.35"),
           cell_contrib_color_coeffs = c(0.5, 1, 2, 5, 10, -0.5, -1,-2, -5, -10) #c(1, 2, 5, -1,-2, -5)
  ) {

    if (hide_near_zero[1] == "auto") {
      max_zero <- max((digits_perc + 2) * sign(digits_perc + 2),
                      digits_no_perc * sign(digits_no_perc))
      hide_near_zero <- as.double(stringr::str_c("0.",
                                                 stringr::str_c(rep(0, max_zero), collapse = ""),
                                                 "49") )
    }

    whole_is_unique_table <- !is.null(purrr::pluck(tabs, purrr::attr_getter("is_unique_table")))

    if ("tab_df" %in% class(tabs) )  tabs <- tab_draw(tabs)

    if ("tab" %in% class(tabs)){
      tabs <- list(tabs)

    } else if (whole_is_unique_table) { #"single_tab" %in% class(tabs)
      tabs <- list(purrr::pluck(tabs, purrr::attr_getter("tab")))

    } else  {
      where_unique_table <- purrr::map_lgl(tabs, ~ !is.null(purrr::pluck(., purrr::attr_getter("is_unique_table"))))

      if (any(where_unique_table)) {
        tabs[where_unique_table] <- tabs[where_unique_table] %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("tab")))
      }

      where_tab <- purrr::map_lgl(tabs, ~ "tab" %in% class(.) )
      if (any(! where_tab)) {
        where_depth2 <- tabs[!where_tab] %>%
          purrr::map_depth(2, ~ "tab" %in% class(.) ) %>%
          purrr::map(~purrr::flatten_lgl(.) %>% all()) %>% purrr::flatten_lgl()
        if (any(! where_depth2)) stop("some elements are not tabs, or buried too deep in a list (level 3 or more)")
        tabs <- list(tabs[where_tab]) %>% append(tabs[!where_tab]) %>% purrr::flatten()
      }
    }


    #Try with :
    #purrr::map(1:max(1, purrr::vec_depth(sup_vars_tabs) - 2), ~ purrr::map_depth(sup_vars_tabs, ., ~ "tab" %in% class(.))) %>% purrr::transpose()

    #Will miss a any() somewhere
    #vars_depth <- purrr::map(1:max(1, purrr::vec_depth(sup_vars_tabs) - 2), ~ purrr::map_depth(sup_vars_tabs, ., ~ "tab" %in% class(.))) %>% purrr::transpose() %>%
    #  purrr::map(purrr::flatten) %>% purrr::map(purrr::flatten_lgl) %>% purrr::map_int(which)

    tabsbase <- tabs
    tabs <- tabs %>% purrr::map(~`attr<-`(., "args", NULL) %>% `attr<-`("wtable", NULL))
    empty_tabs <- tabs %>% purrr::map(~ purrr::list_along(.)) %>% purrr::map_depth(2, ~ single_tab())

    #Remove sup columns and row if they were printed in the main tables
    print_sup_cols <- tabsbase %>% purrr::map(~ purrr::map_lgl(., ~ purrr::pluck(., purrr::attr_getter("print_sup"))[1]) )
    print_sup_rows <- tabsbase %>% purrr::map(~ purrr::map_lgl(., ~ purrr::pluck(., purrr::attr_getter("print_sup"))[2]) )
    if (any(purrr::flatten_lgl(print_sup_cols))) tabs  %<>%
      purrr::map2(print_sup_cols, ~ purrr::map_if(.x, .y, ~ dplyr::select(., 1:(tidyselect::any_of(">>>") -1) ) ))
    if (any(purrr::flatten_lgl(print_sup_rows))) tabs %<>%
      purrr::map2(print_sup_rows, ~ purrr::map_if(.x, .y, ~ dplyr::slice(., -which(stringr::str_detect(dplyr::pull(., 1), "^--")):-nrow(.) ) ))

    #Put long titles or compact titles
    original_data_name <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$original_data_name)

    new_titles <- tabsbase %>%
      purrr::map(~ stringr::str_c(purrr::pluck(., purrr::attr_getter("args"))$original_data_name, "\n", names(.)))
    if (compact == FALSE) {
      tabs %<>% purrr::map2(purrr::map(new_titles, ~ stringr::str_replace(., " : ", "\n")),
                            ~ purrr::map2(.x, .y, ~ dplyr::rename_at(.x, 1, function(var) .y)))
    } else {
      tabs %<>% purrr::map2(purrr::map(new_titles, ~ stringr::str_remove(.[1], " :[^:]*$")),
                            ~ purrr::map_if(.x, 1:length(.x) == 1,
                                            function(.dat) dplyr::rename_at(.dat, 1, function(var) .y),
                                            .else = function(.dat) dplyr::rename_at(.dat, 1, ~ " ")))
    }

    #Needed to know which totals to print anyway to calculate perc colors
    tabs_percentages       <- tabsbase %>%
      purrr::map(~ purrr::map_chr(., ~ purrr::pluck(., purrr::attr_getter("perc"))) ) %>%
      purrr::flatten_chr()

    #Transformations which rely on informations into the main tabs :
    #(when attribute is not found, set to default)
    wtables             <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("wtable")))
    col_var_sort        <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("col_var_sort"))) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ list("no"))

    keep_unused_levels  <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$keep_unused_levels) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ 30)
    multicols           <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$multicols) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ FALSE) %>% purrr::flatten_lgl()
    multirows           <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$multirows) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ FALSE)  %>% purrr::flatten_lgl()
    sup_cols            <- wtables %>% purrr::map_lgl(~ dplyr::filter(., .zone == "sup_cols") %>% nrow() > 0) & ! multicols
    sup_rows            <- wtables %>% purrr::map_lgl(~ dplyr::filter(., .zone == "sup_rows") %>% nrow() > 0) & compact == FALSE & ! multirows
    sup_cols_num        <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$sup_cols_num)
    sup_rows_num        <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$sup_rows_num)
    multicols_quanti_var<- tabs %>%
      purrr::map2(multicols,
                  ~ purrr::map_if(.x, rep(.y, length(.x)),
                                             ~ which(purrr::map_lgl(., ~ is_decimal(.))),
                                  .else = ~ integer())) %>%
      purrr::flatten()
    multirows_quanti_var<- empty_tabs %>% purrr::map_depth(2, ~ integer())
    multirows_quanti_var[multirows] %<>%
      purrr::map2(purrr::map(sup_rows_num[multirows], ~ which(.) + 1),
                  ~ purrr::map(.x, function(.tab) .y))
    multirows_quanti_var %<>% purrr::flatten()

    totals              <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$totals            ) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ c("row", "col"))
    totaltab            <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$totaltab          ) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ "table")
    digits              <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$digits            ) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ 0)
    sup_contrib_tab    <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$sup_contrib       ) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ FALSE)
    minimum_headcount   <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$minimum_headcount ) %>%
      purrr::map_if(purrr::map_lgl(., rlang::is_null), ~ 30)

    #confidence_intervals<- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("args"))$confidence_intervals)

    varnames            <- wtables %>%
      purrr::map(~ c(names(.)[3], names(.)[4], names(.)[2])) %>%
      purrr::map(~ dplyr::if_else(stringr::str_detect(., "^no_var"), "", .))

    #Put defaults if not found ?
    result_base_var     <- tabsbase %>%
      purrr::map(~ purrr::pluck(., purrr::attr_getter("result_var"))[1])
    result_sup_text_var <- tabsbase %>%
      purrr::map(~ purrr::pluck(., purrr::attr_getter("result_var"))[2])
    result_sup_num_var  <- tabsbase %>%
      purrr::map(~ purrr::pluck(., purrr::attr_getter("result_var"))[3])

    newsheet <- tabsbase %>%
      purrr::map(~ 1:length(.) == 1) %>%
      purrr::flatten_lgl()
    if (only_one_sheet == TRUE) newsheet <- 1:length(newsheet) == 1
    #tabs_confidence_intervals <- empty_tabs %>% purrr::map2(confidence_intervals, ~ purrr::map_lgl(.x, function(var) .y)) %>% purrr::flatten_lgl()
    sup_contrib               <- empty_tabs %>%
      purrr::map2(sup_contrib_tab, ~ purrr::map_lgl(.x, function(var) .y)) %>%
      purrr::flatten_lgl() & compact == FALSE
    varnames                  <- empty_tabs %>%
      purrr::map2(varnames, ~ purrr::map(.x, function(var) .y)) %>%
      purrr::flatten()
    totals_tabs_str           <- empty_tabs %>%
      purrr::map2(totals, ~ purrr::map(.x, function(var) .y))
    totals_tabs               <- totals_tabs_str %>%
      purrr::flatten()
    multicols_tabs            <- empty_tabs %>%
      purrr::map2(multicols, ~ purrr::map(.x, function(var) .y)) %>%
      purrr::flatten() %>%
      purrr::flatten_lgl()
    multirows_tabs            <- empty_tabs %>%
      purrr::map2(multirows, ~ purrr::map(.x, function(var) .y)) %>%
      purrr::flatten() %>%
      purrr::flatten_lgl()

    # sup_cols2 <- empty_tabs %>% purrr::map2(sup_cols, ~ purrr::map_lgl(.x, function(var) .y)) %>% purrr::flatten_lgl()
    # sup_rows2 <- empty_tabs %>% purrr::map2(sup_rows, ~ purrr::map_lgl(.x, function(var) .y)) %>% purrr::flatten_lgl()

    # needed_totrows <- color[1] == "auto" & purrr::map_lgl(totals, ~ ! "row" %in% .) & (purrr::map_lgl(tabs_percentages_first, ~ "row" %in% .) | sup_cols)
    # if (any(needed_totrows)) {
    #   warning("total rows were added to calculate percentages colors")
    #   totals  <- totals %>% purrr::map_if(needed_totrows, ~ append(., "row") %>% purrr::discard(. == "no"))
    # }
    # needed_totcols <- color[1] == "auto" & purrr::map_lgl(totals, ~ ! "col" %in% .) & (purrr::map_lgl(tabs_percentages_first, ~ "col" %in% .) | sup_rows)
    # if (any(needed_totcols)) {
    #   warning("total columns were added to calculate percentages colors")
    #   totals  <- totals %>% purrr::map_if(needed_totcols, ~ append(., "col") %>% purrr::discard(. == "no"))
    # }



    #Print the summary tab with the Chi2 probs of all tabs
    pvalue_Chi2_print   <- tabsbase %>% purrr::map(~ purrr::pluck(., purrr::attr_getter("pvalue_Chi2")))
    if (all(purrr::map_lgl(pvalue_Chi2_print, ~ ! rlang::is_null(.)))) {
      pChi2 <- purrr::map_lgl(pvalue_Chi2_print, ~ nrow(.) > 1)
      pvalue_Chi2_print[pChi2] <- pvalue_Chi2_print[pChi2] %>%
        purrr::map2(purrr::map(tabsbase, ~names(.)[1] %>% stringr::str_remove(" :[^:]*$"))[pChi2],
                    ~ dplyr::bind_rows(.x[1,], .x) %>%  dplyr::mutate_at(1, function(.var) ifelse(dplyr::row_number() == 1, stringr::str_c(.y, " :"),
                                                                                                  stringr::str_c("  ", .var))) %>% #There were unbreakable space ??
                      dplyr::mutate_at(-1, function(.var) ifelse(dplyr::row_number() == 1, NA_real_, .var)) %>%
                      dplyr::mutate_at(2, ~ decimal(., 0)) %>% dplyr::mutate_at(3, ~ decimal(., 5)) %>% dplyr::mutate_at(4, ~ pct(., 6)) )
      pvalue_Chi2_print %<>% dplyr::bind_rows()
      pvalue_Chi2_print %>% pillar::colonnade() %>% print()
    }

    only_total_columns <- totals %>%
      purrr::map(~ append(., "only_totcol") %>%
                   purrr::discard(. == "col"))

    insufficient_headcount_var1 <-
      purrr::pmap(list(wtables, only_total_columns, totaltab, keep_unused_levels, minimum_headcount), #col_var_sort,
                  ~ tab_draw(..1, n, tot = ..2, totaltab = ..3, keep_unused_levels = ..4) %>% #perc = "no", col_var_sort = ..5
                    purrr::map_if(purrr::map_lgl(., ~ ncol(.) >= 2),
                                  function(.tab) which(dplyr::pull(.tab, 2) < ..5 ) + 1L,
                                  .else = ~ integer()) ) %>%
      purrr::flatten()    #INSUFF : NULLs are now integer(0) ?

    only_total_rows <- totals %>%
      purrr::map(~ append(., "only_totrow") %>%
                   purrr::discard(. == "row"))
    insufficient_headcount_var2 <-
      purrr::pmap(list(wtables, only_total_rows, totaltab, keep_unused_levels,  minimum_headcount), #col_var_sort,
                  ~ tab_draw(..1, n, tot = ..2, totaltab = ..3, keep_unused_levels = ..4, reverse_row_col = TRUE) %>% #perc = "no", col_var_sort = ..5,
                    purrr::map_if(purrr::map_lgl(., ~ ncol(.) >= 2),
                                  function(.tab) which(dplyr::pull(.tab, 2) < ..5) + 1L,
                                  .else = ~ integer())  ) %>%
      purrr::flatten()


    #Sup contribs
    if (color[1] %in% c("contrib", "auto") | any(purrr::flatten_lgl(sup_contrib_tab)) ) {
      # contrib_tabs <-
      #   purrr::pmap(list(wtables, totals, totaltab, keep_unused_levels, col_var_sort),
      #        ~ tab_draw(..1, contrib, tot = ..2, totaltab = ..3, keep_unused_levels = ..4) ) %>% #perc = "no", col_var_sort = ..5
      #   purrr::map_depth(2, ~ dplyr::mutate(., dplyr::across(where(~ is_pct(.) | is_decimal(.)), as.double))) %>% purrr::flatten()
      # contrib_matrixes <-
      #   purrr::pmap(list(contrib_tabs,
      #             purrr::map_if(contrib_tabs, purrr::map_lgl(totals_tabs, ~ "col" %in% .), ~ ncol(.), .else = ~ 0L),
      #             purrr::map_if(contrib_tabs, purrr::map_lgl(totals_tabs, ~ "row" %in% .), ~ nrow(.), .else = ~ nrow(.) + 1) ),
      #        ~ dplyr::select(..1, -1, - colnames(.)[..2]) %>% dplyr::slice(-..3) )  contrib_tabs <-
      contrib_condition <- wtables %>% purrr::map(~ dplyr::filter(., .zone == "base") ) %>%
        purrr::map_lgl(~ ncol(.) != 0 & nrow(.) != 0)
      contrib_tabs <-
        pmap_if(list(wtables, totaltab, keep_unused_levels), #col_var_sort
                contrib_condition,
                ~ tab_draw(..1, contrib, tot = c("row", "col"), totaltab = ..2, keep_unused_levels = ..3)) #perc = "no", col_var_sort = ..4
      contrib_tabs[!contrib_condition] <- empty_tabs[!contrib_condition]

      contrib_tabs <- contrib_tabs %>%
        purrr::map_depth(2, ~ dplyr::mutate(., dplyr::across(where(~ is_pct(.) | is_decimal(.)), as.double))) %>%
        purrr::flatten()
      contrib_condition_tabs <- purrr::map_lgl(contrib_tabs, ~ ncol(.) != 0 & nrow(.) != 0)
      contrib_tabs_totcol <- contrib_tabs %>% purrr::map_if(contrib_condition_tabs, ~ dplyr::select(., Total))
      contrib_tabs_totrow <- contrib_tabs %>% purrr::map_if(contrib_condition_tabs, ~ dplyr::slice(., nrow(.)))
      contrib_matrixes <- pmap_if(list(contrib_tabs,
                                              purrr::map(contrib_tabs, ~ nrow(.)) ),
                                  contrib_condition_tabs,
                                         ~ dplyr::select(..1, -Total, -1) %>% dplyr::slice(-..2) )


      contrib_tabs <- contrib_tabs %>%
        purrr::map_if(purrr::map2_lgl(totals_tabs, contrib_condition_tabs, ~ ! "row" %in% .x & .y),  ~  dplyr::slice(., -nrow(.))) %>%
        purrr::map_if(purrr::map2_lgl(totals_tabs, contrib_condition_tabs, ~ ! "col" %in% .x & .y),  ~ dplyr::select(., -Total))

      tabs_mean_contrib <- wtables %>%
        purrr::map_if(contrib_condition, ~ dplyr::group_by(., !!rlang::sym(names(.)[2])) %>%
                        dplyr::summarise(ctr = dplyr::first(ctr_mean), .groups = "drop") %>%
                      dplyr::pull(ctr)    ) %>% purrr::flatten()
    }
    #All operations on contrib_tabs must set condition before : they are not always created


    #Supplementary cols and rows
    if (any(sup_cols)) {
      sup_cols_tabs <-
        pmap_if(list(wtables, result_sup_text_var, result_sup_num_var, totals, totaltab, keep_unused_levels), #col_var_sort
                       sup_cols,
                       ~ tab_draw(..1, result_text = !!rlang::sym(..2), result_num = !!rlang::sym(..3), col_var = .SUP, zone = "sup_cols", tot = ..4,
                                 totaltab = ..5, keep_unused_levels = ..6)) %>% #perc = "col", col_var_sort = ..7
        purrr::map_if(!sup_cols, ~ "no")
      empty <- ! purrr::map_lgl(sup_cols_tabs, ~ "list" %in% class(.))
      sup_cols_tabs[empty] <- empty_tabs[empty]
      sup_cols_tabs %<>%
        purrr::map_if(sup_cols, ~ purrr::map(., ~ dplyr::mutate(., dplyr::across(where(~ is_pct(.) | is_decimal(.)), as.double)))) %>%
        purrr::flatten()

      sup_cols_quanti_var <- wtables %>%
        purrr::map_if(sup_cols, ~ dplyr::filter(., .zone == "sup_cols") %>% dplyr::group_by(.SUP_NAME) %>%
                        dplyr::summarise(type = dplyr::first(.TYPE), .groups = "drop") %>% dplyr::pull(type) == "num",
                      .else = ~ integer() )
      sup_cols_quali_var  <- sup_cols_quanti_var %>% purrr::map_if(sup_cols, ~ which(! .)) %>%
        purrr::map2(empty_tabs, ~ purrr::map(.y, function(var) .x)) %>% purrr::flatten()
      sup_cols_quanti_var <- sup_cols_quanti_var %>% purrr::map_if(sup_cols, ~ which(.))  %>%
        purrr::map2(empty_tabs, ~ purrr::map(.y, function(var) .x)) %>% purrr::flatten()
    } else {
      sup_cols_quali_var <- sup_cols_quanti_var <- purrr::map(empty_tabs, ~ purrr::map(., ~ integer())) %>% purrr::flatten()
    }


    if (any(sup_rows)) {
      sup_rows_tabs <-
        pmap_if(list(wtables, result_sup_text_var, result_sup_num_var, totals, keep_unused_levels), #col_var_sort
                       sup_rows,
                       ~ tab_draw(..1, result_text = !!rlang::sym(..2), result_num = !!rlang::sym(..3), row_var = .SUP, zone = "sup_rows", tot = ..4,  #subtext ?
                                 totaltab = "table", keep_unused_levels = ..5)) #, perc = "row"
      empty <- ! purrr::map_lgl(sup_rows_tabs, ~ "list" %in% class(.))
      sup_rows_tabs[empty] <- empty_tabs[empty]
      sup_rows_tabs %<>%
        purrr::map_if(sup_rows, ~ purrr::map(., ~ dplyr::mutate(., dplyr::across(where(~ is_pct(.) | is_decimal(.)), as.double)))) %>%
        purrr::flatten()

      tab_draw(wtables[[1]], result_text = !!rlang::sym(result_sup_text_var[[1]]), result_num = !!rlang::sym(result_sup_num_var[[1]]),
              row_var = .SUP, zone = "sup_rows", tot = totals[[1]],  #subtext ?
              totaltab = "table", keep_unused_levels = keep_unused_levels[[1]]) #, perc = "row"

      #purrr::map(wtables, ~dplyr::mutate_at(., dplyr::vars(tidyselect::any_of("res")), ~ as_pct(.))) #convert quanti to pct, otherwise wrond bind

      sup_rows_quanti_var <- wtables %>%
        purrr::map_if(sup_rows, ~ dplyr::filter(., .zone == "sup_rows") %>% dplyr::group_by(.SUP_NAME) %>%
                        dplyr::summarise(type = dplyr::first(.TYPE), .groups = "drop") %>% dplyr::pull(type) == "num",
                      .else = ~ integer() )
      sup_rows_quali_var  <- sup_rows_quanti_var %>% purrr::map_if(sup_rows, ~ which(! .)) %>%
        purrr::map2(empty_tabs, ~ purrr::map(.y, function(var) .x)) %>% purrr::flatten()
      sup_rows_quanti_var <- sup_rows_quanti_var %>% purrr::map_if(sup_rows, ~ which(.))  %>%
        purrr::map2(empty_tabs, ~ purrr::map(.y, function(var) .x)) %>% purrr::flatten()

    } else {
      sup_rows_quali_var <- sup_rows_quanti_var <- purrr::map(empty_tabs, ~ purrr::map(., ~ integer())) %>% purrr::flatten()
    }

    #Another totals :
    an_totcol_condition <- purrr::map_lgl(wtables, ~ "an_totcol" %in% names(.))
    if ( any(an_totcol_condition) ) {
      another_totcol_tabs <-
        pmap_if(list(wtables, only_total_columns, totaltab, keep_unused_levels), #col_var_sort
                       an_totcol_condition,
                       ~ tab_draw(..1, an_totcol, tot = ..2, totaltab = ..3, keep_unused_levels = ..4) %>% #perc = "col", col_var_sort = ..5
                         purrr::map(~ dplyr::select(., -1)))
      empty <- ! purrr::map_lgl(another_totcol_tabs, ~ "list" %in% class(.))
      another_totcol_tabs[empty] <- empty_tabs[empty]
      another_totcol_tabs %<>%
        purrr::map_if(an_totcol_condition, ~ purrr::map(., ~ dplyr::mutate(., dplyr::across(where(~ is_pct(.) | is_decimal(.)), as.double)))) %>%
        purrr::flatten()
    }

    an_totrow_condition <- purrr::map_lgl(wtables, ~ "an_totrow" %in% names(.)) & compact == FALSE
    if ( any(an_totrow_condition) ) {
      another_totrow_tabs <-
        pmap_if(list(wtables, only_total_rows, totaltab, keep_unused_levels),
                       an_totrow_condition,
                       ~ tab_draw(..1, an_totrow, tot = ..2, totaltab = ..3, keep_unused_levels = ..4)) #, perc = "row"
      empty <- ! purrr::map_lgl(another_totrow_tabs, ~ "list" %in% class(.))
      another_totrow_tabs[empty] <- empty_tabs[empty]
      another_totrow_tabs %<>%
        purrr::map_if(an_totrow_condition, ~ purrr::map(., ~ dplyr::mutate(., dplyr::across(where(~ is_pct(.) | is_decimal(.)), as.double)))) %>%
        purrr::flatten()
    }


    #Transformations which rely on informations into single_tabs :
    #tabs_percentages       <- tabsbase %>% purrr::map(~ purrr::map_chr(., ~ purrr::pluck(., purrr::attr_getter("perc"))) ) %>% purrr::flatten_chr()
    #Calculate here and not in attributes ? ----
    tabs_total_table       <- tabsbase %>%
      purrr::map(~ purrr::map(., ~ purrr::pluck(., purrr::attr_getter("total_table"))) ) %>%
      purrr::flatten() #tabs_total_tables
    tabs_subtext           <- tabsbase %>%
      purrr::map(~ purrr::map(., ~ purrr::pluck(., purrr::attr_getter("subtext"))) ) %>%
      purrr::flatten() # NA_character
    tabs_general_total_row <- tabs_total_table %>%
      purrr::map2_lgl(purrr::map_depth(tabs, 2, ~ nrow(.) == 1) %>%
                        purrr::flatten(), ~ purrr::map2_lgl(.x, .y, ~ .x & .y))

    pvalue_Chi2          <- tabsbase    %>%
      purrr::map_depth(2, ~ purrr::pluck(., purrr::attr_getter("pvalue_Chi2")) ) %>%
      purrr::flatten()

    if (compact == FALSE) {
    Vnuage               <- pvalue_Chi2 %>%
      purrr::map_if(purrr::map_lgl(., ~ ! rlang::is_null(.)),
                    ~ dplyr::pull(., 3) %>% as.double(),
                    .else = ~ NA_real_)
    pvalue               <- pvalue_Chi2 %>%
      purrr::map_if(purrr::map_lgl(., ~ ! rlang::is_null(.)),
                    ~ dplyr::pull(., 4) %>% as.double(),
                    .else = ~ NA_real_)
    unweighted_headcount <- pvalue_Chi2 %>%
      purrr::map_if(purrr::map_lgl(., ~ ! rlang::is_null(.)),
                    ~ dplyr::pull(., 2) %>% as.integer(),
                    .else = ~ NA_real_)
    } else { if (compact == TRUE)
      pvalue <- unweighted_headcount <- Vnuage <-
        pvalue_Chi2 %>% purrr::map(~ NA_real_)
    }

    #Unknown for now :
    # tabs_digits        <- tabsbase %>% #STILL TO IMPLEMENT ----
    #   purrr::map(~ purrr::pluck(., purrr::attr_getter("digits")))



    #FOR EACH TABLE :
    # Cols and rows to be formated as percentages/numbers, with conditional formatting
    total_rows <- purrr::map2(totals_tabs, purrr::map_depth(tabs, 2, ~ nrow(.) + 1) %>%
                                purrr::flatten(),
                              ~ ifelse("row" %in% .x, .y, integer()))
    total_cols <- purrr::map2(totals_tabs, purrr::map_depth(tabs, 2, ~ ncol(.) ) %>%
                                purrr::flatten(),
                              ~ ifelse("col" %in% .x, .y, integer()))
    totr <- total_rows %>% purrr::map_lgl(~ !is.na(.))
    totc <- total_cols %>% purrr::map_lgl(~ !is.na(.))

    if (color[1] %in% c("contrib", "auto") | any(purrr::flatten_lgl(sup_contrib_tab)) ) {
      contrib_tabs_totcol <- purrr::map_if(contrib_tabs_totcol, !totr, ~ dplyr::slice(., - nrow(.)))
      contrib_tabs_totrow <- purrr::map_if(contrib_tabs_totrow, !totc, ~ dplyr::select(., - Total))
    }

    quali_var_rows <- tabs %>%
      purrr::map2(totals_tabs_str, ~ purrr::map_if(.x, purrr::map_lgl(.y, ~ "row" %in% .),
                                                   ~ 2:(nrow(.)),
                                                   .else = ~ 2:(nrow(.) + 1) ) ) %>% purrr::flatten()
    if (any(multirows_tabs)) quali_var_rows[multirows_tabs] %<>%
      map2_if(multirows_quanti_var[multirows_tabs],
                  purrr::map_lgl(multirows_quanti_var[multirows_tabs], ~ length(.) != 0),
                  ~ purrr::discard(.x, .x == .y))


    quali_var_cols <- tabs %>%
      purrr::map2(totals_tabs_str, ~ purrr::map_if(.x, purrr::map_lgl(.y, ~ "col" %in% .),
                                                   ~ 2:(ncol(.) - 1),
                                                   .else = ~ 2:(ncol(.)) ) ) %>% purrr::flatten()
    if (any(multicols_tabs)) quali_var_cols[multicols_tabs] %<>%
      map2_if(multicols_quanti_var[multicols_tabs],
                     purrr::map_lgl(multicols_quanti_var[multicols_tabs], ~ length(.) != 0),
                     ~ purrr::discard(.x, .x == .y))


    #purrr::Flatten the list of tabs and coerce pct and decimal to double.
    tabs %<>% purrr::map_depth(2, ~ dplyr::mutate(., dplyr::across(where(~ is_pct(.) | is_decimal(.)), as.double))) %>% purrr::flatten()

    if (color[1] == "contrib" ) {
      col_perc_var_rows <- quali_var_rows %>% purrr::map(~ integer(0))
      col_perc_var_cols <- quali_var_cols %>% purrr::map(~ integer(0))
      row_perc_var_rows <- quali_var_rows %>% purrr::map(~ integer(0))
      row_perc_var_cols <- quali_var_cols %>% purrr::map(~ integer(0))
      cell_contrib_condition <- # tabs_confidence_intervals == FALSE &
        tabs_percentages %in% c("all", "no", "row", "col") & ! multicols_tabs & ! multirows_tabs
      cell_contrib_var_rows <- quali_var_rows %>% purrr::map_if(cell_contrib_condition, ~ ., .else = ~ integer(0))
      cell_contrib_var_cols <- quali_var_cols %>% purrr::map_if(cell_contrib_condition, ~ ., .else = ~ integer(0))

    } else if (color[1] == "auto") {
      col_perc_condition <- tabs_percentages == "col" & #tabs_confidence_intervals == FALSE &
        purrr::map_lgl(totals_tabs, ~ "col" %in% .)
      col_perc_var_rows <- quali_var_rows %>% purrr::map_if(!col_perc_condition, ~ integer(0))
      col_perc_var_cols <- quali_var_cols %>% purrr::map_if(!col_perc_condition, ~ integer(0))

      row_perc_condition <- tabs_percentages == "row" & #tabs_confidence_intervals == FALSE &
        purrr::map_lgl(totals_tabs, ~ "row" %in% .)
      row_perc_var_rows <- quali_var_rows %>% purrr::map_if(!row_perc_condition, ~ integer(0))
      row_perc_var_cols <- quali_var_cols %>% purrr::map_if(!row_perc_condition, ~ integer(0))

      cell_contrib_condition <- tabs_percentages %in% c("all", "no") & #tabs_confidence_intervals == FALSE &
        ! multicols_tabs & ! multirows_tabs
      cell_contrib_var_rows <- quali_var_rows %>% purrr::map_if(!cell_contrib_condition, ~ integer(0))
      cell_contrib_var_cols <- quali_var_cols %>% purrr::map_if(!cell_contrib_condition, ~ integer(0))
    } else { #color = no
      col_perc_var_rows  <- col_perc_var_cols <- row_perc_var_rows <-
        row_perc_var_cols <- cell_contrib_var_rows <- cell_contrib_var_cols <-
        purrr::map(quali_var_rows, ~ integer(0))
    }




    #Numbers of each supplementary elements
    if (any(sup_cols)) {
      sup_cols_var <- sup_cols_tabs %>% purrr::map_int(~ ifelse(length(.) != 0, ncol(.) - 1L, 0L))
    } else {
      sup_cols_var <- tabs %>% purrr::map_int(~ 0L)
    }
    if (any(sup_rows)) {
      sup_rows_var <- sup_rows_tabs %>% purrr::map_int(~ ifelse(length(.) != 0, nrow(.), 0L))
    } else {
      sup_rows_var <- tabs %>% purrr::map_int(~ 0L)
    }
    another_total_cols_var <- empty_tabs %>% purrr::map2(an_totcol_condition, ~ purrr::map_int(.x, function(var) ifelse(.y, 1L, 0L))) %>% purrr::flatten_int()
    another_total_rows_var <- empty_tabs %>% purrr::map2(an_totrow_condition, ~ purrr::map_int(.x, function(var) ifelse(.y, 1L, 0L))) %>% purrr::flatten_int()
    contrib_var <- sup_contrib %>% purrr::map_int(~ ifelse(., 1L, 0L))
    #Vnuage[is.na(Vnuage)] <- purrr::map(Vnuage[is.na(Vnuage)], ~ NULL)
    vari_row <- Vnuage %>%
      purrr::map_if(purrr::map_lgl(., ~ ! is.na(.)), ~ 1L, .else = ~ 0L) %>% purrr::flatten_int() #is.na OK ?
    # pvalue[is.na(pvalue)] <- purrr::map(pvalue[is.na(pvalue)], ~ NULL)
    pvalue_row <- pvalue %>%
      purrr::map_if(purrr::map_lgl(., ~ ! is.na(.)), ~ 1L, .else = ~ 0L) %>% purrr::flatten_int()
    unweighted_headcount_row <- unweighted_headcount %>%
      purrr::map_if(purrr::map_lgl(., ~ ! is.na(.)), ~ 1L, .else = ~ 0L) %>% purrr::flatten_int()
    subtext_condition <- tabs_subtext %>% purrr::map_lgl(~ ! is.na(.[1]) )
    subtext_rows <- tabs_subtext %>%
      purrr::map_if(subtext_condition, ~ length(.), .else = ~ 0L) %>% purrr::flatten_int()
    #contrib_tabs %>%
    #purrr::map_int(~ ifelse(!is.null(.), 1L, 0L)) # purrr::map_lgl(~ dplyr::if_else(!is.null(.), TRUE, FALSE))

    # tabs_subtext <- rep(NA, 7)
    # purrr::map_lgl(tabs_subtext, ~ ! is.na(.[1]))
    #
    # tabs_subtext %>%
    #   purrr::map_if(purrr::map_lgl(., ~ ! is.na(.[1])), ~ length(.), .else = ~ 0L) %>% purrr::flatten_int()

    #Position of each supplementary element (from ncol(tabs) or nrow(tabs))
    nbcol_tot       <- tabs %>% purrr::map_int(~ 2L)
    nbcol_sup       <- nbcol_tot + another_total_cols_var
    nbcol_ct        <- nbcol_sup + sup_cols_var
    nbcol_ct <- list(nbcol_ct, another_total_cols_var, sup_cols_var) %>%
      purrr::pmap(~ dplyr::if_else(..2 + ..3 >= 1, .x + 1L, .x)) %>% purrr::as_vector()

    nbrow_tot       <- tabs %>% purrr::map_int(~ 2L)
    nbrow_tot <- list(nbrow_tot, another_total_rows_var, sup_rows_var) %>%
      purrr::pmap(~ dplyr::if_else(..2 + ..3 >= 1, .x + 1L, .x)) %>% purrr::as_vector()
    nbrow_sup       <- nbrow_tot + another_total_rows_var
    nbrow_vari      <- nbrow_sup + sup_rows_var
    nbrow_pvalue    <- nbrow_vari + vari_row
    nbrow_uwh       <- nbrow_pvalue + pvalue_row
    nbrow_ct        <- nbrow_uwh + unweighted_headcount_row
    nbrow_sub       <- nbrow_ct + contrib_var
    # Add 2 empty lines if contrib is the last object
    nbrow_sub %<>% purrr::map_if(contrib_var == 1, ~ . + 2L) %>% purrr::flatten_int()
    nbrow_start_row <- nbrow_sub + subtext_rows - 1L

    # Add empty line if subtext is the last object
    nbrow_start_row %<>% purrr::map_if(subtext_rows >= 1, ~ . + 1L) %>% purrr::flatten_int()

    #Vector to purrr::map things on all sup cols/rows, or subtext rows (of type 0:1)
    nbcsup <- sup_cols_var %>% purrr::map(~ ifelse(. >= 1L, . - 1L, 0L)) %>% purrr::map(~ 0L:.)
    nbrsup <- sup_rows_var %>% purrr::map(~ ifelse(. >= 1L, . - 1L, 0L)) %>% purrr::map(~ 0L:.)
    nbrsubtext <- subtext_rows %>% purrr::map(~ ifelse(. >= 1L, . - 1L, 0L)) %>% purrr::map(~ 0L:.)


    #Sheet number, start row and new headers
    sheetnb <- newsheet %>%
      purrr::map_int(~ dplyr::if_else(., 1L, 0L)) %>% cumsum()
    start_row <- list(tabs, nbrow_start_row) %>%
      purrr::pmap_int(~ nrow(.x) + .y)
    start_row <- start_row[-length(start_row)] %>%
      purrr::accumulate2(newsheet[-1], .init = 0L, .f = function(prev, follow, newsheet)
        dplyr::if_else(newsheet, 0L, prev + follow))
    #It is in fact the first line - 1 :
    # for the counters to work without changing all the code,
    # the first table begins at start_row + 1 = 1, the second at start_row + 2, etc.
    w_nsheet_start <- which(purrr::as_vector(newsheet))
    w_nsheet_stop <- append(which(purrr::as_vector(newsheet))[-1] - 1, length(newsheet))
    tabs_on_same_sheet <- w_nsheet_start %>% purrr::map2(w_nsheet_stop, ~ .x:.y)
    newheaders <- tabs %>% purrr::map(~ colnames(.)[-1])
    newheaders <- list(newheaders[-length(newheaders)], newheaders[-1]) %>%
      purrr::pmap(~ all(dplyr::if_else(identical(.x, .y), FALSE, TRUE)))
    newheaders <- append(FALSE, newheaders)
    newheaders[w_nsheet_start] <- FALSE
    only_same_headers_on_sheet <- tabs_on_same_sheet %>%
      purrr::map(function (toss) all(purrr::map_lgl(newheaders[toss], ~ all(!.))))
    hd_remove <- tabs_on_same_sheet %>%
      purrr::map2(only_same_headers_on_sheet, ~ dplyr::case_when(.y ~ .x, TRUE ~ 0L)) %>%
      purrr::flatten_int()
    hd_remove <- dplyr::if_else(hd_remove == 0L, FALSE, TRUE)
    hd_remove[w_nsheet_start] <- FALSE

    sheet_titles <- w_nsheet_start %>% purrr::map(~ varnames[.]) %>%  purrr::flatten() %>%
      purrr::map_if(purrr::map_lgl(., ~ length(.) > 1), ~ stringr::str_c(.[1], " par ", .[2]), .else ~ .[1]) %>%
      purrr::flatten_chr()
    sheet_titles <- dplyr::if_else(duplicated(sheet_titles),
                                   stringr::str_c(sheet_titles, ".1"),
                                   sheet_titles)
    nb = 1
    while (length(unique(sheet_titles)) != length(sheet_titles)) {
      nb = nb + 1
      sheet_titles <- dplyr::if_else(duplicated(sheet_titles),
                                     stringr::str_c(stringr::str_remove(sheet_titles, "..$"), ".", nb),
                                     sheet_titles)
    }
    sheet_titles %<>% as.list()

    # tab_with_CI_on_sheet <-
    #   purrr::map(tabs_on_same_sheet, function(.x)
    #     purrr::flatten_lgl(tabs_confidence_intervals) %>% .[.x] %>% any()) %>%
    #   purrr::flatten_lgl()


    #Create sheets and place tabs :
    wb <- openxlsx::createWorkbook()
    sheet_titles %>% purrr::walk(~ openxlsx::addWorksheet(wb, .))
    purrr::pwalk(list(sheetnb, start_row, tabs),
                 ~ openxlsx::writeData(wb, sheet = ..1, ..3,
                                       startRow = ..2 + 1, startCol = 1))
    #On a sheet, if colnames are the same, just keep the first :
    purrr::pwalk(list(sheetnb[hd_remove], start_row[hd_remove],  tabs[hd_remove]),
                 function(.sheetnb, .start_row, .tabs)
                   openxlsx::deleteData(wb, sheet = .sheetnb, gridExpand = TRUE,
                                        rows = .start_row + 1,
                                        cols = 2:ncol(.tabs)))









    #Define formats ##################################################################
    if (colnames_rotation < 0 | colnames_rotation > 90) {
      stop("Text rotation must be between 0 and 90 degrees")
    }

    if (digits_perc == 0) {numformats = "0%"} else if (digits_perc > 0) {
      numformats = stringr::str_c("0.", stringr::str_c(rep("0", digits_perc), collapse = ""), "%")
    } else {stop("Digits for percentages can't be negative.")}

    if (digits_no_perc == 0) {
      numformats_no_perc = "#,##0"
    } else if (digits_no_perc > 0)  {
      numformats_no_perc <- "#,##0"
      stringr::str_c("#,##0.", stringr::str_c(rep("0", digits_no_perc), collapse = ""))
    } else {
      numformats_no_perc <-
        stringr::str_c("#,",  stringr::str_c(rep("#", 2 - -digits_no_perc %% 3), collapse = ""),
                       stringr::str_c(rep("0", 1 + -digits_no_perc %% 3), collapse = ""),
                       stringr::str_c(rep(",", -digits_no_perc %/% 3), collapse = ""))
    }

    if (digits_quanti == 0) {
      numformats_quanti = "#,##0"
    } else if (digits_quanti > 0) {
      numformats_quanti <-
        stringr::str_c("#,##0.", stringr::str_c(rep("0", digits_quanti),collapse = ""))
    } else {
      numformats_quanti <-
        stringr::str_c("#,",  stringr::str_c(rep("#", 2 - -digits_quanti %% 3), collapse = ""),
                       stringr::str_c(rep("0", 1 + -digits_quanti %% 3), collapse = ""),
                       stringr::str_c(rep(",", -digits_quanti %/% 3), collapse = ""))
    }


    openxlsx::modifyBaseFont(wb, fontSize = 11, fontName = "Verdana")

    if (colnames_rotation == 0) {
      base_style <-
        openxlsx::createStyle(halign = "right", valign = "top", wrapText = TRUE, numFmt = numformats)
      base_style_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc)
      base_style_quanti <-
        openxlsx::createStyle(halign = "right", valign = "top", wrapText = TRUE, numFmt = numformats_quanti)
      base_style_insufficient_headcount <-
        openxlsx::createStyle(halign = "right", valign = "top", wrapText = TRUE, numFmt = numformats,
                              fontColour = "#909090")
      base_style_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              fontColour = "#909090")
      headers <-
        openxlsx::createStyle(halign = "center", valign = "bottom", wrapText = TRUE,
                              textDecoration = "Bold", border = "TopBottom")
      headers_insufficient_headcount <-
        openxlsx::createStyle(halign = "center", valign = "bottom", wrapText = TRUE,
                              textDecoration = "Bold", border = "TopBottom",
                              fontColour = "#909090")
      total_column_style <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "LeftRight")
      total_column_no_perc <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "LeftRight")
      total_column_quanti <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats_quanti,
                              textDecoration = "Bold", border = "LeftRight")
      total_column_insufficient_headcount <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "LeftRight",
                              fontColour = "#909090")
      total_column_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "LeftRight",
                              fontColour = "#909090")
      topright_cell <-
        openxlsx::createStyle(halign = "left", valign = "bottom", wrapText = TRUE,
                              textDecoration = "Bold", border = "BottomLeftRight")
      total_row_style <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "TopBottom")

      total_row_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "TopBottom")
      total_row_quanti <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_quanti,
                              textDecoration = "Bold", border = "TopBottom")
      total_row_insufficient_headcount <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "TopBottom",
                              fontColour = "#909090")
      total_row_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "TopBottom",
                              fontColour = "#909090")
      bottomright_cell <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "TopBottomLeftRight")
      bottomright_cell_no_perc <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "TopBottomLeftRight")
      phantom_headers <-
        openxlsx::createStyle(halign = "left", valign = "bottom", wrapText = TRUE,
                              textDecoration = "Bold",
                              border = "Bottom")

      last_row_no_total <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = "Bottom", wrapText = TRUE)
      last_row_no_total_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = "Bottom", wrapText = TRUE)
      last_row_no_total_quanti <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_quanti,
                              border = "Bottom")
      last_row_no_total_insufficient_headcount <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = "Bottom", fontColour = "#909090", wrapText = TRUE)
      last_row_no_total_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = "Bottom",fontColour = "#909090", wrapText = TRUE)
      last_row_no_total_total_column_insufficient_headcount <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = c("left", "right", "bottom"),
                              fontColour = "#909090")
      last_row_no_total_total_column_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = c("left", "right", "bottom"),
                              fontColour = "#909090")

      last_row_no_total_bottomright_cell <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats,
                              border = "TopBottomLeftRight", wrapText = TRUE)
      last_row_no_total_bottomright_cell_no_perc <-
        openxlsx::createStyle(halign = "left", valign = "top", numFmt = numformats_no_perc,
                              border = "TopBottomLeftRight", wrapText = TRUE)
      last_col_no_total_bottomright_cell <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = "TopBottomLeftRight", wrapText = TRUE)
      last_col_no_total_bottomright_cell_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = "TopBottomLeftRight", wrapText = TRUE)
      both_no_total_bottomright_cell <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = "BottomRight", wrapText = TRUE)
      both_no_total_bottomright_cell_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = "BottomRight", wrapText = TRUE)


    } else if (colnames_rotation > 0) {
      base_style <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = c("left", "right"), wrapText = TRUE)
      base_style_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = c("left", "right"))
      base_style_quanti <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_quanti,
                              border = c("left", "right"), wrapText = TRUE)
      base_style_insufficient_headcount <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = c("left", "right"), fontColour = "#909090")
      base_style_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = c("left", "right"), fontColour = "#909090")
      headers <-
        openxlsx::createStyle(halign = "left", valign = "bottom", wrapText = TRUE,
                              textDecoration = "Bold", textRotation = colnames_rotation,
                              border = c("bottom", "left", "right", "top"))
      headers_insufficient_headcount <-
        openxlsx::createStyle(halign = "left", valign = "bottom", wrapText = TRUE,
                              textDecoration = "Bold", textRotation = colnames_rotation,
                              border = c("bottom", "left", "right", "top"),
                              fontColour = "#909090")
      total_column_style <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "LeftRight")
      total_column_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "LeftRight")
      total_column_quanti <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_quanti,
                              textDecoration = "Bold", border = "LeftRight")
      total_column_insufficient_headcount <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "LeftRight",
                              fontColour = "#909090")
      total_column_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "LeftRight",
                              fontColour = "#909090")
      topright_cell <-
        openxlsx::createStyle(halign = "right", valign = "bottom", wrapText = TRUE,
                              textDecoration = "Bold", border = "TopBottomLeftRight")
      total_row_style <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "TopBottomLeftRight")
      total_row_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "TopBottomLeftRight")
      total_row_quanti <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_quanti,
                              textDecoration = "Bold", border = "TopBottomLeftRight")
      total_row_insufficient_headcount <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "TopBottomLeftRight",
                              fontColour = "#909090")
      total_row_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "TopBottomLeftRight",
                              fontColour = "#909090")
      bottomright_cell <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "TopBottomLeftRight")
      bottomright_cell_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "TopBottomLeftRight")
      phantom_headers <-
        openxlsx::createStyle(halign = "left", valign = "bottom", wrapText = TRUE,
                              textDecoration = "Bold", #textRotation = colnames_rotation,
                              border = "Bottom")

      last_row_no_total <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = c("left", "right", "bottom"), wrapText = TRUE)
      last_row_no_total_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = c("left", "right", "bottom"), wrapText = TRUE)
      last_row_no_total_quanti <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_quanti,
                              border = c("left", "right", "bottom"))
      last_row_no_total_insufficient_headcount <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = c("left", "right", "bottom"), fontColour = "#909090", wrapText = TRUE)
      last_row_no_total_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = c("left", "right", "bottom"),fontColour = "#909090", wrapText = TRUE)
      last_row_no_total_total_column_insufficient_headcount <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = c("left", "right", "bottom"),
                              fontColour = "#909090")
      last_row_no_total_total_column_insufficient_headcount_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = c("left", "right", "bottom"),
                              fontColour = "#909090")

      last_row_no_total_bottomright_cell <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = c("left", "right", "bottom"), wrapText = TRUE)
      last_row_no_total_bottomright_cell_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = c("left", "right", "bottom"), wrapText = TRUE)
      last_col_no_total_bottomright_cell <- #SAME NORMAL
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              textDecoration = "Bold", border = "TopBottomLeftRight", wrapText = TRUE)
      last_col_no_total_bottomright_cell_no_perc <- #SAME NORMAL
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              textDecoration = "Bold", border = "TopBottomLeftRight", wrapText = TRUE)
      both_no_total_bottomright_cell <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats,
                              border = c("left", "right", "bottom"), wrapText = TRUE)
      both_no_total_bottomright_cell_no_perc <-
        openxlsx::createStyle(halign = "right", valign = "top", numFmt = numformats_no_perc,
                              border = c("left", "right", "bottom"), wrapText = TRUE)

    }

    first_column <-
      openxlsx::createStyle(halign = "right", valign = "top", border = "LeftRight",
                            wrapText = TRUE)
    first_column_insufficient_headcount <-
      openxlsx::createStyle(halign = "right", valign = "top", border = "LeftRight",
                            wrapText = TRUE, fontColour = "#909090")
    topleft_cell <-
      openxlsx::createStyle(halign = "right", valign = "bottom", wrapText = TRUE,
                            textDecoration = "Bold", border = c("bottom", "right"))
    bottomleft_cell <-
      openxlsx::createStyle(halign = "right", valign = "top", wrapText = TRUE,
                            textDecoration = "Bold", border = "TopBottomLeftRight")

    last_row_no_total_first_column_insufficient_headcount <-
      openxlsx::createStyle(halign = "right", valign = "top", c("left", "right", "bottom"),
                            wrapText = TRUE, fontColour = "#909090")
    last_row_no_total_bottomleft_cell <-
      openxlsx::createStyle(halign = "right", valign = "top", wrapText = TRUE,
                            border = c("left", "right", "bottom") )

    after_last_col <-
      openxlsx::createStyle(halign = "right", valign = "top", wrapText = TRUE,
                            border = c("left") )

    # Variance, pvalue, unweighted count, legends :
    Vnuage_style <-
      openxlsx::createStyle(halign = "right", valign = "center", numFmt = "0.000", fontSize = 9)
    p_style <-
      openxlsx::createStyle(halign = "right", valign = "center", numFmt = "0.000%", fontSize = 9)
    effectifs_style <-
      openxlsx::createStyle(halign = "right", valign = "center", numFmt = "# ### ### ##0", fontSize = 9)
    Vnuage_p_eff_name_style <-
      openxlsx::createStyle(halign = "right", valign = "center", fontSize = 9)
    subtext_style <- openxlsx::createStyle(halign = "left", valign = "center", fontSize = 9)

    # Contributions of rows and cols to variance,
    #  supplementary total, supplementary ratios :
    row_contrib_top <-
      openxlsx::createStyle(halign = "center", valign = "bottom", # border = "TopBottomLeftRight",
                            wrapText = TRUE)
    row_contrib_column <-
      openxlsx::createStyle(halign = "right", valign = "top", #border = "LeftRight",
                            numFmt = "0%")
    row_contrib_bottom <-
      openxlsx::createStyle(halign = "right", valign = "top", #border = "TopBottomLeftRight",
                            numFmt = "0%")

    row_another_total_top <-
      openxlsx::createStyle(halign = "center", valign = "bottom",  border = "TopBottomLeftRight",
                            wrapText = TRUE)
    row_another_column <-
      openxlsx::createStyle(halign = "right", valign = "top", border = "LeftRight",
                            numFmt = "0.0%")
    no_total_row_row_another_column <-
      openxlsx::createStyle(halign = "right", valign = "top", border = c("left", "right", "bottom"),
                            numFmt = "0.0%")
    row_another_total_bottom <-
      openxlsx::createStyle(halign = "right", valign = "top", border = "TopBottomLeftRight",
                            numFmt = numformats_no_perc)
    col_sup_top <-
      openxlsx::createStyle(halign = "center", valign = "bottom",
                            textDecoration = "Bold", textRotation = 90,
                            border = "TopBottomLeftRight",
                            wrapText = TRUE)
    col_sup_quali_col <-
      openxlsx::createStyle(halign = "right", valign = "top", border = "LeftRight",
                            numFmt = "0%")
    col_sup_quali_bottom  <-
      openxlsx::createStyle(halign = "right", valign = "top", border = "TopBottomLeftRight",
                            numFmt = "0%")
    no_total_row_col_sup_quali_bottom  <-
      openxlsx::createStyle(halign = "right", valign = "top", border = c("left", "right", "bottom"),
                            numFmt = "0%")
    col_sup_quanti_col <-
      openxlsx::createStyle(halign = "right", valign = "top", border = "LeftRight",
                            numFmt = numformats_quanti)
    col_sup_quanti_bottom <-
      openxlsx::createStyle(halign = "right", valign = "top", border = "TopBottomLeftRight",
                            numFmt = numformats_quanti)
    no_total_row_col_sup_quanti_bottom <-
      openxlsx::createStyle(halign = "right", valign = "top", border = c("left", "right", "bottom"),
                            numFmt = numformats_quanti)


    col_contrib_left <-
      openxlsx::createStyle(halign = "right", valign = "bottom", #border = "TopBottomLeftRight",
                            wrapText = TRUE)
    col_contrib_row <-
      openxlsx::createStyle(halign = "right", valign = "bottom", #border = "TopBottom",
                            numFmt = "0%")
    col_contrib_right <-
      openxlsx::createStyle(halign = "right", valign = "bottom", #border = "TopBottomLeftRight",
                            numFmt = "0%")
    col_another_total_left  <-
      openxlsx::createStyle(halign = "right", valign = "bottom", border = "TopBottomLeftRight",
                            wrapText = TRUE)
    col_another_row <-
      openxlsx::createStyle(halign = "right", valign = "bottom", border = "TopBottom",
                            numFmt = "0.0%")
    col_another_total_right <-
      openxlsx::createStyle(halign = "right", valign = "bottom", border = "TopBottomLeftRight",
                            numFmt = numformats_no_perc)
    no_total_row_col_another_total_right <-
      openxlsx::createStyle(halign = "right", valign = "bottom", border = c("top", "bottom", "right"),
                            numFmt = "0.0%")
    row_sup_left <-
      openxlsx::createStyle(halign = "right", valign = "bottom", textDecoration = "Bold", border = "TopBottomLeftRight"
      )
    row_sup_quali_row <- openxlsx::createStyle(halign = "right", valign = "bottom", border = "TopBottom",
                                               numFmt = "0%")
    row_sup_quali_right <- openxlsx::createStyle(halign = "right", valign = "bottom", border = "TopBottomLeftRight",
                                                 numFmt = "0%")
    no_total_col_row_sup_quali_right <-
      openxlsx::createStyle(halign = "right", valign = "bottom", border = c("top", "bottom", "right"),
                            numFmt = "0%")
    row_sup_quanti_row <-
      openxlsx::createStyle(halign = "right", valign = "bottom", border = "TopBottom",
                            numFmt = numformats_quanti)
    row_sup_quanti_right <-
      openxlsx::createStyle(halign = "right", valign = "bottom", border = "TopBottomLeftRight",
                            numFmt = numformats_quanti)
    no_total_col_row_sup_quanti_right <-
      openxlsx::createStyle(halign = "right", valign = "bottom", border = c("top", "bottom", "right"),
                            numFmt = numformats_quanti)

    # Close to 0 :
    style_zero     <- openxlsx::createStyle(fontColour = "#EAEAEA")

    # More than 5% to 35% above mean : vert
    style_plus_5   <- openxlsx::createStyle(bgFill = "#e6f2e6")                   #f2f9f2  #b3d9b5
    style_plus_10  <- openxlsx::createStyle(bgFill = "#b3d9b5")                   #b3d9b5  #8ec690
    style_plus_15  <- openxlsx::createStyle(bgFill = "#82c083")                   #68b36b  #68b36b
    style_plus_25  <- openxlsx::createStyle(bgFill = "#48ad4c")                   #3c903f  #43a047
    style_plus_35  <- openxlsx::createStyle(bgFill = "#3a8a3d")                   #2c5e2c  #3c903f   #fontColour = "#ffffff"

    # Less than 5% to 35% below mean : orange
    style_minus_5  <- openxlsx::createStyle(bgFill = "#ffeddf")      #fff6ef   #ffeddf     #fbcaa2   #f7c3c2
    style_minus_10 <- openxlsx::createStyle(bgFill = "#fbcaa2")                #fbcaa2     #f9b57d   #f4afae
    style_minus_15 <- openxlsx::createStyle(bgFill = "#fcb073")                #f7a058     #f7a058   #ef8885
    style_minus_25 <- openxlsx::createStyle(bgFill = "#de873f")      #b66f36   #de873f     #f79646   #ea605d
    style_minus_35 <- openxlsx::createStyle(bgFill = "#c36a21")      #b66f36               #de873f   #e53935   #, fontColour = "#ffffff"

    style_plus_minus_list <-
      list(style_plus_5, style_plus_10, style_plus_15,
           style_plus_25, style_plus_25, style_plus_35, # plus 25 + double => style 25
           style_minus_5 , style_minus_10, style_minus_15,
           style_minus_25, style_minus_35)

    style_plus_minus_cell_contrib_list <-
      list(style_plus_5, style_plus_10, style_plus_15, style_plus_25, style_plus_35,
           style_minus_5, style_minus_10, style_minus_15, style_minus_25, style_minus_35)
    # list(style_plus_5, style_plus_15, style_plus_35,
    #      style_minus_5, style_minus_15, style_minus_35)


    #Colwidths ################################################################
    purrr::walk(sheetnb, ~openxlsx::setColWidths(wb, sheet = .x, cols = 1, widths = 30))
    if (colnames_rotation > 0) {
      if (colnames_rotation > 30 & colnames_rotation < 60) {
        purrr::walk2(sheetnb, tabs,
                     ~openxlsx::setColWidths(wb, sheet = .x, cols = 2:(ncol(.y)-1), widths = 8))
        purrr::pwalk(list(sheetnb, tabs, purrr::map(totc, ~ dplyr::if_else(., 13, 8))),
                     ~ openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2), widths = ..3))
      } else if (colnames_rotation > 60) {
        purrr::walk2(sheetnb, tabs,
                     ~openxlsx::setColWidths(wb, sheet = .x, cols = 2:(ncol(.y)-1),
                                             widths = 6 + 8*cos(colnames_rotation/90*pi/2))) #Entre 6 et 14
        purrr::pwalk(list(sheetnb, tabs, purrr::map(totc, ~ dplyr::if_else(., 13, 6 + 8*cos(colnames_rotation/90*pi/2)))),
                     ~ openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2), widths = ..3))
      }
      purrr::walk(sheetnb,
                  ~openxlsx::setRowHeights(wb, sheet = ., rows = 1,
                                           heights = 13.8 + 105*sin(colnames_rotation/90*pi/2)))



      #Enlarge columns if there is confidence intervals
      # if (any(tab_with_CI_on_sheet)) {
      #   purrr::walk2(1:length(tab_with_CI_on_sheet)[tab_with_CI_on_sheet],
      #         purrr::map(tabs_on_same_sheet, ~ ncol(tabs[[.[1]]]))[tab_with_CI_on_sheet],
      #         ~ openxlsx::setColWidths(wb, sheet = .x, cols = 2:(.y-1), widths = 14))
      # }

    } else {
      purrr::walk2(sheetnb, tabs,
                   ~openxlsx::setColWidths(wb, sheet = .x, cols = 2:(ncol(.y)-1), widths = 13))
      purrr::pwalk(list(sheetnb, tabs, purrr::map(totc, ~ dplyr::if_else(., 13, 8))),
                   ~ openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2), widths = ..3))
    }



    # Reduce heigth of first line for compact tabs
    if (compact == TRUE) {
      purrr::walk2(sheetnb[newsheet == FALSE], start_row[newsheet == FALSE],
                   ~ openxlsx::setRowHeights(wb, sheet = .x, rows = 1 + .y,
                                             heights = 3))
    }

    purrr::pwalk(list(sheetnb, tabs, purrr::map(totc, ~ dplyr::if_else(., 1.45, 13))),
                 ~openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2) + 1, widths = ..3))

    if (any(purrr::map_lgl(another_total_cols_var, ~ . == 1)) ) {
      purrr::pwalk(list(sheetnb, tabs, nbcol_tot) %>% purrr::map(~ purrr::keep(., purrr::map_lgl(another_total_cols_var, ~ . == 1))),
                   function(.sheetnb, .tabs, .nbcol_tot)
                     openxlsx::setColWidths(wb, sheet = .sheetnb, cols = ncol(.tabs) + .nbcol_tot,
                                            widths = 10.64))
    }
    if (any(purrr::map_lgl(sup_cols_var, ~ . >= 1)) ) {
      purrr::pwalk(list(sheetnb, tabs, nbcol_sup, nbcsup) %>%
                     purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_var, ~ . >= 1))),
                   function(.sheetnb, .tabs, .nbcol_sup, .nbcsup)
                     openxlsx::setColWidths(wb, sheet = .sheetnb, cols = ncol(.tabs) + .nbcol_sup + .nbcsup,
                                            widths = 5))
    }
    # if (any(purrr::map_lgl(sup_cols_var, ~ . >= 1)) ) {
    #   purrr::pwalk(list(sheetnb, tabs, nbcol_sup, nbcsup) %>%
    #           purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_var, ~ . >= 1))),
    #         function(.sheetnb, .tabs, .nbcol_sup, .nbcsup)
    #           openxlsx::setColWidths(wb, sheet = .sheetnb, cols = ncol(.tabs) + .nbcol_sup + .nbcsup,
    #                        widths = 4.55))
    # }
    if (any(purrr::map_lgl(sup_cols_quanti_var, ~ length(.) > 0))) {
      purrr::pwalk(list(sheetnb, tabs, nbcol_sup, sup_cols_quanti_var) %>%
                     purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_quanti_var, ~ length(.) > 0))),
                   function(.sheetnb, .tabs, .nbcol_sup, .quanti)
                     openxlsx::setColWidths(wb, sheet = .sheetnb, cols = ncol(.tabs) + .nbcol_sup + .quanti - 1,
                                            widths = 8))
    }

    if (any(purrr::map_lgl(contrib_var, ~ . == 1)) ) {
      purrr::pwalk(list(sheetnb, tabs, nbcol_ct) %>%
                     purrr::map(~ purrr::keep(., purrr::map_lgl(contrib_var, ~ . == 1))),
                   function(.sheetnb, .tabs, .nbcol_ct)
                     openxlsx::setColWidths(wb, sheet = .sheetnb, cols = ncol(.tabs) + .nbcol_ct - 1,
                                            widths = 1.45))
      purrr::pwalk(list(sheetnb, tabs, nbcol_ct) %>%
                     purrr::map(~ purrr::keep(., purrr::map_lgl(contrib_var, ~ . == 1))),
                   function(.sheetnb, .tabs, .nbcol_ct)
                     openxlsx::setColWidths(wb, sheet = .sheetnb, ncol(.tabs) + .nbcol_ct,
                                            widths = 12))
    }

    purrr::walk(unique(sheetnb),
                ~ openxlsx::showGridLines(wb, sheet = .x, showGridLines = FALSE))

    unique(sheetnb)[only_same_headers_on_sheet == TRUE] %>%
      purrr::walk(~ openxlsx::freezePane(wb, sheet = .x, firstRow = TRUE, firstCol = TRUE))

    # Apply formats ####################################################

    # Base table text
    maplist <- list(sheetnb, start_row, tabs)
    maplist_headers <- list(sheetnb, start_row, tabs) %>% #ERROR HERE ? SEEM OK.
      purrr::map(~ purrr::keep(., !hd_remove))
    maplist_headers_remove <- list(sheetnb, start_row, tabs) %>%
      purrr::map(~ purrr::keep(., hd_remove))


    purrr::pwalk(maplist_headers,  function(.sheetnb, .start_row, .tabs)
      openxlsx::addStyle(wb, .sheetnb, style = headers, gridExpand = T,
                         rows = .start_row + 1, cols = 1:ncol(.tabs)))

    purrr::pwalk(maplist, function(.sheetnb, .start_row, .tabs)
      openxlsx::addStyle(wb, .sheetnb, style = first_column , gridExpand = T,
                         rows = .start_row + 1:(nrow(.tabs) + 1), cols = 1))
    purrr::walk2(sheetnb, start_row,
                 ~ openxlsx::addStyle(wb, sheet = .x, style = topleft_cell, gridExpand = T,
                                      rows = .y + 1, cols = 1))

    if (any(!hd_remove & totc)) {
      maplist_headers_totc <- list(sheetnb, start_row, tabs) %>%
        purrr::map(~ purrr::keep(., !hd_remove & totc))
      purrr::pwalk(maplist_headers_totc,  function(.sheetnb, .start_row, .tabs)
        openxlsx::addStyle(wb, .sheetnb, style = topright_cell, gridExpand = T,
                           rows = .start_row + 1, cols = ncol(.tabs)))
    }
    if (any(totr)) {
      maplist_totr <- list(sheetnb, start_row, tabs) %>%
        purrr::map(~ purrr::keep(., totr))
      purrr::pwalk(maplist_totr, function(.sheetnb, .start_row, .tabs)
        openxlsx::addStyle(wb, .sheetnb, style = bottomleft_cell, gridExpand = T,
                           rows = .start_row + nrow(.tabs) + 1, cols = 1))
    }
    if (any(!totr)) {
      maplist_no_totr <- list(sheetnb, start_row, tabs) %>%
        purrr::map(~ purrr::keep(., !totr))
      purrr::pwalk(maplist_no_totr, function(.sheetnb, .start_row, .tabs)
        openxlsx::addStyle(wb, .sheetnb, style = last_row_no_total_bottomleft_cell, gridExpand = T,
                           rows = .start_row + nrow(.tabs) + 1, cols = 1))
    }

    #phantom_headers farther to beat insufficient_headcount



    #Base table numbers with percentages
    pct_condition <- tabs_percentages %in% c("row", "col", "all")
    if (any(pct_condition)) {
      maplist_percentages <- maplist %>%
        purrr::map(~ purrr::keep(., pct_condition))

      purrr::pwalk(maplist_percentages, function(.sheetnb, .start_row, .tabs)
        openxlsx::addStyle(wb, .sheetnb, style = base_style, gridExpand = T,
                           rows = .start_row + 2:nrow(.tabs), cols = 2:ncol(.tabs)))

      #To get the right border ok anytime :
      purrr::pwalk(maplist_percentages, function(.sheetnb, .start_row, .tabs)
        openxlsx::addStyle(wb, .sheetnb, style = after_last_col, gridExpand = T,
                           rows = .start_row + 2:(nrow(.tabs)+1), cols = ncol(.tabs) + 1))


      if (any(pct_condition & totc)) {
        maplist_percentages_totc <- maplist %>%
          purrr::map(~ purrr::keep(., pct_condition & totc))
        purrr::pwalk(maplist_percentages_totc, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = total_column_style, gridExpand = T,
                             rows = .start_row + 2:(nrow(.tabs)+1),  cols = ncol(.tabs)))
      }

      if (any(pct_condition & totr)) {
        maplist_percentages_totc <- maplist %>%
          purrr::map(~ purrr::keep(., pct_condition & totr))
        purrr::pwalk(maplist_percentages_totc, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = total_row_style, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = 2:(ncol(.tabs)-1)))
      }
      if (any(pct_condition & ! totr)) {
        maplist_percentages_no_totc <- maplist %>%
          purrr::map(~ purrr::keep(., pct_condition & ! totr))
        purrr::pwalk(maplist_percentages_no_totc, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = last_row_no_total, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = 2:(ncol(.tabs)-1)))
      }


      if (any(pct_condition & totr & totc)) {
        maplist_percentages_bottomright <- maplist %>%
          purrr::map(~ purrr::keep(., pct_condition & totr & totc))
        purrr::pwalk(maplist_percentages_bottomright, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = bottomright_cell, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = ncol(.tabs)))
      }
      if (any(pct_condition & totr & !totc)) {
        maplist_percentages_bottomright <- maplist %>%
          purrr::map(~ purrr::keep(., pct_condition & totr & !totc))
        purrr::pwalk(maplist_percentages_bottomright, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = last_col_no_total_bottomright_cell, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = ncol(.tabs)))
      }
      if (any(pct_condition & !totr & totc)) {
        maplist_percentages_bottomright <- maplist %>%
          purrr::map(~ purrr::keep(., pct_condition & !totr & totc))
        purrr::pwalk(maplist_percentages_bottomright, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = last_row_no_total_bottomright_cell, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = ncol(.tabs)))
      }
      if (any(pct_condition & !totr & !totc)) {
        maplist_percentages_bottomright <- maplist %>%
          purrr::map(~ purrr::keep(., pct_condition & !totr & !totc))
        purrr::pwalk(maplist_percentages_bottomright, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = both_no_total_bottomright_cell, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = ncol(.tabs)))
      }



      #Insufficient headcount with perc in grey
      # (less than minimal_headcount, 30 by default) :
      ihv1_perc <- purrr::map_lgl(insufficient_headcount_var1, ~ length(.) != 0) &
        tabs_percentages %in% c("row", "col", "all")
      ihv1_perc_no_totr <-
        purrr::map2_lgl(insufficient_headcount_var1, tabs,
                        ~ purrr::keep(.x, .x == nrow(.y) ) %>% length() != 0) &
        tabs_percentages %in% c("row", "col", "all") & !totr
      # ihv1_perc_no_totc <- purrr::map_lgl(insufficient_headcount_var1, ~ length(.) != 0) &
      #   tabs_percentages %in% c("row", "col", "all") & !totc
      if (any(ihv1_perc)) {
        maplist_perc_insufficient_headcount_var1 <- maplist %>%
          append(list(insufficient_headcount_var1)) %>%
          purrr::map(~ purrr::keep(., ihv1_perc))
        purrr::pwalk(maplist_perc_insufficient_headcount_var1,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = last_row_no_total_insufficient_headcount,
                                          gridExpand = T,
                                          rows = ..2 + ..4, cols = 2:ncol(..3)))
        purrr::pwalk(maplist_perc_insufficient_headcount_var1,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = total_column_insufficient_headcount,
                                          gridExpand = T,
                                          rows = ..2 + ..4,  cols = ncol(..3)))
        purrr::pwalk(maplist_perc_insufficient_headcount_var1,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = first_column_insufficient_headcount,
                                          gridExpand = T,
                                          rows = ..2 + ..4, cols = 1))

        if (any(ihv1_perc_no_totr)) {
          maplist_perc_insufficient_headcount_var1_no_totr <- maplist %>%
            append(list(purrr::map2(insufficient_headcount_var1, tabs, ~ purrr::keep(.x, .x == nrow(.y) )))) %>%
            purrr::map(~ purrr::keep(., ihv1_perc_no_totr))

          purrr::pwalk(maplist_perc_insufficient_headcount_var1_no_totr,
                       ~ openxlsx::addStyle(wb, sheet = ..1, style = base_style_insufficient_headcount, #last_row_no_total_insufficient_headcount
                                            gridExpand = T,
                                            rows = ..2 + ..4, cols = 2:ncol(..3)))
          purrr::pwalk(maplist_perc_insufficient_headcount_var1_no_totr,
                       ~ openxlsx::addStyle(wb, sheet = ..1, style = last_row_no_total_total_column_insufficient_headcount,
                                            gridExpand = T,
                                            rows = ..2 + ..4,  cols = ncol(..3)))
          purrr::pwalk(maplist_perc_insufficient_headcount_var1_no_totr,
                       ~ openxlsx::addStyle(wb, sheet = ..1, style = last_row_no_total_first_column_insufficient_headcount,
                                            gridExpand = T,
                                            rows = ..2 + ..4, cols = 1))
        }
        # if (any(ihv1_perc_no_totc)) {
        #   maplist_perc_insufficient_headcount_var1_no_totc <- maplist %>%
        #     append(list(insufficient_headcount_var1)) %>%
        #     purrr::map(~ purrr::keep(., ihv1_perc_no_totc))
        #   purrr::pwalk(maplist_perc_insufficient_headcount_var1_no_totc,
        #         ~ openxlsx::addStyle(wb, sheet = ..1, style = total_column_insufficient_headcount,
        #                    gridExpand = T,
        #                    rows = ..2 + ..4,  cols = ncol(..3)))
        # }
      }

      ihv2_perc <- purrr::map_lgl(insufficient_headcount_var2, ~ length(.) != 0) &
        tabs_percentages %in% c("row", "col", "all")
      # ihv2_perc_no_totc <- purrr::map_lgl(insufficient_headcount_var2, ~ length(.) != 0) &
      #   tabs_percentages %in% c("row", "col", "all") & ! totc
      ihv2_perc_no_totr <-
        purrr::map_lgl(insufficient_headcount_var2, ~ length(.) != 0) &
        tabs_percentages %in% c("row", "col", "all") & ! totr
      if (any(ihv2_perc)) {
        maplist_perc_insufficient_headcount_var2 <- maplist %>%
          append(list(insufficient_headcount_var2)) %>% #List or already ?
          purrr::map(~ purrr::keep(., ihv2_perc))

        purrr::pwalk(maplist_perc_insufficient_headcount_var2,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = base_style_insufficient_headcount,
                                          gridExpand = T,
                                          rows = ..2 + 2:(nrow(..3) + 1), cols = ..4))
        purrr::pwalk(maplist_perc_insufficient_headcount_var2,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = headers_insufficient_headcount,
                                          gridExpand = T,
                                          rows = ..2 + 1, cols = ..4))
        purrr::pwalk(maplist_perc_insufficient_headcount_var2,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = total_row_insufficient_headcount,
                                          gridExpand = T,
                                          rows = ..2 + nrow(..3) + 1, cols = ..4))

        if (any(ihv2_perc_no_totr)) {
          maplist_perc_insufficient_headcount_var2_nototr <- maplist %>%
            append(list(insufficient_headcount_var2)) %>%
            purrr::map(~ purrr::keep(., ihv2_perc_no_totr))
          purrr::pwalk(maplist_perc_insufficient_headcount_var2_nototr,
                ~ openxlsx::addStyle(wb, sheet = ..1, style = last_row_no_total_insufficient_headcount,
                           gridExpand = T,
                           rows = ..2 + nrow(..3) + 1, cols = ..4))
        }

        # if (any(ihv2_perc_no_totc)) {
        #   maplist_perc_insufficient_headcount_var2_no_totc <- maplist %>%
        #     append(list(purrr::map(insufficient_headcount_var2, ~ dplyr::last(.)))) %>%
        #     purrr::map(~ purrr::keep(., ihv2_perc_no_totc))
        # purrr::pwalk(maplist_perc_insufficient_headcount_var2_no_totc,
        #       ~ openxlsx::addStyle(wb, sheet = ..1, style = base_style_insufficient_headcount,
        #                  gridExpand = T,
        #                  rows = ..2 + 2:(nrow(..3) + 1), cols = ..4))
        # # purrr::pwalk(maplist_perc_insufficient_headcount_var2_no_totc,
        # #       ~ openxlsx::addStyle(wb, sheet = ..1, style = headers_insufficient_headcount,
        # #                  gridExpand = T,
        #                  rows = ..2 + 1, cols = ..4))
        # purrr::pwalk(maplist_perc_insufficient_headcount_var2_no_totc,
        #       ~ openxlsx::addStyle(wb, sheet = ..1, style = total_row_insufficient_headcount,
        #                  gridExpand = T,
        #                  rows = ..2 + nrow(..3) + 1, cols = ..4))
        # }
      }
    }


    #Base table numbers with no percentages
    if (any(tabs_percentages == "no")) {
      maplist_no_perc <- maplist %>% purrr::map(~ purrr::keep(., tabs_percentages == "no"))

      purrr::pwalk(maplist_no_perc, function(.sheetnb, .start_row, .tabs)
        openxlsx::addStyle(wb, .sheetnb, style = base_style_no_perc, gridExpand = T,
                           rows = .start_row + 2:(nrow(.tabs)+1), cols = 2:ncol(.tabs)))

      if (any(tabs_percentages == "no" & totc)) {
        maplist_no_perc_totc <- maplist %>%
          purrr::map(~ purrr::keep(., tabs_percentages == "no" & totc))
        purrr::pwalk(maplist_no_perc_totc, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = total_column_no_perc, gridExpand = T,
                             rows = .start_row + 2:(nrow(.tabs)+1),  cols = ncol(.tabs)))
      }


      if (any(tabs_percentages == "no" & totr)) {
        maplist_no_perc_totc <- maplist %>%
          purrr::map(~ purrr::keep(., tabs_percentages == "no" & totr))
        purrr::pwalk(maplist_no_perc, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = total_row_no_perc, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = 2:(ncol(.tabs)-1)))
      }
      if (any(tabs_percentages == "no" & ! totr)) {
        maplist_no_perc_no_totc <- maplist %>%
          purrr::map(~ purrr::keep(., tabs_percentages == "no" & ! totr))
        purrr::pwalk(maplist_no_perc_no_totc, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = last_row_no_total_no_perc, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = 2:(ncol(.tabs)-1)))
      }

      if (any(tabs_percentages == "no" & totr & totc)) {
        maplist_no_perc_bottomright <- maplist %>%
          purrr::map(~ purrr::keep(., tabs_percentages == "no" & totr & totc))
        purrr::pwalk(maplist_no_perc_bottomright, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = bottomright_cell_no_perc, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = ncol(.tabs)))
      }
      if (any(tabs_percentages == "no" & totr & !totc)) {
        maplist_no_perc_bottomright <- maplist %>%
          purrr::map(~ purrr::keep(., tabs_percentages == "no" & totr & !totc))
        purrr::pwalk(maplist_no_perc_bottomright, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = last_col_no_total_bottomright_cell_no_perc, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = ncol(.tabs)))
      }
      if (any(tabs_percentages == "no" & !totr & totc)) {
        maplist_no_perc_bottomright <- maplist %>%
          purrr::map(~ purrr::keep(., tabs_percentages == "no" & !totr & totc))
        purrr::pwalk(maplist_no_perc_bottomright, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = last_row_no_total_bottomright_cell_no_perc, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = ncol(.tabs)))
      }
      if (any(tabs_percentages == "no" & !totr & !totc)) {
        maplist_no_perc_bottomright <- maplist %>%
          purrr::map(~ purrr::keep(., tabs_percentages == "no" & !totr & !totc))
        purrr::pwalk(maplist_no_perc_bottomright, function(.sheetnb, .start_row, .tabs)
          openxlsx::addStyle(wb, .sheetnb, style = both_no_total_bottomright_cell_no_perc, gridExpand = T,
                             rows = .start_row + nrow(.tabs) + 1, cols = ncol(.tabs)))
      }



      #Insufficient headcount with no perc in grey
      #  (less than minimal_headcount, 30 by default) :
      ihv1_no_perc <- purrr::map_lgl(insufficient_headcount_var1, ~ length(.) != 0) &
        tabs_percentages == "no"
      ihv1_no_perc_no_totr <-
        purrr::map2_lgl(insufficient_headcount_var1, tabs,
                        ~ purrr::keep(.x, .x == nrow(.y) ) %>% length() != 0) &
        tabs_percentages == "no" & !totr
      if (any(ihv1_no_perc)) {
        maplist_no_perc_insufficient_headcount_var1 <- maplist %>%
          append(list(insufficient_headcount_var1)) %>%
          purrr::map(~ purrr::keep(., ihv1_no_perc))

        purrr::pwalk(maplist_no_perc_insufficient_headcount_var1,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = base_style_insufficient_headcount_no_perc,
                                          gridExpand = T,
                                          rows = ..2 + ..4, cols = 2:ncol(..3)))
        purrr::pwalk(maplist_no_perc_insufficient_headcount_var1,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = total_column_insufficient_headcount_no_perc,
                                          gridExpand = T,
                                          rows = ..2 + ..4,  cols = ncol(..3)))
        purrr::pwalk(maplist_no_perc_insufficient_headcount_var1,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = first_column_insufficient_headcount,
                                          gridExpand = T,
                                          rows = ..2 + ..4, cols = 1))



        if (any(ihv1_no_perc_no_totr)) {
          maplist_no_perc_insufficient_headcount_var1_no_totr <- maplist %>%
            append(list(purrr::map2(insufficient_headcount_var1, tabs, ~ purrr::keep(.x, .x == nrow(.y) )))) %>%
            purrr::map(~ purrr::keep(., ihv1_no_perc))

          purrr::pwalk(maplist_no_perc_insufficient_headcount_var1_no_totr,
                       ~ openxlsx::addStyle(wb, sheet = ..1, style = last_row_no_total_insufficient_headcount_no_perc,
                                            gridExpand = T,
                                            rows = ..2 + ..4, cols = 2:ncol(..3)))
          purrr::pwalk(maplist_no_perc_insufficient_headcount_var1_no_totr,
                       ~ openxlsx::addStyle(wb, sheet = ..1, style = last_row_no_total_total_column_insufficient_headcount_no_perc,
                                            gridExpand = T,
                                            rows = ..2 + ..4,  cols = ncol(..3)))
          purrr::pwalk(maplist_no_perc_insufficient_headcount_var1_no_totr,
                       ~ openxlsx::addStyle(wb, sheet = ..1, style = last_row_no_total_first_column_insufficient_headcount,
                                            gridExpand = T,
                                            rows = ..2 + ..4, cols = 1))
        }

      }

      ihv2_no_perc <- purrr::map_lgl(insufficient_headcount_var2, ~ length(.) != 0) &
        tabs_percentages == "no"
      ihv2_no_perc_no_totr <-
        purrr::map_lgl(insufficient_headcount_var2, ~ length(.) != 0) &
        tabs_percentages == "no" & ! totr
      if (any(ihv2_no_perc)) {
        maplist_no_perc_insufficient_headcount_var2 <- maplist %>%
          append(list(insufficient_headcount_var2)) %>%
          purrr::map(~ purrr::keep(., ihv2_no_perc))

        purrr::pwalk(maplist_no_perc_insufficient_headcount_var2,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = base_style_insufficient_headcount_no_perc,
                                          gridExpand = T,
                                          rows = ..2 + 2:(nrow(..3) + 1), cols = ..4))
        purrr::pwalk(maplist_no_perc_insufficient_headcount_var2,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = headers_insufficient_headcount,
                                          gridExpand = T,
                                          rows = ..2 + 1, cols = ..4))
        purrr::pwalk(maplist_no_perc_insufficient_headcount_var2,
                     ~ openxlsx::addStyle(wb, sheet = ..1, style = total_row_insufficient_headcount_no_perc,
                                          gridExpand = T,
                                          rows = ..2 + nrow(..3) + 1, cols = ..4))

        if (any(ihv2_no_perc_no_totr)) {
          maplist_perc_insufficient_headcount_var2_nototr <- maplist %>%
            append(list(insufficient_headcount_var2)) %>%
            purrr::map(~ purrr::keep(., ihv2_no_perc_no_totr))
          purrr::pwalk(maplist_perc_insufficient_headcount_var2_nototr,
                       ~ openxlsx::addStyle(wb, sheet = ..1, style = last_row_no_total_insufficient_headcount_no_perc,
                                            gridExpand = T,
                                            rows = ..2 + nrow(..3) + 1, cols = ..4))
        }
      }
    }


    #Base text phantom headers : to pass before insufficient headcounts
    purrr::pwalk(maplist_headers_remove,  function(.sheetnb, .start_row, .tabs)
      openxlsx::addStyle(wb, .sheetnb, style = phantom_headers, gridExpand = T,
                         rows = .start_row + 1, cols = 2:ncol(.tabs)))

    #Multicols and multirows : quantitative variables
    multicols_quanti_condition <- purrr::map_lgl(multicols_quanti_var, ~ length(.) != 0)
    if (any(multicols_quanti_condition)) {
      maplist_quanti_cols <- maplist %>% append(list(multicols_quanti_var)) %>%
        purrr::map(~ purrr::keep(., multicols_quanti_condition))

      purrr::pwalk(maplist_quanti_cols,
                   ~openxlsx::addStyle(wb, sheet = ..1, style = base_style_quanti,
                                       gridExpand = T,
                                       rows = ..2 + 2:nrow(..3), cols = ..4))
      purrr::pwalk(maplist_quanti_cols,
                   ~openxlsx::addStyle(wb, sheet = ..1, style = total_row_quanti,
                                       gridExpand = T,
                                       rows = ..2 + nrow(..3) + 1,  cols = ..4))
    }

    multirows_quanti_condition <- purrr::map_lgl(multirows_quanti_var, ~ length(.) != 0)
    if (any(multirows_quanti_condition)) {
      maplist_quanti_rows <- maplist %>% append(list(multirows_quanti_var)) %>%
        purrr::map(~ purrr::keep(., multirows_quanti_condition))

      purrr::pwalk(maplist_quanti_rows,
                   ~openxlsx::addStyle(wb, sheet = ..1, style = base_style_quanti,
                                       gridExpand = T,
                                       rows = ..2 + ..4, cols = 2:(ncol(..3) - 1)))
      purrr::pwalk(maplist_quanti_rows,
                   ~openxlsx::addStyle(wb, sheet = ..1, style = total_column_quanti,
                                       gridExpand = T,
                                       rows = ..2 + ..4,  cols = ncol(..3)))
    }






    #Vnuage, probability of Chi2, unweighted count, legend :
    maplist_ <- list(sheetnb, start_row, tabs)

    if (any(vari_row >= 1)) {
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_vari, Vnuage) %>%
                     purrr::map(~ purrr::keep(., vari_row >= 1)),
                   ~openxlsx::writeData(wb, ..1, ..5, colNames = FALSE,
                                        startRow = ..2 + nrow(..3) + ..4, startCol = ncol(..3)))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_vari) %>%
                     purrr::map(~ purrr::keep(., vari_row >= 1)),
                   function(.sheetnb, .start_row, .tabs, .nbrow_vari)
                     openxlsx::addStyle(wb, .sheetnb, style = Vnuage_style, gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_vari, cols = ncol(.tabs)))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_vari) %>%
                     purrr::map(~ purrr::keep(., vari_row >= 1)),
                   ~openxlsx::writeData(wb, ..1,
                                        stringi::stri_unescape_unicode("Variance (m\u00e9trique du Chi\u00b2) ="),
                                        colNames = FALSE,
                                        startRow = ..2 + nrow(..3) + ..4, startCol = ncol(..3) -1))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_vari) %>%
                     purrr::map(~ purrr::keep(., vari_row >= 1)),
                   function(.sheetnb, .start_row, .tabs, .nbrow_vari)
                     openxlsx::addStyle(wb, .sheetnb, style = Vnuage_p_eff_name_style, gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_vari + 0:2,
                                        cols = ncol(.tabs) -1))
    }

    if (any(pvalue_row >= 1)) {
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_pvalue, pvalue) %>%
                     purrr::map(~ purrr::keep(., pvalue_row >= 1)),
                   ~openxlsx::writeData(wb, ..1, ..5, colNames = FALSE,
                                        startRow = ..2 + nrow(..3) + ..4, startCol = ncol(..3)))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_pvalue) %>%
                     purrr::map(~ purrr::keep(., pvalue_row >= 1)),
                   function(.sheetnb, .start_row, .tabs, .nbrow_pvalue)
                     openxlsx::addStyle(wb, .sheetnb, style = p_style, gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_pvalue, cols = ncol(.tabs)))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_pvalue) %>%
                     purrr::map(~ purrr::keep(., pvalue_row >= 1)),
                   ~openxlsx::writeData(wb, ..1,
                                        stringi::stri_unescape_unicode("Probabilit\u00e9 du Chi\u00b2 ="),
                                        colNames = FALSE,
                                        startRow = ..2 + nrow(..3) + ..4, startCol = ncol(..3) -1))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_pvalue) %>%
                     purrr::map(~ purrr::keep(., pvalue_row >= 1)),
                   function(.sheetnb, .start_row, .tabs, .nbrow_pvalue)
                     openxlsx::addStyle(wb, .sheetnb, style = Vnuage_p_eff_name_style, gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_pvalue,
                                        cols = ncol(.tabs) -1))
    }

    if (any(unweighted_headcount_row >= 1)) {
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_uwh, unweighted_headcount) %>%
                     purrr::map(~ purrr::keep(., unweighted_headcount_row >= 1)),
                   ~openxlsx::writeData(wb, ..1, ..5, colNames = FALSE,
                                        startRow = ..2 + nrow(..3) + ..4, startCol = ncol(..3)))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_uwh) %>%
                     purrr::map(~ purrr::keep(., unweighted_headcount_row >= 1)),
                   function(.sheetnb, .start_row, .tabs, .nbrow_uwh)
                     openxlsx::addStyle(wb, .sheetnb, style = effectifs_style, gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_uwh, cols = ncol(.tabs)))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_uwh) %>%
                     purrr::map(~ purrr::keep(., unweighted_headcount_row >= 1)),
                   ~openxlsx::writeData(wb, ..1,
                                        stringi::stri_unescape_unicode("Effectifs non pond\u00e9r\u00e9s ="),
                                        colNames = FALSE,
                                        startRow = ..2 + nrow(..3) + ..4, startCol = ncol(..3) -1))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_uwh) %>%
                     purrr::map(~ purrr::keep(., unweighted_headcount_row >= 1)),
                   function(.sheetnb, .start_row, .tabs, .nbrow_uwh)
                     openxlsx::addStyle(wb, .sheetnb, style = Vnuage_p_eff_name_style, gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_uwh,
                                        cols = ncol(.tabs) -1))
    }

    if (any(subtext_rows >= 1)) {
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_sub, tabs_subtext) %>%
                     purrr::map(~ purrr::keep(., subtext_rows >= 1)),
                   ~openxlsx::writeData(wb, ..1, ..5, colNames = FALSE,
                                        startRow = ..2 + nrow(..3) + ..4, startCol = 1))
      purrr::pwalk(list(sheetnb, start_row, tabs, nbrow_sub, nbrsubtext) %>%
                     purrr::map(~ purrr::keep(., subtext_rows >= 1)),
                   function(.sheetnb, .start_row, .tabs, .nbrow_sub, .nbrsubtext)
                     openxlsx::addStyle(wb, .sheetnb, style = subtext_style, gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_sub +.nbrsubtext,
                                        cols = 1))
    }





    #Supplementary totals :
    #Total columns (for line percentages) :
    if (any(purrr::map_lgl(another_total_cols_var, ~ . == 1)) ) {
      maplist_tot_col <-                list(sheetnb, start_row, tabs, nbcol_tot) %>%
        purrr::map(~ purrr::keep(., another_total_cols_var == 1))
      maplist_tot_col_headers <-        list(sheetnb, start_row, tabs, nbcol_tot) %>%
        purrr::map(~ purrr::keep(., another_total_cols_var == 1 & !hd_remove))
      maplist_tot_col_headers_remove <- list(sheetnb, start_row, tabs, nbcol_tot) %>%
        purrr::map(~ purrr::keep(., another_total_cols_var == 1 & hd_remove))

      purrr::pwalk(append(maplist_tot_col,
                          list(another_totcol_tabs[another_total_cols_var == 1])),
                   function(.sheetnb, .start_row, .tabs, .nbcol_tot, .tots)
                     openxlsx::writeData(wb, .sheetnb, .tots, colNames = FALSE,
                                         startRow = .start_row + 2,
                                         startCol = ncol(.tabs) + .nbcol_tot))
      purrr::pwalk(maplist_tot_col_headers,
                   function(.sheetnb, .start_row, .tabs, .nbcol_tot)
                     openxlsx::writeData(wb, .sheetnb,
                                         stringi::stri_unescape_unicode("Fr\u00e9quences par ligne"), colNames = FALSE,
                                         startRow = .start_row + 1,
                                         startCol = ncol(.tabs) + .nbcol_tot))
      purrr::pwalk(maplist_tot_col_headers,
                   function(.sheetnb, .start_row, .tabs, .nbcol_tot)
                     openxlsx::addStyle(wb, .sheetnb, style = row_another_total_top , gridExpand = T,
                                        rows = .start_row + 1,
                                        cols = ncol(.tabs) + .nbcol_tot))
      purrr::pwalk(maplist_tot_col_headers_remove,
                   function(.sheetnb, .start_row, .tabs, .nbcol_tot)
                     openxlsx::addStyle(wb, .sheetnb, style = phantom_headers, gridExpand = T,
                                        rows = .start_row + 1,
                                        cols = ncol(.tabs) + .nbcol_tot))
      purrr::pwalk(maplist_tot_col,
                   function(.sheetnb, .start_row, .tabs, .nbcol_tot)
                     openxlsx::addStyle(wb, .sheetnb, style = row_another_column , gridExpand = T,
                                        rows = .start_row + 2:nrow(.tabs),
                                        cols = ncol(.tabs) + .nbcol_tot))

      if (any(purrr::map_lgl(another_total_cols_var, ~ . == 1) & totr)) {
        maplist_tot_col_totr <- list(sheetnb, start_row, tabs, nbcol_tot) %>%
          purrr::map(~ purrr::keep(., another_total_cols_var == 1 & totr))
        purrr::pwalk(maplist_tot_col_totr,
                     function(.sheetnb, .start_row, .tabs, .nbcol_tot)
                       openxlsx::addStyle(wb, .sheetnb, style = row_another_total_bottom, gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + 1,
                                          cols = ncol(.tabs) + .nbcol_tot))
      }
      if (any(purrr::map_lgl(another_total_cols_var, ~ . == 1) & ! totr)) {
        maplist_tot_col_no_totr <- list(sheetnb, start_row, tabs, nbcol_tot) %>%
          purrr::map(~ purrr::keep(., another_total_cols_var == 1 & ! totr))
        purrr::pwalk(maplist_tot_col_no_totr,
                     function(.sheetnb, .start_row, .tabs, .nbcol_tot)
                       openxlsx::addStyle(wb, .sheetnb, style = no_total_row_row_another_column, gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + 1,
                                          cols = ncol(.tabs) + .nbcol_tot))

        # purrr::pwalk(maplist_no_totr, function(.sheetnb, .start_row, .tabs)
        #   openxlsx::addStyle(wb, .sheetnb, style = last_row_no_total_bottomleft_cell, gridExpand = T,
        #            rows = .start_row + nrow(.tabs) + 1, cols = 1))
      }
    }

    #Total rows (for column percentages) :
    if (any(purrr::map_lgl(another_total_rows_var, ~ . == 1)) ) {
      maplist_tot_row <- list(sheetnb, start_row, tabs, nbrow_tot) %>%
        purrr::map(~ purrr::keep(., another_total_rows_var == 1))
      purrr::pwalk(append(maplist_tot_row,
                          list(purrr::map(another_totrow_tabs[another_total_rows_var == 1],
                                          ~ dplyr::select(., -1)))),
                   function(.sheetnb, .start_row, .tabs, .nbrow_tot, .tots)
                     openxlsx::writeData(wb, .sheetnb, .tots, colNames = FALSE,
                                         startRow = .start_row + nrow(.tabs) + .nbrow_tot,
                                         startCol = 2))
      purrr::pwalk(maplist_tot_row,
                   function(.sheetnb, .start_row, .tabs, .nbrow_tot)
                     openxlsx::writeData(wb, .sheetnb,
                                         stringi::stri_unescape_unicode("Fr\u00e9quences par colonne"),
                                         colNames = FALSE,
                                         startRow = .start_row + nrow(.tabs) + .nbrow_tot,
                                         startCol = 1))
      purrr::pwalk(maplist_tot_row,
                   function(.sheetnb, .start_row, .tabs, .nbrow_tot)
                     openxlsx::addStyle(wb, .sheetnb, style = col_another_total_left , gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_tot,
                                        cols = 1))
      purrr::pwalk(maplist_tot_row,
                   function(.sheetnb, .start_row, .tabs, .nbrow_tot)
                     openxlsx::addStyle(wb, .sheetnb, style = col_another_row , gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_tot,
                                        cols = 2:ncol(.tabs)))

      if (any(purrr::map_lgl(another_total_rows_var, ~ . == 1) & totc)) {
        maplist_tot_row_totc <- list(sheetnb, start_row, tabs, nbrow_tot) %>%
          purrr::map(~ purrr::keep(., another_total_rows_var == 1 & totc))
        purrr::pwalk(maplist_tot_row_totc,
                     function(.sheetnb, .start_row, .tabs, .nbrow_tot)
                       openxlsx::addStyle(wb, .sheetnb, style = col_another_total_right, gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + .nbrow_tot,
                                          cols = ncol(.tabs)))
      }
      if (any(purrr::map_lgl(another_total_rows_var, ~ . == 1) & !totc)) {
        maplist_tot_row_no_totc <- list(sheetnb, start_row, tabs, nbrow_tot) %>%
          purrr::map(~ purrr::keep(., another_total_rows_var == 1 & !totc))
        purrr::pwalk(maplist_tot_row_no_totc,
                     function(.sheetnb, .start_row, .tabs, .nbrow_tot)
                       openxlsx::addStyle(wb, .sheetnb, style = no_total_row_col_another_total_right, gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + .nbrow_tot,
                                          cols = ncol(.tabs)))
      }
    }



    #Supplementary ratios :
    #Columns vars
    if (any(purrr::map_lgl(sup_cols_var, ~ . >= 1 ))) {
      maplist_sup_col <- list(sheetnb, start_row, tabs, nbcol_sup, nbcsup) %>%
        purrr::map(~ purrr::keep(., sup_cols_var != 0))
      maplist_sup_col_headers <- list(sheetnb, start_row, tabs, nbcol_sup, nbcsup) %>%
        purrr::map(~ purrr::keep(., sup_cols_var != 0 & !hd_remove))
      maplist_sup_col_headers_remove <- list(sheetnb, start_row, tabs, nbcol_sup, nbcsup) %>%
        purrr::map(~ purrr::keep(., sup_cols_var != 0 & hd_remove))

      purrr::pwalk(append(maplist_sup_col,
                          list(purrr::map(sup_cols_tabs[sup_cols_var >= 1],
                                          ~ dplyr::select(., -1)))),
                   function(.sheetnb, .start_row, .tabs, .nbcol_sup, .nbcsup, .sup_cols) #No need .nbcsup
                     openxlsx::writeData(wb, .sheetnb, .sup_cols, colNames = TRUE,
                                         startRow = .start_row + 1,
                                         startCol = ncol(.tabs) + .nbcol_sup))
      purrr::pwalk(maplist_sup_col_headers_remove,
                   function(.sheetnb, .start_row, .tabs, .nbcol_sup, .nbcsup)
                     openxlsx::deleteData(wb, .sheetnb,  row = .start_row + 1, gridExpand = TRUE,
                                          col = ncol(.tabs) + .nbcol_sup + .nbcsup))
      purrr::pwalk(maplist_sup_col_headers,
                   function(.sheetnb, .start_row, .tabs, .nbcol_sup, .nbcsup)
                     openxlsx::addStyle(wb, .sheetnb, style = col_sup_top, gridExpand = T,
                                        rows = .start_row + 1,
                                        cols = ncol(.tabs) + .nbcol_sup + .nbcsup))
    }
    #Column vars, qualitative
    if (any(purrr::map_lgl(sup_cols_quali_var, ~ length(.) > 0))) {
      maplist_sup_col_quali <- list(sheetnb, start_row, tabs, nbcol_sup, sup_cols_quali_var) %>%
        purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_quali_var, ~ length(.) > 0)))
      purrr::pwalk(maplist_sup_col_quali,
                   function(.sheetnb, .start_row, .tabs, .nbcol_sup, .quali)
                     openxlsx::addStyle(wb, .sheetnb, style = col_sup_quali_col, gridExpand = T,
                                        rows = .start_row + 2:nrow(.tabs),
                                        cols = ncol(.tabs) + .nbcol_sup + .quali - 1))

      if (any(purrr::map_lgl(sup_cols_quali_var, ~ length(.) > 0) & totr)) {
        maplist_sup_col_quali_totr <- list(sheetnb, start_row, tabs, nbcol_sup, sup_cols_quali_var) %>%
          purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_quali_var, ~ length(.) > 0) & totr))
        purrr::pwalk(maplist_sup_col_quali_totr,
                     function(.sheetnb, .start_row, .tabs, .nbcol_sup, .quali)
                       openxlsx::addStyle(wb, .sheetnb, style = col_sup_quali_bottom , gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + 1,
                                          cols = ncol(.tabs) + .nbcol_sup + .quali - 1))
      }

      if (any(purrr::map_lgl(sup_cols_quali_var, ~ length(.) > 0) & ! totr)) {
        maplist_sup_col_quali_no_totr <- list(sheetnb, start_row, tabs, nbcol_sup, sup_cols_quali_var) %>%
          purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_quali_var, ~ length(.) > 0) & ! totr))
        purrr::pwalk(maplist_sup_col_quali_no_totr,
                     function(.sheetnb, .start_row, .tabs, .nbcol_sup, .quali)
                       openxlsx::addStyle(wb, .sheetnb, style = no_total_row_col_sup_quali_bottom , gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + 1,
                                          cols = ncol(.tabs) + .nbcol_sup + .quali - 1))
      }
    }
    #Column vars, Quantitative
    if (any(purrr::map_lgl(sup_cols_quanti_var, ~ length(.) > 0))) {
      maplist_sup_col_quanti <- list(sheetnb, start_row, tabs, nbcol_sup, sup_cols_quanti_var) %>%
        purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_quanti_var, ~ length(.) > 0)))

      purrr::pwalk(maplist_sup_col_quanti,
                   function(.sheetnb, .start_row, .tabs, .nbcol_sup, .quanti)
                     openxlsx::addStyle(wb, .sheetnb, style = col_sup_quanti_col, gridExpand = T,
                                        rows = .start_row + 2:nrow(.tabs),
                                        cols = ncol(.tabs) + .nbcol_sup + .quanti - 1))

      if (any(purrr::map_lgl(sup_cols_quanti_var, ~ length(.) > 0) & totr)) {
        maplist_sup_col_quanti_totr <- list(sheetnb, start_row, tabs, nbcol_sup, sup_cols_quanti_var) %>%
          purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_quanti_var, ~ length(.) > 0) & totr))
        purrr::pwalk(maplist_sup_col_quanti_totr,
                     function(.sheetnb, .start_row, .tabs, .nbcol_sup, .quanti)
                       openxlsx::addStyle(wb, .sheetnb, style = col_sup_quanti_bottom , gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + 1,
                                          cols = ncol(.tabs) + .nbcol_sup + .quanti - 1))
      }

      if (any(purrr::map_lgl(sup_cols_quanti_var, ~ length(.) > 0) & ! totr)) {
        maplist_sup_col_quanti_no_totr <- list(sheetnb, start_row, tabs, nbcol_sup, sup_cols_quanti_var) %>%
          purrr::map(~ purrr::keep(., purrr::map_lgl(sup_cols_quanti_var, ~ length(.) > 0) & ! totr))
        purrr::pwalk(maplist_sup_col_quanti_no_totr,
                     function(.sheetnb, .start_row, .tabs, .nbcol_sup, .quanti)
                       openxlsx::addStyle(wb, .sheetnb, style = no_total_row_col_sup_quanti_bottom , gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + 1,
                                          cols = ncol(.tabs) + .nbcol_sup + .quanti - 1))
      }
    }

    if (any(purrr::map_lgl(sup_cols_var, ~ . >= 1 ))) {
      purrr::pwalk(maplist_sup_col_headers_remove,
                   function(.sheetnb, .start_row, .tabs, .nbcol_sup, .nbcsup)
                     openxlsx::addStyle(wb, .sheetnb, style = phantom_headers, gridExpand = T,
                                        rows = .start_row + 1,
                                        cols = ncol(.tabs) + .nbcol_sup + .nbcsup))
    }

    #Line vars :
    if (any(purrr::map_lgl(sup_rows_var, ~ . >= 1 ))) {
      maplist_sup_row <- list(sheetnb, start_row, tabs, nbrow_sup, nbrsup) %>%
        purrr::map(~ purrr::keep(., sup_rows_var!= 0))
      purrr::pwalk(append(maplist_sup_row, list(sup_rows_tabs[sup_rows_var >= 1])),
                   function(.sheetnb, .start_row, .tabs, .nbrow_sup, .nbrsup, .sup_rows) #Pas besoin .nbrsup
                     openxlsx::writeData(wb, .sheetnb, .sup_rows, colNames = FALSE,
                                         startRow = .start_row + nrow(.tabs) + .nbrow_sup,
                                         startCol = 1))
      purrr::pwalk(maplist_sup_row,
                   function(.sheetnb, .start_row, .tabs, .nbrow_sup, .nbrsup)
                     openxlsx::addStyle(wb, .sheetnb, style = row_sup_left, gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_sup + .nbrsup,
                                        cols = 1))
      purrr::pwalk(maplist_sup_row,
                   function(.sheetnb, .start_row, .tabs, .nbrow_sup, .nbrsup)
                     openxlsx::addStyle(wb, .sheetnb, style = row_sup_quali_row , gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_sup + .nbrsup,
                                        cols = 2:(ncol(.tabs)-1)))

      if (any(purrr::map_lgl(sup_rows_var, ~ . >= 1 ) & totc)) {
        maplist_sup_row <- list(sheetnb, start_row, tabs, nbrow_sup, nbrsup) %>%
          purrr::map(~ purrr::keep(., sup_rows_var!= 0 & totc))
        purrr::pwalk(maplist_sup_row,
                     function(.sheetnb, .start_row, .tabs, .nbrow_sup, .nbrsup)
                       openxlsx::addStyle(wb, .sheetnb, style = row_sup_quali_right , gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + .nbrow_sup + .nbrsup,
                                          cols = ncol(.tabs)))
      }

      if (any(purrr::map_lgl(sup_rows_var, ~ . >= 1 ) & !totc)) {
        maplist_sup_row <- list(sheetnb, start_row, tabs, nbrow_sup, nbrsup) %>%
          purrr::map(~ purrr::keep(., sup_rows_var!= 0 & ! totc))
        purrr::pwalk(maplist_sup_row,
                     function(.sheetnb, .start_row, .tabs, .nbrow_sup, .nbrsup)
                       openxlsx::addStyle(wb, .sheetnb, style = no_total_col_row_sup_quali_right , gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + .nbrow_sup + .nbrsup,
                                          cols = ncol(.tabs)))
      }

    }
    #Line vars, quantitative
    if (any(purrr::map_lgl(sup_rows_quanti_var, ~ length(.) > 0))) {
      maplist_sup_row_quanti <- list(sheetnb, start_row, tabs, nbrow_sup, sup_rows_quanti_var) %>%
        purrr::map(~ purrr::keep(., purrr::map_lgl(sup_rows_quanti_var, ~ length(.) > 0)))
      purrr::pwalk(maplist_sup_row_quanti,
                   function(.sheetnb, .start_row, .tabs, .nbrow_sup, .quanti)
                     openxlsx::addStyle(wb, .sheetnb, style = row_sup_quanti_row , gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_sup + .quanti - 1,
                                        cols = 2:(ncol(.tabs)-1)))

      if (any(purrr::map_lgl(sup_rows_quanti_var, ~ length(.) > 0) & totc)) {
        maplist_sup_row_quanti <- list(sheetnb, start_row, tabs, nbrow_sup, sup_rows_quanti_var) %>%
          purrr::map(~ purrr::keep(., purrr::map_lgl(sup_rows_quanti_var, ~ length(.) > 0) & totc))
        purrr::pwalk(maplist_sup_row_quanti,
                     function(.sheetnb, .start_row, .tabs, .nbrow_sup, .quanti)
                       openxlsx::addStyle(wb, .sheetnb, style = row_sup_quanti_right , gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + .nbrow_sup + .quanti - 1,
                                          cols = ncol(.tabs)))
      }
      if (any(purrr::map_lgl(sup_rows_quanti_var, ~ length(.) > 0) & !totc)) {
        maplist_sup_row_quanti <- list(sheetnb, start_row, tabs, nbrow_sup, sup_rows_quanti_var) %>%
          purrr::map(~ purrr::keep(., purrr::map_lgl(sup_rows_quanti_var, ~ length(.) > 0) & !totc))
        purrr::pwalk(maplist_sup_row_quanti,
                     function(.sheetnb, .start_row, .tabs, .nbrow_sup, .quanti)
                       openxlsx::addStyle(wb, .sheetnb, style = no_total_col_row_sup_quanti_right , gridExpand = T,
                                          rows = .start_row + nrow(.tabs) + .nbrow_sup + .quanti - 1,
                                          cols = ncol(.tabs)))
      }
    }


    #Contributions of rows and cols to variance :
    if (any(purrr::map_lgl(contrib_var, ~ . == 1)) ) {
      maplist_contrib_col <- list(sheetnb, start_row, tabs, nbcol_ct) %>%
        purrr::map(~ purrr::keep(., contrib_var == 1))
      maplist_contrib_col_headers <- list(sheetnb, start_row, tabs, nbcol_ct) %>%
        purrr::map(~ purrr::keep(., contrib_var == 1 & !hd_remove))
      maplist_contrib_col_headers_remove <- list(sheetnb, start_row, tabs, nbcol_ct) %>%
        purrr::map(~ purrr::keep(., contrib_var == 1 & hd_remove))

      #Ctr rows (in cols) :
      purrr::pwalk(append(maplist_contrib_col,
                          list(contrib_tabs_totcol[contrib_var == 1])),
                   function(.sheetnb, .start_row, .tabs, .nbcol_ct, .contrib_col)
                     openxlsx::writeData(wb, .sheetnb, .contrib_col, colNames = FALSE,
                                         startRow = .start_row + 2,
                                         startCol = ncol(.tabs) + .nbcol_ct))
      purrr::pwalk(append(maplist_contrib_col_headers,
                          list(purrr::map(varnames[contrib_var == 1 & !hd_remove], ~ .[1]))),
                   function(.sheetnb, .start_row, .tabs, .nbcol_ct, .var1_name)
                     openxlsx::writeData(wb, .sheetnb, stringr::str_c("Contributions de ", .var1_name,
                                                                      " a la variance du tableau"),
                                         colNames = FALSE,
                                         startRow = .start_row + 1,
                                         startCol = ncol(.tabs) + .nbcol_ct))

      purrr::pwalk(maplist_contrib_col_headers,
                   function(.sheetnb, .start_row, .tabs, .nbcol_ct)
                     openxlsx::addStyle(wb, .sheetnb, style = row_contrib_top , gridExpand = T,
                                        rows = .start_row + 1,
                                        cols = ncol(.tabs) + .nbcol_ct))
      purrr::pwalk(maplist_contrib_col_headers_remove,
                   function(.sheetnb, .start_row, .tabs, .nbcol_ct)
                     openxlsx::addStyle(wb, .sheetnb, style = phantom_headers , gridExpand = T,
                                        rows = .start_row + 1,
                                        cols = ncol(.tabs) + .nbcol_ct))
      purrr::pwalk(maplist_contrib_col,
                   function(.sheetnb, .start_row, .tabs, .nbcol_ct)
                     openxlsx::addStyle(wb, .sheetnb, style = row_contrib_column , gridExpand = T,
                                        rows = .start_row + 2:nrow(.tabs),
                                        cols = ncol(.tabs) + .nbcol_ct))
      purrr::pwalk(maplist_contrib_col,
                   function(.sheetnb, .start_row, .tabs, .nbcol_ct)
                     openxlsx::addStyle(wb, .sheetnb, style = row_contrib_bottom , gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + 1,
                                        cols = ncol(.tabs) + .nbcol_ct))

      #Ctr cols (in rows) :
      maplist_contrib_row <- list(sheetnb, start_row, tabs, nbrow_ct) %>%
        purrr::map(~ purrr::keep(., contrib_var == 1))

      purrr::pwalk(append(maplist_contrib_row,
                          list(purrr::map(contrib_tabs_totrow[contrib_var == 1], ~ dplyr::select(., -1) ))),
                   function(.sheetnb, .start_row, .tabs, .nbrow_ct, .contrib_row)
                     openxlsx::writeData(wb, .sheetnb, .contrib_row, colNames = FALSE,
                                         startRow = .start_row + nrow(.tabs) + .nbrow_ct,
                                         startCol = 2))
      purrr::pwalk(append(maplist_contrib_row,
                          list(purrr::map(varnames[contrib_var == 1], ~ .[2]))),
                   function(.sheetnb, .start_row, .tabs, .nbrow_ct, .var2_name)
                     openxlsx::writeData(wb, .sheetnb, stringr::str_c("Contributions de ", .var2_name,
                                                                      " a la variance du tableau"),
                                         colNames = FALSE,
                                         startRow = .start_row + nrow(.tabs) + .nbrow_ct,
                                         startCol = 1))

      purrr::pwalk(maplist_contrib_row,
                   function(.sheetnb, .start_row, .tabs, .nbrow_ct)
                     openxlsx::addStyle(wb, .sheetnb, style = col_contrib_left , gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_ct,
                                        cols = 1))
      purrr::pwalk(maplist_contrib_row,
                   function(.sheetnb, .start_row, .tabs, .nbrow_ct)
                     openxlsx::addStyle(wb, .sheetnb, style = col_contrib_row , gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_ct,
                                        cols = 2:(ncol(.tabs)-1)))
      purrr::pwalk(maplist_contrib_row,
                   function(.sheetnb, .start_row, .tabs, .nbrow_ct)
                     openxlsx::addStyle(wb, .sheetnb, style = col_contrib_right , gridExpand = T,
                                        rows = .start_row + nrow(.tabs) + .nbrow_ct,
                                        cols = ncol(.tabs)))
    }




    # Conditional formatting #######################################################

    #Rules of conditional formatting :
    percentage <- as.list(perc_color_breaks)

    sign1 <- purrr::map(perc_color_breaks, ~ dplyr::if_else(condition = stringr::str_detect(., "^\\+|^\\*"),
                                                            true = ">", false = "<"))

    # Color in white-gray numbers close to 0 (easier to read tables)

    # digits_perc <- 1
    # digits_no_perc <- 0

    purrr::pwalk(list(sheetnb, tabs, start_row),
                 ~ openxlsx::conditionalFormatting(wb, sheet = ..1,
                                                   cols = 2:(ncol(..2) - 1), rows = 2:nrow(..2) + ..3,
                                                   type = "between",
                                                   rule = c(-hide_near_zero, hide_near_zero),
                                                   style = style_zero) )


    # Row percentages :   # B2>(B$T+0,05) # B2<(B$T-0,05)
    rule_colmean_list <-
      purrr::map2(sign1, perc_color_breaks, function(sign1, perc_color_breaks)
        purrr::map2(start_row, tabs, ~
                      stringr::str_c("B", .x + 2, sign1, "(B$", .x + nrow(.y) + 1, perc_color_breaks, ")") ) )

    if (color[1] %in% c("auto", "contrib")) {
      rsc <- purrr::pmap_lgl(list(sup_cols_var, tabs_general_total_row #, totals_tabs
      ), ~ ..1 >= 1 & ..2 == FALSE ) #& "row" %in% ..3
      if (any(rsc) ) {
        rule_sup_cols_list <-
          purrr::map2(sign1, perc_color_breaks, function(sign1, perc_color_breaks)
            purrr::pmap(list(
              col = purrr::map2(tabs, nbcol_sup, ~ stringr::str_c(LETTERS[(ncol(.x) + .y)%/%26],
                                                                  LETTERS[(ncol(.x) + .y)%%26])),
              row = purrr::map(tabs, ~ nrow(.) + 1),
              start_row), function(col, row, start_row)
                stringr::str_c(col, start_row + 2, sign1, "(", col, "$", start_row + row,
                               perc_color_breaks, ")") ) )
      }
    }

    rpv <- purrr::pmap_lgl(list(row_perc_var_rows, row_perc_var_cols,
                                tabs_general_total_row),
                           ~ length(..1) != 0 & length(..2) != 0 & ..3 == FALSE)

    if (any(rpv)) {
      purrr::map2(rule_colmean_list, style_plus_minus_list,
                  function(rule_colmean_list, style_plus_minus_list)
                    purrr::pwalk(list(sheetnb[rpv],
                                      start_row[rpv],
                                      cols = row_perc_var_cols[rpv],
                                      rows = row_perc_var_rows[rpv], # purrr::map(tabs, ~ 2:nrow(.) ),
                                      rule_colmean_list[rpv]),
                                 function(sheetnb, start_row, cols, rows, rule_map)
                                   openxlsx::conditionalFormatting(wb, sheet = sheetnb, cols = cols,
                                                                   rows = start_row + rows, rule = rule_map,
                                                                   style =  style_plus_minus_list) ) )
    }

    if (color[1] %in% c("auto", "contrib")) {
      scv <- purrr::pmap_lgl(list(sup_cols_quali_var, tabs_general_total_row, totals_tabs),
                             ~ length(..1) > 0 & ..2 == FALSE & "row" %in% ..3)
      if (any(scv)) {
        purrr::map2(rule_sup_cols_list, style_plus_minus_list,
                    function (rule_map, style_map)
                      purrr::pwalk(
                        list(sheetnb[scv],
                             start_row[scv],
                             cols = purrr::pmap(list(tabs, nbcol_sup, sup_cols_quali_var), #nbcsup),
                                                ~ ncol(..1) + ..2 + ..3 - 1)[scv],
                             rows = purrr::map(tabs, ~ 2:nrow(.))[scv],
                             rule_map[scv]),
                        ~ openxlsx::conditionalFormatting(wb, sheet = ..1, cols = ..3,
                                                          rows = ..2 + ..4,
                                                          rule = ..5, style = style_map) ) )
      }
    }

    # Column percentages :   # B2>($T2+0,05) # B2<($T2-0,05)
    rule_rowmean_list <-
      purrr::map2(sign1, perc_color_breaks, function(sign1, perc_color_breaks)
        purrr::map2(start_row, tabs,
                    ~ stringr::str_c("B", .x + 2, sign1, "($",
                                     LETTERS[ncol(.y)%/%26], LETTERS[ncol(.y)%%26],
                                     .x + 2, perc_color_breaks, ")") ) )

    if (color[1] %in% c("auto", "contrib")) {
      if (any(purrr::map2_lgl(sup_rows_var, tabs_general_total_row,
                              ~ .x >= 1 & .y == FALSE))) {
        rule_sup_rows_list <-
          purrr::map2(sign1, perc_color_breaks, function(sign1, perc_color_breaks)
            purrr::pmap(list(
              row = purrr::map2(tabs, nbrow_sup, ~ nrow(.x) + .y),
              col = purrr::map(tabs, ~  stringr::str_c(LETTERS[ncol(.)%/%26], LETTERS[ncol(.)%%26])),
              start_row), function(row, col, start_row)
                stringr::str_c("B", start_row + row, sign1, "($", col, start_row + row,
                               perc_color_breaks, ")") ) )
      }
    }

    cpv <- purrr::pmap_lgl(list(col_perc_var_rows, col_perc_var_cols,
                                tabs_general_total_row),
                           ~ length(..1) != 0 & length(..1) != 0 & ..3 == FALSE)

    if (any(cpv)) {
      purrr::map2(rule_rowmean_list, style_plus_minus_list,
                  function (rule_map, style_map)
                    purrr::pwalk(
                      list(sheetnb[cpv],
                           start_row[cpv],
                           cols = col_perc_var_cols[cpv],
                           rows = col_perc_var_rows[cpv], #purrr::map(col_perc_var_rows, ~ . + 1)[cpv],
                           rule_map[cpv]),
                      ~ openxlsx::conditionalFormatting(wb, sheet = ..1, cols = ..3,
                                                        rows = ..2 + ..4,
                                                        rule = ..5, style = style_map) ) )
    }

    if (color[1] %in% c("auto", "contrib")) {
      # if (any(purrr::map_lgl(sup_rows_var, ~ . >= 1 ))) {
      #   srv <- purrr::map_lgl(sup_rows_var, ~ . >= 1 )
      srv <- purrr::pmap_lgl(list(sup_rows_quali_var, tabs_general_total_row, totals_tabs),
                             ~ length(..1) > 0 & ..2 == FALSE  & "col" %in% ..3)
      if (any(srv)) {
        purrr::map2(rule_sup_rows_list, style_plus_minus_list,
                    function (rule_map, style_map)
                      purrr::pwalk(
                        list(sheetnb[srv],
                             start_row[srv],
                             cols = purrr::map(tabs, ~ 2:(ncol(.) - 1) )[srv],
                             rows = purrr::pmap(list(tabs, nbrow_sup, sup_rows_quali_var), #nbrsup),
                                                ~ nrow(..1) + ..2 + ..3 - 1)[srv],
                             rule_map[srv]),
                        ~ openxlsx::conditionalFormatting(wb, sheet = ..1, cols = ..3, rows = ..2 + ..4,
                                                          rule = ..5, style = style_map) ) )
      }
    }


    #Counts and % by table : contribution of cells to variance,
    # positive or negative depending on the sign of Chi2 residuals

    ccv <- purrr::pmap_lgl(list(cell_contrib_var_rows, cell_contrib_var_cols,
                                tabs_general_total_row),
                           ~ length(..1) != 0 & length(..2) != 0 & ..3 == FALSE)

    if (any(ccv)) {
      coeff <- as.list(cell_contrib_color_coeffs)
      sign2 <- purrr::map(coeff, ~ dplyr::if_else(condition = . > 0,
                                                  true = ">", false = "<"))
      # sign2 <- purrr::map(coeff, ~ dplyr::if_else(condition = stringr::str_detect(., "^-"),
      # true = "<", false = ">"))
      coeff_pos_inf <- coeff %>% purrr::keep(. > 0)
      coeff_pos_sup <- coeff_pos_inf[-1] %>% append(Inf)
      coeff_neg_sup <- coeff %>% purrr::keep(. < 0)
      coeff_neg_inf <- coeff_neg_sup[-1] %>% append(-Inf)
      coeff_inf <- coeff_pos_inf %>% append(coeff_neg_inf)
      coeff_sup <- coeff_pos_sup %>% append(coeff_neg_sup)
      coeff <- as.character(coeff)
      mean_contrib <- tabs_mean_contrib[ccv]
      # contrib_matrixes <- purrr::pmap(list(contrib_tabs[ccv],
      #                               purrr::map(contrib_tabs[ccv], ~ nrow(.)) ),
      #                          ~ dplyr::select(..1, -Total, -1) %>% dplyr::slice(-..2) )
      nbrows <- contrib_matrixes[ccv] %>% purrr::map(~ 1:nrow(.))
      nbcols <- contrib_matrixes[ccv] %>% purrr::map(~ 1:ncol(.))

      nbcell <-  purrr::map2(nbcols, nbrows, function(.nbcols, .nbrows)
        purrr::map(.nbcols, function(.x)
          purrr::map(.nbrows, function(.y)
            c(.x, .y) ) ) ) %>% purrr::map(~ purrr::flatten(.))

      cell_contrib <-
        purrr::map2(nbcell, contrib_matrixes[ccv], function(.nbcell, .mat)
          purrr::map(.nbcell, function(..nbcell)
            .mat[..nbcell[2], ..nbcell[1]] ) )

      cell_contrib_selection_list <-
        purrr::map2(coeff_inf, coeff_sup, function(.coeff_inf, .coeff_sup)
          purrr::map2(cell_contrib, mean_contrib, function(.cell_contrib, .mean_contrib)
            purrr::map_lgl(.cell_contrib, function(..cell_contrib)
              ..cell_contrib >= .coeff_inf * .mean_contrib &
                ..cell_contrib < .coeff_sup * .mean_contrib ) %>% which() ) ) %>%
        magrittr::set_names(coeff)



      # cell_contrib_rules_list <-
      #   purrr::map2(sign2, coeff, function(sign2, coeff)
      #     purrr::pmap(list(nbcell, cell_contrib, mean_contrib), function (.nbcell, .cell_contrib, .mean_contrib)
      #       purrr::map2(.nbcell, .cell_contrib, function(..nbcell, ..cell_contrib)
      #         purrr::map2_chr(stringr::str_c(LETTERS[(..nbcell[1] + 1)%/%26],
      #                        LETTERS[(..nbcell[1] + 1)%%26], ..nbcell[2] + 1),
      #                  ..cell_contrib,
      #                  function(...nbcell, ...cell_contrib)
      #                    stringr::str_c(...nbcell, "-", ...nbcell, "+",
      #                          ...cell_contrib, sign2, coeff, "*", .mean_contrib) )
      #       ) ) )

      nbcell_with_startrow <- purrr::map2(nbcell, start_row[ccv], ~ purrr::map(.x, function(.int) .int + c(0, .y)))

      cell_contrib_rules_list <-
        purrr::map2(sign2, coeff, function(sign2, coeff)
          purrr::pmap(list(nbcell_with_startrow, cell_contrib, mean_contrib), function (.nbcell, .cell_contrib, .mean_contrib)
            purrr::map2(.nbcell, .cell_contrib, function(..nbcell, ..cell_contrib)
              purrr::map2_chr(stringr::str_c(LETTERS[(..nbcell[1] + 1)%/%26],
                                             LETTERS[(..nbcell[1] + 1)%%26], ..nbcell[2] + 1),
                              ..cell_contrib,
                              function(...nbcell, ...cell_contrib)
                                stringr::str_c(...nbcell, "-", ...nbcell, "+",
                                               ...cell_contrib, sign2, coeff, "*", .mean_contrib) )
            ) ) )

      rules_selection <-  cell_contrib_rules_list %>%
        purrr::map2(cell_contrib_selection_list, ~ purrr::map2(.x, .y, ~ .x[.y] ))

      nbcell_selection <- purrr::map(cell_contrib_selection_list,
                                     ~ purrr::map2(nbcell, ., ~ .x[.y] ))

      purrr::pmap(list(rules_selection, nbcell_selection,
                       style_plus_minus_cell_contrib_list),
                  function (rule_map, cell_map, style_map)
                    purrr::pmap(list(sheetnb[ccv], start_row[ccv], cell_map, rule_map),
                                function(.sheetnb, .sr, .cell_map, .rule_map)
                                  purrr::walk2(.cell_map, .rule_map, function(..cell_map, ..rule_map)
                                    openxlsx::conditionalFormatting(wb, sheet = .sheetnb, cols = ..cell_map[1] + 1,
                                                                    rows = .sr + ..cell_map[2] + 1,
                                                                    rule = ..rule_map, style = style_map)
                                  ) ) )

      # no_perc_cell_contrib_list <-
      #   purrr::map2(sign2, coefficient, function(sign2, coefficient)
      #     purrr::pmap(list(col = purrr::map(tabs, ~ LETTERS[ncol(.)])[npv],
      #             col1 = purrr::map(tabs, ~ LETTERS[ncol(.)-1])[npv],
      #             row = purrr::map(tabs, ~ nrow(.))[npv],
      #             start_row[npv],
      #             nbrow_vari[npv]),
      #        function(col, col1, row, sr, vari)
      #          stringr::str_c("B", sr + 2, sign2,  "(B$", sr + row + 1, "*$", col, sr + 2,
      #                "/$", col, "$", sr + row + 1, ")*(1", coefficient, "*$", col,
      #                "$", sr + row + vari,  "/(RACINE((B", sr + 2, "/$", col, "$",
      #                sr + row + 1, "-(B$", sr + row + 1, "*$", col, sr + 2,")/($",
      #                col, "$", sr + row + 1, "^2))^2)*NBVAL($B$", sr + 2,":$", col1,
      #                "$", sr + row, ")))") ) )
      #
      #   purrr::map2(no_perc_cell_contrib_list, style_plus_minus_cell_contrib_list,
      #        function (rule_map, style_map)
      #          purrr::pwalk(
      #            list(sheetnb[npv],
      #                 start_row[npv],
      #                 cols = no_perc_var_cols[npv],
      #                 rows = no_perc_var_rows[npv], #purrr::map(all_perc_var_rows, ~ . + 1)[npv],
      #                 rule_map[npv]),
      #            ~ openxlsx::conditionalFormatting(wb, sheet = ..1, cols = ..3,
      #                                    rows = ..2 + ..4,
      #                                    rule = ..5, style = style_map) ) )
    }


    # apv <- purrr::map2_lgl(all_perc_var_rows, all_perc_var_cols,
    #                 ~ length(.x) != 0 & length(.y) != 0)
    # #Frequences : idem, contributions of cells to variance
    # if (any(apv)) {
    #   all_perc_cell_contrib_list <-
    #     purrr::map2(sign2, coefficient, function(sign2, coefficient)
    #       purrr::pmap(list(col = purrr::map(tabs, ~ LETTERS[ncol(.)])[apv],
    #                 col1 = purrr::map(tabs, ~ LETTERS[ncol(.)-1])[apv],
    #                 row = purrr::map(tabs, ~ nrow(.))[apv],
    #                 start_row[apv],
    #                 nbrow_vari[apv]),
    #            function(col, col1, row, sr, vari)
    #              stringr::str_c("B", sr + 2, sign2, "B$", sr + row + 1, "*$", col, sr + 2,
    #                    "*(1", coefficient, "*$", col, "$", sr + row + vari,
    #                    "/(RACINE((B", sr + 2, "-B$", sr + row +1, "*$", col,
    #                    "2)^2)*NBVAL($B$", sr + 2,":$", col1, "$", sr + row, ")))") ) )
    #
    #   purrr::map2(all_perc_cell_contrib_list, style_plus_minus_cell_contrib_list,
    #        function (rule_map, style_map)
    #          purrr::pwalk(
    #            list(sheetnb[apv],
    #                 start_row[apv],
    #                 cols = all_perc_var_cols[apv],
    #                 rows = all_perc_var_rows[apv], #purrr::map(all_perc_var_rows, ~ . + 1)[apv],
    #                 rule_map[apv]),
    #            ~ openxlsx::conditionalFormatting(wb, sheet = ..1, cols = ..3,
    #                                    rows = ..2 + ..4,
    #                                    rule = ..5, style = style_map) ) )
    # }
    if (stringr::str_detect(path, "\\\\|/")) {
      dir_path <- path %>% stringr::str_remove("\\\\[^\\\\]+$|/[^/]+$")
      if (! dir.exists(dir_path))  dir.create(dir_path, recursive = TRUE)
    }
    path_name <- stringr::str_remove(path, "\\.xlsx$")
    if (! stringr::str_detect(path, "\\.xlsx$")) path <-
      stringr::str_c(path, ".xlsx")
    if (replace == FALSE) {
      i <- 0
      file_do_not_exist <- FALSE
      while (file_do_not_exist == FALSE) {
        if (file.exists(path)) {
          i = i+1
          path <- stringr::str_c(path_name, i, ".xlsx")
        } else {
          path <-
            stringr::str_c(path_name, dplyr::if_else(i == 0,
                                                     "",
                                                     stringr::str_c(i)),
                           ".xlsx")
          file_do_not_exist <- TRUE
        }
      }
    }
    print(path)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    if (open == TRUE) { file.show(path) }
  }


#tabs %>% tab_xl(compact = TRUE, color = "contrib")
