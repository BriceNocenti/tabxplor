#Fonctions user-friendly tableaux croises---------------------------------------


#Fonction tabw : tableau croise pondere compatible tidyverse
# Ajouter  : - plusieurs tableaux si plusieurs variables sont indiquees (y compris wt) ?
#            - une fonction tout-en-un pour les etudiants, une autre decomposee pour les utilisateurs du tidyverse ?
#            - faire des wrappers (versions : normal ; tout est simplifie ; resultats ?)
#            - Autres fonctions ? : - soustraire moyenne de la colonne/ligne
#                                   - intervalles de confiance
#                                   - passer sup_contrib, etc., à l'extérieur,
# Ajouter ?
#            - regles de formatage conditionnel a passer dans tabxl ?
#            - choisir signe pourcentages ou pas dans le format
# BUGS       - Format de sortie pour les effectifs : double not decimal (ACM output) ?
#            - Enlever design effect ?
#            - use stopifnot and switch ----------------------------------------------------------
#            - make perc faster, compared to counts ----------------------------





#' Crosstabs
#' @description A full-featured function to create, manipulate and print
#'  crosstabs. You can then modify tables with any functions
#'  using \code{\link{tab_map}}.
#' @param data A data frame.
#' @param var1,var2,var3 \code{var1} is the row variable. \code{var2} is the
#' column variable. A table is made for each level in \code{var3} :
#' leave this table variable empty to make a simple crosstab. All variables
#' will be converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted
#'  results.
#' @param show_na Set to \code{FALSE} to exclude individuals with \code{NA} at
#'  \code{var1}, \code{var2} or \code{var3}.
#' @param keep_unused_levels Set to \code{TRUE} to keep empty levels of factors.
#' @param sup_cols,sup_rows,multicols,multirows,print_sup A character vector of
#'  supplementary column variables, or supplementary row variables. They can be
#'  factor/character, but also numeric (in which case a
#'  \code{\link[stats]{weighted.mean}} is calculated for all categories
#'  of \code{var1}/\code{var2}). \code{sup_cols} and \code{sup_rows} won't
#'  be printed in the results unless :
#'  \itemize{
#'   \item you pass the tabs to \code{\link{tabxl}}
#'   \item you set \code{multicols} OR \code{multirows} to \code{TRUE} :
#'   it draws a table with only \code{var1} for \code{sup_cols},
#'   or with only \code{var2} for \code{sup_rows}. For more simplicity
#'   use \code{\link{tabmulti}}
#'   \item you set print_sup to \code{TRUE} : it draws a table with all
#'   principals AND supplementary variables (confusing and not recommended).
#' }
#' @param only_first_level,not_last_level By default, only the first
#'  level of each supplementary variable is printed : it works well when there
#'  is only two levels. When \code{only_first_level} is set to \code{FALSE},
#'  the last level of each variable is still removed, because it usually brings
#'  no more information. To take advantage of this feature,
#'  use \code{\link[forcats]{fct_relevel}}
#'  to place "negative" levels (expressing the lack of something) at the end.
#'  To print all levels, also set \code{not_last_level} to \code{FALSE}.
#' @param drop_sup_na Set to \code{TRUE} to remove all individuals
#' with \code{NA} in at least  one supplementary variable.
#' @param tot \code{"auto"} prints all totals, unless it is a one-variable
#'  table. \code{"no"} remove them all, \code{"row"} add a total row, \code{"col"}
#'  a total column, and \code{"all"} and \code{c("row", "col")} stand for both.
#' @param perc Type of percentages. \code{"no"}, \code{"row"} and \code{"col"}
#' are self-evident. \code{"all"} print frequencies for each table.
#' If \code{var3} is provided, \code{"all_tabs"} calculate the proportion
#' of each value in the total population.
#' @param totaltab Only useful when \code{var3} is provided. \code{"table"}
#'  means that a totaltable will be created. \code{"line"} means it will be
#'  reduced to a single row. \code{"no"}  means it will be totally removed.
#' @param digits The number of digits to print, as an integer.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like "1-", and text in parenthesis.
#' @param subtext A character vector to print legend rows under the
#' (list of) table(s) in \code{\link{tabxl}}
#' @param sort_by A variable to sort rows in each table with. It must be one
#'  of the principal or supplementary column variables.
#' @param minimum_headcount The minimum unweighted count in each row/col.
#'  Any row/col with less count will be printed in grey in \code{\link{tabxl}}.
#' @param rare_levels_to_other When set to \code{TRUE}, levels with less count
#' than minimum_headcount will be merged into an "Other" level.
#' @param another_total Set to \code{TRUE} to add another total line.
#' Useful to compare row frequencies with \code{perc = "row"}, or columns
#' frequencies with \code{perc = "col"}. It only prints when you pass tabs
#' into \code{\link{tabxl}}.
#' @param sup_contrib Add a column and a row with the contribution of all levels
#'  to the variance of the tabs. It only prints when you pass tabs
#'  into \code{\link{tabxl}}.
#' @param result Any intermediate result in the calculation of Chi2/variance
#'  can be printed :
#'   \itemize{
#'    \item \code{"observed"} : base table with counts or frequencies
#'    \item \code{"contrib"} : contribution of cells to variance, with signs + or -
#'     for attractions and repulsions
#'    \item \code{"expected"} : expected frequencies
#'    \item \code{"spread"} : spread between observed and expected frequencies
#'    \item \code{"binding_ratio"} : spread divided by expected frequencies
#'    \item \code{"ctr_abs"} : absolute contributions of cells to variance
#'    \item \code{"ctr_no_sign"} : relative contributions of cells to variance
#'  }
#' @param confidence_intervals,conf_level,design_effect Set
#' \code{confidence_intervals = TRUE} to print tables with confidence intervals
#'  (for the selected kind of percentages). \code{conf_level} defines a
#'  confidence level (number between 0 and 1). Set \code{design_effect = TRUE}
#'  to increase the confidence intervals, depending on how the \code{wt}
#'   weight variable distort the structure of data.
#' @param force_unique_table If \code{var3} is provided, set to
#'  \code{TRUE} to print multiple tables into one.
# @param print_sup If any sup_cols or sup_rows are provided, set to \code{TRUE} to
#  print them in the main table, without having to use \code{\link{tabxl}}.
# @param multicols Set to \code{TRUE} to print the table(s) of var1 and sup_cols.
# @param multirows Set to \code{TRUE} to print the table(s) of var2 and sup_rows.
#' @param accelerate If \code{TRUE} makes the function faster, but produces
#'  less metadata (Chi2, unweighted counts, contributions of cells to variance).
#  @param ...
#'
#' @return If \code{var3} is empty, a single table with
#' class \code{\link{single_tabr}}, which is a special
#'  \code{\link[tibble]{tibble}} with adapted printing method.
#'  If \code{var3} is provided, a list of single tables, with class
#'   \code{\link{tabr}} (a list of \code{\link[tibble]{tibble}}).
#'   The variables classes depend on the chosen parameters (\code{\link{pct}},
#'   \code{double}, \code{character}). You can then modify tables with
#'    any functions using \code{\link{tab_map}}.
#' @export
#'
#' @examples
#' tabw(forcats::gss_cat, marital, race)
#'
#' tabw(forcats::gss_cat, marital, race, perc = "row")
#'
#' tabw(forcats::gss_cat, marital, race, year, perc = "row")
#'
#' tabw(dplyr::storms, status, category, sup_rows = c("pressure", "wind"),
#'      print_sup = TRUE)
#'
#' \dontrun{
#' forcats::gss_cat %>%
#'   tabw(marital, race, perc = "row") %>%
#'   tabxl()
#' }
tabw <-
  function(data, var1, var2, var3, wt, show_na = TRUE, keep_unused_levels = FALSE,
           sup_cols = NULL, sup_rows = NULL, only_first_level = TRUE, not_last_level = TRUE, drop_sup_na = FALSE,
           tot = "auto", perc = c("no", "row", "col", "all", "all_tabs"), totaltab = c("table", "line", "no"),
           digits = 0, cleannames = FALSE, subtext, sort_by = "no",
           minimum_headcount = 30, rare_levels_to_other = FALSE,
           another_total = FALSE, sup_contrib = FALSE,
           result = c("observed", "contrib", "expected", "spread", "binding_ratio", "ctr_abs", "ctr_no_sign"),
           confidence_intervals = FALSE, conf_level = 0.95, design_effect = TRUE, #CItype = c("range", "+-"),
           force_unique_table = FALSE, print_sup = FALSE, no_formatting = FALSE,
           multicols = FALSE, multirows = FALSE, accelerate = FALSE, ...) {



    # if ( is.data.frame(data) | ! is_tabr(data)) {
    #   original_data <- data
    #   original_tablist <- NULL

    data_name <- substitute(data) %>% as.character() %>% .[length(.)]
    if (data_name == ".") { #Find original name if data has been piped.
      data_name <- get_orig_name(data)
      data_name <- stringr::str_remove(data_name, "^[^\\$]+\\$")
    }
    # }

    # if (is_tabr(data) | all(purrr::map_lgl(data, ~ is_tabr(.)))) {
    #   # if (force_unique_table == TRUE){
    #   #   stop("Cannot use tabw with pipe/from tab with force_unique_table == TRUE") }
    #   original_data <- NULL
    #   original_tablist <- data
    #   if (is_tabr(data)) {
    #     original_args <- purrr::pluck(data, purrr::attr_getter("args"))
    #   } else {
    #     original_args <- purrr::pluck(data[[1]], purrr::attr_getter("args"))
    #   }
    #
    #   if (missing(wt) & !is.na(original_args$wt) ) wt <-         original_args$wt
    #   if (missing(perc) )                perc <-                 original_args$perc
    #   if (missing(show_na) )             show_na <-              original_args$show_na
    #   if (missing(keep_unused_levels) )  keep_unused_levels <-   original_args$keep_unused_levels
    #   if (missing(minimum_headcount) )   minimum_headcount <-    original_args$minimum_headcount
    #   if (missing(tot) )                 tot <-                  original_args$totals
    #   if (missing(totaltab) )            totaltab <-             original_args$totaltab
    #   if (missing(digits) )              digits <-               original_args$digits
    #   if (missing(cleannames) )          cleannames <-           original_args$cleannames
    #   if (missing(rare_levels_to_other)) rare_levels_to_other <- original_args$rare_levels_to_other
    #   # if (missing(sup_cols) )            sup_cols <-             original_args$sup_cols
    #   # if (missing(sup_rows) )            sup_rows <-             original_args$sup_rows
    #   if (missing(only_first_level) )    only_first_level <-     original_args$only_first_level
    #   if (missing(drop_sup_na) )         drop_sup_na <-          original_args$drop_sup_na
    #   if (missing(another_total) )       another_total <-        original_args$another_total
    #   #if (missing(sup_contrib) )         sup_contrib <-          original_args$sup_contrib
    #   # if (missing(confidence_intervals)) confidence_intervals <- original_args$confidence_intervals
    #   if (missing(conf_level) )          conf_level <-           original_args$conf_level
    #   if (missing(subtext) )             subtext <-              original_args$subtext
    #
    #   data <- original_args$original_data
    #   data_name <- original_args$original_data_name
    #   # when ".", find all databases, then test names and length ?
    #   # ls()[purrr::map_lgl(ls(), ~ any(class(get(.)) == "data.frame"))]
    #   # data <- get(data_name)
    #    }
    # if (!("data.frame" %in% class(data)) & ! is_tabr(data)) stop("data is of the wrong type")


    if (missing(var1) & missing(var2)) stop("var1 or var2 needed")

    no_var1 <- missing(var1)
    no_var2 <- missing(var2)
    no_var3 <- missing(var3)
    no_weight <- missing(wt)

    # var1 <- rlang::enquo(var1)
    # var2 <- rlang::enquo(var2)
    # var3 <- rlang::enquo(var3)
    # wt <- rlang::enquo(wt)

    #"Auto" settings, options incompatibilities and warnings
    stopifnot(class(conf_level) == "numeric",  conf_level > 0, conf_level < 1)

    if (tot[1] == "all") tot <- c("row", "col")
    if (tot[1] == "auto" & no_var2 == TRUE ) tot <- c("row")
    if (tot[1] == "auto" & no_var1 == TRUE ) tot <- c("col")
    if (tot[1] == "auto" & no_var2 == FALSE & no_var2 == FALSE ) tot <- c("row", "col")

    if ( (no_var1 == TRUE | no_var2 == TRUE ) & result[1] != "observed" ) {
      result <- "observed"
      warning("since there is no first or second variable, result was set to 'observed")
    }
    if (perc[1] != "all" & result[1] %in% c("spread", "binding_ratio", "contrib", "ctr_abs", "ctr_no_sign")) {
      perc <- "all"
      warning("since result is not 'observed' or 'expected', percentages were set to 'all")
    }

    if (confidence_intervals[1] > 0 & result[1] != "observed") {
      #result <- "observed"
      warning("confidence intervals can only be shown with result = 'observed'")
    }

    if (missing(subtext) ) subtext <- NA_character_

    if (!missing(sort_by) & (class(sort_by) != "character" | length(sort_by) > 2 |
                             ifelse(is.na(sort_by[2] != "desc"), FALSE, sort_by[2] != "desc") )
    ) stop("sort_by must be a character vector of length 1 (or 2 with 'desc')")

    if (multicols == TRUE & multirows == TRUE) stop("cannot print multicols and multirows at the same time")
    if (multicols == TRUE & length(sup_cols) == 0) stop("to use multicols, you must fill in sup_cols (character vector of variables names)")
    if (multirows == TRUE & length(sup_rows) == 0) stop("to use multirows, you must fill in sup_rows (character vector of variables names)")

    if (multicols == TRUE | multirows == TRUE) {
      tot <- c("row", "col")
      print_sup <- FALSE
      another_total <- FALSE
      sup_contrib   <- FALSE
      if (multicols == TRUE) perc <- "row"
      if (multirows == TRUE) perc <- "col"
    }

    if (no_var1) {
      data %<>% dplyr::mutate(no_var1 = factor("n"))
      var1 <- rlang::expr(no_var1)
    } else {
      var1 <- rlang::enquo(var1) %>% rlang::quo_set_expr(rlang::quo_name(.) %>% stringr::str_remove_all('"') %>% rlang::sym())
    }

    if (no_var2) {
      data %<>% dplyr::mutate(no_var2 = factor("n"))
      var2 <- rlang::expr(no_var2)
    } else {
      var2 <- rlang::enquo(var2) %>% rlang::quo_set_expr(rlang::quo_name(.) %>% stringr::str_remove_all('"') %>% rlang::sym())
    }

    if (no_var3) {
      data %<>% dplyr::mutate(no_var3 = factor(" "))
      var3 <- rlang::expr(no_var3)
    } else {
      var3 <- rlang::enquo(var3) %>% rlang::quo_set_expr(rlang::quo_name(.) %>% stringr::str_remove_all('"') %>% rlang::sym())
    }

    if (no_weight) {
      data %<>% dplyr::mutate(no_weight = 1)
      wt <- rlang::expr(no_weight)
    } else {
      wt <- rlang::enquo(wt) %>% rlang::quo_set_expr(rlang::quo_name(.) %>% stringr::str_remove_all('"') %>% rlang::sym())
    }

    dat <- data %>% dplyr::select(!!var1, !!var2, !!var3, !!wt, tidyselect::all_of(sup_cols), tidyselect::all_of(sup_rows))


    tabr_main(dat = dat, data_name = data_name, #original_data = original_data, original_tablist = original_tablist,
              var1 = !!var1, var2 = !!var2,  var3 = !!var3,  wt = !!wt,  #no_var1 = no_var1, no_var2 = no_var2,no_var3 = no_var3, no_weight = no_weight,
              show_na = show_na, keep_unused_levels = keep_unused_levels,
              tot = tot, perc = perc, totaltab = totaltab,
              digits = digits, cleannames = cleannames, subtext = subtext, sort_by = sort_by,
              minimum_headcount = minimum_headcount, rare_levels_to_other = rare_levels_to_other,
              sup_cols = sup_cols, sup_rows = sup_rows, only_first_level = only_first_level, not_last_level = not_last_level, drop_sup_na = drop_sup_na,
              another_total = another_total, sup_contrib = sup_contrib,
              result = result,
              confidence_intervals = confidence_intervals, conf_level = conf_level, design_effect = design_effect,
              force_unique_table = force_unique_table, print_sup = print_sup,
              multicols = multicols, multirows = multirows, accelerate = accelerate)
  }




#' Multiple crosstabs
#' @description Cross one variable with many others, in colums by default,
#'  or in rows with \code{transpose_table = TRUE}. Another variable can be used
#'  to produce as many crosstabs as it has levels. Wrapper around \code{tabw}.
#'
#' @param data A data frame.
#' @param dependent_var A single dependent variable, which by default is the row
#' variable (see \code{transpose_table}).
#' @param explanatory_vars A character vector of variables to be crossed with
#' \code{dependent_var}. They can be factor/character, but also numeric (in
#' which case a \code{\link[stats]{weighted.mean}} is calculated for each row).
#' @param tab_var A table is made for each level in \code{tab_var} :
#' leave empty to make a simple crosstab.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted
#'  results.
#' @param transpose_table When set to \code{TRUE}, \code{dependent_var} goes
#' in columns and \code{explanatory_vars} in rows.
#' @param only_first_level,not_last_level By default, only the first
#'  level of each supplementary variable is printed : it works well when there
#'  is only two levels. When \code{only_first_level} is set to \code{FALSE},
#'  the last level of each variable is still removed, because it usually brings
#'  no more information. To take advantage of this feature,
#'  use \code{\link[forcats]{fct_relevel}}
#'  to place "negative" levels (expressing the lack of something) at the end.
#'  To print all levels, also set \code{not_last_level} to \code{FALSE}.
#' @param totaltab Only useful when \code{var3} is provided. \code{"table"}
#'  means that a total table will be created. \code{"line"} means it will be
#'  reduced to a single row. \code{"no"}  means it will be totally removed.
#' @param show_na Set to \code{FALSE} to exclude individuals with \code{NA} at
#'  \code{dependent_var} or \code{tab_var}.
#' @param drop_sup_na Set to \code{TRUE} to remove all individuals
#' with \code{NA} in at least one of \code{explanatory_vars}.
#' @param digits The number of digits to print, as an integer.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like "1-", and text in parenthesis.
#' @param subtext A character vector to print legend rows under the
#' (list of) table(s) in \code{\link{tabxl}}
#' @param sort_by A variable to sort rows in each table with. It must be among
#'  \code{explanatory_vars}.
#' @param accelerate If \code{TRUE} makes the function faster, but produces
#'  less metadata (Chi2, unweighted counts, contributions of cells to variance).
# @param ...
#'
#' @return When \code{var3} is empty, a single table with
#' class \code{\link{single_tabr}}, which is a special
#'  \code{\link[tibble]{tibble}} with adapted printing method.
#'  When \code{var3} is provided, a list of single tables, with class
#'   \code{\link{tabr}} (a list of \code{\link[tibble]{tibble}}).
#'   The columns are of class pct, with a names column of
#'   class factor. You can then modify tables with any functions
#'   with \code{\link{tab_map}}.
#' @export
#'
#' @examples
#' tabmulti(dplyr::storms, category, explanatory_vars =  c("pressure", "wind"))
#'
#' tabmulti(forcats::gss_cat, year, explanatory_vars =  c("race", "marital"),
#'          only_first_level = FALSE, not_last_level = FALSE)
#'
#' \dontrun{
#'   dplyr::storms %>%
#'     tabmulti(category, c("pressure", "wind"), transpose_table = TRUE) %>%
#'     tabxl()
#' }
tabmulti <- function(data, dependent_var, explanatory_vars, tab_var, wt,
                     transpose_table = FALSE, only_first_level = TRUE, not_last_level = TRUE,
                     totaltab = c("table", "line", "no"), show_na = TRUE, drop_sup_na = FALSE,
                     digits = 0, cleannames = FALSE, subtext, sort_by = "no", accelerate = FALSE, ...) {

  # if (is.data.frame(data) | ! is_tabr(data)) {
  #   original_data <- data
  #   original_tablist <- NULL

  data_name <- substitute(data) %>% as.character() %>% .[length(.)]
  if (data_name == ".") { #Find original name if data has been piped.
    data_name <- get_orig_name(data)
    data_name <- stringr::str_remove(data_name, "^[^\\$]+\\$")
  }
  # }

  # if (is_tabr(data) | all(purrr::map_lgl(data, ~ is_tabr(.)))) {
  #   # if (force_unique_table == TRUE){
  #   #   stop("Cannot use tabw with pipe/from tab with force_unique_table == TRUE") }
  #   original_data <- NULL
  #   original_tablist <- data
  #   if (is_tabr(data)) {
  #     original_args <- purrr::pluck(data, purrr::attr_getter("args"))
  #   } else {
  #     original_args <- purrr::pluck(data[[1]], purrr::attr_getter("args"))
  #   }
  #
  #   if (missing(wt) & !is.na(original_args$wt) ) wt <-         original_args$wt
  #   if (missing(show_na) )             show_na <-              original_args$show_na
  #   if (missing(totaltab) )            totaltab <-             original_args$totaltab
  #   if (missing(digits) )              digits <-               original_args$digits
  #   if (missing(cleannames) )          cleannames <-           original_args$cleannames
  #   if (missing(only_first_level) )    only_first_level <-     original_args$only_first_level
  #   if (missing(drop_sup_na) )         drop_sup_na <-          original_args$drop_sup_na
  #   if (missing(subtext) )             subtext <-              original_args$subtext
  #
  #   data <- original_args$original_data
  #   data_name <- original_args$original_data_name
  # }
  # if (!("data.frame" %in% class(data)) & ! is_tabr(data)) stop("data is of the wrong type")

  no_dependent_var <- missing(dependent_var)
  no_var3 <- missing(tab_var)
  no_weight <- missing(wt)

  # dependent_var <- rlang::enquo(dependent_var)
  # tab_var       <- rlang::enquo(tab_var)
  # wt            <- rlang::enquo(wt)

  #"Auto" settings, options incompatibilities and warnings
  if (missing(subtext) ) subtext <- NA_character_

  if (!missing(sort_by) & (class(sort_by) != "character" | length(sort_by) > 2 |
                           ifelse(is.na(sort_by[2] != "desc"), FALSE, sort_by[2] != "desc") )
  ) stop("sort_by must be a character vector of length 1 (or 2 with 'desc')")

  if (no_dependent_var) {
    stop("no dependent var")
  } else {
    if (transpose_table == FALSE) {
      var1 <- rlang::enquo(dependent_var) %>% rlang::quo_set_expr(rlang::quo_name(.) %>% stringr::str_remove_all('"') %>% rlang::sym())
      data %<>% dplyr::mutate(no_var2 = factor("n"))
      var2 <- rlang::expr(no_var2)
    } else {
      var2 <- rlang::enquo(dependent_var) %>% rlang::quo_set_expr(rlang::quo_name(.) %>% stringr::str_remove_all('"') %>% rlang::sym())
      data %<>% dplyr::mutate(no_var1 = factor("n"))
      var1 <- rlang::expr(no_var1)
    }
  }

  if (no_var3) {
    data %<>% dplyr::mutate(no_var3 = factor(" "))
    var3 <- rlang::expr(no_var3)
  } else {
    var3 <- rlang::enquo(tab_var) %>% rlang::quo_set_expr(rlang::quo_name(.) %>% stringr::str_remove_all('"') %>% rlang::sym())
  }

  if (no_weight) {
    data %<>% dplyr::mutate(no_weight = 1)
    wt <- rlang::expr(no_weight)
  } else {
    wt <- rlang::enquo(wt) %>% rlang::quo_set_expr(rlang::quo_name(.) %>% stringr::str_remove_all('"') %>% rlang::sym())
  }

  dat <- data %>% dplyr::select(!!var1, !!var2, !!var3, !!wt, tidyselect::all_of(explanatory_vars))

  if (transpose_table == FALSE) {
    tabr_main(dat = dat, data_name = data_name, #original_data = original_data, original_tablist = original_tablist,
              var1 = !!var1, var2 = !!var2, var3 = !!var3, wt = !!wt, #no_var1 = FALSE, no_var2 = TRUE, no_var3 = no_var3,  no_weight = no_weight,
              show_na = show_na, totaltab = totaltab,
              digits = digits, cleannames = cleannames, subtext = subtext, sort_by = sort_by,
              sup_cols = explanatory_vars,
              only_first_level = only_first_level, not_last_level = not_last_level, drop_sup_na = drop_sup_na,
              multicols = !transpose_table, multirows = transpose_table, tot = c("row", "col"),
              perc = dplyr::if_else(transpose_table, "col", "row"), accelerate = accelerate)
  } else {
    tabr_main(dat = dat, data_name = data_name, #original_data = original_data, original_tablist = original_tablist,
              var1 = !!var1, var2 = !!var2, var3 = !!var3, wt = !!wt, #no_var1 = FALSE, no_var2 = TRUE, no_var3 = no_var3,  no_weight = no_weight,
              show_na = show_na, totaltab = totaltab,
              digits = digits, cleannames = cleannames, subtext = subtext, sort_by = sort_by,
              sup_rows = explanatory_vars,
              only_first_level = only_first_level, not_last_level = not_last_level, drop_sup_na = drop_sup_na,
              multicols = !transpose_table, multirows = transpose_table, tot = c("row", "col"),
              perc = dplyr::if_else(transpose_table, "col", "row"), accelerate = accelerate)
  }

}










#tabr main functions ----------------------------------------------------------

#Classer les variables supplementaires en texte / numeriques (before that dat must be reorder with numeric vars at the end)
#' @keywords internal
tabr_make_sup_list <- function(dat, sup_cols = NULL, sup_rows = NULL) {
  sup_cols <- names(dat) %>% purrr::keep(. %in% sup_cols)
  sup_cols_num  <- sup_cols %>% purrr::map_lgl(~ dplyr::pull(dat, !!rlang::sym(.)) %>% is.numeric())
  sup_cols_text <- sup_cols %>% purrr::map_lgl(~ dplyr::pull(dat, !!rlang::sym(.)) %>% is.factor() |
                                                 dplyr::pull(dat, !!rlang::sym(.)) %>% is.character() )

  sup_rows <- names(dat) %>% purrr::keep(. %in% sup_rows)
  sup_rows_num  <- sup_rows %>% purrr::map_lgl(~ dplyr::pull(dat, !!rlang::sym(.)) %>% is.numeric())
  sup_rows_text <- sup_rows %>% purrr::map_lgl(~ dplyr::pull(dat, !!rlang::sym(.)) %>% is.factor() |
                                                 dplyr::pull(dat, !!rlang::sym(.)) %>% is.character() )

  list(sup_cols, sup_cols_num, sup_cols_text, sup_rows, sup_rows_num, sup_rows_text) %>%
    magrittr::set_names(c("sup_cols", "sup_cols_num", "sup_cols_text", "sup_rows", "sup_rows_num", "sup_rows_text"))
}

#Ajouter : - Rename levels "Total" and "Ensemble"
#' @keywords internal
tabr_dat_prepare <-
  function(dat, var1, var2, var3, wt, show_na = show_na, drop_sup_na = drop_sup_na, #no_var1, no_var2, no_var3
           cleannames = cleannames, minimum_headcount = minimum_headcount,
           rare_levels_to_other = rare_levels_to_other,
           sup_cols = sup_cols, sup_rows = sup_rows, only_first_level = only_first_level) {
    var1 <- rlang::enquo(var1)
    var2 <- rlang::enquo(var2)
    var3 <- rlang::enquo(var3)
    wt   <- rlang::enquo(wt)

    if (rlang::quo_name(var1) == "no_var1") {no_var1 <- TRUE} else {no_var1 <- FALSE}
    if (rlang::quo_name(var2) == "no_var2") {no_var2 <- TRUE} else {no_var2 <- FALSE}
    if (rlang::quo_name(var3) == "no_var3") {no_var3 <- TRUE} else {no_var3 <- FALSE}

    dat <- dat %>% dplyr::mutate_if(is.character, as.factor) %>%
      dplyr::mutate(dplyr::across(where(is.numeric) & tidyselect::any_of(c(rlang::quo_name(var1), rlang::quo_name(var2), rlang::quo_name(var3))), as.factor)) %>%
      dplyr::mutate(!!wt := as.numeric(!!wt))

    if ( drop_sup_na == TRUE ) {
      #Remove one by one all the NAs of explanatory variables :
      if ( length(sup_cols) != 0 ) dat <- sup_cols %>%
          purrr::reduce(.init = dat, .f = ~ dplyr::filter(.x, !(is.na(!!rlang::sym(.y)))) )
      if ( length(sup_rows) != 0 ) dat <- sup_rows %>%
          purrr::reduce(.init = dat, .f = ~ dplyr::filter(.x, !(is.na(!!rlang::sym(.y)))) )
    } else {
      if (length(sup_cols) != 0 | length(sup_rows) != 0)  dat %<>%
        dplyr::mutate(dplyr::across(where(is.factor) & tidyselect::all_of(c(sup_cols, sup_rows)),
                                    forcats::fct_explicit_na, na_level = "NA"))
    }

    if (only_first_level == TRUE) {
      supvars_3levels <- purrr::map_lgl(dat, ~ is.factor(.) & nlevels(.) >= 3) & colnames(dat) %in% c(sup_cols, sup_rows)
      if (any(supvars_3levels)) dat %<>% dplyr::mutate_if(supvars_3levels, ~ forcats::fct_other(., keep = levels(.)[1], other_level = "Autres"))
    }


    if (show_na == TRUE) {
      dat %<>%
        dplyr::mutate(dplyr::across(where(is.factor)  & ! tidyselect::all_of(c(sup_cols, sup_rows)),
                                    forcats::fct_explicit_na, na_level = "NA"))
    } else {
      if (no_var1 == FALSE) dat %<>% dplyr::filter(!is.na(!!var1))
      if (no_var2 == FALSE) dat %<>% dplyr::filter(!is.na(!!var2))
      if (no_var3 == FALSE) dat %<>% dplyr::filter(!is.na(!!var3))
      # if (!missing(var2)) {
      #   if (!missing(var3)) {
      #     dat %<>% dplyr::filter(!(is.na(!!var1)|(is.na(!!var2))|(is.na(!!var3))))
      #   } else {dat %<>% dplyr::filter(!(is.na(!!var1)|(is.na(!!var2))))}
      # } else {dat %<>% dplyr::filter(!(is.na(!!var1)))}
    }



    if(rare_levels_to_other == TRUE) {
      dat %<>%
        dplyr::mutate_if(is.factor,
                         ~forcats::fct_lump_min(., minimum_headcount, other_level = "Autres"))
      if(!missing(var3)) {
        # We only count third variable's minimum headcount for the ligne variable,
        #  otherwise we got problems.
        dat %<>% dplyr::group_by(!!var3) %>%
          dplyr::mutate(!!var1 := as.character(forcats::fct_lump_min(!!var1, minimum_headcount,
                                                                     other_level = "Autres"))) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(!!var1 := as.factor(!!var1))
      }
    }
    dat %<>% dplyr::mutate_if(is.factor, forcats::fct_drop)  #Remove unused levels anyway


    if (cleannames == TRUE) dat %<>% dplyr::mutate_if(is.factor, ~ fct_clean(.))

    #If sex is in supplementary var, see % of women and not men
    if(!is.null(sup_cols) | !is.null(sup_rows)) {
      if ("SEXE" %in% sup_cols){
        if (!stringr::str_detect(levels(dat$SEXE)[1], "f|F")) dat %<>% dplyr::mutate(SEXE = forcats::fct_rev(SEXE))
      }
    }

    dat %<>% dplyr::select(!!var3, !!var1, !!var2, !!wt, tidyselect::all_of(c(sup_cols, sup_rows))) %>%
      dplyr::select(where(is.factor), where(is.numeric)) %>%
      dplyr::select(!!var3, !!var1, !!var2, !!wt, tidyselect::everything())

    return(dat)
  }






#Ajouter :
#          - Pour les variables numeriques, choix de la fonction (pas seulement moyenne ; difficile CI)
#          - Eclater en plusieurs fonctions pour pouvoir recalculer une valeur au besoin ?
#          - Add requireNamespace("DescTools", quietly = TRUE) -----------------
#' @keywords internal
tabr_core <- function(dat, var1, var2, var3, wt,
                      perc = c("no", "row", "col", "all", "all_tabs"), conf_level = 0.95,
                      keep_unused_levels = FALSE, minimum_headcount = 30, digits = 0L,
                      sup_list,
                      only_first_level = TRUE, not_last_level = TRUE, another_total = FALSE,
                      multicols = FALSE, multirows = FALSE, accelerate = FALSE, confidence_intervals = FALSE, design_effect = TRUE) {
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)
  var3 <- rlang::enquo(var3)
  wt   <- rlang::enquo(wt)

  if (rlang::quo_name(var1) == "no_var1") {no_var1 <- TRUE} else {no_var1 <- FALSE}
  if (rlang::quo_name(var2) == "no_var2") {no_var2 <- TRUE} else {no_var2 <- FALSE}
  if (rlang::quo_name(var3) == "no_var3") {no_var3 <- TRUE} else {no_var3 <- FALSE}

  sup_cols      <-  sup_list$sup_cols
  sup_cols_num  <-  sup_list$sup_cols_num
  sup_cols_text <-  sup_list$sup_cols_text
  sup_rows      <-  sup_list$sup_rows
  sup_rows_num  <-  sup_list$sup_rows_num
  sup_rows_text <-  sup_list$sup_rows_text


  #Note : to keep order of levels with dplyr::arrange(), all text vars must always stay factors.

  #Faire les group_by une seule fois car ils demandent du temps de calcul :
  dat_group3  <- dat %>% dplyr::group_by(!!var3, .drop = FALSE)
  dat_group123 <- dat_group3 %>% dplyr::group_by(!!var1, !!var2, .add = TRUE, .drop = FALSE)

  #Base tab (weighted and unweighted) :
  if (accelerate == FALSE) {
    if (confidence_intervals == TRUE & design_effect == TRUE) {
      wtable <- dat_group123 %>% dplyr::summarise(n =dplyr::n(), weighted_n = sum(!!wt), wn2 = sum((!!wt)^2), .groups = "drop")
    } else {
      wtable <- dat_group123 %>% dplyr::summarise(n =dplyr::n(), weighted_n = sum(!!wt), .groups = "drop")
    }
  } else {
    if (confidence_intervals == TRUE & design_effect == TRUE) {
      wtable <- dat_group123 %>% dplyr::summarise(weighted_n = sum(!!wt), wn2 = sum((!!wt)^2), .groups = "drop")
    } else {
      wtable <- dat_group123 %>% dplyr::summarise(weighted_n = sum(!!wt), .groups = "drop")
    }
  }
  wtable %<>% dplyr::mutate(.zone = factor("base"), .TYPE = factor("factor")  ) %>%
    dplyr::select(.zone, !!var3, tidyselect::everything()) %>% dplyr::arrange(!!var3)

  #Total tab
  if (no_var3 == FALSE) {
    wtable %<>% dplyr::group_by(!!var1, !!var2)
    if (accelerate == FALSE){
      if (confidence_intervals == TRUE & design_effect == TRUE) {
        wtable %<>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var3 := factor("Total"),.TYPE = factor("factor"),
                                            n = sum(n), weighted_n = sum(weighted_n), wn2 = sum(wn2), .groups = "drop"))
      } else {
        wtable %<>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var3 := factor("Total"), .TYPE = factor("factor"),
                                            n = sum(n), weighted_n = sum(weighted_n), .groups = "drop"))
      }
    } else {
      if (confidence_intervals == TRUE & design_effect == TRUE) {
        wtable %<>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var3 := factor("Total"), .TYPE = factor("factor"),
                                            weighted_n = sum(weighted_n), wn2 = sum(wn2), .groups = "drop"))
      } else {
        wtable %<>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var3 := factor("Total"), .TYPE = factor("factor"),
                                            weighted_n = sum(weighted_n), .groups = "drop"))
      }
    }
  }

  wtable %<>% dplyr::group_by(!!var3, !!var1) %>% dplyr::mutate(.wtot1 = sum(weighted_n))
  if (accelerate == FALSE) wtable %<>% dplyr::mutate(.tot1 = sum(n), nbrow =dplyr::n() )
  #if (keep_unused_levels == FALSE) wtable %<>% dplyr::filter(!all(weighted_n == 0))

  wtable %<>% dplyr::group_by(!!var3, !!var2) %>% dplyr::mutate(.wtot2 = sum(weighted_n))
  if (accelerate == FALSE) wtable %<>% dplyr::mutate(.tot2 = sum(n), nbcol =dplyr::n() )

  wtable %<>% dplyr::group_by(!!var3) %>% dplyr::mutate(.wtot3 = sum(weighted_n))
  if (accelerate == FALSE) wtable %<>% dplyr::mutate(.tot3 = sum(n))
  if (confidence_intervals == TRUE & design_effect == TRUE) wtable %<>%
    dplyr::mutate(deff = sum(n) * sum(wn2)/(sum(weighted_n)^2))

  if (perc[1] == "all_tabs") wtable %<>%
    dplyr::mutate(tottab = !!var3 == "Total") %>% dplyr::group_by(tottab) %>%
    dplyr::mutate(.totn = sum(n), .wtotn = sum(weighted_n)) %>%
    dplyr::select(-wn2) %>% dplyr::ungroup() %>% dplyr::select(-tottab)


  if (accelerate == FALSE) {
    if (perc[1] == "all_tabs") {
      if (confidence_intervals == TRUE & design_effect == TRUE) {
        wtable %<>%
          dplyr::group_by(!!var3, !!var1) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
                                            n = dplyr::first(.tot1), weighted_n = dplyr::first(.wtot1), deff = dplyr::first(deff), .TYPE = factor("factor"),
                                            .tot1 = dplyr::first(.tot1), .wtot1 = dplyr::first(.wtot1), nbrow = dplyr::first(nbrow),
                                            .tot2 = dplyr::first(.tot3), .wtot2 = dplyr::first(.wtot3), nbcol = dplyr::first(nbcol),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3),
                                            .totn = dplyr::first(.totn), .wtotn = dplyr::first(.wtotn), .groups = "drop")) %>%
          dplyr::group_by(!!var3, !!var2) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
                                            n = dplyr::first(.tot2), weighted_n = dplyr::first(.wtot2), deff = dplyr::first(deff), .TYPE = factor("factor"),
                                            .tot1 = dplyr::first(.tot3), .wtot1 = dplyr::first(.wtot3), nbrow = dplyr::first(nbrow),
                                            .tot2 = dplyr::first(.tot2), .wtot2 = dplyr::first(.wtot2), nbcol = dplyr::first(nbcol),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3),
                                            .totn = dplyr::first(.totn), .wtotn = dplyr::first(.wtotn), .groups = "drop"))
      } else {
        wtable %<>%
          dplyr::group_by(!!var3, !!var1) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
                                            n = dplyr::first(.tot1), weighted_n = dplyr::first(.wtot1), .TYPE = factor("factor"),
                                            .tot1 = dplyr::first(.tot1), .wtot1 = dplyr::first(.wtot1), nbrow = dplyr::first(nbrow),
                                            .tot2 = dplyr::first(.tot3), .wtot2 = dplyr::first(.wtot3), nbcol = dplyr::first(nbcol),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3),
                                            .totn = dplyr::first(.totn), .wtotn = dplyr::first(.wtotn), .groups = "drop")) %>%
          dplyr::group_by(!!var3, !!var2) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
                                            n = dplyr::first(.tot2), weighted_n = dplyr::first(.wtot2), .TYPE = factor("factor"),
                                            .tot1 = dplyr::first(.tot3), .wtot1 = dplyr::first(.wtot3), nbrow = dplyr::first(nbrow),
                                            .tot2 = dplyr::first(.tot2), .wtot2 = dplyr::first(.wtot2), nbcol = dplyr::first(nbcol),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3),
                                            .totn = dplyr::first(.totn), .wtotn = dplyr::first(.wtotn), .groups = "drop"))
      }
    } else { # perc != "all_tabs"
      if (confidence_intervals == TRUE & design_effect == TRUE) {
        wtable %<>%
          dplyr::group_by(!!var3, !!var1) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
                                            n = dplyr::first(.tot1), weighted_n = dplyr::first(.wtot1), deff = dplyr::first(deff), .TYPE = factor("factor"),
                                            .tot1 = dplyr::first(.tot1), .wtot1 = dplyr::first(.wtot1), nbrow = dplyr::first(nbrow),
                                            .tot2 = dplyr::first(.tot3), .wtot2 = dplyr::first(.wtot3), nbcol = dplyr::first(nbcol),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3), .groups = "drop")) %>%
          dplyr::group_by(!!var3, !!var2) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
                                            n = dplyr::first(.tot2), weighted_n = dplyr::first(.wtot2), deff = dplyr::first(deff), .TYPE = factor("factor"),
                                            .tot1 = dplyr::first(.tot3), .wtot1 = dplyr::first(.wtot3), nbrow = dplyr::first(nbrow),
                                            .tot2 = dplyr::first(.tot2), .wtot2 = dplyr::first(.wtot2), nbcol = dplyr::first(nbcol),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3), .groups = "drop"))
      } else {
        wtable %<>%
          dplyr::group_by(!!var3, !!var1) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
                                            n = dplyr::first(.tot1), weighted_n = dplyr::first(.wtot1), .TYPE = factor("factor"),
                                            .tot1 = dplyr::first(.tot1), .wtot1 = dplyr::first(.wtot1), nbrow = dplyr::first(nbrow),
                                            .tot2 = dplyr::first(.tot3), .wtot2 = dplyr::first(.wtot3), nbcol = dplyr::first(nbcol),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3), .groups = "drop")) %>%
          dplyr::group_by(!!var3, !!var2) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
                                            n = dplyr::first(.tot2), weighted_n = dplyr::first(.wtot2), .TYPE = factor("factor"),
                                            .tot1 = dplyr::first(.tot3), .wtot1 = dplyr::first(.wtot3), nbrow = dplyr::first(nbrow),
                                            .tot2 = dplyr::first(.tot2), .wtot2 = dplyr::first(.wtot2), nbcol = dplyr::first(nbcol),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3), .groups = "drop"))
      }
    }

  } else { # if (accelerate == TRUE)
    if (perc[1] == "all_tabs") {
      if (confidence_intervals == TRUE & design_effect == TRUE) {
        wtable %<>% dplyr::group_by(!!var3, !!var1) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
                                            weighted_n = dplyr::first(.wtot1), deff = dplyr::first(deff), .TYPE = factor("factor"),
                                            .wtot1 = dplyr::first(.wtot1), .wtot2 = dplyr::first(.wtot3), .wtot3 = dplyr::first(.wtot3),
                                            .wtotn = dplyr::first(.wtotn), .groups = "drop")) %>%
          dplyr::group_by(!!var3, !!var2) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
                                            weighted_n = dplyr::first(.wtot2), deff = dplyr::first(deff), .TYPE = factor("factor"),
                                            .wtot1 = dplyr::first(.wtot3), .wtot2 = dplyr::first(.wtot2),.wtot3 = dplyr::first(.wtot3),
                                            .wtotn = dplyr::first(.wtotn), .groups = "drop"))
      } else {
        wtable %<>% dplyr::group_by(!!var3, !!var1) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
                                            weighted_n = dplyr::first(.wtot1), .TYPE = factor("factor"),
                                            .wtot1 = dplyr::first(.wtot1), .wtot2 = dplyr::first(.wtot3), .wtot3 = dplyr::first(.wtot3),
                                            .wtotn = dplyr::first(.wtotn), .groups = "drop")) %>%
          dplyr::group_by(!!var3, !!var2) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
                                            weighted_n = dplyr::first(.wtot2), .TYPE = factor("factor"),
                                            .wtot1 = dplyr::first(.wtot3), .wtot2 = dplyr::first(.wtot2),.wtot3 = dplyr::first(.wtot3),
                                            .wtotn = dplyr::first(.wtotn), .groups = "drop"))
      }
    } else {
      if (confidence_intervals == TRUE & design_effect == TRUE) {
        wtable %<>% dplyr::group_by(!!var3, !!var1) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
                                            weighted_n = dplyr::first(.wtot1), deff = dplyr::first(deff), .TYPE = factor("factor"),
                                            .wtot1 = dplyr::first(.wtot1), .wtot2 = dplyr::first(.wtot3), .wtot3 = dplyr::first(.wtot3), .groups = "drop")) %>%
          dplyr::group_by(!!var3, !!var2) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
                                            weighted_n = dplyr::first(.wtot2), deff = dplyr::first(deff), .TYPE = factor("factor"),
                                            .wtot1 = dplyr::first(.wtot3), .wtot2 = dplyr::first(.wtot2),.wtot3 = dplyr::first(.wtot3), .groups = "drop"))
      } else {
        wtable %<>% dplyr::group_by(!!var3, !!var1) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
                                            weighted_n = dplyr::first(.wtot1), .TYPE = factor("factor"),
                                            .wtot1 = dplyr::first(.wtot1), .wtot2 = dplyr::first(.wtot3), .wtot3 = dplyr::first(.wtot3), .groups = "drop")) %>%
          dplyr::group_by(!!var3, !!var2) %>%
          dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
                                            weighted_n = dplyr::first(.wtot2), .TYPE = factor("factor"),
                                            .wtot1 = dplyr::first(.wtot3), .wtot2 = dplyr::first(.wtot2),.wtot3 = dplyr::first(.wtot3), .groups = "drop"))
      }
    }
  }
  wtable %<>% dplyr::ungroup() %>% dplyr::arrange(!!var3, !!var1)

  #Percentages:
  if (perc[1] == "row") {
    perc_wtot <- rlang::expr(.wtot1)
  } else if (perc[1] == "col")            {
    perc_wtot <- rlang::expr(.wtot2)
  } else if (perc[1] == "all") {
    perc_wtot <- rlang::expr(.wtot3)
  } else if (perc[1] == "all_tabs")       {
    perc_wtot <- rlang::expr(.wtotn)
  }
  if (perc[1] != "no") wtable %<>% dplyr::mutate(pct = (weighted_n/!!perc_wtot) %>% tidyr::replace_na(., 0) %>% round(7)  )

  #Confidence intervals :
  if (confidence_intervals == TRUE) {
    if (perc[1] == "row") {
      perc_tot <- rlang::expr(.tot1)
    } else if (perc[1] == "col")            {
      perc_tot <- rlang::expr(.tot2)
    } else if (perc[1] %in% c("all", "no")) {
      perc_tot <- rlang::expr(.tot3)
    } else if (perc[1] == "all_tabs")       {
      perc_tot <- rlang::expr(.totn)
    }
    wtable %<>%
      dplyr::mutate(moe = DescTools::BinomCI(pct*!!perc_tot, !!perc_tot, method = "wilson",
                                             conf.level = conf_level) %>% as.data.frame() %>%
                      dplyr::mutate(ci = (upr.ci - est)) %>% dplyr::pull(ci) %>% tidyr::replace_na(0) %>% round(7),
                    conf_level = conf_level)
    if (design_effect == TRUE) wtable %<>% dplyr::mutate(ci = ci * deff)

    if (perc[1] == "no"){
      wtable %<>%
        dplyr::mutate(moe_wn = moe * .wtot3,
                      res_min = round(pmax(weighted_n - moe_wn, 0)),  #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
                      res_max = round(weighted_n + moe_wn), #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
                      resCI = dplyr::if_else(res_min == res_max, stringr::str_c(res_min), stringr::str_c(res_min, "-", res_max) ))

    } else {
      wtable %<>%
        dplyr::mutate(res_min = round(pmax((pct - moe)*100, 0), digits),  #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
                      res_max = round(pmin((pct + moe)*100, 100), digits), #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
                      resCI = dplyr::if_else(res_min == res_max, stringr::str_c(res_min, "%"), stringr::str_c(res_min, "-", res_max, "%") ) ) #stringr::str_remove(., "%(?=-)") %>% stringr::str_replace_all("\\.", ",")
      #stringr::str_pad(res_min, max(stringr::str_length(res_min[res_min != res_max])))
    }
    wtable %<>% dplyr::select(-res_min, - res_max)
  }

  # if ( CItype[1] == "range") {
  #   }
  # else if ( CItype[1] == "+-" ) {
  #   wtable %<>% purrr::map(~ adorn_pct_formatting(., digits = digits) %>%
  #                     dplyr::mutate_at(dplyr::vars(-1), ~ stringr::str_replace_all(., "(100)\\.0+%", "\\1%")))
  #   CI  %<>% purrr::map(~ adorn_pct_formatting(., digits = max(digits, 1)) %>%
  #                  dplyr::mutate_at(dplyr::vars(-1), ~ stringr::str_remove_all(., "%") %>%
  #                              stringr::str_remove_all("^-$|^0\\.0+$|^0$") ) %>%
  #                  dplyr::mutate_at(dplyr::vars(1), ~ "") )
  #   wtable <-
  #     purrr::map2(wtable, CI, ~ purrr::map2_df(.x, .y, function(..x, ..y)
  #       stringr::str_c(..x, #stringr::str_pad(..x, 3 + digits, pad = "&")  %>% stringr::str_replace_all("&", "  "),
  #             stringr::str_pad(stringr::str_c(" (\\u00b1", ..y, ")"), 7 + max(digits, 1), pad = "&") %>%
  #               stringr::str_replace_all("&", "  ") %>% stringr::str_replace("( *)\\(\\u00b1( *)\\)$", "\\1    \\2")
  #       )  )) %>% magrittr::set_names(sheet_names[1:length(.)]) %>%
  #     purrr::map(~ dplyr::mutate_at(., dplyr::vars(-1), ~ stringr::str_replace_all(., "\\.", ",") %>%
  #                       stringr::str_replace("(100%) *", "\\1") ) %>%
  #           dplyr::mutate_at(dplyr::vars(1), ~ stringr::str_remove_all(., " *\\(*\\u00b1 *\\)*") ) %>%
  #           dplyr::mutate(llllllllll = 1) %>% as_tabyl() %>% dplyr::select(-llllllllll))
  # }
  # # Print :
  # if (confidence_intervals[1] > 0 ) {
  #   wtable %<>% dplyr::mutate_at(dplyr::vars(-1), ~ stringr::str_replace(., "  \\(\\u00b1", " (\\u00b1") %>%
  #                           stringr::str_replace("  +\\(\\u00b1", "  (\\u00b1") %>%
  #                           stringr::str_replace(" {13}$", "        ") ) %>%
  #     dplyr::mutate_at(dplyr::vars(-1), ~ stringr::str_c(" ", .))
  # }

  # #Table de Gauss
  # tibble::tibble(Effectifs = c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 100000),
  #        `1% (ou 99%)` = Effectifs*0.01,`2% (98%)` = Effectifs*0.02,
  #        `5% (95%)`= Effectifs*0.05,`10% (90%)` = Effectifs*0.1,
  #        `20% (80%)` = Effectifs*0.2,`50%` = Effectifs*0.5 ) %>%
  #   tidyr::pivot_longer(cols = -1) %>%
  #   dplyr::mutate(moeWi = BinomCI(value, Effectifs, method = "wilson"),
  #          moe11 = BinomCI(value, Effectifs, method = "wald"),
  #          moe12 = BinomCI(value, Effectifs, method = "agresti-coull"),
  #          moe13 = BinomCI(value, Effectifs, method = "jeffreys"),
  #          moe14 = BinomCI(value, Effectifs, method = "modified wilson"),
  #          moe15 = BinomCI(value, Effectifs, method = "wilsoncc"),
  #          moe16 = BinomCI(value, Effectifs, method = "modified jeffreys"),
  #          moe17 = BinomCI(value, Effectifs, method = "clopper-pearson"),
  #          moe18 = BinomCI(value, Effectifs, method = "logit"),
  #          moe19 = BinomCI(value, Effectifs, method = "arcsine")) %>%
  #   dplyr::mutate_at(dplyr::vars(starts_with("moe")),
  #             ~ as.data.frame(.) %>% dplyr::mutate(ci = upr.ci - est) %>% dplyr::pull(ci)) %>%
  #   dplyr::mutate_at(dplyr::vars(starts_with("moe")), ~round(. * 1 * 100, 1)) %>%
  #   dplyr::mutate_at(dplyr::vars(starts_with("moe1")), ~ (. - moeWi)) %>%
  #   dplyr::mutate(moe2 = moedeff_calc(pct = value/Effectifs, deff = 1, n = Effectifs) %>% round(1)) %>%
  #   dplyr::mutate(pct = round(value/Effectifs * 100, 1)) %>%
  #   View()





  if (another_total == TRUE) {
    if (perc %in% c("row", "no", "all", "all_tabs")) {
      wtable %<>%
        dplyr::mutate(an_totcol = dplyr::case_when(
          !!var1 == "Total" & !!var2 == "Total" ~ weighted_n,
          !!var2 == "Total"                     ~ weighted_n/.wtot2) )
    } else if (perc %in% "col") {
      wtable %<>%
        dplyr::mutate(an_totrow = dplyr::case_when(
          !!var1 == "Total" & !!var2 == "Total" ~ weighted_n,
          !!var1 == "Total"                     ~ weighted_n/.wtot1) )
    }
  }

  # Tab of relative contributions of cells to explained variance
  if (accelerate == FALSE) {
    if (no_var1 == FALSE & no_var2 == FALSE) {
      #USE EXISTING FUNC TO BE MORE CERTAIN OF THE RESULT ? cor
      wtable %<>%
        dplyr::mutate(expected = (.wtot1 * .wtot2)/(.wtot3 ^ 2),
                      spread = weighted_n/.wtot3 - expected,
                      binding_ratio = spread/expected,
                      ctr_abs = expected * binding_ratio ^ 2) %>%
        dplyr::mutate_at(c("ctr_abs", "binding_ratio"), ~ tidyr::replace_na(., 0)  ) %>%

        dplyr::group_by(!!var3) %>%
        dplyr::mutate(Vnuage = sum(ctr_abs)) %>%

        dplyr::group_by(!!var3, !!var2) %>%
        dplyr::mutate(ctr_abs_var1 = sum(ctr_abs)) %>%

        dplyr::group_by(!!var3, !!var1) %>%
        dplyr::mutate(ctr_abs_var2 = sum(ctr_abs)) %>%

        dplyr::ungroup() %>%
        dplyr::mutate(ctr_abs = ifelse(!!var1 == "Total", ctr_abs_var1, ctr_abs) ) %>%
        dplyr::mutate(ctr_abs = ifelse(!!var2 == "Total", ctr_abs_var2, ctr_abs) ) %>%
        dplyr::mutate(ctr_abs = ifelse(!!var1 == "Total" & !!var2 == "Total", Vnuage, ctr_abs) ) %>%
        dplyr::select(-ctr_abs_var1, -ctr_abs_var2) %>%

        dplyr::mutate(ctr_no_sign = ctr_abs/Vnuage,
                      contrib = (ctr_no_sign * sign(spread)) %>%
                        dplyr::if_else(!!var1 == "Total" | !!var2 == "Total", ctr_no_sign, .),
                      ctr_mean = (1/(nbrow * nbcol)) %>%
                        dplyr::if_else(!!var1 == "Total" | !!var2 == "Total", NA_real_, .) ) %>%
        dplyr::mutate(dplyr::across(expected:contrib, ~ tidyr::replace_na(., 0) %>% round(7) ))


    } else { #If there is no var2
      wtable %<>% dplyr::mutate(expected  = 0, spread  = 0, binding_ratio  = 0, ctr_abs  = 0,
                                Vnuage  = 0, ctr_no_sign  = 0, contrib  = 0, ctr_mean = NA_real_)
    }
  }
  #Supprimer variables transitoires
  wtable %<>% dplyr::select(-tidyselect::any_of(c("nbrow", "nbcol")))










  #Supplementary cols and rows
  sup_tab_calc <- function(dat_group, sup_names, sup_text, .zone, accelerate = FALSE, confidence_intervals) {
    if (accelerate == FALSE) {
      if (confidence_intervals == TRUE) {
        sup_tab_calculation <- sup_names %>%
          purrr::map_if(sup_text, function(.x) # Character and factor vars :
            dat_group %>% dplyr::group_by_at(dplyr::vars(tidyselect::all_of(.x)), .add = TRUE, .drop = FALSE) %>%
              dplyr::summarise(n =dplyr::n(), weighted_n = sum(!!wt), .groups = "drop") %>%
              dplyr::mutate(.SUP_NAME = factor(.x), .TYPE = factor("factor"),
                            res = NA_real_) %>%
              dplyr::rename_at(dplyr::vars(tidyselect::all_of(.x)), ~ ".SUP"),
            .else = function(.x)  # Numeric vars :
              dat_group %>%
              dplyr::summarise(n =dplyr::n(), weighted_n = sum(!!wt),
                               res = stats::weighted.mean(!!rlang::sym(.x), !!wt, na.rm = TRUE),
                               moe_res = suppressWarnings(MeanCI(!!rlang::sym(.x), conf.level = conf_level, na.rm = TRUE)) %>%
                                 t() %>% tibble::as_tibble() %>% dplyr::mutate(ci = upr.ci - mean) %>% dplyr::pull(ci) %>% round(7),
                               .groups = "drop") %>%
              dplyr::mutate(.SUP_NAME = factor(.x), .TYPE = factor("num"), .SUP = factor(.x)) )
      } else { # if (confidence_intervals == FALSE)
        sup_tab_calculation <- sup_names %>%
          purrr::map_if(sup_text, function(.x) # Character and factor vars :
            dat_group %>% dplyr::group_by_at(dplyr::vars(tidyselect::all_of(.x)), .add = TRUE, .drop = FALSE) %>%
              dplyr::summarise(n =dplyr::n(), weighted_n = sum(!!wt), .groups = "drop") %>%
              dplyr::mutate(.SUP_NAME = factor(.x), .TYPE = factor("factor"),
                            res = NA_real_) %>%
              dplyr::rename_at(dplyr::vars(tidyselect::all_of(.x)), ~ ".SUP"),
            .else = function(.x)  # Numeric vars :
              dat_group %>%
              dplyr::summarise(n =dplyr::n(), weighted_n = sum(!!wt),
                               res = stats::weighted.mean(!!rlang::sym(.x), !!wt, na.rm = TRUE), .groups = "drop") %>%
              dplyr::mutate(.SUP_NAME = factor(.x), .TYPE = factor("num"), .SUP = factor(.x)) )
      }

    } else { #    if (accelerate == TRUE)
      if (confidence_intervals == TRUE) {
        sup_tab_calculation <- sup_names %>%
          purrr::map_if(sup_text, function(.x) # Character and factor vars :
            dat_group %>% dplyr::group_by_at(dplyr::vars(tidyselect::all_of(.x)), .add = TRUE, .drop = FALSE) %>%
              dplyr::summarise(weighted_n = sum(!!wt), .groups = "drop") %>%
              dplyr::mutate(.SUP_NAME = factor(.x), .TYPE = factor("factor"),
                            res = NA_real_) %>%
              dplyr::rename_at(dplyr::vars(tidyselect::all_of(.x)), ~ ".SUP"),
            .else = function(.x)  # Numeric vars :
              dat_group %>%
              dplyr::summarise(weighted_n = sum(!!wt),
                               res = stats::weighted.mean(!!rlang::sym(.x), !!wt, na.rm = TRUE),
                               moe_res = suppressWarnings(MeanCI(!!rlang::sym(.x), conf.level = conf_level, na.rm = TRUE)) %>%
                                 t() %>% tibble::as_tibble() %>% dplyr::mutate(ci = upr.ci - mean) %>% dplyr::pull(ci) %>% round(7),
                               .groups = "drop") %>%
              dplyr::mutate(.SUP_NAME = factor(.x), .TYPE = factor("num"), .SUP = factor(.x)) )
      } else { # if (confidence_intervals == FALSE)
        sup_tab_calculation <- sup_names %>%
          purrr::map_if(sup_text, function(.x) # Character and factor vars :
            dat_group %>% dplyr::group_by_at(dplyr::vars(tidyselect::all_of(.x)), .add = TRUE, .drop = FALSE) %>%
              dplyr::summarise(weighted_n = sum(!!wt), .groups = "drop") %>%
              dplyr::mutate(.SUP_NAME = factor(.x), .TYPE = factor("factor"),
                            res = NA_real_) %>%
              dplyr::rename_at(dplyr::vars(tidyselect::all_of(.x)), ~ ".SUP"),
            .else = function(.x)  # Numeric vars :
              dat_group %>%
              dplyr::summarise(weighted_n = sum(!!wt),
                               res = stats::weighted.mean(!!rlang::sym(.x), !!wt, na.rm = TRUE),
                               .groups = "drop") %>%
              dplyr::mutate(.SUP_NAME = factor(.x), .TYPE = factor("num"), .SUP = factor(.x)) )
      }
    }

    sup_tab_calculation %<>% purrr::map_if(purrr::map_lgl(., ~ ! rlang::quo_name(var1) %in% colnames(.)),
                                           ~ dplyr::mutate(., !!var1 := factor("Total")))
    sup_tab_calculation %<>% purrr::map_if(purrr::map_lgl(., ~ ! rlang::quo_name(var2) %in% colnames(.)),
                                           ~ dplyr::mutate(., !!var2 := factor("Total")))
    sup_tab_calculation %<>% purrr::map_if(purrr::map_lgl(., ~ ! rlang::quo_name(var3) %in% colnames(.)),
                                           ~ dplyr::mutate(., !!var3 := factor("Total")))

    duplicated_levels <- sup_tab_calculation %>%
      purrr::map(~ dplyr::pull(., .SUP) %>% levels()) %>% purrr::flatten_chr() %>% .[duplicated(.)] %>% unique()
    new_levels <- sup_tab_calculation %>%
      purrr::map(~ dplyr::filter(., .SUP %in% duplicated_levels) %>%
                   dplyr::group_by(.SUP) %>% dplyr::slice(1) %>%
                   dplyr::mutate(.SUP2 = stringr::str_c(.SUP, "_", .SUP_NAME)) %>%
                   dplyr::mutate(.SUP = magrittr::set_names(as.character(.SUP), .SUP2) ) %>% dplyr::pull(.SUP) )
    where_new_levels <- new_levels %>% purrr::map_lgl(~ length(.) != 0)
    sup_tab_calculation[where_new_levels] <- sup_tab_calculation[where_new_levels] %>%
      purrr::map2(new_levels[where_new_levels], ~ dplyr::mutate(.x, .SUP = fct_rename(.SUP, c(.y))))

    sup_tab_calculation %>%
      purrr::map(~ dplyr::mutate(., .zone = factor(.zone)) %>%
                   dplyr::select(.zone, !!var3, !!var1, !!var2, .SUP_NAME, .TYPE, .SUP, tidyselect::everything(), tidyselect::any_of("n"), weighted_n) %>%  #nlv
                   dplyr::arrange(!!var3)  ) %>%
      dplyr::bind_rows()
  }

  if (length(sup_cols) != 0 & multirows == FALSE) {
    dat_group31 <- dat_group3 %>% dplyr::group_by(!!var1, .add = TRUE, .drop = FALSE)

    sup_cols <- names(dat) %>% purrr::keep(. %in% sup_cols)
    if (missing(sup_cols_num)) sup_cols_num <- sup_cols %>%
      purrr::map_lgl(~ dplyr::pull(dat, !!rlang::sym(.)) %>% is.numeric())
    if (missing(sup_cols_text)) sup_cols_text <- sup_cols %>%
      purrr::map_lgl(~ dplyr::pull(dat, !!rlang::sym(.)) %>% is.factor() |
                       dplyr::pull(dat, !!rlang::sym(.)) %>% is.character() )

    if (confidence_intervals == TRUE & design_effect == TRUE) var3_deff <- wtable %>% dplyr::group_by(!!var3) %>%
      dplyr::summarise(deff = dplyr::first(deff), .groups = "keep") %>% dplyr::mutate(.zone = factor("deff"))

    if(any(sup_cols_text)) {
      if (accelerate == FALSE) {
        sup_cols_tabs_text <- dat_group31 %>%
          sup_tab_calc(sup_cols[sup_cols_text], sup_cols_text[sup_cols_text], .zone = "sup_cols", confidence_intervals = confidence_intervals)
        if (no_var3 == FALSE ) sup_cols_tabs_text %<>%
          dplyr::group_by(.SUP_NAME, !!var1, .SUP) %>%
          dplyr::bind_rows(dplyr::summarise(., !!var3 := factor("Total"), !!var2 := factor("Total"),
                                            .TYPE = dplyr::first(.TYPE), .zone = factor("sup_cols"),
                                            n = sum(n), weighted_n = sum(weighted_n), .groups = "drop"))

      } else { #If (accelerate == TRUE)
        sup_cols_tabs_text <- dat_group31 %>%
          sup_tab_calc(sup_cols[sup_cols_text], sup_cols_text[sup_cols_text], .zone = "sup_cols", accelerate = TRUE, confidence_intervals = confidence_intervals)
        if (no_var3 == FALSE ) sup_cols_tabs_text %<>%
          dplyr::group_by(.SUP_NAME, !!var1, .SUP) %>%
          dplyr::bind_rows(dplyr::summarise(., !!var3 := factor("Total"), !!var2 := factor("Total"),
                                            .TYPE = dplyr::first(.TYPE), .zone = factor("sup_cols"),
                                            weighted_n = sum(weighted_n), .groups = "drop"))
      }

      sup_cols_tabs_text %<>% dplyr::group_by(.SUP_NAME, !!var3, !!var1) %>% dplyr::mutate(.wtot1 = sum(weighted_n))

      if (accelerate == FALSE) sup_cols_tabs_text %<>% dplyr::mutate(.tot1 = sum(n))

      sup_cols_tabs_text %<>% dplyr::group_by(.SUP_NAME, !!var3) %>% dplyr::mutate(.wtot3 = sum(weighted_n))

      if (accelerate == FALSE) sup_cols_tabs_text %<>% dplyr::mutate(.tot3 = sum(n))

      if (accelerate == FALSE) {
        sup_cols_tabs_text %<>% dplyr::group_by(.SUP_NAME, !!var3, .SUP) %>%
          dplyr::bind_rows(dplyr::summarise(., !!var1 := factor("Total"), !!var2 := factor("Total"),
                                            n = sum(n), weighted_n = sum(weighted_n), .TYPE = dplyr::first(.TYPE),
                                            .tot1 = dplyr::first(.tot3), .wtot1 = dplyr::first(.wtot3), .zone = factor("sup_cols"),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3), .groups = "drop") )

      } else { # if (accelerate == TRUE)
        sup_cols_tabs_text %<>% dplyr::group_by(.SUP_NAME, !!var3, .SUP) %>%
          dplyr::bind_rows(dplyr::summarise(., !!var1 := factor("Total"), !!var2 := factor("Total"),
                                            weighted_n = sum(weighted_n), .TYPE = dplyr::first(.TYPE),
                                            .wtot1 = dplyr::first(.wtot3), .zone = factor("sup_cols"),
                                            .wtot3 = dplyr::first(.wtot3), .groups = "drop") )
      }

      #Percentages and confidence intervals:
      sup_cols_tabs_text %<>% dplyr::mutate(pct = weighted_n/.wtot1)

      if (confidence_intervals == TRUE) {
        sup_cols_tabs_text %<>%
          dplyr::mutate(moe = DescTools::BinomCI(pct*.tot1, .tot1, method = "wilson",
                                                 conf.level = conf_level) %>% as.data.frame() %>%
                          dplyr::mutate(ci = (upr.ci - est)) %>% dplyr::pull(ci) %>% tidyr::replace_na(0) %>% round(7))

        if (design_effect == TRUE) sup_cols_tabs_text %<>% dplyr::group_by(!!var3) %>% dplyr::bind_rows(var3_deff) %>%
          dplyr::mutate(deff = dplyr::last(deff)) %>% dplyr::filter(!.zone == "deff") %>% dplyr::ungroup() %>%
          dplyr::mutate(ci = ci * deff)

        sup_cols_tabs_text %<>%
          dplyr::mutate(conf_level = conf_level,
                        res_min = round(pmax((pct - moe)*100, 0), digits),  #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
                        res_max = round(pmin((pct + moe)*100, 100), digits), #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
                        resCI = dplyr::if_else(res_min == res_max, stringr::str_c(res_min, "%"), stringr::str_c(res_min, "-", res_max, "%") )) %>% #stringr::str_remove(., "%(?=-)") %>% stringr::str_replace_all("\\.", ",")
          dplyr::select(-res_min, - res_max)  #stringr::str_pad(res_min, max(stringr::str_length(res_min[res_min != res_max])))
      }
    }


    #Numeric variables :
    if(any(sup_cols_num)) {
      if (no_var3 == FALSE) {
        sup_cols_tabs_num <-
          dplyr::bind_rows(
            dat_group31 %>%
              sup_tab_calc(sup_cols[sup_cols_num], sup_cols_text[sup_cols_num], .zone = "sup_cols", confidence_intervals = confidence_intervals),
            dat %>% dplyr::group_by(!!var1, .drop = FALSE) %>%
              sup_tab_calc(sup_cols[sup_cols_num], sup_cols_text[sup_cols_num], .zone = "sup_cols", confidence_intervals = confidence_intervals),
            dat_group3 %>%
              sup_tab_calc(sup_cols[sup_cols_num], sup_cols_text[sup_cols_num], .zone = "sup_cols", confidence_intervals = confidence_intervals),
            dat %>%
              sup_tab_calc(sup_cols[sup_cols_num], sup_cols_text[sup_cols_num], .zone = "sup_cols", confidence_intervals = confidence_intervals)  )
      } else {
        sup_cols_tabs_num <-
          dplyr::bind_rows(
            dat_group31 %>% sup_tab_calc(sup_cols[sup_cols_num], sup_cols_text[sup_cols_num], .zone = "sup_cols", confidence_intervals = confidence_intervals),
            dat_group3 %>%  sup_tab_calc(sup_cols[sup_cols_num], sup_cols_text[sup_cols_num], .zone = "sup_cols", confidence_intervals = confidence_intervals))
      }


      # if (keep_unused_levels == FALSE) sup_cols_tabs_num %<>%
      #   dplyr::group_by(.SUP_NAME, !!var3, !!var1) %>%
      #   dplyr::filter(!all(weighted_n == 0))

      if (confidence_intervals == TRUE) {
        if (design_effect == TRUE) sup_cols_tabs_num %<>% dplyr::group_by(!!var3) %>% dplyr::bind_rows(var3_deff) %>%
          dplyr::mutate(deff = dplyr::last(deff)) %>% dplyr::filter(!.zone == "deff") %>% dplyr::ungroup() %>%
          dplyr::mutate(moe_res = round(moe_res * deff, 7))
        sup_cols_tabs_num %<>%
          dplyr::mutate(res_min = round(pmax(res - moe_res, 0)),   #, digits
                        res_max = round(res + moe_res),            #, digits
                        resCI = dplyr::if_else(res_min == res_max, stringr::str_c(res_min), stringr::str_c(res_min, "-", res_max) )) %>%
          dplyr::select(-res_min, - res_max)
      }
    }

    #Put together text and numeric variables
    if (any(sup_cols_text) & any(sup_cols_num)){
      sup_cols_tabs <-
        dplyr::bind_rows(sup_cols_tabs_text, sup_cols_tabs_num)
    } else if (any(sup_cols_text)) {
      sup_cols_tabs <- sup_cols_tabs_text
    } else if (any(sup_cols_num)) {
      sup_cols_tabs <- sup_cols_tabs_num
    }


    sup_cols_tabs %<>% dplyr::ungroup() %>% dplyr::mutate(!!var2 := factor("sup_cols")) %>%
      dplyr::arrange(.TYPE, .SUP_NAME, !!var3, !!var1, !!var2)

    if (only_first_level == TRUE) {
      sup_cols_tabs %<>% dplyr::group_by(!!var3, .SUP_NAME, !!var1) %>% dplyr::slice(1) %>% dplyr::ungroup()
    } else if (not_last_level == TRUE) {
      sup_cols_tabs %<>% dplyr::group_by(!!var3, .SUP_NAME, !!var1) %>% dplyr::filter(.SUP != dplyr::last(.SUP)) %>% dplyr::ungroup()
    }
  }



  #Supplementary rows :
  if (length(sup_rows) != 0 & multicols == FALSE) {
    dat_group32 <- dat_group3 %>% dplyr::group_by(!!var2, .add = TRUE, .drop = FALSE)

    sup_rows <- names(dat) %>% purrr::keep(. %in% sup_rows)
    if (missing(sup_rows_num)) sup_rows_num <- sup_rows %>%
      purrr::map_lgl(~ dplyr::pull(dat, !!rlang::sym(.)) %>% is.numeric())
    if (missing(sup_rows_text)) sup_rows_text <- sup_rows %>%
      purrr::map_lgl(~ dplyr::pull(dat, !!rlang::sym(.)) %>% is.factor() |
                       dplyr::pull(dat, !!rlang::sym(.)) %>% is.character() )

    if (confidence_intervals == TRUE & design_effect == TRUE) var3_deff <-  wtable %>% dplyr::group_by(!!var3) %>%
      dplyr::summarise(deff = dplyr::first(deff), .groups = "keep") %>% dplyr::mutate(.zone = factor("deff"))

    if(any(sup_rows_text))  {
      sup_rows_tabs_text <- dat_group32 %>%
        sup_tab_calc(sup_rows[sup_rows_text], sup_rows_text[sup_rows_text], .zone = "sup_rows", confidence_intervals = confidence_intervals)
      if (no_var3 == FALSE) {
        sup_rows_tabs_text %<>% dplyr::group_by(.SUP_NAME, !!var2, .SUP)
        if (accelerate == FALSE) {
          sup_rows_tabs_text %<>%
            dplyr::bind_rows(dplyr::summarise(., !!var3 := factor("Total"), !!var1 := factor("Total"),
                                              .TYPE = dplyr::first(.TYPE), .zone = factor("sup_rows"),
                                              n = sum(n), weighted_n = sum(weighted_n), .groups = "drop"))
        } else { #if (accelerate == TRUE)
          sup_rows_tabs_text %<>%
            dplyr::bind_rows(dplyr::summarise(., !!var3 := factor("Total"), !!var1 := factor("Total"),
                                              .TYPE = dplyr::first(.TYPE), .zone = factor("sup_rows"),
                                              weighted_n = sum(weighted_n),  .groups = "drop"))
        }
      }

      sup_rows_tabs_text %<>% dplyr::group_by(.SUP_NAME, !!var3, !!var2) %>% dplyr::mutate(.wtot2 = sum(weighted_n))
      if (accelerate == FALSE) sup_rows_tabs_text %<>% dplyr::mutate(.tot2 = sum(n))

      sup_rows_tabs_text %<>% dplyr::group_by(.SUP_NAME, !!var3) %>% dplyr::mutate(.wtot3 = sum(weighted_n))
      if (accelerate == FALSE) sup_rows_tabs_text %<>% dplyr::mutate(.tot3 = sum(n))

      if (accelerate == FALSE) {
        sup_rows_tabs_text %<>% dplyr::group_by(.SUP, .add = TRUE) %>%
          dplyr::bind_rows(dplyr::summarise(., !!var2 := factor("Total"), !!var1 := factor("Total"),
                                            n = sum(n), weighted_n = sum(weighted_n), .TYPE = dplyr::first(.TYPE),
                                            .tot2 = dplyr::first(.tot3), .wtot2 = dplyr::first(.wtot3), .zone = factor("sup_rows"),
                                            .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3), .groups = "drop") )
      } else { #if (accelerate == TRUE)
        sup_rows_tabs_text %<>% dplyr::group_by(.SUP, .add = TRUE) %>%
          dplyr::bind_rows(dplyr::summarise(., !!var2 := factor("Total"), !!var1 := factor("Total"),
                                            n = sum(n), weighted_n = sum(weighted_n), .TYPE = dplyr::first(.TYPE),
                                            .wtot2 = dplyr::first(.wtot3), .zone = factor("sup_rows"),
                                            .wtot3 = dplyr::first(.wtot3), .groups = "drop") )
      }

      #Percentages
      sup_rows_tabs_text %<>% dplyr::mutate(pct = weighted_n/.wtot2)

      #Confidence intervals:
      if (confidence_intervals == TRUE) {
        sup_rows_tabs_text %<>%
          dplyr::mutate(moe = DescTools::BinomCI(pct*.tot2, .tot2, method = "wilson",
                                                 conf.level = conf_level) %>%  as.data.frame() %>%
                          dplyr::mutate(ci = upr.ci - est) %>% dplyr::pull(ci) %>% tidyr::replace_na(0) %>% round(7))
        if (design_effect == TRUE) sup_rows_tabs_text %<>% dplyr::group_by(!!var3) %>% dplyr::bind_rows(var3_deff) %>%
          dplyr::mutate(deff = dplyr::last(deff)) %>% dplyr::filter(!.zone == "deff") %>% dplyr::ungroup() %>%
          dplyr::mutate(ci = ci * deff)

        sup_rows_tabs_text %<>%
          dplyr::mutate(conf_level = conf_level,
                        res_min = round(pmax((pct - moe)*100, 0), digits),  #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
                        res_max = round(pmin((pct + moe)*100, 100), digits), #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
                        resCI = dplyr::if_else(res_min == res_max, stringr::str_c(res_min, "%"), stringr::str_c(res_min, "-", res_max, "%") )) %>% #stringr::str_remove(., "%(?=-)") %>% stringr::str_replace_all("\\.", ",")
          dplyr::select(-res_min, - res_max)  #stringr::str_pad(res_min, max(stringr::str_length(res_min[res_min != res_max])))
      }
    }


    #Numeric variables :
    if(any(sup_rows_num)) {
      if (no_var3 == FALSE) {
        sup_rows_tabs_num <-
          dplyr::bind_rows(
            dat_group32 %>%
              sup_tab_calc(sup_rows[sup_rows_num], sup_rows_text[sup_rows_num], .zone = "sup_rows", confidence_intervals = confidence_intervals),
            dat %>% dplyr::group_by(!!var2, .drop = FALSE) %>%
              sup_tab_calc(sup_rows[sup_rows_num], sup_rows_text[sup_rows_num], .zone = "sup_rows", confidence_intervals = confidence_intervals),
            dat_group3 %>%
              sup_tab_calc(sup_rows[sup_rows_num], sup_rows_text[sup_rows_num], .zone = "sup_rows", confidence_intervals = confidence_intervals),
            dat %>%
              sup_tab_calc(sup_rows[sup_rows_num], sup_rows_text[sup_rows_num], .zone = "sup_rows", confidence_intervals = confidence_intervals)  )
      } else {
        sup_rows_tabs_num <-
          dplyr::bind_rows(
            dat_group32 %>%
              sup_tab_calc(sup_rows[sup_rows_num], sup_rows_text[sup_rows_num], .zone = "sup_rows", confidence_intervals = confidence_intervals),
            dat_group3 %>%
              sup_tab_calc(sup_rows[sup_rows_num], sup_rows_text[sup_rows_num], .zone = "sup_rows", confidence_intervals = confidence_intervals))
      }


      if (confidence_intervals == TRUE) {
        if (design_effect == TRUE) sup_rows_tabs_num %<>% dplyr::group_by(!!var3) %>% dplyr::bind_rows(var3_deff) %>%
          dplyr::mutate(deff = dplyr::last(deff)) %>% dplyr::filter(!.zone == "deff") %>% dplyr::ungroup() %>%
          dplyr::mutate(moe_res = round(moe_res * deff, 7))
        sup_rows_tabs_num %<>%
          dplyr::mutate(res_min = round(pmax(res - moe_res, 0)),   #, digits
                        res_max = round(res + moe_res),            #, digits
                        resCI = dplyr::if_else(res_min == res_max, stringr::str_c(res_min), stringr::str_c(res_min, "-", res_max) )) %>%
          dplyr::select(-res_min, - res_max)
      }
    }



    #Put together text and numeric variables
    if (any(sup_rows_text) & any(sup_rows_num)){
      sup_rows_tabs <-
        dplyr::bind_rows(sup_rows_tabs_text, sup_rows_tabs_num)
    } else if (any(sup_rows_text)) {
      sup_rows_tabs <- sup_rows_tabs_text
    } else if (any(sup_rows_num)) {
      sup_rows_tabs <- sup_rows_tabs_num
    }
    sup_rows_tabs %<>% dplyr::ungroup() %>% dplyr::mutate(!!var1 := factor("sup_rows")) %>%
      dplyr::arrange(.TYPE, .SUP_NAME, !!var3, !!var2, !!var1)

    if (only_first_level == TRUE) {
      sup_rows_tabs %<>% dplyr::group_by(!!var3, .SUP_NAME, !!var2) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
        dplyr::mutate(.SUP = forcats::fct_drop(.SUP))
    }  else if (not_last_level == TRUE) {
      sup_rows_tabs %<>% dplyr::group_by(!!var3, .SUP_NAME, !!var2) %>% dplyr::filter(.SUP != dplyr::last(.SUP)) %>% dplyr::ungroup() %>%
        dplyr::mutate(.SUP = forcats::fct_drop(.SUP))
    }

  }

  # if (length(sup_cols) != 0) wtable %<>% purrr::splice(sup_cols_tabs)
  # if (length(sup_rows) != 0) wtable %<>% purrr::splice(sup_rows_tabs)
  # wtable %<>% magrittr::set_names(purrr::map(., ~ dplyr::pull(., .zone) %>% as.character() %>% dplyr::first() ))
  if (length(sup_cols) != 0 & multirows == FALSE) wtable %<>% dplyr::bind_rows(sup_cols_tabs)
  if (length(sup_rows) != 0 & multicols == FALSE) wtable %<>% dplyr::bind_rows(sup_rows_tabs)
  #if (length(sup_cols) == 0 & length(sup_rows) == 0) wtable %<>% dplyr::mutate(.SUP = factor(NA_character_))
  wtable %<>% dplyr::select(.zone, !!var3, !!var1, !!var2, tidyselect::any_of(c(".SUP_NAME", ".SUP",
                                                                                "pct", "moe", "resCI", "n")), weighted_n,  tidyselect::any_of(c("moe_wn", "res", "moe_res", "contrib", "ctr_mean",
                                                                                                                                                "pct1", "pct2", "freq", "freqn",
                                                                                                                                                ".tot1", ".wtot1", ".tot2", ".wtot2", ".tot3", ".wtot3", ".totn", ".wtotn", "an_totcol", "an_totrow",
                                                                                                                                                "deff", "conf_level")), .TYPE,
                            tidyselect::any_of(c("expected", "spread", "binding_ratio", "ctr_abs", "Vnuage", "ctr_no_sign")),
                            tidyselect::everything())


  wtable %<>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(c("pct", "moe", "pct1", "pct2", "freq", "freqn", "contrib", "ctr_mean", "expected", "spread", "binding_ratio", "ctr_no_sign")),
                                ~ pct(., digits = digits))) %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(c("moe_wn", ".tot1", ".wtot1", ".tot2", ".wtot2", ".tot3", ".wtot3", ".totn", ".wtotn")),  #"n", "weighted_n"
                                ~ decimal(., digits = 0L))) %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(c("ctr_abs", "Vnuage")),
                                ~ decimal(., digits = digits + 2L)))

  #Format of calculated means : integer if none is < 0
  if ("res" %in% colnames(wtable) ) {
    max_quiet <- purrr::quietly(max)
    max_mean_of_num_SUP_NAME <-
      wtable %>% dplyr::filter(.TYPE == "num") %>% dplyr::group_by(.SUP_NAME) %>%
      dplyr::summarise(mean_res = mean(res, na.rm = TRUE), .groups = "drop") %>% dplyr::pull(mean_res) %>% max_quiet() %>%  .$result
    if (max_mean_of_num_SUP_NAME < 0) {
      wtable %<>% dplyr::mutate(dplyr::across(tidyselect::any_of(c("res", "moe_res")), ~ decimal(., digits = 3L)) )
    } else {
      wtable %<>% dplyr::mutate(dplyr::across(tidyselect::any_of(c("res", "moe_res")), ~ decimal(., digits = 0L)) )
    }
  }


  return(wtable)
}


#' @keywords internal
tabr_col_var_sort <- function(wtable, row_var, col_var, tab_var, sort_by) {
  row_var <- rlang::enquo(row_var)
  col_var <- rlang::enquo(col_var)
  tab_var <- rlang::enquo(tab_var)
  #Sort row variable by another variable (change the order of factor levels ;
  # can't be done before data is split by var3 => must be done while drawing the tabs)
  if (sort_by[1] != "no") {
    desc_order <- ifelse(is.na(sort_by[2] == "desc"), FALSE, sort_by[2] == "desc")
    if (sort_by[1] %in% (dplyr::pull(wtable, !!col_var) %>% levels()) ) {
      col_var_sort <-  wtable %>%
        dplyr::filter(.zone == "base" & !!col_var == sort_by[1]) %>% dplyr::group_by(!!tab_var) %>% dplyr::group_split() %>%
        purrr::map(~dplyr::mutate_at(., dplyr::vars(!!row_var), ~ forcats::fct_reorder(., pct1, .fun = sum, .desc = desc_order) %>%
                                       forcats::fct_relevel("Total", after = Inf) ) %>%
                     dplyr::pull(!!row_var) %>% levels() )  #.desc = TRUE #  #dplyr::arrange(!!row_var)

    } else if (sort_by[1] %in% (dplyr::pull(dplyr::filter(wtable, .zone == "sup_cols"), .SUP) %>% levels()) ) {

      wtable_sup_sort <- wtable %>% dplyr::filter(.zone == "sup_cols" & .SUP == sort_by[1])

      sort_text_num <- wtable_sup_sort %>% dplyr::pull(.TYPE) %>% forcats::fct_drop() %>% levels()

      if (sort_text_num == "num") {
        col_var_sort <- wtable_sup_sort %>% dplyr::group_by(!!tab_var) %>% dplyr::group_split() %>%
          purrr::map(~ dplyr::mutate_at(., dplyr::vars(!!row_var), ~ forcats::fct_reorder(., res, .fun = sum, .desc = desc_order) %>%
                                          forcats::fct_relevel("Total", after = Inf) ) %>%
                       dplyr::pull(!!row_var) %>% levels())  #.desc = TRUE #
      } else if (sort_text_num == "factor") {
        col_var_sort <- wtable_sup_sort %>% dplyr::group_by(!!tab_var) %>% dplyr::group_split() %>%
          purrr::map(~ dplyr::mutate_at(., dplyr::vars(!!row_var), ~forcats::fct_reorder(., pct, .fun = sum, .desc = desc_order) %>%
                                          forcats::fct_relevel("Total", after = Inf) ) %>%
                       dplyr::pull(!!row_var) %>% levels())  #.desc = TRUE #
      }
    } else {
      warning(stringr::str_c("Sorting column ", sort_by[1]," was found neither in levels of the second variable nor in supplementary columns")) # or another total ?
      col_var_sort <- list(levels(dplyr::pull(wtable, !!row_var)))
    }
    col_var_sort
  } else {
    col_var_sort <- list("no")
  }
}


# wtable = wtables[[1]]
# result_text = rlang::sym(result_sup_text_var[[1]])
# result_num = rlang::sym(result_sup_num_var[[1]])
# row_var = rlang::sym(".SUP")
# col_var = rlang::sym(names(wtable)[4])
# tab_var = rlang::sym(names(wtable)[2])
# zone = "sup_rows"
# tot = totals[[1]]  #subtext ?
# totaltab = "table"
# keep_unused_levels = keep_unused_levels[[1]]
# perc = "row"
# col_var_sort = "no"
# subtext = ""
# reverse_row_col = FALSE

#From the database to actual tables (need 1 new quosure for result,
#                                         and transmitted quosures for var1-2-3 )
tabdraw <-
  function(wtable, result_text, result_num, row_var, col_var, tab_var,
           zone = c("base","sup_cols", "sup_rows"), tot = c("row", "col"),
           totaltab = c("no", "line", "table"), keep_unused_levels = FALSE,
           perc = c("no", "row", "col", "all", "all_tabs"), col_var_sort = "no", subtext = "",
           reverse_row_col = FALSE) {
    result_text <- rlang::enquo(result_text)
    result_num <- rlang::enquo(result_num)
    #If row, col and tab vars are missing, it selects the standard colonne in wtab
    #  (var3 in second position, var1 in third, var2 in fourth)

    if (missing(row_var)) {row_var <- rlang::sym(names(wtable)[3])} else {row_var <- rlang::enquo(row_var)}
    if (missing(col_var)) {col_var <- rlang::sym(names(wtable)[4])} else {col_var <- rlang::enquo(col_var)}
    if (missing(tab_var)) {tab_var <- rlang::sym(names(wtable)[2])} else {tab_var <- rlang::enquo(tab_var)}

    wtable %<>%
      dplyr::select(.zone, .TYPE, tidyselect::any_of(c(".SUP_NAME", ".SUP")), !!tab_var, !!row_var, !!col_var,
                    tidyselect::any_of(c(rlang::quo_name(result_text), rlang::quo_name(result_num))), weighted_n) %>%
      dplyr::filter(.zone == zone[1])

    if (keep_unused_levels == FALSE) wtable %<>%
      dplyr::group_by(!!tab_var, !!row_var) %>% dplyr::filter(!all(weighted_n == 0)) %>% dplyr::ungroup()


    total_levels <- c("Total", "Ensemble")
    wtable %<>%
      dplyr::filter( (!("only_totrow" %in% tot) & !(!!row_var %in% total_levels)) |                  #Keep non-totals, excepted if "only_totrow" is selected
                       (!!row_var %in% total_levels & ("row" %in% tot | "only_totrow" %in% tot)) |   # Drop total row if "row" is not selected in tot
                       (!!row_var %in% total_levels & !!tab_var %in% total_levels & totaltab[1] == "line") ) %>% #Keep total line of total table anyway if "totaltab = line"
      dplyr::filter( (!("only_totcol" %in% tot) & ! (!!col_var %in% total_levels)) |                 #Keep non-totals, excepted if "only_totcol" is selected
                       (!!col_var %in% total_levels & ("col" %in% tot | "only_totcol" %in% tot)) ) %>% # Drop total col if "col" is not selected in tot
      dplyr::filter( !(!!tab_var %in% total_levels) |                                                 # Keep total table or total line if selected
                       (!!tab_var %in% total_levels & totaltab[1] == "table") |
                       (!!tab_var %in% total_levels & !!row_var %in% total_levels & totaltab[1] == "line") )

    #wtable %>% View()

    wtable %<>% dplyr::mutate_at(dplyr::vars(!!tab_var), ~ suppressWarnings(forcats::fct_recode(., "Ensemble" = "Total")) ) %>%
      dplyr::mutate_at(dplyr::vars(!!row_var, !!col_var), ~ suppressWarnings(forcats::fct_recode(., "Total" = "Ensemble" )))
    sheet_names <- levels(forcats::fct_drop(dplyr::pull(wtable, !!tab_var)))
    subtots_name <- stringr::str_c("Total ", stringr::str_to_upper(sheet_names, locale = "fr") )
    subtots_name_named <- subtots_name %>%  purrr::map(~ magrittr::set_names("Total" , .))
    #if (zone[1] == "base" ) {
    # general_title <- dplyr::if_else(! stringr::str_detect(rlang::quo_name(col_var), "^no_var" ),
    #                          stringr::str_c(rlang::quo_name(row_var), " par ", rlang::quo_name(col_var) ),
    #                          rlang::quo_name(row_var))
    general_title <- dplyr::case_when(
      ! stringr::str_detect(rlang::quo_name(row_var), "^no_var" ) & ! stringr::str_detect(rlang::quo_name(col_var), "^no_var" ) ~ stringr::str_c(rlang::quo_name(row_var), " par ", rlang::quo_name(col_var) ),
      stringr::str_detect(rlang::quo_name(row_var), "^no_var" ) ~ rlang::quo_name(col_var),
      stringr::str_detect(rlang::quo_name(col_var), "^no_var" ) ~ rlang::quo_name(row_var)    )
    if (! stringr::str_detect(rlang::quo_name(tab_var), "^no_var" )) {
      sheet_names <- stringr::str_c(general_title, " : ", sheet_names)
    } else {
      sheet_names <- rep(general_title, length(sheet_names))
    }
    #}


    tabdraw_core <- function(wtable, result, row_var, col_var, tab_var, #Only quosures/exprs
                             .sheet_names = sheet_names, .subtots_name = subtots_name,
                             .subtots_name_named = subtots_name_named, .reverse_row_col = reverse_row_col) {

      wtable %<>% dplyr::group_by(!!tab_var) %>% dplyr::group_split()

      if (col_var_sort[[1]][1] != "no") {
        wtable %<>% purrr::map2(col_var_sort[1:length(wtable)], ~ dplyr::mutate_at(.x, dplyr::vars(!!row_var), function(var) forcats::fct_relevel(var, .y)) %>%
                                  dplyr::arrange(.zone, .TYPE, .SUP_NAME, !!row_var, !!col_var) )
      }

      if (.reverse_row_col == FALSE) {
        wtable %>%
          purrr::map(~ tidyr::pivot_wider(., id_cols = !!row_var, names_from = !!col_var, values_from = !!result) ) %>%
          purrr::map2(.subtots_name_named[1:length(.)], ~ dplyr::mutate_at(.x, dplyr::vars(1), ~ suppressWarnings(forcats::fct_recode(., !!!.y)))) %>%
          purrr::map(~ dplyr::mutate_at(., dplyr::vars(1), ~ forcats::fct_expand(., .subtots_name))) %>%
          magrittr::set_names(.sheet_names[1:length(.)])
      } else {
        wtable %>%
          purrr::map(~ tidyr::pivot_wider(., id_cols = !!col_var, names_from = !!row_var, values_from = !!result) ) %>%
          purrr::map2(.subtots_name_named[1:length(.)], ~ dplyr::mutate_at(.x, dplyr::vars(1), ~ suppressWarnings(forcats::fct_recode(., !!!.y)))) %>%
          purrr::map(~ dplyr::mutate_at(., dplyr::vars(1), ~ forcats::fct_expand(., .subtots_name))) %>%
          magrittr::set_names(.sheet_names[1:length(.)])
      }
    }

    wtable_text <- wtable %>% dplyr::filter(.TYPE == "factor")
    wtable_num  <- wtable %>% dplyr::filter(.TYPE == "num")

    if (nrow(wtable_text) != 0) tabs_text <- wtable_text %>%
      tabdraw_core(result_text, row_var, col_var, tab_var)
    if (nrow(wtable_num) != 0) tabs_num <- wtable_num %>%
      tabdraw_core(result_num, row_var, col_var, tab_var)

    if (nrow(wtable_text) != 0 & nrow(wtable_num) != 0) {
      #tabs <- purrr::map2(tabs_text, tabs_num, ~ dplyr::full_join(.x, .y, by = rlang::quo_name(row_var)))
      tabs <- purrr::map2(tabs_text, tabs_num, purrr::quietly(~ dplyr::full_join(.x, .y))) %>% purrr::map(~ .$result)
    } else if (nrow(wtable_text) != 0) {
      tabs <- tabs_text
    } else if (nrow(wtable_num) != 0) {
      tabs <- tabs_num
    } else {
      tabs <- list(tibble::tibble())
    }

    tabs <- purrr::pmap(list(tabs,
                             rep(NA_character_, length(tabs) - 1) %>% as.list() %>% purrr::splice(list(subtext)),
                             rep(FALSE, length(tabs) - 1) %>% as.list() %>% purrr::splice(list(totaltab[1] %in% c("line", "table"))) ),
                        ~ new_single_tabr(..1, nrow = nrow(..1), perc = perc[1], subtext = ..2, total_table = ..3)  )

    return(tabs)
  }

#' @keywords internal
get_orig_name <- function(df) { #Thanks to https://stackoverflow.com/questions/30057278 ------
  i <- 1
  while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  deparse(parent.frame(i)$lhs)  #list(name = deparse(parent.frame(i)$lhs), output = df)
}


# dat <- ct2013s %>%
#   dplyr::mutate(no_var2 = "n") %>% dplyr::select(tidyselect::all_of(list_of_vars), EMP2,  no_var2, PR0, pondcal)
# var1 <- rlang::expr(EMP2)
# var2 <- rlang::expr(no_var2)
# var3 <- rlang::expr(PR0)
# wt <- rlang::expr(pondcal)
#
# perc <- "row"
# tot = c("row", "col")
# digits <- 1
# cleannames <- TRUE
# rare_levels_to_other = FALSE
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
# datbase <- dat
#
# sup_cols = list_of_vars
# sup_rows =  NULL
# sort_by = "no"
# datbase <- dat
# multicols <- TRUE
# multirows <- FALSE
#' @keywords internal
tabr_main <- function(dat, data_name,
                      var1, var2, var3, wt,
                      show_na = TRUE, keep_unused_levels = FALSE,
                      tot = "auto", perc = c("no", "row", "col", "all", "all_tabs"), totaltab = c("table", "line", "no"),
                      digits = 0, cleannames = FALSE, subtext, sort_by = "no",
                      minimum_headcount = 30, rare_levels_to_other = FALSE,
                      sup_cols = NULL, sup_rows = NULL, only_first_level = TRUE, not_last_level = TRUE, drop_sup_na = FALSE,
                      another_total = FALSE, sup_contrib = FALSE,
                      result = c("observed", "contrib", "expected", "spread", "binding_ratio", "ctr_abs", "ctr_no_sign"),
                      confidence_intervals = FALSE, conf_level = 0.95, design_effect = TRUE, #CItype = c("range", "+-"),
                      force_unique_table = FALSE, print_sup = FALSE, # no_formatting = FALSE,
                      multicols = FALSE, multirows = FALSE, accelerate = FALSE, ...) {


  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)
  var3 <- rlang::enquo(var3)
  wt <- rlang::enquo(wt)

  if (rlang::quo_name(var1) == "no_var1") {no_var1 <- TRUE} else {no_var1 <- FALSE}
  if (rlang::quo_name(var2) == "no_var2") {no_var2 <- TRUE} else {no_var2 <- FALSE}
  if (rlang::quo_name(var3) == "no_var3") {no_var3 <- TRUE} else {no_var3 <- FALSE}
  if (rlang::quo_name(wt) == "no_weight") {no_weight <- TRUE} else {no_weight <- FALSE}

  dat <-
    tabr_dat_prepare(dat, !!var1, !!var2, !!var3, wt = !!wt, #no_var1, no_var2, no_var3,
                     show_na = show_na, drop_sup_na = drop_sup_na,
                     cleannames = cleannames, minimum_headcount = minimum_headcount,
                     rare_levels_to_other = rare_levels_to_other,
                     sup_cols = sup_cols, sup_rows = sup_rows, only_first_level = only_first_level)

  # tabr_dat_dplyr::select <- function(dat, var1, var2, var3, wt, sup_cols, sup_rows) {
  # dat %<>% dplyr::select(!!var3, !!var1, !!var2, !!wt, tidyselect::all_of(c(sup_cols, sup_rows))) %>%
  #   dplyr::select(where(is.factor), where(is.numeric)) %>%
  #   dplyr::select(!!var3, !!var1, !!var2, !!wt, tidyselect::everything())
  # }
  # tabr_dat_dplyr::select(dat, !!var1, !!var2, !!var3, !!wt, sup_cols, sup_rows)


  #Class sup variables in text/numeric
  sup_list <- tabr_make_sup_list(dat, sup_cols, sup_rows)


  if (cleannames == TRUE & sort_by[1] != "no") sort_by %<>% stringr::str_remove(cleannames_condition())

  wtable <-
    tabr_core(dat, !!var1, !!var2, !!var3, !!wt, #no_var1, no_var2, no_var3,
              perc, conf_level = conf_level, keep_unused_levels, minimum_headcount, digits,
              sup_list = sup_list, only_first_level, not_last_level = not_last_level, another_total = another_total,
              multicols = multicols, multirows = multirows,
              accelerate = accelerate, confidence_intervals = confidence_intervals, design_effect = design_effect)
  if (no_var3 == FALSE) wtable %<>% dplyr::mutate(!!var3 := suppressWarnings(forcats::fct_recode(!!var3, "Ensemble" = "Total")) )

  col_var_sort <- tabr_col_var_sort(wtable, !!var1, !!var2, !!var3, sort_by = sort_by)



  #Sortie des tableaux et mise en forme --------------------------------------

  #Tests :
  # tot = "no"            ; totaltab = "no"
  # tot = "no"            ; totaltab = "line"
  # tot = "no"            ; totaltab = "table"
  # tot = "row"           ; totaltab = "no"
  # tot = "row"           ; totaltab = "line"
  # tot = "row"           ; totaltab = "table"
  # tot = "col"           ; totaltab = "no"
  # tot = "col"           ; totaltab = "line"
  # tot = "col"           ; totaltab = "table"
  # tot = c("row", "col") ; totaltab = "no"
  # tot = c("row", "col") ; totaltab = "line"
  # tot = c("row", "col") ; totaltab = "table"

  # perc = "no"      ; confidence_intervals = FALSE ; result = "observed"
  # perc = "row"      ; confidence_intervals = FALSE ; result = "observed"
  # perc = "col"      ; confidence_intervals = FALSE ; result = "observed"
  # perc = "all"      ; confidence_intervals = FALSE ; result = "observed"
  # perc = "all_tabs" ; confidence_intervals = FALSE ; result = "observed"

  # perc = "row"      ; confidence_intervals = TRUE  ; result = "observed"
  # perc = "col"      ; confidence_intervals = TRUE  ; result = "observed"
  # perc = "all"      ; confidence_intervals = TRUE  ; result = "observed"
  # perc = "all_tabs" ; confidence_intervals = TRUE  ; result = "observed"
  # perc = "no"       ; confidence_intervals = TRUE  ; result = "observed"

  # perc = "row"      ; confidence_intervals = FALSE ; result = "contrib"
  # perc = "col"      ; confidence_intervals = TRUE  ; result = "contrib"
  # perc = "all"      ; confidence_intervals = FALSE ; result = "expected"
  # perc = "all_tabs" ; confidence_intervals = TRUE  ; result = "spread"
  # perc = "row"      ; confidence_intervals = FALSE ; result = "binding_ratio"
  # perc = "col"      ; confidence_intervals = TRUE  ; result = "ctr_no_sign"
  # perc = "col"      ; confidence_intervals = TRUE  ; result = "ctr_abs"

  #Faire le tableau final :

  if (result[1] == "observed") {
    if (perc[1] == "no")              result_base_var <- rlang::expr(weighted_n)
    if (perc[1] != "no")              result_base_var <- rlang::expr(pct)
    if (confidence_intervals == TRUE) result_base_var <- rlang::expr(resCI)
  } else { #ou alors : if (result %in% c("contrib", "expected", "spread", "binding_ratio", "ctr_abs", "ctr_no_sign"))
    result_base_var <- rlang::enquo(result)
  }

  if (multicols == FALSE & multirows == FALSE) {
    tabs <- wtable %>%
      tabdraw(!!result_base_var, row_var = !!var1, col_var = !!var2, tab_var = !!var3,
              tot = tot, totaltab = totaltab[1], keep_unused_levels = keep_unused_levels,
              perc = perc[1], col_var_sort = col_var_sort, subtext = subtext)
  }

  general_title <- dplyr::case_when(
    ! stringr::str_detect(rlang::quo_name(var1), "^no_var" ) & ! stringr::str_detect(rlang::quo_name(var2), "^no_var" ) ~ stringr::str_c(rlang::quo_name(var1), " par ", rlang::quo_name(var2) ),
    stringr::str_detect(rlang::quo_name(var1), "^no_var" ) ~ rlang::quo_name(var2),
    stringr::str_detect(rlang::quo_name(var2), "^no_var" ) ~ rlang::quo_name(var1)    )


  if (confidence_intervals == FALSE) {
    result_sup_text_var <- rlang::expr(pct)
    result_sup_num_var  <- rlang::expr(res)
  } else {
    result_sup_text_var <- rlang::expr(resCI)
    result_sup_num_var  <- rlang::expr(resCI)
  }

  if (multicols == TRUE) {
    tabs <- wtable %>%
      tabdraw(!!result_sup_text_var, !!result_sup_num_var, !!var1, .SUP, !!var3,
              zone = "sup_cols", tot = tot, totaltab = totaltab[1], perc = "row",
              keep_unused_levels = keep_unused_levels, col_var_sort = col_var_sort, subtext = subtext)
    multicols_tot <- wtable %>% dplyr::mutate(multicols_tot = dplyr::case_when(!!var2 == "Total" ~ as_pct(weighted_n/.wtot2))) %>%
      tabdraw(multicols_tot, tot =  append(tot, "only_totcol") %>% purrr::discard(. == "col"),
              totaltab = totaltab[1], keep_unused_levels = keep_unused_levels,
              perc = "row", col_var_sort = col_var_sort, subtext = subtext)
    tabs <- purrr::map2(tabs, multicols_tot, ~ dplyr::left_join(.x, .y, by = rlang::quo_name(var1)))
  }

  if (multirows == TRUE) {
    tabs <- wtable %>%
      tabdraw(!!result_sup_text_var, !!result_sup_num_var, .SUP, !!var2, !!var3,
              zone = "sup_rows", tot = tot, totaltab = totaltab[1], perc = "col",
              keep_unused_levels = keep_unused_levels, subtext = subtext) %>%
      purrr::map(~ dplyr::mutate(., dplyr::across(where(is_decimal), as_pct)))
    multirows_tot <- wtable %>% dplyr::mutate(multirows_tot = dplyr::case_when(!!var1 == "Total" ~ as_pct(weighted_n/.wtot1))) %>%
      tabdraw(multirows_tot, tot = append(tot, "only_totrow") %>% purrr::discard(. == "row"),
              totaltab = totaltab[1], keep_unused_levels = keep_unused_levels,
              perc = "col", col_var_sort = col_var_sort, subtext = subtext) %>%
      purrr::map(~dplyr::rename_at(., 1, ~ ".SUP") )
    if (no_var3 == FALSE & totaltab[1] == "line") tabs %<>%
      purrr::splice(dplyr::filter(.[[length(.)]], FALSE)) %>% magrittr::set_names(names(multirows_tot))
    tabs <- purrr::map2(tabs, multirows_tot, ~ tibble::add_row(.x, .y))
  }

  #Tables of supplementary cols and rows
  count_text <- stringi::stri_unescape_unicode("Individus enqu\\u00eat\\u00e9s")
  Chi2_text  <- stringi::stri_unescape_unicode("Prob. du Chi\\u00b2")
  if (accelerate == FALSE) {
    if (multicols == FALSE & multirows == FALSE) {

      if (print_sup == TRUE) {
        if (length(sup_cols) != 0) sup_cols_tabs <- wtable %>%
            tabdraw(!!result_sup_text_var, !!result_sup_num_var, !!var1, .SUP, !!var3,
                    zone = "sup_cols", tot = tot, totaltab = totaltab[1], perc = "col",
                    keep_unused_levels = keep_unused_levels, col_var_sort = col_var_sort, subtext = subtext)

        sup_rows_condition_compatibilities <- confidence_intervals == TRUE |
          ! (((perc[1] == "no" | result[1] == "ctr_abs") & any(sup_list$sup_rows_text)) |
               (perc[1] != "no" & result[1] != "ctr_abs" & any(sup_list$sup_rows_num )))

        if (length(sup_rows) != 0 & sup_rows_condition_compatibilities) {
          sup_rows_tabs <- wtable %>%
            tabdraw(!!result_sup_text_var, !!result_sup_num_var, .SUP, !!var2, !!var3,
                    zone = "sup_rows", tot = tot, totaltab = "table", perc = "row",
                    keep_unused_levels = keep_unused_levels, subtext = subtext)
        }

        if (length(sup_rows) != 0 & ! sup_rows_condition_compatibilities) {
          warning("cannot print supplementary row in console, because vector type is different from tabs columns : set print_sup = FALSE and export to Excel with tabxl")
        }

        if (length(sup_cols) != 0) tabs <-
          purrr::map2(purrr::map(tabs, ~dplyr::mutate(., `>> SUP >>` = "|")), sup_cols_tabs,
                      ~ dplyr::full_join(.x, .y, by = c(colnames(.y)[1] %>% magrittr::set_names(colnames(.x)[1]))))

        if (length(sup_rows) != 0 & sup_rows_condition_compatibilities) tabs <-
          purrr::map2(tabs, sup_rows_tabs, ~ tibble::add_row(.x, !!var1 := factor(
            strrep("-", min(max(stringr::str_length(dplyr::pull(.x, 1)), na.rm = TRUE), 30)))) %>%
              dplyr::bind_rows(dplyr::rename(.y, !!var1 := .SUP)) %>%
              dplyr::mutate_at(dplyr::vars(tidyselect::any_of(">> SUP >>")), ~ tidyr::replace_na(., " "))) #There were unbreakable space ??
      }


      #Chi2 test (remove cols or lines with just zeros) :
      if (no_var2 == FALSE & no_var1 == FALSE) {
        pvalue <- wtable %>%
          tabdraw(n, row_var = !!var1, col_var = !!var2, tab_var = !!var3,
                  tot = "no", totaltab = stringr::str_replace(totaltab, "line", "no"),
                  keep_unused_levels = keep_unused_levels, perc = "no", col_var_sort = col_var_sort) %>%
          purrr::map(~ dplyr::mutate(., dplyr::across(where(is.numeric), as.double)) %>%
                       tibble::column_to_rownames(rlang::quo_name(var1)) %>%
                       dplyr::select_if(colSums(.) != 0) %>% dplyr::filter(rowSums(.) != 0)  ) %>%
          purrr::map(purrr::possibly(purrr::quietly(~ stats::chisq.test(.)), tibble::tibble(warnings = "", result = tibble::tibble(p.value = NA_real_)) ) )
        pvalue_warning <- pvalue %>% purrr::map(~ .$warnings) %>% purrr::map_if(purrr::map_lgl(., ~ length(.) == 0), ~ "")
        pvalue <- pvalue %>% purrr::map(~ .$result) %>% purrr::map_dbl(~ .$p.value) %>% as_pct(digits = 6)
        if (no_var3 == FALSE & totaltab[1] == "line") {
          pvalue %<>% append(c("Ensemble" = pct(NA_real_)))
          pvalue_warning %<>% append("")
        }

      } else { #If there is no var2
        pvalue <- purrr::map_int(1:length(tabs), ~ 0L) %>% as_pct()
        pvalue_warning <- purrr::map_chr(1:length(tabs), ~ "")
      }

      pvalue_Chi2 <-
        dplyr::summarise(dplyr::group_by(wtable, !!var3),
                         !!count_text := dplyr::first(.tot3),
                         "Variance" = dplyr::first(Vnuage), .groups = "drop") %>%
        dplyr::mutate_at(3, ~ set_digits(., 5)) %>%
        tibble::add_column(!!Chi2_text := pvalue,
                           "warning" = pvalue_warning) %>%
        dplyr::rename_at(1, ~ "Tableaux")
      if (no_var3 == FALSE & totaltab[1] == "no") pvalue_Chi2 <-
        pvalue_Chi2[-nrow(pvalue_Chi2),]
      if (no_var3 == TRUE) pvalue_Chi2 %<>% dplyr::mutate_at(1, ~ general_title)
      pvalue_Chi2 %<>% dplyr::mutate_at(dplyr::vars(1), ~ dplyr::if_else(
        stringr::str_detect(warning, "incorrect"),
        stringr::str_c(., " (!)"),
        as.character(.))) %>%
        dplyr::select(-warning) #%>% dplyr::mutate_at(1, ~dplyr::if_else(`Prob. du Chi\\u00b2` > 0.5, crayon::red(.), crayon::green(.)))

      if (print_sup == TRUE) {
        if (length(sup_cols) != 0 & length(sup_rows) != 0) {
          tabs %<>% purrr::map(~ `attr<-`(., "print_sup", c(TRUE, TRUE)))
        } else if (length(sup_cols) != 0) {
          tabs %<>% purrr::map(~ `attr<-`(., "print_sup", c(TRUE, FALSE)))
        } else if (length(sup_rows) != 0) {
          tabs %<>% purrr::map(~ `attr<-`(., "print_sup", c(FALSE, TRUE)))
        }
      }

    } else { # If multicols or multirows
      pvalue <- purrr::map_dbl(1:length(tabs), ~ NA_real_) %>% as_pct()
      pvalue_Chi2 <-
        dplyr::summarise(dplyr::group_by(wtable, !!var3),
                         !!count_text := dplyr::first(.tot3),
                         "Variance" = as_decimal(NA_real_), .groups = "drop") %>%
        dplyr::mutate_at(3, ~ set_digits(., 5))

      if (no_var3 == FALSE & totaltab[1] == "no") pvalue_Chi2 <- pvalue_Chi2[-nrow(pvalue_Chi2),]
      pvalue_Chi2 %<>% tibble::add_column(!!Chi2_text := pvalue) %>%
        dplyr::rename_at(1, ~ "Tableaux")
      if (no_var3 == TRUE) pvalue_Chi2 %<>% dplyr::mutate_at(1, ~ general_title)
    }

  } else { # if (accelerate == TRUE)
    pvalue <- purrr::map_dbl(1:length(tabs), ~ NA_real_) %>% as_pct()
    pvalue_Chi2 <-
      dplyr::summarise(dplyr::group_by(wtable, !!var3),
                       !!count_text := as_decimal(NA_real_),
                       "Variance" = as_decimal(NA_real_), .groups = "drop") %>%
      dplyr::mutate_at(3, ~ set_digits(., 5))

    if (no_var3 == FALSE & totaltab[1] == "no") pvalue_Chi2 <-
      pvalue_Chi2[-nrow(pvalue_Chi2),]
    pvalue_Chi2 %<>% tibble::add_column(!!Chi2_text := pvalue) %>%
      dplyr::rename_at(1, ~ "Tableaux")
    if (no_var3 == TRUE) pvalue_Chi2 %<>% dplyr::mutate_at(1, ~ general_title)
  }

  if (force_unique_table == TRUE & length(tabs) >= 2) {
    tabs <- tabs %>%
      purrr::map_if(1:length(.) != length(.), ~ tibble::add_row(., !!var1 := factor(
        strrep("_", min(max(stringr::str_length(dplyr::pull(.x, 1)), na.rm = TRUE), 30)))) %>%
          dplyr::mutate_at(dplyr::vars(tidyselect::any_of("|")), ~ tidyr::replace_na(., " "))  ) %>% #There were unbreakable space ??
      dplyr::bind_rows() %>% new_single_tabr(nrow = nrow(.), perc = perc, pvalue_Chi2 = pvalue_Chi2, total_table = FALSE,
                                             subtext = subtext, force_unique_table = TRUE) %>% list() %>% magrittr::set_names(general_title)
  } else { #If multiple tables
    tabs %<>% purrr::map2(dplyr::group_split(dplyr::rowwise(pvalue_Chi2))[1:length(.)], ~ `attr<-`(.x, "pvalue_Chi2", .y))
  }



  #Set metadatas :
  # Many where put into tabs while changing their classes to "single_tabr" (tabdraw function).
  # Now we want to create a list with class "tabr", and put attributes into it.

  #General attributes, always attached to the tabr
  result_var <-  c("result_base_var"     = rlang::quo_name(result_base_var), #NA_character_), dplyr::if_else(multicols == FALSE & multirows == FALSE,
                   "result_sup_text_var" = dplyr::if_else(length(sup_cols) != 0 | length(sup_rows) != 0,
                                                          rlang::quo_name(result_sup_text_var),
                                                          NA_character_),
                   "result_sup_num_var"  = dplyr::if_else(length(sup_cols) != 0 | length(sup_rows) != 0,
                                                          rlang::quo_name(result_sup_num_var),
                                                          NA_character_)         )

  if (multicols == TRUE | multirows == TRUE) sup_cols <- sup_rows <- NULL

  #ADD : print_sup ?
  args <- list(#"original_data" = original_data,
    "original_data_name" = data_name,
    "perc" = perc[1],
    "wt" = dplyr::if_else(no_weight == FALSE, rlang::quo_name(wt), NA_character_),
    "show_na" = show_na,
    "keep_unused_levels" = keep_unused_levels,
    "minimum_headcount" = minimum_headcount,
    "totals" = tot,
    "totaltab" = totaltab[1],
    "digits" = digits,
    "cleannames" = cleannames,
    "rare_levels_to_other"= rare_levels_to_other,
    "sup_cols" = sup_list$sup_cols,
    "sup_rows" = sup_list$sup_rows,
    "multicols" = multicols,
    "multirows" = multirows,
    "sup_cols_num" = sup_list$sup_cols_num,
    "sup_rows_num" = sup_list$sup_rows_num,
    "only_first_level" = only_first_level,
    "drop_sup_na" = drop_sup_na,
    "another_total" = another_total,
    "sup_contrib" = sup_contrib,
    "confidence_intervals" = confidence_intervals,
    "conf_level" = conf_level,
    "subtext" = subtext)

  tabs <-
    new_tabr(tabs,  args = args, col_var_sort = col_var_sort, result_var = result_var, pvalue_Chi2 = pvalue_Chi2, wtable = wtable)

  #Si tableau unique, afficher le single_tabr, et attacher le tabr en attribut
  if (length(tabs) == 1) {
    tabs <- tabs[[1]] %>% `attr<-`("tabr", tabs) %>%
      `attr<-`("is_unique_table", TRUE)
  }

  #if (force_unique_table == TRUE){ tabs %<>% purrr::map(~ dplyr::rename_at(., dplyr::vars(1), ~ rlang::quo_name(var1)))
  #tabs %<>% purrr::map(~ dplyr::rename_at(., dplyr::vars(1), ~ title) )

  # if (!is.null(original_tablist)) {
  #   if (is_tabr(original_tablist)) {
  #     tabs <- append(list(original_tablist), list(tabs))
  #   } else {
  #     tabs <- append(original_tablist, list(tabs))
  #   }
  # }

  return(tabs)
}

















