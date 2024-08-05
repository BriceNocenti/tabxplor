#Import data.table in NAMESPACE :
#' Internal data.table methods
#' @import data.table
#' @keywords internal
#' @name tabxplor-data.table
NULL


# To possibly add :
# #            - choose to print % sign or not
# #            - supplementary total with unweighted counts by rows ?
# #            - rename variables if "NA", "NULL", "Total", "Ensemble", "no_var", etc.
# #            - unweighted counts in the title of each graph.
# #            - error when after cleannames, two levels have the same name ("P6Q_27-OQ-A aliment PME" / "P6Q_28-OQ-A aliment PME")
# #            - error with empty tabs when calculating Chi2

# #' @examples
# #' tab(forcats::gss_cat, marital, race)
# #'
# #' tab(forcats::gss_cat, marital, race, perc = "row")
# #'
# #' tab(forcats::gss_cat, marital, race, year, perc = "row")
# #'
# #' dplyr::storms %>%
# #'   tab(status, category) %>%
# #'   tab_sup(sup_rows = c("pressure", "wind"), print_sup = TRUE)
# #'
# #' \donttest{
# #' forcats::gss_cat %>%
# #'   tab(marital, race, perc = "row") %>%
# #'   tab_xl()
# #' }
# #'
# #' # To program several tables with different parameters at the same time :
# #' purrr::pmap(
# #'   tibble::tribble(
# #'     ~var1    , ~var2       ,  ~perc,
# #'     "marital", "race"      ,  "no" ,
# #'     "marital", "race"      ,  "row",
# #'     "marital", "race"      ,  "col",
# #'     "relig"  , "race"      ,  "no" ,
# #'     "relig"  , "race"      ,  "row",
# #'     "relig"  , "race"      ,  "col",
# #'   ),
# #'   .f = tab,
# #'   data = forcats::gss_cat, sort_by = c("White", "desc")) #%>%
# #' #tab_xl(only_one_sheet = TRUE)
# tab_last <- function() {"Nothing"}


# MAIN USER-FRIENDLY FUNCTIONS ###########################################################


#' Single cross-table, with color helpers
#' @description A full-featured function to create, manipulate and format single
#' cross-tables, using colors to make the printed tab more easily readable
#' (in R terminal or exported to Excel with \code{\link{tab_xl}}).
#' Since objects of class \code{tab} are also of class \code{tibble}, you can then use all
#' \pkg{dplyr} verbs to modify the result, like \code{\link[dplyr:select]{select}},
#' like \code{\link[dplyr:arrange]{arrange}}, \code{\link[dplyr:filter]{filter}}
#' or \code{\link[dplyr:mutate]{mutate}}.
#' Wrapper around the more powerful \code{\link{tab_many}}.
#' @param data A data frame.
#' @param row_var,col_var The row variable, which will be printed with one level per line,
#'  and the column variable, which will be printed with one level per column.
#'  For numeric variables means are calculated, in a single column.
#' @param tab_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
#' a subtable is made for each combination of levels of the selected variables.
#' Leave empty to make a simple cross-table. All \code{tab_vars} are converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param sup_cols <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' Supplementary columns variables, with only the first level printed, and row percentages
#' (for numeric variables, a mean will be calculated for each \code{row_var}).
#' To pass many variables you may use syntax \code{sup_cols = c(sup_col1, sup_col2, ...)}.
#' To keep all levels of other \code{col_vars}, or other types of percentages,
#' use \code{\link{tab_many}} instead.
#' @param na The policy to adopt for missing values, as a single string :
#'  \itemize{
#'   \item \code{"keep"}: by default, \code{NA}'s of row, col and tab variables
#'   are printed as an explicit `"NA"` level.
#'   \item \code{"drop"}: remove `NA`'s in row, col and tab variables before calculations
#'   are done. Supplementary columns are then calculated for observations with no `NA` in
#'   any of the row, col and tab variables.
#'   }
#' @param digits The number of digits to print, as a single integer. To print a different
#' number of digits for each \code{sup_cols}, an integer vector of length
#' 1 + \code{sup_cols} (the first being the number of digits for the base table).
#' @param totaltab The total table, if there are subtables/groups
#' (i.e. when \code{tab_vars} is provided) :
#'  \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param tot The totals :
#'  \itemize{
#'   \item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
#'   columns.
#'   \item \code{"row"}: only total rows.
#'   \item \code{"col"}: only total column.
#'   \item \code{"no"}: remove all totals (after calculations if needed).
#'  }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param pct The type of percentages to calculate :
#'  \itemize{
#'   \item \code{"row"}: row percentages.
#'   \item \code{"col"}: column percentages.
#'   \item \code{"all"}: frequencies for each subtable/group, if there is \code{tab_vars}.
#'   \item \code{"all_tabs"}: frequencies for the whole (set of) table(s).
#' }
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
#' @param ref2 A second reference cell is needed to calculate odds ratios
#' (or relative risks ratios). The first cell of the row or column is used by default.
#' See `ref` above for the full list of possible values.
#' @param comp The comparison level : by subtables/groups, or for the whole table.
#' \itemize{
#'   \item \code{"tab"}: by default, contributions to variance,
#' row differences from totals/first cells, and row confidence intervals for these
#' differences, are calculated for each \code{tab_vars} group.
#'   \item \code{"all"}: compare cells to the general total line (provided there is
#'    a total table with a total row), or with the first line of the total table
#'    when \code{ref = "first"}.
#' }
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios
#'   (for binary variables) or relative risks ratios (for variables with 3 levels
#'   or more).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' @param chi2 Set to \code{TRUE} to calculate Chi2 summaries with \code{\link{tab_chi2}}.
#' Useful to print metadata, and to color cells based on their contribution to variance
#'  (\code{color = "contrib"}). Automatically added if needed for \code{color}.
#' @param ci The type of confidence intervals to calculate, passed to \code{\link{tab_ci}}
#'  (automatically added if needed for \code{color}).
#'   \itemize{
#'    \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'    \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'    relative total cell (or relative first cell when \code{ref = "first"}).
#'    \item \code{"auto"}: \code{ci = "diff"} for means and row/col percentages,
#'      \code{ci = "cell"} for frequencies ("all", "all_tabs").
#'   }
#'  By default, for percentages, with \code{ci = "cell"} Wilson's method is used,
#'  and with \code{ci = "diff"} Wald's method along Agresti and Caffo's adjustment.
#'  Means use classic method. This can be changed in \code{\link{tab_ci}}.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
# @param ci_visible By default, confidence intervals are calculated and used to set
# colors, but not printed. Set to \code{TRUE} to print them in the result.
#' @param color The type of colors to print, as a single string :
#'  \itemize{
#'   \item \code{"no"}: by default, no colors are printed.
#'   \item \code{"diff"}: color percentages and means based on cells differences from
#'   totals (or from first cells when \code{ref = "first"}).
#'   \item \code{"diff_ci"}: color pct and means based on cells differences from totals
#'   or first cells, removing coloring when the confidence interval of this difference
#'   is higher than the difference itself.
#'   \item \code{"after_ci"}: idem, but cut off the confidence interval from the
#'   difference first.
#'   \item \code{"contrib"}: color cells based on their contribution to variance
#'   (except mean columns, from numeric variables).
#'   \item \code{"OR"}: for `pct == "col"` or `pct == "row"`,
#'   color based on odds ratios (or relative risks ratios)
#'   \item \code{"auto"}: frequencies (\code{pct = "all"}, \code{pct = "all_tabs"})
#'   and counts are colored with \code{"contrib"}.
#'   When \code{ci = "diff"}, row and col percentages are colored with "after_ci" ;
#'   otherwise they are colored with "diff".
#'  }
#' @param subtext A character vector to print rows of legend under the table.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like "1-", and text in parenthesis. All data formatting arguments are
#' passed to \code{\link{tab_prepare}}.
#' @param other_if_less_than When set to a positive integer, levels with less count
#' than it will be merged into an "Others" level.
#' @param other_level The name of the "Other" level, as a single string.
#' @param filter A \code{\link[dplyr:filter]{dplyr::filter}} to apply to the data frame
#' first, as a single string (which will be converted to code, i.e. to a call).
#' Useful when printing multiples tabs with \code{\link[tibble:tribble]{tibble::tribble}},
#' to use different filters for similar tables or simply make the field of observation
#' more visible into the code.
# @param ... Arguments to pass to \code{\link{tab_ci}} and \code{\link{tab_chi2}}.
#'
#' @return A \code{tibble} of class \code{tab}, possibly with colored reading helpers.
#' All non-text columns are of class \code{\link{fmt}}, storing all
#' the data necessary to print formats and colors. Columns with \code{row_var} and
#' \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples # A simple cross-table:
#' tab(forcats::gss_cat, marital, race)
#'
# # With one numeric row or col variables it calculates means by category:
# tab(forcats::gss_cat, marital, age)
#'
#' # With more variables provided, `tab` makes a subtables for each combination of levels:
#' \donttest{
#' tab(forcats::gss_cat, marital, tab_vars = c(year, race))
#'}
#'
#' # You can also add supplementary columns, text or numeric:
#' \donttest{
#' tab(dplyr::storms, category, status, sup_cols = c("pressure", "wind"))
#'}
#'
#' # Colors to help the user read the table:
#' data <- forcats::gss_cat %>%
#'   dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
#' gss  <- "Source: General social survey 2000-2014"
#' gss2 <- "Source: General social survey 2000, 2006 and 2012"
#'
#' # Differences between the cell and it's subtable's total cell:
#' \donttest{
#' tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff")
#' }
#'
#' # Differences between the cell and the whole table's general total cell:
#' \donttest{
#' tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff",
#'   comp = "all")
#' }
#'
#' # Historical differences:
#' \donttest{
#' data2 <- data %>% dplyr::mutate(year = as.factor(year))
#' tab(data2, year, marital, race, subtext = gss2, pct = "row",
#'     color = "diff", ref = "first", tot = "col")
#'
#'
#' # Differences with the total, except if their confidences intervals are superior to them:
#' tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "diff_ci")
#'
#' # Same differences, minus their confidence intervals:
#' tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "after_ci")
#'
#' # Contribution of cells to table's variance, like in a correspondence analysis:
#' tab(forcats::gss_cat, race, marital, subtext = gss, color = "contrib")
#'}
#'
#' # Since the result is a tibble, you can use all dplyr verbs to modify it :
#' \donttest{
#' library(dplyr)
#' tab(dplyr::storms, category, status, sup_cols = c("pressure", "wind")) %>%
#'   dplyr::filter(category != "-1") %>%
#'   dplyr::select(-`tropical depression`) %>%
#'   dplyr::arrange(is_totrow(.), desc(category))
#'}
#'
#'\donttest{
#' # With `dplyr::arrange`, don't forget to keep the order of tab variables and total rows:
#' tab(data, race, marital, year, pct = "row") %>%
#'   dplyr::arrange(year, is_totrow(.), desc(Married))
#'   }
tab <- function(data, row_var, col_var, tab_vars, wt, sup_cols,
                pct = "no", color = "no", OR = "no", chi2 = FALSE,
                na = "keep",  cleannames = NULL,
                other_if_less_than = 0, other_level = tr_("Others"),
                ref = "auto", ref2 = "first", comp = "tab",
                ci = "no", conf_level = 0.95,
                totaltab = "line", totaltab_name = tr_("Ensemble"),
                tot = c("row", "col"), total_names = tr_("Total"),
                subtext = "", digits = 0,
                filter) {

  cleannames <-
    if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}

  row_var_quo <- rlang::enquo(row_var)
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("n"))
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::ensym(row_var)
  }

  col_var_quo <- rlang::enquo(col_var)
  if (quo_miss_na_null_empty_no(col_var_quo)) {
    data <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_var <- "no_col_var"
  } else {
    col_var <- as.character(rlang::ensym(col_var))
  }

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character()
  } else {
    tab_vars <- names(tidyselect::eval_select(tab_vars, data))
  }

  sup_cols <- rlang::enquo(sup_cols)
  if (quo_miss_na_null_empty_no(sup_cols)) {
    sup_cols <- character()
  } else {
    sup_cols <- names(tidyselect::eval_select(sup_cols, data))
  }

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }

  vctrs::vec_assert(comp, size = 1)
  vctrs::vec_assert(color, size = 1)
  vctrs::vec_assert(pct  , size = 1)
  vctrs::vec_assert(ref , size = 1)
  vctrs::vec_assert(ref2, size = 1)
  vctrs::vec_assert(na, size = 1)
  stopifnot(na %in% c("keep", "drop"))

  na_drop_all <- switch(na,
                        "keep" = character(),
                        "drop" = c(as.character(row_var), as.character(col_var),
                                   as.character(tab_vars)))

  stopifnot(all(tot %in% c("row", "col", "both", "no", "")))
  if (tot[1] == "both") tot <- c("row", "col")


  tab_many(data = data, row_vars = !!row_var,
           col_vars = tidyselect::all_of(c(col_var, sup_cols)),
           tab_vars = tidyselect::all_of(tab_vars),
           wt = !!wt,
           levels = c("all", rep("first", length(sup_cols))),
           na = na, na_drop_all = tidyselect::all_of(na_drop_all),
           filter = if (!missing(filter)) !!rlang::enquo(filter),
           digits = digits, cleannames = cleannames,
           other_if_less_than = other_if_less_than, other_level = other_level,
           totaltab = totaltab, totaltab_name = totaltab_name,
           totrow = "row" %in% tot,
           totcol = if ("col" %in% tot) { col_var } else { "no" }, # ?
           total_names = total_names,
           pct  = c(pct , rep("row", length(sup_cols))),
           ref = ref, ref2 = ref2, #c(ref, rep(ref , length(sup_cols))),
           comp = comp,
           chi2 = chi2,
           ci = ci,
           conf_level = conf_level,
           OR = OR,
           color = color,
           subtext = subtext)
}




#' Many cross-tables as one, with color helpers
#' @description A full-featured function to create, manipulate and format many cross-tables
#' as one, using colors to make the printed tab more easily readable (in R terminal or
#' exported to Excel with \code{\link{tab_xl}}).
#' Since objects of class \code{tab} are also of class \code{tibble}, you can then use all
#' \pkg{dplyr} verbs to modify the result, like \code{\link[dplyr:select]{select}},
#' \code{\link[dplyr:arrange]{arrange}}, \code{\link[dplyr:filter]{filter}}
#' or \code{\link[dplyr:mutate]{mutate}}.
#' @param data A data frame.
#' @param row_vars The row variable, which will be printed with one level per line.
#' If numeric, it will be converted to factor. If more than one row_var if provided,
#' a different table is made for each of them.
#' @param col_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' One column is printed for each level of each column variable.
#' For numeric variables means are calculated, in a single column.
#' To pass many variables you may use syntax \code{col_vars = c(col_var1, col_var2, ...)}.
#' @param tab_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' One subtable is made for each combination of levels of the tab variables.
#' To pass many variables you may use syntax \code{tab_vars = c(tab_var1, tab_var2, ...)}.
#' All tab variables are converted to factor. Leave empty to make a simple table.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param levels The levels of \code{col_vars} to keep (for more complex selections
#'  use \code{\link[dplyr:select]{dplyr::select}}). The argument is vectorised over `col_vars`.
#' \itemize{
#'   \item \code{"all"}: by default, all levels are kept.
#'   \item \code{"first"}: only keep the first level of each \code{col_vars}
#'   \item \code{"auto"}: keep the first level when `col_var` is only two levels,
#'   keep all levels otherwise
#'   }
#' @param na The policy to adopt with missing values. It must be a single string.
#' \itemize{
#'   \item \code{na = "keep"}: by default, prints \code{NA}'s as explicit \code{"NA"} level.
#'   \item \code{na = "drop"}: removes \code{NA} levels before making each table
#'   (tabs made with different column variables may have a different number of
#'   observations, and won't exactly have the same total columns).
#'   \item \code{"drop_all"}: remove `NA`'s for all variables before making the tables.
#'   }
#' @param na_drop_all <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' Removes all observations with a `NA` in any of the chosen variables, for all tables
#' (tabs for each column variable will have the same number of observations).
#' @param digits The number of digits to print, as a single integer, or an integer vector
#' the same length as \code{col_vars}. The argument is vectorisez over `col_vars`.
#' @param totaltab The total table, if there are subtables/groups
#'  (i.e. when \code{tab_vars} is provided). Vectorised over `row_vars`.
#' \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param totrow By default, total rows are printed.
#' Set to \code{FALSE} to remove them (after calculations if needed).
#' Vectorised over `row_vars`.
#' @param totcol The policy with total columns. Vectorised over `col_vars`.
#' \itemize{
#'   \item \code{"last"}: by default, only prints a total column for the last
#'   column variable (of class factor, not numeric).
#'   \item \code{"each"}: print a total column for each column variable.
#'   \item \code{"no"}: remove all total columns (after calculations if needed).
#' }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param pct The type of percentages to calculate :
#' \itemize{
#'   \item \code{"row"}: row percentages.
#'   \item \code{"col"}: column percentages.
#'   \item \code{"all"}: frequencies for each subtable/group, if there is \code{tab_vars}.
#'   \item \code{"all_tabs"}: frequencies for the whole (set of) table(s).
#' }
#' The argument is vectorised over both `row_vars` and `col_vars`. You can then write as
#'  the following :
#' `pct = list(row_var1 = list("row", "col", "col"), row_var2 = list("col", "row", "row"))`
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
#' @param ref2 A second reference cell is needed to calculate odds ratios
#' (or relative risks ratios). The first cell of the row or column is used by default.
#' See `ref` above for the full list of possible values.
#' @param comp The comparison level : by subtables/groups, or for the whole table.
#' Vectorised over `row_vars`.
#' \itemize{
#'   \item \code{"tab"}: by default, contributions to variance,
#' row differences from totals/first cells, and row confidence intervals for these
#' differences, are calculated for each \code{tab_vars} group.
#'   \item \code{"all"}: compare cells to the general total line (provided there is
#'    a total table with a total row), or with the reference line of the total table
#'    when \code{ref = "first"}, an integer or a regular expression.
#' }
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios
#'   (for binary variables) or relative risks ratios (for variables with 3 levels
#'   or more).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' @param chi2 Set to \code{TRUE} to calculate Chi2 summaries with \code{\link{tab_chi2}}.
#' Useful to print metadata, and to color cells based on their contribution to variance
#'  (\code{color = "contrib"}). Vectorised over `row_vars`.
#' @param ci The type of confidence intervals to calculate, passed to \code{\link{tab_ci}}.
#' Vectorised over `row_vars`.
#'   \itemize{
#'    \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'    \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'    relative total cell (or relative first cell when \code{ref = "first"}).
#'    \item \code{"auto"}: \code{ci = "diff"} for means and row/col percentages,
#'    \code{ci = "cell"} for frequencies ("all", "all_tabs").
#'   }
#'  By default, for percentages, with \code{ci = "cell"} Wilson's method is used,
#'  and with \code{ci = "diff"} Wald's method along Agresti and Caffo's adjustment.
#'  Means use classic method. This can be changed in \code{\link{tab_ci}}.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
# @param ci_visible By default, confidence intervals are calculated and used to set
# colors, but not printed. Set to \code{TRUE} to print them in the result.
#' @param method_cell Character string specifying which method to use with percentages
#'  for \code{ci = "cell"}. This can be one out of:
#' "wald", "wilson", "wilsoncc", "agresti-coull", "jeffreys", "modified wilson",
#' "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting", "pratt",
#' "midp", "lik" and "blaker". Defaults to "wilson".
#' See \code{\link[DescTools:BinomCI]{BinomCI}}.
#' @param method_diff Character string specifying which method to use with percentages
#' for \code{ci = "diff"}. This can be one out of: "wald", "waldcc", "ac", "score",
#' "scorecc", "mn", "mee", "blj", "ha", "hal", "jp". Defaults to "ac", Wald interval with
#' the adjustment according to Agresti, Caffo for difference in proportions and
#' independent samples. See \code{\link[DescTools:BinomDiffCI]{BinomDiffCI}}.
#' @param color The type of colors to print, as a single string. Vectorised over `row_vars`.
#' \itemize{
#'   \item \code{"no"}: by default, no colors are printed.
#'   \item \code{"diff"}: color percentages and means based on cells differences from
#'   totals (or from first cells when \code{ref = "first"}).
#'   \item \code{"diff_ci"}: color pct and means based on cells differences from totals
#'   or first cells, removing coloring when the confidence interval of this difference
#'   is higher than the difference itself.
#'   \item \code{"after_ci"}: idem, but cut off the confidence interval from the
#'   difference first.
#'   \item \code{"contrib"}: color cells based on their contribution to variance
#'   (except mean columns, from numeric variables).
#'   \item \code{"OR"}: for `pct == "col"` or `pct == "row"`,
#'   color based on odds ratios (or relative risks ratios)
#'   \item \code{"auto"}: frequencies (\code{pct = "all"}, \code{pct = "all_tabs"})
#'   and counts are colored with \code{"contrib"}.
#'   When \code{ci = "diff"}, row and col percentages are colored with "after_ci" ;
#'   otherwise they are colored with "diff".
#' }
#' @param subtext A character vector to print rows of legend under the table.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like "1-", and text in parenthesis. All data formatting arguments are
#' passed to \code{\link{tab_prepare}}.
#' @param other_if_less_than When set to a positive integer, levels with less count
#' than it will be merged into an "Others" level.
#' @param other_level The name of the "Other" level, as a single string.
#' @param filter A \code{\link[dplyr:filter]{dplyr::filter}} to apply to the data frame
#' first, as a single string (which will be converted to code, i.e. to a call).
#' Useful when printing multiples tabs with \code{\link[tibble:tribble]{tibble::tribble}},
#' to use different filters for similar tables or simply make the field of observation
#' more visible into the code.
# @param ... Arguments to pass to \code{\link{tab_ci}} and \code{\link{tab_chi2}}.
#'
#' @return A \code{tibble} of class \code{tab}, possibly with colored reading helpers.
#' When there are two `row_vars` or more, a list of \code{tibble} of class \code{tab}.
#' All non-text columns are of class \code{\link{fmt}}, storing all
#' the data necessary to print formats and colors. Columns with \code{row_var} and
#' \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples # Make a summary table with many col_vars, showing only one specific level :
#' \donttest{
#' library(dplyr)
#' first_lvs <- c("Married", "$25000 or more", "Strong republican", "Protestant")
#' data <- forcats::gss_cat %>% mutate(across(
#'   where(is.factor),
#'   ~ forcats::fct_relevel(., first_lvs[first_lvs %in% levels(.)])
#' ))
#' tab_many(data, race, c(marital, rincome, partyid, relig, age, tvhours),
#'          levels = "first", pct = "row", chi2 = TRUE, color = "auto")
#'}
#'
#' # Can be used with map and tribble to program several tables with different parameters
#' #  all at once, in a readable way:
#' \donttest{
#' library(purrr)
#' library(tibble)
#' pmap(
#'   tribble(
#'     ~row_var, ~col_vars       , ~pct , ~filter              , ~subtext               ,
#'     "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
#'     "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
#'     NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
#'   ),
#'   .f = tab_many,
#'   data = forcats::gss_cat, color = "auto", chi2 = TRUE)
#' }
tab_many <- function(data, row_vars, col_vars, tab_vars, wt,
                     pct = "no", color = "no", OR = "no", chi2 = FALSE,
                     na = "keep", levels = "all", na_drop_all, cleannames = NULL,
                     other_if_less_than = 0, other_level = tr_("Others"), rare_to_other,
                     ref = "auto", ref2 = "first", comp = "tab",
                     ci = "no", conf_level = 0.95, #ci_visible = FALSE,
                     method_cell = "wilson", method_diff = "ac",
                     totaltab = "line", totaltab_name = tr_("Ensemble"),
                     totrow = TRUE, totcol = "last", total_names = tr_("Total"),
                     digits = 0, subtext = "",
                     filter #, listed = FALSE,
                     #spread_vars = NULL, names_prefix, names_sort = FALSE
) {

  cleannames <-
    if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}

  stopifnot(levels %in% c("first", "all", "auto"))
  lvs <- levels

  row_vars <- rlang::enquo(row_vars)
  if (quo_miss_na_null_empty_no(row_vars)) {
    data     <- data %>% dplyr::mutate(no_row_var = factor("n"))
    row_vars <- rlang::syms("no_row_var")
    pos_row_vars <- tidyselect::eval_select("no_row_var", data)
  } else {
    pos_row_vars <- tidyselect::eval_select(row_vars, data)
    row_vars     <- rlang::syms(names(pos_row_vars))
  }
  # row_vars_num  <- purrr::map_lgl(data[pos_row_vars], is.numeric)
  # row_vars_text <- purrr::map_lgl(data[pos_row_vars],
  #                                 ~ is.factor(.) | is.character(.))

  col_vars <- rlang::enquo(col_vars)
  if (quo_miss_na_null_empty_no(col_vars)) {
    data     <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_vars <- rlang::syms("no_col_var")
    pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else {
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
  }
  col_vars_num  <- purrr::map_lgl(data[pos_col_vars], is.numeric)
  col_vars_text <- purrr::map_lgl(data[pos_col_vars],
                                  ~ is.factor(.) | is.character(.))

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    #data     <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
    tab_vars <- character() #rlang::syms("no_tab_vars")
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    #data <- data %>% dplyr::mutate(no_weight = factor("n"))
    wt <- character() #rlang::sym("no_weight")
  } else {
    wt <- rlang::ensym(wt)
  }
  # print(tab_vars) ; print(row_var) ; print(wt) ; print(col_vars)

  if (missing(na_drop_all)) {
    na_drop_all <- character()
  } else{
    na_drop_all <- names(tidyselect::eval_select(rlang::enquo(na_drop_all), data))
  }

  tab_row_names  <- as.character(c(tab_vars, row_vars))



  #The philosophy of tab_many is that :
  # - many col_vars are to be with the same kind of pct and colors (+ comp + diff + ci)
  # - many row_vars can have different colors and different parameters (otherwise tribble)

  #Arguments vectorised over row : tested in tab_plain/tab_num
  nrowvars    <- length(row_vars)
  totaltab    <- vctrs::vec_recycle(totaltab, nrowvars)
  totrow      <- vctrs::vec_recycle(totrow  , nrowvars)
  ref         <- vctrs::vec_recycle(ref     , nrowvars)
  ref2        <- vctrs::vec_recycle(ref2    , nrowvars)
  OR          <- vctrs::vec_recycle(OR      , nrowvars)
  comp        <- vctrs::vec_recycle(comp    , nrowvars)
  color       <- vctrs::vec_recycle(color   , nrowvars)
  #ci_visible <- vctrs::vec_recycle(ci_visible, nrowvars)

  #Arguments vectorised over row : tested here or in tab_num (not in tab_plain)
  ci          <- vctrs::vec_recycle(ci      , nrowvars)
  chi2        <- vctrs::vec_recycle(chi2    , nrowvars)

  #Arguments vectorised over columns : tested here
  ncolvars    <- length(col_vars)
  lvs         <- vctrs::vec_recycle(lvs   , ncolvars)
  digits      <- vctrs::vec_recycle(digits, ncolvars)
  if (totcol[1] %in% c("last", "all_col_vars")) {
    totcol <- col_vars_text[col_vars_text] %>% names() %>% dplyr::last()
  } else if (totcol[1] == "each") {
    totcol <- col_vars[col_vars_text]
  } else if (all(totcol %in% col_vars)) {
    totcol <- col_vars[col_vars %in% totcol & col_vars_text]
  } else if (all(totcol %in% c("col", "no"))) {
    totcol <- col_vars[which(totcol == "col" & col_vars_text)] # which ?
  } else if (is.numeric(totcol)) {
    if (any(totcol > ncolvars)) stop("some totcol indexes are superior to the",
                                     " number of col_vars")
    totcol <- col_vars[unique(as.integer(totcol))]
  } else {
    stop("totcol must be 'last', 'each', a vector of col_vars names, ",
         "a vector of 'col'/'no', or a vector of col_vars indexes")
  }
  tot_cols_type <- dplyr::case_when(
    identical(totcol, col_vars)                                ~ "each",
    identical(totcol, col_vars[ncolvars])                      ~ "all_col_vars",
    length(totcol) == 0 &
      (any(chi2 != FALSE) | any(pct != "no") | any(ci != "no"))~ "no_delete",
    length(totcol) == 0                                        ~ "no_no_create",
    TRUE                                                       ~ "some"
  )



  #Arguments vectorised over columns or rows : test in tab_plain/tab_num
  stopifnot(length(pct) >= 1)
  pct_vect <-
    if (is.character(pct) & length(pct) == 1) {
      rep(list(
        rep(pct, length(col_vars))
      ),
      length(row_vars),
      )
    } else if (is.character(pct) & length(row_vars) == 1) {
      list(vctrs::vec_recycle(pct, length(col_vars)))
    } else if (is.character(pct) & length(col_vars) == 1) {
      as.list(vctrs::vec_recycle(pct, length(row_vars)))
    } else if (is.list(pct) & length(pct) == length(row_vars) &
               all(purrr::map_int(pct, length) == length(col_vars))) {
      pct
    } else {
      stop("pct can't be recycled to the lengths of row_vars and col_vars (see documentation `?tab_many`)")
    }


  #Unique arguments :
  total_names <- vctrs::vec_recycle(total_names, 2)
  conf_level  <- vctrs::vec_recycle(conf_level , 1)
  na          <- vctrs::vec_recycle(na , 1)


  # Tests to be done before tab_plain / tab_num
  OR[OR == "no" & color %in% c("OR", "or")] <- "OR"

  color_auto_text <- color == "auto" & ! sum(col_vars_text) == 0
  if (any(color_auto_text)) color <- dplyr::case_when(
    purrr::map2_lgl(
      pct_vect, OR,
      ~ all(.x[col_vars_text] %in% c("row", "col") &
              .y[col_vars_text] %in% c("OR", "OR_pct", "or", "or_pct")
      )
    )
    ~ "OR",

    purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("row", "col"))) & ci == "diff" ~ "after_ci",
    purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("row", "col")))                ~ "diff"    ,
    purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("", "no", "all", "all_tabs"))) ~ "contrib" ,
    TRUE                                                                                  ~ "no" ,
  )
  if (length(row_vars) == 1) color <- color[1]


  ctr_no_row <- color == "contrib" & totrow == FALSE
  if (any(ctr_no_row)) {
    warning("total rows were added, since color == 'contrib' needs them ",
            "to store information about mean contributions to variance")
    totrow[ctr_no_row] <- TRUE
  }

  ctr_no_chi2 <- color == "contrib" & chi2 == FALSE
  if (any(ctr_no_chi2)) {
    #warning("since color == 'contrib' need chi2 data, chi2 was set to `TRUE` for the relevant row_vars")
    chi2[ctr_no_chi2] <- TRUE
  }
  # if (!is.null(chi2)) if (color == "contrib" & chi2 == FALSE)
  #   warning("since color = 'contrib', contributions of cells to the variance of tables
  #             must be calculated : chi2 was set to TRUE")
  # if (color == "contrib"){ chi2 <- TRUE } else { if (is.null(chi2)) chi2 <- FALSE }

  #Where ? In tab_plain ?
  # color_wrong_pct <- color %in% c("diff", "diff_ci", "after_ci") &
  #   all(pct %in% c("no", "all", "all_tabs")) & sum(col_vars_text) != 0
  # if (any(color_wrong_pct)) {
  #   stop(paste0("color = '", color, "' can't be calculated with pct = 'all' or with ",
  #               "counts (for those categories, only color = 'contrib' is possible)"))
  # }

  color_wrong_diff <- color %in% c("diff", "diff_ci", "after_ci") &
    ( ref %in% c("no", "") | is.na(ref) )
  if(any(color_wrong_diff)) {
    stop("since color = 'diff' for some tables, ref must be provided for those tables")
  }

  color_wrong_ci <- color %in% c("diff_ci", "after_ci") & ci != "diff"
  if (any(color_wrong_ci)) {
    # rlang::warn(
    #   paste0("since color = '", color, "' for some tables, the confidence intervals " ,
    #          "of cells differences from totals (or reference cells) must be calculated ",
    #          "ci was set to 'diff' for those tables")
    # )
    ci[color_wrong_ci] <- "diff"
  }




  # Allow to type expression as string in filter (to work with tibble::tribble)
  with_filter <- FALSE
  if (!missing(filter)) if (! is.null(filter)) {
    filter <- rlang::enquo(filter)
    if (is.character(rlang::get_expr(filter))) filter <- filter %>%
        rlang::get_expr(.) %>% str2lang()

    data <- data %>% dplyr::mutate(.filter = !!filter)
    with_filter <- TRUE
  }


  #Prepare the data
  data <- data %>% dplyr::select(!!!tab_vars, !!!row_vars, !!wt, !!!col_vars,
                                 tidyselect::any_of(".filter")) %>%
    relabel_levels_in_varnames(as.character(col_vars))

  #  Filters : here after selection (operations on rows copy all columns on memory),
  #     orwhen the tables are made for more speed :
  # - na = "drop_all" removes NAs here in tab_prepare (slower), i.e. for all tables mades
  # - na = "drop" : NA in factors and numeric will be removed in each tab_plain/tab_num
  # - na = "keep" : NA in factors (not numeric) will be made explicit in each tab_plain/tab_num

  if (with_filter == TRUE) data <- data %>% dplyr::filter(.data$.filter) %>%
    dplyr::select(-".filter")

  #If all variables on a subtable are "drop_all", then put na = "keep" to gain time
  if (na == "drop_all") {
    na_drop_all <- as.character(c(row_vars, col_vars, tab_vars))
    na_text <- "keep"
    na_num  <- "keep"

  } else {
    if (missing(na_drop_all)) {
      na_drop_all <- character()
    } else{
      na_drop_all <- names(tidyselect::eval_select(rlang::enquo(na_drop_all), data))
    }

    na_text <-
      purrr::map(as.character(row_vars),
                 ~ purrr::map2_lgl(., as.character(col_vars[col_vars_text]),
                                   ~ all(c(.x, .y, as.character(tab_vars)) %in% na_drop_all)
                 ) ) |>
      purrr::map(~ dplyr::if_else(., "keep", na))

    na_num <-
      purrr::map(as.character(row_vars),
                 ~ all(c(., as.character(tab_vars)) %in% na_drop_all)
      ) |>
      purrr::map(~ dplyr::if_else(., "keep", na))
  }

  data <- data %>%
    tab_prepare(
      as.character(c(row_vars, col_vars, tab_vars)),
      na_drop_all = tidyselect::all_of(na_drop_all),
      cleannames = cleannames,
      other_if_less_than = other_if_less_than, other_level = other_level
    )
  # if (!missing(filter)) data <- dplyr::filter(data, {{filter}})


  if (other_if_less_than > 0 & length(tab_vars) != 0) {
    # We only count tab variable's minimum counts for the row variable,
    #  otherwise we get problems.
    data <- data %>%
      dplyr::group_by(!!!tab_vars) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(as.character(row_vars)),
                                  ~ forcats::fct_lump_min(., other_if_less_than,
                                                          other_level = other_level))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(as.character(row_vars), function(.x) forcats::fct_relevel(
        .x,
        unique(append(levels(dplyr::pull(data, dplyr::cur_column())), other_level)) %>%
          purrr::discard(!. %in% levels(.x)))
      ))
  }


  #Remove rows with missing values or 0 in weight, for them not to be added in raw counts
  # remove zero weight in tab_prepare ?
  if (length(wt) != 0) {
    zero_weight <- dplyr::pull(data, !!wt)
    zero_weight <- is.na(zero_weight) | zero_weight == 0
    if (any(zero_weight)) {
      rlang::inform(paste0(sum(zero_weight), " rows with zero or NA weights were removed"))
      data <- data %>% dplyr::filter(!zero_weight)
    }
  }


  if(any(lvs == "auto")) {
    # print(lvs)
    lvs <- purrr::map2_chr(
      lvs,
      dplyr::select(data, !!!col_vars),
      ~ if (.x == "auto") {
        if(nlevels(forcats::fct_drop(.y)) == 2L) "first" else "all"
      } else {
        .x
      }
    )

  }

  # Where only first levels are kept, merge others to minimise useless calculations
  lv1 <- lvs == "first" & col_vars_text
  if (any(lv1)) {
    col_vars_3levels <-
      purrr::map_lgl(dplyr::select(data, !!!col_vars),
                     ~ is.factor(.) & nlevels(.) >= 3) & lv1

    if (any(col_vars_3levels)) {

      rm_levels_by_col_vars <- dplyr::select(data, !!!col_vars[col_vars_3levels]) |>
        purrr::map(~ purrr::set_names(c(levels(.)[-1], "NA"), "remove_levels"))

      data <- data %>%
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(as.character(col_vars[col_vars_3levels])),
          ~ suppressWarnings(forcats::fct_na_value_to_level(., level = "NA") |>
                               forcats::fct_recode(rlang::splice(rm_levels_by_col_vars[[dplyr::cur_column()]] )))
        ))
    }

    remove_levels <- purrr::map(dplyr::select(data, !!!col_vars[lv1]), ~ levels(.)[-1])
  }


  #Make a table for each column variable and store them in a list
  #dat_group3 <- data %>% dplyr::group_by(!!!tab_vars, .add = TRUE, .drop = FALSE)


  color_diff_OR <-
    color %in% c("diff", "OR") | (color == "auto" & any(ci != "diff"))

  color_ctr  <- dplyr::recode(color,
                              "no"       = "no"  ,
                              "auto"     = "auto",
                              "diff"     = "no"  ,
                              "diff_ci"  = "no"  ,
                              "after_ci" = "no"  ,
                              "contrib"  = "all" ,
                              "OR"       = "no"   )
  color_ci   <- dplyr::recode(color,
                              "no"       = "no"      ,
                              "auto"     = dplyr::if_else(any(ci == "diff"), "after_ci", "no"),
                              "diff"     = "no"      ,
                              "diff_ci"  = "diff_ci" ,
                              "after_ci" = "after_ci",
                              "contrib"  = "no"      ,
                              "OR"       = "no"
  )

  color_num <- dplyr::recode(color,
                             "contrib"  = "no" ,
                             "OR"       = "no" ,
                             .default = color   )



  #Calculate all numeric variables in one time (for each row_var)
  if (sum(col_vars_num) != 0) {
    tabs_num <- purrr::pmap(list(row_vars, totaltab, totrow   ,
                                 ref    , comp    , color_num,  ci, na_num),
                            ~ tab_num(data,
                                      !!..1,
                                      as.character(col_vars)[col_vars_num],
                                      as.character(tab_vars),
                                      wt         = !!wt,
                                      na         = ..8,
                                      digits     = digits[col_vars_num],
                                      ref        = ..4,
                                      ci         = ..7,
                                      conf_level = conf_level,
                                      comp       = ..5,
                                      color      = ..6,
                                      totaltab   = ..2,
                                      totaltab_name = totaltab_name,
                                      tot        = dplyr::if_else(..3, "row", "no"),
                                      total_names= total_names
                            )
    ) %>%
      purrr::set_names(row_vars)
  }



  if (sum(col_vars_text) != 0) {
    tabs_text <-     # By column first
      purrr::pmap(list(row_vars, totaltab, totrow, pct_vect, ref, ref2, comp, OR, na_text, color_diff_OR),

                  ~ purrr::pmap(list(col_vars[col_vars_text], digits[col_vars_text], ..9, ..4[col_vars_text]),
                                function(.col_vars, .digits, .na, .pct)
                                  tab_plain(data,
                                            !!..1,
                                            !!.col_vars,
                                            as.character(tab_vars),
                                            wt = !!wt,
                                            na         = .na,
                                            digits     = .digits,
                                            pct        = .pct,
                                            ref        = ..5,
                                            ref2       = ..6,
                                            comp       = ..7,
                                            OR         = ..8,
                                            color      = ..10,
                                            #subtext   = "",
                                            totaltab   = ..2,
                                            totaltab_name = totaltab_name,
                                            tot        = c( "row", "col"), #totrow vectorised ? ----
                                            total_names= total_names)) %>%
                    purrr::set_names(col_vars[col_vars_text])

      ) %>%
      purrr::set_names(row_vars)
    #tot_cols_type != "no_no_create" | totrow == TRUE


    #Join the list of tabs into a single table,
    # managing duplicated levels
    duplicated_levels <- tabs_text %>%
      purrr::map(~ purrr::map(., ~ purrr::discard(names(.),
                                                  names(.) %in% c(row_vars, tab_vars))) %>%
                   purrr::flatten_chr() #%>% .[duplicated(.)] %>% unique())
      ) |>
      purrr::map(~ .[duplicated(.)] %>% unique()) |> purrr::flatten_chr() |> unique()

    if (length(duplicated_levels) != 0) {
      tabs_text <- tabs_text %>%
        purrr::map(~ purrr::imap(., ~ dplyr::rename_with(.x, function(.names)
          dplyr::if_else(.names %in% duplicated_levels, paste0(.names, "_", .y), .names)))
        )
    }

    tabs_text <- purrr::map2(tabs_text, as.character(row_vars), ~ purrr::reduce(
      .x,
      dplyr::full_join,
      by = c(as.character(tab_vars), .y)
    ))

    if (any(chi2)) {
      tabs_text[chi2] <-
        purrr::pmap(list(tabs_text[chi2], comp[chi2], color_ctr[chi2]),
                    ~ tab_chi2(tabs = ..1, comp = ..2, color = ..3))
    }
    chi2 <- purrr::map(tabs_text, get_chi2)
    chi2 <- purrr::map_if(chi2, purrr::map_lgl(chi2, is.null), ~ attr(new_tab(), "chi2"))



    if (any(ci != "no")) {
      tabs_text[ci != "no"] <-
        purrr::pmap(
          list(tabs_text[ci != "no"], ci[ci != "no"], comp[ci != "no"],
               color_ci[ci != "no"], visible = ci[ci != "no"] == "cell"),
          ~ tab_ci(tabs = ..1, ci = ..2, comp = ..3, conf_level = conf_level, color = ..4,
                   visible = ..5, method_cell = method_cell, method_diff = method_diff)
        )

      # if (any(ci == "cell")) tabs_text[ci == "cell"] <- tabs_text[ci == "cell"] |>
      #     purrr::map(~ dplyr::mutate(., dplyr::across(where(is_fmt), ~ set_display(.,"mean_ci"))))
    }



    #Remove unwanted levels (keep only the first when levels = "first")
    if (any(lv1)) {
      remove_levels <-
        purrr::imap(remove_levels, ~ c(.x, paste0(.x, "_", .y))) %>%
        purrr::flatten_chr()

      tabs_text <- tabs_text %>% purrr::map(~ dplyr::select(., -tidyselect::any_of(remove_levels)))
    }


    #Remove unwanted total columns
    if (!tot_cols_type %in% c("each", "no_no_create")) {
      if (tot_cols_type == "no_delete") tabs_text <- tabs_text %>%
          purrr::map(~dplyr::select(., -where(is_totcol)))
      if (tot_cols_type == "some") tabs_text <- tabs_text %>%
          purrr::map(~dplyr::select(., -(where(~ is_totcol(.) & !get_col_var(.) %in% totcol) ))
          )

      if (tot_cols_type == "all_col_vars") {
        no_last_tot <- tabs_text %>%
          purrr::map(is_totcol) %>%
          purrr::map(~ names(.[.])) %>%
          purrr::flatten_chr() %>% unique()
        last_tot <- dplyr::last(no_last_tot)
        no_last_tot <- no_last_tot[no_last_tot != last_tot & !is.na(no_last_tot)]

        tabs_text <- tabs_text %>%
          purrr::map(~dplyr::select(., -tidyselect::any_of(no_last_tot)) %>%
                       dplyr::relocate(where(is_totcol), .after = tidyselect::last_col()) %>%
                       dplyr::rename_with(~ total_names[2], .cols = tidyselect::all_of(last_tot)) %>%
                       dplyr::mutate(dplyr::across(tidyselect::last_col(),
                                                   ~ set_col_var(., "all_col_vars")))
          )
      }
    }

    # Lone total column to "Total" with no col_var name
    totnames <-
      purrr::map(tabs_text,
                 ~names(.)[stringr::str_detect(names(.),
                                               paste0("^", total_names[2], "_"))]) |>
      purrr::flatten_chr()

    if ( length(totnames) == 1 ) tabs_text <- purrr::map(tabs_text, ~ dplyr::rename(
      ., tidyselect::any_of(purrr::set_names(totnames,
                                             rep(total_names[2], length(totnames))) )
    ) )


    # #By rows first
    # tabs_text <-
    #   purrr::pmap(list(col_vars[col_vars_text], digits[col_vars_text]),
    #               function(.col_vars, .digits)
    #
    #                 purrr::map_dfr(row_vars, function(.row_vars)
    #
    #                   tab_plain(data, !!.row_vars, !!.col_vars, !!!tab_vars, wt = !!wt,
    #                             na = na, digits = .digits,
    #                             totrow = totrow, totcol = totcol, totaltab = totaltab) |>
    #                     #                     dplyr::ungroup() |>
    #                     dplyr::mutate(variable = factor(rlang::as_name(.row_vars)), .before = 1) |>
    #                     dplyr::rename(tidyselect::all_of(
    #                       purrr::set_names(rlang::as_name(.row_vars), "row_var")
    #                     ))
    #                 )
    #
    #   ) %>%
    #   purrr::set_names(col_vars[col_vars_text])
  }




  if (sum(col_vars_num) != 0 & sum(col_vars_text) != 0) {
    tabs <- purrr::pmap(list(tabs_text, tabs_num, as.character(row_vars)),
                        ~ dplyr::full_join(..1, ..2,
                                           by = c(as.character(tab_vars), ..3)) #[tab_vars != "no_tab_vars"]
    )

    col_vars_order <- tabs |>
      purrr::map(~ purrr::map(.,
                              ~ purrr::map(get_col_var(.),
                                           ~ which(as.character(col_vars) == .)  ) ) |>
                   purrr::flatten()
      ) |>
      purrr::map(~ purrr::map_if(., names(.) %in% tab_row_names, ~ 0L) |>
                   purrr::map_int(~ if (length(.) == 0) {length(col_vars) + 1L} else {.}) |>
                   sort() |> names()
      )

    tabs <- tabs |> purrr::map2(col_vars_order, ~ dplyr::select(.x, tidyselect::any_of(.y)))

  } else if (sum(col_vars_num) != 0) {
    tabs <- tabs_num
    remove(tabs_num)

  } else {
    tabs <- tabs_text
    remove(tabs_text)
  }



  #Remove unwanted total rows
  no_totrow <- totrow == FALSE & tot_cols_type != "no_no_create"
  if (any(no_totrow)) {
    totrows     <- purrr::map(tabs[no_totrow], ~ is_totrow(.))
    tottab_rows <- purrr::map(tabs[no_totrow], ~ is_tottab(.))
    tottab_line <- purrr::map(tottab_rows[no_totrow], ~ length(.[.]) == 1 & .)

    tabs[no_totrow] <-
      purrr::pmap( list(tabs[no_totrow],totrows, tottab_line),
                   ~ tibble::add_column(..1, totrows = ..2, tottab_line = ..3) %>%
                     dplyr::filter(!.data$totrows | .data$tottab_line) %>%
                     dplyr::select(-"totrows", -"tottab_line")
      )
  }

  if (is.logical(chi2)) { chi2 <- list(attr(new_tab(), "chi2")) }

  if (!any(purrr::map_lgl(tabs, lv1_group_vars)) ) {
    tabs <- tabs %>% purrr::map(~ dplyr::group_by(., !!!tab_vars))
    groups <- purrr::map(tabs, dplyr::group_data)
    tabs <- purrr::pmap(list(tabs, groups, chi2),
                        ~ new_grouped_tab(..1, groups = ..2, subtext = subtext, chi2 = ..3))
  } else {
    tabs <- purrr::map2(tabs, chi2, ~ new_tab(.x, subtext = subtext, chi2 = .y))
  }

  # if (length(spread_vars) >= 1) {
  #   tabs <- tabs %>%
  #     tab_spread(spread_vars = spread_vars,
  #                names_prefix = names_prefix, names_sort = names_sort,
  #                totname = total_names[1])
  # }
  #
  # if (length(spread_vars) >= 2) {
  #
  #
  #
  #   tabs <- tabs %>%
  #     tab_spread(spread_vars = spread_vars,
  #                names_prefix = names_prefix, names_sort = names_sort,
  #                totname = total_names[1])
  #
  # }

  if (is.list(tabs) & !is.data.frame(tabs) & length(tabs) == 1) tabs <- tabs[[1]]

  if (getOption("tabxplor.output_kable") == TRUE) tabs <- tabs %>% tab_kable()

  tabs
}














#' Spread a tab, passing a tab variable to column
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab}},
#' \code{\link{tab_many}} or \code{\link{tab_plain}}.
#' @param spread_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>  The tab variables
#' to pass to column, with a syntax of type \code{c(var1, var2, ...)}.
#' @param names_prefix String added to the start of every variable name.
#' @param names_sort If no \code{names_prefix} is given, new names takes the form
#'  \code{spread_var}_\code{col_var_level}. Should then the column names be sorted ?
#'  If \code{FALSE}, the default, column names are ordered by first appearance.
#' @param totname The new name of the total rows, as a single string.
# @param recalculate Where there is several `tab_vars`, some totals are missing in the
# spreaded table. By default, `tab_spread` try to recalculate them based on `pct` and `wn`.
# Warning : with `means`, a weighted mean is calculated, which is only an approximation.
# Set to `FALSE` to avoid this behavior.
#'
#' @return A \code{tibble} of class \code{tab}, with less rows and more columns.
#' @export
#'
#' @examples
#' \donttest{ data <- forcats::gss_cat %>% dplyr::filter(year %in% c(2000, 2014))
#'
#' tabs <-
#'   tab(data, relig, marital, c(year, race), pct = "row", totaltab = "no",
#'       color = "diff", tot = "row", other_if_less_than = 30)
#'
#' tabs %>%
#'   dplyr::select(year, race, relig, Married) %>%
#'   tab_spread(race)
#'   }
tab_spread <- function(tabs, spread_vars, names_prefix, names_sort = FALSE,
                       totname = tr_("Total") #, recalculate = TRUE
) {
  spread_vars     <- rlang::enquo(spread_vars)
  pos_spread_vars <- tidyselect::eval_select(spread_vars, tabs)
  spread_vars     <- names(pos_spread_vars)
  NA_spread_vars  <- purrr::map_lgl(spread_vars,
                                    ~ as.character(.) %in% c("NA", "NULL", "no"))
  if (all(NA_spread_vars) ) return(tabs)

  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  get_vars   <- tab_get_vars(tabs)
  col_levels <- get_vars$col_vars_levels %>% purrr::flatten_chr()
  row_var    <- get_vars$row_var
  tab_vars   <- get_vars$tab_vars
  tab_vars_new <- tab_vars[!tab_vars %in% spread_vars]

  na_values <- purrr::map(dplyr::ungroup(tabs)[col_levels],
                          ~ fmt0(type = get_type(.x), display = get_display(.x[1]))) %>%
    purrr::set_names(col_levels)


  totrows <- is_totrow(tabs)
  if (any(totrows)) {
    #tab_match_groups_and_totrows(tabs)
    tabs <- tabs %>% dplyr::group_by(!!!rlang::syms(tab_vars))
    groups <- dplyr::group_vars(tabs)

    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows & totrows

    tabs <- tabs %>% tibble::add_column(totrows, tottab_rows, tottab_line)

    # if two tab_vars or more, calculate totals for each level of spread_var
    if (length(tab_vars_new) != 0 & any(tottab_rows)) {

      # if (recalculate) {
      #   if (any(get_type(tabs) == "mean")) {
      #     warning(paste0("Since there are several tab_vars, some totals are missing. ",
      #                    "Means for the new general total row were recalculated based on a ",
      #                    "weighted mean, which is only an approximation"))
      #   }
      #   new_totals <- tabs %>%
      #     dplyr::filter(.data$totrows) %>%
      #     dplyr::group_by(!!!rlang::syms(spread_vars)) %>%
      #     dplyr::summarise(dplyr::across(
      #       where(is_fmt),
      #       ~ new_fmt(display = get_display(.)[1],
      #                 digits  = max(get_digits(.)),
      #                 n       = sum(get_n(.), na.rm = TRUE),
      #                 wn      = sum(get_wn(.), na.rm = TRUE),
      #                 pct     = sum(get_wn(.), na.rm = TRUE) / sum(get_wn(.)/get_pct(.), na.rm = TRUE),
      #                 diff    = NA_real_,
      #                 ctr     = NA_real_,
      #                 mean    = stats::weighted.mean(get_mean(.), get_wn(.), na.rm = TRUE),
      #                 var     = NA_real_,
      #                 ci      = NA_real_,
      #
      #                 in_totrow = TRUE,
      #                 in_refrow = FALSE,
      #                 in_tottab = all(is_tottab(.)), #any ?
      #
      #                 type      = get_type    (.),
      #                 comp_all  = get_comp_all(., replace_na = FALSE),
      #                 ref = get_ref_type(.),
      #                 ci_type   = get_ci_type (.),
      #                 col_var   = get_col_var (.),
      #                 totcol    = is_totcol   (.),
      #                 refcol    = is_refcol   (.),
      #                 color     = get_color   (.)
      #       ), .groups = "drop"
      #     ))
      #   tabs_colors <- get_color(tabs)
      #
      #   ensemble_names <- tabs %>%
      #     dplyr::filter(tottab_line) %>%
      #     dplyr::ungroup() %>%
      #     select(tab_vars_new) %>% purrr::map_chr(~ as.character(dplyr::first(.)))
      #
      #   total_ensemble <- tabs %>%
      #     dplyr::filter(tottab_line) %>%
      #     dplyr::pull(row_var) %>% as.character()
      #
      #   new_totals <- new_totals %>%
      #     tab_pct(just_diff = TRUE) %>%
      #     dplyr::mutate(dplyr::across(where(is_fmt),
      #                                 ~ set_color(., tabs_colors[dplyr::cur_column()]))) %>%
      #     dplyr::mutate(!!rlang::sym(row_var) := factor(total_ensemble))
      #
      #   new_totals <- new_totals %>%
      #     purrr::reduce2(.x = names(ensemble_names), .y = ensemble_names, .init = new_totals,
      #                    .f = ~ dplyr::mutate(..1, !!rlang::sym(..2) := factor(..3))
      #     )  %>%
      #     dplyr::filter(!is_tottab(.))
      #
      # }

      tabs <- tabs %>% dplyr::filter(!tottab_line)

      #if (recalculate) tabs <- tabs %>% dplyr::bind_rows(new_totals)
    }

    new_levels <- tabs %>%
      dplyr::filter(.data$totrows & !.data$tottab_line) %>%
      dplyr::select(!!!tab_vars, !!row_var) %>%
      dplyr::arrange(!!!rlang::syms(tab_vars_new)) %>%
      dplyr::mutate(
        new_levels = paste(totname, paste(!!!rlang::syms(tab_vars_new), sep = " / ")) %>%
          stringr::str_to_upper()
      )
    new_levels <- purrr::set_names(as.character(dplyr::pull(new_levels, row_var)),
                                   new_levels$new_levels)

    # if (length(groups) - 1 != 0) {
    #   group_vars_totals <-
    #     dplyr::group_keys(dplyr::filter(tabs, !tottab_line)) %>% #dplyr::mutate(bis = PR0) %>%
    #     dplyr::select(-tidyselect::all_of(spread_vars)) %>%
    #     tidyr::unite(!!row_var, sep = " / ") %>%
    #     dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ paste(totname, .))) %>%
    #     tibble::deframe() %>%
    #     stringr::str_to_upper() %>% forcats::as_factor()
    # } else {
    #   group_vars_totals <- factor(totname)
    # }
    #
    # former_levels <-
    #   tibble::add_column(tabs, totrows = is_totrow(tabs),
    #                      tottab = is_tottab(tabs)) %>%
    #   dplyr::filter(.data$totrows & !.data$tottab) %>% dplyr::pull(row_var)
    #
    # group_vars_totals <- vctrs::vec_recycle(group_vars_totals, length(former_levels))
    #
    # new_levels <- former_levels %>% as.character() %>%
    #   purrr::set_names(group_vars_totals)

    tabs <- tabs %>% dplyr::mutate(
      !!rlang::sym(row_var) := forcats::fct_recode(!!rlang::sym(row_var),
                                                   !!!new_levels) %>%
        forcats::fct_relevel(unique(names(new_levels)), after = Inf)
    ) %>%
      dplyr::select(-"totrows", -"tottab_rows", -"tottab_line")
  }

  if ( !missing(names_prefix) ) {
    tabs <- tidyr::pivot_wider(tabs,
                               names_from   = tidyselect::all_of(spread_vars),
                               values_from  = tidyselect::all_of(col_levels),
                               names_prefix = names_prefix,
                               values_fill  = na_values,
                               names_sort   = names_sort
    )
  } else {
    tabs <- tidyr::pivot_wider(tabs,
                               names_from   = tidyselect::all_of(spread_vars),
                               values_from  = tidyselect::all_of(col_levels),
                               #names_glue   = "{.value}_{.name}",
                               values_fill  = na_values,
                               names_sort   = names_sort
    )
  }

  tabs <- tabs %>%  dplyr::arrange(!!!rlang::syms(tab_vars_new), !!rlang::sym(row_var))

  tabs <- complete_partial_totals(tabs)


  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {

    group_dat <- dplyr::group_data(tabs)
    new_grouped_tab(tabs, groups = group_dat, subtext = subtext, chi2 = chi2)
  }

}



#' @describeIn tab_many Get the variables names of a \pkg{tabxplor} \code{tab}
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab}},
#' \code{\link{tab_many}} or \code{\link{tab_plain}}.
#' @param vars In `tab_get_vars`, a character vector containing the wanted vars names:
#' \code{"row_var"}, \code{"col_vars"} or \code{"tab_vars"}.
#'
#' @return A list with the variables names.
#' @export
#'
# @examples
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

  if ("row_var" %in% vars) row_var <- names(utils::tail(fct_cols[fct_cols], 1L))

  if ("tab_vars" %in% vars) tab_vars <-
    names(fct_cols[fct_cols & names(fct_cols) != row_var])



  ls(pattern = "^row_var$|^col_vars$|^col_vars_levels$|^tab_vars$") %>%
    purrr::set_names(.) %>%
    purrr::map(~ rlang::sym(.) %>% rlang::eval_tidy())
}




# STEP-BY-STEP FUNCTIONS -----------------------------------------------------------------

#' Prepare data for \code{\link{tab_plain}}.
#'
#' @param data A dataframe.
#' @param ... Variables then to be passed in \code{\link{tab_plain}}.
#' @param na_drop_all <\link[tidyr:tidyr_tidy_select]{tidy-select}> Removes all
#' observation with a `NA` in any of the chosen variables.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param other_if_less_than When set to a positive integer, levels with less count
#' than it will be merged into an "Others" level.
#' @param other_level The name of the "Other" level, as a character vector of length one.
#'
#' @return A modified data.frame.
#' @export
#' @examples \donttest{data <- dplyr::starwars %>%
#' tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'             na_drop_all = sex)
#' data
#' }
tab_prepare <-
  function(data, ..., na_drop_all,
           cleannames = NULL, other_if_less_than = 0,
           other_level = tr_("Others")) {

    cleannames <-
      if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}

    variables     <- rlang::expr(c(...))
    pos_variables <- tidyselect::eval_select(variables, data)
    variables     <- names(pos_variables)

    if (missing(na_drop_all)) {
      na_drop_all <- character()
    } else{
      na_drop_all <- names(tidyselect::eval_select(rlang::enquo(na_drop_all), data))
    }



    #Convert to data.table and back divide the time by two with large dataframes
    if (length(na_drop_all) != 0) {
      data.table::setDT(data)
      data <- tibble::as_tibble(stats::na.omit(data, na_drop_all))
      #data <- tidyr::drop_na(data, tidyselect::all_of(na_drop_all))
    }

    vars_not_numeric <-
      dplyr::select(data[pos_variables], where(~ !is.numeric(.))) %>%
      colnames() #%>% rlang::syms()                # is.integer(.) | is.double()

    #Transform characters to factors first ? Time taker.
    # data <- data %>%
    #   dplyr::mutate(dplyr::across(
    #     tidyselect::all_of(vars_not_numeric) & where(~ !is.factor(.)),
    #     as.factor
    #   ))

    #remove class ordered
    data <- data %>%
      dplyr::mutate(dplyr::across(
        where(is.ordered),
        ~ magrittr::set_class(., class(.)[class(.) != "ordered"])
      ))

    if(other_if_less_than > 0) {
      data <- data %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(vars_not_numeric),
                                    ~ forcats::fct_lump_min(.,
                                                            other_if_less_than,
                                                            other_level = other_level))
        )
    }

    # Remove unused levels : time taker
    # data <- data %>%  #Remove unused levels anyway
    #   dplyr::mutate(dplyr::across(tidyselect::all_of(vars_not_numeric),
    #                               forcats::fct_drop))

    if (cleannames == TRUE)  data <- data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(vars_not_numeric),
        ~forcats::fct_relabel(., ~stringr::str_remove_all(., cleannames_condition() ))
      ))

    data
  }







#' Plain single cross-table
# @description
#' @param data A data frame.
#' @param row_var,col_var The row variable, which will be printed with one level per line,
#'  and the column variable, which will be printed with one level per column. Numeric
#'  variables will be used as factors. To calculate means, use \code{\link{tab_num}}.
#' @param tab_vars  <\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
#' a subtable is made for each combination of levels of the
#' selected variables. Leave empty to make a simple cross-table. All tab variables
#' are converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param digits The number of digits to print, as a single integer.
#' @param na The policy to adopt with missing values, as a single string.
#'  \itemize{
#'   \item \code{"keep"}: by default, \code{NA}'s of row, col and tab variables
#'   are printed as explicit "NA" level.
#'   \item \code{"drop"}: removes NA of row, col and tab variables.
#'   }
#' @param totaltab The total table,
#' if there are subtables/groups (i.e. when \code{tab_vars} is provided) :
#'  \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param tot The totals :
#'  \itemize{
#'   \item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
#'   columns.
#'   \item \code{"row"}: only total rows.
#'   \item \code{"col"}: only total column.
#'   \item \code{"no"}: remove all totals (after calculations if needed).
#'  }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param pct The type of percentages to calculate :
#'  \itemize{
#'   \item \code{"row"}: row percentages.
#'   \item \code{"col"}: column percentages.
#'   \item \code{"all"}: frequencies for each subtable/group, if there is \code{tab_vars}.
#'   \item \code{"all_tabs"}: frequencies for the whole (set of) table(s).
#' }
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
#' @param ref2 A second reference cell is needed to calculate odds ratios
#' (or relative risks ratios). The first cell of the row or column is used by default.
#' See `ref` above for the full list of possible values.
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios
#'   (for binary variables) or relative risks ratios (for variables with 3 levels
#'   or more).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' @param color `TRUE` print the color percentages and means based on cells differences from
#'   totals or reference cell, as provided by `ref`. If `OR` is provided,
#'   odds ratios are used for colors instead. Default to `FALSE`, no colors.
#' @param subtext A character vector to print rows of legend under the table.
#' @param num Set to \code{TRUE} to obtain a table with normal numeric vectors (not fmt).
#' @param df  Set to \code{TRUE} to obtain a plain data.frame (not a tibble),
#' with normal numeric vectors (not fmt). Useful, for example, to pass the table to
#' correspondence analysis with \pkg{FactoMineR}.
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
#' @examples # A typical workflow with tabxplor step-by-step functions :
#' \donttest{
#' data <- dplyr::starwars %>% tab_prepare(sex, hair_color)
#'
#' data %>%
#'   tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row") %>%
#'   tab_chi2() %>%
#'   tab_ci(color = "after_ci")
#' }
tab_plain <- function(data, row_var, col_var, tab_vars, wt,
                      pct = "no", color = TRUE, OR = "no",
                      na = "keep",
                      ref = "auto", ref2 = "first", comp = "tab",
                      totaltab = "line", totaltab_name = tr_("Ensemble"),
                      tot = NULL, total_names = tr_("Total"),
                      subtext = "", digits = 0,
                      num = FALSE, df = FALSE
) {

  row_var_quo <- rlang::enquo(row_var)
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("n"))
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::ensym(row_var)
  }

  col_var_quo <- rlang::enquo(col_var)
  if (quo_miss_na_null_empty_no(col_var_quo)) {
    data <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_var <- rlang::sym("no_col_var")
  } else {
    col_var <- rlang::ensym(col_var)
  }

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character()
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  # if (missing(...)) {
  #   #data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
  #   tab_vars <- character() #rlang::syms("no_tab_vars")
  # } else {
  #   tab_vars_quo <- rlang::enquos(...)
  #   NA_tab_vars  <- purrr::map(tab_vars_quo,
  #                              ~ is.na(as.character(rlang::get_expr(.)))) %>%
  #     purrr::flatten_lgl()
  #   if (all(NA_tab_vars) ) {
  #     #data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
  #     tab_vars <- character() #rlang::syms("no_tab_vars")
  #   } else {
  #     tab_vars     <- rlang::expr(c(...))
  #     pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
  #     tab_vars     <- rlang::syms(names(pos_tab_vars))
  #   }
  # }

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }



  vctrs::vec_assert(pct, size = 1)
  vctrs::vec_assert(ref, size = 1)
  vctrs::vec_assert(ref2, size = 1)
  vctrs::vec_assert(OR, size = 1)
  vctrs::vec_assert(na, size = 1)
  stopifnot(na %in% c("keep", "drop"))
  vctrs::vec_assert(totaltab_name, size = 1)
  total_names  <- vctrs::vec_recycle(total_names, 2)

  #pct
  stopifnot(pct %in% c("no", "row", "col", "all", "all_tabs"))
  if (is.logical(OR)) if(OR) OR <- "OR" else OR <- "no"
  stopifnot(OR %in% c("no", "OR", "OR_pct", "or", "or_pct"))
  if (pct == "all_tabs" & length(tab_vars) == 0) pct <- "all"

  if (color & ref == "no") {
    warning("since color is TRUE/`diff`, ref can't be `no` and was set to `tot`")
    ref <- "tot"
  }

  #tot
  if (is.null(tot)) {
    tot <- switch(pct,
                  "no"  = "no",
                  "row" = , #switch(ref, "tot" = c("row", "col"), "col"),
                  "col" = , #switch(ref, "tot" = c("row", "col"), "row"),
                  "all" = ,
                  "all_tabs" = c("row", "col"),
    )

  } else {
    stopifnot(all(tot %in% c("row", "col", "both", "no", "")))
    if (tot[1] == "both") tot <- c("row", "col")

    if (!"col" %in% tot) {
      if (pct == "row") {
        warning("since pct == 'row', a total column was added")
        tot <- c(tot, "col")
      }
      if (color & pct == "col" & ref == "tot") {
        warning("since color == TRUE and pct == 'col' and ref == 'tot', a total column was added")
        tot <- c(tot, "col")
      }
      if (pct %in% c("all", "all_tabs")) {
        warning("since pct == 'all' or 'all_tabs', a total column was added")
        tot <- c(tot, "col")
      }
    }

    if (!"row" %in% tot) {
      if (pct == "col") {
        warning("since pct == 'row', total rows were added")
        tot <- c(tot, "row")
      }
      if (color & pct == "row" & ref == "tot") {
        warning("since color == TRUE and pct == 'row' and ref == 'tot', total rows were added")
        tot <- c(tot, "row")
      }
      if (pct %in% c("all", "all_tabs")) {
        warning("since pct == 'all' or 'all_tabs', total rows were added")
        tot <- c(tot, "row")
      }
    }
  }

  #comp
  vctrs::vec_assert(comp, size = 1)
  stopifnot(comp %in% c("tab", "all", "") | is.na(comp) | is.null(comp))

  if (comp == "all" & length(tab_vars) == 0) comp <- "tab"

  #ref
  if (ref == "auto") {
   ref <- if (OR != "no") {"first"} else {"tot"}
  }

  #digits
  vctrs::vec_assert(digits, size = 1)
  digits <- vctrs::vec_cast(digits, integer())

  #totaltab
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



  data <- data %>%
    dplyr::select(!!!tab_vars, !!row_var, !!col_var, !!wt) %>%
    dplyr::mutate(dplyr::across(!!wt & !where(is.numeric), as.numeric)) %>%
    relabel_levels_in_varnames(as.character(col_var))
  #Vars are not changed to factors here, but after data.table






  tab_row_names  <- as.character(c(tab_vars, row_var))

  #If variables are in double in cols and rows, duplicate them and manage data.table
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



  #Make all calculations with data.table to gain time
  data.table::setDT(data)
  data.table::setnames(data, as.character(col_var), "col_var", skip_absent = TRUE)

  if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop_all')")

  # row_var_type <- ifelse(is.numeric(dplyr::pull(data, !!row_var) ),
  #                        "numeric", "factor")
  # col_var_type <- ifelse(is.numeric(dplyr::pull(data, !!col_var) ),
  #                        "numeric", "factor")
  # if (row_var_type == "numeric" & col_var_type == "numeric") {
  #   stop("row_var and col_var are both numeric : only one of them can be")
  # }
  # type <- ifelse(row_var_type == "numeric" | col_var_type == "numeric",
  #                "numeric", "factor")
  #
  # if (type == "numeric") {
  #   num_var <- switch(row_var_type, "numeric" = row_var, "factor" = col_var)
  #   fct_var <- switch(row_var_type, "numeric" = col_var, "factor" = row_var)
  # }

  # if (!is_grouped) {
  #   data <- switch(type,
  #                  "factor"   = dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var),
  #                  "numeric"  = dplyr::group_by(data, !!!tab_vars, !!fct_var     ) )
  # }
  #
  # if (type == "numeric") {
  #   if (rlang::as_name(num_var) %in% dplyr::group_vars(data)) {
  #     data <- dplyr::ungroup(data, !!num_var)
  #   }
  # }

  # nlvs <- nlevels(dplyr::pull(data, !!col_var))

  if (df | num) {
    tabs <-
      data.table::dcast(
        data[, list(n  = if(length(wt) != 0) {integer() } else {.N },
                    wn = if(length(wt) != 0) { sum(eval(wt), na.rm = TRUE) } else {double()}),
             keyby = eval(c(tab_row_names2, "col_var"))],
        formula = ... ~ col_var,
        value.var = if (length(wt) != 0) {c("wn")} else {"n"},
        fill = 0
      )

  } else {
    tabs <-
      data.table::dcast(
        data[, list(n  = .N,
                    wn = if(length(wt) != 0) { sum(eval(wt), na.rm = TRUE) } else {double()}),
             keyby = eval(c(tab_row_names2, "col_var"))],
        formula = ... ~ col_var,
        value.var = if (length(wt) != 0) {c("n", "wn")} else {"n"},
        fill = 0
      )
  }


  if (any(col_var_in_row_var)) {
    colvarbis <- names(tabs)[stringr::str_detect(names(tabs), "_colvarbis$")]
    data.table::setnames(tabs, colvarbis, stringr::str_remove(colvarbis, "_colvarbis$"),
                         skip_absent = TRUE)
  }

  not_fct <- !purrr::map_lgl(dplyr::select(tabs, tidyselect::all_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    tabs[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
         .SDcols = names(not_fct)[not_fct]]
  }


  na_cols <- names(tabs) %in% c("n_NA", "wn_NA", "NA")
  if (any(na_cols)) {
    if (na == "drop") {
      suppressWarnings(tabs[, `:=`(n_NA = NULL, wn_NA = NULL, `NA` = NULL)])
    } else {
      data.table::setcolorder(tabs, c(names(tabs)[!na_cols], names(tabs)[na_cols]))
    }
  }

  na_rows <- tabs %>%
    dplyr::select(!!!tab_vars, !!row_var) %>%
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

  num_cols <- tabs %>% purrr::map_lgl(is.numeric)
  num_cols <- names(num_cols)[num_cols]

  if (totaltab %in% c("table", "line")) {
    tabs_totaltab <- switch(
      totaltab[1],
      "table" = tabs[, c(purrr::set_names(rep("Total", length(tab_vars)),
                                          as.character(tab_vars)),
                         purrr::map(.SD, sum, na.rm = TRUE)),
                     .SDcols = num_cols,
                     keyby = eval(as.character(row_var))],

      "line" = tabs[, c(purrr::set_names(rep("Total", length(tab_row_names)),
                                         tab_row_names),
                        purrr::map(.SD, sum, na.rm = TRUE)),
                    .SDcols = num_cols]
    )

    tabs <- rbind(tabs, tabs_totaltab)
    data.table::setorderv(tabs, tab_row_names)
  }



  if ("row" %in% tot) {
    if (length(tab_vars) != 0) {
      group_vars <- rev(purrr::accumulate(as.character(tab_vars) , ~ c(.x, .y)))
      total_vars <- purrr::map(group_vars,
                               ~ c(as.character(tab_vars)[!as.character(tab_vars) %in% .],
                                   as.character(row_var)))
    } else {
      group_vars <- list(character())
      total_vars <- as.character(row_var)
    }


    tabs_tot <-
      purrr::map2_dfr(group_vars, total_vars,
                      ~ tabs[, c(purrr::set_names(rep("Total", length(.y)), eval(.y)),
                                 purrr::map(.SD, sum, na.rm = TRUE)),
                             .SDcols = num_cols,
                             keyby = eval(.x)]
      )

    tabs_tot <-data.table::setorderv(tabs_tot, tab_row_names) |> unique()

    if (totaltab == "line") {
      no_totaltab_line <- dplyr::select(tabs_tot, tidyselect::all_of(tab_row_names)) %>%
        dplyr::transmute(total_line = dplyr::if_any(tidyselect::everything(), ~ . != "Total")) %>%
        tibble::deframe() %>% which()

      tabs_tot <-  tabs_tot[no_totaltab_line, ]
    }

    tabs <- rbind(tabs, tabs_tot)
    data.table::setorderv(tabs, tab_row_names)
  }

  totrow_vector <- dplyr::pull(tabs, !!row_var) == "Total"

  if (length(tab_vars) == 0) {
    tottab_vector <- rep(FALSE, nrow(tabs))
  } else {
    tottab_vector <- dplyr::transmute(tabs, tottab = dplyr::if_all(
      tidyselect::all_of(as.character(tab_vars)),
      ~ . == "Total"
    )) |>
      tibble::deframe()
  }




  if (df | num) {
    if (length(wt) == 0) {
      tabs[, "wn" := NULL]
    } else {
      tabs[, "n" := NULL]
    }

    if (df) return(as_df_merge_rownames(tabs, rlang::as_name(row_var)))

    return(dplyr::group_by(new_tab(tibble::as_tibble(tabs)), !!!tab_vars))

  } else {
    if (length(wt) == 0) {
      tabs[, "wn" := NULL]

      text_vars <- !purrr::map_lgl(tabs, is.numeric)
      text_vars <- text_vars[text_vars]

      if ("col" %in% tot) {
        tabs[, "Total" := as.integer(rowSums(tabs[, -text_vars, with = FALSE]))] #Problems if not integer.
      }
      tabs_n <- tabs

    } else {
      text_vars <- !purrr::map_lgl(tabs, is.numeric)
      n_index  <- stringr::str_detect(names(tabs), "^n_")  | text_vars
      wn_index <- stringr::str_detect(names(tabs), "^wn_") | text_vars

      text_vars <- text_vars[text_vars]

      tabs_n  <- data.table::setnames(tabs[, n_index, with = FALSE] ,
                                      function(.x) stringr::str_remove(.x, "^n_" ))
      tabs_wn <- data.table::setnames(tabs[, wn_index, with = FALSE],
                                      function(.x) stringr::str_remove(.x, "^wn_"))

      tabs_wn[, (names(tabs_wn)) := purrr::map(.SD, as.double)]

      if ("col" %in% tot) {
        tabs_n [, "Total" := as.integer(rowSums(tabs_n[, -names(text_vars), with = FALSE] ))] #Problems if not integer.
        tabs_wn[, "Total" := rowSums(tabs_wn[, -names(text_vars), with = FALSE])]
      }

    }
  }
  tabs_text <- tabs[, names(text_vars), with = FALSE] #tibble::as_tibble()
  cols <- purrr::map_lgl(tabs_n, is.numeric)
  cols <- cols[cols]


  #Percentages
  if (pct != "no") {
    if (length(wt) == 0) {
      tabs_pct <- data.table::copy(tabs_n)
      tabs_pct[, names(cols) := purrr::map(.SD, as.double), .SDcols = names(cols) ]
    } else {
      tabs_pct <- data.table::copy(tabs_wn)
    }

    switch(
      pct,
      "row"      = tabs_pct[, names(cols) := purrr::map(.SD, ~ . / eval(rlang::sym("Total"))),
                            .SDcols = names(cols)],

      "col"      = tabs_pct[, names(cols) := purrr::map(.SD, ~ . / dplyr::last(.)),
                            by = eval(as.character(tab_vars)),
                            .SDcols = names(cols)],

      "all"      = tabs_pct[, names(cols) := purrr::map(.SD, ~ . / dplyr::last(eval(rlang::sym("Total")))),
                            by = eval(as.character(tab_vars)),
                            .SDcols = names(cols)],

      "all_tabs" = tabs_pct[, names(cols) := purrr::map(.SD, ~ . / dplyr::last(eval(rlang::sym("Total")))),
                            .SDcols = names(cols)]
    )

    tabs_pct[, names(cols) := purrr::map(.SD, ~ tidyr::replace_na(., 0)),
             .SDcols = names(cols)]


    #Differences and odds ratio
    if (ref != "no" & pct %in% c("row", "col")) {
      tabs_diff <- data.table::copy(tabs_pct)

      if (pct == "row") {

        refrows <- tabs |>
          calculate_refrows(ref           = ref,
                            comp          = comp,
                            tab_row_names = tab_row_names,
                            tab_vars      = tab_vars,
                            row_var       = row_var,
                            tottab_vector = tottab_vector,
                            totrow_vector = totrow_vector,
                            #pct           = pct,
                            num_names     = names(cols)
                            )

        comp_group <- if (comp == "tab") { as.character(tab_vars) } else { character() }

        tabs_diff[, "ref_rows___" := refrows]

        tabs_diff[,
                  c(names(cols), "ref_rows___") := purrr::map_if(
                    .SD,
                    purrr::map_lgl(.SD, is.numeric),
                    ~ . - dplyr::nth(., tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
                  ),
                  by = eval(comp_group),
                  .SDcols = c(names(cols), "ref_rows___")]

        tabs_diff[, "ref_rows___" := NULL] #keep it for ci ?


        # Odds ratio (when pct = "row")
        if (OR %in% c("OR", "OR_pct", "or", "or_pct")) {

          # Relative risks
          tabs_rr <- data.table::copy(tabs_pct)

          refcols <- dplyr::nth(names(cols),
                                diff_index(ref2,
                                           row_var   = dplyr::pull(tabs_rr, !!row_var),
                                           num_names = names(cols),
                                           pct       = "col"))
          refcols_vector <- names(cols) == refcols


          if (length(refcols) != 0 & !is.na(refcols)) {
            tabs_rr[, names(cols) := purrr::map(.SD,~ ./eval(rlang::sym(refcols)) ),
                    .SDcols = names(cols)]

          } else {
            remove(refcols, refcols_vector) # test if exists after
            warning(paste0(
              "in ref2 = '", ref2, "' , no columns were found as reference for comparison ; ",
              "to remove this warning, precise the value of ref ",
              "until there is one column matched"
            ))
            tabs_rr[, names(cols) := purrr::map(.SD, ~ NA_real_), .SDcols = names(cols)]
          }

          # Odds ratio (binary) or relative risk ratios
          tabs_or <- data.table::copy(tabs_rr)
          tabs_or[, "ref_rows___" := refrows]

          tabs_or[,
                  c(names(cols), "ref_rows___") := purrr::map_if(
                    .SD,
                    purrr::map_lgl(.SD, is.numeric),
                    ~ ./dplyr::nth(., tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
                  ),
                  by = eval(comp_group),
                  .SDcols = c(names(cols), "ref_rows___")]

          tabs_or[, "ref_rows___" := NULL]
        }

      }


      if (pct == "col") {
        refcols <- dplyr::nth(names(cols), diff_index(ref,
                                                      num_names = names(cols),
                                                      pct       = pct))
        refcols_vector <- names(cols) == refcols

        if (length(refcols) != 0 & !is.na(refcols)) {
          tabs_diff[, names(cols) := purrr::map(.SD,~ . - eval(rlang::sym(refcols)) ),
                    .SDcols = names(cols)]

        } else {
          warning(paste0(
            "in ref = '", ref, "' , no columns were found as reference for comparison ; ",
            "to remove this warning, precise the value of ref ",
            "until there is one column matched"
          ))
          tabs_diff[, names(cols) := purrr::map(.SD, ~ NA_real_), .SDcols = names(cols)]
        }


        # Odds ratio (when pct = "col")
        if (OR %in% c("OR", "OR_pct", "or", "or_pct")) {

          # Relative risks
          tabs_rr <- data.table::copy(tabs_pct)

          refrows <- tabs |>
            calculate_refrows(ref           = ref2,
                              comp          = comp,
                              tab_row_names = tab_row_names,
                              tab_vars      = tab_vars,
                              row_var       = row_var,
                              tottab_vector = tottab_vector,
                              totrow_vector = totrow_vector,
                              #pct           = pct,
                              num_names     = names(cols)
                              )

          comp_group <- if (comp == "tab") { as.character(tab_vars) } else { character() }

          tabs_rr[, "ref_rows___" := refrows]

          tabs_rr[,
                  c(names(cols), "ref_rows___") := purrr::map_if(
                    .SD,
                    purrr::map_lgl(.SD, is.numeric),
                    ~ ./dplyr::nth(., tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
                  ),
                  by = eval(comp_group),
                  .SDcols = c(names(cols), "ref_rows___")]

          tabs_rr[, "ref_rows___" := NULL]


          # Odds ratio (binary) or relative risk ratios
          tabs_or <- data.table::copy(tabs_rr)

          if (length(refcols) != 0 & !is.na(refcols)) {
            tabs_or[, names(cols) := purrr::map(.SD,~ ./eval(rlang::sym(refcols)) ),
                    .SDcols = names(cols)]

          } else {
            tabs_or[, names(cols) := purrr::map(.SD, ~ NA_real_), .SDcols = names(cols)]
            remove(refcols, refcols_vector) # test if exists after
          }


        } else if (length(refcols) != 0 & !is.na(refcols)) {
          remove(refcols, refcols_vector) # test if exists after
        }



      }



    }
  }



  #Make the final table with fmt vectors
  # remove(list = c("tabs_n", "tabs_wn", "tabs_pct", "tabs_diff", "tabs_ci", "refcols_vector", "refrows"))
  tabs_n [, names(text_vars) := NULL]
  if (exists("tabs_wn"  , rlang::current_env(), inherits = F)) tabs_wn  [, names(text_vars) := NULL]
  if (exists("tabs_pct" , rlang::current_env(), inherits = F)) tabs_pct [, names(text_vars) := NULL]
  if (exists("tabs_diff", rlang::current_env(), inherits = F)) tabs_diff[, names(text_vars) := NULL]
  if (exists("tabs_rr"  , rlang::current_env(), inherits = F)) tabs_rr  [, names(text_vars) := NULL]
  if (exists("tabs_or"  , rlang::current_env(), inherits = F)) tabs_or  [, names(text_vars) := NULL]
  #if (exists("tabs_ci"  , rlang::current_env(), inherits = F)) tabs_ci  [, names(text_vars) := NULL]

  totcol_vector <- names(tabs_n) == "Total"
  NA_reals <- rep(NA_real_, nrow(tabs_n))

  if (ref == "tot") refrows <- rep(FALSE, nrow(tabs_n))

  refrows <- if (exists("refrows", rlang::current_env(), inherits = F)) {
    refrows
  } else {
    rep(FALSE, nrow(tabs_n))
  }

  tabs <-
    list(tabs_n,
         if (exists("tabs_wn"  , rlang::current_env(), inherits = F)) { tabs_wn   } else { list(NA_reals) },
         if (exists("tabs_pct" , rlang::current_env(), inherits = F)) { tabs_pct  } else { list(NA_reals) },
         if (exists("tabs_diff", rlang::current_env(), inherits = F)) { tabs_diff } else { list(NA_reals) },
         if (exists("tabs_rr"  , rlang::current_env(), inherits = F)) { tabs_rr   } else { list(NA_reals) },
         if (exists("tabs_or"  , rlang::current_env(), inherits = F)) { tabs_or   } else { list(NA_reals) },

         totcol_vector,
         if (exists("refcols_vector", rlang::current_env(), inherits = F)) { refcols_vector } else {
           rep(FALSE, length(cols)) }
    ) |>
    purrr::pmap_dfc(~ new_fmt(
      display   = dplyr::case_when(
        pct != "no" & OR %in% c("OR", "or")         ~ "or",
        pct != "no" & OR %in% c("OR_pct", "or_pct") ~ "or_pct",
        pct != "no"                                 ~ "pct",
        length(wt) != 0                             ~ "wn" ,
        TRUE                                        ~ "n"
        ),
      digits    = vec_recycle(as.integer(digits), length(..1)),
      n         = as.integer(..1),
      wn        = ..2,
      pct       = ..3,
      diff      = ..4,
      rr        = ..5,
      or        = ..6,
      #ci        = ,
      in_totrow = totrow_vector,
      in_tottab = tottab_vector,
      in_refrow = refrows,
      totcol    = ..7,
      refcol    = ..8,
      color     = dplyr::case_when(
        color == FALSE                                    ~ "",
        row_var == "no_row_var" | col_var == "no_col_var" ~ "",

        pct %in% c("row", "col") & OR %in% c("OR", "or", "OR_pct", "or_pct") &
          ref != "no" & ref2 != "no"
        ~ "OR",

        pct %in% c("row", "col") & ref != "no"            ~ "diff",
        TRUE                                              ~ ""
      ),
      type      = dplyr::if_else(pct != "no", pct, "n"),
      ref = switch(as.character(ref), "no" = "", "tot" = "tot", as.character(ref)),
      #ci_type   = ,
      comp      = dplyr::if_else(pct != "no" & ref != "no", comp == "all", NA),
      col_var   = rlang::as_name(col_var)
    ))

  tabs <- dplyr::bind_cols(tibble::as_tibble(tabs_text), tabs)

  #Rename totals
  if (totaltab %in% c("line", "table") &  totaltab_name != "Total") {
    tabs <- tabs |> dplyr::mutate(dplyr::across(
      tidyselect::all_of(as.character(tab_vars)),
      ~ dplyr::if_else(tottab_vector,
                       true  = factor(totaltab_name, c(levels(.), totaltab_name)),
                       false = .) |>
        forcats::fct_drop()
    ))
  }

  if (length(tab_vars) == 0) {

    if ("row" %in% tot & total_names[1] != "Total") tabs <- tabs |>
        dplyr::mutate(!!row_var := forcats::fct_recode(!!row_var,
                                                       purrr::set_names("Total", total_names[1])))
  } else {
    tabs <- tabs |>
      tidyr::unite(col = "tabs_tot_names", !!!tab_vars, sep = " ", remove = FALSE) |>
      dplyr::mutate(
        !!row_var := dplyr::if_else(
          totrow_vector,
          true  = paste(total_names[1], .data$tabs_tot_names) |>
            forcats::fct_expand(levels(!!row_var)) |>
            forcats::fct_relevel(levels(!!row_var)),
          false = !!row_var) |>
          forcats::fct_drop()
        #forcats::fct_recode(!!row_var,
        #                               purrr::set_names("Total", total_names[1]))
      ) |>
      select(-"tabs_tot_names")
  }

  if ("col" %in% tot & total_names[2] != "Total") tabs <- tabs |>
    dplyr::rename(tidyselect::any_of(purrr::set_names("Total", total_names[2])))




  # if (row_var_type == "numeric") {
  #   tabs <- tabs %>%
  #   tidyr::pivot_wider(names_from = !!fct_var, values_from = !!num_var,
  #                      names_glue = "{.value}_{.name}",
  #                      values_fill = fmt0("mean", digits, type = "mean"))
  #   if (as.character(tab_vars) == "no_tab_vars") {
  #     tabs <- tabs %>% dplyr::mutate(no_row_var = factor("no_row_var")) %>%
  #       dplyr::relocate(no_row_var, .before = 1)
  #   }
  # }


  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                                    ~ length(unique(.)) == 1))

  if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext) %>%
      dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs %>% dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext)
  }
}






#' Means table
#' @description Cross categorical variables with numeric variables, and get a table
#' of means and standard deviations.
#' @param data A data frame.
#' @param row_var The row variable, which will be printed with one level per line. If
#' numeric, it will be used as a factor.
#' @param col_vars The numeric variables, which will appear in columns :
#' means and standard deviation are calculated for each levels of `row_var` and `tab_vars`.
#' @param tab_vars  <\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
#' a subtable is made for each combination of levels of the
#' selected variables. Leave empty to make a simple cross-table. All tab variables
#' are converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param digits The number of digits to print, as a single integer.
#' @param na The policy to adopt for missing values in row and tab variables (factors),
#' as a single string.
#'  \itemize{
#'   \item \code{"keep"}: by default, \code{NA}'s of row and tab variables
#'   are printed as an explicit `"NA"` level.
#'   \item \code{"drop"}: remove `NA`'s in row and tab variables.
#'   }
#' `NA`s in numeric variables are always removed when calculating means. For that reason
#' the `n` field of each resulting \code{\link{fmt}} column, used to calculate confidence
#' intervals, only takes into account the complete observations (without `NA`).
#' To drop all rows with `NA` in any numeric variable first, use \code{\link{tab_prepare}}
#' or \code{\link{tab_many}} with the `na_drop_all` argument.
#' @param totaltab The total table,
#' if there are subtables/groups (i.e. when \code{tab_vars} is provided) :
#'  \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param tot The totals :
#'  \itemize{
#'   \item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
#'   columns.
#'   \item \code{"row"}: only total rows.
#'   \item \code{"col"}: only total column.
#'   \item \code{"no"}: remove all totals (after calculations if needed).
#'  }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
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
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param color `TRUE` print the color percentages and means based on cells differences from
#'   totals or reference cell, as provided by `ref`. Default to `FALSE`, no colors.
#' @param subtext A character vector to print rows of legend under the table.
#' @param ci The type of confidence intervals to calculate, passed to \code{\link{tab_ci}}
#'  (automatically added if needed for \code{color}).
#'   \itemize{
#'    \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'    \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'    relative total cell (or relative first cell when \code{ref = "first"}).
#'    \item \code{"auto"}: \code{ci = "diff"} for means and row/col percentages,
#'      \code{ci = "cell"} for frequencies ("all", "all_tabs").
#'   }
#'  By default, for percentages, with \code{ci = "cell"} Wilson's method is used,
#'  and with \code{ci = "diff"} Wald's method along Agresti and Caffo's adjustment.
#'  Means use classic method. This can be changed in \code{\link{tab_ci}}.
#' @param conf_level The confidence level for the confidence intervals,
#'  as a single numeric between 0 and 1. Default to 0.95 (95%).
#' @param num Set to \code{TRUE} to obtain a table with normal numeric vectors (not `fmt`).
#' @param df  Set to \code{TRUE} to obtain a plain data.frame (not a `tibble`),
#' with normal numeric vectors (not `fmt`). Useful, for example, to pass the table to
#' correspondence analysis with \pkg{FactoMineR}.
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
#' data <- dplyr::storms %>% tab_prepare(category, wind, na_drop_all = wind)
#' tab_num(data, category, wind, tot = "row", color = "after_ci")
#' }
tab_num <- function(data, row_var, col_vars, tab_vars, wt,
                    color = c("auto", "diff", "diff_ci", "after_ci"),
                    na = c("keep", "drop", "drop_fct", "drop_num"),
                    ref = "tot", comp = c("tab", "all"),
                    ci = NULL, conf_level = 0.95, #ci_visible = FALSE,
                    totaltab = "line", totaltab_name = tr_("Ensemble"),
                    tot = NULL, total_names = tr_("Total"),
                    subtext = "", digits = 0, num = FALSE, df = FALSE
) {

  row_var_quo <- rlang::enquo(row_var)
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("n"))
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::ensym(row_var)
  }

  col_vars <- rlang::enquo(col_vars)
  if (quo_miss_na_null_empty_no(col_vars)) {
    data     <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_vars <- rlang::syms("no_col_var")
    pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else {
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
  }

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character()
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }

  tab_row_names <- purrr::map_chr(c(tab_vars, row_var), rlang::as_name)

  #forbid the level to have the name of the variable, othewise problems ----



  vctrs::vec_assert(ref, size = 1)
  # ci    <-  ci[1]
  # stopifnot(ci %in% c("diff", "cell", "no", ""))
  comp  <-  comp[1]
  stopifnot(comp %in% c("tab", "all", "") | is.na(comp) | is.null(comp))
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), length(col_vars))
  na <- na[1]
  stopifnot(na %in% c("keep", "drop"))
  vctrs::vec_assert(totaltab_name, size = 1)
  total_names  <- vctrs::vec_recycle(total_names, 2)
  color <-  color[1]
  stopifnot(color %in% c("auto", "diff", "diff_ci", "after_ci", "no", ""))

  ci_diff <- if (!is.null(ci)) { ci == "diff" } else { FALSE }
  ci_cell <- if (!is.null(ci)) { ci == "cell" } else { FALSE }

  color <- dplyr::case_when(
    row_var == "no_row_var" | "no_col_var" %in% col_vars     ~ "",
    color == "auto" & !ref %in% c("no", "") & ci_diff        ~ "after_ci",
    color == "auto" & !ref %in% c("no", "") & !ci_cell       ~ "diff",
    color == "auto"                                          ~ "",
    TRUE                                                     ~ color
  )

  if (row_var == "no_row_var" | "no_col_var" %in% col_vars) color <- ""

  if (color %in% c("diff", "diff_ci", "after_ci") & ref %in% c("no", "")) {
    warning("since color = 'diff', ref must be provided and was set to 'tot'")
    ref <- "tot"
  }

  if (!is.null(ci)) if (color %in% c("diff_ci", "after_ci") & ci != "diff")
    rlang::warn(
      paste0("since color = '", color, "', the confidence intervals of cells differences",
             " from totals (or others cells) must be calculated : ci was set to 'diff' ")
    )
  if (color %in% c("diff_ci", "after_ci")) {
    ci <- "diff"
  } else {
    if (is.null(ci)) ci <- "no"
  }

  if (ci == "diff" & ref %in% c("no", "")) {
    warning("since ci = 'diff', a diff was added with ref = 'tot'")
    ref <- "tot"
  }

  ci_visible <- ci == "cell"


  if (is.null(tot)) {
    tot <- if (ref == "tot" & color %in% c("diff", "diff_ci", "after_ci")) {"row"} else {"no"}

  } else {
    stopifnot(all(tot %in% c("row", "col", "both", "no", "")))
    if (tot[1] == "both") tot <- "row"

    if ((color %in% c("diff", "diff_ci", "after_ci") | ref == "tot") & !tot %in% "row") {
      #warning("since color = '", color, "' and ref = 'tot', a total row was added")
      tot <- "row"
    }
  }

  if (ref == "auto") {
    ref <- "tot"  # ref <- if (OR != "no") {"first"} else {"tot"}
  }

  if (comp == "all" & length(tab_vars) == 0) comp <- "tab"

  if (length(tab_vars) == 0) totaltab <- "no"

  if (comp[1] == "all" & ref == "tot" & !totaltab %in% c("table", "line")) {
    warning("since comp = 'all', a total table was added to compare with")
    totaltab <-  "line"
  }

  if (comp[1] == "all" & !ref %in% c("tot", "no", "") & totaltab != "table") {
    warning("since comp = 'all', a full total table was added to compare with")
    totaltab <- "table"
  }




  data <- data %>%
    dplyr::select(!!!tab_vars, !!row_var, !!!col_vars, !!wt) %>%
    dplyr::mutate(dplyr::across((!!wt | tidyselect::all_of(as.character(col_vars))) &
                                  !where(is.numeric), as.numeric)
    )


  #Faster with data.table
  data.table::setDT(data)
  #data.table::setkeyv(data, tab_row_names) #slower with key, even with simple "by"

  # Remove NA's in factors here, otherwise they are kept in totals after
  if (na == "drop") data <- stats::na.omit(data, tab_row_names) # 0.5 sec


  if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop')")

  # row_var_type <- ifelse(is.numeric(dplyr::pull(data, !!row_var) ),
  #                        "numeric", "factor")
  # col_var_type <- ifelse(is.numeric(dplyr::pull(data, !!col_var) ),
  #                        "numeric", "factor")
  # if (row_var_type == "numeric" & col_var_type == "numeric") {
  #   stop("row_var and col_var are both numeric : only one of them can be")
  # }
  # type <- ifelse(row_var_type == "numeric" | col_var_type == "numeric",
  #                "numeric", "factor")
  #
  # if (type == "numeric") {
  #   num_var <- switch(row_var_type, "numeric" = row_var, "factor" = col_var)
  #   fct_var <- switch(row_var_type, "numeric" = col_var, "factor" = row_var)
  # }

  # if (!is_grouped) {
  #   data <- switch(type,
  #                  "factor"   = dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var),
  #                  "numeric"  = dplyr::group_by(data, !!!tab_vars, !!fct_var     ) )
  # }
  #
  # if (type == "numeric") {
  #   if (rlang::as_name(num_var) %in% dplyr::group_vars(data)) {
  #     data <- dplyr::ungroup(data, !!num_var)
  #   }
  # }

  # nlvs <- nlevels(dplyr::pull(data, !!col_var))

  #        "numeric" = data %>%
  #          dplyr::summarise(!!num_var := stats::weighted.mean(!!num_var, !!wt, na.rm = TRUE),
  #                           .groups = "drop")
  # ) %>%


  #data <- data |> dplyr::group_by(!!!tab_vars, !!row_var)

  # if (df | num) {
  #  tabs <- data |>
  #     dplyr::summarise(across(tidyselect::all_of(as.character(col_vars)), list(
  #       mean = ~ stats::weighted.mean(., !!wt, na.rm = TRUE)
  #     )),
  #     .groups = "drop"
  #     )
  #
  # } else {


  if (df | num) {
    tabs <-
      if (length(wt) == 0) {
        data[, purrr::map(.SD,  ~mean(., na.rm = TRUE)),
             .SDcols = as.character(col_vars),
             keyby = c(tab_row_names)]

      } else {
        data[, purrr::map_if(.SD,
                             names(.SD) != as.character(wt),
                             ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                             .else = ~ NA_real_),
             .SDcols = as.character(c(col_vars, wt)),
             keyby = c(tab_row_names)][, wt := NULL]
      }

  } else {
    # with data.table
    if (length(wt) == 0) {
      tabs <- #4.1 sec
        data[,
             c(purrr::set_names(purrr::map(.SD,  ~ sum(!is.na(.))),
                                paste0(as.character(col_vars), "_n")),

               #list(n = .N),

               purrr::set_names(purrr::map(.SD,  ~ mean(., na.rm = TRUE)),
                                paste0(as.character(col_vars), "_mean")),

               purrr::set_names(purrr::map(.SD, ~ stats::var(., na.rm = TRUE)),
                                paste0(as.character(col_vars), "_var"))
             ),
             .SDcols = as.character(col_vars),
             keyby = c(tab_row_names)]

    } else {
      tabs <- #11.01 sec without key ; 13.75 sec with n and wn for each vars
        data[,
             c(purrr::set_names(purrr::map_if(.SD,
                                              names(.SD) != as.character(wt),
                                              ~ sum(!is.na(.)),
                                              .else = ~ NA_real_),
                                paste0(as.character(c(col_vars, wt)), "_n")),

               purrr::set_names(purrr::map_if(.SD,
                                              names(.SD) != as.character(wt),
                                              ~ sum(as.integer(!is.na(.)) * eval(wt), na.rm = TRUE),
                                              .else = ~ NA_real_),
                                paste0(as.character(c(col_vars, wt)), "_wn")),

               purrr::set_names(purrr::map_if(.SD,
                                              names(.SD) != as.character(wt),
                                              ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                                              .else = ~ NA_real_),
                                paste0(as.character(c(col_vars, wt)), "_mean")),

               purrr::set_names(purrr::map_if(.SD,
                                              names(.SD) != as.character(wt),
                                              ~ weighted.var(., eval(wt), na.rm = TRUE),
                                              .else = ~ NA_real_),
                                paste0(as.character(c(col_vars, wt)), "_var"))
             ),
             #.SDcols = as.character(c(col_vars, wt)),
             keyby = c(tab_row_names)][, paste0(wt, c("_n", "_wn", "_mean", "_var")) := NULL]
    }

    # # groupingsets() could be used to calculate table, totals and total table at once
    # #   but it does not gain times (problem : Total are named NA and mixed with true NAs...)
    # if ("row" %in% tot | totaltab %in% c("line", "table")) {
    #   group_sets <- tab_row_names |> purrr::accumulate(~ c(.x, .y))
    #   group_sets <- rev(group_sets)
    #   if (!"row" %in% tot) group_sets <- dplyr::last(group_sets)
    #   if(totaltab == "table") group_sets <- c(group_sets, as.character(row_var))
    #   group_sets <- c(group_sets, list(character()))
    #
    # tictoc::tic()
    # tabs <- #56.7 sec with totals and total table
    #   data.table::groupingsets(data,
    #        c(list(n  = .N,
    #               wn = sum(eval(wt), na.rm = TRUE)),
    #
    #          purrr::set_names(purrr::map_if(.SD,
    #                                         names(.SD) != as.character(wt),
    #                                         ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
    #                                         .else = ~ NA_real_),
    #                           paste0(as.character(c(col_vars, wt)), "_mean")),
    #
    #          purrr::set_names(purrr::map_if(.SD,
    #                                         names(.SD) != as.character(wt),
    #                                         ~ weighted.var(., eval(wt), na.rm = TRUE),
    #                                         .else = ~ NA_real_),
    #                           paste0(as.character(c(col_vars, wt)), "_var"))
    #        ),
    #        .SDcols = as.character(c(col_vars, wt)),
    #        by = c(tab_row_names),
    #        sets = group_sets,
    #        )[, paste0(wt, c("_mean", "_var")) := NULL]
    # tictoc::toc()
    #
    # data.table::setorderv(tabs, tab_row_names)
    # }
  }

  not_fct <- !purrr::map_lgl(dplyr::select(tabs, tidyselect::any_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    # not_fct_names <- names(not_fct)[not_fct]
    tabs[, names(not_fct)[not_fct] := purrr::map(.SD, as.factor),
         .SDcols = names(not_fct)[not_fct]]
  }

  if (na == "keep") {
    data.table::setorderv(
      tabs, tab_row_names, na.last = TRUE
    )[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
      .SDcols = tab_row_names]
  }





  # tabs <- data |>
  #   dplyr::summarise(dplyr::across(tidyselect::all_of(as.character(col_vars)), ~ new_fmt(
  #     display = "mean",
  #     digits  = as.integer(digits),
  #     n       = dplyr::n(),
  #     wn      = if (wt != "no_weight") {sum(!!wt, na.rm = TRUE)} else {NA_real_},
  #     mean    = stats::weighted.mean(., !!wt, na.rm = TRUE),
  #     var     = weighted.var(., !!wt, na.rm = TRUE),
  #     type    = "mean",
  #     col_var = dplyr::cur_column()
  #   )),
  #   .groups = "drop")
  # #}
  #
  # na_rows <- tabs %>%
  #   dplyr::select(!!!tab_vars, !!row_var) %>%
  #   dplyr::mutate(na_rows = dplyr::if_any(.cols = dplyr::everything(), .fns = is.na)) |>
  #   dplyr::pull(.data$na_rows)
  #
  # if (any(na_rows)) {
  #   if (na == "drop") {
  #     tabs <- tabs[-which(na_rows), ]
  #   } else {
  #     tabs <- tabs %>%
  #       dplyr::mutate(dplyr::across(tidyselect::all_of(tab_row_names),
  #                                   ~ forcats::fct_na_value_to_level(., level = "NA"))) %>%
  #       dplyr::arrange(!!!tab_vars, !!row_var)
  #
  #     #data.table::setorderv(
  #     #  tabs, tab_row_names, na.last = TRUE
  #     #)[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
  #     #  .SDcols = tab_row_names]
  #   }
  # }


  #Calculate means and variances for all totals and subtotals
  if ("row" %in% tot | totaltab %in% c("line", "table")) {
    if (length(tab_vars) != 0) {
      group_vars <- c(as.character(tab_vars)) |> purrr::accumulate(~ c(.x, .y))
      group_vars <- c(rev(group_vars), list(character()))
    } else {
      group_vars <- list(character())
    }
    if (!"row" %in% tot) group_vars <- dplyr::last(group_vars)


    if (df | num) {
      if (length(wt) == 0) {
        suppressWarnings(
          tabs_tot <-
            purrr::map_dfr(
              group_vars,
              ~ data[, c(purrr::set_names(rep("Total", length(c(tab_vars[!tab_vars %in% .], row_var))),
                                          as.character(c(tab_vars[!tab_vars %in% .], row_var)) ),

                         purrr::map(.SD,  ~mean(., na.rm = TRUE)) ),
                     .SDcols = as.character(col_vars),
                     keyby = eval(.)][ , as.character(tab_vars) := purrr::map(.SD, as.factor),
                                       .SDcols = as.character(tab_vars)]
            )
        )
      } else {
        suppressWarnings(
          tabs_tot <-
            purrr::map_dfr(
              group_vars,
              ~ data[, c(purrr::set_names(rep("Total", length(c(tab_vars[!tab_vars %in% .], row_var))),
                                          as.character(c(tab_vars[!tab_vars %in% .], row_var)) ),

                         purrr::map_if(.SD,
                                       names(.SD) != as.character(wt),
                                       ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                                       .else = ~ NA_real_)),
                     .SDcols = as.character(c(col_vars, wt)),
                     keyby = eval(.)][, wt := NULL][ , as.character(tab_vars) := purrr::map(.SD, as.factor),
                                                     .SDcols = as.character(tab_vars)]
            )
        )
      }

    } else {
      if (length(wt) == 0) {
        suppressWarnings(
          tabs_tot <-  #9.75 sec
            purrr::map_dfr(
              group_vars,
              ~ data[,
                     c(purrr::set_names(rep("Total", length(c(tab_vars[!tab_vars %in% .], row_var))),
                                        as.character(c(tab_vars[!tab_vars %in% .], row_var)) ),

                       #list(n  = .N),

                       purrr::set_names(purrr::map(.SD,  ~ sum(!is.na(.))),
                                        paste0(as.character(col_vars), "_n")),

                       purrr::set_names(purrr::map(.SD,  ~mean(., na.rm = TRUE)),
                                        paste0(as.character(col_vars), "_mean")),

                       purrr::set_names(purrr::map(.SD, ~ stats::var(., na.rm = TRUE)),
                                        paste0(as.character(col_vars), "_var"))
                     ),
                     .SDcols = as.character(col_vars),
                     keyby = eval(.)][ , as.character(tab_vars) := purrr::map(.SD, as.factor),
                                       .SDcols = as.character(tab_vars)]
            )
        )
      } else {
        suppressWarnings(
          tabs_tot <-  #31.83 sec
            purrr::map_dfr(
              group_vars,
              ~ data[,
                     c(purrr::set_names(rep("Total", length(c(tab_vars[!tab_vars %in% .], row_var))),
                                        as.character(c(tab_vars[!tab_vars %in% .], row_var)) ),
                       #list(n  = .N,
                       #     wn = sum(eval(wt), na.rm = TRUE)),

                       purrr::set_names(purrr::map_if(.SD,
                                                      names(.SD) != as.character(wt),
                                                      ~ sum(!is.na(.)),
                                                      .else = ~ NA_real_),
                                        paste0(as.character(c(col_vars, wt)), "_n")),

                       purrr::set_names(purrr::map_if(.SD,
                                                      names(.SD) != as.character(wt),
                                                      ~ sum(as.integer(!is.na(.)) * eval(wt), na.rm = TRUE),
                                                      .else = ~ NA_real_),
                                        paste0(as.character(c(col_vars, wt)), "_wn")),

                       purrr::set_names(purrr::map_if(.SD,
                                                      names(.SD) != as.character(wt),
                                                      ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                                                      .else = ~ NA_real_),
                                        paste0(as.character(c(col_vars, wt)), "_mean")),

                       purrr::set_names(purrr::map_if(.SD,
                                                      names(.SD) != as.character(wt),
                                                      ~ weighted.var(., eval(wt), na.rm = TRUE),
                                                      .else = ~ NA_real_),
                                        paste0(as.character(c(col_vars, wt)), "_var"))
                     ),
                     .SDcols = as.character(c(col_vars, wt)),
                     keyby = eval(.)][, paste0(wt, c("_n", "_wn", "_mean", "_var")) := NULL
                     ][ , as.character(tab_vars) := purrr::map(.SD, as.factor),
                        .SDcols = as.character(tab_vars)]
            )
        )
      }
    }

    not_fct <- !purrr::map_lgl(dplyr::select(tabs_tot, tidyselect::any_of(tab_row_names)), is.factor)
    if (any(not_fct)) {
      # not_fct_names <- names(not_fct)[not_fct]
      tabs_tot[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
               .SDcols = names(not_fct)[not_fct]]
    }

    if (na == "keep" & length(tab_vars) != 0) {
      data.table::setorderv(
        tabs_tot, as.character(tab_vars), na.last = TRUE
      )[, as.character(tab_vars) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
        .SDcols = as.character(tab_vars)]
    }

    tabs <- rbind(tabs, tabs_tot)
    data.table::setorderv(tabs, tab_row_names)


    # tabs_tot <-
    #   purrr::map_dfr(ungroup_vars,
    #                  ~ dplyr::ungroup(data, !!!.x) |> #win nearly no time compared to group_by(!!!tab_vars)
    #                    dplyr::summarise(
    #                      dplyr::across(tidyselect::all_of(purrr::map_chr(.x, rlang::as_name)), ~ factor("Total")),
    #                      dplyr::across(tidyselect::all_of(as.character(col_vars)), function(.var) new_fmt(
    #                        display   = "mean",
    #                        digits    = as.integer(digits),
    #                        n         = dplyr::n(),
    #                        wn        = if (wt != "no_weight") {sum(!!wt, na.rm = TRUE)} else {NA_real_},
    #                        mean      = stats::weighted.mean(.var, !!wt, na.rm = TRUE),
    #                        var       = weighted.var(.var, !!wt, na.rm = TRUE),
    #                        type      = "mean",
    #                        col_var   = dplyr::cur_column(),
    #                        in_totrow = TRUE,
    #                        in_tottab = length(.x) == length(tab_vars) + 1L,
    #                      )),
    #                      .groups = "drop")
    #   )
    #
    # na_rows <- tabs_tot %>%
    #   dplyr::select(!!!tab_vars) %>%
    #   dplyr::transmute(na_rows = dplyr::if_any(.cols = dplyr::everything(), .fns = is.na)) |> tibble::deframe()
    #
    # if (any(na_rows)) {
    #   if (na == "drop") {
    #     tabs_tot <- tabs_tot[-which(na_rows), ]
    #   } else {
    #     tabs_tot <- tabs_tot %>%
    #       dplyr::mutate(dplyr::across(tidyselect::all_of(as.character(tab_vars)),
    #                                   ~ forcats::fct_na_value_to_level(., level = "NA"))) %>%
    #       dplyr::arrange(!!!tab_vars, !!row_var)
    #
    #   }
    # }
  }

  #Calculate means and variances for total table
  if (totaltab == "table") {

    if (df | num) {
      if (length(wt) == 0) {
        tabs_totaltab <-
          data[, c(purrr::set_names(rep("Total", length(tab_vars)), as.character(tab_vars)),
                   purrr::map(.SD,  ~mean(., na.rm = TRUE)) ),
               .SDcols = as.character(col_vars),
               keyby = eval(as.character(row_var))]

      } else {
        tabs_totaltab <-
          data[, c(purrr::set_names(rep("Total", length(tab_vars)), as.character(tab_vars)),
                   purrr::map_if(.SD,
                                 names(.SD) != as.character(wt),
                                 ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                                 .else = ~ NA_real_)),
               .SDcols = as.character(c(col_vars, wt)),
               keyby = eval(as.character(row_var))][, wt := NULL]
      }

    } else {
      if (length(wt) == 0) {
        tabs_totaltab <-
          data[,
               c(purrr::set_names(rep("Total", length(tab_vars)), as.character(tab_vars)),

                 #list(n  = .N),

                 purrr::set_names(purrr::map(.SD, ~ sum(!is.na(.))),
                                  paste0(as.character(col_vars), "_n")),

                 purrr::set_names(purrr::map(.SD, ~ mean(., na.rm = TRUE)),
                                  paste0(as.character(col_vars), "_mean")),

                 purrr::set_names(purrr::map(.SD, ~ stats::var(., na.rm = TRUE)),
                                  paste0(as.character(col_vars), "_var"))
               ),
               .SDcols = as.character(col_vars),
               keyby = eval(as.character(row_var))]

      } else {
        tabs_totaltab <-
          data[,
               c(purrr::set_names(rep("Total", length(tab_vars)), as.character(tab_vars)),

                 #list(n  = .N,
                 #     wn = sum(eval(wt), na.rm = TRUE)),

                 purrr::set_names(purrr::map_if(.SD,
                                                names(.SD) != as.character(wt),
                                                ~ sum(!is.na(.)),
                                                .else = ~ NA_real_),
                                  paste0(as.character(c(col_vars, wt)), "_n")),

                 purrr::set_names(purrr::map_if(.SD,
                                                names(.SD) != as.character(wt),
                                                ~ sum(as.integer(!is.na(.)) * eval(wt), na.rm = TRUE),
                                                .else = ~ NA_real_),
                                  paste0(as.character(c(col_vars, wt)), "_wn")),

                 purrr::set_names(purrr::map_if(.SD,
                                                names(.SD) != as.character(wt),
                                                ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                                                .else = ~ NA_real_),
                                  paste0(as.character(c(col_vars, wt)), "_mean")),

                 purrr::set_names(purrr::map_if(.SD,
                                                names(.SD) != as.character(wt),
                                                ~ weighted.var(., eval(wt), na.rm = TRUE),
                                                .else = ~ NA_real_),
                                  paste0(as.character(c(col_vars, wt)), "_var"))
               ),
               .SDcols = as.character(c(col_vars, wt)),
               keyby = eval(as.character(row_var))][, paste0(wt, c("_n", "_wn", "_mean", "_var")) := NULL]
      }
    }

    not_fct <- !purrr::map_lgl(dplyr::select(tabs_totaltab, tidyselect::any_of(tab_row_names)), is.factor)
    if (any(not_fct)) {
      # not_fct_names <- names(not_fct)[not_fct]
      tabs_totaltab[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
                    .SDcols = names(not_fct)[not_fct]]
    }

    if (na == "keep") {
      data.table::setorderv(
        tabs_totaltab, as.character(row_var), na.last = TRUE
      )[, as.character(row_var) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
        .SDcols = as.character(row_var)]
    }


    tabs <- rbind(tabs, tabs_totaltab)
    data.table::setorderv(tabs, tab_row_names)


    # tabs_totaltab <- dplyr::group_by(data, !!row_var) |>
    #   dplyr::summarise(
    #     dplyr::across(tidyselect::all_of(purrr::map_chr(tab_vars, rlang::as_name)),
    #                   ~ factor("Total")),
    #     dplyr::across(tidyselect::all_of(as.character(col_vars)), function(.var) new_fmt(
    #       display   = "mean",
    #       digits    = as.integer(digits),
    #       n         = dplyr::n(),
    #       wn        = if (wt != "no_weight") {sum(!!wt, na.rm = TRUE)} else {NA_real_},
    #       mean      = stats::weighted.mean(.var, !!wt, na.rm = TRUE),
    #       var       = weighted.var(.var, !!wt, na.rm = TRUE),
    #       type      = "mean",
    #       col_var   = dplyr::cur_column(),
    #       in_totrow = TRUE,
    #       in_tottab = TRUE
    #     )),
    #     .groups = "drop")
  }

  if (df | num) {
    if (df) return(as_df_merge_rownames(tabs, rlang::as_name(row_var)))

    return(dplyr::group_by(new_tab(tibble::as_tibble(tabs)),
                           !!!tab_vars))
  }



  totrow_vector <- dplyr::pull(tabs, !!row_var) == "Total"
  if (length(tab_vars) == 0) {
    tottab_vector <- rep(FALSE, nrow(tabs))
  } else {
    tottab_vector <- dplyr::transmute(tabs, tottab = dplyr::if_all(
      tidyselect::all_of(as.character(tab_vars)),
      ~ . == "Total"
    )) |>
      tibble::deframe()
  }
  comp_group <- if (comp == "tab") { as.character(tab_vars) } else { character() }

  diff_index_mean <-  function(ref, row_var, num_names) { #needed for ci too
    if (ref == "tot"   ) return(-1L)
    if (ref == "first" ) return(1L )
    if (is.numeric(ref)) return(as.integer(ref[1]))

    index <- which(stringr::str_detect(row_var, ref))

    if (length(index) >= 2) warning(paste0(
      "with ref = '", ref, "' , several rows were found as ",
      "reference for comparison ; only the first was kept ; ",
      "to remove this warning, precise the value of ref ",
      "until there is only one row_var level matched"
    ))

    index <- tidyr::replace_na(dplyr::first(index), 0)
    if (length(index) == 0) index <- 0

    index
  }


  #Differences and confidence intervals
  if (!ref %in% c("no", "") | ci %in% c("cell", "diff")) {

    if (ref != "tot") {
      refrows <-
        if(comp == "tab") {
          tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
            dplyr::group_by(!!!tab_vars) |>
            dplyr::transmute(
              var =
                dplyr::row_number() == if (diff_index_mean(ref, !!row_var) == -1) {
                  dplyr::n()
                } else {
                  diff_index_mean(ref, !!row_var)
                }
            ) |>
            dplyr::pull("var")

        } else {
          tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
            dplyr::mutate(tottab_vector = tottab_vector) |>
            dplyr::group_by(!!!tab_vars) |>
            dplyr::transmute(
              var = dplyr::if_else(
                condition = .data$tottab_vector,
                true  = dplyr::row_number() == if (diff_index_mean(ref, !!row_var) == -1) {
                  dplyr::n()
                } else {
                  diff_index_mean(ref, !!row_var)
                },
                false = FALSE
              )
            ) |>
            dplyr::pull("var")
        }

      if (!any(refrows)) {
        warning(paste0(
          "in ref = '", ref, "' , no rows were found as reference for comparison ; ",
          "to remove this warning, precise the value of ref ",
          "until there is one row_var level matched"
        ))
      }

    } else {
      refrows <- if (comp == "tab") { totrow_vector } else { totrow_vector & tottab_vector }
    }
    #tabs_diff$DIPLOME[refrows] |> as.character()



    refrows <- tidyr::replace_na(refrows, FALSE)

    tabs[, "ref_rows___" := refrows]

    #Differences
    if (!ref %in% c("no", "") ) {
      tabs[, paste0(col_vars, "_diff") := purrr::map(
        rlang::syms(paste0(col_vars, "_mean")),
        ~ eval(.) / dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) ) #diff_index_mean(diff, eval(row_var))
      ),
      by = eval(comp_group)]
    }




    #Confidence intervals
    if (ci == "cell") {

      tabs[, paste0(col_vars, "_ci") := purrr::map(
        col_vars,
        ~ ci_mean(xvar = eval(rlang::sym(paste0(., "_var"))),
                  xn   = eval(rlang::sym(paste0(., "_n"))),
                  conf_level = conf_level)
      )]

    } else if (ci == "diff") {
      tabs[, paste0(col_vars, "_ci") := purrr::map(
        col_vars,
        ~ ci_mean_diff(xvar = eval(rlang::sym(paste0(., "_var"))),
                       xn   = eval(rlang::sym(paste0(., "_n"))),
                       yvar = dplyr::nth(eval(rlang::sym(paste0(., "_var"))),
                                         tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) ), #diff_index_mean(ref, eval(row_var))
                       yn   = dplyr::nth(eval(rlang::sym(paste0(., "_n"))),
                                         tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) ), #diff_index_mean(ref, eval(row_var))
                       conf_level = conf_level)
      ),
      by = eval(comp_group)]

      tabs[, paste0(col_vars, "_ci") :=
             purrr::map(.SD, ~ dplyr::if_else(1:.N %in% which(refrows),
                                              true  = NA_real_,
                                              false = .)),
           .SDcols = paste0(col_vars, "_ci") ]
    }

    tabs[, "ref_rows___" := NULL]
  }





  #Make the final table with fmt vectors
  # remove(list = c("tabs_n", "tabs_wn", "tabs_pct", "tabs_diff", "tabs_ci", "refcols_vector", "refrows"))

  text_vars <- !purrr::map_lgl(tabs, is.numeric)
  NA_reals <- rep(NA_real_, nrow(tabs))

  #n <- as.integer(tabs[["n"]])
  #wn <- if ("wn" %in% names(tabs)) { tabs[["wn"]] } else { NA_reals }

  tabs_n  <-
    data.table::setnames(tabs[, stringr::str_detect(names(tabs), "_n$"), with = FALSE] ,
                         function(.x) stringr::str_remove(.x, "_n$" ))

  tabs_wn  <-
    if (length(wt) != 0) {
      data.table::setnames(tabs[, stringr::str_detect(names(tabs), "_wn$"), with = FALSE] ,
                           function(.x) stringr::str_remove(.x, "_wn$" ))
    } else {
      list(NA_reals)
    }

  tabs_mean  <-
    data.table::setnames(tabs[, stringr::str_detect(names(tabs), "_mean$"), with = FALSE] ,
                         function(.x) stringr::str_remove(.x, "_mean$" ))

  #Nan to NA
  tabs_mean <- tibble::as_tibble(tabs_mean) |>
    dplyr::mutate(dplyr::across(
      where(~ any(is.nan(.))),
      ~ dplyr::if_else(is.nan(.), NA_real_, .)
    )) |>
    data.table::as.data.table()


  tabs_var  <-
    data.table::setnames(tabs[, stringr::str_detect(names(tabs), "_var$"), with = FALSE],
                         function(.x) stringr::str_remove(.x, "_var$" ))


  are_diff <- stringr::str_detect(names(tabs), "_diff$")
  tabs_diff  <-
    if (any(are_diff)) {
      data.table::setnames(tabs[, are_diff, with = FALSE] ,
                           function(.x) stringr::str_remove(.x, "_diff$" ))
    } else {
      list(NA_reals)
    }

  are_ci <- stringr::str_detect(names(tabs), "_ci$")
  tabs_ci  <-
    if (any(are_ci)) {
      data.table::setnames(tabs[, are_ci, with = FALSE] ,
                           function(.x) stringr::str_remove(.x, "_ci$" ))
    } else {
      list(NA_reals)
    }

  tabs_text <- tabs[, text_vars, with = FALSE]

  if (ref %in% c("tot", "no", "")) refrows <- rep(FALSE, nrow(tabs))


  tabs <-
    list(tabs_n, tabs_wn, tabs_mean, tabs_var, tabs_diff, tabs_ci, as.character(col_vars), digits) |>
    purrr::pmap_dfc(~ new_fmt(
      display   = if (ci_visible) { "mean_ci" } else { "mean" },
      digits    = vec_recycle(..8, length(..1)),
      n         = ..1,
      wn        = ..2,
      mean      = ..3,
      var       = ..4,
      diff      = ..5,
      ci        = ..6,
      in_totrow = totrow_vector,
      in_tottab = tottab_vector,
      in_refrow = refrows,
      color     = color,
      type      = "mean",
      ref = switch(as.character(ref), "no" = "", "tot" = "tot", as.character(ref)),
      ci_type   = ci, #dplyr::if_else(ci == "diff", "diff", ci),
      comp      = dplyr::if_else(ref != "no" | ci != "no", comp == "all", NA),
      col_var   = ..7
    ))

  tabs <- dplyr::bind_cols(tibble::as_tibble(tabs_text), tabs)


  #Rename totals
  if (totaltab %in% c("line", "table") &  totaltab_name != "Total") {
    tabs <- tabs |> dplyr::mutate(dplyr::across(
      tidyselect::all_of(as.character(tab_vars)),
      ~ dplyr::if_else(tottab_vector,
                       true  = factor(totaltab_name, c(levels(.), totaltab_name)),
                       false = .) |>
        forcats::fct_drop()
    ))
  }

  if (length(tab_vars) == 0) {

    if ("row" %in% tot & total_names[1] != "Total") tabs <- tabs |>
        dplyr::mutate(!!row_var := forcats::fct_recode(!!row_var,
                                                       purrr::set_names("Total", total_names[1])))
  } else {
    tabs <- tabs |>
      tidyr::unite(col = "tabs_tot_names", !!!tab_vars, sep = " ", remove = FALSE) |>
      dplyr::mutate(
        !!row_var := dplyr::if_else(
          totrow_vector,
          true  = paste(total_names[1], .data$tabs_tot_names) |>
            forcats::fct_expand(levels(!!row_var)) |>
            forcats::fct_relevel(levels(!!row_var)),
          false = !!row_var) |>
          forcats::fct_drop()
        #forcats::fct_recode(!!row_var,
        #                               purrr::set_names("Total", total_names[1]))
      ) |>
      select(-"tabs_tot_names")
  }

  if ("col" %in% tot & total_names[2] != "Total") tabs <- tabs |>
    dplyr::rename(tidyselect::any_of(purrr::set_names("Total", total_names[2])))





  # Add argument to transpose the table ?
  # if (row_var_type == "numeric") {
  #   tabs <- tabs %>%
  #   tidyr::pivot_wider(names_from = !!fct_var, values_from = !!num_var,
  #                      names_glue = "{.value}_{.name}",
  #                      values_fill = fmt0("mean", digits, type = "mean"))
  #   if (as.character(tab_vars) == "no_tab_vars") {
  #     tabs <- tabs %>% dplyr::mutate(no_row_var = factor("no_row_var")) %>%
  #       dplyr::relocate(no_row_var, .before = 1)
  #   }
  # }


  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                                    ~ length(unique(.)) == 1))

  if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext) %>%
      dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs %>% dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext)
  }
}




#' Add total table to a \code{\link[tabxplor]{tab}}
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
#' @examples \donttest{ data <- dplyr::starwars %>%
#' tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'             na_drop_all = sex)
#'
#' data %>%
#'   tab_plain(sex, hair_color, gender) %>%
#'   tab_totaltab("line")
#'   }
tab_totaltab <- function(tabs, totaltab = c("table", "line", "no"),
                         name = tr_("Ensemble"), data = NULL) {
  #.Deprecated("tab_plain() and tab_num(), which now have a totaltab argument")

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
    dplyr::filter(!.data$tottab) %>% dplyr::select(-"tottab")

  if (totaltab[1] == "no") return(tabs)

  #Calculate the total table
  totaltable <- switch(
    totaltab[1],
    "table" = tibble::as_tibble(tabs) %>% tibble::add_column(totrow = is_totrow(.)) %>%
      dplyr::filter(!.data$totrow) %>% dplyr::select(-"totrow") %>%
      dplyr::group_by(!!row_var) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_tottab(sum(.) ))),

    "line"  = tibble::as_tibble(tabs) %>% tibble::add_column(totrow = is_totrow(.)) %>%
      dplyr::filter(!.data$totrow) %>% dplyr::select(-"totrow") %>%
      dplyr::group_by(!!row_var) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), sum)) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(as_tottab(sum(.))))) %>%
      dplyr::mutate(!!row_var := paste("TOTAL", stringr::str_to_upper(name)))
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
      "table" = purrr::map(mean_vars, ~ tab_plain(data, !!row_var,
                                                  col_var = !!rlang::sym(.))),
      "line" = purrr::map(mean_vars, ~tab_plain(data, col_var = !!rlang::sym(.)))
    )
    mean_calc <-
      purrr::reduce(mean_calc,
                    ~ dplyr::full_join(.x, .y, by = switch(totaltab[1],
                                                           "table" = as.character(row_var),
                                                           "line"  =  "no_row_var") ) ) %>%
      dplyr::select(-tidyselect::starts_with("no_row_var")) %>%
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




#' Add totals to a \code{\link[tabxplor]{tab}}
# @description
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
#' @examples \donttest{data <- dplyr::starwars %>% tab_prepare(sex, hair_color)
#'
#' data %>%
#'   tab_plain(sex, hair_color) %>%
#'   tab_tot("col", totcol = "each")
#'   }
tab_tot <- function(tabs, tot = c("row", "col"), name = tr_("Total"),
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
  col_vars_levels <- col_vars_levels_mean %>%
    purrr::discard(names(.) %in% names(mean_vars))
  tab_vars        <- rlang::syms(get_vars$tab_vars)

  groups <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

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

    if (any(totrows)) tabs <- tabs %>%
      tibble::add_column(totrows, tottab_line) %>%
      dplyr::filter(!.data$totrows | .data$tottab_line) %>%
      dplyr::select(-"totrows", -"tottab_line")
  }

  if ("col" %in% tot | tot[1] == "no") tabs <- tabs %>%
    dplyr::select(-where(is_totcol))

  if (tot[1] == "no") return(tabs)


  # Total rows
  if ("row" %in% tot) {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows #& totrows

    tabs <- tabs %>% tibble::add_column(tottab_rows, tottab_line)

    if (length(groups) != 0) {
      group_vars_totals <-
        dplyr::group_keys(dplyr::filter(tabs, !.data$tottab_line)) %>% #dplyr::mutate(bis = PR0) %>%
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

    totrows <- tabs %>% dplyr::filter(!.data$tottab_line) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(sum(.)) ),
                       .groups = "drop") %>%
      dplyr::bind_cols(group_vars_totals) %>%
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
        ) ) %>%
        dplyr::select(-tidyselect::contains("no_row_var")) %>%
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
                        ~ dplyr::full_join(.x, .y ,by = character() ) ) %>%
          dplyr::select(-tidyselect::starts_with("no_row_var")) %>%
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
      dplyr::select(-"tottab_line", -"tottab_rows")
  }


  #Total columns
  if ("col" %in% tot) {
    col_vars_2levels_or_more <-
      col_vars_levels[purrr::map_int(col_vars_levels, length) >= 2]

    if (length(col_vars_2levels_or_more) != 0 | totcol[1] == "last") {
      tabs <- tabs %>% dplyr::rowwise()

      if (totcol[1] == "last") {
        # c_across don't work. Workaround with quosures : sum(!!!col_vars_levels)
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


#' Add percentages and diffs to a \code{\link[tabxplor]{tab}}
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
  col_vars_no_all  <- col_vars_with_all %>% purrr::discard(. == "all_col_vars")
  col_means  <- (get_type(tabs) == "mean") %>% purrr::keep(., .) %>% names()
  # col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  tab_vars         <- rlang::syms(get_vars$tab_vars)

  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  pct <- vctrs::vec_recycle(pct, length(col_vars_no_all)) %>%
    purrr::set_names(col_vars_no_all)
  pct[col_means] <- "no"

  if (just_diff == FALSE) {

    if (all(pct == "no")) {
      tabs <- tabs %>% dplyr::mutate(dplyr::across(
        where(~ get_type(.) %in% c("row", "col", "all", "all_tabs")),
        ~ set_pct(., NA_real_) %>% set_type("n") %>%
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
          tabs <- tabs %>% tab_totaltab('line')
        }
        tabs <- tabs %>%
          dplyr::with_groups(NULL, ~ tab_match_groups_and_totrows(.) %>%
                               tab_add_totcol_if_no()
          )
      }
    }

    if ( any(pct %in% c("col", "all") ) | (any(pct == "row") & ref[1] == "tot") ) {
      tabs <- tabs %>% tab_match_groups_and_totrows()
    }

    if ( any(pct %in% c("row", "all")) | (any(pct == "col") & ref[1] == "tot") ) {
      tabs <- tabs %>% tab_add_totcol_if_no()
    }

    comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
    tabs <- tabs %>% tab_match_comp_and_tottab(comp)

    if (any(pct != "no")){
      pct <- c(pct, all_col_vars = dplyr::last(pct[pct != "no"]))
      pct <- purrr::map_chr(tabs, ~ pct[get_col_var(.)] ) %>%
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
        digits <- vctrs::vec_recycle(digits, length(col_vars_with_all)) %>%
          purrr::set_names(col_vars_with_all)
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
      reference_cols <- purrr::map_chr(reference, as.character) %>% unique()
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
      tabs <- tabs %>%
        dplyr::with_groups(
          NULL,
          ~ dplyr::mutate(., dplyr::across(
            where(~ get_type(.) %in% c("row", "col", "mean")),
            ~ set_diff(., diff_formula(
              .,
              type = type[[dplyr::cur_column()]],
              ref = ref[1],
              refer  = rlang::eval_tidy(reference[[dplyr::cur_column()]])
            )) %>% set_diff_type(ref[1])
          ))
        )

    } else {
      tabs <- tabs %>%
        dplyr::mutate(dplyr::across(
          where(~ get_type(.) %in% c("row", "col", "mean") ) &
            !( where(is_totcol) &
                 tidyselect::any_of(names(reference)[reference == ""]) ),
          ~ set_diff(., diff_formula(
            .,
            type = type[[dplyr::cur_column()]],
            ref = ref[1],
            refer = rlang::eval_tidy(reference[[dplyr::cur_column()]])
          )) %>% set_diff_type(ref[1])
        ))
    }

    if ( any(type %in% c("row", "mean")) ) tabs <- tabs %>%
        dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., comp[1] == "all")))

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
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext,
                    chi2 = chi2)
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


#Ci spread (negative numbers mean no significant difference)
#' Add confidence intervals to a \code{\link[tabxplor]{tab}}
#'
#' @param tabs A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
#' @param ci The type of ci to calculate. Set to "cell" to calculate absolute confidence
#' intervals. Set to "diff" to calculate the confidence intervals of the difference
#' between a cell and the relative total cell (or the reference cell,
#'  when `ref` is not `"tot"` in \code{\link{tab_plain}} or \code{\link{tab_num}}).
#'  By default, "diff" ci are calculated for means and row and col percentages,
#'  "cell" ci for frequencies ("all", "all_tabs").
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
#' @param method_cell Character string specifying which method to use with percentages
#'  for \code{ci = "cell"}. This can be one out of:
#' "wald", "wilson", "wilsoncc", "agresti-coull", "jeffreys", "modified wilson",
#' "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting", "pratt",
#' "midp", "lik" and "blaker". Defaults to "wilson".
#' See \code{\link[DescTools:BinomCI]{BinomCI}}.
#' @param method_diff Character string specifying which method to use with percentages
#' for \code{ci = "diff"}. This can be one out of: "wald", "waldcc", "ac", "score",
#' "scorecc", "mn", "mee", "blj", "ha", "hal", "jp". Defaults to "ac", Wald interval with
#' the adjustment according to Agresti, Caffo for difference in proportions and
#' independent samples. See \code{\link[DescTools:BinomDiffCI]{BinomDiffCI}}.
#' @param color The type of colors to print, as a single string.
#' \itemize{
#'   \item \code{"no"}: by default, no colors are printed
#'   \item \code{"diff_ci"}: color pct and means based on cells differences from totals
#'   or first cells, removing coloring when the confidence interval of this difference
#'   is higher than the difference itself
#'   \item \code{"after_ci"}: idem, but cut off the confidence interval from the
#'   difference
#' }
#' @param visible By default confidence intervals are calculated and used to set colors,
#' but not printed. Set to \code{TRUE} to print them in the result.
#'
#' @return A \code{tibble} of class \code{tab}, colored based on differences (from
#' totals/first cells) and confidence intervals.
#' @export
#'
#' @examples # A typical workflow with tabxplor step-by-step functions :
#' \donttest{
#' data <- dplyr::starwars %>%
#'   tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'               na_drop_all = sex)
#'
#' data %>%
#'   tab_plain(sex, hair_color, gender, tot = c("row", "col"),
#'     pct = "row", comp = "all") %>%
#'     tab_ci("diff", color = "after_ci")
#'   }
tab_ci <- function(tabs,
                   ci = "auto",
                   comp = NULL,
                   conf_level = 0.95,
                   color = "no",
                   visible = FALSE,
                   method_cell = "wilson", method_diff = "ac") {
  stopifnot(all(ci %in% c("auto", "cell", "diff", "no")), #"r_to_r", "c_to_c", "tab_to_tab",
            all(comp %in%  c("tab", "all"))
  )

  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  get_vars          <- tab_get_vars(tabs)
  col_vars_with_all <- rlang::syms(get_vars$col_vars)
  col_vars_no_all   <- col_vars_with_all %>% purrr::discard(. == "all_col_vars")

  fmtc <- purrr::map_lgl(tabs, is_fmt)
  ci <- vctrs::vec_recycle(ci, length(col_vars_no_all)) %>%
    purrr::set_names(col_vars_no_all)
  ci <- c(ci, all_col_vars = dplyr::last(ci[ci != "no"]))
  ci <- purrr::map_chr(tabs, ~ ci[get_col_var(.)] ) %>%
    tidyr::replace_na(NA_character_)

  visible <- vctrs::vec_recycle(visible, length(col_vars_no_all)) %>%
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

  ref <- get_ref_type(tabs)
  ref_cols  <- detect_firstcol(tabs)
  ref_cols[is.na(ci)] <- list(rlang::sym(""))

  ref_cols <- dplyr::if_else(ref == "tot",
                             true  = tot_cols,
                             false = ref_cols     ) %>%
    purrr::set_names(names(ref)) #keep ci_yes ?
  names_refcols <- ref_cols %>% purrr::map_chr(as.character) %>% unique() %>%
    purrr::discard(. == "")

  ci[fmtc] <- dplyr::case_when(
    !type[fmtc] %in% c("mean", "row", "col", "all", "all_tabs") ~ "no"      ,
    ci[fmtc] == "cell"                                          ~ "cell"    ,
    ci[fmtc] == "diff"   & type[fmtc] %in% c("row", "mean")     ~ "diff_row",
    ci[fmtc] == "diff"   & type[fmtc] == "col"                  ~ "diff_col",

    ci[fmtc] == "auto"   & type[fmtc] %in% c("row", "mean")     ~ "diff_row",
    ci[fmtc] == "auto"   & type[fmtc] == "col"                  ~ "diff_col",
    ci[fmtc] == "auto"   & type[fmtc] %in% c("all","all_tabs")  ~ "cell"    ,

    TRUE                                                        ~ "no"
  )


  #Depending of ci type, totals and reference cols (for diff), not calculate ci
  ci <- dplyr::if_else(
    condition = (!type %in% c("row", "col", "all", "all_tabs", "mean")) |
      (ci %in% c("diff_col", "spread_col") & type == "mean"),
    true = "no",
    false = ci
  )
  ci_with_ref <- ci %>% purrr::set_names(names(tabs))
  ci <- dplyr::if_else(
    condition = (ci == "diff_col" & names(tabs) %in% names_refcols) |
      (ci == "diff_col" & get_col_var(tabs) == "all_col_vars") |
      (ci == "diff_row" & names(tabs) %in% names_totcols),
    true = "no",
    false = ci
  )
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

    ci_select <- rlang::expr(tidyselect::all_of(names(ci_yes)[ci_yes]))
    diff_select <- rlang::expr(tidyselect::all_of(
      names(ci_yes)[ci %in% c("diff_row", "diff_col")]
    ))
    mean_select <- rlang::expr(tidyselect::all_of(
      names(ci_yes)[ci =="diff_row" & type == "mean"]
    ))
    row_select <- rlang::expr(tidyselect::all_of(
      names(ci_yes)[ci =="diff_row"]
    ))

    ref_rows <- tabs %>% dplyr::transmute(dplyr::across(
      !!row_select,
      ~ .[dplyr::last(which(switch(get_ref_type(.),
                                   "tot" = is_totrow(.),
                                   is_refrow(.)         )))]
    ))

    tot_rows <- tabs %>% dplyr::transmute(dplyr::across(
      !!ci_select & where(~ get_type(.) == "col"),
      ~ .[dplyr::last(which(is_totrow(.)))]
    ))

    ref_to_na <- tabs %>% dplyr::transmute(dplyr::across(
      !!ci_select,
      ~ tidyr::replace_na(dplyr::row_number() ==
                            dplyr::last(which(switch(get_ref_type(.),
                                                     "tot" = is_totrow(.) ,
                                                     is_refrow(.)))),
                          FALSE)
    ))

    tabs_nogroup <- tabs %>% dplyr::ungroup()

    #The n for each cell is the n of the relative 100% total
    # set to NA for reference, because we don't want to calculate it's ci
    x_n <- tabs_nogroup %>%
      dplyr::transmute(dplyr::across(
        !!ci_select,
        ~ dplyr::if_else(
          condition = ref_to_na[[dplyr::cur_column()]],
          true      = NA_integer_,
          false     = switch(
            get_type(.),
            "col" = get_n(tot_rows[[dplyr::cur_column()]]),
            "row" = get_n(rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])),
            "mean" = get_n(.)
          )
        )
      ))
    # tabs_ci %>% dplyr::mutate(dplyr::across(where(is_fmt), get_n))

    ref <- tabs_nogroup %>%
      dplyr::transmute(dplyr::across(
        !!diff_select,
        ~ switch(
          ci[[dplyr::cur_column()]],
          "diff_col" = get_pct(rlang::eval_tidy(ref_cols[[dplyr::cur_column()]])),
          "diff_row" = switch(get_type(.),
                              "mean" = get_mean(ref_rows[[dplyr::cur_column()]]),
                              get_pct(ref_rows[[dplyr::cur_column()]])
          )
        )
      ))
    # tabs_ci %>% dplyr::mutate(dplyr::across(where(is_fmt), get_ci))

    ref_var <- tabs_nogroup %>%
      dplyr::transmute(dplyr::across(
        !!mean_select,
        ~ get_var(ref_rows[[dplyr::cur_column()]])
      ))
    # tabs_ci %>% dplyr::mutate(dplyr::across(where(is_fmt), get_ctr))

    # The n for the comparison reference cells is the relative 100% total
    # - for means it is the n of the reference cell
    # - for row pct it is the n of the 100% cell of the reference row
    # - for col pct it is the n of the 100% cell of the reference col
    ref_n <- tabs %>%
      dplyr::transmute(dplyr::across(
        !!diff_select,
        ~ switch(ci[[dplyr::cur_column()]],
                 "diff_col" = rlang::eval_tidy(
                   ref_cols[[dplyr::cur_column()]]
                 )[dplyr::last(which(is_totrow(.)))] %>% get_n(),
                 "diff_row" = switch(
                   get_type(.),
                   "mean" = .[dplyr::last(which(switch(get_ref_type(.),
                                                       "tot" = is_totrow(.),
                                                       is_refrow(.))))] %>%
                     get_n(), # = n of ref_rows (copy error with groups)

                   rlang::eval_tidy(
                     tot_cols[[dplyr::cur_column()]]
                   )[dplyr::last(which(switch(get_ref_type(.),
                                              "tot" = is_totrow(.),
                                              is_refrow(.))))] %>%
                     get_n()
                 )
        )
      ))

    #Formulas :

    zs <- zscore_formula(conf_level)

    ci_mean      <- function(xvar, xn) {
      zs * sqrt( xvar / xn )
    }

    ci_mean_diff <- function(xvar, xn, yvar, yn) {
      zs * sqrt( xvar/xn + yvar/yn )
    }

    ci_base <- function(xpct, xn, method_cell) {
      #zs * sqrt(xpct*(1 - xpct)/xn)

      DescTools::BinomCI(xpct * xn, xn,
                         conf.level = conf_level, method = method_cell) %>%
        as.data.frame() %>% dplyr::mutate(ci = .data$upr.ci - .data$est ) %>%
        dplyr::pull(.data$ci)
    }

    ci_diff <-  function(xpct, xn, ypct, yn, method_diff) {
      #zs * sqrt( xpct*(1 - xpct)/xn   +   ypct*(1 - ypct)/yn )

      DescTools::BinomDiffCI(x1 = xpct * xn, n1 = xn,
                             x2 = ypct * yn, n2 = yn,
                             conf.level = conf_level, method = method_diff)  %>%
        as.data.frame() %>% dplyr::mutate(ci = .data$upr.ci - .data$est ) %>%
        dplyr::pull(.data$ci)
    }

    #Calculate the confidence intervals
    tabs <- tabs %>%
      dplyr::with_groups(
        NULL,
        ~ dplyr::mutate(., dplyr::across(
          !!ci_select,
          ~ set_ci(., switch(
            ci[[dplyr::cur_column()]],
            "cell"        = switch(
              get_type(.),
              "mean" = ci_mean(xvar = get_var(.),
                               xn   = x_n[[dplyr::cur_column()]]),

              ci_base(xpct        = get_pct(.),
                      xn          = x_n[[dplyr::cur_column()]],
                      method_cell = method_cell)
            ),

            "diff_col"   = ,
            "diff_row"   = switch(
              get_type(.),
              "mean" = ci_mean_diff(xvar = get_var(.),
                                    xn   = x_n[[dplyr::cur_column()]],
                                    yvar = ref_var[[dplyr::cur_column()]],
                                    yn   = ref_n[[dplyr::cur_column()]]),

              ci_diff(xpct        = get_pct(.),
                      xn          = x_n[[dplyr::cur_column()]],
                      ypct        = ref[[dplyr::cur_column()]],
                      yn          = ref_n[[dplyr::cur_column()]],
                      method_diff = method_diff)
            )
          ))
        )))
    #tabs %>% dplyr::mutate(dplyr::across(where(is_fmt), get_ci))


    #Change ci_type and color, even for totals with no ci result
    ci_with_ref <- stringr::str_remove(ci_with_ref, "_row|_col")
    ci_yes_ref  <- !is.na(ci_with_ref) & !ci_with_ref == "no"

    tabs[ci_yes_ref] <-
      purrr::map2_df(tabs[ci_yes_ref],
                     ci_with_ref[ci_yes_ref],
                     ~ set_ci_type(.x, .y) %>%
                       set_color(
                         ifelse(!is.null(color[1]) & ! color[1] %in% c("no", ""),
                                color[1], get_color(.))
                       ))

    if (any(ci == "diff_row")) tabs <- tabs %>%
      dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., comp[1] == "all")))

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
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext,
                    chi2 = chi2)
  }
}





#' Add Chi2 summaries to a \code{\link[tabxplor]{tab}}
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
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
#' @return A \code{tibble} of class \code{tab}, with Chi2 summaries as metadata,
#' possibly colored based on contributions of cells to variance.
#' @export
#'
# @examples # A typical workflow with tabxplor step-by-step functions :
# \donttest{
# data <- dplyr::starwars %>%
#   tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#               na_drop_all = sex)
#
# data %>%
#   tab_plain(sex, hair_color, gender, tot = c("row", "col")) %>%
#   tab_chi2(calc = c("p", "ctr"), color = TRUE)
#   }
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

  is_a_mean <-
    purrr::map_lgl(col_vars_levels,
                   ~ purrr::map_lgl(dplyr::select(dplyr::ungroup(tabs), !!!.),
                                    ~ get_type(.) == "mean") %>% any()
    )
  if (all(is_a_mean)) {
    calc <- "counts"
  } else {
    tabs <- tabs %>% tab_match_groups_and_totrows() %>% tab_add_totcol_if_no()
  }

  if (comp == "all") tabs <- tabs %>% dplyr::ungroup()

  tot_cols <- detect_totcols(tabs)


  all_col_tot <- names(col_vars_levels) == "all_col_vars"

  tot_cols_names <- purrr::map_lgl(tabs, is_totcol) #%>%  .[.] %>% names()
  tot_cols_names <- tot_cols_names[tot_cols_names] %>% names()
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
                     where(is.double), ~ dplyr::if_else(.data$totrows, 0,
                                                        dplyr::if_else(is.na(.), 0, 1))
                   )) %>%
                   dplyr::select(-"totrows")
      )


    cells_by_row <- cells_calc[!is_a_mean & !all_col_tot] %>%
      purrr::map2(col_vars_levels_no_tot[!all_col_tot & !is_a_mean],
                  ~ dplyr::mutate(.x, cells = sum(!!!.y), .groups = "drop") %>%
                    dplyr::pull(.data$cells)
      )

    cells_by_group[!is_a_mean[!all_col_tot]] <-
      cells_calc[!is_a_mean[!all_col_tot]] %>%
      purrr::map2(col_vars_levels_no_tot[!all_col_tot & !is_a_mean],
                  ~ dplyr::summarise(.x[!is_totrow(tabs),],
                                     cells = sum(!!!.y), .groups = "drop") %>%
                    dplyr::pull(.data$cells)
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
                                          true      = set_ctr(., 1/.data$.cell),
                                          false     = set_ctr(., .data$.var   ) )
                       )) %>%
                       dplyr::select(-".var", -".cell")
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
      dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., comp[1] == "all")))

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
        purrr::map_int(~ dplyr::mutate(.,dplyr::across(.cols = dplyr::everything(), .fns = get_n)) %>%
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
                      dplyr::select(where(~ !is_totcol(.))) %>% # ?????
                      dplyr::group_split() %>%
                      purrr::map( ~ dplyr::select(., where(is_fmt)) ) %>%
                      purrr::map(
                        ~ dplyr::mutate(., dplyr::across(.cols = dplyr::everything(), .fns = get_n)) %>%
                          dplyr::select(-where(~ sum(., na.rm = TRUE) == 0)) %>%
                          dplyr::rowwise() %>%
                          dplyr::filter(! sum(dplyr::c_across(cols = dplyr::everything()),
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
    pvalue_p     <- purrr::map(pvalues, ~ dplyr::pull(., .data$pvalue))
    #pvalue_w     <- purrr::map(pvalues, ~ dplyr::pull(., .data$warnings))
    pvalue_df    <- purrr::map(pvalues, ~ dplyr::pull(., .data$df) %>% as.integer())
  }


  #Assemble everything and put it into the metadata of the tab
  #if (length(groups) != 0) {
  tables <- tabs[!is_totrow(tabs),] %>% dplyr::select(!where(is_totcol)) %>%
    dplyr::summarise(row_var = factor(row_var), .groups = "drop") #%>%
  #dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ stringr::str_remove_all(., "/") ))


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

  # purrr::map(counts            , length)
  # purrr::map(pvalue_p          , length)
  # purrr::map(variances_by_group, length)
  # purrr::map(cells_by_group    , length)
  # purrr::map(pvalue_df         , length)

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
    ) %>%
    purrr::set_names(names(col_vars_levels[!all_col_tot]))
  #%>%
  # purrr::map(~ dplyr::mutate(., warnings = dplyr::if_else(
  #   stringr::str_detect(.data$warnings, "incorrect"), "!", ""))
  # )

  chi2 <- chi2 %>% purrr::imap(
    ~ dplyr::mutate(.x, dplyr::across(
      tidyselect::any_of(c("count", "pvalue",
                           "variance", "cells", "df")),
      as.double)) %>%
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
        set_pct(get_var(.)) %>% set_digits(2L),
      `chi2 stats` == "count"    ~ as_totrow(.),
      TRUE                       ~ .
    ))) %>% new_tab()

  tabs <- tabs %>% dplyr::select(-tidyselect::any_of("tottabs"))

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext,
                    chi2 = chi2)
  }
}





# INTERNAL FUNCTIONS #####################################################################

#' @keywords internal
tab_match_groups_and_totrows <- function(tabs) {
  #chi2 : not to match groups and totrows with alltabs ? ----

  #tab_vars <- tab_get_vars(tabs)$tab_vars
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
    if (utils::tail(is_totrow(tabs), 1L)) return(dplyr::ungroup(tabs))


    tabs_totrow_groups <- tabs %>% dplyr::ungroup() %>%
      tibble::add_column(totrow_groups = as.integer(is_totrow(.))) %>%
      dplyr::mutate(totrow_groups = 1 + cumsum(.data$totrow_groups) - .data$totrow_groups)
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
    return(dplyr::relocate(tabs_totrow_groups, .data$totrow_groups, .before = 1) %>%
             dplyr::group_by(.data$totrow_groups)
    )

  }

}



#' @keywords internal
tab_add_totcol_if_no <- function(tabs) {
  if (!any(is_totcol(tabs)) & ! all(get_type(tabs) == "mean")) { # & !only_one_column
    only_one_column <- length(which(purrr::map_lgl(tabs, is_fmt))) == 1L
    tabs <- tabs %>% tab_tot("col", totcol = "last")
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
    tabs <- tabs %>% tab_totaltab('line')
  }
  tabs
}



#' @keywords internal
weighted.var <- function(x, wt, na.rm = FALSE) {
  #Nwt_non_zero <- length((wt)[wt != 0])
  round(
    sum(wt * (x - stats::weighted.mean(x, wt, na.rm = na.rm))^2,  na.rm = na.rm) /
      ( sum(wt, na.rm = na.rm) ),
    10)
  #((Nwt_non_zero - 1) / Nwt_non_zero) *
} #Same results as sqrt(Hmisc::wtd.var(!!num_var, !!wt, na.rm = TRUE, method = "ML")


#' @keywords internal
zscore_formula <- function(conf_level) {
  # Calculate the z-score for the given confidence level (thanks to mindcrime) :
  # https://datascience.stackexchange.com/questions/10093/how-to-find-a-confidence-level-given-the-z-value
  stopifnot(conf_level >= 0, conf_level <= 1)
  stats::qnorm((1 - conf_level)/2,lower.tail = FALSE)
}


#' @keywords internal
ci_mean      <- function(xvar, xn, conf_level) {
  zscore_formula(conf_level) * sqrt( xvar / xn )
}

#' @keywords internal
ci_mean_diff <- function(xvar, xn, yvar, yn, conf_level) {
  zscore_formula(conf_level) * sqrt( xvar/xn + yvar/yn )
}

# #' @keywords internal
# ci_base <- function(xpct, xn, conf_level) {
#   #zs * sqrt(xpct*(1 - xpct)/xn)
#
#   DescTools::BinomCI(xpct * xn, xn,
#                      conf.level = conf_level, method = method_cell) %>%
#     as.data.frame() %>% dplyr::mutate(ci = .data$upr.ci - .data$est ) %>%
#     dplyr::pull(.data$ci)
# }
#
# #' @keywords internal
# ci_diff <-  function(xpct, xn, ypct, yn, conf_level) {
#   #zs * sqrt( xpct*(1 - xpct)/xn   +   ypct*(1 - ypct)/yn )
#
#   DescTools::BinomDiffCI(x1 = xpct * xn, n1 = xn,
#                          x2 = ypct * yn, n2 = yn,
#                          conf.level = conf_level, method = method_diff)  %>%
#     as.data.frame() %>% dplyr::mutate(ci = .data$upr.ci - .data$est ) %>%
#     dplyr::pull(.data$ci)
# }



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


#' @keywords internal
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




#' @keywords internal
quo_miss_na_null_empty_no <- function(quo) {
  if (rlang::quo_is_missing(quo)) return (TRUE)
  if (rlang::quo_is_null(quo)) return(TRUE)
  quo <- rlang::get_expr(quo) %>% as.character()
  all(is.na(quo) | quo %in% c("", "no"))
}


#' @keywords internal
as_df_merge_rownames <- function(tabs, row_var) {
  text_cols <- !purrr::map_lgl(tabs, is.numeric)
  text_cols <- names(text_cols)[which(text_cols)]
  new_rownames  <- paste0(text_cols, collapse = "_")

  if (length(text_cols) >= 2) {
    tabs <- tabs %>%
      tibble::as_tibble() %>%
      dplyr::mutate(!!new_rownames :=
                      paste(!!!purrr::map(text_cols, rlang::sym), sep = "_")) %>%
      dplyr::select(-tidyselect::all_of(text_cols)) %>%
      dplyr::relocate(where(is.character), .before = 1) %>%
      tibble::column_to_rownames(var = new_rownames)
  } else {
    #tabs <- tabs %>% tibble::column_to_rownames(var = rlang::as_name(row_var))
    rnames <- as.character(tabs[[row_var]])
    tabs[, eval(row_var) := NULL]
    data.table::setDF(tabs, rownames = rnames)
  }

  tabs
}

#' @keywords internal
relabel_levels_in_varnames <- function(data, col_vars) {
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(col_vars) &
        where(~ (is.factor(.) & any(levels(.) %in% names(data))) |
                (is.character(.) & any(. %in% names(data)) )       ) ,
      ~ forcats::fct_relabel(., ~ dplyr::if_else(. %in% names(data), paste0(., "_lv"), .))
    ))
}

#' @keywords internal
diff_index <-  function(ref, row_var, num_names, pct) {
  if (ref == "tot"   ) return(-1L)
  if (ref == "first" ) return(1L )
  if (is.numeric(ref) | !is.na(suppressWarnings(as.integer(ref)))
  ) {
    return(as.integer(ref[1]))
  }

  index <-
    switch(pct,
           "row" = which(stringr::str_detect(row_var, ref)),

           "col" = which(stringr::str_detect(num_names, ref))
    )
  if (length(index) >= 2) {
    switch(pct,
           "row" = warning(paste0(
             "with ref = '", ref, "' , several rows were found as ",
             "reference for comparison ; only the first was kept ; ",
             "to remove this warning, precise the value of ref ",
             "until there is only one row_var level matched"
           )),

           "col" = warning(paste0(
             "with ref = '", ref, "' , several columns were found as ",
             "reference for comparison ; only the first was kept ; ",
             "to remove this warning, precise the value of ref ",
             "until there is only one column matched"
           ))
    )
  }
  index <- tidyr::replace_na(dplyr::first(index), 0)

  if (length(index) == 0) index <- 0

  index
}

#' @keywords internal
calculate_refrows <- function(tabs, ref, comp, tab_row_names, tab_vars,
                              row_var, tottab_vector, totrow_vector, # pct,
                              num_names) {
  if (ref != "tot") {
    refrows <-
      if(comp == "tab") {
        tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
          dplyr::group_by(!!!tab_vars) |>
          dplyr::transmute(
            var =
              dplyr::row_number() == if (diff_index(ref, !!row_var,
                                                    num_names = num_names,
                                                    pct = "row") == -1) {
                dplyr::n()
              } else {
                diff_index(ref, !!row_var, num_names = num_names, pct = "row")
              }
          ) |>
          dplyr::pull("var")

      } else {
        tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
          dplyr::mutate(tottab_vector = tottab_vector) |>
          dplyr::group_by(!!!tab_vars) |>
          dplyr::transmute(
            var = dplyr::if_else(
              condition = .data$tottab_vector,
              true  = dplyr::row_number() == if (diff_index(ref, !!row_var,
                                                            num_names = num_names,
                                                            pct = "row") == -1) {
                dplyr::n()
              } else {
                diff_index(ref, !!row_var, num_names = num_names, pct = "row")
              },
              false = FALSE
            )
          ) |>
          dplyr::pull("var")
      }
    #tabs_diff$DIPLOME[refrows] |> as.character()

    if (!any(refrows)) {
      warning(paste0(
        "in ref = '", ref, "' , no rows were found as reference for comparison ; ",
        "to remove this warning, precise the value of ref ",
        "until there is one row_var level matched"
      ))
    }
  } else {
    refrows <- if (comp == "tab") { totrow_vector } else { totrow_vector & tottab_vector }
  }
  refrows <- tidyr::replace_na(refrows, FALSE)

  return(refrows)
}



# tab_ci former implementation ----
# tabs_nogroup <- tabs %>% dplyr::ungroup() %>% .[ci_yes]
#
# #Compute all variables needed to calculate ci in different tabs
# xbase <- tabs_nogroup %>%
#   dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns =  ~ dplyr::if_else(
#     condition = get_display(.) == "mean",
#     true      = get_mean(.),
#     false     = get_pct(.)
#   )))
#
# xvar <- tabs_nogroup %>%
#   dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ dplyr::if_else(
#     condition = get_display(.) == "mean",
#     true      = get_var(.),
#     false     = NA_real_
#   )))
#
# ybase <-
#   tibble::tibble(ci, tot_cols, names = rlang::syms(names(tabs))) %>%
#   dplyr::filter(ci_yes) %>%
#   purrr::pmap_df(function(ci, tot_cols, names) switch(
#     ci,
#     "cell"     = NA_real_,
#     "diff_col" = dplyr::pull(tabs, !!tot_cols),
#     "diff_row" = dplyr::mutate(tabs, comp = dplyr::last(!!names)) %>%
#       dplyr::pull(comp)
#   ))
#
# yvar <- ybase %>%
#   dplyr::mutate(dplyr::across(where(~ !get_type(.)=="mean"), ~NA_real_)) %>%
#   dplyr::mutate(dplyr::across(where(~ get_type(.) =="mean"), get_var))
#
# ybase <- ybase %>%
#   dplyr::mutate(dplyr::across(
#     where(~ is_fmt(.) & !get_type(.) == "mean"),
#     get_pct
#   )) %>%
#   dplyr::mutate(dplyr::across( where(~ get_type(.) == "mean"), get_mean))
#
# xn <-
#   tibble::tibble(type, tot_cols, names = rlang::syms(names(tabs))) %>%
#   dplyr::filter(ci_yes) %>%
#   purrr::pmap_df(function(type, tot_cols, names) switch(
#     type,
#     "row"      = dplyr::pull(tabs, !!tot_cols) %>% get_n(),
#     "mean"     = dplyr::pull(tabs, !!names   ) %>% get_n(),
#     "col"      = dplyr::mutate(tabs, xn = dplyr::last(get_n(!!names)) ) %>%
#       dplyr::pull(xn),
#     "all"      = ,
#     "all_tabs" = dplyr::mutate(tabs, xn = dplyr::last(get_n(!!tot_cols)) ) %>%
#       dplyr::pull(xn),
#     NA_integer_
#   ))
#
# yn <-
#   tibble::tibble(ci, type, tot_cols, names = rlang::syms(names(tabs))) %>%
#   dplyr::filter(ci_yes) %>%
#   purrr::pmap_df(function(ci, type, tot_cols, names) switch(
#     ci,
#     "cell"       = NA_real_,
#     "diff_col"   =
#       switch(type,
#              "row"      = dplyr::pull(tabs, !!tot_cols) %>% get_n(),
#              "col"      = ,
#              "all"      = ,
#              "all_tabs" =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!tot_cols)) ) %>%
#                dplyr::pull(yn),
#              NA_real_
#       ),
#     "diff_row"   =
#       switch(type,
#              "mean"     = ,
#              "col"      =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!names)) ) %>%
#                dplyr::pull(yn),
#              "row"      = ,
#              "all"      = ,
#              "all_tabs" =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!tot_cols)) ) %>%
#                dplyr::pull(yn),
#              NA_real_
#       )
#   ) )
#
#
# ci_map <-
#   list(xbase = xbase, xvar = xvar,
#        ybase = ybase, yvar = yvar,
#        xn = xn, yn = yn) %>%
#   purrr::map(~purrr::map(., ~ .)) %>%
#   purrr::transpose() %>% purrr::map(~ tibble::as_tibble(.)) %>%
#   tibble::tibble(.name_repair = ~ "ci_map") %>%
#   tibble::add_column(ci = ci[ci_yes], type = type[ci_yes]) %>%
#   dplyr::mutate(ci_map = dplyr::if_else(
#     ci %in% c("diff_col", "diff_row"),
#     true  = purrr::map(ci_map, ~ dplyr::mutate(., xn = dplyr::if_else(
#       condition =
#         ( comp == "tab" & is_totrow(tabs) ) |
#         ( comp == "all" & append(rep(FALSE, nrow(tabs) - 1), TRUE)),
#       true      = NA_integer_,
#       false     = xn)
#     )),
#     false = ci_map
#   ) %>% purrr::set_names(names(tabs)[ci_yes])
#   )
#
# calculations <- ci_map %>%
#   purrr::pmap(function(ci_map, ci, type)
#     dplyr::mutate(ci_map, res = switch(
#       ci,
#       "cell"        = switch(type,
#                              "mean" = ci_mean(xvar = xvar, xn = xn),
#                              ci_base(xpct = xbase, xn = xn)
#       ),
#       "diff_col"   = ,
#       "diff_row"   = switch(type,
#                             "mean" = ci_mean_diff(xvar = xvar, xn = xn,
#                                                   yvar = yvar, yn = yn),
#                             ci_diff(xpct = xbase, xn = xn,
#                                     ypct = ybase, yn = yn)
#       ),
#       # "spread_col" = ,
#       # "spread_row" = switch(type,
#       #                       "mean" = ci_mean_spread(
#       #                         xmean = xbase,  xvar = xvar, xn = xn,
#       #                         ymean = ybase,  yvar = yvar, yn = yn
#       #                       ),
#       #                       ci_diff_spread(xpct = xbase, xn = xn,
#       #                                      ypct = ybase, yn = yn)
#       # ),
#       "no"         = NA_real_,
#     ) ) )
#
# result <- calculations %>% purrr::map_df(~ dplyr::pull(., res))
#
# tabs[ci_yes] <- purrr::map2_df(tabs[ci_yes], result, ~ set_ci(.x, .y) )
