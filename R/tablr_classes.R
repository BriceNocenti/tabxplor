# Create class single_tabr--------------------------------------------------------
# sloop::s3_methods_class("tbl")
# sloop::s3_get_method(print.tbl)
# cli::cat_line()
# sloop::s3_get_method(format.tbl)
# tibble::trunc_mat #Gives classes :
# c("trunc_mat_single_tabr", "trunc_mat_tbl_df", "trunc_mat_tbl", "trunc_mat_data.frame", "trunc_mat")
# sloop::s3_methods_class("tibble::trunc_mat")
# sloop::s3_get_method(format.tibble::trunc_mat)
# sloop::s3_get_method(print.tibble::trunc_mat)
# sloop::s3_methods_class("pillar_colonnade")
# sloop::s3_get_method(format.pillar_colonnade)
# sloop::s3_get_method(print.pillar_colonnade)
# pillar::squeeze
# sloop::s3_methods_class("single_tabr")


#' Class single_tabr
#'
#' @param x A table, stored into a \code{\link[tibble]{tibble}} data.frame.
#' It is generally made with \code{\link{tabw}} or \code{\link{tabmulti}}.
#' @param perc The type of percentages of this table : "no", "row", "col", "all"
#'  or "all_tabs".
#' @param pvalue_Chi2 A tibble storing information about pvalues, unweighted
#' counts, and variances.
#' @param total_table TRUE when it is the total table of a \code{\link{tabr}}
#' (list of tables).
#' @param subtext A character vector to print legend lines under the table
#' with \code{\link{tabxl}}
#' @param force_unique_table TRUE when multiple tables are bound into one.
#' @param print_sup TRUE when supplementary rows and cols are printed into the
#' main table.
#'
#' @return A table of class single_tabr.
#' @export
#  @examples
single_tabr <-
  function(x = tibble::tibble(), perc = "no", pvalue_Chi2 = tibble::tibble(), total_table = FALSE,
           subtext = "", force_unique_table = FALSE, print_sup = c(FALSE, FALSE)) {
    x <- tibble::as_tibble(x)
    perc <- as.character(perc[1])
    pvalue_Chi2 <- tibble::as_tibble(pvalue_Chi2)
    total_table <- as.logical(total_table[1])
    subtext <- as.character(subtext)
    force_unique_table <- as.logical(force_unique_table[1])
    print_sup <- as.logical(print_sup)
    new_single_tabr(x, nrow = nrow(x), perc = perc, pvalue_Chi2 = pvalue_Chi2,
                    total_table = total_table, subtext = subtext,
                    force_unique_table = force_unique_table, print_sup = print_sup) # validate_single_tabr(new_single_tabr(x))
  }

# Useful test fonction :
#' @describeIn single_tabr A test function for class single_tabr
#' @export
is_single_tabr <- function(x) {
  inherits(x, "single_tabr")
}

# as_single_tabr <- function(x, ...) {
#   UseMethod("as_single_tabr")
# }
# as_single_tabr.default <- function(x, ...) {
#   #vctrs::vec_cast(x, single_tabr())
# }

#' @keywords internal
new_single_tabr <-
  function(x = tibble::tibble(), nrow = 0L, perc = "no", pvalue_Chi2 = tibble::tibble(), total_table = FALSE,
           subtext = "", force_unique_table = FALSE, print_sup = c(FALSE, FALSE),  ..., class = NULL) {
    #x <- vctrs::vec_assert(x, tibble::tibble()) #check type or size
    nrow <- vctrs::vec_assert(nrow, ptype = integer(), size = 1)
    if (nrow == 0L) nrow <- nrow(x)
    perc <- vctrs::vec_assert(perc, ptype = character(), size = 1)
    #pvalue_Chi2 <- vctrs::vec_assert(pvalue_Chi2, ptype = tibble::tibble())
    total_table <- vctrs::vec_assert(total_table, ptype = logical(), size = 1)
    subtext <- vctrs::vec_assert(subtext, ptype = character())
    force_unique_table <- vctrs::vec_assert(force_unique_table, ptype = logical(), size = 1)
    print_sup <- vctrs::vec_assert(print_sup, ptype = logical(), size = 2)
    tibble::new_tibble(x, nrow = nrow, class = "single_tabr", perc = perc, pvalue_Chi2 = pvalue_Chi2,
                       total_table = total_table, subtext = subtext,
                       force_unique_table = force_unique_table, print_sup = print_sup) #... ?
  }

#' Print method for class single_tabr
#'
#' @param x An object of class single_tabr.
#' @param ... Arguments passed to print.default
#'
#' @return The printed single table.
#' @export
#'
# @examples
print.single_tabr <- function(x, ...) {
  #cli::cat_line(format(x, ..., n = 30, width = 500)) #Can be use to color bg and text

  if (nrow(x) > 0 & ncol(x) > 0) {
    out <- x
    if (class(dplyr::pull(x, 1)) %in% c("factor", "character")) {
      #First column must be same length for all tabs
      # (for that must be a factor with the levels of all the other tables)
      out <- dplyr::mutate_at(x, dplyr::vars(1), ~ as.factor(.))
      max_length_all <- dplyr::pull(out, 1) %>% levels() %>% stringr::str_length() %>% max(na.rm = TRUE)
      if (dplyr::pull(out, 1) %>% stringr::str_length() %>% max(na.rm = TRUE) < max_length_all) {
        out <- dplyr::mutate_at(out, dplyr::vars(1), ~ `levels<-`(., stringr::str_pad(levels(.), max_length_all - 2, "right")))
      }
      #Truncate first column if too long
      if (dplyr::pull(out, 1) %>% stringr::str_length() %>% max(na.rm = TRUE) > 30) {
        out <-
          dplyr::mutate_at(out, dplyr::vars(1), ~ `levels<-`(., dplyr::if_else(stringr::str_length(levels(.)) > 30,
                                                                               stringr::str_trunc(levels(.), 30),
                                                                               levels(.) )))
      }
    }
    #Truncate columns names if too long
    if (any(stringr::str_length(colnames(out)) > 15) ) {
      out <- magrittr::set_colnames(out, dplyr::if_else(stringr::str_length(colnames(out)) > 15,
                                                        stringr::str_trunc(colnames(out), 15),
                                                        colnames(out) ) )
    }
    cli::cat_line(format(pillar::colonnade(out, width = 500))) #Less formatting but no "A tibble::tibble" introduction
  } else {
    cli::cat_line("# A single_tabr: 0 x 0", col = "grey")
  }

  #invisible(x)
}

#Add with pillar_shaft : - contributions to variance (in attribute)





#Create class tabr -------------------------------------------------------

#  @examples

#' Class tabr
#  @description
#' @param tabs Dataframes, possible of class single_tabr,
#' to be joined into a list of tables.
#' @param args Arguments of functions \code{\link{tabw}} and
#' \code{\link{tabmulti}} used when printing the tables with \code{\link{tabxl}}.
#' @param col_var_sort The variable to use to sort the single tabs.
#' @param result_var The name of variables to use to reconstite the tables from
#' wtable with \code{\link{tabdraw}}.
#' @param pvalue_Chi2 A tibble storing information about pvalues, unweighted
#' counts, and variances of all the tabs in the list.
#' @param wtable A unique \code{\link[tibble]{tibble}} dataframe with all data
#' necessary to draw the list of tabs.
#'
#' @return A list of tables. Objects of class \code{\link{tabr}} are printed
#' with an adapted method.
#' @export
tabr <- function(tabs = list(), args = list(), col_var_sort = character(), result_var = character(), pvalue_Chi2 = tibble::tibble(), wtable = tibble::tibble()) {
  tabs <- vctrs::vec_cast(tabs, list()) #take anything coercible as a list
  #tabs %<>% purrr::map(~ single_tabr(.)) #Lose unofficial attributes ?
  wtable <- tibble::as_tibble(wtable) #vctrs::vec_recycle(vctrs::vec_cast(wtable, tibble::tibble()))
  pvalue_Chi2 <- tibble::as_tibble(pvalue_Chi2)
  col_var_sort <- vctrs::vec_cast(col_var_sort, list())
  result_var <- vctrs::vec_cast(result_var, character())
  args <- vctrs::vec_cast(args, list())
  new_tabr(tabs, args = args, col_var_sort = col_var_sort, result_var = result_var, pvalue_Chi2 = pvalue_Chi2,  wtable = wtable)
}

#' @describeIn tabr A test function for class tabr (list of tables)
#' @export
is_tabr <- function(x) {
  inherits(x, "tabr")
}

#' @keywords internal
new_tabr <- function(tabs = list(), args = list(), col_var_sort = list(), result_var = character(), pvalue_Chi2 = tibble::tibble(), wtable = tibble::tibble()) {
  tabs <- vctrs::vec_assert(tabs, list()) #check type or size
  result_var <- vctrs::vec_assert(result_var, character())
  col_var_sort <- vctrs::vec_assert(col_var_sort, list())
  args <- vctrs::vec_assert(args, list())
  #purrr::map(tabs, ~ vctrs::vec_assert(., ptype = single_tabr()) ) #don't work
  #if ( !all(purrr::map_lgl(tabs, ~ is_single_tabr(.))) ) stop("all elements in the list must be of class single_tabr")
  if ( !tibble::is_tibble(wtable)) stop("the wtable (data frame necessary to make the tabs) must be a tibble")
  if ( !tibble::is_tibble(pvalue_Chi2)) stop("data frame with pvalues and variances must be a tibble")
  #vctrs::vec_assert(wtable, ptype = tibble::tibble()) #check type or size
  #vctrs::vec_assert(digits, ptype = integer(), size = 1)
  vctrs::new_vctr(tabs, args = args, col_var_sort = col_var_sort, result_var = result_var, pvalue_Chi2 = pvalue_Chi2, wtable = wtable,
                  class = "tabr", inherit_base_type = TRUE)
}


#' Print method for class tabr
#'
#' @param x A tabr object
#' @param ... Arguments passed to print.default
#'
#' @return The printed list of tables, preceded by a pvalue_Chi2 summary.
#' @export
#'
# @examples
print.tabr <- function(x, ...) {
  cat("\n")
  purrr::pluck(x, purrr::attr_getter("pvalue_Chi2")) %>% pillar::colonnade(has_row_id = FALSE) %>% print()
  cat("\n")
  x <- `attributes<-`(x, list("names" = purrr::pluck(x, purrr::attr_getter("names"))))
  print.default(x, ...)
}
# sloop::s3_dispatch(print(tabs))
# sloop::s3_get_method(print.default)

#tabr(tabs, wtable)
# single_tabr()
# purrr::map(tabs, ~class(.))

# class(tabs)
# class(wtable)
# class(tibble::tibble())



