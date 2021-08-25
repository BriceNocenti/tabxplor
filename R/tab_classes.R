# Special tibble class needed for printing, even if the most meaningful attributes
#  where passed to fmt class variables (only chi2 and subtext remains at tab level) :
#  my implementation relies on "grouped_df" class structure, and to manage it, it is
#  necessary to add one method for class "grouped_tab" for each dplyr function...
#  (Thank to Giulia Pais, Davis Vaughan and Hadley Wickham,
#   https://github.com/tidyverse/dplyr/issues/5480).

# grouped_tab class still don’t handle [] ----

#Import dplyr in NAMESPACE :
# dplyr is imported as a "Depends" package, otherwise dplyr::filter, needed for methods,
# cannot be found by roxygen2 because it replaces base::filter.

#' Internal dplyr methods
#' @rawNamespace import(dplyr, except = data_frame)
#  otherwise, conflict with vctrs. Thanks to Thomas :
#  https://stackoverflow.com/questions/51899220/import-all-the-functions-of-a-package-except-one-when-building-a-package
#' @keywords internal
#' @name tabxplor-dplyr
NULL

# Create class tab -----------------------------------------------------------------------
# sloop::s3_methods_class("tbl")
# sloop::s3_get_method(print.tbl)
# cli::cat_line()
# sloop::s3_get_method(format.tbl)
# tibble::trunc_mat #Gives classes :
# c("trunc_mat_single_tab", "trunc_mat_tbl_df", "trunc_mat_tbl", "trunc_mat_data.frame", "trunc_mat")
# sloop::s3_methods_class("tibble::trunc_mat")
# sloop::s3_get_method(format.tibble::trunc_mat)
# sloop::s3_get_method(print.tibble::trunc_mat)
# sloop::s3_methods_class("pillar_colonnade")
# sloop::s3_get_method(format.pillar_colonnade)
# sloop::s3_get_method(print.pillar_colonnade)
# pillar::squeeze
# sloop::s3_methods_class("single_tab")

#' A constructor for class tab
#'
#' @param tabs A table, stored into a \code{\link[tibble]{tibble}} data.frame.
#' It is generally made with \code{\link{tab}}, \code{\link{tab_many}}
#' or \code{\link{tab_core}}.
#' @param subtext A character vector to print legend lines under the table.
#' @param chi2 A tibble storing information about pvalues and variances, to fill with
#' \code{\link{tab_chi2}}.
#' @param ... Needed to implement subclasses.
#' @param class Needed to implement subclasses.
#'
#' @return A \code{tibble} of class \code{tab}.
#' @export
#  @examples
new_tab <-
  function(tabs = tibble::tibble(), subtext = "",
           chi2 = tibble::tibble(tables   = character(),
                                 pvalue   = double()   ,
                                 df       = integer()  ,
                                 cells    = integer()  ,
                                 variance = double()   ,
                                 count    = integer()   ),
           ..., class = character()) {
    stopifnot(is.data.frame(tabs))
    #vec_assert(subtext    , character())

    tibble::new_tibble(tabs, subtext = subtext, chi2 = chi2, ...,
                       nrow = nrow(tabs), class = c(class, "tab"))
  }

#' @param groups The grouping data.
#' @rdname new_tab
#' @export
new_grouped_tab <-
  function(tabs = tibble::tibble(), groups,
           subtext = "",
           chi2 = tibble::tibble(tables   = character(),
                                 pvalue   = double()   ,
                                 df       = integer()  ,
                                 cells    = integer()  ,
                                 variance = double()   ,
                                 count    = integer()   ),
           ..., class = character()) {
    if (missing(groups)) groups <- attr(tabs, "groups")
    class <- c(class, c("grouped_tab", "grouped_df"))

    new_tab(tabs, groups = groups,
            subtext = subtext, chi2 = chi2,
            ...,
            class = class)
  }



# Functions to work with class tab -------------------------------------------------------

# Useful test fonction :
#' @describeIn tab_many a test function for class tab
#' @param x A object to test with \code{\link{is_tab}}.
#' @export
is_tab <- function(x) {
  inherits(x, "tab")
}

get_subtext <- purrr::attr_getter("subtext")
get_chi2    <- purrr::attr_getter("chi2")

# # In doc exemple they do :
#  df_colour <- function(x) {
# if (inherits(x, "my_tibble")) {
#   attr(x, "colour")
# } else {
#   NULL
# }
# }


# as_tab <- function(x, ...) {
#   UseMethod("as_tab")
# }
# as_tab.default <- function(x, ...) {
#   #vctrs::vec_cast(x, tab())
# }

#' @keywords internal
untab <- function(tabs) {
  if (lv1_group_vars(tabs)) {
    `class<-`(tabs, class(tabs) %>% purrr::discard(. == "tab"))
  } else {
    `class<-`(tabs, class(tabs) %>%
                        purrr::discard(. %in% c("grouped_tab", "tab")))
  }
  }


#Methods for class tab -------------------------------------------------------------------

#' @export
#' @method print tab
print.tab <- function(x, ..., n = 100, width = NULL, n_extra = NULL) {
  print_colors <- tab_color_legend(x)
  subtext <- get_subtext(x) %>% purrr::discard(. == "")
  cli::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  if (length(print_colors) != 0) cli::cat_line(paste0(
    pillar::style_subtle("# "), print_colors
    ))
  if (length(subtext) != 0) cli::cat_line(pillar::style_subtle(
    paste0("# ", subtext)
    ))
  invisible(x)
  }

#' @export
#' @method print grouped_tab
print.grouped_tab <- function(x, ..., n = 100, width = NULL, n_extra = NULL) {
  print_colors <- tab_color_legend(x)
  subtext <- get_subtext(x) %>% purrr::discard(. == "")
  cli::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  if (length(print_colors) != 0) cli::cat_line(paste0(
    pillar::style_subtle("# "), print_colors
  ))
  if (length(subtext) != 0) cli::cat_line(pillar::style_subtle(
    paste0("# ", subtext)
  ))
  invisible(x)
}

# test <- "\033[3m\033[38;5;246m<fct>\033[39m\033[23m"
# crayon::col_nchar(test)
# crayon::col_substr(test, 2, 4) %>% cli::cat_line()

# #' Print method for class tab
# #'
# #' @param x An object of class tab.
# #' @param ... Arguments passed to print.default
# #' @return The printed single table.
# #' @export
# print.tab <- function(x, ...) {
#   # cat(sprintf("<%s: %s>\n", class(x)[[1]], df_colour(x)))
#   # cli::cat_line(format(x)[-1])
#
#   #cli::cat_line(format(x, ..., n = 30, width = 500)) #Can be use to color bg and text
#
#   if (nrow(x) > 0 & ncol(x) > 0) {
#     out <- x
#     if (class(dplyr::pull(x, 1)) %in% c("factor", "character")) {
#       # #First column must be same length for all tabs
#       # # (for that must be a factor with the levels of all the other tables)
#       # out <- dplyr::mutate_at(x, dplyr::vars(1), ~ as.factor(.))
#       # max_length_all <- dplyr::pull(out, 1) %>% levels() %>% stringr::str_length() %>% max(na.rm = TRUE)
#       # if (dplyr::pull(out, 1) %>% stringr::str_length() %>% max(na.rm = TRUE) < max_length_all) {
#       #   out <- dplyr::mutate_at(out, dplyr::vars(1), ~ `levels<-`(., stringr::str_pad(levels(.), max_length_all - 2, "right")))
#       # }
#       #Truncate first column if too long
#       if (dplyr::pull(out, 1) %>% stringr::str_length() %>% max(na.rm = TRUE) > 30) {
#         out <-
#           dplyr::mutate_at(out, dplyr::vars(1), ~ `levels<-`(., dplyr::if_else(stringr::str_length(levels(.)) > 30,
#                                                                                stringr::str_trunc(levels(.), 30),
#                                                                                levels(.) )))
#       }
#     }
#     #Truncate columns names if too long
#     if (any(stringr::str_length(colnames(out)) > 15) ) {
#       out <- magrittr::set_colnames(out, dplyr::if_else(stringr::str_length(colnames(out)) > 15,
#                                                         stringr::str_trunc(colnames(out), 15),
#                                                         colnames(out) ) )
#     }
#     cli::cat_line(format(pillar::colonnade(out, width = 500))) #Less formatting but no "A tibble::tibble" introduction
#   } else {
#     cli::cat_line("# A tab: 0 x 0", col = "grey")
#   }
#
#   #invisible(x)
# }

#Two possibility : by rows ; by cols ???
#Two type vectors : columns and rows

#Put informations on fmt on printing ?
#tabs %>% dplyr::group_indices()


#Add with pillar_shaft : - contributions to variance (in attribute)


# #Define abbreviated type name (for tibble::tibble headers)
# #' @export
# vec_ptype_abbr.tab <- function(x, ...) {
#   "tab"
# }

# # Include numbers of digits and types in the printed name
#  #' @export
#  vec_ptype_full.tab <- function(x, ...) {
#    "tab-"
#  }



# If n_groups == 1, then go back to new_tab ----


# @importFrom dplyr group_by # not needed since tabxplor import dplyr as a "Depends" package
#' @method group_by tab
#' @export
group_by.tab <- function(.data,
                         ...,
                         .add = FALSE,
                         .drop = group_by_drop_default(.data)) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  new_grouped_tab(out, groups,
                  subtext = get_subtext(.data), chi2 = get_chi2(.data))
}



# (from vctrs documentation)
# The coercion methods for data frames operate in two steps:
# • They check for compatible subclass attributes. In our case the tibble colour has to
# be the same, or be undefined.
# • They call their parent methods, in this case tib_ptype2() and tib_cast() because we
# have a subclass of tibble. This eventually calls the data frame methods df_ptype2() and
# tib_ptype2() which match the columns and their types.

#' Coercion between two tab
#' @param x,y,to Subclasses of data frame.
#' @param ... For future extensions.
#' @param x_arg Argument names for x and y. These are used in error messages to inform
#' the user about the locations of incompatible types.
#' @param to_arg Argument names for x and to. These are used in error messages to inform
#' the user about the locations of incompatible types.
#'
#' @export
tab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  subtext     <- vec_c(get_subtext(x), get_subtext(to)) %>% unique()
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]
  chi2        <- vec_rbind(get_chi2(x), get_chi2(to))

  new_tab(out, subtext = subtext, chi2 = chi2)
}

#' @rdname tab_cast
#' @param y_arg Argument names for x and y. These are used in error messages to inform
#' the user about the locations of incompatible types.
#' @export
tab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  out <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  #colour <- df_colour(x) %||% df_colour(y)

  chi2        <- vec_rbind(get_chi2(x), get_chi2(y))
  subtext     <- vec_c(get_subtext(x), get_subtext(y)) %>% unique()
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]

  new_tab(out, subtext = subtext, chi2 = chi2)
}


#Let’s now implement the coercion methods, starting with the self-self methods.
#' @export
vec_ptype2.tab.tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_cast.tab.tab <- function(x, to, ...) {
  tab_cast(x, to, ...)
}

# The methods for combining our class with tibbles follow the same pattern.
# For ptype2 we return our class in both cases because it is the richer type
#' @export
vec_ptype2.tab.tbl_df <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.tbl_df.tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_cast.tab.tbl_df <- function(x, to, ...) {
  tab_cast(x, to, ...)
}
#' @export
vec_cast.tbl_df.tab <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

#' @export
vec_ptype2.tab.data.frame <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_cast.tab.data.frame <- function(x, to, ...) {
  tab_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.tab <- function(x, to, ...) {
  df_cast(x, to, ...)
}




#Methods for class grouped_tab------------------------------------------------------------
# @importFrom dplyr ungroup
#' @method ungroup grouped_tab
#' @export
ungroup.grouped_tab <- function (x, ...)
{
  if (missing(...)) {
    new_tab(x, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
  else {
    old_groups <- dplyr::group_vars(x)
    to_remove  <- tidyselect::vars_select(names(x), ...)
    new_groups <- setdiff(old_groups, to_remove)
    dplyr::group_by(x, !!!syms(new_groups))
  }
}

#' @keywords internal
lv1_group_vars <- function(tabs) {
  dplyr::n_groups(tabs) <= 1

  #groupvars <- dplyr::group_vars(tabs)
  # all(purrr::map_lgl(groupvars,
  #                ~ nlevels(forcats::fct_drop(dplyr::pull(tabs, .))) == 1)) |
  #   length(groupvars) == 0
}

# @importFrom dplyr mutate
#' @method mutate grouped_tab
#' @export
mutate.grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }

}




# @importFrom dplyr transmute
#' @method transmute grouped_tab
#' @export
transmute.grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# @importFrom dplyr summarise
#' @method summarise grouped_tab
#' @export
summarise.grouped_tab <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# @importFrom dplyr filter
#' @method filter grouped_tab
# @rdname filter
#' @export
filter.grouped_tab <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# slice not working with grouped_tab ? ----
# @importFrom dplyr slice
#' @method slice grouped_tab
#' @export
slice.grouped_tab <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# @importFrom dplyr arrange
#' @method arrange grouped_tab
#' @export
arrange.grouped_tab <- function(.data, ..., .by_group = FALSE) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# @importFrom dplyr select
#' @method select grouped_tab
#' @export
select.grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# @importFrom dplyr relocate
#' @method relocate grouped_tab
#' @export
relocate.grouped_tab <- function(.data, ..., .before = NULL, .after = NULL) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# @importFrom dplyr rename
#' @method rename grouped_tab
#' @export
rename.grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# @importFrom dplyr rename_with
#' @method rename_with grouped_tab
#' @export
rename_with.grouped_tab <- function(.data, .fn,
                                    .cols = dplyr::everything(), ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# @importFrom dplyr distinct
#' @method distinct grouped_tab
#' @export
distinct.grouped_tab <- function(.data, ...,  .keep_all = FALSE) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}







#' @rdname tab_cast
#' @export
gtab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  #based upon vctrs:::gdf_cast()
  df <- df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  gdf <- dplyr::grouped_df(df, vars, drop = drop)

  groups <- dplyr::group_data(gdf)
  new_grouped_tab(gdf, groups, subtext = get_subtext(to), chi2 = get_chi2(to))
}

#' @rdname tab_cast
#' @export
gtab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  #based upon vctrs:::gdf_ptype2
  common <- df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  x_vars <- dplyr::group_vars(x)
  y_vars <- dplyr::group_vars(y)
  vars <- union(x_vars, y_vars)
  drop <- dplyr::group_by_drop_default(x) && dplyr::group_by_drop_default(y)
  gdf <-  dplyr::grouped_df(common, vars, drop = drop)

  groups <- dplyr::group_data(gdf)
  new_grouped_tab(gdf, groups, subtext = get_subtext(x), chi2 = get_chi2(x))
  }

#Self-self
#' @export
vec_ptype2.grouped_tab.grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.grouped_tab.grouped_tab <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}

#grouped_tab / grouped_df
#' @export
vec_ptype2.grouped_tab.grouped_df <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.grouped_df.grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.grouped_tab.grouped_df <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @export
vec_cast.grouped_df.grouped_tab <- function(x, to, ...) {
  #vctrs:::gdf_cast
  df <- df_cast(x, to, ...)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  dplyr::grouped_df(df, vars, drop = drop)
}

#grouped_tab / tab
#' @export
vec_ptype2.grouped_tab.tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.tab.grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.grouped_tab.tab <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @export
vec_cast.tab.grouped_tab <- function(x, to, ...) {
  tab_cast(x, to, ...)
}

#grouped_tab / tbl_df
#' @export
vec_ptype2.grouped_tab.tbl_df <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.tbl_df.grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.grouped_tab.tbl_df <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @export
vec_cast.tbl_df.grouped_tab <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

#grouped_tab / data.frame
#' @export
vec_ptype2.grouped_tab.data.frame <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.grouped_tab.data.frame <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.grouped_tab <- function(x, to, ...) {
  df_cast(x, to, ...)
}





#Colors for printing fmt in tabs -------------------------------------------------------
pos5  <- crayon::make_style(rgb(0, 5, 0, maxColorValue = 5),
                            colors = crayon::num_colors(forget = TRUE)) #hcl(120, 200, 100)
pos4  <- crayon::make_style(rgb(1, 5, 1, maxColorValue = 5))
pos3  <- crayon::make_style(rgb(3, 5, 1, maxColorValue = 5))
pos2  <- crayon::make_style(rgb(4, 5, 1, maxColorValue = 5))
pos1  <- crayon::make_style(rgb(4, 4, 1, maxColorValue = 5)) #5, 5, 1

neg5  <- crayon::make_style(rgb(5, 0, 0, maxColorValue = 5))
neg4  <- crayon::make_style(rgb(5, 1, 0, maxColorValue = 5))
neg3  <- crayon::make_style(rgb(5, 2, 1, maxColorValue = 5))
neg2  <- crayon::make_style(rgb(5, 3, 1, maxColorValue = 5))
neg1  <- crayon::make_style(rgb(4, 3, 2, maxColorValue = 5))

fmtgrey4 <- crayon::make_style(grey(0.9), grey = TRUE)
fmtgrey3 <- crayon::make_style(grey(0.7), grey = TRUE)
fmtgrey2 <- crayon::make_style(grey(0.5), grey = TRUE)
fmtgrey1 <- crayon::make_style(grey(0.3), grey = TRUE)

posb5  <- crayon::make_style(rgb(0, 0, 5, maxColorValue = 5)) #hcl(120, 200, 100)
posb4  <- crayon::make_style(rgb(0, 1, 5, maxColorValue = 5))
posb3  <- crayon::make_style(rgb(0, 3, 5, maxColorValue = 5))
posb2  <- crayon::make_style(rgb(1, 4, 5, maxColorValue = 5))
posb1  <- crayon::make_style(rgb(2, 5, 5, maxColorValue = 5))

negb5  <- crayon::make_style(rgb(5, 0, 0, maxColorValue = 5))
negb4  <- crayon::make_style(rgb(5, 0, 1, maxColorValue = 5))
negb3  <- crayon::make_style(rgb(5, 1, 1, maxColorValue = 5))
negb2  <- crayon::make_style(rgb(5, 1, 3, maxColorValue = 5))
negb1  <- crayon::make_style(rgb(5, 2, 3, maxColorValue = 5))



# cat("\n",
#     pos1("42%" ), neg1("42%\n" ),
#     pos2("42%" ), neg2("42%\n" ),
#     pos3("42%" ), neg3("42%\n" ),
#     pos4("42%" ), neg4("42%\n" ),
#     pos5("42%" ), neg5("42%\n" ),
#     "\n",
#     posb1("42%"), negb1("42%\n" ),
#     posb2("42%" ), negb2("42%\n" ),
#     posb3("42%" ), negb3("42%\n" ),
#     posb4("42%" ), negb4("42%\n" ),
#     posb5("42%" ), negb5("42%\n" ),
#     "\n",
#     "42%"       ,       "42%\n" ,
#     fmtgrey4("42%"), fmtgrey4("42%\n"),
#     fmtgrey3("42%"), fmtgrey3("42%\n"),
#     fmtgrey2("42%"), fmtgrey2("42%\n"),
#     fmtgrey1("42%"), fmtgrey1("42%\n") )

bgpos5   <- crayon::make_style(rgb(0, 5, 0, maxColorValue = 5), bg = TRUE) #hcl(120, 200, 100)
bgpos4   <- crayon::make_style(rgb(0, 4, 0, maxColorValue = 5), bg = TRUE)
bgpos3   <- crayon::make_style(rgb(0, 3, 0, maxColorValue = 5), bg = TRUE)
bgpos2   <- crayon::make_style(rgb(0, 2, 0, maxColorValue = 5), bg = TRUE)
bgpos1   <- crayon::make_style(rgb(0, 1, 0, maxColorValue = 5), bg = TRUE) #5, 5, 1

bgneg5   <- crayon::make_style(rgb(5, 0, 0, maxColorValue = 5), bg = TRUE)
bgneg4   <- crayon::make_style(rgb(4, 0, 0, maxColorValue = 5), bg = TRUE)
bgneg3   <- crayon::make_style(rgb(3, 0, 0, maxColorValue = 5), bg = TRUE)
bgneg2   <- crayon::make_style(rgb(2, 0, 0, maxColorValue = 5), bg = TRUE)
bgneg1   <- crayon::make_style(rgb(1, 0, 0, maxColorValue = 5), bg = TRUE)

bgposb5  <- crayon::make_style(rgb(0, 0, 5, maxColorValue = 5), bg = TRUE) #hcl(120, 200, 100)
bgposb4  <- crayon::make_style(rgb(0, 0, 4, maxColorValue = 5), bg = TRUE)
bgposb3  <- crayon::make_style(rgb(0, 0, 3, maxColorValue = 5), bg = TRUE)
bgposb2  <- crayon::make_style(rgb(0, 0, 2, maxColorValue = 5), bg = TRUE)
bgposb1  <- crayon::make_style(rgb(0, 0, 1, maxColorValue = 5), bg = TRUE)

bgnegb5  <- crayon::make_style(rgb(5, 0, 0, maxColorValue = 5), bg = TRUE)
bgnegb4  <- crayon::make_style(rgb(4, 0, 0, maxColorValue = 5), bg = TRUE)
bgnegb3  <- crayon::make_style(rgb(3, 0, 0, maxColorValue = 5), bg = TRUE)
bgnegb2  <- crayon::make_style(rgb(2, 0, 0, maxColorValue = 5), bg = TRUE)
bgnegb1  <- crayon::make_style(rgb(1, 0, 0, maxColorValue = 5), bg = TRUE)

# cat("\n",
#     bgpos1("42%"  ), bgneg1("42%\n" ),
#     bgpos2("42%"  ), bgneg2("42%\n" ),
#     bgpos3("42%"  ), bgneg3("42%\n" ),
#     bgpos4("42%"  ), bgneg4("42%\n" ),
#     bgpos5("42%"  ), bgneg5("42%\n" ),
#     "\n",
#     bgposb1("42%" ), bgnegb1("42%\n" ),
#     bgposb2("42%" ), bgnegb2("42%\n" ),
#     bgposb3("42%" ), bgnegb3("42%\n" ),
#     bgposb4("42%" ), bgnegb4("42%\n" ),
#     bgposb5("42%" ), bgnegb5("42%\n" ),
#     "\n",
#     "42%"       ,       "42%\n" ,
#     fmtgrey4("42%"), fmtgrey4("42%\n"),
#     fmtgrey3("42%"), fmtgrey3("42%\n"),
#     fmtgrey2("42%"), fmtgrey2("42%\n"),
#     fmtgrey1("42%"), fmtgrey1("42%\n") )




# pos4  <- crayon::make_style(hsv(122/360, 1   , 1  ),
#                             colors = crayon::num_colors(forget = TRUE)) #hcl(120, 200, 100)
# pos3  <- crayon::make_style(hsv(122/360, 0.75, 1  ))
# pos2  <- crayon::make_style(hsv(122/360, 0.50, 1  ))
# pos1  <- crayon::make_style(hsv(122/360, 0.20, 0.8))
# #pos5  <- crayon::make_style(hsv(122/360, 0.20, 0.8))
#
# neg4  <- crayon::make_style(hsv( 27/360, 1   , 1  ))
# neg3  <- crayon::make_style(hsv( 27/360, 0.75, 1  ))
# neg2  <- crayon::make_style(hsv( 27/360, 0.50, 1  ))
# neg1  <- crayon::make_style(hsv( 27/360, 0.20, 0.8))
# #neg5  <- crayon::make_style(hsv( 27/360, 0.2, 0.8))
#
# fmtgrey3 <- crayon::make_style(grey(0.9), grey = TRUE)
# fmtgrey2 <- crayon::make_style(grey(0.6), grey = TRUE)
# fmtgrey1 <- crayon::make_style(grey(0.4), grey = TRUE)
#
#
# posb4  <- crayon::make_style(hsv(210/360, 1    , 1  )) #hcl(120, 200, 100)
# posb3  <- crayon::make_style(hsv(210/360, 0.755, 1  ))
# posb2  <- crayon::make_style(hsv(210/360, 0.550, 1  ))
# posb1  <- crayon::make_style(hsv(210/360, 0.355, 0.8))
# #posb5  <- crayon::make_style(hsv(210/360, 0.2, 0.8))
#
# negb4  <- crayon::make_style(hsv(0      , 1   , 1  ))
# negb3  <- crayon::make_style(hsv(0      , 0.75, 1  ))
# negb2  <- crayon::make_style(hsv(0      , 0.55, 1  ))
# negb1  <- crayon::make_style(hsv(0      , 0.35, 0.8))
# #negb5  <- crayon::make_style(hsv(0      , 0.2, 0.8))
#
# # cat("\n",
# #     pos1("42%" ), neg1("42%\n" ),
# #     pos2("42%" ), neg2("42%\n" ),
# #     pos3("42%" ), neg3("42%\n" ),
# #     pos4("42%" ), neg4("42%\n" ),
# #     #pos5("42%" ), neg5("42%\n" ),
# #     "\n",
# #     posb1("42%"), negb1("42%\n" ),
# #     posb2("42%" ), negb2("42%\n" ),
# #     posb3("42%" ), negb3("42%\n" ),
# #     posb4("42%" ), negb4("42%\n" ),
# #     #posb5("42%" ), negb5("42%\n" ),
# #     "\n",
# #     "42%"       ,       "42%\n" ,
# #     fmtgrey1("42%"), fmtgrey1("42%\n"),
# #     fmtgrey2("42%"), fmtgrey2("42%\n"),
# #     fmtgrey3("42%"), fmtgrey3("42%\n") )
#
# # crayon::show_ansi_colors()



# #Background colors
# bgpos4   <- crayon::make_style(hsv(122/360, 1   , 1  ), bg = TRUE)
# bgpos3   <- crayon::make_style(hsv(122/360, 0.7 , 0.7), bg = TRUE)
# bgpos2   <- crayon::make_style(hsv(122/360, 0.50, 0.5), bg = TRUE)
# bgpos1   <- crayon::make_style(hsv(122/360, 0.50, 0.3), bg = TRUE)
# bgpos5   <- crayon::make_style(hsv(122/360, 0.20, 0.8), bg = TRUE)
#
# bgneg4   <- crayon::make_style(hsv( 27/360, 1   , 1  ), bg = TRUE)
# bgneg3   <- crayon::make_style(hsv( 27/360, 0.7 , 1  ), bg = TRUE)
# bgneg2   <- crayon::make_style(hsv( 27/360, 0.50, 0.6), bg = TRUE)
# bgneg1   <- crayon::make_style(hsv( 27/360, 0.50, 0.3), bg = TRUE)
# bgneg5   <- crayon::make_style(hsv( 27/360, 0.2 , 0.8), bg = TRUE)
#
# bgposb4  <- crayon::make_style(hsv(210/360, 1   , 1  ), bg = TRUE)
# bgposb3  <- crayon::make_style(hsv(210/360, 0.7 , 1  ), bg = TRUE)
# bgposb2  <- crayon::make_style(hsv(210/360, 0.50, 0.6), bg = TRUE)
# bgposb1  <- crayon::make_style(hsv(210/360, 0.50, 0.3), bg = TRUE)
# bgposb5  <- crayon::make_style(hsv(210/360, 0.30 , 0.8), bg = TRUE)
#
# bgnegb4  <- crayon::make_style(hsv(0      , 1   , 1  ), bg = TRUE)
# bgnegb3  <- crayon::make_style(hsv(0      , 0.7 , 1  ), bg = TRUE)
# bgnegb2  <- crayon::make_style(hsv(0      , 0.50, 0.6), bg = TRUE)
# bgnegb1  <- crayon::make_style(hsv(0      , 0.50, 0.3), bg = TRUE)
# bgnegb5  <- crayon::make_style(hsv(0      , 0.2  , 0.8), bg = TRUE)
#
#
# # cat("\n",
# #     bgpos1("42%"  ), bgneg1("42%\n"  ),
# #     bgpos2("42%"  ), bgneg2("42%\n"  ),
# #     bgpos3("42%"  ), bgneg3("42%\n"  ),
# #     bgpos4("42%"  ), bgneg4("42%\n"  ),
# #     #bgpos5$black("42%"  ), bgneg5$black("42%\n"  ),
# #     "\n",
# #     bgposb1("42%" ), bgnegb1("42%\n" ),
# #     bgposb2("42%" ), bgnegb2("42%\n" ),
# #     bgposb3("42%" ), bgnegb3("42%\n" ),
# #     bgposb4("42%" ), bgnegb4("42%\n" ),
# #     #bgposb5$black("42%" ), bgnegb5$black("42%\n" ),
# #     "\n",
# #     "42%"        ,       "42%\n"  ,
# #     fmtgrey1("42%") , fmtgrey1("42%\n" ),
# #     fmtgrey2("42%") , fmtgrey2("42%\n" ),
# #     fmtgrey3("42%") , fmtgrey3("42%\n" ) )

green_red <-
  c(pos1 = "pos1", pos2 = "pos2", pos3 = "pos3", pos4 = "pos4", pos5 = "pos5",
    neg1 = "neg1", neg2 = "neg2", neg3 = "neg3", neg4 = "neg4", neg5 = "neg5" )
blue_red <-
  c(pos1 = "posb1", pos2 = "posb2", pos3 = "posb3", pos4 = "posb4", pos5 = "posb5",
    neg1 = "negb1", neg2 = "negb2", neg3 = "negb3", neg4 = "negb4", neg5 = "negb5" )
green_red_bg <-
  c(pos1 = "bgpos1", pos2 = "bgpos2", pos3 = "bgpos3", pos4 = "bgpos4", pos5 = "bgpos5",
    neg1 = "bgneg1", neg2 = "bgneg2", neg3 = "bgneg3", neg4 = "bgneg4", neg5 = "bgneg5" )
blue_red_bg <-
  c(pos1 = "bgposb1", pos2 = "bgposb2", pos3 = "bgposb3", pos4 = "bgposb4", pos5 = "bgposb5",
    neg1 = "bgnegb1", neg2 = "bgnegb2", neg3 = "bgnegb3", neg4 = "bgnegb4", neg5 = "bgnegb5"  )
greys <-
  c(pos1 = "fmtgrey1", pos2 = "fmtgrey2", pos3 = "fmtgrey3", pos4 = "white")


#' Define the color style used to print tibbles of class \code{\link{tab}}
#' @describeIn tab_many define the color style used to print tibbles of class \code{\link{tab}}
#' @param styles The style chosen in \code{tab_color_style()}, among \code{"green_red"},
#'  \code{"blue_red"}, \code{"green_red_bg"} and \code{"blue_red_bg"}.
#'
#' @return
#' @export
#'
#' @examples #
#' # To change the overall color style used to print all \code{\link{tab}} :
#'  \dontrun{tab_color_style("blue_red_bg")}
tab_color_style <-
  function(styles = c("green_red", "blue_red",
                      "green_red_bg", "blue_red_bg")
  ) {
    assign("color_styles_all", rlang::eval_tidy(rlang::sym(styles[1])),
           pos = "package:tabxplor" #globalenv() #
    )
  }
color_styles_all <- green_red

#Color breaks for printing fmt in tabs ------------------------------------------------

#calculate pct breaks based on the number of levels ? ----

pct_breaks      <- c(0.05, 0.1, 0.2, 0.3)
mean_breaks     <- c(1.15, 1.5, 2, 4)
contrib_breaks  <- c(1, 2, 5, 10)

pct_ci_breaks   <- pct_breaks - pct_breaks[1]
mean_ci_breaks  <- mean_breaks / mean_breaks[1]

pct_brksup      <- c(pct_breaks    [2:length(pct_breaks)    ], Inf)
mean_brksup     <- c(mean_breaks   [2:length(mean_breaks)   ], Inf)
contrib_brksup  <- c(contrib_breaks[2:length(contrib_breaks)], Inf)
pct_ci_brksup   <- c(pct_ci_breaks [2:length(pct_ci_breaks) ], Inf)
mean_ci_brksup  <- c(mean_ci_breaks[2:length(mean_ci_breaks)], Inf)

pct_brk         <- pct_breaks     %>% c(., -.)
mean_brk        <- mean_breaks    %>% c(., 1/.)
contrib_brk     <- contrib_breaks %>% c(., -.)
pct_ci_brk      <- pct_ci_breaks  %>% c(., -.)
mean_ci_brk     <- mean_ci_breaks %>% c(., -.) #then - again

pct_brksup      <- pct_brksup     %>% c(., -.)
mean_brksup     <- mean_brksup    %>% c(., 1/.)
contrib_brksup  <- contrib_brksup %>% c(., -.)
pct_ci_brksup   <- pct_ci_brksup  %>% c(., -.)
mean_ci_brksup  <- mean_ci_brksup %>% c(., -.) #then - again

#' Get the breaks currently used to print colors
#' @describeIn tab_many get the breaks currently used to print colors
#' @param brk When missing, return all color breaks. Specify to return a given color
#' break, among \code{"pct"}, \code{"mean"}, \code{"contrib"}, \code{"pct_ci"} and
#' \code{"mean_ci"}.
#'
#' @export
get_color_breaks <- function(brk) {
  if (missing(brk)) {
    return(
      list(pct_breaks = pct_breaks, mean_breaks = mean_breaks,
           contrib_breaks = contrib_breaks, pct_ci_breaks = pct_ci_breaks,
           mean_ci_breaks = mean_ci_breaks)
    )
  }
  switch (brk,
          "pct"     = pct_breaks    ,
          "mean"    = mean_breaks   ,
          "contrib" = contrib_breaks,
          "pct_ci"  = pct_ci_breaks ,
          "mean_ci" = mean_ci_breaks
  )
}

#' Set the breaks used to print colors
#' @describeIn tab_many set the breaks used to print colors
#' @description Only breaks for attractions/surrepresentations (in green) should be given,
#' as a vector of positive doubles, with length between 1 and 5.
#' Breaks for repulsions/subrepresentations (in orange/red) will simply be the opposite.
#' @param pct_breaks If they are to be changed, the breaks used for percentages.
#' Default to \code{c(0.05, 0.1, 0.2, 0.3)} : first color used when the pct of a cell
#' is +5% superior to the pct of the related total ; second color used when
#' it is +10% superior ; third +20% superior ; fourth +30% superior.
#' The opposite for cells inferior to the total.
#' With \code{color = "after_ci"}, the first break is substracted from all breaks
#' (default becomes \code{c(0, 0.05, 0.15, 0.25)} : +0%, +5%, +15%, +25%).
#' @param mean_breaks If they are to be changed, the breaks used for means.
#' Default to \code{c(1.15, 1.5, 2, 4)} : first color used when the mean of a cell
#' is superior to 1.15 times the mean of the related total row ; second color
#' used when it is superior to 1.5 times ; etc.
#' The opposite for cells inferior to the total.
#' With \code{color = "after_ci"}, the first break is divided from all breaks
#' (default becomes \code{c(1, 1.3, 1.7, 3.5)}).
#' @param contrib_breaks If they are to be changed, the breaks used for contributions to
#' variance. Default to \code{c(1, 2, 5, 10)} : first color used when the contribution of
#' a cell is superior to the mean contribution ; second color used when it is superior to
#' 2 times the mean contribution ; etc. The global color (for example green or
#' red/orange) is given by the sign of the spread.
#'
#' @export
set_color_breaks <- function(pct_breaks, mean_breaks, contrib_breaks) {

  if (!missing(pct_breaks)) {
    stopifnot(is.numeric(pct_breaks)     ,
              length(pct_breaks)     <= 5,
              all(pct_breaks         >= 0))
    pct_ci_breaks   <- pct_breaks - pct_breaks[1]
    pct_brk         <- pct_breaks     %>% c(., -.)
    pct_brksup      <- c(pct_breaks   [2:length(pct_breaks)    ], Inf)
    pct_brksup      <- pct_brksup     %>% c(., -.)
    pct_ci_brk      <- pct_ci_breaks  %>% c(., -.)
    pct_ci_brksup   <- c(pct_ci_breaks[2:length(pct_ci_breaks) ], Inf)
    pct_ci_brksup   <- pct_ci_brksup  %>% c(., -.)
    assign("pct_breaks"   , pct_breaks   , pos = "package:tabxplor")
    assign("pct_ci_breaks", pct_ci_breaks, pos = "package:tabxplor")
    assign("pct_brk"      , pct_brk      , pos = "package:tabxplor")
    assign("pct_brksup"   , pct_brksup   , pos = "package:tabxplor")
    assign("pct_ci_brk"   , pct_ci_brk   , pos = "package:tabxplor")
    assign("pct_ci_brksup", pct_ci_brksup, pos = "package:tabxplor")
  }

  if (!missing(mean_breaks)) {
    stopifnot(is.numeric(mean_breaks)    ,
              length(mean_breaks)    <= 5,
              all(mean_breaks        >= 0))
    mean_ci_breaks  <- mean_breaks / mean_breaks[1]
    mean_brk        <- mean_breaks    %>% c(., 1/.)
    mean_brksup     <- c(mean_breaks   [2:length(mean_breaks)   ], Inf)
    mean_brksup     <- mean_brksup    %>% c(., 1/.)
    mean_ci_brk     <- mean_ci_breaks %>% c(., -.) #then - again
    mean_ci_brksup  <- c(mean_ci_breaks[2:length(mean_ci_breaks)], Inf)
    mean_ci_brksup  <- mean_ci_brksup %>% c(., -.) #then - again
    assign("mean_breaks"   , mean_breaks   , pos = "package:tabxplor")
    assign("mean_ci_breaks", mean_ci_breaks, pos = "package:tabxplor")
    assign("mean_brk"      , mean_brk      , pos = "package:tabxplor")
    assign("mean_brksup"   , mean_brksup   , pos = "package:tabxplor")
    assign("mean_ci_brk"   , mean_ci_brk   , pos = "package:tabxplor")
    assign("mean_ci_brksup", mean_ci_brksup, pos = "package:tabxplor")
  }


  if (!missing(contrib_breaks)) {
    stopifnot(is.numeric(contrib_breaks) ,
              length(contrib_breaks) <= 5,
              all(contrib_breaks     >= 0))
    contrib_brk     <- contrib_breaks %>% c(., -.)
    contrib_brksup  <- c(contrib_breaks[2:length(contrib_breaks)], Inf)
    contrib_brksup  <- contrib_brksup %>% c(., -.)
    assign("contrib_breaks", contrib_breaks, pos = "package:tabxplor")
    assign("contrib_brk"   , contrib_brk   , pos = "package:tabxplor")
    assign("contrib_brksup", contrib_brksup, pos = "package:tabxplor")
  }
}

#' @keywords internal
get_full_color_breaks <- function() {
  list(pct_brk        = pct_brk       ,
       pct_brksup     = pct_brksup    ,
       pct_ci_brk     = pct_ci_brk    ,
       pct_ci_brksup  = pct_ci_brksup ,
       mean_brk       = mean_brk      ,
       mean_brksup    = mean_brksup   ,
       mean_ci_brk    = mean_ci_brk   ,
       mean_ci_brksup = mean_ci_brksup,
       contrib_brk    = contrib_brk   ,
       contrib_brksup = contrib_brksup )
}

# get_color_breaks()
#
# set_color_breaks(pct_breaks = c(0.05, 0.10, 0.15, 0.25, 0.35))

# get_full_color_breaks()

# pct_breaks     = c(0.05, 0.10, 0.15, 0.25, 0.35)
# mean_breaks    = c(1.15, 1.25, 1.5 , 2   , 4   )
# contrib_breaks = c(0.5 , 1   , 2   , 5   , 10  )







# Tests -----
# new_tab() %>% get_chi2()
# new_tab() %>% get_total_table()
# new_tab() %>% get_subtext()

# vec_ptype2(new_tab(), new_tab()) %>% attributes()
#
# vec_rbind(red, red)
# vec_rbind(green, green)
# vec_rbind(green, red)
#
# vec_rbind(red, tibble::tibble(x = 10:12))
# vec_rbind(red, data.frame(x = 10:12))



# vctrs documentation --------------------------------------------------------------------

# howto-faq-coercion-data-frame
# FAQ - How to implement ptype2 and cast methods? (Data frames)
# Description
# This guide provides a practical recipe for implementing vec_ptype2() and vec_cast() methods
# for coercions of data frame subclasses. Related topics:
#   • For an overview of the coercion mechanism in vctrs, see ?theory-faq-coercion.
# • For an example of implementing coercion methods for simple vectors, see ?howto-faq-coercion.
# Coercion of data frames occurs when different data frame classes are combined in some way. The
# two main methods of combination are currently row-binding with vec_rbind() and col-binding
# with vec_cbind() (which are in turn used by a number of dplyr and tidyr functions). These functions
# take multiple data frame inputs and automatically coerce them to their common type.
# vctrs is generally strict about the kind of automatic coercions that are performed when combining
# inputs. In the case of data frames we have decided to be a bit less strict for convenience. Instead of
# throwing an incompatible type error, we fall back to a base data frame or a tibble if we don’t know
# how to combine two data frame subclasses. It is still a good idea to specify the proper coercion
# behaviour for your data frame subclasses as soon as possible.
# We will see two examples in this guide. The first example is about a data frame subclass that has
# no particular attributes to manage. In the second example, we implement coercion methods for a
# tibble subclass that includes potentially incompatible attributes.

# Roxygen workflow:
#   To implement methods for generics, first import the generics in your namespace and redocument:
#   #' @importFrom vctrs vec_ptype2 vec_cast
#   NULL
# Note that for each batches of methods that you add to your package, you need to export the
# methods and redocument immediately, even during development. Otherwise they won’t be in
# scope when you run unit tests e.g. with testthat.
# Implementing double dispatch methods is very similar to implementing regular S3 methods. In
# these examples we are using roxygen2 tags to register the methods, but you can also register the
# methods manually in your NAMESPACE file or lazily with s3_register().

# Parent methods:
#   Most of the common type determination should be performed by the parent class. In vctrs, double
# dispatch is implemented in such a way that you need to call the methods for the parent class manually.
# For vec_ptype2() this means you need to call df_ptype2() (for data frame subclasses) or
# tib_ptype2() (for tibble subclasses). Similarly, df_cast() and tib_cast() are the workhorses
# for vec_cast() methods of subtypes of data.frame and tbl_df. These functions take the union
# of the columns in x and y, and ensure shared columns have the same type.
# These functions are much less strict than vec_ptype2() and vec_cast() as they accept any
# subclass of data frame as input. They always return a data.frame or a tbl_df. You will probably
# want to write similar functions for your subclass to avoid repetition in your code. You may want
# to export them as well if you are expecting other people to derive from your class.

# A data.table example:
# […]

# #A tibble example:
# #  In this example we implement coercion methods for a tibble subclass that carries a colour as a
# #scalar metadata:
#
#   # User constructor
#   my_tibble <- function(colour = NULL, ...) {
#     new_my_tibble(tibble::tibble(...), colour = colour)
#   }
# # Developer constructor
# new_my_tibble <- function(x, colour = NULL) {
#   stopifnot(is.data.frame(x))
#   tibble::new_tibble(
#     x,
#     colour = colour,
#     class = "my_tibble",
#     nrow = nrow(x)
#   )
# }
# df_colour <- function(x) {
#   if (inherits(x, "my_tibble")) {
#     attr(x, "colour")
#   } else {
#     NULL
#   }
# }
# #'@export
# print.my_tibble <- function(x, ...) {
#   cat(sprintf("<%s: %s>\n", class(x)[[1]], df_colour(x)))
#   cli::cat_line(format(x)[-1])
# }
# #This subclass is very simple. All it does is modify the header.
# red <- my_tibble("red", x = 1, y = 1:2)
# red
# #> <my_tibble: red>
# #> x y
# #> <dbl> <int>
# #> 1 1 1
# #> 2 1 2
# red[2]
# #> <my_tibble: red>
# #> y
# #> <int>
# #> 1 1
# #> 2 2
# green <- my_tibble("green", z = TRUE)
# green
# #> <my_tibble: green>
# #> z
#
# #> <lgl>
# #> 1 TRUE
# #Combinations do not work properly out of the box, instead vctrs falls back to a bare tibble:
#   vec_rbind(red, tibble::tibble(x = 10:12))
# #> # A tibble: 5 x 2
# #> x y
# #> <dbl> <int>
# #> 1 1 1
# #> 2 1 2
# #> 3 10 NA
# #> 4 11 NA
# #> 5 12 NA
# # Instead of falling back to a data frame, we would like to return a <my_tibble> when combined
# # with a data frame or a tibble. Because this subclass has more metadata than normal data frames
# # (it has a colour), it is a supertype of tibble and data frame, i.e. it is the richer type. This is similar
# # to how a grouped tibble is a more general type than a tibble or a data frame. Conceptually, the
# # latter are pinned to a single constant group.
