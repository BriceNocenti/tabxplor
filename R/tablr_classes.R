# Special tibble class needed for printing, even if the most meaningful attributes
#  where passed to fmt class variables (only chi2 and subtext remains at tab level) :
#  my implementation relies on "grouped_df" class structure, and to manage it, it is
#  necessary to add one method for class "grouped_tab" for each dplyr function...
#  (Thank to Giulia Pais, Davis Vaughan and Hadley Wickham,
#   https://github.com/tidyverse/dplyr/issues/5480).

# grouped_tab class still don’t handle [] ----

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


#' A construction for class tab
#'
#' @param tabs A table, stored into a \code{\link[tibble]{tibble}} data.frame.
#' It is generally made with \code{\link{tab}} or \code{\link{tab_multi}}.
#' @param chi2 A tibble storing information about pvalues and variances.
#' @param subtext A character vector to print legend lines under the table
#' with \code{\link{tab_xl}}
#'
#' @return A table of class tab.
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
                       nrow = nrow(tabs), class = c(class, "tab")) #... ?
  }

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
#' @describeIn tab A test function for class tab
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
print.tab <- function(x, ...) {
  cli::cat_line(format(x, ..., n = 300)) #width = 500
}

#' @export
print.grouped_tab <- function(x, ...) {
  cli::cat_line(format(x, ..., n = 300)) #width = 500
}

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


#' @importFrom dplyr group_by
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

#' @importFrom dplyr ungroup
#' @method ungroup grouped_tab
#' @export



# (from vctrs documentation)
# The coercion methods for data frames operate in two steps:
#   • They check for compatible subclass attributes. In our case the tibble colour has to be the
# same, or be undefined.
# • They call their parent methods, in this case tib_ptype2() and tib_cast() because we
# have a subclass of tibble. This eventually calls the data frame methods df_ptype2() and
# tib_ptype2() which match the columns and their types.
#' @export
tab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  subtext     <- vec_c(get_subtext(x), get_subtext(to)) %>% unique()
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]
  chi2        <- vec_rbind(get_chi2(x), get_chi2(to))

  new_tab(out, subtext = subtext, chi2 = chi2)
}

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
ungroup.grouped_tab <- function (x, ...)
{
  if (missing(...)) {
    new_tab(x, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
  else {
    old_groups <- dplyr::group_vars(x)
    to_remove <- tidyselect::vars_select(names(x), ...)
    new_groups <- setdiff(old_groups, to_remove)
    dplyr::group_by(x, !!!syms(new_groups))
  }
}

lv1_group_vars <- function(tabs) {
  groupvars <- dplyr::group_vars(tabs)
  all(purrr::map_lgl(groupvars,
                 ~ nlevels(forcats::fct_drop(dplyr::pull(tabs, .))) == 1)) |
    length(groupvars) == 0
}

#' @importFrom dplyr mutate
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




#' @importFrom dplyr transmute
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

#' @importFrom dplyr summarise
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

#' @importFrom dplyr filter
#' @method filter grouped_tab
#' @export
filter.grouped_tab <- function(.data, ..., .preserve = FALSE) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# slice not working with grouped_tab ----
#' @importFrom dplyr slice
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

#' @importFrom dplyr arrange
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

#' @importFrom dplyr select
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

#' @importFrom dplyr relocate
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

#' @importFrom dplyr rename
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

#' @importFrom dplyr rename_with
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

#' @importFrom dplyr distinct
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
