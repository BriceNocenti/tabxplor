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


#' Class tab
#'
#' @param tabs A table, stored into a \code{\link[tibble]{tibble}} data.frame.
#' It is generally made with \code{\link{tab}} or \code{\link{tab_multi}}.
#' @param chi2 A tibble storing information about pvalues and variances.
#' @param total_table TRUE when it is the total table of a \code{\link{tab}}
#' (list of tables).
#' @param subtext A character vector to print legend lines under the table
#' with \code{\link{tab_xl}}
#'
#' @return A table of class tab.
#' @export
#  @examples
new_tab <-
  function(tabs = tibble::tibble(), chi2 = tibble::tibble(pvalue = double(),
                                                          var    = double(),
                                                          count  = integer()),
           total_table = FALSE, subtext = "", ...) {
    stopifnot(is.data.frame(tabs))
    #vec_recycle(vec_cast(nrow, integer()), size = 1)
    stopifnot(is.data.frame(chi2))
    vec_assert(chi2, tibble::tibble(pvalue = double(),
                                    var    = double(),
                                    count  = integer()) )
    # vec_assert(chi2$pvalue, double())
    # vec_assert(chi2$var   , double())
    # vec_assert(chi2$count , integer())
    vec_assert(total_table, logical(), size = 1)
    vec_assert(subtext    , character())

    tibble::new_tibble(tabs, chi2 = chi2, total_table = total_table,
                       subtext = subtext,
                       nrow = nrow(tabs), class = "tab", ...) #... ?
  }
# tab <-
#   function(tabs = tibble::tibble(), chi2 = tibble::tibble(), total_table = FALSE,
#            subtext = "") {
#     tabs <- tibble::as_tibble(tabs)
#     chi2 <- tibble::as_tibble(chi2)
#     total_table <- as.logical(total_table[1])
#     subtext <- as.character(subtext)
#     new_tab(tabs, nrow = nrow(tabs), perc = perc, chi2 = chi2,
#                     total_table = total_table, subtext = subtext)
#   }

# Functions to work with class tab -------------------------------------------------------

# Useful test fonction :
#' @describeIn tab A test function for class tab
#' @export
is_tab <- function(x) {
  inherits(x, "tab")
}

get_chi2        <- purrr::attr_getter("chi2")
get_total_table <- purrr::attr_getter("total_table")
get_subtext     <- purrr::attr_getter("subtext")

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





#Methods for class tab -------------------------------------------------------------------

#' Print method for class tab
#'
#' @param x An object of class tab.
#' @param ... Arguments passed to print.default
#' @return The printed single table.
#' @export
print.tab <- function(x, ...) {
  # cat(sprintf("<%s: %s>\n", class(x)[[1]], df_colour(x)))
  # cli::cat_line(format(x)[-1])

  #cli::cat_line(format(x, ..., n = 30, width = 500)) #Can be use to color bg and text

  if (nrow(x) > 0 & ncol(x) > 0) {
    out <- x
    if (class(dplyr::pull(x, 1)) %in% c("factor", "character")) {
      # #First column must be same length for all tabs
      # # (for that must be a factor with the levels of all the other tables)
      # out <- dplyr::mutate_at(x, dplyr::vars(1), ~ as.factor(.))
      # max_length_all <- dplyr::pull(out, 1) %>% levels() %>% stringr::str_length() %>% max(na.rm = TRUE)
      # if (dplyr::pull(out, 1) %>% stringr::str_length() %>% max(na.rm = TRUE) < max_length_all) {
      #   out <- dplyr::mutate_at(out, dplyr::vars(1), ~ `levels<-`(., stringr::str_pad(levels(.), max_length_all - 2, "right")))
      # }
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
    cli::cat_line("# A tab: 0 x 0", col = "grey")
  }

  #invisible(x)
}

#Two possibility : by rows ; by cols ???
#Two type vectors : columns and rows

#Put informations on fmtn on printing ?
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


# (from vctrs documentation)
# The coercion methods for data frames operate in two steps:
#   • They check for compatible subclass attributes. In our case the tibble colour has to be the
# same, or be undefined.
# • They call their parent methods, in this case tib_ptype2() and tib_cast() because we
# have a subclass of tibble. This eventually calls the data frame methods df_ptype2() and
# tib_ptype2() which match the columns and their types.
#' @export
tab_cast <- function(x, to, total_table = FALSE, ..., x_arg = "", to_arg = "") {
out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

chi2        <- vec_rbind(get_chi2(x), get_chi2(to))
subtext     <- vec_c(get_subtext(x), get_subtext(to)) %>% unique()
if (length(subtext) > 1) subtext <- subtext[subtext != ""]

new_tab(out, chi2 = chi2, total_table = total_table, subtext = subtext)
}

#' @export
tab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  out <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  #colour <- df_colour(x) %||% df_colour(y)

  chi2        <- vec_rbind(get_chi2(x), get_chi2(y))
  subtext     <- vec_c(get_subtext(x), get_subtext(y)) %>% unique()
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]

  new_tab(out, chi2 = chi2, total_table = total_table, subtext = subtext)
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

# The methods for combining our class with tibbles follow the same pattern. For ptype2 we return
# our class in both cases because it is the richer type:
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
