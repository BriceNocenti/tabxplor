# Special tibble class needed for printing, even if the most meaningful attributes
#  where passed to fmt class variables (only chi2 and subtext remains at tab level) :
#  the implementation relies on "grouped_df" class structure, and to manage it, it is
#  necessary to add one method for class "tabxplor_grouped_tab" for each dplyr function...
#  (Thank to Giulia Pais, Davis Vaughan and Hadley Wickham,
#   https://github.com/tidyverse/dplyr/issues/5480).

# grouped_tab class still don't handle [] ----

# Problem with methods for dplyr::filter, because it replaces base::filter,
# which cannot be detached in namespace

# #Import dplyr in NAMESPACE :
# # dplyr is imported as a "Depends" package, otherwise dplyr::filter, needed for methods,
# # cannot be found by roxygen2 because it replaces base::filter.
#
# #' Internal dplyr methods
# #' @rawNamespace import(dplyr, except = data_frame)
# #  otherwise, conflict with vctrs. Thanks to Thomas :
# #  https://stackoverflow.com/questions/51899220/import-all-the-functions-of-a-package-except-one-when-building-a-package
# #' @keywords internal
# #' @name tabxplor-dplyr
# NULL

# #' To allow dplyr::filter to be used for methods
# #' @rawNamespace import(base, except = filter)
# #' @keywords internal
# #' @name no_base_filter
# NULL


# Create class tabxplor_tab --------------------------------------------------------------
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

#' A constructor for class tabxplor_tab
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
#' @return A \code{tibble} of class \code{tabxplor_tab}.
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
                       nrow = nrow(tabs), class = c(class, "tabxplor_tab"))
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
    class <- c(class, c("tabxplor_grouped_tab", "grouped_df"))

    new_tab(tabs, groups = groups,
            subtext = subtext, chi2 = chi2,
            ...,
            class = class)
  }



# Functions to work with class tabxplor_tab ----------------------------------------------

# Useful test fonction :
#' @describeIn tab_many a test function for class tabxplor_tab
#' @param x A object to test with \code{\link{is_tab}}.
#' @export
is_tab <- function(x) {
  inherits(x, "tabxplor_tab")
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
    `class<-`(tabs, class(tabs) %>% purrr::discard(. == "tabxplor_tab"))
  } else {
    `class<-`(tabs, class(tabs) %>%
                purrr::discard(. %in% c("tabxplor_grouped_tab", "tabxplor_tab")))
  }
}


#Methods to print class tabxplor_tab -----------------------------------------------------

#' @export
#' @method print tabxplor_tab
print.tabxplor_tab <- function(x, width = NULL, ..., n = 100, max_extra_cols = NULL,
                      max_footer_lines = NULL, min_row_var = 30) {
  print_chi2(x, width = width)

  # Use pillar::char() on row_var to control truncation
  row_var   <- tab_get_vars(x)$row_var
  n_row_var <- which(names(x) == row_var)

  out <- dplyr::mutate(x, dplyr::across(
    tidyselect::all_of(row_var),
    ~ pillar::char(as.character(.), min_chars = min_row_var)
  ))

  # out <- format(out, width = NULL)
  out <- format(out, width = width, ..., n = n, max_extra_cols = max_extra_cols,
                max_footer_lines = max_footer_lines)

  # very bad workaround to retransform the <char> type into <fct>
  if (length(n_row_var) != 0) {
    regular_ex <-
      paste0("^(", paste0(rep("[^<]+<", n_row_var), collapse = ""), ")<char>") %>%
      stringr::str_replace("<\\)<", ")<")

    out[3] <- out[3] %>% stringr::str_replace(regular_ex, "\\1<fct> ")
  }
  writeLines(out)

  # writeLines(format(x, width = width, ..., n = n, max_extra_cols = max_extra_cols,
  #                   max_footer_lines = max_footer_lines))
  invisible(x)
}

#' @export
#' @method print tabxplor_grouped_tab
print.tabxplor_grouped_tab <- function(x, width = NULL, ..., n = 100,
                                       max_extra_cols = NULL,max_footer_lines = NULL,
                                       min_row_var = 30) {
  print_chi2(x, width = width)

  # Use pillar::char() on row_var to control truncation
  row_var   <- tab_get_vars(x)$row_var
  n_row_var <- which(names(x) == row_var)

  out <- dplyr::mutate(x, dplyr::across(
    tidyselect::all_of(row_var),
    ~ pillar::char(as.character(.), min_chars = min_row_var)
  ))

  # out <- format(out, width = NULL)
  out <- format(out, width = width, ..., n = n, max_extra_cols = max_extra_cols,
                max_footer_lines = max_footer_lines)

  # very bad workaround to retransform the <char> type into <fct>
  if (length(n_row_var) != 0) {
    regular_ex <-
      paste0("^(", paste0(rep("[^<]+<", n_row_var), collapse = ""), ")<char>") %>%
      stringr::str_replace("<\\)<", ")<")

    out[4] <- out[4] %>% stringr::str_replace(regular_ex, "\\1<fct> ")
  }
  writeLines(out)

  # writeLines(format(x, width = width, ..., n = n, max_extra_cols = max_extra_cols,
  #                   max_footer_lines = max_footer_lines))
  invisible(x)
}

#' @keywords internal
print_chi2 <- function(x, width = NULL) {
  chi2 <- get_chi2(x)
  if (is.null(chi2)) return(NULL)
  if (nrow(chi2) == 0) return(NULL)
  # if (is.na(chi2)) return(NULL)

  chi2 <- chi2 %>% dplyr::select(-.data$row_var) %>%
    dplyr::filter(!.data$`chi2 stats` %in% c("cells"))

  fmt_cols <- purrr::map_lgl(chi2, is_fmt) %>% purrr::keep(. == TRUE) %>%
    names() #%>% rlang::syms()
  if (length(fmt_cols) != 0) {
    row_all_na <- chi2 %>%
      dplyr::select(where(is_fmt)) %>%
      purrr::map_df(is.na)
    row_all_na <- row_all_na %>%
      dplyr::rowwise() %>%
      dplyr::mutate(empty = all(dplyr::c_across())) %>%
      dplyr::pull(.data$empty)

    chi2 <- chi2 %>% dplyr::filter(!row_all_na)
  }

  chi2 <- chi2 %>%
    dplyr::mutate(dplyr::across(where(is_fmt),
                                ~ `class<-`(., c("tab_chi2_fmt", class(.)))  ))

  nrow_chi2 <- nrow(chi2)
  if (nrow_chi2 == 0) return(NULL)

  ind <- chi2 %>% dplyr::group_by(dplyr::across(where(is.factor))) %>%
    dplyr::group_indices()
  ind   <- c(TRUE, ind != dplyr::lead(ind, default = max(ind) + 1) )

  chi2 <- chi2 %>%
    dplyr::mutate(dplyr::across(
      where(is.factor),
      ~ dplyr::if_else(. == dplyr::lag(., default = paste0(as.character(.[1]), "a")),
                       true = stringi::stri_unescape_unicode("\\u00a0"),
                       false = as.character(.))
      %>% as.factor()
    ))

  # setup <- pillar::tbl_format_setup(chi2, width = NULL)
  setup <- pillar::tbl_format_setup(chi2, width = width, n = Inf)
  body_no_type <- tbl_format_body(chi2, setup)[-2]
  body_no_type <- body_no_type %>%
    stringr::str_replace("`chi2 stats`", "chi2 stats  ") %>%
    crayon::col_substr(stringr::str_length(nrow_chi2) + 2L, crayon::col_nchar(.))
  body_no_type[ind] <- crayon::underline(body_no_type[ind] )
  body_no_type <- body_no_type %>% `class<-`("pillar_vertical")

  cli::cat_line(body_no_type)
  cli::cat_line()
}


# Change headers
#' @importFrom dplyr tbl_sum
#' @export
#' @method tbl_sum tabxplor_tab
tbl_sum.tabxplor_tab <- function(x, ...) {
  tbl_header <- NextMethod()
  names(tbl_header)[1] <- "A tabxplor tab"
  tbl_header
}
#' @export
#' @method tbl_sum tabxplor_grouped_tab
tbl_sum.tabxplor_grouped_tab <- function(x, ...) {
  grouped_tbl_header <- NextMethod()
  names(grouped_tbl_header)[1] <- "A tabxplor tab"
  grouped_tbl_header
}


# Change footer
#' @importFrom pillar tbl_format_footer
#' @export
#' @method tbl_format_footer tabxplor_tab
tbl_format_footer.tabxplor_tab <- function(x, setup, ...) {
  default_footer <- NextMethod()

  print_colors <- tab_color_legend(x)
  subtext <- get_subtext(x) %>% purrr::discard(. == "")
  if (length(print_colors) != 0) print_colors <- paste0(
    pillar::style_subtle("# "), print_colors
  )
  if (length(subtext) != 0) subtext <- pillar::style_subtle( paste0("# ", subtext) )

  c(default_footer, print_colors, subtext)
}


#Change body
#' @importFrom pillar tbl_format_body
#' @export
#' @method tbl_format_body tabxplor_tab
tbl_format_body.tabxplor_tab <- function(x, setup, ...) {
  default_body <- NextMethod()

  body_data  <- default_body[-(1:2)]
  ind   <- dplyr::group_indices(setup$x)[1:length(body_data)]
  ind   <- ind != dplyr::lag(ind, default = 1L)
  body_data <- body_data %>%
    purrr::map2(ind, function(.x, .y) if (.y) {c("", .x)} else {.x}) %>%
    purrr::flatten_chr()

  c(default_body[1:2], body_data) %>% `class<-`("pillar_vertical")
}




#Methods for class tabxplor_tab ----------------------------------------------------------

# importFrom not needed when tabxplor import dplyr as a "Depends" package
#' @importFrom dplyr group_by
#' @method group_by tabxplor_tab
#' @export
group_by.tabxplor_tab <- function(.data,
                         ...,
                         .add = FALSE,
                         .drop = dplyr::group_by_drop_default(.data)) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  new_grouped_tab(out, groups,
                  subtext = get_subtext(.data), chi2 = get_chi2(.data))
}


#' @importFrom dplyr rowwise
#' @method rowwise tabxplor_tab
#' @export
rowwise.tabxplor_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  out <- new_grouped_tab(out, groups,
                         subtext = get_subtext(.data), chi2 = get_chi2(.data))

  `class<-`(out, stringr::str_replace(class(out), "grouped_df", "rowwise_df"))
}




# (from vctrs documentation)
# The coercion methods for data frames operate in two steps:
# They check for compatible subclass attributes. In our case the tibble colour has to
# be the same, or be undefined.
# They call their parent methods, in this case tib_ptype2() and tib_cast() because we
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
#' @keywords internal
# @export
tab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- vctrs::tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  subtext     <- vctrs::vec_c(get_subtext(x), get_subtext(to)) %>% unique()
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]
  chi2        <- vctrs::vec_rbind(get_chi2(x), get_chi2(to))

  new_tab(out, subtext = subtext, chi2 = chi2)
}

#' @rdname tab_cast
#' @param y_arg Argument names for x and y. These are used in error messages to inform
#' the user about the locations of incompatible types.
#' @keywords internal
# @export
tab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  out <- vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  #colour <- df_colour(x) %||% df_colour(y)

  chi2        <- vctrs::vec_rbind(get_chi2(x), get_chi2(y))
  subtext     <- vctrs::vec_c(get_subtext(x), get_subtext(y)) %>% unique()
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]

  new_tab(out, subtext = subtext, chi2 = chi2)
}


#Let's now implement the coercion methods, starting with the self-self methods.
#' @export
vec_ptype2.tabxplor_tab.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_cast.tabxplor_tab.tabxplor_tab <- function(x, to, ...) {
  tab_cast(x, to, ...)
}

# The methods for combining our class with tibbles follow the same pattern.
# For ptype2 we return our class in both cases because it is the richer type
#' @export
vec_ptype2.tabxplor_tab.tbl_df <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.tbl_df.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_cast.tabxplor_tab.tbl_df <- function(x, to, ...) {
  tab_cast(x, to, ...)
}
#' @export
vec_cast.tbl_df.tabxplor_tab <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

#' @export
vec_ptype2.tabxplor_tab.data.frame <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @export
vec_cast.tabxplor_tab.data.frame <- function(x, to, ...) {
  tab_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.tabxplor_tab <- function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}




#Methods for class grouped_tab------------------------------------------------------------

# just modify the methodes currently used by dplyr class "grouped_df" (not relative to groups)
# .S3methods(class = "grouped_df")

# dplyr_col_modify      dplyr_reconstruct     dplyr_row_slice
# ungroup               distinct_        rename_     select_     summarise
# [                     [<-          [[<-
# cbind                 rbind  rowwise

#' @importFrom dplyr ungroup
#' @method ungroup tabxplor_grouped_tab
#' @export
ungroup.tabxplor_grouped_tab <- function (x, ...)
{
  if (missing(...)) {
    new_tab(x, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
  else {
    old_groups <- dplyr::group_vars(x)
    to_remove  <- tidyselect::vars_select(names(x), ...)
    new_groups <- setdiff(old_groups, to_remove)
    dplyr::group_by(x, !!!rlang::syms(new_groups))
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


#' @importFrom dplyr dplyr_row_slice
#' @method dplyr_row_slice tabxplor_grouped_tab
#' @export
dplyr_row_slice.tabxplor_grouped_tab <- function(data, i, ...) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(data), chi2 = get_chi2(data))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(data), chi2 = get_chi2(data))
  }
}
# dplyr:::dplyr_row_slice.grouped_df

#' @importFrom dplyr dplyr_col_modify
#' @method dplyr_col_modify tabxplor_grouped_tab
#' @export
dplyr_col_modify.tabxplor_grouped_tab <- function(data, cols) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(data), chi2 = get_chi2(data))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(data), chi2 = get_chi2(data))
  }
}
# dplyr:::dplyr_col_modify.grouped_df

#' @importFrom dplyr dplyr_reconstruct
#' @method dplyr_reconstruct tabxplor_grouped_tab
#' @export
dplyr_reconstruct.tabxplor_grouped_tab <- function(data, template) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(data), chi2 = get_chi2(data))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(data), chi2 = get_chi2(data))
  }
}
# dplyr:::dplyr_reconstruct.grouped_df

#' @method `[` tabxplor_grouped_tab
#' @export
`[.tabxplor_grouped_tab` <- function(x, i, j, drop = FALSE) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(x), chi2 = get_chi2(x))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
}
# dplyr:::`[.grouped_df`

#' @method `[<-` tabxplor_grouped_tab
#' @export
`[<-.tabxplor_grouped_tab` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(x), chi2 = get_chi2(x))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
}
# dplyr:::`[<-.grouped_df`

#' @method `[[<-` tabxplor_grouped_tab
#' @export
`[[<-.tabxplor_grouped_tab` <- function(x, ..., value) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(x), chi2 = get_chi2(x))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
}
# dplyr:::`[[<-.grouped_df`

#' @importFrom dplyr rowwise
#' @method rowwise tabxplor_grouped_tab
#' @export
rowwise.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)

  out <- new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  `class<-`(out, stringr::str_replace(class(out), "grouped_df", "rowwise_df"))

}

# #' @method rbind tabxplor_grouped_tab
# #' @export
# rbind.tabxplor_grouped_tab <- function(...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
# # dplyr:::rbind.grouped_df
#
# #' @method cbind tabxplor_grouped_tab
# #' @export
# cbind.tabxplor_grouped_tab <- function(...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
# # dplyr:::cbind.grouped_df

#' @importFrom dplyr summarise
#' @method summarise tabxplor_grouped_tab
#' @export
summarise.tabxplor_grouped_tab <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}


#' @importFrom dplyr select
#' @method select tabxplor_grouped_tab
#' @export
select.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

#' @importFrom dplyr rename
#' @method rename tabxplor_grouped_tab
#' @export
rename.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

#' @importFrom dplyr rename_with
#' @method rename_with tabxplor_grouped_tab
#' @export
rename_with.tabxplor_grouped_tab <- function(.data, .fn, .cols = dplyr::everything(), ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

# not for grouped_df
#' @importFrom dplyr relocate
#' @method relocate tabxplor_grouped_tab
#' @export
relocate.tabxplor_grouped_tab <- function(.data, ...) { #.before = NULL, .after = NULL
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
} # dplyr:::relocate.grouped_df


#' @importFrom dplyr distinct_
#' @method distinct_ tabxplor_grouped_tab
#' @export
distinct_.tabxplor_grouped_tab <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}
# dplyr:::distinct_.grouped_df



#
# # Past methods, not needed anymore :
#
# #' @importFrom dplyr mutate
# #' @method mutate tabxplor_grouped_tab
# #' @export
# mutate.tabxplor_grouped_tab <- function(.data, ...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
#
# }
#
#
#
#
# #' @importFrom dplyr transmute
# #' @method transmute tabxplor_grouped_tab
# #' @export
# transmute.tabxplor_grouped_tab <- function(.data, ...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
# #' @importFrom dplyr summarise
# #' @method summarise tabxplor_grouped_tab
# #' @export
# summarise.tabxplor_grouped_tab <- function(.data, ..., .groups = NULL) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
# #' @importFrom dplyr filter
# #' @method filter tabxplor_grouped_tab
# # @rdname filter
# #' @export
# filter.tabxplor_grouped_tab <- function(.data, ..., .preserve = FALSE) {
#   out <- NextMethod()
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     groups <- dplyr::group_data(out)
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
#
# # slice not working with grouped_tab ? ----
# #' @importFrom dplyr slice
# #' @method slice tabxplor_grouped_tab
# #' @export
# slice.tabxplor_grouped_tab <- function(.data, ..., .preserve = FALSE) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
# #' @importFrom dplyr arrange
# #' @method arrange tabxplor_grouped_tab
# #' @export
# arrange.tabxplor_grouped_tab <- function(.data, ..., .by_group = FALSE) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
#
#
# #' @importFrom dplyr select
# #' @method select tabxplor_grouped_tab
# #' @export
# select.tabxplor_grouped_tab <- function(.data, ...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
# #' @importFrom dplyr relocate
# #' @method relocate tabxplor_grouped_tab
# #' @export
# relocate.tabxplor_grouped_tab <- function(.data, ..., .before = NULL, .after = NULL) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
# #' @importFrom dplyr rename
# #' @method rename tabxplor_grouped_tab
# #' @export
# rename.tabxplor_grouped_tab <- function(.data, ...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
# #' @importFrom dplyr rename_with
# #' @method rename_with tabxplor_grouped_tab
# #' @export
# rename_with.tabxplor_grouped_tab <- function(.data, .fn,
#                                     .cols = dplyr::everything(), ...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
#
# #' @importFrom dplyr distinct
# #' @method distinct tabxplor_grouped_tab
# #' @export
# distinct.tabxplor_grouped_tab <- function(.data, ...,  .keep_all = FALSE) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }





#' @rdname tab_cast
#' @keywords internal
# @export
gtab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  #based upon vctrs:::gdf_cast()
  df <- vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  gdf <- dplyr::grouped_df(df, vars, drop = drop)

  groups <- dplyr::group_data(gdf)
  new_grouped_tab(gdf, groups, subtext = get_subtext(to), chi2 = get_chi2(to))
}

#' @rdname tab_cast
#' @keywords internal
# @export
gtab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  #based upon vctrs:::gdf_ptype2
  common <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
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
vec_ptype2.tabxplor_grouped_tab.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.tabxplor_grouped_tab.tabxplor_grouped_tab <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}

#grouped_tab / grouped_df
#' @export
vec_ptype2.tabxplor_grouped_tab.grouped_df <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.grouped_df.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.tabxplor_grouped_tab.grouped_df <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @export
vec_cast.grouped_df.tabxplor_grouped_tab <- function(x, to, ...) {
  #vctrs:::gdf_cast
  df <- vctrs::df_cast(x, to, ...)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  dplyr::grouped_df(df, vars, drop = drop)
}

#grouped_tab / tab
#' @export
vec_ptype2.tabxplor_grouped_tab.tabxplor_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.tabxplor_tab.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.tabxplor_grouped_tab.tabxplor_tab <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @export
vec_cast.tabxplor_tab.tabxplor_grouped_tab <- function(x, to, ...) {
  tab_cast(x, to, ...)
}

#grouped_tab / tbl_df
#' @export
vec_ptype2.tabxplor_grouped_tab.tbl_df <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.tbl_df.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.tabxplor_grouped_tab.tbl_df <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @export
vec_cast.tbl_df.tabxplor_grouped_tab <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

#grouped_tab / data.frame
#' @export
vec_ptype2.tabxplor_grouped_tab.data.frame <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @export
vec_cast.tabxplor_grouped_tab.data.frame <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.tabxplor_grouped_tab <- function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}





#Colors for printing fmt in tabs -------------------------------------------------------
pos5     <- crayon::make_style(pos5     = rgb(0, 5, 0, maxColorValue = 5), colors = 256) #hcl(120, 200, 100)
pos4     <- crayon::make_style(pos4     = rgb(1, 5, 1, maxColorValue = 5), colors = 256)
pos3     <- crayon::make_style(pos3     = rgb(3, 5, 1, maxColorValue = 5), colors = 256)
pos2     <- crayon::make_style(pos2     = rgb(4, 5, 1, maxColorValue = 5), colors = 256)
pos1     <- crayon::make_style(pos1     = rgb(4, 4, 1, maxColorValue = 5), colors = 256) #5, 5, 1

neg5     <- crayon::make_style(neg5     = rgb(5, 0, 0, maxColorValue = 5), colors = 256)
neg4     <- crayon::make_style(neg4     = rgb(5, 1, 0, maxColorValue = 5), colors = 256)
neg3     <- crayon::make_style(neg3     = rgb(5, 2, 1, maxColorValue = 5), colors = 256)
neg2     <- crayon::make_style(neg2     = rgb(5, 3, 1, maxColorValue = 5), colors = 256)
neg1     <- crayon::make_style(neg1     = rgb(4, 3, 2, maxColorValue = 5), colors = 256)

fmtgrey4 <- crayon::make_style(fmtgrey4 = grey(0.9), grey = TRUE, colors = 256)
fmtgrey3 <- crayon::make_style(fmtgrey3 = grey(0.7), grey = TRUE, colors = 256)
fmtgrey2 <- crayon::make_style(fmtgrey2 = grey(0.5), grey = TRUE, colors = 256)
fmtgrey1 <- crayon::make_style(fmtgrey1 = grey(0.3), grey = TRUE, colors = 256)

posb5    <- crayon::make_style(posb5    = rgb(0, 0, 5, maxColorValue = 5), colors = 256) #hcl(120, 200, 100)
posb4    <- crayon::make_style(posb4    = rgb(0, 1, 5, maxColorValue = 5), colors = 256)
posb3    <- crayon::make_style(posb3    = rgb(0, 3, 5, maxColorValue = 5), colors = 256)
posb2    <- crayon::make_style(posb2    = rgb(1, 4, 5, maxColorValue = 5), colors = 256)
posb1    <- crayon::make_style(posb1    = rgb(2, 5, 5, maxColorValue = 5), colors = 256)

negb5    <- crayon::make_style(negb5    = rgb(5, 0, 0, maxColorValue = 5), colors = 256)
negb4    <- crayon::make_style(negb4    = rgb(5, 0, 1, maxColorValue = 5), colors = 256)
negb3    <- crayon::make_style(negb3    = rgb(5, 1, 1, maxColorValue = 5), colors = 256)
negb2    <- crayon::make_style(negb2    = rgb(5, 1, 3, maxColorValue = 5), colors = 256)
negb1    <- crayon::make_style(negb1    = rgb(5, 2, 3, maxColorValue = 5), colors = 256)



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

bgpos5   <- crayon::make_style(bgpos5  = rgb(0, 5, 0, maxColorValue = 5), bg = TRUE, colors = 256) #hcl(120, 200, 100)
bgpos4   <- crayon::make_style(bgpos4  = rgb(0, 4, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgpos3   <- crayon::make_style(bgpos3  = rgb(0, 3, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgpos2   <- crayon::make_style(bgpos2  = rgb(0, 2, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgpos1   <- crayon::make_style(bgpos1  = rgb(0, 1, 0, maxColorValue = 5), bg = TRUE, colors = 256) #5, 5, 1

bgneg5   <- crayon::make_style(bgneg5  = rgb(5, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgneg4   <- crayon::make_style(bgneg4  = rgb(4, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgneg3   <- crayon::make_style(bgneg3  = rgb(3, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgneg2   <- crayon::make_style(bgneg2  = rgb(2, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgneg1   <- crayon::make_style(bgneg1  = rgb(1, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)

bgposb5  <- crayon::make_style(bgposb5 = rgb(0, 0, 5, maxColorValue = 5), bg = TRUE, colors = 256) #hcl(120, 200, 100)
bgposb4  <- crayon::make_style(bgposb4 = rgb(0, 0, 4, maxColorValue = 5), bg = TRUE, colors = 256)
bgposb3  <- crayon::make_style(bgposb3 = rgb(0, 0, 3, maxColorValue = 5), bg = TRUE, colors = 256)
bgposb2  <- crayon::make_style(bgposb2 = rgb(0, 0, 2, maxColorValue = 5), bg = TRUE, colors = 256)
bgposb1  <- crayon::make_style(bgposb1 = rgb(0, 0, 1, maxColorValue = 5), bg = TRUE, colors = 256)

bgnegb5  <- crayon::make_style(bgnegb5 = rgb(5, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgnegb4  <- crayon::make_style(bgnegb4 = rgb(4, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgnegb3  <- crayon::make_style(bgnegb3 = rgb(3, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgnegb2  <- crayon::make_style(bgnegb2 = rgb(2, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)
bgnegb1  <- crayon::make_style(bgnegb1 = rgb(1, 0, 0, maxColorValue = 5), bg = TRUE, colors = 256)

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
#' @description Only breaks for attractions/over-representations (in green) should be
#' given, as a vector of positive doubles, with length between 1 and 5.
#' Breaks for aversions/under-representations (in orange/red) will simply be the opposite.
#' @param pct_breaks If they are to be changed, the breaks used for percentages.
#' Default to \code{c(0.05, 0.1, 0.2, 0.3)} : first color used when the pct of a cell
#' is +5% superior to the pct of the related total ; second color used when
#' it is +10% superior ; third +20% superior ; fourth +30% superior.
#' The opposite for cells inferior to the total.
#' With \code{color = "after_ci"}, the first break is subtracted from all breaks
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
#  - For an overview of the coercion mechanism in vctrs, see ?theory-faq-coercion.
#  - For an example of implementing coercion methods for simple vectors, see ?howto-faq-coercion.
# Coercion of data frames occurs when different data frame classes are combined in some way. The
# two main methods of combination are currently row-binding with vec_rbind() and col-binding
# with vec_cbind() (which are in turn used by a number of dplyr and tidyr functions). These functions
# take multiple data frame inputs and automatically coerce them to their common type.
# vctrs is generally strict about the kind of automatic coercions that are performed when combining
# inputs. In the case of data frames we have decided to be a bit less strict for convenience. Instead of
# throwing an incompatible type error, we fall back to a base data frame or a tibble if we don't know
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
# methods and redocument immediately, even during development. Otherwise they won't be in
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

# A data.tabxplor_tab le example:
# [...]

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
