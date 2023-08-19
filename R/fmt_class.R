# Create formated numbers class
#Import vctrs in NAMESPACE :
#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name tabxplor-vctrs
NULL


# binding for global variables not found by R cmd check
. = NULL
globalVariables(c(":=", ".SD", ".N"))


# EXPORTED FUNCTIONS TO WORK WITH CLASS FMT ##############################################


#' Create a vector of class formatted numbers
#' @description \code{fmt} vectors, of class \code{tabxplor_fmt}, powers \pkg{tabxplor}
#' and \code{\link{tab}} tibbles.
#' As a \code{\link[vctrs:new_rcrd]{record}}, they stores all data necessary to
#' calculate percentages, Chi2 metadata or confidence intervals, but also to format and
#' color the table to help the user read it. You can access this data with
#' \code{\link[vctrs:field]{vctrs::field}}, or change it with
#' \code{\link[vctrs:field]{vctrs:field<-}}. A \code{fmt} vector have 13 fields :
#' \code{n}, \code{digits}, \code{display}, \code{wn}, \code{pct}, \code{mean},
#' \code{diff}, \code{ctr}, \code{var}, \code{ci}, \code{in_totrow},  \code{in_tottab},
#' \code{in_refrow}. Other arguments are attributes, attached not to each value, but to
#' the whole vector, like \code{type}, \code{totcol} or \code{color}. You can get them
#' with \code{\link[base:attr]{attr}} and modify them with
#' \code{\link[base:attr]{attr<-}}. Special functions listed below are made to
#' facilitate programming with with \pkg{tabxplor} formatted numbers.
#' \code{taxplfmt} vectors can use all standard operations, like +, -, sum(), or c(),
#' using \pkg{vctrs}.
#'
#' @param n The underlying count, as an integer vector of length \code{n()}. It is used
#' to calculate confidence intervals.
#' @param type The type of the column, which defines the type of background calculation
#' to be made (as a single string, since it's not a field but an attribute) :
#' \itemize{
#'   \item \code{"n"}: counts
#'   \item \code{"mean"}: mean column (from numeric variables)
#'   \item \code{"row"}: row percentages
#'   \item \code{"col"}: column percentages
#'   \item \code{"all"}: frequencies by subtable/group (i.e. by \code{tab_vars})
#'   \item \code{"all_tabs"}: frequencies for the whole table
#' }
#' @param digits The number of digits, as an integer, or an integer vector the length
#' of \code{n}.
#' @param display The display type : the name of the field you want to show when printing
#' the vector. Among \code{"n"}, \code{"wn"}, \code{"pct"}, \code{"diff"}, \code{"ctr"},
#'  \code{"mean"}, \code{"var"}, \code{"ci"},
#'  \code{"pct_ci"} (percentages with visible confidence interval),
#'  \code{"mean_ci"} (means with visible confidence interval). As a single string, or a
#'  character vector the length of \code{n}.
#' @param wn The underlying weighted counts, as a double vector the length of
#' \code{n}. It is used in certain operations on \code{\link{fmt}}, like means.
#' @param pct The percentages, as a double vector the length of \code{n}.
#'  Calculate with \code{\link{tab_pct}}.
#' @param mean The means, as a double vector the length of \code{n}.
#' @param diff The differences (from totals or first cells),
#' as a double vector the length of \code{n}. Used to set colors for means and
#' row or col percentages. Calculate with \code{\link{tab_pct}}.
#' @param ctr The contributions of cells to (sub)tables variances,
#' as a double vector the length of \code{n}. Used to print colors when
#' \code{color = "contrib"}. The mean contribution of each (sub)table is written on
#' total rows (then, colors don't print well without total rows).
#' Calculate with \code{\link{tab_chi2}}.
#' @param var The cells variances, as a double vector the length of \code{n}.
#' Used with \code{type = "mean"} to calculate confidence intervals.
#' Calculate with \code{tab_plain}.
#' @param ci The confidence intervals, as a double vector the length of \code{n}.
#' Used to print colors (\code{"diff_ci"}, \code{"after_ci"}).
#' Calculate with \code{tab_ci}.
#' @param in_totrow \code{TRUE} when the cell is part of a total row
#' @param in_tottab \code{TRUE} when the cell is part of a total table
#' @param in_refrow \code{TRUE} when the cell is part of a reference row
#' (cf. \code{diff_type})
#' @param comp_all  \code{FALSE} when the comparison level is the subtable/group,
#' \code{TRUE} when it is the whole table
#' @param diff_type The type of difference of the vector (calculate
#' with \code{\link{tab_pct}}) :
#' \itemize{
#'   \item \code{""} or \code{"no"}: no differences have been calculated
#'   \item \code{"tot"}: the reference row (or column) is the total row (or column)
#'   \item \code{"first"}: the reference row (or column) is the first row (or column)
#' }
#' @param ci_type The type of confidence intervals of the vector (calculate
#'  with \code{\link{tab_ci}}) :
#' \itemize{
#'   \item \code{""} or \code{"no"}: no ci have been calculated
#'   \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'   \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'   relative total cell (or relative first cell when \code{diff_type = "first"}).
#'   \item \code{"auto"}: \code{"diff"} for means and row/col percentages,
#'   \code{"cell"} for frequencies ("all", "all_tabs").
#'  }
#' @param col_var The name of the \code{col_var} used to calculate the vector
#' @param totcol \code{TRUE} when the vector is a total column
#' @param refcol \code{TRUE} when the vector is a reference column
# @param fmt A fmt vector to test or to modify fields.
#' @param x The object to test, to get a field in, or to modify.
#' @param ... Used in methods to add arguments in the future.
#' @param color The type of color to print :
#' \itemize{
#'   \item \code{"no"}: no colors are printed.
#'   \item \code{"diff"}: color percentages and means based on cells differences from
#'   totals (or from first cells when \code{diff = "first"}).
#'   \item \code{"diff_ci"}: color pct and means based on cells differences from totals
#'   or first cells, removing coloring when the confidence interval of this difference
#'   is higher than the difference itself.
#'   \item \code{"after_ci"}: idem, but cut off the confidence interval from the
#'   difference first.
#'   \item \code{"contrib"}: color cells based on their contribution to variance
#'   (except mean columns, from numeric variables).
#' }
#' @return A vector of class \code{tabxplor_fmt}.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' f <- fmt(n = c(7, 19, 2), type = "row", pct = c(0.25, 0.679, 0.07))
#' f
#'
#' # To get the currently displayed field :
#' get_num(f)
#'
#' # To modify the currently displayed field :
#' set_num(f, c(1, 0, 0))
#'
#'
#' # See all the underlying fields of a fmt vector (a data frame with a number of rows
#' #  equal to the length of the vector) :
#' vctrs::vec_data(f)
#'
#' # To get the numbers of digits :
#' vctrs::field(f, "digits")
#' f$digits
#'
#' # To get the count :
#' vctrs::field(f, "n")
#' f$n
#'
#' # To get the display :
#' vctrs::field(f, "display")
#' f$display
#'
#' # To modify a field, you can use `dplyr::mutate` on the fmt vector,
#' # referring to the names of the columns of the underlying data.frame (`vctrs::vec_data`) :
#' vctrs::`field<-`(f, "pct", c(1, 0, 0))
#' mutate(f, pct = c(1, 0, 0))
#'
#' # See all the attributes of a fmt vector :
#' attributes(f)
#'
#' # To modify the "type" attribute of a fmt vector :
#' set_type(f, "col")
#'
#' # To modify the "color" attribute of a fmt vector :
#' set_color(f, "contrib")
#'
#'
#'
#'
#' tabs <- tab(starwars, sex, hair_color, gender, na = "drop", pct = "row",
#'             rare_to_other = TRUE, n_min = 5)
#'
#' # To identify the total columns, and work with them :
#' is_totcol(tabs)
#' tabs %>% mutate(across(where(is_totcol), ~ "total column"))
#'
#' # To identify the total rows, and work with them :
#' is_totrow(tabs)
#' tabs %>%
#'   mutate(across(
#'     where(is_fmt),
#'     ~ if_else(is_totrow(.), true = "into_total_row", false = "normal_cell")
#'   ))
#'
#' # To identify the total tables, and work with them :
#' tottabs <- is_tottab(tabs)
#' tabs %>% tibble::add_column(tottabs) %>%
#'   mutate(total = if_else(tottabs, "part of a total table", "normal cell"))
#'
#' # To access the displayed numbers, as numeric vectors :
#' tabs %>% mutate(across(where(is_fmt), get_num))
#'
#' # To access the displayed numbers, as character vectors (without colors) :
#' tabs %>% mutate(across(where(is_fmt), format))
#'
#' # To access the (non-displayed) differences of the cells percentages from totals :
#' tabs %>% mutate(across(where(is_fmt), ~ vctrs::field(., "diff")))
#'
#'
#' # To do more complex operations, like creating a new column with standard deviation and
#' # print it with 2 decimals, use `dplyr::mutate` on all the fmt columns of a table :
#'
#' tab_num(forcats::gss_cat, race, c(age, tvhours), marital, digits = 1L, comp = "all") |>
#'   dplyr::mutate(dplyr::across( #Mutate over the whole table.
#'     c(age, tvhours),
#'     ~ dplyr::mutate(.,         #Mutate over each fmt vector's underlying data.frame.
#'                     var     = sqrt(var),
#'                     display = "var",
#'                     digits  = 2L) |>
#'       set_color("no"),
#'     .names = "{.col}_sd"
#'   ))
fmt <- function(n         = integer(),
                type      = "n",

                digits    = rep(0L      , length(n)),
                display   = dplyr::case_when(
                  type == "mean"                                ~ "mean",
                  type %in% c("row", "col", "all", "all_tabs")  ~ "pct" ,
                  TRUE                                          ~ "n"    ),

                wn        = rep(NA_real_, length(n)),
                pct       = rep(NA_real_, length(n)),
                mean      = rep(NA_real_, length(n)),
                diff      = rep(NA_real_, length(n)),
                ctr       = rep(NA_real_, length(n)),
                var       = rep(NA_real_, length(n)),
                ci        = rep(NA_real_, length(n)),

                in_totrow = rep(FALSE, length(n)),
                in_tottab = rep(FALSE, length(n)),
                in_refrow = rep(FALSE, length(n)),


                comp_all  = NA   ,
                diff_type = ""   ,
                ci_type   = ""   ,
                col_var   = ""   ,
                totcol    = FALSE,
                refcol    = FALSE,
                color     = ""    ) {

  max_size <- list(n, wn, pct, digits, ctr, mean, var, ci) %>% #display
    purrr::map_int(length) %>% max()

  display <- vctrs::vec_recycle(vctrs::vec_cast(display, character()), size = max_size)
  n       <- vctrs::vec_recycle(vctrs::vec_cast(n      , integer())  , size = max_size)
  wn      <- vctrs::vec_recycle(vctrs::vec_cast(wn     , double())   , size = max_size) #anything coercible as a double
  pct     <- vctrs::vec_recycle(vctrs::vec_cast(pct    , double())   , size = max_size)
  diff    <- vctrs::vec_recycle(vctrs::vec_cast(diff   , double())   , size = max_size)
  digits  <- vctrs::vec_recycle(vctrs::vec_cast(digits , integer())  , size = max_size)
  ctr     <- vctrs::vec_recycle(vctrs::vec_cast(ctr    , double())   , size = max_size)
  mean    <- vctrs::vec_recycle(vctrs::vec_cast(mean   , double())   , size = max_size)
  var     <- vctrs::vec_recycle(vctrs::vec_cast(var    , double())   , size = max_size)
  ci      <- vctrs::vec_recycle(vctrs::vec_cast(ci     , double())   , size = max_size)

  in_totrow <- vctrs::vec_recycle(vctrs::vec_cast(in_totrow, logical()), size = max_size)
  in_tottab <- vctrs::vec_recycle(vctrs::vec_cast(in_tottab, logical()), size = max_size)
  in_refrow <- vctrs::vec_recycle(vctrs::vec_cast(in_refrow, logical()), size = max_size)

  type      <- vctrs::vec_recycle(vctrs::vec_cast(type     , character()), size = 1)
  comp_all  <- vctrs::vec_recycle(vctrs::vec_cast(comp_all , logical()  ), size = 1)
  diff_type <- vctrs::vec_recycle(vctrs::vec_cast(diff_type, character()), size = 1)
  ci_type   <- vctrs::vec_recycle(vctrs::vec_cast(ci_type  , character()), size = 1)
  col_var   <- vctrs::vec_recycle(vctrs::vec_cast(col_var  , character()), size = 1)
  totcol    <- vctrs::vec_recycle(vctrs::vec_cast(totcol   , logical()  ), size = 1)
  refcol    <- vctrs::vec_recycle(vctrs::vec_cast(totcol   , logical()  ), size = 1)
  color     <- vctrs::vec_recycle(vctrs::vec_cast(color    , character()), size = 1)

  new_fmt(n = n, display = display, digits = digits,
          wn = wn, pct = pct,  mean = mean,
          diff = diff, ctr = ctr,var = var, ci = ci,
          in_totrow = in_totrow, in_tottab = in_tottab, in_refrow = in_refrow,
          type = type, comp_all = comp_all,  diff_type = diff_type,
          ci_type = ci_type, col_var = col_var, totcol = totcol, refcol = refcol,
          color = color)
}

#' @describeIn fmt a test function for class fmt.
#' @return A logical vector.
#' @export
is_fmt <- function(x) {
  inherits(x, "tabxplor_fmt")
}


# #' A function to convert vectors to class fmt.
# #' @param x A vector coercible to double, or a character vector with numbers.
# #' @param ... The number of digits as an integer, to be passed to the method.
# #'
# #' @export
# as_fmt <- function(x, ...) {
#   UseMethod("as_fmt")
# }

# # @describeIn as_fmt
# #' @export
# as_fmt.default <- function(x, digits = rep(0L, length(x)), #display = rep("count", length(x)),
#                             # n = rep(NA_integer_, length(x)), wn = rep(NA_real_, length(x)),
#                             # var = rep(NA_real_, length(x)), ci = rep(NA_real_, length(x)),
#                             ...) {
#   new_fmt(vec_data(x))
# }



#' @describeIn fmt get the currently displayed field
#' @return A double vector.
#' @export
get_num <- function(x) {
  out     <- get_n(x)
  display <- get_display(x)
  nas     <- is.na(display)
  out[!nas & display == "wn"     ] <- get_wn  (x)[!nas & display == "wn"     ]
  out[!nas & display == "pct"    ] <- get_pct (x)[!nas & display == "pct"    ]
  out[!nas & display == "diff"   ] <- get_diff(x)[!nas & display == "diff"   ]
  out[!nas & display == "pct_ci" ] <- get_pct (x)[!nas & display == "pct_ci" ]
  out[!nas & display == "ctr"    ] <- get_ctr (x)[!nas & display == "ctr"    ]
  out[!nas & display == "mean"   ] <- get_mean(x)[!nas & display == "mean"   ]
  out[!nas & display == "mean_ci"] <- get_mean(x)[!nas & display == "mean_ci"]
  out[!nas & display == "var"    ] <- get_var (x)[!nas & display == "var"    ]
  out[!nas & display == "ci"     ] <- get_ci  (x)[!nas & display == "ci"     ]
  out
}

#' @describeIn fmt set the currently displayed field (not changing display type)
#' @param value The value you want to inject in some \code{fmt} vector's vctrs::field
#' or attribute using a given "set" function.
#' @return A modified fmt vector.
#' @export
set_num <- function(x, value) {
  value <- vctrs::vec_recycle(value, length(x))
  out     <- x
  display <- get_display(x)
  nas     <- is.na(display)
  out[!nas & display == "n"   ] <- set_n   (x[!nas & display == "n"   ], value[!nas & display == "n"   ])
  out[!nas & display == "wn"  ] <- set_wn  (x[!nas & display == "wn"  ], value[!nas & display == "wn"  ])
  out[!nas & display == "pct" ] <- set_pct (x[!nas & display == "pct" ], value[!nas & display == "pct" ])
  out[!nas & display == "diff"] <- set_pct (x[!nas & display == "diff"], value[!nas & display == "diff"])
  out[!nas & display == "ctr" ] <- set_ctr (x[!nas & display == "ctr" ], value[!nas & display == "ctr" ])
  out[!nas & display == "mean"] <- set_mean(x[!nas & display == "mean"], value[!nas & display == "mean"])
  out[!nas & display == "var" ] <- set_var (x[!nas & display == "var" ], value[!nas & display == "var" ])
  out[!nas & display == "ci"  ] <- set_ci  (x[!nas & display == "ci"  ], value[!nas & display == "ci"  ])
  out
}

#' @describeIn fmt get types of fmt columns (at \code{fmt} level or \code{tab} level)
#' @param x The object to test, to get a field in, or to modify.
#' @param ... Used in methods to add arguments in the future.
#' @return A character vector with the vectors type.
#' @export
get_type <- function(x, ...) UseMethod("get_type")
#' Get types of fmt columns
#' @inheritParams fmt
#' @return An empty character vector.
#' @export
get_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("type")(x)),
         yes = purrr::attr_getter("type")(x),
         no  = "") #NA_character_
}
#' Get types of fmt columns
#' @method get_type tabxplor_fmt
#' @inheritParams fmt
#' @return A single string with the vector's type.
#' @export
get_type.tabxplor_fmt <- function(x, ...) attr(x, "type", exact = TRUE)
#' Get types of fmt columns
#' @inheritParams fmt
#' @return A character vector with the data.frame column's types.
#' @export
get_type.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_type(.))

#' @describeIn fmt set the column type attribute of a \code{fmt} vector
#' @return A modified fmt vector.
#' @export
set_type      <- function(x, type) {
  if (type %in% c("no", "", NA_character_)) type <- "n"
  stopifnot(type %in% c("row", "col", "all", "all_tabs", "mean", "n"))
  `attr<-`(x ,"type"    , type)
}




#' @describeIn fmt test function to detect cells in total rows
#' (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors totrow field.
#' @export
is_totrow <- function(x, ...) UseMethod("is_totrow")
#' Test function to detect cells in total rows
#' @inheritParams fmt
#' @return A logical vector with \code{FALSE}.
#' @export
is_totrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' Test function to detect cells in total rows
#' @method is_totrow tabxplor_fmt
#' @inheritParams fmt
#' @return A logical vector with the totrow field.
#' @export
is_totrow.tabxplor_fmt <- function(x, ...) vctrs::field(x, "in_totrow")
#' Test function to detect cells in total rows
#' @inheritParams fmt
#' @param partial Should partial total rows be counted as total rows ? Default to FALSE.
#' @return A list of logical vectors, with the data.frame column's totrow fields.
#' @export
is_totrow.data.frame <- function(x, ..., partial = FALSE) {
  totrow_cells_test <- dplyr::ungroup(x) %>% dplyr::select(where(is_fmt)) %>%
    purrr::map_df(~ is_totrow(.))

  if (partial == TRUE) {
    totrow_cells_test |> dplyr::transmute(var = dplyr::if_any()) |> tibble::deframe()
  } else {
    test_result <- totrow_cells_test %>%
      dplyr::transmute(complete = dplyr::if_all(),
                       partial  = dplyr::if_all(-.data$complete) & !.data$complete)
    if (tidyr::replace_na(any(test_result$partial), FALSE)) {
      warning("partial total rows (with some fmt cells not tagged 'totrow') ",
              "were not taken into account ")
    }
    test_result$complete
  }
}

#' @describeIn fmt set the "in_totrow" field (belong to total row)
#' @return A modified fmt vector with totrow field changed.
#' @export
as_totrow  <- function(x, in_totrow = TRUE) {
  vctrs::vec_assert(in_totrow, logical())
  vctrs::`field<-`(x, "in_totrow", vctrs::vec_recycle(in_totrow, length(x)))
}

#' Complete partial total rows
#'
#' @param tabs A table or data.framate containting `tabxplor_fmt` columns.
#'
#' @return The table with completed total rows, total tables, and reference rows.
#' @export
#'
# @examples
complete_partial_totals <- function(tabs) {
  .diff_totrows <- suppressWarnings(is_totrow(tabs)) != is_totrow(tabs, partial = TRUE)

  if (any(.diff_totrows)) {
    tabs <- tabs |>
      tibble::add_column(.diff_totrows) |>
      dplyr::mutate(dplyr::across(where(is_fmt), ~ dplyr::if_else(
        condition = .diff_totrows,
        true      = as_totrow(.),
        false     = .
      ))) |>
      select(-.diff_totrows)
  }

  .diff_tottabs <- suppressWarnings(is_tottab(tabs)) != is_tottab(tabs, partial = TRUE)
  if (any(.diff_tottabs)) {
    tabs <- tabs |>
      tibble::add_column(.diff_tottabs) |>
      dplyr::mutate(dplyr::across(where(is_fmt), ~ dplyr::if_else(
        condition = .diff_tottabs,
        true      = as_tottab(.),
        false     = .
      ))) |>
      select(-.diff_tottabs)
  }

  .diff_refrows <- suppressWarnings(is_refrow(tabs)) != is_refrow(tabs, partial = TRUE)
  if (any(.diff_refrows)) {
    tabs <- tabs |>
      tibble::add_column(.diff_refrows) |>
      dplyr::mutate(dplyr::across(where(is_fmt), ~ dplyr::if_else(
        condition = .diff_refrows,
        true      = as_refrow(.),
        false     = .
      ))) |>
      select(-.diff_refrows)
  }

  tabs
}




#' @describeIn fmt test function to detect cells in total tables
#' (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors tottab field.
#' @export
is_tottab <- function(x, ...) UseMethod("is_tottab")
#' Test function to detect cells in total tables
#' @method is_tottab default
#' @inheritParams fmt
#' @return A logical vector with \code{FALSE}.
#' @export
is_tottab.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' Test function to detect cells in total tables
#' @method is_tottab tabxplor_fmt
#' @inheritParams fmt
#' @return A logical vector with the tottab field.
#' @export
is_tottab.tabxplor_fmt <- function(x, ...) vctrs::field(x, "in_tottab")
#' Test function to detect cells in total tables
#' @param partial Should partial total tabs be counted as total tabs ? Default to FALSE.
#' @inheritParams fmt
#' @return A list of logical vectors, with the data.frame column's tottab fields.
#' @export
is_tottab.data.frame <- function(x, ..., partial = FALSE) {
  tottab_cells_test <- dplyr::ungroup(x) %>% dplyr::select(where(is_fmt)) %>%
    purrr::map_df(~ is_tottab(.))



  if (partial == TRUE) {
    tottab_cells_test %>% dplyr::transmute(var = dplyr::if_any()) %>% tibble::deframe()
  } else {
    test_result <- tottab_cells_test %>%
      dplyr::transmute(complete = dplyr::if_all(),
                       partial  = dplyr::if_all(-.data$complete) & !.data$complete)
    if (tidyr::replace_na(any(test_result$partial), FALSE)) {
      warning("partial total rows (with some fmt cells not tagged 'totrow') ",
              "were not taken into account ")
    }
    test_result$complete
  }
}

#' @describeIn fmt set the "in_tottab" field (belong to total table)
#' @return A modified fmt vector with tottab field changed.
#' @export
as_tottab  <- function(x, in_tottab = TRUE) {
  vctrs::vec_assert(in_tottab, logical())
  vctrs::`field<-`(x, "in_tottab", vctrs::vec_recycle(in_tottab, length(x)))
}





#' @describeIn fmt test function for total columns
#' (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors totcol attribute.
#' @export
is_totcol <- function(x, ...) UseMethod("is_totcol")
#' Test function for total columns
#' @inheritParams fmt
#' @return A single logical vector with the totcol attribute
#' @export
is_totcol.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("totcol")(x)),
         yes = purrr::attr_getter("totcol")(x),
         no  = FALSE)
}
#' Test function for total columns
#' @inheritParams fmt
#' @return A single logical vector with the totcol attribute
#' @export
is_totcol.tabxplor_fmt <- function(x, ...) attr(x, "totcol", exact = TRUE)
#' Test function for total columns
#' @inheritParams fmt
#' @return A logical vector, with the data.frame column's totcol attributes.
#' @export
is_totcol.data.frame <- function(x, ...) purrr::map_lgl(x, ~ is_totcol(.))

#' @describeIn fmt set the "totcol" attribute of a \code{fmt} vector
#' @return A modified fmt vector with totcol attribute changed.
#' @export
as_totcol     <- function(x, totcol = TRUE) {
  vctrs::vec_assert(totcol, logical(), size = 1)
  `attr<-`(x ,"totcol"  , totcol)
}



#' @describeIn fmt test function to detect cells in reference rows
#' (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors in_refrow field
#' @export
is_refrow <- function(x, ...) UseMethod("is_refrow")
#' Test function to detect cells in reference rows
#' @method is_refrow default
#' @inheritParams fmt
#' @return A logical vector with FALSE, the length of x.
#' @export
is_refrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' Test function to detect cells in reference rows
#' @method is_refrow tabxplor_fmt
#' @inheritParams fmt
#' @return  A logical vector with the in_refrow field.
#' @export
is_refrow.tabxplor_fmt <- function(x, ...) vctrs::field(x, "in_refrow")
#' Test function to detect cells in reference rows
#' @method is_refrow data.frame
#' @param partial Should partial reference rows be counted as reference rows ? Default to FALSE.
#' @inheritParams fmt
#' @return A list of logical vectors with the in_refrow fields.
#' @export
is_refrow.data.frame <- function(x, ..., partial = TRUE) {
  refrow_cells_test <- dplyr::ungroup(x) %>% dplyr::select(where(is_fmt)) %>%
    purrr::map_df(~ is_refrow(.))

  if (partial == TRUE) {
    refrow_cells_test %>% dplyr::transmute(var = dplyr::if_any()) %>% tibble::deframe()
  } else {
    test_result <- refrow_cells_test %>%
      dplyr::transmute(complete = dplyr::if_all(),
                       partial  = dplyr::if_all(-.data$complete) & !.data$complete)
    if (tidyr::replace_na(any(test_result$partial), FALSE)) {
      warning("partial total rows (with some fmt cells not tagged 'refrow') ",
              "were not taken into account ")
    }
    test_result$complete
  }
}

#' @describeIn fmt set the "in_refrow" field (belong to reference row)
#' @return A modified fmt vector with in_refrom field changed.
#' @export
as_refrow  <- function(x, in_refrow = TRUE) {
  vctrs::vec_assert(in_refrow, logical())
  vctrs::`field<-`(x, "in_refrow", vctrs::vec_recycle(in_refrow, length(x)))
}


#' @describeIn fmt get comparison level of fmt columns
#' @inheritParams fmt
#' @param replace_na By default, \code{\link{get_comp_all}} takes NA in comparison level
#' to be a \code{FALSE} (=comparison at subtables/groups level). Set to \code{FALSE}
#' to avoid this behavior.
# @keywords internal
#' @export
get_comp_all <- function(x, replace_na = TRUE) {
  comp <- attr(x, "comp_all", exact = TRUE)
  if (is.null(comp)) return(NA)
  if (replace_na & is.na(comp)) comp <- FALSE
  comp
}

#' @describeIn fmt set the comparison level attribute of a \code{fmt} vector
# @param fmt  The fmt object to modify.
# @param value One of "tab" (comparison inside subtables) or "all" (comparison with
# total table).
#' @return A modified fmt vector with comp attribute changed.
#' @export
set_comp_all      <- function(x, comp_all = FALSE) { #comp_all = c("tab", "all")
  `attr<-`(x, "comp_all", comp_all) # comp_all == "all"
}



#' @describeIn fmt get differences type of fmt columns (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors type attributes
#' @export
get_diff_type <- function(x, ...) UseMethod("get_diff_type")
#' Get differences type of fmt columns
#' @method get_diff_type default
#' @inheritParams fmt
#' @return A single character with the diff_type attribute.
#' @export
get_diff_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("diff_type")(x)),
         yes = purrr::attr_getter("diff_type")(x),
         no  = "") #NA_character_
}
#' Get differences type of fmt columns
#' @method get_diff_type tabxplor_fmt
#' @inheritParams fmt
#' @return A single character with the diff_type attribute.
#' @export
get_diff_type.tabxplor_fmt <- function(x, ...) attr(x, "diff_type", exact = TRUE)
#' Get differences type of fmt columns
#' @method get_diff_type data.frame
#' @inheritParams fmt
#' @return A character vector with the diff_type attribute.
#' @export
get_diff_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_diff_type(.))
}

#' @describeIn fmt set the differences type attribute of a \code{fmt} vector
#' @return A modified fmt vector.
#' @export
set_diff_type   <- function(x, diff_type) {
  #stopifnot(diff_type %in% c("tot", "first", "no", "", NA_character_))
  `attr<-`(x ,"diff_type" , diff_type)
}




#' @describeIn fmt get confidence intervals type of fmt columns (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors ci_type attributes
#' @export
get_ci_type <- function(x, ...) UseMethod("get_ci_type")
#' Get confidence intervals type of fmt columns
#' @method get_ci_type default
#' @inheritParams fmt
#' @return A single character with the ci_type attribute.
#' @export
get_ci_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("ci_type")(x)),
         yes = purrr::attr_getter("ci_type")(x),
         no  = "") #NA_character_
}
#' Get confidence intervals type of fmt columns
#' @method get_ci_type tabxplor_fmt
#' @inheritParams fmt
#' @return A single character with the ci_type attribute.
#' @export
get_ci_type.tabxplor_fmt <- function(x, ...) attr(x, "ci_type", exact = TRUE)
#' Get confidence intervals type of fmt columns
#' @method get_ci_type data.frame
#' @inheritParams fmt
#' @return A character vector with the ci_type attributes.
#' @export
get_ci_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_ci_type(.))
}


#' @describeIn fmt set the confidence intervals type attribute of a \code{fmt} vector
# @param ci_type The type of confidence interval calculated in "ci", as a single string.
#' @return A modified fmt vector.
#' @export
set_ci_type   <- function(x, ci_type) {
  stopifnot(ci_type %in% c("cell", "diff", "diff_row", "diff_col",
                           "no", "", NA_character_))
  `attr<-`(x ,"ci_type" , ci_type)
}


#' @describeIn fmt get names of column variable of fmt columns (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors col_var attributes
#' @export
get_col_var <- function(x, ...) UseMethod("get_col_var")
#' Get names of column variable of fmt columns
#' @method get_col_var default
#' @inheritParams fmt
#' @return A single character with the col_var attribute.
#' @export
get_col_var.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("col_var")(x)),
         yes = purrr::attr_getter("col_var")(x),
         no  = "") #NA_character_
}
#' Get names of column variable of fmt columns
#' @method get_col_var tabxplor_fmt
#' @inheritParams fmt
#' @return A single character with the col_var attribute.
#' @export
get_col_var.tabxplor_fmt <- function(x, ...) attr(x, "col_var", exact = TRUE)
#' Get names of column variable of fmt columns
#' @method get_col_var data.frame
#' @inheritParams fmt
#' @return A character vector with the col_var attributes.
#' @export
get_col_var.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_col_var(.))

#' @describeIn fmt set the "col_var" attribute of a \code{fmt} vector
# @param col_var The name of the column variable, as a single string.
#' @return A modified fmt vector.
#' @export
set_col_var   <- function(x, col_var) {
  vctrs::vec_assert(col_var, character(), size = 1)
  `attr<-`(x ,"col_var" , col_var)
}



#' @describeIn fmt test function for reference columns (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors is_refcol attributes
#' @export
is_refcol <- function(x, ...) UseMethod("is_refcol")
#' Test function for reference columns
#' @method is_refcol default
#' @inheritParams fmt
#' @return A single character with the ref_col attribute.
#' @export
is_refcol.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("refcol")(x)),
         yes = purrr::attr_getter("refcol")(x),
         no  = FALSE)
}
#' Test function for reference columns
#' @method is_refcol tabxplor_fmt
#' @inheritParams fmt
#' @return A single character with the ref_col attribute.
#' @export
is_refcol.tabxplor_fmt <- function(x, ...) attr(x, "refcol", exact = TRUE)
#' Test function for reference columns
#' @method is_refcol data.frame
#' @inheritParams fmt
#' @return A character vector with the ref_col attributes.
#' @export
is_refcol.data.frame <- function(x, ...) purrr::map_lgl(x, ~ is_refcol(.))


#' @describeIn fmt set the "ref_col" attribute of a \code{fmt} vector
# @param refcol Is the vector a reference column ? As a logical vector of length one.
#' @return A modified fmt vector.
#' @export
as_refcol     <- function(x, refcol = TRUE) {
  vctrs::vec_assert(refcol, logical(), size = 1)
  `attr<-`(x ,"refcol"  , refcol)
}


#' @describeIn fmt get color (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors color attributes
#' @export
get_color <- function(x, ...) UseMethod("get_color")
#' Get color
#' @method get_color default
#' @inheritParams fmt
#' @return A single character with the color attribute.
#' @export
get_color.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("color")(x)),
         yes = purrr::attr_getter("color")(x),
         no  = "") #NA_character_
}
#' Get color
#' @method get_color tabxplor_fmt
#' @inheritParams fmt
#' @return A single character with the color attribute.
#' @export
get_color.tabxplor_fmt <- function(x, ...) attr(x, "color", exact = TRUE)
#' Get color
#' @method get_color data.frame
#' @inheritParams fmt
#' @return A character vector with the color attributes.
#' @export
get_color.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_color(.))
}


#' @describeIn fmt set the "color" attribute of a \code{fmt} vector
# @param color The type of color to print in tibbles, as a single string.
#' @return A modified fmt vector.
#' @export

# @keywords internal
#' @export
set_color     <- function(x, color) {
  if (color %in% c("no", NA_character_)) color <- "" #NA_character_
  stopifnot(color %in% c("diff", "diff_ci", "after_ci", "contrib", "ci",
                         "", NA_character_))
  `attr<-`(x ,"color", color)
}



# fmt_get_color_code() doen't work in mutate with groups.

#' Get HTML Color Code of a fmt vector
#' @param x The fmt vector to get the html color codes from.
#'
#' @param type The style type in \code{set_color_style} and \code{get_color_style},
#'  \code{"text"} to color the text, \code{"bg"} to color the background.
#' @param theme For \code{set_color_style} and \code{get_color_style}, is your console
#' or html table background \code{"light"} or \code{"dark"} ? Default to RStudio theme.
#' @param html_24_bit Should specific 24bits colors palettes be used for html tables ?
#' With light themes only. Default to \code{getOption("tabxplor.color_html_24_bit")}
#' @return A character vector with html color codes, of the length of the initial vector.
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
#' dplyr::mutate(tabs, across(where(is_fmt), fmt_get_color_code))
#'}

fmt_get_color_code <- function(x, type = "text", theme = "light", html_24_bit = NULL) {
  html_24_bit <- if (is.null(html_24_bit)) {getOption("tabxplor.color_html_24_bit")} else {html_24_bit}

  color_selection <- fmt_color_selection(x) %>% purrr::map(which)

  color_styles <- select_in_color_style(length(color_selection))
  color_styles <- get_color_style("color_code", type = type, theme = theme,
                                  html_24_bit = html_24_bit)[color_styles]

  color_positions <- color_selection %>%
    purrr::map2(color_styles, ~ purrr::set_names(.x, stringr::str_to_upper(.y))) %>%
    purrr::flatten_int()

  no_color <- 1:length(x)
  no_color <- purrr::set_names(no_color[!no_color %in% color_positions], NA_character_)

  names(sort(c(color_positions, no_color)))
}






# INTERNAL FUNCTIONS #####################################################################


# @describeIn
#' fmt a constructor for class fmt.
#' @param class Subclasses to assign to the new object, default: none.
#' @keywords internal
# @export
new_fmt <- function(n         = integer(),
                    type      = "n"          ,

                    digits    = rep(0L      , length(n)),
                    display   = dplyr::case_when(
                      type == "mean"                                ~ "mean",
                      type %in% c("row", "col", "all", "all_tabs")  ~ "pct" ,
                      TRUE                                          ~ "n"    ),

                    wn        = rep(NA_real_, length(n)),
                    pct       = rep(NA_real_, length(n)),
                    mean      = rep(NA_real_, length(n)),
                    diff      = rep(NA_real_, length(n)),
                    ctr       = rep(NA_real_, length(n)),
                    var       = rep(NA_real_, length(n)),
                    ci        = rep(NA_real_, length(n)),

                    in_totrow = rep(FALSE   , length(n)),
                    in_tottab = rep(FALSE   , length(n)),
                    in_refrow = rep(FALSE   , length(n)),

                    comp_all  = NA   ,
                    diff_type = ""   ,
                    ci_type   = ""   ,
                    col_var   = ""   ,
                    totcol    = FALSE,
                    refcol    = FALSE,
                    color     = ""   ,
                    ..., class = character()
) {
  # stopifnot(
  #   all(display %in% c("n", "wn", "pct", "pct_ci", "ctr", "mean", "mean_ci", "var", "ci")),
  #   type %in% c("row", "col", "all", "all_tabs", "mixed", NA_character_)
  # )

  # list(display, n, wn, pct, digits, ctr, mean, var, ci, col_var, totcol, type) %>%
  #   purrr::map(print)
  # cat("\n")

  # list(n = n, display = display, digits = digits,
  #      wn = wn, pct = pct, mean = mean,
  #      diff = diff, ctr = ctr, var = var, ci = ci,
  #      in_totrow = in_totrow, in_tottab = in_tottab,
  #      in_refrow = in_refrow) |>
  #   purrr::map(length) |> print()
  # cat("\n")

  #vctrs::vec_assert(display, character()) #check display or size
  display <- vctrs::vec_recycle(display, size = length(n))
  # vctrs::vec_assert(n     , integer()) #, size = length(n)
  # vctrs::vec_assert(wn    , double() ) #, size = length(n)
  # vctrs::vec_assert(pct   , double() ) #, size = length(n)
  # vctrs::vec_assert(digits, integer()) #, size = length(n)
  # vctrs::vec_assert(ctr   , double() ) #, size = length(n)
  # vctrs::vec_assert(mean  , double() ) #, size = length(n)
  # vctrs::vec_assert(var   , double() ) #, size = length(n)
  # vctrs::vec_assert(ci    , double() ) #, size = length(n)
  #
  # vctrs::vec_assert(in_totrow, logical())
  # vctrs::vec_assert(in_tottab, logical())
  #
  # vctrs::vec_assert(type    , character(), size = 1)
  # vctrs::vec_assert(comp_all, logical()  , size = 1)
  # vctrs::vec_assert(ci_type , character(), size = 1)
  # vctrs::vec_assert(col_var , character(), size = 1)
  # vctrs::vec_assert(totcol  , logical()  , size = 1)
  # vctrs::vec_assert(color   , character(), size = 1)

  vctrs::new_rcrd(
    list(n = n, display = display, digits = digits,
         wn = wn, pct = pct, mean = mean,
         diff = diff, ctr = ctr, var = var, ci = ci,
         in_totrow = in_totrow, in_tottab = in_tottab,
         in_refrow = in_refrow),
    type = type, comp_all = comp_all, diff_type = diff_type,
    ci_type = ci_type, col_var = col_var, totcol = totcol, refcol = refcol,
    color = color,  class = c(class, "tabxplor_fmt"))
  #access with fields() n_fields() vctrs::field() vctrs::`field<-`() ;
  #vec_data() return the tibble with all fields
}





#' @keywords internal
fmt0 <- function(display = "n", digits = 0, type = "n") {
  new_fmt(n = 0L, display = display, digits = as.integer(digits), type = type)
  # switch (display,
  #   "n"       = new_fmt(display = display, n = 0L,                           digits = as.integer(digits)),
  #   "wn"      = new_fmt(display = display, n = 0L, wn = 0,                   digits = as.integer(digits)),
  #   "pct"     = ,
  #   "pct_ci"  = new_fmt(display = display, n = 0L, wn = 0, pct = 0,          digits = as.integer(digits)),
  #   "ctr"     = new_fmt(display = display, n = 0L, wn = 0, pct = 0, ctr = 0, digits = as.integer(digits)),
  #   "mean"    = ,
  #   "mean_ci" = new_fmt(display = display, n = 0L, wn = 0, mean = 0, var = 0, digits = as.integer(digits)),
  #   "var"      = new_fmt(display = display, n = 0L, wn = 0, mean = 0, var = 0, digits = as.integer(digits)),
  #   "ci"      = new_fmt(display = display, n = 0L, ci = 0,                   digits = as.integer(digits)),
  # )
}




# Internal functions to get fields and attributes of class fmt

#' @keywords internal
fmt_field_factory <- function(.field) {
  function(x) vctrs::field(x, .field)
}

# @describeIn fmt
#' get the "display" field of a \code{fmt} vector
#' @param x The formatted number in which you want to find data for "get" functions,
#' to modify data for "set" functions.
#' @keywords internal
# @export
get_display <- fmt_field_factory("display")
# @describeIn fmt get the "n" field (unweighted counts)
#' @keywords internal
# @export
get_n      <- fmt_field_factory("n")
# @describeIn fmt get the "wn" field (weighted counts)
#' @keywords internal
# @export
get_wn     <- function(x) { #If there is no weighted counts, take counts
  out <- vctrs::field(x, "wn")
  if (any(is.na(out))) {
    counts <- vctrs::field(x, "n") %>% as.double()
    out[is.na(out)] <- counts[is.na(out)]
  }
  out
}
# @describeIn fmt get the "pct" field
#' @keywords internal
# @export
get_pct    <- fmt_field_factory("pct")
# @describeIn fmt get the "diff" field (differences from totals or first cells)
#' @keywords internal
# @export
get_diff   <- fmt_field_factory("diff")
#get_pct_ci <- function(x) vctrs::field("pct")
#' @describeIn fmt get the "digits" field
# @keywords internal
#' @export
get_digits <- fmt_field_factory("digits")
# @describeIn fmt get the "ctr" field (relative contributions of cells to variance)
#' @keywords internal
# @export
get_ctr    <- fmt_field_factory("ctr")
# @describeIn fmt get the "mean" field
#' @keywords internal
# @export
get_mean   <- fmt_field_factory("mean")
# @describeIn fmt get the "var" field (cell variances of means)
#' @keywords internal
# @export
get_var    <- fmt_field_factory("var")
# @describeIn fmt get the "ci" field (confidence intervals)
#' @keywords internal
# @export
get_ci     <- fmt_field_factory("ci")

#' @keywords internal
get_mean_contrib <- function(x) {
  comp    <- get_comp_all(x)
  totrows <- is_totrow(x)
  tottabs <- is_tottab(x)
  ctr     <- get_ctr(x)

  if (!any(totrows)) return(rep(NA_real_, length(x)))

  if (comp) {
    rep(ctr[totrows & tottabs], length(x))
  } else {
    tibble::tibble(
      ctr = ctr,
      gr = cumsum(as.integer(totrows)) - as.integer(totrows) ) %>%
      dplyr::mutate(nb = dplyr::row_number()) %>%
      dplyr::with_groups(.data$gr, ~ dplyr::mutate(., nb = dplyr::last(.data$nb))) %>%
      dplyr::mutate(mean_ctr = .data$ctr[.data$nb]) %>% dplyr::pull(.data$mean_ctr)
  }
}

#' @keywords internal
get_ref_means <- function(x) {
  comp      <- get_comp_all(x)
  diff_type <- get_diff_type(x)

  refrows <- if (diff_type == "tot") { is_totrow(x) } else { is_refrow(x) }
  tottabs <- is_tottab(x)
  mean    <- get_mean(x)

  if (comp) {
   refs <- mean[refrows & tottabs]
   if (length(refs) == 0) {rep(NA_real_, length(x))} else {rep(mean[refs], length(x))}
  } else {
    tibble::tibble(
      mean = mean,
      gr = cumsum(as.integer(refrows)) - as.integer(refrows) ) %>%
      dplyr::mutate(nb = dplyr::row_number()) %>%
      dplyr::with_groups(.data$gr, ~ dplyr::mutate(., nb = dplyr::last(.data$nb))) %>%
      dplyr::mutate(ref_means = .data$mean[.data$nb]) %>%
      dplyr::pull(.data$ref_means)
  }
}

# "every S3 method must be exported, even if the generic is not" Really (->CRAN pb) ??

# #' @return A character vector with the vectors type.
# #' @return An empty character vector.
# #' @return A single string with the vector's type.
# #' @return A character vector with the data.frame column's types.
# #' @return A modified fmt vector.
#
# #' @return A logical vector with the fmt vectors tottab field.
# #' @return A logical vector with \code{FALSE}.
# #' @return A logical vector with the tottab field.
# #' @return A list of logical vectors, with the data.frame column's tottab fields.
# #' @return A modified fmt vector with tottab field changed.
#
# #' @return A logical vector with the fmt vectors totcol attribute.
# #' @return A logical vector with \code{FALSE}.
# #' @return A single logical vector with the totcol attribute
# #' @return A logical vector, with the data.frame column's totcol attributes.
# #' @return A modified fmt vector with totcol attribute changed.


#' @keywords internal
detect_firstcol <- function(tabs) {
  col_vars <- get_col_var(tabs)
  firstcol <- which(col_vars != dplyr::lag(col_vars, default = NA_character_))
  if (any(col_vars == "all_col_vars")) firstcol <- firstcol %>%
    purrr::discard(names(.) == names(col_vars)[col_vars == "all_col_vars"])

  res <- purrr::map(1:ncol(tabs), function(.i)
    tidyr::replace_na(
      dplyr::last(names(firstcol[firstcol <= .i]) ),
      "")) %>%
    rlang::syms() %>%
    purrr::set_names(names(tabs))

  if (any(col_vars == "all_col_vars")) {
    #   res_all_col <- tabs[as.character(res[col_vars == "all_col_vars"])]
    #
    # if (get_type(res_all_col) == "mean") res[col_vars == "all_col_vars"] <-
    #     rlang::syms("")
    res[col_vars == "all_col_vars"] <- rlang::syms("")
  }
  res
}

#For each column, detect which total column it depends on
#' @keywords internal
detect_totcols <- function(tabs) {
  #detect totcols by col vars names, no position ? ----
  tot <- which(is_totcol(tabs))

  purrr::map(1:ncol(tabs), function(.i)
    tidyr::replace_na(names(tot[tot >= .i])[1], "")) %>%
    rlang::syms() %>%
    purrr::set_names(names(tabs))
}



# Internal functions to modify class tabxplor_fmt

#' @keywords internal
fmt_set_field_factory <- function(.field, cast) {
  function(x, value) {
    value <- vctrs::vec_cast(value, cast) %>% vctrs::vec_recycle(size = length(x))
    vctrs::`field<-`(x, .field, value)
  }
}
# @describeIn fmt set the "display" vctrs::field of a \code{fmt} vector
#' @keywords internal
# @export
set_display <- fmt_set_field_factory("display", cast = character())
# @describeIn fmt set the "n" field (unweighted counts)
#' @keywords internal
# @export
set_n       <- fmt_set_field_factory("n"      , cast = integer()  )
# @describeIn fmt set the "wn" field (weighted counts)
#' @keywords internal
# @export
set_wn      <- fmt_set_field_factory("wn"     , cast = double()   )
# @describeIn fmt set the "pct" field
#' @keywords internal
# @export
set_pct     <- fmt_set_field_factory("pct"    , cast = double()   )
# @describeIn fmt set the "diff" field (differences from totals or first cells)
#' @keywords internal
# @export
set_diff    <- fmt_set_field_factory("diff"   , cast = double()   )
#' @describeIn fmt set the "digits" field
# @keywords internal
#' @export
set_digits  <- fmt_set_field_factory("digits" , cast = integer()  )
# @describeIn fmt set the "ctr" field (relative contributions of cells to variance)
# @keywords internal
# @export
set_ctr     <- fmt_set_field_factory("ctr"    , cast = double()   )
# @describeIn fmt set the "mean" field
#' @keywords internal
# @export
set_mean    <- fmt_set_field_factory("mean"   , cast = double()   )
# @describeIn fmt set the "var" field (cell variances of means)
#' @keywords internal
# @export
set_var     <- fmt_set_field_factory("var"    , cast = double()   )
# @describeIn fmt set the "ci" field (confidence intervals)
#' @keywords internal
# @export
set_ci      <- fmt_set_field_factory("ci"     , cast = double()   )






# METHODS FOR CLASS tabxplor_fmt #########################################################

#' @keywords internal
print_num <- function(num, digits) {
  sprintf(paste0("%-0.", digits, "f"), num) %>%
    stringr::str_replace("^0.0+$|^-0.0+$", "0") %>%
    stringr::str_replace("^100.0+$", "100")
}

ci_html_subscript <- function(x, html = FALSE) {
  if (html) x <- dplyr::if_else(stringr::str_detect(x,"^ *$" ),
                                true  = "",
                                false = paste0("$_{", x, "}$"))
  x
}

# Format/printing methods for class tabxplor_fmt -----------------------------------------
#The first method for every class should almost always be a format() method.
#This should return a character vector the same length as x.

#' Print method for class tabxplor_fmt
#'
#' @param x A fmt object.
#' @param ... Other parameters.
#' @param html Should html tags be added (to print confidence intervals as subscripts) ?
#' @param na How `NA`s should be printed. Default to `NA`.
#'
#' @return The fmt printed in a character vector.
#' @export
format.tabxplor_fmt <- function(x, ..., html = FALSE, na = NA) {
  out    <- get_num(x)
  na_out <- is.na(out)

  display <- get_display(x)
  nas  <- is.na(display)
  digits <- get_digits(x)
  digits[!nas & display == "n"] <- 0

  ok <- !na_out & !nas

  type    <- get_type(x)
  ci_type <- get_ci_type(x)

  pm <- stringi::stri_unescape_unicode("\\u00b1") # sign "plus minus"

  pct_or_ci     <- ok & display %in% c("pct", "pct_ci", "diff", "ci", "ctr") &
    !(display %in% c("ci", "diff") & type == "mean")
  pct_ci  <- ok & display == "pct_ci"
  mean_ci <- ok & display == "mean_ci"
  diff_mean <- display == "diff" & type == "mean"
  plus_ci <- pct_ci | mean_ci
  #pct_or_pct_ci <- ok & display %in% c("pct", "pct_ci", "diff", "ctr")
  pct_no_ci     <- ok & display %in% c("pct", "diff", "ctr") & !(display == "diff" & type == "mean")
  n_wn          <- ok & (display %in% c("n", "wn", "mean", "mean_ci", "var") |
                           (display == "ci" & type == "mean") )
  type_ci       <- ok & display == "ci"

  out[pct_or_ci] <- out[pct_or_ci] * 100
  digits[diff_mean] <- dplyr::if_else(digits[diff_mean] == 0, 1, digits[diff_mean])

  if (any(plus_ci)) {
    ci_print_trim <- function(x) {
      x <- stringr::str_remove_all(x, paste0("^", pm, "0$|^", pm, "0.0+$|^", pm, "-0.0+$|^",
                                             pm, "NA"))
      stringr::str_pad(x, max(stringr::str_length(x)))
    }


    # ci_print_pad <- function(x) {
    #   stringr::str_pad(x, max(stringr::str_length(x)))
    # }
    ci <- get_ci(x)[plus_ci]

    if (getOption("tabxplor.ci_print") == "moe") {
      out_ci <-
        paste0(print_num(out[plus_ci], digits[plus_ci]),
               dplyr::if_else(pct_ci[plus_ci], "%", ""),
               ci_print_trim(paste0(pm, sprintf(
                 paste0("%-0.",
                        digits[plus_ci] + dplyr::if_else(pct_ci[plus_ci] & digits[plus_ci] == 0, 1L, 0L),
                        "f"), dplyr::if_else(pct_ci[plus_ci], ci * 100, ci)
               )) ) %>% ci_html_subscript(html = html)
        )
    } else {
      lower <- dplyr::if_else(condition = mean_ci[plus_ci],
                              true      = pmax(out[plus_ci] - ci, 0),
                              false     = pmax(out[plus_ci] - ci * 100, 0) )

      upper <- dplyr::if_else(condition = mean_ci[plus_ci],
                              true      = out[plus_ci] + ci,
                              false     = pmin(out[plus_ci] + ci, 100) )
      out_ci <- dplyr::if_else(
        condition = is.na(lower) | is.na(upper) | round(lower, digits) == round(upper, digits),
        true      = print_num(out[plus_ci], digits[plus_ci]),
        false     = paste0(
          sprintf(paste0("%-0.", digits[plus_ci], "f"), lower),
          stringi::stri_unescape_unicode("\\u00b7"), # middle-point
          sprintf(paste0("%-0.", digits[plus_ci], "f"), upper)
        )
      )
      out_ci <- paste0(out_ci, dplyr::if_else(pct_ci[plus_ci], "%", ""))
    }
  }

  out[!na_out] <- print_num(out[!na_out], digits[!na_out])
  out[na_out] <- na
  if (any(plus_ci)) out[plus_ci] <- out_ci
  out[n_wn] <- out[n_wn] %>% prettyNum(big.mark = " ", preserve.width = "individual")
  out[pct_no_ci] <- paste0(out[pct_no_ci], "%") #pillar::style_subtle()

  out[type_ci] <- switch(
    type,
    "n"       = ,
    "mean"    = paste0(pm, out[type_ci]),
    "row"     = ,
    "col"     = ,
    "all"     = ,
    "all_tabs"= paste0(pm, out[type_ci], "%")
  )

  #out <- stringr::str_pad(out, max(stringr::str_length(out), na.rm = TRUE))
  out
}






#' Pillar_shaft method to print class fmt in a \code{\link[tibble:tibble]{tibble}} column
#'
#' @param x A fmt object.
#' @param ... Other parameter.
#'
#'
#' @return A fmt printed in a pillar.
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.tabxplor_fmt <- function(x, ...) {
  # print color type somewhere (and brk legend beneath ?) ----

  out     <- format(x)

  display <- get_display(x)
  nas  <- is.na(display)

  type <- get_type(x)
  comp <- get_comp_all(x)

  ci_type   <- get_ci_type(x)
  pct       <- get_type(x)
  color     <- get_color(x)

  totcol    <- is_totcol(x)
  totrows   <- is_totrow(x)
  tottabs   <- is_tottab(x)

  disp_diff <- display == "diff" & !nas
  disp_ci   <- display == "ci" & ci_type == "diff" & !nas
  disp_ctr  <- display == "ctr" & !nas

  if (any(disp_diff)) {
    ref     <- get_reference(x[disp_diff], mode = "cells")
    reffmt  <- set_display(x[disp_diff],
                           ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
      format() #%>% stringr::str_trim()
    out[disp_diff] <- dplyr::if_else(ref,
                                     paste0("ref:", reffmt),
                                     out[disp_diff])
  }

  if (any(disp_ci)) {
    ref     <- get_reference(x[disp_ci], mode = "cells")
    reffmt  <- set_display(x[disp_ci],
                           ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
      format()
    out[disp_ci] <- dplyr::if_else(ref,
                                   paste0("ref:x-", reffmt),
                                   out[disp_ci])
  }

  if (any(disp_ctr)) {
    mctr <- if (comp) {
      disp_ctr & totrows & tottabs & !totcol
    } else {
      disp_ctr & totrows & !totcol
    }
    out[mctr] <- paste0("mean:", stringr::str_trim(out[mctr])) %>%
      stringr::str_remove("mean:Inf%|NA")
  }

  if (color == "contrib" & !any(totrows)) warning(
    "cannot print color == 'contrib' with no total rows to store ",
    "information about mean contributions to variance"
  )  # store mean_contrib in a vctrs::field of fmt ? ----

  na_out  <- is.na(out)
  ok      <- !na_out & !nas


  if (!is.na(color) & ! color %in% c("no", "") & !(color == "contrib" & !any(totrows))) {
    color_selection <- fmt_color_selection(x)

    color_styles <- select_in_color_style(length(color_selection))

    color_styles <- get_color_style()[color_styles]

    unselected <- purrr::transpose(color_selection) %>%
      purrr::map_lgl(~ ! any(purrr::flatten_lgl(.)))

    out[ok] <-
      purrr::reduce2(.init = out[ok], .x = purrr::map(color_selection, ~ .[ok]),
                     .y = color_styles,
                     ~ dplyr::if_else(..2, rlang::exec(..3, ..1), ..1) )
    totals <- get_reference(x, mode = "all_totals") #c("cells", "lines")

    out[ok & unselected & !totals] <-  #fmtgrey3
      pillar::style_subtle(out[ok & unselected & !totals])

    #Columns with no color
  } else {
    # - problem with bold in console : it offsets all column unaesthetically

    # - use underline and | to make the imitate the borders of a table
    # if (any(totrows)) out <- dplyr::if_else(totrows & ! totcol,
    #                                         crayon::underline(out), out)
    # if (totcol)       out <- dplyr::if_else(totrows,
    #                                         paste0(crayon::underline(out), "|"),
    #                                         paste0(out, "|"))

    # # - normal cells a bit grayer to see the totals better
    # totals <- get_reference(x, mode = "all_totals")
    # out[ok & !totals] <- fmtgrey4(out[ok & !totals])

    out[ok] <- out[ok] %>%
      stringr::str_replace("^0%$|^-0%$", pillar::style_subtle("0%")) %>% # 0 in gray
      stringr::str_replace("^0$|^0$", pillar::style_subtle("0"))
  }

  pillar::new_pillar_shaft_simple(out, align = "right", na = "")
}

#' Print Chi2 tables columns
#' @param x A fmt object.
#' @param ... Other parameter.
#' @export
#' @return A Chi2 table column printed in a pillar.
# @keywords internal
# @method pillar_shaft tab_chi2_fmt
pillar_shaft.tab_chi2_fmt <- function(x, ...) {
  # print color type somewhere (and brk legend beneath ?) ----

  out     <- format(x)
  display <- get_display(x)
  nas     <- is.na(display)

  color_style <- get_color_style()

  pvalues <- out[!nas & display == "pct"]
  p_values <- get_num(x)[!nas & display == "pct"]

  out[!nas & display == "pct"] <-
    dplyr::if_else(condition = p_values >= 0.05,
                   true      = color_style$neg5(pvalues),
                   false     = color_style$pos5(pvalues) )

  pillar::new_pillar_shaft_simple(out, align = "right", na = "")
}


#' mutate method to access vctrs::fields of tabxplor_fmt vectors
#' @importFrom dplyr mutate
#' @method mutate tabxplor_fmt
#' @param .data A tabxplor_fmt column.
#' @param ... <[`data-masking`][dplyr_data_masking]> Name-value pairs.
#'   The name gives the name of the column in the output (do not change it).
#'
#'   The value can be:
#'
#'   * A vector of length 1, which will be recycled to the correct length.
#'   * A vector the same length as the current group (or the whole data frame
#'     if ungrouped).
#' @return An object of class \code{tabxplor_fmt}.
#' @export
mutate.tabxplor_fmt <- function(.data, ...) {
  dots <- rlang::enquos(...)

  .data |>
    vctrs::vec_proxy() |>
    dplyr::mutate(!!!dots, .keep = "all", .before = NULL, .after = NULL) |>
    vctrs::vec_restore(.data)
}

#' $ method for class tabxplor_fmt
#' @param x A tabxplor_fmt object.
#' @param name The name of the field to extract.
# @method `$` tabxplor_fmt
#' @return The relevant field of the tabxplor_fmt.
#' @export
`$.tabxplor_fmt` <- function(x, name) {
  dplyr::pull(vctrs::vec_proxy(x), name)
}

# #Problem : dplyr::last doesn't work anymore with fmt, because it relies on `[[`
# #' Extract method for class tabxplor_fmt
# #' @param x A tabxplor_fmt object.
# #' @param i,j,... Indices of names of the field to extract.
# #' @method `[[` tabxplor_fmt
# #' @return The relevant field of the tabxplor_fmt.
# #' @export
# `[[.tabxplor_fmt` <- function(x, i, j, ...) {
#  if (missing(j)) {
#    suppressWarnings(`[[`(vctrs::vec_proxy(x), i = i, ..., exact = TRUE))
#  } else {
#    suppressWarnings(`[[`(vctrs::vec_proxy(x), i = i, j = j, ..., exact = TRUE))
#  }
# }



#' @keywords internal
fmt_color_selection <- function(x, force_color, force_breaks) {
  type    <- get_type (x)
  type_ci <- get_ci_type(x)
  color   <- if (missing(force_color)) get_color(x) else force_color
  # color <- get_color(x)

  if (!missing(force_breaks)) {
    mean_breaks    <- force_breaks$mean_breaks
    pct_breaks     <- force_breaks$pct_breaks
    mean_ci_breaks <- force_breaks$mean_ci_breaks
    pct_ci_breaks  <- force_breaks$pct_ci_breaks
    contrib_breaks <- force_breaks$contrib_breaks
    mean_brksup    <- force_breaks$mean_brksup
    pct_brksup     <- force_breaks$pct_brksup
    mean_ci_brksup <- force_breaks$mean_ci_brksup
    pct_ci_brksup  <- force_breaks$pct_ci_brksup
    contrib_brksup <- force_breaks$contrib_brksup
  } else {
    tabxplor_color_breaks <- getOption("tabxplor.color_breaks")

    mean_breaks    <- tabxplor_color_breaks$mean_breaks
    pct_breaks     <- tabxplor_color_breaks$pct_breaks
    mean_ci_breaks <- tabxplor_color_breaks$mean_ci_breaks
    pct_ci_breaks  <- tabxplor_color_breaks$pct_ci_breaks
    contrib_breaks <- tabxplor_color_breaks$contrib_breaks
    mean_brksup    <- tabxplor_color_breaks$mean_brksup
    pct_brksup     <- tabxplor_color_breaks$pct_brksup
    mean_ci_brksup <- tabxplor_color_breaks$mean_ci_brksup
    pct_ci_brksup  <- tabxplor_color_breaks$pct_ci_brksup
    contrib_brksup <- tabxplor_color_breaks$contrib_brksup
  }

  diff <- if (color %in% c("diff", "diff_ci", "after_ci", "ci") ) {
    get_diff(x)
  } else {
    rep(NA_real_, length(x))  #vctrs::vec_recycle(NA_real_, length(x))
  }

  ci <- if (color %in% c("diff_ci", "after_ci", "ci") & type_ci == "diff" ) {
    get_ci(x)
  } else {
    NA_real_
  }

  ref_means <- if (color %in% c("diff_ci", "after_ci", "ci") & type == "mean") {
    get_ref_means(x)
  } else {
    NA_real_
  }

  ctr <- if (color == "contrib") {
    dplyr::if_else(is_totrow(x),
                   true  = NA_real_,
                   false =  get_ctr(x))
  } else {
    NA_real_
  }

  mean_ctr <- if (color == "contrib") {
    get_mean_contrib(x)
  } else {
    NA_real_
  }

  brk <-
    switch(color,
           "diff"     = ,
           "diff_ci"  = if (type == "mean") mean_breaks    else pct_breaks   ,
           "ci"       = if (type == "mean"){
             mean_ci_breaks[c(1, length(mean_ci_breaks)/2 + 1)]
           } else {
             pct_ci_breaks[c(1, length(pct_ci_breaks)/2 + 1)]
           } ,
           "after_ci" = if (type == "mean") mean_ci_breaks else pct_ci_breaks,
           "contrib"  = contrib_breaks                                     )

  brksup <-
    switch(color,
           "diff"     = ,
           "diff_ci"  = if (type == "mean") mean_brksup    else pct_brksup   ,
           "ci"       = if (type == "mean") {
             mean_ci_brksup[c(length(mean_ci_brksup)/2, length(mean_ci_brksup))]
           }  else {
             pct_ci_brksup[c(length(pct_ci_brksup)/2, length(pct_ci_brksup))]
           },
           "after_ci" = if (type == "mean") mean_ci_brksup else pct_ci_brksup,
           "contrib"  = contrib_brksup                                        )

  purrr::map2(brk, brksup,
              ~ color_formula(type = type, color = color,
                              diff = diff, ci = ci, ref_means = ref_means,
                              ctr = ctr, mean_ctr = mean_ctr,
                              brk = .x, brksup = .y)
  ) %>% purrr::set_names(as.character(round(brk, 2)))
}

# !means & brk >= 0 & brksup > 0
# ~ diff >= 0  &  abs(diff) - ci > brk   &  abs(diff) - ci < brksup,
#
# !means & brk <=  0 & brksup < 0
# ~ diff  < 0  &  abs(diff) - ci > -brk  &  abs(diff) - ci < -brksup),


# diff >= 1                                              &
#   (1 + abs(1 - diff) ) * ref_means  >  (ref_means + ci) * brk[1]   &
#   abs(1 - diff) * ref_means  <  (ref_means + ci) * brksup[1]
#
#
# ref_means + abs(1 - diff) * ref_means  > (ref_means + ci) * brk[1]
# ref_means + abs(1 - diff) * ref_means  > ref_means * brk[1] + ci * brk[1]
# abs(1 - diff) * ref_means > ref_means * brk[1] - ref_means + ci * brk[1]
# abs(1 - diff) * ref_means > ref_means * (brk[1] - 1) + ci * brk[1]
#
# (ref + ci) * brk -ref
#
# abs(1 - diff) > brk[1] - 1 + ci/ref_means * brk[1]
# abs(1 - diff) + 1 > brk[1] * (ci/ref_means + 1)
#
# (1 + abs(1 - diff)) * ref_means   > ref_means * brk[1] + ci * brk[1]
#

# ctr >= brk[1] * mean_ctr & ctr < brksup[1] * mean_ctr
# ctr >= brk[2] * mean_ctr & ctr < brksup[2] * mean_ctr


#' @keywords internal
color_formula <- function(type, color, diff, ci, ref_means,
                          ctr, mean_ctr, brk, brksup) {
  means <- type %in% c("mean", "n")

  res <-
    switch(
      color,
      "diff"     =
        if( (!means & brk >= 0) | (means & brk >= 1) ) {
          diff > brk & diff < brksup} else {
            diff < brk & diff > brksup},

      "diff_ci"  = dplyr::case_when(
        means & brk >= 1    ~ diff > brk & diff < brksup &
          abs(1 - diff) * ref_means - ci > 0,

        means & brk <  1    ~ diff < brk & diff > brksup &
          abs(1 - diff) * ref_means - ci > 0,

        !means & brk >= 0   ~ diff > brk & diff < brksup & abs(diff) - ci > 0,
        !means & brk <  0   ~ diff < brk & diff > brksup & abs(diff) - ci > 0 ),

      "ci"       = dplyr::case_when(
        means & (brk == 1)
        ~ diff >= 1  &  abs(1 - diff) * ref_means > ci,

        means & (brk == -1)
        ~ diff  < 1  &  abs(1 - diff) * ref_means > ci,

        !means & brk == 0 & brksup > 0
        ~ diff >= 0  &  abs(diff) > ci,

        !means & brk == 0 & brksup < 0
        ~ diff  < 0  &  abs(diff) > ci
      ),

      "after_ci" = dplyr::case_when(
        means & brk > 0
        ~ diff >= 1                                                         &
          ref_means + abs(1 - diff) * ref_means  > (ref_means + ci) * brk   &
          ref_means + abs(1 - diff) * ref_means  < (ref_means + ci) * brksup ,
        #wrong : abs(1 - diff) > ci * brk
        #wrong : abs(1 - diff) * ref_means  >  (ref_means + ci) * brk

        means & brk < 0
        ~ diff < 1                                                           &
          ref_means + abs(1 - diff) * ref_means  > (ref_means + ci) * -brk   &
          ref_means + abs(1 - diff) * ref_means  < (ref_means + ci) * -brksup ,

        !means & brk >= 0 & brksup > 0
        ~ diff >= 0  &  abs(diff) - ci > brk   &  abs(diff) - ci < brksup,

        !means & brk <= 0 & brksup < 0
        ~ diff  < 0  &  abs(diff) - ci > -brk  &  abs(diff) - ci < -brksup),

      "contrib"     = if (brk >= 0) {
        ctr >= brk * mean_ctr & ctr < brksup * mean_ctr
      } else {
        ctr <= brk * mean_ctr & ctr > brksup * mean_ctr
      },

      rep(FALSE, length(diff))
    )

  tidyr::replace_na(res, FALSE)
}

#' @keywords internal
tab_color_legend <- function(x, colored = TRUE, mode = c("console", "html"),
                             html_theme = NULL, html_type = NULL, html_24_bit,
                             text_color = NULL, grey_color = NULL,
                             add_color_and_diff_types = FALSE, all_variables_names = FALSE) {
  color     <- get_color(x)

  x <- x[!is.na(color) & !color %in% c("no", "")]

  color <- get_color(x)

  type      <- get_type(x)
  diff_type <- get_diff_type(x)
  col_vars_levels <- tab_get_vars(x)$col_vars_levels %>%
    purrr::discard(names(.) == "all_col_vars")

  color_type <- col_vars_levels %>%
    purrr::map(~ get_color_type(color[.], type[.]) %>%
                 purrr::flatten_chr()) %>%
    purrr::map_chr(~ dplyr::if_else(
      condition = length(unique(.x)) == 1,
      true      = .x[1],
      false     = "mixed"                 )
    )

  diff_type <- col_vars_levels %>%
    purrr::map(~ diff_type[.]) %>%
    purrr::map_chr(~ dplyr::if_else(
      condition = length(unique(.x)) == 1,
      true      = .x[1],
      false     = "mixed"                 )
    )

  if (all(is.na(color_type) | color_type %in% c("", "no"))) return(NULL)

  cross <- stringi::stri_unescape_unicode("\\u00d7")

  breaks_with_op <- function(breaks, color_type) purrr::map_chr(
    breaks,
    ~ switch(
      color_type,
      "diff_mean"     = ,
      "diff_ci_mean"  = dplyr::if_else(
        condition = stringr::str_detect(.x, "^-"),
        true      = paste0("/", stringr::str_remove(.x, "^-")),
        false     = paste0(cross, .x) # sign * in cross
      ),
      "diff"          = ,
      "diff_ci"       = ,
      "after_ci"      = dplyr::if_else(
        condition = stringr::str_detect(.x, "^-"),
        true      = .x,
        false     = paste0("+", .x)
      ),
      "after_ci_mean" = paste0(cross, stringr::str_remove(.x, "^-")),
      "contrib"       = paste0(cross, stringr::str_remove(.x, "^-")),
      #"ci_mean"       = ,
      "ci"            = "",      #just 1 ?
      .x
    ) )

  color_formula_chr <- function(color_type, ref, sign, breaks, mode = "console") {
    if (mode == "console") {
      purrr::map2_chr(breaks, sign, function(.breaks, .sign)
        switch(
          color_type,
          "diff_mean"     = , # 1/mean and sign /
          "diff"          = paste0("x", .sign, ref, " ", .breaks),
          "diff_ci_mean"  = ,
          "diff_ci"       = paste0("|x-", ref, "|>ci & x", .sign,
                                   ref, " ", .breaks),
          #"ci_mean"       = ,
          "ci"            = paste0("|x-", ref, "| > ci"),      #just 1 ?
          "after_ci_mean" = paste0(ref, " + |x-", ref, "| > (", ref, " + ci) ", .breaks),
          "after_ci"      = paste0("|x-", ref, "| > ci ", .breaks), #+ -
          "contrib"       = paste0("contrib > mean_ctr "     , .breaks),
          character()
        ))

    } else { # mode == "html"
      purrr::map2_chr(breaks, sign, function(.breaks, .sign)
        switch(
          color_type,
          "diff_mean"     = , # 1/mean and sign /
          "diff"          = paste0("x", .sign, ref, " ", "<b>", .breaks, "</b>"),
          "diff_ci_mean"  = ,
          "diff_ci"       = paste0("|x-", ref, "|>ci & x", .sign,
                                   ref, " ","<b>", .breaks, "</b>"),
          #"ci_mean"       = ,
          "ci"            = paste0("|x-", ref, "| > ci"),      #just 1 ?
          "after_ci_mean" = paste0(ref, " + |x-", ref, "| > (", ref, " + ci) ", "<b>",
                                   .breaks, "</b>"),
          "after_ci"      = paste0("|x-", ref, "| > ci ", "<b>", .breaks, "</b>"), #+ -
          "contrib"       = paste0("contrib > mean_ctr ", "<b>", .breaks, "</b>"),
          character()
        ))
    }

  }

  color_table <-
    tibble::tibble(color_type, diff_type, names = names(color_type)) %>%
    dplyr::filter(!is.na(.data$color_type) & !.data$color_type %in% c("no", "")) %>%
    dplyr::group_by(.data$color_type)

  if (all_variables_names == FALSE) {
    color_table <- color_table |>
      dplyr::mutate(etc = dplyr::if_else(dplyr::row_number() >= 3, ",...", "") ) %>%
      dplyr::slice(1:3)
  } else {
    color_table <- color_table |> dplyr::mutate(etc =  "")
  }

  color_table <- color_table |>
    dplyr::summarise(names = paste0(.data$names, collapse = ", "),
                     etc = dplyr::first(.data$etc),
                     diff_type = dplyr::first(.data$diff_type),
                     .groups = "drop") %>%
    dplyr::mutate(names = paste0(.data$names, .data$etc))

  if (add_color_and_diff_types) {
    color_table <- color_table |>
      dplyr::mutate(names = paste0(
        "[color:", .data$color_type, "] ",
        dplyr::if_else(color_type %in% c("diff_mean", "diff", "diff_ci_mean",
                                         "diff_ci", "after_ci_mean", "after_ci"),
          true  = paste0("[diff:", .data$diff_type, "] "),
          false = ""),
        .data$names
      ))
  }

  if (colored == TRUE) color_table <- color_table %>%
    dplyr::mutate(names = stringr::str_pad(.data$names,
                                           max(stringr::str_length(.data$names)),
                                           side = "right"))

  color_table <- color_table %>%
    dplyr::mutate(
      breaks = brk_from_color(.data$color_type),
      breaks = dplyr::if_else(.data$color_type %in% c("diff_mean", "diff_ci_mean"),
                              true  = purrr::map(.data$breaks,
                                                 ~ .[1:(length(.)/2)] %>%
                                                   c(., purrr::map(., `-`))
                              ),
                              false = .data$breaks),
      breaks = dplyr::if_else(
        condition = .data$color_type %in% c("diff", "diff_ci", "after_ci"),
        true      = purrr::map(.data$breaks,
                               ~ paste0(sprintf("%1.0f",
                                                purrr::map_dbl(., ~ .[1]) * 100),
                                        "%") ),
        false     = purrr::map(.data$breaks,
                               ~ sprintf("%1.3g", purrr::map_dbl(., ~ .[1]))),
      ) %>%
        purrr::map2(.data$color_type, ~ breaks_with_op(.x, .y)),
      ref  = purrr::map_chr(.data$diff_type, ~ if (is.na(suppressWarnings(as.integer(.)))) {
        switch(., "first" = "x1", "tot" = "tot", .data$diff_type)
      } else {paste0("x", as.integer(.))} ),
      sign = purrr::map(.data$breaks, ~ 1:length(.)) %>%
        purrr::map(~ dplyr::if_else(condition = . >= max(.)/2 +1,
                                    true      = " < ",
                                    false     = " > ")),
    )

  if (colored == TRUE & mode[1] == "console") color_table <- color_table %>%
    dplyr::mutate(
      styles = purrr::map(.data$breaks, ~ select_in_color_style(length(.))),
      styles = purrr::map(.data$styles, ~ get_color_style()[.]),
      breaks = purrr::map2(.data$styles, .data$breaks,
                           ~ purrr::map2_chr(
                             .x, .y,
                             ~ rlang::exec(.x, rlang::sym(.y))
                           ))
    )

  if (colored == TRUE & mode[1] == "html") {
    if (html_type == "text") {
      color_table <- color_table %>%
        dplyr::mutate(
          styles = purrr::map(.data$breaks, ~ select_in_color_style(length(.))),
          styles = purrr::map(.data$styles, ~ get_color_style(mode  = "color_code",
                                                              theme = html_theme,
                                                              type  = html_type,
                                                              html_24_bit = html_24_bit)[.]),
          breaks = purrr::map2(.data$styles, .data$breaks,
                               ~ purrr::map2_chr(
                                 .x, .y,
                                 ~ kableExtra::text_spec(.y, color = .x)
                               ))
        )
    } else {
      color_table <- color_table %>%
        dplyr::mutate(
          styles = purrr::map(.data$breaks, ~ select_in_color_style(length(.))),
          styles = purrr::map(.data$styles, ~ get_color_style(mode  = "color_code",
                                                              theme = html_theme,
                                                              type  = html_type)[.]),
          breaks = purrr::map2(.data$styles, .data$breaks,
                               ~ purrr::map2_chr(
                                 .x, .y,
                                 ~ kableExtra::text_spec(.y, background = .x)
                               ))
        )
    }
  }


  color_table <- color_table %>%
    dplyr::mutate(
      color_scale = list(.data$color_type, .data$ref, .data$sign, .data$breaks,
                         purrr::map(.data$breaks, ~ 1:length(.))) %>%
        purrr::pmap(~ dplyr::if_else(
          condition = ..5 %in% c(1, max(..5)/2 + 1),
          true      = color_formula_chr(color_type = ..1, ref = ..2,
                                        sign = ..3, breaks = ..4, mode = mode[1]),
          false     = if (mode[1] == "console") { ..4 } else { paste0("<b>", ..4, "</b>")}
        ))
    )

  if (colored == TRUE) {
    if (mode[1] == "console") {
      color_table <- color_table %>%
        dplyr::mutate(
          color_scale = purrr::map_chr(.data$color_scale, ~ paste0(
            .,
            collapse = pillar::style_subtle("; ")          )
          ),
          names = pillar::style_subtle(paste0(.data$names, ": "))
        )

    } else {
      color_table <- color_table %>%
        dplyr::mutate(
          color_scale = purrr::map_chr(.data$color_scale, ~ paste0(., collapse = "; ")
          ), #%>% stringr::str_replace_all(";", kableExtra::text_spec(";", color = grey_color)),
          names = paste0(.data$names, ": ") %>% kableExtra::text_spec(color = grey_color)
        )
    }

  } else {
    color_table <- color_table %>%
      dplyr::mutate(
        color_scale = purrr::map_chr(.data$color_scale, ~ paste0(., collapse = "; " )),
        names = paste0(.data$names, ": ")
      )
  }

  paste0(color_table$names, color_table$color_scale)
  # %>% cli::cat_line()
  # stringr::str_remove("\\+0.0+")
}
# tab_color_legend(tabs[[7]]) %>% cli::cat_line()
# tab_color_legend(tabs[[7]], colored = FALSE)

#' @keywords internal
brk_from_color <- function(color_type) {
  tabxplor_color_breaks <- getOption("tabxplor.color_breaks")

  purrr::map(color_type, ~
               switch(.x,
                      "diff_mean"     = ,
                      "diff_ci_mean"  = list(tabxplor_color_breaks$mean_breaks,
                                             tabxplor_color_breaks$mean_brksup),
                      "after_ci_mean" = list(tabxplor_color_breaks$mean_ci_breaks,
                                             tabxplor_color_breaks$mean_ci_brksup),
                      "diff"          = ,
                      "diff_ci"       = list(tabxplor_color_breaks$pct_breaks,
                                             tabxplor_color_breaks$pct_brksup),
                      "after_ci"      = list(tabxplor_color_breaks$pct_ci_breaks,
                                             tabxplor_color_breaks$pct_ci_brksup),
                      "contrib"       = list(tabxplor_color_breaks$contrib_breaks,
                                             tabxplor_color_breaks$contrib_brksup),
                      "ci"            = ,
                      "ci_mean"       = list(0, Inf), #list(c(0, 0), c(Inf, -Inf)),
                      list()
               ) %>%
               purrr::transpose() %>%
               purrr::map(purrr::flatten_dbl)
  )
}

#' @keywords internal
get_color_type <- function(color, type) {
  purrr::map2(color, type, ~ dplyr::case_when(
    .x == "contrib" ~ "contrib",
    .x == "ci"      ~ "ci"     ,

    .x %in% c("diff", "diff_ci", "after_ci") & .y == "mean"
    ~ paste0(.x, "_mean"),

    .x %in% c("diff", "diff_ci", "after_ci") &
      .y %in% c("row", "col", "all", "all_tabs")
    ~ .x

  ) %>% purrr::set_names(names(.x)))
}

#' @keywords internal
select_in_color_style <- function(length) {

  color_code_pos1 <- get_color_style()$pos1 |> attr("_styles") |> names()


  if (stringr::str_detect(color_code_pos1, "#CCFFCC|#000033e")) {
    switch(as.character(length),
           "1"  = c(3)              ,
           "2"  = c(3, 8)           ,
           "4"  = c(1, 3, 6, 8)     ,
           "6"  = c(1, 3, 5, 6, 8, 10),
           "8"  = c(1:3, 4, 6:8, 10),
           "10" = 1:10               )
  } else {
    switch(as.character(length),
           "1"  = c(3)              ,
           "2"  = c(3, 8)           ,
           "4"  = c(3, 5, 8, 10)    ,
           "6"  = c(3:5, 8:10)      ,
           "8"  = c(2:5, 7:10)      ,
           "10" = 1:10               )
  }
}


# brk_from_color <- function(color, type) {
#   means <- type == "mean"
#   purrr::map2(color, means,
#               ~ switch(
#                 .x,
#                 "diff"          = ,
#                 "diff_ci"       = if (.y) {
#                   list(mean_breaks, mean_brksup)
#                 } else {
#                   list(pct_breaks, pct_brksup)
#                 },
#                 "after_ci"      = if (.y) {
#                   list(mean_ci_breaks, mean_ci_brksup)
#                 } else {
#                   list(pct_ci_breaks, pct_ci_brksup)
#                 },
#                 "contrib"       = list(contrib_breaks, contrib_brksup),
#                 list()
#               ) %>%
#                 purrr::transpose() %>%
#                 purrr::map(purrr::flatten_chr)
#   )
# }

#' @keywords internal
get_reference <- function(x, mode = c("cells", "lines", "all_totals")) {
  type        <- get_type(x)
  diff_type   <- get_diff_type(x)
  comp_all    <- get_comp_all(x)
  totcol      <- is_totcol(x)
  totrows     <- is_totrow(x)
  tottab_line <- is_tottab(x) & totrows

  refrows     <- is_refrow(x)
  refcol      <- is_refcol(x)
  tottab_ref  <- is_tottab(x) & refrows

  if (diff_type == "tot") {
    switch(mode[1],
           "cells"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ totrows & !totcol     ,
             type %in% c("row", "mean") &  comp_all ~ tottab_line & !totcol ,
             type == "col"                          ~ totcol & !totrows     ,
             type == "all"                          ~ totrows & totcol      ,
             type == "all_tabs"                     ~ tottab_line & totcol  ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "lines"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ totrows               ,
             type %in% c("row", "mean") &  comp_all ~ tottab_line           ,
             type == "col"                          ~ rep(totcol, length(x)),
             type == "all"                          ~ totrows & totcol      ,
             type == "all_tabs"                     ~ tottab_line & totcol  ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "all_totals" = dplyr::case_when(
             type %in% c("n", "col", "all") |
               (type %in% c("row", "mean") & !comp_all)
             ~ totrows | totcol,

             type == "all_tabs" | (type %in% c("row", "mean") &  comp_all)
             ~ tottab_line | totcol,
             # type == "col"                          ~ rep(totcol, length(x)),
             # type == "all"                          ~ totrows & totcol      ,
             # type == "all_tabs"                     ~ tottab_line & totcol  ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           )
    )
  } else {
    switch(mode[1],
           "cells"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ refrows & !totcol     ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref & !totcol  ,
             type == "col"                          ~ refcol & !totrows     ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "lines"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ refrows               ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref            ,
             type == "col"                          ~ rep(refcol, length(x)),
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "all_totals" = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ refrows | totcol      ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref | totcol   ,
             type == "col"                          ~ totrows | refcol      ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           )
    )

  }
}






# is_RStudio <- function() Sys.getenv("RSTUDIO") == "1"
# #.Platform$GUI == "RStudio"
#
# is_dark <- ifelse(is_RStudio(), rstudioapi::getThemeInfo()$dark, FALSE)

# format.pillar_shaft_fmt <- function(x, width, ...) {
#   if (get_max_extent(x$deg_min) <= width) {
#     ornament <- x$deg_min
#   } else {
#     ornament <- x$deg
#   }
#
#   pillar::new_ornament(ornament, align = "right")
# }



#' Abbreviated display name for class fmt in tibbles
#' @param x A fmt object.
#' @param ... Other parameter.
#' @return A single string with abbreviated fmt type.
#' @export
vec_ptype_abbr.tabxplor_fmt <- function(x, ...) {
  display  <- get_display(x) %>% unique()
  display  <- ifelse(length(display) > 1, "mixed", display)
  type     <- get_type(x)
  row_mean <- type %in% c("row", "mean")
  if (type %in% c("row", "col", "all", "all_tabs")) type <- paste0(type, "%")
  ci <- get_ci_type(x)
  if (display == "ci" & ci %in% c("cell", "diff")) display <- paste0("ci_", ci)

  out <- paste0(type, "-", display) %>%
    stringr::str_replace("^n-n", "n") %>%
    stringr::str_replace("^mean-mean", "mean") %>%
    stringr::str_replace("^mixed-mixed", "mixed") %>%
    stringr::str_replace("([^%]+%)-pct", "\\1") %>%
    stringr::str_remove("^NA") %>%
    stringr::str_remove("_ci$")
  #if (get_comp_all(x)) out <- paste0(out, "-all")

  out
}


#' Printed type for class fmt
#' @param x A fmt object.
#' @param ... Other parameter.
#' @return A single string with full fmt type.
#' @export
vec_ptype_full.tabxplor_fmt <- function(x, ...) {
  display  <- get_display(x) %>% unique()
  display  <- ifelse(length(display) > 1, "mixed", display)
  type     <- get_type(x)
  row_mean <- type %in% c("row", "mean")
  if (type %in% c("row", "col", "all", "all_tabs")) type <- paste0(type, "%")
  ci <- get_ci_type(x)
  if (display == "ci" & ci %in% c("cell", "diff")) display <- paste0("ci_", ci)

  out <- paste0("fmt-", type, "-", display) %>%
    stringr::str_replace("-n-n", "-n") %>%
    stringr::str_replace("-mean-mean", "-mean") %>%
    stringr::str_replace("-mixed-mixed", "-mixed") %>%
    stringr::str_replace("([^%]+%)-pct", "\\1") %>%
    stringr::str_remove("-NA") %>%
    stringr::str_remove("_ci$")
  #if (get_comp_all(x)) out <- paste0(out, "-all")

  out
}
# x <- fmt(7, "row", pct = 0.6)
# x %>% vec_data()
# x %>% attributes()

#Coertion and convertion methods for formatted numbers -------------------------

#Make our tabxplor_fmt class coercible with herself, and back and forth with double and
# integer vectors :
#' Find common ptype between fmt and fmt
#' @param x A fmt object.
#' @param y A fmt object.
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
vec_ptype2.tabxplor_fmt.tabxplor_fmt    <- function(x, y, ...) {
  type_x       <- get_type(x)
  same_type    <- type_x == get_type(y)
  comp_x       <- get_comp_all(x, replace_na = FALSE)
  comp_y       <- get_comp_all(y, replace_na = FALSE)
  same_comp    <- comp_x == comp_y | (is.na(comp_x) & is.na(comp_y))
  diff_type_x  <- get_diff_type(x)
  same_diff_type <- diff_type_x == get_diff_type(y)
  ci_type_x    <- get_ci_type(x)
  same_ci_type <- ci_type_x == get_ci_type(y)
  col_var_x    <- get_col_var(x)
  same_col_var <- col_var_x == get_col_var(y)
  totcol_x     <- is_totcol(x)
  same_totcol  <- totcol_x == is_totcol(y)
  refcol_x     <- is_refcol(x)
  same_refcol  <- refcol_x == is_refcol(y)
  color_x      <- get_color(x)
  same_color   <- color_x == get_color(y)
  #l            <- length(x)

  new_fmt(
    type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
    comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
    diff_type= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
    ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
    col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
    totcol   = dplyr::if_else(same_totcol , totcol_x , FALSE         ),
    refcol   = dplyr::if_else(same_refcol , refcol_x , FALSE         ),
    color    = dplyr::if_else(same_color  , color_x  , ""            )
  )
  # new_fmt(
  #   type     = dplyr::if_else(same_type, true  = type_x,
  #                             false = "mixed"),
  #   comp_all = dplyr::if_else(same_comp,
  #                             true  = comp_x,
  #                             false = vctrs::vec_recycle(FALSE, l )),
  #   ci_type  = dplyr::if_else(same_ci_type,
  #                             true  = ci_type_x,
  #                             false = vctrs::vec_recycle(NA_character_, l )),
  #   col_var  = dplyr::if_else(same_col_var,
  #                             true  = col_var_x,
  #                             false = vctrs::vec_recycle("several_vars", l )),
  #   totcol   = dplyr::if_else(same_totcol,
  #                             true  = totcol_x,
  #                             false = vctrs::vec_recycle(FALSE, l )),
  #   color    = dplyr::if_else(same_color,
  #                             true  = color_x,
  #                             false = vctrs::vec_recycle(NA_character_, l))
  # )
}
#' Find common ptype between fmt and double
#' @param x A fmt vector
#' @param y A double vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
vec_ptype2.tabxplor_fmt.double  <- function(x, y, ...) x # new_fmt() #double()
#' Find common ptype between double and fmt
#' @param x A double vector
#' @param y A fmt vector
#' @param ... Other parameter.
#' #' @return A fmt vector
#' @export
vec_ptype2.double.tabxplor_fmt  <- function(x, y, ...) y # new_fmt() #double()
#' Find common ptype between fmt and integer
#' @param x A fmt vector
#' @param y An integer vector
#' @param ... Other parameter.
#' #' @return A fmt vector
#' @export
vec_ptype2.tabxplor_fmt.integer <- function(x, y, ...) x # fmt() #double()
#' Find common ptype between integer and fmt
#' @param x An integer vector
#' @param y A fmt vector
#' @param ... Other parameter.
#' #' @return A fmt vector
#' @export
vec_ptype2.integer.tabxplor_fmt <- function(x, y, ...) y # new_fmt() #double()

# Conversions :
#' Convert fmt into fmt
#' @param x A fmt vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' #' @return A fmt vector
#' @export
vec_cast.tabxplor_fmt.tabxplor_fmt  <- function(x, to, ...)
  new_fmt(display   = get_display (x),
          n         = get_n       (x),
          wn        = get_wn      (x),
          pct       = get_pct     (x),
          diff      = get_diff    (x),
          digits    = get_digits  (x),
          ctr       = get_ctr     (x),
          mean      = get_mean    (x),
          var       = get_var     (x),
          ci        = get_ci      (x),

          in_totrow = is_totrow   (x),
          in_refrow = is_refrow   (x),
          in_tottab = is_tottab   (x),

          type      = get_type    (to),
          comp_all  = get_comp_all(to, replace_na = FALSE),
          diff_type = get_diff_type(to),
          ci_type   = get_ci_type (to),
          col_var   = get_col_var (to),
          totcol    = is_totcol   (to),
          refcol    = is_refcol   (to),
          color     = get_color   (to)

  )

#' Convert double into fmt
#' @param x A double vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' #' @return A fmt vector
#' @export
vec_cast.tabxplor_fmt.double   <- function(x, to, ...)
  fmt(n = NA_integer_            ,
      display   = "wn", wn = x     ,
      type      = get_type    (to),
      comp_all  = get_comp_all(to, replace_na = FALSE),
      diff_type = get_diff_type(to),
      ci_type   = get_ci_type (to),
      col_var   = get_col_var (to),
      totcol    = is_totcol   (to),
      refcol    = is_refcol   (to),
      color     = get_color   (to),

  )
#' Convert fmt into double
#' @param x A fmt vector
#' @param to A double vector
#' @param ... Other parameter.
#' @return A double vector
#' @method vec_cast.double tabxplor_fmt
#' @export
vec_cast.double.tabxplor_fmt  <- function(x, to, ...) get_num(x) %>% as.double() #vctrs::field(x, "pct")

#' Convert integer into fmt
#' @param x A integer vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
vec_cast.tabxplor_fmt.integer <- function(x, to, ...)
  fmt(n        = x               ,
      type     = get_type    (to),
      comp_all = get_comp_all(to, replace_na = FALSE),
      diff_type = get_diff_type(to),
      ci_type  = get_ci_type (to),
      col_var  = get_col_var (to),
      totcol   = is_totcol   (to),
      refcol    = is_refcol   (to),
      color    = get_color   (to)

  ) #new_fmt(pct = as.double(x))
#' Convert fmt into integer
#' @param x A integer vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' #' @return An integer vector
#' @method vec_cast.integer tabxplor_fmt
#' @export
vec_cast.integer.tabxplor_fmt    <- function(x, to, ...) get_num(x) %>% as.integer() #vctrs::field(x, "pct") %>% as.integer()

#' Convert fmt into character
#' @param x A fmt vector
#' @param to A character vector
#' @param ... Other parameter
#' #' @return A character vector
#' @method vec_cast.character tabxplor_fmt
#' @export
vec_cast.character.tabxplor_fmt  <- function(x, to, ...) format(x)

#Comparisons and sorting :
#' Test equality with fmt vector
#' @param x A fmt vector
#' @param ... Other parameter
#' @return A double vector
#' @export
vec_proxy_equal.tabxplor_fmt   <- function(x, ...) {
  get_num(x)
}
#' Compare with fmt vector
#' @param x A fmt vector
#' @param ... Other parameter
#' @return A double vector
#' @export
vec_proxy_compare.tabxplor_fmt <- function(x, ...) {
  get_num(x)
}

#Once you've implemented vec_ptype2() and vctrs::vec_cast(), you get vec_c(), [<-, and [[<- implementations for free.
#You'll also get mostly correct behaviour for c().


#Arithmetic operations :

# Thank you very much it works perfectly (I had tried with ```@method```, but not consistently enougth to put it in the generic) !
# Just a detail : with ```vec_arith tabxplor_fmt  default``` , I have a "Warning: [D:\... ] @method  can have at most 2 words"
# I replaced with ```vec_arith.tabxplor_fmt default``` and it worked.

#' Vec_arith method for fmt
#' @param op Operation to do.
#'
#' @param x fmt object.
#' @param y Second object.
#' @param ... Other parameter.
#'
#' @return A fmt vector
#' @method vec_arith tabxplor_fmt
#' @export
vec_arith.tabxplor_fmt <- function(op, x, y, ...) {
  UseMethod("vec_arith.tabxplor_fmt", y)
}

#' @describeIn vec_arith.tabxplor_fmt default vec_arith method for fmt
# @return A fmt vector
#' @method vec_arith.tabxplor_fmt default
#' @export
vec_arith.tabxplor_fmt.default <- function(op, x, y, ...) {
  vctrs::vec_arith_base(op, get_num(x), vctrs::vec_data(y))
  #stop_incompatible_op(op, x, y)
}

# positive_double <- function(n) n * sign(n)
# positive_integer <- function(n) as.integer(n * sign(n))

#' @describeIn vec_arith.tabxplor_fmt vec_arith method for fmt + fmt
# @return A fmt vector
#' @method vec_arith.tabxplor_fmt tabxplor_fmt
#' @export
vec_arith.tabxplor_fmt.tabxplor_fmt <- function(op, x, y, ...) {
  type_x       <- get_type(x)
  same_type    <- type_x == get_type(y)
  comp_x       <- get_comp_all(x, replace_na = FALSE)
  comp_y       <- get_comp_all(y, replace_na = FALSE)
  same_comp    <- comp_x == comp_y | (is.na(comp_x) & is.na(comp_y))
  diff_type_x  <- get_diff_type(x)
  same_diff_type <- diff_type_x == get_diff_type(y)
  ci_type_x    <- get_ci_type(x)
  same_ci_type <- ci_type_x == get_ci_type(y)
  col_var_x    <- get_col_var(x)
  same_col_var <- col_var_x == get_col_var(y)
  l            <- length(x)
  rep_NA_real  <- rep(NA_real_, l)

  if (!same_type) warning("operation ", op,
                          " over columns with different pct types, ",
                          "or mixing pct and means (",
                          type_x, "/", get_type(y), ")")
  if (!same_comp) warning("operation ", op,
                          " may mix calculations made on tabs and calculations ",
                          "made on all tabs (different 'comp_all')")
  if (!same_col_var) warning("operation ", op,
                             " over columns belonging to different variables(",
                             col_var_x , "/", get_col_var(y), ")")

  switch(
    op,
    "+" = ,
    "-" = new_fmt(
      display = get_display(x),      #dplyr::if_else(get_display(x) == get_display(x)), true = get_display(x), false = "n),
      n       = vctrs::vec_arith_base(op, get_n(x)  , get_n(y)  ), #%>% positive_integer(),
      wn      = vctrs::vec_arith_base(op, get_wn(x) , get_wn(y) ), #%>% positive_double(),
      pct     = if (same_type & !type_x %in% c("col", "mean", "n") ) {
        tidyr::replace_na(vctrs::vec_arith_base(op, get_pct(x), get_pct(y)), NA_real_)
      } else {
        rep_NA_real
      },
      diff    = rep_NA_real,
      digits  = pmax(get_digits(x), get_digits(y)),
      ctr     = rep_NA_real, # ???
      mean    = vctrs::vec_arith_base(op, get_mean(x) * get_wn(x), get_mean(y) * get_wn(y)) /
        vctrs::vec_arith_base("+", get_wn(x) , get_wn(y) ),# weighted mean
      var     = rep_NA_real,
      ci      = rep_NA_real,

      in_totrow = is_totrow(x) & is_totrow(y), # Just x ?
      in_refrow = is_refrow(x) & is_refrow(y),
      in_tottab = is_tottab(x) & is_tottab(y),

      type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
      comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
      diff_type= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
      ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
      col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
      totcol   = FALSE                                                  ,
      refcol   = FALSE                                                  ,
      color    = get_color(x)

      # type     = dplyr::if_else(same_type,
      #                           true  = type_x,
      #                           false = vctrs::vec_recycle("mixed", l )),
      # comp_all = dplyr::if_else(same_comp,
      #                           true  = comp_x,
      #                           false = vctrs::vec_recycle(FALSE, l )),
      # ci_type  = dplyr::if_else(same_ci_type,
      #                           true  = ci_type_x,
      #                           false = vctrs::vec_recycle(NA_character_, l )),
      # col_var  = dplyr::if_else(same_col_var,
      #                           true  = col_var_x,
      #                           false = vctrs::vec_recycle("several_vars", l )),
    ),
    "/" = ,
    "*" = new_fmt(
      display   = get_display(x),
      n      = get_n(x)   ,
      wn     = get_wn(x)  ,
      pct    = vctrs::vec_arith_base(op, get_pct(x), get_pct(y)), #Remove multiplication ?
      diff   = rep_NA_real,
      digits = pmax(get_digits(x), get_digits(y)),
      ctr    = rep_NA_real,
      mean   = rep_NA_real,
      var    = rep_NA_real,
      ci     = rep_NA_real,

      in_totrow = is_totrow(x),
      in_refrow = is_refrow(x),
      in_tottab = is_tottab(x),

      type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
      comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
      diff_type= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
      ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
      col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
      totcol   = FALSE                                                  ,
      refcol   = FALSE                                                  ,
      color    = get_color(x)

      # type     = dplyr::if_else(same_type,
      #                           true  = type_x,
      #                           false = vctrs::vec_recycle("mixed", l )),
      # comp_all = dplyr::if_else(same_comp,
      #                           true  = comp_x,
      #                           false = vctrs::vec_recycle(FALSE, l )),
      # ci_type  = dplyr::if_else(same_ci_type,
      #                           true  = ci_type_x,
      #                           false = vctrs::vec_recycle(NA_character_, l )),
      # col_var  = dplyr::if_else(same_col_var,
      #                           true  = col_var_x,
      #                           false = vctrs::vec_recycle("several_vars", l )),
    ),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @describeIn vec_arith.tabxplor_fmt vec_arith method for fmt + numeric
# @return A fmt vector
#' @method vec_arith.tabxplor_fmt numeric
#' @export
vec_arith.tabxplor_fmt.numeric <- function(op, x, y, ...) {
  set_num(x, vctrs::vec_arith_base(op, get_num(x), y))
  # new_fmt(pct    = vec_arith_base(op, vctrs::field(x, "pct"), y),
  #          display   = vctrs::field(x, "display"  ),
  #          digits = vctrs::field(x, "digits"),
  #          n      = vctrs::field(x, "n"     ),
  #          wn     = vctrs::field(x, "wn"    ),
  #          var     = vctrs::field(x, "var"    ),
  #          ci     = vctrs::field(x, "ci"    )                     )
}

#' @describeIn vec_arith.tabxplor_fmt vec_arith method for numeric + fmt
# @return A fmt vector
#' @method vec_arith.numeric tabxplor_fmt
#' @export
vec_arith.numeric.tabxplor_fmt <- function(op, x, y, ...) {
  set_num(y, vctrs::vec_arith_base(op, x, get_num(y)))
  # new_fmt(pct    = vec_arith_base(op, x, vctrs::field(y, "pct")),
  #          display   = vctrs::field(y, "display"  ),
  #          digits = vctrs::field(y, "digits"),
  #          n      = vctrs::field(y, "n"     ),
  #          wn     = vctrs::field(y, "wn"    ),
  #          var     = vctrs::field(y, "var"    ),
  #          ci     = vctrs::field(y, "ci"    )                     )
}

#' @describeIn vec_arith.tabxplor_fmt vec_arith method for -fmt
# @return A fmt vector
#' @method vec_arith.tabxplor_fmt MISSING
#' @export
vec_arith.tabxplor_fmt.MISSING <- function(op, x, y, ...) { #unary + and - operators
  switch(op,
         `-` = set_num(x, get_num(x) * -1),
         # new_fmt(pct    = vctrs::field(x, "pct"   ) * -1,
         #              display   = vctrs::field(x, "display"  ),
         #              digits = vctrs::field(x, "digits"),
         #              n      = vctrs::field(x, "n"     ),
         #              wn     = vctrs::field(x, "wn"    ),
         #              var     = vctrs::field(x, "var"    ),
         #              ci     = vctrs::field(x, "ci"    )       ),
         `+` = x,
         vctrs::stop_incompatible_op(op, x, y)
  )
}


#Mathematical operations :
# (direct operations on counts,
# automatically calculate weighted means for pct and means, erase var and ci)
#' Vec_math method for class fmt
#' @param .fn A function
#' @param .x A fmt object
#' @param ... Other parameter
#' @return A fmt vector
#' @export
vec_math.tabxplor_fmt <- function(.fn, .x, ...) {
  if (!is.na(get_type(.x) ) & get_type(.x) == "mixed") warning(
    "operation ", .fn,
    " within a variable mixing different types of percentages"
  )

  switch(.fn,
         "sum" = new_fmt(display   = get_display(.x)[1],
                         digits = min(get_digits(.x)),
                         n      = vctrs::vec_math_base(.fn, get_n(.x)  , ...),
                         wn     = vctrs::vec_math_base(.fn, get_wn(.x) , ...),
                         pct    = ifelse(! get_type(.x) %in% c("row", "col"),
                                         yes = vctrs::vec_math_base(.fn, get_pct(.x), ...),
                                         no  = NA_real_) %>%
                           tidyr::replace_na(NA_real_),
                         diff   = NA_real_,
                         ctr    = NA_real_,
                         mean   = vctrs::vec_math_base("sum", get_mean(.x) * get_wn(.x), ...) /
                           vctrs:: vec_math_base("sum", get_wn(.x), ...),
                         var    = NA_real_,
                         ci     = NA_real_,

                         in_totrow = all(is_totrow(.x)),
                         in_refrow = all(is_refrow(.x)),
                         in_tottab = all(is_tottab(.x)), #any ?

                         type      = get_type    (.x),
                         comp_all  = get_comp_all(.x, replace_na = FALSE),
                         diff_type = get_diff_type(.x),
                         ci_type   = get_ci_type (.x),
                         col_var   = get_col_var (.x),
                         totcol    = is_totcol   (.x),
                         refcol    = is_refcol   (.x),
                         color     = get_color   (.x)
         ),
         "mean" = new_fmt(display = get_display(.x)[1],
                          digits  = max(get_digits(.x)),
                          n       = vctrs::vec_math_base("sum", get_n(.x)  , ...),
                          wn      = vctrs::vec_math_base("sum", get_wn(.x) , ...),
                          pct     = vctrs::vec_math_base("sum", get_pct(.x) * get_wn(.x), ...) /
                            vctrs::vec_math_base("sum", get_wn(.x), ...),
                          diff    = NA_real_,
                          ctr     = NA_real_,
                          mean    = vctrs::vec_math_base("sum", get_mean(.x) * get_wn(.x), ...) /
                            vctrs::vec_math_base("sum", get_wn(.x), ...),
                          var     = NA_real_,
                          ci      = NA_real_,

                          in_totrow = FALSE,
                          in_refrow = FALSE,
                          in_tottab = all(is_tottab(.x)), #any ?

                          type      = get_type    (.x),
                          comp_all  = get_comp_all(.x, replace_na = FALSE),
                          diff_type = get_diff_type(.x),
                          ci_type   = get_ci_type (.x),
                          col_var   = get_col_var (.x),
                          totcol    = is_totcol   (.x),
                          refcol    = is_refcol   (.x),
                          color     = get_color   (.x)
         ),
         vctrs::vec_math_base(.fn, get_num(.x), ...) )
}


