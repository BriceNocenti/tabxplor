# PURPOSE: Define tabxplor_fmt, a vctrs record class for formatted cross-table cells.
# ROLE: Foundation of the entire package. Every numeric column in a tabxplor_tab is fmt.
# KEY CONSTRAINTS:
#   - Adding a new field requires updating: new_fmt(), fmt(), format.tabxplor_fmt(),
#     pillar_shaft.tabxplor_fmt(), vec_arith methods, and possibly tab_pct/tab_ci/tab_chi2.
#   - Fields are per-cell (vctrs::field), attributes are per-column (attr). Do not confuse.
#   - pct is stored as 0-1 internally; multiplied by 100 only in format().
#   - For type="mean", the diff field stores a RATIO (cell/ref), not a difference.
#   - Display glyph constants (mult_sign, cross, unbrk, sigma_sign) are defined in utils.R.
# See: CLAUDE.md § Design Decisions > Type System.

# Create formated numbers class
#Import vctrs in NAMESPACE :
#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name tabxplor-vctrs
NULL


# binding for global variables not found by R CMD check
. = NULL
globalVariables(c(":=", ".SD", ".N"))
# data.table NSE column symbols used in tab_plain()'s aggregation j-expressions:
globalVariables(c("n", "wn"))
# Phase 3b test engine (R/tab-agg.R) data.table NSE column symbols:
globalVariables(c("table_id", "row_id", "col_id", "o", "rowtot", "coltot", "ok",
                  "grandtot", "nr", "nc", "e", "contrib", "signed_contrib",
                  "statistic", "df", "min_e", "w", "group_id"))


# EXPORTED FUNCTIONS TO WORK WITH CLASS FMT ##############################################


#' Create a vector of class formatted numbers
#' @description \code{fmt} vectors, of class \code{tabxplor_fmt}, powers \pkg{tabxplor}
#' and \code{\link{tab}} tibbles.
#' As a \code{\link[vctrs:new_rcrd]{record}}, they stores all data necessary to
#' calculate percentages, Chi2 metadata or confidence intervals, but also to format and
#' color the table to help the user read it. You can access this data with
#' \code{\link[vctrs:field]{vctrs::field}}, or change it with
#' \code{\link[vctrs:field]{vctrs:field<-}}. A \code{fmt} vector have 18 fields :
#' \code{n}, \code{digits}, \code{display}, \code{wn}, \code{pct}, \code{mean},
#' \code{diff}, \code{ratio}, \code{ctr}, \code{var}, \code{ci_inf}, \code{ci_sup},
#' \code{pvalue}, \code{or}, \code{tot_n},
#' \code{in_totrow},  \code{in_tottab},
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
#' @param ratio The ratio to the reference (relative risk for percentages, mean ratio for
#' means), as a double vector the length of \code{n}. Renamed from the former \code{rr}
#' field.
#' @param ctr The contributions of cells to (sub)tables variances,
#' as a double vector the length of \code{n}. Used to print colors when
#' \code{color = "contrib"}. The mean contribution of each (sub)table is written on
#' total rows (then, colors don't print well without total rows).
#' Calculate with \code{\link{tab_chi2}}.
#' @param var The cells variances, as a double vector the length of \code{n}.
#' Used with \code{type = "mean"} to calculate confidence intervals.
#' Calculate with \code{tab_plain}.
#' @param ci The confidence interval half-width (margin of error), as a double vector the
#' length of \code{n}. Kept for backward compatibility: it is stored as the symmetric
#' bounds \code{ci_inf}/\code{ci_sup} and read back by \code{get_ci()}.
#' Calculate with \code{tab_ci}.
#' @param ci_inf,ci_sup The lower and upper bounds of the confidence interval, as double
#' vectors the length of \code{n}. Calculate with \code{tab_ci}.
#' @param pvalue The per-cell significance p-value, as a double vector the length of
#' \code{n}.
#' @param or The odds ratio or relative risk ratio, as a double vector the length of \code{n}.
#' @param tot_n The cell's own (unweighted) percentage base, as a double vector the length
#' of \code{n}.
#' @param in_totrow \code{TRUE} when the cell is part of a total row
#' @param in_tottab \code{TRUE} when the cell is part of a total table
#' @param in_refrow \code{TRUE} when the cell is part of a reference row
#' (cf. \code{ref})
#' @param comp_all  \code{FALSE} when the comparison level is the subtable/group,
#' \code{TRUE} when it is the whole table
#' @param ref The type of difference of the vector. Cf. \code{\link{tab}}.
#' @param ci_type The type of confidence intervals of the vector (calculate
#'  with \code{\link{tab_ci}}) :
#' \itemize{
#'   \item \code{""} or \code{"no"}: no ci have been calculated
#'   \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'   \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'   relative total cell (or relative first cell when \code{ref = "first"}).
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
#'   totals (or from first cells when \code{ref = "first"}).
#'   \item \code{"diff_ci"}: color pct and means based on cells differences from totals
#'   or first cells, removing coloring when the confidence interval of this difference
#'   is higher than the difference itself.
#'   \item \code{"after_ci"}: idem, but cut off the confidence interval from the
#'   difference first.
#'   \item \code{"contrib"}: color cells based on their contribution to variance
#'   (except mean columns, from numeric variables).
#' }
#' @param color_signif How significance gates the color, as a single string
#' (\code{"ignore"} / \code{"grey_non_signif"} / \code{"color_all_signif"}). See \code{\link{tab}}.
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
#'             other_if_less_than = 5)
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
                ratio     = rep(NA_real_, length(n)),
                ctr       = rep(NA_real_, length(n)),
                var       = rep(NA_real_, length(n)),
                ci        = rep(NA_real_, length(n)),
                ci_inf    = rep(NA_real_, length(n)),
                ci_sup    = rep(NA_real_, length(n)),
                pvalue    = rep(NA_real_, length(n)),
                or        = rep(NA_real_, length(n)),
                tot_n     = rep(NA_real_, length(n)),

                in_totrow = rep(FALSE, length(n)),
                in_tottab = rep(FALSE, length(n)),
                in_refrow = rep(FALSE, length(n)),


                comp_all  = NA   ,
                ref = ""   ,
                ci_type   = ""   ,
                col_var   = ""   ,
                totcol    = FALSE,
                refcol    = FALSE,
                color     = ""    ,
                color_signif = "ignore") {

  # DESIGN: these 8 fields set the recycling reference length. display, diff, ratio, or,
  # the ci bounds, pvalue, tot_n and the in_* flags are recycled TO it below, so they must
  # not be passed longer than these (vec_recycle would error, not extend).
  max_size <- list(n, wn, pct, digits, ctr, mean, var, ci) %>% #display
    purrr::map_int(length) %>% max()

  display <- vctrs::vec_recycle(vctrs::vec_cast(display, character()), size = max_size)
  n       <- vctrs::vec_recycle(vctrs::vec_cast(n      , integer())  , size = max_size)
  wn      <- vctrs::vec_recycle(vctrs::vec_cast(wn     , double())   , size = max_size) #anything coercible as a double
  pct     <- vctrs::vec_recycle(vctrs::vec_cast(pct    , double())   , size = max_size)
  diff    <- vctrs::vec_recycle(vctrs::vec_cast(diff   , double())   , size = max_size)
  ratio   <- vctrs::vec_recycle(vctrs::vec_cast(ratio  , double())   , size = max_size)
  digits  <- vctrs::vec_recycle(vctrs::vec_cast(digits , integer())  , size = max_size)
  ctr     <- vctrs::vec_recycle(vctrs::vec_cast(ctr    , double())   , size = max_size)
  mean    <- vctrs::vec_recycle(vctrs::vec_cast(mean   , double())   , size = max_size)
  var     <- vctrs::vec_recycle(vctrs::vec_cast(var    , double())   , size = max_size)
  ci      <- vctrs::vec_recycle(vctrs::vec_cast(ci     , double())   , size = max_size)
  ci_inf  <- vctrs::vec_recycle(vctrs::vec_cast(ci_inf , double())   , size = max_size)
  ci_sup  <- vctrs::vec_recycle(vctrs::vec_cast(ci_sup , double())   , size = max_size)
  pvalue  <- vctrs::vec_recycle(vctrs::vec_cast(pvalue , double())   , size = max_size)
  or      <- vctrs::vec_recycle(vctrs::vec_cast(or     , double())   , size = max_size)
  tot_n   <- vctrs::vec_recycle(vctrs::vec_cast(tot_n  , double())   , size = max_size)

  # Phase 3a: the public `ci` arg is a symmetric half-width; store it as ABSOLUTE bounds
  # around the estimate the interval is centred on (the difference for diff-type CIs, the mean
  # for cell means, the proportion otherwise), matching how tab_ci()/tab_num() now store real
  # asymmetric bounds. Explicit ci_inf/ci_sup win; get_ci() reads the half-width back as
  # ci_sup - centre. See dev/tabxplor_1.4.0_decisions.md §1, §20.
  est_center <- dplyr::coalesce(
    if (ci_type %in% c("diff", "diff_row", "diff_col")) diff else
      if (type == "mean") mean else pct,
    0)
  ci_sup  <- dplyr::coalesce(ci_sup, est_center + ci)
  ci_inf  <- dplyr::coalesce(ci_inf, est_center - ci)

  in_totrow <- vctrs::vec_recycle(vctrs::vec_cast(in_totrow, logical()), size = max_size)
  in_tottab <- vctrs::vec_recycle(vctrs::vec_cast(in_tottab, logical()), size = max_size)
  in_refrow <- vctrs::vec_recycle(vctrs::vec_cast(in_refrow, logical()), size = max_size)

  type      <- vctrs::vec_recycle(vctrs::vec_cast(type     , character()), size = 1)
  comp_all  <- vctrs::vec_recycle(vctrs::vec_cast(comp_all , logical()  ), size = 1)
  ref <- vctrs::vec_recycle(vctrs::vec_cast(ref, character()), size = 1)
  ci_type   <- vctrs::vec_recycle(vctrs::vec_cast(ci_type  , character()), size = 1)
  col_var   <- vctrs::vec_recycle(vctrs::vec_cast(col_var  , character()), size = 1)
  totcol    <- vctrs::vec_recycle(vctrs::vec_cast(totcol   , logical()  ), size = 1)
  refcol    <- vctrs::vec_recycle(vctrs::vec_cast(refcol   , logical()  ), size = 1)
  # `color` is a per-column attribute of length 1 (text channel) or 2 (text, background) -- NOT
  # recycled to 1 (Phase 5 §9.1). color_signif is the scalar significance policy.
  color        <- vctrs::vec_cast(color, character())
  color_signif <- vctrs::vec_recycle(vctrs::vec_cast(color_signif, character()), size = 1)

  new_fmt(n = n, display = display, digits = digits,
          wn = wn, pct = pct,  mean = mean,
          diff = diff, ratio = ratio, ctr = ctr, var = var,
          ci_inf = ci_inf, ci_sup = ci_sup, pvalue = pvalue, or = or, tot_n = tot_n,
          in_totrow = in_totrow, in_tottab = in_tottab, in_refrow = in_refrow,
          type = type, comp_all = comp_all,  ref = ref,
          ci_type = ci_type, col_var = col_var, totcol = totcol, refcol = refcol,
          color = color, color_signif = color_signif)
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
  # DESIGN: get_num() is the authoritative `display` -> underlying-field map. Allowed
  # display values and the field each reads: n/(default)->n, wn->wn,
  # pct/pct_ci/pvalue->pct, diff->diff, ctr->ctr, mean/mean_ci->mean, var->var,
  # ci->get_ci() (the CI half-width, read from the ci_sup bound via the Phase 1a shim),
  # rr->ratio (the "rr" display token maps to the renamed `ratio` field), or/OR/or_pct->or.
  # format.tabxplor_fmt() renders these plus the CI/label variants (pct_ci, mean_ci,
  # or_pct, OR_pct). When adding a display value, keep this map, set_num() and format() in
  # sync (see the /vctrs-field skill).
  out     <- get_n(x)
  display <- get_display(x)
  nas     <- is.na(display)
  out[!nas & display == "wn"     ] <- get_wn  (x)[!nas & display == "wn"     ]
  out[!nas & display == "pct"    ] <- get_pct (x)[!nas & display == "pct"    ]
  out[!nas & display == "pvalue" ] <- get_pct (x)[!nas & display == "pvalue" ]
  out[!nas & display == "diff"   ] <- get_diff(x)[!nas & display == "diff"   ]
  out[!nas & display == "pct_ci" ] <- get_pct (x)[!nas & display == "pct_ci" ]
  out[!nas & display == "ctr"    ] <- get_ctr (x)[!nas & display == "ctr"    ]
  out[!nas & display == "mean"   ] <- get_mean(x)[!nas & display == "mean"   ]
  out[!nas & display == "mean_ci"] <- get_mean(x)[!nas & display == "mean_ci"]
  out[!nas & display == "var"    ] <- get_var (x)[!nas & display == "var"    ]
  out[!nas & display == "ci"     ] <- get_ci   (x)[!nas & display == "ci"     ]
  out[!nas & display == "rr"     ] <- get_ratio(x)[!nas & display == "rr"     ]
  out[!nas & display %in% c("or", "OR")] <- get_or(x)[!nas & display %in% c("or", "OR")     ]
  out[!nas & display == "or_pct" ] <- get_or  (x)[!nas & display == "or_pct" ]
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
  out[!nas & display == "diff"] <- set_diff(x[!nas & display == "diff"], value[!nas & display == "diff"])
  out[!nas & display == "ctr" ] <- set_ctr (x[!nas & display == "ctr" ], value[!nas & display == "ctr" ])
  out[!nas & display == "mean"] <- set_mean(x[!nas & display == "mean"], value[!nas & display == "mean"])
  out[!nas & display == "var" ] <- set_var (x[!nas & display == "var" ], value[!nas & display == "var" ])
  out[!nas & display == "ci"  ] <- set_ci   (x[!nas & display == "ci"  ], value[!nas & display == "ci"  ])
  out[!nas & display == "rr"  ] <- set_ratio(x[!nas & display == "rr"  ], value[!nas & display == "rr"  ])
  out[!nas & display %in% c("or", "OR")] <- set_or(x[!nas & display %in% c("or", "OR")  ], value[!nas & display == "or"  ])
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
    totrow_cells_test |>
      dplyr::transmute(var = dplyr::if_any(.cols = dplyr::everything())) |>
      tibble::deframe()
  } else {
    test_result <- totrow_cells_test %>%
      dplyr::transmute(complete = dplyr::if_all(.cols = dplyr::everything() ),
                       partial  = dplyr::if_all(-"complete") & !.data$complete)
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
    tottab_cells_test %>%
      dplyr::transmute(var = dplyr::if_any(.cols = dplyr::everything())) %>%
      tibble::deframe()
  } else {
    test_result <- tottab_cells_test %>%
      dplyr::transmute(complete = dplyr::if_all(.cols = dplyr::everything() ),
                       partial  = dplyr::if_all(-"complete") & !.data$complete)
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


#' @describeIn fmt set the "display" vctrs::field of a \code{fmt} vector, or of
#' all of them in the whole tibble.
#' @return The entered objects, with all fmt vectors with the wanted display.
#' @export
set_display <- function(x, value) UseMethod("set_display")
#' Set the "display" vctrs::field of a \code{fmt} vector.
#' @inheritParams fmt
#' @return The entered vector (nothing happens).
#' @export
set_display.default <- function(x, value) {
return(x)
}
#' Set the "display" vctrs::field of a \code{fmt} vector.
#' @inheritParams fmt
#' @return A fmt vectors with the wanted display.
#' @export
set_display.tabxplor_fmt <- function(x, value) {
  value <- vctrs::vec_cast(value, character()) %>% vctrs::vec_recycle(size = length(x))
  vctrs::`field<-`(x, "display", value)
}
#' Set the "display" vctrs::field of a \code{fmt} vector.
#' @inheritParams fmt
#' @return The entered objects, with all fmt vectors with the wanted display.
#' @export
set_display.data.frame <- function(x, value) {
  x |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is_fmt) & -(tidyselect::any_of(c("n", "wn")) &
                                 dplyr::where(~ get_type(.) == "n")),
      ~ set_display(., value)
    ))
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
    refrow_cells_test %>%
      dplyr::transmute(var = dplyr::if_any(.cols = dplyr::everything() )) %>%
      tibble::deframe()
  } else {
    test_result <- refrow_cells_test %>%
      dplyr::transmute(complete = dplyr::if_all(.cols = dplyr::everything() ),
                       partial  = dplyr::if_all(-"complete") & !.data$complete)
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
get_ref_type <- function(x, ...) UseMethod("get_ref_type")
#' Get differences type of fmt columns
#' @method get_ref_type default
#' @inheritParams fmt
#' @return A single character with the ref attribute.
#' @export
get_ref_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("ref")(x)),
         yes = purrr::attr_getter("ref")(x),
         no  = "") #NA_character_
}
#' Get differences type of fmt columns
#' @method get_ref_type tabxplor_fmt
#' @inheritParams fmt
#' @return A single character with the ref attribute.
#' @export
get_ref_type.tabxplor_fmt <- function(x, ...) attr(x, "ref", exact = TRUE)
#' Get differences type of fmt columns
#' @method get_ref_type data.frame
#' @inheritParams fmt
#' @return A character vector with the ref attribute.
#' @export
get_ref_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_ref_type(.))
}

#' @describeIn fmt set the differences type attribute of a \code{fmt} vector
#' @return A modified fmt vector.
#' @export
set_diff_type   <- function(x, ref) {
  #stopifnot(ref %in% c("tot", "first", "no", "", NA_character_))
  `attr<-`(x ,"ref" , ref)
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


# Phase 5 (§9.1): the `color` attribute holds ONE or TWO measures -- the text channel and an
# optional background channel. fmt_color_attr() returns the FULL vector (the vctrs reconcilers
# read this so the bg channel is not silently dropped on c()/cast/group). get_color() returns the
# TEXT channel [1] -- the unchanged scalar contract every existing consumer relies on.
#' @keywords internal
fmt_color_attr <- function(x) attr(x, "color", exact = TRUE)

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
  a <- purrr::attr_getter("color")(x)
  if (is.null(a)) "" else a[1]
}
#' Get color
#' @method get_color tabxplor_fmt
#' @inheritParams fmt
#' @return A single character with the color attribute (the text channel).
#' @export
get_color.tabxplor_fmt <- function(x, ...) attr(x, "color", exact = TRUE)[1]
#' Get color
#' @method get_color data.frame
#' @inheritParams fmt
#' @return A character vector with the color attributes.
#' @export
get_color.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_color(.))
}

#' @describeIn fmt get the background-channel color measure (\code{NA} when there is none)
#' @return A single character with the background color measure, or \code{NA}.
#' @export
get_color_bg <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_chr(x, ~ get_color_bg(.)))
  a <- fmt_color_attr(x)
  if (length(a) >= 2L) a[2] else NA_character_
}

#' @describeIn fmt get the significance policy (\code{"ignore"} / \code{"grey_non_signif"} / \code{"color_all_signif"})
#' @export
get_color_signif <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_chr(x, ~ get_color_signif(.)))
  a <- attr(x, "color_signif", exact = TRUE)
  if (is.null(a)) "ignore" else a[1]
}

# Normalize a color argument (scalar / unnamed c(text, bg) / named c(text=, background=)) into a
# positional length-1-or-2 character vector [text, background]. Accepts the new measures
# (diff/ratio/contrib/or) and, transitionally, the old catalogue strings (diff_ci/after_ci/ci),
# until Step 4d decodes those into (measure, color_signif) at the argument boundary.
#' @keywords internal
resolve_color_channels <- function(color) {
  if (length(color) == 0L) return("")
  nms <- names(color)
  if (!is.null(nms) && any(nzchar(nms))) {
    text <- if ("text" %in% nms) unname(color[["text"]]) else ""
    bg   <- if ("background" %in% nms) unname(color[["background"]]) else
            if ("bg" %in% nms) unname(color[["bg"]]) else NA_character_
    color <- if (is.na(bg)) text else c(text, bg)
  }
  color <- unname(vapply(color, function(m)
    if (is.na(m) || identical(m, "no")) "" else if (identical(m, "or")) "OR" else m,
    character(1)))
  if (length(color) > 2L) cli::cli_abort("{.arg color} accepts at most two values (text, background).")
  ok <- c("diff", "ratio", "contrib", "OR", "diff_ci", "after_ci", "ci", "")
  if (!all(color %in% ok)) {
    cli::cli_abort(c("Unknown color measure {.val {setdiff(color, ok)}}.",
                     "i" = "Valid measures: {.val {c('diff','ratio','contrib','or')}}."))
  }
  if (length(color) == 2L && color[2] %in% c("contrib", "OR", "diff_ci", "after_ci", "ci")) {
    cli::cli_abort("{.val {color[2]}} is a whole-cell measure; it cannot go on the background channel.")
  }
  if (length(color) == 2L && color[2] == "") color <- color[1]   # trim an empty bg
  color
}

#' @describeIn fmt set the "color" attribute of a \code{fmt} vector
# @param color The type of color to print in tibbles, as a single string.
#' @return A modified fmt vector.
#' @export

# @keywords internal
#' @export
set_color     <- function(x, color) {
  `attr<-`(x, "color", resolve_color_channels(color))
}

#' @describeIn fmt set the significance policy attribute of a \code{fmt} vector
#' @export
set_color_signif <- function(x, color_signif) {
  color_signif <- color_signif[1]
  if (is.na(color_signif) || color_signif %in% c("", "no")) color_signif <- "ignore"
  ok <- c("ignore", "grey_non_signif", "color_all_signif")
  if (!color_signif %in% ok) {
    cli::cli_abort(c("Unknown {.arg color_signif} value {.val {color_signif}}.",
                     "i" = "Valid: {.val {ok}}."))
  }
  `attr<-`(x, "color_signif", color_signif)
}



# fmt_get_color_code() doen't work in mutate with groups.

#' Get HTML Color Code of a fmt vector
#' @param x The fmt vector to get the html color codes from.
#'
#' @param type The style type in \code{set_color_style} and \code{get_color_style},
#'  \code{"text"} to color the text, \code{"bg"} to color the background.
#' @param theme For \code{set_color_style} and \code{get_color_style}, is your console
#' or html table background \code{"light"} or \code{"dark"} ? Default to RStudio theme.
#' @param html_24_bit Use 24bits colors palettes for html tables : set to `"green_red"`
#' or `"blue_red"`. Only with `mode = "color_code"` (not `mode = "crayon"`) and
#' `theme = "light`. Default to \code{getOption("tabxplor.color_html_24_bit")}.
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

  color <- get_color(x)
  if (length(color) == 0L || is.na(color[1]) || color[1] %in% c("no", "")) {
    return(rep(NA_character_, length(x)))
  }

  # `type` selects BOTH the slot table channel (text/bg families spread intensities differently)
  # and the palette variant, so the rendered hex matches the palette.
  channel <- if (type == "bg") "bg" else "text"
  slot    <- fmt_color_slots(x, fmt_color_plan(x, channel = channel))
  styles  <- get_color_style("color_code", type = type, theme = theme, html_24_bit = html_24_bit)

  out     <- rep(NA_character_, length(x))
  colored <- slot > 0L
  # historical output is upper-case hex (the old path str_to_upper'd); the 24-bit palettes carry
  # lower-case codes, so upper-case here to keep the rendered code identical.
  out[colored] <- toupper(unname(styles[slot[colored]]))
  out
}






# INTERNAL FUNCTIONS #####################################################################


# DESIGN: new_fmt() is the internal constructor. Attributes (type, color, ci_type, etc.)
#   are SCALAR per-column, not per-cell. Fields (n, pct, diff, etc.) are per-cell vectors.
#   This distinction is fundamental: attributes describe column semantics,
#   fields carry individual cell data. See vctrs::new_rcrd().
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
                    ratio     = rep(NA_real_, length(n)),
                    ctr       = rep(NA_real_, length(n)),
                    var       = rep(NA_real_, length(n)),
                    ci_inf    = rep(NA_real_, length(n)),
                    ci_sup    = rep(NA_real_, length(n)),
                    pvalue    = rep(NA_real_, length(n)),
                    or        = rep(NA_real_, length(n)),
                    tot_n     = rep(NA_real_, length(n)),

                    in_totrow = rep(FALSE   , length(n)),
                    in_tottab = rep(FALSE   , length(n)),
                    in_refrow = rep(FALSE   , length(n)),

                    comp_all  = NA   ,
                    ref = ""   ,
                    ci_type   = ""   ,
                    col_var   = ""   ,
                    totcol    = FALSE,
                    refcol    = FALSE,
                    color     = ""   ,
                    color_signif = "ignore",
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
         diff = diff, ratio = ratio, ctr = ctr, var = var,
         ci_inf = ci_inf, ci_sup = ci_sup, pvalue = pvalue, or = or,
         tot_n = tot_n,
         in_totrow = in_totrow, in_tottab = in_tottab,
         in_refrow = in_refrow),
    type = type, comp_all = comp_all, ref = ref,
    ci_type = ci_type, col_var = col_var, totcol = totcol, refcol = refcol,
    color = color, color_signif = color_signif[1], class = c(class, "tabxplor_fmt"))
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
# @describeIn fmt get the "ratio" field (ratio to the reference; formerly "rr")
#' @keywords internal
# @export
get_ratio  <- fmt_field_factory("ratio")
# @describeIn fmt get the "var" field (cell variances of means)
#' @keywords internal
# @export
get_var    <- fmt_field_factory("var")
# @describeIn fmt get the "ci_inf" field (lower confidence-interval bound)
#' @keywords internal
# @export
get_ci_inf <- fmt_field_factory("ci_inf")
# @describeIn fmt get the "ci_sup" field (upper confidence-interval bound)
#' @keywords internal
# @export
get_ci_sup <- fmt_field_factory("ci_sup")
# ci_center(): the estimate a stored CI is centred on -- the difference for diff-type CIs, the
# mean for cell means, the proportion otherwise. get_ci() reads the (upper-arm) half-width back
# as ci_sup - centre, retro-compatible with the former `upr.ci - est`; get_ci_moe() returns the
# conservative larger arm for the +/- moe display (Wilson/Newcombe bounds are asymmetric).
# Phase 3a: ci_inf/ci_sup now hold real asymmetric ABSOLUTE bounds. See §1, §20.
#' @keywords internal
ci_center  <- function(x) {
  if (get_ci_type(x) %in% c("diff", "diff_row", "diff_col")) get_diff(x)
  else if (get_type(x) == "mean")                            get_mean(x)
  else                                                       get_pct(x)
}
# @describeIn fmt get the confidence-interval half-width (upper arm, from the stored bounds)
#' @keywords internal
# @export
get_ci     <- function(x) get_ci_sup(x) - ci_center(x)
# @describeIn fmt get the CI margin of error (conservative larger arm, for the +/- moe display)
#' @keywords internal
get_ci_moe <- function(x) {
  ctr <- ci_center(x)
  pmax(ctr - get_ci_inf(x), get_ci_sup(x) - ctr)
}
# get_stars(): per-cell significance glyphs from the stored `pvalue` (universal CI-inclusion,
# so they always agree with the interval bracket). Thresholds/labels are options; "" where the
# pvalue is NA (cell CIs, non-diff cells, or stars opted out). See §20.
#' @keywords internal
get_stars  <- function(x, p = get_pvalue(x)) {
  brk <- sort(getOption("tabxplor.signif_levels", c(0.10, 0.05, 0.01)), decreasing = TRUE)
  lab <- getOption("tabxplor.signif_labels", c("*", "**", "***"))
  nb  <- rowSums(outer(p, brk, `<`), na.rm = TRUE)
  out <- c("", lab)[nb + 1L]
  out[is.na(p)] <- ""
  out
}
# @describeIn fmt get the "pvalue" field (per-cell significance)
#' @keywords internal
# @export
get_pvalue <- fmt_field_factory("pvalue")
# @describeIn fmt get the "or" field (odds ratio or relative risk ratio)
#' @keywords internal
# @export
get_or     <- fmt_field_factory("or")
# @describeIn fmt get the "tot_n" field (the cell's own unweighted percentage base)
#' @keywords internal
# @export
get_tot_n  <- fmt_field_factory("tot_n")

# get_tot_wn(): the cell's OWN WEIGHTED percentage base. This is NOT a stored field -- it is
# recovered as wn / pct (pct is stored at full precision; only display is rounded), mirroring the
# way get_ci() recovers the half-width from the stored bounds. For an empty cell (pct == 0) the
# ratio is undefined, so fall back to a same-column total (100%) cell's weighted count when one is
# present (covers col% and grand-total modes); a row%-empty cell whose base lives in another column
# is not recoverable here and returns NA (documented -- decisions doc §11, open item C2). Unweighted
# tables return get_wn() == get_n() so the base is still exact. Consumed from Phase 3 on.
#' @keywords internal
get_tot_wn <- function(x) {
  wn  <- get_wn(x)
  pct <- get_pct(x)
  tw  <- wn / pct
  bad <- !is.finite(tw)
  if (any(bad)) {
    base_cell <- which(is_totrow(x) & is.finite(pct) & abs(pct - 1) < 1e-9)
    tw[bad] <- if (length(base_cell) > 0) wn[base_cell[length(base_cell)]] else NA_real_
  }
  tw
}

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
      dplyr::with_groups("gr", ~ dplyr::mutate(., nb = dplyr::last(.data$nb))) %>%
      dplyr::mutate(mean_ctr = .data$ctr[.data$nb]) %>% dplyr::pull(.data$mean_ctr)
  }
}

#' @keywords internal
get_ref_means <- function(x) {
  comp      <- get_comp_all(x)
  ref <- get_ref_type(x)

  refrows <- if (ref == "tot") { is_totrow(x) } else { is_refrow(x) }
  tottabs <- is_tottab(x)
  mean    <- get_mean(x)

  if (comp) {
    refs <- refrows & tottabs
    if (!any(refs)) {rep(NA_real_, length(x))} else {rep(mean[refs], length(x))}

    #refs <- mean[refrows & tottabs]
   #if (length(refs) == 0) {rep(NA_real_, length(x))} else {rep(mean[refs], length(x))}
  } else {
    tibble::tibble(
      mean = mean,
      gr = cumsum(as.integer(refrows)) - as.integer(refrows) ) %>%
      dplyr::mutate(nb = dplyr::row_number()) %>%
      dplyr::with_groups("gr", ~ dplyr::mutate(., nb = dplyr::last(.data$nb))) %>%
      dplyr::mutate(ref_means = .data$mean[.data$nb]) %>%
      dplyr::pull(.data$ref_means)
  }
}

#' @keywords internal
get_ref_pct <- function(x) {
  comp      <- get_comp_all(x)
  ref <- get_ref_type(x)

  refrows <- if (ref == "tot") { is_totrow(x) } else { is_refrow(x) }
  tottabs <- is_tottab(x)
  pct    <- get_pct(x)

  if (comp) {
    refs <- refrows & tottabs # pct[refrows & tottabs]
    if (!any(refs)) {rep(NA_real_, length(x))} else {rep(pct[refs], length(x))}
  } else {
    tibble::tibble(
      pct = pct,
      gr = cumsum(as.integer(refrows)) - as.integer(refrows) ) %>%
      dplyr::mutate(nb = dplyr::row_number()) %>%
      dplyr::with_groups("gr", ~ dplyr::mutate(., nb = dplyr::last(.data$nb))) %>%
      dplyr::mutate(ref_pcts = .data$pct[.data$nb]) %>%
      dplyr::pull(.data$ref_pcts)
  }
}

# Phase 5: the reference cell's VARIANCE, broadcast to every cell of its group (mirror of
# get_ref_means/get_ref_pct reading the `var` field). Used for Glass's delta = diff / sqrt(var_ref),
# the sd-standardized numeric diff-color scale (§18). NA/0 var_ref -> no color at the call site.
#' @keywords internal
get_ref_var <- function(x) {
  comp <- get_comp_all(x)
  ref  <- get_ref_type(x)

  refrows <- if (ref == "tot") { is_totrow(x) } else { is_refrow(x) }
  tottabs <- is_tottab(x)
  var     <- get_var(x)

  if (comp) {
    refs <- refrows & tottabs
    if (!any(refs)) {rep(NA_real_, length(x))} else {rep(var[refs], length(x))}
  } else {
    tibble::tibble(
      var = var,
      gr = cumsum(as.integer(refrows)) - as.integer(refrows) ) %>%
      dplyr::mutate(nb = dplyr::row_number()) %>%
      dplyr::with_groups("gr", ~ dplyr::mutate(., nb = dplyr::last(.data$nb))) %>%
      dplyr::mutate(ref_vars = .data$var[.data$nb]) %>%
      dplyr::pull(.data$ref_vars)
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
  # Total columns are identified by the `totcol` attribute (is_totcol) / the "no_col_var"
  # col_var — robust, not by hard-coded position. Each column is then mapped to the first
  # such total column at or after its own position.
  tot <- which(is_totcol(tabs) | get_col_var(tabs) == "no_col_var")

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
# @describeIn fmt set the "ratio" field (ratio to the reference; formerly "rr")
#' @keywords internal
# @export
set_ratio   <- fmt_set_field_factory("ratio"  , cast = double()   )
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
# @describeIn fmt set the "ci_inf" field (lower confidence-interval bound)
#' @keywords internal
# @export
set_ci_inf  <- fmt_set_field_factory("ci_inf" , cast = double()   )
# @describeIn fmt set the "ci_sup" field (upper confidence-interval bound)
#' @keywords internal
# @export
set_ci_sup  <- fmt_set_field_factory("ci_sup" , cast = double()   )
# set_ci() (legacy): takes a symmetric half-width and stores it as ABSOLUTE bounds around the
# estimate the interval is centred on (ci_center()), so get_ci() reads the half-width back. The
# Phase 3a writers (tab_ci()/tab_num()) store real asymmetric bounds directly via set_ci_inf/
# set_ci_sup and do NOT use this; it is kept for back-compatible external callers. Its centring
# is best-effort (needs ci_type/type already set). See dev/tabxplor_1.4.0_decisions.md §1, §20.
# @describeIn fmt set the confidence-interval half-width (stored as symmetric absolute bounds)
#' @keywords internal
# @export
set_ci      <- function(x, value) {
  value <- vctrs::vec_cast(value, double()) %>% vctrs::vec_recycle(size = length(x))
  ctr   <- dplyr::coalesce(ci_center(x), 0)
  x <- set_ci_sup(x, ctr + value)
  x <- set_ci_inf(x, ctr - value)
  x
}
# @describeIn fmt set the "pvalue" field (per-cell significance)
#' @keywords internal
# @export
set_pvalue  <- fmt_set_field_factory("pvalue" , cast = double()   )
# @describeIn fmt set the "or" field (odds ratio or relative risk ratio)
#' @keywords internal
# @export
set_or      <- fmt_set_field_factory("or"     , cast = double()   )
# @describeIn fmt set the "tot_n" field (the cell's own unweighted percentage base)
#' @keywords internal
# @export
set_tot_n   <- fmt_set_field_factory("tot_n"  , cast = double()   )







# METHODS FOR CLASS tabxplor_fmt #########################################################

#' @keywords internal
print_num <- function(num, digits) {
  sprintf(paste0("%-0.", digits, "f"), num) %>%
    stringr::str_replace("^0.0+$|^-0.0+$", "0") %>%
    stringr::str_replace("^100.0+$", "100")
}

# WARNING: currently a no-op passthrough. Rendering CIs as HTML/LaTeX subscripts (the
# commented `$_{...}$` / <sub> variants below) worked in console and RMarkdown but broke in
# Jamovi, so subscript formatting is disabled until a Jamovi-safe encoding is found.
ci_html_subscript <- function(x, html = FALSE) {
  if (html) x <- dplyr::if_else(
    condition = stringr::str_detect(x,"^ *$" ),
    true      = "",
    false     = x #paste0("$_{", x, "}$")
      # paste0('<span style="vertical-align: baseline; position: relative;top: -0.5em;>', x, '</span>')
      # paste0("<p><sub>", x, "</sub></p>")
  )
  x
}

# Format/printing methods for class tabxplor_fmt -----------------------------------------
#The first method for every class should almost always be a format() method.
#This should return a character vector the same length as x.

# DESIGN: Central display method. Handles 20+ display modes (n, wn, pct, diff, ctr, ci,
#   pct_ci, mean_ci, var, pvalue, or, OR, etc.). Key transformations:
#   - pct stored as 0-1 is multiplied by 100 here for display
#   - Two CI display modes controlled by option tabxplor.ci_print: "moe" (±margin) or "ci" ([lo;hi])
#   - diff for means shows with "*" symbol; diff for pct shows with +/- sign
#   - special_formatting=TRUE adds "ref:" prefix and "mean:" labels (used in pillar)
#' Print method for class tabxplor_fmt
#'
#' @param x A fmt object.
#' @param ... Other parameters.
#' @param html Should html tags be added (to print confidence intervals as subscripts) ?
#' @param na How `NA`s should be printed. Default to `NA`.
#' @param special_formatting Set to `TRUE` to print more verbose results,
#' like indicating which is the reference row or col for differences.
#'
#' @return The fmt printed in a character vector.
#' @export
format.tabxplor_fmt <- function(x, ..., html = FALSE, na = NA,
                                special_formatting = FALSE) {

  out    <- get_num(x)
  na_out <- is.na(out)

  display <- get_display(x)
  nas  <- is.na(display)
  digits <- get_digits(x)
  digits[!nas & display == "n"] <- 0
  digits[!nas & display %in% c("or", "or_pct", "OR", "OR_pct") & # no "var" (used in chi2_table)
           digits < 2L] <- 2L


  ok <- !na_out & !nas


  type    <- get_type(x)
  ci_type <- get_ci_type(x)

  pm <- stringi::stri_unescape_unicode("\\u00b1") # sign "plus minus"

  pct_or_ci     <- ok & display %in% c("pct", "pct_ci", "diff", "ci", "ctr") &
    !(display %in% c("ci", "diff") & type == "mean")
  pct_ci  <- ok & display == "pct_ci"
  mean_ci <- ok & display == "mean_ci"
  diff_mean <- ok & display == "diff" & type == "mean"

  disp_ci   <- display == "ci" & ci_type == "diff" & !nas
  plus_ci <- (pct_ci | mean_ci) # ci_pct_mean
  plus_disp_ci <- (plus_ci | disp_ci)
  # plus_ci <- (ci_pct_mean | disp_ci)# & !is.na(get_ci(x))

  #pct_or_pct_ci <- ok & display %in% c("pct", "pct_ci", "diff", "ctr")
  pct_no_ci     <- ok & display %in% c("pct", "diff", "ctr") & !(display == "diff" & type == "mean")
  diff_pct      <- ok & display == "diff" & type != "mean"
  n_wn          <- ok & (display %in% c("n", "wn", "mean", "mean_ci", "var", "rr", "or", "or_pct",
                                        "OR", "OR_pct") |
                           (display == "ci" & type == "mean") )
  type_ci       <- ok & display == "ci"
  pvalue        <- ok & display == "pvalue"

  out[pct_or_ci] <- out[pct_or_ci] * 100
  digits[diff_mean] <- dplyr::if_else(digits[diff_mean] == 0, 1, digits[diff_mean])


  ci_print_moe <- getOption("tabxplor.ci_print") == "moe"
  if (any(plus_ci | disp_ci)) {
    if (any(plus_ci) & ci_print_moe) {
      # Phase 3a: the +/- moe shows the conservative LARGER arm (Wilson bounds are asymmetric).
      ci <- dplyr::if_else(condition = mean_ci[plus_ci],
                           true  = get_ci_moe(x)[plus_ci] ,
                           false = get_ci_moe(x)[plus_ci] * 100)

      ci_print_trim <- function(x) {
        x <- stringr::str_remove_all(x, paste0("^", pm, "0$|^", pm, "0.0+$|^", pm, "-0.0+$|^",
                                               pm, "NA"))
        stringr::str_pad(x, max(stringr::str_length(x)))
      }


      # ci_print_pad <- function(x) {
      #   stringr::str_pad(x, max(stringr::str_length(x)))
      # }

      out_ci <-
        paste0(print_num(out[plus_ci], digits[plus_ci]),
               dplyr::if_else(pct_ci[plus_ci], "%", ""),
               ci_print_trim(paste0(pm, sprintf(
                 paste0("%-0.",
                        digits[plus_ci] + dplyr::if_else(pct_ci[plus_ci] & digits[plus_ci] == 0, 1L, 0L),
                        "f"), ci
               )) ) %>% ci_html_subscript(html = html)
        )

    } else if (any(plus_disp_ci) ) { # !ci_print_moe
      # Phase 3a: read the real asymmetric bounds ci_inf/ci_sup directly (Wilson/Newcombe/AC/
      # Welch-t) instead of reconstructing a symmetric bracket from the half-width. This also
      # resolves the former WS2 mean-scaling FIXME -- the stored mean-diff bounds are absolute.
      lower <- dplyr::if_else(plus_disp_ci[plus_disp_ci] & type == "mean",
                              get_ci_inf(x)[plus_disp_ci],
                              get_ci_inf(x)[plus_disp_ci] * 100)
      upper <- dplyr::if_else(plus_disp_ci[plus_disp_ci] & type == "mean",
                              get_ci_sup(x)[plus_disp_ci],
                              get_ci_sup(x)[plus_disp_ci] * 100)

      # The estimate the bracket is centred on -- shown when the rounded bounds coincide.
      ref_for_ci <- dplyr::if_else(
        disp_ci[plus_disp_ci],
        true  = dplyr::if_else(plus_disp_ci[plus_disp_ci] & type == "mean",
                               true  =  get_diff(x)[plus_disp_ci],
                               false =  get_diff(x)[plus_disp_ci] * 100 ),
        false = out[plus_disp_ci])

      lower <- dplyr::if_else(pct_ci[plus_disp_ci], pmax(lower,   0), lower)
      upper <- dplyr::if_else(pct_ci[plus_disp_ci], pmin(upper, 100), upper)



      out_ci <- dplyr::if_else(
        condition = is.na(lower) | is.na(upper) |
          round(lower, digits[plus_disp_ci]) == round(upper, digits[plus_disp_ci]),
        true      = print_num(ref_for_ci, digits[plus_disp_ci]),
        false     = paste0("[",
                           sprintf(paste0("%-0.", digits[plus_disp_ci], "f"), lower),
                           ";", #", ",
                           #stringi::stri_unescape_unicode("\\u00b7"), # middle-point
                           sprintf(paste0("%-0.", digits[plus_disp_ci], "f"), upper),
                           "]"
        )
      )
      out_ci <- paste0(out_ci, dplyr::if_else(plus_disp_ci[plus_disp_ci] & type == "mean", "", "%")) # pct_ci[plus_disp_ci]
    }
  }
  # }

  out[!na_out] <- print_num(out[!na_out], digits[!na_out])
  out[na_out] <- NA
  if (any(plus_ci | disp_ci)) {
    if (any(plus_ci) | ci_print_moe) {
      out[plus_ci]  <- out_ci
    } else if (any(plus_disp_ci)) {
      out[plus_disp_ci] <- out_ci
    }
  }
  out[n_wn] <- out[n_wn] %>% prettyNum(big.mark = " ", preserve.width = "individual")
  out[pct_no_ci] <- paste0(out[pct_no_ci], "%") #pillar::style_subtle()

  if (any(pvalue)) {
    p    <- get_pct(x[pvalue])

    out[pvalue]    <- paste0(
      dplyr::if_else(
        p < 0.0001,
        true  = "<0.01",
        false = print_num(p * 100, digits = 2L)
      ),
      "%"
    )
  }

  out[diff_pct] <- dplyr::if_else(                   # "+" sign on positive pct diffs
    !stringr::str_detect(out[diff_pct], "^-"),  # !out[diff_pct] %in% c("0%", ) &
    true  = paste0("+", out[diff_pct]),
    false = out[diff_pct]
  )
  out[diff_mean] <- paste0(mult_sign, out[diff_mean]) # multiply sign on mean diffs


 if (ci_print_moe) {
   out[type_ci] <- switch(
     type,
     "n"       = ,
     "mean"    = paste0(pm, out[type_ci]),
     "row"     = ,
     "col"     = ,
     "all"     = ,
     "all_tabs"= paste0(pm, out[type_ci], "%") |> stringr::str_replace_all("%%", "%")
   )
 }



  if (special_formatting) {
    disp_diff   <- display == "diff" & !nas
    disp_moe    <- disp_ci & ci_print_moe # no if `ci_print = "ci"`
    disp_ctr    <- display == "ctr" & !nas
    disp_or     <- display %in% c("or", "OR") & !nas
    disp_or_pct <- display %in% c("or_pct", "OR_pct") & !nas
    disp_mean_sd <- display == "mean" & type == "mean" & !nas & !is.na(x$var)


    if (any (disp_mean_sd)) {
      sd <-
        print_num(get_num(set_display(set_var(x[disp_mean_sd],
                                              suppressWarnings(sqrt(get_var(x[disp_mean_sd]))) ), "var")),
                  digits = x[disp_mean_sd]$digits) # + 1L
      sd <- sd |>
        stringr::str_pad(width = max(stringr::str_length(sd)), side = "right")

      out[disp_mean_sd] <- paste0(out[disp_mean_sd], unbrk, "(", sigma_sign, sd, ")")
    }


    if (any(disp_diff)) {
      ref     <- get_reference(x[disp_diff], mode = "cells")
      reffmt  <- set_display(x[disp_diff],
                             ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
        format() #%>% stringr::str_trim()
      out[disp_diff] <- dplyr::if_else(ref,
                                       paste0("ref:", reffmt),
                                       out[disp_diff])
    }

    if (any(disp_moe)) {
      ref     <- get_reference(x[disp_moe], mode = "cells")
      reffmt  <- set_display(x[disp_moe],
                             ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
        format()
      out[disp_moe] <- dplyr::if_else(ref,
                                     paste0("ref:x-", reffmt),
                                     out[disp_moe])
    }

    if (any(disp_ctr)) {
      comp    <- get_comp_all(x)
      totcol  <- is_totcol(x)
      totrows <- is_totrow(x)
      tottabs <- is_tottab(x)

      mctr <- if (comp) {
        disp_ctr & totrows & tottabs & !totcol
      } else {
        disp_ctr & totrows & !totcol
      }
      out[mctr] <- paste0("mean:", stringr::str_trim(out[mctr])) %>%
        stringr::str_remove("mean:Inf%|NA")
    }

    if (any(disp_or)) {
      # refcol  <- is_refcol(x)
      refer   <- get_reference(x[disp_or], mode = "all_totals")
      reffmt  <- set_display(x[disp_or], "pct") |> # ifelse(refcol, "pct", "rr")
        set_digits(0L) |> format() #%>% stringr::str_trim()
      reffmt <- suppressWarnings(
        stringr::str_pad(reffmt,
                         suppressWarnings(
                           max(stringr::str_length(reffmt), na.rm = TRUE)
                         )
        )
      )
      out[disp_or] <- dplyr::if_else(
        refer & !is.na(reffmt),
        paste0(stringr::str_replace(out[disp_or], "1.0+", "1"),
               " (", reffmt, ")"),
        out[disp_or]
      )
      # out[disp_or] <- dplyr::case_when(
      #   ref & type == "row" & refcol ~ paste0("1 (ref)"),
      #   ref & type == "row"          ~ paste0("1 (rel ", reffmt, ")"),
      #   ref & type == "col" & refrows~ paste0("1 (ref)"),
      #   ref & type == "col"          ~ paste0("1 (rel ", reffmt, ")"),
      #   TRUE                         ~ out[disp_or]
      # )
    }

    if (any(disp_or_pct)) {
      reffmt  <- set_display(x[disp_or_pct], "pct") |> set_digits(0L) |> format()
      out[disp_or_pct] <- paste0(out[disp_or_pct], " (", reffmt, ")")
    }
  }

  # Phase 3a: append significance stars (universal CI-inclusion) after the cell value, wherever
  # a per-cell pvalue was stored (diff-type CIs). Single source of truth -> flows to console,
  # tab_md() and tab_kable(). get_stars() is "" for NA pvalue; gated by the option so a table
  # built with stars can still be printed without them. See dev/tabxplor_1.4.0_decisions.md §20.
  if (isTRUE(getOption("tabxplor.stars", TRUE))) {
    st  <- get_stars(x)
    add <- !is.na(out) & nzchar(st)
    out[add] <- paste0(out[add], st[add])
  }

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

  out     <- format(x, special_formatting = TRUE)
  display <- get_display(x)
  nas     <- is.na(display)
  color   <- get_color(x)
  color_bg <- get_color_bg(x)                        # Phase 5: the background channel measure
  type    <- get_type(x)
  #totcol  <- is_totcol(x)
  totrows <- is_totrow(x)
  #tottabs <- is_tottab(x)





  #
  #   comp <- get_comp_all(x)
  #
  #   ci_type   <- get_ci_type(x)
  #   pct       <- get_type(x)

  #
  #   disp_diff <- display == "diff" & !nas
  #   disp_ci   <- display == "ci" & ci_type == "diff" & !nas
  #   disp_ctr  <- display == "ctr" & !nas
  #   disp_or   <- display == "or" & !nas
  #   disp_or_pct<-display == "or_pct" & !nas
  #
  #   if (any(disp_diff)) {
  #     ref     <- get_reference(x[disp_diff], mode = "cells")
  #     reffmt  <- set_display(x[disp_diff],
  #                            ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
  #       format() #%>% stringr::str_trim()
  #     out[disp_diff] <- dplyr::if_else(ref,
  #                                      paste0("ref:", reffmt),
  #                                      out[disp_diff])
  #   }
  #
  #   if (any(disp_ci)) {
  #     ref     <- get_reference(x[disp_ci], mode = "cells")
  #     reffmt  <- set_display(x[disp_ci],
  #                            ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
  #       format()
  #     out[disp_ci] <- dplyr::if_else(ref,
  #                                    paste0("ref:x-", reffmt),
  #                                    out[disp_ci])
  #   }
  #
  #   if (any(disp_ctr)) {
  #     mctr <- if (comp) {
  #       disp_ctr & totrows & tottabs & !totcol
  #     } else {
  #       disp_ctr & totrows & !totcol
  #     }
  #     out[mctr] <- paste0("mean:", stringr::str_trim(out[mctr])) %>%
  #       stringr::str_remove("mean:Inf%|NA")
  #   }
  #
  #   if (any(disp_or)) {
  #     # refcol  <- is_refcol(x)
  #     ref     <- get_reference(x[disp_or], mode = "all_totals")
  #     reffmt  <- set_display(x[disp_or], "pct") %>% # ifelse(refcol, "pct", "rr")
  #       set_digits(0L) |> format() #%>% stringr::str_trim()
  #     reffmt <- stringr::str_pad(reffmt, max(stringr::str_length(reffmt)) )
  #     out[disp_or] <- dplyr::if_else(
  #       ref,
  #       paste0(stringr::str_replace(out[disp_or], "1.0+", "1"),
  #             " (", reffmt, ")"),
  #       out[disp_or]
  #     )
  #     # out[disp_or] <- dplyr::case_when(
  #     #   ref & type == "row" & refcol ~ paste0("1 (ref)"),
  #     #   ref & type == "row"          ~ paste0("1 (rel ", reffmt, ")"),
  #     #   ref & type == "col" & refrows~ paste0("1 (ref)"),
  #     #   ref & type == "col"          ~ paste0("1 (rel ", reffmt, ")"),
  #     #   TRUE                         ~ out[disp_or]
  #     # )
  #   }
  #
  #   if (any(disp_or_pct)) {
  #     reffmt  <- set_display(x[disp_or_pct], "pct") |> set_digits(0L) |> format()
  #     out[disp_or_pct] <- paste0(out[disp_or_pct], " (", reffmt, ")")
  #   }



  # DESIGN: color="contrib" needs total rows because the per-(sub)table MEAN contribution
  # to variance is stored ON the total row (see get_mean_contrib), not in each cell.
  if (color == "contrib" & !any(totrows)) warning(
    "cannot print color == 'contrib' with no total rows to store ",
    "information about mean contributions to variance"
  )  # store mean_contrib in a vctrs::field of fmt ? ----

  na_out  <- is.na(out)
  ok      <- !na_out & !nas


  has_text  <- !is.na(color)    && ! color    %in% c("no", "")
  has_bg    <- !is.na(color_bg) && ! color_bg %in% c("no", "")
  if ((has_text || has_bg) & !(has_text && color == "contrib" & !any(totrows))) {
    # Phase 5 engine: two integer slot vectors (text + background channel; 0 = uncolored). The
    # text channel uses the current-option palette (back-compat with color_style_type); the
    # background channel always uses the pale bg palette, stacked on top (a cell can carry both).
    channels     <- fmt_color_channels(x)
    text_crayons <- get_color_style()                  # current type/theme/24-bit options
    bg_crayons   <- get_color_style(type = "bg")

    for (s in sort(unique(channels$text_slot[channels$text_slot > 0L & ok]))) {
      cells <- ok & channels$text_slot == s
      out[cells] <- text_crayons[[s]](out[cells])
    }
    for (s in sort(unique(channels$bg_slot[channels$bg_slot > 0L & ok]))) {
      cells <- ok & channels$bg_slot == s
      out[cells] <- bg_crayons[[s]](out[cells])
    }
    totals <- get_reference(x, mode = "all_totals") #c("cells", "lines")

    # Cells matching no break on EITHER channel are greyed (style_subtle) so colored cells stand
    # out; reference/total cells are exempt, staying full-strength as reading anchors.
    unselected <- channels$text_slot == 0L & channels$bg_slot == 0L
    out[ok & unselected & !totals] <-  #fmtgrey3
      pillar::style_subtle(out[ok & unselected & !totals])

    #Columns with no color
  } else {
    # DESIGN: uncolored columns only grey out zeros here. Styling totals with bold /
    # underline / borders was tried (commented below) but rejected: bold offsets column
    # widths in the console unaesthetically. The underline+"|" was the border-imitation try.
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

  pvalues <- out[!nas & display == "pvalue"]
  p_values <- get_num(x)[!nas & display == "pvalue"]

  # Non-significant p-values (>= 5%) print red (neg5), significant ones green (pos5):
  # red warns the reader the (sub)table may not differ from the independence hypothesis.
  out[!nas & display == "pvalue"] <-
    dplyr::if_else(condition = p_values >= 0.05,
                   true      = color_style$neg5(pvalues),
                   false     = color_style$pos5(pvalues) )

  pillar::new_pillar_shaft_simple(out, align = "right", na = "")
}


#' mutate method to access vctrs::fields of tabxplor_fmt vectors
#' @importFrom dplyr mutate
#' @method mutate tabxplor_fmt
#' @param .data A tabxplor_fmt column.
#' @param ... Name-value pairs.
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
  # DESIGN: $wn falls back to the raw count n when there are no weighted counts (same
  # fallback as get_wn() — keep the two in sync). $ci is no longer a stored field (Phase 1a
  # dropped it): it is recomputed from the ci_inf/ci_sup bounds by get_ci(), so user code
  # reading $ci keeps working. ($ratio, $ci_inf, $ci_sup, $pvalue, $tot_n are real fields
  # and resolve via the proxy; $rr is gone — renamed $ratio.)
  if (name == "wn" & all(is.na( dplyr::pull(vctrs::vec_proxy(x), "wn")))) {
    dplyr::pull(vctrs::vec_proxy(x), "n")

  } else if (name == "ci") {
    get_ci(x)

  } else if (name == "tot_wn") {
    get_tot_wn(x)

  } else {
    dplyr::pull(vctrs::vec_proxy(x), name)
  }

}

# WARNING: do NOT add a `[[.tabxplor_fmt` method. It was tried (commented below) but broke
# dplyr::last() on fmt vectors, which relies on the default `[[`.
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



# ============================================================================================
# Phase 5 findInterval color ENGINE (Step 3) -- replaces fmt_color_selection / keep_last_break /
# select_in_color_style. Per column, per channel:
#   fmt_color_plan(x, channel)  -> the measure, per-cell score, significance gate, positive
#                                  breaks and the level->slot maps (per direction).
#   fmt_color_slots(x, plan)    -> fold the score to a magnitude that grows away from its neutral
#                                  center, findInterval() against the positive breaks, split by
#                                  direction into palette slots (0 = uncolored). C-level, no
#                                  per-cell reduce (the old keep_last_break hotspot is gone).
#   fmt_color_channels(x)       -> list(text_slot, bg_slot): the only artifact consumers map to
#                                  crayon/hex. (bg wired at Step 4.)
# Significance gates read the Phase-3a ci_inf/ci_sup bounds. See dev/new_colors_UI.md §8-9.
# Byte-identity gate: factor "diff" (incl. the x2), "contrib", "OR", and the mean CI-gated modes
# are reproduced exactly; numeric "diff" (Glass's delta) and the pct CI-gated modes change
# consciously (asymmetric-interval fix). Locked by test-color-golden.R.
# ============================================================================================

# The canonical break scales for a column (per-table override folded in at Step 4).
#' @keywords internal
color_scales <- function(x) {
  sc <- getOption("tabxplor.color_breaks")
  if (is.null(sc) || is.null(sc$pct_diff)) sc <- default_color_scales()
  sc
}

# Map the legacy scalar `color` attribute + column type to (measure, policy). Step 4 replaces
# this with the per-channel color / color_signif attributes; kept here so Step 3 can reroute the
# console + fmt_get_color_code byte-identically for the locked modes.
#' @keywords internal
color_measure_policy <- function(color, type) {
  measure <- dplyr::case_when(
    color %in% c("diff", "diff_ci", "after_ci", "ci") ~ "diff",
    color %in% c("OR", "or")                          ~ "or",
    color == "contrib"                                ~ "contrib",
    color == "ratio"                                  ~ "ratio",
    TRUE                                              ~ ""
  )
  policy <- dplyr::case_when(
    color == "diff_ci"             ~ "grey_non_signif",
    color %in% c("after_ci", "ci") ~ "color_all_signif",
    TRUE                           ~ "ignore"
  )
  list(measure = measure, policy = policy, single0 = color == "ci")
}

#' @keywords internal
fmt_color_plan <- function(x, channel = c("text", "bg"), color = NULL, signif = NULL) {
  channel <- match.arg(channel)
  n    <- length(x)
  type <- get_type(x)
  # `channel` selects the SLOT TABLE / palette family (text vs bg spread intensities differently).
  # The MEASURE is the `color` arg when given, else the text-channel measure -- so
  # fmt_get_color_code(type="bg") still renders the text selection in the bg palette (golden), and
  # fmt_color_channels() passes get_color_bg() explicitly for the background channel.
  if (is.null(color)) color <- get_color(x)
  if (length(color) == 0L || is.na(color[1]) || color[1] %in% c("", "no")) return(NULL)

  mp      <- color_measure_policy(color[1], type)
  measure <- mp$measure
  if (measure == "") return(NULL)
  # policy: an explicit `signif` arg wins; else the old combined strings (diff_ci/after_ci/ci)
  # carry their implied policy (transition, until Step 4d decodes them at the arg boundary); else
  # the stored per-column color_signif attribute.
  policy  <- if (!is.null(signif)) signif
             else if (color[1] %in% c("diff_ci", "after_ci", "ci")) mp$policy
             else get_color_signif(x)

  is_mean <- type %in% c("mean", "n")
  sc      <- color_scales(x)
  scale   <- switch(measure,
                    "diff"    = if (is_mean) sc$mean_diff  else sc$pct_diff,
                    "ratio"   = if (is_mean) sc$mean_ratio else sc$pct_ratio,
                    "or"      = sc$mean_ratio,
                    "contrib" = sc$contrib)
  center <- scale$center
  strict <- scale$strict

  # observed per-cell quantity
  raw <- switch(measure,
                "diff"    = get_diff(x),
                "ratio"   = get_ratio(x),
                "or"      = get_or(x),
                "contrib" = dplyr::if_else(is_totrow(x), NA_real_,
                                           get_ctr(x) / get_mean_contrib(x)))

  # numeric diff is standardized (Glass's delta) unless absolute unit breaks were supplied
  sd_ref <- NULL
  if (measure == "diff" && is_mean && isTRUE(scale$std)) {
    sd_ref      <- sqrt(get_ref_var(x))
    raw         <- raw / sd_ref
    raw[!is.finite(raw)] <- NA_real_        # sd_ref 0/NA -> undefined -> uncolored
  }

  # significance from the Phase-3a bounds (only a diff-type interval is meaningful here)
  has_diff_ci <- get_ci_type(x) %in% c("diff", "diff_row", "diff_col")
  sig_pos <- has_diff_ci & get_ci_inf(x) > 0
  sig_neg <- has_diff_ci & get_ci_sup(x) < 0
  sig_pos[is.na(sig_pos)] <- FALSE
  sig_neg[is.na(sig_neg)] <- FALSE

  if (policy == "color_all_signif") {
    floor_q <- dplyr::case_when(sig_pos ~ get_ci_inf(x),
                                sig_neg ~ get_ci_sup(x),
                                TRUE    ~ NA_real_)
    if (measure == "diff" && is_mean && isTRUE(scale$std)) floor_q <- floor_q / sd_ref
    score <- floor_q
    gate  <- !is.na(floor_q)
  } else if (policy == "grey_non_signif") {
    score <- raw
    dir0  <- if (center == 1) dplyr::case_when(raw > 1 ~ 1L, raw < 1 ~ -1L, TRUE ~ 0L)
             else sign(raw)
    gate  <- (dir0 > 0L & sig_pos) | (dir0 < 0L & sig_neg)
    gate[is.na(gate)] <- FALSE
  } else {                                   # ignore
    score <- raw
    gate  <- !is.na(raw)
    if (measure == "contrib") gate <- gate & !is_totrow(x)
  }

  pos_breaks <- if (isTRUE(mp$single0)) c(0) else scale$pos
  slots      <- build_slots(length(pos_breaks), channel)

  # Legacy in-text x2 (byte-identity for the scalar color="diff" on factors): the x2 ratio
  # currently colors slot 11 wherever the diff is not already at its strongest break. It rides
  # BOTH the text and bg palettes today (select_in_color_style injected it regardless), so it is
  # channel-independent here. Step 4 moves it to the dedicated background channel.
  x2 <- NULL
  if (measure == "diff" && !is_mean && policy == "ignore" && length(sc$pct_ratio$pos) == 1L) {
    x2 <- list(v = sc$pct_ratio$pos[1], slot = 11L,
               rr = get_ratio(x),       # §3: the reference-relative ratio (repointed off `mean`)
               top = slots$pos_slots[length(pos_breaks) + 1L])
  }

  list(measure = measure, policy = policy, score = score, center = center, strict = strict,
       pos_breaks = pos_breaks, pos_slots = slots$pos_slots, neg_slots = slots$neg_slots,
       gate = gate, x2 = x2)
}

#' @keywords internal
fmt_color_slots <- function(x, plan) {
  n <- length(x)
  if (is.null(plan)) return(integer(n))
  score <- plan$score

  if (plan$center == 1) {                    # multiplicative: fold around 1
    mag <- dplyr::if_else(score >= 1, score, 1 / score)
    dir <- dplyr::case_when(score > 1 ~ 1L, score < 1 ~ -1L, TRUE ~ 0L)
  } else {                                    # additive: fold around 0
    mag <- abs(score)
    dir <- dplyr::case_when(score > 0 ~ 1L, score < 0 ~ -1L, TRUE ~ 0L)
  }
  mag[!is.finite(mag)] <- NA_real_
  dir[is.na(dir)]      <- 0L
  level <- findInterval(mag, plan$pos_breaks, left.open = plan$strict)
  level[is.na(level)]  <- 0L

  slot <- integer(n)
  posi <- dir > 0L
  negi <- dir < 0L
  slot[posi] <- plan$pos_slots[level[posi] + 1L]
  slot[negi] <- plan$neg_slots[level[negi] + 1L]

  if (!is.null(plan$x2)) {
    rr <- plan$x2$rr
    ov <- posi & slot != plan$x2$top & !is.na(rr) & rr > plan$x2$v
    ov[is.na(ov)] <- FALSE
    slot[ov] <- plan$x2$slot
  }

  slot[!plan$gate] <- 0L
  slot
}

#' @keywords internal
fmt_color_channels <- function(x) {
  # text channel = the primary measure on the text slot table; background channel = the second
  # measure (get_color_bg, NA when absent) on the bg slot table. Each is an integer slot vector.
  list(text_slot = fmt_color_slots(x, fmt_color_plan(x, "text", color = get_color(x))),
       bg_slot   = fmt_color_slots(x, fmt_color_plan(x, "bg",   color = get_color_bg(x))))
}

# The single slot -> hex mapping shared by the exporters (tab_kable / tab_plot / tab_xl). Returns the
# per-cell colour code of BOTH channels (NA where uncoloured on that channel), plus the raw slot
# vectors (for bold / gate decisions). The text channel is rendered in the `color_type` palette
# family (the exporter's global text-vs-bg toggle, default "text"); the background channel always
# uses the "bg" palette. This mirrors pillar_shaft.tabxplor_fmt's two-channel logic, so console and
# exports render identical colours. (fmt_get_color_code stays single-channel for the golden.)
#' @keywords internal
fmt_channel_codes <- function(x, color_type = "text", theme = "light", html_24_bit = NULL) {
  html_24_bit <-
    if (is.null(html_24_bit)) {getOption("tabxplor.color_html_24_bit")} else {html_24_bit}
  n  <- length(x)
  ch <- fmt_color_channels(x)

  text_styles <- get_color_style("color_code", type = color_type, theme = theme,
                                 html_24_bit = html_24_bit)
  bg_styles   <- get_color_style("color_code", type = "bg", theme = theme,
                                 html_24_bit = html_24_bit)

  text <- rep(NA_character_, n)
  bg   <- rep(NA_character_, n)
  tsel <- ch$text_slot > 0L
  bsel <- ch$bg_slot   > 0L
  # historical output is upper-case hex (cf. fmt_get_color_code).
  text[tsel] <- toupper(unname(text_styles[ch$text_slot[tsel]]))
  bg[bsel]   <- toupper(unname(bg_styles[ch$bg_slot[bsel]]))

  list(text = text, bg = bg, text_slot = ch$text_slot, bg_slot = ch$bg_slot)
}




#' @keywords internal
tab_color_legend <- function(x, colored = TRUE, mode = c("console", "html"),
                             html_theme = NULL, html_type = NULL, html_24_bit,
                             text_color = NULL, grey_color = NULL,
                             add_color_and_diff_types = FALSE, all_variables_names = FALSE) {
  # PURPOSE: build the human-readable colour legend, driven by the SAME per-channel plan
  # (fmt_color_plan) and slot->palette path the cells use, so legend and cells can never disagree.
  # Two channels are described independently (text measure + background measure); each measure's
  # thresholds are read from the canonical scales and coloured by their palette slots. Numeric
  # `diff` shows the standardized (Glass's delta, SD) thresholds; ratios show x/1 operators.
  mode        <- mode[1]
  if (missing(html_24_bit)) html_24_bit <- getOption("tabxplor.color_html_24_bit")
  html_theme  <- if (is.null(html_theme)) getOption("tabxplor.color_style_theme") else html_theme

  # keep only coloured fmt columns (text OR background channel)
  is_f  <- purrr::map_lgl(x, is_fmt)
  ct    <- get_color(x)
  cbg   <- get_color_bg(x)
  keep  <- is_f & ((!is.na(ct)  & !ct  %in% c("no", "")) |
                   (!is.na(cbg) & !cbg %in% c("no", "")))
  if (!any(keep)) return(NULL)

  # group columns by col_var (colour is uniform within a col_var); keep the first coloured column
  # of each col_var as its representative.
  col_vars_levels <- tab_get_vars(x)$col_vars_levels %>%
    purrr::discard(names(.) == "all_col_vars")
  kept_names <- names(x)[keep]
  reps <- col_vars_levels %>%                                # map_chr keeps the col_var names
    purrr::map_chr(~ {
      cols <- .x[.x %in% kept_names]
      if (length(cols) == 0) NA_character_ else cols[1]
    })
  reps <- reps[!is.na(reps)]
  if (length(reps) == 0) return(NULL)

  # ---- formatting helpers --------------------------------------------------------------------
  format_g <- function(v) trimws(formatC(v, format = "fg", digits = 3, drop0trailing = TRUE))

  ref_label <- function(r) {
    if (length(r) == 0 || is.na(r)) return("")
    ri <- suppressWarnings(as.integer(r))
    if (!is.na(ri)) return(paste0("row", ri))
    switch(r, "first" = "1st", "tot" = "tot", r)
  }

  measure_word <- function(measure, is_mean, std) {
    switch(measure,
           "diff"    = if (is_mean && std) "diff/sd" else "diff",
           "ratio"   = "ratio",
           "or"      = "OR",
           "contrib" = "contrib",
           measure)
  }

  brk_label <- function(measure, v, dir, is_mean, std) {
    if (v == 0) return("signif")                             # single-0 break = the old "ci" look
    if (measure == "diff" && !is_mean) {
      lab <- paste0(sprintf("%1.0f", abs(v) * 100), "%")
      if (dir > 0) paste0("+", lab) else paste0("-", lab)
    } else if (measure == "diff" && is_mean && std) {
      if (dir > 0) paste0("+", format_g(v), "sd") else paste0("-", format_g(v), "sd")
    } else if (measure == "diff" && is_mean) {
      if (dir > 0) paste0("+", format_g(v)) else paste0("-", format_g(v))
    } else if (measure %in% c("ratio", "or")) {
      if (dir > 0) paste0(cross, format_g(v)) else paste0("/", format_g(v))
    } else if (measure == "contrib") {
      paste0(cross, format_g(v))
    } else {
      as.character(v)
    }
  }

  # colour one label with a palette slot, for the current mode / channel
  paint <- function(label, slot, palette_type) {
    if (!isTRUE(colored) || is.na(slot) || slot == 0L) return(label)
    if (mode == "console") {
      get_color_style("crayon", type = palette_type)[[slot]](label)
    } else {
      hex <- get_color_style("color_code", type = palette_type, theme = html_theme,
                             html_24_bit = if (palette_type == "text") html_24_bit else NULL)[[slot]]
      if (palette_type == "text") kableExtra::text_spec(label, color = hex)
      else                        kableExtra::text_spec(label, background = hex)
    }
  }

  # one channel's coloured threshold string, from its plan (NULL -> no channel)
  channel_scale <- function(plan, is_mean, std, palette_type) {
    if (is.null(plan)) return(NA_character_)
    measure <- plan$measure
    K       <- length(plan$pos_breaks)
    neg <- purrr::map_chr(seq_len(K), ~ paint(
      brk_label(measure, plan$pos_breaks[.x], -1L, is_mean, std),
      plan$neg_slots[.x + 1L], palette_type))
    pos <- purrr::map_chr(seq_len(K), ~ paint(
      brk_label(measure, plan$pos_breaks[.x], +1L, is_mean, std),
      plan$pos_slots[.x + 1L], palette_type))
    x2 <- character(0)
    if (!is.null(plan$x2)) x2 <- paint(paste0(cross, format_g(plan$x2$v)), plan$x2$slot, "text")
    labs <- c(rev(neg), pos, x2)
    labs <- labs[nzchar(labs)]
    paste(labs, collapse = " ")
  }

  policy_note <- function(policy) {
    switch(policy,
           "grey_non_signif"  = " [signif. only]",
           "color_all_signif" = " [signif., CI-floor]",
           "")
  }

  # ---- per-representative-column legend spec (imap: cn = column name, cv = col_var name) ------
  specs <- purrr::imap(reps, function(cn, cv) {
    col      <- x[[cn]]
    is_mean  <- get_type(col) %in% c("mean", "n")
    plan_txt <- fmt_color_plan(col, "text", color = get_color(col))
    plan_bg  <- fmt_color_plan(col, "bg",   color = get_color_bg(col))
    if (is.null(plan_txt) && is.null(plan_bg)) return(NULL)
    scales   <- color_scales(col)
    std      <- isTRUE(scales$mean_diff$std)
    policy   <- if (!is.null(plan_txt)) plan_txt$policy else "ignore"
    m_txt    <- if (!is.null(plan_txt)) plan_txt$measure else NA_character_
    m_bg     <- if (!is.null(plan_bg))  plan_bg$measure  else NA_character_
    reference <- ref_label(get_ref_type(col)[1])
    list(
      col_var = cv,
      sig     = paste(m_txt, m_bg, policy, reference, is_mean, std, sep = "\r"),
      is_mean = is_mean, std = std, policy = policy, ref = reference,
      m_txt = m_txt, m_bg = m_bg,
      txt = if (!is.null(plan_txt)) channel_scale(plan_txt, is_mean, std, "text") else NA_character_,
      bg  = if (!is.null(plan_bg))  channel_scale(plan_bg,  is_mean, std, "bg")   else NA_character_
    )
  }) %>% purrr::compact()
  if (length(specs) == 0) return(NULL)

  # ---- group col_vars sharing a signature, assemble one legend line each ---------------------
  grp   <- split(specs, purrr::map_chr(specs, "sig"))
  lines <- purrr::map_chr(grp, function(g) {
    s      <- g[[1]]
    vnames <- unique(purrr::map_chr(g, "col_var"))
    etc    <- if (!all_variables_names && length(vnames) > 3) ",..." else ""
    if (!all_variables_names) vnames <- utils::head(vnames, 3)
    names_txt <- paste0(paste(vnames, collapse = ", "), etc)

    if (add_color_and_diff_types) {
      cty <- paste0("[color:", s$m_txt,
                    if (!is.na(s$m_bg)) paste0("+", s$m_bg) else "", "]")
      dty <- if (!is.na(s$m_txt) && s$m_txt %in% c("diff", "ratio", "or"))
        paste0(" [ref:", s$ref, "]") else ""
      names_txt <- paste0(cty, dty, " ", names_txt)
    }

    ref_part <- if (!is.na(s$m_txt) && s$m_txt %in% c("diff", "ratio", "or") && nzchar(s$ref)) {
      paste0("/", s$ref)
    } else if (identical(s$m_txt, "contrib")) "/indep." else ""
    body <- character(0)
    if (!is.na(s$txt))
      body <- c(body, paste0(measure_word(s$m_txt, s$is_mean, s$std), ref_part, ": ", s$txt))
    if (!is.na(s$bg))
      body <- c(body, paste0("bg ", measure_word(s$m_bg, s$is_mean, s$std), ": ", s$bg))
    body <- paste0(paste(body, collapse = "; "), policy_note(s$policy))

    if (isTRUE(colored) && mode == "console") {
      names_txt <- pillar::style_subtle(paste0(names_txt, ": "))
    } else if (mode != "console" && !is.null(grey_color)) {
      names_txt <- kableExtra::text_spec(paste0(names_txt, ": "), color = grey_color)
    } else {
      names_txt <- paste0(names_txt, ": ")
    }
    paste0(names_txt, body)
  })

  unname(lines)
}
# tab_color_legend(tabs[[7]]) %>% cli::cat_line()
# tab_color_legend(tabs[[7]], colored = FALSE)

# Phase 5 (Step 2): the level -> palette-slot rule, replacing select_in_color_style's hand-tuned
# lookup + fragile hex-sniff. `channel` ("text"/"bg") picks the palette family EXPLICITLY (the
# sniff guessed it from pos1's hex, and its "#000033e" typo made bg_dark fall through to the
# text table -- a bug this rule fixes, so bg_dark coloring changes consciously at Step 3).
# `L` = 2*K = the number of signed break levels (K positive). Returns the slot index for each
# of the L levels, so that with few breaks the chosen intensities stay visually spread out.
# Byte-identical to select_in_color_style for the text family (locked by test-color-engine.R).
#' @keywords internal
color_slot_table <- function(L, channel = c("text", "bg")) {
  channel <- match.arg(channel)
  key <- as.character(L)
  if (channel == "bg") {
    switch(key,
           "0"  = integer(0),
           "1"  = 3L,
           "2"  = c(3L, 8L),
           "4"  = c(1L, 3L, 6L, 8L),
           "6"  = c(1L, 3L, 5L, 6L, 8L, 10L),
           "8"  = c(1L, 2L, 3L, 4L, 6L, 7L, 8L, 10L),
           "10" = 1:10,
           cli::cli_abort("Unsupported color break count L = {L} (max 5 breaks per side).")
    )
  } else {
    switch(key,
           "0"  = integer(0),
           "1"  = 3L,
           "2"  = c(3L, 8L),
           "4"  = c(3L, 5L, 8L, 10L),
           "6"  = c(3L, 4L, 5L, 8L, 9L, 10L),
           "8"  = c(2L, 3L, 4L, 5L, 7L, 8L, 9L, 10L),
           "10" = 1:10,
           cli::cli_abort("Unsupported color break count L = {L} (max 5 breaks per side).")
    )
  }
}

# Per-direction level->slot maps for K positive breaks: pos_slots[level+1] / neg_slots[level+1],
# with a leading 0 for the neutral (uncolored) level 0. The x2 ratio (slot 11) is injected
# separately by the engine's x2 override, not here.
#' @keywords internal
build_slots <- function(K, channel = c("text", "bg")) {
  channel <- match.arg(channel)
  if (K == 0L) return(list(pos_slots = 0L, neg_slots = 0L))
  base     <- color_slot_table(2L * K, channel)
  list(pos_slots = c(0L, base[seq_len(K)]),
       neg_slots = c(0L, base[K + seq_len(K)]))
}


#' @keywords internal
get_reference <- function(x, mode = c("cells", "lines", "all_totals")) {
  type        <- get_type(x)
  ref   <- get_ref_type(x)
  comp_all    <- get_comp_all(x)
  totcol      <- is_totcol(x)
  totrows     <- is_totrow(x)
  tottab_line <- is_tottab(x) & totrows

  refrows     <- is_refrow(x)
  refcol      <- is_refcol(x)
  tottab_ref  <- is_tottab(x) & refrows

  color       <- get_color(x)

  if (color %in% c("OR", "or")) {
    switch(mode[1],
           "cells"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ refrows               ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref            ,
             type == "col"                          ~ rep(refcol, length(x)),
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "lines"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ refrows               ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref            ,
             type == "col"                          ~ rep(refcol, length(x)),
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "all_totals" = dplyr::case_when(
             type %in% c("row", "mean") & ref == "tot" & !comp_all
             ~ totrows | refcol,

             type %in% c("row", "mean") & ref == "tot" &  comp_all
             ~ tottab_line | refcol,

             type == "col" & ref == "tot"     ~ totrows | refcol,

             type %in% c("row", "mean") & !comp_all ~ refrows | refcol      ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref | refcol   ,
             type == "col"                          ~ refrows | refcol      ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           )
    )

  } else if (ref == "tot") {
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

             type == "all_tabs" | (type %in% c("row", "mean") & comp_all)
             ~ tottab_line | totcol,
             # type == "col"                          ~ rep(totcol, length(x)),
             # type == "all"                          ~ totrows & totcol      ,
             # type == "all_tabs"                     ~ tottab_line & totcol  ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           )
    )
  } else {
    # DESIGN: the three modes pick different cell sets relative to the baseline:
    #   "cells"      = the individual reference CELLS each cell compares to, EXCLUDING the
    #                  totals themselves (drives diffs and "ref:" labels).
    #   "lines"      = the whole reference ROWS/COLS (refrows incl. totcol / the refcol).
    #   "all_totals" = union of ALL total AND reference cells — the reading anchors kept
    #                  full-strength (exempt from greying) in pillar_shaft.
    # comp_all switches the row/mean reference from the subtable total (refrows) to the
    # total-table reference (tottab_ref).
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
  if (identical(sort(display), c("pct", "pvalue"))) display <- "pct"
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
  # DESIGN: common ptype of two fmt columns (drives c() / vec_c()). Any per-column
  # attribute that differs collapses to a neutral value: type->"mixed", col_var->
  # "several_vars", comp_all/totcol/refcol->FALSE, ref/ci_type/color->"". So binding
  # unlike fmt columns is allowed but loses the mismatched metadata (by design).
  type_x       <- get_type(x)
  same_type    <- type_x == get_type(y)
  comp_x       <- get_comp_all(x, replace_na = FALSE)
  comp_y       <- get_comp_all(y, replace_na = FALSE)
  same_comp    <- comp_x == comp_y | (is.na(comp_x) & is.na(comp_y))
  diff_type_x  <- get_ref_type(x)
  same_diff_type <- diff_type_x == get_ref_type(y)
  ci_type_x    <- get_ci_type(x)
  same_ci_type <- ci_type_x == get_ci_type(y)
  col_var_x    <- get_col_var(x)
  same_col_var <- col_var_x == get_col_var(y)
  totcol_x     <- is_totcol(x)
  same_totcol  <- totcol_x == is_totcol(y)
  refcol_x     <- is_refcol(x)
  same_refcol  <- refcol_x == is_refcol(y)
  # Phase 5 (§9.1): read the FULL color attribute (length <= 2) -- reading get_color()=[1] here
  # would silently drop the background channel on every c()/bind/group. `==` recycles the shorter
  # (1 divides 2), so the reconciled result is length <= 2. color_signif reconciles like the other
  # scalar attributes.
  color_x      <- fmt_color_attr(x)
  same_color   <- color_x == fmt_color_attr(y)
  signif_x     <- get_color_signif(x)
  same_signif  <- signif_x == get_color_signif(y)
  #l            <- length(x)

  new_fmt(
    type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
    comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
    ref= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
    ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
    col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
    totcol   = dplyr::if_else(same_totcol , totcol_x , FALSE         ),
    refcol   = dplyr::if_else(same_refcol , refcol_x , FALSE         ),
    color    = dplyr::if_else(same_color  , color_x  , ""            ),
    color_signif = dplyr::if_else(same_signif, signif_x, "ignore")
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
#' @return A fmt vector
#' @export
vec_ptype2.double.tabxplor_fmt  <- function(x, y, ...) y # new_fmt() #double()
#' Find common ptype between fmt and integer
#' @param x A fmt vector
#' @param y An integer vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
vec_ptype2.tabxplor_fmt.integer <- function(x, y, ...) x # fmt() #double()
#' Find common ptype between integer and fmt
#' @param x An integer vector
#' @param y A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
vec_ptype2.integer.tabxplor_fmt <- function(x, y, ...) y # new_fmt() #double()

# Conversions :
#' Convert fmt into fmt
#' @param x A fmt vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
vec_cast.tabxplor_fmt.tabxplor_fmt  <- function(x, to, ...)
  new_fmt(display   = get_display (x),
          n         = get_n       (x),
          wn        = get_wn      (x),
          pct       = get_pct     (x),
          diff      = get_diff    (x),
          ratio     = get_ratio   (x),
          digits    = get_digits  (x),
          ctr       = get_ctr     (x),
          mean      = get_mean    (x),
          var       = get_var     (x),
          ci_inf    = get_ci_inf  (x),
          ci_sup    = get_ci_sup  (x),
          pvalue    = get_pvalue  (x),
          or        = get_or      (x),
          tot_n     = get_tot_n   (x),

          in_totrow = is_totrow   (x),
          in_refrow = is_refrow   (x),
          in_tottab = is_tottab   (x),

          type      = get_type    (to),
          comp_all  = get_comp_all(to, replace_na = FALSE),
          ref = get_ref_type(to),
          ci_type   = get_ci_type (to),
          col_var   = get_col_var (to),
          totcol    = is_totcol   (to),
          refcol    = is_refcol   (to),
          color     = fmt_color_attr(to),          # full attribute (both channels)
          color_signif = get_color_signif(to)

  )

# DESIGN: numeric <-> fmt cast contract (matters for arithmetic, sorting and export):
#   double  -> fmt : a WEIGHTED-COUNT cell (display="wn", wn=x, n=NA)
#   integer -> fmt : a COUNT cell (n=x)
#   fmt -> double/integer/character : returns the CURRENTLY DISPLAYED field (get_num /
#     format), NOT pct or n. So ==, sorting and numeric coercion all act on whatever
#     `display` currently shows (see also vec_proxy_equal / vec_proxy_compare below).
#' Convert double into fmt
#' @param x A double vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
vec_cast.tabxplor_fmt.double   <- function(x, to, ...)
  fmt(n = NA_integer_            ,
      display   = "wn", wn = x     ,
      type      = get_type    (to),
      comp_all  = get_comp_all(to, replace_na = FALSE),
      ref = get_ref_type(to),
      ci_type   = get_ci_type (to),
      col_var   = get_col_var (to),
      totcol    = is_totcol   (to),
      refcol    = is_refcol   (to),
      color     = fmt_color_attr(to),
      color_signif = get_color_signif(to),

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
      ref = get_ref_type(to),
      ci_type  = get_ci_type (to),
      col_var  = get_col_var (to),
      totcol   = is_totcol   (to),
      refcol    = is_refcol   (to),
      color    = fmt_color_attr(to),
      color_signif = get_color_signif(to)

  ) #new_fmt(pct = as.double(x))
#' Convert fmt into integer
#' @param x A integer vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return An integer vector
#' @method vec_cast.integer tabxplor_fmt
#' @export
vec_cast.integer.tabxplor_fmt    <- function(x, to, ...) get_num(x) %>% as.integer() #vctrs::field(x, "pct") %>% as.integer()

#' Convert fmt into character
#' @param x A fmt vector
#' @param to A character vector
#' @param ... Other parameter
#' @return A character vector
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
#' @return A fmt vector
#' @method vec_arith.tabxplor_fmt default
#' @export
vec_arith.tabxplor_fmt.default <- function(op, x, y, ...) {
  vctrs::vec_arith_base(op, get_num(x), vctrs::vec_data(y))
  #stop_incompatible_op(op, x, y)
}

# positive_double <- function(n) n * sign(n)
# positive_integer <- function(n) as.integer(n * sign(n))

# DESIGN: fmt + fmt arithmetic operates on n, wn, pct fields. For means, recalculates
#   weighted mean. Resets diff/ci/ctr to NA (must be recomputed via tab_pct/tab_ci/tab_chi2).
#' @describeIn vec_arith.tabxplor_fmt vec_arith method for fmt + fmt
#' @return A fmt vector
#' @method vec_arith.tabxplor_fmt tabxplor_fmt
#' @export
vec_arith.tabxplor_fmt.tabxplor_fmt <- function(op, x, y, ...) {
  type_x       <- get_type(x)
  same_type    <- type_x == get_type(y)
  comp_x       <- get_comp_all(x, replace_na = FALSE)
  comp_y       <- get_comp_all(y, replace_na = FALSE)
  same_comp    <- comp_x == comp_y | (is.na(comp_x) & is.na(comp_y))
  diff_type_x  <- get_ref_type(x)
  same_diff_type <- diff_type_x == get_ref_type(y)
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
      ratio   = rep_NA_real,
      digits  = pmax(get_digits(x), get_digits(y)),
      ctr     = rep_NA_real, # ???
      mean    = vctrs::vec_arith_base(op, get_mean(x) * get_wn(x), get_mean(y) * get_wn(y)) /
        vctrs::vec_arith_base("+", get_wn(x) , get_wn(y) ),# weighted mean
      var     = rep_NA_real,
      ci_inf  = rep_NA_real,
      ci_sup  = rep_NA_real,
      pvalue  = rep_NA_real,
      or      = rep_NA_real,
      tot_n   = rep_NA_real,

      # FIXME: is the AND right? A cell stays "total" only if BOTH operands are total —
      # arguably it should follow x alone (x - a non-total y should probably stay total).
      in_totrow = is_totrow(x) & is_totrow(y),
      in_refrow = is_refrow(x) & is_refrow(y),
      in_tottab = is_tottab(x) & is_tottab(y),

      type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
      comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
      ref= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
      ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
      col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
      totcol   = FALSE                                                  ,
      refcol   = FALSE                                                  ,
      color    = fmt_color_attr(x),
      color_signif = get_color_signif(x)

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
      # FIXME: suspect. Unlike +/- (which recomputes a weighted mean), */ operates pct_x
      # against pct_y directly and drops mean to NA. Multiplying/dividing two percentage
      # fields is rarely meaningful; revisit what * and / on fmt should actually mean.
      pct    = vctrs::vec_arith_base(op, get_pct(x), get_pct(y)),
      diff   = rep_NA_real,
      ratio  = rep_NA_real,
      digits = pmax(get_digits(x), get_digits(y)),
      ctr    = rep_NA_real,
      mean   = rep_NA_real,
      var    = rep_NA_real,
      ci_inf = rep_NA_real,
      ci_sup = rep_NA_real,
      pvalue = rep_NA_real,
      or     = rep_NA_real,
      tot_n  = rep_NA_real,

      in_totrow = is_totrow(x),
      in_refrow = is_refrow(x),
      in_tottab = is_tottab(x),

      type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
      comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
      ref= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
      ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
      col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
      totcol   = FALSE                                                  ,
      refcol   = FALSE                                                  ,
      color    = fmt_color_attr(x),
      color_signif = get_color_signif(x)

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
#' @return A fmt vector
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
#' @return A fmt vector
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
#' @return A fmt vector
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
                         ratio  = NA_real_,
                         ctr    = NA_real_,
                         mean   = vctrs::vec_math_base("sum", get_mean(.x) * get_wn(.x), ...) /
                           vctrs:: vec_math_base("sum", get_wn(.x), ...),
                         var    = NA_real_,
                         ci_inf = NA_real_,
                         ci_sup = NA_real_,
                         pvalue = NA_real_,
                         or     = NA_real_,
                         tot_n  = NA_real_,

                         in_totrow = all(is_totrow(.x)),
                         in_refrow = all(is_refrow(.x)),
                         in_tottab = all(is_tottab(.x)), #any ?

                         type      = get_type    (.x),
                         comp_all  = get_comp_all(.x, replace_na = FALSE),
                         ref = get_ref_type(.x),
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
                          ratio   = NA_real_,
                          ctr     = NA_real_,
                          mean    = vctrs::vec_math_base("sum", get_mean(.x) * get_wn(.x), ...) /
                            vctrs::vec_math_base("sum", get_wn(.x), ...),
                          var     = NA_real_,
                          ci_inf  = NA_real_,
                          ci_sup  = NA_real_,
                          pvalue  = NA_real_,
                          or      = NA_real_,
                          tot_n   = NA_real_,

                          in_totrow = FALSE,
                          in_refrow = FALSE,
                          in_tottab = all(is_tottab(.x)), #any ?

                          type      = get_type    (.x),
                          comp_all  = get_comp_all(.x, replace_na = FALSE),
                          ref = get_ref_type(.x),
                          ci_type   = get_ci_type (.x),
                          col_var   = get_col_var (.x),
                          totcol    = is_totcol   (.x),
                          refcol    = is_refcol   (.x),
                          color     = get_color   (.x)
         ),
         vctrs::vec_math_base(.fn, get_num(.x), ...) )
}


