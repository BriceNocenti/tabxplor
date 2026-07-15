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
utils::globalVariables(c(":=", ".SD", ".N"))
# data.table NSE column symbols used in tab_plain()'s aggregation j-expressions:
utils::globalVariables(c("n", "wn"))
# Phase 3b test engine (R/tab-agg.R) data.table NSE column symbols:
utils::globalVariables(c("table_id", "row_id", "col_id", "o", "rowtot", "coltot", "ok",
                  "grandtot", "nr", "nc", "e", "contrib", "signed_contrib",
                  "statistic", "df", "min_e", "w", "group_id"))

# The `ctx` fields of the tab_build() pipeline (Phase 7d-ii). Each stage starts with
# `list2env(ctx, environment())` (R/tab.R: tab_setup / tab_prepare_pop / tab_aggregate /
# tab_transform / tab_assemble_tables / tab_assemble_output), which binds every ctx field as a
# local -- correct at run time, but invisible to codetools, which then reports each one as an
# undefined global. Listing them here is the only way to keep R CMD check quiet short of
# unpacking ~70 fields by hand in six functions.
utils::globalVariables(c(
  "by_table", "chi2", "chi2_num", "cleannames", "col_vars", "col_vars_num", "col_vars_quo",
  "col_vars_text", "color_ci", "color_ctr", "color_diff_OR", "color_num", "comp", "conf_level",
  "data", "digits", "fine_fused", "fine_num", "lv1", "method_cell", "method_diff", "na",
  "na_drop_all_quo", "na_num", "na_text", "names_prefix", "names_sort", "other_if_less_than",
  "other_level", "output", "pct", "pct_vect", "ref", "ref2", "remove_levels", "row_vars",
  "row_vars_quo", "spread_vars", "stars", "subtext", "tab_row_names", "tab_vars", "tab_vars_quo",
  "tabs_num", "tot_cols_type", "total_names", "totaltab", "totaltab_name", "totrow",
  "with_filter", "wt", "wt_quo", "add_n", "add_pct", "ci", "OR"))

# NSE column symbols in dplyr verbs over ordinary data frames:
#   `var`               -- reg_build()'s group_by(var) on the regression skeleton (R/tab_reg.R)
#   `name`/`size`/`color` -- tab_xl_plan_one()'s font/style plan tibbles (R/tab_xl.R)
utils::globalVariables(c("var", "name", "size", "color"))

# mirai daemon globals (R/tab-parallel.R): `tabx_opts`/`tabx_ship` are list2env()'d into each
# daemon's .GlobalEnv by tab_pmap(); `.stop` is mirai_map()'s own collection selector.
utils::globalVariables(c("tabx_opts", "tabx_ship", ".stop"))


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
#' @param or The odds ratio (for a 3+ level variable, the OR of each level versus the reference),
#'   as a double vector the length of \code{n}.
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
#' (\code{"ignore"} / \code{"grey_non_signif"} / \code{"guaranteed_effect"}). See \code{\link{tab}}.
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
  # Phase 10i-A: resolve composite templates ("{pct} (n={n})") to their PRIMARY field before the
  # dispatch masks -- byte-identical (and one fixed grepl) when the column carries no composite.
  display <- display_primary(get_display(x))
  nas     <- is.na(display)
  out[!nas & display == "wn"     ] <- get_wn  (x)[!nas & display == "wn"     ]
  out[!nas & display == "pct"    ] <- get_pct (x)[!nas & display == "pct"    ]
  out[!nas & display == "pvalue" ] <- get_pct (x)[!nas & display == "pvalue" ]
  out[!nas & display == "diff"   ] <- get_diff(x)[!nas & display == "diff"   ]
  out[!nas & display == "coef"   ] <- get_diff(x)[!nas & display == "coef"   ]  # Phase 12c: raw regression coef -> diff field
  out[!nas & display == "gof"    ] <- get_diff(x)[!nas & display == "gof"    ]  # Phase 12f: model-fit stat (N/R2/AIC/...) -> diff field
  out[!nas & display == "pct_ci" ] <- get_pct (x)[!nas & display == "pct_ci" ]
  out[!nas & display == "ctr"    ] <- get_ctr (x)[!nas & display == "ctr"    ]
  out[!nas & display == "mean"   ] <- get_mean(x)[!nas & display == "mean"   ]
  out[!nas & display == "mean_ci"] <- get_mean(x)[!nas & display == "mean_ci"]
  out[!nas & display == "var"    ] <- get_var (x)[!nas & display == "var"    ]
  out[!nas & display == "ci"     ] <- get_ci   (x)[!nas & display == "ci"     ]
  out[!nas & display == "rr"     ] <- get_ratio(x)[!nas & display == "rr"     ]
  out[!nas & display %in% c("or", "OR")] <- get_or(x)[!nas & display %in% c("or", "OR")     ]
  out[!nas & display == "or_pct" ] <- get_or  (x)[!nas & display == "or_pct" ]
  # Phase 12h: est_ci = "<estimate> [ci_inf; ci_sup]" (regression OR / beta with a visible interval).
  # The PRIMARY number is the point estimate: the OR (ci_type=="or") or the coefficient (else). ci_type
  # is a per-column scalar attribute, so one branch per column (never mixed within a column).
  est_ci_m <- !nas & display == "est_ci"
  if (any(est_ci_m)) {
    out[est_ci_m] <- (if (identical(as.character(get_ci_type(x))[1], "or")) get_or(x) else get_diff(x))[est_ci_m]
  }
  # Phase 7g: "blank" is a display-only mask (the n_min helper sets it on small-base cells);
  # it carries NO number (format() emits ""), while the underlying n/pct/tot_n stay intact so
  # the mask is fully reversible by resetting `display`.
  out[!nas & display == "blank"  ] <- NA_real_
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
  # Phase 10i-A: a composite cell writes back to its PRIMARY field (the first {token}).
  display <- display_primary(get_display(x))
  nas     <- is.na(display)
  out[!nas & display == "n"   ] <- set_n   (x[!nas & display == "n"   ], value[!nas & display == "n"   ])
  out[!nas & display == "wn"  ] <- set_wn  (x[!nas & display == "wn"  ], value[!nas & display == "wn"  ])
  out[!nas & display == "pct" ] <- set_pct (x[!nas & display == "pct" ], value[!nas & display == "pct" ])
  out[!nas & display == "diff"] <- set_diff(x[!nas & display == "diff"], value[!nas & display == "diff"])
  out[!nas & display == "coef"] <- set_diff(x[!nas & display == "coef"], value[!nas & display == "coef"])  # Phase 12c
  out[!nas & display == "gof" ] <- set_diff(x[!nas & display == "gof" ], value[!nas & display == "gof" ])  # Phase 12f
  out[!nas & display == "ctr" ] <- set_ctr (x[!nas & display == "ctr" ], value[!nas & display == "ctr" ])
  out[!nas & display == "mean"] <- set_mean(x[!nas & display == "mean"], value[!nas & display == "mean"])
  out[!nas & display == "var" ] <- set_var (x[!nas & display == "var" ], value[!nas & display == "var" ])
  out[!nas & display == "ci"  ] <- set_ci   (x[!nas & display == "ci"  ], value[!nas & display == "ci"  ])
  out[!nas & display == "rr"  ] <- set_ratio(x[!nas & display == "rr"  ], value[!nas & display == "rr"  ])
  out[!nas & display %in% c("or", "OR")] <- set_or(x[!nas & display %in% c("or", "OR")  ], value[!nas & display == "or"  ])
  # Phase 12h: est_ci writes back to its point-estimate field (OR or coefficient), like get_num reads it.
  est_ci_m <- !nas & display == "est_ci"
  if (any(est_ci_m)) {
    out[est_ci_m] <- if (identical(as.character(get_ci_type(x))[1], "or")) {
      set_or(x[est_ci_m], value[est_ci_m])
    } else {
      set_diff(x[est_ci_m], value[est_ci_m])
    }
  }
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
  # "coef" (Phase 12c): a regression-coefficient column (gaussian beta). It routes the effect-size
  # color (mean_diff Cohen scale, standardized by beta/SD(Y) via the `var` field) and the raw
  # `display="coef"` render, without abusing "mean"/"row" or fighting get_ref_var() (whose
  # refrow-at-END grouping is built for crosstab subtable totals). OR/IRR stay on "row".
  stopifnot(type %in% c("row", "col", "all", "all_tabs", "mean", "n", "coef"))
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

# Phase 9b-3: aggregate a per-cell fmt flag (in_totrow / in_tottab / in_refrow) across a data.frame's
# fmt columns to a per-ROW logical. Byte-identical to the former `select(where(is_fmt)) |> map_df |>
# if_all/if_any` but reads the field directly + reduces (28x faster on a grouped table; is_totrow /
# is_tottab / is_refrow are on many hot paths). DESIGN: the old `partial` warning branch was DEAD CODE
# (if_all(-"complete") & !complete is always FALSE), so it is not reproduced. partial=FALSE => a row
# where ALL fmt cells are flagged (if_all); partial=TRUE => ANY (if_any). No fmt cols => logical(0).
fmt_row_flag <- function(x, field, partial = FALSE) {
  cols     <- unclass(x)
  fmt_cols <- cols[vapply(cols, is_fmt, logical(1))]
  if (length(fmt_cols) == 0L) return(logical(0L))
  flags <- lapply(fmt_cols, function(col) vctrs::field(col, field))
  purrr::reduce(flags, if (partial) `|` else `&`)
}

#' Test function to detect cells in total rows
#' @inheritParams fmt
#' @param partial Should partial total rows be counted as total rows ? Default to FALSE.
#' @return A list of logical vectors, with the data.frame column's totrow fields.
#' @export
is_totrow.data.frame <- function(x, ..., partial = FALSE) {
  fmt_row_flag(x, "in_totrow", partial)
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
  fmt_row_flag(x, "in_tottab", partial)
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
  # Phase 9b-3: same fold as is_totrow/is_tottab (default partial = TRUE -> if_any). See fmt_row_flag.
  fmt_row_flag(x, "in_refrow", partial)
}

#' @describeIn fmt set the "in_refrow" field (belong to reference row)
#' @return A modified fmt vector with in_refrom field changed.
#' @export
as_refrow  <- function(x, in_refrow = TRUE) {
  vctrs::vec_assert(in_refrow, logical())
  vctrs::`field<-`(x, "in_refrow", vctrs::vec_recycle(in_refrow, length(x)))
}


#' @describeIn fmt get comparison level of fmt columns
# No @inheritParams fmt here: @describeIn merges this block into the `fmt` topic, where `x` is
# already documented -- roxygen2 then errors that nothing remains to inherit.
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
  # "or" (Phase 12a): the cell carries a log-OR Wald exp() interval in ci_inf/ci_sup, centred on
  # the odds ratio (neutral value 1) -- read by ci_center() and the color significance gate.
  stopifnot(ci_type %in% c("cell", "diff", "diff_row", "diff_col", "or",
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

#' @describeIn fmt get the significance policy (\code{"ignore"} / \code{"grey_non_signif"} / \code{"guaranteed_effect"})
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
  # COMPAT (Phase 13a): the renamed policy value, accepted silently here (the user-facing
  # deprecation message fires once in normalize_color_spec()).
  if (identical(color_signif, "color_all_signif")) color_signif <- "guaranteed_effect"
  ok <- c("ignore", "grey_non_signif", "guaranteed_effect")
  if (!color_signif %in% ok) {
    cli::cli_abort(c("Unknown {.arg color_signif} value {.val {color_signif}}.",
                     "i" = "Valid: {.val {ok}}."))
  }
  `attr<-`(x, "color_signif", color_signif)
}

# === SECTION: display {} grammar (Phase 10i-A) ======================================
# The per-cell `display` field is EITHER a simple token ("pct"/"diff"/"n"/...) OR a glue-style
# COMPOSITE template ("{pct} (n={n})") that renders several fields in ONE value cell (text backends
# only -- get_num()/Excel fall back to the PRIMARY = the first {token}). These three shared helpers
# are the single source of truth for the grammar: one gated resolver, one parser, one write-time
# validator; every consumer that dispatches on the display token routes through display_primary().
# WARNING: display_primary() is on the O(cells) hot path (get_num/set_num/format) -- keep the
# no-composite fast path a single fixed grepl (Phase 10i-A benchmark). Replaced the Phase-10c
# `display_spec` per-column attribute (§34: add_n/add_pct are ROWS under pct="col", so the composite
# must be per-cell, not a column attribute).

# Field names accepted inside {}; mapped to the internal get_num() display token by the alias table.
tabxplor_display_fields  <- c("pct", "n", "wn", "mean", "diff", "ratio", "ci", "or", "ctr", "var")
# user-facing {name} -> internal display token (get_num()'s vocabulary). Only `ratio` differs (`rr`).
tabxplor_display_aliases <- c(ratio = "rr")

# Resolve a display-value vector to its PRIMARY simple token: a composite ("{field} ...") -> its
# first {field} (alias-applied); a simple token / NA -> unchanged. Gated so a column carrying no
# composite pays one fixed grepl and returns. A malformed token (no closing brace) is left as-is
# and falls through to get_num()'s default `n` -- never errors (robust to hand-injected templates).
display_primary <- function(display) {
  comp <- !is.na(display) & grepl("{", display, fixed = TRUE)
  if (!any(comp)) return(display)
  tok <- sub("^[^{]*\\{\\s*([^{}]+?)\\s*\\}.*$", "\\1", display[comp])
  hit <- tok %in% names(tabxplor_display_aliases)
  tok[hit] <- unname(tabxplor_display_aliases[tok[hit]])
  display[comp] <- tok
  display
}

# Split ONE template into ordered segments (called once per unique template in a column, which are
# ~uniform). Returns pieces (literals + {token}s in order), is_tok (which pieces are field tokens),
# and fields (the alias-resolved internal tokens, in order). A degenerate template with no {field}
# (e.g. malformed) yields is_tok all FALSE -> the format() branch leaves those cells plain.
parse_display_template <- function(tmpl) {
  pieces <- regmatches(tmpl, gregexpr("\\{[^{}]+\\}|[^{}]+", tmpl))[[1]]
  is_tok <- startsWith(pieces, "{")
  fields <- character(0)
  if (any(is_tok)) {
    raw <- trimws(gsub("[{}]", "", pieces[is_tok]))
    hit <- raw %in% names(tabxplor_display_aliases)
    raw[hit] <- unname(tabxplor_display_aliases[raw[hit]])
    fields <- raw
  }
  list(pieces = pieces, is_tok = is_tok, fields = fields)
}

# WRITE-time: VALIDATE a `display=` {} template and return it. Composites use the {} grammar ONLY
# (no curated recipes -- one consistent syntax; the internal pct_ci/mean_ci/or_pct tokens are pipeline-
# set rendering modes, never user-typed, so they are unaffected). Checks balanced non-empty braces and
# known field names. The ONLY place a bad `display=` value aborts.
#' @keywords internal
validate_display_template <- function(recipe) {
  recipe <- recipe[[1]]
  if (!grepl("[{}]", recipe)) {
    cli::cli_abort(c(
      "Invalid {.arg display} value {.val {recipe}}.",
      "i" = "Composite display uses a {{}} template listing the fields to combine,
             e.g. {.code {{pct}} (n={{n}})} or {.code {{diff}} [{{ci}}]}."
    ))
  }
  opens  <- stringr::str_count(recipe, "\\{")
  closes <- stringr::str_count(recipe, "\\}")
  toks   <- regmatches(recipe, gregexpr("\\{[^{}]+\\}", recipe))[[1]]
  fields_used <- trimws(gsub("[{}]", "", toks))
  if (opens != closes || length(toks) != opens || any(!nzchar(fields_used))) {
    cli::cli_abort(c("Malformed {.arg display} template {.val {recipe}}.",
                     "i" = "Use balanced, non-empty tokens, e.g. {.code {{pct}} (n={{n}})}."))
  }
  unknown <- setdiff(fields_used, c(tabxplor_display_fields, names(tabxplor_display_aliases)))
  if (length(unknown)) {
    cli::cli_abort(c("Unknown field{?s} {.val {unknown}} in {.arg display} template.",
                     "i" = "Valid fields: {.val {tabxplor_display_fields}}."))
  }
  recipe
}



# fmt_get_color_code() doen't work in mutate with groups.

#' Get HTML Color Code of a fmt vector
#' @param x The fmt vector to get the html color codes from.
#' @param ... Absorbs deprecated arguments (e.g. \code{html_24_bit}); ignored.
#'
#' @param type The style type, \code{"text"} to color the text, \code{"bg"} to color the background.
#' @param theme Is your console or html table background \code{"light"} or \code{"dark"} ? Default
#' to the current setting (RStudio theme when detectable, else \code{"light"}).
#' @return A character vector with html color codes, of the length of the initial vector.
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
#' dplyr::mutate(tabs, across(where(is_fmt), fmt_get_color_code))
#'}

fmt_get_color_code <- function(x, type = "text", theme = "light", ...) {  # ... absorbs deprecated html_24_bit
  color <- get_color(x)
  if (length(color) == 0L || is.na(color[1]) || color[1] %in% c("no", "")) {
    return(rep(NA_character_, length(x)))
  }

  # `type` selects the palette family (text vs bg); the slot integer (1:8) indexes it.
  channel <- if (type == "bg") "bg" else "text"
  slot    <- fmt_color_slots(x, fmt_color_plan(x, channel = channel))
  styles  <- get_color_style("color_code", type = type, theme = theme)

  out     <- rep(NA_character_, length(x))
  colored <- slot > 0L
  # historical output is upper-case hex; the 24-bit palettes carry lower-case codes.
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
    color = color, color_signif = color_signif[1],
    class = c(class, "tabxplor_fmt"))
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
  else if (get_ci_type(x) == "or")                          get_or(x)   # Phase 12a: OR CI centred on the odds ratio
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
# @describeIn fmt get the "or" field (odds ratio; per-level OR vs the reference for 3+ level vars)
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

# grand_totrow() -- the whole-table grand-total row mask under comp = "all": the total row of the
# total table (is_totrow & is_tottab). Degrades to the plain total row when there is no total-table
# axis (no tab_vars), so a single unsubtabled table is treated as its own total table -- keeping
# comp = "all" usable (and byte-identical to comp = "tab") there instead of crashing the colour
# engine / storing the mean-contribution seed on an empty selection. Shared by get_mean_contrib()
# (read) and chi2_write_contrib() (write) so the two never drift.
#' @keywords internal
grand_totrow <- function(x) {
  g <- is_totrow(x) & is_tottab(x)
  if (any(g)) g else is_totrow(x)
}

#' @keywords internal
get_mean_contrib <- function(x) {
  comp    <- get_comp_all(x)
  totrows <- is_totrow(x)
  ctr     <- get_ctr(x)

  if (!any(totrows)) return(rep(NA_real_, length(x)))

  if (comp) {
    grand <- grand_totrow(x)
    if (!any(grand)) return(rep(NA_real_, length(x)))
    rep(ctr[grand][sum(grand)], length(x))
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

# For each column, detect the REFERENCE column of its col_var group -- the one marked by the `refcol`
# attribute (is_refcol). Falls back to detect_firstcol()'s first-column-of-group when no reference is
# marked, so it is byte-identical to detect_firstcol() whenever the reference IS the first level (or is
# unmarked). Phase 7g-iii: tab_ci() uses it so the diff-CI reference column matches the diff/colour
# reference column, once a per-col_var reference can be neither the first level nor the total.
#' @keywords internal
detect_refcol <- function(tabs) {
  col_vars  <- get_col_var(tabs)
  refcol    <- is_refcol(tabs)
  nms       <- names(tabs)
  firstcols <- detect_firstcol(tabs)   # per-column sym of each group's first column (fallback + "" edges)
  res <- purrr::map(seq_len(ncol(tabs)), function(.i) {
    in_grp <- which(col_vars == col_vars[.i] & refcol)
    if (length(in_grp) >= 1L) rlang::sym(nms[in_grp[1]]) else firstcols[[.i]]
  }) %>%
    purrr::set_names(nms)
  # mirror detect_firstcol: no reference column for the all_col_vars total group
  if (any(col_vars == "all_col_vars")) res[col_vars == "all_col_vars"] <- rlang::syms("")
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
# @describeIn fmt set the "or" field (odds ratio; per-level OR vs the reference for 3+ level vars)
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

# Excel number-format code per cell -- the tab_xl() bypass, folded here so format() is the ONE
# display source of truth (Phase 10g). `format(x, syntax = "excel")` returns these codes instead
# of rendered strings; tab_xl() writes the raw get_num() value and hands display to Excel's engine.
# It is fed format()'s OWN masks (so it can never desync -- the whole point of the fold):
#   digits = format()'s ADJUSTED digits (n->0, or->>=2, mean-diff->>=1);
#   pct    = format()'s x100 mask `pct_or_ci` (a "%" code multiplies by 100 in Excel, matching the
#            x100 format() applies) -- this is what a hand-maintained numfmt() kept getting wrong;
#   ci     = a standalone "ci" display (prepend the plus-minus glyph, matching format());
#   text   = base_plus_ci (pct_ci/mean_ci) -> written as TEXT (the value+CI string is pre-formatted).
# WARNING: a negative digit count rounds to a power of ten (Excel thousands mask). A percentage
# rounded to a power of ten yields no code -> Excel "General".
excel_numfmt_code <- function(digits, pct, ci, text, signed = FALSE, ratio = FALSE) {
  out <- rep(NA_character_, length(digits))
  ok  <- !is.na(digits)
  if (!any(ok)) return(out)

  n    <- digits[ok]
  p    <- pct[ok]
  isci <- ci[ok]
  txt  <- text[ok]
  sgn  <- if (length(signed) == 1L) rep(signed, sum(ok)) else signed[ok]
  rat  <- if (length(ratio)  == 1L) rep(ratio,  sum(ok)) else ratio[ok]
  n_inf <- n < 0
  n_0   <- n == 0
  rep0_n <- vapply(abs(n), function(k) paste0(rep("0", k), collapse = ""), character(1))

  res <- dplyr::case_when(
    txt          ~ "TEXT",
    p & n_inf    ~ NA_character_,
    p & n_0      ~ "0%",
    p            ~ paste0("0.", rep0_n, "%"),
    n_0          ~ "#,##0",
    n_inf        ~ paste0(
      "#,",
      vapply(abs(n), function(k) paste0(rep("#", 2 - k %% 3), collapse = ""), character(1)),
      vapply(abs(n), function(k) paste0(rep("0", 1 + k %% 3), collapse = ""), character(1)),
      vapply(abs(n), function(k) paste0(rep(",",     k %/% 3), collapse = ""), character(1))),
    TRUE         ~ paste0("#,##0.", rep0_n)
  )
  res <- dplyr::if_else(isci, paste0(stringi::stri_unescape_unicode("\\u00b1"), res), res)
  # Phase 13c-v: an explicit +/- sign for diff/contrib cells (a signed difference reads clearer), and a
  # leading multiply sign for ratio cells (kept a real, editable number). Skip TEXT / power-of-ten (NA)
  # codes. `+0.0%;-0.0%` = positive shows "+", negative shows "-"; `"x"#,##0.0` = "x2.0".
  can <- !is.na(res) & res != "TEXT"
  s2  <- can & sgn
  res[s2] <- paste0("+", res[s2], ";-", res[s2])
  r2  <- can & rat
  res[r2] <- paste0('"', mult_sign, '"', res[r2])
  out[ok] <- res
  out
}


# DESIGN: Central display method. Handles 20+ display modes (n, wn, pct, diff, ctr, ci,
#   pct_ci, mean_ci, var, pvalue, or, OR, etc.). Key transformations:
#   - pct stored as 0-1 is multiplied by 100 here for display
#   - Two CI display modes controlled by option tabxplor.ci_print: "moe" (±margin) or "ci" ([lo;hi])
#   - diff for means shows with "*" symbol; diff for pct shows with +/- sign
#   - special_formatting=TRUE adds "ref:" prefix and "mean:" labels (used in pillar)
#   - syntax="excel" returns Excel number-format codes (via excel_numfmt_code) instead of strings
#' Print method for class tabxplor_fmt
#'
#' @param x A fmt object.
#' @param ... Other parameters.
#' @param html Should html tags be added (to print confidence intervals as subscripts) ?
#' @param na How `NA`s should be printed. Default to `NA`.
#' @param special_formatting Set to `TRUE` to print more verbose results,
#' like indicating which is the reference row or col for differences.
#' @param stars Append significance stars after the value (opt-in; default `FALSE`). Stars appear
#' only where a per-cell p-value was stored (diff-type CIs / regression coefficients) and are
#' right-padded so numbers stay aligned. The main display (console, [tab_kable()], [tab_md()]) sets
#' this `TRUE`; tooltip / secondary-field re-renders leave it `FALSE`, so stars never leak.
#' @param bold_split Internal (default `FALSE`): when `TRUE`, attach a per-cell `primary_nchar`
#' attribute giving the bold-prefix width of a composite `"{pct} (n={n})"` cell, so exporters can
#' bold only the primary field in a bold row. Off by default -> the output is attribute-free.
#' @param syntax `"text"` (default) returns the rendered display strings; `"excel"` returns the
#' per-cell Excel number-format codes used by [tab_xl()] (the raw value is written unchanged).
#' @param .ref Internal: precomputed reference masks `list(cells=, all_totals=)` (derive-once
#' speed-up passed by the exporter prep); computed internally when `NULL`.
#'
#' @return The fmt printed in a character vector.
#' @export
format.tabxplor_fmt <- function(x, ..., html = FALSE, na = NA,
                                special_formatting = FALSE, stars = FALSE,
                                bold_split = FALSE,
                                syntax = c("text", "excel"), .ref = NULL) {
  syntax <- match.arg(syntax)

  out    <- get_num(x)
  na_out <- is.na(out)

  # Phase 10i-A: keep the RAW display field for the composite-template expansion at the end; the
  # dispatch masks below run on the PRIMARY token (byte-identical, one fixed grepl, when no cell is
  # a composite). get_num() above already resolved composites to their primary value.
  raw_display <- get_display(x)
  display <- display_primary(raw_display)
  nas  <- is.na(display)
  digits <- get_digits(x)
  digits[!nas & display == "n"] <- 0
  digits[!nas & display %in% c("or", "or_pct", "OR", "OR_pct", "est_ci") & # no "var" (used in chi2_table)
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
                                        "OR", "OR_pct", "gof") |             # Phase 12f: gof -> big.mark
                           (display == "ci" & type == "mean") )
  type_ci       <- ok & display == "ci"
  pvalue        <- ok & display == "pvalue"

  out[pct_or_ci] <- out[pct_or_ci] * 100
  digits[diff_mean] <- ifelse(digits[diff_mean] == 0, 1, digits[diff_mean])

  # Phase 10g: Excel number-format codes reuse format()'s OWN finalized masks (the x100 mask
  # pct_or_ci, the standalone-ci marker, the base_plus_ci TEXT mask) + adjusted digits, so the
  # tab_xl bypass can never drift from the console display. Return here, before any string building.
  if (syntax == "excel") {
    # pvalue is shown x100 with "%" by its own rendering path (not pct_or_ci), so add it to the
    # Excel "%" mask; a p-value shown as a "<0.01%" threshold still stores its raw value.
    excel_pct <- pct_or_ci | (!nas & display == "pvalue")
    # Phase 13c-v: pct diff + contrib get an explicit +/- sign; ratio gets a leading x. Mean diffs are
    # left as-is (their text display is still the legacy ratio, deferred to Phase 5 -- don't desync).
    return(excel_numfmt_code(digits, pct = excel_pct,
                             ci = !nas & display == "ci", text = plus_ci,
                             signed = !nas & (display == "ctr" |
                                                (display == "diff" & type != "mean")),
                             ratio  = !nas & display == "rr"))
  }


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

  # Phase 13c-i: ratio (rr) display shows the multiplicative sign, so ratios read symmetrically (like
  # the legend and the OR display): a cell >= its reference prints "x2", a cell below prints "/2"
  # (the divide sign over 1/ratio). Default 1 digit (>= the column's digits), trailing zeros trimmed,
  # right-padded so the column aligns in a monospace font. Text syntax only (Excel returned early
  # above -> ratio stays a real number there, per the Phase 13c Excel decision).
  disp_rr <- ok & display == "rr"
  if (any(disp_rr)) {
    rv  <- get_ratio(x)[disp_rr]
    inv <- !is.na(rv) & rv > 0 & rv < 1
    mag <- ifelse(inv, 1 / rv, rv)
    dg  <- pmax(1L, digits[disp_rr])
    num <- sprintf(paste0("%.", dg, "f"), mag)      # dg >= 1 -> always a decimal point
    num <- sub("\\.?0+$", "", num)                  # trim trailing zeros + a bare trailing dot
    # a ratio rounding to 1 is "equal to the reference" -> always show "x1" (never the confusing "/1").
    sym <- ifelse(inv & num != "1", div_sign, mult_sign)
    val <- out[disp_rr]
    nn  <- !is.na(rv)
    val[nn] <- paste0(sym[nn], num[nn])
    if (any(nn))
      val[nn] <- stringr::str_pad(val[nn], max(stringr::str_length(val[nn])), side = "left")
    out[disp_rr] <- val
  }

  if (any(pvalue)) {
    p    <- get_pct(x[pvalue])

    out[pvalue]    <- paste0(
      ifelse(
        p < 0.0001,
        "<0.01",
        print_num(p * 100, digits = 2L)
      ),
      "%"
    )
  }

  out[diff_pct] <- ifelse(                            # "+" sign on positive pct diffs
    !startsWith(out[diff_pct], "-"),            # !out[diff_pct] %in% c("0%", ) &
    paste0("+", out[diff_pct]),
    out[diff_pct]
  )
  out[diff_mean] <- paste0(mult_sign, out[diff_mean]) # multiply sign on mean diffs


 if (ci_print_moe) {
   out[type_ci] <- switch(
     type,
     "n"       = ,
     "coef"    = ,                                     # Phase 12c: coef CI moe is unit-scale (no %)
     "mean"    = paste0(pm, out[type_ci]),
     "row"     = ,
     "col"     = ,
     "all"     = ,
     "all_tabs"= paste0(pm, out[type_ci], "%") |> stringr::str_replace_all("%%", "%")
   )
 }



  if (special_formatting) {
    # Phase 10c: compute each reference mask ONCE for this column (was up to 3 get_reference()
    # calls here + 1 in pillar_shaft). The exporter prep (10d) passes precomputed masks via `.ref`;
    # otherwise they are memoized lazily below. Uses get_reference(x[mask], m) == get_reference(x,
    # m)[mask] (subset-equivalence, byte-verified). NB: keep `.ref = NULL` on the nested reffmt
    # format() calls (their columns are "pct"/"mean", so they take no reference branch anyway).
    ref_cells  <- if (!is.null(.ref)) .ref$cells      else NULL
    ref_alltot <- if (!is.null(.ref)) .ref$all_totals else NULL

    disp_diff   <- display == "diff" & !nas
    disp_moe    <- disp_ci & ci_print_moe # no if `ci_print = "ci"`
    disp_ctr    <- display == "ctr" & !nas
    disp_coef   <- display == "coef" & !nas             # Phase 12c: raw regression coefficient
    disp_or     <- display %in% c("or", "OR") & !nas
    disp_or_pct <- display %in% c("or_pct", "OR_pct") & !nas
    disp_est_ci <- display == "est_ci" & !nas           # Phase 12h: estimate + visible CI bracket
    # get_var() (the vctrs::field accessor) not x$var (the dplyr::pull `$` method): x$var here ran
    # unconditionally for EVERY column and was ~28% of format() self-time (Phase 10c profile).
    disp_mean_sd <- display == "mean" & type == "mean" & !nas & !is.na(get_var(x))


    if (any (disp_mean_sd)) {
      sd <-
        print_num(get_num(set_display(set_var(x[disp_mean_sd],
                                              suppressWarnings(sqrt(get_var(x[disp_mean_sd]))) ), "var")),
                  digits = get_digits(x[disp_mean_sd])) # + 1L
      sd <- sd |>
        stringr::str_pad(width = max(stringr::str_length(sd)), side = "right")

      out[disp_mean_sd] <- paste0(out[disp_mean_sd], unbrk, "(", sigma_sign, sd, ")")
    }


    if (any(disp_diff)) {
      if (is.null(ref_cells)) ref_cells <- get_reference(x, "cells")
      ref     <- ref_cells[disp_diff]
      reffmt  <- set_display(x[disp_diff],
                             ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
        format() #%>% stringr::str_trim()
      out[disp_diff] <- ifelse(ref,
                               paste0("ref:", reffmt),
                               out[disp_diff])
    }

    if (any(disp_moe)) {
      if (is.null(ref_cells)) ref_cells <- get_reference(x, "cells")
      ref     <- ref_cells[disp_moe]
      reffmt  <- set_display(x[disp_moe],
                             ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
        format()
      out[disp_moe] <- ifelse(ref,
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
      # Phase 12a: OR display. (1) an OR < 1 prints as "1/x" so it compares symmetrically to an
      # OR > 1 (odds ratios are multiplicatively symmetric) -- everywhere, incl. empirical OR.
      # (2) reference rows (OR == 1) print a bare "1", annotated with the empirical reference %
      # when one is present (an empirical-OR crosstab); a pure model-OR table (tab_logit) has no
      # pct, so the "( )" annotation drops. The empirical-OR path is byte-identical to before
      # except for the intended 1/x rendering of OR < 1 cells.
      if (is.null(ref_alltot)) ref_alltot <- get_reference(x, "all_totals")
      refer  <- ref_alltot[disp_or]
      or_val <- get_or(x)[disp_or]
      or_dig <- digits[disp_or]
      vals   <- out[disp_or]

      recip       <- !is.na(or_val) & or_val > 0 & or_val < 1
      vals[recip] <- paste0("1/", print_num(1 / or_val[recip], or_dig[recip]))
      one         <- stringr::str_replace(vals, "1\\.0+", "1")

      if (any(!is.na(get_pct(x)[disp_or]))) {                # empirical-OR crosstab: annotate ref %
        reffmt <- set_display(x[disp_or], "pct") |> set_digits(0L) |> format()
        reffmt <- suppressWarnings(
          stringr::str_pad(reffmt, suppressWarnings(max(stringr::str_length(reffmt), na.rm = TRUE)))
        )
        out[disp_or] <- ifelse(refer & !is.na(reffmt), paste0(one, " (", reffmt, ")"), vals)
      } else {                                               # pure model-OR: bare "1" on ref rows
        out[disp_or] <- ifelse(refer, one, vals)
      }
    }

    if (any(disp_est_ci)) {
      # Phase 12h: OR / beta cell with a VISIBLE confidence interval. `out` already holds the RAW point
      # estimate (get_num -> get_or / get_diff, no x100, NO 1/x reciprocal -- the standard forest-plot
      # convention when a bracket is shown), to which we append the real asymmetric bounds
      # [ci_inf; ci_sup] (exp() Wald for OR, additive Wald for beta). Reference rows have NA bounds ->
      # no bracket (bare "1" / coefficient baseline). Stars are appended by the shared block below.
      lo  <- get_ci_inf(x)[disp_est_ci]
      hi  <- get_ci_sup(x)[disp_est_ci]
      dg  <- digits[disp_est_ci]
      brk <- ifelse(
        is.na(lo) | is.na(hi), "",
        paste0(" [", sprintf(paste0("%-0.", dg, "f"), lo), ";",
               sprintf(paste0("%-0.", dg, "f"), hi), "]")
      )
      out[disp_est_ci] <- paste0(out[disp_est_ci], brk)
    }

    if (any(disp_or_pct)) {
      reffmt  <- set_display(x[disp_or_pct], "pct") |> set_digits(0L) |> format()
      out[disp_or_pct] <- paste0(out[disp_or_pct], " (", reffmt, ")")
    }

    if (any(disp_coef)) {
      # Phase 12c: a regression-coefficient cell renders as the plain signed value (no x100 / % / x
      # -- "coef" is absent from the pct_or_ci / diff_mean masks). Reference-level coefficients are
      # 0 (the additive neutral); show a bare "0" (the additive twin of the OR "1"). The intercept
      # ("Constant") carries the baseline value (diff != 0), so it keeps its number.
      ref0 <- disp_coef & !is.na(get_diff(x)) & get_diff(x) == 0
      out[ref0] <- "0"
    }
  }

  # Phase 3a / bug-fix: append significance stars (universal CI-inclusion) after the cell value,
  # wherever a per-cell pvalue was stored (diff-type CIs / regression coefficients). OPT-IN: the
  # `stars` argument (default FALSE) is TRUE only at the MAIN display sites (pillar_shaft, tab_kable,
  # tab_md) -- so tooltip / character-cast re-renders never leak stars onto secondary fields.
  # get_stars() is already "" for NA pvalue, so a table built with stars = FALSE shows none anyway.
  # PADDING: when any value cell is starred, right-pad every value cell's star field to the
  # column-max star width (stars left, spaces right) so the numbers stay aligned in a monospace font.
  if (isTRUE(stars)) {
    st  <- get_stars(x)
    val <- !is.na(out) & nzchar(out)
    if (any(val & nzchar(st))) {
      w  <- max(nchar(st[val]))
      st_pad <- formatC(st, width = -w)          # left-justify glyphs, pad right with spaces
      out[val] <- paste0(out[val], st_pad[val])
    }
  }

  # Phase 7g: a "blank" cell (n_min mask) renders as a true empty string in every consumer
  # (console/pillar, tab_kable, tab_md), distinct from a genuine NA cell (which keeps `na`).
  out[!nas & display == "blank"] <- ""

  # Phase 10i-A: opt-in COMPOSITE display -- a per-cell `display` template like "{pct} (n={n})"
  # renders several fields in one value cell. Parsed ONLY here, gated by one fixed grepl so the whole
  # function is byte-identical when no cell is a composite (get_num/Excel already fell back to the
  # PRIMARY = first token). Each {field} is rendered by RE-USING format() with a simple token (inner
  # fast path, no recursion); STARS ride the primary (first token keeps its pvalue, the others get
  # set_pvalue(NA)); the template is applied only where every token rendered (else the plain primary
  # is kept). tab(display=) writes the template only onto value cells, so p-value/blank/total cells
  # keep their own token and are never composited.
  composite <- !nas & grepl("{", raw_display, fixed = TRUE)
  prim_nchar <- NULL                                          # Phase 13c-ii bold-prefix widths
  if (any(composite)) {
    if (bold_split) prim_nchar <- rep(NA_integer_, length(out))
    for (tmpl in unique(raw_display[composite])) {
      seg   <- parse_display_template(tmpl)
      if (!any(seg$is_tok)) next
      cells <- which(composite & raw_display == tmpl)
      xc    <- x[cells]
      toks  <- lapply(seq_along(seg$fields), function(i) {
        xi <- if (i == 1L) xc else set_pvalue(xc, NA_real_)   # stars ride the primary token
        format(set_display(xi, seg$fields[i]), na = na, special_formatting = FALSE,
               stars = isTRUE(stars) && i == 1L)
      })
      # Phase 13c-i: align each {field} to a uniform width within the column so numbers line up in a
      # monospace font (e.g. "100% (n=  849)" / "100% (n=3 648)"). Right-aligned (left-pad) over the
      # non-NA cells; the literal pieces are constant, so only the {tokens} are padded.
      toks <- lapply(toks, function(s) {
        keep <- !is.na(s)
        if (any(keep))
          s[keep] <- stringr::str_pad(s[keep], max(stringr::str_length(s[keep])), side = "left")
        s
      })
      strs <- vector("list", length(seg$pieces)); ti <- 0L
      for (j in seq_along(seg$pieces)) {
        if (seg$is_tok[j]) { ti <- ti + 1L; strs[[j]] <- toks[[ti]] }
        else               { strs[[j]] <- rep(seg$pieces[j], length(cells)) }
      }
      ok_c <- Reduce(`&`, lapply(toks, function(s) !is.na(s)))
      asm  <- do.call(paste0, strs)
      out[cells[ok_c]] <- asm[ok_c]
      # Phase 13c-ii: OPT-IN (bold_split) record of the bold-prefix width (through the FIRST {token})
      # so exporters bold only the primary field of a composite cell in a bold row; the remaining
      # literals/tokens stay plain. Off by default -> format() output is attribute-free / byte-identical.
      if (bold_split) {
        first_tok <- which(seg$is_tok)[1]
        prefix    <- do.call(paste0, strs[seq_len(first_tok)])
        prim_nchar[cells[ok_c]] <- nchar(prefix)[ok_c]
      }
    }
  }

  # Phase 10e: honour the `na` argument on the main path. Historically NA cells were hard-coded to NA
  # (`out[na_out] <- NA` above) and `na` was consumed only by the composite branch. Applied LAST so
  # it dominates every intermediate append. Default na=NA -> no-op (byte-identical for the console and
  # any caller not passing `na`); tab_kable()/tab_md() pass na="" -> NA cells render "" at source,
  # which retires tab_kable()'s post-hoc `>NA</span>` string surgery.
  if (!is.na(na)) out[na_out] <- na

  # Phase 13c-ii: expose the per-cell bold-prefix width of composite cells (NA elsewhere) so exporters
  # can bold only the primary field in bold rows. Dropped silently by any downstream string op, so
  # consumers must read it right after format() (see md_render_one / render_*_engine / tab_xl).
  if (!is.null(prim_nchar)) attr(out, "primary_nchar") <- prim_nchar

  #out <- stringr::str_pad(out, max(stringr::str_length(out), na.rm = TRUE))
  out
}






#' Pillar_shaft method to print class fmt in a \code{\link[tibble:tibble]{tibble}} column
#'
#' @param x A fmt object.
#' @param ... Other parameter.
#' @param .ref Internal: precomputed reference masks, as
#' \code{list(cells =, all_totals =)}, threaded to \code{format()} to avoid deriving them again
#' (exporters compute them once for the whole table). \code{NULL} (the default, and the console
#' path) recomputes them. Not for direct use.
#'
#' @return A fmt printed in a pillar.
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.tabxplor_fmt <- function(x, ..., .ref = NULL) {
  # print color type somewhere (and brk legend beneath ?) ----

  # Phase 10c: `.ref` (precomputed reference masks) threads through to the internal format() and
  # the greying `totals` mask below. NULL on the console path (each computed as before).
  out     <- format(x, special_formatting = TRUE, stars = TRUE, .ref = .ref)
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
    totals <- if (!is.null(.ref)) .ref$all_totals else get_reference(x, "all_totals") #c("cells","lines")

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
  # Phase 12f: a p-value cell may carry an in-cell test label ("{pvalue} (Chi2)"); resolve the
  # composite to its PRIMARY token so the red/green colouring still fires on the labelled cell.
  display <- display_primary(get_display(x))
  nas     <- is.na(display)

  color_style <- get_color_style()

  is_p     <- !nas & display == "pvalue"
  pvalues  <- out[is_p]
  p_values <- get_num(x)[is_p]

  # Non-significant p-values (>= 5%) print in the strongest under-represented colour (slot 8),
  # significant ones in the strongest over-represented colour (slot 4): a warning colour flags a
  # (sub)table that may not differ from the independence hypothesis.
  out[is_p] <-
    dplyr::if_else(condition = p_values >= 0.05,
                   true      = color_style[[8]](pvalues),
                   false     = color_style[[4]](pvalues) )

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
    color %in% c("after_ci", "ci") ~ "guaranteed_effect",
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
  # Phase 12c: a "coef" column (gaussian regression beta) colours the STANDARDIZED effect beta/SD(Y)
  # against the mean_diff (Cohen 0.2/0.5/0.8/1.2) breaks -- the additive twin of OR-by-ratio. It uses
  # the mean_diff scale like a mean-diff, but standardizes by its OWN `var` field (= var(Y), constant),
  # NOT by get_ref_var() (whose refrow-at-END grouping is meaningless for a regression skeleton).
  is_std_diff <- is_mean || type == "coef"
  sc      <- color_scales(x)
  scale   <- switch(measure,
                    "diff"    = if (is_std_diff) sc$mean_diff  else sc$pct_diff,
                    "ratio"   = if (is_mean)     sc$mean_ratio else sc$pct_ratio,
                    "or"      = sc$mean_ratio,
                    "contrib" = sc$contrib)
  center <- if (is.null(scale)) 0 else scale$center
  strict <- if (is.null(scale)) TRUE else scale$strict

  # observed per-cell quantity
  raw <- switch(measure,
                "diff"    = get_diff(x),
                "ratio"   = get_ratio(x),
                "or"      = get_or(x),
                "contrib" = dplyr::if_else(is_totrow(x), NA_real_,
                                           get_ctr(x) / get_mean_contrib(x)))

  # numeric diff is standardized (Glass's delta) unless absolute unit breaks were supplied
  sd_ref <- NULL
  if (measure == "diff" && is_std_diff && isTRUE(scale$std)) {
    sd_ref      <- if (type == "coef") sqrt(get_var(x)) else sqrt(get_ref_var(x))
    raw         <- raw / sd_ref
    raw[!is.finite(raw)] <- NA_real_        # sd_ref 0/NA -> undefined -> uncolored
  }

  # significance from the stored bounds. A diff-type CI tests exclusion of 0; an OR CI (ci_type
  # "or", log-OR Wald exp() bounds) tests exclusion of 1, the multiplicative neutral (Phase 12a).
  has_diff_ci <- get_ci_type(x) %in% c("diff", "diff_row", "diff_col")
  is_or_ci    <- measure == "or" & get_ci_type(x) == "or"
  neutral     <- if (measure == "or") 1 else 0
  sig_pos <- (has_diff_ci | is_or_ci) & get_ci_inf(x) > neutral
  sig_neg <- (has_diff_ci | is_or_ci) & get_ci_sup(x) < neutral
  sig_pos[is.na(sig_pos)] <- FALSE
  sig_neg[is.na(sig_neg)] <- FALSE

  if (policy == "guaranteed_effect") {
    # The GUARANTEED (CI-floor) magnitude, on the MEASURE'S OWN scale so fmt_color_slots() folds it
    # around the right centre. The stored bounds are the shared cell-vs-ref DIFFERENCE CI (centre 0)
    # for diff/ratio, or the native OR CI (centre 1) for `or`.
    floor_q <- dplyr::case_when(sig_pos ~ get_ci_inf(x),
                                sig_neg ~ get_ci_sup(x),
                                TRUE    ~ NA_real_)
    if (measure == "ratio") {
      # ratio has no native CI: convert the guaranteed DIFF to a guaranteed RATIO. Since
      # ratio - 1 = diff / p_ref, (ratio - 1) * (guar_diff / diff) = guar_diff / p_ref, so the
      # guaranteed ratio is 1 + guar_diff/p_ref = (p_ref + guar_diff)/p_ref (centre 1). Without this
      # the raw diff bound (~0.05) was folded around 1 -> 1/0.05 -> strongest UNDER colour on every
      # significant cell, regardless of direction.
      floor_q <- 1 + (get_ratio(x) - 1) * (floor_q / get_diff(x))
      floor_q[!is.finite(floor_q)] <- NA_real_
    } else if (measure == "diff" && is_std_diff && isTRUE(scale$std)) {
      floor_q <- floor_q / sd_ref
    }
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

  # Phase 12c: a reference row is a baseline, not an effect -> never coloured. Redundant for
  # crosstabs (a reference cell's diff is 0 / OR is 1, already slot 0), it uncolours a regression
  # INTERCEPT (in_refrow but a non-neutral baseline value) under every policy.
  if (measure %in% c("diff", "ratio", "or")) gate <- gate & !is_refrow(x)

  # Per-direction breaks + palette slots (Phase 13a). Each side of the scale carries its own
  # magnitudes (over$breaks / under$breaks) and intensities 1:4 (over$slots / under$slots). The
  # engine folds every cell to a magnitude >= the neutral, findInterval() against the side's breaks,
  # and reads the intensity: over -> slots 1:4, under -> slots 5:8 (the two halves of the 8-colour
  # palette). The former in-text "x2 rule" is gone -- it is now just a 1-break ratio scale carried on
  # the dedicated background channel (color = c("diff", "ratio")).
  if (isTRUE(mp$single0)) {                  # legacy color = "ci": one shade per direction, break at 0
    over_breaks <- 0; over_slots <- c(0L, 3L); under_breaks <- 0; under_slots <- c(0L, 7L)
  } else {
    over_breaks  <- scale$over$breaks
    under_breaks <- scale$under$breaks
    over_slots   <- c(0L, scale$over$slots)         # 0 = neutral level, then intensities 1:4
    under_slots  <- c(0L, scale$under$slots + 4L)   # 0 = neutral, then 5:8 (under half of the palette)
  }

  list(measure = measure, policy = policy, score = score, center = center, strict = strict,
       over_breaks = over_breaks, over_slots = over_slots,
       under_breaks = under_breaks, under_slots = under_slots, gate = gate)
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

  slot <- integer(n)
  posi <- dir > 0L
  negi <- dir < 0L
  if (any(posi)) {
    lp <- findInterval(mag[posi], plan$over_breaks, left.open = plan$strict)
    lp[is.na(lp)] <- 0L
    slot[posi] <- plan$over_slots[lp + 1L]
  }
  if (any(negi)) {
    ln <- findInterval(mag[negi], plan$under_breaks, left.open = plan$strict)
    ln[is.na(ln)] <- 0L
    slot[negi] <- plan$under_slots[ln + 1L]
  }

  slot[!plan$gate] <- 0L
  # Phase 7g: a "blank" cell (n_min mask) shows no value, so it must show no colour either --
  # in both channels and in fmt_get_color_code (which all route through here).
  # Phase 12f: a "gof" cell (a model-fit stat: N/R2/AIC/BIC/dispersion) is never effect-coloured --
  # a large AIC in the `diff` field would otherwise score to the strongest colour slot.
  slot[get_display(x) %in% c("blank", "gof")] <- 0L
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
fmt_channel_codes <- function(x, color_type = "text", theme = "light") {
  n  <- length(x)
  ch <- fmt_color_channels(x)

  text_styles <- get_color_style("color_code", type = color_type, theme = theme)
  bg_styles   <- get_color_style("color_code", type = "bg", theme = theme)

  text <- rep(NA_character_, n)
  bg   <- rep(NA_character_, n)
  tsel <- ch$text_slot > 0L
  bsel <- ch$bg_slot   > 0L
  # historical output is upper-case hex (cf. fmt_get_color_code).
  text[tsel] <- toupper(unname(text_styles[ch$text_slot[tsel]]))
  bg[bsel]   <- toupper(unname(bg_styles[ch$bg_slot[bsel]]))

  list(text = text, bg = bg, text_slot = ch$text_slot, bg_slot = ch$bg_slot)
}




# === SECTION: Phase 13b colour legend ==============================================================
# tab_color_legend() builds the human-readable colour legend, driven by the SAME per-channel plan
# (fmt_color_plan) + slot->palette path the CELLS use, so legend and cells can never disagree.
# Pipeline (one spec -> two assemblers -> per-medium renderer):
#   legend_specs(x)                         per col_var group: measures / breaks / ref / method /
#                                           policy / shade names / reg effect word.
#   legend_tokens_terse / _prose            a TOKEN stream (plain-text | coloured-break tokens);
#                                           `terse` = compact (console), `prose` = full sentences
#                                           (exports), translated via gettext (domain "R-tabxplor").
#   legend_render_line(tokens, medium)      console crayon / html text_spec / md pandoc span /
#                                           excel fmt_txt runs / plain.
# The break-word colours come from the engine's per-side slots (over 1:4, under 5:8) indexed into the
# 8-hex palette -- the exact path fmt_channel_codes() / tx_slot_class() use for the cells.

# fixed (non-translated) symbols, kept as \uXXXX so R source stays ASCII.
.lg_ge    <- "\u2265"   # >=
.lg_le    <- "\u2264"   # <=
.lg_times <- "\u00d7"   # x  (times)
.lg_div   <- "\u00f7"   # /  (division)
.lg_beta  <- "\u03b2"   # beta

# a legend token: plain text (c = NA) or a coloured break-word (c = palette slot 1:8).
# Phase 13d: the CSS class is not stored -- it is tx_slot_class(ch, c), derived at render time, so a
# legend break-word and the cells it describes cannot name different classes.
.lg_tok  <- function(t) list(t = t, c = NA_integer_, ch = NA_character_)
.lg_ctok <- function(t, slot, ch) list(t = t, c = as.integer(slot), ch = ch)

# resolve the display language: explicit `lang` > options(tabxplor.lang) > R/OS locale; english default.
#' @keywords internal
legend_resolve_lang <- function(lang = NULL) {
  if (is.null(lang) || identical(lang, "")) lang <- getOption("tabxplor.lang", "auto")
  lang <- tolower(as.character(lang)[1])
  if (lang %in% c("fr", "french", "francais", "fran\u00e7ais")) return("fr")
  if (lang %in% c("en", "english"))                             return("en")
  # auto: prioritise the MESSAGE-language signals (a user running English R on a French Windows must
  # get English), falling back to the character locale only when none is set.
  sources <- c(Sys.getenv("LANGUAGE"), Sys.getlocale("LC_MESSAGES"),
               Sys.getenv("LC_MESSAGES"), Sys.getenv("LANG"), Sys.getenv("LC_ALL"))
  sources <- sources[nzchar(sources)]
  probe   <- if (length(sources)) sources[1] else Sys.getlocale("LC_CTYPE")
  if (grepl("(^|[^a-z])fr|franc", probe, ignore.case = TRUE)) "fr" else "en"
}

# Flush gettext's cache of already-translated strings, so a mid-session LANGUAGE change is honoured.
# glibc caches per (domain, msgid) and only invalidates on setlocale()/bindtextdomain()/textdomain();
# without this, LANGUAGE changes silently no-op on Linux (they happen to work on Windows/macOS).
# Binding a throwaway domain is the portable flush (what withr::local_language() does since 3.0.0);
# the older Sys.setlocale(LC_MESSAGES) trick fails on musl/Alpine (withr#213).
#' @keywords internal
flush_gettext_cache <- function() {
  try(invisible(bindtextdomain("tabxplor_reset", tempdir())), silent = TRUE)
  invisible(NULL)
}

# number -> string (trimmed, no padding), FR decimal comma.
legend_num <- function(v, lang) {
  s <- trimws(formatC(v, format = "fg", digits = 4, drop0trailing = TRUE))
  if (identical(lang, "fr")) s <- gsub("[.]", ",", s)
  s
}

# a compact reference word for the terse (console) form.
legend_ref_short <- function(spec, lang) {
  ref <- spec$ref
  switch(ref$kind,
         "tot"      = if (!is.na(ref$label) && nzchar(ref$label)) ref$label else gettext("Total"),
         "level"    = if (!is.na(ref$label) && nzchar(ref$label)) ref$label else gettext("ref."),
         "category" = gettext("ref."),
         "indep"    = gettext("indep."),
         "")
}

# one break threshold -> its bare label (no colour), per measure. `is_std` = the diff is sd-standardized
# (numeric mean / regression coef): show SD units, not pct-points.
legend_break_label <- function(measure, brk, dir, is_std, lang) {
  neg <- dir < 0L
  if (identical(measure, "diff") && !is_std) {                 # pct-points
    paste0(if (neg) "-" else "+", legend_num(abs(brk) * 100, lang))
  } else if (identical(measure, "diff")) {                     # sd-standardized (Glass's delta / coef)
    paste0(if (neg) "-" else "+", legend_num(abs(brk), lang))
  } else if (measure %in% c("ratio", "contrib")) {
    paste0(if (neg) .lg_div else .lg_times, legend_num(abs(brk), lang))
  } else if (identical(measure, "or")) {
    if (neg) paste0("1/", legend_num(abs(brk), lang)) else legend_num(abs(brk), lang)
  } else as.character(brk)
}

# the coloured break tokens of one channel, split over / under (each a list of tokens). Slot 0 (a
# scale that skips an intensity via NA) -> a plain, uncoloured token. The token carries the palette
# slot; its colour (hex) or class is resolved per medium at render time.
legend_break_tokens <- function(plan, is_std, is_mean, channel, lang) {
  if (is.null(plan)) return(list(over = list(), under = list()))
  measure <- plan$measure
  mk_side <- function(breaks, slots, dir) {
    lapply(seq_along(breaks), function(l) {
      slot <- slots[l + 1L]
      lab  <- legend_break_label(measure, breaks[l], dir, is_std, lang)
      if (is.na(slot) || slot == 0L) return(.lg_tok(lab))
      .lg_ctok(lab, slot, channel)
    })
  }
  list(over  = mk_side(plan$over_breaks,  plan$over_slots,  +1L),
       under = mk_side(plan$under_breaks, plan$under_slots, -1L))
}

# join tokens with a plain-text separator.
legend_join <- function(toks, sep) {
  if (length(toks) == 0) return(list())
  out <- list(toks[[1]])
  for (i in seq_along(toks)[-1]) out <- c(out, list(.lg_tok(sep)), list(toks[[i]]))
  out
}

# default palette -> baked colour-shade names; a custom palette (set_color_palette) -> NA (generic,
# the coloured break-words carry the meaning). The over side of the default palette is teal->blue,
# the under side gold->red, in both light and dark, so the names are hue-descriptive and theme-free.
legend_shade_names <- function() {
  is_default <- tryCatch({
    b <- get0("base", envir = tabxplor_palette_env)
    is.null(b) || (identical(b$text_colors,     default_text_colors) &&
                   identical(b$text_colors_neg, default_text_colors_neg))
  }, error = function(e) FALSE)
  if (isTRUE(is_default))
    c(over = gettext("Shades of blue"), under = gettext("Shades of yellow to red"))
  else
    c(over = NA_character_, under = NA_character_)
}

# a regression column's effect word (OR / IRR / beta / AME / MER), read from the column-name suffix the
# package itself writes ("<base>: <word>", reg_effect_word); NA when not a recognised reg label.
legend_reg_effect_word <- function(cn) {
  w <- sub(".*:[[:space:]]*", "", cn)
  if (identical(w, cn)) return(NA_character_)
  if (w %in% c("OR", "IRR", .lg_beta, "AME", "MER", paste0("exp(", .lg_beta, ")"))) w else NA_character_
}

# recover a NON-total reference's actual label (the marked reference row / column). Returns NA when
# there is no single unambiguous label -- e.g. a grouped table's per-subtable references, or a total
# reference (those use the generic localized "Total"). The prose falls back gracefully on NA.
legend_ref_label <- function(x, col, orientation) {
  tryCatch({
    if (identical(orientation, "col")) {
      idx <- which(purrr::map_lgl(x, ~ is_fmt(.) && isTRUE(is_refcol(.))))
      if (length(idx) == 0) return(NA_character_)
      nm <- names(x)[idx[[1]]]
      if (startsWith(nm, "Total")) NA_character_ else nm     # a total column -> the generic "Total"
    } else {
      rv <- tab_get_vars(x)$row_var
      if (is.null(rv) || length(rv) == 0 || is.na(rv)) return(NA_character_)
      idx <- which(is_refrow(col))                           # the marked reference row(s) only
      if (length(idx) == 0) return(NA_character_)
      labs <- unique(as.character(x[[rv]][idx]))
      if (length(labs) == 1) labs else NA_character_          # ambiguous across subtables -> generic
    }
  }, error = function(e) NA_character_)
}

# per col_var reference descriptor (kind + recovered label + orientation). A "tot" reference always
# uses the generic localized "Total" (label = NA); only a non-total reference (ref = "first" / a level /
# an index) recovers its actual label.
legend_ref_info <- function(x, col, measure, orientation) {
  if (identical(measure, "contrib"))
    return(list(kind = "indep", label = NA_character_, orientation = orientation))
  if (identical(measure, "or"))
    return(list(kind = "category", label = legend_ref_label(x, col, "row"), orientation = "row"))
  ref <- get_ref_type(col); ref <- if (length(ref)) as.character(ref)[1] else "tot"
  if (identical(ref, "tot"))
    list(kind = "tot", label = NA_character_, orientation = orientation)
  else
    list(kind = "level", label = legend_ref_label(x, col, orientation), orientation = orientation)
}

# the localized reference phrase used in the lead / grey note.
legend_ref_phrase <- function(spec, lang) {
  ref <- spec$ref
  lab <- ref$label
  if (identical(ref$kind, "indep")) return(gettext("independence"))
  if (identical(ref$kind, "category")) {
    if (!is.na(lab) && nzchar(lab)) return(gettextf("the reference category (%s)", lab))
    return(gettext("the reference category"))
  }
  base <- if (identical(ref$orientation, "col")) gettext("column") else gettext("row")
  if (is.na(lab) || !nzchar(lab)) lab <- gettext("Total")
  gettextf("the %s %s", lab, base)                 # EN "the Total row"; FR "la %2$s %1$s" -> "la ligne Total"
}

# the CI-method name (NA when there is none, e.g. contrib).
legend_method_name <- function(spec) {
  cis <- spec$ci_settings
  if (isTRUE(spec$is_reg)) {
    if (identical(cis$method_diff, "profile")) return(gettext("profile-likelihood interval"))
    if (identical(spec$ci_type, "or"))         return(gettext("Wald interval on the log odds-ratio"))
    return(gettext("Wald interval"))
  }
  if (identical(spec$measure_text, "or")) return(gettext("Wald interval on the log odds-ratio"))
  if (identical(spec$measure_text, "contrib")) return(NA_character_)
  if (isTRUE(spec$is_mean)) return(gettext("Welch t interval"))
  md <- cis$method_diff; if (is.null(md)) md <- "newcombe"
  switch(md,
         "newcombe" = gettext("Newcombe score interval"),
         "ac"       = gettext("Wald interval with Agresti-Caffo adjustment"),
         "wald"     = gettext("Wald interval"),
         gettext("confidence interval"))
}

# "<method>, 95% confidence" (or just the confidence text when there is no method name).
legend_method_phrase <- function(spec, lang) {
  conf <- gettextf("%s%% confidence", legend_num(spec$ci_settings$conf_level * 100, lang))
  m    <- legend_method_name(spec)
  if (is.na(m)) conf else gettextf("%s, %s", m, conf)
}

# the measure / effect word (reg effect word takes precedence).
legend_measure_word <- function(measure, is_std, eff_word, lang) {
  if (!is.na(eff_word)) return(eff_word)
  switch(measure,
         "diff"    = if (isTRUE(is_std)) gettext("standardized difference") else gettext("difference"),
         "ratio"   = gettext("ratio"),
         "or"      = "OR",
         "contrib" = gettext("contribution to Chi2"),
         measure)
}

legend_ucfirst <- function(s) {
  if (!nzchar(s)) return(s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# ---- assemblers: spec -> token stream --------------------------------------------------------------

# TERSE (console): compact, one line per group -- names? + measure (ref): <breaks>  [; bg]  [policy].
legend_tokens_terse <- function(spec, lang, show_names) {
  colon <- if (identical(lang, "fr")) " : " else ": "
  toks <- list()
  if (show_names) toks <- c(toks, list(.lg_tok(paste0(paste(utils::head(spec$col_names, 3),
                                                            collapse = ", "), colon))))
  rs <- legend_ref_short(spec, lang)
  add_channel <- function(plan, prefix, is_bg) {
    mw <- legend_measure_word(plan$measure, spec$is_std, spec$eff_word, lang)
    bt <- legend_break_tokens(plan, spec$is_std, spec$is_mean, if (is_bg) "bg" else "text", lang)
    seq_toks <- c(rev(bt$under), bt$over)
    lbl <- paste0(prefix, mw, if (!is_bg && nzchar(rs)) paste0(" (", rs, ")") else "", colon)
    c(list(.lg_tok(lbl)), legend_join(seq_toks, " "))
  }
  if (!is.null(spec$plan_txt)) toks <- c(toks, add_channel(spec$plan_txt, "", FALSE))
  if (!is.null(spec$plan_bg))  toks <- c(toks, list(.lg_tok(if (identical(lang, "fr")) " ; " else "; ")),
                                         add_channel(spec$plan_bg, paste0(gettext("bg"), " "), TRUE))
  pn <- switch(spec$policy,
               "grey_non_signif"   = gettext("significant only"),
               "guaranteed_effect" = gettext("significant, error-adjusted"),
               "")
  if (nzchar(pn)) toks <- c(toks, list(.lg_tok(paste0(" [", pn, "]"))))
  toks
}

# PROSE (exports): full translatable sentences with coloured break-words. Everything measure-specific
# (subject / lead / unit / whether the reference is in the lead) is derived from the PLAN's own
# measure inside one_side(), so the text channel (e.g. diff) and the background channel (e.g. ratio)
# each describe themselves correctly.
legend_tokens_prose <- function(spec, lang, show_names) {
  ref_phrase  <- legend_ref_phrase(spec, lang)
  meth_phrase <- legend_method_phrase(spec, lang)
  # French typography: a (thin) space before the high punctuation ; : (matches the user's examples).
  semi  <- if (identical(lang, "fr")) " ; " else "; "
  colon <- if (identical(lang, "fr")) " : " else ": "

  one_side <- function(plan, dir, is_bg, no_shade = FALSE) {
    if (is.null(plan)) return(NULL)
    bt   <- legend_break_tokens(plan, spec$is_std, spec$is_mean, if (is_bg) "bg" else "text", lang)
    side <- if (dir > 0) bt$over else bt$under
    if (length(side) == 0) return(NULL)
    measure <- plan$measure
    subject <- if (!is.na(spec$eff_word)) spec$eff_word
               else if (identical(measure, "or")) "OR" else gettext("cells")
    has_ref_lead <- !identical(measure, "or") && !isTRUE(spec$is_coef)  # coef / OR carry the ref in the note only
    unit <- switch(measure,
                   "diff" = if (isTRUE(spec$is_std)) paste0(" ", gettext("SD")) else paste0(" ", gettext("points")),
                   "")
    cmp   <- if (dir > 0) .lg_ge else .lg_le
    shade <- if (no_shade) NA_character_ else if (dir > 0) spec$shades[["over"]] else spec$shades[["under"]]
    lead  <- if (has_ref_lead) gettextf("%s %s %s", subject, cmp, ref_phrase) else gettextf("%s %s", subject, cmp)
    head_toks <- if (!is.na(shade)) list(.lg_tok(paste0(shade, colon, lead, " ")))
                 else               list(.lg_tok(paste0(legend_ucfirst(lead), " ")))
    # guaranteed_effect: the coloured thresholds are the CI floor -> annotate the OVER sentence
    # ("..., after subtracting the margin of error (<method>).") instead of a bare ".".
    tail <- if (dir > 0 && identical(spec$policy, "guaranteed_effect"))
              paste0(unit, ", ", gettextf("after subtracting the margin of error (%s)", meth_phrase), ".")
            else paste0(unit, ".")
    c(head_toks, legend_join(side, semi), list(.lg_tok(tail)))
  }

  toks <- list()
  if (show_names)
    toks <- c(toks, list(.lg_tok(paste0(paste(spec$col_names, collapse = ", "), " \u2014 "))))

  is_bg_only <- is.null(spec$plan_txt)
  primary    <- if (is_bg_only) spec$plan_bg else spec$plan_txt
  ov <- one_side(primary, +1L, is_bg_only); un <- one_side(primary, -1L, is_bg_only)
  if (!is.null(ov)) toks <- c(toks, ov)
  if (!is.null(un)) toks <- c(toks, list(.lg_tok(" ")), un)

  # a second measure on the background channel (e.g. color = c("diff","ratio")).
  if (!is.null(spec$plan_txt) && !is.null(spec$plan_bg)) {
    bgw <- legend_measure_word(spec$measure_bg, spec$is_std, NA_character_, lang)
    toks <- c(toks, list(.lg_tok(paste0(" ", gettextf("Background colour (%s):", bgw)))))
    bov <- one_side(spec$plan_bg, +1L, TRUE, no_shade = TRUE)
    bun <- one_side(spec$plan_bg, -1L, TRUE, no_shade = TRUE)
    if (!is.null(bov)) toks <- c(toks, list(.lg_tok(" ")), bov)
    if (!is.null(bun)) toks <- c(toks, list(.lg_tok(" ")), bun)
  }

  # the grey-cells note (guaranteed_effect already annotated the over sentence).
  if (identical(spec$policy, "grey_non_signif"))
    toks <- c(toks, list(.lg_tok(paste0(" ", gettextf("Grey: not significantly different from %s (%s).",
                                                  ref_phrase, meth_phrase)))))
  else if (identical(spec$policy, "guaranteed_effect"))
    toks <- c(toks, list(.lg_tok(paste0(" ", gettextf(
      "Grey: not significantly different from %s after the margin of error.", ref_phrase)))))
  toks
}

# ---- render a token stream for one medium ----------------------------------------------------------
# excel -> a list of runs list(text=, color=, bold=); every other medium -> a single string.
legend_render_line <- function(tokens, medium, theme, color_type, colored, classes = FALSE) {
  # Phase 13d: `theme` may be the render intent "auto"; a palette is always light/dark. Without this,
  # get_color_style() builds the key "text_auto", finds no palette and errors on a length-0 vector.
  pal      <- tx_palette_theme(theme)
  slot_hex <- function(slot, ch)
    toupper(unname(get_color_style("color_code",
                                   type = if (identical(ch, "text")) color_type else "bg",
                                   theme = pal)[slot]))
  if (identical(medium, "excel")) {
    return(lapply(tokens, function(tk) {
      if (isTRUE(colored) && !is.na(tk$c) && tk$c > 0L)
        list(text = tk$t, color = slot_hex(tk$c, tk$ch), bold = TRUE)
      else list(text = tk$t, color = NA_character_, bold = FALSE)
    }))
  }
  parts <- vapply(tokens, function(tk) {
    if (!isTRUE(colored) || is.na(tk$c) || tk$c == 0L) return(tk$t)
    if (identical(medium, "console")) {
      get_color_style("crayon", type = if (identical(tk$ch, "text")) color_type else "bg")[[tk$c]](tk$t)
    } else if (identical(medium, "html")) {
      # DESIGN: the span is emitted inline rather than via kableExtra::text_spec(). text_spec()'s
      # byte output is version-unstable (1.4.0 -> 1.4.1 moved the rgba alpha 255 -> 1, dropped the
      # tile border-radius and leaked `class="TRUE"`), which made every legend-bearing snapshot
      # hostage to kableExtra's release schedule -- and it was the last kableExtra call on the
      # home-built "self-contained" html engine's path. Legend tokens are package-generated
      # ("+5", "x2", "1/1.5"), so they need no escaping (uncoloured tokens are emitted raw too).
      # Phase 13d: `classes` = "our stylesheet ships with this output", i.e. the html engine. There the
      # break-word must carry a CLASS, exactly like the cells it describes -- the legend sits in the
      # table's own <tfoot>, so inline hex would freeze it while the cells follow a theme toggle. The
      # discriminator is the ENGINE, not the theme: engine = "html" + theme = "light" + css = FALSE is
      # a real case (a document that emits tab_css("auto") itself), and there hex would be wrong too.
      # kableExtra carries no tabxplor stylesheet, so it keeps inline hex. (No `!important` on the
      # class path: it existed to beat kableExtra's lightable rules, which never match here.)
      if (isTRUE(classes)) {
        cls <- tx_slot_class(tk$ch, tk$c)
        if (identical(tk$ch, "text")) paste0("<span class=\"", cls, "\">", tk$t, "</span>")
        else paste0("<span class=\"", cls, "\" style=\"border-radius:4px;",
                    "padding-right:4px;padding-left:4px;\">", tk$t, "</span>")
      } else {
        hex <- slot_hex(tk$c, tk$ch)
        if (identical(tk$ch, "text"))
          paste0("<span style=\"color:", hex, " !important;\">", tk$t, "</span>")
        else
          paste0("<span style=\"background-color:", hex, " !important;border-radius:4px;",
                 "padding-right:4px;padding-left:4px;\">", tk$t, "</span>")
      }
    } else if (identical(medium, "md")) {
      cls <- tx_slot_class(tk$ch, tk$c)
      if (!nzchar(cls)) tk$t else paste0("[", tk$t, "]{.", cls, "}")
    } else tk$t
  }, character(1))
  paste0(parts, collapse = "")
}

# ---- build the per col_var specs -------------------------------------------------------------------
#' @keywords internal
legend_specs <- function(x) {
  is_f <- purrr::map_lgl(x, is_fmt)
  ct   <- get_color(x); cbg <- get_color_bg(x)
  keep <- is_f & ((!is.na(ct)  & !ct  %in% c("no", "")) |
                  (!is.na(cbg) & !cbg %in% c("no", "")))
  if (!any(keep)) return(list())

  col_vars_levels <- tab_get_vars(x)$col_vars_levels
  col_vars_levels <- col_vars_levels[names(col_vars_levels) != "all_col_vars"]
  kept_names <- names(x)[keep]
  reps <- purrr::map_chr(col_vars_levels, function(cols) {
    cc <- cols[cols %in% kept_names]; if (length(cc) == 0) NA_character_ else cc[[1]]
  })
  reps <- reps[!is.na(reps)]
  if (length(reps) == 0) return(list())

  is_reg <- tryCatch(is_reg_footer(get_test(x)), error = function(e) FALSE)
  cis    <- get_ci_settings(x); if (is.null(cis)) cis <- default_ci_settings()
  shades <- legend_shade_names()

  specs <- purrr::imap(reps, function(cn, cv) {
    col      <- x[[cn]]
    plan_txt <- fmt_color_plan(col, "text", color = get_color(col))
    plan_bg  <- fmt_color_plan(col, "bg",   color = get_color_bg(col))
    if (is.null(plan_txt) && is.null(plan_bg)) return(NULL)
    type     <- get_type(col)
    is_coef  <- identical(type, "coef")
    is_mean  <- type %in% c("mean", "n")
    is_std   <- is_mean || is_coef                  # matches fmt_color_plan's is_std_diff (fixes the beta bug)
    policy   <- if (!is.null(plan_txt)) plan_txt$policy else plan_bg$policy
    m_txt    <- if (!is.null(plan_txt)) plan_txt$measure else NA_character_
    m_bg     <- if (!is.null(plan_bg))  plan_bg$measure  else NA_character_
    orient   <- if (identical(type, "col")) "col" else "row"
    eff_word <- if (isTRUE(is_reg)) legend_reg_effect_word(cn) else NA_character_
    ref      <- legend_ref_info(x, col, m_txt, orient)
    ci_type  <- get_ci_type(col)
    sig <- paste(m_txt, m_bg, policy, orient, is_std, eff_word, ref$kind, ref$label, ci_type, sep = "\r")
    list(col_var = cv, plan_txt = plan_txt, plan_bg = plan_bg,
         measure_text = m_txt, measure_bg = m_bg,
         is_mean = is_mean, is_std = is_std, is_coef = is_coef,
         policy = policy, orientation = orient, ci_type = ci_type,
         is_reg = is_reg, eff_word = eff_word, ci_settings = cis, shades = shades,
         ref = ref, sig = sig)
  })
  purrr::compact(specs)
}

#' Build the colour legend of a table
#'
#' Internal. Returns one legend line per colour-signature group. For \code{medium = "excel"} each line
#' is a list of runs \code{list(text, color, bold)} (for a rich-text cell); otherwise a character string.
#' @param x A \code{tabxplor_tab}.
#' @param medium One of "console", "html", "md", "excel", "plain".
#' @param style "terse" (compact, console default) or "prose" (full sentences, export default).
#' @param lang NULL (auto from locale) / "en" / "fr".
#' @param colored Whether to colour the break-words.
#' @param theme,color_type Palette theme / family (default from options).
#' @param classes `medium = "html"` only: emit the break-words as CSS slot classes rather than inline
#'   hex, because a tabxplor stylesheet ships with the output (`tab_kable(engine = "html")`). Then the
#'   legend follows a theme toggle exactly like the cells it describes. `FALSE` (the kableExtra engine,
#'   which carries no stylesheet of ours) keeps inline hex.
#' @return A character vector (or, for excel, a list of run-lists), or NULL when nothing is coloured.
#' @keywords internal
tab_color_legend <- function(x, medium = c("console", "html", "md", "excel", "plain"),
                             style = NULL, lang = NULL, colored = TRUE,
                             theme = NULL, color_type = NULL, classes = FALSE) {
  medium <- match.arg(medium)
  if (is.null(style))      style      <- if (identical(medium, "console")) "terse" else "prose"
  if (is.null(theme))      theme      <- getOption("tabxplor.color_style_theme", "light")
  if (is.null(color_type)) color_type <- getOption("tabxplor.color_style_type", "text")

  # apply the resolved language for the gettext lookups. LANGUAGE env is the reliable, mid-session,
  # R>=4.1 lever (Sys.setLanguage() needs R>=4.2 and is flaky on Windows); restored on exit.
  # WARNING: setting LANGUAGE is NOT enough on its own -- glibc caches translated strings, and the
  # cache is only invalidated by setlocale()/bindtextdomain()/textdomain(). Without a flush the
  # switch silently no-ops on Linux once the cache is warm (it happens to work on Windows/macOS,
  # which is why this went unnoticed until CI): `lang = "fr"` returned English. Flush BEFORE (drop
  # any entry cached under the previous language) and AFTER (don't leak ours to the next caller).
  # bindtextdomain() is the portable lever -- the older Sys.setlocale() trick fails on musl
  # (withr#213); this mirrors withr::local_language(), which we can't call (Suggests-only).
  # NOTE: gettext ignores LANGUAGE altogether when the locale is "C", so this cannot translate
  # under LANG=C (e.g. inside testthat, which sets LANG=C) -- that is a gettext rule, not a bug here.
  lg  <- legend_resolve_lang(lang)
  old <- Sys.getenv("LANGUAGE", unset = NA_character_)
  flush_gettext_cache()
  Sys.setenv(LANGUAGE = lg)
  flush_gettext_cache()
  on.exit({
    if (is.na(old)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = old)
    flush_gettext_cache()
  }, add = TRUE)

  specs <- legend_specs(x)
  if (length(specs) == 0) return(NULL)

  grp        <- split(specs, purrr::map_chr(specs, "sig"))
  show_names <- length(grp) > 1
  lines <- purrr::map(grp, function(g) {
    spec <- g[[1]]
    spec$col_names <- unique(purrr::map_chr(g, "col_var"))
    toks <- if (identical(style, "prose")) legend_tokens_prose(spec, lg, show_names)
            else                           legend_tokens_terse(spec, lg, show_names)
    legend_render_line(toks, medium, theme, color_type, colored, classes = classes)
  })

  # enc2utf8 the catalog output (gettext may return the native encoding on some platforms).
  if (identical(medium, "excel")) {
    return(unname(purrr::map(lines, function(line)
      purrr::map(line, function(r) { r$text <- enc2utf8(r$text); r }))))  # run-lists
  }
  enc2utf8(unname(unlist(lines)))
}
# tab_color_legend(tabs[[7]], medium = "console") %>% cli::cat_line()

# Phase 13a: the level -> palette-slot mapping now lives with the break scales themselves
# (mk_color_scale() precomputes over$slots / under$slots via intensity_slots(), R/tab_classes.R),
# so the old color_slot_table() / build_slots() lookups are gone. fmt_color_plan() reads the
# per-side breaks + slots directly; fmt_color_slots() folds + findInterval() per direction.

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

  n      <- length(x)
  none   <- logical(n)                   # == rep(FALSE, length(x))
  is_rm  <- type %in% c("row", "mean")   # scalar: row/mean share the reference logic
  comp_t <- isTRUE(comp_all)             # NA-safe scalar branch selectors: comp_all may be NA,
  comp_f <- isFALSE(comp_all)            #   then neither arm fires -> `none` (as the old case_when)
  m      <- mode[1]

  # DESIGN (Phase 10c): the former 3 x switch(mode) x dplyr::case_when collapsed to base boolean
  # composition. Every branch selector (type/comp_all/ref/color/totcol/refcol) is a SCALAR column
  # attribute, so each case_when really selected ONE arm; the arms are pure per-cell boolean of the
  # subsettable field masks (totrows/refrows/tottab_*). Byte-identical to the case_when output (incl.
  # the comp_all==NA "fall through to all-FALSE" and the mode/type default arms), with no per-arm
  # rep(FALSE)/DataMask allocation. Equivalence relied on by format()'s .ref memoization:
  # get_reference(x[mask], mode) == get_reference(x, mode)[mask].
  if (color %in% c("OR", "or")) {
    switch(m,
           "cells" = ,                                      # cells and lines identical for OR
           "lines" = if      (is_rm && comp_f) refrows
                     else if (is_rm && comp_t) tottab_ref
                     else if (type == "col")   rep(refcol, n)
                     else                      none,
           "all_totals" = if      (is_rm && ref == "tot" && comp_f) totrows | refcol
                          else if (is_rm && ref == "tot" && comp_t) tottab_line | refcol
                          else if (type == "col" && ref == "tot")   totrows | refcol
                          else if (is_rm && comp_f)                 refrows | refcol
                          else if (is_rm && comp_t)                 tottab_ref | refcol
                          else if (type == "col")                   refrows | refcol
                          else                                      none
    )

  } else if (ref == "tot") {
    switch(m,
           "cells" = if      (is_rm && comp_f)    totrows & !totcol
                     else if (is_rm && comp_t)    tottab_line & !totcol
                     else if (type == "col")      totcol & !totrows
                     else if (type == "all")      totrows & totcol
                     else if (type == "all_tabs") tottab_line & totcol
                     else                         none,
           "lines" = if      (is_rm && comp_f)    totrows
                     else if (is_rm && comp_t)    tottab_line
                     else if (type == "col")      rep(totcol, n)
                     else if (type == "all")      totrows & totcol
                     else if (type == "all_tabs") tottab_line & totcol
                     else                         none,
           "all_totals" = if (type %in% c("n", "col", "all") || (is_rm && comp_f)) totrows | totcol
                          else if (type == "all_tabs" || (is_rm && comp_t))        tottab_line | totcol
                          else                                                     none
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
    switch(m,
           "cells" = if      (is_rm && comp_f) refrows & !totcol
                     else if (is_rm && comp_t) tottab_ref & !totcol
                     else if (type == "col")   refcol & !totrows
                     else                      none,
           "lines" = if      (is_rm && comp_f) refrows
                     else if (is_rm && comp_t) tottab_ref
                     else if (type == "col")   rep(refcol, n)
                     else                      none,
           "all_totals" = if      (is_rm && comp_f) refrows | totcol
                          else if (is_rm && comp_t) tottab_ref | totcol
                          else if (type == "col")   totrows | refcol
                          else                      none
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
  # Phase 10i-A: a composite column shows its PRIMARY type in the tibble header (e.g. "row%"), not
  # the raw "{pct} (n={n})" template.
  display  <- display_primary(get_display(x)) %>% unique()
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
    stringr::str_replace("^coef-coef", "coef") %>%   # Phase 12c
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
  display  <- display_primary(get_display(x)) %>% unique()
  display  <- ifelse(length(display) > 1, "mixed", display)
  type     <- get_type(x)
  row_mean <- type %in% c("row", "mean")
  if (type %in% c("row", "col", "all", "all_tabs")) type <- paste0(type, "%")
  ci <- get_ci_type(x)
  if (display == "ci" & ci %in% c("cell", "diff")) display <- paste0("ci_", ci)

  out <- paste0("fmt-", type, "-", display) %>%
    stringr::str_replace("-n-n", "-n") %>%
    stringr::str_replace("-mean-mean", "-mean") %>%
    stringr::str_replace("-coef-coef", "-coef") %>%   # Phase 12c
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

  # Phase 9c: the reconcile is scalar-attribute picking; base-R if/else replaces the 9 dplyr::if_else
  # (~3x faster per call, byte-identical). This method drives EVERY c()/vec_c()/bind/group over fmt
  # columns -- the compact merge's per-column vec_ptype_common() reduce is the hottest caller (the
  # entire tab() merge marginal, dev/tabxplor_1.4.0_decisions.md 30). WARNING: every same_* is a
  # non-NA length-1 logical EXCEPT `same_comp` (comp_all is NA on count columns, so binding a count
  # with a pct column gives same_comp = NA -> dplyr::if_else returned NA; a bare `if (NA)` would
  # ERROR) -> it is checked with is.na() first. `color` is length <= 2 (§9.1) -> ifelse when 2.
  new_fmt(
    type     = if (same_type)      type_x      else "mixed",
    comp_all = if (is.na(same_comp)) NA else if (same_comp) comp_x else FALSE,
    ref      = if (same_diff_type) diff_type_x else "",
    ci_type  = if (same_ci_type)   ci_type_x   else "",
    col_var  = if (same_col_var)   col_var_x   else "several_vars",
    totcol   = if (same_totcol)    totcol_x    else FALSE,
    refcol   = if (same_refcol)    refcol_x    else FALSE,
    color    = if (length(same_color) == 1L) { if (same_color) color_x else "" }
               else ifelse(same_color, color_x, ""),
    color_signif = if (same_signif) signif_x else "ignore"
  )
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


