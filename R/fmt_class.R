# PURPOSE: Define tabxplor_fmt, a vctrs record class for formatted cross-table cells.
# ROLE: Foundation of the entire package. Every numeric column in a tabxplor_tab is fmt.
# KEY CONSTRAINTS:
#   - Adding a new field requires updating: new_fmt(), fmt(), format.tabxplor_fmt(),
#     pillar_shaft.tabxplor_fmt(), vec_arith methods, and possibly tab_pct/tab_ci/tab_chi2.
#   - Fields are per-cell (vctrs::field), attributes are per-column (attr). Do not confuse.
#   - pct is stored as 0-1 internally; multiplied by 100 only in format().
#   - `diff` is always a DIFFERENCE (Phase 2 flipped the numeric one; the ratio moved to `ratio`).
#   - Display glyph constants (mult_sign, div_sign, unbrk, sigma_sign, fig_space) live in utils.R.
#     fig_space (U+2007) is the pad wherever the output is rendered in a PROPORTIONAL font.
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
                  "grandtot", "nr", "nc", "e", "contrib", "signed_contrib", "contrib_unc",
                  "statistic", "df", "min_e", "w", "group_id"))

# The `ctx` fields of the tab_build() pipeline (Phase 7d-ii) and reg_build()'s `shared` list
# (Phase 17h). Each stage starts with `list2env(ctx, environment())` (R/tab.R: tab_setup /
# tab_prepare_pop / tab_aggregate / tab_transform / tab_assemble_tables / tab_assemble_output;
# R/tab_reg.R: reg_build), which binds every field as a local -- correct at run time, but
# invisible to codetools, which then reports each one as an undefined global. Listing them here
# is the only way to keep R CMD check quiet short of unpacking ~70 fields by hand.
utils::globalVariables(c(
  "by_table", "chi2", "chi2_num", "cleannames", "col_vars", "col_vars_num", "col_vars_quo",
  "col_vars_text", "color_ci", "color_ctr", "color_diff_OR", "color_num", "comp", "conf_level",
  "data", "digits", "fine_fused", "fine_num", "lv1", "method_cell", "method_diff",
  "method_ratio", "method_mean_diff", "method_mean_ratio", "na",
  "na_drop_all_quo", "na_num", "na_text", "names_prefix", "names_sort", "other_if_less_than",
  "other_level", "output", "pct", "pct_vect", "ref", "ref2", "remove_levels", "row_vars",
  "row_vars_quo", "spread_vars", "stars", "subtext", "tab_row_names", "tab_vars", "tab_vars_quo",
  "tabs_num", "tot_cols_type", "total_names", "totaltab", "totaltab_name", "totrow",
  "with_filter", "wt", "wt_quo", "add_n", "add_pct", "ci", "OR", "color_signif",
  "color_ratio_ci", "ci_scale",
  # tab_build ctx fields added by Phases 17e/j/k (settings spine, robust tests, var labels):
  "cached_tests", "common_totrow", "defer_level_merge", "design_spec", "n_min", "inference_mode",
  "var_labels",
  # reg_build()'s `shared` list fields (Phase 17h):
  "at", "baseline", "compare", "effect", "empirical", "estimate_display",
  "inverse_two_level_factors", "method", "multiplier", "spread_models", "stats",
  "union_predictors", "weighted"))

# NSE column symbols in dplyr verbs over ordinary data frames:
#   `var`               -- reg_build()'s group_by(var) on the regression skeleton (R/tab_reg.R)
#   `name`/`size`/`color` -- tab_xl_plan_one()'s font/style plan tibbles (R/tab_xl.R)
#   marital/race/partyid/rincome/relig -- gss_cat_data_formatting()'s mutate() over forcats::gss_cat
utils::globalVariables(c("var", "name", "size", "color",
                         "marital", "race", "partyid", "rincome", "relig"))

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
#' \code{\link[vctrs:field]{vctrs:field<-}}. A \code{fmt} vector have 21 fields :
#' \code{n}, \code{digits}, \code{display}, \code{wn}, \code{pct}, \code{mean},
#' \code{diff}, \code{ratio}, \code{ctr}, \code{var}, \code{ci_inf}, \code{ci_sup},
#' \code{pvalue}, \code{or}, \code{tot_n}, \code{n_eff}, \code{obs}, \code{gap_se},
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
#'  \code{"mean"}, \code{"var"}, \code{"ci"}, \code{"ratio"} (the cell-to-reference ratio;
#'  the legacy synonym \code{"rr"} still resolves to it),
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
#' Calculate with \code{\link{tab_chi2}}. The cell's adjusted standardized residual is not a
#' field of its own: it is recovered from \code{pvalue} and this field's sign, and readable with
#' \code{display = "resid"} (see \code{\link{tab}}).
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
#' @param n_eff The effective sample size used for this cell's confidence interval:
#' the DESIGN-based \code{p(1-p) / Var_design(p)} (a mean: \code{s^2 / Var_design(mean)})
#' when a \code{survey::svydesign} was passed as \code{data}, else Kish's
#' \code{(sum w)^2 / sum(w^2)} when \code{options(tabxplor.kish_neff = TRUE)} on weighted
#' data, else \code{NA} (the CI then falls back to the raw unweighted base).
#' A double vector the length of \code{n}. Non-displayed.
#' @param obs The value this cell's estimate is COMPARED TO by the \code{tab_reg} colour
#' measures \code{"adjustment"} and \code{"between_groups"}, on the cell's own scale: the
#' observed (crude, unadjusted) effect beside a model effect, or -- under
#' \code{split_var} with \code{color = "between_groups"} -- the reference group's estimate.
#' \code{NA} on cross-tables and wherever there is no counterpart (the Constant, numeric
#' predictors, multinomial / ordinal outcomes), which leaves those cells uncoloured.
#' A double vector the length of \code{n}; displayable as \code{display = "\{obs\}"}.
#' @param gap_se The standard error of the GAP between this cell's estimate and \code{obs},
#' on the estimate's own test scale (the log-ratio when \code{ci_type} is \code{"or"} or
#' \code{"ratio"}, the plain difference when \code{"diff"}). Written by \code{tab_reg} where
#' the two estimates are independent (\code{split_var} groups), so that
#' \code{color = "between_groups"} can honour \code{color_signif}; \code{NA} everywhere
#' else, which leaves the significance policies inert there.
#' A double vector the length of \code{n}. Non-displayed.
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
#'   (except mean columns, from numeric variables). Under
#'   \code{color_signif = "guaranteed_effect"} it switches to the absolute adjusted standardized
#'   residual instead -- see \code{\link{tab}}.
#' }
#' @param color_signif How significance gates the color, as a single string
#' (\code{"ignore"} / \code{"grey_non_signif"} / \code{"guaranteed_effect"}). See \code{\link{tab}}.
#' @param model_family For regression tables (\code{\link{tab_reg}}): the column's model family
#' (\code{"binomial"}, \code{"gaussian"}, \code{"poisson"}, \code{"multinomial"}, \code{"ordinal"}),
#' as a single string. Empty (\code{""}) on cross-tables. Lets a table mix several dependents with
#' different families, each column keeping its own effect wording.
#' @param role For regression tables (\code{\link{tab_reg}}): the column's role, \code{"model"} for a
#' model-estimate column or \code{"emp"} for an empirical (crude) companion column. Empty (\code{""})
#' on cross-tables. Read by the colour legend to name each column's effect without matching its label.
#' @param conf_level The confidence level this column's stored interval and its significance
#' thresholds were computed at, as a single number in (0, 1). \code{NA} (the default) means
#' "unknown": every threshold in the colour engine then falls back to
#' \code{options("tabxplor.conf_level")}. It is stored per COLUMN because colours are resolved per
#' column at print time and cannot see the table's \code{conf_level} argument -- without it a table
#' built at a 99 percent level would be greyed at 95 percent.
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
#' tabs |> mutate(across(where(is_totcol), ~ "total column"))
#'
#' # To identify the total rows, and work with them :
#' is_totrow(tabs)
#' tabs |>
#'   mutate(across(
#'     where(is_fmt),
#'     ~ if_else(is_totrow(.), true = "into_total_row", false = "normal_cell")
#'   ))
#'
#' # To identify the total tables, and work with them :
#' tottabs <- is_tottab(tabs)
#' tabs |> tibble::add_column(tottabs) |>
#'   mutate(total = if_else(tottabs, "part of a total table", "normal cell"))
#'
#' # To access the displayed numbers, as numeric vectors :
#' tabs |> mutate(across(where(is_fmt), get_num))
#'
#' # To access the displayed numbers, as character vectors (without colors) :
#' tabs |> mutate(across(where(is_fmt), format))
#'
#' # To access the (non-displayed) differences of the cells percentages from totals :
#' tabs |> mutate(across(where(is_fmt), ~ vctrs::field(., "diff")))
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
                n_eff     = rep(NA_real_, length(n)),
                obs       = rep(NA_real_, length(n)),
                gap_se    = rep(NA_real_, length(n)),

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
                color_signif = "ignore",
                model_family = ""   ,   # Phase 15e: per-column regression family ("" on crosstabs)
                role         = ""   ,   # Phase 17c: per-column role -- "model"/"emp" on reg columns
                conf_level   = NA_real_) { # Last Phase z13: the level this column's interval was built at

  # DESIGN: these 8 fields set the recycling reference length. display, diff, ratio, or,
  # the ci bounds, pvalue, tot_n and the in_* flags are recycled TO it below, so they must
  # not be passed longer than these (vec_recycle would error, not extend).
  max_size <- list(n, wn, pct, digits, ctr, mean, var, ci) |> #display
    purrr::map_int(length) |> max()

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
  # Last Phase s: the effective sample size used for this cell's CI (Kish n_eff when opted in,
  # else NA -> tab_ci/num_core fall back to the raw unweighted base). Non-displayed, CI-only.
  n_eff   <- vctrs::vec_recycle(vctrs::vec_cast(n_eff  , double())   , size = max_size)
  # Last Phase z5: the value this cell is COMPARED TO by `color = "adjustment"` /
  # "between_groups" (the crude effect, or the reference group's estimate), on the cell's own
  # scale. NA everywhere else -> those measures score NA -> uncoloured.
  obs     <- vctrs::vec_recycle(vctrs::vec_cast(obs    , double())   , size = max_size)
  # Last Phase z8: the SE of the GAP between the estimate and `obs`, on the estimate's own test
  # scale (log-ratio for or/ratio, plain difference for diff). Written only where the two are
  # independent (split_var groups); NA elsewhere -> the gap has no interval -> the significance
  # policies stay inert on those cells. Non-displayed.
  gap_se  <- vctrs::vec_recycle(vctrs::vec_cast(gap_se , double())   , size = max_size)

  # Phase 3a: the public `ci` arg is a symmetric half-width; store it as ABSOLUTE bounds
  # around the estimate the interval is centred on (the difference for diff-type CIs, the mean
  # for cell means, the proportion otherwise), matching how tab_ci()/tab_num() now store real
  # asymmetric bounds. Explicit ci_inf/ci_sup win; get_ci() reads the half-width back as
  # ci_sup - centre. See dev/tabxplor_2.0.0_decisions.md §1, §20.
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
  model_family <- vctrs::vec_recycle(vctrs::vec_cast(model_family, character()), size = 1)
  role         <- vctrs::vec_recycle(vctrs::vec_cast(role        , character()), size = 1)
  conf_level   <- vctrs::vec_recycle(vctrs::vec_cast(conf_level  , double()   ), size = 1)

  new_fmt(n = n, display = display, digits = digits,
          wn = wn, pct = pct,  mean = mean,
          diff = diff, ratio = ratio, ctr = ctr, var = var,
          ci_inf = ci_inf, ci_sup = ci_sup, pvalue = pvalue, or = or, tot_n = tot_n,
          n_eff = n_eff, obs = obs, gap_se = gap_se,
          in_totrow = in_totrow, in_tottab = in_tottab, in_refrow = in_refrow,
          type = type, comp_all = comp_all,  ref = ref,
          ci_type = ci_type, col_var = col_var, totcol = totcol, refcol = refcol,
          color = color, color_signif = color_signif, model_family = model_family,
          role = role, conf_level = conf_level)
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
  # ratio (canonical; the legacy synonym rr is aliased to it) -> the `ratio` field,
  # or/OR/or_pct/OR_pct->or, obs->obs. format.tabxplor_fmt() renders these plus the CI/label variants
  # (pct_ci, mean_ci, or_pct, OR_pct). When adding a display value, keep this map, set_num() and
  # format() in sync (see the /vctrs-field skill).
  out     <- get_n(x)
  # Phase 10i-A: resolve composite templates ("{pct} (n={n})") to their PRIMARY field before the
  # dispatch masks -- byte-identical (and one fixed grepl) when the column carries no composite.
  display <- display_primary(get_display(x))
  nas     <- is.na(display)
  out[!nas & display == "wn"     ] <- get_wn  (x)[!nas & display == "wn"     ]
  out[!nas & display == "pct"    ] <- get_pct (x)[!nas & display == "pct"    ]
  out[!nas & display == "pvalue" ] <- get_pvalue(x)[!nas & display == "pvalue" ]  # Phase 17c: honest p in the pvalue field
  out[!nas & display == "diff"   ] <- get_diff(x)[!nas & display == "diff"   ]
  out[!nas & display == "coef"   ] <- get_diff(x)[!nas & display == "coef"   ]  # Phase 12c: raw regression coef -> diff field
  out[!nas & display == "gof"    ] <- get_diff(x)[!nas & display == "gof"    ]  # Phase 12f: model-fit stat (N/R2/AIC/...) -> diff field
  out[!nas & display == "pct_ci" ] <- get_pct (x)[!nas & display == "pct_ci" ]
  out[!nas & display == "ctr"    ] <- get_ctr (x)[!nas & display == "ctr"    ]
  # Last Phase z4: DERIVED (no field of its own) -- the adjusted standardized residual behind
  # `color = "contrib"`'s significance. Read-only: set_num() has no matching arm.
  out[!nas & display == "resid"  ] <- fmt_resid(x)[!nas & display == "resid"  ]
  out[!nas & display == "mean"   ] <- get_mean(x)[!nas & display == "mean"   ]
  out[!nas & display == "mean_ci"] <- get_mean(x)[!nas & display == "mean_ci"]
  out[!nas & display == "var"    ] <- get_var (x)[!nas & display == "var"    ]
  out[!nas & display == "ci"     ] <- get_ci   (x)[!nas & display == "ci"     ]
  out[!nas & display == "ratio"] <- get_ratio(x)[!nas & display == "ratio"]
  out[!nas & display %in% c("or", "OR")] <- get_or(x)[!nas & display %in% c("or", "OR")     ]
  # Last Phase z9: BOTH spellings, as format() has always matched (`c("or_pct", "OR_pct")`). "OR_pct"
  # is written verbatim by the jamovi display ComboBox, and the missing arm made such a cell fall
  # through to the raw count.
  out[!nas & display %in% c("or_pct", "OR_pct")] <- get_or(x)[!nas & display %in% c("or_pct", "OR_pct")]
  # Last Phase z5: the value this cell is COMPARED TO (the observed/crude effect, or the reference
  # group's estimate). A real stored field, so -- unlike the derived `resid` -- it round-trips:
  # set_num() has a matching arm.
  out[!nas & display == "obs"    ] <- get_obs (x)[!nas & display == "obs"    ]
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
  out[!nas & display == "ratio"] <- set_ratio(x[!nas & display == "ratio"], value[!nas & display == "ratio"])
  # Last Phase z9: ONE mask for target and value. The value side read only "or", so a column displaying
  # "OR" fed a length-0 value into a non-empty target. Same pass adds the or_pct/OR_pct arms get_num()
  # and format() already had -- the three maps are meant to stay in sync (see the /vctrs-field skill).
  or_m <- !nas & display %in% c("or", "OR", "or_pct", "OR_pct")
  out[or_m] <- set_or(x[or_m], value[or_m])
  out[!nas & display == "obs" ] <- set_obs(x[!nas & display == "obs" ], value[!nas & display == "obs" ])  # Last Phase z5
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
#' @keywords internal
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
#' @keywords internal
get_type.tabxplor_fmt <- function(x, ...) attr(x, "type", exact = TRUE)
#' Get types of fmt columns
#' @inheritParams fmt
#' @return A character vector with the data.frame column's types.
#' @export
#' @keywords internal
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
#' @keywords internal
is_totrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' Test function to detect cells in total rows
#' @method is_totrow tabxplor_fmt
#' @inheritParams fmt
#' @return A logical vector with the totrow field.
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
is_tottab.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' Test function to detect cells in total tables
#' @method is_tottab tabxplor_fmt
#' @inheritParams fmt
#' @return A logical vector with the tottab field.
#' @export
#' @keywords internal
is_tottab.tabxplor_fmt <- function(x, ...) vctrs::field(x, "in_tottab")
#' Test function to detect cells in total tables
#' @param partial Should partial total tabs be counted as total tabs ? Default to FALSE.
#' @inheritParams fmt
#' @return A list of logical vectors, with the data.frame column's tottab fields.
#' @export
#' @keywords internal
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
#' @keywords internal
set_display.default <- function(x, value) {
return(x)
}
#' Set the "display" vctrs::field of a \code{fmt} vector.
#' @inheritParams fmt
#' @details The special value \code{value = "num_ci"} is a type-adaptive alias for the
#'   \code{"\{base\} \{ci\}"} composite: it writes \code{"\{pct\} \{ci\}"} on percentage/frequency
#'   columns and \code{"\{mean\} \{ci\}"} on numeric (mean) columns, so each value cell shows its base
#'   value followed by whatever confidence interval the table carries (a cell, difference or ratio CI,
#'   as driven by \code{ci = } / \code{color}). It is a display overlay: cells with no CI show the bare
#'   base value.
#' @return A fmt vectors with the wanted display.
#' @export
#' @keywords internal
set_display.tabxplor_fmt <- function(x, value) {
  # "num_ci" is a type-adaptive alias for the "{base} {ci}" composite: show each value cell with the
  # confidence interval the table already carries (the difference / ratio CI driven by ci = / color,
  # not a forced cell CI). Resolve it per the column's own type -- see fmt_apply_num_ci().
  if (length(value) == 1L && identical(as.character(value), "num_ci")) return(fmt_apply_num_ci(x))
  value <- vctrs::vec_cast(value, character()) |> vctrs::vec_recycle(size = length(x))
  vctrs::`field<-`(x, "display", value)
}

# num_ci is a type-adaptive display alias: it writes the composite "{mean} {ci}" template on numeric
# (mean) columns and "{pct} {ci}" on percentage/frequency columns, so each value cell shows its base
# value plus whatever confidence interval the table computes (cell, difference or ratio). It is a
# pure DISPLAY overlay applied with tab_apply_display()'s EXACT value-cell eligibility, so the result
# is byte-identical to writing the concrete template per column: only genuine value cells where BOTH
# fields render (non-NA) get it -- count-only, p-value and total-marker cells keep their own token,
# a cell with no CI keeps its bare base, and a cell already showing value+CI (pct_ci / mean_ci from
# ci = "cell") is left untouched (it is already "{base} {ci}").
# Why this exists: shared by set_display.tabxplor_fmt() and tab_apply_display() so the "num_ci" alias
# resolves the same way whether requested at build (tab(display=)) or post-hoc (set_display()).
fmt_apply_num_ci <- function(col) {
  base <- if (identical(get_type(col), "mean")) "mean" else "pct"
  tmpl <- paste0("{", base, "} {ci}")
  fields <- parse_display_template(tmpl)$fields
  d <- get_display(col)
  elig <- d %in% c("pct", "mean", "n", "wn")
  for (f in fields) elig <- elig & !is.na(get_num(set_display(col, f)))
  d[elig] <- tmpl
  set_display(col, d)
}
#' Set the "display" vctrs::field of a \code{fmt} vector.
#' @inheritParams fmt
#' @return The entered objects, with all fmt vectors with the wanted display.
#' @export
#' @keywords internal
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
#' @keywords internal
is_totcol.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("totcol")(x)),
         yes = purrr::attr_getter("totcol")(x),
         no  = FALSE)
}
#' Test function for total columns
#' @inheritParams fmt
#' @return A single logical vector with the totcol attribute
#' @export
#' @keywords internal
is_totcol.tabxplor_fmt <- function(x, ...) attr(x, "totcol", exact = TRUE)
#' Test function for total columns
#' @inheritParams fmt
#' @return A logical vector, with the data.frame column's totcol attributes.
#' @export
#' @keywords internal
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
#' @keywords internal
is_refrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' Test function to detect cells in reference rows
#' @method is_refrow tabxplor_fmt
#' @inheritParams fmt
#' @return  A logical vector with the in_refrow field.
#' @export
#' @keywords internal
is_refrow.tabxplor_fmt <- function(x, ...) vctrs::field(x, "in_refrow")
#' Test function to detect cells in reference rows
#' @method is_refrow data.frame
#' @param partial Should partial reference rows be counted as reference rows ? Default to FALSE.
#' @inheritParams fmt
#' @return A list of logical vectors with the in_refrow fields.
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
get_ref_type.tabxplor_fmt <- function(x, ...) attr(x, "ref", exact = TRUE)
#' Get differences type of fmt columns
#' @method get_ref_type data.frame
#' @inheritParams fmt
#' @return A character vector with the ref attribute.
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
get_ci_type.tabxplor_fmt <- function(x, ...) attr(x, "ci_type", exact = TRUE)
#' Get confidence intervals type of fmt columns
#' @method get_ci_type data.frame
#' @inheritParams fmt
#' @return A character vector with the ci_type attributes.
#' @export
#' @keywords internal
get_ci_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_ci_type(.))
}


#' @describeIn fmt set the confidence intervals type attribute of a \code{fmt} vector
# @param ci_type The type of confidence interval calculated in "ci", as a single string.
#' @return A modified fmt vector.
#' @export
set_ci_type   <- function(x, ci_type) {
  # The two MULTIPLICATIVE interval scales (neutral 1, read by ci_center() + the colour significance
  # gate + format()'s bracket), as opposed to the additive diff* ones (neutral 0):
  #   "or"    (Phase 12a) -- a log-OR Wald exp() interval, centred on the odds ratio.
  #   "ratio" (Phase 14b) -- a Katz log-RR exp() interval, centred on the cell/reference ratio.
  stopifnot(ci_type %in% c("cell", "diff", "diff_row", "diff_col", "or", "ratio",
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
#' @keywords internal
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
#' @keywords internal
get_col_var.tabxplor_fmt <- function(x, ...) attr(x, "col_var", exact = TRUE)
#' Get names of column variable of fmt columns
#' @method get_col_var data.frame
#' @inheritParams fmt
#' @return A character vector with the col_var attributes.
#' @export
#' @keywords internal
get_col_var.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_col_var(.))

#' @describeIn fmt set the "col_var" attribute of a \code{fmt} vector
# @param col_var The name of the column variable, as a single string.
#' @return A modified fmt vector.
#' @export
set_col_var   <- function(x, col_var) {
  vctrs::vec_assert(col_var, character(), size = 1)
  `attr<-`(x ,"col_var" , col_var)
}


#' @describeIn fmt get the regression model family of fmt columns (at \code{fmt} or \code{tab} level)
#' @return A character vector with the fmt vectors' model_family attributes (\code{""} when unset,
#'   e.g. on cross-tables). On a data.frame, one value per column.
#' @export
get_model_family <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_chr(x, get_model_family))
  mf <- attr(x, "model_family", exact = TRUE)
  if (is.null(mf)) "" else mf
}

#' @describeIn fmt set the "model_family" attribute of a \code{fmt} vector (Phase 15e: the per-column
#'   regression family, "" on crosstabs)
# @param model_family The regression model family, as a single string.
#' @return A modified fmt vector.
#' @export
set_model_family <- function(x, model_family) {
  vctrs::vec_assert(model_family, character(), size = 1)
  `attr<-`(x ,"model_family" , model_family)
}

# Phase 17c: the per-column `role` attribute -- "model"/"emp" on a regression column, "" on a crosstab
# column. Written by the reg builders (R/tab_reg.R), read by the legend adapters (legend_specs /
# legend_reg_eff_word) instead of matching the "Emp." name prefix. Internal (no exported getter yet).
#' @keywords internal
#' @noRd
get_role <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_chr(x, get_role))
  r <- attr(x, "role", exact = TRUE)
  if (is.null(r)) "" else r
}

# Last Phase z13 (D3): the per-column `conf_level` attribute -- the level this column's stored interval
# and its significance thresholds were computed at.
#
# TWO accessors on purpose, and the split is load-bearing. The RAW one is for the reconcilers: binding
# two columns that never recorded a level must carry "unknown" forward as unknown, not bake today's
# option into the result (the twin of fmt_color_attr() beside get_color()). The RESOLVED one is for the
# colour engine, whose four thresholds used to read the option directly -- so a table built at
# conf_level = 0.99 printed 99 % intervals and 99 % stars while its greying stayed at 95 %.
#' @keywords internal
#' @noRd
fmt_conf_level_attr <- function(x) {
  cl <- attr(x, "conf_level", exact = TRUE)
  if (is.null(cl)) NA_real_ else cl
}

#' @keywords internal
#' @noRd
get_conf_level <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_dbl(x, get_conf_level))
  cl <- fmt_conf_level_attr(x)
  if (!is.finite(cl)) getOption("tabxplor.conf_level", 0.95) else cl
}

#' @keywords internal
#' @noRd
set_conf_level <- function(x, conf_level) {
  conf_level <- vctrs::vec_recycle(vctrs::vec_cast(conf_level, double()), size = 1)
  `attr<-`(x, "conf_level", conf_level)
}

# Project the table's confidence level onto every fmt column, at each build tail -- the ONE point where
# the call's `conf_level` and the finished columns are both in scope. Doing it per fmt() call site would
# mean a dozen-plus builders, each of which the next new builder would have to find again.
#' @keywords internal
#' @noRd
tab_stamp_conf_level <- function(tabs, conf_level) {
  if (length(conf_level) == 0L || !is.finite(conf_level[1])) return(tabs)
  if (is.list(tabs) && !is.data.frame(tabs))
    return(purrr::map(tabs, tab_stamp_conf_level, conf_level))
  for (nm in names(tabs)) if (is_fmt(tabs[[nm]]))
    tabs[[nm]] <- set_conf_level(tabs[[nm]], conf_level[1])
  tabs
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
#' @keywords internal
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
#' @keywords internal
is_refcol.tabxplor_fmt <- function(x, ...) attr(x, "refcol", exact = TRUE)
#' Test function for reference columns
#' @method is_refcol data.frame
#' @inheritParams fmt
#' @return A character vector with the ref_col attributes.
#' @export
#' @keywords internal
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
#' @keywords internal
get_color.default     <- function(x, ...) {
  a <- purrr::attr_getter("color")(x)
  if (is.null(a)) "" else a[1]
}
#' Get color
#' @method get_color tabxplor_fmt
#' @inheritParams fmt
#' @return A single character with the color attribute (the text channel).
#' @export
#' @keywords internal
get_color.tabxplor_fmt <- function(x, ...) attr(x, "color", exact = TRUE)[1]
#' Get color
#' @method get_color data.frame
#' @inheritParams fmt
#' @return A character vector with the color attributes.
#' @export
#' @keywords internal
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
# positional length-1-or-2 character vector [text, background]. Phase 17d: the stored `color`
# attribute is a CLEAN measure (diff/ratio/contrib/or); the legacy combined strings are decoded at the
# argument boundary (color_decode_legacy, R/tab.R) before they ever reach set_color(), so they are no
# longer accepted here.
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
  # Last Phase z5: `adjustment` / `between_groups` are tab_reg-only measures (they score the `obs`
  # field, which only a regression table fills), but they are ordinary measures at the STORAGE
  # boundary -- and unlike contrib / OR they ARE allowed on the background, which is the whole point of
  # the headline reading `color = c("OR", "adjustment")`: effect size in the text, what adjustment did
  # to it in the fill.
  ok <- c("diff", "ratio", "contrib", "OR", "adjustment", "between_groups", "")
  if (!all(color %in% ok)) {
    cli::cli_abort(c("Unknown color measure {.val {setdiff(color, ok)}}.",
                     "i" = "Valid measures: {.val {c('diff','ratio','contrib','or')}}.",
                     "i" = "In {.fn tab_reg} also: {.val {c('adjustment','between_groups')}}."))
  }
  if (length(color) == 2L && color[2] %in% c("contrib", "OR")) {
    cli::cli_abort("{.val {color[2]}} is a whole-cell measure; it cannot go on the background channel.")
  }
  if (all(c("adjustment", "between_groups") %in% color)) {
    cli::cli_abort(c(
      "{.val adjustment} and {.val between_groups} cannot be used together.",
      "i" = "Both score the same per-cell comparison value, so a cell can carry only one of them."))
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
# Last Phase z4: `resid` is a DERIVED field (fmt_resid(): the adjusted standardized residual, read back
# from the stored p-value + the contribution's sign), exactly as `ci` is derived from its bounds. It is
# read-only -- get_num() has an arm, set_num() deliberately does not.
# Last Phase z5: `obs` is a real stored FIELD (the value a reg cell is compared to), so unlike `resid`
# it is fully round-trippable -- get_num() reads it and set_num() writes it.
tabxplor_display_fields  <- c("pct", "n", "wn", "mean", "diff", "ratio", "ci", "or", "ctr", "var",
                              "resid", "obs")
# Phase 17d: the internal display token is now the canonical `ratio` (was `rr`). The alias table is
# READ-SIDE ONLY -- the legacy synonym `rr` (bare stored token / a `{rr}` composite) maps to `ratio`,
# so old objects still resolve, but nothing produces `rr` and every mask matches the single "ratio".
tabxplor_display_aliases <- c(rr = "ratio")

# Resolve a display-value vector to its PRIMARY simple token: a composite ("{field} ...") -> its
# first {field} (alias-applied); a simple token / NA -> unchanged. Gated so a column carrying no
# composite pays one fixed grepl and returns. A malformed token (no closing brace) is left as-is
# and falls through to get_num()'s default `n` -- never errors (robust to hand-injected templates).
display_primary <- function(display) {
  comp <- !is.na(display) & grepl("{", display, fixed = TRUE)
  if (any(comp)) display[comp] <- sub("^[^{]*\\{\\s*([^{}]+?)\\s*\\}.*$", "\\1", display[comp])
  # Read-side alias (Phase 17d): a bare / composite legacy token (only `rr` today) -> its canonical
  # internal token. The `%in%` guard keeps the common canonical path free of the match() pass, so the
  # no-alias hot case stays one fixed grepl + one cheap vector `%in%` (Phase 10i-A benchmark).
  al <- tabxplor_display_aliases
  if (any(display %in% names(al))) {
    hit <- match(display, names(al)); aliased <- !is.na(hit)
    display[aliased] <- unname(al[hit[aliased]])
  }
  display
}

# fmt_display_shows() -- does a cell's display ALREADY show this field, anywhere in its template?
#
# Last Phase z10: the html tooltip suppressed a line by testing display_primary(), i.e. the FIRST token
# only -- so every composite cell repeated its own bracket on hover. It shipped that way: an AME column
# reading "{diff} ({pct})" printed the adjusted percentage in the cell AND again in the tooltip, and the
# reg_marginal_column WARNING about the "prob_ratio" reference template was that same bug patched at one
# producer. One helper on the EXISTING template parser fixes it at the gate for every field at once.
#' @keywords internal
fmt_display_shows <- function(display, token) {
  out  <- !is.na(display) & display == token
  comp <- !is.na(display) & grepl("{", display, fixed = TRUE)
  if (any(comp)) {
    for (tmpl in unique(display[comp]))
      out[comp & display == tmpl] <- token %in% parse_display_template(tmpl)$fields
  }
  out
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
  # Ergonomics / back-compat: a bare field name (no braces) that is a known display field is treated as
  # the single-field template "{field}", so e.g. display = "ci" == display = "{ci}" (and "diff"/"pct"/...).
  # One general rule, not an ad-hoc "ci" case. A genuinely unknown bare value still hits the abort below.
  if (!grepl("[{}]", recipe) &&
      recipe %in% c(tabxplor_display_fields, names(tabxplor_display_aliases))) {
    recipe <- paste0("{", recipe, "}")
  }
  if (!grepl("[{}]", recipe)) {
    cli::cli_abort(c(
      "Invalid {.arg display} value {.val {recipe}}.",
      "i" = "Composite display uses a {{}} template listing the fields to combine,
             e.g. {.code {{pct}} (n={{n}})} or {.code {{diff}} {{ci}}}."
    ))
  }
  opens  <- stringi::stri_count_regex(recipe, "\\{")
  closes <- stringi::stri_count_regex(recipe, "\\}")
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

                    digits    = NULL,
                    display   = NULL,

                    wn        = NULL,
                    pct       = NULL,
                    mean      = NULL,
                    diff      = NULL,
                    ratio     = NULL,
                    ctr       = NULL,
                    var       = NULL,
                    ci_inf    = NULL,
                    ci_sup    = NULL,
                    pvalue    = NULL,
                    or        = NULL,
                    tot_n     = NULL,
                    n_eff     = NULL,
                    obs       = NULL,
                    gap_se    = NULL,

                    in_totrow = NULL,
                    in_tottab = NULL,
                    in_refrow = NULL,

                    comp_all  = NA   ,
                    ref = ""   ,
                    ci_type   = ""   ,
                    col_var   = ""   ,
                    totcol    = FALSE,
                    refcol    = FALSE,
                    color     = ""   ,
                    color_signif = "ignore",
                    model_family = ""   ,   # Phase 15e: regression model family per column ("" on crosstabs)
                    role      = ""   ,   # Phase 17c: column role -- "model"/"emp" on reg columns, "" on crosstabs
                    # Last Phase z13 (D3): the confidence level THIS column's stored interval and its
                    # significance thresholds were computed at. NA = unknown -> get_conf_level() falls
                    # back to options(tabxplor.conf_level). It is a per-COLUMN fact because colours are
                    # resolved per column at print time and cannot see the table's `conf_level`
                    # argument -- without it a table built at 99 % was greyed at 95 %.
                    conf_level = NA_real_,
                    ..., class = character()
) {
  # stopifnot(
  #   all(display %in% c("n", "wn", "pct", "pct_ci", "ctr", "mean", "mean_ci", "var", "ci")),
  #   type %in% c("row", "col", "all", "all_tabs", "mixed", NA_character_)
  # )

  # list(display, n, wn, pct, digits, ctr, mean, var, ci, col_var, totcol, type) |>
  #   purrr::map(print)
  # cat("\n")

  # list(n = n, display = display, digits = digits,
  #      wn = wn, pct = pct, mean = mean,
  #      diff = diff, ctr = ctr, var = var, ci = ci,
  #      in_totrow = in_totrow, in_tottab = in_tottab,
  #      in_refrow = in_refrow) |>
  #   purrr::map(length) |> print()
  # cat("\n")

  # Last Phase z6: the 18 unset fields share ONE `NA` vector instead of allocating one each.
  #   R's copy-on-write makes this invisible -- any `field<-` / vec_slice / arithmetic duplicates
  #   the touched field alone -- but a freshly built record costs 1 allocation, not 17 (measured
  #   at 1e6 cells: 9.9 MB instead of 129.7 MB; about half the sharing survives the pipeline).
  #   The `display` default is base-R for the same reason: the dplyr::case_when() it replaces cost
  #   90 us per call, more than half the whole constructor, on all 210 calls of a tab_many() build.
  #   `%in%` (not ==) so an NA `type` falls through to "n", exactly as case_when did.
  #   WARNING: NULL defaults, so a field passed as NULL is "unset", not an error. No caller does
  #   that; if one ever needs an empty column it must pass a 0-length vector with length(n) == 0.
  #   See dev/empty_vctrs_fields_sparse_record.md (why the fields stay ALWAYS present).
  size <- length(n)
  nas  <- rep(NA_real_, size)
  fls  <- rep(FALSE   , size)
  if (is.null(display)) {
    display <- rep("n", length(type))
    display[type %in% c("row", "col", "all", "all_tabs")] <- "pct"
    display[type %in% "mean"                            ] <- "mean"
  }
  if (is.null(digits)) digits <- rep(0L, size)
  if (is.null(wn    )) wn     <- nas
  if (is.null(pct   )) pct    <- nas
  if (is.null(mean  )) mean   <- nas
  if (is.null(diff  )) diff   <- nas
  if (is.null(ratio )) ratio  <- nas
  if (is.null(ctr   )) ctr    <- nas
  if (is.null(var   )) var    <- nas
  if (is.null(ci_inf)) ci_inf <- nas
  if (is.null(ci_sup)) ci_sup <- nas
  if (is.null(pvalue)) pvalue <- nas
  if (is.null(or    )) or     <- nas
  if (is.null(tot_n )) tot_n  <- nas
  if (is.null(n_eff )) n_eff  <- nas
  if (is.null(obs   )) obs    <- nas
  if (is.null(gap_se)) gap_se <- nas
  if (is.null(in_totrow)) in_totrow <- fls
  if (is.null(in_tottab)) in_tottab <- fls
  if (is.null(in_refrow)) in_refrow <- fls

  #vctrs::vec_assert(display, character()) #check display or size
  display <- vctrs::vec_recycle(display, size = size)
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
         tot_n = tot_n, n_eff = n_eff, obs = obs, gap_se = gap_se,
         in_totrow = in_totrow, in_tottab = in_tottab,
         in_refrow = in_refrow),
    type = type, comp_all = comp_all, ref = ref,
    ci_type = ci_type, col_var = col_var, totcol = totcol, refcol = refcol,
    color = color, color_signif = color_signif[1], model_family = model_family[1],
    role = role[1], conf_level = conf_level[1],
    class = c(class, "tabxplor_fmt"))
  #access with fields() n_fields() vctrs::field() vctrs::`field<-`() ;
  #vec_data() return the tibble with all fields
}

# The 21 per-cell record FIELDS of new_fmt(), single-sourced so the column-attribute list below can be
# DERIVED rather than hand-maintained. (Defect: model_family became a 10th attribute in Phase 15e but
# was never added to the hand-written fmt_col_attrs -> it was silently dropped on every carrier
# round-trip / bind.) Adding a FIELD updates this vector (the /vctrs-field checklist forces it);
# adding an ATTRIBUTE (a new_fmt() formal that is not a field) needs NO change here -- it appears in
# fmt_col_attrs automatically. Order follows the new_rcrd() list() above; do NOT reorder.
fmt_field_names <- c("n", "display", "digits", "wn", "pct", "mean", "diff", "ratio", "ctr", "var",
                     "ci_inf", "ci_sup", "pvalue", "or", "tot_n", "n_eff", "obs", "gap_se",
                     "in_totrow", "in_tottab", "in_refrow")

# The per-column ATTRIBUTE names carried when a fmt column is rebuilt/round-tripped: every new_fmt()
# formal that is NOT a per-cell field (and not `...`/`class`). Order follows new_fmt()'s signature =
# type, comp_all, ref, ci_type, col_var, totcol, refcol, color, color_signif, model_family, role,
# conf_level.
# Read by fmt_unwrap / tab_stack_tables (tab.R), the column reconcile (tab_classes.R) and
# tab-test-display.R. `color` is carried WHOLE (length 1 or 2).
fmt_col_attrs <- setdiff(names(formals(new_fmt)), c(fmt_field_names, "...", "class"))





#' @keywords internal
fmt0 <- function(display = "n", digits = 0, type = "n") {
  new_fmt(n = 0L, display = display, digits = as.integer(digits), type = type)
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
    counts <- vctrs::field(x, "n") |> as.double()
    out[is.na(out)] <- counts[is.na(out)]
  }
  out
}

# as.list(vec_data(col)) with the `wn` field MATERIALISED -- the frame shape vec_cast produces via the
# getters. Raw vec_data() keeps wn's NAs, but get_wn() is the only getter with a fallback (NA -> the n
# field); every other field is a raw read, so only wn needs the fixup. Shared by tab_stack_tables()
# (tab_classes.R) and the test-display column stacker (tab-test-display.R).
#' @keywords internal
fmt_data_wn <- function(col) {
  fr <- as.list(vctrs::vec_data(col))
  fr$wn <- get_wn(col)
  fr
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
  else if (get_ci_type(x) == "ratio")                       get_ratio(x)# Phase 14b: Katz RR CI centred on the ratio
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
# stars_from_pvalue(): the star LADDER alone -- p-values to glyphs, per the two options. Split out
# (Last Phase z8) so a p-value with no fmt cell behind it (the aggregated interaction test's footer
# line) reads the same ladder as every cell instead of a second copy of the thresholds.
#' @keywords internal
stars_from_pvalue <- function(p) {
  brk <- sort(getOption("tabxplor.signif_levels", c(0.10, 0.05, 0.01)), decreasing = TRUE)
  lab <- getOption("tabxplor.signif_labels", c("*", "**", "***"))
  out <- c("", lab)[rowSums(outer(p, brk, `<`), na.rm = TRUE) + 1L]
  out[is.na(p)] <- ""
  out
}

# get_stars(): per-cell significance glyphs from the stored `pvalue` (universal CI-inclusion,
# so they always agree with the interval bracket). Thresholds/labels are options; "" where the
# pvalue is NA (cell CIs, non-diff cells, or stars opted out). See §20.
#' @keywords internal
get_stars  <- function(x, p = get_pvalue(x)) {
  out <- stars_from_pvalue(p)
  # Phase 17c: a footer cell (a "gof" stat, a "pvalue" test row, or a "blank" filler) now carries a real
  # `pvalue` field (honest storage), but it is NOT a "different from the reference" comparison, so it must
  # never print a star -- nor flip prep's has_stars / tab_xl's star padding. format() already excludes
  # these from the star APPEND; gating here makes every get_stars() caller agree at the source.
  out[display_primary(get_display(x)) %in% c("gof", "pvalue", "blank")] <- ""
  out
}
# Phase 16d: whether a column's stored pvalue drives significance STARS. A `contrib` column stores a
# standardized-residual (independence) pvalue purely to gate its OWN colouring -- it is NOT a
# "different from the reference category" test, so it must not print stars nor trigger the stars legend.
# It is the only measure whose pvalue is not a reference comparison; every other pvalue (a factor
# diff-test, a regression coefficient p) legitimately maps to stars.
#' @keywords internal
fmt_stars_applicable <- function(x) !identical(get_color(x)[1], "contrib")
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

# @describeIn fmt get the "n_eff" field (the effective sample size used for this cell's CI:
# Kish n_eff when opted in, else NA -> the CI falls back to the raw unweighted base)
#' @keywords internal
# @export
get_n_eff  <- fmt_field_factory("n_eff")

# @describeIn fmt get the "obs" field (Last Phase z5: the value this cell's estimate is COMPARED TO
# by the tab_reg colour measures -- the observed/crude effect, or the reference group's estimate)
#' @keywords internal
# @export
get_obs    <- fmt_field_factory("obs")

# @describeIn fmt get the "gap_se" field (Last Phase z8: the standard error of the gap between this
# cell's estimate and `obs`, on the estimate's own test scale -- NA wherever the gap has no test)
#' @keywords internal
# @export
get_gap_se <- fmt_field_factory("gap_se")

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
    fmt_broadcast_last(ctr, totrows)
  }
}

# fmt_resid() -- the ADJUSTED STANDARDIZED (Haberman) residual of each cell, DERIVED from the two
# fields that already store it: the two-sided p-value written by chi2_write_contrib() (magnitude) and
# the signed contribution (direction). Last Phase z4 chose this over a 20th fmt field because the
# p-value determines |z| exactly -- they are the same number in two coordinates -- and because it
# makes it impossible for the colour gate and the displayed residual to disagree.
# WARNING: it MUST be -qnorm(p/2), never qnorm(1 - p/2): `1 - p/2` is exactly 1 in double precision
# for any p < 2.2e-16, i.e. for every |z| > 8.2 -- routine in survey-sized tables -- which would
# saturate the whole tail to Inf. This form is exact down to p ~ 1e-300 (|z| ~ 37).
# Meaningful on a column that carries a chi2 contribution (`color = "contrib"` / `chi2 = TRUE`);
# elsewhere `ctr` is NA and the result is NA, so a stray `display = "{resid}"` blanks rather than lies.
#' @keywords internal
fmt_resid <- function(x) {
  sign(get_ctr(x)) * -stats::qnorm(get_pvalue(x) / 2)
}

# fmt_adjustment_score() -- Last Phase z5: how far a model estimate sits from the value it is COMPARED
# TO (the `obs` field: the observed/crude effect, or a reference group's estimate). ONE helper behind
# both `color = "adjustment"` and `color = "between_groups"`; they differ only in what `obs` holds.
#
# The comparison rides the estimate's own scale, read ONCE per column from `ci_type` (a scalar
# attribute, like get_num()'s est_ci arm) -- never a per-cell test:
#   multiplicative (or / ratio) : the ratio of the two effects, magnitude folded around 1;
#   additive       (diff)       : their difference, magnitude folded around 0.
#
# DESIGN -- the SIGN is "away from vs toward the NULL", not raw up/down. A raw sign colours a
# protective effect backwards: crude OR 0.50 attenuated to 0.60 moves UP while the identical
# attenuation of a risky 2.00 -> 1.67 moves DOWN, so the two halves of a diverging palette would mean
# nothing consistent. Scoring |log est| - |log obs| (|est| - |obs| when additive) makes one pole
# always "the model STRENGTHENED this effect" (suppression / negative confounding) and the other
# always "it ATTENUATED it" (the covariates explained part of the raw association), for protective and
# risky effects alike, and it stays correct through the null (crude 0.90 -> adjusted 1.20 reads as
# strengthened). The magnitude fed to findInterval is direction-free; only the sign carries the reading.
# fmt_gap_parts() -- Last Phase z8: the ONE decomposition of the estimate-vs-`obs` comparison, read
# once per column from `ci_type` and shared by the score, its interval and its p-value, so those three
# can never describe different quantities. Returns:
#   mult  the estimate is multiplicative (or / ratio) -> neutral 1, else additive -> neutral 0;
#   est / obs / ok  the two values and where both are usable;
#   sign  the NULL DIRECTION: +1 when the estimate is FURTHER from the null than `obs` (the model
#         strengthened the effect), -1 when nearer (it attenuated it), 0 when equal.

# fmt_est_field() / fmt_est_of() -- Last Phase z9: WHICH field holds a column's point estimate, keyed on
# its declared `ci_type`. ONE rule: an "or" column keeps its estimate in `or`, a "ratio" column in
# `ratio` (an Obs_rate column is ci_type "ratio"), everything else -- a beta, an AME, a risk difference,
# a logged OR/RR/IRR -- in `diff`. It was written out three times (here, reg_write_group_gap()'s local
# est_of, and the crude numeric overlay would have been a fourth), which is one encoding too many for a
# fact that decides where a number is READ and WRITTEN.
#' @keywords internal
fmt_est_field <- function(ci_type)
  switch(as.character(ci_type)[1], "or" = "or", "ratio" = "ratio", "diff")

#' @keywords internal
fmt_est_of <- function(x)
  switch(fmt_est_field(get_ci_type(x)), "or" = get_or(x), "ratio" = get_ratio(x), get_diff(x))

#' @keywords internal
fmt_gap_parts <- function(x) {
  cit  <- as.character(get_ci_type(x))[1]
  mult <- cit %in% c("or", "ratio")
  obs  <- get_obs(x)
  est  <- fmt_est_of(x)
  if (mult) {
    ok <- is.finite(est) & is.finite(obs) & est > 0 & obs > 0
    s  <- sign(abs(log(ifelse(ok, est, NA_real_))) - abs(log(ifelse(ok, obs, NA_real_))))
  } else {
    ok <- is.finite(est) & is.finite(obs)
    s  <- sign(abs(est) - abs(obs))
  }
  list(mult = mult, est = est, obs = obs, ok = ok, sign = s)
}

# fmt_adjustment_score() -- Last Phase z5: how far a model estimate sits from the value it is COMPARED
# TO (the `obs` field: the observed/crude effect, or a reference group's estimate). ONE helper behind
# both `color = "adjustment"` and `color = "between_groups"`; they differ only in what `obs` holds.
#
# The comparison rides the estimate's own scale, read ONCE per column from `ci_type` (a scalar
# attribute, like get_num()'s est_ci arm) -- never a per-cell test:
#   multiplicative (or / ratio) : the ratio of the two effects, magnitude folded around 1;
#   additive       (diff)       : their difference, magnitude folded around 0.
#
# DESIGN -- the SIGN is "away from vs toward the NULL", not raw up/down. A raw sign colours a
# protective effect backwards: crude OR 0.50 attenuated to 0.60 moves UP while the identical
# attenuation of a risky 2.00 -> 1.67 moves DOWN, so the two halves of a diverging palette would mean
# nothing consistent. Scoring |log est| - |log obs| (|est| - |obs| when additive) makes one pole
# always "the model STRENGTHENED this effect" (suppression / negative confounding) and the other
# always "it ATTENUATED it" (the covariates explained part of the raw association), for protective and
# risky effects alike, and it stays correct through the null (crude 0.90 -> adjusted 1.20 reads as
# strengthened). The magnitude fed to findInterval is direction-free; only the sign carries the reading.
#' @keywords internal
fmt_adjustment_score <- function(x) {
  p <- fmt_gap_parts(x)
  if (p$mult) {
    r   <- ifelse(p$ok, p$est / p$obs, NA_real_)
    mag <- pmax(r, 1 / r)                                    # size of the move, direction-free
    ifelse(p$sign < 0, 1 / mag, mag)                         # centre 1: below 1 = attenuated
  } else {
    ifelse(p$ok, abs(p$est - p$obs) * p$sign, NA_real_)      # centre 0: below 0 = attenuated
  }
}

# fmt_gap_raw() -- Last Phase z8: the SIGNED gap on the estimate's own TEST scale (the log-ratio when
# multiplicative, the plain difference when additive). This -- not the score -- is what `gap_se` is the
# standard error OF, so the two must be read from the same decomposition.
#' @keywords internal
fmt_gap_raw <- function(x) {
  p <- fmt_gap_parts(x)
  if (p$mult) ifelse(p$ok, log(p$est) - log(p$obs), NA_real_)
  else        ifelse(p$ok, p$est - p$obs           , NA_real_)
}

# fmt_gap_bounds() -- Last Phase z8: the confidence interval OF THE SCORE, so every existing branch of
# fmt_color_plan() works on it unchanged (it is the `bounds` fact of the two gap measures; every other
# measure keeps the stored ci_inf/ci_sup).
#
# DESIGN -- why the interval is re-signed rather than passed through raw. The score's sign is the NULL
# DIRECTION (away from / toward the null) while a raw gap interval is signed up/down; the two disagree
# for a protective effect. Handing the raw interval to the engine would then break BOTH policies:
# `grey_non_signif` matches the score's direction against sig_pos/sig_neg, and `guaranteed_effect`
# takes the bound nearest the neutral as the coloured magnitude. Folding the interval of |gap| back
# with the score's own sign makes both correct with no measure-specific branch:
#   * a gap interval excluding 0 puts BOTH bounds strictly on the score's side  -> significant, same
#     direction as the colour;
#   * one covering 0 pins the near bound exactly at the neutral                 -> not significant;
#   * the bound nearest the neutral IS the guaranteed gap ("moved by at least x1.1"), already signed.
# Last Phase z13 (D3): the level comes from the COLUMN (get_conf_level), not from the option. It used
# to read the option like every other threshold in the engine, which bit hardest here -- for this
# measure the whole interval is manufactured at print time, so nothing 99 %-wide was stored to fall
# back on, and a table built at conf_level = 0.99 printed 99 % intervals and stars while its gap
# greying silently stayed at 95 %.
#' @keywords internal
fmt_gap_bounds <- function(x) {
  p    <- fmt_gap_parts(x)
  se   <- get_gap_se(x)
  g    <- if (p$mult) ifelse(p$ok, log(p$est) - log(p$obs), NA_real_)
          else        ifelse(p$ok, p$est - p$obs           , NA_real_)
  ok   <- is.finite(g) & is.finite(se) & se > 0 & !is.na(p$sign)
  half <- zscore_formula(get_conf_level(x)) * se
  lo   <- ifelse(ok, p$sign * pmax(0, abs(g) - half), NA_real_)   # magnitude interval of |gap|,
  hi   <- ifelse(ok, p$sign * (abs(g) + half)       , NA_real_)   #   re-signed by the null direction
  if (p$mult) { lo <- exp(lo); hi <- exp(hi) }                    # centre 1 (exp is monotone)
  list(lo = pmin(lo, hi), hi = pmax(lo, hi))
}

# fmt_gap_p() -- the two-sided p of the gap (z on the test scale). Display only: the colour reads the
# interval above, so the two agree by construction. NA wherever `gap_se` is.
#' @keywords internal
fmt_gap_p <- function(x) {
  g  <- fmt_gap_raw(x)
  se <- get_gap_se(x)
  ifelse(is.finite(g) & is.finite(se) & se > 0, 2 * stats::pnorm(-abs(g / se)), NA_real_)
}

# fmt_gap_force_policy() -- Last Phase z8-B: the `force_policy` of BOTH gap measures (see MEASURES), as
# a PREDICATE ON THE COLUMN rather than a constant. A gap measure has a test exactly where tab_reg
# could write a `gap_se` for it, so "is there a test in this column?" is ONE read of the field the test
# itself produced -- no measure has to be told twice which paths are honest, and no engine has to guess
# an estimand from a rendered label or a display string.
#
# It covers, in one rule, every case where the gap has no interval: `adjustment` on a CONDITIONAL ODDS
# RATIO (maintainer ruling Q1(b): non-collapsible, so the test would read "significant" everywhere for
# a reason that is not confounding), on a model whose fitted object was distilled away (jamovi's digest
# path), on a model-comparison column fitted on different rows, and on any engine with no crude twin;
# plus `between_groups` under `method = "profile"`, whose asymmetric bounds are not est +/- crit*se so
# no SE can be recovered -- which without this read as `grey_non_signif` and greyed the WHOLE column
# (measured).
#
# Byte-identical wherever a `gap_se` exists: NULL leaves the column's own `color_signif` in place.
#' @keywords internal
fmt_gap_force_policy <- function(x) if (all(is.na(get_gap_se(x)))) "ignore" else NULL

# fmt_broadcast_last() -- base-R broadcast of the LAST value of each group to every row of that group,
# where each TRUE in `boundary` closes a group (the reference / total row sits at the group's end).
# Replaces the per-getter tibble + dplyr::with_groups(last()) idiom on the colour hot path with one
# base-R pass. Byte-identical to `nb = last(row_number())`: groups are contiguous, so a group's max
# row index is its last row.
#' @keywords internal
fmt_broadcast_last <- function(values, boundary) {
  gr <- cumsum(boundary) - boundary
  values[stats::ave(seq_along(values), gr, FUN = max)]
}

# get_ref_field() -- the reference cell's `getter` value, broadcast to every cell of its reference
# group (the total / marked reference row ends each subtable; under comp = "all" the total-table
# reference is broadcast to the whole column). The one helper behind the get_ref_means / get_ref_pct /
# get_ref_var mirror set (Phase 17d).
#' @keywords internal
get_ref_field <- function(x, getter) {
  refrows <- if (get_ref_type(x) == "tot") is_totrow(x) else is_refrow(x)
  values  <- getter(x)
  if (get_comp_all(x)) {
    refs <- refrows & is_tottab(x)
    if (!any(refs)) rep(NA_real_, length(x)) else rep(values[refs], length(x))
  } else {
    fmt_broadcast_last(values, refrows)
  }
}

#' @keywords internal
get_ref_means <- function(x) get_ref_field(x, get_mean)

#' @keywords internal
get_ref_pct <- function(x) get_ref_field(x, get_pct)

# Phase 5: the reference cell's VARIANCE, for Glass's delta = diff / sqrt(var_ref), the sd-standardized
# numeric diff-color scale (§18). NA/0 var_ref -> no color at the call site.
#' @keywords internal
get_ref_var <- function(x) get_ref_field(x, get_var)

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
  if (any(col_vars == "all_col_vars"))
    firstcol <- purrr::discard(firstcol, names(firstcol) == names(col_vars)[col_vars == "all_col_vars"])

  res <- purrr::map(1:ncol(tabs), function(.i)
    tidyr::replace_na(
      dplyr::last(names(firstcol[firstcol <= .i]) ),
      "")) |>
    rlang::syms() |>
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
  }) |>
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
    tidyr::replace_na(names(tot[tot >= .i])[1], "")) |>
    rlang::syms() |>
    purrr::set_names(names(tabs))




}



# Internal functions to modify class tabxplor_fmt

#' @keywords internal
fmt_set_field_factory <- function(.field, cast) {
  function(x, value) {
    value <- vctrs::vec_cast(value, cast) |> vctrs::vec_recycle(size = length(x))
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
# is best-effort (needs ci_type/type already set). See dev/tabxplor_2.0.0_decisions.md §1, §20.
# @describeIn fmt set the confidence-interval half-width (stored as symmetric absolute bounds)
#' @keywords internal
# @export
set_ci      <- function(x, value) {
  value <- vctrs::vec_cast(value, double()) |> vctrs::vec_recycle(size = length(x))
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
# @describeIn fmt set the "n_eff" field (the effective sample size used for this cell's CI)
#' @keywords internal
# @export
set_n_eff   <- fmt_set_field_factory("n_eff"  , cast = double()   )
# @describeIn fmt set the "obs" field (the value this cell's estimate is compared to -- written by
# tab_reg's crude-companion / split-group passes, NA everywhere else)
#' @keywords internal
# @export
set_obs     <- fmt_set_field_factory("obs"    , cast = double()   )
# @describeIn fmt set the "gap_se" field (the SE of the estimate-vs-`obs` gap, written by tab_reg
# where the two estimates are independent -- NA everywhere else)
#' @keywords internal
# @export
set_gap_se  <- fmt_set_field_factory("gap_se" , cast = double()   )







# METHODS FOR CLASS tabxplor_fmt #########################################################

#' @keywords internal
print_num <- function(num, digits) {
  sprintf(paste0("%-0.", digits, "f"), num) |>
    stringi::stri_replace_first_regex("^0.0+$|^-0.0+$", "0") |>
    stringi::stri_replace_first_regex("^100.0+$", "100")
}

# Format/printing methods for class tabxplor_fmt -----------------------------------------
#The first method for every class should almost always be a format() method.
#This should return a character vector the same length as x.

# Excel embeds LITERAL text in a numFmt code either as "text" OR by backslash-escaping each character
# (\t\e\x\t) -- both render identically. The quote form breaks the OLDER openxlsx2 bundled by jamovi
# (Windows-side): it does not XML-escape the " inside formatCode="...", so its own read_xml round-trip
# rejects the malformed fragment ("xml import unsuccessful", the Excel-export crash). Backslash escaping
# leaves no " in the attribute, so it is XML-safe on every openxlsx2 version. Every star / sigma / label /
# multiply-sign literal folded into a numFmt code MUST go through this.
# WARNING: never reintroduce a raw " into a numFmt formatCode. Vectorised; "" and NA pass through.
xl_numfmt_literal <- function(s) gsub("(.)", "\\\\\\1", s, perl = TRUE)

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
  # codes. `+0.0%;-0.0%` = positive shows "+", negative shows "-"; `\×#,##0.0` = "×2.0".
  # WARNING: the multiply sign is backslash-escaped (xl_numfmt_literal), NOT double-quote-wrapped -- a raw
  # " in a formatCode crashes the older jamovi-bundled openxlsx2 ("xml import unsuccessful").
  can <- !is.na(res) & res != "TEXT"
  s2  <- can & sgn
  res[s2] <- paste0("+", res[s2], ";-", res[s2])
  r2  <- can & rat
  res[r2] <- paste0(xl_numfmt_literal(mult_sign), res[r2])
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
#' @param pad The character used to align numbers: it pads values (composite displays, significance
#' stars, confidence intervals, a mean with no sd) **and separates thousands**. Defaults to a plain
#' space, or to a **figure space** (`U+2007`, exactly one digit wide) when `html = TRUE`. Media read in
#' a monospace font (the console, markdown) want the plain space; media rendered in a proportional font
#' (html, Excel) need the figure space, since an ASCII space is only half a digit wide there -- and CSS
#' collapses runs of them. One glyph for both jobs, so the thousands mark can never disagree with the
#' padding around it.
#' @param syntax `"text"` (default) returns the rendered display strings; `"excel"` returns the
#' per-cell Excel number-format codes used by [tab_xl()] (the raw value is written unchanged).
#' @param .ref Internal: precomputed reference masks `list(cells=, all_totals=)` (derive-once
#' speed-up passed by the exporter prep); computed internally when `NULL`.
#'
#' @return The fmt printed in a character vector.
#' @export
#' @keywords internal
format.tabxplor_fmt <- function(x, ..., html = FALSE, na = NA,
                                special_formatting = FALSE, stars = FALSE,
                                bold_split = FALSE, pad = if (isTRUE(html)) fig_space else " ",
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
  # Last Phase z4: a standardized residual on a pct column would inherit digits = 0 and print "+4"
  # for a 3.89; one decimal is the SPSS convention and enough to read the +/-2 / +/-3 rule.
  digits[!nas & display == "resid" & digits < 1L] <- 1L


  ok <- !na_out & !nas


  type    <- get_type(x)
  ci_type <- get_ci_type(x)

  pm <- stringi::stri_unescape_unicode("\\u00b1") # sign "plus minus"

  pct_or_ci     <- ok & display %in% c("pct", "pct_ci", "diff", "ci", "ctr") &
    !(display %in% c("ci", "diff") & type == "mean")
  pct_ci  <- ok & display == "pct_ci"
  mean_ci <- ok & display == "mean_ci"
  diff_mean <- ok & display == "diff" & type == "mean"

  # Phase 14b: the stored interval's SCALE, a scalar (ci_type is a column attribute). The additive
  # diff* scales are shown x100 with a "%"; the multiplicative "ratio" one is a bare ratio (neutral 1)
  # -- never x100, never clamped to [0;100], no "%". Same shape as a mean's absolute bounds, hence
  # `ci_bare`, which is what the branches below actually key on.
  ci_mult   <- ci_type %in% c("or", "ratio")
  ci_bare   <- (type == "mean") | ci_mult

  # Last Phase z5: `obs` prints exactly like the estimate it is compared to -- a crude OR reads like
  # the Model_OR beside it, a crude risk difference like its AME, a crude log(OR) like the raw
  # coefficient. The scale is a per-COLUMN fact (`ci_type` / `type` are scalar attributes), so this is
  # ONE branch per column, never a per-cell test: the same shape as get_num()'s est_ci arm.
  obs_m    <- ok & display == "obs"
  obs_mult <- ci_mult                          # OR / RR / IRR       -> like `or`  (bare, big.mark, 2 dg)
  obs_coef <- !ci_mult && type == "coef"       # beta / log(OR)      -> like `coef` (plain)
  obs_pct  <- !ci_mult && type != "coef"       # AME / risk-diff     -> like `diff` (x100, signed, %)
  if (obs_mult) digits[obs_m & digits < 2L] <- 2L
  obs_as_pct <- obs_m & obs_pct
  disp_ci   <- display == "ci" & ci_type %in% c("diff", "ratio") & !nas
  # A ratio interval on a pct column would inherit that column's digits = 0 and print "[1;2]".
  # 2 decimals, exactly like the `or` displays just above: a ratio bracket is the same kind of
  # quantity, and at 1 decimal the bounds routinely round equal -- which makes the block below
  # collapse the bracket to a bare point estimate ("0.6" for a real [0.55;0.63]).
  digits[!nas & display == "ci" & ci_type == "ratio" & digits < 2L] <- 2L
  plus_ci <- (pct_ci | mean_ci) # ci_pct_mean
  plus_disp_ci <- (plus_ci | disp_ci)
  # plus_ci <- (ci_pct_mean | disp_ci)# & !is.na(get_ci(x))

  #pct_or_pct_ci <- ok & display %in% c("pct", "pct_ci", "diff", "ctr")
  pct_no_ci     <- ok & display %in% c("pct", "diff", "ctr") & !(display == "diff" & type == "mean")
  pct_no_ci     <- pct_no_ci | obs_as_pct                     # Last Phase z5
  # Phase 14b: EVERY diff display is signed (see the sign block below). Means keep their own mask
  # only because their digits are bumped to >= 1 and they take no x100 / "%".
  # Last Phase z4: `resid` joins the signed mask -- the direction (over- / under-represented) is half
  # of what a standardized residual says, so it must never print bare.
  diff_signed   <- (ok & display %in% c("diff", "resid")) | obs_as_pct   # Last Phase z5
  n_wn          <- ok & (display %in% c("n", "wn", "mean", "mean_ci", "var", "ratio", "or", "or_pct",
                                        "OR", "OR_pct", "gof", "resid") |    # Phase 12f: gof -> big.mark
                           (display == "ci" & type == "mean") )
  n_wn          <- n_wn | (obs_m & obs_mult)                  # Last Phase z5
  type_ci       <- ok & display == "ci"
  pvalue        <- ok & display == "pvalue"

  pct_or_ci <- pct_or_ci | obs_as_pct                          # Last Phase z5 (x100 + "%")
  out[pct_or_ci] <- out[pct_or_ci] * 100
  digits[diff_mean] <- ifelse(digits[diff_mean] == 0, 1, digits[diff_mean])

  # Phase 10g: Excel number-format codes reuse format()'s OWN finalized masks (the x100 mask
  # pct_or_ci, the standalone-ci marker, the base_plus_ci TEXT mask) + adjusted digits, so the
  # tab_xl bypass can never drift from the console display. Return here, before any string building.
  if (syntax == "excel") {
    # pvalue is shown x100 with "%" by its own rendering path (not pct_or_ci), so add it to the
    # Excel "%" mask; a p-value shown as a "<0.01%" threshold still stores its raw value.
    excel_pct <- pct_or_ci | (!nas & display == "pvalue")
    # Phase 13c-v: diff + contrib get an explicit +/- sign; ratio gets a leading x.
    # Phase 14b: mean diffs join the `signed` mask. They were excluded only because their text display
    # was still the legacy multiply sign, which that display no longer is -- so excluding them now is
    # what WOULD desync the bypass from format()'s "+1.2".
    return(excel_numfmt_code(digits, pct = excel_pct,
                             ci = !nas & display == "ci", text = plus_ci,
                             signed = (!nas & display %in% c("ctr", "diff", "resid")) | obs_as_pct,
                             ratio  = !nas & display == "ratio"))
  }


  ci_print_moe <- getOption("tabxplor.ci_print") == "moe"
  if (any(plus_ci | disp_ci)) {
    if (any(plus_ci) & ci_print_moe) {
      # Phase 3a: the +/- moe shows the conservative LARGER arm (Wilson bounds are asymmetric).
      ci <- dplyr::if_else(condition = mean_ci[plus_ci],
                           true  = get_ci_moe(x)[plus_ci] ,
                           false = get_ci_moe(x)[plus_ci] * 100)

      ci_print_trim <- function(x) {
        x <- stringi::stri_replace_all_regex(x, paste0("^", pm, "0$|^", pm, "0.0+$|^", pm, "-0.0+$|^",
                                               pm, "NA"), "")
        stringi::stri_pad(x, max(stringi::stri_length(x)), pad = pad)
      }


      # ci_print_pad <- function(x) {
      #   stringi::stri_pad(x, max(stringi::stri_length(x)))
      # }

      out_ci <-
        paste0(print_num(out[plus_ci], digits[plus_ci]),
               dplyr::if_else(pct_ci[plus_ci], "%", ""),
               {
                 .ci <- ci_print_trim(paste0(pm, sprintf(
                   paste0("%-0.",
                          digits[plus_ci] + dplyr::if_else(pct_ci[plus_ci] & digits[plus_ci] == 0, 1L, 0L),
                          "f"), ci
                 )) )
                 # Phase 17a: was ci_html_subscript() -- a no-op except it blanks whitespace-only CI
                 # strings under html (subscript formatting is disabled; it broke in Jamovi).
                 if (html) dplyr::if_else(stringi::stri_detect_regex(.ci, "^ *$"), "", .ci) else .ci
               }
        )

    } else if (any(plus_disp_ci) ) { # !ci_print_moe
      # Phase 3a: read the real asymmetric bounds ci_inf/ci_sup directly (Wilson/Newcombe/AC/
      # Welch-t) instead of reconstructing a symmetric bracket from the half-width. This also
      # resolves the former WS2 mean-scaling FIXME -- the stored mean-diff bounds are absolute.
      # Phase 14b: `ci_bare` (a mean's absolute bounds, or a multiplicative ratio one) widens what was
      # `type == "mean"` -- a ratio interval is likewise shown as stored, not x100.
      lower <- dplyr::if_else(plus_disp_ci[plus_disp_ci] & ci_bare,
                              get_ci_inf(x)[plus_disp_ci],
                              get_ci_inf(x)[plus_disp_ci] * 100)
      upper <- dplyr::if_else(plus_disp_ci[plus_disp_ci] & ci_bare,
                              get_ci_sup(x)[plus_disp_ci],
                              get_ci_sup(x)[plus_disp_ci] * 100)

      # The estimate the bracket is centred on -- shown when the rounded bounds coincide. ci_center()
      # is the same dispatch the bounds were built on (diff / ratio / or), so it cannot disagree.
      ctr_for_ci <- ci_center(x)
      ref_for_ci <- dplyr::if_else(
        disp_ci[plus_disp_ci],
        true  = dplyr::if_else(plus_disp_ci[plus_disp_ci] & ci_bare,
                               true  =  ctr_for_ci[plus_disp_ci],
                               false =  ctr_for_ci[plus_disp_ci] * 100 ),
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
      out_ci <- paste0(out_ci, dplyr::if_else(plus_disp_ci[plus_disp_ci] & ci_bare, "", "%")) # pct_ci[plus_disp_ci]
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
  # Phase 14h: the thousands mark IS the pad glyph. It used to be hard-coded to an ASCII space, which
  # is only HALF a digit wide in a proportional font and which CSS collapses -- so "(n=1 811)" broke
  # the very alignment the figure-space padding around it had just bought. `pad` resolves per medium
  # (ASCII in the console/markdown, where it is already exactly one digit wide; fig_space in
  # html/Excel), so the mark can never again disagree with the padding it sits in.
  out[n_wn] <- out[n_wn] |> prettyNum(big.mark = pad, preserve.width = "individual")
  out[pct_no_ci] <- paste0(out[pct_no_ci], "%") #pillar::style_subtle()

  # Phase 13c-i: ratio (rr) display shows the multiplicative sign, so ratios read symmetrically (like
  # the legend and the OR display): a cell >= its reference prints "x2", a cell below prints "/2"
  # (the divide sign over 1/ratio). Default 1 digit (>= the column's digits), trailing zeros trimmed,
  # right-padded so the column aligns in a monospace font. Text syntax only (Excel returned early
  # above -> ratio stays a real number there, per the Phase 13c Excel decision).
  disp_rr <- ok & display == "ratio"   # canonical token ("rr" is aliased to it in display_primary)
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
      val[nn] <- stringi::stri_pad(val[nn], max(stringi::stri_length(val[nn])), side = "left",
                                  pad = pad)
    out[disp_rr] <- val
  }

  if (any(pvalue)) {
    p    <- get_pvalue(x[pvalue])                       # Phase 17c: honest p in the pvalue field

    out[pvalue]    <- paste0(
      ifelse(
        p < 0.0001,
        "<0.01",
        print_num(p * 100, digits = 2L)
      ),
      "%"
    )
  }

  # Phase 14b: an explicit "+" on every non-negative diff, means included. The numeric `diff` field
  # has been a real difference (cell_mean - ref_mean) since Phase 2, but its DISPLAY kept the legacy
  # multiply sign, so a mean diff read "x-0.2" -- a multiplicative glyph on an additive quantity,
  # which no reader could tell from the ratio. Means now render in the variable's own units
  # ("+1.2" / "-0.22"), exactly like a pct diff minus the "%". The sd-standardized (Glass's delta)
  # view the colour uses stays a COLOUR device: it is named by the legend and by the tooltip's
  # "std diff:" line, never by the cell -- so the number always equals $diff, and tab_xl (which
  # writes the raw field) cannot desync. `ratio` is what carries a multiply sign now (disp_rr above).
  out[diff_signed] <- ifelse(
    !startsWith(out[diff_signed], "-"),
    paste0("+", out[diff_signed]),
    out[diff_signed]
  )


 if (ci_print_moe) {
   out[type_ci] <- switch(
     type,
     "n"       = ,
     "coef"    = ,                                     # Phase 12c: coef CI moe is unit-scale (no %)
     "mean"    = paste0(pm, out[type_ci]),
     "row"     = ,
     "col"     = ,
     "all"     = ,
     "all_tabs"= paste0(pm, out[type_ci], "%") |> stringi::stri_replace_all_regex("%%", "%")
   )
 }



  # Phase 13c-ii/14h: per-cell bold-prefix widths. Written by TWO branches -- the mean/sd tail below
  # and the composite `{}` templates further down -- so it is allocated here, ahead of both. Attached
  # to the result only if one of them actually wrote (format() stays attribute-free otherwise).
  prim_nchar <- if (isTRUE(bold_split)) rep(NA_integer_, length(out)) else NULL

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
    # Phase 14h: a mean cell whose var is NA gets no "(sigma sd)" tail, so under the column's
    # right-align the whole cell slides right and its mean stops lining up with the others
    # ("1.0" against "1.7 (s2.1)"). Padded to the tail's width below.
    # WARNING: `!na_out` is load-bearing -- an EMPTY cell also has an NA var, and padding it would
    # paste onto the NA, turning it into the literal string "NA" + spaces. Only the `na` argument
    # (kable/md pass "") hid that; the console, which keeps NA, printed it.
    disp_mean_nosd <- display == "mean" & type == "mean" & !nas & !na_out & is.na(get_var(x))

    if (any (disp_mean_sd)) {
      sd <-
        print_num(get_num(set_display(set_var(x[disp_mean_sd],
                                              suppressWarnings(sqrt(get_var(x[disp_mean_sd]))) ), "var")),
                  digits = get_digits(x[disp_mean_sd])) # + 1L
      sd <- sd |>
        stringi::stri_pad(width = max(stringi::stri_length(sd)), side = "right", pad = pad)

      # Phase 14h: bold only the MEAN of a "mean (sigma sd)" cell in a bold row, exactly as a
      # composite "{pct} (n={n})" cell does -- bold glyphs are wider than plain ones, so a fully
      # bold cell stops aligning with the plain ones beside it. Recorded BEFORE the tail is pasted,
      # and the stars appended later ride outside the prefix, so this offset stays valid.
      if (isTRUE(bold_split)) prim_nchar[disp_mean_sd] <- nchar(out[disp_mean_sd])

      # Phase 14x: the mean <-> "(sigma sd)" joiner is the medium `pad`, not a narrow no-break space
      # (U+202F). `pad` is a plain ASCII space in the monospace media (console, markdown -- one digit
      # wide there, and no exotic glyph to confuse a raw-file editor / copy-paste) and a FIGURE SPACE
      # (U+2007, exactly one digit wide) in html, so the number and the "(sd)" tail keep the same
      # digit-grid gap as the rest of the row instead of a tighter, off-grid one.
      out[disp_mean_sd] <- paste0(out[disp_mean_sd], pad, "(", sigma_sign, sd, ")")

      # WARNING: this pads by CHARACTER COUNT, which is exact only in a monospace medium (console,
      # markdown). In html/Excel it lands within about one digit-width, because "(", sigma and ")"
      # are not digit-wide -- no run of spaces can match them exactly there. An exact fix needs
      # markup (a hidden tail), not padding; that belongs to the html engine, not to format().
      if (any(disp_mean_nosd)) {
        tail_w <- nchar(pad) + nchar(sigma_sign) + 2L + max(stringi::stri_length(sd))
        if (isTRUE(bold_split)) prim_nchar[disp_mean_nosd] <- nchar(out[disp_mean_nosd])
        out[disp_mean_nosd] <- paste0(out[disp_mean_nosd], strrep(pad, tail_w))
      }
    }


    if (any(disp_diff)) {
      if (is.null(ref_cells)) ref_cells <- get_reference(x, "cells")
      ref     <- ref_cells[disp_diff]
      reffmt  <- set_display(x[disp_diff],
                             ifelse(type %in% c("n", "mean"), "mean", "pct")) |>
        format() #|> stringi::stri_trim()
      out[disp_diff] <- ifelse(ref,
                               paste0("ref:", reffmt),
                               out[disp_diff])
    }

    if (any(disp_moe)) {
      if (is.null(ref_cells)) ref_cells <- get_reference(x, "cells")
      ref     <- ref_cells[disp_moe]
      reffmt  <- set_display(x[disp_moe],
                             ifelse(type %in% c("n", "mean"), "mean", "pct")) |>
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
      out[mctr] <- paste0("mean:", stringi::stri_trim(out[mctr])) |>
        stringi::stri_replace_first_regex("mean:Inf%|NA", "")
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
      one         <- stringi::stri_replace_first_regex(vals, "1\\.0+", "1")

      if (any(!is.na(get_pct(x)[disp_or]))) {                # empirical-OR crosstab: annotate ref %
        reffmt <- set_display(x[disp_or], "pct") |> set_digits(0L) |> format()
        reffmt <- suppressWarnings(
          stringi::stri_pad(reffmt, suppressWarnings(max(stringi::stri_length(reffmt), na.rm = TRUE)),
                           pad = pad)
        )
        # z10: `!is.na(or_val)` -- a reference cell with NO odds ratio must not claim "1". Byte-identical
        # for every OR flavour whose reference cell is 1 by construction; it is the cumulative OR's
        # degenerate last cut ("at or below the top level" is certain) that has a reference row and no
        # ratio, and would otherwise print the raw "NA" beside the reference percentage.
        out[disp_or] <- ifelse(refer & !is.na(or_val) & !is.na(reffmt),
                               paste0(one, " (", reffmt, ")"), vals)
      } else {                                               # pure model-OR: bare "1" on ref rows
        out[disp_or] <- ifelse(refer & !is.na(or_val), one, vals)
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
  # Phase 14m-ii (L5): a footer SUMMARY cell (a "gof" stat -- N/AIC/R2 -- or a "pvalue" row) never
  # carries a per-cell star, so it does NOT reserve the star column: a right-aligned summary number then
  # reaches the column edge (the data cells' stars hang into the width beside it) rather than being
  # indented to line up under the starred data. get_stars() is "" for these anyway, so dropping them
  # leaves the width `w` unchanged -- the only effect is that they take no trailing pad.
  if (isTRUE(stars) && fmt_stars_applicable(x)) {
    st  <- get_stars(x)
    val <- !is.na(out) & nzchar(out) & !(display %in% c("gof", "pvalue"))
    if (any(val & nzchar(st))) {
      w  <- max(nchar(st[val]))
      st_pad <- stringi::stri_pad(st, w, side = "right", pad = pad)  # glyphs left, pad right
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
  if (any(composite)) {
    for (tmpl in unique(raw_display[composite])) {
      seg   <- parse_display_template(tmpl)
      if (!any(seg$is_tok)) next
      cells <- which(composite & raw_display == tmpl)
      xc    <- x[cells]
      toks  <- lapply(seq_along(seg$fields), function(i) {
        # Stars ride the primary token, so the others have their p-value blanked. EXCEPT `resid`
        # (Last Phase z4), which is DERIVED from the p-value -- blanking it would render NA and drop
        # the whole composite. It cannot draw a star anyway: format() gates stars on the `stars` flag
        # (primary-only, below) and on fmt_stars_applicable(), which excludes contrib columns.
        xi <- if (i == 1L || identical(seg$fields[i], "resid")) xc else set_pvalue(xc, NA_real_)
        format(set_display(xi, seg$fields[i]), na = na, special_formatting = FALSE,
               stars = isTRUE(stars) && i == 1L, pad = pad)   # the inner tokens pad too
      })
      # Phase 13c-i: align each {field} to a uniform width within the column so numbers line up in a
      # monospace font (e.g. "100% (n=  849)" / "100% (n=3 648)"). Right-aligned (left-pad) over the
      # non-NA cells; the literal pieces are constant, so only the {tokens} are padded.
      toks <- lapply(toks, function(s) {
        keep <- !is.na(s)
        if (any(keep))
          s[keep] <- stringi::stri_pad(s[keep], max(stringi::stri_length(s[keep])), side = "left",
                                     pad = pad)
        s
      })
      strs <- vector("list", length(seg$pieces)); ti <- 0L
      for (j in seq_along(seg$pieces)) {
        if (seg$is_tok[j]) { ti <- ti + 1L; strs[[j]] <- toks[[ti]] }
        else {
          # Phase g (A6): in a non-breaking medium (pad != " ": html / md-with-css), the ASCII spaces
          # in a template literal like " (n=" must not break -- else "100% (n=16 382)" wraps to two
          # lines (the inner "16 382" already holds, its big.mark being the figure space). U+00A0 keeps
          # a normal-width join. Console (pad = " ") is byte-identical.
          piece <- seg$pieces[j]
          if (!identical(pad, " ")) piece <- gsub(" ", "\u00a0", piece, fixed = TRUE)
          strs[[j]] <- rep(piece, length(cells))
        }
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

  # Phase 13c-ii: expose the per-cell bold-prefix width of composite and mean/sd cells (NA elsewhere)
  # so exporters can bold only the primary field in bold rows. Dropped silently by any downstream
  # string op, so consumers must read it right after format() (see md_render_one / render_*_engine).
  # `any(!is.na())`: with nothing to split the result stays attribute-free, as before Phase 14h.
  if (!is.null(prim_nchar) && any(!is.na(prim_nchar))) attr(out, "primary_nchar") <- prim_nchar

  #out <- stringi::stri_pad(out, max(stringi::stri_length(out), na.rm = TRUE))
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
#' @keywords internal
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
  # Phase 16f: bold reference/total (+ coloured) cells, but ONLY on a console that renders ANSI bold at
  # fixed glyph width (tabxplor.console_bold, IDE-gated at load -- Positron / VS Code, never RStudio). Off
  # everywhere by default, so this is a no-op unless opted in. Read fresh so a mid-session toggle applies.
  bold_on <- isTRUE(getOption("tabxplor.console_bold"))

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
    channels    <- fmt_color_channels(x)
    text_styles <- get_color_style()                  # current type/theme/24-bit options (ANSI, cli)
    bg_styles   <- get_color_style(type = "bg")

    for (s in sort(unique(channels$text_slot[channels$text_slot > 0L & ok]))) {
      cells <- ok & channels$text_slot == s
      out[cells] <- text_styles[[s]](out[cells])
    }
    for (s in sort(unique(channels$bg_slot[channels$bg_slot > 0L & ok]))) {
      cells <- ok & channels$bg_slot == s
      out[cells] <- bg_styles[[s]](out[cells])
    }
    totals <- if (!is.null(.ref)) .ref$all_totals else get_reference(x, "all_totals") #c("cells","lines")
    # Phase 14q: a reference ROW is also an anchor. A regression EMPIRICAL column carries
    # ref_type = "tot" yet marks its reference CATEGORY via in_refrow, which get_reference("all_totals")
    # misses -- so its reference cells were greyed in the console too. For crosstabs is_refrow is a
    # subset of `totals`, so this is a no-op there.
    totals <- totals | is_refrow(x)

    # Cells matching no break on EITHER channel are greyed (style_subtle) so colored cells stand
    # out; reference/total cells are exempt, staying full-strength as reading anchors.
    unselected <- channels$text_slot == 0L & channels$bg_slot == 0L
    out[ok & unselected & !totals] <-  #fmtgrey3
      pillar::style_subtle(out[ok & unselected & !totals])

    # Phase 16f: export-parity bold = the anchors (totals) PLUS the text-coloured cells, matching
    # fmt_col_ann()'s `bold = !is.na(text_hex) | keep_black` (R/tab-export-prep.R). pillar measures the
    # ANSI-stripped width, so bold adds none -- alignment holds on a fixed-width-bold console.
    if (bold_on) {
      m <- ok & (totals | channels$text_slot > 0L)
      out[m] <- cli::style_bold(out[m])
    }

    #Columns with no color
  } else {
    # DESIGN: uncolored columns only grey out zeros here. Styling totals with bold /
    # underline / borders was tried (commented below) but rejected: bold offsets column
    # widths in the console unaesthetically. The underline+"|" was the border-imitation try.
    # - use underline and | to make the imitate the borders of a table
    # if (any(totrows)) out <- dplyr::if_else(totrows & ! totcol,
    #                                         cli::style_underline(out), out)
    # if (totcol)       out <- dplyr::if_else(totrows,
    #                                         paste0(cli::style_underline(out), "|"),
    #                                         paste0(out, "|"))

    # # - normal cells a bit grayer to see the totals better
    # totals <- get_reference(x, mode = "all_totals")
    # out[ok & !totals] <- fmtgrey4(out[ok & !totals])

    out[ok] <- out[ok] |>
      stringi::stri_replace_first_regex("^0%$|^-0%$", pillar::style_subtle("0%")) |> # 0 in gray
      stringi::stri_replace_first_regex("^0$|^0$", pillar::style_subtle("0"))

    # Phase 16f: an uncolored column (e.g. the Total column, or a plain table) has no text-coloured
    # cells, so only the reference/total anchors are bold -- the same `totals` mask the coloured branch uses.
    if (bold_on) {
      tot <- (if (!is.null(.ref)) .ref$all_totals else get_reference(x, "all_totals")) | is_refrow(x)
      out[ok & tot] <- cli::style_bold(out[ok & tot])
    }
  }

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
#' @keywords internal
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
#' @keywords internal
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
#                                  ansi/hex. (bg wired at Step 4.)
# Significance gates read the Phase-3a ci_inf/ci_sup bounds. See dev/new_colors_UI.md §8-9.
# Byte-identity gate: factor "diff" (incl. the x2), "contrib", "OR", and the mean CI-gated modes
# are reproduced exactly; numeric "diff" (Glass's delta) and the pct CI-gated modes change
# consciously (asymmetric-interval fix). Locked by test-color-golden.R.
# ============================================================================================

# The canonical break scales for a column (per-table override folded in at Step 4).
#' @keywords internal
color_scales <- function(x) {
  sc <- getOption("tabxplor.color_breaks")
  if (is.null(sc) || is.null(sc$pct_diff)) return(default_color_scales())
  # Last Phase z13: fill in scales a STALE option list predates. The option is a snapshot -- a session
  # that saved it (or an .Rprofile that sets it) before a scale existed would otherwise hand the engine
  # a NULL scale, i.e. a silently uncoloured column. Byte-identical when the list is complete, since
  # set_color_breaks() always starts from default_color_scales().
  utils::modifyList(default_color_scales(), sc)
}

# Phase 14a: shift ONE per-direction break scale so its first break sits at the neutral value, for
# the `guaranteed_effect` policy (the why is at the call site in fmt_color_plan). `breaks` are
# POSITIVE magnitudes -- fmt_color_slots() folds each side around the centre -- so the neutral
# magnitude is 0 on an additive scale and 1 on a multiplicative one. Empty side (that measure is off
# for this column type) -> unchanged.
#   additive       c(0.05, 0.10, 0.20, 0.30) -> c(0, 0.05, 0.15, 0.25)
#   multiplicative c(1.15, 1.5,  2,    4   ) -> c(1, 1.30, 1.74, 3.48)
# Last Phase z4: `origin` re-anchors an ADDITIVE offset scale somewhere other than 0. The default
# (NULL = 0) is the CI-floor reading above. `color = "contrib"` passes the significance threshold
# z(conf_level): its guaranteed reading scores the ABSOLUTE standardized residual, so the first colour
# step must sit at the threshold itself and the breaks stay real |z| values a reader can name
# ("this is a +-3 cell") -- the legend prints these same numbers, so it cannot say "+0.62".
#   c(1.96, 2.58, 3.89, 6) with origin 1.96 -> unchanged ; at conf 0.99 -> c(2.58, 3.20, 4.51, 6.62)
#' @keywords internal
offset_guaranteed_breaks <- function(breaks, center, origin = NULL) {
  if (length(breaks) == 0L || is.na(breaks[1])) return(breaks)
  if (identical(center, 1)) return(breaks / breaks[1])
  breaks - breaks[1] + if (is.null(origin)) 0 else origin
}

# Phase g: the additive (center-0) break scale for a NON-gaussian regression coefficient
# (exponentiate = FALSE), derived by LOGGING the odds_ratio scale and ROUNDING to 1 decimal --
# log(OR breaks c(1.2, 1.5, 2, 4)) = c(0.18, 0.41, 0.69, 1.39) -> c(0.2, 0.4, 0.7, 1.4). So a
# log-odds/log-rate coefficient reads ~the same colour intensity as its exponentiated OR/IRR twin
# (readable breaks, not 4-decimal logs), and follows any user change to `odds_ratio`. `std = FALSE`, so
# fmt_color_plan's SD-division block skips it (no var(Y) on the link scale). Empty/dropped odds_ratio
# scale -> empty (uncoloured), the same graceful fallback as any missing scale.
#' @keywords internal
log_odds_scale <- function(or_scale) {
  log_side <- function(side) list(breaks = round(log(side$breaks), 1L), slots = side$slots)
  list(center = 0, strict = TRUE, std = FALSE,
       over = log_side(or_scale$over), under = log_side(or_scale$under))
}

# Last Phase z13 (D2): WHICH break scale a gap measure (`adjustment` / `between_groups`) reads. z5 chose
# between two scales on `ci_type` alone, so a beta on an outcome recorded in hours, minutes or days read
# three different ways -- in minutes every cell saturated at the deepest break, in days the feature was
# entirely dark. The gap of an effect belongs on the same KIND of ladder as the effect itself, which is
# the dispatch `diff` already performs; this states it for the adj_* ladders.
#
# The ORDER is the contract. A poisson count AME and a raw poisson coefficient are indistinguishable in
# (type, ci_type, model_family) -- both ("coef", "diff", "poisson") -- and `is_logcoef` claims both. What
# separates them is `var`: var(Y) is written exactly on the columns whose estimate lives in the
# OUTCOME's own units (reg_column's additive arm and reg_marginal_column's raw arm, both gated on a
# non-probability scale), which is also the SD the standardization needs. So the var test must precede
# the log-coefficient one, and that one must precede the percentage-point default.
#' @keywords internal
fmt_gap_scale_key <- function(x, type, ci_mult, is_logcoef) {
  if (isTRUE(ci_mult))                                     return("adj_ratio")      # OR / RR / IRR
  if (identical(type, "coef") && !all(is.na(get_var(x))))   return("adj_diff_std")  # beta, count AME
  if (isTRUE(is_logcoef))                                  return("adj_diff_log")   # log(OR) coef
  "adj_diff"                                                                        # probability points
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

  # Phase 17d (Step 4d complete): the stored `color` attribute is now a CLEAN measure -- the legacy
  # combined strings (diff_ci/after_ci/ci) are decoded ONCE at the argument / storage boundary
  # (color_decode_legacy, R/tab.R), so the engine never re-parses them. `or`/`OR` is the only surviving
  # synonym. A non-measure token (e.g. a hand-built column) -> uncoloured.
  measure <- if (color[1] %in% c("or", "OR")) "or" else color[1]
  if (!measure %in% names(MEASURES)) return(NULL)
  # policy: an explicit `signif` arg wins; else the stored per-column color_signif attribute. Last Phase
  # z5: measure_policy() then applies a measure's `force_policy` (a measure with no significance test of
  # its own always reads under `ignore`); the legend resolves the policy through the same accessor.
  # Last Phase z8-B: `force_policy` may be a predicate on the COLUMN (a gap measure has a test exactly
  # where a `gap_se` was written), so the column is passed in. The legend reads `plan$policy` off the
  # plan this returns, so it inherits the resolution rather than repeating it.
  policy  <- measure_policy(measure, if (!is.null(signif)) signif else get_color_signif(x), x)

  is_mean <- type %in% c("mean", "n")
  # Phase 12c: a "coef" column (gaussian regression beta) colours the STANDARDIZED effect beta/SD(Y)
  # against the mean_diff (Cohen 0.2/0.5/0.8/1.2) breaks -- the additive twin of OR-by-ratio. It uses
  # the mean_diff scale like a mean-diff, but standardizes by its OWN `var` field (= var(Y), constant),
  # NOT by get_ref_var() (whose refrow-at-END grouping is meaningless for a regression skeleton).
  is_std_diff <- is_mean || type == "coef"
  # Phase g: a NON-gaussian coefficient (exponentiate = FALSE: a log-odds / log-rate / cumulative-logit
  # coefficient) has no var(Y) to standardize by (it lived on the LINK scale, so SD(Y) is undefined) --
  # the pre-g code fed sqrt(NA) and greyed every cell out. Instead it colours on the LOG of the
  # odds_ratio scale (center 0, no SD-division), so a coefficient of log(2) reads the same intensity as
  # an OR of 2 -- the exponentiate=TRUE twin. Derived from odds_ratio so the two always agree. A
  # gaussian beta keeps its own SD-standardized mean_diff scale (var(Y) is meaningful there).
  # Last Phase z3: the family list is reg_fam_logscale() (R/tab_reg.R) -- ONE predicate shared with the
  # legend's twin gate below, which used to repeat this vector verbatim and be kept in sync by comment.
  is_logcoef <- type == "coef" && reg_fam_logscale(get_model_family(x))
  # Phase 17d: the measure's engine facts (scale keys, raw getter, sig source, row gate) live in ONE
  # MEASURES row alongside its legend facts -- fmt_color_plan reads them instead of four switch arms
  # kept in sync by hand. `std_when` picks the std vs pct scale key per column kind (see MEASURES).
  # Last Phase z4: read through measure_facts(), which folds in a measure's per-policy override (only
  # contrib has one: `guaranteed_effect` swaps the relative contribution for the absolute residual).
  md      <- measure_facts(measure, policy)
  # the stored interval's SCALE (a scalar column attribute); also the significance neutral below.
  cit     <- get_ci_type(x)
  ci_mult <- cit %in% c("or", "ratio")
  # Last Phase z5: "additive" keys the scale on the ESTIMATE's own scale rather than on the column kind
  # -- Model_OR and Model_AME are both type "row", so only ci_type separates a multiplicative effect
  # from an additive one.
  use_std <- switch(md$std_when, "std_diff" = is_std_diff, "mean" = is_mean, "additive" = !ci_mult,
                    "na" = TRUE)
  sc      <- color_scales(x)
  # Last Phase z13: the selected scale as a KEY, kept on the plan -- the legend then takes its glyphs
  # and its unit from the scale actually used (D4) instead of from a static measure field. The two gap
  # measures dispatch on the ESTIMATE's own scale (D2, fmt_gap_scale_key); `diff` keeps its one runtime
  # swap (a log-scale coefficient reads the logged OR ladder, so it matches its exponentiated twin).
  scale_key <- if (identical(md$std_when, "additive")) fmt_gap_scale_key(x, type, ci_mult, is_logcoef)
               else if (is_logcoef && measure == "diff") "log_odds"
               else md$scale[[if (use_std) "std" else "pct"]]
  scale   <- switch(scale_key,
                    "log_odds"     = log_odds_scale(sc[["odds_ratio"]]),
                    # the gap of a log-scale coefficient is the LOG of the gap of its exponentiated
                    # twin, so the same helper derives its ladder from adj_ratio -- one calibration,
                    # both readings, and a user's set_color_breaks(adj_ratio =) reaches both.
                    "adj_diff_log" = log_odds_scale(sc[["adj_ratio"]]),
                    sc[[scale_key]])
  md      <- measure_facts(measure, policy, scale_key)   # re-resolve with the per-scale override
  center <- if (is.null(scale)) 0 else scale$center
  strict <- if (is.null(scale)) TRUE else scale$strict

  # observed per-cell quantity
  raw <- md$raw(x)

  # Standardized by SD(Y) when the SCALE says so -- Glass's delta for a numeric `diff`, and (z13) the
  # additive gap of an arbitrary-unit outcome. The gate used to name the measure and re-derive the
  # column kind; `mean_diff` was the only pre-z13 scale with std = TRUE and `diff` the only measure
  # naming it, with use_std == is_std_diff, so the two dropped conjuncts were implied.
  sd_ref <- NULL
  if (isTRUE(scale$std)) {
    sd_ref      <- if (type == "coef") sqrt(get_var(x)) else sqrt(get_ref_var(x))
    raw         <- raw / sd_ref
    raw[!is.finite(raw)] <- NA_real_        # sd_ref 0/NA -> undefined -> uncolored
  }

  # Significance from the measure's interval. Phase 14b: it is a property of the INTERVAL, not of the
  # measure being coloured -- an interval is significant when it excludes ITS OWN neutral (0 for the
  # additive diff* scales, 1 for the multiplicative "or"/"ratio" ones). This was keyed on the measure,
  # which held only while each measure had exactly one possible ci_type; now that `ratio` can own the
  # stored interval (and a `diff` channel derive from it), the two must be read apart. It also fixes
  # the old mismatch: measure "or" + a difference ci_type tested the diff bounds against the OR's
  # neutral 1, so nothing was ever significant. All three scales test the same null (p1 = p2), so
  # whichever interval is stored answers it.
  # Last Phase z8: WHICH interval is a declared measure fact (`bounds`, defaulted by measure_facts to
  # the stored ci_inf/ci_sup). The gap measures derive theirs from `gap_se`, already folded onto the
  # score's own scale and sign -- so this block, the floor block below and the direction match in
  # `grey_non_signif` all keep working with no measure-specific branch.
  bd          <- md$bounds(x)
  has_ci      <- cit %in% c("diff", "diff_row", "diff_col", "or", "ratio")
  ci_neutral  <- if (ci_mult) 1 else 0
  sig_pos <- has_ci & bd$lo > ci_neutral
  sig_neg <- has_ci & bd$hi < ci_neutral
  sig_pos[is.na(sig_pos)] <- FALSE
  sig_neg[is.na(sig_neg)] <- FALSE

  # Last Phase a bug-fix: `contrib` carries NO confidence interval (ci_type == ""), so its significance
  # is read from the stored standardized-residual p-value (written by chi2_write_contrib) rather than
  # the bounds; direction from the sign of the signed contribution `raw` (ctr/mean_contrib). Previously
  # both signif policies gated on the absent bounds and coloured NOTHING. (The residual p-value is the
  # one place colour reads `pvalue` -- justified because contrib has no interval.)
  if (md$sig_source == "pvalue") {
    alpha   <- 1 - get_conf_level(x)
    pv      <- get_pvalue(x)
    ctr_sig <- !is_totrow(x) & !is.na(pv) & pv < alpha
    sig_pos <- ctr_sig & raw > 0; sig_pos[is.na(sig_pos)] <- FALSE
    sig_neg <- ctr_sig & raw < 0; sig_neg[is.na(sig_neg)] <- FALSE
  }

  if (policy == "guaranteed_effect") {
    if (md$sig_source == "pvalue") {
      # Last Phase z4: no interval to take a CI-floor of, so this policy carries contrib's OTHER
      # reading instead -- the ADJUSTED STANDARDIZED RESIDUAL on the absolute `zscore` scale (the
      # scale swap is the MEASURES `guar` override, applied by measure_facts() above). The score is the
      # residual itself, in real |z| units, and the break scale is re-anchored to the significance
      # threshold (break_origin below), so the policy's invariant holds by construction -- a cell is
      # coloured iff |z| > z(conf_level) -- while the thresholds stay numbers a reader can name.
      # fmt_resid()'s sign is sign(ctr) = the sign of `raw`, so direction matches the two other policies.
      score <- fmt_resid(x)
      gate  <- sig_pos | sig_neg
    } else {
    # The GUARANTEED (CI-floor) magnitude, on the MEASURE'S OWN scale so fmt_color_slots() folds it
    # around the right centre. The stored bounds may be on ANOTHER scale: only one interval is stored
    # per column (the primary/text measure's), and the second channel derives from it.
    floor_q <- dplyr::case_when(sig_pos ~ bd$lo,
                                sig_neg ~ bd$hi,
                                TRUE    ~ NA_real_)
    # `diff` and `ratio` are two views of ONE cell-vs-reference comparison: both are affine in the
    # cell proportion with the reference held at its point estimate (ratio - 1 = diff / p_ref). So a
    # bound on either maps exactly onto the other by one ratio of offsets from their neutrals -- no
    # new field, and the diff -> ratio direction is byte-identical to the expression that was here.
    # Without it the raw diff bound (~0.05) was folded around the ratio's centre 1 -> 1/0.05 ->
    # strongest UNDER colour on every significant cell, regardless of direction.
    rescale_bound <- function(q, pt_from, nt_from, pt_to, nt_to)
      nt_to + (pt_to - nt_to) * (q - nt_from) / (pt_from - nt_from)
    # The scrub stays scoped to the conversions (a 0/0 there gives NaN); leaving it off the
    # unconverted measures is what keeps every existing colour byte-identical.
    ci_is_ratio <- identical(cit, "ratio")
    if (measure == "ratio" && !ci_is_ratio) {          # diff bound -> ratio bound
      floor_q <- rescale_bound(floor_q, get_diff(x),  0, get_ratio(x), 1)
      floor_q[!is.finite(floor_q)] <- NA_real_
    } else if (measure == "diff" && ci_is_ratio) {     # ratio bound -> diff bound (the mirror)
      floor_q <- rescale_bound(floor_q, get_ratio(x), 1, get_diff(x),  0)
      floor_q[!is.finite(floor_q)] <- NA_real_
    }
    # A mean/coef never carries a ratio CI (Katz is proportions-only), so this cannot combine above.
    if (isTRUE(scale$std)) {
      floor_q <- floor_q / sd_ref
    }
    score <- floor_q
    gate  <- !is.na(floor_q)
    }
  } else if (policy == "grey_non_signif") {
    score <- raw
    dir0  <- if (center == 1) dplyr::case_when(raw > 1 ~ 1L, raw < 1 ~ -1L, TRUE ~ 0L)
             else sign(raw)
    gate  <- (dir0 > 0L & sig_pos) | (dir0 < 0L & sig_neg)
    gate[is.na(gate)] <- FALSE
  } else {                                   # ignore
    score <- raw
    gate  <- !is.na(raw)
    if (md$gate_row == "totrow") gate <- gate & !is_totrow(x)
  }

  # Phase 12c: a reference row is a baseline, not an effect -> never coloured. Redundant for
  # crosstabs (a reference cell's diff is 0 / OR is 1, already slot 0), it uncolours a regression
  # INTERCEPT (in_refrow but a non-neutral baseline value) under every policy.
  if (md$gate_row == "refrow") gate <- gate & !is_refrow(x)

  # Per-direction breaks + palette slots (Phase 13a). Each side of the scale carries its own
  # magnitudes (over$breaks / under$breaks) and intensities 1:4 (over$slots / under$slots). The
  # engine folds every cell to a magnitude >= the neutral, findInterval() against the side's breaks,
  # and reads the intensity: over -> slots 1:4, under -> slots 5:8 (the two halves of the 8-colour
  # palette). The former in-text "x2 rule" is gone -- it is now just a 1-break ratio scale carried on
  # the dedicated background channel (color = c("diff", "ratio")).
  over_breaks  <- scale$over$breaks
  under_breaks <- scale$under$breaks
  over_slots   <- c(0L, scale$over$slots)         # 0 = neutral level, then intensities 1:4
  under_slots  <- c(0L, scale$under$slots + 4L)   # 0 = neutral, then 5:8 (under half of the palette)

  # Phase 14a: under `guaranteed_effect` the score is the CI FLOOR -- the effect you are confident
  # of AT LEAST -- so the scale must START at the neutral value: a cell whose interval excludes the
  # neutral IS a guaranteed effect and must be coloured. Scoring the floor against the ordinary
  # magnitude breaks left every significant-but-modest cell grey (diff = +7% with ci_inf = +0.4%
  # scored 0.004 < the 0.05 first break -> uncoloured, though "0 is outside the interval" is exactly
  # what the mode exists to show). Offset each side by its OWN first break -- the sides are
  # independent and may be asymmetric (Phase 13a). The user cannot express this (0 / 1 are rejected
  # as breaks), so it must be internal. `legend_specs()` reads this same plan, so the legend follows.
  if (identical(policy, "guaranteed_effect")) {
    # Last Phase z4: `break_origin` is a declared measure fact (only contrib's `guar` sets it) --
    # "threshold" re-anchors the offset at z(conf_level) instead of 0, because that reading scores an
    # absolute standardized residual rather than a CI floor. See offset_guaranteed_breaks().
    org <- if (identical(md$break_origin, "threshold")) {
      zscore_formula(get_conf_level(x))
    } else NULL
    over_breaks  <- offset_guaranteed_breaks(over_breaks,  center, org)
    under_breaks <- offset_guaranteed_breaks(under_breaks, center, org)
  }

  # Phase 16c: a `guaranteed_effect` channel whose scale has a single break per side collapses under
  # offset_guaranteed_breaks() to the neutral value -> findInterval() paints EVERY significant cell with
  # one flat intensity (a gradient-less "x1" fill, redundant with the text channel). Flag it so the
  # cross-channel arbiter (resolve_color_channel_plans) can disable it -- unless it is the only channel.
  degenerate <- identical(policy, "guaranteed_effect") && !is.null(scale) &&
    max(length(scale$over$breaks), length(scale$under$breaks)) <= 1L

  list(measure = measure, policy = policy, scale_key = scale_key,
       score = score, center = center, strict = strict,
       over_breaks = over_breaks, over_slots = over_slots,
       under_breaks = under_breaks, under_slots = under_slots, gate = gate,
       degenerate = degenerate)
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
  disp0 <- display_primary(get_display(x))
  slot[disp0 %in% c("blank", "gof")] <- 0L
  # Phase 17c: a "pvalue" test cell colours as a SIGNIFICANCE WARNING, not as a data effect -- a
  # non-significant test (p > alpha) gets the deepest under-slot (deep red), a significant one stays
  # uncoloured. It reads the honest `pvalue` field (defect 5: this used to be steered by a fake
  # diff = -0.5, so red never fired under grey_non_signif / guaranteed_effect). Scoped to the additive
  # `diff` channel -- the crosstab default -- so a p-value cell paints red TEXT with no background,
  # exactly as before on the common table, and stays uncoloured on non-diff measures (OR / ratio /
  # contrib), where it was uncoloured too.
  is_pv <- disp0 == "pvalue"
  if (any(is_pv) && identical(plan$measure, "diff")) {
    alpha  <- 1 - get_conf_level(x)
    pv     <- get_pvalue(x)
    slot[is_pv] <- 0L                                    # significant -> uncoloured
    slot[is_pv & !is.na(pv) & pv > alpha] <- max(plan$under_slots)   # non-significant -> deep-red warning
  }
  slot
}

# resolve_color_channel_plans() -- Phase 16c. Builds the text + background plans for a column AND applies
# the cross-channel arbitration that fmt_color_plan() (per-channel) cannot see: under `guaranteed_effect`
# a channel whose scale is a single break per side is `degenerate` (a flat "x1" fill, no gradient, and
# redundant with the other channel). Drop a degenerate channel, but NEVER the last one -- if the text
# channel is degenerate it survives only when a non-degenerate background does not (so a lone / both-
# degenerate table keeps the text channel, per the roadmap "keep the first channel"). Shared by both the
# cells (fmt_color_channels) and the legend (legend_specs) so they can never disagree.
#' @keywords internal
resolve_color_channel_plans <- function(x) {
  text <- fmt_color_plan(x, "text", color = get_color(x))
  bg   <- fmt_color_plan(x, "bg",   color = get_color_bg(x))
  keep_bg   <- !is.null(bg)   && !isTRUE(bg$degenerate)
  keep_text <- !is.null(text) && (!isTRUE(text$degenerate) || !keep_bg)
  list(text = if (keep_text) text else NULL,
       bg   = if (keep_bg)   bg   else NULL)
}

#' @keywords internal
fmt_color_channels <- function(x) {
  # text channel = the primary measure on the text slot table; background channel = the second
  # measure (get_color_bg, NA when absent) on the bg slot table. Each is an integer slot vector.
  # The cross-channel `guaranteed_effect` degenerate-drop lives in resolve_color_channel_plans().
  pl <- resolve_color_channel_plans(x)
  list(text_slot = fmt_color_slots(x, pl$text),
       bg_slot   = fmt_color_slots(x, pl$bg))
}

# The single slot -> APPEARANCE mapping shared by the exporters (tab_kable / tab_plot / tab_xl).
# Returns the per-cell rendering of BOTH channels: the colour code (NA where uncoloured on that
# channel), the raw slot vectors (for gate decisions), and -- Last Phase z11 -- the per-cell FACE
# (bold / italic / underline). The text channel uses the "text" palette, the background channel the
# "bg" palette. This mirrors pillar_shaft.tabxplor_fmt's two-channel logic, so console and exports
# render identical colours. (fmt_get_color_code stays single-channel for the golden.)
# DESIGN (z11): the face is the palette's answer to "how is this slot drawn", the twin of the hex, and
# it is what lets a monochrome palette exist at all -- five backends used to infer "bold" from "has a
# hex", which collapses when every text hex is black. The colour palettes answer bold-on-every-text-slot
# and nothing on the background ones, i.e. exactly how they have always been drawn, so this is
# byte-identical for light/dark. THE ENGINE STAYS THEME-BLIND: slots are computed above without knowing
# the theme, and only this boundary turns a slot into an appearance.
#' @keywords internal
fmt_channel_codes <- function(x, theme = "light") {
  n  <- length(x)
  ch <- fmt_color_channels(x)

  text_styles <- get_color_style("color_code", type = "text", theme = theme)
  bg_styles   <- get_color_style("color_code", type = "bg", theme = theme)

  text <- rep(NA_character_, n)
  bg   <- rep(NA_character_, n)
  tsel <- ch$text_slot > 0L
  bsel <- ch$bg_slot   > 0L
  # historical output is upper-case hex (cf. fmt_get_color_code).
  text[tsel] <- toupper(unname(text_styles[ch$text_slot[tsel]]))
  bg[bsel]   <- toupper(unname(bg_styles[ch$bg_slot[bsel]]))

  # Broadcast the palette's 8 slot faces onto the cells; slot 0 (uncoloured) keeps FALSE throughout.
  slot_face <- function(slot, type) {
    f   <- get_color_style("face", type = type, theme = theme)
    sel <- slot > 0L
    out <- list(bold = logical(n), italic = logical(n), underline = logical(n))
    for (k in names(out)) out[[k]][sel] <- f[[k]][slot[sel]]
    out
  }

  list(text = text, bg = bg, text_slot = ch$text_slot, bg_slot = ch$bg_slot,
       text_face = slot_face(ch$text_slot, "text"),
       bg_face   = slot_face(ch$bg_slot,   "bg"))
}

# Does this theme's palette need its face emitted as MARKUP (<b>/<i>/<u>) rather than only as CSS?
# TRUE for "print": the two destinations that matter for a publication table -- GitHub's markdown
# sanitizer (strips class AND style) and an HTML -> Word paste (keeps character formatting, drops
# stylesheets) -- carry tags and nothing else. FALSE for the colour palettes, whose meaning is the
# colour itself, so their markup is byte-unchanged.
#' @keywords internal
fmt_face_semantic <- function(theme = "light") {
  isTRUE(get_color_style("face", type = "text", theme = theme)$semantic)
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
#   legend_render_line(tokens, medium)      console ansi (cli) / html text_spec / md pandoc span /
#                                           excel fmt_txt runs / plain.
# The break-word colours come from the engine's per-side slots (over 1:4, under 5:8) indexed into the
# 8-hex palette -- the exact path fmt_channel_codes() / tx_slot_class() use for the cells.

# fixed (non-translated) symbols, kept as \uXXXX so R source stays ASCII.
.lg_ge    <- "\u2265"   # >=
.lg_le    <- "\u2264"   # <=
.lg_times <- "\u00d7"   # x  (times)
.lg_div   <- "\u00f7"   # /  (division)
.lg_beta  <- "\u03b2"   # beta

# Phase 16e: the per-measure fact table -- every language-invariant display fact of a colour measure in
# ONE place, instead of ~5 scattered switch arms the earlier code kept in sync by hand. Adding a measure
# is a row; a per-measure divergence (contrib colours BOTH sides "x N of the mean", ratio uses x over /
# div under) is a FIELD, not a switch case one can forget.
# Phase 17d: the ENGINE facts join the display facts here, so ONE row drives both the colour PLAN
# (fmt_color_plan) and the legend -- they can never diverge. Legend fields:
#   word / word_i18n   the measure word (gettext at render when word_i18n; "OR" is a literal).
#   break_over/under   the break-label glyph per side (see legend_break_label); break_scale = TRUE means a
#                      factor pct diff is shown x100 (gated by is_pct).
#   ref_kind           the baseline concept: "category" (a reference level), "indep" (independence), or NA
#                      = read the column's own tot/level reference.
#   threshold_mult     the grey-note first-break glyph is x (TRUE) or the symmetric +/- (FALSE).
#   unit_kind          drives the prose unit suffix: "diff" (points/SD/none by is_pct/is_std) | "contrib"
#                      ("the mean contribution") | "none".
#   has_ref_lead       the effect is stated vs a reference in the sentence LEAD (diff/ratio) rather than
#                      being already relative to it (or/contrib/reg effect).
# Engine fields (Phase 17d, read by fmt_color_plan):
#   raw                a getter closure(x) -> the observed per-cell quantity that is scored + coloured.
#   scale              named c(std=, pct=) of color_scales() keys; `std_when` picks which (see below).
#   std_when           which column kinds take the `std` scale key: "std_diff" (is_mean || coef, factor
#                      pct otherwise) | "mean" (is_mean) | "na" (both keys equal -> selector inert).
#   sig_source         where significance comes from: "bounds" (an interval, read through `bounds`) |
#                      "pvalue" (contrib -- no interval, reads the stored standardized-residual p-value).
#   bounds             (optional, Last Phase z8) closure(x) -> list(lo, hi), THE interval the two
#                      significance policies read. Absent = the stored ci_inf/ci_sup (every measure whose
#                      interval the table carries); the gap measures derive theirs from `gap_se`.
#                      measure_facts() fills the default, so a new row needs this only when it differs.
#   gate_row           which structural row this measure never colours: "refrow" (a reference level /
#                      regression intercept is a baseline) | "totrow" (contrib is undefined on a total).
#   force_policy       (optional, Last Phase z5) the measure has no significance test of its own, so it
#                      ALWAYS reads under this policy whatever the column's color_signif says -- applied
#                      by measure_policy(), which the plan and the legend both call.
#   by_scale           (optional, Last Phase z13) presentation facts that belong to a SCALE rather than
#                      to the measure, for the measures whose scale is chosen at runtime. Folded in by
#                      measure_facts() from the plan's `scale_key`, the same mechanism as `guar` folds a
#                      per-POLICY override -- so the legend cannot describe one branch while the cells
#                      colour the other. A measure with no entry resolves exactly as before.

# The presentation of a gap measure's ADDITIVE scales (Last Phase z13). Shared by both gap measures
# because both dispatch through fmt_gap_scale_key(): a "+"/"-" ladder around 0, and the unit the legend
# names -- percentage points on a probability-scale marginal effect, SD on a standardized one, nothing
# on a log coefficient. `break_scale = TRUE` renders the probability ladder x100 (2 rather than 0.02),
# the same convention the `diff` measure uses for a factor percentage.
#' @keywords internal
GAP_ADDITIVE_FACTS <- list(
  adj_diff     = list(break_over = "+", break_under = "-", threshold_mult = FALSE,
                      break_scale = TRUE,  unit_kind = "points"),
  adj_diff_std = list(break_over = "+", break_under = "-", threshold_mult = FALSE,
                      break_scale = FALSE, unit_kind = "std"),
  adj_diff_log = list(break_over = "+", break_under = "-", threshold_mult = FALSE,
                      break_scale = FALSE, unit_kind = "none")
)

MEASURES <- list(
  diff    = list(word = "difference",           word_i18n = TRUE,  break_over = "+",       break_under = "-",
                 break_scale = TRUE,  ref_kind = NA_character_, threshold_mult = FALSE, unit_kind = "diff",
                 has_ref_lead = TRUE,
                 raw = function(x) get_diff(x),  scale = c(std = "mean_diff",  pct = "pct_diff"),
                 std_when = "std_diff", sig_source = "bounds", gate_row = "refrow"),
  ratio   = list(word = "ratio",                word_i18n = TRUE,  break_over = .lg_times, break_under = .lg_div,
                 break_scale = FALSE, ref_kind = NA_character_, threshold_mult = TRUE,  unit_kind = "none",
                 has_ref_lead = TRUE,
                 raw = function(x) get_ratio(x), scale = c(std = "mean_ratio", pct = "pct_ratio"),
                 std_when = "mean",     sig_source = "bounds", gate_row = "refrow"),
  or      = list(word = "OR",                   word_i18n = FALSE, break_over = "",        break_under = "1/",
                 break_scale = FALSE, ref_kind = "category",    threshold_mult = TRUE,  unit_kind = "none",
                 has_ref_lead = FALSE,
                 raw = function(x) get_or(x),    scale = c(std = "odds_ratio", pct = "odds_ratio"),
                 std_when = "na",       sig_source = "bounds", gate_row = "refrow"),
  contrib = list(word = "contribution to Chi2", word_i18n = TRUE,  break_over = .lg_times, break_under = .lg_times,
                 break_scale = FALSE, ref_kind = "indep",       threshold_mult = TRUE,  unit_kind = "contrib",
                 has_ref_lead = FALSE,
                 raw = function(x) dplyr::if_else(is_totrow(x), NA_real_, get_ctr(x) / get_mean_contrib(x)),
                 scale = c(std = "contrib", pct = "contrib"),
                 std_when = "na",       sig_source = "pvalue", gate_row = "totrow",
                 # Last Phase z4: contrib is the ONE measure whose reading changes with the significance
                 # policy, so the divergence is a FIELD (an override applied by measure_facts()), never a
                 # switch arm. `ignore` / `grey_non_signif` colour the RELATIVE contribution (a share of
                 # this table's chi2 -- the correspondence-analysis reading, which necessarily floats with
                 # the table); `guaranteed_effect` colours the ADJUSTED STANDARDIZED RESIDUAL on the
                 # absolute `zscore` scale, whose thresholds mean the same thing in every table (the
                 # SPSS reading). Both readings share ONE significance source: the residual p-value.
                 # Last Phase z13: `guar` keeps only what depends on the POLICY. The glyphs and the
                 # threshold form it used to repeat by hand follow from the scale it swaps to, and now
                 # come from `by_scale$zscore` -- one override mechanism, keyed on the scale.
                 guar = list(word = "standardized residual", break_origin = "threshold",
                             scale = c(std = "zscore", pct = "zscore")),
                 by_scale = list(zscore = list(break_over = "+", break_under = "-",
                                               threshold_mult = FALSE, unit_kind = "none"))),
  # Last Phase z5 -- the two tab_reg-only measures. They score the SAME quantity through the SAME
  # helper (how far the model estimate sits from the value stored in `obs`) and differ ONLY in what
  # that value is, hence in the reference the legend names. `std_when = "additive"` selects the scale
  # from the estimate's own scale rather than from the column kind: an OR / RR / IRR is folded around
  # 1 on `adj_ratio`, a beta / AME / risk-difference around 0 on `adj_diff`.
  # Both derive their interval from the stored `gap_se` (Last Phase z8), so both read `color_signif`
  # normally -- WHERE tab_reg could write one. Where it could not, `force_policy` (now a predicate on
  # the column, fmt_gap_force_policy) makes the measure read under `ignore`, i.e. descriptively. The
  # two SEs come from different mathematics: `between_groups` compares DISJOINT split groups, so
  # quadrature on the printed intervals is exact; `adjustment` compares two estimates fitted on the
  # SAME rows, so its SE needs the difference of their influence functions (R/reg-influence.R,
  # dev/model_vs_observed_gap_test.md SS3).
  # Last Phase z13 (D4): the static presentation fields above describe `adj_ratio`, the multiplicative
  # branch; `by_scale` overrides them on each ADDITIVE one. Before it they were fixed per measure while
  # the scale was chosen at runtime, so an AME gap of two percentage points printed as "x0.02".
  adjustment     = list(word = "adjustment",    word_i18n = TRUE, break_over = .lg_times, break_under = .lg_div,
                 break_scale = FALSE, ref_kind = "observed",     threshold_mult = TRUE,  unit_kind = "none",
                 has_ref_lead = TRUE,
                 raw = function(x) fmt_adjustment_score(x), scale = c(std = "adj_diff", pct = "adj_ratio"),
                 std_when = "additive", sig_source = "bounds", bounds = fmt_gap_bounds,
                 gate_row = "refrow", force_policy = fmt_gap_force_policy,
                 by_scale = GAP_ADDITIVE_FACTS),
  between_groups = list(word = "between groups", word_i18n = TRUE, break_over = .lg_times, break_under = .lg_div,
                 break_scale = FALSE, ref_kind = "group",        threshold_mult = TRUE,  unit_kind = "none",
                 has_ref_lead = TRUE,
                 raw = function(x) fmt_adjustment_score(x), scale = c(std = "adj_diff", pct = "adj_ratio"),
                 std_when = "additive", sig_source = "bounds", bounds = fmt_gap_bounds,
                 gate_row = "refrow", force_policy = fmt_gap_force_policy,
                 by_scale = GAP_ADDITIVE_FACTS)
)

# The default `bounds` fact: the interval the column STORES. Every measure but the two gap ones reads
# it, so it lives here once instead of on each row (measure_facts fills it in).
#' @keywords internal
fmt_stored_bounds <- function(x) list(lo = get_ci_inf(x), hi = get_ci_sup(x))

# The measure's facts as they apply UNDER A GIVEN POLICY: the MEASURES row, with its `guar` override
# folded in for `guaranteed_effect` and its `bounds` default filled. THE single accessor --
# fmt_color_plan() and every legend helper read the facts through it, so the colour plan and the legend
# that describes it cannot diverge.
#' @keywords internal
measure_facts <- function(measure, policy = "ignore", scale_key = NULL) {
  md <- MEASURES[[measure]]
  if (is.null(md)) return(md)
  if (!is.null(md$guar) && identical(policy, "guaranteed_effect")) md <- utils::modifyList(md, md$guar)
  # Last Phase z13: the SELECTED scale's presentation override, applied AFTER `guar` (which may SWAP the
  # scale). A measure with no `by_scale`, or a scale with no entry, is untouched -- so every pre-z13
  # measure resolves identically whatever `scale_key` is passed, and a caller that reads only
  # scale-independent facts may omit it.
  if (!is.null(scale_key) && !is.null(md$by_scale[[scale_key]]))
    md <- utils::modifyList(md, md$by_scale[[scale_key]])
  if (is.null(md$bounds)) md$bounds <- fmt_stored_bounds
  md
}

# The policy a measure ACTUALLY reads under: the column's `color_signif`, unless the measure declares a
# `force_policy` (a measure with no significance test of its own). Last Phase z5: the twin of
# measure_facts() -- the plan and the legend both resolve the policy here, so a neutralised measure
# cannot be coloured under one policy while the legend describes another.
# Last Phase z5: does this measure's baseline live in ANOTHER COLUMN (the observed effect, a reference
# group) rather than in a row of its own? Two consequences, both of which would otherwise be
# hand-repeated: such a measure NAMES ITSELF in the legend (the column's effect word -- "OR", "AME" --
# is the thing being compared, not what the colour measures), and it resolves its own reference phrase
# per channel instead of borrowing the text channel's.
#' @keywords internal
measure_own_ref <- function(measure) isTRUE(MEASURES[[measure]]$ref_kind %in% c("observed", "group"))

# Last Phase z13 (D7): is THIS column the baseline of its own gap measure? A measure whose baseline is
# another column leaves `obs` empty on the column that IS that baseline -- the reference `split_var`
# group, or a model with no observed counterpart -- so not one cell can ever be coloured and every break
# in the ladder is unreachable. Printing the scale there describes a colouring that does not exist, and
# costs the grouping (the line cannot merge with the columns that DO colour). Say what the column is
# instead.
# The test is the STORED `obs` being empty (spec$no_obs), not the plan's gate: under grey_non_signif a
# fully-comparable column with no significant gap also gates nothing, and it must still show its ladder.
#' @keywords internal
legend_gap_baseline <- function(plan, no_obs)
  !is.null(plan) && isTRUE(no_obs) && measure_own_ref(plan$measure)

#' @keywords internal
legend_gap_baseline_word <- function(plan) {
  if (identical(MEASURES[[plan$measure]]$ref_kind, "group")) gettext("reference group")
  else                                                       gettext("no observed effect")
}

#' @keywords internal
measure_policy <- function(measure, policy = "ignore", x = NULL) {
  fp <- MEASURES[[measure]]$force_policy
  if (is.null(fp)) return(policy)
  # Last Phase z8-B: a `force_policy` may be a PREDICATE ON THE COLUMN rather than a constant (see
  # fmt_gap_force_policy). With no column to ask, the caller's policy stands.
  if (is.function(fp)) fp <- if (is.null(x)) NULL else fp(x)
  if (is.null(fp)) policy else fp
}

# a legend token: plain text (c = NA) or a coloured break-word (c = palette slot 1:8).
# Phase 13d: the CSS class is not stored -- it is tx_slot_class(ch, c), derived at render time, so a
# legend break-word and the cells it describes cannot name different classes.
# Phase g: `b` = an explicit bold flag on a PLAIN token (variable names are bolded in every medium
# without being coloured). `esc` = escape markdown-active `*` in the md medium (the significance-stars
# legend), so pandoc does not read `***`/`*` as emphasis. User subtext is NOT flagged (its markdown is
# left intact). Coloured tokens decide weight in legend_render_line (text = bold, bg = plain).
.lg_tok  <- function(t, bold = FALSE, esc = FALSE)
  list(t = t, c = NA_integer_, ch = NA_character_, b = isTRUE(bold), esc = isTRUE(esc))
.lg_ctok <- function(t, slot, ch) list(t = t, c = as.integer(slot), ch = ch, b = FALSE, esc = FALSE)

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
# Last Phase z4: the reference-free ("indep") baseline word is a per-channel FACT (ref_word, resolved in
# legend_resolve_spec from the policy-aware MEASURES row), because contrib's two readings name it
# differently: the CONTRIBUTION is a multiple of the mean cell contribution, the RESIDUAL is a distance
# from independence itself. Fallback keeps the pre-z4 wording for a spec built without the fact.
legend_ref_short <- function(spec, lang) {
  ref <- spec$ref
  switch(ref$kind,
         "tot"      = if (!is.na(ref$label) && nzchar(ref$label)) ref$label else gettext("Total"),
         "level"    = if (!is.na(ref$label) && nzchar(ref$label)) ref$label else gettext("ref."),
         "category" = gettext("ref."),
         "indep"    = if (!is.null(spec$txt$ref_word)) spec$txt$ref_word else gettext("vs the mean"),
         "")
}

# one break threshold -> its bare label (no colour), per measure. Phase 16e: fully MEASURES-driven --
# each side's glyph is a data field (contrib's "x N on BOTH sides", ratio's x / div, or's N / 1/N). `is_pct`
# = a factor pct-point diff (x100 units, break_scale); a numeric-mean / coef diff (is_pct FALSE) shows the
# raw break value, whether it is sd-standardized (SD units) or raw (custom mean_diff breaks).
legend_break_label <- function(measure, brk, dir, is_pct, lang, policy = "ignore", scale_key = NULL) {
  m <- measure_facts(measure, policy, scale_key)
  if (is.null(m)) return(as.character(brk))
  scale <- if (isTRUE(m$break_scale) && isTRUE(is_pct)) 100 else 1
  glyph <- if (dir < 0L) m$break_under else m$break_over
  paste0(glyph, legend_num(abs(brk) * scale, lang))
}

# the coloured break tokens of one channel, split over / under (each a list of tokens). Slot 0 (a
# scale that skips an intensity via NA) -> a plain, uncoloured token. The token carries the palette
# slot; its colour (hex) or class is resolved per medium at render time.
legend_break_tokens <- function(plan, is_pct, is_mean, channel, lang, theme = "light") {
  if (is.null(plan)) return(list(over = list(), under = list()))
  measure <- plan$measure
  # Last Phase z11: the legend must not promise a distinction the cells do not make. Typography
  # honestly supports 2 levels per side, so the print palette gives slots 1&2 (and 3&4) the SAME
  # rendering; a token whose full rendering repeats the previous one is dropped, and the survivor is
  # the LOWER threshold -- "bold = at least +5 points", which is what the appearance means. Byte-
  # identical under the colour palettes: their four hexes per side are all distinct, so nothing
  # collapses. This is deliberately NOT a cap inside fmt_color_slots() -- the ENGINE must stay
  # theme-blind (dev/black_and_white_publication_palette.md SS4.3).
  fam <- if (identical(channel, "text")) "text" else "bg"
  hex <- get_color_style("color_code", type = fam, theme = theme)
  fc  <- get_color_style("face",       type = fam, theme = theme)
  look <- function(slot) paste(hex[slot], fc$bold[slot], fc$italic[slot], fc$underline[slot])
  mk_side <- function(breaks, slots, dir) {
    prev <- NA_character_
    out  <- list()
    for (l in seq_along(breaks)) {
      slot <- slots[l + 1L]
      lab  <- legend_break_label(measure, breaks[l], dir, is_pct, lang, plan$policy, plan$scale_key)
      if (is.na(slot) || slot == 0L) { out <- c(out, list(.lg_tok(lab))); prev <- NA_character_; next }
      key <- look(slot)
      if (!is.na(prev) && identical(key, prev)) next     # same rendering as the previous break
      prev <- key
      out  <- c(out, list(.lg_ctok(lab, slot, channel)))
    }
    out
  }
  list(over  = mk_side(plan$over_breaks,  plan$over_slots,  +1L),
       under = mk_side(plan$under_breaks, plan$under_slots, -1L))
}

# Phase 14x: the FIRST colour threshold as a compact phrase -- the smallest departure from the
# reference a cell must reach to be coloured. Shared by the grey_non_signif terse tag AND prose note so
# they name the SAME concrete threshold instead of the vague "too small a difference". Additive
# measures (pct / standardized diff) are symmetric -> "±<v> points" / "±<v> SD"; multiplicative
# ones (ratio / OR / contrib) -> "×<v>". NA when the scale carries no first break (an uncoloured table).
legend_threshold_phrase <- function(plan, is_pct, is_std, lang) {
  if (is.null(plan) || length(plan$over_breaks) == 0L) return(NA_character_)
  brk <- plan$over_breaks[[1]]
  if (is.na(brk)) return(NA_character_)
  md <- measure_facts(plan$measure, plan$policy, plan$scale_key)
  if (isTRUE(md$threshold_mult)) {                         # ratio / OR / contrib
    paste0(.lg_times, legend_num(abs(brk), lang))
  } else {                                                 # diff: symmetric +/- <v> [unit]
    # Last Phase z13: the x100 rule is the one legend_break_label() uses, so the grey-note threshold
    # and the break ladder it describes cannot disagree (this read `is_pct` alone).
    sc100 <- isTRUE(md$break_scale) && isTRUE(is_pct)
    val   <- legend_num(abs(brk) * if (sc100) 100 else 1, lang)
    unit  <- legend_unit_word(md, is_pct, is_std)
    if (nzchar(unit)) paste0("\u00b1", val, " ", unit) else paste0("\u00b1", val)
  }
}

# The prose unit suffix of ONE measure-at-scale, as a bare word (each caller adds its own spacing).
# Last Phase z13: shared by legend_resolve_spec()'s chan() and legend_threshold_phrase(), which held the
# same switch twice. `"diff"` consults the column kind (a factor percentage vs a standardized numeric
# difference); the gap scales DECLARE their unit, which keeps them clear of `is_std` -- that flag reads
# whether the `mean_diff` scale happens to be standardized, so a user's set_color_breaks(mean_diff =)
# would otherwise drop "SD" from a legend describing a genuinely standardized gap.
legend_unit_word <- function(md, is_pct, is_std) switch(
  md$unit_kind,
  "diff"    = if (isTRUE(is_pct)) gettext("points") else if (isTRUE(is_std)) gettext("SD") else "",
  "points"  = gettext("points"),
  "std"     = gettext("SD"),
  "contrib" = gettext("the mean contribution"),
  "")

# join tokens with a plain-text separator.
legend_join <- function(toks, sep) {
  if (length(toks) == 0) return(list())
  out <- list(toks[[1]])
  for (i in seq_along(toks)[-1]) out <- c(out, list(.lg_tok(sep)), list(toks[[i]]))
  out
}

# default palette -> baked shade names; a custom palette (set_color_palette) -> NA (generic, the
# coloured break-words carry the meaning). The over side of the default palette is teal->blue, the
# under side gold->red, in both light and dark, so those names are hue-descriptive and theme-free.
# Last Phase z11: returns one pair PER CHANNEL, because the print palette names two different things --
# its text side is a TYPOGRAPHY ("Bold" / "Italic") and its background side a grey fill. The split is
# not cosmetic: legend_tokens_prose()'s one_side() passes no_shade = FALSE for a BACKGROUND-ONLY
# coloured column, so a single pair would have made such a table announce "Bold:" about grey fills.
legend_shade_names <- function(theme = "light") {
  if (identical(tx_palette_theme(theme), "print")) {
    # Curated palette, so these are always right (no is_default dance): they describe the face table.
    return(list(text = c(over = gettext("Bold"),       under = gettext("Italic")),
                bg   = c(over = gettext("Grey fill"),  under = gettext("Grey fill"))))
  }
  is_default <- tryCatch({
    b <- get0("base", envir = tabxplor_palette_env)
    is.null(b) || (identical(b$text_colors,     default_text_colors) &&
                   identical(b$text_colors_neg, default_text_colors_neg))
  }, error = function(e) FALSE)
  pair <- if (isTRUE(is_default))
    c(over = gettext("Shades of blue"), under = gettext("Shades of yellow to red"))
  else
    c(over = NA_character_, under = NA_character_)
  list(text = pair, bg = pair)
}

# Phase 14w: a regression column's effect word (OR / IRR / beta / AME / MER), DERIVED from the table
# family/effect (reg_meta) + the column's own ci_type / type -- replaces parsing the column-name suffix,
# which the 14w header rename dropped ("Model OR" / "Ind vs Rep" no longer end in ": <word>"). An
# EMPIRICAL crude column (% / mean / diff / rate) has no effect word; an empirical OR/IRR takes the
# family's multiplicative word, so its legend names the right scale (Emp. IRR -> rate-ratio, item 5).
legend_reg_eff_word <- function(col, meta) {
  # Phase 15e: the OR-vs-IRR split reads the column's OWN family (the `model_family` fmt attribute), so a
  # mixed table names each column correctly; fall back to the table's scalar family when unset. `effect`/
  # `at` stay table-level (one per call). A `coef`-type column in coefficient mode is always the additive
  # beta scale (AME is handled by the effect branch above), so no scalar `do_exp` check is needed -- that
  # scalar would mislabel a gaussian column in a binomial-first mixed table.
  fam <- get_model_family(col); if (!nzchar(fam)) fam <- meta$family
  # Last Phase z3: two ways a multiplicative column can be a RISK ratio rather than an odds ratio.
  # (a) effect = "ame_ratio" -- the ESTIMAND is a ratio of adjusted probabilities, whatever family was
  #     fitted (a logistic fit still yields a marginal RR), so it wins over the family switch; it also
  #     covers the crude Obs_RR companion, which carries the model's family attribute.
  # (b) family "rr" -- the modified Poisson, whose exp(coef) is a risk ratio by construction.
  if (identical(get_ci_type(col), "or")) {
    if (identical(meta$effect, "ame_ratio")) return("RR")
    return(switch(fam, "poisson" = , "quasipoisson" = "IRR", "rr" = "RR", "OR"))
  }
  if (!identical(get_role(col), "emp")) {              # Phase 17c: a model (not crude) column, by stored role
    if (identical(meta$effect, "ame")) return(if (identical(meta$at, "reference")) "MER" else "AME")
    if (identical(get_type(col), "coef")) return(.lg_beta)   # gaussian beta
  }
  NA_character_
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
      if (isTRUE(is_totcol(x[[idx[[1]]]]))) NA_character_ else nm   # Phase 17c: a total column (by stored attr) -> generic "Total"
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
# Phase 14c: `is_coef` (a regression beta) must take the same "category" branch as OR/IRR. It reads
# ref_type "tot" like any fmt column, but a regression table HAS no total row -- the legend claimed
# "not significantly different from the Total row". Its baseline is the reference category, exactly as
# for the multiplicative effects. (Imprecise for a numeric predictor's per-unit beta, whose null is 0
# -- the same approximation the OR arm has always made.)
legend_ref_info <- function(x, col, measure, orientation, is_coef = FALSE, is_reg = FALSE,
                            policy = "ignore") {
  base_kind <- measure_facts(measure, policy)$ref_kind  # Phase 16e: the measure's baseline concept, one field
  if (identical(base_kind, "indep"))
    return(list(kind = "indep", label = NA_character_, orientation = orientation))
  # Last Phase z5: these two baselines are NEITHER a total nor a predictor's reference category -- they
  # are another COLUMN's estimate (the observed effect, or the reference group's). They must be
  # resolved BEFORE the is_reg branch below, which would otherwise claim "the reference category" for
  # every reg column and describe the wrong comparison.
  if (base_kind %in% c("observed", "group"))
    return(list(kind = base_kind, label = NA_character_, orientation = "row"))
  # Phase 14w: a regression table has no total row -- every reg column (incl. AME, ci_type "diff", and the
  # empirical crude columns) is compared to the predictor's REFERENCE CATEGORY, never "the Total row".
  if (isTRUE(is_reg) || identical(base_kind, "category") || isTRUE(is_coef))
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
  if (identical(ref$kind, "observed")) return(gettext("the observed (crude) effect"))
  # Last Phase z8: "...'s effect", not just "the reference group" -- with the significance policies live
  # the note reads "significantly different from ...", and what differs is the EFFECT, not the group.
  if (identical(ref$kind, "group"))    return(gettext("the reference group's effect"))
  if (identical(ref$kind, "category")) {
    if (!is.na(lab) && nzchar(lab)) return(gettextf("the reference category (%s)", lab))
    return(gettext("the reference category"))
  }
  base <- if (identical(ref$orientation, "col")) gettext("column") else gettext("row")
  if (is.na(lab) || !nzchar(lab)) lab <- gettext("Total")
  gettextf("the %s %s", lab, base)                 # EN "the Total row"; FR "la %2$s %1$s" -> "la ligne Total"
}

# the CI-method name (NA when there is none, e.g. contrib). Last Phase z8: `measure` defaults to the
# text channel's but is passed explicitly per channel by legend_resolve_spec -- a gap measure on the
# background names ITS OWN test, not the text channel's model interval.
legend_method_name <- function(spec, measure = spec$measure_text) {
  cis <- spec$ci_settings
  # Last Phase z5 / z8: these measures score a GAP between two estimates, so the model's own Wald
  # interval (which the is_reg branch below would claim) is never the right name -- each has a test of
  # its own, and they are DIFFERENT tests. `between_groups` compares two DISJOINT subpopulations, so
  # the two estimates are independent and quadrature is exact; `adjustment` compares two estimates
  # fitted on the SAME rows, so its variance is the difference of their influence functions, which is
  # the only quantity carrying the covariance between them (R/reg-influence.R).
  if (identical(measure, "between_groups"))
    return(gettext("z test on the difference between two independent estimates"))
  if (identical(measure, "adjustment"))
    return(gettext("z test on the difference between two estimates fitted on the same sample"))
  if (isTRUE(spec$is_reg)) {
    if (identical(cis$method_diff, "profile")) return(gettext("profile-likelihood interval"))
    # Phase 14c: ci_type "or" is the multiplicative SHAPE, shared by the odds ratio, the Poisson rate
    # ratio and the cumulative OR -- naming it "log odds-ratio" unconditionally called a Poisson IRR an
    # odds ratio. The effect word (the column-name suffix the package itself writes) is the scale.
    if (identical(spec$ci_type, "or")) {
      w <- spec$eff_word; if (is.null(w) || is.na(w)) w <- ""
      return(switch(w,
                    "IRR" = gettext("Wald interval on the log rate-ratio"),
                    "OR"  = gettext("Wald interval on the log odds-ratio"),
                    "RR"  = gettext("Wald interval on the log risk-ratio"),
                    gettext("Wald interval on the log scale")))
    }
    return(gettext("Wald interval"))
  }
  if (identical(measure, "or")) return(gettext("Wald interval on the log odds-ratio"))
  if (identical(measure, "contrib")) return(NA_character_)
  # 14v-ii: a mean names the method actually used, from ci_settings. A ratio-of-means (ci_type "ratio")
  # is one of the dispersion-ladder intervals (robust / quasi / naive Poisson); a mean difference is
  # Welch or pooled Student (method_mean_diff).
  if (isTRUE(spec$is_mean)) {
    if (identical(spec$ci_type, "ratio")) {
      mmr <- cis$method_mean_ratio; if (is.null(mmr)) mmr <- "robust"
      return(switch(mmr,
                    "robust"       = gettext("robust-Poisson (delta) interval"),
                    "quasipoisson" = gettext("quasi-Poisson interval"),
                    "poisson"      = gettext("Poisson interval"),
                    gettext("confidence interval")))
    }
    mmd <- cis$method_mean_diff; if (is.null(mmd)) mmd <- "welch"
    return(switch(mmd,
                  "welch"   = gettext("Welch t interval"),
                  "student" = gettext("Student t interval"),
                  gettext("confidence interval")))
  }
  # Phase 14b: the STORED interval names itself. A ratio-coloured proportion column carries the Katz
  # log-RR bounds, not one of the `method_diff` difference approximations the switch below names --
  # and the legend must not claim a method the bracket was never built with.
  if (identical(spec$ci_type, "ratio")) return(gettext("Katz interval on the log risk-ratio"))
  md <- cis$method_diff; if (is.null(md)) md <- "newcombe"
  switch(md,
         "newcombe" = gettext("Newcombe score interval"),
         "ac"       = gettext("Wald interval with Agresti-Caffo adjustment"),
         "wald"     = gettext("Wald interval"),
         gettext("confidence interval"))
}

# "<method>, 95% confidence" (or just the confidence text when there is no method name).
legend_method_phrase <- function(spec, lang, measure = spec$measure_text) {
  conf <- gettextf("%s%% confidence", legend_num(spec$ci_settings$conf_level * 100, lang))
  m    <- legend_method_name(spec, measure)
  if (is.na(m)) conf else gettextf("%s, %s", m, conf)
}

# the measure / effect word (reg effect word takes precedence). Phase 16e: MEASURES-driven -- the only
# non-table special-case is the sd-standardized diff wording (a spec fact, not a measure fact).
legend_measure_word <- function(measure, is_std, eff_word, lang, policy = "ignore") {
  if (!is.na(eff_word) && !measure_own_ref(measure)) return(eff_word)
  if (identical(measure, "diff") && isTRUE(is_std)) return(gettext("standardized difference"))
  m <- measure_facts(measure, policy)
  if (is.null(m)) return(measure)
  if (isTRUE(m$word_i18n)) gettext(m$word) else m$word
}
# Last Phase w: potools extracts translatable strings by STATIC analysis, so the dynamic gettext(m$word)
# above is invisible to it. This dead-code anchor lists the MEASURES$word literals (word_i18n = TRUE) so
# they land in the .pot and are compiled into the .mo -- runtime lookup then matches. Keep in sync with
# MEASURES; it is never executed.
if (FALSE) c(gettext("difference"), gettext("ratio"), gettext("contribution to Chi2"),
             gettext("standardized residual"), gettext("adjustment"), gettext("between groups"))

legend_ucfirst <- function(s) {
  if (!nzchar(s)) return(s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# Phase 16e: pre-compute EVERY per-measure / per-channel display fact into the spec ONCE (called between
# legend_specs and grouping, under the render language), so the token assemblers below are dumb templates
# -- no switch(measure), no is_reg / is_coef branch. Per-channel facts (subject / has_ref_lead / unit) are
# resolved for BOTH the text and background measures into spec$txt / spec$bg; the scalar phrases
# (ref_phrase / method_phrase / threshold_phrase) once. The measure WORD stays a legend_measure_word()
# call at the (few) use sites -- it is a MEASURES lookup, not a switch, and the text vs background channels
# feed it different eff_word, matching the historical wording exactly.
legend_resolve_spec <- function(spec, lang) {
  # Last Phase z8-B: each channel resolves its facts under ITS OWN policy. `spec$policy` is the text
  # channel's, and since a gap measure's force_policy became a per-column predicate the two channels
  # can genuinely differ (an OR text channel greying by its Wald interval, an `adjustment` background
  # with no test in this column reading descriptively). Resolved once here, per channel, as everything
  # else in this spec is.
  chan <- function(measure, policy = spec$policy, scale_key = NULL) {
    if (is.na(measure)) return(NULL)
    if (is.null(policy)) policy <- spec$policy
    md   <- measure_facts(measure, policy, scale_key)
    subj <- if (!is.na(spec$eff_word)) spec$eff_word
            else if (identical(measure, "or")) "OR" else gettext("cells")
    u    <- legend_unit_word(md, spec$is_pct, spec$is_std)
    unit <- if (nzchar(u)) paste0(" ", u) else ""
    # Last Phase z5: `adjustment` / `between_groups` are the only measures whose baseline is ANOTHER
    # COLUMN's estimate rather than a row of this one, so the reference is a per-CHANNEL fact -- the
    # scalar spec$ref_phrase (resolved for the text measure) would describe the wrong comparison when
    # one of them rides the background. It also has to survive the is_reg strip just below, which
    # exists because an ordinary reg effect states its reference in the note, not in the lead.
    own_ref <- measure_own_ref(measure)
    list(subject      = subj,
         ref_lead     = if (own_ref)
           legend_ref_phrase(list(ref = list(kind = md$ref_kind, label = NA_character_)), lang)
           else NA_character_,
         has_ref_lead = own_ref ||
           (isTRUE(md$has_ref_lead) && !isTRUE(spec$is_coef) && !isTRUE(spec$is_reg)),
         # Last Phase z4: under `guaranteed_effect` this measure's breaks are ABSOLUTE thresholds on
         # the quantity itself (contrib's standardized residual), not a CI floor -- so the sentence
         # must not say "after subtracting the margin of error". One declared fact, two wordings.
         guar_abs     = identical(md$break_origin, "threshold"),
         # the reference-free baseline word (ref_kind "indep"): "x N of the mean contribution" for the
         # contribution, a distance from independence for the residual.
         ref_word     = if (identical(md$unit_kind, "contrib")) gettext("vs the mean")
                        else gettext("vs independence"),
         # Last Phase z8: the interval NAME is per channel for the same reason the reference is -- a
         # gap measure on the background runs its own test, so the "after subtracting the margin of
         # error (...)" tail must not borrow the text channel's model interval.
         method_phrase = legend_method_phrase(spec, lang, measure),
         unit         = unit)
  }
  spec$txt <- chan(spec$measure_text, spec$plan_txt$policy, spec$plan_txt$scale_key)
  spec$bg  <- chan(spec$measure_bg,   spec$plan_bg$policy,  spec$plan_bg$scale_key)
  spec$ref_phrase       <- legend_ref_phrase(spec, lang)
  spec$method_phrase    <- legend_method_phrase(spec, lang)
  primary <- if (is.null(spec$plan_txt)) spec$plan_bg else spec$plan_txt
  spec$threshold_phrase <- legend_threshold_phrase(primary, spec$is_pct, spec$is_std, lang)
  spec
}

# ---- assemblers: spec -> token stream (dumb templates over legend_resolve_spec() fields) ------------

# TERSE (console): compact, one line per group -- names? + measure (ref): <breaks>  [; bg]  [policy].
legend_tokens_terse <- function(spec, lang, show_names) {
  colon <- if (identical(lang, "fr")) " : " else ": "
  toks <- list()
  # Phase g: variable names are bold in every medium.
  if (show_names) toks <- c(toks, list(.lg_tok(paste0(legend_name_list(spec$col_names, lang = lang),
                                                      colon), bold = TRUE)))
  rs <- legend_ref_short(spec, lang)
  add_channel <- function(plan, prefix, is_bg) {
    if (legend_gap_baseline(plan, spec$no_obs))
      return(list(.lg_tok(paste0(prefix,
                                 legend_measure_word(plan$measure, spec$is_std, spec$eff_word, lang,
                                                     plan$policy),
                                 colon, legend_gap_baseline_word(plan)))))
    mw <- legend_measure_word(plan$measure, spec$is_std, spec$eff_word, lang, plan$policy)
    bt <- legend_break_tokens(plan, spec$is_pct, spec$is_mean, if (is_bg) "bg" else "text", lang,
                             spec$theme %||% "light")
    seq_toks <- c(rev(bt$under), bt$over)
    lbl <- paste0(prefix, mw, if (!is_bg && nzchar(rs)) paste0(" (", rs, ")") else "", colon)
    c(list(.lg_tok(lbl)), legend_join(seq_toks, " "))
  }
  if (!is.null(spec$plan_txt)) toks <- c(toks, add_channel(spec$plan_txt, "", FALSE))
  if (!is.null(spec$plan_bg))  toks <- c(toks, list(.lg_tok(if (identical(lang, "fr")) " ; " else "; ")),
                                         add_channel(spec$plan_bg, paste0(gettext("bg"), " "), TRUE))
  # Phase 14x: grey_non_signif names the first threshold a cell must reach ("or under ±5 points"), so
  # the tag no longer implies the false converse (grey == not significant). A grey cell is EITHER not
  # significant OR below that threshold; the guarantee is only coloured => significant.
  thr <- spec$threshold_phrase
  # Last Phase z13 (D8): "or not tested" only where some rows genuinely carry no test (partial_test).
  untested <- if (isTRUE(spec$partial_test)) paste0(", ", gettext("or not tested")) else ""
  pn <- switch(spec$policy,
               "grey_non_signif"   = if (!is.na(thr))
                                       paste0(gettextf("grey: non-significant or under %s", thr), untested)
                                     else paste0(gettext("grey: non-significant or small"), untested),
               # Last Phase z4: "error-adjusted" describes a CI floor; the absolute-threshold reading
               # (contrib's standardized residual) subtracts nothing -- the breaks ARE the quantity.
               "guaranteed_effect" = if (isTRUE(spec$txt$guar_abs))
                                       gettext("all that is significant is colored")
                                     else gettext("all that is significant is colored, error-adjusted"),
               "")
  if (nzchar(pn)) toks <- c(toks, list(.lg_tok(paste0(" [", pn, "]"))))
  toks
}

# PROSE (exports): full translatable sentences with coloured break-words. Everything measure-specific
# (subject / lead / unit / whether the reference is in the lead) is derived from the PLAN's own
# measure inside one_side(), so the text channel (e.g. diff) and the background channel (e.g. ratio)
# each describe themselves correctly.
legend_tokens_prose <- function(spec, lang, show_names) {
  # French typography: a (thin) space before the high punctuation ; : (matches the user's examples).
  semi  <- if (identical(lang, "fr")) " ; " else "; "
  colon <- if (identical(lang, "fr")) " : " else ": "

  one_side <- function(plan, dir, is_bg, no_shade = FALSE) {
    if (is.null(plan)) return(NULL)
    # the baseline column itself: one clause, on the over side only (there is no ladder to describe).
    # The measure is already named by the caller (the "Background colour (between groups):" header, or
    # the text channel's own subject), so this states only WHAT the column is.
    if (legend_gap_baseline(plan, spec$no_obs)) {
      if (dir < 0) return(NULL)
      return(list(.lg_tok(paste0(legend_ucfirst(legend_gap_baseline_word(plan)), "."))))
    }
    bt   <- legend_break_tokens(plan, spec$is_pct, spec$is_mean, if (is_bg) "bg" else "text", lang,
                             spec$theme %||% "light")
    side <- if (dir > 0) bt$over else bt$under
    if (length(side) == 0) return(NULL)
    # Phase 16e: subject / has_ref_lead / unit are resolved per channel in legend_resolve_spec (coef / OR /
    # contrib / reg carry the ref in the note only -> has_ref_lead FALSE; contrib's "the mean contribution"
    # is a unit suffix, not a lead). The template just reads them.
    cf    <- if (is_bg) spec$bg else spec$txt
    cmp   <- if (dir > 0) .lg_ge else .lg_le
    sh    <- spec$shades[[if (is_bg) "bg" else "text"]]
    shade <- if (no_shade) NA_character_ else if (dir > 0) sh[["over"]] else sh[["under"]]
    rp    <- if (!is.na(cf$ref_lead)) cf$ref_lead else spec$ref_phrase   # Last Phase z5: per channel
    lead  <- if (cf$has_ref_lead) gettextf("%s %s %s", cf$subject, cmp, rp)
             else                 gettextf("%s %s", cf$subject, cmp)
    head_toks <- if (!is.na(shade)) list(.lg_tok(paste0(shade, colon, lead, " ")))
                 else               list(.lg_tok(paste0(legend_ucfirst(lead), " ")))
    # guaranteed_effect: the coloured thresholds are the CI floor -> annotate the OVER sentence
    # ("..., after subtracting the margin of error (<method>).") instead of a bare ".".
    tail <- if (dir > 0 && identical(spec$policy, "guaranteed_effect") && !isTRUE(cf$guar_abs))
              paste0(cf$unit, ", ", gettextf("after subtracting the margin of error (%s)", cf$method_phrase), ".")
            else paste0(cf$unit, ".")
    c(head_toks, legend_join(side, semi), list(.lg_tok(tail)))
  }

  toks <- list()
  if (show_names)  # Phase g: variable names are bold in every medium.
    toks <- c(toks, list(.lg_tok(paste0(legend_name_list(spec$col_names, lang = lang), " \u2014 "),
                                 bold = TRUE)))

  # Last Phase z5: `adjustment` on an ODDS RATIO needs one sentence of honesty. The odds ratio is
  # NON-COLLAPSIBLE: adjusting for a covariate that predicts the outcome moves it away from 1 even with
  # zero confounding (measured +7.9 % on a simulation where the covariate is independent of the
  # exposure, against +0.26 % for the risk ratio -- dev/model_vs_observed_effect_colour.md SS3). That is
  # the same order of magnitude as the 10 % first break, so without this the first colour step reads as
  # confounding when it may be arithmetic. Collapsible estimands (AME, RR, IRR, gaussian beta) are
  # exempt, which is exactly the point to make: the caveat names the fix by naming who does not need it.
  adj_ch <- c(spec$measure_text, spec$measure_bg)
  # `is_coef` covers exponentiate = FALSE (a raw logit coefficient is the same non-collapsible
  # quantity, logged) -- it must not be read off eff_word there, because legend_reg_adapter()
  # deliberately neutralises a model column's effect word when a crude sibling shares its measure.
  # Last Phase z8-B: `reg_fam_prob()` (R/tab_reg.R), not the family list written out -- it WAS the exact
  # body of that predicate, i.e. the third copy the z3 predicate block exists to prevent. The legend
  # cannot see `effect`, so it reads the same rule off COLUMN facts: a probability-scale family whose
  # column is a coefficient (is_coef covers exponentiate = FALSE; eff_word the exponentiated twin). Keep
  # set-identical to reg_estimand_collapsible(), which states it from the build side.
  if ("adjustment" %in% adj_ch &&
      isTRUE(reg_fam_prob(spec$model_family)) &&
      (isTRUE(spec$is_coef) || isTRUE(spec$eff_word %in% c("OR", .lg_beta)))) {
    spec$caveat <- gettext("Part of an odds-ratio gap is non-collapsibility, not confounding: a risk ratio or a marginal effect is the collapsible comparison.")
  }

  is_bg_only <- is.null(spec$plan_txt)
  primary    <- if (is_bg_only) spec$plan_bg else spec$plan_txt
  ov <- one_side(primary, +1L, is_bg_only); un <- one_side(primary, -1L, is_bg_only)
  if (!is.null(ov)) toks <- c(toks, ov)
  if (!is.null(un)) toks <- c(toks, list(.lg_tok(" ")), un)

  # a second measure on the background channel (e.g. color = c("diff","ratio")).
  if (!is.null(spec$plan_txt) && !is.null(spec$plan_bg)) {
    bgw <- legend_measure_word(spec$measure_bg, spec$is_std, NA_character_, lang, spec$policy)
    toks <- c(toks, list(.lg_tok(paste0(" ", gettextf("Background colour (%s):", bgw)))))
    bov <- one_side(spec$plan_bg, +1L, TRUE, no_shade = TRUE)
    bun <- one_side(spec$plan_bg, -1L, TRUE, no_shade = TRUE)
    if (!is.null(bov)) toks <- c(toks, list(.lg_tok(" ")), bov)
    if (!is.null(bun)) toks <- c(toks, list(.lg_tok(" ")), bun)
  }

  # the grey-cells note (guaranteed_effect already annotated the over sentence).
  # Phase 14q (Item B): under grey_non_signif a cell is coloured only when it is significant AND its
  # effect reaches the first break, so an UNCOLOURED cell may be significant-but-small (some even carry
  # stars). The old "Grey: not significantly different" was therefore statistically false: the only
  # guarantee is coloured => significant. State that directly.
  # Phase 14x: name the first threshold concretely ("or a difference under ±5 points" -- generalised to
  # ×1.15 for ratios, ±0.2 SD for standardized means, and custom breaks) instead of "too small a
  # difference to colour". Falls back to the vague form only when the scale has no first break.
  # NB: the format string is ONE literal, not paste0("a ", "b"): xgettext extracts each string constant
  # separately, so a paste0-split message never matches the paste0-JOINED string gettextf looks up at
  # runtime -> the translation silently fails (the split-part po entries are dead). Keep it on one line.
  if (identical(spec$policy, "grey_non_signif")) {
    thr  <- spec$threshold_phrase
    note <- if (!is.na(thr))
      gettextf("Coloured: significantly different from %s (%s), by at least the first colour threshold. Uncoloured: either not significant, or a difference under %s.",
               spec$ref_phrase, spec$method_phrase, thr)
    else
      gettextf("Coloured: significantly different from %s (%s), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.",
               spec$ref_phrase, spec$method_phrase)
    # Last Phase z13 (D8): where only SOME rows carry a test, grey means a third thing as well -- say so
    # rather than let the reader take an untested cell for a tested-and-null one (the html tooltip shows
    # which is which: an untested cell has no "gap:" line).
    if (isTRUE(spec$partial_test))
      note <- paste0(note, " ", gettext("Some rows carry no test and are left uncoloured."))
    toks <- c(toks, list(.lg_tok(paste0(" ", note))))
  }
  else if (identical(spec$policy, "guaranteed_effect"))
    # Last Phase z4: the absolute-threshold reading (contrib's standardized residual) grades the
    # quantity itself, so its note names the significance threshold rather than a subtracted margin.
    toks <- c(toks, list(.lg_tok(paste0(" ", if (isTRUE(spec$txt$guar_abs)) gettextf(
      "Grey: below the significance threshold (%s). The thresholds above are comparable between tables.",
      spec$method_phrase) else gettextf(
      "Grey: not significantly different from %s after the margin of error.", spec$ref_phrase)))))
  # Last Phase z8: the note above states ONE comparison (the text channel's). A gap measure on the
  # background compares something else, by a test of its own, so it needs one clause of its own --
  # otherwise the note silently claims the model's interval greyed the fill.
  # Last Phase z8-B: the gate reads the BACKGROUND's own resolved policy, not `spec$policy` (which is
  # the TEXT channel's). They differ exactly when the background is a gap measure with no test in this
  # column -- and there the clause was claiming a greying rule that was never applied (a false sentence
  # shipping since z5, when `adjustment` was pinned to `ignore` while the text channel was not).
  if (!identical(spec$plan_bg$policy, "ignore") &&
      !is.null(spec$plan_txt) && !is.null(spec$plan_bg) &&
      !is.null(spec$bg) && !is.na(spec$bg$ref_lead)) {
    toks <- c(toks, list(.lg_tok(paste0(" ", gettextf(
      "Background: the same rule, applied to the gap with %s (%s).",
      spec$bg$ref_lead, spec$bg$method_phrase)))))
  }
  if (!is.null(spec$caveat)) toks <- c(toks, list(.lg_tok(paste0(" ", spec$caveat))))
  toks
}

# ---- render a token stream for one medium ----------------------------------------------------------
# "runs" -> a list of runs list(text=, color=, bold=); every other medium -> a single string.
# Phase 14c: coloured break-words carry the visual weight of the coloured numbers they describe.
# Phase g refines the weight rule per token:
#   - TEXT-colour break-words stay BOLD (they mirror text-coloured cells, which the engines bold);
#   - BACKGROUND-colour break-words are PLAIN (they mirror filled cells, where a fill alone bolds
#     nothing) -- so a background legend reads in normal weight;
#   - variable NAMES (plain tokens flagged `b = TRUE`) are BOLD in every medium.
# The md branch also backslash-escapes `*` in plain-token text (the significance-stars legend), so
# pandoc/quarto does not read `***`/`*` as emphasis markup.
legend_render_line <- function(tokens, medium, theme, colored, classes = FALSE) {
  # Phase 13d: `theme` may be the render intent "auto"; a palette is always light/dark. Without this,
  # get_color_style() builds the key "text_auto", finds no palette and errors on a length-0 vector.
  pal <- tx_palette_theme(theme)
  # Phase 14c: a "runs" medium draws TEXT and cannot fill, so a background break-word borrows the
  # darker bg_legend palette (the fills themselves are invisible on the white page a run sits on).
  # Phase 14l: the text channel is the "text" family (the color_type override is gone). Hard-wiring it
  # also closes a latent bug -- color_type was fed here UNVALIDATED, so "bg_legend" would reach
  # get_color_style("crayon", ...) and hit its cli_abort.
  fam <- function(ch) if (identical(ch, "text")) "text"
                      else if (identical(medium, "runs")) "bg_legend" else "bg"
  slot_hex <- function(slot, ch)
    toupper(unname(get_color_style("color_code", type = fam(ch), theme = pal)[slot]))
  is_colored_tok <- function(tk) isTRUE(colored) && !is.na(tk$c) && tk$c > 0L
  # Last Phase z11: the break-word wears the SAME face as the cells it describes -- read from the
  # palette, not inferred. `(coloured & ch != "bg")` was a sixth hex/slot->bold heuristic, and here it
  # was not merely cosmetic: the html branch writes `font-weight:bold` INLINE, which beats the
  # stylesheet, so under `print` every under-representation break-word would have rendered bold while
  # its cells rendered italic -- exactly the legend/cell disagreement the slot vocabulary exists to
  # prevent. The colour palettes report bold on every text slot and nothing on the bg ones, i.e.
  # bit-for-bit the old expression.
  tok_face <- function(tk, k) {
    if (!is_colored_tok(tk)) return(FALSE)
    isTRUE(get_color_style("face", type = fam(tk$ch), theme = pal)[[k]][tk$c])
  }
  is_bold_tok  <- function(tk) tok_face(tk, "bold") || isTRUE(tk$b)
  semantic     <- fmt_face_semantic(pal)
  is_ital_tok  <- function(tk) tok_face(tk, "italic")
  is_under_tok <- function(tk) tok_face(tk, "underline")
  if (identical(medium, "runs")) {
    return(lapply(tokens, function(tk) {
      col <- if (is_colored_tok(tk)) slot_hex(tk$c, tk$ch) else NA_character_
      list(text = tk$t, color = col, bold = is_bold_tok(tk),
           italic = is_ital_tok(tk), underline = is_under_tok(tk))
    }))
  }
  parts <- vapply(tokens, function(tk) {
    bold <- is_bold_tok(tk); ital <- is_ital_tok(tk); und <- is_under_tok(tk)
    if (!is_colored_tok(tk)) {
      # plain token: a variable name (bold) or footer text (stars, weight line...). The stars token is
      # `esc`-flagged: escape `*` so pandoc does not read `***`/`*` as emphasis (user subtext is left raw).
      # DESIGN: the html medium needs it too -- a knitted page's raw-html block goes THROUGH pandoc
      # (Rmd -> md -> html on pkgdown/Quarto), whose markdown-in-html parsing pairs the legend's
      # `***: ... **: ... *:` runs as emphasis and swallows the stars (the Viewer, jamovi and a
      # standalone file never re-parse, so they were unaffected). `&#42;` renders as `*` in every
      # browser but is never an emphasis delimiter to pandoc (it round-trips it as an escaped literal,
      # the same path that keeps the in-cell stars alive).
      # Last Phase z8: the html arm also entity-encodes `&` and `<` -- the interaction line carries
      # p-values like "<0.01%", and a bare `<` in a raw-html footer is at the mercy of whatever parser
      # or sanitizer sees it next. `&` FIRST, or it would double-escape the `&#42;` written just after.
      txt <- tk$t
      if (identical(medium, "md")   && isTRUE(tk$esc)) txt <- gsub("*", "\\*", txt, fixed = TRUE)
      if (identical(medium, "html") && isTRUE(tk$esc)) {
        txt <- gsub("&", "&amp;", txt, fixed = TRUE)
        txt <- gsub("<", "&lt;" , txt, fixed = TRUE)
        txt <- gsub("*", "&#42;", txt, fixed = TRUE)
      }
      if (!bold) return(txt)
      if (identical(medium, "console")) return(cli::style_bold(txt))
      if (identical(medium, "html"))    return(paste0("<b>", txt, "</b>"))
      if (identical(medium, "md"))      return(paste0("**", txt, "**"))
      return(txt)
    }
    if (identical(medium, "console")) {
      # `theme` is an argument, so the palette must follow it -- reading the option here silently
      # rendered a legend the caller never asked for (it disagreed with slot_hex above).
      style <- get_color_style("crayon", type = fam(tk$ch), theme = pal)[[tk$c]]
      out <- style(tk$t)
      if (bold) out <- cli::style_bold(out)
      if (ital) out <- cli::style_italic(out)
      if (und)  out <- cli::style_underline(out)
      out
    } else if (identical(medium, "html")) {
      # DESIGN: the span is emitted inline rather than via kableExtra::text_spec() (byte-unstable across
      # kableExtra releases). Legend tokens are package-generated ("+5", "x2", "1/1.5"), so they need no
      # escaping. Phase 13d: `classes` = "our stylesheet ships with this output" (the html engine) -> the
      # break-word carries a slot CLASS (theme-toggle-safe in the table's <tfoot>); kableExtra keeps hex.
      # Phase g: weight is per-channel -- `font-weight:bold` only on the text channel (the .o*/.u* bg
      # classes are deliberately not bold, mirroring filled cells, which a fill alone does not bold).
      # z11: the face is the palette's, so a monochrome break-word says italic/underline rather than
      # colour. `font-weight` is stated EXPLICITLY when the palette says not-bold -- this span is
      # inline, so it must override the stylesheet's own `.p1..m4{font-weight:bold}` baseline.
      wt <- if (bold) "font-weight:bold;" else if (identical(tk$ch, "text")) "font-weight:normal;" else ""
      if (ital) wt <- paste0(wt, "font-style:italic;")
      if (und)  wt <- paste0(wt, "text-decoration:underline;")
      # z11: a palette whose meaning is TYPOGRAPHY writes the break-word as markup too, exactly as the
      # cells do -- a sanitizer that strips `class` and `style` (GitHub) or a paste into Word keeps the
      # tags, so the legend still describes itself. No-op under the colour palettes.
      lab <- if (semantic) html_face_wrap(tk$t, bold, ital, und) else tk$t
      if (isTRUE(classes)) {
        cls <- tx_slot_class(tk$ch, tk$c)
        if (identical(tk$ch, "text"))
          paste0("<span class=\"", cls, "\" style=\"", wt, "\">", lab, "</span>")
        else paste0("<span class=\"", cls, "\" style=\"", wt, "border-radius:4px;",
                    "padding-right:4px;padding-left:4px;\">", lab, "</span>")
      } else {
        hex <- slot_hex(tk$c, tk$ch)
        if (identical(tk$ch, "text"))
          paste0("<span style=\"", wt, "color:", hex, " !important;\">", lab, "</span>")
        else
          paste0("<span style=\"", wt, "background-color:", hex,
                 " !important;border-radius:4px;padding-right:4px;padding-left:4px;\">",
                 lab, "</span>")
      }
    } else if (identical(medium, "md")) {
      # `**` on top of the .p*/.m* stylesheet bold makes the TEXT break-words stand out in the RAW
      # markdown too; the .o*/.u* background channel is plain (Phase g) -> bracketed span without `**`.
      # z11: a monochrome palette's under-side is ITALIC, so the raw markdown says `*[..]{.m1}*`.
      cls <- tx_slot_class(tk$ch, tk$c)
      if (!nzchar(cls)) tk$t
      else {
        out <- paste0("[", tk$t, "]{.", cls, "}")
        if (ital) out <- paste0("*", out, "*")
        if (bold) out <- paste0("**", out, "**")
        out
      }
    } else tk$t
  }, character(1))
  paste0(parts, collapse = "")
}

# ---- build the per col_var specs -------------------------------------------------------------------
#' @keywords internal
legend_specs <- function(x, theme = "light") {
  is_f <- purrr::map_lgl(x, is_fmt)
  ct   <- get_color(x); cbg <- get_color_bg(x)
  keep <- is_f & ((!is.na(ct)  & !ct  %in% c("no", "")) |
                  (!is.na(cbg) & !cbg %in% c("no", "")))
  if (!any(keep)) return(list())

  col_vars_levels <- tab_get_vars(x)$col_vars_levels
  col_vars_levels <- col_vars_levels[names(col_vars_levels) != "all_col_vars"]
  kept_names <- names(x)[keep]

  meta   <- get_reg_meta(x)
  is_reg <- !is.null(meta)                            # Phase 14w: robust, survives footer materialisation
  cis    <- get_ci_settings(x); if (is.null(cis)) cis <- default_ci_settings()
  shades <- legend_shade_names(theme)
  # Phase 16d: the mean_diff scale in force (pushed per render). Its `std` flag decides whether a numeric
  # mean / regression-coef diff is sd-standardized (SD units) or raw (custom breaks -> std FALSE). This
  # is the SAME source fmt_color_plan() reads, so the legend can never disagree with the cells.
  mean_diff_std <- isTRUE(color_scales(NULL)$mean_diff$std)

  # One spec per colored column (was one per col_var), so several measures sharing a col_var -- a reg
  # table's model + empirical columns under one outcome span (Phase 14w) -- each get their own spec.
  # legend_group_by_body() below collapses columns with an IDENTICAL rendered body, so a crosstab's level
  # columns still fold to one line (byte-identical legends): same body -> one group -> the col_var prefix.
  reps <- purrr::imap(col_vars_levels, function(cols, cv) {
    cc <- cols[cols %in% kept_names]
    purrr::map(cc, function(cn) list(cn = cn, cv = cv))
  })
  reps <- purrr::flatten(purrr::compact(reps))
  if (length(reps) == 0) return(list())

  # Build the rich specs. For a reg table the empirical companion + model columns describe the SAME colour
  # scale but differ superficially (the emp/model `role`, the additive effect word "AME"/"beta" vs the
  # neutral "cells", and a recovered-vs-NA reference label). Those are reconciled per col_var
  # (legend_reg_adapter) so the emp + model bodies MATCH and one legend line (legend_group_by_body)
  # covers both. A crosstab is untouched (role uniformly "model").
  specs <- purrr::map(reps, function(e) {
    cn <- e$cn; cv <- e$cv
    col      <- x[[cn]]
    # Phase 16c: same cross-channel arbiter as the cells (drops a degenerate guaranteed_effect channel),
    # so a disabled channel loses its legend line too.
    pl       <- resolve_color_channel_plans(col)
    plan_txt <- pl$text
    plan_bg  <- pl$bg
    if (is.null(plan_txt) && is.null(plan_bg)) return(NULL)
    type     <- get_type(col)
    is_coef  <- identical(type, "coef")
    is_mean  <- type %in% c("mean", "n")
    # Phase 16d: three diff "kinds" -- factor pct (x100, "points"), numeric/coef STANDARDIZED (SD) and
    # numeric/coef RAW (custom mean_diff breaks: as-is, no unit). is_pct drives the x100; is_std drives
    # the "SD"/"standardized" wording. Reading mean_diff_std keeps the legend and the cells consistent.
    is_num   <- is_mean || is_coef
    is_pct   <- !is_num
    # Phase g: a NON-gaussian coefficient (exponentiate = FALSE) colours on the LOGGED odds_ratio scale
    # (log_odds_scale), NOT the SD-standardized mean_diff -- so its legend must NOT say "SD" (the breaks
    # are log-odds/log-rate units). A gaussian beta keeps is_std (var(Y)-standardized). Last Phase z3:
    # this and fmt_color_plan's gate now SHARE reg_fam_logscale() -- they cannot drift apart any more.
    is_logcoef <- is_coef && reg_fam_logscale(get_model_family(col))
    is_std   <- is_num && mean_diff_std && !is_logcoef
    policy   <- if (!is.null(plan_txt)) plan_txt$policy else plan_bg$policy
    m_txt    <- if (!is.null(plan_txt)) plan_txt$measure else NA_character_
    m_bg     <- if (!is.null(plan_bg))  plan_bg$measure  else NA_character_
    orient   <- if (identical(type, "col")) "col" else "row"
    eff_word <- if (isTRUE(is_reg)) legend_reg_eff_word(col, meta) else NA_character_
    # Phase 17c: the emp/model split reads the column's STORED `role` attr (written by the reg builders),
    # not the "Emp." name prefix. Fall back to "model" if an old/hand-built reg column lacks it.
    role     <- if (isTRUE(is_reg)) { r <- get_role(col); if (nzchar(r)) r else "model" } else "model"
    ref      <- legend_ref_info(x, col, m_txt, orient, is_coef = is_coef, is_reg = is_reg,
                                policy = policy)
    ci_type  <- get_ci_type(col)
    # Last Phase z13 (D8): does this column carry a test on SOME rows only? A gap measure's SE is
    # missing wherever it could not be computed -- a group with an empty cell yields an infinite log
    # interval, a profile bracket is not est +/- crit*se -- and those rows then render exactly like a
    # tested-and-non-significant one. The colours are right (the policy promises "coloured => shown
    # significant", which an untested cell is not), but the grey NOTE must not claim they were all
    # tested. A per-column fact, so a fully-tested column's legend is byte-unchanged.
    # Last Phase z13 (D7): this column has no baseline to be compared to -- so it IS the baseline.
    no_obs      <- all(is.na(get_obs(col)))
    gse         <- get_gap_se(col)
    partial_test <- !identical(policy, "ignore") &&
      any(c(m_txt, m_bg) %in% c("adjustment", "between_groups"), na.rm = TRUE) &&
      any(is.na(gse)) && any(!is.na(gse))
    list(col_var = cv, col_name = cn, plan_txt = plan_txt, plan_bg = plan_bg,
         partial_test = partial_test, no_obs = no_obs,
         measure_text = m_txt, measure_bg = m_bg,
         is_mean = is_mean, is_std = is_std, is_pct = is_pct, is_coef = is_coef,
         policy = policy, orientation = orient, ci_type = ci_type,
         is_reg = is_reg, eff_word = eff_word, role = role, ci_settings = cis, shades = shades,
         theme = theme,
         model_family = get_model_family(col),        # Last Phase z5: the collapsibility caveat below
         ref = ref)
  })
  specs <- purrr::compact(specs)
  if (length(specs) == 0) return(list())

  if (isTRUE(is_reg)) specs <- legend_reg_adapter(specs)
  specs
}

# Phase 16e: group the per-column specs into legend lines by their RENDERED BODY (the name-less token
# stream for the style in force), replacing the hand-maintained 10-field `sig` string. Two specs share a
# line iff they render the same body -- so grouping can NEVER drift from what actually prints (the 16d
# is_pct-in-sig patch was exactly such a drift: a fact that changes the body but was forgotten in the
# sig). Grouping is style-local (terse vs prose may fold differently -- correct: each medium groups by
# what it shows), and groups keep first-occurrence (column-reading) order.
legend_group_by_body <- function(specs, style, lang) {
  body_of <- function(s) {
    toks <- if (identical(style, "prose")) legend_tokens_prose(s, lang, FALSE)
            else                           legend_tokens_terse(s, lang, FALSE)
    paste0(vapply(toks, function(tk) tk$t, character(1)), collapse = "")
  }
  bodies <- vapply(specs, body_of, character(1))
  lapply(unique(bodies), function(k) specs[bodies == k])
}

# Phase 16d: reconcile the empirical + model specs of each col_var of a REG table so they fold into one
# legend line. (1) SHARE the reference label -- a single-predictor empirical column knows the baseline
# label ("Other"), the model recovers NA (ambiguous); when a col_var has exactly one distinct non-NA
# label, apply it to every spec (correct: the treatment-contrast baseline IS the reference). (2)
# NEUTRALISE the additive effect word -- when a col_var carries BOTH an empirical AND a model additive
# ("diff") column sharing policy/is_std/orientation, drop the model's "AME"/"beta" subject to the neutral
# "cells" the empirical companion already uses, so their bodies match. The effect identity survives in
# the column-name prefix + the "Model:" line. Multiplicative OR/IRR keep their word (both siblings carry
# it, and legend_method_name reads it for the "rate-ratio" vs "odds-ratio" wording). A no-empirical
# AME/beta/MER table is left untouched (no sibling to fold with), keeping its effect word in the body.
legend_reg_adapter <- function(specs) {
  by_cv <- split(seq_along(specs), purrr::map_chr(specs, "col_var"))
  for (idx in by_cv) {
    labs <- unique(stats::na.omit(vapply(specs[idx], function(s) s$ref$label, character(1))))
    if (length(labs) == 1L) for (i in idx) specs[[i]]$ref$label <- labs
    for (i in idx) {
      s <- specs[[i]]
      if (identical(s$role, "model") && identical(s$measure_text, "diff") && !is.na(s$eff_word)) {
        has_emp <- any(vapply(specs[idx], function(o)
          identical(o$role, "emp") && identical(o$measure_text, "diff") &&
          identical(o$policy, s$policy) && identical(o$is_std, s$is_std) &&
          identical(o$orientation, s$orientation), logical(1)))
        if (has_emp) specs[[i]]$eff_word <- NA_character_
      }
    }
  }
  specs
}

# Phase 16d: render a colour-legend column-name prefix. Normalises each name (undo the html-path wrap --
# <br>/\n/U+202F narrow-no-break-space -> space, squish), then protects intra-name spaces with a
# no-break space so no medium re-breaks a name mid-word (pillar's strwrap on the console, the wrapped
# rd$tab on the kable path), joins with a breakable ", ", and caps the list at `max_n` + "... +N vars".
legend_name_list <- function(names, max_n = 6L, lang = "en") {
  norm <- vapply(names, function(nm) {
    nm <- gsub("<br>|\n|\u202f", " ", nm)                  # undo html-path wrap markers
    nm <- trimws(gsub("[[:space:]]+", " ", nm))
    gsub(" ", "\u00a0", nm)                                # protect intra-name spaces (no-break)
  }, character(1), USE.NAMES = FALSE)
  extra <- length(norm) - max_n
  if (extra > 0L) norm <- c(utils::head(norm, max_n), gettextf("\u2026 +%d vars", extra))
  paste(norm, collapse = ", ")
}

# Phase 16e: the colour-legend token streams (one per body-group), UNRENDERED. The grouping + prefix logic
# factored out of tab_color_legend so BOTH tab_color_legend (renders per medium) and tab_footer_streams
# (concatenates them with the plain footer lines) share ONE legend core. Returns a list of token-lists,
# empty when nothing is coloured. Built under the render language (with_legend_lang). The specs are resolved
# ONCE here (legend_resolve_spec), so grouping-by-body and the assemblers both read plain fields.
legend_streams <- function(x, style, lang, theme = "light") {
  with_legend_lang(lang, function(lg) {
    # z11: `theme` reaches here for ONE reason -- the shade NAMES a palette gives its two directions
    # ("Shades of blue" / "Bold"). Everything else the legend needs is theme-free.
    specs <- legend_specs(x, theme)
    if (length(specs) == 0) return(list())
    specs <- lapply(specs, function(s) legend_resolve_spec(s, lg))
    grp   <- legend_group_by_body(specs, style, lg)
    show_global <- length(grp) > 1
    # Phase 14w: a col_var spawning SEVERAL legend lines (a reg outcome span -> model + empirical) is
    # prefixed by the COLUMN names (the col_var alone is ambiguous); a single-line col_var keeps its name.
    cv_lines <- table(unlist(lapply(grp, function(g) unique(purrr::map_chr(g, "col_var")))))
    lapply(grp, function(g) {
      spec <- g[[1]]
      cvs  <- unique(purrr::map_chr(g, "col_var"))
      # Phase 16d: a role-MIXED group (empirical + model merge) always shows a prefix and names the COLUMNS
      # (Emp. OR, Model OR) -- the col_var alone is ambiguous, and the prefix now carries the folded effect
      # identity. A role-uniform group (a crosstab, one multinomial span) keeps the old rule.
      mixed       <- length(unique(purrr::map_chr(g, "role"))) > 1
      show_this   <- show_global || mixed
      name_by_col <- mixed || any(cv_lines[cvs] > 1)
      spec$col_names <- if (name_by_col) unique(purrr::map_chr(g, "col_name")) else cvs
      # Last Phase m: a multi-dependent regression column carries a trailing " [dep]" disambiguation
      # bracket in its NAME ("Model_OR [married]") for console clash-avoidance. The col_var span row
      # already names the outcome, so the legend strips the bracket (same regex as the header strip,
      # tab-export-prep.R). Gated to reg groups (columns carry a role) so a level label that happens to
      # end in "[...]" is untouched.
      if (any(nzchar(purrr::map_chr(g, "role"))))
        spec$col_names <- sub(" \\[[^]]*\\]$", "", spec$col_names)
      if (identical(style, "prose")) legend_tokens_prose(spec, lg, show_this)
      else                           legend_tokens_terse(spec, lg, show_this)
    })
  })
}

# Phase 16e: render a list of RAW token-streams for one medium -> a character vector (one string per
# stream), or, for "runs", a list of run-lists (Excel / plot). enc2utf8 guards the gettext catalog output.
render_streams <- function(streams, medium, theme, colored, classes = FALSE) {
  if (identical(medium, "runs")) {
    return(unname(lapply(streams, function(toks)
      lapply(legend_render_line(toks, "runs", theme, colored, classes),
             function(r) { r$text <- enc2utf8(r$text); r }))))
  }
  enc2utf8(vapply(streams, function(toks)
    legend_render_line(toks, medium, theme, colored, classes), character(1)))
}

#' Build the colour legend of a table
#'
#' Internal. Returns one legend line per colour-signature group. For \code{medium = "runs"} each line
#' is a list of runs \code{list(text, color, bold)}; otherwise a character string.
#' @param x A \code{tabxplor_tab}.
#' @param medium One of "console", "html", "md", "runs", "plain". \code{"runs"} is for the media that
#'   draw the legend as coloured TEXT and cannot fill: an Excel rich-text cell (\code{\link{tab_xl}})
#'   and a \pkg{ggpubr} label (\code{\link{tab_plot}}). It returns the runs unrendered, and draws the
#'   background channel from the darker \code{bg_legend} palette (see \code{\link{set_color_palette}}).
#' @param style "terse" (compact, console default) or "prose" (full sentences, export default).
#' @param lang NULL (auto from locale) / "en" / "fr".
#' @param colored Whether to colour the break-words.
#' @param theme Palette theme (default from options).
#' @param classes `medium = "html"` only: emit the break-words as CSS slot classes rather than inline
#'   hex, because a tabxplor stylesheet ships with the output (`tab_kable(engine = "html")`). Then the
#'   legend follows a theme toggle exactly like the cells it describes. `FALSE` (the kableExtra engine,
#'   which carries no stylesheet of ours) keeps inline hex.
#' @return A character vector (or, for "runs", a list of run-lists), or NULL when nothing is coloured.
#' @keywords internal
tab_color_legend <- function(x, medium = c("console", "html", "md", "runs", "plain"),
                             style = NULL, lang = NULL, colored = TRUE,
                             theme = NULL, classes = FALSE) {
  medium <- match.arg(medium)
  if (is.null(style))      style      <- if (identical(medium, "console")) "terse" else "prose"
  if (is.null(theme))      theme      <- tx_getOption(c("tabxplor.console_theme", "tabxplor.color_style_theme"), "light")
  streams <- legend_streams(x, style, lang, theme)
  if (length(streams) == 0) return(NULL)
  render_streams(streams, medium, theme, colored, classes)
}
# tab_color_legend(tabs[[7]], medium = "console") |> cli::cat_line()

# Phase 16d: run f(lg) with LANGUAGE set for the gettext lookups (flushing glibc's cache before/after,
# mirroring tab_color_legend). Shared by the plain-text footer helpers below (stars / weight legend),
# which are not coloured so they need no per-medium renderer -- they return one plain string.
with_legend_lang <- function(lang, f) {
  lg  <- legend_resolve_lang(lang)
  old <- Sys.getenv("LANGUAGE", unset = NA_character_)
  flush_gettext_cache(); Sys.setenv(LANGUAGE = lg); flush_gettext_cache()
  on.exit({
    if (is.na(old)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = old)
    flush_gettext_cache()
  }, add = TRUE)
  f(lg)
}

# DESIGN: every footer line is a TOKEN STREAM -- there is no plain-vs-legend kind split, because
# legend_render_line() already renders uncoloured tokens, so a plain one-liner is just a 1-token stream.
# One renderer (render_footer) therefore covers the whole footer; the `role` only picks the console subtle.
# Phase 16e: THE ordered below-table footer of a table, as a list of typed token-streams -- one shared
# definition of what goes below a table and in what order (weight -> Model: -> colour-legend group(s) ->
# stars -> user subtext), replacing the 5x per-backend re-ordering + the 2x export-prep field pre-compute.
# Every line is a token-stream: the plain one-liners (weight / Model / stars / subtext, each self-
# translated) wrap as a single .lg_tok, the colour legend contributes its unrendered groups. Each stream
# carries a `role` so render_footer() can subtle the plain lines whole while a legend keeps its colours.
# `subtext` = the user subtext lines (backend-specific: the console/md/html pass it, Excel keeps user
# subtext on its own plain rows below the rich-text legend and passes character(0)); `legend = FALSE`
# drops the colour legend (a backend's color_legend = FALSE).
# Phase 16e: the legend style for EXPORTS (md / html / Excel). Default "prose" (full sentences); a user can
# set options(tabxplor.legend_style = "terse") for the compact console-style one-liner. The console itself
# always uses "terse" (a terminal is width-bound). Any value but "terse" resolves to "prose".
legend_export_style <- function() {
  if (identical(getOption("tabxplor.legend_style", "prose"), "terse")) "terse" else "prose"
}

tab_footer_streams <- function(x, style = "prose", lang = NULL,
                               subtext = character(0), legend = TRUE, theme = "light") {
  lg      <- legend_resolve_lang(lang)
  streams <- list()
  push <- function(tokens, role) if (length(tokens))
    streams[[length(streams) + 1L]] <<- list(tokens = tokens, role = role)
  wl <- tab_weight_line(x, lang = lg);   if (!is.null(wl)) push(list(.lg_tok(wl)), "weight")
  for (rl in reg_model_lines(x, lg)) if (nzchar(rl)) push(list(.lg_tok(rl)), "reg")  # Last Phase w: translated per family
  # Last Phase z8: the aggregated effect-modification test (predictor x split_var) -- table-wide, so it
  # rides the stream footer like the weight / Model: lines rather than the per-column footer rows.
  # `esc = TRUE`: the p-values carry significance stars, which pandoc would read as emphasis.
  for (il in reg_interaction_lines(x, lg)) if (nzchar(il)) push(list(.lg_tok(il, esc = TRUE)), "reg")
  # (Last Phase z15: the per-predictor global test used to push a line here too. It is footer ROWS now
  # -- see reg_footer_plan() -- because it belongs to one model column and a line could not say which.)
  if (isTRUE(legend)) for (toks in legend_streams(x, style, lg, theme)) push(toks, "legend")
  # Phase g: `esc = TRUE` -> the md renderer escapes the `*` glyphs (else pandoc reads them as emphasis).
  sl <- suppressWarnings(tab_stars_legend(x, lang = lg)); if (!is.null(sl)) push(list(.lg_tok(sl, esc = TRUE)), "stars")
  for (s in subtext) if (nzchar(s)) push(list(.lg_tok(s)), "subtext")
  streams
}

# Phase 16e: render the footer streams for one medium. Console applies the "# " subtle prefix per line,
# role-aware: a legend keeps its coloured break-words (only the prefix is subtle), every other line is
# subtle whole (matching the historical tbl_format_footer wrapping). Other media return the rendered
# character vector (md/html/plain) or run-lists (runs); the caller places them (tfoot, xl rows, ...).
render_footer <- function(streams, medium, theme = NULL, colored = TRUE, classes = FALSE) {
  if (is.null(theme)) theme <- tx_getOption(c("tabxplor.console_theme", "tabxplor.color_style_theme"), "light")
  if (length(streams) == 0) return(if (identical(medium, "runs")) list() else character(0))
  toks_list <- lapply(streams, function(s) s$tokens)
  out <- render_streams(toks_list, medium, theme, colored, classes)
  if (identical(medium, "console")) {
    roles <- vapply(streams, function(s) s$role, character(1))
    out <- ifelse(roles == "legend",
                  paste0(pillar::style_subtle("# "), out),
                  pillar::style_subtle(paste0("# ", out)))
  }
  out
}

# Phase 16d: the significance-stars legend line, shown when any DISPLAYED, star-applicable fmt column
# carries a star (so it appears alongside `stars = TRUE` crosstabs and reg tables, never on a contrib
# table -- fmt_stars_applicable). Thresholds/labels come from the same options get_stars() reads, so the
# named confidence levels always match the glyphs actually drawn. Returns one plain string (uncoloured),
# or NULL. `lang` NULL -> auto.
tab_stars_legend <- function(x, lang = NULL) {
  cols <- purrr::keep(x, ~ is_fmt(.) && fmt_stars_applicable(.))
  if (length(cols) == 0) return(NULL)
  if (!any(vapply(cols, function(cl) any(nzchar(get_stars(cl))), logical(1)))) return(NULL)
  with_legend_lang(lang, function(lg) {
    lev  <- sort(getOption("tabxplor.signif_levels", c(0.10, 0.05, 0.01)))     # ascending p
    lab  <- getOption("tabxplor.signif_labels", c("*", "**", "***"))
    lab  <- lab[order(nchar(lab), decreasing = TRUE)]                          # most stars first
    conf <- (1 - lev) * 100                                                    # aligned: *** <-> 99%
    semi <- if (identical(lg, "fr")) " ; " else "; "
    # Last Phase z13 (D11): a REGRESSION table's stars do not all test "vs the reference category" --
    # the `Constant` row has no reference category, and its star tests the baseline value itself (odds
    # of 1, a beta of 0). One wording that is true of every starred row, keyed on the stored reg
    # metadata rather than on a row label.
    first <- if (is_reg_footer(get_test(x))) gettextf(
      "%s: significantly different from no effect (the reference category in bold; for the Constant, the null value) at the %s%% confidence level",
      lab[1], legend_num(conf[1], lg))
    else gettextf(
      "%s: significantly different from the reference category (in bold) at the %s%% confidence level",
      lab[1], legend_num(conf[1], lg))
    rest <- if (length(lab) > 1)
      vapply(2:length(lab), function(i) gettextf("%s: at the %s%% level", lab[i],
                                                 legend_num(conf[i], lg)), character(1))
    else character(0)
    none <- gettext("no star: not significant")
    enc2utf8(paste0(paste(c(first, rest, none), collapse = semi), "."))
  })
}

# Phase 16d: the "Weighted by <wt>." footer line (FR "Pondere par <wt>."), shown FIRST in the footer
# when the table was built with a weight. The weight column NAME is persisted on the table (the `vars`
# attribute for a crosstab, `reg_meta` for a regression); NULL when unweighted. Returns one plain string.
tab_weight_line <- function(x, lang = NULL) {
  wt <- get_vars_attr(x)$wt
  if (is.null(wt) || length(wt) == 0L || is.na(wt) || !nzchar(wt))
    wt <- tryCatch(get_reg_meta(x)$wt, error = function(e) NULL)
  if (is.null(wt) || length(wt) == 0L || is.na(wt) || !nzchar(wt)) return(NULL)
  # Last Phase z14-i (D7/D8): a survey design passed as `data` resolves to the package-owned weight
  # name on EVERY path (tab()'s vars_attr, the tab_plain/tab_num leaves, tab_reg's reg_meta), so the
  # fact "this table is design-based" is already on the table, in one field. Read it as a fact rather
  # than printing it as a name -- the internal `.svy_weights` used to leak into user-facing output,
  # and tab_reg() under a design emitted no weight line at all.
  # Last Phase z14-ii (ruling Q7): the sentence now claims the intervals too, because Route A made
  # them design-based -- a rung-3 table has to be distinguishable from a rung-2 (Kish) one, which
  # S3.2/S3.3 measured can differ by a factor of 2 in EITHER direction. It is blanket, and since
  # z14-iii made tab_reg()'s crude Obs_* intervals design-based too it is blanket with nothing left
  # to qualify -- a table whose design variance could NOT be computed says so at build time
  # (svy_var_degraded()), so the sentence is never silently untrue.
  if (identical(as.character(wt)[1], svy_wt_col))
    return(with_legend_lang(lang, function(lg) enc2utf8(gettext(
      "Design-based (survey): weighted estimates, intervals and tests account for the sample design."
    ))))
  with_legend_lang(lang, function(lg) enc2utf8(gettextf("Weighted by %s.", wt)))
}

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



# Shared body of vec_ptype_abbr/vec_ptype_full (Phase 17a). The two differ only by the label `prefix`
# ("" for abbr, "fmt-" for full) -- which also flips the doubled-type / trailing-NA collapse anchor
# from "^" to the prefix's trailing "-" -- and by `pct_pvalue_collapse` (a pct/pvalue composite shows
# as "pct" in the abbreviation only). Phase 10i-A: a composite column shows its PRIMARY type.
fmt_ptype_label <- function(x, prefix, pct_pvalue_collapse) {
  display <- display_primary(get_display(x)) |> unique()
  if (pct_pvalue_collapse && identical(sort(display), c("pct", "pvalue"))) display <- "pct"
  display <- ifelse(length(display) > 1, "mixed", display)
  type    <- get_type(x)
  if (type %in% c("row", "col", "all", "all_tabs")) type <- paste0(type, "%")
  ci <- get_ci_type(x)
  if (display == "ci" & ci %in% c("cell", "diff")) display <- paste0("ci_", ci)

  pat_anchor <- if (nzchar(prefix)) "-" else "^"   # boundary before a doubled "<t>-<t>"
  rep_anchor <- if (nzchar(prefix)) "-" else ""
  out <- paste0(prefix, type, "-", display)
  for (t in c("n", "mean", "coef", "mixed")) {     # Phase 12c added "coef"
    out <- stringi::stri_replace_first_regex(out, paste0(pat_anchor, t, "-", t),
                                             paste0(rep_anchor, t))
  }
  out |>
    stringi::stri_replace_first_regex("([^%]+%)-pct", "$1") |>
    stringi::stri_replace_first_regex(paste0(pat_anchor, "NA"), "") |>
    stringi::stri_replace_first_regex("_ci$", "")
}

#' Abbreviated display name for class fmt in tibbles
#' @param x A fmt object.
#' @param ... Other parameter.
#' @return A single string with abbreviated fmt type.
#' @export
#' @keywords internal
vec_ptype_abbr.tabxplor_fmt <- function(x, ...) {
  fmt_ptype_label(x, prefix = "", pct_pvalue_collapse = TRUE)
}


#' Printed type for class fmt
#' @param x A fmt object.
#' @param ... Other parameter.
#' @return A single string with full fmt type.
#' @export
#' @keywords internal
vec_ptype_full.tabxplor_fmt <- function(x, ...) {
  fmt_ptype_label(x, prefix = "fmt-", pct_pvalue_collapse = FALSE)
}
# x <- fmt(7, "row", pct = 0.6)
# x |> vec_data()
# x |> attributes()

#Coertion and convertion methods for formatted numbers -------------------------

#Make our tabxplor_fmt class coercible with herself, and back and forth with double and
# integer vectors :
#' Find common ptype between fmt and fmt
#' @param x A fmt object.
#' @param y A fmt object.
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
#' @keywords internal
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
  mf_x         <- get_model_family(x)
  same_mf      <- mf_x == get_model_family(y)
  role_x       <- get_role(x)
  same_role    <- role_x == get_role(y)
  # Last Phase z13: the RAW attribute, so binding two columns that never recorded a level keeps
  # "unknown" instead of freezing today's option into the result. Two NAs compare NA, which a bare
  # `if ()` would ERROR on -- the `same_comp` trap two lines below, in its second instance.
  cl_x         <- fmt_conf_level_attr(x)
  cl_y         <- fmt_conf_level_attr(y)
  same_cl      <- (is.na(cl_x) && is.na(cl_y)) || isTRUE(cl_x == cl_y)
  #l            <- length(x)

  # Phase 9c: the reconcile is scalar-attribute picking; base-R if/else replaces the 9 dplyr::if_else
  # (~3x faster per call, byte-identical). This method drives EVERY c()/vec_c()/bind/group over fmt
  # columns -- the compact merge's per-column vec_ptype_common() reduce is the hottest caller (the
  # entire tab() merge marginal, dev/tabxplor_2.0.0_decisions.md 30). WARNING: every same_* is a
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
    color_signif = if (same_signif) signif_x else "ignore",
    model_family = if (same_mf) mf_x else "",
    role         = if (same_role) role_x else "",
    conf_level   = if (same_cl) cl_x else NA_real_
  )
}
#' Find common ptype between fmt and double
#' @param x A fmt vector
#' @param y A double vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
#' @keywords internal
vec_ptype2.tabxplor_fmt.double  <- function(x, y, ...) x # new_fmt() #double()
#' Find common ptype between double and fmt
#' @param x A double vector
#' @param y A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
#' @keywords internal
vec_ptype2.double.tabxplor_fmt  <- function(x, y, ...) y # new_fmt() #double()
#' Find common ptype between fmt and integer
#' @param x A fmt vector
#' @param y An integer vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
#' @keywords internal
vec_ptype2.tabxplor_fmt.integer <- function(x, y, ...) x # fmt() #double()
#' Find common ptype between integer and fmt
#' @param x An integer vector
#' @param y A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
#' @keywords internal
vec_ptype2.integer.tabxplor_fmt <- function(x, y, ...) y # new_fmt() #double()

# Conversions :
#' Convert fmt into fmt
#' @param x A fmt vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
#' @keywords internal
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
          n_eff     = get_n_eff   (x),
          obs       = get_obs     (x),
          gap_se    = get_gap_se  (x),

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
          color_signif = get_color_signif(to),
          model_family = get_model_family(to),
          role         = get_role(to),
          conf_level   = fmt_conf_level_attr(to)

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
#' @keywords internal
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
      model_family = get_model_family(to),
      role         = get_role(to),
      conf_level   = fmt_conf_level_attr(to),

  )
#' Convert fmt into double
#' @param x A fmt vector
#' @param to A double vector
#' @param ... Other parameter.
#' @return A double vector
#' @method vec_cast.double tabxplor_fmt
#' @export
#' @keywords internal
vec_cast.double.tabxplor_fmt  <- function(x, to, ...) get_num(x) |> as.double() #vctrs::field(x, "pct")

#' Convert integer into fmt
#' @param x A integer vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
#' @keywords internal
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
      color_signif = get_color_signif(to),
      model_family = get_model_family(to),
      role         = get_role(to),
      conf_level   = fmt_conf_level_attr(to)

  ) #new_fmt(pct = as.double(x))
#' Convert fmt into integer
#' @param x A integer vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return An integer vector
#' @method vec_cast.integer tabxplor_fmt
#' @export
#' @keywords internal
vec_cast.integer.tabxplor_fmt    <- function(x, to, ...) get_num(x) |> as.integer() #vctrs::field(x, "pct") |> as.integer()

#' Convert fmt into character
#' @param x A fmt vector
#' @param to A character vector
#' @param ... Other parameter
#' @return A character vector
#' @method vec_cast.character tabxplor_fmt
#' @export
#' @keywords internal
vec_cast.character.tabxplor_fmt  <- function(x, to, ...) format(x)

#Comparisons and sorting :
#' Test equality with fmt vector
#' @param x A fmt vector
#' @param ... Other parameter
#' @return A double vector
#' @export
#' @keywords internal
vec_proxy_equal.tabxplor_fmt   <- function(x, ...) {
  get_num(x)
}
#' Compare with fmt vector
#' @param x A fmt vector
#' @param ... Other parameter
#' @return A double vector
#' @export
#' @keywords internal
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
#' @keywords internal
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
      n       = vctrs::vec_arith_base(op, get_n(x)  , get_n(y)  ), #|> positive_integer(),
      wn      = vctrs::vec_arith_base(op, get_wn(x) , get_wn(y) ), #|> positive_double(),
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
      n_eff   = rep_NA_real,
      obs     = rep_NA_real,
      gap_se  = rep_NA_real,

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
      color_signif = get_color_signif(x),
      model_family = get_model_family(x),
      role         = get_role(x),
      conf_level   = fmt_conf_level_attr(x)
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
      n_eff  = rep_NA_real,
      obs    = rep_NA_real,
      gap_se = rep_NA_real,

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
      color_signif = get_color_signif(x),
      model_family = get_model_family(x),
      role         = get_role(x),
      conf_level   = fmt_conf_level_attr(x)
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
}

#' @describeIn vec_arith.tabxplor_fmt vec_arith method for numeric + fmt
#' @return A fmt vector
#' @method vec_arith.numeric tabxplor_fmt
#' @export
vec_arith.numeric.tabxplor_fmt <- function(op, x, y, ...) {
  set_num(y, vctrs::vec_arith_base(op, x, get_num(y)))
}

#' @describeIn vec_arith.tabxplor_fmt vec_arith method for -fmt
#' @return A fmt vector
#' @method vec_arith.tabxplor_fmt MISSING
#' @export
vec_arith.tabxplor_fmt.MISSING <- function(op, x, y, ...) { #unary + and - operators
  switch(op,
         `-` = set_num(x, get_num(x) * -1),
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
#' @keywords internal
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
                                         no  = NA_real_) |>
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
                         n_eff  = NA_real_,
                         obs    = NA_real_,
                         gap_se = NA_real_,

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
                         color        = fmt_color_attr   (.x),
                         color_signif = get_color_signif (.x),
                         model_family = get_model_family (.x),
                         role         = get_role         (.x),
                         conf_level   = fmt_conf_level_attr(.x)
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
                          n_eff   = NA_real_,
                          obs     = NA_real_,
                          gap_se  = NA_real_,

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
                          color        = fmt_color_attr   (.x),
                          color_signif = get_color_signif (.x),
                          model_family = get_model_family (.x),
                          role         = get_role         (.x),
                          conf_level   = fmt_conf_level_attr(.x)
         ),
         vctrs::vec_math_base(.fn, get_num(.x), ...) )
}


