# PURPOSE: tabxplor_fmt, the rich-cell vctrs record -- plus the colour engine and legend that read it.
# ROLE: Foundation of the package. Every numeric column of a table is an fmt vector; the colour engine
#   here is the one artifact every backend (console / HTML / Excel / Markdown / plot) consumes.
# KEY CONSTRAINTS:
#   - Fields are per-cell (vctrs::field); attributes are per-column (attr). Never confuse the two.
#   - WHAT A COLUMN ESTIMATES is STORED, not derived: `scale` is one key into EST_SCALES (which field
#     holds the estimate, its null, geometry, colour ladder, SD source); `pct_type` is which axis its
#     reference lies on; `ci_method` says which engine built the bounds. Read them through
#     fmt_scale_row() / fmt_var_kind() / get_pct_type(); never re-derive a scale from a display, a
#     family, or from whether `var` is non-NA.
#   - The record is DENSE: every column carries all fields, an inapplicable one stored as NA.
#   - pct is stored 0-1 (x100 only in format()); `diff` is always a DIFFERENCE (the ratio is `ratio`).
#   - `{est}` / `{base}` are SCALE-RELATIVE tokens: they name a role, and each column answers with the
#     token it has always rendered (EST_SCALES' est_display / base_display, resolved once by
#     fmt_resolve_scale_tokens()). That is why they need no arm of their own anywhere.
#   - ONE multiplicative rendering, outside `special_formatting` so composites keep it: a value below
#     its neutral prints as its inverse, with the MEASURE's own glyphs (MEASURES$break_over/under, the
#     pair the legend ladder and the forest axis print). options(tabxplor.ratio_print = "raw") opts out.
#   - THE BASELINE PRINTS THE BARE NEUTRAL -- "1" on a multiplicative column, "0" / "0%" on an
#     additive one -- and only a cell that IS the baseline does: one that merely ROUNDS to the neutral
#     keeps its glyph and its sign ("x1.00", "+0%"), so the reader can tell the two apart. Both rules
#     sit outside `special_formatting`, or the composite recursion would disagree with the bare token.
#   - FORMAT() NEVER PASTES A STRING IT DID NOT RENDER (fmt_rendered()): a void field is BLANK, never
#     the literal "NA", and takes no significance star.
#   - fmt_display_label() IS THE ONE SOURCE OF A COLUMN'S NAME, as format() is of its cells': it walks
#     the column's own template and substitutes each token's declared short `label`
#     (DISPLAY_TOKENS, R/tab-display.R), so the console type tag, the exports' unit header row and an
#     Excel aside column's header cannot name a layout three different ways. It keeps the template's
#     STRUCTURE, not its wording -- of every literal only the brackets survive -- and a column is
#     named by its DATA rows (`row_kind`), never by a p-value or base-count row sharing its column,
#     nor by a template that renders nothing wherever it sits.
#   - Adding a FIELD touches ~9 sites here (follow the /vctrs-field skill); adding an ATTRIBUTE is a
#     new_fmt() formal + one `fmt_attr_rules` row (a build-time stopifnot refuses a missing row).
#   - Display glyph constants (mult_sign, div_sign, unbrk, sigma_sign, fig_space) live in utils.R.
# See: CLAUDE.md § tabxplor architecture (type system + colour system); /vctrs-field + /color-mode skills.

# Create formated numbers class
#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name tabxplor-vctrs
NULL


# binding for global variables not found by R CMD check
. = NULL
utils::globalVariables(c(":=", ".SD", ".N"))
# data.table NSE column symbols in tab_plain()'s aggregation j-expressions:
utils::globalVariables(c("n", "wn", "w2"))
# the whole-table test engine's data.table NSE column symbols (R/tab-agg.R):
utils::globalVariables(c("table_id", "row_id", "col_id", "o", "rowtot", "coltot", "ok",
                  "grandtot", "nr", "nc", "e", "contrib", "signed_contrib", "contrib_unc",
                  "statistic", "df", "min_e", "w", "group_id"))

# NSE column symbols in dplyr verbs over ordinary data frames:
#   `var`               -- reg_build()'s group_by(var) on the regression skeleton (R/tab_reg.R)
#   `name`/`size`/`color` -- tab_xl_plan_one()'s font/style plan tibbles (R/tab_xl.R)
#   marital/race/partyid/rincome/relig -- gss_cat_data_formatting()'s mutate() over forcats::gss_cat
utils::globalVariables(c("var", "name", "size", "color",
                         "marital", "race", "partyid", "rincome", "relig"))

# mirai daemon globals (R/tab-parallel.R): `tabx_opts`/`tabx_ship` are list2env()'d into each daemon.
utils::globalVariables(c("tabx_opts", "tabx_ship"))

# the superseded producers' declared `...`-args, bound at runtime via list2env() and so invisible to
# R's static checker:
utils::globalVariables(c("OR", "tot", "color_breaks"))


# EXPORTED FUNCTIONS TO WORK WITH CLASS FMT ##############################################


#' Create a vector of class formatted numbers
#' @description \code{fmt} vectors, of class \code{tabxplor_fmt}, powers \pkg{tabxplor}
#' and \code{\link{tab}} tibbles.
#' As a \code{\link[vctrs:new_rcrd]{record}}, they stores all data necessary to
#' calculate percentages, Chi2 metadata or confidence intervals, but also to format and
#' color the table to help the user read it. You can access this data with
#' \code{\link[vctrs:field]{vctrs::field}}, or change it with
#' \code{\link[vctrs:field]{vctrs:field<-}}. Its per-cell \strong{fields} are listed below.
#' The other arguments are \strong{attributes}, attached not to each value but to
#' the whole vector, like \code{scale}, \code{col_var}, \code{totcol} or \code{color}. You can get
#' them with \code{\link[base:attr]{attr}} and modify them with
#' \code{\link[base:attr]{attr<-}}. Special functions listed below are made to
#' facilitate programming with with \pkg{tabxplor} formatted numbers.
#' \code{taxplfmt} vectors can use all standard operations, like +, -, sum(), or c(),
#' using \pkg{vctrs}.
#'
#' @param n The underlying count, as an integer vector of length \code{n()}. It is used
#' to calculate confidence intervals.
#' @param scale What the column estimates, as a single string (an attribute, not a field): one key
#' into the declared library of estimate scales. It says which field holds the estimate, what its
#' null value is, whether the scale is additive or multiplicative, and which colour ladder it reads.
#' \itemize{
#'   \item \code{"level_n"}: counts
#'   \item \code{"level_pct"}: percentages (\code{pct_type} says of what)
#'   \item \code{"level_mean"}: means (from numeric variables)
#'   \item \code{"points"}: a difference between two percentages, in percentage points
#'   \item \code{"mean_diff"}: a difference between two means, in the outcome's own units
#'   \item \code{"raw_diff"}: a regression coefficient / marginal effect in the outcome's units
#'   \item \code{"pct_ratio"}, \code{"mean_ratio"}: the ratio of two percentages / two means
#'   \item \code{"odds_ratio"}: a multiplicative effect (odds ratio, risk ratio, rate ratio)
#'   \item \code{"log_coef"}: a link-scale coefficient (a log-odds, a log-rate)
#'   \item \code{"mixed"}: what binding columns of unlike scales collapses to
#' }
#' @param pct_type For a percentage column, what the percentage is a percentage OF, and hence which
#' axis its reference lies on (as a single string): \code{"row"}, \code{"col"}, \code{"all"}
#' (frequencies by subtable / group, i.e. by \code{tab_vars}), \code{"all_tabs"} (frequencies for
#' the whole table), or \code{"none"} (counts, means, coefficients).
#' @param digits The number of digits, as an integer, or an integer vector the length
#' of \code{n}.
#' @param display The display type : the name of the field you want to show when printing
#' the vector, as a single string or a character vector the length of \code{n}. Every accepted
#'  value is listed in \emph{Every display token} below; a \code{\{\}} template combining several
#'  of them (e.g. \code{"\{pct\} (n=\{n\})"}) is also accepted --- see \code{\link{tab}}.
#' @param wn The underlying weighted counts, as a double vector the length of
#' \code{n}. It is used in certain operations on \code{\link{fmt}}, like means.
#' @param pct The percentages, as a double vector the length of \code{n}.
#'  Calculate with \code{\link{tab_pct}}.
#' @param mean The means, as a double vector the length of \code{n}.
#' @param diff The differences (from totals or first cells),
#' as a double vector the length of \code{n}. Used to set colors for means and
#' row or col percentages. Built by \code{\link{tab}}.
#' @param ratio The ratio to the reference (relative risk for percentages, mean ratio for
#' means), as a double vector the length of \code{n}.
#' @param ctr The contributions of cells to (sub)tables variances,
#' as a double vector the length of \code{n}. Used to print colors when
#' \code{color = "contrib"}. The mean contribution of each (sub)table is written on
#' total rows (then, colors don't print well without total rows).
#' Built by \code{\link{tab}}. The cell's adjusted standardized residual is not a
#' field of its own: it is recovered from \code{pvalue} and this field's sign, and readable with
#' \code{display = "resid"} (see \code{\link{tab}}).
#' @param var The cells variances, as a double vector the length of \code{n}.
#' Used with \code{scale = "level_mean"} to calculate confidence intervals.
#' @param ci The confidence interval half-width (margin of error), as a double vector the
#' length of \code{n}. Kept for backward compatibility: it is stored as the symmetric
#' bounds \code{ci_inf}/\code{ci_sup} and read back by \code{get_ci()}.
#' @param ci_inf,ci_sup The lower and upper bounds of the confidence interval, as double
#' vectors the length of \code{n}. Built by \code{\link{tab}}.
#' @param pvalue The per-cell significance p-value, as a double vector the length of
#' \code{n}.
#' @param or The odds ratio (for a 3+ level variable, the OR of each level versus the reference),
#'   as a double vector the length of \code{n}.
#' @param tot_n The cell's own (unweighted) percentage base, as a double vector the length
#' of \code{n}.
#' @param n_eff The effective sample size used for this cell's confidence interval,
#' \code{p(1-p) / Var_design(p)} (a mean: \code{s^2 / Var_design(mean)}): from
#' \code{survey::svyrecvar} under a \code{survey::svydesign}, from the closed-form flat-design
#' variance when the weighted basis is asked for (\code{tab(design_effect = TRUE)}), else \code{NA}
#' (the CI falls back to the raw unweighted base). It records \emph{the base that was used}: a finite
#' value where the design or weights corrected it, \code{NA} where nothing did, and the \strong{raw
#' count} where a correction was asked for but this cell could not carry one. Populated for
#' descriptive cells (a crosstab/mean cell, a \code{tab_reg} \code{Obs_*} column whose interval came
#' from a closed form); a coefficient column, and any column whose interval came from a fit instead,
#' carry none. A double vector the length of \code{n}. Non-displayed.
#' @param obs The value this cell's estimate is COMPARED TO by the \code{tab_reg} colour measures
#' \code{"adjustment"} / \code{"between_groups"}, on the cell's own scale: the observed (crude)
#' effect beside a model effect, or -- under \code{tab_vars} with \code{color = "between_groups"} --
#' the reference group's estimate. \code{NA} on cross-tables and wherever there is no counterpart
#' (leaving those cells uncoloured). A double vector the length of \code{n}; displayable as
#' \code{display = "\{obs\}"}.
#' @param gap_se The standard error of the GAP between this cell's estimate and \code{obs}, on the
#' estimate's own test scale. Written by \code{tab_reg} where the two estimates are independent
#' (\code{tab_vars} groups), so \code{color = "between_groups"} can honour \code{color_signif};
#' \code{NA} elsewhere. A double vector the length of \code{n}. Non-displayed.
#' @param row_kind What kind of row the cell sits in --- one of \code{"data"} (an ordinary body row),
#' \code{"total"}, and the synthetic display rows \code{"n"}, \code{"pct"}, \code{"pvalue"},
#' \code{"gof"}, \code{"blank"}. A character vector the length of \code{n}. It supersedes the logical
#' \code{in_totrow} field, kept as a soft-deprecated argument and read-only \code{$in_totrow}.
#' @param in_totrow `r lifecycle::badge("deprecated")` Use \code{row_kind = "total"}.
#' @param in_tottab \code{TRUE} when the cell is part of a total table
#' @param in_refrow \code{TRUE} when the cell is part of a reference row
#' (cf. \code{ref})
#' @param comp_all  \code{FALSE} when the comparison level is the subtable/group,
#' \code{TRUE} when it is the whole table
#' @param ref The type of difference of the vector. Cf. \code{\link{tab}}.
#' @param col_var The name of the \code{col_var} used to calculate the vector
#' @param col_group The sub-population this column's block belongs to: a level of a
#'   \code{spread_vars} variable (\code{\link{tab_spread}}), or a \code{\link{tab_reg}}
#'   \code{tab_vars} group. \code{""} (the default) when the table was never spread. Together with
#'   \code{col_var} it identifies a column BLOCK: two blocks may show the same variable for two
#'   sub-populations, and exports head them on two lines.
#' @param totcol \code{TRUE} when the vector is a total column
#' @param refcol \code{TRUE} when the vector is a reference column
#' @param x The object to test, to get a field in, or to modify.
#' @param ... In \code{fmt()}, not used: it exists only so that the arguments removed in tabxplor
#'   2.0.0 (\code{type}, \code{ci_type}) get an error naming their replacement. In the accessor
#'   methods below, to add arguments in the future.
#' @param color The colour measure, as a single string --- how a cell's value is compared to colour
#' it (significance is handled separately by \code{color_signif}):
#' \itemize{
#'   \item \code{"no"}: no colors are printed.
#'   \item \code{"diff"} (\code{"difference"}): the cell's difference from the reference (a total, or
#'   the first cell when \code{ref = "first"}) --- percentage points for factors, a standardized
#'   difference for means.
#'   \item \code{"ratio"}: the ratio to the reference (relative risk for percentages, mean ratio).
#'   \item \code{"or"} (\code{"odds_ratio"}): the odds ratio, for row/col percentages.
#'   \item \code{"contrib"}: the cell's contribution to the table's variance. Under
#'   \code{color_signif = "guaranteed_effect"} it switches to the absolute adjusted standardized
#'   residual --- see \code{\link{tab}}.
#'   \item \code{"adjustment"} / \code{"between_groups"}: the two \code{\link{tab_reg}} measures,
#'   which compare a cell to \emph{another column} rather than to a reference row. A hand-built
#'   column may carry them, provided it fills the \code{obs} field they score.
#' }
#' The value is \strong{validated and normalised}: every accepted spelling --- the discipline's
#' acronyms included (\code{"RD"}, \code{"RR"}, \code{"IRR"}, \code{"RoM"}, \code{"OR"} and their
#' lowercase twins) --- is stored as its canonical measure name, and an unknown one is an error.
#' The tabxplor 1.x combined strings \code{"diff_ci"} / \code{"after_ci"} still work but are
#' superseded by the \code{color} + \code{color_signif} pair; here they resolve to their
#' \emph{measure} half only, so pass the significance policy through \code{color_signif}.
#' @param color_signif How significance gates the color, as a single string
#' (\code{"ignore"} / \code{"grey_non_signif"} / \code{"guaranteed_effect"}). See \code{\link{tab}}.
#' @param model_family For regression tables (\code{\link{tab_reg}}): the column's model family
#' (\code{"binomial"}, \code{"gaussian"}, \code{"poisson"}, \code{"multinomial"}, \code{"ordinal"}),
#' as a single string. Empty (\code{""}) on cross-tables. Lets a table mix several outcomes with
#' different families, each column keeping its own effect wording.
#' @param role For regression tables (\code{\link{tab_reg}}): the column's role, \code{"model"} for a
#' model-estimate column or \code{"emp"} for an empirical (crude) companion column. Empty (\code{""})
#' on cross-tables. Read by the colour legend to name each column's effect without matching its label.
#' @param conf_level The confidence level this column's interval and thresholds were computed at, as
#' a single number in (0, 1). \code{NA} (default) means "unknown" --- the colour engine then falls
#' back to \code{options("tabxplor.conf_level")}. Stored per COLUMN, because colours are resolved per
#' column at print time and cannot see the table's \code{conf_level} argument.
#' @param degf The degrees of freedom this column's interval is referred to. On a cross-table that is
#' the survey design's \code{#PSU - #strata}, which matters below ~30 primary sampling units; on a
#' regression it is the fitted model's own residual df (for an \code{svyglm}, \code{degf + 1 - p}),
#' so a model column and its observed companion legitimately differ. \code{NA} (default) means
#' "refer to the normal quantile".
#' @param basis How this column's interval and significance were computed --- \code{"n"} (the raw
#' sample size), \code{"weights"} (the design effect of the weights), \code{"design"} (a full
#' \code{survey} design), or \code{"design_partial"} (a design was given but its variance could not
#' be computed). Default \code{"n"}. A per-COLUMN fact, so a table states honestly what its numbers
#' carry even after a pipeline drops the table's metadata; binding columns keeps the WEAKEST basis.
#' @param ci_method Which interval ENGINE built this column's bounds --- \code{"wilson"},
#' \code{"wald"}, \code{"beta"} (a cell proportion), \code{"newcombe"}, \code{"ac"} (a difference of
#' proportions), \code{"katz"} (a ratio of proportions), \code{"welch"}, \code{"student"},
#' \code{"ols"} (a difference of means), \code{"robust"}, \code{"quasipoisson"}, \code{"poisson"} (a ratio of
#' means), \code{"woolf"}, \code{"wald_log"}, \code{"profile"}; \code{""} (default) when the column
#' carries no interval. Read back by the colour legend, so it always names the method the bounds were
#' built with.
#' @eval fmt_fields_rd()
#' @eval display_tokens_rd(user_only = FALSE)
#' @eval display_presets_rd()
#'
#' @return A vector of class \code{tabxplor_fmt}.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' f <- fmt(n = c(7, 19, 2), pct = c(0.25, 0.679, 0.07),
#'          scale = "level_pct", pct_type = "row")
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
#' # To modify the "pct_type" attribute of a fmt vector (what the percentage is a percentage OF) :
#' set_pct_type(f, "col")
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
                scale     = "level_n",

                digits    = rep(0L      , length(n)),
                display   = est_default_display(scale[1]),   # ONE declared rule (EST_SCALES)

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

                row_kind  = rep("data", length(n)),
                in_tottab = rep(FALSE, length(n)),
                in_refrow = rep(FALSE, length(n)),
                in_totrow = NULL,


                comp_all  = NA   ,
                ref = ""   ,
                pct_type  = "none",
                col_var   = ""   ,
                col_group = ""   ,   # the sub-population this column's block belongs to
                totcol    = FALSE,
                refcol    = FALSE,
                color     = ""    ,
                color_signif = "ignore",
                model_family = ""   ,   # per-column regression family ("" on crosstabs)
                role         = ""   ,   # per-column role -- "model"/"emp" on reg columns
                conf_level   = NA_real_, # the level this column's interval was built at
                degf         = NA_real_, # the design df its critical value uses
                basis        = "n"     , # ... and HOW its interval was computed (see new_fmt())
                ci_method    = ""      , # which interval ENGINE built its bounds
                ...) {

  # `...` exists only to catch the removed `type`/`ci_type` args and answer with their replacement
  # (`scale` + `pct_type`) at the point of the mistake, instead of R's opaque "unused argument".
  fmt_abort_legacy_args(...)
  if (length(scale) != 1L || is.na(scale) || !scale %in% EST_SCALE_KEYS)
    cli::cli_abort(c("{.arg scale} must be one of {.val {EST_SCALE_KEYS}}.",
                     "x" = "Got {.val {scale}}."), call = NULL)

  # DESIGN: these 8 fields set the recycling reference length. display, diff, ratio, or,
  # the ci bounds, pvalue, tot_n and the in_* flags are recycled TO it below, so they must
  # not be passed longer than these (vec_recycle would error, not extend).
  max_size <- list(n, wn, pct, digits, ctr, mean, var, ci) |> #display
    purrr::map_int(length) |> max()

  display <- vctrs::vec_recycle(vctrs::vec_cast(display, character()), size = max_size)
  # A LEGACY LAYOUT SPELLING IS NORMALISED AT THE BOUNDARY, once: "or_pct" / "OR_pct" were tokens with
  # a rendering branch of their own; they are the preset `or_base` now, so a value stored by 1.x code
  # (or written verbatim by the jamovi ComboBox) becomes the {} template it always meant, and nothing
  # downstream keeps a legacy arm. Only the ALIAS rows are resolved -- `est` / `base` are both a token
  # and a preset name, and the pipeline means the TOKEN.
  if (any(display %in% names(DISPLAY_PRESET_ALIASES))) {
    for (v in intersect(unique(display), names(DISPLAY_PRESET_ALIASES)))
      display[display == v] <- display_resolve(v)
  }
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
  # effective sample size for this cell's CI; NA -> fall back to the raw unweighted base. CI-only.
  n_eff   <- vctrs::vec_recycle(vctrs::vec_cast(n_eff  , double())   , size = max_size)
  # the value this cell is compared to by color "adjustment"/"between_groups", on its own scale;
  # NA everywhere else -> those measures score NA -> uncoloured.
  obs     <- vctrs::vec_recycle(vctrs::vec_cast(obs    , double())   , size = max_size)
  # SE of the gap between the estimate and `obs`, on the estimate's own test scale; written only
  # where the two are independent (tab_vars groups), NA elsewhere -> significance policies stay inert.
  gap_se  <- vctrs::vec_recycle(vctrs::vec_cast(gap_se , double())   , size = max_size)

  # The public `ci` arg is a symmetric half-width; store it as ABSOLUTE bounds around the estimate
  # the interval is centred on -- the scale's declared `est_field` (ONE rule). Explicit
  # ci_inf/ci_sup win; get_ci() reads the half-width back as ci_sup - centre.
  est_center <- dplyr::coalesce(
    switch(EST_SCALES[[scale[1]]]$est_field,
           or = or, ratio = ratio, diff = diff, mean = mean, n = as.double(n), pct),
    0)
  ci_sup  <- dplyr::coalesce(ci_sup, est_center + ci)
  ci_inf  <- dplyr::coalesce(ci_inf, est_center - ci)

  # `row_kind` records which of the seven row kinds a cell sits in (body/total + five synthetic
  # display rows), so it rides every slice and bind. `in_totrow =` is soft-deprecated -> "total".
  if (!is.null(in_totrow)) {
    lifecycle::deprecate_soft("2.0.0", "fmt(in_totrow = )", "fmt(row_kind = )")
    in_totrow <- vctrs::vec_recycle(vctrs::vec_cast(in_totrow, logical()), size = max_size)
    row_kind  <- dplyr::if_else(in_totrow, "total", "data")
  }
  row_kind  <- vctrs::vec_recycle(vctrs::vec_cast(row_kind , character()), size = max_size)
  if (!all(row_kind %in% ROW_KINDS))
    cli::cli_abort(c("{.arg row_kind} must be one of {.val {ROW_KINDS}}.",
                     "x" = "Got {.val {setdiff(row_kind, ROW_KINDS)}}."), call = NULL)
  in_tottab <- vctrs::vec_recycle(vctrs::vec_cast(in_tottab, logical()), size = max_size)
  in_refrow <- vctrs::vec_recycle(vctrs::vec_cast(in_refrow, logical()), size = max_size)

  scale     <- vctrs::vec_recycle(vctrs::vec_cast(scale    , character()), size = 1)
  comp_all  <- vctrs::vec_recycle(vctrs::vec_cast(comp_all , logical()  ), size = 1)
  ref <- vctrs::vec_recycle(vctrs::vec_cast(ref, character()), size = 1)
  pct_type  <- vctrs::vec_recycle(vctrs::vec_cast(pct_type , character()), size = 1)
  col_var   <- vctrs::vec_recycle(vctrs::vec_cast(col_var  , character()), size = 1)
  totcol    <- vctrs::vec_recycle(vctrs::vec_cast(totcol   , logical()  ), size = 1)
  refcol    <- vctrs::vec_recycle(vctrs::vec_cast(refcol   , logical()  ), size = 1)
  # `color` is a per-column attribute of length 1 (text channel) or 2 (text, background) -- NOT
  # recycled to 1. color_signif is the scalar significance policy. BOTH go through the storage
  # boundary the setters use, so every spelling is resolved to its canonical measure here and an
  # unknown one aborts: a stored `color` attribute is ALWAYS a MEASURES key, never an acronym, which
  # is what lets every reader compare it to a literal.
  # ⚠ `fmt()` normalises WITHOUT decoding: a legacy `"after_ci"` resolves to its MEASURE half and
  #   drops the policy half, exactly as set_color() does. Only tab()'s argument boundary decodes the
  #   pair (color_decode_legacy(), R/tab.R) -- one decoder, and this is not it.
  color        <- resolve_color_channels(color)
  color_signif <- vctrs::vec_recycle(vctrs::vec_cast(color_signif, character()), size = 1)
  color_signif <- resolve_color_signif(color_signif)
  model_family <- vctrs::vec_recycle(vctrs::vec_cast(model_family, character()), size = 1)
  role         <- vctrs::vec_recycle(vctrs::vec_cast(role        , character()), size = 1)
  conf_level   <- vctrs::vec_recycle(vctrs::vec_cast(conf_level  , double()   ), size = 1)
  degf         <- vctrs::vec_recycle(vctrs::vec_cast(degf        , double()   ), size = 1)
  basis        <- vctrs::vec_recycle(vctrs::vec_cast(basis       , character()), size = 1)
  ci_method    <- vctrs::vec_recycle(vctrs::vec_cast(ci_method   , character()), size = 1)

  new_fmt(n = n, display = display, digits = digits,
          wn = wn, pct = pct,  mean = mean,
          diff = diff, ratio = ratio, ctr = ctr, var = var,
          ci_inf = ci_inf, ci_sup = ci_sup, pvalue = pvalue, or = or, tot_n = tot_n,
          n_eff = n_eff, obs = obs, gap_se = gap_se,
          row_kind = row_kind, in_tottab = in_tottab, in_refrow = in_refrow,
          scale = scale, comp_all = comp_all,  ref = ref,
          pct_type = pct_type, col_var = col_var, col_group = col_group,
          totcol = totcol, refcol = refcol,
          color = color, color_signif = color_signif, model_family = model_family,
          role = role, conf_level = conf_level, degf = degf, basis = basis,
          ci_method = ci_method)
}

# The removed `type` / `ci_type` obituary, delivered where the mistake is made: the error names the
# replacement rather than leaving R to say "unused argument".
#' @keywords internal
#' @noRd
fmt_abort_legacy_args <- function(...) {
  bad <- names(list(...))
  if (!length(bad)) return(invisible(NULL))
  hint <- c(
    type    = "{.arg type} conflated two facts and is SPLIT in two: {.arg scale} (what the column
               estimates) and {.arg pct_type} (which kind of percentage: row / col / all). A row
               percentage is {.code scale = \"level_pct\", pct_type = \"row\"}, a mean
               {.code scale = \"level_mean\"}, a count {.code scale = \"level_n\"}, a regression
               coefficient {.code scale = \"raw_diff\"}.",
    ci_type = "{.arg ci_type} is gone: the stored interval is always on the estimate's own
               {.arg scale}. A difference interval on a row percentage is
               {.code scale = \"points\"}, on a mean {.code scale = \"mean_diff\"}, an odds ratio
               {.code scale = \"odds_ratio\"}, a ratio {.code scale = \"pct_ratio\"} /
               {.code \"mean_ratio\"}.")
  known <- intersect(bad, names(hint))
  if (!length(known))
    cli::cli_abort("Unused argument{?s} in {.fn fmt}: {.arg {bad}}.", call = NULL)
  cli::cli_abort(c("{.fn fmt} no longer has {.arg {known}} (tabxplor 2.0.0).",
                   stats::setNames(unname(hint[known]), rep("i", length(known))),
                   "i" = "See {.code ?fmt} and {.code names(tabxplor:::EST_SCALES)}."),
                 call = NULL)
}

#' @describeIn fmt a test function for class fmt.
#' @return A logical vector.
#' @export
is_fmt <- function(x) {
  inherits(x, "tabxplor_fmt")
}




#' @describeIn fmt get the currently displayed field
#' @return A double vector.
#' @export
get_num <- function(x) {
  # DESIGN: get_num() is the authoritative `display` -> underlying-field map. Allowed display values
  # and the field each reads: n/(default)->n, wn->wn, pct/pvalue->pct, diff->diff, ctr->ctr,
  # mean->mean, var->var, ci/moe->get_ci() (the CI half-width from the ci_sup bound),
  # ratio (rr aliased to it)->ratio, or (OR aliased to it)->or, obs->obs. When adding a display value,
  # keep this map, set_num() and format() in sync (see the /vctrs-field skill).
  out     <- get_n(x)
  # resolve composite templates ("{pct} (n={n})") to their PRIMARY field, then the scale-relative
  # `est` / `base` tokens to the token this column renders them as, before the dispatch masks.
  display <- fmt_resolve_scale_tokens(display_primary(get_display(x)), fmt_scale_row(x))
  nas     <- is.na(display)
  out[!nas & display == "wn"     ] <- get_wn  (x)[!nas & display == "wn"     ]
  out[!nas & display == "pct"    ] <- get_pct (x)[!nas & display == "pct"    ]
  out[!nas & display == "pvalue" ] <- get_pvalue(x)[!nas & display == "pvalue" ]  # honest p in the pvalue field
  out[!nas & display == "diff"   ] <- get_diff(x)[!nas & display == "diff"   ]
  # `coef` is THE LINK-SCALE reading: the stored `diff` where the column is already additive (a
  # gaussian or a log coefficient -- unchanged), log(estimate) where it is multiplicative, since
  # log(OR) IS the coefficient the model fitted. Derived there, so nothing is stored twice.
  coef_m <- !nas & display == "coef"
  if (any(coef_m)) out[coef_m] <- fmt_coef_of(x)[coef_m]
  gf <- !nas & display %in% c("gof", "gof_warn")     # model-fit stat (N/R2/AIC/...) -> diff field
  out[gf] <- get_diff(x)[gf]
  out[!nas & display == "ctr"    ] <- get_ctr (x)[!nas & display == "ctr"    ]
  # DERIVED (no field of its own): the adjusted standardized residual behind color = "contrib"'s
  # significance. Read-only: set_num() has no matching arm.
  out[!nas & display == "resid"  ] <- fmt_resid(x)[!nas & display == "resid"  ]
  # DERIVED too: the model-vs-observed gap, on the estimate's own scale (a ratio around 1, a
  # difference around 0). It is the very number `color = "adjustment"` grades, so a printed gap and
  # its shade cannot disagree. Read-only: there is nothing to write a gap back into.
  out[!nas & display == "gap"    ] <- fmt_adjustment_score(x)[!nas & display == "gap"    ]
  out[!nas & display == "mean"   ] <- get_mean(x)[!nas & display == "mean"   ]
  out[!nas & display == "var"    ] <- get_var (x)[!nas & display == "var"    ]
  # DERIVED from `var`, like `resid` and `gap`: the sd is its square root, so nothing is stored twice.
  sd_m <- !nas & display == "sd"
  if (any(sd_m)) out[sd_m] <- suppressWarnings(sqrt(get_var(x)))[sd_m]
  # DERIVED from `var` AND `mean`. VOID where the mean is not strictly positive: a spread expressed as
  # a share of a level at or below zero is not a share of anything, and it flips sign with the mean.
  # Declared void here, exactly as `moe` is on a multiplicative scale, rather than special-cased at
  # render time -- so the per-cell void rule blanks it and keeps the column aligned.
  cv_m <- !nas & display == "cv"
  if (any(cv_m)) {
    m <- get_mean(x) ; v <- get_var(x)
    out[cv_m] <- ifelse(!is.na(m) & m > 0 & !is.na(v) & v >= 0,
                        suppressWarnings(sqrt(v)) / m, NA_real_)[cv_m]
  }
  out[!nas & display == "ci"     ] <- get_ci   (x)[!nas & display == "ci"     ]
  # `moe` reads the SAME field as `ci` -- the two are one interval in two notations -- but a ratio
  # has no half-width, so it is void on a multiplicative scale. Declared void, like any other token
  # with nothing to show, rather than special-cased at render time.
  moe_m <- !nas & display == "moe"
  if (any(moe_m)) out[moe_m] <- if (isTRUE(fmt_scale_row(x)$mult)) NA_real_ else get_ci(x)[moe_m]
  out[!nas & display == "ratio"] <- get_ratio(x)[!nas & display == "ratio"]
  out[!nas & display == "or"     ] <- get_or  (x)[!nas & display == "or"     ]
  # the value this cell is compared to (observed/crude effect, or the reference group's estimate);
  # a real stored field, so it round-trips (set_num() has a matching arm).
  out[!nas & display == "obs"    ] <- get_obs (x)[!nas & display == "obs"    ]
  # the base count: the SMALLEST base sits in `n` (the largest, when the bases differ, in `tot_n` --
  # a format() concern), so every numeric consumer here reads a real count.
  out[!nas & display == "n_range"] <- get_n(x)[!nas & display == "n_range"]
  # "blank" is a display-only mask (n_min sets it on small-base cells): carries NO number (format()
  # emits ""), while the underlying n/pct/tot_n stay intact, so the mask is fully reversible.
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
  # a composite cell writes back to its PRIMARY field (the first {token}); `est` / `base` write back
  # to the field the column renders them as, so the read and the write map cannot drift.
  display <- fmt_resolve_scale_tokens(display_primary(get_display(x)), fmt_scale_row(x))
  nas     <- is.na(display)
  out[!nas & display == "n"   ] <- set_n   (x[!nas & display == "n"   ], value[!nas & display == "n"   ])
  out[!nas & display == "wn"  ] <- set_wn  (x[!nas & display == "wn"  ], value[!nas & display == "wn"  ])
  out[!nas & display == "diff"] <- set_diff(x[!nas & display == "diff"], value[!nas & display == "diff"])
  # the write mirrors get_num()'s read, exp() included, so the two maps cannot drift.
  coef_m <- !nas & display == "coef"
  if (any(coef_m)) {
    out[coef_m] <- if (!isTRUE(fmt_scale_row(x)$mult)) set_diff(x[coef_m], value[coef_m])
                   else switch(fmt_center_field(x), or = set_or, ratio = set_ratio,
                               set_diff)(x[coef_m], exp(value[coef_m]))
  }
  gf <- !nas & display %in% c("gof", "gof_warn")
  out[gf] <- set_diff(x[gf], value[gf])
  out[!nas & display == "ctr" ] <- set_ctr (x[!nas & display == "ctr" ], value[!nas & display == "ctr" ])
  out[!nas & display == "var" ] <- set_var (x[!nas & display == "var" ], value[!nas & display == "var" ])
  ci_m <- !nas & display %in% c("ci", "moe")            # one field, two notations
  out[ci_m] <- set_ci(x[ci_m], value[ci_m])
  out[!nas & display == "ratio"] <- set_ratio(x[!nas & display == "ratio"], value[!nas & display == "ratio"])
  # `pvalue` needs a write arm too, or arithmetic through vec_arith -> set_num() would return the
  # column unchanged. R/tab-display.R asserts at BUILD time that every token DISPLAY_TOKENS marks
  # `settable` has an arm here.
  out[!nas & display == "pct" ] <- set_pct (x[!nas & display == "pct" ], value[!nas & display == "pct" ])
  out[!nas & display == "mean"] <- set_mean(x[!nas & display == "mean"], value[!nas & display == "mean"])
  out[!nas & display == "pvalue"] <- set_pvalue(x[!nas & display == "pvalue"],
                                                value[!nas & display == "pvalue"])
  or_m <- !nas & display == "or"
  out[or_m] <- set_or(x[or_m], value[or_m])
  out[!nas & display == "obs" ] <- set_obs(x[!nas & display == "obs" ], value[!nas & display == "obs" ])
  out
}

#' @describeIn fmt get the estimate scale of fmt columns (at \code{fmt} level or \code{tab} level)
#' @return A character vector with the vectors scale.
#' @export
get_scale <- function(x, ...) UseMethod("get_scale")
#' @method get_scale default
#' @export
#' @noRd
get_scale.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("scale")(x)),
         yes = purrr::attr_getter("scale")(x),
         no  = "mixed")
}
#' @method get_scale tabxplor_fmt
#' @export
#' @noRd
get_scale.tabxplor_fmt <- function(x, ...) attr(x, "scale", exact = TRUE)
#' @method get_scale data.frame
#' @export
#' @noRd
get_scale.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_scale(.))

#' @describeIn fmt set the estimate scale attribute of a \code{fmt} vector
#' @return A modified fmt vector.
#' @export
set_scale     <- function(x, scale) {
  scale <- as.character(scale)[1]
  # the scale is a KEY into EST_SCALES, so the vocabulary and the allow-list are one object.
  stopifnot(scale %in% EST_SCALE_KEYS)
  `attr<-`(x, "scale", scale)
}

#' @describeIn fmt get which kind of percentage fmt columns hold (at \code{fmt} level or \code{tab} level)
#' @return A character vector with the vectors percentage type.
#' @export
get_pct_type <- function(x, ...) UseMethod("get_pct_type")
#' @method get_pct_type default
#' @export
#' @noRd
get_pct_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("pct_type")(x)),
         yes = purrr::attr_getter("pct_type")(x),
         no  = "none")
}
#' @method get_pct_type tabxplor_fmt
#' @export
#' @noRd
get_pct_type.tabxplor_fmt <- function(x, ...) attr(x, "pct_type", exact = TRUE)
#' @method get_pct_type data.frame
#' @export
#' @noRd
get_pct_type.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_pct_type(.))

# A column REPURPOSED as a plain count (the no-col_var `n`/`wn` columns, the Excel base-count layout
# column): it estimates a count, of nothing. Both halves in one call, because setting only the scale
# would leave a stale `pct_type` claiming the counts are percentages of a row.
#' @keywords internal
#' @noRd
set_count_col <- function(x) set_pct_type(set_scale(x, "level_n"), "none")

#' @describeIn fmt set the percentage-type attribute of a \code{fmt} vector
#' @return A modified fmt vector.
#' @export
set_pct_type  <- function(x, pct_type) {
  pct_type <- as.character(pct_type)[1]
  if (is.na(pct_type) || pct_type %in% c("no", "", "n")) pct_type <- "none"
  stopifnot(pct_type %in% PCT_TYPES)
  `attr<-`(x, "pct_type", pct_type)
}




#' @describeIn fmt test function to detect cells in total rows
#' (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors totrow field.
#' @export
is_totrow <- function(x, ...) UseMethod("is_totrow")
#' @method is_totrow default
#' @export
#' @noRd
is_totrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' @method is_totrow tabxplor_fmt
#' @export
#' @noRd
is_totrow.tabxplor_fmt <- function(x, ...) vctrs::field(x, "row_kind") == "total"

#' @describeIn fmt get the "row_kind" field: what kind of row each cell sits in
#' (one of \code{"data"}, \code{"total"}, \code{"n"}, \code{"pct"}, \code{"pvalue"},
#' \code{"gof"}, \code{"blank"}).
#' @return A character vector with the fmt vector's row_kind field.
#' @export
get_row_kind <- function(x) {
  if (!is_fmt(x)) return(rep("data", length(x)))
  vctrs::field(x, "row_kind")
}

#' @describeIn fmt set the "row_kind" field
#' @param row_kind The kind of row a cell sits in (see \code{\link{get_row_kind}}).
#' @return A modified fmt vector with the row_kind field changed.
#' @export
set_row_kind <- function(x, row_kind) {
  row_kind <- vctrs::vec_recycle(vctrs::vec_cast(row_kind, character()), length(x))
  stopifnot(all(row_kind %in% ROW_KINDS))
  vctrs::`field<-`(x, "row_kind", row_kind)
}

# the per-ROW kind of a whole table: the reduce of `row_kind` across its fmt columns, "first
# non-data wins" (a row is never two kinds at once, so the ORDER of ROW_KINDS is a tie-break).
#' @keywords internal
#' @noRd
fmt_row_kind <- function(x) {
  cols     <- unclass(x)
  fmt_cols <- cols[vapply(cols, is_fmt, logical(1))]
  if (length(fmt_cols) == 0L) return(character(0L))
  kinds <- lapply(fmt_cols, function(col) vctrs::field(col, "row_kind"))
  purrr::reduce(kinds, function(a, b) dplyr::if_else(a == "data", b, a))
}

# aggregate a per-cell fmt flag (row_kind / in_tottab / in_refrow) across a data.frame's fmt columns
# to a per-ROW logical, reading the field directly then reducing (a hot path: is_totrow / is_tottab /
# is_refrow). partial=FALSE => a row where ALL fmt cells are flagged (if_all); partial=TRUE => ANY
# (if_any). No fmt cols => logical(0).
fmt_row_flag <- function(x, field, partial = FALSE) {
  cols     <- unclass(x)
  fmt_cols <- cols[vapply(cols, is_fmt, logical(1))]
  if (length(fmt_cols) == 0L) return(logical(0L))
  # `row_kind` is a character field, so the total-row flag is derived per column before the reduce.
  flags <- if (identical(field, "row_kind"))
    lapply(fmt_cols, function(col) vctrs::field(col, "row_kind") == "total")
  else lapply(fmt_cols, function(col) vctrs::field(col, field))
  purrr::reduce(flags, if (partial) `|` else `&`)
}

#' @method is_totrow data.frame
#' @export
#' @noRd
is_totrow.data.frame <- function(x, ..., partial = FALSE) {
  fmt_row_flag(x, "row_kind", partial)
}

#' @describeIn fmt set the "total" row kind (belong to total row)
#' @return A modified fmt vector with totrow field changed.
#' @export
as_totrow  <- function(x, in_totrow = TRUE) {
  vctrs::vec_assert(in_totrow, logical())
  in_totrow <- vctrs::vec_recycle(in_totrow, length(x))
  set_row_kind(x, dplyr::if_else(in_totrow, "total", "data"))
}

#' Complete partial total rows
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' A build-internal repair: after a reshape, a row that is a total in SOME columns is made a total
#' in all of them --- **and so are `in_tottab` and `in_refrow`**, which is why nothing calls it any
#' more: after a spread those two are facts about a column BLOCK, not about a row. [tab_spread()]
#' completes the row kind alone. It will be made internal in 2.1.0.
#'
#' @param tabs A table or data frame containing `tabxplor_fmt` columns.
#'
#' @return The table with completed total rows, total tables, and reference rows.
#' @keywords internal
#' @export
#'
complete_partial_totals <- function(tabs) {
  # tabxplor's own call (tab_spread) stays silent; a direct user call is nudged.
  if (tx_user_call())
    lifecycle::deprecate_soft("2.0.0", "complete_partial_totals()",
                              details = "It repairs a table the build no longer produces.")
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
#' @method is_tottab default
#' @export
#' @noRd
is_tottab.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' @method is_tottab tabxplor_fmt
#' @export
#' @noRd
is_tottab.tabxplor_fmt <- function(x, ...) vctrs::field(x, "in_tottab")
#' @method is_tottab data.frame
#' @export
#' @noRd
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
#' @method set_display default
#' @export
#' @noRd
set_display.default <- function(x, value) {
return(x)
}
#' @method set_display tabxplor_fmt
#' @export
#' @noRd
set_display.tabxplor_fmt <- function(x, value) {
  # A PRESET NAME naming a multi-field LAYOUT goes through THE shared display writer
  # (display_write_col) -- the same one tab(display =) runs, so a post-hoc set_display() and a
  # build-time one cannot differ, and the per-cell eligibility / void rules apply. A raw token or
  # {} template is written verbatim: the producers set per-cell tokens that way and must keep doing so.
  # the column's own `role` picks the preset's arm, so `across()`-ing one preset over a regression
  # table gives the crude and the model columns their mirrored layouts in one call.
  if (length(value) == 1L && !is.na(value) &&
      as.character(value) %in% c(names(DISPLAY_PRESETS), names(DISPLAY_PRESET_ALIASES))) {
    tmpl <- display_resolve(value, get_role(x))
    if (grepl("{", tmpl, fixed = TRUE)) return(display_write_col(x, tmpl)$col)
    value <- tmpl
  }
  fmt_set_display(x, value)
}

# The RAW display write, with no preset routing. `est` and `base` are both TOKENS and preset NAMES;
# the pipeline's per-cell writers and display_write_col()'s own probes mean the token, so they must
# not take the layout a user would mean by the same word.
#' @keywords internal
fmt_set_display <- function(x, value) {
  value <- vctrs::vec_cast(value, character()) |> vctrs::vec_recycle(size = length(x))
  vctrs::`field<-`(x, "display", value)
}

#' @method set_display data.frame
#' @export
#' @noRd
set_display.data.frame <- function(x, value) {
  # WARNING: column by column, NOT dplyr::across() -- on a GROUPED tab across() runs PER GROUP, and
  # display_write_col()'s "is this field empty in the WHOLE column" rule would then be answered per
  # sub-table: a one-row group would have its aside pruned away while its neighbours keep theirs, and
  # the column would stop lining up. A display is a property of the column, never of a sub-table.
  for (nm in names(x)) {
    col <- x[[nm]]
    if (!is_fmt(col)) next
    if (nm %in% c("n", "wn") && identical(fmt_var_kind(col), "count")) next
    x[[nm]] <- set_display(col, value)
  }
  x
}


#' @describeIn fmt test function for total columns
#' (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors totcol attribute.
#' @export
is_totcol <- function(x, ...) UseMethod("is_totcol")
#' @method is_totcol default
#' @export
#' @noRd
is_totcol.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("totcol")(x)),
         yes = purrr::attr_getter("totcol")(x),
         no  = FALSE)
}
#' @method is_totcol tabxplor_fmt
#' @export
#' @noRd
is_totcol.tabxplor_fmt <- function(x, ...) attr(x, "totcol", exact = TRUE)
#' @method is_totcol data.frame
#' @export
#' @noRd
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
#' @method is_refrow default
#' @export
#' @noRd
is_refrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' @method is_refrow tabxplor_fmt
#' @export
#' @noRd
is_refrow.tabxplor_fmt <- function(x, ...) vctrs::field(x, "in_refrow")
#' @method is_refrow data.frame
#' @export
#' @noRd
is_refrow.data.frame <- function(x, ..., partial = TRUE) {
  # same fold as is_totrow/is_tottab (default partial = TRUE -> if_any). See fmt_row_flag.
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
#' @export
get_comp_all <- function(x, replace_na = TRUE) {
  comp <- attr(x, "comp_all", exact = TRUE)
  if (is.null(comp)) return(NA)
  if (replace_na & is.na(comp)) comp <- FALSE
  comp
}

#' @describeIn fmt set the comparison level attribute of a \code{fmt} vector
#' @return A modified fmt vector with comp attribute changed.
#' @export
set_comp_all      <- function(x, comp_all = FALSE) {
  `attr<-`(x, "comp_all", comp_all)
}



#' @describeIn fmt get differences type of fmt columns (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors type attributes
#' @export
get_ref_type <- function(x, ...) UseMethod("get_ref_type")
#' @method get_ref_type default
#' @export
#' @noRd
get_ref_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("ref")(x)),
         yes = purrr::attr_getter("ref")(x),
         no  = "") #NA_character_
}
#' @method get_ref_type tabxplor_fmt
#' @export
#' @noRd
get_ref_type.tabxplor_fmt <- function(x, ...) attr(x, "ref", exact = TRUE)
#' @method get_ref_type data.frame
#' @export
#' @noRd
get_ref_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_ref_type(.))
}

#' @describeIn fmt set the reference attribute of a \code{fmt} vector — which row or column a
#'   comparison is made against. It is the writer of the attribute [get_ref_type()] reads.
#' @return A modified fmt vector.
#' @export
set_ref_type <- function(x, ref) {
  # The stored `ref` values are the resolver's own vocabulary plus a row NUMBER or a matched LABEL,
  # which is why this checks a shape rather than a fixed list.
  ref <- as.character(ref)[1]
  if (is.na(ref)) ref <- ""
  `attr<-`(x, "ref", ref)
}

#' @describeIn fmt `r lifecycle::badge("deprecated")` Use [set_ref_type()], which shares its stem
#'   with the getter [get_ref_type()] and with the `ref` attribute both of them address.
#' @export
set_diff_type <- function(x, ref) {
  lifecycle::deprecate_soft("2.0.0", "set_diff_type()", "set_ref_type()")
  set_ref_type(x, ref)
}




# The stored interval is ALWAYS on the estimate's own `scale`, so "does this column carry an interval"
# is a DATA fact -- all(is.na(get_ci_inf(x))) -- not a separate vocabulary. This is that read.
#' @keywords internal
#' @noRd
fmt_has_interval <- function(x) !all(is.na(get_ci_inf(x)))


#' @describeIn fmt get names of column variable of fmt columns (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors col_var attributes
#' @export
get_col_var <- function(x, ...) UseMethod("get_col_var")
#' @method get_col_var default
#' @export
#' @noRd
get_col_var.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("col_var")(x)),
         yes = purrr::attr_getter("col_var")(x),
         no  = "") #NA_character_
}
#' @method get_col_var tabxplor_fmt
#' @export
#' @noRd
get_col_var.tabxplor_fmt <- function(x, ...) attr(x, "col_var", exact = TRUE)
#' @method get_col_var data.frame
#' @export
#' @noRd
get_col_var.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_col_var(.))

#' @describeIn fmt set the "col_var" attribute of a \code{fmt} vector
#' @return A modified fmt vector.
#' @export
set_col_var   <- function(x, col_var) {
  vctrs::vec_assert(col_var, character(), size = 1)
  `attr<-`(x ,"col_var" , col_var)
}

#' @describeIn fmt get the sub-population of fmt columns (at \code{fmt} level or \code{tab} level)
#' @return A character vector with the vectors' col_group attributes (\code{""} when the table was
#'   never spread). On a data.frame, one value per column.
#' @export
get_col_group <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_chr(x, get_col_group))
  g <- attr(x, "col_group", exact = TRUE)
  if (is.null(g) || is.na(g)) "" else g
}

# writing is the PIPELINE's job (spread_relabel / reg_build's split branch), so the setter stays
# internal -- like the get_ci_method()/set_ci_method() split.
#' @keywords internal
#' @noRd
set_col_group <- function(x, col_group) {
  col_group <- vctrs::vec_recycle(vctrs::vec_cast(col_group, character()), size = 1)
  if (is.na(col_group)) col_group <- ""
  `attr<-`(x, "col_group", col_group)
}

# THE column-block identity, and the ONE place it is composed. A block is a (col_var, col_group)
# PAIR: read the pair, never col_var alone, or two spread blocks of the same variable collapse.
#   `key`   an internal, never-rendered identifier (the "\r" separator idiom reg_skel_key uses).
#   `label` what a reader sees when the two must appear as one string on ONE line (the legend).
#' @keywords internal
#' @noRd
fmt_col_block <- function(col_var, col_group = "") {
  col_group <- ifelse(is.na(col_group), "", col_group)
  list(key   = ifelse(nzchar(col_group), paste0(col_group, "\r", col_var), col_var),
       label = ifelse(nzchar(col_group), paste0(col_group, " ", col_var),  col_var))
}

# The distinct column BLOCKS of a table, in table order: one row per (col_var, col_group) pair, with
# the key and the one-line label fmt_col_block() composes. `tab_render_vars()$col_vars` is the same
# thing PROJECTED onto its first component, which is all a table without sub-populations needs.
#' @keywords internal
#' @noRd
tab_col_blocks <- function(x) {
  fm <- vapply(x, is_fmt, logical(1))
  if (!any(fm)) return(data.frame(col = character(), col_group = character(),
                                  key = character(), label = character(),
                                  stringsAsFactors = FALSE))
  cv <- unname(get_col_var(x[fm]))
  cg <- unname(get_col_group(x[fm]))
  keep <- !is.na(cv)
  d <- data.frame(col = cv[keep], col_group = cg[keep], stringsAsFactors = FALSE)
  d <- d[!duplicated(d), , drop = FALSE]
  b <- fmt_col_block(d$col, d$col_group)
  d$key   <- b$key
  d$label <- b$label
  d
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

#' @describeIn fmt set the "model_family" attribute of a \code{fmt} vector (the per-column
#'   regression family, "" on crosstabs)
#' @return A modified fmt vector.
#' @export
set_model_family <- function(x, model_family) {
  vctrs::vec_assert(model_family, character(), size = 1)
  `attr<-`(x ,"model_family" , model_family)
}

# the per-column `role` attribute -- "model"/"emp" on a regression column, "" on a crosstab column.
# Written by the reg builders (R/tab_reg.R), read by the legend adapters instead of matching the
# "Emp." name prefix. Internal (no exported getter yet).
#' @keywords internal
#' @noRd
get_role <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_chr(x, get_role))
  r <- attr(x, "role", exact = TRUE)
  if (is.null(r)) "" else r
}

# the reg builders write `role` through fmt()'s formal, but a column built by COPYING another (the
# Excel aside column, mat_aside_cols) needs to restate it afterwards -- hence a setter.
#' @keywords internal
#' @noRd
set_role <- function(x, role) {
  vctrs::vec_assert(role, character(), size = 1)
  `attr<-`(x, "role", role)
}

# the base-count / add_pct HELPER columns -- a whole-table count or column percentage belonging to NO
# col_var -- declare a `role` ("n" / "pct") and keep `col_var` "", so every "not a real col_var"
# filter works unchanged.
#' @keywords internal
#' @noRd
# fmt_has_role() -- the ONE "is this fmt column one of these roles?" predicate; normalises the stored
# `role` in one place. data.frame-mapped, so `names(x)[fmt_has_role(x, "n")]` finds the count column.
fmt_has_role <- function(x, roles) {
  if (is.data.frame(x)) return(purrr::map_lgl(x, fmt_has_role, roles))
  is_fmt(x) && as.character(get_role(x))[1] %in% roles
}

# fmt_is_aside() -- IS THIS A SPLIT-OFF ASIDE? (mat_aside_cols, the Excel columns a composite cell
# becomes). The role keeps what the column was carved FROM -- "aside:emp" / "aside:model" on a
# regression column, plain "aside" on a crosstab one -- because the qualifier a percentage prints
# ("obs%" / "adj%") is read off that role, and overwriting it made a crude aside read "row%".
# THE ONE TEST, so no consumer re-derives it from a literal.
#' @keywords internal
fmt_is_aside <- function(x) startsWith(get_role(x) %||% "", "aside")

fmt_is_helper_col <- function(x) fmt_has_role(x, c("n", "pct"))

# the per-column `conf_level` attribute -- the level this column's interval and thresholds were
# computed at.
#
# TWO accessors on purpose. The RAW one (fmt_conf_level_attr) is for the reconcilers: binding two
# columns that never recorded a level carries "unknown" forward, not today's option. The RESOLVED one
# (get_conf_level) is for the colour engine, which must colour at the level the interval was built at.
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
  if (!is.finite(cl)) conf_level_default() else cl
}

#' @keywords internal
#' @noRd
set_conf_level <- function(x, conf_level) {
  conf_level <- vctrs::vec_recycle(vctrs::vec_cast(conf_level, double()), size = 1)
  `attr<-`(x, "conf_level", conf_level)
}

# `degf` and `basis`, the twins of conf_level -- the df an interval is referred to, and HOW it was
# computed. Same two-accessor split for `degf` (RAW for the reconcilers, resolved to Inf = refer to z
# for the engines); `basis` needs only one, its default "n" already being the honest unknown.
#' @keywords internal
#' @noRd
fmt_degf_attr <- function(x) {
  d <- attr(x, "degf", exact = TRUE)
  if (is.null(d)) NA_real_ else d
}

#' @keywords internal
#' @noRd
get_degf <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_dbl(x, get_degf))
  d <- fmt_degf_attr(x)
  if (!is.finite(d) || d <= 0) Inf else d
}

#' @keywords internal
#' @noRd
get_basis <- function(x, ...) {
  if (is.data.frame(x)) return(purrr::map_chr(x, get_basis))
  b <- attr(x, "basis", exact = TRUE)
  if (is.null(b) || !length(b) || is.na(b[1]) || !nzchar(b[1])) "n" else as.character(b)[1]
}

# THE total order of the four inference bases, WEAKEST first. A merge of two columns -- or of two
# tables -- can only claim what its weakest part carried. Declared here, beside the attribute it ranks;
# R/survey-design.R produces the values.
#' @keywords internal
#' @noRd
basis_rank <- function(b) {
  r <- match(b %||% "n", c("n", "weights", "design_partial", "design"))
  if (is.na(r)) 1L else r                     # an unknown basis claims nothing
}

#' @keywords internal
#' @noRd
basis_weakest <- function(a, b) if (basis_rank(a) <= basis_rank(b)) a else b

# THE inference facts of a whole TABLE, DERIVED from its fmt columns. Deriving them (rather than
# storing a table attribute) means a table cannot lose them while keeping its columns. The
# table-level answer is the weakest basis and the smallest df its columns carry -- the weakest-claim
# rule the ptype2 reconcile applies, asked of a whole table at once.
#' @keywords internal
#' @noRd
tab_inference_basis <- function(x) {
  if (!is.data.frame(x)) return(get_basis(x))
  b <- purrr::map_chr(purrr::keep(x, is_fmt), get_basis)
  if (!length(b)) "n" else purrr::reduce(b, basis_weakest)
}

#' @keywords internal
#' @noRd
tab_inference_degf <- function(x) {
  if (!is.data.frame(x)) return(get_degf(x))
  d <- purrr::map_dbl(purrr::keep(x, is_fmt), fmt_degf_attr)
  d <- d[is.finite(d) & d > 0]
  if (!length(d)) Inf else min(d)
}

#' @keywords internal
#' @noRd
set_degf <- function(x, degf) {
  degf <- vctrs::vec_recycle(vctrs::vec_cast(degf, double()), size = 1)
  `attr<-`(x, "degf", degf)
}

#' @keywords internal
#' @noRd
set_basis <- function(x, basis) {
  basis <- vctrs::vec_recycle(vctrs::vec_cast(basis, character()), size = 1)
  `attr<-`(x, "basis", basis)
}

#' @describeIn fmt get the interval method of fmt columns (at \code{fmt} level or \code{tab} level)
#' @return A character vector with the vectors interval method ("" when no interval was computed).
#' @export
get_ci_method <- function(x, ...) UseMethod("get_ci_method")
#' @method get_ci_method default
#' @export
#' @noRd
get_ci_method.default <- function(x, ...) {
  m <- purrr::attr_getter("ci_method")(x)
  if (is.null(m) || is.na(m)) "" else m
}
#' @method get_ci_method tabxplor_fmt
#' @export
#' @noRd
get_ci_method.tabxplor_fmt <- function(x, ...) {
  m <- attr(x, "ci_method", exact = TRUE)
  if (is.null(m) || is.na(m)) "" else m
}
#' @method get_ci_method data.frame
#' @export
#' @noRd
get_ci_method.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_ci_method(.))

#' @keywords internal
#' @noRd
set_ci_method <- function(x, ci_method) {
  ci_method <- vctrs::vec_recycle(vctrs::vec_cast(ci_method, character()), size = 1)
  if (is.na(ci_method)) ci_method <- ""
  `attr<-`(x, "ci_method", ci_method)
}

# Project the table's inference facts onto every fmt column, at each build tail -- the ONE point where
# the call's settings and the finished columns are both in scope. One sweep carries all three
# (conf_level/degf/basis): they are one fact, "how was this column's interval computed". Each argument
# is skipped when absent, so a caller that knows only the level still behaves.
#' @keywords internal
#' @noRd
tab_stamp_inference <- function(tabs, conf_level = NULL, degf = NULL, basis = NULL) {
  ok_num <- function(v) length(v) > 0L && is.finite(v[1])
  ok_chr <- function(v) length(v) > 0L && !is.na(v[1]) && nzchar(v[1])
  if (!ok_num(conf_level) && !ok_num(degf) && !ok_chr(basis)) return(tabs)
  if (is.list(tabs) && !is.data.frame(tabs))
    return(purrr::map(tabs, tab_stamp_inference, conf_level, degf, basis))
  for (nm in names(tabs)) if (is_fmt(tabs[[nm]])) {
    if (ok_num(conf_level)) tabs[[nm]] <- set_conf_level(tabs[[nm]], conf_level[1])
    if (ok_num(degf))       tabs[[nm]] <- set_degf(      tabs[[nm]], degf[1])
    if (ok_chr(basis))      tabs[[nm]] <- set_basis(     tabs[[nm]], basis[1])
  }
  tabs
}



#' @describeIn fmt test function for reference columns (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors is_refcol attributes
#' @export
is_refcol <- function(x, ...) UseMethod("is_refcol")
#' @method is_refcol default
#' @export
#' @noRd
is_refcol.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("refcol")(x)),
         yes = purrr::attr_getter("refcol")(x),
         no  = FALSE)
}
#' @method is_refcol tabxplor_fmt
#' @export
#' @noRd
is_refcol.tabxplor_fmt <- function(x, ...) attr(x, "refcol", exact = TRUE)
#' @method is_refcol data.frame
#' @export
#' @noRd
is_refcol.data.frame <- function(x, ...) purrr::map_lgl(x, ~ is_refcol(.))


#' @describeIn fmt set the "ref_col" attribute of a \code{fmt} vector
#' @return A modified fmt vector.
#' @export
as_refcol     <- function(x, refcol = TRUE) {
  vctrs::vec_assert(refcol, logical(), size = 1)
  `attr<-`(x ,"refcol"  , refcol)
}


# the `color` attribute holds ONE or TWO measures (text channel + optional background). fmt_color_attr()
# returns the FULL vector (the reconcilers read this so the bg channel is not dropped on c()/cast);
# get_color() returns the TEXT channel [1], the scalar contract every consumer relies on.
#' @keywords internal
fmt_color_attr <- function(x) attr(x, "color", exact = TRUE)

#' @describeIn fmt get color (at \code{fmt} level or \code{tab} level)
#' @return A logical vector with the fmt vectors color attributes
#' @export
get_color <- function(x, ...) UseMethod("get_color")
#' @method get_color default
#' @export
#' @noRd
get_color.default     <- function(x, ...) {
  a <- purrr::attr_getter("color")(x)
  if (is.null(a)) "" else a[1]
}
#' @method get_color tabxplor_fmt
#' @export
#' @noRd
get_color.tabxplor_fmt <- function(x, ...) attr(x, "color", exact = TRUE)[1]
#' @method get_color data.frame
#' @export
#' @noRd
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
# positional length-1-or-2 vector [text, background]. This is the STORAGE boundary: it accepts every
# producer's clean measures (diff/ratio/contrib/OR -- legacy combined strings are decoded earlier by
# color_decode_legacy) and validates through the ONE MEASURES table, so it cannot disagree with the
# ARGUMENT boundary normalize_color_spec() (the same call with producer = "tab").
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
  # the canonical STORED spelling of every token (the MEASURES key: "OR"/"or" -> "odds_ratio",
  # "diff" -> "difference"; NA / "no" -> "")
  color <- unname(vapply(color, function(m) {
    k <- measure_key(m)
    if (is.na(k)) as.character(m) else if (!nzchar(k)) "" else k
  }, character(1)))
  if (length(color) > 2L) cli::cli_abort("{.arg color} accepts at most two values (text, background).")
  measure_validate(color)
  if (length(color) == 2L && color[2] == "") color <- color[1]   # trim an empty bg
  color
}

#' @describeIn fmt set the "color" attribute of a \code{fmt} vector
#' @return A modified fmt vector.
#' @export

#' @export
set_color     <- function(x, color) {
  `attr<-`(x, "color", resolve_color_channels(color))
}

# THE significance-policy vocabulary, read by three boundaries: the storage boundary below, tab()'s
# spec parser, and reg_validate_args() (R/reg-resolve.R).
# WARNING: NO roxygen block here -- it sits inside fmt()'s @describeIn chain, so a `#'` comment would
# attach set_color_signif()'s documentation to this constant instead.
COLOR_SIGNIF_VALUES <- c("ignore", "grey_non_signif", "guaranteed_effect")

#' @describeIn fmt set the significance policy attribute of a \code{fmt} vector
#' @export
set_color_signif <- function(x, color_signif) {
  `attr<-`(x, "color_signif", resolve_color_signif(color_signif))
}

# the policy half of the storage boundary, split out of set_color_signif() so the CONSTRUCTOR can
# validate too -- fmt() has no `x` to set the attribute on yet. The twin of resolve_color_channels().
#' @keywords internal
resolve_color_signif <- function(color_signif) {
  color_signif <- color_signif[1]
  if (is.na(color_signif) || color_signif %in% c("", "no")) color_signif <- "ignore"
  # COMPAT: the renamed policy value, accepted silently (the user-facing deprecation fires once in
  # normalize_color_spec()).
  if (identical(color_signif, "color_all_signif")) color_signif <- "guaranteed_effect"
  if (!color_signif %in% COLOR_SIGNIF_VALUES) {
    cli::cli_abort(c("Unknown {.arg color_signif} value {.val {color_signif}}.",
                     "i" = "Valid: {.val {COLOR_SIGNIF_VALUES}}."))
  }
  color_signif
}

# === SECTION: display {} grammar ======================================
# The per-cell `display` field is EITHER a simple token ("pct"/"diff"/"n"/...) OR a glue-style
# COMPOSITE template ("{pct} (n={n})") that renders several fields in ONE value cell (text backends
# only -- get_num()/Excel fall back to the PRIMARY token). These three shared helpers are the single
# source of truth: one gated resolver, one parser, one write-time validator; every consumer that
# dispatches on the display token routes through display_primary().
#
# DESIGN: THE PRIMARY OF A COMPOSITE IS THE FIRST TOKEN OUTSIDE BRACKETS -- and, if every token is
# bracketed, the first one. A bracket is already this package's notation for a subordinate number, so
# the code reads what the reader reads, and a template may put the aside FIRST: "({base}) {est}"
# prints the level then the estimate while the ESTIMATE stays primary -- which is what puts a crude
# and a modelled effect side by side across a regression table. The primary is what carries the
# stars, what get_num() and Excel return, and what the colour gates dispatch on.
#' @keywords internal
#' @noRd
DISPLAY_ASIDE_OPEN  <- c("(", "[")
#' @keywords internal
#' @noRd
DISPLAY_ASIDE_CLOSE <- c(")", "]")
# WARNING: display_primary() is on the O(cells) hot path (get_num/set_num/format) -- keep the
# no-composite fast path a single fixed grepl, and resolve per UNIQUE template, never per cell. The
# composite is per-CELL, not a column attribute (the base count / add_pct are ROWS under pct="col").

# The accepted {} field names and read-side aliases are the `user` / `alias` columns of DISPLAY_TOKENS
# (R/tab-display.R, as DISPLAY_USER_FIELDS / DISPLAY_ALIASES). Facts worth stating here beside
# get_num()/set_num():
#   - `resid` is a DERIVED field (fmt_resid(): the adjusted standardized residual from p-value + the
#     contribution's sign), read-only: get_num() has an arm, set_num() does not (`settable = FALSE`).
#   - `obs` is a real stored FIELD (the value a reg cell is compared to), so it round-trips.
#   - `coef` is DERIVED where the column is multiplicative -- log(estimate) IS the coefficient the
#     model fitted -- and settable all the same: the write mirrors the read through exp().
#   - the canonical internal token is `ratio` (`rr` is a READ-SIDE alias only).
# R/tab-display.R loads AFTER this file; fine, every read below is at RUN time.

# Gated so a column with no composite pays one fixed grepl. A malformed token (no closing brace) is
# left as-is and falls through to get_num()'s default `n` -- never errors.
display_primary <- function(display) {
  comp <- !is.na(display) & grepl("{", display, fixed = TRUE)
  if (any(comp)) {
    # per UNIQUE template: a column holds a handful of them over thousands of cells
    u <- unique(display[comp])
    p <- vapply(u, function(tmpl) {
      seg <- parse_display_template(tmpl)
      if (!length(seg$fields)) tmpl else seg$fields[[seg$primary]]
    }, character(1), USE.NAMES = FALSE)
    display[comp] <- p[match(display[comp], u)]
  }
  # Read-side alias: a legacy token (only `rr` today) -> its canonical internal token. The `%in%`
  # guard keeps the common canonical path off the match() pass.
  al <- DISPLAY_ALIASES
  if (any(display %in% names(al))) {
    hit <- match(display, names(al)); aliased <- !is.na(hit)
    display[aliased] <- unname(al[hit[aliased]])
  }
  display
}

# display_primary_digits() -- the precision the PRIMARY token of a template declared ("{est:3}"), NA
# where it declared none. The sibling of display_primary(), read for the same reason: the top-level
# format() dispatches on the primary, and the Excel number format is finalized there, before the
# composite expander that reads every other token's suffix.
#' @keywords internal
#' @noRd
display_primary_digits <- function(display) {
  out  <- rep(NA_integer_, length(display))
  comp <- !is.na(display) & grepl("{", display, fixed = TRUE)
  if (!any(comp)) return(out)
  u <- unique(display[comp])
  p <- vapply(u, function(tmpl) {
    seg <- parse_display_template(tmpl)
    if (!length(seg$fields)) NA_integer_ else seg$field_digits[[seg$primary]]
  }, integer(1), USE.NAMES = FALSE)
  out[comp] <- p[match(display[comp], u)]
  out
}

# fmt_display_shows() -- does a cell's display ALREADY show this field, ANYWHERE in its template?
# (tests the whole composite, not just the primary token -- e.g. the html tooltip uses it to suppress
# a line the cell already shows.) `row` (optional, lazy) resolves the scale-relative `est` / `base`
# tokens first, so a cell printing "{base}" counts as showing the percentage it prints.
#' @keywords internal
fmt_display_shows <- function(display, token, row = NULL) {
  # `n_range` IS the count (one number, or the min-max the base column reports): a cell showing it
  # must not have the tooltip repeat `n:` beside it.
  resolve <- function(v) {
    v <- if (is.null(row)) v else fmt_resolve_scale_tokens(v, row)
    if (identical(token, "n")) v[!is.na(v) & v == "n_range"] <- "n"
    v
  }
  out  <- !is.na(display) & resolve(display) == token
  comp <- !is.na(display) & grepl("{", display, fixed = TRUE)
  if (any(comp)) {
    for (tmpl in unique(display[comp]))
      out[comp & display == tmpl] <- token %in% resolve(parse_display_template(tmpl)$fields)
  }
  out
}

# Split ONE template into ordered segments (called once per unique template in a column, which are
# ~uniform). Returns pieces (literals + {token}s in order), is_tok (which pieces are field tokens),
# fields (the alias-resolved internal tokens, in order) and `primary` (the INDEX into `fields` of the
# token the cell is really about -- see the DESIGN note above: the first one outside parentheses).
# A degenerate template with no {field} (e.g. malformed) yields is_tok all FALSE -> the format()
# branch leaves those cells plain.
parse_display_template <- function(tmpl) {
  raw_pieces <- regmatches(tmpl, gregexpr("\\{[^{}]+\\}|[^{}]+", tmpl))[[1]]
  raw_is_tok <- startsWith(raw_pieces, "{")
  # BRACKET DEPTH, walked character by character: a token is an ASIDE when it sits inside a pair of
  # brackets -- "(" or "[". Depth is clamped at 0, so a stray closer can never promote a later token,
  # and an opener never closed leaves everything after it an aside (which is what it looks like).
  # Literal pieces cannot contain "{" or "}": the parser splits on those, so a token can never be
  # nested inside another.
  # ⚠ THE LITERALS ARE SPLIT AT THE TOP-LEVEL BRACKET BOUNDARIES, so every piece belongs to exactly
  # ONE group -- " (n=" straddles the top level and group 1, ") (" closes one group and opens the
  # next. Without the split, dropping a spent group would eat its neighbour's bracket.
  pieces <- character(0); is_tok <- logical(0); group <- integer(0)
  add    <- function(p, tk, g) {
    if (!nzchar(p)) return(invisible(NULL))
    pieces <<- c(pieces, p); is_tok <<- c(is_tok, tk); group <<- c(group, g)
  }
  depth <- 0L; grp <- 0L
  for (j in seq_along(raw_pieces)) {
    if (raw_is_tok[j]) { add(raw_pieces[j], TRUE, if (depth > 0L) grp else 0L); next }
    ch <- strsplit(raw_pieces[j], "", fixed = TRUE)[[1]]
    cur <- character(0); cur_g <- if (depth > 0L) grp else 0L
    for (c1 in ch) {
      if (c1 %in% DISPLAY_ASIDE_OPEN) {
        if (depth == 0L) { add(paste0(cur, collapse = ""), FALSE, cur_g); cur <- character(0)
                           grp <- grp + 1L; cur_g <- grp }
        depth <- depth + 1L; cur <- c(cur, c1)
      } else if (c1 %in% DISPLAY_ASIDE_CLOSE && depth > 0L) {
        depth <- depth - 1L; cur <- c(cur, c1)
        if (depth == 0L) { add(paste0(cur, collapse = ""), FALSE, cur_g); cur <- character(0)
                           cur_g <- 0L }
      } else cur <- c(cur, c1)
    }
    add(paste0(cur, collapse = ""), FALSE, cur_g)
  }
  fields <- character(0); primary <- 1L; field_group <- integer(0)
  field_digits <- integer(0)
  if (any(is_tok)) {
    rw  <- trimws(gsub("[{}]", "", pieces[is_tok]))
    # A TOKEN MAY CARRY ITS OWN PRECISION -- "{base:1}", "{est:3}". Digits ARE a display property, so
    # this is where they belong: the cell's `digits` field is one number for a whole cell, and only
    # the template can say that the aside reads at one decimal while the estimate reads at three.
    dg  <- suppressWarnings(as.integer(sub("^[^:]*:", "", rw)))
    dg[!grepl(":", rw, fixed = TRUE)] <- NA_integer_
    rw  <- sub(":.*$", "", rw)
    hit <- rw %in% names(DISPLAY_ALIASES)
    rw[hit] <- unname(DISPLAY_ALIASES[rw[hit]])
    fields       <- rw
    field_digits <- dg
    field_group  <- group[is_tok]
    if (any(field_group == 0L)) primary <- which(field_group == 0L)[[1]]
  }
  list(pieces = pieces, is_tok = is_tok, group = group, fields = fields,
       field_digits = field_digits, field_group = field_group, primary = primary)
}

# WHICH PIECES SURVIVE when some fields render nothing, over the WHOLE column. ONE rule, read by both
# writers: display_write_col(), which rewrites the template, and format()'s expander, which must
# prune too -- a raw set_display(col, "{est} ({base})") never reaches display_write_col(), and
# without this it printed empty parentheses. A bracket GROUP whose every token is empty goes whole,
# brackets and inner literals included; a spent top-level token goes with its adjacent separator.
# ⚠ THE PRIMARY AND ITS GROUP ARE NEVER DROPPED: "({n_range})" is a template whose only token is
# bracketed, and blanking it would blank the cell.
#' @keywords internal
#' @noRd
display_template_keep <- function(seg, empty) {
  keep <- rep(TRUE, length(seg$pieces))
  if (!length(seg$fields) || !any(empty)) return(keep)
  prim_g <- seg$field_group[[seg$primary]]
  tok_j  <- which(seg$is_tok)
  # the separator a dropped run carried away with it: the preceding top-level blank, else the
  # following one -- otherwise "{est} ({base})" would leave a trailing space behind the aside.
  drop_sep <- function(from, to) {
    nb <- c(from - 1L, to + 1L)
    nb <- nb[nb >= 1L & nb <= length(keep)]
    nb <- nb[!seg$is_tok[nb] & seg$group[nb] == 0L & keep[nb] & !nzchar(trimws(seg$pieces[nb]))]
    if (length(nb)) keep[nb[[1]]] <<- FALSE
  }
  for (g in unique(seg$field_group[seg$field_group > 0L])) {
    if (g == prim_g) next
    if (!all(empty[seg$field_group == g])) next
    j <- which(seg$group == g)
    keep[j] <- FALSE
    drop_sep(min(j), max(j))
  }
  for (i in which(empty & seg$field_group == 0L)) {
    if (i == seg$primary) next
    j <- tok_j[[i]]
    keep[j] <- FALSE
    drop_sep(j, j)
  }
  keep
}

# The template a column really needs, once the fields empty EVERYWHERE are pruned out.
#' @keywords internal
#' @noRd
display_prune_template <- function(seg, empty)
  trimws(paste0(seg$pieces[display_template_keep(seg, empty)], collapse = ""))

# WRITE-time: VALIDATE a `display=` {} template and return it. Composites use the {} grammar ONLY --
# one consistent syntax, no curated recipes; the named LAYOUTS are DISPLAY_PRESETS, resolved to a
# template before they get here. Checks balanced non-empty braces and known field names (aliases
# included, so "{OR}" passes). The ONLY place a bad `display=` value aborts.
#' @keywords internal
validate_display_template <- function(recipe) {
  recipe <- recipe[[1]]
  # Ergonomics / back-compat: a bare field name (no braces) that is a known display field is treated as
  # the single-field template "{field}", so e.g. display = "ci" == display = "{ci}" (and "diff"/"pct"/...).
  # One general rule, not an ad-hoc "ci" case. A genuinely unknown bare value still hits the abort below.
  if (!grepl("[{}]", recipe) &&
      recipe %in% c(DISPLAY_USER_FIELDS, names(DISPLAY_ALIASES))) {
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
  # the optional per-token precision -- "{base:1}" -- validated here and stripped before the
  # known-field check below, so a token spelling exists in exactly one place (parse_display_template).
  dg_used     <- fields_used[grepl(":", fields_used, fixed = TRUE)]
  fields_used <- sub(":.*$", "", fields_used)
  bad_dg <- dg_used[!grepl("^[^:]+:[0-6]$", dg_used)]
  if (length(bad_dg))
    cli::cli_abort(c("Invalid precision in {.arg display} token{?s} {.val {bad_dg}}.",
                     "i" = "Write {.code {{token}}:{{digits}}}, digits 0-6, e.g. {.code {{base}}:1}."))
  if (opens != closes || length(toks) != opens || any(!nzchar(fields_used))) {
    cli::cli_abort(c("Malformed {.arg display} template {.val {recipe}}.",
                     "i" = "Use balanced, non-empty tokens, e.g. {.code {{pct}} (n={{n}})}."))
  }
  unknown <- setdiff(fields_used, c(DISPLAY_USER_FIELDS, names(DISPLAY_ALIASES)))
  if (length(unknown)) {
    cli::cli_abort(c("Unknown field{?s} {.val {unknown}} in {.arg display} template.",
                     "i" = "Valid fields: {.val {DISPLAY_USER_FIELDS}}."))
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
#' tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "difference")
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


# === SECTION: the ESTIMATE's scale =================================================================
#
# `MEASURES` above says what a COLOUR MEASURE is; this says what a COLUMN ESTIMATES. The `scale`
# attribute is one key into EST_SCALES, the declared record library; fmt_scale_of() resolves the
# per-column parts (the ladder, SD(Y), the secondary axis). What a column estimates is STORED, never
# re-derived from a display, a family, or whether `var` is non-NA. The colour measure and the
# estimate may live on different ladders (a `color = "adjustment"` Model_OR column has odds_ratio
# gridlines and an adj_ratio colour ladder), which is why each is declared separately.
#
# EST_SCALES columns:
#   kind       "effect" (there is a null to draw, and a stored interval can be tested against it)
#              | "level" (a percentage / mean / count: no null). A level column's own one-proportion
#              interval has no reference null, so the significance gate must not read it.
#   geometry   the WORD the producers' arguments resolve into: "ratio" | "difference" | "log" | "level".
#   var_kind   what the column summarises: "pct" | "mean" | "count" | "coef". With `pct_type` this is
#              the old `type` attribute, split into its two honest halves.
#   ladder     which break scale a colour MEASURE reads on this column: "pct" | "std" | "log"
#              (MEASURES$<m>$scale is a 3-entry map keyed by it).
#   neutral    the null value, on the estimate's own scale.
#   trans      the axis transform -- "log10" makes x2 and 1/2 equidistant from 1 (why a ratio forest
#              plot is logarithmic).
#   mult       multiplicative fold (the fmt_color_slots center-1 rule).
#   is_pct     the estimate renders x100.
#   est_field  the fmt field the estimate lives in (fmt_center_field() reads this).
#   est_display  the DISPLAY_TOKENS token `{est}` borrows on this scale, so "the estimate, whatever
#              this column estimates" renders exactly as the column's own estimate always did. Its
#              token's `field` IS est_field (checked at load).
#   base_display  the token `{base}` borrows: the LEVEL beside the estimate (a percentage, a mean, a
#              count). NA where the level is ambiguous -- on a link scale a coefficient may sit over a
#              probability or over a mean, and guessing would be a lie; `{base}` renders void there.
#   est_digits   the ESTIMATE token's own precision, a FLOOR (absent = the cell's). The mirror of base_digits,
#              for the scales where the estimate is FINER than the level it sits on: a per-item odds
#              ratio wants two decimals while the mean score under it wants one, and one `digits`
#              per cell cannot say both. ⚠ it is not DISPLAY_TOKENS$min_digits: that one is a
#              default a cell overrides by asking, this one is the scale's own statement.
#   base_digits  the LEVEL's own precision, absent = the cell's. One `digits` per cell serves every
#              token of it, so an estimate needing a decimal (a risk difference in points) used to
#              drag its percentage aside to "50.8 %". Declared only on the EFFECT scales, where
#              `{base}` really is an aside -- on a LEVEL scale `{base}` IS the estimate and the
#              column's own `digits` is the user's answer. A template may override it per token,
#              "{base:2}", which is how `tab_reg(digits = c(base = 2))` is written.
#   const_display the token a regression's BASELINE row renders: the quantity this column's effects
#              OPERATE ON. Odds ratios multiply odds, so an odds column shows the baseline odds; risk
#              and rate ratios multiply the level, and differences add to it, so those show the level
#              itself; a coefficient adds on the link scale. NA on the level scales, which have no
#              baseline row. A LEVEL token here also means the row carries no p-value: there is no
#              null a percentage or a mean could be tested against.
#   unit       the axis title, as a KEY (translated at render, never here).
#   break_key  the ESTIMATE's ladder in color_scales(); NA = no ladder, use the device's own breaks.
#   gap_key    the adj_* ladder its GAP reads (fmt_gap_scale_key() reads this).
#   label_meas which MEASURES row supplies this scale's glyphs -- the forest axis's break labels AND
#              the multiplicative cell rendering ("odds_ratio" -> "1/2" / "2", "ratio" -> "/2" / "x2").
#              ONE declaration per measure, so a cell, its ladder and its axis cannot disagree.
#              WARNING: a MEASURES KEY -- a rename must reach here or both lose their glyphs.
#   sd_from    where the SD-standardized ladder's divisor comes from: a regression column's stored
#              var(Y) ("var") or a crosstab cell's REFERENCE variance ("ref_var"). NULL = none.
#   sec        NULL, or the secondary axis this scale needs to stay readable: when the colour ladder
#              lives on a different scale from the printed estimate, that ladder's scale is the axis.
#' @keywords internal
EST_SCALES <- list(
  odds_ratio = list(kind = "effect", geometry = "ratio", var_kind = "pct",  ladder = "pct",
                    neutral = 1,  trans = "log10",   mult = TRUE,  is_pct = FALSE,
                    est_field = "or",    unit = "or",    default_display = "pct",
                    est_display = "or", base_display = "pct", base_digits = 0L, const_display = "or",
                    break_key = "odds_ratio", gap_key = "adj_ratio",
                    label_meas = "odds_ratio", sec = NULL),
  # THE TWO SUMMED-SCORE ROWS (`tab_reg(trials =)`): a multiplicative effect on the PER-ITEM
  # probability, sitting on the mean SCORE -- the average number of "yes" out of `trials`, which is
  # what a reader of a battery of items wants. ⚠ "score" names the LEVEL, not the ratio, so
  # neither can borrow its ungrouped twin's row: `{base}` would fold a score into `pct` (x100, "%")
  # and the column would claim var_kind "pct" to every tooltip and plot.
  # WARNING: they are TWO rows for the same reason odds_ratio and pct_ratio are -- an odds ratio and
  # a risk ratio are different quantities with different fields and different glyphs, and folding
  # them printed every summed-score RR as "1/x". An incidence-rate ratio is in neither: a rate ratio
  # is a ratio of means, so it is `mean_ratio`, whose `unit` already says so.
  score_odds_ratio = list(kind = "effect", geometry = "ratio", var_kind = "mean", ladder = "pct",
                    neutral = 1,  trans = "log10",   mult = TRUE,  is_pct = FALSE,
                    est_field = "or",    unit = "or", default_display = "mean",
                    # the aside is a mean SCORE out of `trials` -- one decimal, whatever precision
                    # the odds ratio beside it is read at (`or` declares a minimum of 2).
                    est_display = "or", est_digits = 2L, base_display = "mean", const_display = "or",
                    break_key = "odds_ratio", gap_key = "adj_ratio",
                    label_meas = "odds_ratio", sec = NULL),
  score_ratio = list(kind = "effect", geometry = "ratio", var_kind = "mean", ladder = "pct",
                    neutral = 1,  trans = "log10",   mult = TRUE,  is_pct = FALSE,
                    est_field = "ratio", unit = "ratio", default_display = "mean",
                    est_display = "ratio", est_digits = 2L, base_display = "mean",
                    const_display = "mean",
                    break_key = "pct_ratio", gap_key = "adj_ratio",
                    label_meas = "ratio", sec = NULL),
  pct_ratio  = list(kind = "effect", geometry = "ratio", var_kind = "pct",  ladder = "pct",
                    neutral = 1,  trans = "log10",   mult = TRUE,  is_pct = FALSE,
                    est_field = "ratio", unit = "ratio", default_display = "pct",
                    est_display = "ratio", base_display = "pct", base_digits = 0L, const_display = "pct",
                    break_key = "pct_ratio",  gap_key = "adj_ratio",
                    label_meas = "ratio", sec = NULL),
  mean_ratio = list(kind = "effect", geometry = "ratio", var_kind = "mean", ladder = "std",
                    neutral = 1,  trans = "log10",   mult = TRUE,  is_pct = FALSE,
                    est_field = "ratio", unit = "rate_ratio", default_display = "mean",
                    est_display = "ratio", est_digits = 2L, base_display = "mean",
                    const_display = "mean",
                    break_key = "mean_ratio", gap_key = "adj_ratio",
                    label_meas = "ratio", sec = NULL),
  # a beta / a count AME: printed in the OUTCOME's units, coloured on the SD-standardized ladder.
  # DESIGN: `est_display = "diff"`, like the crosstab row below it -- an identity-link beta IS a mean
  # difference, and naming it "coef" gave one quantity two names (the header already says `diff`).
  # `coef` survives only where the model's own scale is logged, and there it says log(OR) -- see
  # fmt_coef_label().
  # WARNING: raw_diff and mean_diff are NOT one row with two `sd_from` values -- their `gap_key`
  # differs too (adj_diff_std vs adj_diff), and folding them would mean re-deriving both from
  # `model_family`, i.e. re-introducing the dispatch this key exists to delete. Every stamping site
  # knows which of the two it is building.
  raw_diff   = list(kind = "effect", geometry = "difference", var_kind = "coef", ladder = "std",
                    neutral = 0,  trans = "identity", mult = FALSE, is_pct = FALSE,
                    est_field = "diff",  unit = "units", default_display = "n",
                    est_display = "diff", base_display = "mean", const_display = "mean",
                    break_key = "mean_diff",  gap_key = "adj_diff_std",
                    label_meas = "difference", sec = "sd", sd_from = "var"),
  # a crosstab MEAN difference: the same ladder, standardized by the REFERENCE cell's SD rather than
  # by a stored var(Y) -- which is exactly the split fmt_color_plan()'s sd_ref block already makes.
  mean_diff  = list(kind = "effect", geometry = "difference", var_kind = "mean", ladder = "std",
                    neutral = 0,  trans = "identity", mult = FALSE, is_pct = FALSE,
                    est_field = "diff",  unit = "units", default_display = "mean",
                    est_display = "diff", base_display = "mean", const_display = "mean",
                    break_key = "mean_diff",  gap_key = "adj_diff",
                    label_meas = "difference", sec = "sd", sd_from = "ref_var"),
  # measure = "raw_coefficient": printed on the link scale, coloured on the logged odds_ratio ladder (what
  # `ladder = "log"` selects).
  log_coef   = list(kind = "effect", geometry = "log", var_kind = "coef", ladder = "log",
                    neutral = 0,  trans = "identity", mult = FALSE, is_pct = FALSE,
                    est_field = "diff",  unit = "log",   default_display = "n",
                    est_display = "coef", base_display = NA_character_, const_display = "coef",
                    break_key = "log_odds",   gap_key = "adj_diff_log",
                    label_meas = "difference", sec = "exp"),
  points     = list(kind = "effect", geometry = "difference", var_kind = "pct", ladder = "pct",
                    neutral = 0,  trans = "identity", mult = FALSE, is_pct = TRUE,
                    est_field = "diff",  unit = "points", default_display = "pct",
                    est_display = "diff", base_display = "pct", base_digits = 0L, const_display = "pct",
                    break_key = "pct_diff",   gap_key = "adj_diff",
                    label_meas = "difference", sec = NULL),
  # the three LEVEL scales: a cell percentage / a mean / a count. No null to draw (the reference is a
  # per-column value), and no ladder of their own -- a level column's colour ladder grades its
  # DIFFERENCE, so putting it on the level axis would be a lie. `gap_key` is "adj_diff" only so
  # fmt_gap_scale_key() stays uniform on a column no gap measure can ride.
  level_pct  = list(kind = "level",  geometry = "level", var_kind = "pct",   ladder = "pct",
                    neutral = NA_real_, trans = "identity", mult = FALSE, is_pct = TRUE,
                    est_field = "pct",   unit = "pct",   default_display = "pct",
                    est_display = "pct", base_display = "pct", const_display = NA_character_,
                    break_key = NA_character_, gap_key = "adj_diff",
                    label_meas = "difference", sec = NULL),
  level_mean = list(kind = "level",  geometry = "level", var_kind = "mean",  ladder = "std",
                    neutral = NA_real_, trans = "identity", mult = FALSE, is_pct = FALSE,
                    est_field = "mean",  unit = "units", default_display = "mean",
                    est_display = "mean", base_display = "mean", const_display = NA_character_,
                    break_key = NA_character_, gap_key = "adj_diff", sd_from = "ref_var",
                    label_meas = "difference", sec = NULL),
  level_n    = list(kind = "level",  geometry = "level", var_kind = "count", ladder = "std",
                    neutral = NA_real_, trans = "identity", mult = FALSE, is_pct = FALSE,
                    est_field = "n",     unit = "count", default_display = "n",
                    est_display = "n", base_display = "n", const_display = NA_character_,
                    break_key = NA_character_, gap_key = "adj_diff", sd_from = "ref_var",
                    label_meas = "difference", sec = NULL),
  # THE NEUTRAL: what binding two columns of unlike scales collapses to (fmt_attr_rules). Its content
  # is level_pct's, so vec_arith's mismatch warning has a real fact to test, not a magic string.
  mixed      = list(kind = "level",  geometry = "level", var_kind = "pct",   ladder = "pct",
                    neutral = NA_real_, trans = "identity", mult = FALSE, is_pct = TRUE,
                    est_field = "pct",   unit = "pct",   default_display = "n",
                    est_display = "pct", base_display = "pct", const_display = NA_character_,
                    break_key = NA_character_, gap_key = "adj_diff",
                    label_meas = "difference", sec = NULL)
)

#' @keywords internal
EST_SCALE_KEYS <- names(EST_SCALES)
#' @keywords internal
est_var_kind <- function(key) (EST_SCALES[[key]] %||% EST_SCALES[["mixed"]])$var_kind
# WARNING: `odds_ratio` / `pct_ratio` / `points` default to "pct" and NOT to their own `est_field`.
# That is today's behaviour on an exported constructor: changing it is a user-visible decision, not a
# refactor.
#' @keywords internal
est_default_display <- function(key) (EST_SCALES[[key]] %||% EST_SCALES[["mixed"]])$default_display
#' @keywords internal
PCT_TYPES <- c("row", "col", "all", "all_tabs", "none")

# `kind`: forest_plot(what =)'s override -- the level/effect twin of the column's scale (a column with
# no stored contrast interval still HAS a difference, just no whisker).
#' @keywords internal
fmt_scale_key <- function(x, kind = c("auto", "effect", "level")) {
  key <- get_scale(x)
  switch(match.arg(kind),
         auto   = key,
         level  = EST_SCALES[[key]]$level_twin  %||% key,
         effect = EST_SCALES[[key]]$effect_twin %||% key)
}

for (.k in EST_SCALE_KEYS) {
  EST_SCALES[[.k]]$level_twin  <- switch(EST_SCALES[[.k]]$var_kind,
                                         mean = "level_mean", count = "level_n", "level_pct")
  EST_SCALES[[.k]]$effect_twin <- if (identical(EST_SCALES[[.k]]$kind, "effect")) .k
                                  else switch(EST_SCALES[[.k]]$var_kind,
                                              mean = "mean_diff", count = "mean_diff", "points")
}
rm(.k)

#' @keywords internal
fmt_scale_row <- function(x) EST_SCALES[[fmt_scale_key(x)]] %||% EST_SCALES[["mixed"]]

# The journal-convention opt-out: print a ratio raw ("0.83") rather than inverted ("/1.2").
#' @keywords internal
tx_ratio_print_raw <- function() identical(getOption("tabxplor.ratio_print", "inverse"), "raw")

# display_segment_of() -- the TEMPLATE of one field, with the literals that belong to it: field `i` of
# "{pct} (n={n})" is "(n={n})", of "{mean} (sigma{sd})" is "(sigma{sd})". A bracket group is taken
# whole (that is what a group IS, and its pieces are contiguous); a top-level field is taken bare.
# ⚠ Only where the group holds ONE field: with two, the group's text belongs to neither alone.
# Read by mat_aside_cols(), so a split-off aside prints what it printed inside the composite cell.
#' @keywords internal
display_segment_of <- function(seg, i) {
  pj <- which(seg$is_tok)[[i]]
  g  <- seg$group[[pj]]
  if (g == 0L || sum(seg$is_tok & seg$group == g) != 1L) return(seg$pieces[[pj]])
  trimws(paste0(seg$pieces[seg$group == g], collapse = ""))
}

# fmt_mult_plan() -- WHICH CELLS PRINT A MULTIPLICATIVE DISTANCE, and under which measure's glyphs.
# A multiplicative cell shows its DISTANCE from the neutral, not its raw value: below the neutral it
# reads the inverse, so "half" reads as strongly as "double". THE ONE definition, read three times --
# the text rendering (format()), the Excel number-format sections, and the Excel reading VALUE
# (fmt_excel_value) -- so a cell, its format code and the number under it cannot disagree.
#' @keywords internal
fmt_mult_plan <- function(x, display = NULL, scl = NULL) {
  scl     <- scl %||% fmt_scale_row(x)
  display <- display %||% fmt_resolve_scale_tokens(display_primary(get_display(x)), scl)
  tok     <- c(ratio = "ratio", or = "odds_ratio")
  cells   <- !is.na(display) & (display %in% names(tok) |
                                  (isTRUE(scl$mult) & display %in% c("obs", "gap")))
  # a scale-relative cell takes the COLUMN's measure (EST_SCALES$label_meas): `{est}` on a mean-ratio
  # column reads "/2", on an odds-ratio one "1/2".
  meas <- rep(NA_character_, length(display))
  meas[cells] <- unname(ifelse(display[cells] %in% names(tok), tok[display[cells]],
                               scl$label_meas %||% NA_character_))
  list(cells = cells, measure = meas)
}

# ... and the two glyphs that measure prints on each side of its neutral (MEASURES' own pair, the one
# the legend ladder and the forest axis also print).
#' @keywords internal
fmt_mult_glyphs <- function(measure) {
  g <- function(side) vapply(measure, function(k)
    if (is.na(k)) "" else MEASURES[[k]][[side]] %||% "", character(1), USE.NAMES = FALSE)
  list(over = g("break_over"), under = g("break_under"))
}

# fmt_excel_value() -- THE NUMBER A WORKBOOK CELL HOLDS.
# Excel cannot compute inside a number format, so "1/2.11" cannot be shown from a stored 0.474 the way
# the console shows it. The cell holds the READING VALUE instead: the fold, signed by its direction --
# `x` at or above the neutral, `-1/x` below it -- which an unconditional two-section code prints as
# "2.11" and "1/2.11" (Excel drops the minus in a section it was not written into). So the workbook
# says exactly what the screen says, and stays a real number: it sorts and filters in the direction a
# reader reads, and takes the reader's own decimal separator. ?tab_xl says how to get the raw ratio
# back (=IF(A2<0, -1/A2, A2)); `ratio_cells = "raw"` stores it untransformed instead.
#' @keywords internal
fmt_excel_value <- function(x, fold = TRUE) {
  v <- get_num(x)
  if (!fold || tx_ratio_print_raw()) return(v)
  scl  <- fmt_scale_row(x)
  disp <- fmt_resolve_scale_tokens(display_primary(get_display(x)), scl)
  mp   <- fmt_mult_plan(x, disp, scl)
  dg   <- get_digits(x)
  md   <- unname(DISPLAY_MIN_DIGITS[disp]); hit <- !is.na(md) & dg == 0L; dg[hit] <- md[hit]
  # a value ROUNDING to the neutral keeps the over side, never "1/1.00" -- format()'s own `one` rule
  sel  <- mp$cells & !is.na(v) & v > 0 & v < 1 & round(1 / v, dg) > 1
  v[sel] <- -1 / v[sel]
  v
}

# fmt_coef_label() -- THE NAME of the `coef` token, composed the way the header is
# (reg_word_logged): a coefficient is only worth calling one where the model's own scale is LOGGED,
# and there the truth is log(OR) / log(IRR) / log(cumOR). On an additive column the coefficient IS
# the difference and says so, which is why an identity-link beta never prints "coef".
# The acronym comes from the family (reg_own_word); a crosstab, which has none, reads its scale.
#' @keywords internal
fmt_coef_label <- function(x) {
  scl <- fmt_scale_row(x)
  if (!isTRUE(scl$mult) && !identical(scl$geometry, "log")) return("diff")
  w <- reg_own_word(get_model_family(x))
  if (is.na(w) && isTRUE(scl$mult))
    w <- switch(scl$label_meas %||% "", odds_ratio = "OR", ratio = "ratio", NA_character_)
  if (is.na(w)) "coef" else paste0("log(", w, ")")
}

# THE LEVEL a column sits on: the field its scale names for `{base}`, or NA where the scale names
# none. One read, shared by the display grammar and by the plot's `what = "level"`.
#' @keywords internal
est_level_of <- function(x) {
  b <- fmt_scale_row(x)$base_display
  if (is.null(b) || is.na(b)) return(rep(NA_real_, vctrs::vec_size(x)))
  as.double(vctrs::field(x, b))
}

#' @keywords internal
fmt_var_kind <- function(x) {
  if (is.data.frame(x)) return(purrr::map_chr(x, fmt_var_kind))
  fmt_scale_row(x)$var_kind
}

# The short human LABEL of a column -- "row%"/"col%"/"all%"/"all_tabs%"/"mean"/"n"/"coef"/"mixed".
# A rendered string (pillar ptype abbreviation, arithmetic-warning wording), never a fact to branch
# on: read `pct_type` / `var_kind` for that.
#' @keywords internal
fmt_kind_label <- function(x) {
  if (is.data.frame(x)) return(purrr::map_chr(x, fmt_kind_label))
  base <- get_pct_type(x)
  if (!identical(base, "none")) return(paste0(base, "%"))
  switch(fmt_var_kind(x), mean = "mean", count = "n", coef = "coef", "mixed")
}

# THE decimals floor for an interval BOUND, read by `{ci}` and `{moe}` alike. DERIVED, not declared:
# `mult` and `is_pct` already say everything it needs. 2 where the estimate is multiplicative (at 1 dp
# the two bounds routinely round equal and the bracket collapses to a point), 0 where it is a
# percentage (a point is already a readable unit), 1 where it is in the outcome's OWN units, whose
# magnitude is unknown -- there 0 decimals can round a whole interval away and a "+/-0" says nothing.
#' @keywords internal
fmt_ci_digits <- function(row) if (isTRUE(row$mult)) 2L else if (isTRUE(row$is_pct)) 0L else 1L

#' @keywords internal
fmt_center_field <- function(x) fmt_scale_row(x)$est_field

# THE `{est}` / `{base}` resolution, in one place. Both tokens are scale-relative: they name a ROLE
# ("the estimate", "the level beside it") and each column answers with the token it has always
# rendered. Resolving here rather than giving them their own get_num()/format() arms is what makes
# every existing mask, glyph, reference annotation and Excel code apply unchanged.
# A scale declaring NO level (`base_display = NA`, the link scales) resolves to `blank`: "there is
# nothing to print here". get_num() then returns NA, so display_note_empty() names what would fill it.
# `row` is evaluated LAZILY (R promises), so the common path pays no fmt_scale_row() lookup.
#' @keywords internal
fmt_resolve_scale_tokens <- function(display, row) {
  hit <- display %in% c("est", "base")
  if (!any(hit)) return(display)
  void <- function(tok) if (is.null(tok) || is.na(tok)) "blank" else tok
  display[hit & display == "est" ] <- void(row$est_display)
  display[hit & display == "base"] <- void(row$base_display)
  display
}

# =====================================================================================================
# fmt_display_label() -- THE NAME OF WHAT A COLUMN HOLDS, built from the column's own display template:
# each {token} replaced by the token's declared `label` (DISPLAY_TOKENS), the literals kept. So the
# name mimics the cell -- "row% (n)", "OR (row%)", "(row%) OR" -- and a layout can never be named two
# different ways: the console type tag, the exports' unit line and an Excel aside column's header all
# read this one builder.
#
# DESIGN: THE PCT TYPE IS PRINTED BY THE `pct` TOKEN'S OWN LABEL, and by nothing else. A crosstab
# whose template prints no percentage keeps the old prefixed form ("row%-diff", "row%-OR"), because
# there the direction of reading is the only thing that says what the deviation is a deviation OF;
# a regression column never takes it (its `role` is the declared fact that separates them), which is
# what turns tab_reg()'s misleading "row%-or" into "OR (row%)".
#
# WARNING: it must never error -- an fmt column extracted from its table keeps its fields and its
# attributes but nothing else, and the tag is asked for on print.
#' @keywords internal
#' @noRd
display_token_label <- function(token, x) {
  lab <- DISPLAY_TOKEN_LABELS[[token]]
  if (is.null(lab)) return(token)
  if (is.function(lab)) lab <- tryCatch(lab(x), error = function(e) token)
  if (length(lab) != 1L || is.na(lab)) return(token)
  as.character(lab)
}

# One template -> its tokens, scale-resolved and in order. A bare token (no braces) is the one-token
# case, which is most columns.
#' @keywords internal
#' @noRd
display_template_tokens <- function(tmpl, scl) {
  if (!grepl("{", tmpl, fixed = TRUE)) return(fmt_resolve_scale_tokens(display_primary(tmpl), scl))
  fmt_resolve_scale_tokens(parse_display_template(tmpl)$fields, scl)
}

# One template -> its name.
# ⚠ A LABEL KEEPS THE TEMPLATE'S STRUCTURE, NOT ITS WORDING: of every literal only its brackets and
# one space survive, so "{pct} (n={n})" reads "row% (n)" and the sparkline glyph run mat_reg_spark()
# appends to a base-count template stays out of the name entirely.
# ⚠ THE PRIMARY'S OWN BRACKETS ARE DROPPED: in a label a bracket marks an ASIDE, and the primary is
# never one -- a Total cell reduced to "({n_range})" is a count column, and reads "n", not "(n)".
#' @keywords internal
#' @noRd
display_template_label <- function(tmpl, x, scl) {
  if (!grepl("{", tmpl, fixed = TRUE))
    return(display_token_label(fmt_resolve_scale_tokens(display_primary(tmpl), scl), x))
  seg <- parse_display_template(tmpl)
  if (!length(seg$fields)) return(tmpl)
  out <- seg$pieces
  out[!seg$is_tok] <- gsub("[[:space:]]+", " ", gsub("[^][(){}[:space:]]", "", out[!seg$is_tok]))
  out[seg$is_tok]  <- vapply(fmt_resolve_scale_tokens(seg$fields, scl),
                             display_token_label, character(1), x = x)
  prim_g <- seg$field_group[[seg$primary]]
  if (prim_g > 0L) {                                # the whole cell is one bracket group
    j <- which(seg$group == prim_g & !seg$is_tok)
    out[j] <- gsub("[][()]", "", out[j])
  }
  # a literal reduced to its punctuation may leave a space hugging a bracket ("{mean} (cv {cv})" ->
  # "mean ( cv)"): the brackets are structure, the space was wording.
  trimws(gsub("[[:space:]]+([])])", "\\1", gsub("([[(])[[:space:]]+", "\\1",
                                                paste0(out, collapse = ""))))
}

# THE COLUMN NAME AN EXPORT HEADER USES: fmt_display_label(), minus any aside whose rendered cell
# already says its own name ("cv 36 %"). A header that repeated it would say it twice; the console
# type tag keeps it, because there the tag is the only thing naming the layout.
#' @keywords internal
#' @noRd
fmt_header_label <- function(x) {
  d <- unique(get_display(x))
  d <- d[!is.na(d) & grepl("{", d, fixed = TRUE)]
  if (length(d) && length(DISPLAY_SELF_NAMED)) {
    tmpl <- d[[which.max(vapply(d, function(t) length(parse_display_template(t)$fields), integer(1)))]]
    seg  <- parse_display_template(tmpl)
    tok  <- fmt_resolve_scale_tokens(seg$fields, fmt_scale_row(x))
    hit  <- setdiff(which(tok %in% DISPLAY_SELF_NAMED), seg$primary)
    if (length(hit))
      return(display_template_label(display_prune_template(seg, seq_along(tok) %in% hit),
                                    x, fmt_scale_row(x)))
  }
  # the TAG style: its prefix is what says a bare deviation is a deviation of means
  # ("mean-ci" for a column of intervals), which is the whole job of a level header.
  fmt_display_label(x, "tag")
}

#' @keywords internal
#' @noRd
fmt_display_label <- function(x, style = c("tag", "plain"), footer_collapse = TRUE) {
  style <- match.arg(style)
  d     <- get_display(x)
  # A COLUMN IS NAMED BY ITS DATA, not by the statistic rows it shares its column with. The chi2
  # p-value row, the base-count row `pct = "col"` appends and a model-fit footer all carry their own
  # token, and letting them vote turned an ordinary percentage column into "mixed". `row_kind` is the
  # declared fact (ROW_KINDS); the FULL ptype (footer_collapse = FALSE) still sees every row.
  ok <- rep(TRUE, length(d))
  if (footer_collapse) {
    rk <- vctrs::field(x, "row_kind")
    dr <- !is.na(rk) & rk %in% c("data", "total")
    if (any(dr)) ok <- dr
  }
  d <- unique(d[ok & !is.na(d)])
  if (!length(d)) return("")
  scl  <- fmt_scale_row(x)
  # ... nor by a template that renders NOTHING on any cell that carries it: a regression's Constant
  # row keeps the column's own token where a `display =` has no field to show there, and that empty
  # cell must not make the column "mixed".
  if (footer_collapse && length(d) > 1L) {
    disp  <- get_display(x)
    shown <- vapply(d, function(t) {
      sel <- ok & !is.na(disp) & disp == t
      any(sel) && any(!is.na(get_num(fmt_set_display(x, t))[sel]))
    }, logical(1))
    if (any(shown)) d <- d[shown]
  }
  prim <- fmt_resolve_scale_tokens(display_primary(d), scl)
  # ... nor by its BASELINE ROW. A column is named by what it ESTIMATES, and a regression's Constant
  # row prints the level those effects operate on (EST_SCALES$const_display) -- a ratio sits on a
  # percentage, a beta on a mean -- so on every scale where the two tokens differ the vote saw two
  # primaries and named the column "mixed". Only an EFFECT scale has a baseline row to ignore; a
  # level scale (every crosstab column) never reaches this.
  if (footer_collapse && length(unique(prim)) > 1L && identical(scl$kind, "effect")) {
    own <- prim == fmt_resolve_scale_tokens("est", scl)
    if (any(own)) { d <- d[own]; prim <- prim[own] }
  }
  if (length(unique(prim)) > 1L) return("mixed")
  # the FULLEST template showing that primary: a Total column carries "pct" on the rows the base
  # count could not be folded into and "{pct} ({n_range})" on the others, and the name is the aside's.
  tmpl <- d[[which.max(vapply(d, function(t) length(parse_display_template(t)$fields), integer(1)))]]
  out  <- display_template_label(tmpl, x, scl)
  if (identical(style, "plain") || !nzchar(out)) return(out)
  # THE PREFIX: only where the cell prints no LEVEL at all -- a column of bare deviations, where the
  # direction of reading is the only thing saying what they are deviations OF. A level names itself
  # ("row%", "mean", "n"), and a regression column never takes the prefix (`role` is the fact that
  # separates the two producers).
  if (!identical(get_role(x) %||% "", "")) return(out)
  geo <- DISPLAY_TOKEN_GEOMETRY[display_template_tokens(tmpl, scl)]
  if (any(!is.na(geo) & geo == "level")) return(out)
  paste0(fmt_kind_label(x), "-", out)
}

# `breaks` are AXIS POSITIONS (both sides + the neutral), NOT the positive magnitudes the colour engine
# folds; `break_dir` says which side each came from, so the label can pick its glyph.
#' @keywords internal
fmt_scale_of <- function(x, kind = "auto") {
  key <- fmt_scale_key(x, kind)
  s   <- EST_SCALES[[key]]
  s$key       <- key
  s$sd_y      <- NA_real_
  s$breaks    <- numeric(0)
  s$break_dir <- integer(0)
  s$break_mag <- numeric(0)
  if (!is.null(s$sd_from)) {
    v <- if (identical(s$sd_from, "var")) get_var(x) else get_ref_var(x)
    v <- v[is.finite(v) & v > 0]
    s$sd_y <- if (length(v)) sqrt(v[1]) else NA_real_
  }

  if (!is.na(s$break_key)) {
    # through color_scale_resolve(), so a DERIVED ladder (log_odds) reads its derivation from the one
    # declared place -- the axis and the colour engine cannot disagree about it.
    sc <- color_scale_resolve(s$break_key, color_scales())
    over  <- if (is.null(sc)) numeric(0) else sc$over$breaks
    under <- if (is.null(sc)) numeric(0) else sc$under$breaks
    over  <- over[ is.finite(over) ]
    under <- under[is.finite(under)]
    if (!is.null(s$sd_from)) {                        # the ladder is in SD units -> units of Y
      if (!is.finite(s$sd_y)) { over <- numeric(0); under <- numeric(0) }
      else { over <- over * s$sd_y; under <- under * s$sd_y }
    }
    pos <- if (isTRUE(s$mult)) c(rev(1 / under), s$neutral, over)
           else                c(rev(-under),    s$neutral, over)
    s$breaks    <- pos
    s$break_dir <- c(rep(-1L, length(under)), 0L, rep(1L, length(over)))
    # the MAGNITUDE each position stands for -- lets an axis and a footer print the same glyph.
    s$break_mag <- c(rev(under), 0, over)
  }
  if (!is.null(s$sd_from) && !is.finite(s$sd_y)) s$sec <- NULL
  s
}




# DESIGN: new_fmt() is the internal constructor. Attributes (scale, color, pct_type, ...) are SCALAR
#   per-column; fields (n, pct, diff, ...) are per-cell vectors. See vctrs::new_rcrd().
# No validation, no recycling -- fmt() does both and calls this. Not exported and not its own topic
# (roxygen would produce a page of undocumented formals); `?fmt` documents the whole type.
# ⚠ AND NEVER HERE: `fmt_ptype_empty <- new_fmt()` runs at SOURCE time, ~1900 lines before MEASURES
#   is bound, so a validating call moved into this constructor fails the install outright.
#' @noRd
new_fmt <- function(n         = integer(),
                    scale     = "level_n"    ,

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

                    # what KIND of row this cell sits in -- see ROW_KINDS (R/row-model.R). is_totrow()
                    # is the derived read `row_kind == "total"`.
                    row_kind  = NULL,
                    in_tottab = NULL,
                    in_refrow = NULL,

                    comp_all  = NA   ,
                    ref = ""   ,
                    pct_type  = "none",
                    col_var   = ""   ,
                    # WHICH SUB-POPULATION this column's block belongs to -- a level of a `spread_vars`
                    # variable, or a `tab_reg(tab_vars =)` group; "" when never spread. Stored apart
                    # from `col_var` so a two-line header composes only where wanted and `<br>` in a
                    # header means exactly one thing.
                    col_group = ""   ,
                    totcol    = FALSE,
                    refcol    = FALSE,
                    color     = ""   ,
                    color_signif = "ignore",
                    model_family = ""   ,   # regression model family per column ("" on crosstabs)
                    role      = ""   ,   # column role -- "model"/"emp" on reg columns, "" on crosstabs
                    # the confidence level THIS column's interval and thresholds were computed at. NA =
                    # unknown -> get_conf_level() falls back to options(tabxplor.conf_level). Per-COLUMN
                    # because colours are resolved per column at print time.
                    conf_level = NA_real_,
                    # the two facts that say HOW this column's interval was computed, beside the level
                    # it was computed AT. Per-COLUMN (like conf_level) so a table's inference survives a
                    # pipeline that drops table metadata.
                    #   degf  the design's degrees of freedom (#PSU - #strata); NA = refer to z
                    #   basis "n" | "weights" | "design_partial" | "design" (R/survey-design.R)
                    degf      = NA_real_,
                    basis     = "n"  ,
                    # WHICH interval engine built this column's bounds -- "wilson" / "wald" / "beta" /
                    # "newcombe" / "ac" / "welch" / "student" / "ols" / "katz" / "woolf" / "robust" /
                    # "quasipoisson" / "poisson" / "wald_log" / "profile"; "" = no interval. Per-COLUMN
                    # so the legend names the method the bounds were actually built with.
                    ci_method = ""   ,
                    ..., class = character()
) {
  # the unset fields share ONE `NA` vector instead of allocating one each (copy-on-write duplicates a
  # field only when it is touched); the `display` default is base-R for the same speed reason.
  # WARNING: NULL defaults, so a field passed as NULL is "unset", not an error. An empty column must
  # pass a 0-length vector with length(n) == 0. The fields stay ALWAYS present (a dense record).
  size <- length(n)
  nas  <- rep(NA_real_, size)
  fls  <- rep(FALSE   , size)
  if (is.null(display)) display <- est_default_display(scale[1])  # ONE declared rule
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
  if (is.null(row_kind )) row_kind  <- rep("data", size)
  if (is.null(in_tottab)) in_tottab <- fls
  if (is.null(in_refrow)) in_refrow <- fls

  display <- vctrs::vec_recycle(display, size = size)

  vctrs::new_rcrd(
    list(n = n, display = display, digits = digits,
         wn = wn, pct = pct, mean = mean,
         diff = diff, ratio = ratio, ctr = ctr, var = var,
         ci_inf = ci_inf, ci_sup = ci_sup, pvalue = pvalue, or = or,
         tot_n = tot_n, n_eff = n_eff, obs = obs, gap_se = gap_se,
         row_kind = row_kind, in_tottab = in_tottab,
         in_refrow = in_refrow),
    scale = scale, comp_all = comp_all, ref = ref,
    pct_type = pct_type, col_var = col_var, col_group = col_group[1],
    totcol = totcol, refcol = refcol,
    color = color, color_signif = color_signif[1], model_family = model_family[1],
    role = role[1], conf_level = conf_level[1],
    degf = degf[1], basis = basis[1], ci_method = ci_method[1],
    class = c(class, "tabxplor_fmt"))
}

# The 21 per-cell record FIELDS of new_fmt(), single-sourced so the column-attribute list below can be
# DERIVED rather than hand-maintained. Adding a FIELD updates this vector (the /vctrs-field checklist
# forces it); adding an ATTRIBUTE (a new_fmt() formal that is not a field) needs NO change here -- it
# appears in fmt_col_attrs automatically. Order follows the new_rcrd() list() above; do NOT reorder.
fmt_field_names <- c("n", "display", "digits", "wn", "pct", "mean", "diff", "ratio", "ctr", "var",
                     "ci_inf", "ci_sup", "pvalue", "or", "tot_n", "n_eff", "obs", "gap_se",
                     "row_kind", "in_tottab", "in_refrow")

# One gloss per FIELD, so `?fmt`'s roll-call and the programming vignette's table are one list, not
# two hand-kept copies. Exhaustive by a build-time assertion (the fmt_attr_rules precedent): adding a
# field without a gloss breaks the install.
#' @keywords internal
#' @noRd
FMT_FIELD_DOC <- c(
  n         = "the unweighted count",
  display   = "which field this cell shows (a bare name, or a `{}` template)",
  digits    = "how many decimals this cell prints",
  wn        = "the weighted count",
  pct       = "the percentage",
  mean      = "the mean, on a numeric column variable",
  diff      = "the difference from the reference cell (percentage points, or the outcome's own units)",
  ratio     = "the ratio to the reference cell (a relative risk, or a ratio of means)",
  ctr       = "the cell's contribution to the table's Chi-2",
  # the RULE, not the enumeration: `var` carries a variance of a mean, the Chi-2 variance of a
  # percentage, or var(Y) on a regression column, and `scale` says which -- so listing the cases here
  # would be a copy that drifts every time a scale is added.
  var       = "the column's variance quantity -- which one is given by its `scale`",
  ci_inf    = "the lower bound of the confidence interval",
  ci_sup    = "the upper bound of the confidence interval",
  pvalue    = "the cell's own significance p-value, which the stars read",
  or        = "the odds ratio against the `ref2` level",
  tot_n     = "the cell's own base --- the count its percentage is computed on",
  n_eff     = "the effective sample size its interval was computed on (weights or a survey design)",
  obs       = "`tab_reg()` only: the observed (crude) effect the modelled one is compared to",
  gap_se    = "`tab_reg()` only: the standard error of the gap between the estimate and `obs`",
  row_kind  = "what kind of row the cell sits in --- see [get_row_kind()]",
  in_tottab = "is the cell in a total table (logical)",
  in_refrow = "is the cell in a reference row (logical)"
)
stopifnot(setequal(names(FMT_FIELD_DOC), fmt_field_names))
FMT_FIELD_DOC <- FMT_FIELD_DOC[fmt_field_names]   # lock new_fmt()'s own order

# The `#' @eval` generator behind ?fmt's field roll-call. Escaping job: backticks -> \code{}, and
# `%`/`\` are Rd-special.
#' @keywords internal
#' @noRd
fmt_fields_rd <- function() {
  esc <- function(s) {
    s <- gsub("%", "\\\\%", gsub("\\", "\\\\", s, fixed = TRUE))
    s <- gsub("\\[([^]]+)\\]\\(\\)", "\\\\code{\\1}", s)
    gsub("`([^`]+)`", "\\\\code{\\1}", s)
  }
  c("@section The fields of a cell:",
    paste0("A \\code{fmt} cell carries ", length(fmt_field_names), " fields. Many are \\code{NA} ",
           "when the quantity was not requested; read one with \\code{x$field} or"),
    "\\code{\\link[vctrs:field]{vctrs::field()}}, and see them all with",
    "\\code{\\link[vctrs:vec_data]{vctrs::vec_data()}}:",
    "\\itemize{",
    vapply(fmt_field_names,
           function(f) paste0("  \\item \\code{", f, "} --- ", esc(unname(FMT_FIELD_DOC[[f]])), "."),
           character(1)),
    "}")
}

# The per-column ATTRIBUTE names carried when a fmt column is rebuilt: every new_fmt() formal that is
# NOT a per-cell field (nor `...`/`class`), in signature order. `color` is carried WHOLE (length 1 or 2).
fmt_col_attrs <- setdiff(names(formals(new_fmt)), c(fmt_field_names, "...", "class"))


# The values a `col_var` attribute takes that are NOT a variable name -- the build's placeholders:
#   "no_col_var" / "no_row_var"  the synthetic single-level factor tab() injects when an axis is
#                                absent; "all_col_vars" a column belonging to no col_var; "" / "no" /
#                                NA the empty spellings (incl. the base-count / add_pct helpers).
# `no_col_var` is NOT a real variable name -- rendering it as a header, or reporting it from
# tab_structure(), is noise.
#
# TWO predicates, deliberately distinct:
#   is_real_col_var(x)     of a STORED col_var: "does this name a column variable?"
#   is_placeholder_var(nm) of a build-time VARIABLE NAME (a symbol, before any column exists)
#' @keywords internal
#' @noRd
TAB_PLACEHOLDER_COL_VARS <- c("all_col_vars", "no_col_var", "no_row_var", "", "no", NA_character_)

#' @keywords internal
#' @noRd
is_real_col_var <- function(x) !is.na(x) & !x %in% TAB_PLACEHOLDER_COL_VARS

# ⚠ as.character(): the build passes VARIABLE NAMES as rlang symbols as often as as strings, and
# `sym == "x"` coerces while `sym %in% "x"` errors ("'match' requires vector arguments").
#' @keywords internal
#' @noRd
is_placeholder_var <- function(nm) as.character(nm) %in% c("no_row_var", "no_col_var")


# ==============================================================================================
# === THE DECLARED reconcile rules of the per-column attributes ================================
# ==============================================================================================
# The four reconstructor families (vec_ptype2 / vec_cast / vec_arith / vec_math, at the bottom of
# this file) are DRIVEN by this table, exactly as `meta_bind_rules` + tab_meta_bind()
# (R/tab_classes.R) drive the table-level `meta`.
#
#   ADDING AN ATTRIBUTE = add a formal to new_fmt() + ONE row here. Nothing else.
#
# The declared columns:
#   neutral  the value a MISMATCH collapses to (binding unlike columns is allowed but loses the
#            mismatched metadata). Also the value the "neutral" arith policy forces.
#   merge    how vec_ptype2 (= every c() / bind / group) reconciles two columns:
#              "same"        identical -> x's value, else `neutral`
#              "comp3"       like "same" but THREE-valued: comp_all is NA on count columns, and
#                            NA-vs-set must stay NA (a bare if() on `==` would ERROR)
#              "elementwise" `color` is length 1 OR 2 (text + background); each channel reconciles alone
#              "min"         the widest critical value wins -> the smallest positive finite `degf`
#              "weakest"     basis_weakest(): a merge claims only what its weakest part carried
#   arith    the per-context policy of vec_arith (fmt +-*/ fmt):
#              "merge"    reconcile, like ptype2
#              "neutral"  FORCED to the neutral (a sum of two columns is never a total column)
#              "x"        taken from x blindly -- a DISPLAY fact, not an inferential one
#   scalar   stored length 1 (new_fmt() `[1]`-subsets these); `color` is carried WHOLE.
#   write    THE writer -- the attribute's own setter, so validation is stated once and the generic
#            `fmt_attr<-()` cannot become a second, laxer way to write it. Adding an attribute without
#            a writer fails the build.
#
# vec_cast takes every attribute from `to`, vec_math (sum/mean) from `.x` -- neither needs a column here.
#
# WARNING: `arith` is NOT uniformly "merge". `totcol`/`refcol` are forced FALSE (arithmetic destroys
# the position that made a column a total/reference one); the display facts follow x (a mutate() keeps
# the colour); the inferential trio `conf_level`/`degf`/`basis` RECONCILE (weakest-claim), so
# `design_col + n_col` cannot claim "design".
#' @keywords internal
#' @noRd
fmt_attr_rules <- list(
  scale        = list(neutral = "mixed",        merge = "same",        arith = "merge",   scalar = TRUE , write = set_scale),
  comp_all     = list(neutral = FALSE,          merge = "comp3",       arith = "merge",   scalar = TRUE , write = set_comp_all),
  ref          = list(neutral = "",             merge = "same",        arith = "merge",   scalar = TRUE , write = set_ref_type),
  pct_type     = list(neutral = "none",         merge = "same",        arith = "merge",   scalar = TRUE , write = set_pct_type),
  col_var      = list(neutral = "several_vars", merge = "same",        arith = "merge",   scalar = TRUE , write = set_col_var),
  # like `col_var` -- binding two sub-populations loses the distinction; the neutral is "no
  # sub-population", because an unspread and a spread column differ by PRESENCE, not by name.
  col_group    = list(neutral = "",             merge = "same",        arith = "merge",   scalar = TRUE , write = set_col_group),
  totcol       = list(neutral = FALSE,          merge = "same",        arith = "neutral", scalar = TRUE , write = as_totcol),
  refcol       = list(neutral = FALSE,          merge = "same",        arith = "neutral", scalar = TRUE , write = as_refcol),
  color        = list(neutral = "",             merge = "elementwise", arith = "x",       scalar = FALSE, write = set_color),
  color_signif = list(neutral = "ignore",       merge = "same",        arith = "x",       scalar = TRUE , write = set_color_signif),
  model_family = list(neutral = "",             merge = "same",        arith = "x",       scalar = TRUE , write = set_model_family),
  role         = list(neutral = "",             merge = "same",        arith = "x",       scalar = TRUE , write = set_role),
  conf_level   = list(neutral = NA_real_,       merge = "same",        arith = "merge",   scalar = TRUE , write = set_conf_level),
  degf         = list(neutral = NA_real_,       merge = "min",         arith = "merge",   scalar = TRUE , write = set_degf),
  basis        = list(neutral = "n",            merge = "weakest",     arith = "merge",   scalar = TRUE , write = set_basis),
  # arithmetic destroys the interval (the bounds are reset to NA just below), so a sum must not keep
  # claiming Newcombe; binding unlike methods keeps no claim either.
  ci_method    = list(neutral = "",             merge = "same",        arith = "neutral", scalar = TRUE , write = set_ci_method)
)

# THE completeness assertion, at PACKAGE BUILD (the index vectors below are derived then): a missing
# row would make which() short and the loops SILENTLY SKIP an attribute. Adding a new_fmt() formal
# without a rule row breaks the install. Mirrored in test-fmt_class.R for cached binary installs.
stopifnot(setequal(names(fmt_attr_rules), fmt_col_attrs),
          # ...and every row names its writer, so `fmt_attr<-()` validates as the named setter does.
          all(vapply(fmt_attr_rules, function(r) is.function(r$write), logical(1))))
fmt_attr_rules <- fmt_attr_rules[fmt_col_attrs]      # lock new_fmt()'s own order

# Each reader's default IS new_fmt()'s own formal default -- DERIVED, not declared, so they cannot
# drift. (All attribute formals are atomic length-1 constants, so eval() is total.)
#' @keywords internal
#' @noRd
fmt_attr_default <- lapply(formals(new_fmt)[fmt_col_attrs], eval, envir = baseenv())

# Parallel POSITION vectors, computed once at build time: the run-time loops dispatch on an integer
# index, never a rule string, never allocate a closure -- what keeps the rule-driven reconcile fast.
fmt_attr_n        <- length(fmt_col_attrs)
fmt_attr_neutral  <- unname(lapply(fmt_attr_rules, `[[`, "neutral"))
fmt_attr_i_same   <- unname(which(vapply(fmt_attr_rules, function(r) r$merge, "") == "same"       ))
fmt_attr_i_comp3  <- unname(which(vapply(fmt_attr_rules, function(r) r$merge, "") == "comp3"      ))
fmt_attr_i_elt    <- unname(which(vapply(fmt_attr_rules, function(r) r$merge, "") == "elementwise"))
fmt_attr_i_min    <- unname(which(vapply(fmt_attr_rules, function(r) r$merge, "") == "min"        ))
fmt_attr_i_weak   <- unname(which(vapply(fmt_attr_rules, function(r) r$merge, "") == "weakest"    ))
fmt_attr_i_ar_neu <- unname(which(vapply(fmt_attr_rules, function(r) r$arith, "") == "neutral"    ))
fmt_attr_i_ar_x   <- unname(which(vapply(fmt_attr_rules, function(r) r$arith, "") == "x"          ))
fmt_attr_i_scalar <- unname(which(vapply(fmt_attr_rules, function(r) r$scalar, TRUE)))
fmt_attr_i_basis  <- match("basis", fmt_col_attrs)

# THE reader: a fmt column's attributes, in new_fmt()'s order and storage shape -- ONE attributes()
# call instead of 14 getters. Handed straight to new_fmt()/fmt(), matched by EXACT name, so the
# `comp` -> `comp_all` partial-match hazard cannot fire. `basis` folds NA/"" to "n".
#' @keywords internal
#' @noRd
fmt_attrs_of <- function(x) {
  a <- attributes(x)[fmt_col_attrs]
  names(a) <- fmt_col_attrs                       # an absent attribute comes back named NA -- reset
  for (i in seq_len(fmt_attr_n))  if (is.null(a[[i]]))      a[[i]] <- fmt_attr_default[[i]]
  for (i in fmt_attr_i_scalar)    if (length(a[[i]]) != 1L) a[[i]] <- a[[i]][1L]
  b <- a[[fmt_attr_i_basis]]
  if (is.na(b) || !nzchar(b)) a[[fmt_attr_i_basis]] <- "n"     # == get_basis()
  a
}

# === THE GENERIC ATTRIBUTE ACCESSOR ==========================================
# `fmt_col_attrs` declares the 16 attributes exhaustively, by build-time assertion -- and the
# EXPORTED accessors were ~23 hand-written functions beside it, neither exhaustive (four attributes
# had none) nor consistent (the `ref` pair did not share a stem). That is the last hand-written
# mirror of the attribute table, and it grew with every phase that stored a fact.
#
# THE ADMISSION TEST, stated here beside the one that governs a new attribute: *storing a fact is
# internal; exporting its accessor is a user contract -- name the user story first.* Since 20a the
# answer to "a 17th attribute needs an accessor" is NO: fmt_attr() reaches it the day it exists.
#
# TWO SURFACES, deliberately, and the distinction is what keeps this from being duplication:
#   the NAMED accessors  are the TAUGHT surface -- a user writes get_scale(x), and the vignettes do.
#   fmt_attr()           is the PROGRAMMATIC one -- a helper loops over fmt_col_attrs, and until now
#                        had to write a switch over 16 function names to do it.
#
# ⚠ THE HOT PATH STAYS HAND-WRITTEN (the DISPLAY_TOKENS / fmt_attr_rules precedent). get_col_var()
# (33 call sites in R/), is_totrow() (44) and get_scale() are untouched: they are `attr()` reads on
# O(columns) loops, and routing them through a table lookup would buy nothing and cost a dispatch.
#
# ⚠ RAW, not resolved. fmt_attr() returns the STORED value with the declared `neutral` default. Three
# named getters do more than read -- get_conf_level() falls back to the option, get_degf() maps NA to
# Inf, get_basis() folds "" to "n" -- and those are RESOLVERS, which is why they stay internal: the
# question "what does this column claim" and "what will the engine use" are two questions.

#' Read or write one `fmt` column attribute, by name
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The generic form of the `get_*()` / `set_*()` family: one function covering every per-column
#' attribute a `tabxplor_fmt` vector carries, so a helper can loop over them instead of naming each.
#' The named accessors ([get_scale()], [get_col_var()], [is_totcol()], …) remain the readable way to
#' address one known attribute.
#'
#' @param x A `tabxplor_fmt` vector, or a data.frame (then every `fmt` column is read).
#' @param name The attribute: one of `"scale"`, `"comp_all"`, `"ref"`, `"pct_type"`, `"col_var"`,
#'   `"col_group"`, `"totcol"`, `"refcol"`, `"color"`, `"color_signif"`, `"model_family"`, `"role"`,
#'   `"conf_level"`, `"degf"`, `"basis"`, `"ci_method"`. An unknown name is an error naming the set.
#' @param value The new value. Written through the attribute's own setter, so it is validated
#'   exactly as `set_scale()` and friends validate it.
#'
#' @return The stored value (its declared default when the attribute is unset). On a data.frame, one
#'   entry per `fmt` column, named. Writing returns the modified vector.
#' @seealso [tab_columns()] for every column's attributes at once; [fmt()] for what each one means.
#' @export
#'
#' @examples
#' x <- fmt(n = c(10, 20), pct = c(0.3, 0.7), scale = "level_pct", pct_type = "row")
#' fmt_attr(x, "scale")
#' fmt_attr(x, "col_var") <- "region"
#' fmt_attr(x, "col_var")
fmt_attr <- function(x, name) {
  name <- fmt_attr_check_name(name)
  if (is.data.frame(x)) {
    keep <- vapply(x, is_fmt, logical(1))
    out  <- lapply(x[keep], function(col) fmt_attr(col, name))
    # `color` is the one non-scalar attribute (text + background channels), so it stays a list;
    # everything else simplifies to the named vector every caller expects.
    if (!isTRUE(fmt_attr_rules[[name]]$scalar)) return(out)
    return(unlist(out, use.names = TRUE))
  }
  v <- attr(x, name, exact = TRUE)
  if (is.null(v)) fmt_attr_rules[[name]]$neutral else v
}

#' @rdname fmt_attr
#' @export
`fmt_attr<-` <- function(x, name, value) {
  name <- fmt_attr_check_name(name)
  if (is.data.frame(x))
    cli::cli_abort(c("{.fn fmt_attr<-} writes one {.cls tabxplor_fmt} column at a time.",
                     "i" = "Use {.code dplyr::mutate(x, dplyr::across(where(is_fmt), ~ ...))}."))
  fmt_attr_rules[[name]]$write(x, value)
}

#' @keywords internal
#' @noRd
fmt_attr_check_name <- function(name) {
  name <- as.character(name)[1]
  if (is.na(name) || !name %in% fmt_col_attrs)
    cli::cli_abort(c("Unknown {.cls tabxplor_fmt} column attribute {.val {name}}.",
                     "i" = "Valid: {.val {fmt_col_attrs}}."), call = NULL)
  name
}


#' @keywords internal
#' @noRd
fmt_attrs_merge <- function(ax, ay) {
  for (i in fmt_attr_i_same)
    if (!identical(ax[[i]], ay[[i]])) ax[[i]] <- fmt_attr_neutral[[i]]
  for (i in fmt_attr_i_comp3) {                   # 3-valued: NA-vs-set stays NA, and never ERRORS
    cx <- ax[[i]]; cy <- ay[[i]]
    s  <- cx == cy | (is.na(cx) & is.na(cy))
    ax[[i]] <- if (is.na(s)) NA else if (s) cx else fmt_attr_neutral[[i]]
  }
  for (i in fmt_attr_i_elt) {                     # `color`: per CHANNEL (length 1 or 2)
    vx <- ax[[i]]; s <- vx == ay[[i]]
    ax[[i]] <- if (length(s) == 1L) { if (s) vx else fmt_attr_neutral[[i]] }
               else ifelse(s, vx, fmt_attr_neutral[[i]])
  }
  for (i in fmt_attr_i_min) {                     # `degf`: the widest critical value wins
    d <- c(ax[[i]], ay[[i]]); d <- d[is.finite(d) & d > 0]
    ax[[i]] <- if (length(d)) min(d) else fmt_attr_neutral[[i]]
  }
  for (i in fmt_attr_i_weak) ax[[i]] <- basis_weakest(ax[[i]], ay[[i]])
  ax
}

#' @keywords internal
#' @noRd
fmt_attrs_arith <- function(ax, ay) {
  out <- fmt_attrs_merge(ax, ay)
  for (i in fmt_attr_i_ar_neu) out[[i]] <- fmt_attr_neutral[[i]]
  for (i in fmt_attr_i_ar_x)   out[[i]] <- ax[[i]]
  out
}

# fmt_ptype_empty is a ZERO-LENGTH prototype spliced into every c(). `attributes(out)[...] <- a` copies
# first, so the shared prototype is never mutated.
#' @keywords internal
#' @noRd
fmt_ptype_empty <- new_fmt()

#' @keywords internal
#' @noRd
fmt_ptype_attrs <- function(a) {
  out <- fmt_ptype_empty
  attributes(out)[fmt_col_attrs] <- a
  out
}





#' @keywords internal
fmt0 <- function(display = "n", digits = 0, scale = "level_n") {
  new_fmt(n = 0L, display = display, digits = as.integer(digits), scale = scale)
}




# Internal functions to get fields and attributes of class fmt

#' @keywords internal
fmt_field_factory <- function(.field) {
  function(x) vctrs::field(x, .field)
}

#' get the "display" field of a \code{fmt} vector
#' @param x The formatted number in which you want to find data for "get" functions,
#' to modify data for "set" functions.
#' @keywords internal
get_display <- fmt_field_factory("display")
#' @keywords internal
get_n      <- fmt_field_factory("n")
#' @keywords internal
get_wn     <- function(x) { #If there is no weighted counts, take counts
  out <- vctrs::field(x, "wn")
  if (any(is.na(out))) {
    counts <- vctrs::field(x, "n") |> as.double()
    out[is.na(out)] <- counts[is.na(out)]
  }
  out
}

# THE `wn` MATERIALISATION, in the two shapes that need it.
#
# THE RULE: `get_wn()` is the ONLY getter with a fallback -- it answers the `n` field wherever `wn` is
# NA (an unweighted table stores no weighted count). So reading through it and writing back is NOT a
# no-op: it FIXES the fallback into the record. Everything that combines fmt vectors does that
# implicitly (vec_ptype2 / vec_cast go through the getters); the two callers below do it explicitly.
#
# fmt_data_wn(col)        -- the FRAME shape (as.list(vec_data()) with wn fixed): tab_stack_tables()
#                            (tab_classes.R), the test-display column stacker (tab-test-display.R).
# fmt_materialize_wn(col) -- the COLUMN shape: chi2_write_contrib() (R/tab-chi2.R), tab_ci()
#                            (R/tab-steps-legacy.R). The goldens pin the materialised value.
#' @keywords internal
fmt_data_wn <- function(col) {
  fr <- as.list(vctrs::vec_data(col))
  fr$wn <- get_wn(col)
  fr
}
#' @keywords internal
fmt_materialize_wn <- function(col) set_wn(col, get_wn(col))
#' @keywords internal
get_pct    <- fmt_field_factory("pct")
#' @keywords internal
get_diff   <- fmt_field_factory("diff")
#' @describeIn fmt get the "digits" field
#' @export
get_digits <- fmt_field_factory("digits")
#' @keywords internal
get_ctr    <- fmt_field_factory("ctr")
#' @keywords internal
get_mean   <- fmt_field_factory("mean")
#' @keywords internal
get_ratio  <- fmt_field_factory("ratio")
#' @keywords internal
get_var    <- fmt_field_factory("var")
#' @keywords internal
get_ci_inf <- fmt_field_factory("ci_inf")
#' @keywords internal
get_ci_sup <- fmt_field_factory("ci_sup")
# ci_inf/ci_sup are real asymmetric ABSOLUTE bounds; get_ci()/get_ci_moe() read the half-width / the
# larger arm back off the centre (fmt_center_field(), a lookup on the STORED scale).
#' @keywords internal
ci_center  <- function(x) as.double(vctrs::field(x, fmt_center_field(x)))
#' @keywords internal
get_ci     <- function(x) get_ci_sup(x) - ci_center(x)
#' @keywords internal
get_ci_moe <- function(x) {
  ctr <- ci_center(x)
  pmax(ctr - get_ci_inf(x), get_ci_sup(x) - ctr)
}
#' @keywords internal
stars_from_pvalue <- function(p) {
  ladder <- tx_stars_ladder()          # ONE option carries glyphs AND cut-offs
  brk <- unname(ladder)                                                # descending p
  lab <- names(ladder)
  out <- c("", lab)[rowSums(outer(p, brk, `<`), na.rm = TRUE) + 1L]
  out[is.na(p)] <- ""
  out
}

# per-cell stars from the stored `pvalue` (CI-inclusion, so they always agree with the bracket).
#' @keywords internal
get_stars  <- function(x, p = get_pvalue(x)) {
  out <- stars_from_pvalue(p)
  # a footer cell (a "gof" stat, a "pvalue" test row, a "blank" filler) carries a real `pvalue`, but
  # it is NOT a "different from the reference" comparison, so it must never print a star. Gating here
  # makes every get_stars() caller agree at the source.
  out[display_primary(get_display(x)) %in% DISPLAY_FOOTER_TOKENS] <- ""
  out
}
# THE cell suffix: a publication palette's effect-size MARKS, or the significance STARS -- one or the
# other, never both. They sit in the same place after the value and say different things (how big the
# deviation is, against the breaks; how sure it is, against a p-value threshold), so a palette that
# marks its cells suppresses the stars rather than crowding them into two contradictory symbol runs.
# The mark is the SLOT's rendering, exactly like the ink and the face, so nothing here re-derives a
# direction: the slot already carries the side and the magnitude, and a greyed cell has slot 0.
#' @keywords internal
fmt_cell_suffix <- function(x, stars = FALSE, theme = NULL) {
  # `theme = NULL` means "no palette" -- the tooltip and character-cast re-renders, which must stay
  # theme-blind. Reading the option here would annotate a cell nobody asked to annotate.
  marks <- if (is.null(theme)) NULL else get_color_style("face", type = "text", theme = theme)$marks
  if (!is.null(marks) && any(nzchar(marks))) {
    out <- c("", marks)[fmt_color_channels(x)$text_slot + 1L]
    out[display_primary(get_display(x)) %in% DISPLAY_FOOTER_TOKENS] <- ""
    return(out)
  }
  if (isTRUE(stars) && fmt_stars_applicable(x)) get_stars(x) else rep("", length(x))
}

# whether a column's stored pvalue drives significance STARS. A `contrib` column stores a
# standardized-residual pvalue only to gate its OWN colouring -- not a "different from the reference"
# test -- so it must not print stars. That IS `sig_source == "pvalue"`: it names the rule, not the row.
#' @keywords internal
fmt_stars_applicable <- function(x) {
  k <- measure_key(get_color(x))
  is.na(k) || !nzchar(k) || !identical(MEASURES[[k]]$sig_source, "pvalue")
}
#' @describeIn fmt get the per-cell p-value (what the significance stars read)
#' @export
get_pvalue <- fmt_field_factory("pvalue")
#' @keywords internal
get_or     <- fmt_field_factory("or")
#' @keywords internal
get_tot_n  <- fmt_field_factory("tot_n")

#' @keywords internal
get_n_eff  <- fmt_field_factory("n_eff")

#' @keywords internal
get_obs    <- fmt_field_factory("obs")

#' @keywords internal
get_gap_se <- fmt_field_factory("gap_se")

# get_tot_wn(): the cell's OWN WEIGHTED percentage base. NOT a stored field -- recovered as wn / pct
# (pct is stored at full precision), mirroring get_ci(). For an empty cell (pct == 0) the ratio is
# undefined, so fall back to a same-column total (100%) cell's weighted count when present (covers
# col% and grand-total modes); a row%-empty cell whose base lives in another column returns NA.
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

# grand_totrow() -- the grand-total row mask under comp = "all". Degrades to the plain total row when
# there is no total-table axis (no tab_vars), so a single table is its own total table -- comp = "all"
# stays usable there (byte-identical to comp = "tab"). Shared by get_mean_contrib/chi2_write_contrib.
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
# fields that already store it: the two-sided p-value (magnitude) and the signed contribution
# (direction) -- not a 20th field, so the colour gate and displayed residual cannot disagree.
# WARNING: it MUST be -qnorm(p/2), never qnorm(1 - p/2): `1 - p/2` is exactly 1 in double precision
# for any p < 2.2e-16 (every |z| > 8.2, routine in survey tables), which saturates the tail to Inf.
# NA where `ctr` is NA (no chi2 contribution), so a stray `display = "{resid}"` blanks rather than lies.
#' @keywords internal
fmt_resid <- function(x) {
  sign(get_ctr(x)) * -stats::qnorm(get_pvalue(x) / 2)
}

# fmt_gap_parts() -- the ONE decomposition of the estimate-vs-`obs` comparison, shared by the score,
# its interval and its p-value, so those three can never describe different quantities. Returns:
#   mult  the estimate is multiplicative (or / ratio) -> neutral 1, else additive -> neutral 0;
#   est / obs / ok  the two values and where both are usable;
#   sign  the NULL DIRECTION: +1 when the estimate is FURTHER from the null than `obs` (strengthened),
#         -1 when nearer (attenuated) OR when the two sit on OPPOSITE SIDES of the null, 0 when equal.
#
# DESIGN -- why a REVERSAL is signed as attenuation. With two poles a reversal has to be one of them,
# and it belongs on the attenuated side: whatever the observed effect claimed, the model says it is
# not that -- the crude reading did not survive, which is exactly what that pole means. Its magnitude
# stays the full move, so a big flip reads as a big move; and the reader sees the reversal itself in
# the pair of cells, whose multiplicative glyphs face opposite ways. This also removes the one case
# `sign` could come out 0 with a large magnitude (a perfect mirror, x2 -> /2), which fell into the
# "strengthened" arm and painted the deepest blue.

#' @keywords internal
fmt_est_of <- function(x) as.double(vctrs::field(x, fmt_center_field(x)))

#' @keywords internal
fmt_gap_parts <- function(x) {
  mult <- isTRUE(fmt_scale_row(x)$mult)
  obs  <- get_obs(x)
  est  <- fmt_est_of(x)
  if (mult) {
    ok <- is.finite(est) & is.finite(obs) & est > 0 & obs > 0
    s  <- sign(abs(log(ifelse(ok, est, NA_real_))) - abs(log(ifelse(ok, obs, NA_real_))))
    flip <- ok & (est - 1) * (obs - 1) < 0
  } else {
    ok <- is.finite(est) & is.finite(obs)
    s  <- sign(abs(est) - abs(obs))
    flip <- ok & est * obs < 0
  }
  s[flip] <- -1
  list(mult = mult, est = est, obs = obs, ok = ok & !fmt_gap_degenerate(x, mult, est, obs), sign = s)
}

# ⚠ A MODEL THAT IS ITS OWN CRUDE TWIN HAS NO GAP. With one predictor -- or any predictor set the
# crude fit already contains -- the two estimators coincide exactly, so both the gap and its standard
# error are floating-point dust; dividing one by the other yields z = -20 and "p < 0.01 %", and
# `color = "adjustment"` then paints the column at full strength on nothing at all.
# The tolerance is RELATIVE TO THE ESTIMATE, never absolute: a gap of 1e-4 is dust beside an odds
# ratio of 3 and a real finding beside a coefficient of 1e-3. Both halves must be dust -- a genuinely
# tiny gap with an honest SE is a real "adjustment changed nothing" and keeps its interval.
#' @keywords internal
fmt_gap_degenerate <- function(x, mult, est, obs) {
  se  <- get_gap_se(x)
  # on the TEST scale, which is the log for a multiplicative column -- that is what `gap_se` is the
  # standard error of, so the gap and the scale it is judged against must be read there too.
  a   <- if (mult) suppressWarnings(log(est)) else est
  b   <- if (mult) suppressWarnings(log(obs)) else obs
  tol <- .Machine$double.eps^0.5 * pmax(abs(a), abs(b), 1, na.rm = TRUE)
  !is.na(a) & !is.na(b) & abs(a - b) <= tol & !is.na(se) & se <= tol
}

# fmt_adjustment_score() -- how far a model estimate sits from `obs`. ONE helper behind both
# `color = "adjustment"` and `color = "between_groups"`; they differ only in what `obs` holds. The
# comparison rides the estimate's own scale: multiplicative (or/ratio) folds around 1, additive (diff)
# around 0.
# DESIGN -- the SIGN is "away from vs toward the NULL", not raw up/down. Scoring |log est| - |log obs|
# (|est| - |obs| when additive) makes one pole always "the model STRENGTHENED this effect" and the
# other "it ATTENUATED it", for protective and risky effects alike, correct through the null. The
# magnitude fed to findInterval is direction-free; only the sign carries the reading.
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

# fmt_gap_raw() -- the SIGNED gap on the estimate's own TEST scale (the log-ratio when multiplicative,
# the plain difference when additive). This -- not the score -- is what `gap_se` is the standard
# error OF, so the two must be read from the same decomposition.
#' @keywords internal
fmt_gap_raw <- function(x) {
  p <- fmt_gap_parts(x)
  if (p$mult) ifelse(p$ok, log(p$est) - log(p$obs), NA_real_)
  else        ifelse(p$ok, p$est - p$obs           , NA_real_)
}

# fmt_gap_bounds() -- the confidence interval OF THE SCORE, so every branch of fmt_color_plan() works
# on it unchanged (it is the `bounds` fact of the two gap measures; every other measure keeps the
# stored ci_inf/ci_sup).
#
# DESIGN -- why the interval is re-signed rather than passed through raw. The score's sign is the NULL
# DIRECTION (away from / toward the null) while a raw gap interval is signed up/down; the two disagree
# for a protective effect. Folding the interval of |gap| back with the score's own sign makes both
# policies correct with no measure-specific branch:
#   * a gap interval excluding 0 puts BOTH bounds on the score's side -> significant, same direction;
#   * one covering 0 pins the near bound exactly at the neutral       -> not significant;
#   * the bound nearest the neutral IS the guaranteed gap ("moved by at least x1.1"), already signed.
# The level comes from the COLUMN (get_conf_level), not the option: the whole interval is manufactured
# at print time, so nothing level-wide was stored to fall back on.
#' @keywords internal
fmt_gap_bounds <- function(x) {
  p    <- fmt_gap_parts(x)
  se   <- get_gap_se(x)
  g    <- if (p$mult) ifelse(p$ok, log(p$est) - log(p$obs), NA_real_)
          else        ifelse(p$ok, p$est - p$obs           , NA_real_)
  ok   <- is.finite(g) & is.finite(se) & se > 0 & !is.na(p$sign)
  half <- zscore_formula(get_conf_level(x)) * se
  # ⚠ `+ 0` kills the IEEE NEGATIVE ZERO a pinned near bound carries out of the sign flip:
  # sprintf("%+.1f", -0) renders "-0.0", which reads as "just excludes the null" when it IS the null.
  # exp(-0) is 1, so only the additive branch ever showed it.
  lo   <- ifelse(ok, p$sign * pmax(0, abs(g) - half) + 0, NA_real_)   # magnitude interval of |gap|,
  hi   <- ifelse(ok, p$sign * (abs(g) + half)           , NA_real_)   #   re-signed by the null direction
  if (p$mult) { lo <- exp(lo); hi <- exp(hi) }                    # centre 1 (exp is monotone)
  list(lo = pmin(lo, hi), hi = pmax(lo, hi))
}

# fmt_gap_text() -- the gap and its interval as text, through format() and nothing else. THE gap is
# already a display token (`{gap}` carries the adjustment score), so the estimate needs no renderer of
# its own; the BOUNDS are manufactured rather than stored, so they are written into `ci_inf`/`ci_sup`
# on a throwaway copy and rendered as the `{ci}` token every other interval in the package uses.
# WARNING: do NOT re-introduce a second renderer here. The one this replaced hard-coded "%+.1f" and a
# " pts" suffix nothing else in the package writes, so a hovered gap and a printed one disagreed
# about both their precision and their unit.
#' @keywords internal
fmt_gap_text <- function(x) {
  bd  <- fmt_gap_bounds(x)
  est <- format(fmt_set_display(x, "gap"), stars = FALSE, na = "")
  ci  <- format(fmt_set_display(set_ci_sup(set_ci_inf(x, bd$lo), bd$hi), "ci"),
                stars = FALSE, na = "")
  list(est = trimws(est), ci = trimws(ci))
}

# fmt_gap_p() -- the two-sided p of the gap (z on the test scale). Display only: the colour reads the
# interval above, so the two agree by construction. NA wherever `gap_se` is.
#' @keywords internal
fmt_gap_p <- function(x) {
  g  <- fmt_gap_raw(x)
  se <- get_gap_se(x)
  ifelse(is.finite(g) & is.finite(se) & se > 0, 2 * stats::pnorm(-abs(g / se)), NA_real_)
}

# fmt_gap_force_policy() -- the `force_policy` of BOTH gap measures (see MEASURES), as a PREDICATE ON
# THE COLUMN: a gap measure has a test exactly where tab_reg could write a `gap_se`, so "is there a
# test in this column?" is ONE read of the field the test produced -- no estimand guessed from a label.
#
# It covers, in one rule, every case where the gap has no interval: `adjustment` on a CONDITIONAL ODDS
# RATIO (non-collapsible), a distilled fitted object (jamovi's digest path), a model-comparison column
# fitted on different rows, any engine with no crude twin; plus `between_groups` under
# `method = "profile"`, whose asymmetric bounds yield no SE.
#
# Byte-identical wherever a `gap_se` exists: NULL leaves the column's own `color_signif` in place.
# It also UPGRADES `ignore` wherever a test does exist: there is no meaningful "colour every movement
# without testing it" for a comparison of two estimates, and `ignore` (the package default) would
# make a gap fill a description rather than the test the design says it is. ⚠ ONLY `ignore` is
# upgraded -- `guaranteed_effect` is the stricter policy and a user who asked for it keeps it.
#' @keywords internal
fmt_gap_force_policy <- function(x) {
  if (all(is.na(get_gap_se(x)))) return("ignore")
  if (identical(get_color_signif(x), "ignore")) "grey_non_signif" else NULL
}

# fmt_broadcast_last() -- broadcast the LAST value of each group to every row, where each TRUE in
# `boundary` closes a group. Groups are contiguous, so a group's max row index is its last row.
#' @keywords internal
fmt_broadcast_last <- function(values, boundary) {
  gr <- cumsum(boundary) - boundary
  values[stats::ave(seq_along(values), gr, FUN = max)]
}

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

# fmt_base() -- the base a cell's CI is computed on: the EFFECTIVE sample size (`n_eff`) when populated,
# else the raw base (`tot_n` for a proportion, `n` for a mean). n_eff < n widens the interval, n_eff > n narrows.
#' @keywords internal
#' @noRd
fmt_base <- function(x, mean = FALSE) {
  dplyr::coalesce(get_n_eff(x), if (mean) as.double(get_n(x)) else get_tot_n(x))
}

#' @keywords internal
get_ref_var <- function(x) get_ref_field(x, get_var)

#' @keywords internal
detect_totcols <- function(tabs) {
  # Total columns are identified by the `totcol` attribute (is_totcol) / the "no_col_var" col_var --
  # robust, not by position. Each column maps to the first such total column at or after its position.
  # ⚠ this "no_col_var" is NOT the placeholder question (is_real_col_var): a table with no col_var has
  # a single value column that BEHAVES as its own total.
  tot <- which(is_totcol(tabs) | get_col_var(tabs) %in% "no_col_var")

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
#' @keywords internal
set_n       <- fmt_set_field_factory("n"      , cast = integer()  )
#' @keywords internal
set_wn      <- fmt_set_field_factory("wn"     , cast = double()   )
#' @keywords internal
set_pct     <- fmt_set_field_factory("pct"    , cast = double()   )
#' @keywords internal
set_diff    <- fmt_set_field_factory("diff"   , cast = double()   )
#' @keywords internal
set_ratio   <- fmt_set_field_factory("ratio"  , cast = double()   )
#' @describeIn fmt set the "digits" field
#' @export
set_digits  <- fmt_set_field_factory("digits" , cast = integer()  )
set_ctr     <- fmt_set_field_factory("ctr"    , cast = double()   )
#' @keywords internal
set_mean    <- fmt_set_field_factory("mean"   , cast = double()   )
#' @keywords internal
set_var     <- fmt_set_field_factory("var"    , cast = double()   )
#' @keywords internal
set_ci_inf  <- fmt_set_field_factory("ci_inf" , cast = double()   )
#' @keywords internal
set_ci_sup  <- fmt_set_field_factory("ci_sup" , cast = double()   )
# set_ci() (legacy): takes a symmetric half-width and stores it as ABSOLUTE bounds around the estimate
# the interval is centred on (ci_center()), so get_ci() reads it back. The build writers store real
# asymmetric bounds directly and do NOT use this; kept for back-compatible external callers.
#' @keywords internal
set_ci      <- function(x, value) {
  value <- vctrs::vec_cast(value, double()) |> vctrs::vec_recycle(size = length(x))
  ctr   <- dplyr::coalesce(ci_center(x), 0)
  x <- set_ci_sup(x, ctr + value)
  x <- set_ci_inf(x, ctr - value)
  x
}
#' @describeIn fmt set the per-cell p-value. `set_pvalue(x, NA_real_)` is how a duplicated,
#' purely descriptive copy of a column loses its stars: the stored p-value is their only source.
#' @export
set_pvalue  <- fmt_set_field_factory("pvalue" , cast = double()   )
#' @keywords internal
set_or      <- fmt_set_field_factory("or"     , cast = double()   )
# `n_eff` is write-once (the leaves' new_fmt() call) and read-only after, so it has no setter.
# `tot_n` has exactly ONE writer after the build: mat_base_n(), which puts the LARGEST base of the
# table in the presentation cell so `{n_range}` can print "min-max". Do not widen that.
#' @keywords internal
set_tot_n   <- fmt_set_field_factory("tot_n"  , cast = double()   )
#' @keywords internal
set_obs     <- fmt_set_field_factory("obs"    , cast = double()   )
#' @keywords internal
set_gap_se  <- fmt_set_field_factory("gap_se" , cast = double()   )







# METHODS FOR CLASS tabxplor_fmt #########################################################

#' @keywords internal
print_num <- function(num, digits) {
  # A value rounding to zero prints "0", from either side and at any digit count: "-0" says a
  # direction the rounding has just erased. At digits = 0 sprintf yields the bare "-0", which is why
  # the decimals are optional here.
  sprintf(paste0("%-0.", digits, "f"), num) |>
    stringi::stri_replace_first_regex("^-?0(\\.0+)?$", "0") |>
    stringi::stri_replace_first_regex("^100(\\.0+)?$", "100")
}

# THE one test of "this cell rendered something", shared by every annotation paste, by the stars mask
# and by the composite expander's empty-token rule -- so the three can never disagree about what is
# void. WARNING: it must trim the UNICODE whitespace class, not trimws(): the html/Excel pad glyph is
# a FIGURE SPACE (U+2007), and a padded-blank cell would otherwise read as content.
#' @keywords internal
fmt_rendered <- function(s) !is.na(s) & nzchar(stringi::stri_trim_both(s, "\\P{Wspace}"))

# THE interval a `{ci}` token prints -- ONE renderer, on the COLUMN'S OWN SCALE, for every scale.
# Everything it keys on is DECLARED in EST_SCALES: `is_pct` gives the x100 and the "%", `mult` the
# measure's inverse glyph per BOUND (the bounds are NOT reordered -- the glyph carries the direction,
# so "1/3.13" still sits left of "1/2.27" exactly as 0.32 sits left of 0.44), fmt_ci_digits() the
# decimals floor.
# A void bound yields NA: an interval that was never computed is BLANK, never the point estimate
# wearing brackets. `clamp` holds a PROPORTION's own interval inside 0-100.
# `{coef}` -- the estimate on the model's own LINK scale. On a multiplicative column that is
# log(estimate) (an odds ratio's coefficient IS its logarithm); on an additive one the estimate is
# already the coefficient. Derived, never stored: nothing would be gained by a 22nd field.
#' @keywords internal
fmt_coef_of <- function(x) {
  if (!isTRUE(fmt_scale_row(x)$mult)) return(get_diff(x))
  v <- fmt_est_of(x)
  ifelse(!is.na(v) & v > 0, suppressWarnings(log(v)), NA_real_)
}

#' @keywords internal
fmt_ci_bracket <- function(lo, hi, digits, is_pct = FALSE, clamp = FALSE, mult_under = NULL) {
  if (is_pct) { lo <- lo * 100; hi <- hi * 100 }
  if (clamp)  { lo <- pmax(lo, 0); hi <- pmin(hi, 100) }
  bnd <- function(b) {
    s <- sprintf(paste0("%-0.", digits, "f"), b)
    if (is.null(mult_under)) return(s)
    i <- !is.na(b) & b > 0 & b < 1
    s[i] <- paste0(mult_under, sprintf(paste0("%-0.", digits[i], "f"), 1 / b[i]))
    s
  }
  out <- paste0("[", bnd(lo), ";", bnd(hi), "]", if (is_pct) "%" else "")
  out[is.na(lo) | is.na(hi)] <- NA_character_
  out
}

# Format/printing methods for class tabxplor_fmt -----------------------------------------

# Excel numFmt embeds literal text either as "text" or by backslash-escaping each char (\t\e\x\t).
# WARNING: the quote form crashes the older openxlsx2 bundled by jamovi (unescaped " in the
# formatCode attribute). Every literal folded into a numFmt code MUST go through this backslash form.
xl_numfmt_literal <- function(s) gsub("(.)", "\\\\\\1", s, perl = TRUE)

# Excel number-format code per cell -- format()'s ONE display source of truth, so tab_xl() cannot
# desync: it writes the raw get_num() value and hands display to Excel, fed format()'s OWN masks.
#   digits format()'s adjusted digits; pct the x100 "%" mask; ci a standalone "ci" (prepend +/-);
#   text  a rendered string with no number format of its own (a `{ci}` bracket, an n RANGE).
# WARNING: a negative digit count rounds to a power of ten (Excel thousands mask); a percentage so
# rounded yields no code -> Excel "General".
excel_numfmt_code <- function(digits, pct, ci, text, signed = FALSE, mult = FALSE,
                              mult_over = "", mult_under = "") {
  out <- rep(NA_character_, length(digits))
  ok  <- !is.na(digits)
  if (!any(ok)) return(out)

  n    <- digits[ok]
  p    <- pct[ok]
  isci <- ci[ok]
  txt  <- text[ok]
  sgn  <- if (length(signed) == 1L) rep(signed, sum(ok)) else signed[ok]
  rat  <- if (length(mult)   == 1L) rep(mult,   sum(ok)) else mult[ok]
  m_ov <- if (length(mult_over)  == 1L) rep(mult_over,  sum(ok)) else mult_over[ok]
  m_un <- if (length(mult_under) == 1L) rep(mult_under, sum(ok)) else mult_under[ok]
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
  # explicit +/- for diff/contrib cells, leading multiply sign for ratio cells.
  # WARNING: the multiply sign is backslash-escaped (xl_numfmt_literal), NOT quote-wrapped -- a raw "
  # in a formatCode crashes the older jamovi openxlsx2.
  can <- !is.na(res) & res != "TEXT"
  s2  <- can & sgn
  res[s2] <- paste0("+", res[s2], ";-", res[s2])
  # THE MULTIPLICATIVE PAIR, one section per side of the neutral: the cell holds the SIGNED FOLD
  # (fmt_excel_value), so the positive section prints the over glyph and the negative one the under
  # glyph -- "x1.20" and "/1.20", "2.11" and "1/2.11". The second section is unconditional, which is
  # what makes Excel drop the minus: a `[<0]` condition would print it.
  r2  <- can & rat
  res[r2] <- paste0(xl_numfmt_literal(m_ov[r2]), res[r2], ";",
                    xl_numfmt_literal(m_un[r2]), res[r2])
  out[ok] <- res
  out
}

# xl_numfmt_affix() -- put a literal before and/or after a number in EVERY SECTION of its format
# code. A code may have two sections (a signed difference, a multiplicative pair), and a suffix
# pasted onto the whole string lands on the last one alone -- which is why a NEGATIVE difference used
# to wear the significance stars while its positive twin went bare.
# WARNING: the literal is backslash-escaped (xl_numfmt_literal), NEVER double-quote-wrapped -- a raw
# " in a formatCode crashes the older jamovi-bundled openxlsx2 ("xml import unsuccessful").
#' @keywords internal
xl_numfmt_affix <- function(code, prefix = "", suffix = "") {
  n   <- length(code)
  pre <- if (length(prefix) == 1L) rep(prefix, n) else prefix
  suf <- if (length(suffix) == 1L) rep(suffix, n) else suffix
  vapply(seq_len(n), function(i) {
    if (is.na(code[i])) return(NA_character_)
    if (!nzchar(pre[i]) && !nzchar(suf[i])) return(code[i])
    parts <- strsplit(code[i], ";", fixed = TRUE)[[1]]
    paste(paste0(xl_numfmt_literal(pre[i]), parts, xl_numfmt_literal(suf[i])), collapse = ";")
  }, character(1))
}


# DESIGN: the central display method -- every display mode -> a rendered string (or, syntax="excel",
# a numFmt code). pct is stored 0-1 and x100'd here; CI display is "moe" (+/-) or "ci" ([lo;hi]) per
# option; the same masks feed excel_numfmt_code() so text and Excel cannot diverge.
# DESIGN: FORMAT() NEVER PASTES A STRING IT DID NOT RENDER. An annotation -- "ref:", "mean:", the
# reference "%" beside an odds ratio, the "(sigma sd)" tail -- is added only where fmt_rendered()
# holds for the annotation AND the cell; otherwise the cell keeps its own value. So a void field
# renders BLANK, never the literal "NA", and a blank cell takes no significance star.
# DESIGN: ... but A TEMPLATE'S TOP-LEVEL LITERAL *IS* RENDERED CONTENT. A spent primary blanks the
# cell only when the template has nothing else to say, so "{n_range}<sparkline>" still draws its
# curve on a row that has no count. Only bracket-group 0 counts: "(Chi2)" and "(n=" sit inside a
# group, which is what keeps a void p-value or a void percentage blank. No template the package
# itself writes has a bare top-level literal (asserted in test-display-grammar.R), so this reaches
# user-authored displays and the base-count sparkline alone.
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
#' right-padded so numbers stay aligned. They **support** the number rather than compete with it: in
#' every theme they are drawn like an aside -- the chrome's secondary grey, never bold, italic or
#' underlined -- so a run of symbols never shouts louder than the value it qualifies. The main
#' display (console, [tab_kable()], [tab_md()]) sets this `TRUE`; tooltip / secondary-field
#' re-renders leave it `FALSE`, so stars never leak.
#' @param theme Which palette the cells are being rendered in. Only the black-and-white publication
#' palettes use it: `theme = "print_marks"` writes a repeated superscript mark after each value
#' instead of the significance stars, drawn exactly like them (see `stars`). `NULL` (the default)
#' renders no palette annotation at all.
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
#' @param .digits Internal: the precision one token was named at in a composite template
#' (`"{base:1}"`), passed by the composite expander; overrides every declared default.
#'
#' @return The fmt printed in a character vector.
#' @export
#' @keywords internal
format.tabxplor_fmt <- function(x, ..., html = FALSE, na = NA,
                                special_formatting = FALSE, stars = FALSE, theme = NULL,
                                bold_split = FALSE, pad = if (isTRUE(html)) fig_space else " ",
                                syntax = c("text", "excel"), .ref = NULL, .digits = NULL) {
  syntax <- match.arg(syntax)

  out    <- get_num(x)
  na_out <- is.na(out)

  # keep the RAW display for composite expansion at the end; the dispatch masks run on the PRIMARY.
  raw_display <- get_display(x)
  scl      <- fmt_scale_row(x)
  # `{est}` / `{base}` resolve HERE, once, to the token this column has always rendered them as, so
  # every mask, glyph, annotation and Excel code below applies to them with no arm of their own.
  display <- fmt_resolve_scale_tokens(display_primary(raw_display), scl)
  nas  <- is.na(display)
  digits <- get_digits(x)
  digits[!nas & display %in% c("n", "n_range")] <- 0
  # ONLY 0 is overridden, and only where the token declares a floor (DISPLAY_TOKENS$min_digits): a
  # cell asking for 1 or 3 decimals gets them. `digits` is per-cell but ONE value serves every
  # display of that cell -- a percentage wants 0 and its own ratio wants 2 -- so the floor cannot be
  # stored on the cell; it belongs where what is being SHOWN is known, which is here.
  # ⚠ 0 MEANS "UNSET", which is why a SCALE must not declare a precision its own estimate token
  # already declares (REG_CELL_DIGITS): a 1 there does not raise the token's 2, it SILENCES it --
  # which is how a grouped-binomial ratio printed x1.4 where x1.44 was meant.
  md <- unname(DISPLAY_MIN_DIGITS[display])
  digits[!nas & !is.na(md) & digits == 0L] <- md[!nas & !is.na(md) & digits == 0L]
  # THE LEVEL HAS ITS OWN PRECISION (EST_SCALES$base_digits). Keyed on the RAW token, so it reaches a
  # `{base}` aside (the composite loop below re-enters with the unresolved name) and a bare
  # `display = "base"` column alike, and never a `{pct}` a user wrote themselves.
  if (!is.null(scl$base_digits))
    digits[!nas & raw_display == "base"] <- scl$base_digits
  # ...and THE ESTIMATE has its own where the scale says so: `{est}` as written, or the token it
  # resolves to, so a bare `display = "or"` column reads at the same precision as `{est}` does.
  # ⚠ a FLOOR, unlike base_digits: the level's precision may be coarser than the cell's and the
  # estimate's may not, so a user asking for more decimals must still get them.
  if (!is.null(scl$est_digits)) {
    e <- !nas & (raw_display == "est" | display == (scl$est_display %||% ""))
    digits[e] <- pmax(digits[e], scl$est_digits)
  }


  ok <- !na_out & !nas


  is_mean  <- identical(scl$var_kind, "mean")
  is_coef  <- identical(scl$var_kind, "coef")
  # the stored interval's geometry, as the scale declares it.
  ci_mult  <- isTRUE(scl$mult)

  # the multiplicative rendering's column-level facts, resolved once (the block after print_num and
  # the `{ci}` bracket both read them). `mult_under` is the MEASURE's own glyph -- see that block.
  ratio_raw    <- tx_ratio_print_raw()
  mult_inverse <- ci_mult && !ratio_raw
  mult_under   <- MEASURES[[scl$label_meas]]$break_under

  pm <- stringi::stri_unescape_unicode("\\u00b1")

  # AN INTERVAL RENDERS ON ITS COLUMN'S OWN SCALE, and `is_pct` is the one fact that decides: x100
  # with a "%" exactly where the ESTIMATE is a percentage (a cell %, a percentage-point difference),
  # bare everywhere else -- a mean's absolute bounds, a coefficient's, a ratio's. It replaces a
  # per-token guess that made a bare `{ci}` x100 a gaussian coefficient and drop the bracket on an
  # odds ratio altogether.
  is_pct  <- isTRUE(scl$is_pct)
  # a token whose declared `unit` is "pct" is a PROPORTION: printed x100 with a "%", wherever it sits.
  # One statement of it (DISPLAY_TOKENS), so a new such token needs no edit here.
  unit_pct      <- ok & display %in% DISPLAY_PCT_TOKENS
  # ⚠ THE `diff` TOKEN ASKS ABOUT THE COLUMN'S LEVEL, not about its estimate, and `var_kind` is that
  # fact: a difference is in percentage POINTS wherever the level it is a difference of is a
  # percentage -- which includes the additive reading of an odds-ratio or risk-ratio column, whose
  # own `is_pct` is FALSE. A COEFFICIENT column (`var_kind = "coef"`: a gaussian beta, a log-link
  # estimate) is bare, and used to print "-364.1%" for -3.6 in print, in Excel and on hover alike.
  diff_pct      <- identical(scl$var_kind, "pct")
  pct_or_ci     <- ok & (display %in% c("pct", "ctr") | (display == "diff" & diff_pct) |
                         (display %in% c("ci", "moe") & is_pct))
  pct_or_ci     <- pct_or_ci | unit_pct
  # a bare difference reads at ONE decimal at least, whether it is a mean's or a coefficient's.
  diff_mean <- ok & display == "diff" & !diff_pct

  # `obs` and `gap` print exactly like the estimate they are compared to / measured on (per-COLUMN
  # scale, one branch per column). `gap` carries the ADJUSTMENT SCORE -- the very number the colour
  # grades -- so a printed gap and its shade can never say different things.
  obs_m    <- ok & (display %in% c("obs", "gap"))
  obs_mult <- ci_mult                          # OR / RR / IRR       -> like `or`  (bare, big.mark, 2 dg)
  obs_coef <- !ci_mult && is_coef              # beta / log(OR)      -> like `coef` (plain)
  obs_pct  <- !ci_mult && !is_coef             # AME / risk-diff     -> like `diff` (x100, signed, %)
  # the precision follows the SCALE, not the cell's own value, so an empty cell keeps its column's
  # Excel number format (`!nas` rather than `ok`).
  # an `obs` / `gap` takes the SAME decimals floor as the estimate it is compared to: one comparison,
  # one precision, so a crude ratio never prints two decimals beside a modelled one printing one.
  if (obs_mult) {
    o_floor <- unname(DISPLAY_MIN_DIGITS[scl$est_display %||% NA_character_])
    if (!is.na(o_floor))
      digits[!nas & display %in% c("obs", "gap") & digits == 0L] <- o_floor
  }
  obs_as_pct <- obs_m & obs_pct
  # EVERY scale: the `ci` token is the interval of whatever this column compares, and each column
  # answers it with its own geometry.
  disp_ci   <- display == "ci"  & !nas
  disp_moe  <- display == "moe" & !nas
  # THE decimals floor for an interval bound, derived from the scale (fmt_ci_digits()), one rule for
  # the `{ci}` bracket and the `{moe}` half-width alike.
  ci_floor  <- fmt_ci_digits(scl)
  if (ci_floor > 0L) {
    lo_dg <- !nas & display %in% c("ci", "moe") & digits < ci_floor
    digits[lo_dg] <- ci_floor
  }

  pct_no_ci     <- ok & (display %in% c("pct", "ctr") | (display == "diff" & diff_pct))
  pct_no_ci     <- pct_no_ci | obs_as_pct | unit_pct
  # a coefficient of variation is read as a rough order of magnitude ("about a third of the mean"),
  # so it takes no decimals whatever the cell's own `digits` says.
  digits[ok & display == "cv"] <- 0L
  # THE ADDITIVE ESTIMANDS, one list: a difference, a coefficient, a standardized residual (direction
  # is half of what one says, so it must never print bare) and an `obs`/`gap` on an additive scale.
  # A ratio carries a multiply glyph instead, and `gof` shares the `diff` FIELD but is a model-fit
  # statistic, not an estimate -- neither is ever signed.
  diff_signed   <- ok & (display %in% c("diff", "coef", "resid") | (obs_m & !obs_mult))
  # A THOUSANDS MARK IS A PROPERTY OF THE NUMBER, NOT OF THE TOKEN THAT CARRIES IT. It used to be a
  # list of tokens, which left `diff` / `coef` / `gap` out -- so one gaussian column printed its
  # Constant row "101 002.4" (token `mean`) beside its effects "+14088.0" (token `coef`). Every
  # rendered value takes it now; a value under 1000 is unchanged, so only the omissions moved.
  n_wn          <- ok
  pvalue        <- ok & display == "pvalue"
  # a base count is a genuine RANGE only where a larger `tot_n` sits beside the `n`; otherwise the
  # token prints one number, and Excel keeps it an editable count instead of text.
  n_range_hi    <- ok & display == "n_range" & !is.na(get_tot_n(x)) &
    get_tot_n(x) > as.double(get_n(x))
  n_range_hi[is.na(n_range_hi)] <- FALSE

  pct_or_ci <- pct_or_ci | obs_as_pct
  out[pct_or_ci] <- out[pct_or_ci] * 100
  digits[diff_mean] <- ifelse(digits[diff_mean] == 0, 1, digits[diff_mean])
  # `.digits`: THIS TOKEN'S OWN PRECISION, named in the template ("{base:1}"). Applied last, so it
  # beats every declared default above -- the token's minimum, the level's `base_digits`, the
  # interval floor. Internal: the composite expander passes it, no user calls format() with it.
  # Where it does NOT, the PRIMARY token's own suffix is read off the template here, because this
  # call renders the primary -- and the Excel number format returns just below, never reaching the
  # expander.
  pdg <- if (is.null(.digits)) display_primary_digits(raw_display)
         else rep(as.integer(.digits)[[1L]], length(digits))
  digits[!nas & !is.na(pdg)] <- pdg[!nas & !is.na(pdg)]

  # Excel number-format codes reuse format()'s OWN finalized masks + adjusted digits, so the tab_xl
  # bypass can never drift from the console display. Return here, before any string building.
  if (syntax == "excel") {
    # pvalue is shown x100 with "%" by its own rendering path (not pct_or_ci), so add it to the
    # Excel "%" mask; a p-value shown as a "<0.01%" threshold still stores its raw value.
    excel_pct <- pct_or_ci | (!nas & display == "pvalue")
    # diff + contrib get an explicit +/- sign; ratio a leading x; mean diffs join the `signed` mask
    # (so the bypass matches format()'s "+1.2").
    # `moe` IS the +/- numFmt; the BRACKET form has no number format at all, so it exports as the
    # rendered string like every other composite-looking cell.
    mp <- fmt_mult_plan(x, display, scl)
    gl <- fmt_mult_glyphs(mp$measure)
    # under `ratio_print = "raw"` the workbook keeps the raw number, so it prints ONE section with the
    # over glyph -- the same thing the console does there.
    if (ratio_raw) gl$under <- gl$over
    return(excel_numfmt_code(digits, pct = excel_pct,
                             ci = !nas & display == "moe",
                             text = n_range_hi | (!nas & display == "ci"),
                             signed = (!nas & display %in% c("ctr", "diff", "resid")) | obs_as_pct,
                             mult = !nas & mp$cells,
                             mult_over = gl$over, mult_under = gl$under))
  }


  # the numbers as such, kept for the multiplicative block below (from here `out` is character).
  num_out <- out
  out[!na_out] <- print_num(out[!na_out], digits[!na_out])
  out[na_out] <- NA
  # THE TWO INTERVAL TOKENS, one notation each, neither reading an option: `{ci}` is `[lo;hi]` from
  # the stored bounds, `{moe}` the half-width `+/-x`. Both take the "%"/x100 and the decimals from the
  # COLUMN's own scale, so one template works on a percentage column and a mean column alike. A void
  # bound yields NA -- an interval that was never computed renders BLANK, never the centre value
  # wearing brackets -- and the composite expander then pads it to its column's width.
  if (any(disp_ci))
    out[disp_ci] <- fmt_ci_bracket(get_ci_inf(x)[disp_ci], get_ci_sup(x)[disp_ci], digits[disp_ci],
                                   is_pct = is_pct, clamp = is_pct && identical(scl$kind, "level"),
                                   mult_under = if (mult_inverse) mult_under else NULL)
  if (any(disp_moe)) {
    m <- out[disp_moe]                                    # NA -> NA, like the bracket
    out[disp_moe] <- ifelse(is.na(m), NA_character_, paste0(pm, m, if (is_pct) "%" else ""))
  }
  # the thousands mark IS the pad glyph (`pad` resolves per medium: ASCII in console/markdown, a figure
  # space in html/Excel), so the mark can never disagree with the padding it sits in.
  out[n_wn] <- out[n_wn] |> prettyNum(big.mark = pad, preserve.width = "individual")
  if (any(n_range_hi)) {
    hi <- print_num(get_tot_n(x)[n_range_hi], 0L) |>
      prettyNum(big.mark = pad, preserve.width = "individual")
    lo <- out[n_range_hi]
    # WARNING: EACH BOUND IS PADDED TO ITS OWN COLUMN, so the "-" separators line up down the
    # column. The composite expander pads the joined string as one token, which would leave the
    # separator ragged ("( 8 610-16 301)" over "(  1 700-3 093)"). Same rule, and the same medium
    # `pad` glyph, as the "(sigma sd)" tail below.
    lo <- stringi::stri_pad(lo, width = max(stringi::stri_length(lo)), side = "left", pad = pad)
    hi <- stringi::stri_pad(hi, width = max(stringi::stri_length(hi)), side = "left", pad = pad)
    out[n_range_hi] <- paste0(lo, "-", hi)
  }
  out[pct_no_ci] <- paste0(out[pct_no_ci], "%")

  # === THE ONE MULTIPLICATIVE RENDERING ==========================================================
  # A cell printing a multiplicative quantity shows its DISTANCE from the neutral, not its raw value:
  # below the neutral it prints the inverse, so "half" reads as strongly as "double". The glyphs are
  # the MEASURE's own -- MEASURES$<m>$break_over / break_under, the very pair the legend ladder and
  # the forest axis print -- so a cell, its ladder and its axis cannot disagree. An odds ratio reads
  # "1/2.67", a risk / rate / mean ratio "/2.67" and "x1.5".
  # WARNING: this sits OUTSIDE special_formatting on purpose. It was the composite expander's
  # recursion (special_formatting = FALSE) that dropped the inverse, so "{or} ({obs})" printed a raw
  # "0.37" beside a "1/2.67" in the same table. Text syntax only (Excel keeps a real number).
  # Opt out with options(tabxplor.ratio_print = "raw") for the journal convention.
  mult_plan  <- fmt_mult_plan(x, display, scl)
  mult_cells <- ok & mult_plan$cells
  if (any(mult_cells) && !ratio_raw) {
    v    <- num_out[mult_cells]
    dg   <- digits[mult_cells]
    meas <- mult_plan$measure[mult_cells]
    inv  <- !is.na(v) & v > 0 & v < 1
    mag  <- ifelse(inv, 1 / v, v)
    num  <- prettyNum(print_num(mag, dg), big.mark = pad, preserve.width = "individual")
    # a value ROUNDING to the neutral takes the over glyph, never the confusing "/1.00" -- at that
    # rounding the two say the same thing. Only a REFERENCE cell loses the glyph (below).
    one  <- !is.na(mag) & round(mag, dg) == 1
    glyph <- function(side)
      vapply(meas, function(k) MEASURES[[k]][[side]], character(1), USE.NAMES = FALSE)
    val  <- ifelse(inv & !one, paste0(glyph("break_under"), num), paste0(glyph("break_over"), num))
    val[is.na(v)] <- out[mult_cells][is.na(v)]
    out[mult_cells] <- val
  }

  # THE cells that ARE the baseline of this column's comparison -- what the bare-neutral rules below
  # and the "ref:" annotation are keyed on. Memoized, and read through a function so a column that
  # needs neither never pays the lookup. On a crosstab it is `all_totals` (the reference ROW and,
  # under `ref2`, the reference COLUMN); on a REGRESSION column it is `in_refrow`, which every
  # producer stamps -- get_reference() returns nothing at all on a `raw_diff` / `log_coef` column,
  # whose pct_type is "none", which is why a gaussian coefficient needed its own rule before.
  ref_alltot <- if (!is.null(.ref)) .ref$all_totals else NULL
  .ref_base  <- NULL
  ref_base   <- function() {
    if (is.null(.ref_base)) {
      .ref_base <<- if (nzchar(as.character(get_role(x))[1])) is_refrow(x)
                    else {
                      if (is.null(ref_alltot)) ref_alltot <<- get_reference(x, "all_totals")
                      ref_alltot
                    }
    }
    .ref_base
  }

  # DESIGN: a REFERENCE cell of a multiplicative column prints a bare "1" -- no glyph, no decimals.
  # "x" means "times the reference", which the reference itself is not, and the short bare number is
  # what makes its row stand out. A cell that merely ROUNDS to the neutral keeps "x1.00": the reader
  # must be able to tell "this is the baseline" from "this happens to equal it".
  # WARNING: part of the ONE multiplicative rendering, so it sits OUTSIDE special_formatting too --
  # else the composite expander's recursion prints "x1.00 (51.3%)" where the bare token prints "1".
  # WARNING: `& at the neutral` is load-bearing -- a regression's Constant row IS a reference row and
  # its odds ratio is the baseline odds, a real value.
  if (any(mult_cells)) {
    at_one <- !is.na(num_out) & round(num_out, digits) == 1
    at_one[is.na(at_one)] <- FALSE
    out[mult_cells & ref_base() & at_one] <- "1"
  }

  if (any(pvalue)) {
    p    <- get_pvalue(x[pvalue])

    out[pvalue]    <- paste0(
      ifelse(
        p < 0.0001,
        "<0.01",
        print_num(p * 100, digits = 2L)
      ),
      "%"
    )
  }

  # ONE SIGNING RULE for every additive estimand: an explicit "+" on every non-negative value, the
  # rounded-to-zero "+0%" included -- exactly as a ratio rounding to the neutral keeps "x1.00" rather
  # than losing its glyph. Only a cell that IS the baseline drops the sign (the block below), so the
  # reader can tell "this is the reference" from "this happens to equal it". Means are signed too:
  # they render in the variable's own units ("+1.2" / "-0.22"), a pct diff minus the "%". The
  # sd-standardized (Glass's delta) view is a COLOUR device only (named by legend/tooltip, never the
  # cell), so the number always equals $diff and tab_xl cannot desync.
  out[diff_signed] <- ifelse(
    !startsWith(out[diff_signed], "-"),
    paste0("+", out[diff_signed]),
    out[diff_signed]
  )

  # DESIGN: A REFERENCE CELL PRINTS THE BARE NEUTRAL -- "0" / "0%" here, "1" above. It is the additive
  # twin of the multiplicative rule, and the same two masks: this cell IS the baseline, AND its value
  # rounds to the neutral. WARNING: it sits OUTSIDE special_formatting for the same reason its twin
  # does -- the composite expander recurses with special_formatting = FALSE, and a reference cell
  # would print "+0% (49%)" where the bare token prints "0%".
  # DESIGN: on a REGRESSION column the baseline row drops the sign whatever its value, because there
  # it is a LEVEL and not a comparison -- a log-scale intercept is log(odds) / log(mean), which "+"
  # would read as a gain. Every other scale renders that row on a level token, where the signing rule
  # never fires at all; only the log scales reach here. ⚠ the twin above keeps `at_one` all the same:
  # there the rule REPLACES the value, and a baseline odds is a real number.
  if (any(diff_signed)) {
    at_zero <- !is.na(num_out) & round(num_out, digits) == 0
    at_zero[is.na(at_zero)] <- FALSE
    base_c  <- diff_signed & ref_base() & (at_zero | nzchar(as.character(get_role(x))[1]))
    out[base_c] <- stringi::stri_replace_first_fixed(out[base_c], "+", "")
  }



  # per-cell bold-prefix widths, written by TWO branches (the mean/sd tail and the composite `{}`
  # templates), so allocated here ahead of both. Attached to the result only if one actually wrote.
  prim_nchar <- if (isTRUE(bold_split)) rep(NA_integer_, length(out)) else NULL
  prim_from  <- if (isTRUE(bold_split)) rep(NA_integer_, length(out)) else NULL
  # cells the composite expander wrote: the `na` argument below must not paint over them.
  wrote <- rep(FALSE, length(out))

  # THE CELL SUFFIX (stars, or a publication palette's marks), computed HERE -- before the
  # "mean (sigma sd)" tail -- so it can be attached to the PRIMARY token rather than after the aside.
  # WARNING: appending it at the end instead is what used to drop a star into the `.tx-sec` piece of a
  # "mean (sigma sd)" cell, where it was drawn in the aside's grey and un-bolded. The face and the
  # colour stop at the primary; so must what they grade.
  # PADDING: when any cell carries a suffix, every value cell reserves the column-max width, so the
  # numbers stay aligned. Footer SUMMARY cells (gof / pvalue rows) carry none and reserve none.
  st     <- fmt_cell_suffix(x, stars = stars, theme = theme)
  # fmt_rendered(), not nzchar(): a void token is padded to its width, not emptied.
  st_val <- fmt_rendered(out) & !(display %in% DISPLAY_FOOTER_TOKENS)
  st_pad <- rep("", length(out))
  st_w   <- 0L
  if (any(st_val & nzchar(st))) {
    st_w   <- max(nchar(st[st_val]))
    st_pad <- stringi::stri_pad(st, st_w, side = "right", pad = pad)
    st_pad[!st_val] <- ""
  }
  st_done <- rep(FALSE, length(out))

  if (special_formatting) {
    # compute each reference mask ONCE per column; the exporter prep passes precomputed masks via
    # `.ref`, else memoized lazily below (`ref_alltot` above). Keep `.ref = NULL` on the nested
    # reffmt format() calls.
    ref_cells  <- if (!is.null(.ref)) .ref$cells      else NULL
    # DESIGN: THE "ref:" LEVEL SUBSTITUTION IS A CROSSTAB ANNOTATION. A crosstab reference cell has
    # no other way to say what it sits at, so it shows its own percentage/mean in place of a
    # difference that is 0 by construction. A regression column states its level through `{base}` --
    # which its default display already prints -- so there the reference cell shows its measure's
    # NEUTRAL instead, and "ref:49% (49%)" never happens. The gate is `role` (a declared column
    # attribute, "" on crosstabs) and NOT the scale: tab(ci = "ref") stamps the very same `points`
    # scale a regression risk-difference column carries.
    is_crosstab <- !nzchar(as.character(get_role(x))[1])

    disp_diff   <- display == "diff" & !nas
    # the "ref:x-" annotation belongs to the +/- notation, so it fires exactly where `{moe}` renders
    disp_ctr    <- display == "ctr" & !nas
    disp_or     <- display == "or" & !nas
    if (any(disp_diff) && is_crosstab) {
      if (is.null(ref_cells)) ref_cells <- get_reference(x, "cells")
      ref     <- ref_cells[disp_diff]
      reffmt  <- set_display(x[disp_diff],
                             ifelse(scl$var_kind %in% c("count", "mean"), "mean", "pct")) |>
        format() #|> stringi::stri_trim()
      out[disp_diff] <- ifelse(ref & fmt_rendered(reffmt),
                               paste0("ref:", reffmt),
                               out[disp_diff])
    }

    if (any(disp_moe) && is_crosstab) {
      if (is.null(ref_cells)) ref_cells <- get_reference(x, "cells")
      ref     <- ref_cells[disp_moe]
      reffmt  <- set_display(x[disp_moe],
                             ifelse(scl$var_kind %in% c("count", "mean"), "mean", "pct")) |>
        format()
      out[disp_moe] <- ifelse(ref & fmt_rendered(reffmt),
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
      trim <- stringi::stri_trim(out[mctr])
      out[mctr] <- ifelse(fmt_rendered(trim) & is.finite(num_out[mctr]),
                          paste0("mean:", trim), "")
    }

    # LEVEL scales only: on a crosstab the odds ratio rides on a percentage column, and its reference
    # row is worth annotating with that percentage. An EFFECT column (every regression one) states
    # its level through `{base}` instead, which the reader asked for or did not.
    if (any(disp_or) && identical(scl$kind, "level")) {
      # a reference row is annotated with the empirical reference % when present (an empirical-OR
      # crosstab); a pure model-OR table has no pct, so the annotation drops.
      if (is.null(ref_alltot)) ref_alltot <- get_reference(x, "all_totals")
      refer  <- ref_alltot[disp_or]
      or_val <- get_or(x)[disp_or]
      vals   <- out[disp_or]

      # ⚠ NOT on an Excel ASIDE column (mat_aside_cols): it exists to hold ONE field beside the column
      # it was split out of, so re-composing the pair there would print the bracket twice.
      if (any(!is.na(get_pct(x)[disp_or])) && !fmt_is_aside(x)) {  # annotate ref %
        reffmt <- set_display(x[disp_or], "pct") |> set_digits(0L) |> format()
        reffmt <- suppressWarnings(
          stringi::stri_pad(reffmt, suppressWarnings(max(stringi::stri_length(reffmt), na.rm = TRUE)),
                           pad = pad)
        )
        # `!is.na(or_val)` -- a reference cell with NO odds ratio must not claim "1" (the cumulative
        # OR's degenerate last cut has a reference row and no ratio; it would print raw "NA").
        out[disp_or] <- ifelse(refer & !is.na(or_val) & fmt_rendered(reffmt),
                               paste0(vals, " (", reffmt, ")"), vals)
      }
    }

  }

  # OPT-IN: `stars` / `theme` are set only at the MAIN display sites (pillar_shaft, tab_kable, tab_md,
  # tab_xl), so tooltip / character-cast re-renders never leak an annotation.
  # Every value cell the "mean (sigma sd)" branch did not already suffix. THE SUFFIX IS A SUPPORTING
  # PIECE, not part of the number: a plain cell that wears one therefore gets a primary RANGE it would
  # not otherwise need, ending where the value ends -- so the stars / marks fall outside it and every
  # backend draws them like an aside.
  st_sel <- st_val & !st_done & nzchar(st_pad)
  if (any(st_sel)) {
    if (isTRUE(bold_split)) {
      fresh <- st_sel & is.na(prim_nchar)
      prim_from [fresh] <- 1L
      prim_nchar[fresh] <- nchar(out[fresh])
    }
    out[st_sel] <- paste0(out[st_sel], st_pad[st_sel])
  }

  # a "blank" cell (n_min mask) renders as a true empty string in every consumer, distinct from NA.
  out[!nas & display == "blank"] <- ""

  # opt-in COMPOSITE display -- a per-cell `display` template like "{pct} (n={n})" renders several
  # fields in one cell. Parsed ONLY here (gated by one grepl). Each {field} re-uses format() with a
  # simple token; STARS ride the primary; the cell is written wherever the PRIMARY rendered.
  #
  # DESIGN: AN EMPTY ASIDE KEEPS ITS WIDTH, so one missing field never breaks the column. A cell whose
  # bracket group has nothing to say renders that group as spaces of the same width -- the estimates
  # stay in the same character column as the rows that do have an aside. A group empty across EVERY
  # cell of the template is dropped outright (display_template_keep(), shared with
  # display_write_col()), so a column that can never fill an aside pays no padding at all.
  # WARNING: the padding is by CHARACTER COUNT -- exact in a monospace medium, within ~one digit
  # width in html/Excel, where a figure space is a digit wide but "(" and ")" are not.
  composite <- !nas & grepl("{", raw_display, fixed = TRUE)
  if (any(composite)) {
    for (tmpl in unique(raw_display[composite])) {
      seg   <- parse_display_template(tmpl)
      if (!any(seg$is_tok)) next
      # A TOP-LEVEL LITERAL IS CONTENT. A spent primary blanks the cell only when the template has
      # nothing else to say -- so "{n_range}<sparkline>" still draws its curve on a row that has no
      # count. Restricted to bracket-group 0: "(Chi2)" and "(n=" sit INSIDE a group, which is what
      # keeps a void p-value or a void percentage blank, exactly as before.
      lit0  <- any(fmt_rendered(seg$pieces[!seg$is_tok & seg$group == 0L]))
      cells <- which(composite & raw_display == tmpl)
      xc    <- x[cells]
      toks  <- lapply(seq_along(seg$fields), function(i) {
        # Stars ride the PRIMARY token -- the first one outside parentheses, not the first one written
        # -- so the others have their p-value blanked, EXCEPT `resid`, which is DERIVED from the
        # p-value (blanking it would render NA and drop the composite).
        xi <- if (i == seg$primary || identical(seg$fields[i], "resid")) xc
              else set_pvalue(xc, NA_real_)
        # fmt_set_display(): the RAW write. `est` / `base` are tokens here, never preset names.
        format(fmt_set_display(xi, seg$fields[i]), na = na, special_formatting = FALSE,
               # the token's own precision, if the template named one ("{base:1}")
               .digits = seg$field_digits[[i]],
               stars = isTRUE(stars) && i == seg$primary,
               theme = if (i == seg$primary) theme else NULL,
               # `bold_split` on the primary ONLY to learn how wide the suffix it added is, so the
               # range recorded below can stop before it.
               bold_split = i == seg$primary, pad = pad)  # the inner tokens pad too
      })
      # read BEFORE the padding below, which returns fresh vectors and drops the attribute.
      sfx_w <- attr(toks[[seg$primary]], "suffix_nchar") %||% 0L
      # an empty token becomes "" and is padded WITH the others, so it occupies its column's width
      void <- lapply(toks, function(s) !fmt_rendered(s))
      toks <- purrr::map2(toks, void, function(s, e) {
        s[e] <- ""
        if (all(e)) return(s)
        stringi::stri_pad(s, max(stringi::stri_length(s[!e])), side = "left", pad = pad)
      })
      keep <- display_template_keep(seg, vapply(void, all, logical(1)))
      strs <- vector("list", length(seg$pieces)); ti <- 0L
      for (j in seq_along(seg$pieces)) {
        if (seg$is_tok[j]) { ti <- ti + 1L; strs[[j]] <- toks[[ti]] }
        else {
          # in a non-breaking medium (pad != " "), the ASCII spaces in a template literal like " (n="
          # must not break -- U+00A0 keeps a normal-width join. Console (pad = " ") is byte-identical.
          piece <- seg$pieces[j]
          if (!identical(pad, " ")) piece <- gsub(" ", "\u00a0", piece, fixed = TRUE)
          strs[[j]] <- rep(piece, length(cells))
        }
        if (!keep[j]) strs[[j]] <- rep("", length(cells))
      }
      # per CELL: blank the literals of a bracket group whose every token is void (its tokens are
      # already spaces from the padding above). The primary's own group is never touched.
      prim_g <- seg$field_group[[seg$primary]]
      for (g in setdiff(unique(seg$field_group[seg$field_group > 0L]), prim_g)) {
        spent <- Reduce(`&`, void[seg$field_group == g])
        if (!any(spent)) next
        for (j in which(seg$group == g & !seg$is_tok & keep))
          strs[[j]][spent] <- strrep(pad, nchar(strs[[j]][spent]))
      }
      # A COMPOSITE RENDERS WHEREVER ANY OF ITS PIECES DID -- not only where the primary did. A cell
      # whose primary is void but whose aside is not still has something to say, and the brackets say
      # which it is: a numeric predictor's OBSERVED cell prints "[1.46]", the odds ratio it has,
      # aligned under the rows that also have a risk difference. A cell with nothing at all, and a
      # template whose only top-level literal is whitespace, still blank (`lit0`).
      ok_c <- purrr::reduce(lapply(void, `!`), `|`) | lit0
      asm  <- do.call(paste0, strs)
      out[cells[ok_c]] <- asm[ok_c]
      wrote[cells[ok_c]] <- TRUE
      # OPT-IN (bold_split) record of the primary token's character RANGE, so a backend can bold --
      # and colour -- only the field the cell is really about. A RANGE, not a prefix width: the
      # primary is the first token OUTSIDE brackets, which "({base}) {est}" puts last.
      # A blanked group keeps its width and a dropped one is "" for every cell of the template, so
      # the head width stays uniform and this needs no adjustment.
      # Off by default -> attribute-free output.
      # ⚠ NOT `ok_c`: a cell carried by its literal alone has no primary to bold or colour, and a
      # recorded zero-width range would wrap the whole string in the grey aside span. NA = one plain
      # piece, which is what paint_split() and html_cell_text() both read it as.
      if (bold_split) {
        pj    <- which(seg$is_tok)[seg$primary]
        prim  <- ok_c & !void[[seg$primary]]
        head  <- if (pj > 1L) do.call(paste0, strs[seq_len(pj - 1L)]) else rep("", length(cells))
        prim_from [cells[prim]] <- nchar(head)[prim] + 1L
        # minus the suffix the primary token wears: the stars / marks are a supporting piece and fall
        # OUTSIDE the range, beside the asides. The token's own pad is leading, so the range still
        # ends on the value.
        prim_nchar[cells[prim]] <- nchar(strs[[pj]])[prim] - sfx_w
      }
    }
  }

  # honour the `na` argument on the main path, applied LAST so it dominates every intermediate append.
  # Default na=NA -> no-op; tab_kable()/tab_md() pass na="" -> NA cells render "" at source.
  # ⚠ `!wrote`: a cell whose number is NA but whose template said something anyway (a top-level
  # literal) HAS been rendered -- painting `na` over it would leave the feature console-only, since
  # md / xl / html / transpose all pass na = "".
  if (!is.na(na)) out[na_out & !wrote] <- na

  # expose the per-cell bold-prefix width (NA elsewhere) so exporters bold only the primary field.
  # Dropped by any downstream string op, so consumers must read it right after format().
  if (!is.null(prim_nchar) && any(!is.na(prim_nchar))) {
    attr(out, "primary_nchar") <- prim_nchar
    attr(out, "primary_from")  <- prim_from
  }
  # how wide the cell suffix is -- read by the composite expander above, which calls this on its own
  # primary token and must subtract it from the range it records.
  if (isTRUE(bold_split) && st_w > 0L) attr(out, "suffix_nchar") <- st_w

  out
}






# === SECTION: the primary/secondary paint split ======================================================
#
# A composite cell reads as ONE number with an aside -- "1/1.63*** (31%)" -- and what a measure grades
# is the number, not the aside. So only the PRIMARY token carries the cell's rendering and the aside is
# set slightly back from the table's own text. That is the colour, and equally the FACE wherever a
# palette speaks through typography (a publication one): both are the measure talking, so both stop in
# the same place -- the face half is applied by html_cell_text() (R/tab-render-html.R), which is where
# the pieces are known. The split is possible at all because format(bold_split = TRUE) hands back the
# primary's character RANGE -- the same fact the exporters already use to bold only the primary field.
# DESIGN: THERE IS NOTHING TO CHOOSE HERE. Which shade an aside takes is a PALETTE fact, resolved
# per theme like every other piece of chrome; the only thing left to decide is whether the split
# happens at all, which is the one expert opt-out below.
#' @keywords internal
#' @noRd
color_whole_cell_opt <- function() isTRUE(tx_option("color_whole_cell"))

# THE colour an aside takes, per THEME -- one resolver, so the console, the stylesheet and any future
# backend cannot disagree, and so a light-theme grey can never be baked into a dark page.
# DESIGN: it is the chrome's `grey2`, and that is NOT a coincidence -- that slot already means
# "present, but nothing is being said about it": tx_chrome_hex() gives it to an uncoloured cell in a
# column with no colour measure (the `g2` class), and an aside is the same idea inside one cell.
# WARNING: `grey2` therefore has TWO readers. Retuning it moves both, which is intended; deleting it
# for one breaks the other.
#' @keywords internal
#' @noRd
color_secondary_hex <- function(theme = "light") tx_chrome_hex(theme)$grey2

# The painter a backend applies to one cell: `style` over the primary range, `sec` over the rest.
# A cell with no recorded range (a simple token, or an NA) is painted whole -- there is no aside.
#' @keywords internal
#' @noRd
paint_split <- function(txt, style, from, n, sec = identity) {
  if (!length(txt)) return(txt)
  whole <- is.na(from) | is.na(n) | from <= 1L & n >= nchar(txt)
  out <- txt
  out[whole] <- style(txt[whole])
  k <- !whole & !is.na(txt)
  if (any(k)) {
    to  <- from[k] + n[k] - 1L
    pre <- substr(txt[k], 1L, from[k] - 1L)
    mid <- substr(txt[k], from[k], to)
    post <- substr(txt[k], to + 1L, nchar(txt[k]))
    out[k] <- paste0(ifelse(nzchar(pre), sec(pre), pre), style(mid),
                     ifelse(nzchar(post), sec(post), post))
  }
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
  # `.ref` (precomputed reference masks) threads through to format() and the greying `totals` mask;
  # NULL on the console path.
  # `bold_split`: the primary token's character range, so the colour lands on the number and not on
  # the aside beside it (unless tabxplor.color_whole_cell opts out of the split).
  # the console's own palette decides the cell suffix: a publication palette marks, the colour ones
  # star. The console does not select one by default -- options(tabxplor.color_style_theme=) can.
  out     <- format(x, special_formatting = TRUE, stars = TRUE, .ref = .ref, bold_split = TRUE,
                    theme = tx_theme_option("console") %||% "light")
  prim_f  <- if (color_whole_cell_opt()) NULL else attr(out, "primary_from")
  prim_n  <- attr(out, "primary_nchar")
  # the CONSOLE's own theme (tabxplor.color_style_theme, best-effort detected), so a dark terminal
  # gets the light grey and a light one the dark grey -- never a colour baked for the other.
  sec_hex <- if (is.null(prim_f)) NULL else color_secondary_hex(tx_theme_option("console"))
  sec_sty <- if (is.null(sec_hex)) identity
             else tryCatch(cli::make_ansi_style(sec_hex), error = function(e) identity)
  paint   <- function(txt, style, cells)
    if (is.null(prim_f)) style(txt)
    else paint_split(txt, style, prim_f[cells], prim_n[cells], sec_sty)
  # THE one styling write: paint the cells of `mask` and put them back. `bolded` rides the PRIMARY,
  # so it is composed into the style rather than wrapped around the finished string.
  paint_cells <- function(mask, style, bolded) {
    for (b in unique(bolded[mask])) {
      cells <- mask & bolded == b
      if (!any(cells)) next
      out[cells] <<- paint(out[cells],
                           if (b) function(z) cli::style_bold(style(z)) else style, cells)
    }
  }
  out     <- as.character(out)
  display <- get_display(x)
  nas     <- is.na(display)
  color   <- get_color(x)
  color_bg <- get_color_bg(x)                        # the background channel measure
  totrows <- is_totrow(x)
  # bold reference/total (+ coloured) cells, but ONLY on a console that renders ANSI bold at fixed
  # glyph width (tabxplor.console_bold, IDE-gated; off by default). Read fresh so a toggle applies.
  bold_on <- isTRUE(getOption("tabxplor.console_bold"))

  # DESIGN: a measure that REQUIRES total rows stores what it needs ON them -- contrib keeps the
  # per-(sub)table MEAN contribution to variance there (get_mean_contrib) -- so without them it cannot
  # print. The requirement is the measure's own declared `requires["totrow"]`.
  needs_totrow <- measure_forces(color, "totrow") && !any(totrows)
  if (needs_totrow) warning(
    "cannot print color == '", measure_key(color), "' with no total rows to store ",
    "information about mean contributions to variance"
  )

  na_out  <- is.na(out)
  ok      <- !na_out & !nas


  has_text  <- !is.na(color)    && ! color    %in% c("no", "")
  has_bg    <- !is.na(color_bg) && ! color_bg %in% c("no", "")
  if ((has_text || has_bg) & !(has_text && needs_totrow)) {
    # two integer slot vectors (text + background channel; 0 = uncolored). Text uses the option
    # palette; background always the pale bg palette, stacked on top (a cell can carry both).
    channels    <- fmt_color_channels(x)
    text_styles <- get_color_style()                  # current type/theme/24-bit options (ANSI, cli)
    bg_styles   <- get_color_style(type = "bg")

    totals <- if (!is.null(.ref)) .ref$all_totals else get_reference(x, "all_totals")
    # a reference ROW is also an anchor: a regression EMPIRICAL column marks its reference CATEGORY via
    # in_refrow, which get_reference("all_totals") misses. For crosstabs is_refrow is a subset -> no-op.
    totals <- totals | is_refrow(x)
    # Cells matching no break on EITHER channel are greyed (style_subtle) so colored cells stand out;
    # reference/total cells are exempt, staying full-strength as reading anchors.
    unselected <- channels$text_slot == 0L & channels$bg_slot == 0L
    # bold = the anchors PLUS the text-coloured cells (export parity, matching fmt_col_ann()). It
    # rides the PRIMARY only, like the html span's font-weight:normal on the aside. pillar measures
    # the ANSI-stripped width, so bold adds none -- alignment holds.
    bolded <- if (bold_on) ok & (totals | channels$text_slot > 0L) else rep(FALSE, length(out))

    # EVERY cell goes through paint() exactly ONCE, and the style it paints with is the COMPOSITION of
    # everything that applies to the primary: its text slot (or the greyed / anchor fallback), its
    # BACKGROUND slot, and bold. Composed BEFORE the paint, never wrapped around the finished string.
    # ⚠ THE BACKGROUND IS A COLOUR MEASURE, NOT THE CELL'S GROUND. It grades the number exactly as the
    # text channel does, so it must stop at the primary; wrapping it afterwards filled the aside too,
    # and the aside is not what any measure grades. Wrapping is also unsafe in general: an inner reset
    # ends the outer style early, which is what an aside written FIRST ("({base}) {est}") would do.
    grey_cells <- ok & unselected & !totals
    key <- paste(channels$text_slot, channels$bg_slot, grey_cells, bolded)
    for (k in unique(key[ok])) {
      cells <- ok & key == k
      i <- which(cells)[[1]]
      f <- if (channels$text_slot[i] > 0L) text_styles[[channels$text_slot[i]]]
           else if (grey_cells[i])         pillar::style_subtle
           else                            identity
      for (g in list(if (channels$bg_slot[i] > 0L) bg_styles[[channels$bg_slot[i]]],
                     if (bolded[i]) cli::style_bold))
        if (!is.null(g)) f <- local({ inner <- f; outer <- g; function(z) outer(inner(z)) })
      out[cells] <- paint(out[cells], f, cells)
    }

    #Columns with no color
  } else {
    # DESIGN: uncolored columns only grey out zeros here. Bold / underline / border styling for totals
    # was tried and rejected: bold offsets column widths in the console.
    # The aside still takes the secondary colour: a Total cell's "100% (1 157-2 139)" reads the same
    # way here as in every other column and in html.
    bolded <- if (bold_on) {
      ok & ((if (!is.null(.ref)) .ref$all_totals else get_reference(x, "all_totals")) | is_refrow(x))
    } else rep(FALSE, length(out))
    paint_cells(ok, identity, bolded)
    out[ok] <- out[ok] |>
      stringi::stri_replace_first_regex("^0%$|^-0%$", pillar::style_subtle("0%")) |> # 0 in gray
      stringi::stri_replace_first_regex("^0$|^0$", pillar::style_subtle("0"))
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
#' @return The relevant field of the tabxplor_fmt.
#' @export
#' @keywords internal
`$.tabxplor_fmt` <- function(x, name) {
  # DESIGN: $wn falls back to the raw count n when there are no weighted counts (same fallback as
  # get_wn() -- keep in sync). $ci is not a stored field: it is recomputed from the ci_inf/ci_sup
  # bounds by get_ci(), so user code reading $ci keeps working. ($rr is a read-side alias of $ratio.)
  if (name == "wn" & all(is.na( dplyr::pull(vctrs::vec_proxy(x), "wn")))) {
    dplyr::pull(vctrs::vec_proxy(x), "n")

  } else if (name == "ci") {
    get_ci(x)

  } else if (name == "tot_wn") {
    get_tot_wn(x)

  } else if (name == "in_totrow") {
    # `in_totrow` is no longer a field (row_kind has seven values). The README teaches `$` field
    # access, so the logical read keeps working.
    is_totrow(x)

  } else {
    dplyr::pull(vctrs::vec_proxy(x), name)
  }

}

# WARNING: do NOT add a `[[.tabxplor_fmt` method. It was tried (forwarding to vec_proxy() so `[[`
# extracts a FIELD) and it broke dplyr::last() on fmt vectors, which relies on `[[` returning an element.



# ============================================================================================
# findInterval colour ENGINE. Per column, per channel:
#   fmt_color_plan(x, channel)  -> the measure, per-cell score, significance gate, positive breaks
#                                  and the level->slot maps (per direction).
#   fmt_color_slots(x, plan)    -> fold the score to a magnitude that grows away from its neutral
#                                  centre, findInterval() against the positive breaks, split by
#                                  direction into palette slots (0 = uncolored).
#   fmt_color_channels(x)       -> list(text_slot, bg_slot): the only artifact consumers map to ansi/hex.
# ============================================================================================

#' @keywords internal
color_scales <- function() {
  sc <- getOption("tabxplor.color_breaks")
  if (is.null(sc) || is.null(sc$pct_diff)) return(default_color_scales())
  # fill in scales a STALE option list predates: a session that saved the option before a scale existed
  # would otherwise hand the engine a NULL (silently uncoloured) scale.
  utils::modifyList(default_color_scales(), sc)
}

# ONE per-direction break scale, rewritten for the `guaranteed_effect` policy. That policy scores the
# CI FLOOR instead of the estimate, and is meant to COLOUR MORE -- to show everything solid in a small
# table while still grading what is left of the effect -- so the ladder starts at the neutral: every
# cell whose interval excludes it takes at least the faintest shade.
#   THE RULE: prepend the neutral, drop the top rung. One rung down, no arithmetic, so every printed
#   threshold is a number the reader already knows from the same ladder under `ignore`, and a shade
#   means the same size of deviation whichever policy is on.
#     additive       c(0.05, 0.10, 0.20, 0.30) -> c(0, 0.05, 0.10, 0.20)
#     multiplicative c(1.1,  1.2,  1.5,  2   ) -> c(1, 1.1,  1.2,  1.5 )
#   The top rung is not lost: a guaranteed effect is smaller than its estimate by construction, so the
#   old top rung fired on almost nothing -- dropping it RECOVERS the deepest shade.
# `origin` is the one exemption, declared by the scale that needs it: `zscore` is written in confidence
# levels, its first rung IS the significance threshold, so it re-anchors there instead. Prepending 0
# would give it a structurally empty faintest shade (|z| <= 1.96 is exactly a gated-out cell).
#   c(1.96, 2.58, 3.89, 6) with origin 1.96 -> unchanged ; at conf 0.99 -> c(2.58, 3.20, 4.51, 6.62)
#' @keywords internal
guaranteed_breaks <- function(breaks, center, origin = NULL) {
  if (length(breaks) == 0L || is.na(breaks[1])) return(breaks)
  if (!is.null(origin)) return(breaks - breaks[1] + origin)
  c(center, utils::head(breaks, -1L))
}

# the additive (center-0) break scale for a NON-gaussian regression coefficient on the LINK scale,
# derived by LOGGING the odds_ratio scale and rounding to 1 dp (log c(1.2,1.5,2,4) -> c(0.2,0.4,0.7,1.4)),
# so a log-odds/log-rate coefficient reads ~the same intensity as its exponentiated OR/IRR twin and
# follows any user change to `odds_ratio`. `std = FALSE` (no var(Y) on the link scale).
#' @keywords internal
log_odds_scale <- function(or_scale) {
  log_side <- function(side) list(breaks = round(log(side$breaks), 1L), slots = side$slots)
  list(center = 0, strict = TRUE, std = FALSE,
       over = log_side(or_scale$over), under = log_side(or_scale$under))
}

# a break-scale KEY -> the scale in force. A settable scale reads from the live breaks list; a DERIVED
# one (COLOR_SCALES$<k>$derive) is built from its parent, so it follows any user change to that parent.
#' @keywords internal
color_scale_resolve <- function(key, scales) {
  d <- COLOR_SCALES[[key]]$derive
  if (is.null(d)) return(scales[[key]])
  switch(d$how, "log" = log_odds_scale(scales[[d$from]]), scales[[d$from]])
}

# WHICH break scale a gap measure (`adjustment` / `between_groups`) reads: the ESTIMATE's own scale,
# one lookup on EST_SCALES (the gap of an effect belongs on the same KIND of ladder as the effect).
#' @keywords internal
fmt_gap_scale_key <- function(x) EST_SCALES[[fmt_scale_key(x)]]$gap_key


#' @keywords internal
fmt_color_plan <- function(x, channel = c("text", "bg"), color = NULL, signif = NULL) {
  channel <- match.arg(channel)
  n    <- length(x)
  scl  <- fmt_scale_row(x)
  # `channel` selects the SLOT TABLE / palette family (text vs bg spread intensities differently). The
  # MEASURE is the `color` arg when given, else the text-channel measure.
  if (is.null(color)) color <- get_color(x)
  if (length(color) == 0L || is.na(color[1]) || color[1] %in% c("", "no")) return(NULL)

  # the stored `color` attribute is a CLEAN measure (legacy combined strings decoded earlier by
  # color_decode_legacy; acronym synonyms resolve through the ONE table). A non-measure token -> uncoloured.
  measure <- measure_key(color[1])
  if (is.na(measure) || !nzchar(measure)) return(NULL)
  # policy: an explicit `signif` arg wins, else the stored color_signif. measure_policy() then applies
  # the measure's `force_policy` -- which may be a PREDICATE on the column (a gap measure has a test
  # exactly where a `gap_se` was written), so the column is passed in. The legend reads plan$policy back.
  policy  <- measure_policy(measure, if (!is.null(signif)) signif else get_color_signif(x), x)

  # the measure's engine facts (scale keys, raw getter, sig source, row gate) live in ONE MEASURES row.
  # Read through measure_facts(), which folds in a per-policy override (only contrib has one:
  # `guaranteed_effect` swaps the relative contribution for the absolute residual).
  md      <- measure_facts(measure, policy)
  ci_mult <- isTRUE(scl$mult)          # the stored interval's geometry (neutral 1 vs neutral 0)
  sc      <- color_scales()
  # the selected scale as a KEY, kept on the plan (the legend takes its glyphs/unit from the scale
  # actually used). WHICH of a measure's three ladders a column reads is the column's declared `ladder`
  # ("pct"|"std"|"log"); the two gap measures dispatch on the ESTIMATE's own scale (`scale_from = "gap"`).
  scale_key <- if (identical(md$scale_from, "gap")) fmt_gap_scale_key(x)
               else md$scale[[scl$ladder]]
  # a DERIVED scale (log_odds, adj_diff_log) declares its parent and derivation in COLOR_SCALES, so a
  # user's set_color_breaks(odds_ratio =) / (adj_ratio =) reaches it.
  scale   <- color_scale_resolve(scale_key, sc)
  md      <- measure_facts(measure, policy, scale_key)   # re-resolve with the per-scale override
  center <- if (is.null(scale)) 0 else scale$center
  strict <- if (is.null(scale)) TRUE else scale$strict

  raw <- md$raw(x)

  # Standardized by SD(Y) when the SCALE says so (Glass's delta for a numeric `diff`, the additive gap
  # of an arbitrary-unit outcome). WHERE the SD comes from is the scale's declared `sd_from` -- a
  # regression column's stored var(Y), a crosstab cell's REFERENCE variance -- the same entry
  # fmt_scale_of() reads for the plot's SD axis, so the table and plot standardize by the same number.
  sd_ref <- NULL
  if (isTRUE(scale$std)) {
    sd_ref      <- if (identical(scl$sd_from, "var")) sqrt(get_var(x)) else sqrt(get_ref_var(x))
    raw         <- raw / sd_ref
    raw[!is.finite(raw)] <- NA_real_        # sd_ref 0/NA -> undefined -> uncolored
  }

  # Significance is a property of the INTERVAL, not of the measure being coloured -- an interval is
  # significant when it excludes ITS OWN neutral (0 for additive diff* scales, 1 for multiplicative
  # or/ratio). All three scales test the same null (p1 = p2), so whichever interval is stored answers it.
  # WHICH interval is a declared measure fact (`bounds`, defaulted to the stored ci_inf/ci_sup). The gap
  # measures derive theirs from `gap_se`, already folded onto the score's scale and sign -- so this
  # block, the floor block and the grey_non_signif direction all work with no measure-specific branch.
  bd          <- md$bounds(x)
  # "does this column carry an interval the significance gate can test" IS the scale's declared `kind`:
  # an "effect" scale has a null to test against, a "level" one does not.
  # WARNING: a one-proportion cell interval is centred on the cell itself and has NO reference null, so
  # it must never reach this gate -- that is exactly what `kind == "level"` says.
  has_ci      <- identical(scl$kind, "effect")
  ci_neutral  <- if (is.na(scl$neutral)) 0 else scl$neutral
  sig_pos <- has_ci & bd$lo > ci_neutral
  sig_neg <- has_ci & bd$hi < ci_neutral
  sig_pos[is.na(sig_pos)] <- FALSE
  sig_neg[is.na(sig_neg)] <- FALSE

  # `contrib` carries NO confidence interval, so its significance is read from the stored
  # standardized-residual p-value rather than the bounds; direction from the sign of `raw`. (The one
  # place colour reads `pvalue` -- justified because contrib has no interval.)
  if (md$sig_source == "pvalue") {
    alpha   <- 1 - get_conf_level(x)
    pv      <- get_pvalue(x)
    ctr_sig <- !is_totrow(x) & !is.na(pv) & pv < alpha
    sig_pos <- ctr_sig & raw > 0; sig_pos[is.na(sig_pos)] <- FALSE
    sig_neg <- ctr_sig & raw < 0; sig_neg[is.na(sig_neg)] <- FALSE
  }

  if (policy == "guaranteed_effect") {
    if (md$sig_source == "pvalue") {
      # no interval to take a CI-floor of, so this policy carries contrib's OTHER reading -- the
      # ADJUSTED STANDARDIZED RESIDUAL on the absolute |z| scale (the MEASURES `guar` override). The
      # break scale is re-anchored to the significance threshold, so a cell is coloured iff
      # |z| > z(conf_level) while the thresholds stay numbers a reader can name.
      score <- fmt_resid(x)
      gate  <- sig_pos | sig_neg
    } else {
    # The GUARANTEED (CI-floor) magnitude, on the MEASURE'S OWN scale so fmt_color_slots() folds it
    # around the right centre. Only one interval is stored per column (the primary measure's), so the
    # second channel derives from it.
    floor_q <- dplyr::case_when(sig_pos ~ bd$lo,
                                sig_neg ~ bd$hi,
                                TRUE    ~ NA_real_)
    # `diff` and `ratio` are two views of ONE cell-vs-reference comparison (both affine in the cell
    # proportion with the reference at its point estimate), so a bound on either maps onto the other by
    # one ratio of offsets from their neutrals -- no new field.
    rescale_bound <- function(q, pt_from, nt_from, pt_to, nt_to)
      nt_to + (pt_to - nt_to) * (q - nt_from) / (pt_from - nt_from)
    # keep the finite-scrub scoped to the conversions (a 0/0 there gives NaN).
    ci_is_ratio <- identical(scl$est_field, "ratio")
    if (measure == "ratio" && !ci_is_ratio) {          # diff bound -> ratio bound
      floor_q <- rescale_bound(floor_q, get_diff(x),  0, get_ratio(x), 1)
      floor_q[!is.finite(floor_q)] <- NA_real_
    } else if (measure == "difference" && ci_is_ratio) {  # ratio bound -> diff bound (the mirror)
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

  # a reference row is a baseline, not an effect -> never coloured (uncolours a regression INTERCEPT,
  # which is in_refrow but has a non-neutral value).
  if (md$gate_row == "refrow") gate <- gate & !is_refrow(x)

  # Per-direction breaks + palette slots: each side carries its own magnitudes (over/under $breaks) and
  # intensities 1:4. The engine folds each cell to a magnitude >= the neutral, findInterval() against
  # the side's breaks: over -> slots 1:4, under -> slots 5:8.
  over_breaks  <- scale$over$breaks
  under_breaks <- scale$under$breaks
  over_slots   <- c(0L, scale$over$slots)         # 0 = neutral level, then intensities 1:4
  under_slots  <- c(0L, scale$under$slots + 4L)   # 0 = neutral, then 5:8 (under half of the palette)

  # under `guaranteed_effect` the score is the CI FLOOR (the effect you are confident of AT LEAST), so
  # the ladder must START at the neutral: a cell whose interval excludes the neutral IS a guaranteed
  # effect and must be coloured -- which is what the policy is for. See guaranteed_breaks().
  if (identical(policy, "guaranteed_effect")) {
    # `break_origin` is a declared measure fact (only contrib's `guar` sets it): its ladder is written
    # in confidence levels, so it re-anchors at z(conf_level) instead of taking the rung shift.
    org <- if (identical(md$break_origin, "threshold")) {
      zscore_formula(get_conf_level(x))
    } else NULL
    over_breaks  <- guaranteed_breaks(over_breaks,  center, org)
    under_breaks <- guaranteed_breaks(under_breaks, center, org)
  }

  # THE BACKGROUND IS A COARSER VOICE. A fill is read at a glance and sits behind a number the text
  # channel already grades, so a ladder may declare (COLOR_SCALES$bg_keep) how many of its LOUD rungs
  # survive there -- the ratio scales keep two, because their faint rungs only restate the difference
  # channel beside them. The slots come along unchanged, so a fill means what its shade always meant.
  # WARNING: after the guaranteed_effect shift, never before -- trimming first would leave the
  # prepended neutral as the background's own faintest rung, colouring every significant cell.
  keep <- COLOR_SCALES[[scale_key]]$bg_keep
  if (identical(channel, "bg") && !is.null(keep)) {
    trim <- function(b, sl) {
      if (length(b) <= keep) return(list(b, sl))
      i <- seq.int(length(b) - keep + 1L, length(b))
      list(b[i], c(0L, sl[-1L][i]))
    }
    o <- trim(over_breaks,  over_slots);  over_breaks  <- o[[1]]; over_slots  <- o[[2]]
    u <- trim(under_breaks, under_slots); under_breaks <- u[[1]]; under_slots <- u[[2]]
  }

  # a `guaranteed_effect` channel with a single break per side collapses to a flat "x1" fill (no
  # gradient). Flag it so the cross-channel arbiter can disable it -- unless it is the only channel.
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
  # a "blank" cell (n_min mask) and a "gof" cell (model-fit stat) show no value, so no colour either:
  # a large AIC in the `diff` field would otherwise score to the strongest slot.
  disp0 <- display_primary(get_display(x))
  # DISPLAY_TOKENS' `colour` column (NOT `footer`: `pvalue` is a footer token that IS coloured below).
  slot[disp0 %in% DISPLAY_NO_COLOR] <- 0L
  # a "pvalue" test cell colours as a SIGNIFICANCE WARNING, not a data effect: a non-significant test
  # (p > alpha) gets the deepest under-slot (deep red), a significant one stays uncoloured. Reads the
  # honest `pvalue` field, scoped to the additive `diff` channel (the crosstab default).
  # ...and a MODEL CHECK past the convention its REG_CHECKS row declares takes the FAINTEST under
  # slot: a rule of thumb earns "look at this", never the verdict's deep red.
  # ⚠ `under_slots` carries a leading 0 (the below-the-first-break slot), so the FAINTEST shade is
  # the smallest POSITIVE one, never min().
  is_wn <- disp0 == "gof_warn"
  faint <- plan$under_slots[plan$under_slots > 0L]
  if (any(is_wn) && length(faint)) slot[is_wn] <- min(faint)
  is_pv <- disp0 == "pvalue"
  if (any(is_pv) && identical(plan$measure, "difference")) {
    alpha  <- 1 - get_conf_level(x)
    pv     <- get_pvalue(x)
    slot[is_pv] <- 0L                                    # significant -> uncoloured
    slot[is_pv & !is.na(pv) & pv > alpha] <- max(plan$under_slots)   # non-significant -> deep-red warning
  }
  slot
}

# resolve_color_channel_plans() -- builds the text + background plans AND applies the cross-channel
# arbitration fmt_color_plan() (per-channel) cannot see: under `guaranteed_effect` a channel whose
# scale is a single break per side is degenerate; drop it, but NEVER the last channel. Shared by the
# cells (fmt_color_channels) and the legend (legend_specs) so they cannot disagree.
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
# Returns per cell for BOTH channels: the colour code (NA where uncoloured), the raw slot vectors, and
# the per-cell FACE (bold / italic / underline). Text channel uses the "text" palette, background the
# "bg" palette -- mirrors pillar_shaft's two-channel logic so console and exports match.
# DESIGN: the face is the palette's answer to "how is this slot drawn", the twin of the hex -- it is
# what lets a monochrome palette exist (a black-text palette cannot infer "bold" from "has a hex").
# THE ENGINE STAYS THEME-BLIND: slots are computed without knowing the theme; only this boundary turns
# a slot into an appearance.
#' @keywords internal
fmt_channel_codes <- function(x, theme = "light", ink = "text") {
  n  <- length(x)
  ch <- fmt_color_channels(x)

  # `ink` names the family the TEXT channel takes its hex from -- "text" everywhere except a graphics
  # device, which cannot draw a rule (tx_plot_ink_family). The FACE is always the text family's: a
  # substitution changes what a cell is coloured with, never what the palette says about the slot.
  text_styles <- get_color_style("color_code", type = ink, theme = theme)
  bg_styles   <- get_color_style("color_code", type = "bg", theme = theme)

  text <- rep(NA_character_, n)
  bg   <- rep(NA_character_, n)
  tsel <- ch$text_slot > 0L
  bsel <- ch$bg_slot   > 0L
  # historical output is upper-case hex (cf. fmt_get_color_code).
  text[tsel] <- toupper(unname(text_styles[ch$text_slot[tsel]]))
  bg[bsel]   <- toupper(unname(bg_styles[ch$bg_slot[bsel]]))

  slot_face <- function(slot, type) {
    f   <- get_color_style("face", type = type, theme = theme)
    sel <- slot > 0L
    # `underline` is "" / "single" / "double", not a logical -- see print_palette().
    out <- list(bold = logical(n), italic = logical(n), underline = character(n))
    out$underline[] <- ""
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




# === SECTION: colour legend ==============================================================
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
#
# THE PROSE GRAMMAR, one shape for every case (see legend_tokens_prose):
#   [<col names> -- ]<HEAD><LADDER> <NOTE>
#     HEAD    the measure NAMED IN WORDS ("Percentage points (risk) difference:"), which is what a
#             reader needs first. Dropped where the subject already IS the measure (a regression's
#             effect word) or where the measure writes its own lead (the gap measures). Under
#             `guaranteed_effect` it carries the guarantee and names the interval ONCE, both channels.
#     LADDER  per side "<subject> >= <ref> <breaks> <unit>", the two sides joined by ";" -- ONE
#             sentence. Under `guaranteed_effect` they merge into one list after "from <ref>".
#     NOTE    what an UNCOLOURED (or, on a publication palette, UNMARKED) cell means. Only that:
#             "coloured => significant" is a tautology the cells already show.
# THE MEASURE IS NAMED IN TWO REGISTERS, both facts on the MEASURES row: `word` (short) for the
# console and a plot guide, `word_long` (per SCALE, via by_scale) for the export footers -- a
# difference of proportions, of means and of log odds are three quantities, not one word.

# fixed (non-translated) symbols, kept as \uXXXX so R source stays ASCII.
.lg_ge    <- "\u2265"   # >=
.lg_le    <- "\u2264"   # <=
.lg_times <- "\u00d7"   # x  (times)
.lg_div   <- "\u00f7"   # /  (division)

# The per-measure fact table -- every language-invariant fact of a colour measure in ONE place: its
# vocabulary (name / where it may go / who may ask / what it needs / when it is automatic), its legend
# words, and its engine facts. Adding a measure is ONE row; a per-measure divergence is a FIELD, not a
# switch arm. ONE row drives both the colour PLAN (fmt_color_plan) and the legend, so they cannot diverge.
# WARNING: every reader goes through an accessor (measure_facts / measure_policy / measure_key /
# measure_legal / measure_auto / measure_requires / measure_builds). A bare MEASURES[[m]]$field read
# outside them is the drift this table exists to prevent.
# Vocabulary fields:
#   channels     which colour channels this measure may ride: "text" and/or "bg". A whole-cell measure
#                (or / contrib) is text-only. THE single eligibility list (storage + argument boundaries
#                are two views of it).
#   producers    which producer can BUILD it: "tab" (crosstab core) and/or "reg" (tab_reg). Separates
#                "illegal" from "not available here": `adjustment` scores `obs`, which only a regression
#                fills, so tab() refuses it.
#   color_arg    whose `color =` argument may NAME it -- a DIFFERENT question from `producers`, and the
#                one the scope refusals ask. tab_reg() BUILDS `difference`/`ratio`/`odds_ratio` (its
#                columns are coloured on them) yet its `color` names none of them: there the estimand
#                is `measure`'s job, and what is left to choose is what to compare it TO. Read only
#                through measure_nameable().
#   applies_to   which column kinds it can colour: "pct" (a percentage) and/or "num" (mean/count/coef).
#   builds       WHICH per-cell fields the pipeline must compute: "diff" (diff + ratio, together), "or"
#                (the odds ratio + its Woolf interval) or "contrib" (the chi2 contributions). Two measures
#                sharing a `builds` class are a pure re-paint of each other (the jamovi "arming class").
#   requires     what asking for it FORCES on the build, a named vector of "always" or "gated" ("only when
#                a significance policy is in force"). Keys: ref / ci / chi2 / totrow / empirical / interaction.
#   ref_auto     the reference this measure picks when the user leaves `ref = "auto"`.
#   auto_for     the contexts in which it IS `color = TRUE`'s answer, per channel: list(text =, bg =).
#                Contexts named by what the COLUMN or model is -- "pct", "counts", "num", "reg_diff", "reg_ratio".
#   method       how the legend names this measure's TEST when it is not the column's stored interval:
#                NA = no interval (contrib), a closure = its own sentence (the gap measures). Absent =
#                the column's `ci_method` names it.
#   caveat       optional closure(spec) -> one sentence of honesty, or NULL.
# Legend fields:
#   word               the SHORT measure word, as a CLOSURE so gettext() runs at RENDER (a top-level
#                      gettext() would freeze the build locale; potools extracts the literal
#                      statically). A non-translated word is function() "OR". Used by the console.
#   word_long          the measure NAMED IN WORDS, for the export footers -- the discipline's term and
#                      the base measure together ("percentage points (risk) difference"). Mostly a
#                      `by_scale` fact, because what a difference IS depends on the ladder it is read
#                      on; row-level where it does not vary. `word_std` / `word_long_std` are the SD
#                      twins, so the standardized reading is a field rather than a branch. Absent =
#                      fall back leftwards (word_long_std -> word_long -> word_std -> word).
#   word_guar          closure(conf_pct) -> the `guaranteed_effect` head, as ONE msgid per measure
#                      (`word_guar_std` its SD twin). ⚠ NOT a shared "%s-guaranteed %s" template: in
#                      French the participle agrees with the measure (*differance garantie* vs
#                      *rapport garanti*), which no single format string can do. Absent = that
#                      generic template, which is right wherever no catalogue needs agreement.
#   subject            the legend's noun for what is GRADED, when that is not the cell itself. A
#                      column may be coloured on a quantity it does not print -- an odds ratio behind
#                      a percentage -- and "cell >= 1.2" would then be false.
#   break_over/under   the break-label glyph per side; break_scale = TRUE means a factor pct diff x100.
#   ref_kind           the baseline concept: "category" | "indep" (independence) | NA = the column's own ref.
#   threshold_mult     the grey-note first-break glyph is x (TRUE) or the symmetric +/- (FALSE).
#   unit_kind          the prose unit suffix: "diff" | "contrib" | "none".
#   has_ref_lead       the effect is stated vs a reference in the sentence LEAD (diff/ratio) rather than
#                      already relative to it (or/contrib/reg effect).
#   lead               optional closure(subject, ref, dir) -> the whole sentence lead, for a measure the
#                      default "<subject> >= <reference>" mis-states. Absent = that default.
# Engine fields (read by fmt_color_plan):
#   raw                a getter closure(x) -> the observed per-cell quantity scored + coloured.
#   scale              named c(pct=, std=, log=) of color_scales() keys, ONE PER LADDER; the COLUMN says
#                      which it reads (EST_SCALES$ladder), so a measure never asks what column it is on.
#   scale_from         (optional) "gap": scores a GAP, so its ladder comes from the ESTIMATE's own scale
#                      (fmt_gap_scale_key), not `scale`. Absent = read `scale`.
#   sig_source         "bounds" (an interval, via `bounds`) | "pvalue" (contrib -- reads the stored
#                      standardized-residual p-value).
#   bounds             (optional) closure(x) -> list(lo, hi), THE interval the significance policies read.
#                      Absent = the stored ci_inf/ci_sup; the gap measures derive theirs from `gap_se`.
#   gate_row           which structural row it never colours: "refrow" (a baseline) | "totrow" (contrib).
#   force_policy       (optional) the measure has no significance test of its own, so it ALWAYS reads
#                      under this policy (may be a predicate on the column).
#   by_scale           (optional) presentation facts belonging to a SCALE rather than the measure, for
#                      measures whose scale is chosen at runtime; folded in by measure_facts().

# The presentation of a gap measure's ADDITIVE scales, shared by both gap measures: a "+"/"-" ladder
# around 0, and the unit the legend names (points on a probability-scale ME, SD on a standardized one,
# nothing on a log coefficient). `break_scale = TRUE` renders the probability ladder x100.

# fmt_gap_lead() -- the legend lead of the two GAP measures. The generic "<subject> >= <reference>"
# states a SIGNED move (is the model estimate higher than the crude one?), which is not what
# fmt_adjustment_score() grades: it compares DISTANCES FROM THE NULL, so on a protective effect the
# two disagree outright -- a crude 0.92 adjusted to 0.74 is a strengthening, and the generic lead
# called it a fall. Declared per measure so the sentence and the shade cannot drift apart again.
#' @keywords internal
fmt_gap_lead <- function(subject, ref, dir, neutral = NA_character_) {
  # name the null the distance is measured from (1 on a ratio, 0 on a difference): "no effect" is the
  # concept, the number is what the reader sees in the column. EST_SCALES$neutral, so it cannot drift.
  # ⚠ the UNDER side also holds a SIGN FLIP: an estimate that crossed the null is "closer" to it in
  # this measure's arithmetic (it compares DISTANCES), so the sentence has to admit that reading.
  if (is.na(neutral)) {
    if (dir > 0) gettextf("%s further from no effect than %s, by", subject, ref)
    else         gettextf("%s closer to no effect than %s (or inversed effect), by", subject, ref)
  } else {
    if (dir > 0) gettextf("%s further from no effect (%s) than %s, by", subject, neutral, ref)
    else         gettextf("%s closer to no effect (%s) than %s (or inversed effect), by", subject, neutral, ref)
  }
}

# fmt_contrib_lead() -- the legend lead of `contrib`. The contribution to chi2 is SIGNED, and the
# generic "<subject> >= <breaks>" said the same thing on both sides (the glyph is x on each), leaving
# the sign to the colour alone. Positive means the cell holds MORE cases than independence predicts;
# the discipline's words for that are over- / under-represented, which is also what the French
# glossary settled. The reference is named on the FIRST side only (legend_tokens_prose's
# first-then-short rule gives `indep` an empty short form), so it is stated once.
#' @keywords internal
fmt_contrib_lead <- function(subject, ref, dir, neutral = NA_character_) {
  if (is.null(ref) || is.na(ref) || !nzchar(ref)) {
    if (dir > 0) gettextf("%s over-represented, by",  subject)
    else         gettextf("%s under-represented, by", subject)
  } else {
    if (dir > 0) gettextf("%s over-represented vs %s, by",  subject, ref)
    else         gettextf("%s under-represented vs %s, by", subject, ref)
  }
}

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
  difference = list(word = function() gettext("difference"),           break_over = "+",       break_under = "-",
                 doc = "cell difference from the reference (percentage points for factors; the standardized difference Glass's \\eqn{\\Delta} for numeric means).",
                 break_scale = TRUE,  ref_kind = NA_character_, threshold_mult = FALSE, unit_kind = "diff",
                 has_ref_lead = TRUE,
                 channels = c("text", "bg"), producers = c("tab", "reg"), color_arg = "tab",
                 applies_to = c("pct", "num"), builds = "diff",
                 requires = c(ref = "always", ci = "gated"),
                 auto_for = list(text = c("pct", "reg_diff")),
                 raw = function(x) get_diff(x),
                 scale = c(pct = "pct_diff", std = "mean_diff", log = "log_odds"),
                 sig_source = "bounds", gate_row = "refrow",
                 # WHAT a difference is depends on the ladder, so the export name is a per-scale fact:
                 # a difference of proportions, of means, or of log odds are three quantities.
                 by_scale = list(
                   pct_diff  = list(word_long = function() gettext("percentage points (risk) difference"),
                                    word_guar = function(p) gettextf("%s%%-guaranteed percentage points (risk) difference", p)),
                   mean_diff = list(word_long     = function() gettext("mean difference"),
                                    word_std      = function() gettext("standardized difference"),
                                    word_long_std = function() gettext("standardized mean difference"),
                                    word_guar     = function(p) gettextf("%s%%-guaranteed mean difference", p),
                                    word_guar_std = function(p) gettextf("%s%%-guaranteed standardized mean difference", p)),
                   log_odds  = list(word_long = function() gettext("log-odds difference"),
                                    word_guar = function(p) gettextf("%s%%-guaranteed log-odds difference", p)))),
  ratio      = list(word = function() gettext("ratio"),                break_over = .lg_times, break_under = .lg_div,
                 doc = "relative risk (factors) or mean ratio (numerics) vs the reference.",
                 break_scale = FALSE, ref_kind = NA_character_, threshold_mult = TRUE,  unit_kind = "none",
                 has_ref_lead = TRUE,
                 # `ratio` shares `diff`'s build class: the leaf computes both fields in one pass (a
                 # diff <-> ratio toggle never rebuilds -- jamovi tier-3 re-paint).
                 channels = c("text", "bg"), producers = c("tab", "reg"), color_arg = "tab",
                 applies_to = c("pct", "num"), builds = "diff",
                 requires = c(ref = "always", ci = "gated"),
                 auto_for = list(text = "num", bg = "pct"),
                 raw = function(x) get_ratio(x),
                 scale = c(pct = "pct_ratio", std = "mean_ratio", log = "mean_ratio"),
                 sig_source = "bounds", gate_row = "refrow",
                 by_scale = list(
                   pct_ratio  = list(word_long = function() gettext("relative risk (ratio)"),
                                     word_guar = function(p) gettextf("%s%%-guaranteed relative risk (ratio)", p)),
                   mean_ratio = list(word_long = function() gettext("ratio of means"),
                                     word_guar = function(p) gettextf("%s%%-guaranteed ratio of means", p)))),
  odds_ratio = list(word = function() "OR", word_long = function() gettext("odds ratio"),
                 word_guar = function(p) gettextf("%s%%-guaranteed odds ratio", p),
                 # the graded quantity is NOT the cell: `color = "or"` colours a percentage table on
                 # its odds ratios, so "cell >= 1.2" would compare a percentage to an odds ratio.
                 subject = "OR",
                 break_over = "",        break_under = "1/",
                 doc = "the empirical odds ratio (for \\code{pct = \"row\"}/\\code{\"col\"}), coloured on its own symmetric \\code{odds_ratio} scale (so \\code{pct_ratio} stays free for \\code{\"ratio\"}).",
                 break_scale = FALSE, ref_kind = "category",    threshold_mult = TRUE,  unit_kind = "none",
                 has_ref_lead = FALSE,
                 # text-only (a whole-cell measure) and percentages only (a mean has no odds). Its
                 # baseline is the FIRST level, not the total row. "gated" here means the Woolf interval
                 # (the interval of THIS comparison), tested against the odds-ratio neutral.
                 channels = "text", producers = c("tab", "reg"), color_arg = "tab",
                 applies_to = "pct", builds = "or", ref_auto = "first",
                 requires = c(ref = "always", ci = "gated"),
                 # ⚠ tab() NEVER auto-resolves to the odds ratio (it is asked for by name) -> reg-only context.
                 auto_for = list(text = "reg_ratio"),
                 raw = function(x) get_or(x),
                 scale = c(pct = "odds_ratio", std = "odds_ratio", log = "odds_ratio"),
                 sig_source = "bounds", gate_row = "refrow"),
  contrib    = list(word = function() gettext("contribution to Chi2"), break_over = .lg_times, break_under = .lg_times,
                 doc = "signed contribution to the chi-squared (reference-free).",
                 break_scale = FALSE, ref_kind = "indep",       threshold_mult = TRUE,  unit_kind = "contrib",
                 has_ref_lead = FALSE, lead = fmt_contrib_lead,
                 # the ONE measure the test step computes and stamps: the signed chi2 residual needs the
                 # whole table and stores each cell's mean contribution ON the total row (both forced).
                 # `method = NA` = no interval to name.
                 channels = "text", producers = "tab", color_arg = "tab",
                 applies_to = "pct", builds = "contrib",
                 requires = c(chi2 = "always", totrow = "always"),
                 auto_for = list(text = "counts"),
                 method = NA,
                 raw = function(x) dplyr::if_else(is_totrow(x), NA_real_, get_ctr(x) / get_mean_contrib(x)),
                 scale = c(pct = "contrib", std = "contrib", log = "contrib"),
                 sig_source = "pvalue", gate_row = "totrow",
                 # contrib is the ONE measure whose reading changes with the significance policy, so the
                 # divergence is a FIELD (a `guar` override), never a switch arm. `ignore` /
                 # `grey_non_signif` colour the RELATIVE contribution (a share of this table's chi2 -- the
                 # correspondence-analysis reading); `guaranteed_effect` colours the ADJUSTED STANDARDIZED
                 # RESIDUAL on the absolute |z| scale (the SPSS reading). Both share ONE significance
                 # source: the residual p-value. `guar` keeps only what depends on the POLICY; the glyphs
                 # follow from the scale it swaps to (`by_scale$zscore`).
                 guar = list(word = function() gettext("standardized residual"),
                             break_origin = "threshold",
                             scale = c(pct = "zscore", std = "zscore", log = "zscore")),
                 by_scale = list(zscore = list(break_over = "+", break_under = "-",
                                               threshold_mult = FALSE, unit_kind = "none"))),
  # the two tab_reg-only measures. They score the SAME quantity through the SAME helper (how far the
  # model estimate sits from `obs`) and differ ONLY in what `obs` is, hence in the reference the legend
  # names. `scale_from = "gap"` takes the ladder from the ESTIMATE's own scale: an OR/RR/IRR folds around
  # 1 on `adj_ratio`, a beta/AME/risk-difference around 0 on `adj_diff`. Both derive their interval from
  # the stored `gap_se`, so both read `color_signif` normally -- WHERE tab_reg could write one; where it
  # could not, `force_policy` (fmt_gap_force_policy) makes them read under `ignore`. The two SEs come
  # from different mathematics: `between_groups` compares DISJOINT groups (quadrature on the printed
  # intervals is exact); `adjustment` compares two estimates on the SAME rows, so its SE needs the
  # difference of their influence functions (R/reg-influence.R). Both are `producers = "reg"` (they
  # score `obs`, which only a regression fills); `by_scale` overrides the multiplicative presentation
  # on each ADDITIVE scale.
  adjustment = list(word = function() gettext("adjustment"),    break_over = .lg_times, break_under = .lg_div,
                 doc = "how far each \\strong{modelled} effect sits from its \\strong{observed} (crude, unadjusted) counterpart -- what adjusting for the other predictors did to it. Turns \\code{empirical = TRUE} on. Meant for the \\emph{background} channel.",
                 break_scale = FALSE, ref_kind = "observed",     threshold_mult = TRUE,  unit_kind = "none",
                 has_ref_lead = TRUE, lead = fmt_gap_lead,
                 channels = c("text", "bg"), producers = "reg", color_arg = "reg",
                 applies_to = c("pct", "num"), builds = "diff",
                 requires = c(empirical = "always"),
                 method = function() gettext("z test on the difference between two estimates fitted on the same sample"),
                 caveat = function(spec) fmt_noncollapsible_caveat(spec),
                 raw = function(x) fmt_adjustment_score(x), scale_from = "gap",
                 sig_source = "bounds", bounds = fmt_gap_bounds,
                 gate_row = "refrow", force_policy = fmt_gap_force_policy,
                 by_scale = GAP_ADDITIVE_FACTS),
  between_groups = list(word = function() gettext("between groups"), break_over = .lg_times, break_under = .lg_div,
                 doc = "with \\code{tab_vars}, how far each group's effect sits from the \\strong{first} group's, on the same row: a per-predictor reading of effect modification. Meant for the \\emph{background} channel.",
                 break_scale = FALSE, ref_kind = "group",        threshold_mult = TRUE,  unit_kind = "none",
                 has_ref_lead = TRUE, lead = fmt_gap_lead,
                 channels = c("text", "bg"), producers = "reg", color_arg = "reg",
                 applies_to = c("pct", "num"), builds = "diff",
                 requires = c(interaction = "always"),
                 method = function() gettext("z test on the difference between two independent estimates"),
                 raw = function(x) fmt_adjustment_score(x), scale_from = "gap",
                 sig_source = "bounds", bounds = fmt_gap_bounds,
                 gate_row = "refrow", force_policy = fmt_gap_force_policy,
                 by_scale = GAP_ADDITIVE_FACTS)
)

# the table must be COMPLETE on the vocabulary fields, or an accessor would silently answer "no channel"
# for a forgotten row. Build-time, like fmt_attr_rules' exhaustiveness check.
stopifnot(all(vapply(MEASURES, function(m)
  all(c("channels", "producers", "color_arg", "applies_to", "builds", "doc") %in% names(m)),
  logical(1))))

# WHO a measure belongs to, the value set of both scope columns.
#' @keywords internal
MEASURE_PRODUCERS <- c("tab", "reg")
# ...and the user-facing function each names, so a refusal names the door instead of hardcoding it.
#' @keywords internal
MEASURE_PRODUCER_FN <- c(tab = "tab", reg = "tab_reg")

# `adjustment` on an ODDS RATIO needs one sentence of honesty: the odds ratio is NON-COLLAPSIBLE --
# adjusting for a covariate that predicts the outcome moves it away from 1 even with zero confounding,
# the same order of magnitude as the 10 % first break. Collapsible estimands (AME, RR, IRR, beta) are
# exempt, which is what the caveat says. `is_coef` covers a raw logit coefficient (the same quantity, logged).
# WARNING: reads `reg_fam_prob()` off COLUMN facts (the legend cannot see `effect`); keep set-identical
# to reg_estimand_collapsible(), which states it from the build side.
#' @keywords internal
fmt_noncollapsible_caveat <- function(spec) {
  if (!isTRUE(reg_fam_prob(spec$model_family))) return(NULL)
  if (!(isTRUE(spec$is_coef) || isTRUE(reg_word_noncollapsible(spec$eff_word)))) return(NULL)
  gettext("Part of an odds-ratio gap is non-collapsibility, not confounding: a risk ratio or a marginal effect is the collapsible comparison.")
}

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
  # the SELECTED scale's presentation override, applied AFTER `guar` (which may SWAP the scale). A
  # measure with no `by_scale`, or a scale with no entry, is untouched.
  if (!is.null(scale_key) && !is.null(md$by_scale[[scale_key]]))
    md <- utils::modifyList(md, md$by_scale[[scale_key]])
  if (is.null(md$bounds)) md$bounds <- fmt_stored_bounds
  md
}

# The policy a measure ACTUALLY reads under: the column's `color_signif`, unless the measure declares a
# `force_policy` (a measure with no significance test of its own). The twin of measure_facts() -- the
# plan and the legend both resolve the policy here, so a neutralised measure cannot be coloured under
# one policy while the legend describes another.
# does this measure's baseline live in ANOTHER COLUMN (the observed effect, a reference group) rather
# than a row of its own? Two consequences: such a measure NAMES ITSELF in the legend (the column's
# effect word is the thing compared), and it resolves its own reference phrase per channel.
#' @keywords internal
measure_own_ref <- function(measure) isTRUE(MEASURES[[measure]]$ref_kind %in% c("observed", "group"))

# WHICH measures a producer's `color =` argument may NAME -- the declared `color_arg`, a different
# question from `producers` (which says who can BUILD one; see the field block above). ONE reader for
# the two scope refusals and for the generated @param list, so the rule is stated once.
# `channel` narrows it to the measures eligible on that channel: `?tab`'s value list is the TEXT one.
#' @keywords internal
measure_nameable <- function(producer, channel = NULL) {
  k <- names(MEASURES)[vapply(MEASURES, function(m) identical(m$color_arg, producer), logical(1))]
  if (is.null(channel)) k else
    k[vapply(k, function(z) channel %in% MEASURES[[z]]$channels, logical(1))]
}

# ---- the vocabulary accessors ---------------------------------------------
# Each is the ONLY reader of its fact. Nothing outside this block indexes MEASURES by a literal measure
# name, which is what makes "adding a measure is one row" true.

# THE ACRONYM VOCABULARY -- the discipline's short names, ONE table for every argument that names a
# measure: `tab(color =)`, `fmt(color =)`, `tab_reg(measure =)`, `tab_reg(link =)`. It IS the REG_WORDS
# set -- what a header can print is what an argument can be typed -- and a foreign key checks that at
# load, in both directions. The acronyms are permanent aliases, never deprecated: the argument teaches
# the CONCEPT word ("ratio"), the header keeps the discipline's ("RR" / "IRR" / "RoM").
#
# A mismatched acronym is a REQUEST, and the header is the answer: `measure = "IRR"` on a gaussian
# outcome resolves to the outcome's own word (`Model_mRoM`), and a crosstab's legend prints the concept
# word, so nothing can mislabel itself. That is why the table is permissive and needs no message.
#' @keywords internal
MEASURE_ACRONYMS <- c(RD = "difference", diff = "difference",
                      RR = "ratio", IRR = "ratio", RoM = "ratio",
                      OR = "odds_ratio")

# ⚠ REGRESSION-ONLY: the words a RANK level names (REG_LEVEL_MEASURES$rank) plus the ordinal family's
# own `cumOR`. A crosstab has neither a cumulative odds ratio nor a pair of people to rank, so
# accepting them there would widen the request silently under a legend that does not say so. They are
# declared HERE, beside the shared table, so a crosstab can refuse them BY NAME rather than call them
# unknown -- the header prints `Model_cumOR`, so a reader will type it.
#' @keywords internal
MEASURE_ACRONYMS_REG <- c(cumOR = "odds_ratio", D = "difference", WR = "ratio")

# the all-lowercase twin of every acronym, DERIVED so that a row cannot be forgotten. ⚠ ONE clause: a
# ONE-LETTER acronym gets none -- `d` is a slip, not a spelling.
#' @keywords internal
measure_twins <- function(x) {
  keep <- nchar(names(x)) > 1L & tolower(names(x)) != names(x)
  c(x, stats::setNames(unname(x[keep]), tolower(names(x)[keep])))
}

# the spellings that reach ONE measure, for a message or an @param bullet. ONE renderer, so a hint and
# a help page cannot list different ones.
#' @keywords internal
measure_spellings <- function(measure, scope = MEASURE_ACRONYMS)
  names(scope)[scope %in% measure]

# The SOFT-DEPRECATED spellings, and all that is left of the old alias table: each is a
# (measure, policy) PAIR, which is why none could ever be a measure value -- `color_signif` owns the
# policy half now. The plain acronyms moved to MEASURE_ACRONYMS above.
#' @keywords internal
COLOR_LEGACY_ALIASES <- list(
  diff_ci    = list(measure = "difference", policy = "grey_non_signif"),
  after_ci   = list(measure = "difference", policy = "guaranteed_effect"),
  ci         = list(measure = "difference", policy = "guaranteed_effect")
)
#' @keywords internal
color_legacy_spellings <- function() names(COLOR_LEGACY_ALIASES)

# THE COLOUR VIEW -- every spelling `tab(color =)` / `fmt(color =)` accept, to its MEASURES key.
# Precomputed, because measure_key() resolves per (column x channel x backend).
#' @keywords internal
MEASURE_COLOR_KEYS <- c(
  stats::setNames(names(MEASURES), names(MEASURES)),
  measure_twins(MEASURE_ACRONYMS),
  vapply(COLOR_LEGACY_ALIASES, function(a) a$measure, character(1))
)
stopifnot("no colour spelling is declared twice" = !anyDuplicated(names(MEASURE_COLOR_KEYS)))

# a colour token -> its MEASURES key. "" for a "no colour" spelling, NA for a token that names nothing
# (the caller decides: an argument error, or an uncoloured hand-built column).
# WARNING: read once per (column x channel x backend) -- 85 lookups to build a 324-cell table, 129 in
# tab_html(), never one per cell. Keep it a lookup on the precomputed vector, never a regex.
# ⚠ single `[`: on a named CHARACTER vector `[[` throws on a missing name, where `[` gives NA.
#' @keywords internal
measure_key <- function(x) {
  if (length(x) == 0L) return("")
  x <- as.character(x)[1]
  if (is.na(x) || x %in% c("", "no")) return("")
  unname(MEASURE_COLOR_KEYS[x])
}

# "diff" | "or" | "contrib" (or "off"). Two measures sharing a class are a pure re-paint of each other.
#' @keywords internal
measure_builds <- function(measure) {
  k <- measure_key(measure)
  if (is.na(k) || !nzchar(k)) return("off")
  MEASURES[[k]]$builds
}

# the build classes in PRECEDENCE order, strongest first: when two channels ask different classes the
# pipeline computes the STRONGEST, the weaker deriving from fields already produced.
#' @keywords internal
COLOR_BUILD_ORDER <- c("contrib", "or", "diff")
stopifnot(setequal(COLOR_BUILD_ORDER,
                   unique(vapply(MEASURES, function(m) m$builds, character(1)))))

#' @keywords internal
measure_of_build <- function(build) {
  k <- names(MEASURES)[vapply(MEASURES, function(m) identical(m$builds, build), logical(1))]
  if (!length(k)) "" else k[[1]]
}

#' @keywords internal
measure_requires <- function(measure, gated = FALSE) {
  k <- measure_key(measure)
  if (is.na(k) || !nzchar(k)) return(character())
  rq <- MEASURES[[k]]$requires
  if (is.null(rq)) return(character())
  rq[rq == "always" | (rq == "gated" & isTRUE(gated))]
}

#' @keywords internal
measure_forces <- function(measure, what, gated = FALSE)
  what %in% names(measure_requires(measure, gated))

#' @keywords internal
measure_ref_auto <- function(measure) {
  k <- measure_key(measure)
  if (is.na(k) || !nzchar(k)) return(NA_character_)
  r <- MEASURES[[k]]$ref_auto
  if (is.null(r)) NA_character_ else r
}

# THE `color = TRUE` resolver: the automatic measure for a CONTEXT (auto_for keys), per channel.
# Priority is names(MEASURES) order -- a new measure declares its precedence by where its row sits.
#' @keywords internal
measure_auto <- function(context, channel = "text") {
  for (k in names(MEASURES)) {
    if (context %in% MEASURES[[k]]$auto_for[[channel]]) return(k)
  }
  ""
}

#' @keywords internal
measure_applies <- function(measure, kind) {
  k <- measure_key(measure)
  if (is.na(k) || !nzchar(k)) return(FALSE)
  kind %in% MEASURES[[k]]$applies_to
}

#' @keywords internal
COLOR_COL_KINDS <- c("pct", "num")

# `diff`/`ratio` are keyed by a COLUMN kind (repaintable by a per-kind spec); `or`/`contrib` by what the
# whole TABLE is (not).
#' @keywords internal
measure_kind_keyed <- function(measure) {
  k <- measure_key(measure)
  if (is.na(k) || !nzchar(k)) return(FALSE)
  any(unlist(MEASURES[[k]]$auto_for, use.names = FALSE) %in% COLOR_COL_KINDS)
}

#' @keywords internal
measure_validate <- function(color, producer = NULL, call = rlang::caller_env()) {
  keys <- vapply(color, measure_key, character(1), USE.NAMES = FALSE)
  # what may be NAMED here, each measure followed by the spellings that reach it -- so a near-miss
  # user is shown the acronym they actually typed, not only the canonical word.
  ok_here <- vapply(if (is.null(producer)) names(MEASURES) else measure_nameable(producer),
                    function(k) {
                      a <- measure_spellings(k)
                      paste0("\"", k, "\"",
                             if (length(a)) paste0(" (", paste0("\"", a, "\"", collapse = ", "), ")")
                             else "")
                    }, character(1), USE.NAMES = FALSE)
  bad  <- color[is.na(keys)]
  if (length(bad)) {
    # a REGRESSION-ONLY acronym is not "unknown": it names a real measure, at another door. Derived
    # from the scoped table, so the two cannot drift.
    scoped <- measure_twins(MEASURE_ACRONYMS_REG)
    hit    <- bad[bad %in% names(scoped)]
    if (length(hit)) cli::cli_abort(c(
      "{.val {hit}} {?is/are} a {.fn tab_reg} spelling of {.val {unname(scoped[hit])}}.",
      "i" = "Only a model estimates {?it/them}; here, write {.val {unname(scoped[hit])}}."),
      call = call)
    cli::cli_abort(c("Unknown color measure {.val {bad}}.",
                     "i" = "Valid measures: {ok_here}."),
                   call = call)
  }
  if (!is.null(producer)) {
    elsewhere <- setdiff(keys[nzchar(keys)], measure_nameable(producer))
    if (length(elsewhere)) {
      # name WHERE the measure lives instead of a bare "unknown measure". The producer's own word and
      # the sentence both DERIVE: measure_nameable("reg") is exactly the own-ref set (a measure whose
      # baseline is another column), which is what makes the second line true without a branch.
      other <- setdiff(MEASURE_PRODUCERS, producer)
      cli::cli_abort(c(
        "{.val {elsewhere}} cannot be used in a {.fn {MEASURE_PRODUCER_FN[[producer]]}} table.",
        "i" = paste0("It is a {.fn {MEASURE_PRODUCER_FN[[other]]}} measure: it compares an effect to ",
                     "another COLUMN, which only a model fills."),
        "i" = "Valid here: {ok_here}."),
        call = call)
    }
  }
  if (length(color) >= 2L && nzchar(keys[2]) &&
      !"bg" %in% MEASURES[[keys[2]]]$channels) {
    cli::cli_abort(paste0("{.val {color[2]}} is a whole-cell measure; ",
                          "it cannot go on the background channel."), call = call)
  }
  # Two measures whose baseline is ANOTHER COLUMN both score the single `obs` field, so a cell can
  # carry only one of them. Derived from measure_own_ref(), which names exactly those rows.
  own <- keys[nzchar(keys)]
  if (sum(vapply(own, measure_own_ref, logical(1))) > 1L) {
    cli::cli_abort(c(
      "{.val {own[vapply(own, measure_own_ref, logical(1))]}} cannot be used together.",
      "i" = "Both score the same per-cell comparison value, so a cell can carry only one of them."),
      call = call)
  }
  invisible(color)
}

# is THIS column the baseline of its own gap measure? A measure whose baseline is another column leaves
# `obs` empty on the column that IS that baseline (the reference group, or a model with no observed
# counterpart), so not one cell can be coloured. Say what the column is instead of printing an
# unreachable ladder. Tested on the STORED `obs` being empty, not the plan's gate (grey_non_signif also
# gates nothing on a comparable column, which must still show its ladder).
#' @keywords internal
legend_gap_baseline <- function(plan, no_obs)
  !is.null(plan) && isTRUE(no_obs) && measure_own_ref(plan$measure)

#' @keywords internal
# WHAT this column IS, said in the ladder's place. A crude companion is not "no observed effect" --
# it IS the observed effect, the baseline the shades beside it are measured from, which is exactly
# what a reader of a crude/adjusted pair needs told once.
legend_gap_baseline_word <- function(plan, spec = NULL) {
  if (identical(MEASURES[[plan$measure]]$ref_kind, "group"))  return(gettext("reference group"))
  if (identical(spec$role, "emp"))
    return(gettext("the observed effect (the reference for the adjustment)"))
  gettext("no observed effect")
}

#' @keywords internal
measure_policy <- function(measure, policy = "ignore", x = NULL) {
  fp <- MEASURES[[measure]]$force_policy
  if (is.null(fp)) return(policy)
  # a `force_policy` may be a PREDICATE ON THE COLUMN rather than a constant (see fmt_gap_force_policy).
  # With no column to ask, the caller's policy stands.
  if (is.function(fp)) fp <- if (is.null(x)) NULL else fp(x)
  if (is.null(fp)) policy else fp
}

# a legend token: plain text (c = NA) or a coloured break-word (c = palette slot 1:8). The CSS class is
# derived at render (tx_slot_class), not stored, so a break-word and the cells it describes name the
# same class. `esc` = escape markdown-active `*` in the md medium.
# DESIGN: a PLAIN token has no face of its own -- weight in a legend comes from the palette and from
# nothing else, so a legend never puts more emphasis on itself than the cells it describes carry.
.lg_tok  <- function(t, esc = FALSE)
  list(t = t, c = NA_integer_, ch = NA_character_, esc = isTRUE(esc))
.lg_ctok <- function(t, slot, ch) list(t = t, c = as.integer(slot), ch = ch, esc = FALSE)

#' @keywords internal
legend_resolve_lang <- function(lang = NULL) {
  if (is.null(lang) || identical(lang, "")) lang <- tx_option("lang")
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
# The older Sys.setlocale(LC_MESSAGES) trick fails on musl/Alpine (withr#213).
#
# ⚠ IT MUST RE-BIND OUR OWN DOMAIN, not a throwaway one: glibc keys the cache on (domain, msgid), so
# binding some other name leaves "R-tabxplor" cached and the SECOND language switch of a session
# silently no-ops -- one `lang = "fr"` render used to make every later `lang = "en"` one French.
# Rebinding to tempdir() and back to the real catalogue is the invalidation.
#' @keywords internal
flush_gettext_cache <- function() {
  try({
    po <- system.file("po", package = "tabxplor")
    bindtextdomain("R-tabxplor", tempdir())
    if (nzchar(po)) bindtextdomain("R-tabxplor", po)
  }, silent = TRUE)
  invisible(NULL)
}

legend_num <- function(v, lang) {
  s <- trimws(formatC(v, format = "fg", digits = 4, drop0trailing = TRUE))
  if (identical(lang, "fr")) s <- gsub("[.]", ",", s)
  s
}

# a compact reference word for the terse (console) form. The reference-free ("indep") baseline word is
# a per-channel FACT (ref_word, resolved from the policy-aware MEASURES row), because contrib's two
# readings name it differently. `lang` is set by with_legend_lang() in the calling environment, so
# gettext() already answers in the right language.
legend_ref_short <- function(spec) {
  ref <- spec$ref
  switch(ref$kind,
         "tot"      = if (!is.na(ref$label) && nzchar(ref$label)) ref$label else gettext("Total"),
         "level"    = if (!is.na(ref$label) && nzchar(ref$label)) ref$label else gettext("ref."),
         "category" = gettext("ref."),
         "indep"    = if (!is.null(spec$txt$ref_word)) spec$txt$ref_word else gettext("vs the mean"),
         "")
}

legend_break_label <- function(measure, brk, dir, is_pct, lang, policy = "ignore", scale_key = NULL) {
  m <- measure_facts(measure, policy, scale_key)
  if (is.null(m)) return(as.character(brk))
  scale <- if (isTRUE(m$break_scale) && isTRUE(is_pct)) 100 else 1
  # the ladder follows the CELLS: under `ratio_print = "raw"` a multiplicative threshold is written
  # the way the cells write it -- the plain number, and the inverse below the neutral. Only where the
  # two sides have DIFFERENT glyphs: a contribution is direction-free and reads "x2" on both sides.
  if (isTRUE(m$threshold_mult) && !identical(m$break_under, m$break_over) &&
      tx_ratio_print_raw()) {
    v <- abs(brk) * scale
    return(legend_num(if (dir < 0L) 1 / v else v, lang))
  }
  glyph <- if (dir < 0L) m$break_under else m$break_over
  paste0(glyph, legend_num(abs(brk) * scale, lang))
}

legend_break_tokens <- function(plan, is_pct, channel, lang, theme = "light") {
  if (is.null(plan)) return(list(over = list(), under = list()))
  measure <- plan$measure
  # the legend must not promise a distinction the cells do not make: a publication palette can render
  # two slots the same (the default one gives slots 3 and 4 one rendering), so a token whose rendering
  # repeats the previous one is dropped, keeping the LOWER threshold ("bold = at least +5 points"). NOT
  # a cap inside fmt_color_slots() -- the ENGINE stays theme-blind. The key is the WHOLE rendering,
  # marks included, so a palette that separates two slots by their mark alone keeps both break-words.
  fam <- if (identical(channel, "text")) "text" else "bg"
  hex <- get_color_style("color_code", type = fam, theme = theme)
  fc  <- get_color_style("face",       type = fam, theme = theme)
  look <- function(slot) paste(hex[slot], fc$bold[slot], fc$italic[slot], fc$underline[slot],
                               fc$marks[slot])
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
      # a marks palette says nothing typographically, so the break-word must WEAR its mark or the
      # legend would list four thresholds that all look alike.
      out  <- c(out, list(.lg_ctok(paste0(lab, fc$marks[slot]), slot, channel)))
    }
    out
  }
  list(over  = mk_side(plan$over_breaks,  plan$over_slots,  +1L),
       under = mk_side(plan$under_breaks, plan$under_slots, -1L))
}

legend_threshold_phrase <- function(plan, is_pct, is_std, lang) {
  if (is.null(plan)) return(NA_character_)
  md   <- measure_facts(plan$measure, plan$policy, plan$scale_key)
  # ONE break, written the way the ladder writes it. The x100 rule is legend_break_label()'s, so the
  # grey note and the ladder it describes cannot disagree.
  one  <- function(brk, glyph) {
    if (isTRUE(md$threshold_mult)) return(paste0(glyph, legend_num(abs(brk), lang)))
    sc100 <- isTRUE(md$break_scale) && isTRUE(is_pct)
    val   <- legend_num(abs(brk) * if (sc100) 100 else 1, lang)
    unit  <- legend_unit_word(md, is_pct, is_std)
    if (nzchar(unit)) paste0(glyph, val, " ", unit) else paste0(glyph, val)
  }
  pick <- function(v) if (length(v) == 0L || is.na(v[[1]])) NA_real_ else v[[1]]
  o <- pick(plan$over_breaks); u <- pick(plan$under_breaks)
  if (is.na(o) && is.na(u)) return(NA_character_)
  if (is.na(o) || is.na(u) || isTRUE(all.equal(o, u)))
    one(if (is.na(o)) u else o, if (isTRUE(md$threshold_mult)) .lg_times else "\u00b1")
  else
    # an ASYMMETRIC ladder enters at a different rung on each side, so the note must name both.
    paste0(one(o, md$break_over), " / ", one(u, md$break_under))
}

# `"diff"` consults the column kind (factor pct vs standardized numeric); the gap scales DECLARE their
# unit, keeping them clear of `is_std`.
legend_unit_word <- function(md, is_pct, is_std) switch(
  md$unit_kind,
  "diff"    = if (isTRUE(is_pct)) gettext("points") else if (isTRUE(is_std)) gettext("SD") else "",
  "points"  = gettext("points"),
  "std"     = gettext("SD"),
  "contrib" = gettext("the mean contribution"),
  "")

legend_join <- function(toks, sep) {
  if (length(toks) == 0) return(list())
  out <- list(toks[[1]])
  for (i in seq_along(toks)[-1]) out <- c(out, list(.lg_tok(sep)), list(toks[[i]]))
  out
}

# The word a palette gives each DIRECTION, or NA where it gives none.
#
# DESIGN: a COLOUR palette names none. Its two directions are a diverging ramp, and every medium now
# renders it -- the break-words in the legend are themselves blue and red, so "Shades of blue:" said
# in words what the words already looked like. A PUBLICATION palette is the opposite case: greyscale
# collapses the diverging ramp, direction lives in the face alone, and the two sides genuinely need
# naming ("Underlined:" / "Italic:") -- which is why those are the only legends still built as two
# sentences. NA on a side the palette does not name typographically (the emphasis palette's over
# side, both sides of the marks one).
# One pair PER CHANNEL: the background side is a grey fill in every publication palette, so a
# background-only column must not announce "Underlined:" about fills.
legend_shade_names <- function(theme = "light") {
  pal <- print_palette_of(tx_palette_theme(theme))
  if (is.null(pal)) return(list(text = c(over = NA_character_, under = NA_character_),
                                bg   = c(over = NA_character_, under = NA_character_)))
  nm <- function(f) if (is.null(f)) NA_character_ else f()
  list(text = c(over = nm(pal$shade$over), under = nm(pal$shade$under)),
       bg   = c(over = gettext("Grey fill"), under = gettext("Grey fill")))
}

# THE word the colour legend names a regression column by.
#
# ⚠ it is the MEASURE, never the contrast: reg_legend_word() drops the `m` / `ref` marker on purpose,
# because legend_group_by_body() groups columns by their rendered sentence and a crude column reading
# "RR" beside a model column reading "mRR" would split the one block the crude/adjusted merge exists
# to produce. The legend describes the ladder (the measure's); the header and the "Model:" line
# describe the estimand.
legend_reg_eff_word <- function(col, meta) {
  # the column's OWN family (the `model_family` attr), so a mixed table names each column correctly;
  # fall back to the table's scalar family when unset. `effect` / `measure` stay table-level.
  fam <- get_model_family(col); if (!nzchar(fam)) fam <- meta$family
  est <- reg_meta_estimand(meta, family = fam)
  # ⚠ A CRUDE column is named from ITS OWN SHAPE, not from the model's estimand: the two are the same
  # measure wherever they pair (so the block merges), and where they do not -- a poisson AME beside a
  # crude rate ratio -- the crude column must say what it actually holds.
  if (identical(get_role(col), "emp")) {
    ck <- reg_meta_crude_key(meta, fam)
    return(reg_crude_word(reg_crude_shape(ck, est)) %||% NA_character_)
  }
  if (!identical(get_role(col), "model")) return(NA_character_)   # an `n` column names no effect
  # the model column's measure, marker dropped (reg_legend_word). An unnamed additive one falls
  # through to the ladder's own word ("difference"), which reads better than an abbreviation would.
  reg_legend_word(est)
}

# The crude BLOCK a column belongs to, from the table's own record -- the key reg_crude_shape() needs
# to name a crude column. `crude_keys` is stored per outcome; a mixed table finds its own.
#' @keywords internal
reg_meta_crude_key <- function(meta, family = NULL) {
  ck <- meta$crude_keys
  if (is.null(ck) || !length(ck)) return(NA_character_)
  ck <- unlist(ck)
  if (!is.null(family) && nzchar(family)) {
    fk   <- unname(REG_FIT_FAMILY[family]); if (is.na(fk)) fk <- family
    fams <- meta$families %||% meta$family
    hit  <- names(fams)[fams %in% c(family, fk)]
    if (length(hit) && hit[[1]] %in% names(ck)) return(unname(ck[[hit[[1]]]]))
  }
  unname(ck[[1]])
}

legend_ref_label <- function(x, col, orientation) {
  tryCatch({
    if (identical(orientation, "col")) {
      idx <- which(purrr::map_lgl(x, ~ is_fmt(.) && isTRUE(is_refcol(.))))
      if (length(idx) == 0) return(NA_character_)
      nm <- names(x)[idx[[1]]]
      if (isTRUE(is_totcol(x[[idx[[1]]]]))) NA_character_ else nm   # a total column (by stored attr) -> generic "Total"
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

# legend_tottab_label() -- the name of the TOTAL TABLE, for a column that compares against it
# (`comp = "all"`). Composed from the two declared total names, never read off a row label: after a
# spread the total table IS a column block and has no row of its own to name it.
#' @keywords internal
#' @noRd
legend_tottab_label <- function(x) {
  tn <- tab_total_names()
  g  <- tryCatch({
    idx <- which(purrr::map_lgl(x, ~ is_fmt(.) && nzchar(get_col_group(.)) && all(is_tottab(.))))
    if (length(idx)) get_col_group(x[[idx[[1]]]]) else NA_character_
  }, error = function(e) NA_character_)
  if (is.na(g) || !nzchar(g)) g <- unname(tn[["tab"]])
  paste(unname(tn[["row"]]), g)
}

legend_ref_info <- function(x, col, measure, orientation, is_coef = FALSE, is_reg = FALSE,
                            policy = "ignore") {
  base_kind <- measure_facts(measure, policy)$ref_kind  # the measure's baseline concept, one field
  if (identical(base_kind, "indep"))
    return(list(kind = "indep", label = NA_character_, orientation = orientation))
  # these two baselines are NEITHER a total nor a predictor's reference category -- they are another
  # COLUMN's estimate (the observed effect, or the reference group's). Resolve BEFORE the is_reg branch,
  # which would otherwise claim "the reference category" and describe the wrong comparison.
  if (base_kind %in% c("observed", "group"))
    return(list(kind = base_kind, label = NA_character_, orientation = "row"))
  # a regression table has no total row -- every reg column is compared to the predictor's REFERENCE CATEGORY.
  if (isTRUE(is_reg) || identical(base_kind, "category") || isTRUE(is_coef))
    return(list(kind = "category", label = legend_ref_label(x, col, "row"), orientation = "row"))
  ref <- get_ref_type(col); ref <- if (length(ref)) as.character(ref)[1] else "tot"
  # DESIGN: `comp = "all"` moves the baseline from the sub-table's own total to the TOTAL TABLE's --
  # the whole point of the argument, and the legend used to print "Total" for both. Naming it is
  # what makes a spread table readable: the reference is one CELL, in the total-table block.
  if (identical(ref, "tot"))
    list(kind = "tot",
         label = if (isTRUE(get_comp_all(col))) legend_tottab_label(x) else NA_character_,
         orientation = orientation)
  else
    list(kind = "level", label = legend_ref_label(x, col, orientation), orientation = orientation)
}

# THE reference, in the FORM the sentence position needs. A legend line names its baseline three or
# four times, so a long phrase said in full each time buries the numbers:
#   "full"   the first naming in a line -- everything the reader must be told once
#   "short"  every later naming in the same line
#   "plain"  inside the NOTE, where a parenthetical follows: a phrase ending in "(...)" would give
#            two brackets in a row ("...the reference category (in bold) (Newcombe...)")
# A short reference that is already short (a Total row) is the same string in all three.
LEGEND_REF_FORMS <- c("full", "short", "plain")
legend_ref_phrase <- function(spec, form = "full") {
  ref <- spec$ref
  lab <- ref$label
  # "" as the short form: a second naming would only repeat what the first side already said.
  if (identical(ref$kind, "indep"))
    return(if (identical(form, "short")) "" else gettext("independence"))
  # the gap LEAD points at the column beside this one; the grey NOTE names the quantity tested.
  if (identical(ref$kind, "observed"))
    return(if (identical(form, "plain")) gettext("the observed effect")
           else gettext("the observed column"))
  # "...'s effect", not just "the reference group": what differs is the EFFECT, not the group.
  if (identical(ref$kind, "group"))    return(gettext("the reference group's effect"))
  # DESIGN: `ref != "tot"` says ONE thing, whatever level was picked and whether or not it resolves.
  # A merged table has one reference row PER sub-table, so legend_ref_label() returns NA there and the
  # phrase used to fall back to the literal "Total" -- describing a comparison the table never made.
  # The level is already visible (it is the bold row), and "the reference category (in bold)" is word
  # for word what tab_stars_legend() says, so the two footer lines name one thing once.
  if (ref$kind %in% c("category", "level"))
    return(switch(form,
                  short = gettext("ref"),
                  plain = gettext("the reference category"),
                  gettext("the reference category (in bold)")))
  base <- if (identical(ref$orientation, "col")) gettext("column") else gettext("row")
  if (is.na(lab) || !nzchar(lab)) lab <- gettext("Total")
  gettextf("the %s %s", lab, base)                 # EN "the Total row"; FR "la %2$s %1$s" -> "la ligne Total"
}

# the CI-method name (NA when there is none, e.g. contrib). `measure` is passed per channel: a gap
# measure on the background names ITS OWN test.
# THE interval-method labels: one row per engine, keyed on the `ci_method` attribute the producer
# stamps. Each entry is a FUNCTION so gettext() runs at render.
# WARNING: the keys are the values the producers write (tab_ci(), the two leaves, reg_column(),
# emp_col()) -- adding an engine means adding a row here; a missing row degrades to the generic phrase.
#' @keywords internal
CI_METHOD_LABELS <- list(
  wilson       = function() gettext("Wilson score interval"),
  wald         = function() gettext("Wald interval"),
  beta         = function() gettext("Korn-Graubard (beta) interval"),
  newcombe     = function() gettext("Newcombe score interval"),
  ac           = function() gettext("Wald interval with Agresti-Caffo adjustment"),
  welch        = function() gettext("Welch t interval"),
  student      = function() gettext("Student t interval"),
  ols          = function() gettext("Student t interval, pooled over the variable's levels"),
  woolf        = function() gettext("Wald interval on the log odds-ratio"),
  katz         = function() gettext("Wald interval on the log risk-ratio"),
  quasipoisson = function() gettext("quasi-Poisson interval"),
  robust       = function() gettext("robust-Poisson (delta) interval"),
  poisson      = function() gettext("Poisson interval"),
  profile      = function() gettext("profile-likelihood interval")
  # `katz` and `wald_log` live in CI_METHOD_WORDED below: their label needs the effect word.
)

# CI_METHOD_WORDED -- the engine whose LABEL needs a second fact. An OR, an IRR and an RR are the same
# interval on the same log scale, and only the effect WORD tells them apart, so the MODEL's engine is
# the one that has to ask. Every other engine names itself in CI_METHOD_LABELS.
#' @keywords internal
CI_METHOD_WORDED <- list(
  wald_log = list(IRR      = function() gettext("Wald interval on the log rate-ratio"),
                  OR       = function() gettext("Wald interval on the log odds-ratio"),
                  RR       = function() gettext("Wald interval on the log risk-ratio"),
                  .default = function() gettext("Wald interval on the log scale"))
)

# The engines that are a CLOSED FORM of the interval a MODEL column would fit. A crude column
# evaluates one of these instead of fitting, and each reproduces the univariable model's own interval:
# Woolf's 3e-13, Katz's 8e-09, the pooled OLS 2e-14, the dispersion one 5e-09, and the per-group
# (Welch / robust) forms the SANDWICH a design-based or over-dispersed fit reports -- 3e-03 on a
# weighted mean difference, 1.5e-03 on a poisson marginal effect.
#
# DESIGN: a label names the ESTIMAND, not the engine, so on a REGRESSION column these render as the
# interval the model column beside them renders (legend_method_name()) -- otherwise one arithmetic
# gets two legend blocks. The closed form is named once on the merged block instead, which is where a
# reader can act on it. On a plain `tab()` column there is no model twin and each names itself.
#' @keywords internal
CI_METHOD_CLOSED_FORM <- c(woolf = "Woolf", katz = "Katz", quasipoisson = "quasi-Poisson",
                           ols = "pooled OLS", welch = "Welch", robust = "robust")
# NOTE: potools extracts the closures above by static analysis (a gettext() literal inside a closure
# body is statically visible), so no `if (FALSE)` anchor is needed here. Contrast REG_CHECKS
# (R/reg-assumptions.R), whose nouns are BARE STRINGS gettext()ed dynamically -- its anchor is load-bearing.

legend_method_name <- function(spec, measure = spec$measure_text) {
  # a measure that does NOT read the column's own stored interval declares its own `method` -- NA for
  # contrib (no interval), one sentence each for the two gap measures (their SEs come from different
  # mathematics: `between_groups` compares DISJOINT subpopulations -> independent, quadrature exact;
  # `adjustment` compares two estimates on the SAME rows -> the difference of their influence functions).
  if (!is.null(measure) && !is.na(measure) && measure %in% names(MEASURES)) {
    md <- MEASURES[[measure]]
    if ("method" %in% names(md)) return(if (is.function(md$method)) md$method() else NA_character_)
  }
  m <- spec$ci_method
  if (is.null(m) || is.na(m) || !nzchar(m)) return(NA_character_)
  worded <- function(engine) {
    wd <- CI_METHOD_WORDED[[engine]]
    w  <- spec$eff_word; if (is.null(w) || is.na(w)) w <- ""
    (wd[[w]] %||% wd[[".default"]])()
  }
  # D23: on a REGRESSION column a closed form renders the interval its model twin renders, because it
  # IS that interval. Which twin depends only on the column's own scale: a multiplicative estimand is
  # a Wald interval on the log of it, everything else the plain Wald one.
  if (isTRUE(spec$is_reg) && m %in% names(CI_METHOD_CLOSED_FORM))
    return(if (isTRUE(EST_SCALES[[spec$scale]]$mult)) worded("wald_log") else CI_METHOD_LABELS$wald())
  if (!is.null(CI_METHOD_WORDED[[m]])) return(worded(m))
  lab <- CI_METHOD_LABELS[[m]]
  if (is.null(lab)) gettext("confidence interval") else lab()
}

# GATED on the basis: an unweighted / weights-only table refers to z and must not grow a "design df"
# clause that says nothing; only a real survey design gains it (df = t(#PSU - #strata), not z).
#
# ⚠ AND GATED OFF A REGRESSION. There the df is per COLUMN -- a model column and its crude twin are
# fitted on different numbers of parameters -- while this phrase is part of the legend's GROUPING
# key, so naming a number here would split the one crude/adjusted block the pair exists to form. A
# regression states its reference distribution once per model, in the "Model:" footer line instead.
legend_method_phrase <- function(spec, lang, measure = spec$measure_text) {
  conf <- gettextf("%s%% confidence", legend_num(spec$conf_level * 100, lang))
  df   <- spec$degf
  if (!isTRUE(spec$is_reg) && isTRUE(spec$basis %in% c("design", "design_partial")) &&
      !is.null(df) && length(df) == 1L && is.finite(df) && df > 0)
    conf <- gettextf("%s, %s design df", conf, legend_num(df, lang))
  m    <- legend_method_name(spec, measure)
  if (is.na(m)) conf else gettextf("%s, %s", m, conf)
}

# THE measure's name, in the register the medium can afford: `long = TRUE` (the export footers) gives
# the discipline's term and the base measure together, `long = FALSE` (the console, a plot guide) the
# short word. Both are read through measure_facts(), so the SCALE the ladder is on chooses the name --
# a difference of proportions, of means and of log odds are three quantities, not one word.
legend_measure_word <- function(measure, is_std, eff_word, policy = "ignore",
                                scale_key = NULL, long = FALSE) {
  # an SD-scaled ladder prints bare numbers (`-0.8 -0.4 -0.2 -0.1`) that are not in the outcome's own
  # units, so the name has to carry the unit -- once, before any of them, rather than only in the
  # trailing grey clause where a reader meets it after the numbers.
  if (!is.na(eff_word) && !measure_own_ref(measure))
    return(if (isTRUE(is_std) && identical(measure, "difference")) gettextf("%s in SD", eff_word)
           else eff_word)
  m <- measure_facts(measure, policy, scale_key)
  if (is.null(m)) return(measure)
  # fall back leftwards, so a scale that declares nothing still answers with the measure's own word.
  w <- NULL
  if (isTRUE(long) && isTRUE(is_std)) w <- m$word_long_std
  if (is.null(w) && isTRUE(long))     w <- m$word_long
  if (is.null(w) && isTRUE(is_std))   w <- m$word_std
  if (is.null(w))                     w <- m$word
  # `word` is a CLOSURE, so gettext() runs at render (never at build, which would freeze the locale)
  # AND its literal is visible to potools' static extraction. A non-translated word is function() "OR".
  w()
}

legend_ucfirst <- function(s) {
  if (!nzchar(s)) return(s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# pre-compute EVERY per-measure / per-channel display fact into the spec ONCE, so the token assemblers
# below are dumb templates (no switch(measure), no is_reg/is_coef branch). Per-channel facts resolve for
# BOTH channels into spec$txt / spec$bg.
legend_resolve_spec <- function(spec, lang) {
  # each channel resolves its facts under ITS OWN policy. `spec$policy` is the text channel's, and since
  # a gap measure's force_policy is a per-column predicate the two channels can genuinely differ.
  chan <- function(measure, policy = spec$policy, scale_key = NULL) {
    if (is.na(measure)) return(NULL)
    if (is.null(policy)) policy <- spec$policy
    md   <- measure_facts(measure, policy, scale_key)
    subj <- if (!is.na(spec$eff_word)) spec$eff_word
            else if (!is.null(md$subject)) md$subject else gettext("cell")
    u    <- legend_unit_word(md, spec$is_pct, spec$is_std)
    unit <- if (nzchar(u)) paste0(" ", u) else ""
    # `adjustment` / `between_groups` compare to ANOTHER COLUMN's estimate, so the reference is a
    # per-CHANNEL fact -- the scalar spec$ref_phrase would describe the wrong comparison on the background.
    own_ref <- measure_own_ref(measure)
    own_ref_phrase <- function(form) if (!own_ref) NA_character_ else
      legend_ref_phrase(list(ref = list(kind = md$ref_kind, label = NA_character_)), form)
    list(subject      = subj,
         ref_lead     = own_ref_phrase("full"),
         ref_short    = own_ref_phrase("short"),
         ref_note     = own_ref_phrase("plain"),
         # the measure NAMED IN WORDS, and the interval's bare name -- what the prose head is built of.
         word_long    = legend_measure_word(measure, spec$is_std, spec$eff_word, policy,
                                            scale_key, long = TRUE),
         word_guar    = if (isTRUE(spec$is_std) && is.function(md$word_guar_std)) md$word_guar_std
                        else md$word_guar,
         method_name  = legend_method_name(spec, measure),
         has_ref_lead = own_ref ||
           (isTRUE(md$has_ref_lead) && !isTRUE(spec$is_coef) && !isTRUE(spec$is_reg)),
         # under `guaranteed_effect` this measure's breaks are ABSOLUTE thresholds (contrib's residual),
         # not a CI floor -- so the sentence must not say "after subtracting the margin of error".
         guar_abs     = identical(md$break_origin, "threshold"),
         ref_word     = if (identical(md$unit_kind, "contrib")) gettext("vs the mean")
                        else gettext("vs independence"),
         # the interval NAME is per channel: a gap measure on the background runs its own test, so the
         # tail must not borrow the text channel's model interval.
         method_phrase = legend_method_phrase(spec, lang, measure),
         # a measure the generic "<subject> >= <reference>" lead would mis-state writes its own.
         lead_fn      = MEASURES[[measure]]$lead,
         policy       = policy,
         unit         = unit)
  }
  spec$txt <- chan(spec$measure_text, spec$plan_txt$policy, spec$plan_txt$scale_key)
  spec$bg  <- chan(spec$measure_bg,   spec$plan_bg$policy,  spec$plan_bg$scale_key)
  spec$ref_phrase       <- legend_ref_phrase(spec, "full")
  spec$ref_short        <- legend_ref_phrase(spec, "short")
  spec$ref_plain        <- legend_ref_phrase(spec, "plain")
  spec$method_phrase    <- legend_method_phrase(spec, lang)
  spec$conf_pct         <- legend_num(spec$conf_level * 100, lang)
  # the null a GAP is measured from, as the reader sees it in the column (1 on a ratio, 0 on a
  # difference). NA where the scale declares none -- fmt_gap_lead() then says "no effect" alone.
  nt <- EST_SCALES[[spec$scale %||% ""]]$neutral
  spec$neutral          <- if (is.null(nt) || is.na(nt)) NA_character_ else legend_num(nt, lang)
  primary <- if (is.null(spec$plan_txt)) spec$plan_bg else spec$plan_txt
  spec$threshold_phrase <- legend_threshold_phrase(primary, spec$is_pct, spec$is_std, lang)
  spec
}

# ---- assemblers: spec -> token stream (dumb templates over legend_resolve_spec() fields) ------------

legend_tokens_terse <- function(spec, lang, show_names) {
  colon <- if (identical(lang, "fr")) " : " else ": "
  toks <- list()
  # `esc = TRUE`: a COLUMN NAME is data -- a money level ("1-Lt $10000") or a starred one would
  # otherwise reach pandoc as inline math / emphasis.
  if (show_names) toks <- c(toks, list(.lg_tok(paste0(legend_name_list(spec$col_names),
                                                      colon), esc = TRUE)))
  rs <- legend_ref_short(spec)
  add_channel <- function(plan, prefix, is_bg) {
    if (legend_gap_baseline(plan, spec$no_obs))
      return(list(.lg_tok(paste0(prefix,
                                 legend_measure_word(plan$measure, spec$is_std, spec$eff_word,
                                                     plan$policy, plan$scale_key),
                                 colon, legend_gap_baseline_word(plan, spec)))))
    mw <- legend_measure_word(plan$measure, spec$is_std, spec$eff_word, plan$policy, plan$scale_key)
    bt <- legend_break_tokens(plan, spec$is_pct, if (is_bg) "bg" else "text", lang,
                             spec$theme %||% "light")
    seq_toks <- c(rev(bt$under), bt$over)
    lbl <- paste0(prefix, mw, if (!is_bg && nzchar(rs)) paste0(" (", rs, ")") else "", colon)
    c(list(.lg_tok(lbl)), legend_join(seq_toks, " "))
  }
  if (!is.null(spec$plan_txt)) toks <- c(toks, add_channel(spec$plan_txt, "", FALSE))
  if (!is.null(spec$plan_bg))  toks <- c(toks, list(.lg_tok(if (identical(lang, "fr")) " ; " else "; ")),
                                         add_channel(spec$plan_bg, paste0(gettext("bg"), " "), TRUE))
  # grey_non_signif names the first threshold a cell must reach: a grey cell is EITHER not significant
  # OR below that threshold (the guarantee is only coloured => significant).
  thr <- spec$threshold_phrase
  # "or not tested" only where some rows genuinely carry no test (partial_test).
  untested <- if (isTRUE(spec$partial_test)) paste0(", ", gettext("or not tested")) else ""
  pn <- switch(spec$policy,
               "grey_non_signif"   = if (!is.na(thr))
                                       paste0(gettextf("grey: non-significant or under %s", thr), untested)
                                     else paste0(gettext("grey: non-significant or small"), untested),
               # "error-adjusted" describes a CI floor; the absolute-threshold reading (contrib's
               # residual) subtracts nothing -- the breaks ARE the quantity.
               "guaranteed_effect" = if (isTRUE(spec$txt$guar_abs))
                                       gettext("all that is significant is colored")
                                     else gettext("all that is significant is colored, error-adjusted"),
               "")
  if (nzchar(pn)) toks <- c(toks, list(.lg_tok(paste0(" [", pn, "]"))))
  toks
}

# THE export legend, one grammar for every case:
#
#   [<col names> -- ]<HEAD><LADDER> <NOTE>
#
#   HEAD    "<Measure>: "  -- the measure NAMED IN WORDS, which is what a reader needs first and what
#           the old palette-led form ("Shades of blue:") never said. Dropped where the subject IS the
#           measure (a regression column's own effect word) or where the measure writes its own lead
#           (the two gap measures), so no line names one thing twice. Under `guaranteed_effect` it
#           carries the guarantee and names the interval ONCE, for both channels.
#   LADDER  per side "<subject> >= <ref> <breaks> <unit>", the two sides joined by ";" -- one
#           sentence, not two. Under `guaranteed_effect` they merge into ONE list after "from <ref>",
#           since both sides then read off the same interval floor.
#   NOTE    what an UNCOLOURED cell means. Only that: "coloured => significant" is a tautology the
#           reader can see. A publication palette says "Unmarked" for the same fact.
#
# A palette that NAMES its directions (the publication ones -- greyscale has no diverging ramp) is the
# one exception: its two sides stay two sentences, led by the face word.
legend_tokens_prose <- function(spec, lang, show_names) {
  # French typography: a (thin) space before the high punctuation ; : (matches the user's examples).
  semi  <- if (identical(lang, "fr")) " ; " else "; "
  colon <- if (identical(lang, "fr")) " : " else ": "
  mark  <- tx_is_print(tx_palette_theme(spec$theme))

  # ---- one side of one ladder: "<lead> <b1>; <b2>; ... <unit>" --------------------------------
  side_tokens <- function(plan, dir, is_bg, lead) {
    bt   <- legend_break_tokens(plan, spec$is_pct, if (is_bg) "bg" else "text", lang,
                                spec$theme %||% "light")
    side <- if (dir > 0) bt$over else bt$under
    if (length(side) == 0) return(NULL)
    cf <- if (is_bg) spec$bg else spec$txt
    c(list(.lg_tok(paste0(lead, " "))), legend_join(side, semi), list(.lg_tok(cf$unit)))
  }

  # THE REFERENCE IS NAMED IN FULL ONCE PER LINE, then short. A line names its baseline three or four
  # times, and "the reference category (in bold)" said four times buries the thresholds it is there to
  # frame. Line-level, not channel-level: the background channel continues the same sentence.
  named_ref <- FALSE
  ref_of <- function(cf) {
    out <- if (named_ref) { if (!is.na(cf$ref_short)) cf$ref_short else spec$ref_short }
           else           { if (!is.na(cf$ref_lead))  cf$ref_lead  else spec$ref_phrase }
    named_ref <<- TRUE
    out
  }

  # ---- one channel: head + ladder ---------------------------------------------------------------
  channel_tokens <- function(plan, is_bg, with_shades) {
    if (is.null(plan)) return(NULL)
    cf   <- if (is_bg) spec$bg else spec$txt
    guar <- identical(cf$policy, "guaranteed_effect")
    # the baseline column of a gap measure: one clause, no ladder. The measure is named by the ladder
    # beside it, so this states only WHAT the column is -- but on the background channel it must still
    # say which channel it is talking about.
    if (legend_gap_baseline(plan, spec$no_obs)) {
      w <- legend_gap_baseline_word(plan, spec)
      return(list(.lg_tok(if (is_bg) paste0(gettext("Background colour"), colon, w, ".")
                          else       paste0(legend_ucfirst(w), "."))))
    }
    # merge the two sides into one list only where they differ by the SIGN alone: a measure with its
    # own lead says something different on each side ("further from" / "closer to"), and one without
    # a reference has no "from <ref>" to hang the merged list on.
    merged <- guar && !isTRUE(cf$guar_abs) && isTRUE(cf$has_ref_lead) && is.null(cf$lead_fn)
    sh     <- if (with_shades) spec$shades[[if (is_bg) "bg" else "text"]]
              else c(over = NA_character_, under = NA_character_)

    # -- head
    head_txt <- ""
    # a REGRESSION column already names its measure in the subject -- its effect word IS the acronym
    # the header prints -- so a head would say the same thing twice. Everything else takes one,
    # including a measure that writes its own lead: `contrib`'s lead states a DIRECTION, not a name.
    if (is.na(spec$eff_word)) {
      w <- cf$word_long
      if (guar && !isTRUE(cf$guar_abs)) {
        # ONE msgid per measure, not "%s-guaranteed %s": in French the participle agrees with the
        # measure (*differance garantie* vs *rapport garanti*), which a shared template cannot do.
        w <- if (is.function(cf$word_guar)) cf$word_guar(spec$conf_pct)
             else gettextf("%s%%-guaranteed %s", spec$conf_pct, w)
        if (!is_bg && !is.na(cf$method_name)) w <- gettextf("%s (%s floor)", w, cf$method_name)
      }
      head_txt <- if (is_bg) gettextf("Background colour, %s", w) else legend_ucfirst(w)
    } else if (is_bg) {
      head_txt <- gettext("Background colour")
    }

    # -- ladder
    if (merged) {
      # NO colon between this head and its ladder: the guarantee reads as ONE sentence
      # ("95%-guaranteed <measure> (<method> floor) from the Total row +0; ..."); a colon cuts it in
      # two. Everywhere else the head is a LABEL and keeps its colon.
      lead <- gettextf("from %s", ref_of(cf))
      if (!nzchar(head_txt)) lead <- legend_ucfirst(lead)
      bt   <- legend_break_tokens(plan, spec$is_pct, if (is_bg) "bg" else "text", lang,
                                  spec$theme %||% "light")
      both <- c(bt$over, bt$under)
      if (length(both) == 0) return(NULL)
      body <- c(list(.lg_tok(paste0(lead, " "))), legend_join(both, semi), list(.lg_tok(cf$unit)))
      return(c(if (nzchar(head_txt)) list(.lg_tok(paste0(head_txt, " "))), body, list(.lg_tok("."))))
    }
    # a named face makes the two sides two SENTENCES (the face is what tells them apart); otherwise
    # they are one sentence with a ";". A palette may name ONE side only (print_emphasis): the other
    # then opens its own sentence and must be capitalised like one.
    named <- !is.na(sh[["over"]]) || !is.na(sh[["under"]])
    one <- function(dir) {
      cmp   <- if (dir > 0) .lg_ge else .lg_le
      rp    <- ref_of(cf)
      lead  <- if (!is.null(cf$lead_fn)) cf$lead_fn(cf$subject, rp, dir, spec$neutral)
               else if (cf$has_ref_lead) gettextf("%s %s %s", cf$subject, cmp, rp)
               else                      gettextf("%s %s", cf$subject, cmp)
      shade <- if (dir > 0) sh[["over"]] else sh[["under"]]
      if (!is.na(shade)) lead <- paste0(shade, colon, lead)
      # ⚠ AN ACRONYM IS DATA, NEVER PROSE: capitalising it printed "CumOR" / "Diff" where the header
      # says `cumOR` / `diff`. Only the generic subject ("cell") ever opens a sentence.
      else if (is.na(spec$eff_word) && (named || !nzchar(head_txt))) lead <- legend_ucfirst(lead)
      side_tokens(plan, dir, is_bg, lead)
    }
    ov <- one(+1L); un <- one(-1L)
    if (is.null(ov) && is.null(un)) return(NULL)
    sep   <- if (named) list(.lg_tok(". ")) else list(.lg_tok(semi))
    body  <- if (is.null(ov)) un else if (is.null(un)) ov else c(ov, sep, un)
    head_tok <- if (!nzchar(head_txt)) NULL
                else if (named) list(.lg_tok(paste0(head_txt, ". ")))
                else            list(.lg_tok(paste0(head_txt, colon)))
    c(head_tok, body, list(.lg_tok(".")))
  }

  toks <- list()
  if (show_names)  # `esc` keeps a variable name DATA (see terse); its face is plain (see .lg_tok).
    toks <- c(toks, list(.lg_tok(paste0(legend_name_list(spec$col_names), " \u2014 "), esc = TRUE)))

  # a measure may declare ONE sentence of honesty about itself (MEASURES$<m>$caveat). Only `adjustment`
  # has one -- see fmt_noncollapsible_caveat().
  for (m in c(spec$measure_text, spec$measure_bg)) {
    if (is.na(m) || is.null(MEASURES[[m]]$caveat)) next
    cv <- MEASURES[[m]]$caveat(spec)
    if (!is.null(cv)) { spec$caveat <- cv; break }
  }

  # ... but a line that shows no ladder at all (a gap measure's baseline column) says nothing the
  # caveat could qualify, and the ladder line beside it already carries it.
  if (legend_gap_baseline(spec$plan_txt %||% spec$plan_bg, spec$no_obs)) spec$caveat <- NULL

  is_bg_only <- is.null(spec$plan_txt)
  primary    <- if (is_bg_only) spec$plan_bg else spec$plan_txt
  toks <- c(toks, channel_tokens(primary, is_bg_only, with_shades = TRUE))
  # a second measure on the background channel (e.g. color = c("diff","ratio")): it takes no face
  # word (the fills carry magnitude only) but names its own measure.
  if (!is.null(spec$plan_txt) && !is.null(spec$plan_bg)) {
    bg <- channel_tokens(spec$plan_bg, TRUE, with_shades = FALSE)
    if (!is.null(bg)) toks <- c(toks, list(.lg_tok(" ")), bg)
  }

  # ---- the note: what an UNCOLOURED cell means --------------------------------------------------
  # NB: each format string is ONE literal, not paste0(...): xgettext extracts each constant
  # separately, so a paste0-split message never matches the joined string gettextf looks up ->
  # translation silently fails. And ONE WHOLE SENTENCE per variant, never a %s for the verb: a single
  # word carries gender and number in French, which only a full-sentence msgid can get right.
  note <- NULL
  if (identical(spec$policy, "grey_non_signif")) {
    thr <- spec$threshold_phrase
    note <- if (!is.na(thr)) {
      if (mark)
        gettextf("Unmarked: not significantly different from %s (%s) or under the first threshold (%s).",
                 spec$ref_plain, spec$method_phrase, thr)
      else
        gettextf("Uncoloured: not significantly different from %s (%s) or under the first colour threshold (%s).",
                 spec$ref_plain, spec$method_phrase, thr)
    } else {
      if (mark)
        gettextf("Unmarked: not significantly different from %s (%s).", spec$ref_plain, spec$method_phrase)
      else
        gettextf("Uncoloured: not significantly different from %s (%s).", spec$ref_plain, spec$method_phrase)
    }
  } else if (identical(spec$policy, "guaranteed_effect")) {
    # the absolute-threshold reading (contrib's residual) grades the quantity itself, so its note
    # names the significance threshold rather than a guarantee subtracted from a deviation.
    note <- if (isTRUE(spec$txt$guar_abs)) {
      if (mark)
        gettextf("Unmarked: below the significance threshold (%s). The thresholds above are comparable between tables.",
                 spec$method_phrase)
      else
        gettextf("Uncoloured: below the significance threshold (%s). The thresholds above are comparable between tables.",
                 spec$method_phrase)
    } else {
      if (mark) gettextf("Unmarked: not significantly different from %s.", spec$ref_plain)
      else      gettextf("Uncoloured: not significantly different from %s.", spec$ref_plain)
    }
  }
  # where only SOME rows carry a test, uncoloured means a third thing -- say so, or a reader takes an
  # untested cell for a tested-and-null one.
  if (!is.null(note) && isTRUE(spec$partial_test))
    note <- paste0(note, " ", if (mark) gettext("Some rows carry no test and are left unmarked.")
                              else      gettext("Some rows carry no test and are left uncoloured."))
  if (!is.null(note)) toks <- c(toks, list(.lg_tok(paste0(" ", note))))

  # the note above states ONE comparison (the text channel's). A gap measure on the background compares
  # something else, by a test of its own, so it needs one clause -- gated on the BACKGROUND's own
  # resolved policy, not spec$policy (the TEXT channel's).
  if (!identical(spec$plan_bg$policy, "ignore") &&
      !is.null(spec$plan_txt) && !is.null(spec$plan_bg) &&
      !is.null(spec$bg) && !is.na(spec$bg$ref_note)) {
    toks <- c(toks, list(.lg_tok(paste0(" ", gettextf(
      "Background: the same rule, applied to the gap with %s (%s).",
      spec$bg$ref_note, spec$bg$method_phrase)))))
  }
  if (!is.null(spec$caveat)) toks <- c(toks, list(.lg_tok(paste0(" ", spec$caveat))))
  toks
}

# ---- render a token stream for one medium ----------------------------------------------------------
# "runs" -> a list of runs list(text=, color=, bold=); every other medium -> a single string.
# Coloured break-words carry the visual weight of the numbers they describe: TEXT-colour ones stay
# BOLD, BACKGROUND-colour ones are PLAIN (a fill bolds nothing). A PLAIN token is always plain -- the
# palette is the only source of weight here.
# The md branch backslash-escapes `*` in plain-token text so pandoc does not read emphasis.
legend_render_line <- function(tokens, medium, theme, colored, classes = FALSE) {
  # `theme` may be the render intent "auto"; a palette is always light/dark -- resolve it or
  # get_color_style() errors on a length-0 vector.
  pal <- tx_palette_theme(theme)
  # a "runs" medium draws TEXT and cannot fill, so a background break-word borrows the darker bg_legend
  # palette (the fills are invisible on the white page a run sits on). The text channel is the "text" family.
  fam <- function(ch) if (identical(ch, "text")) "text"
                      else if (identical(medium, "runs")) "bg_legend" else "bg"
  slot_hex <- function(slot, ch)
    toupper(unname(get_color_style("color_code", type = fam(ch), theme = pal)[slot]))
  is_colored_tok <- function(tk) isTRUE(colored) && !is.na(tk$c) && tk$c > 0L
  # the break-word wears the SAME face as the cells it describes -- read from the palette, not inferred
  # (the html branch writes `font-weight:bold` inline, which beats the stylesheet, so a "has a hex" ->
  # bold guess would render a print under-side break-word bold while its cells are italic).
  tok_face <- function(tk, k) {
    if (!is_colored_tok(tk)) return(FALSE)
    isTRUE(get_color_style("face", type = fam(tk$ch), theme = pal)[[k]][tk$c])
  }
  is_bold_tok  <- function(tk) tok_face(tk, "bold")
  semantic     <- fmt_face_semantic(pal)
  is_ital_tok  <- function(tk) tok_face(tk, "italic")
  # `underline` is the three-value vocabulary, so it has its own reader.
  is_under_tok <- function(tk) {
    if (!is_colored_tok(tk)) return("")
    get_color_style("face", type = fam(tk$ch), theme = pal)$underline[tk$c]
  }
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
      # plain token: a variable name (bold) or footer text (stars, weight line...). `esc` escapes the
      # pandoc metacharacters so a legend is not re-read as markup (user subtext left raw): `*` runs
      # would pair as emphasis, and `$` runs as INLINE MATH -- which a money level name ("1-Lt
      # $10000", "$25000 or more") triggers as soon as two of them appear in one line.
      # DESIGN: the html medium needs it too -- a knitted page's raw-html goes THROUGH pandoc. The
      # html arm entity-encodes instead, `&` FIRST or it double-escapes the entities it just wrote.
      txt <- tk$t
      if (identical(medium, "md")   && isTRUE(tk$esc)) {
        txt <- gsub("*", "\\*", txt, fixed = TRUE)
        txt <- gsub("$", "\\$", txt, fixed = TRUE)
      }
      if (identical(medium, "html") && isTRUE(tk$esc)) {
        txt <- gsub("&", "&amp;", txt, fixed = TRUE)
        txt <- gsub("<", "&lt;" , txt, fixed = TRUE)
        txt <- gsub("*", "&#42;", txt, fixed = TRUE)
        txt <- gsub("$", "&#36;", txt, fixed = TRUE)
      }
      if (!bold) return(txt)
      if (identical(medium, "console")) return(cli::style_bold(txt))
      if (identical(medium, "html"))    return(paste0("<b>", txt, "</b>"))
      if (identical(medium, "md"))      return(paste0("**", txt, "**"))
      return(txt)
    }
    if (identical(medium, "console")) {
      # `theme` is an argument, so the palette must follow it -- reading the option here would render a
      # legend the caller never asked for.
      style <- get_color_style("crayon", type = fam(tk$ch), theme = pal)[[tk$c]]
      out <- style(tk$t)
      if (bold) out <- cli::style_bold(out)
      if (ital) out <- cli::style_italic(out)
      # no terminal rule is portably doubled, so both ruled rungs read as one line here.
      if (nzchar(und))  out <- cli::style_underline(out)
      out
    } else if (identical(medium, "html")) {
      # DESIGN: the span is emitted inline (kableExtra::text_spec() is byte-unstable across releases).
      # `classes` = "our stylesheet ships with this output" -> the break-word carries a slot CLASS
      # (theme-toggle-safe); else keep hex. Weight is per-channel: `font-weight:bold` only on the text
      # channel. `font-weight` is stated EXPLICITLY when the palette says not-bold, since this inline
      # span must override the stylesheet's `.p1..m4{font-weight:bold}` baseline.
      wt <- if (bold) "font-weight:bold;" else if (identical(tk$ch, "text")) "font-weight:normal;" else ""
      if (ital) wt <- paste0(wt, "font-style:italic;")
      if (nzchar(und)) wt <- paste0(wt, "text-decoration:",
                                    if (identical(und, "double")) "underline double" else "underline",
                                    ";")
      # a palette whose meaning is TYPOGRAPHY writes the break-word as markup too, so a sanitizer that
      # strips class/style (GitHub, Word paste) keeps the tags. No-op under the colour palettes.
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
      # `**` makes the TEXT break-words stand out in RAW markdown too; the background channel is plain.
      # A monochrome palette's under-side is ITALIC (`*[..]{.m1}*`).
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
# Does any cell of this column carry a value the colour measures in force could grade? Reads the same
# per-measure `raw` getter fmt_color_plan() does, so "coloured nowhere" and "named in no legend" are
# the one fact.
#' @keywords internal
#' @noRd
fmt_has_color_source <- function(col) {
  ks <- unique(c(get_color(col), get_color_bg(col)))
  ks <- ks[!is.na(ks) & !ks %in% c("no", "")]
  if (!length(ks)) return(FALSE)
  any(vapply(ks, function(k) {
    m <- MEASURES[[measure_key(k)]]
    is.null(m) || any(!is.na(m$raw(col)))
  }, logical(1)))
}

legend_specs <- function(x, theme = "light") {
  is_f <- purrr::map_lgl(x, is_fmt)
  ct   <- get_color(x); cbg <- get_color_bg(x)
  keep <- is_f & ((!is.na(ct)  & !ct  %in% c("no", "")) |
                  (!is.na(cbg) & !cbg %in% c("no", "")))
  # ... and a CROSSTAB column must have something for that measure to GRADE. A ladder names the columns
  # it reads, and a column whose measure is void everywhere can never wear a shade -- the row-% Total
  # of an odds-ratio table, whose 2x2 is degenerate (tab_apply_reference), is the case this exists for.
  # ⚠ REGRESSION columns are exempt (`role`): a crude column under `color = "adjustment"` is void by
  # construction -- it IS the baseline the gap is measured from -- and must still be named beside its
  # model column, which is what legend_reg_adapter() folds into one line.
  if (any(keep)) keep[keep] <- purrr::map_lgl(x[keep], function(col)
    nzchar(get_role(col) %||% "") || fmt_has_color_source(col))
  if (!any(keep)) return(list())

  col_vars_levels <- tab_get_vars(x)$col_vars_levels
  col_vars_levels <- col_vars_levels[is_real_col_var(names(col_vars_levels))]
  kept_names <- names(x)[keep]

  # the KIND is a stored fact (meta$spec$kind) -- ask it, not "does this table carry a model recipe": a
  # reg table can legitimately have no `call` and would then be legended as a crosstab.
  meta   <- reg_call(x)
  is_reg <- tab_is_reg(x)
  shades <- legend_shade_names(theme)
  # the mean_diff scale in force. Its `std` flag decides whether a numeric/coef diff is sd-standardized
  # (SD units) or raw. SAME source fmt_color_plan() reads, so the legend can never disagree with the cells.
  mean_diff_std <- isTRUE(color_scales()$mean_diff$std)

  # One spec per colored column, so several measures sharing a col_var (a reg outcome span's model +
  # empirical columns) each get their own spec. legend_group_by_body() folds columns with an IDENTICAL
  # rendered body, so a crosstab's level columns still collapse to one line.
  reps <- purrr::imap(col_vars_levels, function(cols, cv) {
    cc <- cols[cols %in% kept_names]
    purrr::map(cc, function(cn) list(cn = cn, cv = cv))
  })
  reps <- purrr::flatten(purrr::compact(reps))
  if (length(reps) == 0) return(list())

  # Build the rich specs. For a reg table the empirical + model columns describe the SAME scale but
  # differ superficially (role, effect word, reference label); legend_reg_adapter reconciles them per
  # col_var so both fold into one line. A crosstab is untouched.
  specs <- purrr::map(reps, function(e) {
    cn <- e$cn; cv <- e$cv
    col      <- x[[cn]]
    # same cross-channel arbiter as the cells (drops a degenerate guaranteed_effect channel), so a
    # disabled channel loses its legend line too.
    pl       <- resolve_color_channel_plans(col)
    plan_txt <- pl$text
    plan_bg  <- pl$bg
    if (is.null(plan_txt) && is.null(plan_bg)) return(NULL)
    # the column's stored scale row answers all of these -- the SAME facts fmt_color_plan() reads, so
    # the legend and the cells cannot describe different ladders.
    scl      <- fmt_scale_row(col)
    is_coef  <- identical(scl$var_kind, "coef")
    is_mean  <- scl$var_kind %in% c("mean", "count")
    # three diff "kinds": factor pct (x100, "points"), numeric/coef STANDARDIZED (SD), numeric/coef RAW.
    # is_pct drives the x100; is_std drives the "SD" wording.
    is_pct   <- identical(scl$ladder, "pct")
    # a NON-gaussian coefficient (measure = "raw_coefficient") colours on the LOGGED odds_ratio scale, NOT the
    # SD-standardized one, so its legend must NOT say "SD". That three-way distinction IS `ladder`.
    is_std   <- identical(scl$ladder, "std") && mean_diff_std
    policy   <- if (!is.null(plan_txt)) plan_txt$policy else plan_bg$policy
    m_txt    <- if (!is.null(plan_txt)) plan_txt$measure else NA_character_
    m_bg     <- if (!is.null(plan_bg))  plan_bg$measure  else NA_character_
    orient   <- if (identical(get_pct_type(col), "col")) "col" else "row"
    eff_word <- if (isTRUE(is_reg)) legend_reg_eff_word(col, meta) else NA_character_
    # the emp/model split reads the column's STORED `role` attr, not the "Emp." name prefix. Fall back
    # to "model" if an old/hand-built reg column lacks it.
    role     <- if (isTRUE(is_reg)) { r <- get_role(col); if (nzchar(r)) r else "model" } else "model"
    ref      <- legend_ref_info(x, col, m_txt, orient, is_coef = is_coef, is_reg = is_reg,
                                policy = policy)
    scale_key <- get_scale(col)
    ci_method <- get_ci_method(col)
    conf_lvl  <- get_conf_level(col)
    # `fmt_degf_attr()` is the RAW read (an unstamped column must contribute nothing, where get_degf()
    # would answer Inf).
    degf_col  <- fmt_degf_attr(col)
    basis_col <- get_basis(col)
    # does this column carry a test on SOME rows only? A gap measure's SE is missing wherever it could
    # not be computed, and those rows render like a tested-non-significant one, so the grey NOTE must
    # not claim they were all tested. Per-column, so a fully-tested column's legend is unchanged.
    # `no_obs`: this column has no baseline to be compared to -- so it IS the baseline.
    no_obs      <- all(is.na(get_obs(col)))
    gse         <- get_gap_se(col)
    # "a measure whose test may be missing per row" IS "a measure whose baseline is another column"
    # (measure_own_ref names exactly those rows).
    gap_chans    <- c(m_txt, m_bg)
    gap_chans    <- gap_chans[!is.na(gap_chans)]
    partial_test <- !identical(policy, "ignore") &&
      any(vapply(gap_chans, measure_own_ref, logical(1))) &&
      any(is.na(gse)) && any(!is.na(gse))
    # a legend line names the column BLOCK, not the bare variable -- after a spread two blocks share one
    # col_var and differ only by sub-population, so a bare name would say "marital" twice.
    list(col_var = fmt_col_block(cv, get_col_group(col))$label,
         col_name = cn, plan_txt = plan_txt, plan_bg = plan_bg,
         partial_test = partial_test, no_obs = no_obs,
         measure_text = m_txt, measure_bg = m_bg,
         is_mean = is_mean, is_std = is_std, is_pct = is_pct, is_coef = is_coef,
         policy = policy, orientation = orient, scale = scale_key,
         ci_method = ci_method, conf_level = conf_lvl, degf = degf_col, basis = basis_col,
         is_reg = is_reg, eff_word = eff_word, role = role, shades = shades,
         theme = theme,
         model_family = get_model_family(col),        # the collapsibility caveat below
         ref = ref)
  })
  specs <- purrr::compact(specs)
  if (length(specs) == 0) return(list())

  if (isTRUE(is_reg)) specs <- legend_reg_adapter(specs)
  specs
}

# group the per-column specs into legend lines by their RENDERED BODY (the name-less token stream): two
# specs share a line iff they render the same body, so grouping can NEVER drift from what prints.
# Style-local (terse vs prose may fold differently); groups keep first-occurrence order.
legend_group_by_body <- function(specs, style, lang) {
  body_of <- function(s) {
    toks <- if (identical(style, "prose")) legend_tokens_prose(s, lang, FALSE)
            else                           legend_tokens_terse(s, lang, FALSE)
    paste0(vapply(toks, function(tk) tk$t, character(1)), collapse = "")
  }
  bodies <- vapply(specs, body_of, character(1))
  lapply(unique(bodies), function(k) specs[bodies == k])
}

# reconcile the empirical + model specs of each col_var of a REG table so they fold into one legend
# line: when a col_var has one distinct non-NA reference label, apply it to every spec there.
#
# It used to NEUTRALISE the model's additive effect word as well, because a crude column had none to
# match. Both sides name their own measure now (legend_reg_eff_word), and they agree wherever they
# pair -- so neutralising would REINTRODUCE the mismatch it was written to remove.
legend_reg_adapter <- function(specs) {
  by_cv <- split(seq_along(specs), purrr::map_chr(specs, "col_var"))
  for (idx in by_cv) {
    labs <- unique(stats::na.omit(vapply(specs[idx], function(s) s$ref$label, character(1))))
    if (length(labs) == 1L) for (i in idx) specs[[i]]$ref$label <- labs
  }
  specs
}

legend_name_list <- function(names, max_n = 6L) {
  norm <- vapply(names, function(nm) {
    nm <- gsub("<br>|\n|\u202f", " ", nm)                  # undo html-path wrap markers
    nm <- trimws(gsub("[[:space:]]+", " ", nm))
    gsub(" ", "\u00a0", nm)                                # protect intra-name spaces (no-break)
  }, character(1), USE.NAMES = FALSE)
  extra <- length(norm) - max_n
  if (extra > 0L) norm <- c(utils::head(norm, max_n), gettextf("\u2026 +%d vars", extra))
  paste(norm, collapse = ", ")
}

legend_streams <- function(x, style, lang, theme = "light") {
  with_legend_lang(lang, function(lg) {
    # `theme` reaches here for ONE reason -- the shade NAMES a palette gives its directions. Everything
    # else the legend needs is theme-free.
    specs <- legend_specs(x, theme)
    if (length(specs) == 0) return(list())
    specs <- lapply(specs, function(s) legend_resolve_spec(s, lg))
    grp   <- legend_group_by_body(specs, style, lg)
    show_global <- length(grp) > 1
    # a col_var spawning SEVERAL legend lines (a reg outcome span -> model + empirical) is prefixed by
    # the COLUMN names (the col_var alone is ambiguous); a single-line col_var keeps its name.
    cv_lines <- table(unlist(lapply(grp, function(g) unique(purrr::map_chr(g, "col_var")))))
    lapply(grp, function(g) {
      spec <- g[[1]]
      cvs  <- unique(purrr::map_chr(g, "col_var"))
      # a role-MIXED group (empirical + model merge) shows a prefix and names the COLUMNS; a role-uniform
      # group keeps the old rule.
      mixed       <- length(unique(purrr::map_chr(g, "role"))) > 1
      show_this   <- show_global || mixed
      name_by_col <- mixed || any(cv_lines[cvs] > 1)
      spec$col_names <- if (name_by_col) unique(purrr::map_chr(g, "col_name")) else cvs
      # a multi-outcome regression column carries a trailing " [dep]" bracket in its NAME for console
      # clash-avoidance; the col_var span already names the outcome, so the legend strips it. Gated to
      # reg groups so a level label ending in "[...]" is untouched.
      if (any(nzchar(purrr::map_chr(g, "role"))))
        spec$col_names <- tx_strip_outcome_suffix(spec$col_names)
      # A crude column EVALUATES a closed form of the very interval the model column fits, which is
      # why the two share one block (both labels name the estimand). The block names the closed form
      # once, so the reader is told which arithmetic the observed column ran.
      if (identical(style, "prose") && !is.na(spec$method_phrase)) {
        cf <- unique(stats::na.omit(vapply(g, function(o)
          unname(CI_METHOD_CLOSED_FORM[o$ci_method %||% ""]), character(1))))
        if (length(cf) == 1L)
          spec$method_phrase <- if (mixed)
            gettextf("%s; matching %s interval on the observed column", spec$method_phrase, cf)
          else gettextf("%s; %s closed form", spec$method_phrase, cf)
      }
      if (identical(style, "prose")) legend_tokens_prose(spec, lg, show_this)
      else                           legend_tokens_terse(spec, lg, show_this)
    })
  })
}

# fmt_point_palette() -- the 8 slot colours to paint a MARK with (a plotted point, a row band), not a
# glyph. One forced deviation: a publication palette gives every TEXT slot near-black and separates directions by
# bold vs italic, which a point cannot be, so a mark borrows the print palette's dark grey ramp
# (bg_legend). Nothing is lost: in a forest plot the DIRECTION is read off the null line, so colour only
# carries magnitude. Every other theme returns the table's own palette.
#' @keywords internal
fmt_point_palette <- function(theme = "light", channel = c("text", "bg")) {
  get_color_style("color_code", type = tx_plot_ink_family(theme, channel), theme = theme)
}

# legend_guide_spec() -- the colour legend as a real GGPLOT GUIDE instead of a sentence. Same producers,
# a different medium: legend_specs() -> legend_resolve_spec() -> legend_break_tokens() (which already
# drops a break that renders identically, so under a publication palette the twin ladders collapse for free).
#
# The honest limit: a ggplot has exactly ONE scale per aesthetic, so a key list can describe only one
# ladder. When the plotted columns form several legend body-groups this returns NULL and forest_plot()
# falls back to printing the whole legend in the caption -- the same grouping rule the footer uses.
#
# Returns list(title, keys = data.frame(slot, hex, label), grey_hex, grey_label), or NULL.
#' @keywords internal
legend_guide_spec <- function(x, cols, channel = c("text", "bg"), theme = "light", lang = NULL) {
  channel <- match.arg(channel)
  with_legend_lang(lang, function(lg) {
    specs <- legend_specs(x, theme)
    specs <- Filter(function(s) s$col_name %in% cols, specs)
    if (!length(specs)) return(NULL)
    specs <- lapply(specs, function(s) legend_resolve_spec(s, lg))
    pl_of <- function(s) if (identical(channel, "text")) s$plan_txt else s$plan_bg
    specs <- Filter(function(s) !is.null(pl_of(s)), specs)
    if (!length(specs)) return(NULL)
    if (length(legend_group_by_body(specs, "terse", lg)) > 1L) return(NULL)   # several ladders

    spec <- specs[[1]]
    plan <- pl_of(spec)
    tk   <- legend_break_tokens(plan, spec$is_pct, channel, lg, theme)
    if (!length(tk$over) && !length(tk$under)) return(NULL)
    hex  <- fmt_point_palette(theme, channel)      # what the PLOT paints, not what the table prints
    side <- function(toks, glyph) {
      if (!length(toks)) return(NULL)
      data.frame(slot  = vapply(toks, function(t) as.integer(t$c), integer(1)),
                 label = vapply(toks, function(t) paste0(glyph, "\u00a0", t$t), character(1)),
                 stringsAsFactors = FALSE)
    }
    # strongest OVER at the top, then the under side deepening downwards -- the reading order of a
    # vertical guide beside a forest plot whose x axis runs the same way.
    keys <- rbind(side(rev(tk$over), .lg_ge), side(tk$under, .lg_le))
    keys$hex <- hex[keys$slot]
    # A palette whose two directions render the SAME swatch (a publication one) would produce duplicate
    # keys. Merge them: one swatch, both thresholds (the direction is read off the axis anyway).
    if (anyDuplicated(keys$hex)) {
      keys <- do.call(rbind, lapply(unique(keys$hex), function(h) {
        k <- keys[keys$hex == h, , drop = FALSE]
        data.frame(slot = k$slot[1], label = paste(k$label, collapse = " / "), hex = h,
                   stringsAsFactors = FALSE)
      }))
    }
    ch  <- if (identical(channel, "text")) spec$txt else spec$bg
    # the MEASURE names the guide, not the subject word (a two-channel table would say "Cells vs the
    # Total row" twice). legend_measure_word is the namer: an effect word on a reg column, the measure elsewhere.
    meas <- if (identical(channel, "text")) spec$measure_text else spec$measure_bg
    word <- legend_measure_word(meas, spec$is_std, spec$eff_word, plan$policy, plan$scale_key)
    # the baseline this measure is read against: its OWN, when it has one (the two gap measures name
    # another column), else the column's -- the same two-step legend_tokens_prose() makes.
    rw  <- if (isTRUE(ch$has_ref_lead) && !is.na(ch$ref_lead)) ch$ref_lead else spec$ref_phrase
    # under `guaranteed_effect` the coloured quantity is not the deviation but the part of it the
    # interval guarantees, so the title has to say so -- the grey key ("not guaranteed") already does.
    if (identical(plan$policy, "guaranteed_effect")) {
      cf   <- suppressWarnings(get_conf_level(x[[spec$col_name]])[1])
      word <- if (is.finite(cf)) gettextf("guaranteed (%s%%) %s", format(100 * cf), word)
              else gettextf("guaranteed %s", word)
    }
    list(title = trimws(paste(legend_ucfirst(word),
                              if (is.na(rw) || !nzchar(rw)) "" else gettextf("vs %s", rw))),
         keys = keys, grey_hex = tx_chrome_hex(theme)$grey,
         grey_label = switch(plan$policy,
                             grey_non_signif   = gettext("not significant"),
                             guaranteed_effect = gettext("not guaranteed"),
                             gettext("below the first threshold")))
  })
}

# enc2utf8 guards the gettext catalog output. "runs" -> run-lists (Excel / plot), else a char vector.
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
#'   hex, because a tabxplor stylesheet ships with the output (`tab_html()`). Then the
#'   legend follows a theme toggle exactly like the cells it describes. `FALSE` (the kableExtra engine,
#'   which carries no stylesheet of ours) keeps inline hex.
#' @return A character vector (or, for "runs", a list of run-lists), or NULL when nothing is coloured.
#' @keywords internal
tab_color_legend <- function(x, medium = c("console", "html", "md", "runs", "plain"),
                             style = NULL, lang = NULL, colored = TRUE,
                             theme = NULL, classes = FALSE) {
  medium <- match.arg(medium)
  if (is.null(style))      style      <- if (identical(medium, "console")) "terse" else "prose"
  if (is.null(theme))      theme      <- tx_theme_option("console")
  streams <- legend_streams(x, style, lang, theme)
  if (length(streams) == 0) return(NULL)
  render_streams(streams, medium, theme, colored, classes)
}

# run f(lg) with LANGUAGE set for the gettext lookups (flushing glibc's cache before/after). Shared by
# the plain-text footer helpers (stars / weight legend), which are not coloured.
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

# DESIGN: every footer line is a TOKEN STREAM -- no plain-vs-legend split, because legend_render_line()
# renders uncoloured tokens too, so a plain one-liner is a 1-token stream. One renderer (render_footer)
# covers the whole footer; `role` only picks the console subtle.
# THE ordered below-table footer, as a list of typed token-streams: weight -> Model: -> colour-legend
# group(s) -> stars -> user subtext. Each stream carries a `role` so render_footer() can subtle the
# plain lines whole while a legend keeps its colours. `subtext` = the user subtext lines
# (backend-specific); `legend = FALSE` drops the colour legend.
# the legend style for EXPORTS (md / html / Excel). Default "prose"; options(tabxplor.legend_style =
# "terse") gives the compact one-liner. The console always uses "terse". Any value but "terse" -> "prose".
legend_export_style <- function() {
  if (identical(tx_option("legend_style"), "terse")) "terse" else "prose"
}

tab_footer_streams <- function(x, style = "prose", lang = NULL,
                               subtext = character(0), legend = TRUE, theme = "light") {
  lg      <- legend_resolve_lang(lang)
  streams <- list()
  push <- function(tokens, role) if (length(tokens))
    streams[[length(streams) + 1L]] <<- list(tokens = tokens, role = role)
  wl <- tab_weight_line(x, lang = lg);   if (!is.null(wl)) push(list(.lg_tok(wl)), "weight")
  for (rl in reg_model_lines(x, lg)) if (nzchar(rl)) push(list(.lg_tok(rl)), "reg")  # translated per family
  # the aggregated effect-modification test (predictor x tab_vars) -- table-wide, so it rides the stream
  # footer like the weight / Model: lines. `esc = TRUE`: the p-values carry stars pandoc would read as emphasis.
  for (il in reg_interaction_lines(x, lg)) if (nzchar(il)) push(list(.lg_tok(il, esc = TRUE)), "reg")
  # (the per-predictor global test is footer ROWS -- see reg_footer_plan() -- not a line here.)
  if (isTRUE(legend)) for (toks in legend_streams(x, style, lg, theme)) push(toks, "legend")
  # `esc = TRUE` -> the md renderer escapes the `*` glyphs (else pandoc reads them as emphasis).
  sl <- suppressWarnings(tab_stars_legend(x, lang = lg, theme = theme))
  if (!is.null(sl)) push(list(.lg_tok(sl, esc = TRUE)), "stars")
  for (s in subtext) if (nzchar(s)) push(list(.lg_tok(s)), "subtext")
  streams
}

# render the footer streams for one medium. Console applies the "# " subtle prefix per line, role-aware:
# a legend keeps its coloured break-words (only the prefix is subtle), every other line is subtle whole.
# Other media return the rendered character vector (md/html/plain) or run-lists (runs); the caller places them.
render_footer <- function(streams, medium, theme = NULL, colored = TRUE, classes = FALSE) {
  # the theme scope is derived from the MEDIUM (only the console footer belongs to the console palette),
  # read through tx_theme_option() (R/tab-css.R).
  if (is.null(theme))
    theme <- tx_theme_option(if (identical(medium, "console")) "console" else "export")
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

# The null a regression `Constant` row's star is tested against, or NA when no column TESTS that row.
# "Constant" is the SKELETON's own untranslated key, not a label, so this reads a stored fact. NA too
# when the starred model columns disagree about their null -- a mixed table cannot name one number.
#' @keywords internal
tab_constant_null <- function(x, cols) {
  if (!tab_is_reg(x)) return(NA_real_)
  ax <- x[["var"]]
  if (is.null(ax)) return(NA_real_)
  cst <- as.character(ax) == "Constant"
  cst[is.na(cst)] <- FALSE
  if (!any(cst)) return(NA_real_)
  n <- unique(unlist(purrr::map(cols, function(cl) {
    # ⚠ the test is a finite P-VALUE, not a finite estimate: a `marginal` / `at_reference` Constant
    # holds a predicted BASELINE, which carries no p-value and takes no star, so it has no null to
    # name; an ordinal fit has no intercept at all and shows nothing there.
    if (!identical(get_role(cl), "model")) return(NULL)
    if (!any(is.finite(get_pvalue(cl)[cst]))) return(NULL)
    fmt_scale_row(cl)$neutral
  })))
  n <- n[!is.na(n)]
  if (length(n) == 1L) n else NA_real_
}

# the significance-stars legend line, shown when any DISPLAYED, star-applicable fmt column carries a
# star (never on a contrib table -- fmt_stars_applicable). Thresholds/labels come from the same options
# get_stars() reads, so the named confidence levels match the glyphs drawn. Returns one plain string or NULL.
# A publication palette that MARKS its cells prints no star at all (fmt_cell_suffix), so it prints no
# stars legend either -- the marks are explained by the break-words they ride on.
tab_stars_legend <- function(x, lang = NULL, theme = NULL) {
  if (print_palette_marks(print_palette_of(tx_palette_theme(theme)))) return(NULL)
  cols <- purrr::keep(x, ~ is_fmt(.) && fmt_stars_applicable(.))
  if (length(cols) == 0) return(NULL)
  if (!any(vapply(cols, function(cl) any(nzchar(get_stars(cl))), logical(1)))) return(NULL)
  with_legend_lang(lang, function(lg) {
    ladder <- tx_stars_ladder()
    lev  <- sort(unname(ladder))                                              # ascending p
    lab  <- names(ladder)
    lab  <- lab[order(nchar(lab), decreasing = TRUE)]                          # most stars first
    conf <- (1 - lev) * 100                                                    # aligned: *** <-> 99%
    semi <- if (identical(lg, "fr")) " ; " else "; "
    # ONE sentence for every table. A regression's `Constant` row is the exception -- its star tests
    # the baseline value against the measure's own null -- so it is a parenthesis, appended only where such
    # a row exists, naming the null EST_SCALES declares (1 on a ratio, 0 on a difference).
    nul   <- tab_constant_null(x, cols)
    first <- if (is.na(nul)) gettextf(
      "%s: significantly different from the reference category (in bold) at the %s%% confidence level",
      lab[1], legend_num(conf[1], lg))
    else gettextf(
      "%s: significantly different from the reference category (in bold) at the %s%% confidence level (from %s for the Constant)",
      lab[1], legend_num(conf[1], lg), legend_num(nul, lg))
    rest <- if (length(lab) > 1)
      vapply(2:length(lab), function(i) gettextf("%s: at the %s%% level", lab[i],
                                                 legend_num(conf[i], lg)), character(1))
    else character(0)
    none <- gettext("no star: not significant")
    enc2utf8(paste0(paste(c(first, rest, none), collapse = semi), "."))
  })
}

# the weight footer line, shown FIRST when the table was built with a weight (NULL when unweighted).
# ONE sentence per INFERENCE BASIS, generated from the stored basis -- so the claim cannot outlive the
# computation, and a weighted estimate on a raw-n interval (the DEFAULT) is stated, not silent.
tab_weight_line <- function(x, lang = NULL) {
  wt <- get_vars_attr(x)$wt
  if (is.null(wt) || length(wt) == 0L || is.na(wt) || !nzchar(wt))
    wt <- tryCatch(reg_call(x)$wt, error = function(e) NULL)
  if (is.null(wt) || length(wt) == 0L || is.na(wt) || !nzchar(wt)) return(NULL)
  wt    <- as.character(wt)[1]
  # the basis is a STORED fact, read through its one resolver -- and derived from the COLUMNS, so the
  # sentence survives every rebuild that keeps them.
  basis <- tryCatch(tab_inference_basis(x), error = function(e) "n")
  # `.svy_weights` is the INTERNAL name of a design's sampling weights and must never be printed. This
  # only fires when a design table's stored inference was lost -> drop the line (missing-metadata
  # contract), never invent a claim about the intervals.
  if (identical(wt, svy_wt_col) && !basis %in% c("design", "design_partial")) return(NULL)
  with_legend_lang(lang, function(lg) enc2utf8(switch(
    basis,
    "design" = gettext(
      "Design-based (survey): weighted estimates, intervals and tests account for the sample design."),
    "design_partial" = gettext(
      "Design-based (survey) estimates; this table's design variance could not be computed, so its intervals account for the weighting only."),
    "weights" = gettextf(
      "Weighted by %s; confidence intervals and tests account for the weighting.", wt),
    gettextf("Weighted by %s; confidence intervals and tests use the unweighted sample size.", wt)
  )))
}

# the level -> palette-slot mapping lives with the break scales (mk_color_scale() precomputes
# over$slots / under$slots, R/tab_classes.R); fmt_color_plan() reads them, fmt_color_slots() folds.

#' @keywords internal
get_reference_base <- function(x, mode = c("cells", "lines", "all_totals")) {
  # `pct_type` says which AXIS this column's reference lies on (a row percentage compares against a
  # reference ROW; a column percentage against a reference COLUMN), and `var_kind` says what the column
  # summarises. Together they are the two questions this function asks.
  base        <- get_pct_type(x)
  vkind       <- fmt_var_kind(x)
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
  is_rm  <- base == "row" | vkind == "mean"  # scalar: row%/mean share the reference logic (a ROW)
  is_col <- base == "col"
  is_all <- base == "all"
  is_all_tabs <- base == "all_tabs"
  comp_t <- isTRUE(comp_all)             # NA-safe scalar branch selectors: comp_all may be NA,
  comp_f <- isFALSE(comp_all)            #   then neither arm fires -> `none` (as the old case_when)
  m      <- mode[1]

  # DESIGN: base boolean composition -- every branch selector (pct_type/var_kind/comp_all/ref/color/
  # totcol/refcol) is a SCALAR column attribute, so each really selects ONE arm; the arms are per-cell
  # boolean of the subsettable field masks. Subset-equivalence is relied on by format()'s .ref
  # memoization: get_reference(x[mask], mode) == get_reference(x, mode)[mask].
  # measure_key() is THE spelling normaliser (stored "OR", table key "or"). Kept a plain lookup (hot path).
  if (identical(measure_key(color), "odds_ratio")) {
    switch(m,
           "cells" = ,                                      # cells and lines identical for OR
           "lines" = if      (is_rm && comp_f) refrows
                     else if (is_rm && comp_t) tottab_ref
                     else if (is_col)          rep(refcol, n)
                     else                      none,
           "all_totals" = if      (is_rm && ref == "tot" && comp_f) totrows | refcol
                          else if (is_rm && ref == "tot" && comp_t) tottab_line | refcol
                          else if (is_col && ref == "tot")          totrows | refcol
                          else if (is_rm && comp_f)                 refrows | refcol
                          else if (is_rm && comp_t)                 tottab_ref | refcol
                          else if (is_col)                          refrows | refcol
                          else                                      none
    )

  } else if (ref == "tot") {
    switch(m,
           "cells" = if      (is_rm && comp_f)    totrows & !totcol
                     else if (is_rm && comp_t)    tottab_line & !totcol
                     else if (is_col)             totcol & !totrows
                     else if (is_all)             totrows & totcol
                     else if (is_all_tabs)        tottab_line & totcol
                     else                         none,
           "lines" = if      (is_rm && comp_f)    totrows
                     else if (is_rm && comp_t)    tottab_line
                     else if (is_col)             rep(totcol, n)
                     else if (is_all)             totrows & totcol
                     else if (is_all_tabs)        tottab_line & totcol
                     else                         none,
           "all_totals" = if (vkind == "count" || is_col || is_all || (is_rm && comp_f)) totrows | totcol
                          else if (is_all_tabs || (is_rm && comp_t))                    tottab_line | totcol
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
                     else if (is_col)          refcol & !totrows
                     else                      none,
           "lines" = if      (is_rm && comp_f) refrows
                     else if (is_rm && comp_t) tottab_ref
                     else if (is_col)          rep(refcol, n)
                     else                      none,
           "all_totals" = if      (is_rm && comp_f) refrows | totcol
                          else if (is_rm && comp_t) tottab_ref | totcol
                          else if (is_col)          totrows | refcol
                          else                      none
    )
  }
}

# DESIGN: a GAP measure compares each cell to ANOTHER COLUMN, and that column is marked `refcol` --
# the crude column under `adjustment`, the reference group's block under `between_groups`. It is
# uncoloured by construction (its own `obs` is empty), so the reading anchor is what tells a reader
# which column the shades are measured from -- the same job `refcol` already does under an odds
# ratio, one measure over. The gate is the DECLARED predicate `measure_own_ref()`, so a new
# baseline-in-another-column measure needs no edit here. "cells" is left alone: this is a whole-column
# baseline, not the cell a difference is taken against, so no "ref:" label appears.
get_reference <- function(x, mode = c("cells", "lines", "all_totals")) {
  m   <- match.arg(mode)
  out <- get_reference_base(x, m)
  if (m == "cells" || !isTRUE(is_refcol(x))) return(out)
  gap <- any(vapply(c(get_color(x), get_color_bg(x)), function(k) {
    mk <- measure_key(k)
    !is.na(mk) && nzchar(mk) && measure_own_ref(mk)
  }, logical(1)))
  if (gap) out | TRUE else out
}

# fmt_ref_cells() -- THE cells a column compares against, ROLE-AWARE. `get_reference()` answers on
# the crosstab's percentage axis and so returns nothing at all on a regression column, whose baseline
# is `in_refrow` -- every producer stamps it. Twin of format()'s own ref_base() (the `all_totals`
# variant, which additionally keeps the reading anchors full-strength); the two must agree about
# WHICH rule applies to a column, never about how many total cells it folds in.
#' @keywords internal
fmt_ref_cells <- function(x, .ref = NULL) {
  if (nzchar(as.character(get_role(x))[1])) return(is_refrow(x))
  if (!is.null(.ref)) .ref else get_reference(x, "cells")
}









# Shared body of vec_ptype_abbr/vec_ptype_full: the column's own name (fmt_display_label()), which
# names the asides as well as the primary -- "<row% (n)>", "<OR (row%)>". The two callers differ only
# by the `prefix` ("" for abbr, "fmt-" for full) and by `footer_collapse` (a column whose data cells
# and footer row disagree shows the DATA cells' name in the abbr, "mixed" in the full type).
fmt_ptype_label <- function(x, prefix, pct_pvalue_collapse) {
  paste0(prefix, fmt_display_label(x, "tag", footer_collapse = pct_pvalue_collapse))
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
  # DESIGN: common ptype of two fmt columns -- drives EVERY c() / vec_c() / bind / group. Any per-column
  # attribute that differs collapses to its DECLARED neutral (`fmt_attr_rules`): binding unlike fmt
  # columns is allowed but loses the mismatched metadata, by design.
  fmt_ptype_attrs(fmt_attrs_merge(fmt_attrs_of(x), fmt_attrs_of(y)))
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
# the FIELDS come from `x` (fmt_data_wn = vec_data() with get_wn()'s NA -> n fixup), every per-column
# ATTRIBUTE unconditionally from `to`. do.call() by exact name => no partial match.
vec_cast.tabxplor_fmt.tabxplor_fmt  <- function(x, to, ...)
  do.call(new_fmt, c(fmt_data_wn(x), fmt_attrs_of(to)))

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
  do.call(fmt, c(list(n = NA_integer_, display = "wn", wn = x), fmt_attrs_of(to)))
#' Convert fmt into double
#' @param x A fmt vector
#' @param to A double vector
#' @param ... Other parameter.
#' @return A double vector
#' @method vec_cast.double tabxplor_fmt
#' @export
#' @keywords internal
vec_cast.double.tabxplor_fmt  <- function(x, to, ...) get_num(x) |> as.double()

#' Convert integer into fmt
#' @param x A integer vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return A fmt vector
#' @export
#' @keywords internal
vec_cast.tabxplor_fmt.integer <- function(x, to, ...)
  do.call(fmt, c(list(n = x), fmt_attrs_of(to)))
#' Convert fmt into integer
#' @param x A integer vector
#' @param to A fmt vector
#' @param ... Other parameter.
#' @return An integer vector
#' @method vec_cast.integer tabxplor_fmt
#' @export
#' @keywords internal
vec_cast.integer.tabxplor_fmt    <- function(x, to, ...) get_num(x) |> as.integer()

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

#Arithmetic operations :

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
}


# DESIGN: fmt + fmt arithmetic operates on n, wn, pct fields. For means, recalculates
#   weighted mean. Resets diff/ci/ctr to NA (must be recomputed via tab_pct/tab_ci/tab_chi2).
#' @describeIn vec_arith.tabxplor_fmt vec_arith method for fmt + fmt
#' @return A fmt vector
#' @method vec_arith.tabxplor_fmt tabxplor_fmt
#' @export
vec_arith.tabxplor_fmt.tabxplor_fmt <- function(op, x, y, ...) {
  # the per-column attributes are reconciled ONCE, by the declared `fmt_attr_rules` (`arith` column),
  # and spliced into both arms. `[[` not `$`: a list `$` PARTIAL-matches (comp -> comp_all).
  ax    <- fmt_attrs_of(x)
  ay    <- fmt_attrs_of(y)
  attrs <- fmt_attrs_arith(ax, ay)
  col_var_x    <- ax[["col_var"]]
  # the warning is about mixing percentage BASES, or a percentage with a mean -- the kind label, not
  # the estimate scale. Adding a row% column to its own difference column is legitimate and stays silent.
  same_type    <- fmt_kind_label(x) == fmt_kind_label(y)
  same_col_var <- col_var_x == ay[["col_var"]]
  same_comp    <- ax[["comp_all"]] == ay[["comp_all"]] |
    (is.na(ax[["comp_all"]]) & is.na(ay[["comp_all"]]))
  l            <- length(x)
  rep_NA_real  <- rep(NA_real_, l)

  if (!same_type) warning("operation ", op,
                          " over columns with different pct types, ",
                          "or mixing pct and means (",
                          fmt_kind_label(x), "/", fmt_kind_label(y), ")")
  # isFALSE, not `!`: `same_comp` is THREE-valued (comp_all is NA on a count column, so a count + a
  # pct gives NA) and a bare `if (!NA)` ERRORS -- adding a count column to a percentage one aborted
  # instead of warning. The reconcile itself has always been NA-safe (rule "comp3").
  if (isFALSE(same_comp)) warning("operation ", op,
                          " may mix calculations made on tabs and calculations ",
                          "made on all tabs (different 'comp_all')")
  if (!same_col_var) warning("operation ", op,
                             " over columns belonging to different variables(",
                             col_var_x , "/", ay[["col_var"]], ")")

  switch(
    op,
    "+" = ,
    "-" = do.call(new_fmt, c(list(
      display = get_display(x),
      n       = vctrs::vec_arith_base(op, get_n(x)  , get_n(y)  ),
      wn      = vctrs::vec_arith_base(op, get_wn(x) , get_wn(y) ),
      # a sum of two ROW percentages is a percentage of the same base; a sum of two column / mean /
      # count columns is not.
      pct     = if (same_type & ax[["pct_type"]] %in% c("row", "all", "all_tabs")) {
        tidyr::replace_na(vctrs::vec_arith_base(op, get_pct(x), get_pct(y)), NA_real_)
      } else {
        rep_NA_real
      },
      diff    = rep_NA_real,
      ratio   = rep_NA_real,
      digits  = pmax(get_digits(x), get_digits(y)),
      ctr     = rep_NA_real,
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

      # DESIGN: `+` / `-` take two SYMMETRIC operands, so the three row facts survive only where the
      # operands AGREE: the sum of a total-row cell and a data cell sits in no row kind, no reference
      # row, no single sub-table. `*` / `/` answer the same question asymmetrically (below), by design.
      row_kind  = dplyr::if_else(get_row_kind(x) == get_row_kind(y), get_row_kind(x), "data"),
      in_refrow = is_refrow(x) & is_refrow(y),
      in_tottab = is_tottab(x) & is_tottab(y)
    ), attrs)),
    "/" = ,
    "*" = do.call(new_fmt, c(list(
      display   = get_display(x),
      n      = get_n(x)   ,
      wn     = get_wn(x)  ,
      # DESIGN: this arm is fmt-BY-fmt only (`fmt * 2` returns a bare numeric via .default), so both
      # operands are cells. `x * y` / `x / y` read "x per y": x is the SUBJECT, so every metadata fact
      # is taken from x (where `+`/`-` require agreement). `mean` is dropped (a product/ratio of two
      # means is not a mean; a real ratio is the leaf's `ratio` field). This owes a user only that
      # `mutate(a = x / y)` returns a well-formed fmt column instead of erroring.
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

      row_kind  = get_row_kind(x),
      in_refrow = is_refrow(x),
      in_tottab = is_tottab(x)
    ), attrs)),
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
  # ONE vector in, one out -- nothing to reconcile, so every per-column attribute is carried whole from
  # `.x`. The scale is read off the same list, not through a second getter call.
  am   <- fmt_attrs_of(.x)
  scl  <- am[["scale"]]
  base <- am[["pct_type"]]
  if (!is.na(scl) && scl == "mixed") warning(
    "operation ", .fn,
    " within a variable mixing different types of percentages"
  )

  switch(.fn,
         "sum" = do.call(new_fmt, c(list(display   = get_display(.x)[1],
                         digits = min(get_digits(.x)),
                         n      = vctrs::vec_math_base(.fn, get_n(.x)  , ...),
                         wn     = vctrs::vec_math_base(.fn, get_wn(.x) , ...),
                         pct    = ifelse(! base %in% c("row", "col"),
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

                         row_kind  = if (length(unique(get_row_kind(.x))) == 1L)
                           get_row_kind(.x)[1] else "data",
                         in_refrow = all(is_refrow(.x)),
                         in_tottab = all(is_tottab(.x)) #any ?
         ), am)),
         "mean" = do.call(new_fmt, c(list(display = get_display(.x)[1],
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

                          row_kind  = "data",
                          in_refrow = FALSE,
                          in_tottab = all(is_tottab(.x)) #any ?
         ), am)),
         vctrs::vec_math_base(.fn, get_num(.x), ...) )
}


