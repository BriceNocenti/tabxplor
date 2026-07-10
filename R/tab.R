# PURPOSE: Main user-facing API for cross-tabulation.
# ROLE: tab() and tab_many() are thin wrappers over the internal engine tab_build() (Phase 6);
#   plus the pipeline workers tab_plain(), tab_num(), tab_prepare(), the shared finalize helper
#   tab_apply_tests(), tab_add_n_pct(), and the superseded step functions
#   (tab_pct, tab_ci, tab_chi2, tab_tot, tab_totaltab, tab_spread).
# KEY CONSTRAINTS:
#   - tab_plain()/tab_num() use data.table internally for aggregation speed.
#     Column names are temporarily renamed to avoid DT conflicts, then restored.
#   - tab() and tab_many() BOTH call tab_build(); they differ only in the default `output`
#     shape (tab() merges >=2 row_vars; tab_many() keeps a list). tab_build() reads no options.
#   - tab_prepare() runs ONCE on the whole DB (prep -> aggregate -> transform -> assemble seam,
#     the granularity Phase 10 Jamovi caching drives). Do only per-table work per row_var.
#   - The row_var axis is globalised on tab() (OR/pct/color/comp/ci/chi2/ref2 scalar); ref is a
#     named/ordered per-row_var vector; the col_var axis stays flexible (pct/levels/digits).
#   - All public function signatures are part of CRAN API — deprecate before changing.
# See: CLAUDE.md § Phase 6 and dev/tabxplor_architecture.md § Calculation Pipeline.

#Import data.table in NAMESPACE :
#' Internal data.table methods
#' @import data.table
#' @keywords internal
#' @name tabxplor-data.table
NULL


# To possibly add :
# #            - choose to print % sign or not
# #            - supplementary total with unweighted counts by rows ?
# #            - rename variables if "NA", "NULL", "Total", "Ensemble", "no_var", etc.
# #            - unweighted counts in the title of each graph.
# #            - error when after cleannames, two levels have the same name ("P6Q_27-OQ-A aliment PME" / "P6Q_28-OQ-A aliment PME")
# #            - error with empty tabs when calculating Chi2

# #' @examples
# #' tab(forcats::gss_cat, marital, race)
# #'
# #' tab(forcats::gss_cat, marital, race, perc = "row")
# #'
# #' tab(forcats::gss_cat, marital, race, year, perc = "row")
# #'
# #' dplyr::storms %>%
# #'   tab(status, category) %>%
# #'   tab_sup(sup_rows = c("pressure", "wind"), print_sup = TRUE)
# #'
# #' \donttest{
# #' forcats::gss_cat %>%
# #'   tab(marital, race, perc = "row") %>%
# #'   tab_xl()
# #' }
# #'
# #' # To program several tables with different parameters at the same time :
# #' purrr::pmap(
# #'   tibble::tribble(
# #'     ~var1    , ~var2       ,  ~perc,
# #'     "marital", "race"      ,  "no" ,
# #'     "marital", "race"      ,  "row",
# #'     "marital", "race"      ,  "col",
# #'     "relig"  , "race"      ,  "no" ,
# #'     "relig"  , "race"      ,  "row",
# #'     "relig"  , "race"      ,  "col",
# #'   ),
# #'   .f = tab,
# #'   data = forcats::gss_cat, sort_by = c("White", "desc")) #%>%
# #' #tab_xl(only_one_sheet = TRUE)
# tab_last <- function() {"Nothing"}


# MAIN USER-FRIENDLY FUNCTIONS ###########################################################


#' Single cross-table, with color helpers
#' @description A full-featured function to create, manipulate and format single
#' cross-tables, using colors to make the printed tab more easily readable
#' (in R terminal or exported to Excel with \code{\link{tab_xl}}).
#' Since objects of class \code{tabxplor_tab} are also of class \code{tibble}, you can then use all
#' \pkg{dplyr} verbs to modify the result, like \code{\link[dplyr:select]{select}},
#' like \code{\link[dplyr:arrange]{arrange}}, \code{\link[dplyr:filter]{filter}}
#' or \code{\link[dplyr:mutate]{mutate}}.
#' Wrapper around the more powerful \code{\link{tab_many}}.
#' @param data A data frame.
#' @param row_vars,col_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The row variable(s),
#'  printed with one level per line, and the column variable(s), printed with one level per
#'  column. For numeric variables means are calculated, in a single column. Each accepts one
#'  variable or several (e.g. \code{c(var1, var2)}); with several \code{row_vars} the mirror
#'  tables are merged into one by default (see \code{output_list}).
#' @param row_var,col_var `r lifecycle::badge("deprecated")` Singular aliases of
#'  \code{row_vars}/\code{col_vars} (which now accept several variables). Kept working.
#' @param tab_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
#' a subtable is made for each combination of levels of the selected variables.
#' Leave empty to make a simple cross-table. All \code{tab_vars} are converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param sup_cols `r lifecycle::badge("deprecated")` Supplementary columns variables, with
#' only the first level printed. Deprecated in 1.4.0: pass these columns in \code{col_vars} and
#' set \code{levels = "first"} instead (\code{col_vars} already accepts several variables).
#' @param na The policy to adopt for missing values, as a single string :
#'  \itemize{
#'   \item \code{"keep"}: by default, \code{NA}'s of row, col and tab variables
#'   are printed as an explicit `"NA"` level.
#'   \item \code{"drop"}: remove `NA`'s in each row, col and tab variable before calculations,
#'   so each column is computed on its own non-missing observations (bases can then differ
#'   between col_vars).
#'   \item \code{"drop_all"}: remove every observation missing on the \code{row_vars}, \strong{any}
#'   \code{col_vars} or a \code{tab_vars}, so all columns share the same base (no `NA` anywhere).
#'   \item \code{"common_base"}: fix a single population -- observations non-missing on the
#'   \code{row_vars} and the \strong{first} \code{col_vars} (and \code{tab_vars}) -- shared by
#'   every column, while secondary \code{col_vars} keep their own `NA`'s as a level within it.
#'   This reproduces the historical \code{tab()} behaviour. Microdata only (not
#'   \code{\link{tab_counts}}).
#'   }
#' @param levels The levels of \code{col_vars} to keep, as a single string or a vector the same
#' length as \code{col_vars} (for finer selections use \code{\link[dplyr:select]{dplyr::select}}) :
#'  \itemize{
#'   \item \code{"all"}: by default, all levels are kept.
#'   \item \code{"first"}: only keep the first level of each \code{col_vars} (handy for compact
#'   summary tables with many indicators).
#'   \item \code{"auto"}: keep the first level when a \code{col_vars} has only two levels, keep all
#'   levels otherwise.
#'   }
#' @param digits The number of digits to print, as a single integer, or an integer vector the
#' same length as \code{col_vars}.
#' @param n_min A single positive integer (default \code{0}, off). A pure display filter applied
#' last: it hides small-base cells without recomputing anything. A row is dropped only when its
#' \emph{largest} base across the column variables is below \code{n_min}; surviving cells whose own
#' base is below \code{n_min} are blanked. Under \code{pct = "col"} the same rule drops weak
#' columns. Total rows/columns, the added-\code{n} row/column and the p-value line are always kept.
#' @param totaltab The total table, if there are subtables/groups
#' (i.e. when \code{tab_vars} is provided) :
#'  \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param tot The totals :
#'  \itemize{
#'   \item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
#'   columns.
#'   \item \code{"row"}: only total rows.
#'   \item \code{"col"}: only total column.
#'   \item \code{"no"}: remove all totals (after calculations if needed).
#'  }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param pct The type of percentages to calculate :
#'  \itemize{
#'   \item \code{"row"}: row percentages.
#'   \item \code{"col"}: column percentages.
#'   \item \code{"all"}: frequencies for each subtable/group, if there is \code{tab_vars}.
#'   \item \code{"all_tabs"}: frequencies for the whole (set of) table(s).
#' }
#' @param ref The reference cell to calculate differences and ratios
#'  (used to print \code{colors}) :
#'  \itemize{
#'   \item \code{"auto"}: by default, cell difference from the corresponding total
#'   (rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
#'   used for `diff` ; cell ratio from the first line (or col) is use for `OR`
#'   (odds ratio/relative risks ratio).
#'   \item \code{"tot"}: totals are always used.
#'   \item \code{"first"}: calculate cell difference or ratio from the first cell
#' of the row or column (useful to color temporal developments).
#'   \item \code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.
#'   \item \code{"regex"}: when `ref` is a string, it it used as a regular expression,
#'   to match with the names of the rows (or columns). Be precise enough to match only one
#'   column or row, otherwise you get a warning message.
#'   \item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
#' }
#' @param ref2 A second reference cell is needed to calculate odds ratios
#' (or relative risks ratios). The first cell of the row or column is used by default.
#' See `ref` above for the full list of possible values.
#' @param comp The comparison level : by subtables/groups, or for the whole table.
#' \itemize{
#'   \item \code{"tab"}: by default, contributions to variance,
#' row differences from totals/first cells, and row confidence intervals for these
#' differences, are calculated for each \code{tab_vars} group.
#'   \item \code{"all"}: compare cells to the general total line (provided there is
#'    a total table with a total row), or with the first line of the total table
#'    when \code{ref = "first"}.
#' }
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios
#'   (for binary variables) or relative risks ratios (for variables with 3 levels
#'   or more).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' @param chi2 Set to \code{TRUE} to calculate Chi2 summaries with \code{\link{tab_chi2}}.
#' Useful to print metadata, and to color cells based on their contribution to variance
#'  (\code{color = "contrib"}). Automatically added if needed for \code{color}.
#' @param ci The type of confidence intervals to calculate, passed to \code{\link{tab_ci}}
#'  (automatically added if needed for \code{color}).
#'   \itemize{
#'    \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'    \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'    relative total cell (or relative first cell when \code{ref = "first"}).
#'    \item \code{"auto"}: \code{ci = "diff"} for means and row/col percentages,
#'      \code{ci = "cell"} for frequencies ("all", "all_tabs").
#'   }
#'  By default, for percentages, with Wilson's method is used,
#'  and with \code{ci = "diff"} Wald's method along Agresti and Caffo's adjustment.
#'  Means use classic method. This can be changed in \code{\link{tab_many}}. By
#'  default, with \code{ci = "cell"}, the result is printed in the `[inf;sup]` form.
#'  Set `options("tabxplor.ci_print" = "moe")` to print `pct +- moe` instead.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
#' @param stars Logical (default \code{TRUE}). With \code{ci = "diff"}, print significance stars
#' for each cell's difference from its reference, read from the displayed interval itself
#' (universal CI-inclusion). \code{NULL} uses `options("tabxplor.stars")`. See \code{\link{tab_many}}.
#' @param method_cell,method_diff Character strings choosing the confidence-interval method for
#' \code{ci = "cell"} (\code{"wilson"} default, or \code{"wald"}) / \code{ci = "diff"}
#' (\code{"newcombe"} default, \code{"ac"} or \code{"wald"}). See \code{\link{tab_many}}.
# @param ci_visible By default, confidence intervals are calculated and used to set
# colors, but not printed. Set to \code{TRUE} to print them in the result.
#' @param color Which measure(s) to color, on which visual channel. \code{FALSE} (default)
#' prints no color; \code{TRUE} uses the smart per-column-type scheme (factors: \code{diff} on
#' the text + \code{ratio} on the background; numerics: \code{ratio}; counts: \code{contrib};
#' odds-ratio columns: \code{or}). Otherwise a measure name, on the \strong{text} channel:
#'  \itemize{
#'   \item \code{"diff"}: cell difference from the reference (percentage points for factors;
#'   the standardized difference Glass's \eqn{\Delta} for numeric means).
#'   \item \code{"ratio"}: relative risk (factors) or mean ratio (numerics) vs the reference.
#'   \item \code{"contrib"}: signed contribution to the chi-squared (reference-free).
#'   \item \code{"OR"}: empirical odds ratio (for \code{pct = "row"}/\code{"col"}).
#'  }
#' To color two measures at once, pass a length-2 vector: unnamed \code{c("diff", "ratio")}
#' puts the first on the text channel and the second on the background; named
#' \code{c(text = "diff", background = "ratio")} is explicit; \code{c(background = "ratio")}
#' colors only the background. Only \code{diff} / \code{ratio} may go on the background.
#' Thresholds come from \code{\link{set_color_breaks}}. (The old combined strings
#' \code{"diff_ci"}, \code{"after_ci"} and \code{"ci"} still work but are soft-deprecated in
#' favor of \code{color_signif}.)
#' @param color_signif How significance gates the color, as a single string:
#'  \itemize{
#'   \item \code{"ignore"} (default): color every deviation by its observed size.
#'   \item \code{"grey_non_signif"}: color by the observed size, but grey out cells whose
#'   deviation is not significant at \code{conf_level}.
#'   \item \code{"color_all_signif"}: color by the guaranteed (confidence-bound) effect --
#'   only cells whose interval clears the threshold, with dimmer, conservative colors.
#'  }
#' @param add_n For `pct = "row"` or `pct = "col"`, set to `FALSE` not to add another
#' column or row with unweighted counts (`n`).
#' @param add_pct Set to `TRUE` to add a column with the frequencies of the row
#' variable (for `pct = "row"`) or a row with the frequencies of the column variable
#' (for  `pct = "col"`).
#' @param subtext A character vector to print rows of legend under the table.
#' @param output_list Logical (default \code{FALSE}). With several \code{row_var}, \code{FALSE}
#'  merges the mirror tables into a single \code{tabxplor_tab}; \code{TRUE} returns a list with
#'  one table per \code{row_var}. With \code{tab_vars}, tables stay a list regardless.
#' @param spread_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> A subset of \code{tab_vars}
#'  to pivot from subtables into columns, via \code{\link{tab_spread}} (applied at the end).
#' @param names_prefix,names_sort Passed to \code{\link{tab_spread}} when \code{spread_vars} is
#'  given: a string prefixed to each new column name, and whether to sort the new columns.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like "1-", and text in parenthesis. All data formatting arguments are
#' passed to \code{\link{tab_prepare}}.
#' @param other_if_less_than When set to a positive integer, levels with less count
#' than it will be merged into an "Others" level.
#' @param other_level The name of the "Other" level, as a single string.
#' @param filter A \code{\link[dplyr:filter]{dplyr::filter}} to apply to the data frame
#' first, as a single string (which will be converted to code, i.e. to a call).
#' Useful when printing multiples tabs with \code{\link[tibble:tribble]{tibble::tribble}},
#' to use different filters for similar tables or simply make the field of observation
#' more visible into the code.
#' @param .cache,.defer_level_merge,.return_armed Internal, for the jamovi \code{jmvtab} live cache
#' only: \code{.cache} is a mutable environment the content-addressed multi-tier store is threaded
#' through (Phase 7e); \code{.defer_level_merge} keeps full factor levels through the aggregate and
#' test so \code{levels} becomes a display-time drop; \code{.return_armed} (Phase 7f) returns the
#' pre-\code{finalize_color_spec} table so the tier-3 cache can re-paint colours without a rebuild.
#' All default off; not for direct use.
# @param ... Arguments to pass to \code{\link{tab_ci}} and \code{\link{tab_chi2}}.
#'
#' @inheritSection tab_ci Significance stars
#'
#' @return A \code{tibble} of class \code{tab}, possibly with colored reading helpers.
#' All non-text columns are of class \code{\link{fmt}}, storing all
#' the data necessary to print formats and colors. Columns with \code{row_var} and
#' \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples # A simple cross-table:
#' tab(forcats::gss_cat, marital, race)
#'
# # With one numeric row or col variables it calculates means by category:
# tab(forcats::gss_cat, marital, age)
#'
#' # With more variables provided, `tab` makes a subtables for each combination of levels:
#' \donttest{
#' tab(forcats::gss_cat, marital, tab_vars = c(year, race))
#'}
#'
#' # You can add several col_vars, mixing factors and numeric (means) ; `levels = "first"`
#' # keeps only the first level of each factor col_var for compact summary tables:
#' \donttest{
#' tab(dplyr::storms, category, c(status, pressure, wind))
#'}
#'
#' # Colors to help the user read the table:
#' data <- forcats::gss_cat %>%
#'   dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
#' gss  <- "Source: General social survey 2000-2014"
#' gss2 <- "Source: General social survey 2000, 2006 and 2012"
#'
#' # Differences between the cell and it's subtable's total cell:
#' \donttest{
#' tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff")
#' }
#'
#' # Differences between the cell and the whole table's general total cell:
#' \donttest{
#' tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff",
#'   comp = "all")
#' }
#'
#' # Historical differences:
#' \donttest{
#' data2 <- data %>% dplyr::mutate(year = as.factor(year))
#' tab(data2, year, marital, race, subtext = gss2, pct = "row",
#'     color = "diff", ref = "first", tot = "col")
#'
#'
#' # Differences with the total, except if their confidences intervals are superior to them:
#' tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "diff_ci")
#'
#' # Same differences, minus their confidence intervals:
#' tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "after_ci")
#'
#' # Contribution of cells to table's variance, like in a correspondence analysis:
#' tab(forcats::gss_cat, race, marital, subtext = gss, color = "contrib")
#'}
#'
#' # Since the result is a tibble, you can use all dplyr verbs to modify it :
#' \donttest{
#' library(dplyr)
#' tab(dplyr::storms, category, c(status, pressure, wind)) %>%
#'   dplyr::filter(category != "-1") %>%
#'   dplyr::select(-`tropical depression`) %>%
#'   dplyr::arrange(is_totrow(.), desc(category))
#'}
#'
#'\donttest{
#' # With `dplyr::arrange`, don't forget to keep the order of tab variables and total rows:
#' tab(data, race, marital, year, pct = "row") %>%
#'   dplyr::arrange(year, is_totrow(.), desc(Married))
#'   }
tab <- function(data, row_vars, col_vars, tab_vars, wt, sup_cols,
                pct = "no", color = "no", color_signif = "ignore",
                OR = "no", chi2 = FALSE,
                na = "keep", levels = "all",
                cleannames = NULL, #compact = NULL, # pvalue_line = NULL,
                other_if_less_than = 0, other_level = "Others",
                ref = "auto", ref2 = "first", comp = "tab",
                ci = "no", conf_level = 0.95, stars = NULL,
                method_cell = "wilson", method_diff = "newcombe",
                totaltab = "line", totaltab_name = "Ensemble",
                tot = c("row", "col"), total_names = "Total",
                add_n = TRUE, add_pct = FALSE,
                subtext = "", digits = 0, n_min = 0,
                output_list = FALSE,
                spread_vars, names_prefix = NULL, names_sort = FALSE,
                row_var, col_var,
                .cache = NULL, .defer_level_merge = FALSE, .return_armed = FALSE,
                .levels_order = NULL,
                filter) {

  # Phase 6f (§6): singular row_var/col_var are soft-deprecated aliases of the plural
  # row_vars/col_vars (which now accept one variable OR several). Capture the effective quosure
  # once via enquo() (never evaluate the tidy-select arg), nudging users of the old names.
  .rv_dep <- rlang::enquo(row_var)
  .cv_dep <- rlang::enquo(col_var)
  row_var_quo <- if (!rlang::quo_is_missing(.rv_dep)) {
    lifecycle::deprecate_soft("1.4.0", "tab(row_var = )", "tab(row_vars = )")
    .rv_dep
  } else rlang::enquo(row_vars)
  col_var_quo <- if (!rlang::quo_is_missing(.cv_dep)) {
    lifecycle::deprecate_soft("1.4.0", "tab(col_var = )", "tab(col_vars = )")
    .cv_dep
  } else rlang::enquo(col_vars)

  cleannames <-
    if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}

  # pvalue_line <-
  #   if (is.null(pvalue_line)) { getOption("tabxplor.pvalue_lines") } else {pvalue_line}


  # `row_vars`/`col_vars` accept a <tidy-select> (one variable OR several, e.g. `c(race, relig)`),
  # so tab() can build several mirror tables and merge them by default (§13). row_var_quo /
  # col_var_quo were resolved above (plural name, or the deprecated singular alias).
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- "no_row_var"
  } else {
    row_var <- names(tidyselect::eval_select(row_var_quo, data))
  }

  if (quo_miss_na_null_empty_no(col_var_quo)) {
    data <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_var <- "no_col_var"
  } else {
    col_var <- names(tidyselect::eval_select(col_var_quo, data))
  }

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character()
  } else {
    tab_vars <- names(tidyselect::eval_select(tab_vars, data))
  }

  # Phase 7a: `sup_cols` is soft-deprecated. `col_vars` already accepts several variables, so
  # supplementary columns go there with `levels = "first"`. Kept working during deprecation by
  # folding them into col_vars at levels = "first" (below).
  sup_cols_quo <- rlang::enquo(sup_cols)
  if (quo_miss_na_null_empty_no(sup_cols_quo)) {
    sup_cols <- character()
  } else {
    lifecycle::deprecate_soft(
      "1.4.0", "tab(sup_cols = )",
      details = "Pass these columns in `col_vars` and set `levels = \"first\"`."
    )
    sup_cols <- names(tidyselect::eval_select(sup_cols_quo, data))
  }

  # Phase 6i: spread_vars (a subset of tab_vars) are pivoted to columns at the end via
  # tab_spread(). Resolve against the tab_vars.
  spread_vars_quo <- rlang::enquo(spread_vars)
  if (quo_miss_na_null_empty_no(spread_vars_quo)) {
    spread_vars <- character()
  } else {
    spread_vars <- names(tidyselect::eval_select(spread_vars_quo, data))
    if (!all(spread_vars %in% tab_vars)) {
      cli::cli_abort(c("{.arg spread_vars} must be among the {.arg tab_vars}.",
                       "i" = "Got {.val {setdiff(spread_vars, tab_vars)}}, tab_vars are {.val {tab_vars}}."))
    }
  }

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }

  vctrs::vec_assert(comp, size = 1)
  # Phase 5: `color` accepts FALSE / TRUE / a scalar / c(text, background) / c(text=, background=),
  # so it is NOT size-1-asserted. It is parsed to a spec here; the pipeline runs on the text-channel
  # legacy string, then finalize_color_spec() sets the final color / color_signif attributes.
  color_spec <- normalize_color_spec(color, color_signif)
  color <- color_spec$legacy
  vctrs::vec_assert(pct  , size = 1)
  # Phase 6d (§4): `ref` may be a (named) vector -- one reference row per row_var -- so it is NOT
  # size-1-asserted. tab_build() matches names to row_vars (else by order); scalar applies to all.
  vctrs::vec_assert(ref2, size = 1)
  vctrs::vec_assert(na, size = 1)
  stopifnot(na %in% c("keep", "drop", "drop_all", "common_base"))
  # Phase 7a: `levels` (per col_var) is honoured for the main col_vars (see the tab_build call).
  stopifnot(all(levels %in% c("all", "first", "auto")))

  # Phase 6 (§5): the row_var axis is globalised -- OR/ci/chi2 (like comp/pct/ref/ref2) apply to
  # ALL row_vars. For genuinely different settings per variable, build separate tab()s and list
  # them. (The col_var axis stays flexible: pct/levels/digits are still per col_var in tab_many.)
  vctrs::vec_assert(OR  , size = 1)
  vctrs::vec_assert(ci  , size = 1)
  vctrs::vec_assert(chi2, size = 1)

  # Phase 6g (§4, S3) + Phase 7a: `na` population policy.
  # - "keep": NAs shown as an explicit level.
  # - "drop": each col_var drops its OWN missing values (bases can then differ across col_vars).
  #   Forwarded straight to tab_build (per-table drop in tab_plain/tab_num).
  # - "drop_all": drop every observation missing on the row_var(s), ANY col_var, or a tab_var, so
  #   all columns share one base (no NA anywhere). tab_build resolves na = "drop_all" natively
  #   (it sets na_drop_all = {row_vars, col_vars, tab_vars} internally), so nothing to translate.
  # - "common_base" (the old-tab() behaviour): a SINGLE population -- non-NA on the row_var(s), the
  #   PRIMARY (first) col_var and tab_vars -- shared by every column, while secondary col_vars keep
  #   their own NAs. Mechanically a global drop of {row_var(s), first col_var, tab_vars} + na="keep".
  #   For a single col_var it equals na = "drop".
  na_drop_all <- switch(na,
                        "keep"        = character(),
                        "drop"        = character(),
                        "drop_all"    = character(),
                        "common_base" = c(row_var, col_var[1], tab_vars))
  na_effective <- if (na == "common_base") "keep" else na

  stopifnot(all(tot %in% c("row", "col", "both", "no", "")))
  if (tot[1] == "both") tot <- c("row", "col")


  result <- tab_build(data = data,
           row_vars = tidyselect::all_of(row_var),
           col_vars = tidyselect::all_of(c(col_var, sup_cols)),
           tab_vars = tidyselect::all_of(tab_vars),
           wt = !!wt,
           # Phase 7a: `levels` (per col_var) drives the main col_vars; sup_cols (soft-deprecated)
           # always show their first level. `levels` recycles to length(col_var).
           levels = c(rep(levels, length.out = length(col_var)), rep("first", length(sup_cols))),
           na = na_effective, na_drop_all = tidyselect::all_of(na_drop_all),
           filter = if (missing(filter)) NULL else {{ filter }},
           digits = digits,
           cleannames = cleannames,
           output = if (isTRUE(output_list)) "list" else "single", #pvalue_line = pvalue_line,
           other_if_less_than = other_if_less_than, other_level = other_level,
           totaltab = totaltab, totaltab_name = totaltab_name,
           totrow = "row" %in% tot,
           # Phase 6e (§6): exactly ONE total column by default. With several main col_vars the
           # per-col_var totals are redundant (all equal each row's base for row%, and the
           # row_var marginal for col%), so "last" shows a single total column. For one col_var
           # this is byte-identical to the historical per-col_var total.
           totcol = if ("col" %in% tot) { "last" } else { "no" },
           total_names = total_names,
           pct  = c(rep(pct, length(col_var)), rep("row", length(sup_cols))),
           ref = ref, ref2 = ref2, #c(ref, rep(ref , length(sup_cols))),
           comp = comp,
           chi2 = chi2,
           ci = ci,
           conf_level = conf_level,
           stars = stars,
           method_cell = method_cell, method_diff = method_diff,
           OR = OR,
           color = color,
           add_n = add_n, add_pct = add_pct,
           subtext = subtext, n_min = n_min,
           spread_vars = spread_vars, names_prefix = names_prefix, names_sort = names_sort,
           # Phase 7e: pass the jmvtab live-cache seam straight through (NULL/FALSE for normal tab()).
           # Phase 7g-ii: `.levels_order` (a per-variable named list of ordered levels) is jmvtab-only
           # (NULL for normal tab()); consumed post-aggregate in jmv_cache_aggregate() (design 4e).
           .cache = .cache, .defer_level_merge = .defer_level_merge,
           .levels_order = .levels_order)

  # Phase 7f: the jmvtab tier-3 cache stores the PRE-finalize armed table (field values + the
  # `legacy` colour), then applies finalize_color_spec() itself on every interaction, so a colour /
  # colour-policy toggle is a cheap re-paint of cached fmt cells rather than a rebuild. `.return_armed`
  # returns `result` before the paint; jmvtab_build() owns the same normalize/finalize pair.
  if (isTRUE(.return_armed)) return(result)

  # Phase 5: set the final two-channel color + significance-policy attributes (per column type
  # for color = TRUE). Plain scalar colors pass through untouched.
  finalize_color_spec(result, color_spec)
}


# Phase 5: parse the tab() `color` / `color_signif` arguments into a spec. `color` accepts FALSE,
# TRUE (per-type default scheme), a scalar measure/old-string, an unnamed c(text, background), or
# a named c(text=, background=). Returns list(legacy, per_type, bg, text, signif): `legacy` is the
# scalar string fed to the (text-channel) tab_many pipeline so its ci/chi2 side effects still fire;
# `text`/`bg`/`signif`/`per_type` drive finalize_color_spec() on the built table.
#' @keywords internal
normalize_color_spec <- function(color, color_signif = "ignore") {
  signif <- if (length(color_signif) == 0L) "ignore" else color_signif[1]
  if (is.na(signif) || signif %in% c("", "no")) signif <- "ignore"
  ok_signif <- c("ignore", "grey_non_signif", "color_all_signif")
  if (!signif %in% ok_signif) {
    cli::cli_abort(c("Unknown {.arg color_signif} value {.val {signif}}.",
                     "i" = "Valid: {.val {ok_signif}}."))
  }

  if (is.logical(color)) {
    if (isTRUE(color)) {
      return(list(legacy = "auto", per_type = TRUE, bg = NA_character_, text = "auto", signif = signif))
    }
    return(list(legacy = "no", per_type = FALSE, bg = NA_character_, text = "", signif = "ignore"))
  }

  nms   <- names(color)                       # capture BEFORE as.character() (which drops names)
  color <- as.character(color)
  names(color) <- nms
  if (!is.null(nms) && any(nzchar(nms))) {
    text <- if ("text" %in% nms) unname(color[["text"]]) else ""
    bg   <- if ("background" %in% nms) unname(color[["background"]]) else
            if ("bg" %in% nms) unname(color[["bg"]]) else NA_character_
  } else if (length(color) >= 2L) {
    text <- color[1]; bg <- color[2]
  } else {
    text <- color[1]; bg <- NA_character_
  }
  norm <- function(m) if (is.na(m) || identical(m, "no")) "" else if (identical(m, "or")) "OR" else m
  text <- norm(text); bg <- if (is.na(bg)) NA_character_ else norm(bg)
  if (!is.na(bg) && bg == "") bg <- NA_character_
  if (!is.na(bg) && !bg %in% c("diff", "ratio")) {
    cli::cli_abort("{.val {bg}} cannot go on the background channel (only {.val diff} / {.val ratio}).")
  }

  ok_text <- c("diff", "ratio", "contrib", "OR", "auto", "diff_ci", "after_ci", "ci", "")
  if (!text %in% ok_text) cli::cli_abort("Unknown text color measure {.val {text}}.")

  # Phase 5: the combined color strings are superseded by `color` + `color_signif`
  # ("diff_ci" = diff + grey_non_signif, "after_ci"/"ci" = diff + color_all_signif). They keep
  # working unchanged (the engine decodes them, byte-identical) -- this is only a gentle nudge.
  if (text %in% c("diff_ci", "after_ci", "ci")) {
    lifecycle::deprecate_soft(
      "1.4.0",
      I(paste0("The `color = \"", text, "\"` mode")),
      with = I("`color = \"diff\"` with the `color_signif` argument"),
      # normalize_color_spec() is called by tab()/tab_num(), so the real user is two frames up;
      # this keeps the nudge for user calls but silent for tab_many()'s internal recursion.
      user_env = rlang::caller_env(2)
    )
  }

  legacy <- if (text %in% c("", "no")) {
    if (!is.na(bg)) "diff" else "no"                       # bg-only still needs ref/pct -> "diff"
  } else if (text %in% c("diff", "ratio")) {
    switch(signif, "grey_non_signif" = "diff_ci", "color_all_signif" = "after_ci", "diff")
  } else {
    text                                                    # contrib / OR / auto / old strings
  }

  list(legacy = legacy, per_type = FALSE, bg = bg, text = text, signif = signif)
}

# Apply the color spec to a built table (or a list of tables), rewriting the color / color_signif
# attributes to the clean (measure, policy) model ONLY when a new capability is used (color = TRUE,
# a background channel, an explicit color_signif, or the `ratio` measure). Plain old scalar colors
# pass through untouched (no golden churn; the engine decodes them). color = TRUE resolves per
# column type here (factor -> diff text + ratio bg; numeric -> ratio text; OR cols -> or).
#' @keywords internal
finalize_color_spec <- function(x, spec) {
  if (is.list(x) && !is.data.frame(x)) return(purrr::map(x, ~ finalize_color_spec(., spec)))
  rewrite <- spec$per_type || !is.na(spec$bg) || spec$signif != "ignore" || identical(spec$text, "ratio")
  if (!rewrite) return(x)
  dplyr::mutate(x, dplyr::across(dplyr::where(is_fmt), ~ finalize_one_col(.x, spec)))
}

#' @keywords internal
finalize_one_col <- function(col, spec) {
  built <- get_color(col)
  type  <- get_type(col)
  if (built %in% c("", "no")) return(col)                  # the pipeline did not color this column
  if (spec$per_type) {
    # color = TRUE default scheme, resolved per column type (only where the pipeline already colored)
    if (built == "OR")      return(set_color(col, "OR"))                        # odds-ratio columns
    if (built == "contrib") return(col)                                        # counts/all -> contrib
    if (type == "mean")     return(set_color_signif(set_color(col, "ratio"), spec$signif))  # numeric
    if (type %in% c("row", "col", "all", "all_tabs")) {                        # factor % -> diff + ratio bg
      return(set_color_signif(set_color(col, c("diff", "ratio")), spec$signif))
    }
    return(col)
  }
  # text measure: "auto" -> the measure the pipeline built; "" -> empty (a background-only cell);
  # else the explicit measure.
  text <- if (identical(spec$text, "auto")) color_measure_policy(built, type)$measure else spec$text
  if (text == "" && is.na(spec$bg)) return(col)            # nothing to set
  col <- if (is.na(spec$bg)) set_color(col, text) else set_color(col, c(text, spec$bg))
  set_color_signif(col, spec$signif)
}




# DESIGN (Phase 6): the shared engine is now the internal tab_build(); tab_many() is a thin
# (soft-deprecated) wrapper that keeps the historical list-default. col_vars still share
# pct/color (one table) and stay per-col_var flexible (levels/digits/pct); the row_var axis is
# globalised on tab() (OR/pct/color/comp/ci/chi2/ref2 are scalar there). tab_build() still
# recycles those over row_vars internally, so tab_many()'s legacy per-row_var vectors keep working.
#' Many cross-tables as one, with color helpers
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (1.4.0) by [tab()], the unified entry point (it accepts several row_vars /
#' col_vars). `tab_many()` keeps working and keeps its historical list return for >=2 row_vars
#' (tab() merges them by default; pass `output_list = TRUE` for a list).
#'
#' A full-featured function to create, manipulate and format many cross-tables
#' as one, using colors to make the printed tab more easily readable (in R terminal or
#' exported to Excel with \code{\link{tab_xl}}).
#' Since objects of class \code{tabxplor_tab} are also of class \code{tibble}, you can then use all
#' \pkg{dplyr} verbs to modify the result, like \code{\link[dplyr:select]{select}},
#' \code{\link[dplyr:arrange]{arrange}}, \code{\link[dplyr:filter]{filter}}
#' or \code{\link[dplyr:mutate]{mutate}}.
#' @param data A data frame.
#' @param row_vars The row variable, which will be printed with one level per line.
#' If numeric, it will be converted to factor. If more than one row_var if provided,
#' a different table is made for each of them.
#' @param col_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' One column is printed for each level of each column variable.
#' For numeric variables means are calculated, in a single column.
#' To pass many variables you may use syntax \code{col_vars = c(col_var1, col_var2, ...)}.
#' @param tab_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' One subtable is made for each combination of levels of the tab variables.
#' To pass many variables you may use syntax \code{tab_vars = c(tab_var1, tab_var2, ...)}.
#' All tab variables are converted to factor. Leave empty to make a simple table.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param levels The levels of \code{col_vars} to keep (for more complex selections
#'  use \code{\link[dplyr:select]{dplyr::select}}). The argument is vectorised over `col_vars`.
#' \itemize{
#'   \item \code{"all"}: by default, all levels are kept.
#'   \item \code{"first"}: only keep the first level of each \code{col_vars}
#'   \item \code{"auto"}: keep the first level when `col_var` is only two levels,
#'   keep all levels otherwise
#'   }
#' @param na The policy to adopt with missing values. It must be a single string.
#' \itemize{
#'   \item \code{na = "keep"}: by default, prints \code{NA}'s as explicit \code{"NA"} level.
#'   \item \code{na = "drop"}: removes \code{NA} levels before making each table
#'   (tabs made with different column variables may have a different number of
#'   observations, and won't exactly have the same total columns).
#'   \item \code{"drop_all"}: remove `NA`'s for all variables before making the tables.
#'   }
#' @param na_drop_all <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' Removes all observations with a `NA` in any of the chosen variables, for all tables
#' (tabs for each column variable will have the same number of observations).
#' @param digits The number of digits to print, as a single integer, or an integer vector
#' the same length as \code{col_vars}. The argument is vectorisez over `col_vars`.
#' @param n_min A single positive integer (default \code{0}, off). A pure display filter -- see
#' \code{\link{tab}} -- that hides small-base rows/cells (largest base below \code{n_min} drops the
#' row; own base below \code{n_min} blanks the cell) without recomputing anything.
#' @param totaltab The total table, if there are subtables/groups
#'  (i.e. when \code{tab_vars} is provided). Vectorised over `row_vars`.
#' \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param totrow By default, total rows are printed.
#' Set to \code{FALSE} to remove them (after calculations if needed).
#' Vectorised over `row_vars`.
#' @param totcol The policy with total columns. Vectorised over `col_vars`.
#' \itemize{
#'   \item \code{"last"}: by default, only prints a total column for the last
#'   column variable (of class factor, not numeric).
#'   \item \code{"each"}: print a total column for each column variable.
#'   \item \code{"no"}: remove all total columns (after calculations if needed).
#' }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param pct The type of percentages to calculate :
#' \itemize{
#'   \item \code{"row"}: row percentages.
#'   \item \code{"col"}: column percentages.
#'   \item \code{"all"}: frequencies for each subtable/group, if there is \code{tab_vars}.
#'   \item \code{"all_tabs"}: frequencies for the whole (set of) table(s).
#' }
#' The argument is vectorised over both `row_vars` and `col_vars`. You can then write as
#'  the following :
#' `pct = list(row_var1 = list("row", "col", "col"), row_var2 = list("col", "row", "row"))`
#' @param ref The reference cell to calculate differences and ratios
#'  (used to print \code{colors}) :
#'  \itemize{
#'   \item \code{"auto"}: by default, cell difference from the corresponding total
#'   (rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
#'   used for `diff` ; cell ratio from the first line (or col) is use for `OR`
#'   (odds ratio/relative risks ratio).
#'   \item \code{"tot"}: totals are always used.
#'   \item \code{"first"}: calculate cell difference or ratio from the first cell
#' of the row or column (useful to color temporal developments).
#'   \item \code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.
#'   \item \code{"regex"}: when `ref` is a string, it it used as a regular expression,
#'   to match with the names of the rows (or columns). Be precise enough to match only one
#'   column or row, otherwise you get a warning message.
#'   \item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
#' }
#' @param ref2 A second reference cell is needed to calculate odds ratios
#' (or relative risks ratios). The first cell of the row or column is used by default.
#' See `ref` above for the full list of possible values.
#' @param comp The comparison level : by subtables/groups, or for the whole table.
#' Vectorised over `row_vars`.
#' \itemize{
#'   \item \code{"tab"}: by default, contributions to variance,
#' row differences from totals/first cells, and row confidence intervals for these
#' differences, are calculated for each \code{tab_vars} group.
#'   \item \code{"all"}: compare cells to the general total line (provided there is
#'    a total table with a total row), or with the reference line of the total table
#'    when \code{ref = "first"}, an integer or a regular expression.
#' }
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios
#'   (for binary variables) or relative risks ratios (for variables with 3 levels
#'   or more).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' @param chi2 Set to \code{TRUE} to calculate Chi2 summaries with \code{\link{tab_chi2}}.
#' Useful to print metadata, and to color cells based on their contribution to variance
#'  (\code{color = "contrib"}). Vectorised over `row_vars`.
#' @param ci The type of confidence intervals to calculate, passed to \code{\link{tab_ci}}.
#' Vectorised over `row_vars`.
#'   \itemize{
#'    \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'    \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'    relative total cell (or relative first cell when \code{ref = "first"}).
#'    \item \code{"auto"}: \code{ci = "diff"} for means and row/col percentages,
#'    \code{ci = "cell"} for frequencies ("all", "all_tabs").
#'   }
#'  Confidence intervals use fast closed-form methods. For percentages, \code{ci = "cell"}
#'  uses the Wilson score interval and \code{ci = "diff"} the Newcombe method-10 hybrid-score
#'  interval (its dual, so the bracket and the significance stars always agree); means use the
#'  Welch t interval. These can be changed with \code{method_cell} / \code{method_diff}. By
#'  default the interval is printed in the `[inf;sup]` form; set
#'  `options("tabxplor.ci_print" = "moe")` to print `pct +- moe` instead.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
#' @param stars Logical. When \code{TRUE} (the default) and \code{ci = "diff"}, each cell shows
#' significance stars for the difference from its reference (\code{*} p<0.10, \code{**} p<0.05,
#' \code{***} p<0.01, customisable via `options("tabxplor.signif_levels")` /
#' `"tabxplor.signif_labels"`). Significance is read from the same interval that is displayed
#' (universal CI-inclusion), so stars and bracket never disagree. \code{FALSE} skips the
#' significance computation entirely. \code{NULL} uses `options("tabxplor.stars")`.
# @param ci_visible By default, confidence intervals are calculated and used to set
# colors, but not printed. Set to \code{TRUE} to print them in the result.
#' @param method_cell Character string, the proportion confidence-interval method for
#' \code{ci = "cell"}. Either \code{"wilson"} (the score interval, default) or \code{"wald"}
#' (the normal approximation, commonly taught -- degenerate at cell proportions of 0 or 1).
#' @param method_diff Character string, the proportion confidence-interval method for
#' \code{ci = "diff"}. One of \code{"newcombe"} (default, the hybrid-score interval, dual of the
#' two-proportion score test), \code{"ac"} (Agresti-Caffo) or \code{"wald"}. Whatever method is
#' chosen, the stars come from that same interval, so they always agree with the bracket.
#' @param color Which measure(s) to color, on which visual channel -- see \code{\link{tab}}
#' for the full description (\code{FALSE}/\code{TRUE}, a measure such as \code{"diff"}, or a
#' two-channel \code{c(text, background)} / \code{c(text = , background = )}). The old combined
#' strings \code{"diff_ci"}/\code{"after_ci"}/\code{"ci"} still work (superseded by
#' \code{color} + \code{color_signif}). Applies to all \code{row_vars}.
#' @param color_signif How significance gates the color -- see \code{\link{tab}}
#' (\code{"ignore"} / \code{"grey_non_signif"} / \code{"color_all_signif"}).
#' @param add_n For `pct = "row"` or `pct = "col"`, set to `FALSE` not to add another
#' column or row with unweighted counts (`n`).
#' @param add_pct Set to `TRUE` to add a column with the frequencies of the row
#' variable (for `pct = "row"`) or a row with the frequencies of the column variable
#' (for  `pct = "col"`).
#' @param subtext A character vector to print rows of legend under the table.
#' @param compact With several `row_vars`, set to `TRUE` to bind all tables
#' in a single `tabxplor_tab` (`FALSE` by default). The `tabxplor.compact` option has been
#' removed; use the `output_list` argument of [tab()] instead (the unified entry point, which
#' merges by default).
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like "1-", and text in parenthesis. All data formatting arguments are
#' passed to \code{\link{tab_prepare}}.
#' @param other_if_less_than When set to a positive integer, levels with less count
#' than it will be merged into an "Others" level.
#' @param other_level The name of the "Other" level, as a single string.
#' @param filter A \code{\link[dplyr:filter]{dplyr::filter}} to apply to the data frame
#' first, as a single string (which will be converted to code, i.e. to a call).
#' Useful when printing multiples tabs with \code{\link[tibble:tribble]{tibble::tribble}},
#' to use different filters for similar tables or simply make the field of observation
#' more visible into the code.
# @param ... Arguments to pass to \code{\link{tab_ci}} and \code{\link{tab_chi2}}.
#' @param color_signif How significance gates the color -- see \code{\link{tab}}.
#' @param .by_table Internal: force the table-by-table path (disable scan-fusion).
#'
#' @inheritSection tab_ci Significance stars
#'
#' @return A \code{tibble} of class \code{tab}, possibly with colored reading helpers.
#' When there are two `row_vars` or more, a list of \code{tibble} of class \code{tab}.
#' All non-text columns are of class \code{\link{fmt}}, storing all
#' the data necessary to print formats and colors. Columns with \code{row_var} and
#' \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples # Make a summary table with many col_vars, showing only one specific level :
#' \donttest{
#' library(dplyr)
#' first_lvs <- c("Married", "$25000 or more", "Strong republican", "Protestant")
#' data <- forcats::gss_cat %>% mutate(across(
#'   where(is.factor),
#'   ~ forcats::fct_relevel(., first_lvs[first_lvs %in% levels(.)])
#' ))
#' tab_many(data, race, c(marital, rincome, partyid, relig, age, tvhours),
#'          levels = "first", pct = "row", chi2 = TRUE, color = "auto")
#'}
#'
#' # Can be used with map and tribble to program several tables with different parameters
#' #  all at once, in a readable way:
#' \donttest{
#' library(purrr)
#' library(tibble)
#' pmap(
#'   tribble(
#'     ~row_var, ~col_vars       , ~pct , ~filter              , ~subtext               ,
#'     "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
#'     "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
#'     NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
#'   ),
#'   .f = tab_many,
#'   data = forcats::gss_cat, color = "auto", chi2 = TRUE)
#' }
tab_many <- function(data, row_vars, col_vars, tab_vars, wt,
                     pct = "no", color = "no", OR = "no", chi2 = FALSE,
                     na = "keep", levels = "all", na_drop_all,
                     cleannames = NULL, compact = NULL, #pvalue_line = NULL,
                     other_if_less_than = 0, other_level = "Others",
                     ref = "auto", ref2 = "first", comp = "tab",
                     ci = "no", conf_level = 0.95, stars = NULL, #ci_visible = FALSE,
                     method_cell = "wilson", method_diff = "newcombe",
                     totaltab = "line", totaltab_name = "Ensemble",
                     totrow = TRUE, totcol = "last", total_names = "Total",
                     add_n = TRUE, add_pct = FALSE,
                     digits = 0, subtext = "", n_min = 0, color_signif = "ignore",
                     .by_table = FALSE,

                     filter #, listed = FALSE,
                     #spread_vars = NULL, names_prefix, names_sort = FALSE
) {
  # Phase 6f: tab_many() is soft-deprecated in favour of the unified tab(). Silent for
  # same-package callers (e.g. the jmvtab module), so only direct external users are nudged.
  lifecycle::deprecate_soft(
    "1.4.0", "tab_many()", "tab()",
    details = c(
      "i" = paste0("tab() accepts several row_vars / col_vars. It merges >=2 row_vars into one ",
                   "table by default; pass output_list = TRUE for a list (tab_many()'s old default).")
    )
  )

  # tab_many() keeps its historical list-default (one table per row_var; a bare tab for a single
  # row_var) and maps the deprecated `compact` argument onto the shared engine's output shape:
  #   compact = TRUE  -> "single" (bind the row_var tables into one)
  #   compact = FALSE -> "legacy" (list for >=2 row_vars, bare tab for one; historical default)
  # The `tabxplor.compact` option is dropped (§6); compact now defaults to FALSE.
  compact <- if (is.null(compact)) FALSE else compact

  # Phase 6e (§6): totrow / totcol are soft-deprecated. A total row is always computed and
  # exactly one total column is shown by default; both remain purely cosmetic (drop/move with
  # dplyr afterwards). Old totcol values ("each"/"no"/names) still work.
  if (!missing(totrow) && !all(as.logical(totrow))) {
    lifecycle::deprecate_soft(
      "1.4.0", "tab_many(totrow = )",
      details = "A total row is always computed; drop it afterwards with `dplyr::filter(!is_totrow(.))`."
    )
  }
  if (!missing(totcol) && !identical(totcol, "last")) {
    lifecycle::deprecate_soft(
      "1.4.0", "tab_many(totcol = )",
      details = "Exactly one total column is shown by default; move or drop columns with dplyr afterwards."
    )
  }

  # Phase 6c: parse the new color / color_signif forms here too (same one-parse contract as
  # tab()), so tab_many() accepts color = TRUE / c(text, background) / named / a measure +
  # color_signif. Plain scalar strings (incl. jmvtab's) pass through as the legacy color.
  color_spec <- normalize_color_spec(color, color_signif)
  result <- tab_build(
    data = data,
    row_vars = {{ row_vars }}, col_vars = {{ col_vars }}, tab_vars = {{ tab_vars }},
    wt = {{ wt }},
    pct = pct, color = color_spec$legacy, OR = OR, chi2 = chi2, na = na, levels = levels,
    na_drop_all = {{ na_drop_all }},
    cleannames = cleannames, other_if_less_than = other_if_less_than,
    other_level = other_level, ref = ref, ref2 = ref2, comp = comp, ci = ci,
    conf_level = conf_level, stars = stars, method_cell = method_cell,
    method_diff = method_diff, totaltab = totaltab, totaltab_name = totaltab_name,
    totrow = totrow, totcol = totcol, total_names = total_names,
    add_n = add_n, add_pct = add_pct, digits = digits, subtext = subtext, n_min = n_min,
    .by_table = .by_table,
    filter = if (missing(filter)) NULL else {{ filter }},
    output = if (isTRUE(compact)) "single" else "legacy"
  )
  finalize_color_spec(result, color_spec)
}


# Phase 7d-ii: update `ctx` with the fields a stage produced. Uses single-bracket `[<-` so that
# (a) NULL values are PRESERVED as list elements (unlike `ctx$x <- NULL`, which deletes -- which
# would break the downstream list2env() unpack), and (b) data-frame elements are replaced wholesale
# (unlike modifyList(), which recurses and tries to merge tibbles column-by-column).
#' @keywords internal
#' @noRd
ctx_update <- function(ctx, updates) {
  ctx[names(updates)] <- updates
  ctx
}


# tab_build() -- the shared table-building engine behind tab() and tab_many().
# Stages: prep-once (whole DB) -> aggregate -> transform -> assemble. Both public entry points
# are thin wrappers differing only in the default `output` shape they pass. Kept internal (not
# exported) so a future Jamovi caching layer can drive the same core without any deprecation
# nudge, and so tab() never triggers tab_many()'s soft-deprecation.
#   `output`: "single" merges >=2 row_vars into one table (the tab() default); "list" always
#   returns a list, incl. length 1 (tab(output_list = TRUE)); "legacy" returns a list for >=2
#   row_vars and a bare table for one (the tab_many() default). Tables with tab_vars stay a
#   list regardless (merging deferred, §7).
# WARNING: keep byte-identical to the pre-6b tab_many() body except the intended output-shape
# and option changes.
#' @keywords internal
#' @noRd
tab_build <- function(data, row_vars, col_vars, tab_vars, wt,
                      pct = "no", color = "no", OR = "no", chi2 = FALSE,
                      na = "keep", levels = "all", na_drop_all,
                      cleannames = NULL, output = "single", #pvalue_line = NULL,
                      other_if_less_than = 0, other_level = "Others",
                      ref = "auto", ref2 = "first", comp = "tab",
                      ci = "no", conf_level = 0.95, stars = NULL, #ci_visible = FALSE,
                      method_cell = "wilson", method_diff = "newcombe",
                      totaltab = "line", totaltab_name = "Ensemble",
                      totrow = TRUE, totcol = "last", total_names = "Total",
                      add_n = TRUE, add_pct = FALSE,
                      digits = 0, subtext = "", n_min = 0,
                      .by_table = FALSE,
                      spread_vars = character(), names_prefix = NULL, names_sort = FALSE,
                      .cache = NULL, .defer_level_merge = FALSE,
                      .levels_order = NULL,

                      filter #, listed = FALSE,
) {
  # Phase 7d-ii: tab_build is the ARGUMENT SURFACE + the five-stage pipeline. It defuses the NSE
  # args here (where their promises live) and applies `filter` here too -- the string form (for
  # tribble) and the pre-existing bare-expression behaviour must stay in this frame. Each stage
  # takes and returns `ctx`; tab_assemble() returns the final tab/list. The stage split matches the
  # jmvtab cache tiers (dev/tabxplor_jmvtab_cache_design.md §8): setup (-) -> prepare_pop (tier 0)
  # -> aggregate (tier 1) -> transform (tier 3 + the tier-2 test) -> assemble (tier 4).

  # Allow to type expression as string in filter (to work with tibble::tribble)
  with_filter <- FALSE
  if (!missing(filter)) if (! is.null(filter)) {
    filter <- rlang::enquo(filter)
    if (is.character(rlang::get_expr(filter))) filter <- filter %>%
        rlang::get_expr(.) %>% str2lang()

    data <- data %>% dplyr::mutate(.filter = !!filter)
    with_filter <- TRUE
  }

  ctx <- list(
    data = data, with_filter = with_filter,
    row_vars_quo = rlang::enquo(row_vars), col_vars_quo = rlang::enquo(col_vars),
    tab_vars_quo = rlang::enquo(tab_vars), wt_quo = rlang::enquo(wt),
    na_drop_all_quo = rlang::enquo(na_drop_all),
    pct = pct, color = color, OR = OR, chi2 = chi2, na = na, levels = levels,
    cleannames = cleannames, output = output,
    other_if_less_than = other_if_less_than, other_level = other_level,
    ref = ref, ref2 = ref2, comp = comp, ci = ci, conf_level = conf_level, stars = stars,
    method_cell = method_cell, method_diff = method_diff,
    totaltab = totaltab, totaltab_name = totaltab_name, totrow = totrow, totcol = totcol,
    total_names = total_names, add_n = add_n, add_pct = add_pct, digits = digits,
    subtext = subtext, n_min = n_min, by_table = .by_table,
    spread_vars = spread_vars, names_prefix = names_prefix, names_sort = names_sort,
    # Phase 7e jmvtab cache seam: `cache_env` is a mutable environment holding $store / $hits (NULL
    # for tab()/tab_many() -> the hooks below are inert). `defer_level_merge` keeps full levels for
    # a cacheable aggregate + test (see tab_prepare_pop / the design doc). Both are strictly additive.
    cache_env = .cache, defer_level_merge = .defer_level_merge,
    # Phase 7g-ii: jmvtab-only per-variable level reordering (named list var -> ordered levels).
    # NULL for tab()/tab_many() -> no-op. Applied post-aggregate (tier-3) in jmv_cache_aggregate().
    levels_order = .levels_order
  )

  ctx <- tab_setup(ctx)
  ctx <- tab_prepare_pop(ctx)
  ctx <- tab_aggregate(ctx)      # jmvtab: replaced by the cached per-pair build (hook at its top)
  ctx <- tab_transform(ctx)
  # Phase 7e: persist freshly-computed tier-2 tests (cache misses) before display assembly.
  if (!is.null(ctx$cache_env)) jmv_cache_store_tests(ctx)
  tab_assemble(ctx)
}


# === STAGE 1/5: tab_setup() -- resolve & recycle arguments (no cache tier) ===================
# Pure argument resolution shared by all downstream stages: tidy-select the four var roles, the
# factor/numeric masks, the per-row_var and per-col_var arg recycling, totcol -> tot_cols_type,
# pct_vect, and the colour cascade + cache keys via tab_resolve_settings(). Reads only argument
# VALUES + column classes -- the data-free boundary the jamovi .js mirrors (Phase 7c).
#' @keywords internal
#' @noRd
tab_setup <- function(ctx) {
  # Bring every ctx field into scope as a local so the (verbatim) resolution blocks read as before;
  # the NSE args arrive as *_quo quosures (defused in tab_build), aliased to their plain names below.
  list2env(ctx, environment())

  stopifnot(output %in% c("single", "list", "legacy"))

  cleannames <-
    if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}

  # Phase 3a: significance stars default (universal CI-inclusion). NULL -> option default.
  stars <- if (is.null(stars)) getOption("tabxplor.stars", TRUE) else stars

  # pvalue_line <-
  #   if (is.null(pvalue_line)) { getOption("tabxplor.pvalue_lines") } else {pvalue_line}


  stopifnot(levels %in% c("first", "all", "auto"))
  lvs <- levels

  row_vars <- row_vars_quo
  if (quo_miss_na_null_empty_no(row_vars)) {
    data     <- data %>% dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_vars <- rlang::syms("no_row_var")
    pos_row_vars <- tidyselect::eval_select("no_row_var", data)
  } else {
    pos_row_vars <- tidyselect::eval_select(row_vars, data)
    row_vars     <- rlang::syms(names(pos_row_vars))
  }
  # row_vars_num  <- purrr::map_lgl(data[pos_row_vars], is.numeric)
  # row_vars_text <- purrr::map_lgl(data[pos_row_vars],
  #                                 ~ is.factor(.) | is.character(.))

  col_vars <- col_vars_quo
  if (quo_miss_na_null_empty_no(col_vars)) {
    data     <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_vars <- rlang::syms("no_col_var")
    pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else {
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
  }
  col_vars_num  <- purrr::map_lgl(data[pos_col_vars], is.numeric)
  col_vars_text <- purrr::map_lgl(data[pos_col_vars],
                                  ~ is.factor(.) | is.character(.))

  tab_vars <- tab_vars_quo
  if (quo_miss_na_null_empty_no(tab_vars)) {
    #data     <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
    tab_vars <- character() #rlang::syms("no_tab_vars")
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  # wt_quo arrives from ctx (defused in tab_build); resolve to a bare symbol or character().
  if (quo_miss_na_null_empty_no(wt_quo)) {
    #data <- data %>% dplyr::mutate(no_weight = factor("n"))
    wt <- character() #rlang::sym("no_weight")
  } else {
    wt <- rlang::sym(rlang::as_name(wt_quo))
  }
  # print(tab_vars) ; print(row_var) ; print(wt) ; print(col_vars)

  # na_drop_all_quo arrives from ctx (defused in tab_build); a missing/NULL selection means
  # "drop nothing globally".
  if (rlang::quo_is_missing(na_drop_all_quo) || rlang::quo_is_null(na_drop_all_quo)) {
    na_drop_all <- character()
  } else {
    na_drop_all <- names(tidyselect::eval_select(na_drop_all_quo, data))
  }

  tab_row_names  <- as.character(c(tab_vars, row_vars))



  #The philosophy of tab_many is that :
  # - many col_vars are to be with the same kind of pct and colors (+ comp + diff + ci)
  # - many row_vars can have different colors and different parameters (otherwise tribble)

  #Arguments vectorised over row : tested in tab_plain/tab_num
  nrowvars    <- length(row_vars)
  totaltab    <- vctrs::vec_recycle(totaltab, nrowvars)
  totrow      <- vctrs::vec_recycle(totrow  , nrowvars)
  # Phase 6d (§4): `ref` = one reference row per row_var (named -> matched by name, else by
  # order; scalar -> same for all).
  # Phase 7g-iii (§4): under a col% regime a per-COL_VAR reference (a vector NAMED by col_var)
  # instead selects a reference COLUMN for each col_var -> routed into `ref_vect` (per col_var),
  # the scalar `ref` becoming unset. Detect it BEFORE resolve_ref_vector(row_vars) (which would
  # warn on the col_var names). A per-ROW_VAR *row* reference stays meaningless under col%, so a
  # (row_var-named) multi-element ref still collapses to a single column reference (+ message).
  pct_flat      <- unlist(pct)
  col_regime    <- any(pct_flat == "col") && !any(pct_flat == "row")
  ref_by_colvar <- NULL
  if (col_regime && !is.null(names(ref)) && any(nzchar(names(ref))) &&
      any(names(ref) %in% as.character(col_vars))) {
    ref_by_colvar <- resolve_ref_vector(ref, as.character(col_vars), what = "col_var")
    ref <- "auto"   # scalar unset: tab_num / settings / the row% path behave as no per-row ref
  }
  ref_is_vector <- length(ref) > 1
  ref         <- resolve_ref_vector(ref, as.character(row_vars))
  if (ref_is_vector && col_regime) {
    cli::cli_inform(c("i" = paste0("With {.code pct = \"col\"}, {.arg ref} is a single column ",
                                   "reference: the per-row_var reference is collapsed to its first value.")))
    ref <- vctrs::vec_recycle(ref[1], nrowvars)
  }
  ref2        <- vctrs::vec_recycle(ref2    , nrowvars)
  OR          <- vctrs::vec_recycle(OR      , nrowvars)
  comp        <- vctrs::vec_recycle(comp    , nrowvars)
  color       <- vctrs::vec_recycle(color   , nrowvars)
  #ci_visible <- vctrs::vec_recycle(ci_visible, nrowvars)

  #Arguments vectorised over row : tested here or in tab_num (not in tab_plain)
  ci          <- vctrs::vec_recycle(ci      , nrowvars)
  chi2        <- vctrs::vec_recycle(chi2    , nrowvars)

  #Arguments vectorised over columns : tested here
  ncolvars    <- length(col_vars)
  lvs         <- vctrs::vec_recycle(lvs   , ncolvars)
  digits      <- vctrs::vec_recycle(digits, ncolvars)
  if (totcol[1] %in% c("last", "all_col_vars")) {
    totcol <- col_vars_text[col_vars_text] %>% names() %>% dplyr::last()
    if (all(lvs == "first") & all(pct == "row") & ncolvars > 1) {
      totcol <- NULL
    }
  } else if (totcol[1] == "each") {
    totcol <- col_vars[col_vars_text]
  } else if (all(totcol %in% col_vars)) {
    totcol <- col_vars[col_vars %in% totcol & col_vars_text]
  } else if (all(totcol %in% c("col", "no"))) {
    totcol <- col_vars[which(totcol == "col" & col_vars_text)] # which ?
  } else if (is.numeric(totcol)) {
    if (any(totcol > ncolvars)) stop("some totcol indexes are superior to the",
                                     " number of col_vars")
    totcol <- col_vars[unique(as.integer(totcol))]
  } else {
    stop("totcol must be 'last', 'each', a vector of col_vars names, ",
         "a vector of 'col'/'no', or a vector of col_vars indexes")
  }
  # tot_cols_type summarises what to do with total columns downstream (consumed at ~L1366):
  #   "each"         = one total col per col_var (totcol == all col_vars)
  #   "all_col_vars" = a single total col spanning all col_vars (the last one)
  #   "some"         = total cols for a named subset of col_vars
  #   "no_delete"    = none requested, but one is needed internally (pct/ci/chi2/OR need a
  #                    reference total) -> build it, drop only at the very end
  #   "no_no_create" = no total col at all
  tot_cols_type <- dplyr::case_when(
    identical(totcol, col_vars)                                ~ "each",
    identical(totcol, col_vars[ncolvars])                      ~ "all_col_vars",
    length(totcol) == 0 &
      (any(chi2 != FALSE) | any(pct != "no") | any(ci != "no") |
         any(OR != "no") )                                     ~ "no_delete",
    length(totcol) == 0                                        ~ "no_no_create",
    TRUE                                                       ~ "some"
  )

  if (all( pct == "row" & OR %in% c("OR", "or", "OR_pct", "or_pct"))  ) {
    tot_cols_type <- "no_delete"
  }




  #Arguments vectorised over columns or rows : test in tab_plain/tab_num
  stopifnot(length(pct) >= 1)
  pct_vect <-
    if (is.character(pct) & length(pct) == 1) {
      rep(list(
        rep(pct, length(col_vars))
      ),
      length(row_vars),
      )
    } else if (is.character(pct) & length(row_vars) == 1) {
      list(vctrs::vec_recycle(pct, length(col_vars)))
    } else if (is.character(pct) & length(col_vars) == 1) {
      as.list(vctrs::vec_recycle(pct, length(row_vars)))
    } else if (is.character(pct) & length(pct) == length(col_vars)) {
      # Phase 7e FIX (was KNOWN-BUG): a per-col_var pct VECTOR with >= 2 row_vars used to fall
      # through to the stop(). tab() recycles pct to length(col_var) (`pct = c(rep(pct,
      # length(col_var)), ...)`), so `tab(data, >=2 row_vars, >=2 col_vars)` errored for ANY pct
      # (jmvtab drives exactly these multi x multi tables). Broadcast the per-col_var vector across
      # every row_var. Reached only after the length-1 / single-row_var / single-col_var branches,
      # so here length(row_vars) >= 2 and length(col_vars) >= 2.
      rep(list(pct), length(row_vars))
    } else if (is.list(pct) & length(pct) == length(row_vars) &
               all(purrr::map_int(pct, length) == length(col_vars))) {
      pct
    } else {
      stop("pct can't be recycled to the lengths of row_vars and col_vars (see documentation `?tab_many`)")
    }

  # Phase 7g-iii: ref_vect -- per row_var, a per-col_var reference vector (aligned to col_vars),
  # the reference analogue of pct_vect. Default: broadcast the per-row_var scalar `ref` across
  # col_vars (byte-identical .ref per col_var). The col%-per-col_var picker overrides EVERY row_var
  # with ref_by_colvar (one reference column per col_var). Threaded into the factor leaf (tab_plain)
  # only; tab_num keeps the scalar per-row_var `ref`.
  ref_vect <-
    if (!is.null(ref_by_colvar)) {
      rep(list(ref_by_colvar), length(row_vars))
    } else {
      purrr::map(ref, ~ rep(.x, length(col_vars)))
    }


  #Unique arguments :
  total_names <- vctrs::vec_recycle(total_names, 2)
  conf_level  <- vctrs::vec_recycle(conf_level , 1)
  na          <- vctrs::vec_recycle(na , 1)


  # Tests to be done before tab_plain / tab_num.
  # Phase 7b: the whole colour cascade -- color = "auto" resolution, the contrib -> totrow/chi2
  # and diff-family -> ci forcing, and the split of `color` into the per-step sub-passes
  # (color_diff_OR / color_ctr / color_ci / color_num) -- now lives in ONE pure resolver,
  # tab_resolve_settings() (R/tab-resolve.R), shared with tab_counts(). It is a data-free
  # function of the arguments + column classes: the exact boundary the Jamovi `.js` mirrors and
  # the Phase 7c cache keys on. Data-dependent resolution (ref = "auto"/regex, levels = "auto",
  # the leaf tot/totaltab forcing) deliberately stays in the leaf builders below.
  # See dev/tabxplor_argument_computation_map.md.
  .settings     <- tab_resolve_settings(color = color, OR = OR, ci = ci, chi2 = chi2,
                                         ref = ref, pct_vect = pct_vect,
                                         col_vars_text = col_vars_text, totrow = totrow,
                                         na = na, wt_name = as.character(wt),
                                         other_if_less_than = other_if_less_than, comp = comp,
                                         tab_vars = as.character(tab_vars),
                                         row_vars = as.character(row_vars),
                                         col_vars = as.character(col_vars),
                                         filter_expr = NA_character_)
  color         <- .settings$color
  chi2          <- .settings$chi2
  ci            <- .settings$ci
  totrow        <- .settings$totrow
  color_diff_OR <- .settings$color_diff_OR
  color_ctr     <- .settings$color_ctr
  color_ci      <- .settings$color_ci
  color_num     <- .settings$color_num
  cache_keys    <- .settings$cache_keys

  # --- repack: setup produces the resolved/recycled settings every downstream stage reads.
  # ctx_update() preserves a field resolved to NULL (e.g. totcol) as a NULL element -- `ctx$x <-
  # NULL` would delete it, breaking the downstream list2env() unpack. ---
  ctx_update(ctx, list(
    data = data,
    row_vars = row_vars, col_vars = col_vars, tab_vars = tab_vars, wt = wt,
    col_vars_num = col_vars_num, col_vars_text = col_vars_text,
    tab_row_names = tab_row_names, na_drop_all = na_drop_all,
    cleannames = cleannames, stars = stars, lvs = lvs,
    totaltab = totaltab, totrow = totrow, ref = ref, ref2 = ref2, ref_vect = ref_vect,
    OR = OR, comp = comp, color = color, ci = ci, chi2 = chi2,
    digits = digits, total_names = total_names, conf_level = conf_level, na = na,
    totcol = totcol, tot_cols_type = tot_cols_type, pct_vect = pct_vect,
    color_diff_OR = color_diff_OR, color_ctr = color_ctr,
    color_ci = color_ci, color_num = color_num, cache_keys = cache_keys
  ))
}


# === STAGE 2/5: tab_prepare_pop() -- prepare the population ONCE (cache tier 0) ==============
# Row-level preparation of the whole DB, shared by every table: select + relabel, apply the
# `filter` column (mutated in tab_build), na_text/na_num policy, tab_prepare() (ordered-strip +
# listwise removal + lump + cleannames), the tab_vars other_if_less_than re-lump, zero-weight
# removal, levels = "auto" resolution, and the lv1 non-first-level pre-merge. Everything here
# removes ROWS (a population change), never a per-pair reuse.
#' @keywords internal
#' @noRd
tab_prepare_pop <- function(ctx) {
  list2env(ctx, environment())
  # Phase 7e: jmvtab sets ctx$defer_level_merge = TRUE so `levels = "first"` does NOT collapse
  # non-first levels PRE-aggregate -- the aggregate + chi2/ANOVA see FULL levels (cacheable; the
  # level-drop is a display step in tab_assemble). tab()/tab_counts() leave it absent -> FALSE ->
  # today's pre-merge (byte-identical). The jmvtab full-level test therefore intentionally diverges
  # from tab(levels = "first"). See dev/tabxplor_jmvtab_cache_design.md 3.3/4e/5.
  if (!exists("defer_level_merge", inherits = FALSE)) defer_level_merge <- FALSE

  #Prepare the data
  data <- data %>% dplyr::select(!!!tab_vars, !!!row_vars, !!wt, !!!col_vars,
                                 tidyselect::any_of(".filter")) %>%
    relabel_levels_in_varnames(as.character(col_vars))

  #  Filters : here after selection (operations on rows copy all columns on memory),
  #     orwhen the tables are made for more speed :
  # - na = "drop_all" removes NAs here in tab_prepare (slower), i.e. for all tables mades
  # - na = "drop" : NA in factors and numeric will be removed in each tab_plain/tab_num
  # - na = "keep" : NA in factors (not numeric) will be made explicit in each tab_plain/tab_num

  if (with_filter == TRUE) data <- data %>% dplyr::filter(.data$.filter) %>%
    dplyr::select(-".filter")

  #If all variables on a subtable are "drop_all", then put na = "keep" to gain time
  if (na == "drop_all") {
    na_drop_all <- as.character(c(row_vars, col_vars, tab_vars))
    na_text <- "keep"
    na_num  <- "keep"

  } else {
    # na_drop_all was resolved to column names in tab_setup (Block B); re-resolve it against the
    # now-selected data. Byte-identical: the former `if (missing(na_drop_all))` branch was
    # unreachable once Block B assigned it (missing() is FALSE after assignment).
    na_drop_all <- names(tidyselect::eval_select(rlang::enquo(na_drop_all), data))

    na_text <-
      purrr::map(as.character(row_vars),
                 ~ purrr::map2_lgl(., as.character(col_vars[col_vars_text]),
                                   ~ all(c(.x, .y, as.character(tab_vars)) %in% na_drop_all)
                 ) ) |>
      purrr::map(~ dplyr::if_else(., "keep", na))

    na_num <-
      purrr::map(as.character(row_vars),
                 ~ all(c(., as.character(tab_vars)) %in% na_drop_all)
      ) |>
      purrr::map(~ dplyr::if_else(., "keep", na))
  }

  data <- data %>%
    tab_prepare(
      as.character(c(row_vars, col_vars, tab_vars)),
      na_drop_all = tidyselect::all_of(na_drop_all),
      cleannames = cleannames,
      other_if_less_than = other_if_less_than, other_level = other_level
    )
  # if (!missing(filter)) data <- dplyr::filter(data, {{filter}})


  if (other_if_less_than > 0 & length(tab_vars) != 0) {
    # We only count tab variable's minimum counts for the row variable,
    #  otherwise we get problems.
    data <- data %>%
      dplyr::group_by(!!!tab_vars) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(as.character(row_vars)),
                                  ~ forcats::fct_lump_min(., other_if_less_than,
                                                          other_level = other_level))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(as.character(row_vars), function(.x) forcats::fct_relevel(
        .x,
        unique(append(levels(dplyr::pull(data, dplyr::cur_column())), other_level)) %>%
          purrr::discard(!. %in% levels(.x)))
      ))
  }


  #Remove rows with missing values or 0 in weight, for them not to be added in raw counts
  # remove zero weight in tab_prepare ?
  if (length(wt) != 0) {
    zero_weight <- dplyr::pull(data, !!wt)
    zero_weight <- is.na(zero_weight) | zero_weight == 0
    if (any(zero_weight)) {
      rlang::inform(paste0(sum(zero_weight), " rows with zero or NA weights were removed"))
      data <- data %>% dplyr::filter(!zero_weight)
    }
  }


  if(any(lvs == "auto")) {
    # print(lvs)
    lvs <- purrr::map2_chr(
      lvs,
      dplyr::select(data, !!!col_vars),
      ~ if (.x == "auto") {
        if(!(is.factor(.y) | is.character(.y))) {"first"} else {
          if(nlevels(forcats::fct_drop(.y)) == 2L) "first" else "all"
        }
      } else {
        .x
      }
    )

  }

  # Where only first levels are kept, merge others to minimise useless calculations.
  # Phase 7e: skip the PRE-aggregate merge when defer_level_merge (jmvtab) -- keep full levels so the
  # aggregate + test are cacheable; the drop happens in tab_assemble. remove_levels then lists every
  # non-first level (+ the explicit "NA" column made by the leaves under na = "keep"; any_of ignores
  # it when absent), so the final table still shows only the first level.
  lv1 <- lvs == "first" & col_vars_text
  if (any(lv1)) {
    if (!isTRUE(defer_level_merge)) {
      col_vars_3levels <-
        purrr::map_lgl(dplyr::select(data, !!!col_vars),
                       ~ is.factor(.) & nlevels(.) >= 3) & lv1

      if (any(col_vars_3levels)) {

        rm_levels_by_col_vars <- dplyr::select(data, !!!col_vars[col_vars_3levels]) |>
          purrr::map(~ purrr::set_names(c(levels(.)[-1], "NA"), "remove_levels"))

        data <- data %>%
          dplyr::mutate(dplyr::across(
            tidyselect::all_of(as.character(col_vars[col_vars_3levels])),
            ~ suppressWarnings(forcats::fct_na_value_to_level(., level = "NA") |>
                                 forcats::fct_recode(rlang::splice(rm_levels_by_col_vars[[dplyr::cur_column()]] )))
          ))
      }
    }

    remove_levels <- purrr::map(dplyr::select(data, !!!col_vars[lv1]), ~ levels(.)[-1])
    if (isTRUE(defer_level_merge)) remove_levels <- purrr::map(remove_levels, ~ c(.x, "NA"))
  }


  #Make a table for each column variable and store them in a list

  # --- repack: prepare_pop produces the prepared population + level metadata (tier 0) ---
  ctx_update(ctx, list(
    data = data,
    na_text = na_text, na_num = na_num,
    lvs = lvs, lv1 = lv1,
    remove_levels = if (any(lv1)) remove_levels else NULL
  ))
}


# === STAGE 3/5: tab_aggregate() -- the tier-1 count / moment-sum aggregates ==================
# Prepped population -> the persisted cache tier: per-row_var numeric moment aggregates (via the
# shared tab_aggregate_num()) and the fused factor count aggregate `.fine` (the opt-in scan-fusion
# path, guarded). Both are NULL under `.by_table` (the table-by-table raw-scan path). tab_plain() /
# tab_num() are NOT split -- they adopt these via `.fine=` and remain the tier-3 transform.
#' @keywords internal
#' @noRd
tab_aggregate <- function(ctx) {
  # Phase 7e: the jmvtab live cache replaces the fused batch aggregate with a content-addressed
  # per-(row_var x col_var) build + tier-1 lookup (+ tier-2 test keys), mutating ctx$cache_env$store.
  # Inert for tab()/tab_many() (cache_env NULL). Same downstream contract: sets fine_fused (here a
  # per-pair named list -> fine_for_pair()) + fine_num (+ cached_tests / tier2_keys).
  if (!is.null(ctx$cache_env)) return(jmv_cache_aggregate(ctx))

  list2env(ctx, environment())
  .by_table <- by_table

  # Numeric tier-1: per-row_var moment-sum aggregate via tab_aggregate_num() (Phase 7d-i seam, now
  # HOISTED out of tab_num()'s pmap so the numeric aggregate is a first-class cache object). NEVER
  # fused across row_vars -- a shared scan can't reproduce per-row_var na.omit(<row_var>) and would
  # change float summation order. `.by_table` -> NULL -> tab_num() re-scans. Byte-identical to the
  # former in-pmap build (tab_aggregate_num() is pure and order-independent).
  fine_num <- NULL
  if (sum(col_vars_num) != 0) {
    fine_num <- if (.by_table) {
      rep(list(NULL), length(row_vars))
    } else {
      purrr::map2(row_vars, na_num, ~ tab_aggregate_num(
        data, !!.x,
        as.character(col_vars)[col_vars_num],
        as.character(tab_vars),
        wt = !!wt, na = .y
      ))
    }
    fine_num <- purrr::set_names(fine_num, as.character(row_vars))
  }

  # Factor tier-1: ONE shared finest-grain data.table aggregate keyed on all (tab_vars, row_vars,
  # factor col_vars), rolled up per (row_var x col_var) in tab_plain(`.fine`) instead of re-scanning
  # N rows per pair. OPT-IN (fuse_min_rows default Inf -> off): the win is modest once per-table
  # downstream dominates; kept as reusable infra (jmvtab caching). Guard: prod(nlevels) <= N, no
  # col/row var overlap, >= 2 tables, not `.by_table`. Keep the `keyby` sort order (rollup identity).
  .fine <- NULL
  if (!.by_table && sum(col_vars_text) != 0) {
    fct_col_vars <- as.character(col_vars[col_vars_text])
    col_in_row   <- any(fct_col_vars %in% as.character(c(row_vars, tab_vars)))
    if (!col_in_row) {
      fine_keys <- unique(as.character(c(tab_vars, row_vars, fct_col_vars)))
      key_card  <- function(x) if (is.factor(x)) nlevels(x) + as.integer(anyNA(x)) else
        data.table::uniqueN(x)
      prod_nlev <- prod(vapply(fine_keys, function(k) key_card(data[[k]]), numeric(1)))
      n_tables <- length(row_vars) * sum(col_vars_text)
      if (is.finite(prod_nlev) && prod_nlev <= nrow(data) &&
          nrow(data) >= getOption("tabxplor.fuse_min_rows", Inf) && n_tables >= 2) {
        dt___ <- data.table::as.data.table(data[unique(c(fine_keys, as.character(wt)))])
        .fine <- if (length(wt) != 0) {
          dt___[, list(n = .N, wn = sum(as.numeric(eval(wt)), na.rm = TRUE)), keyby = fine_keys]
        } else {
          dt___[, list(n = .N), keyby = fine_keys]
        }
      }
    }
  }

  # Both aggregates are NULL on the raw / .by_table path -- ctx_update() preserves them as NULL
  # elements so tab_transform()'s list2env() finds them (`ctx$x <- NULL` would delete them).
  ctx_update(ctx, list(fine_num = fine_num, fine_fused = .fine))
}


# fine_for_pair() -- pick the factor tier-1 aggregate for one (row_var x col_var) pair.
# DESIGN (Phase 7e): tab_transform() feeds tab_plain(.fine=) either the ONE fused joint DT (batch
# tab()/tab_counts() -- the is.data.table branch returns it UNCHANGED, byte-for-byte the pre-7e
# code, so golden/fuse/counts parity cannot move) OR a per-pair named list keyed "row_var\rcol_var"
# (the jmvtab cache: the reuse unit is per pair -- see dev/tabxplor_jmvtab_cache_design.md 3.2/6).
# A missing pair -> NULL -> tab_plain()'s `use_raw` raw scan. tab_plain always MARGINALISES .fine to
# its own pair, so a per-pair margin is idempotent there (locked by test-fuse-parity.R).
#' @keywords internal
#' @noRd
fine_for_pair <- function(fine, row_var, col_var) {
  if (is.null(fine) || data.table::is.data.table(fine)) return(fine)
  fine[[paste(as.character(row_var), as.character(col_var), sep = "\r")]]
}


# === STAGE 4/5: tab_transform() -- pct/diff/ratio/or/CI + fmt + the tier-2 test =============
# Aggregate -> the per-cell fmt fields and the whole-table test, via the UNCHANGED tab_num(.fine=) /
# tab_plain(.fine=) leaves (tier 3, O(cells), recomputed each run) + the post-join tab_apply_tests()
# (the tier-2 chi2/ANOVA test). Preserves the ordering invariant: tests run on the FULL levels,
# BEFORE the non-first-level drop (which lives in tab_assemble).
#' @keywords internal
#' @noRd
tab_transform <- function(ctx) {
  list2env(ctx, environment())
  .by_table <- by_table
  .fine     <- fine_fused
  # `chi2` stays the per-row_var logical flag (do_chi2). `tests` is the whole-table test output:
  # it starts as that logical (so a numeric-only table hits assemble's is.logical() fallback) and
  # is overwritten with the captured test tibbles when factor tables are built. NULL-init the two
  # table lists so the repack + assemble join are safe on the numeric-only / factor-only branches.
  tabs_text <- NULL
  tabs_num  <- NULL
  tests     <- chi2
  # Phase 7e tier-2 hook: jmvtab sets ctx$cached_tests; the tab()/tab_counts() ctx does not carry it,
  # so default it to NULL (list2env() only brings in fields present in ctx).
  if (!exists("cached_tests", inherits = FALSE)) cached_tests <- NULL
  # Phase 7g-iii: ref_vect (per row_var x per col_var reference) is built in tab_setup(); default it
  # to the scalar-ref broadcast if a ctx reached transform without it (byte-identical).
  if (!exists("ref_vect", inherits = FALSE)) ref_vect <- purrr::map(ref, ~ rep(.x, length(col_vars)))

  # Numeric transform: adopt the tier-1 moment aggregate `fine_num` (`.fine = ..9`), one tab_num()
  # per row_var; `.by_table` -> ..9 is NULL -> re-scan. Everything downstream is O(cells).
  if (sum(col_vars_num) != 0) {
    tabs_num <- purrr::pmap(list(row_vars, totaltab, totrow, ref, comp, color_num, ci, na_num,
                                 fine_num),
                            ~ tab_num(data,
                                      !!..1,
                                      as.character(col_vars)[col_vars_num],
                                      as.character(tab_vars),
                                      wt         = !!wt,
                                      na         = ..8,
                                      digits     = digits[col_vars_num],
                                      ref        = ..4,
                                      ci         = ..7,
                                      conf_level = conf_level,
                                      stars      = stars,
                                      comp       = ..5,
                                      color      = ..6,
                                      totaltab   = ..2,
                                      totaltab_name = totaltab_name,
                                      tot        = dplyr::if_else(..3, "row", "no"),
                                      total_names= total_names,
                                      .fine      = ..9,
                                      .by_table  = .by_table
                            )
    ) %>%
      purrr::set_names(row_vars)
  }

  # Phase 3b: whole-table test for NUMERIC col_vars = one-way ANOVA (Welch + classic F), computed
  # per row_var by running tab_chi2()'s test step on the numeric table (it detects mean col_vars and
  # calls agg_anova()). Only the tidy `test` tibble is kept; merged with the factor `chi2` at assemble.
  chi2_num <- NULL
  if (sum(col_vars_num) != 0 && any(chi2)) {
    chi2_num <- purrr::pmap(
      list(tabs_num, comp, chi2),
      ~ if (isTRUE(..3)) get_test(tab_chi2(tabs = ..1, calc = "p", comp = ..2))
        else new_test_tibble()
    )
  }

  if (sum(col_vars_text) != 0) {
    tabs_text <-     # By column first
      # Phase 7g-iii: ..11 = ref_vect (per row_var, per col_var); `..11[col_vars_text]` feeds the
      # inner pmap a per-factor-col_var reference `.ref`, so each col_var can have its own reference
      # column under pct="col". Default ref_vect broadcasts the per-row_var scalar -> byte-identical.
      # (..5 = the per-row_var scalar `ref` is kept in the list for index stability; tab_plain now
      # takes `.ref` instead.)
      purrr::pmap(list(row_vars, totaltab, totrow, pct_vect, ref, ref2, comp, OR, na_text, color_diff_OR,
                       ref_vect),

                  ~ purrr::pmap(list(col_vars[col_vars_text], digits[col_vars_text], ..9,
                                     ..4[col_vars_text], ..11[col_vars_text]),
                                function(.col_vars, .digits, .na, .pct, .ref)
                                  tab_plain(data,
                                            !!..1,
                                            !!.col_vars,
                                            as.character(tab_vars),
                                            wt = !!wt,
                                            na         = .na,
                                            digits     = .digits,
                                            pct        = .pct,
                                            ref        = .ref,
                                            ref2       = ..6,
                                            comp       = ..7,
                                            OR         = ..8,
                                            color      = ..10,
                                            #subtext   = "",
                                            totaltab   = ..2,
                                            totaltab_name = totaltab_name,
                                            tot        = c( "row", "col"), # vectorise totrow ?
                                            total_names= total_names,
                                            .fine      = fine_for_pair(.fine, ..1, .col_vars),
                                            .by_table  = .by_table)) %>%
                    purrr::set_names(col_vars[col_vars_text])

      ) %>%
      purrr::set_names(row_vars)
    #tot_cols_type != "no_no_create" | totrow == TRUE


    #Join the list of tabs into a single table,
    # managing duplicated levels
    duplicated_levels <- tabs_text %>%
      purrr::map(~ purrr::map(., ~ purrr::discard(names(.),
                                                  names(.) %in% c(row_vars, tab_vars))) %>%
                   purrr::flatten_chr() #%>% .[duplicated(.)] %>% unique())
      ) |>
      purrr::map(~ .[duplicated(.)] %>% unique()) |> purrr::flatten_chr() |> unique()

    if (length(duplicated_levels) != 0) {
      tabs_text <- tabs_text %>%
        purrr::map(~ purrr::imap(., ~ dplyr::rename_with(.x, function(.names)
          dplyr::if_else(.names %in% duplicated_levels, paste0(.names, "_", .y), .names)))
        )
    }

    tabs_text <- purrr::map2(tabs_text, as.character(row_vars), ~ purrr::reduce(
      .x,
      dplyr::full_join,
      by = c(as.character(tab_vars), .y)
    ))

    # DESIGN: ordering invariant — tab_chi2() and tab_ci() are INDEPENDENT (either order
    # works), but BOTH must run BEFORE non-first levels are dropped (L~1173), so they are
    # computed on the full set of levels. Do not move the level-drop above chi2/ci.
    # See CLAUDE.md § Global Architecture.
    # Phase 6a: one per-table pass through the shared tab_apply_tests() helper (chi2 ->
    # capture test -> ci). Byte-identical to the former two-batch passes: the tables are
    # independent for these steps, and `test` is still captured before ci. Phase 3b: contrib
    # ("ctr") is computed only when contrib coloring is requested (color_ctr != "no").
    # Phase 7e: cached_tests (per-row_var list) is the jmvtab tier-2 hook; NULL/absent on the tab()
    # path -> a per-table list of NULLs -> tab_apply_tests() recomputes as before.
    ct_aligned <- if (is.null(cached_tests)) rep(list(NULL), length(tabs_text))
                  else cached_tests[names(tabs_text)]
    applied <- purrr::pmap(
      list(tabs_text, chi2, ci, comp, color_ctr, color_ci, ct_aligned),
      function(.tab, .chi2, .ci, .comp, .cctr, .cci, .ct)
        tab_apply_tests(.tab, do_chi2 = .chi2, ci = .ci, comp = .comp,
                        color_ctr = .cctr, color_ci = .cci,
                        conf_level = conf_level, stars = stars,
                        method_cell = method_cell, method_diff = method_diff,
                        cached_test = .ct)
    )
    tabs_text <- purrr::map(applied, "tab")
    tests     <- purrr::map(applied, "test")
  }

  # --- repack: transform produces the built+tested tables + the tier-2 test.
  # tabs_text/tabs_num/chi2_num are NULL on the factor-only / numeric-only / no-test branches --
  # ctx_update() preserves them so tab_assemble()'s list2env() finds them. ---
  ctx_update(ctx, list(
    tabs_text = tabs_text, tabs_num = tabs_num, tests = tests, chi2_num = chi2_num
  ))
}


# === STAGE 5/5: tab_assemble() -- join, totals, wrap, output shape, render prep (tier 4) ====
# Built tables -> the final tabxplor_tab / list: non-first-level drop, add_n/add_pct, total col/row
# removal, the numeric+factor join, the whole-table test merge + class wrap, output-shape compaction,
# p-value lines, tab_spread, unwrap, and the optional tab_kable. Pure O(cells) display assembly.
#' @keywords internal
#' @noRd
tab_assemble <- function(ctx) {
  list2env(ctx, environment())

  if (sum(col_vars_text) != 0) {

    #Remove unwanted levels (keep only the first when levels = "first")
    if (any(lv1)) {
      remove_levels <-
        purrr::imap(remove_levels, ~ c(.x, paste0(.x, "_", .y))) %>%
        purrr::flatten_chr()

      tabs_text <- tabs_text %>% purrr::map(~ dplyr::select(., -tidyselect::any_of(remove_levels)))
    }


    # return(tabs_text)


    # Add column or row with n counts, or column or row with the other kind or percentages.
    tabs_text <- tab_add_n_pct(tabs_text, add_n, add_pct)




    #Remove unwanted total columns
    if (!tot_cols_type %in% c("each", "no_no_create")) {
      if (tot_cols_type == "no_delete") tabs_text <- tabs_text %>%
          purrr::map(~dplyr::select(., -where(is_totcol)))
      if (tot_cols_type == "some") tabs_text <- tabs_text %>%
          purrr::map(~dplyr::select(., -(where(~ is_totcol(.) & !get_col_var(.) %in% totcol) ))
          )

      if (tot_cols_type == "all_col_vars") {
        no_last_tot <- tabs_text %>%
          purrr::map(is_totcol) %>%
          purrr::map(~ names(.[.])) %>%
          purrr::flatten_chr() %>% unique()
        last_tot <- dplyr::last(no_last_tot)
        no_last_tot <- no_last_tot[no_last_tot != last_tot & !is.na(no_last_tot)]

        tabs_text <- tabs_text %>%
          purrr::map(~dplyr::select(., -tidyselect::any_of(no_last_tot)) %>%
                       dplyr::relocate(where(is_totcol), .after = tidyselect::last_col()) %>%
                       dplyr::rename_with(~ total_names[2], .cols = tidyselect::all_of(last_tot)) %>%
                       dplyr::mutate(dplyr::across(tidyselect::last_col(),
                                                   ~ set_col_var(., "all_col_vars")))
          )
      }
    }





    # Lone total column to "Total" with no col_var name
    totnames <-
      purrr::map(tabs_text,
                 ~names(.)[stringr::str_detect(names(.),
                                               paste0("^", total_names[2], "_"))]) |>
      purrr::flatten_chr()

    if ( length(totnames) == 1 ) tabs_text <- purrr::map(tabs_text, ~ dplyr::rename(
      ., tidyselect::any_of(purrr::set_names(totnames,
                                             rep(total_names[2], length(totnames))) )
    ) )


    # #By rows first
    # tabs_text <-
    #   purrr::pmap(list(col_vars[col_vars_text], digits[col_vars_text]),
    #               function(.col_vars, .digits)
    #
    #                 purrr::map_dfr(row_vars, function(.row_vars)
    #
    #                   tab_plain(data, !!.row_vars, !!.col_vars, !!!tab_vars, wt = !!wt,
    #                             na = na, digits = .digits,
    #                             totrow = totrow, totcol = totcol, totaltab = totaltab) |>
    #                     #                     dplyr::ungroup() |>
    #                     dplyr::mutate(variable = factor(rlang::as_name(.row_vars)), .before = 1) |>
    #                     dplyr::rename(tidyselect::all_of(
    #                       purrr::set_names(rlang::as_name(.row_vars), "row_var")
    #                     ))
    #                 )
    #
    #   ) %>%
    #   purrr::set_names(col_vars[col_vars_text])
  }




  if (sum(col_vars_num) != 0 & sum(col_vars_text) != 0) {
    tabs <- purrr::pmap(list(tabs_text, tabs_num, as.character(row_vars)),
                        ~ dplyr::full_join(..1, ..2,
                                           by = c(as.character(tab_vars), ..3)) #[tab_vars != "no_tab_vars"]
    )

    col_vars_order <- tabs |>
      purrr::map(~ purrr::map(.,
                              ~ purrr::map(get_col_var(.),
                                           ~ which(as.character(col_vars) == .)  ) ) |>
                   purrr::flatten()
      ) |>
      purrr::map(~ purrr::map_if(., names(.) %in% tab_row_names, ~ 0L) |>
                   purrr::map_int(~ if (length(.) == 0) {length(col_vars) + 1L} else {.}) |>
                   sort() |> names()
      )

    tabs <- tabs |> purrr::map2(col_vars_order, ~ dplyr::select(.x, tidyselect::any_of(.y)))

  } else if (sum(col_vars_num) != 0) {
    tabs <- tabs_num
    remove(tabs_num)

  } else {
    tabs <- tabs_text
    remove(tabs_text)
  }



  #Remove unwanted total rows
  no_totrow <- (totrow == FALSE |
                  (pct == "col" &  OR %in% c("OR", "or", "OR_pct", "or_pct")) &
                  tot_cols_type != "no_no_create")
  if (any(no_totrow)) {
    totrows     <- purrr::map(tabs[no_totrow], ~ is_totrow(.))
    tottab_rows <- purrr::map(tabs[no_totrow], ~ is_tottab(.))
    tottab_line <- purrr::map(tottab_rows[no_totrow], ~ length(.[.]) == 1 & .)

    tabs[no_totrow] <-
      purrr::pmap( list(tabs[no_totrow],totrows, tottab_line),
                   ~ tibble::add_column(..1, totrows = ..2, tottab_line = ..3) %>%
                     dplyr::filter(!.data$totrows | .data$tottab_line) %>%
                     dplyr::select(-"totrows", -"tottab_line")
      )
  }

  # Combine the factor (chi2) and numeric (ANOVA F) whole-table test tibbles, per row_var. For a
  # numeric-only table `tests` is still the boolean `chi2` flag here (the factor branch was skipped),
  # so is.logical() converts it to empty test tibbles. (Phase 7d-ii: `tests` replaces the former
  # `chi2` name overload -- `chi2` stays the per-row_var logical flag; `tests` the test tibbles.)
  if (is.logical(tests)) { tests <- rep(list(new_test_tibble()), length(tabs)) }
  if (!is.null(chi2_num)) { tests <- purrr::map2(tests, chi2_num, dplyr::bind_rows) }

  if (!any(purrr::map_lgl(tabs, lv1_group_vars)) ) {
    tabs <- tabs %>% purrr::map(~ dplyr::group_by(., !!!tab_vars))
    groups <- purrr::map(tabs, dplyr::group_data)
    tabs <- purrr::pmap(list(tabs, groups, tests),
                        ~ new_grouped_tab(..1, groups = ..2, subtext = subtext, test = ..3))
  } else {
    tabs <- purrr::map2(tabs, tests, ~ new_tab(.x, subtext = subtext, test = .y))
  }

  # === STAGE: assemble output shape (§13 truth table) ===
  # Merge the per-row_var tables into one only in "single" mode (tab() default) and only when
  # there are no tab_vars (merging with tab_vars is deferred, §7). `tabxplor.output_kable` also
  # forces a merge (its historical behaviour). A length-1 list (single row_var) is never merged
  # -- it is unwrapped below instead.
  can_merge <- length(tab_vars) == 0
  merge_now <- (output == "single" | getOption("tabxplor.output_kable") == TRUE) & can_merge
  if (merge_now &
      !(is.list(tabs) & !is.data.frame(tabs) & length(tabs) == 1 ) ) {
    tabs <- tabs |> tab_compact() # pvalue_lines = FALSE
  }


  if (is.data.frame(tabs)) {
    tabs <- tabs |> tab_pvalue_lines()

  } else {
    tabs <- purrr::map(tabs, tab_pvalue_lines)
  }

  # Phase 7g: n_min small-base DISPLAY filter -- the last, pure-display step (drops rows/cols
  # whose base < n_min and blanks weak cells; recomputes nothing). See tab_apply_n_min().
  # Defaults to 0 (off) for stage callers that don't thread it (e.g. tab_counts()).
  n_min <- if (exists("n_min", inherits = FALSE)) n_min else 0
  if (length(n_min) > 0 && any(n_min > 0, na.rm = TRUE)) {
    tabs <- if (is.data.frame(tabs)) tab_apply_n_min(tabs, n_min)
            else purrr::map(tabs, tab_apply_n_min, n_min = n_min)
  }


  # Phase 6i: spread selected tab_vars into columns via tab_spread() (kept active per the
  # maintainer's choice). Applied per table (list) or once (single tab). `spread_vars` is a
  # character subset of tab_vars resolved by the caller.
  if (length(spread_vars) != 0) {
    .spread_one <- function(t) {
      if (is.null(names_prefix)) {
        tab_spread(t, spread_vars = tidyselect::all_of(spread_vars),
                   names_sort = names_sort, totname = total_names[1])
      } else {
        tab_spread(t, spread_vars = tidyselect::all_of(spread_vars),
                   names_prefix = names_prefix, names_sort = names_sort,
                   totname = total_names[1])
      }
    }
    tabs <- if (is.data.frame(tabs)) .spread_one(tabs) else purrr::map(tabs, .spread_one)
  }

  # Unwrap a length-1 list to a bare tab, EXCEPT when a list was explicitly requested
  # (output == "list": tab(output_list = TRUE) keeps the length-1 list, §13).
  if (output != "list" &
      is.list(tabs) & !is.data.frame(tabs) & length(tabs) == 1) tabs <- tabs[[1]]

  if (getOption("tabxplor.output_kable") == TRUE) tabs <- tabs %>% tab_kable()

  tabs
}














#' Spread a tab, passing a tab variable to column
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab}},
#' \code{\link{tab_many}} or \code{\link{tab_plain}}.
#' @param spread_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>  The tab variables
#' to pass to column, with a syntax of type \code{c(var1, var2, ...)}.
#' @param names_prefix String added to the start of every variable name.
#' @param names_sort If no \code{names_prefix} is given, new names takes the form
#'  \code{spread_var}_\code{col_var_level}. Should then the column names be sorted ?
#'  If \code{FALSE}, the default, column names are ordered by first appearance.
#' @param totname The new name of the total rows, as a single string.
# @param recalculate Where there is several `tab_vars`, some totals are missing in the
# spreaded table. By default, `tab_spread` try to recalculate them based on `pct` and `wn`.
# Warning : with `means`, a weighted mean is calculated, which is only an approximation.
# Set to `FALSE` to avoid this behavior.
#'
#' @return A \code{tibble} of class \code{tab}, with less rows and more columns.
#' @export
#'
#' @examples
#' \donttest{ data <- forcats::gss_cat %>% dplyr::filter(year %in% c(2000, 2014))
#'
#' tabs <-
#'   tab(data, relig, marital, c(year, race), pct = "row", totaltab = "no",
#'       color = "diff", tot = "row", other_if_less_than = 30)
#'
#' tabs %>%
#'   dplyr::select(year, race, relig, Married) %>%
#'   tab_spread(race)
#'   }
tab_spread <- function(tabs, spread_vars, names_prefix, names_sort = FALSE,
                       totname = "Total" #, recalculate = TRUE
) {
  spread_vars     <- rlang::enquo(spread_vars)
  pos_spread_vars <- tidyselect::eval_select(spread_vars, tabs)
  spread_vars     <- names(pos_spread_vars)
  NA_spread_vars  <- purrr::map_lgl(spread_vars,
                                    ~ as.character(.) %in% c("NA", "NULL", "no"))
  if (all(NA_spread_vars) ) return(tabs)

  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  get_vars   <- tab_get_vars(tabs)
  col_levels <- get_vars$col_vars_levels %>% purrr::flatten_chr()
  row_var    <- get_vars$row_var
  tab_vars   <- get_vars$tab_vars
  tab_vars_new <- tab_vars[!tab_vars %in% spread_vars]

  na_values <- purrr::map(dplyr::ungroup(tabs)[col_levels],
                          ~ fmt0(type = get_type(.x), display = get_display(.x[1]))) %>%
    purrr::set_names(col_levels)


  totrows <- is_totrow(tabs)
  if (any(totrows)) {
    #tab_match_groups_and_totrows(tabs)
    tabs <- tabs %>% dplyr::group_by(!!!rlang::syms(tab_vars))
    groups <- dplyr::group_vars(tabs)

    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows & totrows

    tabs <- tabs %>% tibble::add_column(totrows, tottab_rows, tottab_line)

    # if two tab_vars or more, calculate totals for each level of spread_var
    if (length(tab_vars_new) != 0 & any(tottab_rows)) {

      # if (recalculate) {
      #   if (any(get_type(tabs) == "mean")) {
      #     warning(paste0("Since there are several tab_vars, some totals are missing. ",
      #                    "Means for the new general total row were recalculated based on a ",
      #                    "weighted mean, which is only an approximation"))
      #   }
      #   new_totals <- tabs %>%
      #     dplyr::filter(.data$totrows) %>%
      #     dplyr::group_by(!!!rlang::syms(spread_vars)) %>%
      #     dplyr::summarise(dplyr::across(
      #       where(is_fmt),
      #       ~ new_fmt(display = get_display(.)[1],
      #                 digits  = max(get_digits(.)),
      #                 n       = sum(get_n(.), na.rm = TRUE),
      #                 wn      = sum(get_wn(.), na.rm = TRUE),
      #                 pct     = sum(get_wn(.), na.rm = TRUE) / sum(get_wn(.)/get_pct(.), na.rm = TRUE),
      #                 diff    = NA_real_,
      #                 ctr     = NA_real_,
      #                 mean    = stats::weighted.mean(get_mean(.), get_wn(.), na.rm = TRUE),
      #                 var     = NA_real_,
      #                 ci      = NA_real_,
      #
      #                 in_totrow = TRUE,
      #                 in_refrow = FALSE,
      #                 in_tottab = all(is_tottab(.)), #any ?
      #
      #                 type      = get_type    (.),
      #                 comp_all  = get_comp_all(., replace_na = FALSE),
      #                 ref = get_ref_type(.),
      #                 ci_type   = get_ci_type (.),
      #                 col_var   = get_col_var (.),
      #                 totcol    = is_totcol   (.),
      #                 refcol    = is_refcol   (.),
      #                 color     = get_color   (.)
      #       ), .groups = "drop"
      #     ))
      #   tabs_colors <- get_color(tabs)
      #
      #   ensemble_names <- tabs %>%
      #     dplyr::filter(tottab_line) %>%
      #     dplyr::ungroup() %>%
      #     select(tab_vars_new) %>% purrr::map_chr(~ as.character(dplyr::first(.)))
      #
      #   total_ensemble <- tabs %>%
      #     dplyr::filter(tottab_line) %>%
      #     dplyr::pull(row_var) %>% as.character()
      #
      #   new_totals <- new_totals %>%
      #     tab_pct(just_diff = TRUE) %>%
      #     dplyr::mutate(dplyr::across(where(is_fmt),
      #                                 ~ set_color(., tabs_colors[dplyr::cur_column()]))) %>%
      #     dplyr::mutate(!!rlang::sym(row_var) := factor(total_ensemble))
      #
      #   new_totals <- new_totals %>%
      #     purrr::reduce2(.x = names(ensemble_names), .y = ensemble_names, .init = new_totals,
      #                    .f = ~ dplyr::mutate(..1, !!rlang::sym(..2) := factor(..3))
      #     )  %>%
      #     dplyr::filter(!is_tottab(.))
      #
      # }

      tabs <- tabs %>% dplyr::filter(!tottab_line)

      #if (recalculate) tabs <- tabs %>% dplyr::bind_rows(new_totals)
    }

    new_levels <- tabs %>%
      dplyr::filter(.data$totrows & !.data$tottab_line) %>%
      dplyr::select(!!!tab_vars, !!row_var) %>%
      dplyr::arrange(!!!rlang::syms(tab_vars_new), .by_group = FALSE,
                     .by_totals = FALSE, .only_main_display = FALSE) %>%
      dplyr::mutate(
        new_levels = paste(totname, paste(!!!rlang::syms(tab_vars_new), sep = " / ")) %>%
          stringr::str_to_upper()
      )
    new_levels <- purrr::set_names(as.character(dplyr::pull(new_levels, row_var)),
                                   new_levels$new_levels)

    # if (length(groups) - 1 != 0) {
    #   group_vars_totals <-
    #     dplyr::group_keys(dplyr::filter(tabs, !tottab_line)) %>% #dplyr::mutate(bis = PR0) %>%
    #     dplyr::select(-tidyselect::all_of(spread_vars)) %>%
    #     tidyr::unite(!!row_var, sep = " / ") %>%
    #     dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ paste(totname, .))) %>%
    #     tibble::deframe() %>%
    #     stringr::str_to_upper() %>% forcats::as_factor()
    # } else {
    #   group_vars_totals <- factor(totname)
    # }
    #
    # former_levels <-
    #   tibble::add_column(tabs, totrows = is_totrow(tabs),
    #                      tottab = is_tottab(tabs)) %>%
    #   dplyr::filter(.data$totrows & !.data$tottab) %>% dplyr::pull(row_var)
    #
    # group_vars_totals <- vctrs::vec_recycle(group_vars_totals, length(former_levels))
    #
    # new_levels <- former_levels %>% as.character() %>%
    #   purrr::set_names(group_vars_totals)

    tabs <- tabs %>% dplyr::mutate(
      !!rlang::sym(row_var) := forcats::fct_recode(!!rlang::sym(row_var),
                                                   !!!new_levels) %>%
        forcats::fct_relevel(unique(names(new_levels)), after = Inf)
    ) %>%
      dplyr::select(-"totrows", -"tottab_rows", -"tottab_line")
  }

  if ( !missing(names_prefix) ) {
    tabs <- tidyr::pivot_wider(tabs,
                               names_from   = tidyselect::all_of(spread_vars),
                               values_from  = tidyselect::all_of(col_levels),
                               names_prefix = names_prefix,
                               values_fill  = na_values,
                               names_sort   = names_sort
    )
  } else {
    tabs <- tidyr::pivot_wider(tabs,
                               names_from   = tidyselect::all_of(spread_vars),
                               values_from  = tidyselect::all_of(col_levels),
                               #names_glue   = "{.value}_{.name}",
                               values_fill  = na_values,
                               names_sort   = names_sort
    )
  }

  tabs <- tabs %>%
    dplyr::arrange(!!!rlang::syms(tab_vars_new), !!rlang::sym(row_var),
                   .only_main_display = FALSE)

  tabs <- complete_partial_totals(tabs)


  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {

    group_dat <- dplyr::group_data(tabs)
    new_grouped_tab(tabs, groups = group_dat, subtext = subtext, chi2 = chi2)
  }

}


# # NEW FUNCTION TO FINISH, DOCUMENT and integrate into the package ?
# #' @export
# #'
# # @examples
# tab_transpose <- function(tabs, name = "variables") {
#   row_var <- tab_get_vars(tabs, "row_var")$row_var
#   totrow_names <- filter(tabs, is_totrow(tabs)) |> pull(1) |> as.character()
#   if (length(totrow_names) >= 2) stop("not working for now with many total rows")
#   totcol_name <- is_totcol(tabs) ; totcol_name <- names(totcol_name[totcol_name])
#   if (length(totcol_name) >= 2) stop("not working for now with many total columns")
  
#   tabs |>
#     pivot_longer(cols = -1, names_to = name, values_to = "value") |> 
#     pivot_wider(names_from = all_of(row_var), values_from = value, names_sort = TRUE) |> 
#     mutate(across(where(is.character), as_factor)) |>
#     mutate(across(where(is_fmt), ~ set_type(., "col"))) |> 
#     mutate(across(where(is_fmt), ~ as_totcol(., FALSE))) |> 
#     mutate(across(any_of(totrow_names), ~ as_totrow(as_totcol(.), FALSE))) |>
#     mutate(across(where(is_fmt), ~ if_else(!!sym(name) == totcol_name, 
#                                            as_totrow(.), 
#                                            as_totrow(., FALSE)))) |> 
#     new_tab()
# }





#' @describeIn tab_many Get the variables names of a \pkg{tabxplor} \code{tab}
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab}},
#' \code{\link{tab_many}} or \code{\link{tab_plain}}.
#' @param vars In `tab_get_vars`, a character vector containing the wanted vars names:
#' \code{"row_var"}, \code{"col_vars"} or \code{"tab_vars"}.
#'
#' @return A list with the variables names.
#' @export
#'
# @examples
tab_get_vars <- function(tabs, vars = c("row_var", "col_vars", "tab_vars")) {
  stopifnot(is.data.frame(tabs))

  if ("col_vars" %in% vars) {
    fmtc <- purrr::map_lgl(tabs, is_fmt)
    col_vars       <- get_col_var(tabs[fmtc]) %>% purrr::discard(is.na(.))
    col_vars_names <- col_vars %>% unique()

    col_vars_levels <-
      purrr::map(col_vars_names, ~ names(col_vars[col_vars == .])) %>%
      purrr::set_names(col_vars_names)

    col_vars <- col_vars_names
  }

  fct_cols <- purrr::map_lgl(tabs, is.factor)

  if ("row_var" %in% vars) row_var <- names(utils::tail(fct_cols[fct_cols], 1L))

  if ("tab_vars" %in% vars) tab_vars <-
    names(fct_cols[fct_cols & names(fct_cols) != row_var])



  ls(pattern = "^row_var$|^col_vars$|^col_vars_levels$|^tab_vars$") %>%
    purrr::set_names(.) %>%
    purrr::map(~ rlang::sym(.) %>% rlang::eval_tidy())
}




# STEP-BY-STEP FUNCTIONS -----------------------------------------------------------------

# Lump factor levels whose (unweighted) count is below `other_if_less_than` into `other_level`.
# Phase 7d-ii: extracted verbatim from tab_prepare() so the internal pipeline and the jmvtab cache
# can run this as a standalone, keyable pre-aggregate step; tab_prepare() still composes it.
# `across(all_of(character()))` is a no-op, so the length guard only short-circuits the common case.
tab_lump_others <- function(data, vars_not_numeric, other_if_less_than = 0,
                            other_level = "Others") {
  if (other_if_less_than > 0 && length(vars_not_numeric) != 0) {
    data <- data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(vars_not_numeric),
        ~ forcats::fct_lump_min(., other_if_less_than, other_level = other_level)
      ))
  }
  data
}

# Strip the cleannames regex (prefix numbers like "1-", parenthesised text) from factor labels.
# Phase 7d-ii: extracted verbatim from tab_prepare(). The tab()/tab_build path runs it PRE-aggregate
# (kept, cache-design §5 — summing cleannames); jmvtab (Phase 7e) will call it at DISPLAY instead.
# The caller decides whether cleannames is on; this helper only performs the relabel.
tab_cleannames_relabel <- function(data, vars_not_numeric) {
  if (length(vars_not_numeric) != 0) data <- data %>%
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(vars_not_numeric),
      ~ forcats::fct_relabel(., ~ stringr::str_remove_all(., cleannames_condition()))
    ))
  data
}

#' Prepare data for \code{\link{tab_plain}}.
#'
#' @param data A dataframe.
#' @param ... Variables then to be passed in \code{\link{tab_plain}}.
#' @param na_drop_all <\link[tidyr:tidyr_tidy_select]{tidy-select}> Removes all
#' observation with a `NA` in any of the chosen variables.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param other_if_less_than When set to a positive integer, levels with less count
#' than it will be merged into an "Others" level.
#' @param other_level The name of the "Other" level, as a character vector of length one.
#'
#' @return A modified data.frame.
#' @export
#' @examples \donttest{data <- dplyr::starwars %>%
#' tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'             na_drop_all = sex)
#' data
#' }
tab_prepare <-
  function(data, ..., na_drop_all,
           cleannames = NULL, other_if_less_than = 0,
           other_level = "Others") {

    cleannames <-
      if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}

    variables     <- rlang::expr(c(...))
    pos_variables <- tidyselect::eval_select(variables, data)
    variables     <- names(pos_variables)

    if (missing(na_drop_all)) {
      na_drop_all <- character()
    } else{
      na_drop_all <- names(tidyselect::eval_select(rlang::enquo(na_drop_all), data))
    }



    #Converting to data.table and back divides the time by two with large dataframes
    if (length(na_drop_all) != 0) {
      data.table::setDT(data)
      data <- tibble::as_tibble(stats::na.omit(data, na_drop_all))
      #data <- tidyr::drop_na(data, tidyselect::all_of(na_drop_all))
    }

    vars_not_numeric <-
      dplyr::select(data[pos_variables], where(~ !is.numeric(.))) %>%
      colnames() #%>% rlang::syms()                # is.integer(.) | is.double()

    #Transform characters to factors first ? Time taker.
    # data <- data %>%
    #   dplyr::mutate(dplyr::across(
    #     tidyselect::all_of(vars_not_numeric) & where(~ !is.factor(.)),
    #     as.factor
    #   ))

    # Strip the `ordered` class from factors. Pragmatic: ordered factors once triggered an
    # error downstream (likely in MCA / an external step), and dropping the class was the
    # simplest fix. FIXME(future): keep `ordered` instead, to support ordinal-specific
    # behaviours/options — remove this once the downstream error is pinned down.
    data <- data %>%
      dplyr::mutate(dplyr::across(
        where(is.ordered),
        ~ magrittr::set_class(., class(.)[class(.) != "ordered"])
      ))

    # Remove unused levels : time taker
    # data <- data %>%  #Remove unused levels anyway
    #   dplyr::mutate(dplyr::across(tidyselect::all_of(vars_not_numeric),
    #                               forcats::fct_drop))

    # Phase 7d-ii: rare-level lump + cleannames relabel are now standalone helpers (callable by the
    # jmvtab cache); tab_prepare composes them here in the same lump-then-clean order (byte-identical).
    data <- data %>% tab_lump_others(vars_not_numeric, other_if_less_than, other_level)
    if (cleannames == TRUE) data <- data %>% tab_cleannames_relabel(vars_not_numeric)

    data
  }







# DESIGN: tab_plain() is the core aggregation function. Internal sequence:
#   1. data.table dcast (row_var ~ col_var, fun = sum of weights) for speed
#   2. Wrap counts into fmt vectors via new_fmt()
#   3. Add total rows/cols, then chain to tab_pct/tab_ci/tab_chi2 as requested
#   Column names are temporarily prefixed to avoid DT reserved name conflicts.
#' Plain single cross-table
# @description
#' @param data A data frame.
#' @param row_var,col_var The row variable, which will be printed with one level per line,
#'  and the column variable, which will be printed with one level per column. Numeric
#'  variables will be used as factors. To calculate means, use \code{\link{tab_num}}.
#' @param tab_vars  <\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
#' a subtable is made for each combination of levels of the
#' selected variables. Leave empty to make a simple cross-table. All tab variables
#' are converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param digits The number of digits to print, as a single integer.
#' @param na The policy to adopt with missing values, as a single string.
#'  \itemize{
#'   \item \code{"keep"}: by default, \code{NA}'s of row, col and tab variables
#'   are printed as explicit "NA" level.
#'   \item \code{"drop"}: removes NA of row, col and tab variables.
#'   }
#' @param totaltab The total table,
#' if there are subtables/groups (i.e. when \code{tab_vars} is provided) :
#'  \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param tot The totals :
#'  \itemize{
#'   \item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
#'   columns.
#'   \item \code{"row"}: only total rows.
#'   \item \code{"col"}: only total column.
#'   \item \code{"no"}: remove all totals (after calculations if needed).
#'  }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param pct The type of percentages to calculate :
#'  \itemize{
#'   \item \code{"row"}: row percentages.
#'   \item \code{"col"}: column percentages.
#'   \item \code{"all"}: frequencies for each subtable/group, if there is \code{tab_vars}.
#'   \item \code{"all_tabs"}: frequencies for the whole (set of) table(s).
#' }
#' @param ref The reference cell to calculate differences and ratios
#'  (used to print \code{colors}) :
#'  \itemize{
#'   \item \code{"auto"}: by default, cell difference from the corresponding total
#'   (rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
#'   used for `diff` ; cell ratio from the first line (or col) is use for `OR`
#'   (odds ratio/relative risks ratio).
#'   \item \code{"tot"}: totals are always used.
#'   \item \code{"first"}: calculate cell difference or ratio from the first cell
#' of the row or column (useful to color temporal developments).
#'   \item \code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.
#'   \item \code{"regex"}: when `ref` is a string, it it used as a regular expression,
#'   to match with the names of the rows (or columns). Be precise enough to match only one
#'   column or row, otherwise you get a warning message.
#'   \item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
#' }
#' @param ref2 A second reference cell is needed to calculate odds ratios
#' (or relative risks ratios). The first cell of the row or column is used by default.
#' See `ref` above for the full list of possible values.
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios
#'   (for binary variables) or relative risks ratios (for variables with 3 levels
#'   or more).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' @param color The type of colors to print, as a single string :
#'  \itemize{
#'   \item \code{"no"}: by default, no colors are printed.
#'   \item \code{"diff"}: color percentages and means based on cells differences from
#'   totals (or from first cells when \code{ref = "first"}).
#'   \item \code{"OR"}: for `pct == "col"` or `pct == "row"`,
#'   color based on odds ratios (or relative risks ratios)
#'  }
#' @param subtext A character vector to print rows of legend under the table.
#' @param num Set to \code{TRUE} to obtain a table with normal numeric vectors (not fmt).
#' @param df  Set to \code{TRUE} to obtain a plain data.frame (not a tibble),
#' with normal numeric vectors (not fmt). Useful, for example, to pass the table to
#' correspondence analysis with \pkg{FactoMineR}.
#' @param .fine,.by_table Internal. `.fine` is a pre-computed count-aggregate to roll up from
#' instead of scanning the raw data (used by \code{\link{tab_counts}} and the scan-fusion path);
#' `.by_table` forces the table-by-table path.
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}. If \code{...} (\code{tab_vars})
#'  are provided, a \code{tab} of class \code{tabxplor_grouped_tab}.
#' All non-text columns are \code{\link{fmt}} vectors of class \code{tabxplor_fmt},
#' storing all the data necessary to print formats and colors. Columns with \code{row_var}
#' and \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples # A typical workflow with tabxplor step-by-step functions :
#' \donttest{
#' data <- dplyr::starwars %>% tab_prepare(sex, hair_color)
#'
#' data %>%
#'   tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row") %>%
#'   tab_chi2() %>%
#'   tab_ci(color = "after_ci")
#' }
tab_plain <- function(data, row_var, col_var, tab_vars, wt,
                      pct = "no", color = "no", OR = "no",
                      na = "keep",
                      ref = "auto", ref2 = "first", comp = "tab",
                      totaltab = "line", totaltab_name = "Ensemble",
                      tot = NULL, total_names = "Total",
                      subtext = "", digits = 0,
                      num = FALSE, df = FALSE,
                      .fine = NULL, .by_table = FALSE
) {

  row_var_quo <- rlang::enquo(row_var)
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::ensym(row_var)
  }

  col_var_quo <- rlang::enquo(col_var)
  if (quo_miss_na_null_empty_no(col_var_quo)) {
    data <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_var <- rlang::sym("no_col_var")
  } else {
    col_var <- rlang::ensym(col_var)
  }

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character()
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  # if (missing(...)) {
  #   #data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
  #   tab_vars <- character() #rlang::syms("no_tab_vars")
  # } else {
  #   tab_vars_quo <- rlang::enquos(...)
  #   NA_tab_vars  <- purrr::map(tab_vars_quo,
  #                              ~ is.na(as.character(rlang::get_expr(.)))) %>%
  #     purrr::flatten_lgl()
  #   if (all(NA_tab_vars) ) {
  #     #data <- data %>% dplyr::mutate(no_tab_vars = factor(" "))
  #     tab_vars <- character() #rlang::syms("no_tab_vars")
  #   } else {
  #     tab_vars     <- rlang::expr(c(...))
  #     pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
  #     tab_vars     <- rlang::syms(names(pos_tab_vars))
  #   }
  # }

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }



  vctrs::vec_assert(pct, size = 1)
  vctrs::vec_assert(ref, size = 1)
  ref <- stringr::str_squish(ref)
  vctrs::vec_assert(ref2, size = 1)
  ref2 <- stringr::str_squish(ref2)
  vctrs::vec_assert(OR, size = 1)
  vctrs::vec_assert(na, size = 1)
  stopifnot(na %in% c("keep", "drop"))
  vctrs::vec_assert(totaltab_name, size = 1)
  total_names  <- vctrs::vec_recycle(total_names, 2)

  #pct
  stopifnot(pct %in% c("no", "row", "col", "all", "all_tabs"))
  if (is.logical(OR)) if(OR) OR <- "OR" else OR <- "no"
  stopifnot(OR %in% c("no", "OR", "OR_pct", "or", "or_pct"))
  if (pct == "all_tabs" & length(tab_vars) == 0) pct <- "all"

  if (color != "no" & ref == "no") {
    warning("since color is ", color, " ref can't be `no` and was set to `tot`")
    ref <- "tot"
  }

  #tot
  if (is.null(tot)) {
    tot <- switch(pct,
                  "no"  = "no",
                  "row" = , #switch(ref, "tot" = c("row", "col"), "col"),
                  "col" = , #switch(ref, "tot" = c("row", "col"), "row"),
                  "all" = ,
                  "all_tabs" = c("row", "col"),
    )

  } else {
    stopifnot(all(tot %in% c("row", "col", "both", "no", "")))
    if (tot[1] == "both") tot <- c("row", "col")

    if (!"col" %in% tot) {
      if (pct == "row") {
        warning("since pct == 'row', a total column was added")
        tot <- c(tot, "col")
      }
      if (color != "no" & pct == "col" & ref == "tot") {
        warning("since color == ", color, " and pct == 'col' and ref == 'tot', a total column was added")
        tot <- c(tot, "col")
      }
      if (pct %in% c("all", "all_tabs")) {
        warning("since pct == 'all' or 'all_tabs', a total column was added")
        tot <- c(tot, "col")
      }
    }

    if (!"row" %in% tot) {
      if (pct == "col") {
        warning("since pct == 'row', total rows were added")
        tot <- c(tot, "row")
      }
      if (color != "no" & pct == "row" & ref == "tot") {
        warning("since color == ", color, " and pct == 'row' and ref == 'tot', total rows were added")
        tot <- c(tot, "row")
      }
      if (pct %in% c("all", "all_tabs")) {
        warning("since pct == 'all' or 'all_tabs', total rows were added")
        tot <- c(tot, "row")
      }
    }
  }

  #comp
  vctrs::vec_assert(comp, size = 1)
  stopifnot(comp %in% c("tab", "all", "") | is.na(comp) | is.null(comp))

  if (comp == "all" & length(tab_vars) == 0) comp <- "tab"

  #ref
  # LEAF resolution (Phase 7b): ref = "auto" is type-specific and intentionally stays here, NOT
  # in tab_resolve_settings() -- for a mixed table it must differ between this factor leaf and the
  # numeric leaf (tab_num). OR / empirical-OR colour compare to the first level -> "first";
  # otherwise the total row -> "tot". See the map doc, § static-vs-data line.
  if (ref == "auto") {
    ref <- if (OR != "no" | color %in% c("or", "OR")) {"first"} else {"tot"}
  }

  #digits
  vctrs::vec_assert(digits, size = 1)
  digits <- vctrs::vec_cast(digits, integer())

  #totaltab
  if (length(tab_vars) == 0) totaltab <- "no"

  if (((comp[1] == "all" & ref == "tot") | pct == "all_tabs") &
      !totaltab %in% c("table", "line")) {
    warning("since comp = 'all', a total table was added to compare with")
    totaltab <-  "line"
  }

  if (comp[1] == "all" & !ref %in% c("tot", "no", "") & totaltab != "table") {
    warning("since comp = 'all', a full total table was added to compare with")
    totaltab <- "table"
  }



  # DESIGN: fused aggregation. When tab_many supplies a shared finest-grain aggregate (`.fine`),
  # skip the per-table raw-data prep + scan and roll `.fine` up instead (see the aggregation branch
  # below). `use_raw` keeps the table-by-table path fully intact; forced on by `.by_table`, and
  # always for the df/num paths (which are never fused).
  use_raw <- .by_table || is.null(.fine) || df || num

  if (use_raw) {
    data <- data %>%
      dplyr::select(!!!tab_vars, !!row_var, !!col_var, !!wt) %>%
      dplyr::mutate(dplyr::across(!!wt & !where(is.numeric), as.numeric)) %>%
      # PERF/FIXME: redundant — relabel_levels_in_varnames() already runs once in tab_many
      # (~L889). Kept for the step-by-step entry straight into tab_plain. Cheap now (post the
      # short-circuit fix, see CLAUDE.md § Discovered bugs) but a removal candidate.
      relabel_levels_in_varnames(as.character(col_var))
    #Vars are not changed to factors here, but after data.table
  }






  tab_row_names  <- as.character(c(tab_vars, row_var))

  # DESIGN: data.table name round-trip (how user column names survive dcast). We (1) rename
  # the col_var to the fixed internal name "col_var" (~L2239) so the dcast formula is stable,
  # and (2) when a col_var ALSO appears among row/tab vars (self cross-tab), duplicate it as
  # "<var>_colvarbis" so one column can be both an aggregation key and the spread variable.
  # The internal names ("col_var", "_colvarbis", and dcast's "n_"/"wn_" value prefixes) are
  # all stripped later (~L2317 setnames, ~L2437 prefix removal) to restore the user's names.
  #If variables are in double in cols and rows, duplicate them and manage data.table
  col_var_in_row_var <- tab_row_names %in% as.character(col_var)
  if (any(col_var_in_row_var)) {
    in_col_vars <- tab_row_names[col_var_in_row_var]

    data <- data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(in_col_vars), ~ ., .names = "{.col}_colvarbis"))
    tabs_vars2 <-
      if (length(tab_vars) != 0) {
        dplyr::recode(as.character(tab_vars),
                      !!!purrr::set_names(paste0(in_col_vars, "_colvarbis"),
                                          in_col_vars))
      } else {
        character()
      }

    row_var2 <- dplyr::recode(as.character(row_var),
                              !!!purrr::set_names(paste0(in_col_vars, "_colvarbis"),
                                                  in_col_vars))
    tab_row_names2 <- c(tabs_vars2, row_var2)
  } else {
    tab_row_names2 <- tab_row_names
  }



  #Make all calculations with data.table to gain time
  if (use_raw) {
    data.table::setDT(data)
    data.table::setnames(data, as.character(col_var), "col_var", skip_absent = TRUE)

    if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop_all')")
  } else if (nrow(.fine) == 0) {
    stop("data is of length 0 (possibly after filter or na = 'drop_all')")
  }

  # row_var_type <- ifelse(is.numeric(dplyr::pull(data, !!row_var) ),
  #                        "numeric", "factor")
  # col_var_type <- ifelse(is.numeric(dplyr::pull(data, !!col_var) ),
  #                        "numeric", "factor")
  # if (row_var_type == "numeric" & col_var_type == "numeric") {
  #   stop("row_var and col_var are both numeric : only one of them can be")
  # }
  # type <- ifelse(row_var_type == "numeric" | col_var_type == "numeric",
  #                "numeric", "factor")
  #
  # if (type == "numeric") {
  #   num_var <- switch(row_var_type, "numeric" = row_var, "factor" = col_var)
  #   fct_var <- switch(row_var_type, "numeric" = col_var, "factor" = row_var)
  # }

  # if (!is_grouped) {
  #   data <- switch(type,
  #                  "factor"   = dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var),
  #                  "numeric"  = dplyr::group_by(data, !!!tab_vars, !!fct_var     ) )
  # }
  #
  # if (type == "numeric") {
  #   if (rlang::as_name(num_var) %in% dplyr::group_vars(data)) {
  #     data <- dplyr::ungroup(data, !!num_var)
  #   }
  # }

  # nlvs <- nlevels(dplyr::pull(data, !!col_var))

  if (df | num) {
    tabs <-
      data.table::dcast(
        data[, list(n  = if(length(wt) != 0) {integer() } else {.N },
                    wn = if(length(wt) != 0) { sum(eval(wt), na.rm = TRUE) } else {double()}),
             keyby = eval(c(tab_row_names2, "col_var"))],
        formula = ... ~ col_var,
        value.var = if (length(wt) != 0) {c("wn")} else {"n"},
        fill = 0
      )

  } else {
    # DESIGN: aggregation source for the default (factor x factor) path. `use_raw` -> table-by-table
    # (one raw scan per row_var x col_var, current behaviour, kept verbatim). Otherwise roll up the
    # shared finest-grain aggregate `.fine` (built once in tab_many) for this pair. Both feed the
    # SAME dcast below, so everything downstream is byte-identical. Fused runs only when col_var is a
    # factor and there is no col_var/row_var overlap (both guaranteed by tab_many).
    if (use_raw) {
      long <- data[, list(n  = .N,
                          wn = if(length(wt) != 0) { sum(eval(wt), na.rm = TRUE) } else {double()}),
                   keyby = eval(c(tab_row_names2, "col_var"))]
    } else {
      ocv  <- as.character(col_var)
      long <- if (length(wt) != 0) {
        .fine[, list(n = as.integer(sum(n)), wn = sum(wn)), keyby = eval(c(tab_row_names, ocv))]
      } else {
        .fine[, list(n = as.integer(sum(n))),              keyby = eval(c(tab_row_names, ocv))]
      }
      if (ocv != "col_var") data.table::setnames(long, ocv, "col_var")
    }

    tabs <-
      data.table::dcast(
        long,
        formula = ... ~ col_var,
        value.var = if (length(wt) != 0) {c("n", "wn")} else {"n"},
        fill = 0
      )
  }


  if (any(col_var_in_row_var)) {
    colvarbis <- names(tabs)[stringr::str_detect(names(tabs), "_colvarbis$")]
    data.table::setnames(tabs, colvarbis, stringr::str_remove(colvarbis, "_colvarbis$"),
                         skip_absent = TRUE)
  }

  not_fct <- !purrr::map_lgl(dplyr::select(tabs, tidyselect::all_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    tabs[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
         .SDcols = names(not_fct)[not_fct]]
  }


  na_cols <- names(tabs) %in% c("n_NA", "wn_NA", "NA")
  if (any(na_cols)) {
    if (na == "drop") {
      suppressWarnings(tabs[, `:=`(n_NA = NULL, wn_NA = NULL, `NA` = NULL)])
    } else {
      data.table::setcolorder(tabs, c(names(tabs)[!na_cols], names(tabs)[na_cols]))
    }
  }

  na_rows <- tabs %>%
    dplyr::select(!!!tab_vars, !!row_var) %>%
    dplyr::mutate(na_rows = dplyr::if_any(.cols = dplyr::everything(), .fns = is.na)) |>
    dplyr::pull(.data$na_rows)

  if (any(na_rows)) {
    if (na == "drop") {
      tabs <- tabs[-which(na_rows), ]
    } else {
      data.table::setorderv(
        tabs, tab_row_names, na.last = TRUE
      )[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
        .SDcols = tab_row_names]
    }
  }

  num_cols <- tabs %>% purrr::map_lgl(is.numeric)
  num_cols <- names(num_cols)[num_cols]

  if (totaltab %in% c("table", "line")) {
    tabs_totaltab <- switch(
      totaltab[1],
      "table" = tabs[, c(purrr::set_names(rep("Total", length(tab_vars)),
                                          as.character(tab_vars)),
                         purrr::map(.SD, sum, na.rm = TRUE)),
                     .SDcols = num_cols,
                     keyby = eval(as.character(row_var))],

      "line" = tabs[, c(purrr::set_names(rep("Total", length(tab_row_names)),
                                         tab_row_names),
                        purrr::map(.SD, sum, na.rm = TRUE)),
                    .SDcols = num_cols]
    )

    tabs <- rbind(tabs, tabs_totaltab)
    data.table::setorderv(tabs, tab_row_names)
  }



  if ("row" %in% tot) {
    if (length(tab_vars) != 0) {
      group_vars <- rev(purrr::accumulate(as.character(tab_vars) , ~ c(.x, .y)))
      total_vars <- purrr::map(group_vars,
                               ~ c(as.character(tab_vars)[!as.character(tab_vars) %in% .],
                                   as.character(row_var)))
    } else {
      group_vars <- list(character())
      total_vars <- as.character(row_var)
    }


    tabs_tot <-
      purrr::map2_dfr(group_vars, total_vars,
                      ~ tabs[, c(purrr::set_names(rep("Total", length(.y)), eval(.y)),
                                 purrr::map(.SD, sum, na.rm = TRUE)),
                             .SDcols = num_cols,
                             keyby = eval(.x)]
      )

    tabs_tot <-data.table::setorderv(tabs_tot, tab_row_names) |> unique()

    if (totaltab == "line") {
      no_totaltab_line <- dplyr::select(tabs_tot, tidyselect::all_of(tab_row_names)) %>%
        dplyr::transmute(total_line = dplyr::if_any(tidyselect::everything(), ~ . != "Total")) %>%
        tibble::deframe() %>% which()

      tabs_tot <-  tabs_tot[no_totaltab_line, ]
    }

    tabs <- rbind(tabs, tabs_tot)
    data.table::setorderv(tabs, tab_row_names)
  }

  totrow_vector <- dplyr::pull(tabs, !!row_var) == "Total"

  if (length(tab_vars) == 0) {
    tottab_vector <- rep(FALSE, nrow(tabs))
  } else {
    tottab_vector <- dplyr::transmute(tabs, tottab = dplyr::if_all(
      tidyselect::all_of(as.character(tab_vars)),
      ~ . == "Total"
    )) |>
      tibble::deframe()
  }




  if (df | num) {
    if (length(wt) == 0) {
      if ("wn" %in% names(tabs)) tabs[, "wn" := NULL]
    } else {
      if ("n" %in% names(tabs)) tabs[, "n" := NULL]
    }

    if (df) return(as_df_merge_rownames(tabs, rlang::as_name(row_var)))

    return(dplyr::group_by(new_tab(tibble::as_tibble(tabs)), !!!tab_vars))

  } else {
    if (length(wt) == 0) {
      if ("wn" %in% names(tabs)) tabs[, "wn" := NULL]

      text_vars <- !purrr::map_lgl(tabs, is.numeric)
      text_vars <- text_vars[text_vars]

      if ("col" %in% tot) {
        tabs[, "Total" := as.integer(rowSums(tabs[, -text_vars, with = FALSE]))] #Problems if not integer.
      }
      tabs_n <- tabs

    } else {
      text_vars <- !purrr::map_lgl(tabs, is.numeric)
      n_index  <- stringr::str_detect(names(tabs), "^n_")  | text_vars
      wn_index <- stringr::str_detect(names(tabs), "^wn_") | text_vars

      text_vars <- text_vars[text_vars]

      tabs_n  <- data.table::setnames(tabs[, n_index, with = FALSE] ,
                                      function(.x) stringr::str_remove(.x, "^n_" ))
      tabs_wn <- data.table::setnames(tabs[, wn_index, with = FALSE],
                                      function(.x) stringr::str_remove(.x, "^wn_"))

      tabs_wn[, (names(tabs_wn)) := purrr::map(.SD, as.double)]

      if ("col" %in% tot) {
        tabs_n [, "Total" := as.integer(rowSums(tabs_n[, -names(text_vars), with = FALSE] ))] #Problems if not integer.
        tabs_wn[, "Total" := rowSums(tabs_wn[, -names(text_vars), with = FALSE])]
      }

    }
  }
  tabs_text <- tabs[, names(text_vars), with = FALSE] #tibble::as_tibble()
  cols <- purrr::map_lgl(tabs_n, is.numeric)
  cols <- cols[cols]


  #Percentages
  # DESIGN: copy() before each in-place := derivation below (tabs_pct/diff/mean/rr/or). The
  # aggregated table is shared by reference; without copy() a := would mutate the source and
  # every other derived table too (data.table reference semantics).
  if (pct != "no") {
    if (length(wt) == 0) {
      tabs_pct <- data.table::copy(tabs_n)
      tabs_pct[, names(cols) := purrr::map(.SD, as.double), .SDcols = names(cols) ]
    } else {
      tabs_pct <- data.table::copy(tabs_wn)
    }

    switch(
      pct,
      "row"      = tabs_pct[, names(cols) := purrr::map(.SD, ~ . / eval(rlang::sym("Total"))),
                            .SDcols = names(cols)],

      "col"      = tabs_pct[, names(cols) := purrr::map(.SD, ~ . / dplyr::last(.)),
                            by = eval(as.character(tab_vars)),
                            .SDcols = names(cols)],

      "all"      = tabs_pct[, names(cols) := purrr::map(.SD, ~ . / dplyr::last(eval(rlang::sym("Total")))),
                            by = eval(as.character(tab_vars)),
                            .SDcols = names(cols)],

      "all_tabs" = tabs_pct[, names(cols) := purrr::map(.SD, ~ . / dplyr::last(eval(rlang::sym("Total")))),
                            .SDcols = names(cols)]
    )

    tabs_pct[, names(cols) := purrr::map(.SD, ~ tidyr::replace_na(., 0)),
             .SDcols = names(cols)]

    # Phase 2 (1.4.0): each cell's OWN unweighted percentage base (row / column / grand total,
    # per `pct`), stored in the `tot_n` field so a built table is self-sufficient for exact
    # statistics -- this retires detect_totcols() on built tables (decisions §2, §11). Built from
    # the UNWEIGHTED tabs_n and BROADCAST (not divided) with the same denominator logic as the
    # percentages above. tab_plain() runs per col_var, so each col_var's tot_n is its own base
    # (cross-col_var exactness when col_vars have different NA totals is automatic).
    tabs_totn <- data.table::copy(tabs_n)
    tabs_totn[, names(cols) := purrr::map(.SD, as.double), .SDcols = names(cols)]
    switch(
      pct,
      "row"      = tabs_totn[, names(cols) := purrr::map(.SD, ~ as.double(eval(rlang::sym("Total")))),
                             .SDcols = names(cols)],
      "col"      = tabs_totn[, names(cols) := purrr::map(.SD, ~ rep(dplyr::last(.), length(.))),
                             by = eval(as.character(tab_vars)), .SDcols = names(cols)],
      "all"      = tabs_totn[, names(cols) := purrr::map(.SD, ~ rep(dplyr::last(eval(rlang::sym("Total"))), length(.))),
                             by = eval(as.character(tab_vars)), .SDcols = names(cols)],
      "all_tabs" = tabs_totn[, names(cols) := purrr::map(.SD, ~ rep(dplyr::last(eval(rlang::sym("Total"))), length(.))),
                             .SDcols = names(cols)]
    )


    #Differences and odds ratio
    if (ref != "no" & pct %in% c("row", "col")) {
      # Phase 7f: the reference step is the shared tab_apply_reference() (used verbatim here and by the
      # jmvtab tier-3 re-ref). It returns diff / ratio(=tabs_mean) and, when OR/color needs them, rr /
      # or + the ref-col vector; refrows is the ref-row marker. Assign each only when produced so the
      # downstream exists() guards behave exactly as with the former inline locals.
      ref_res <- tab_apply_reference(
        tabs = tabs, tabs_pct = tabs_pct, ref = ref, ref2 = ref2, comp = comp, OR = OR,
        color = color, pct = pct, tab_row_names = tab_row_names, tab_vars = tab_vars,
        row_var = row_var, tottab_vector = tottab_vector, totrow_vector = totrow_vector, cols = cols
      )
      tabs_diff <- ref_res$diff
      tabs_mean <- ref_res$ratio
      if (!is.null(ref_res$rr))             tabs_rr        <- ref_res$rr
      if (!is.null(ref_res$or))             tabs_or        <- ref_res$or
      if (!is.null(ref_res$refcols_vector)) refcols_vector <- ref_res$refcols_vector
      if (!is.null(ref_res$refrows))        refrows        <- ref_res$refrows
    }
  }



  #Make the final table with fmt vectors
  # remove(list = c("tabs_n", "tabs_wn", "tabs_pct", "tabs_diff", "tabs_ci", "refcols_vector", "refrows"))
  tabs_n [, names(text_vars) := NULL]
  if (exists("tabs_wn"  , rlang::current_env(), inherits = F)) tabs_wn  [, names(text_vars) := NULL]
  if (exists("tabs_pct" , rlang::current_env(), inherits = F)) tabs_pct [, names(text_vars) := NULL]
  if (exists("tabs_diff", rlang::current_env(), inherits = F)) tabs_diff[, names(text_vars) := NULL]
  if (exists("tabs_mean", rlang::current_env(), inherits = F)) tabs_mean[, names(text_vars) := NULL]
  if (exists("tabs_rr"  , rlang::current_env(), inherits = F)) tabs_rr  [, names(text_vars) := NULL]
  if (exists("tabs_or"  , rlang::current_env(), inherits = F)) tabs_or  [, names(text_vars) := NULL]
  if (exists("tabs_totn", rlang::current_env(), inherits = F)) tabs_totn[, names(text_vars) := NULL]
  #if (exists("tabs_ci"  , rlang::current_env(), inherits = F)) tabs_ci  [, names(text_vars) := NULL]

  totcol_vector <- names(tabs_n) == "Total"
  NA_reals <- rep(NA_real_, nrow(tabs_n))

  if (ref == "tot") refrows <- rep(FALSE, nrow(tabs_n))

  refrows <- if (exists("refrows", rlang::current_env(), inherits = F)) {
    refrows
  } else {
    rep(FALSE, nrow(tabs_n))
  }

  # Phase 7f-1: display / colour / type / ref / comp / col_var and the digits recycle are
  # column-INVARIANT here (they read only tab_plain-scope scalars/symbols -- pct/OR/wt/color/ref/
  # ref2/row_var/col_var/comp/digits -- never the per-column pmap args ..N), yet the old code
  # recomputed each once per output column inside the closure. Compute them ONCE. new_fmt()
  # recycles the scalar `display` to length(n) (fmt_class.R), so this is byte-identical to the
  # former per-column case_when/if_else/switch. NA_reals (built above at length nrow(tabs_n)) is
  # reused for every all-NA field (identical values, one allocation instead of ~6 per column).
  display_1 <- dplyr::case_when(
    pct %in% c("row", "col") & OR %in% c("OR", "or") ~ "or",
    pct != "no" & OR %in% c("OR_pct", "or_pct")      ~ "or_pct",
    pct != "no"                                      ~ "pct",
    length(wt) != 0                                  ~ "wn" ,
    TRUE                                             ~ "n"
  )
  color_1 <- dplyr::case_when(
    color %in% c("", "no")                            ~ "",
    row_var == "no_row_var" | col_var == "no_col_var" ~ "",

    color %in% c("OR", "or") & pct %in% c("row", "col") &
      # OR %in% c("OR", "or", "OR_pct", "or_pct") &
      ref != "no" & ref2 != "no"
    ~ "OR",

    pct %in% c("row", "col") & ref != "no"            ~ "diff",
    TRUE                                              ~ ""
  )
  type_1   <- dplyr::if_else(pct != "no", pct, "n")
  ref_1    <- switch(as.character(ref), "no" = "", "tot" = "tot", as.character(ref))
  comp_1   <- dplyr::if_else(pct != "no" & ref != "no", comp == "all", NA)
  colvar_1 <- rlang::as_name(col_var)
  digits_v <- vctrs::vec_recycle(as.integer(digits), nrow(tabs_n))

  tabs <-
    list(tabs_n,
         if (exists("tabs_wn"  , rlang::current_env(), inherits = F)) { tabs_wn   } else { list(NA_reals) },
         if (exists("tabs_pct" , rlang::current_env(), inherits = F)) { tabs_pct  } else { list(NA_reals) },
         if (exists("tabs_diff", rlang::current_env(), inherits = F)) { tabs_diff } else { list(NA_reals) },
         if (exists("tabs_mean", rlang::current_env(), inherits = F)) { tabs_mean } else { list(NA_reals) },
         if (exists("tabs_rr"  , rlang::current_env(), inherits = F)) { tabs_rr   } else { list(NA_reals) },
         if (exists("tabs_or"  , rlang::current_env(), inherits = F)) { tabs_or   } else { list(NA_reals) },

         totcol_vector,
         if (exists("refcols_vector", rlang::current_env(), inherits = F)) { refcols_vector } else {
           rep(FALSE, length(cols)) },
         if (exists("tabs_totn", rlang::current_env(), inherits = F)) { tabs_totn } else { list(NA_reals) }
    ) |>
    purrr::pmap_dfc(~ new_fmt(
      display   = display_1,
      digits    = digits_v,
      n         = as.integer(..1),
      wn        = ..2,
      pct       = ..3,
      diff      = ..4,
      # Phase 5 (§3): the `ratio` field is the REFERENCE-RELATIVE ratio (cell / reference per the
      # pct direction = tabs_mean = ..5), the home of the "x2 rule" and the colour ratio measure.
      # The `mean` field holds ONLY actual means now (NA for pct columns) -- the old mean-overload
      # is gone; the colour engine reads `ratio` (get_ratio). The cross-direction RR (..6 =
      # tabs_rr) feeds tabs_or internally and is no longer stored (nothing displays it).
      mean      = NA_reals,
      ratio     = ..5,
      or        = ..7,
      ctr       = NA_reals,
      var       = NA_reals,
      ci_inf    = NA_reals,
      ci_sup    = NA_reals,
      pvalue    = NA_reals,
      #ci        = ,
      in_totrow = totrow_vector,
      in_tottab = tottab_vector,
      in_refrow = refrows,
      totcol    = ..8,
      refcol    = ..9,
      tot_n     = ..10,
      color     = color_1,
      type      = type_1,
      ref       = ref_1,
      #ci_type   = ,
      comp      = comp_1,
      col_var   = colvar_1
    ))

  tabs <- dplyr::bind_cols(tibble::as_tibble(tabs_text), tabs)

  #Rename totals
  if (totaltab %in% c("line", "table") &  totaltab_name != "Total") {
    tabs <- tabs |> dplyr::mutate(dplyr::across(
      tidyselect::all_of(as.character(tab_vars)),
      ~ dplyr::if_else(tottab_vector,
                       true  = factor(totaltab_name, c(levels(.), totaltab_name)),
                       false = .) |>
        forcats::fct_drop()
    ))
  }

  if (length(tab_vars) == 0) {

    if ("row" %in% tot & total_names[1] != "Total") tabs <- tabs |>
        dplyr::mutate(!!row_var := forcats::fct_recode(!!row_var,
                                                       purrr::set_names("Total", total_names[1])))
  } else {
    tabs <- tabs |>
      tidyr::unite(col = "tabs_tot_names", !!!tab_vars, sep = " ", remove = FALSE) |>
      dplyr::mutate(
        !!row_var := dplyr::if_else(
          totrow_vector,
          true  = paste(total_names[1], .data$tabs_tot_names) |>
            forcats::fct_expand(levels(!!row_var)) |>
            forcats::fct_relevel(levels(!!row_var)),
          false = !!row_var) |>
          forcats::fct_drop()
        #forcats::fct_recode(!!row_var,
        #                               purrr::set_names("Total", total_names[1]))
      ) |>
      select(-"tabs_tot_names")
  }

  if ("col" %in% tot & total_names[2] != "Total") tabs <- tabs |>
    dplyr::rename(tidyselect::any_of(purrr::set_names("Total", total_names[2])))


  # with no col_var
  no_col_vars_cols <- get_col_var(tabs) == "no_col_var" #& pct %in% c("row", "col", "all", "all_tabs")
  if (any(no_col_vars_cols) ) {
    tabs <- tabs |>
      dplyr::mutate(n = set_display(.data$n, "n") |> set_type("n") |> as_totcol(FALSE)) |>
      dplyr::relocate("n", .after = tidyselect::last_col())

    if (pct %in% c("row", "col", "all", "all_tabs")) {
      tabs <- tabs |>
        dplyr::rename(tidyselect::any_of(c("pct" = total_names[2]))) |> # if (total_names[2] == "Total")
        dplyr::mutate(pct = as_totcol(pct, FALSE))
         } else {
      tabs <- tabs |> dplyr::select(-dplyr::where(is_totcol))
    }

    if (length(wt) != 0) tabs <- tabs |>
        dplyr::mutate(wn = set_display(.data$n, "wn") |> set_type("n")) |>
        dplyr::relocate("wn", .after = tidyselect::last_col() )
  }

  # # with no row_var : not needed, it's not the simplest way to get a one var table
  # no_row_vars_cols <- any(names(tabs) == "no_row_var") #& pct %in% c("row", "col", "all", "all_tabs")
  # if (no_row_vars_cols) {
  #   tabs <- tabs |>
  #     dplyr::mutate(
  #       dplyr::across(
  #       where(is_fmt),
  #       ~ dplyr::if_else(!is_totrow(.), set_display(., "n"), .)
  #     ),
  #
  #
  #     dplyr::across(all_of(names(tabs)[which(names(tabs) == "no_row_var") + 1L]), is_tottab, .names = "tottab"),
  #
  #     cond = stringr::str_detect(no_row_var, total_names[1]) & !tottab,
  #
  #     no_row_var = dplyr::if_else(cond,
  #                                 true  = forcats::fct_relabel(
  #                                   no_row_var,
  #                                   ~ stringr::str_replace(., total_names[1], "pct"),
  #                                 ),
  #                                 false = no_row_var
  #     ) |>
  #       forcats::fct_relevel("n", after = Inf),
  #
  #    ) |>
  #     dplyr::select(-tottab, - cond)
  #
  #   if (length(wt) != 0) tabs <- tabs |>
  #       dplyr::bind_rows(
  #         dplyr::filter(tabs, no_row_var == "n") |> set_display("wn") |>
  #           dplyr::mutate(no_row_var = factor("wn"))
  #       )
  #
  #   tabs <- tabs |> dplyr::arrange(!!!tab_vars, no_row_var)
  # }


  # if (row_var_type == "numeric") {
  #   tabs <- tabs %>%
  #   tidyr::pivot_wider(names_from = !!fct_var, values_from = !!num_var,
  #                      names_glue = "{.value}_{.name}",
  #                      values_fill = fmt0("mean", digits, type = "mean"))
  #   if (as.character(tab_vars) == "no_tab_vars") {
  #     tabs <- tabs %>% dplyr::mutate(no_row_var = factor("no_row_var")) %>%
  #       dplyr::relocate(no_row_var, .before = 1)
  #   }
  # }


  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                                    ~ length(unique(.)) == 1))

  if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext) %>%
      dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs %>% dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext)
  }
}


# tab_apply_reference() -- the reference step (Phase 7f carve): from the pct data.table + a reference
# selector, derive the reference-relative fields diff (cell - ref), ratio (cell / ref, the "x2 rule")
# and, when OR/color needs it, rr / or; plus the ref-row / ref-col markers. Extracted VERBATIM from
# tab_plain()'s inline block so the FRESH build stays byte-identical AND the jmvtab tier-3 re-ref
# (jmv_tab3_reref) can recompute exactly these ref-dependent fields from a cached table's ref-
# INDEPENDENT pct base, without a new_fmt() rebuild -- one implementation, no forked math.
# Returns a list; elements not computed for the given (pct, OR/color) are NULL, so the caller's
# `exists()`/`is.null()` guards behave identically to the former inline locals.
#' @keywords internal
#' @noRd
tab_apply_reference <- function(tabs, tabs_pct, ref, ref2, comp, OR, color, pct,
                                tab_row_names, tab_vars, row_var, tottab_vector, totrow_vector, cols) {
  tabs_diff <- data.table::copy(tabs_pct)
  tabs_mean <- data.table::copy(tabs_pct)
  refrows   <- NULL

  if (pct == "row") {

    refrows <- tabs |>
      calculate_refrows(ref           = ref,
                        comp          = comp,
                        tab_row_names = tab_row_names,
                        tab_vars      = tab_vars,
                        row_var       = row_var,
                        tottab_vector = tottab_vector,
                        totrow_vector = totrow_vector,
                        #pct           = pct,
                        num_names     = names(cols)
      )

    comp_group <- if (comp == "tab") { as.character(tab_vars) } else { character() }

    tabs_diff[, "ref_rows___" := refrows]

    tabs_diff[,
              c(names(cols), "ref_rows___") := purrr::map_if(
                .SD,
                purrr::map_lgl(.SD, is.numeric),
                ~ . - dplyr::nth(., tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
              ),
              by = eval(comp_group),
              .SDcols = c(names(cols), "ref_rows___")]

    tabs_diff[, "ref_rows___" := NULL] #keep it for ci ?


    # with pct, tabs_mean are for the *2 rule : ratio is used instead of difference
    tabs_mean[, "ref_rows___" := refrows]

    tabs_mean[,
              c(names(cols), "ref_rows___") := purrr::map_if(
                .SD,
                purrr::map_lgl(.SD, is.numeric),
                ~ . / dplyr::nth(., tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
              ),
              by = eval(comp_group),
              .SDcols = c(names(cols), "ref_rows___")]

    tabs_mean[, "ref_rows___" := NULL]



    # Odds ratio (when pct = "row")
    if (OR %in% c("OR", "OR_pct", "or", "or_pct") | color %in% c("or", "OR")) {

      # Relative risks
      tabs_rr <- data.table::copy(tabs_pct)

      refcols <- dplyr::nth(names(cols),
                            diff_index(ref2,
                                       row_var   = dplyr::pull(tabs_rr, !!row_var),
                                       num_names = names(cols),
                                       pct       = "col"))
      refcols_vector <- names(cols) == refcols


      if (length(refcols) != 0 & !is.na(refcols)) {
        tabs_rr[, names(cols) := purrr::map(.SD,~ ./eval(rlang::sym(refcols)) ),
                .SDcols = names(cols)]

      } else {
        remove(refcols, refcols_vector) # test if exists after
        warning(paste0(
          "in ref2 = '", ref2, "' , no columns were found as reference for comparison ; ",
          "to remove this warning, precise the value of ref ",
          "until there is one column matched"
        ))
        tabs_rr[, names(cols) := purrr::map(.SD, ~ NA_real_), .SDcols = names(cols)]
      }

      # Odds ratio (binary) or relative risk ratios
      tabs_or <- data.table::copy(tabs_rr)
      tabs_or[, "ref_rows___" := refrows]

      tabs_or[,
              c(names(cols), "ref_rows___") := purrr::map_if(
                .SD,
                purrr::map_lgl(.SD, is.numeric),
                ~ ./dplyr::nth(., tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
              ),
              by = eval(comp_group),
              .SDcols = c(names(cols), "ref_rows___")]

      tabs_or[, "ref_rows___" := NULL]
    }

  }


  if (pct == "col") {
    refcols <- dplyr::nth(names(cols), diff_index(ref,
                                                  num_names = names(cols),
                                                  pct       = pct))
    refcols_vector <- names(cols) == refcols

    if (length(refcols) != 0 & !is.na(refcols)) {
      tabs_diff[, names(cols) := purrr::map(.SD,~ . - eval(rlang::sym(refcols)) ),
                .SDcols = names(cols)]

      #   with pct, tabs_mean are for the *2 rule : ratio is used instead of difference
      tabs_mean[, names(cols) := purrr::map(.SD,~ . / eval(rlang::sym(refcols)) ),
                .SDcols = names(cols)]
    } else {
      warning(paste0(
        "in ref = '", ref, "' , no columns were found as reference for comparison ; ",
        "to remove this warning, precise the value of ref ",
        "until there is one column matched"
      ))
      tabs_diff[, names(cols) := purrr::map(.SD, ~ NA_real_), .SDcols = names(cols)]
      tabs_mean[, names(cols) := purrr::map(.SD, ~ NA_real_), .SDcols = names(cols)]
    }


    # Odds ratio (when pct = "col")
    if (OR %in% c("OR", "OR_pct", "or", "or_pct") | color %in% c("or", "OR")) {

      # Relative risks
      tabs_rr <- data.table::copy(tabs_pct)

      refrows <- tabs |>
        calculate_refrows(ref           = ref2,
                          comp          = comp,
                          tab_row_names = tab_row_names,
                          tab_vars      = tab_vars,
                          row_var       = row_var,
                          tottab_vector = tottab_vector,
                          totrow_vector = totrow_vector,
                          #pct           = pct,
                          num_names     = names(cols)
        )

      comp_group <- if (comp == "tab") { as.character(tab_vars) } else { character() }

      tabs_rr[, "ref_rows___" := refrows]

      tabs_rr[,
              c(names(cols), "ref_rows___") := purrr::map_if(
                .SD,
                purrr::map_lgl(.SD, is.numeric),
                ~ ./dplyr::nth(., tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
              ),
              by = eval(comp_group),
              .SDcols = c(names(cols), "ref_rows___")]

      tabs_rr[, "ref_rows___" := NULL]


      # Odds ratio (binary) or relative risk ratios
      tabs_or <- data.table::copy(tabs_rr)

      if (length(refcols) != 0 & !is.na(refcols)) {
        tabs_or[, names(cols) := purrr::map(.SD,~ ./eval(rlang::sym(refcols)) ),
                .SDcols = names(cols)]

      } else {
        tabs_or[, names(cols) := purrr::map(.SD, ~ NA_real_), .SDcols = names(cols)]
        # remove(refcols, refcols_vector) # test if exists after
      }
    }
  }

  list(
    diff           = tabs_diff,
    ratio          = tabs_mean,
    rr             = if (exists("tabs_rr",        inherits = FALSE)) tabs_rr        else NULL,
    or             = if (exists("tabs_or",        inherits = FALSE)) tabs_or        else NULL,
    refcols_vector = if (exists("refcols_vector", inherits = FALSE)) refcols_vector else NULL,
    refrows        = refrows
  )
}






#' Means table
#' @description Cross categorical variables with numeric variables, and get a table
#' of means and standard deviations.
#' @param data A data frame.
#' @param row_var The row variable, which will be printed with one level per line. If
#' numeric, it will be used as a factor.
#' @param col_vars The numeric variables, which will appear in columns :
#' means and standard deviation are calculated for each levels of `row_var` and `tab_vars`.
#' @param tab_vars  <\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
#' a subtable is made for each combination of levels of the
#' selected variables. Leave empty to make a simple cross-table. All tab variables
#' are converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param digits The number of digits to print, as a single integer.
#' @param na The policy to adopt for missing values in row and tab variables (factors),
#' as a single string.
#'  \itemize{
#'   \item \code{"keep"}: by default, \code{NA}'s of row and tab variables
#'   are printed as an explicit `"NA"` level.
#'   \item \code{"drop"}: remove `NA`'s in row and tab variables.
#'   }
#' `NA`s in numeric variables are always removed when calculating means. For that reason
#' the `n` field of each resulting \code{\link{fmt}} column, used to calculate confidence
#' intervals, only takes into account the complete observations (without `NA`).
#' To drop all rows with `NA` in any numeric variable first, use \code{\link{tab_prepare}}
#' or \code{\link{tab_many}} with the `na_drop_all` argument.
#' @param totaltab The total table,
#' if there are subtables/groups (i.e. when \code{tab_vars} is provided) :
#'  \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param tot The totals :
#'  \itemize{
#'   \item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
#'   columns.
#'   \item \code{"row"}: only total rows.
#'   \item \code{"col"}: only total column.
#'   \item \code{"no"}: remove all totals (after calculations if needed).
#'  }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param ref The reference cell to calculate differences and ratios
#'  (used to print \code{colors}) :
#'  \itemize{
#'   \item \code{"auto"}: by default, cell difference from the corresponding total
#'   (rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
#'   used for `diff` ; cell ratio from the first line (or col) is use for `OR`
#'   (odds ratio/relative risks ratio).
#'   \item \code{"tot"}: totals are always used.
#'   \item \code{"first"}: calculate cell difference or ratio from the first cell
#' of the row or column (useful to color temporal developments).
#'   \item \code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.
#'   \item \code{"regex"}: when `ref` is a string, it it used as a regular expression,
#'   to match with the names of the rows (or columns). Be precise enough to match only one
#'   column or row, otherwise you get a warning message.
#'   \item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
#' }
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param color Which measure(s) to color, on which channel -- see \code{\link{tab}} for the full
#'   syntax (\code{FALSE}/\code{TRUE}, a measure name, or a two-channel \code{c(text, background)}
#'   vector). For numeric means the useful measures are \code{"diff"} (standardized, Glass's
#'   \eqn{\Delta}) and \code{"ratio"} (mean ratio); \code{TRUE} uses \code{"ratio"}. Default
#'   \code{"auto"} keeps the historical behavior.
#' @param color_signif How significance gates the color (\code{"ignore"} / \code{"grey_non_signif"}
#'   / \code{"color_all_signif"}) -- see \code{\link{tab}}.
#' @param subtext A character vector to print rows of legend under the table.
#' @param ci The type of confidence intervals to calculate, passed to \code{\link{tab_ci}}
#'  (automatically added if needed for \code{color}).
#'   \itemize{
#'    \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'    \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'    relative total cell (or relative first cell when \code{ref = "first"}).
#'    \item \code{"auto"}: \code{ci = "diff"} for means and row/col percentages,
#'      \code{ci = "cell"} for frequencies ("all", "all_tabs").
#'   }
#' @param conf_level The confidence level for the confidence intervals,
#'  as a single numeric between 0 and 1. Default to 0.95 (95%).
#' @param stars Logical (default \code{TRUE}, or `options("tabxplor.stars")` when \code{NULL}).
#' With \code{ci = "diff"}, print per-cell Welch t significance stars for the difference from the
#' reference row; the mean-diff interval then uses the Welch t quantile (z when \code{FALSE}).
#' @param num Set to \code{TRUE} to obtain a table with normal numeric vectors (not `fmt`).
#' @param df  Set to \code{TRUE} to obtain a plain data.frame (not a `tibble`),
#' with normal numeric vectors (not `fmt`). Useful, for example, to pass the table to
#' correspondence analysis with \pkg{FactoMineR}.
#' @param .fine,.by_table Internal. `.fine` is a pre-computed moment-sum aggregate (from
#' \code{tab_aggregate_num()}) to adopt instead of scanning the raw data; `.by_table` forces
#' the table-by-table path (a fresh scan). Both default to the fresh-scan behaviour.
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}. If \code{...} (\code{tab_vars})
#'  are provided, a \code{tab} of class \code{tabxplor_grouped_tab}.
#' All non-text columns are \code{\link{fmt}} vectors of class \code{tabxplor_fmt},
#' storing all the data necessary to print formats and colors. Columns with \code{row_var}
#' and \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples
#' \donttest{
#' data <- dplyr::storms %>% tab_prepare(category, wind, na_drop_all = wind)
#' tab_num(data, category, wind, tot = "row", color = "after_ci")
#' }
tab_num <- function(data, row_var, col_vars, tab_vars, wt,
                    color = "auto", color_signif = "ignore",
                    na = c("keep", "drop", "drop_fct", "drop_num"),
                    ref = "tot", comp = c("tab", "all"),
                    ci = NULL, conf_level = 0.95, stars = NULL, #ci_visible = FALSE,
                    totaltab = "line", totaltab_name = "Ensemble",
                    tot = NULL, total_names = "Total",
                    subtext = "", digits = 0, num = FALSE, df = FALSE,
                    .fine = NULL, .by_table = FALSE
) {

  row_var_quo <- rlang::enquo(row_var)
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data %>% dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::ensym(row_var)
  }

  col_vars <- rlang::enquo(col_vars)
  if (quo_miss_na_null_empty_no(col_vars)) {
    data     <- data %>% dplyr::mutate(no_col_var = factor("n"))
    col_vars <- rlang::syms("no_col_var")
    pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else {
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
  }

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character()
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }

  tab_row_names <- purrr::map_chr(c(tab_vars, row_var), rlang::as_name)

  #forbid the level to have the name of the variable, othewise problems ----



  vctrs::vec_assert(ref, size = 1)
  # ci    <-  ci[1]
  # stopifnot(ci %in% c("diff", "cell", "no", ""))
  comp  <-  comp[1]
  stopifnot(comp %in% c("tab", "all", "") | is.na(comp) | is.null(comp))
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), length(col_vars))
  na <- na[1]
  stopifnot(na %in% c("keep", "drop"))
  vctrs::vec_assert(totaltab_name, size = 1)
  total_names  <- vctrs::vec_recycle(total_names, 2)
  # Phase 5: `color` accepts the new forms (FALSE/TRUE/scalar/c(text,bg)/named) + `color_signif`.
  # Parse to a spec, run the pipeline on the text-channel legacy string, finalize on the result.
  color_spec <- normalize_color_spec(color, color_signif)
  color      <- color_spec$legacy
  stopifnot(color %in% c("auto", "diff", "diff_ci", "after_ci", "no", ""))

  # Phase 7b: the numeric color = "auto" resolution is the means arm of the shared cascade,
  # in resolve_color_auto_num() (R/tab-resolve.R). A mean has no contrib/OR notion, so it keys
  # only on whether a real difference is possible (a `ref`, and ci != "cell"). Under tab_build()
  # this receives color_num (never "OR"/"contrib"); direct tab_num() callers also land here.
  color <- resolve_color_auto_num(color, ref, ci, row_var, col_vars)

  if (row_var == "no_row_var" | "no_col_var" %in% col_vars) color <- ""

  if (color %in% c("diff", "diff_ci", "after_ci") & ref %in% c("no", "")) {
    warning("since color = 'diff', ref must be provided and was set to 'tot'")
    ref <- "tot"
  }

  if (!is.null(ci)) if (color %in% c("diff_ci", "after_ci") & ci != "diff")
    rlang::warn(
      paste0("since color = '", color, "', the confidence intervals of cells differences",
             " from totals (or others cells) must be calculated : ci was set to 'diff' ")
    )
  if (color %in% c("diff_ci", "after_ci")) {
    ci <- "diff"
  } else {
    if (is.null(ci)) ci <- "no"
  }

  if (ci == "diff" & ref %in% c("no", "")) {
    warning("since ci = 'diff', a diff was added with ref = 'tot'")
    ref <- "tot"
  }

  ci_visible <- ci == "cell"


  if (is.null(tot)) {
    tot <- if (ref == "tot" & color %in% c("diff", "diff_ci", "after_ci")) {"row"} else {"no"}

  } else {
    stopifnot(all(tot %in% c("row", "col", "both", "no", "")))
    if (tot[1] == "both") tot <- "row"

    if ((color %in% c("diff", "diff_ci", "after_ci") | ref == "tot") & !tot %in% "row") {
      #warning("since color = '", color, "' and ref = 'tot', a total row was added")
      tot <- "row"
    }
  }

  # LEAF resolution (Phase 7b): ref = "auto" is type-specific and intentionally stays here, NOT
  # in tab_resolve_settings(). A mean's reference is always its total row ("tot"); tab_num() has
  # no OR mode, so the factor rule's "first" branch (tab_plain, below) can never fire for means.
  # This is the documented (byte-identical) counterpart of tab_plain()'s ref = "auto" rule.
  if (ref == "auto") {
    ref <- "tot"  # ref <- if (OR != "no") {"first"} else {"tot"}
  }

  if (comp == "all" & length(tab_vars) == 0) comp <- "tab"

  if (length(tab_vars) == 0) totaltab <- "no"

  if (comp[1] == "all" & ref == "tot" & !totaltab %in% c("table", "line")) {
    warning("since comp = 'all', a total table was added to compare with")
    totaltab <-  "line"
  }

  if (comp[1] == "all" & !ref %in% c("tot", "no", "") & totaltab != "table") {
    warning("since comp = 'all', a full total table was added to compare with")
    totaltab <- "table"
  }




  # Phase 7d: aggregate-injection seam (mirrors tab_plain's `.fine`). When tab_build() supplies a
  # prebuilt moment-sum aggregate (`.fine`, from tab_aggregate_num()), skip the raw-data prep + scan
  # and adopt it. `use_raw` keeps the table-by-table path intact; forced on by `.by_table` and always
  # for the df/num mean-direct paths (never fused). The moment MATH lives once in num_moment_scan()
  # (R/tab-agg.R), shared with the producer.
  use_raw <- .by_table || is.null(.fine) || df || num

  if (use_raw) {
    data <- data %>%
      dplyr::select(!!!tab_vars, !!row_var, !!!col_vars, !!wt) %>%
      dplyr::mutate(dplyr::across((!!wt | tidyselect::all_of(as.character(col_vars))) &
                                    !where(is.numeric), as.numeric)
      )

    #Faster with data.table
    data.table::setDT(data)

    # Remove NA's in factors here, otherwise they are kept in totals after
    if (na == "drop") data <- stats::na.omit(data, tab_row_names) # 0.5 sec

    if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop')")
  } else if (nrow(.fine) == 0) {
    stop("data is of length 0 (possibly after filter or na = 'drop')")
  }

  # row_var_type <- ifelse(is.numeric(dplyr::pull(data, !!row_var) ),
  #                        "numeric", "factor")
  # col_var_type <- ifelse(is.numeric(dplyr::pull(data, !!col_var) ),
  #                        "numeric", "factor")
  # if (row_var_type == "numeric" & col_var_type == "numeric") {
  #   stop("row_var and col_var are both numeric : only one of them can be")
  # }
  # type <- ifelse(row_var_type == "numeric" | col_var_type == "numeric",
  #                "numeric", "factor")
  #
  # if (type == "numeric") {
  #   num_var <- switch(row_var_type, "numeric" = row_var, "factor" = col_var)
  #   fct_var <- switch(row_var_type, "numeric" = col_var, "factor" = row_var)
  # }

  # if (!is_grouped) {
  #   data <- switch(type,
  #                  "factor"   = dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var),
  #                  "numeric"  = dplyr::group_by(data, !!!tab_vars, !!fct_var     ) )
  # }
  #
  # if (type == "numeric") {
  #   if (rlang::as_name(num_var) %in% dplyr::group_vars(data)) {
  #     data <- dplyr::ungroup(data, !!num_var)
  #   }
  # }

  # nlvs <- nlevels(dplyr::pull(data, !!col_var))

  #        "numeric" = data %>%
  #          dplyr::summarise(!!num_var := stats::weighted.mean(!!num_var, !!wt, na.rm = TRUE),
  #                           .groups = "drop")
  # ) %>%


  #data <- data |> dplyr::group_by(!!!tab_vars, !!row_var)

  # if (df | num) {
  #  tabs <- data |>
  #     dplyr::summarise(across(tidyselect::all_of(as.character(col_vars)), list(
  #       mean = ~ stats::weighted.mean(., !!wt, na.rm = TRUE)
  #     )),
  #     .groups = "drop"
  #     )
  #
  # } else {


  if (!use_raw) {
    # Adopt the prebuilt moment aggregate. copy(): the factor-key coercion + na-order relabel just
    # below mutate `tabs` by reference, so a reused/cached `.fine` must not be corrupted.
    tabs <- data.table::copy(.fine)

  } else if (df | num) {
    tabs <-
      if (length(wt) == 0) {
        data[, purrr::map(.SD,  ~mean(., na.rm = TRUE)),
             .SDcols = as.character(col_vars),
             keyby = c(tab_row_names)]

      } else {
        data[, purrr::map_if(.SD,
                             names(.SD) != as.character(wt),
                             ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                             .else = ~ NA_real_),
             .SDcols = as.character(c(col_vars, wt)),
             keyby = c(tab_row_names)][, wt := NULL]
      }

  } else {
    # Phase 2/7d: sufficient moment sums (n [, wn], s1 = Sigma[w]x, s2 = Sigma[w]x^2) in ONE grouped
    # pass; mean/var are derived afterwards by num_derive_stats() (R/tab-agg.R), replacing the old
    # weighted.var double scan. The scan itself lives in num_moment_scan() (R/tab-agg.R) so tab_num()
    # and tab_aggregate_num() share it verbatim. (The moment sums are ADDITIVE, so the total-row and
    # total-table blocks below are num_rollup()s of this aggregate, not extra N-scans.)
    tabs <- num_moment_scan(data, tab_row_names, col_vars, wt)
  }

  not_fct <- !purrr::map_lgl(dplyr::select(tabs, tidyselect::any_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    # not_fct_names <- names(not_fct)[not_fct]
    tabs[, names(not_fct)[not_fct] := purrr::map(.SD, as.factor),
         .SDcols = names(not_fct)[not_fct]]
  }

  if (na == "keep") {
    data.table::setorderv(
      tabs, tab_row_names, na.last = TRUE
    )[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
      .SDcols = tab_row_names]
  }





  # tabs <- data |>
  #   dplyr::summarise(dplyr::across(tidyselect::all_of(as.character(col_vars)), ~ new_fmt(
  #     display = "mean",
  #     digits  = as.integer(digits),
  #     n       = dplyr::n(),
  #     wn      = if (wt != "no_weight") {sum(!!wt, na.rm = TRUE)} else {NA_real_},
  #     mean    = stats::weighted.mean(., !!wt, na.rm = TRUE),
  #     var     = weighted.var(., !!wt, na.rm = TRUE),
  #     type    = "mean",
  #     col_var = dplyr::cur_column()
  #   )),
  #   .groups = "drop")
  # #}
  #
  # na_rows <- tabs %>%
  #   dplyr::select(!!!tab_vars, !!row_var) %>%
  #   dplyr::mutate(na_rows = dplyr::if_any(.cols = dplyr::everything(), .fns = is.na)) |>
  #   dplyr::pull(.data$na_rows)
  #
  # if (any(na_rows)) {
  #   if (na == "drop") {
  #     tabs <- tabs[-which(na_rows), ]
  #   } else {
  #     tabs <- tabs %>%
  #       dplyr::mutate(dplyr::across(tidyselect::all_of(tab_row_names),
  #                                   ~ forcats::fct_na_value_to_level(., level = "NA"))) %>%
  #       dplyr::arrange(!!!tab_vars, !!row_var)
  #
  #     #data.table::setorderv(
  #     #  tabs, tab_row_names, na.last = TRUE
  #     #)[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
  #     #  .SDcols = tab_row_names]
  #   }
  # }


  #Calculate means and variances for all totals and subtotals
  # Phase 2 rollup: the total rows and the total table below are subtotals of the main aggregate.
  # Its moment sums (n [, wn], s1, s2) are ADDITIVE, so both are computed as ROLLUPS of a captured
  # copy of the main aggregate via num_rollup() (R/tab-agg.R) instead of two extra N-row re-scans.
  moment_cols <- setdiff(names(tabs), tab_row_names)
  main_agg    <- if (!(df | num)) data.table::copy(tabs) else NULL

  if ("row" %in% tot | totaltab %in% c("line", "table")) {
    if (length(tab_vars) != 0) {
      group_vars <- c(as.character(tab_vars)) |> purrr::accumulate(~ c(.x, .y))
      group_vars <- c(rev(group_vars), list(character()))
    } else {
      group_vars <- list(character())
    }
    # Phase 6e KNOWN-BUG fix: when tot="no" but a total table is still built, keep ONLY the
    # grand total -- but as a length-1 LIST (the grand-total key `character()`), not the bare
    # `character()` that `dplyr::last()` returned. The bare vector made `map_dfr()` iterate zero
    # times -> an empty `tabs_tot` -> the `setorderv()`/`rbind()` below crashed with tab_vars
    # (and silently dropped the total table without). Now the grand total is actually computed.
    if (!"row" %in% tot) group_vars <- group_vars[length(group_vars)]


    if (df | num) {
      if (length(wt) == 0) {
        suppressWarnings(
          tabs_tot <-
            purrr::map_dfr(
              group_vars,
              ~ data[, c(purrr::set_names(rep("Total", length(c(tab_vars[!tab_vars %in% .], row_var))),
                                          as.character(c(tab_vars[!tab_vars %in% .], row_var)) ),

                         purrr::map(.SD,  ~mean(., na.rm = TRUE)) ),
                     .SDcols = as.character(col_vars),
                     keyby = eval(.)][ , as.character(tab_vars) := purrr::map(.SD, as.factor),
                                       .SDcols = as.character(tab_vars)]
            )
        )
      } else {
        suppressWarnings(
          tabs_tot <-
            purrr::map_dfr(
              group_vars,
              ~ data[, c(purrr::set_names(rep("Total", length(c(tab_vars[!tab_vars %in% .], row_var))),
                                          as.character(c(tab_vars[!tab_vars %in% .], row_var)) ),

                         purrr::map_if(.SD,
                                       names(.SD) != as.character(wt),
                                       ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                                       .else = ~ NA_real_)),
                     .SDcols = as.character(c(col_vars, wt)),
                     keyby = eval(.)][, wt := NULL][ , as.character(tab_vars) := purrr::map(.SD, as.factor),
                                                     .SDcols = as.character(tab_vars)]
            )
        )
      }

    } else {
      # Phase 2 rollup: the total rows are subtotals of the main aggregate (moment sums are
      # additive), so sum them by each group_vars subset instead of re-scanning N rows. One path
      # for weighted and unweighted -- moment_cols carries _wn only when weighted.
      tabs_tot <- purrr::map_dfr(
        group_vars,
        ~ num_rollup(
          main_agg,
          by           = .,
          drop_keys    = as.character(c(tab_vars[!tab_vars %in% .], row_var)),
          moment_cols  = moment_cols,
          tab_vars_chr = as.character(tab_vars)
        )
      )
    }

    not_fct <- !purrr::map_lgl(dplyr::select(tabs_tot, tidyselect::any_of(tab_row_names)), is.factor)
    if (any(not_fct)) {
      # not_fct_names <- names(not_fct)[not_fct]
      tabs_tot[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
               .SDcols = names(not_fct)[not_fct]]
    }

    # Fixed in Phase 6e (the grand-total grouping-set is now a length-1 LIST, see above) and
    # golden-locked by n_ci_tabvars / n_ci_tabvars_all; num_rollup() guarantees every tab_var is
    # present in tabs_tot. Phase 7d belt-and-suspenders: restrict the reorder/relabel to the
    # tab_vars actually present, so it is byte-identical in every real case (intersect == the full
    # set) and can only differ on the genuinely-absent-column path that used to crash.
    if (na == "keep" & length(tab_vars) != 0) {
      tv <- intersect(as.character(tab_vars), names(tabs_tot))
      if (length(tv) != 0) {
        data.table::setorderv(
          tabs_tot, tv, na.last = TRUE
        )[, (tv) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
          .SDcols = tv]
      }
    }

    tabs <- rbind(tabs, tabs_tot)
    data.table::setorderv(tabs, tab_row_names)


    # tabs_tot <-
    #   purrr::map_dfr(ungroup_vars,
    #                  ~ dplyr::ungroup(data, !!!.x) |> #win nearly no time compared to group_by(!!!tab_vars)
    #                    dplyr::summarise(
    #                      dplyr::across(tidyselect::all_of(purrr::map_chr(.x, rlang::as_name)), ~ factor("Total")),
    #                      dplyr::across(tidyselect::all_of(as.character(col_vars)), function(.var) new_fmt(
    #                        display   = "mean",
    #                        digits    = as.integer(digits),
    #                        n         = dplyr::n(),
    #                        wn        = if (wt != "no_weight") {sum(!!wt, na.rm = TRUE)} else {NA_real_},
    #                        mean      = stats::weighted.mean(.var, !!wt, na.rm = TRUE),
    #                        var       = weighted.var(.var, !!wt, na.rm = TRUE),
    #                        type      = "mean",
    #                        col_var   = dplyr::cur_column(),
    #                        in_totrow = TRUE,
    #                        in_tottab = length(.x) == length(tab_vars) + 1L,
    #                      )),
    #                      .groups = "drop")
    #   )
    #
    # na_rows <- tabs_tot %>%
    #   dplyr::select(!!!tab_vars) %>%
    #   dplyr::transmute(na_rows = dplyr::if_any(.cols = dplyr::everything(), .fns = is.na)) |> tibble::deframe()
    #
    # if (any(na_rows)) {
    #   if (na == "drop") {
    #     tabs_tot <- tabs_tot[-which(na_rows), ]
    #   } else {
    #     tabs_tot <- tabs_tot %>%
    #       dplyr::mutate(dplyr::across(tidyselect::all_of(as.character(tab_vars)),
    #                                   ~ forcats::fct_na_value_to_level(., level = "NA"))) %>%
    #       dplyr::arrange(!!!tab_vars, !!row_var)
    #
    #   }
    # }
  }

  #Calculate means and variances for total table
  if (totaltab == "table") {

    if (df | num) {
      if (length(wt) == 0) {
        tabs_totaltab <-
          data[, c(purrr::set_names(rep("Total", length(tab_vars)), as.character(tab_vars)),
                   purrr::map(.SD,  ~mean(., na.rm = TRUE)) ),
               .SDcols = as.character(col_vars),
               keyby = eval(as.character(row_var))]

      } else {
        tabs_totaltab <-
          data[, c(purrr::set_names(rep("Total", length(tab_vars)), as.character(tab_vars)),
                   purrr::map_if(.SD,
                                 names(.SD) != as.character(wt),
                                 ~ round(stats::weighted.mean(., eval(wt), na.rm = TRUE), 10),
                                 .else = ~ NA_real_)),
               .SDcols = as.character(c(col_vars, wt)),
               keyby = eval(as.character(row_var))][, wt := NULL]
      }

    } else {
      # Phase 2 rollup: the total table is the main aggregate summed by row_var (its tab_vars
      # collapsed to "Total"), reusing the additive moment sums instead of a third N-row re-scan.
      tabs_totaltab <- num_rollup(
        main_agg,
        by           = as.character(row_var),
        drop_keys    = as.character(tab_vars),
        moment_cols  = moment_cols,
        tab_vars_chr = as.character(tab_vars)
      )
    }

    not_fct <- !purrr::map_lgl(dplyr::select(tabs_totaltab, tidyselect::any_of(tab_row_names)), is.factor)
    if (any(not_fct)) {
      # not_fct_names <- names(not_fct)[not_fct]
      tabs_totaltab[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
                    .SDcols = names(not_fct)[not_fct]]
    }

    if (na == "keep") {
      data.table::setorderv(
        tabs_totaltab, as.character(row_var), na.last = TRUE
      )[, as.character(row_var) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
        .SDcols = as.character(row_var)]
    }


    tabs <- rbind(tabs, tabs_totaltab)
    data.table::setorderv(tabs, tab_row_names)


    # tabs_totaltab <- dplyr::group_by(data, !!row_var) |>
    #   dplyr::summarise(
    #     dplyr::across(tidyselect::all_of(purrr::map_chr(tab_vars, rlang::as_name)),
    #                   ~ factor("Total")),
    #     dplyr::across(tidyselect::all_of(as.character(col_vars)), function(.var) new_fmt(
    #       display   = "mean",
    #       digits    = as.integer(digits),
    #       n         = dplyr::n(),
    #       wn        = if (wt != "no_weight") {sum(!!wt, na.rm = TRUE)} else {NA_real_},
    #       mean      = stats::weighted.mean(.var, !!wt, na.rm = TRUE),
    #       var       = weighted.var(.var, !!wt, na.rm = TRUE),
    #       type      = "mean",
    #       col_var   = dplyr::cur_column(),
    #       in_totrow = TRUE,
    #       in_tottab = TRUE
    #     )),
    #     .groups = "drop")
  }

  if (df | num) {
    if (df) return(as_df_merge_rownames(tabs, rlang::as_name(row_var)))

    return(dplyr::group_by(new_tab(tibble::as_tibble(tabs)),
                           !!!tab_vars))
  }

  # Phase 2 (1.4.0): derive per-col_var mean and variance from the moment sums (<v>_n [, _wn],
  # _s1, _s2) the aggregate + totals scans produced, in ONE pass over the small bound table.
  # Reproduces the pre-1.4.0 stats::var (unweighted) / weighted.var (weighted) definitions
  # exactly and removes the old weighted.var double scan. See R/tab-agg.R.
  tabs <- num_derive_stats(tabs, col_vars, weighted = length(wt) != 0)



  totrow_vector <- dplyr::pull(tabs, !!row_var) == "Total"
  if (length(tab_vars) == 0) {
    tottab_vector <- rep(FALSE, nrow(tabs))
  } else {
    tottab_vector <- dplyr::transmute(tabs, tottab = dplyr::if_all(
      tidyselect::all_of(as.character(tab_vars)),
      ~ . == "Total"
    )) |>
      tibble::deframe()
  }
  comp_group <- if (comp == "tab") { as.character(tab_vars) } else { character() }

  diff_index_mean <-  function(ref, row_var, num_names) { #needed for ci too
    if (ref == "tot"   ) return(-1L)
    if (ref == "first" ) return(1L )
    if (is.numeric(ref)) return(as.integer(ref[1]))

    index <- which(stringr::str_detect(row_var, ref))

    if (length(index) >= 2) warning(paste0(
      "with ref = '", ref, "' , several rows were found as ",
      "reference for comparison ; only the first was kept ; ",
      "to remove this warning, precise the value of ref ",
      "until there is only one row_var level matched"
    ))

    index <- tidyr::replace_na(dplyr::first(index), 0)
    if (length(index) == 0) index <- 0

    index
  }


  #Differences and confidence intervals
  if (!ref %in% c("no", "") | ci %in% c("cell", "diff")) {

    if (ref != "tot") {
      refrows <-
        if(comp == "tab") {
          tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
            dplyr::group_by(!!!tab_vars) |>
            dplyr::transmute(
              var =
                dplyr::row_number() == if (diff_index_mean(ref, !!row_var) == -1) {
                  dplyr::n()
                } else {
                  diff_index_mean(ref, !!row_var)
                }
            ) |>
            dplyr::pull("var")

        } else {
          tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
            dplyr::mutate(tottab_vector = tottab_vector) |>
            dplyr::group_by(!!!tab_vars) |>
            dplyr::transmute(
              var = dplyr::if_else(
                condition = .data$tottab_vector,
                true  = dplyr::row_number() == if (diff_index_mean(ref, !!row_var) == -1) {
                  dplyr::n()
                } else {
                  diff_index_mean(ref, !!row_var)
                },
                false = FALSE
              )
            ) |>
            dplyr::pull("var")
        }

      if (!any(refrows)) {
        warning(paste0(
          "in ref = '", ref, "' , no rows were found as reference for comparison ; ",
          "to remove this warning, precise the value of ref ",
          "until there is one row_var level matched"
        ))
      }

    } else {
      refrows <- if (comp == "tab") { totrow_vector } else { totrow_vector & tottab_vector }
    }
    #tabs_diff$DIPLOME[refrows] |> as.character()



    refrows <- tidyr::replace_na(refrows, FALSE)

    tabs[, "ref_rows___" := refrows]

    #Differences and ratios
    # Phase 2 (1.4.0): the numeric `diff` field is now a real DIFFERENCE (cell_mean - ref_mean);
    # the cell/ref RATIO (the old `diff` value) moves to the `ratio` field. Numeric coloring keeps
    # reading `ratio` against mean_breaks until Phase 5 (D3 interim). See decisions doc §3, §Phasing.
    if (!ref %in% c("no", "") ) {
      tabs[, paste0(col_vars, "_diff") := purrr::map(
        rlang::syms(paste0(col_vars, "_mean")),
        ~ eval(.) - dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
      ),
      by = eval(comp_group)]
      tabs[, paste0(col_vars, "_ratio") := purrr::map(
        rlang::syms(paste0(col_vars, "_mean")),
        ~ eval(.) / dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
      ),
      by = eval(comp_group)]
    }




    # Confidence intervals (Phase 3a): store real bounds (<v>_ci_inf / <v>_ci_sup) + the
    # per-cell significance <v>_pvalue, via the ci_pivot() engine (R/tab-agg.R). Means use the
    # z pivot for cell CIs and the Welch-t pivot for diff CIs when stars are on; the pvalue is
    # the Welch-t inversion p (universal CI-inclusion) -- NA for cell CIs and when stars are
    # opted out (one interval eval). See dev/tabxplor_1.4.0_decisions.md §20.
    if (ci %in% c("cell", "diff")) {
      stars_on <- if (is.null(stars)) getOption("tabxplor.stars", TRUE) else stars
      want_p   <- isTRUE(stars_on) && ci == "diff"
      cvs      <- as.character(col_vars)

      # Effective sample size per cell for the CI/test (§14): Kish n_eff = wn^2 / Sigma(w^2)
      # when opted in, else the unweighted count. The DISPLAYED `n` field stays the real count;
      # only the inference uses this. (Factor-side Kish is deferred -- open item.)
      kish <- isTRUE(getOption("tabxplor.kish_neff", FALSE))
      for (v in cvs) {
        data.table::set(
          tabs, j = paste0(v, "_en"),
          value = if (kish && paste0(v, "_w2") %in% names(tabs)) {
            tabs[[paste0(v, "_wn")]]^2 / tabs[[paste0(v, "_w2")]]
          } else {
            as.double(tabs[[paste0(v, "_n")]])
          })
      }

      if (ci == "diff") {
        # Broadcast the reference row's mean / var / effective-n within each comparison group
        # (the same `nth(., ref index within group)` idiom the diff/ratio block above uses).
        tabs[, paste0(cvs, "_refm") := purrr::map(
          rlang::syms(paste0(cvs, "_mean")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
        tabs[, paste0(cvs, "_refv") := purrr::map(
          rlang::syms(paste0(cvs, "_var")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
        tabs[, paste0(cvs, "_refn") := purrr::map(
          rlang::syms(paste0(cvs, "_en")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
      }

      for (v in cvs) {
        m  <- tabs[[paste0(v, "_mean")]]
        vv <- tabs[[paste0(v, "_var")]]
        nn <- tabs[[paste0(v, "_en")]]
        if (ci == "cell") {
          res <- ci_pivot(m, sqrt(vv / nn), df = Inf, conf_level = conf_level, want_p = FALSE)
        } else {
          mr <- tabs[[paste0(v, "_refm")]]
          vr <- tabs[[paste0(v, "_refv")]]
          nr <- tabs[[paste0(v, "_refn")]]
          res <- ci_mean_diff2(m, vv, nn, mr, vr, nr, conf_level = conf_level, want_p = want_p)
          # A reference row has no CI/test against itself.
          res$inf[refrows] <- NA_real_
          res$sup[refrows] <- NA_real_
          res$pvalue[refrows] <- NA_real_
        }
        data.table::set(tabs, j = paste0(v, "_ci_inf"), value = res$inf)
        data.table::set(tabs, j = paste0(v, "_ci_sup"), value = res$sup)
        data.table::set(tabs, j = paste0(v, "_pvalue"), value = res$pvalue)
      }

      data.table::set(tabs, j = paste0(cvs, "_en"), value = NULL)
      if (ci == "diff")
        data.table::set(tabs, j = paste0(rep(cvs, each = 3L),
                                         c("_refm", "_refv", "_refn")), value = NULL)
    }

    tabs[, "ref_rows___" := NULL]
  }

  # G1: drop the Kish Sigma(w^2) scratch (accumulated only when opted in) before the reshape,
  # so it never leaks into the fmt columns.
  w2_cols <- names(tabs)[stringr::str_detect(names(tabs), "_w2$")]
  if (length(w2_cols) > 0) data.table::set(tabs, j = w2_cols, value = NULL)





  #Make the final table with fmt vectors
  # remove(list = c("tabs_n", "tabs_wn", "tabs_pct", "tabs_diff", "tabs_ci", "refcols_vector", "refrows"))

  text_vars <- !purrr::map_lgl(tabs, is.numeric)
  NA_reals <- rep(NA_real_, nrow(tabs))

  #n <- as.integer(tabs[["n"]])
  #wn <- if ("wn" %in% names(tabs)) { tabs[["wn"]] } else { NA_reals }

  tabs_n  <-
    data.table::setnames(tabs[, stringr::str_detect(names(tabs), "_n$"), with = FALSE] ,
                         function(.x) stringr::str_remove(.x, "_n$" ))

  tabs_wn  <-
    if (length(wt) != 0) {
      data.table::setnames(tabs[, stringr::str_detect(names(tabs), "_wn$"), with = FALSE] ,
                           function(.x) stringr::str_remove(.x, "_wn$" ))
    } else {
      list(NA_reals)
    }

  tabs_mean  <-
    data.table::setnames(tabs[, stringr::str_detect(names(tabs), "_mean$"), with = FALSE] ,
                         function(.x) stringr::str_remove(.x, "_mean$" ))

  #Nan to NA
  tabs_mean <- tibble::as_tibble(tabs_mean) |>
    dplyr::mutate(dplyr::across(
      where(~ any(is.nan(.))),
      ~ dplyr::if_else(is.nan(.), NA_real_, .)
    )) |>
    data.table::as.data.table()


  # WARNING: tab_num reshapes by column-name SUFFIX (_n/_wn/_mean/_var/_diff/_ci) — fragile.
  # "no_row_var" ends in "_var" and would be mis-detected as a variance column, hence the
  # explicit exclusion. A numeric col_var whose name ends in one of these suffixes would
  # likewise be mis-parsed.
  tabs_var  <-
    data.table::setnames(tabs[, stringr::str_detect(names(tabs), "_var$") &
                                names(tabs) != "no_row_var",
                              with = FALSE],
                         function(.x) stringr::str_remove(.x, "_var$" ))


  are_diff <- stringr::str_detect(names(tabs), "_diff$")
  tabs_diff  <-
    if (any(are_diff)) {
      data.table::setnames(tabs[, are_diff, with = FALSE] ,
                           function(.x) stringr::str_remove(.x, "_diff$" ))
    } else {
      list(NA_reals)
    }

  are_ratio <- stringr::str_detect(names(tabs), "_ratio$")
  tabs_ratio  <-
    if (any(are_ratio)) {
      data.table::setnames(tabs[, are_ratio, with = FALSE] ,
                           function(.x) stringr::str_remove(.x, "_ratio$" ))
    } else {
      list(NA_reals)
    }

  # Phase 3a: reshape the real CI bounds + per-cell pvalue (were a single symmetric half-width).
  reshape_suffix <- function(sfx) {
    hit <- stringr::str_detect(names(tabs), paste0(sfx, "$"))
    if (any(hit)) {
      data.table::setnames(tabs[, hit, with = FALSE],
                           function(.x) stringr::str_remove(.x, paste0(sfx, "$")))
    } else {
      list(NA_reals)
    }
  }
  tabs_ci_inf <- reshape_suffix("_ci_inf")
  tabs_ci_sup <- reshape_suffix("_ci_sup")
  tabs_pvalue <- reshape_suffix("_pvalue")

  tabs_text <- tabs[, text_vars, with = FALSE]

  if (ref %in% c("tot", "no", "")) refrows <- rep(FALSE, nrow(tabs))


  # Phase 7f-1: display / ref / comp are column-invariant (scalars for this tab_num call) -- compute
  # once. `digits` and `col_var` stay per-column (digits reads the per-column mean magnitude ..3);
  # the per-column case_when becomes a base if/else (scalar conditions, only one branch evaluated) --
  # byte-identical. NA_reals is reused for the always-NA fields (pct/ctr/tot_n/or) new_fmt defaults.
  display_1 <- if (ci_visible) { "mean_ci" } else { "mean" }
  ref_1     <- switch(as.character(ref), "no" = "", "tot" = "tot", as.character(ref))
  comp_1    <- dplyr::if_else(ref != "no" | ci != "no", comp == "all", NA)
  NA_reals  <- rep(NA_real_, nrow(tabs_n))

  tabs <-
    list(tabs_n, tabs_wn, tabs_mean, tabs_var, tabs_diff, tabs_ci_sup, as.character(col_vars),
         digits, tabs_ratio, tabs_ci_inf, tabs_pvalue) |>
    purrr::pmap_dfc(~ new_fmt(
      display   = display_1,
      digits    = {
        m <- max(..3, na.rm = TRUE)
        if      (m <= 1 ) vec_recycle(max(..8, 2L), length(..1))
        else if (m <= 10) vec_recycle(max(..8, 1L), length(..1))
        else              vec_recycle(..8,          length(..1))
      },
      n         = ..1,
      wn        = ..2,
      mean      = ..3,
      var       = ..4,
      diff      = ..5,
      ratio     = ..9,
      pct       = NA_reals,
      ctr       = NA_reals,
      or        = NA_reals,
      tot_n     = NA_reals,
      # Phase 3a: real asymmetric CI bounds + per-cell significance (mean CIs are symmetric
      # around the estimate, but stored as absolute bounds like the proportion path).
      ci_sup    = ..6,
      ci_inf    = ..10,
      pvalue    = ..11,
      in_totrow = totrow_vector,
      in_tottab = tottab_vector,
      in_refrow = refrows,
      color     = color,
      type      = "mean",
      ref       = ref_1,
      ci_type   = ci, #dplyr::if_else(ci == "diff", "diff", ci),
      comp      = comp_1,
      col_var   = ..7
    ))

  tabs <- dplyr::bind_cols(tibble::as_tibble(tabs_text), tabs)


  #Rename totals
  if (totaltab %in% c("line", "table") &  totaltab_name != "Total") {
    tabs <- tabs |> dplyr::mutate(dplyr::across(
      tidyselect::all_of(as.character(tab_vars)),
      ~ dplyr::if_else(tottab_vector,
                       true  = factor(totaltab_name, c(levels(.), totaltab_name)),
                       false = .) |>
        forcats::fct_drop()
    ))
  }

  if (length(tab_vars) == 0) {

    if ("row" %in% tot & total_names[1] != "Total") tabs <- tabs |>
        dplyr::mutate(!!row_var := forcats::fct_recode(!!row_var,
                                                       purrr::set_names("Total", total_names[1])))
  } else {
    tabs <- tabs |>
      tidyr::unite(col = "tabs_tot_names", !!!tab_vars, sep = " ", remove = FALSE) |>
      dplyr::mutate(
        !!row_var := dplyr::if_else(
          totrow_vector,
          true  = paste(total_names[1], .data$tabs_tot_names) |>
            forcats::fct_expand(levels(!!row_var)) |>
            forcats::fct_relevel(levels(!!row_var)),
          false = !!row_var) |>
          forcats::fct_drop()
        #forcats::fct_recode(!!row_var,
        #                               purrr::set_names("Total", total_names[1]))
      ) |>
      select(-"tabs_tot_names")
  }

  if ("col" %in% tot & total_names[2] != "Total") tabs <- tabs |>
    dplyr::rename(tidyselect::any_of(purrr::set_names("Total", total_names[2])))





  # Add argument to transpose the table ?
  # if (row_var_type == "numeric") {
  #   tabs <- tabs %>%
  #   tidyr::pivot_wider(names_from = !!fct_var, values_from = !!num_var,
  #                      names_glue = "{.value}_{.name}",
  #                      values_fill = fmt0("mean", digits, type = "mean"))
  #   if (as.character(tab_vars) == "no_tab_vars") {
  #     tabs <- tabs %>% dplyr::mutate(no_row_var = factor("no_row_var")) %>%
  #       dplyr::relocate(no_row_var, .before = 1)
  #   }
  # }


  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                                    ~ length(unique(.)) == 1))

  result <- if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext) %>%
      dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs %>% dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext)
  }

  # Phase 5: set the final two-channel colour / significance-policy attributes (a no-op for a
  # plain scalar colour passed straight through, e.g. when tab_many() drives tab_num()).
  finalize_color_spec(result, color_spec)
}




#' Add total table to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (1.4.0): the total table is built directly by the `totaltab` argument of
#' [tab()] / [tab_plain()] / [tab_num()]. `tab_totaltab()` still works on an existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
#' @param totaltab If there are subtables, corresponding to the levels of tab_vars,
#' \code{totaltab = "table"} add a complete total table.
#' \code{totaltab = "line"} add a total table of only one row with the general total.
#' \code{totaltab = "no"} remove any existing total table.
#' @param name The name of the total table, as a single string.
#' @param data The original database used to calculate the \code{tab} : it is only useful
#' for mean columns (of numeric variables), in order to calculate the variances
#' necessary to calculate confidence intervals with \code{\link{tab_ci}}.
#'
#' @return A \code{tibble} of class \code{tab}. Rows belonging to the total table can then
#' be detected using \code{\link{is_tottab}}.
#' @export
#'
#' @examples \donttest{ data <- dplyr::starwars %>%
#' tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'             na_drop_all = sex)
#'
#' data %>%
#'   tab_plain(sex, hair_color, gender) %>%
#'   tab_totaltab("line")
#'   }
tab_totaltab <- function(tabs, totaltab = c("table", "line", "no"),
                         name = "Ensemble", data = NULL) {
  #.Deprecated("tab_plain() and tab_num(), which now have a totaltab argument")

  get_vars  <- tab_get_vars(tabs)

  row_var   <- rlang::sym(get_vars$row_var)
  tab_vars  <- rlang::syms(get_vars$tab_vars)
  mean_vars <- (get_type(tabs) == "mean") %>% purrr::keep(., .) %>% names()


  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  if (length(tab_vars) == 0) return(tabs)

  #Remove the existing total table if there is one
  tottab_rows <- is_tottab(tabs)
  if (any(tottab_rows)) tabs <- tabs %>%
    tibble::add_column(tottab = tottab_rows) %>%
    dplyr::filter(!.data$tottab) %>% dplyr::select(-"tottab")

  if (totaltab[1] == "no") return(tabs)

  #Calculate the total table
  totaltable <- switch(
    totaltab[1],
    "table" = tibble::as_tibble(tabs) %>% tibble::add_column(totrow = is_totrow(.)) %>%
      dplyr::filter(!.data$totrow) %>% dplyr::select(-"totrow") %>%
      dplyr::group_by(!!row_var) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_tottab(sum(.) ))),

    "line"  = tibble::as_tibble(tabs) %>% tibble::add_column(totrow = is_totrow(.)) %>%
      dplyr::filter(!.data$totrow) %>% dplyr::select(-"totrow") %>%
      dplyr::group_by(!!row_var) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), sum)) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(as_tottab(sum(.))))) %>%
      dplyr::mutate(!!row_var := paste("TOTAL", stringr::str_to_upper(name)))
  )

  if (totaltab[1] == "line") {
    tabs <- tabs %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(
        !!row_var,
        levels(dplyr::pull(totaltable, !!row_var))
      ))

    totaltable <- totaltable %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(
        !!row_var, levels(dplyr::pull(tabs, !!row_var))
      ))
  }

  totaltable <-
    purrr::reduce(tab_vars, .init = totaltable,
                  .f = ~ dplyr::mutate(.x, !!.y := factor(name)))


  # If there are mean columns, the calculation of variances, necessary to
  #  calculate confidence intervals, needs access to the original database
  if (length(mean_vars) != 0 & !is.null(data)) {

    mean_calc <- switch(
      totaltab[1],
      "table" = purrr::map(mean_vars, ~ tab_plain(data, !!row_var,
                                                  col_var = !!rlang::sym(.))),
      "line" = purrr::map(mean_vars, ~tab_plain(data, col_var = !!rlang::sym(.)))
    )
    mean_calc <-
      purrr::reduce(mean_calc,
                    ~ dplyr::full_join(.x, .y, by = switch(totaltab[1],
                                                           "table" = as.character(row_var),
                                                           "line"  =  "no_row_var") ) ) %>%
      dplyr::select(-tidyselect::starts_with("no_row_var")) %>%
      dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(.)))

    if (totaltab[1] == "line") mean_calc <- mean_calc %>%
      dplyr::mutate(dplyr::across(where(is_fmt), ~ as_totrow(.)))

    totaltable <- switch(
      totaltab[1],
      "table" = dplyr::left_join(dplyr::select(totaltable,
                                               -tidyselect::all_of(mean_vars)),
                                 mean_calc, by = as.character(row_var)),
      "line"  = dplyr::left_join(dplyr::select(totaltable,
                                               -tidyselect::all_of(mean_vars)),
                                 mean_calc, by = character())
    )

    totaltable
  }


  #Bind the total table to the tabs
  if (lv1_group_vars(tabs)) {
    tabs %>% dplyr::bind_rows(totaltable)
  } else {

    df <- tabs %>% dplyr::bind_rows(totaltable)
    groups <- dplyr::group_data(df)
    new_grouped_tab(df, groups = groups, subtext = subtext, chi2 = chi2)
  }
}




#' Add totals to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (1.4.0): totals are built directly by [tab()] / [tab_plain()] / [tab_num()] (a
#' total row is always computed, one total column shown). `tab_tot()` still works on an
#' existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
#' @param tot \code{c("col", "row")} and \code{"both"} print total rows and total columns.
#'  Set to \code{"row"} or \code{"col"} to print only one type.
#'  Set to \code{"no"} to remove all totals.
#' @param name  The names of the totals, as a character vector of length one or two.
#' Use \code{c("Total_row", "Total_column")} to set different names for rows and cols.
#' @param totcol \code{"last"} only prints a total column for the last factor column
#' variable. Set to \code{"each"} to print a total column for each column variable.
#' @param data The original database used to calculate the \code{tab} : it is only useful
#' for mean columns (of numeric variables), in order to calculate the variances of
#' total rows, necessary to calculate confidence intervals with \code{\link{tab_ci}}.
#'
#' @return A \code{tibble} of class \code{tab}. Total rows can then be detected using
#'  \code{\link{is_totrow}}, and total columns using \code{\link{is_totcol}}.
#' @export
#'
#' @examples \donttest{data <- dplyr::starwars %>% tab_prepare(sex, hair_color)
#'
#' data %>%
#'   tab_plain(sex, hair_color) %>%
#'   tab_tot("col", totcol = "each")
#'   }
tab_tot <- function(tabs, tot = c("row", "col"), name = "Total",
                    totcol = "last", data = NULL) {
  #.Deprecated("tab_plain() and tab_num(), which now have a tot argument")

  stopifnot(
    tot %in% c("no", "row", "col", "both"),
    totcol %in% c("last", "each", "no", "")
  )

  get_vars        <- tab_get_vars(tabs)
  row_var         <- rlang::sym(get_vars$row_var)
  #col_vars        <- rlang::sym(get_vars$col_vars)
  col_vars_levels_mean <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  mean_vars <- get_type(tabs) == "mean"
  col_vars_levels <- col_vars_levels_mean %>%
    purrr::discard(names(.) %in% names(mean_vars))
  tab_vars        <- rlang::syms(get_vars$tab_vars)

  groups <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  if (any("both" %in% tot)) tot <- c("row", "col")
  name <- vctrs::vec_recycle(name, 2)

  if (length(col_vars_levels) == 0 & "col" %in% tot) {
    warning("can't add a total column without at least one non-mean col_var")
    tot <- dplyr::if_else("row" %in% tot, "row", "no")
  }


  #Remove existing totals, except if there is a total table of one line
  if ("row" %in% tot | tot[1] == "no") {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows #& totrows

    if (any(totrows)) tabs <- tabs %>%
      tibble::add_column(totrows, tottab_line) %>%
      dplyr::filter(!.data$totrows | .data$tottab_line) %>%
      dplyr::select(-"totrows", -"tottab_line")
  }

  if ("col" %in% tot | tot[1] == "no") tabs <- tabs %>%
    dplyr::select(-where(is_totcol))

  if (tot[1] == "no") return(tabs)


  # Total rows
  if ("row" %in% tot) {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows #& totrows

    tabs <- tabs %>% tibble::add_column(tottab_rows, tottab_line)

    if (length(groups) != 0) {
      group_vars_totals <-
        dplyr::group_keys(dplyr::filter(tabs, !.data$tottab_line)) %>% #dplyr::mutate(bis = PR0) %>%
        tidyr::unite(!!row_var, sep = " / ") %>%
        dplyr::mutate(!!row_var := paste(name[1], !!row_var) %>%
                        stringr::str_to_upper() %>% forcats::as_factor())  #stringr::str_remove_all()
    } else {
      group_vars_totals <- tibble::tibble(!!row_var := factor(name[1]))
    }
    group_vars_totals_levels <- group_vars_totals %>% dplyr::pull(1) %>% levels()

    tabs <- tabs %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(!!row_var, group_vars_totals_levels))

    row_var_levels <- dplyr::pull(tabs, !!row_var) %>% levels()

    totrows <- tabs %>% dplyr::filter(!.data$tottab_line) %>%
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(sum(.)) ),
                       .groups = "drop") %>%
      dplyr::bind_cols(group_vars_totals) %>%
      dplyr::mutate(!!row_var := forcats::fct_expand(!!row_var, row_var_levels))

    #For mean vars, calculate variances based on original datas
    # (necessary to calculate confidence intervals)
    if (any(mean_vars) & !is.null(data)) {
      mean_names <- names(mean_vars[mean_vars])

      mean_calc <-
        purrr::map(mean_names, ~ tab_plain(data, row_var = NA_character_,
                                           col_var = !!rlang::sym(.),
                                           purrr::map_chr(tab_vars, as.character))
        )

      mean_calc <-
        purrr::reduce(mean_calc,~ dplyr::full_join(
          .x, .y,
          by = c(purrr::map_chr(tab_vars, as.character))
        ) ) %>%
        dplyr::select(-tidyselect::contains("no_row_var")) %>%
        dplyr::mutate(dplyr::across(where(is_fmt), ~ as_totrow(.)))

      general_totrow_condition <- any(tabs$tottab_rows) & !any(tabs$tottab_line)

      if (general_totrow_condition) {
        general_totrow <-
          purrr::map(mean_names,
                     ~ tab_plain(data, row_var = NA_character_,
                                 col_var = !!rlang::sym(.))
          )

        general_totrow <-
          purrr::reduce(general_totrow,
                        ~ dplyr::full_join(.x, .y ,by = character() ) ) %>%
          dplyr::select(-tidyselect::starts_with("no_row_var")) %>%
          dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(as_totrow(.))))

        general_totrow  <- dplyr::group_keys(tabs) %>%
          dplyr::slice(dplyr::n_groups(tabs)) %>%
          dplyr::bind_cols(general_totrow)

        mean_calc <- dplyr::bind_rows(mean_calc, general_totrow)
      }

      totrows <- dplyr::left_join(
        dplyr::select(totrows,
                      -tidyselect::all_of(mean_names)),
        mean_calc,
        by = purrr::map_chr(tab_vars, as.character)
      )
    }


    tabs <- dplyr::bind_rows(tabs, totrows) %>%
      dplyr::arrange(.by_group = TRUE) %>%
      dplyr::select(-"tottab_line", -"tottab_rows")
  }


  #Total columns
  if ("col" %in% tot) {
    col_vars_2levels_or_more <-
      col_vars_levels[purrr::map_int(col_vars_levels, length) >= 2]

    if (length(col_vars_2levels_or_more) != 0 | totcol[1] == "last") {
      tabs <- tabs %>% dplyr::rowwise()

      if (totcol[1] == "last") {
        # c_across don't work. Workaround with quosures : sum(!!!col_vars_levels)
        tabs <- tabs %>%
          dplyr::mutate(
            !!rlang::sym(name[2]) :=
              sum(!!!col_vars_levels[[length(col_vars_levels)]]) %>%
              as_totcol() %>% set_col_var("all_col_vars"))

      } else if (totcol[1] == "each") {
        totcol_names <- purrr::map(paste0(name[2],"_",
                                          names(col_vars_2levels_or_more)),
                                   rlang::sym)
        tabs <-
          purrr::reduce2(col_vars_2levels_or_more, totcol_names, .init = tabs,
                         function(.tab, .levels, .names)
                           dplyr::mutate(.tab, !!.names := sum(!!!.levels) %>%
                                           as_totcol())
          )
        tabs <-
          purrr::reduce(names(col_vars_2levels_or_more), .init = tabs,
                        function(.tab, .var)
                          dplyr::relocate(
                            .tab,
                            where(~ tidyr::replace_na(get_col_var(.) == .var & is_totcol(.),
                                                      FALSE)),
                            .after = where(~ tidyr::replace_na(get_col_var(.) == .var &
                                                                 !is_totcol(.),
                                                               FALSE)
                            ) ) )
      }

      tabs <- tabs %>% dplyr::group_by(!!!rlang::syms(groups))
    }
  }

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {

    group_dat <- dplyr::group_data(tabs)
    new_grouped_tab(tabs, groups = group_dat, subtext = subtext, chi2 = chi2)
  }
}


# WARNING: For type="mean" columns, diff stores a RATIO (cell_mean/ref_mean), not a
#   difference. This is intentional — mean breaks (1.15, 1.5, 2, 4) are ratio thresholds.
#   For pct columns, diff stores an additive difference (cell_pct - ref_pct).
#' Add percentages and diffs to a \code{\link[tabxplor]{tab}}
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (1.4.0): percentages, differences and ratios are computed directly by
#' [tab()] / [tab_plain()] via the `pct` / `ref` arguments. `tab_pct()` still works on an
#' existing tab.
#'
#' @param tabs A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
#' @param pct The type of percentages to calculate. \code{"row"} draw row percentages.
#' Set to \code{"col"} for column percentages. Set to \code{"all"} for frequencies
#' (based on each subtable/group if \code{tab_vars} is provided).
#' Set to \code{"all_tabs"} to calculate frequencies based on the whole (set of) table(s).
#' @param digits The number of digits to print for percentages. As a single integer,
#' or an integer vector the same length than \code{col_vars}.
#' @param ref The reference cell to calculate differences and ratios
#'  (used to print \code{colors}) :
#'  \itemize{
#'   \item \code{"auto"}: by default, cell difference from the corresponding total
#'   (rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
#'   used for `diff` ; cell ratio from the first line (or col) is use for `OR`
#'   (odds ratio/relative risks ratio).
#'   \item \code{"tot"}: totals are always used.
#'   \item \code{"first"}: calculate cell difference or ratio from the first cell
#' of the row or column (useful to color temporal developments).
#'   \item \code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.
#'   \item \code{"regex"}: when `ref` is a string, it it used as a regular expression,
#'   to match with the names of the rows (or columns). Be precise enough to match only one
#'   column or row, otherwise you get a warning message.
#'   \item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
#' }
#' @param comp Comparison level. When \code{tab_vars} are present, should the row
#' differences be calculated for each subtable/group (by default \code{comp = "tab"} :
#' comparison of each cell to the relative total row) ?
#' Should they be calculated for the whole table (\code{comp = "all"} :
#' comparison of each cell to the total row of the total table) ?
#' When \code{comp = "all"} and \code{ref = "first"}, cells are compared to the first
#' cell of the total table instead.
#' This parameter doesn't affect column percentages.
#' \code{comp} must be set once and for all the first time you use \code{\link{tab_chi2}},
#' \code{\link{tab_pct}} with rows, or \code{\link{tab_ci}}.
#' @param color Set to \code{TRUE} to color the resulting tab based on differences (from
#' totals or from the first cell).
#' @param just_diff If percentages are already calculated and you just want
#' to recalculate differences.
#'
#' @return A \code{tibble} of class \code{tab}, with percentages displayed, possibly
#' colored based on differences from totals or first cell.
#' @export
tab_pct <- function(tabs, pct = "row", #c("row", "col", "all", "all_tabs", "no"),
                    digits = NULL, ref = c("tot", "first", "no"),
                    comp = NULL, color = FALSE, just_diff = FALSE) { #Add keep/change grouping ?

  # .Deprecated("tab_plain() and tab_num(), which now have pct and ref arguments")

  #stopifnot(pct[1] %in% c("row", "col", "all", "all_tabs", "no"))
  get_vars         <- tab_get_vars(tabs)
  #row_var         <- rlang::sym(get_vars$row_var) #col_var ??
  col_vars_with_all<- rlang::syms(get_vars$col_vars)
  col_vars_no_all  <- col_vars_with_all %>% purrr::discard(. == "all_col_vars")
  col_means  <- (get_type(tabs) == "mean") %>% purrr::keep(., .) %>% names()
  # col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  tab_vars         <- rlang::syms(get_vars$tab_vars)

  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  pct <- vctrs::vec_recycle(pct, length(col_vars_no_all)) %>%
    purrr::set_names(col_vars_no_all)
  pct[col_means] <- "no"

  if (just_diff == FALSE) {

    if (all(pct == "no")) {
      tabs <- tabs %>% dplyr::mutate(dplyr::across(
        where(~ get_type(.) %in% c("row", "col", "all", "all_tabs")),
        ~ set_pct(., NA_real_) %>% set_type("n") %>%
          set_display("wn")
      ))
      if (length(col_means) == 0) return(tabs)
    }


    #Ready table for percentages (need total rows and cols, compatible grouping)
    if (any(pct == "all_tabs")) {
      if (length(tab_vars) != 0          &
          !(is_tottab(tabs[nrow(tabs),]) &
            is_totrow(tabs[nrow(tabs),]) &
            any(is_totcol(tabs))) ) {
        warning("since percentages are 'all_tabs', a total table ",
                "was added")
        if (!is_tottab(tabs[nrow(tabs),])) {
          tabs <- tabs %>% tab_totaltab('line')
        }
        tabs <- tabs %>%
          dplyr::with_groups(NULL, ~ tab_match_groups_and_totrows(.) %>%
                               tab_add_totcol_if_no()
          )
      }
    }

    if ( any(pct %in% c("col", "all") ) | (any(pct == "row") & ref[1] == "tot") ) {
      tabs <- tabs %>% tab_match_groups_and_totrows()
    }

    if ( any(pct %in% c("row", "all")) | (any(pct == "col") & ref[1] == "tot") ) {
      tabs <- tabs %>% tab_add_totcol_if_no()
    }

    comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
    tabs <- tabs %>% tab_match_comp_and_tottab(comp)

    if (any(pct != "no")){
      pct <- c(pct, all_col_vars = dplyr::last(pct[pct != "no"]))
      pct <- purrr::map_chr(tabs, ~ pct[get_col_var(.)] ) %>%
        tidyr::replace_na("no")
      row_pct      <- names(pct)[pct == "row"]
      col_pct      <- names(pct)[pct == "col"]
      all_pct      <- names(pct)[pct == "all"]
      all_tabs_pct <- names(pct)[pct == "all_tabs"]


      #Calculate percentages
      # pct_formula <- function(x, pct, tot) {
      #   switch(pct,
      #          "row"     =  get_wn(x) / get_wn(tot             ),
      #          "col"     =  get_wn(x) / get_wn(dplyr::last(x)  ),
      #          "all"     =  get_wn(x) / get_wn(dplyr::last(tot)),
      #          "all_tabs"=  get_wn(x) / get_wn(dplyr::last(tot)),
      #          NA_real_)
      # }
      #For each var, the first total column at the right is taken
      tot_cols <- detect_totcols(tabs)


      if (any(pct != "all_tabs")) {
        pct_nat <- pct %>% stringr::str_replace("all_tabs", "no") %>%
          purrr::set_names(names(pct))

        tabs <- tabs %>%
          dplyr::mutate(dplyr::across(
            where(~ is_fmt(.) & !get_type(.) == "mean"),
            ~ set_pct(., pct_formula(
              .,
              pct = pct_nat[[dplyr::cur_column()]],
              tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
            )) %>%
              set_display(ifelse(pct_nat[[dplyr::cur_column()]] != "no", "pct", "wn")) %>%
              set_type(pct_nat[[dplyr::cur_column()]])
          ))
      }

      if (any(pct == "all_tabs")) {
        tabs <- tabs %>%
          dplyr::with_groups(
            NULL,
            ~ dplyr::mutate(., dplyr::across(
              tidyselect::all_of(all_tabs_pct),
              ~ set_pct(., pct_formula(
                .,
                pct = "all_tabs",
                tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
              )) %>%
                set_display("pct") %>% set_type("all_tabs")
            ))
          )
      }

      #Set digits if provided. Always zero digits for the 100% cells
      if (!is.null(digits)) {
        digits <- vctrs::vec_recycle(digits, length(col_vars_with_all)) %>%
          purrr::set_names(col_vars_with_all)
        digits <- c(digits, all_col_vars = dplyr::last(digits[!is.na(digits)]))
        digits <- purrr::map_dbl(tabs, ~ digits[get_col_var(.)] )
        digits[pct == "no"] <- NA_real_

        digits_cols <- names(digits)[!is.na(digits)]

        tabs <- tabs %>% dplyr::mutate(dplyr::across(
          tidyselect::all_of(digits_cols),
          ~ set_digits(., as.integer(digits[[dplyr::cur_column()]])) ))
      }

      if (length(row_pct     ) != 0) tabs <- tabs %>% dplyr::mutate(dplyr::across(
        where(is_totcol) & tidyselect::all_of(row_pct), ~ set_digits(., 0L)))
      if (length(col_pct     ) != 0) tabs <- tabs %>% dplyr::mutate(dplyr::across(
        tidyselect::all_of(col_pct),
        ~ dplyr::if_else(is_totrow(.), set_digits(., 0L), .)))
      if (length(all_pct     ) != 0) tabs <- tabs %>% dplyr::mutate(dplyr::across(
        where(is_totcol) & tidyselect::all_of(all_pct),
        ~ dplyr::if_else(is_totrow(.), set_digits(., 0L), .)))
      if (length(all_tabs_pct) != 0) tabs <- dplyr::ungroup(tabs) %>%
        dplyr::mutate(., dplyr::across(
          where(is_totcol) & tidyselect::all_of(all_tabs_pct),
          ~ dplyr::if_else(dplyr::row_number()==dplyr::n(), set_digits(., 0L), .))) %>%
        dplyr::group_by(!!!rlang::syms(groups))
    }

  } else {
    comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  }

  type <- get_type(tabs)
  #Calculate diffs (used to color pct depending on spread from row or col mean)
  if (ref[1] != "no" & any(type %in% c("row", "col", "mean")) ) {
    # diff_formula <- function(x, type, dif, refer) {
    #   switch(
    #     ref, #ref[1] before
    #     "tot"   = switch(type,
    #                      "row"     =  get_pct(x)  - get_pct(dplyr::last(x  )),
    #                      "col"     =  get_pct(x)  - get_pct(refer             ),
    #                      "mean"    =  get_mean(x) / get_mean(dplyr::last(x) ),
    #                      NA_real_),
    #     "first" = switch(type,
    #                      "row"     =  get_pct(x)  - get_pct(dplyr::first(x  )),
    #                      "col"     =  get_pct(x)  - get_pct(refer              ),
    #                      "mean"    =  get_mean(x) / get_mean(dplyr::first(x) ),
    #                      NA_real_)
    #   )
    # }

    if (ref[1] == "tot"  ) reference <- detect_totcols(tabs)
    if (ref[1] == "first") {
      reference <- detect_firstcol(tabs)
      reference_cols <- purrr::map_chr(reference, as.character) %>% unique()
      reference_cols <- reference_cols[reference_cols != ""]

      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          where(~ get_type(.) == "col") & tidyselect::all_of(reference_cols),
          as_refcol
        ))
      # is_refcol(tabs)

      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          where(~ get_type(.) %in% c("row", "mean")),
          ~ as_refrow(., dplyr::row_number() == 1 &
                        (comp == "tab" | (comp == "all" & is_tottab(.)) ) )
        ))
      # is_refrow(tabs)
    }

    if ( comp == "all" & any(type %in% c("row", "mean")) ) {
      tabs <- tabs %>%
        dplyr::with_groups(
          NULL,
          ~ dplyr::mutate(., dplyr::across(
            where(~ get_type(.) %in% c("row", "col", "mean")),
            ~ set_diff(., diff_formula(
              .,
              type = type[[dplyr::cur_column()]],
              ref = ref[1],
              refer  = rlang::eval_tidy(reference[[dplyr::cur_column()]])
            )) %>% set_diff_type(ref[1])
          ))
        )

    } else {
      tabs <- tabs %>%
        dplyr::mutate(dplyr::across(
          where(~ get_type(.) %in% c("row", "col", "mean") ) &
            !( where(is_totcol) &
                 tidyselect::any_of(names(reference)[reference == ""]) ),
          ~ set_diff(., diff_formula(
            .,
            type = type[[dplyr::cur_column()]],
            ref = ref[1],
            refer = rlang::eval_tidy(reference[[dplyr::cur_column()]])
          )) %>% set_diff_type(ref[1])
        ))
    }

    if ( any(type %in% c("row", "mean")) ) tabs <- tabs %>%
        dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., comp[1] == "all")))

    if (color == TRUE) {
      tabs <- tabs %>%
        dplyr::mutate(dplyr::across(
          where(is_fmt),
          ~ set_color(., ifelse(
            type[[dplyr::cur_column()]] %in% c("row", "col", "mean"),
            "diff",
            get_color(.)
          )) ))
    }
  }

  tabs <- tabs %>% dplyr::select(-tidyselect::any_of("totrow_groups"))

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext,
                    chi2 = chi2)
  }
}


# ci_formula_factory <- function(y) {
#   function(x, y, zscore) zscore *
#     sqrt( get_pct(x) * (1 - get_pct(x)) / get_n(x)   +   get_pct(y) * (1 - get_pct(y)) / get_n(y) )
# }
#
# ci_formula_gen <- function(ci) {
#   switch(
#     ci,
#     "col"      = ci_formula_factory(tot),
#     "row"      = ci_formula_factory( dplyr::last(x) ),
#     "cell"      = ci_formula_factory(fmt0(pct)),
#     #"totaltab" = function(x, tot, zscore) ,
#     # "r_to_r"   = function(x, nx, y, ny, zscore) ,
#     # "c_to_c"   = function(x, nx, y, ny, zscore) ,
#     # "tab_to_t" = function(x, nx, y, ny, zscore) ,
#     "no"       = function(x, tot, zscore) NA_real_
#   )
# }


# DESIGN: CI is stored as a half-width (margin of error), not a full interval.
#   The ci field = z * sqrt(variance). For pct, stored as 0-1 (multiplied by 100 in format).
#   method_cell controls the proportion CI formula (wilson default); method_diff controls
#   the difference CI formula (agresti-caffo default). Negative CI values indicate
#   non-significant differences (used by color_formula for diff_ci/after_ci modes).
#Ci spread (negative numbers mean no significant difference)
#' Add confidence intervals to a \code{\link[tabxplor]{tab}}
#'
#' @param tabs A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
#' @param ci The type of ci to calculate. Set to "cell" to calculate absolute confidence
#' intervals. Set to "diff" to calculate the confidence intervals of the difference
#' between a cell and the relative total cell (or the reference cell,
#'  when `ref` is not `"tot"` in \code{\link{tab_plain}} or \code{\link{tab_num}}).
#'  By default, "diff" ci are calculated for means and row and col percentages,
#'  "cell" ci for frequencies ("all", "all_tabs"). By default, with \code{ci = "cell"},
#'  the result is printed in the `[inf;sup]` form. Set
#'  `options("tabxplor.ci_print" = "moe")` to print `pct +- moe` instead.
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
#' @param stars Logical (default \code{TRUE}, or `options("tabxplor.stars")` when \code{NULL}).
#' With \code{ci = "diff"}, store and print per-cell significance stars for the difference from
#' the reference, read from the same interval that is displayed (universal CI-inclusion), so the
#' stars and the bracket never disagree. \code{FALSE} skips the significance computation.
#' @param method_cell Character string, the proportion CI method for \code{ci = "cell"}: either
#' \code{"wilson"} (the score interval, default) or \code{"wald"} (the normal approximation).
#' @param method_diff Character string, the proportion CI method for \code{ci = "diff"}: one of
#' \code{"newcombe"} (default, hybrid-score, dual of the two-proportion score test), \code{"ac"}
#' (Agresti-Caffo) or \code{"wald"}. Whatever the method, the stars come from that interval.
#' @param color The type of colors to print, as a single string.
#' \itemize{
#'   \item \code{"no"}: by default, no colors are printed
#'   \item \code{"diff_ci"}: color pct and means based on cells differences from totals
#'   or first cells, removing coloring when the confidence interval of this difference
#'   is higher than the difference itself
#'   \item \code{"after_ci"}: idem, but cut off the confidence interval from the
#'   difference
#' }
#' @param visible By default confidence intervals are calculated and used to set colors,
#' but not printed. Set to \code{TRUE} to print them in the result.
#'
#' @section Significance stars:
#' With \code{ci = "diff"} and \code{stars = TRUE}, each cell shows how sure we can be that its
#' difference from the reference is real and not just sampling noise: \code{*} means significant at
#' the 10\% level (p < 0.10), \code{**} at 5\% (p < 0.05), \code{***} at 1\% (p < 0.01). The exact
#' p-value is stored per cell in the \code{pvalue} field of the \code{fmt} vectors, readable with
#' \code{$pvalue} or \code{get_pvalue()}.
#'
#' There is no separate statistical test run behind the scenes: the significance is read straight
#' from the confidence interval that is displayed. A cell is significant at a given level exactly
#' when its interval at that confidence level no longer contains zero, so the stars and the printed
#' \code{[inf; sup]} bracket can never contradict each other. Which test this amounts to depends on
#' the interval:
#' \itemize{
#'   \item \strong{percentage difference} (default, \code{method_diff = "newcombe"}): inverting the
#'     Newcombe hybrid-score interval. This is, to a very close approximation, the classical
#'     two-sample test of proportions (the score / "N-1" chi-squared test).
#'   \item \strong{percentage difference} with \code{method_diff = "ac"} or \code{"wald"}: inverting
#'     the Agresti-Caffo (adjusted Wald) or the Wald interval -- an (adjusted) two-proportion z-test.
#'   \item \strong{mean difference}: the \strong{Welch two-sample t-test} (for groups with unequal
#'     variances); inverting the Welch t interval is exactly this well-known test.
#'   \item \code{ci = "cell"} (an absolute cell interval, not a difference) is purely descriptive,
#'     so it carries no stars and its \code{pvalue} is \code{NA}.
#' }
#' On weighted data the estimate is weighted but the sample size used is the real (unweighted)
#' number of cases, unless you opt in to Kish's effective sample size with
#' \code{options("tabxplor.kish_neff" = TRUE)}.
#'
#' @return A \code{tibble} of class \code{tab}, colored based on differences (from
#' totals/first cells) and confidence intervals.
#' @export
#'
#' @examples # A typical workflow with tabxplor step-by-step functions :
#' \donttest{
#' data <- dplyr::starwars %>%
#'   tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'               na_drop_all = sex)
#'
#' data %>%
#'   tab_plain(sex, hair_color, gender, tot = c("row", "col"),
#'     pct = "row", comp = "all") %>%
#'     tab_ci("diff", color = "after_ci")
#'   }
tab_ci <- function(tabs,
                   ci = "auto",
                   comp = NULL,
                   conf_level = 0.95,
                   color = "no",
                   visible = FALSE,
                   stars = NULL,
                   method_cell = "wilson", method_diff = "newcombe") {
  stopifnot(all(ci %in% c("auto", "cell", "diff", "no")), #"r_to_r", "c_to_c", "tab_to_tab",
            all(comp %in%  c("tab", "all")),
            all(method_cell %in% c("wilson", "wald")),
            all(method_diff %in% c("newcombe", "ac", "wald"))
  )
  # Phase 3a: significance stars default (universal CI-inclusion). NULL -> option default.
  stars <- if (is.null(stars)) getOption("tabxplor.stars", TRUE) else stars

  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  # no_col_var <- get_col_var(tabs) == "no_col_var"
  # no_col_var <- no_col_var[no_col_var]
  # tabs <- tabs |> mutate(across(
  #   all_of(no_col_var),
  #   as_totcol,
  #   .names = "{.col}_Total"
  # ))

  get_vars          <- tab_get_vars(tabs)

  col_vars_with_all <- rlang::syms(get_vars$col_vars)
  col_vars_no_all   <- col_vars_with_all %>% purrr::discard(. == "all_col_vars")

  fmtc <- purrr::map_lgl(tabs, is_fmt)
  ci <- vctrs::vec_recycle(ci, length(col_vars_no_all)) %>%
    purrr::set_names(col_vars_no_all)
  ci <- c(ci, all_col_vars = dplyr::last(ci[ci != "no"]))
  ci <- purrr::map_chr(tabs, ~ ci[get_col_var(.)] ) %>%
    tidyr::replace_na(NA_character_)

  visible <- vctrs::vec_recycle(visible, length(col_vars_no_all)) %>%
    purrr::set_names(col_vars_no_all)
  visible <- c(visible, all_col_vars = dplyr::last(visible[visible != "no"]))
  visible <- purrr::map_lgl(tabs, ~ visible[get_col_var(.)] ) %>%
    tidyr::replace_na(FALSE)


  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs %>% tab_match_comp_and_tottab(comp)

  type <- get_type(tabs)
  tot_cols <- detect_totcols(tabs)
  tot_cols[is.na(ci)] <- list(rlang::sym(""))
  names_totcols <- tot_cols %>% purrr::map_chr(as.character) %>% unique() %>%
    purrr::discard(. == "")

  ref <- get_ref_type(tabs)
  # Phase 7g-iii: the diff-CI reference column must match the diff/colour reference column
  # (detect_refcol = the marked refcol, falling back to the first level -> byte-identical for
  # ref = "first"; ref = "tot" uses tot_cols below, so detect_refcol is not consulted there).
  ref_cols  <- detect_refcol(tabs)
  ref_cols[is.na(ci)] <- list(rlang::sym(""))

  ref_cols <- dplyr::if_else(ref == "tot",
                             true  = tot_cols,
                             false = ref_cols     ) %>%
    purrr::set_names(names(ref)) #keep ci_yes ?
  names_refcols <- ref_cols %>% purrr::map_chr(as.character) %>% unique() %>%
    purrr::discard(. == "")

  ci[fmtc] <- dplyr::case_when(
    !type[fmtc] %in% c("mean", "row", "col", "all", "all_tabs") ~ "no"      ,
    ci[fmtc] == "cell"                                          ~ "cell"    ,
    ci[fmtc] == "diff"   & type[fmtc] %in% c("row", "mean")     ~ "diff_row",
    ci[fmtc] == "diff"   & type[fmtc] == "col"                  ~ "diff_col",

    ci[fmtc] == "auto"   & type[fmtc] %in% c("row", "mean")     ~ "diff_row",
    ci[fmtc] == "auto"   & type[fmtc] == "col"                  ~ "diff_col",
    ci[fmtc] == "auto"   & type[fmtc] %in% c("all","all_tabs")  ~ "cell"    ,

    TRUE                                                        ~ "no"
  )


  #Depending of ci type, totals and reference cols (for diff), not calculate ci
  ci <- dplyr::if_else(
    condition = (!type %in% c("row", "col", "all", "all_tabs", "mean")) |
      (ci %in% c("diff_col", "spread_col") & type == "mean"),
    true = "no",
    false = ci
  )
  ci_with_ref <- ci %>% purrr::set_names(names(tabs))
  ci <- dplyr::if_else(
    condition = (ci == "diff_col" & names(tabs) %in% names_refcols) |
      (ci == "diff_col" & get_col_var(tabs) == "all_col_vars") |
      (ci == "diff_row" & names(tabs) %in% names_totcols),
    true = "no",
    false = ci
  )
  ci <- ci %>% purrr::set_names(names(tabs))
  ci_yes <- !is.na(ci) & ! ci == "no"


  if (any(ci_yes)) {
    #Ready table for percentages (needed totals, compatible grouping)
    if ( any(ci == "diff_col" ) ) tabs <- tabs %>% tab_add_totcol_if_no()
    if ( any(ci == "diff_row") ) {
      tabs <- switch(comp[1],
                     "tab" = tabs %>% tab_match_groups_and_totrows(),
                     "all" = tabs %>% dplyr::ungroup()               )
    }

    ci_select <- rlang::expr(tidyselect::all_of(names(ci_yes)[ci_yes]))
    diff_select <- rlang::expr(tidyselect::all_of(
      names(ci_yes)[ci %in% c("diff_row", "diff_col")]
    ))
    mean_select <- rlang::expr(tidyselect::all_of(
      names(ci_yes)[ci =="diff_row" & type == "mean"]
    ))
    row_select <- rlang::expr(tidyselect::all_of(
      names(ci_yes)[ci =="diff_row"]
    ))

    ref_rows <- tabs %>% dplyr::transmute(dplyr::across(
      !!row_select,
      ~ .[dplyr::last(which(switch(get_ref_type(.),
                                   "tot" = is_totrow(.),
                                   is_refrow(.)         )))]
    ))

    tot_rows <- tabs %>% dplyr::transmute(dplyr::across(
      !!ci_select & where(~ get_type(.) == "col"),
      ~ .[dplyr::last(which(is_totrow(.)))]
    ))

    ref_to_na <- tabs %>% dplyr::transmute(dplyr::across(
      !!ci_select,
      ~ tidyr::replace_na(dplyr::row_number() ==
                            dplyr::last(which(switch(get_ref_type(.),
                                                     "tot" = is_totrow(.) ,
                                                     is_refrow(.)))),
                          FALSE)
    ))

    tabs_nogroup <- tabs %>% dplyr::ungroup()

    #The n for each cell is the n of the relative 100% total
    # set to NA for reference, because we don't want to calculate it's ci
    x_n <- tabs_nogroup %>%
      dplyr::transmute(dplyr::across(
        !!ci_select,
        ~ dplyr::if_else(
          condition = ref_to_na[[dplyr::cur_column()]],
          true      = NA_integer_,
          false     = switch(
            get_type(.),
            # Phase 6h: each proportion cell carries its OWN unweighted base in the tot_n field
            # (row/col total), so read it directly instead of looking up the total row/column via
            # detect_totcols(). Byte-identical when all columns share one base; exact per-col_var
            # otherwise. Means keep their own n.
            "col" = get_tot_n(.),
            "row" = get_tot_n(.),
            "mean" = get_n(.)
          )
        )
      ))
    # tabs_ci %>% dplyr::mutate(dplyr::across(where(is_fmt), get_n))

    ref <- tabs_nogroup %>%
      dplyr::transmute(dplyr::across(
        !!diff_select,
        ~ switch(
          ci[[dplyr::cur_column()]],
          "diff_col" = get_pct(rlang::eval_tidy(ref_cols[[dplyr::cur_column()]])),
          "diff_row" = switch(get_type(.),
                              "mean" = get_mean(ref_rows[[dplyr::cur_column()]]),
                              get_pct(ref_rows[[dplyr::cur_column()]])
          )
        )
      ))
    # tabs_ci %>% dplyr::mutate(dplyr::across(where(is_fmt), get_ci))

    ref_var <- tabs_nogroup %>%
      dplyr::transmute(dplyr::across(
        !!mean_select,
        ~ get_var(ref_rows[[dplyr::cur_column()]])
      ))
    # tabs_ci %>% dplyr::mutate(dplyr::across(where(is_fmt), get_ctr))

    # The n for the comparison reference cells is the relative 100% total
    # - for means it is the n of the reference cell
    # - for row pct it is the n of the 100% cell of the reference row
    # - for col pct it is the n of the 100% cell of the reference col
    # Phase 6h: the reference base also comes from the tot_n field -- the reference column's own
    # base for diff_col, the reference row's own base for diff_row -- instead of a detect_totcols
    # total row/column lookup. Byte-identical when one base is shared; exact per-col_var otherwise.
    ref_n <- tabs %>%
      dplyr::transmute(dplyr::across(
        !!diff_select,
        ~ switch(ci[[dplyr::cur_column()]],
                 "diff_col" = rlang::eval_tidy(
                   ref_cols[[dplyr::cur_column()]]
                 )[dplyr::last(which(is_totrow(.)))] %>% get_tot_n(),
                 "diff_row" = switch(
                   get_type(.),
                   "mean" = .[dplyr::last(which(switch(get_ref_type(.),
                                                       "tot" = is_totrow(.),
                                                       is_refrow(.))))] %>%
                     get_n(), # = n of ref_rows (copy error with groups)

                   .[dplyr::last(which(switch(get_ref_type(.),
                                             "tot" = is_totrow(.),
                                             is_refrow(.))))] %>%
                     get_tot_n()
                 )
        )
      ))

    # Confidence intervals & per-cell significance (Phase 3a): the closed-form engine
    # (R/tab-agg.R) stores real asymmetric bounds ci_inf/ci_sup + the universal-inclusion
    # pvalue -- no per-cell DescTools. Weighted rule (§14): weighted proportion get_pct() /
    # weighted mean get_mean(), with the UNWEIGHTED base x_n (get_n of the relevant 100%
    # total). Cell CIs carry no pvalue; diff CIs star only when `stars` is on. A reference
    # cell has x_n = NA (ref_to_na) -> NA bounds, so it is never self-compared.
    tabs <- tabs %>%
      dplyr::with_groups(
        NULL,
        ~ dplyr::mutate(., dplyr::across(
          !!ci_select,
          ~ {
            col    <- dplyr::cur_column()
            want_p <- isTRUE(stars) && ci[[col]] %in% c("diff_row", "diff_col")
            res <- switch(
              ci[[col]],
              "cell" = switch(
                get_type(.),
                "mean" = ci_pivot(get_mean(.), sqrt(get_var(.) / x_n[[col]]),
                                  df = Inf, conf_level = conf_level, want_p = FALSE),
                # Phase 7g: the proportion cell CI honours method_cell (default wilson; wald opt-in).
                switch(method_cell,
                       "wilson" = ci_wilson(get_pct(.), x_n[[col]], conf_level = conf_level),
                       "wald"   = ci_wald(  get_pct(.), x_n[[col]], conf_level = conf_level))
              ),
              "diff_col" = ,
              "diff_row" = switch(
                get_type(.),
                "mean" = ci_mean_diff2(get_mean(.), get_var(.), x_n[[col]],
                                       ref[[col]], ref_var[[col]], ref_n[[col]],
                                       conf_level = conf_level, want_p = want_p),
                ci_prop_diff(get_pct(.), x_n[[col]], ref[[col]], ref_n[[col]],
                             conf_level = conf_level, method = method_diff, want_p = want_p)
              )
            )
            set_pvalue(set_ci_sup(set_ci_inf(., res$inf), res$sup), res$pvalue)
          }
        )))
    #tabs %>% dplyr::mutate(dplyr::across(where(is_fmt), get_ci))


    #Change ci_type and color, even for totals with no ci result
    ci_with_ref <- stringr::str_remove(ci_with_ref, "_row|_col")
    ci_yes_ref  <- !is.na(ci_with_ref) & !ci_with_ref == "no"

    tabs[ci_yes_ref] <-
      purrr::map2_df(tabs[ci_yes_ref],
                     ci_with_ref[ci_yes_ref],
                     ~ set_ci_type(.x, .y) %>%
                       set_color(
                         ifelse(!is.null(color[1]) & ! color[1] %in% c("no", ""),
                                color[1], get_color(.))
                       ))

    if (any(ci == "diff_row")) tabs <- tabs %>%
      dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., comp[1] == "all")))

    # Change types for columns where visible = TRUE
    if (any(visible & ci != "no" )) {
      tabs <-
        dplyr::mutate(tabs, dplyr::across(
          tidyselect::all_of(names(visible)[visible & ci != "no" ]),
          ~ switch(
            ci[dplyr::cur_column()],
            "cell" = set_display(., ifelse(get_type(.) == "mean",
                                           "mean_ci", "pct_ci")),
            set_display(., "ci")
          ) ) )
    }
  }


  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, chi2 = chi2)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext,
                    chi2 = chi2)
  }
}





#' Add Chi2 summaries to a \code{\link[tabxplor]{tab}}
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab_plain}} or
#' \code{\link{tab_many}}.
#' @param calc By default all elements of the Chi2 summary are calculated :
#' contributions to variance, pvalue, variance and unweighted count. You can choose which
#' are computed by selecting elements in the vector \code{c("ctr", "p", "var", "counts")}.
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param color The type of colors to print, as a single string.
#' \itemize{
#'   \item \code{"no"}: by default, no colors are printed
#'   \item \code{"all"}: color all cells based on their contribution to variance
#' (except for mean columns, from numeric variables)
#'   \item \code{"all_pct"}: color all percentages cells based on their contribution to
#'   variance
#'   \item \code{"auto"}: only color columns with counts, \code{pct = "all"} or
#'    \code{pct = "all_tabs"}
#' }
#' @return A \code{tibble} of class \code{tab}, with Chi2 summaries as metadata,
#' possibly colored based on contributions of cells to variance.
#' @export
#'
# @examples # A typical workflow with tabxplor step-by-step functions :
# \donttest{
# data <- dplyr::starwars %>%
#   tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#               na_drop_all = sex)
#
# data %>%
#   tab_plain(sex, hair_color, gender, tot = c("row", "col")) %>%
#   tab_chi2(calc = c("p", "ctr"), color = TRUE)
#   }
tab_chi2 <- function(tabs, calc = c("ctr", "p", "var", "counts"),
                     comp = NULL, color = c("no", "auto", "all", "all_pct")
) {
  get_vars        <- tab_get_vars(tabs)
  row_var         <- get_vars$row_var
  #col_vars        <- rlang::sym(get_vars$col_vars)
  col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  groups          <- rlang::syms(dplyr::group_vars(tabs))
  #ngroups         <- dplyr::n_groups(tabs)

  stopifnot(all(calc %in% c("all", "ctr", "p", "var", "counts")))
  if ("all" %in% calc) calc <- c("ctr", "p", "var", "counts")
  subtext         <- get_subtext(tabs)

  if (all(get_col_var(tabs) %in% c("", "no_col_var")) |
      "no_row_var" %in% names(tabs)
  ) return(tabs)

  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs %>% tab_match_comp_and_tottab(comp)

  is_a_mean <-
    purrr::map_lgl(col_vars_levels,
                   ~ purrr::map_lgl(dplyr::select(dplyr::ungroup(tabs), !!!.),
                                    ~ get_type(.) == "mean") %>% any()
    )
  # Phase 3b: mean col_vars now get an ANOVA F (the chi2 mirror), so an all-means table is no
  # longer skipped -- only the factor total-row/total-col scaffolding (which is factor-oriented)
  # is skipped for it. The ANOVA runs on the data rows (row_var-level groups) via agg_anova().
  if (!all(is_a_mean)) {
    tabs <- tabs %>% tab_match_groups_and_totrows() %>% tab_add_totcol_if_no()
  }

  if (comp == "all") tabs <- tabs %>% dplyr::ungroup()

  tot_cols <- detect_totcols(tabs)


  all_col_tot <- names(col_vars_levels) == "all_col_vars"

  tot_cols_names <- purrr::map_lgl(tabs, is_totcol) #%>%  .[.] %>% names()
  tot_cols_names <- tot_cols_names[tot_cols_names] %>% names()
  col_vars_levels_no_tot <-
    purrr::map(col_vars_levels,~ purrr::discard(., . %in% tot_cols_names ) )



  #Calculate absolute contributions to variance (with spread sign)
  if ("ctr" %in% calc | "var" %in% calc) {
    tabs <- tabs %>%
      dplyr::mutate(dplyr::across(
        where(~ is_fmt(.) & !get_type(.) == "mean" & !get_col_var(.) %in% c("no_col_var", "all_col_vars") ),
        ~ set_var(., var_contrib(
          .,
          tot  = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]]),
          calc = "ctr_with_sign",
          comp = comp
        ) )
      ))
    # tabs %>% dplyr::mutate(dplyr::across( where(is_fmt), ~ get_var(.)   ))


    #Calculate variances (per groups and per column variables)
    variances_calc <-
      purrr::map_if(col_vars_levels, !is_a_mean & !all_col_tot,
                    .f    = ~ dplyr::select(tabs, !!!groups, !!!.) %>%
                      dplyr::select(where(~ !is_totcol(.))) %>%
                      dplyr::mutate(dplyr::across(where(is_fmt),
                                                  ~ abs(get_var(.)))),
                    .else = ~ NA_real_ #Weighted mean of variances ?
      )

    variances_by_row <-
      purrr::map(variances_calc[!is_a_mean & !all_col_tot],
                 ~ dplyr::mutate(., dplyr::across(where(is.double),
                                                  ~ sum(., na.rm = TRUE))) %>%
                   dplyr::ungroup() %>%
                   dplyr::select(where(is.double)) %>% rowSums(na.rm = TRUE)
      )

    variances_by_group <-
      purrr::map_if(variances_calc[!all_col_tot], !is_a_mean[!all_col_tot],
                    .f    = ~ dplyr::group_split(.[!is_totrow(tabs),]) %>% #.keep = FALSE
                      purrr::map(~ dplyr::select(., where(is.double))) %>%
                      purrr::map_dbl(~ rowSums(., na.rm = TRUE) %>% sum(na.rm = TRUE)),
                    .else = ~ NA_real_ #Weighted mean of variances ?
      )


    cells_calc <- cells_by_group <-
      rlang::rep_along(variances_calc[!all_col_tot], NA_integer_)

    cells_calc[!is_a_mean[!all_col_tot]] <-
      variances_calc[!all_col_tot & !is_a_mean] %>%
      purrr::map(~ tibble::add_column(.x,  totrows = is_totrow(tabs)) %>%
                   dplyr::mutate(dplyr::across(
                     where(is.double), ~ dplyr::if_else(.data$totrows, 0,
                                                        dplyr::if_else(is.na(.), 0, 1))
                   )) %>%
                   dplyr::select(-"totrows")
      )


    cells_by_row <- cells_calc[!is_a_mean & !all_col_tot] %>%
      purrr::map2(col_vars_levels_no_tot[!all_col_tot & !is_a_mean],
                  ~ dplyr::mutate(.x, cells = sum(!!!.y), .groups = "drop") %>%
                    dplyr::pull(.data$cells)
      )

    cells_by_group[!is_a_mean[!all_col_tot]] <-
      cells_calc[!is_a_mean[!all_col_tot]] %>%
      purrr::map2(col_vars_levels_no_tot[!all_col_tot & !is_a_mean],
                  ~ dplyr::summarise(.x[!is_totrow(tabs),],
                                     cells = sum(!!!.y), .groups = "drop") %>%
                    dplyr::pull(.data$cells)
      )
  }


  #Calculate relative contributions to variance
  if ("ctr" %in% calc) {
    tabs <-
      purrr::reduce2(col_vars_levels[!is_a_mean & !all_col_tot],
                     purrr::transpose(list(var = variances_by_row,
                                           cell = cells_by_row)),
                     .init = tabs, .f = function(.tab, .levels, .l)
                       tibble::add_column(.tab,
                                          .var  = .l[["var"]],
                                          .cell = .l[["cell"]]) %>%
                       dplyr::mutate(dplyr::across(
                         tidyselect::all_of(purrr::map_chr(.levels, as.character)),
                         ~ dplyr::if_else(condition = is_totrow(.),
                                          true      = set_ctr(., 1/.data$.cell),
                                          false     = set_ctr(., .data$.var   ) )
                       )) %>%
                       dplyr::select(-".var", -".cell")
      )

    tabs <- tabs %>%
      dplyr::mutate(dplyr::across(
        where(is_fmt),
        ~ dplyr::if_else(condition = (comp == "tab" & is_totrow(.)) |
                           (comp == "all" & is_totrow(.) & is_tottab(.)),
                         true      = .,
                         false     = set_ctr(., get_var(.) / get_ctr(.)) )
      ))

    tabs <- tabs %>%
      dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., comp[1] == "all")))

    if (color[1] != "no" & !is.na(color[1])) {
      color_condition <-
        switch(color[1],
               "auto"    = c("n", "all", "all_tabs"),
               "all"     = c("n", "row", "col", "all", "all_tabs"),
               "all_pct" = c("all", "all_tabs")
        )

      tabs <- tabs %>% dplyr::mutate(dplyr::across(
        where(~ get_type(.) %in% color_condition),
        ~ set_color(., "contrib")
      ))
    }

    # tabs %>% dplyr::mutate(dplyr::across(where(is_fmt), get_ctr))
    # tabs %>% dplyr::mutate(dplyr::across(where(is_fmt), ~ set_display(., "ctr")))



    # #Relative contributions of col_vars levels (on total rows)
    # tabs <- tabs %>%
    #   dplyr::mutate(dplyr::across(
    #     where(is_fmt),
    #     ~ dplyr::if_else(condition = dplyr::row_number() == dplyr::n(),
    #                      true      = set_ctr(., sum(abs(get_ctr(.)))),
    #                      false     = . )
    #   ))
    # #tabs %>%  dplyr::mutate(dplyr::across( where(is_fmt), ~ set_display(., "ctr")  ))


    #mean_contrib <- contrib_no_sign %>% map(~ 1 / ( ncol(.) * nrow(.) ) )
  }

  tabs2 <- if (comp == "all") {
    tabs[!is_totrow(tabs) & !is_tottab(tabs),]
  } else {
    tabs[!is_totrow(tabs),]
  }

  # Drop any add_n / add_pct display rows (reserved row_var labels "n" / "row_pct") so a table that
  # already carries them is tested cleanly -- fixes tab_chi2() on an add_n'd table (the pipeline
  # runs the test before add_n, so this only matters for a manual chi2 on a built table).
  if (as.character(row_var) %in% names(tabs2)) {
    tabs2 <- tabs2[!as.character(tabs2[[as.character(row_var)]]) %in% c("n", "row_pct"), ]
  }


  # === Whole-table tests via the vectorised engine (R/tab-agg.R, Phase 3b) ============
  # DESIGN: chi2/ANOVA run on the already-AGGREGATED cell statistics (get_n / get_mean+get_var
  # over the fmt cells), never a raw N-scan -- cost scales with cells, not observations. Every
  # (subtable x col_var) is one "table_id"; ALL tables are stacked and tested in ONE agg_chi2 /
  # agg_anova pass (see the engine header). This replaces the pre-1.4.0 per-(sub)table
  # group_split() + stats::chisq.test() loop. Chi2 stays fully unweighted (chisq.test parity,
  # G2); ANOVA F follows §14 (weighted group mean/var + unweighted n).
  subtab_idx   <- dplyr::group_indices(tabs2)
  subtab_keys  <- dplyr::group_keys(tabs2)
  tab_vars_chr <- names(subtab_keys)
  n_rows2      <- nrow(tabs2)

  factor_cvs <- names(col_vars_levels)[!is_a_mean & !all_col_tot]
  mean_cvs   <- names(col_vars_levels)[ is_a_mean & !all_col_tot]

  # --- Chi2 for factor col_vars (UNWEIGHTED counts) ---
  chi2_rows <- NULL
  if (length(factor_cvs) > 0 && n_rows2 > 0) {
    long <- dplyr::bind_rows(purrr::imap(
      col_vars_levels_no_tot[factor_cvs],
      function(levels, cv) {
        lv_cols <- purrr::map_chr(levels, rlang::as_name)
        if (length(lv_cols) == 0) return(NULL)
        M  <- vapply(lv_cols, function(cc) as.double(get_n(tabs2[[cc]])), double(n_rows2))
        ncM <- ncol(M)
        tibble::tibble(
          col_var  = cv,
          subtab   = rep(subtab_idx, times = ncM),
          table_id = paste(cv, rep(subtab_idx, times = ncM), sep = "\r"),
          row_id   = rep(seq_len(n_rows2), times = ncM),
          col_id   = rep(seq_len(ncM), each = n_rows2),
          o        = as.vector(M)
        )
      }
    ))
    if (nrow(long) > 0) {
      res <- agg_chi2(long$table_id, long$row_id, long$col_id, long$o, correct = TRUE)
      map <- dplyr::distinct(long, .data$table_id, .data$col_var, .data$subtab)
      chi2_rows <- dplyr::left_join(map, tibble::as_tibble(res$tables), by = "table_id") %>%
        dplyr::transmute(
          .data$subtab, .data$col_var, test = "chi2",
          statistic = .data$statistic, df1 = as.double(.data$df), df2 = NA_real_,
          pvalue = .data$pvalue, n = as.double(.data$n),
          variance = NA_real_, min_e = .data$min_e)
    }
  }

  # --- ANOVA for mean col_vars (Welch + classic F, from per-group summary stats) ---
  anova_rows <- NULL
  if (length(mean_cvs) > 0 && n_rows2 > 0) {
    longA <- dplyr::bind_rows(purrr::imap(
      col_vars_levels[mean_cvs],
      function(levels, cv) {
        cols <- purrr::map_chr(levels, rlang::as_name)
        keep <- purrr::map_lgl(cols, ~ get_type(tabs2[[.x]]) == "mean" &&
                                 !any(is_totcol(tabs2[[.x]])))
        col  <- cols[keep][1]
        if (is.na(col)) return(NULL)
        tibble::tibble(
          col_var  = cv,
          subtab   = subtab_idx,
          table_id = paste(cv, subtab_idx, sep = "\r"),
          group_id = seq_len(n_rows2),
          n        = as.double(get_n(tabs2[[col]])),
          mean     = get_mean(tabs2[[col]]),
          var      = get_var(tabs2[[col]]))
      }
    ))
    if (nrow(longA) > 0) {
      resA  <- tibble::as_tibble(agg_anova(longA$table_id, longA$group_id,
                                           longA$n, longA$mean, longA$var))
      mapA  <- dplyr::distinct(longA, .data$table_id, .data$col_var, .data$subtab)
      baseA <- dplyr::left_join(mapA, resA, by = "table_id")
      welch <- dplyr::transmute(
        baseA, .data$subtab, .data$col_var, test = "F_welch",
        statistic = .data$statistic, df1 = .data$df1, df2 = .data$df2,
        pvalue = .data$pvalue, n = as.double(.data$n), variance = NA_real_, min_e = NA_real_)
      classic <- dplyr::transmute(
        baseA, .data$subtab, .data$col_var, test = "F_classic",
        statistic = .data$statistic_classic, df1 = .data$df1_classic, df2 = .data$df2_classic,
        pvalue = .data$pvalue_classic, n = as.double(.data$n), variance = NA_real_, min_e = NA_real_)
      anova_rows <- dplyr::bind_rows(welch, classic)
    }
  }

  # --- Assemble the tidy `test` attribute (one row per subtable x col_var x test-type) ---
  test_tbl <- dplyr::bind_rows(chi2_rows, anova_rows)
  if (nrow(test_tbl) == 0) {
    test_tbl <- new_test_tibble()
  } else {
    subtab_keys2 <- dplyr::mutate(subtab_keys, subtab = dplyr::row_number())
    test_tbl <- test_tbl %>%
      dplyr::arrange(.data$subtab, .data$col_var, .data$test) %>%
      dplyr::left_join(subtab_keys2, by = "subtab") %>%
      dplyr::mutate(row_var = !!row_var) %>%
      dplyr::select(-"subtab") %>%
      dplyr::relocate(tidyselect::any_of(tab_vars_chr), "row_var", "col_var")
  }

  tabs <- tabs %>% dplyr::select(-tidyselect::any_of("tottabs"))

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, test = test_tbl)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext,
                    test = test_tbl)
  }
}





# INTERNAL FUNCTIONS #####################################################################

#' @keywords internal
tab_match_groups_and_totrows <- function(tabs) {
  #chi2 : not to match groups and totrows with alltabs ? ----

  #tab_vars <- tab_get_vars(tabs)$tab_vars
  groups   <- dplyr::group_vars(tabs)

  #If there is a total_row at the end of each group, keep (un)grouping as is
  ind <- dplyr::group_indices(tabs) # 1 1 1 if data isn't grouped
  end_groups <- append(ind[-length(ind)] != ind[-1], FALSE)
  if (any(is_totrow(tabs)) & all(is_totrow(tabs)[end_groups]) ) {return(tabs)}

  #If there isn't any total row, keep actual (un)grouping and add some
  if ( !any(is_totrow(tabs))) {


    if (length(groups) != 0) {
      #if ( !identical(tab_vars, groups) ) {
      warning("no total row(s) found. Some added based on actual grouping variables : ",
              paste(groups, collapse = ", "))
      return(dplyr::group_by(tabs, !!!rlang::syms(groups)) %>% tab_tot("row"))
      # } else {
      #   tabs <- tabs %>% tab_tot("row")
      #   warning("no total row(s) found. One added for the whole table")
      # }
    } else if ( !any(is_tottab(tabs)) ) { #If there are no groups
      warning("no groups nor total row(s) found. One added for the whole table")
      return(tab_tot(tabs, "row"))
    } else {
      warning("no groups nor total row(s), but total table found. ",
              "Grouped upon tab_vars and total rows added")
      tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
      return(dplyr::group_by(tabs, !!!tab_vars) %>% tab_tot("row"))
    }

    #If there is at least one total row, calculate new groups based on them
  } else {
    if (utils::tail(is_totrow(tabs), 1L)) return(dplyr::ungroup(tabs))


    tabs_totrow_groups <- tabs %>% dplyr::ungroup() %>%
      tibble::add_column(totrow_groups = as.integer(is_totrow(.))) %>%
      dplyr::mutate(totrow_groups = 1 + cumsum(.data$totrow_groups) - .data$totrow_groups)
    totrow_indices <- tabs_totrow_groups$totrow_groups

    #Control if totrows groups match tab_vars, collectively or individualy, if yes group
    tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
    if ( !identical(tab_vars, groups) ) {
      tabs_tab_vars_groups <- tabs %>% dplyr::group_by(!!!tab_vars)
      tab_vars_indices <- dplyr::group_indices(tabs_tab_vars_groups)

      if (all(totrow_indices == tab_vars_indices)) return(tabs_tab_vars_groups)
    }

    each_tab_var_indices <-
      tabs %>% dplyr::ungroup() %>% dplyr::select(!!!tab_vars) %>%
      dplyr::transmute(dplyr::across(dplyr::everything(), as.integer)) %>%
      purrr::map(~ .)

    each_tab_var_totrow_comp <-
      purrr::map_lgl(each_tab_var_indices, ~ all(. == totrow_indices))

    if (any(each_tab_var_totrow_comp)) {
      group_var_name <- names(each_tab_var_totrow_comp[each_tab_var_totrow_comp])[1]
      return(dplyr::group_by(tabs, !!rlang::sym(group_var_name)))
    }

    # Otherwise return a df grouped with the total rows groups, in a new variable
    warning("grouping variable(s) not corresponding to total_rows, ",
            "new groups calculated, based on actual total_rows")
    return(dplyr::relocate(tabs_totrow_groups, .data$totrow_groups, .before = 1) %>%
             dplyr::group_by(.data$totrow_groups)
    )

  }

}



#' @keywords internal
tab_add_totcol_if_no <- function(tabs) {
  if (!any(is_totcol(tabs)) & ! all(get_type(tabs) == "mean")) { # & !only_one_column
    only_one_column <- length(which(purrr::map_lgl(tabs, is_fmt))) == 1L
    tabs <- tabs %>% tab_tot("col", totcol = "last")
    if (!only_one_column) warning("no total column, one was added (from the last non-mean column)")
  }
  tabs
}





#' @keywords internal
tab_validate_comp <- function(tabs, comp) {
  comp_all        <- purrr::map_lgl(tabs[purrr::map_lgl(tabs, is_fmt)],
                                    ~ get_comp_all(., replace_na = FALSE))
  comp_all_no_na  <- comp_all[!is.na(comp_all)]

  if (!all(is.na(comp_all))) {
    if(comp == "tab" & any(comp_all_no_na) ) {
      warning("since at least one column already have an element calculated ",
              "with comparison to the total row of the total table (pct or means ",
              "diffs from total, chi2 variances or confidence intervals), ",
              "comp were set to 'all'")
      comp <- "all"
    }
    if (comp == "all" & all(!comp_all_no_na) ) {
      warning("since at least one column already have an element calculated ",
              "with comparison to the total row of each tab_var (pct or means ",
              "diffs from total, chi2 variances or confidence intervals), ",
              "comp were set to 'tab'")
      comp <- "tab"
    }
  }
  if (comp == "null") {
    if ( all(is.na(comp_all)) ) {
      comp <- "tab"
    } else {
      comp <- ifelse(any(comp_all_no_na), "all", "tab")
    }
  }
  comp
}



#' @keywords internal
tab_match_comp_and_tottab <- function(tabs, comp) {
  if(comp == "all" & !any(is_tottab(tabs) & is_totrow(tabs)) ) {
    warning("since 'comp' is 'all', a total table with a ",
            "total row was added")
    tabs <- tabs %>% tab_totaltab('line')
  }
  tabs
}



# weighted.var() was removed in 1.4.0 (Phase 2): tab_num() now derives the weighted (ML) variance
# from moment sums in a single pass via num_derive_stats() (R/tab-agg.R), instead of a per-group
# helper that recomputed weighted.mean() on every call (the old double scan). The ML-vs-sample
# variance question it flagged is tracked for Phase 3 (dev/tabxplor_1.4.0_decisions.md §14).

#' @keywords internal
zscore_formula <- function(conf_level) {
  # Calculate the z-score for the given confidence level (thanks to mindcrime) :
  # https://datascience.stackexchange.com/questions/10093/how-to-find-a-confidence-level-given-the-z-value
  stopifnot(conf_level >= 0, conf_level <= 1)
  stats::qnorm((1 - conf_level)/2,lower.tail = FALSE)
}


# Phase 3a: the scalar mean-CI helpers ci_mean()/ci_mean_diff() and the DescTools proportion-CI
# closures ci_base()/ci_diff() were removed. All CI math now lives in the vectorised closed-form
# engine (ci_pivot/ci_wilson/ci_newcombe/ci_prop_diff/ci_mean_diff2, R/tab-agg.R). zscore_formula()
# above is kept -- the engine uses it for the normal quantile.


#' @keywords internal
pct_formula <- function(x, pct, tot) {
  switch(pct,
         "row"     =  get_wn(x) / get_wn(tot             ),
         "col"     =  get_wn(x) / get_wn(dplyr::last(x)  ),
         "all"     =  get_wn(x) / get_wn(dplyr::last(tot)),
         "all_tabs"=  get_wn(x) / get_wn(dplyr::last(tot)),
         NA_real_)
}

#' @keywords internal
diff_formula <- function(x, type, ref, refer) {
  switch(
    ref,
    "tot"   = switch(type,
                     "row"     =  get_pct(x)  - get_pct(dplyr::last(x  )),
                     "col"     =  get_pct(x)  - get_pct(refer             ),
                     "mean"    =  get_mean(x) / get_mean(dplyr::last(x) ),
                     NA_real_),
    "first" = switch(type,
                     "row"     =  get_pct(x)  - get_pct(dplyr::first(x  )),
                     "col"     =  get_pct(x)  - get_pct(refer              ),
                     "mean"    =  get_mean(x) / get_mean(dplyr::first(x) ),
                     NA_real_)
  )
}


#' @keywords internal
var_contrib <- function(x, tot, calc = c("ctr", "expected_freq", "spread",
                                         "binding_ratio",
                                         "ctr_with_sign"),
                        comp = NULL) {
  # x   <- tabs$Encadrant
  # tot <- tabs$Total
  xout             <- get_wn(x)
  tot <- get_wn(tot)
  if (!is.null(comp)) { if (comp == "all") {
    tot_row_or_tab <- is_totrow(x[-length(x)]) | is_tottab(x[-length(x)])
    xout[-length(x)] <-
      dplyr::if_else(tot_row_or_tab, rep(0, length(x) -1), xout[-length(x)])

    tot[-length(x)] <-
      dplyr::if_else(tot_row_or_tab, rep(0, length(x) -1), tot [-length(x)])
  }}

  observed_freq     <- xout / dplyr::last(tot)
  expected_freq     <- dplyr::last(xout) * tot / dplyr::last(tot)^2
  spread            <- observed_freq - expected_freq
  switch(calc[1],
         "ctr"           = spread^2 / expected_freq, # = expected_freq * binding_ratio^2,
         "spread"        = spread                  ,
         "binding_ratio" = spread   / expected_freq,
         "expected_freq" = expected_freq           ,
         "ctr_with_sign" = sign(spread) * spread ^2 / expected_freq
  )
  #tidyr::replace_na(res, 0)
}




#' @keywords internal
quo_miss_na_null_empty_no <- function(quo) {
  if (rlang::quo_is_missing(quo)) return (TRUE)
  if (rlang::quo_is_null(quo)) return(TRUE)
  base_quo <- quo
  quo <- rlang::get_expr(quo) %>% as.character()
  # message(paste0(quo, collapse = ", "))

  # if (quo[1] %in% c("all_of", "any_of") & exists(quo[2])) {
  #   if (is.character(rlang::eval_tidy(rlang::sym(quo[2])))) {
  #     if (all(rlang::eval_tidy(rlang::sym(quo[2])) %in% c("", "no",
  #                                                         "no_row_var",
  #                                                         "no_col_var"))) {
  #       return(TRUE)
  #     }
  #   }
  # }

  all(is.na(quo) | quo %in% c("", "no")) |
    (quo[1] %in% c("all_of", "any_of") &
       !is.na(quo[2]) & quo[2] %in% c("", "no", "no_row_var", "no_col_var"))
}


#' @keywords internal
as_df_merge_rownames <- function(tabs, row_var) {
  text_cols <- !purrr::map_lgl(tabs, is.numeric)
  text_cols <- names(text_cols)[which(text_cols)]
  new_rownames  <- paste0(text_cols, collapse = "_")

  if (length(text_cols) >= 2) {
    tabs <- tabs %>%
      tibble::as_tibble() %>%
      dplyr::mutate(!!new_rownames :=
                      paste(!!!purrr::map(text_cols, rlang::sym), sep = "_")) %>%
      dplyr::select(-tidyselect::all_of(text_cols)) %>%
      dplyr::relocate(where(is.character), .before = 1) %>%
      tibble::column_to_rownames(var = new_rownames)
  } else {
    #tabs <- tabs %>% tibble::column_to_rownames(var = rlang::as_name(row_var))
    rnames <- as.character(tabs[[row_var]])
    tabs[, eval(row_var) := NULL]
    data.table::setDF(tabs, rownames = rnames)
  }

  tabs
}

#' @keywords internal
# Guard against a factor level / character value equal to a column name (which would collide with
# data.table internals) by relabelling it to "<value>_lv". Examine ONLY the col_vars targets, never
# the other columns: a `where()` predicate over all columns coerced a numeric `wt` column's whole
# 8M-row vector to strings (~15s x2 calls) -> the weighted-table 60x slowdown. Short-circuit &&/||
# so a numeric target costs nothing; selection set and transform are unchanged (byte-identical out).
relabel_levels_in_varnames <- function(data, col_vars) {
  nms      <- names(data)
  col_vars <- intersect(col_vars, nms)
  needs <- purrr::map_lgl(col_vars, function(v) {
    col <- data[[v]]
    (is.factor(col)    && any(levels(col) %in% nms)) ||
      (is.character(col) && any(unique(col) %in% nms))
  })
  targets <- col_vars[needs]
  if (length(targets) == 0) return(data)
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(targets),
      ~ forcats::fct_relabel(., ~ dplyr::if_else(. %in% nms, paste0(., "_lv"), .))
    ))
}

#' @keywords internal
diff_index <-  function(ref, row_var, num_names, pct) {
  if (ref == "tot"   ) return(-1L)
  if (ref == "first" ) return(1L )
  if (is.numeric(ref) | !is.na(suppressWarnings(as.integer(ref)))
  ) {
    return(as.integer(ref[1]))
  }

  targets <- switch(pct, "row" = row_var, "col" = num_names)
  # Phase 7g-iii: try an EXACT match first, so a chosen level label (which may contain regex
  # metacharacters -- e.g. "$25000 or more" -- or be a substring of another level) selects exactly
  # its own row/column. This is what fixes the jmvtab reference picker: a raw level label is matched
  # literally, not as a broken/ambiguous regex. Fall back to REGEX matching (the documented `ref`
  # behaviour) only when no target is exactly equal to `ref`.
  exact <- which(targets == ref)
  index <- if (length(exact) >= 1L) exact else which(stringr::str_detect(targets, ref))
  if (length(index) >= 2) {
    switch(pct,
           "row" = warning(paste0(
             "with ref = '", ref, "' , several rows were found as ",
             "reference for comparison ; only the first was kept ; ",
             "to remove this warning, precise the value of ref ",
             "until there is only one row_var level matched"
           )),

           "col" = warning(paste0(
             "with ref = '", ref, "' , several columns were found as ",
             "reference for comparison ; only the first was kept ; ",
             "to remove this warning, precise the value of ref ",
             "until there is only one column matched"
           ))
    )
  }
  index <- tidyr::replace_na(dplyr::first(index), 0)

  if (length(index) == 0) index <- 0

  index
}

#' @keywords internal
calculate_refrows <- function(tabs, ref, comp, tab_row_names, tab_vars,
                              row_var, tottab_vector, totrow_vector, # pct,
                              num_names) {
  if (ref != "tot") {
    refrows <-
      if(comp == "tab") {
        tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
          dplyr::group_by(!!!tab_vars) |>
          dplyr::transmute(
            var =
              dplyr::row_number() == if (diff_index(ref, !!row_var,
                                                    num_names = num_names,
                                                    pct = "row") == -1) {
                dplyr::n()
              } else {
                diff_index(ref, !!row_var, num_names = num_names, pct = "row")
              }
          ) |>
          dplyr::pull("var")

      } else {
        tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
          dplyr::mutate(tottab_vector = tottab_vector) |>
          dplyr::group_by(!!!tab_vars) |>
          dplyr::transmute(
            var = dplyr::if_else(
              condition = .data$tottab_vector,
              true  = dplyr::row_number() == if (diff_index(ref, !!row_var,
                                                            num_names = num_names,
                                                            pct = "row") == -1) {
                dplyr::n()
              } else {
                diff_index(ref, !!row_var, num_names = num_names, pct = "row")
              },
              false = FALSE
            )
          ) |>
          dplyr::pull("var")
      }
    #tabs_diff$DIPLOME[refrows] |> as.character()

    if (!any(refrows)) {
      warning(paste0(
        "in ref = '", ref, "' , no rows were found as reference for comparison ; ",
        "to remove this warning, precise the value of ref ",
        "until there is one row_var level matched"
      ))
    }
  } else {
    refrows <- if (comp == "tab") { totrow_vector } else { totrow_vector & tottab_vector }
  }
  refrows <- tidyr::replace_na(refrows, FALSE)

  return(refrows)
}



# tab_ci former implementation ----
# tabs_nogroup <- tabs %>% dplyr::ungroup() %>% .[ci_yes]
#
# #Compute all variables needed to calculate ci in different tabs
# xbase <- tabs_nogroup %>%
#   dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns =  ~ dplyr::if_else(
#     condition = get_display(.) == "mean",
#     true      = get_mean(.),
#     false     = get_pct(.)
#   )))
#
# xvar <- tabs_nogroup %>%
#   dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ dplyr::if_else(
#     condition = get_display(.) == "mean",
#     true      = get_var(.),
#     false     = NA_real_
#   )))
#
# ybase <-
#   tibble::tibble(ci, tot_cols, names = rlang::syms(names(tabs))) %>%
#   dplyr::filter(ci_yes) %>%
#   purrr::pmap_df(function(ci, tot_cols, names) switch(
#     ci,
#     "cell"     = NA_real_,
#     "diff_col" = dplyr::pull(tabs, !!tot_cols),
#     "diff_row" = dplyr::mutate(tabs, comp = dplyr::last(!!names)) %>%
#       dplyr::pull(comp)
#   ))
#
# yvar <- ybase %>%
#   dplyr::mutate(dplyr::across(where(~ !get_type(.)=="mean"), ~NA_real_)) %>%
#   dplyr::mutate(dplyr::across(where(~ get_type(.) =="mean"), get_var))
#
# ybase <- ybase %>%
#   dplyr::mutate(dplyr::across(
#     where(~ is_fmt(.) & !get_type(.) == "mean"),
#     get_pct
#   )) %>%
#   dplyr::mutate(dplyr::across( where(~ get_type(.) == "mean"), get_mean))
#
# xn <-
#   tibble::tibble(type, tot_cols, names = rlang::syms(names(tabs))) %>%
#   dplyr::filter(ci_yes) %>%
#   purrr::pmap_df(function(type, tot_cols, names) switch(
#     type,
#     "row"      = dplyr::pull(tabs, !!tot_cols) %>% get_n(),
#     "mean"     = dplyr::pull(tabs, !!names   ) %>% get_n(),
#     "col"      = dplyr::mutate(tabs, xn = dplyr::last(get_n(!!names)) ) %>%
#       dplyr::pull(xn),
#     "all"      = ,
#     "all_tabs" = dplyr::mutate(tabs, xn = dplyr::last(get_n(!!tot_cols)) ) %>%
#       dplyr::pull(xn),
#     NA_integer_
#   ))
#
# yn <-
#   tibble::tibble(ci, type, tot_cols, names = rlang::syms(names(tabs))) %>%
#   dplyr::filter(ci_yes) %>%
#   purrr::pmap_df(function(ci, type, tot_cols, names) switch(
#     ci,
#     "cell"       = NA_real_,
#     "diff_col"   =
#       switch(type,
#              "row"      = dplyr::pull(tabs, !!tot_cols) %>% get_n(),
#              "col"      = ,
#              "all"      = ,
#              "all_tabs" =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!tot_cols)) ) %>%
#                dplyr::pull(yn),
#              NA_real_
#       ),
#     "diff_row"   =
#       switch(type,
#              "mean"     = ,
#              "col"      =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!names)) ) %>%
#                dplyr::pull(yn),
#              "row"      = ,
#              "all"      = ,
#              "all_tabs" =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!tot_cols)) ) %>%
#                dplyr::pull(yn),
#              NA_real_
#       )
#   ) )
#
#
# ci_map <-
#   list(xbase = xbase, xvar = xvar,
#        ybase = ybase, yvar = yvar,
#        xn = xn, yn = yn) %>%
#   purrr::map(~purrr::map(., ~ .)) %>%
#   purrr::transpose() %>% purrr::map(~ tibble::as_tibble(.)) %>%
#   tibble::tibble(.name_repair = ~ "ci_map") %>%
#   tibble::add_column(ci = ci[ci_yes], type = type[ci_yes]) %>%
#   dplyr::mutate(ci_map = dplyr::if_else(
#     ci %in% c("diff_col", "diff_row"),
#     true  = purrr::map(ci_map, ~ dplyr::mutate(., xn = dplyr::if_else(
#       condition =
#         ( comp == "tab" & is_totrow(tabs) ) |
#         ( comp == "all" & append(rep(FALSE, nrow(tabs) - 1), TRUE)),
#       true      = NA_integer_,
#       false     = xn)
#     )),
#     false = ci_map
#   ) %>% purrr::set_names(names(tabs)[ci_yes])
#   )
#
# calculations <- ci_map %>%
#   purrr::pmap(function(ci_map, ci, type)
#     dplyr::mutate(ci_map, res = switch(
#       ci,
#       "cell"        = switch(type,
#                              "mean" = ci_mean(xvar = xvar, xn = xn),
#                              ci_base(xpct = xbase, xn = xn)
#       ),
#       "diff_col"   = ,
#       "diff_row"   = switch(type,
#                             "mean" = ci_mean_diff(xvar = xvar, xn = xn,
#                                                   yvar = yvar, yn = yn),
#                             ci_diff(xpct = xbase, xn = xn,
#                                     ypct = ybase, yn = yn)
#       ),
#       # "spread_col" = ,
#       # "spread_row" = switch(type,
#       #                       "mean" = ci_mean_spread(
#       #                         xmean = xbase,  xvar = xvar, xn = xn,
#       #                         ymean = ybase,  yvar = yvar, yn = yn
#       #                       ),
#       #                       ci_diff_spread(xpct = xbase, xn = xn,
#       #                                      ypct = ybase, yn = yn)
#       # ),
#       "no"         = NA_real_,
#     ) ) )
#
# result <- calculations %>% purrr::map_df(~ dplyr::pull(., res))
#
# tabs[ci_yes] <- purrr::map2_df(tabs[ci_yes], result, ~ set_ci(.x, .y) )

# resolve_ref_vector() -- Phase 6d (§4): resolve a `ref` spec against a set of variable keys.
# A scalar applies to every key (recycled -- byte-identical to the old behaviour). A NAMED
# character vector matches keys by name (unmatched keys fall back to "auto"; names matching no
# key warn). An unnamed length>1 vector matches by order (must recycle to the number of keys).
# Returns an unnamed vector of length = length(row_vars_chr). Used for the per-row_var reference
# (row%/means) and, Phase 7g-iii, the per-col_var reference (col%) -- `what` only names the axis
# in the "no match" warning.
resolve_ref_vector <- function(ref, row_vars_chr, what = "row_var") {
  n <- length(row_vars_chr)
  # An UNNAMED length-1 ref is a scalar applied to every key; a NAMED length-1 ref must still be
  # matched by name (else a single-name vector like c(race = "Black") would recycle to ALL keys).
  if (length(ref) == 1L && is.null(names(ref))) return(vctrs::vec_recycle(ref, n))
  nms <- names(ref)
  if (!is.null(nms) && any(nzchar(nms))) {
    unknown <- setdiff(nms[nzchar(nms)], row_vars_chr)
    if (length(unknown)) {
      cli::cli_warn("{.arg ref} name{?s} {.val {unknown}} match no {what} and {?is/are} ignored.")
    }
    out  <- rlang::set_names(rep("auto", n), row_vars_chr)
    keep <- intersect(nms, row_vars_chr)
    out[keep] <- as.character(ref[keep])
    unname(out)
  } else {
    vctrs::vec_recycle(ref, n)
  }
}


# tab_apply_tests() -- the shared "chi2 -> capture test -> ci" finalize block for ONE built
# factor table. Extracted (Phase 6a) so tab_many() and tab_counts() construct the
# tab_chi2()/tab_ci() calls in exactly ONE place: the argument wiring must stay in sync (the
# whole-table `test` attribute + per-cell CI fmt fields flow through here).
# Returns list(tab = <table, CI/contrib fmt fields set>, test = <whole-table test tibble>).
# The `test` is captured BETWEEN chi2 and ci and re-attached by the caller at rewrap, matching
# the historical order (chi2 -> get_test -> ci). `do_chi2` is the per-table chi2 flag; `ci ==
# "no"` skips the CI step. WARNING: keep byte-identical to the pre-6a two-batch passes.
tab_apply_tests <- function(tab, do_chi2, ci, comp, color_ctr, color_ci,
                            conf_level, stars, method_cell, method_diff,
                            cached_test = NULL) {
  if (isTRUE(do_chi2)) {
    # Phase 7e tier-2 cache: on a hit (cached_test supplied) and the common non-contrib path,
    # inject the cached omnibus test instead of re-running the vectorised engine. Restricted to
    # color_ctr == "no": contrib coloring (calc = c("ctr","p")) also writes the per-cell ctr/var
    # FIELDS, which are not in the test tibble, so it must recompute. tab_chi2(calc = "p",
    # color = "no") is structurally identity on transform tables (totrow+totcol already present),
    # so skipping it changes only the `test` attribute (locked by test-jmvtab-cache.R).
    if (!is.null(cached_test) && color_ctr == "no") {
      tab <- set_test(tab, cached_test)
    } else {
      tab <- tab_chi2(tabs = tab,
                      calc = if (color_ctr != "no") c("ctr", "p") else "p",
                      comp = comp, color = color_ctr)
    }
  }

  test <- get_test(tab)
  if (is.null(test)) test <- new_test_tibble()

  if (ci != "no") {
    tab <- tab_ci(tabs = tab, ci = ci, comp = comp, conf_level = conf_level,
                  color = color_ci, visible = ci == "cell", stars = stars,
                  method_cell = method_cell, method_diff = method_diff)
  }

  list(tab = tab, test = test)
}


# tab_add_n_pct() -- append the base-n column (add_n) and/or the col%/row% companion
# (add_pct) to each built factor table. Extracted verbatim from tab_many()'s finalize so
# BOTH tab_many() and tab_counts() share ONE implementation (no divergence). Operates on the
# tabs_text LIST (one entry per row_var); returns it modified. See CLAUDE.md Phase 4.
tab_add_n_pct <- function(tabs_text, add_n, add_pct) {
  if (!add_n && !add_pct) return(tabs_text)

    # cols, with pct = "row"
    last_totcols_pct_rows <- tabs_text |>
      purrr::imap_chr(
        ~ dplyr::last(names(.x)[is_totcol(.x) & get_type(.x) == "row" &
                                  get_col_var(.x) != "no_col_var" &
                                  tab_get_vars(.)$row_var != "no_row_var"]) |>
          purrr::set_names(.y)
      )

    # last_totcols_pct_rows <- tabs_text |>
    #   purrr::map(~ dplyr::mutate(., across(where(is_fmt), ~ set_type(., "col")))) |>
    #   purrr::imap_chr(~ dplyr::last(names(.x)[is_totcol(.x) & get_type(.x) == "row"]) |>
    #                 purrr::set_names(.y)
    #
    #   )
    last_totcols_pct_rows <- last_totcols_pct_rows[!is.na(last_totcols_pct_rows)]

    if (length(last_totcols_pct_rows) > 0) {
      if (add_pct) {
        tabs_text <- tabs_text |>
          purrr::map2(
            last_totcols_pct_rows,
            ~ dplyr::mutate(
              .x,
              col_pct := dplyr::mutate(
                !!rlang::sym(.y),
                pct = get_wn(!!rlang::sym(.y)) /
                  dplyr::last(get_wn(!!rlang::sym(.y)),
                              #which(get_reference(!!rlang::sym(.y), "lines"))
                  )
              ) |>
                set_type("col") |> as_totcol(FALSE) |> set_color("no") |>
                set_col_var("all_col_vars") |>
                set_diff(NA_real_) |> set_ci(NA_real_) |> set_mean(NA_real_) |>
                set_ctr(NA_real_) |> set_var(NA_real_)
            )
          )
      }

      if (add_n) {
        tabs_text <- tabs_text |>
          purrr::map2(
            last_totcols_pct_rows, ~ dplyr::mutate(
              .x, # !!rlang::sym(paste0(names(.y), "_n"))
              n = set_display(!!rlang::sym(.y), "n") |>
                set_type("n") |> as_totcol(FALSE) |> set_color("no") |>
                set_col_var("all_col_vars") |>
                set_diff(NA_real_) |> set_ci(NA_real_) |> set_mean(NA_real_) |>
                set_pct(NA_real_) |> set_ctr(NA_real_) |> set_var(NA_real_)
            )
          )
      }

    }


    # rows, with pct = "col"
    last_totrow <- tabs_text |>
      purrr::map_int(
        ~ dplyr::last(which(is_totrow(.) & tab_get_vars(.)$row_var != "no_row_var"),
                      default = NA_integer_)
      )
    last_totrow <- last_totrow[!is.na(last_totrow)]
    if (length(last_totrow) > 0) {


      last_totrow_pct_cols <- tabs_text |>
        purrr::map(~ names(.)[get_type(.) == "col" & get_col_var(.) != "no_col_var" &
                                 names(.) != "col_pct"] )
      last_totrow_pct_cols_no_empty <- purrr::map_lgl(last_totrow_pct_cols, ~ length(.) > 0)
      # last_totrow_pct_cols <- last_totrow_pct_cols[last_totrow_pct_cols_no_empty]


      if (any(last_totrow_pct_cols_no_empty)) {

        if (add_pct) {
          tabs_text <-
            purrr::pmap(
              list(tabs_text, last_totrow_pct_cols_no_empty, last_totrow, last_totrow_pct_cols),
              ~ {
                totcols_ref <- purrr::map_chr(detect_totcols(..1), as.character)
                if (..2) {
                  dplyr::bind_rows(
                    ..1,
                    dplyr::slice(..1, ..3) |>
                      dplyr::mutate(
                        dplyr::across(
                          where(is_fmt),
                          ~ dplyr::mutate(
                            .,
                            pct = get_wn(.) /
                              get_wn(rlang::eval_tidy(
                                rlang::sym(totcols_ref[[dplyr::cur_column()]])
                              ))
                          )
                        ),
                        dplyr::across(where(is_fmt), ~ as_totrow(., FALSE) |>
                                        set_diff(NA_real_) |> set_ci(NA_real_) |>
                                        set_mean(NA_real_) |>
                                        set_ctr(NA_real_) |> set_var(NA_real_)
                                        ),
                        dplyr::across(
                          where(is_fmt) & -tidyselect::all_of(..4),
                          ~ set_num(., value = NA_real_)
                        ),
                        dplyr::across(
                          all_of(tab_get_vars(..1)$row_var),
                          ~ factor("row_pct")
                        )
                      )

                  )
                } else {
                  ..1
                }
              }
            )
        }

        if (add_n) {
          tabs_text <-
            purrr::pmap(list(tabs_text, last_totrow_pct_cols_no_empty, last_totrow, last_totrow_pct_cols),
                        ~ if (..2) {
                          dplyr::bind_rows(
                            ..1,
                            dplyr::slice(..1, ..3) |> set_display("n") |>
                              dplyr::mutate(
                                dplyr::across(where(is_fmt), ~ as_totrow(., FALSE)  |>
                                                set_diff(NA_real_) |> set_ci(NA_real_) |>
                                                set_mean(NA_real_) |> set_pct(NA_real_) |>
                                                set_ctr(NA_real_) |> set_var(NA_real_)
                                              ),
                                dplyr::across(
                                  where(is_fmt) & -tidyselect::all_of(..4),
                                  ~ set_num(., value = NA_real_)
                                ),
                                dplyr::across(
                                  all_of(tab_get_vars(..1)$row_var),
                                  ~ factor("n")
                                )
                              )

                          )
                        } else {
                          ..1
                        }
            )
        }

      }

    }


    # tabs_text |>
    #   purrr::map(
    #     ~ dplyr::mutate(., dplyr::across(
    #       dplyr::where(is_totcol),
    #       ~ set_display(., "n") |> set_type("n") |>
    #         as_totcol(FALSE) |> set_color("no"),
    #       .names = "{.col}_.nnnnnn" # paste0(, "_n")
    #     )
    #     ) %>%
    #       dplyr::rename(all_of(
    #         purrr::set_names(
    #           names(.)[stringr::str_detect(names(.), "_.nnnnnn$")],
    #           paste0(get_col_var(.)[stringr::str_detect(names(.), "_.nnnnnn$")], "_n")
    #         )
    #       ))
    #   )


  tabs_text
}


# tab_apply_n_min() -- the small-base display filter (Phase 7g). A PURE end-of-pipeline DISPLAY
# helper: it recomputes NOTHING (no fields, no chi2/ANOVA, no CI). The user has already seen the
# whole table; n_min just strips the noise of unreliable small-base cells so it reads cleanly.
# Rule: for row-oriented columns (type row/all/mean) drop a row only if its LARGEST base across
# those columns is < n_min, then blank (display "") each surviving cell whose OWN base < n_min;
# for col-oriented columns (type "col", the pct="col" case) drop the whole column when its base
# is < n_min. Orientation is read from each fmt column's stored `type`, so no `pct` argument is
# needed and mixed tables Just Work. Base = get_tot_n() for proportions, get_n() for means; an NA
# base is never weak. NEVER drops: total rows/tables, the total column, add_n/add_pct helper rows
# (row_var "n"/"row_pct") or columns (col_var "all_col_vars"), or the p-value line (all n NA).
# Class + attributes (subtext/test/grouping) survive via the tabxplor dplyr S3 methods.
tab_apply_n_min <- function(tab, n_min) {
  if (length(n_min) == 0 || is.na(n_min[1]) || n_min[1] <= 0) return(tab)
  n_min <- n_min[1]
  if (!is.data.frame(tab)) return(tab)

  fmt_names <- names(tab)[purrr::map_lgl(tab, is_fmt)]
  if (length(fmt_names) == 0) return(tab)

  type   <- purrr::map_chr(tab[fmt_names], get_type)
  helper <- purrr::map_lgl(tab[fmt_names], ~ get_col_var(.) == "all_col_vars")
  totcol <- purrr::map_lgl(tab[fmt_names], is_totcol)

  cell_base <- function(col) if (get_type(col) == "mean") get_n(col) else get_tot_n(col)

  # --- protected rows (never dropped) --------------------------------------------------------
  fmt_all <- tab[fmt_names]
  totrow  <- purrr::reduce(purrr::map(fmt_all, is_totrow), `|`)
  tottab  <- purrr::reduce(purrr::map(fmt_all, is_tottab), `|`)
  pline   <- purrr::reduce(purrr::map(fmt_all, ~ is.na(get_n(.))), `&`)   # the p-value line
  rvars   <- tab_get_vars(tab)$row_var
  rvars   <- rvars[rvars %in% names(tab)]
  helprow <- if (length(rvars)) {
    purrr::reduce(purrr::map(tab[rvars], ~ as.character(.) %in% c("n", "row_pct")), `|`)
  } else rep(FALSE, nrow(tab))
  protect <- totrow | tottab | pline | helprow

  # --- row-drop + cell-blank on row-oriented columns -----------------------------------------
  row_cols <- fmt_names[type %in% c("row", "all", "mean") & !helper]  # totcol INCLUDED in the max
  if (length(row_cols) > 0) {
    bases    <- purrr::map(tab[row_cols], ~ { b <- cell_base(.); b[is.na(b)] <- Inf; b })
    row_base <- purrr::reduce(bases, pmax)
    keep     <- protect | !(row_base < n_min)
    if (!all(keep)) {
      # Filter globally: a grouped_tab would split the length-n `keep` per group, so ungroup,
      # filter, then restore the grouping (the tabxplor S3 methods carry subtext/test through).
      gv  <- dplyr::group_vars(tab)
      tab <- dplyr::ungroup(tab)
      tab <- dplyr::filter(tab, keep)
      if (length(gv) > 0) tab <- dplyr::group_by(tab, dplyr::across(tidyselect::all_of(gv)))
    }
  }
  # blank surviving weak cells (row-oriented, non-total, non-helper stat columns)
  blank_cols <- fmt_names[type %in% c("row", "all", "mean") & !helper & !totcol]
  blank_cols <- intersect(blank_cols, names(tab))
  if (length(blank_cols) > 0) {
    tab <- dplyr::mutate(tab, dplyr::across(
      tidyselect::all_of(blank_cols),
      ~ {
        b <- cell_base(.)
        w <- !is.na(b) & b < n_min
        if (any(w)) .[w] <- set_display(.[w], "blank")
        .
      }
    ))
  }

  # --- column-drop on col-oriented columns (pct = "col") -------------------------------------
  drop_cols <- fmt_names[type == "col" & !helper & !totcol]
  drop_cols <- intersect(drop_cols, names(tab))
  if (length(drop_cols) > 0) {
    weak <- purrr::map_lgl(tab[drop_cols], ~ {
      mb <- suppressWarnings(max(get_tot_n(.), na.rm = TRUE))
      is.finite(mb) && mb < n_min
    })
    if (any(weak)) tab <- dplyr::select(tab, -tidyselect::all_of(drop_cols[weak]))
  }

  tab
}


