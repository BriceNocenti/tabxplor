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
# #' dplyr::storms |>
# #'   tab(status, category) |>
# #'   tab_sup(sup_rows = c("pressure", "wind"), print_sup = TRUE)
# #'
# #' \donttest{
# #' forcats::gss_cat |>
# #'   tab(marital, race, perc = "row") |>
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
# #'   data = forcats::gss_cat, sort_by = c("White", "desc")) #|>
# #' #tab_xl(only_one_sheet = TRUE)
# tab_last <- function() {"Nothing"}


# MAIN USER-FRIENDLY FUNCTIONS ###########################################################


#' Cross-table with color helpers
#'
#' @description
#' `tab()` builds a cross-table of one or several row variables by one or several column
#' variables, and colors the cells so the table is easy to read at a glance --- in the R
#' console, or exported to Excel, HTML or Word. Cells can show counts, row or column
#' percentages, or (for a numeric column variable) means, optionally with differences,
#' confidence intervals and statistical tests.
#'
#' The result is a `tibble` (of class `tabxplor_tab`), so you can keep working on it with the
#' usual \pkg{dplyr} verbs ([dplyr::select()], [dplyr::filter()], [dplyr::arrange()],
#' [dplyr::mutate()]).
#'
#' New to the package? Start with `vignette("tabxplor")` and with just four arguments ---
#' `data`, `row_vars`, `col_vars` and `pct` --- then add `color` when you want reading helpers.
#'
#' @details
#' `tab()` has many arguments, but you only need a handful to begin. They fall into groups:
#' \itemize{
#'   \item **The table**: `data`, `row_vars`, `col_vars`, `tab_vars` (one sub-table per group),
#'     `wt` (a weight variable).
#'   \item **What each cell shows**: `pct` (row or column percentages, or leave counts), `digits`.
#'   \item **Colors (reading helpers)**: `color`, and `color_signif` (whether statistical
#'     significance gates the color). Thresholds and palettes are set once for the whole session
#'     with [set_color_breaks()] and [set_color_palette()]; a color legend prints automatically.
#'   \item **Comparisons**: `OR` (odds ratios), and `ref` / `ref2` / `comp` (which cell is the
#'     baseline for differences).
#'   \item **Statistics**: `test` (chi-squared or Welch's F), and `ci` + `conf_level` + `stars`
#'     (confidence intervals). The fine interval methods (`method_cell`, `method_diff`, ...) are
#'     documented on [tab_ci()].
#'   \item **Totals & missing values**: `tot`, `total_names`, `totaltab`, `na`, `levels`.
#'   \item **Advanced / output**: `display`, `n_min`, `output_list`, `parallel`, `spread_vars`,
#'     `filter`.
#' }
#' The package-wide display, color and statistics defaults are `options()`, listed at
#' [tabxplor-options]. `tab()` is a friendly wrapper around the more powerful [tab_many()].
#'
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
#'   When several \code{row_vars} are combined into one table (no \code{tab_vars}), their \code{Total}
#'   rows are identical whenever they share one population (\code{"keep"}, \code{"drop_all"},
#'   \code{"common_base"}) and are then displayed as a \strong{single} Total row; only \code{"drop"}
#'   can make them genuinely differ, in which case every Total row is kept (with a message).
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
#' @param display A single optional \strong{composite display template} to show several fields in each
#'   value cell (text output only -- the console, \code{\link{tab_kable}} and \code{\link{tab_md}};
#'   Excel falls back to the primary field). A \code{\{\}} template listing the fields to combine, e.g.
#'   \code{"\{pct\} (n=\{n\})"} (a percentage with its count), \code{"\{n\} (\{pct\})"} or
#'   \code{"\{diff\} \{ci\}"}. Valid fields: \code{pct}, \code{n}, \code{wn}, \code{mean},
#'   \code{diff}, \code{ratio}, \code{ci}, \code{or}, \code{ctr}, \code{var}; the first field is the
#'   \emph{primary}, shown alone by Excel and used for coloring. A bare field name is also accepted as a
#'   shorthand for its single-field template, so \code{display = "ci"} is the same as
#'   \code{display = "\{ci\}"} (it shows the confidence interval). \code{NULL} (default) keeps the plain
#'   single-field display. It is a display overlay only: colors, differences and the underlying fields
#'   are unchanged.
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
#' @param ref2 The second reference level for odds ratios (or relative risk ratios), needed
#' only for a factor with **3 levels or more** (the "OR of each level versus \code{ref2}"). The
#' first level is used by default. For a **binary** factor \code{ref2} is ignored: each level's
#' OR is computed against the *other* level, so both levels show a value (reciprocals of one
#' another) instead of one being forced to \code{1}. See `ref` above for the list of possible values.
#' @param comp The comparison level : by subtables/groups, or for the whole table.
#' \itemize{
#'   \item \code{"tab"}: by default, contributions to variance,
#' row differences from totals/first cells, and row confidence intervals for these
#' differences, are calculated for each \code{tab_vars} group.
#'   \item \code{"all"}: compare cells to the general total line (provided there is
#'    a total table with a total row), or with the first line of the total table
#'    when \code{ref = "first"}.
#' }
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios:
#'   for a binary variable the usual odds ratio; for a variable with 3 levels or more,
#'   the odds ratio of each level versus the reference level (the empirical analogue of
#'   the "OR (j vs reference)" from a multinomial [tab_reg()] model).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' Odds ratios don't add up to 100\%, so the total column drops its "100\%" and shows only the base
#' \code{n} (console), exports the base-\code{n} column only, or nothing when \code{add_n = FALSE}.
#' @param test Set to \code{TRUE} to calculate a statistical test of independence for each
#' (sub)table: \strong{Chi-squared} for factor \code{col_vars}, \strong{Welch's F} (one-way
#' ANOVA) for numeric ones -- see \code{\link{tab_chi2}}. Useful to print metadata, and to
#' color cells based on their contribution to variance (\code{color = "contrib"}).
#' Automatically added if needed for \code{color}.
#' @param chi2 `r lifecycle::badge("deprecated")` Renamed to \code{test} in 1.4.0: the test is a
#' Chi-squared only for factors (numeric \code{col_vars} get Welch's F), so the old name was
#' misleading. Still works.
#' @param ci The type of confidence intervals to calculate, passed to \code{\link{tab_ci}}
#'  (automatically added if needed for \code{color}).
#'   \itemize{
#'    \item \code{"cell"}: absolute confidence intervals of cells percentages.
#'    \item \code{"diff"}: confidence intervals of the difference between a cell and the
#'    relative total cell (or relative first cell when \code{ref = "first"}).
#'    \item \code{"auto"}: \code{ci = "diff"} for means and row/col percentages,
#'      \code{ci = "cell"} for frequencies ("all", "all_tabs").
#'   }
#'  By default, for percentages, \code{ci = "cell"} uses the Wilson score interval and
#'  \code{ci = "diff"} the Newcombe hybrid-score interval (its dual, so the bracket and the
#'  significance stars always agree); means use the Welch t interval. The method can be changed
#'  in \code{\link{tab_many}} (\code{method_cell} / \code{method_diff} / \code{method_mean_diff}). By
#'  default, with \code{ci = "cell"}, the result is printed in the `[inf;sup]` form.
#'  Set `options("tabxplor.ci_print" = "moe")` to print `pct +- moe` instead.
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
#' @param stars Logical (default \code{FALSE} \emph{opt-in}). With \code{ci = "diff"}, print
#' significance stars for each cell's difference from its reference, read from the displayed interval
#' itself (universal CI-inclusion). \code{NULL} uses `options("tabxplor.stars")` (default
#' \code{FALSE}). See \code{\link{tab_many}}.
#' @param method_cell,method_diff Character strings choosing the confidence-interval method for
#' \code{ci = "cell"} (\code{"wilson"} default, or \code{"wald"}) / \code{ci = "diff"}
#' (\code{"newcombe"} default, \code{"ac"} or \code{"wald"}). See \code{\link{tab_many}}.
#' @param method_ratio,method_mean_diff,method_mean_ratio Character strings choosing the
#' confidence-interval method for the ratio / numeric-mean intervals. \code{method_ratio} (proportion
#' ratio, \code{color = "ratio"}): \code{"katz"} (log risk-ratio, the only value). \code{method_mean_diff}
#' (numeric mean difference): \code{"welch"} (default, each group's own variance) or \code{"student"}
#' (pooled variance = a linear-regression coefficient interval). \code{method_mean_ratio} (numeric mean
#' ratio, \code{color = "ratio"} on a mean): \code{"robust"} (default, each group's own variance =
#' modified/robust Poisson), \code{"quasipoisson"} (dispersion-scaled = a quasi-Poisson regression) or
#' \code{"poisson"} (naive). See \code{\link{tab_many}}.
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
#'   \item \code{"OR"}: empirical odds ratio (for \code{pct = "row"}/\code{"col"}), coloured on its
#'   own symmetric \code{odds_ratio} scale (so \code{pct_ratio} stays free for \code{"ratio"}).
#'  }
#' The grammar: \strong{position picks the channel} (1st value -> text, 2nd -> background) and
#' \strong{names pick the column type} (\code{pct} / \code{mean}). So \code{c("diff", "ratio")}
#' puts \code{diff} on the text and \code{ratio} on the background of every column;
#' \code{c(pct = "diff", mean = "ratio")} colors factors by \code{diff} and numeric means by
#' \code{ratio} (text channel); \code{list(pct = c("diff", "ratio"), mean = "ratio")} combines
#' both (per-type, with channels). Only \code{diff} / \code{ratio} may go on the background.
#' Thresholds come from \code{\link{set_color_breaks}} or the per-table \code{color_breaks}
#' argument. (The old combined strings \code{"diff_ci"}, \code{"after_ci"} and \code{"ci"} still
#' work but are soft-deprecated in favor of \code{color_signif}.)
#' @param color_signif How significance gates the color, as a single string:
#'  \itemize{
#'   \item \code{"ignore"} (default): color every deviation by its observed size.
#'   \item \code{"grey_non_signif"}: color by the observed size, but grey out cells whose
#'   deviation is not significant at \code{conf_level}. A cell is coloured only when it is BOTH
#'   significant AND at least as large as the first colour threshold, so an un-coloured (grey) cell
#'   may still be significant -- just too small to colour (and it can carry significance stars). The
#'   only guarantee is: a coloured cell is significantly different from its reference.
#'   \item \code{"guaranteed_effect"}: color by the guaranteed (confidence-bound) effect --
#'   only cells whose interval clears the threshold, with dimmer, conservative colors.
#'  }
#' @param color_breaks A per-table override of the colour thresholds, a named list of scales like
#' \code{\link{set_color_breaks}} accepts, e.g. \code{list(pct_ratio = list(over = 2))}. Stored as
#' a table attribute and applied at print / export; \code{NULL} (default) uses the global breaks.
#' Unset scales fall back to the global setting.
#' @param add_n For `pct = "row"` or `pct = "col"`, set to `FALSE` not to add another
#' column or row with unweighted counts (`n`).
#' @param add_pct Set to `TRUE` to add a column with the frequencies of the row
#' variable (for `pct = "row"`) or a row with the frequencies of the column variable
#' (for  `pct = "col"`).
#' @param subtext A character vector to print rows of legend under the table.
#' @param output_list Logical (default \code{FALSE}). With several \code{row_var}, \code{FALSE}
#'  merges the mirror tables into a single \code{tabxplor_tab}; \code{TRUE} returns a list with
#'  one table per \code{row_var}. With \code{tab_vars}, tables stay a list regardless.
#' @param parallel Opt-in parallel build of the per-\code{row_var} tables, using the (Suggests-only)
#'  \pkg{mirai} package. \code{NULL} (default) reads \code{getOption("tabxplor.parallel")} (off);
#'  \code{FALSE} forces serial; \code{TRUE} uses an auto worker count; an integer sets the number of
#'  worker processes. Byte-identical to the serial result. It pays off for the survey workflow --
#'  \emph{many} \code{row_vars} against a small/medium data frame (roughly 10k-60k rows) in ONE
#'  \code{tab()} call -- and is a loss for few tables or multi-million-row data (so it stays opt-in).
#'  The worker pool persists for the session; release it with \code{\link{tab_parallel_stop}}.
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
#' @param .cache,.defer_level_merge,.return_armed,.levels_order Internal, for the jamovi
#' \code{jmvtab} live cache only: \code{.cache} is a mutable environment the content-addressed
#' multi-tier store is threaded through (Phase 7e); \code{.defer_level_merge} keeps full factor
#' levels through the aggregate and test so \code{levels} becomes a display-time drop;
#' \code{.return_armed} (Phase 7f) returns the pre-\code{finalize_color_spec} table so the tier-3
#' cache can re-paint colours without a rebuild; \code{.levels_order} (Phase 7g-ii) is a named list
#' of factor level orders applied post-aggregate, backing the jamovi level-reordering control (in R,
#' relevel with \code{\link[forcats:fct_relevel]{forcats::fct_relevel}} before calling \code{tab()}).
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
#' data <- forcats::gss_cat |>
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
#' data2 <- data |> dplyr::mutate(year = as.factor(year))
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
#' tab(dplyr::storms, category, c(status, pressure, wind)) |>
#'   dplyr::filter(category != "-1") |>
#'   dplyr::select(-`tropical depression`) |>
#'   dplyr::arrange(is_totrow(.), desc(category))
#'}
#'
#'\donttest{
#' # With `dplyr::arrange`, don't forget to keep the order of tab variables and total rows:
#' tab(data, race, marital, year, pct = "row") |>
#'   dplyr::arrange(year, is_totrow(.), desc(Married))
#'   }
#'
#' @seealso
#'   [tab_many()] (the full-featured engine behind `tab()`) and [tab_reg()] (regression tables).
#'   Go further with the helper functions: [tab_ci()] (confidence intervals and their methods),
#'   [set_color_breaks()] / [set_color_palette()] / [set_color_style()] (colors),
#'   [tab_chi2()] (statistical tests), [tab_pct()] / [tab_tot()] (percentages and totals).
#'   Export a table with [tab_xl()] (Excel), [tab_kable()] (HTML), [tab_md()] (Markdown) or
#'   [tab_plot()]. Package-wide defaults live in [tabxplor-options].
tab <- function(data, row_vars, col_vars, tab_vars, wt, sup_cols,
                pct = "no", color = "no", color_signif = "ignore",
                OR = "no", test = FALSE,
                na = "keep", levels = "all",
                cleannames = NULL, #compact = NULL, # pvalue_line = NULL,
                other_if_less_than = 0, other_level = "Others",
                ref = "auto", ref2 = "first", comp = "tab",
                ci = "no", conf_level = getOption("tabxplor.conf_level", 0.95), stars = NULL,
                method_cell = "wilson", method_diff = "newcombe",
                method_ratio = "katz", method_mean_diff = "welch",
                method_mean_ratio = "robust",
                totaltab = "line", totaltab_name = "Ensemble",
                tot = c("row", "col"), total_names = "Total",
                add_n = TRUE, add_pct = FALSE,
                subtext = "", digits = 0, n_min = 0, display = NULL,
                color_breaks = NULL,
                output_list = FALSE, parallel = NULL,
                spread_vars, names_prefix = NULL, names_sort = FALSE,
                row_var, col_var,
                chi2 = lifecycle::deprecated(),
                .cache = NULL, .defer_level_merge = FALSE, .return_armed = FALSE,
                .levels_order = NULL,
                filter) {

  # Phase 14a: `chi2` renamed `test` -- for a numeric col_var the whole-table test is Welch's F, not
  # a chi2 (Phase 3b), so the old name named only half of what it does.
  if (lifecycle::is_present(chi2)) {
    lifecycle::deprecate_soft("1.4.0", "tab(chi2 = )", "tab(test = )")
    test <- chi2
  }

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


  # `row_vars`/`col_vars` accept a <tidy-select> (one variable OR several, e.g. `c(race, relig)`),
  # so tab() can build several mirror tables and merge them by default (§13). row_var_quo /
  # col_var_quo were resolved above (plural name, or the deprecated singular alias).
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data |> dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- "no_row_var"
  } else {
    row_var <- names(tidyselect::eval_select(row_var_quo, data))
  }

  if (quo_miss_na_null_empty_no(col_var_quo)) {
    data <- data |> dplyr::mutate(no_col_var = factor("n"))
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
  vctrs::vec_assert(test, size = 1)

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
           # tab_build()'s internal arg keeps the `chi2` name (it drives tab_chi2(); the ANOVA arm
           # branches inside tab_transform()); only the PUBLIC tab() surface is renamed.
           chi2 = test,
           ci = ci,
           conf_level = conf_level,
           stars = stars,
           method_cell = method_cell, method_diff = method_diff,
           method_ratio = method_ratio, method_mean_diff = method_mean_diff,
           method_mean_ratio = method_mean_ratio,
           OR = OR,
           color = color,
           # Phase 14a: the NORMALIZED policy (post the "color_all_signif" COMPAT rename), so
           # tab_resolve_settings() can force the difference CI a gated colour needs.
           color_signif = color_spec$signif,
           # Phase 14b: same reason -- the two-channel spec, not the legacy string, knows whether the
           # ratio is the measure the reader sees (and so owns the stored interval).
           color_ratio_ci = color_pct_text_is_ratio(color_spec),
           add_n = add_n, add_pct = add_pct,
           subtext = subtext, n_min = n_min, parallel = parallel,
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
  result <- finalize_color_spec(result, color_spec)

  # Phase 10i-A: opt-in composite display recipe (e.g. `display = "pct (n)"` or `"{pct} (n={n})"`)
  # written into the display FIELD of value cells; NULL (default) leaves the plain single-field display.
  result <- tab_apply_display(result, display)

  # Phase 13a: a per-table color_breaks override, stored as a table attribute (set LAST so no earlier
  # step strips it; installed transiently at render). NULL (default) -> the global breaks apply.
  result <- set_color_breaks_attr(result, resolve_color_breaks_arg(color_breaks))

  # Phase 13c-iv: a multi-table result becomes a tabxplor_tabs (still a list) so it auto-prints like a
  # single tab and routes to the Viewer under options("tabxplor.print" = "kable"). No-op on a single tab.
  as_tabxplor_tabs(result)
}


# Phase 10i-A: apply an opt-in COMPOSITE display recipe (curated sugar "pct (n)"/"n (pct)"/"pct_n",
# or a raw "{pct} (n={n})" template) to a built table (single tab, grouped tab, or a list of tabs).
# It is a DISPLAY overlay only (text backends via format()); get_num(), coloring and the Excel bypass
# keep showing the PRIMARY field. validate_display_template() checks the {} template (aborts on
# bad input); the {} template is written into the `display` FIELD but ONLY on genuine value cells, so
# the already-present p-value / blank / total-marker cells keep their own token (this write runs last
# in tab(), after those rows exist).
#' @keywords internal
tab_apply_display <- function(tabs, display) {
  if (is.null(display) || length(display) == 0L) return(tabs)
  ds <- display[[1]]
  if (is.na(ds) || ds %in% c("", "no")) return(tabs)
  tmpl   <- validate_display_template(ds)
  fields <- parse_display_template(tmpl)$fields
  set_one <- function(tab) {
    dplyr::mutate(tab, dplyr::across(dplyr::where(is_fmt), function(col) {
      d <- get_display(col)
      # Only genuine value cells, AND only where EVERY template field renders (non-NA) -- so a
      # count-only column (pct NA, e.g. the added-n column) or an empty cell keeps its own token
      # and renders normally (byte-identical to the Phase-10c `both` guard).
      elig <- d %in% c("pct", "mean", "n", "wn")
      for (f in fields) elig <- elig & !is.na(get_num(set_display(col, f)))
      d[elig] <- tmpl
      set_display(col, d)
    }))
  }
  if (is.data.frame(tabs)) set_one(tabs) else purrr::map(tabs, set_one)
}


# Phase 13a: `color` grammar -- POSITION = channel (1st -> text, 2nd -> background), NAMES = column
# type (pct / mean). FALSE -> off; TRUE -> the smart per-type default; a scalar/positional vector ->
# the same measure(s) on every column; a NAMED vector or a list(pct =, mean =) -> per column type
# (each entry a positional channel vector). Returns list(mode, legacy, text, bg, types, signif):
# `legacy` is the scalar string fed to the (text-channel) pipeline so its ci/chi2 side effects still
# fire; `mode`/`text`/`bg`/`types`/`signif` drive finalize_color_spec() on the built table.
#' @keywords internal
normalize_color_spec <- function(color, color_signif = "ignore", deprecate = TRUE) {
  signif <- if (length(color_signif) == 0L) "ignore" else color_signif[1]
  if (is.na(signif) || signif %in% c("", "no")) signif <- "ignore"
  # COMPAT (Phase 13a): the renamed policy value, wired through with a soft-deprecation.
  if (identical(signif, "color_all_signif")) {
    lifecycle::deprecate_soft("1.4.0", I('color_signif = "color_all_signif"'),
                              with = I('color_signif = "guaranteed_effect"'),
                              user_env = rlang::caller_env(2))
    signif <- "guaranteed_effect"
  }
  ok_signif <- c("ignore", "grey_non_signif", "guaranteed_effect")
  if (!signif %in% ok_signif) {
    cli::cli_abort(c("Unknown {.arg color_signif} value {.val {signif}}.",
                     "i" = "Valid: {.val {ok_signif}}."))
  }
  # normalize_color_spec() is called by tab()/tab_num(), so the real user is two frames up; this keeps
  # the deprecation nudge for user calls but silent for tab_many()'s internal recursion.
  uenv       <- rlang::caller_env(2)
  norm       <- function(m) if (is.na(m) || identical(m, "no")) "" else if (identical(m, "or")) "OR" else m
  ok_measure <- c("diff", "ratio", "contrib", "OR", "auto", "diff_ci", "after_ci", "ci", "")

  # WARNING: `deprecate = FALSE` is not a convenience -- it is required on the internal seam.
  # legacy_union() MANUFACTURES "diff_ci"/"after_ci" (e.g. color = "ratio" + color_signif =
  # "grey_non_signif" -> "diff_ci") and tab_transform() hands that string to tab_num(), which
  # re-parses it here. Deprecating then blames the user for a string the pipeline wrote. The `uenv`
  # heuristic above is not enough: lifecycle's from_testthat() deliberately forces a package's own
  # internal soft-deprecations to warn while its suite runs, so the false positive surfaces in
  # tabxplor's tests -- and in the tests of any package that calls tab() on a numeric column.
  deprecate_old <- function(text) {
    if (!deprecate) return(invisible(NULL))
    if (text %in% c("diff_ci", "after_ci", "ci")) {
      lifecycle::deprecate_soft(
        "1.4.0",
        I(paste0("The `color = \"", text, "\"` mode")),
        with = I("`color = \"diff\"` with the `color_signif` argument"),
        user_env = uenv)
    }
  }

  # one positional channel vector (text[, background]) -> c(text, bg-or-NA) validated measures
  parse_channels <- function(v) {
    v    <- unname(as.character(v))
    text <- norm(v[1])
    bg   <- if (length(v) >= 2L) norm(v[2]) else NA_character_
    if (!is.na(bg) && bg == "") bg <- NA_character_
    if (!text %in% ok_measure) cli::cli_abort("Unknown color measure {.val {text}}.")
    if (!is.na(bg) && !bg %in% c("diff", "ratio")) {
      cli::cli_abort("{.val {bg}} cannot go on the background channel (only {.val diff} / {.val ratio}).")
    }
    deprecate_old(text)
    c(text, if (is.na(bg)) NA_character_ else bg)
  }

  # the pipeline legacy string demanded by a set of measures (ci/chi2/OR side-effects)
  legacy_union <- function(ms) {
    ms <- ms[!is.na(ms) & ms != ""]
    if ("auto"    %in% ms) return("auto")     # resolved per column type downstream (tab_resolve_settings)
    if ("contrib" %in% ms) return("contrib")
    if ("OR"      %in% ms) return("OR")
    old <- ms[ms %in% c("diff_ci", "after_ci", "ci")]
    if (length(old)) return(old[1])
    if (any(ms %in% c("diff", "ratio"))) {
      return(switch(signif, "grey_non_signif" = "diff_ci", "guaranteed_effect" = "after_ci", "diff"))
    }
    "no"
  }

  # ---- FALSE / TRUE ----
  if (is.logical(color)) {
    if (isTRUE(color)) {
      return(list(mode = "auto", legacy = "auto", text = "auto", bg = NA_character_,
                  types = NULL, signif = signif))
    }
    return(list(mode = "off", legacy = "no", text = "", bg = NA_character_,
                types = NULL, signif = "ignore"))
  }

  # COMPAT (Phase 13a): the former channel-name form c(text =, background =). Names are now COLUMN
  # TYPES, so remap text/background -> the positional channel form, with a soft-deprecation.
  cnms <- names(color)
  if (!is.null(cnms) && length(setdiff(cnms[nzchar(cnms)], c("text", "background", "bg"))) == 0L &&
      any(cnms %in% c("text", "background", "bg"))) {
    lifecycle::deprecate_soft("1.4.0", I('color = c(text = , background = )'),
                              with = I('a positional color = c("diff", "ratio")'),
                              user_env = rlang::caller_env(2))
    cc   <- as.character(color)
    tval <- if ("text" %in% cnms) cc[cnms == "text"][1] else ""
    bval <- if ("background" %in% cnms) cc[cnms == "background"][1]
            else if ("bg" %in% cnms) cc[cnms == "bg"][1] else NA_character_
    color <- if (is.na(bval)) tval else c(tval, bval)   # -> positional; falls through to the flat path
  }

  # ---- list(pct =, mean =) or a NAMED vector : per column TYPE ----
  is_typed <- (is.list(color) && !is.null(names(color)) && all(nzchar(names(color)))) ||
    (!is.null(names(color)) && any(nzchar(names(color))))
  if (is_typed) {
    nms <- names(color)
    if (is.null(nms) || !all(nzchar(nms)) || !all(nms %in% c("pct", "mean"))) {
      cli::cli_abort(c("A per-type {.arg color} must be named by column type ({.field pct} / {.field mean}).",
                       "i" = 'e.g. {.code list(pct = c("diff", "ratio"), mean = "ratio")}.',
                       "i" = "For two channels on every column use positions: {.code c(\"diff\", \"ratio\")}."))
    }
    entries <- if (is.list(color)) color else as.list(color)
    types   <- purrr::map(entries, parse_channels)
    legacy  <- legacy_union(unlist(types, use.names = FALSE))
    return(list(mode = "by_type", legacy = legacy, text = NA_character_, bg = NA_character_,
                types = types, signif = signif))
  }

  # ---- unnamed scalar / positional vector : the SAME measure(s) on every column ----
  ch     <- parse_channels(color)
  text   <- ch[1]; bg <- ch[2]
  legacy <- if (text %in% c("", "no") && !is.na(bg)) "diff" else legacy_union(ch)
  list(mode = "flat", legacy = legacy, text = text, bg = bg, types = NULL, signif = signif)
}

# Apply the color spec to a built table (or list), rewriting the color / color_signif attributes to
# the clean (measure, policy) model ONLY when a new capability is used (color = TRUE, a per-type spec,
# a background channel, an explicit color_signif, or the `ratio` measure). Plain old scalar colors
# pass through untouched (no golden churn; the engine decodes them).
#' @keywords internal
finalize_color_spec <- function(x, spec) {
  if (is.list(x) && !is.data.frame(x)) return(purrr::map(x, ~ finalize_color_spec(., spec)))
  rewrite <- spec$mode %in% c("auto", "by_type") || !is.na(spec$bg) ||
    spec$signif != "ignore" || identical(spec$text, "ratio")
  if (!rewrite) return(x)
  dplyr::mutate(x, dplyr::across(dplyr::where(is_fmt), ~ finalize_one_col(.x, spec)))
}

# Phase 14b / 14v-ii: does the EXPLICIT text channel carry the ratio measure? That is the trigger for
# the ratio CI: the measure the reader sees owns the stored interval, and any second channel derives
# from it (fmt_color_plan()'s rescale_bound). Decided here, on the spec, because the `legacy` string
# tab_resolve_settings() runs on cannot express it -- legacy_union() maps every ratio onto a diff-family
# string, which is exactly why the policy had to be threaded separately in 14a.
#
# 14v-ii: both proportions (-> Katz log-RR) AND numeric means (-> ci_mean_ratio, the ratio-of-means CI)
# are covered now; the Fieller-scope limit is lifted. Fires ONLY on an EXPLICIT ratio channel (flat
# `color = "ratio"` or a by_type `pct = "ratio"` / `mean = "ratio"`); `color = TRUE` is "auto" mode ->
# NA -> FALSE, so a mean's auto text channel (which IS the ratio) keeps its difference CI, unchanged.
#' @keywords internal
color_pct_text_is_ratio <- function(spec) {
  if (is.null(spec) || is.null(spec$mode)) return(FALSE)
  m <- switch(spec$mode,
              "flat"    = spec$text,
              "by_type" = c(spec$types[["pct"]][1], spec$types[["mean"]][1]),
              NA_character_)   # "auto" -> a column's text channel is resolved later; "off" -> no colour
  "ratio" %in% unname(m)
}

# The per-column measure vector (text[, background]) the spec assigns to a column, given its built
# color + type. NULL = leave the column as the pipeline built it (e.g. contrib/OR under a pct/mean
# spec, or a type the spec does not mention).
#' @keywords internal
resolve_col_measures <- function(spec, type, built) {
  if (spec$mode == "auto") {                                # color = TRUE smart per-type default
    if (built == "OR")      return("OR")
    if (built == "contrib") return(NULL)                    # counts / all -> keep contrib
    if (type %in% c("mean", "n", "coef"))             return("ratio")          # numeric -> ratio text
    if (type %in% c("row", "col", "all", "all_tabs")) return(c("diff", "ratio"))  # factor -> diff + ratio bg
    return(NULL)
  }
  if (spec$mode == "by_type") {
    if (built %in% c("OR", "contrib")) return(NULL)         # not keyable by pct/mean -> keep built
    key <- if (type %in% c("mean", "n", "coef")) "mean"
           else if (type %in% c("row", "col", "all", "all_tabs")) "pct" else NA_character_
    if (is.na(key) || is.null(spec$types[[key]])) return(NULL)
    m <- spec$types[[key]]
    return(if (is.na(m[2])) m[1] else m)
  }
  # flat
  text <- if (identical(spec$text, "auto")) color_measure_policy(built, type)$measure else spec$text
  if (text == "" && is.na(spec$bg)) return(NULL)
  if (is.na(spec$bg)) text else c(text, spec$bg)
}

#' @keywords internal
finalize_one_col <- function(col, spec) {
  built <- get_color(col)
  type  <- get_type(col)
  if (built %in% c("", "no")) return(col)                  # the pipeline did not color this column
  measures <- resolve_col_measures(spec, type, built)
  if (is.null(measures)) return(col)
  if (length(measures) == 1L && measures %in% c("", "no")) return(col)
  set_color_signif(set_color(col, measures), spec$signif)
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
#' @param ref2 The second reference level for odds ratios (or relative risk ratios), needed
#' only for a factor with **3 levels or more** (the "OR of each level versus \code{ref2}"). The
#' first level is used by default. For a **binary** factor \code{ref2} is ignored: each level's
#' OR is computed against the *other* level, so both levels show a value (reciprocals of one
#' another) instead of one being forced to \code{1}. See `ref` above for the list of possible values.
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
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios:
#'   for a binary variable the usual odds ratio; for a variable with 3 levels or more,
#'   the odds ratio of each level versus the reference level (the empirical analogue of
#'   the "OR (j vs reference)" from a multinomial [tab_reg()] model).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' Odds ratios don't add up to 100\%, so the total column drops its "100\%" and shows only the base
#' \code{n} (console), exports the base-\code{n} column only, or nothing when \code{add_n = FALSE}.
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
#' @param stars Logical. When \code{TRUE} (opt-in; default \code{FALSE}) and \code{ci = "diff"}, each
#' cell shows significance stars for the difference from its reference (\code{*} p<0.10, \code{**} p<0.05,
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
#' @param method_ratio Character string, the proportion \emph{ratio} interval (\code{color = "ratio"}):
#' \code{"katz"} (the log risk-ratio interval, the only value for now).
#' @param method_mean_diff Character string, the numeric mean-\emph{difference} interval: \code{"welch"}
#' (default, each group's own variance, Welch--Satterthwaite df) or \code{"student"} (pooled variance,
#' df \eqn{= n_1 + n_2 - 2}, reproducing a linear-regression coefficient interval).
#' @param method_mean_ratio Character string, the numeric mean-\emph{ratio} interval (\code{color =
#' "ratio"} on a mean): \code{"robust"} (default, each group's own variance = modified/robust Poisson),
#' \code{"quasipoisson"} (Poisson SE scaled by the dispersion, reproducing a quasi-Poisson regression)
#' or \code{"poisson"} (naive Var = mean). All are the log-scale Wald/t interval, exp-back. As for the
#' other methods, the significance stars come from the same interval so bracket and stars always agree.
#' @param color Which measure(s) to color, on which visual channel -- see \code{\link{tab}}
#' for the full grammar (\code{FALSE}/\code{TRUE}, a measure such as \code{"diff"}, a positional
#' two-channel \code{c("diff", "ratio")}, or a per-type \code{c(pct = , mean = )} /
#' \code{list(pct = , mean = )}). The old combined strings
#' \code{"diff_ci"}/\code{"after_ci"}/\code{"ci"} still work (superseded by \code{color} +
#' \code{color_signif}). Applies to all \code{row_vars}.
#' @param color_signif How significance gates the color -- see \code{\link{tab}}
#' (\code{"ignore"} / \code{"grey_non_signif"} / \code{"guaranteed_effect"}).
#' @param color_breaks A per-table colour-threshold override -- see \code{\link{tab}}.
#' @param parallel Opt-in parallel build of the per-\code{row_var} tables (Suggests-only
#' \pkg{mirai}); see \code{\link{tab}}. \code{NULL} (default) reads the
#' \code{tabxplor.parallel} option.
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
#' data <- forcats::gss_cat |> mutate(across(
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
                     ci = "no", conf_level = getOption("tabxplor.conf_level", 0.95), stars = NULL, #ci_visible = FALSE,
                     method_cell = "wilson", method_diff = "newcombe",
                     method_ratio = "katz", method_mean_diff = "welch",
                     method_mean_ratio = "robust",
                     totaltab = "line", totaltab_name = "Ensemble",
                     totrow = TRUE, totcol = "last", total_names = "Total",
                     add_n = TRUE, add_pct = FALSE,
                     digits = 0, subtext = "", n_min = 0, color_signif = "ignore",
                     color_breaks = NULL,
                     parallel = NULL,
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
    pct = pct, color = color_spec$legacy, color_signif = color_spec$signif,
    OR = OR, chi2 = chi2, na = na, levels = levels,
    na_drop_all = {{ na_drop_all }},
    cleannames = cleannames, other_if_less_than = other_if_less_than,
    other_level = other_level, ref = ref, ref2 = ref2, comp = comp, ci = ci,
    conf_level = conf_level, stars = stars, method_cell = method_cell,
    method_diff = method_diff, method_ratio = method_ratio,
    method_mean_diff = method_mean_diff, method_mean_ratio = method_mean_ratio,
    totaltab = totaltab, totaltab_name = totaltab_name,
    totrow = totrow, totcol = totcol, total_names = total_names,
    add_n = add_n, add_pct = add_pct, digits = digits, subtext = subtext, n_min = n_min,
    parallel = parallel,
    .by_table = .by_table,
    filter = if (missing(filter)) NULL else {{ filter }},
    output = if (isTRUE(compact)) "single" else "legacy"
  )
  result <- finalize_color_spec(result, color_spec)
  result <- set_color_breaks_attr(result, resolve_color_breaks_arg(color_breaks))
  # Phase 13c-iv: wrap the multi-table list (tab_many keeps its list-default) so it auto-prints.
  as_tabxplor_tabs(result)
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
                      pct = "no", color = "no", color_signif = "ignore",
                      color_ratio_ci = FALSE,
                      OR = "no", chi2 = FALSE,
                      na = "keep", levels = "all", na_drop_all,
                      cleannames = NULL, output = "single", #pvalue_line = NULL,
                      other_if_less_than = 0, other_level = "Others",
                      ref = "auto", ref2 = "first", comp = "tab",
                      ci = "no", conf_level = 0.95, stars = NULL, #ci_visible = FALSE,
                      method_cell = "wilson", method_diff = "newcombe",
                      method_ratio = "katz", method_mean_diff = "welch",
                      method_mean_ratio = "robust",
                      totaltab = "line", totaltab_name = "Ensemble",
                      totrow = TRUE, totcol = "last", total_names = "Total",
                      add_n = TRUE, add_pct = FALSE,
                      digits = 0, subtext = "", n_min = 0,
                      parallel = NULL,
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
    if (is.character(rlang::get_expr(filter))) filter <- filter |>
        rlang::get_expr() |> str2lang()

    data <- data |> dplyr::mutate(.filter = !!filter)
    with_filter <- TRUE
  }

  ctx <- list(
    data = data, with_filter = with_filter,
    row_vars_quo = rlang::enquo(row_vars), col_vars_quo = rlang::enquo(col_vars),
    tab_vars_quo = rlang::enquo(tab_vars), wt_quo = rlang::enquo(wt),
    na_drop_all_quo = rlang::enquo(na_drop_all),
    pct = pct, color = color, color_signif = color_signif,
    color_ratio_ci = color_ratio_ci, OR = OR, chi2 = chi2,
    na = na, levels = levels,
    cleannames = cleannames, output = output,
    other_if_less_than = other_if_less_than, other_level = other_level,
    ref = ref, ref2 = ref2, comp = comp, ci = ci, conf_level = conf_level, stars = stars,
    method_cell = method_cell, method_diff = method_diff, method_ratio = method_ratio,
    method_mean_diff = method_mean_diff, method_mean_ratio = method_mean_ratio,
    totaltab = totaltab, totaltab_name = totaltab_name, totrow = totrow, totcol = totcol,
    total_names = total_names, add_n = add_n, add_pct = add_pct, digits = digits,
    subtext = subtext, n_min = n_min, by_table = .by_table,
    # Phase 8: `parallel` gates the tab_pmap() dispatch in tab_transform() (NULL -> option default
    # -> serial). Read only there; every other stage ignores it. See R/tab-parallel.R.
    parallel = parallel,
    spread_vars = spread_vars, names_prefix = names_prefix, names_sort = names_sort,
    # Phase 7e jmvtab cache seam: `cache_env` is a mutable environment holding $store / $hits (NULL
    # for tab()/tab_many() -> the hooks below are inert). `defer_level_merge` keeps full levels for
    # a cacheable aggregate + test (see tab_prepare_pop / the design doc). Both are strictly additive.
    cache_env = .cache, defer_level_merge = .defer_level_merge,
    # Phase 7g-ii: jmvtab-only per-variable level reordering (named list var -> ordered levels).
    # NULL for tab()/tab_many() -> no-op. Applied post-aggregate (tier-3) in jmv_cache_aggregate().
    levels_order = .levels_order
  )

  ctx <- tab_setup(ctx)          # resolve per-row_var + per-col_var arg vectors + colour cascade + keys
  ctx <- tab_prepare_pop(ctx)    # prepare the whole DB once (na/lump/levels; the global drop_all drop)
  ctx <- tab_aggregate(ctx)      # tier-1 aggregates (fine_num per-rv + shared fine_fused); jmvtab hook
  tab_build_tables(ctx)          # the OUTER map over row_vars + the cross-row_var output shape
}


# tab_build_tables() -- the row axis as ONE outer map + the output shape (Phase 9a). Shared by
# tab_build() (microdata) and tab_counts() (from-the-middle): both reach here with the aggregates in
# ctx (fine_num per-row_var + fine_fused shared/per-pair). It resolves a per-row_var ctx list
# (tab_rowvar_ctxs), maps the whole-per-row_var worker tab_build_one() over it -- serial (purrr::map)
# OR a mirai daemon pool, the SINGLE dispatch (R/tab-parallel.R) -- then runs the cross-row_var output
# shape once on main. Byte-identical to the pre-9a serial build (which already produced a per-row_var
# list of finished tabs and merged them); the parallel path is byte-identical by the Phase 8 parity net.
# jmvtab (cache_env) forces serial and keeps its tier-2 store hook.
#' @keywords internal
#' @noRd
tab_build_tables <- function(ctx) {
  workers <- tab_parallel_workers(ctx$parallel, ctx$cache_env)
  units   <- tab_rowvar_ctxs(ctx)
  built   <- tab_pmap(list(ctx_i = units), "tab_build_one",
                      .ship = list(data = ctx$data, fine_fused = ctx$fine_fused),
                      workers = workers)
  rv_names <- as.character(ctx$row_vars)
  # Name by row_var: tab_assemble_output()'s merge derives the merged `row_var` factor labels from
  # names(tabs), and jmv_cache_store_tests keys `tests` by row_var name.
  tabs  <- purrr::set_names(purrr::map(built, "tab"),  rv_names)
  tests <- purrr::set_names(purrr::map(built, "test"), rv_names)
  ctx   <- ctx_update(ctx, list(tabs = tabs, tests = tests))

  # Phase 7e: persist freshly-computed tier-2 tests (jmvtab cache misses) before display assembly.
  if (!is.null(ctx$cache_env)) jmv_cache_store_tests(ctx)

  tab_assemble_output(ctx)
}


# tab_rowvar_ctxs() -- split the post-aggregate ctx into one lean ctx per row_var, ready to map/ship
# (Phase 9a; replaces ctx_slice() + the tabxplor_rowvar_fields constant). Every field that VARIES per
# row_var is listed ONCE here (the single source of truth); each unit takes its element (`[[i]]`, so a
# per-row_var LIST field like fine_num yields its element, an atomic vector its scalar). A field that
# stays scalar under na = "drop_all" (na_text / na_num collapse to a single "keep") is left as-is.
# Everything else -- per-col_var (pct_vect is nested, but the per-row_var slice is a col_var vector),
# scalar, or the shared jmvtab cached_tests list (kept whole; the transform picks its row_var entry) --
# rides in the shared skeleton. `data` / `fine_fused` are dropped (shipped once by tab_pmap); the heavy
# NSE quosures are dropped (they would drag user data into every mirai task).
#' @keywords internal
#' @noRd
tab_rowvar_ctxs <- function(ctx) {
  n <- length(ctx$row_vars)
  per_rv <- c("row_vars", "color", "OR", "chi2", "ref", "ref2", "comp", "ci", "ci_scale",
              "totaltab", "totrow", "color_diff_OR", "color_ctr", "color_ci", "color_num",
              "ref_vect", "pct_vect", "na_text", "na_num", "fine_num")
  shared <- ctx[setdiff(names(ctx), c(per_rv, "data", "fine_fused"))]
  shared <- shared[!grepl("_quo$", names(shared))]
  shared$parallel  <- FALSE     # the worker never spawns nested daemons
  shared$cache_env <- NULL

  lapply(seq_len(n), function(i) {
    u <- lapply(per_rv, function(nm) {
      x <- ctx[[nm]]
      if (!is.null(x) && length(x) == n) x[[i]] else x
    })
    names(u) <- per_rv
    u$row_vars      <- ctx$row_vars[i]                              # keep as a length-1 sym list
    u$tab_row_names <- as.character(c(ctx$tab_vars, ctx$row_vars[i]))
    c(shared, u)
  })
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
  stars <- if (is.null(stars)) getOption("tabxplor.stars", FALSE) else stars


  stopifnot(levels %in% c("first", "all", "auto"))
  lvs <- levels

  row_vars <- row_vars_quo
  if (quo_miss_na_null_empty_no(row_vars)) {
    data     <- data |> dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
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
    data     <- data |> dplyr::mutate(no_col_var = factor("n"))
    col_vars <- rlang::syms("no_col_var")
    pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else {
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
  }
  # DESIGN: extract by POSITION with `[[` (not `data[pos_col_vars]`): `df[<int vector>]` is column-
  # subsetting on a data.frame/tibble but ROW-subsetting on a data.table, which silently mis-classified
  # col_vars (-> NA col_var -> tab_num eval_select crash) on a data.table input. `data[[<int>]]` is
  # engine-agnostic.
  col_vars_num  <- purrr::map_lgl(pos_col_vars, ~ is.numeric(data[[.x]]))
  col_vars_text <- purrr::map_lgl(pos_col_vars,
                                  ~ is.factor(data[[.x]]) || is.character(data[[.x]]))

  tab_vars <- tab_vars_quo
  if (quo_miss_na_null_empty_no(tab_vars)) {
    #data     <- data |> dplyr::mutate(no_tab_vars = factor(" "))
    tab_vars <- character() #rlang::syms("no_tab_vars")
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  # wt_quo arrives from ctx (defused in tab_build); resolve to a bare symbol or character().
  if (quo_miss_na_null_empty_no(wt_quo)) {
    #data <- data |> dplyr::mutate(no_weight = factor("n"))
    wt <- character() #rlang::sym("no_weight")
  } else {
    wt <- rlang::sym(rlang::as_name(wt_quo))
  }
  # Last Phase a bug-fix: a weight that is ALSO a selected variable is nonsensical (you cannot weight a
  # mean by the same column you are averaging, nor cross a variable by itself) and used to abort with a
  # cryptic data.table error. Fail early with a clear message. num_moment_scan is otherwise shadow-proof,
  # so an ORDINARY weight named "wt" is fine -- only this double-role collision is rejected.
  if (length(wt) != 0L &&
      as.character(wt) %in% c(as.character(row_vars), as.character(col_vars), as.character(tab_vars))) {
    cli::cli_abort(c(
      "The weight variable {.val {as.character(wt)}} is also used as a row, column or tab variable.",
      "i" = "A weight cannot be a table variable at the same time \u2014 pick a different weight column."
    ))
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
    totcol <- col_vars_text[col_vars_text] |> names() |> dplyr::last()
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

  # WARNING: `pct` is per-col_var (length ncolvars), `OR` is per-row_var (length nrowvars) -- a
  # vectorised `pct == "row" & OR %in% ...` recycles and warns when the counts don't divide (e.g. 3x4).
  # Two independent scalar reductions are byte-identical (all(A & B) == all(A) && all(B) for any lengths)
  # without the recycle. Twin of the Phase 9a fix at tab_assemble_tables (~L1859).
  if (all(pct == "row") && all(OR %in% c("OR", "or", "OR_pct", "or_pct"))) {
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
                                         color_signif = color_signif,
                                         color_ratio_ci = color_ratio_ci, stars = stars,
                                         na = na, wt_name = as.character(wt),
                                         other_if_less_than = other_if_less_than, comp = comp,
                                         tab_vars = as.character(tab_vars),
                                         row_vars = as.character(row_vars),
                                         col_vars = as.character(col_vars),
                                         filter_expr = NA_character_)
  color         <- .settings$color
  chi2          <- .settings$chi2
  ci            <- .settings$ci
  ci_scale      <- .settings$ci_scale     # Phase 14b: "diff" / "ratio" (the Katz interval)
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
    cleannames = cleannames, stars = stars, lvs = lvs, color_signif = color_signif,
    totaltab = totaltab, totrow = totrow, ref = ref, ref2 = ref2, ref_vect = ref_vect,
    OR = OR, comp = comp, color = color, ci = ci, ci_scale = ci_scale, chi2 = chi2,
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
  data <- data |> dplyr::select(!!!tab_vars, !!!row_vars, !!wt, !!!col_vars,
                                 tidyselect::any_of(".filter")) |>
    relabel_levels_in_varnames(as.character(col_vars))

  #  Filters : here after selection (operations on rows copy all columns on memory),
  #     orwhen the tables are made for more speed :
  # - na = "drop_all" removes NAs here in tab_prepare (slower), i.e. for all tables mades
  # - na = "drop" : NA in factors and numeric will be removed in each tab_plain/tab_num
  # - na = "keep" : NA in factors (not numeric) will be made explicit in each tab_plain/tab_num

  if (with_filter == TRUE) data <- data |> dplyr::filter(.data$.filter) |>
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

  data <- data |>
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
    data <- data |>
      dplyr::group_by(!!!tab_vars) |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(as.character(row_vars)),
                                  ~ forcats::fct_lump_min(., other_if_less_than,
                                                          other_level = other_level))) |>
      dplyr::ungroup() |>
      dplyr::mutate(dplyr::across(as.character(row_vars), function(.x) forcats::fct_relevel(
        .x,
        unique(append(levels(dplyr::pull(data, dplyr::cur_column())), other_level)) |>
          purrr::discard(\(v) !v %in% levels(.x)))
      ))
  }


  #Remove rows with missing values or 0 in weight, for them not to be added in raw counts
  # remove zero weight in tab_prepare ?
  if (length(wt) != 0) {
    zero_weight <- dplyr::pull(data, !!wt)
    zero_weight <- is.na(zero_weight) | zero_weight == 0
    if (any(zero_weight)) {
      rlang::inform(paste0(sum(zero_weight), " rows with zero or NA weights were removed"))
      data <- data |> dplyr::filter(!zero_weight)
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

  # Where only first levels are kept, merge the OTHER (non-first) levels to minimise useless
  # calculations. Phase 14x: the NA is NOT folded into a level here -- it stays NA, so the leaf's own
  # na handling remains authoritative (na = "keep" makes an explicit "NA" column counted in the base;
  # na = "drop" drops those rows from the base). The NA column is then discarded from DISPLAY for EVERY
  # arity by appending "NA" to remove_levels below (any_of ignores it when absent). The old pre-merge
  # folded NA into "remove_levels" for 3+-level factors only, which had two bugs: (a) it left the NA
  # column visible for 2-level factors under na = "keep" (no pre-merge fired, and "NA" was never added
  # to remove_levels), and (b) it defeated na = "drop" for 3+-level factors (NA became a real level, so
  # the leaf found nothing to drop and the base wrongly included the NA rows). This now matches the
  # jmvtab defer path exactly.
  # Phase 7e: skip the PRE-aggregate merge when defer_level_merge (jmvtab) -- keep full levels so the
  # aggregate + test are cacheable; the drop happens in tab_assemble.
  lv1 <- lvs == "first" & col_vars_text
  if (any(lv1)) {
    if (!isTRUE(defer_level_merge)) {
      col_vars_3levels <-
        purrr::map_lgl(dplyr::select(data, !!!col_vars),
                       ~ is.factor(.) & nlevels(.) >= 3) & lv1

      if (any(col_vars_3levels)) {

        rm_levels_by_col_vars <- dplyr::select(data, !!!col_vars[col_vars_3levels]) |>
          purrr::map(~ purrr::set_names(levels(.)[-1], "remove_levels"))

        data <- data |>
          dplyr::mutate(dplyr::across(
            tidyselect::all_of(as.character(col_vars[col_vars_3levels])),
            ~ suppressWarnings(forcats::fct_recode(., rlang::splice(rm_levels_by_col_vars[[dplyr::cur_column()]] )))
          ))
      }
    }

    remove_levels <- purrr::map(dplyr::select(data, !!!col_vars[lv1]), ~ c(levels(.)[-1], "NA"))
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

  # Factor tier-1: NONE on the tab()/tab_many() path. The former opt-in shared-scan "fusion"
  # (`options(tabxplor.fuse_min_rows)`, off by default) was removed in Phase 9c: it was a NET NEGATIVE
  # once the O(cells) per-table build dominates (the survey-scale build is N-independent -- fusing the
  # O(N) scan buys nothing; +1-7% when forced on) -- see dev/tabxplor_1.4.0_decisions.md 30. The
  # `.fine`/`fine_for_pair()`/`use_raw` seam in tab_plain() STAYS: it is now EXCLUSIVELY the jmvtab
  # cache seam (jmv_cache_aggregate() injects a per-pair `fine_fused`; that path early-returns above),
  # locked by test-fuse-parity.R (direct factor `.fine` == raw scan) + test-jmvtab-cache.R.
  #
  # fine_fused = NULL is kept as an explicit ctx element (ctx_update()'s single-bracket [<-) so
  # tab_transform()'s list2env() finds it (`ctx$x <- NULL` would delete the key).
  ctx_update(ctx, list(fine_num = fine_num, fine_fused = NULL))
}


# fine_for_pair() -- pick the factor tier-1 aggregate for one (row_var x col_var) pair.
# DESIGN (Phase 7e): tab_transform() feeds tab_plain(.fine=) either ONE joint count DT (tab_counts()'s
# injected aggregate -- the is.data.table branch returns it UNCHANGED, byte-for-byte, so counts parity
# cannot move) OR a per-pair named list keyed "row_var\rcol_var" (the jmvtab cache: the reuse unit is
# per pair -- see dev/tabxplor_jmvtab_cache_design.md 3.2/6). Plain tab()/tab_many() no longer supply a
# `.fine` (Phase 9c removed the opt-in factor fusion) -> NULL here -> tab_plain()'s `use_raw` raw scan.
# A missing pair also -> NULL. tab_plain always MARGINALISES .fine to its own pair, so a per-pair margin
# is idempotent there (locked by test-fuse-parity.R + test-jmvtab-cache.R).
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
# Phase 9a: SCALAR over ONE row_var. The row axis is now an OUTER map in tab_build_tables(); this ctx
# describes a single row_var (its per-row_var settings are scalars, its fine_num is one aggregate).
# The former internal tab_pmap() row-dispatch is gone (it was always serial once the whole-pipeline
# tab_build_one() worker took over the parallel dispatch). The col axis stays vectorised (pmap over
# factor col_vars). tabs_text / tabs_num / tests / chi2_num are now SINGLE objects (or NULL).
#' @keywords internal
#' @noRd
tab_transform <- function(ctx) {
  list2env(ctx, environment())
  .by_table <- by_table
  .fine     <- fine_fused
  row_var   <- as.character(row_vars)                 # this ctx describes exactly ONE row_var
  rv        <- rlang::sym(row_var)
  # `wt` arrives as a character name (or character(0) for no weight); rebuild the bare symbol for `!!`.
  wt_sym    <- if (length(wt) == 0L) wt else rlang::sym(as.character(wt))

  # Defaults for leaner ctxs (tab_counts / direct stage tests / a ctx built without the jmvtab hook).
  # cached_tests is the jmvtab tier-2 hook: the FULL per-row_var list keyed by row_var name (from
  # jmv_cache_aggregate) -- kept whole in the shared ctx, this row_var's entry picked below. NULL/absent
  # on the tab()/tab_counts() path -> recompute in tab_apply_tests(). ref_vect defaults to the scalar-ref
  # broadcast over col_vars when a leaner ctx reached transform without it.
  if (!exists("cached_tests", inherits = FALSE)) cached_tests <- NULL
  if (!exists("ref_vect",     inherits = FALSE)) ref_vect     <- rep(ref, length(col_vars))
  # 14v-ii: the CI-method args (leaner ctxs -- tab_counts / stage tests -- may omit them).
  if (!exists("method_ratio",      inherits = FALSE)) method_ratio      <- "katz"
  if (!exists("method_mean_diff",  inherits = FALSE)) method_mean_diff  <- "welch"
  if (!exists("method_mean_ratio", inherits = FALSE)) method_mean_ratio <- "robust"
  cached_test <- if (is.null(cached_tests)) NULL else cached_tests[[row_var]]

  # --- numeric col_vars: one tab_num() (adopts the per-row_var moment aggregate fine_num) ---
  tabs_num <- NULL
  chi2_num <- NULL
  if (sum(col_vars_num) != 0) {
    tabs_num <- tab_num(data, !!rv, as.character(col_vars)[col_vars_num], as.character(tab_vars),
                        wt = !!wt_sym, na = na_num, digits = digits[col_vars_num],
                        ref = ref, ci = ci, conf_level = conf_level, stars = stars, comp = comp,
                        method_mean_diff = method_mean_diff, method_mean_ratio = method_mean_ratio,
                        ci_scale = ci_scale[1],
                        color = color_num, totaltab = totaltab, totaltab_name = totaltab_name,
                        tot = dplyr::if_else(totrow, "row", "no"), total_names = total_names,
                        .fine = fine_num, .by_table = .by_table,
                        # `color_num` is the pipeline's own legacy string (legacy_union() may have
                        # built "diff_ci"/"after_ci" from the user's color + color_signif), so
                        # tab_num() must not deprecate it back at the user.
                        .color_deprecate = FALSE)
    # Phase 3b: whole-table test for NUMERIC col_vars = one-way ANOVA (Welch + classic F), via
    # tab_chi2()'s test step (it detects mean col_vars and calls agg_anova()). Only the tidy `test`
    # tibble is kept (merged with the factor test at assemble); NULL when chi2 is off for this row_var.
    if (isTRUE(chi2)) chi2_num <- get_test(tab_chi2(tabs = tabs_num, calc = "p", comp = comp))
  }

  # --- factor col_vars: one tab_plain() per col_var, joined into ONE table ---
  tabs_text <- NULL
  tests     <- chi2   # logical placeholder; assemble's is.logical() fallback handles a numeric-only tab
  if (sum(col_vars_text) != 0) {
    text <- purrr::pmap(
      list(col_vars[col_vars_text], digits[col_vars_text], na_text,
           pct_vect[col_vars_text], ref_vect[col_vars_text]),
      function(.col_var, .digits, .na, .pct, .ref)
        tab_plain(data, !!rv, !!.col_var, as.character(tab_vars), wt = !!wt_sym, na = .na,
                  digits = .digits, pct = .pct, ref = .ref, ref2 = ref2, comp = comp, OR = OR,
                  color = color_diff_OR, totaltab = totaltab, totaltab_name = totaltab_name,
                  tot = c("row", "col"), total_names = total_names,
                  conf_level = conf_level, stars = stars, color_signif = color_signif,
                  .fine = fine_for_pair(.fine, row_var, .col_var), .by_table = .by_table)
    ) |> purrr::set_names(as.character(col_vars[col_vars_text]))

    # Rename level names duplicated across col_vars (suffix with the col_var name) before the join.
    # Computed per-row_var: the level names are col_var-determined (identical across row_vars), so this
    # is byte-identical to the former global set -- locked by test-parallel-parity.R's collision case.
    lvl_names <- text |>
      purrr::map(~ purrr::discard(names(.), names(.) %in% c(row_var, as.character(tab_vars)))) |>
      purrr::flatten_chr()
    duplicated_levels <- unique(lvl_names[duplicated(lvl_names)])
    if (length(duplicated_levels) != 0) {
      text <- purrr::imap(text, ~ dplyr::rename_with(.x, function(.names)
        dplyr::if_else(.names %in% duplicated_levels, paste0(.names, "_", .y), .names)))
    }

    tabs_text <- purrr::reduce(text, dplyr::full_join, by = c(as.character(tab_vars), row_var))

    # Phase 9b-5: the 9b-4 no-op carrier round-trip that used to sit here is gone -- tab_chi2()'s
    # whole-table test now reads plain fields directly (chi2_compute_test(), no fmt reconstruction),
    # so the tests boundary no longer needs a pre-established carrier. (tab_ci() is still record-based;
    # its carrier write-back is Phase 9b-5 increment 2. fmt_unwrap/fmt_wrap stay for that + the tests.)

    # DESIGN: ordering invariant — tab_chi2() and tab_ci() are INDEPENDENT (either order works), but
    # BOTH must run BEFORE non-first levels are dropped (in tab_assemble), so they are computed on the
    # full set of levels. See CLAUDE.md § Global Architecture. Phase 6a: one pass through the shared
    # tab_apply_tests() (chi2 -> capture test -> ci). Phase 3b: contrib ("ctr") only when color_ctr !=
    # "no". Phase 7e: cached_test is this row_var's tier-2 hit (NULL -> recompute as before).
    applied   <- tab_apply_tests(tabs_text, do_chi2 = chi2, ci = ci, comp = comp,
                                 color_ctr = color_ctr, color_ci = color_ci,
                                 conf_level = conf_level, stars = stars,
                                 method_cell = method_cell, method_diff = method_diff,
                                 method_ratio = method_ratio, method_mean_diff = method_mean_diff,
                                 method_mean_ratio = method_mean_ratio,
                                 ci_scale = ci_scale, cached_test = cached_test)
    tabs_text <- applied$tab
    tests     <- applied$test
  }

  # --- repack: transform produces this row_var's built+tested table(s) + the tier-2 test. ---
  ctx_update(ctx, list(
    tabs_text = tabs_text, tabs_num = tabs_num, tests = tests, chi2_num = chi2_num
  ))
}


# === STAGE 5/5: tab_assemble() -- join, totals, wrap, output shape, render prep (tier 4) ====
# Built tables -> the final tabxplor_tab / list: non-first-level drop, add_n/add_pct, total col/row
# removal, the numeric+factor join, the whole-table test merge + class wrap, output-shape compaction,
# p-value lines, tab_spread, unwrap, and the optional tab_kable. Pure O(cells) display assembly.
# Phase 8/9a: split into tab_assemble_tables() (finish ONE row_var's table -- level-drop, add_n/pct,
# total col/row removal, numeric+factor join, whole-table test merge + class wrap) and
# tab_assemble_output() (the cross-row_var output shape: merge/compact, p-value lines, n_min, spread,
# unwrap, kable). tab_assemble_tables() is SCALAR over one row_var (the outer map in tab_build_tables()
# drives the row axis); it is byte-identical whether the row_var is built alone or as a slice of an
# integrated build (the total-col decoupling, Phase 8). See R/tab-parallel.R.
#' @keywords internal
#' @noRd
tab_assemble <- function(ctx) {
  tab_assemble_output(tab_assemble_tables(ctx))
}

# SCALAR over ONE row_var. tabs_text / tabs_num are the single built factor / numeric table (or NULL);
# tests / chi2_num the single test tibbles. Produces ctx$tabs = the single finished tabxplor_tab (its
# whole-table test baked into the `test` attribute) + ctx$tests (kept for the jmvtab tier-2 store).
#' @keywords internal
#' @noRd
tab_assemble_tables <- function(ctx) {
  list2env(ctx, environment())
  row_var <- as.character(row_vars)

  if (sum(col_vars_text) != 0) {

    #Remove unwanted levels (keep only the first when levels = "first")
    if (any(lv1)) {
      rm_levels <- purrr::imap(remove_levels, ~ c(.x, paste0(.x, "_", .y))) |> purrr::flatten_chr()
      tabs_text <- dplyr::select(tabs_text, -tidyselect::any_of(rm_levels))
    }

    # Phase 10i-B: add_n / add_pct are NO LONGER baked here. The intent is stored in the
    # `render_extras` attribute (below), and tab_materialize_extras() re-creates the add_n `n`
    # column / in-cell composite + the add_pct `col_pct` / `row_pct` at DISPLAY, byte-identically
    # (it reuses this very tab_add_n_pct() on the finished table). This keeps the built tab the
    # "core" table and lets the reserved-`n`/`row_pct`/`all_col_vars` special-cases downstream go.

    #Remove unwanted total columns
    if (!tot_cols_type %in% c("each", "no_no_create")) {
      if (tot_cols_type == "no_delete")
        tabs_text <- dplyr::select(tabs_text, -where(is_totcol))
      if (tot_cols_type == "some")
        tabs_text <- dplyr::select(tabs_text, -(where(~ is_totcol(.) & !get_col_var(.) %in% totcol)))

      if (tot_cols_type == "all_col_vars") {
        totcols_present <- unique(names(tabs_text)[is_totcol(tabs_text)])
        last_tot <- dplyr::last(totcols_present)
        drop_tot <- totcols_present[totcols_present != last_tot & !is.na(totcols_present)]

        tabs_text <- tabs_text |>
          dplyr::select(-tidyselect::any_of(drop_tot)) |>
          dplyr::relocate(where(is_totcol), .after = tidyselect::last_col()) |>
          dplyr::rename_with(~ total_names[2], .cols = tidyselect::all_of(last_tot)) |>
          dplyr::mutate(dplyr::across(tidyselect::last_col(), ~ set_col_var(., "all_col_vars")))
      }
    }

    # Lone total column -> "Total" with no col_var name. Phase 8: dedup with unique() so the "lone
    # total" test is on the DISTINCT total-column name (the "Total_<lastcv>" internal suffix leaked into
    # multi-row_var tables otherwise). A genuinely multi-total table (>1 distinct name) keeps the
    # qualified names. This is what DECOUPLES the per-row_var build from the integrated one.
    totnames <- unique(names(tabs_text)[stringi::stri_detect_regex(names(tabs_text),
                                                            paste0("^", total_names[2], "_"))])
    if (length(totnames) == 1)
      tabs_text <- dplyr::rename(
        tabs_text,
        tidyselect::any_of(purrr::set_names(totnames, rep(total_names[2], length(totnames)))))
  }

  # Join numeric + factor (or take whichever exists).
  if (sum(col_vars_num) != 0 & sum(col_vars_text) != 0) {
    tab <- dplyr::full_join(tabs_text, tabs_num, by = c(as.character(tab_vars), row_var))

    col_vars_order <- tab |>
      purrr::map(~ purrr::map(get_col_var(.), ~ which(as.character(col_vars) == .))) |>
      purrr::flatten()
    col_vars_order <- col_vars_order |>
      purrr::map_if(names(col_vars_order) %in% tab_row_names, ~ 0L) |>
      purrr::map_int(~ if (length(.) == 0) length(col_vars) + 1L else .) |>
      sort() |> names()

    tab <- dplyr::select(tab, tidyselect::any_of(col_vars_order))

  } else if (sum(col_vars_num) != 0) {
    tab <- tabs_num
  } else {
    tab <- tabs_text
  }

  #Remove the unwanted total row. Phase 9a: scalar; `any(pct == "col")` designs out a pct/OR
  # length-mismatch (the numeric-only per-col_var-pct latent bug, analogous to tab.R's ex-L1252).
  no_totrow <- (totrow == FALSE) ||
    ((any(pct == "col") && OR %in% c("OR", "or", "OR_pct", "or_pct")) &&
       tot_cols_type != "no_no_create")
  if (no_totrow) {
    totrows     <- is_totrow(tab)
    tottab_rows <- is_tottab(tab)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows
    tab <- tab |>
      tibble::add_column(totrows = totrows, tottab_line = tottab_line) |>
      dplyr::filter(!.data$totrows | .data$tottab_line) |>
      dplyr::select(-"totrows", -"tottab_line")
  }

  # Combine the factor (chi2) and numeric (ANOVA F) whole-table test tibbles. For a numeric-only table
  # `tests` is still the boolean `chi2` flag (the factor branch was skipped), so is.logical() converts
  # it to an empty test tibble.
  if (is.logical(tests)) tests <- new_test_tibble()
  if (!is.null(chi2_num)) tests <- dplyr::bind_rows(tests, chi2_num)

  # Phase 10i-B: store the add_n / add_pct DISPLAY intent (materialised by tab_materialize_extras()).
  # Phase 14p: add_n / add_pct only make sense beside a col_var (they fold the base `n` / a `col_pct`
  # into the crosstab). A no-col_var table (`tab(relig)`) is a plain frequency: its `n` / `pct` / `wn`
  # columns ARE the primary content (built by tab_plain()'s no_col_var block), not display extras, so
  # the intent must be OFF -- else tab_fold_addn_incell() would try to fold into a Total column that
  # does not exist and silently DROP the real `n` column (the <=1.3.1 regression).
  fmt_here        <- purrr::map_lgl(tab, is_fmt)
  has_real_colvar <- any(fmt_here & get_col_var(tab) != "no_col_var")
  render_extras <- list(add_n  = isTRUE(add_n)  && has_real_colvar,
                        add_pct = isTRUE(add_pct) && has_real_colvar)
  # Phase 13b / 14v-ii: record which CI method / confidence level was actually used, so
  # tab_color_legend() can name it (only meaningful when a CI was computed; harmless otherwise -- absent
  # settings fall back). 14v-ii adds the numeric + ratio methods (welch/student, robust/quasi/poisson,
  # katz); the legend picks the relevant one off the column's type / ci_type.
  ci_settings <- if (!identical(ci, "no")) {
    list(conf_level = conf_level, method_cell = method_cell, method_diff = method_diff,
         method_ratio = method_ratio, method_mean_diff = method_mean_diff,
         method_mean_ratio = method_mean_ratio)
  } else NULL
  # Phase 14d: record the variable ROLES here, where they are known. Recovering them from the finished
  # table is guesswork (and wrong after tab_compact) -- see get_vars_attr() in R/tab_classes.R.
  # Phase 16d: also record the weight column NAME (character(0) when unweighted) -> the footer "Weighted
  # by <wt>." line. `wt` is a local from list2env(ctx) -- the resolved weight name (see tab_transform()).
  vars_attr <- new_vars_attr(row_vars = row_var, col_vars = as.character(col_vars),
                             tab_vars = as.character(tab_vars),
                             wt = if (length(wt) == 0L) NA_character_ else as.character(wt)[1])
  if (!lv1_group_vars(tab)) {
    tab    <- dplyr::group_by(tab, !!!tab_vars)
    groups <- dplyr::group_data(tab)
    tab    <- new_grouped_tab(tab, groups = groups, subtext = subtext, test = tests,
                              render_extras = render_extras, ci_settings = ci_settings,
                              vars = vars_attr)
  } else {
    tab <- new_tab(tab, subtext = subtext, test = tests, render_extras = render_extras,
                   ci_settings = ci_settings, vars = vars_attr)
  }

  # Row_var finishing done: ctx$tabs is the single finished tabxplor_tab/grouped_tab (the whole-table
  # test baked into its `test` attribute). tab_build_tables() gathers these into a list. ctx$tests is
  # kept for the jmvtab tier-2 store (jmv_cache_store_tests).
  ctx_update(ctx, list(tabs = tab, tests = tests))
}

# tab_assemble_output() -- the cross-row_var output shape (Phase 8 split from tab_assemble()).
# Takes ctx$tabs (the list of finished per-row_var tabs) and merges/unwraps into the final result.
#' @keywords internal
#' @noRd
tab_assemble_output <- function(ctx) {
  list2env(ctx, environment())

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


  # Phase 10i-B: p-value rows are NO LONGER baked at build. The whole-table `test` attribute is KEPT
  # (tab_assemble_tables set it and nothing drops it now), and the p-value rows are materialised at
  # DISPLAY -- as body rows by the exporters (tab_export_prep / tab_xl via tab_materialize_extras), and
  # as the summary block in the console (Phase 16a). This lets every reserved-"pvalue"-row
  # special-case downstream (n_min, jmvtab re-ref, ...) shrink, and keeps the built tab the "core"
  # table (see dev/tabxplor_1.4.0_decisions.md §34).

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

  # KNOWN-BUG (Last Phase e): with a two-channel colour (a background channel, e.g. color = TRUE ->
  # c("diff","ratio")) this tab_kable() errors "no applicable method for 'mutate' ... tabxplor_kable".
  # Only via this output_kable switch; tab_kable(tab(..., color = TRUE)) and console print both work.
  if (getOption("tabxplor.output_kable") == TRUE) tabs <- tabs |> tab_kable()

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
#' \donttest{ data <- forcats::gss_cat |> dplyr::filter(year %in% c(2000, 2014))
#'
#' tabs <-
#'   tab(data, relig, marital, c(year, race), pct = "row", totaltab = "no",
#'       color = "diff", tot = "row", other_if_less_than = 30)
#'
#' tabs |>
#'   dplyr::select(year, race, relig, Married) |>
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
  col_levels <- get_vars$col_vars_levels |> purrr::flatten_chr()
  row_var    <- get_vars$row_var
  tab_vars   <- get_vars$tab_vars
  tab_vars_new <- tab_vars[!tab_vars %in% spread_vars]

  na_values <- purrr::map(dplyr::ungroup(tabs)[col_levels],
                          ~ fmt0(type = get_type(.x), display = get_display(.x[1]))) |>
    purrr::set_names(col_levels)


  totrows <- is_totrow(tabs)
  if (any(totrows)) {
    #tab_match_groups_and_totrows(tabs)
    tabs <- tabs |> dplyr::group_by(!!!rlang::syms(tab_vars))
    groups <- dplyr::group_vars(tabs)

    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows & totrows

    tabs <- tabs |> tibble::add_column(totrows, tottab_rows, tottab_line)

    # if two tab_vars or more, calculate totals for each level of spread_var
    if (length(tab_vars_new) != 0 & any(tottab_rows)) {

      # if (recalculate) {
      #   if (any(get_type(tabs) == "mean")) {
      #     warning(paste0("Since there are several tab_vars, some totals are missing. ",
      #                    "Means for the new general total row were recalculated based on a ",
      #                    "weighted mean, which is only an approximation"))
      #   }
      #   new_totals <- tabs |>
      #     dplyr::filter(.data$totrows) |>
      #     dplyr::group_by(!!!rlang::syms(spread_vars)) |>
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
      #   ensemble_names <- tabs |>
      #     dplyr::filter(tottab_line) |>
      #     dplyr::ungroup() |>
      #     select(tab_vars_new) |> purrr::map_chr(~ as.character(dplyr::first(.)))
      #
      #   total_ensemble <- tabs |>
      #     dplyr::filter(tottab_line) |>
      #     dplyr::pull(row_var) |> as.character()
      #
      #   new_totals <- new_totals |>
      #     tab_pct(just_diff = TRUE) |>
      #     dplyr::mutate(dplyr::across(where(is_fmt),
      #                                 ~ set_color(., tabs_colors[dplyr::cur_column()]))) |>
      #     dplyr::mutate(!!rlang::sym(row_var) := factor(total_ensemble))
      #
      #   new_totals <- new_totals |>
      #     purrr::reduce2(.x = names(ensemble_names), .y = ensemble_names, .init = new_totals,
      #                    .f = ~ dplyr::mutate(..1, !!rlang::sym(..2) := factor(..3))
      #     )  |>
      #     dplyr::filter(!is_tottab(.))
      #
      # }

      tabs <- tabs |> dplyr::filter(!tottab_line)

      #if (recalculate) tabs <- tabs |> dplyr::bind_rows(new_totals)
    }

    new_levels <- tabs |>
      dplyr::filter(.data$totrows & !.data$tottab_line) |>
      dplyr::select(!!!tab_vars, !!row_var) |>
      dplyr::arrange(!!!rlang::syms(tab_vars_new), .by_group = FALSE,
                     .by_totals = FALSE, .only_main_display = FALSE) |>
      dplyr::mutate(
        new_levels = paste(totname, paste(!!!rlang::syms(tab_vars_new), sep = " / ")) |>
          stringi::stri_trans_toupper()
      )
    new_levels <- purrr::set_names(as.character(dplyr::pull(new_levels, row_var)),
                                   new_levels$new_levels)

    # if (length(groups) - 1 != 0) {
    #   group_vars_totals <-
    #     dplyr::group_keys(dplyr::filter(tabs, !tottab_line)) |> #dplyr::mutate(bis = PR0) |>
    #     dplyr::select(-tidyselect::all_of(spread_vars)) |>
    #     tidyr::unite(!!row_var, sep = " / ") |>
    #     dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ paste(totname, .))) |>
    #     tibble::deframe() |>
    #     stringi::stri_trans_toupper() |> forcats::as_factor()
    # } else {
    #   group_vars_totals <- factor(totname)
    # }
    #
    # former_levels <-
    #   tibble::add_column(tabs, totrows = is_totrow(tabs),
    #                      tottab = is_tottab(tabs)) |>
    #   dplyr::filter(.data$totrows & !.data$tottab) |> dplyr::pull(row_var)
    #
    # group_vars_totals <- vctrs::vec_recycle(group_vars_totals, length(former_levels))
    #
    # new_levels <- former_levels |> as.character() |>
    #   purrr::set_names(group_vars_totals)

    tabs <- tabs |> dplyr::mutate(
      !!rlang::sym(row_var) := forcats::fct_recode(!!rlang::sym(row_var),
                                                   !!!new_levels) |>
        forcats::fct_relevel(unique(names(new_levels)), after = Inf)
    ) |>
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

  tabs <- tabs |>
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


#' Transpose a cross-table (swap its rows and columns)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_transpose()` is **soft-deprecated** since tabxplor 1.4.0. It flips the *object* (the
#' `tabxplor_fmt` fields), which cannot carry a transposed column's mixed cell types, so a table with
#' several row variables or numeric columns transposes incorrectly (numeric cells mis-coloured,
#' duplicated total columns). Use the exporters' `transpose = TRUE` argument instead --- it flips the
#' finished render model after colours are computed, and handles several row variables and numeric
#' columns:
#'
#' ```r
#' tab(data, row_vars, col_vars, pct = "row") |> tab_kable(transpose = TRUE)   # or tab_md() / tab_xl()
#' ```
#'
#' The function is kept (unchanged) for the single-row-variable round-trip it always supported.
#'
#' @param tabs A single table made with \code{\link{tab}} (one row variable, one column variable; not
#'   a subtabled table with `tab_vars`, and at most one total row and one total column).
#' @param name The name to give the new first (label) column, holding the old column-variable levels.
#'   `NULL` (default) uses the old column-variable name.
#'
#' @return A transposed `tabxplor_tab`.
#' @export
#'
#' @examples
#' \donttest{
#' # build marital x race as row percentages, then display it as race x marital:
#' tab(forcats::gss_cat, marital, race, pct = "row") |>
#'   tab_kable(transpose = TRUE)
#' }
tab_transpose <- function(tabs, name = NULL) {
  lifecycle::deprecate_soft(
    "1.4.0", "tab_transpose()",
    details = 'Use the `transpose = TRUE` argument of tab_kable() / tab_md() / tab_xl() / tab_export().')
  if (!is.data.frame(tabs)) {
    cli::cli_abort("{.arg tabs} must be a {.pkg tabxplor} table.")
  }
  tabs <- dplyr::ungroup(tabs)

  vars <- tab_get_vars(tabs)
  if (length(vars$tab_vars) > 0) {
    cli::cli_abort(c(
      "{.fn tab_transpose} does not support tables with {.arg tab_vars} yet.",
      "i" = "It transposes a single table (one row variable, one column variable)."
    ))
  }
  row_var <- vars$row_var
  if (length(row_var) != 1) {
    cli::cli_abort("{.fn tab_transpose} needs a table with exactly one row variable.")
  }

  fmt_mask <- purrr::map_lgl(tabs, is_fmt)
  fmtc     <- names(tabs)[fmt_mask]
  if (length(fmtc) == 0) {
    cli::cli_abort("{.arg tabs} has no {.pkg tabxplor} formatted columns to transpose.")
  }

  # Phase 14d: SEVERAL row_vars (a merged tab(): `row_var` + `levels` columns). Its row key is the
  # PAIR (row_var, level), so fold the pair into one key column and let the single-row_var pivot below
  # run unchanged; `src_row_var` then maps each new column back to the variable it came from, which
  # becomes that column's col_var (so a merged row% table transposes into a col% table whose col_vars
  # are the old row_vars -- exporters span their names over their levels for free).
  # The level names are suffixed `_<var>` ONLY where two row_vars share one, mirroring the convention
  # tab() itself uses for colliding col_var levels (`Other_race`) -- and the exporters' suffix
  # stripping (tab_col_var_header) reverses it.
  merged      <- isTRUE(tab_vars_recorded(tabs)$compacted)
  src_row_var <- NULL
  if (merged) {
    src   <- as.character(tabs[["row_var"]])
    lvl   <- as.character(tabs[[row_var]])
    dup   <- lvl %in% names(which(tapply(src, lvl, function(s) length(unique(s))) > 1))
    key   <- ifelse(dup, paste0(lvl, "_", src), lvl)
    if (anyDuplicated(key)) {
      cli::cli_abort(c("{.fn tab_transpose} cannot name the transposed columns uniquely.",
                       "i" = "Two rows share the same variable and level."))
    }
    src_row_var <- stats::setNames(src, key)
    tabs[[".tx_key"]] <- factor(key, levels = key)
    row_var <- ".tx_key"
  }

  # --- capture the axis roles BEFORE the pivot (in_totrow / in_refrow are uniform across fmt cols) ---
  totrow_lgl   <- vctrs::field(tabs[[fmtc[1]]], "in_totrow")
  refrow_lgl   <- vctrs::field(tabs[[fmtc[1]]], "in_refrow")
  totcol_names <- fmtc[purrr::map_lgl(tabs[fmtc], is_totcol)]
  refcol_names <- fmtc[purrr::map_lgl(tabs[fmtc], is_refcol)]
  # WARNING: base `[[`, NOT dplyr::pull(all_of(row_var)) -- tidyselect evaluates `row_var` in the DATA
  # MASK first, and a merged table has a column literally NAMED `row_var`, so the tidyselect form
  # silently pulled that column instead of the local variable. (Latent before Phase 14d: a merged table
  # never got past the tab_vars guard above.)
  labels        <- as.character(tabs[[row_var]])
  totrow_labels <- labels[totrow_lgl]
  refrow_labels <- labels[refrow_lgl]
  # One total row per SUB-TABLE (a merged table legitimately has one per row_var); each becomes its
  # own total column. Only a single sub-table with two total rows is ambiguous.
  max_per_sub <- function(x) if (merged) max(c(0L, table(as.character(tabs[["row_var"]])[x])))
                             else sum(x)
  if (max_per_sub(totrow_lgl) > 1) {
    cli::cli_abort("{.fn tab_transpose} does not work (yet) with more than one total row.")
  }
  if (length(totcol_names) > 1) {
    cli::cli_abort("{.fn tab_transpose} does not work (yet) with more than one total column.")
  }

  # representative REAL col_var level column (not the total column, not the count "all_col_vars"),
  # whose per-column attributes are copied onto every transposed column (they are uniform).
  real_col_vars <- vars$col_vars[!vars$col_vars %in%
                                   c("all_col_vars", "", "no", NA_character_)]
  old_col_var <- if (length(real_col_vars) > 0) real_col_vars[[1]] else NA_character_
  rep_name <- fmtc[purrr::map_lgl(tabs[fmtc], ~ identical(get_col_var(.), old_col_var))]
  rep_name <- if (length(rep_name) > 0) rep_name[[1]] else fmtc[[1]]
  rep_attrs <- purrr::set_names(
    lapply(fmt_col_attrs, function(a) attr(tabs[[rep_name]], a, exact = TRUE)), fmt_col_attrs)
  old_type <- if (is.null(rep_attrs$type)) "row" else rep_attrs$type
  new_type <- switch(old_type, row = "col", col = "row", old_type)

  # --- the pivot: old columns become rows, old row_var levels become columns ---
  if (is.null(name)) name <- if (!is.na(old_col_var)) old_col_var else "variables"
  # the merged table's (row_var, levels) pair is now carried by .tx_key: drop the originals, else
  # pivot_wider would treat them as extra id columns and give one row per (col_var level x row_var).
  if (merged) tabs <- tabs[, setdiff(names(tabs), c("row_var", "levels")), drop = FALSE]
  long <- tabs |>
    tidyr::pivot_longer(cols = tidyselect::all_of(fmtc),
                        names_to = name, values_to = "value")
  long[[name]] <- factor(long[[name]], levels = fmtc)          # keep the col_var-level order as rows
  wide <- long |>
    tidyr::pivot_wider(names_from = tidyselect::all_of(row_var),
                       values_from = "value", names_sort = FALSE)

  new_fmtc   <- setdiff(names(wide), name)                     # = the old row_var levels
  new_labels <- as.character(wide[[name]])                     # = fmtc (the old column names)

  # --- rebuild the flattened per-column attributes + swap the axis flags ---
  for (nm in new_fmtc) {
    col <- wide[[nm]]
    for (a in fmt_col_attrs) attr(col, a) <- rep_attrs[[a]]    # restore uniform col_var attributes
    col <- set_type(col, new_type)                            # row <-> col
    # new col_var = the old row variable this column's rows came from (per column when merged)
    col <- set_col_var(col, if (merged) unname(src_row_var[[nm]]) else row_var)
    col <- as_totcol(col, FALSE)
    col <- as_refcol(col, FALSE)
    col <- as_totrow(col, new_labels %in% totcol_names)       # old total COLUMN -> new total ROW
    col <- as_refrow(col, new_labels %in% refcol_names)       # old reference COLUMN -> new ref ROW
    wide[[nm]] <- col
  }
  # old total ROW -> new total COLUMN; old reference ROW -> new reference COLUMN (else, under the
  # col%-inversion, the total column is the reference, matching a native pct = "col" table).
  # Phase 14d: ONE per sub-table, not one per table -- a merged table has a total row per row_var, and
  # each must become its own total column (the guard above already rejects two within one sub-table).
  for (lab in intersect(totrow_labels, new_fmtc)) {
    wide[[lab]] <- as_totcol(wide[[lab]], TRUE)
  }
  ref_targets <- if (length(refrow_labels) >= 1) refrow_labels else totrow_labels
  for (lab in intersect(ref_targets, new_fmtc)) {
    wide[[lab]] <- as_refcol(wide[[lab]], TRUE)
  }

  wide[[name]] <- factor(new_labels, levels = new_labels)

  # re-key the whole-table test tibble: the new row_var is the old col_var and vice versa.
  test <- get_test(tabs)
  if (is.data.frame(test) && nrow(test) > 0) {
    rv <- test[["row_var"]]; cv <- test[["col_var"]]
    test[["row_var"]] <- cv
    test[["col_var"]] <- rv
  }

  # Phase 10i-B: carry the add_n/add_pct DISPLAY intent through the transpose (orientation-agnostic --
  # the materialiser adds add_n as a ROW once the table reads as col%). So transpose(row% add_n) then
  # display == a native col% add_n table.
  # Phase 14d: the roles SWAP, so (like `test` above) they must be re-keyed, never passed through.
  # The result is a single-row_var table whose row_var is the old col_var and whose col_vars are the
  # old row_vars -- `compacted` is FALSE again: the merged shape is gone, undone by the pivot.
  attrs <- tab_attrs(tabs)
  attrs$test <- test
  attrs$vars <- new_vars_attr(row_vars = name,
                              col_vars = unique(purrr::map_chr(wide[new_fmtc], get_col_var)),
                              tab_vars = character(0),
                              wt = get_vars_attr(tabs)$wt)   # Phase 16d: the weight survives a transpose
  rlang::exec(new_tab, wide, !!!attrs)
}





# Phase 14d: read the RECORDED roles (the `vars` attribute), validated against the table's actual
# columns -- a dplyr chain can rename or drop the very columns the attribute names, and a stale
# attribute must never beat what is really there. NULL -> the caller keeps the column-type heuristic.
# CONTRACT: the returned `row_var`/`tab_vars` are COLUMN names (what every consumer indexes with),
# NOT the source variable names. On a compacted table those differ: the row levels live in a column
# literally named "levels" and the source names only in the `row_var` column's values -- which is
# exactly why the roles have to be recorded. `row_vars` carries the source names for the few callers
# that want them (the tab_xl title).
#' @keywords internal
tab_vars_recorded <- function(tabs) {
  v <- get_vars_attr(tabs)
  if (is.null(v)) return(NULL)
  nms     <- names(tabs)
  row_col <- if (isTRUE(v$compacted)) "levels" else v$row_vars
  if (length(row_col) != 1 || !row_col %in% nms) return(NULL)
  tab_vars <- v$tab_vars
  if (!all(tab_vars %in% nms)) return(NULL)
  list(row_var = row_col, tab_vars = tab_vars,
       row_vars = v$row_vars, compacted = isTRUE(v$compacted))
}


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
  rec <- tab_vars_recorded(tabs)

  if ("col_vars" %in% vars) {
    fmtc <- purrr::map_lgl(tabs, is_fmt)
    col_vars       <- get_col_var(tabs[fmtc]) |> purrr::discard(~ is.na(.))
    col_vars_names <- col_vars |> unique()

    col_vars_levels <-
      purrr::map(col_vars_names, ~ names(col_vars[col_vars == .])) |>
      purrr::set_names(col_vars_names)

    col_vars <- col_vars_names
  }

  fct_cols <- purrr::map_lgl(tabs, is.factor)

  # Phase 10c guard: with no factor column `tail()` returns a NULL name -> keep it a 0-length
  # character so downstream `which()/%in%` stay well-defined (the crash is caught upstream by
  # tab_render_vars(), but tab_get_vars() must not itself emit a stray NULL). See tab_render_vars().
  if ("row_var" %in% vars) {
    row_var <- if (!is.null(rec)) rec$row_var else names(utils::tail(fct_cols[fct_cols], 1L))
    if (is.null(row_var)) row_var <- character(0)
  }

  if ("tab_vars" %in% vars) tab_vars <-
    if (!is.null(rec))            rec$tab_vars
    else if (length(row_var) == 0) names(fct_cols[fct_cols])
    else names(fct_cols[fct_cols & names(fct_cols) != row_var])



  ls(pattern = "^row_var$|^col_vars$|^col_vars_levels$|^tab_vars$") |>
    purrr::set_names() |>
    purrr::map(~ rlang::sym(.) |> rlang::eval_tidy())
}


# Phase 10c: the ROBUST render-time variable detector, used by the print methods and the
# exporters' graceful-degrade guard (and, from Phase 10d, by the shared export prep). Unlike
# tab_get_vars() -- which derives row_var from the fragile "last factor column" heuristic and
# lets consumers crash (dplyr::pull(tabs, integer(0))) when there is none -- this:
#   - keeps the position-independent col_var-attribute path for col_vars;
#   - places row_var / tab_vars from dplyr::group_vars() (which survives rename/select/relocate:
#     tab_build() groups by tab_vars, tab_compact() by the literal "row_var" column) so a factor
#     moved AFTER the fmt columns is no longer miswritten; row_var = the last factor NOT in the
#     groups (= "levels" for a compacted tab, the real row var otherwise);
#   - returns list(degrade = TRUE, reason = ...) when the object can't be read as a tabxplor
#     table (not a data frame / no tabxplor_fmt columns / no factor row-or-tab variable).
# BYTE-IDENTICAL to tab_get_vars() for every well-formed table (verified across the fixtures);
# it only fixes the mis-positioned-factor case and the no-factor/no-fmt crashes.
#' @keywords internal
tab_render_vars <- function(tabs) {
  if (!is.data.frame(tabs))
    return(list(degrade = TRUE, reason = "the object is not a data frame"))

  fmt_mask <- purrr::map_lgl(tabs, is_fmt)
  if (!any(fmt_mask))
    return(list(degrade = TRUE,
                reason = "the table has no tabxplor_fmt columns (not a tabxplor table)"))

  fct_names <- names(tabs)[purrr::map_lgl(tabs, is.factor)]
  if (length(fct_names) == 0)
    return(list(degrade = TRUE,
                reason = "the table has no factor column to use as the row variable"))

  # col_vars: robust, position-independent col_var-attribute path (as in tab_get_vars()).
  col_vars <- get_col_var(tabs[fmt_mask]) |> purrr::discard(~ is.na(.))
  col_vars_names  <- unique(col_vars)
  col_vars_levels <- purrr::map(col_vars_names, ~ names(col_vars[col_vars == .])) |>
    purrr::set_names(col_vars_names)

  # Phase 14d: the RECORDED roles first (validated against the real columns); the detection below is
  # the fallback for a table that never recorded them (tab_plain(), a hand-built frame, an older
  # object). row_var = last factor NOT in the grouping; tab_vars = every other factor (column order,
  # so it matches tab_get_vars() exactly). An ungrouped table falls back to the last-factor heuristic.
  rec <- tab_vars_recorded(tabs)
  if (!is.null(rec)) {
    row_var  <- rec$row_var
    tab_vars <- rec$tab_vars
  } else {
    groups    <- intersect(dplyr::group_vars(tabs), fct_names)
    non_group <- setdiff(fct_names, groups)
    row_var   <- if (length(groups) == 0 || length(non_group) == 0) {
      utils::tail(fct_names, 1L)
    } else {
      utils::tail(non_group, 1L)
    }
    tab_vars <- setdiff(fct_names, row_var)
  }

  if (length(row_var) == 0 || is.na(row_var) || !row_var %in% fct_names)
    return(list(degrade = TRUE, reason = "could not identify the row variable"))

  # `row_var` is the COLUMN holding the row labels; `row_vars` is the SOURCE variable name(s) it came
  # from. They differ only on a merged table, where the column is the literal "levels" and the real
  # names live in the `row_var` column's values -- which is what a title or a caption wants to name.
  list(degrade = FALSE, row_var = row_var, tab_vars = tab_vars,
       row_vars = if (!is.null(rec)) rec$row_vars else row_var,
       compacted = !is.null(rec) && isTRUE(rec$compacted),
       col_vars = col_vars_names, col_vars_levels = col_vars_levels)
}


# Phase 10c: the shared "graceful degrade" notice for exporters/print when a table cannot be
# read as a tabxplor table -- render the plain frame (per backend) instead of crashing.
#' @keywords internal
tab_degrade_inform <- function(reason) {
  cli::cli_inform(c(
    "!" = "tabxplor formatting and colors skipped: {reason}.",
    "i" = "Rendering the plain table instead."
  ))
}




# STEP-BY-STEP FUNCTIONS -----------------------------------------------------------------

# Lump factor levels whose (unweighted) count is below `other_if_less_than` into `other_level`.
# Phase 7d-ii: extracted verbatim from tab_prepare() so the internal pipeline and the jmvtab cache
# can run this as a standalone, keyable pre-aggregate step; tab_prepare() still composes it.
# `across(all_of(character()))` is a no-op, so the length guard only short-circuits the common case.
tab_lump_others <- function(data, vars_not_numeric, other_if_less_than = 0,
                            other_level = "Others") {
  if (other_if_less_than > 0 && length(vars_not_numeric) != 0) {
    data <- data |>
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
  if (length(vars_not_numeric) != 0) data <- data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(vars_not_numeric),
      ~ forcats::fct_relabel(., ~ stringi::stri_replace_all_regex(., cleannames_condition(), ""))
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
#' @examples \donttest{data <- dplyr::starwars |>
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
      dplyr::select(data[pos_variables], where(~ !is.numeric(.))) |>
      colnames() #|> rlang::syms()                # is.integer(.) | is.double()

    #Transform characters to factors first ? Time taker.
    # data <- data |>
    #   dplyr::mutate(dplyr::across(
    #     tidyselect::all_of(vars_not_numeric) & where(~ !is.factor(.)),
    #     as.factor
    #   ))

    # Strip the `ordered` class from factors. Pragmatic: ordered factors once triggered an
    # error downstream (likely in MCA / an external step), and dropping the class was the
    # simplest fix. FIXME(future): keep `ordered` instead, to support ordinal-specific
    # behaviours/options — remove this once the downstream error is pinned down.
    data <- data |>
      dplyr::mutate(dplyr::across(
        where(is.ordered),
        ~ `class<-`(., class(.)[class(.) != "ordered"])
      ))

    # Remove unused levels : time taker
    # data <- data |>  #Remove unused levels anyway
    #   dplyr::mutate(dplyr::across(tidyselect::all_of(vars_not_numeric),
    #                               forcats::fct_drop))

    # Phase 7d-ii: rare-level lump + cleannames relabel are now standalone helpers (callable by the
    # jmvtab cache); tab_prepare composes them here in the same lump-then-clean order (byte-identical).
    data <- data |> tab_lump_others(vars_not_numeric, other_if_less_than, other_level)
    if (cleannames == TRUE) data <- data |> tab_cleannames_relabel(vars_not_numeric)

    data
  }







# ============================================================================================
# === Phase 9b-3: the plain carrier (unwrapped fmt columns) ==================================
# ============================================================================================
# The in-build table is carried as plain atomic field-vectors up to a SINGLE new_fmt() call at
# the end of tab_build_one(), instead of materializing the tabxplor_fmt record inside each leaf
# and reconstructing it through every downstream join / slice / rbind (the vctrs ptype2/cast/
# restore round-trip §29 pins at ~99% of tab()). See dev/tabxplor_phase9b_fmt_display_only.md.
#
# A carrier COLUMN = list(frame, meta):
#   frame : named list of the 18 per-cell FIELDS, each length nrow, correctly typed (landmine L1):
#           n/digits integer, in_totrow/in_tottab/in_refrow logical, display character, the 12
#           other numerics double. new_fmt() does NO casting, so the carrier owns the types.
#   meta  : named list of the 9 per-column ATTRIBUTES (type, comp_all, ref, ci_type, col_var,
#           totcol, refcol, color, color_signif). `color` is carried WHOLE (length 1 or 2).
# The field / attribute name order is the new_fmt() contract (fmt_class.R); do NOT reorder. The 18
# per-cell field NAMES are not kept as a constant -- the leaves build the frame inline; only the 9
# per-column ATTRIBUTE names (below) are needed as a shared vector (fmt_unwrap / tab_stack_tables).
fmt_col_attrs    <- c("type", "comp_all", "ref", "ci_type", "col_var",
                      "totcol", "refcol", "color", "color_signif")

# WARNING: pass `comp_all` by EXACT name (not `comp`). The leaves historically wrote `comp = x`,
# which PARTIAL-MATCHES the `comp_all` formal (verified) -- `comp_all = x` is the identical result.
# fmt_materialize_col() -- the ONE new_fmt() call. do.call by exact names => no partial-match drift.
fmt_materialize_col <- function(frame, meta) do.call(new_fmt, c(frame, meta))

# fmt_unwrap() / fmt_wrap() -- the carrier ROUND-TRIP (Phase 9b-4). This is the tests-boundary seam:
# a built table is DECOMPOSED to plain field-vectors so tab_apply_tests() (chi2/ci) can eventually
# read/write them directly (Phase 9b-5) instead of reconstructing the tabxplor_fmt record at every
# step. In 9b-4 the two are composed as a byte-identical no-op (fmt_wrap(fmt_unwrap(x))) that carries
# the table to the seam and validates the round-trip in the real pipeline.
#
# fmt_unwrap(tab) -> a carrier list:
#   is_fmt  : logical over the data columns (rebuild order + fmt/factor split).
#   factors : the non-fmt columns, passed through WHOLE (length-nrow, own attrs kept).
#   fmt     : named list, one entry per fmt column = list(frame, meta) -- frame = as.list(vec_data())
#             (the 18 raw fields, exact types), meta = the 9 fmt_col_attrs read by exact name.
#   attrs   : attributes(tab) VERBATIM (class / names / row.names / subtext / test / groups).
# fmt_wrap(carrier) is the exact inverse: materialize each fmt column via the single
# fmt_materialize_col() seam, pass the factor columns through, restore `attrs` wholesale.
#
# Byte-identity: new_fmt() does NO casting (L1) so vec_data() -> new_fmt() preserves every field's
# storage type; the 9 attrs are read/written by exact name; restoring attributes() reproduces the
# grouped/ungrouped class, subtext and test attribute. Provably identical() to the input (locked by
# test-carrier-parity.R).
fmt_unwrap <- function(tab) {
  cols <- unclass(tab)                                     # the data columns (fmt + factor), by name
  is_f <- vapply(cols, is_fmt, logical(1))
  fmt  <- lapply(cols[is_f], function(col) list(
    frame = as.list(vctrs::vec_data(col)),
    meta  = purrr::set_names(lapply(fmt_col_attrs, function(a) attr(col, a, exact = TRUE)),
                             fmt_col_attrs)
  ))
  list(is_fmt = is_f, factors = cols[!is_f], fmt = fmt, attrs = attributes(tab))
}

fmt_wrap <- function(carrier) {
  cols <- vector("list", length(carrier$is_fmt))
  names(cols) <- names(carrier$is_fmt)
  cols[!carrier$is_fmt] <- carrier$factors
  cols[ carrier$is_fmt] <- lapply(carrier$fmt, function(cc) fmt_materialize_col(cc$frame, cc$meta))
  attributes(cols) <- carrier$attrs                       # class/names/row.names/subtext/test/groups
  cols
}

# fmt_stack_frames() -- Phase 9b-6 (Boundary B): ROW-BIND fmt columns on PLAIN field-frames. `frames`
# is a list of per-source field-frames (each = as.list(vctrs::vec_data(col)), the 18 raw fields);
# concat field-by-field with vctrs::vec_c (type-stable, so L1 holds: int+int -> int, NA_integer_ vs
# NA_real_ preserved) and materialize ONCE via the fmt_materialize_col() seam with the supplied `meta`
# (the 9 fmt_col_attrs). This replaces a vec_rbind over the tabxplor_fmt RECORDS (which casts +
# restores every row of every column) in tab_compact() / tab_pvalue_lines(). The caller supplies `meta`
# per its reconcile policy: tab_compact() = vctrs::vec_ptype_common() across the stacked tables (reuses
# vec_ptype2's attribute reconcile = L3, byte-identical, O(#tables) not O(#rows)); tab_pvalue_lines() =
# the source table's OWN per-column meta (matching the map2_df(vec_restore, tabs) that discarded the
# added p-value row's attrs -- no reconcile).
fmt_stack_frames <- function(frames, meta) {
  frames   <- unname(frames)                     # else vec_c() takes the list names as outer names
  fields   <- names(frames[[1]])
  combined <- purrr::set_names(
    lapply(fields, function(f) do.call(vctrs::vec_c, lapply(frames, `[[`, f))),
    fields)
  fmt_materialize_col(combined, meta)
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
#' @param ref2 The second reference level for odds ratios (or relative risk ratios), needed
#' only for a factor with **3 levels or more** (the "OR of each level versus \code{ref2}"). The
#' first level is used by default. For a **binary** factor \code{ref2} is ignored: each level's
#' OR is computed against the *other* level, so both levels show a value (reciprocals of one
#' another) instead of one being forced to \code{1}. See `ref` above for the list of possible values.
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param OR With `pct = "row"` or `pct = "col"`, calculate and print odds ratios:
#'   for a binary variable the usual odds ratio; for a variable with 3 levels or more,
#'   the odds ratio of each level versus the reference level (the empirical analogue of
#'   the "OR (j vs reference)" from a multinomial [tab_reg()] model).
#'  \itemize{
#'   \item \code{"no"}: by default, no OR are calculated.
#'   \item \code{"OR"}: print OR (instead of percentages).
#'   \item \code{"OR_pct"}: print OR, with percentages in bracket.
#' }
#' Odds ratios don't add up to 100\%, so the total column drops its "100\%" and shows only the base
#' \code{n} (console), exports the base-\code{n} column only, or nothing when \code{add_n = FALSE}.
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
#' data <- dplyr::starwars |> tab_prepare(sex, hair_color)
#'
#' data |>
#'   tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row") |>
#'   tab_chi2() |>
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
                      conf_level = 0.95, stars = FALSE, color_signif = "ignore",
                      .fine = NULL, .by_table = FALSE
) {

  row_var_quo <- rlang::enquo(row_var)
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data |> dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::ensym(row_var)
  }

  col_var_quo <- rlang::enquo(col_var)
  if (quo_miss_na_null_empty_no(col_var_quo)) {
    data <- data |> dplyr::mutate(no_col_var = factor("n"))
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
  #   #data <- data |> dplyr::mutate(no_tab_vars = factor(" "))
  #   tab_vars <- character() #rlang::syms("no_tab_vars")
  # } else {
  #   tab_vars_quo <- rlang::enquos(...)
  #   NA_tab_vars  <- purrr::map(tab_vars_quo,
  #                              ~ is.na(as.character(rlang::get_expr(.)))) |>
  #     purrr::flatten_lgl()
  #   if (all(NA_tab_vars) ) {
  #     #data <- data |> dplyr::mutate(no_tab_vars = factor(" "))
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
  ref <- stringi::stri_trim_both(stringi::stri_replace_all_regex(ref, "\\s+", " "))
  vctrs::vec_assert(ref2, size = 1)
  ref2 <- stringi::stri_trim_both(stringi::stri_replace_all_regex(ref2, "\\s+", " "))
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
    data <- data |>
      dplyr::select(!!!tab_vars, !!row_var, !!col_var, !!wt) |>
      dplyr::mutate(dplyr::across(!!wt & !where(is.numeric), as.numeric)) |>
      # DESIGN: REQUIRED for the direct tab_plain() entry (the public no-total escape hatch).
      # tab_many() also relabels once upstream (~L889), so this is redundant ONLY on the tab()/
      # tab_many() path; the op is idempotent and cheap (post the short-circuit fix, see CLAUDE.md
      # § Discovered bugs). Keep it -- removing it would break a bare tab_plain() call for a
      # negligible perf gain.
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
    colvarbis <- names(tabs)[stringi::stri_detect_regex(names(tabs), "_colvarbis$")]
    data.table::setnames(tabs, colvarbis, stringi::stri_replace_first_regex(colvarbis, "_colvarbis$", ""),
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

  na_rows <- tabs |>
    dplyr::select(!!!tab_vars, !!row_var) |>
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

  num_cols <- tabs |> purrr::map_lgl(is.numeric)
  num_cols <- names(num_cols)[num_cols]

  # Region B (Phase 9d): total-TABLE row(s) via base-R group-sum. "table" = one total row per row_var
  # level (tab_vars set to "Total"); "line" = one grand total row (all tab_row_names "Total").
  if (totaltab %in% c("table", "line")) {
    if (totaltab[1] == "table") { bt_keys <- as.character(row_var); bt_totvars <- as.character(tab_vars) }
    else                        { bt_keys <- character();           bt_totvars <- tab_row_names }
    tabs_totaltab <- build_total_rows(tabs, bt_keys, bt_totvars, tab_row_names, num_cols)
    tabs <- finalize_total_rows(tabs, tabs_totaltab, bt_totvars, tab_row_names)
  }



  # Region C (Phase 9d): total ROWS via base-R group-sum, one build_total_rows() per tab_vars
  # accumulation level (subtable totals + grand total), deduped (identical duplicate rows collapse,
  # order-independent -> the final setorderv dominates), then the totaltab=="line" grand-line drop.
  if ("row" %in% tot) {
    if (length(tab_vars) != 0) {
      group_vars <- rev(purrr::accumulate(as.character(tab_vars) , ~ c(.x, .y)))
      total_vars <- purrr::map(group_vars,
                               ~ c(as.character(tab_vars)[!as.character(tab_vars) %in% .],
                                   as.character(row_var)))
    } else {
      group_vars <- list(character())
      total_vars <- list(as.character(row_var))
    }

    parts    <- purrr::map2(group_vars, total_vars,
                            ~ build_total_rows(tabs, .x, .y, tab_row_names, num_cols))
    tabs_tot <- do.call(rbind, parts)
    tabs_tot <- tabs_tot[do.call(order, tabs_tot[tab_row_names]), , drop = FALSE]
    tabs_tot <- tabs_tot[!duplicated(tabs_tot), , drop = FALSE]

    if (totaltab == "line") {
      keep     <- Reduce(`|`, lapply(tab_row_names, function(v) as.character(tabs_tot[[v]]) != "Total"))
      tabs_tot <- tabs_tot[keep, , drop = FALSE]
    }

    tabs <- finalize_total_rows(tabs, tabs_tot, unique(unlist(total_vars)), tab_row_names)
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
      n_index  <- stringi::stri_detect_regex(names(tabs), "^n_")  | text_vars
      wn_index <- stringi::stri_detect_regex(names(tabs), "^wn_") | text_vars

      text_vars <- text_vars[text_vars]

      tabs_n  <- data.table::setnames(tabs[, n_index, with = FALSE] ,
                                      function(.x) stringi::stri_replace_first_regex(.x, "^n_" , ""))
      tabs_wn <- data.table::setnames(tabs[, wn_index, with = FALSE],
                                      function(.x) stringi::stri_replace_first_regex(.x, "^wn_", ""))

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
    # Phase 9d: percentages + the tot_n base on a numeric matrix (base-R) via leaf_wide_pct(),
    # replacing the copy() + switch(pct) + purrr::map(.SD, ~ ./eval(sym("Total"))) per column.
    # `tot_n` (Phase 2, 1.4.0) = each cell's OWN unweighted percentage base (row / column / grand
    # total, per `pct`), BROADCAST from the UNWEIGHTED tabs_n so the built table is self-sufficient
    # for exact statistics (retires detect_totcols() on built tables, decisions §2, §11). Byte-
    # identical to the former per-cell path (dev/benchmarks/phase9d_leaf_math_parity.R).
    res_e     <- leaf_wide_pct(tabs_n, if (length(wt) == 0) NULL else tabs_wn,
                               pct, as.character(tab_vars), cols)
    tabs_pct  <- res_e$pct
    tabs_totn <- res_e$tot_n


    #Differences and odds ratio
    if (ref != "no" & pct %in% c("row", "col")) {
      # Phase 7f: the reference step is the shared tab_apply_reference() (used verbatim here and by the
      # jmvtab tier-3 re-ref). It returns diff / ratio(=tabs_mean) and, when OR/color needs them, rr /
      # or + the ref-col vector; refrows is the ref-row marker. Assign each only when produced so the
      # downstream exists() guards behave exactly as with the former inline locals.
      # 14z: compute the OR interval only when a colour policy or stars needs it (else a NULL tabs_totn
      # skips it in tab_apply_reference -> no ci_type/bounds change, so existing ignore-OR tables stay
      # byte-identical). color_signif reads the bounds; stars read the (want_p-gated) pvalue.
      or_want_ci <- (OR %in% c("OR", "OR_pct", "or", "or_pct") | color %in% c("or", "OR")) &&
        (!identical(color_signif[1], "ignore") || isTRUE(stars))
      ref_res <- tab_apply_reference(
        tabs = tabs, tabs_pct = tabs_pct, ref = ref, ref2 = ref2, comp = comp, OR = OR,
        color = color, pct = pct, tab_row_names = tab_row_names, tab_vars = tab_vars,
        row_var = row_var, tottab_vector = tottab_vector, totrow_vector = totrow_vector, cols = cols,
        tabs_totn = if (or_want_ci) tabs_totn else NULL, conf_level = conf_level, stars = stars
      )
      tabs_diff <- ref_res$diff
      tabs_mean <- ref_res$ratio
      if (!is.null(ref_res$rr))             tabs_rr        <- ref_res$rr
      if (!is.null(ref_res$or))             tabs_or        <- ref_res$or
      if (!is.null(ref_res$or_ci_inf))      tabs_or_ci_inf <- ref_res$or_ci_inf
      if (!is.null(ref_res$or_ci_sup))      tabs_or_ci_sup <- ref_res$or_ci_sup
      if (!is.null(ref_res$or_pvalue))      tabs_or_pvalue <- ref_res$or_pvalue
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
  if (exists("tabs_or_ci_inf", rlang::current_env(), inherits = F)) tabs_or_ci_inf[, names(text_vars) := NULL]
  if (exists("tabs_or_ci_sup", rlang::current_env(), inherits = F)) tabs_or_ci_sup[, names(text_vars) := NULL]
  if (exists("tabs_or_pvalue", rlang::current_env(), inherits = F)) tabs_or_pvalue[, names(text_vars) := NULL]
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
         if (exists("tabs_totn", rlang::current_env(), inherits = F)) { tabs_totn } else { list(NA_reals) },
         if (exists("tabs_or_ci_inf", rlang::current_env(), inherits = F)) { tabs_or_ci_inf } else { list(NA_reals) },
         if (exists("tabs_or_ci_sup", rlang::current_env(), inherits = F)) { tabs_or_ci_sup } else { list(NA_reals) },
         if (exists("tabs_or_pvalue", rlang::current_env(), inherits = F)) { tabs_or_pvalue } else { list(NA_reals) }
    ) |>
    # Phase 9b-3: build the plain carrier column (frame + meta) then materialize via the single
    # fmt_materialize_col() (== the former inline new_fmt, byte-identical). pmap_dfc is KEPT so the
    # output columns keep their exact col_var-cell names/order. `..6` (tabs_rr) is unused, as before.
    # Phase 5 (§3): `ratio` (= tabs_mean = ..5) is the REFERENCE-RELATIVE ratio (the "x2 rule" / colour
    # ratio measure); `mean` is NA for pct columns (the old mean-overload is gone; colour reads ratio).
    purrr::pmap_dfc(function(...) {
      a <- list(...)
      # 14z: a[[11..13]] carry the empirical-OR interval (all-NA unless a colour policy/stars asked for
      # it). ci_type "or" is set PER COLUMN only where real bounds exist -> the ref2/Total columns (NA'd
      # in tab_apply_reference) and every non-OR table keep ci_type "" byte-for-byte.
      fmt_materialize_col(
        frame = list(
          n         = as.integer(a[[1]]), display = display_1, digits = digits_v,
          wn        = a[[2]], pct = a[[3]], mean = NA_reals, diff = a[[4]], ratio = a[[5]],
          ctr       = NA_reals, var = NA_reals, ci_inf = a[[11]], ci_sup = a[[12]],
          pvalue    = a[[13]], or = a[[7]], tot_n = a[[10]],
          in_totrow = totrow_vector, in_tottab = tottab_vector, in_refrow = refrows),
        meta  = list(
          type      = type_1, comp_all = comp_1, ref = ref_1,
          ci_type   = if (!all(is.na(a[[11]]))) "or" else "", col_var = colvar_1,
          totcol    = a[[8]], refcol = a[[9]], color = color_1, color_signif = "ignore")
      )
    })

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

  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                                    ~ length(unique(.)) == 1))

  if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext) |>
      dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs |> dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext)
  }
}


# leaf_wide_pct() -- Phase 9d: tab_plain()'s Region E (percentages + the tot_n base) on a numeric
# matrix (base-R) instead of copy() + switch(pct) + purrr::map(.SD, ~ ./eval(rlang::sym("Total")))
# per column. `pct` = the value matrix / denominator matrix `D` (row -> the row's Total; col -> the
# tab_vars-group's last (= total) row; all/all_tabs -> that row's / the grand Total), then NA/NaN ->
# 0 (== tidyr::replace_na). `tot_n` = D built on the UNWEIGHTED tabs_n, broadcast (not divided).
# `grp_last <- ave(seq_len(n), grp, max)` reproduces dplyr::last(.) = the group's total row exactly.
# Byte-identical to the former per-cell path (dev/benchmarks/phase9d_leaf_math_parity.R).
#' @keywords internal
#' @noRd
leaf_wide_pct <- function(tabs_n, tabs_wn, pct, tab_vars, cols) {
  nm <- names(cols); n <- nrow(tabs_n); k <- length(nm)
  grp <- if (length(tab_vars) == 0) rep(1L, n) else {
    key <- do.call(paste, c(lapply(tab_vars, function(v) as.character(tabs_n[[v]])), sep = "\r"))
    match(key, unique(key))
  }
  grp_last <- stats::ave(seq_len(n), grp, FUN = max)
  M_pct  <- if (!is.null(tabs_wn)) as.matrix(tabs_wn[, nm, with = FALSE]) else
                                   as.matrix(tabs_n[,  nm, with = FALSE]) * 1.0
  M_totn <- as.matrix(tabs_n[, nm, with = FALSE]) * 1.0
  Dmat <- function(M) switch(pct,
    "row"      = matrix(M[, "Total"],         n, k),
    "col"      = M[grp_last, , drop = FALSE],
    "all"      = matrix(M[grp_last, "Total"], n, k),
    "all_tabs" = matrix(M[n,        "Total"], n, k))
  P <- M_pct / Dmat(M_pct); P[is.na(P)] <- 0
  Tn <- Dmat(M_totn)
  wb <- function(src, M2) {
    dt <- data.table::copy(src)
    dt[, (nm) := lapply(seq_len(k), function(j) M2[, j])]
    dt
  }
  list(pct   = wb(if (!is.null(tabs_wn)) tabs_wn else tabs_n, P),
       tot_n = wb(tabs_n, Tn))
}


# build_total_rows() / finalize_total_rows() -- Phase 9d: tab_plain()'s total-TABLE (Region B) and
# total-ROW (Region C) group-sums via base-R instead of data.table `keyby`. DECISIVE: sum with
# base::sum() per split() group -- NOT rowsum()/data.table-gforce, whose plain-double accumulator
# drifts 1 ULP from the `purrr::map(.SD, sum, na.rm=TRUE)` (long-double accumulator) the old code
# used, breaking identical(). finalize_total_rows() appends the "Total" level to exactly the columns
# that receive it (totvars) before rbind + setorderv, matching data.table's factor-union. Byte-
# identical across 648 shapes (dev/benchmarks/phase9d_leaf_math_parity.R).
#' @keywords internal
#' @noRd
build_total_rows <- function(tabs, keys, totvars, tab_row_names, num_cols) {
  n <- nrow(tabs)
  if (length(keys) == 0) { idx <- list(seq_len(n)); kf <- NULL } else {
    key <- do.call(paste, c(lapply(keys, function(v) as.character(tabs[[v]])), sep = "\r"))
    f   <- factor(key, levels = unique(key))
    idx <- split(seq_len(n), f)
    kf  <- as.data.frame(do.call(rbind, strsplit(levels(f), "\r", fixed = TRUE)),
                         stringsAsFactors = FALSE)
    names(kf) <- keys
  }
  summ <- lapply(num_cols, function(cc) {
    col <- tabs[[cc]]; fv <- if (is.integer(col)) integer(1) else numeric(1)
    vapply(idx, function(ii) sum(col[ii], na.rm = TRUE), fv)
  })
  names(summ) <- num_cols
  lab <- lapply(tab_row_names, function(v)
    if (!is.null(kf) && v %in% names(kf)) kf[[v]] else rep("Total", length(idx)))
  names(lab) <- tab_row_names
  # check.names = FALSE: value-cell / key names carry special chars (e.g. "$25000 or more") that the
  # default as.data.frame() would mangle, breaking the c(tab_row_names, num_cols) reselect below.
  out <- cbind(as.data.frame(lab,  stringsAsFactors = FALSE, check.names = FALSE),
               as.data.frame(summ, stringsAsFactors = FALSE, check.names = FALSE))
  out[, c(tab_row_names, num_cols), drop = FALSE]
}

#' @keywords internal
#' @noRd
finalize_total_rows <- function(tabs, extra, cols_get_total, tab_row_names) {
  for (v in cols_get_total) if (v %in% names(tabs))
    tabs[[v]] <- factor(tabs[[v]], levels = unique(c(levels(tabs[[v]]), "Total")))
  for (v in tab_row_names)
    extra[[v]] <- factor(extra[[v]], levels = levels(tabs[[v]]))
  out <- rbind(tabs, data.table::as.data.table(extra))
  data.table::setorderv(out, tab_row_names)
  out[]
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
                                tab_row_names, tab_vars, row_var, tottab_vector, totrow_vector, cols,
                                tabs_totn = NULL, conf_level = 0.95, stars = FALSE) {
  # Phase 9d: the reference arithmetic (diff = cell - ref, ratio = cell / ref, rr / or) runs on a
  # plain numeric matrix via base-R sweep instead of the former per-cell data.table `:=` +
  # purrr::map_if -- byte-identical, ~100x faster on the isolated block (proven across 648 shapes:
  # dev/benchmarks/phase9d_leaf_math_parity.R). Index/name resolution (calculate_refrows, diff_index)
  # and the RETURN SHAPE (diff/ratio/rr/or frames indexable by col name + `refrows` logical) are
  # unchanged, so tab_plain() and the jmvtab tier-3 re-ref (jmv_tab3_reref) are unaffected.
  nm <- names(cols)
  n  <- nrow(tabs_pct)
  k  <- length(nm)
  P  <- as.matrix(tabs_pct[, nm, with = FALSE]) * 1.0

  tabs_diff <- data.table::copy(tabs_pct)
  tabs_mean <- data.table::copy(tabs_pct)
  refrows   <- NULL

  # write a derived matrix M2 (columns aligned to `nm`) into a data.table's value columns in place
  set_cols <- function(dt, M2) dt[, (nm) := lapply(seq_len(k), function(j) M2[, j])]

  # per-comp-group first reference-row absolute index (NA -> P[NA, ] is an all-NA row, reproducing
  # `x - dplyr::nth(x, replace_na(which(ref_rows)[1], 0))` = x - NA). comp_group = tab_vars (comp
  # "tab") or none (comp "all" / no tab_vars) -- the plain form of the former `by = eval(comp_group)`.
  comp_group <- if (comp == "tab") as.character(tab_vars) else character()
  grp_comp   <- if (length(comp_group) != 0) {
    do.call(paste, c(lapply(comp_group, function(v) as.character(tabs[[v]])), sep = "\r"))
  } else rep(1L, n)
  ref_abs <- function(refr) {
    out <- rep(NA_integer_, n)
    for (rows in split(seq_len(n), grp_comp)) {
      p <- which(refr[rows])[1]
      if (!is.na(p)) out[rows] <- rows[p]
    }
    out
  }

  if (pct == "row") {

    refrows <- tabs |>
      calculate_refrows(ref           = ref,
                        comp          = comp,
                        tab_row_names = tab_row_names,
                        tab_vars      = tab_vars,
                        row_var       = row_var,
                        tottab_vector = tottab_vector,
                        totrow_vector = totrow_vector,
                        num_names     = names(cols)
      )

    ra   <- ref_abs(refrows)
    Pref <- P[ra, , drop = FALSE]
    set_cols(tabs_diff, P - Pref)
    set_cols(tabs_mean, P / Pref)   # with pct, tabs_mean is the *2 rule ratio, not a difference


    # Odds ratio (when pct = "row")
    if (OR %in% c("OR", "OR_pct", "or", "or_pct") | color %in% c("or", "OR")) {

      # Phase 16c: PER-COLUMN reference index. For a BINARY col_var (exactly 2 non-Total level columns)
      # each level's OR is computed against the OTHER level (the two columns are reciprocals, neither is
      # forced to "1", and ref2 is unused). For 3+ levels every column references the single ref2 column
      # (which then shows OR = 1) -- byte-identical to the former `P / P[, refcols]`. `nm == "Total"` is
      # the caller's own convention (tab.R below, pre-rename); tab_plain has ONE factor col_var so the
      # non-Total columns ARE its levels.
      ridx0   <- diff_index(ref2, row_var = dplyr::pull(tabs_pct, !!row_var),
                            num_names = names(cols), pct = "col")
      ok_ref2 <- length(ridx0) != 0 && !is.na(ridx0) && ridx0 >= 1L && ridx0 <= k
      lv      <- which(nm != "Total")
      binary  <- length(lv) == 2L

      if (binary || ok_ref2) {
        ref_col_idx <- rep(if (ok_ref2) as.integer(ridx0) else NA_integer_, k)
        if (binary) { ref_col_idx[lv[1]] <- lv[2]; ref_col_idx[lv[2]] <- lv[1] }
        RR <- P / P[, ref_col_idx, drop = FALSE]
      } else {
        warning(paste0(
          "in ref2 = '", ref2, "' , no columns were found as reference for comparison ; ",
          "to remove this warning, precise the value of ref ",
          "until there is one column matched"
        ))
        ref_col_idx <- rep(NA_integer_, k)
        RR <- matrix(NA_real_, n, k)
      }
      # self-referencing columns show OR = 1 by construction: the ref2 column for 3+ levels, none for binary
      refcols_vector <- !is.na(ref_col_idx) & ref_col_idx == seq_len(k)

      tabs_rr <- data.table::copy(tabs_pct)
      set_cols(tabs_rr, RR)

      # Odds ratio (binary complement, or per-level vs ref2 for 3+ levels) : rr / reference ROW
      tabs_or <- data.table::copy(tabs_pct)
      set_cols(tabs_or, RR / RR[ra, , drop = FALSE])
    }

  }


  if (pct == "col") {
    refcols <- dplyr::nth(names(cols), diff_index(ref,
                                                  num_names = names(cols),
                                                  pct       = pct))
    refcols_vector <- names(cols) == refcols

    if (length(refcols) != 0 & !is.na(refcols)) {
      set_cols(tabs_diff, P - P[, refcols])
      set_cols(tabs_mean, P / P[, refcols])   # *2 rule ratio
    } else {
      warning(paste0(
        "in ref = '", ref, "' , no columns were found as reference for comparison ; ",
        "to remove this warning, precise the value of ref ",
        "until there is one column matched"
      ))
      set_cols(tabs_diff, matrix(NA_real_, n, k))
      set_cols(tabs_mean, matrix(NA_real_, n, k))
    }


    # Odds ratio (when pct = "col")
    if (OR %in% c("OR", "OR_pct", "or", "or_pct") | color %in% c("or", "OR")) {

      # Relative risks : cell / reference ROW
      refrows <- tabs |>
        calculate_refrows(ref           = ref2,
                          comp          = comp,
                          tab_row_names = tab_row_names,
                          tab_vars      = tab_vars,
                          row_var       = row_var,
                          tottab_vector = tottab_vector,
                          totrow_vector = totrow_vector,
                          num_names     = names(cols)
        )
      ra <- ref_abs(refrows)
      RR <- P / P[ra, , drop = FALSE]
      tabs_rr <- data.table::copy(tabs_pct)
      set_cols(tabs_rr, RR)

      # Per-level OR vs the reference COLUMN. Phase 16c: the pct="col" binary-row mirror is deferred
      # (see decisions doc); the reference is the single `refcols` column (level 1 shows OR = 1), as
      # before. `ref_col_idx` (all pointing at the ref column) feeds the shared Woolf block below, keeping
      # the pct="col" interval byte-identical.
      tabs_or <- data.table::copy(tabs_pct)
      if (length(refcols) != 0 & !is.na(refcols)) {
        set_cols(tabs_or, RR / RR[, refcols])
      } else {
        set_cols(tabs_or, matrix(NA_real_, n, k))
      }
      ref_col_idx <- rep(which(refcols_vector)[1], k)
    }
  }

  # 14z: Woolf log-OR Wald interval for the empirical odds ratio, computed ONLY when the caller needs
  # it (a colour policy / stars) -- signalled by a non-NULL `tabs_totn`, the UNWEIGHTED base. The 2x2 is
  # CONDITIONAL on {level j, its reference level} x {row i, ref row `ra`}, built the §14 way (WEIGHTED
  # proportion P x UNWEIGHTED base N, so the totals cancel). Phase 16c: `ref_col_idx` is the PER-COLUMN
  # reference index -- the ref2 column for 3+ levels (that column's OR = 1), the COMPLEMENT level for a
  # binary col_var (both levels get a real, reciprocal interval; only the ref row is NA'd), or the single
  # ref column for pct="col". For 3+ / pct="col" `ref_col_idx` is constant, so B/D are byte-identical to
  # the former single-`ridx` broadcast. The gate now fires whenever the OR was computed (any non-NA
  # reference), not only when a self-referencing `refcols_vector` column exists -- which for binary is
  # empty, so the old gate silently skipped its intervals. ci_or() (R/tab-agg.R) is the shared engine and
  # gives the CI-inversion pvalue (want_p = stars) that duals with the bracket, like the modelled OR.
  or_ci_inf <- or_ci_sup <- or_pvalue <- NULL
  if (!is.null(tabs_totn) && exists("tabs_or", inherits = FALSE) &&
      exists("ref_col_idx", inherits = FALSE) && exists("ra", inherits = FALSE) &&
      !is.null(refrows) && any(!is.na(ref_col_idx))) {
    N  <- as.matrix(tabs_totn[, nm, with = FALSE]) * 1.0
    PN <- P * N
    A  <- PN                                         # a = level j, this row      (positive cell)
    B  <- PN[, ref_col_idx, drop = FALSE]            # b = reference level, this row  (negative cell)
    Cc <- P[ra, , drop = FALSE] * N[ra, , drop = FALSE]   # c = level j, ref row
    D  <- Cc[, ref_col_idx, drop = FALSE]            # d = reference level, ref row
    oc <- ci_or(as.vector(A), as.vector(B), as.vector(Cc), as.vector(D),
                conf_level = conf_level, want_p = isTRUE(stars))
    OINF <- matrix(oc$inf, n, k); OSUP <- matrix(oc$sup, n, k); OPV <- matrix(oc$pvalue, n, k)
    # No interval on a reference position (OR = 1 there by construction): the ref row and any
    # self-referencing column (the ref2/ref column; none for a binary col_var).
    rrm <- !is.na(refrows) & refrows
    OINF[rrm, ] <- NA_real_; OSUP[rrm, ] <- NA_real_; OPV[rrm, ] <- NA_real_
    if (exists("refcols_vector", inherits = FALSE) && any(refcols_vector)) {
      OINF[, refcols_vector] <- NA_real_; OSUP[, refcols_vector] <- NA_real_; OPV[, refcols_vector] <- NA_real_
    }
    or_ci_inf <- data.table::copy(tabs_pct); set_cols(or_ci_inf, OINF)
    or_ci_sup <- data.table::copy(tabs_pct); set_cols(or_ci_sup, OSUP)
    or_pvalue <- data.table::copy(tabs_pct); set_cols(or_pvalue, OPV)
  }

  list(
    diff           = tabs_diff,
    ratio          = tabs_mean,
    rr             = if (exists("tabs_rr",        inherits = FALSE)) tabs_rr        else NULL,
    or             = if (exists("tabs_or",        inherits = FALSE)) tabs_or        else NULL,
    or_ci_inf      = or_ci_inf,
    or_ci_sup      = or_ci_sup,
    or_pvalue      = or_pvalue,
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
#'   grammar (\code{FALSE}/\code{TRUE}, a measure name, or a positional two-channel
#'   \code{c("diff", "ratio")} vector). For numeric means the useful measures are \code{"diff"}
#'   (standardized, Glass's \eqn{\Delta}) and \code{"ratio"} (mean ratio); \code{TRUE} uses
#'   \code{"ratio"}. Default \code{"auto"} keeps the historical behavior.
#' @param color_signif How significance gates the color (\code{"ignore"} / \code{"grey_non_signif"}
#'   / \code{"guaranteed_effect"}) -- see \code{\link{tab}}.
#' @param color_breaks A per-table colour-threshold override -- see \code{\link{tab}}.
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
#' @param method_mean_diff,method_mean_ratio Character strings, the numeric-mean CI methods -- see
#'  \code{\link{tab}}. \code{method_mean_diff}: mean difference (\code{"welch"} / \code{"student"}).
#'  \code{method_mean_ratio}: mean ratio (\code{"robust"} / \code{"quasipoisson"} / \code{"poisson"}).
#' @param ci_scale Character string, the scale the \code{ci = "diff"} interval is expressed on:
#'  \code{"diff"} (default, neutral 0) or \code{"ratio"} (a ratio-of-means interval, neutral 1, stored
#'  as \code{ci_type = "ratio"}). \code{tab()} sets it from the colour (\code{color = "ratio"}).
#' @param stars Logical (opt-in; default \code{FALSE}, or `options("tabxplor.stars")` when \code{NULL}).
#' With \code{ci = "diff"}, print per-cell Welch t significance stars for the difference from the
#' reference row; the mean-diff interval then uses the Welch t quantile (z when \code{FALSE}).
#' @param num Set to \code{TRUE} to obtain a table with normal numeric vectors (not `fmt`).
#' @param df  Set to \code{TRUE} to obtain a plain data.frame (not a `tibble`),
#' with normal numeric vectors (not `fmt`). Useful, for example, to pass the table to
#' correspondence analysis with \pkg{FactoMineR}.
#' @param .fine,.by_table Internal. `.fine` is a pre-computed moment-sum aggregate (from
#' \code{tab_aggregate_num()}) to adopt instead of scanning the raw data; `.by_table` forces
#' the table-by-table path (a fresh scan). Both default to the fresh-scan behaviour.
#' @param .color_deprecate Internal. Set to \code{FALSE} by \code{tab()}'s pipeline to mark
#' \code{color} as an internally-generated legacy string, so re-parsing it raises no deprecation
#' against the user. Not for direct use.
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
#' data <- dplyr::storms |> tab_prepare(category, wind, na_drop_all = wind)
#' tab_num(data, category, wind, tot = "row", color = "after_ci")
#' }
tab_num <- function(data, row_var, col_vars, tab_vars, wt,
                    color = "auto", color_signif = "ignore",
                    na = c("keep", "drop", "drop_fct", "drop_num"),
                    ref = "tot", comp = c("tab", "all"),
                    ci = NULL, conf_level = getOption("tabxplor.conf_level", 0.95), stars = NULL, #ci_visible = FALSE,
                    method_mean_diff = "welch", method_mean_ratio = "robust", ci_scale = "diff",
                    totaltab = "line", totaltab_name = "Ensemble",
                    tot = NULL, total_names = "Total",
                    subtext = "", digits = 0, num = FALSE, df = FALSE,
                    color_breaks = NULL,
                    .fine = NULL, .by_table = FALSE, .color_deprecate = TRUE
) {

  row_var_quo <- rlang::enquo(row_var)
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data |> dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::ensym(row_var)
  }

  col_vars <- rlang::enquo(col_vars)
  if (quo_miss_na_null_empty_no(col_vars)) {
    data     <- data |> dplyr::mutate(no_col_var = factor("n"))
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
  # `.color_deprecate = FALSE` (set only by tab_transform()) marks `color` as the pipeline's own
  # legacy string, so re-parsing it here does not raise a deprecation against the user.
  color_spec <- normalize_color_spec(color, color_signif, deprecate = .color_deprecate)
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

  # Last Phase a bug-fix: the weight name captured OUTSIDE any data.table `j`, so the mean-direct
  # (df/num) blocks below reference it as a plain string. data.table `j` exposes columns as variables,
  # so a column named "wt" would shadow the `wt` ARGUMENT (see num_moment_scan's WARNING); wt_name +
  # get(wt_name) + `[, (wt_name) := NULL]` (dynamic drop, was the literal `wt`) are shadow-proof.
  wt_name <- as.character(wt)

  if (use_raw) {
    data <- data |>
      dplyr::select(!!!tab_vars, !!row_var, !!!col_vars, !!wt) |>
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
                             names(.SD) != wt_name,
                             ~ round(stats::weighted.mean(., get(wt_name), na.rm = TRUE), 10),
                             .else = ~ NA_real_),
             .SDcols = as.character(c(col_vars, wt)),
             keyby = c(tab_row_names)][, (wt_name) := NULL]
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
                                       names(.SD) != wt_name,
                                       ~ round(stats::weighted.mean(., get(wt_name), na.rm = TRUE), 10),
                                       .else = ~ NA_real_)),
                     .SDcols = as.character(c(col_vars, wt)),
                     keyby = eval(.)][, (wt_name) := NULL][ , as.character(tab_vars) := purrr::map(.SD, as.factor),
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
                                 names(.SD) != wt_name,
                                 ~ round(stats::weighted.mean(., get(wt_name), na.rm = TRUE), 10),
                                 .else = ~ NA_real_)),
               .SDcols = as.character(c(col_vars, wt)),
               keyby = eval(as.character(row_var))][, (wt_name) := NULL]
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

    index <- which(stringi::stri_detect_regex(row_var, ref))

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
      stars_on <- if (is.null(stars)) getOption("tabxplor.stars", FALSE) else stars
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
          # Rule B (14v-ii, §48): a mean cell interval estimates the variance -> one-sample Student
          # t(n-1), not z (df = Inf). z was a large-sample approximation; t is the textbook cell CI.
          res <- ci_pivot(m, sqrt(vv / nn), df = nn - 1, conf_level = conf_level, want_p = FALSE)
        } else {
          mr <- tabs[[paste0(v, "_refm")]]
          vr <- tabs[[paste0(v, "_refv")]]
          nr <- tabs[[paste0(v, "_refn")]]
          # 14v-ii: the mean interval follows the measure the reader sees (ci_scale). "ratio" -> a real
          # ratio-of-means CI (method_mean_ratio); else the mean-DIFFERENCE CI (method_mean_diff). The
          # old path always used the difference bounds, so a ratio-coloured mean showed the diff CI
          # mislabelled as a ratio (decisions §48).
          res <- if (identical(ci_scale[1], "ratio"))
            ci_mean_ratio(m, vv, nn, mr, vr, nr, method = method_mean_ratio,
                          conf_level = conf_level, want_p = want_p)
          else
            ci_mean_diff2(m, vv, nn, mr, vr, nr, method = method_mean_diff,
                          conf_level = conf_level, want_p = want_p)
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
  w2_cols <- names(tabs)[stringi::stri_detect_regex(names(tabs), "_w2$")]
  if (length(w2_cols) > 0) data.table::set(tabs, j = w2_cols, value = NULL)





  #Make the final table with fmt vectors
  # remove(list = c("tabs_n", "tabs_wn", "tabs_pct", "tabs_diff", "tabs_ci", "refcols_vector", "refrows"))

  text_vars <- !purrr::map_lgl(tabs, is.numeric)
  NA_reals <- rep(NA_real_, nrow(tabs))

  #n <- as.integer(tabs[["n"]])
  #wn <- if ("wn" %in% names(tabs)) { tabs[["wn"]] } else { NA_reals }

  tabs_n  <-
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_n$"), with = FALSE] ,
                         function(.x) stringi::stri_replace_first_regex(.x, "_n$" , ""))

  tabs_wn  <-
    if (length(wt) != 0) {
      data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_wn$"), with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_wn$" , ""))
    } else {
      list(NA_reals)
    }

  tabs_mean  <-
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_mean$"), with = FALSE] ,
                         function(.x) stringi::stri_replace_first_regex(.x, "_mean$" , ""))

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
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_var$") &
                                names(tabs) != "no_row_var",
                              with = FALSE],
                         function(.x) stringi::stri_replace_first_regex(.x, "_var$" , ""))


  are_diff <- stringi::stri_detect_regex(names(tabs), "_diff$")
  tabs_diff  <-
    if (any(are_diff)) {
      data.table::setnames(tabs[, are_diff, with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_diff$" , ""))
    } else {
      list(NA_reals)
    }

  are_ratio <- stringi::stri_detect_regex(names(tabs), "_ratio$")
  tabs_ratio  <-
    if (any(are_ratio)) {
      data.table::setnames(tabs[, are_ratio, with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_ratio$" , ""))
    } else {
      list(NA_reals)
    }

  # Phase 3a: reshape the real CI bounds + per-cell pvalue (were a single symmetric half-width).
  reshape_suffix <- function(sfx) {
    hit <- stringi::stri_detect_regex(names(tabs), paste0(sfx, "$"))
    if (any(hit)) {
      data.table::setnames(tabs[, hit, with = FALSE],
                           function(.x) stringi::stri_replace_first_regex(.x, paste0(sfx, "$"), ""))
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
    # Phase 9b-3: build the plain carrier column (frame + meta) then materialize via the single
    # fmt_materialize_col() (== the former inline new_fmt, byte-identical). The digits mean-magnitude
    # floor stays per-column (reads this column's means ..3). Phase 3a: real asymmetric CI bounds +
    # per-cell significance (mean CIs symmetric around the estimate, stored as absolute bounds).
    purrr::pmap_dfc(function(...) {
      a <- list(...)
      m <- max(a[[3]], na.rm = TRUE)
      digits_col <-
        if      (m <= 1 ) vec_recycle(max(a[[8]], 2L), length(a[[1]]))
        else if (m <= 10) vec_recycle(max(a[[8]], 1L), length(a[[1]]))
        else              vec_recycle(a[[8]],          length(a[[1]]))
      fmt_materialize_col(
        frame = list(
          n         = a[[1]], display = display_1, digits = digits_col,
          wn        = a[[2]], pct = NA_reals, mean = a[[3]], diff = a[[5]], ratio = a[[9]],
          ctr       = NA_reals, var = a[[4]], ci_inf = a[[10]], ci_sup = a[[6]],
          pvalue    = a[[11]], or = NA_reals, tot_n = NA_reals,
          in_totrow = totrow_vector, in_tottab = tottab_vector, in_refrow = refrows),
        meta  = list(
          # 14v-ii: a ratio-scale mean interval is stored as ci_type = "ratio" (neutral 1), so
          # ci_center()/format()/the colour gate read the ratio bounds, not a diff mislabelled ratio.
          type      = "mean", comp_all = comp_1, ref = ref_1,
          ci_type   = if (identical(ci_scale[1], "ratio") && ci == "diff") "ratio" else ci,
          col_var   = a[[7]],
          totcol    = FALSE, refcol = FALSE, color = color, color_signif = "ignore")
      )
    })

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







  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                                    ~ length(unique(.)) == 1))

  result <- if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext) |>
      dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs |> dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext)
  }

  # Phase 5: set the final two-channel colour / significance-policy attributes (a no-op for a
  # plain scalar colour passed straight through, e.g. when tab_many() drives tab_num()).
  result <- finalize_color_spec(result, color_spec)
  set_color_breaks_attr(result, resolve_color_breaks_arg(color_breaks))
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
#' @examples \donttest{ data <- dplyr::starwars |>
#' tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'             na_drop_all = sex)
#'
#' data |>
#'   tab_plain(sex, hair_color, gender) |>
#'   tab_totaltab("line")
#'   }
tab_totaltab <- function(tabs, totaltab = c("table", "line", "no"),
                         name = "Ensemble", data = NULL) {
  #.Deprecated("tab_plain() and tab_num(), which now have a totaltab argument")

  get_vars  <- tab_get_vars(tabs)

  row_var   <- rlang::sym(get_vars$row_var)
  tab_vars  <- rlang::syms(get_vars$tab_vars)
  mean_vars <- (get_type(tabs) == "mean") |> purrr::keep(\(x) x) |> names()


  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  if (length(tab_vars) == 0) return(tabs)

  #Remove the existing total table if there is one
  tottab_rows <- is_tottab(tabs)
  if (any(tottab_rows)) tabs <- tabs |>
    tibble::add_column(tottab = tottab_rows) |>
    dplyr::filter(!.data$tottab) |> dplyr::select(-"tottab")

  if (totaltab[1] == "no") return(tabs)

  #Calculate the total table
  totaltable <- switch(
    totaltab[1],
    "table" = tibble::as_tibble(tabs) |>
      (\(d) tibble::add_column(d, totrow = is_totrow(d)))() |>
      dplyr::filter(!.data$totrow) |> dplyr::select(-"totrow") |>
      dplyr::group_by(!!row_var) |>
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_tottab(sum(.) ))),

    "line"  = tibble::as_tibble(tabs) |>
      (\(d) tibble::add_column(d, totrow = is_totrow(d)))() |>
      dplyr::filter(!.data$totrow) |> dplyr::select(-"totrow") |>
      dplyr::group_by(!!row_var) |>
      dplyr::summarise(dplyr::across(where(is_fmt), sum)) |>
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(as_tottab(sum(.))))) |>
      dplyr::mutate(!!row_var := paste("TOTAL", stringi::stri_trans_toupper(name)))
  )

  if (totaltab[1] == "line") {
    tabs <- tabs |>
      dplyr::mutate(!!row_var := forcats::fct_expand(
        !!row_var,
        levels(dplyr::pull(totaltable, !!row_var))
      ))

    totaltable <- totaltable |>
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
                                                           "line"  =  "no_row_var") ) ) |>
      dplyr::select(-tidyselect::starts_with("no_row_var")) |>
      dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(.)))

    if (totaltab[1] == "line") mean_calc <- mean_calc |>
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
    tabs |> dplyr::bind_rows(totaltable)
  } else {

    df <- tabs |> dplyr::bind_rows(totaltable)
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
#' @examples \donttest{data <- dplyr::starwars |> tab_prepare(sex, hair_color)
#'
#' data |>
#'   tab_plain(sex, hair_color) |>
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
  col_vars_levels <- purrr::discard(col_vars_levels_mean, names(col_vars_levels_mean) %in% names(mean_vars))
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

    if (any(totrows)) tabs <- tabs |>
      tibble::add_column(totrows, tottab_line) |>
      dplyr::filter(!.data$totrows | .data$tottab_line) |>
      dplyr::select(-"totrows", -"tottab_line")
  }

  if ("col" %in% tot | tot[1] == "no") tabs <- tabs |>
    dplyr::select(-where(is_totcol))

  if (tot[1] == "no") return(tabs)


  # Total rows
  if ("row" %in% tot) {
    totrows     <- is_totrow(tabs)
    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows #& totrows

    tabs <- tabs |> tibble::add_column(tottab_rows, tottab_line)

    if (length(groups) != 0) {
      group_vars_totals <-
        dplyr::group_keys(dplyr::filter(tabs, !.data$tottab_line)) |> #dplyr::mutate(bis = PR0) |>
        tidyr::unite(!!row_var, sep = " / ") |>
        dplyr::mutate(!!row_var := paste(name[1], !!row_var) |>
                        stringi::stri_trans_toupper() |> forcats::as_factor())  #stringi::stri_replace_all_regex(, "")
    } else {
      group_vars_totals <- tibble::tibble(!!row_var := factor(name[1]))
    }
    group_vars_totals_levels <- group_vars_totals |> dplyr::pull(1) |> levels()

    tabs <- tabs |>
      dplyr::mutate(!!row_var := forcats::fct_expand(!!row_var, group_vars_totals_levels))

    row_var_levels <- dplyr::pull(tabs, !!row_var) |> levels()

    totrows <- tabs |> dplyr::filter(!.data$tottab_line) |>
      dplyr::summarise(dplyr::across(where(is_fmt), ~ as_totrow(sum(.)) ),
                       .groups = "drop") |>
      dplyr::bind_cols(group_vars_totals) |>
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
        ) ) |>
        dplyr::select(-tidyselect::contains("no_row_var")) |>
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
                        ~ dplyr::full_join(.x, .y ,by = character() ) ) |>
          dplyr::select(-tidyselect::starts_with("no_row_var")) |>
          dplyr::mutate(dplyr::across(where(is_fmt), ~ as_tottab(as_totrow(.))))

        general_totrow  <- dplyr::group_keys(tabs) |>
          dplyr::slice(dplyr::n_groups(tabs)) |>
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


    tabs <- dplyr::bind_rows(tabs, totrows) |>
      dplyr::arrange(.by_group = TRUE) |>
      dplyr::select(-"tottab_line", -"tottab_rows")
  }


  #Total columns
  if ("col" %in% tot) {
    col_vars_2levels_or_more <-
      col_vars_levels[purrr::map_int(col_vars_levels, length) >= 2]

    if (length(col_vars_2levels_or_more) != 0 | totcol[1] == "last") {
      tabs <- tabs |> dplyr::rowwise()

      if (totcol[1] == "last") {
        # c_across don't work. Workaround with quosures : sum(!!!col_vars_levels)
        tabs <- tabs |>
          dplyr::mutate(
            !!rlang::sym(name[2]) :=
              sum(!!!col_vars_levels[[length(col_vars_levels)]]) |>
              as_totcol() |> set_col_var("all_col_vars"))

      } else if (totcol[1] == "each") {
        totcol_names <- purrr::map(paste0(name[2],"_",
                                          names(col_vars_2levels_or_more)),
                                   rlang::sym)
        tabs <-
          purrr::reduce2(col_vars_2levels_or_more, totcol_names, .init = tabs,
                         function(.tab, .levels, .names)
                           dplyr::mutate(.tab, !!.names := sum(!!!.levels) |>
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

      tabs <- tabs |> dplyr::group_by(!!!rlang::syms(groups))
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
  col_vars_no_all  <- col_vars_with_all |> purrr::discard(\(s) as.character(s) == "all_col_vars")
  col_means  <- (get_type(tabs) == "mean") |> purrr::keep(\(x) x) |> names()
  # col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)
  tab_vars         <- rlang::syms(get_vars$tab_vars)

  groups  <- dplyr::group_vars(tabs)
  subtext <- get_subtext(tabs)
  chi2    <- get_chi2(tabs)

  pct <- vctrs::vec_recycle(pct, length(col_vars_no_all)) |>
    purrr::set_names(col_vars_no_all)
  pct[col_means] <- "no"

  if (just_diff == FALSE) {

    if (all(pct == "no")) {
      tabs <- tabs |> dplyr::mutate(dplyr::across(
        where(~ get_type(.) %in% c("row", "col", "all", "all_tabs")),
        ~ set_pct(., NA_real_) |> set_type("n") |>
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
          tabs <- tabs |> tab_totaltab('line')
        }
        tabs <- tabs |>
          dplyr::with_groups(NULL, ~ tab_match_groups_and_totrows(.) |>
                               tab_add_totcol_if_no()
          )
      }
    }

    if ( any(pct %in% c("col", "all") ) | (any(pct == "row") & ref[1] == "tot") ) {
      tabs <- tabs |> tab_match_groups_and_totrows()
    }

    if ( any(pct %in% c("row", "all")) | (any(pct == "col") & ref[1] == "tot") ) {
      tabs <- tabs |> tab_add_totcol_if_no()
    }

    comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
    tabs <- tabs |> tab_match_comp_and_tottab(comp)

    if (any(pct != "no")){
      pct <- c(pct, all_col_vars = dplyr::last(pct[pct != "no"]))
      pct <- purrr::map_chr(tabs, ~ pct[get_col_var(.)] ) |>
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
        pct_nat <- pct |> stringi::stri_replace_first_regex("all_tabs", "no") |>
          purrr::set_names(names(pct))

        tabs <- tabs |>
          dplyr::mutate(dplyr::across(
            where(~ is_fmt(.) & !get_type(.) == "mean"),
            ~ set_pct(., pct_formula(
              .,
              pct = pct_nat[[dplyr::cur_column()]],
              tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
            )) |>
              set_display(ifelse(pct_nat[[dplyr::cur_column()]] != "no", "pct", "wn")) |>
              set_type(pct_nat[[dplyr::cur_column()]])
          ))
      }

      if (any(pct == "all_tabs")) {
        tabs <- tabs |>
          dplyr::with_groups(
            NULL,
            ~ dplyr::mutate(., dplyr::across(
              tidyselect::all_of(all_tabs_pct),
              ~ set_pct(., pct_formula(
                .,
                pct = "all_tabs",
                tot = rlang::eval_tidy(tot_cols[[dplyr::cur_column()]])
              )) |>
                set_display("pct") |> set_type("all_tabs")
            ))
          )
      }

      #Set digits if provided. Always zero digits for the 100% cells
      if (!is.null(digits)) {
        digits <- vctrs::vec_recycle(digits, length(col_vars_with_all)) |>
          purrr::set_names(col_vars_with_all)
        digits <- c(digits, all_col_vars = dplyr::last(digits[!is.na(digits)]))
        digits <- purrr::map_dbl(tabs, ~ digits[get_col_var(.)] )
        digits[pct == "no"] <- NA_real_

        digits_cols <- names(digits)[!is.na(digits)]

        tabs <- tabs |> dplyr::mutate(dplyr::across(
          tidyselect::all_of(digits_cols),
          ~ set_digits(., as.integer(digits[[dplyr::cur_column()]])) ))
      }

      if (length(row_pct     ) != 0) tabs <- tabs |> dplyr::mutate(dplyr::across(
        where(is_totcol) & tidyselect::all_of(row_pct), ~ set_digits(., 0L)))
      if (length(col_pct     ) != 0) tabs <- tabs |> dplyr::mutate(dplyr::across(
        tidyselect::all_of(col_pct),
        ~ dplyr::if_else(is_totrow(.), set_digits(., 0L), .)))
      if (length(all_pct     ) != 0) tabs <- tabs |> dplyr::mutate(dplyr::across(
        where(is_totcol) & tidyselect::all_of(all_pct),
        ~ dplyr::if_else(is_totrow(.), set_digits(., 0L), .)))
      if (length(all_tabs_pct) != 0) tabs <- dplyr::ungroup(tabs) |>
        dplyr::mutate(dplyr::across(
          where(is_totcol) & tidyselect::all_of(all_tabs_pct),
          ~ dplyr::if_else(dplyr::row_number()==dplyr::n(), set_digits(., 0L), .))) |>
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
      reference_cols <- purrr::map_chr(reference, as.character) |> unique()
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
      tabs <- tabs |>
        dplyr::with_groups(
          NULL,
          ~ dplyr::mutate(., dplyr::across(
            where(~ get_type(.) %in% c("row", "col", "mean")),
            ~ set_diff(., diff_formula(
              .,
              type = type[[dplyr::cur_column()]],
              ref = ref[1],
              refer  = rlang::eval_tidy(reference[[dplyr::cur_column()]])
            )) |> set_diff_type(ref[1])
          ))
        )

    } else {
      tabs <- tabs |>
        dplyr::mutate(dplyr::across(
          where(~ get_type(.) %in% c("row", "col", "mean") ) &
            !( where(is_totcol) &
                 tidyselect::any_of(names(reference)[reference == ""]) ),
          ~ set_diff(., diff_formula(
            .,
            type = type[[dplyr::cur_column()]],
            ref = ref[1],
            refer = rlang::eval_tidy(reference[[dplyr::cur_column()]])
          )) |> set_diff_type(ref[1])
        ))
    }

    if ( any(type %in% c("row", "mean")) ) tabs <- tabs |>
        dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., comp[1] == "all")))

    if (color == TRUE) {
      tabs <- tabs |>
        dplyr::mutate(dplyr::across(
          where(is_fmt),
          ~ set_color(., ifelse(
            type[[dplyr::cur_column()]] %in% c("row", "col", "mean"),
            "diff",
            get_color(.)
          )) ))
    }
  }

  tabs <- tabs |> dplyr::select(-tidyselect::any_of("totrow_groups"))

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
#' @param stars Logical (opt-in; default \code{FALSE}, or `options("tabxplor.stars")` when \code{NULL}).
#' With \code{ci = "diff"}, store and print per-cell significance stars for the difference from
#' the reference, read from the same interval that is displayed (universal CI-inclusion), so the
#' stars and the bracket never disagree. \code{FALSE} skips the significance computation.
#' @param method_cell Character string, the proportion CI method for \code{ci = "cell"}: either
#' \code{"wilson"} (the score interval, default) or \code{"wald"} (the normal approximation).
#' @param method_diff Character string, the proportion CI method for \code{ci = "diff"}: one of
#' \code{"newcombe"} (default, hybrid-score, dual of the two-proportion score test), \code{"ac"}
#' (Agresti-Caffo) or \code{"wald"}. Whatever the method, the stars come from that interval.
#' It selects among the \emph{difference} methods only -- see \code{ci_scale}.
#' @param method_ratio,method_mean_diff,method_mean_ratio Character strings, the ratio / numeric-mean
#' CI methods -- see \code{\link{tab}}. \code{method_ratio}: proportion ratio (\code{"katz"}).
#' \code{method_mean_diff}: mean difference (\code{"welch"} / \code{"student"}). \code{method_mean_ratio}:
#' mean ratio (\code{"robust"} / \code{"quasipoisson"} / \code{"poisson"}).
#' @param ci_scale Character string, the scale the \code{ci = "diff"} interval is expressed on:
#' \code{"diff"} (default) for a difference interval (neutral 0, one of the \code{method_diff}
#' methods), or \code{"ratio"} for a ratio interval (neutral 1), stored as \code{ci_type = "ratio"} and
#' centred on the cell/reference ratio -- Katz's log-risk-ratio for proportions (\code{method_ratio}),
#' or a ratio-of-means interval for numeric means (\code{method_mean_ratio}). \code{tab()} sets it from
#' the colour: the measure the reader sees owns the interval, so \code{color = "ratio"} (or
#' \code{c("ratio", "diff")}) asks for the ratio one.
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
#' data <- dplyr::starwars |>
#'   tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'               na_drop_all = sex)
#'
#' data |>
#'   tab_plain(sex, hair_color, gender, tot = c("row", "col"),
#'     pct = "row", comp = "all") |>
#'     tab_ci("diff", color = "after_ci")
#'   }
tab_ci <- function(tabs,
                   ci = "auto",
                   comp = NULL,
                   conf_level = getOption("tabxplor.conf_level", 0.95),
                   color = "no",
                   visible = FALSE,
                   stars = NULL,
                   method_cell = "wilson", method_diff = "newcombe",
                   method_ratio = "katz", method_mean_diff = "welch",
                   method_mean_ratio = "robust",
                   ci_scale = "diff") {
  stopifnot(all(ci %in% c("auto", "cell", "diff", "no")), #"r_to_r", "c_to_c", "tab_to_tab",
            all(ci_scale %in% c("diff", "ratio")),
            all(comp %in%  c("tab", "all")),
            all(method_cell %in% c("wilson", "wald")),
            all(method_diff %in% c("newcombe", "ac", "wald")),
            all(method_ratio %in% "katz"),
            all(method_mean_diff %in% c("welch", "student")),
            all(method_mean_ratio %in% c("robust", "quasipoisson", "poisson"))
  )
  # Phase 3a: significance stars default (universal CI-inclusion). NULL -> option default.
  stars <- if (is.null(stars)) getOption("tabxplor.stars", FALSE) else stars

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
  col_vars_no_all   <- col_vars_with_all |> purrr::discard(\(s) as.character(s) == "all_col_vars")

  fmtc <- purrr::map_lgl(tabs, is_fmt)
  ci <- vctrs::vec_recycle(ci, length(col_vars_no_all)) |>
    purrr::set_names(col_vars_no_all)
  ci <- c(ci, all_col_vars = dplyr::last(ci[ci != "no"]))
  ci <- purrr::map_chr(tabs, ~ ci[get_col_var(.)] ) |>
    tidyr::replace_na(NA_character_)

  visible <- vctrs::vec_recycle(visible, length(col_vars_no_all)) |>
    purrr::set_names(col_vars_no_all)
  visible <- c(visible, all_col_vars = dplyr::last(visible[visible != "no"]))
  visible <- purrr::map_lgl(tabs, ~ visible[get_col_var(.)] ) |>
    tidyr::replace_na(FALSE)


  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs |> tab_match_comp_and_tottab(comp)

  type <- get_type(tabs)
  tot_cols <- detect_totcols(tabs)
  tot_cols[is.na(ci)] <- list(rlang::sym(""))
  names_totcols <- tot_cols |> purrr::map_chr(as.character) |> unique() |>
    purrr::discard(\(s) s == "")

  ref <- get_ref_type(tabs)
  # Phase 7g-iii: the diff-CI reference column must match the diff/colour reference column
  # (detect_refcol = the marked refcol, falling back to the first level -> byte-identical for
  # ref = "first"; ref = "tot" uses tot_cols below, so detect_refcol is not consulted there).
  ref_cols  <- detect_refcol(tabs)
  ref_cols[is.na(ci)] <- list(rlang::sym(""))

  ref_cols <- dplyr::if_else(ref == "tot",
                             true  = tot_cols,
                             false = ref_cols     ) |>
    purrr::set_names(names(ref)) #keep ci_yes ?
  names_refcols <- ref_cols |> purrr::map_chr(as.character) |> unique() |>
    purrr::discard(\(s) s == "")

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
  ci_with_ref <- ci |> purrr::set_names(names(tabs))
  ci <- dplyr::if_else(
    condition = (ci == "diff_col" & names(tabs) %in% names_refcols) |
      (ci == "diff_col" & get_col_var(tabs) == "all_col_vars") |
      (ci == "diff_row" & names(tabs) %in% names_totcols),
    true = "no",
    false = ci
  )
  ci <- ci |> purrr::set_names(names(tabs))
  ci_yes <- !is.na(ci) & ! ci == "no"


  if (any(ci_yes)) {
    #Ready table for percentages (needed totals, compatible grouping)
    if ( any(ci == "diff_col" ) ) tabs <- tabs |> tab_add_totcol_if_no()
    if ( any(ci == "diff_row") ) {
      tabs <- switch(comp[1],
                     "tab" = tabs |> tab_match_groups_and_totrows(),
                     "all" = tabs |> dplyr::ungroup()               )
    }

    # Phase 9b-5 increment 2: reference-row selection + reference stats on PLAIN fields, replacing the
    # ref_rows/tot_rows/ref_to_na grouped transmutes and the x_n/ref/ref_var/ref_n transmutes (each a
    # reconstruction over the fmt columns). Per SUBTABLE, group_last_pos(mask) = the ABSOLUTE index of
    # the group's last masked row, broadcast to that group (NA if none) -- the plain form of
    # `.[dplyr::last(which(<mask>))]` under grouping. The old `tot_rows` was DEAD (computed, never read).
    ci_cols   <- names(ci_yes)[ci_yes]
    diff_cols <- names(ci_yes)[ci %in% c("diff_row", "diff_col")]
    mean_cols <- names(ci_yes)[ci == "diff_row" & type == "mean"]

    gid  <- dplyr::group_indices(tabs)
    gids <- unique(gid)
    group_last_pos <- function(mask) {
      pos <- rep(NA_integer_, length(mask))
      for (g in gids) {
        r <- which(gid == g); w <- which(mask[r])
        if (length(w)) pos[r] <- r[[w[[length(w)]]]]
      }
      pos
    }
    # the reference row per cell = last total row (ref = "tot") else last is_refrow row.
    ref_mask <- function(col) if (identical(get_ref_type(col), "tot")) is_totrow(col) else is_refrow(col)

    empty <- stats::setNames(vector("list", length(ci_cols)), ci_cols)
    x_n <- ref <- ref_var <- ref_n <- ci_inf <- ci_sup <- pvalue <- empty
    for (nm in ci_cols) {
      col <- tabs[[nm]]
      tp  <- get_type(col)
      rp  <- group_last_pos(ref_mask(col))                     # per-row reference-row index (NA if none)
      rtona <- !is.na(rp) & (seq_along(rp) == rp)              # ref_to_na: the cell's own reference row
      # Phase 6h: each cell's OWN unweighted base (tot_n for proportions, n for means); NA on the
      # reference cell so its own CI is not computed.
      x_n[[nm]] <- dplyr::if_else(
        rtona, NA_integer_,
        switch(tp, "col" = get_tot_n(col), "row" = get_tot_n(col), "mean" = get_n(col)))
      if (nm %in% diff_cols) {
        if (ci[[nm]] == "diff_col") {
          rcol        <- tabs[[as.character(ref_cols[[nm]])]]  # the reference COLUMN (its own base)
          ref[[nm]]   <- get_pct(rcol)
          ref_n[[nm]] <- get_tot_n(rcol)[group_last_pos(is_totrow(col))]
        } else {                                               # diff_row: the reference ROW cell
          ref[[nm]]   <- if (tp == "mean") get_mean(col)[rp] else get_pct(col)[rp]
          ref_n[[nm]] <- if (tp == "mean") get_n(col)[rp]     else get_tot_n(col)[rp]
        }
        if (nm %in% mean_cols) ref_var[[nm]] <- get_var(col)[rp]
      }

      # Confidence interval + per-cell pvalue via the closed-form engine (R/tab-agg.R). Weighted rule
      # (§14): weighted proportion get_pct() / weighted mean get_mean(), UNWEIGHTED base x_n. Cell CIs
      # carry no pvalue; diff CIs star only when `stars` is on (want_p). The reference cell has
      # x_n = NA (rtona) -> NA bounds, so it is never self-compared.
      want_p <- isTRUE(stars) && ci[[nm]] %in% c("diff_row", "diff_col")
      res <- switch(
        ci[[nm]],
        "cell" = switch(
          tp,
          # Rule B (14v-ii, §48): one-sample Student t(n-1) cell interval (variance is estimated).
          "mean" = ci_pivot(get_mean(col), sqrt(get_var(col) / x_n[[nm]]),
                            df = x_n[[nm]] - 1, conf_level = conf_level, want_p = FALSE),
          # Phase 7g: the proportion cell CI honours method_cell (default wilson; wald opt-in).
          switch(method_cell,
                 "wilson" = ci_wilson(get_pct(col), x_n[[nm]], conf_level = conf_level),
                 "wald"   = ci_wald(  get_pct(col), x_n[[nm]], conf_level = conf_level))),
        "diff_col" = ,
        "diff_row" = switch(
          tp,
          # 14v-ii: a MEAN's interval now also follows the measure the reader sees (ci_scale). "ratio"
          # -> a real ratio-of-means CI (ci_mean_ratio, method_mean_ratio: robust / quasipoisson /
          # poisson); else the mean-DIFFERENCE CI (method_mean_diff: welch / student). Was a plain
          # ci_mean_diff2 whatever the colour, which showed the diff bounds mislabelled as a ratio.
          "mean" = if (identical(ci_scale[1], "ratio"))
            ci_mean_ratio(get_mean(col), get_var(col), x_n[[nm]],
                          ref[[nm]], ref_var[[nm]], ref_n[[nm]], method = method_mean_ratio,
                          conf_level = conf_level, want_p = want_p)
          else
            ci_mean_diff2(get_mean(col), get_var(col), x_n[[nm]],
                          ref[[nm]], ref_var[[nm]], ref_n[[nm]], method = method_mean_diff,
                          conf_level = conf_level, want_p = want_p),
          # Proportions: the interval follows the measure the reader sees (ci_scale, resolved once in
          # tab_resolve_settings()). "ratio" -> Katz log-RR bounds on the ratio scale (method_ratio,
          # currently only "katz"); else the difference methods. `method_diff` selects among the
          # DIFFERENCE approximations only.
          if (identical(ci_scale[1], "ratio"))
            switch(method_ratio,
                   "katz" = ci_katz_rr(get_pct(col), x_n[[nm]], ref[[nm]], ref_n[[nm]],
                                       conf_level = conf_level, want_p = want_p))
          else
            ci_prop_diff(get_pct(col), x_n[[nm]], ref[[nm]], ref_n[[nm]],
                         conf_level = conf_level, method = method_diff, want_p = want_p)))
      ci_inf[[nm]] <- res$inf; ci_sup[[nm]] <- res$sup; pvalue[[nm]] <- res$pvalue
    }

    # Phase 9b-5 increment 2: apply the precomputed CI bounds/pvalue (loop above) + `comp_all` + the
    # `visible` display in ONE mutate over plain vectors (was: a with_groups(NULL) CI mutate, then a
    # mutate for comp_all, then one for display -- 3 fmt reconstructions). All three writes are
    # ROW-WISE, so run ungrouped then restore grouping (matching the with_groups(NULL) the CI used).
    diff_row_any <- any(ci == "diff_row")
    comp_all_val <- comp[1] == "all"
    vis_mask     <- visible & ci != "no"
    visible_cols <- names(visible)[!is.na(vis_mask) & vis_mask]
    display      <- stats::setNames(lapply(visible_cols, function(nm)
      if (ci[[nm]] == "cell") ifelse(type[[nm]] == "mean", "mean_ci", "pct_ci") else "ci"), visible_cols)
    # comp_all touches ALL fmt columns (if diff_row); otherwise only the CI + visible columns.
    write_cols   <- if (diff_row_any) names(tabs)[purrr::map_lgl(tabs, is_fmt)]
                    else union(ci_cols, visible_cols)
    grp <- dplyr::group_vars(tabs); drp <- dplyr::group_by_drop_default(tabs)
    tabs <- dplyr::mutate(dplyr::ungroup(tabs), dplyr::across(
      tidyselect::all_of(write_cols),
      function(col) {
        nm <- dplyr::cur_column()
        if (nm %in% ci_cols)
          col <- set_pvalue(set_ci_sup(set_ci_inf(col, ci_inf[[nm]]), ci_sup[[nm]]), pvalue[[nm]])
        if (diff_row_any)         col <- set_comp_all(col, comp_all_val)
        if (nm %in% visible_cols) col <- set_display(col, display[[nm]])
        # Byte-identity quirk (as in chi2_write_contrib): the pre-9b-5 comp_all / visible writes were
        # GROUPED mutates, whose per-group recombine MATERIALISES the `wn` field (NA -> n). Reproduce
        # it for exactly those columns (comp_all = all fmt on diff_row; visible = its own columns) when
        # the table is grouped; a no-op when wn is already set / weighted, or the table is ungrouped.
        if (length(grp) > 0L && (diff_row_any || nm %in% visible_cols))
          col <- set_wn(col, get_wn(col))
        col
      }))
    if (length(grp)) tabs <- dplyr::group_by(tabs, dplyr::across(dplyr::all_of(grp)), .drop = drp)


    #Change ci_type and color, even for totals with no ci result
    ci_with_ref <- stringi::stri_replace_first_regex(ci_with_ref, "_row|_col", "")
    # Phase 14b / 14v-ii: name the SCALE the bounds were actually built on, so every reader
    # (ci_center(), format()'s bracket, the colour significance gate, the legend) dispatches off the
    # stored attribute rather than re-deriving the colour spec. 14v-ii: means now also take the ratio
    # branch above (ci_mean_ratio), so the `type != "mean"` exclusion is gone -- a ratio mean gets
    # ci_type = "ratio" (neutral 1, bare bracket) like a ratio proportion.
    if (identical(ci_scale[1], "ratio")) {
      is_ratio_ci <- ci_with_ref == "diff"
      ci_with_ref[is_ratio_ci] <- "ratio"
    }
    ci_yes_ref  <- !is.na(ci_with_ref) & !ci_with_ref == "no"

    tabs[ci_yes_ref] <-
      purrr::map2_df(tabs[ci_yes_ref],
                     ci_with_ref[ci_yes_ref],
                     ~ set_ci_type(.x, .y) |>
                       set_color(
                         ifelse(!is.null(color[1]) & ! color[1] %in% c("no", ""),
                                color[1], get_color(.))
                       ))
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
# data <- dplyr::starwars |>
#   tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#               na_drop_all = sex)
#
# data |>
#   tab_plain(sex, hair_color, gender, tot = c("row", "col")) |>
#   tab_chi2(calc = c("p", "ctr"), color = TRUE)
#   }
tab_chi2 <- function(tabs, calc = c("ctr", "p", "var", "counts"),
                     comp = NULL, color = c("no", "auto", "all", "all_pct")
) {
  get_vars        <- tab_get_vars(tabs)
  row_var         <- get_vars$row_var
  #col_vars        <- rlang::sym(get_vars$col_vars)
  col_vars_levels <- purrr::map(get_vars$col_vars_levels, rlang::syms)

  stopifnot(all(calc %in% c("all", "ctr", "p", "var", "counts")))
  if ("all" %in% calc) calc <- c("ctr", "p", "var", "counts")
  subtext         <- get_subtext(tabs)

  if (all(get_col_var(tabs) %in% c("", "no_col_var")) |
      "no_row_var" %in% names(tabs)
  ) return(tabs)

  comp <- tab_validate_comp(tabs, comp = ifelse(is.null(comp), "null", comp))
  tabs <- tabs |> tab_match_comp_and_tottab(comp)

  # Phase 10j-B: per col_var, is ANY of its level columns a mean? Read get_type() -- a scalar column
  # attribute -- DIRECTLY off each level column, instead of dplyr::select(ungroup(tabs), <levels>) per
  # col_var (which reconstructed the fmt columns just to read that attribute: ~4.6 % of a chi2 build).
  # Byte-identical (PoC dev/benchmarks/phase10j_tests_parity.R: 26/26 identical over factor/mixed/mean
  # x comp tab/all x 0-2 tab_vars x weighted x a 2x2 Yates).
  is_a_mean <-
    purrr::map_lgl(col_vars_levels, function(levs) {
      cols <- purrr::map_chr(levs, rlang::as_name)
      any(vapply(cols, function(cc) get_type(tabs[[cc]]) == "mean", logical(1)))
    })
  # Phase 3b: mean col_vars now get an ANOVA F (the chi2 mirror), so an all-means table is no
  # longer skipped -- only the factor total-row/total-col scaffolding (which is factor-oriented)
  # is skipped for it. The ANOVA runs on the data rows (row_var-level groups) via agg_anova().
  if (!all(is_a_mean)) {
    tabs <- tabs |> tab_match_groups_and_totrows() |> tab_add_totcol_if_no()
  }

  if (comp == "all") tabs <- tabs |> dplyr::ungroup()

  tot_cols <- detect_totcols(tabs)


  all_col_tot <- names(col_vars_levels) == "all_col_vars"

  tot_cols_names <- purrr::map_lgl(tabs, is_totcol) #|>  .[.] |> names()
  tot_cols_names <- tot_cols_names[tot_cols_names] |> names()
  col_vars_levels_no_tot <-
    purrr::map(col_vars_levels,~ purrr::discard(., . %in% tot_cols_names ) )



  # Phase 9b-5: the per-cell contribution-to-variance WRITES (var, ctr) + the comp_all / contrib-color
  # col-meta -- ported to ONE mutate(across()) over plain-precomputed vectors (chi2_write_contrib()),
  # replacing the pre-9b-5 ~6 mutate(across(where(is_fmt), set_*)) passes (each a full fmt-record
  # reconstruction). Byte-identical; the real cost of the contrib color path (+~97% vs a plain build).
  if ("ctr" %in% calc | "var" %in% calc) {
    tabs <- chi2_write_contrib(tabs, calc, comp, color, col_vars_levels,
                               col_vars_levels_no_tot, is_a_mean, all_col_tot, tot_cols)
  }

  # Phase 9b-5: the whole-table chi2/ANOVA test is a READ-ONLY computation over the cell fields (it
  # builds the tidy `test` tibble, never touches the cells) -- extracted so its plain-field
  # marshalling is isolated from the record-based tab_chi2 orchestration. See chi2_compute_test().
  test_tbl <- chi2_compute_test(tabs, comp, row_var, col_vars_levels,
                                col_vars_levels_no_tot, is_a_mean, all_col_tot)

  tabs <- tabs |> dplyr::select(-tidyselect::any_of("tottabs"))

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, test = test_tbl)
  } else {
    new_grouped_tab(tabs, groups = dplyr::group_data(tabs), subtext = subtext,
                    test = test_tbl)
  }
}


# chi2_compute_test() -- the whole-table chi2 (factor col_vars) + ANOVA (mean col_vars) tests for one
# built factor table, returning the tidy `test` tibble (one row per subtable x col_var x test-type).
# Phase 9b-5: extracted from tab_chi2() as a READ-ONLY marshalling step -- it reads the aggregated cell
# statistics (get_n / get_mean / get_var) and the subtable grouping, feeds the plain-vector engines
# agg_chi2()/agg_anova() (R/tab-agg.R), and NEVER modifies the cells (so cell byte-identity is a given;
# only this plain tibble is recomputed). `tabs` is the prepped, post-tab_match_* record; the remaining
# args are its already-computed metadata (from tab_chi2()'s head).
# DESIGN: chi2/ANOVA run on the already-AGGREGATED cell statistics, never a raw N-scan -- cost scales
# with cells, not observations. Every (subtable x col_var) is one "table_id"; ALL tables are stacked
# and tested in ONE agg_chi2 / agg_anova pass (see the engine header). Chi2 stays fully unweighted
# (chisq.test parity incl. Yates on 2x2, G2); ANOVA F follows §14 (weighted group mean/var + unweighted
# n). WARNING: keep byte-identical to the pre-9b-5 inline block (locked by test-calculations.R: chi2 +
# Yates, Welch/classic F, add_n parity; test-golden.R: the `test` attribute).
chi2_compute_test <- function(tabs, comp, row_var, col_vars_levels,
                              col_vars_levels_no_tot, is_a_mean, all_col_tot) {
  # Phase 9b-5: the kept-rows MASK over `tabs` (replaces the tabs2 = tabs[!is_totrow,] record-slice,
  # which reconstructed every fmt column just to read counts off it). Drops total rows (and total tabs
  # under comp = "all"). is_totrow/is_tottab are the pass-2 fmt_row_flag fast path (plain logical, no
  # reconstruction). Phase 10i-B: the former add_n/add_pct row exclusion ("n"/"row_pct") is gone --
  # chi2 runs at build on the CORE table, which never carries those display-only rows.
  mask2 <- if (comp == "all") !is_totrow(tabs) & !is_tottab(tabs) else !is_totrow(tabs)
  n_rows2 <- sum(mask2)

  # Subtable grouping over the kept rows. Byte-identical to group_indices()/group_keys() of the
  # totrow-dropped grouped_df -- computed on a fmt-FREE view (fmt columns dropped first) so the row
  # slice reconstructs NO fmt records; the same dplyr grouping machinery (incl. `.drop` and the
  # lv1_group_vars downgrade) runs, and grouping depends only on the untouched grouping columns.
  tabs2_grp    <- dplyr::select(tabs, !where(is_fmt))[mask2, ]
  subtab_idx   <- dplyr::group_indices(tabs2_grp)
  subtab_keys  <- dplyr::group_keys(tabs2_grp)
  tab_vars_chr <- names(subtab_keys)

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
        M  <- vapply(lv_cols, function(cc) as.double(get_n(tabs[[cc]])[mask2]), double(n_rows2))
        # Phase 14a: `length(lv_cols)`, NOT `ncol(M)`. vapply() only returns a MATRIX when
        # FUN.VALUE has length > 1, so a row_var with exactly ONE non-total row (n_rows2 == 1 --
        # e.g. all but one level emptied by na = "drop") made M a plain vector, ncol(M) NULL, and
        # every rep(times = ncM) below died with "invalid 'times' argument". It surfaced as a
        # mirai error ("In index: 3 ... Caused by error in rep()"), but was never parallel-specific:
        # the serial map hits the identical line. `length(lv_cols)` is the column count by
        # construction and is shape-independent (as.vector(M) is column-major either way).
        ncM <- length(lv_cols)
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
      chi2_rows <- dplyr::left_join(map, tibble::as_tibble(res$tables), by = "table_id") |>
        dplyr::transmute(
          .data$subtab, .data$col_var, test = "chi2",
          statistic = .data$statistic, df1 = as.double(.data$df), df2 = NA_real_,
          pvalue = .data$pvalue, n = as.double(.data$n), min_e = .data$min_e)
    }
  }

  # --- ANOVA for mean col_vars (Welch + classic F, from per-group summary stats) ---
  anova_rows <- NULL
  if (length(mean_cvs) > 0 && n_rows2 > 0) {
    longA <- dplyr::bind_rows(purrr::imap(
      col_vars_levels[mean_cvs],
      function(levels, cv) {
        cols <- purrr::map_chr(levels, rlang::as_name)
        keep <- purrr::map_lgl(cols, ~ get_type(tabs[[.x]]) == "mean" &&
                                 !any(is_totcol(tabs[[.x]])))
        col  <- cols[keep][1]
        if (is.na(col)) return(NULL)
        tibble::tibble(
          col_var  = cv,
          subtab   = subtab_idx,
          table_id = paste(cv, subtab_idx, sep = "\r"),
          group_id = seq_len(n_rows2),
          n        = as.double(get_n(tabs[[col]])[mask2]),
          mean     = get_mean(tabs[[col]])[mask2],
          var      = get_var(tabs[[col]])[mask2])
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
        pvalue = .data$pvalue, n = as.double(.data$n), min_e = NA_real_)
      classic <- dplyr::transmute(
        baseA, .data$subtab, .data$col_var, test = "F_classic",
        statistic = .data$statistic_classic, df1 = .data$df1_classic, df2 = .data$df2_classic,
        pvalue = .data$pvalue_classic, n = as.double(.data$n), min_e = NA_real_)
      anova_rows <- dplyr::bind_rows(welch, classic)
    }
  }

  # --- Assemble the tidy `test` attribute (one row per subtable x col_var x test-type) ---
  test_tbl <- dplyr::bind_rows(chi2_rows, anova_rows)
  if (nrow(test_tbl) == 0) {
    test_tbl <- new_test_tibble()
  } else {
    subtab_keys2 <- dplyr::mutate(subtab_keys, subtab = dplyr::row_number())
    test_tbl <- test_tbl |>
      dplyr::arrange(.data$subtab, .data$col_var, .data$test) |>
      dplyr::left_join(subtab_keys2, by = "subtab") |>
      dplyr::mutate(row_var = !!row_var) |>
      dplyr::select(-"subtab") |>
      dplyr::relocate(tidyselect::any_of(tab_vars_chr), "row_var", "col_var")
  }

  test_tbl
}


# var_contrib_ctr_signed() -- the plain-vector form of var_contrib(x, tot, "ctr_with_sign", comp) used
# by chi2_write_contrib(): the signed absolute contribution of each cell to the (weighted) chi2, from
# the column's weighted counts `xwn` (get_wn) and its total column's `twn`, using the LAST element as
# the grand total. comp = "all" first zeroes the intermediate total rows/tabs (all but the last).
# Byte-identical to var_contrib()'s "ctr_with_sign" branch (R/tab.R), read plainly.
var_contrib_ctr_signed <- function(xwn, twn, in_totrow, in_tottab, comp) {
  n <- length(xwn)
  if (comp == "all") {
    idx <- seq_len(n - 1L)
    tor <- in_totrow[idx] | in_tottab[idx]
    xwn[idx] <- dplyr::if_else(tor, 0, xwn[idx])
    twn[idx] <- dplyr::if_else(tor, 0, twn[idx])
  }
  observed_freq <- xwn / twn[n]
  expected_freq <- xwn[n] * twn / twn[n]^2
  spread        <- observed_freq - expected_freq
  sign(spread) * spread^2 / expected_freq
}

# contrib_pvalue() -- the standardized-residual two-sided p-value companion to var_contrib_ctr_signed().
# `v` is the cell's SIGNED contribution on the FREQUENCY scale = the chi2 contribution divided by N
# (N = twn[n], the subtable grand total), so the Pearson residual is sign(v)*sqrt(|v|*N), asymptotically
# N(0,1) under independence, and the two-sided p is 2*pnorm(-sqrt(|v|*N)). Total rows/tabs are margins,
# not cells -> NA. Written into the `pvalue` field by chi2_write_contrib() so fmt_color_plan() can gate
# `color = "contrib"` under a significance policy (contrib has NO confidence interval to gate on).
# Weighted note: like the contribution itself, N and v are weighted -> the residual is approximate under
# variable weights (anti-conservative), consistent with tabxplor's weighted-inference framework
# (dev/tabxplor_1.4.0_decisions.md §10/§18); exact for unweighted tables.
contrib_pvalue <- function(v, twn, in_totrow, in_tottab, comp) {
  n  <- length(v)
  pv <- 2 * stats::pnorm(-sqrt(abs(v) * twn[n]))
  prot <- if (comp == "all") in_totrow | in_tottab else in_totrow
  pv[prot] <- NA_real_
  pv[!is.finite(pv)] <- NA_real_
  pv
}

# chi2_write_contrib() -- Phase 9b-5: the per-cell contribution-to-variance WRITES (the `var` = signed
# absolute contribution, and the `ctr` = relative contribution = |cell| / group-total) plus the
# `comp_all` / contrib-`color` col-meta. The pre-9b-5 record path did this in ~6 successive
# mutate(across(where(is_fmt), set_*)) passes -- EACH a full tabxplor_fmt reconstruction. Here every
# value is PRECOMPUTED as a plain vector (plain field reads + the group sums run through the SAME dplyr
# but on fmt-FREE tibbles, so no reconstruction), then applied in ONE mutate(across()) with the real
# setters. `tabs` is the prepped, post-tab_match_* record; the remaining args are tab_chi2()'s already-
# computed metadata (`tot_cols` = detect_totcols()'s per-column total-column syms). Returns the modified
# `tabs`. `var` is written whenever calc has "var"/"ctr"; `ctr`/`comp_all`/`color` only under "ctr".
# WARNING: byte-identical to the pre-9b-5 blocks (locked by test-calculations.R variance-contributions
# + test-color-golden.R + test-golden.R). The dead `variances_by_group`/`cells_by_group` of the old
# path (computed, never used) are dropped.
chi2_write_contrib <- function(tabs, calc, comp, color, col_vars_levels,
                               col_vars_levels_no_tot, is_a_mean, all_col_tot, tot_cols) {
  do_ctr  <- "ctr" %in% calc
  fmt_nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  # var_contrib / the ctr seed are PER SUBTABLE: the pre-9b-5 writes were GROUPED mutates, so each
  # subtable's contributions use its own last (total) row. gid = the (post-prep) subtable of each row
  # (all 1s when ungrouped, e.g. comp = "all"). The row-wise ctr divide + colour don't depend on it.
  gid <- dplyr::group_indices(tabs)
  gids <- unique(gid)

  # --- 1a. absolute signed contribution -> `var` (eligible: non-mean cells of a real col_var) ---
  # Phase 10i-B: the `all_col_vars` exclusion (add_n/add_pct helper columns) is gone -- contrib runs
  # at build on the CORE table, which never carries them; only the total column (`no_col_var`) is out.
  var_after <- purrr::set_names(lapply(fmt_nms, function(nm) get_var(tabs[[nm]])), fmt_nms)
  # Last Phase a bug-fix: the per-cell standardized-residual p-value, computed here (where N = twn[n],
  # the subtable grand total, is in hand) and stored in `pvalue` so fmt_color_plan() can gate
  # `color = "contrib"` under a significance policy. Only under `do_ctr` (contrib coloring is on); the
  # pipeline computes contributions solely then (calc = c("ctr","p")), so plain tables are untouched.
  pval_after <- if (do_ctr) purrr::set_names(lapply(fmt_nms, function(nm) get_pvalue(tabs[[nm]])), fmt_nms)
  elig_col  <- purrr::keep(fmt_nms, function(nm) get_type(tabs[[nm]]) != "mean" &&
                             get_col_var(tabs[[nm]]) != "no_col_var")
  for (nm in elig_col) {
    tot_nm <- as.character(tot_cols[[nm]])
    xwn <- get_wn(tabs[[nm]]); twn <- get_wn(tabs[[tot_nm]])
    itr <- is_totrow(tabs[[nm]]); itt <- is_tottab(tabs[[nm]])
    v   <- var_after[[nm]]
    pv  <- if (do_ctr) pval_after[[nm]]
    for (g in gids) {
      r <- which(gid == g)
      v[r] <- var_contrib_ctr_signed(xwn[r], twn[r], itr[r], itt[r], comp)
      if (do_ctr) pv[r] <- contrib_pvalue(v[r], twn[r], itr[r], itt[r], comp)
    }
    var_after[[nm]] <- v
    if (do_ctr) pval_after[[nm]] <- pv
  }

  ctr_final <- NULL; comp_all_val <- NULL; color_apply <- character(0)
  if (do_ctr) {
    gv           <- dplyr::group_vars(tabs)
    grp_cols     <- purrr::set_names(lapply(gv, function(g) tabs[[g]]), gv)
    table_totrow <- is_totrow(tabs)
    elig_cv      <- names(col_vars_levels)[!is_a_mean & !all_col_tot]

    # per eligible col_var: variances_by_row + cells_by_row -- plain grouped tibbles mirroring the old
    # variances_calc / cells_calc, run through the EXACT original downstream dplyr (no fmt columns).
    ctr_after <- purrr::set_names(lapply(fmt_nms, function(nm) get_ctr(tabs[[nm]])), fmt_nms)
    for (cv in elig_cv) {
      lev_nt <- purrr::map_chr(col_vars_levels_no_tot[[cv]], rlang::as_name)
      vcalc  <- tibble::as_tibble(c(
        grp_cols,
        purrr::set_names(lapply(lev_nt, function(cc) abs(var_after[[cc]])), lev_nt)))
      if (length(gv)) vcalc <- dplyr::group_by(vcalc, dplyr::across(dplyr::all_of(gv)))

      vbr <- vcalc |>
        dplyr::mutate(dplyr::across(where(is.double), ~ sum(., na.rm = TRUE))) |>
        dplyr::ungroup() |> dplyr::select(where(is.double)) |> rowSums(na.rm = TRUE)

      cbr <- vcalc |> tibble::add_column(totrows = table_totrow) |>
        dplyr::mutate(dplyr::across(where(is.double),
          ~ dplyr::if_else(.data$totrows, 0, dplyr::if_else(is.na(.), 0, 1)))) |>
        dplyr::select(-"totrows") |>
        dplyr::mutate(cells = sum(!!!col_vars_levels_no_tot[[cv]]), .groups = "drop") |>
        dplyr::pull(.data$cells)

      # relative-contribution seed on ALL of cv's level columns (incl. its total column):
      # total rows -> 1/cells, others -> the group total variance (broadcast).
      for (L in purrr::map_chr(col_vars_levels[[cv]], rlang::as_name)) {
        ctr_after[[L]] <- dplyr::if_else(is_totrow(tabs[[L]]), 1 / cbr, vbr)
      }
    }

    # divide by the seed to get the relative contribution (|cell| / group-total), keeping the protected
    # total rows untouched (comp = "tab": total rows; comp = "all": total rows of the total table).
    ctr_final <- purrr::set_names(lapply(fmt_nms, function(nm) {
      # comp = "all": protect the total table's total row (it holds the whole-table mean-contribution
      # seed, read back by get_mean_contrib); grand_totrow() degrades to the plain total row when
      # there is no total table (no tab_vars), so the seed is stored, not overwritten.
      prot <- if (comp == "tab") is_totrow(tabs[[nm]]) else grand_totrow(tabs[[nm]])
      dplyr::if_else(prot, ctr_after[[nm]], var_after[[nm]] / ctr_after[[nm]])
    }), fmt_nms)

    comp_all_val <- comp[1] == "all"

    if (!is.na(color[1]) && color[1] != "no") {
      color_condition <- switch(color[1],
        "auto"    = c("n", "all", "all_tabs"),
        "all"     = c("n", "row", "col", "all", "all_tabs"),
        "all_pct" = c("all", "all_tabs"))
      color_apply <- purrr::keep(fmt_nms, function(nm) get_type(tabs[[nm]]) %in% color_condition)
    }
  }

  # single write pass over the UNGROUPED table (so each `col` is the full column that the full-length
  # precomputed vectors match), then restore the original grouping: `var` (always) + `ctr`/`comp_all`/
  # `color` (only under "ctr" calc). The values are group-correct already (var per subtable above; the
  # ctr divide + colour are row-wise), so ungroup/rewrite/regroup is byte-identical.
  grp <- dplyr::group_vars(tabs)
  drp <- dplyr::group_by_drop_default(tabs)
  res <- dplyr::mutate(dplyr::ungroup(tabs), dplyr::across(where(is_fmt), function(col) {
    nm  <- dplyr::cur_column()
    col <- set_var(col, var_after[[nm]])
    if (do_ctr) {
      col <- set_ctr(col, ctr_final[[nm]])
      # Reproduce a byte-identity quirk of the pre-9b-5 path: its ctr writes used dplyr::if_else() over
      # fmt columns, and combining fmt vectors MATERIALISES the `wn` field (NA -> the n fallback). The
      # plain set_ctr here does not, so fill wn from get_wn() (a no-op when wn is already set / weighted;
      # matters only for an unweighted table built via tab_plain() |> tab_chi2(), where wn was NA).
      col <- set_wn(col, get_wn(col))
      # Last Phase a bug-fix: the standardized-residual p-value (contrib significance gate). A no-op on
      # non-eligible columns (pval_after there is the original get_pvalue); the residual on contrib cells.
      col <- set_pvalue(col, pval_after[[nm]])
      col <- set_comp_all(col, comp_all_val)
      if (nm %in% color_apply) col <- set_color(col, "contrib")
    }
    col
  }))
  if (length(grp)) res <- dplyr::group_by(res, dplyr::across(dplyr::all_of(grp)), .drop = drp)
  res
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
      return(dplyr::group_by(tabs, !!!rlang::syms(groups)) |> tab_tot("row"))
      # } else {
      #   tabs <- tabs |> tab_tot("row")
      #   warning("no total row(s) found. One added for the whole table")
      # }
    } else if ( !any(is_tottab(tabs)) ) { #If there are no groups
      warning("no groups nor total row(s) found. One added for the whole table")
      return(tab_tot(tabs, "row"))
    } else {
      warning("no groups nor total row(s), but total table found. ",
              "Grouped upon tab_vars and total rows added")
      tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
      return(dplyr::group_by(tabs, !!!tab_vars) |> tab_tot("row"))
    }

    #If there is at least one total row, calculate new groups based on them
  } else {
    if (utils::tail(is_totrow(tabs), 1L)) return(dplyr::ungroup(tabs))


    tabs_totrow_groups <- tabs |> dplyr::ungroup() |>
      (\(d) tibble::add_column(d, totrow_groups = as.integer(is_totrow(d))))() |>
      dplyr::mutate(totrow_groups = 1 + cumsum(.data$totrow_groups) - .data$totrow_groups)
    totrow_indices <- tabs_totrow_groups$totrow_groups

    #Control if totrows groups match tab_vars, collectively or individualy, if yes group
    tab_vars <- rlang::syms(tab_get_vars(tabs)$tab_vars)
    if ( !identical(tab_vars, groups) ) {
      tabs_tab_vars_groups <- tabs |> dplyr::group_by(!!!tab_vars)
      tab_vars_indices <- dplyr::group_indices(tabs_tab_vars_groups)

      if (all(totrow_indices == tab_vars_indices)) return(tabs_tab_vars_groups)
    }

    each_tab_var_indices <-
      tabs |> dplyr::ungroup() |> dplyr::select(!!!tab_vars) |>
      dplyr::transmute(dplyr::across(dplyr::everything(), as.integer)) |>
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
    return(dplyr::relocate(tabs_totrow_groups, .data$totrow_groups, .before = 1) |>
             dplyr::group_by(.data$totrow_groups)
    )

  }

}



#' @keywords internal
tab_add_totcol_if_no <- function(tabs) {
  if (!any(is_totcol(tabs)) & ! all(get_type(tabs) == "mean")) { # & !only_one_column
    only_one_column <- length(which(purrr::map_lgl(tabs, is_fmt))) == 1L
    tabs <- tabs |> tab_tot("col", totcol = "last")
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
    tabs <- tabs |> tab_totaltab('line')
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
  quo <- rlang::get_expr(quo) |> as.character()
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
    tabs <- tabs |>
      tibble::as_tibble() |>
      dplyr::mutate(!!new_rownames :=
                      paste(!!!purrr::map(text_cols, rlang::sym), sep = "_")) |>
      dplyr::select(-tidyselect::all_of(text_cols)) |>
      dplyr::relocate(where(is.character), .before = 1) |>
      tibble::column_to_rownames(var = new_rownames)
  } else {
    #tabs <- tabs |> tibble::column_to_rownames(var = rlang::as_name(row_var))
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
  index <- if (length(exact) >= 1L) exact else which(stringi::stri_detect_regex(targets, ref))
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
# tabs_nogroup <- tabs |> dplyr::ungroup() |> .[ci_yes]
#
# #Compute all variables needed to calculate ci in different tabs
# xbase <- tabs_nogroup |>
#   dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns =  ~ dplyr::if_else(
#     condition = get_display(.) == "mean",
#     true      = get_mean(.),
#     false     = get_pct(.)
#   )))
#
# xvar <- tabs_nogroup |>
#   dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ dplyr::if_else(
#     condition = get_display(.) == "mean",
#     true      = get_var(.),
#     false     = NA_real_
#   )))
#
# ybase <-
#   tibble::tibble(ci, tot_cols, names = rlang::syms(names(tabs))) |>
#   dplyr::filter(ci_yes) |>
#   purrr::pmap_df(function(ci, tot_cols, names) switch(
#     ci,
#     "cell"     = NA_real_,
#     "diff_col" = dplyr::pull(tabs, !!tot_cols),
#     "diff_row" = dplyr::mutate(tabs, comp = dplyr::last(!!names)) |>
#       dplyr::pull(comp)
#   ))
#
# yvar <- ybase |>
#   dplyr::mutate(dplyr::across(where(~ !get_type(.)=="mean"), ~NA_real_)) |>
#   dplyr::mutate(dplyr::across(where(~ get_type(.) =="mean"), get_var))
#
# ybase <- ybase |>
#   dplyr::mutate(dplyr::across(
#     where(~ is_fmt(.) & !get_type(.) == "mean"),
#     get_pct
#   )) |>
#   dplyr::mutate(dplyr::across( where(~ get_type(.) == "mean"), get_mean))
#
# xn <-
#   tibble::tibble(type, tot_cols, names = rlang::syms(names(tabs))) |>
#   dplyr::filter(ci_yes) |>
#   purrr::pmap_df(function(type, tot_cols, names) switch(
#     type,
#     "row"      = dplyr::pull(tabs, !!tot_cols) |> get_n(),
#     "mean"     = dplyr::pull(tabs, !!names   ) |> get_n(),
#     "col"      = dplyr::mutate(tabs, xn = dplyr::last(get_n(!!names)) ) |>
#       dplyr::pull(xn),
#     "all"      = ,
#     "all_tabs" = dplyr::mutate(tabs, xn = dplyr::last(get_n(!!tot_cols)) ) |>
#       dplyr::pull(xn),
#     NA_integer_
#   ))
#
# yn <-
#   tibble::tibble(ci, type, tot_cols, names = rlang::syms(names(tabs))) |>
#   dplyr::filter(ci_yes) |>
#   purrr::pmap_df(function(ci, type, tot_cols, names) switch(
#     ci,
#     "cell"       = NA_real_,
#     "diff_col"   =
#       switch(type,
#              "row"      = dplyr::pull(tabs, !!tot_cols) |> get_n(),
#              "col"      = ,
#              "all"      = ,
#              "all_tabs" =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!tot_cols)) ) |>
#                dplyr::pull(yn),
#              NA_real_
#       ),
#     "diff_row"   =
#       switch(type,
#              "mean"     = ,
#              "col"      =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!names)) ) |>
#                dplyr::pull(yn),
#              "row"      = ,
#              "all"      = ,
#              "all_tabs" =
#                dplyr::transmute(tabs, yn = dplyr::last(get_n(!!tot_cols)) ) |>
#                dplyr::pull(yn),
#              NA_real_
#       )
#   ) )
#
#
# ci_map <-
#   list(xbase = xbase, xvar = xvar,
#        ybase = ybase, yvar = yvar,
#        xn = xn, yn = yn) |>
#   purrr::map(~purrr::map(., ~ .)) |>
#   purrr::transpose() |> purrr::map(~ tibble::as_tibble(.)) |>
#   tibble::tibble(.name_repair = ~ "ci_map") |>
#   tibble::add_column(ci = ci[ci_yes], type = type[ci_yes]) |>
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
#   ) |> purrr::set_names(names(tabs)[ci_yes])
#   )
#
# calculations <- ci_map |>
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
# result <- calculations |> purrr::map_df(~ dplyr::pull(., res))
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
                            method_ratio = "katz", method_mean_diff = "welch",
                            method_mean_ratio = "robust",
                            ci_scale = "diff", cached_test = NULL) {
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
                  method_cell = method_cell, method_diff = method_diff,
                  method_ratio = method_ratio, method_mean_diff = method_mean_diff,
                  method_mean_ratio = method_mean_ratio, ci_scale = ci_scale)
  }

  list(tab = tab, test = test)
}


# tab_append_pctcol_rows() -- Phase 14a. Under pct = "col" the add_n / add_pct extras are ROWS: a
# re-displayed copy of a sub-table's total row. `transform` takes the sliced source row(s) and
# returns the row(s) to insert. Two bugs lived in the inline `bind_rows(tab, slice(tab, last_totrow))`
# this replaces:
#   1. `last_totrow` is a GLOBAL row index (is_totrow.data.frame is not group-aware), but a merged
#      multi-row_var tab is a grouped_df where dplyr::slice() indexes WITHIN each group. No group had
#      that many rows, so slice() returned ZERO rows and bind_rows() silently dropped the extra --
#      the reported "the n row disappears with several row_vars". Fix: slice on the ungrouped tab.
#   2. only the LAST total row of the whole table was copied, and appended at the very bottom. With
#      several row_vars that single row would sit under the last sub-table as if it belonged to it.
#      Fix: one row per sub-table, spliced in right after its OWN source row.
# Byte-identical wherever a table has one sub-table whose total row is last (every shape the goldens
# cover): one source row, spliced after the last row == appended.
# WARNING: the group column must NOT be relabelled -- the copy keeps its sub-table's `row_var` value
# so it stays inside that group; `transform` only relabels tab_get_vars()$row_var (= "levels" on a
# compacted tab, the real row_var otherwise).
tab_append_pctcol_rows <- function(tab, transform) {
  gv   <- dplyr::group_vars(tab)
  flat <- dplyr::ungroup(tab)
  n0   <- nrow(flat)
  tot  <- is_totrow(flat) & tab_get_vars(flat)$row_var != "no_row_var"
  if (!any(tot)) return(tab)
  gid  <- if (length(gv) > 0) dplyr::group_indices(tab) else rep(1L, n0)
  grps <- unique(gid[tot])
  # SOURCE = each sub-table's last total row; ANCHOR = the END of that sub-table. They differ once a
  # previous pass has already inserted an extra (add_pct runs before add_n), and anchoring on the
  # group's end is what keeps the historical `Total | row_pct | n` order -- with one ungrouped
  # sub-table it is exactly the old `bind_rows(tab, ...)` append.
  src    <- vapply(grps, function(g) { i <- which(tot & gid == g); i[[length(i)]] }, integer(1))
  anchor <- vapply(grps, function(g) { i <- which(gid == g);       i[[length(i)]] }, integer(1))
  ord    <- order(src)
  src    <- src[ord]; anchor <- anchor[ord]
  out    <- dplyr::bind_rows(flat, transform(dplyr::slice(flat, src)))
  # splice: bind_rows put the new rows at the very end, so re-order by "just after my sub-table".
  out    <- dplyr::slice(out, order(c(seq_len(n0), anchor + 0.5)))
  if (length(gv) > 0) dplyr::group_by(out, dplyr::across(tidyselect::all_of(gv))) else out
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
              list(tabs_text, last_totrow_pct_cols_no_empty, last_totrow_pct_cols),
              ~ {
                totcols_ref <- purrr::map_chr(detect_totcols(..1), as.character)
                val_cols    <- ..3
                row_lab     <- tab_get_vars(..1)$row_var
                if (..2) {
                  tab_append_pctcol_rows(..1, function(src) {
                    src |>
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
                          where(is_fmt) & -tidyselect::all_of(val_cols),
                          ~ set_num(., value = NA_real_)
                        ),
                        dplyr::across(
                          all_of(row_lab),
                          ~ factor("row_pct")
                        )
                      )
                  })
                } else {
                  ..1
                }
              }
            )
        }

        if (add_n) {
          tabs_text <-
            purrr::pmap(list(tabs_text, last_totrow_pct_cols_no_empty, last_totrow_pct_cols),
                        ~ {
                          val_cols <- ..3
                          row_lab  <- tab_get_vars(..1)$row_var
                          if (..2) {
                            tab_append_pctcol_rows(..1, function(src) {
                              src |> set_display("n") |>
                                dplyr::mutate(
                                  dplyr::across(where(is_fmt), ~ as_totrow(., FALSE)  |>
                                                  set_diff(NA_real_) |> set_ci(NA_real_) |>
                                                  set_mean(NA_real_) |> set_pct(NA_real_) |>
                                                  set_ctr(NA_real_) |> set_var(NA_real_)
                                                ),
                                  dplyr::across(
                                    where(is_fmt) & -tidyselect::all_of(val_cols),
                                    ~ set_num(., value = NA_real_)
                                  ),
                                  dplyr::across(
                                    all_of(row_lab),
                                    ~ factor("n")
                                  )
                                )
                            })
                          } else {
                            ..1
                          }
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
    #     ) |>
    #       dplyr::rename(all_of(
    #         purrr::set_names(
    #           names(.)[stringi::stri_detect_regex(names(.), "_.nnnnnn$")],
    #           paste0(get_col_var(.)[stringi::stri_detect_regex(names(.), "_.nnnnnn$")], "_n")
    #         )
    #       ))
    #   )


  tabs_text
}


# tab_is_or_display() -- Phase 16c. TRUE when the table DISPLAYS odds ratios (any fmt value column with
# display "or"/"or_pct"). The "100%" total column is meaningless for such a table (ORs don't sum to 1),
# so the Total column shows only the base n (console) / is dropped in favour of the base-n column
# (export). Keyed on the DISPLAYED quantity, NOT ci_type: `color = "OR"` with `OR = "no"` shows real
# percentages (a meaningful 100% total) yet can still carry an OR interval.
tab_is_or_display <- function(tab) {
  if (!is.data.frame(tab)) return(FALSE)
  fc <- purrr::map_lgl(tab, is_fmt)
  if (!any(fc)) return(FALSE)
  any(purrr::map_lgl(tab[fc], ~ any(get_display(.) %in% c("or", "or_pct"))))
}

# tab_fold_addn_incell() -- Phase 10i-B decision 1. For TEXT backends (console / kable / md), the
# add_n base moves out of a separate `n` COLUMN and into the Total cell as an in-cell composite
# `{pct} (n={n})` (via the Phase-10i-A display grammar). The materialiser first builds the real `n`
# column with tab_add_n_pct() (byte-identical to the old build), then this drops it and folds the
# base into the Total column's display. Default (`tabxplor.totcol_range = "off"`): each Total cell
# shows its OWN base `{n}`. Option `"range"` / `"min"`: the cross-col_var base via tab_totcol_range()
# (a per-row literal `[min;max]` / smallest), for tables whose col_vars have differing NA bases.
# Phase 16c: for an OR/RRR table the "100%" is dropped -> the cell shows only `n={n}` (the base).
# NB: run BEFORE tab_pvalue_lines(), so the Total column has only data/total cells (all eligible).
tab_fold_addn_incell <- function(tab) {
  tot_nm <- dplyr::last(names(tab)[is_totcol(tab) & get_type(tab) == "row" &
                                     get_col_var(tab) != "no_col_var"])
  if (length(tot_nm) != 1 || is.na(tot_nm)) return(dplyr::select(tab, -tidyselect::any_of("n")))
  style <- getOption("tabxplor.totcol_range", "off")
  is_or <- tab_is_or_display(tab)

  rng <- if (!identical(style, "off")) {
    fmt_cols <- which(purrr::map_lgl(tab, is_fmt))
    tab_totcol_range(tab, fmt_cols, get_col_var(tab), which(is_totcol(tab)), style = style)
  } else NULL

  tmpl <- if (is_or) {                                # OR/RRR: show only the base n, drop the "100%"
    if (is.null(rng)) rep("n={n}", nrow(tab))
    else dplyr::if_else(is.na(rng$text), "", paste0("n=", rng$text))
  } else if (is.null(rng)) {
    NULL                                              # uniform "{pct} (n={n})"
  } else {
    # per-row literal: "{pct} (n=<base>)"; a row with no base falls back to "{pct}".
    dplyr::if_else(is.na(rng$text), "{pct}", paste0("{pct} (n=", rng$text, ")"))
  }

  tab <- dplyr::select(tab, -tidyselect::any_of("n"))   # drop the xl-style `n` column
  dplyr::mutate(tab, dplyr::across(tidyselect::all_of(tot_nm), function(col) {
    d    <- get_display(col)
    # only genuine value cells where both fields render (Phase-10i-A `both` guard); the Total
    # column is all pct/n non-NA here (p-value rows are materialised later), so this is all cells.
    elig <- !is.na(get_num(set_display(col, "pct"))) & !is.na(get_num(set_display(col, "n")))
    if (is.null(tmpl)) d[elig] <- "{pct} (n={n})" else d[elig] <- tmpl[elig]
    set_display(col, d)
  }))
}

# tab_or_total_col() -- Phase 16c. Complements tab_fold_addn_incell for the cases the in-cell fold does
# not cover: the "100%" total column is meaningless on an OR/RRR table, so drop it for EXCEL (the base n
# is exported as its own `n` column when add_n is on, nothing otherwise) and for the CONSOLE add_n=FALSE
# case (no base to fold -> nothing). The console add_n=TRUE case is already handled by the fold above
# (the Total cell shows `n={n}`), so this no-ops there.
tab_or_total_col <- function(tab, backend, add_n_on) {
  if (!is.data.frame(tab) || !tab_is_or_display(tab)) return(tab)
  tot_nm <- names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && is_totcol(.) &&
                                        get_type(.) == "row" && get_col_var(.) != "no_col_var")]
  if (!length(tot_nm)) return(tab)
  if (identical(backend, "xl") || !isTRUE(add_n_on)) {
    tab <- dplyr::select(tab, -tidyselect::all_of(tot_nm))
  }
  tab
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
  totcol <- purrr::map_lgl(tab[fmt_names], is_totcol)

  cell_base <- function(col) if (get_type(col) == "mean") get_n(col) else get_tot_n(col)

  # --- protected rows (never dropped) --------------------------------------------------------
  # Phase 10i-B: n_min runs at build on the CORE table -- the add_n/add_pct/p-value extras are
  # materialised later, at display -- so the former helper-COLUMN ("all_col_vars") and helper-ROW
  # ("n"/"row_pct"/p-value) protections are dead. Only the total row / total table are protected.
  fmt_all <- tab[fmt_names]
  totrow  <- purrr::reduce(purrr::map(fmt_all, is_totrow), `|`)
  tottab  <- purrr::reduce(purrr::map(fmt_all, is_tottab), `|`)
  protect <- totrow | tottab

  # --- row-drop + cell-blank on row-oriented columns -----------------------------------------
  row_cols <- fmt_names[type %in% c("row", "all", "mean")]  # totcol INCLUDED in the max
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
  # blank surviving weak cells (row-oriented, non-total stat columns)
  blank_cols <- fmt_names[type %in% c("row", "all", "mean") & !totcol]
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
  drop_cols <- fmt_names[type == "col" & !totcol]
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


