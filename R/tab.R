# PURPOSE: Main user-facing API for cross-tabulation -- tab() and the tab_build() pipeline it drives.
# ROLE: tab() is a thin wrapper over the internal engine tab_build() (Phase 6), whose six stages live
#   here, plus tab_prepare(), tab_spread(), tab_transpose() and the variable-model readers.
# KEY CONSTRAINTS:
#   - Phase 19l SPLIT this file (it had reached 7918 lines holding four unrelated subsystems). What
#     left, and where to look for it:
#       R/tab-leaf.R       the aggregate core -- tab_plain/tab_num, plain_core/num_core, every
#                          leaf_*, tab_apply_reference, leaf_ci_plain, the total-row builders
#       R/tab-chi2.R       chi2_compute_test / chi2_write_contrib + the contribution helpers
#       R/tab-display.R    the `{}` display grammar + the add_n / add_pct materialisation
#       R/tab-deprecate.R  the retired-argument shims + the superseded tab_many()
#       R/tab-steps-legacy.R  the pre-2.0.0 step API and, since 19l, its own six helpers
#     Whole functions moved, nothing changed. tab.R sorts AFTER every tab-*.R in the C collation R
#     uses, so a new file may read tab.R's top-level objects but not the reverse.
#   - ONE AGGREGATE CORE (Phase 19j, KEY 5): the leaf computes the cells, THEIR INTERVAL and the
#     whole-table TEST, because that is where the plan is. There is no second pass: tab_apply_tests()
#     is gone, and tab_ci()/tab_chi2() are superseded wrappers that RECONSTRUCT a plan from fmt
#     markers for the exported step path only. Both sides share the arithmetic, so a step and a build
#     cannot compute two different answers. The ordering invariant (compute on the FULL level set,
#     before tab_assemble drops the non-first levels) is STRUCTURAL.
#   - The build pipeline has ONE carrier for its settings: `ctx$settings`, the star schema
#     tab_setup() builds and tab_prepare_pop() completes. Each stage projects it into the bare names
#     it reads with ctx_settings_locals(); nothing writes those names into the ctx, and the raw
#     inputs the spine owns (SPINE_OWNED_INPUTS) leave it once tab_setup() has consumed them.
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
#' only the first level printed. Deprecated in 2.0.0: pass these columns in \code{col_vars} and
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
#'   \code{"\{pct\} \{ci\}"}. Valid fields: \code{pct}, \code{n}, \code{wn}, \code{mean},
#'   \code{diff}, \code{ratio}, \code{ci}, \code{or}, \code{ctr}, \code{var}, \code{resid},
#'   \code{obs}; the first field is the \emph{primary}, shown alone by Excel and used for coloring.
#'   \code{ctr} is the cell's contribution to the chi-squared and \code{resid} its adjusted
#'   standardized residual (both need \code{color = "contrib"} or \code{test = TRUE}), so
#'   \code{display = "\{pct\} (\{resid\})"} prints each percentage with the residual that says
#'   whether it departs from independence -- the SPSS cell layout. \code{obs} is
#'   \code{\link{tab_reg}}-only: the OBSERVED (crude) effect beside the modelled one, so
#'   \code{\link{set_display}(t, "\{or\} (obs \{obs\})")} on a regression table prints each
#'   adjusted odds ratio next to the unadjusted one it is compared to (\code{tab_reg} has no
#'   \code{display} argument of its own; see \code{color = "adjustment"} in \code{?tab_reg}). A bare field name is also accepted as a
#'   shorthand for its single-field template, so \code{display = "ci"} is the same as
#'   \code{display = "\{ci\}"} (it shows the confidence interval). The special value
#'   \code{display = "num_ci"} is a type-adaptive shorthand for \code{"\{pct\} \{ci\}"} on percentage
#'   columns and \code{"\{mean\} \{ci\}"} on numeric (mean) columns, chosen per column, so a mixed
#'   factor + numeric table shows each value with its confidence interval in one call. Like
#'   \code{"\{pct\} \{ci\}"} it displays the CI the table computes (the cell, difference or ratio CI
#'   set by \code{ci = } / \code{color}), so pair it with a \code{ci = } value or a \code{color} that
#'   needs one. \code{NULL} (default) keeps the plain single-field display. It is a display overlay
#'   only: colors, differences and the underlying fields are unchanged.
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
#'   \item \code{"last"}: the mirror of \code{"first"} — the **last level** of the row (or column)
#' variable. A total row or column is not a level and is never selected: use \code{"tot"} for that.
#' Resolved inside each subtable when there are \code{tab_vars}.
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
#' @param OR `r lifecycle::badge("deprecated")` The odds ratio is computed on **every** row/col
#'  percentage table since 2.0.0, so this argument had nothing left to switch on: it was a
#'  \code{display}, a \code{color} and a \code{ref2} welded together. Each value maps to one of them:
#'  \itemize{
#'   \item \code{"OR"} -> \code{display = "\{or\}"} (show the odds ratio instead of the percentage);
#'   \item \code{"OR_pct"} -> \code{display = "\{or\} (\{pct\})"};
#'   \item \code{"cumOR"} -> \code{ref2 = "cumulative"}.
#'  }
#'  Colour it with \code{color = "odds_ratio"}, and pick which 2x2 with \code{ref} (the row) and
#'  \code{ref2} (the column level).
#' @param test Set to \code{TRUE} to calculate a statistical test of independence for each
#' (sub)table: \strong{Chi-squared} for factor \code{col_vars}, \strong{Welch's F} (one-way
#' ANOVA) for numeric ones -- see \code{\link{tab_chi2}}. The whole-table summary also carries an
#' \strong{effect size} (Cramer's V / phi for factors, eta-squared for means) and, on a small sparse
#' factor table where the chi-squared is unreliable, an exact \strong{Fisher} p-value. Useful to print
#' metadata, and to color cells based on their contribution to variance (\code{color = "contrib"}).
#' Automatically added if needed for \code{color}.
#'
#' \code{test} says only \emph{whether} to test; \strong{what kind of test you get follows what you
#' passed}. \code{wt} says how the \emph{estimate} is computed; a second, orthogonal fact --- the
#' \strong{inference basis} (the "weighting level" of \code{vignette("tabxplor")}), stored on each
#' column and named in the table's footer --- says how the \emph{interval and the test} are:
#' \enumerate{
#'  \item \code{wt = w} --- estimates, the whole-table test and the effect size are all computed on the
#'  \strong{weighted} table, but with the raw unweighted number of respondents as the sample size, so
#'  they carry no design effect. This is the default, and the footer says so.
#'  \item \code{wt = w} plus \code{design_effect = TRUE} (or, for a whole session,
#'  \code{options(tabxplor.design_effect = TRUE)}) --- the same intervals and
#'  tests \strong{account for the unequal weighting, exactly}. A weight column IS a survey design
#'  (the flat one, \code{ids = ~1}), so this is not an approximation: the base becomes
#'  \code{n_eff = p(1-p) / Var_design(p)} in closed form, and the whole-table test becomes
#'  \code{survey::svychisq} / a \code{svyglm} Wald F on that same flat design. Being exact rather
#'  than a bound, it can make an interval \emph{narrower} as well as wider --- unequal probabilities
#'  can carry more information than equal ones. It is blind to clustering and to calibration, which
#'  the weights do not record.
#'  \item a prebuilt \code{survey::svydesign} passed as \code{data} --- fully \strong{design-based}:
#'  the same quantities, now with strata, clusters, \code{fpc} and calibration, and every interval
#'  referred to the design's own degrees of freedom.
#' }
#' A fourth basis is not a choice but a fallback: when a design-based table's variance cannot be
#' computed, it reverts to the weighting-only correction, and its footer says so.
#' Turn the option on when you want a \code{tab()} percentage interval to be comparable with the
#' \code{Obs_*} column of a \code{\link{tab_reg}} on the same data: \code{tab_reg()} never reads it,
#' because its crude companions are \emph{always} on the weighted basis, beside a model column that
#' always was. Replicate-weight (\code{svrepdesign}) and two-phase designs are not supported, and
#' \code{wt} beside a design is an error (a design already carries its own weights).
#' @param anova Which one-way ANOVA \strong{F} the p-value line shows for \emph{numeric}
#' \code{col_vars}: \code{"welch"} (does not assume equal variances) or \code{"classic"} (the pooled
#' F). \code{NULL} (default) reads \code{options(tabxplor.anova)}. Both statistics are always
#' computed and stored in the table's \code{test} attribute, so this is a pure display choice ---
#' it changes which row is shown, never a number.
#' @param chi2 `r lifecycle::badge("deprecated")` Renamed to \code{test} in 2.0.0: the test is a
#' Chi-squared only for factors (numeric \code{col_vars} get Welch's F), so the old name was
#' misleading. Still works.
#' @param ci **What the confidence interval is anchored on** -- one question, four answers. The
#'  \emph{geometry} of the interval is not asked here: it follows the comparison the table makes
#'  (\code{color}, then \code{display}), so an odds-ratio table gets an odds-ratio interval and a
#'  ratio-coloured one a Katz ratio interval, with no way for the two to disagree.
#'   \itemize{
#'    \item \code{"auto"} (default): an interval on the comparison when the table makes one
#'      (percentages by row/column, means), an absolute cell interval for plain frequencies, and
#'      none at all when nothing needs one.
#'    \item \code{"ref"}: the interval of the difference (or ratio, or odds ratio) between a cell
#'      and its reference -- the total cell, or the first cell under \code{ref = "first"}.
#'    \item \code{"cell"}: the absolute interval of the cell's own percentage or mean.
#'    \item \code{"no"}: no interval.
#'   }
#'  \code{"cell"} and \code{"no"} anchor nothing to compare, so \code{stars} and
#'  \code{color_signif} have nothing to read: asking for either alongside them informs you once and
#'  disables it, rather than silently testing something else.
#'  Methods are chosen with \code{ci_method} and named in the table's legend; by default percentages
#'  use the Wilson score interval for a cell and the Newcombe hybrid score for a difference (its
#'  dual, so the bracket and the stars always agree), and means the Welch t interval. With
#'  \code{ci = "cell"} the result prints as `[inf;sup]`; set
#'  `options("tabxplor.ci_print" = "moe")` for `pct +- moe`.
#'  \code{"diff"} and \code{"ratio"} are soft-deprecated spellings of \code{"ref"} (the second one
#'  also pins the ratio scale -- say \code{color = "ratio"} instead).
#' @param conf_level The confidence level, as a single numeric between 0 and 1.
#' Default to 0.95 (95%).
#' @param stars Logical (default \code{FALSE} \emph{opt-in}). With \code{ci = "diff"}, print
#' significance stars for each cell's difference from its reference, read from the displayed interval
#' itself (universal CI-inclusion). \code{NULL} uses `options("tabxplor.stars")` (default
#' \code{FALSE}). See \code{\link{tab_many}}.
#' @param ci_method The confidence-interval method of each kind of interval, as ONE named vector --
#' partial, like \code{ref} or \code{pct}, so an unnamed kind keeps its default.
#' \itemize{
#'   \item \code{cell}: a proportion's own interval (\code{ci = "cell"}) -- \code{"wilson"}
#'     (default, the score interval), \code{"wald"} (the normal approximation, commonly taught --
#'     degenerate at 0 or 1) or \code{"beta"} (Korn-Graubard: the exact Clopper-Pearson interval on
#'     the effective base, referred to a survey design's own degrees of freedom).
#'   \item \code{diff}: a proportion minus its reference (\code{ci = "diff"}) -- \code{"newcombe"}
#'     (default, the hybrid-score interval, dual of the two-proportion score test), \code{"ac"}
#'     (Agresti-Caffo) or \code{"wald"}.
#'   \item \code{mean_diff}: a numeric mean minus its reference -- \code{"welch"} (default, each
#'     group's own variance) or \code{"student"} (pooled variance = a linear-regression coefficient
#'     interval).
#'   \item \code{mean_ratio}: a numeric mean over its reference (\code{color = "ratio"}) --
#'     \code{"robust"} (default, each group's own variance = modified/robust Poisson),
#'     \code{"quasipoisson"} (dispersion-scaled = a quasi-Poisson regression) or \code{"poisson"}
#'     (naive).
#' }
#' Whatever the method, the significance stars come from that same interval, so bracket and stars
#' always agree. A proportion \emph{ratio} has only one method (Katz's log risk-ratio), so it is not
#' a choice. Example: \code{ci_method = c(cell = "beta", diff = "ac")}.
#' @param design_effect Logical or \code{NULL} (default). Whether the confidence intervals, stars and
#' colour thresholds of a \strong{weighted} table account for the weighting's own design effect (the
#' exact flat-design variance) instead of using the raw sample size. \code{NULL} takes
#' \code{options("tabxplor.design_effect")} (\code{FALSE} by default). Ignored without \code{wt}, and
#' superseded by a \code{\link[survey]{svydesign}} passed as \code{data} (which is always
#' design-based). See the "Weights" section of the introduction vignette.
#' @param method_cell,method_diff `r lifecycle::badge("deprecated")` Use
#' \code{ci_method = c(cell = , diff = )} instead.
# @param ci_visible By default, confidence intervals are calculated and used to set
# colors, but not printed. Set to \code{TRUE} to print them in the result.
#' @param color Which measure(s) to color, on which visual channel. \code{FALSE} (default)
#' prints no color; \code{TRUE} uses the smart per-column-type scheme (factors: the
#' \code{difference} on the text + the \code{ratio} on the background; numerics: the
#' \code{ratio}; counts: \code{contrib}). Otherwise a measure name, on the \strong{text} channel:
#'  \itemize{
#'   \item \code{"difference"}: cell difference from the reference (percentage points for factors;
#'   the standardized difference Glass's \eqn{\Delta} for numeric means).
#'   \item \code{"ratio"}: relative risk (factors) or mean ratio (numerics) vs the reference.
#'   \item \code{"odds_ratio"}: the empirical odds ratio (for \code{pct = "row"}/\code{"col"}),
#'   coloured on its own symmetric \code{odds_ratio} scale (so \code{pct_ratio} stays free for
#'   \code{"ratio"}).
#'   \item \code{"contrib"}: signed contribution to the chi-squared (reference-free).
#'  }
#' The discipline's acronyms are permanent aliases of those names: \code{"diff"} / \code{"RD"},
#' \code{"RR"}, \code{"or"} / \code{"OR"}.
#' The grammar: \strong{position picks the channel} (1st value -> text, 2nd -> background) and
#' \strong{names pick the column type} (\code{pct} / \code{mean}). So
#' \code{c("difference", "ratio")} puts the difference on the text and the ratio on the background
#' of every column; \code{c(pct = "difference", mean = "ratio")} colors factors by the difference
#' and numeric means by the ratio (text channel); \code{list(pct = c("difference", "ratio"),
#' mean = "ratio")} combines both (per-type, with channels). Only \code{difference} / \code{ratio}
#' may go on the background.
#' Thresholds come from \code{\link{set_color_breaks}} or the per-table \code{color_breaks}
#' argument. \code{color} also names the table's COMPARISON, and so decides which interval
#' \code{ci = "auto"} builds. (The old combined strings \code{"diff_ci"}, \code{"after_ci"} and
#' \code{"ci"} still work but are soft-deprecated in favor of \code{color_signif}.)
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
#' With \code{color = "contrib"} the three values are three readings of the same departure from
#' independence, because a contribution has no confidence interval to floor:
#'  \itemize{
#'   \item \code{"ignore"} and \code{"grey_non_signif"} color the \strong{relative} contribution
#'   (a share of \emph{this} table's chi-squared, in multiples of the mean cell contribution --
#'   the correspondence-analysis reading, so the scale is relative to the table);
#'   \item \code{"guaranteed_effect"} colors the \strong{adjusted standardized residual} itself, on
#'   the absolute \code{zscore} break scale (+/-1.96, +/-2.58, +/-3.89, +/-6 by default). Those
#'   thresholds mean the same thing in every table, which is the SPSS "adjusted residual" reading.
#'  }
#' In all three, significance is the adjusted standardized residual (Haberman; SPSS's "adjusted
#' residual", R's \code{chisq.test()$stdres}), \emph{not} the Pearson residual \code{(o-e)/sqrt(e)},
#' whose variance is below 1 and which therefore under-rejects. Under weights the residual follows
#' the package rule -- weighted estimate, and a base that follows the inference basis: the raw
#' \code{n} by default (the reading a correspondence analysis expects), and under
#' \code{options(tabxplor.design_effect = TRUE)} or a \code{survey} design that raw \code{n} divided
#' by the \strong{association's} design effect -- Rao-Scott's mean generalized delta-bar, the very
#' one the whole-table test reports, so the colours and the p-value of one table describe one design
#' effect. The
#' contribution itself stays weighted (it estimates the population table's structure, and is therefore
#' identical at every basis, which is what keeps the correspondence-analysis reading safe). One base
#' for the whole table, so a counts table and a percentage table of the same data give the SAME
#' residuals. Cells whose expected count is below 1 are left
#' uncolored: the normal approximation does not hold there.
#' Colors are computed per column at print time; since 2.0.0 each column records the confidence level
#' it was built at, so the significance thresholds follow the call's \code{conf_level}. A column that
#' never recorded one (a hand-built \code{\link{fmt}}) falls back to
#' \code{options(tabxplor.conf_level)}.
#' @param color_breaks A per-table override of the colour thresholds, a named list of scales like
#' \code{\link{set_color_breaks}} accepts, e.g. \code{list(pct_ratio = list(over = 2))}. Stored as
#' a table attribute and applied at print / export; \code{NULL} (default) uses the global breaks.
#' Unset scales fall back to the global setting.
#' @param add_n For `pct = "row"` or `pct = "col"`, set to `FALSE` not to add another
#' column or row with unweighted counts (`n`).
#' @param add_pct Set to `TRUE` to add a column with the frequencies of the row
#' variable (for `pct = "row"`) or a row with the frequencies of the column variable
#' (for  `pct = "col"`).
#' @param common_totrow With several \code{row_vars}, `FALSE` (the default) shows one Total row per
#' row variable. Set to `TRUE` to collapse the identical Total rows into a single shared Total,
#' displayed in its own group after a blank-line separator (bold when the total is the reference for
#' at least one row variable). Genuinely different totals (e.g. under `na = "drop"`) are never merged.
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
#' @param names_prefix,names_sort `r lifecycle::badge("deprecated")` These belong to
#'  \code{\link{tab_spread}}, which is the function that names the new columns; they reach it only
#'  when \code{spread_vars} is given. Call \code{tab_spread()} yourself for control over the names.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like "1-", and text in parenthesis. All data formatting arguments are
#' passed to \code{\link{tab_prepare}}.
#' @param other_if_less_than When set to a positive integer, levels with less count
#' than it will be merged into an "Others" level.
#' @param other_level The name of the "Other" level, as a single string.
#' @param filter `r lifecycle::badge("superseded")` A
#' \code{\link[dplyr:filter]{dplyr::filter}} to apply to the data frame first, as a single string
#' (which will be converted to code, i.e. to a call). Prefer filtering the data with
#' \code{\link[dplyr:filter]{dplyr::filter}} upstream of \code{tab()}; this argument is kept
#' for back-compatibility (e.g. printing multiple tabs from a
#' \code{\link[tibble:tribble]{tibble::tribble}}).
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
#' @details
#' \strong{Ordered factors.} Since v2.0.0 the \code{ordered} class survives the whole pipeline (it
#' used to be stripped at preparation), which is what lets \code{OR = "cumOR"} pick its col_vars by
#' class. One consequence worth knowing: the synthetic \code{"Total"} / \code{"Ensemble"} / \code{"NA"}
#' levels are appended \emph{after} the real ones, so on an ordered grouping column they compare as the
#' greatest levels. They are labels, not points on the scale.
#'
#' \strong{Weighted confidence intervals.} With a weight (\code{wt}), by default a cell confidence
#' interval is exactly \code{Wilson(weighted p, unweighted n = tot_n)}: it treats the weighted
#' proportion as if it came from \code{tot_n} independent Bernoulli trials (means use the unweighted n
#' the same way). Under unequal weights this carries no design effect, so the default interval is
#' \strong{usually too narrow} --- and the table's footer now says exactly that.
#'
#' \code{design_effect = TRUE} (or \code{options(tabxplor.design_effect = TRUE)} for a whole session)
#' corrects it, \strong{exactly}: a weight column is the
#' flat survey design \code{ids = ~1}, whose variance has a closed form in the per-cell
#' \code{sum(w^2)} the aggregate already computes, so the base becomes
#' \code{n_eff = p(1-p) / Var_design(p)} (or \code{s^2 / Var_design(mean)}) in \strong{every}
#' descriptive interval --- proportions and means alike (cell, difference, ratio, and the
#' \code{color = "OR"} significance). Because it is the exact variance and not an upper bound, it can
#' also make an interval \emph{narrower}: where the weights line up with what is being tabulated,
#' \code{n_eff} comes out above the cell's own \code{n}, which is correct and not a bug. It reproduces
#' \code{survey} to the last digit, including \code{survey}'s own finite-sample factor --- so a table
#' weighted by a \emph{constant} gets an effective n a whisker \emph{below} the raw one
#' (\code{n_base * (N-1)/N}, \code{N} being the number of respondents the table is built on), not
#' exactly \code{n}. It needs the microdata weights, so
#' \code{\link{tab_counts}} on pre-aggregated counts cannot apply it --- such a table says so in its
#' footer instead of claiming a correction it does not have. Kish's \code{(sum w)^2 / sum(w^2)}, which
#' earlier versions used here, is that same formula with each cell's own \code{sum(w^2)} discarded;
#' it survives only as the limit for a cell that carries no information (a 0 \% or 100 \% cell).
#'
#' \strong{Design-based confidence intervals.} Pass a \code{survey::svydesign} as \code{data} and the
#' same base comes from \code{survey::svyrecvar} on each cell's influence function, so strata,
#' clusters, \code{fpc} \emph{and} calibration reach every interval, star and colour threshold; a flat
#' \code{svydesign(ids = ~1)} takes the closed form instead, which is its exact answer. Every interval
#' is then referred to the design's own degrees of freedom (\code{#PSU - #strata}), which matters below
#' about 30 PSUs. Two things to know. It is exact for a \emph{cell} and mildly conservative for a
#' cell-vs-reference \emph{difference} (it cannot carry the design covariance between two rows), so it
#' never produces a star the design does not support, and sometimes withholds one it would. And the
#' \code{color = "contrib"} residual takes the \emph{whole table's} design effect (Rao-Scott's
#' delta-bar, the one its omnibus test reports) rather than each cell's own --- the standard
#' first-order correction, and what makes a counts table and a percentage table of the same data give
#' identical residuals. An exact per-cell design residual would need each cell's own influence
#' function, which the aggregate does not carry. If a table's design variance cannot be computed at
#' all, it falls back to the weighting-only correction and its footer says so, rather than claiming a
#' design its numbers do not carry. A design-based table costs roughly 3x a
#' weighted one (6x if calibrated); the payoff needs real design information, so if your file ships one
#' calibrated weight and no stratum or cluster variable, \code{tabxplor.design_effect} is already all
#' the correction available to you.
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
#'   dplyr::arrange(is_totrow(pick(everything())), desc(category))
#'}
#'
#'\donttest{
#' # With `dplyr::arrange`, don't forget to keep the order of tab variables and total rows:
#' tab(data, race, marital, year, pct = "row") |>
#'   dplyr::arrange(year, is_totrow(dplyr::pick(dplyr::everything())), desc(Married))
#'   }
#'
#' @seealso
#'   [tab_many()] (the full-featured engine behind `tab()`) and [tab_reg()] (regression tables).
#'   Go further with the helper functions: [tab_ci()] (confidence intervals and their methods),
#'   [set_color_breaks()] / [set_color_palette()] / [set_color_style()] (colors),
#'   [tab_chi2()] (statistical tests), [tab_pct()] / [tab_tot()] (percentages and totals).
#'   Export a table with [tab_xl()] (Excel), [tab_kable()] (HTML), [tab_md()] (Markdown) or
#'   [tab_plot()], and CHART it with [forest_plot()] (every cell's estimate, interval and colour --
#'   `tab_plot()` renders the table as an image, `forest_plot()` is the real chart).
#'   Package-wide defaults live in [tabxplor-options].
#'
#'   `color = "contrib"` shows each cell's departure from the **log-linear model of independence**
#'   (that is what the chi-squared is), so it reads as a heatmap of the association pattern. For the
#'   specialist contingency-table models built on top of it --- quasi-independence, Goodman's RC
#'   association models, UNIDIFF --- see the \pkg{logmult} package
#'   (\url{https://cran.r-project.org/package=logmult}), which also supports complex survey designs.
tab <- function(data, row_vars, col_vars, tab_vars, wt, sup_cols,
                pct = "no", color = "no", color_signif = "ignore",
                OR = "no", test = FALSE, anova = NULL,
                na = "keep", levels = "all",
                cleannames = NULL, #compact = NULL, # pvalue_line = NULL,
                other_if_less_than = 0, other_level = "Others",
                ref = "auto", ref2 = "first", comp = "tab",
                ci = "auto", conf_level = conf_level_default(), stars = NULL,
                ci_method = NULL, design_effect = NULL,
                method_cell = NULL, method_diff = NULL,
                totaltab = "line", totaltab_name = "Ensemble",
                tot = c("row", "col"), total_names = "Total",
                add_n = TRUE, add_pct = FALSE, common_totrow = FALSE,
                subtext = "", digits = 0, n_min = 0, display = NULL,
                color_breaks = NULL,
                output_list = FALSE, parallel = NULL,
                spread_vars, names_prefix = NULL, names_sort = FALSE,
                row_var, col_var,
                chi2 = lifecycle::deprecated(),
                .cache = NULL, .defer_level_merge = FALSE, .return_armed = FALSE,
                .levels_order = NULL,
                filter) {

  # Phase 19i: THE argument boundary -- validation + every "one rule written N times" derivation --
  # runs once, here, in tab_resolve_common_args() (R/tab-resolve.R), shared with tab_many(),
  # tab_plain(), tab_num(), tab_counts() and the jamovi bridge. What stays in this function is what
  # is genuinely tab()'s: the tidy-select of the four variable roles, the survey unwrap, and the
  # `na` -> population translation.
  # WARNING: it must run BEFORE the tidy-select block below, because `chi2` -> `test` and the `OR`
  # route change values that block does not touch but the tab_build() call does.
  .a <- tab_resolve_common_args(
    "tab", test = test, chi2 = chi2, color = color, color_signif = color_signif,
    ci = ci, stars = stars, conf_level = conf_level,
    ci_method = ci_method, method_cell = method_cell, method_diff = method_diff,
    cleannames = cleannames, OR = OR, display = display, ref = ref, ref2 = ref2,
    tot = tot, total_names = total_names, na = na, levels = levels, pct = pct,
    comp = comp, totaltab = totaltab, n_min = n_min, anova = anova,
    user_env = rlang::caller_env())
  test <- .a$test ; cleannames <- .a$cleannames ; stars <- .a$stars ; ci_method <- .a$ci_method
  display <- .a$display ; ref <- .a$ref ; ref2 <- .a$ref2
  color_spec <- .a$color_spec ; color <- .a$color
  total_names <- .a$total_names ; tot <- .a$tot

  # Phase 6f (§6): singular row_var/col_var are soft-deprecated aliases of the plural
  # row_vars/col_vars (which now accept one variable OR several). Capture the effective quosure
  # once via enquo() (never evaluate the tidy-select arg), nudging users of the old names.
  .rv_dep <- rlang::enquo(row_var)
  .cv_dep <- rlang::enquo(col_var)
  row_var_quo <- if (!rlang::quo_is_missing(.rv_dep)) {
    lifecycle::deprecate_soft("2.0.0", "tab(row_var = )", "tab(row_vars = )")
    .rv_dep
  } else rlang::enquo(row_vars)
  col_var_quo <- if (!rlang::quo_is_missing(.cv_dep)) {
    lifecycle::deprecate_soft("2.0.0", "tab(col_var = )", "tab(col_vars = )")
    .cv_dep
  } else rlang::enquo(col_vars)

  # Phase 18z14-i: a prebuilt survey design passed as `data` is unwrapped at THE one boundary
  # (R/survey-design.R) -- its model frame drives the whole pipeline, its weights become the weight
  # column, and the design itself drives the test p-values (Rao-Scott). tab()'s CIs stay the
  # weighted-point + n approximation (S14) until z14-ii.
  svy <- svy_unwrap_data(data, "tab")
  if (!is.null(svy)) data <- svy$data


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

  # Phase 7a: `sup_cols` is soft-deprecated -- `col_vars` already accepts several variables, so
  # supplementary columns go there with `levels = "first"`.
  sup_cols_quo <- rlang::enquo(sup_cols)
  if (quo_miss_na_null_empty_no(sup_cols_quo)) {
    sup_cols <- character()
  } else {
    lifecycle::deprecate_soft(
      "2.0.0", "tab(sup_cols = )",
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

  # Defused here and forwarded as a VALUE (see tab_build()'s WARNING): a quosure, or NULL when absent.
  filter_quo <- rlang::enquo(filter)
  if (rlang::quo_is_missing(filter_quo) || rlang::quo_is_null(filter_quo)) filter_quo <- NULL

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }

  # A survey design carries its own weights -> they ARE the weight column, so the estimates are
  # design-weighted. Passing BOTH is a contradiction and aborts (W10) rather than silently dropping
  # the user's column, as every other variable-role collision in tab() does.
  if (!is.null(svy)) {
    svy_abort_wt_design(length(wt) != 0L)
    wt <- rlang::sym(svy$spec$wt)
  }
  else if (length(wt) && identical(as.character(wt)[1], svy_wt_col))
    cli::cli_abort(c("{.val {svy_wt_col}} is a name tabxplor reserves for a survey design.",
                     "i" = "Rename that column, or pass a {.fn survey::svydesign} as {.arg data}."))

  # `test` says only WHETHER to test; the BASIS (n / weights / design) is derived once in tab_setup()
  # -- see svy_inference_basis() in R/survey-design.R. `test_on` is the boundary's resolved logical.
  test_on     <- test
  design_spec <- svy$spec

  # Phase 19h: `pct` is per COL_VAR, like `levels` and `digits` -- it used to be the odd one out,
  # size-1-asserted, although the engine has always recycled it (that is how tab_many() offered it).
  # A per-ROW_VAR list stays refused: Phase 6 globalised the row axis on purpose (§5). (It is a
  # SHAPE refusal, not a vocabulary one, so it stays here rather than in TAB_ARG_VALUES.)
  if (is.list(pct))
    cli::cli_abort(c(
      "{.arg pct} is per {.arg col_vars}, so it must be a character vector, not a list.",
      "i" = "The row-variable axis is global in {.fn tab}: for different percentages per row
             variable, build one {.fn tab} per variable."))
  # Phase 6d (§4): `ref` may be a (named) vector -- one reference row per row_var -- so it is NOT
  # size-1-asserted. tab_build() matches names to row_vars (else by order); scalar applies to all.
  vctrs::vec_assert(ref2, size = 1)
  # Phase 6 (§5): the row_var axis is globalised -- ci/chi2 (like comp/pct/ref/ref2) apply to
  # ALL row_vars. For genuinely different settings per variable, build separate tab()s and list
  # them. (The col_var axis stays flexible: pct/levels/digits are still per col_var in tab_many.)
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

  # Phase 19h: the deprecated `sup_cols` axis is folded into the col_var axis ONCE, here, instead of
  # being mirrored into three separate arguments of the call below (col_vars / levels / pct), where
  # a fourth mirror -- `ref` -- had already been written and commented out.
  sup <- tab_deprecate_sup_cols(sup_cols, col_var, levels, pct)

  result <- tab_build(data = data,
           row_vars = tidyselect::all_of(row_var),
           col_vars = tidyselect::all_of(sup$col_vars),
           tab_vars = tidyselect::all_of(tab_vars),
           wt = !!wt,
           levels = sup$levels,
           na = na_effective, na_drop_all = tidyselect::all_of(na_drop_all),
           # defused here, passed as a VALUE: `{{ }}` inside the `if` used to defuse the `if` itself.
           filter = filter_quo,
           digits = digits,
           cleannames = cleannames,
           output = if (isTRUE(output_list)) "list" else "single", #pvalue_line = pvalue_line,
           other_if_less_than = other_if_less_than, other_level = other_level,
           totaltab = totaltab, totaltab_name = totaltab_name,
           common_totrow = common_totrow,
           totrow = .a$totrow,
           # Phase 6e (§6): exactly ONE total column by default. With several main col_vars the
           # per-col_var totals are redundant (all equal each row's base for row%, and the
           # row_var marginal for col%), so "last" shows a single total column. For one col_var
           # this is byte-identical to the historical per-col_var total.
           totcol = .a$totcol,
           total_names = total_names,
           pct  = sup$pct,
           ref = ref, ref2 = ref2, #c(ref, rep(ref , length(sup_cols))),
           comp = comp,
           # tab_build()'s internal arg keeps the `chi2` name (it drives tab_chi2(); the ANOVA arm
           # branches inside tab_transform()); only the PUBLIC tab() surface is renamed. `test_on` is the
           # boolean; `design_spec` carries the design, from which tab_setup() derives the
           # INFERENCE BASIS (R/survey-design.R).
           chi2 = test_on,
           # Phase 19k: WHICH stored one-way F the p-value line shows (display intent, NULL = the
           # global option) -- see tab_anova().
           anova = anova,
           design_spec = design_spec,
           ci = ci,
           conf_level = conf_level,
           stars = stars,
           ci_method = ci_method, design_effect = design_effect,
           color = color,
           # Phase 14a: the NORMALIZED policy (post the "color_all_signif" COMPAT rename), so
           # tab_resolve_settings() can force the difference CI a gated colour needs.
           color_signif = color_spec$signif,
           # Phase 14b: same reason -- the two-channel spec, not the legacy string, knows whether the
           # ratio is the measure the reader sees (and so owns the stored interval).
           color_ratio_ci = color_pct_text_is_ratio(color_spec),
           # Phase 19d: the SECOND link of the comparison chain -- what the table shows names the
           # comparison when the colour does not (study SS8.6 caveat 3).
           display = display,
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

  # The shared wrapper tail (finalize colour spec -> display recipe -> per-table breaks). Same three
  # steps in tab_many()/tab_num()/tab_counts(); see finalize_color_tail() below.
  result <- finalize_color_tail(result, color_spec, color_breaks, display)

  # Phase 17g: the `tabxplor.output_kable` convenience render runs HERE -- AFTER colour finalisation,
  # tab_apply_display and the color_breaks attr -- because tab_kable() consumes the FINALISED table via
  # format(). It used to run inside tab_assemble_output() (pre-finalize), which both crashed on a
  # two-channel colour (finalize_color_spec then mutate()d the returned tabxplor_kable) and rendered a
  # pre-finalize table (missing the background channel). merge_now (tab_assemble_output) still forces the
  # merge in the build -- that is a build concern; only the render moved.
  if (isTRUE(getOption("tabxplor.output_kable"))) return(tab_html(result))

  # Phase 13c-iv: a multi-table result becomes a tabxplor_tabs (still a list) so it auto-prints like a
  # single tab and routes to the Viewer under options("tabxplor.print" = "kable"). No-op on a single tab.
  as_tabxplor_tabs(result)
}




# The shared wrapper tail every public entry point runs after the engine returns the PRE-finalise
# table: set the two-channel colour + significance-policy attributes (finalize_color_spec), apply the
# opt-in composite display recipe (tab_apply_display, a no-op on NULL), then store the per-table
# color_breaks override LAST (set_color_breaks_attr, so no earlier step strips it). Extracted so
# tab()/tab_many()/tab_num()/tab_counts() cannot drift. Callers keep their own trailing steps
# (tab()'s output_kable / as_tabxplor_tabs; tab_num()'s df||num early return).
#' @keywords internal
#' @noRd
finalize_color_tail <- function(result, color_spec, color_breaks = NULL, display = NULL) {
  result <- finalize_color_spec(result, color_spec)
  result <- tab_apply_display(result, display)
  set_color_breaks_attr(result, resolve_color_breaks_arg(color_breaks))
}



# Phase 17d (Step 4d): decode a legacy COMBINED colour string into the clean (measure, policy) pair,
# ONCE, at the argument / storage boundary -- so the resolve cascade, the stored `color` attribute and
# the colour engine (fmt_color_plan) never carry a composite and never re-parse one. A clean measure
# passes through with policy = NULL ("leave color_signif as it is"). The one-shade rendering of the old
# `color = "ci"` (single0) is retired: it folds into guaranteed_effect, i.e. exactly `after_ci`.
# Phase 19c: the decoding IS the declared COLOR_ALIASES table (R/fmt_class.R), so `"ci"` stopped being
# a third switch arm and became a third row -- and the same table is what makes names(MEASURES) +
# names(COLOR_ALIASES) the one allow-list.
#' @keywords internal
color_decode_legacy <- function(color) {
  a <- COLOR_ALIASES[[color]]
  if (is.null(a)) list(measure = color, policy = NULL) else a
}

# Phase 13a: `color` grammar -- POSITION = channel (1st -> text, 2nd -> background), NAMES = column
# type (pct / mean). FALSE -> off; TRUE -> the smart per-type default; a scalar/positional vector ->
# the same measure(s) on every column; a NAMED vector or a list(pct =, mean =) -> per column type
# (each entry a positional channel vector). Returns list(mode, legacy, text, bg, types, signif):
# `legacy` is the scalar CLEAN measure fed to the (text-channel) pipeline so its ci/chi2 side effects
# still fire (Phase 17d: no longer a manufactured diff_ci/after_ci -- the policy rides `signif`);
# `mode`/`text`/`bg`/`types`/`signif` drive finalize_color_spec() on the built table.
#' @keywords internal
normalize_color_spec <- function(color, color_signif = "ignore", deprecate = TRUE) {
  signif <- if (length(color_signif) == 0L) "ignore" else color_signif[1]
  if (is.na(signif) || signif %in% c("", "no")) signif <- "ignore"
  # COMPAT (Phase 13a): the renamed policy value, wired through with a soft-deprecation.
  if (identical(signif, "color_all_signif")) {
    lifecycle::deprecate_soft("2.0.0", I('color_signif = "color_all_signif"'),
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
  # Phase 19c: the canonical stored spelling of a token, from the ONE table. "auto" is the sentinel
  # this parser must pass through untouched (it is resolved per column TYPE downstream), so it is the
  # only non-measure word the boundary knows.
  # WARNING: it must run AFTER the alias decode, never before -- measure_key() resolves a
  # policy-carrying alias to its MEASURE, so normalising first would silently discard the policy half
  # of "diff_ci"/"after_ci"/"ci" (measured: 18 cases lost their color_signif and their forced CI).
  norm       <- function(m) {
    if (is.na(m) || identical(m, "no")) return("")
    if (identical(m, "auto")) return("auto")
    k <- measure_key(m); if (is.na(k)) as.character(m) else if (!nzchar(k)) "" else k
  }

  # WARNING: `deprecate = FALSE` is not a convenience -- it is required on the internal seam.
  # legacy_union() MANUFACTURES "diff_ci"/"after_ci" (e.g. color = "ratio" + color_signif =
  # "grey_non_signif" -> "diff_ci") and tab_transform() hands that string to tab_num(), which
  # re-parses it here. Deprecating then blames the user for a string the pipeline wrote. The `uenv`
  # heuristic above is not enough: lifecycle's from_testthat() deliberately forces a package's own
  # internal soft-deprecations to warn while its suite runs, so the false positive surfaces in
  # tabxplor's tests -- and in the tests of any package that calls tab() on a numeric column.
  deprecate_old <- function(text) {
    if (!deprecate) return(invisible(NULL))
    # Phase 19c: the soft-deprecated spellings are the COLOR_ALIASES rows that carry a POLICY (a
    # colour string that also says how significance gates it) -- derived, so the list cannot drift.
    if (text %in% color_legacy_spellings()) {
      lifecycle::deprecate_soft(
        "2.0.0",
        I(paste0("The `color = \"", text, "\"` mode")),
        with = I("`color = \"diff\"` with the `color_signif` argument"),
        user_env = uenv)
    }
  }

  # one positional channel vector (text[, background]) -> c(text, bg-or-NA) validated measures
  parse_channels <- function(v) {
    v   <- unname(as.character(v))
    raw <- if (length(v) >= 1L) v[1] else NA_character_
    deprecate_old(raw)
    # Phase 17d: decode a legacy combined string HERE, once. The policy rides `signif` (scalar for the
    # whole spec); the measure becomes a clean "diff". A clean measure is left untouched (policy NULL).
    dec  <- if (is.na(raw)) list(measure = raw, policy = NULL) else color_decode_legacy(raw)
    if (!is.null(dec$policy)) signif <<- dec$policy
    text <- norm(dec$measure)
    bg   <- if (length(v) >= 2L) v[2] else NA_character_
    # A combined string is a (measure, policy) PAIR and the policy is scalar for the whole spec, so it
    # cannot describe a second channel. Refuse it rather than silently keep its measure half.
    if (!is.na(bg) && !is.null(COLOR_ALIASES[[bg]]$policy)) {
      cli::cli_abort(c("{.val {bg}} cannot go on the background channel.",
                       "i" = "It also names a significance policy; set that with {.arg color_signif}."))
    }
    bg <- if (is.na(bg)) NA_character_ else norm(bg)
    if (!is.na(bg) && bg == "") bg <- NA_character_
    # Phase 19c: ONE validator, shared with the storage boundary (resolve_color_channels). Called with
    # producer = "tab", so a measure only tab_reg() can build is refused HERE, with a message
    # GENERATED from its `producers` field -- the hand-written "that is a tab_reg measure" hint and
    # the hand-written background allow-list are both gone, and with them D4 (the two lists disagreed
    # about whether the gap measures may ride the background; they may, and they say so once).
    # `auto` is this boundary's own sentinel, resolved per column type downstream, so it is exempt.
    if (identical(text, "auto")) {
      if (!is.na(bg)) measure_validate(c("", bg), producer = "tab", call = rlang::caller_env())
    } else {
      measure_validate(c(text, if (is.na(bg)) NULL else bg), producer = "tab",
                       call = rlang::caller_env())
    }
    c(text, if (is.na(bg)) NA_character_ else bg)
  }

  # the pipeline CLEAN measure demanded by a set of measures (ci/chi2/OR side-effects). Phase 17d: it no
  # longer manufactures diff_ci/after_ci -- the legacy strings are already decoded (parse_channels) into
  # a "diff" measure + the `signif` policy, so the cascade reads the clean measure and color_signif apart.
  # Phase 19c: the four hand-ordered `if`s are ONE walk down the declared COLOR_BUILD_ORDER -- the
  # pipeline computes the strongest build class any channel asks for, and the weaker channel derives
  # from the fields that pass already produced (which is why `ratio` resolves to the diff class: the
  # leaf computes diff AND ratio together, and ci_scale tags which of the two owns the interval).
  legacy_union <- function(ms) {
    ms <- ms[!is.na(ms) & ms != ""]
    if ("auto" %in% ms) return("auto")   # resolved per column type downstream (tab_resolve_settings)
    builds <- vapply(ms, measure_builds, character(1), USE.NAMES = FALSE)
    for (b in COLOR_BUILD_ORDER) if (b %in% builds) return(measure_of_build(b))
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
    lifecycle::deprecate_soft("2.0.0", I('color = c(text = , background = )'),
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
# color + what the column is. NULL = leave the column as the pipeline built it (e.g. contrib/OR under
# a pct/mean spec, or a kind the spec does not mention).
# Phase 19b: `numeric` = "this column does not summarise a percentage" (a mean, a count, a
# coefficient) and `pct` = "it has a percentage base" -- the two halves the old 8-value `type` was
# being partitioned into here, by two hand-written vectors, in four places.
#' @keywords internal
resolve_col_measures <- function(spec, numeric_col, pct_col, built) {
  # Phase 19c: "the pipeline built a measure this per-column pass must not repaint" IS "that measure
  # is not selected by a column KIND" -- `or` and `contrib` are the automatic answer for a whole
  # TABLE (an OR table, a counts table), never for a percentage-vs-mean column, so a `pct =` / `mean =`
  # spec does not name them. Two `built %in% c("OR","contrib")` literals became one declared question,
  # and the per-kind defaults themselves are now MEASURES' own `auto_for`.
  kind <- if (numeric_col) "num" else if (pct_col) "pct" else NA_character_
  if (spec$mode == "auto") {                                # color = TRUE smart per-kind default
    # a whole-table measure: OR is re-stamped in its canonical spelling, contrib kept as built
    if (!measure_kind_keyed(built))
      return(if (identical(measure_builds(built), "or")) "odds_ratio" else NULL)
    if (is.na(kind)) return(NULL)
    m <- c(measure_auto(kind, "text"), measure_auto(kind, "bg"))
    m <- m[nzchar(m)]
    return(if (length(m) == 0L) NULL else unname(m))
  }
  if (spec$mode == "by_type") {
    if (!measure_kind_keyed(built)) return(NULL)            # keep what the pipeline built
    key <- if (numeric_col) "mean" else if (pct_col) "pct" else NA_character_
    if (is.na(key) || is.null(spec$types[[key]])) return(NULL)
    m <- spec$types[[key]]
    return(if (is.na(m[2])) m[1] else m)
  }
  # flat -- one measure (or a c(text, bg) pair) for every column.
  # Phase 19c: `color = "auto"` -- the STRING form of `color = TRUE` -- also lands here (only the
  # logical takes mode "auto"), and the unresolved sentinel used to be handed straight to set_color(),
  # which ABORTED with "Unknown color measure". Measured on HEAD: every `color = "auto"` combined with
  # any `color_signif` policy errored, on factor and numeric tables alike -- the one shape in which
  # this branch is reached at all (with no policy, finalize_color_spec does not rewrite). It resolves
  # the sentinel per column kind now, exactly as the logical form does.
  text <- spec$text
  if (identical(text, "auto")) {
    if (is.na(kind)) return(NULL)
    text <- measure_auto(kind, "text")
    if (!nzchar(text)) return(NULL)
    if (is.na(spec$bg)) {
      bg <- measure_auto(kind, "bg")
      return(if (nzchar(bg)) c(text, bg) else text)
    }
  }
  if (text == "" && is.na(spec$bg)) return(NULL)
  if (is.na(spec$bg)) text else c(text, spec$bg)
}

#' @keywords internal
finalize_one_col <- function(col, spec) {
  built <- get_color(col)
  if (built %in% c("", "no")) return(col)                  # the pipeline did not color this column
  measures <- resolve_col_measures(spec, fmt_var_kind(col) != "pct", get_pct_base(col) != "none",
                                   built)
  if (is.null(measures)) return(col)
  if (length(measures) == 1L && measures %in% c("", "no")) return(col)
  set_color_signif(set_color(col, measures), spec$signif)
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


# ctx_settings_locals() -- Phase 19i: project the SETTINGS SPINE into the bare names every stage
# reads. THE SPINE IS THE ONLY CARRIER; this is its projection into ONE stage's scope, rebuilt at
# each stage head and never written back into the ctx.
#
# WHY THIS SHAPE. Before 19i the same ~15 facts existed TWICE: `tab_setup()` built the spine and
# then also wrote every one of them flat into the ctx, and `tab_rowvar_ctxs()` sliced the spine only
# to re-flatten it into those same names per row_var (`row_scalar <- setdiff(names(rows),
# "row_var")`). Two carriers for one fact -- so the spine advertised itself as the interface while
# every consumer read the duplicate. The alternative (rewrite ~200 bare reads as `settings$rows$x`)
# would have made the resolution blocks less readable, not more; projecting the spine keeps the
# reading idiom and leaves exactly one carrier.
#
# DESIGN: pre-slice a spine column is a VECTOR over row_vars; post-slice (`tab_rowvar_ctxs()` hands
# each unit `rows[i, ]`) it is a length-1 slice, i.e. the scalar the per-row_var stages expect. Same
# code, both shapes -- which is exactly the property the flat duplicates had, and why they existed.
# `NULL` spine -> `list()`: a hand-built ctx that never ran `tab_setup()` keeps `new_ctx()`'s
# defaults, so the NULL guards in `tab_transform()` stay reachable (Phase 19a, D7).
#
# WARNING: `col_vars_num` / `col_vars_text` are NAMED logicals downstream (`names()` is read to pick
# the total column, `tab.R` ~L2173) but the spine stores them unnamed -- the names are restored here
# from `cols$col_var`. Dropping them silently returns NULL from that `names() |> last()`.
#
# CTX_SETTINGS_LOCALS = the names this produces, declared so codetools can be told about them below
# (they are bindings no static reader can see) and so a spine column added without a projection --
# or a projection with no home in the spine -- fails the assert rather than going quiet.
#' @keywords internal
#' @noRd
CTX_SETTINGS_LOCALS <- c(
  # settings$rows, minus its key (na_num is added by tab_prepare_pop)
  "color", "comparison", "or_ci", "chi2", "ref", "ref2", "comp", "ci", "ci_scale",
  "totaltab", "totrow", "na_num",
  # settings$cols (lv1 added by tab_prepare_pop)
  "lvs", "lv1", "digits", "col_vars_num", "col_vars_text",
  # settings$pairs (na added by tab_prepare_pop)
  "pct_vect", "ref_vect", "ref2_vect", "na_text"
)

#' @keywords internal
#' @noRd
ctx_settings_locals <- function(ctx) {
  s <- ctx$settings
  if (is.null(s)) return(list())
  # WARNING: `[[`, never `$` -- the spine is filled in TWO stages (`lv1` / `na` / `na_num` are
  # tab_prepare_pop's), and tibble's `$` warns "Unknown or uninitialised column" on the earlier ones.
  out <- c(as.list(s$rows[setdiff(names(s$rows), "row_var")]),
           list(lvs           = s$cols[["lvs"]],
                lv1           = s$cols[["lv1"]],
                digits        = s$cols[["digits"]],
                col_vars_num  = stats::setNames(s$cols[["is_num"]] , s$cols[["col_var"]]),
                col_vars_text = stats::setNames(s$cols[["is_text"]], s$cols[["col_var"]]),
                pct_vect      = s$pairs[["pct"]],
                ref_vect      = s$pairs[["ref"]],
                ref2_vect     = s$pairs[["ref2"]],
                na_text       = s$pairs[["na"]][s$pairs[["is_text"]]]))
  # one-directional: the spine is filled in two stages, so a column may legitimately be absent here
  # (before tab_prepare_pop) -- but never present-and-undeclared.
  stopifnot(all(names(out) %in% CTX_SETTINGS_LOCALS))
  out
}


# Phase 17e: the TYPED ctx constructor. The entry ctx used to be a hand-written `list(...)` literal
# in BOTH tab_build() and tab_counts() (which drift), with ~7 downstream fields left absent and
# defaulted by scattered `exists(<field>, inherits = FALSE)` guards. new_ctx() gives every field a
# single default in ONE place, so every ctx (tab_build's, tab_counts's, a hand-built stage-test's)
# carries the full field set and the guards are deletable. The body is ctx_update(defaults, list(...)):
# DESIGN: the single-bracket `[<-` means an explicitly-passed `totcol = NULL` (or fine_fused = NULL) is
# written as a PRESENT-but-NULL key -- the NULL-preservation rule the downstream list2env() relies on,
# now encoded in the helper instead of three comment sites. Callers pass CTX field names (by_table,
# cache_env, defer_level_merge, levels_order), not tab_build's dot-prefixed formals.
#' @keywords internal
#' @noRd
new_ctx <- function(...) {
  defaults <- list(
    # NSE carriers (defused by the caller; a plain default is never consumed on a real path)
    # Phase 19k (D13): `filter_expr` is the filter as a SYMBOLIC string (NA = none), and it is the
    # only carrier -- "is there a filter" is derived from it. It exists because the cache keys need
    # to discriminate two calls that differ only by their filter: tab_setup() used to hand
    # tab_cache_keys() a hardcoded NA_character_, so `tier0$filter` was constant and a filter change
    # never invalidated the jamovi tier-0/tier-1 entries.
    data = NULL, filter_expr = NA_character_,
    row_vars_quo = NULL, col_vars_quo = NULL, tab_vars_quo = NULL,
    wt_quo = NULL, na_drop_all_quo = NULL,
    # inputs (= each formal's current default)
    pct = "no", color = "no", color_signif = "ignore", color_ratio_ci = FALSE,
    # Phase 19k: WHICH one-way F a mean col_var's p-value line shows. NULL = the global option.
    anova = NULL,
    # Phase 19d: `display` is a ctx INPUT now -- not to be applied in the build (it stays the tail's
    # job) but because it is the SECOND link of the comparison chain, which tab_setup() resolves.
    display = NULL, chi2 = FALSE, design_spec = NULL,
    # Phase 18z16-iiiii: "this call holds a pre-aggregate, not microdata" -- declared by
    # tab_counts(), read ONCE by tab_setup()'s svy_inference_basis(can_serve =). Such an input carries
    # no per-observation Sum(w^2), so it cannot serve the weighted basis and must not claim it.
    agg_only = FALSE,
    na = "keep", levels = "all",
    cleannames = NULL, output = "single",
    other_if_less_than = 0, other_level = "Others",
    ref = "auto", ref2 = "first", comp = "tab",
    ci = "auto", conf_level = 0.95, stars = NULL,
    # tab_setup INPUTS: `conf_level`, `ci_method`, `design_effect`, `design_spec` and `agg_only` are
    # read there and resolved into ONE `inference` object (new_inference()). Nothing downstream of
    # tab_setup() reads them -- the leaves, the tests and the assembler take `inference` whole.
    ci_method = default_ci_method(), design_effect = NULL,
    inference = new_inference(),
    totaltab = "line", totaltab_name = "Ensemble", totrow = TRUE, totcol = "last",
    total_names = "Total", add_n = TRUE, add_pct = FALSE, common_totrow = FALSE, digits = 0,
    subtext = "", n_min = 0, by_table = FALSE, parallel = NULL,
    spread_vars = character(), names_prefix = NULL, names_sort = FALSE,
    # the three jmvtab seams. Phase 19a re-examined `levels_order` (study §5 said "cut from ctx, pass
    # directly") and KEPT it: its one reader is jmv_cache_aggregate(ctx), reached through
    # tab_aggregate()'s `if (!is.null(ctx$cache_env)) return(...)` hook, which passes nothing but the
    # ctx -- so there is no "directly" to pass it. It is a DECLARED field with one legitimate reader,
    # exactly like its two neighbours here. Revisit in 19k, which owns the jamovi boundary.
    cache_env = NULL, defer_level_merge = FALSE, levels_order = NULL,

    # --- STAGE PRODUCTS: written by one stage, read by a later one -----------------------------
    # Phase 19i: they are DECLARED, not left to appear. An undeclared key is simply ABSENT, and
    # `list2env(ctx, environment())` creates no binding for an absent key -- so its own NULL guard
    # does not return TRUE, it ERRORS ("object not found"). That is exactly the class of bug 19a's D7
    # was (`is.null(ref_vect)` on an undeclared field), and 54 declared keys against ~81 live ones
    # left 27 more of them possible. Declaring costs one line each and makes the ctx self-describing:
    # a reader can see what a stage may find without running the pipeline.
    # NOTE: the per-row_var / per-col_var / per-pair SETTINGS are not here -- they ride
    # `settings` and reach each stage through ctx_settings_locals(). What is listed is what the
    # spine's three grains cannot express.
    #
    # tab_setup:        the resolved variable roles + the arg products no grain fits
    settings = NULL, row_vars = NULL, col_vars = NULL, tab_vars = NULL, wt = NULL,
    tab_row_names = NULL, na_drop_all = NULL, tot_cols_type = NULL, cache_keys = NULL,
    # (`totcol` is an INPUT above -- tab_setup resolves it in place, it is not a second field)
    # tab_prepare_pop:  the non-first levels dropped at display time (NULL = nothing to drop)
    remove_levels = NULL,
    # tab_aggregate:    the tier-1 aggregates + the two jmvtab cache products
    fine_num = NULL, fine_fused = NULL, cached_tests = NULL, tier2_keys = NULL,
    # tab_transform:    this row_var's built tables + its whole-table tests
    tabs_text = NULL, tabs_num = NULL, chi2_num = NULL,
    # Phase 18z16-iv (W-B): the robust omnibus GRID, produced once in tab_transform() because two
    # consumers need it -- the contrib residual's base (there) and the `test` overlay (assemble).
    robust_tests = NULL,
    # tab_*_tables:     the finished per-row_var tab(s) + the tier-2 test store
    tabs = NULL, tests = NULL,
    # Phase k: variable labels (name -> label) captured in tab_setup for the opt-in name display-swap
    var_labels = character()
  )
  ctx_update(defaults, list(...))
}
# (the derived globalVariables() declaration for these fields is at the END of this file -- see there)


# Phase 17e: single-source the two "resolve NULL -> option / force default" rules that were copy-pasted
# across the pipeline and its public leaves. (Full leaf-side removal waits on the 17f wrapper/core split;
# these keep the leaves callable directly while the logic lives in ONE place.)
# resolve_stars(): NULL -> the tabxplor.stars option (else the explicit value). Since 19i it is called
# at ONE place per producer -- tab_resolve_common_args() for tab()/tab_plain()/tab_num()/tab_counts(),
# plus tab_ci() (a self-contained step entry). It used to be resolved at three different DEPTHS, and
# tab_num()'s was so late that `resolve_leaf_ci()` had already tested a NULL: see the fixture in
# test-arg-boundary.R. force_comp(): comp = "all" is meaningless without tab_vars -> collapse to
# "tab". Sites: the two leaf resolvers plain_resolve / num_resolve.
#' @keywords internal
#' @noRd
resolve_stars <- function(stars) {
  if (is.null(stars)) getOption("tabxplor.stars", FALSE) else stars
}
#' @keywords internal
#' @noRd
force_comp <- function(comp, tab_vars) {
  if (length(tab_vars) == 0 && all(comp == "all")) "tab" else comp
}

# Phase 19a: the same shape for the two remaining "one rule, written N times" argument defaults.
#
# resolve_cleannames(): NULL -> the tabxplor.cleannames option. Sites: tab(), tab_setup(),
# tab_prepare(), tab_counts(), tab_reg(). The five copies had DRIFTED -- the tab_reg one passed a
# `FALSE` fallback the other four lacked, so with the option unset (it is not, .onLoad sets FALSE)
# four of them yielded NULL and one yielded FALSE. Single-sourcing settles it on the safe fallback.
#
# conf_level_default(): THE default confidence level, as a formal default. It was the literal
# `getOption("tabxplor.conf_level", 0.95)` in TEN signatures (tab, tab_many, tab_plain, tab_num,
# tab_ci, tab_counts, tab_reg, tab_logit, multi_logit, new_inference). The option is still what it
# reads, and ?tabxplor-options + each @param still name it -- only the ten copies of the expression
# are gone.
#' @keywords internal
#' @noRd
resolve_cleannames <- function(cleannames) {
  if (is.null(cleannames)) getOption("tabxplor.cleannames", FALSE) else cleannames
}
#' @keywords internal
#' @noRd
conf_level_default <- function() getOption("tabxplor.conf_level", 0.95)


# tab_build() -- the shared table-building engine behind tab() and tab_many().
# Stages: prep-once (whole DB) -> aggregate -> transform -> assemble. Both public entry points
# are thin wrappers differing only in the default `output` shape they pass. Kept internal (not
# exported) so a future Jamovi caching layer can drive the same core without any deprecation
# nudge, and so tab() never triggers tab_many()'s soft-deprecation.
#   `output`: "single" merges >=2 row_vars into one table (the tab() default); "list" always
#   returns a list, incl. length 1 (tab(output_list = TRUE)). Phase 19h (KEY 7) deleted the third
#   value "legacy" (a list for >=2 row_vars, a bare table for one) together with its only producer,
#   tab_many()'s pre-shim body -- so the returned SHAPE is now a function of `output` alone. Since
#   19f, a table carrying tab_vars merges like any other.
# WARNING: keep byte-identical to the pre-6b tab_many() body except the intended output-shape
# and option changes.
#' @keywords internal
#' @noRd
tab_build <- function(data, row_vars, col_vars, tab_vars, wt,
                      pct = "no", color = "no", color_signif = "ignore",
                      color_ratio_ci = FALSE,
                      display = NULL, chi2 = FALSE, anova = NULL, design_spec = NULL,
                      na = "keep", levels = "all", na_drop_all,
                      cleannames = NULL, output = "single", #pvalue_line = NULL,
                      other_if_less_than = 0, other_level = "Others",
                      ref = "auto", ref2 = "first", comp = "tab",
                      ci = "auto", conf_level = 0.95, stars = NULL, #ci_visible = FALSE,
                      ci_method = default_ci_method(), design_effect = NULL,
                      totaltab = "line", totaltab_name = "Ensemble",
                      totrow = TRUE, totcol = "last", total_names = "Total",
                      add_n = TRUE, add_pct = FALSE, common_totrow = FALSE,
                      digits = 0, subtext = "", n_min = 0,
                      parallel = NULL,
                      .by_table = FALSE,
                      spread_vars = character(), names_prefix = NULL, names_sort = FALSE,
                      .cache = NULL, .defer_level_merge = FALSE,
                      .levels_order = NULL,

                      # Phase 19h: `filter` is a VALUE here (a quosure / a string / NULL), not an
                      # NSE argument -- see the WARNING at its use site below.
                      filter = NULL #, listed = FALSE,
) {
  # Phase 7d-ii: tab_build is the ARGUMENT SURFACE + the five-stage pipeline. It defuses the NSE
  # args here (where their promises live) and applies `filter` here too. Each stage
  # takes and returns `ctx`; tab_assemble() returns the final tab/list. The stage split matches the
  # jmvtab cache tiers (dev/tabxplor_jmvtab_cache_design.md §8): setup (-) -> prepare_pop (tier 0)
  # -> aggregate (tier 1) -> transform (tier 3 + the tier-2 test) -> assemble (tier 4).

  # Allow to type expression as string in filter (to work with tibble::tribble)
  filter_expr <- NA_character_
  # WARNING (Phase 19h): `filter` reaches this INTERNAL engine already DEFUSED -- a quosure, a plain
  # character string, or NULL -- it is not an NSE argument here. It used to be, and the caller wrote
  # `filter = if (missing(filter)) NULL else {{ filter }}`, which defuses the whole `if` CALL: a bare
  # `filter = !is.na(g)` was then evaluated as `if (missing(filter)) NULL else !is.na(g)` inside the
  # data mask and aborted. Only a character filter ever worked, although ?tab documents a
  # dplyr::filter expression.
  if (!is.null(filter) && !(rlang::is_quosure(filter) && rlang::quo_is_null(filter))) {
    # A character filter (the documented tribble idiom) spells an expression: parse it.
    # WARNING: rlang gives a CONSTANT quosure the EMPTY environment (a literal needs no scope), so
    # re-quoting the parsed call with quo_get_env() would leave it unable to find even `%in%`.
    # Fall back to this frame's caller, whose scope reaches the namespace and the search path.
    if (rlang::is_quosure(filter)) {
      fx <- rlang::quo_get_expr(filter)
      if (is.character(fx)) {
        env <- rlang::quo_get_env(filter)
        if (identical(env, rlang::empty_env())) env <- rlang::caller_env()
        filter <- rlang::new_quosure(str2lang(fx), env)
      }
    } else if (is.character(filter)) {
      filter <- rlang::new_quosure(str2lang(filter), rlang::caller_env())
    }
    data <- data |> dplyr::mutate(.filter = !!filter)
    # Phase 19k (D13): the filter's SYMBOLIC form, carried to tab_cache_keys() via the ctx. It is
    # also the "is there a filter" flag (tab_prepare_pop reads !is.na(ctx$filter_expr)) -- one fact,
    # one carrier. The environment is deliberately NOT hashed: two identical expressions evaluated in
    # different scopes are a case no cache consumer can produce (jamovi never sets `filter`).
    filter_expr <- paste(rlang::as_label(filter), collapse = "")
  }

  # Phase 17e: the entry ctx is built by the typed new_ctx() constructor (defaults in ONE place),
  # not a hand-written list literal. `parallel` gates tab_pmap() (Phase 8); `cache_env`/
  # `defer_level_merge`/`levels_order` are the jmvtab cache seams (Phase 7e/7g-ii), NULL/FALSE here.
  ctx <- new_ctx(
    data = data, filter_expr = filter_expr,
    row_vars_quo = rlang::enquo(row_vars), col_vars_quo = rlang::enquo(col_vars),
    tab_vars_quo = rlang::enquo(tab_vars), wt_quo = rlang::enquo(wt),
    na_drop_all_quo = rlang::enquo(na_drop_all),
    pct = pct, color = color, color_signif = color_signif,
    color_ratio_ci = color_ratio_ci, display = display, chi2 = chi2, anova = anova,
    design_spec = design_spec,
    na = na, levels = levels,
    cleannames = cleannames, output = output,
    other_if_less_than = other_if_less_than, other_level = other_level,
    ref = ref, ref2 = ref2, comp = comp, ci = ci, conf_level = conf_level, stars = stars,
    ci_method = ci_method, design_effect = design_effect,
    totaltab = totaltab, totaltab_name = totaltab_name, totrow = totrow, totcol = totcol,
    total_names = total_names, add_n = add_n, add_pct = add_pct, common_totrow = common_totrow,
    digits = digits,
    subtext = subtext, n_min = n_min, by_table = .by_table,
    parallel = parallel,
    spread_vars = spread_vars, names_prefix = names_prefix, names_sort = names_sort,
    cache_env = .cache, defer_level_merge = .defer_level_merge,
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
                      .ship = list(data = ctx$data, fine_fused = ctx$fine_fused,
                                   design = ctx$inference$design),
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
# (Phase 9a; replaces ctx_slice() + the tabxplor_rowvar_fields constant). Phase 17e: slices the
# SETTINGS SPINE (ctx$settings, built in tab_setup) by explicit KEY -- the former `length(x) == n`
# heuristic (guess "per-row_var iff length happens to equal the row_var count") is GONE.
#
# Phase 19i: it SLICES the spine and stops there. It used to slice it and then RE-FLATTEN every
# column into the ~11 bare names the ctx already carried from tab_setup(), which is what made the
# spine a vehicle rather than an interface; the stages read those names through
# ctx_settings_locals() now, so one slice is the whole job. What is left is the per-row_var work the
# spine's grain cannot express: which row_var symbol this unit is, its tab_row_names, and its moment
# aggregate (sliced by NAME).
#
# Everything else -- scalars, or the shared jmvtab cached_tests list (kept whole; the transform picks
# its row_var entry) -- rides in the shared skeleton. `data` / `fine_fused` are dropped (shipped once
# by tab_pmap); the heavy NSE quosures are dropped (they would drag user data into every mirai task).
#' @keywords internal
#' @noRd
tab_rowvar_ctxs <- function(ctx) {
  rows  <- ctx$settings$rows
  pairs <- ctx$settings$pairs
  n     <- nrow(rows)
  # per-row_var fields carried into each unit, so they must not ALSO ride whole in `shared`:
  per_rv <- c("row_vars", "settings", "tab_row_names", "fine_num")
  # Phase 18z14-i: the survey DESIGN is dropped here and SHIPPED once, like `data` -- a prebuilt
  # design carries its whole `$variables` frame, so riding in `shared` copied the entire dataset into
  # every per-row_var unit while the microdata itself was serialised once. z16-iiiii: it now rides
  # inside the one `inference` object, so only that field is emptied (`[<-` keeps a present-NULL key,
  # which is what tab_build_one() fills back in).
  shared <- ctx[setdiff(names(ctx), c(per_rv, "data", "fine_fused", "design_spec"))]
  shared$inference["design"] <- list(NULL)
  shared <- shared[!grepl("_quo$", names(shared))]
  shared$parallel  <- FALSE     # the worker never spawns nested daemons
  shared$cache_env <- NULL

  lapply(seq_len(n), function(i) {
    rv   <- rows$row_var[i]
    keep <- pairs$row_var == rv
    u <- list()
    u$row_vars      <- ctx$row_vars[i]                             # keep as a length-1 sym list
    u$tab_row_names <- as.character(c(ctx$tab_vars, ctx$row_vars[i]))
    # the slice: this row_var's scalars, the shared per-col_var settings, this row_var's pairs
    # (col order preserved -- pairs is row-major).
    u$settings      <- list(rows = rows[i, ], cols = ctx$settings$cols, pairs = pairs[keep, ])
    u$fine_num      <- ctx$fine_num[[rv]]                          # by NAME (NULL when no numeric cols)
    c(shared, u)
  })
}


# or_cum_ok() / ref2_resolve_cum() -- Phase 18z10, re-homed on `ref2` in 19d: THE
# `ref2 = "cumulative"` eligibility rule, in one place.
#
# A cumulative odds ratio dichotomises a col_var at each cut point ("at or below level j"), which is
# only meaningful on an ORDERED scale, and needs at least 3 levels to say anything a plain OR does not
# (a 2-level factor has one cut, i.e. the ordinary OR). It also reads the ROW distribution, so it is a
# `pct = "row"` quantity. An ineligible pair DEGRADES to the ordinary "first" dichotomisation rather
# than aborting: a table can mix an ordered and a nominal col_var, and only the ordered one has cut
# points. One message site, two reasons -- the "make them ordered" hint the user needs, and the pct one.
#
# Phase 19d: it lives on `ref2` because a cumulative odds ratio is not a different MEASURE, it is a
# different dichotomisation of the column variable -- and `ref2` is precisely "what is each level
# compared against, within the column variable" (ruling b). That is what made `OR` deletable.
#' @keywords internal
#' @noRd
or_cum_ok <- function(x) is.ordered(x) && nlevels(x) >= 3L

#' @keywords internal
#' @noRd
ref2_resolve_cum <- function(ref2, pct, col_vars_cumor) {
  v <- vctrs::vec_recycle(as.character(ref2), length(col_vars_cumor))
  if (!any(v == "cumulative")) return(v)
  want <- v == "cumulative"
  bad_class <- want & !col_vars_cumor
  bad_pct   <- want &  col_vars_cumor & pct != "row"
  if (any(bad_class)) cli::cli_inform(c(
    "i" = paste0("{.code ref2 = \"cumulative\"} needs an {.cls ordered} col_var with 3+ levels; ",
                 "{cli::qty(sum(bad_class))} {?it is/they are} skipped here."),
    "i" = "{.code data |> dplyr::mutate(x = factor(x, levels = c(...), ordered = TRUE))}"
  ))
  if (any(bad_pct)) cli::cli_inform(c(
    "i" = paste0("{.code ref2 = \"cumulative\"} cumulates each row's distribution, so it needs ",
                 "{.code pct = \"row\"}; skipped here.")
  ))
  v[bad_class | bad_pct] <- "first"
  v
}


# === STAGE 1/5: tab_setup() -- resolve arguments + build the settings spine (no cache tier) ==
# Pure argument resolution shared by all downstream stages: tidy-select the four var roles, the
# factor/numeric masks, the per-row_var and per-col_var arg recycling, totcol -> tot_cols_type,
# the colour cascade + cache keys via tab_resolve_settings(), and (Phase 17e) the SETTINGS SPINE
# `ctx$settings` = list(rows, cols, pairs) -- the star schema built ONCE here that tab_rowvar_ctxs()
# slices by KEY. Reads only argument VALUES + column classes -- the data-free boundary the jamovi
# .js mirrors (Phase 7c).
#' @keywords internal
#' @noRd
tab_setup <- function(ctx) {
  # Bring every ctx field into scope as a local so the (verbatim) resolution blocks read as before;
  # the NSE args arrive as *_quo quosures (defused in tab_build), aliased to their plain names below.
  list2env(ctx, environment())

  stopifnot(output %in% c("single", "list"))

  cleannames <-
    resolve_cleannames(cleannames)

  # Phase 3a: significance stars default (universal CI-inclusion). NULL -> option default.
  stars <- resolve_stars(stars)


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

  col_vars <- col_vars_quo
  if (quo_miss_na_null_empty_no(col_vars)) {
    data     <- data |> dplyr::mutate(no_col_var = factor("n"))
    col_vars <- rlang::syms("no_col_var")
    pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else {
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
  }
  tab_vars <- tab_vars_quo
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character() #rlang::syms("no_tab_vars")
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  # Phase k (labelled interop): capture variable labels (the `label` attr) BEFORE labelled
  # conversion strips them, then convert haven/labelled columns to value-label factors. Doing it
  # here -- before the numeric/text classification below -- makes a labelled categorical read as a
  # factor (and an incomplete-labelled numeric keep its real numeric type). The weight is left
  # untouched (it is numeric). var_labels rides ctx into meta$vars for the opt-in name display-swap.
  sel_vars   <- unique(c(as.character(row_vars), as.character(col_vars), as.character(tab_vars)))
  var_labels <- capture_var_labels(data, sel_vars)
  data       <- data |> tab_apply_val_labels(sel_vars)

  # Phase 18p (Bug A): an NA factor *level* (a factor built with `exclude = NULL`) is a real
  # category whose label is NA. Convert it to an NA *value* on every selected factor, so the existing
  # `na=` machinery handles it uniformly (na="drop" drops the row, na="keep" relabels it to "NA")
  # instead of the NA poisoning the total-row mask and crashing print/format/every export. A factor
  # with no NA level is untouched -> byte-identical.
  for (v in sel_vars) {
    if (is.factor(data[[v]]) && anyNA(levels(data[[v]])))
      data[[v]] <- forcats::fct_na_level_to_value(data[[v]])
  }

  # Phase 18p (Bug B): a logical col_var is a natural 2-level cross-tab variable (tab_plain already
  # accepts it), but the numeric-vs-factor/character classification below covers neither logical nor
  # Date -- both masks stay FALSE, tab_transform builds nothing, and tab_restore -> n_groups(NULL)
  # crashes. Coerce a logical col_var to a factor (routes through plain_core, matching tab_plain), and
  # abort cleanly for any genuinely unsupported col_var type (Date/POSIXct/list/...).
  for (p in pos_col_vars) {
    nm <- names(data)[[p]]
    v  <- data[[p]]
    if (is.logical(v)) {
      data[[nm]] <- forcats::as_factor(v)
    } else if (!is.numeric(v) && !is.factor(v) && !is.character(v)) {
      cli::cli_abort(c(
        "Column variable {.val {nm}} must be a factor, character or numeric.",
        "x" = "Got a {.cls {class(v)}} column.",
        "i" = "Convert it first \u2014 bin a date or continuous variable into groups, or use {.code as.factor()}."
      ))
    }
  }

  # DESIGN: extract by POSITION with `[[` (not `data[pos_col_vars]`): `df[<int vector>]` is column-
  # subsetting on a data.frame/tibble but ROW-subsetting on a data.table, which silently mis-classified
  # col_vars (-> NA col_var -> tab_num eval_select crash) on a data.table input. `data[[<int>]]` is
  # engine-agnostic.
  col_vars_num  <- purrr::map_lgl(pos_col_vars, ~ is.numeric(data[[.x]]))
  col_vars_text <- purrr::map_lgl(pos_col_vars,
                                  ~ is.factor(data[[.x]]) || is.character(data[[.x]]))
  # Phase 18z10: which col_vars `OR = "cumOR"` may apply to -- an ORDERED factor with 3+ levels,
  # since "at or below level j" is only meaningful on a scale. Read from the RAW data here, i.e.
  # before tab_prepare()'s lump/cleannames pass, which is why the ordered class only has to survive
  # as far as tab_setup() for the feature to work (it now survives the whole pipeline anyway).
  col_vars_cumor <- purrr::map_lgl(pos_col_vars, ~ or_cum_ok(data[[.x]]))

  # wt_quo arrives from ctx (defused in tab_build); resolve to a bare symbol or character().
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character() #rlang::sym("no_weight")
  } else {
    wt <- rlang::sym(rlang::as_name(wt_quo))
  }
  # Phase 18z14-i: the test RUNG is derived HERE, the one place that holds both the resolved weight
  # and the design_spec -- so tab(), tab_many() and tab_counts() cannot disagree about it (before, only
  # tab() had the rule, which left tab_many() silently always classic).
  # Phase 18z16-iiiii: `agg_only` -- "this call holds a pre-aggregate, not microdata" -- is declared
  # by tab_counts() and folded in here, so the basis is resolved ONCE against what the input can serve.
  conf_level <- vctrs::vec_recycle(conf_level, 1)
  # THIS is the one inference object of the whole build: the weight, the design, the basis those two
  # imply, the design df, the confidence level and the four interval methods. Everything downstream --
  # both leaves, the tests, the omnibus grid, the assembler -- takes it whole instead of ten formals.
  inference <- new_inference(wt, design_spec, conf_level, ci_method, agg_only,
                             design_effect = design_effect)
  # Phase 18a bug-fix: a weight that is ALSO a selected variable is nonsensical (you cannot weight a
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
  # Phase 18p bug-fix: a variable used BOTH as a tab_var and as a row/col var used to surface a
  # cryptic tidyselect ("Element `x` doesn't exist") or data.table ("assign to the same column twice")
  # error. Mirror the weight-collision guard above with an actionable message.
  tab_dup <- intersect(as.character(tab_vars),
                       c(as.character(row_vars), as.character(col_vars)))
  if (length(tab_dup) != 0L) {
    cli::cli_abort(c(
      "{cli::qty(tab_dup)}The variable{?s} {.val {tab_dup}} {?is/are} used both as a tab variable \\
       and as a row or column variable.",
      "i" = "A variable cannot be a tab variable and a row/column variable at the same time \u2014 \\
             pick a different variable for one of the two roles."
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
  named_colvar   <- !is.null(names(ref)) && any(nzchar(names(ref))) &&
                    any(names(ref) %in% as.character(col_vars))
  # Phase 18m (§Q2): under a col% regime `ref` is vectorised over COL_VARS -- a NAMED-by-col_var
  # vector (Phase 7g-iii), OR an unnamed positional vector whose length matches #col_vars. Each item
  # then selects a reference COLUMN for a factor col_var and a reference ROW for a numeric (mean)
  # col_var (orthogonal), routed through `ref_vect`. A per-ROW_VAR *row* reference stays meaningless
  # under col%, so any OTHER multi-element ref still collapses to a single column reference (+ message).
  positional_colvar <- col_regime && is.null(names(ref)) && length(ref) > 1 &&
                       length(ref) == length(col_vars)
  if (col_regime && (named_colvar || positional_colvar)) {
    ref_by_colvar <- resolve_ref_vector(ref, as.character(col_vars), what = "col_var")
    ref <- "auto"   # scalar unset: tab_num / settings / the row% path behave as no per-row ref
  }
  ref_is_vector <- length(ref) > 1
  ref         <- resolve_ref_vector(ref, as.character(row_vars))
  if (ref_is_vector && col_regime) {
    cli::cli_inform(c("i" = paste0("With {.code pct = \"col\"}, {.arg ref} is vectorised over the ",
                                   "col_vars (length {length(col_vars)}); this ref did not match, so it ",
                                   "is collapsed to a single column reference (its first value).")))
    ref <- vctrs::vec_recycle(ref[1], nrowvars)
  }
  ref2        <- vctrs::vec_recycle(ref2    , nrowvars)
  comp        <- vctrs::vec_recycle(comp    , nrowvars)
  color       <- vctrs::vec_recycle(color   , nrowvars)

  #Arguments vectorised over row : tested here or in tab_num (not in tab_plain)
  ci          <- vctrs::vec_recycle(ci      , nrowvars)
  chi2        <- vctrs::vec_recycle(chi2    , nrowvars)

  #Arguments vectorised over columns : tested here
  ncolvars    <- length(col_vars)
  lvs         <- vctrs::vec_recycle(lvs   , ncolvars)
  digits      <- vctrs::vec_recycle(digits, ncolvars)
  # Phase 19h (KEY 7): `totcol` asks ONE question -- is there a total column? Since Phase 6 exactly
  # one is shown, so "each" and "all_col_vars" are accepted spellings of "last" rather than shapes of
  # their own (tab() speaks `tot`; tab_many() maps its legacy values in tab_deprecate_many()).
  #
  # ⚠ WARNING -- why this collapsed to three states. The classifier below used to compare `totcol`
  # against `col_vars` with identical(), but on the "last" path `totcol` is a CHARACTER while
  # `col_vars` is a LIST of symbols: both arms were therefore DEAD. "all_col_vars" was unreachable
  # as a tot_cols_type, and every tab() call fell through to the catch-all "some" -- which is what
  # actually did the work. Three states are all there ever were; naming them is the whole fix.
  if (as.character(totcol)[1] %in% c("last", "all_col_vars", "each")) {
    totcol <- col_vars_text[col_vars_text] |> names() |> dplyr::last()
    if (all(lvs == "first") & all(pct == "row") & ncolvars > 1) {
      totcol <- NULL
    }
  } else if (as.character(totcol)[1] %in% c("no", "")) {
    totcol <- col_vars[0]                                       # no total column
  } else {
    cli::cli_abort(c('{.arg totcol} must be {.val last} or {.val no}.',
                     "i" = "Through {.fn tab}, say {.code tot = \"col\"} or drop it from {.arg tot}."))
  }
  # tot_cols_type says what to do with the total columns downstream (consumed in tab_assemble_tables):
  #   "one"          = keep the ONE requested total column (the last text col_var's), drop the rest
  #   "no_delete"    = none requested, but one is needed internally (pct/ci/chi2/OR need a
  #                    reference total) -> build it, drop only at the very end
  #   "no_no_create" = no total col at all
  tot_cols_type <- if (length(totcol) != 0) {
    "one"
  } else if (any(chi2 != FALSE) | any(pct != "no") | any(ci != "no")) {
    "no_delete"
  } else {
    "no_no_create"
  }

  # Phase 19d: the build-time "an OR table has no meaningful 100 % total column, so delete it" rule is
  # GONE. It keyed on `OR`, i.e. on an ARGUMENT, to decide something that is purely about what the
  # table SHOWS -- and the display-keyed rules that say the same thing already exist and already run
  # (tab_fold_addn_incell / tab_or_total_col, both on tab_is_or_display()). One rule, at the render
  # end, where the display is finally known. The visible consequence is that an odds-ratio table keeps
  # its Total column, now reading `n=<base>` -- which is what ?tab has always promised.




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

  # Phase 18z10 / 19d: ref2_vect -- per row_var, a per-col_var ref2 vector, the ref2 analogue of
  # ref_vect. DESIGN: `ref2` is a per-ROW_VAR argument but `ref2 = "cumulative"` is only meaningful
  # on an ORDERED col_var with 3+ levels under row percentages, so eligibility is a property of the
  # PAIR. The settings spine is exactly where the two axes are allowed to meet (17e rule 4), so the
  # resolved value lives on `pairs`; `rows$ref2` keeps the REQUESTED value (the jamovi cache tuple
  # reads it). Every other ref2 value broadcasts unchanged.
  ref2_vect <- purrr::map2(ref2, pct_vect, ~ ref2_resolve_cum(.x, .y, col_vars_cumor))


  #Unique arguments :
  total_names <- vctrs::vec_recycle(total_names, 2)
  na          <- vctrs::vec_recycle(na , 1)


  # Tests to be done before tab_plain / tab_num.
  # Phase 7b: the whole colour cascade -- color = "auto" resolution and the measure's declared
  # forcing of totrow / chi2 / ci / ref -- lives in ONE pure resolver,
  # tab_resolve_settings() (R/tab-resolve.R), shared with tab_counts(). Phase 19c: it returns ONE
  # resolved measure; each consumer derives its own need from it (measure_builds / measure_applies)
  # instead of reading one of four precomputed per-step sub-passes. It is a data-free
  # function of the arguments + column classes: the exact boundary the Jamovi `.js` mirrors and
  # the Phase 7c cache keys on. Data-dependent resolution (ref = "auto"/regex, levels = "auto",
  # the leaf tot/totaltab forcing) deliberately stays in the leaf builders below.
  # See dev/tabxplor_argument_computation_map.md.
  .settings     <- tab_resolve_settings(color = color, ci = ci, chi2 = chi2,
                                         ref = ref, pct_vect = pct_vect,
                                         display_measure = display_comparison(display),
                                         col_vars_text = col_vars_text, totrow = totrow,
                                         color_signif = color_signif,
                                         color_ratio_ci = color_ratio_ci, stars = stars,
                                         na = na, wt_name = as.character(wt),
                                         other_if_less_than = other_if_less_than, comp = comp,
                                         tab_vars = as.character(tab_vars),
                                         row_vars = as.character(row_vars),
                                         col_vars = as.character(col_vars),
                                         # Phase 19k (D13): the REAL filter, not a hardcoded NA --
                                         # two calls differing only by `filter` used to hash to the
                                         # same tier-0/tier-1 key, so a filter change never
                                         # invalidated the jamovi aggregate cache.
                                         filter_expr = filter_expr)
  color         <- .settings$color         # Phase 19c: ONE resolved measure (was + 4 sub-passes)
  chi2          <- .settings$chi2
  ci            <- .settings$ci
  ci_scale      <- .settings$ci_scale     # Phase 14b: "diff" / "ratio" (the Katz interval)
  # Phase 19d: THE comparison this table makes, and whether the LEAF owns its interval (the Woolf
  # log-OR one) instead of tab_ci(). `color_signif`/`stars` come back because `ci = "cell"` disables
  # them (D28) -- one rule, both consumers.
  comparison    <- .settings$comparison
  or_ci         <- .settings$or_ci
  color_signif  <- .settings$color_signif
  stars         <- .settings$stars
  totrow        <- .settings$totrow
  cache_keys    <- .settings$cache_keys

  # Phase 17e: the SETTINGS SPINE -- a star schema built ONCE here, the single place the two axes
  # combine and the vehicle tab_rowvar_ctxs() slices by explicit KEY (no more length == n guessing).
  # DESIGN: three typed tibbles at their natural grain:
  #   rows  = one row per row_var (the per-row_var scalars),
  #   cols  = one row per col_var (the per-col_var settings + factor/numeric masks),
  #   pairs = one row per (row_var x col_var) -- the fact table carrying pct + ref + the `na` policy
  #           (added in prepare_pop, which is what resolves it). expand_grid is ROW-MAJOR (row_var
  #           outer, col_var inner), matching the unlist() order of the former pct_vect/ref_vect
  #           nested lists, so pairs$pct/$ref are byte-identical to those. pct_vect/ref_vect thus
  #           stop being ctx fields (pairs is their home); tab_resolve_settings() above still
  #           consumed the LOCAL pct_vect.
  #
  # Phase 19i: it is now the ONLY carrier -- `tab_setup()` no longer ALSO writes each of these flat
  # into the ctx, and `tab_rowvar_ctxs()` no longer re-flattens them per row_var. Every stage reads
  # them through ctx_settings_locals() (which see), so the two copies that used to be "kept in sync
  # by the same ctx_update" are one.
  #
  # WHERE THE LINE IS: the spine carries SETTINGS -- values the user chose or a resolver derived,
  # at one of the three grains. It never carries built OBJECTS: `fine_num` (a moment aggregate,
  # sliced by name), `remove_levels`, `na_drop_all` and the stage products ride the ctx.
  rv_chr <- as.character(row_vars) ; cv_chr <- as.character(col_vars)
  settings <- list(
    rows = tibble::tibble(
      row_var = rv_chr, color = color, comparison = comparison, or_ci = or_ci, chi2 = chi2,
      ref = ref, ref2 = ref2,
      comp = comp, ci = ci, ci_scale = ci_scale, totaltab = totaltab, totrow = totrow
    ),
    cols = tibble::tibble(
      col_var = cv_chr, is_num = unname(col_vars_num), is_text = unname(col_vars_text),
      lvs = lvs, digits = digits
    ),
    pairs = tibble::tibble(
      row_var = rep(rv_chr, each  = length(cv_chr)),
      col_var = rep(cv_chr, times = length(rv_chr)),
      is_text = rep(unname(col_vars_text), times = length(rv_chr)),
      pct     = unlist(pct_vect, use.names = FALSE),
      ref     = unlist(ref_vect , use.names = FALSE),
      ref2    = unlist(ref2_vect, use.names = FALSE)
    )
  )

  # --- repack: setup produces the resolved/recycled settings every downstream stage reads.
  # ctx_update() preserves a field resolved to NULL (e.g. totcol) as a NULL element -- `ctx$x <-
  # NULL` would delete it, breaking the downstream list2env() unpack.
  # Phase 19i: the 15 fields that duplicated a spine column (color / comparison / or_ci / chi2 /
  # ci / ci_scale / totaltab / totrow / ref / ref2 / comp / lvs / digits / col_vars_num /
  # col_vars_text) are GONE from here -- they ride `settings` and reach each stage through
  # ctx_settings_locals(). What stays is what the spine's three grains cannot express. ---
  ctx <- ctx_update(ctx, list(
    data = data, settings = settings,
    row_vars = row_vars, col_vars = col_vars, tab_vars = tab_vars, wt = wt,
    tab_row_names = tab_row_names, na_drop_all = na_drop_all,
    cleannames = cleannames, stars = stars, color_signif = color_signif,
    inference = inference,
    total_names = total_names, na = na,
    totcol = totcol, tot_cols_type = tot_cols_type,
    cache_keys = cache_keys,
    var_labels = var_labels
  ))
  # ... and the INPUTS the spine now owns leave the ctx entirely. They are the user's raw,
  # unrecycled, unresolved values: keeping them beside the resolved spine column of the same name is
  # the same two-carriers problem one step earlier, and the stale copy is the one a bare-name read
  # would find. Deleting them makes "the spine is the only carrier" mechanical rather than
  # conventional -- a downstream read of the old flat name now fails loudly instead of quietly
  # returning the pre-resolution value. (`na` stays: it is a scalar the cache key and tab_counts()
  # read as such, and the spine carries only its per-grain resolutions.)
  ctx[SPINE_OWNED_INPUTS] <- NULL
  ctx
}

# The tab_build() inputs whose resolved form is a SETTINGS SPINE column, and which therefore stop
# existing as ctx fields once tab_setup() has run (see there). `levels` resolves to `cols$lvs`.
#' @keywords internal
#' @noRd
SPINE_OWNED_INPUTS <- c("pct", "color", "chi2", "ci", "ref", "ref2", "comp",
                        "totaltab", "totrow", "digits", "levels")


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
  list2env(ctx_settings_locals(ctx), environment())   # Phase 19i: lvs / col_vars_text, from the spine
  # Phase 7e: jmvtab sets ctx$defer_level_merge = TRUE so `levels = "first"` does NOT collapse
  # non-first levels PRE-aggregate -- the aggregate + chi2/ANOVA see FULL levels (cacheable; the
  # level-drop is a display step in tab_assemble). tab()/tab_counts() leave it at new_ctx()'s FALSE
  # default -> today's pre-merge (byte-identical). The jmvtab full-level test therefore intentionally
  # diverges from tab(levels = "first"). See dev/tabxplor_jmvtab_cache_design.md 3.3/4e/5.

  #Prepare the data
  # Phase 18z14-i: `.svy_row` (the position each row holds in the survey design passed as `data`)
  # rides through the preparation exactly as `.filter` does, so the design-based test can index the
  # design from the PREPARED microdata -- the table the user actually sees, after `filter=`, level
  # lumping and relabelling. `any_of()` is a no-op without a design, so nothing else moves.
  data <- data |> dplyr::select(!!!tab_vars, !!!row_vars, !!wt, !!!col_vars,
                                 tidyselect::any_of(c(svy_row_col, ".filter"))) |>
    relabel_levels_in_varnames(as.character(col_vars))

  #  Filters : here after selection (operations on rows copy all columns on memory),
  #     orwhen the tables are made for more speed :
  # - na = "drop_all" removes NAs here in tab_prepare (slower), i.e. for all tables mades
  # - na = "drop" : NA in factors and numeric will be removed in each tab_plain/tab_num
  # - na = "keep" : NA in factors (not numeric) will be made explicit in each tab_plain/tab_num

  # Phase 19k (D13): "is there a filter" is DERIVED from the one carrier, `filter_expr`.
  if (!is.na(filter_expr)) data <- data |> dplyr::filter(.data$.filter) |>
    dplyr::select(-".filter")

  #If all variables on a subtable are "drop_all", then put na = "keep" to gain time
  if (na == "drop_all") {
    na_drop_all <- as.character(c(row_vars, col_vars, tab_vars))
    # Per-row_var lists of "keep" (SAME shape as the else branch): na_num one scalar per row_var,
    # na_text one char vector (per text col_var) per row_var. Keeping the "keep" value preserves the
    # speed shortcut; the per-row_var shape is what the spine's two grains are assembled from below
    # (19i) -- it must not collapse to a scalar, which is what broke jmvtab on >= 2 row_vars.
    na_text <- rep(list(rep("keep", sum(col_vars_text))), length(row_vars))
    na_num  <- rep(list("keep"), length(row_vars))

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
      # WARNING: no nested lambda referencing `.x` here — dplyr >= 1.2 inlines across() functions,
      # which breaks the closure (`object '.x' not found`). Keep `.x` in the direct body only.
      dplyr::mutate(dplyr::across(tidyselect::all_of(as.character(row_vars)), function(.x) {
        lvs <- unique(append(levels(dplyr::pull(data, dplyr::cur_column())), other_level))
        forcats::fct_relevel(.x, lvs[lvs %in% levels(.x)])
      }))
  }


  #Remove rows with missing values or 0 in weight, for them not to be added in raw counts
  # remove zero weight in tab_prepare ?
  if (length(wt) != 0) {
    zero_weight <- dplyr::pull(data, !!wt)
    zero_weight <- is.na(zero_weight) | zero_weight == 0
    # Phase 18p bug-fix: when EVERY row has a zero/NA weight, the empty frame used to surface the
    # generic "data is of length 0" downstream, never mentioning weights. Abort with a weight-aware
    # message here instead.
    if (nrow(data) != 0L && all(zero_weight)) {
      cli::cli_abort(c(
        "Every row has a zero or missing weight ({.val {as.character(wt)}}) \u2014 nothing to tabulate.",
        "i" = "Check the weight variable {.val {as.character(wt)}} for all-zero or all-NA values."
      ))
    }
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
  # Phase 19a (study §7.10): the SETTINGS SPINE is refreshed here too, and it must be. `lvs` is
  # written into ctx$settings$cols by tab_setup() while it may still hold the sentinel "auto";
  # THIS stage is what resolves it (against the real level counts), and only a flat ctx duplicate
  # used to be updated -- so the spine's copy stayed "auto", and tab_rowvar_ctxs() shipped that stale
  # copy to every parallel worker. Dormant when found (nothing read settings$cols for a value), but
  # 19i made the spine the only carrier, so that stale "auto" is now what EVERY consumer would read.
  # `lv1`, the fact consumers actually want, is stored beside it rather than re-derived.
  #
  # Phase 19i: the `na` policy joins it, at the two grains it really has -- per PAIR for the factor
  # leaves (a text col_var whose row_var/col_var/tab_vars are all in `na_drop_all` keeps its NAs, so
  # the policy genuinely varies by pair) and per ROW_VAR for the numeric one. That is what the spine's
  # own comment has promised since 17e. The per-pair vector is assembled from the two lists rather
  # than recomputed, so the values are the ones the byte-parity nets already lock; a numeric pair
  # carries its row_var's `na_num`, which is what the leaves read for it.
  ctx$settings$cols$lvs <- lvs
  ctx$settings$cols$lv1 <- lv1
  ctx$settings$rows$na_num <- purrr::map_chr(na_num, 1L)
  ctx$settings$pairs$na <- unlist(
    purrr::map2(na_text, na_num, function(nt, nn) {
      v <- rep(nn[[1]], length(col_vars_text))
      v[col_vars_text] <- nt
      v
    }), use.names = FALSE)
  ctx_update(ctx, list(
    data = data,
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
  list2env(ctx_settings_locals(ctx), environment())   # Phase 19i: col_vars_num / na_num, from the spine
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
  # O(N) scan buys nothing; +1-7% when forced on) -- see dev/tabxplor_2.0.0_decisions.md 30. The
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
# Aggregate -> the per-cell fmt fields AND the whole-table test, both from the UNCHANGED
# tab_num(.fine=) / tab_plain(.fine=) leaves (tier 3, O(cells), recomputed each run). Since 19j the
# leaf owns the test too (leaf_chi2 / leaf_chi2_num) -- there is no post-join pass. That is what makes
# the ordering invariant STRUCTURAL: the test necessarily sees the FULL levels, because it is computed
# before the non-first-level drop can exist (that drop lives in tab_assemble_output).
# Phase 9a: SCALAR over ONE row_var. The row axis is now an OUTER map in tab_build_tables(); this ctx
# describes a single row_var (its per-row_var settings are scalars, its fine_num is one aggregate).
# The former internal tab_pmap() row-dispatch is gone (it was always serial once the whole-pipeline
# tab_build_one() worker took over the parallel dispatch). The col axis stays vectorised (pmap over
# factor col_vars). tabs_text / tabs_num / tests / chi2_num are now SINGLE objects (or NULL).
#' @keywords internal
#' @noRd
tab_transform <- function(ctx) {
  list2env(ctx, environment())
  # Phase 19i: this ctx describes ONE row_var, so every spine column projects to the scalar this
  # stage has always read -- color / chi2 / ci / ci_scale / ref / ref2 / comp / totaltab / totrow /
  # comparison / or_ci / na_num, plus the per-col_var digits / col_vars_* / lv1 and the per-pair
  # pct_vect / ref_vect / ref2_vect / na_text.
  list2env(ctx_settings_locals(ctx), environment())
  .by_table <- by_table
  .fine     <- fine_fused
  row_var   <- as.character(row_vars)                 # this ctx describes exactly ONE row_var
  rv        <- rlang::sym(row_var)
  # `wt` arrives as a character name (or character(0) for no weight); rebuild the bare symbol for `!!`.
  wt_sym    <- if (length(wt) == 0L) wt else rlang::sym(as.character(wt))

  # cached_tests is the jmvtab tier-2 hook: the FULL per-row_var list keyed by row_var name (from
  # jmv_cache_aggregate) -- kept whole in the shared ctx, this row_var's entry picked below. new_ctx()'s
  # NULL default carries on the tab()/tab_counts() path -> the leaf computes the test itself. The method_*
  # CI-method fields are likewise always present (new_ctx defaults). Phase 17e: their former exists()
  # guards are gone. Phase 19i: pct_vect / ref_vect / ref2_vect come from `settings$pairs` through
  # ctx_settings_locals(), so on a real build they are bound and non-NULL. The broadcast below is the
  # NO-SPINE path only -- a ctx hand-built for a stage test, which keeps new_ctx()'s raw inputs
  # because tab_setup() never ran to consume them into the spine. (Phase 19a's declarations are what
  # made these guards REACHABLE at all: D7, an absent field errors instead of testing NULL.)
  if (is.null(pct_vect))  pct_vect  <- rep(pct , length(col_vars))
  if (is.null(ref_vect))  ref_vect  <- rep(ref , length(col_vars))
  # z10/19d: same rule for ref2_vect (the per-pair ref2, "cumulative" already resolved per col_var).
  if (is.null(ref2_vect)) ref2_vect <- rep(ref2, length(col_vars))
  cached_test <- if (is.null(cached_tests)) NULL else cached_tests[[row_var]]

  # Phase 18z16-iv (W-B): the robust omnibus GRID, produced ONCE here because two consumers need it
  # -- the `color = "contrib"` residual's base (chi2_write_contrib(), below, inside tab_chi2) and the
  # `test` overlay (tab_assemble_tables(), which needs the numeric ANOVA rows bound first). It used to
  # be computed only in assemble, so the residual could never see it and always fell back to the
  # weights-only B^2/S -- overstating |z| by x2.52 on a cluster-level row_var.
  # The gate is the OLD overlay's, plus W-H: an input that cannot SERVE the weighted basis
  # (pre-aggregated counts / a cached `.fine`, which carry no per-observation Sigma w^2 -- the leaves
  # then report `unserved` and the table states basis "n") must not carry a design-based p
  # either. `color = "contrib"` already forces chi2 (resolve_color_auto), so this costs no new
  # svychisq on exactly the tables W-B is about. Plain tab()/tab_many() always have fine_fused NULL.
  # Phase 18z16-iiiii: the gate is now just "the basis asks for it, and a test was asked for". Its
  # third clause re-derived, in a third spelling, the fact tab_setup() already resolved: an input that
  # cannot serve the weighted basis never reaches basis != "n" now (svy_inference_basis(can_serve =)),
  # and svy_omnibus_grid() runs on `data`, which is microdata on every path that gets here.
  robust_tests <- NULL
  if (!identical(inference$basis, "n") && isTRUE(chi2)) {
    robust_tests <- svy_omnibus_grid(
      data, row_var, as.character(col_vars),
      stats::setNames(as.logical(col_vars_num), as.character(col_vars)),
      as.character(tab_vars), wt, inference$basis, inference$design, comp[1],
      totaltab_name = if (identical(totaltab, "table")) totaltab_name else NULL)
  }

  # Phase 17f: the pipeline calls the resolved-args CORES directly (num_resolve/num_core and
  # plain_resolve/plain_core) instead of the public leaf wrappers -- so the argument forcing runs
  # ONCE, colour is finalised ONCE downstream by tab()/tab_many() (no double finalize_color_spec),
  # and there is no .color_deprecate dance (the pipeline never re-normalises the legacy colour).
  tv_syms <- rlang::syms(as.character(tab_vars))

  # --- numeric col_vars: one num_core() (adopts the per-row_var moment aggregate fine_num) ---
  # num_resolve is forcing-only, so replicate the num-wrapper's validate (digits cast, total_names
  # recycle) here -- the byte-identical counterpart of tab_num()'s validate block.
  tabs_num <- NULL
  chi2_num <- NULL
  if (sum(col_vars_num) != 0) {
    num_col_syms <- rlang::syms(as.character(col_vars)[col_vars_num])
    num_digits   <- vctrs::vec_recycle(vctrs::vec_cast(digits[col_vars_num], integer()),
                                       length(num_col_syms))
    total_names2 <- vctrs::vec_recycle(total_names, 2)
    # Phase 18m: under pct = "col" a numeric col_var takes its POSITIONAL reference ROW from ref_vect
    # (byte-identical to the scalar `ref` when no per-col_var ref is set -- ref_vect is then its broadcast).
    # num_core is one call for all numeric col_vars, so a mix of differing numeric refs uses the first.
    ref_num_vec <- unlist(ref_vect, use.names = FALSE)[col_vars_num]
    ref_num     <- if (length(ref_num_vec)) ref_num_vec[[1]] else ref
    if (length(unique(ref_num_vec)) > 1L)
      cli::cli_inform(c("i" = paste0("Several numeric col_vars with different references: the first ",
                                     "({.val {ref_num}}) applies to all mean columns.")))
    # Phase 19c: the numeric leaf gets the resolved measure iff the measure can colour a mean --
    # its declared `applies_to`. `contrib` and `or` cannot (a mean has no chi2 contribution and no
    # odds), which is exactly what the `color_num` recode used to say by naming them. `"auto"` is the
    # resolver's own sentinel on a numeric-only table and passes through to resolve_color_auto_num().
    color_num <- if (identical(color, "auto") || measure_applies(color, "num")) color else "no"
    r_num <- num_resolve(color_num, ref_num, ci, dplyr::if_else(totrow, "row", "no"),
                         comp[1], totaltab, rv, num_col_syms, tv_syms)
    tabs_num <- num_core(
      data, rv, num_col_syms, tv_syms, wt_sym,
      color = r_num$color, na = na_num[1], ref = r_num$ref, comp = r_num$comp, ci = r_num$ci,
      ci_visible = r_num$ci_visible, stars = stars,
      ci_scale = ci_scale[1], totaltab = r_num$totaltab, totaltab_name = totaltab_name,
      tot = r_num$tot, total_names = total_names2, subtext = "", digits = num_digits,
      num = FALSE, df = FALSE, .fine = fine_num, .by_table = .by_table,
      inference = inference
    )
    # Phase 3b: whole-table test for NUMERIC col_vars = one-way ANOVA (Welch + classic F). Only the
    # tidy `test` tibble is kept (merged with the factor test at assemble); NULL when chi2 is off for
    # this row_var. Phase 19j: computed from the leaf's own metadata (leaf_chi2_num) instead of
    # through tab_chi2(), which re-derived it from markers and could MUTATE the table on the way.
    if (isTRUE(chi2)) chi2_num <- leaf_chi2_num(tabs_num, comp, rv, num_col_syms, tv_syms)
  }

  # --- factor col_vars: one plain_core() per col_var, joined into ONE table ---
  # plain_resolve does the full validate + forcing, so raw args pass straight through it.
  tabs_text <- NULL
  tests     <- chi2   # logical placeholder; assemble's is.logical() fallback handles a numeric-only tab
  if (sum(col_vars_text) != 0) {
    # Phase 19j (KEY 5): WHICH test each leaf must compute. "ctr" also writes the per-cell contribution
    # FIELDS (which are not in the `test` tibble, so a tier-2 cache HIT cannot serve it); "no" covers
    # both `test = FALSE` and a hit, whose cached tibble is injected below instead.
    want_ctr  <- identical(measure_builds(color), "contrib")
    test_leaf <- if (!isTRUE(chi2)) "no"
                 else if (!is.null(cached_test) && !want_ctr) "no"
                 else if (want_ctr) "ctr" else "p"
    text <- purrr::pmap(
      list(col_vars[col_vars_text], digits[col_vars_text], na_text,
           pct_vect[col_vars_text], ref_vect[col_vars_text], ref2_vect[col_vars_text],
           # `levels = "first"` on THIS col_var: the table shows one level against the merged rest, so
           # the leaf must build the odds ratio of that dichotomy (see tab_apply_reference). True on
           # both paths -- tab() has already merged, jmvtab has not -- and the leaf picks which
           # realisation applies, so the fact travels instead of being re-derived from the level count.
           lv1[col_vars_text]),
      function(.col_var, .digits, .na, .pct, .ref, .ref2, .lv1) {
        # Phase 19c: the leaf stamps every measure but contrib, whose per-cell contributions are a
        # SEPARATE computation (chi2_write_contrib). That single question replaced the
        # `color_diff_OR` recode; passing the measure straight through would make the leaf stamp
        # "diff" on a contrib table (its `color_1` fall-through). 19l: it is `measure_builds()`
        # directly -- `measure_stage()` wrapped exactly this test and named the answer after a step
        # 19j deleted. `want_ctr` above is the same question, computed once.
        color_leaf <- if (want_ctr) "no" else color
        r_pl <- plain_resolve(.pct, .ref, .ref2, .na, totaltab_name, total_names,
                              c("row", "col"), comp, color_leaf, .digits, totaltab, tv_syms,
                              comparison = comparison)
        plain_core(
          data, rv, .col_var, tv_syms, wt_sym,
          pct = r_pl$pct, color = color_leaf, na = r_pl$na, ref = r_pl$ref,
          ref2 = r_pl$ref2, comp = r_pl$comp, totaltab = r_pl$totaltab, totaltab_name = totaltab_name,
          tot = r_pl$tot, total_names = r_pl$total_names, subtext = "", digits = r_pl$digits,
          num = FALSE, df = FALSE, stars = stars,
          comparison = comparison, or_ci = or_ci, dichotomise = isTRUE(.lv1),
          # Phase 19j (KEY 5): the interval AND test plans reach the leaf, exactly as the numeric arm
          # above already hands the interval to num_core(). Both are per row_var on the settings spine.
          ci = ci, ci_scale = ci_scale[1], test = test_leaf, deff = robust_tests,
          color_signif = color_signif, .fine = fine_for_pair(.fine, row_var, .col_var),
          .by_table = .by_table, inference = inference
        )
      }
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

    # Phase 19j (KEY 5): each leaf carries its OWN test tibble (one row per subtable x col_var x
    # test-type -- the grain chi2_compute_test() already produced), so the join is where they are
    # bound, and the `arrange` reproduces its final sort across col_vars. The ordering invariant
    # (compute on the FULL level set, before tab_assemble drops the non-first levels) is now
    # STRUCTURAL: the leaf is upstream of assemble entirely.
    leaf_tests <- purrr::map(text, get_test) |> purrr::compact()
    text       <- purrr::map(text, ~ set_test(.x, NULL))

    tabs_text <- purrr::reduce(text, dplyr::full_join, by = c(as.character(tab_vars), row_var))

    tests <- if (!isTRUE(chi2)) chi2
             else if (!is.null(cached_test) && !want_ctr) cached_test           # tier-2 hit
             else if (length(leaf_tests) == 0L) new_test_tibble()
             else {
               tt <- vctrs::vec_rbind(!!!leaf_tests)
               if (nrow(tt) == 0L) new_test_tibble() else
                 dplyr::arrange(tt, dplyr::across(tidyselect::any_of(
                   c(as.character(tab_vars), "col", "test"))))
             }
    if (isTRUE(chi2)) tabs_text <- set_test(tabs_text, tests)
  }

  # --- repack: transform produces this row_var's built+tested table(s) + the tier-2 test. ---
  ctx_update(ctx, list(
    tabs_text = tabs_text, tabs_num = tabs_num, tests = tests, chi2_num = chi2_num,
    robust_tests = robust_tests
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
# Phase 19a: the `tab_assemble(ctx)` convenience composing the two halves is DELETED -- it had no
# caller anywhere (tab_build() has called tab_build_tables() then tab_assemble_output() since the
# Phase 8/9a split). "tab_assemble" survives only as the NAME OF THIS STAGE in the comments.

# SCALAR over ONE row_var. tabs_text / tabs_num are the single built factor / numeric table (or NULL);
# tests / chi2_num the single test tibbles. Produces ctx$tabs = the single finished tabxplor_tab (its
# whole-table test baked into the `test` attribute) + ctx$tests (kept for the jmvtab tier-2 store).
#' @keywords internal
#' @noRd
tab_assemble_tables <- function(ctx) {
  list2env(ctx, environment())
  list2env(ctx_settings_locals(ctx), environment())   # Phase 19i: lv1 / col_vars_* / totrow / ref
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
    if (tot_cols_type == "no_delete")
      tabs_text <- dplyr::select(tabs_text, -where(is_totcol))
    if (tot_cols_type == "one")
      tabs_text <- dplyr::select(tabs_text, -(where(~ is_totcol(.) & !get_col_var(.) %in% totcol)))

    # Lone total column -> "Total" with no col_var name. Phase 8: dedup with unique() so the "lone
    # total" test is on the DISTINCT total-column name (the "Total_<lastcv>" internal suffix leaked into
    # multi-row_var tables otherwise). A genuinely multi-total table (>1 distinct name) keeps the
    # qualified names. This is what DECOUPLES the per-row_var build from the integrated one.
    # Phase 19l: found by the STORED `totcol` flag, not by a regex built from `total_names[2]`. That
    # string is the USER's, so it was interpolated unescaped -- a total named "Total (n)" or
    # "Ensemble." was a regex -- and it is the same job tab_compact() already does through is_totcol()
    # + get_col_var().
    totnames <- unique(names(tabs_text)[purrr::map_lgl(tabs_text, ~ is_fmt(.) && is_totcol(.))])
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

  # Remove the unwanted total row. Phase 19d: the second half -- "a col% odds-ratio table drops its
  # total row" -- went with `OR`, for the same reason as its total-COLUMN twin in tab_setup(): it
  # keyed on an argument to decide a purely DISPLAY question, and the display is not known here.
  no_totrow <- (totrow == FALSE)
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

  # Phase 18j: the OPT-IN robust omnibus overlay (the flat design / a survey design) replaces the
  # classic chi2 / F rows, keeping the descriptive effect sizes. Phase 18z16-iv: the GRID it lays
  # over was computed in tab_transform() (it also feeds the contrib residual's base); this is only the
  # tidy join. The default basis `"n"` produces no grid, so the ordinary path is untouched.
  if (!is.null(robust_tests) && nrow(tests) > 0) {
    tests <- tab_robust_overlay(tests, robust_tests, as.character(tab_vars))
  }

  # Phase 10i-B: store the add_n / add_pct DISPLAY intent (materialised by tab_materialize_extras()).
  # Phase 14p: add_n / add_pct only make sense beside a col_var (they fold the base `n` / a `col_pct`
  # into the crosstab). A no-col_var table (`tab(relig)`) is a plain frequency: its `n` / `pct` / `wn`
  # columns ARE the primary content (built by tab_plain()'s no_col_var block), not display extras, so
  # the intent must be OFF -- else tab_fold_addn_incell() would try to fold into a Total column that
  # does not exist and silently DROP the real `n` column (the <=1.3.1 regression).
  fmt_here        <- purrr::map_lgl(tab, is_fmt)
  has_real_colvar <- any(fmt_here & get_col_var(tab) != "no_col_var")
  # Phase 18m: `common_totrow` collapses a several-row_vars table's redundant per-block Total rows into
  # ONE shared Total shown in its own group (default FALSE = one Total per row_var). `common_totrow_ref`
  # records whether ANY row_var used the total as its reference (ref = "tot"), so the shared Total renders
  # bold (it is a reading anchor for at least one variable).
  render_extras <- list(add_n  = isTRUE(add_n)  && has_real_colvar,
                        add_pct = isTRUE(add_pct) && has_real_colvar)
  # Stored ONLY when opted in, so a default table's render_extras (and every golden) is byte-unchanged.
  if (isTRUE(common_totrow)) {
    render_extras$common_totrow     <- TRUE
    render_extras$common_totrow_ref <- any(ref == "tot")
  }
  # Phase 19k: `anova` -- WHICH one-way F this table shows for its mean col_vars. Both F rows are
  # computed and stored in `test`, so the choice is display-only and belongs here beside add_n /
  # add_pct; tab_anova() reads it back at render, falling back to the option. Stored only when the
  # user stated it (NULL = "the option decides"), so nothing moves by default.
  if (!is.null(anova)) render_extras$anova <- as.character(anova)[[1]]
  # Phase 14d: record the variable ROLES here, where they are known. Recovering them from the finished
  # table is guesswork (and wrong after tab_compact) -- see get_vars_attr() in R/tab_classes.R.
  # Phase 16d: also record the weight column NAME (character(0) when unweighted) -> the footer "Weighted
  # by <wt>." line. `wt` is a local from list2env(ctx) -- the resolved weight name (see tab_transform()).
  # Phase 19f (KEY 1): the variable MODEL is declared on the index columns themselves (stamped by the
  # leaves, read back by tab_declared_vars()); what is recorded here is what no column can carry.
  vars_attr <- new_vars_attr(
    wt = if (length(wt) == 0L) NA_character_ else as.character(wt)[1],
    # 19l: no exists() guard -- `var_labels` is a DECLARED ctx field (new_ctx(), default character()),
    # so list2env() above always creates the binding. This was the last of the guards 19i's
    # declaration was meant to retire; it could never be FALSE.
    var_labels = var_labels)
  # Phase 17b: the two 2.0.0-new attrs left here are ONE `meta` list (drop-NULL happens in new_tab()).
  # Phase 19g (KEY 6): the table STATES its identity -- kind + what no column can carry.
  meta <- list(render_extras = render_extras, spec = new_spec("crosstab", vars = vars_attr))
  # Phase 18z13 (D3): project the call's confidence level onto every fmt column. `meta$ci_settings`
  # records it for the legend, but the colour engine is per COLUMN and never sees the table -- so
  # without this stamp every threshold in it falls back to the global option, and a table built at
  # conf_level = 0.99 prints 99 % intervals while greying at 95 %. Stamped whatever `ci` says: the
  # level is also the alpha of the contrib significance gate and of the p-value cell.
  # z16-iiiii: the LEVEL only. The design df and the inference basis are stamped by each core on its
  # own columns (leaf_inference), because only the core knows what its own build found out -- so a
  # numeric block that had to fall back no longer downgrades the factor block joined beside it, and
  # the whole-table answer stays the weakest of the columns (tab_inference_basis()).
  tab <- tab_stamp_inference(tab, inference$conf_level)
  if (!lv1_group_vars(tab)) {
    tab    <- dplyr::group_by(tab, !!!tab_vars)
    groups <- dplyr::group_data(tab)
    tab    <- new_grouped_tab(tab, groups = groups, subtext = subtext, test = tests, meta = meta)
  } else {
    tab <- new_tab(tab, subtext = subtext, test = tests, meta = meta)
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
  # Merge the per-row_var tables into one only in "single" mode (tab() default). A length-1 list
  # (single row_var) is never merged -- it is unwrapped below instead.
  # Phase 19f (KEY 1): `can_merge <- length(tab_vars) == 0` is GONE. Several row_vars and tab_vars used
  # to compete for the one dplyr grouping slot, so asking for both silently returned a LIST rather
  # than a table -- a documented product limitation. The row-variable axis is a declared column now.
  # Phase 19h (KEY 7): `| getOption("tabxplor.output_kable") == TRUE` is GONE too. That is a DISPLAY
  # option, read here inside a BUILD stage, and it changed the CLASS of the returned object. The
  # option keeps its render (tab()'s tail calls tab_html()); it no longer decides a shape.
  merge_now <- output == "single"
  if (merge_now &
      !(is.list(tabs) & !is.data.frame(tabs) & length(tabs) == 1 ) ) {
    tabs <- tabs |> tab_compact() # pvalue_lines = FALSE
  }


  # Phase 10i-B: p-value rows are NO LONGER baked at build. The whole-table `test` attribute is KEPT
  # (tab_assemble_tables set it and nothing drops it now), and the p-value rows are materialised at
  # DISPLAY -- as body rows by the exporters (tab_export_prep / tab_xl via tab_materialize_extras), and
  # as the summary block in the console (Phase 16a). This lets every reserved-"pvalue"-row
  # special-case downstream (n_min, jmvtab re-ref, ...) shrink, and keeps the built tab the "core"
  # table (see dev/tabxplor_2.0.0_decisions.md §34).

  # Phase 7g: n_min small-base DISPLAY filter -- the last, pure-display step (drops rows/cols
  # whose base < n_min and blanks weak cells; recomputes nothing). See tab_apply_n_min().
  # Phase 17e: always present via new_ctx()'s 0 default (was an exists() guard); tab_counts() inherits it.
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
  test    <- get_test(tabs)
  # Phase 18z16-iiiii (defect 1): capture `meta` HERE, while `tabs` is still a tab -- the
  # tidyr::pivot_wider() below returns a plain tibble carrying no table attributes. This function
  # ended in a bare `new_tab(tabs, subtext =, test =)` literal, so EVERY spread table silently lost
  # its whole `meta`: the weight footer and the inference basis (measured: basis "weights" -> "n",
  # i.e. the footer stated the opposite of what was computed), the CI-method legend, the variable
  # roles and the add_n/add_pct intent. tab_spread() is exported AND it is what tab(spread_vars =)
  # calls, so this was the second rebuild-from-a-literal site (z16-iv's record claims tab_compact()
  # was the only one). See tab_meta_merge()'s WARNING: never a fresh `meta = list(...)`.
  meta_in  <- get_meta(tabs)

  get_vars   <- tab_get_vars(tabs)
  col_levels <- get_vars$col_vars_levels |> purrr::flatten_chr()
  row_var    <- get_vars$row_var
  tab_vars   <- get_vars$tab_vars
  tab_vars_new <- tab_vars[!tab_vars %in% spread_vars]
  # captured BEFORE the pivot, which is the last moment the spread variables still exist as columns:
  # every new column name ends with one of these levels, and that is how a spread column is paired
  # back with the sub-population it describes (spread_relabel(), below).
  spread_levels <- unique(unlist(lapply(
    spread_vars, function(v) as.character(unique(dplyr::pull(dplyr::ungroup(tabs), v))))))
  spread_levels <- spread_levels[!is.na(spread_levels) & nzchar(spread_levels)]
  # Phase 19f emptied `vars` of the variable MODEL (the columns declare it), so the pivot has nothing
  # to re-key here: `wt` / `caption` / `var_labels` all survive it untouched.
  spec_out <- get_spec(tabs)

  na_values <- purrr::map(dplyr::ungroup(tabs)[col_levels],
                          ~ fmt0(scale = get_scale(.x), display = get_display(.x[1]))) |>
    purrr::set_names(col_levels)


  totrows <- is_totrow(tabs)
  if (any(totrows)) {
    tabs <- tabs |> dplyr::group_by(!!!rlang::syms(tab_vars))
    groups <- dplyr::group_vars(tabs)

    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows & totrows

    tabs <- tabs |> tibble::add_column(totrows, tottab_rows, tottab_line)

    # if two tab_vars or more, calculate totals for each level of spread_var
    if (length(tab_vars_new) != 0 & any(tottab_rows)) {
      tabs <- tabs |> dplyr::filter(!tottab_line)
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

  # Phase 19h (KEY 7): THE two post-spread repairs, which used to live in reg_spread_models() and so
  # ran for a regression's split groups only. Both are generic -- a spread column belongs to a level
  # of the spread variable, on either producer -- and the second was a latent defect on the crosstab
  # side: a `test` row keyed on the spread variable pointed at a column that no longer exists.
  spread <- spread_relabel(tabs, spread_vars, spread_levels, test, get_vars$col_vars)
  tabs   <- spread$tabs ; test <- spread$test

  meta_out <- tab_meta_merge(list(meta_in), spec = spec_out)

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, test = test, meta = meta_out)
  } else {

    group_dat <- dplyr::group_data(tabs)
    new_grouped_tab(tabs, groups = group_dat, subtext = subtext, test = test, meta = meta_out)
  }

}


# spread_relabel() -- Phase 19h (KEY 7): THE post-spread repair, for both producers.
#
# tidyr::pivot_wider() moves the data and nothing else, so after a spread two facts are stale:
#
#   1. every new column's stored `col_var` still names the ORIGINAL column variable, with nothing to
#      say which level of the spread variable the block belongs to. Folding the level in as
#      "{level}<br>{col_var}" is what makes the exported span header read on two lines and what puts
#      a border between the blocks -- and what lets the legend tell two sub-populations apart.
#   2. every `test` row keyed on the spread variable points at a sub-population that is no longer a
#      ROW group but a set of COLUMNS, so its `col` key must be re-pointed at what the spread made of
#      what it named, and its group key cleared. This ran for a regression's split groups only; a
#      crosstab's per-tab_var chi2 rows had the same problem and simply kept a stale key.
#
# Matching is by column NAME: pivot_wider names a new column `{level}` when there is a single value
# column and `{value}_{level}` otherwise, so the longest matching level wins (that is what
# disambiguates nested level names).
#
# ⚠ WARNING -- `test$col` holds TWO kinds of entity, which is why the re-key needs a discriminator:
# a crosstab row names a COL_VAR (test_grid_crosstab intersects it with the table's col_vars), a
# regression row names a COLUMN. One rule -- "follow `col` through the spread" -- two lookups. That
# overload is the `test` schema's, not this function's; unifying it belongs to a later phase.
#' @keywords internal
#' @noRd
spread_relabel <- function(tabs, spread_vars, spread_levels, test, col_vars = character(0)) {
  if (length(spread_levels) == 0L) return(list(tabs = tabs, test = test))
  col_of_group <- stats::setNames(rep(NA_character_, length(spread_levels)), spread_levels)
  for (nm in names(tabs)[vapply(tabs, is_fmt, logical(1))]) {
    hits <- spread_levels[vapply(spread_levels,
                                 function(g) nm == g || endsWith(nm, paste0("_", g)), logical(1))]
    if (!length(hits)) next
    g <- hits[which.max(nchar(hits))]
    tabs[[nm]] <- set_col_var(tabs[[nm]], paste0(g, "<br>", get_col_var(tabs[[nm]])))
    # the `n` column comes FIRST but is a row descriptor, never a model column, so keying a group's
    # footer block under it would put every statistic beneath its counts. Its stored role says so.
    if (identical(get_role(tabs[[nm]]), "n")) next
    if (is.na(col_of_group[[g]])) col_of_group[[g]] <- nm
  }

  if (!is.null(test) && nrow(test) > 0) {
    for (sv in spread_vars) {
      key <- test_key_col(test, sv)
      # only rows describing one of the spread groups move. A row with an empty key is table-wide
      # (a pooled interaction test, an Ensemble chi2) and must stay exactly where it is.
      known <- which(key %in% spread_levels)
      if (!length(known)) next
      lv   <- key[known]
      old  <- test_key_col(test, "col")[known]
      newc <- ifelse(old %in% col_vars,
                     paste0(lv, "<br>", old),          # a col_var: it was folded, follow the fold
                     unname(col_of_group[lv]))         # a column: this group's first real column
      # a group that produced no column of its own has nowhere to sit: drop those rows rather than
      # leave them pointing at something that no longer exists.
      test$col[known[!is.na(newc)]] <- newc[!is.na(newc)]
      # WARNING: a tab_var key column is a FACTOR, and `[<-` on a factor with an unknown level gives
      # NA plus a warning. Blank it as character.
      test[[sv]] <- test_key_col(test, sv)
      test[[sv]][known] <- ""
      if (anyNA(newc)) test <- test[-known[is.na(newc)], , drop = FALSE]
    }
  }
  list(tabs = tabs, test = test)
}


#' Transpose a cross-table (swap its rows and columns)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_transpose()` is **soft-deprecated** since tabxplor 2.0.0. It flips the *object* (the
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
    "2.0.0", "tab_transpose()",
    details = 'Use the `transpose = TRUE` argument of tab_kable() / tab_md() / tab_xl() / tab_export().')
  if (!is.data.frame(tabs)) {
    cli::cli_abort("{.arg tabs} must be a {.pkg tabxplor} table.")
  }
  tabs <- dplyr::ungroup(tabs)

  # Phase 19h (KEY 7): the two SHAPE refusals are declared in TAB_OPS (R/tab-shape.R), so
  # `tab_supports(x, "transpose_object")` answers them without trying.
  tab_check_shape(tabs, "transpose_object")

  vars    <- tab_get_vars(tabs)
  row_var <- vars$row_var
  # not a shape fact: a table so degraded that the role detector finds no row-variable column at all.
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
  # 19l: the variable column is the DECLARED one, not a column that happens to be named "row_var".
  dvars       <- tab_declared_vars(tabs)
  merged      <- isTRUE(dvars$compacted)
  var_col_nm  <- intersect(dvars$var_col, names(tabs))
  src_of      <- function() as.character(tabs[[var_col_nm[[1]]]])
  src_row_var <- NULL
  if (merged) {
    src   <- src_of()
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

  # --- capture the axis roles BEFORE the pivot (row_kind / in_refrow are uniform across fmt cols) ---
  totrow_lgl   <- is_totrow(tabs[[fmtc[1]]])
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
  max_per_sub <- function(x) if (merged) max(c(0L, table(src_of()[x])))
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
  # Phase 19b: transposing swaps the percentage BASE (a row % becomes a column %), never the
  # estimate scale -- the numbers are the same numbers, read along the other axis.
  old_base <- if (is.null(rep_attrs$pct_base)) "row" else rep_attrs$pct_base
  new_base <- switch(old_base, row = "col", col = "row", old_base)

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
    col <- set_pct_base(col, new_base)                        # row % <-> col %
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

  # Phase 19f: the transposed table declares its own index -- one level column, named after the old
  # col_var, whose `var` IS that variable. `compacted` is FALSE again by construction: the merged
  # shape is gone, undone by the pivot, so there is no "var"-role column to declare.
  wide[[name]] <- new_lvl(factor(new_labels, levels = new_labels), "level", name)

  # re-key the whole-table test tibble: the new row variable is the old col_var and vice versa.
  test <- get_test(tabs)
  if (is.data.frame(test) && nrow(test) > 0) {
    rv <- test[["var"]]; cv <- test[["col"]]
    test[["var"]] <- cv
    test[["col"]] <- rv
  }

  # Phase 10i-B: carry the add_n/add_pct DISPLAY intent through the transpose (orientation-agnostic --
  # the materialiser adds add_n as a ROW once the table reads as col%). So transpose(row% add_n) then
  # display == a native col% add_n table.
  # Phase 14d: the roles SWAP, so (like `test` above) they must be re-keyed, never passed through.
  # The result is a single-row_var table whose row_var is the old col_var and whose col_vars are the
  # old row_vars -- `compacted` is FALSE again: the merged shape is gone, undone by the pivot.
  attrs <- tab_attrs(tabs)
  attrs$test <- test
  # Phase 17b: `vars` is a sub-field of the carried `meta` list. Phase 18z16-iv: rebuilt through
  # tab_meta_merge(), the ONE "rebuild a meta" idiom -- so every other sub-field rides along by
  # construction instead of by this call remembering to carry it.
  attrs$meta <- tab_meta_merge(
    list(attrs$meta),                                    # Phase 16d: weight survives transpose
    spec = new_spec("crosstab", vars = new_vars_attr(wt = get_vars_attr(tabs)$wt)))
  rlang::exec(new_tab, wide, !!!attrs)
}





# Phase 19f (KEY 1): the roles come from the COLUMNS that declare them (tab_declared_vars(), see
# R/row-model.R), not from a stored `vars` triple that a dplyr chain could leave stale. NULL -> the
# caller keeps the column-type heuristic, which is now the DEGRADED path only (a hand-built frame, an
# object from an older version, or `mutate(levels = as.character(levels))`).
# CONTRACT unchanged: `row_var`/`tab_vars` are COLUMN names (what every consumer indexes with), NOT
# the source variable names. On a merged table those differ -- the row levels live in a column
# literally named "levels" and the source names in the `var`-role column's values -- which is exactly
# what a declared role expresses and a name convention cannot. `row_vars` carries the source names
# for the few callers that want them (the tab_xl title).



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
  rec <- tab_declared_vars(tabs)

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
# Phase 19f (KEY 1): the per-row kind ("data"/"total"/"n"/"pct"/"pvalue"/"gof"/"blank"), length
# nrow(tab) -- read straight off the record's `row_kind` field (fmt_row_kind()). It used to be a
# positional character vector stored in meta$vars$row_roles, seeded at RENDER and living exactly one
# render pass, so every consumer outside that pass fell back to matching English row labels. Now the
# rows carry their own kind through every slice, bind, arrange and rebuild, and the label-matching
# fallback below fires only for a frame with NO fmt columns at all.
tab_row_roles <- function(tab) {
  n <- nrow(tab)
  kinds <- fmt_row_kind(tab)
  if (length(kinds) == n) return(kinds)
  rep("data", n)
}

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
  rec <- tab_declared_vars(tabs)
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
       # Phase 19f: the DECLARED "var"-role column -- the one naming, per row, which variable that
       # row belongs to. It is `row_var` on a merged crosstab and `var` on a regression, which is
       # why every consumer used to test for a column literally NAMED "row_var" and then needed a
       # second, different rule for the regression shape.
       var_col = if (!is.null(rec)) rec$var_col else character(0),
       col_vars = col_vars_names, col_vars_levels = col_vars_levels)
}


# Phase 10c: the shared "graceful degrade" notice for exporters/print when a table cannot be
# read as a tabxplor table -- render the plain frame (per backend) instead of crashing. Fired once
# per render batch (the `notify` gate in tab_export_prep dedups within one render); left per-render
# (not throttled once-per-session) so a knit / loop that re-degrades a genuinely non-tabxplor frame
# still tells the user each time -- and so the degrade-message tests stay meaningful (test-edge-cases).
#' @keywords internal
tab_degrade_inform <- function(reason) {
  cli::cli_inform(c(
    "!" = "tabxplor formatting and colors skipped: {reason}.",
    "i" = "Rendering the plain table instead."
  ))
}




# STEP-BY-STEP FUNCTIONS -----------------------------------------------------------------

# === SECTION: labelled-data (haven/labelled) interop =================================

# Convert ONE haven/labelled column to a factor using its value labels, without any haven/labelled
# dependency -- keyed only off the base `labels` attribute (a named vector: names = label text,
# values = codes). Phase k rule: convert ONLY when the labels are COMPLETE (every observed non-NA
# value is labelled); otherwise strip the labelled class to the underlying vector, so a coded
# numeric keeps its means path (tab_num) and an incomplete categorical is treated as its real type.
# A column with no `labels` attribute is returned unchanged (byte-identity for non-labelled data).
# Idempotent: the result never carries a `labels` attribute, so a second call is a no-op.
# WARNING: this drops the `label` attribute too -- capture variable labels BEFORE calling it.
val_labels_to_factor <- function(x) {
  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs) || length(labs) == 0L) return(x)

  raw <- x
  attributes(raw) <- NULL                       # bare atomic values, drops labelled/label/class

  observed <- unique(raw[!is.na(raw)])
  if (!all(observed %in% unname(labs))) return(raw)   # incomplete -> underlying numeric/character

  # DESIGN: levels follow the `labels`-vector order (the survey's intended order), NOT sorted codes.
  # Duplicate label text merges its codes (base factor() behaviour). Empty labelled levels are
  # dropped so an unobserved code adds no phantom row.
  f <- factor(raw, levels = unname(labs), labels = names(labs))
  forcats::fct_drop(f)
}

# Apply val_labels_to_factor() across the labelled columns among `vars`. No-op (byte-identical) when
# none carry a `labels` attribute -- the common case, so this stays free for non-labelled data.
# WARNING: column access by `[[` (name), never `data[vars]` -- the latter ROW-subsets a data.table
# (the same engine-agnostic trap tab_setup documents at ~L1567).
tab_apply_val_labels <- function(data, vars) {
  vars <- intersect(unique(vars), names(data))
  for (v in vars) {
    if (!is.null(attr(data[[v]], "labels", exact = TRUE)))
      data[[v]] <- val_labels_to_factor(data[[v]])
  }
  data
}

# Read the variable-label (`label` attr) for each of `vars`, as a named character (name -> label).
# Only variables that HAVE a non-empty label appear -- so an all-unlabelled table yields character()
# and stores nothing in meta$vars (absent-when-unset, no golden churn). Base attr(), no dependency.
capture_var_labels <- function(data, vars) {
  vars <- intersect(unique(vars), names(data))
  if (length(vars) == 0L) return(character())
  labs <- vapply(vars, function(v) {
    l <- attr(data[[v]], "label", exact = TRUE)
    if (is.null(l) || !nzchar(as.character(l)[[1]])) NA_character_ else as.character(l)[[1]]
  }, character(1))
  names(labs) <- vars
  labs[!is.na(labs)]
}

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
      resolve_cleannames(cleannames)

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
    }

    # Phase k: labelled (haven/labelled) columns become value-label factors BEFORE the numeric
    # classification below, so a labelled categorical is seen as a factor (and an incomplete-labelled
    # numeric keeps its real numeric type). Idempotent with tab_setup's earlier call.
    data <- data |> tab_apply_val_labels(variables)

    vars_not_numeric <-
      dplyr::select(data[pos_variables], where(~ !is.numeric(.))) |>
      colnames() #|> rlang::syms()                # is.integer(.) | is.double()


    # Phase 18z10: the blanket `ordered`-strip that used to live here is GONE. Its FIXME guessed at
    # MCA; the real cause, measured, was two vctrs bind sites in the TOTALS machinery, both reachable
    # only through `tab_vars` -- adding a "Total"/"Ensemble" level produced a plain factor that vctrs
    # then refused to combine with an ordered one. Both are fixed at the source (leaf_rename_totals()
    # here, num_rollup() in R/tab-agg.R), so ordered factors now survive the whole pipeline, which is
    # what makes `OR = "cumOR"` able to select its col_vars by class.
    # WARNING (public surface): a table's grouping columns now come back `ordered`, with "NA" and
    # "Total"/"Ensemble" appended as the GREATEST levels -- they are labels, not scale points.


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
#           n/digits integer, in_tottab/in_refrow logical, row_kind/display character, the 12
#           other numerics double. new_fmt() does NO casting, so the carrier owns the types.
#   meta  : named list of the per-column ATTRIBUTES = `fmt_col_attrs` (defined in fmt_class.R, DERIVED
#           from new_fmt()'s formals so an attribute can never again be forgotten here). `color` is
#           carried WHOLE (length 1 or 2). The field / attribute name order is the new_fmt() contract.

# WARNING: pass `comp_all` by EXACT name (not `comp`). The leaves historically wrote `comp = x`,
# which PARTIAL-MATCHES the `comp_all` formal (verified) -- `comp_all = x` is the identical result.
# fmt_materialize_col() -- the ONE new_fmt() call. do.call by exact names => no partial-match drift.
fmt_materialize_col <- function(frame, meta) do.call(new_fmt, c(frame, meta))

# fmt_unwrap() / fmt_wrap() -- the carrier ROUND-TRIP (Phase 9b-4). This is the jmvtab tier-3 CARRIER
# seam: a built table is DECOMPOSED to plain field-vectors so the cache stores data rather than a live
# tab, and so the chi2 / CI arithmetic can read and write those vectors directly (Phase 9b-5) instead
# of reconstructing the tabxplor_fmt record at every step. In 9b-4 the two are composed as a byte-identical no-op (fmt_wrap(fmt_unwrap(x))) that carries
# the table to the seam and validates the round-trip in the real pipeline.
#
# fmt_unwrap(tab) -> a carrier list:
#   is_fmt  : logical over the data columns (rebuild order + fmt/factor split).
#   factors : the non-fmt columns, passed through WHOLE (length-nrow, own attrs kept).
#   fmt     : named list, one entry per fmt column = list(frame, meta) -- frame = as.list(vec_data())
#             (the 21 raw fields, exact types), meta = the fmt_col_attrs read by exact name.
#   attrs   : attributes(tab) VERBATIM (class / names / row.names / subtext / test / groups).
# fmt_wrap(carrier) is the exact inverse: materialize each fmt column via the single
# fmt_materialize_col() seam, pass the factor columns through, restore `attrs` wholesale.
#
# Byte-identity: new_fmt() does NO casting (L1) so vec_data() -> new_fmt() preserves every field's
# storage type; the fmt_col_attrs are read/written by exact name; restoring attributes() reproduces the
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
# is a list of per-source field-frames (each = as.list(vctrs::vec_data(col)), the 21 raw fields);
# concat field-by-field with vctrs::vec_c (type-stable, so L1 holds: int+int -> int, NA_integer_ vs
# NA_real_ preserved) and materialize ONCE via the fmt_materialize_col() seam with the supplied `meta`
# (the fmt_col_attrs). This replaces a vec_rbind over the tabxplor_fmt RECORDS (which casts +
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























# weighted.var() was removed in 2.0.0 (Phase 2): tab_num() now derives the weighted (ML) variance
# from moment sums in a single pass via num_derive_stats() (R/tab-agg.R), instead of a per-group
# helper that recomputed weighted.mean() on every call (the old double scan). The ML-vs-sample
# variance question it flagged is tracked for Phase 3 (dev/tabxplor_2.0.0_decisions.md §14).

# Phase 3a: the scalar mean-CI helpers ci_mean()/ci_mean_diff() and the DescTools proportion-CI
# closures ci_base()/ci_diff() were removed. All CI math now lives in the vectorised closed-form
# engine (ci_pivot/ci_wilson/ci_newcombe/ci_prop_diff/ci_mean_diff2, R/tab-agg.R), alongside
# zscore_formula() (moved there in Phase 17a) which supplies the normal quantile.




#' @keywords internal
quo_miss_na_null_empty_no <- function(quo) {
  if (rlang::quo_is_missing(quo)) return (TRUE)
  if (rlang::quo_is_null(quo)) return(TRUE)
  base_quo <- quo
  quo <- rlang::get_expr(quo) |> as.character()
  # message(paste0(quo, collapse = ", "))


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
    rnames <- as.character(tabs[[row_var]])
    tabs[, eval(row_var) := NULL]
    data.table::setDF(tabs, rownames = rnames)
  }

  tabs
}


# leaf_totrow_tottab() -- Phase 17f: the shared total-row / total-table row flags both leaves derive
# from the built table (`totrow` = a "Total" row_var level; `tottab` = every tab_var == "Total").
#' @keywords internal
leaf_totrow_tottab <- function(tabs, row_var, tab_vars) {
  # DESIGN: `%in%` not `==` so an NA row/tab label (a real NA *level*) yields FALSE, never NA. An NA
  # in in_totrow/in_tottab would poison is_totrow()/get_reference()/is_refrow() and crash the
  # `out[mask] <-` assignments in pillar_shaft/format (Phase 18p, Bug A). Mirrors replace_na below.
  totrow_vector <- dplyr::pull(tabs, !!row_var) %in% "Total"
  tottab_vector <- if (length(tab_vars) == 0) {
    rep(FALSE, nrow(tabs))
  } else {
    dplyr::transmute(tabs, tottab = dplyr::if_all(
      tidyselect::all_of(as.character(tab_vars)),
      ~ . %in% "Total"
    )) |>
      tibble::deframe()
  }
  # Phase 19f: `kind` is what the record stores (row_kind); the two logicals stay because the
  # reference / total-column machinery below still asks the two questions separately.
  list(totrow = totrow_vector, tottab = tottab_vector,
       kind   = dplyr::if_else(totrow_vector, "total", "data"))
}


# leaf_rename_totals() -- Phase 17f: the shared "#Rename totals" tail both leaves run before the final
# wrap -- recode the tab_var totals to totaltab_name, rename the total ROW to total_names[1] (prefixed
# with the subtable name when grouped) and the total COLUMN to total_names[2]. Byte-identical across
# the two leaves (was verbatim in each), so it lives here once.
#' @keywords internal
leaf_rename_totals <- function(tabs, row_var, tab_vars, tot, total_names, totaltab, totaltab_name,
                               tottab_vector, totrow_vector) {
  #Rename totals
  # Phase 18z10: both renames are MASK-ASSIGNMENTS on the expanded factor, not `dplyr::if_else()`.
  # if_else built its `true =` branch as a fresh factor / character, so an ORDERED input hit
  # "Can't combine <factor> and <ordered>" -- the real cause behind the old blanket ordered-strip in
  # tab_prepare(). fct_expand() + `[<-` keeps the class (ordered or not) by construction.
  # WARNING: `sort(unique(.))` below is load-bearing, NOT tidying. The old `true =` branch was a
  # CHARACTER vector, so factor() sorted the new labels alphabetically; dropping the sort silently
  # reorders the total rows of every grouped table.
  if (totaltab %in% c("line", "table") &  totaltab_name != "Total") {
    tabs <- tabs |> dplyr::mutate(dplyr::across(
      tidyselect::all_of(as.character(tab_vars)),
      ~ {
        z <- forcats::fct_expand(., totaltab_name)
        z[tottab_vector] <- totaltab_name
        forcats::fct_drop(z)
      }
    ))
  }

  if (length(tab_vars) == 0) {

    if ("row" %in% tot & total_names[1] != "Total") tabs <- tabs |>
        dplyr::mutate(!!row_var := forcats::fct_recode(!!row_var,
                                                       purrr::set_names("Total", total_names[1])))
  } else {
    tabs <- tabs |>
      tidyr::unite(col = "tabs_tot_names", !!!tab_vars, sep = " ", remove = FALSE)
    totrow_labels <- paste(total_names[1], tabs$tabs_tot_names)
    tabs <- tabs |>
      dplyr::mutate(
        !!row_var := {
          z <- forcats::fct_expand(!!row_var, sort(unique(totrow_labels)))
          z[totrow_vector] <- totrow_labels[totrow_vector]
          forcats::fct_drop(z)
        }
      ) |>
      dplyr::select(-"tabs_tot_names")
  }

  if ("col" %in% tot & total_names[2] != "Total") tabs <- tabs |>
    dplyr::rename(tidyselect::any_of(purrr::set_names("Total", total_names[2])))

  tabs
}


# leaf_extract_raw() -- Phase 17f: the df=/num= escape hatch. Instead of a duplicated raw-scan branch
# in each leaf, df/num now build the NORMAL fmt table and pull the displayed number per cell with
# get_num() at the very end (mean for numeric columns, count for pct = "no", the percentage for a
# pct table). df -> a plain data.frame with the factor columns merged into rownames (for FactoMineR
# & co.); num -> a tabxplor_tab of plain numeric columns (grouping preserved).
#' @keywords internal
leaf_extract_raw <- function(result, df, num, row_var) {
  fmt_cols <- names(result)[purrr::map_lgl(result, is_fmt)]
  nums <- dplyr::mutate(result, dplyr::across(tidyselect::all_of(fmt_cols), get_num))
  if (num) return(nums)
  # df: a plain data.frame (drop the tabxplor table attrs) with factor cols merged into rownames.
  out <- as_df_merge_rownames(data.table::as.data.table(nums), rlang::as_name(row_var))
  for (a in c("subtext", "test", "meta")) attr(out, a) <- NULL
  out
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
diff_index <-  function(ref, row_var, num_names, pct, is_total = FALSE) {
  if (ref == "tot"   ) return(-1L)
  if (ref == "first" ) return(1L )
  if (is.numeric(ref) | !is.na(suppressWarnings(as.integer(ref)))
  ) {
    return(as.integer(ref[1]))
  }

  targets <- switch(pct, "row" = row_var, "col" = num_names)

  # Phase 19a (D27): "last", the mirror of "first". It is the ONLY sentinel that needs `targets`, so
  # it cannot sit with the three above. ONE meaning on both axes -- the last LEVEL -- because a total
  # is not a level: `ref = "tot"` is what names it, and "last" must not silently become a synonym.
  # The two axes differ only in how that is EXPRESSED, because the callers work at different grains:
  #   col  `targets` IS the column set, so exclude the total column(s) and take the last index
  #        (dplyr::nth() at the pct = "col" site and the `ridx0 >= 1L` guard at the ref2 site both
  #        want a real 1-based index).
  #   row  `targets` is the row_var stacked over EVERY sub-table, while the caller compares
  #        dplyr::row_number() WITHIN one -- so no absolute index can say it. -1L is the sentinel
  #        calculate_refrows() resolves per sub-table (max(which(!totrow_vector))).
  # Before this, "last" fell through to the regex matcher, matched nothing, and
  # first(integer(0)) -> replace_na(0) gave index 0 -> the "no columns were found as reference"
  # warning and an all-NA `or` field.
  # WARNING: like "tot"/"first", the sentinel wins over a level LITERALLY named "last"; select such
  # a level by its integer index instead.
  # `is_total` is a logical over `targets` supplied by the caller -- the leaf's OWN internal naming
  # (names(cols) == "Total", the same convention totcol_vector and the binary-OR `lv` already use,
  # applied before the user's `total_names` are restored), never a rendered user label.
  if (identical(ref, "last")) {
    if (identical(pct, "row")) return(-1L)
    keep <- which(!vctrs::vec_recycle(is_total, length(targets)))
    return(if (length(keep)) max(keep) else length(targets))
  }

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
    # Phase 19a (D27): -1L now MEANS something here. It is what diff_index() returns for
    # ref = "last", and the branch it lands in was previously DEAD -- "tot" is the only other -1L,
    # and the `ref != "tot"` guard above excludes it. "last" is the mirror of "first", so it must
    # name the last LEVEL, not the last ROW: a total row is not a level, it is what `ref = "tot"`
    # selects, and "last" must not silently become a synonym for it. Hence last_lvl() per sub-table,
    # rather than the dplyr::n() this branch used to hold.
    # WARNING: diff_index() stays INSIDE the transmute. `!!row_var` is tidy-eval, so each grouped
    # call sees its OWN sub-table's labels -- which is what makes an exact/regex `ref` resolve per
    # sub-table. Hoisting it out of the transmute makes `!!row_var` an invalid argument (measured).
    last_lvl <- function(is_tot) {
      keep <- which(!is_tot)
      if (length(keep)) max(keep) else length(is_tot)   # a sub-table of nothing but totals
    }
    refrows <-
      if(comp == "tab") {
        tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
          dplyr::mutate(totrow_vector = totrow_vector) |>
          dplyr::group_by(!!!tab_vars) |>
          dplyr::transmute(
            var =
              dplyr::row_number() == if (diff_index(ref, !!row_var,
                                                    num_names = num_names,
                                                    pct = "row") == -1) {
                last_lvl(.data$totrow_vector)
              } else {
                diff_index(ref, !!row_var, num_names = num_names, pct = "row")
              }
          ) |>
          dplyr::pull("var")

      } else {
        tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
          dplyr::mutate(tottab_vector = tottab_vector, totrow_vector = totrow_vector) |>
          dplyr::group_by(!!!tab_vars) |>
          dplyr::transmute(
            var = dplyr::if_else(
              condition = .data$tottab_vector,
              true  = dplyr::row_number() == if (diff_index(ref, !!row_var,
                                                            num_names = num_names,
                                                            pct = "row") == -1) {
                last_lvl(.data$totrow_vector)
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
      # DESIGN: every {?} marker must resolve to the SAME quantity, else cli aborts with a raw
      # "Multiple quantities for pluralization". Pin them all to length(unknown) via cli::qty().
      cli::cli_warn(paste0(
        "{cli::qty(unknown)}Unknown {.arg ref} name{?s} {.val {unknown}}: ",
        "{cli::qty(unknown)}{?it matches/they match} no {what} and {cli::qty(unknown)}{?is/are} ignored."
      ))
    }
    out  <- rlang::set_names(rep("auto", n), row_vars_chr)
    keep <- intersect(nms, row_vars_chr)
    out[keep] <- as.character(ref[keep])
    unname(out)
  } else {
    vctrs::vec_recycle(ref, n)
  }
}







# --- codetools: the tab_build() ctx fields -----------------------------------------------------
# Every stage starts with `list2env(ctx, environment())` + `list2env(ctx_settings_locals(ctx), ...)`,
# which binds every field as a local -- correct at run time, invisible to codetools, which then
# reports each one as an undefined global. Phase 19i DERIVES the list from the two declarations
# instead of mirroring ~70 names by hand in R/fmt_class.R (where `inference_mode` outlived by two
# phases the field it named, leaving the LIVE one undeclared). Same move as 19g's for reg_build()'s
# `shared`, and it lives beside the declarations it derives from, so it cannot go stale.
# The third source is leaf_inference_setup(), whose result the two compute cores also list2env().
# Derived by CALLING it on a neutral inference object, so it cannot go stale either.
# WARNING: it must stay at the END of this file -- `new_ctx()`'s defaults call
# `conf_level_default()`, which is defined further down, and top-level code runs in source order.
utils::globalVariables(c(names(new_ctx()), CTX_SETTINGS_LOCALS,
                         names(leaf_inference_setup(new_inference(), NULL, FALSE))))
