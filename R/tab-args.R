# PURPOSE: THE argument surface as data (Phase 20b, KEY 1 + KEY 8).
# ROLE: an argument of a crosstab producer is declared ONCE -- which producers take it, what it may
#   be, which option is its default, and what it means -- and the signature, the reference page and
#   the value list all read that declaration.
#
# WHY IT EXISTS. 83 of the 149 crosstab formals were the same argument written a 2nd, 3rd or 4th
# time, each mirror carrying its own `@param` block: `@param color` was written 16 times across R/,
# `@param theme` 12, `@param conf_level` 9. 19i gave the four producers ONE resolver
# (tab_resolve_common_args); it did not give them one DECLARATION, so a vocabulary could be added to
# TAB_ARG_VALUES and still be described three ways in three help pages.
#
# THE RULE, and it is what keeps this table from swallowing the fact tables:
#   *** THE FACT TABLE OWNS THE VOCABULARY. TAB_ARGS OWNS THE ARGUMENT. ***
# MEASURES knows what `difference` is; TAB_ARGS knows that `color` is an argument of four producers,
# that it names a measure, and how to say so in a help page. `values_rd` is the edge between them
# (checked in R/zzz-fact-keys.R). An argument whose vocabulary has no other home -- `na`, `tot`,
# `levels`, `totaltab`, `comp`, `pct` -- declares it HERE, in `values`, and that is why
# TAB_ARG_VALUES is derived from this table rather than living beside it.
#
# KEY CONSTRAINTS:
#   - `doc_in_producer = TRUE` says the prose stays in the producer's OWN roxygen.
#
#     ⚠ WHAT `tab_reg()`'s ROWS DECLARE, AND WHAT THEY DO NOT (Phase 20c, KEY 4). All 25 of its
#     formals have a row, and every row states `producers`, `default_for` and where its vocabulary
#     lives -- which is what lets tx_check_tab_args() police that signature exactly as it polices a
#     crosstab one, and what makes "these two producers ask the SAME question" a declared fact
#     rather than a claim. But `tab_reg()` does NOT get `@eval tab_args_rd()`, because the phase
#     MEASURED the thing that would have justified it and it was not there:
#
#         the two producers share the NAME and the GRAMMAR of `wt` / `ref` / `na` / `display` /
#         `color` / `ci_method` / `tab_vars` / `conf_level` / `add_n` / `data`.
#         They do NOT share the PROSE -- every one of those reads differently on a model
#         (`wt` is a survey design, `ref` a treatment contrast, `na` a per-fit drop grain,
#         `display` an effect cell), and emitting the crosstab text into ?tab_reg would be
#         WRONG documentation, not deduplicated documentation.
#
#     THE TEST for moving prose here is the one §4 states for a bundle: it must remove a DUPLICATE.
#     Two hundred lines that say two different things are not a duplicate.
#   - `doc` is roxygen text, moved VERBATIM from the producer's own block. It is rendered by
#     tab_args_rd(), which orders by `formals()` and ASSERTS that the declared set and the formals
#     are the same set -- so an argument added to a signature without a row breaks the build.
#   - ⚠ There is deliberately NO `group` column. The plan proposed one for grouping the generated
#     `@param`s; ordering by `formals()` is better (it matches `\usage{}` and it is self-checking),
#     which left `group` with no reader -- and a column with no reader is weight, not a fact (19b's
#     admission test). Add it the day something groups by it.
#   - ⚠ `status` may be a NAMED vector when an argument is deprecated on ONE producer and live on
#     another: `row_var` is a deprecated singular alias on tab() and the REAL formal of the leaves.
#     Read it with tab_arg_status(name, producer), never with `$status`.
#   - This file sorts before tab.R, so nothing here may read tab.R's top-level objects at SOURCE
#     time. Everything below is a literal or a function body.

#' @keywords internal
#' @noRd
TAB_ARGS <- list(
  data = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_many", "tab_reg"),
    doc = "A data frame."),
  row_vars = list(
    producers = c("tab", "tab_many"),
    doc = c("<\\link[tidyr:tidyr_tidy_select]{tidy-select}> The row variable(s),",
            " printed with one level per line, and the column variable(s), printed with one level per",
            " column. For numeric variables means are calculated, in a single column. Each accepts one",
            " variable or several (e.g. \\code{c(var1, var2)}); with several \\code{row_vars} the mirror",
            " tables are merged into one by default (see \\code{output_list}).")),
  col_vars = list(producers = c("tab", "tab_num", "tab_many"), doc_with = "row_vars"),
  row_var = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"), status = c(tab = "deprecated"),
    doc = c("`r lifecycle::badge(\"deprecated\")` Singular aliases of",
            " \\code{row_vars}/\\code{col_vars} (which now accept several variables). Kept working.")),
  col_var = list(producers = c("tab", "tab_plain", "tab_counts"), status = c(tab = "deprecated"), doc_with = "row_var"),
  tab_vars = list(
    default_for = list(tab_reg = NULL),
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_many", "tab_reg"),
    doc = c("<\\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :",
            "a subtable is made for each combination of levels of the selected variables.",
            "Leave empty to make a simple cross-table. All \\code{tab_vars} are converted to factor.")),
  wt = list(
    default_for = list(tab_reg = NULL),
    producers = c("tab", "tab_plain", "tab_num", "tab_many", "tab_reg"),   # tab_counts says `wt_counts`
    doc = "A weight variable, of class numeric. Leave empty for unweighted results."),
  sup_cols = list(
    producers = c("tab"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` Supplementary columns variables, with",
            "only the first level printed. Deprecated in 2.0.0: pass these columns in \\code{col_vars} and",
            "set \\code{levels = \"first\"} instead (\\code{col_vars} already accepts several variables).")),
  na = list(
    default = "keep", default_for = list(tab_reg = c("drop_by_outcome", "drop_by_model", "drop_all"), tab_num = c("keep", "drop")),
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), values = c("keep", "drop", "drop_all", "common_base"), leaf = c("keep", "drop"), size = 1L,
    doc = c("The policy to adopt for missing values, as a single string :",
            " \\itemize{",
            "  \\item \\code{\"keep\"}: by default, \\code{NA}'s of row, col and tab variables",
            "  are printed as an explicit `\"NA\"` level.",
            "  \\item \\code{\"drop\"}: remove `NA`'s in each row, col and tab variable before calculations,",
            "  so each column is computed on its own non-missing observations (bases can then differ",
            "  between col_vars).",
            "  \\item \\code{\"drop_all\"}: remove every observation missing on the \\code{row_vars}, \\strong{any}",
            "  \\code{col_vars} or a \\code{tab_vars}, so all columns share the same base (no `NA` anywhere).",
            "  \\item \\code{\"common_base\"}: fix a single population -- observations non-missing on the",
            "  \\code{row_vars} and the \\strong{first} \\code{col_vars} (and \\code{tab_vars}) -- shared by",
            "  every column, while secondary \\code{col_vars} keep their own `NA`'s as a level within it.",
            "  This reproduces the historical \\code{tab()} behaviour. Microdata only (not",
            "  \\code{\\link{tab_counts}}).",
            "  }",
            "  When several \\code{row_vars} are combined into one table (no \\code{tab_vars}), their \\code{Total}",
            "  rows are identical whenever they share one population (\\code{\"keep\"}, \\code{\"drop_all\"},",
            "  \\code{\"common_base\"}) and are then displayed as a \\strong{single} Total row; only \\code{\"drop\"}",
            "  can make them genuinely differ, in which case every Total row is kept (with a message).")),
  levels = list(
    default = "all",
    producers = c("tab"), values = c("all", "first", "auto"),
    doc = c("The levels of \\code{col_vars} to keep, as a single string or a vector the same",
            "length as \\code{col_vars} (for finer selections use \\code{\\link[dplyr:select]{dplyr::select}}) :",
            " \\itemize{",
            "  \\item \\code{\"all\"}: by default, all levels are kept.",
            "  \\item \\code{\"first\"}: only keep the first level of each \\code{col_vars} (handy for compact",
            "  summary tables with many indicators).",
            "  \\item \\code{\"auto\"}: keep the first level when a \\code{col_vars} has only two levels, keep all",
            "  levels otherwise.",
            "  }")),
  digits = list(
    default = 0,
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"),
    doc = c("The number of digits to print, as a single integer, or an integer vector the",
            "same length as \\code{col_vars}.")),
  n_min = list(
    default = 0,
    producers = c("tab", "tab_counts"), check = "count",
    doc = c("A single positive integer (default \\code{0}, off). A pure display filter applied",
            "last: it hides small-base cells without recomputing anything. A row is dropped only when its",
            "\\emph{largest} base across the column variables is below \\code{n_min}; surviving cells whose own",
            "base is below \\code{n_min} are blanked. Under \\code{pct = \"col\"} the same rule drops weak",
            "columns. Total rows/columns, the added-\\code{n} row/column and the p-value line are always kept.")),
  display = list(
    default = NULL,
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), values_from = "DISPLAY_TOKENS",
    doc = c("What each value cell shows (text output only -- the console, \\code{\\link{tab_kable}}",
            "  and \\code{\\link{tab_md}}; Excel falls back to the primary field). \\code{NULL} (default)",
            "  keeps each cell's plain value. Three ways to ask, from the shortest:",
            "  \\itemize{",
            "    \\item a \\strong{named layout}: \\code{\"est\"} (the estimate), \\code{\"est_ci\"} (with a",
            "      visible interval), \\code{\"base_ci\"} (each value with its interval), \\code{\"est_base\"}",
            "      / \\code{\"base_est\"} (the two together, in the order the words are written),",
            "      \\code{\"base\"} (the level alone). The same names work on \\code{\\link{tab_reg}}.",
            "    \\item a \\strong{single field}, e.g. \\code{display = \"ci\"} or \\code{\"diff\"}. Valid",
            "      fields are listed in \\emph{Display fields} below.",
            "    \\item a \\strong{\\{\\} template} combining several, e.g. \\code{\"\\{pct\\} (n=\\{n\\})\"}",
            "      (a percentage with its count) or \\code{\"\\{pct\\} (\\{resid\\})\"} (each percentage with",
            "      the standardized residual that says whether it departs from independence --- the",
            "      SPSS cell layout).",
            "  }",
            "  Two fields are \\strong{scale-relative}, and are what makes one layout work on every",
            "  table: \\code{\\{est\\}} is whatever the column estimates (a percentage, a difference, an",
            "  odds ratio) and \\code{\\{base\\}} the level it sits on (a percentage, a mean, a count).",
            "  A layout showing an interval displays the CI the table computes (the cell, difference",
            "  or ratio CI set by \\code{ci = } / \\code{color}), so pair it with a \\code{ci = } value",
            "  or a \\code{color} that needs one. It is a display overlay only: colors, differences and",
            "  the underlying fields are unchanged.")),
  totaltab = list(
    default = "line",
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"), values = c("line", "table", "no", ""), size = 1L,
    doc = c("The total table, if there are subtables/groups",
            "(i.e. when \\code{tab_vars} is provided) :",
            " \\itemize{",
            "  \\item \\code{\"line\"}: by default, add a general total line (necessary for",
            "  calculations with \\code{comp = \"all\"})",
            "  \\item \\code{\"table\"}: add a complete total table",
            " (i.e. \\code{row_var} by \\code{col_vars} without \\code{tab_vars}).",
            "  \\item \\code{\"no\"}: not to draw any total table.",
            " }")),
  totaltab_name = list(
    default = "Ensemble",
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"),
    doc = "The name of the total table, as a single string."),
  tot = list(
    default = c("row", "col"), default_for = list(tab_plain = NULL, tab_num = NULL),
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"), values = c("row", "col", "both", "no", ""),
    doc = c("The totals :",
            " \\itemize{",
            "  \\item \\code{c(\"col\", \"row\")} or \\code{\"both\"} : by default, both total rows and total",
            "  columns.",
            "  \\item \\code{\"row\"}: only total rows.",
            "  \\item \\code{\"col\"}: only total column.",
            "  \\item \\code{\"no\"}: remove all totals (after calculations if needed).",
            " }")),
  total_names = list(
    default = "Total",
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"),
    doc = c("The names of the totals, as a character vector of length one or two.",
            "Use syntax of type \\code{c(\"Total row\", \"Total column\")} to set different names for",
            "rows and cols.")),
  pct = list(
    default = "no",
    producers = c("tab", "tab_plain", "tab_counts"), values = c("no", "row", "col", "all", "all_tabs"), na_ok = TRUE, stored = "none",
    doc = c("The type of percentages to calculate, as a single string or a vector the same length",
            "as \\code{col_vars} (like \\code{levels} and \\code{digits}) :",
            " \\itemize{",
            "  \\item \\code{\"row\"}: row percentages.",
            "  \\item \\code{\"col\"}: column percentages.",
            "  \\item \\code{\"all\"}: frequencies for each subtable/group, if there is \\code{tab_vars}.",
            "  \\item \\code{\"all_tabs\"}: frequencies for the whole (set of) table(s).",
            "}",
            "The default is \\code{\"no\"} --- \\strong{deliberately}: a bare \\code{tab()} is a",
            "table of \\emph{counts}, which is what a first look at the data should be. Say",
            "\\code{pct = \"row\"} (or \\code{\"col\"}) as soon as you want to compare profiles;",
            "everything else in the package --- the reference, the interval, the colour --- follows",
            "from that choice.")),
  ref = list(
    default = "auto", default_for = list(tab_reg = NULL, tab_num = "tot"),
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    doc = c("The reference cell to calculate differences and ratios",
            " (used to print \\code{colors}) :",
            " \\itemize{",
            "  \\item \\code{\"auto\"}: by default, cell difference from the corresponding total",
            "  (rows or cols depending on \\code{pct = \"row\"} or \\code{pct = \"col\"}) is",
            "  used for `diff`; the first line (or col) is used for the odds ratio.",
            "  \\item \\code{\"tot\"}: totals are always used.",
            "  \\item \\code{\"first\"}: calculate cell difference or ratio from the first cell",
            "of the row or column (useful to color temporal developments).",
            "  \\item \\code{\"last\"}: the mirror of \\code{\"first\"} -- the **last level** of the row (or column)",
            "variable. A total row or column is not a level and is never selected: use \\code{\"tot\"} for that.",
            "Resolved inside each subtable when there are \\code{tab_vars}.",
            "  \\item \\code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.",
            "  \\item \\code{\"regex\"}: when `ref` is a string, it it used as a regular expression,",
            "  to match with the names of the rows (or columns). Be precise enough to match only one",
            "  column or row, otherwise you get a warning message.",
            "  \\item \\code{\"no\"}: not use ref and not calculate diffs to gain calculation time.",
            "}",
            "A (named) vector gives one reference per \\code{row_vars} --- \\code{ref = c(race = \"first\")}",
            "names the row variable it applies to, an unnamed vector goes by position, and any variable it",
            "does not mention keeps \\code{\"auto\"}.")),
  ref2 = list(
    default = "first",
    producers = c("tab", "tab_plain", "tab_counts"),
    doc = c("The second reference level for odds ratios (or relative risk ratios), needed",
            "only for a factor with **3 levels or more** (the \"OR of each level versus \\code{ref2}\"). The",
            "first level is used by default. For a **binary** factor \\code{ref2} is ignored: each level's",
            "OR is computed against the *other* level, so both levels show a value (reciprocals of one",
            "another) instead of one being forced to \\code{1}. See `ref` above for the list of possible values.")),
  comp = list(
    default = "tab", default_for = list(tab_num = c("tab", "all")),
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"), values = c("tab", "all", ""), size = 1L, na_ok = TRUE,
    doc = c("The comparison level : by subtables/groups, or for the whole table.",
            "\\itemize{",
            "  \\item \\code{\"tab\"}: by default, contributions to variance,",
            "row differences from totals/first cells, and row confidence intervals for these",
            "differences, are calculated for each \\code{tab_vars} group.",
            "  \\item \\code{\"all\"}: compare cells to the general total line (provided there is",
            "   a total table with a total row), or with the first line of the total table",
            "   when \\code{ref = \"first\"}.",
            "}")),
  OR = list(
    default = "no",
    producers = c("tab", "tab_plain", "tab_counts"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` The odds ratio is computed on **every** row/col",
            " percentage table since 2.0.0, so this argument had nothing left to switch on: it was a",
            " \\code{display}, a \\code{color} and a \\code{ref2} welded together. Each value maps to one of them:",
            " \\itemize{",
            "  \\item \\code{\"OR\"} -> \\code{display = \"\\{or\\}\"} (show the odds ratio instead of the percentage);",
            "  \\item \\code{\"OR_pct\"} -> \\code{display = \"\\{or\\} (\\{pct\\})\"};",
            "  \\item \\code{\"cumOR\"} -> \\code{ref2 = \"cumulative\"}.",
            " }",
            " Colour it with \\code{color = \"odds_ratio\"}, and pick which 2x2 with \\code{ref} (the row) and",
            " \\code{ref2} (the column level).")),
  test = list(
    default = FALSE,
    producers = c("tab", "tab_counts"),
    doc = c("Set to \\code{TRUE} to calculate a statistical test of independence for each",
            "(sub)table: \\strong{Chi-squared} for factor \\code{col_vars}, \\strong{Welch's F} (one-way",
            "ANOVA) for numeric ones -- see \\code{\\link{tab_chi2}}. The whole-table summary also carries an",
            "\\strong{effect size} (Cramer's V / phi for factors, eta-squared for means) and, on a small sparse",
            "factor table where the chi-squared is unreliable, an exact \\strong{Fisher} p-value. Useful to print",
            "metadata, and to color cells based on their contribution to variance (\\code{color = \"contrib\"}).",
            "Automatically added if needed for \\code{color}.",
            "",
            "\\code{test} says only \\emph{whether} to test; \\strong{what kind of test you get follows what you",
            "passed}. \\code{wt} says how the \\emph{estimate} is computed; a second, orthogonal fact --- the",
            "\\strong{inference basis} (the \"weighting level\" of \\code{vignette(\"tabxplor\")}), stored on each",
            "column and named in the table's footer --- says how the \\emph{interval and the test} are:",
            "\\enumerate{",
            " \\item \\code{wt = w} --- estimates, the whole-table test and the effect size are all computed on the",
            " \\strong{weighted} table, but with the raw unweighted number of respondents as the sample size, so",
            " they carry no design effect. This is the default, and the footer says so.",
            " \\item \\code{wt = w} plus \\code{design_effect = TRUE} (or, for a whole session,",
            " \\code{options(tabxplor.design_effect = TRUE)}) --- the same intervals and",
            " tests \\strong{account for the unequal weighting, exactly}. A weight column IS a survey design",
            " (the flat one, \\code{ids = ~1}), so this is not an approximation: the base becomes",
            " \\code{n_eff = p(1-p) / Var_design(p)} in closed form, and the whole-table test becomes",
            " \\code{survey::svychisq} / a \\code{svyglm} Wald F on that same flat design. Being exact rather",
            " than a bound, it can make an interval \\emph{narrower} as well as wider --- unequal probabilities",
            " can carry more information than equal ones. It is blind to clustering and to calibration, which",
            " the weights do not record.",
            " \\item a prebuilt \\code{survey::svydesign} passed as \\code{data} --- fully \\strong{design-based}:",
            " the same quantities, now with strata, clusters, \\code{fpc} and calibration, and every interval",
            " referred to the design's own degrees of freedom.",
            "}",
            "A fourth basis is not a choice but a fallback: when a design-based table's variance cannot be",
            "computed, it reverts to the weighting-only correction, and its footer says so.",
            "Turn the option on when you want a \\code{tab()} percentage interval to be comparable with the",
            "\\code{Obs_*} column of a \\code{\\link{tab_reg}} on the same data: \\code{tab_reg()} never reads it,",
            "because its crude companions are \\emph{always} on the weighted basis, beside a model column that",
            "always was. Replicate-weight (\\code{svrepdesign}) and two-phase designs are not supported, and",
            "\\code{wt} beside a design is an error (a design already carries its own weights).")),
  anova = list(
    default = NULL,
    producers = c("tab", "tab_num"), option = "anova", values = c("welch", "classic"), size = 1L,
    doc = c("Which one-way ANOVA \\strong{F} the p-value line shows for \\emph{numeric}",
            "\\code{col_vars}: \\code{\"welch\"} (does not assume equal variances) or \\code{\"classic\"} (the pooled",
            "F). \\code{NULL} (default) reads \\code{options(tabxplor.anova)}. Both statistics are always",
            "computed and stored in the table's \\code{test} attribute, so this is a pure display choice ---",
            "it changes which row is shown, never a number.")),
  chi2 = list(
    default = lifecycle::deprecated(),
    producers = c("tab", "tab_counts", "tab_many"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` Renamed to \\code{test} in 2.0.0: the test is a",
            "Chi-squared only for factors (numeric \\code{col_vars} get Welch's F), so the old name was",
            "misleading. Still works.")),
  ci = list(
    default = "auto",
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"),
    values = c("auto", "no", "cell", "ref"), validate = FALSE,
    doc = c("**What the confidence interval is anchored on** -- one question, four answers. The",
            " \\emph{geometry} of the interval is not asked here: it follows the comparison the table makes",
            " (\\code{color}, then \\code{display}), so an odds-ratio table gets an odds-ratio interval and a",
            " ratio-coloured one a Katz ratio interval, with no way for the two to disagree.",
            "  \\itemize{",
            "   \\item \\code{\"auto\"} (default): an interval on the comparison when the table makes one",
            "     (percentages by row/column, means), an absolute cell interval for plain frequencies, and",
            "     none at all when nothing needs one.",
            "   \\item \\code{\"ref\"}: the interval of the difference (or ratio, or odds ratio) between a cell",
            "     and its reference -- the total cell, or the first cell under \\code{ref = \"first\"}.",
            "   \\item \\code{\"cell\"}: the absolute interval of the cell's own percentage or mean.",
            "   \\item \\code{\"no\"}: no interval.",
            "  }",
            " \\code{\"cell\"} and \\code{\"no\"} anchor nothing to compare, so \\code{stars} and",
            " \\code{color_signif} have nothing to read: asking for either alongside them informs you once and",
            " disables it, rather than silently testing something else.",
            " Methods are chosen with \\code{ci_method} and named in the table's legend; by default percentages",
            " use the Wilson score interval for a cell and the Newcombe hybrid score for a difference (its",
            " dual, so the bracket and the stars always agree), and means the Welch t interval. With",
            " \\code{ci = \"cell\"} the result prints as `[inf;sup]`; set",
            " `options(\"tabxplor.ci_print\" = \"moe\")` for `pct +- moe`.",
            " \\code{\"diff\"} and \\code{\"ratio\"} are soft-deprecated spellings of \\code{\"ref\"} (the second one",
            " also pins the ratio scale -- say \\code{color = \"ratio\"} instead).")),
  conf_level = list(
    default = NULL,   # 20b/20c: NULL on EVERY producer, the crosstab boundary and the
                      # regression one each resolving it -- one idiom, and no call at source time
                      # (this file sorts before the one that defines conf_level_default()).
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), option = "conf_level", check = "probability",
    doc = c("The confidence level, as a single numeric between 0 and 1.",
            "Default to 0.95 (95%).")),
  stars = list(
    default_for = list(tab_reg = TRUE),
    default = NULL,
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), option = "stars",
    doc = c("Logical (default \\code{FALSE} \\emph{opt-in}). With \\code{ci = \"ref\"}, print",
            "significance stars for each cell's difference from its reference, read from the displayed interval",
            "itself (universal CI-inclusion). \\code{NULL} uses `options(\"tabxplor.stars\")` (default",
            "\\code{FALSE}). \\code{ci = \"cell\"} and \\code{ci = \"no\"} anchor nothing to compare, so asking for",
            "stars alongside them informs you once and disables them.")),
  ci_method = list(
    default_for = list(tab_reg = NULL),
    default = NULL,
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), values_from = "CI_METHODS",
    doc = c("The confidence-interval method of each kind of interval, as ONE named vector --",
            "partial, like \\code{ref} or \\code{pct}, so an unnamed kind keeps its default.",
            "\\itemize{",
            "  \\item \\code{cell}: a proportion's own interval (\\code{ci = \"cell\"}) -- \\code{\"wilson\"}",
            "    (default, the score interval), \\code{\"wald\"} (the normal approximation, commonly taught --",
            "    degenerate at 0 or 1) or \\code{\"beta\"} (Korn-Graubard: the exact Clopper-Pearson interval on",
            "    the effective base, referred to a survey design's own degrees of freedom).",
            "  \\item \\code{diff}: a proportion minus its reference (\\code{ci = \"ref\"}) -- \\code{\"newcombe\"}",
            "    (default, the hybrid-score interval, dual of the two-proportion score test), \\code{\"ac\"}",
            "    (Agresti-Caffo) or \\code{\"wald\"}.",
            "  \\item \\code{mean_diff}: a numeric mean minus its reference -- \\code{\"welch\"} (default, each",
            "    group's own variance), \\code{\"student\"} (the two groups pooled = a two-sample t test) or",
            "    \\code{\"ols\"} (pooled over EVERY level of the variable = the interval a linear regression",
            "    gives that coefficient).",
            "  \\item \\code{mean_ratio}: a numeric mean over its reference (\\code{color = \"ratio\"}) --",
            "    \\code{\"robust\"} (default, each group's own variance = modified/robust Poisson),",
            "    \\code{\"quasipoisson\"} (scaled by the dispersion a quasi-Poisson regression estimates over",
            "    every level of the variable = that regression's own interval) or \\code{\"poisson\"} (naive).",
            "}",
            "Whatever the method, the significance stars come from that same interval, so bracket and stars",
            "always agree. A proportion \\emph{ratio} has only one method (Katz's log risk-ratio), so it is not",
            "a choice. Example: \\code{ci_method = c(cell = \"beta\", diff = \"ac\")}.")),
  design_effect = list(
    default = NULL,
    producers = c("tab", "tab_plain", "tab_num"), option = "design_effect",
    doc = c("Logical or \\code{NULL} (default). Whether the confidence intervals, stars and",
            "colour thresholds of a \\strong{weighted} table account for the weighting's own design effect (the",
            "exact flat-design variance) instead of using the raw sample size. \\code{NULL} takes",
            "\\code{options(\"tabxplor.design_effect\")} (\\code{FALSE} by default). Ignored without \\code{wt}, and",
            "superseded by a \\code{\\link[survey]{svydesign}} passed as \\code{data} (which is always",
            "design-based). See the \"Weights\" section of the introduction vignette.")),
  method_cell = list(
    producers = c("tab"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` Use",
            "\\code{ci_method = c(cell = , diff = )} instead.")),
  method_diff = list(producers = c("tab"), status = "deprecated", doc_with = "method_cell"),
  color = list(
    default = "no", default_for = list(tab_reg = TRUE, tab_num = "auto"),
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    values_from = "MEASURES", values_rd = "color_measures_rd",
    doc = c("Which measure(s) to color, on which visual channel. \\code{FALSE} (default)",
            "prints no color; \\code{TRUE} uses the smart per-column-type scheme (factors: the",
            "\\code{difference} on the text + the \\code{ratio} on the background; numerics: the",
            "\\code{ratio}; counts: \\code{contrib}). Otherwise a measure name, on the \\strong{text} channel:",
            "{VALUES}",
            "The discipline's acronyms are permanent aliases of those names: \\code{\"diff\"} / \\code{\"RD\"},",
            "\\code{\"RR\"}, \\code{\"or\"} / \\code{\"OR\"}.",
            "The grammar: \\strong{position picks the channel} (1st value -> text, 2nd -> background) and",
            "\\strong{names pick the column type} (\\code{pct} / \\code{mean}). So",
            "\\code{c(\"difference\", \"ratio\")} puts the difference on the text and the ratio on the background",
            "of every column; \\code{c(pct = \"difference\", mean = \"ratio\")} colors factors by the difference",
            "and numeric means by the ratio (text channel); \\code{list(pct = c(\"difference\", \"ratio\"),",
            "mean = \"ratio\")} combines both (per-type, with channels). Only \\code{difference} / \\code{ratio}",
            "may go on the background.",
            "Thresholds come from \\code{\\link{set_color_breaks}} or the per-table \\code{color_breaks}",
            "argument. \\code{color} also names the table's COMPARISON, and so decides which interval",
            "\\code{ci = \"auto\"} builds. (The old combined strings \\code{\"diff_ci\"}, \\code{\"after_ci\"} and",
            "\\code{\"ci\"} still work but are soft-deprecated in favor of \\code{color_signif}.)")),
  color_signif = list(
    default_for = list(tab_reg = NULL),
    default = "ignore",
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    values_from = "COLOR_SIGNIF_VALUES", values_rd = "color_signif_rd",
    doc = c("How significance gates the color, as a single string:",
            "{VALUES}",
            "With \\code{color = \"contrib\"} the three values are three readings of the same departure from",
            "independence, because a contribution has no confidence interval to floor:",
            " \\itemize{",
            "  \\item \\code{\"ignore\"} and \\code{\"grey_non_signif\"} color the \\strong{relative} contribution",
            "  (a share of \\emph{this} table's chi-squared, in multiples of the mean cell contribution --",
            "  the correspondence-analysis reading, so the scale is relative to the table);",
            "  \\item \\code{\"guaranteed_effect\"} colors the \\strong{adjusted standardized residual} itself, on",
            "  the absolute \\code{zscore} break scale (+/-1.96, +/-2.58, +/-3.89, +/-6 by default). Those",
            "  thresholds mean the same thing in every table, which is the SPSS \"adjusted residual\" reading.",
            " }",
            "In all three, significance is the adjusted standardized residual (Haberman; SPSS's \"adjusted",
            "residual\", R's \\code{chisq.test()$stdres}), \\emph{not} the Pearson residual \\code{(o-e)/sqrt(e)},",
            "whose variance is below 1 and which therefore under-rejects. Under weights the residual follows",
            "the package rule -- weighted estimate, and a base that follows the inference basis: the raw",
            "\\code{n} by default (the reading a correspondence analysis expects), and under",
            "\\code{options(tabxplor.design_effect = TRUE)} or a \\code{survey} design that raw \\code{n} divided",
            "by the \\strong{association's} design effect -- Rao-Scott's mean generalized delta-bar, the very",
            "one the whole-table test reports, so the colours and the p-value of one table describe one design",
            "effect. The",
            "contribution itself stays weighted (it estimates the population table's structure, and is therefore",
            "identical at every basis, which is what keeps the correspondence-analysis reading safe). One base",
            "for the whole table, so a counts table and a percentage table of the same data give the SAME",
            "residuals. Cells whose expected count is below 1 are left",
            "uncolored: the normal approximation does not hold there.",
            "Colors are computed per column at print time; since 2.0.0 each column records the confidence level",
            "it was built at, so the significance thresholds follow the call's \\code{conf_level}. A column that",
            "never recorded one (a hand-built \\code{\\link{fmt}}) falls back to",
            "\\code{options(tabxplor.conf_level)}.")),
  color_breaks = list(
    default = NULL,
    producers = c("tab", "tab_num", "tab_counts"), option = "color_breaks",
    values_from = "COLOR_SCALES",
    doc = c("A per-table override of the colour thresholds, in the form",
            "\\code{\\link{set_color_breaks}} accepts; unset scales keep the global ones.")),
  add_n = list(
    default_for = list(tab_reg = TRUE),
    default = TRUE,
    producers = c("tab", "tab_counts", "tab_reg"),
    doc = c("For `pct = \"row\"` or `pct = \"col\"`, set to `FALSE` not to add another",
            "column or row with unweighted counts (`n`).")),
  add_pct = list(
    default = FALSE,
    producers = c("tab", "tab_counts"),
    doc = c("Set to `TRUE` to add a column with the frequencies of the row",
            "variable (for `pct = \"row\"`) or a row with the frequencies of the column variable",
            "(for  `pct = \"col\"`).")),
  common_totrow = list(
    default = FALSE,
    producers = c("tab", "tab_counts"),
    doc = c("With several \\code{row_vars}, `FALSE` (the default) shows one Total row per",
            "row variable. Set to `TRUE` to collapse the identical Total rows into a single shared Total,",
            "displayed in its own group after a blank-line separator (bold when the total is the reference for",
            "at least one row variable). Genuinely different totals (e.g. under `na = \"drop\"`) are never merged.")),
  subtext = list(
    default_for = list(tab_reg = ""),
    default = "",
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    doc = "A character vector to print rows of legend under the table."),
  output_list = list(
    default = FALSE,
    producers = c("tab"),
    doc = c("Logical (default \\code{FALSE}). With several \\code{row_var}, \\code{FALSE}",
            " merges the mirror tables into a single \\code{tabxplor_tab}; \\code{TRUE} returns a list with",
            " one table per \\code{row_var}. With \\code{tab_vars}, tables stay a list regardless.")),
  parallel = list(
    default = NULL,
    # Phase 20f-iii (KEY 4): ONE `parallel` for both producers -- the same option, the same worker
    # count rule, the same pool, the same tab_parallel_stop(). What differs is the UNIT it maps
    # over, hence `doc_for`: `tab()` dispatches per `row_var`, `tab_reg()` per model (several
    # outcomes / a models list), per `tab_vars` group, and per outcome of a multi-outcome recursion.
    producers = c("tab", "tab_reg"), option = "parallel",
    doc_for = list(tab_reg = c(
      "Opt-in parallel build of the models of one call, using the (Suggests-only) \\pkg{mirai}",
      " package: several \\code{outcome}s, a \\code{predictors} list, or the \\code{tab_vars} groups.",
      " \\code{NULL} (default) reads \\code{getOption(\"tabxplor.parallel\")} (off); \\code{FALSE} forces",
      " serial; \\code{TRUE} uses an auto worker count; an integer sets the number of worker processes.",
      " Byte-identical to the serial result. It pays off for MANY, EVENLY SIZED models against a",
      " survey-size data frame, and is a loss otherwise (the pool costs about a second to start, and",
      " two uneven models cannot gain much). One shape is always serial and says so when asked:",
      " a model comparison (\\code{stats = \"compare_*\"}) is a test BETWEEN the fits, so they are",
      " built together. The worker pool persists for the session; release it with",
      " \\code{\\link{tab_parallel_stop}}.")),
    doc = c("Opt-in parallel build of the per-\\code{row_var} tables, using the (Suggests-only)",
            " \\pkg{mirai} package. \\code{NULL} (default) reads \\code{getOption(\"tabxplor.parallel\")} (off);",
            " \\code{FALSE} forces serial; \\code{TRUE} uses an auto worker count; an integer sets the number of",
            " worker processes. Byte-identical to the serial result. It pays off for the survey workflow --",
            " \\emph{many} \\code{row_vars} against a small/medium data frame (roughly 10k-60k rows) in ONE",
            " \\code{tab()} call -- and is a loss for few tables or multi-million-row data (so it stays opt-in).",
            " The worker pool persists for the session; release it with \\code{\\link{tab_parallel_stop}}.")),
  spread_vars = list(
    default = character(),
    producers = c("tab", "tab_counts"),
    doc = c("<\\link[tidyr:tidyr_tidy_select]{tidy-select}> A subset of \\code{tab_vars}",
            " to pivot from subtables into columns, via \\code{\\link{tab_spread}} (applied at the end).")),
  names_prefix = list(
    default = NULL,
    producers = c("tab", "tab_counts"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` These belong to",
            " \\code{\\link{tab_spread}}, which is the function that names the new columns; they reach it only",
            " when \\code{spread_vars} is given. Call \\code{tab_spread()} yourself for control over the names.")),
  names_sort = list(producers = c("tab", "tab_counts"), status = "deprecated", default = FALSE,
                    doc_with = "names_prefix"),
  cleannames = list(
    default_for = list(tab_reg = NULL),
    default = NULL,
    producers = c("tab", "tab_counts", "tab_reg"), option = "cleannames",
    doc = c("Set to \\code{TRUE} to clean levels names, by removing",
            "prefix numbers like \"1-\", and text in parenthesis. All data formatting arguments are",
            "passed to \\code{\\link{tab_prepare}}.")),
  other_if_less_than = list(
    default = 0,
    producers = c("tab"),
    doc = c("When set to a positive integer, levels with less count",
            "than it will be merged into an \"Others\" level.")),
  other_level = list(
    producers = c("tab"),
    doc = "The name of the \"Other\" level, as a single string."),
  filter = list(
    producers = c("tab", "tab_many"), status = "superseded",
    doc = c("`r lifecycle::badge(\"superseded\")` A",
            "\\code{\\link[dplyr:filter]{dplyr::filter}} to apply to the data frame first, as a single string",
            "(which will be converted to code, i.e. to a call). Prefer filtering the data with",
            "\\code{\\link[dplyr:filter]{dplyr::filter}} upstream of \\code{tab()}; this argument is kept",
            "for back-compatibility (e.g. printing multiple tabs from a",
            "\\code{\\link[tibble:tribble]{tibble::tribble}}).")),
  .cache = list(
    producers = c("tab"), status = "internal",
    doc = c("Internal, for the jamovi",
            "\\code{jmvtab} live cache only: \\code{.cache} is a mutable environment the content-addressed",
            "multi-tier store is threaded through (Phase 7e); \\code{.defer_level_merge} keeps full factor",
            "levels through the aggregate and test so \\code{levels} becomes a display-time drop;",
            "\\code{.return_armed} (Phase 7f) returns the pre-\\code{finalize_color_spec} table so the tier-3",
            "cache can re-paint colours without a rebuild; \\code{.levels_order} (Phase 7g-ii) is a named list",
            "of factor level orders applied post-aggregate, backing the jamovi level-reordering control (in R,",
            "relevel with \\code{\\link[forcats:fct_relevel]{forcats::fct_relevel}} before calling \\code{tab()});",
            "\\code{.levels_collapse} (Phase 20g-ii) is its twin for MERGING levels -- a named list, one",
            "element per variable, of merged label -> the levels it swallows -- applied pre-aggregate, so it",
            "is exactly \\code{\\link[forcats:fct_collapse]{forcats::fct_collapse}} on the data before",
            "\\code{tab()}, which is how to do it in R.",
            "All default off; not for direct use.")),
  .defer_level_merge = list(producers = c("tab"), status = "internal", doc_with = ".cache"),
  .return_armed = list(producers = c("tab"), status = "internal", doc_with = ".cache"),
  .levels_order = list(producers = c("tab"), status = "internal", doc_with = ".cache"),
  .levels_collapse = list(producers = c("tab", "tab_reg"), status = "internal", doc_with = ".cache"),
  num = list(
    default = FALSE,
    producers = c("tab_plain", "tab_num"),
    doc = "Set to \\code{TRUE} to obtain a table with normal numeric vectors (not fmt)."),
  df = list(
    default = FALSE,
    producers = c("tab_plain", "tab_num"),
    doc = c(" Set to \\code{TRUE} to obtain a plain data.frame (not a tibble),",
            "with normal numeric vectors (not fmt). Useful, for example, to pass the table to",
            "correspondence analysis with \\pkg{FactoMineR}.")),
  .fine = list(
    default = NULL,
    producers = c("tab_plain", "tab_num"), status = "internal",
    doc = c("Internal. `.fine` is a pre-computed count-aggregate to roll up from",
            "instead of scanning the raw data (used by \\code{\\link{tab_counts}} and the scan-fusion path);",
            "`.by_table` forces the table-by-table path.")),
  .by_table = list(producers = c("tab_plain", "tab_num"), status = "internal", default = FALSE,
                   doc_with = ".fine"),
  counts = list(
    producers = c("tab_counts"),
    doc = "The column holding the **unweighted** count for each cell (long tidy shape)."),
  wt_counts = list(
    producers = c("tab_counts"),
    doc = c("Optional column holding the **weighted** count for each cell. Leave empty for an",
            "  unweighted table.")),
  cols = list(
    producers = c("tab_counts"),
    doc = c("<[`tidy-select`][tidyr::tidyr_tidy_select]> For a wide `data.frame`: the columns",
            "  holding the `col_var` levels.")),
  col_name = list(
    default = "variable",
    producers = c("tab_counts"),
    doc = "Name of the (synthesised) column variable when `cols` is used."),
  base = list(
    producers = c("tab_counts"),
    doc = "For `input = \"pct\"`: the column holding each row's sample size N."),
  input = list(
    default = c("counts", "pct"),
    producers = c("tab_counts"), values = c("counts", "pct"), size = 1L, validate = FALSE,
    doc = c("`\"counts\"` (default) or `\"pct\"` (with `cols` and `base`: the level columns hold",
            "  frequencies, and counts are rebuilt from them and `base`).")),
  totrow = list(
    producers = c("tab_many"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` Use [tab()]'s `tot`. A total row is",
            "  always computed and exactly one total column is shown, so both are cosmetic; `totcol = \"each\"`",
            "  and `\"all_col_vars\"` now give that same single total column instead of erroring.")),
  totcol = list(producers = c("tab_many"), status = "deprecated", values = c("last", "each", "all_col_vars", "no", ""), size = 1L, doc_with = "totrow"),
  compact = list(
    producers = c("tab_many"), status = "deprecated",
    doc = "`r lifecycle::badge(\"deprecated\")` Use [tab()]'s `output_list` (inverted)."),
  na_drop_all = list(
    producers = c("tab_many"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` <\\link[tidyr:tidyr_tidy_select]{tidy-select}>",
            "  Use [tab()]'s `filter`: `na_drop_all = c(a, b)` is `filter = !is.na(a) & !is.na(b)`.")),
  # --- tab_reg()'s own arguments (Phase 20c, KEY 4) -----------------------------------------------
  # Declared so tx_check_tab_args() covers the whole signature and `producers` / `default` /
  # `values_from` are stated once; the prose stays in R/tab_reg.R's roxygen (`doc_in_producer`),
  # because none of it has a duplicate to remove -- see the header.
  outcome = list(producers = "tab_reg", doc_in_producer = TRUE),
  predictors = list(producers = "tab_reg", default = NULL, doc_in_producer = TRUE),
  family = list(producers = "tab_reg", default = "auto", values_from = "REG_FAMILIES",
                doc_in_producer = TRUE),
  effect = list(producers = "tab_reg", default = "coefficient", values_from = "REG_ESTIMANDS",
                doc_in_producer = TRUE),
  measure = list(producers = "tab_reg", default = "auto", values_from = "REG_ESTIMANDS",
                 doc_in_producer = TRUE),
  trials = list(producers = "tab_reg", default = NULL, doc_in_producer = TRUE),
  empirical = list(producers = "tab_reg", default = FALSE, size = 1L, validate = FALSE,
                   values = c("no", "cell", "column"), doc_in_producer = TRUE),
  outcome_level = list(producers = "tab_reg", default = NULL, values_from = "REG_FAMILIES",
                       doc_in_producer = TRUE),
  multiplier = list(producers = "tab_reg", default = "sd", doc_in_producer = TRUE),
  shape = list(producers = "tab_reg", default = NULL, values_from = "REG_CHECKS",
               doc_in_producer = TRUE),
  stats = list(producers = "tab_reg", default = NULL, values_from = "TEST_ROWS",
               doc_in_producer = TRUE),
  output = list(
    producers = c("tab_build"), status = "internal",
    values = c("single", "list"), size = 1L,
    doc = "Internal. The shape tab_build() returns: one merged table, or a list.")
)

# --- EXPORT_ARGS: the RENDER surface (Phase 20h, KEY 8) -------------------------------------------
# The exporters' half of the argument surface, in the same shape and read through the same functions.
#
# WHY IT IS A SECOND TABLE AND NOT MORE ROWS OF TAB_ARGS -- three names mean something ELSE here, and
# a named list cannot hold two rows under one key:
#   `color`   on a producer is a MEASURE spec ("difference"); on an exporter a logical ("render in
#             colour at all").
#   `subtext` on a producer is the character vector of legend lines; on an exporter a logical
#             ("print the footer").
#   `stars`   likewise (a ladder vs "draw them"). It stays LOCAL to forest_plot -- see the scope rule.
# So this is not a duplicate of TAB_ARGS; it is the other producer family's surface.
#
# THE SCOPE RULE (this table's own admission test -- narrower than TAB_ARGS', deliberately):
# a row for an exporter argument that is EITHER
#   (i)  shared by >= 2 exporters, OR
#   (ii) the per-call twin of an option (TAB_OPTIONS$arg) that has no TAB_ARGS row.
# A single-backend geometry argument (`sheets`, `titles`, `colwidth`, `colnames_rotation`, the text
# sizes, `get_data`, `style_tag`, and forest_plot's ten plot controls) stays in its own roxygen: the
# table owns what is SHARED or CROSS-REFERENCED, not everything an exporter takes. That is why
# tx_check_tab_args() checks the exporters SCOPED (R/zzz-fact-keys.R), the way it already checks
# tab_build().
#
# ⚠ WHICH ROWS CARRY PROSE, AND WHY MOST DO NOT. The rule for moving prose is the one the header
# above states: *it must remove a DUPLICATE*. Measured across the seven exporters, that is true of
# nine concepts (`lang`, `var_names`, `color`, `color_legend`, `transpose`, `wrap_rows`, `wrap_cols`,
# `whitespace_only`, `tabs`/`x`), whose ~26 hand-written blocks said one thing in up to five
# wordings. It is NOT true of the rest, and one case is worth recording because it looks like the
# worst duplication in the package and is not:
#
#     `@param theme` is written seven times, but the ACCEPTED VALUES differ by backend --
#     `allow_auto = TRUE` is passed only by tab_html(), tab_md() and tab_css(), the three that ship a
#     stylesheet, so only they take "auto"; tab_plot() and tab_xl() resolve it to "light".
#     Seven texts describing five value sets are not one duplicate, so `theme` keeps its prose
#     per backend and takes `doc_in_producer = TRUE` -- a DECLARED row (which is what empties the
#     foreign key) with its documentation left where it is true.
#
# Same verdict, same mechanism, for `caption` (five different renderings: a pandoc caption line, an
# Excel title, an html caption needing CSS, a ggplot caption, "NULL keeps the table's own"), `css`,
# `format`, `file`, `path`, `subtext`, and every option twin whose prose is one backend's business.
#' @keywords internal
#' @noRd
EXPORT_ARGS <- list(
  # --- the table itself -------------------------------------------------------------------------
  tabs = list(
    producers = c("tab_html", "tab_md", "tab_xl", "tab_plot"),
    doc = c("A table made with \\code{\\link{tab}} or \\code{\\link{tab_reg}}, or a `list` of tab.",
            "A list of tables sharing the same `col_vars` (and no `tab_vars`) is merged into one; any",
            "other list --- several `row_vars` and/or `tab_vars` --- is rendered one table after",
            "another, each keeping its own sub-tables.")),
  # tab_export() and forest_plot() name the same thing `x`; one prose, each producer's own tag.
  x = list(producers = c("tab_export", "forest_plot"), doc_with = "tabs"),

  # --- the shared render controls ---------------------------------------------------------------
  color = list(
    producers = c("tab_html", "tab_xl", "tab_plot", "tab_export"), option = NULL,
    doc = "Set to \\code{FALSE} to render the table without colours (monochrome).",
    # md wraps each cell in a pandoc span, and a forest plot colours POINTS -- two real differences.
    doc_for = list(
      tab_md = c("When `TRUE` (default) and the table carries colours (e.g. built with",
                 "`tab(..., color = \"difference\")`), each fmt cell is wrapped in a short pandoc",
                 "bracketed span `[value]{.class}` so the markdown renders coloured in Quarto /",
                 "RMarkdown / pandoc (and \\code{\\link[=tab_css]{tab_css(format = \"md\")}} styles the",
                 "classes). `FALSE` produces plain monochrome markdown. Uncoloured tables never get",
                 "spans."),
      forest_plot = "Set to \\code{FALSE} for a plain plot with no colour measure.")),
  color_legend = list(
    producers = c("tab_html", "tab_md", "tab_xl", "tab_plot", "tab_export"),
    doc = c("Print the colour legend below the table (with the subtext). `TRUE` by default, and a",
            "no-op on a table that carries no colours.")),
  lang = list(
    producers = c("tab_html", "tab_md", "tab_xl", "tab_plot", "tab_export", "forest_plot"),
    option = "lang",
    doc = c("Colour-legend language: \\code{NULL} (auto from the R/OS locale, English fallback),",
            "\\code{\"en\"} or \\code{\"fr\"}.")),
  transpose = list(
    producers = c("tab_html", "tab_md", "tab_xl", "tab_plot", "tab_export"),
    doc = c("Set to \\code{TRUE} to transpose each table before export (rows become columns) --",
            "the col-percentages-with-several-row-variables use case.")),
  var_names = list(
    producers = c("tab_html", "tab_md", "tab_xl", "tab_plot", "tab_export"), option = "var_names",
    doc = c("Which variable names to write beside the table: `\"both\"` (the default), `\"rows\"`,",
            "`\"cols\"` or `\"none\"`. The row-variable name is the leading column a table with several",
            "`row_vars` uses to name each block (written once per block); the column-variable names",
            "are the spanning row above their level columns. Level headers always keep their name.")),
  wrap_rows = list(
    producers = c("tab_html", "tab_md", "tab_plot"),
    doc = "By default, rownames are wrapped when larger than 30 characters.",
    # a markdown pipe cell cannot hold a raw newline, so md can only truncate.
    doc_for = list(
      tab_md = c("Max width for row labels before truncation. `NULL` (default) never truncates",
                 "(lossless -- the column grows); set a number to cap the label width. A markdown pipe",
                 "cell cannot hold a raw newline, so md \"wrapping\" means \"do not truncate\"."))),
  wrap_cols = list(
    producers = c("tab_html", "tab_plot"),
    doc = "By default, colnames are wrapped when larger than 12 characters."),
  whitespace_only = list(
    producers = c("tab_html", "tab_plot"),
    doc = "Set to `FALSE` to wrap also on non whitespace characters."),

  # --- DECLARED, prose stays home (see the header) ------------------------------------------------
  theme = list(producers = c("tab_html", "tab_md", "tab_xl", "tab_plot", "tab_export", "tab_css",
                             "forest_plot"),
               option = "theme", doc_in_producer = TRUE),
  caption = list(producers = c("tab_html", "tab_md", "tab_xl", "tab_plot", "tab_export",
                               "forest_plot"), doc_in_producer = TRUE),
  css = list(producers = c("tab_html", "tab_md"), option = "css", doc_in_producer = TRUE),
  format = list(producers = c("tab_export", "tab_css"), doc_in_producer = TRUE),
  file = list(producers = c("tab_md", "tab_css"), doc_in_producer = TRUE),
  path = list(producers = c("tab_xl", "tab_export"), doc_in_producer = TRUE),
  subtext = list(producers = c("tab_md", "forest_plot"), doc_in_producer = TRUE),
  tooltips = list(producers = "tab_html", option = "tooltips", doc_in_producer = TRUE),
  popover  = list(producers = "tab_html", option = "popover",  doc_in_producer = TRUE),
  print_rules = list(producers = "tab_css", option = "print_rules", doc_in_producer = TRUE),
  or_numeric  = list(producers = "tab_xl",  option = "or_numeric",  doc_in_producer = TRUE),
  font_text = list(producers = "tab_xl", option = "font_text", doc_in_producer = TRUE),
  font_num  = list(producers = "tab_xl", option = "font_num",  doc_in_producer = TRUE),
  font_num_stars = list(producers = "tab_xl", option = "font_num_stars", doc_in_producer = TRUE)
)

# EXPORT_PRODUCERS -- DERIVED, never a hand-written mapping: it is what tells the shared readers which
# of the two tables declares a given producer.
#' @keywords internal
#' @noRd
EXPORT_PRODUCERS <- sort(unique(unlist(lapply(EXPORT_ARGS, `[[`, "producers"))))

# THE table that declares a producer's arguments. One line, so `tab_args_rd()` / `tab_args_for()` /
# `tab_arg()` / `tab_arg_status()` serve both surfaces without knowing there are two.
#' @keywords internal
#' @noRd
arg_table_of <- function(producer)
  if (producer %in% EXPORT_PRODUCERS) EXPORT_ARGS else TAB_ARGS

# --- the derived vocabulary view ------------------------------------------------------------------
# TAB_ARG_VALUES survives, DERIVED, with its contents AND ORDER intact (the DISPLAY_TOKENS
# precedent, 19m-iii): tab_validate_args(), tab_deprecate_many(), tab_ci()'s totcol guard and
# test-jamovi-vocabulary.R all read it unchanged.
# ⚠ `validate = FALSE` is what keeps `ci` out of it. Its vocabulary is DECLARED here (so
# resolve_ci_value() stops spelling it twice in its own body) but it is validated by that resolver
# instead, because two of its values are soft-deprecated: validating them means REWRITING them.
#' @keywords internal
#' @noRd
TAB_ARG_VALUES <- local({
  keys <- names(TAB_ARGS)[vapply(TAB_ARGS, function(r)
    !is.null(r[["values"]]) && !identical(r[["validate"]], FALSE), logical(1))]
  stats::setNames(lapply(keys, function(k) {
    r <- TAB_ARGS[[k]]
    list(values = r[["values"]], leaf = r[["leaf"]],
         size = r[["size"]] %||% NA, na_ok = isTRUE(r[["na_ok"]]))
  }), keys)
})

# --- the readers ----------------------------------------------------------------------------------
#' @keywords internal
#' @noRd
tab_arg <- function(name, producer = NULL)
  if (is.null(producer)) TAB_ARGS[[name]] else arg_table_of(producer)[[name]]

# Every argument a producer declares. `formals()` is the ORDER (it matches \usage{}) and the
# declared set is the CHECK: the two must agree, which is asserted at load in R/zzz-fact-keys.R.
#' @keywords internal
#' @noRd
tab_args_for <- function(producer) {
  tb <- arg_table_of(producer)
  names(tb)[vapply(tb, function(r) producer %in% r[["producers"]], logical(1))]
}

# An argument's status FOR ONE PRODUCER: a bare string applies everywhere, a named one overrides
# per producer (see the header's `row_var` note).
#' @keywords internal
#' @noRd
tab_arg_status <- function(name, producer = NULL) {
  st <- (if (is.null(producer)) TAB_ARGS else arg_table_of(producer))[[name]][["status"]]
  if (is.null(st)) return("live")
  if (is.null(names(st))) return(st[[1]])
  if (!is.null(producer) && producer %in% names(st)) st[[producer]] else "live"
}

# --- `...` on the superseded producers ------------------------------------------------------------
# tab_check_dots() -- THE validator that makes `...` a net gain rather than a loss. Before it, a typo
# produced R's bare "unused argument"; now it produces a suggestion, and an UNNAMED extra argument is
# refused by name rather than silently bound to whatever formal happened to sit at that position.
#
# ⚠ SCOPE -- three surfaces, two reaches:
#   tab_args_rd()   serves BOTH the crosstab producers and the exporters (via arg_table_of): a
#     declaration is documentation either way.
#   tab_check_dots() validates the `...` of the crosstab producers AND tab_reg() (Phase 20j: one
#     dots-validator for both, replacing tab_reg()'s own retired-arg guard) -- every declared formal
#     is a known name, and a dot-prefixed name is skipped as internal plumbing. It does NOT serve the
#     exporters: a backend's `...` is a pass-through, so refusing an unknown name would refuse a
#     legitimate backend argument, and tx_deprecate_inert() (R/utils.R) already names the retired ones.
#   tab_dots_expand() serves the CROSSTAB leaves only (it fills an unsupplied formal from its default;
#     tab_reg() declares every argument as a real formal, so it needs no expansion).
# The exporters' narrower scope (EXPORT_ARGS' header) is why tab_check_dots() stops at them.
#' @keywords internal
#' @noRd
tab_check_dots <- function(dots, producer, call = rlang::caller_env()) {
  nms <- names(dots)
  if (length(dots) && (is.null(nms) || any(!nzchar(nms)))) {
    pos <- if (is.null(nms)) seq_along(dots) else which(!nzchar(nms))
    cli::cli_abort(c(
      "{cli::qty(length(pos))}Argument{?s} {pos} of {.fn {producer}} {?is/are} not named.",
      "i" = "Since 2.0.0 only {.arg data} and the variable roles are positional; everything else
             must be named (this is what stops a value landing in the wrong argument)."),
      call = call)
  }
  if (!length(dots)) return(invisible(TRUE))
  known <- tab_args_for(producer)
  # A DOT-PREFIXED name is internal plumbing, never a user argument: `.fit_cache` /
  # `.levels_collapse` (tab_reg's jamovi-live cache + level-merge spec) ride `...`, as do tab()'s
  # `.cache` / `.return_armed`. The convention is the whole package's, so the validator honours it.
  bad   <- setdiff(nms, known)
  bad   <- bad[!startsWith(bad, ".")]
  if (!length(bad)) return(invisible(TRUE))
  # Two kinds of near miss, and the second matters more than it looks: an argument sitting AFTER
  # `...` is matched EXACTLY, so an ABBREVIATION that R's partial matching used to accept silently
  # now arrives here. It must be named, not merely refused.
  near <- function(x) {
    pre <- known[startsWith(known, x)]
    d   <- utils::adist(x, known, ignore.case = TRUE)[1, ]
    typo <- known[d <= max(1L, floor(nchar(x) / 3))]
    unique(c(pre, typo[order(d[match(typo, known)])]))
  }
  sug <- near(bad[[1]])
  cli::cli_abort(c(
    "{cli::qty(length(bad))}Unknown argument{?s} {.arg {bad}} in {.fn {producer}}.",
    if (length(sug)) c("i" = "Did you mean {.arg {sug[1:min(2L, length(sug))]}}?")
    else c("i" = "See {.fn {producer}} for the arguments it takes.")),
    call = call)
}

# Read one argument out of a captured `...`. The quosures are captured by the CALLER (rlang::enquos),
# so an NSE argument stays a quosure and a value argument is evaluated in the user's own environment.
#' @keywords internal
#' @noRd
dots_value <- function(dots, name, default = NULL) {
  if (!name %in% names(dots)) return(default)
  rlang::eval_tidy(dots[[name]])
}

# --- the generated documentation ------------------------------------------------------------------
# tab_args_rd() -- the `#' @eval` generator behind every producer's `@param` block (the
# reg_measures_rd() precedent, but emitting `@param` tags rather than an `@section`).
#
# The ORDER is formals(); the SET is asserted equal to the declared one at load. An argument
# documented WITH another (`col_vars` with `row_vars`) is folded into one comma-separated tag, in the
# order the formals give.
#' @keywords internal
#' @noRd
tab_args_rd <- function(producer) {
  tb  <- arg_table_of(producer)
  fn  <- get(producer, envir = asNamespace("tabxplor"))
  nms <- setdiff(names(formals(fn)), "...")
  nms <- intersect(nms, names(tb))
  owner <- function(k) tb[[k]][["doc_with"]] %||% k
  out <- character(0)
  done <- character(0)
  for (k in nms) {
    o <- owner(k)
    if (o %in% done) next
    done <- c(done, o)
    # ⚠ the tag head must be a formal OF THIS PRODUCER: tab_num() takes `row_var` and `col_vars`,
    # whose doc owners are `row_var` and `row_vars` -- emitting the owner blindly would document a
    # `row_vars` this function does not have (checkDocFiles catches it, but only after the fact).
    tag <- nms[vapply(nms, owner, character(1)) == o]
    tag <- c(intersect(o, tag), setdiff(tag, o))
    r   <- tb[[o]]
    # a row may hold ONE prose per producer (`doc_for`) where the same argument genuinely reads
    # differently -- `na`'s two vocabularies, `color`'s two channel sets. `default_for`'s idiom.
    body <- r[["doc_for"]][[producer]] %||% r[["doc"]]
    # ...and a row may hold NO prose at all: see `doc_in_producer` in the header.
    if (is.null(body)) next
    if (!is.null(r[["values_rd"]])) {
      vals <- do.call(r[["values_rd"]], list(producer = producer))
      at   <- which(body == "{VALUES}")
      # the list goes where the prose asks for it, not at the end: an argument's value list usually
      # sits mid-paragraph, with the grammar explained after it.
      body <- if (length(at)) append(body[-at], vals, after = at[[1]] - 1L) else c(body, vals)
    }
    out <- c(out, paste0("@param ", paste(tag, collapse = ","), " ", body[[1]]), body[-1])
  }
  out
}

# --- the value-list renderers ---------------------------------------------------------------------
# Each reads the table that OWNS the vocabulary, so a measure or a policy is described exactly once,
# in the file that declares it. `producer` filters: a reg-only measure is not offered in ?tab, and a
# crosstab-only one is not offered in ?tab_reg -- which is the disagreement three hand-written value
# lists used to encode three ways.
#' @keywords internal
#' @noRd
color_measures_rd <- function(producer = "tab") {
  who <- if (identical(producer, "tab_reg")) "reg" else "tab"
  keys <- names(MEASURES)[vapply(MEASURES, function(m)
    who %in% m[["producers"]] && "text" %in% m[["channels"]], logical(1))]
  c(" \\itemize{",
    vapply(keys, function(k)
      paste0("  \\item \\code{\"", k, "\"}: ", MEASURES[[k]][["doc"]]), character(1)),
    " }")
}

# The significance POLICY vocabulary. Its values are COLOR_SIGNIF_VALUES (fmt_class.R); the glosses
# are the argument's own business, so they live here.
#' @keywords internal
#' @noRd
COLOR_SIGNIF_DOC <- c(
  ignore = "color every deviation by its observed size.",
  grey_non_signif = paste(
    "color by the observed size, but grey out cells whose deviation is not significant at",
    "\\code{conf_level}. A cell is coloured only when it is BOTH significant AND at least as large",
    "as the first colour threshold, so an un-coloured (grey) cell may still be significant -- just",
    "too small to colour (and it can carry significance stars). The only guarantee is: a coloured",
    "cell is significantly different from its reference."),
  guaranteed_effect = paste(
    "color by the guaranteed (confidence-bound) effect -- only cells whose interval clears the",
    "threshold, with dimmer, conservative colors.")
)

# The FIRST value is the default -- the convention CI_METHODS already uses ("first = the default"),
# so the marker is derived rather than typed into the prose beside it.
# ⚠ `producer` is UNREAD here and KEPT: it is the `values_rd` calling convention (tab_args_rd() passes
# it to every renderer), so the formal is the INTERFACE, not weight. The policies are the same on both
# producers -- unlike the measures, which color_measures_rd() really does filter.
#' @keywords internal
#' @noRd
color_signif_rd <- function(producer = "tab") {
  c(" \\itemize{",
    vapply(seq_along(COLOR_SIGNIF_VALUES), function(i) {
      k <- COLOR_SIGNIF_VALUES[[i]]
      paste0("  \\item \\code{\"", k, "\"}", if (i == 1L) " (default)" else "", ": ",
             COLOR_SIGNIF_DOC[[k]])
    }, character(1)),
    " }")
}

# --- build-time exhaustiveness --------------------------------------------------------------------
# The CROSS-table edges (a `values_from` / `values_rd` / `option` naming something that does not
# exist, and every producer's formals against its declared rows) live in R/zzz-fact-keys.R, which is
# the only file that sorts after all of them. What is checked HERE is this table's own shape.
stopifnot(
  all(vapply(TAB_ARGS, function(r) !is.null(r[["producers"]]) && is.character(r[["producers"]]), logical(1))),
  # every row says where its prose is: here (`doc`), on a sibling row (`doc_with`), or in the
  # producer's own roxygen (`doc_in_producer`). What is forbidden is saying nothing.
  all(vapply(TAB_ARGS, function(r) !is.null(r[["doc"]]) || !is.null(r[["doc_with"]]) ||
               isTRUE(r[["doc_in_producer"]]), logical(1))),
  all(vapply(TAB_ARGS, function(r)
    is.null(r[["doc_with"]]) || r[["doc_with"]] %in% names(TAB_ARGS), logical(1))),
  all(vapply(TAB_ARGS, function(r) is.null(r[["status"]]) ||
               all(r[["status"]] %in% c("live", "deprecated", "superseded", "internal")), logical(1))),
  all(vapply(TAB_ARGS, function(r) is.null(r[["check"]]) ||
               r[["check"]] %in% c("probability", "count"), logical(1))),
  # the derived view must hold EXACTLY the nine 19i declared -- `ci` and `input` are declared here
  # too but validated by their own resolvers (resolve_ci_value / rlang::arg_match), which is what
  # `validate = FALSE` says.
  setequal(names(TAB_ARG_VALUES),
           c("pct", "na", "levels", "comp", "tot", "totaltab", "totcol", "output", "anova"))
)

# ...and the SAME shape rules on the render surface (Phase 20h). Stated as its own block rather than a
# loop over both tables: the two differ in what they may declare, and the differences are the point.
stopifnot(
  all(vapply(EXPORT_ARGS, function(r)
    !is.null(r[["producers"]]) && is.character(r[["producers"]]), logical(1))),
  all(vapply(EXPORT_ARGS, function(r) !is.null(r[["doc"]]) || !is.null(r[["doc_with"]]) ||
               isTRUE(r[["doc_in_producer"]]), logical(1))),
  all(vapply(EXPORT_ARGS, function(r)
    is.null(r[["doc_with"]]) || r[["doc_with"]] %in% names(EXPORT_ARGS), logical(1))),
  # ⚠ no `values` on this table, so it CANNOT feed TAB_ARG_VALUES (whose derived set is asserted
  # exactly above). A render argument's vocabulary lives with its resolver (tx_theme_resolve,
  # resolve_export_opts), not here.
  all(vapply(EXPORT_ARGS, function(r) is.null(r[["values"]]), logical(1))),
  # the two surfaces may share a NAME (`color` / `subtext` / `stars`) but must not share a producer:
  # one function's arguments are declared in exactly one table, or arg_table_of() would be a coin toss.
  length(intersect(
    EXPORT_PRODUCERS,
    unique(unlist(lapply(TAB_ARGS, `[[`, "producers"))))) == 0L
)

# tab_dots_expand() -- a superseded producer's `...` becomes the arguments it declares, each
# unsupplied one filled from its DECLARED default.
#
# WHY THE DEFAULTS ARE DECLARED (`default` / `default_for`). tab_plain(), tab_num() and tab_counts()
# share 25-34 of their formals with tab(), and the mirrors documented themselves as "same meaning as
# in tab()" -- but their DEFAULTS were not all the same, and nothing said which: tab_num() alone
# starts from `color = "auto"`, `ref = "tot"`, `comp = c("tab", "all")` and `na = c("keep", "drop")`,
# and both leaves start from `tot = NULL`. Moving those formals into `...` would have thrown that
# away silently, so the divergences are stated (`default_for`) instead of lost -- and a load-time
# check in R/zzz-fact-keys.R holds the surviving formals to what is declared here.
#' @keywords internal
#' @noRd
tab_dots_expand <- function(dots, producer) {
  # ⚠ crosstab producers ONLY (see tab_check_dots()'s SCOPE note): an exporter's `...` is a
  # pass-through to its backend, so nothing there is filled from a declared default.
  keys <- setdiff(tab_args_for(producer), names(formals(get(producer, envir = asNamespace("tabxplor")))))
  out  <- list()
  for (k in keys) {
    r <- TAB_ARGS[[k]]
    if (k %in% names(dots)) { out[[k]] <- dots[[k]]; next }
    ov <- r[["default_for"]]
    d  <- if (!is.null(ov) && producer %in% names(ov)) ov[producer] else list(r[["default"]])
    # keep a NULL default a real NULL (`out[[k]] <- NULL` would DROP the entry)
    out[k] <- d
  }
  out
}
