# PURPOSE: THE argument surface as data.
# ROLE: an argument of a crosstab producer is declared ONCE -- which producers take it, what it may
#   be, which option is its default, and what it means -- and the signature, the reference page and
#   the value list all read that declaration. TAB_ARGS covers the producers; EXPORT_ARGS is its twin
#   for the render surface.
# DESIGN -- THE RULE, and it is what keeps this table from swallowing the fact tables:
#     *** THE FACT TABLE OWNS THE VOCABULARY. TAB_ARGS OWNS THE ARGUMENT. ***
#   MEASURES knows what `difference` is; TAB_ARGS knows that `color` is an argument of four
#   producers, that it names a measure, and how to say so in a help page. `values_rd` is the edge
#   between them, checked at load in R/zzz-fact-keys.R. An argument whose vocabulary has no other
#   home -- `na`, `tot`, `levels`, `totaltab`, `comp`, `pct` -- declares it HERE, in `values`, which
#   is why TAB_ARG_VALUES is derived from this table rather than living beside it.
# DESIGN: `doc` IS the roxygen text, rendered by tab_args_rd(), which orders by formals() and
#   ASSERTS that the declared set and the formals are the same set -- so an argument added to a
#   signature without a row breaks the build. `doc_in_producer = TRUE` says the prose stays in the
#   producer's own roxygen. tab_reg() declares all 25 of its formals here, so tx_check_tab_args()
#   polices its signature exactly as it polices a crosstab one -- but it does NOT get
#   @eval tab_args_rd(): the two producers share the NAME and the GRAMMAR of `wt` / `ref` / `na` /
#   `display` / `color` / `ci_method` / `tab_vars` / `conf_level` / `data`, and not the PROSE. Every
#   one of those reads differently on a model, and emitting the crosstab text into ?tab_reg would be
#   WRONG documentation, not deduplicated documentation.
# KEY CONSTRAINTS:
#   - There is deliberately NO `group` column: ordering by formals() matches \usage{} and is
#     self-checking, which left `group` with no reader. Add it the day something groups by it.
#   - ⚠ `status` may be a NAMED vector when an argument is deprecated on ONE producer and live on
#     another (`row_var` is a deprecated alias on tab() and the real formal of the leaves). Read it
#     with tab_arg_status(name, producer), never with `$status`.
#   - EXPORT_ARGS is a separate table rather than more rows: `color`, `subtext` and `stars` mean
#     something structurally different on an exporter than on a producer. Its own scope rule is
#     stated at the table.
#   - This file sorts before tab.R, so nothing here may read tab.R's top-level objects at SOURCE
#     time. Everything below is a literal or a function body.
# See: CLAUDE.md § tabxplor architecture (the declarative architecture); R/tab-options.R (the option
#   twins this table's `option` column points at).

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
            "    \\item a \\strong{named layout}, e.g. \\code{\"est_ci\"} or \\code{\"base_ratio\"} ---",
            "      the whole list is in \\emph{Display layouts} below, and every name works on",
            "      \\code{\\link{tab_reg}} too.",
            "    \\item a \\strong{single field}, e.g. \\code{display = \"ci\"} or \\code{\"diff\"}. Valid",
            "      fields are listed in \\emph{Display fields} below.",
            "    \\item a \\strong{\\{\\} template} combining several, e.g. \\code{\"\\{pct\\} (n=\\{n\\})\"}",
            "      (a percentage with its count) or \\code{\"\\{pct\\} (\\{resid\\})\"} (each percentage with",
            "      the standardized residual that says whether it departs from independence --- the",
            "      SPSS cell layout).",
            "  }",
            "  An acronym spells the field of the same name: \\code{\"OR\"} is \\code{\"or\"} and",
            "  \\code{\"rr\"} is \\code{\"ratio\"}, in a template as much as on their own.",
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
  # `dots`: still current, but out of tab()'s signature -- a table always HAS both totals, so this
  # only says which ones to show, and a crowded signature is the wrong place to ask that.
  tot = list(
    default = c("row", "col"), default_for = list(tab_plain = NULL, tab_num = NULL), dots = "tab",
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
    doc = c("The reference cell that differences and ratios are computed against:",
            " \\itemize{",
            "  \\item \\code{\"auto\"} (default): the corresponding total (row or column, following",
            "  \\code{pct}) for a difference; the first row (or column) for an odds ratio.",
            "  \\item \\code{\"tot\"}: always the total.",
            "  \\item \\code{\"first\"}: the first cell of the row or column -- useful to color a temporal",
            "  development.",
            "  \\item \\code{\"last\"}: its mirror, the \\strong{last level} of the row (or column) variable. A",
            "  total row or column is not a level and is never selected: use \\code{\"tot\"} for that.",
            "  \\item an \\strong{integer}: the nth row (or column).",
            "  \\item a \\strong{string}: a regular expression matched against the row (or column) names. Be",
            "  precise enough to match only one, otherwise you get a warning.",
            "  \\item \\code{\"no\"}: no reference, and no differences computed -- saves calculation time.",
            "}",
            "Resolved inside each subtable when there are \\code{tab_vars}.",
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
            "passed}, and the table's footer names it:",
            "\\enumerate{",
            " \\item \\code{wt = w} --- the estimates, the test and the effect size are all \\strong{weighted},",
            " but the sample size is the raw number of respondents, so they carry no design effect (default).",
            " \\item plus \\code{design_effect = TRUE} --- the intervals and the test \\strong{account for the",
            " unequal weighting, exactly}: a weight column IS a survey design (the flat one), so this is not an",
            " approximation, and it can make an interval \\emph{narrower} as well as wider. It stays blind to",
            " clustering and to calibration, which weights do not record.",
            " \\item a \\code{survey::svydesign} passed as \\code{data} --- fully \\strong{design-based}: strata,",
            " clusters, \\code{fpc} and calibration, every interval referred to the design's own degrees of",
            " freedom. When that variance cannot be computed, the table falls back to the weighting-only",
            " correction and says so.",
            "}",
            "Replicate-weight (\\code{svrepdesign}) and two-phase designs are not supported, and \\code{wt}",
            "beside a design is an error (a design already carries its own weights).")),
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
            " Methods are chosen with \\code{ci_method} and named in the table's legend. The interval prints",
            " as \\code{[inf;sup]}; \\code{display = \"base_moe\"} writes it as \\code{pct +- margin of error}",
            " instead, and \\code{display = \"base_ci\"} as \\code{pct [inf;sup]}.",
            " \\code{\"diff\"} and \\code{\"ratio\"} are soft-deprecated spellings of \\code{\"ref\"} (the second one",
            " also pins the ratio scale -- say \\code{color = \"ratio\"} instead).")),
  conf_level = list(
    default = NULL,   # NULL everywhere; each producer's own boundary resolves it at call time.
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
    # on tab_reg it rides `...`: one binary choice (wald / profile) does not earn a place in a
    # signature a user reads to learn the producer.
    dots = "tab_reg",
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
    doc = c("Which measure(s) of deviation to color, on which visual channel. \\code{FALSE} (default)",
            "prints no color; \\code{TRUE} uses the smart per-column-type scheme (factors: the",
            "\\code{difference} on the text + the \\code{ratio} on the background; numerics: the",
            "\\code{ratio}; counts: \\code{contrib}). Otherwise a measure name, on the \\strong{text} channel:",
            "{VALUES}",
            "The acronyms in brackets are permanent aliases, the same words \\code{\\link{tab_reg}}'s",
            "\\code{measure} takes, each with an all-lowercase twin (\\code{\"rd\"}, \\code{\"rr\"},",
            "\\code{\"irr\"}, \\code{\"rom\"}, \\code{\"or\"}). An acronym always names a \\strong{measure}:",
            "\\code{display =} names a \\emph{field} and \\code{OR =} / \\code{ref2 =} a \\emph{level},",
            "so the same letters mean different things in those arguments.",
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
            "  -- a share of \\emph{this} table's chi-squared, in multiples of the mean cell contribution, so",
            "  the scale is relative to the table (the correspondence-analysis reading);",
            "  \\item \\code{\"guaranteed_effect\"} colors the \\strong{adjusted standardized residual} itself, on",
            "  the absolute \\code{zscore} break scale (+/-1.96, +/-2.58, +/-3.89, +/-6 by default), whose",
            "  thresholds mean the same thing in every table (the SPSS \"adjusted residual\" reading).",
            " }",
            "In all three, significance is the adjusted standardized residual (Haberman; SPSS's \"adjusted",
            "residual\", R's \\code{chisq.test()$stdres}), \\emph{not} the Pearson residual",
            "\\code{(o-e)/sqrt(e)}, which under-rejects. Under weights it uses ONE base for the whole table,",
            "following the table's inference basis, so a counts table and a percentage table of the same data",
            "give the SAME residuals. Cells whose expected count is below 1 are left uncolored: the normal",
            "approximation does not hold there. Thresholds follow the call's \\code{conf_level}, which each",
            "column records; a hand-built \\code{\\link{fmt}} falls back to \\code{options(tabxplor.conf_level)}.")),
  color_breaks = list(
    default = NULL,
    producers = c("tab", "tab_num", "tab_counts"), option = "color_breaks",
    values_from = "COLOR_SCALES",
    doc = c("A per-table override of the colour thresholds, in the form",
            "\\code{\\link{set_color_breaks}} accepts; unset scales keep the global ones.")),
  n = list(
    default = NULL,
    producers = c("tab", "tab_counts", "tab_reg"), option = "n",
    values = c("range", "min", "no"), size = 1L,
    doc = c("How many people this table is about. \\code{\"range\"} (the default) prints the",
            "unweighted base beside the \\code{Total} cell --- \\code{100\\% (9 838)} --- and, in a",
            "\\code{\\link{tab_reg}} table, in the \\code{n} column beside each predictor level.",
            "When the parts of the table do not rest on the same people --- several column",
            "variables losing different missing values, several models --- it prints the whole",
            "range, \\code{100\\% (6 712-9 838)}, so an unequal base can never pass unnoticed.",
            "\\code{\"min\"} prints the smallest base only; \\code{\"no\"} prints no count.")),
  # NULL default on purpose: tab_dots_expand() refills an unsupplied argument, so a TRUE here
  #   would make every tab_counts() call look user-supplied and warn.
  add_n = list(
    default = NULL,
    producers = c("tab", "tab_counts"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` use `n` instead: `add_n = FALSE` is",
            "`n = \"no\"`.")),
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
  spread_vars = list(
    default = character(),
    producers = c("tab", "tab_counts"),
    doc = c("<\\link[tidyr:tidyr_tidy_select]{tidy-select}> The \\code{tab_vars} to show",
            "  ACROSS the page instead of down it: each of their levels becomes a block of columns,",
            "  and the table becomes as compact as it can be. A variable named here alone is added to",
            "  \\code{tab_vars} for you.",
            "  The layout follows: one \\code{Total} row for the whole table, the base count in one",
            "  \\code{n} column per block at the right (instead of a \\code{Total} column per block,",
            "  which would only repeat 100\\%), and --- since a total \\emph{line} cannot become a block",
            "  --- \\code{totaltab = \"line\"} becomes \\code{\"table\"}. Pair it with",
            "  \\code{comp = \"all\"} to compare every block against the overall total, and with",
            "  \\code{levels = \"first\"} to keep one column per block.")),
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
            "multi-tier store is threaded through; \\code{.defer_level_merge} keeps full factor",
            "levels through the aggregate and test so \\code{levels} becomes a display-time drop;",
            "\\code{.return_armed} returns the pre-\\code{finalize_color_spec} table so the tier-3",
            "cache can re-paint colours without a rebuild; \\code{.levels_order} is a named list",
            "of factor level orders applied post-aggregate, backing the jamovi level-reordering control (in R,",
            "relevel with \\code{\\link[forcats:fct_relevel]{forcats::fct_relevel}} before calling \\code{tab()});",
            "\\code{.levels_collapse} is its twin for MERGING levels -- a named list, one",
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
  # --- tab_reg()'s own arguments -------------------------------------------------------------------
  outcome = list(producers = "tab_reg", doc_in_producer = TRUE),
  predictors = list(producers = "tab_reg", default = NULL, doc_in_producer = TRUE),
  family = list(producers = "tab_reg", default = "auto", values_from = "REG_FAMILIES",
                doc_in_producer = TRUE),
  # the cascade: family -> link -> measure -> effect, each "auto" following from the left.
  link = list(producers = "tab_reg", default = "auto", values_from = "REG_FAMILIES",
              doc_in_producer = TRUE),
  measure = list(producers = "tab_reg", default = "auto", values_from = "REG_ESTIMANDS",
                 doc_in_producer = TRUE),
  effect = list(producers = "tab_reg", default = "auto", values_from = "REG_ESTIMANDS",
                doc_in_producer = TRUE),
  trials = list(producers = "tab_reg", default = NULL, doc_in_producer = TRUE),
  empirical = list(producers = "tab_reg", default = TRUE, size = 1L, validate = FALSE,
                   values = c("no", "tooltip", "cell", "column"), doc_in_producer = TRUE),
  outcome_level = list(producers = "tab_reg", default = NULL, values_from = "REG_FAMILIES",
                       doc_in_producer = TRUE),
  multiplier = list(producers = "tab_reg", default = "sd", doc_in_producer = TRUE),
  # `dots` on tab(): the whole vocabulary is one help page of its own (?shape_numeric_var), and a
  # crowded signature is the wrong place to teach it. tab_reg() keeps its own prose -- there `shape`
  # is about FITTING, and a quadratic is a model term tab() cannot take.
  shape = list(producers = c("tab", "tab_reg"), default = NULL, values_from = "VAR_SHAPES",
               dots = "tab", doc_in_producer = TRUE,
               doc_for = list(tab = c(
                 "How a \\strong{numeric} variable enters the table. Cut it into groups and it",
                 "  becomes an ordinary factor --- one row (or one column) per group:",
                 "  \\code{\"quartiles\"}, \\code{\"quintiles\"}, an integer number of groups,",
                 "  \\code{\"sd_bands\"} (four bands at the mean and one standard deviation either",
                 "  side) or \\code{\"levels\"} (one level per distinct value). \\code{\"log\"} /",
                 "  \\code{\"sqrt\"} keep it a number and are for \\code{col_vars} only.",
                 "  One value for every numeric variable, or one per variable:",
                 "  \\code{shape = c(age = \"quintiles\")}. A numeric \\code{row_vars} /",
                 "  \\code{tab_vars} defaults to \\code{\"auto\"} --- one level per value for a",
                 "  counted number or a short scale, \\code{\"sd_bands\"} for a continuous one ---",
                 "  and a numeric \\code{col_vars} keeps its means. See",
                 "  \\code{\\link{shape_numeric_var}} for the whole vocabulary."))),
  shape_name = list(producers = "tab", default = TRUE, dots = "tab",
                    doc = c("Whether a shaped variable writes its own name onto its",
                            "  \\strong{first} level (\\code{\"age: [18,30) low\"}), so a table whose",
                            "  leading text columns are stripped still says what the levels are levels",
                            "  of. \\code{TRUE} by default.")),
  stats = list(producers = "tab_reg", default = NULL, values_from = "TEST_ROWS",
               doc_in_producer = TRUE),
  output = list(
    producers = c("tab_build"), status = "internal",
    values = c("single", "list"), size = 1L,
    doc = "Internal. The shape tab_build() returns: one merged table, or a list.")
)

# --- EXPORT_ARGS: the RENDER surface ---------------------------------------------------------------
# The exporters' half of the argument surface, a separate table because `color` / `subtext` /
#   `stars` mean something ELSE on an exporter (a logical switch) than on a producer -- a shared
#   key cannot hold both rows.
# DESIGN -- the scope rule (narrower than TAB_ARGS'): a row exists only when shared by >= 2
#   exporters, or is the per-call twin of an option with no TAB_ARGS row of its own. Most rows
#   carry no `doc` here even though shared, because the ACCEPTED VALUES differ by backend
#   (`theme`'s "auto" exists only where a stylesheet ships) -- differing value sets are not a
#   duplicate, so the prose stays with `doc_in_producer = TRUE`.
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
  x = list(producers = c("tab_export", "forest_plot"), doc_with = "tabs"),

  # --- the shared render controls ---------------------------------------------------------------
  color = list(
    producers = c("tab_html", "tab_xl", "tab_plot", "tab_export"), option = NULL,
    doc = "Set to \\code{FALSE} to render the table without colours (monochrome).",
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
  ratio_cells = list(producers = "tab_xl",  option = "ratio_cells", doc_in_producer = TRUE),
  check       = list(producers = "tab_xl",  doc_in_producer = TRUE),
  data        = list(producers = "tab_xl",  doc_in_producer = TRUE),
  font_text = list(producers = "tab_xl", option = "font_text", doc_in_producer = TRUE),
  font_num  = list(producers = "tab_xl", option = "font_num",  doc_in_producer = TRUE),
  font_num_stars = list(producers = "tab_xl", option = "font_num_stars", doc_in_producer = TRUE)
)

#' @keywords internal
#' @noRd
EXPORT_PRODUCERS <- sort(unique(unlist(lapply(EXPORT_ARGS, `[[`, "producers"))))

#' @keywords internal
#' @noRd
arg_table_of <- function(producer)
  if (producer %in% EXPORT_PRODUCERS) EXPORT_ARGS else TAB_ARGS

# --- the derived vocabulary view ------------------------------------------------------------------
# TAB_ARG_VALUES is DERIVED, order preserved -- callers read it as a stable vocabulary list.
# ⚠ `ci` is deliberately excluded: two of its values are soft-deprecated, and validating them
#   centrally here would silently rewrite them instead of warning.
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

#' @keywords internal
#' @noRd
tab_args_for <- function(producer) {
  tb <- arg_table_of(producer)
  names(tb)[vapply(tb, function(r) producer %in% r[["producers"]], logical(1))]
}

#' @keywords internal
#' @noRd
tab_arg_status <- function(name, producer = NULL) {
  st <- (if (is.null(producer)) TAB_ARGS else arg_table_of(producer))[[name]][["status"]]
  if (is.null(st)) return("live")
  if (is.null(names(st))) return(st[[1]])
  if (!is.null(producer) && producer %in% names(st)) st[[producer]] else "live"
}

# --- `...` on the superseded producers ------------------------------------------------------------
# tab_check_dots() validates `...`: an unnamed argument is refused by name rather than silently
#   bound to a formal's position, and a typo gets a suggestion. It validates the crosstab producers
#   AND tab_reg(), but not the exporters -- a backend's `...` is a pass-through.
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
  # A dot-prefixed name is internal plumbing (the jamovi live-cache / level-merge fields), never a
  #   user argument, so it is never flagged as unknown.
  bad   <- setdiff(nms, known)
  bad   <- bad[!startsWith(bad, ".")]
  if (!length(bad)) return(invisible(TRUE))
  # R does NOT partial-match an argument written after `...`: an abbreviation lands here instead,
  #   indistinguishable from a typo -- hence the "did you mean" suggestion below.
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

#' @keywords internal
#' @noRd
dots_value <- function(dots, name, default = NULL) {
  if (!name %in% names(dots)) return(default)
  rlang::eval_tidy(dots[[name]])
}

# --- the generated documentation ------------------------------------------------------------------

# tab_dots_rd() -- the `@param ...` twin of tab_args_rd(): what a producer accepts BY NAME without
# declaring a formal, read off the same declaration instead of a hand-kept list that drifts. Two
# groups, and the difference is the whole point: an argument still current but kept out of a crowded
# signature (`dots = <producer>`), and one retired in 2.0.0 (declared, not a formal, no `dots`).
# ⚠ Only for a producer whose signature declares MOST of its arguments -- tab_counts() takes
# everything through `...`, so the second group would list its whole surface.
#' @keywords internal
#' @noRd
tab_dots_rd <- function(producer, extra = NULL) {
  tb   <- arg_table_of(producer)
  nms  <- setdiff(tab_args_for(producer), names(formals(get(producer, envir = asNamespace("tabxplor")))))
  live <- nms[vapply(nms, function(k) producer %in% (tb[[k]][["dots"]] %||% character()), logical(1))]
  out  <- c("@param ... Arguments taken by name, and kept out of the signature. An UNNAMED argument",
            "  here is refused outright --- past the variable roles, every argument must be named ---",
            "  and an unknown name is refused with a suggestion.")
  for (k in live) {
    body <- tb[[k]][["doc_for"]][[producer]] %||% tb[[k]][["doc"]]
    if (is.null(body)) next
    out <- c(out, paste0("  \\strong{\\code{", k, "}} ", body[[1]]), paste0("  ", body[-1]))
  }
  dead <- setdiff(nms, live)
  # a dot-prefixed name is never a user argument -- the jamovi live-cache plumbing tab_check_dots()
  # lets through. Named, not documented as a choice.
  hidden <- dead[startsWith(dead, ".")]
  dead   <- setdiff(dead, hidden)
  if (length(dead))
    out <- c(out, paste0("  \\strong{Retired in 2.0.0}, still taken by name, each warning once and saying",
                         " what to use instead: \\code{", paste(dead, collapse = "}, \\code{"), "}."))
  if (length(hidden))
    out <- c(out, paste0("  The dot-prefixed names (\\code{", paste(hidden, collapse = "}, \\code{"),
                         "}) are internal plumbing, not user arguments."))
  c(out, extra)
}

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
    # The tag head must be a formal OF THIS PRODUCER (a leaf's doc owner can differ from its own
    #   formal name) -- emitting the owner blindly would document an argument the function lacks.
    tag <- nms[vapply(nms, owner, character(1)) == o]
    tag <- c(intersect(o, tag), setdiff(tag, o))
    r   <- tb[[o]]
    body <- r[["doc_for"]][[producer]] %||% r[["doc"]]
    if (is.null(body)) next
    if (!is.null(r[["values_rd"]])) {
      vals <- do.call(r[["values_rd"]], list(producer = producer))
      at   <- which(body == "{VALUES}")
      body <- if (length(at)) append(body[-at], vals, after = at[[1]] - 1L) else c(body, vals)
    }
    out <- c(out, paste0("@param ", paste(tag, collapse = ","), " ", body[[1]]), body[-1])
  }
  out
}

# --- the value-list renderers ---------------------------------------------------------------------
#' @keywords internal
#' @noRd
color_measures_rd <- function(producer = "tab") {
  # `producer` is the values_rd calling convention; only the crosstab producers reach this today
  # (?tab_reg's @param color is hand-written, and richer than MEASURES$doc).
  who  <- if (identical(producer, "tab_reg")) "reg" else "tab"
  keys <- measure_nameable(who, channel = "text")
  quo  <- function(v) paste0("\\code{\"", v, "\"}")
  c(" \\itemize{",
    vapply(keys, function(k) {
      # each measure followed by the acronyms that reach it, read off the ONE shared table -- so this
      # list and the argument accept the same words by construction.
      a <- measure_spellings(k)
      paste0("  \\item ", quo(k),
             if (length(a)) paste0(" (", paste(vapply(a, quo, character(1)), collapse = ", "), ")"),
             ": ", MEASURES[[k]][["doc"]])
    }, character(1)),
    " }")
}

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

# The first value is the default (the CI_METHODS convention); `producer` is unread here and kept
#   anyway -- it is the `values_rd` calling convention (every renderer takes it), so the formal is
#   the interface, not dead weight.
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
stopifnot(
  all(vapply(TAB_ARGS, function(r) !is.null(r[["producers"]]) && is.character(r[["producers"]]), logical(1))),
  all(vapply(TAB_ARGS, function(r) !is.null(r[["doc"]]) || !is.null(r[["doc_with"]]) ||
               isTRUE(r[["doc_in_producer"]]), logical(1))),
  all(vapply(TAB_ARGS, function(r)
    is.null(r[["doc_with"]]) || r[["doc_with"]] %in% names(TAB_ARGS), logical(1))),
  all(vapply(TAB_ARGS, function(r) is.null(r[["status"]]) ||
               all(r[["status"]] %in% c("live", "deprecated", "superseded", "internal")), logical(1))),
  all(vapply(TAB_ARGS, function(r) is.null(r[["check"]]) ||
               r[["check"]] %in% c("probability", "count"), logical(1))),
  setequal(names(TAB_ARG_VALUES),
           c("pct", "na", "levels", "comp", "tot", "totaltab", "totcol", "output", "anova", "n"))
)

stopifnot(
  all(vapply(EXPORT_ARGS, function(r)
    !is.null(r[["producers"]]) && is.character(r[["producers"]]), logical(1))),
  all(vapply(EXPORT_ARGS, function(r) !is.null(r[["doc"]]) || !is.null(r[["doc_with"]]) ||
               isTRUE(r[["doc_in_producer"]]), logical(1))),
  all(vapply(EXPORT_ARGS, function(r)
    is.null(r[["doc_with"]]) || r[["doc_with"]] %in% names(EXPORT_ARGS), logical(1))),
  all(vapply(EXPORT_ARGS, function(r) is.null(r[["values"]]), logical(1))),
  # the two surfaces may share a NAME (`color` / `subtext` / `stars`) but must not share a producer:
  # one function's arguments are declared in exactly one table, or arg_table_of() would be a coin toss.
  length(intersect(
    EXPORT_PRODUCERS,
    unique(unlist(lapply(TAB_ARGS, `[[`, "producers"))))) == 0L
)

# tab_dots_expand() -- a superseded producer's `...` becomes its declared arguments, filled from
#   its DECLARED default. The leaves' defaults diverge from tab()'s for several shared formals
#   (tab_num() alone starts `color` at `"auto"`, `ref` at `"tot"`) -- `default_for` records the
#   divergence so it is not silently lost when the formal moves into `...`.
#' @keywords internal
#' @noRd
tab_dots_expand <- function(dots, producer) {
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
