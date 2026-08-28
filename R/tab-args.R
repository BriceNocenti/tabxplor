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
# DESIGN: `doc` IS the roxygen text -- and it is a REFERENCE entry, not a lesson: what the argument
#   is, its values, at most one sentence on when to change it. Teaching goes in the vignettes, which
#   the prose links to once per page. Rendered by tab_args_rd(), which orders by formals() and
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

# COLUMNS. `producers` is the only one every row carries; everything else states what is unusual
# about the argument. Every row writes them in TAB_ARG_ORDER -- short facts first, `doc` last -- so
# the 81 rows read down a column; asserted at load.
#   producers   the producers this is a FORMAL of. tab_args_rd() orders by formals() and asserts the
#               two sets match, so a signature and this table cannot drift.
#   dots        the producers that take it through `...` instead of as a formal.
#   status      "deprecated" / "superseded". ⚠ may be a NAMED vector, one entry per producer -- read
#               it with tab_arg_status(name, producer), never with `$status`.
#   default     the formal's default; `default_for` overrides it per producer (a named list).
#   option      the tabxplor.* option that supplies the default when the formal is NULL (a
#               TAB_OPTIONS key, checked at load).
#   values      the argument's OWN value set, for an argument no fact table gives a vocabulary to.
#   values_from the fact table that owns the vocabulary instead (checked at load).
#   values_rd   the *_rd() generator that renders the value list into the help page.
#   validate    FALSE = declared for the documentation only, never checked at the boundary.
#   leaf        the narrower value set the LEAF producers accept, where the full one is wider.
#   size        the exact length the value must have (absent = any).
#   na_ok       NA is a legal value.
#   check       the numeric contract the value must meet: "probability" | "count".
#   stored      `pct` only: the PCT_TYPES key stamped when the argument is off.
#   doc         the roxygen @param body, as Rd lines. `doc_for` overrides it per producer;
#               `doc_with` says another row's block documents this one; `doc_in_producer = TRUE`
#               that the prose stays in the producer's own roxygen.
# The one order every row of both tables writes its fields in, asserted below.
#' @keywords internal
#' @noRd
TAB_ARG_ORDER <- c("producers", "dots", "status", "default", "default_for", "option",
                   "values", "values_from", "values_rd", "validate", "leaf", "size", "na_ok",
                   "check", "stored",
                   "doc", "doc_for", "doc_with", "doc_in_producer")

#' @keywords internal
#' @noRd
TAB_ARGS <- list(
  data = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_many", "tab_reg"),
    doc = "A data frame."),
  row_vars = list(
    producers = c("tab", "tab_many"),
    doc = c("<\\link[tidyr:tidyr_tidy_select]{tidy-select}> The row variable(s), printed with one",
            " level per line, and the column variable(s), one level per column. A numeric variable",
            " gives a single column of means. Each accepts one variable or several,",
            " \\code{c(var1, var2)}.")),
  col_vars = list(
    producers = c("tab", "tab_num", "tab_many"),
    doc = c("<\\link[tidyr:tidyr_tidy_select]{tidy-select}> The column variable(s) --- see",
            " \\code{row_vars}. \\strong{An interaction} is written \\code{a*b}, as in",
            " \\code{\\link{tab_reg}()}, and only \\code{col_vars} takes one: two factors give one",
            " column per observed cell of the pair, a number crossed with a factor one mean column",
            " per level. See \\code{vignette(\"tabxplor\")}.")),
  row_var = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"), status = c(tab = "deprecated"),
    doc = c("`r lifecycle::badge(\"deprecated\")` Singular aliases of",
            " \\code{row_vars}/\\code{col_vars} (which now accept several variables). Kept working.")),
  col_var = list(producers = c("tab", "tab_plain", "tab_counts"), status = c(tab = "deprecated"), doc_with = "row_var"),
  tab_vars = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_many", "tab_reg"),
    default_for = list(tab_reg = NULL),
    doc = c("<\\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables: one subtable per",
            "combination of their levels. Leave empty for a simple cross-table.")),
  wt = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_many", "tab_reg"),   # tab_counts says `wt_counts`
    default_for = list(tab_reg = NULL),
    doc = "A weight variable, of class numeric. Leave empty for unweighted results."),
  sup_cols = list(
    producers = c("tab"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` Supplementary columns variables, with",
            "only the first level printed. Deprecated in 2.0.0: pass these columns in \\code{col_vars} and",
            "set \\code{levels = \"first\"} instead (\\code{col_vars} already accepts several variables).")),
  na = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), default = "keep",
    default_for = list(tab_reg = c("drop_by_outcome", "drop_by_model", "drop_all", "keep_for_predictors"), tab_num = c("keep", "drop")),
    values = c("keep", "drop", "drop_all", "common_base"), leaf = c("keep", "drop"), size = 1L,
    doc = c("The policy to adopt for missing values, as a single string :",
            " \\itemize{",
            "  \\item \\code{\"keep\"} (default): every `NA` becomes an explicit `\"NA\"` level.",
            "  \\item \\code{\"drop\"}: each column is computed on its own non-missing observations,",
            "  so bases can differ between \\code{col_vars}.",
            "  \\item \\code{\"drop_all\"}: drop every observation missing on any variable, so all",
            "  columns share one base.",
            "  \\item \\code{\"common_base\"}: one population --- non-missing on the \\code{row_vars}",
            "  and the \\strong{first} \\code{col_vars} --- while the other \\code{col_vars} keep",
            "  their own `NA`'s as a level within it. Microdata only.",
            "  }")),
  levels = list(
    producers = c("tab"), default = "all", values = c("all", "first", "auto"),
    doc = c("The levels of \\code{col_vars} to keep, as a single string or a vector the same",
            "length as \\code{col_vars}: \\code{\"all\"} (default), \\code{\"first\"} (only the first",
            "level of each --- a compact summary of many items), or \\code{\"auto\"} (the first level",
            "of a two-level variable, all of them otherwise). For finer selections use",
            "\\code{\\link[dplyr:select]{dplyr::select}} on the finished table.")),
  digits = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    default = 0,
    doc = c("The number of digits to print, as a single integer, or an integer vector the",
            "same length as \\code{col_vars}."),
    doc_for = list(tab_reg = c(
      "The number of digits to print, as a single integer --- a \\strong{minimum}: each measure",
      "  keeps its own precision where that is finer (an odds ratio reads at two decimals, a mean",
      "  score at one). Name a display field to set just that one, including an aside:",
      "  \\code{digits = c(ratio = 3)}, \\code{digits = c(base = 2)}, \\code{digits = c(1, or = 3)}."))),
  n_min = list(
    producers = c("tab", "tab_counts"), default = 0, check = "count",
    doc = c("A single positive integer (default \\code{0}, off). A pure display filter applied",
            "last: it hides cells resting on too few people, without recomputing anything. Totals and",
            "the p-value line are always kept.")),
  display = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), default = NULL,
    values_from = "DISPLAY_TOKENS",
    doc = c("What each value cell shows (text output only -- the console, \\code{\\link{tab_kable}}",
            "  and \\code{\\link{tab_md}}; Excel falls back to the primary field). \\code{NULL} (default)",
            "  keeps each cell's plain value. Three ways to ask: a \\strong{named layout}",
            "  (\\code{\"est_ci\"}, \\code{\"base_ratio\"}), a \\strong{single field} (\\code{\"ci\"},",
            "  \\code{\"diff\"}), or a \\strong{\\{\\} template} of your own",
            "  (\\code{\"\\{pct\\} (n=\\{n\\})\"}). The whole vocabulary is in \\link{tabxplor-display};",
            "  \\code{\\link{set_display}} changes it on a table already built.",
            "  A layout showing an interval prints the one the table computed, so pair it with a",
            "  \\code{ci = } value or a \\code{color} that needs one.")),
  totaltab = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"), default = "line",
    values = c("line", "table", "no", ""), size = 1L,
    doc = c("The total table, when \\code{tab_vars} makes subtables: \\code{\"line\"} (default, a",
            "general total line), \\code{\"table\"} (a complete total table --- \\code{row_vars} by",
            "\\code{col_vars}, without \\code{tab_vars}) or \\code{\"no\"}.")),
  totaltab_name = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"),
    default = "Ensemble",
    doc = "The name of the total table, as a single string."),
  # `dots`: still current, but out of tab()'s signature -- a table always HAS both totals, so this
  # only says which ones to show, and a crowded signature is the wrong place to ask that.
  tot = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"), dots = "tab",
    default = c("row", "col"), default_for = list(tab_plain = NULL, tab_num = NULL),
    values = c("row", "col", "both", "no", ""),
    doc = c("Which totals to show: \\code{c(\"row\", \"col\")} or \\code{\"both\"} (default),",
            "\\code{\"row\"}, \\code{\"col\"}, or \\code{\"no\"} (removed after the calculations that",
            "need them).")),
  total_names = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"),
    default = "Total",
    doc = c("The names of the totals, as a character vector of length one or two.",
            "Use syntax of type \\code{c(\"Total row\", \"Total column\")} to set different names for",
            "rows and cols.")),
  pct = list(
    producers = c("tab", "tab_plain", "tab_counts"), default = "no",
    values = c("no", "row", "col", "all", "all_tabs"), na_ok = TRUE, stored = "none",
    doc = c("The percentages to calculate, as a single string or a vector the same length as",
            "\\code{col_vars}: \\code{\"row\"}, \\code{\"col\"}, \\code{\"all\"} (frequencies within",
            "each subtable), \\code{\"all_tabs\"} (frequencies over every table) or \\code{\"no\"}",
            "(default, counts). Everything else --- the reference, the interval, the colour ---",
            "follows from this choice.")),
  ref = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    default = "auto", default_for = list(tab_reg = NULL, tab_num = "tot"),
    doc = c("The reference cell that differences and ratios are computed against:",
            " \\itemize{",
            "  \\item \\code{\"auto\"} (default): the corresponding total for a difference, the first",
            "  row (or column) for an odds ratio. \\code{\"tot\"}: always the total.",
            "  \\item \\code{\"first\"} / \\code{\"last\"}: the first or last \\strong{level} --- useful",
            "  to color a temporal development. A total is not a level and is never selected.",
            "  \\item an \\strong{integer}, the nth row (or column); a \\strong{string}, a regular",
            "  expression matched against the row (or column) names. \\code{\"no\"}: no reference.",
            "}",
            "One reference per \\code{row_vars} with a named vector,",
            "\\code{ref = c(race = \"first\")}; an unnamed one goes by position.")),
  ref2 = list(
    producers = c("tab", "tab_plain", "tab_counts"),
    default = "first",
    doc = c("The second reference level for odds ratios, needed only for a factor with",
            "**3 levels or more** (the \"OR of each level versus \\code{ref2}\"); the first level by",
            "default. Ignored for a **binary** factor, where each level's OR is taken against the",
            "other. Same values as \\code{ref}.")),
  comp = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"), default = "tab",
    default_for = list(tab_num = c("tab", "all")), values = c("tab", "all", ""), size = 1L, na_ok = TRUE,
    doc = c("What each cell is compared with: \\code{\"tab\"} (default) compares it inside its own",
            "\\code{tab_vars} subtable, \\code{\"all\"} against the total table's own reference line.")),
  OR = list(
    producers = c("tab", "tab_plain", "tab_counts"), status = "deprecated",
    default = "no",
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
    producers = c("tab", "tab_counts"),
    default = FALSE,
    doc = c("Set to \\code{TRUE} to test each (sub)table for independence: \\strong{Chi-squared}",
            "for factor \\code{col_vars}, \\strong{Welch's F} for numeric ones, with an effect size",
            "beside it. Needed by \\code{color = \"contrib\"}, and added automatically for it. The",
            "footer names the test you actually got --- see \\code{vignette(\"tabxplor-weights\")}.")),
  anova = list(
    producers = c("tab", "tab_num"), default = NULL, option = "anova", values = c("welch", "classic"), size = 1L,
    doc = c("Which one-way ANOVA \\strong{F} the p-value line shows for \\emph{numeric}",
            "\\code{col_vars}: \\code{\"welch\"} (does not assume equal variances) or \\code{\"classic\"}",
            "(the pooled F). \\code{NULL} (default) reads \\code{options(tabxplor.anova)} ---",
            "\\code{\"welch\"}. Both are always computed, so this only chooses which row is shown.")),
  chi2 = list(
    producers = c("tab", "tab_counts", "tab_many"), status = "deprecated",
    default = lifecycle::deprecated(),
    doc = c("`r lifecycle::badge(\"deprecated\")` Renamed to \\code{test} in 2.0.0: the test is a",
            "Chi-squared only for factors (numeric \\code{col_vars} get Welch's F), so the old name was",
            "misleading. Still works.")),
  ci = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts"),
    default = "auto",
    values = c("auto", "no", "cell", "ref"), validate = FALSE,
    doc = c("**What the confidence interval is anchored on**. Its \\emph{geometry} is not asked",
            " here: it follows the comparison the table makes, so an odds-ratio table gets an",
            " odds-ratio interval.",
            "  \\itemize{",
            "   \\item \\code{\"auto\"} (default): on the comparison where the table makes one, on the",
            "     cell for plain frequencies, none where nothing needs one.",
            "   \\item \\code{\"ref\"}: on the cell's deviation from its reference. \\code{\"cell\"}: on",
            "     the cell's own percentage or mean. \\code{\"no\"}: none.",
            "  }",
            " \\code{\"cell\"} and \\code{\"no\"} anchor nothing to compare, so \\code{stars} and",
            " \\code{color_signif} are disabled (with a message). The method is chosen with",
            " \\code{ci_method} and named in the table's legend.")),
  conf_level = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    default = NULL,   # NULL everywhere; each producer's own boundary resolves it at call time.
    option = "conf_level", check = "probability",
    doc = c("The confidence level, as a single numeric between 0 and 1. \\code{NULL} (default)",
            "reads \\code{options(tabxplor.conf_level)} --- 0.95.")),
  stars = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    default = NULL, default_for = list(tab_reg = TRUE), option = "stars",
    doc = c("Logical. With \\code{ci = \"ref\"}, print significance stars for each cell's difference",
            "from its reference, read from the displayed interval itself. \\code{NULL} (default) reads",
            "\\code{options(tabxplor.stars)} --- \\code{FALSE}.")),
  ci_method = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    # on tab_reg it rides `...`: one binary choice (wald / profile) does not earn a place in a
    # signature a user reads to learn the producer.
    dots = "tab_reg",
    default = NULL, default_for = list(tab_reg = NULL), values_from = "CI_METHODS",
    doc = c("The interval method, one kind at a time, as ONE named vector -- partial, like",
            "\\code{ref} or \\code{pct}, so an unnamed kind keeps its default. Example:",
            "\\code{ci_method = c(cell = \"beta\", diff = \"ac\")}.",
            "\\itemize{",
            "  \\item \\code{cell}, a proportion's own interval: \\code{\"wilson\"} (default),",
            "    \\code{\"wald\"}, \\code{\"beta\"}.",
            "  \\item \\code{diff}, a proportion minus its reference: \\code{\"newcombe\"} (default),",
            "    \\code{\"ac\"}, \\code{\"wald\"}.",
            "  \\item \\code{mean_diff}: \\code{\"welch\"} (default), \\code{\"student\"}, \\code{\"ols\"}.",
            "  \\item \\code{mean_ratio}: \\code{\"robust\"} (default), \\code{\"quasipoisson\"},",
            "    \\code{\"poisson\"}.",
            "}",
            "A proportion \\emph{ratio} has only one method (Katz), so it is not a choice.")),
  design_effect = list(
    producers = c("tab", "tab_plain", "tab_num"), default = NULL, option = "design_effect",
    doc = c("Whether the intervals, stars and colour thresholds of a \\strong{weighted} table",
            "account for the weighting's own design effect instead of using the raw sample size.",
            "\\code{NULL} (default) reads \\code{options(tabxplor.design_effect)} --- \\code{FALSE}.",
            "Ignored without \\code{wt}. See \\code{vignette(\"tabxplor-weights\")}.")),
  method_cell = list(
    producers = c("tab"), status = "deprecated",
    doc = c("`r lifecycle::badge(\"deprecated\")` Use",
            "\\code{ci_method = c(cell = , diff = )} instead.")),
  method_diff = list(producers = c("tab"), status = "deprecated", doc_with = "method_cell"),
  color = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"),
    default = "no", default_for = list(tab_reg = "measure", tab_num = "auto"),
    values_from = "MEASURES", values_rd = "color_measures_rd",
    doc = c("Which \\strong{measure of deviation} to color --- a deviation being how far a cell",
            "sits from its reference, the measure which of the ways of expressing it you read.",
            "\\code{\"no\"} (default, \\code{FALSE} equivalently) prints no color; \\code{TRUE} picks",
            "one per column type. Otherwise:",
            "{VALUES}",
            "The acronyms in brackets are permanent aliases, the same words \\code{\\link{tab_reg}}'s",
            "\\code{measure} takes. An acronym here always names a \\strong{measure}, where",
            "\\code{display =} names a \\emph{field} and \\code{ref2 =} a \\emph{level}.",
            "\\strong{Position picks the channel} (1st value -> text, 2nd -> background) and",
            "\\strong{names pick the column type}: \\code{c(\"difference\", \"ratio\")},",
            "\\code{c(pct = \"difference\", mean = \"ratio\")}, or both with a \\code{list()}. Only",
            "\\code{difference} / \\code{ratio} may go on the background; thresholds come from",
            "\\code{\\link{set_color_breaks}}.")),
  color_signif = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), default = "ignore",
    default_for = list(tab_reg = NULL), values_from = "COLOR_SIGNIF_VALUES", values_rd = "color_signif_rd",
    doc = c("How significance gates the color, as a single string:",
            "{VALUES}",
            "With \\code{color = \"contrib\"}, which has no interval to floor, the first two color",
            "the \\strong{relative} contribution and \\code{\"guaranteed_effect\"} the \\strong{adjusted",
            "standardized residual}. See \\code{vignette(\"tabxplor\")}.")),
  color_breaks = list(
    producers = c("tab", "tab_num", "tab_counts"), default = NULL, option = "color_breaks",
    values_from = "COLOR_SCALES",
    doc = c("A per-table override of the colour thresholds, in the form",
            "\\code{\\link{set_color_breaks}} accepts; unset scales keep the global ones.")),
  n = list(
    producers = c("tab", "tab_counts", "tab_reg"), default = NULL, option = "n", values = c("range", "min", "no"),
    size = 1L,
    doc = c("How many people this table is about. \\code{NULL} (default) reads",
            "\\code{options(tabxplor.n)} --- \\code{\"range\"}, which prints the unweighted base beside",
            "the \\code{Total} cell, \\code{100\\% (9 838)}, or the whole range where the columns do",
            "not rest on the same people, \\code{100\\% (6 712-9 838)}, so an unequal base cannot",
            "pass unnoticed. \\code{\"min\"} prints the smallest base only; \\code{\"no\"} none.")),
  # NULL default on purpose: tab_dots_expand() refills an unsupplied argument, so a TRUE here
  #   would make every tab_counts() call look user-supplied and warn.
  add_n = list(
    producers = c("tab", "tab_counts"), status = "deprecated",
    default = NULL,
    doc = c("`r lifecycle::badge(\"deprecated\")` use `n` instead: `add_n = FALSE` is",
            "`n = \"no\"`.")),
  add_pct = list(
    producers = c("tab", "tab_counts"),
    default = FALSE,
    doc = c("Set to `TRUE` to add a column with the frequencies of the row",
            "variable (for `pct = \"row\"`) or a row with the frequencies of the column variable",
            "(for  `pct = \"col\"`).")),
  common_totrow = list(
    producers = c("tab", "tab_counts"),
    default = FALSE,
    doc = c("With several \\code{row_vars}, `FALSE` (default) shows one Total row per row",
            "variable; `TRUE` collapses the identical ones into a single shared Total. Genuinely",
            "different totals (which only `na = \"drop\"` can produce) are never merged.")),
  subtext = list(
    producers = c("tab", "tab_plain", "tab_num", "tab_counts", "tab_reg"), default = "",
    default_for = list(tab_reg = ""), doc = "A character vector to print rows of legend under the table."),
  output_list = list(
    producers = c("tab"),
    default = FALSE,
    doc = c("Logical (default \\code{FALSE}). With several \\code{row_var}, \\code{FALSE}",
            " merges the mirror tables into a single \\code{tabxplor_tab}; \\code{TRUE} returns a list with",
            " one table per \\code{row_var}. With \\code{tab_vars}, tables stay a list regardless.")),
  spread_vars = list(
    producers = c("tab", "tab_counts"),
    default = character(),
    doc = c("<\\link[tidyr:tidyr_tidy_select]{tidy-select}> The \\code{tab_vars} to show",
            "  ACROSS the page instead of down it: each of their levels becomes a block of columns,",
            "  and the table becomes as compact as it can be. A variable named here alone is added to",
            "  \\code{tab_vars} for you. Pair it with \\code{comp = \"all\"} to compare every block",
            "  against the overall total, and with \\code{levels = \"first\"} to keep one column per",
            "  block. Because the columns are multiplied, a cell layout you did not ask for narrows",
            "  to its bare estimate --- a numeric column shows its mean alone, without the",
            "  coefficient of variation; name a layout with \\code{display =} to keep one.")),
  names_prefix = list(
    producers = c("tab", "tab_counts"), status = "deprecated",
    default = NULL,
    doc = c("`r lifecycle::badge(\"deprecated\")` These belong to",
            " \\code{\\link{tab_spread}}, which is the function that names the new columns; they reach it only",
            " when \\code{spread_vars} is given. Call \\code{tab_spread()} yourself for control over the names.")),
  names_sort = list(producers = c("tab", "tab_counts"), status = "deprecated", default = FALSE,
                    doc_with = "names_prefix"),
  cleannames = list(
    producers = c("tab", "tab_counts", "tab_reg"), default = NULL, default_for = list(tab_reg = NULL),
    option = "cleannames",
    doc = c("Set to \\code{TRUE} to clean level names, by removing prefix numbers like \"1-\" and",
            "text in parentheses. \\code{NULL} (default) reads",
            "\\code{options(tabxplor.cleannames)} --- \\code{FALSE}.")),
  other_if_less_than = list(
    producers = c("tab"),
    default = 0,
    doc = c("When set to a positive integer, levels with less count",
            "than it will be merged into an \"Others\" level.")),
  other_level = list(
    producers = c("tab"),
    doc = "The name of the \"Other\" level, as a single string."),
  filter = list(
    producers = c("tab", "tab_many"), status = "superseded",
    doc = c("`r lifecycle::badge(\"superseded\")` A",
            "\\code{\\link[dplyr:filter]{dplyr::filter}} to apply to the data first, as a single string.",
            "Prefer filtering upstream of \\code{tab()}.")),
  .cache = list(
    producers = c("tab"), status = "internal",
    doc = c("Internal, for the jamovi",
            "\\code{jmvtab} live cache only: \\code{.cache} is a mutable environment the content-addressed",
            "multi-tier store is threaded through; \\code{.defer_level_merge} keeps full factor",
            "levels through the aggregate and test so \\code{levels} becomes a display-time drop;",
            "\\code{.return_armed} returns the pre-\\code{finalize_color_spec} table so the tier-3",
            "cache can re-paint colours without a rebuild; \\code{.levels_order} is a named list",
            "of factor level orders backing the jamovi level-reordering control -- applied post-aggregate in",
            "\\code{tab()}, and to the ROW SKELETON in \\code{tab_reg()}, where a predictor's order is",
            "display and only \\code{ref} reaches the fit (in R, relevel with",
            "\\code{\\link[forcats:fct_relevel]{forcats::fct_relevel}} before calling \\code{tab()});",
            "\\code{.levels_collapse} is its twin for MERGING levels -- a named list, one",
            "element per variable, of merged label -> the levels it swallows -- applied pre-aggregate, so it",
            "is exactly \\code{\\link[forcats:fct_collapse]{forcats::fct_collapse}} on the data before",
            "\\code{tab()}, which is how to do it in R.",
            "All default off; not for direct use.")),
  .defer_level_merge = list(producers = c("tab"), status = "internal", doc_with = ".cache"),
  .return_armed = list(producers = c("tab"), status = "internal", doc_with = ".cache"),
  .levels_order = list(producers = c("tab", "tab_reg"), status = "internal", doc_with = ".cache"),
  .levels_collapse = list(producers = c("tab", "tab_reg"), status = "internal", doc_with = ".cache"),
  num = list(
    producers = c("tab_plain", "tab_num"),
    default = FALSE,
    doc = "Set to \\code{TRUE} to obtain a table with normal numeric vectors (not fmt)."),
  df = list(
    producers = c("tab_plain", "tab_num"),
    default = FALSE,
    doc = c(" Set to \\code{TRUE} to obtain a plain data.frame (not a tibble),",
            "with normal numeric vectors (not fmt). Useful, for example, to pass the table to",
            "correspondence analysis with \\pkg{FactoMineR}.")),
  .fine = list(
    producers = c("tab_plain", "tab_num"), status = "internal",
    default = NULL,
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
    producers = c("tab_counts"),
    default = "variable",
    doc = "Name of the (synthesised) column variable when `cols` is used."),
  base = list(
    producers = c("tab_counts"),
    doc = "For `input = \"pct\"`: the column holding each row's sample size N."),
  input = list(
    producers = c("tab_counts"), default = c("counts", "pct"), values = c("counts", "pct"), validate = FALSE,
    size = 1L,
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
  empirical = list(
    producers = "tab_reg", default = TRUE, values = c("no", "tooltip", "cell", "column"), validate = FALSE,
    size = 1L, doc_in_producer = TRUE),
  outcome_level = list(producers = "tab_reg", default = NULL, values_from = "REG_FAMILIES",
                       doc_in_producer = TRUE),
  multiplier = list(producers = "tab_reg", default = "2sd", doc_in_producer = TRUE),
  # `dots` on tab(): the whole vocabulary is one help page of its own (?shape_numeric_var), and a
  # crowded signature is the wrong place to teach it. tab_reg() keeps its own prose -- there `shape`
  # is about FITTING, and a quadratic is a model term tab() cannot take.
  shape = list(producers = c("tab", "tab_reg"), dots = "tab", default = NULL,
               values_from = "VAR_SHAPES",
               doc_for = list(tab = c(
                 "How a \\strong{numeric} variable enters the table. Cut it into groups and it",
                 "  becomes an ordinary factor --- one row (or column) per group. One value for",
                 "  every numeric variable, or one per variable:",
                 "  \\code{shape = c(age = \"quintiles\")}. On the row and tab axes a number always",
                 "  gets one, \\code{\"auto\"} by default; a numeric \\code{col_vars} keeps its means.",
                 "  \\code{\\link{shape_numeric_var}} lists the whole vocabulary.")),
               doc_in_producer = TRUE),
  shape_name = list(
    producers = "tab", dots = "tab", default = FALSE,
    doc = c("Whether a shaped variable writes its own name onto its",
                            "  \\strong{first} level (\\code{\"age: [18,30) low\"}), so a table whose",
                            "  leading text columns are stripped still says what the levels are levels",
                            "  of. \\code{FALSE} by default.")),
  stats = list(producers = "tab_reg", default = "auto", values_from = "TEST_ROWS",
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
# COLUMNS: the same as TAB_ARGS' -- `producers`, `option`, `doc` / `doc_for` / `doc_with` /
#   `doc_in_producer` -- minus every validation one: an exporter's value set is its backend's, so no
#   row here carries `values` (asserted at load).
#' @keywords internal
#' @noRd
EXPORT_ARGS <- list(
  # --- the table itself -------------------------------------------------------------------------
  tabs = list(
    producers = c("tab_html", "tab_md", "tab_xl"),
    doc = c("A table made with \\code{\\link{tab}} or \\code{\\link{tab_reg}}, or a `list` of tab.",
            "A list of tables sharing the same `col_vars` (and no `tab_vars`) is merged into one; any",
            "other list --- several `row_vars` and/or `tab_vars` --- is rendered one table after",
            "another, each keeping its own sub-tables.")),
  x = list(producers = c("tab_export", "forest_plot"), doc_with = "tabs"),

  # --- the shared render controls ---------------------------------------------------------------
  color = list(
    producers = c("tab_html", "tab_xl", "tab_export"), option = NULL,
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
    producers = c("tab_html", "tab_md", "tab_xl", "tab_export"),
    doc = c("Print the colour legend below the table (with the subtext). `TRUE` by default, and a",
            "no-op on a table that carries no colours.")),
  lang = list(
    producers = c("tab_html", "tab_md", "tab_xl", "tab_export", "forest_plot"),
    option = "lang",
    doc = c("Colour-legend language: \\code{NULL} (auto from the R/OS locale, English fallback),",
            "\\code{\"en\"} or \\code{\"fr\"}.")),
  transpose = list(
    producers = c("tab_html", "tab_md", "tab_xl", "tab_export"),
    doc = c("Set to \\code{TRUE} to transpose each table before export (rows become columns) --",
            "the col-percentages-with-several-row-variables use case.")),
  var_names = list(
    producers = c("tab_html", "tab_md", "tab_xl", "tab_export"), option = "var_names",
    doc = c("Which variable names to write beside the table: `\"both\"` (the default), `\"rows\"`,",
            "`\"cols\"` or `\"none\"`. The row-variable name is the leading column a table with several",
            "`row_vars` uses to name each block (written once per block); the column-variable names",
            "are the spanning row above their level columns. Level headers always keep their name.")),
  wrap_rows = list(
    producers = c("tab_html", "tab_md", "tab_xl"),
    doc = "By default, rownames are wrapped when larger than 30 characters.",
    doc_for = list(
      tab_md = c("Max width for row labels before truncation. `NULL` (default) never truncates",
                 "(lossless -- the column grows); set a number to cap the label width. A markdown pipe",
                 "cell cannot hold a raw newline, so md \"wrapping\" means \"do not truncate\"."))),
  wrap_cols = list(
    producers = c("tab_html", "tab_xl"),
    doc = "By default, colnames are wrapped when larger than 12 characters."),
  whitespace_only = list(
    producers = c("tab_html"),
    doc = "Set to `FALSE` to wrap also on non whitespace characters."),

  # --- DECLARED, prose stays home (see the header) ------------------------------------------------
  theme          = list(producers = c("tab_html", "tab_md", "tab_xl", "tab_export", "tab_css",
                                      "forest_plot"),                option = "theme",          doc_in_producer = TRUE),
  caption        = list(producers = c("tab_html", "tab_md", "tab_xl", "tab_export",
                                      "forest_plot"),                                           doc_in_producer = TRUE),
  css            = list(producers = c("tab_html", "tab_md"),         option = "css",            doc_in_producer = TRUE),
  format         = list(producers = c("tab_export", "tab_css"),                                 doc_in_producer = TRUE),
  file           = list(producers = c("tab_md", "tab_css"),                                     doc_in_producer = TRUE),
  path           = list(producers = c("tab_xl", "tab_export"),                                  doc_in_producer = TRUE),
  subtext        = list(producers = c("tab_md", "forest_plot"),                                 doc_in_producer = TRUE),
  tooltips       = list(producers = "tab_html",                      option = "tooltips",       doc_in_producer = TRUE),
  popover        = list(producers = "tab_html",                      option = "popover",        doc_in_producer = TRUE),
  print_rules    = list(producers = "tab_css",                       option = "print_rules",    doc_in_producer = TRUE),
  ratio_cells    = list(producers = "tab_xl",                        option = "ratio_cells",    doc_in_producer = TRUE),
  check          = list(producers = "tab_xl",                                                   doc_in_producer = TRUE),
  data           = list(producers = "tab_xl",                                                   doc_in_producer = TRUE),
  font_text      = list(producers = "tab_xl",                        option = "font_text",      doc_in_producer = TRUE),
  font_num       = list(producers = "tab_xl",                        option = "font_num",       doc_in_producer = TRUE),
  font_num_stars = list(producers = "tab_xl",                        option = "font_num_stars", doc_in_producer = TRUE)
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
      "i" = "Only {.arg data} and the variable roles are positional; name everything else."),
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
  out  <- c("@param ... Arguments taken by name, and kept out of the signature. Past the variable",
            "  roles every argument must be named, and an unknown name is refused with a suggestion.")
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
    "\\code{conf_level}. A coloured cell is then significantly different from its reference; a grey",
    "one may still be significant, only too small to colour."),
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
           c("pct", "na", "levels", "comp", "tot", "totaltab", "totcol", "output", "anova", "n")),
  # THE GRID RULE on a table too ragged to be a tribble: every row writes its fields in ONE order,
  # so the 81 of them read down a column. TAB_ARG_ORDER is that order, and this is what keeps it.
  all(vapply(c(TAB_ARGS, EXPORT_ARGS),
             function(r) identical(names(r), intersect(TAB_ARG_ORDER, names(r))), logical(1)))
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
