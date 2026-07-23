# Cross-table with color helpers

`tab()` builds a cross-table of one or several row variables by one or
several column variables, and colors the cells so the table is easy to
read at a glance — in the R console, or exported to Excel, HTML or Word.
Cells can show counts, row or column percentages, or (for a numeric
column variable) means, optionally with differences, confidence
intervals and statistical tests.

The result is a `tibble` (of class `tabxplor_tab`), so you can keep
working on it with the usual dplyr verbs
([`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)).

New to the package? Start with
[`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor.md)
and with just four arguments — `data`, `row_vars`, `col_vars` and `pct`
— then add `color` when you want reading helpers.

## Usage

``` r
tab(
  data,
  row_vars,
  col_vars,
  tab_vars,
  wt,
  sup_cols,
  pct = "no",
  color = "no",
  color_signif = "ignore",
  OR = "no",
  test = FALSE,
  na = "keep",
  levels = "all",
  cleannames = NULL,
  other_if_less_than = 0,
  other_level = "Others",
  ref = "auto",
  ref2 = "first",
  comp = "tab",
  ci = "no",
  conf_level = getOption("tabxplor.conf_level", 0.95),
  stars = NULL,
  method_cell = "wilson",
  method_diff = "newcombe",
  method_ratio = "katz",
  method_mean_diff = "welch",
  method_mean_ratio = "robust",
  ids = NULL,
  strata = NULL,
  fpc = NULL,
  nest = FALSE,
  totaltab = "line",
  totaltab_name = "Ensemble",
  tot = c("row", "col"),
  total_names = "Total",
  add_n = TRUE,
  add_pct = FALSE,
  common_totrow = FALSE,
  subtext = "",
  digits = 0,
  n_min = 0,
  display = NULL,
  color_breaks = NULL,
  output_list = FALSE,
  parallel = NULL,
  spread_vars,
  names_prefix = NULL,
  names_sort = FALSE,
  row_var,
  col_var,
  chi2 = lifecycle::deprecated(),
  .cache = NULL,
  .defer_level_merge = FALSE,
  .return_armed = FALSE,
  .levels_order = NULL,
  filter
)
```

## Arguments

- data:

  A data frame.

- row_vars, col_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The row variable(s), printed with one level per line, and the column
  variable(s), printed with one level per column. For numeric variables
  means are calculated, in a single column. Each accepts one variable or
  several (e.g. `c(var1, var2)`); with several `row_vars` the mirror
  tables are merged into one by default (see `output_list`).

- tab_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Tab variables : a subtable is made for each combination of levels of
  the selected variables. Leave empty to make a simple cross-table. All
  `tab_vars` are converted to factor.

- wt:

  A weight variable, of class numeric. Leave empty for unweighted
  results.

- sup_cols:

  **\[deprecated\]** Supplementary columns variables, with only the
  first level printed. Deprecated in 2.0.0: pass these columns in
  `col_vars` and set `levels = "first"` instead (`col_vars` already
  accepts several variables).

- pct:

  The type of percentages to calculate :

  - `"row"`: row percentages.

  - `"col"`: column percentages.

  - `"all"`: frequencies for each subtable/group, if there is
    `tab_vars`.

  - `"all_tabs"`: frequencies for the whole (set of) table(s).

- color:

  Which measure(s) to color, on which visual channel. `FALSE` (default)
  prints no color; `TRUE` uses the smart per-column-type scheme
  (factors: `diff` on the text + `ratio` on the background; numerics:
  `ratio`; counts: `contrib`; odds-ratio columns: `or`). Otherwise a
  measure name, on the **text** channel:

  - `"diff"`: cell difference from the reference (percentage points for
    factors; the standardized difference Glass's \\\Delta\\ for numeric
    means).

  - `"ratio"`: relative risk (factors) or mean ratio (numerics) vs the
    reference.

  - `"contrib"`: signed contribution to the chi-squared
    (reference-free).

  - `"OR"`: empirical odds ratio (for `pct = "row"`/`"col"`), coloured
    on its own symmetric `odds_ratio` scale (so `pct_ratio` stays free
    for `"ratio"`).

  The grammar: **position picks the channel** (1st value -\> text, 2nd
  -\> background) and **names pick the column type** (`pct` / `mean`).
  So `c("diff", "ratio")` puts `diff` on the text and `ratio` on the
  background of every column; `c(pct = "diff", mean = "ratio")` colors
  factors by `diff` and numeric means by `ratio` (text channel);
  `list(pct = c("diff", "ratio"), mean = "ratio")` combines both
  (per-type, with channels). Only `diff` / `ratio` may go on the
  background. Thresholds come from
  [`set_color_breaks`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
  or the per-table `color_breaks` argument. (The old combined strings
  `"diff_ci"`, `"after_ci"` and `"ci"` still work but are
  soft-deprecated in favor of `color_signif`.)

- color_signif:

  How significance gates the color, as a single string:

  - `"ignore"` (default): color every deviation by its observed size.

  - `"grey_non_signif"`: color by the observed size, but grey out cells
    whose deviation is not significant at `conf_level`. A cell is
    coloured only when it is BOTH significant AND at least as large as
    the first colour threshold, so an un-coloured (grey) cell may still
    be significant – just too small to colour (and it can carry
    significance stars). The only guarantee is: a coloured cell is
    significantly different from its reference.

  - `"guaranteed_effect"`: color by the guaranteed (confidence-bound)
    effect – only cells whose interval clears the threshold, with
    dimmer, conservative colors.

- OR:

  With `pct = "row"` or `pct = "col"`, calculate and print odds ratios:
  for a binary variable the usual odds ratio; for a variable with 3
  levels or more, the odds ratio of each level versus the reference
  level (the empirical analogue of the "OR (j vs reference)" from a
  multinomial
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
  model).

  - `"no"`: by default, no OR are calculated.

  - `"OR"`: print OR (instead of percentages).

  - `"OR_pct"`: print OR, with percentages in bracket.

  Odds ratios don't add up to 100\\ `n` (console), exports the base-`n`
  column only, or nothing when `add_n = FALSE`.

- test:

  Set to `TRUE` to calculate a statistical test of independence for each
  (sub)table: **Chi-squared** for factor `col_vars`, **Welch's F**
  (one-way ANOVA) for numeric ones – see
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md).
  The whole-table summary also carries an **effect size** (Cramer's V /
  phi for factors, eta-squared for means) and, on a small sparse factor
  table where the chi-squared is unreliable, an exact **Fisher**
  p-value. Useful to print metadata, and to color cells based on their
  contribution to variance (`color = "contrib"`). Automatically added if
  needed for `color`.

  For a weighted table you can opt in to more robust uncertainty:
  `options(tabxplor.kish_neff = TRUE)` replaces the raw unweighted n
  with Kish's effective sample size `(sum w)^2 / sum w^2` in **every
  weighted confidence interval** (see the *Weighted confidence
  intervals* note below) *and* switches the whole-table tests to a
  first-order **Rao-Scott** correction (the chi-squared rescaled to
  `n_eff`, the F on per-group effective n). `test = "survey"` instead
  runs a fully **design-based** test
  ([`survey::svychisq`](https://rdrr.io/pkg/survey/man/svychisq.html)
  for factors, a `svyglm` Wald F for means), built from `wt` plus the
  optional `ids`/`strata`/`fpc` arguments. You may also pass a prebuilt
  [`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html)
  as `data`: its weights drive the estimates, the design drives the
  p-values (estimates/CIs stay tabxplor's single-stage weighted
  approximation).

- na:

  The policy to adopt for missing values, as a single string :

  - `"keep"`: by default, `NA`'s of row, col and tab variables are
    printed as an explicit `"NA"` level.

  - `"drop"`: remove `NA`'s in each row, col and tab variable before
    calculations, so each column is computed on its own non-missing
    observations (bases can then differ between col_vars).

  - `"drop_all"`: remove every observation missing on the `row_vars`,
    **any** `col_vars` or a `tab_vars`, so all columns share the same
    base (no `NA` anywhere).

  - `"common_base"`: fix a single population – observations non-missing
    on the `row_vars` and the **first** `col_vars` (and `tab_vars`) –
    shared by every column, while secondary `col_vars` keep their own
    `NA`'s as a level within it. This reproduces the historical `tab()`
    behaviour. Microdata only (not
    [`tab_counts`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_counts.md)).

  When several `row_vars` are combined into one table (no `tab_vars`),
  their `Total` rows are identical whenever they share one population
  (`"keep"`, `"drop_all"`, `"common_base"`) and are then displayed as a
  **single** Total row; only `"drop"` can make them genuinely differ, in
  which case every Total row is kept (with a message).

- levels:

  The levels of `col_vars` to keep, as a single string or a vector the
  same length as `col_vars` (for finer selections use
  [`dplyr::select`](https://dplyr.tidyverse.org/reference/select.html))
  :

  - `"all"`: by default, all levels are kept.

  - `"first"`: only keep the first level of each `col_vars` (handy for
    compact summary tables with many indicators).

  - `"auto"`: keep the first level when a `col_vars` has only two
    levels, keep all levels otherwise.

- cleannames:

  Set to `TRUE` to clean levels names, by removing prefix numbers like
  "1-", and text in parenthesis. All data formatting arguments are
  passed to
  [`tab_prepare`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_prepare.md).

- other_if_less_than:

  When set to a positive integer, levels with less count than it will be
  merged into an "Others" level.

- other_level:

  The name of the "Other" level, as a single string.

- ref:

  The reference cell to calculate differences and ratios (used to print
  `colors`) :

  - `"auto"`: by default, cell difference from the corresponding total
    (rows or cols depending on `pct = "row"` or `pct = "col"`) is used
    for `diff` ; cell ratio from the first line (or col) is use for `OR`
    (odds ratio/relative risks ratio).

  - `"tot"`: totals are always used.

  - `"first"`: calculate cell difference or ratio from the first cell of
    the row or column (useful to color temporal developments).

  - `n`: when `ref` is an integer, the nth row (or column) is used for
    comparison.

  - `"regex"`: when `ref` is a string, it it used as a regular
    expression, to match with the names of the rows (or columns). Be
    precise enough to match only one column or row, otherwise you get a
    warning message.

  - `"no"`: not use ref and not calculate diffs to gain calculation
    time.

- ref2:

  The second reference level for odds ratios (or relative risk ratios),
  needed only for a factor with **3 levels or more** (the "OR of each
  level versus `ref2`"). The first level is used by default. For a
  **binary** factor `ref2` is ignored: each level's OR is computed
  against the *other* level, so both levels show a value (reciprocals of
  one another) instead of one being forced to `1`. See `ref` above for
  the list of possible values.

- comp:

  The comparison level : by subtables/groups, or for the whole table.

  - `"tab"`: by default, contributions to variance, row differences from
    totals/first cells, and row confidence intervals for these
    differences, are calculated for each `tab_vars` group.

  - `"all"`: compare cells to the general total line (provided there is
    a total table with a total row), or with the first line of the total
    table when `ref = "first"`.

- ci:

  The type of confidence intervals to calculate, passed to
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md)
  (automatically added if needed for `color`).

  - `"cell"`: absolute confidence intervals of cells percentages.

  - `"diff"`: confidence intervals of the difference between a cell and
    the relative total cell (or relative first cell when
    `ref = "first"`).

  - `"ratio"`: like `"diff"`, but the interval is on the *ratio*
    (relative risk / mean ratio) scale between a cell and its reference
    (the Katz interval).

  - `"auto"`: `ci = "diff"` for means and row/col percentages,
    `ci = "cell"` for frequencies ("all", "all_tabs").

  By default, for percentages, `ci = "cell"` uses the Wilson score
  interval and `ci = "diff"` the Newcombe hybrid-score interval (its
  dual, so the bracket and the significance stars always agree); means
  use the Welch t interval. The method can be changed in
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
  (`method_cell` / `method_diff` / `method_mean_diff`). By default, with
  `ci = "cell"`, the result is printed in the `[inf;sup]` form. Set
  `options("tabxplor.ci_print" = "moe")` to print `pct +- moe` instead.

- conf_level:

  The confidence level, as a single numeric between 0 and 1. Default to
  0.95 (95%).

- stars:

  Logical (default `FALSE` *opt-in*). With `ci = "diff"`, print
  significance stars for each cell's difference from its reference, read
  from the displayed interval itself (universal CI-inclusion). `NULL`
  uses `options("tabxplor.stars")` (default `FALSE`). See
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

- method_cell, method_diff:

  Character strings choosing the confidence-interval method for
  `ci = "cell"` (`"wilson"` default, or `"wald"`) / `ci = "diff"`
  (`"newcombe"` default, `"ac"` or `"wald"`). See
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

- method_ratio, method_mean_diff, method_mean_ratio:

  Character strings choosing the confidence-interval method for the
  ratio / numeric-mean intervals. `method_ratio` (proportion ratio,
  `color = "ratio"`): `"katz"` (log risk-ratio, the only value).
  `method_mean_diff` (numeric mean difference): `"welch"` (default, each
  group's own variance) or `"student"` (pooled variance = a
  linear-regression coefficient interval). `method_mean_ratio` (numeric
  mean ratio, `color = "ratio"` on a mean): `"robust"` (default, each
  group's own variance = modified/robust Poisson), `"quasipoisson"`
  (dispersion-scaled = a quasi-Poisson regression) or `"poisson"`
  (naive). See
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

- ids, strata, fpc, nest:

  Survey-design specifications (column name(s) / a formula) used only
  when `test = "survey"` to build the design for the p-values: cluster
  ids (default `~1`, no clustering), strata, finite-population
  correction, and `nest` (are ids nested in strata). Ignored otherwise.
  Mirror the same arguments of
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md).

- totaltab:

  The total table, if there are subtables/groups (i.e. when `tab_vars`
  is provided) :

  - `"line"`: by default, add a general total line (necessary for
    calculations with `comp = "all"`)

  - `"table"`: add a complete total table (i.e. `row_var` by `col_vars`
    without `tab_vars`).

  - `"no"`: not to draw any total table.

- totaltab_name:

  The name of the total table, as a single string.

- tot:

  The totals :

  - `c("col", "row")` or `"both"` : by default, both total rows and
    total columns.

  - `"row"`: only total rows.

  - `"col"`: only total column.

  - `"no"`: remove all totals (after calculations if needed).

- total_names:

  The names of the totals, as a character vector of length one or two.
  Use syntax of type `c("Total row", "Total column")` to set different
  names for rows and cols.

- add_n:

  For `pct = "row"` or `pct = "col"`, set to `FALSE` not to add another
  column or row with unweighted counts (`n`).

- add_pct:

  Set to `TRUE` to add a column with the frequencies of the row variable
  (for `pct = "row"`) or a row with the frequencies of the column
  variable (for `pct = "col"`).

- common_totrow:

  With several `row_vars`, `FALSE` (the default) shows one Total row per
  row variable. Set to `TRUE` to collapse the identical Total rows into
  a single shared Total, displayed in its own group after a blank-line
  separator (bold when the total is the reference for at least one row
  variable). Genuinely different totals (e.g. under `na = "drop"`) are
  never merged.

- subtext:

  A character vector to print rows of legend under the table.

- digits:

  The number of digits to print, as a single integer, or an integer
  vector the same length as `col_vars`.

- n_min:

  A single positive integer (default `0`, off). A pure display filter
  applied last: it hides small-base cells without recomputing anything.
  A row is dropped only when its *largest* base across the column
  variables is below `n_min`; surviving cells whose own base is below
  `n_min` are blanked. Under `pct = "col"` the same rule drops weak
  columns. Total rows/columns, the added-`n` row/column and the p-value
  line are always kept.

- display:

  A single optional **composite display template** to show several
  fields in each value cell (text output only – the console,
  [`tab_kable`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)
  and
  [`tab_md`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_md.md);
  Excel falls back to the primary field). A
  [`{}`](https://rdrr.io/r/base/Paren.html) template listing the fields
  to combine, e.g. `"\{pct\} (n=\{n\})"` (a percentage with its count),
  `"\{n\} (\{pct\})"` or `"\{pct\} \{ci\}"`. Valid fields: `pct`, `n`,
  `wn`, `mean`, `diff`, `ratio`, `ci`, `or`, `ctr`, `var`; the first
  field is the *primary*, shown alone by Excel and used for coloring. A
  bare field name is also accepted as a shorthand for its single-field
  template, so `display = "ci"` is the same as `display = "\{ci\}"` (it
  shows the confidence interval). The special value `display = "num_ci"`
  is a type-adaptive shorthand for `"\{pct\} \{ci\}"` on percentage
  columns and `"\{mean\} \{ci\}"` on numeric (mean) columns, chosen per
  column, so a mixed factor + numeric table shows each value with its
  confidence interval in one call. Like `"\{pct\} \{ci\}"` it displays
  the CI the table computes (the cell, difference or ratio CI set by
  `ci = ` / `color`), so pair it with a `ci = ` value or a `color` that
  needs one. `NULL` (default) keeps the plain single-field display. It
  is a display overlay only: colors, differences and the underlying
  fields are unchanged.

- color_breaks:

  A per-table override of the colour thresholds, a named list of scales
  like
  [`set_color_breaks`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
  accepts, e.g. `list(pct_ratio = list(over = 2))`. Stored as a table
  attribute and applied at print / export; `NULL` (default) uses the
  global breaks. Unset scales fall back to the global setting.

- output_list:

  Logical (default `FALSE`). With several `row_var`, `FALSE` merges the
  mirror tables into a single `tabxplor_tab`; `TRUE` returns a list with
  one table per `row_var`. With `tab_vars`, tables stay a list
  regardless.

- parallel:

  Opt-in parallel build of the per-`row_var` tables, using the
  (Suggests-only) mirai package. `NULL` (default) reads
  `getOption("tabxplor.parallel")` (off); `FALSE` forces serial; `TRUE`
  uses an auto worker count; an integer sets the number of worker
  processes. Byte-identical to the serial result. It pays off for the
  survey workflow – *many* `row_vars` against a small/medium data frame
  (roughly 10k-60k rows) in ONE `tab()` call – and is a loss for few
  tables or multi-million-row data (so it stays opt-in). The worker pool
  persists for the session; release it with
  [`tab_parallel_stop`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_parallel_stop.md).

- spread_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A subset of `tab_vars` to pivot from subtables into columns, via
  [`tab_spread`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_spread.md)
  (applied at the end).

- names_prefix, names_sort:

  Passed to
  [`tab_spread`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_spread.md)
  when `spread_vars` is given: a string prefixed to each new column
  name, and whether to sort the new columns.

- row_var, col_var:

  **\[deprecated\]** Singular aliases of `row_vars`/`col_vars` (which
  now accept several variables). Kept working.

- chi2:

  **\[deprecated\]** Renamed to `test` in 2.0.0: the test is a
  Chi-squared only for factors (numeric `col_vars` get Welch's F), so
  the old name was misleading. Still works.

- .cache, .defer_level_merge, .return_armed, .levels_order:

  Internal, for the jamovi `jmvtab` live cache only: `.cache` is a
  mutable environment the content-addressed multi-tier store is threaded
  through (Phase 7e); `.defer_level_merge` keeps full factor levels
  through the aggregate and test so `levels` becomes a display-time
  drop; `.return_armed` (Phase 7f) returns the pre-`finalize_color_spec`
  table so the tier-3 cache can re-paint colours without a rebuild;
  `.levels_order` (Phase 7g-ii) is a named list of factor level orders
  applied post-aggregate, backing the jamovi level-reordering control
  (in R, relevel with
  [`forcats::fct_relevel`](https://forcats.tidyverse.org/reference/fct_relevel.html)
  before calling `tab()`). All default off; not for direct use.

- filter:

  **\[superseded\]** A
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  to apply to the data frame first, as a single string (which will be
  converted to code, i.e. to a call). Prefer filtering the data with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  upstream of `tab()`; this argument is kept for back-compatibility
  (e.g. printing multiple tabs from a
  [`tibble::tribble`](https://tibble.tidyverse.org/reference/tribble.html)).

## Value

A `tibble` of class `tab`, possibly with colored reading helpers. All
non-text columns are of class
[`fmt`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md),
storing all the data necessary to print formats and colors. Columns with
`row_var` and `tab_vars` are of class `factor` : every added `factor`
will be considered as a `tab_vars` and used for grouping. To add text
columns without using them in calculations, be sure they are of class
`character`.

## Details

`tab()` has many arguments, but you only need a handful to begin. They
fall into groups:

- **The table**: `data`, `row_vars`, `col_vars`, `tab_vars` (one
  sub-table per group), `wt` (a weight variable).

- **What each cell shows**: `pct` (row or column percentages, or leave
  counts), `digits`.

- **Colors (reading helpers)**: `color`, and `color_signif` (whether
  statistical significance gates the color). Thresholds and palettes are
  set once for the whole session with
  [`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
  and
  [`set_color_palette()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md);
  a color legend prints automatically.

- **Comparisons**: `OR` (odds ratios), and `ref` / `ref2` / `comp`
  (which cell is the baseline for differences).

- **Statistics**: `test` (chi-squared or Welch's F), and `ci` +
  `conf_level` + `stars` (confidence intervals). The fine interval
  methods (`method_cell`, `method_diff`, ...) are documented on
  [`tab_ci()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md).

- **Totals & missing values**: `tot`, `total_names`, `totaltab`, `na`,
  `levels`.

- **Advanced / output**: `display`, `n_min`, `output_list`, `parallel`,
  `spread_vars`, `filter`.

The package-wide display, color and statistics defaults are
[`options()`](https://rdrr.io/r/base/options.html), listed at
[tabxplor-options](https://bricenocenti.github.io/tabxplor/fr/reference/tabxplor-options.md).
`tab()` is a friendly wrapper around the more powerful
[`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

**Weighted confidence intervals.** With a weight (`wt`), by default a
cell confidence interval is exactly
`Wilson(weighted p, unweighted n = tot_n)`: it treats the weighted
proportion as if it came from `tot_n` independent Bernoulli trials
(means use the unweighted n the same way). Under unequal weights this
carries no design effect, so the default interval is **too narrow**. Opt
in to Kish's effective sample size with
`options(tabxplor.kish_neff = TRUE)`: it replaces that raw n with
`n_eff = (sum w)^2 / sum(w^2)` in **all** the descriptive intervals –
factor proportions and means alike (cell, difference, ratio, and the
`color = "OR"` significance) – and in the crude `empirical =` companions
of
[`tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md),
so unequal weights widen the intervals honestly. This is a single-stage
approximation (it needs the microdata weights, so
[`tab_counts`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_counts.md)
on pre-aggregated counts cannot apply it, and it is not valid for
clustered designs). Use `test = "survey"` for a fully design-based
p-value, or reach for
[`tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
(whose *model* standard errors are always design-based) when the
uncertainty must be exact.

## Significance stars

With `ci = "diff"` and `stars = TRUE`, each cell shows how sure we can
be that its difference from the reference is real and not just sampling
noise: `*` means significant at the 10\\ p-value is stored per cell in
the `pvalue` field of the `fmt` vectors, readable with `$pvalue` or
`get_pvalue()`.

There is no separate statistical test run behind the scenes: the
significance is read straight from the confidence interval that is
displayed. A cell is significant at a given level exactly when its
interval at that confidence level no longer contains zero, so the stars
and the printed `[inf; sup]` bracket can never contradict each other.
Which test this amounts to depends on the interval:

- **percentage difference** (default, `method_diff = "newcombe"`):
  inverting the Newcombe hybrid-score interval. This is, to a very close
  approximation, the classical two-sample test of proportions (the score
  / "N-1" chi-squared test).

- **percentage difference** with `method_diff = "ac"` or `"wald"`:
  inverting the Agresti-Caffo (adjusted Wald) or the Wald interval – an
  (adjusted) two-proportion z-test.

- **mean difference**: the **Welch two-sample t-test** (for groups with
  unequal variances); inverting the Welch t interval is exactly this
  well-known test.

- `ci = "cell"` (an absolute cell interval, not a difference) is purely
  descriptive, so it carries no stars and its `pvalue` is `NA`.

On weighted data the estimate is weighted but the sample size used is
the real (unweighted) number of cases, unless you opt in to Kish's
effective sample size with `options("tabxplor.kish_neff" = TRUE)`.

## See also

[`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
(the full-featured engine behind `tab()`) and
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
(regression tables). Go further with the helper functions:
[`tab_ci()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md)
(confidence intervals and their methods),
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
/
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
/
[`set_color_style()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
(colors),
[`tab_chi2()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md)
(statistical tests),
[`tab_pct()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_pct.md)
/
[`tab_tot()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_tot.md)
(percentages and totals). Export a table with
[`tab_xl()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_xl.md)
(Excel),
[`tab_kable()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)
(HTML),
[`tab_md()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_md.md)
(Markdown) or
[`tab_plot()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plot.md).
Package-wide defaults live in
[tabxplor-options](https://bricenocenti.github.io/tabxplor/fr/reference/tabxplor-options.md).

## Examples

``` r
# A simple cross-table:
tab(forcats::gss_cat, marital, race)
#> # A tabxplor tab: 7 × 5
#>   marital       Other Black  White  Total
#>   <fct>           <n>   <n>    <n>    <n>
#> 1 No answer         2     2     13     17
#> 2 Never married   633 1 305  3 478  5 416
#> 3 Separated       110   196    437    743
#> 4 Divorced        212   495  2 676  3 383
#> 5 Widowed          70   262  1 475  1 807
#> 6 Married         932   869  8 316 10 117
#> 7 Total         1 959 3 129 16 395 21 483


# With more variables provided, `tab` makes a subtables for each combination of levels:
# \donttest{
tab(forcats::gss_cat, marital, tab_vars = c(year, race))
#> # A tabxplor tab: 160 × 4
#> # Groups:         year, race [33]
#>     year  race  marital              n
#>     <fct> <fct> <fct>              <n>
#>   1 2000  Other No answer            1
#>   2 2000  Other Never married       60
#>   3 2000  Other Separated            8
#>   4 2000  Other Divorced            20
#>   5 2000  Other Widowed              8
#>   6 2000  Other Married             78
#>   7 2000  Other Total 2000 Other   175
#> 
#>   8 2000  Black Never married      157
#>   9 2000  Black Separated           43
#>  10 2000  Black Divorced            60
#>  11 2000  Black Widowed             48
#>  12 2000  Black Married            121
#>  13 2000  Black Total 2000 Black   429
#> 
#>  14 2000  White Never married      495
#>  15 2000  White Separated           61
#>  16 2000  White Divorced           361
#>  17 2000  White Widowed            217
#>  18 2000  White Married          1 079
#>  19 2000  White Total 2000 White 2 213
#> 
#>  20 2000  Total Total 2000 Total 2 817
#> 
#>  21 2002  Other Never married       47
#>  22 2002  Other Separated            9
#>  23 2002  Other Divorced            26
#>  24 2002  Other Widowed              2
#>  25 2002  Other Married             83
#>  26 2002  Other Total 2002 Other   167
#> 
#>  27 2002  Black Never married      163
#>  28 2002  Black Separated           26
#>  29 2002  Black Divorced            66
#>  30 2002  Black Widowed             39
#>  31 2002  Black Married            116
#>  32 2002  Black Total 2002 Black   410
#> 
#>  33 2002  White Never married      498
#>  34 2002  White Separated           61
#>  35 2002  White Divorced           353
#>  36 2002  White Widowed            206
#>  37 2002  White Married          1 070
#>  38 2002  White Total 2002 White 2 188
#> 
#>  39 2002  Total Total 2002 Total 2 765
#> 
#>  40 2004  Other Never married       58
#>  41 2004  Other Separated            8
#>  42 2004  Other Divorced            17
#>  43 2004  Other Widowed              6
#>  44 2004  Other Married            112
#>  45 2004  Other Total 2004 Other   201
#> 
#>  46 2004  Black Never married      152
#>  47 2004  Black Separated           27
#>  48 2004  Black Divorced            50
#>  49 2004  Black Widowed             23
#>  50 2004  Black Married            125
#>  51 2004  Black Total 2004 Black   377
#> 
#>  52 2004  White Never married      409
#>  53 2004  White Separated           60
#>  54 2004  White Divorced           348
#>  55 2004  White Widowed            175
#>  56 2004  White Married          1 242
#>  57 2004  White Total 2004 White 2 234
#> 
#>  58 2004  Total Total 2004 Total 2 812
#> 
#>  59 2006  Other No answer            1
#>  60 2006  Other Never married      165
#>  61 2006  Other Separated           40
#>  62 2006  Other Divorced            65
#>  63 2006  Other Widowed             19
#>  64 2006  Other Married            302
#>  65 2006  Other Total 2006 Other   592
#> 
#>  66 2006  Black Never married      270
#>  67 2006  Black Separated           35
#>  68 2006  Black Divorced           102
#>  69 2006  Black Widowed             54
#>  70 2006  Black Married            173
#>  71 2006  Black Total 2006 Black   634
#> 
#>  72 2006  White No answer            5
#>  73 2006  White Never married      645
#>  74 2006  White Separated           81
#>  75 2006  White Divorced           565
#>  76 2006  White Widowed            293
#>  77 2006  White Married          1 695
#>  78 2006  White Total 2006 White 3 284
#> 
#>  79 2006  Total Total 2006 Total 4 510
#> 
#>  80 2008  Other Never married       68
#>  81 2008  Other Separated           11
#>  82 2008  Other Divorced            14
#>  83 2008  Other Widowed             10
#>  84 2008  Other Married             80
#>  85 2008  Other Total 2008 Other   183
#> 
#>  86 2008  Black Never married      117
#>  87 2008  Black Separated           25
#>  88 2008  Black Divorced            36
#>  89 2008  Black Widowed             18
#>  90 2008  Black Married             85
#>  91 2008  Black Total 2008 Black   281
#> 
#>  92 2008  White No answer            5
#>  93 2008  White Never married      346
#>  94 2008  White Separated           34
#>  95 2008  White Divorced           231
#>  96 2008  White Widowed            136
#>  97 2008  White Married            807
#>  98 2008  White Total 2008 White 1 559
#> 
#>  99 2008  Total Total 2008 Total 2 023
#> 
#> 100 2010  Other Never married       74
#> # ℹ 60 more rows
# }

# You can add several col_vars, mixing factors and numeric (means) ; `levels = "first"`
# keeps only the first level of each factor col_var for compact summary tables:
# \donttest{
tab(dplyr::storms, category, c(status, pressure, wind))
#> # A tabxplor tab: 7 × 13
#>   category disturbance extratropical hurricane `other low`
#>   <fct>            <n>           <n>       <n>         <n>
#> 1 1                  0             0     2 707           0
#> 2 2                  0             0     1 046           0
#> 3 3                  0             0       632           0
#> 4 4                  0             0       586           0
#> 5 5                  0             0       129           0
#> 6 NA               212         2 318         0       1 623
#> 7 Total            212         2 318     5 100       1 623
#> # ℹ 8 more variables: `subtropical depression` <n>, `subtropical storm` <n>,
#> #   `tropical depression` <n>, `tropical storm` <n>, `tropical wave` <n>,
#> #   Total <n>, pressure <mean>, wind <mean>
# }

# Colors to help the user read the table:
data <- forcats::gss_cat |>
  dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
gss  <- "Source: General social survey 2000-2014"
gss2 <- "Source: General social survey 2000, 2006 and 2012"

# Differences between the cell and it's subtable's total cell:
# \donttest{
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff")
#> # A tabxplor tab: 13 × 7
#> # Groups:         year [4]
#>    year     race           `Never married` Separated Divorced Married
#>    <fct>    <fct>                   <row%>    <row%>   <row%>  <row%>
#>  1 2000     Other                      36%        5%      12%     47%
#>  2 2000     Black                      41%       11%      16%     32%
#>  3 2000     White                      25%        3%      18%     54%
#>  4 2000     Total 2000                 28%        4%      17%     50%
#> 
#>  5 2006     Other                      29%        7%      11%     53%
#>  6 2006     Black                      47%        6%      18%     30%
#>  7 2006     White                      22%        3%      19%     57%
#>  8 2006     Total 2006                 26%        4%      18%     52%
#> 
#>  9 2012     Other                      37%        6%       7%     51%
#> 10 2012     Black                      43%        5%      21%     31%
#> 11 2012     White                      25%        3%      18%     53%
#> 12 2012     Total 2012                 29%        4%      18%     50%
#> 
#> 13 Ensemble Total Ensemble             27%        4%      18%     51%
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
#> # Source: General social survey 2000, 2006 and 2012
# }

# Differences between the cell and the whole table's general total cell:
# \donttest{
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff",
  comp = "all")
#> # A tabxplor tab: 13 × 7
#> # Groups:         year [4]
#>    year     race           `Never married` Separated Divorced Married
#>    <fct>    <fct>                   <row%>    <row%>   <row%>  <row%>
#>  1 2000     Other                      36%        5%      12%     47%
#>  2 2000     Black                      41%       11%      16%     32%
#>  3 2000     White                      25%        3%      18%     54%
#>  4 2000     Total 2000                 28%        4%      17%     50%
#> 
#>  5 2006     Other                      29%        7%      11%     53%
#>  6 2006     Black                      47%        6%      18%     30%
#>  7 2006     White                      22%        3%      19%     57%
#>  8 2006     Total 2006                 26%        4%      18%     52%
#> 
#>  9 2012     Other                      37%        6%       7%     51%
#> 10 2012     Black                      43%        5%      21%     31%
#> 11 2012     White                      25%        3%      18%     53%
#> 12 2012     Total 2012                 29%        4%      18%     50%
#> 
#> 13 Ensemble Total Ensemble             27%        4%      18%     51%
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
#> # Source: General social survey 2000, 2006 and 2012
# }

# Historical differences:
# \donttest{
data2 <- data |> dplyr::mutate(year = as.factor(year))
tab(data2, year, marital, race, subtext = gss2, pct = "row",
    color = "diff", ref = "first", tot = "col")
#> # A tabxplor tab: 10 × 7
#> # Groups:         race [4]
#>    race     year           `Never married` Separated Divorced Married
#>    <fct>    <fct>                   <row%>    <row%>   <row%>  <row%>
#>  1 Other    2000                       36%        5%      12%     47%
#>  2 Other    2006                       29%        7%      11%     53%
#>  3 Other    2012                       37%        6%       7%     51%
#> 
#>  4 Black    2000                       41%       11%      16%     32%
#>  5 Black    2006                       47%        6%      18%     30%
#>  6 Black    2012                       43%        5%      21%     31%
#> 
#>  7 White    2000                       25%        3%      18%     54%
#>  8 White    2006                       22%        3%      19%     57%
#>  9 White    2012                       25%        3%      18%     53%
#> 
#> 10 Ensemble Total Ensemble             27%        4%      18%     51%
#> # ℹ 1 more variable: Total <row%>
#> # difference (ref.): -30 -20 -10 -5 +5 +10 +20 +30
#> # Source: General social survey 2000, 2006 and 2012


# Differences with the total, except if their confidences intervals are superior to them:
tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "diff_ci")
#> Warning: The `color = "diff_ci"` mode was deprecated in tabxplor 2.0.0.
#> ℹ Please use `color = "diff"` with the `color_signif` argument instead.
#> # A tabxplor tab: 4 × 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>   <fct>       <row%>          <row%>    <row%>   <row%>  <row%>  <row%>
#> 1 Other           0%             32%        6%      11%      4%     48%
#> 2 Black           0%             42%        6%      16%      8%     28%
#> 3 White           0%             21%        3%      16%      9%     51%
#> 4 Total           0%             25%        3%      16%      8%     47%
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30 [grey: non-significant or under ±5 points]
#> # Source: General social survey 2000-2014

# Same differences, minus their confidence intervals:
tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "after_ci")
#> Warning: The `color = "after_ci"` mode was deprecated in tabxplor 2.0.0.
#> ℹ Please use `color = "diff"` with the `color_signif` argument instead.
#> # A tabxplor tab: 4 × 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>   <fct>       <row%>          <row%>    <row%>   <row%>  <row%>  <row%>
#> 1 Other           0%             32%        6%      11%      4%     48%
#> 2 Black           0%             42%        6%      16%      8%     28%
#> 3 White           0%             21%        3%      16%      9%     51%
#> 4 Total           0%             25%        3%      16%      8%     47%
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -25 -15 -5 -0 +0 +5 +15 +25 [all that is significant is colored, error-adjusted]
#> # Source: General social survey 2000-2014

# Contribution of cells to table's variance, like in a correspondence analysis:
tab(forcats::gss_cat, race, marital, subtext = gss, color = "contrib")
#> |      | Tests           |  marital |
#> |:-----|:----------------|---------:|
#> | race | N               |   21 483 |
#> |      | pvalue (Chi2 !) |   <0.01% |
#> |      | Cramér's V      | V = 0.15 |
#> 
#> # A tabxplor tab: 4 × 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married  Total
#>   <fct>          <n>             <n>       <n>      <n>     <n>     <n>    <n>
#> 1 Other            2             633       110      212      70     932  1 959
#> 2 Black            2           1 305       196      495     262     869  3 129
#> 3 White           13           3 478       437    2 676   1 475   8 316 16 395
#> 4 Total           17           5 416       743    3 383   1 807  10 117 21 483
#> # contribution to Chi2 (vs the mean): ×10 ×5 ×2 ×1 ×1 ×2 ×5 ×10
#> # Source: General social survey 2000-2014
# }

# Since the result is a tibble, you can use all dplyr verbs to modify it :
# \donttest{
library(dplyr)
tab(dplyr::storms, category, c(status, pressure, wind)) |>
  dplyr::filter(category != "-1") |>
  dplyr::select(-`tropical depression`) |>
  dplyr::arrange(is_totrow(.), desc(category))
#> Error in dplyr::arrange(.data = tibble::as_tibble(.data), ... = !!!dots,     .by_group = FALSE, .locale = .locale): ℹ In argument: `..3 = is_totrow(.)`.
#> Caused by error:
#> ! object '.' not found
# }

# \donttest{
# With `dplyr::arrange`, don't forget to keep the order of tab variables and total rows:
tab(data, race, marital, year, pct = "row") |>
  dplyr::arrange(year, is_totrow(.), desc(Married))
#> Error in dplyr::arrange(.data = tibble::as_tibble(.data), ... = !!!dots,     .by_group = FALSE, .locale = .locale): ℹ In argument: `..5 = is_totrow(.)`.
#> Caused by error:
#> ! object '.' not found
  # }
```
