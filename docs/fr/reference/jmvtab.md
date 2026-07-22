# Crosstables

Crosstables

## Usage

``` r
jmvtab(
  data,
  row_vars = NULL,
  col_vars = NULL,
  tab_vars = NULL,
  wt = NULL,
  pct = "no",
  color = "no",
  OR = "no",
  color_signif = "ignore",
  chi2 = FALSE,
  anova = "welch",
  test_robust = "classic",
  na = "keep",
  lvs = "all",
  other_if_less_than = 0,
  cleannames = TRUE,
  refLevels = NULL,
  levelOrder = NULL,
  ref = "auto",
  ref2 = "first",
  comp = "tab",
  ci = "auto",
  conf_level = 0.95,
  ci_print = "ci",
  stars = FALSE,
  method_cell = "wilson",
  method_diff = "newcombe",
  method_ratio = "katz",
  method_mean_diff = "welch",
  method_mean_ratio = "robust",
  totaltab = "line",
  wrap_rows = 35,
  wrap_cols = 15,
  display = "auto",
  add_n = TRUE,
  add_pct = FALSE,
  subtext = "",
  digits = "0",
  n_min = 0,
  export_format = "excel",
  exportExcel = FALSE,
  export_dir = "~/Documents",
  export_filename = "Table",
  resetPath = FALSE,
  xl_replace = FALSE
)
```

## Arguments

- data:

  A data.frame.

- row_vars:

  The row variable, which will be printed with one level per line. If
  numeric, it will be converted to factor. If several row variables are
  provided, it's not possible to add any tab_vars.

- col_vars:

  One column is printed for each level of each column variable. For
  numeric variables means are calculated, in a single column.

- tab_vars:

  One subtable is made for each combination of levels of the tab
  variables. All tab variables are converted to factor. Leave empty to
  make a simple table. Not used when there are several row_vars.

- wt:

  A weight variable, of class numeric. Leave empty for unweighted
  results.

- pct:

  The type of percentages to calculate :

  - `"row"`: row percentages.

  - `"col"`: column percentages.

  - `"all"`: frequencies for each subtable/group, if there is
    `tab_vars`.

  - `"all_tabs"`: frequencies for the whole (set of) table(s).

- color:

  Which measure to use for color helpers, as a single string.

  - `"no"`: by default, no colors are printed.

  - `"auto"`: a smart per-column-type default (percentage-point
    difference on the text plus a relative-risk highlight on the
    background for factors, mean ratio for numeric columns).

  - `"diff"`: color the difference of each cell from its total (or
    reference cell). For factors this is a percentage-point difference;
    for numeric columns the standardized (SD-scaled) mean difference.

  - `"ratio"`: color the relative risk (factors) or mean ratio
    (numeric).

  - `"contrib"`: color cells based on their contribution to variance
    (factor columns only).

  - `"OR"`: for `pct == "col"` or `pct == "row"`, color based on odds
    ratios.

  How significance gates these colors is set separately by
  `color_signif`.

- OR:

  With `pct = "row"` or `pct = "col"`, calculate and print odds ratios
  (for binary variables) or relative risks ratios (for variables with 3
  levels or more).

  - `"no"`: by default, no OR are calculated.

  - `"OR"`: print OR (instead of percentages).

  - `"OR_pct"`: print OR, with percentages in bracket.

- color_signif:

  How statistical significance gates the colors, as a single string.

  - `"ignore"`: by default, color every deviation by its observed size.

  - `"grey_non_signif"`: color by observed size, but grey out cells
    whose deviation is not significant (at `conf_level`). A confidence
    interval on the difference is computed automatically.

  - `"guaranteed_effect"`: color by the guaranteed (confidence-bound)
    effect – all cells whose interval clears the threshold show, with
    dimmer colors.

- chi2:

  Set to `TRUE` to add a test p-value row: a Chi-square test for
  categorical column variables and an ANOVA F-test for numeric ones
  (chosen automatically per column type). Also enables colouring cells
  by their contribution to variance.

- anova:

  Which F statistic to display for numeric column variables when the
  test is on: Welch's F (default, does not assume equal variances) or
  the classic pooled F.

- test_robust:

  For a weighted table, a more robust p-value: "classic" (unweighted
  chi2 / Welch F); "kish" (first-order Rao-Scott rescale to the
  effective sample size).

- na:

  The policy to adopt with missing values. It must be a single string.

  - `na = "keep"`: by default, prints `NA`'s as explicit `"NA"` level.

  - `na = "drop"`: each column variable drops its own `NA`'s, so tables
    made with different column variables may have a different number of
    observations.

  - `na = "drop_all"`: drops every observation missing on the row
    variable, any column variable or a tab variable (all columns then
    share one base).

  - `na = "common_base"`: fixes a single population (non-missing on the
    row variable and the first column variable), while secondary column
    variables keep their own `NA`'s. Reproduces the historical
    [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
    behaviour.

- lvs:

  The levels of `col_vars` to keep.

  - `"all"`: by default, all levels are kept.

  - `"first"`: only keep the first level of each `col_vars`

  - `"auto"`: keep the first level when `col_var` is only two levels,
    keep all levels otherwise.

- other_if_less_than:

  When set to a positive integer, levels with less count than that will
  be merged into an "Others" level.

- cleannames:

  By default, clean levels names, by removing prefix numbers like "1-",
  and text in parenthesis. Set to `FALSE` to avoid this behaviour.

- refLevels:

  .

- levelOrder:

  .

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

  With `OR = OR` (odds-ratios) and a 3+ levels factor, a second
  reference cell is needed to calculate relative risks ratios. First
  cell by default.

- comp:

  The comparison level : by subtables/groups, or for the whole table.

- ci:

  The type of confidence intervals to calculate, passed to
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md).

  - `"cell"`: absolute confidence intervals of cells percentages.

  - `"diff"`: confidence intervals of the difference between a cell and
    the relative total cell (or relative first cell when
    `ref = "first"`).

  - `"auto"`: `ci = "diff"` for means and row/col percentages,
    `ci = "cell"` for frequencies ("all", "all_tabs").

  By default, for percentages, with `ci = "cell"` Wilson's method is
  used, and with `ci = "diff"` Wald's method along Agresti and Caffo's
  adjustment. Means use classic method.

- conf_level:

  The confidence level, as a single numeric between 0 and

  1.  Default to 0.95 (95\\

- ci_print:

  By default confidence interval are printed with the interval display.
  Set to "moe" to use pct +- moe instead.

- stars:

  With `ci = "diff"`, print significance stars (`*` `**` `***`) for the
  difference of each cell from its reference. Read from the same
  confidence interval that is displayed, so stars and bracket always
  agree.

- method_cell:

  The proportion confidence-interval method for `ci = "cell"`:
  `"wilson"` (the score interval, default) or `"wald"` (the normal
  approximation).

- method_diff:

  The proportion confidence-interval method for `ci = "diff"`.
  `"newcombe"` (default) is the dual of the two-proportion score test,
  so the interval and the significance stars always agree.

- method_ratio:

  The confidence-interval method for a ratio of proportions or rates
  (`ci = "ratio"` on percentages): Katz's log-ratio interval.

- method_mean_diff:

  The confidence-interval method for the difference of numeric means
  (means with `ci = "diff"`): Welch (default) or Student (pooled
  variance).

- method_mean_ratio:

  The confidence-interval method for a ratio of numeric means (means
  with `ci = "ratio"`).

- totaltab:

  The total table, if there are subtables/groups (i.e. when `tab_vars`
  is provided). Vectorised over `row_vars`.

  - `"line"`: by default, add a general total line (necessary for
    calculations with `comp = "all"`)

  - `"table"`: add a complete total table (i.e. `row_var` by `col_vars`
    without `tab_vars`).

  - `"no"`: not to draw any total table.

- wrap_rows:

  By default, rownames are wrapped when larger than 30 characters.

- wrap_cols:

  By default, colnames are wrapped when larger than 12 characters.

- display:

  The information to display in the table.

- add_n:

  For `pct = "row"` or `pct = "col"`, set to `FALSE` not to add another
  column or row with unweighted counts (`n`).

- add_pct:

  Set to `TRUE` to add a column with the frequencies of the row variable
  (for `pct = "row"`) or a row with the frequencies of the column
  variable (for `pct = "col"`)

- subtext:

  A character vector to print rows of legend under the table.

- digits:

  The number of digits to print, as a single integer (0-6). In R,
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  also accepts an integer vector the same length as `col_vars`.

- n_min:

  A pure display filter (0 = off). A row is dropped only when its
  largest base across the column variables is below `n_min`; surviving
  cells whose own base is below `n_min` are blanked. Under `pct = "col"`
  weak columns are dropped instead. Totals, the added-n row/column and
  the p-value line are always kept. Recomputes nothing.

- export_format:

  The export file format: Excel (`.xlsx`), HTML (`.html`) or Markdown
  (`.md`).

- exportExcel:

  Press to export the table to the chosen format (the button label
  follows the format).

- export_dir:

  The folder to save the exported file in. Blank or `~/Documents`
  auto-detects your real Documents folder (a redirected `D:` or network
  Documents included). Type any other folder to override; a leading `~`
  there expands to your home folder.

- export_filename:

  The bare file name, with NO extension (the chosen format adds it).
  Illegal characters are removed automatically. Blank saves as "Table".

- resetPath:

  Reset the folder and file name to their defaults (your Documents
  folder and "Table").

- xl_replace:

  "Set to `TRUE` to overwrite an existing file."

## Value

A results object containing:

|                       |     |     |     |     |          |
|-----------------------|-----|-----|-----|-----|----------|
| `results$html_table`  |     |     |     |     | a html   |
| `results$cache_state` |     |     |     |     | an image |
