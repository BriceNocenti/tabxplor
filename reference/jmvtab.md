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
  color_signif = "ignore",
  test = FALSE,
  anova = "welch",
  na = "keep",
  lvs = "all",
  cleannames = TRUE,
  ref_levels = NULL,
  levels_order = NULL,
  levels_collapse = NULL,
  shape = NULL,
  ref = "auto",
  ref2 = "first",
  comp = "tab",
  ci = "auto",
  conf_level = 0.95,
  stars = FALSE,
  design_effect = FALSE,
  ci_method_cell = "wilson",
  ci_method_diff = "newcombe",
  ci_method_mean_diff = "welch",
  ci_method_mean_ratio = "robust",
  tab_theme = "light",
  totaltab = "line",
  wrap_rows = 35,
  wrap_cols = 15,
  display = "auto",
  n = "range",
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

  Which measure to use for color helpers, as a single string. The values
  are the measure names
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  itself takes (the short spellings `"diff"` / `"OR"` stay valid aliases
  in R).

  - `"no"`: by default, no colors are printed.

  - `"auto"`: a smart per-column-type default (percentage-point
    difference on the text plus a relative-risk highlight on the
    background for factors, mean ratio for numeric columns).

  - `"difference"`: color the difference of each cell from its total (or
    reference cell). For factors this is a percentage-point difference;
    for numeric columns the standardized (SD-scaled) mean difference.

  - `"ratio"`: color the relative risk (factors) or mean ratio
    (numeric).

  - `"odds_ratio"`: for `pct == "col"` or `pct == "row"`, color based on
    odds ratios. To PRINT them, set `display` (the colour and the
    printed quantity are two questions).

  - `"contrib"`: color cells based on their contribution to variance
    (factor columns only).

  How significance gates these colors is set separately by
  `color_signif`.

- color_signif:

  How statistical significance gates the colors, as a single string.

  - `"ignore"`: by default, color every deviation by its observed size.

  - `"grey_non_signif"`: color by observed size, but grey out cells
    whose deviation is not significant (at `conf_level`). A confidence
    interval on the difference is computed automatically.

  - `"guaranteed_effect"`: color by the guaranteed (confidence-bound)
    effect – all cells whose interval clears the threshold show, with
    dimmer colors.

- test:

  Set to `TRUE` to add a test p-value row: a Chi-square test for
  categorical column variables and an ANOVA F-test for numeric ones
  (chosen automatically per column type). Also enables colouring cells
  by their contribution to variance.

- anova:

  Which F statistic to display for numeric column variables when the
  test is on: Welch's F (default, does not assume equal variances) or
  the classic pooled F.

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
    [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
    behaviour.

- lvs:

  The levels of `col_vars` to keep.

  - `"all"`: by default, all levels are kept.

  - `"first"`: only keep the first level of each `col_vars`

  - `"auto"`: keep the first level when `col_var` is only two levels,
    keep all levels otherwise.

- cleannames:

  By default, clean levels names, by removing prefix numbers like "1-",
  and text in parenthesis. Set to `FALSE` to avoid this behaviour.

- ref_levels:

  .

- levels_order:

  .

- levels_collapse:

  .

- shape:

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

  **What the confidence interval is anchored on** – one question, four
  answers. The GEOMETRY of the interval is not asked here: it follows
  the comparison the table makes (set by `color` / `display`), so a
  difference table gets a difference interval and a ratio table a ratio
  one.

  - `"auto"`: build the comparison interval when something reads it
    (`stars`, or a `color_signif` policy), and none otherwise.

  - `"no"`: no interval at all.

  - `"cell"`: each cell's own interval (a percentage / a mean with its
    own bounds). It anchors nothing to compare, so `stars` and
    `color_signif` are informed and switched off.

  - `"ref"`: the interval of the comparison with the reference cell –
    what `stars` and `color_signif` read.

- conf_level:

  The confidence level, a single numeric between 0 and 1 — 0.95 by
  default.

- stars:

  With `ci = "diff"`, print significance stars (`*` `**` `***`) for the
  difference of each cell from its reference. Read from the same
  confidence interval that is displayed, so stars and bracket always
  agree.

- design_effect:

  For a WEIGHTED table, make the confidence intervals, the significance
  stars, the colour thresholds AND the p-values account for the unequal
  weighting (the exact flat survey-design variance) instead of using the
  raw number of respondents. Sets options(tabxplor.design_effect). Off
  by default; it moves every interval in the table, not only the
  p-value.

- ci_method_cell:

  The proportion confidence-interval method for `ci = "cell"`:
  `"wilson"` (the score interval, default) or `"wald"` (the normal
  approximation).

- ci_method_diff:

  The proportion confidence-interval method for `ci = "diff"`.
  `"newcombe"` (default) is the dual of the two-proportion score test,
  so the interval and the significance stars always agree.

- ci_method_mean_diff:

  The confidence-interval method for the difference of numeric means
  (means with `ci = "diff"`): Welch (default), Student (the two groups
  pooled) or OLS (pooled over every level of the variable, i.e. the
  interval a linear model gives that coefficient).

- ci_method_mean_ratio:

  The confidence-interval method for a ratio of numeric means (means
  with `ci = "ratio"`).

- tab_theme:

  How the table is painted, in the results panel and in every export.
  `"light"` is the colour palette; `"print_ready"` says the same thing
  typographically — bold, italics, underlines and marks instead of blue
  and red — for a page that has no colour. See
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md).

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

  What each cell shows. Every value here is a `tab(display =)` value: a
  bare field name, a named layout such as `"base_ci"` (each value with
  its interval), or a [`{}`](https://rdrr.io/r/base/Paren.html) template
  combining fields. `"auto"` keeps whatever the table was built with. A
  template naming a field the table does not carry renders empty and
  says which argument would fill it.

- n:

  How many people the table is about: `"range"` prints the unweighted
  base beside the Total cell (as `min-max` when the column variables
  rest on different people), `"min"` the smallest base only, `"no"` no
  count at all.

- add_pct:

  Set to `TRUE` to add a column with the frequencies of the row variable
  (for `pct = "row"`) or a row with the frequencies of the column
  variable (for `pct = "col"`)

- subtext:

  A character vector to print rows of legend under the table.

- digits:

  The number of digits to print, as a single integer (0-6). In R,
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
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
  auto-detects your real Documents folder (a redirected `D:/Documents`
  or network Documents included). Type any other folder to override; a
  leading `~` there expands to your home folder.

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
