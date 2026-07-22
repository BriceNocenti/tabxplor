# Means table

Cross categorical variables with numeric variables, and get a table of
means and standard deviations.

## Usage

``` r
tab_num(
  data,
  row_var,
  col_vars,
  tab_vars,
  wt,
  color = "auto",
  color_signif = "ignore",
  na = c("keep", "drop"),
  ref = "tot",
  comp = c("tab", "all"),
  ci = NULL,
  conf_level = getOption("tabxplor.conf_level", 0.95),
  stars = NULL,
  method_mean_diff = "welch",
  method_mean_ratio = "robust",
  ci_scale = "diff",
  totaltab = "line",
  totaltab_name = "Ensemble",
  tot = NULL,
  total_names = "Total",
  subtext = "",
  digits = 0,
  num = FALSE,
  df = FALSE,
  color_breaks = NULL,
  .fine = NULL,
  .by_table = FALSE
)
```

## Arguments

- data:

  A data frame.

- row_var:

  The row variable, which will be printed with one level per line. If
  numeric, it will be used as a factor.

- col_vars:

  The numeric variables, which will appear in columns : means and
  standard deviation are calculated for each levels of `row_var` and
  `tab_vars`.

- tab_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Tab variables : a subtable is made for each combination of levels of
  the selected variables. Leave empty to make a simple cross-table. All
  tab variables are converted to factor.

- wt:

  A weight variable, of class numeric. Leave empty for unweighted
  results.

- color:

  Which measure(s) to color, on which channel – see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) for
  the full grammar (`FALSE`/`TRUE`, a measure name, or a positional
  two-channel `c("diff", "ratio")` vector). For numeric means the useful
  measures are `"diff"` (standardized, Glass's \\\Delta\\) and `"ratio"`
  (mean ratio); `TRUE` uses `"ratio"`. Default `"auto"` keeps the
  historical behavior.

- color_signif:

  How significance gates the color (`"ignore"` / `"grey_non_signif"` /
  `"guaranteed_effect"`) – see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- na:

  The policy to adopt for missing values in row and tab variables
  (factors), as a single string.

  - `"keep"`: by default, `NA`'s of row and tab variables are printed as
    an explicit `"NA"` level.

  - `"drop"`: remove `NA`'s in row and tab variables.

  `NA`s in numeric variables are always removed when calculating means.
  For that reason the `n` field of each resulting
  [`fmt`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  column, used to calculate confidence intervals, only takes into
  account the complete observations (without `NA`). To drop all rows
  with `NA` in any numeric variable first, use
  [`tab_prepare`](https://bricenocenti.github.io/tabxplor/reference/tab_prepare.md)
  or
  [`tab_many`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  with the `na_drop_all` argument.

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

- comp:

  Comparison level. When `tab_vars` are present, should the
  contributions to variance be calculated for each subtable/group (by
  default, `comp = "tab"`) ? Should they be calculated for the whole
  table (`comp = "all"`) ? `comp` must be set once and for all the first
  time you use
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md),
  `tab_num` or
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/reference/tab_chi2.md)
  with rows, or
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md).

- ci:

  The type of confidence intervals to calculate, passed to
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
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

- conf_level:

  The confidence level for the confidence intervals, as a single numeric
  between 0 and 1. Default to 0.95 (95%).

- stars:

  Logical (opt-in; default `FALSE`, or `options("tabxplor.stars")` when
  `NULL`). With `ci = "diff"`, print per-cell Welch t significance stars
  for the difference from the reference row; the mean-diff interval then
  uses the Welch t quantile (z when `FALSE`).

- method_mean_diff, method_mean_ratio:

  Character strings, the numeric-mean CI methods – see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).
  `method_mean_diff`: mean difference (`"welch"` / `"student"`).
  `method_mean_ratio`: mean ratio (`"robust"` / `"quasipoisson"` /
  `"poisson"`).

- ci_scale:

  Character string, the scale the `ci = "diff"` interval is expressed
  on: `"diff"` (default, neutral 0) or `"ratio"` (a ratio-of-means
  interval, neutral 1, stored as `ci_type = "ratio"`).
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  sets it from the colour (`color = "ratio"`).

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

- subtext:

  A character vector to print rows of legend under the table.

- digits:

  The number of digits to print, as a single integer.

- num:

  Set to `TRUE` to obtain a table with normal numeric vectors (not
  `fmt`).

- df:

  Set to `TRUE` to obtain a plain data.frame (not a `tibble`), with
  normal numeric vectors (not `fmt`). Useful, for example, to pass the
  table to correspondence analysis with FactoMineR.

- color_breaks:

  A per-table colour-threshold override – see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- .fine, .by_table:

  Internal. `.fine` is a pre-computed moment-sum aggregate (from
  `tab_aggregate_num()`) to adopt instead of scanning the raw data;
  `.by_table` forces the table-by-table path (a fresh scan). Both
  default to the fresh-scan behaviour.

## Value

A `tibble` of class `tabxplor_tab`. If `...` (`tab_vars`) are provided,
a `tab` of class `tabxplor_grouped_tab`. All non-text columns are
[`fmt`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
vectors of class `tabxplor_fmt`, storing all the data necessary to print
formats and colors. Columns with `row_var` and `tab_vars` are of class
`factor` : every added `factor` will be considered as a `tab_vars` and
used for grouping. To add text columns without using them in
calculations, be sure they are of class `character`.

## Examples

``` r
# \donttest{
data <- dplyr::storms |> tab_prepare(category, wind, na_drop_all = wind)
tab_num(data, category, wind, tot = "row", color = "after_ci")
#> # A tabxplor tab: 7 × 2
#>   category      wind
#>   <fct>       <mean>
#> 1 1         71 (σ6 )
#> 2 2         89 (σ4 )
#> 3 3        104 (σ4 )
#> 4 4        122 (σ6 )
#> 5 5        146 (σ6 )
#> 6 NA        38 (σ12)
#> 7 Total     50 (σ25)
#> # standardized difference (Total): -0.6 -0.3 -0 +0 +0.3 +0.6 [all that is significant is colored, error-adjusted]
# }
```
