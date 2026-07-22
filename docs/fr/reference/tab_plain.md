# Plain single cross-table

Plain single cross-table

## Usage

``` r
tab_plain(
  data,
  row_var,
  col_var,
  tab_vars,
  wt,
  pct = "no",
  color = "no",
  OR = "no",
  na = "keep",
  ref = "auto",
  ref2 = "first",
  comp = "tab",
  totaltab = "line",
  totaltab_name = "Ensemble",
  tot = NULL,
  total_names = "Total",
  subtext = "",
  digits = 0,
  num = FALSE,
  df = FALSE,
  conf_level = 0.95,
  stars = FALSE,
  color_signif = "ignore",
  .fine = NULL,
  .by_table = FALSE
)
```

## Arguments

- data:

  A data frame.

- row_var, col_var:

  The row variable, which will be printed with one level per line, and
  the column variable, which will be printed with one level per column.
  Numeric variables will be used as factors. To calculate means, use
  [`tab_num`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md).

- tab_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Tab variables : a subtable is made for each combination of levels of
  the selected variables. Leave empty to make a simple cross-table. All
  tab variables are converted to factor.

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

  The type of colors to print, as a single string :

  - `"no"`: by default, no colors are printed.

  - `"diff"`: color percentages and means based on cells differences
    from totals (or from first cells when `ref = "first"`).

  - `"OR"`: for `pct == "col"` or `pct == "row"`, color based on odds
    ratios (or relative risks ratios)

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

- na:

  The policy to adopt with missing values, as a single string.

  - `"keep"`: by default, `NA`'s of row, col and tab variables are
    printed as explicit "NA" level.

  - `"drop"`: removes NA of row, col and tab variables.

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

  Comparison level. When `tab_vars` are present, should the
  contributions to variance be calculated for each subtable/group (by
  default, `comp = "tab"`) ? Should they be calculated for the whole
  table (`comp = "all"`) ? `comp` must be set once and for all the first
  time you use `tab_plain`,
  [`tab_num`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md)
  or
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md)
  with rows, or
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md).

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

  Set to `TRUE` to obtain a table with normal numeric vectors (not fmt).

- df:

  Set to `TRUE` to obtain a plain data.frame (not a tibble), with normal
  numeric vectors (not fmt). Useful, for example, to pass the table to
  correspondence analysis with FactoMineR.

- .fine, .by_table:

  Internal. `.fine` is a pre-computed count-aggregate to roll up from
  instead of scanning the raw data (used by
  [`tab_counts`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_counts.md)
  and the scan-fusion path); `.by_table` forces the table-by-table path.

## Value

A `tibble` of class `tabxplor_tab`. If `...` (`tab_vars`) are provided,
a `tab` of class `tabxplor_grouped_tab`. All non-text columns are
[`fmt`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
vectors of class `tabxplor_fmt`, storing all the data necessary to print
formats and colors. Columns with `row_var` and `tab_vars` are of class
`factor` : every added `factor` will be considered as a `tab_vars` and
used for grouping. To add text columns without using them in
calculations, be sure they are of class `character`.

## Examples

``` r
# A typical workflow with tabxplor step-by-step functions :
# \donttest{
data <- dplyr::starwars |> tab_prepare(sex, hair_color)

data |>
  tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row") |>
  tab_chi2() |>
  tab_ci(color = "after_ci")
#> |     | Tests           | hair_color |
#> |:----|:----------------|-----------:|
#> | sex | N               |         87 |
#> |     | pvalue (Chi2 !) |       6.8% |
#> |     | Cramér's V      |   V = 0.41 |
#> 
#> # A tabxplor tab: 6 × 14
#>   sex            auburn `auburn, grey` `auburn, white`  black blond blonde brown
#>   <fct>          <row%>         <row%>          <row%> <row%> <row> <row%> <row>
#> 1 female             6%             0%              0%    19%    0%     6%   31%
#> 2 hermaphroditic     0%             0%              0%     0%    0%     0%    0%
#> 3 male               0%             2%              2%    15%    5%     0%   18%
#> 4 none               0%             0%              0%     0%    0%     0%    0%
#> 5 NA                 0%             0%              0%    25%    0%     0%   50%
#> 6 Total              1%             1%              1%    15%    3%     1%   21%
#> # ℹ 6 more variables: `brown, grey` <row%>, grey <row%>, none <row%>,
#> #   white <row%>, `NA` <row%>, Total <row%>
#> # difference (Total): -25 -10 -0 +0 +10 +25 [all that is significant is colored, error-adjusted]
# }
```
