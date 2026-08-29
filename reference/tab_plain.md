# Plain single cross-table

**\[superseded\]**

One bare cross-table of counts or percentages, from ONE row variable and
ONE column variable. Superseded by
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
which does the same and everything around it (several variables,
colours, totals, tests) – but it stays the smallest entry point into the
aggregate core, and takes the same `ci` / `ci_method` / `conf_level` /
`stars` / `display` arguments, resolved by the same rules, so its
numbers agree with
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
cell for cell.

## Usage

``` r
tab_plain(
  data,
  row_var,
  col_var,
  tab_vars,
  wt,
  ...,
  num = FALSE,
  df = FALSE,
  .fine = NULL,
  .by_table = FALSE
)
```

## Arguments

- data:

  A data frame.

- row_var, col_var:

  **\[deprecated\]** Singular aliases of `row_vars`/`col_vars` (which
  now accept several variables). Kept working.

- tab_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Tab variables: one subtable per combination of their levels. Leave
  empty for a simple cross-table.

- wt:

  A weight variable, of class numeric. Leave empty for unweighted
  results.

- ...:

  Every other argument of
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) –
  `pct`, `color`, `ci`, `tot`, ... – passed by name. See
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md); a
  typo gets a suggestion.

- num:

  Set to `TRUE` to obtain a table with normal numeric vectors (not fmt).

- df:

  Set to `TRUE` to obtain a plain data.frame (not a tibble), with normal
  numeric vectors (not fmt). Useful, for example, to pass the table to
  correspondence analysis with FactoMineR.

- .fine, .by_table:

  Internal. `.fine` is a pre-computed count-aggregate to roll up from
  instead of scanning the raw data (used by
  [`tab_counts`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
  and the scan-fusion path); `.by_table` forces the table-by-table path.

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
# the leaf builds the cells AND their intervals: `ci` is resolved here exactly as in tab(),
# so tab_plain(ci = "ref") and tab(ci = "ref") agree cell for cell.
dplyr::starwars |>
  tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row",
            ci = "ref", color = "difference", color_signif = "grey_non_signif")
#> # A tabxplor tab: 6 × 14
#>   sex            auburn `auburn, grey` `auburn, white`  black blond blonde brown
#>                  <row%>         <row%>          <row%> <row%> <row> <row%> <row>
#> 1 female             6%             0%              0%    19%    0%     6%   31%
#> 2 hermaphroditic     0%             0%              0%     0%    0%     0%    0%
#> 3 male               0%             2%              2%    15%    5%     0%   18%
#> 4 none               0%             0%              0%     0%    0%     0%    0%
#> 5 NA                 0%             0%              0%    25%    0%     0%   50%
#> 6 Total              1%             1%              1%    15%    3%     1%   21%
#> # ℹ 6 more variables: `brown, grey` <row%>, grey <row%>, none <row%>,
#> #   white <row%>, `NA` <row%>, Total <row% (n)>
#> # difference (Total): -30 -15 -5 +5 +15 +30 [grey: non-significant or under ±5 points]
# }
```
