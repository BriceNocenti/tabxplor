# Means table

**\[superseded\]**

Cross categorical variables with numeric ones, and get a table of means.
Superseded by
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
which builds the same table whenever `col_vars` holds numeric variables
– and everything around it (both kinds of variable at once, colours,
totals, tests). It stays the smallest entry point into the numeric
aggregate core, and takes the same arguments resolved by the same rules,
so its numbers agree with
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
cell for cell.

## Usage

``` r
tab_num(
  data,
  row_var,
  col_vars,
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

- row_var:

  **\[deprecated\]** Singular aliases of `row_vars`/`col_vars` (which
  now accept several variables). Kept working.

- col_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The column variable(s) — see `row_vars`. **An interaction** is written
  `a*b`, as in
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
  and only `col_vars` takes one: two factors give one column per
  observed cell of the pair, a number crossed with a factor one mean
  column per level. See
  [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md).

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
  `color`, `ci`, `tot`, `digits`, ... – passed by name. See
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
data <- dplyr::storms |> dplyr::filter(!is.na(wind))
tab_num(data, category, wind, tot = "row",
        color = "difference", color_signif = "guaranteed_effect")
#> # A tabxplor tab: 7 × 2
#>   category         wind
#>             <mean (cv)>
#> 1 1         71 (cv  8%)
#> 2 2         89 (cv  4%)
#> 3 3        104 (cv  4%)
#> 4 4        122 (cv  5%)
#> 5 5        146 (cv  4%)
#> 6 NA        38 (cv 31%)
#> 7 Total     50 (cv 51%)
#> # standardized difference (Total): -0.4 -0.2 -0.1 -0 +0 +0.1 +0.2 +0.4 [all that is significant is colored, error-adjusted]
# }
```
