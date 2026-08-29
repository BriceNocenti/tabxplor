# Turn a sub-table variable into columns

Turns each level of a `tab_vars` variable into a **block of columns**:
fewer rows, more columns, and every column stores which sub-population
it belongs to (`col_group`) beside the variable it shows (`col_var`).
Every total row merges into ONE, named `totname` — the remaining
`tab_vars` are still index columns of their own, so the label does not
repeat them. A total *table*'s own line joins that row when no
`tab_vars` is left to hold it, and is dropped when one is.
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
`spread_vars` calls it for you, and takes care of the totals beforehand.

## Usage

``` r
tab_spread(tabs, spread_vars, names_prefix, names_sort = FALSE, totname = NULL)
```

## Arguments

- tabs:

  A `tibble` of class `tab`, made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  or
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md).

- spread_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The tab variables to pass to column, with a syntax of type
  `c(var1, var2, ...)`.

- names_prefix:

  String added to the start of every variable name.

- names_sort:

  If no `names_prefix` is given, new names takes the form
  `spread_var`\_`col_var_level`. Should then the column names be sorted
  ? If `FALSE`, the default, column names are ordered by first
  appearance.

- totname:

  The name the merged total row takes, as a single string. `NULL`
  (default) uses the one `options(tabxplor.total_names)` declares.

## Value

A `tibble` of class `tab`, with less rows and more columns.

## Examples

``` r
 data <- forcats::gss_cat |> dplyr::filter(year %in% c(2000, 2014))

tabs <-
  tab(data, relig, marital, c(year, race), pct = "row", totaltab = "no",
      color = "difference", tot = "row", other_if_less_than = 30)

tabs |>
  dplyr::select(year, race, relig, Married) |>
  tab_spread(race)
#> # A tabxplor tab: 14 × 10
#> # Groups:         year [2]
#>    year  relig      Married_Other Married_Black Married_White Married_Total
#>                            <row%>        <row%>        <row%>        <row%>
#>  1 2000  Other                                            31%              
#>  2 2000  None                               12%           42%              
#>  3 2000  Jewish                                           49%              
#>  4 2000  Catholic             44%           20%           49%              
#>  5 2000  Protestant                         32%           51%              
#>  6 2000  Others               46%           17%           47%              
#>  7 2000  Total                45%           28%           49%           45%
#> 
#>  8 2014  Christian                          31%           49%              
#>  9 2014  None                 44%           19%           39%              
#> 10 2014  Jewish                                           54%              
#> 11 2014  Catholic             40%                         49%              
#> 12 2014  Protestant           39%           25%           57%              
#> 13 2014  Others               51%           30%           44%              
#> 14 2014  Total                43%           25%           50%           46%
#> # ℹ 4 more variables: n_Other <n>, n_Black <n>, n_White <n>, n_Total <n>
#> # difference (Total): -30 -15 -5 +5 +15 +30
  
```
