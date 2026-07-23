# Spread a tab, passing a tab variable to column

Spread a tab, passing a tab variable to column

## Usage

``` r
tab_spread(
  tabs,
  spread_vars,
  names_prefix,
  names_sort = FALSE,
  totname = "Total"
)
```

## Arguments

- tabs:

  A `tibble` of class `tab`, made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  [`tab_many`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
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

  The new name of the total rows, as a single string.

## Value

A `tibble` of class `tab`, with less rows and more columns.

## Examples

``` r
 data <- forcats::gss_cat |> dplyr::filter(year %in% c(2000, 2014))

tabs <-
  tab(data, relig, marital, c(year, race), pct = "row", totaltab = "no",
      color = "diff", tot = "row", other_if_less_than = 30)

tabs |>
  dplyr::select(year, race, relig, Married) |>
  tab_spread(race)
#> # A tabxplor tab: 14 × 6
#> # Groups:         year [2]
#>    year  relig       Other  Black  White  Total
#>    <fct> <fct>      <row%> <row%> <row%> <row%>
#>  1 2000  Other                       31%       
#>  2 2000  None                 12%    42%       
#>  3 2000  Jewish                      49%       
#>  4 2000  Catholic      44%    20%    49%       
#>  5 2000  Protestant           32%    51%       
#>  6 2000  Others        46%    17%    47%       
#>  7 2000  TOTAL 2000    45%    28%    49%    45%
#> 
#>  8 2014  Christian            31%    49%       
#>  9 2014  None          44%    19%    39%       
#> 10 2014  Jewish                      54%       
#> 11 2014  Catholic      40%           49%       
#> 12 2014  Protestant    39%    25%    57%       
#> 13 2014  Others        51%    30%    44%       
#> 14 2014  TOTAL 2014    43%    25%    50%    46%
#> # difference (Total): -30 -15 -5 +5 +15 +30
  
```
