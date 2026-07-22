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
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md),
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
  or
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md).

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
#> Error in dplyr::mutate(dplyr::ungroup(dplyr::mutate(dplyr::group_by(data,     !!!tab_vars), dplyr::across(tidyselect::all_of(as.character(row_vars)),     ~forcats::fct_lump_min(., other_if_less_than, other_level = other_level)))),     dplyr::across(as.character(row_vars), function(.x) forcats::fct_relevel(.x,         purrr::discard(unique(append(levels(dplyr::pull(data,             dplyr::cur_column())), other_level)), function(v) !v %in%             levels(.x))))): ℹ In argument: `dplyr::across(...)`.
#> Caused by error in `across()`:
#> ! Can't compute column `relig`.
#> Caused by error in `purrr::discard()`:
#> ℹ In index: 1.
#> Caused by error in `.fn()`:
#> ! object '.x' not found

tabs |>
  dplyr::select(year, race, relig, Married) |>
  tab_spread(race)
#> Error: object 'tabs' not found
  
```
