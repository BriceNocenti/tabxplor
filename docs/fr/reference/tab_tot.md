# Add totals to a [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)

**\[superseded\]**

Superseded (1.4.0): totals are built directly by
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md) /
[`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
/
[`tab_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md)
(a total row is always computed, one total column shown). `tab_tot()`
still works on an existing tab.

## Usage

``` r
tab_tot(
  tabs,
  tot = c("row", "col"),
  name = "Total",
  totcol = "last",
  data = NULL
)
```

## Arguments

- tabs:

  A `tibble` of class `tab`, made with
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
  or
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

- tot:

  `c("col", "row")` and `"both"` print total rows and total columns. Set
  to `"row"` or `"col"` to print only one type. Set to `"no"` to remove
  all totals.

- name:

  The names of the totals, as a character vector of length one or two.
  Use `c("Total_row", "Total_column")` to set different names for rows
  and cols.

- totcol:

  `"last"` only prints a total column for the last factor column
  variable. Set to `"each"` to print a total column for each column
  variable.

- data:

  The original database used to calculate the `tab` : it is only useful
  for mean columns (of numeric variables), in order to calculate the
  variances of total rows, necessary to calculate confidence intervals
  with
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md).

## Value

A `tibble` of class `tab`. Total rows can then be detected using
[`is_totrow`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md),
and total columns using
[`is_totcol`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md).

## Examples

``` r
data <- dplyr::starwars |> tab_prepare(sex, hair_color)

data |>
  tab_plain(sex, hair_color) |>
  tab_tot("col", totcol = "each")
#> # A tabxplor tab: 5 × 14
#>   sex            auburn `auburn, grey` `auburn, white` black blond blonde brown
#>   <fct>             <n>            <n>             <n>   <n>   <n>    <n>   <n>
#> 1 female              1              0               0     3     0      1     5
#> 2 hermaphroditic      0              0               0     0     0      0     0
#> 3 male                0              1               1     9     3      0    11
#> 4 none                0              0               0     0     0      0     0
#> 5 NA                  0              0               0     1     0      0     2
#> # ℹ 6 more variables: `brown, grey` <n>, grey <n>, none <n>, white <n>,
#> #   `NA` <n>, Total_hair_color <n>
  
```
