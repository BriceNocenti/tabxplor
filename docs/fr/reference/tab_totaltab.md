# Add total table to a [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)

**\[superseded\]**

Superseded (2.0.0): the total table is built directly by the `totaltab`
argument of
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md) /
[`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
/
[`tab_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md).
`tab_totaltab()` still works on an existing tab.

## Usage

``` r
tab_totaltab(
  tabs,
  totaltab = c("table", "line", "no"),
  name = "Ensemble",
  data = NULL
)
```

## Arguments

- tabs:

  A `tibble` of class `tab`, made with
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
  or
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

- totaltab:

  If there are subtables, corresponding to the levels of tab_vars,
  `totaltab = "table"` add a complete total table. `totaltab = "line"`
  add a total table of only one row with the general total.
  `totaltab = "no"` remove any existing total table.

- name:

  The name of the total table, as a single string.

- data:

  The original database used to calculate the `tab` : it is only useful
  for mean columns (of numeric variables), in order to calculate the
  variances necessary to calculate confidence intervals with
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md).

## Value

A `tibble` of class `tab`. Rows belonging to the total table can then be
detected using
[`is_tottab`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md).

## Examples

``` r
 data <- dplyr::starwars |>
tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
            na_drop_all = sex)

data |>
  tab_plain(sex, hair_color, gender) |>
  tab_totaltab("line")
#> # A tabxplor tab: 6 × 7
#> # Groups:         gender [3]
#>   gender    sex            black brown none Others `NA`
#>   <fct>     <fct>            <n>   <n>  <n>    <n>  <n>
#> 1 feminine  female             3     5    5      3    0
#> 2 feminine  none               0     0    1      0    0
#> 
#> 3 masculine male               9    11   29     10    1
#> 4 masculine none               0     0    2      0    3
#> 5 masculine Others             0     0    0      0    1
#> 
#> 6 Ensemble  TOTAL ENSEMBLE    12    16   37     13    5
  
```
