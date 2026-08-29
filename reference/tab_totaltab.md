# Add total table to a [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)

**\[deprecated\]**

Deprecated in 2.0.0, defunct in 2.1.0 – the total table is built
directly by the `totaltab` argument of
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).
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
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
  or [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- totaltab:

  With subtables (the levels of `tab_vars`) : `"table"` adds a complete
  total table, `"line"` a total table of a single general-total row,
  `"no"` removes any existing total table.

- name:

  The name of the total table, as a single string.

- data:

  The original database : only useful for mean columns (numeric
  variables), whose variances — needed by
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  — can only be computed from the microdata.

## Value

A `tibble` of class `tab`. Total-table rows are then detected with
[`is_tottab`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md).

## Examples

``` r
 data <- dplyr::starwars |> dplyr::filter(!is.na(sex))

data |>
  tab_plain(sex, hair_color, gender) |>
  tab_totaltab("line")
#> Warning: `tab_totaltab()` was deprecated in tabxplor 2.0.0.
#> ℹ Please use the `totaltab` argument of `tab()` instead.
#> The step-by-step chain is superseded: tab() / tab_num() compute this in one
#> pass.
#> ℹ The arithmetic is shared, so the numbers are identical -- only the chaining
#>   API goes.
#> # A tabxplor tab: 6 × 14
#> # Groups:         gender [3]
#>   gender sex            auburn `auburn, grey` `auburn, white` black blond blonde
#>                            <n>            <n>             <n>   <n>   <n>    <n>
#> 1 femin… female              1              0               0     3     0      1
#> 2 femin… none                0              0               0     0     0      0
#> 
#> 3 mascu… none                0              0               0     0     0      0
#> 4 mascu… hermaphroditic      0              0               0     0     0      0
#> 5 mascu… male                0              1               1     9     3      0
#> 
#> 6 Ensem… TOTAL ENSEMBLE      1              1               1    12     3      1
#> # ℹ 6 more variables: brown <n>, `brown, grey` <n>, grey <n>, none <n>,
#> #   white <n>, `NA` <n>
  
```
