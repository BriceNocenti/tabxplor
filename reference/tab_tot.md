# Add totals to a [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)

**\[deprecated\]**

Deprecated in 2.0.0, defunct in 2.1.0 – totals are built directly by
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) (a
total row is always computed, one total column shown). `tab_tot()` still
works on an existing tab.

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
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
  or [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- tot:

  `c("col", "row")` or `"both"` print total rows and total columns ;
  `"row"` or `"col"` print only one type ; `"no"` removes all totals.

- name:

  The names of the totals, as a character vector of length one or two
  (`c("Total_row", "Total_column")` to name rows and cols differently).

- totcol:

  `"last"` prints a total column for the last factor column variable
  only ; `"each"` prints one for each column variable.

- data:

  The original database : only useful for mean columns, whose total-row
  variances — needed by
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  — can only be computed from the microdata.

## Value

A `tibble` of class `tab`. Total rows are then detected with
[`is_totrow`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md),
and total columns with
[`is_totcol`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md).

## Examples

``` r
data <- dplyr::starwars

data |>
  tab_plain(sex, hair_color) |>
  tab_tot("col", totcol = "each")
#> Warning: `tab_tot()` was deprecated in tabxplor 2.0.0.
#> ℹ Please use the `tot` argument of `tab()` instead.
#> The step-by-step chain is superseded: tab() / tab_num() compute this in one
#> pass.
#> ℹ The arithmetic is shared, so the numbers are identical -- only the chaining
#>   API goes.
#> # A tabxplor tab: 5 × 14
#>   sex            auburn `auburn, grey` `auburn, white` black blond blonde brown
#>                     <n>            <n>             <n>   <n>   <n>    <n>   <n>
#> 1 female              1              0               0     3     0      1     5
#> 2 hermaphroditic      0              0               0     0     0      0     0
#> 3 male                0              1               1     9     3      0    11
#> 4 none                0              0               0     0     0      0     0
#> 5 NA                  0              0               0     1     0      0     2
#> # ℹ 6 more variables: `brown, grey` <n>, grey <n>, none <n>, white <n>,
#> #   `NA` <n>, Total_hair_color <n>
  
```
