# Prepare data for [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md).

Prepare data for
[`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md).

## Usage

``` r
tab_prepare(
  data,
  ...,
  na_drop_all,
  cleannames = NULL,
  other_if_less_than = 0,
  other_level = "Others"
)
```

## Arguments

- data:

  A dataframe.

- ...:

  Variables then to be passed in
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md).

- na_drop_all:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Removes all observation with a `NA` in any of the chosen variables.

- cleannames:

  Set to `TRUE` to clean levels names, by removing prefix numbers like
  `"1-"`, and text in parentheses.

- other_if_less_than:

  When set to a positive integer, levels with less count than it will be
  merged into an "Others" level.

- other_level:

  The name of the "Other" level, as a character vector of length one.

## Value

A modified data.frame.

## Examples

``` r
data <- dplyr::starwars |>
tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
            na_drop_all = sex)
data
#> # A tibble: 83 × 14
#>    name     height  mass hair_color skin_color eye_color birth_year sex   gender
#>    <chr>     <int> <dbl> <fct>      <chr>      <chr>          <dbl> <fct> <fct> 
#>  1 Luke Sk…    172    77 Others     fair       blue            19   male  mascu…
#>  2 C-3PO       167    75 NA         gold       yellow         112   none  mascu…
#>  3 R2-D2        96    32 NA         white, bl… red             33   none  mascu…
#>  4 Darth V…    202   136 none       white      yellow          41.9 male  mascu…
#>  5 Leia Or…    150    49 brown      light      brown           19   fema… femin…
#>  6 Owen La…    178   120 Others     light      blue            52   male  mascu…
#>  7 Beru Wh…    165    75 brown      light      blue            47   fema… femin…
#>  8 R5-D4        97    32 NA         white, red red             NA   none  mascu…
#>  9 Biggs D…    183    84 black      light      brown           24   male  mascu…
#> 10 Obi-Wan…    182    77 Others     fair       blue-gray       57   male  mascu…
#> # ℹ 73 more rows
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>
```
