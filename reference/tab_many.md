# Many cross-tables as one, with color helpers

**\[superseded\]**

Superseded (2.0.0) by
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md), the
unified entry point: it accepts several `row_vars` / `col_vars` and
merges them into one table by default (`output_list = TRUE` gives the
list shape `tab_many()` returns).

`tab_many()` forwards everything to
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
translating the five renamed arguments:

|  |  |
|----|----|
| `tab_many()` | [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) |
| `chi2 = TRUE` | `test = TRUE` |
| `totrow = FALSE` | `tot = "col"` |
| `totcol = "no"` | `tot = "row"` |
| `compact = TRUE` | `output_list = FALSE` |
| `na_drop_all = c(a, b)` | `filter = !is.na(a) & !is.na(b)` |

Everything else keeps its name and meaning — see
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

## Usage

``` r
tab_many(
  data,
  row_vars,
  col_vars,
  tab_vars,
  wt,
  ...,
  chi2,
  totrow,
  totcol,
  compact,
  na_drop_all,
  filter
)
```

## Arguments

- data:

  A data frame, or a
  [`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html).

- row_vars, col_vars, tab_vars, wt:

  The variable roles — see
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).
  With `data`, the only arguments that may be passed by position:
  everything else must be named, because
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  argument order differs.

- ...:

  Passed on to
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- chi2:

  **\[deprecated\]** Use
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  `test`.

- totrow, totcol:

  **\[deprecated\]** Use
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  `tot`. A total row is always computed and exactly one total column is
  shown, so both are cosmetic; `totcol = "each"` and `"all_col_vars"`
  now give that same single total column instead of erroring.

- compact:

  **\[deprecated\]** Use
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  `output_list` (inverted).

- na_drop_all:

  **\[deprecated\]**
  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Use
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  `filter`: `na_drop_all = c(a, b)` is `filter = !is.na(a) & !is.na(b)`.

- filter:

  **\[superseded\]** A
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  to apply to the data frame first — see
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).
  Prefer filtering upstream.

## Value

What [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
returns: a `tabxplor_tab` (a `tabxplor_grouped_tab` with `tab_vars`), or
a `tabxplor_tabs` list under `output_list = TRUE` / `compact = FALSE`.

## Examples

``` r
# Make a summary table with many col_vars, showing only one specific level :
# \donttest{
library(dplyr)
first_lvs <- c("Married", "$25000 or more", "Strong republican", "Protestant")
data <- forcats::gss_cat |> mutate(across(
  where(is.factor),
  ~ forcats::fct_relevel(., first_lvs[first_lvs %in% levels(.)])
))
tab_many(data, race, c(marital, rincome, partyid, relig, age, tvhours),
         levels = "first", pct = "row", test = TRUE, color = "auto")
#> Warning: `tab_many()` was deprecated in tabxplor 2.0.0.
#> ℹ Please use `tab()` instead.
#> ℹ tab() accepts several row_vars / col_vars. It merges >=2 row_vars into one
#>   table by default; pass output_list = TRUE for a list (tab_many()'s old
#>   default).
#> |      | Tests                  |  marital |   |  rincome |   |  partyid |   |    relig |   |         age |   |     tvhours |
#> |:-----|:-----------------------|---------:|:-:|---------:|:-:|---------:|:-:|---------:|:-:|------------:|:-:|------------:|
#> | race | N                      |   21 483 |   |   21 483 |   |   21 483 |   |   21 483 |   |      21 407 |   |      11 337 |
#> |      | pvalue (Chi2, Welch F) |   <0.01% |   |   <0.01% |   |   <0.01% |   |   <0.01% |   |      <0.01% |   |      <0.01% |
#> |      | Cramér's V, eta2       | V = 0.16 |   | V = 0.06 |   | V = 0.15 |   | V = 0.25 |   | eta2 = 0.03 |   | eta2 = 0.04 |
#> 
#> # A tabxplor tab: 4 × 8
#>   race               n Married `$25000 or more` `Strong republican` Protestant
#>              <n_range>  <row%>           <row%>              <row%>     <row%>
#> 1 Other   1 027- 1 959     48%              32%                  4%        20%
#> 2 Black   1 700- 3 129     28%              28%                  2%        73%
#> 3 White   8 610-16 395     51%              36%                 13%        50%
#> 4 Total  11 337-21 483     47%              34%                 11%        50%
#> # ℹ 2 more variables: age <mean (cv)>, tvhours <mean (cv)>
#> # marital, rincome, partyid, relig: difference (Total): -30 -15 -5 +5 +15 +30; bg ratio: ×2
#> # age, tvhours: ratio (Total): ÷4 ÷2 ÷1.15 ×1.15 ×2 ×4
# }

# Can be used with map and tribble to program several tables with different parameters
#  all at once, in a readable way:
# \donttest{
library(purrr)
library(tibble)
pmap(
  tribble(
    ~row_vars, ~col_vars      , ~pct , ~filter              , ~subtext               ,
    "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
    "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
    NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
  ),
  .f = tab_many,
  data = forcats::gss_cat, color = "auto", test = TRUE)
#> [[1]]
#> |      | Tests           |  marital |
#> |:-----|:----------------|---------:|
#> | race | N               |   21 483 |
#> |      | pvalue (Chi2 !) |   <0.01% |
#> |      | Cramér's V      | V = 0.15 |
#> 
#> # A tabxplor tab: 4 × 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>               <row%>          <row%>    <row%>   <row%>  <row%>  <row%>
#> 1 Other           0%             32%        6%      11%      4%     48%
#> 2 Black           0%             42%        6%      16%      8%     28%
#> 3 White           0%             21%        3%      16%      9%     51%
#> 4 Total           0%             25%        3%      16%      8%     47%
#> # ℹ 1 more variable: Total <row% (n)>
#> # difference (Total): -30 -15 -5 +5 +15 +30; bg ratio: ×2
#> # Source: GSS 2000-2014
#> 
#> [[2]]
#> |       | Tests                    |     race |   |         age |
#> |:------|:-------------------------|---------:|:-:|------------:|
#> | relig | N                        |   16 971 |   |      16 909 |
#> |       | pvalue (Chi2 !, Welch F) |   <0.01% |   |      <0.01% |
#> |       | Cramér's V, eta2         | V = 0.26 |   | eta2 = 0.04 |
#> 
#> # A tabxplor tab: 16 × 6
#>    relig                    Other  Black  White                Total         age
#>                            <row%> <row%> <row%>     <row% (n_range)> <mean (cv)>
#>  1 No answer                  12%    17%    71% 100% (    70-    76) 47 (cv 32%)
#>  2 Don't know                 29%    29%    43% 100% (            7) 35 (cv 26%)
#>  3 Inter-nondenominational     2%    26%    72% 100% (          103) 40 (cv 35%)
#>  4 Native american            67%     0%    33% 100% (           15) 40 (cv 35%)
#>  5 Christian                  12%    18%    70% 100% (   433-   435) 39 (cv 32%)
#>  6 Orthodox-christian          1%     1%    98% 100% (           80) 49 (cv 31%)
#>  7 Moslem/islam               41%    34%    24% 100% (           82) 36 (cv 29%)
#>  8 Other eastern              21%     8%    71% 100% (           24) 47 (cv 31%)
#>  9 Hinduism                   84%     2%    14% 100% (    48-    49) 38 (cv 34%)
#> 10 Buddhism                   54%     7%    39% 100% (          115) 44 (cv 38%)
#> 11 Other                      11%    10%    79% 100% (          172) 41 (cv 35%)
#> 12 None                        9%    11%    81% 100% ( 2 607- 2 614) 41 (cv 38%)
#> 13 Jewish                      2%     2%    96% 100% (   319-   320) 52 (cv 35%)
#> 14 Catholic                   18%     4%    78% 100% ( 4 055- 4 074) 46 (cv 37%)
#> 15 Protestant                  3%    21%    76% 100% ( 8 779- 8 805) 49 (cv 35%)
#> 16 Total                       9%    14%    77% 100% (16 909-16 971) 47 (cv 37%)
#> # race: difference (Total): -30 -15 -5 +5 +15 +30; bg ratio: ×2
#> # age: ratio (Total): ÷4 ÷2 ÷1.15 ×1.15 ×2 ×4
#> # Source: GSS 2000-2010
#> 
#> [[3]]
#> # A tabxplor tab: 2 × 5
#>   no_row_var Other Black  White  Total
#>                <n>   <n>    <n>    <n>
#> 1 no_row_var 1 959 3 129 16 395 21 483
#> 2 Total      1 959 3 129 16 395 21 483
#> # Source: GSS 2000-2014
#> 
# }
```
