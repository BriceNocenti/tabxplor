# Bind a list of tables into one

Bind a list of tables into one

## Usage

``` r
tab_compact(tabs)
```

## Arguments

- tabs:

  A `list` of `tabxplor_tab` (or a `tabxplor_tab`)

## Value

A `tabxplor_tab`

## Examples

``` r
# \donttest{
forcats::gss_cat |>
  tab(c(race, rincome), marital, pct = "row", color = "difference", output_list = TRUE) |>
  tab_compact()
#> # A tabxplor tab: 21 × 9
#> # Groups:         row_var [2]
#>    row_var levels         `No answer` `Never married` Separated Divorced Widowed
#>                                <row%>          <row%>    <row%>   <row%>  <row%>
#>  1 race    Other                   0%             32%        6%      11%      4%
#>  2 race    Black                   0%             42%        6%      16%      8%
#>  3 race    White                   0%             21%        3%      16%      9%
#>  4 race    Total                   0%             25%        3%      16%      8%
#> 
#>  5 rincome No answer               3%             25%        2%      21%      5%
#>  6 rincome Don't know              0%             25%        4%      12%      8%
#>  7 rincome Refused                 0%             19%        4%      14%      5%
#>  8 rincome $25000 or more          0%             22%        3%      18%      2%
#>  9 rincome $20000 - 24999          0%             30%        4%      18%      3%
#> 10 rincome $15000 - 19999          0%             33%        4%      17%      3%
#> 11 rincome $10000 - 14999          0%             33%        5%      16%      5%
#> 12 rincome $8000 to 9999           0%             40%        6%      12%      5%
#> 13 rincome $7000 to 7999           0%             45%        2%      10%      7%
#> 14 rincome $6000 to 6999           0%             38%        5%      13%      4%
#> 15 rincome $5000 to 5999           0%             41%        5%      10%      5%
#> 16 rincome $4000 to 4999           0%             42%        5%      11%      4%
#> 17 rincome $3000 to 3999           0%             45%        6%      11%      4%
#> 18 rincome $1000 to 2999           0%             50%        5%       9%      3%
#> 19 rincome Lt $1000                0%             43%        3%      12%      5%
#> 20 rincome Not applicable          0%             21%        3%      14%     19%
#> 21 rincome Total                   0%             25%        3%      16%      8%
#> # ℹ 2 more variables: Married <row%>, Total <row% (n)>
#> # difference (Total): -30 -15 -5 +5 +15 +30
# }
```
