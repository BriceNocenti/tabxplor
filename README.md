
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tablr

<!-- badges: start -->
<!-- badges: end -->

If R makes complex things simple, it can sometimes make simple things
difficult. This is why tablr try to make it easy to deal with multiple
crosstabs: to create, manipulate, color and export tabs and lists of
tabs to Excel. All functions are tidyverse-propelled, render tibbles,
and are pipe-friendly. In development.

## Installation

You can install tablr from github with:

``` r
# install.packages("devtools")
devtools::install_github("BriceNocenti/tablr")
```

## Usage

The main functions are made to be user-friendly and time-saving is data
analysis workflows.

`tab` make a simple crosstab:

``` r
library(tablr)
tab(forcats::gss_cat, marital, race)
#>   marital         Other Black White Total
#>   <fct>           <dbl> <dbl> <dbl> <dbl>
#> 1 "No answer"         2     2    13    17
#> 2 "Never married"   633  1305  3478  5416
#> 3 "Separated"       110   196   437   743
#> 4 "Divorced"        212   495  2676  3383
#> 5 "Widowed"          70   262  1475  1807
#> 6 "Married"         932   869  8316 10117
#> 7 "Total "         1959  3129 16395 21483
```

It comes with options to weight the table, print percentages and totals,
manage missing values, sort lines or calculate Chi2  results (excepted
frequencies, contributions of cells to variance, etc.).

``` r
tab(forcats::gss_cat, marital, race, perc = "row", tot = "row", show_na = FALSE, keep_unused_levels = TRUE)
```

When providing a third variable, `tab` makes a list with as many
crosstabs as it has levels:

``` r
tab(forcats::gss_cat, marital, race, year, perc = "row")
```

`tab_sup` adds supplementary rows or colums (when numeric it calculates
means by category):

``` r
dplyr::storms %>%
  tab(status, category) %>%
  tab_sup(sup_rows = c("pressure", "wind"), print_sup = TRUE)
```

`tab_xl` exports tabs or lists of tabs to Excel, where options can color
cells to show deviations from mean or contribution to variance:

``` r
forcats::gss_cat %>%
  tab(marital, race, perc = "row") %>%
  tab_xl(color = "contrib")
```

`tab_multi` crosses one dependent variable with several explanatory
variables (only keeping their first levels or printing them all) :

``` r
tab_multi(dplyr::storms, category, explanatory_vars = c("pressure", "wind"))
```

Using `tab` with `purrr::map`, you can program several tables with
different parameters at once:

``` r
purrr::pmap(
  tibble::tribble(
    ~var1    , ~var2 ,  ~perc,
    "marital", "race",  "no" ,
    "marital", "race",  "row",
    "marital", "race",  "col",
    "relig"  , "race",  "no" ,
    "relig"  , "race",  "row",
    "relig"  , "race",  "col",
  ),
  .f = tab,
  data = forcats::gss_cat, sort_by = c("White", "desc")) #%>%
#tab_xl(only_one_sheet = TRUE)
```

When not doing data analysis but writing functions, you can use the
sub-functions of `tab` step by step to attain more flexibility or speed.
It uses a three-steps process : prepare the data ; calculate all the
needeed informations in a single “long” dataframe ; draw the list of
tables. At this stage of development it may not be so intuitive to use,
but I’m trying to improve that.

``` r
forcats::gss_cat %>%
  tab_prepare(marital, relig, race, rare_to_other = TRUE) %>%
  tab_df(marital, relig, race, perc = "col") %>%
  tab_draw(totaltab = "line")
```
