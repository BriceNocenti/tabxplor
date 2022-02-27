
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabxplor

<!-- badges: start -->

[![R-CMD-check](https://github.com/BriceNocenti/tabxplor/workflows/R-CMD-check/badge.svg)](https://github.com/BriceNocenti/tabxplor/actions)
<!-- badges: end -->

If R makes complex things simple, it can sometimes make simple things
difficult. This is why `tabxplor` tries to make it easy to deal with
multiple cross-tables: to create and manipulate them, but also to read
them, using color helpers to highlight important informations. It would
love to enhance your data exploration experience with simple yet
powerful tools. All functions are propelled by `tidyverse`,
pipe-friendly, and render `tibble` data frames which can be easily
manipulated with `dplyr`. Tables can be exported to Excel and in html
with formats and colors.

## Installation

You can install tabxplor from CRAN with:

``` r
install.packages("tabxplor")
```

Or from github with :

``` r
# install.packages(devtools)
devtools::install_github("BriceNocenti/tabxplor")
```

## Base usage: cross-tables with color helpers

The main functions are made to be user-friendly and time-saving is data
analysis workflows.

`tab` makes a simple cross-table:

``` r
library(tabxplor)
tab(forcats::gss_cat, marital, race)
#> Warning: data frame results in `filter()` are deprecated, use `if_any()` or
#> `if_all()`.

#> Warning: data frame results in `filter()` are deprecated, use `if_any()` or
#> `if_all()`.
#> # A tabxplor tab: 7 x 5
#>   marital       Other Black  White  Total
#>   <fct>           <n>   <n>    <n>    <n>
#> 1 No answer         2     2     13     17
#> 2 Never married   633 1 305  3 478  5 416
#> 3 Separated       110   196    437    743
#> 4 Divorced        212   495  2 676  3 383
#> 5 Widowed          70   262  1 475  1 807
#> 6 Married         932   869  8 316 10 117
#> 7 Total         1 959 3 129 16 395 21 483
```

When one of the row or column variables is numeric, `tab` calculates
means by category of the other variable.

`tab` comes with options to weight the table, print percentages, manage
totals, digits and missing values, add legends, gather rare categories
in a “Others” level.

``` r
tab(forcats::gss_cat, marital, race, pct = "row", na = "drop", subtext = gss,
rare_to_other = TRUE, n_min = 1000, other_level = "Custom_other_level_name")
```

When a third variable is provided, `tab` makes a table with as many
subtables as it has levels. With several `tab_vars`, it makes a subtable
for each combination of their levels. The result is grouped: in dplyr,
operations like `sum()` or `all()` are done within each subtable, and
not for the whole dataframe.

Colors may be added to highlight over-represented and under-represented
cells, and therefore help the user read the table. By default, with
`color = "diff"`, colors are based on the differences between a cell and
it’s related total (which only works with means and row or col pct).
When a percentage is superior to the average percentage of the line or
column, it appears with shades of green (or blue). When it’s inferior,
it appears with shades of red/orange. A color legend is added below the
table. In RStudio colors are adapted to the theme, light or dark.

``` r
data <- forcats::gss_cat %>% 
  dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
gss  <- "Source: General social survey 2000-2014"
gss2 <- "Source: General social survey 2000, 2006 and 2012"
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff")
```

![](.readme_images/tabxplor1_group.jpg)

The `sup_cols` argument adds supplementary column variables to the
table. With numeric variables, it calculates the mean for each category
or the row variable. With text variables, only the first level is kept
(you can choose which one to use by placing it first with
`forcats::fct_relevel`). Use `tab_many` to keep all levels.

``` r
tab(dplyr::storms, category, status, sup_cols = c("pressure", "wind"))
#> Warning: data frame results in `filter()` are deprecated, use `if_any()` or
#> `if_all()`.

#> Warning: data frame results in `filter()` are deprecated, use `if_any()` or
#> `if_all()`.
#> # A tabxplor tab: 8 x 7
#>   category hurricane `tropical depressi~` `tropical storm`  Total pressure  wind
#>   <fct>    <n-mixed>            <n-mixed>        <n-mixed> <n-mi>   <mean> <mea>
#> 1 -1               0                2 898                0  2 898    1 008    27
#> 2 0                0                    0            5 347  5 347      999    46
#> 3 1            1 933                    0                1  1 934      981    71
#> 4 2              749                    0                0    749      967    89
#> 5 3              434                    0                0    434      954   104
#> 6 4              411                    0                0    411      939   122
#> 7 5               86                    0                0     86      917   146
#> 8 Total        3 613                2 898            5 348 11 859      992    54
```

## References and comparison levels for colors

By default, to calculate colors, each cell is compared to the subtable’s
related total.

When a third variable or more are provided, it’s possible to compare
with the general total line instead, by setting `comp = "all"`. Here,
only the last total row is highlighted (TOTAL ENSEMBLE appears in white
but other total rows in grey).

``` r
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff", comp = "all")
```

![](.readme_images/tabxplor2_comp_all.jpg)

With `diff = "first"`, each row (or column) is compared to the first row
(or column), which is particularly helpful to highlight historical
evolutions. The first rows then appears in white (while totals are
themselves colored like normal lines).

``` r
data <- data %>% dplyr::mutate(year = as.factor(year))
tab(data, year, marital, race, pct = "row", color = "diff", diff = "first", tot = "col",
    totaltab = "table")
```

![](.readme_images/tabxplor3_first.jpg)

## Confidence intervals

It it possible to print confidence intervals for each cell:

``` r
tab(forcats::gss_cat, race, marital, pct = "row", ci = "cell")
#> Warning: data frame results in `filter()` are deprecated, use `if_any()` or
#> `if_all()`.

#> Warning: data frame results in `filter()` are deprecated, use `if_any()` or
#> `if_all()`.
#> # A tabxplor tab: 4 x 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married  Total
#>   <fct>       <row%>          <row%>    <row%>   <row%>  <row%>  <row%> <row%>
#> 1 Other       0%±0.3         32%±2.1    6%±1.1  11%±1.5  4%±0.9 48%±2.2   100%
#> 2 Black       0%±0.2         42%±1.7    6%±0.9  16%±1.3  8%±1.0 28%±1.6   100%
#> 3 White       0%±0.1         21%±0.6    3%±0.3  16%±0.6  9%±0.4 51%±0.8   100%
#> 4 Total       0%             25%        3%      16%      8%     47%       100%
```

It is also possible to use confidence intervals to enhance colors
helpers. With `color = "diff_ci"`, the cells are only colored if the
confidence interval of the difference between them and their reference
cell (in total or first row/col) is superior to the difference itself.
Otherwise, it means the cell is not significantly different from it’s
reference in the total (or first) row: it turns grey, and the reader is
not anymore tempted to over-interpret the difference.

``` r
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff_ci")
```

![](.readme_images/tabxplor4_diff_ci.jpg)

Finally, another calculation appears helpful: the difference between the
cell and the total, minus the confidence interval of this difference (or
in other word, what remains of that difference after having subtracted
the confidence interval). `ci = "after_ci"` highligths all the cells
whose value is significantly different from the relative total (or first
cell). This is particularly useful when working on small populations: we
can see at a glance which numbers we have right to read and interpret.

``` r
tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "after_ci")
```

![](.readme_images/tabxplor5_after_ci.jpg)

## Chi2 stats and contributions of cells to variance

`chi2 = TRUE` add summary statistics made in the chi2 metric: degrees of
freedom (df), unweighted count, pvalue and (sub)table’s variance. Chi2
pvalue is colored in green when inferior to 5%, and in red when superior
or equal to 5%, meaning that the table is not significantly different
from the independent hypothesis (the two variables may be independent).

``` r
tab(forcats::gss_cat, race, marital, chi2 = TRUE)
#> Warning: data frame results in `filter()` are deprecated, use `if_any()` or
#> `if_all()`.

#> Warning: data frame results in `filter()` are deprecated, use `if_any()` or
#> `if_all()`.
#> chi2 stats     marital
#> df                  12
#> variance        0.0464
#> pvalue              0%
#> count           21 483
#> 
#> # A tabxplor tab: 4 x 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married  Total
#>   <fct>          <n>             <n>       <n>      <n>     <n>     <n>    <n>
#> 1 Other            2             633       110      212      70     932  1 959
#> 2 Black            2           1 305       196      495     262     869  3 129
#> 3 White           13           3 478       437    2 676   1 475   8 316 16 395
#> 4 Total           17           5 416       743    3 383   1 807  10 117 21 483
```

Chi2 stats can also be used to color cells based on their contributions
to the variance of the (sub)table, with `color = "contrib"`. By default,
only the cells whose contribution is superior to the mean contribution
are colored. It highlights the cells which would stand out in a
correspondence analysis (the two related categories would be located at
the edges of the first axes ; here, being black is associated with never
married and being separated).

``` r
tab(forcats::gss_cat, race, marital, color = "contrib")
```

![](.readme_images/tabxplor6_contrib.jpg)

## Combine `tabxplor` and `dplyr`

The result of `tab` is a `tibble::tibble` data frame with class `tab`.
It gets it’s own printing methods but, in the same time, can be
transformed using most `dplyr` verbs, like a normal `tibble`.

``` r
library(dplyr)
tab(storms, category, status, sup_cols = c("pressure", "wind")) %>%
  filter(category != "-1") %>%
dplyr::select(-`tropical depression`)
  arrange(is_totrow(.), desc(category)) # use is_totrow to keep total rows
```

With `dplyr::arrange`, don’t forget to keep the order of tab variables
and total rows:

``` r
tab(data, race, marital, year, pct = "row") %>%
  arrange(year, is_totrow(.), desc(Married))
```

## Draw more complex tables with `tab_many`

`tab` is a wrapper around the more powerful function `tab_many`, which
can be used to customize your tables.

It’s possible, for example, to make a summary table of as many columns
variables as you want (showing all levels, or showing only one specific
level like here):

``` r
first_lvs <- c("Married", "$25000 or more", "Strong republican", "Protestant")
data <- forcats::gss_cat %>% mutate(across(
  where(is.factor),
  ~ forcats::fct_relevel(., first_lvs[first_lvs %in% levels(.)])
))
tab_many(data, race, c(marital, rincome, partyid, relig, age, tvhours),
         levels = "first", pct = "row", chi2 = TRUE, color = "auto")
```

![](.readme_images/tabxplor7_tab_many.jpg)

Using `tab` or `tab_many` with `purrr::map` and `tibble::tribble`, you
can program several tables with different parameters all at once, in a
readable way:

``` r
tabs <-
  purrr::pmap(
    tibble::tribble(
      ~row_var, ~col_vars       , ~pct , ~filter              , ~subtext               ,
      "race"  , "marital"       , "no" , NULL                 , "Source: GSS 2000-2014",
      "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
      "race"  , "marital"       , "col", NULL                 , "Source: GSS 2000-2014",
      "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
      "relig" , c("race", "age"), "row", "year %in% 2010:2014", "Source: GSS 2010-2014",
      NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
    ),
    .f = tab_many,
    data = forcats::gss_cat, color = "auto", chi2 = TRUE)
```

## Export to html or Excel

To export a table to html with colors, tabxplor uses `knitr::kable` and
`kableExtra`. In this format differences from totals, confidence
intervals, contribution to variance, and unweighted counts, are
available in a tooltip at cells hover.

``` r
tabs <- tab(forcats::gss_cat, race, marital, subtext = "Source: GSS 2000-2014", 
            pct = "row", color = "diff")
tabs %>% tab_kable()
```

![](.readme_images/tabxplor8_tab_kable.jpg)

To print an html table by default (for example, in RStudio viewer), use
tabxplor options:

``` r
options(tabxplor.print = "kable") # default to options(tabxplor.print = "console")
```

`tab_xl` exports any table or list of tables to Excel, with all colors,
chi2 stats and formatting. On Excel, it is still possible to do
calculations on raw numbers.

``` r
tabs %>% tab_xl(replace = TRUE, sheets = "unique")
```

## Programming with `tabxplor`

When not doing data analysis but writing functions, you can use the
sub-functions of `tab_many` step by step to attain more flexibility or
speed. That way, it’s possible to write new functions to customize your
tables even more.

``` r
data <- dplyr::starwars %>%
  tab_prepare(sex, hair_color, gender, rare_to_other = TRUE,
              n_min = 5, na = "keep")

data %>%
  tab_plain(sex, hair_color, gender) %>%
  tab_totaltab("line")  %>%
  tab_tot()  %>%
  tab_pct(comp = "all")  %>%
  tab_ci("diff", color = "after_ci")  %>%
  tab_chi2(calc = "p")
```

The whole architecture of `tabxplor` is powered by a special vector
class, named `fmt` for formatted numbers. As a `vctrs::record`, it
stores behind the scenes all the data necessary to calculate printed
results, formats and colors. A set of functions are available to access
or transform this data, like `is_totrow`, `is_totcol` or `is_tottab`.
`?fmt` to get more information.

The simple way to recover the underlying numbers as numeric vectors is
`get_num`:

``` r
tab(data, race, marital, year, pct = "row") %>%
  mutate(across(where(is_fmt), get_num))
```

To render character vectors (without colors), use `format`:

``` r
tab(data, race, marital, year, pct = "row") %>%
  mutate(across(where(is_fmt), format))
```

For the simplest table, with only numeric counts (no `fmt`), or even as
normal data.frame (not a `tibble`):

``` r
# combine with `tab_prepare` to handle missing values
tab_plain(data, race, marital, num = TRUE) # counts as numeric vector
tab_plain(data, race, marital, df = TRUE)  # same, with unique class = "data.frame"
```
