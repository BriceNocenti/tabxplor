
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabxplor

<!-- badges: start -->
<!-- badges: end -->

If R makes complex things simple, it can sometimes make simple things
difficult. This is why `tabxplor` tries to make it easy to deal with
multiple crosstabs: to create and manipulate them, but also to read
them, using color helpers to highlight the important information. It
will to enhance your data exploration experience with simple yet
powerful tools. All functions are tidyverse-propelled, pipe-friendly,
and render tibbles, which can be easily modified using `dplyr`. Tables
can be exported to Excel with formats and colors.

## Installation

You can install tabxplor from github with:

``` r
# install.packages("devtools")
devtools::install_github("BriceNocenti/tabxplor")
```

## Base usage: crosstables with color helpers

The main functions are made to be user-friendly and time-saving is data
analysis workflows.

`tab` makes a simple crosstable:

``` r
library(tabxplor)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
tab(forcats::gss_cat, marital, race)
#> # A tabxplor tab: 7 x 5
#>   marital        Other  Black  White Total_race
#>   <char>        <n-wn> <n-wn> <n-wn>     <n-wn>
#> 1 No answer          2      2     13         17
#> 2 Never married    633  1 305  3 478      5 416
#> 3 Separated        110    196    437        743
#> 4 Divorced         212    495  2 676      3 383
#> 5 Widowed           70    262  1 475      1 807
#> 6 Married          932    869  8 316     10 117
#> 7 Total          1 959  3 129 16 395     21 483
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
operations like sum() or all() are done within each subtable, and not
for the whole dataframe.

Colors may be added to highlight over-represented and under-represented
cells, and therefore help the user read the table. By default, with
`color = "diff"`, colors are based on the differences between a cell and
it’s related total (which only works with means and row or col pct).
When a percentage is superior to the average percentage of the line or
column, it appears with shades of green. When it’s inferior, it appears
with shades of red/orange. A color legend is added below the table. Note
that, for now, colors are made to stand out with dark themes in RStudio,
not ligth ones.

``` r
data <- forcats::gss_cat %>% 
  dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
gss  <- "Source: General social survey 2000-2014"
gss2 <- "Source: General social survey 2000, 2006 and 2012"
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff")
#> # A tabxplor tab: 13 x 7
#> # Groups:         year [4]
#>    year     race           `Never married` Separated Divorced Married Total_marital
#>    <fct>    <fct>                   <row%>    <row%>   <row%>  <row%>        <row%>
#>  1 2000     Other                      36%        5%      12%     47%          100%
#>  2 2000     Black                      41%       11%      16%     32%          100%
#>  3 2000     White                      25%        3%      18%     54%          100%
#>  4 2000     TOTAL 2000                 28%        4%      17%     50%          100%
#> 
#>  5 2006     Other                      29%        7%      11%     53%          100%
#>  6 2006     Black                      47%        6%      18%     30%          100%
#>  7 2006     White                      22%        3%      19%     57%          100%
#>  8 2006     TOTAL 2006                 26%        4%      18%     52%          100%
#> 
#>  9 2012     Other                      37%        6%       7%     51%          100%
#> 10 2012     Black                      43%        5%      21%     31%          100%
#> 11 2012     White                      25%        3%      18%     53%          100%
#> 12 2012     TOTAL 2012                 29%        4%      18%     50%          100%
#> 
#> 13 Ensemble TOTAL ENSEMBLE             27%        4%      18%     51%          100%
#> # marital: x > tot +5%; +10%; +20%; +30%; x < tot -5%; -10%; -20%; -30%
#> # Source: General social survey 2000, 2006 and 2012
```

The `sup_cols` argument adds supplementary column variables to the
table. With numeric variables, it calculates the mean for each category
or the row variable. With text variables, only the first level is kept
(you can choose which one to use by placing it first with
`forcats::fct_relevel`). Use `tab_many` to keep all levels.

``` r
tab(storms, category, status, sup_cols = c("pressure", "wind"))
#> # A tabxplor tab: 8 x 7
#>   category `tropical depressio~ `tropical storm` hurricane Total_status pressure
#>   <char>                 <n-wn>           <n-wn>    <n-wn>       <n-wn>   <mean>
#> 1 -1                      2 545                0         0        2 545    1 008
#> 2 0                           0            4 373         0        4 373      999
#> 3 1                           0                1     1 684        1 685      982
#> 4 2                           0                0       628          628      967
#> 5 3                           0                0       363          363      954
#> 6 4                           0                0       348          348      940
#> 7 5                           0                0        68           68      916
#> 8 Total                   2 545            4 374     3 091       10 010      992
#> # ... with 1 more variable: wind <mean>
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
#> # A tabxplor tab: 13 x 7
#> # Groups:         year [4]
#>    year     race           `Never married` Separated Divorced Married Total_marital
#>    <fct>    <fct>                   <row%>    <row%>   <row%>  <row%>        <row%>
#>  1 2000     Other                      36%        5%      12%     47%          100%
#>  2 2000     Black                      41%       11%      16%     32%          100%
#>  3 2000     White                      25%        3%      18%     54%          100%
#>  4 2000     TOTAL 2000                 28%        4%      17%     50%          100%
#> 
#>  5 2006     Other                      29%        7%      11%     53%          100%
#>  6 2006     Black                      47%        6%      18%     30%          100%
#>  7 2006     White                      22%        3%      19%     57%          100%
#>  8 2006     TOTAL 2006                 26%        4%      18%     52%          100%
#> 
#>  9 2012     Other                      37%        6%       7%     51%          100%
#> 10 2012     Black                      43%        5%      21%     31%          100%
#> 11 2012     White                      25%        3%      18%     53%          100%
#> 12 2012     TOTAL 2012                 29%        4%      18%     50%          100%
#> 
#> 13 Ensemble TOTAL ENSEMBLE             27%        4%      18%     51%          100%
#> # marital: x > tot +5%; +10%; +20%; +30%; x < tot -5%; -10%; -20%; -30%
#> # Source: General social survey 2000, 2006 and 2012
```

With `diff = "first"`, each row (or column) is compared to the first row
(or column), which is particularly helpful to highlight historical
evolutions. The first rows then appears in white (while totals are
themselves colored like normal lines).

``` r
data <- data %>% dplyr::mutate(year = as.factor(year))
tab(data, year, marital, race, pct = "row", color = "diff", diff = "first", tot = "col",
    totaltab = "table")
#> # A tabxplor tab: 12 x 7
#> # Groups:         race [4]
#>    race     year   `Never married` Separated Divorced Married Total_marital
#>    <fct>    <fct>           <row%>    <row%>   <row%>  <row%>        <row%>
#>  1 Other    2000               36%        5%      12%     47%          100%
#>  2 Other    2006               29%        7%      11%     53%          100%
#>  3 Other    2012               37%        6%       7%     51%          100%
#> 
#>  4 Black    2000               41%       11%      16%     32%          100%
#>  5 Black    2006               47%        6%      18%     30%          100%
#>  6 Black    2012               43%        5%      21%     31%          100%
#> 
#>  7 White    2000               25%        3%      18%     54%          100%
#>  8 White    2006               22%        3%      19%     57%          100%
#>  9 White    2012               25%        3%      18%     53%          100%
#> 
#> 10 Ensemble 2000               28%        4%      17%     50%          100%
#> 11 Ensemble 2006               26%        4%      18%     52%          100%
#> 12 Ensemble 2012               29%        4%      18%     50%          100%
#> # marital: x > x1 +5%; +10%; +20%; +30%; x < x1 -5%; -10%; -20%; -30%
```

## Confidence intervals

It it possible to print confidence intervals for each cell:

``` r
tab(forcats::gss_cat, race, marital, pct = "row", ci = "cell")
#> # A tabxplor tab: 4 x 8
#>   race   `No answer` `Never married` Separated Divorced Widowed  Married
#>   <char>      <row%>          <row%>    <row%>   <row%>  <row%>   <row%>
#> 1 Other      0% ±0.1        32% ±2.1   6% ±1.0 11% ±1.4 4% ±0.8 48% ±2.2
#> 2 Black      0% ±0.1        42% ±1.7   6% ±0.8 16% ±1.3 8% ±1.0 28% ±1.6
#> 3 White      0%             21% ±0.6   3% ±0.2 16% ±0.6 9% ±0.4 51% ±0.8
#> 4 Total      0%             25%        3%      16%      8%      47%     
#> # ... with 1 more variable: Total_marital <row%>
```

It is also possible to use confidence intervals to enhance colors
helpers. With `color = "diff_ci`, the cells are only colored if the
confidence interval of the difference between them and their reference
cell (in total or first row/col) is superior to the difference itself.
Otherwise, it means the cell is not significantly different from it’s
reference in the total (or first) row: it turns grey, and the reader is
not anymore tempted to overinterpret the difference.

``` r
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff_ci")
#> # A tabxplor tab: 4 x 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>   <char>      <row%>          <row%>    <row%>   <row%>  <row%>  <row%>
#> 1 Other           0%             32%        6%      11%      4%     48%
#> 2 Black           0%             42%        6%      16%      8%     28%
#> 3 White           0%             21%        3%      16%      9%     51%
#> 4 Total           0%             25%        3%      16%      8%     47%
#> # ... with 1 more variable: Total_marital <row%>
#> # marital: |x-tot|>ci & x > tot +5%; +10%; +20%; +30%; |x-tot|>ci & x < tot -5%; -10%; -20%; -30%
```

Finally, another calculation appears helpful: the difference between the
cell and the total, minus the confidence interval of this difference (or
in other word, what remains of that difference after having substrated
the confidence interval). `ci = "after_ci"` highligths all the cells
whose value is significatively different from the relative total (or
first cell). This is particularly useful when working on small
populations: we can see at a glance which numbers we have right to read
and interpret.

``` r
tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "after_ci")
#> # A tabxplor tab: 4 x 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>   <char>      <row%>          <row%>    <row%>   <row%>  <row%>  <row%>
#> 1 Other           0%             32%        6%      11%      4%     48%
#> 2 Black           0%             42%        6%      16%      8%     28%
#> 3 White           0%             21%        3%      16%      9%     51%
#> 4 Total           0%             25%        3%      16%      8%     47%
#> # ... with 1 more variable: Total_marital <row%>
#> # marital: |x-tot| > ci +0%; +5%; +15%; +25%; |x-tot| > ci -0%; -5%; -15%; -25%
#> # Source: General social survey 2000-2014
```

## Chi2 stats and contributions of cells to variance

`chi2 = TRUE` add summary statistics made in the chi2 metric: degrees of
freedom (df), unweighted count, pvalue and (sub)table’s variance. Chi2
pvalue is colored in green when inferior to 5%, and in red when superior
or equal to 5%, meaning that the table is not significantly different
from the independant hypothesis (the two variables may be independant).

``` r
tab(forcats::gss_cat, race, marital, chi2 = TRUE)
#> chi2 stats     marital
#> df                  12
#> variance        0.0464
#> pvalue              0%
#> count           21 483
#> 
#> # A tabxplor tab: 4 x 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>   <char>      <n-wn>          <n-wn>    <n-wn>   <n-wn>  <n-wn>  <n-wn>
#> 1 Other            2             633       110      212      70     932
#> 2 Black            2           1 305       196      495     262     869
#> 3 White           13           3 478       437    2 676   1 475   8 316
#> 4 Total           17           5 416       743    3 383   1 807  10 117
#> # ... with 1 more variable: Total_marital <n-wn>
```

Chi2 stats can also be used to color cells based on their contributions
to the variance of the (sub)table, with `color = "contrib"`. By default,
only the cells whose contribution is superior to the mean contribution
are colored. It highlights the cells which would stand out in a
correspondance analysis (the two related categories would be located at
the edges of the first axes ; here, being black is associated with never
married and being separated).

``` r
tab(forcats::gss_cat, race, marital, color = "contrib")
#> chi2 stats     marital
#> df                  12
#> variance        0.0464
#> pvalue              0%
#> count           21 483
#> 
#> # A tabxplor tab: 4 x 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>   <char>      <n-wn>          <n-wn>    <n-wn>   <n-wn>  <n-wn>  <n-wn>
#> 1 Other            2             633       110      212      70     932
#> 2 Black            2           1 305       196      495     262     869
#> 3 White           13           3 478       437    2 676   1 475   8 316
#> 4 Total           17           5 416       743    3 383   1 807  10 117
#> # ... with 1 more variable: Total_marital <n-wn>
#> # marital: contrib > mean_ctr ×1; ×2; ×5; ×10; contrib > mean_ctr ×1; ×2; ×5; ×10
```

## Combine tabxplor and dplyr

The result of `tab` is a `tibble::tibble` dataframe with class `tab`. It
gets it’s own printing methods but, in the same time, can be transformed
using most dplyr verbs, like a normal tibble.

``` r
library(dplyr)
tab(storms, category, status, sup_cols = c("pressure", "wind")) %>%
  filter(category != "-1") %>%
  select(-`No answer`) %>%
  arrange(is_totrow(.), desc(category)) # use is_totrow to keep total rows
```

With `dplyr::arrange`, don’t forget to keep the order of tab variables
and total rows:

``` r
tab(data, race, marital, year, pct = "row") %>%
  arrange(year, is_totrow(.), desc(Married))
```

# Draw more complex tables with `tab_many`

`tab` is a wrapper around the more powerful function `tab_many`, which
can be used to customized your table.

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
#> chi2 stats     marital   rincome   partyid     relig       age   tvhours
#> df                  12        32        20        30                    
#> variance        0.0464    0.0092    0.1231    0.1299                    
#> pvalue              0%        0%        0%        0%                    
#> count           21 483    21 483    21 483    21 483    21 407    11 337
#> 
#> # A tabxplor tab: 4 x 8
#>   race  Married `$25000 or more` `Strong republi~ Protestant   age tvhours Total
#>   <cha>  <row%>           <row%>           <row%>     <row%> <mea>  <mean> <row>
#> 1 Other     48%              32%               4%        20%    39       3  100%
#> 2 Black     28%              28%               2%        73%    44       4  100%
#> 3 White     51%              36%              13%        50%    49       3  100%
#> 4 Total     47%              34%              11%        50%    47       3  100%
#> # marital, rincome, partyid: x > tot +5%; +10%; +20%; +30%; x < tot -5%; -10%; -20%; -30%
#> # age, tvhours             : x > tot ×1.15; ×1.5; ×2; ×4; x < tot /1.15; /1.5; /2; /4
```

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

## Export to Excel

`tab_xl` exports any table or list of tables to Excel, with all colors,
chi2 stats and formatting. On Excel, it is still possible to do
calculations on raw numbers.

``` r
tabs %>% tab_xl(replace = TRUE, sheets = "unique")
```

## Program with `tabxplor`

When not doing data analysis but writing functions, you can use the
sub-functions of `tab_many` step by step to attain more flexibility or
speed. That way, it’s possible to write new functions to customize your
tables even more.

``` r
data <- dplyr::starwars %>%
  tab_prepare(sex, hair_color, gender, rare_to_other = TRUE,
              n_min = 5, na = "keep")

data %>%
  tab_core(sex, hair_color, gender) %>%
  tab_totaltab("line")  %>%
  tab_tot()  %>%
  tab_pct(comp = "all")  %>%
  tab_ci("diff", color = "after_ci")  %>%
  tab_chi2(calc = "p")
```

The whole architecture of `tabxplor` is powered by a special vector
class, named `fmt` for formatted numbers. As a `vctrs::record`, it
stores behind the scenes all data necessary to calculate printed
results, formats and colors. A whole set of functions are available to
access or transform this data, like `is_fmt`, `is_totrow`, `is_totcol`
or `is_tottab`. `?fmt` to get more information.

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
