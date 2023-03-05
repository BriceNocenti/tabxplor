
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabxplor

<!-- badges: start -->

[![R-CMD-check](https://github.com/BriceNocenti/tabxplor/workflows/R-CMD-check/badge.svg)](https://github.com/BriceNocenti/tabxplor/actions)
<!-- badges: end -->

If R makes complex things simple, it can sometimes make simple things
difficult. This is why `tabxplor` tries to make it easy to deal with
multiple cross-tables: to create and manipulate them, but also to read
them, using color helpers to highlight important informations
(differences from totals, comparisons between lines or columns,
contributions to variance, margins of error, etc.). It would love to
enhance your data exploration experience with simple yet powerful tools.
All functions are propelled by `tidyverse`, pipe-friendly, and render
`tibble` data frames which can be easily manipulated with `dplyr`. In
the same time, time-taking operations are done with `data.table` to go
faster with big dataframes. Tables can be exported to Excel and in html
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

The main functions are made to be user-friendly and time-saving in data
analysis workflows.

`tab` makes a simple cross-table:

``` r
library(tabxplor)
tab(forcats::gss_cat, marital, race)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `na_rows = dplyr::if_any(.fns = is.na)`.
#> Caused by warning:
#> ! Using `if_any()` without supplying `.cols` was deprecated in dplyr 1.1.0.
#> ℹ Please supply `.cols` instead.
#> # A tabxplor tab: 7 × 5
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
tab(forcats::gss_cat, marital, race, pct = "row", na = "drop", 
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
#> Warning: `fct_explicit_na()` was deprecated in forcats 1.0.0.
#> ℹ Please use `fct_na_value_to_level()` instead.
#> ℹ The deprecated feature was likely used in the base package.
#>   Please report the issue to the authors.
#> # A tabxplor tab: 7 × 13
#>   category disturbance extratr…¹ hurri…² other…³ subtr…⁴ subtr…⁵ tropi…⁶ tropi…⁷
#>   <fct>            <n>       <n>     <n>     <n>     <n>     <n>     <n>     <n>
#> 1 1                  0         0   2 478       0       0       0       0       0
#> 2 2                  0         0     973       0       0       0       0       0
#> 3 3                  0         0     579       0       0       0       0       0
#> 4 4                  0         0     539       0       0       0       0       0
#> 5 5                  0         0     115       0       0       0       0       0
#> 6 NA               146     2 068       0   1 405     151     292   3 525   6 684
#> 7 Total            146     2 068   4 684   1 405     151     292   3 525   6 684
#> # … with 4 more variables: `tropical wave` <n>, Total <n>, pressure <mean>,
#> #   wind <mean>, and abbreviated variable names ¹​extratropical, ²​hurricane,
#> #   ³​`other low`, ⁴​`subtropical depression`, ⁵​`subtropical storm`,
#> #   ⁶​`tropical depression`, ⁷​`tropical storm`
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
evolutions. The first rows then appears in white (while rows totals are
themselves colored like normal lines).

``` r
data <- data %>% dplyr::mutate(year = as.factor(year))
tab(data, year, marital, race, pct = "row", color = "diff", diff = "first", tot = "col",
totaltab = "table")
```

![](.readme_images/tabxplor3_first.jpg)

When `diff` is a number, the nth row (or column) is used for comparison.

``` r
tab(data, year, marital, race, pct = "row", color = "diff", diff = 3)
```

Finally, when `diff` is a string, it it used as a regular expression, to
match with the names of the rows (or columns).

``` r
tab(data, year, marital, race, pct = "col", tot = "row", color = "diff", diff = "Married")
```

## Confidence intervals

It it possible to print confidence intervals for each cell:

``` r
tab(forcats::gss_cat, race, marital, pct = "row", ci = "cell")
#> Warning: There was 1 warning in `dplyr::transmute()`.
#> ℹ In argument: `complete = dplyr::if_all()`.
#> Caused by warning:
#> ! Using `if_all()` without supplying `.cols` was deprecated in dplyr 1.1.0.
#> ℹ Please supply `.cols` instead.
#> # A tabxplor tab: 4 × 8
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
in other word, what remains of the difference after having subtracted
the confidence interval). `ci = "after_ci"` highligths all the cells
whose value is significantly different from the relative total (or first
cell). This is particularly useful when working on small samples : we
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
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `dplyr::across(.fns = get_n)`.
#> Caused by warning:
#> ! Using `across()` without supplying `.cols` was deprecated in dplyr 1.1.0.
#> ℹ Please supply `.cols` instead.
#> Warning: There was 1 warning in `dplyr::filter()`.
#> ℹ In argument: `!sum(dplyr::c_across(), na.rm = TRUE) == 0`.
#> ℹ In row 1.
#> Caused by warning:
#> ! Using `c_across()` without supplying `cols` was deprecated in dplyr 1.1.0.
#> ℹ Please supply `cols` instead.
#> ℹ The deprecated feature was likely used in the tabxplor package.
#>   Please report the issue at <]8;;https://github.com/BriceNocenti/tabxplor/issueshttps://github.com/BriceNocenti/tabxplor/issues]8;;>.
#> chi2 stats      marital
#> df                   12
#> variance         0.0464
#> pvalue               0%
#> count            21 483
#> 
#> # A tabxplor tab: 4 × 8
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
arrange(is_totrow(.), desc(category)) # use is_totrow to keep total rows order
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
calculations on raw numbers (display is rounded but, below, decimals are
kept).

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
n_min = 5, na_drop_all = sex)

data %>%
tab_plain(sex, hair_color, gender, tot = c("row", "col"), pct = "row", comp = "all") %>%
tab_ci("diff", color = "after_ci")  %>%
tab_chi2(calc = "p")
```

The whole architecture of `tabxplor` is powered by a special vector
class, named `tabxplor_fmt` for formatted numbers. As a `vctrs::record`,
it stores behind the scenes all the data necessary to calculate printed
results, formats and colors. A set of functions are available to access
or transform this data. `?fmt` to get more information.

The simple way to recover the underlying numbers as numeric vectors is
`get_num`, which extract the currently displayed field whatever it is :

``` r
tabs <- tab(forcats::gss_cat, race, marital, pct = "row")
tabs %>% dplyr::mutate(across(where(is_fmt), get_num))
#> # A tabxplor tab: 4 × 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married Total
#>   <fct>        <dbl>           <dbl>     <dbl>    <dbl>   <dbl>   <dbl> <dbl>
#> 1 Other     0.00102            0.323    0.0562    0.108  0.0357   0.476     1
#> 2 Black     0.000639           0.417    0.0626    0.158  0.0837   0.278     1
#> 3 White     0.000793           0.212    0.0267    0.163  0.0900   0.507     1
#> 4 Total     0.000791           0.252    0.0346    0.157  0.0841   0.471     1
```

To render character vectors (without colors), use `format`:

``` r
tabs %>% mutate(across(where(is_fmt), format))
```

The following fields compose any `fmt` column (though many can be `NA`
if not calculated) :

-   `display` : name of the field to display, customisable for each cell
    (character)

-   `n` : raw count (integer)

-   `wn` : weighted count

-   `pct` : percentages

-   `diff` : differences from totals or reference cells

-   `digits` : digits to display, customisable for each cell (integer)

-   `ctr` : contributions of cells to variance (with
    `color = "contrib"`)

-   `mean` : means (for numeric column variables)

-   `var` : variance (for numeric column variables ; Chi2 variance with
    `pct`)

-   `ci` : confidence intervals

-   `in_totrow` : `TRUE` if the cell is part of a total row, `FALSE`
    otherwise (logical)

-   `in_tottab` : `TRUE` if the cell is part of a total table, `FALSE`
    otherwise (logical)

-   `in_refrow` : `TRUE` if the cell is part of a reference row, `FALSE`
    otherwise (logical)

``` r
vctrs::vec_data(tabs$Married)
#>       n display digits wn       pct mean         diff ctr var ci in_totrow
#> 1   932     pct      0 NA 0.4757529   NA  0.004822432  NA  NA NA     FALSE
#> 2   869     pct      0 NA 0.2777245   NA -0.193205991  NA  NA NA     FALSE
#> 3  8316     pct      0 NA 0.5072278   NA  0.036297310  NA  NA NA     FALSE
#> 4 10117     pct      0 NA 0.4709305   NA  0.000000000  NA  NA NA      TRUE
#>   in_tottab in_refrow
#> 1     FALSE     FALSE
#> 2     FALSE     FALSE
#> 3     FALSE     FALSE
#> 4     FALSE     FALSE
```

To get those underlying fields you can either use `vctrs::fields` or,
more simply, `$` :

``` r
tabs %>% mutate(across(where(is_fmt), ~ vctrs::field(., "pct") ))

tabs$Married$pct
tabs$Married$n
tabs %>% mutate(across(where(is_fmt), ~ .$n))
```

To modify a field, you can use `vctrs` `field<-`. For example, to change
the displayed field :

``` r
tab(data, race, marital, year, pct = "row") %>%
mutate(across(where(is_fmt), ~ vctrs::`field<-`(., "display", rep("diff", length(.)))))
```

Faster to write and easier to read, you can also use `dplyr::mutate()`
on an `fmt` vector. For example, to create a new column with standards
deviations and display it with decimals :

``` r
tab_num(data, race, c(age, tvhours), marital, digits = 1L, comp = "all") |>
  dplyr::mutate(dplyr::across( #Mutate over the whole table.
    c(age, tvhours),
    ~ dplyr::mutate(., #Mutate over each fmt vector's underlying data.frame.
                    var     = sqrt(var), 
                    display = "var", 
                    digits  = 2L) |> 
      set_color("no"),
    .names = "{.col}_sd"
  ))
```

Some helper functions exists for total rows, total tables and reference
rows (`is_totrow()` / `as_totrow()`, `is_tottab()` / `as_tottab()`,
`is_refrow()` / `as_refrow()`) :

``` r
tab(data, race, marital, year, pct = "row") %>%
  dplyr::mutate(across( 
    where(is_fmt),
    ~ dplyr::if_else(is_totrow(.), 
                true  = mutate(., digits = 1L), 
                false = mutate(., digits = 2L))
  ))
```

Each `fmt` column have attributes, which you can access or modify with
`get_` and `set_` functions :

-   type / `get_type()` / `set_type()` : the type of the `fmt` vector,
    among `c("n", "mean", "row", "col", "all", "all_tabs")` ; it
    determines which calculations are done within `tab_` functions.

-   totcol / `is_totcol()` / `as_totcol()` : `TRUE` if the column is a
    total column, `FALSE` otherwise (logical)

-   refcol / `is_refcol()` / `as_refcol()` : `TRUE` if the column is a
    reference column for comparison, `FALSE` otherwise (logical)

-   color / `get_color()` / `set_color()` : the calculation to make to
    print colors ; among
    `c("", "no", "diff", "diff_ci", "after_ci", "contrib")`

-   col_var / `get_col_var()` / `set_col_var()` : the name of the column
    variable (there can be many in one single table)

-   comp_all / `get_comp_all` / `set_comp_all()` : when there are
    `tab_vars`, is the reference for comparison the subtable (`FALSE`),
    or the total table (`TRUE`) ?

-   diff_type / `get_diff_type()` / `set_diff_type()` : the type of
    difference calculated, either `"no"`, `"tot"` for totals, an index,
    or a regular expression.

-   ci_type / `get_ci_type()` / `set_ci_type()` : the type of confidence
    interval, either `"cell"` or `"diff"`

For example, to print the number of observations of the total column :

``` r
tab(data, race, marital, year, pct = "row") %>%
  mutate(across(where(is_totcol), ~ mutate(., display = "n") ))
```

Note that, if `tab_vars` are provided, the table is grouped and all
operations are made within groups. To remove grouping (for example when
it gives errors), use `dplyr::ungroup()`.

If you only need the simplest table, with only numeric counts (no
`fmt`), or even a base `data.frame` (not a `tibble`) :

``` r
tab_plain(data, race, marital, num = TRUE) # counts as a numeric vector
tab_plain(data, race, marital, df = TRUE)  # same, with unique class = "data.frame"
```
