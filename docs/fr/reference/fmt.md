# Create a vector of class formatted numbers

`fmt` vectors, of class `tabxplor_fmt`, powers tabxplor and
[`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
tibbles. As a
[`record`](https://vctrs.r-lib.org/reference/new_rcrd.html), they stores
all data necessary to calculate percentages, Chi2 metadata or confidence
intervals, but also to format and color the table to help the user read
it. You can access this data with
[`vctrs::field`](https://vctrs.r-lib.org/reference/fields.html), or
change it with
[`vctrs:field<-`](https://vctrs.r-lib.org/reference/fields.html). A
`fmt` vector have 19 fields : `n`, `digits`, `display`, `wn`, `pct`,
`mean`, `diff`, `ratio`, `ctr`, `var`, `ci_inf`, `ci_sup`, `pvalue`,
`or`, `tot_n`, `n_eff`, `in_totrow`, `in_tottab`, `in_refrow`. Other
arguments are attributes, attached not to each value, but to the whole
vector, like `type`, `totcol` or `color`. You can get them with
[`attr`](https://rdrr.io/r/base/attr.html) and modify them with
[`attr<-`](https://rdrr.io/r/base/attr.html). Special functions listed
below are made to facilitate programming with with tabxplor formatted
numbers. `taxplfmt` vectors can use all standard operations, like +, -,
sum(), or c(), using vctrs.

## Usage

``` r
fmt(
  n = integer(),
  type = "n",
  digits = rep(0L, length(n)),
  display = dplyr::case_when(type == "mean" ~ "mean", type %in% c("row", "col", "all",
    "all_tabs") ~ "pct", TRUE ~ "n"),
  wn = rep(NA_real_, length(n)),
  pct = rep(NA_real_, length(n)),
  mean = rep(NA_real_, length(n)),
  diff = rep(NA_real_, length(n)),
  ratio = rep(NA_real_, length(n)),
  ctr = rep(NA_real_, length(n)),
  var = rep(NA_real_, length(n)),
  ci = rep(NA_real_, length(n)),
  ci_inf = rep(NA_real_, length(n)),
  ci_sup = rep(NA_real_, length(n)),
  pvalue = rep(NA_real_, length(n)),
  or = rep(NA_real_, length(n)),
  tot_n = rep(NA_real_, length(n)),
  n_eff = rep(NA_real_, length(n)),
  in_totrow = rep(FALSE, length(n)),
  in_tottab = rep(FALSE, length(n)),
  in_refrow = rep(FALSE, length(n)),
  comp_all = NA,
  ref = "",
  ci_type = "",
  col_var = "",
  totcol = FALSE,
  refcol = FALSE,
  color = "",
  color_signif = "ignore",
  model_family = "",
  role = ""
)

is_fmt(x)

get_num(x)

set_num(x, value)

get_type(x, ...)

set_type(x, type)

is_totrow(x, ...)

as_totrow(x, in_totrow = TRUE)

is_tottab(x, ...)

as_tottab(x, in_tottab = TRUE)

set_display(x, value)

is_totcol(x, ...)

as_totcol(x, totcol = TRUE)

is_refrow(x, ...)

as_refrow(x, in_refrow = TRUE)

get_comp_all(x, replace_na = TRUE)

set_comp_all(x, comp_all = FALSE)

get_ref_type(x, ...)

set_diff_type(x, ref)

get_ci_type(x, ...)

set_ci_type(x, ci_type)

get_col_var(x, ...)

set_col_var(x, col_var)

get_model_family(x, ...)

set_model_family(x, model_family)

is_refcol(x, ...)

as_refcol(x, refcol = TRUE)

get_color(x, ...)

get_color_bg(x, ...)

get_color_signif(x, ...)

set_color(x, color)

set_color_signif(x, color_signif)

get_digits(x)

set_digits(x, value)
```

## Arguments

- n:

  The underlying count, as an integer vector of length
  [`n()`](https://dplyr.tidyverse.org/reference/context.html). It is
  used to calculate confidence intervals.

- type:

  The type of the column, which defines the type of background
  calculation to be made (as a single string, since it's not a field but
  an attribute) :

  - `"n"`: counts

  - `"mean"`: mean column (from numeric variables)

  - `"row"`: row percentages

  - `"col"`: column percentages

  - `"all"`: frequencies by subtable/group (i.e. by `tab_vars`)

  - `"all_tabs"`: frequencies for the whole table

- digits:

  The number of digits, as an integer, or an integer vector the length
  of `n`.

- display:

  The display type : the name of the field you want to show when
  printing the vector. Among `"n"`, `"wn"`, `"pct"`, `"diff"`, `"ctr"`,
  `"mean"`, `"var"`, `"ci"`, `"ratio"` (the cell-to-reference ratio; the
  legacy synonym `"rr"` still resolves to it), `"pct_ci"` (percentages
  with visible confidence interval), `"mean_ci"` (means with visible
  confidence interval). As a single string, or a character vector the
  length of `n`.

- wn:

  The underlying weighted counts, as a double vector the length of `n`.
  It is used in certain operations on `fmt`, like means.

- pct:

  The percentages, as a double vector the length of `n`. Calculate with
  [`tab_pct`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_pct.md).

- mean:

  The means, as a double vector the length of `n`.

- diff:

  The differences (from totals or first cells), as a double vector the
  length of `n`. Used to set colors for means and row or col
  percentages. Calculate with
  [`tab_pct`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_pct.md).

- ratio:

  The ratio to the reference (relative risk for percentages, mean ratio
  for means), as a double vector the length of `n`. Renamed from the
  former `rr` field.

- ctr:

  The contributions of cells to (sub)tables variances, as a double
  vector the length of `n`. Used to print colors when
  `color = "contrib"`. The mean contribution of each (sub)table is
  written on total rows (then, colors don't print well without total
  rows). Calculate with
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md).

- var:

  The cells variances, as a double vector the length of `n`. Used with
  `type = "mean"` to calculate confidence intervals. Calculate with
  `tab_plain`.

- ci:

  The confidence interval half-width (margin of error), as a double
  vector the length of `n`. Kept for backward compatibility: it is
  stored as the symmetric bounds `ci_inf`/`ci_sup` and read back by
  `get_ci()`. Calculate with `tab_ci`.

- ci_inf, ci_sup:

  The lower and upper bounds of the confidence interval, as double
  vectors the length of `n`. Calculate with `tab_ci`.

- pvalue:

  The per-cell significance p-value, as a double vector the length of
  `n`.

- or:

  The odds ratio (for a 3+ level variable, the OR of each level versus
  the reference), as a double vector the length of `n`.

- tot_n:

  The cell's own (unweighted) percentage base, as a double vector the
  length of `n`.

- n_eff:

  The effective sample size used for this cell's confidence interval:
  Kish's `(sum w)^2 / sum(w^2)` when
  `options(tabxplor.kish_neff = TRUE)` on weighted data, else `NA` (the
  CI then falls back to the raw unweighted base). A double vector the
  length of `n`. Non-displayed.

- in_totrow:

  `TRUE` when the cell is part of a total row

- in_tottab:

  `TRUE` when the cell is part of a total table

- in_refrow:

  `TRUE` when the cell is part of a reference row (cf. `ref`)

- comp_all:

  `FALSE` when the comparison level is the subtable/group, `TRUE` when
  it is the whole table

- ref:

  The type of difference of the vector. Cf.
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).

- ci_type:

  The type of confidence intervals of the vector (calculate with
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md))
  :

  - `""` or `"no"`: no ci have been calculated

  - `"cell"`: absolute confidence intervals of cells percentages.

  - `"diff"`: confidence intervals of the difference between a cell and
    the relative total cell (or relative first cell when
    `ref = "first"`).

  - `"auto"`: `"diff"` for means and row/col percentages, `"cell"` for
    frequencies ("all", "all_tabs").

- col_var:

  The name of the `col_var` used to calculate the vector

- totcol:

  `TRUE` when the vector is a total column

- refcol:

  `TRUE` when the vector is a reference column

- color:

  The type of color to print :

  - `"no"`: no colors are printed.

  - `"diff"`: color percentages and means based on cells differences
    from totals (or from first cells when `ref = "first"`).

  - `"diff_ci"`: color pct and means based on cells differences from
    totals or first cells, removing coloring when the confidence
    interval of this difference is higher than the difference itself.

  - `"after_ci"`: idem, but cut off the confidence interval from the
    difference first.

  - `"contrib"`: color cells based on their contribution to variance
    (except mean columns, from numeric variables).

- color_signif:

  How significance gates the color, as a single string (`"ignore"` /
  `"grey_non_signif"` / `"guaranteed_effect"`). See
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).

- model_family:

  For regression tables
  ([`tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)):
  the column's model family (`"binomial"`, `"gaussian"`, `"poisson"`,
  `"multinomial"`, `"ordinal"`), as a single string. Empty (`""`) on
  cross-tables. Lets a table mix several dependents with different
  families, each column keeping its own effect wording.

- role:

  For regression tables
  ([`tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)):
  the column's role, `"model"` for a model-estimate column or `"emp"`
  for an empirical (crude) companion column. Empty (`""`) on
  cross-tables. Read by the colour legend to name each column's effect
  without matching its label.

- x:

  The object to test, to get a field in, or to modify.

- value:

  The value you want to inject in some `fmt` vector's vctrs::field or
  attribute using a given "set" function.

- ...:

  Used in methods to add arguments in the future.

- replace_na:

  By default, `get_comp_all` takes NA in comparison level to be a
  `FALSE` (=comparison at subtables/groups level). Set to `FALSE` to
  avoid this behavior.

## Value

A vector of class `tabxplor_fmt`.

A logical vector.

A double vector.

A modified fmt vector.

A character vector with the vectors type.

A modified fmt vector.

A logical vector with the fmt vectors totrow field.

A modified fmt vector with totrow field changed.

A logical vector with the fmt vectors tottab field.

A modified fmt vector with tottab field changed.

The entered objects, with all fmt vectors with the wanted display.

A logical vector with the fmt vectors totcol attribute.

A modified fmt vector with totcol attribute changed.

A logical vector with the fmt vectors in_refrow field

A modified fmt vector with in_refrom field changed.

A modified fmt vector with comp attribute changed.

A logical vector with the fmt vectors type attributes

A modified fmt vector.

A logical vector with the fmt vectors ci_type attributes

A modified fmt vector.

A logical vector with the fmt vectors col_var attributes

A modified fmt vector.

A character vector with the fmt vectors' model_family attributes (`""`
when unset, e.g. on cross-tables). On a data.frame, one value per
column.

A modified fmt vector.

A logical vector with the fmt vectors is_refcol attributes

A modified fmt vector.

A logical vector with the fmt vectors color attributes

A single character with the background color measure, or `NA`.

A modified fmt vector.

## Functions

- `is_fmt()`: a test function for class fmt.

- `get_num()`: get the currently displayed field

- `set_num()`: set the currently displayed field (not changing display
  type)

- `get_type()`: get types of fmt columns (at `fmt` level or `tab` level)

- `set_type()`: set the column type attribute of a `fmt` vector

- `is_totrow()`: test function to detect cells in total rows (at `fmt`
  level or `tab` level)

- `as_totrow()`: set the "in_totrow" field (belong to total row)

- `is_tottab()`: test function to detect cells in total tables (at `fmt`
  level or `tab` level)

- `as_tottab()`: set the "in_tottab" field (belong to total table)

- `set_display()`: set the "display" vctrs::field of a `fmt` vector, or
  of all of them in the whole tibble.

- `is_totcol()`: test function for total columns (at `fmt` level or
  `tab` level)

- `as_totcol()`: set the "totcol" attribute of a `fmt` vector

- `is_refrow()`: test function to detect cells in reference rows (at
  `fmt` level or `tab` level)

- `as_refrow()`: set the "in_refrow" field (belong to reference row)

- `get_comp_all()`: get comparison level of fmt columns

- `set_comp_all()`: set the comparison level attribute of a `fmt` vector

- `get_ref_type()`: get differences type of fmt columns (at `fmt` level
  or `tab` level)

- `set_diff_type()`: set the differences type attribute of a `fmt`
  vector

- `get_ci_type()`: get confidence intervals type of fmt columns (at
  `fmt` level or `tab` level)

- `set_ci_type()`: set the confidence intervals type attribute of a
  `fmt` vector

- `get_col_var()`: get names of column variable of fmt columns (at `fmt`
  level or `tab` level)

- `set_col_var()`: set the "col_var" attribute of a `fmt` vector

- `get_model_family()`: get the regression model family of fmt columns
  (at `fmt` or `tab` level)

- `set_model_family()`: set the "model_family" attribute of a `fmt`
  vector (Phase 15e: the per-column regression family, "" on crosstabs)

- `is_refcol()`: test function for reference columns (at `fmt` level or
  `tab` level)

- `as_refcol()`: set the "ref_col" attribute of a `fmt` vector

- `get_color()`: get color (at `fmt` level or `tab` level)

- `get_color_bg()`: get the background-channel color measure (`NA` when
  there is none)

- `get_color_signif()`: get the significance policy (`"ignore"` /
  `"grey_non_signif"` / `"guaranteed_effect"`)

- `set_color()`: set the "color" attribute of a `fmt` vector

- `set_color_signif()`: set the significance policy attribute of a `fmt`
  vector

- `get_digits()`: get the "digits" field

- `set_digits()`: set the "digits" field

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

f <- fmt(n = c(7, 19, 2), type = "row", pct = c(0.25, 0.679, 0.07))
f
#> <fmt-row%[3]>
#> [1] 25% 68% 7% 

# To get the currently displayed field :
get_num(f)
#> [1] 0.250 0.679 0.070

# To modify the currently displayed field :
set_num(f, c(1, 0, 0))
#> <fmt-row%[3]>
#> [1] 100% 0%   0%  


# See all the underlying fields of a fmt vector (a data frame with a number of rows
#  equal to the length of the vector) :
vctrs::vec_data(f)
#>    n display digits wn   pct mean diff ratio ctr var ci_inf ci_sup pvalue or
#> 1  7     pct      0 NA 0.250   NA   NA    NA  NA  NA     NA     NA     NA NA
#> 2 19     pct      0 NA 0.679   NA   NA    NA  NA  NA     NA     NA     NA NA
#> 3  2     pct      0 NA 0.070   NA   NA    NA  NA  NA     NA     NA     NA NA
#>   tot_n n_eff in_totrow in_tottab in_refrow
#> 1    NA    NA     FALSE     FALSE     FALSE
#> 2    NA    NA     FALSE     FALSE     FALSE
#> 3    NA    NA     FALSE     FALSE     FALSE

# To get the numbers of digits :
vctrs::field(f, "digits")
#> [1] 0 0 0
f$digits
#> [1] 0 0 0

# To get the count :
vctrs::field(f, "n")
#> [1]  7 19  2
f$n
#> [1]  7 19  2

# To get the display :
vctrs::field(f, "display")
#> [1] "pct" "pct" "pct"
f$display
#> [1] "pct" "pct" "pct"

# To modify a field, you can use `dplyr::mutate` on the fmt vector,
# referring to the names of the columns of the underlying data.frame (`vctrs::vec_data`) :
vctrs::`field<-`(f, "pct", c(1, 0, 0))
#> <fmt-row%[3]>
#> [1] 100% 0%   0%  
mutate(f, pct = c(1, 0, 0))
#> <fmt-row%[3]>
#> [1] 100% 0%   0%  

# See all the attributes of a fmt vector :
attributes(f)
#> $names
#>  [1] "n"         "display"   "digits"    "wn"        "pct"       "mean"     
#>  [7] "diff"      "ratio"     "ctr"       "var"       "ci_inf"    "ci_sup"   
#> [13] "pvalue"    "or"        "tot_n"     "n_eff"     "in_totrow" "in_tottab"
#> [19] "in_refrow"
#> 
#> $type
#> [1] "row"
#> 
#> $comp_all
#> [1] NA
#> 
#> $ref
#> [1] ""
#> 
#> $ci_type
#> [1] ""
#> 
#> $col_var
#> [1] ""
#> 
#> $totcol
#> [1] FALSE
#> 
#> $refcol
#> [1] FALSE
#> 
#> $color
#> [1] ""
#> 
#> $color_signif
#> [1] "ignore"
#> 
#> $model_family
#> [1] ""
#> 
#> $role
#> [1] ""
#> 
#> $class
#> [1] "tabxplor_fmt" "vctrs_rcrd"   "vctrs_vctr"  
#> 

# To modify the "type" attribute of a fmt vector :
set_type(f, "col")
#> <fmt-col%[3]>
#> [1] 25% 68% 7% 

# To modify the "color" attribute of a fmt vector :
set_color(f, "contrib")
#> <fmt-row%[3]>
#> [1] 25% 68% 7% 




tabs <- tab(starwars, sex, hair_color, gender, na = "drop", pct = "row",
            other_if_less_than = 5)
#> Error in dplyr::mutate(dplyr::ungroup(dplyr::mutate(dplyr::group_by(data,     !!!tab_vars), dplyr::across(tidyselect::all_of(as.character(row_vars)),     ~forcats::fct_lump_min(., other_if_less_than, other_level = other_level)))),     dplyr::across(as.character(row_vars), function(.x) forcats::fct_relevel(.x,         purrr::discard(unique(append(levels(dplyr::pull(data,             dplyr::cur_column())), other_level)), function(v) !v %in%             levels(.x))))): ℹ In argument: `dplyr::across(...)`.
#> Caused by error in `across()`:
#> ! Can't compute column `sex`.
#> Caused by error in `purrr::discard()`:
#> ℹ In index: 1.
#> Caused by error in `.fn()`:
#> ! object '.x' not found

# To identify the total columns, and work with them :
is_totcol(tabs)
#> Error: object 'tabs' not found
tabs |> mutate(across(where(is_totcol), ~ "total column"))
#> Error: object 'tabs' not found

# To identify the total rows, and work with them :
is_totrow(tabs)
#> Error: object 'tabs' not found
tabs |>
  mutate(across(
    where(is_fmt),
    ~ if_else(is_totrow(.), true = "into_total_row", false = "normal_cell")
  ))
#> Error: object 'tabs' not found

# To identify the total tables, and work with them :
tottabs <- is_tottab(tabs)
#> Error: object 'tabs' not found
tabs |> tibble::add_column(tottabs) |>
  mutate(total = if_else(tottabs, "part of a total table", "normal cell"))
#> Error: object 'tabs' not found

# To access the displayed numbers, as numeric vectors :
tabs |> mutate(across(where(is_fmt), get_num))
#> Error: object 'tabs' not found

# To access the displayed numbers, as character vectors (without colors) :
tabs |> mutate(across(where(is_fmt), format))
#> Error: object 'tabs' not found

# To access the (non-displayed) differences of the cells percentages from totals :
tabs |> mutate(across(where(is_fmt), ~ vctrs::field(., "diff")))
#> Error: object 'tabs' not found


# To do more complex operations, like creating a new column with standard deviation and
# print it with 2 decimals, use `dplyr::mutate` on all the fmt columns of a table :

tab_num(forcats::gss_cat, race, c(age, tvhours), marital, digits = 1L, comp = "all") |>
  dplyr::mutate(dplyr::across( #Mutate over the whole table.
    c(age, tvhours),
    ~ dplyr::mutate(.,         #Mutate over each fmt vector's underlying data.frame.
                    var     = sqrt(var),
                    display = "var",
                    digits  = 2L) |>
      set_color("no"),
    .names = "{.col}_sd"
  ))
#> # A tabxplor tab: 25 × 6
#> # Groups:         marital [7]
#>    marital       race                         age    tvhours   age_sd tvhours_sd
#>    <fct>         <fct>                     <mean>     <mean> <mean-v> <mean-var>
#>  1 No answer     Other               34.0 (σ8.5 ) 2.0            8.49           
#>  2 No answer     Black               64.0                                       
#>  3 No answer     White               56.0 (σ15.7) 2.6 (σ1.2)    15.71       1.19
#>  4 No answer     Total No answer     52.4 (σ16.5) 2.6 (σ1.1)    16.51       1.13
#> 
#>  5 Never married Other               30.2 (σ10.6) 2.8 (σ2.7)    10.60       2.67
#>  6 Never married Black               34.5 (σ12.1) 4.2 (σ3.4)    12.14       3.39
#>  7 Never married White               34.4 (σ14.3) 2.8 (σ2.6)    14.29       2.56
#>  8 Never married Total Never married 33.9 (σ13.5) 3.1 (σ2.9)    13.47       2.86
#> 
#>  9 Separated     Other               42.5 (σ13.0) 3.3 (σ3.3)    12.97       3.26
#> 10 Separated     Black               46.2 (σ13.4) 5.1 (σ4.7)    13.36       4.73
#> 11 Separated     White               45.6 (σ13.5) 2.9 (σ2.8)    13.52       2.77
#> 12 Separated     Total Separated     45.3 (σ13.4) 3.5 (σ3.6)    13.43       3.60
#> 
#> 13 Divorced      Other               45.5 (σ11.8) 3.0 (σ2.7)    11.82       2.71
#> 14 Divorced      Black               51.0 (σ12.7) 4.3 (σ3.7)    12.67       3.74
#> 15 Divorced      White               51.6 (σ13.2) 2.9 (σ2.4)    13.22       2.43
#> 16 Divorced      Total Divorced      51.1 (σ13.1) 3.1 (σ2.7)    13.14       2.73
#> 
#> 17 Widowed       Other               64.5 (σ14.8) 4.2 (σ2.8)    14.84       2.79
#> 18 Widowed       Black               67.5 (σ13.9) 4.7 (σ3.7)    13.89       3.70
#> 19 Widowed       White               72.8 (σ12.5) 3.7 (σ2.7)    12.48       2.70
#> 20 Widowed       Total Widowed       71.7 (σ13.0) 3.9 (σ2.9)    13.00       2.90
#> 
#> 21 Married       Other               42.2 (σ13.0) 2.5 (σ1.9)    13.01       1.88
#> 22 Married       Black               46.4 (σ13.4) 3.8 (σ3.1)    13.40       3.06
#> 23 Married       White               49.7 (σ15.2) 2.6 (σ2.0)    15.24       1.98
#> 24 Married       Total Married       48.7 (σ15.1) 2.7 (σ2.1)    15.06       2.11
#> 
#> 25 Ensemble      Total Ensemble      47.2 (σ17.3) 3.0 (σ2.6)    17.29       2.59
#> # standardized difference (Total): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8
```
