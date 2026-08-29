# Create an `fmt` vector, the tabxplor cell

`fmt` vectors, of class `tabxplor_fmt`, powers tabxplor and
[`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
tibbles. As a
[`record`](https://vctrs.r-lib.org/reference/new_rcrd.html), they stores
all data necessary to calculate percentages, Chi2 metadata or confidence
intervals, but also to format and color the table to help the user read
it. You can access this data with
[`vctrs::field`](https://vctrs.r-lib.org/reference/fields.html), or
change it with
[`vctrs:field<-`](https://vctrs.r-lib.org/reference/fields.html). Its
per-cell **fields** are listed below. The other arguments are
**attributes**, attached not to each value but to the whole vector, like
`scale`, `col_var`, `totcol` or `color`. You can get them with
[`attr`](https://rdrr.io/r/base/attr.html) and modify them with
[`attr<-`](https://rdrr.io/r/base/attr.html). Special functions listed
below are made to facilitate programming with with tabxplor formatted
numbers. `taxplfmt` vectors can use all standard operations, like +, -,
sum(), or c(), using vctrs.

## Usage

``` r
fmt(
  n = integer(),
  scale = "level_n",
  digits = rep(0L, length(n)),
  display = est_default_display(scale[1]),
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
  obs = rep(NA_real_, length(n)),
  gap_se = rep(NA_real_, length(n)),
  row_kind = rep("data", length(n)),
  in_tottab = rep(FALSE, length(n)),
  in_refrow = rep(FALSE, length(n)),
  in_totrow = NULL,
  comp_all = NA,
  ref = "",
  pct_type = "none",
  col_var = "",
  col_group = "",
  totcol = FALSE,
  refcol = FALSE,
  color = "",
  color_signif = "ignore",
  model_family = "",
  role = "",
  conf_level = NA_real_,
  degf = NA_real_,
  basis = "n",
  ci_method = "",
  ...
)

is_fmt(x)
```

## Arguments

- n:

  The underlying count, as an integer vector of length
  [`n()`](https://dplyr.tidyverse.org/reference/context.html). It is
  used to calculate confidence intervals.

- scale:

  What the column estimates, as a single string (an attribute, not a
  field): one key into the declared library of estimate scales. It says
  which field holds the estimate, what its null value is, whether the
  scale is additive or multiplicative, and which colour ladder it reads.

  - `"level_n"`: counts

  - `"level_pct"`: percentages (`pct_type` says of what)

  - `"level_mean"`: means (from numeric variables)

  - `"points"`: a difference between two percentages, in percentage
    points

  - `"mean_diff"`: a difference between two means, in the outcome's own
    units

  - `"raw_diff"`: a regression coefficient / marginal effect in the
    outcome's units

  - `"pct_ratio"`, `"mean_ratio"`: the ratio of two percentages / two
    means

  - `"odds_ratio"`: a multiplicative effect (odds ratio, risk ratio,
    rate ratio)

  - `"log_coef"`: a link-scale coefficient (a log-odds, a log-rate)

  - `"mixed"`: what binding columns of unlike scales collapses to

- digits:

  The number of digits, as an integer, or an integer vector the length
  of `n`.

- display:

  The display type : the name of the field you want to show when
  printing the vector, as a single string or a character vector the
  length of `n`. Every accepted value is listed in *Every display token*
  below; a named layout or a [`{}`](https://rdrr.io/r/base/Paren.html)
  template combining several (e.g. `"\{pct\} (n=\{n\})"`) is also
  accepted — see
  [tabxplor-display](https://bricenocenti.github.io/tabxplor/reference/tabxplor-display.md).

- wn:

  The underlying weighted counts, as a double vector the length of `n`.
  It is used in certain operations on `fmt`, like means.

- pct:

  The percentages, as a double vector the length of `n`. Calculate with
  [`tab_pct`](https://bricenocenti.github.io/tabxplor/reference/tab_pct.md).

- mean:

  The means, as a double vector the length of `n`.

- diff:

  The differences (from totals or first cells), as a double vector the
  length of `n`. Used to set colors for means and row or col
  percentages. Built by
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- ratio:

  The ratio to the reference (relative risk for percentages, mean ratio
  for means), as a double vector the length of `n`.

- ctr:

  The contributions of cells to (sub)tables variances, as a double
  vector the length of `n`. Used to print colors when
  `color = "contrib"`. The mean contribution of each (sub)table is
  written on total rows (then, colors don't print well without total
  rows). Built by
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md). The
  cell's adjusted standardized residual is not a field of its own: it is
  recovered from `pvalue` and this field's sign, and readable with
  `display = "resid"` (see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)).

- var:

  The cells variances, as a double vector the length of `n`. Used with
  `scale = "level_mean"` to calculate confidence intervals.

- ci:

  The confidence interval half-width (margin of error), as a double
  vector the length of `n`. Kept for backward compatibility: it is
  stored as the symmetric bounds `ci_inf`/`ci_sup` and read back by
  `get_ci()`.

- ci_inf, ci_sup:

  The lower and upper bounds of the confidence interval, as double
  vectors the length of `n`. Built by
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

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

  The effective sample size used for this cell's confidence interval,
  `p(1-p) / Var_design(p)` (a mean: `s^2 / Var_design(mean)`): from
  [`survey::svyrecvar`](https://rdrr.io/pkg/survey/man/svyrecvar.html)
  under a
  [`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html),
  from the closed-form flat-design variance when the weighted basis is
  asked for (`tab(design_effect = TRUE)`), else `NA` (the CI falls back
  to the raw unweighted base). It records *the base that was used*: a
  finite value where the design or weights corrected it, `NA` where
  nothing did, and the **raw count** where a correction was asked for
  but this cell could not carry one. Populated for descriptive cells (a
  crosstab/mean cell, a `tab_reg` `Obs_*` column whose interval came
  from a closed form); a coefficient column, and any column whose
  interval came from a fit instead, carry none. A double vector the
  length of `n`. Non-displayed.

- obs:

  The value this cell's estimate is COMPARED TO by the `tab_reg` colour
  measures `"adjustment"` / `"between_groups"`, on the cell's own scale:
  the observed (crude) effect beside a model effect, or – under
  `tab_vars` with `color = "between_groups"` – the reference group's
  estimate. `NA` on cross-tables and wherever there is no counterpart
  (leaving those cells uncoloured). A double vector the length of `n`;
  displayable as `display = "\{obs\}"`.

- gap_se:

  The standard error of the GAP between this cell's estimate and `obs`,
  on the estimate's own test scale. Written by `tab_reg` where the two
  estimates are independent (`tab_vars` groups), so
  `color = "between_groups"` can honour `color_signif`; `NA` elsewhere.
  A double vector the length of `n`. Non-displayed.

- row_kind:

  What kind of row the cell sits in — one of `"data"` (an ordinary body
  row), `"total"`, and the synthetic display rows `"n"`, `"pct"`,
  `"pvalue"`, `"gof"`, `"blank"`. A character vector the length of `n`.
  It supersedes the logical `in_totrow` field, kept as a soft-deprecated
  argument and read-only `$in_totrow`.

- in_tottab:

  `TRUE` when the cell is part of a total table

- in_refrow:

  `TRUE` when the cell is part of a reference row (cf. `ref`)

- in_totrow:

  **\[deprecated\]** Use `row_kind = "total"`.

- comp_all:

  `FALSE` when the comparison level is the subtable/group, `TRUE` when
  it is the whole table

- ref:

  The type of difference of the vector. Cf.
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- pct_type:

  For a percentage column, what the percentage is a percentage OF, and
  hence which axis its reference lies on (as a single string): `"row"`,
  `"col"`, `"all"` (frequencies by subtable / group, i.e. by
  `tab_vars`), `"all_tabs"` (frequencies for the whole table), or
  `"none"` (counts, means, coefficients).

- col_var:

  The name of the `col_var` used to calculate the vector

- col_group:

  The sub-population this column's block belongs to: a level of a
  `spread_vars` variable
  ([`tab_spread`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)),
  or a
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  `tab_vars` group. `""` (the default) when the table was never spread.
  Together with `col_var` it identifies a column BLOCK: two blocks may
  show the same variable for two sub-populations, and exports head them
  on two lines.

- totcol:

  `TRUE` when the vector is a total column

- refcol:

  `TRUE` when the vector is a reference column

- color:

  The colour measure, as a single string — how a cell's value is
  compared to colour it (significance is handled separately by
  `color_signif`):

  - `"no"`: no colors are printed.

  - `"diff"` (`"difference"`): the cell's difference from the reference
    (a total, or the first cell when `ref = "first"`) — percentage
    points for factors, a standardized difference for means.

  - `"ratio"`: the ratio to the reference (relative risk for
    percentages, mean ratio).

  - `"or"` (`"odds_ratio"`): the odds ratio, for row/col percentages.

  - `"contrib"`: the cell's contribution to the table's variance. Under
    `color_signif = "guaranteed_effect"` it switches to the absolute
    adjusted standardized residual — see
    [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

  - `"adjustment"` / `"between_groups"`: the two
    [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
    measures, which compare a cell to *another column* rather than to a
    reference row. A hand-built column may carry them, provided it fills
    the `obs` field they score.

  The value is **validated and normalised**: every accepted spelling —
  the discipline's acronyms included (`"RD"`, `"RR"`, `"IRR"`, `"RoM"`,
  `"OR"` and their lowercase twins) — is stored as its canonical measure
  name, and an unknown one is an error. The tabxplor 1.x combined
  strings `"diff_ci"` / `"after_ci"` still work but are superseded by
  the `color` + `color_signif` pair; here they resolve to their
  *measure* half only, so pass the significance policy through
  `color_signif`.

- color_signif:

  How significance gates the color, as a single string (`"ignore"` /
  `"grey_non_signif"` / `"guaranteed_effect"`). See
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- model_family:

  For regression tables
  ([`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)):
  the column's model family (`"binomial"`, `"gaussian"`, `"poisson"`,
  `"multinomial"`, `"ordinal"`), as a single string. Empty (`""`) on
  cross-tables. Lets a table mix several outcomes with different
  families, each column keeping its own effect wording.

- role:

  For regression tables
  ([`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)):
  the column's role, `"model"` for a model-estimate column or `"emp"`
  for an empirical (crude) companion column. Empty (`""`) on
  cross-tables. Read by the colour legend to name each column's effect
  without matching its label.

- conf_level:

  The confidence level this column's interval and thresholds were
  computed at, as a single number in (0, 1). `NA` (default) means
  "unknown" — the colour engine then falls back to
  `options("tabxplor.conf_level")`. Stored per COLUMN, because colours
  are resolved per column at print time and cannot see the table's
  `conf_level` argument.

- degf:

  The degrees of freedom this column's interval is referred to. On a
  cross-table that is the survey design's `#PSU - #strata`, which
  matters below ~30 primary sampling units; on a regression it is the
  fitted model's own residual df (for an `svyglm`, `degf + 1 - p`), so a
  model column and its observed companion legitimately differ. `NA`
  (default) means "refer to the normal quantile".

- basis:

  How this column's interval and significance were computed — `"n"` (the
  raw sample size), `"weights"` (the design effect of the weights),
  `"design"` (a full `survey` design), or `"design_partial"` (a design
  was given but its variance could not be computed). Default `"n"`. A
  per-COLUMN fact, so a table states honestly what its numbers carry
  even after a pipeline drops the table's metadata; binding columns
  keeps the WEAKEST basis.

- ci_method:

  Which interval ENGINE built this column's bounds — `"wilson"`,
  `"wald"`, `"beta"` (a cell proportion), `"newcombe"`, `"ac"` (a
  difference of proportions), `"katz"` (a ratio of proportions),
  `"welch"`, `"student"`, `"ols"` (a difference of means), `"robust"`,
  `"quasipoisson"`, `"poisson"` (a ratio of means), `"woolf"`,
  `"wald_log"`, `"profile"`; `""` (default) when the column carries no
  interval. Read back by the colour legend, so it always names the
  method the bounds were built with.

- ...:

  In `fmt()`, it exists only for the arguments retired in tabxplor
  2.0.0: `type` is translated into `scale` + `pct_type` (see
  [`tabxplor-type`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-type.md)),
  `ci_type` gets an error naming its replacement. In the accessor
  methods below, to add arguments in the future.

- x:

  The object to test, to get a field in, or to modify.

## Value

A vector of class `tabxplor_fmt`.

A logical vector.

## Functions

- `is_fmt()`: a test function for class fmt.

## The fields of a cell

A `fmt` cell carries 21 fields. Many are `NA` when the quantity was not
requested; read one with `x$field` or
[`vctrs::field()`](https://vctrs.r-lib.org/reference/fields.html), and
see them all with
[`vctrs::vec_data()`](https://vctrs.r-lib.org/reference/vec_data.html):

- `n` — the unweighted count.

- `display` — which field this cell shows (a bare name, or a
  [`{}`](https://rdrr.io/r/base/Paren.html) template).

- `digits` — how many decimals this cell prints.

- `wn` — the weighted count.

- `pct` — the percentage.

- `mean` — the mean, on a numeric column variable.

- `diff` — the difference from the reference cell (percentage points, or
  the outcome's own units).

- `ratio` — the ratio to the reference cell (a relative risk, or a ratio
  of means).

- `ctr` — the cell's contribution to the table's Chi-2.

- `var` — the column's variance quantity – which one is given by its
  `scale`.

- `ci_inf` — the lower bound of the confidence interval.

- `ci_sup` — the upper bound of the confidence interval.

- `pvalue` — the cell's own significance p-value, which the stars read.

- `or` — the odds ratio against the `ref2` level.

- `tot_n` — the cell's own base — the count its percentage is computed
  on.

- `n_eff` — the effective sample size its interval was computed on
  (weights or a survey design).

- `obs` —
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  only: the observed (crude) effect the modelled one is compared to.

- `gap_se` —
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  only: the standard error of the gap between the estimate and `obs`.

- `row_kind` — what kind of row the cell sits in — see
  [`get_row_kind()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md).

- `in_tottab` — is the cell in a total table (logical).

- `in_refrow` — is the cell in a reference row (logical).

## Every display token

Generated from the package's own display table, so it cannot drift from
what
[`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
reads. Each of `pct`, `n`, `wn`, `mean`, `diff`, `ratio`, `or`, `ctr`,
`var`, `obs`, `pvalue` shows the field of the same name, described
above. The rest are composed or derived by the pipeline itself, and the
last few are not meant to be typed:

- `est` — the estimate, whatever this column estimates — an odds ratio,
  a risk difference, a coefficient, a percentage. The one token that
  means the same thing on every table.

- `base` — the level the estimate sits on: the percentage, the mean or
  the count. On a plain percentage table it is the same number as `est`;
  beside a regression effect it is the adjusted prediction.

- `ci` — the confidence interval of whatever the column compares, as
  `[low;high]`.

- `moe` — the margin of error — the same interval as `ci`, written as
  the half-width `+/-x` around the estimate. Void where the column
  compares a RATIO: a ratio's interval is symmetric on the LOG scale, so
  it has no half-width.

- `sd` — the standard deviation, in the variable's own unit.

- `cv` — the coefficient of variation — the standard deviation as a
  percentage of the mean.

- `resid` — the adjusted standardized residual – whether the cell
  departs from independence. Derived from the p-value and the sign of
  `ctr`, so it is read-only.

- `coef` — the estimate on the model's LINK scale — the coefficient a
  linear or log-link model fitted. The same number as `est` where the
  column is already additive, its logarithm where the column shows a
  ratio.

- `gap` — how far adjustment moved the effect: the gap between the
  modelled estimate and its observed counterpart, on the estimate's own
  scale. What `color = "adjustment"` grades — readable in print and
  Excel, not only in an html tooltip.

- `gof` — a model-fit statistic (N, R2, AIC, BIC, dispersion).

- `gof_warn` — a model-fit statistic past the threshold its check is
  read against.

- `n_range` — the unweighted base: one count, or a `min-max` range over
  the table.

- `blank` — nothing: a cell masked by `n_min`.

- `rr` — the legacy synonym of `ratio`, still accepted.

- `OR` — the acronym spelling of `or`, still accepted.

## See also

[tabxplor-display](https://bricenocenti.github.io/tabxplor/reference/tabxplor-display.md)
for the [`{}`](https://rdrr.io/r/base/Paren.html) grammar and the named
layouts `display` accepts;
[fmt_fields](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
and
[fmt_attributes](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
for the accessors.

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

f <- fmt(n = c(7, 19, 2), pct = c(0.25, 0.679, 0.07),
         scale = "level_pct", pct_type = "row")
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
#>   tot_n n_eff obs gap_se row_kind in_tottab in_refrow
#> 1    NA    NA  NA     NA     data     FALSE     FALSE
#> 2    NA    NA  NA     NA     data     FALSE     FALSE
#> 3    NA    NA  NA     NA     data     FALSE     FALSE

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
#> [13] "pvalue"    "or"        "tot_n"     "n_eff"     "obs"       "gap_se"   
#> [19] "row_kind"  "in_tottab" "in_refrow"
#> 
#> $scale
#> [1] "level_pct"
#> 
#> $comp_all
#> [1] NA
#> 
#> $ref
#> [1] ""
#> 
#> $pct_type
#> [1] "row"
#> 
#> $col_var
#> [1] ""
#> 
#> $col_group
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
#> $conf_level
#> [1] NA
#> 
#> $degf
#> [1] NA
#> 
#> $basis
#> [1] "n"
#> 
#> $ci_method
#> [1] ""
#> 
#> $class
#> [1] "tabxplor_fmt" "vctrs_rcrd"   "vctrs_vctr"  
#> 

# To modify the "pct_type" attribute of a fmt vector (what the percentage is a percentage OF) :
set_pct_type(f, "col")
#> <fmt-col%[3]>
#> [1] 25% 68% 7% 

# To modify the "color" attribute of a fmt vector :
set_color(f, "contrib")
#> <fmt-row%[3]>
#> [1] 25% 68% 7% 




tabs <- tab(starwars, sex, hair_color, gender, na = "drop", pct = "row",
            other_if_less_than = 5)

# To identify the total columns, and work with them :
is_totcol(tabs)
#> gender    sex  black  brown   none Others  Total 
#>  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE   TRUE 
tabs |> mutate(across(where(is_totcol), ~ "total column"))
#> # A tabxplor tab: 8 × 8
#> # Groups:         gender [3]
#>   gender    sex              black  brown   none Others Total          n
#>                             <row%> <row%> <row%> <row%>              <n>
#> 1 feminine  female             19%    31%    31%    19% total column  16
#> 2 feminine  Others              0%     0%   100%     0% total column   1
#> 3 feminine  Total feminine     18%    29%    35%    18% total column  17
#> 
#> 4 masculine male               15%    19%    49%    17% total column  59
#> 5 masculine none                0%     0%   100%     0% total column   2
#> 6 masculine Others              0%     0%     0%     0% total column   0
#> 7 masculine Total masculine    15%    18%    51%    16% total column  61
#> 
#> 8 Ensemble  Total Ensemble     15%    21%    47%    17% total column  78

# To identify the total rows, and work with them :
is_totrow(tabs)
#> [1] FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE
tabs |>
  mutate(across(
    where(is_fmt),
    ~ if_else(is_totrow(.), true = "into_total_row", false = "normal_cell")
  ))
#> # A tabxplor tab: 8 × 7
#> # Groups:         gender [3]
#>   gender    sex             black          brown          none      Others Total
#>                                                                                 
#> 1 feminine  female          normal_cell    normal_cell    normal_c… norma… norm…
#> 2 feminine  Others          normal_cell    normal_cell    normal_c… norma… norm…
#> 3 feminine  Total feminine  into_total_row into_total_row into_tot… into_… into…
#> 
#> 4 masculine male            normal_cell    normal_cell    normal_c… norma… norm…
#> 5 masculine none            normal_cell    normal_cell    normal_c… norma… norm…
#> 6 masculine Others          normal_cell    normal_cell    normal_c… norma… norm…
#> 7 masculine Total masculine into_total_row into_total_row into_tot… into_… into…
#> 
#> 8 Ensemble  Total Ensemble  into_total_row into_total_row into_tot… into_… into…

# To identify the total tables, and work with them :
tottabs <- is_tottab(tabs)
tabs |> tibble::add_column(tottabs) |>
  mutate(total = if_else(tottabs, "part of a total table", "normal cell"))
#> # A tabxplor tab: 8 × 9
#> # Groups:         gender [3]
#>   gender    sex              black  brown   none Others      Total tottabs total
#>                             <row%> <row%> <row%> <row%> <row% (n)> <lgl>        
#> 1 feminine  female             19%    31%    31%    19%  100% (16) FALSE   norm…
#> 2 feminine  Others              0%     0%   100%     0%  100% ( 1) FALSE   norm…
#> 3 feminine  Total feminine     18%    29%    35%    18%  100% (17) FALSE   norm…
#> 
#> 4 masculine male               15%    19%    49%    17%  100% (59) FALSE   norm…
#> 5 masculine none                0%     0%   100%     0%  100% ( 2) FALSE   norm…
#> 6 masculine Others              0%     0%     0%     0%    0% ( 0) FALSE   norm…
#> 7 masculine Total masculine    15%    18%    51%    16%  100% (61) FALSE   norm…
#> 
#> 8 Ensemble  Total Ensemble     15%    21%    47%    17%  100% (78) TRUE    part…

# To access the displayed numbers, as numeric vectors :
tabs |> mutate(across(where(is_fmt), get_num))
#> # A tabxplor tab: 8 × 7
#> # Groups:         gender [3]
#>   gender    sex             black brown  none Others Total
#>                             <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1 feminine  female          0.188 0.312 0.312  0.188     1
#> 2 feminine  Others          0     0     1      0         1
#> 3 feminine  Total feminine  0.176 0.294 0.353  0.176     1
#> 
#> 4 masculine male            0.153 0.186 0.492  0.169     1
#> 5 masculine none            0     0     1      0         1
#> 6 masculine Others          0     0     0      0         0
#> 7 masculine Total masculine 0.148 0.180 0.508  0.164     1
#> 
#> 8 Ensemble  Total Ensemble  0.154 0.205 0.474  0.167     1

# To access the displayed numbers, as character vectors (without colors) :
tabs |> mutate(across(where(is_fmt), format))
#> # A tabxplor tab: 8 × 7
#> # Groups:         gender [3]
#>   gender    sex             black brown none  Others Total
#>                                                           
#> 1 feminine  female          19%   31%   31%   19%    100% 
#> 2 feminine  Others          0%    0%    100%  0%     100% 
#> 3 feminine  Total feminine  18%   29%   35%   18%    100% 
#> 
#> 4 masculine male            15%   19%   49%   17%    100% 
#> 5 masculine none            0%    0%    100%  0%     100% 
#> 6 masculine Others          0%    0%    0%    0%     0%   
#> 7 masculine Total masculine 15%   18%   51%   16%    100% 
#> 
#> 8 Ensemble  Total Ensemble  15%   21%   47%   17%    100% 

# To access the (non-displayed) differences of the cells percentages from totals :
tabs |> mutate(across(where(is_fmt), ~ vctrs::field(., "diff")))
#> # A tabxplor tab: 8 × 7
#> # Groups:         gender [3]
#>   gender    sex                black    brown    none   Others Total
#>                                <dbl>    <dbl>   <dbl>    <dbl> <dbl>
#> 1 feminine  female           0.0110   0.0184  -0.0404  0.0110      0
#> 2 feminine  Others          -0.176   -0.294    0.647  -0.176       0
#> 3 feminine  Total feminine   0        0        0       0           0
#> 
#> 4 masculine male             0.00500  0.00611 -0.0167  0.00556     0
#> 5 masculine none            -0.148   -0.180    0.492  -0.164       0
#> 6 masculine Others          -0.148   -0.180   -0.508  -0.164      -1
#> 7 masculine Total masculine  0        0        0       0           0
#> 
#> 8 Ensemble  Total Ensemble   0        0        0       0           0


# To do more complex operations, like creating a new column with standard deviation and
# print it with 2 decimals, use `dplyr::mutate` on all the fmt columns of a table :

tab(forcats::gss_cat, race, c(age, tvhours), marital, digits = 1L, comp = "all",
    color = "auto") |>
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
#>    marital     race                          age       tvhours age_sd tvhours_sd
#>                                      <mean (cv)>   <mean (cv)> <mean> <mean-var>
#>  1 No answer   Other               34.0 (cv 25%) 2.0             8.49           
#>  2 No answer   Black               64.0                                         
#>  3 No answer   White               56.0 (cv 28%) 2.6 (cv  45%)  15.71       1.19
#>  4 No answer   Total No answer     52.4 (cv 32%) 2.6 (cv  44%)  16.51       1.13
#> 
#>  5 Never marr… Other               30.2 (cv 35%) 2.8 (cv  94%)  10.60       2.67
#>  6 Never marr… Black               34.5 (cv 35%) 4.2 (cv  82%)  12.14       3.39
#>  7 Never marr… White               34.4 (cv 41%) 2.8 (cv  93%)  14.29       2.56
#>  8 Never marr… Total Never married 33.9 (cv 40%) 3.1 (cv  92%)  13.47       2.86
#> 
#>  9 Separated   Other               42.5 (cv 30%) 3.3 (cv  99%)  12.97       3.26
#> 10 Separated   Black               46.2 (cv 29%) 5.1 (cv  93%)  13.36       4.73
#> 11 Separated   White               45.6 (cv 30%) 2.9 (cv  96%)  13.52       2.77
#> 12 Separated   Total Separated     45.3 (cv 30%) 3.5 (cv 101%)  13.43       3.60
#> 
#> 13 Divorced    Other               45.5 (cv 26%) 3.0 (cv  92%)  11.82       2.71
#> 14 Divorced    Black               51.0 (cv 25%) 4.3 (cv  88%)  12.67       3.74
#> 15 Divorced    White               51.6 (cv 26%) 2.9 (cv  85%)  13.22       2.43
#> 16 Divorced    Total Divorced      51.1 (cv 26%) 3.1 (cv  89%)  13.14       2.73
#> 
#> 17 Widowed     Other               64.5 (cv 23%) 4.2 (cv  67%)  14.84       2.79
#> 18 Widowed     Black               67.5 (cv 21%) 4.7 (cv  78%)  13.89       3.70
#> 19 Widowed     White               72.8 (cv 17%) 3.7 (cv  72%)  12.48       2.70
#> 20 Widowed     Total Widowed       71.7 (cv 18%) 3.9 (cv  74%)  13.00       2.90
#> 
#> 21 Married     Other               42.2 (cv 31%) 2.5 (cv  76%)  13.01       1.88
#> 22 Married     Black               46.4 (cv 29%) 3.8 (cv  81%)  13.40       3.06
#> 23 Married     White               49.7 (cv 31%) 2.6 (cv  78%)  15.24       1.98
#> 24 Married     Total Married       48.7 (cv 31%) 2.7 (cv  80%)  15.06       2.11
#> 
#> 25 Ensemble    Total Ensemble      47.2 (cv 37%) 3.0 (cv  87%)  17.29       2.59
#> # ratio (Total Ensemble): ÷2 ÷1.5 ÷1.2 ÷1.1 ×1.1 ×1.2 ×1.5 ×2
```
