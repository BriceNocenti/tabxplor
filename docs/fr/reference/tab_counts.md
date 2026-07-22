# Cross-table from already-aggregated counts ("from the middle")

`tab_counts()` builds the same color-coded cross-table as
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md),
but from data that is **already cross-tabulated** (a table of counts)
rather than from microdata (one row per individual). This is the common
case when you start from a
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
result, a contingency table, or a published table of counts or
percentages. All the usual calculations — percentages, differences,
confidence intervals, chi-squared, colors, totals — are done on the
counts, and the result is identical to the table
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
would build from the underlying microdata.

It accepts four input shapes:

- **Long tidy counts** (the default): one row per `row_var` \\\times\\
  `col_var` (\\\times\\ `tab_vars`) combination, with the count in
  `counts` (and, weighted, the weighted count in `wt_counts`).

- **A wide `data.frame`**: a label (`row_var`) column plus one column
  per `col_var` level — select those level columns with `cols` and name
  the column variable with `col_name`.

- **A `table` / `xtabs` / `matrix` object**: melted automatically; the
  row/column variables are read from the dimnames (or set with `row_var`
  / `col_var`).

- **Frequencies + base N**: as the wide shape, plus `input = "pct"` and
  `base` (the column of row sample sizes); the integer counts are
  rebuilt from the percentages and the base.

For weighted data, supply the real (unweighted) count in `counts`
**and** the weighted count in `wt_counts`: estimates use the weighted
counts while confidence intervals and tests use the real unweighted
sample size. When the counts are not real whole numbers (a base-less /
weighted-only input), confidence intervals and chi-squared are disabled
with a message.

## Usage

``` r
tab_counts(
  data,
  row_var,
  col_var,
  tab_vars,
  counts,
  wt_counts,
  cols,
  col_name = "variable",
  base,
  input = c("counts", "pct"),
  pct = "no",
  color = "no",
  color_signif = "ignore",
  OR = "no",
  test = FALSE,
  na = "keep",
  cleannames = NULL,
  ref = "auto",
  ref2 = "first",
  comp = "tab",
  ci = "no",
  conf_level = 0.95,
  stars = NULL,
  method_cell = "wilson",
  method_diff = "newcombe",
  method_ratio = "katz",
  totaltab = "line",
  totaltab_name = "Ensemble",
  tot = c("row", "col"),
  total_names = "Total",
  add_n = TRUE,
  add_pct = FALSE,
  common_totrow = FALSE,
  subtext = "",
  digits = 0,
  n_min = 0,
  display = NULL,
  color_breaks = NULL,
  spread_vars = character(),
  names_prefix = NULL,
  names_sort = FALSE,
  chi2 = lifecycle::deprecated()
)
```

## Arguments

- data:

  A data frame of counts, or a `table` / `xtabs` / `matrix` object.

- row_var:

  The row variable (one level per line). For a `table` object it
  defaults to the first dimension.

- col_var:

  The column variable (one column per level). For a `table` object it
  defaults to the second dimension. Not used with `cols`.

- tab_vars:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Tab variables: a subtable is made for each combination of their
  levels.

- counts:

  The column holding the **unweighted** count for each cell (long tidy
  shape).

- wt_counts:

  Optional column holding the **weighted** count for each cell. Leave
  empty for an unweighted table.

- cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  For a wide `data.frame`: the columns holding the `col_var` levels.

- col_name:

  Name of the (synthesised) column variable when `cols` is used.

- base:

  For `input = "pct"`: the column holding each row's sample size N.

- input:

  `"counts"` (default) or `"pct"` (with `cols` and `base`: the level
  columns hold frequencies, and counts are rebuilt from them and
  `base`).

- pct, color, color_signif, OR, test, na, cleannames, ref, ref2, comp,
  ci, conf_level, stars, method_cell, method_diff, method_ratio,
  totaltab, totaltab_name, tot, total_names, add_n, add_pct,
  common_totrow, subtext, digits, n_min, display, color_breaks,
  spread_vars, names_prefix, names_sort:

  Same meaning as in
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).
  `color` accepts every form
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  does (`FALSE` / `TRUE` / a measure / `c(text, background)` /
  `list(pct =, mean =)`). Only `na = "keep"` / `"drop"` are available
  (`"drop_all"` / `"common_base"` need the microdata). The
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  arguments that pick or collapse levels *during the microdata prep* —
  which `tab_counts()` starts past — are not offered: `levels = "first"`
  / `"auto"` (keeping a subset of levels), `other_if_less_than` /
  `other_level` (lumping rare levels counts individual observations);
  build from microdata with
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  for those. Likewise the microdata-only / numeric-mean-only arguments:
  `wt` (use `wt_counts`); the survey design `ids`/`strata`/`fpc`/`nest`;
  `method_mean_diff`/`method_mean_ratio` (a counts table has no numeric
  column); `parallel`; `output_list`; `sup_cols`.

- chi2:

  **\[deprecated\]** Renamed to `test` in 1.4.0 (see
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)).

## Value

A `tabxplor_tab` (or `tabxplor_grouped_tab` when `tab_vars` are
provided).

## Examples

``` r
# Long tidy counts (as from dplyr::count()) reproduce the microdata table :
counts <- dplyr::count(forcats::gss_cat, marital, race)
tab_counts(counts, marital, race, counts = n, pct = "row")
#> # A tabxplor tab: 7 × 5
#>   marital        Other  Black  White           Total
#>   <fct>         <row%> <row%> <row%>          <row%>
#> 1 No answer        12%    12%    76% 100% (n=    17)
#> 2 Never married    12%    24%    64% 100% (n= 5 416)
#> 3 Separated        15%    26%    59% 100% (n=   743)
#> 4 Divorced          6%    15%    79% 100% (n= 3 383)
#> 5 Widowed           4%    14%    82% 100% (n= 1 807)
#> 6 Married           9%     9%    82% 100% (n=10 117)
#> 7 Total             9%    15%    76% 100% (n=21 483)
# tab(forcats::gss_cat, marital, race, pct = "row")   # identical

# A contingency table object :
tab_counts(table(forcats::gss_cat$marital, forcats::gss_cat$race), pct = "row")
#> # A tabxplor tab: 7 × 5
#>   Var1           Other  Black  White           Total
#>   <fct>         <row%> <row%> <row%>          <row%>
#> 1 No answer        12%    12%    76% 100% (n=    17)
#> 2 Never married    12%    24%    64% 100% (n= 5 416)
#> 3 Separated        15%    26%    59% 100% (n=   743)
#> 4 Divorced          6%    15%    79% 100% (n= 3 383)
#> 5 Widowed           4%    14%    82% 100% (n= 1 807)
#> 6 Married           9%     9%    82% 100% (n=10 117)
#> 7 Total             9%    15%    76% 100% (n=21 483)

# A wide data.frame of counts :
wide <- tidyr::pivot_wider(counts, names_from = race, values_from = n)
tab_counts(wide, row_var = marital, cols = c(Other, Black, White),
           col_name = "race", pct = "row")
#> # A tabxplor tab: 7 × 5
#>   marital        Other  Black  White           Total
#>   <fct>         <row%> <row%> <row%>          <row%>
#> 1 No answer        12%    12%    76% 100% (n=    17)
#> 2 Never married    12%    24%    64% 100% (n= 5 416)
#> 3 Separated        15%    26%    59% 100% (n=   743)
#> 4 Divorced          6%    15%    79% 100% (n= 3 383)
#> 5 Widowed           4%    14%    82% 100% (n= 1 807)
#> 6 Married           9%     9%    82% 100% (n=10 117)
#> 7 Total             9%    15%    76% 100% (n=21 483)
```
