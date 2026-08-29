# Cross-tables from already-aggregated counts

`tab_counts()` builds the same color-coded cross-table as
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md), but
from data that is **already cross-tabulated** — a
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
result, a contingency table, or a published table of counts or
percentages — instead of microdata (one row per individual).
Percentages, differences, confidence intervals, chi-squared, colors and
totals are all computed from the counts, and the result is identical to
the table
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
would build from the microdata behind them.

It accepts four input shapes:

- **Long tidy counts** (the default): one row per `row_var` \\\times\\
  `col_var` (\\\times\\ `tab_vars`) combination, with the count in
  `counts` (and the weighted count in `wt_counts`).

- **A wide `data.frame`**: a label (`row_var`) column plus one column
  per `col_var` level — select those level columns with `cols` and name
  the column variable with `col_name`.

- **A `table` / `xtabs` / `matrix` object**: melted automatically, the
  row and column variables read from the dimnames (or set with `row_var`
  / `col_var`).

- **Frequencies + base N**: the wide shape, plus `input = "pct"` and
  `base` (the column of row sample sizes); the integer counts are
  rebuilt from the percentages and the base.

With weighted data, give the real (unweighted) count in `counts` **and**
the weighted count in `wt_counts`: estimates use the weighted counts,
while confidence intervals and tests use the real unweighted sample
size. Counts that are not whole numbers (weighted-only or frequency-only
input) disable confidence intervals and chi-squared, with a message.

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
  ...
)
```

## Arguments

- data:

  A data frame.

- row_var, col_var:

  **\[deprecated\]** Singular aliases of `row_vars`/`col_vars` (which
  now accept several variables). Kept working.

- tab_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Tab variables: one subtable per combination of their levels. Leave
  empty for a simple cross-table.

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

- ...:

  Every other argument of
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) –
  `pct`, `color`, `ci`, `tot`, ... – passed by name. See
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md); a
  typo gets a suggestion.

## Value

A `tabxplor_tab` (or `tabxplor_grouped_tab` when `tab_vars` are
provided).

## Examples

``` r
# Long tidy counts (as from dplyr::count()) reproduce the microdata table :
counts <- dplyr::count(forcats::gss_cat, marital, race)
tab_counts(counts, marital, race, counts = n, pct = "row")
#> # A tabxplor tab: 7 × 5
#>   marital        Other  Black  White         Total
#>                 <row%> <row%> <row%>    <row% (n)>
#> 1 No answer        12%    12%    76% 100% (    17)
#> 2 Never married    12%    24%    64% 100% ( 5 416)
#> 3 Separated        15%    26%    59% 100% (   743)
#> 4 Divorced          6%    15%    79% 100% ( 3 383)
#> 5 Widowed           4%    14%    82% 100% ( 1 807)
#> 6 Married           9%     9%    82% 100% (10 117)
#> 7 Total             9%    15%    76% 100% (21 483)
# tab(forcats::gss_cat, marital, race, pct = "row")   # identical

# A contingency table object :
tab_counts(table(forcats::gss_cat$marital, forcats::gss_cat$race), pct = "row")
#> # A tabxplor tab: 7 × 5
#>   Var1           Other  Black  White         Total
#>                 <row%> <row%> <row%>    <row% (n)>
#> 1 No answer        12%    12%    76% 100% (    17)
#> 2 Never married    12%    24%    64% 100% ( 5 416)
#> 3 Separated        15%    26%    59% 100% (   743)
#> 4 Divorced          6%    15%    79% 100% ( 3 383)
#> 5 Widowed           4%    14%    82% 100% ( 1 807)
#> 6 Married           9%     9%    82% 100% (10 117)
#> 7 Total             9%    15%    76% 100% (21 483)

# A wide data.frame of counts :
wide <- tidyr::pivot_wider(counts, names_from = race, values_from = n)
tab_counts(wide, row_var = marital, cols = c(Other, Black, White),
           col_name = "race", pct = "row")
#> # A tabxplor tab: 7 × 5
#>   marital        Other  Black  White         Total
#>                 <row%> <row%> <row%>    <row% (n)>
#> 1 No answer        12%    12%    76% 100% (    17)
#> 2 Never married    12%    24%    64% 100% ( 5 416)
#> 3 Separated        15%    26%    59% 100% (   743)
#> 4 Divorced          6%    15%    79% 100% ( 3 383)
#> 5 Widowed           4%    14%    82% 100% ( 1 807)
#> 6 Married           9%     9%    82% 100% (10 117)
#> 7 Total             9%    15%    76% 100% (21 483)
```
