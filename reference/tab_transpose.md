# Swap the rows and columns of a cross-table

Turns a table's rows into its columns and its columns into its rows, and
returns a real `tabxplor_tab` — one you can keep piping through dplyr,
colour, print and export. Row percentages become column percentages, and
the old total column and total row swap places.

Its job is the **profile table**: many variables down the page, a few
groups across it. It is also the only way to put a *mean* on a row,
since a number given to `row_vars` is always cut into levels — build the
means as columns (`tab(data, groups, numeric_vars)`) and transpose.

Use the exporters' `transpose = TRUE` argument instead whenever you only
need the OUTPUT: it flips the finished render model after every colour
and cell string is computed, so it handles what a data-level flip cannot
(several row variables, `tab_vars` sub-tables, columns of unlike kinds).

    tab(data, row_vars, col_vars, pct = "row") |> tab_kable(transpose = TRUE)   # or tab_md() / tab_xl()

## Usage

``` r
tab_transpose(tabs, name = NULL)
```

## Arguments

- tabs:

  A single table made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) (one
  row variable, one column variable; not a subtabled table with
  `tab_vars`, and at most one total row and one total column).

- name:

  The name to give the new first (label) column, holding the old
  column-variable levels. `NULL` (default) uses the old column-variable
  name.

## Value

A transposed `tabxplor_tab`.

## Columns of unlike kinds

A transposed column stacks whatever the original rows held, so
transposing a table that mixes percentage and mean columns gives a
`mixed` column. Its numbers and its cell layouts are exact; only the
colour ladder is shared, so an *additive* measure
(`color = "difference"`) grades the percentage cells and leaves the
others uncoloured, while a *multiplicative* one (`color = "ratio"`)
grades them all — the percentage and mean ratio ladders being the same
rungs.

## See also

[`tab_spread`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md),
[`tab_compact`](https://bricenocenti.github.io/tabxplor/reference/tab_compact.md),
[`tab_export`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md).

## Examples

``` r
# race x marital, read as marital x race:
tab(forcats::gss_cat, marital, race, pct = "row") |> tab_transpose()
#> # A tabxplor tab: 5 × 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married  Total
#>               <col%>          <col%>    <col%>   <col%>  <col%>  <col%> <col%>
#> 1 Other          12%             12%       15%       6%      4%      9%     9%
#> 2 Black          12%             24%       26%      15%     14%      9%    15%
#> 3 White          76%             64%       59%      79%     82%     82%    76%
#> 4 Total         100%            100%      100%     100%    100%    100%   100%
#> 5 n               17           5 416       743    3 383   1 807  10 117 21 483

# \donttest{
# the profile table: mean rows come from mean columns
tab(forcats::gss_cat, marital, c(age, tvhours)) |> tab_transpose()
#> # A tabxplor tab: 2 × 8
#>   age      `No answer` `Never married`     Separated     Divorced      Widowed
#>            <mean (cv)>     <mean (cv)>   <mean (cv)>  <mean (cv)>  <mean (cv)>
#> 1 age      52 (cv 32%)     34 (cv 40%)  45 (cv  30%)  51 (cv 26%)  72 (cv 18%)
#> 2 tvhours 2.6 (cv 44%)    3.1 (cv 92%) 3.5 (cv 101%) 3.1 (cv 89%) 3.9 (cv 74%)
#> # ℹ 2 more variables: Married <mean (cv)>, Total <mean (cv)>
# }
```
