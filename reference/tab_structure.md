# The structure of a table

**\[experimental\]**

What kind of object a `tabxplor` result is, read from its own declared
model — the row-index columns (their stored roles) and the table's
stated kind — rather than guessed from column names.

## Usage

``` r
tab_structure(x)
```

## Arguments

- x:

  A `tabxplor_tab` / `tabxplor_grouped_tab`, or a list of them
  (`output_list = TRUE`).

## Value

A named list:

- `container`:

  `"table"` or `"list"`.

- `kind`:

  `"crosstab"` or `"regression"` (`NA` when the table carries no
  metadata).

- `merged`:

  `TRUE` when several row variables are stacked in one table (a `var`
  column names each row's variable).

- `grouped`:

  `TRUE` when the table has `tab_vars` (sub-tables).

- `row_vars`, `tab_vars`, `col_vars`:

  the variables on each axis.

- `same_col_vars`, `same_tab_vars`:

  for a list only: whether its tables agree.

## See also

[`tab_columns()`](https://bricenocenti.github.io/tabxplor/reference/tab_columns.md)
for the column-axis view.

## Examples

``` r
# \donttest{
t <- tab(forcats::gss_cat, c(marital, relig), race, pct = "row")
tab_structure(t)$merged
#> [1] TRUE
# }
```
