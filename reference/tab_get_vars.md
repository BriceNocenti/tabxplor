# The variables of a tabxplor table

**\[superseded\]**

Which variable plays which role in a finished table: the row variable,
the column variable(s) and the sub-table variable(s). Read off the
table's own declared model (the index columns' stored roles and the
`fmt` columns' `col_var`), never guessed from a column name — so it
survives renaming, `dplyr` verbs and a merge of several row variables.

Superseded by
[`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md),
which answers the same question — as `row_vars`, `col_vars` and
`tab_vars` — and says in the same breath what kind of object the table
is (merged, grouped, a list) and which reshape operations accept it.
`tab_get_vars()` keeps working, unchanged.

## Usage

``` r
tab_get_vars(tabs, vars = c("row_var", "col_vars", "tab_vars"))
```

## Arguments

- tabs:

  A `tibble` of class `tab`, made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  or
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md).

- vars:

  A character vector naming the roles you want: `"row_var"`,
  `"col_vars"` or `"tab_vars"`.

## Value

A list with the variables names.

## See also

[`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md),
which reports the table's STRUCTURE (merged / grouped / list) and which
operations accept it.
