# A declared tabxplor label column

`tabxplor_lvl` is a light **factor subclass** carrying what a row-index
column is *for*: its `role` (`"level"` / `"var"` / `"tab_var"`), the
`var` its labels belong to, and — per variable — whether that variable
was `ordered` in the source data. It is still a factor
([`is.factor()`](https://rdrr.io/r/base/factor.html) is `TRUE`), so
every base, dplyr and forcats operation keeps working unchanged.

## Usage

``` r
new_lvl(x, role = "level", var = NA_character_, ordered = NULL)

is_lvl(x)
```

## Arguments

- x:

  A factor (or anything [`factor()`](https://rdrr.io/r/base/factor.html)
  accepts).

- role:

  One of `"level"`, `"var"`, `"tab_var"`.

- var:

  The variable name the labels belong to; `NA` on a merged `levels`
  column.

- ordered:

  A named logical vector, one entry per variable, saying whether that
  variable was ordered in the source data. A single-variable column
  keeps its own `ordered` class as well.

## Value

A `tabxplor_lvl` vector.
