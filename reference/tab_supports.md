# Does this table's structure allow an operation?

The support matrix of the structure-sensitive operations, as a
predicate. Every place the package refuses a table for its structure
reads this same table of rules, so what is allowed can be *read* instead
of discovered.

## Usage

``` r
tab_supports(x, op)
```

## Arguments

- x:

  A table or list of tables — see
  [`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md).

- op:

  One of `"compact"`, `"transpose_object"` (the deprecated object-level
  [`tab_transpose()`](https://bricenocenti.github.io/tabxplor/reference/tab_transpose.md))
  or `"transpose_render"` (the `transpose = TRUE` argument of the
  exporters).

## Value

A single `TRUE`/`FALSE`.

## See also

[`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md).
