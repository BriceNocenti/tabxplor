# Add percentages and diffs to a [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)

**\[deprecated\]**

Deprecated in 2.0.0, defunct in 2.1.0 – percentages, differences and
ratios are computed directly by
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
through its `pct` / `ref` / `comp` arguments. `tab_pct()` still works on
an existing tab.

## Usage

``` r
tab_pct(
  tabs,
  pct = "row",
  digits = NULL,
  ref = c("tot", "first", "no"),
  comp = NULL,
  color = FALSE,
  just_diff = FALSE
)
```

## Arguments

- tabs:

  A `tibble` of class `tab` made with
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
  or [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- pct:

  The type of percentages : `"row"`, `"col"`, `"all"` (frequencies of
  each subtable/group when `tab_vars` are provided), or `"all_tabs"`
  (frequencies of the whole set of tables).

- digits:

  The number of digits to print for percentages. As a single integer, or
  an integer vector the same length than `col_vars`.

- ref:

  The reference cell differences and ratios — and so `colors` — are
  calculated from : `"tot"` (the corresponding total row or column),
  `"first"` (the first cell of the row or column, useful to color
  temporal developments), an integer (the nth row or column), a string
  (a regular expression matching one row or column name, precise enough
  to match only one), or `"no"` to skip differences entirely. See
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) for
  the full vocabulary.

- comp:

  Comparison level, when `tab_vars` are present : `"tab"` (the default)
  compares each cell to the total row of its own subtable, `"all"` to
  the total row of the total table (and, with `ref = "first"`, to the
  first cell of the total table). It doesn't affect column percentages,
  and must be set once and for all the first time you use `tab_pct` with
  rows,
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  or
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/reference/tab_chi2.md).

- color:

  Set to `TRUE` to color the resulting tab based on those differences.

- just_diff:

  Set to `TRUE` when percentages are already calculated and you only
  want to recalculate differences.

## Value

A `tibble` of class `tab`, with percentages displayed, possibly colored
based on differences from totals or first cell.
