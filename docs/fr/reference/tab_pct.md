# Add percentages and diffs to a [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)

**\[superseded\]**

Superseded (2.0.0): percentages, differences and ratios are computed
directly by
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md) /
[`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
via the `pct` / `ref` arguments. `tab_pct()` still works on an existing
tab.

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
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
  or
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

- pct:

  The type of percentages to calculate. `"row"` draw row percentages.
  Set to `"col"` for column percentages. Set to `"all"` for frequencies
  (based on each subtable/group if `tab_vars` is provided). Set to
  `"all_tabs"` to calculate frequencies based on the whole (set of)
  table(s).

- digits:

  The number of digits to print for percentages. As a single integer, or
  an integer vector the same length than `col_vars`.

- ref:

  The reference cell to calculate differences and ratios (used to print
  `colors`) :

  - `"auto"`: by default, cell difference from the corresponding total
    (rows or cols depending on `pct = "row"` or `pct = "col"`) is used
    for `diff` ; cell ratio from the first line (or col) is use for `OR`
    (odds ratio/relative risks ratio).

  - `"tot"`: totals are always used.

  - `"first"`: calculate cell difference or ratio from the first cell of
    the row or column (useful to color temporal developments).

  - `n`: when `ref` is an integer, the nth row (or column) is used for
    comparison.

  - `"regex"`: when `ref` is a string, it it used as a regular
    expression, to match with the names of the rows (or columns). Be
    precise enough to match only one column or row, otherwise you get a
    warning message.

  - `"no"`: not use ref and not calculate diffs to gain calculation
    time.

- comp:

  Comparison level. When `tab_vars` are present, should the row
  differences be calculated for each subtable/group (by default
  `comp = "tab"` : comparison of each cell to the relative total row) ?
  Should they be calculated for the whole table (`comp = "all"` :
  comparison of each cell to the total row of the total table) ? When
  `comp = "all"` and `ref = "first"`, cells are compared to the first
  cell of the total table instead. This parameter doesn't affect column
  percentages. `comp` must be set once and for all the first time you
  use
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md),
  `tab_pct` with rows, or
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md).

- color:

  Set to `TRUE` to color the resulting tab based on differences (from
  totals or from the first cell).

- just_diff:

  If percentages are already calculated and you just want to recalculate
  differences.

## Value

A `tibble` of class `tab`, with percentages displayed, possibly colored
based on differences from totals or first cell.
