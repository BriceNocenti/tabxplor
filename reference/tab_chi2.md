# Add Chi2 summaries to a [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)

**\[deprecated\]**

Deprecated in 2.0.0, defunct in 2.1.0 – the whole-table test and the
per-cell contributions are computed directly by
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
through its `test` and `color` arguments. `tab_chi2()` still works on an
existing tab, reconstructing that plan from the table's own markers.

## Usage

``` r
tab_chi2(
  tabs,
  calc = c("ctr", "p", "var", "counts"),
  comp = NULL,
  color = c("no", "auto", "all", "all_pct"),
  .deff = NULL
)
```

## Arguments

- tabs:

  A `tibble` of class `tab`, made with
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
  or [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- calc:

  Which elements of the Chi2 summary to compute, as a selection in
  `c("ctr", "p", "var", "counts")` : contributions to variance, pvalue,
  variance and unweighted count. All of them by default.

- comp:

  Comparison level, when `tab_vars` are present : contributions to
  variance are calculated for each subtable/group (by default,
  `comp = "tab"`) or for the whole set of tables (`comp = "all"`). It
  must be set once and for all the first time you use
  [`tab_pct`](https://bricenocenti.github.io/tabxplor/reference/tab_pct.md)
  with rows,
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  or `tab_chi2`.

- color:

  The type of colors to print, as a single string: `"no"` (the default),
  `"all"` (color all cells by their contribution to variance, except
  mean columns), `"all_pct"` (all percentage cells), or `"auto"` (only
  columns with counts, `pct = "all"` or `pct = "all_tabs"`).

- .deff:

  Internal pipeline seam. The design-based omnibus grid (one row per
  subtable x col_var, carrying Rao-Scott's mean generalized design
  effect), used as the divisor of the `color = "contrib"` residual's
  base when the table's inference basis is not `"n"`. `NULL` — the
  default, and every direct call — keeps the unweighted base.

## Value

A `tibble` of class `tab`, with Chi2 summaries as metadata, possibly
colored based on contributions of cells to variance.
