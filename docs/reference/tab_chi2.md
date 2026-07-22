# Add Chi2 summaries to a [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)

Add Chi2 summaries to a
[`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)

## Usage

``` r
tab_chi2(
  tabs,
  calc = c("ctr", "p", "var", "counts"),
  comp = NULL,
  color = c("no", "auto", "all", "all_pct")
)
```

## Arguments

- tabs:

  A `tibble` of class `tab`, made with
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
  or
  [`tab_many`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md).

- calc:

  By default all elements of the Chi2 summary are calculated :
  contributions to variance, pvalue, variance and unweighted count. You
  can choose which are computed by selecting elements in the vector
  `c("ctr", "p", "var", "counts")`.

- comp:

  Comparison level. When `tab_vars` are present, should the
  contributions to variance be calculated for each subtable/group (by
  default, `comp = "tab"`) ? Should they be calculated for the whole
  table (`comp = "all"`) ? `comp` must be set once and for all the first
  time you use
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md),
  [`tab_num`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md)
  or `tab_chi2` with rows, or
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md).

- color:

  The type of colors to print, as a single string.

  - `"no"`: by default, no colors are printed

  - `"all"`: color all cells based on their contribution to variance
    (except for mean columns, from numeric variables)

  - `"all_pct"`: color all percentages cells based on their contribution
    to variance

  - `"auto"`: only color columns with counts, `pct = "all"` or
    `pct = "all_tabs"`

## Value

A `tibble` of class `tab`, with Chi2 summaries as metadata, possibly
colored based on contributions of cells to variance.
