# Render a table as a plain pipe table

The markdown grid without the markup: one GFM pipe table, its unit line
kept (`<col%>`, `<var>`), no colour spans, no footer and no stylesheet.
It is what the console prints under a table for each of its subordinate
tables
([`set_footer_tabs`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.md)),
the same shape a regression's *shape table* takes there — a grid one can
read as text and paste anywhere.

It is
[`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
with three arguments fixed, not a second renderer: a pipe table that
drifted from the markdown export would be a second answer to one
question.

## Usage

``` r
tab_pipe(tabs, ...)
```

## Arguments

- tabs:

  A `tabxplor_tab`, or a list of them.

- ...:

  Passed to
  [`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  — `color = TRUE` brings the colour spans back, `subtext = TRUE` the
  footer.

## Value

A character vector, one element per line.

## See also

[`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
[`set_footer_tabs()`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.md).

## Examples

``` r
cat(tab_pipe(tab(forcats::gss_cat, race, marital, pct = "row")), sep = "\n")
#> | race    |No answer  |Never married  |Separated  |Divorced  |Widowed  |Married  |          Total  |
#> |:--------|----------:|--------------:|----------:|---------:|--------:|--------:|----------------:|
#> |         | *marital* |               |           |          |         |         |                 |
#> |         | *<row%>*  |               |           |          |         |         |   *<row% (n)>*  |
#> | Other   |       0%  |          32%  |       6%  |     11%  |     4%  |    48%  |  100% ( 1 959)  |
#> | Black   |       0%  |          42%  |       6%  |     16%  |     8%  |    28%  |  100% ( 3 129)  |
#> | White   |       0%  |          21%  |       3%  |     16%  |     9%  |    51%  |  100% (16 395)  |
#> |**Total**|     **0%**|        **25%**|     **3%**|   **16%**|   **8%**|  **47%**|**100%** (21 483)|
```
