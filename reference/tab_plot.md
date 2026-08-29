# Print a tabxplor table as plot (defunct)

**\[defunct\]**

Removed in 2.0.0. `tab_plot()` drew a *picture of the table* as a ggpubr
image. Use
[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
[`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
or
[`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
to export the table itself, and
[`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
for a chart of the numbers – every estimate with its confidence
interval, its significance and its colour.

It was the only part of the package needing ggpubr, cowplot and gtable,
whose dependency trees every user paid for; its display never matched
the other backends'.

## Usage

``` r
tab_plot(tabs, ...)
```

## Arguments

- tabs:

  A data.frame.

- ...:

  Ignored.

## Value

Never returns: it errors.
