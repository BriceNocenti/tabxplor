# Odds-ratio forest plot of a tabxplor regression table

**\[experimental\]**

A finalfit-style forest plot of the odds ratios in a
[`tab_logit()`](https://bricenocenti.github.io/tabxplor/reference/tab_logit.md)
/
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
table: a log-scale point-and-interval plot beside a text table of the
estimates. It reads the stored `fmt` fields (odds ratio, confidence
interval, significance, count) directly – no model is re-fitted.

## Usage

``` r
or_plot(tabs, column = NULL, point_size = c(1.5, 6), title = NULL, ...)
```

## Arguments

- tabs:

  A `tabxplor` table from
  [`tab_logit()`](https://bricenocenti.github.io/tabxplor/reference/tab_logit.md)
  /
  [`multi_logit()`](https://bricenocenti.github.io/tabxplor/reference/multi_logit.md)
  /
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  (binomial / poisson / multinomial / ordinal – any table with an
  odds-ratio-shaped column).

- column:

  Optional column name to plot when the table has several odds-ratio
  columns (default: the first one).

- point_size:

  Length-2 numeric, the `ggplot2` point-size range mapped to the cell
  counts.

- title:

  Optional plot title (default: the plotted column's name).

- ...:

  Unused, for future extension.

## Value

Invisibly, the assembled `gtable` (drawn on the current graphics
device).

## Examples

``` r
data <- forcats::gss_cat |>
  dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
                                                "Married", "Not married")))
if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("gridExtra", quietly = TRUE)) {
  or_plot(tab_logit(data, "married", c("race", "age")))
}
```
