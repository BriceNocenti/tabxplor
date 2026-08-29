# Hand a table to base R

[`as.matrix()`](https://rdrr.io/r/base/matrix.html) gives the table's
numbers as a plain numeric matrix;
[`as.table()`](https://rdrr.io/r/base/table.html) gives the same matrix
as a base [`table`](https://rdrr.io/r/base/table.html), its `dimnames`
named after the row and column variables. That is the shape base R and
the packages built on it expect — a correspondence analysis,
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html),
[`mosaicplot()`](https://rdrr.io/r/graphics/mosaicplot.html):

    FactoMineR::CA(as.matrix(tab(forcats::gss_cat, race, marital)), graph = FALSE)

Only the DATA cells come across. The total row, the total columns and
the display-time rows (the base count, `add_pct`, the p-value and
model-fit lines) are dropped, because a test or an analysis run on a
table's own margins is wrong; `totals = TRUE` keeps them. Each cell
contributes the number it *shows*, so a plain
[`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) gives
counts, a `pct = "row"` table proportions, and a numeric column means.

## Usage

``` r
# S3 method for class 'tabxplor_tab'
as.matrix(x, totals = FALSE, ...)

# S3 method for class 'tabxplor_tab'
as.table(x, totals = FALSE, ...)
```

## Arguments

- x:

  A table made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  [`tab_counts`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
  or
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md).

- totals:

  Set to `TRUE` to keep the total row, the total columns and the
  display-time rows.

- ...:

  Not used.

## Value

A numeric `matrix`, or a base `table`.

## Functions

- `as.matrix(tabxplor_tab)`: the table's numbers as a numeric matrix

- `as.table(tabxplor_tab)`: the same, as a base `table` with named
  dimnames

## See also

[`get_num`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md),
[`tab_export`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md).

## Examples

``` r
tabs <- tab(forcats::gss_cat, race, marital)
as.matrix(tabs)
#>       No answer Never married Separated Divorced Widowed Married
#> Other         2           633       110      212      70     932
#> Black         2          1305       196      495     262     869
#> White        13          3478       437     2676    1475    8316
as.table(tabs)
#>        marital
#> race    No answer Never married Separated Divorced Widowed Married
#>   Other         2           633       110      212      70     932
#>   Black         2          1305       196      495     262     869
#>   White        13          3478       437     2676    1475    8316

# a row-percentage table gives proportions, not counts:
as.matrix(tab(forcats::gss_cat, race, marital, pct = "row"))
#>          No answer Never married  Separated  Divorced    Widowed   Married
#> Other 0.0010209290     0.3231240 0.05615110 0.1082185 0.03573252 0.4757529
#> Black 0.0006391818     0.4170662 0.06263982 0.1581975 0.08373282 0.2777245
#> White 0.0007929247     0.2121378 0.02665447 0.1632205 0.08996645 0.5072278
```
