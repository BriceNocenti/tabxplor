# Complete partial total rows

**\[deprecated\]**

A build-internal repair: after a reshape, a row that is a total in SOME
columns is made a total in all of them — **and so are `in_tottab` and
`in_refrow`**, which is why nothing calls it any more: after a spread
those two are facts about a column BLOCK, not about a row.
[`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
completes the row kind alone. It will be made internal in 2.1.0.

## Usage

``` r
complete_partial_totals(tabs)
```

## Arguments

- tabs:

  A table or data frame containing `tabxplor_fmt` columns.

## Value

The table with completed total rows, total tables, and reference rows.
