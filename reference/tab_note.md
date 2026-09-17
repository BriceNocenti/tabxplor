# A note under a table

A small grid of **already-rendered** character columns, printed under a
table in the aside ink: a glossary, a range, a set of diagnostics —
something that belongs to the table without being a row of it. Attach it
with
[`set_footer_tabs`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.md),
beside (or instead of) a real table.

A plain `data.frame` passed to
[`set_footer_tabs()`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.md)
already renders as a note, its own names as headers, everything
left-aligned. `tab_note()` is for when that is not enough.

## Usage

``` r
tab_note(
  df,
  headers = NULL,
  align = NULL,
  grey = NULL,
  note = character(0),
  kind = NULL
)
```

## Arguments

- df:

  A data.frame of character columns, already formatted.

- headers:

  Column titles; defaults to `names(df)`.

- align:

  One of `"left"` / `"right"` per column; defaults to all left.

- grey:

  One logical per row: a row to render in the dimmer aside ink (a result
  the note itself marks as not to be read). `NULL` for none.

- note:

  One or more lines printed under the grid, smaller still — what a cell
  cannot say for itself.

- kind:

  Per column, `"text"` (default) or `"spark"`, a run of block glyphs the
  html backend upgrades to an inline `<svg>`.

## Value

A `tabxplor_note`.

## See also

[`set_footer_tabs()`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.md)
to attach one,
[`set_subtext()`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md)
for the footer's text.

## Examples

``` r
n <- tab_note(data.frame(axis = c("1", "2"), variance = c("9.9%", "7.2%")),
              headers = c("Axis", "% variance"), align = c("left", "right"))
set_footer_tabs(tab(forcats::gss_cat, race, marital, pct = "row"), list("Axes" = n))
#> | Axis | % variance |
#> |:-----|-----------:|
#> | 1    |       9.9% |
#> | 2    |       7.2% |
#> 
#> # A tabxplor tab: 4 × 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>               <row%>          <row%>    <row%>   <row%>  <row%>  <row%>
#> 1 Other           0%             32%        6%      11%      4%     48%
#> 2 Black           0%             42%        6%      16%      8%     28%
#> 3 White           0%             21%        3%      16%      9%     51%
#> 4 Total           0%             25%        3%      16%      8%     47%
#> # ℹ 1 more variable: Total <row% (n)>
```
