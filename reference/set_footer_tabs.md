# Attach subordinate tables under a table

Records one or more `tabxplor_tab`s that BELONG to `x` and are rendered
under it by every medium — the console,
[`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
[`tab_html`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
and
[`tab_xl`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
— exactly as if they had been passed in one list, and which travel with
`x` through a dplyr pipeline (they are kept in `x`'s
`meta$footer_tabs`).

The use is a fact that belongs to the table without being a row of it:
the eigenvalues of the axes beside a factorial-analysis summary, a
sample description beside the result it describes.

## Usage

``` r
set_footer_tabs(x, tabs)

get_footer_tabs(x)
```

## Arguments

- x:

  A `tabxplor_tab`.

- tabs:

  A `tabxplor_tab`, a list of them, or `NULL` to remove whatever is
  attached. A named element is captioned with its name
  ([`set_caption`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md))
  unless it carries a caption already.

## Value

`x`, with its subordinate tables set (`set_footer_tabs`) ; the list of
them, or `NULL` when none (`get_footer_tabs`).

## See also

[`new_tab()`](https://bricenocenti.github.io/tabxplor/reference/new_tab.md)
for the whole `meta` record.

## Examples

``` r
main <- tab(forcats::gss_cat, race, marital, pct = "row")
side <- tab(forcats::gss_cat, race)
main <- set_footer_tabs(main, list("Base" = side))
get_footer_tabs(main)
#> $Base
#> # A tabxplor tab: 4 × 2
#>   race        n
#>             <n>
#> 1 Other   1 959
#> 2 Black   3 129
#> 3 White  16 395
#> 4 Total  21 483
#> 
```
