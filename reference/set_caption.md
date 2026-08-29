# Store a caption on a table

Records a caption/title on a `tabxplor_tab` that survives a dplyr
pipeline (it is kept in the table's `meta$vars$caption`, carried through
every verb) and is read by the exporters
([`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
[`tab_kable`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
[`tab_xl`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md))
as the table title, ahead of a regression table's auto-title, when the
exporter's own `caption=` argument is not supplied. `get_caption()`
reads it back (`NULL` when none is stored).

## Usage

``` r
set_caption(x, caption)

get_caption(x)
```

## Arguments

- x:

  A `tabxplor_tab` (or a `tabxplor_tabs` list of them).

- caption:

  A single string, or `NULL` / `NA` to remove any stored caption.

## Value

`x`, with its stored caption set (`set_caption`) ; the caption or `NULL`
(`get_caption`).
