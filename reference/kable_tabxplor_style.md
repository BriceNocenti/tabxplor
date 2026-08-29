# Print a tabxplor table in html (defunct)

**\[defunct\]**

Removed in 2.0.0. Use
[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
which renders any table – a `tabxplor_tab` or a plain data.frame –
through the shared exporter prep, with colours, tooltips and spanning
headers.

`kable_tabxplor_style()` predated
[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
and never shared its machinery: it found total rows and columns by
matching the literal strings `"Total"` / `"Ensemble"`, so it was
hardcoded to English and French. Nothing in the package ever called it.

## Usage

``` r
kable_tabxplor_style(tabs, ...)
```

## Arguments

- tabs:

  A data.frame.

- ...:

  Ignored.

## Value

Never returns: it errors.
