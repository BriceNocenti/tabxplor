# Print a tabxplor html table

Opens the html table
[`tab_kable`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
returned in the Viewer, on a page painted to match it – so a
`theme = "dark"` table no longer sits in a white pane. Under
`theme = "auto"` the theme is resolved from **your editor** rather than
your operating system: the Viewer is a webview, and its
`prefers-color-scheme` reports the OS, so it cannot see the editor the
table is sitting in. Anything else – a non-interactive print, a knitted
document, or a table tabxplor did not style (`css = FALSE`, or the
kableExtra engine) – prints exactly as kableExtra does.

## Usage

``` r
# S3 method for class 'tabxplor_kable'
print(x, ...)
```

## Arguments

- x:

  A html table returned by
  [`tab_kable`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md).

- ...:

  Passed to kableExtra's print method.

## Value

`x`, invisibly.

## See also

[`tab_kable`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
[`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
