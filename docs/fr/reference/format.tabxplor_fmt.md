# Print method for class tabxplor_fmt

Print method for class tabxplor_fmt

## Usage

``` r
# S3 method for class 'tabxplor_fmt'
format(
  x,
  ...,
  html = FALSE,
  na = NA,
  special_formatting = FALSE,
  stars = FALSE,
  bold_split = FALSE,
  pad = if (isTRUE(html)) fig_space else " ",
  syntax = c("text", "excel"),
  .ref = NULL
)
```

## Arguments

- x:

  A fmt object.

- ...:

  Other parameters.

- html:

  Should html tags be added (to print confidence intervals as
  subscripts) ?

- na:

  How `NA`s should be printed. Default to `NA`.

- special_formatting:

  Set to `TRUE` to print more verbose results, like indicating which is
  the reference row or col for differences.

- stars:

  Append significance stars after the value (opt-in; default `FALSE`).
  Stars appear only where a per-cell p-value was stored (diff-type CIs /
  regression coefficients) and are right-padded so numbers stay aligned.
  The main display (console,
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md),
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_md.md))
  sets this `TRUE`; tooltip / secondary-field re-renders leave it
  `FALSE`, so stars never leak.

- bold_split:

  Internal (default `FALSE`): when `TRUE`, attach a per-cell
  `primary_nchar` attribute giving the bold-prefix width of a composite
  `"{pct} (n={n})"` cell, so exporters can bold only the primary field
  in a bold row. Off by default -\> the output is attribute-free.

- pad:

  The character used to align numbers: it pads values (composite
  displays, significance stars, confidence intervals, a mean with no sd)
  **and separates thousands**. Defaults to a plain space, or to a
  **figure space** (`U+2007`, exactly one digit wide) when
  `html = TRUE`. Media read in a monospace font (the console, markdown)
  want the plain space; media rendered in a proportional font (html,
  Excel) need the figure space, since an ASCII space is only half a
  digit wide there – and CSS collapses runs of them. One glyph for both
  jobs, so the thousands mark can never disagree with the padding around
  it.

- syntax:

  `"text"` (default) returns the rendered display strings; `"excel"`
  returns the per-cell Excel number-format codes used by
  [`tab_xl()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_xl.md)
  (the raw value is written unchanged).

- .ref:

  Internal: precomputed reference masks `list(cells=, all_totals=)`
  (derive-once speed-up passed by the exporter prep); computed
  internally when `NULL`.

## Value

The fmt printed in a character vector.
