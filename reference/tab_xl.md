# Write a table to an Excel workbook

The Excel exporter behind
[`tab_export`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md):
`tab_export(x, format = "xl")` calls this. Colours follow the same
palettes as the console and the HTML output, so a table looks the same
wherever it is read; change them with
[`set_color_style`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
and
[`set_color_breaks`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md).

## Usage

``` r
tab_xl(
  tabs,
  path = NULL,
  replace = FALSE,
  open = rlang::is_interactive(),
  lang = NULL,
  colnames_rotation = 0,
  remove_tab_vars = TRUE,
  colwidth = "auto",
  color_legend = TRUE,
  sheets = "auto",
  titles,
  caption = NULL,
  font_text = NULL,
  font_num = NULL,
  font_num_stars = NULL,
  text_size = 10,
  text_size_headers = 9,
  text_size_subtext = 9,
  theme = NULL,
  color = TRUE,
  transpose = FALSE,
  var_names = NULL,
  wrap_rows = 35,
  wrap_cols = 15,
  ratio_cells = NULL,
  check = FALSE,
  data = NULL,
  print_color_legend = lifecycle::deprecated(),
  ...
)
```

## Arguments

- tabs:

  A table made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) or
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
  or a `list` of tab. A list of tables sharing the same `col_vars` (and
  no `tab_vars`) is merged into one; any other list — several `row_vars`
  and/or `tab_vars` — is rendered one table after another, each keeping
  its own sub-tables.

- path, replace, open:

  The name, and possibly the path, of the Excel file to create (the
  `.xlsx` extension is optional). Defaults to a temporary directory; set
  the global option `"tabxplor.export_dir"` with
  [`options`](https://rdrr.io/r/base/options.html) to change it.
  `replace` defaults to `TRUE` when `path` is given and `FALSE`
  otherwise; set it to `TRUE` to overwrite an existing file. Use
  `open = FALSE` not to open the workbook straight away in Excel (or
  whatever opens `.xlsx` files).

- lang:

  Colour-legend language: `NULL` (auto from the R/OS locale, English
  fallback), `"en"` or `"fr"`.

- colnames_rotation:

  Rotate the names of columns to an angle (in degrees).

- remove_tab_vars:

  By default, `tab_vars` columns are removed to gain space — the
  sub-table's Total row names it. Ignored where several `row_vars` are
  stacked: the level column alone is then not a complete row index, so
  the column stays. Set to `FALSE` to keep them.

- colwidth:

  Column widths. `"auto"` (the default) fits every column to what its
  cells actually show, so a number column is exactly as wide as its
  widest figure and a text column wraps instead of growing past a cap.
  Give a number instead to force that fixed width on every numeric
  column (a mean's `sd` sibling then takes a proportionally narrower
  one). Widths are set per *sheet*, so several tables written to one
  sheet all fit.

- color_legend:

  Print the colour legend below the table (with the subtext). `TRUE` by
  default, and a no-op on a table that carries no colours.

- sheets:

  The Excel sheets options :

  - `"tabs"`: a new sheet is created for each table

  - `"unique"`: all tables are on the same sheet

  - `"auto"`: subsequent tables with the same column vars are printed on
    the same sheets

- titles:

  The titles of the different tables, as a character vector. When
  missing titles are given based on the names of the variables.

- caption:

  A single caption; a shortcut that fills `titles` (an explicit `titles`
  still wins). Unified name across all exporters.

- font_text, font_num, font_num_stars:

  Fonts for text (labels, headers) and for numbers. The number font is
  chosen **per table**: `font_num` (default `"DejaVu Sans"`) when the
  table shows no significance stars, and `font_num_stars` (default
  `"Cascadia Mono"`, a **monospace** font) when it does — monospace
  aligns the stars and `(n=...)` composites, which a proportional font
  cannot. Defaults from `options(tabxplor.xl_font_text)` /
  `options(tabxplor.xl_font_num)` /
  `options(tabxplor.xl_font_num_stars)`. Note that xlsx, unlike CSS, has
  **no font-fallback list**: only one name is recorded, so if it is
  missing on the machine opening the workbook Excel substitutes by its
  own rules. Set the options to a font you know is installed.

- text_size, text_size_headers, text_size_subtext:

  Font sizes of text elements.

- theme:

  By default (`"light"`) a white table with black text; set to `"dark"`
  for a black table with white text (the colours follow the theme). The
  black-and-white **publication** palettes render a table for a page
  that has no colour: `"print_ready"` picks the right one per table, or
  name it yourself – `"print_marks"`, `"print_emphasis"`,
  `"print_minimalistic"` (`"bw"`). See
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  for what each of them says.

- color:

  Set to `FALSE` to render the table without colours (monochrome).

- transpose:

  Set to `TRUE` to transpose each table before export (rows become
  columns) – the col-percentages-with-several-row-variables use case.

- var_names:

  Which variable names to write beside the table: `"both"` (the
  default), `"rows"`, `"cols"` or `"none"`. The row-variable name is the
  leading column a table with several `row_vars` uses to name each block
  (written once per block); the column-variable names are the spanning
  row above their level columns. Level headers always keep their name.

- wrap_rows:

  By default, rownames are wrapped when larger than 30 characters.

- wrap_cols:

  By default, colnames are wrapped when larger than 12 characters.

- ratio_cells:

  What a ratio / odds-ratio cell holds in the workbook. Excel cannot
  compute inside a number format, so a cell storing `0.83` cannot be
  made to print `÷1.2` the way the console does. `"fold"` (the default)
  stores the **reading value** instead — the fold, signed by its
  direction (`x` at or above the neutral, `-1/x` below it) — which
  prints as `×1.20` and `÷1.20`, `2.11` and `1/2.11`. The cell stays a
  real number: it sorts and filters in the direction it is read, and
  takes the reader's own decimal separator. `"raw"` stores the
  untransformed ratio (printed `×0.83`); `"text"` writes the exact
  display string, which reads perfectly but is no longer a number.
  Option twin: `tabxplor.xl_ratio_cells`.

- check:

  Model-check plots to draw under each
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  table: `FALSE` (the default), `"auto"`, or a vector of check keys —
  the same values
  [`reg_check_plots`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
  takes, which is what draws them. Each grid is written as a picture
  below the table it belongs to. Needs `ggplot2` and `gridExtra`; a
  crosstab takes none.

- data:

  The data frame the models were fitted on. Only needed when `check` is
  on AND the
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  call cannot be replayed from the name it was written with (a `%>%`
  pipeline, a subset expression) — an ordinary `tab_reg(gss, ...)`
  recovers it by itself.

- print_color_legend:

  **\[deprecated\]** Renamed to `color_legend`.

- ...:

  Retired arguments, accepted and ignored with a deprecation message
  since 2.0.0 (`color_type`, `html_24_bit`, `n_min`, `hide_near_zero`):
  colour is a channel of `color =`, Excel is always 24-bit, and the
  other two are
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  business — the small-base filter and the display template. Anything
  else is an error naming the argument you meant, as it already was in
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

## Value

The table(s) with formatting and colors in an Excel file, as a side
effect. Invisibly returns `tabs`.

## Recovering the raw ratio in Excel

A ratio or odds-ratio cell holds its **reading value**: the fold, signed
by its direction. The sign IS the marker — negative means the cell reads
`÷` (or `1/`) — so one formula gives the raw ratio back, with no macro
and no add-in:

      =IF(A2<0, -1/A2, A2)     the ratio itself
      =ABS(A2)                how many times, whichever way it goes

Sorting and filtering need neither: the stored value is monotone in the
direction it is read, so "at least twice as likely" is `>2` and "at
least twice as unlikely" is `<-2`. Use `ratio_cells = "raw"` when the
untransformed ratio matters more than the reading.

## Examples

``` r
# \donttest{
# openxlsx2 is Suggests-only and tab_xl() stops without it, so guard the example: \donttest{}
# does NOT exempt it from R CMD check --as-cran, which CRAN also runs without Suggests.
if (requireNamespace("openxlsx2", quietly = TRUE)) {
  forcats::gss_cat |>
    tab(marital, race, pct = "row", color = "difference") |>
    tab_xl()
}
#> ✔ Excel file written to /tmp/RtmpOYx35c/Tab.xlsx
# }
```
