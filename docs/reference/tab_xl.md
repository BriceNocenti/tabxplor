# Excel output for tabxplor tables, with formatting and colors

The Excel exporter behind
[`tab_export`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md):
`tab_export(x, format = "xl")` calls this. To modify the colors used
into the Excel table, you can change the global options with
[`set_color_style`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
and
[`set_color_breaks`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md).

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
  colwidth = 10,
  color_legend = TRUE,
  sheets = "auto",
  titles,
  caption = NULL,
  font_text = getOption("tabxplor.xl_font_text", "DejaVu Sans Condensed"),
  font_num = getOption("tabxplor.xl_font_num", "DejaVu Sans"),
  font_num_stars = getOption("tabxplor.xl_font_num_stars", "Cascadia Mono"),
  text_size = 10,
  text_size_headers = 9,
  text_size_subtext = 9,
  theme = NULL,
  color_type = lifecycle::deprecated(),
  html_24_bit = NULL,
  color = TRUE,
  transpose = FALSE,
  var_names = NULL,
  or_numeric = getOption("tabxplor.xl_or_numeric", FALSE),
  print_color_legend = lifecycle::deprecated()
)
```

## Arguments

- tabs:

  A table made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  [`tab_many`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  or
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md),
  or a list of such tables.

- path, replace, open:

  The name, and possibly the path, of the Excel file to create (possibly
  without the .xlsx extension). Default path to temporary directory. Set
  global option `"tabxplor.export_dir"` with
  `link[base:options]{options}` to change default directory. By default
  replace is `TRUE` when `path` is provided, `FALSE` when `path` is not
  provided. Use `replace = TRUE` to overwrite existing files. Use
  `open = FALSE` if you don't want to automatically open the tables in
  Excel (or another software associated with .xlsx files).

- lang:

  Colour-legend language: `NULL` (auto from the R/OS locale, English
  fallback), `"en"` or `"fr"`.

- colnames_rotation:

  Rotate the names of columns to an angle (in degrees).

- remove_tab_vars:

  By default, `tab_vars` columns are removed to gain space. Set to
  `FALSE` to keep them.

- colwidth:

  The standard width for numeric columns, as a number. Set to `"auto"`
  to let Excel choose.

- color_legend:

  Should the color legends be printed with the subtexts ?

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
  `"Cascadia Mono"`, a **monospace** font) when it does – monospace
  aligns the stars and `(n=...)` composites, which a proportional font
  cannot. Defaults from `options(tabxplor.xl_font_text)` /
  `options(tabxplor.xl_font_num)` /
  `options(tabxplor.xl_font_num_stars)`. Note that xlsx, unlike CSS, has
  **no font-fallback list**: only one name can be recorded per font, so
  if it is missing on the machine opening the workbook, Excel
  substitutes by its own rules and no fallback can be named here. Set
  the options to a font you know is installed.

- text_size, text_size_headers, text_size_subtext:

  Font sizes of text elements.

- theme:

  By default (`"light"`) a white table with black text; set to `"dark"`
  for a black table with white text (the colours follow the theme).

- color_type:

  **\[deprecated\]** Inert since 2.0.0: the text channel always uses the
  text palette. The colour CHANNEL is chosen by
  `color = c(text, background)` (see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)).

- html_24_bit:

  Kept for a uniform exporter signature; inert for Excel (always
  24-bit).

- color:

  Set to `FALSE` to export without colours (monochrome).

- transpose:

  Set to `TRUE` to transpose each table before export (rows become
  columns). Useful for column percentages tables with several row
  variables.

- var_names:

  Which variable names to write beside the table: `"both"` (the
  default), `"rows"`, `"cols"` or `"none"`. The row-variable name is the
  leading column a table with several `row_vars` uses to name each block
  (merged over it and rotated 90 degrees); the column-variable names are
  the merged row above their level columns. See
  [`tab_kable`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md).

- or_numeric:

  Odds ratios export as text ("1/x" reciprocal for OR \< 1) by default
  so an OR below 1 reads symmetrically to an OR above 1. Set to `TRUE`
  (or the option `tabxplor.xl_or_numeric`) to keep them as real,
  editable numbers instead.

- print_color_legend:

  **\[deprecated\]** Renamed to `color_legend`.

## Value

The table(s) with formatting and colors in an Excel file, as a side
effect. Invisibly returns `tabs`.

## Examples

``` r
# \donttest{
# openxlsx2 is Suggests-only and tab_xl() stops without it, so guard the example: \donttest{}
# does NOT exempt it from R CMD check --as-cran, which CRAN also runs without Suggests.
if (requireNamespace("openxlsx2", quietly = TRUE)) {
  forcats::gss_cat |>
    tab(marital, race, pct = "row", color = "diff") |>
    tab_xl()
}
#> ✔ Excel file written to /tmp/RtmpueHYmF/Tab.xlsx
# }
```
