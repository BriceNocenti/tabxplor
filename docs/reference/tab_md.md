# Export a tabxplor table to a markdown table

The Markdown exporter behind
[`tab_export`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md):
`tab_export(x, format = "md")` calls this.

## Usage

``` r
tab_md(
  tabs,
  bold_references = TRUE,
  special_formatting = TRUE,
  wrap_rows = NULL,
  subtext = TRUE,
  color = TRUE,
  color_legend = TRUE,
  lang = NULL,
  theme = NULL,
  color_type = lifecycle::deprecated(),
  html_24_bit = NULL,
  caption = NULL,
  transpose = FALSE,
  var_names = NULL,
  css = TRUE,
  clipboard = FALSE,
  file = NULL,
  print = TRUE,
  title = lifecycle::deprecated(),
  col_var_names = lifecycle::deprecated()
)
```

## Arguments

- tabs:

  A table made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) or
  [`tab_many`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md),
  or a `list` of tab. A list of tables sharing the same `col_vars` (and
  no `tab_vars`) is merged into one; any other list — several `row_vars`
  and/or `tab_vars` (e.g.
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  with several row variables and a `tab_vars`) — is rendered one table
  after another, each keeping its own sub-tables.

- bold_references:

  Bold reference/total rows with markdown `**...**`.

- special_formatting:

  Passed to
  [`format()`](https://bricenocenti.github.io/tabxplor/reference/format.tabxplor_fmt.md).
  When `TRUE`, shows "ref:" prefix on diff reference cells, "mean:" on
  ctr totals, sigma on means.

- wrap_rows:

  Max width for row labels before truncation. `NULL` (default) never
  truncates (lossless – the column grows); set a number to cap the label
  width. A markdown pipe cell cannot hold a raw newline, so md
  "wrapping" means "do not truncate".

- subtext:

  Print chi2/footnotes below the table.

- color:

  When `TRUE` (default) and the table carries colours (e.g. built with
  `tab(..., color = "diff")`), each fmt cell is wrapped in a short
  pandoc bracketed span `[value]{.class}` so the markdown renders
  coloured in Quarto / RMarkdown / pandoc (and the companion
  [`tab_md_css`](https://bricenocenti.github.io/tabxplor/reference/tab_md_css.md)
  styles the classes). `FALSE` produces plain monochrome markdown.
  Uncoloured tables never get spans.

- color_legend:

  When `TRUE` (default) and the table is coloured, prepend a
  colour-legend prose line (its break-words in the same pandoc classes
  as the cells) above the subtext.

- lang:

  Colour-legend language: `NULL` (auto from the R/OS locale, English
  fallback), `"en"` or `"fr"`.

- theme, html_24_bit:

  Colour palette selectors (as in
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md));
  they only affect the CSS emitted by `css = TRUE` /
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md),
  since the span *class names* are palette- and theme-independent.
  `theme` accepts `"auto"` (follow the reader's colour scheme).

- color_type:

  **\[deprecated\]** Inert since 2.0.0: the text channel always uses the
  text palette. The colour CHANNEL is chosen by
  `color = c(text, background)` (see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)).

- caption:

  Optional table caption, rendered as a pandoc caption line `: caption`
  (captions only the first table of a list).

- transpose:

  Set to `TRUE` to transpose each table before export (rows become
  columns) – the col-percentages-with-several-row-variables use case.

- var_names:

  Which variable names to write beside the table: `"both"` (the
  default), `"rows"`, `"cols"` or `"none"`. The column variables' names
  are written as an italic body row above their level columns; the
  row-variable name is the leading column a table with several
  `row_vars` uses to name each block (written once per block, in
  italics). See
  [`tab_kable`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md).

- css:

  When `TRUE` (the **default**), prepend an inline `<style>` block (from
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)),
  so the exported markdown is self-contained – it renders coloured and
  compact on its own. Set `FALSE` inside an `.Rmd`/`.qmd` document (the
  host page brings the stylesheet, or emit
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  once at the top – it styles every table), otherwise the inline
  `<style>` block is duplicated per table. Any **styled** table
  (coloured, or `css = TRUE`) is wrapped in a pandoc fenced div
  `::: {.tabxplor-tab}`, which pandoc renders as
  `<div class="tabxplor-tab">` – the hook
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)'s
  table styling needs, since pandoc emits a bare `<table>` it could not
  otherwise reach. So the rendered HTML of a markdown table can look
  like
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)'s
  (compact layout, thin rules under the variable-name row and between
  sub-tables, no host borders), not just be coloured – even with
  `css = FALSE`, as long as the stylesheet is brought in some other way.
  A plain uncoloured table is left byte-identical (no div).

- clipboard:

  Copy output to clipboard via
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md).
  Requires the clipr package.

- file:

  Path to write the markdown to a file. `NULL` (default) skips.

- print:

  If `TRUE`, print via [`cat()`](https://rdrr.io/r/base/cat.html) and
  return invisibly. If `FALSE`, return the character string.

- title:

  **\[deprecated\]** Renamed to `caption`.

- col_var_names:

  **\[deprecated\]** Replaced by `var_names`: `col_var_names = FALSE` is
  `var_names = "rows"` (or `"none"`).

## Value

A character string (visible or invisible depending on `print`).

## Examples

``` r
# \donttest{
tab(forcats::gss_cat, race, marital, pct = "row") |> tab_md()
#> <style>
#> .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
#> .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
#> .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
#> .tabxplor-tab tfoot{font-size:80%;text-align:left;}
#> .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
#> .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
#> .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
#> .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
#> .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
#> .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab p{font-size:80%;}
#> .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab .tx-r{text-align:right;}
#> .tabxplor-tab .tx-l{text-align:left;}
#> .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
#> .tabxplor-tab .tx-num{white-space:nowrap;}
#> .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
#> .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
#> .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
#> .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
#> .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
#> .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
#> .tabxplor-tab .tx-foot{width:0;min-width:100%;}
#> .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
#> .tooltip-inner{max-width:none;white-space:nowrap;}
#> .popover{max-width:none;}
#> .popover-body,.popover-content{padding:6px;white-space:nowrap;}
#> .tabxplor-tab{color:#000000;background:#ffffff;}
#> .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
#> .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
#> .g1,.tabxplor-tab .g1{color:#9f9f9f;}
#> .g2,.tabxplor-tab .g2{color:#111111;}
#> .tabxplor-caption{color:#000000;}
#> .p1,.tabxplor-tab .p1{color:#02A5B3;}
#> .p2,.tabxplor-tab .p2{color:#0891C9;}
#> .p3,.tabxplor-tab .p3{color:#0267C7;}
#> .p4,.tabxplor-tab .p4{color:#300DFD;}
#> .m1,.tabxplor-tab .m1{color:#DCA331;}
#> .m2,.tabxplor-tab .m2{color:#DE7C01;}
#> .m3,.tabxplor-tab .m3{color:#DD5301;}
#> .m4,.tabxplor-tab .m4{color:#D60103;}
#> .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
#> .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
#> .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
#> .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
#> .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
#> .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
#> .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
#> .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
#> </style>
#> 
#> ::: {.tabxplor-tab}
#> | race    | |No answer  |Never married  |Separated  |Divorced  |Widowed  |Married  | |            Total  |
#> |:--------|-|----------:|--------------:|----------:|---------:|--------:|--------:|-|------------------:|
#> |         | | *marital* |               |           |          |         |         | |                   |
#> |         | |           |               |           |          |         |         | |                   |
#> | Other   | |       0%  |          32%  |       6%  |     11%  |     4%  |    48%  | |  100% (n= 1 959)  |
#> | Black   | |       0%  |          42%  |       6%  |     16%  |     8%  |    28%  | |  100% (n= 3 129)  |
#> | White   | |       0%  |          21%  |       3%  |     16%  |     9%  |    51%  | |  100% (n=16 395)  |
#> |**Total**| |     **0%**|        **25%**|     **3%**|   **16%**|   **8%**|  **47%**| |**100%** (n=21 483)|
#> ::: 
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |> tab_md()
#> <style>
#> .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
#> .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
#> .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
#> .tabxplor-tab tfoot{font-size:80%;text-align:left;}
#> .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
#> .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
#> .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
#> .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
#> .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
#> .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab p{font-size:80%;}
#> .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab .tx-r{text-align:right;}
#> .tabxplor-tab .tx-l{text-align:left;}
#> .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
#> .tabxplor-tab .tx-num{white-space:nowrap;}
#> .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
#> .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
#> .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
#> .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
#> .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
#> .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
#> .tabxplor-tab .tx-foot{width:0;min-width:100%;}
#> .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
#> .tooltip-inner{max-width:none;white-space:nowrap;}
#> .popover{max-width:none;}
#> .popover-body,.popover-content{padding:6px;white-space:nowrap;}
#> .tabxplor-tab{color:#000000;background:#ffffff;}
#> .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
#> .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
#> .g1,.tabxplor-tab .g1{color:#9f9f9f;}
#> .g2,.tabxplor-tab .g2{color:#111111;}
#> .tabxplor-caption{color:#000000;}
#> .p1,.tabxplor-tab .p1{color:#02A5B3;}
#> .p2,.tabxplor-tab .p2{color:#0891C9;}
#> .p3,.tabxplor-tab .p3{color:#0267C7;}
#> .p4,.tabxplor-tab .p4{color:#300DFD;}
#> .m1,.tabxplor-tab .m1{color:#DCA331;}
#> .m2,.tabxplor-tab .m2{color:#DE7C01;}
#> .m3,.tabxplor-tab .m3{color:#DD5301;}
#> .m4,.tabxplor-tab .m4{color:#D60103;}
#> .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
#> .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
#> .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
#> .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
#> .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
#> .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
#> .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
#> .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
#> </style>
#> 
#> ::: {.tabxplor-tab}
#> | race    | |No answer  |Never married  |Separated  |Divorced  |Widowed  |     Married  | |                Total  |
#> |:--------|-|----------:|--------------:|----------:|---------:|--------:|-------------:|-|----------------------:|
#> |         | | *marital* |               |           |          |         |              | |                       |
#> |         | |           |               |           |          |         |              | |                       |
#> | Other   | |    0%     |   [32%]{.p1}  |    6%     |    11%   |    4%   |    48%       | |      100% (n= 1 959)  |
#> | Black   | |    0%     |   [42%]{.p3}  |    6%     |    16%   |    8%   |   [28%]{.m3} | |      100% (n= 3 129)  |
#> | White   | |    0%     |    21%        |    3%     |    16%   |    9%   |    51%       | |      100% (n=16 395)  |
#> |**Total**| |  **0%**   |  **25%**      |  **3%**   |  **16%** |  **8%** |  **47%**     | |  **100%** (n=21 483)  |
#> 
#> Shades of blue: cells ≥ the Total row **[+5]{.p1}**; **[+15]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: cells ≤ the Total row **[-5]{.m1}**; **[-15]{.m3}**; **[-30]{.m4}** points.
#> ::: 
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
  dplyr::mutate(dplyr::across(dplyr::where(is_fmt), ~set_display(., "diff"))) |>
  tab_md()
#> <style>
#> .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
#> .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
#> .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
#> .tabxplor-tab tfoot{font-size:80%;text-align:left;}
#> .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
#> .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
#> .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
#> .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
#> .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
#> .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab p{font-size:80%;}
#> .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab .tx-r{text-align:right;}
#> .tabxplor-tab .tx-l{text-align:left;}
#> .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
#> .tabxplor-tab .tx-num{white-space:nowrap;}
#> .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
#> .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
#> .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
#> .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
#> .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
#> .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
#> .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
#> .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
#> .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
#> .tabxplor-tab .tx-foot{width:0;min-width:100%;}
#> .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
#> .tooltip-inner{max-width:none;white-space:nowrap;}
#> .popover{max-width:none;}
#> .popover-body,.popover-content{padding:6px;white-space:nowrap;}
#> .tabxplor-tab{color:#000000;background:#ffffff;}
#> .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
#> .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
#> .g1,.tabxplor-tab .g1{color:#9f9f9f;}
#> .g2,.tabxplor-tab .g2{color:#111111;}
#> .tabxplor-caption{color:#000000;}
#> .p1,.tabxplor-tab .p1{color:#02A5B3;}
#> .p2,.tabxplor-tab .p2{color:#0891C9;}
#> .p3,.tabxplor-tab .p3{color:#0267C7;}
#> .p4,.tabxplor-tab .p4{color:#300DFD;}
#> .m1,.tabxplor-tab .m1{color:#DCA331;}
#> .m2,.tabxplor-tab .m2{color:#DE7C01;}
#> .m3,.tabxplor-tab .m3{color:#DD5301;}
#> .m4,.tabxplor-tab .m4{color:#D60103;}
#> .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
#> .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
#> .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
#> .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
#> .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
#> .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
#> .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
#> .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
#> </style>
#> 
#> ::: {.tabxplor-tab}
#> | race    | | No answer  |   Never married  | Separated  |   Divorced  |   Widowed  |         Married  | |                Total  |
#> |:--------|-|-----------:|-----------------:|-----------:|------------:|-----------:|-----------------:|-|----------------------:|
#> |         | | *marital*  |                  |            |             |            |                  | |                       |
#> |         | |            |                  |            |             |            |                  | |                       |
#> | Other   | |       +0%  |       [+7%]{.p1} |       +2%  |        -5%  |       -5%  |        +0%       | |      100% (n= 1 959)  |
#> | Black   | |       -0%  |      [+16%]{.p3} |       +3%  |        +0%  |       -0%  |      [-19%]{.m3} | |      100% (n= 3 129)  |
#> | White   | |       +0%  |        -4%       |       -1%  |        +1%  |       +1%  |        +4%       | |      100% (n=16 395)  |
#> |**Total**| |  **ref:0%**|  **ref:25%**     |  **ref:3%**|  **ref:16%**|  **ref:8%**|  **ref:47%**     | |  **100%** (n=21 483)  |
#> 
#> Shades of blue: cells ≥ the Total row **[+5]{.p1}**; **[+15]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: cells ≤ the Total row **[-5]{.m1}**; **[-15]{.m3}**; **[-30]{.m4}** points.
#> ::: 
# }
```
