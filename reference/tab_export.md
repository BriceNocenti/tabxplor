# Export a table to html, Excel or Markdown (wrapper)

One entry point over the format-specific exporters
[`tab_html`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
(HTML),
[`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
(Markdown),
[`tab_xl`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
(Excel) and
[`forest_plot`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
(a chart of the estimates). They share one set of display-option names
and defaults; `tab_export()` forwards them and passes any
format-specific argument through `...`.

## Usage

``` r
tab_export(
  x,
  format = c("html", "md", "xl", "forest"),
  path = NULL,
  theme = NULL,
  color = TRUE,
  color_legend = TRUE,
  lang = NULL,
  transpose = FALSE,
  caption = NULL,
  var_names = NULL,
  ...
)
```

## Arguments

- x:

  A table made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) or
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
  or a `list` of tab. A list of tables sharing the same `col_vars` (and
  no `tab_vars`) is merged into one; any other list — several `row_vars`
  and/or `tab_vars` — is rendered one table after another, each keeping
  its own sub-tables.

- format:

  One of `"html"` (the default), `"md"` (Markdown), `"xl"` (Excel) or
  `"forest"` (a forest plot of the estimates, see
  [`forest_plot`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)).

- path:

  Optional output file. For `"xl"` it is the workbook path; for `"md"`
  and `"html"` the rendered text is written to it; ignored for
  `"forest"`.

- theme:

  By default (`"light"`) a white table with black text; `"dark"` for the
  inverse (the colours follow the theme). `"auto"` follows the reader's
  colour scheme (their operating system, and any dark-mode toggle of the
  host page); it needs a stylesheet, so it works for `format = "html"`
  and `"md"` and resolves to `"light"` for the static `"xl"` backend.
  The black-and-white **publication** palettes render a table for a page
  that has no colour: `"print_ready"` picks the right one per table, or
  name it yourself – `"print_marks"`, `"print_emphasis"`,
  `"print_minimalistic"` (`"bw"`). Defaults to
  `getOption("tabxplor.theme")`. See
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  for what each says.

- color:

  Set to `FALSE` to render the table without colours (monochrome).

- color_legend:

  Print the colour legend below the table (with the subtext). `TRUE` by
  default, and a no-op on a table that carries no colours.

- lang:

  Colour-legend language: `NULL` (auto from the R/OS locale, English
  fallback), `"en"` or `"fr"`.

- transpose:

  Set to `TRUE` to transpose each table before export (rows become
  columns) – the col-percentages-with-several-row-variables use case.

- caption:

  A single caption / title for the table.

- var_names:

  Which variable names to write beside the table: `"both"` (the
  default), `"rows"`, `"cols"` or `"none"`. The row-variable name is the
  leading column a table with several `row_vars` uses to name each block
  (written once per block); the column-variable names are the spanning
  row above their level columns. Level headers always keep their name.

- ...:

  Format-specific arguments passed to the underlying exporter. Retired
  arguments (`color_type`, `html_24_bit`, `engine`, `html_font`,
  `full_width`, `position`, `n_min`, `hide_near_zero`) are caught here,
  reported once, and not forwarded; the exporter this hands to refuses
  anything else it cannot use.

## Value

The value of the underlying exporter: an HTML/knitr object (`"html"`), a
markdown string (`"md"`), `x` invisibly with the Excel file written
(`"xl"`), or a `ggplot` (`"forest"`).

## Details

Each exporter is also callable on its own, which reads better in a pipe
(`x |> tab_xl()`); use `tab_export()` when the format comes from a
variable.

## Examples

``` r
# \donttest{
tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "difference")
tab_export(tabs, "md")
#> <style>
#> .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
#> .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
#> .tabxplor-tab{margin-bottom:1.2em;}
#> .tabxplor-caption{display:block;text-align:left;font-weight:bold;font-size:110%;white-space:normal;width:0;min-width:100%;}
#> .tabxplor-tab>caption{caption-side:top;padding:0;margin:0;}
#> .tabxplor-tab tfoot{font-size:80%;text-align:left;}
#> .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
#> .tabxplor-tab th,.tabxplor-tab td{border-width:0;}
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
#> .tabxplor-tab thead .tx-unit{font-weight:normal;font-style:italic;font-size:80%;text-align:left;border-top-width:0;padding-top:0;}
#> .tabxplor-tab thead tr:has(+ tr > .tx-unit) > th:not([rowspan]){border-bottom-width:0;}
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
#> .tabxplor-tab tr.tx-bb>.tx-nb,.tabxplor-tab tr.tx-bb2>.tx-nb{border-bottom-style:none;}
#> .tabxplor-tab td.tx-bb2{border-bottom-style:solid;border-bottom-width:2px;}
#> .tabxplor-tab tr.tx-bt2>*{border-top-style:solid;border-top-width:2px;}
#> .tabxplor-tab .tx-foot{width:0;min-width:100%;padding-bottom:5px;}
#> .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;margin:0 -4px;}
#> .tabxplor-tab .tx-spark{display:block;margin:0 auto;}
#> .tabxplor-tab .tx-sparkcell{vertical-align:middle;text-align:center;padding:1px 2px;}
#> .tabxplor-tab.tx-shape{font-size:90%;}
#> .tooltip-inner{max-width:none;white-space:pre;}
#> .popover{max-width:none;}
#> .popover-body,.popover-content{padding:6px;white-space:pre;}
#> .tabxplor-tab{color:#000000;background:transparent;}
#> .tabxplor-tab th,.tabxplor-tab td{background-color:transparent;border-color:#000000;}
#> .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
#> .g1,.tabxplor-tab .g1{color:#949494;}
#> .g2,.tabxplor-tab .g2{color:#444444;}
#> .tabxplor-tab .tx-unit{color:#949494;}
#> .tabxplor-caption{color:#000000;}
#> .tabxplor-tab .tx-foot{color:#444444;}
#> .tabxplor-tab.tx-shape{color:#444444;}
#> .tabxplor-tab.tx-shape thead th{color:#444444;}
#> .tabxplor-tab.tx-shape .tx-sec{color:#949494;}
#> .tabxplor-tab .tx-sec{color:#444444;}
#> .p1,.tabxplor-tab .p1{color:#02A5B3;}
#> .p2,.tabxplor-tab .p2{color:#0891C9;}
#> .p3,.tabxplor-tab .p3{color:#0267C7;}
#> .p4,.tabxplor-tab .p4{color:#300DFD;}
#> .m1,.tabxplor-tab .m1{color:#DCA331;}
#> .m2,.tabxplor-tab .m2{color:#DE7C01;}
#> .m3,.tabxplor-tab .m3{color:#DD5301;}
#> .m4,.tabxplor-tab .m4{color:#D60103;}
#> .o1,.tabxplor-tab .o1{background-color:#C4EAEE;}
#> .o2,.tabxplor-tab .o2{background-color:#B7DEF6;}
#> .o3,.tabxplor-tab .o3{background-color:#B2D0F8;}
#> .o4,.tabxplor-tab .o4{background-color:#AEC2FF;}
#> .u1,.tabxplor-tab .u1{background-color:#F0DFC4;}
#> .u2,.tabxplor-tab .u2{background-color:#F6CFB0;}
#> .u3,.tabxplor-tab .u3{background-color:#FCBDA5;}
#> .u4,.tabxplor-tab .u4{background-color:#FEAC9F;}
#> @media print {
#>   .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
#>   .tabxplor-tab{color:#000000;background:#ffffff;}
#>   .tabxplor-tab th,.tabxplor-tab td{color:#000000;background-color:#ffffff;border-color:#000000;}
#>   .tabxplor-tab tbody tr:hover{background:transparent;}
#>   .g1,.tabxplor-tab .g1{color:#949494;}
#>   .g2,.tabxplor-tab .g2{color:#444444;}
#>   .tabxplor-tab .tx-unit{color:#949494;}
#>   .tabxplor-caption{color:#000000;}
#>   .tabxplor-tab .tx-foot{color:#444444;}
#>   .tabxplor-tab.tx-shape{color:#444444;}
#>   .tabxplor-tab.tx-shape thead th{color:#444444;}
#>   .tabxplor-tab.tx-shape .tx-sec{color:#949494;}
#>   .tabxplor-tab .tx-sec{color:#444444;font-style:normal;text-decoration:none;display:inline-block;}
#>   .tabxplor-tab .tx-mark{color:#000000;font-style:normal;text-decoration:none;display:inline-block;}
#>   .p1,.tabxplor-tab .p1{color:#555555;font-weight:normal;text-decoration:underline;}
#>   .p2,.tabxplor-tab .p2{color:#000000;font-weight:normal;text-decoration:underline;}
#>   .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
#>   .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
#>   .m1,.tabxplor-tab .m1{color:#555555;font-weight:normal;font-style:italic;}
#>   .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
#>   .m3,.tabxplor-tab .m3{color:#000000;font-style:italic;}
#>   .m4,.tabxplor-tab .m4{color:#000000;font-style:italic;}
#>   .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
#>   .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
#>   .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
#>   .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
#>   .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
#>   .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
#>   .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
#>   .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
#> }
#> </style>
#> 
#> ::: {.tabxplor-tab}
#> | race    | |No answer  |Never married  |Separated  |Divorced  |Widowed  |     Married  | |             Total  |
#> |:--------|-|----------:|--------------:|----------:|---------:|--------:|-------------:|-|-------------------:|
#> |         | | *marital* |               |           |          |         |              | |                    |
#> |         | | *<row%>*  |               |           |          |         |              | |      *<row% (n)>*  |
#> |         | |           |               |           |          |         |              | |                    |
#> | Other   | |   0%      |  [32%]{.p1}   |   6%      |   11%    |   4%    |   48%        | |     100% ( 1 959)  |
#> | Black   | |   0%      |  [42%]{.p3}   |   6%      |   16%    |   8%    |  [28%]{.m3}  | |     100% ( 3 129)  |
#> | White   | |   0%      |   21%         |   3%      |   16%    |   9%    |   51%        | |     100% (16 395)  |
#> |**Total**| | **0%**    | **25%**       | **3%**    | **16%**  | **8%**  | **47%**      | | **100%** (21 483)  |
#> 
#> Percentage points (risk) difference: cell ≥ the Total row **[+5]{.p1}**; **[+15]{.p3}**; **[+30]{.p4}** points; cell ≤ the Total row **[-5]{.m1}**; **[-15]{.m3}**; **[-30]{.m4}** points.
#> ::: 
# }
```
