# Export a tabxplor table to Excel, HTML, Markdown, or a plot

A single entry point that dispatches to the format-specific exporters
[`tab_html`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)
(HTML),
[`tab_md`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_md.md)
(Markdown),
[`tab_xl`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_xl.md)
(Excel) and
[`tab_plot`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plot.md)
(a `ggplot`). The four functions share one set of display-option names
and defaults; `tab_export()` forwards them and passes any
format-specific argument through `...`.

## Usage

``` r
tab_export(
  x,
  format = c("html", "md", "xl", "plot"),
  path = NULL,
  theme = NULL,
  color_type = lifecycle::deprecated(),
  html_24_bit = NULL,
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

  A table (or list of tables) made with
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md) /
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

- format:

  One of `"html"` (the default), `"md"` (Markdown), `"xl"` (Excel) or
  `"plot"` (a `ggplot`). The HTML backend engine (home-built or
  kableExtra) is chosen with `engine =` (see
  [`tab_html`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)).

- path:

  Optional output file. For `"xl"` it is the workbook path; for `"md"`
  and `"html"` the rendered text is written to it; ignored for `"plot"`.

- theme:

  By default (`"light"`) a white table with black text; `"dark"` for the
  inverse (colours follow the theme). `"auto"` follows the reader's
  colour scheme (their OS, and any dark-mode toggle of the host page),
  which needs a stylesheet: it works for `format = "kable"` with
  `engine = "html"` and for `"md"`, and resolves to `"light"` for the
  static `"xl"` / `"plot"` backends and the kableExtra engine. Defaults
  to `getOption("tabxplor.theme")`. See
  [`tab_css`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_css.md).

- color_type:

  **\[deprecated\]** Inert since 1.4.0: the text channel always uses the
  text palette. The colour CHANNEL is chosen by
  `color = c(text, background)` (see
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)).

- html_24_bit:

  **\[deprecated\]** Inert since 1.4.0 (exports are always 24-bit).

- color:

  Set to `FALSE` to render without colours (monochrome).

- color_legend:

  Print the colour legend with the subtext
  (`"kable"`/`"md"`/`"xl"`/`"plot"`).

- lang:

  Legend language: `NULL` (auto from the R/OS locale, English fallback),
  `"en"` or `"fr"`.

- transpose:

  Set to `TRUE` to transpose each table before export (rows become
  columns) – the col-percentages-with-several-row-variables use case.

- caption:

  A single caption / title for the table.

- var_names:

  Which variable names to write beside the table: `"both"` (the
  default), `"rows"`, `"cols"` or `"none"`. Defaults to
  `getOption("tabxplor.var_names", "both")`. See
  [`tab_kable`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md).

- ...:

  Format-specific arguments passed to the underlying exporter.

## Value

The value of the underlying exporter: an HTML/knitr object (`"html"`), a
markdown string (`"md"`), `x` invisibly with the Excel file written
(`"xl"`), or a `ggplot` (`"plot"`).

## Examples

``` r
# \donttest{
tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
tab_export(tabs, "md")
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
#> .g1{color:#9f9f9f;}
#> .g2{color:#111111;}
#> .tabxplor-caption{color:#000000;}
#> .p1{color:#02A5B3;}
#> .p2{color:#0891C9;}
#> .p3{color:#0267C7;}
#> .p4{color:#300DFD;}
#> .m1{color:#DCA331;}
#> .m2{color:#DE7C01;}
#> .m3{color:#DD5301;}
#> .m4{color:#D60103;}
#> .o1{background-color:#DFFCFF;}
#> .o2{background-color:#D7EFFF;}
#> .o3{background-color:#CEE3FF;}
#> .o4{background-color:#BBCCFF;}
#> .u1{background-color:#FFF4E1;}
#> .u2{background-color:#FFE6D3;}
#> .u3{background-color:#FFD7C8;}
#> .u4{background-color:#FFBAAF;}
#> </style>
#> 
#> ::: {.tabxplor-tab}
#> | race    | |No answer  |Never married  |Separated  |Divorced  |Widowed  |     Married  | |                Total  |
#> |:--------|-|----------:|--------------:|----------:|---------:|--------:|-------------:|-|----------------------:|
#> |         | | *marital* |               |           |          |         |              | |                       |
#> |         | |           |               |           |          |         |              | |                       |
#> | Other   | |    0%     |   [32%]{.p1}  |    6%     |    11%   |    4%   |    48%       | |      100% (n= 1 959)  |
#> | Black   | |    0%     |   [42%]{.p2}  |    6%     |    16%   |    8%   |   [28%]{.m2} | |      100% (n= 3 129)  |
#> | White   | |    0%     |    21%        |    3%     |    16%   |    9%   |    51%       | |      100% (n=16 395)  |
#> |**Total**| |  **0%**   |  **25%**      |  **3%**   |  **16%** |  **8%** |  **47%**     | |  **100%** (n=21 483)  |
#> 
#> Shades of blue: cells ≥ the Total row **[+5]{.p1}**; **[+10]{.p2}**; **[+20]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: cells ≤ the Total row **[-5]{.m1}**; **[-10]{.m2}**; **[-20]{.m3}**; **[-30]{.m4}** points.
#> ::: 
# }
```
