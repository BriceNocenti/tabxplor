# Print a tabxplor table in html

The HTML exporter behind
[`tab_export`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md):
`tab_export(x, format = "html")` calls this, and `tab_kable()` is a
permanent alias of `tab_html()`. Use it directly for HTML-specific
arguments.

## Usage

``` r
tab_html(
  tabs,
  theme = NULL,
  color_type = lifecycle::deprecated(),
  html_24_bit = NULL,
  color = TRUE,
  tooltips = TRUE,
  popover = NULL,
  color_legend = TRUE,
  lang = NULL,
  caption = knitr::opts_current$get("tab.cap"),
  transpose = FALSE,
  var_names = NULL,
  html_font = NULL,
  get_data = FALSE,
  full_width = FALSE,
  wrap_rows = 35,
  wrap_cols = 15,
  whitespace_only = TRUE,
  engine = NULL,
  css = NULL,
  ...
)

tab_kable(
  tabs,
  theme = NULL,
  color_type = lifecycle::deprecated(),
  html_24_bit = NULL,
  color = TRUE,
  tooltips = TRUE,
  popover = NULL,
  color_legend = TRUE,
  lang = NULL,
  caption = knitr::opts_current$get("tab.cap"),
  transpose = FALSE,
  var_names = NULL,
  html_font = NULL,
  get_data = FALSE,
  full_width = FALSE,
  wrap_rows = 35,
  wrap_cols = 15,
  whitespace_only = TRUE,
  engine = NULL,
  css = NULL,
  ...
)
```

## Arguments

- tabs:

  A table made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) or
  [`tab_many`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md),
  or a `list` of tab with the same `col_vars` and no `tab_vars`.

- theme:

  By default (`"light"`) a white table with black text; `"dark"` for a
  black table with white text; `"auto"` (opt-in) to follow whoever is
  **reading** the table:

  - in a file or a knitted document, the reader's browser decides –
    their operating system, plus any dark-mode toggle of the host page
    (Quarto, Bootstrap 5.3, Tailwind);

  - printed to the **Viewer**, your editor decides. Its webview reports
    the operating system rather than the editor's colour theme, so the
    theme is resolved in R instead (RStudio's, or Positron's,
    best-effort).

  `"auto"` needs `engine = "html"` (kableExtra's themes are baked at
  render time); asking it of the kableExtra engine renders light with a
  message. Defaults to `getOption("tabxplor.theme")`, i.e. `"light"` – a
  dark table is always a deliberate choice.

- color_type:

  **\[deprecated\]** Inert since 2.0.0: the text channel always uses the
  text palette. The colour CHANNEL is chosen by
  `color = c(text, background)` (see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)).

- html_24_bit:

  **\[deprecated\]** Inert since 2.0.0: exports are always 24-bit (the
  OKLCH palettes). Kept only so old calls do not error.

- color:

  Set to `FALSE` to render the table without colours (monochrome).

- tooltips:

  By default, html tooltips are used to display additional informations
  at mouse hover. Set to `FALSE` to discard.

- popover:

  By default, takes `getOption("tabxplor.kable_popover")`. When `FALSE`,
  html tooltips are of the base kind : they can't be used with floating
  table of content in rmarkdown documents. Set to `TRUE` to use
  kableExtra html popovers instead, which are compatible with floating
  toc. Remember to enable the `popover` module by copying the following
  code into your document :
  `<script> $(document).ready(function(){ $('[data-toggle="popover"]').popover(); }); </script> `

- color_legend:

  Print colors legend below the table ?

- lang:

  Colour-legend language: `NULL` (auto from the R/OS locale, English
  fallback), `"en"` or `"fr"`. You can then use a `css` chunk in
  rmarkdown to change popovers colors.

- caption:

  The table caption. For formatting, you need to use a `css` with
  `caption{}`in rmarkdown.

- transpose:

  Set to `TRUE` to transpose the table before export (rows become
  columns) – the col-percentages-with-several-row-variables use case.

- var_names:

  Which variable names to write beside the table: `"both"` (the
  default), `"rows"`, `"cols"` or `"none"`. The row-variable name is the
  leading column a table with several `row_vars` uses to name each block
  (written once per block, vertically); the column-variable names are
  the spanning row above their level columns. Level headers always keep
  their name. Defaults to `getOption("tabxplor.var_names", "both")`.

- html_font:

  A string for HTML css font. By default, it uses
  `'"DejaVu Sans", "Arial", arial, helvetica, sans-serif'`. Set another
  default by setting `options("tabxplor.kable_html_font" = )`.

- get_data:

  Get the transformed data instead of the html table.

- full_width:

  A TRUE or FALSE variable controlling whether the HTML table should
  have the preferable format for full_width. If not specified, a HTML
  table will have full width by default but this option will be set to
  FALSE for a LaTeX table.

- wrap_rows:

  By default, rownames are wrapped when larger than 30 characters.

- wrap_cols:

  By default, colnames are wrapped when larger than 12 characters.

- whitespace_only:

  Set to `FALSE` to wrap also on non whitespace characters.

- engine:

  The HTML render engine. `"html"` (default) is a dependency-free
  `<table>` renderer: faster, and every look is a CSS class you can
  restyle (see
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)),
  which is what makes `theme = "auto"` possible. `"kableExtra"` is the
  legacy engine (kableExtra); it bakes its own theme, so it cannot
  follow the reader's colour scheme. Defaults to
  `getOption("tabxplor.tab_kable_engine", "html")`.

- css:

  `engine = "html"` only: inline the stylesheet with the table, so the
  output is self-contained (default, from
  `getOption("tabxplor.tab_kable_css")`). Set `FALSE` in a many-table
  document that emits
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  once at the top – the stylesheet is table-independent, so one copy
  styles every table. With `FALSE` and no
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  call, tables render uncoloured.

- ...:

  Other arguments to pass to
  [`kableExtra::kable_styling`](https://rdrr.io/pkg/kableExtra/man/kable_styling.html).

## Value

A html table. Printing it opens it in the Viewer, on a page painted to
match the table – so a `theme = "dark"` table no longer sits in a white
pane. Differences from totals, confidence intervals, contribution to
variance, and unweighted counts, are available in an html tooltip at
cells hover.

## Details

`tab_kable()` is a permanent alias of `tab_html()` – the two are
identical. `tab_html()` names the output (an HTML table), while the HTML
backend *engine* (home-built or kableExtra) is chosen with `engine =`.

## Examples

``` r
# \donttest{
tabs <- tab(forcats::gss_cat, race, marital, year, pct = "row", color = "diff")
tab_html(tabs, theme = "light")
#> <style>.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
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
#> .u4{background-color:#FFBAAF;}</style>
#> <table class="tabxplor-tab"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="6">marital</th><th class="tx-span" colspan="1"></th></tr><tr><th class="tx-l tx-br tx-bl tx-rv">race</th><th class="tx-r tx-num">No answer</th><th class="tx-r tx-num">Never married</th><th class="tx-r tx-num">Separated</th><th class="tx-r tx-num">Divorced</th><th class="tx-r tx-num">Widowed</th><th class="tx-r tx-num">Married</th><th class="tx-r tx-num tx-br tx-bl tx-tot">Total</th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×16.1 ; n: 1">1%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +9% ; ratio: ×1.4 ; n: 60">34%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 8">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.4 ; n: 20">11%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷2.1 ; n: 8">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ×1 ; n: 78">45%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 175">100%<span style="font-weight:normal;"> (n=   175)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +11% ; ratio: ×1.4 ; n: 157">37%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +6% ; ratio: ×2.5 ; n: 43">10%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -2% ; ratio: ÷1.1 ; n: 60">14%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.2 ; n: 48">11%</td><td class="tx-r tx-num m2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -17% ; ratio: ÷1.6 ; n: 121">28%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 429">100%<span style="font-weight:normal;"> (n=   429)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.1 ; n: 495">22%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.4 ; n: 61">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1 ; n: 361">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 217">10%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 1 079">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 213">100%<span style="font-weight:normal;"> (n= 2 213)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2000</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 712">25%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 112">4%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 441">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 273">10%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 278">45%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 817">100%<span style="font-weight:normal;"> (n= 2 817)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 47">28%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +2% ; ratio: ×1.6 ; n: 9">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ×1 ; n: 26">16%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -8% ; ratio: ÷7.5 ; n: 2">1%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×1.1 ; n: 83">50%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 167">100%<span style="font-weight:normal;"> (n=   167)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +14% ; ratio: ×1.6 ; n: 163">40%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.8 ; n: 26">6%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 66">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 39">10%</td><td class="tx-r tx-num m2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -18% ; ratio: ÷1.6 ; n: 116">28%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 410">100%<span style="font-weight:normal;"> (n=   410)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.1 ; n: 498">23%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.2 ; n: 61">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 353">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.1 ; n: 206">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 1 070">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 188">100%<span style="font-weight:normal;"> (n= 2 188)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2002</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 0">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 708">26%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 96">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 445">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 247">9%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 269">46%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 765">100%<span style="font-weight:normal;"> (n= 2 765)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +7% ; ratio: ×1.3 ; n: 58">29%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.2 ; n: 8">4%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.7 ; n: 17">8%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷2.4 ; n: 6">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 112">56%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 201">100%<span style="font-weight:normal;"> (n=   201)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +18% ; ratio: ×1.8 ; n: 152">40%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×2.1 ; n: 27">7%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.1 ; n: 50">13%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.2 ; n: 23">6%</td><td class="tx-r tx-num m2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -19% ; ratio: ÷1.6 ; n: 125">33%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 377">100%<span style="font-weight:normal;"> (n=   377)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.2 ; n: 409">18%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.3 ; n: 60">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 348">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 175">8%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 1 242">56%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 234">100%<span style="font-weight:normal;"> (n= 2 234)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2004</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 0">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 619">22%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 95">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 415">15%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 204">7%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 479">53%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 812">100%<span style="font-weight:normal;"> (n= 2 812)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.3 ; n: 1">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×1.2 ; n: 165">28%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×2 ; n: 40">7%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷1.5 ; n: 65">11%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷2.5 ; n: 19">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 302">51%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 592">100%<span style="font-weight:normal;"> (n=   592)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +19% ; ratio: ×1.8 ; n: 270">43%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +2% ; ratio: ×1.6 ; n: 35">6%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×1 ; n: 102">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 54">9%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -21% ; ratio: ÷1.8 ; n: 173">27%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 634">100%<span style="font-weight:normal;"> (n=   634)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.1 ; n: 5">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.2 ; n: 645">20%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.4 ; n: 81">2%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 565">17%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 293">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 1 695">52%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 3 284">100%<span style="font-weight:normal;"> (n= 3 284)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2006</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 6">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 080">24%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 156">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 732">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 366">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 2 170">48%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 4 510">100%<span style="font-weight:normal;"> (n= 4 510)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +11% ; ratio: ×1.4 ; n: 68">37%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.7 ; n: 11">6%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.8 ; n: 14">8%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.5 ; n: 10">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.1 ; n: 80">44%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 183">100%<span style="font-weight:normal;"> (n=   183)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +15% ; ratio: ×1.6 ; n: 117">42%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×2.6 ; n: 25">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.1 ; n: 36">13%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -2% ; ratio: ÷1.3 ; n: 18">6%</td><td class="tx-r tx-num m2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -18% ; ratio: ÷1.6 ; n: 85">30%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 281">100%<span style="font-weight:normal;"> (n=   281)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.3 ; n: 5">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.2 ; n: 346">22%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.6 ; n: 34">2%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 231">15%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 136">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×1.1 ; n: 807">52%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 1 559">100%<span style="font-weight:normal;"> (n= 1 559)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2008</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 5">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 531">26%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 70">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 281">14%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 164">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 972">48%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 023">100%<span style="font-weight:normal;"> (n= 2 023)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +13% ; ratio: ×1.5 ; n: 74">40%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.9 ; n: 11">6%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.5 ; n: 20">11%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.8 ; n: 9">5%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.2 ; n: 69">38%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 183">100%<span style="font-weight:normal;"> (n=   183)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +23% ; ratio: ×1.8 ; n: 159">51%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.2 ; n: 8">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1 ; n: 54">17%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.2 ; n: 23">7%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -22% ; ratio: ÷2 ; n: 67">22%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 311">100%<span style="font-weight:normal;"> (n=   311)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.3 ; n: 1">0%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.3 ; n: 332">21%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ÷1.1 ; n: 46">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1 ; n: 267">17%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 149">10%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×1.1 ; n: 755">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 1 550">100%<span style="font-weight:normal;"> (n= 1 550)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2010</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 565">28%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 65">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 341">17%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 181">9%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 891">44%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 044">100%<span style="font-weight:normal;"> (n= 2 044)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +9% ; ratio: ×1.3 ; n: 70">36%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +2% ; ratio: ×1.6 ; n: 11">6%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -9% ; ratio: ÷2.4 ; n: 13">7%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷2.7 ; n: 6">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 96">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 196">100%<span style="font-weight:normal;"> (n=   196)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +13% ; ratio: ×1.5 ; n: 120">40%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.3 ; n: 13">4%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×1.2 ; n: 59">20%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×1 ; n: 24">8%</td><td class="tx-r tx-num m2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -17% ; ratio: ÷1.6 ; n: 85">28%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 301">100%<span style="font-weight:normal;"> (n=   301)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.2 ; n: 336">23%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ÷1.2 ; n: 44">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1 ; n: 245">17%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 133">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.1 ; n: 719">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 1 477">100%<span style="font-weight:normal;"> (n= 1 477)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2012</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 0">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 526">27%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 68">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 317">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 163">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 900">46%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 1 974">100%<span style="font-weight:normal;"> (n= 1 974)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +8% ; ratio: ×1.3 ; n: 91">35%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.4 ; n: 12">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -2% ; ratio: ÷1.1 ; n: 37">14%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷2.2 ; n: 10">4%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.1 ; n: 112">43%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 262">100%<span style="font-weight:normal;"> (n=   262)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×3.3 ; n: 2">1%</td><td class="tx-r tx-num p2 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +17% ; ratio: ×1.6 ; n: 167">43%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +2% ; ratio: ×1.5 ; n: 19">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 68">18%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 33">9%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -20% ; ratio: ÷1.8 ; n: 97">25%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 386">100%<span style="font-weight:normal;"> (n=   386)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ÷1.5 ; n: 2">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷1.2 ; n: 417">22%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.2 ; n: 50">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×1 ; n: 306">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 166">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×1.1 ; n: 949">50%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 1 890">100%<span style="font-weight:normal;"> (n= 1 890)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2014</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 4">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 675">27%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 81">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 411">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 209">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 158">46%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 2 538">100%<span style="font-weight:normal;"> (n= 2 538)</span></td></tr>
#> <tr class="tx-b tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total Ensemble</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 17">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 5 416">25%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 743">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 3 383">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 807">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 10 117">47%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 21 483">100%<span style="font-weight:normal;"> (n=21 483)</span></td></tr></tbody><tfoot><tr><td colspan="8"><div class="tx-foot">Shades of blue: cells ≥ the Total row <span class="p1" style="font-weight:bold;">+5</span>; <span class="p2" style="font-weight:bold;">+10</span>; <span class="p3" style="font-weight:bold;">+20</span>; <span class="p4" style="font-weight:bold;">+30</span> points. Shades of yellow to red: cells ≤ the Total row <span class="m1" style="font-weight:bold;">-5</span>; <span class="m2" style="font-weight:bold;">-10</span>; <span class="m3" style="font-weight:bold;">-20</span>; <span class="m4" style="font-weight:bold;">-30</span> points.</div></td></tr></tfoot></table>
# }
```
