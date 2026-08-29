# Render a table as html

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
  color = TRUE,
  tooltips = NULL,
  popover = NULL,
  color_legend = TRUE,
  lang = NULL,
  caption = NULL,
  transpose = FALSE,
  var_names = NULL,
  get_data = FALSE,
  wrap_rows = 35,
  wrap_cols = 15,
  whitespace_only = TRUE,
  css = NULL,
  ...
)

tab_kable(
  tabs,
  theme = NULL,
  color = TRUE,
  tooltips = NULL,
  popover = NULL,
  color_legend = TRUE,
  lang = NULL,
  caption = NULL,
  transpose = FALSE,
  var_names = NULL,
  get_data = FALSE,
  wrap_rows = 35,
  wrap_cols = 15,
  whitespace_only = TRUE,
  css = NULL,
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

  Defaults to `getOption("tabxplor.theme")`, i.e. `"light"` – a dark
  table is always a deliberate choice.

  `"print_ready"`, `"print_marks"`, `"print_emphasis"` and
  `"print_minimalistic"` (`"bw"`) are the black-and-white
  **publication** palettes: a greyscale print loses the colour palette's
  direction entirely (both ramps convert to the same greys), so each
  says it with something else — see
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  for what. Their typography is written as real `<b>`/`<i>`/`<u>` markup
  as well as CSS, so it survives a stylesheet-less destination (a paste
  into Word, GitHub's markdown). You rarely need to ask for one: any
  coloured table already **prints** in the first, see
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)'s
  `print_rules`.

- color:

  Set to `FALSE` to render the table without colours (monochrome).

- tooltips:

  By default, takes `getOption("tabxplor.tab_kable_tooltips")` (`TRUE`
  unless set): html tooltips display additional informations at mouse
  hover. Set to `FALSE` to discard (or set the option to `FALSE` once
  per document, e.g. in a vignette or report where every table
  auto-prints).

- popover:

  By default, takes `getOption("tabxplor.kable_popover")`. When `FALSE`,
  html tooltips are of the base kind: they can't be used with a floating
  table of contents in rmarkdown documents. Set to `TRUE` for click
  popovers instead, which are compatible with a floating toc. Both are
  bound automatically, in the Viewer and in a knitted document alike,
  provided rmarkdown and htmltools are installed.

- color_legend:

  Print the colour legend below the table (with the subtext). `TRUE` by
  default, and a no-op on a table that carries no colours.

- lang:

  Colour-legend language: `NULL` (auto from the R/OS locale, English
  fallback), `"en"` or `"fr"`.

- caption:

  The table caption. For formatting, you need to use a `css` with
  `caption{}`in rmarkdown.

- transpose:

  Set to `TRUE` to transpose each table before export (rows become
  columns) – the col-percentages-with-several-row-variables use case.

- var_names:

  Which variable names to write beside the table: `"both"` (the
  default), `"rows"`, `"cols"` or `"none"`. The row-variable name is the
  leading column a table with several `row_vars` uses to name each block
  (written once per block); the column-variable names are the spanning
  row above their level columns. Level headers always keep their name.

- get_data:

  Get the transformed data instead of the html table.

- wrap_rows:

  By default, rownames are wrapped when larger than 30 characters.

- wrap_cols:

  By default, colnames are wrapped when larger than 12 characters.

- whitespace_only:

  Set to `FALSE` to wrap also on non whitespace characters.

- css:

  Inline the stylesheet with the table, so the output is self-contained
  (default, from `getOption("tabxplor.tab_kable_css")`). Set `FALSE` in
  a many-table document that emits
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  once at the top – the stylesheet is table-independent, so one copy
  styles every table. With `FALSE` and no
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  call, tables render uncoloured.

- ...:

  Retired arguments, accepted and ignored with a deprecation message
  since 2.0.0: `color_type`, `html_24_bit`, `engine`, `html_font`,
  `full_width`, `position`. The table is rendered by one dependency-free
  `<table>` engine whose every look is a CSS class you can restyle –
  font, width, colour and placement are all
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)'s
  business now. Anything else is an error naming the argument you meant,
  as it already was in
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

## Value

A html table. Printing it opens it in the Viewer, on a page painted to
match the table – so a `theme = "dark"` table no longer sits in a white
pane. Everything the cell has no room for – the confidence interval, the
exact p-value, the other ways of reading the same comparison
(difference, ratio, odds ratio), the chi-squared contribution and the
base count – is one hover away, each line named after the field it
shows.

## Details

`tab_kable()` is a permanent alias of `tab_html()` – the two are
identical. `tab_html()` names the output (an HTML table); `tab_kable()`
is the name it had when kableExtra rendered it.

## Examples

``` r
# \donttest{
tabs <- tab(forcats::gss_cat, race, marital, year, pct = "row", color = "difference")
#> ℹ year: one row per value.
#> ℹ Choose otherwise with `shape = c(year = "quintiles")`.
#> This message is displayed once per session.
tab_html(tabs, theme = "light")
#> <style>.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
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
#> }</style>
#> <table class="tabxplor-tab" data-quarto-disable-processing="true"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="6">marital</th><th class="tx-span" colspan="1"></th></tr><tr><th class="tx-l tx-br tx-bl tx-rv" rowspan="2">race</th><th class="tx-r tx-num">No answer</th><th class="tx-r tx-num">Never married</th><th class="tx-r tx-num">Separated</th><th class="tx-r tx-num">Divorced</th><th class="tx-r tx-num">Widowed</th><th class="tx-r tx-num">Married</th><th class="tx-r tx-num tx-br tx-bl tx-tot">Total</th></tr><tr><th class="tx-r tx-num tx-unit">&lt;row%&gt;</th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-br tx-bl tx-tot tx-unit">&lt;row% (n)&gt;</th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×16.10 ; OR: 1.00 ; n: 1">1%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +9% ; ratio: ×1.36 ; OR: 1/11.87 ; n: 60">34%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.15 ; OR: 1/14.00 ; n: 8">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.37 ; OR: 1/22.05 ; n: 20">11%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷2.12 ; OR: 1/34.12 ; n: 8">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.02 ; OR: 1/16.38 ; n: 78">45%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   175)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +11% ; ratio: ×1.45 ; OR: Inf ; n: 157">37%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +6% ; ratio: ×2.52 ; OR: Inf ; n: 43">10%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -2% ; ratio: ÷1.12 ; OR: Inf ; n: 60">14%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.15 ; OR: Inf ; n: 48">11%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -17% ; ratio: ÷1.61 ; OR: Inf ; n: 121">28%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   429)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.13 ; OR: Inf ; n: 495">22%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.44 ; OR: Inf ; n: 61">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.04 ; OR: Inf ; n: 361">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.01 ; OR: Inf ; n: 217">10%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.07 ; OR: Inf ; n: 1 079">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 213)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2000</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 712">25%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 112">4%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 441">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 273">10%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 278">45%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 817)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.10 ; n: 47">28%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +2% ; ratio: ×1.55 ; n: 9">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.03 ; n: 26">16%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -8% ; ratio: ÷7.46 ; n: 2">1%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×1.08 ; n: 83">50%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   167)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +14% ; ratio: ×1.55 ; n: 163">40%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.83 ; n: 26">6%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.00 ; n: 66">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.06 ; n: 39">10%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -18% ; ratio: ÷1.62 ; n: 116">28%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   410)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.13 ; n: 498">23%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.25 ; n: 61">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.00 ; n: 353">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.05 ; n: 206">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.07 ; n: 1 070">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 188)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2002</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 0">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 708">26%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 96">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 445">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 247">9%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 269">46%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 765)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +7% ; ratio: ×1.31 ; n: 58">29%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.18 ; n: 8">4%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.74 ; n: 17">8%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷2.43 ; n: 6">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.06 ; n: 112">56%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   201)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +18% ; ratio: ×1.83 ; n: 152">40%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×2.12 ; n: 27">7%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.11 ; n: 50">13%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.19 ; n: 23">6%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -19% ; ratio: ÷1.59 ; n: 125">33%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   377)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.20 ; n: 409">18%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.26 ; n: 60">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.06 ; n: 348">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.08 ; n: 175">8%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.06 ; n: 1 242">56%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 234)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2004</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 0">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 619">22%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 95">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 415">15%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 204">7%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 479">53%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 812)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.27 ; OR: 1.00 ; n: 1">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×1.16 ; OR: 1/1.09 ; n: 165">28%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.95 ; OR: 1.54 ; n: 40">7%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷1.48 ; OR: 1/1.88 ; n: 65">11%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷2.53 ; OR: 1/3.21 ; n: 19">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.06 ; OR: 1/1.20 ; n: 302">51%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   592)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +19% ; ratio: ×1.78 ; OR: Inf ; n: 270">43%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +2% ; ratio: ×1.60 ; OR: Inf ; n: 35">6%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ÷1.01 ; OR: Inf ; n: 102">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.05 ; OR: Inf ; n: 54">9%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -21% ; ratio: ÷1.76 ; OR: Inf ; n: 173">27%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   634)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.14 ; OR: 1.00 ; n: 5">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.22 ; OR: 1/1.40 ; n: 645">20%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.40 ; OR: 1/1.60 ; n: 81">2%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.06 ; OR: 1/1.08 ; n: 565">17%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.10 ; OR: 1/1.04 ; n: 293">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.07 ; OR: 1/1.07 ; n: 1 695">52%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 3 284)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2006</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 6">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 080">24%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 156">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 732">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 366">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 2 170">48%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 4 510)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +11% ; ratio: ×1.42 ; OR: Inf ; n: 68">37%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.74 ; OR: Inf ; n: 11">6%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.82 ; OR: Inf ; n: 14">8%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.48 ; OR: Inf ; n: 10">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.10 ; OR: Inf ; n: 80">44%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   183)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +15% ; ratio: ×1.59 ; OR: Inf ; n: 117">42%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×2.57 ; OR: Inf ; n: 25">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.08 ; OR: Inf ; n: 36">13%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -2% ; ratio: ÷1.27 ; OR: Inf ; n: 18">6%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -18% ; ratio: ÷1.59 ; OR: Inf ; n: 85">30%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   281)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.30 ; OR: 1.00 ; n: 5">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.18 ; OR: 1/1.53 ; n: 346">22%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.59 ; OR: 1/2.06 ; n: 34">2%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.07 ; OR: 1/1.22 ; n: 231">15%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.08 ; OR: 1/1.21 ; n: 136">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×1.08 ; OR: 1/1.20 ; n: 807">52%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 1 559)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2008</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 5">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 531">26%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 70">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 281">14%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 164">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 972">48%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 023)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +13% ; ratio: ×1.46 ; OR: Inf ; n: 74">40%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.89 ; OR: Inf ; n: 11">6%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.53 ; OR: Inf ; n: 20">11%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.80 ; OR: Inf ; n: 9">5%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.16 ; OR: Inf ; n: 69">38%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   183)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +23% ; ratio: ×1.85 ; OR: Inf ; n: 159">51%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.24 ; OR: Inf ; n: 8">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.04 ; OR: Inf ; n: 54">17%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.20 ; OR: Inf ; n: 23">7%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -22% ; ratio: ÷2.02 ; OR: Inf ; n: 67">22%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   311)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.32 ; OR: 1.00 ; n: 1">0%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.29 ; OR: 1/1.70 ; n: 332">21%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ÷1.07 ; OR: 1/1.41 ; n: 46">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.03 ; OR: 1/1.28 ; n: 267">17%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.09 ; OR: 1/1.21 ; n: 149">10%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×1.12 ; OR: 1/1.18 ; n: 755">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 1 550)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2010</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 565">28%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 65">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 341">17%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 181">9%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 891">44%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 044)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +9% ; ratio: ×1.34 ; n: 70">36%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +2% ; ratio: ×1.63 ; n: 11">6%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -9% ; ratio: ÷2.42 ; n: 13">7%</td><td class="tx-r tx-num m1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷2.70 ; n: 6">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.07 ; n: 96">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   196)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +13% ; ratio: ×1.50 ; n: 120">40%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.25 ; n: 13">4%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +4% ; ratio: ×1.22 ; n: 59">20%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ÷1.04 ; n: 24">8%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -17% ; ratio: ÷1.61 ; n: 85">28%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   301)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; n: 0">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷1.17 ; n: 336">23%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ÷1.16 ; n: 44">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.03 ; n: 245">17%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.09 ; n: 133">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.07 ; n: 719">49%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 1 477)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2012</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 0">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 526">27%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 68">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 317">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 163">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 900">46%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 1 974)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Other</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×0 ; n: 0">0%</td><td class="tx-r tx-num p1 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +8% ; ratio: ×1.31 ; OR: Inf ; n: 91">35%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.44 ; OR: Inf ; n: 12">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -2% ; ratio: ÷1.15 ; OR: Inf ; n: 37">14%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -4% ; ratio: ÷2.16 ; OR: Inf ; n: 10">4%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.07 ; OR: Inf ; n: 112">43%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   262)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Black</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×3.29 ; OR: 1.00 ; n: 2">1%</td><td class="tx-r tx-num p3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +17% ; ratio: ×1.63 ; OR: 1/2.02 ; n: 167">43%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +2% ; ratio: ×1.54 ; OR: 1/2.13 ; n: 19">5%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.09 ; OR: 1/3.02 ; n: 68">18%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.04 ; OR: 1/3.17 ; n: 33">9%</td><td class="tx-r tx-num m3 tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -20% ; ratio: ÷1.82 ; OR: 1/5.97 ; n: 97">25%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (   386)</span></td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">White</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ÷1.49 ; OR: 1.00 ; n: 2">0%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷1.21 ; OR: 1.24 ; n: 417">22%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.21 ; OR: 1.23 ; n: 50">3%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1.00 ; OR: 1.49 ; n: 306">16%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.07 ; OR: 1.59 ; n: 166">9%</td><td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×1.10 ; OR: 1.64 ; n: 949">50%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 1 890)</span></td></tr>
#> <tr class="tx-b tx-bt tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total 2014</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 4">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 675">27%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 81">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 411">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 209">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 158">46%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> ( 2 538)</span></td></tr>
#> <tr class="tx-b tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total Ensemble</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 17">0%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 5 416">25%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 743">3%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 3 383">16%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 807">8%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 10 117">47%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">100%<span class="tx-sec" style="font-weight:normal;"> (21 483)</span></td></tr></tbody><tfoot><tr><td colspan="8"><div class="tx-foot">Percentage points (risk) difference: cell ≥ the Total row <span class="p1" style="font-weight:bold;">+5</span>; <span class="p3" style="font-weight:bold;">+15</span>; <span class="p4" style="font-weight:bold;">+30</span> points; cell ≤ the Total row <span class="m1" style="font-weight:bold;">-5</span>; <span class="m3" style="font-weight:bold;">-15</span>; <span class="m4" style="font-weight:bold;">-30</span> points.</div></td></tr></tfoot></table>
# }
```
