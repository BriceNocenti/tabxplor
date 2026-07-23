# Generate the tabxplor stylesheet

The CSS that colours tabxplor tables. It is a **constant** – a pure
function of the colour palette, the channel type and the theme – so it
does not take a table: one stylesheet styles every table in a document,
whatever their `color_breaks`.

## Usage

``` r
tab_css(
  theme = NULL,
  color_type = lifecycle::deprecated(),
  chrome = TRUE,
  style_tag = TRUE,
  file = NULL
)
```

## Arguments

- theme:

  `"light"`, `"dark"`, or – opt-in – `"auto"` to follow the reader's
  colour scheme (their operating system, and any dark-mode toggle of the
  host page: Quarto, Bootstrap 5.3, Tailwind). Defaults to
  `getOption("tabxplor.theme")`, i.e. `"light"`: a dark table is always
  a deliberate choice. `"auto"` emits every rule four times (a light
  base, the OS media query, then both toggle directions), which is also
  what lets
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)'s
  own Viewer page force the editor's theme – see its `theme` argument.

- color_type:

  **\[deprecated\]** Inert since 2.0.0: the text channel always uses the
  text palette. The colour CHANNEL is chosen by
  `color = c(text, background)` (see
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)).

- chrome:

  When `TRUE` (default) also style the table itself
  (font/background/border colours, the greys) – what
  `tab_kable(engine = "html")` needs. `FALSE` emits the colour classes
  only, which is what
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  wants: bare selectors you can map in your own editor's CSS.

- style_tag:

  Wrap the CSS in a `<style>` tag (default `TRUE`).

- file:

  Optional path to write to instead of returning.

## Value

The CSS, invisibly when `file` is given. Printed as-is by `knitr` with
`results = "asis"`.

## Details

Cells carry classes named after the palette **slot** (`.p1`-`.p4`
over-represented text, `.m1`-`.m4` under-represented text, `.o1`-`.o4` /
`.u1`-`.u4` for the background channel), so
[`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
and
[`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
share one vocabulary.

## Two workflows

**Self-contained (the default).** `tab_kable(css = TRUE)` and
`tab_md(css = TRUE)` inline the stylesheet with the table, so a single
file works anywhere (RStudio/Positron Viewer, jamovi, a standalone
`.html`). Nothing to do.

**Once per document.** In an `.Rmd`/`.qmd` with many tables, emit it
once and let every table reuse it:

    ```{r, results = "asis"}
    options(tabxplor.tab_kable_css = FALSE)
    tab_css(theme = "auto")
    ```

Every later
[`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
then emits classes only. Two things to know: with `css = FALSE` and
**no** `tab_css()` call the tables render uncoloured; and one stylesheet
means one `theme` for the whole document.

## Restyling a table

Nothing is written inline on a cell, so **any** of the look can be
overridden by adding your own rules after the stylesheet – no
`!important` needed. Column widths in particular are left to the browser
(it sizes each column to its content); to pin one, style its role:

    .tabxplor-tab .tx-rv  { min-width: 10em; }   /* the row-variable levels column */
    .tabxplor-tab .tx-tot { min-width: 5.5em; }  /* total columns                  */
    .tabxplor-tab .tx-num { min-width: 4em; }    /* every number column            */

The roles a cell can carry: `.tx-l`/`.tx-r` (alignment), `.tx-num`
(numbers), `.tx-rv` (the row-variable levels column), `.tx-tot` (total
columns), `.tx-bl`/`.tx-br` (side borders), `.tx-b` (bold),
`.tx-lbl`/`.tx-vname` (a variable name spanning its block), `.tx-pill`
(a background-coloured value), `.tx-span` (the variable-name header
row), `.tx-foot` (the footnote). Rows carry `.tx-bt`/`.tx-bb`/`.tx-bb2`
(top / bottom / thick-bottom rules).

## See also

[`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
[`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md),
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)

## Examples

``` r
cat(tab_css(theme = "auto"))
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
#> @media (prefers-color-scheme: dark) {
#>   .tabxplor-tab{color:#CECDC3;background:#222222;}
#>   .tabxplor-tab th,.tabxplor-tab td{border-color:#CECDC3;}
#>   .tabxplor-tab tbody tr:hover{background:rgba(255,242,204,.10);}
#>   .g1{color:#707070;}
#>   .g2{color:#EEEEEE;}
#>   .tabxplor-caption{color:#FFFFFF;}
#>   .p1{color:#028282;}
#>   .p2{color:#018BC1;}
#>   .p3{color:#4687D8;}
#>   .p4{color:#6987FF;}
#>   .m1{color:#867002;}
#>   .m2{color:#B87501;}
#>   .m3{color:#EC6F02;}
#>   .m4{color:#FF626B;}
#>   .o1{background-color:#001B1B;}
#>   .o2{background-color:#002537;}
#>   .o3{background-color:#132D5C;}
#>   .o4{background-color:#17226D;}
#>   .u1{background-color:#1C1600;}
#>   .u2{background-color:#321C00;}
#>   .u3{background-color:#4C1F00;}
#>   .u4{background-color:#6B141F;}
#> }
#> body.quarto-light .tabxplor-tab,[data-bs-theme=light] .tabxplor-tab,[data-theme=light] .tabxplor-tab{color:#000000;background:#ffffff;}
#> body.quarto-light .tabxplor-tab th,body.quarto-light .tabxplor-tab td,[data-bs-theme=light] .tabxplor-tab th,[data-bs-theme=light] .tabxplor-tab td,[data-theme=light] .tabxplor-tab th,[data-theme=light] .tabxplor-tab td{border-color:#000000;}
#> body.quarto-light .tabxplor-tab tbody tr:hover,[data-bs-theme=light] .tabxplor-tab tbody tr:hover,[data-theme=light] .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
#> body.quarto-light .g1,[data-bs-theme=light] .g1,[data-theme=light] .g1{color:#9f9f9f;}
#> body.quarto-light .g2,[data-bs-theme=light] .g2,[data-theme=light] .g2{color:#111111;}
#> body.quarto-light .tabxplor-caption,[data-bs-theme=light] .tabxplor-caption,[data-theme=light] .tabxplor-caption{color:#000000;}
#> body.quarto-light .p1,[data-bs-theme=light] .p1,[data-theme=light] .p1{color:#02A5B3;}
#> body.quarto-light .p2,[data-bs-theme=light] .p2,[data-theme=light] .p2{color:#0891C9;}
#> body.quarto-light .p3,[data-bs-theme=light] .p3,[data-theme=light] .p3{color:#0267C7;}
#> body.quarto-light .p4,[data-bs-theme=light] .p4,[data-theme=light] .p4{color:#300DFD;}
#> body.quarto-light .m1,[data-bs-theme=light] .m1,[data-theme=light] .m1{color:#DCA331;}
#> body.quarto-light .m2,[data-bs-theme=light] .m2,[data-theme=light] .m2{color:#DE7C01;}
#> body.quarto-light .m3,[data-bs-theme=light] .m3,[data-theme=light] .m3{color:#DD5301;}
#> body.quarto-light .m4,[data-bs-theme=light] .m4,[data-theme=light] .m4{color:#D60103;}
#> body.quarto-light .o1,[data-bs-theme=light] .o1,[data-theme=light] .o1{background-color:#DFFCFF;}
#> body.quarto-light .o2,[data-bs-theme=light] .o2,[data-theme=light] .o2{background-color:#D7EFFF;}
#> body.quarto-light .o3,[data-bs-theme=light] .o3,[data-theme=light] .o3{background-color:#CEE3FF;}
#> body.quarto-light .o4,[data-bs-theme=light] .o4,[data-theme=light] .o4{background-color:#BBCCFF;}
#> body.quarto-light .u1,[data-bs-theme=light] .u1,[data-theme=light] .u1{background-color:#FFF4E1;}
#> body.quarto-light .u2,[data-bs-theme=light] .u2,[data-theme=light] .u2{background-color:#FFE6D3;}
#> body.quarto-light .u3,[data-bs-theme=light] .u3,[data-theme=light] .u3{background-color:#FFD7C8;}
#> body.quarto-light .u4,[data-bs-theme=light] .u4,[data-theme=light] .u4{background-color:#FFBAAF;}
#> body.quarto-dark .tabxplor-tab,[data-bs-theme=dark] .tabxplor-tab,[data-theme=dark] .tabxplor-tab,html.dark .tabxplor-tab{color:#CECDC3;background:#222222;}
#> body.quarto-dark .tabxplor-tab th,body.quarto-dark .tabxplor-tab td,[data-bs-theme=dark] .tabxplor-tab th,[data-bs-theme=dark] .tabxplor-tab td,[data-theme=dark] .tabxplor-tab th,[data-theme=dark] .tabxplor-tab td,html.dark .tabxplor-tab th,html.dark .tabxplor-tab td{border-color:#CECDC3;}
#> body.quarto-dark .tabxplor-tab tbody tr:hover,[data-bs-theme=dark] .tabxplor-tab tbody tr:hover,[data-theme=dark] .tabxplor-tab tbody tr:hover,html.dark .tabxplor-tab tbody tr:hover{background:rgba(255,242,204,.10);}
#> body.quarto-dark .g1,[data-bs-theme=dark] .g1,[data-theme=dark] .g1,html.dark .g1{color:#707070;}
#> body.quarto-dark .g2,[data-bs-theme=dark] .g2,[data-theme=dark] .g2,html.dark .g2{color:#EEEEEE;}
#> body.quarto-dark .tabxplor-caption,[data-bs-theme=dark] .tabxplor-caption,[data-theme=dark] .tabxplor-caption,html.dark .tabxplor-caption{color:#FFFFFF;}
#> body.quarto-dark .p1,[data-bs-theme=dark] .p1,[data-theme=dark] .p1,html.dark .p1{color:#028282;}
#> body.quarto-dark .p2,[data-bs-theme=dark] .p2,[data-theme=dark] .p2,html.dark .p2{color:#018BC1;}
#> body.quarto-dark .p3,[data-bs-theme=dark] .p3,[data-theme=dark] .p3,html.dark .p3{color:#4687D8;}
#> body.quarto-dark .p4,[data-bs-theme=dark] .p4,[data-theme=dark] .p4,html.dark .p4{color:#6987FF;}
#> body.quarto-dark .m1,[data-bs-theme=dark] .m1,[data-theme=dark] .m1,html.dark .m1{color:#867002;}
#> body.quarto-dark .m2,[data-bs-theme=dark] .m2,[data-theme=dark] .m2,html.dark .m2{color:#B87501;}
#> body.quarto-dark .m3,[data-bs-theme=dark] .m3,[data-theme=dark] .m3,html.dark .m3{color:#EC6F02;}
#> body.quarto-dark .m4,[data-bs-theme=dark] .m4,[data-theme=dark] .m4,html.dark .m4{color:#FF626B;}
#> body.quarto-dark .o1,[data-bs-theme=dark] .o1,[data-theme=dark] .o1,html.dark .o1{background-color:#001B1B;}
#> body.quarto-dark .o2,[data-bs-theme=dark] .o2,[data-theme=dark] .o2,html.dark .o2{background-color:#002537;}
#> body.quarto-dark .o3,[data-bs-theme=dark] .o3,[data-theme=dark] .o3,html.dark .o3{background-color:#132D5C;}
#> body.quarto-dark .o4,[data-bs-theme=dark] .o4,[data-theme=dark] .o4,html.dark .o4{background-color:#17226D;}
#> body.quarto-dark .u1,[data-bs-theme=dark] .u1,[data-theme=dark] .u1,html.dark .u1{background-color:#1C1600;}
#> body.quarto-dark .u2,[data-bs-theme=dark] .u2,[data-theme=dark] .u2,html.dark .u2{background-color:#321C00;}
#> body.quarto-dark .u3,[data-bs-theme=dark] .u3,[data-theme=dark] .u3,html.dark .u3{background-color:#4C1F00;}
#> body.quarto-dark .u4,[data-bs-theme=dark] .u4,[data-theme=dark] .u4,html.dark .u4{background-color:#6B141F;}
#> </style>
cat(tab_css(chrome = FALSE, style_tag = FALSE))  # the markdown flavour
#> .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
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
```
