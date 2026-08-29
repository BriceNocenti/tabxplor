# The stylesheet an html table needs

The CSS that colours tabxplor tables. It is a **constant** – a pure
function of the colour palette, the channel type and the theme – so it
does not take a table: one stylesheet styles every table in a document,
whatever their `color_breaks`.

## Usage

``` r
tab_css(
  theme = NULL,
  format = c("html", "md"),
  style_tag = TRUE,
  file = NULL,
  print_rules = NULL,
  ...
)
```

## Arguments

- theme:

  `"light"`, `"dark"`, a black-and-white publication palette
  (`"print_ready"`, `"print_marks"`, `"print_emphasis"`,
  `"print_minimalistic"`; `"bw"` is a synonym of the last – see the
  section below), or – opt-in – `"auto"` to follow the reader's colour
  scheme (their operating system, and any dark-mode toggle of the host
  page: Quarto, Bootstrap 5.3, Tailwind). Defaults to
  `getOption("tabxplor.theme")`, i.e. `"light"`: a dark table is always
  a deliberate choice. `"auto"` emits every rule four times (a light
  base, the OS media query, then both toggle directions), which is also
  what lets
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)'s
  own Viewer page force the editor's theme.

- format:

  Which output the stylesheet is for, in
  [`tab_export()`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md)'s
  own vocabulary. `"html"` (the default) is the full stylesheet
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  needs: the colour classes **and** the table's own look (font,
  background, border colours, the greys). `"md"` emits the colour
  classes only, which is what
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  wants — bare selectors you can map in your own editor's or publisher's
  CSS.

- style_tag:

  Wrap the CSS in a `<style>` tag (default `TRUE`).

- file:

  Optional path to write to instead of returning.

- print_rules:

  Also emit a black-and-white publication palette inside an
  `@media print` block, so a coloured page prints (or saves to PDF)
  publication-ready with no further action. Defaults to
  `getOption("tabxplor.print_rules")`. Set to `FALSE` if your printer is
  a colour one and the colours are the point, or name a palette
  (`"print_emphasis"`) to print in that one. `"print_marks"` cannot be
  used here: its marks are cell text, and a print rule can restyle a
  page but not add characters to it. It adds roughly 1.5 KB to a
  `light`/`dark` stylesheet and 6 KB to an `"auto"` one.

- ...:

  Retired arguments, accepted and ignored with a deprecation message
  since 2.0.0 (`color_type`): the text channel always uses the text
  palette, and the colour CHANNEL is chosen by
  `color = c(text, background)` (see
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)).
  Anything else is an error naming the argument you meant, as it already
  was in
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

## Value

The CSS, invisibly when `file` is given. Printed as-is by `knitr` with
`results = "asis"`.

## Details

Cells carry classes named after the palette **slot** (`.p1`-`.p4`
over-represented text, `.m1`-`.m4` under-represented text, `.o1`-`.o4` /
`.u1`-`.u4` for the background channel), so
[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
and
[`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
share one vocabulary.

## Two workflows

**Self-contained (the default).** `tab_html(css = TRUE)` and
`tab_md(css = TRUE)` inline the stylesheet with the table, so a single
file works anywhere (the RStudio/Positron Viewer, jamovi, a standalone
`.html`). Nothing to do.

**Once per document.** In an `.Rmd`/`.qmd` with many tables, emit it
once and let every table reuse it:

    ```{r, results = "asis"}
    options(tabxplor.tab_kable_css = FALSE)
    tab_css(theme = "auto")
    ```

Every later
[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
then emits classes only. Two things to know: with `css = FALSE` and
**no** `tab_css()` call the tables render uncoloured; and one stylesheet
means one `theme` for the whole document.

## Restyling a table

Nothing is written inline on a cell, so **any** of the look can be
overridden by adding your own rules after the stylesheet – no
`!important` needed. The cell colour classes are also emitted scoped
(`.tabxplor-tab .p1`) so they survive host pages that style table cells
themselves, such as Bootstrap-based sites including pkgdown. Column
widths in particular are left to the browser, which sizes each column to
its content; to pin one, style its role:


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

## The black-and-white publication palettes

A greyscale print loses colour entirely — both direction ramps become
the same grey — so these palettes say the same thing with something
else. **`theme = "print_ready"` is the one to reach for**: it picks per
table, the marks for a cross-table and the emphasis ladder for a
regression, whose cells already carry their own direction symbol. Name
one yourself to override that. They share ONE grey fill ramp (a
background colour measure keeps carrying its magnitude) and differ in
the text channel:

- `"print_minimalistic"`:

  direction by underline (over) and italic (under); magnitude by an ink
  ladder.

- `"print_emphasis"`:

  magnitude by an emphasis ladder (bold, then underline, then double
  underline) in pure black; direction by the cell's own measure symbol,
  plus italic under the null.

- `"print_marks"`:

  magnitude and direction by a repeated superscript mark after the value
  (no significance stars: the marks take their place, do not use with
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)).

In all of them a non-significant cell is greyed out, and the
significance stars stay — except under `"print_marks"`, where the marks
take their place (one run of symbols after a value, not two). `"bw"` is
a synonym of `"print_minimalistic"`. *One caveat*, and only for a
document that emits `tab_css()` once and renders its tables with
`css = FALSE`: a stylesheet is table-independent, so it carries ONE of
them. A cross-table is fine whatever it carries (its marks are cell
text), but a regression's ladder is css and nothing else — name it
there, `tab_css(theme = "print_emphasis")`.

## See also

[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
[`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md),
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)

## Examples

``` r
cat(tab_css(theme = "auto"))
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
#> @media (prefers-color-scheme: dark) {
#>   .tabxplor-tab{color:#f1efe0;background:transparent;}
#>   .tabxplor-tab td:not(.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4) .tx-pill,:is(.o1,.o2,.o3,.o4,.u1,.u2,.u3,.u4):not(.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4):not(.tx-pill),.tabxplor-tab :is(.o1,.o2,.o3,.o4,.u1,.u2,.u3,.u4):not(.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4):not(.tx-pill){color:#21252b;}
#>   .tabxplor-tab th,.tabxplor-tab td{background-color:transparent;border-color:#CDCBBC;}
#>   .tabxplor-tab tbody tr:hover{background:rgba(255,242,204,.10);}
#>   .g1,.tabxplor-tab .g1{color:#919085;}
#>   .g2,.tabxplor-tab .g2{color:#CDCBBC;}
#>   .tabxplor-tab .tx-unit{color:#919085;}
#>   .tabxplor-caption{color:#FFFFFF;}
#>   .tabxplor-tab .tx-foot{color:#CDCBBC;}
#>   .tabxplor-tab.tx-shape{color:#CDCBBC;}
#>   .tabxplor-tab.tx-shape thead th{color:#CDCBBC;}
#>   .tabxplor-tab.tx-shape .tx-sec{color:#919085;}
#>   .tabxplor-tab .tx-sec{color:#CDCBBC;}
#>   .p1,.tabxplor-tab .p1{color:#2BA1A7;}
#>   .p2,.tabxplor-tab .p2{color:#37A8D7;}
#>   .p3,.tabxplor-tab .p3{color:#72A7FF;}
#>   .p4,.tabxplor-tab .p4{color:#9C84FF;}
#>   .m1,.tabxplor-tab .m1{color:#D6A13D;}
#>   .m2,.tabxplor-tab .m2{color:#EC923E;}
#>   .m3,.tabxplor-tab .m3{color:#FF885E;}
#>   .m4,.tabxplor-tab .m4{color:#FF635F;}
#>   .o1,.tabxplor-tab .o1{background-color:#C3ECEE;}
#>   .o2,.tabxplor-tab .o2{background-color:#B4E0F6;}
#>   .o3,.tabxplor-tab .o3{background-color:#B3CFFD;}
#>   .o4,.tabxplor-tab .o4{background-color:#C1B9FC;}
#>   .u1,.tabxplor-tab .u1{background-color:#F3E0C2;}
#>   .u2,.tabxplor-tab .u2{background-color:#F6D0B2;}
#>   .u3,.tabxplor-tab .u3{background-color:#FABDA8;}
#>   .u4,.tabxplor-tab .u4{background-color:#FCAAA3;}
#> }
#> body.quarto-light .tabxplor-tab,[data-bs-theme=light] .tabxplor-tab,[data-theme=light] .tabxplor-tab{color:#000000;background:transparent;}
#> body.quarto-light .tabxplor-tab th,body.quarto-light .tabxplor-tab td,[data-bs-theme=light] .tabxplor-tab th,[data-bs-theme=light] .tabxplor-tab td,[data-theme=light] .tabxplor-tab th,[data-theme=light] .tabxplor-tab td{background-color:transparent;border-color:#000000;}
#> body.quarto-light .tabxplor-tab tbody tr:hover,[data-bs-theme=light] .tabxplor-tab tbody tr:hover,[data-theme=light] .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
#> body.quarto-light .g1,body.quarto-light .tabxplor-tab .g1,[data-bs-theme=light] .g1,[data-bs-theme=light] .tabxplor-tab .g1,[data-theme=light] .g1,[data-theme=light] .tabxplor-tab .g1{color:#949494;}
#> body.quarto-light .g2,body.quarto-light .tabxplor-tab .g2,[data-bs-theme=light] .g2,[data-bs-theme=light] .tabxplor-tab .g2,[data-theme=light] .g2,[data-theme=light] .tabxplor-tab .g2{color:#444444;}
#> body.quarto-light .tabxplor-tab .tx-unit,[data-bs-theme=light] .tabxplor-tab .tx-unit,[data-theme=light] .tabxplor-tab .tx-unit{color:#949494;}
#> body.quarto-light .tabxplor-caption,[data-bs-theme=light] .tabxplor-caption,[data-theme=light] .tabxplor-caption{color:#000000;}
#> body.quarto-light .tabxplor-tab .tx-foot,[data-bs-theme=light] .tabxplor-tab .tx-foot,[data-theme=light] .tabxplor-tab .tx-foot{color:#444444;}
#> body.quarto-light .tabxplor-tab.tx-shape,[data-bs-theme=light] .tabxplor-tab.tx-shape,[data-theme=light] .tabxplor-tab.tx-shape{color:#444444;}
#> body.quarto-light .tabxplor-tab.tx-shape thead th,[data-bs-theme=light] .tabxplor-tab.tx-shape thead th,[data-theme=light] .tabxplor-tab.tx-shape thead th{color:#444444;}
#> body.quarto-light .tabxplor-tab.tx-shape .tx-sec,[data-bs-theme=light] .tabxplor-tab.tx-shape .tx-sec,[data-theme=light] .tabxplor-tab.tx-shape .tx-sec{color:#949494;}
#> body.quarto-light .tabxplor-tab .tx-sec,[data-bs-theme=light] .tabxplor-tab .tx-sec,[data-theme=light] .tabxplor-tab .tx-sec{color:#444444;}
#> body.quarto-light .p1,body.quarto-light .tabxplor-tab .p1,[data-bs-theme=light] .p1,[data-bs-theme=light] .tabxplor-tab .p1,[data-theme=light] .p1,[data-theme=light] .tabxplor-tab .p1{color:#02A5B3;}
#> body.quarto-light .p2,body.quarto-light .tabxplor-tab .p2,[data-bs-theme=light] .p2,[data-bs-theme=light] .tabxplor-tab .p2,[data-theme=light] .p2,[data-theme=light] .tabxplor-tab .p2{color:#0891C9;}
#> body.quarto-light .p3,body.quarto-light .tabxplor-tab .p3,[data-bs-theme=light] .p3,[data-bs-theme=light] .tabxplor-tab .p3,[data-theme=light] .p3,[data-theme=light] .tabxplor-tab .p3{color:#0267C7;}
#> body.quarto-light .p4,body.quarto-light .tabxplor-tab .p4,[data-bs-theme=light] .p4,[data-bs-theme=light] .tabxplor-tab .p4,[data-theme=light] .p4,[data-theme=light] .tabxplor-tab .p4{color:#300DFD;}
#> body.quarto-light .m1,body.quarto-light .tabxplor-tab .m1,[data-bs-theme=light] .m1,[data-bs-theme=light] .tabxplor-tab .m1,[data-theme=light] .m1,[data-theme=light] .tabxplor-tab .m1{color:#DCA331;}
#> body.quarto-light .m2,body.quarto-light .tabxplor-tab .m2,[data-bs-theme=light] .m2,[data-bs-theme=light] .tabxplor-tab .m2,[data-theme=light] .m2,[data-theme=light] .tabxplor-tab .m2{color:#DE7C01;}
#> body.quarto-light .m3,body.quarto-light .tabxplor-tab .m3,[data-bs-theme=light] .m3,[data-bs-theme=light] .tabxplor-tab .m3,[data-theme=light] .m3,[data-theme=light] .tabxplor-tab .m3{color:#DD5301;}
#> body.quarto-light .m4,body.quarto-light .tabxplor-tab .m4,[data-bs-theme=light] .m4,[data-bs-theme=light] .tabxplor-tab .m4,[data-theme=light] .m4,[data-theme=light] .tabxplor-tab .m4{color:#D60103;}
#> body.quarto-light .o1,body.quarto-light .tabxplor-tab .o1,[data-bs-theme=light] .o1,[data-bs-theme=light] .tabxplor-tab .o1,[data-theme=light] .o1,[data-theme=light] .tabxplor-tab .o1{background-color:#C4EAEE;}
#> body.quarto-light .o2,body.quarto-light .tabxplor-tab .o2,[data-bs-theme=light] .o2,[data-bs-theme=light] .tabxplor-tab .o2,[data-theme=light] .o2,[data-theme=light] .tabxplor-tab .o2{background-color:#B7DEF6;}
#> body.quarto-light .o3,body.quarto-light .tabxplor-tab .o3,[data-bs-theme=light] .o3,[data-bs-theme=light] .tabxplor-tab .o3,[data-theme=light] .o3,[data-theme=light] .tabxplor-tab .o3{background-color:#B2D0F8;}
#> body.quarto-light .o4,body.quarto-light .tabxplor-tab .o4,[data-bs-theme=light] .o4,[data-bs-theme=light] .tabxplor-tab .o4,[data-theme=light] .o4,[data-theme=light] .tabxplor-tab .o4{background-color:#AEC2FF;}
#> body.quarto-light .u1,body.quarto-light .tabxplor-tab .u1,[data-bs-theme=light] .u1,[data-bs-theme=light] .tabxplor-tab .u1,[data-theme=light] .u1,[data-theme=light] .tabxplor-tab .u1{background-color:#F0DFC4;}
#> body.quarto-light .u2,body.quarto-light .tabxplor-tab .u2,[data-bs-theme=light] .u2,[data-bs-theme=light] .tabxplor-tab .u2,[data-theme=light] .u2,[data-theme=light] .tabxplor-tab .u2{background-color:#F6CFB0;}
#> body.quarto-light .u3,body.quarto-light .tabxplor-tab .u3,[data-bs-theme=light] .u3,[data-bs-theme=light] .tabxplor-tab .u3,[data-theme=light] .u3,[data-theme=light] .tabxplor-tab .u3{background-color:#FCBDA5;}
#> body.quarto-light .u4,body.quarto-light .tabxplor-tab .u4,[data-bs-theme=light] .u4,[data-bs-theme=light] .tabxplor-tab .u4,[data-theme=light] .u4,[data-theme=light] .tabxplor-tab .u4{background-color:#FEAC9F;}
#> body.quarto-dark .tabxplor-tab,[data-bs-theme=dark] .tabxplor-tab,[data-theme=dark] .tabxplor-tab,html.dark .tabxplor-tab{color:#f1efe0;background:transparent;}
#> body.quarto-dark .tabxplor-tab td:not(.p1,body.quarto-dark .p2,body.quarto-dark .p3,body.quarto-dark .p4,body.quarto-dark .m1,body.quarto-dark .m2,body.quarto-dark .m3,body.quarto-dark .m4) .tx-pill,body.quarto-dark :is(.o1,body.quarto-dark .o2,body.quarto-dark .o3,body.quarto-dark .o4,body.quarto-dark .u1,body.quarto-dark .u2,body.quarto-dark .u3,body.quarto-dark .u4):not(.p1,body.quarto-dark .p2,body.quarto-dark .p3,body.quarto-dark .p4,body.quarto-dark .m1,body.quarto-dark .m2,body.quarto-dark .m3,body.quarto-dark .m4):not(.tx-pill),body.quarto-dark .tabxplor-tab :is(.o1,body.quarto-dark .o2,body.quarto-dark .o3,body.quarto-dark .o4,body.quarto-dark .u1,body.quarto-dark .u2,body.quarto-dark .u3,body.quarto-dark .u4):not(.p1,body.quarto-dark .p2,body.quarto-dark .p3,body.quarto-dark .p4,body.quarto-dark .m1,body.quarto-dark .m2,body.quarto-dark .m3,body.quarto-dark .m4):not(.tx-pill),[data-bs-theme=dark] .tabxplor-tab td:not(.p1,[data-bs-theme=dark] .p2,[data-bs-theme=dark] .p3,[data-bs-theme=dark] .p4,[data-bs-theme=dark] .m1,[data-bs-theme=dark] .m2,[data-bs-theme=dark] .m3,[data-bs-theme=dark] .m4) .tx-pill,[data-bs-theme=dark] :is(.o1,[data-bs-theme=dark] .o2,[data-bs-theme=dark] .o3,[data-bs-theme=dark] .o4,[data-bs-theme=dark] .u1,[data-bs-theme=dark] .u2,[data-bs-theme=dark] .u3,[data-bs-theme=dark] .u4):not(.p1,[data-bs-theme=dark] .p2,[data-bs-theme=dark] .p3,[data-bs-theme=dark] .p4,[data-bs-theme=dark] .m1,[data-bs-theme=dark] .m2,[data-bs-theme=dark] .m3,[data-bs-theme=dark] .m4):not(.tx-pill),[data-bs-theme=dark] .tabxplor-tab :is(.o1,[data-bs-theme=dark] .o2,[data-bs-theme=dark] .o3,[data-bs-theme=dark] .o4,[data-bs-theme=dark] .u1,[data-bs-theme=dark] .u2,[data-bs-theme=dark] .u3,[data-bs-theme=dark] .u4):not(.p1,[data-bs-theme=dark] .p2,[data-bs-theme=dark] .p3,[data-bs-theme=dark] .p4,[data-bs-theme=dark] .m1,[data-bs-theme=dark] .m2,[data-bs-theme=dark] .m3,[data-bs-theme=dark] .m4):not(.tx-pill),[data-theme=dark] .tabxplor-tab td:not(.p1,[data-theme=dark] .p2,[data-theme=dark] .p3,[data-theme=dark] .p4,[data-theme=dark] .m1,[data-theme=dark] .m2,[data-theme=dark] .m3,[data-theme=dark] .m4) .tx-pill,[data-theme=dark] :is(.o1,[data-theme=dark] .o2,[data-theme=dark] .o3,[data-theme=dark] .o4,[data-theme=dark] .u1,[data-theme=dark] .u2,[data-theme=dark] .u3,[data-theme=dark] .u4):not(.p1,[data-theme=dark] .p2,[data-theme=dark] .p3,[data-theme=dark] .p4,[data-theme=dark] .m1,[data-theme=dark] .m2,[data-theme=dark] .m3,[data-theme=dark] .m4):not(.tx-pill),[data-theme=dark] .tabxplor-tab :is(.o1,[data-theme=dark] .o2,[data-theme=dark] .o3,[data-theme=dark] .o4,[data-theme=dark] .u1,[data-theme=dark] .u2,[data-theme=dark] .u3,[data-theme=dark] .u4):not(.p1,[data-theme=dark] .p2,[data-theme=dark] .p3,[data-theme=dark] .p4,[data-theme=dark] .m1,[data-theme=dark] .m2,[data-theme=dark] .m3,[data-theme=dark] .m4):not(.tx-pill),html.dark .tabxplor-tab td:not(.p1,html.dark .p2,html.dark .p3,html.dark .p4,html.dark .m1,html.dark .m2,html.dark .m3,html.dark .m4) .tx-pill,html.dark :is(.o1,html.dark .o2,html.dark .o3,html.dark .o4,html.dark .u1,html.dark .u2,html.dark .u3,html.dark .u4):not(.p1,html.dark .p2,html.dark .p3,html.dark .p4,html.dark .m1,html.dark .m2,html.dark .m3,html.dark .m4):not(.tx-pill),html.dark .tabxplor-tab :is(.o1,html.dark .o2,html.dark .o3,html.dark .o4,html.dark .u1,html.dark .u2,html.dark .u3,html.dark .u4):not(.p1,html.dark .p2,html.dark .p3,html.dark .p4,html.dark .m1,html.dark .m2,html.dark .m3,html.dark .m4):not(.tx-pill){color:#21252b;}
#> body.quarto-dark .tabxplor-tab th,body.quarto-dark .tabxplor-tab td,[data-bs-theme=dark] .tabxplor-tab th,[data-bs-theme=dark] .tabxplor-tab td,[data-theme=dark] .tabxplor-tab th,[data-theme=dark] .tabxplor-tab td,html.dark .tabxplor-tab th,html.dark .tabxplor-tab td{background-color:transparent;border-color:#CDCBBC;}
#> body.quarto-dark .tabxplor-tab tbody tr:hover,[data-bs-theme=dark] .tabxplor-tab tbody tr:hover,[data-theme=dark] .tabxplor-tab tbody tr:hover,html.dark .tabxplor-tab tbody tr:hover{background:rgba(255,242,204,.10);}
#> body.quarto-dark .g1,body.quarto-dark .tabxplor-tab .g1,[data-bs-theme=dark] .g1,[data-bs-theme=dark] .tabxplor-tab .g1,[data-theme=dark] .g1,[data-theme=dark] .tabxplor-tab .g1,html.dark .g1,html.dark .tabxplor-tab .g1{color:#919085;}
#> body.quarto-dark .g2,body.quarto-dark .tabxplor-tab .g2,[data-bs-theme=dark] .g2,[data-bs-theme=dark] .tabxplor-tab .g2,[data-theme=dark] .g2,[data-theme=dark] .tabxplor-tab .g2,html.dark .g2,html.dark .tabxplor-tab .g2{color:#CDCBBC;}
#> body.quarto-dark .tabxplor-tab .tx-unit,[data-bs-theme=dark] .tabxplor-tab .tx-unit,[data-theme=dark] .tabxplor-tab .tx-unit,html.dark .tabxplor-tab .tx-unit{color:#919085;}
#> body.quarto-dark .tabxplor-caption,[data-bs-theme=dark] .tabxplor-caption,[data-theme=dark] .tabxplor-caption,html.dark .tabxplor-caption{color:#FFFFFF;}
#> body.quarto-dark .tabxplor-tab .tx-foot,[data-bs-theme=dark] .tabxplor-tab .tx-foot,[data-theme=dark] .tabxplor-tab .tx-foot,html.dark .tabxplor-tab .tx-foot{color:#CDCBBC;}
#> body.quarto-dark .tabxplor-tab.tx-shape,[data-bs-theme=dark] .tabxplor-tab.tx-shape,[data-theme=dark] .tabxplor-tab.tx-shape,html.dark .tabxplor-tab.tx-shape{color:#CDCBBC;}
#> body.quarto-dark .tabxplor-tab.tx-shape thead th,[data-bs-theme=dark] .tabxplor-tab.tx-shape thead th,[data-theme=dark] .tabxplor-tab.tx-shape thead th,html.dark .tabxplor-tab.tx-shape thead th{color:#CDCBBC;}
#> body.quarto-dark .tabxplor-tab.tx-shape .tx-sec,[data-bs-theme=dark] .tabxplor-tab.tx-shape .tx-sec,[data-theme=dark] .tabxplor-tab.tx-shape .tx-sec,html.dark .tabxplor-tab.tx-shape .tx-sec{color:#919085;}
#> body.quarto-dark .tabxplor-tab .tx-sec,[data-bs-theme=dark] .tabxplor-tab .tx-sec,[data-theme=dark] .tabxplor-tab .tx-sec,html.dark .tabxplor-tab .tx-sec{color:#CDCBBC;}
#> body.quarto-dark .p1,body.quarto-dark .tabxplor-tab .p1,[data-bs-theme=dark] .p1,[data-bs-theme=dark] .tabxplor-tab .p1,[data-theme=dark] .p1,[data-theme=dark] .tabxplor-tab .p1,html.dark .p1,html.dark .tabxplor-tab .p1{color:#2BA1A7;}
#> body.quarto-dark .p2,body.quarto-dark .tabxplor-tab .p2,[data-bs-theme=dark] .p2,[data-bs-theme=dark] .tabxplor-tab .p2,[data-theme=dark] .p2,[data-theme=dark] .tabxplor-tab .p2,html.dark .p2,html.dark .tabxplor-tab .p2{color:#37A8D7;}
#> body.quarto-dark .p3,body.quarto-dark .tabxplor-tab .p3,[data-bs-theme=dark] .p3,[data-bs-theme=dark] .tabxplor-tab .p3,[data-theme=dark] .p3,[data-theme=dark] .tabxplor-tab .p3,html.dark .p3,html.dark .tabxplor-tab .p3{color:#72A7FF;}
#> body.quarto-dark .p4,body.quarto-dark .tabxplor-tab .p4,[data-bs-theme=dark] .p4,[data-bs-theme=dark] .tabxplor-tab .p4,[data-theme=dark] .p4,[data-theme=dark] .tabxplor-tab .p4,html.dark .p4,html.dark .tabxplor-tab .p4{color:#9C84FF;}
#> body.quarto-dark .m1,body.quarto-dark .tabxplor-tab .m1,[data-bs-theme=dark] .m1,[data-bs-theme=dark] .tabxplor-tab .m1,[data-theme=dark] .m1,[data-theme=dark] .tabxplor-tab .m1,html.dark .m1,html.dark .tabxplor-tab .m1{color:#D6A13D;}
#> body.quarto-dark .m2,body.quarto-dark .tabxplor-tab .m2,[data-bs-theme=dark] .m2,[data-bs-theme=dark] .tabxplor-tab .m2,[data-theme=dark] .m2,[data-theme=dark] .tabxplor-tab .m2,html.dark .m2,html.dark .tabxplor-tab .m2{color:#EC923E;}
#> body.quarto-dark .m3,body.quarto-dark .tabxplor-tab .m3,[data-bs-theme=dark] .m3,[data-bs-theme=dark] .tabxplor-tab .m3,[data-theme=dark] .m3,[data-theme=dark] .tabxplor-tab .m3,html.dark .m3,html.dark .tabxplor-tab .m3{color:#FF885E;}
#> body.quarto-dark .m4,body.quarto-dark .tabxplor-tab .m4,[data-bs-theme=dark] .m4,[data-bs-theme=dark] .tabxplor-tab .m4,[data-theme=dark] .m4,[data-theme=dark] .tabxplor-tab .m4,html.dark .m4,html.dark .tabxplor-tab .m4{color:#FF635F;}
#> body.quarto-dark .o1,body.quarto-dark .tabxplor-tab .o1,[data-bs-theme=dark] .o1,[data-bs-theme=dark] .tabxplor-tab .o1,[data-theme=dark] .o1,[data-theme=dark] .tabxplor-tab .o1,html.dark .o1,html.dark .tabxplor-tab .o1{background-color:#C3ECEE;}
#> body.quarto-dark .o2,body.quarto-dark .tabxplor-tab .o2,[data-bs-theme=dark] .o2,[data-bs-theme=dark] .tabxplor-tab .o2,[data-theme=dark] .o2,[data-theme=dark] .tabxplor-tab .o2,html.dark .o2,html.dark .tabxplor-tab .o2{background-color:#B4E0F6;}
#> body.quarto-dark .o3,body.quarto-dark .tabxplor-tab .o3,[data-bs-theme=dark] .o3,[data-bs-theme=dark] .tabxplor-tab .o3,[data-theme=dark] .o3,[data-theme=dark] .tabxplor-tab .o3,html.dark .o3,html.dark .tabxplor-tab .o3{background-color:#B3CFFD;}
#> body.quarto-dark .o4,body.quarto-dark .tabxplor-tab .o4,[data-bs-theme=dark] .o4,[data-bs-theme=dark] .tabxplor-tab .o4,[data-theme=dark] .o4,[data-theme=dark] .tabxplor-tab .o4,html.dark .o4,html.dark .tabxplor-tab .o4{background-color:#C1B9FC;}
#> body.quarto-dark .u1,body.quarto-dark .tabxplor-tab .u1,[data-bs-theme=dark] .u1,[data-bs-theme=dark] .tabxplor-tab .u1,[data-theme=dark] .u1,[data-theme=dark] .tabxplor-tab .u1,html.dark .u1,html.dark .tabxplor-tab .u1{background-color:#F3E0C2;}
#> body.quarto-dark .u2,body.quarto-dark .tabxplor-tab .u2,[data-bs-theme=dark] .u2,[data-bs-theme=dark] .tabxplor-tab .u2,[data-theme=dark] .u2,[data-theme=dark] .tabxplor-tab .u2,html.dark .u2,html.dark .tabxplor-tab .u2{background-color:#F6D0B2;}
#> body.quarto-dark .u3,body.quarto-dark .tabxplor-tab .u3,[data-bs-theme=dark] .u3,[data-bs-theme=dark] .tabxplor-tab .u3,[data-theme=dark] .u3,[data-theme=dark] .tabxplor-tab .u3,html.dark .u3,html.dark .tabxplor-tab .u3{background-color:#FABDA8;}
#> body.quarto-dark .u4,body.quarto-dark .tabxplor-tab .u4,[data-bs-theme=dark] .u4,[data-bs-theme=dark] .tabxplor-tab .u4,[data-theme=dark] .u4,[data-theme=dark] .tabxplor-tab .u4,html.dark .u4,html.dark .tabxplor-tab .u4{background-color:#FCAAA3;}
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
#>   body.quarto-light .tabxplor-tab,[data-bs-theme=light] .tabxplor-tab,[data-theme=light] .tabxplor-tab,body.quarto-dark .tabxplor-tab,[data-bs-theme=dark] .tabxplor-tab,[data-theme=dark] .tabxplor-tab,html.dark .tabxplor-tab{color:#000000;background:#ffffff;}
#>   body.quarto-light .tabxplor-tab th,body.quarto-light .tabxplor-tab td,[data-bs-theme=light] .tabxplor-tab th,[data-bs-theme=light] .tabxplor-tab td,[data-theme=light] .tabxplor-tab th,[data-theme=light] .tabxplor-tab td,body.quarto-dark .tabxplor-tab th,body.quarto-dark .tabxplor-tab td,[data-bs-theme=dark] .tabxplor-tab th,[data-bs-theme=dark] .tabxplor-tab td,[data-theme=dark] .tabxplor-tab th,[data-theme=dark] .tabxplor-tab td,html.dark .tabxplor-tab th,html.dark .tabxplor-tab td{color:#000000;background-color:#ffffff;border-color:#000000;}
#>   body.quarto-light .tabxplor-tab tbody tr:hover,[data-bs-theme=light] .tabxplor-tab tbody tr:hover,[data-theme=light] .tabxplor-tab tbody tr:hover,body.quarto-dark .tabxplor-tab tbody tr:hover,[data-bs-theme=dark] .tabxplor-tab tbody tr:hover,[data-theme=dark] .tabxplor-tab tbody tr:hover,html.dark .tabxplor-tab tbody tr:hover{background:transparent;}
#>   body.quarto-light .g1,body.quarto-light .tabxplor-tab .g1,[data-bs-theme=light] .g1,[data-bs-theme=light] .tabxplor-tab .g1,[data-theme=light] .g1,[data-theme=light] .tabxplor-tab .g1,body.quarto-dark .g1,body.quarto-dark .tabxplor-tab .g1,[data-bs-theme=dark] .g1,[data-bs-theme=dark] .tabxplor-tab .g1,[data-theme=dark] .g1,[data-theme=dark] .tabxplor-tab .g1,html.dark .g1,html.dark .tabxplor-tab .g1{color:#949494;}
#>   body.quarto-light .g2,body.quarto-light .tabxplor-tab .g2,[data-bs-theme=light] .g2,[data-bs-theme=light] .tabxplor-tab .g2,[data-theme=light] .g2,[data-theme=light] .tabxplor-tab .g2,body.quarto-dark .g2,body.quarto-dark .tabxplor-tab .g2,[data-bs-theme=dark] .g2,[data-bs-theme=dark] .tabxplor-tab .g2,[data-theme=dark] .g2,[data-theme=dark] .tabxplor-tab .g2,html.dark .g2,html.dark .tabxplor-tab .g2{color:#444444;}
#>   body.quarto-light .tabxplor-tab .tx-unit,[data-bs-theme=light] .tabxplor-tab .tx-unit,[data-theme=light] .tabxplor-tab .tx-unit,body.quarto-dark .tabxplor-tab .tx-unit,[data-bs-theme=dark] .tabxplor-tab .tx-unit,[data-theme=dark] .tabxplor-tab .tx-unit,html.dark .tabxplor-tab .tx-unit{color:#949494;}
#>   body.quarto-light .tabxplor-caption,[data-bs-theme=light] .tabxplor-caption,[data-theme=light] .tabxplor-caption,body.quarto-dark .tabxplor-caption,[data-bs-theme=dark] .tabxplor-caption,[data-theme=dark] .tabxplor-caption,html.dark .tabxplor-caption{color:#000000;}
#>   body.quarto-light .tabxplor-tab .tx-foot,[data-bs-theme=light] .tabxplor-tab .tx-foot,[data-theme=light] .tabxplor-tab .tx-foot,body.quarto-dark .tabxplor-tab .tx-foot,[data-bs-theme=dark] .tabxplor-tab .tx-foot,[data-theme=dark] .tabxplor-tab .tx-foot,html.dark .tabxplor-tab .tx-foot{color:#444444;}
#>   body.quarto-light .tabxplor-tab.tx-shape,[data-bs-theme=light] .tabxplor-tab.tx-shape,[data-theme=light] .tabxplor-tab.tx-shape,body.quarto-dark .tabxplor-tab.tx-shape,[data-bs-theme=dark] .tabxplor-tab.tx-shape,[data-theme=dark] .tabxplor-tab.tx-shape,html.dark .tabxplor-tab.tx-shape{color:#444444;}
#>   body.quarto-light .tabxplor-tab.tx-shape thead th,[data-bs-theme=light] .tabxplor-tab.tx-shape thead th,[data-theme=light] .tabxplor-tab.tx-shape thead th,body.quarto-dark .tabxplor-tab.tx-shape thead th,[data-bs-theme=dark] .tabxplor-tab.tx-shape thead th,[data-theme=dark] .tabxplor-tab.tx-shape thead th,html.dark .tabxplor-tab.tx-shape thead th{color:#444444;}
#>   body.quarto-light .tabxplor-tab.tx-shape .tx-sec,[data-bs-theme=light] .tabxplor-tab.tx-shape .tx-sec,[data-theme=light] .tabxplor-tab.tx-shape .tx-sec,body.quarto-dark .tabxplor-tab.tx-shape .tx-sec,[data-bs-theme=dark] .tabxplor-tab.tx-shape .tx-sec,[data-theme=dark] .tabxplor-tab.tx-shape .tx-sec,html.dark .tabxplor-tab.tx-shape .tx-sec{color:#949494;}
#>   body.quarto-light .tabxplor-tab .tx-sec,[data-bs-theme=light] .tabxplor-tab .tx-sec,[data-theme=light] .tabxplor-tab .tx-sec,body.quarto-dark .tabxplor-tab .tx-sec,[data-bs-theme=dark] .tabxplor-tab .tx-sec,[data-theme=dark] .tabxplor-tab .tx-sec,html.dark .tabxplor-tab .tx-sec{color:#444444;font-style:normal;text-decoration:none;display:inline-block;}
#>   body.quarto-light .tabxplor-tab .tx-mark,[data-bs-theme=light] .tabxplor-tab .tx-mark,[data-theme=light] .tabxplor-tab .tx-mark,body.quarto-dark .tabxplor-tab .tx-mark,[data-bs-theme=dark] .tabxplor-tab .tx-mark,[data-theme=dark] .tabxplor-tab .tx-mark,html.dark .tabxplor-tab .tx-mark{color:#000000;font-style:normal;text-decoration:none;display:inline-block;}
#>   body.quarto-light .p1,body.quarto-light .tabxplor-tab .p1,[data-bs-theme=light] .p1,[data-bs-theme=light] .tabxplor-tab .p1,[data-theme=light] .p1,[data-theme=light] .tabxplor-tab .p1,body.quarto-dark .p1,body.quarto-dark .tabxplor-tab .p1,[data-bs-theme=dark] .p1,[data-bs-theme=dark] .tabxplor-tab .p1,[data-theme=dark] .p1,[data-theme=dark] .tabxplor-tab .p1,html.dark .p1,html.dark .tabxplor-tab .p1{color:#555555;font-weight:normal;text-decoration:underline;}
#>   body.quarto-light .p2,body.quarto-light .tabxplor-tab .p2,[data-bs-theme=light] .p2,[data-bs-theme=light] .tabxplor-tab .p2,[data-theme=light] .p2,[data-theme=light] .tabxplor-tab .p2,body.quarto-dark .p2,body.quarto-dark .tabxplor-tab .p2,[data-bs-theme=dark] .p2,[data-bs-theme=dark] .tabxplor-tab .p2,[data-theme=dark] .p2,[data-theme=dark] .tabxplor-tab .p2,html.dark .p2,html.dark .tabxplor-tab .p2{color:#000000;font-weight:normal;text-decoration:underline;}
#>   body.quarto-light .p3,body.quarto-light .tabxplor-tab .p3,[data-bs-theme=light] .p3,[data-bs-theme=light] .tabxplor-tab .p3,[data-theme=light] .p3,[data-theme=light] .tabxplor-tab .p3,body.quarto-dark .p3,body.quarto-dark .tabxplor-tab .p3,[data-bs-theme=dark] .p3,[data-bs-theme=dark] .tabxplor-tab .p3,[data-theme=dark] .p3,[data-theme=dark] .tabxplor-tab .p3,html.dark .p3,html.dark .tabxplor-tab .p3{color:#000000;text-decoration:underline;}
#>   body.quarto-light .p4,body.quarto-light .tabxplor-tab .p4,[data-bs-theme=light] .p4,[data-bs-theme=light] .tabxplor-tab .p4,[data-theme=light] .p4,[data-theme=light] .tabxplor-tab .p4,body.quarto-dark .p4,body.quarto-dark .tabxplor-tab .p4,[data-bs-theme=dark] .p4,[data-bs-theme=dark] .tabxplor-tab .p4,[data-theme=dark] .p4,[data-theme=dark] .tabxplor-tab .p4,html.dark .p4,html.dark .tabxplor-tab .p4{color:#000000;text-decoration:underline;}
#>   body.quarto-light .m1,body.quarto-light .tabxplor-tab .m1,[data-bs-theme=light] .m1,[data-bs-theme=light] .tabxplor-tab .m1,[data-theme=light] .m1,[data-theme=light] .tabxplor-tab .m1,body.quarto-dark .m1,body.quarto-dark .tabxplor-tab .m1,[data-bs-theme=dark] .m1,[data-bs-theme=dark] .tabxplor-tab .m1,[data-theme=dark] .m1,[data-theme=dark] .tabxplor-tab .m1,html.dark .m1,html.dark .tabxplor-tab .m1{color:#555555;font-weight:normal;font-style:italic;}
#>   body.quarto-light .m2,body.quarto-light .tabxplor-tab .m2,[data-bs-theme=light] .m2,[data-bs-theme=light] .tabxplor-tab .m2,[data-theme=light] .m2,[data-theme=light] .tabxplor-tab .m2,body.quarto-dark .m2,body.quarto-dark .tabxplor-tab .m2,[data-bs-theme=dark] .m2,[data-bs-theme=dark] .tabxplor-tab .m2,[data-theme=dark] .m2,[data-theme=dark] .tabxplor-tab .m2,html.dark .m2,html.dark .tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
#>   body.quarto-light .m3,body.quarto-light .tabxplor-tab .m3,[data-bs-theme=light] .m3,[data-bs-theme=light] .tabxplor-tab .m3,[data-theme=light] .m3,[data-theme=light] .tabxplor-tab .m3,body.quarto-dark .m3,body.quarto-dark .tabxplor-tab .m3,[data-bs-theme=dark] .m3,[data-bs-theme=dark] .tabxplor-tab .m3,[data-theme=dark] .m3,[data-theme=dark] .tabxplor-tab .m3,html.dark .m3,html.dark .tabxplor-tab .m3{color:#000000;font-style:italic;}
#>   body.quarto-light .m4,body.quarto-light .tabxplor-tab .m4,[data-bs-theme=light] .m4,[data-bs-theme=light] .tabxplor-tab .m4,[data-theme=light] .m4,[data-theme=light] .tabxplor-tab .m4,body.quarto-dark .m4,body.quarto-dark .tabxplor-tab .m4,[data-bs-theme=dark] .m4,[data-bs-theme=dark] .tabxplor-tab .m4,[data-theme=dark] .m4,[data-theme=dark] .tabxplor-tab .m4,html.dark .m4,html.dark .tabxplor-tab .m4{color:#000000;font-style:italic;}
#>   body.quarto-light .o1,body.quarto-light .tabxplor-tab .o1,[data-bs-theme=light] .o1,[data-bs-theme=light] .tabxplor-tab .o1,[data-theme=light] .o1,[data-theme=light] .tabxplor-tab .o1,body.quarto-dark .o1,body.quarto-dark .tabxplor-tab .o1,[data-bs-theme=dark] .o1,[data-bs-theme=dark] .tabxplor-tab .o1,[data-theme=dark] .o1,[data-theme=dark] .tabxplor-tab .o1,html.dark .o1,html.dark .tabxplor-tab .o1{background-color:#F5F5F5;}
#>   body.quarto-light .o2,body.quarto-light .tabxplor-tab .o2,[data-bs-theme=light] .o2,[data-bs-theme=light] .tabxplor-tab .o2,[data-theme=light] .o2,[data-theme=light] .tabxplor-tab .o2,body.quarto-dark .o2,body.quarto-dark .tabxplor-tab .o2,[data-bs-theme=dark] .o2,[data-bs-theme=dark] .tabxplor-tab .o2,[data-theme=dark] .o2,[data-theme=dark] .tabxplor-tab .o2,html.dark .o2,html.dark .tabxplor-tab .o2{background-color:#E4E4E4;}
#>   body.quarto-light .o3,body.quarto-light .tabxplor-tab .o3,[data-bs-theme=light] .o3,[data-bs-theme=light] .tabxplor-tab .o3,[data-theme=light] .o3,[data-theme=light] .tabxplor-tab .o3,body.quarto-dark .o3,body.quarto-dark .tabxplor-tab .o3,[data-bs-theme=dark] .o3,[data-bs-theme=dark] .tabxplor-tab .o3,[data-theme=dark] .o3,[data-theme=dark] .tabxplor-tab .o3,html.dark .o3,html.dark .tabxplor-tab .o3{background-color:#D0D0D0;}
#>   body.quarto-light .o4,body.quarto-light .tabxplor-tab .o4,[data-bs-theme=light] .o4,[data-bs-theme=light] .tabxplor-tab .o4,[data-theme=light] .o4,[data-theme=light] .tabxplor-tab .o4,body.quarto-dark .o4,body.quarto-dark .tabxplor-tab .o4,[data-bs-theme=dark] .o4,[data-bs-theme=dark] .tabxplor-tab .o4,[data-theme=dark] .o4,[data-theme=dark] .tabxplor-tab .o4,html.dark .o4,html.dark .tabxplor-tab .o4{background-color:#B8B8B8;}
#>   body.quarto-light .u1,body.quarto-light .tabxplor-tab .u1,[data-bs-theme=light] .u1,[data-bs-theme=light] .tabxplor-tab .u1,[data-theme=light] .u1,[data-theme=light] .tabxplor-tab .u1,body.quarto-dark .u1,body.quarto-dark .tabxplor-tab .u1,[data-bs-theme=dark] .u1,[data-bs-theme=dark] .tabxplor-tab .u1,[data-theme=dark] .u1,[data-theme=dark] .tabxplor-tab .u1,html.dark .u1,html.dark .tabxplor-tab .u1{background-color:#F5F5F5;}
#>   body.quarto-light .u2,body.quarto-light .tabxplor-tab .u2,[data-bs-theme=light] .u2,[data-bs-theme=light] .tabxplor-tab .u2,[data-theme=light] .u2,[data-theme=light] .tabxplor-tab .u2,body.quarto-dark .u2,body.quarto-dark .tabxplor-tab .u2,[data-bs-theme=dark] .u2,[data-bs-theme=dark] .tabxplor-tab .u2,[data-theme=dark] .u2,[data-theme=dark] .tabxplor-tab .u2,html.dark .u2,html.dark .tabxplor-tab .u2{background-color:#E4E4E4;}
#>   body.quarto-light .u3,body.quarto-light .tabxplor-tab .u3,[data-bs-theme=light] .u3,[data-bs-theme=light] .tabxplor-tab .u3,[data-theme=light] .u3,[data-theme=light] .tabxplor-tab .u3,body.quarto-dark .u3,body.quarto-dark .tabxplor-tab .u3,[data-bs-theme=dark] .u3,[data-bs-theme=dark] .tabxplor-tab .u3,[data-theme=dark] .u3,[data-theme=dark] .tabxplor-tab .u3,html.dark .u3,html.dark .tabxplor-tab .u3{background-color:#D0D0D0;}
#>   body.quarto-light .u4,body.quarto-light .tabxplor-tab .u4,[data-bs-theme=light] .u4,[data-bs-theme=light] .tabxplor-tab .u4,[data-theme=light] .u4,[data-theme=light] .tabxplor-tab .u4,body.quarto-dark .u4,body.quarto-dark .tabxplor-tab .u4,[data-bs-theme=dark] .u4,[data-bs-theme=dark] .tabxplor-tab .u4,[data-theme=dark] .u4,[data-theme=dark] .tabxplor-tab .u4,html.dark .u4,html.dark .tabxplor-tab .u4{background-color:#B8B8B8;}
#> }
#> </style>
cat(tab_css(format = "md", style_tag = FALSE))  # the markdown flavour
#> .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
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
```
