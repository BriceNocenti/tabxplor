# Transpose a cross-table (swap its rows and columns)

**\[deprecated\]**

`tab_transpose()` is **soft-deprecated** since tabxplor 1.4.0. It flips
the *object* (the `tabxplor_fmt` fields), which cannot carry a
transposed column's mixed cell types, so a table with several row
variables or numeric columns transposes incorrectly (numeric cells
mis-coloured, duplicated total columns). Use the exporters'
`transpose = TRUE` argument instead — it flips the finished render model
after colours are computed, and handles several row variables and
numeric columns:

    tab(data, row_vars, col_vars, pct = "row") |> tab_kable(transpose = TRUE)   # or tab_md() / tab_xl()

The function is kept (unchanged) for the single-row-variable round-trip
it always supported.

## Usage

``` r
tab_transpose(tabs, name = NULL)
```

## Arguments

- tabs:

  A single table made with
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  (one row variable, one column variable; not a subtabled table with
  `tab_vars`, and at most one total row and one total column).

- name:

  The name to give the new first (label) column, holding the old
  column-variable levels. `NULL` (default) uses the old column-variable
  name.

## Value

A transposed `tabxplor_tab`.

## Examples

``` r
# \donttest{
# build marital x race as row percentages, then display it as race x marital:
tab(forcats::gss_cat, marital, race, pct = "row") |>
  tab_kable(transpose = TRUE)
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
#> <table class="tabxplor-tab"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="6">marital</th><th class="tx-span" colspan="1"></th></tr><tr><th class="tx-l tx-bl tx-rv">race</th><th class="tx-r tx-num">No answer</th><th class="tx-r tx-num">Never married</th><th class="tx-r tx-num">Separated</th><th class="tx-r tx-num">Divorced</th><th class="tx-r tx-num">Widowed</th><th class="tx-r tx-num tx-br">Married</th><th class="tx-r tx-num tx-br tx-bl tx-tot">Total</th></tr></thead><tbody><tr><td class="tx-l tx-bl tx-rv">Other</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.3 ; n: 2">12%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1.3 ; n: 633">12%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +6% ; ratio: ×1.6 ; n: 110">15%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.5 ; n: 212">6%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷2.4 ; n: 70">4%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 932">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 959">9%</td></tr>
#> <tr><td class="tx-l tx-bl tx-rv">Black</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.2 ; n: 2">12%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +10% ; ratio: ×1.7 ; n: 1 305">24%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +12% ; ratio: ×1.8 ; n: 196">26%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 495">15%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×1 ; n: 262">14%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.7 ; n: 869">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 3 129">15%</td></tr>
#> <tr><td class="tx-l tx-bl tx-rv">White</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 13">76%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -12% ; ratio: ÷1.2 ; n: 3 478">64%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -18% ; ratio: ÷1.3 ; n: 437">59%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1 ; n: 2 676">79%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×1.1 ; n: 1 475">82%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +6% ; ratio: ×1.1 ; n: 8 316">82%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 16 395">76%</td></tr>
#> <tr class="tx-b tx-bt"><td class="tx-l tx-bl tx-rv">Total</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 17">100%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 5 416">100%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 743">100%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 3 383">100%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 1 807">100%</td><td class="tx-r tx-num tx-br tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 10 117">100%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 21 483">100%</td></tr>
#> <tr class="tx-bb"><td class="tx-l tx-bl tx-rv">n</td><td class="tx-r tx-num g2">17</td><td class="tx-r tx-num g2">5 416</td><td class="tx-r tx-num g2">743</td><td class="tx-r tx-num g2">3 383</td><td class="tx-r tx-num g2">1 807</td><td class="tx-r tx-num tx-br g2">10 117</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">21 483</td></tr></tbody></table>
# }
```
