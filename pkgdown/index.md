
<!-- pkgdown/index.md is generated from pkgdown/index.Rmd. Please edit that file.
     This page IS README.Rmd in colour -- same prose, same examples, the tables rendered in the
     real palette instead of the black-and-white one GitHub forces. Edit both, or neither. -->

# tabxplor

`tabxplor` makes cross-tables readable at a glance, for data
exploration. One line of code builds a table with percentages, weighted
counts, confidence intervals and tests — and **colors highlight the
cells that stand out from the total, only when the difference is
statistically solid**. You spot the structure of your data immediately,
instead of scanning numbers row by row.

- **Colors encode effect size *and* significance** at once: the stronger
  the difference, the deeper the color; non-significant cells stay
  uncolored (or greyed). A black-and-white `theme = "print_ready"`
  renders the same reading for journals.
- **Cells are rich values**: each one carries its count, percentage,
  confidence interval and reference behind the displayed number — tables
  are `tibble`s you can keep working on with `dplyr`.
- **The same colors follow you everywhere**: console, html, Excel,
  markdown/Quarto and plots.
- **Regression tables too**: `tab_reg()` presents logistic and other
  models with the same visual language, next to the observed
  percentages.
- Weighted and survey data are supported throughout, and a
  point-and-click [jamovi](https://www.jamovi.org/) module is available.

<style>
.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
.tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
.tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;width:0;min-width:100%;}
.tabxplor-tab tfoot{font-size:80%;text-align:left;}
.tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
.tabxplor-tab th,.tabxplor-tab td{border-width:0;}
.tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
.tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
.tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
.tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
.tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
.tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
.tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
.tabxplor-tab p{font-size:80%;}
.tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
.tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
.tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
.tabxplor-tab .tx-r{text-align:right;}
.tabxplor-tab .tx-l{text-align:left;}
.tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
.tabxplor-tab thead .tx-unit{font-weight:normal;font-style:italic;font-size:80%;text-align:left;border-top-width:0;padding-top:0;}
.tabxplor-tab thead tr:has(+ tr > .tx-unit) > th:not([rowspan]){border-bottom-width:0;}
.tabxplor-tab .tx-num{white-space:nowrap;}
.tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
.tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
.tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
.tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
.tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
.tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
.tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
.tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
.tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
.tabxplor-tab tr.tx-bb>.tx-nb,.tabxplor-tab tr.tx-bb2>.tx-nb{border-bottom-style:none;}
.tabxplor-tab td.tx-bb2{border-bottom-style:solid;border-bottom-width:2px;}
.tabxplor-tab tr.tx-bt2>*{border-top-style:solid;border-top-width:2px;}
.tabxplor-tab .tx-foot{width:0;min-width:100%;}
.tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;margin:0 -4px;}
.tabxplor-tab .tx-spark{display:block;margin:0 auto;}
.tabxplor-tab .tx-sparkcell{vertical-align:middle;text-align:center;padding:1px 2px;}
.tooltip-inner{max-width:none;white-space:pre;}
.popover{max-width:none;}
.popover-body,.popover-content{padding:6px;white-space:pre;}
.tabxplor-tab{color:#000000;background:#ffffff;}
.tabxplor-tab th,.tabxplor-tab td{background-color:transparent;border-color:#000000;}
.tabxplor-tab tbody tr:hover{background:#FFFCE5;}
.g1,.tabxplor-tab .g1{color:#949494;}
.g2,.tabxplor-tab .g2{color:#444444;}
.tabxplor-tab .tx-unit{color:#949494;}
.tabxplor-caption{color:#000000;}
.tabxplor-tab .tx-sec{color:#444444;}
.p1,.tabxplor-tab .p1{color:#02A5B3;}
.p2,.tabxplor-tab .p2{color:#0891C9;}
.p3,.tabxplor-tab .p3{color:#0267C7;}
.p4,.tabxplor-tab .p4{color:#300DFD;}
.m1,.tabxplor-tab .m1{color:#DCA331;}
.m2,.tabxplor-tab .m2{color:#DE7C01;}
.m3,.tabxplor-tab .m3{color:#DD5301;}
.m4,.tabxplor-tab .m4{color:#D60103;}
.o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
.o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
.o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
.o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
.u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
.u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
.u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
.u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
@media (prefers-color-scheme: dark) {
  .tabxplor-tab{color:#f0efe5;background:#222222;}
  .tabxplor-tab th,.tabxplor-tab td{background-color:transparent;border-color:#CECDC3;}
  .tabxplor-tab tbody tr:hover{background:rgba(255,242,204,.10);}
  .g1,.tabxplor-tab .g1{color:#707070;}
  .g2,.tabxplor-tab .g2{color:#CECDC3;}
  .tabxplor-tab .tx-unit{color:#707070;}
  .tabxplor-caption{color:#FFFFFF;}
  .tabxplor-tab .tx-sec{color:#CECDC3;}
  .p1,.tabxplor-tab .p1{color:#028282;}
  .p2,.tabxplor-tab .p2{color:#0286B1;}
  .p3,.tabxplor-tab .p3{color:#4687D8;}
  .p4,.tabxplor-tab .p4{color:#6987FF;}
  .m1,.tabxplor-tab .m1{color:#867002;}
  .m2,.tabxplor-tab .m2{color:#B87501;}
  .m3,.tabxplor-tab .m3{color:#EC6F02;}
  .m4,.tabxplor-tab .m4{color:#FF626B;}
  .o1,.tabxplor-tab .o1{background-color:#002828;}
  .o2,.tabxplor-tab .o2{background-color:#012D3F;}
  .o3,.tabxplor-tab .o3{background-color:#122E5D;}
  .o4,.tabxplor-tab .o4{background-color:#202E7A;}
  .u1,.tabxplor-tab .u1{background-color:#292100;}
  .u2,.tabxplor-tab .u2{background-color:#3B2300;}
  .u3,.tabxplor-tab .u3{background-color:#4F2100;}
  .u4,.tabxplor-tab .u4{background-color:#720119;}
}
body.quarto-light .tabxplor-tab,[data-bs-theme=light] .tabxplor-tab,[data-theme=light] .tabxplor-tab{color:#000000;background:#ffffff;}
body.quarto-light .tabxplor-tab th,body.quarto-light .tabxplor-tab td,[data-bs-theme=light] .tabxplor-tab th,[data-bs-theme=light] .tabxplor-tab td,[data-theme=light] .tabxplor-tab th,[data-theme=light] .tabxplor-tab td{background-color:transparent;border-color:#000000;}
body.quarto-light .tabxplor-tab tbody tr:hover,[data-bs-theme=light] .tabxplor-tab tbody tr:hover,[data-theme=light] .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
body.quarto-light .g1,body.quarto-light .tabxplor-tab .g1,[data-bs-theme=light] .g1,[data-bs-theme=light] .tabxplor-tab .g1,[data-theme=light] .g1,[data-theme=light] .tabxplor-tab .g1{color:#949494;}
body.quarto-light .g2,body.quarto-light .tabxplor-tab .g2,[data-bs-theme=light] .g2,[data-bs-theme=light] .tabxplor-tab .g2,[data-theme=light] .g2,[data-theme=light] .tabxplor-tab .g2{color:#444444;}
body.quarto-light .tabxplor-tab .tx-unit,[data-bs-theme=light] .tabxplor-tab .tx-unit,[data-theme=light] .tabxplor-tab .tx-unit{color:#949494;}
body.quarto-light .tabxplor-caption,[data-bs-theme=light] .tabxplor-caption,[data-theme=light] .tabxplor-caption{color:#000000;}
body.quarto-light .tabxplor-tab .tx-sec,[data-bs-theme=light] .tabxplor-tab .tx-sec,[data-theme=light] .tabxplor-tab .tx-sec{color:#444444;}
body.quarto-light .p1,body.quarto-light .tabxplor-tab .p1,[data-bs-theme=light] .p1,[data-bs-theme=light] .tabxplor-tab .p1,[data-theme=light] .p1,[data-theme=light] .tabxplor-tab .p1{color:#02A5B3;}
body.quarto-light .p2,body.quarto-light .tabxplor-tab .p2,[data-bs-theme=light] .p2,[data-bs-theme=light] .tabxplor-tab .p2,[data-theme=light] .p2,[data-theme=light] .tabxplor-tab .p2{color:#0891C9;}
body.quarto-light .p3,body.quarto-light .tabxplor-tab .p3,[data-bs-theme=light] .p3,[data-bs-theme=light] .tabxplor-tab .p3,[data-theme=light] .p3,[data-theme=light] .tabxplor-tab .p3{color:#0267C7;}
body.quarto-light .p4,body.quarto-light .tabxplor-tab .p4,[data-bs-theme=light] .p4,[data-bs-theme=light] .tabxplor-tab .p4,[data-theme=light] .p4,[data-theme=light] .tabxplor-tab .p4{color:#300DFD;}
body.quarto-light .m1,body.quarto-light .tabxplor-tab .m1,[data-bs-theme=light] .m1,[data-bs-theme=light] .tabxplor-tab .m1,[data-theme=light] .m1,[data-theme=light] .tabxplor-tab .m1{color:#DCA331;}
body.quarto-light .m2,body.quarto-light .tabxplor-tab .m2,[data-bs-theme=light] .m2,[data-bs-theme=light] .tabxplor-tab .m2,[data-theme=light] .m2,[data-theme=light] .tabxplor-tab .m2{color:#DE7C01;}
body.quarto-light .m3,body.quarto-light .tabxplor-tab .m3,[data-bs-theme=light] .m3,[data-bs-theme=light] .tabxplor-tab .m3,[data-theme=light] .m3,[data-theme=light] .tabxplor-tab .m3{color:#DD5301;}
body.quarto-light .m4,body.quarto-light .tabxplor-tab .m4,[data-bs-theme=light] .m4,[data-bs-theme=light] .tabxplor-tab .m4,[data-theme=light] .m4,[data-theme=light] .tabxplor-tab .m4{color:#D60103;}
body.quarto-light .o1,body.quarto-light .tabxplor-tab .o1,[data-bs-theme=light] .o1,[data-bs-theme=light] .tabxplor-tab .o1,[data-theme=light] .o1,[data-theme=light] .tabxplor-tab .o1{background-color:#DFFCFF;}
body.quarto-light .o2,body.quarto-light .tabxplor-tab .o2,[data-bs-theme=light] .o2,[data-bs-theme=light] .tabxplor-tab .o2,[data-theme=light] .o2,[data-theme=light] .tabxplor-tab .o2{background-color:#D7EFFF;}
body.quarto-light .o3,body.quarto-light .tabxplor-tab .o3,[data-bs-theme=light] .o3,[data-bs-theme=light] .tabxplor-tab .o3,[data-theme=light] .o3,[data-theme=light] .tabxplor-tab .o3{background-color:#CEE3FF;}
body.quarto-light .o4,body.quarto-light .tabxplor-tab .o4,[data-bs-theme=light] .o4,[data-bs-theme=light] .tabxplor-tab .o4,[data-theme=light] .o4,[data-theme=light] .tabxplor-tab .o4{background-color:#BBCCFF;}
body.quarto-light .u1,body.quarto-light .tabxplor-tab .u1,[data-bs-theme=light] .u1,[data-bs-theme=light] .tabxplor-tab .u1,[data-theme=light] .u1,[data-theme=light] .tabxplor-tab .u1{background-color:#FFF4E1;}
body.quarto-light .u2,body.quarto-light .tabxplor-tab .u2,[data-bs-theme=light] .u2,[data-bs-theme=light] .tabxplor-tab .u2,[data-theme=light] .u2,[data-theme=light] .tabxplor-tab .u2{background-color:#FFE6D3;}
body.quarto-light .u3,body.quarto-light .tabxplor-tab .u3,[data-bs-theme=light] .u3,[data-bs-theme=light] .tabxplor-tab .u3,[data-theme=light] .u3,[data-theme=light] .tabxplor-tab .u3{background-color:#FFD7C8;}
body.quarto-light .u4,body.quarto-light .tabxplor-tab .u4,[data-bs-theme=light] .u4,[data-bs-theme=light] .tabxplor-tab .u4,[data-theme=light] .u4,[data-theme=light] .tabxplor-tab .u4{background-color:#FFBAAF;}
body.quarto-dark .tabxplor-tab,[data-bs-theme=dark] .tabxplor-tab,[data-theme=dark] .tabxplor-tab,html.dark .tabxplor-tab{color:#f0efe5;background:#222222;}
body.quarto-dark .tabxplor-tab th,body.quarto-dark .tabxplor-tab td,[data-bs-theme=dark] .tabxplor-tab th,[data-bs-theme=dark] .tabxplor-tab td,[data-theme=dark] .tabxplor-tab th,[data-theme=dark] .tabxplor-tab td,html.dark .tabxplor-tab th,html.dark .tabxplor-tab td{background-color:transparent;border-color:#CECDC3;}
body.quarto-dark .tabxplor-tab tbody tr:hover,[data-bs-theme=dark] .tabxplor-tab tbody tr:hover,[data-theme=dark] .tabxplor-tab tbody tr:hover,html.dark .tabxplor-tab tbody tr:hover{background:rgba(255,242,204,.10);}
body.quarto-dark .g1,body.quarto-dark .tabxplor-tab .g1,[data-bs-theme=dark] .g1,[data-bs-theme=dark] .tabxplor-tab .g1,[data-theme=dark] .g1,[data-theme=dark] .tabxplor-tab .g1,html.dark .g1,html.dark .tabxplor-tab .g1{color:#707070;}
body.quarto-dark .g2,body.quarto-dark .tabxplor-tab .g2,[data-bs-theme=dark] .g2,[data-bs-theme=dark] .tabxplor-tab .g2,[data-theme=dark] .g2,[data-theme=dark] .tabxplor-tab .g2,html.dark .g2,html.dark .tabxplor-tab .g2{color:#CECDC3;}
body.quarto-dark .tabxplor-tab .tx-unit,[data-bs-theme=dark] .tabxplor-tab .tx-unit,[data-theme=dark] .tabxplor-tab .tx-unit,html.dark .tabxplor-tab .tx-unit{color:#707070;}
body.quarto-dark .tabxplor-caption,[data-bs-theme=dark] .tabxplor-caption,[data-theme=dark] .tabxplor-caption,html.dark .tabxplor-caption{color:#FFFFFF;}
body.quarto-dark .tabxplor-tab .tx-sec,[data-bs-theme=dark] .tabxplor-tab .tx-sec,[data-theme=dark] .tabxplor-tab .tx-sec,html.dark .tabxplor-tab .tx-sec{color:#CECDC3;}
body.quarto-dark .p1,body.quarto-dark .tabxplor-tab .p1,[data-bs-theme=dark] .p1,[data-bs-theme=dark] .tabxplor-tab .p1,[data-theme=dark] .p1,[data-theme=dark] .tabxplor-tab .p1,html.dark .p1,html.dark .tabxplor-tab .p1{color:#028282;}
body.quarto-dark .p2,body.quarto-dark .tabxplor-tab .p2,[data-bs-theme=dark] .p2,[data-bs-theme=dark] .tabxplor-tab .p2,[data-theme=dark] .p2,[data-theme=dark] .tabxplor-tab .p2,html.dark .p2,html.dark .tabxplor-tab .p2{color:#0286B1;}
body.quarto-dark .p3,body.quarto-dark .tabxplor-tab .p3,[data-bs-theme=dark] .p3,[data-bs-theme=dark] .tabxplor-tab .p3,[data-theme=dark] .p3,[data-theme=dark] .tabxplor-tab .p3,html.dark .p3,html.dark .tabxplor-tab .p3{color:#4687D8;}
body.quarto-dark .p4,body.quarto-dark .tabxplor-tab .p4,[data-bs-theme=dark] .p4,[data-bs-theme=dark] .tabxplor-tab .p4,[data-theme=dark] .p4,[data-theme=dark] .tabxplor-tab .p4,html.dark .p4,html.dark .tabxplor-tab .p4{color:#6987FF;}
body.quarto-dark .m1,body.quarto-dark .tabxplor-tab .m1,[data-bs-theme=dark] .m1,[data-bs-theme=dark] .tabxplor-tab .m1,[data-theme=dark] .m1,[data-theme=dark] .tabxplor-tab .m1,html.dark .m1,html.dark .tabxplor-tab .m1{color:#867002;}
body.quarto-dark .m2,body.quarto-dark .tabxplor-tab .m2,[data-bs-theme=dark] .m2,[data-bs-theme=dark] .tabxplor-tab .m2,[data-theme=dark] .m2,[data-theme=dark] .tabxplor-tab .m2,html.dark .m2,html.dark .tabxplor-tab .m2{color:#B87501;}
body.quarto-dark .m3,body.quarto-dark .tabxplor-tab .m3,[data-bs-theme=dark] .m3,[data-bs-theme=dark] .tabxplor-tab .m3,[data-theme=dark] .m3,[data-theme=dark] .tabxplor-tab .m3,html.dark .m3,html.dark .tabxplor-tab .m3{color:#EC6F02;}
body.quarto-dark .m4,body.quarto-dark .tabxplor-tab .m4,[data-bs-theme=dark] .m4,[data-bs-theme=dark] .tabxplor-tab .m4,[data-theme=dark] .m4,[data-theme=dark] .tabxplor-tab .m4,html.dark .m4,html.dark .tabxplor-tab .m4{color:#FF626B;}
body.quarto-dark .o1,body.quarto-dark .tabxplor-tab .o1,[data-bs-theme=dark] .o1,[data-bs-theme=dark] .tabxplor-tab .o1,[data-theme=dark] .o1,[data-theme=dark] .tabxplor-tab .o1,html.dark .o1,html.dark .tabxplor-tab .o1{background-color:#002828;}
body.quarto-dark .o2,body.quarto-dark .tabxplor-tab .o2,[data-bs-theme=dark] .o2,[data-bs-theme=dark] .tabxplor-tab .o2,[data-theme=dark] .o2,[data-theme=dark] .tabxplor-tab .o2,html.dark .o2,html.dark .tabxplor-tab .o2{background-color:#012D3F;}
body.quarto-dark .o3,body.quarto-dark .tabxplor-tab .o3,[data-bs-theme=dark] .o3,[data-bs-theme=dark] .tabxplor-tab .o3,[data-theme=dark] .o3,[data-theme=dark] .tabxplor-tab .o3,html.dark .o3,html.dark .tabxplor-tab .o3{background-color:#122E5D;}
body.quarto-dark .o4,body.quarto-dark .tabxplor-tab .o4,[data-bs-theme=dark] .o4,[data-bs-theme=dark] .tabxplor-tab .o4,[data-theme=dark] .o4,[data-theme=dark] .tabxplor-tab .o4,html.dark .o4,html.dark .tabxplor-tab .o4{background-color:#202E7A;}
body.quarto-dark .u1,body.quarto-dark .tabxplor-tab .u1,[data-bs-theme=dark] .u1,[data-bs-theme=dark] .tabxplor-tab .u1,[data-theme=dark] .u1,[data-theme=dark] .tabxplor-tab .u1,html.dark .u1,html.dark .tabxplor-tab .u1{background-color:#292100;}
body.quarto-dark .u2,body.quarto-dark .tabxplor-tab .u2,[data-bs-theme=dark] .u2,[data-bs-theme=dark] .tabxplor-tab .u2,[data-theme=dark] .u2,[data-theme=dark] .tabxplor-tab .u2,html.dark .u2,html.dark .tabxplor-tab .u2{background-color:#3B2300;}
body.quarto-dark .u3,body.quarto-dark .tabxplor-tab .u3,[data-bs-theme=dark] .u3,[data-bs-theme=dark] .tabxplor-tab .u3,[data-theme=dark] .u3,[data-theme=dark] .tabxplor-tab .u3,html.dark .u3,html.dark .tabxplor-tab .u3{background-color:#4F2100;}
body.quarto-dark .u4,body.quarto-dark .tabxplor-tab .u4,[data-bs-theme=dark] .u4,[data-bs-theme=dark] .tabxplor-tab .u4,[data-theme=dark] .u4,[data-theme=dark] .tabxplor-tab .u4,html.dark .u4,html.dark .tabxplor-tab .u4{background-color:#720119;}
@media print {
  .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
  .tabxplor-tab{color:#000000;background:#ffffff;}
  .tabxplor-tab th,.tabxplor-tab td{color:#000000;background-color:#ffffff;border-color:#000000;}
  .tabxplor-tab tbody tr:hover{background:transparent;}
  .g1,.tabxplor-tab .g1{color:#949494;}
  .g2,.tabxplor-tab .g2{color:#444444;}
  .tabxplor-tab .tx-unit{color:#949494;}
  .tabxplor-caption{color:#000000;}
  .tabxplor-tab .tx-sec{color:#444444;font-style:normal;text-decoration:none;display:inline-block;}
  .tabxplor-tab .tx-mark{color:#000000;font-style:normal;text-decoration:none;display:inline-block;}
  .p1,.tabxplor-tab .p1{color:#555555;font-weight:normal;text-decoration:underline;}
  .p2,.tabxplor-tab .p2{color:#000000;font-weight:normal;text-decoration:underline;}
  .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
  .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
  .m1,.tabxplor-tab .m1{color:#555555;font-weight:normal;font-style:italic;}
  .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
  .m3,.tabxplor-tab .m3{color:#000000;font-style:italic;}
  .m4,.tabxplor-tab .m4{color:#000000;font-style:italic;}
  .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
  .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
  .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
  .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
  .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
  .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
  .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
  .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
  body.quarto-light .tabxplor-tab,[data-bs-theme=light] .tabxplor-tab,[data-theme=light] .tabxplor-tab,body.quarto-dark .tabxplor-tab,[data-bs-theme=dark] .tabxplor-tab,[data-theme=dark] .tabxplor-tab,html.dark .tabxplor-tab{color:#000000;background:#ffffff;}
  body.quarto-light .tabxplor-tab th,body.quarto-light .tabxplor-tab td,[data-bs-theme=light] .tabxplor-tab th,[data-bs-theme=light] .tabxplor-tab td,[data-theme=light] .tabxplor-tab th,[data-theme=light] .tabxplor-tab td,body.quarto-dark .tabxplor-tab th,body.quarto-dark .tabxplor-tab td,[data-bs-theme=dark] .tabxplor-tab th,[data-bs-theme=dark] .tabxplor-tab td,[data-theme=dark] .tabxplor-tab th,[data-theme=dark] .tabxplor-tab td,html.dark .tabxplor-tab th,html.dark .tabxplor-tab td{color:#000000;background-color:#ffffff;border-color:#000000;}
  body.quarto-light .tabxplor-tab tbody tr:hover,[data-bs-theme=light] .tabxplor-tab tbody tr:hover,[data-theme=light] .tabxplor-tab tbody tr:hover,body.quarto-dark .tabxplor-tab tbody tr:hover,[data-bs-theme=dark] .tabxplor-tab tbody tr:hover,[data-theme=dark] .tabxplor-tab tbody tr:hover,html.dark .tabxplor-tab tbody tr:hover{background:transparent;}
  body.quarto-light .g1,body.quarto-light .tabxplor-tab .g1,[data-bs-theme=light] .g1,[data-bs-theme=light] .tabxplor-tab .g1,[data-theme=light] .g1,[data-theme=light] .tabxplor-tab .g1,body.quarto-dark .g1,body.quarto-dark .tabxplor-tab .g1,[data-bs-theme=dark] .g1,[data-bs-theme=dark] .tabxplor-tab .g1,[data-theme=dark] .g1,[data-theme=dark] .tabxplor-tab .g1,html.dark .g1,html.dark .tabxplor-tab .g1{color:#949494;}
  body.quarto-light .g2,body.quarto-light .tabxplor-tab .g2,[data-bs-theme=light] .g2,[data-bs-theme=light] .tabxplor-tab .g2,[data-theme=light] .g2,[data-theme=light] .tabxplor-tab .g2,body.quarto-dark .g2,body.quarto-dark .tabxplor-tab .g2,[data-bs-theme=dark] .g2,[data-bs-theme=dark] .tabxplor-tab .g2,[data-theme=dark] .g2,[data-theme=dark] .tabxplor-tab .g2,html.dark .g2,html.dark .tabxplor-tab .g2{color:#444444;}
  body.quarto-light .tabxplor-tab .tx-unit,[data-bs-theme=light] .tabxplor-tab .tx-unit,[data-theme=light] .tabxplor-tab .tx-unit,body.quarto-dark .tabxplor-tab .tx-unit,[data-bs-theme=dark] .tabxplor-tab .tx-unit,[data-theme=dark] .tabxplor-tab .tx-unit,html.dark .tabxplor-tab .tx-unit{color:#949494;}
  body.quarto-light .tabxplor-caption,[data-bs-theme=light] .tabxplor-caption,[data-theme=light] .tabxplor-caption,body.quarto-dark .tabxplor-caption,[data-bs-theme=dark] .tabxplor-caption,[data-theme=dark] .tabxplor-caption,html.dark .tabxplor-caption{color:#000000;}
  body.quarto-light .tabxplor-tab .tx-sec,[data-bs-theme=light] .tabxplor-tab .tx-sec,[data-theme=light] .tabxplor-tab .tx-sec,body.quarto-dark .tabxplor-tab .tx-sec,[data-bs-theme=dark] .tabxplor-tab .tx-sec,[data-theme=dark] .tabxplor-tab .tx-sec,html.dark .tabxplor-tab .tx-sec{color:#444444;font-style:normal;text-decoration:none;display:inline-block;}
  body.quarto-light .tabxplor-tab .tx-mark,[data-bs-theme=light] .tabxplor-tab .tx-mark,[data-theme=light] .tabxplor-tab .tx-mark,body.quarto-dark .tabxplor-tab .tx-mark,[data-bs-theme=dark] .tabxplor-tab .tx-mark,[data-theme=dark] .tabxplor-tab .tx-mark,html.dark .tabxplor-tab .tx-mark{color:#000000;font-style:normal;text-decoration:none;display:inline-block;}
  body.quarto-light .p1,body.quarto-light .tabxplor-tab .p1,[data-bs-theme=light] .p1,[data-bs-theme=light] .tabxplor-tab .p1,[data-theme=light] .p1,[data-theme=light] .tabxplor-tab .p1,body.quarto-dark .p1,body.quarto-dark .tabxplor-tab .p1,[data-bs-theme=dark] .p1,[data-bs-theme=dark] .tabxplor-tab .p1,[data-theme=dark] .p1,[data-theme=dark] .tabxplor-tab .p1,html.dark .p1,html.dark .tabxplor-tab .p1{color:#555555;font-weight:normal;text-decoration:underline;}
  body.quarto-light .p2,body.quarto-light .tabxplor-tab .p2,[data-bs-theme=light] .p2,[data-bs-theme=light] .tabxplor-tab .p2,[data-theme=light] .p2,[data-theme=light] .tabxplor-tab .p2,body.quarto-dark .p2,body.quarto-dark .tabxplor-tab .p2,[data-bs-theme=dark] .p2,[data-bs-theme=dark] .tabxplor-tab .p2,[data-theme=dark] .p2,[data-theme=dark] .tabxplor-tab .p2,html.dark .p2,html.dark .tabxplor-tab .p2{color:#000000;font-weight:normal;text-decoration:underline;}
  body.quarto-light .p3,body.quarto-light .tabxplor-tab .p3,[data-bs-theme=light] .p3,[data-bs-theme=light] .tabxplor-tab .p3,[data-theme=light] .p3,[data-theme=light] .tabxplor-tab .p3,body.quarto-dark .p3,body.quarto-dark .tabxplor-tab .p3,[data-bs-theme=dark] .p3,[data-bs-theme=dark] .tabxplor-tab .p3,[data-theme=dark] .p3,[data-theme=dark] .tabxplor-tab .p3,html.dark .p3,html.dark .tabxplor-tab .p3{color:#000000;text-decoration:underline;}
  body.quarto-light .p4,body.quarto-light .tabxplor-tab .p4,[data-bs-theme=light] .p4,[data-bs-theme=light] .tabxplor-tab .p4,[data-theme=light] .p4,[data-theme=light] .tabxplor-tab .p4,body.quarto-dark .p4,body.quarto-dark .tabxplor-tab .p4,[data-bs-theme=dark] .p4,[data-bs-theme=dark] .tabxplor-tab .p4,[data-theme=dark] .p4,[data-theme=dark] .tabxplor-tab .p4,html.dark .p4,html.dark .tabxplor-tab .p4{color:#000000;text-decoration:underline;}
  body.quarto-light .m1,body.quarto-light .tabxplor-tab .m1,[data-bs-theme=light] .m1,[data-bs-theme=light] .tabxplor-tab .m1,[data-theme=light] .m1,[data-theme=light] .tabxplor-tab .m1,body.quarto-dark .m1,body.quarto-dark .tabxplor-tab .m1,[data-bs-theme=dark] .m1,[data-bs-theme=dark] .tabxplor-tab .m1,[data-theme=dark] .m1,[data-theme=dark] .tabxplor-tab .m1,html.dark .m1,html.dark .tabxplor-tab .m1{color:#555555;font-weight:normal;font-style:italic;}
  body.quarto-light .m2,body.quarto-light .tabxplor-tab .m2,[data-bs-theme=light] .m2,[data-bs-theme=light] .tabxplor-tab .m2,[data-theme=light] .m2,[data-theme=light] .tabxplor-tab .m2,body.quarto-dark .m2,body.quarto-dark .tabxplor-tab .m2,[data-bs-theme=dark] .m2,[data-bs-theme=dark] .tabxplor-tab .m2,[data-theme=dark] .m2,[data-theme=dark] .tabxplor-tab .m2,html.dark .m2,html.dark .tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
  body.quarto-light .m3,body.quarto-light .tabxplor-tab .m3,[data-bs-theme=light] .m3,[data-bs-theme=light] .tabxplor-tab .m3,[data-theme=light] .m3,[data-theme=light] .tabxplor-tab .m3,body.quarto-dark .m3,body.quarto-dark .tabxplor-tab .m3,[data-bs-theme=dark] .m3,[data-bs-theme=dark] .tabxplor-tab .m3,[data-theme=dark] .m3,[data-theme=dark] .tabxplor-tab .m3,html.dark .m3,html.dark .tabxplor-tab .m3{color:#000000;font-style:italic;}
  body.quarto-light .m4,body.quarto-light .tabxplor-tab .m4,[data-bs-theme=light] .m4,[data-bs-theme=light] .tabxplor-tab .m4,[data-theme=light] .m4,[data-theme=light] .tabxplor-tab .m4,body.quarto-dark .m4,body.quarto-dark .tabxplor-tab .m4,[data-bs-theme=dark] .m4,[data-bs-theme=dark] .tabxplor-tab .m4,[data-theme=dark] .m4,[data-theme=dark] .tabxplor-tab .m4,html.dark .m4,html.dark .tabxplor-tab .m4{color:#000000;font-style:italic;}
  body.quarto-light .o1,body.quarto-light .tabxplor-tab .o1,[data-bs-theme=light] .o1,[data-bs-theme=light] .tabxplor-tab .o1,[data-theme=light] .o1,[data-theme=light] .tabxplor-tab .o1,body.quarto-dark .o1,body.quarto-dark .tabxplor-tab .o1,[data-bs-theme=dark] .o1,[data-bs-theme=dark] .tabxplor-tab .o1,[data-theme=dark] .o1,[data-theme=dark] .tabxplor-tab .o1,html.dark .o1,html.dark .tabxplor-tab .o1{background-color:#F5F5F5;}
  body.quarto-light .o2,body.quarto-light .tabxplor-tab .o2,[data-bs-theme=light] .o2,[data-bs-theme=light] .tabxplor-tab .o2,[data-theme=light] .o2,[data-theme=light] .tabxplor-tab .o2,body.quarto-dark .o2,body.quarto-dark .tabxplor-tab .o2,[data-bs-theme=dark] .o2,[data-bs-theme=dark] .tabxplor-tab .o2,[data-theme=dark] .o2,[data-theme=dark] .tabxplor-tab .o2,html.dark .o2,html.dark .tabxplor-tab .o2{background-color:#E4E4E4;}
  body.quarto-light .o3,body.quarto-light .tabxplor-tab .o3,[data-bs-theme=light] .o3,[data-bs-theme=light] .tabxplor-tab .o3,[data-theme=light] .o3,[data-theme=light] .tabxplor-tab .o3,body.quarto-dark .o3,body.quarto-dark .tabxplor-tab .o3,[data-bs-theme=dark] .o3,[data-bs-theme=dark] .tabxplor-tab .o3,[data-theme=dark] .o3,[data-theme=dark] .tabxplor-tab .o3,html.dark .o3,html.dark .tabxplor-tab .o3{background-color:#D0D0D0;}
  body.quarto-light .o4,body.quarto-light .tabxplor-tab .o4,[data-bs-theme=light] .o4,[data-bs-theme=light] .tabxplor-tab .o4,[data-theme=light] .o4,[data-theme=light] .tabxplor-tab .o4,body.quarto-dark .o4,body.quarto-dark .tabxplor-tab .o4,[data-bs-theme=dark] .o4,[data-bs-theme=dark] .tabxplor-tab .o4,[data-theme=dark] .o4,[data-theme=dark] .tabxplor-tab .o4,html.dark .o4,html.dark .tabxplor-tab .o4{background-color:#B8B8B8;}
  body.quarto-light .u1,body.quarto-light .tabxplor-tab .u1,[data-bs-theme=light] .u1,[data-bs-theme=light] .tabxplor-tab .u1,[data-theme=light] .u1,[data-theme=light] .tabxplor-tab .u1,body.quarto-dark .u1,body.quarto-dark .tabxplor-tab .u1,[data-bs-theme=dark] .u1,[data-bs-theme=dark] .tabxplor-tab .u1,[data-theme=dark] .u1,[data-theme=dark] .tabxplor-tab .u1,html.dark .u1,html.dark .tabxplor-tab .u1{background-color:#F5F5F5;}
  body.quarto-light .u2,body.quarto-light .tabxplor-tab .u2,[data-bs-theme=light] .u2,[data-bs-theme=light] .tabxplor-tab .u2,[data-theme=light] .u2,[data-theme=light] .tabxplor-tab .u2,body.quarto-dark .u2,body.quarto-dark .tabxplor-tab .u2,[data-bs-theme=dark] .u2,[data-bs-theme=dark] .tabxplor-tab .u2,[data-theme=dark] .u2,[data-theme=dark] .tabxplor-tab .u2,html.dark .u2,html.dark .tabxplor-tab .u2{background-color:#E4E4E4;}
  body.quarto-light .u3,body.quarto-light .tabxplor-tab .u3,[data-bs-theme=light] .u3,[data-bs-theme=light] .tabxplor-tab .u3,[data-theme=light] .u3,[data-theme=light] .tabxplor-tab .u3,body.quarto-dark .u3,body.quarto-dark .tabxplor-tab .u3,[data-bs-theme=dark] .u3,[data-bs-theme=dark] .tabxplor-tab .u3,[data-theme=dark] .u3,[data-theme=dark] .tabxplor-tab .u3,html.dark .u3,html.dark .tabxplor-tab .u3{background-color:#D0D0D0;}
  body.quarto-light .u4,body.quarto-light .tabxplor-tab .u4,[data-bs-theme=light] .u4,[data-bs-theme=light] .tabxplor-tab .u4,[data-theme=light] .u4,[data-theme=light] .tabxplor-tab .u4,body.quarto-dark .u4,body.quarto-dark .tabxplor-tab .u4,[data-bs-theme=dark] .u4,[data-bs-theme=dark] .tabxplor-tab .u4,[data-theme=dark] .u4,[data-theme=dark] .tabxplor-tab .u4,html.dark .u4,html.dark .tabxplor-tab .u4{background-color:#B8B8B8;}
}
</style>

## Installation

``` r
install.packages("tabxplor", dependencies = TRUE)

# Development version:
# install.packages("devtools")
devtools::install_github("BriceNocenti/tabxplor")
```

## A quick look

A simple cross-table with row percentages: shades of blue mean the cell
is over-represented compared to the total row, shades of red mean it is
under-represented, and the legend below the table says by how much.

``` r
gss <- gss_cat_data_formatting() # a cleaned-up version of forcats::gss_cat

tab(gss, race, party3, pct = "row", color = "difference")
```

<table class="tabxplor-tab">

<thead>

<tr>

<th class="tx-span" colspan="1">

</th>

<th class="tx-span" colspan="4">

party3
</th>

<th class="tx-span" colspan="1">

</th>

</tr>

<tr>

<th class="tx-l tx-br tx-bl tx-rv" rowspan="2">

race
</th>

<th class="tx-r tx-num">

1-Democrat
</th>

<th class="tx-r tx-num">

2-Independent,<br>other
</th>

<th class="tx-r tx-num">

3-Republican
</th>

<th class="tx-r tx-num">

NA
</th>

<th class="tx-r tx-num tx-br tx-bl tx-tot">

Total
</th>

</tr>

<tr>

<th class="tx-r tx-num tx-unit">

\<row%\>
</th>

<th class="tx-r tx-num tx-unit">

</th>

<th class="tx-r tx-num tx-unit">

</th>

<th class="tx-r tx-num tx-unit">

</th>

<th class="tx-r tx-num tx-br tx-bl tx-tot tx-unit">

\<row% (n)\>
</th>

</tr>

</thead>

<tbody>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

White
</td>

<td class="tx-r tx-num m1 tx-b">

39%
</td>

<td class="tx-r tx-num g1">

21%
</td>

<td class="tx-r tx-num p1 tx-b">

40%
</td>

<td class="tx-r tx-num g1">

1%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span class="tx-sec" style="font-weight:normal;"> (16 395)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Black
</td>

<td class="tx-r tx-num p3 tx-b">

75%
</td>

<td class="tx-r tx-num g1">

16%
</td>

<td class="tx-r tx-num m3 tx-b">

8%
</td>

<td class="tx-r tx-num g1">

1%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span class="tx-sec" style="font-weight:normal;"> ( 3 129)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Other
</td>

<td class="tx-r tx-num g1">

48%
</td>

<td class="tx-r tx-num p2 tx-b">

32%
</td>

<td class="tx-r tx-num m2 tx-b">

18%
</td>

<td class="tx-r tx-num g1">

1%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span class="tx-sec" style="font-weight:normal;"> ( 1 959)</span>
</td>

</tr>

<tr class="tx-b tx-bt tx-bb tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

Total
</td>

<td class="tx-r tx-num tx-b">

45%
</td>

<td class="tx-r tx-num tx-b">

21%
</td>

<td class="tx-r tx-num tx-b">

33%
</td>

<td class="tx-r tx-num tx-b">

1%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span class="tx-sec" style="font-weight:normal;"> (21 483)</span>
</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="6">

<div class="tx-foot">

Percentage points (risk) difference: cell ≥ the Total row
<span class="p1" style="font-weight:bold;">+5</span>; <span class="p2"
style="font-weight:bold;">+10</span>; <span class="p3"
style="font-weight:bold;">+20</span>; <span class="p4"
style="font-weight:bold;">+30</span> points; cell ≤ the Total row
<span class="m1" style="font-weight:bold;">-5</span>; <span class="m2"
style="font-weight:bold;">-10</span>; <span class="m3"
style="font-weight:bold;">-20</span>; <span class="m4"
style="font-weight:bold;">-30</span> points.

</div>

</td>

</tr>

</tfoot>

</table>

Several column variables can be crossed at once — handy for series of
survey questions, keeping only the level of interest. With
`color_signif = "grey_non_signif"`, cells that are *not* significantly
different from the total are greyed out, so every colored (or black)
figure is a solid one. Use `wt =` for weighted or survey data.

``` r
tab(gss, relig, c(married, income25k, black), pct = "row", levels = "first",
    color = "difference", color_signif = "grey_non_signif")
```

<table class="tabxplor-tab">

<thead>

<tr>

<th class="tx-span" colspan="2">

</th>

<th class="tx-span" colspan="1">

married
</th>

<th class="tx-span" colspan="1">

income25k
</th>

<th class="tx-span" colspan="1">

black
</th>

</tr>

<tr>

<th class="tx-l tx-br tx-bl tx-rv" rowspan="2">

relig
</th>

<th class="tx-r tx-num tx-br">

n
</th>

<th class="tx-r tx-num tx-br">

01-Married
</th>

<th class="tx-r tx-num tx-br">

01-\$25000 or<br>more
</th>

<th class="tx-r tx-num tx-br">

01-Black
</th>

</tr>

<tr>

<th class="tx-r tx-num tx-br tx-unit">

\<n\>
</th>

<th class="tx-r tx-num tx-br tx-unit">

\<row%\>
</th>

<th class="tx-r tx-num tx-br tx-unit">

\<row%\>
</th>

<th class="tx-r tx-num tx-br tx-unit">

\<row%\>
</th>

</tr>

</thead>

<tbody>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

1-Protestant
</td>

<td class="tx-r tx-num tx-br g2">

10 846
</td>

<td class="tx-r tx-num tx-br g1">

50%
</td>

<td class="tx-r tx-num tx-br g1">

32%
</td>

<td class="tx-r tx-num tx-br p1 tx-b">

21%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

2-Catholic
</td>

<td class="tx-r tx-num tx-br g2">

5 124
</td>

<td class="tx-r tx-num tx-br g1">

50%
</td>

<td class="tx-r tx-num tx-br g1">

35%
</td>

<td class="tx-r tx-num tx-br m2 tx-b">

4%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

3-Other christian
</td>

<td class="tx-r tx-num tx-br g2">

784
</td>

<td class="tx-r tx-num tx-br g1">

44%
</td>

<td class="tx-r tx-num tx-br g1">

35%
</td>

<td class="tx-r tx-num tx-br g1">

18%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

4-Jewish
</td>

<td class="tx-r tx-num tx-br g2">

388
</td>

<td class="tx-r tx-num tx-br g1">

51%
</td>

<td class="tx-r tx-num tx-br p1 tx-b">

43%
</td>

<td class="tx-r tx-num tx-br m2 tx-b">

3%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

5-Buddhist/Hinduist
</td>

<td class="tx-r tx-num tx-br g2">

218
</td>

<td class="tx-r tx-num tx-br g1">

51%
</td>

<td class="tx-r tx-num tx-br p2 tx-b">

47%
</td>

<td class="tx-r tx-num tx-br m1 tx-b">

5%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

6-Muslim
</td>

<td class="tx-r tx-num tx-br g2">

104
</td>

<td class="tx-r tx-num tx-br g1">

53%
</td>

<td class="tx-r tx-num tx-br g1">

32%
</td>

<td class="tx-r tx-num tx-br p2 tx-b">

34%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

7-Other
</td>

<td class="tx-r tx-num tx-br g2">

388
</td>

<td class="tx-r tx-num tx-br m1 tx-b">

37%
</td>

<td class="tx-r tx-num tx-br g1">

37%
</td>

<td class="tx-r tx-num tx-br g1">

13%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

8-None
</td>

<td class="tx-r tx-num tx-br g2">

3 523
</td>

<td class="tx-r tx-num tx-br m2 tx-b">

37%
</td>

<td class="tx-r tx-num tx-br g1">

37%
</td>

<td class="tx-r tx-num tx-br g1">

11%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

NA
</td>

<td class="tx-r tx-num tx-br g2">

108
</td>

<td class="tx-r tx-num tx-br g1">

45%
</td>

<td class="tx-r tx-num tx-br m2 tx-b">

15%
</td>

<td class="tx-r tx-num tx-br g1">

18%
</td>

</tr>

<tr class="tx-b tx-bt tx-bb tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

Total
</td>

<td class="tx-r tx-num tx-br g2">

21 483
</td>

<td class="tx-r tx-num tx-br tx-b">

47%
</td>

<td class="tx-r tx-num tx-br tx-b">

34%
</td>

<td class="tx-r tx-num tx-br tx-b">

15%
</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="5">

<div class="tx-foot">

Percentage points (risk) difference: cell ≥ the Total row
<span class="p1" style="font-weight:bold;">+5</span>; <span class="p2"
style="font-weight:bold;">+10</span>; <span class="p3"
style="font-weight:bold;">+20</span>; <span class="p4"
style="font-weight:bold;">+30</span> points; cell ≤ the Total row
<span class="m1" style="font-weight:bold;">-5</span>; <span class="m2"
style="font-weight:bold;">-10</span>; <span class="m3"
style="font-weight:bold;">-20</span>; <span class="m4"
style="font-weight:bold;">-30</span> points. Uncoloured: not
significantly different from the Total row (Newcombe score interval, 95%
confidence) or under the first colour threshold (±5 points).

</div>

</td>

</tr>

</tfoot>

</table>

The same visual language extends to regression models: `tab_reg()`
detects a binary outcome and fits a logistic regression, coloring odds
ratios by strength and greying the non-significant ones, with a possible
comparison between modelised quantities and their crude observed
empirical counterparts.

``` r
tab_reg(gss, outcome = "married", predictors = c("race", "age", "rincome"), empirical = TRUE)
```

<div class="tabxplor-caption">

Logistic regression: married by race, age +1 more

</div>

<table class="tabxplor-tab tx-has-stars">

<thead>

<tr>

<th class="tx-span" colspan="3">

</th>

<th class="tx-span" colspan="2">

married: 01-Married
</th>

</tr>

<tr>

<th class="tx-l tx-br tx-bl" rowspan="2">

</th>

<th class="tx-l tx-br tx-bl tx-rv" rowspan="2">

levels
</th>

<th class="tx-r tx-num tx-br">

n
</th>

<th class="tx-r tx-num">

Obs_OR
</th>

<th class="tx-r tx-num tx-br">

Model_OR
</th>

</tr>

<tr>

<th class="tx-r tx-num tx-br tx-unit">

\<n\>
</th>

<th class="tx-r tx-num tx-unit">

\<(obs%) OR\>
</th>

<th class="tx-r tx-num tx-br tx-unit">

\<OR (adj%)\>
</th>

</tr>

</thead>

<tbody>

<tr class="tx-b tx-bb2">

<td class="tx-l tx-br tx-bl tx-lbl tx-b tx-nb" rowspan="1">

Constant
</td>

<td class="tx-l tx-br tx-bl tx-rv">

Reference profile
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

1/1.43<span class="tx-sec" style="font-weight:normal;">\*\*\* (41%)</span>
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-lbl tx-b tx-nb" rowspan="3">

race
</td>

<td class="tx-l tx-br tx-bl tx-rv">

White
</td>

<td class="tx-r tx-num tx-br tx-b">

9 862
</td>

<td class="tx-r tx-num tx-b">

<span class="tx-sec"
style="font-weight:normal;">(52%) </span>     1<span class="tx-sec"
style="font-weight:normal;">   </span>
</td>

<td class="tx-r tx-num tx-br tx-b">

     1<span class="tx-sec" style="font-weight:normal;">    (51%)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Black
</td>

<td class="tx-r tx-num tx-br g2">

1 867
</td>

<td class="tx-r tx-num m3 tx-b">

<span class="tx-sec"
style="font-weight:normal;">(31%) </span>1/2.45<span class="tx-sec" style="font-weight:normal;">\*\*\*</span>
</td>

<td class="tx-r tx-num tx-br m3 tx-b">

1/2.22<span class="tx-sec" style="font-weight:normal;">\*\*\* (33%)</span>
</td>

</tr>

<tr class="tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

Other
</td>

<td class="tx-r tx-num tx-br g2">

1 261
</td>

<td class="tx-r tx-num g1">

<span class="tx-sec">(49%) </span>1/1.11<span class="tx-sec">\*  </span>
</td>

<td class="tx-r tx-num tx-br g1">

  1.08<span class="tx-sec">    (53%)</span>
</td>

</tr>

<tr class="tx-bb2">

<td class="tx-l tx-br tx-bl tx-lbl tx-b tx-nb" rowspan="1">

age
</td>

<td class="tx-l tx-br tx-bl tx-rv">

per 26.9 (2SD), at 42.4 (mean)
</td>

<td class="tx-r tx-num tx-br g2">

</td>

<td class="tx-r tx-num p3 tx-b">

<span class="tx-sec"
style="font-weight:normal;">      </span>  2.13<span class="tx-sec" style="font-weight:normal;">\*\*\*</span>
</td>

<td class="tx-r tx-num tx-br p2 tx-b">

  1.95<span class="tx-sec" style="font-weight:normal;">\*\*\*      </span>
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-lbl tx-b tx-nb" rowspan="4">

rincome
</td>

<td class="tx-l tx-br tx-bl tx-rv">

1-Lt \$10000
</td>

<td class="tx-r tx-num tx-br tx-b">

2 149
</td>

<td class="tx-r tx-num tx-b">

<span class="tx-sec"
style="font-weight:normal;">(37%) </span>     1<span class="tx-sec"
style="font-weight:normal;">   </span>
</td>

<td class="tx-r tx-num tx-br tx-b">

     1<span class="tx-sec" style="font-weight:normal;">    (39%)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

2-\$10000 to 14999
</td>

<td class="tx-r tx-num tx-br g2">

1 168
</td>

<td class="tx-r tx-num p1 tx-b">

<span class="tx-sec"
style="font-weight:normal;">(41%) </span>  1.21<span class="tx-sec" style="font-weight:normal;">\*\* </span>
</td>

<td class="tx-r tx-num tx-br g1">

  1.15<span class="tx-sec">\*   (42%)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

3-\$15000 to 24999
</td>

<td class="tx-r tx-num tx-br g2">

2 325
</td>

<td class="tx-r tx-num p1 tx-b">

<span class="tx-sec"
style="font-weight:normal;">(43%) </span>  1.33<span class="tx-sec" style="font-weight:normal;">\*\*\*</span>
</td>

<td class="tx-r tx-num tx-br p1 tx-b">

  1.28<span class="tx-sec" style="font-weight:normal;">\*\*\* (45%)</span>
</td>

</tr>

<tr class="tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

4-\$25000 or more
</td>

<td class="tx-r tx-num tx-br g2">

7 348
</td>

<td class="tx-r tx-num p3 tx-b">

<span class="tx-sec"
style="font-weight:normal;">(55%) </span>  2.14<span class="tx-sec" style="font-weight:normal;">\*\*\*</span>
</td>

<td class="tx-r tx-num tx-br p2 tx-b">

  1.85<span class="tx-sec" style="font-weight:normal;">\*\*\* (54%)</span>
</td>

</tr>

<tr class="tx-b tx-bt2">

<td class="tx-l tx-br tx-bl tx-lbl tx-vname tx-b tx-bb2" rowspan="8">

Model fit
</td>

<td class="tx-l tx-br tx-bl tx-rv">

N
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

12 990
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

Dispersion (robust/model SE)
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

1.00
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

Collinearity (max VIF)
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

1.03
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

Influence (max dfbetas)
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

0.05
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

LR vs null
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

\<0.01%
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

McFadden R2
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

0.049
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

AIC
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

17 129
</td>

</tr>

<tr class="tx-b tx-bb tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

BIC
</td>

<td class="tx-r tx-num tx-br tx-b">

</td>

<td class="tx-r tx-num tx-b">

</td>

<td class="tx-r tx-num tx-br tx-b">

17 181
</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="5">

<div class="tx-foot">

Model: logistic regression; OR: odds ratio (vs the reference category);
obs%: observed proportion; adj%: adjusted/predicted
proportion.<br>Obs_OR, Model_OR — OR ≥ <span class="p1"
style="font-weight:bold;">1.2</span>; <span class="p2"
style="font-weight:bold;">1.5</span>; <span class="p3"
style="font-weight:bold;">2</span>; <span class="p4"
style="font-weight:bold;">4</span>; OR ≤ <span class="m1"
style="font-weight:bold;">1/1.2</span>; <span class="m2"
style="font-weight:bold;">1/1.5</span>; <span class="m3"
style="font-weight:bold;">1/2</span>; <span class="m4"
style="font-weight:bold;">1/4</span>. Uncoloured: not significantly
different from the reference category (Wald interval on the log
odds-ratio, 95% confidence; matching Woolf interval on the observed
column) or under the first colour threshold (×1.2).<br>\*\*\*:
significantly different from the reference category (in bold) at the 99%
confidence level (from 1 for the Constant); \*\*: at the 95% level; \*:
at the 90% level; no star: not significant.

</div>

</td>

</tr>

</tfoot>

</table>

<br>
<table class="tabxplor-tab">

<thead>

<tr>

<th class="tx-l">

outcome (model scale)
</th>

<th class="tx-l">

numeric predictor
</th>

<th class="tx-l">

observed range
</th>

<th class="tx-l">

observed shape (central 95%)
</th>

</tr>

</thead>

<tbody>

<tr>

<td class="tx-l">

log(%Married / (1 - %Married))
</td>

<td class="tx-l">

age
</td>

<td class="tx-l">

13-57% (OR 8.7)
</td>

<td class="tx-l tx-sparkcell">

<svg class="tx-spark" width="192.6" height="44" viewBox="0 0 192.6 44" aria-hidden="true">

<polyline points="1.3,42.7 11.3,30.9 21.3,13.1 31.3,7.2 41.3,1.3 51.3,1.3 61.3,1.3 71.3,1.3 81.3,1.3 91.3,1.3 101.3,1.3 111.3,1.3 121.3,1.3 131.3,1.3 141.3,1.3 151.3,1.3 161.3,7.2 171.3,7.2 181.3,13.1 191.3,13.1" fill="none" stroke="currentColor" stroke-width="2.6" stroke-linejoin="round" stroke-linecap="round"/>
</svg>

</td>

</tr>

</tbody>

</table>

## Export your tables

Any table exports with its colors to Excel, html or markdown, and can be
drawn as a plot:

``` r
tab(gss, marital, race, pct = "row", color = "difference") |> tab_export()  # "html"
tab(gss, marital, race, pct = "row", color = "difference") |> tab_xl()      # Excel
tab(gss, marital, race, pct = "row", color = "difference") |> tab_xl(theme = "print_ready")  # black & white
```

A colored html table also *prints*, or saves to PDF, in that
black-and-white scheme on its own.

## Learn more

- [Introduction to tabxplor](articles/tabxplor.html) — the place to
  start (*aussi disponible [en français](articles/tabxplor-fr.html)*).
- [Regression tables with tab_reg()](articles/tabxplor-reg.html) (*aussi
  disponible [en français](articles/tabxplor-reg-fr.html)*).
- [All else equal: reading a regression without losing the
  data](articles/tabxplor-all-else-equal.html) — a single analysis
  walked from a first cross-table to a finished sentence (*aussi
  disponible [en français](articles/tabxplor-all-else-equal-fr.html)*).
- [Weighted and survey data](articles/tabxplor-weights.html) — the three
  levels of margin of error, and which one your file deserves (*aussi
  disponible [en français](articles/tabxplor-weights-fr.html)*).
- [Programming with tabxplor](articles/tabxplor-programming.html) — many
  tables at once, custom workflows, options (*aussi disponible [en
  français](articles/tabxplor-programming-fr.html)*).

No code needed: the tabxplor modules for the free statistical
spreadsheet [jamovi](https://www.jamovi.org/) offer the same tables
(Crosstables and Regressions) in a point-and-click interface.
