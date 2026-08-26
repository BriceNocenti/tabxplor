<style>
.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
.tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
.tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
.tabxplor-tab tfoot{font-size:80%;text-align:left;}
.tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
.tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
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
.tabxplor-tab .tx-foot{width:0;min-width:100%;}
.tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
.tooltip-inner{max-width:none;white-space:nowrap;}
.popover{max-width:none;}
.popover-body,.popover-content{padding:6px;white-space:nowrap;}
.tabxplor-tab{color:#000000;background:#ffffff;}
.tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
.tabxplor-tab tbody tr:hover{background:#FFFCE5;}
.g1,.tabxplor-tab .g1{color:#9f9f9f;}
.g2,.tabxplor-tab .g2{color:#111111;}
.tabxplor-caption{color:#000000;}
.tabxplor-tab .tx-sec{color:#000000;}
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
@media print {
  .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
  .tabxplor-tab{color:#000000;background:#ffffff;}
  .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
  .tabxplor-tab tbody tr:hover{background:transparent;}
  .g1,.tabxplor-tab .g1{color:#595959;}
  .g2,.tabxplor-tab .g2{color:#111111;}
  .tabxplor-caption{color:#000000;}
  .tabxplor-tab .tx-sec{color:#000000;}
  .p1,.tabxplor-tab .p1{color:#000000;}
  .p2,.tabxplor-tab .p2{color:#000000;}
  .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
  .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
  .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
  .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
  .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
  .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
  .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
  .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
  .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
  .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
  .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
  .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
  .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
  .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
}
</style>

::: {.tabxplor-tab}

|             | levels                                |   |          n |         n |   |                 Obs_OR |               Model_OR |   |                Obs_IRR |              Model_IRR |
|:------------|:--------------------------------------|---|-----------:|----------:|---|-----------------------:|-----------------------:|---|-----------------------:|-----------------------:|
|             |                                       |   |        *n* |           |   |     *married: Married* |                        |   |              *tvhours* |                        |
|             |                                       |   |            |           |   |                        |                        |   |                        |                        |
| *Constant*  | **Reference population**              |   | **12 960** | **6 803** |   |                        |          **1/3.07***** |   |                        |           **×2.51***** |
|             |                                       |   |            |           |   |                        |                        |   |                        |                        |
| *race*      | **White**                             |   |  **9 846** | **5 132** |   |       (52%)      **1** |         **1    (51%)** |   |       (2.36)     **1** |        **1    (2.37)** |
|             | Black                                 |   |      1 860 |     1 006 |   | [(31%) 1/2.43***]{.m3} | [1/2.40*** (32%)]{.m3} |   | [(3.66) ×1.55***]{.p2} | [×1.51*** (3.59)]{.p2} |
|             | Other                                 |   |      1 254 |       665 |   |          (49%) 1/1.12* |          1.11    (54%) |   |           (2.44) ×1.03 |        ×1.03    (2.44) |
|             |                                       |   |            |           |   |                        |                        |   |                        |                        |
| *rincome*   | **Lt $10000**                         |   |  **2 142** | **1 150** |   |       (37%)      **1** |         **1    (39%)** |   |       (3.10)     **1** |        **1    (3.12)** |
|             | $10000 to 14999                       |   |      1 164 |       664 |   |         (41%)   1.20** |          1.14*   (42%) |   |           (3.02) ÷1.03 |        ÷1.05    (2.97) |
|             | $15000 to 24999                       |   |      2 322 |     1 156 |   | [(43%)   1.32***]{.p1} |   [1.27*** (45%)]{.p1} |   |        (2.84) ÷1.09*** |        ÷1.12*** (2.79) |
|             | $25000 or more                        |   |      7 332 |     3 833 |   | [(55%)   2.12***]{.p3} |   [1.86*** (54%)]{.p2} |   | [(2.24) ÷1.38***]{.m1} | [÷1.39*** (2.25)]{.m1} |
|             |                                       |   |            |           |   |                        |                        |   |                        |                        |
| *relig*     | **Protestant**                        |   |  **6 269** | **3 230** |   |       (52%)      **1** |         **1    (53%)** |   |       (2.72)     **1** |        **1    (2.61)** |
|             | Catholic                              |   |      3 121 |     1 617 |   |           (52%) 1/1.03 |        1/1.17*** (49%) |   |        (2.50) ÷1.09*** |        ×1.02    (2.65) |
|             | Other christian                       |   |        507 |       272 |   | [(42%) 1/1.53***]{.m2} | [1/1.43*** (45%)]{.m1} |   |        (2.31) ÷1.18*** |        ÷1.15*** (2.26) |
|             | Jewish                                |   |        222 |       109 |   |           (55%)   1.10 |        1/1.29*   (47%) |   | [(2.14) ÷1.27***]{.m1} |        ÷1.11    (2.36) |
|             | Buddhist/Hinduist                     |   |        144 |        78 |   |           (50%) 1/1.09 |        1/1.36*   (46%) |   | [(1.95) ÷1.40***]{.m1} | [÷1.24**  (2.11)]{.m1} |
|             | Muslim                                |   |         56 |        35 |   |           (48%) 1/1.17 |          1.05    (54%) |   | [(1.89) ÷1.44** ]{.m1} | [÷1.54*** (1.70)]{.m2} |
|             | Other                                 |   |        267 |       140 |   | [(35%) 1/2.04***]{.m3} | [1/2.04*** (37%)]{.m3} |   |           (2.64) ÷1.03 |        ×1.01    (2.65) |
|             | None                                  |   |      2 374 |     1 322 |   | [(38%) 1/1.76***]{.m2} | [1/1.79*** (39%)]{.m2} |   |        (2.37) ÷1.15*** |        ÷1.07*** (2.44) |
|             |                                       |   |            |           |   |                        |                        |   |                        |                        |
| *age*       | age (per 1 SD (13.5))                 |   |            |           |   |         [1.46***]{.p1} |         [1.35***]{.p1} |   |                ×1.02** |               ×1.05*** |
|             |                                       |   |            |           |   |                        |                        |   |                        |                        |
| *Model fit* | **N**                                 |   |            |           |   |                        |             **12 960** |   |                        |              **6 803** |
|             | **LR vs null**                        |   |            |           |   |                        |             **<0.01%** |   |                        |             **<0.01%** |
|             | **McFadden R2**                       |   |            |           |   |                        |              **0.057** |   |                        |              **0.033** |
|             | **AIC**                               |   |            |           |   |                        |             **16 960** |   |                        |             **26 121** |
|             | **BIC**                               |   |            |           |   |                        |             **17 064** |   |                        |             **26 217** |
|             | **Pearson dispersion (phi)**          |   |            |           |   |                        |                        |   |                        |               **1.46** |
|             | **Overall association (LR): race**    |   |            |           |   |                        |             **<0.01%** |   |                        |             **<0.01%** |
|             | **Overall association (LR): relig**   |   |            |           |   |                        |             **<0.01%** |   |                        |             **<0.01%** |
|             | **Overall association (LR): rincome** |   |            |           |   |                        |             **<0.01%** |   |                        |             **<0.01%** |
|             | **Dispersion (robust/model SE)**      |   |            |           |   |                        |               **1.03** |   |                        |               **1.41** |
|             | **Influence (max dfbetas)**           |   |            |           |   |                        |               **0.19** |   |                        |               **0.66** |
|             | **Collinearity (max VIF)**            |   |            |           |   |                        |               **1.08** |   |                        |               **1.09** |

: Regression models: married, tvhours by race, rincome +2 more

Model (married): logistic regression; OR = odds ratio (vs the reference category).
Model (tvhours): Poisson regression; IRR = incidence-rate ratio (vs the reference category).
**Obs_OR, Model_OR —**Shades of blue: OR ≥ **[1.2]{.p1}**; **[1.5]{.p2}**; **[2]{.p3}**; **[4]{.p4}**. Shades of yellow to red: OR ≤ **[1/1.2]{.m1}**; **[1/1.5]{.m2}**; **[1/2]{.m3}**; **[1/4]{.m4}**. Coloured: significantly different from the reference category (Wald interval on the log odds-ratio, 95% confidence --- Woolf closed form on the observed column), by at least the first colour threshold. Uncoloured: either not significant, or a difference under ×1.2.
**Obs_IRR, Model_IRR —**Shades of blue: IRR ≥ **[×1.2]{.p1}**; **[×1.5]{.p2}**; **[×2]{.p3}**; **[×4]{.p4}**. Shades of yellow to red: IRR ≤ **[÷1.2]{.m1}**; **[÷1.5]{.m2}**; **[÷2]{.m3}**; **[÷4]{.m4}**. Coloured: significantly different from the reference category (Wald interval on the log rate-ratio, 95% confidence --- quasi-Poisson closed form on the observed column), by at least the first colour threshold. Uncoloured: either not significant, or a difference under ×1.2.
\*\*\*: significantly different from no effect (the reference category in bold; for the Constant, the null value) at the 99% confidence level; \*\*: at the 95% level; \*: at the 90% level; no star: not significant.
:::
