# Introduction to tabxplor

``` r
library(tabxplor)
```

`tabxplor` helps you **explore data with cross-tables, coloring the
cells so you can read a table at a glance**. Over-represented cells use
shades of blue, under-represented ones turn red/orange, so patterns jump
out without you squinting at every number.

Everything is a `tibble`, so the result works with the usual `dplyr`
verbs, and tables export to Excel, HTML and Markdown with their color
helpers. Underlying heavy computations run on `data.table`

Throughout this vignette we use `gss_simple`, a cleaned-up version of
the General Social Survey
([`forcats::gss_cat`](https://forcats.tidyverse.org/reference/gss_cat.html))
with factors levels merged and reordered.

``` r
gss_simple <- gss_cat_data_formatting()
```

## Your first cross-tables

[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
needs a data frame, a **row variable** and a **column variable**. By
default it shows counts:

``` r
tab(gss_simple, marital, race)
```

``` r-output
#> # A tabxplor tab: 7 × 5
#>   marital        White Black Other  Total
#>   <fct>            <n>   <n>   <n>    <n>
#> 1 Married        8 316   869   932 10 117
#> 2 Separated        437   196   110    743
#> 3 Divorced       2 676   495   212  3 383
#> 4 Widowed        1 475   262    70  1 807
#> 5 Never married  3 478 1 305   633  5 416
#> 6 NA                13     2     2     17
#> 7 Total         16 395 3 129 1 959 21 483
```

Add `pct = "row"` for row percentages (or `"col"` for column
percentages). A **Total** row/column and a count column (`n`) are added
automatically:

``` r
tab(gss_simple, marital, race, pct = "row")
```

``` r-output
#> # A tabxplor tab: 7 × 5
#>   marital        White  Black  Other           Total
#>   <fct>         <row%> <row%> <row%>          <row%>
#> 1 Married          82%     9%     9% 100% (n=10 117)
#> 2 Separated        59%    26%    15% 100% (n=   743)
#> 3 Divorced         79%    15%     6% 100% (n= 3 383)
#> 4 Widowed          82%    14%     4% 100% (n= 1 807)
#> 5 Never married    64%    24%    12% 100% (n= 5 416)
#> 6 NA               76%    12%    12% 100% (n=    17)
#> 7 Total            76%    15%     9% 100% (n=21 483)
```

When the column variable is **numeric**,
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
shows its **mean** in each row instead of percentages:

``` r
tab(gss_simple, marital, age)
```

``` r-output
#> # A tabxplor tab: 7 × 2
#>   marital            age
#>   <fct>           <mean>
#> 1 Married       49 (σ15)
#> 2 Separated     45 (σ13)
#> 3 Divorced      51 (σ13)
#> 4 Widowed       72 (σ13)
#> 5 Never married 34 (σ13)
#> 6 NA            52 (σ17)
#> 7 Total         47 (σ17)
```

You can pass **several row and column variables at once**.

``` r
tab(gss_simple, c(race, relig), c(party3, tvhours), na = "drop", pct = "row")
```

``` r-output
#> # A tabxplor tab: 13 × 7
#> # Groups:         row_var [2]
#>    row_var levels              `1-Democrat` `2-Independent, other`
#>    <fct>   <fct>                     <row%>                 <row%>
#>  1 race    White                        39%                    21%
#>  2 race    Black                        76%                    17%
#>  3 race    Other                        49%                    33%
#>  4 race    Total                        45%                    21%
#> 
#>  5 relig   1-Protestant                 43%                    17%
#>  6 relig   2-Catholic                   46%                    22%
#>  7 relig   3-Other christian            42%                    24%
#>  8 relig   4-Jewish                     68%                    12%
#>  9 relig   5-Buddhist/Hinduist          57%                    29%
#> 10 relig   6-Muslim                     66%                    22%
#> 11 relig   7-Other                      48%                    29%
#> 12 relig   8-None                       50%                    31%
#> 13 relig   Total                        45%                    21%
#> # ℹ 3 more variables: `3-Republican` <row%>, Total <row%>, tvhours <mean>
```

`levels = "first"` keeps only the first level of each column factor,
which is handy to display many binary factors, like survey questions
with multiple answers, all at once, in a compact way :

``` r
tab(gss_simple, relig, c(married, black, income25k), pct = "row", levels = "first", na = "drop", cleannames = TRUE)
```

``` r-output
#> # A tabxplor tab: 9 × 4
#>   relig             Married  Black `$25000 or more`
#>   <fct>              <row%> <row%>           <row%>
#> 1 Protestant            50%    21%              32%
#> 2 Catholic              50%     4%              35%
#> 3 Other christian       44%    18%              35%
#> 4 Jewish                51%     3%              43%
#> 5 Buddhist/Hinduist     51%     5%              47%
#> 6 Muslim                53%    34%              32%
#> 7 Other                 37%    13%              37%
#> 8 None                  37%    11%              37%
#> 9 Total                 47%    15%              34%
```

A few other everyday arguments: `na = "drop"` to drop missing values
from the base, `digits =` for the number of decimals, and
`cleannames = TRUE` to strip prefixes like `"1-"` from level names. See
[`?tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
for the full list.

## Weights

The argument `wt =` adds a survey weight. Every percentage and mean is
then **weighted**, while the sample size behind the confidence intervals
stays the real, **unweighted** number of cases — the honest default
basis for uncertainty.

``` r
data(hdv2003, package = "questionr")
tab(hdv2003, nivetud, occup, wt = poids, pct = "row", na="drop", digits = 1)
```

Under **unequal** weights that default interval carries no design
effect, so it runs a little too narrow. Kish’s *effective sample size*
fixes this : it counts each observation by how much it really
contributes, `n_eff = (sum of w)² / (sum of w²)` (always at most the
real *n*), and uses `n_eff` in place of the raw *n*. Turn it on and
every weighted confidence interval in the table — proportions and means
alike — widens honestly:

``` r
options(tabxplor.kish_neff = TRUE)
```

This is a simple single-stage approximation (it needs the individual
weights, so it is not available for tables built from pre-aggregated
counts). A **fully design-based** result — clusters, strata, exact
standard errors — is available for Chi2 pvalues (factors) and ANOVA F
pvalues (numeric column variables) only with `test = "survey"` and the
related arguments (see the `test =` argument) : confidence intervals are
not covered. See the [survey](https://CRAN.R-project.org/package=survey)
package for more informations about survey design.
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
regressions tables with weights always use design-effect for standard
errors
([`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor-reg.md)),
but the base version only use Kish’s effective sample size.

## Sub-tables

Give
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md) a
third variable as `tab_vars` and it builds **one sub-table per group**
(here, one per income group). The result is *grouped*: `dplyr`
operations then run within each sub-table.

``` r
tab(gss_simple, race, party3, rincome, na = "drop", pct = "row")
```

``` r-output
#> # A tabxplor tab: 17 × 6
#> # Groups:         rincome [5]
#>    rincome           race                    `1-Democrat` `2-Independent, other`
#>    <fct>             <fct>                         <row%>                 <row%>
#>  1 1-Lt $10000       White                            38%                    26%
#>  2 1-Lt $10000       Black                            67%                    22%
#>  3 1-Lt $10000       Other                            49%                    31%
#>  4 1-Lt $10000       Total 1-Lt $10000                45%                    26%
#> 
#>  5 2-$10000 to 14999 White                            40%                    27%
#>  6 2-$10000 to 14999 Black                            76%                    14%
#>  7 2-$10000 to 14999 Other                            43%                    44%
#>  8 2-$10000 to 14999 Total 2-$10000 to 14999          47%                    26%
#> 
#>  9 3-$15000 to 24999 White                            38%                    26%
#> 10 3-$15000 to 24999 Black                            79%                    15%
#> 11 3-$15000 to 24999 Other                            45%                    39%
#> 12 3-$15000 to 24999 Total 3-$15000 to 24999          46%                    25%
#> 
#> 13 4-$25000 or more  White                            39%                    17%
#> 14 4-$25000 or more  Black                            81%                    12%
#> 15 4-$25000 or more  Other                            56%                    22%
#> 16 4-$25000 or more  Total 4-$25000 or more           45%                    16%
#> 
#> 17 Ensemble          Total Ensemble                   45%                    20%
#> # ℹ 2 more variables: `3-Republican` <row%>, Total <row%>
```

When you pass several **row variables** *without* `tab_vars`,
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
merges the mirror tables into a single table by default.
`output_list = TRUE` returns instead a **list with one table per row
variable** (with `tab_vars`, the result is always a list):

``` r
tab(gss_simple, c(married, income25k), race, pct = "row", output_list = TRUE)
```

``` r-output
<!-- KNITR_ASIS_OUTPUT_TOKEN --><style>.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
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
.g1{color:#9f9f9f;}
.g2{color:#111111;}
.tabxplor-caption{color:#000000;}
.p1{color:#02A5B3;}
.p2{color:#0891C9;}
.p3{color:#0267C7;}
.p4{color:#300DFD;}
.m1{color:#DCA331;}
.m2{color:#DE7C01;}
.m3{color:#DD5301;}
.m4{color:#D60103;}
.o1{background-color:#DFFCFF;}
.o2{background-color:#D7EFFF;}
.o3{background-color:#CEE3FF;}
.o4{background-color:#BBCCFF;}
.u1{background-color:#FFF4E1;}
.u2{background-color:#FFE6D3;}
.u3{background-color:#FFD7C8;}
.u4{background-color:#FFBAAF;}</style>
<table class="tabxplor-tab"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="3">race</th><th class="tx-span" colspan="1"></th></tr><tr><th class="tx-l tx-br tx-bl tx-rv">married</th><th class="tx-r tx-num">White</th><th class="tx-r tx-num">Black</th><th class="tx-r tx-num">Other</th><th class="tx-r tx-num tx-br tx-bl tx-tot">Total</th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">01-Married</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +6% ; ratio: ×1.1 ; n: 8 316">82%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.7 ; n: 869">9%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 932">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 10 117">100%<span style="font-weight:normal;"> (n=10 117)</span></td></tr>
<tr><td class="tx-l tx-br tx-bl tx-rv">02-Not married</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷1.1 ; n: 8 079">71%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×1.4 ; n: 2 260">20%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×1 ; n: 1 027">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 11 366">100%<span style="font-weight:normal;"> (n=11 366)</span></td></tr>
<tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 16 395">76%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 3 129">15%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 959">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 21 483">100%<span style="font-weight:normal;"> (n=21 483)</span></td></tr></tbody></table>
<br>
<table class="tabxplor-tab"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="3">race</th><th class="tx-span" colspan="1"></th></tr><tr><th class="tx-l tx-br tx-bl tx-rv">income25k</th><th class="tx-r tx-num">White</th><th class="tx-r tx-num">Black</th><th class="tx-r tx-num">Other</th><th class="tx-r tx-num tx-br tx-bl tx-tot">Total</th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">01-$25000 or more</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1 ; n: 5 856">80%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.2 ; n: 886">12%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.1 ; n: 621">8%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 7 363">100%<span style="font-weight:normal;"> (n= 7 363)</span></td></tr>
<tr><td class="tx-l tx-br tx-bl tx-rv">02-Less than 25k</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -2% ; ratio: ×1 ; n: 10 539">75%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 2 243">16%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 1 338">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 14 120">100%<span style="font-weight:normal;"> (n=14 120)</span></td></tr>
<tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 16 395">76%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 3 129">15%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 959">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 21 483">100%<span style="font-weight:normal;"> (n=21 483)</span></td></tr></tbody></table>

<!-- KNITR_ASIS_OUTPUT_TOKEN -->
```

## colors: reading helpers

One you the main purposes of `tabxplor` is to provide a full palette of
color helpers for data exploration. `color = "diff"` colors each cell by
**how far it sits from its reference** — by default the Total of its row
or column. Cells clearly above the average turn **blue**, cells clearly
below turn **red/orange** — the further a cell sits from its reference,
the stronger the shade — and a color legend is printed underneath.

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff")
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race  `1-Democrat` `2-Independent, other` `3-Republican`  `NA`           Total
#>   <cha>       <row%>                 <row%>         <row%> <row>          <row%>
#> 1 White          39%                    21%            40%    1% 100% (n=16 395)
#> 2 Black          75%                    16%             8%    1% 100% (n= 3 129)
#> 3 Other          48%                    32%            18%    1% 100% (n= 1 959)
#> 4 Total          45%                    21%            33%    1% 100% (n=21 483)
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
```

`color = TRUE` picks a sensible scheme automatically for each column
type (both differences and ratio for percentages, only ratios for means,
…), check which one in the legend:

``` r
tab(gss_simple, rincome, c(party3, marital), pct = "row", color = TRUE)
```

``` r-output
#> # A tabxplor tab: 6 × 12
#>   rincome           `1-Democrat` `2-Independent, other` `3-Republican` NA_party3
#>   <fct>                   <row%>                 <row%>         <row%>    <row%>
#> 1 1-Lt $10000                44%                    25%            30%        1%
#> 2 2-$10000 to 14999          46%                    26%            27%        1%
#> 3 3-$15000 to 24999          45%                    25%            29%        0%
#> 4 4-$25000 or more           45%                    16%            38%        0%
#> 5 NA                         45%                    22%            32%        1%
#> 6 Total                      45%                    21%            33%        1%
#> # ℹ 7 more variables: Married <row%>, Separated <row%>, Divorced <row%>,
#> #   Widowed <row%>, `Never married` <row%>, NA_marital <row%>, Total <row%>
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30; bg ratio: ÷4 ÷2 ÷1.5 ×1.5 ×2 ×4
```

Numeric columns are colored the same way, on their **means** (here,
hours of TV per day by income):

``` r
tab(gss_simple, rincome, tvhours, color = "diff")
```

``` r-output
#> # A tabxplor tab: 6 × 2
#>   rincome              tvhours
#>   <fct>                 <mean>
#> 1 1-Lt $10000       3.1 (σ2.8)
#> 2 2-$10000 to 14999 3.0 (σ2.4)
#> 3 3-$15000 to 24999 2.8 (σ2.1)
#> 4 4-$25000 or more  2.2 (σ1.7)
#> 5 NA                3.6 (σ3.1)
#> 6 Total             3.0 (σ2.6)
#> # standardized difference (Total): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8
```

**Which cell is the reference for comparison ?** By default each cell is
compared to the relevant Total (Total row for row percentages and Total
column for column percentages) to highlight over-representations and
under-representations. Two useful alternatives:

- `ref = 1` compares each row to the **first row** — perfect for reading
  an evolution over time or an ordinal factor.
- with sub-tables, `comp = "all"` compares against the **overall** Total
  instead of each sub-table’s own Total.

``` r
tab(gss_simple, year, marital, pct = "row", color = "diff", ref = 1)
```

``` r-output
#> # A tabxplor tab: 9 × 8
#>   year  Married Separated Divorced Widowed `Never married`  `NA`           Total
#>   <cha>  <row%>    <row%>   <row%>  <row%>          <row%> <row>          <row%>
#> 1 2000      45%        4%      16%     10%             25%    0% 100% (n= 2 817)
#> 2 2002      46%        3%      16%      9%             26%    0% 100% (n= 2 765)
#> 3 2004      53%        3%      15%      7%             22%    0% 100% (n= 2 812)
#> 4 2006      48%        3%      16%      8%             24%    0% 100% (n= 4 510)
#> 5 2008      48%        3%      14%      8%             26%    0% 100% (n= 2 023)
#> 6 2010      44%        3%      17%      9%             28%    0% 100% (n= 2 044)
#> 7 2012      46%        3%      16%      8%             27%    0% 100% (n= 1 974)
#> 8 2014      46%        3%      16%      8%             27%    0% 100% (n= 2 538)
#> 9 Total     47%        3%      16%      8%             25%    0% 100% (n=21 483)
#> # difference (2000): -30 -20 -10 -5 +5 +10 +20 +30
```

``` r
tab(gss_simple, rincome, party3, race, na = "drop", pct = "row", color = TRUE, comp="all")
```

``` r-output
#> # A tabxplor tab: 16 × 6
#> # Groups:         race [4]
#>    race     rincome           `1-Democrat` `2-Independent, other` `3-Republican`
#>    <fct>    <fct>                   <row%>                 <row%>         <row%>
#>  1 White    1-Lt $10000                38%                    26%            36%
#>  2 White    2-$10000 to 14999          40%                    27%            33%
#>  3 White    3-$15000 to 24999          38%                    26%            36%
#>  4 White    4-$25000 or more           39%                    17%            45%
#>  5 White    Total White                39%                    20%            41%
#> 
#>  6 Black    1-Lt $10000                67%                    22%            11%
#>  7 Black    2-$10000 to 14999          76%                    14%            10%
#>  8 Black    3-$15000 to 24999          79%                    15%             6%
#>  9 Black    4-$25000 or more           81%                    12%             7%
#> 10 Black    Total Black                77%                    15%             8%
#> 
#> 11 Other    1-Lt $10000                49%                    31%            20%
#> 12 Other    2-$10000 to 14999          43%                    44%            13%
#> 13 Other    3-$15000 to 24999          45%                    39%            16%
#> 14 Other    4-$25000 or more           56%                    22%            22%
#> 15 Other    Total Other                51%                    30%            19%
#> 
#> 16 Ensemble Total Ensemble             45%                    20%            34%
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30; bg ratio: ÷4 ÷2 ÷1.5 ×1.5 ×2 ×4
```

**A different reference for each variable.** `ref` is reinterpreted by
`pct`. Under **row** percentages (or means) it picks a reference
**row**, so a *named* vector gives each row variable its own — here
`race` is read against its first row, `relig` against its Total:

``` r
tab(gss_simple, c(race, relig), party3, pct = "row", color = "diff",
    ref = c(race = "first", relig = "tot"), na = "drop")
```

``` r-output
#> # A tabxplor tab: 13 × 6
#> # Groups:         row_var [2]
#>    row_var levels              `1-Democrat` `2-Independent, other`
#>    <fct>   <fct>                     <row%>                 <row%>
#>  1 race    White                        39%                    21%
#>  2 race    Black                        76%                    17%
#>  3 race    Other                        49%                    33%
#>  4 race    Total                        45%                    21%
#> 
#>  5 relig   1-Protestant                 43%                    17%
#>  6 relig   2-Catholic                   46%                    22%
#>  7 relig   3-Other christian            42%                    24%
#>  8 relig   4-Jewish                     68%                    12%
#>  9 relig   5-Buddhist/Hinduist          57%                    29%
#> 10 relig   6-Muslim                     66%                    22%
#> 11 relig   7-Other                      48%                    29%
#> 12 relig   8-None                       50%                    31%
#> 13 relig   Total                        45%                    21%
#> # ℹ 2 more variables: `3-Republican` <row%>, Total <row%>
#> # difference (ref.): -30 -20 -10 -5 +5 +10 +20 +30
```

Under **column** percentages `ref` picks a reference **column** instead,
vectorised over the column variables — either named
(`ref = c(party3 = "first", marital = "tot")`) or positional, one value
per column variable:

``` r
tab(gss_simple, race, c(party3, marital), pct = "col", color = "diff",
    ref = c("first", "tot"), na = "drop")
```

``` r-output
#> # A tabxplor tab: 5 × 10
#>   race   `1-Democrat` `2-Independent, other` `3-Republican`    Married Separated
#>   <fct>  <col%-mixed>           <col%-mixed>   <col%-mixed> <col%-mix> <col%-mi>
#> 1 White           66%                    75%            92%        82%       59%
#> 2 Black           24%                    11%             3%         9%       26%
#> 3 Other           10%                    14%             5%         9%       15%
#> 4 Total          100%                   100%           100%       100%      100%
#> 5 n             9 679                  4 512          7 137     10 117       743
#> # ℹ 4 more variables: Divorced <col%-mixed>, Widowed <col%-mixed>,
#> #   `Never married` <col%-mixed>, Total <col%-mixed>
#> # party3: difference (1-Democrat): -30 -20 -10 -5 +5 +10 +20 +30
#> # marital: difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
```

Color thresholds and the palette can be customised : set them **once for
the whole session** with
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
and
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

## colors that respect significance

The colors above show the *size* of a deviation, but not whether it is
**statistically reliable**. On small samples a big-looking difference
can be pure noise. The `color_signif` argument brings significance into
the coloring:

- `"ignore"` (default): color every deviation by its observed size. Grey
  out small differences below a certain threshold.
- `"grey_non_signif"`: color by size of the effect, grey out small
  effects below a certain threshold, but also **grey out cells with
  important effects that are not significant**. Every colored cell is
  then guaranteed to be significantly different from its reference,
  without being bothered by very small significant differences.
- `"guaranteed_effect"`: color only by the part of the effect you can be
  confident about (its confidence bound), with dimmer, conservative
  colors. Use it on **small samples** to **highlight all the differences
  you have the right to interpret**. Everything colored is significant ;
  nothing grey is.

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff", color_signif = "grey_non_signif")
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race  `1-Democrat` `2-Independent, other` `3-Republican`  `NA`           Total
#>   <cha>       <row%>                 <row%>         <row%> <row>          <row%>
#> 1 White          39%                    21%            40%    1% 100% (n=16 395)
#> 2 Black          75%                    16%             8%    1% 100% (n= 3 129)
#> 3 Other          48%                    32%            18%    1% 100% (n= 1 959)
#> 4 Total          45%                    21%            33%    1% 100% (n=21 483)
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30 [grey: non-significant or under ±5 points]
```

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", color = "diff", color_signif = "guaranteed_effect")
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race   `1-Democrat` `2-Independent, other` `3-Republican`  `NA`          Total
#>   <fct>        <row%>                 <row%>         <row%> <row>         <row%>
#> 1 White           40%                    22%            37%    1% 100% (n=1 477)
#> 2 Black           81%                    14%             5%    0% 100% (n=  301)
#> 3 Other           47%                    32%            19%    2% 100% (n=  196)
#> 4 Total           47%                    22%            30%    1% 100% (n=1 974)
#> # difference (Total): -25 -15 -5 -0 +0 +5 +15 +25 [all that is significant is colored, error-adjusted]
```

On **small samples** a big-looking percentage can rest on a handful of
respondents. `n_min =` is a purely visual filter, applied last: it hides
cells whose (unweighted) base is below the threshold and drops a row
entirely when its largest base falls short. Here the rarest religions
drop out:

``` r
tab(gss_simple, relig, race, pct = "row", n_min = 400)
```

``` r-output
#> # A tabxplor tab: 5 × 5
#>   relig              White  Black  Other           Total
#>   <fct>             <row%> <row%> <row%>          <row%>
#> 1 1-Protestant         75%    21%     4% 100% (n=10 846)
#> 2 2-Catholic           78%     4%    18% 100% (n= 5 124)
#> 3 3-Other christian    72%    18%    10% 100% (n=   784)
#> 4 8-None               80%    11%     9% 100% (n= 3 523)
#> 5 Total                76%    15%     9% 100% (n=21 483)
```

An alternative is to keep the small rows and cols but group them all in
a “Other” level :

``` r
tab(gss_simple, relig, race, pct = "row",  other_if_less_than = 400)
```

``` r-output
#> # A tabxplor tab: 7 × 5
#>   relig              White  Black  Other           Total
#>   <fct>             <row%> <row%> <row%>          <row%>
#> 1 1-Protestant         75%    21%     4% 100% (n=10 846)
#> 2 2-Catholic           78%     4%    18% 100% (n= 5 124)
#> 3 3-Other christian    72%    18%    10% 100% (n=   784)
#> 4 8-None               80%    11%     9% 100% (n= 3 523)
#> 5 Others               68%    10%    22% 100% (n= 1 098)
#> 6 NA                   67%    18%    16% 100% (n=   108)
#> 7 Total                76%    15%     9% 100% (n=21 483)
```

## Confidence intervals, tests and contributions

Print confidence intervals for the percentage or mean of each cell with
`ci = "cell"` :

``` r
tab(gss_simple, race, party3, pct = "row", ci = "cell") # by default, conf_level = 0.95
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race   `1-Democrat` `2-Independent, other` `3-Republican`   `NA`
#>   <fct>        <row%>                 <row%>         <row%> <row%>
#> 1 White      [38;40]%               [20;21]%       [39;41]% [0;1]%
#> 2 Black      [73;76]%               [15;18]%         [7;9]% [1;2]%
#> 3 Other      [46;50]%               [30;34]%       [16;20]% [1;2]%
#> 4 Total           45%                    21%            33%     1%
#> # ℹ 1 more variable: Total <row%>
```

Print the confidence intervals of the **difference** with a reference,
used to calculate significance (if 0 belongs to the confidence interval,
the cell is not significantly different from the reference, here the
Total row) :

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
      display = "num_ci" # "{pct} {ci}"
  )
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race    `1-Democrat` `2-Independent, other` `3-Republican`       `NA`
#>   <fct>         <row%>                 <row%>         <row%>     <row%>
#> 1 White  40% [-10;-3]%          22%   [-3;3]% 37%    [4;10]% 1% [-1;1]%
#> 2 Black  81%  [28;38]%          14% [-12;-3]%  5% [-28;-22]% 0% [-1;1]%
#> 3 Other  47%   [-7;7]%          32%   [4;17]% 19%  [-17;-5]% 2% [-0;4]%
#> 4 Total            47%                    22%            30%         1%
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -25 -15 -5 -0 +0 +5 +15 +25; bg ratio: ÷2.667 ÷1.333 ÷1 ×1 ×1.333 ×2.667 [all that is significant is colored, error-adjusted]
```

`display = "num_ci"` is a type-adaptive shorthand for this: it shows
each value with whatever confidence interval the table computes —
`{pct} {ci}` on percentage columns and `{mean} {ci}` on numeric columns,
chosen per column — so it works for a mix of factors and numbers in one
call:

Add significance stars with `stars = TRUE`. They tell the same story
than confidence intervals of the difference with the reference, but for
different confidence levels (99%, 95%, 90%) :

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(rincome, c(party3, tvhours), pct = "row", display = "num_ci", stars = TRUE)
```

``` r-output
#> # A tabxplor tab: 6 × 7
#>   rincome              `1-Democrat` `2-Independent, other` `3-Republican`
#>   <fct>                      <row%>                 <row%>         <row%>
#> 1 1-Lt $10000       40%** [-15;-1]%        33%***  [5;18]% 27%    [-9;3]%
#> 2 2-$10000 to 14999 45%    [-12;7]%        27%    [-2;15]% 26%   [-12;5]%
#> 3 3-$15000 to 24999 50%    [-5;10]%        25%    [-2;10]% 25%   [-12;1]%
#> 4 4-$25000 or more  49%     [-2;7]%        16%*** [-9;-2]% 35%**   [0;8]%
#> 5 NA                47%     [-4;4]%        22%     [-3;4]% 30%    [-4;3]%
#> 6 Total                       47%                   22%             30%  
#> # ℹ 3 more variables: `NA` <row%>, Total <row%>, tvhours <mean>
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

**Show two numbers in one cell.** `display` is not just for confidence
intervals: it takes a **[`{}`](https://rdrr.io/r/base/Paren.html)
template** that combines any cell fields. For example
`display = "{pct} ({diff})"` prints each percentage followed by its
difference from the reference, and `"{pct} (n={n})"` follows it with the
count:

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff", display = "{pct} ({diff})")
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race   `1-Democrat` `2-Independent, other` `3-Republican`     `NA`
#>   <fct>        <row%>                 <row%>         <row%>   <row%>
#> 1 White    39% ( -6%)             21% ( -0%)     40% ( +7%) 1% (-0%)
#> 2 Black    75% (+30%)             16% ( -5%)      8% (-26%) 1% (+0%)
#> 3 Other    48% ( +3%)             32% (+11%)     18% (-15%) 1% (+1%)
#> 4 Total    45% ( +0%)             21% ( +0%)     33% ( +0%) 1% (+0%)
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
```

The first field in the template is the *primary* one — the value Excel
keeps and the one the colours read.
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor-programming.md)
explains the full grammar and lists every field you can combine.

`test = TRUE` adds a statistical test of independence per (sub-)table —
**Chi-squared** for factor columns, **Welch’s F ANOVA** for numeric
variables (`options(tabxplor.anova = "classic")` switches to the pooled
F):

``` r
tab(gss_simple, race, c(party3, tvhours), pct = "row", test = TRUE)
```

``` r-output
#> |      | Tests                  |   party3 |   |     tvhours |
#> |:-----|:-----------------------|---------:|:-:|------------:|
#> | race | N                      |   21 483 |   |      11 337 |
#> |      | pvalue (Chi2, Welch F) |   <0.01% |   |      <0.01% |
#> |      | Cramér's V, eta2       | V = 0.21 |   | eta2 = 0.04 |
#> 
#> # A tabxplor tab: 4 × 7
#>   race  `1-Democrat` `2-Independent, other` `3-Republican`  `NA`           Total
#>   <cha>       <row%>                 <row%>         <row%> <row>          <row%>
#> 1 White          39%                    21%            40%    1% 100% (n=16 395)
#> 2 Black          75%                    16%             8%    1% 100% (n= 3 129)
#> 3 Other          48%                    32%            18%    1% 100% (n= 1 959)
#> 4 Total          45%                    21%            33%    1% 100% (n=21 483)
#> # ℹ 1 more variable: tvhours <mean>
```

`color = "contrib"` colors cells by their **contribution to the
Chi-squared** — the cells that would stand out in a correspondence
analysis :

``` r
tab(gss_simple, race, party3, color = "contrib")
```

``` r-output
#> |      | Tests         |   party3 |
#> |:-----|:--------------|---------:|
#> | race | N             |   21 483 |
#> |      | pvalue (Chi2) |   <0.01% |
#> |      | Cramér's V    | V = 0.21 |
#> 
#> # A tabxplor tab: 4 × 6
#>   race   `1-Democrat` `2-Independent, other` `3-Republican` `NA`  Total
#>   <fct>           <n>                    <n>            <n>  <n>    <n>
#> 1 White         6 390                  3 365          6 546   94 16 395
#> 2 Black         2 344                    513            236   36  3 129
#> 3 Other           945                    634            355   25  1 959
#> 4 Total         9 679                  4 512          7 137  155 21 483
#> # contribution to Chi2 (vs the mean): ×10 ×5 ×2 ×1 ×1 ×2 ×5 ×10
```

``` r
# tab(gss_simple, race, party3, pct="row", color = "contrib") # works with pct, but independent from rows/columns
```

See below for the detail of how confidence intervals and colors can be
composed.

**A note on weights.** With a weight (`wt =`), every proportion or mean
is weighted, but by default the sample size behind the confidence
intervals and tests stays the real, **unweighted** number of
observations. Under unequal weights it carries no design effect, so it
runs a little too narrow : opt in to Kish’s effective sample size with
`options(tabxplor.kish_neff = TRUE)` (see [Weights](#weights)) to widen
every interval honestly and switch the whole-table Chi2 tests to a
Rao–Scott correction.

## Exporting

A finished table exports with its colors to Excel, HTML or Markdown:

``` r
tabs <- tab(gss_simple, race, party3, pct = "row", color = "diff")
tab_export(tabs) # default : html table (RStudio Viewer, .Rmd/.qmd, etc.)
tab_export(tabs, format = "xl", path = "table") # Excel export 
tab_export(tabs, format = "md", path = "table") # flat markdown file (pipes tables)
```

Two options are worth knowing:

- `theme = "auto"` lets an HTML or Markdown export **follow the reader’s
  light/dark mode** (it flips live). For the console,
  `set_color_palette(theme = "auto")` detects the editor (RStudio,
  Positron, etc.) and picks the matching palette — it is applied
  automatically when the package loads.

``` r
tab_export(tabs, theme = "auto") # HTML that follows the reader's light/dark modes
```

- Since numeric variables can only be passed in columns, some complex
  layout with numeric variables in rows need to transpose the table
  during export using `transpose = TRUE` :

``` r
tab(gss_simple, party3, c(race, relig, tvhours), pct = "row") |>
  tab_export(transpose = TRUE)
```

- **One stylesheet for a whole document.** In an `.Rmd`/`.qmd` report,
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_css.md)
  writes the colour CSS once and every later
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)
  emits only classes, so a single `theme` — including `"auto"`, which
  follows the reader’s light/dark mode — styles every table at once:

``` r
options(tabxplor.tab_kable_css = FALSE)
tab_css(theme = "auto")   # emit once, near the top of the document
```

Nothing is written inline on a cell, so any look is overridable with
plain CSS afterwards (column widths, fonts…); see
[`?tab_css`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_css.md)
for the role classes (`.tx-rv`, `.tx-tot`, `.tx-num`).

## Working with the result

[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
returns a `tibble` (of class `tabxplor_tab`), so `dplyr` verbs just
work. Use the helper
[`is_totrow()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
to keep the Total row in place when you re-order (it flags total rows,
so sorting on it first sends them to the bottom):

``` r
library(dplyr)
tab(gss_simple, race, marital, pct = "row") |>
  arrange(desc(Married))
```

``` r-output
#> # A tabxplor tab: 4 × 8
#>   race  Married Separated Divorced Widowed `Never married`  `NA`           Total
#>   <cha>  <row%>    <row%>   <row%>  <row%>          <row%> <row>          <row%>
#> 1 White     51%        3%      16%      9%             21%    0% 100% (n=16 395)
#> 2 Other     48%        6%      11%      4%             32%    0% 100% (n= 1 959)
#> 3 Black     28%        6%      16%      8%             42%    0% 100% (n= 3 129)
#> 4 Total     47%        3%      16%      8%             25%    0% 100% (n=21 483)
```

**Titling and annotating.** `subtext =` prints one or more legend lines
under a table (a data source, a note).
[`set_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)
gives a table a **title that survives a dplyr pipeline**, and every
exporter uses it as the table title:

``` r
tab(gss_simple, race, marital, pct = "row", subtext = "Source: GSS, 2000-2014") |>
  set_caption("Custom title")
```

## Confidence intervals and colors composition : variable_type × `color` × `color_signif`

This section is the reference behind the two color sections above. It
shows how the parameters fit together — the **variable type**, the
**measure** you color (`color`), and the **significance policy**
(`color_signif`).

The **type** is set by the column variable — a **factor** (percentages
are computed, `pct`) or a **numeric** (means are computed, `mean`). The
row variable is always turned into a factor.

Every colored table answers three questions:

- **How to measure deviation?** `color =` picks what a color *means*:
  `"diff"` (distance from the reference), `"ratio"` (relative risk for
  percentages, mean ratio for means), `"contrib"` (weight in the
  Chi-squared), `"OR"` (odds ratio). `color = TRUE` picks a sensible one
  per column type.
- **How confident are we in this measure?** every color reads one
  **confidence interval** at `conf_level` (95% by default). A cell is
  *significant* when that interval excludes its neutral value — **0**
  for a difference, **1** for a ratio or odds ratio. The printed
  bracket, the significance stars and the greying all read that same
  interval, so they can never disagree.
- **How to show significance?** `color_signif` — `"ignore"`,
  `"grey_non_signif"` or `"guaranteed_effect"` ; `stars = TRUE` to use
  significance stars instead of, or stacked with, colors.

**The confidence interval used for the colors and stars** compares each
cell to its reference cell (by default, the corresponding cell in the
Total row or Total column):

| type | color | what the color measures | confidence interval (default) |
|----|----|----|----|
| pct | `diff` | cell % - reference % (percentage points) | Newcombe hybrid-score |
| pct | `ratio` | cell % / reference % (relative risk) | Katz log-risk-ratio |
| pct | `OR` | empirical odds ratio | Woolf log-odds-ratio |
| pct | `contrib` | signed χ² contribution (no reference) | — (standardized residual) |
| mean | `diff` | cell mean - reference mean (SD units) | Welch *t* |
| mean | `ratio` | cell mean / reference mean | robust ratio-of-means |

Alternative interval methods (see
[`?tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md)): -
`method_diff = "ac"` (Agresti-Caffo) or `"wald"` for a percentage
difference; - `method_mean_diff = "student"` (pooled, the OLS two-group
interval) for a mean difference; - `method_mean_ratio = "quasipoisson"`
or `"poisson"` for a mean ratio. - The relative-risk
(`method_ratio = "katz"`) and odds-ratio (Woolf) intervals have no
alternative. - A mean **difference** is colored **standardized** —
Glass’s Δ, the difference divided by the reference’s standard deviation
— so the `mean_diff` color breaks are read in SD units, unless the user
provides a custom break scale. - For a factor with **3 or more levels**,
the odds ratio (and its interval) compares each level to the `ref2`
baseline level : it is a relative risk ratio (RRR) (the same observed
quantity that is modelised by a multinomial logistic regression). The OR
interval is computed only when `color_signif` or `stars` needs it.

**Simple, cell-by-cell confidence intervals** (`ci = "cell"`) compare
each cell to 0 % (or a mean of 0), *not* to a reference:

| type | color  | confidence interval (default) | other method                    |
|------|--------|-------------------------------|---------------------------------|
| pct  | `cell` | Wilson score interval         | `method_cell = "wald"` (normal) |
| mean | `cell` | one-sample Student *t* (n-1)  | —                               |

Because they compare to 0 and not to a reference, cell intervals are
purely descriptive: they carry **no significance and no stars**.
`method_cell` chooses `"wilson"` (default) or `"wald"` for percentages;
a mean cell interval is always the one-sample Student *t*.

**`color_signif` turns that interval into a coloring policy.** -
`"ignore"` colors **every** cell by its **observed effect** size, for
example the observed difference compared to the Total row. Grey cells
have an observed effect below the threshold (for example, differences of
less than ±5 points of percentages). - `"grey_non_signif"` and
`"guaranteed_effect"` both color **only significant cells**, but differ
in the *intensity basis*: + `grey_non_signif` colors by the **observed**
effect, like “ignore”, greying out small deviations, but it also **greys
out any large deviation that turns to be non-significant**. Ideal for
large samples. + `guaranteed_effect` colors by the **guaranteed** effect
— the confidence-bound (CI-floor), the **smaller deviation that is
assured at a given confidence level** (default 95%) — so its colors are
dimmer and conservative, but **all significative differences are
colored**, which is ideal for small samples.

| type | color | `="ignore"` | `="grey_non_signif"` | `="guaranteed_effect"` |
|----|----|----|----|----|
| pct | `diff` | observed diff | grey if the diff-CI contains 0 | diff CI-floor |
| pct | `ratio` | observed ratio | grey if the ratio-CI contains 1 | ratio CI-floor |
| pct | `OR` | observed OR | grey if the OR-CI contains 1 | OR CI-floor |
| pct | `contrib` | χ² contribution | grey if residual \< 1.96 (conf 95%) | residual \>= 1.96 (conf 95%) |
| mean | `diff` | observed diff | grey if the diff-CI contains 0 | diff CI-floor |
| mean | `ratio` | observed ratio | grey if the ratio-CI contains 1 | ratio CI-floor |

Examples :

``` r
# --- factors: percentages -------------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "diff",  color_signif = "grey_non_signif")
tab(gss_simple, race, party3, pct = "row", color = "ratio", color_signif = "guaranteed_effect")
tab(gss_simple, rincome, married, pct = "row", color = "OR", OR = TRUE, ref2 = 1)
tab(gss_simple, rincome, party3, color = "contrib")   # works with pct = "row"/"col" too

# --- numerics: means ------------------------------------------------------
tab(gss_simple, rincome, tvhours, color = "diff",  color_signif = "guaranteed_effect")
tab(gss_simple, rincome, tvhours, color = "ratio", color_signif = "grey_non_signif")

#    a custom scale for differences in means, and a "first row" reference
tab(gss_simple, rincome, tvhours, color = "diff", color_signif = "grey_non_signif",
    color_breaks = list(mean_diff = c(0.4, 0.8, 1.6)), ref = 1)
```

## Session options

A handful of [`options()`](https://rdrr.io/r/base/options.html) set your
preferred defaults once for the whole session — put them at the top of a
script, or in your `.Rprofile`. Each one has a per-call argument too;
the option just changes the default. The everyday ones:

- `options(tabxplor.print = "html")` — print tables not in console, but
  as html in RStudio or Positron Viewer Pane by default (recommended)
- `options(tabxplor.cleannames = TRUE)` — strip `"1-"`-style prefixes
  from level names everywhere.
- `options(tabxplor.parallel = 8)` — parallelise tables with multiples
  variables on different CPU cores by default (needs `mirai`)
- `options(tabxplor.var_labels = TRUE)` — in exports, show a variable’s
  label (from `haven`/`labelled` data) instead of its bare name.
- `options(tabxplor.theme = "auto")` — the export theme
  (`"light"`/`"dark"`/`"auto"`); `set_color_palette(theme = "auto")`
  does the same for the console.
- `options(tabxplor.stars = TRUE)` — show significance stars in every
  table (like `stars = TRUE`).
- `options(tabxplor.conf_level = 0.9)` — the confidence level for
  intervals and tests (default `0.95`).
- `options(tabxplor.ci_print = "moe")` — print a confidence interval as
  a `pct ± margin of error` instead of a `[low; high]` bracket.
- `options(tabxplor.lang = "fr")` — the language of the colour legends
  and footers (`"auto"`/`"en"`/`"fr"`).

Colour thresholds and palettes have their own helpers,
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
and
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).
`?tabxplor-options` documents every option, and
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor-programming.md)
covers the more advanced ones (export fonts, parallel builds…).

## A point-and-click interface (jamovi)

Everything above is also available **without writing R code**, through a
[jamovi](https://www.jamovi.org/download) module. jamovi is a free,
open-source statistical software : install it, open the modules menu
(the **`+`** at the top-right), choose **jamovi library**, and install
*tabxplor*. It adds a **Crosstables** analysis — and a **Regressions**
analysis powered by
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
— with the same coloured, exportable tables, driven entirely by menus.
Handy for teaching, or for colleagues who do not use R.

## Where to go next

- [`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor-reg.md)
  — regression tables with
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md),
  and comparing modelled to observed effects.
- [`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor-programming.md)
  — the `tabxplor_fmt` cell type and how to program with its fields.
- [`?tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  for every argument (grouped by purpose),
  [`?tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md)
  for confidence-interval methods, and `?tabxplor-options` for the
  package-wide defaults.
