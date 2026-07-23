
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabxplor

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tabxplor)](https://CRAN.R-project.org/package=tabxplor)
[![R-CMD-check](https://github.com/BriceNocenti/tabxplor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BriceNocenti/tabxplor/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`tabxplor` makes cross-tables readable at a glance, for data
exploration. One line of code builds a table with percentages, weighted
counts, confidence intervals and tests — and **colors highlight the
cells that stand out from the total, only when the difference is
statistically solid**. You spot the structure of your data immediately,
instead of scanning numbers row by row.

What sets it apart:

- **Colors encode effect size *and* significance** at once: the stronger
  the difference, the deeper the color; non-significant cells stay
  uncolored (or greyed).
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

<!-- TODO maintainer: recapture this hero screenshot with the 2.0.0 palette and legend -->

<figure>
<img src="man/figures/README-hero.jpg"
alt="A color-coded tabxplor cross-table in the console" />
<figcaption aria-hidden="true">A color-coded tabxplor cross-table in the
console</figcaption>
</figure>

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
.u4{background-color:#FFBAAF;}
</style>

*Note for GitHub readers: the tables below lose their colors here — see
the [package website](https://bricenocenti.github.io/tabxplor/) for the
full colored version.*

## Installation

``` r
install.packages("tabxplor")

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

tab(gss, marital, race, pct = "row", color = "diff")
```

<table class="tabxplor-tab">

<thead>

<tr>

<th class="tx-span" colspan="1">

</th>

<th class="tx-span" colspan="3">

race
</th>

<th class="tx-span" colspan="1">

</th>

</tr>

<tr>

<th class="tx-l tx-br tx-bl tx-rv">

marital
</th>

<th class="tx-r tx-num">

White
</th>

<th class="tx-r tx-num">

Black
</th>

<th class="tx-r tx-num">

Other
</th>

<th class="tx-r tx-num tx-br tx-bl tx-tot">

Total
</th>

</tr>

</thead>

<tbody>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Married
</td>

<td class="tx-r tx-num p1 tx-b">

82%
</td>

<td class="tx-r tx-num m1 tx-b">

9%
</td>

<td class="tx-r tx-num g1">

9%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span style="font-weight:normal;"> (n=10 117)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Separated
</td>

<td class="tx-r tx-num m2 tx-b">

59%
</td>

<td class="tx-r tx-num p2 tx-b">

26%
</td>

<td class="tx-r tx-num p1 tx-b">

15%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span style="font-weight:normal;"> (n=   743)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Divorced
</td>

<td class="tx-r tx-num g1">

79%
</td>

<td class="tx-r tx-num g1">

15%
</td>

<td class="tx-r tx-num g1">

6%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span style="font-weight:normal;"> (n= 3 383)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Widowed
</td>

<td class="tx-r tx-num p1 tx-b">

82%
</td>

<td class="tx-r tx-num g1">

14%
</td>

<td class="tx-r tx-num m1 tx-b">

4%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span style="font-weight:normal;"> (n= 1 807)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Never married
</td>

<td class="tx-r tx-num m2 tx-b">

64%
</td>

<td class="tx-r tx-num p1 tx-b">

24%
</td>

<td class="tx-r tx-num g1">

12%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span style="font-weight:normal;"> (n= 5 416)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

NA
</td>

<td class="tx-r tx-num g1">

76%
</td>

<td class="tx-r tx-num g1">

12%
</td>

<td class="tx-r tx-num g1">

12%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span style="font-weight:normal;"> (n=    17)</span>
</td>

</tr>

<tr class="tx-b tx-bt tx-bb tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

Total
</td>

<td class="tx-r tx-num tx-b">

76%
</td>

<td class="tx-r tx-num tx-b">

15%
</td>

<td class="tx-r tx-num tx-b">

9%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

100%<span style="font-weight:normal;"> (n=21 483)</span>
</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="5">

<div class="tx-foot">

Shades of blue: cells ≥ the Total row <span class="p1"
style="font-weight:bold;">+5</span>; <span class="p2"
style="font-weight:bold;">+10</span>; <span class="p3"
style="font-weight:bold;">+20</span>; <span class="p4"
style="font-weight:bold;">+30</span> points. Shades of yellow to red:
cells ≤ the Total row <span class="m1"
style="font-weight:bold;">-5</span>; <span class="m2"
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
tab(gss, relig, c(married, income25k), pct = "row", levels = "first",
    color = "diff", color_signif = "grey_non_signif")
```

<table class="tabxplor-tab">

<thead>

<tr>

<th class="tx-span" colspan="1">

</th>

<th class="tx-span" colspan="1">

married
</th>

<th class="tx-span" colspan="1">

income25k
</th>

</tr>

<tr>

<th class="tx-l tx-br tx-bl tx-rv">

relig
</th>

<th class="tx-r tx-num tx-br">

01-Married
</th>

<th class="tx-r tx-num tx-br">

01-\$25000 or<br>more
</th>

</tr>

</thead>

<tbody>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

1-Protestant
</td>

<td class="tx-r tx-num tx-br g1">

50%
</td>

<td class="tx-r tx-num tx-br g1">

32%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

2-Catholic
</td>

<td class="tx-r tx-num tx-br g1">

50%
</td>

<td class="tx-r tx-num tx-br g1">

35%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

3-Other christian
</td>

<td class="tx-r tx-num tx-br g1">

44%
</td>

<td class="tx-r tx-num tx-br g1">

35%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

4-Jewish
</td>

<td class="tx-r tx-num tx-br g1">

51%
</td>

<td class="tx-r tx-num tx-br p1 tx-b">

43%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

5-Buddhist/Hinduist
</td>

<td class="tx-r tx-num tx-br g1">

51%
</td>

<td class="tx-r tx-num tx-br p2 tx-b">

47%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

6-Muslim
</td>

<td class="tx-r tx-num tx-br g1">

53%
</td>

<td class="tx-r tx-num tx-br g1">

32%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

7-Other
</td>

<td class="tx-r tx-num tx-br m1 tx-b">

37%
</td>

<td class="tx-r tx-num tx-br g1">

37%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

8-None
</td>

<td class="tx-r tx-num tx-br m2 tx-b">

37%
</td>

<td class="tx-r tx-num tx-br g1">

37%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

NA
</td>

<td class="tx-r tx-num tx-br g1">

45%
</td>

<td class="tx-r tx-num tx-br m2 tx-b">

15%
</td>

</tr>

<tr class="tx-b tx-bt tx-bb tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

Total
</td>

<td class="tx-r tx-num tx-br tx-b">

47%
</td>

<td class="tx-r tx-num tx-br tx-b">

34%
</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="3">

<div class="tx-foot">

Shades of blue: cells ≥ the Total row <span class="p1"
style="font-weight:bold;">+5</span>; <span class="p2"
style="font-weight:bold;">+10</span>; <span class="p3"
style="font-weight:bold;">+20</span>; <span class="p4"
style="font-weight:bold;">+30</span> points. Shades of yellow to red:
cells ≤ the Total row <span class="m1"
style="font-weight:bold;">-5</span>; <span class="m2"
style="font-weight:bold;">-10</span>; <span class="m3"
style="font-weight:bold;">-20</span>; <span class="m4"
style="font-weight:bold;">-30</span> points. Coloured: significantly
different from the Total row (Newcombe score interval, 95% confidence),
by at least the first colour threshold. Uncoloured: either not
significant, or a difference under ±5 points.

</div>

</td>

</tr>

</tfoot>

</table>

The same visual language extends to regression models: `tab_reg()`
detects a binary outcome and fits a logistic regression, coloring odds
ratios by strength and greying the non-significant ones.

``` r
tab_reg(gss, dependent = "married", predictors = c("race", "age", "rincome"))
```

<div class="tabxplor-caption">

Logistic regression: married by race, age +1 more

</div>

<table class="tabxplor-tab tx-has-stars">

<thead>

<tr>

<th class="tx-span" colspan="1">

</th>

<th class="tx-span" colspan="1">

married: 01-Married
</th>

</tr>

<tr>

<th class="tx-l tx-br tx-bl tx-rv">

levels
</th>

<th class="tx-r tx-num tx-br">

Model_OR
</th>

</tr>

</thead>

<tbody>

<tr class="tx-b tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

Reference population
</td>

<td class="tx-r tx-num tx-br tx-b">

1/4.09\*\*\*
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

White
</td>

<td class="tx-r tx-num tx-br tx-b">

1   
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Black
</td>

<td class="tx-r tx-num tx-br m3 tx-b">

1/2.22\*\*\*
</td>

</tr>

<tr class="tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

Other
</td>

<td class="tx-r tx-num tx-br g1">

1.08   
</td>

</tr>

<tr class="tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

age
</td>

<td class="tx-r tx-num tx-br g1">

1.03\*\*\*
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

1-Lt \$10000
</td>

<td class="tx-r tx-num tx-br tx-b">

1   
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

2-\$10000 to 14999
</td>

<td class="tx-r tx-num tx-br g1">

1.15\*  
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

3-\$15000 to 24999
</td>

<td class="tx-r tx-num tx-br p1 tx-b">

1.28\*\*\*
</td>

</tr>

<tr class="tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

4-\$25000 or more
</td>

<td class="tx-r tx-num tx-br p2 tx-b">

1.85\*\*\*
</td>

</tr>

<tr class="tx-b tx-bt">

<td class="tx-l tx-br tx-bl tx-rv">

N
</td>

<td class="tx-r tx-num tx-br tx-b">

12 990
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

LR vs null
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

0.049
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

AIC
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

17 181
</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="2">

<div class="tx-foot">

Model: logistic regression; odds ratios (vs the reference
category).<br>Shades of blue: OR ≥ <span class="p1"
style="font-weight:bold;">1.2</span>; <span class="p2"
style="font-weight:bold;">1.5</span>; <span class="p3"
style="font-weight:bold;">2</span>; <span class="p4"
style="font-weight:bold;">4</span>. Shades of yellow to red: OR ≤
<span class="m1" style="font-weight:bold;">1/1.2</span>;
<span class="m2" style="font-weight:bold;">1/1.5</span>;
<span class="m3" style="font-weight:bold;">1/2</span>; <span class="m4"
style="font-weight:bold;">1/4</span>. Coloured: significantly different
from the reference category (Wald interval on the log odds-ratio, 95%
confidence), by at least the first colour threshold. Uncoloured: either
not significant, or a difference under ×1.2.<br>***: significantly
different from the reference category (in bold) at the 99% confidence
level;** : at the 95% level;* : at the 90% level; no star: not
significant.

</div>

</td>

</tr>

</tfoot>

</table>

## Export your tables

Any table exports with its colors to Excel, html or markdown, and can be
drawn as a plot:

``` r
tab(gss, marital, race, pct = "row", color = "diff") |> tab_xl()      # Excel
tab(gss, marital, race, pct = "row", color = "diff") |> tab_export("html")
```

## Learn more

- [Introduction to
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor.html)
  — the place to start (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-fr.html)*).
- [Regression tables with
  tab_reg()](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html)
  (*[en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg-fr.html)*).
- [Programming with
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.html)
  — many tables at once, custom workflows, options (*[en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming-fr.html)*).

No code needed: the tabxplor modules for the free statistical
spreadsheet [jamovi](https://www.jamovi.org/) offer the same tables
(Crosstables and Regressions) in a point-and-click interface.
