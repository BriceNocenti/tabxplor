
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabxplor

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tabxplor)](https://CRAN.R-project.org/package=tabxplor)
[![R-CMD-check](https://github.com/BriceNocenti/tabxplor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BriceNocenti/tabxplor/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/BriceNocenti/tabxplor/graph/badge.svg)](https://app.codecov.io/gh/BriceNocenti/tabxplor)
<!-- badges: end -->

`tabxplor` makes cross-tables and regression models readable at a glance
for data exploration. It builds a table with percentages, weighted
counts, confidence intervals, tests — and colors highlight the cells
that stand out from the total or reference, only when the difference is
statistically solid, to spot the structure of your data immediately.

- **Colors encode effect size and significance**: the stronger the
  difference, the deeper the color; non-significant cells are
  greyed-out.
- Html, Excel and markdown/Quarto exports are available.
- It comes with a point-and-click [jamovi](https://www.jamovi.org/)
  graphical interface: no code needed.
- A black-and-white `theme = "print_ready"` renders the same reading for
  journals.
- **Regression models** are presented with the same visual language,
  next to their observed effect.
- In R the tables **are `tibble`s you can keep working on with
  `dplyr`**. Cells are rich values, each one carries its count,
  percentage, confidence interval and reference behind the displayed
  number.
- Weighted data and survey design are supported.

![A color-coded tabxplor cross-table in the
console](man/figures/README-hero.jpg)

<style>
.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
.tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
.tabxplor-tab{margin-bottom:1.2em;}
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
.tabxplor-tab .tx-foot{width:0;min-width:100%;padding-bottom:5px;}
.tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;margin:0 -4px;}
.tabxplor-tab .tx-spark{display:block;margin:0 auto;}
.tabxplor-tab .tx-sparkcell{vertical-align:middle;text-align:center;padding:1px 2px;}
.tabxplor-tab.tx-shape{font-size:90%;}
.tooltip-inner{max-width:none;white-space:pre;}
.popover{max-width:none;}
.popover-body,.popover-content{padding:6px;white-space:pre;}
.tabxplor-tab{color:#000000;background:#ffffff;}
.tabxplor-tab th,.tabxplor-tab td{color:#000000;background-color:#ffffff;border-color:#000000;}
.tabxplor-tab tbody tr:hover{background:transparent;}
.g1,.tabxplor-tab .g1{color:#888888;}
.g2,.tabxplor-tab .g2{color:#444444;}
.tabxplor-tab .tx-unit{color:#888888;}
.tabxplor-caption{color:#000000;}
.tabxplor-tab .tx-foot{color:#444444;}
.tabxplor-tab.tx-shape{color:#444444;}
.tabxplor-tab.tx-shape thead th{color:#444444;}
.tabxplor-tab.tx-shape .tx-sec{color:#888888;}
.tabxplor-tab .tx-sec{color:#444444;font-style:normal;text-decoration:none;display:inline-block;}
.tabxplor-tab .tx-mark{color:#000000;font-style:normal;text-decoration:none;display:inline-block;}
.p1,.tabxplor-tab .p1{color:#000000;font-weight:normal;}
.p2,.tabxplor-tab .p2{color:#000000;}
.p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
.p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline double;}
.m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
.m2,.tabxplor-tab .m2{color:#000000;font-style:italic;}
.m3,.tabxplor-tab .m3{color:#000000;font-style:italic;text-decoration:underline;}
.m4,.tabxplor-tab .m4{color:#000000;font-style:italic;text-decoration:underline double;}
.o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
.o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
.o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
.o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
.u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
.u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
.u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
.u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
@media print {
  .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
}
</style>

*The tables below are shown in tabxplor’s publication-ready
black-and-white scheme (`theme = "print_ready"`): a superscript `+` or
`-` after a cross-table cell, repeated once more at each threshold, and
a bold / underlined ladder on the regression table. **Colors are the
default**, and the right choice for exploring — github strips them from
a README, see [package
website](https://bricenocenti.github.io/tabxplor/).*

## Installation

``` r
install.packages("tabxplor", dependencies = TRUE)

# Development version:
# install.packages("devtools")
devtools::install_github("BriceNocenti/tabxplor")
```

## A quick look

A simple cross-table with row percentages: shades of blue mean the cell
is over-represented compared to the total row, shades of yellow to red
mean it is under-represented.

``` r
gss <- gss_cat_data_formatting() # a cleaned-up version of forcats::gss_cat

tab(gss, race, party3, pct = "row", color = "difference")
```

<table class="tabxplor-tab tx-has-stars">

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

<td class="tx-r tx-num m1">

39%<span class="tx-mark">⁻</span><span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num g1">

21%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num p1">

40%<span class="tx-mark">⁺</span><span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num g1">

1%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

<b>100%</b><span class="tx-sec"
style="font-weight:normal;"> (16 395)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Black
</td>

<td class="tx-r tx-num p3">

<u>75%</u><span class="tx-mark">⁺⁺⁺</span>
</td>

<td class="tx-r tx-num g1">

16%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num m3">

<u>8%</u><span class="tx-mark">⁻⁻⁻</span>
</td>

<td class="tx-r tx-num g1">

1%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

<b>100%</b><span class="tx-sec"
style="font-weight:normal;"> ( 3 129)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Other
</td>

<td class="tx-r tx-num g1">

48%<span class="tx-sec">   </span>
</td>

<td class="tx-r tx-num p2">

32%<span class="tx-mark">⁺⁺</span>
</td>

<td class="tx-r tx-num m2">

18%<span class="tx-mark">⁻⁻</span><span class="tx-sec"> </span>
</td>

<td class="tx-r tx-num g1">

1%
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

<b>100%</b><span class="tx-sec"
style="font-weight:normal;"> ( 1 959)</span>
</td>

</tr>

<tr class="tx-b tx-bt tx-bb tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

<b>Total</b>
</td>

<td class="tx-r tx-num tx-b">

<b>45%</b><span class="tx-sec" style="font-weight:normal;">   </span>
</td>

<td class="tx-r tx-num tx-b">

<b>21%</b><span class="tx-sec" style="font-weight:normal;">  </span>
</td>

<td class="tx-r tx-num tx-b">

<b>33%</b><span class="tx-sec" style="font-weight:normal;">   </span>
</td>

<td class="tx-r tx-num tx-b">

<b>1%</b>
</td>

<td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">

<b>100%</b><span class="tx-sec"
style="font-weight:normal;"> (21 483)</span>
</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="6">

<div class="tx-foot">

Percentage points (risk) difference: cell ≥ the Total row
<span class="p1" style="font-weight:normal;">+5⁺</span>;
<span class="p2" style="font-weight:normal;">+10⁺⁺</span>;
<span class="p3"
style="font-weight:normal;text-decoration:underline;"><u>+20⁺⁺⁺</u></span>;
<span class="p4"
style="font-weight:normal;text-decoration:underline;"><u>+30⁺⁺⁺⁺</u></span>
points; cell ≤ the Total row <span class="m1"
style="font-weight:normal;">-5⁻</span>; <span class="m2"
style="font-weight:normal;">-10⁻⁻</span>; <span class="m3"
style="font-weight:normal;text-decoration:underline;"><u>-20⁻⁻⁻</u></span>;
<span class="m4"
style="font-weight:normal;text-decoration:underline;"><u>-30⁻⁻⁻⁻</u></span>
points.

</div>

</td>

</tr>

</tfoot>

</table>

Several column variables can be crossed at once for series of Yes/No
survey questions. With `color_signif = "grey_non_signif"`, cells that
are not significantly different from the total are greyed out, so every
colored figure is a solid one. Use `wt =` for weighted or survey data.
Example with [FactoMineR](http://factominer.free.fr/index_fr.html) tea
data :

``` r
tea_when_vars <- c("breakfast", "tea.time", "evening", "lunch", "dinner", "always")
tab(facto_tea, SPC, all_of(tea_when_vars), pct = "row", 
    levels = "first", na = "drop", 
    color = "difference", ref = "first", color_signif = "grey_non_signif")
```

<table class="tabxplor-tab tx-has-stars">

<thead>

<tr>

<th class="tx-span" colspan="2">

</th>

<th class="tx-span" colspan="1">

breakfast
</th>

<th class="tx-span" colspan="1">

tea.time
</th>

<th class="tx-span" colspan="1">

evening
</th>

<th class="tx-span" colspan="1">

lunch
</th>

<th class="tx-span" colspan="1">

dinner
</th>

<th class="tx-span" colspan="1">

always
</th>

</tr>

<tr>

<th class="tx-l tx-br tx-bl tx-rv" rowspan="2">

SPC
</th>

<th class="tx-r tx-num tx-br">

n
</th>

<th class="tx-r tx-num tx-br">

breakfast_lv
</th>

<th class="tx-r tx-num tx-br">

tea time
</th>

<th class="tx-r tx-num tx-br">

evening_lv
</th>

<th class="tx-r tx-num tx-br">

lunch_lv
</th>

<th class="tx-r tx-num tx-br">

dinner_lv
</th>

<th class="tx-r tx-num tx-br">

always_lv
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

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-rv">

<b>employee</b>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>59</b>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>49%</b>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>53%</b>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>44%</b><span class="tx-sec" style="font-weight:normal;">   </span>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>7%</b><span class="tx-sec" style="font-weight:normal;">  </span>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>14%</b><span class="tx-sec" style="font-weight:normal;">  </span>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>34%</b>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

middle
</td>

<td class="tx-r tx-num tx-br g2">

40
</td>

<td class="tx-r tx-num tx-br g1">

60%
</td>

<td class="tx-r tx-num tx-br g1">

48%
</td>

<td class="tx-r tx-num tx-br g1">

30%<span class="tx-sec">   </span>
</td>

<td class="tx-r tx-num tx-br g1">

5%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br m2">

0%<span class="tx-mark">⁻⁻</span>
</td>

<td class="tx-r tx-num tx-br g1">

28%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

non-worker
</td>

<td class="tx-r tx-num tx-br g2">

64
</td>

<td class="tx-r tx-num tx-br g1">

44%
</td>

<td class="tx-r tx-num tx-br g1">

59%
</td>

<td class="tx-r tx-num tx-br m3">

<u>20%</u><span class="tx-mark">⁻⁻⁻</span>
</td>

<td class="tx-r tx-num tx-br p2">

20%<span class="tx-mark">⁺⁺</span>
</td>

<td class="tx-r tx-num tx-br m2">

3%<span class="tx-mark">⁻⁻</span>
</td>

<td class="tx-r tx-num tx-br g1">

23%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

other worker
</td>

<td class="tx-r tx-num tx-br g2">

20
</td>

<td class="tx-r tx-num tx-br g1">

40%
</td>

<td class="tx-r tx-num tx-br g1">

60%
</td>

<td class="tx-r tx-num tx-br g1">

40%<span class="tx-sec">   </span>
</td>

<td class="tx-r tx-num tx-br g1">

0%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br g1">

10%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br g1">

35%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

senior
</td>

<td class="tx-r tx-num tx-br g2">

35
</td>

<td class="tx-r tx-num tx-br g1">

63%
</td>

<td class="tx-r tx-num tx-br g1">

57%
</td>

<td class="tx-r tx-num tx-br g1">

31%<span class="tx-sec">   </span>
</td>

<td class="tx-r tx-num tx-br p2">

26%<span class="tx-mark">⁺⁺</span>
</td>

<td class="tx-r tx-num tx-br g1">

3%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br g1">

34%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

student
</td>

<td class="tx-r tx-num tx-br g2">

70
</td>

<td class="tx-r tx-num tx-br g1">

43%
</td>

<td class="tx-r tx-num tx-br g1">

61%
</td>

<td class="tx-r tx-num tx-br g1">

44%<span class="tx-sec">   </span>
</td>

<td class="tx-r tx-num tx-br p2">

21%<span class="tx-mark">⁺⁺</span>
</td>

<td class="tx-r tx-num tx-br g1">

7%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br g1">

50%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

workman
</td>

<td class="tx-r tx-num tx-br g2">

12
</td>

<td class="tx-r tx-num tx-br g1">

25%
</td>

<td class="tx-r tx-num tx-br g1">

50%
</td>

<td class="tx-r tx-num tx-br g1">

17%<span class="tx-sec">   </span>
</td>

<td class="tx-r tx-num tx-br g1">

8%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br g1">

25%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br g1">

25%
</td>

</tr>

<tr class="tx-bt tx-bb tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

Total
</td>

<td class="tx-r tx-num tx-br g2">

300
</td>

<td class="tx-r tx-num tx-br g1">

48%
</td>

<td class="tx-r tx-num tx-br g1">

56%
</td>

<td class="tx-r tx-num tx-br g1">

34%<span class="tx-sec">   </span>
</td>

<td class="tx-r tx-num tx-br g1">

15%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br g1">

7%<span class="tx-sec">  </span>
</td>

<td class="tx-r tx-num tx-br g1">

34%
</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="8">

<div class="tx-foot">

Percentage points (risk) difference: cell ≥ the reference category (in
bold) <span class="p1" style="font-weight:normal;">+5⁺</span>;
<span class="p2" style="font-weight:normal;">+10⁺⁺</span>;
<span class="p3"
style="font-weight:normal;text-decoration:underline;"><u>+20⁺⁺⁺</u></span>;
<span class="p4"
style="font-weight:normal;text-decoration:underline;"><u>+30⁺⁺⁺⁺</u></span>
points; cell ≤ ref <span class="m1"
style="font-weight:normal;">-5⁻</span>; <span class="m2"
style="font-weight:normal;">-10⁻⁻</span>; <span class="m3"
style="font-weight:normal;text-decoration:underline;"><u>-20⁻⁻⁻</u></span>;
<span class="m4"
style="font-weight:normal;text-decoration:underline;"><u>-30⁻⁻⁻⁻</u></span>
points. Unmarked: not significantly different from the reference
category (Newcombe score interval, 95% confidence) or under the first
threshold (±5 points).

</div>

</td>

</tr>

</tfoot>

</table>

The same visual language extends to regression models: `tab_reg()`
detects a binary outcome and fits a logistic regression, coloring odds
ratios by strength and greying the non-significant ones, with a default
comparison between the modelised deviations and their crude/observed
counterparts.

``` r
tab_reg(gss, outcome = "married", predictors = c("race", "age", "rincome"))
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

<b>Reference profile</b>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b></b>
</td>

<td class="tx-r tx-num tx-b">

<b></b>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>1/1.43</b><span class="tx-sec" style="font-weight:normal;">\*\*\* (41%)</span>
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-lbl tx-b tx-nb" rowspan="3">

race
</td>

<td class="tx-l tx-br tx-bl tx-rv">

<b>White</b>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>9 862</b>
</td>

<td class="tx-r tx-num tx-b">

<span class="tx-sec"
style="font-weight:normal;">(52%) </span><b>     1</b><span class="tx-sec"
style="font-weight:normal;">   </span>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>     1</b><span class="tx-sec"
style="font-weight:normal;">    (51%)</span>
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
style="font-weight:normal;">(31%) </span><b><i><u>1/2.45</u></i></b><span class="tx-sec" style="font-weight:normal;">\*\*\*</span>
</td>

<td class="tx-r tx-num tx-br m3 tx-b">

<b><i><u>1/2.22</u></i></b><span class="tx-sec" style="font-weight:normal;">\*\*\* (33%)</span>
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
style="font-weight:normal;">      </span><b><u>  2.13</u></b><span class="tx-sec" style="font-weight:normal;">\*\*\*</span>
</td>

<td class="tx-r tx-num tx-br p2 tx-b">

<b>  1.95</b><span class="tx-sec" style="font-weight:normal;">\*\*\*      </span>
</td>

</tr>

<tr class="tx-b">

<td class="tx-l tx-br tx-bl tx-lbl tx-b tx-nb" rowspan="4">

rincome
</td>

<td class="tx-l tx-br tx-bl tx-rv">

<b>1-Lt \$10000</b>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>2 149</b>
</td>

<td class="tx-r tx-num tx-b">

<span class="tx-sec"
style="font-weight:normal;">(37%) </span><b>     1</b><span class="tx-sec"
style="font-weight:normal;">   </span>
</td>

<td class="tx-r tx-num tx-br tx-b">

<b>     1</b><span class="tx-sec"
style="font-weight:normal;">    (39%)</span>
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

2-\$10000 to 14999
</td>

<td class="tx-r tx-num tx-br g2">

1 168
</td>

<td class="tx-r tx-num p1">

<span class="tx-sec">(41%) </span>  1.21<span class="tx-sec">\*\* </span>
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

<td class="tx-r tx-num p1">

<span class="tx-sec">(43%) </span>  1.33<span class="tx-sec">\*\*\*</span>
</td>

<td class="tx-r tx-num tx-br p1">

  1.28<span class="tx-sec">\*\*\* (45%)</span>
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
style="font-weight:normal;">(55%) </span><b><u>  2.14</u></b><span class="tx-sec" style="font-weight:normal;">\*\*\*</span>
</td>

<td class="tx-r tx-num tx-br p2 tx-b">

<b>  1.85</b><span class="tx-sec" style="font-weight:normal;">\*\*\* (54%)</span>
</td>

</tr>

<tr class="tx-bt2">

<td class="tx-l tx-br tx-bl tx-lbl tx-vname tx-b tx-bb2" rowspan="8">

Model fit
</td>

<td class="tx-l tx-br tx-bl tx-rv">

N
</td>

<td class="tx-r tx-num tx-br">

</td>

<td class="tx-r tx-num">

</td>

<td class="tx-r tx-num tx-br">

12 990
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Dispersion (robust/model SE)
</td>

<td class="tx-r tx-num tx-br">

</td>

<td class="tx-r tx-num">

</td>

<td class="tx-r tx-num tx-br">

1.00
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Collinearity (max VIF)
</td>

<td class="tx-r tx-num tx-br">

</td>

<td class="tx-r tx-num">

</td>

<td class="tx-r tx-num tx-br">

1.03
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

Influence (max dfbetas)
</td>

<td class="tx-r tx-num tx-br">

</td>

<td class="tx-r tx-num">

</td>

<td class="tx-r tx-num tx-br">

0.05
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

LR vs null
</td>

<td class="tx-r tx-num tx-br">

</td>

<td class="tx-r tx-num">

</td>

<td class="tx-r tx-num tx-br">

\<0.01%
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

McFadden R2
</td>

<td class="tx-r tx-num tx-br">

</td>

<td class="tx-r tx-num">

</td>

<td class="tx-r tx-num tx-br">

0.049
</td>

</tr>

<tr>

<td class="tx-l tx-br tx-bl tx-rv">

AIC
</td>

<td class="tx-r tx-num tx-br">

</td>

<td class="tx-r tx-num">

</td>

<td class="tx-r tx-num tx-br">

17 129
</td>

</tr>

<tr class="tx-bb tx-bb2">

<td class="tx-l tx-br tx-bl tx-rv">

BIC
</td>

<td class="tx-r tx-num tx-br">

</td>

<td class="tx-r tx-num">

</td>

<td class="tx-r tx-num tx-br">

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
style="font-weight:normal;">1.2</span>; <span class="p2"
style="font-weight:bold;"><b>1.5</b></span>; <span class="p3"
style="font-weight:bold;text-decoration:underline;"><b><u>2</u></b></span>;
<span class="p4"
style="font-weight:bold;text-decoration:underline double;"><b><u>4</u></b></span>.
Italic: OR ≤ <span class="m1"
style="font-weight:normal;font-style:italic;"><i>1/1.2</i></span>;
<span class="m2"
style="font-weight:bold;font-style:italic;"><b><i>1/1.5</i></b></span>;
<span class="m3"
style="font-weight:bold;font-style:italic;text-decoration:underline;"><b><i><u>1/2</u></i></b></span>;
<span class="m4"
style="font-weight:bold;font-style:italic;text-decoration:underline double;"><b><i><u>1/4</u></i></b></span>.
Unmarked: not significantly different from the reference category (Wald
interval on the log odds-ratio, 95% confidence; matching Woolf interval
on the observed column) or under the first threshold (×1.2).<br>\*\*\*:
significantly different from the reference category (in bold) at the 99%
confidence level (from 1 for the Constant); \*\*: at the 95% level; \*:
at the 90% level; no star: not significant.

</div>

</td>

</tr>

</tfoot>

</table>

<table class="tabxplor-tab tx-shape">

<thead>

<tr>

<th class="tx-l">

outcome
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

p = %<sub>Married</sub> ; log(p/(1-p))
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

Any table exports with its colors to Excel, html or markdown (for Word,
copy-paste from Excel) :

``` r
tab(gss, marital, race, pct = "row", color = "difference") |> tab_html()
tab(gss, marital, race, pct = "row", color = "difference") |> tab_xl()
tab(gss, marital, race, pct = "row", color = "difference") |>
  tab_xl(theme = "print_ready")
```

## Learn more

- [Introduction to
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor.html)
  — the place to start (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-fr.html)*).
- [Regression tables with
  tab_reg()](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html)
  (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg-fr.html)*).
- [Reading a regression without losing sight of the
  percentages](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression.html)
  — a single analysis walked from a first cross-table to a finished
  sentence (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression-fr.html)*).
- [Weighted and survey
  data](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.html)
  — the three levels of margin of error, and which one your file
  deserves (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights-fr.html)*).
- [Programming with
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.html)
  — many tables at once, custom workflows, options (*aussi disponible
  [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming-fr.html)*).
