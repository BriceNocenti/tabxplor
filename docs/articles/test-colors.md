# TEMPORARY — color rendering test

Checklist — everything listed must show in color (not plain
black/white):

1.  **Text channel** (blue over- / red under-represented cells, several
    shades):

``` r
tab(gss, marital, race, pct = "row", color = "diff")
```

[TABLE]

2.  **Greyed non-significant cells** (`.g1`/`.g2`: some cells grey, the
    rest colored or black):

``` r
tab(gss, relig, race, pct = "row", color = "diff", color_signif = "grey_non_signif")
```

[TABLE]

3.  **Background channel** (`.o*`/`.u*`: cell fills, not text):

``` r
tab(gss, marital, race, pct = "row", color = c("", "diff"))
```

[TABLE]

4.  **Two channels at once** (text + background):

``` r
tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
```

[TABLE]

5.  **A regression table** (colored ORs, greyed non-significant):

``` r
tab_reg(gss, dependent = "married", predictors = c("race", "age", "rincome"))
```

Logistic regression: married by race, age +1 more

[TABLE]

6.  The **legend spans** under each table were colored even before the
    fix — they are the control group: if a table above is plain but its
    legend is colored, the host override is back.
