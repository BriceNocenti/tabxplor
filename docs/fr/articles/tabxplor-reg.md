# Regression tables with tab_reg()

``` r
library(tabxplor)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# As in the introduction vignette, tables are shown as the coloured console output turned to HTML.
options(cli.num_colors = 256)
set_color_palette(theme = "light")
```

For the most common regression models,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
builds a **regression table** that looks and behaves like a `tabxplor`
cross-table: one row per predictor level, significance stars, colours
that grey out non-significant effects, and the same Excel, html or
markdown exports. You give it a data frame, an **outcome** (`dependent`)
and some **predictors**, and it tries to pick the right kind of model
from the outcome’s type. Its distinctive feature is `empirical = TRUE`,
which shows the **observed / crude / empirical effect right next to the
model’s adjusted one**, so you can see what “controlling for the other
variables” actually changed.

We use a formatted version of the
[`forcats::gss_cat`](https://forcats.tidyverse.org/reference/gss_cat.html)
data, from the US General Social Survey:

``` r
gss_simple <- gss_cat_data_formatting()
```

## The outcome’s type chooses the model and the observed quantity to compare with

You rarely set `family` by hand —
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
mostly detects it:

| Outcome | Detected model | Effect measure |
|:---|:---|:---|
| 2-level factor | binomial (logistic) | odds ratio (OR) |
| numeric (continuous) | gaussian (linear) | coefficient β |
| count | poisson | incidence-rate ratio (IRR) |
| 3+ level unordered factor | multinomial | one OR column per category vs. the reference |
| 3+ level ordered factor | ordinal (proportional odds) | cumulative OR |

With `empirical = TRUE`, each model column is joined by a crude/observed
companion column showing the *empirical, unadjusted, single-predictor*
effect — the effect you would see with no controls at all, “all things
being unequal”, for that predictor. - 2 levels logistic (binomial) →
observed %, and observed odd-ratios (not modelised: calculated from
percentages only) - 3+ levels logistic (multinomial) → the observed ORs
are shown as a tooltip on the model cells in html exports - gaussian
(linear) → group means and their difference - poisson (counts) →
observed rate and observed rate ratio

## Logistic regression (a binary factor)

When the outcome is a two-level factor, here “married” versus “not
married”,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
chooses a **binomial** family to fit a logistic regression and reports
**odds ratios** (the reference level of each predictor shows the neutral
value `1`). - Like in every regression model, the effect of a predictor
level on the dependent variable reads “all the others chosen predictors
being equals”. - Colors read like any `tabxplor` table: an odds ratio
above 1 (blue) means *more likely to be married* than the reference
level, below 1 (red) means *less likely*; stars and colors both flag
significance.

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"))
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors          | Model fit   | married |
#> |:--------------------|:------------|--------:|
#> | race, age, rincome, | N           |  12 960 |
#> | relig               | LR vs null  |  <0.01% |
#> |                     | McFadden R2 |   0.057 |
#> |                     | AIC         |  16 960 |
#> |                     | BIC         |  17 064 |
#> 
#> # A tabxplor tab: 17 × 3
#> # Groups:         var [5]
#>    var      levels                Model_OR
#>    <fct>    <fct>                <row%-or>
#>  1 Constant Reference population 1/3.07***
#> 
#>  2 race     White                     1   
#>  3 race     Black                1/2.40***
#>  4 race     Other                  1.11   
#> 
#>  5 age      age                    1.02***
#> 
#>  6 rincome  1-Lt $10000               1   
#>  7 rincome  2-$10000 to 14999      1.14*  
#>  8 rincome  3-$15000 to 24999      1.27***
#>  9 rincome  4-$25000 or more       1.86***
#> 
#> 10 relig    1-Protestant              1   
#> 11 relig    2-Catholic           1/1.17***
#> 12 relig    3-Other christian    1/1.43***
#> 13 relig    4-Jewish             1/1.29*  
#> 14 relig    5-Buddhist/Hinduist  1/1.36*  
#> 15 relig    6-Muslim               1.05   
#> 16 relig    7-Other              1/2.04***
#> 17 relig    8-None               1/1.79***
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

For a simple logistic regression, `empirical = TRUE` add **raw
percentages** and **crude unmodelised odds-ratios** for each predictor.

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), empirical = TRUE)
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors          | Model fit   | married |
#> |:--------------------|:------------|--------:|
#> | race, age, rincome, | N           |  12 960 |
#> | relig               | LR vs null  |  <0.01% |
#> |                     | McFadden R2 |   0.057 |
#> |                     | AIC         |  16 960 |
#> |                     | BIC         |  17 064 |
#> 
#> # A tabxplor tab: 17 × 5
#> # Groups:         var [5]
#>    var      levels               `Obs_%`    Obs_OR  Model_OR
#>    <fct>    <fct>                 <row%> <row%-or> <row%-or>
#>  1 Constant Reference population                   1/3.07***
#> 
#>  2 race     White                 52%         1         1   
#>  3 race     Black                 31%*** 1/2.43*** 1/2.40***
#>  4 race     Other                 49%*   1/1.12*     1.11   
#> 
#>  5 age      age                                      1.02***
#> 
#>  6 rincome  1-Lt $10000           37%         1         1   
#>  7 rincome  2-$10000 to 14999     41%**    1.20**    1.14*  
#>  8 rincome  3-$15000 to 24999     43%***   1.32***   1.27***
#>  9 rincome  4-$25000 or more      55%***   2.12***   1.86***
#> 
#> 10 relig    1-Protestant          52%         1         1   
#> 11 relig    2-Catholic            52%    1/1.03    1/1.17***
#> 12 relig    3-Other christian     42%*** 1/1.53*** 1/1.43***
#> 13 relig    4-Jewish              55%      1.10    1/1.29*  
#> 14 relig    5-Buddhist/Hinduist   50%    1/1.09    1/1.36*  
#> 15 relig    6-Muslim              48%    1/1.17      1.05   
#> 16 relig    7-Other               35%*** 1/2.04*** 1/2.04***
#> 17 relig    8-None                38%*** 1/1.76*** 1/1.79***
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # Obs_%: difference (ref.): -30 -20 -10 -5 +5 +10 +20 +30 [grey: non-significant or under ±5 points]
#> # Obs_OR, Model_OR: OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

The raw percentages `Obs_%` are a summary of the base observed results
everything in the model is derived from :“28% of black americans are
married, compared to 51% for white americans” - The plain difference
from reference is colored (28% - 51% = -23%). - Cells that are
non-significantly different from the reference are greyed out (based on
a Newcombe confidence interval for differences of proportions).

The **modelised odds-ratios** `Model_OR` directly compared to the
**observed odds-ratios** `Obs_OR` : - Comparing the two tells you what
adjustment the model did: if a predictor’s **model** OR is much closer
to 1 than its **crude** OR, the raw association was largely explained by
the other predictors. - Here, “all things being unequal”, black
americans have 2.68 lower odds to be married than white americans. “All
things being equal” (more precisely : income, age and religion being
equal), black americans still have 2.4 lower odds to be married than
white americans. The result holds, it’s not explained by income
differences or religious differences.

The **observed odds-ratios** `Obs_OR` are the same as those you can
calculate from percentages only in a crosstable : - Colors and
significance stars use a **Woolf OR confidence interval** that matches
what is done in the regression model. - The population of the table must
match the complete-cases population of the model, filtering out
indivivuals with `NA` at any involved variable.

``` r
gss_simple |> 
  dplyr::filter(dplyr::if_all(all_of(c("race", "age", "rincome", "relig")), ~ !is.na(.) )) |> 
  tab(race, married, pct = "row", na = "drop", 
    OR = "OR", color = "OR", color_signif = "grey_non_signif"
   )
```

``` r-output
#> # A tabxplor tab: 4 × 3
#>   race   `01-Married` `02-Not married`
#>   <fct>     <row%-or>        <row%-or>
#> 1 White       1 (52%)          1 (48%)
#> 2 Black        1/2.43             2.43
#> 3 Other        1/1.12             1.12
#> 4 Total        1/1.14             1.14
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
```

### Average marginal effects (AME) of a logistic regression

Another way to interpret a logistic regression is to use **average
marginal effects** (AME) with `effect = "ame"` : - Instead of directly
reading the modelised odd-ratio we can infer, from it, the average
difference of percentage of each level compared to the reference. - Here
`Model_AME` reads like this : “Comparing Black and White respondents who
are alike in income, age and religion, being Black is associated with a
marriage rate 19.8 points lower, on average.”

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), effect = "ame", empirical = TRUE)
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors          | Model fit   | married |
#> |:--------------------|:------------|--------:|
#> | race, age, rincome, | N           |  12 960 |
#> | relig               | LR vs null  |  <0.01% |
#> |                     | McFadden R2 |   0.057 |
#> |                     | AIC         |  16 960 |
#> |                     | BIC         |  17 064 |
#> 
#> # A tabxplor tab: 17 × 5
#> # Groups:         var [5]
#>    var      levels               `Obs_%`    Obs_diff `Model_AME (adjusted %)`
#>    <fct>    <fct>                 <row%> <row%-diff>             <row%-mixed>
#>  1 Constant Reference population                                             
#> 
#>  2 race     White                 52%         +0%                     (51.3%)
#>  3 race     Black                 31%***     -21%***        -19.8%*** (31.5%)
#>  4 race     Other                 49%*        -3%*           +2.4%    (53.8%)
#> 
#>  5 age      age                                                      +0.5%***
#> 
#>  6 rincome  1-Lt $10000           37%         +0%                     (39.2%)
#>  7 rincome  2-$10000 to 14999     41%**       +4%**          +3.0%*   (42.2%)
#>  8 rincome  3-$15000 to 24999     43%***      +7%***         +5.5%*** (44.7%)
#>  9 rincome  4-$25000 or more      55%***     +18%***        +14.5%*** (53.7%)
#> 
#> 10 relig    1-Protestant          52%         +0%                     (52.9%)
#> 11 relig    2-Catholic            52%         -1%            -3.7%*** (49.3%)
#> 12 relig    3-Other christian     42%***     -11%***         -8.3%*** (44.6%)
#> 13 relig    4-Jewish              55%         +2%            -6.0%*   (46.9%)
#> 14 relig    5-Buddhist/Hinduist   50%         -2%            -7.2%*   (45.7%)
#> 15 relig    6-Muslim              48%         -4%            +1.2%    (54.1%)
#> 16 relig    7-Other               35%***     -17%***        -16.4%*** (36.5%)
#> 17 relig    8-None                38%***     -14%***        -13.5%*** (39.4%)
#> # Model: logistic regression; marginal effects on the probability scale (percentage points) (sample-averaged); each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability.
#> # Obs_%, Obs_diff, Model_AME (adjusted %): difference (ref.): -30 -20 -10 -5 +5 +10 +20 +30 [grey: non-significant or under ±5 points]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

Then we can use `empirical = TRUE` to compare this new modelised
quantity to the **raw, observed difference of percentages** : - In the
sample (`Obs_diff`), we observe a rate of marriages 23 points lower for
black americans compared to white american (`28% - 51% = -23%`). - The
modelised difference `Model_AME` is very close, `-19.8%`, so very little
of this difference between black and white can be explained by age,
income or religion.

We can also compare the raw percentage `Obs_%` with the **model-adjusted
probability** — the value in parentheses in the `Model_AME (adjusted %)`
column : - Adjusted percentage, which is a result of the model, reads
this way : “if the whole sample kept its income/age/religion
distribution but everyone were Black, an estimated 31.5% would be
married, vs 51.3% if everyone were White”. - Blacks marry at 30.9% in
this sample (observed); standardizing their income/age/religion to the
population mix moves that only to 31.5% — so almost none of the
Black–White gap is accounted for by differences in those three
variables; it persists after adjustment.

We can, again, get the same empirical differences and confidence
intervals with a plain table :

``` r
dependent  <- "married"
predictors <- c("race", "age", "rincome", "relig")
factor_predictors <- c("race", "rincome", "relig")
gss_simple |>
  dplyr::filter(dplyr::if_all(all_of(c(predictors, dependent)), ~ !is.na(.) )) |> 
  tab(all_of(factor_predictors), all_of(dependent), pct = "row", na = "drop", 
      color = "diff", ref = 1, color_signif = "grey_non_signif", stars = TRUE, 
      method_diff = "wald"
      ) |>
  dplyr::mutate(diff = set_display(`01-Married`, "diff"))
```

``` r-output
#> # A tabxplor tab: 18 × 6
#> # Groups:         row_var [3]
#>    row_var levels              `01-Married` `02-Not married`           Total
#>    <fct>   <fct>                     <row%>           <row%>          <row%>
#>  1 race    White                     52%              48%    100% (n= 9 846)
#>  2 race    Black                     31%***           69%*** 100% (n= 1 860)
#>  3 race    Other                     49%*             51%*   100% (n= 1 254)
#>  4 race    Total                     49%***           51%*** 100% (n=12 960)
#> 
#>  5 rincome 1-Lt $10000               37%              63%    100% (n= 2 142)
#>  6 rincome 2-$10000 to 14999         41%**            59%**  100% (n= 1 164)
#>  7 rincome 3-$15000 to 24999         43%***           57%*** 100% (n= 2 322)
#>  8 rincome 4-$25000 or more          55%***           45%*** 100% (n= 7 332)
#>  9 rincome Total                     49%***           51%*** 100% (n=12 960)
#> 
#> 10 relig   1-Protestant              52%              48%    100% (n= 6 269)
#> 11 relig   2-Catholic                52%              48%    100% (n= 3 121)
#> 12 relig   3-Other christian         42%***           58%*** 100% (n=   507)
#> 13 relig   4-Jewish                  55%              45%    100% (n=   222)
#> 14 relig   5-Buddhist/Hinduist       50%              50%    100% (n=   144)
#> 15 relig   6-Muslim                  48%              52%    100% (n=    56)
#> 16 relig   7-Other                   35%***           65%*** 100% (n=   267)
#> 17 relig   8-None                    38%***           62%*** 100% (n= 2 374)
#> 18 relig   Total                     49%***           51%*** 100% (n=12 960)
#> # ℹ 1 more variable: diff <row%-diff>
#> # difference (ref.): -30 -20 -10 -5 +5 +10 +20 +30 [grey: non-significant or under ±5 points]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

## Grouped-binomial outcomes (a summed score)

When the outcome is a **summed score** — how many of several yes/no
items a respondent answered one way — you model the number of
“successes” out of a fixed number of items with `trials =`.
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
then fits `cbind(score, trials - score)` as a binomial, so the odds
ratios read on the *per-item* probability.
[`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/fr/reference/score_from_lv1.md)
(see
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor-programming.md))
builds such a score by counting factors at their first level:

``` r
gss_simple |>
  score_from_lv1("score", vars_list = c("married", "income25k")) |>   # a 0–2 score
  tab_reg("score", c("race", "age"), family = "binomial", trials = 2)
```

``` r-output
#> | predictors | Model fit   |  score |
#> |:-----------|:------------|-------:|
#> | race, age  | N           | 21 407 |
#> |            | LR vs null  | <0.01% |
#> |            | McFadden R2 |  0.012 |
#> |            | AIC         | 44 395 |
#> |            | BIC         | 44 427 |
#> |            | Dispersion  |   1.09 |
#> 
#> # A tabxplor tab: 5 × 3
#> # Groups:         var [3]
#>   var      levels                Model_OR
#>   <fct>    <fct>                <row%-or>
#> 1 Constant Reference population 1/1.10***
#> 
#> 2 race     White                     1   
#> 3 race     Black                1/1.99***
#> 4 race     Other                1/1.20***
#> 
#> 5 age      age                  1/1.00***
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

Grouped-binomial (like Poisson) models report a Pearson **dispersion**
check in the footer, flagging over-dispersed counts.

## Ordinal and nominal outcomes (a factor with 3+ levels)

An **ordered** factor outcome is fit as a proportional-odds (cumulative)
logistic model:

``` r
tab_reg(gss_simple, "rincome", c("race", "age", "relig"))
#> ℹ "rincome": ordered outcome detected -> `family = "ordinal"`
#>   (proportional-odds).
#> Warning in brant::brant(fit): 5 combinations in table(dv,ivs) do not occur.
#> Because of that, the test results might be invalid.
#> Warning: ! The proportional-odds (parallel-lines) assumption is rejected (Brant omnibus
#>   p = 0.0089).
#> ℹ Cumulative odds ratios may mislead; consider `family = "multinomial"` or a
#>   partial proportional-odds model.
#> ℹ The Brant test over-rejects at large N; inspect the per-variable tests too.
```

``` r-output
#> | predictors       | Model fit     | rincome |
#> |:-----------------|:--------------|--------:|
#> | race, age, relig | N             |  12 960 |
#> |                  | LR vs null    |  <0.01% |
#> |                  | McFadden R2   |   0.017 |
#> |                  | AIC           |  29 193 |
#> |                  | BIC           |  29 290 |
#> |                  | Brant PO test |  0.887% |
#> 
#> # A tabxplor tab: 13 × 3
#> # Groups:         var [4]
#>    var      levels                Model_OR
#>    <fct>    <fct>                <row%-or>
#>  1 Constant Reference population          
#> 
#>  2 race     White                     1   
#>  3 race     Black                1/1.40***
#>  4 race     Other                1/1.45***
#> 
#>  5 age      age                    1.02***
#> 
#>  6 relig    1-Protestant              1   
#>  7 relig    2-Catholic             1.15***
#>  8 relig    3-Other christian      1.02   
#>  9 relig    4-Jewish               2.00***
#> 10 relig    5-Buddhist/Hinduist    2.41***
#> 11 relig    6-Muslim               1.32   
#> 12 relig    7-Other                1.09   
#> 13 relig    8-None                 1.05   
#> # Model: ordinal logistic regression; cumulative odds ratios (proportional-odds model).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

A nominal outcome with three or more unordered levels is fit as one
**multinomial** logistic model, giving one odds-ratio column per outcome
category versus the reference category (also called relative risks
ratios) :

``` r
tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"))
#> ℹ "party3": nominal outcome detected -> `family = "multinomial"` (multinomial
#>   logistic).
```

``` r-output
#> | predictors          | Model fit   | party3 |
#> |:--------------------|:------------|-------:|
#> | race, age, rincome, | N           | 12 914 |
#> | relig               | LR vs null  | <0.01% |
#> |                     | McFadden R2 |  0.083 |
#> |                     | AIC         | 24 939 |
#> |                     | BIC         | 25 148 |
#> 
#> # A tabxplor tab: 17 × 4
#> # Groups:         var [5]
#>    var      levels               2-Independent, other v…¹ 3-Republican vs 1-De…²
#>    <fct>    <fct>                               <row%-or>              <row%-or>
#>  1 Constant Reference population                    18                   1.87***
#> 
#>  2 race     White                                    1                      1   
#>  3 race     Black                               1/3.03***             1/12.89***
#>  4 race     Other                                 1.05                 1/2.37***
#> 
#>  5 age      age                                 1/1.01***              1/1.00***
#> 
#>  6 rincome  1-Lt $10000                              1                      1   
#>  7 rincome  2-$10000 to 14999                     1.01                 1/1.21** 
#>  8 rincome  3-$15000 to 24999                   1/1.00                 1/1.08   
#>  9 rincome  4-$25000 or more                    1/1.53***                1.17** 
#> 
#> 10 relig    1-Protestant                             1                      1   
#> 11 relig    2-Catholic                          1/1.11                 1/1.78***
#> 12 relig    3-Other christian                     1.13                 1/1.27** 
#> 13 relig    4-Jewish                            1/2.96***              1/4.88***
#> 14 relig    5-Buddhist/Hinduist                 1/1.33                 1/4.83***
#> 15 relig    6-Muslim                            1/2.36**               1/5.96***
#> 16 relig    7-Other                               1.10                 1/2.56***
#> 17 relig    8-None                                1.22***              1/3.54***
#> # ℹ abbreviated names: ¹​`2-Independent, other vs 1-Democrat`,
#> #   ²​`3-Republican vs 1-Democrat`
#> # Model: multinomial logistic regression; odds ratios (each category vs the reference).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

Relative risk ratios may be quite difficult to read because they are
relative to **two** reference levels : not only the reference chosen for
the predictor, but also the reference level chosen for the dependent
variable. It’s specially difficult when it’s difficult to find a good
reference level corresponding to the most common situation (like
“married” for matrimonial status).

Most of the time, average marginal effects (AME) are easier to
interpret, because they make the second reference level disappear, and
directly modelise, for each level of the dependent variable, difference
of percentages of each predictor compared to their reference level (a
less abstract quantity than odds ratios).

``` r
tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"), effect = "ame", empirical = TRUE) # |> tab_export()
#> ℹ "party3": nominal outcome detected -> `family = "multinomial"` (multinomial
#>   logistic).
```

``` r-output
#> | predictors          | Model fit   | party3 |
#> |:--------------------|:------------|-------:|
#> | race, age, rincome, | N           | 12 914 |
#> | relig               | LR vs null  | <0.01% |
#> |                     | McFadden R2 |  0.083 |
#> |                     | AIC         | 24 939 |
#> |                     | BIC         | 25 148 |
#> 
#> # A tabxplor tab: 17 × 5
#> # Groups:         var [5]
#>    var      levels                    `1-Democrat` `2-Independent, other`
#>    <fct>    <fct>                     <row%-mixed>           <row%-mixed>
#>  1 Constant Reference population                                         
#> 
#>  2 race     White                          (38.3%)                (20.6%)
#>  3 race     Black                +40.6%*** (78.9%)       -6.6%*** (14.0%)
#>  4 race     Other                +11.0%*** (49.3%)       +6.8%*** (27.4%)
#> 
#>  5 age      age                           +0.2%***               -0.1%***
#> 
#>  6 rincome  1-Lt $10000                    (44.0%)                (24.5%)
#>  7 rincome  2-$10000 to 14999     +2.0%    (46.0%)       +1.6%    (26.1%)
#>  8 rincome  3-$15000 to 24999     +0.9%    (44.9%)       +0.5%    (25.0%)
#>  9 rincome  4-$25000 or more      +1.8%    (45.8%)       -7.7%*** (16.8%)
#> 
#> 10 relig    1-Protestant                   (39.2%)                (17.2%)
#> 11 relig    2-Catholic            +9.2%*** (48.4%)       +2.4%*** (19.6%)
#> 12 relig    3-Other christian     +2.4%    (41.7%)       +3.7%**  (20.9%)
#> 13 relig    4-Jewish             +31.4%*** (70.6%)       -6.0%*** (11.2%)
#> 14 relig    5-Buddhist/Hinduist  +23.1%*** (62.3%)       +4.4%    (21.6%)
#> 15 relig    6-Muslim             +31.6%*** (70.9%)       -3.1%    (14.1%)
#> 16 relig    7-Other              +11.7%*** (50.9%)       +8.0%*** (25.2%)
#> 17 relig    8-None               +13.4%*** (52.7%)      +11.9%*** (29.1%)
#> # ℹ 1 more variable: `3-Republican` <row%-mixed>
#> # Model: multinomial logistic regression; marginal effects on the probability scale (percentage points) (sample-averaged); each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability.
#> # AME (ref.): -30 -20 -10 -5 +5 +10 +20 +30 [grey: non-significant or under ±5 points]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

With `empirical = TRUE`, the crude/observed/unajusted differences and
raw percentages appear in html tooltips, available at mouse hover of a
cell.

## Linear regression (a numeric outcome)

A continuous outcome gives plain linear regression coefficients (here we
set `family` explicitly, because an integer like `age` is ambiguous — it
could also be modelled as a count):

``` r
tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian")
```

``` r-output
#> | predictors     | Model fit   |    age |
#> |:---------------|:------------|-------:|
#> | race, marital, | N           | 12 958 |
#> | relig, rincome | F           | <0.01% |
#> |                | R2          |  0.256 |
#> |                | Adjusted R2 |  0.255 |
#> |                | Residual SD |  11.62 |
#> 
#> # A tabxplor tab: 21 × 3
#> # Groups:         var [5]
#>    var      levels                 Model_β
#>    <fct>    <fct>                   <coef>
#>  1 Constant Reference population  43.81***
#> 
#>  2 race     White                     0   
#>  3 race     Black                 -1.00***
#>  4 race     Other                 -3.32***
#> 
#>  5 marital  Married                   0   
#>  6 marital  Separated             -2.02***
#>  7 marital  Divorced               2.88***
#>  8 marital  Widowed               15.15***
#>  9 marital  Never married        -10.72***
#> 
#> 10 relig    1-Protestant              0   
#> 11 relig    2-Catholic            -1.03***
#> 12 relig    3-Other christian     -3.25***
#> 13 relig    4-Jewish               3.39***
#> 14 relig    5-Buddhist/Hinduist    0.49   
#> 15 relig    6-Muslim              -2.86*  
#> 16 relig    7-Other               -3.52***
#> 17 relig    8-None                -2.79***
#> 
#> 18 rincome  1-Lt $10000               0   
#> 19 rincome  2-$10000 to 14999      1.18***
#> 20 rincome  3-$15000 to 24999      0.96***
#> 21 rincome  4-$25000 or more       3.17***
#> # Model: linear regression; coefficients (mean difference vs the reference category).
#> # β (ref.): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8 [grey: non-significant or under ±0.2 SD]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

In the case of a linear regression, the **empirical/observed counterpart
of the model coefficient** for a categorical predictor is simply the
**difference of means** : here, the difference of mean age, per level of
the predictor, compared to the reference level.

``` r
tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian", empirical = TRUE)
```

``` r-output
#> | predictors     | Model fit   |    age |
#> |:---------------|:------------|-------:|
#> | race, marital, | N           | 12 958 |
#> | relig, rincome | F           | <0.01% |
#> |                | R2          |  0.256 |
#> |                | Adjusted R2 |  0.255 |
#> |                | Residual SD |  11.62 |
#> 
#> # A tabxplor tab: 21 × 5
#> # Groups:         var [5]
#>    var      levels                     Obs_mean  Obs_diff   Model_β
#>    <fct>    <fct>                        <mean>    <coef>    <coef>
#>  1 Constant Reference population                           43.81***
#> 
#>  2 race     White                43.32 (σ13.58)      0         0   
#>  3 race     Black                40.31 (σ12.67)  -3.01***  -1.00***
#>  4 race     Other                38.06 (σ12.46)  -5.26***  -3.32***
#> 
#>  5 marital  Married              44.91 (σ12.20)      0         0   
#>  6 marital  Separated            41.94 (σ10.93)  -2.97***  -2.02***
#>  7 marital  Divorced             47.63 (σ11.16)   2.72***   2.88***
#>  8 marital  Widowed              59.88 (σ13.13)  14.97***  15.15***
#>  9 marital  Never married        33.03 (σ11.42) -11.88*** -10.72***
#> 
#> 10 relig    1-Protestant         44.15 (σ13.60)      0         0   
#> 11 relig    2-Catholic           41.99 (σ13.37)  -2.16***  -1.03***
#> 12 relig    3-Other christian    39.23 (σ11.47)  -4.92***  -3.25***
#> 13 relig    4-Jewish             48.04 (σ15.08)   3.89***   3.39***
#> 14 relig    5-Buddhist/Hinduist  41.45 (σ12.95)  -2.70**    0.49   
#> 15 relig    6-Muslim             38.71 (σ10.55)  -5.43***  -2.86*  
#> 16 relig    7-Other              38.91 (σ11.78)  -5.24***  -3.52***
#> 17 relig    8-None               38.92 (σ12.69)  -5.23***  -2.79***
#> 
#> 18 rincome  1-Lt $10000          38.50 (σ16.75)      0         0   
#> 19 rincome  2-$10000 to 14999    41.09 (σ15.17)   2.59***   1.18***
#> 20 rincome  3-$15000 to 24999    40.82 (σ13.34)   2.32***   0.96***
#> 21 rincome  4-$25000 or more     44.21 (σ11.69)   5.71***   3.17***
#> # Model: linear regression; coefficients (mean difference vs the reference category).
#> # Obs_diff, Model_β: standardized difference (ref.): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8 [grey: non-significant or under ±0.2 SD]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

The empirical mean ages, and differences of mean ages from reference,
can be computed in a simple table with :

``` r
tab(gss_simple, "race", "age", pct = "row", digits = 2, na = "drop",
    color = "diff", ref = 1,  method_mean_diff = "student"
) |> 
  mutate(diff = set_display(age, "diff"))
```

``` r-output
#> # A tabxplor tab: 4 × 3
#>   race              age        diff
#>   <fct>          <mean> <mean-diff>
#> 1 White  48.72 (σ17.50)   ref:48.72
#> 2 Black  43.90 (σ16.06)       -4.83
#> 3 Other  39.48 (σ14.39)       -9.24
#> 4 Total  47.18 (σ17.29)       -1.54
#> # standardized difference (White): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8
```

``` r
# student : confidence intervals with pooled variance, to match those computed by linear regression
```

## Poisson regression (a count outcome)

``` r
tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson")
```

``` r-output
#> | predictors     | Model fit   | tvhours |
#> |:---------------|:------------|--------:|
#> | race, marital, | N           |   6 811 |
#> | relig, rincome | LR vs null  |  <0.01% |
#> |                | McFadden R2 |   0.033 |
#> |                | AIC         |  26 179 |
#> |                | BIC         |  26 295 |
#> |                | Dispersion  |    1.45 |
#> 
#> # A tabxplor tab: 21 × 3
#> # Groups:         var [5]
#>    var      levels               Model_IRR
#>    <fct>    <fct>                <row%-or>
#>  1 Constant Reference population   2.83***
#> 
#>  2 race     White                     1   
#>  3 race     Black                  1.47***
#>  4 race     Other                  1.01   
#> 
#>  5 marital  Married                   1   
#>  6 marital  Separated              1.06   
#>  7 marital  Divorced               1.05*  
#>  8 marital  Widowed                1.11** 
#>  9 marital  Never married          1.07***
#> 
#> 10 relig    1-Protestant              1   
#> 11 relig    2-Catholic             1.00   
#> 12 relig    3-Other christian    1/1.18***
#> 13 relig    4-Jewish             1/1.11   
#> 14 relig    5-Buddhist/Hinduist  1/1.26** 
#> 15 relig    6-Muslim             1/1.58***
#> 16 relig    7-Other              1/1.02   
#> 17 relig    8-None               1/1.11***
#> 
#> 18 rincome  1-Lt $10000               1   
#> 19 rincome  2-$10000 to 14999    1/1.04   
#> 20 rincome  3-$15000 to 24999    1/1.10***
#> 21 rincome  4-$25000 or more     1/1.34***
#> # Model: Poisson regression; incidence-rate ratios (vs the reference category).
#> # IRR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

An **incidence-rate ratio** (IRR) of 1.5 means “50% more hours of TV per
day”. Unweighted Poisson models automatically use dispersion-scaled
(quasi-Poisson) standard errors, so over-dispersed counts get honest,
wider intervals. Concretely: with an over-dispersed outcome,
`family = "poisson"` returns CIs and p-values **identical to
`family = "quasipoisson"`** and emits a warning saying so (the footer
reports the dispersion); at equidispersion (≈ 1) the scaling is a no-op
and the result matches a plain `glm(family = poisson)` — so a comparison
to a hand-fit Poisson `glm` never surprises you.

In the case of a poisson regression, the **empirical/observed
counterpart of the model’s exponentiated coefficient** for a categorical
predictor is the **ratio of means** : here, the ratio of average TV
hours compared to the reference level.

``` r
tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson", empirical = TRUE)
```

``` r-output
#> | predictors     | Model fit   | tvhours |
#> |:---------------|:------------|--------:|
#> | race, marital, | N           |   6 811 |
#> | relig, rincome | LR vs null  |  <0.01% |
#> |                | McFadden R2 |   0.033 |
#> |                | AIC         |  26 179 |
#> |                | BIC         |  26 295 |
#> |                | Dispersion  |    1.45 |
#> 
#> # A tabxplor tab: 21 × 5
#> # Groups:         var [5]
#>    var      levels               Obs_rate   Obs_IRR Model_IRR
#>    <fct>    <fct>                  <mean> <row%-or> <row%-or>
#>  1 Constant Reference population                      2.83***
#> 
#>  2 race     White                 2.36         1         1   
#>  3 race     Black                 3.66***   1.55***   1.47***
#>  4 race     Other                 2.44      1.03      1.01   
#> 
#>  5 marital  Married               2.36         1         1   
#>  6 marital  Separated             2.87***   1.21***   1.06   
#>  7 marital  Divorced              2.54***   1.08***   1.05*  
#>  8 marital  Widowed               2.93***   1.24***   1.11** 
#>  9 marital  Never married         2.81***   1.19***   1.07***
#> 
#> 10 relig    1-Protestant          2.72         1         1   
#> 11 relig    2-Catholic            2.50*** 1/1.09***   1.00   
#> 12 relig    3-Other christian     2.31*** 1/1.18*** 1/1.18***
#> 13 relig    4-Jewish              2.14*** 1/1.27*** 1/1.11   
#> 14 relig    5-Buddhist/Hinduist   1.95*** 1/1.40*** 1/1.26** 
#> 15 relig    6-Muslim              1.89**  1/1.44**  1/1.58***
#> 16 relig    7-Other               2.64    1/1.03    1/1.02   
#> 17 relig    8-None                2.37*** 1/1.15*** 1/1.11***
#> 
#> 18 rincome  1-Lt $10000           3.10         1         1   
#> 19 rincome  2-$10000 to 14999     3.02    1/1.03    1/1.04   
#> 20 rincome  3-$15000 to 24999     2.84**  1/1.09**  1/1.10***
#> 21 rincome  4-$25000 or more      2.24*** 1/1.39*** 1/1.34***
#> # Model: Poisson regression; incidence-rate ratios (vs the reference category).
#> # Obs_rate: ratio (ref.): ÷4 ÷2 ÷1.5 ÷1.2 ×1.2 ×1.5 ×2 ×4 [grey: non-significant or under ×1.2]
#> # Obs_IRR, Model_IRR: IRR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

The empirical average TV hours per day, and differences of rate of
television watching from reference, can be computed in a simple table
with :

``` r
tab(gss_simple, "race", "tvhours", pct = "row", digits = 2, na = "drop",
    color = "ratio", ref = 1,  method_mean_ratio = "quasipoisson"
) |> 
  mutate(IRR = set_display(tvhours, "ratio"))
```

``` r-output
#> # A tabxplor tab: 4 × 3
#>   race        tvhours          IRR
#>   <fct>        <mean> <mean-ratio>
#> 1 White  2.77 (σ2.31)           ×1
#> 2 Black  4.18 (σ3.51)        ×1.51
#> 3 Other  2.76 (σ2.41)           ×1
#> 4 Total  2.98 (σ2.59)        ×1.08
#> # ratio (White): ÷4 ÷2 ÷1.5 ÷1.2 ×1.2 ×1.5 ×2 ×4
```

``` r
# default method for confidence intervals is robust ratio of means with unequal variable ;
#  we use "quasipoisson" to match those computed by quasi-poisson regression 
#  (assumption : variance is proportional to mean). 
```

## Weighted and survey data

Passing a weight column with `wt =` switches estimation to a **survey
design**
([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)),
which gives correct *design-based* standard errors. The framework
`tabxplor` uses is simple: the **estimate is weighted**, and the model’s
standard errors come from the survey design, so unequal weights widen
the confidence intervals honestly. (This is the design-based counterpart
to
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)’s
cheaper `options(tabxplor.kish_neff = TRUE)` approximation — see
[`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor.md).)

``` r
# `weight` is a survey weight column in your data, using mostly a basic  :
tab_reg(data, "dependent", c("pred1", "pred2"), wt = "weight")

# A stratified / clustered design (strata usually *narrow* the intervals, clusters widen them):
tab_reg(data, "dependent", c("pred1", "pred2"),
        wt = "weight", ids = "psu", strata = "stratum")

# Or pass a design you built yourself with survey::svydesign() as `data` (see ?survey::svydesign
# for calibration, replicate weights and the more complex cases).
```

If you only have simple weights, `wt =` is all you need. Give `strata =`
when you have a stratified survey: it typically buys a little precision
(narrower confidence intervals) when the strata variables are related to
the outcome.

The `empirical = TRUE` observed companions columns (the unadjusted
`Obs_*` effects) are *descriptive*, so on weighted data their confidence
intervals honour `options(tabxplor.kish_neff = TRUE)` exactly like
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).
Set it to `TRUE` when using
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
with weights for a more like-for-like uncertainty comparison (the model
columns themselves stay design-based regardless, so there’s always a
remainnig discrepency between the two ways to calculate confidence
intervals).

## Comparing several models

Pass a **named list** of predictor sets instead of a vector to fit and
show several models side by side, and `compare =` adds a comparison
Likelihood ratio test in the footer (`"baseline"` = each model vs. the
first; `"sequential"` = each vs. the previous):

``` r
tab_reg(gss_simple,
        "married",
        list("Race only"    = "race",
             "+ age"        = c("race", "age"),
             "+ party"      = c("race", "age", "party3")),
        compare = "sequential")
#> ℹ "married": binary outcome detected -> `family = "binomial"` (logistic).
#> ℹ Column "+ age": models are not nested or N differs -> showing the AIC
#>   difference vs the previous model instead of a likelihood-ratio test.
#> ℹ A different N is usually the per-model missing-value drop; set `na =
#>   "drop_all_models"` to fit every model on the same complete cases so the
#>   likelihood-ratio test can run.
#> ℹ Column "+ party": models are not nested or N differs -> showing the AIC
#>   difference vs the previous model instead of a likelihood-ratio test.
#> ℹ A different N is usually the per-model missing-value drop; set `na =
#>   "drop_all_models"` to fit every model on the same complete cases so the
#>   likelihood-ratio test can run.
```

``` r-output
#> | Model fit             | Race only |   |  + age |   | + party |
#> |:----------------------|----------:|:-:|-------:|:-:|--------:|
#> | N                     |    21 483 |   | 21 407 |   |  21 261 |
#> | LR vs null            |    <0.01% |   | <0.01% |   |  <0.01% |
#> | McFadden R2           |     0.019 |   |  0.023 |   |   0.032 |
#> | AIC                   |    29 139 |   | 28 933 |   |  28 489 |
#> | BIC                   |    29 163 |   | 28 965 |   |  28 537 |
#> | Delta-AIC vs previous |           |   |   -206 |   |    -444 |
#> 
#> # A tabxplor tab: 8 × 5
#> # Groups:         var [4]
#>   var      levels               `Race only`   `+ age` `+ party`
#>   <fct>    <fct>                  <row%-or> <row%-or> <row%-or>
#> 1 Constant Reference population       13*   1/1.46*** 1/1.79***
#> 
#> 2 race     White                       1         1         1   
#> 3 race     Black                  1/2.68*** 1/2.58*** 1/2.19***
#> 4 race     Other                  1/1.13*** 1/1.05      1.05   
#> 
#> 5 age      age                                1.01***   1.01***
#> 
#> 6 party3   1-Democrat                                      1   
#> 7 party3   2-Independent, other                         1.05   
#> 8 party3   3-Republican                                 1.65***
#> # Model: logistic regression of married ('01-Married'); odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

## The same model within sub-populations

`split_var =` is the regression analogue of
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)’s
`tab_vars`: the same model is fitted **within each level** of a grouping
variable, and the per-group tables are stacked into one grouped table.
It answers “does this effect hold in every subgroup?”. With just one
dependent variable, it uses
[`tab_spread()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_spread.md)
internally to lay the groups out as side-by-side columns for an easy
comparison :

``` r
tab_reg(gss_simple, "married", c("race", "rincome"), split_var = "year")
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors    | Model fit   |   2000 |   |   2002 |   |   2004 |   |   2006 |   |   2008 |   |   2010 |   |   2012 |   |   2014 |
#> |:--------------|:------------|-------:|:-:|-------:|:-:|-------:|:-:|-------:|:-:|-------:|:-:|-------:|:-:|-------:|:-:|-------:|
#> | race, rincome | N           |  1 818 |   |  1 780 |   |  1 688 |   |  2 669 |   |  1 189 |   |  1 202 |   |  1 146 |   |  1 523 |
#> |               | LR vs null  | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |
#> |               | McFadden R2 |  0.020 |   |  0.024 |   |  0.033 |   |  0.029 |   |  0.034 |   |  0.043 |   |  0.050 |   |  0.046 |
#> |               | AIC         |  2 475 |   |  2 418 |   |  2 267 |   |  3 604 |   |  1 604 |   |  1 598 |   |  1 519 |   |  2 021 |
#> |               | BIC         |  2 508 |   |  2 451 |   |  2 299 |   |  3 640 |   |  1 635 |   |  1 628 |   |  1 549 |   |  2 053 |
#> 
#> # A tabxplor tab: 8 × 10
#> # Groups:         var [3]
#>   var     levels                  `2000`    `2002`    `2004`    `2006`    `2008`
#>   <fct>   <fct>                <row%-or> <row%-or> <row%-or> <row%-or> <row%-or>
#> 1 Consta… Reference population 1/1.54*** 1/1.45*** 1/1.23    1/1.22*   1/1.60***
#> 
#> 2 race    White                     1         1         1         1         1   
#> 3 race    Black                1/1.89*** 1/2.15*** 1/2.42*** 1/2.44*** 1/2.21***
#> 4 race    Other                1/1.10      1.12      1.04      1.01    1/1.10   
#> 
#> 5 rincome 1-Lt $10000               1         1         1         1         1   
#> 6 rincome 2-$10000 to 14999      1.44**    1.48**    1.11    1/1.20      1.13   
#> 7 rincome 3-$15000 to 24999      1.28      1.22      1.29      1.08      1.43*  
#> 8 rincome 4-$25000 or more       1.85***   1.79***   2.04***   1.70***   2.25***
#> # ℹ 3 more variables: `2010` <row%-or>, `2012` <row%-or>, `2014` <row%-or>
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

## Fine-tuning the display, and scaling a predictor

A few arguments change *what each cell shows*, or *how a predictor is
scaled* — without changing the model itself.

**`estimate_display`** folds a second quantity into each estimate cell.
`"ci"` shows the confidence interval beside every estimate (any family):

``` r
tab_reg(gss_simple, "married", c("race", "age"), estimate_display = "ci")
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors | Model fit   | married |
#> |:-----------|:------------|--------:|
#> | race, age  | N           |  21 407 |
#> |            | LR vs null  |  <0.01% |
#> |            | McFadden R2 |   0.023 |
#> |            | AIC         |  28 933 |
#> |            | BIC         |  28 965 |
#> 
#> # A tabxplor tab: 5 × 3
#> # Groups:         var [3]
#>   var      levels                          Model_OR
#>   <fct>    <fct>                         <row%-est>
#> 1 Constant Reference population 0.69 [0.63;0.74]***
#> 
#> 2 race     White                            1.00   
#> 3 race     Black                0.39 [0.36;0.42]***
#> 4 race     Other                0.95 [0.86;1.04]   
#> 
#> 5 age      age                  1.01 [1.01;1.01]***
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

For a logistic model you can instead fold the model-adjusted predicted
probability (`"prob"`) or the average marginal effect (`"ame"`) into the
odds-ratio cell (these two need the `marginaleffects` package). Note the
difference: `estimate_display = "ame"` *keeps* the odds-ratio column and
adds the AME beside it, whereas `effect = "ame"` (seen above) turns the
**whole** column into an AME.

**`multiplier`** rescales a continuous predictor’s effect. A one-year
change in `age` barely moves the odds, so its odds ratio sits near 1;
`multiplier = c(age = 10)` reports the effect **per 10 years** instead
(the odds ratio raised to the power 10, with its interval scaled to
match):

``` r
tab_reg(gss_simple, "married", c("race", "age"), multiplier = c(age = 10))
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors | Model fit   | married |
#> |:-----------|:------------|--------:|
#> | race, age  | N           |  21 407 |
#> |            | LR vs null  |  <0.01% |
#> |            | McFadden R2 |   0.023 |
#> |            | AIC         |  28 933 |
#> |            | BIC         |  28 965 |
#> 
#> # A tabxplor tab: 5 × 3
#> # Groups:         var [3]
#>   var      levels                Model_OR
#>   <fct>    <fct>                <row%-or>
#> 1 Constant Reference population 1/1.46***
#> 
#> 2 race     White                     1   
#> 3 race     Black                1/2.58***
#> 4 race     Other                1/1.05   
#> 
#> 5 age      age (per 10)           1.09***
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```

**`at = "reference"`** evaluates the effect at the reference profile
(the abstract population combined the reference level of all predictors)
instead of averaging it over the sample — a marginal effect *at the
reference* for `effect = "ame"`, or a “category vs. the rest” odds ratio
at that profile for a multinomial model:

``` r
tab_reg(gss_simple, "married", c("race", "age"), effect = "ame", at = "reference")
```

## Reading the footer statistics

The footer summarises model fit; which statistics appear depends on the
family:

- **N** — the number of observations used.
- **LR vs null** — a likelihood-ratio test of the whole model against
  the intercept-only model (is the model worth anything?).
- **McFadden R²** — a pseudo-R² for binomial and poisson models (higher
  = the predictors explain more; values are much smaller than a linear
  R²).
- **AIC / BIC** — information criteria for comparing models on the same
  data (lower = better; BIC penalises complexity more).
- Linear models (gaussian) add the usual **R² / adjusted R² / F / σ**;
  Poisson adds a **dispersion** check.

For weighted models a reduced set is reported (a Wald test against the
null, Nagelkerke / Cox–Snell pseudo-R², a Rao–Scott AIC), because the
likelihood-based quantities do not apply to a design-based fit.

## Plots

[`or_plot()`](https://bricenocenti.github.io/tabxplor/fr/reference/or_plot.md)
draws a forest plot of the odds ratios of a finished table, and
[`lm_plots()`](https://bricenocenti.github.io/tabxplor/fr/reference/lm_plots.md)
draws the standard 2×2 linear-model diagnostics:

``` r
t <- tab_reg(gss_simple, "married", c("race", "age"))
or_plot(t)

m <- tab_reg(gss_simple, "age", c("race", "marital"), family = "gaussian")
lm_plots(m)
```

## Where to go next

- [`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
  is also available **without writing R code**, as a point-and-click
  **Regressions** analysis in the
  [jamovi](https://www.jamovi.org/download) module — install *tabxplor*
  from jamovi’s module library (see
  [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor.md)).
- [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor.md)
  — cross-tables and the colour helpers.
- [`?tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
  for every argument (grouped by purpose), and
  [`?tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
  → *Details* for the modelling choices.
- [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  for complex designs (calibration, replicate weights, finite-population
  corrections).
