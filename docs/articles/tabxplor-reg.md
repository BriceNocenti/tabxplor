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

# As in the introduction vignette, tables render as tabxplor's real html tables (the recommended
# everyday setting); the shared stylesheet is emitted once by tab_css() below, tooltips kept off.
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

options(cli.num_colors = 256)
set_color_palette(theme = "light")
```

For the most common regression models,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
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
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
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
being unequal”, for that predictor.

- 2 levels logistic (binomial) → observed %, and observed odd-ratios
  (not modelised: calculated from percentages only)
- 3+ levels logistic (multinomial) → the observed ORs are shown as a
  tooltip on the model cells in html exports
- gaussian (linear) → group means and their difference
- poisson (counts) → observed rate and observed rate ratio

## Logistic regression (a binary factor)

When the outcome is a two-level factor, here “married” versus “not
married”,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
chooses a **binomial** family to fit a logistic regression and reports
**odds ratios** (the reference level of each predictor shows the neutral
value `1`).

- Like in every regression model, the effect of a predictor level on the
  dependent variable reads “all the others chosen predictors being
  equals”.
- Colors read like any `tabxplor` table: an odds ratio above 1 (blue)
  means *more likely to be married* than the reference level, below 1
  (red) means *less likely*; stars and colors both flag significance.

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"))
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

Logistic regression: married by race, age +2 more

[TABLE]

For a simple logistic regression, `empirical = TRUE` add **raw
percentages** and **crude unmodelised odds-ratios** for each predictor.

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), empirical = TRUE)
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

Logistic regression: married by race, age +2 more

[TABLE]

The raw percentages `Obs_%` are a summary of the base observed results
everything in the model is derived from :“28% of black americans are
married, compared to 51% for white americans”

- The plain difference from reference is colored (28% - 51% = -23%).
- Cells that are non-significantly different from the reference are
  greyed out (based on a Newcombe confidence interval for differences of
  proportions).

The **modelised odds-ratios** `Model_OR` directly compared to the
**observed odds-ratios** `Obs_OR` :

- Comparing the two tells you what adjustment the model did: if a
  predictor’s **model** OR is much closer to 1 than its **crude** OR,
  the raw association was largely explained by the other predictors.
- Here, “all things being unequal”, black americans have 2.68 lower odds
  to be married than white americans. “All things being equal” (more
  precisely : income, age and religion being equal), black americans
  still have 2.4 lower odds to be married than white americans. The
  result holds, it’s not explained by income differences or religious
  differences.

The **observed odds-ratios** `Obs_OR` are the same as those you can
calculate from percentages only in a crosstable :

- Colors and significance stars use a **Woolf OR confidence interval**
  that matches what is done in the regression model.
- The population of the table must match the complete-cases population
  of the model, filtering out indivivuals with `NA` at any involved
  variable.

``` r
gss_simple |> 
  dplyr::filter(dplyr::if_all(all_of(c("race", "age", "rincome", "relig")), ~ !is.na(.) )) |> 
  tab(race, married, pct = "row", na = "drop", 
    OR = "OR", color = "OR", color_signif = "grey_non_signif"
   )
```

[TABLE]

### Average marginal effects (AME) of a logistic regression

Another way to interpret a logistic regression is to use **average
marginal effects** (AME) with `effect = "ame"` :

- Instead of directly reading the modelised odd-ratio we can infer, from
  it, the average difference of percentage of each level compared to the
  reference.
- Here `Model_AME` reads like this : “Comparing Black and White
  respondents who are alike in income, age and religion, being Black is
  associated with a marriage rate 19.8 points lower, on average.”

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), effect = "ame", empirical = TRUE)
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

Logistic regression: married by race, age +2 more

[TABLE]

Then we can use `empirical = TRUE` to compare this new modelised
quantity to the **raw, observed difference of percentages** :

- In the sample (`Obs_diff`), we observe a rate of marriages 23 points
  lower for black americans compared to white american
  (`28% - 51% = -23%`).
- The modelised difference `Model_AME` is very close, `-19.8%`, so very
  little of this difference between black and white can be explained by
  age, income or religion.

We can also compare the raw percentage `Obs_%` with the **model-adjusted
probability** — the value in parentheses in the `Model_AME (adjusted %)`
column :

- Adjusted percentage, which is a result of the model, reads this way :
  “if the whole sample kept its income/age/religion distribution but
  everyone were Black, an estimated 31.5% would be married, vs 51.3% if
  everyone were White”.
- Blacks marry at 30.9% in this sample (observed); standardizing their
  income/age/religion to the population mix moves that only to 31.5% —
  so almost none of the Black–White gap is accounted for by differences
  in those three variables; it persists after adjustment.

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

[TABLE]

## Grouped-binomial outcomes (a summed score)

When the outcome is a **summed score** — how many of several yes/no
items a respondent answered one way — you model the number of
“successes” out of a fixed number of items with `trials =`.
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
then fits `cbind(score, trials - score)` as a binomial, so the odds
ratios read on the *per-item* probability.
[`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/reference/score_from_lv1.md)
(see
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md))
builds such a score by counting factors at their first level:

``` r
gss_simple |>
  score_from_lv1("score", vars_list = c("married", "income25k")) |>   # a 0–2 score
  tab_reg("score", c("race", "age"), family = "binomial", trials = 2)
```

Logistic regression: score by race, age

[TABLE]

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

Ordinal logistic regression: rincome by race, age +1 more

[TABLE]

A nominal outcome with three or more unordered levels is fit as one
**multinomial** logistic model, giving one odds-ratio column per outcome
category versus the reference category (also called relative risks
ratios) :

``` r
tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"))
#> ℹ "party3": nominal outcome detected -> `family = "multinomial"` (multinomial
#>   logistic).
```

Multinomial logistic regression: party3 by race, age +2 more

[TABLE]

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

Multinomial logistic regression: party3 by race, age +2 more

[TABLE]

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

Linear regression: age by race, marital +2 more

[TABLE]

In the case of a linear regression, the **empirical/observed counterpart
of the model coefficient** for a categorical predictor is simply the
**difference of means** : here, the difference of mean age, per level of
the predictor, compared to the reference level.

``` r
tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian", empirical = TRUE)
```

Linear regression: age by race, marital +2 more

[TABLE]

The empirical mean ages, and differences of mean ages from reference,
can be computed in a simple table with :

``` r
tab(gss_simple, "race", "age", pct = "row", digits = 2, na = "drop",
    color = "diff", ref = 1,  method_mean_diff = "student"
) |> 
  mutate(diff = set_display(age, "diff"))
```

[TABLE]

``` r
# student : confidence intervals with pooled variance, to match those computed by linear regression
```

## Poisson regression (a count outcome)

``` r
tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson")
```

Poisson regression: tvhours by race, marital +2 more

[TABLE]

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

Poisson regression: tvhours by race, marital +2 more

[TABLE]

The empirical average TV hours per day, and differences of rate of
television watching from reference, can be computed in a simple table
with :

``` r
tab(gss_simple, "race", "tvhours", pct = "row", digits = 2, na = "drop",
    color = "ratio", ref = 1,  method_mean_ratio = "quasipoisson"
) |> 
  mutate(IRR = set_display(tvhours, "ratio"))
```

[TABLE]

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
to [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)’s
cheaper `options(tabxplor.kish_neff = TRUE)` approximation — see
[`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md).)

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
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md). Set
it to `TRUE` when using
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
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

Logistic regressions (models comparison): married, ‘01-Married’ (OR)

[TABLE]

## The same model within sub-populations

`split_var =` is the regression analogue of
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)’s
`tab_vars`: the same model is fitted **within each level** of a grouping
variable, and the per-group tables are stacked into one grouped table.
It answers “does this effect hold in every subgroup?”. With just one
dependent variable, it uses
[`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
internally to lay the groups out as side-by-side columns for an easy
comparison :

``` r
tab_reg(gss_simple, "married", c("race", "rincome"), split_var = "year")
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

Logistic regression: married by race, rincome (tabbed by year)

[TABLE]

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

Logistic regression: married by race, age

[TABLE]

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

Logistic regression: married by race, age

[TABLE]

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

[`or_plot()`](https://bricenocenti.github.io/tabxplor/reference/or_plot.md)
draws a forest plot of the odds ratios of a finished table, and
[`lm_plots()`](https://bricenocenti.github.io/tabxplor/reference/lm_plots.md)
draws the standard 2×2 linear-model diagnostics:

``` r
t <- tab_reg(gss_simple, "married", c("race", "age"))
or_plot(t)

m <- tab_reg(gss_simple, "age", c("race", "marital"), family = "gaussian")
lm_plots(m)
```

## Where to go next

- [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  is also available **without writing R code**, as a point-and-click
  **Regressions** analysis in the
  [jamovi](https://www.jamovi.org/download) module — install *tabxplor*
  from jamovi’s module library (see
  [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)).
- [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
  — cross-tables and the colour helpers.
- [`?tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  for every argument (grouped by purpose), and
  [`?tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  → *Details* for the modelling choices.
- [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  for complex designs (calibration, replicate weights, finite-population
  corrections).
