# Compare several logistic-regression models (odds ratios side by side)

Convenience wrapper of
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
for the binomial family in model-comparison mode: fits several models
for ONE binary `dependent`, one per named predictor set in `models`, and
returns a `tabxplor` table with one odds-ratio column per model
(predictors absent from a model left blank).

## Usage

``` r
multi_logit(
  data,
  dependent,
  models,
  wt = NULL,
  ids = NULL,
  strata = NULL,
  fpc = NULL,
  nest = FALSE,
  inverse_two_level_factors = TRUE,
  split_var = NULL,
  multiplier = NULL,
  empirical = FALSE,
  conf_level = getOption("tabxplor.conf_level", 0.95),
  method = c("wald", "profile"),
  stats = NULL,
  compare = c("none", "baseline", "sequential"),
  baseline = NULL,
  estimate_display = c("value", "ci", "prob", "ame"),
  color_signif = c("grey_non_signif", "ignore", "guaranteed_effect"),
  stars = TRUE,
  na = c("drop_by_model", "drop_all_models"),
  cleannames = NULL,
  subtext = ""
)
```

## Arguments

- data:

  A data frame, **or a prebuilt survey design**
  ([`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  /
  [`survey::svrepdesign()`](https://rdrr.io/pkg/survey/man/svrepdesign.html)).
  When a design is passed, its weights (and clustering / stratification
  / calibration) drive the estimation and `wt` / `ids` / `strata` /
  `fpc` are ignored.

- dependent:

  Character. Name of the binary dependent variable. May be a **vector**
  of several binary dependents: the model comparison is then run once
  per dependent and the per-dependent tables are returned as a list (one
  sheet each when exported to Excel).

- models:

  A named list of character vectors; each element is one model's
  predictor set and its name labels the column. Unnamed elements are
  labelled `model1`, `model2`, ...

- wt:

  Optional. Name of a weight column (character). Switches to
  design-based survey estimation
  ([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)):
  the sandwich standard errors are scale-invariant, so raw population
  weights are handled correctly (no normalisation) and the point
  estimates match the weighted crosstabs.

- ids, strata, fpc:

  Optional survey-design specification for the `wt` path (each a column
  name / character vector, or a formula such as `~psu` / `~region`).
  `ids` gives the cluster identifier(s) from largest to smallest stage
  (default no clustering); `strata` the stratifying variable(s); `fpc`
  the finite-population correction. Give correct
  clustering/stratification for honest design-based variances (a flat
  `ids = ~1` can understate them). Ignored when `data` is a design.

- nest:

  Logical. Passed to
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html):
  set `TRUE` when cluster ids are reused across strata. Default `FALSE`.

- inverse_two_level_factors:

  Logical, binomial only. If `TRUE` (default), models the FIRST level of
  a 2-level factor dependent (e.g. `"1-Married"` before
  `"2-Not married"`).

- split_var:

  Optional. Name of a grouping variable (character): the regression
  analogue of
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  `tab_vars`. The same model(s) are fitted **within each level** of this
  variable and the per-group tables are stacked into one grouped table
  (grouped by `split_var`), sharing the variable/level stub. Use
  [`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
  on `split_var` to pivot the groups into side-by-side columns for an
  easy across-group comparison. A level absent from a group shows empty
  cells.

- multiplier:

  Optional named numeric vector `c(var = k)` rescaling a **continuous**
  predictor's effect to a k-unit change (e.g. `c(age = 10)` shows the
  odds ratio / beta per decade of age = OR^10 / beta\*10). The
  confidence interval scales with it; the p-value is unchanged. Names
  must be numeric predictors; not available for multinomial / ordinal
  outcomes.

- empirical:

  Logical. If `TRUE`, adds the descriptive **crude** (unadjusted,
  single-predictor) companion of the model effect for each
  factor-predictor level – the unadjusted bivariate association, which
  IS the modelised quantity when there is a single predictor (the
  standard "crude vs adjusted" comparison; a large gap signals
  confounding). Per family: **binomial** adds `Obs_%`

  - `Obs_OR` (coefficient) or `Obs_%` + `Obs_diff` (AME); **gaussian**
    adds `Obs_mean` + `Obs_diff`; **poisson** adds `Obs_rate` +
    `Obs_IRR`; **multinomial** shows the crude % + difference per
    category in the HTML tooltip (columns would explode). By design
    every crude quantity is computed on **exactly the same complete-case
    population as the model** (listwise-complete on the dependent, all
    predictors and any design variable), so crude and adjusted are
    directly comparable and not confounded by differing missingness
    (reproduce it with
    [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html) +
    [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
    on the same rows). Also works with a vector of dependents. Ordinal
    has no clean crude analogue and is ignored (with a message). These
    crude companion CIs are descriptive, so on weighted data they honour
    `options(tabxplor.kish_neff = TRUE)` (Kish's effective sample size)
    exactly like
    [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md);
    the model column's own CI is always design-based
    ([`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html)) and
    unaffected. Default `FALSE`.

- conf_level:

  Confidence level for the intervals. Default `0.95`.

- method:

  How the interval and p-value are computed. `"wald"` (default) uses the
  Wald interval and the Wald z / t test: fast, matches standard software
  output, and the only option for weighted models. `"profile"` uses the
  profile-likelihood interval
  ([`stats::confint()`](https://rdrr.io/r/stats/confint.html), needs
  `MASS`) and the likelihood-ratio test: more accurate near separation,
  unweighted binomial/poisson models only (else it falls back to Wald
  with a message; gaussian always uses the exact-t interval).

- stats:

  The goodness-of-fit statistics shown in the model-summary **footer**
  (one block per model). `NULL` (default) uses the per-family set:
  linear models show N, R square, adjusted R square, the overall F-test
  and the residual SD; other models show N, the likelihood-ratio test
  versus the null model, McFadden's pseudo-R square, AIC and BIC
  (poisson / grouped-binomial models also show the Pearson dispersion).
  Pass a character vector to pick and order the statistics (`"n"`,
  `"lr_null"`, `"mcfadden_r2"`, `"aic"`, `"bic"`, `"dispersion"`,
  `"r2"`, `"r2_adj"`, `"f_model"`, `"sigma"`), or `FALSE` / `"none"` to
  hide the footer. Weighted models show a reduced, survey-appropriate
  set (design-based Wald test, Nagelkerke pseudo-R square, AIC).

- compare:

  Add a **model-comparison** footer row (only with several models /
  dependents). `"none"` (default) adds nothing; `"baseline"` tests each
  model against the `baseline` column; `"sequential"` tests each model
  against the previous one. Uses a likelihood-ratio test (F for linear /
  quasi models, a design-based Wald test for weighted / survey models);
  when the models are not nested or fit on different numbers of
  observations it falls back to the AIC difference with a message.

- baseline:

  For `compare = "baseline"`: which column is the reference model (its
  label, or a position). Defaults to the first model.

- estimate_display:

  What each effect cell shows beside the estimate. `"value"` (default)
  the plain estimate (e.g. `2.34`); `"ci"` adds a visible
  confidence-interval bracket (`2.34 [1.20; 4.50]`, any family);
  `"prob"` folds the model-adjusted predicted probability into the
  odds-ratio cell (`2.34 (16%)`); `"ame"` folds the average marginal
  effect (`2.34 (+8%)`). `"prob"`/`"ame"` need the `marginaleffects`
  package and apply to binomial (logistic) coefficient models only (they
  degrade to `"ci"` otherwise, with a message). Note
  `estimate_display = "ame"` *adds* an AME beside the odds ratio,
  whereas `effect = "ame"` makes the whole column an AME (probability
  scale); the two are different and, when both are set, `effect = "ame"`
  wins and `estimate_display` is reset to `"value"`.

- color_signif:

  How significance drives the colours. `"grey_non_signif"` (default)
  colours only odds ratios whose confidence interval excludes 1 and
  greys the rest.

- stars:

  Logical (default `TRUE` for regression tables, where significance
  stars are standard). When `FALSE`, the per-cell p-value is dropped and
  no stars are shown (colours still read the CI).

- na:

  How missing values are handled across models. `"drop_by_model"`
  (default) drops `NA` rows per model (each model / dependent uses its
  own complete cases). `"drop_all_models"` fits every model on ONE
  shared complete-case population (rows with no `NA` on any predictor /
  dependent / design variable), so nested models get equal N and the
  likelihood-ratio comparison can run; note this **changes all
  estimates** (shared population), hence opt-in. Ignored for a prebuilt
  survey design.

- cleannames:

  Logical. If `TRUE`, strips numeric prefixes from factor levels for
  display. Uses `getOption("tabxplor.cleannames")` when `NULL`.

- subtext:

  Optional character. A note shown below the table.

## Value

A `tabxplor_grouped_tab` (grouped by predictor), one odds-ratio column
per model; or, for several `dependent`s, a `tabxplor_tabs` list of such
tables (one per dependent).

## Examples

``` r
data <- forcats::gss_cat |>
  dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
                                                "Married", "Not married")))
if (requireNamespace("broom", quietly = TRUE)) {
  multi_logit(
    data, dependent = "married",
    models = list(demographic = c("race", "age"),
                  full        = c("race", "age", "rincome"))
  )
}
#> | Model fit   | demographic |   |   full |
#> |:------------|------------:|:-:|-------:|
#> | N           |      21 407 |   | 21 407 |
#> | LR vs null  |      <0.01% |   | <0.01% |
#> | McFadden R2 |       0.023 |   |  0.039 |
#> | AIC         |      28 933 |   | 28 494 |
#> | BIC         |      28 965 |   | 28 645 |
#> 
#> # A tabxplor tab: 21 × 4
#> # Groups:         var [4]
#>    var      levels               demographic      full
#>    <fct>    <fct>                  <row%-or> <row%-or>
#>  1 Constant Reference population   1/1.54*** 1/1.82***
#> 
#>  2 race     Other                       1         1   
#>  3 race     Black                  1/2.45*** 1/2.47***
#>  4 race     White                    1.05      1.01   
#> 
#>  5 age      age                      1.01***   1.01***
#> 
#>  6 rincome  No answer                             1   
#>  7 rincome  Don't know                         1.21   
#>  8 rincome  Refused                            1.72***
#>  9 rincome  $25000 or more                     1.48** 
#> 10 rincome  $20000 - 24999                     1.02   
#> 11 rincome  $15000 - 19999                   1/1.05   
#> 12 rincome  $10000 - 14999                   1/1.11   
#> 13 rincome  $8000 to 9999                    1/1.32   
#> 14 rincome  $7000 to 7999                    1/1.44   
#> 15 rincome  $6000 to 6999                    1/1.11   
#> 16 rincome  $5000 to 5999                    1/1.12   
#> 17 rincome  $4000 to 4999                    1/1.25   
#> 18 rincome  $3000 to 3999                    1/1.43*  
#> 19 rincome  $1000 to 2999                    1/1.46** 
#> 20 rincome  Lt $1000                         1/1.34   
#> 21 rincome  Not applicable                   1/1.29   
#> # Model: logistic regression of married ('Married'); odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```
