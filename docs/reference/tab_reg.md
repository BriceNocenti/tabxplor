# Regression table (effect measures) as a tabxplor table

Fits one regression model per column and returns a `tabxplor` table of
the per-family effect measure – linear **beta** (gaussian), **odds
ratios** (binomial / logistic), **incidence-rate ratios** (poisson),
**multinomial odds ratios** (one column per outcome category vs the
reference, nominal 3+ level), **cumulative odds ratios** (ordinal /
proportional-odds) – one row per predictor level (the reference level
shown as the neutral value `0` or `1`), grouped by predictor. Each cell
stores the estimate, its confidence interval and p-value, so the table
prints with significance stars, greys out non-significant effects, and
exports (kable / Markdown / Excel) like any `tabxplor` crosstab.

## Usage

``` r
tab_reg(
  data,
  dependent,
  predictors = NULL,
  split_var = NULL,
  wt = NULL,
  family = "auto",
  effect = c("coefficient", "ame"),
  at = c("average", "reference"),
  exponentiate = TRUE,
  trials = NULL,
  empirical = FALSE,
  color = TRUE,
  color_signif = NULL,
  stars = TRUE,
  conf_level = getOption("tabxplor.conf_level", 0.95),
  method = c("wald", "profile"),
  reference = NULL,
  inverse_two_level_factors = TRUE,
  multiplier = NULL,
  stats = NULL,
  compare = c("none", "baseline", "sequential"),
  baseline = NULL,
  na = c("drop_by_model", "drop_all_models"),
  estimate_display = c("value", "ci", "prob", "ame"),
  cleannames = NULL,
  subtext = "",
  spread_models = TRUE,
  ids = NULL,
  strata = NULL,
  fpc = NULL,
  nest = FALSE,
  .fit_cache = NULL
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

  Character outcome variable name(s), **or a model formula** (the escape
  hatch). With a `predictors` character vector, several names give one
  effect column per outcome; with a `predictors` list, a single name is
  required. A formula supplies its own model (leave `predictors` unset).

- predictors:

  Either a character vector of predictor names (one model), or a **named
  list** of character vectors (one model per element, its name labelling
  the column). Leave `NULL` when `dependent` is a formula.

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

- wt:

  Optional. Name of a weight column (character). Switches to
  design-based survey estimation
  ([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)):
  the sandwich standard errors are scale-invariant, so raw population
  weights are handled correctly (no normalisation) and the point
  estimates match the weighted crosstabs.

- family:

  The model family, **resolved per dependent** so several outcomes with
  different families can share one table (one effect column-group each).
  `"auto"` (default) detects each outcome: a binary (-\> `"binomial"`),
  an ordered 3+ level (-\> `"ordinal"`), a nominal 3+ level (-\>
  `"multinomial"`), or a continuous (-\> `"gaussian"`) outcome, emitting
  a message; an integer count stays ambiguous and must be named (for
  that outcome only). Set it explicitly with `"gaussian"` (linear),
  `"binomial"` (logistic), `"poisson"` / `"quasipoisson"` (counts),
  `"multinomial"` (nominal 3+ level), `"ordinal"` (ordered 3+ level). A
  **scalar** applies to every dependent; a **vector** aligned to
  `dependent`, or a **named** vector keyed by dependent (e.g.
  `c(income = "poisson", satisfied = "binomial")`), sets one family per
  outcome. Mixed families work only with a character `predictors` (one
  model per outcome); a `predictors` list (model comparison) is
  single-outcome, hence single-family.

  **Over-dispersed counts.** An unweighted `"poisson"` fit auto-scales
  its standard errors by the square root of the Pearson dispersion, so
  with an over-dispersed outcome (dispersion clearly above 1) its CIs
  and p-values are **identical to `"quasipoisson"`**, and it **warns**
  to say so (the footer reports the dispersion). At equidispersion
  (\\\approx\\1) the scaling is a no-op and the result matches a
  standard `glm(family = poisson)` z-interval — so a user comparing to a
  hand-fit Poisson `glm` is not surprised by wider intervals.

- effect:

  The interpretation scale, orthogonal to `family`. `"coefficient"`
  (default) shows the native per-family effect (beta / OR / IRR /
  cumulative-OR). `"ame"` shows **average marginal effects** with the
  **adjusted predicted probability** in parentheses (e.g.
  `-8%*** (16%)`): a probability-scale, cross-model-comparable summary
  (Mood 2010) for logistic / multinomial / ordinal outcomes (percentage
  points), the expected-count change for poisson, and the coefficient
  itself for gaussian. The parenthetical is a *marginal-standardized*
  prediction (`avg_predictions(variables=)`: the predictor set to each
  level for the whole sample, other covariates kept as observed, then
  averaged), so it is genuinely covariate-adjusted and coheres with the
  effect — adjusted-%(reference)

  - AME(level) equals adjusted-%(level). Read it as a standardized
    comparison ("holding the measured covariates' distribution fixed"),
    not a manipulation. Requires the `marginaleffects` package. A
    multinomial / ordinal outcome gets one AME column per outcome
    category.

- at:

  Where the profile-conditional quantities are evaluated (needs
  `marginaleffects`). `"average"` (default) is the sample average (the
  AME / adjusted prediction over the data). `"reference"` evaluates at
  the **reference profile** — every other predictor held at its
  reference (factor first level, numeric mean): for `effect = "ame"`
  this gives the marginal effect *at reference* (MER) with the adjusted
  prediction there; for a **multinomial** `effect = "coefficient"` it
  gives the odds ratio of each outcome category *versus the rest* at
  that profile (one column per category). It has no effect on ordinary
  coefficients (they are profile-independent). Note the reference
  profile can be an unusual baseline (e.g. a factor's first level =
  `"No answer"`).

- exponentiate:

  Logical. `TRUE` (default) exponentiates coefficients into ratios (odds
  ratios for logistic, incidence-rate ratios for poisson, cumulative
  odds ratios for ordinal), automatically leaving gaussian linear betas
  on their raw scale. `FALSE` keeps every coefficient on the coefficient
  (log / linear) scale. Ignored when `effect = "ame"` (marginal effects
  are always on the response scale).

- trials:

  Grouped-binomial (summed-score) outcomes only. The number of items
  behind the score, fitting `cbind(score, trials - score)` as a
  binomial. `NULL` (default) fits an ordinary binary logit; a single
  integer (or a vector named by dependent) sets the item count; `TRUE`
  uses each dependent's observed maximum score. Requires
  `family = "binomial"`.

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

- color, color_signif:

  Colouring of the effect cells. `color = TRUE` (default) auto-picks the
  sensible per-family measure (`"OR"` magnitude for ratios, standardized
  `"diff"` for betas); `color = FALSE` turns colouring off for every
  column (model and empirical). Power users may pass a measure string
  (`"OR"`, `"diff"`, `"ratio"`, `"no"`) to override. `color_signif` is
  the significance policy (default `"grey_non_signif"`). See
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- stars:

  Logical (default `TRUE` for regression tables, where significance
  stars are standard). When `FALSE`, the per-cell p-value is dropped and
  no stars are shown (colours still read the CI).

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

- reference:

  Optional named vector `c(var = "baseline level")` choosing the
  treatment-contrast reference level of one or more factor predictors
  (the effect of every other level is measured against it). For a
  **multinomial** outcome, keying the vector by the outcome name (e.g.
  `c(partyid = "Independent")`) also sets the baseline outcome category
  all the OR columns are compared against. This is how factor contrasts
  are set; other contrast codings can be applied by passing a formula in
  `dependent` with the terms already coded.

- inverse_two_level_factors:

  Logical, binomial only. If `TRUE` (default), models the FIRST level of
  a 2-level factor dependent (e.g. `"1-Married"` before
  `"2-Not married"`).

- multiplier:

  Optional named numeric vector `c(var = k)` rescaling a **continuous**
  predictor's effect to a k-unit change (e.g. `c(age = 10)` shows the
  odds ratio / beta per decade of age = OR^10 / beta\*10). The
  confidence interval scales with it; the p-value is unchanged. Names
  must be numeric predictors; not available for multinomial / ordinal
  outcomes.

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

- na:

  How missing values are handled across models. `"drop_by_model"`
  (default) drops `NA` rows per model (each model / dependent uses its
  own complete cases). `"drop_all_models"` fits every model on ONE
  shared complete-case population (rows with no `NA` on any predictor /
  dependent / design variable), so nested models get equal N and the
  likelihood-ratio comparison can run; note this **changes all
  estimates** (shared population), hence opt-in. Ignored for a prebuilt
  survey design.

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

- cleannames:

  Logical. If `TRUE`, strips numeric prefixes from factor levels for
  display. Uses `getOption("tabxplor.cleannames")` when `NULL`.

- subtext:

  Optional character. A note shown below the table.

- spread_models:

  Logical, only used with `split_var`. If `TRUE` (default), a single
  non-multinomial model fitted within each `split_var` level is
  automatically pivoted with
  [`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
  so the per-group models sit side by side as columns. `FALSE` keeps the
  stacked grouped table (one block of rows per group).

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

- .fit_cache:

  Internal, for the jamovi live UI (Phase 15b): a mutable cache
  environment (see `jmvreg_cache_env()`) that memoizes fitted models so
  display / colour / reference toggles avoid a refit. On the
  single-equation GLM coefficient path a factor-predictor reference
  change is reparametrized from the cached fit (no refit). `NULL` (the
  default) leaves ordinary calls unchanged.

## Value

A `tabxplor_grouped_tab` (grouped by predictor), one effect column per
model / dependent.

## Details

New to regressions with tabxplor? A first model needs only three
arguments: `data`, `dependent` (the outcome) and `predictors`. tabxplor
picks the right model from the outcome's type — a two-level factor gives
logistic **odds ratios**, a numeric gives linear **betas**, a count
gives Poisson **rate ratios**, and a 3+ level factor gives multinomial
or ordinal odds ratios — so you rarely set `family` by hand. Add
`empirical = TRUE` to show the crude (unadjusted) effect beside the
model's adjusted one. See
[`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)
for a guided tour.

The arguments fall into groups:

- **The model**: `data`, `dependent`, `predictors` (a character vector =
  one model; a named list = several models to compare), `family`
  (usually detected), `wt` (survey weights).

- **What each cell shows**: `exponentiate`, `effect` (`"coefficient"` or
  average marginal effect `"ame"`), `estimate_display`, `empirical`
  (crude vs adjusted effect).

- **Colors & significance**: `color`, `color_signif`, `stars`,
  `conf_level` — as in
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- **Comparisons & structure**: `reference` (baseline levels), `compare`
  / `baseline` (model comparison test), `split_var` (one table per
  group), `multiplier` (effect per *k* units).

- **Survey design**: `wt`, `ids`, `strata`, `fpc`, `nest`, or pass a
  prebuilt design as `data`.

- **Diagnostics**: `stats` (footer statistics), and the plots
  [`or_plot()`](https://bricenocenti.github.io/tabxplor/reference/or_plot.md)
  /
  [`lm_plots()`](https://bricenocenti.github.io/tabxplor/reference/lm_plots.md).

`predictors` selects the mode: a **character vector** fits one model,
and `dependent` may itself be a vector -\> one column per dependent; a
**named list** of predictor sets fits one model each -\> one column per
model (predictors absent from a model are left blank), for comparing
specifications.

`effect = "ame"` switches from the native coefficient to **average
marginal effects** with the adjusted **predicted probability** shown in
parentheses (e.g. `-8%*** (16%)`) – a probability-scale,
cross-model-comparable interpretation (Mood 2010), computed with the
`marginaleffects` package. `at = "reference"` instead evaluates at a
**reference profile** (other predictors held at their reference level /
mean): the marginal effect *at reference*, or – for a multinomial
`effect = "coefficient"` – the odds ratio of each outcome category
*versus the rest* at that profile.

Unweighted models use [`stats::lm()`](https://rdrr.io/r/stats/lm.html) /
[`stats::glm()`](https://rdrr.io/r/stats/glm.html); a `wt` weight column
switches to a survey design
([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)),
which gives correct design-based standard errors rather than the
frequency-inflated ones of `glm(weights=)`. `broom` (always) and
`survey` (only with `wt`) are optional dependencies.
[`tab_logit()`](https://bricenocenti.github.io/tabxplor/reference/tab_logit.md)
/
[`multi_logit()`](https://bricenocenti.github.io/tabxplor/reference/multi_logit.md)
are convenience wrappers for the binomial family.

A **nominal** outcome with 3+ unordered levels is fit as one multinomial
logit
([`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)),
giving **one odds-ratio column per non-reference outcome category**
("`<category>` vs `<reference>`: OR"). An **ordered** outcome with 3+
levels is fit as a proportional-odds cumulative logit
([`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html)), giving one
cumulative-odds-ratio column; the parallel-lines assumption is tested
with the Brant test (install the `brant` package) and a warning is
issued if it is violated. (Weighted 3+ level models are planned for a
later release.)

A **summed-score** outcome (a count of "yes" answers out of a fixed
number of items) is fit as a grouped binomial when you pass `trials`
(the number of items). Power users can pass a **model formula** as
`dependent` – `tab_reg(data, y ~ x1 + poly(x2, 2) + x1:x3)` – driving
the model directly; simple `y ~ a + b` formulas behave exactly like
`dependent = "y"`, `predictors = c("a", "b")`, while interactions /
[`poly()`](https://rdrr.io/r/stats/poly.html) /
[`I()`](https://rdrr.io/r/base/AsIs.html) terms render as best-effort
term rows.

## Examples

``` r
  data <- gss_cat_data_formatting()

  # logistic (odds ratios):
  tab_reg(data, dependent = "married", predictors = c("race", "rincome"), family = "binomial")
#> | predictors    | Model fit   | married |
#> |:--------------|:------------|--------:|
#> | race, rincome | N           |  13 015 |
#> |               | LR vs null  |  <0.01% |
#> |               | McFadden R2 |   0.031 |
#> |               | AIC         |  17 483 |
#> |               | BIC         |  17 528 |
#> 
#> # A tabxplor tab: 8 × 3
#> # Groups:         var [3]
#>   var      levels                Model_OR
#>   <fct>    <fct>                <row%-or>
#> 1 Constant Reference population 1/1.51***
#> 
#> 2 race     White                     1   
#> 3 race     Black                1/2.33***
#> 4 race     Other                1/1.04   
#> 
#> 5 rincome  1-Lt $10000               1   
#> 6 rincome  2-$10000 to 14999      1.21***
#> 7 rincome  3-$15000 to 24999      1.33***
#> 8 rincome  4-$25000 or more       2.07***
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
  # linear (betas):
  tab_reg(data, dependent = "tvhours", predictors = c("rincome", "age"), family = "gaussian")
#> | predictors   | Model fit   | tvhours |
#> |:-------------|:------------|--------:|
#> | rincome, age | N           |   6 819 |
#> |              | F           |  <0.01% |
#> |              | R2          |   0.037 |
#> |              | Adjusted R2 |   0.037 |
#> |              | Residual SD |    2.05 |
#> 
#> # A tabxplor tab: 6 × 3
#> # Groups:         var [3]
#>   var      levels                Model_β
#>   <fct>    <fct>                  <coef>
#> 1 Constant Reference population  2.77***
#> 
#> 2 rincome  1-Lt $10000              0   
#> 3 rincome  2-$10000 to 14999    -0.14   
#> 4 rincome  3-$15000 to 24999    -0.32***
#> 5 rincome  4-$25000 or more     -0.95***
#> 
#> 6 age      age                   0.01***
#> # Model: linear regression; coefficients (mean difference vs the reference category).
#> # β (ref.): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8 [grey: non-significant or under ±0.2 SD]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
  # to use normal R model formulas instead (same model, terser):
  tab_reg(data, married ~ race + rincome, family = "binomial")
#> | predictors    | Model fit   | married |
#> |:--------------|:------------|--------:|
#> | race, rincome | N           |  13 015 |
#> |               | LR vs null  |  <0.01% |
#> |               | McFadden R2 |   0.031 |
#> |               | AIC         |  17 483 |
#> |               | BIC         |  17 528 |
#> 
#> # A tabxplor tab: 8 × 3
#> # Groups:         var [3]
#>   var      levels                Model_OR
#>   <fct>    <fct>                <row%-or>
#> 1 Constant Reference population 1/1.51***
#> 
#> 2 race     White                     1   
#> 3 race     Black                1/2.33***
#> 4 race     Other                1/1.04   
#> 
#> 5 rincome  1-Lt $10000               1   
#> 6 rincome  2-$10000 to 14999      1.21***
#> 7 rincome  3-$15000 to 24999      1.33***
#> 8 rincome  4-$25000 or more       2.07***
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.

# \donttest{
 # logistic : comparison between observed odds-ratio and modelised odds-ratio
  tab_reg(data, dependent = "married", predictors = c("race", "rincome"), family = "binomial",
          empirical = TRUE
  )
#> | predictors    | Model fit   | married |
#> |:--------------|:------------|--------:|
#> | race, rincome | N           |  13 015 |
#> |               | LR vs null  |  <0.01% |
#> |               | McFadden R2 |   0.031 |
#> |               | AIC         |  17 483 |
#> |               | BIC         |  17 528 |
#> 
#> # A tabxplor tab: 8 × 5
#> # Groups:         var [3]
#>   var      levels               `Obs_%`    Obs_OR  Model_OR
#>   <fct>    <fct>                 <row%> <row%-or> <row%-or>
#> 1 Constant Reference population                   1/1.51***
#> 
#> 2 race     White                 52%         1         1   
#> 3 race     Black                 31%*** 1/2.45*** 1/2.33***
#> 4 race     Other                 49%*   1/1.11*   1/1.04   
#> 
#> 5 rincome  1-Lt $10000           37%         1         1   
#> 6 rincome  2-$10000 to 14999     41%**    1.21**    1.21***
#> 7 rincome  3-$15000 to 24999     43%***   1.33***   1.33***
#> 8 rincome  4-$25000 or more      55%***   2.13***   2.07***
#> # Model: logistic regression; odds ratios (vs the reference category).
#> # Obs_%: difference (ref.): -30 -15 -5 +5 +15 +30 [grey: non-significant or under ±5 points]
#> # Obs_OR, Model_OR: OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
# average marginal effects + adjusted predictions (needs the marginaleffects package):
if (requireNamespace("marginaleffects", quietly = TRUE)) {
  tab_reg(data, dependent = "married", predictors = c("race", "rincome"),
                family = "binomial", effect = "ame")
  # marginal effects at the reference profile (others at their reference level / mean):
  tab_reg(data, dependent = "married", predictors = c("race", "rincome"),
                family = "binomial", effect = "ame", at = "reference")
}
#> | predictors    | Model fit   | married |
#> |:--------------|:------------|--------:|
#> | race, rincome | N           |  13 015 |
#> |               | LR vs null  |  <0.01% |
#> |               | McFadden R2 |   0.031 |
#> |               | AIC         |  17 483 |
#> |               | BIC         |  17 528 |
#> 
#> # A tabxplor tab: 8 × 3
#> # Groups:         var [3]
#>   var      levels                       Model_MER
#>   <fct>    <fct>                     <row%-mixed>
#> 1 Constant Reference population                  
#> 
#> 2 race     White                          (39.8%)
#> 3 race     Black                -17.7%*** (22.1%)
#> 4 race     Other                 -1.0%    (38.8%)
#> 
#> 5 rincome  1-Lt $10000                    (39.8%)
#> 6 rincome  2-$10000 to 14999     +4.7%*** (44.5%)
#> 7 rincome  3-$15000 to 24999     +7.0%*** (46.8%)
#> 8 rincome  4-$25000 or more     +17.9%*** (57.7%)
#> # Model: logistic regression; marginal effects on the probability scale (percentage points) at the reference profile (other predictors held at their reference level / mean); each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability.
#> # MER (ref.): -30 -15 -5 +5 +15 +30 [grey: non-significant or under ±5 points]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
# multinomial (nominal 3+ level): one OR column per outcome category vs the reference
  tab_reg(data, dependent = "party3", predictors = c("race", "age"),
                family = "multinomial", reference = c(party3 = "3-Republican"))
#> | predictors | Model fit   | party3 |
#> |:-----------|:------------|-------:|
#> | race, age  | N           | 21 261 |
#> |            | LR vs null  | <0.01% |
#> |            | McFadden R2 |  0.052 |
#> |            | AIC         | 42 495 |
#> |            | BIC         | 42 558 |
#> 
#> # A tabxplor tab: 5 × 4
#> # Groups:         var [3]
#>   var      levels               1-Democrat vs 3-Republi…¹ 2-Independent, other…²
#>   <fct>    <fct>                                <row%-or>              <row%-or>
#> 1 Constant Reference population                 1/1.15***                  11   
#> 
#> 2 race     White                                     1                      1   
#> 3 race     Black                                 10.30***                3.95***
#> 4 race     Other                                  2.78***                3.06***
#> 
#> 5 age      age                                    1.00**               1/1.01***
#> # ℹ abbreviated names: ¹​`1-Democrat vs 3-Republican`,
#> #   ²​`2-Independent, other vs 3-Republican`
#> # Model: multinomial logistic regression; odds ratios (each category vs the reference).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
# ordinal (proportional-odds): one cumulative-OR column
  tab_reg(data, dependent = "rincome", predictors = c("race", "age"), family = "ordinal")
#> Warning: ! The proportional-odds (parallel-lines) assumption is rejected (Brant omnibus
#>   p = 0.0036).
#> ℹ Cumulative odds ratios may mislead; consider `family = "multinomial"` or a
#>   partial proportional-odds model.
#> ℹ The Brant test over-rejects at large N; inspect the per-variable tests too.
#> | predictors | Model fit     | rincome |
#> |:-----------|:--------------|--------:|
#> | race, age  | N             |  12 990 |
#> |            | LR vs null    |  <0.01% |
#> |            | McFadden R2   |   0.015 |
#> |            | AIC           |  29 299 |
#> |            | BIC           |  29 344 |
#> |            | Brant PO test |  0.359% |
#> 
#> # A tabxplor tab: 5 × 3
#> # Groups:         var [3]
#>   var      levels                Model_OR
#>   <fct>    <fct>                <row%-or>
#> 1 Constant Reference population          
#> 
#> 2 race     White                     1   
#> 3 race     Black                1/1.47***
#> 4 race     Other                1/1.34***
#> 
#> 5 age      age                    1.02***
#> # Model: ordinal logistic regression; cumulative odds ratios (proportional-odds model).
#> # OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
# }
```
