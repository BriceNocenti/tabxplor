# Regressions

Regressions

## Usage

``` r
jmvtabreg(
  data,
  dependent = NULL,
  predictors = NULL,
  split_var = NULL,
  wt = NULL,
  depFamily = NULL,
  depModelLevel = NULL,
  depTrials = NULL,
  exponentiate = TRUE,
  effect = "coefficient",
  at = "average",
  empirical = FALSE,
  models = NULL,
  baseline = 1,
  compare = "none",
  na = "drop_by_model",
  run_compare = FALSE,
  refLevels = NULL,
  multiplicator = NULL,
  conf_level = 0.95,
  method = "wald",
  stars = TRUE,
  color = TRUE,
  color_signif = "grey_non_signif",
  estimate_display = "value",
  cleannames = TRUE,
  subtext = "",
  wrap_rows = 35,
  wrap_cols = 15,
  ids = NULL,
  strata = NULL,
  fpc = NULL,
  nest = FALSE,
  export_format = "excel",
  exportExcel = FALSE,
  export_dir = "~/Documents",
  export_filename = "Reg_model",
  resetPath = FALSE,
  xl_replace = FALSE
)
```

## Arguments

- data:

  A data.frame.

- dependent:

  The outcome variable(s). One model is built per dependent. Set each
  outcome's family (and, for a binomial outcome, its modelled level or
  number of trials) in the Model table.

- predictors:

  The explanatory variables of the model. Factors are shown one line per
  level (the reference level as the neutral value); numeric predictors
  as a single line.

- split_var:

  A grouping variable. The same model is fitted within each of its
  levels and the tables are stacked (like tab_vars for crosstables).

- wt:

  A survey weight variable. Switches to design-based estimation
  (scale-invariant sandwich standard errors). Leave empty for unweighted
  results.

- depFamily:

  .

- depModelLevel:

  .

- depTrials:

  .

- exponentiate:

  Exponentiate the coefficients into ratios (odds ratios /
  incidence-rate ratios), automatically leaving gaussian linear
  coefficients on their raw scale. Uncheck to keep every coefficient on
  the coefficient (log / linear) scale.

- effect:

  "coefficient" is the native per-family effect (beta / OR / IRR). "AME"
  is the average marginal effect on the response scale (needs the
  marginaleffects package).

- at:

  Where marginal effects are evaluated: averaged over the sample, or at
  the reference profile (other predictors at their reference level /
  mean).

- empirical:

  Add the crude, unadjusted, single-predictor companion columns beside
  the model effect (the bivariate association that IS the modelised
  quantity with a single predictor). Binomial / gaussian / poisson only.

- models:

  .

- baseline:

  .

- compare:

  With several models (a predictor-subset list), add a likelihood-ratio
  / F / Wald comparison-test footer row: "baseline" tests each model
  against the chosen baseline model, "sequential" against the previous
  one (an AIC difference when not nested).

- na:

  "drop by model" fits each model / outcome on its own complete cases;
  "drop all models" uses one shared complete-case population across all
  predictors (equal N; changes the estimates), which a valid
  likelihood-ratio comparison test needs.

- run_compare:

  .

- refLevels:

  .

- multiplicator:

  .

- conf_level:

  The confidence level for intervals and the significance stars.

- method:

  Wald intervals (also the only option for weighted models) or
  profile-likelihood intervals (unweighted binomial / poisson only;
  needs MASS).

- stars:

  Show per-cell significance stars (the colours read the confidence
  interval either way).

- color:

  Colour the effect cells with the sensible per-family colour helper (OR
  magnitude for ratios, standardized difference for gaussian betas).
  Uncheck for no colours.

- color_signif:

  How significance interacts with the colours: observed size + grey out
  non-significant cells, colour only the guaranteed (error-adjusted)
  effect, ignore significance.

- estimate_display:

  The estimate-cell layout. "ci" shows a visible interval; "prob" /
  "ame" fold the adjusted predicted probability / marginal effect into
  the OR cell (binomial coefficient models only).

- cleannames:

  Strip numeric prefixes from factor level labels.

- subtext:

  A free note printed below the table.

- wrap_rows:

  .

- wrap_cols:

  .

- ids:

  Cluster identifiers (survey design), largest to smallest stage. Only
  used with a weight; leave empty for no clustering.

- strata:

  Stratification variables (survey design). Only used with a weight.

- fpc:

  Finite-population correction (survey design). Only used with a weight.

- nest:

  Set when cluster ids are reused across strata (survey design).

- export_format:

  .

- exportExcel:

  Press to export the table to the chosen format (the button label
  follows the format).

- export_dir:

  The folder to save the exported file in. Blank or `~/Documents`
  auto-detects your real Documents folder (a redirected `D:/Documents`
  or network Documents included). Type any other folder to override; a
  leading `~` there expands to your home folder.

- export_filename:

  The bare file name, with NO extension (the chosen format adds it).

- resetPath:

  Reset the folder and file name to their defaults (your Documents
  folder and "Regression").

- xl_replace:

  "Set to `TRUE` to overwrite an existing file."

## Value

A results object containing:

|                         |     |     |     |     |          |
|-------------------------|-----|-----|-----|-----|----------|
| `results$html_table`    |     |     |     |     | a html   |
| `results$cache_state`   |     |     |     |     | an image |
| `results$compare_state` |     |     |     |     | an image |
