# Regressions

Regressions

## Usage

``` r
jmvtabreg(
  data,
  outcome = NULL,
  predictors = NULL,
  tab_vars = NULL,
  wt = NULL,
  family = NULL,
  link = NULL,
  outcome_level = NULL,
  trials = NULL,
  effect = "auto",
  measure = "auto",
  empirical = TRUE,
  models = NULL,
  na = "drop_by_outcome",
  run_compare = FALSE,
  levels_order = NULL,
  levels_collapse = NULL,
  crosses = NULL,
  ref_levels = NULL,
  shape = NULL,
  multiplier = NULL,
  conf_level = 0.95,
  ci_method = "wald",
  stars = TRUE,
  color = "measure",
  color_signif = "grey_non_signif",
  display = "auto",
  n = "range",
  digits = "0",
  cleannames = TRUE,
  subtext = "",
  tab_theme = "light",
  wrap_rows = 35,
  wrap_cols = 15,
  export_format = "excel",
  exportExcel = FALSE,
  export_dir = "~/Documents",
  export_filename = "Reg_model",
  resetPath = FALSE,
  xl_check = FALSE,
  xl_replace = FALSE
)
```

## Arguments

- data:

  A data.frame.

- outcome:

  The outcome variable(s). One model is built per outcome. Set each
  outcome's family (and, for a binomial outcome, its modelled level or
  number of trials) in the Model table.

- predictors:

  The explanatory variables of the model. Factors are shown one line per
  level (the reference level as the neutral value); numeric predictors
  as a single line.

- tab_vars:

  A grouping variable. The same model is fitted within each of its
  levels and the tables are stacked (like tab_vars for crosstables).

- wt:

  A survey weight variable. Switches to design-based estimation
  (scale-invariant sandwich standard errors). Leave empty for unweighted
  results.

- family:

  .

- link:

  WHICH MEASURE THE MODEL ESTIMATES – the only argument that changes the
  model. A link IS a measure, so it takes the same words as `measure`.
  Chosen per outcome, in the Model table beside `family`: `"auto"` is
  the family's own (a logistic regression for a binary outcome, a linear
  one for a quantity, a Poisson one for a count), and `"odds_ratio"` /
  `"ratio"` / `"difference"` name the model whose coefficient IS that
  measure – on a binary outcome, the logistic fit, the modified Poisson
  (Zou 2004) and the identity-link additive-risk one. The picker only
  ever offers the links the chosen family can be fitted on.

- outcome_level:

  .

- trials:

  .

- effect:

  WHERE THE NUMBER COMES FROM, once the model and the reported measure
  are fixed.

  - `"auto"`: the coefficients when the reported measure IS the model's
    own, the model's predictions otherwise. Nobody needs to change this.

  - `"conditional"`: read off the model's own coefficients.

  - `"marginal"`: worked out from the model's predictions for every
    observed person, then averaged.

  - `"at_reference"`: the same, at one profile (every other predictor at
    its reference level / mean).

- measure:

  WHICH MEASURE IS REPORTED. It never changes the model: where it is not
  the measure the model estimates (see `link`), it is worked out from
  the model's predictions.

  - `"auto"`: the model's own measure – follow from the left. On a
    prediction route it steps back to the outcome's own (a percentage
    reads as "x times as likely"), because a marginal odds ratio is a
    specialist quantity, asked for by name.

  - `"odds_ratio"` / `"ratio"` / `"difference"`: the named measure, when
    the outcome's level can carry it. One it cannot says so, and lists
    what it does offer.

  - `"raw_coefficient"`: the model's own coefficient, un-transformed —
    the log of the reported measure wherever that measure is
    multiplicative, and the additive estimate itself on a model that is
    already additive.

- empirical:

  Show the crude, unadjusted, single-predictor effect beside each model
  effect — the bivariate association that IS the modelised quantity when
  there is a single predictor, so the gap between the two is what
  adjustment changed.

- models:

  .

- na:

  "drop_by_outcome" (default) fits every model OF ONE OUTCOME on the
  same complete cases, which is what makes the observed columns
  comparable to the model beside them and lets the likelihood-ratio
  comparison run; "drop_by_model" gives each model its own complete
  cases (a model on a different population then gets no observed
  effect); "drop_all" shares one population across every outcome as
  well.

- run_compare:

  .

- levels_order:

  .

- levels_collapse:

  .

- crosses:

  .

- ref_levels:

  .

- shape:

  .

- multiplier:

  .

- conf_level:

  The confidence level for intervals and the significance stars.

- ci_method:

  Wald intervals (also the only option for weighted models) or
  profile-likelihood intervals (unweighted binomial / poisson only;
  needs MASS). A profile interval is an output of the likelihood at one
  confidence level, so it cannot be cached: every change refits the
  models.

- stars:

  Show per-cell significance stars (the colours read the confidence
  interval either way).

- color:

  WHAT the effect cells are coloured by. The colour LADDER always comes
  from what the column estimates (an odds ratio is read on the
  odds-ratio scale, a beta on the standardized-difference one), so what
  is left to choose is what the estimate is compared TO.

  - `"measure"`: the effect's own size (compared to no effect).

  - `"no"`: no colours.

  - `"adjustment"`: how far the ADJUSTED effect moved from the crude one
    – needs `empirical`.

  - `"between_groups"`: how far each group's effect is from the first
    group's – needs `tab_vars`.

- color_signif:

  How significance interacts with the colours: observed size + grey out
  non-significant cells, colour only the guaranteed (error-adjusted)
  effect, ignore significance.

- display:

  The estimate-cell LAYOUT (never the estimand: a display may fold in
  another quantity of the SAME fit, it can never change the fit). The
  same named layouts
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  offers, written in the same [`{}`](https://rdrr.io/r/base/Paren.html)
  grammar: `est` is whatever the column estimates and `base` the level
  it sits on (an adjusted probability, an adjusted mean). `"auto"` keeps
  the built-in layout.

- n:

  The column giving the number of observations behind each predictor
  level: `"range"` prints `min-max` when several models were fitted on
  different people, `"min"` the smallest count only, `"no"` no column at
  all.

- digits:

  The minimum number of digits to print, as a single integer (0-6): each
  measure keeps its own precision where that is finer (an odds ratio
  reads at two decimals, a mean score at one). In R,
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  also names one display field at a time, `digits = c(ratio = 3)`.

- cleannames:

  Strip numeric prefixes from factor level labels.

- subtext:

  A free note printed below the table.

- tab_theme:

  How the table is painted, in the results panel and in every export.
  `"light"` is the colour palette; `"print_ready"` says the same thing
  typographically — bold, italics, underlines and marks instead of blue
  and red — for a page that has no colour. See
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md).

- wrap_rows:

  .

- wrap_cols:

  .

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

- xl_check:

  Excel export only: draw the model-check plots
  ([`reg_check_plots()`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md))
  under each table in the workbook — the panels that apply to the fitted
  family (`tab_xl(check = "auto")`). Needs `ggplot2` and `gridExtra`;
  without them the export says so and writes the table alone.

- xl_replace:

  "Set to `TRUE` to overwrite an existing file."

## Value

A results object containing:

|                         |     |     |     |     |          |
|-------------------------|-----|-----|-----|-----|----------|
| `results$html_table`    |     |     |     |     | a html   |
| `results$cache_state`   |     |     |     |     | an image |
| `results$compare_state` |     |     |     |     | an image |
