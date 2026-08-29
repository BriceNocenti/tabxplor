# All-in-one tables for regressions, with each modelled effect beside its observed one

Fits one regression model per column and returns a `tabxplor` table of
the per-family effect measure — a linear **mean difference** (gaussian),
**odds ratios** (binomial), **incidence-rate ratios** (poisson), one
**odds-ratio column per outcome category** (nominal 3+ level), a
**cumulative odds ratio** (ordinal) — one row per predictor level,
grouped by predictor, with the **observed (crude)** effect beside each
adjusted one. Each cell stores its estimate, interval and p-value, so
the table prints with stars, greys what is not significant, and exports
like any `tabxplor` crosstab.

## Usage

``` r
tab_reg(
  data,
  outcome,
  predictors = NULL,
  tab_vars = NULL,
  wt = NULL,
  family = "auto",
  link = "auto",
  measure = "auto",
  effect = "auto",
  outcome_level = NULL,
  trials = NULL,
  empirical = TRUE,
  n = NULL,
  color = "measure",
  color_signif = NULL,
  stars = TRUE,
  ref = NULL,
  multiplier = "2sd",
  shape = NULL,
  stats = "auto",
  conf_level = NULL,
  na = c("drop_by_outcome", "drop_by_model", "drop_all", "keep_for_predictors"),
  display = NULL,
  digits = 0,
  cleannames = NULL,
  subtext = "",
  caption = NULL,
  ...
)
```

## Arguments

- data:

  A data frame, **or a prebuilt survey design**
  ([`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)).
  A design's weights, clustering, stratification and calibration drive
  the estimation, and `wt` is ignored.

- outcome:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The outcome variable(s) — bare names, quoted names, or any selection
  helper, exactly as in
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) —
  **or a model formula** (the escape hatch; leave `predictors` unset).
  Several names give one effect column per outcome; with a `predictors`
  list, a single name is required.
  [`reg_formulas()`](https://bricenocenti.github.io/tabxplor/reference/reg_formulas.md)
  shows what was fitted.

- predictors:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The predictors of one model — or a **named list**, one model per
  element, its name labelling the column, each element selected on its
  own (`list(m1 = c(race, age), m2 = starts_with("inc"))`), which is how
  models are compared. Leave `NULL` when `outcome` is a formula. A bare
  name is a column of `data` first, then an object, so a variable
  holding names works without
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html).

  **`a*b` is an interaction**, R's own spelling, bare or quoted — *"a's
  effect, allowed to vary with b"*. Two categorical variables give one
  row per **cell** of the pair; a continuous `a` gives its **slope
  within each level** of `b`. An interaction supplies both its
  variables, so do not list them beside it, which is what makes "with
  and without" an ordinary model comparison. `a:b`, which drops the main
  effects, is refused. See [the regression
  vignette](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html).

- tab_vars:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Optional. One grouping variable — the same argument as
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s:
  one sub-table per group, the same model(s) fitted **within each
  level**. Two readings of "does this effect hold in every subgroup?"
  come with it: `color = "between_groups"` colours and tests each effect
  against the first group's, row by row, and
  `stats = c(..., "group_interaction")` adds the aggregated test, once
  per predictor. For an interaction between two PREDICTORS of one model,
  write it in `predictors` as `a*b` instead.

- wt:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Optional. One weight column, switching to design-based survey
  estimation
  ([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)).
  For clustering, stratification, a finite-population correction or
  calibration, build the design with
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  and pass it as `data`. See
  [`vignette("tabxplor-weights")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.md).

- family:

  The model family, **resolved per outcome** so several outcomes with
  different families can share one table. `"auto"` (default) detects
  each one and says so: a binary outcome gives `"binomial"`, an ordered
  3+ level `"ordinal"`, a nominal 3+ level `"multinomial"`, any other
  numeric `"gaussian"` — a genuine count is yours to name. Or set it:
  `"gaussian"` (linear), `"binomial"` (logistic), `"poisson"` /
  `"quasipoisson"` (counts), `"multinomial"`, `"ordinal"`. A **scalar**
  applies to every outcome; a **vector** aligned to `outcome`, or a
  **named** vector keyed by outcome (e.g.
  `c(income = "poisson", satisfied = "binomial")`), sets one family per
  outcome. Mixed families need a character `predictors`.

  It answers **one** question — what kind of number the outcome is — and
  never picks a link behind your back: on a binary outcome
  `family = "poisson"` is refused, naming the two things it could have
  meant, `link = "ratio"` and `measure = "ratio"`.

- link:

  **Which measure the model estimates** — the only argument that changes
  the model. A link *is* a measure (the one a model gives you directly),
  so it takes `measure`'s own words:

  - `"auto"` (default) — the family's own: logistic for a binary
    outcome, linear for a quantity, Poisson for a count.

  - `"odds_ratio"` — the logit fit (binomial, multinomial, ordinal).

  - `"ratio"` — the log link: the **modified Poisson** on a binary
    outcome (a conditional risk ratio), Poisson on a count, Poisson
    pseudo-likelihood on a continuous one.

  - `"difference"` — the identity link; on a binary outcome the **risk
    difference**. It can fail to converge, and the linear probability
    model then takes over, with a message.

  Reach for it when you want the model's *coefficient* to be that
  measure; to report a measure without changing the model, set `measure`
  instead. `"log"` is the one word the two arguments do not share: here
  the **log link**, on `measure` a spelling of `"raw_coefficient"`.

- measure:

  **Which measure of deviation is reported** — a deviation being how far
  a group sits from the reference, the measure which of the ways of
  expressing it you read. The one argument most readers ever set, and
  the one that never changes the model. `"auto"` (default) is the
  model's own. The full word is canonical, the discipline's acronym a
  synonym:

  - `"odds_ratio"` (`"OR"`) — the odds of the outcome, times what.

  - `"ratio"` (`"RR"`, `"IRR"`, `"RoM"`) — how many times as likely, as
    frequent, as large. Reach for it when the outcome is **common**,
    where an odds ratio is far from the risk ratio people hear in it,
    and because a risk ratio stays comparable across nested models.

  - `"difference"` (`"RD"`, `"diff"`) — how much more, in the outcome's
    own units.

  - `"raw_coefficient"` (`"coef"`, `"log"`, ...) — the model's own
    coefficient, un-transformed.

  On an **ordered** outcome the first three read the whole predicted
  distribution rather than one category, so they stay in one column:
  Somers' `D` and the win ratio. Where the measure IS the model's own it
  is read off the coefficients, otherwise from its predictions — so it
  is available whichever model you fit. `"auto"` never lands on a
  **predicted odds ratio**, a specialist quantity to be asked for by
  name. Call
  [`reg_measures()`](https://bricenocenti.github.io/tabxplor/reference/reg_measures.md)
  to see what an outcome offers.

- effect:

  **Where the reported number comes from**, once the model and the
  measure are fixed. `"auto"` (default) needs no thought: the model's
  own coefficients when the reported measure is the model's, its
  predictions otherwise. The other values name a reading:

  - `"conditional"` — read off the coefficients ("holding the other
    predictors constant"). Only where `measure` is the model's own;
    otherwise the abort names the two cures.

  - `"marginal"` — the **average marginal effect**: the model's
    prediction for every observed person, averaged. Comparable across
    models (Mood 2010), and always available.

  - `"at_reference"` — the same at **one profile**, every other
    predictor at its reference.

  The contrast is a **marker on the measure** in the header, so the
  acronym stays the one thing to look up: `Model_OR`, `Model_mRR`,
  `Model_refRD`. The observed companion carries the measure alone
  (`Obs_RR`), a univariable effect having no adjustment to be marginal
  over.

- outcome_level:

  Which level of the **outcome** to single out, as a named vector keyed
  by outcome name — `outcome_level = c(married = "Married")`. It is the
  twin of `ref`: **`ref` names the level you compare AGAINST,
  `outcome_level` the one you MODEL.**

  - **binomial**: the level whose probability is estimated; it becomes
    the column header. Defaults to the outcome's **first** level. A 0/1
    numeric outcome may be named either way.

  - **multinomial**: the baseline category the other categories' columns
    are compared to.

  - **ordinal and numeric outcomes**: refused, with the reason.

- trials:

  Grouped-binomial (summed-score) outcomes only. The number of items
  behind the score, fitting `cbind(score, trials - score)` as a
  binomial. `NULL` (default) fits an ordinary binary logit; an integer
  (or a vector named by outcome) sets the item count; `TRUE` uses each
  outcome's observed maximum. Requires `family = "binomial"`.

- empirical:

  Show the **observed, unadjusted (crude)** effect beside each modelled
  one — the same quantity fitted with a single predictor, on exactly the
  same people. The distance between the two is what adjustment changed,
  read left to right; it is the feature the package exists for. `TRUE`
  (**the default**) or `FALSE`; three spellings say *where* it goes, and
  in every one but `"no"` it is stored in the `obs` field and read by
  `$obs`, `color = "adjustment"`,
  [`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
  and the hover tooltip:

  - `TRUE` — a crude **column** beside the model one, except where that
    would double a table already wide (`tab_vars` groups, a 3+ level
    outcome), which take `"tooltip"`.

  - `"column"` — always the column, per outcome category if that is what
    it takes.

  - `"tooltip"` — computed, printed nowhere. The narrowest table.

  - `"cell"` — **inside** the model cell, `(1/1.69) 1/1.63***`.
    `display` overrides it.

  The two columns are the same column twice: same estimand, same colour
  ladder, one legend block. Each cell prints the effect with the level
  it sits on — the observed percentage or mean on the crude side, the
  **adjusted** prediction on the model side. A **continuous** predictor
  has no levels, so its crude cell is the univariable slope, which
  assumes linearity: check that with `shape` first.

- n:

  How many people the table is about. `NULL` (default) reads
  `options(tabxplor.n)` — `"range"`, which adds an `n` column holding
  the **unadjusted count** behind each predictor level, on the model's
  own complete cases. Where several models rest on different people it
  prints the whole range (`5 139-9 862`), so an unequal base cannot pass
  unnoticed; `"min"` shows the smallest count only, `"no"` none.
  Continuous predictors are left blank: their count is the model N, the
  first footer row.

- color, color_signif:

  Colouring of the effect cells. `color = "measure"` (default, `TRUE`
  equivalently) grades each cell on **its own measure**, so the ladder
  follows what the column estimates; `color = FALSE` turns colouring
  off. `color_signif` is the significance policy — `NULL` (default) is
  `"grey_non_signif"` here, where
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  defaults to `"ignore"`.

  What is left to choose is what each effect is compared **to**. `color`
  is positional, `c(text, background)`, so
  `color = c("measure", "adjustment")` answers "how strong is this
  effect?" and "how much did the model change it?" in one glance:

  - `"adjustment"` — how far each **modelled** effect sits from its
    **observed** (crude) counterpart: what adjusting for the other
    predictors did to it. It turns `empirical = TRUE` on. Set its
    thresholds with
    [`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
    (`adj_ratio`, `adj_diff`, `adj_diff_std`).

  - `"between_groups"` — with `tab_vars`, how far each group's effect
    sits from the **first** group's: a per-predictor reading of effect
    modification.

  The two are mutually exclusive, and each always tests its own gap: a
  gap whose interval covers zero is greyed whatever `color_signif` says
  — so a cell can be filled while neither estimate carries a star, which
  is correct rather than odd. A conditional **odds ratio** is not
  collapsible, so there the colours stay descriptive and are not tested.
  Read a coloured cell as "adjustment moved this effect", not as "this
  variable is a confounder". See [the regression
  vignette](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html).

- stars:

  Logical (default `TRUE` for regression tables, where significance
  stars are standard). When `FALSE`, the per-cell p-value is dropped and
  no stars are shown (colours still read the interval).

- ref:

  The reference every effect is measured **from** — one argument, one
  meaning per kind of predictor. For a **factor** it is the level the
  others are compared against (a level name, or `"first"` (default) /
  `"last"`); for a **continuous** predictor the value it is **anchored**
  at (a number, or `"mean"` (default), `"median"`, `"min"`, `"max"`).

  Anchoring a continuous predictor **does not change its own effect** —
  a slope is the same wherever you start reading it from — but it does
  move the **Constant** row; its own row says where the anchor sits,
  `per SD/13.5 (at mean/42.4)`. The default is the mean because zero is
  usually outside the data: nobody is 0 years old.

  `ref`, `multiplier` and `shape` share one grammar: a value **on its
  own** is the default for every predictor it can apply to, a **named**
  one overrides that variable —
  `ref = c("median", "last", race = "Black")`.

  For the level of the **outcome**, see `outcome_level`: `ref` names the
  level you compare AGAINST, `outcome_level` the one you MODEL.

- multiplier:

  How a **continuous** predictor's effect is scaled — the unit its row
  reports. One unit is rarely a readable amount (a one-year change in
  `age` barely moves the odds), so the default is **two standard
  deviations**: roughly the span a binary predictor's own contrast
  covers, which is what makes a continuous row and a factor row
  comparable at a glance (Gelman 2008). Values: `"2sd"` (default),
  `"sd"`, or a number of units (`10` = per decade). Same grammar as
  `ref`: `multiplier = c("sd", age = 10)`.

  The estimate, its interval and the observed companion all scale
  together; the p-value does not move. **Because the default is not 1, a
  continuous predictor's `Model_*` cell does not equal
  `exp(coef(glm(...)))` unless you pass `multiplier = 1`.**

- shape:

  How a **continuous** predictor enters the model, when one straight
  line is not enough. The `Linearity` footer row and the little curve
  drawn in the predictor's `n` cell tell you *whether* a line is enough;
  this argument is how you fix it without leaving the framework. Same
  grammar as `multiplier` and `ref` — `shape = "quintiles"` cuts every
  continuous predictor, `shape = c(age = "quadratic")` only that one,
  and anything unnamed stays linear:

  `"linear"`

  :   one slope (the default).

  `"quintiles"` / `"quartiles"` / an integer `k`

  :   cut into `k` quantile groups, so the predictor becomes an ordinary
      **factor**: one estimate per group, its own observed companion,
      counts and colours per group — the non-linearity becomes visible
      in the printed numbers. Start here; it is the most readable
      answer.

  `"sd_bands"`

  :   cut at the **mean and one standard deviation either side** — the
      classic low / average / high reading, whose cut points mean the
      same thing across sub-samples, where quantiles move with each one.
      The bands are not balanced: prefer quantiles when the group sizes
      matter.

  `"quadratic"`

  :   adds a curvature term, so the predictor takes **two rows** — the
      slope at the mean, and whether it flattens or accelerates away
      from it.

  `"log"` / `"sqrt"`

  :   fit `log(x)` / `sqrt(x)` instead of `x` — diminishing returns.

  Example: `shape = c(age = "quadratic", income = "log")`. The observed
  companion is fitted with the same shape, so the comparison stays like
  with like. It is the vocabulary
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  takes
  ([`shape_numeric_var()`](https://bricenocenti.github.io/tabxplor/reference/shape_numeric_var.md))
  plus `"quadratic"`, which is a model term.

- stats:

  The statistics shown in the model-summary **footer** (one block per
  model). `"auto"` (default) uses the per-family set — R square,
  adjusted R square, the overall F-test and the residual SD for a linear
  model, the likelihood-ratio test against the null model, McFadden's
  pseudo-R square, AIC and BIC otherwise — plus the default **model
  checks** (see below). A weighted model shows the survey-appropriate
  set. Pass a character vector to pick them: `"n"`, `"lr_null"`,
  `"mcfadden_r2"`, `"aic"`, `"bic"`, `"phi"`, `"r2"`, `"r2_adj"`,
  `"f_model"`, `"sigma"`, `"global"`, `"interaction"`,
  `"group_interaction"`, `"linearity"`, `"proportionality"`,
  `"dispersion"`, `"influence"`, `"collinearity"`; `"all"` for
  everything, or `NULL` / `FALSE` / `"no"` to hide the footer.

  **Model comparison happens by default** wherever it means anything —
  when `predictors` is a list of several models. Where each nests in the
  next, every model is tested against the previous one; otherwise each
  against the first. `"compare_sequential"` and `"compare_baseline"`
  (optionally naming the model) override that, and naming any footer
  statistic drops it.

  Three tests are asked for by name: `"global"`, one **overall test per
  predictor** — "is this variable associated with the outcome at all?",
  which a block of stars against a reference category cannot answer;
  `"interaction"`, whether each **crossed pair** in `predictors` is real
  or the additive model is enough; and `"group_interaction"`, one
  aggregated **effect-modification** test per predictor across
  `tab_vars` groups.

- conf_level:

  Confidence level for the intervals. `NULL` (default) reads
  `options(tabxplor.conf_level)` — 0.95. It drives every interval, the
  significance stars, the greying under `color_signif` and the
  model-versus-observed gap interval, and each column records the level
  it was built at.

- na:

  Which rows each model is fitted on — the grain at which missing values
  are dropped. `"drop_by_outcome"` (default) gives every model **of one
  outcome** the same complete-case population, which is what makes the
  comparisons honest: the observed columns are computed on exactly the
  model's rows, and nested models get equal N. `"drop_by_model"` lets
  each model use its own complete cases — more rows, at the price of
  comparability. `"drop_all"` shares one population across the whole
  call.

  `"keep_for_predictors"` drops nothing but a missing **outcome**: every
  predictor keeps its missing values as an ordinary `NA` level, with its
  own row, count and effect — often the fastest way to find out whether
  non-response is itself patterned. A number has no level to put them
  in, so a numeric predictor that has any is cut into bands.

- display:

  What each effect cell shows —
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  display grammar, same names, same meaning, on every family and on the
  crude column as well as the model one. `NULL` (default) shows the
  plain estimate, or, with `empirical`, the estimate with the level it
  sits on beside it. The whole vocabulary — the named layouts, the
  [`{}`](https://rdrr.io/r/base/Paren.html) templates and the per-token
  precision `"{est:3} ({base:1})"` — is in
  [tabxplor-display](https://bricenocenti.github.io/tabxplor/reference/tabxplor-display.md);
  the ones this table uses most are `"est_ci"`
  (`1/2.22*** [1/2.47;1/1.99]`), `"est_base"` (`1/2.22 (32.8%)`),
  `"est_coef"` and `"base_est_mdiff"` / `"base_est_mratio"`, which read
  the same comparison the other way. The **Constant** row holds the
  quantity the column's effects operate on, so it is read in one step: a
  baseline *odds* on an odds-ratio column, the level itself on an
  additive one.

  `display` is **post-hoc**: every quantity it can name is already
  stored, so choosing a layout never triggers a computation and never
  changes a number —
  [`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  on a built table gives the same result. It never changes the estimand,
  which is `measure`'s job alone.

- digits:

  The number of decimals. A single integer sets every cell (`0`, the
  default, means "each measure's own"), and a measure finer than the
  level it sits on keeps its own precision. Name a display field to set
  just that one, an aside included — `digits = c(ratio = 3)`,
  `digits = c(1, or = 3)`; a template may carry its own,
  `display = "{est:3} ({base:1})"`. Left alone, a column reading in the
  outcome's own units (a mean, a mean difference) follows that outcome's
  magnitude, so a six-figure salary prints no decimals and a rate prints
  two; `digits =` always raises it back.

- cleannames:

  Logical. If `TRUE`, strips numeric prefixes from factor levels for
  display. Uses `getOption("tabxplor.cleannames")` when `NULL`.

- subtext:

  Optional character. A note shown below the table.

- caption:

  A title for the table, stored on it and carried into every export.
  Without one a regression table titles itself from the model it shows.

- ...:

  One rarely-typed argument, plus internal plumbing.

  `ci_method` — how the interval and p-value are computed: the same
  argument, and the same named-vector grammar, as in
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  whose fifth slot is this producer's. `"wald"` (default) matches
  standard software output and is the only option for weighted models;
  `"profile"` uses the profile-likelihood interval and the
  likelihood-ratio test — more accurate near separation, unweighted
  binomial / poisson only.

  Every argument removed or renamed while `tab_reg()` was in development
  is still accepted here, and gives an error naming its replacement
  rather than R's bare "unused argument". The dot-prefixed names are the
  jamovi live-cache plumbing, not user arguments.

## Value

A `tabxplor_grouped_tab` (grouped by predictor), one effect column per
model / outcome.

## Details

To **learn** what such a table says, read [Reading a
regression](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression.html)
([`vignette("tabxplor-reading-a-regression")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression.md));
to **look something up**, the [regression
vignette](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html)
([`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)).

New to regressions with tabxplor? A first model needs three arguments:
`data`, `outcome` and `predictors`. The model follows the outcome's type
— a two-level factor gives logistic **odds ratios**, a numeric a linear
**mean difference**, a count Poisson **rate ratios**, a 3+ level factor
multinomial or ordinal odds ratios — so you rarely set `family` by hand.

**The estimand is a cascade**: `family` -\> `link` -\> `measure` -\>
`effect`, where `"auto"` means *follow from the left*. `family` says
what kind of number the outcome is; `link` **which measure the model
estimates** (a link *is* a measure, so it takes the same words);
`measure` **which one is reported**; `effect` where that number comes
from. Set any one and everything to its right re-derives, so most tables
set none of them — and the one most people ever set is `measure`.

A **coefficient** exists only where the reported measure IS the model's
own. Ask for another and it is worked out from the model's predictions
instead, averaged over the sample. So `measure = "ratio"` on a binary
outcome gives a *marginal* risk ratio from the logistic fit, while
`link = "ratio"` fits the modified Poisson and gives its *conditional*
one — two different quantities, and now two different arguments.

## Model checks

Beside the fit statistics the footer carries five **model checks**, each
naming an assumption and the instrument that measured it: **Linearity**
and **Proportionality** (p-values) say whether the estimate means what
it claims, **Dispersion** whether the intervals are wide enough,
**Influence** whether one respondent carries the result,
**Collinearity** why the intervals are wide. Four are shown by default;
`stats = "linearity"` adds the fifth, and `shape` is the cure for what
it flags.
[`reg_check_plots()`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
draws them all. What each one asks, and how to read it: [the regression
vignette](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html).

## Out of scope

`tab_reg()` covers linear, logistic, Poisson, multinomial and ordinal
models, with survey designs. Three families of models are deliberately
**not** supported, and are unlikely to be: **survival / Cox** models,
**mixed / multilevel** models, and pooling over **multiply-imputed**
datasets. Fit those with their own packages.

## The header acronyms

A column header names the **measure**; the **contrast** is a marker on
it — no marker for a conditional effect, `m` for a marginal one, `ref`
at the reference profile — and `measure = "log"` wraps it (`Model_mRR`,
`Model_refRD`, `Model_log(OR)`). The observed companion carries the
measure alone (`Obs_RR`).

- `OR` — odds ratio (binomial, multinomial)

- `cumOR` — cumulative odds ratio (ordinal)

- `RR` — risk ratio (binomial, multinomial)

- `RD` — risk difference (binomial, multinomial)

- `IRR` — incidence-rate ratio (poisson)

- `RoM` — ratio of means (gaussian)

- `diff` — mean difference (gaussian, poisson)

- `D` — Somers' D (ordinal)

- `WR` — win ratio (ordinal)

## References

Clogg, C. C., Petkova, E. & Haritou, A. (1995). Statistical Methods for
Comparing Regression Coefficients between Models. *American Journal of
Sociology*, 100(5), 1261-1293 — the comparison `color = "adjustment"`
implements.

Zou, G. (2004). A Modified Poisson Regression Approach to Prospective
Studies with Binary Data. *American Journal of Epidemiology*, 159(7),
702-706 — `link = "ratio"`.

Altman, D. G. & Bland, J. M. (2003). Interaction revisited: the
difference between two estimates. *BMJ*, 326, 219 — the
`color = "between_groups"` test.

## See also

[`reg_formulas()`](https://bricenocenti.github.io/tabxplor/reference/reg_formulas.md)
shows the formula each column was fitted with, and
[`reg_measures()`](https://bricenocenti.github.io/tabxplor/reference/reg_measures.md)
what an outcome can be modelled as.
[`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
draws the finished table — every effect with its interval, its stars and
its colour, and (with `empirical = TRUE`) the observed effect beside it
with the margin of error of the gap.
[`reg_check_plots()`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
draws the model checks.
[tabxplor-display](https://bricenocenti.github.io/tabxplor/reference/tabxplor-display.md)
says what a cell can show,
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
builds cross-tables.

## Examples

``` r
# The shape table a continuous predictor draws under the footer is noise in an example:
.opt <- options(tabxplor.shape_table = "no")

# Logistic: the odds of being released, adjusted, beside the observed (crude) odds ratio.
tab_reg(car_arrests, "released", c("colour", "checks"))
#> ℹ "released": binary outcome detected -> `family = "binomial"` (logit).
#> This message is displayed once per session.
#> | predictors     | Model fit                    | released |
#> |:---------------|:-----------------------------|---------:|
#> | colour, checks | N                            |    5 226 |
#> |                | Dispersion (robust/model SE) |     1.02 |
#> |                | Collinearity (max VIF)       |     1.01 |
#> |                | Influence (max dfbetas)      |     0.06 |
#> |                | LR vs null                   |   <0.01% |
#> |                | McFadden R2                  |    0.076 |
#> |                | AIC                          |    4 420 |
#> |                | BIC                          |    4 440 |
#> 
#> # A tabxplor tab: 4 × 5
#> # Outcome:        released
#> # Groups:         var [3]
#>   var      levels                             n          Obs_OR        Model_OR
#>                                             <n>     <(obs%) OR>     <OR (adj%)>
#> 1 Constant Reference profile                                      6.51*** (87%)
#> 
#> 2 colour   White                          3 938 (86%)      1         1    (85%)
#> 3 colour   Black                          1 288 (74%) 1/2.11*** 1/1.72*** (77%)
#> 
#> 4 checks   per 3.08 (2SD), at 1.64 (mean)             1/3.71*** 1/3.47***      
#> # Model: logistic regression; OR: odds ratio (vs the reference category); obs%: observed proportion; adj%: adjusted/predicted proportion.
#> # Obs_OR, Model_OR: OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level (from 1 for the Constant); **: at the 95% level; *: at the 90% level; no star: not significant.

# \donttest{
# Linear: a mean difference in dollars.
tab_reg(car_salaries, "salary", c("sex", "discipline", "rank"))
#> | predictors       | Model fit                    |    salary |
#> |:-----------------|:-----------------------------|----------:|
#> | sex, discipline, | N                            |       397 |
#> | rank             | Dispersion (robust/model SE) |      1.00 |
#> |                  | Collinearity (max VIF)       |      1.02 |
#> |                  | Influence (max dfbetas)      |      0.22 |
#> |                  | F                            |    <0.01% |
#> |                  | R2                           |     0.447 |
#> |                  | Adjusted R2                  |     0.441 |
#> |                  | Residual SD                  | 22 640.99 |
#> 
#> # A tabxplor tab: 8 × 5
#> # Outcome:        salary
#> # Groups:         var [4]
#>   var        levels              n             Obs_diff           Model_diff
#>                                <n>    <(obs mean) diff>    <diff (adj mean)>
#> 1 Constant   Reference profile   6                                 68 224   
#> 
#> 2 sex        Female             39 (101 002)       0          0    (109 656)
#> 3 sex        Male              358 (115 090) +14 088***  +4 492    (114 148)
#> 
#> 4 discipline A                 181 (108 548)       0          0    (106 248)
#> 5 discipline B                 216 (118 029)  +9 480*** +13 709*** (119 957)
#> 
#> 6 rank       AsstProf           67 ( 80 776)       0          0    ( 79 733)
#> 7 rank       AssocProf          64 ( 93 876) +13 100*** +13 723*** ( 93 456)
#> 8 rank       Prof              266 (126 772) +45 996*** +47 403*** (127 136)
#> # Model: linear regression; diff: mean difference (vs the reference category); obs mean: observed mean; adj mean: adjusted/predicted mean.
#> # Obs_diff, Model_diff: diff in SD (ref.): -0.8 -0.4 -0.2 -0.1 +0.1 +0.2 +0.4 +0.8 [grey: non-significant or under ±0.1 SD]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.

# A count outcome: incidence-rate ratios.
tab_reg(car_arrests, "checks", c("colour", "employed"), family = "poisson")
#> | predictors       | Model fit                    | checks |
#> |:-----------------|:-----------------------------|-------:|
#> | colour, employed | N                            |  5 226 |
#> |                  | Dispersion (robust/model SE) |   1.23 |
#> |                  | Pearson dispersion (phi)     |   1.40 |
#> |                  | Collinearity (max VIF)       |   1.02 |
#> |                  | Influence (max dfbetas)      |   0.07 |
#> |                  | LR vs null                   | <0.01% |
#> |                  | McFadden R2                  |  0.030 |
#> |                  | AIC                          | 18 058 |
#> |                  | BIC                          | 18 078 |
#> 
#> # A tabxplor tab: 5 × 5
#> # Outcome:        checks
#> # Groups:         var [3]
#>   var      levels                n            Obs_IRR          Model_IRR
#>                                <n> <(obs mean) ratio> <ratio (adj mean)>
#> 1 Constant Reference profile 3 200                                1.3   
#> 
#> 2 colour   White             3 938     (1.5)     1            1    (1.5)
#> 3 colour   Black             1 288     (2.1) ×1.41***     ×1.34*** (2.0)
#> 
#> 4 employed Yes               4 111     (1.4)     1            1    (1.5)
#> 5 employed No                1 115     (2.4) ×1.63***     ×1.57*** (2.3)
#> # Model: Poisson regression; IRR: incidence-rate ratio (vs the reference category); obs mean: observed mean; adj mean: adjusted/predicted mean.
#> # Obs_IRR, Model_IRR: IRR (ref.): ÷4 ÷2 ÷1.15 ×1.15 ×2 ×4 [grey: non-significant or under ×1.15]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.

# `measure` reports another measure WITHOUT changing the model: a MARGINAL risk ratio,
# averaged over the sample, still from the logistic fit.
tab_reg(car_arrests, "released", c("colour", "checks"), measure = "ratio")
#> | predictors     | Model fit                    | released |
#> |:---------------|:-----------------------------|---------:|
#> | colour, checks | N                            |    5 226 |
#> |                | Dispersion (robust/model SE) |     1.02 |
#> |                | Collinearity (max VIF)       |     1.01 |
#> |                | Influence (max dfbetas)      |     0.06 |
#> |                | LR vs null                   |   <0.01% |
#> |                | McFadden R2                  |    0.076 |
#> |                | AIC                          |    4 420 |
#> |                | BIC                          |    4 440 |
#> 
#> # A tabxplor tab: 4 × 5
#> # Outcome:        released
#> # Groups:         var [3]
#>   var      levels                             n         Obs_RR      Model_mRR
#>                                             <n> <(obs%) ratio> <ratio (adj%)>
#> 1 Constant Population average             5 226                        83%   
#> 
#> 2 colour   White                          3 938 (86%)     1        1    (85%)
#> 3 colour   Black                          1 288 (74%) ÷1.16*** ÷1.10*** (77%)
#> 
#> 4 checks   per 3.08 (2SD), at 1.64 (mean)             ÷1.40*** ÷1.36***      
#> # Model: logistic regression; mRR: marginal risk ratio (the ratio of adjusted proportions, sample-averaged); obs%: observed proportion; adj%: adjusted/predicted proportion.
#> # Obs_RR, Model_mRR: RR (ref.): ×2 [grey: non-significant or under ×2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.

# `link` changes the model: the CONDITIONAL risk ratio of a modified-Poisson fit.
tab_reg(car_arrests, "released", c("colour", "checks"), link = "ratio")
#> | predictors     | Model fit                    | released |
#> |:---------------|:-----------------------------|---------:|
#> | colour, checks | N                            |    5 226 |
#> |                | Dispersion (robust/model SE) |     1.00 |
#> |                | Collinearity (max VIF)       |     1.02 |
#> |                | Influence (max dfbetas)      |     0.06 |
#> |                | Wald vs null                 |   <0.01% |
#> 
#> # A tabxplor tab: 4 × 5
#> # Outcome:        released
#> # Groups:         var [3]
#>   var      levels                             n         Obs_RR       Model_RR
#>                                             <n> <(obs%) ratio> <ratio (adj%)>
#> 1 Constant Reference profile                                           84%   
#> 
#> 2 colour   White                          3 938 (86%)     1        1    (85%)
#> 3 colour   Black                          1 288 (74%) ÷1.16*** ÷1.11*** (77%)
#> 
#> 4 checks   per 3.08 (2SD), at 1.64 (mean)             ÷1.26*** ÷1.24***      
#> # Model: modified Poisson regression; RR: risk ratio (vs the reference category); obs%: observed proportion; adj%: adjusted/predicted proportion.
#> # Obs_RR, Model_RR: RR (ref.): ×2 [grey: non-significant or under ×2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.

# A named list of predictor sets: one column per model, compared in the footer.
tab_reg(car_salaries, "salary",
        list("sex alone" = "sex",
             "+ field"   = c("sex", "discipline"),
             "+ rank"    = c("sex", "discipline", "rank")),
        empirical = FALSE)
#> | Model fit                    | sex alone |   |   + field |   |    + rank |
#> |:-----------------------------|----------:|:-:|----------:|:-:|----------:|
#> | N                            |       397 |   |       397 |   |       397 |
#> | Dispersion (robust/model SE) |      0.87 |   |      1.00 |   |      1.00 |
#> | Collinearity (max VIF)       |           |   |      1.00 |   |      1.02 |
#> | Influence (max dfbetas)      |      0.32 |   |      0.29 |   |      0.22 |
#> | F                            |    0.567% |   |   0.0159% |   |    <0.01% |
#> | R2                           |     0.019 |   |     0.043 |   |     0.447 |
#> | Adjusted R2                  |     0.017 |   |     0.039 |   |     0.441 |
#> | Residual SD                  | 30 034.61 |   | 29 699.35 |   | 22 640.99 |
#> | F vs previous                |           |   |    0.172% |   |    <0.01% |
#> 
#> # A tabxplor tab: 8 × 6
#> # Outcome:        salary
#> # Groups:         var [4]
#>   var        levels                    n `sex alone`  `+ field`   `+ rank`
#>                                <n_range>      <diff>     <diff>     <diff>
#> 1 Constant   Reference profile      6-39  101 002     95 914     68 224   
#> 
#> 2 sex        Female                   39        0          0          0   
#> 3 sex        Male                    358  +14 088*** +14 029***  +4 492   
#> 
#> 4 discipline A                       181                   0          0   
#> 5 discipline B                       216              +9 449*** +13 709***
#> 
#> 6 rank       AsstProf                 67                              0   
#> 7 rank       AssocProf                64                        +13 723***
#> 8 rank       Prof                    266                        +47 403***
#> # Model: linear regression of salary; diff: mean difference (vs the reference category).
#> # diff in SD (ref.): -0.8 -0.4 -0.2 -0.1 +0.1 +0.2 +0.4 +0.8 [grey: non-significant or under ±0.1 SD]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.

# A continuous predictor cut into groups, on French survey data:
tab_reg(questionr_hdv, "cinema", c("qualif", "age"), shape = c(age = "quartiles"))
#> ℹ "cinema": binary outcome detected -> `family = "binomial"` (logit).
#> This message is displayed once per session.
#> | predictors  | Model fit                    | cinema |
#> |:------------|:-----------------------------|-------:|
#> | qualif, age | N                            |  1 653 |
#> |             | Dispersion (robust/model SE) |   1.07 |
#> |             | Collinearity (max VIF)       |   1.03 |
#> |             | Influence (max dfbetas)      |   0.22 |
#> |             | LR vs null                   | <0.01% |
#> |             | McFadden R2                  |  0.160 |
#> |             | AIC                          |  1 904 |
#> |             | BIC                          |  1 958 |
#> 
#> # A tabxplor tab: 12 × 5
#> # Outcome:        cinema
#> # Groups:         var [3]
#>    var      levels                     n          Obs_OR        Model_OR
#>                                      <n>     <(obs%) OR>     <OR (adj%)>
#>  1 Constant Reference profile         49                   6.26*** (86%)
#> 
#>  2 qualif   Cadre                    260 (65%)      1         1    (67%)
#>  3 qualif   Ouvrier specialise       203 (22%) 1/6.63*** 1/9.39*** (23%)
#>  4 qualif   Ouvrier qualifie         292 (25%) 1/5.67*** 1/7.72*** (26%)
#>  5 qualif   Technicien                86 (43%) 1/2.50*** 1/3.73*** (40%)
#>  6 qualif   Profession intermediaire 160 (46%) 1/2.25*** 1/2.76*** (47%)
#>  7 qualif   Employe                  594 (44%) 1/2.39*** 1/3.40*** (42%)
#>  8 qualif   Autre                     58 (47%) 1/2.17*** 1/3.28*** (43%)
#> 
#>  9 age      18 to 34                 368 (62%)      1         1    (62%)
#> 10 age      35 to 47                 457 (55%) 1/1.33**  1/1.43**  (54%)
#> 11 age      48 to 59                 432 (32%) 1/3.39*** 1/3.90*** (32%)
#> 12 age      60 to 97                 396 (18%) 1/7.37*** 1/9.45*** (18%)
#> # Model: logistic regression; OR: odds ratio (vs the reference category); obs%: observed proportion; adj%: adjusted/predicted proportion.
#> # Obs_OR, Model_OR: OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: non-significant or under ×1.2]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level (from 1 for the Constant); **: at the 95% level; *: at the 90% level; no star: not significant.
# }
options(.opt)
```
