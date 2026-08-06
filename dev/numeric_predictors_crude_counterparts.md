# Crude (`Obs_*`) counterparts for numeric predictors — design study

Round 1: 2026-08-05 (Last Phase z7, item 2). **Round 2: 2026-08-06** — a second research pass driven by
the maintainer's answers to round 1's open questions, plus the two follow-up questions raised while
reviewing this file (§0.2). Status: **FULLY IMPLEMENTED** (Last Phase z9, 2026-08-06).

**Implementation corrections to this report** (measured while building it):

1. **§11.3 / §3.3 are wrong about `haven_labelled`.** `is.numeric(haven::labelled(...))` returns TRUE,
   so `is.factor || is.character` and `!is.numeric` *agree* for it. Only **`logical`** and
   **`Date`/`POSIXct`** ever diverged — and a logical was doubly broken (the skeleton gave it the
   numeric arm, `term = <var>`, while `glm` names the coefficient `<var>TRUE`, so it rendered a
   completely blank row). The shipped predicate is `reg_is_factor_var()` =
   `is.factor || is.character || is.logical`; Date stays numeric, where it already worked.
2. **§14.1's overlay point had to move.** Splicing the numeric rows before `emp_col()` is a live bug: on
   the binomial `ame` branch the base and effect columns are built from the SAME `rd_fields` list, and
   `REG_EMPIRICAL$binomial$base` declares `color = "diff"` — so the AME would land in `Obs_%`'s `diff`
   field and colour a cell that displays nothing. It ships inside `two()`, the one place the effect
   shape is known.
3. **§13's "two IF paths" is right, and the AME arm is not optional.** `reg_estimand_collapsible()`
   already refuses the binomial COEFFICIENT gap test, so for the flagship logistic table the numeric
   gap test lives *entirely* in `reg_ame_if_maker()`'s new numeric counterfactual.
4. **§10's cost note gained a consequence**: the AME crude counterpart's ~229 ms per predictor is what
   motivates the `tab_reg()` parallelisation phase the maintainer asked for in Q7 (CLAUDE.md, z9b).
5. The `n` of a numeric crude cell is `NA`, not the model's *n* — matching the model column's own rule
   ("whole-model N is in the footer, not a per-cell n:").

Scope: `tab_reg(..., empirical = TRUE)` builds an observed/crude companion beside each model effect —
but **only for factor predictors**. Numeric predictors get `NA` in both crude columns. This report asks
what they should get, and answers the maintainer's architectural objections one by one.

Every number below was measured on this box (`gss_simple`, complete cases as stated) or simulated.
Round-2 scripts are in the session scratchpad; the measured tables are reproduced inline.

---

## 0. Executive summary

### 0.1 The verdict

**Fill the effect column from a univariable fit; leave the base column blank; and solve "polish" with a
standardised unit, not with a filled cell.**

1. **The effect column is not a design problem at all.** An OR per unit *is* an OR: same field, same
   `ci_type`, same break scale, same colour, same legend. The crude effect for a numeric predictor is
   the univariable model's effect — which is *already the rule tabxplor applies to factors*, where the
   univariable model happens to be saturated and so has a closed form. Extending it removes a special
   case rather than adding one. §2, §3.
2. **The producer already exists.** `reg_fit()` takes `(data, dependent, predictors, family,
   design_spec, do_exp, conf_level, method, trials, formula, multiplier, cross)`. Calling it with one
   predictor gives the crude effect **with the right family, the right design, the right CI method and
   the right `multiplier`** — so Q6's exactness rule is satisfied *by construction*, not by a mirrored
   line of code. §3.2, §9.
3. **Nothing can honestly fill the base column, and I measured why.** The maintainer asked whether the
   univariable fit could supply it. It cannot: the fit's only base-scale output is a predicted
   probability, and `P̂(Y | X = mean X)` came out **0.4738 for `age` and 0.4738 for `tvhours`** against
   an overall rate of **0.4744** — i.e. the same number on every numeric row, because a logistic fit
   reproduces the marginal rate at the mean. A cell that looks per-predictor but is not is worse than a
   blank. §4.
4. **The polish the maintainer wants is a unit problem, not a missing-cell problem.** Per 1 year, the
   crude OR of `age` is 0.969 — inside the first colour break, so it never colours, and it reads as
   "nothing". Per 1 SD it is **0.657 crude → 0.683 adjusted**, which colours, and which sits on the
   same visual scale as the factor contrasts in the same table (measured: 2.23, 0.93, 0.66, 0.86,
   1.02). **Recommendation: `multiplier = "sd"`** — one new accepted value on an existing argument.
   It costs nothing, it applies identically to the crude and the model column (both go through
   `reg_fit(multiplier=)`), and it is the standard "standardised coefficient" of the social sciences.
   §5.
5. **`tab()` already computes the observed association of a numeric variable** — `tab(married, age)`
   gives `49 (σ15)` / `46 (σ19)` per outcome level with a standardised difference and a closed-form CI.
   What `tab()` cannot produce is that association *on the model's scale*. §6 measures the three
   classical closed forms: **gaussian is exact** (cov/var, ratio 1.000000), while binomial and the
   log-link families have closed forms that are **exact only for a normal predictor** and degrade to
   50–70 % error under skew. Given Q6, that disqualifies them for the crude column — but it is exactly
   the bridge to document between `tab()` and `tab_reg()`.
6. **The heterogeneous-display objection is empirically dead, and irrelevant anyway.** A single fmt
   column mixing `pct` and `mean` cells formats, pads, colours, exports to md/html and emits per-cell
   Excel numFmt codes correctly today — `display` and `digits` are per-*cell* fields and pillar even
   labels the result `<row%-mixed>`. So the objection to filling the base column was never technical;
   it is semantic, and the semantics is what kills it. §7.
7. **`color = "between_groups"` already works on numeric rows** — verified: the `age` row carries
   `obs = 1.004` and `gap_se = 0.0027 / 0.0035` in a `split_var` table today, because
   `reg_write_group_gap()` keys on the skeleton and never looks at the predictor's class. Only
   `color = "adjustment"` is blocked, and only because `obs` is missing. §8.
8. **Cost**: one 2-parameter fit per numeric predictor — 10.4 ms against 14.6 ms for the full 18-parameter
   model at n = 6819 (71 %, not the 54 % round 1 reported). `effect = "ame"` adds a
   `marginaleffects` call at **229 ms** per numeric predictor, which is the only cost worth flagging.
   §10.
9. **Four incidental defects** surfaced during the audit, all pre-existing and none blocking. §11.

### 0.2 The two follow-up questions, answered

> *"Effect column only, but the base cell gets the univariate model? Would it be reliable?"*

**No — and the reason is arithmetic, not taste.** §4 and item 3 above: the quantity is the same for
every numeric predictor. The univariable fit has no per-predictor output in the base column's units.

> *"Since OR etc. are meaningful for numeric columns too, is there a way to compute observed OR per
> unit in `tab()`? Would it need a multiplier per numeric var to be readable, and how to measure the
> effect in a way that does not depend on the unit, standardised?"*

Three separate answers, all measured:

| sub-question                                        | answer                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|-----------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| an observed OR per unit **in `tab()`, closed-form** | Only approximately, and only for the gaussian family exactly. The binomial closed form (the classical **discriminant / Cornfield–Efron** estimator, `(x̄₁ − x̄₀) / s²ₚ`) is **exact for a normal predictor** (simulated ratio to ML = 1.00) and lands at ×1.010 for `age` but ×0.945 for `tvhours` (skew 2.9) on real data, degrading to ×0.50 when skew and effect are both large. The log-link analogue behaves identically (×1.003 for a near-normal predictor, ×1.68 under lognormal skew). **Q6 forbids shipping an approximation as the crude effect.** §6 |
| does it **need a multiplier to be readable**        | Yes — and that is the real finding. Per 1 unit, a numeric crude/model pair is uncolourable and unreadable next to factor contrasts. §5                                                                                                                                                                                                                                                                                                                                                                                                                           |
| the **unit-free, standardised** measure             | Two exist and tabxplor already owns both: **per 1 SD of X** (`multiplier = "sd"`, recommended — it stays on the OR/IRR/β scale and needs no new colour work), and the **standardised mean difference** (Cohen's *d* / Glass's Δ), which is *literally* what the colour engine already scores for numeric `diff` columns. §5                                                                                                                                                                                                                                      |

> *"I want the numeric crude CI to be computable in `tab()`, not only `tab_reg()`, and `tab()` is not
> full design effect with weights."*

The observed association **is** computable in `tab()` today (mean per outcome level + standardised
difference + closed-form CI); only the OR-scale translation needs the fit. So: the fit stays in
`tab_reg()`, and §7 of the docs gets the bridge sentence plus the honest note that the two functions
answer the same question on two different scales, under two different variance rules. §6.4, §7.

---

## 1. Round 1 recap and the maintainer's rulings

Round 1 established (and nothing in round 2 contradicts):

- `Obs_OR` **is** `exp(coef(glm(y ~ x)))` to 1e-13 — the hand-rolled cell sums are a closed form of the
  univariable fit, not a different estimator. Every family verified.
- Every `REG_EMPIRICAL` CI literal is that fit's variance under the same rule the model column uses
  (Woolf = all three variance estimators at once on a saturated table; Katz = the sandwich, which is
  what z3's `rr` model column uses).
- The real numbers are worth having. `gss_simple`, `married ~ age + tvhours + race + rincome`,
  n = 6819:

  | predictor | crude OR / unit | 95 % CI          | model OR / unit | per 10 units: crude → model |
  |-----------|-----------------|------------------|-----------------|-----------------------------|
  | `age`     | 0.9693          | [0.9657; 0.9729] | 0.9721          | 0.732 → 0.753               |
  | `tvhours` | 1.0993          | [1.0724; 1.1268] | 1.0585          | **2.576 → 1.765**           |

  `tvhours` loses a third of its association to adjustment, on a row that renders blank today.

Maintainer's rulings (round 1 §11), carried forward:

| Q              | ruling                                                                 | round-2 status                                                                                                                                                                                                                      |
|----------------|------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Q1 ship it     | **yes**; asked whether the fit is needed or `tab_num()` maths would do | **Answered §3, §6: the fit is needed for the effect** (Q6 forbids the approximation); `tab_num()`-style maths answers only the descriptive. The "factors together, then numerics together, then join" shape is exactly right — §3.3 |
| Q2 base column | **needs more study**; blank numeric rows are "a no-go for many users"  | **Answered §4 + §5**: nothing can honestly fill it (measured); the polish comes from the standardised unit instead                                                                                                                  |
| Q3 wording     | asked whether "observed" is defensible for a numeric                   | **Answered §12**: yes for gaussian, a stretch for binomial; use one sentence defining `Obs_*` as *observed / unadjusted (univariable)*                                                                                              |
| Q4 weighting   | **Kish-rescale the numeric crude SE**                                  | **Re-opened, §7** — the measurement behind that choice was wrong, and design-based is free                                                                                                                                          |
| Q5 order       | implement the gap test for numerics now                                | **Split, §8**: `between_groups` already works; `adjustment` needs z8 Phase B                                                                                                                                                        |
| Q6 multiplier  | the crude effect must be raised to the same power k                    | **Satisfied by construction** via `reg_fit(multiplier=)`, §9                                                                                                                                                                        |

---

## 2. The crude effect of a numeric predictor is not a new concept

### 2.1 For a factor, "observed" already *is* "univariable"

| crude column                    | equals                            | max abs. difference |
|---------------------------------|-----------------------------------|---------------------|
| `Obs_OR` (binomial)             | `exp(coef(glm(y ~ x, binomial)))` | 9.2e-14             |
| `Obs_RR` (`rr`, Katz)           | `exp(coef(glm(y ~ x, poisson)))`  | 1.8e-10             |
| `Obs_diff` (gaussian)           | `coef(lm(y ~ x))`                 | 1.3e-13             |
| `Obs_IRR` (poisson counts)      | `exp(coef(glm(y ~ x, poisson)))`  | 3.7e-10             |
| `Obs_%` risk difference (`ame`) | `coef(glm(y ~ x, gaussian))`      | 1.7e-14             |
| weighted variants               | the weighted fits                 | ≤ 9.8e-15           |

A one-factor GLM is **saturated**, so its coefficients *are* the raw cell contrasts. tabxplor computes
them from weighted cell sums because that is faster and gives exact closed-form intervals — not because
it is estimating something else.

**So the rule "the crude effect is the univariable model's effect" is already the rule**, with a fast
path where that model is saturated. Extending it to numerics deletes a special case.

### 2.2 The CI method extends with no new method

| crude effect    | model-based SE | sandwich SE | tabxplor's `REG_EMPIRICAL` method    |
|-----------------|----------------|-------------|--------------------------------------|
| log OR          | 0.074469       | 0.074469    | Woolf **0.074469**                   |
| log RR          | 0.054786       | 0.040842    | Katz **0.040842** = the sandwich     |
| mean difference | 0.046915       | 0.047106    | pooled Student **0.046915**          |
| log IRR         | 0.030373       | 0.030306    | quasi-Poisson (φ-scaled model-based) |

Each family's crude interval is already "the univariable fit's interval under the same variance rule as
the model column". For a numeric predictor the same rule gives the Wald / sandwich / Student / φ-scaled
interval of the univariable fit — which is what `reg_fit()` returns. **No new CI function, no new
`REG_EMPIRICAL` row, no per-family branch.**

---

## 3. The effect column: one rule, one existing fitter

### 3.1 Why it is unproblematic

`Model_OR` already holds "vs the reference level" on factor rows and "per one unit" on numeric rows, in
one column, with one header, since Phase 12c. A crude column that refuses the same heterogeneity is not
protecting an invariant — it breaks the symmetry with the column beside it. The "numeric variables
become whole columns" constraint is a **`tab()`** fact (a numeric *col_var* is the axis); in
`tab_reg()` a predictor is a **row**.

| worry                  | verdict                                                                                                                                                                               |
|------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| unit clash in the cell | **None.** An OR is an OR; `digits = 2` suits both row kinds                                                                                                                           |
| the reference row      | **None.** Numerics have no reference level, so no neutral `1.00` — exactly as `Model_OR` already behaves. `refrows` in `emp_col()` is `skeleton$is_ref & is_fac`, already FALSE there |
| colour                 | The scale is right; the *magnitude* is the problem, and it is the same problem `Model_OR` has today. §5 fixes it for both columns at once                                             |
| the base column        | Real — §4                                                                                                                                                                             |

### 3.2 `reg_fit()` is the producer

Measured signature: `reg_fit(data, dependent, predictors, family, design_spec, do_exp,
inverse_two_level_factors, conf_level, method, trials, formula, multiplier, cross)`, returning
`list(tidy, nobs, var_y, positive_level, fit, data)` where `tidy` carries
`term / estimate / std.error / statistic / p.value / conf.low / conf.high` with `estimate` already
exponentiated when `do_exp = TRUE`.

Calling it with `predictors = <one numeric>` and the model's own `design_spec`, `family`, `conf_level`,
`method`, `trials` and `multiplier` therefore produces the crude effect **on exactly the model's scale,
under exactly the model's variance and CI rules**. This is the single most important design point in
this report: it makes Q6 structural rather than a rule someone must remember.

Verified: `reg_fit(d, "married", "age", "binomial", design_spec = NULL, ...)` returns
`estimate = 0.9700451, std.error = 0.00192439, conf.low = 0.9663932, conf.high = 0.9737108`
(the univariable model on the 4-predictor complete-case frame), and the same call with
`multiplier = list(age = 10)` returns the k-scaled estimate and interval.

### 3.3 Shape: two producers, one skeleton join

The maintainer's suggested shape — *"creating all factors observed counterparts together, then all
numeric observed counterparts together, then join both to the reg table"* — is exactly the existing
idiom. `reg_empirical()` already returns a long tibble keyed `(var, level)` and
`reg_empirical_columns()` joins it with `reg_skel_match()` (`paste(var, level, sep = "\r")`). A numeric
predictor's skeleton row is `var = p, level = p, is_ref = FALSE`, so **today's blank is a key miss, not
a guard** — there is no `if (is.numeric)` anywhere in the empirical path. The numeric producer emits
rows on the same key and the existing join absorbs them.

⚠ One consequence to respect: `reg_empirical()` is *not itself* restricted to factors — the caller is
(`reg_build()` at the `fac_preds_e` line). Three different predicates decide "is this predictor a
factor" across the file (`is.factor || is.character` twice, `!is.numeric` twice); they disagree for
logical, `Date` and `haven_labelled` predictors. Unify them while here. §11.

---

## 4. The base column: measured proof that nothing can fill it

`Obs_%` / `Obs_mean` / `Obs_rate` answer *"what is the outcome, within each level of the predictor?"*
A numeric predictor has no levels.

### 4.1 The univariable fit cannot supply it

The maintainer's proposal was to take the base cell from the same univariable fit that gives the effect
— "doesn't the univariate model give a quantity that is computable easily out of the model, with the
matching CI?". The fit's only outputs in the base column's units are predicted probabilities. Measured
(`married ~ X`, n = 6819, overall rate **0.4744**):

| predictor | `P̂(Y \| X = mean X)` | 95 % CI          | mean of fitted values |
|-----------|-----------------------|------------------|-----------------------|
| `age`     | **0.4738**            | [0.4617; 0.4859] | 0.4744                |
| `tvhours` | **0.4738**            | [0.4619; 0.4857] | 0.4744                |

Identical to three decimals, for both predictors, and equal to the marginal rate — which is not a
coincidence but a property of the logistic fit (the score equation for the intercept forces the mean
fitted value to the observed rate; evaluating at `mean(X)` is that same quantity up to a small Jensen
term). The other candidates fail differently: `P̂` at X = 0 is the intercept (meaningless for `age`);
`P̂` at mean ± σ is two numbers and is an *effect*, which is the other column's job.

**Conclusion: the fit has nothing per-predictor to say in the base column's units.** A cell showing
47 % on every numeric row, looking like a fact about that predictor, is worse than a blank.

### 4.2 The descriptive that *is* well defined is conditioned the other way

`mean(X | Y)` is well defined and `tab()` already produces it:

```
  married             age    tvhours
1 01-Married     49 (σ15) 2.7 (σ2.1)
2 02-Not married 46 (σ19) 3.3 (σ2.9)
3 Total          47 (σ17) 3.0 (σ2.6)
```

But the base column holds `P(Y | X)`. For a factor these are the row% and col% views of one cross-tab;
for a numeric only the second exists. Putting a mean age (in years) in a column whose other cells are
outcome percentages is the field overload the 2.0.0 rewrite removed. It is also worst exactly where it
would look most plausible: for **gaussian**, `Obs_mean` holds means of *Y*, so a mean of *X* there is
indistinguishable at a glance.

### 4.3 Options considered, and why the blank wins

| # | option                                                                           | verdict                                                                                                                                                                                                                                                                                                                            |
|---|----------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | **base cell stays `NA`; the descriptive goes in the tooltip**                    | **Recommended, and the maintainer's choice.** `reg_empirical_tips` + the `empirical_tips` meta attribute already carry exactly this kind of "too rich for a cell" crude number for multinomial, and the render seam (`reg_append_empirical_tip`) is a two-line hop                                                                 |
| 2 | `mean (σ)` of X in the base cell                                                 | Technically free (§7) but semantically dual; and the σ tail is gated on the column attribute `type == "mean"`, so on binomial's `type = "row"` column it would render a bare `42.3` unless the gate is relaxed. Rejected on meaning, not on mechanism                                                                              |
| 3 | `P̂(Y \| X = mean X)` from the fit                                               | **Rejected by measurement**, §4.1                                                                                                                                                                                                                                                                                                  |
| 4 | bin the numeric and show `P(Y \| bin)`                                           | **Rejected.** tabxplor must not choose a cut-point silently. The user's own `cut()` is the honest route and is often the better analysis — worth one vignette line                                                                                                                                                                 |
| 5 | a separate uniform "Sample" column (`n (%)` for levels, `mean (σ)` for numerics) | The structurally pure answer (every column keeps one meaning; it is the finalfit/gtsummary Table-1 column) and it renders correctly today — but it is **a different feature**, orthogonal to crude-vs-adjusted, and it costs a column. Park it: if the blank still bothers users after §5 lands, this is the upgrade, not option 2 |

---

## 5. The real polish: a standardised unit

This is round 2's main new proposal, and it answers the maintainer's *"how to measure effect in a way
that does not depend on the unit, standardised?"* far better than any cell-filling.

### 5.1 The problem, quantified

`gss_simple`, `married ~ age + tvhours + race + rincome`, n = 6819:

| predictor | SD    | crude / unit | model / unit | **crude / SD** | **model / SD** | crude / 2 SD | model / 2 SD |
|-----------|-------|--------------|--------------|----------------|----------------|--------------|--------------|
| `age`     | 13.46 | 0.969        | 0.972        | **0.657**      | **0.683**      | 0.431        | 0.466        |
| `tvhours` | 2.09  | 1.099        | 1.058        | **1.219**      | **1.126**      | 1.486        | 1.268        |

Factor contrasts in the same table, for scale: 2.227, 0.930, 0.660, 0.862, 1.015.

Per unit, both numeric rows sit inside the first colour break (`odds_ratio` starts at 1.2) and therefore
**never colour** — the row reads as "no effect" when `age` in fact multiplies the odds by 0.66 per SD.
Per SD, the numerics land squarely in the same range as the factor contrasts, colour normally, and the
crude→model gap becomes visible (`age` 0.657 → 0.683, `tvhours` 1.219 → 1.126).

### 5.2 The proposal: `multiplier = "sd"`

`multiplier` already exists, already accepts a per-variable numeric, already scales the tidy *before*
the Wald interval is formed (so the CI and p scale with it), already relabels the level as
`"age (per 10)"`, and is already restricted to numeric predictors. Accepting the string `"sd"` (and, for
Gelman's argument, optionally `"2sd"`) means:

- one new accepted value on one existing argument;
- the crude and model columns scale **identically**, because both go through `reg_fit(multiplier=)`;
- the level label becomes self-describing — `per 1 SD (13.5 years)` — which is also the answer to the
  redundant `var = age, levels = age` that numeric rows show today;
- no new colour work, no new scale, no new field.

⚠ **The one hard constraint: the SD must be computed ONCE, on a common frame.** Measured instability:

| where                                        | SD of `age`                    |
|----------------------------------------------|--------------------------------|
| by `split_var = race`                        | 13.59 / 12.66 / 12.39          |
| by a 2-group income split                    | **15.91 / 12.22** (30 % apart) |
| across compared models' complete-case frames | 17.287 / 17.388 / 17.396       |

A per-group SD would make `color = "between_groups"` compare *different quantities* — the exact failure
Q6 forbids. Fix the SD on the union/estimation frame, store it in `reg_meta`, and reuse it for every
group and every compared model.

### 5.3 The other standardisation, already in the package

The **standardised mean difference** (Cohen's *d* / Glass's Δ) is the unit-free measure of a numeric
variable's association with a group — and it is *literally* what tabxplor's colour engine already scores
for numeric `diff` columns (the `mean_diff` scale, breaks 0.2 / 0.5 / 0.8). Worth saying out loud in the
docs, because it means the answer to "standardised effect of a numeric variable" already existed on the
`tab()` side; §6 is the bridge between the two.

(Round 1 rejected "auto-standardising numeric predictors" because it would silently change the model
column. That objection stands for an *automatic* default; it does not apply to an explicit
`multiplier = "sd"`, which is a user choice and scales both columns together.)

---

## 6. Closed forms: what `tab()` can and cannot compute

The maintainer wants the numeric crude quantity computable in `tab()`, without a fit. Measured, per
family.

### 6.1 Gaussian — exact

The univariable slope is `cov(X, Y) / var(X)`:

```
fit 0.00385170790   closed form 0.00385170790   ratio 1.000000
```

Exact to machine precision, from moments `tab_num()` already computes.

### 6.2 Binomial — the discriminant estimator, exact only under normality

The classical Cornfield/Efron **discriminant** estimate of a logistic slope is
`β̂ = (x̄₁ − x̄₀) / s²ₚ` (pooled within-group variance) — moments only.

Real data (`married ~ X`, n = 6819):

| predictor | ML logit slope | discriminant | ratio     | skew of X |
|-----------|----------------|--------------|-----------|-----------|
| `age`     | 0.03122        | 0.03154      | **1.010** | 0.32      |
| `tvhours` | −0.09463       | −0.08942     | **0.945** | 2.89      |

Simulated (n = 4000 × 60 replicates):

| X         | true β | ML     | discriminant | ratio    |
|-----------|--------|--------|--------------|----------|
| normal    | 0.2    | 0.2020 | 0.2018       | **1.00** |
| normal    | 0.5    | 0.5032 | 0.5030       | **1.00** |
| normal    | 1.0    | 0.9939 | 0.9891       | 0.995    |
| lognormal | 0.2    | 0.1951 | 0.1830       | 0.94     |
| lognormal | 0.5    | 0.5044 | 0.3616       | **0.72** |
| lognormal | 1.0    | 1.0070 | 0.5025       | **0.50** |

Exact for a normal predictor; degrades with skew **and** with effect size, to 50 % error in the worst
cell.

⚠ Also measured and worth recording so nobody re-tries it: the **Hasselblad–Hedges / Chinn** conversion
`log OR = 1.8138 × d`, which the meta-analysis literature uses, is the wrong tool here — it converts an
effect size when the *continuous variable is the outcome*. Applied in the predictor direction it
overstates by a factor of ~1.8 consistently (measured ratios to ML: 1.80, 1.76, 1.65 on normal
simulations).

### 6.3 Log-link families (poisson IRR, `rr` risk ratio) — same story

The analogue is an exponential-tilt identity, `β̂ ≈ (mean of X weighted by Y − mean X) / var(X)`:

| case                                   | ML              | closed form     | ratio             |
|----------------------------------------|-----------------|-----------------|-------------------|
| poisson counts, `tvhours ~ age`        | logIRR 0.001498 | 0.001502        | **1.003**         |
| `rr` on binary, `age`                  | logRR 0.015450  | 0.015896        | 1.029             |
| `rr` on binary, `tvhours` (skew 2.9)   | logRR −0.053804 | −0.046602       | **0.866**         |
| simulated normal X, β = 0.05 / 0.20    | 0.0518 / 0.1998 | 0.0518 / 0.1997 | **1.000 / 1.000** |
| simulated lognormal X, β = 0.05 / 0.20 | 0.0540 / 0.2005 | 0.0606 / 0.3370 | 1.122 / **1.681** |

Identical pattern: exact under normality, badly wrong under skew.

### 6.4 What this means

- **Q6 forbids shipping any of §6.2–6.3 as the crude effect.** A 5 % error on a real, mildly skewed
  variable — and up to 68 % on a skewed one — would make "observed vs modelised" an artefact of the
  predictor's shape. The fit stays.
- **§6.1 means the gaussian crude effect *could* be closed-form.** Use the fit anyway: one rule beats
  two paths, and the fit is 0.8 ms.
- **The identity is excellent documentation.** It is the honest bridge the maintainer is looking for:
  a `tab()` standardised mean difference and a `tab_reg()` crude OR are two views of one association,
  related exactly under normality and approximately otherwise. One vignette paragraph, one `?tab_reg`
  sentence. It also gives the user a *reason* to check linearity/normality — which they should do
  anyway before keeping a numeric predictor in a model.
- **No approximate OR should be added to `tab()`.** It would be a second, silently-different estimator
  of a quantity `tab_reg()` computes exactly — two encodings of one fact, which is the disease Phase 17
  spent itself removing.

---

## 7. The architecture objections, tested

The maintainer's Q2 raised a list of technical worries about mixing row kinds in one column. Every one
was tested; the summary is that **rendering is already per-cell, semantics is per-column, and the
obstacle was never rendering.**

| question                                         | measured answer                                                                                                                                                                                                                                                                                                                                                                                                     |
|--------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| can one column print percentages and means?      | **Yes.** `display` and `digits` are per-**cell** vctrs fields (`fmt_field_names`, 21 fields). A column with `display = c("pct","pct","mean","mean")` formats as `"51%" "28%" "42" "2.6"`, emits per-cell Excel numFmt `"0%" "0%" "#,##0" "#,##0.0"`, renders correctly in md and html, and pillar labels the ptype `<row%-mixed>`                                                                                   |
| what breaks                                      | Only the σ tail: `disp_mean_sd <- display == "mean" & type == "mean" & …` gates a *display* decision on a *column* attribute, so a mean cell in a `type = "row"` column loses its `(σ13.1)`. Relaxing that gate is a legitimate Phase-17-style decoupling — but it is only needed if option 2 of §4.3 is ever adopted                                                                                               |
| colour breaks / scales                           | **The scale is per column and cannot vary per cell** — `switch(md$std_when, …)` is structurally scalar and the plan carries fixed `over_breaks`/`under_breaks` into two `findInterval` calls. But numeric base cells would carry no `diff` and no reference, so they gate to slot 0 automatically. **No colour work either way**                                                                                    |
| the `color = c(text, bg)` two-channel trick      | **Rejected.** Both channels apply to every cell; the "trick" would only work because each measure happens to be NA on the other row kind — an implicit coupling, not a mechanism. It would also burn the background channel and make the legend describe two measures for one column                                                                                                                                |
| a new per-cell `type` attribute                  | **Not needed, and would not help.** `type` is a colour/legend key, not a rendering key; rendering is already per-cell, and the cells in question want no colour. Making `type` per-cell would mean rewriting `fmt_color_plan()`'s scalar `switch` and every `switch(type, …)` in `format()`, for zero gain                                                                                                          |
| "a robust test to detect which rows are numeric" | **There is none today.** Nothing records predictor types in the built table: `reg_meta$predictors` is a bare name vector, and the only implicit marker is the `level == var` convention — which `cleannames` and the `multiplier` relabel **already break**. Store it: `reg_meta$predictor_types` (one named character vector), read wherever needed. This is Phase 17's rule 2 ("roles are stored, never guessed") |

---

## 8. Weighting and the SE rule (re-opening Q4)

Round 1 §9 reported the Kish rescale as 9 % below the design-based SE, and the maintainer chose Kish on
that basis. **That measurement was one draw of one configuration.** Round 2, 5 weight shapes × 5 terms,
`married ~ age + tvhours + race`, n = 6819:

| weight shape     | deff | kish / design | naive / design |
|------------------|------|---------------|----------------|
| gamma(4,4)       | 1.25 | 0.988 – 1.042 | 0.883 – 0.931  |
| gamma(1,1)       | 1.96 | 0.944 – 1.022 | 0.675 – 0.730  |
| lognormal(0, .6) | 1.45 | 0.849 – 1.044 | 0.706 – 0.868  |
| lognormal(0, 1)  | 2.76 | 0.969 – 1.133 | 0.583 – 0.682  |
| 2-strata 1:5     | 1.70 | 0.976 – 1.030 | 0.743 – 0.789  |

Kish removes essentially all of the design effect (mostly within ±5 %, worst ±15 %), where naive is
25–40 % too narrow. So Kish is a *good* first-order rule — but that is not the deciding fact.

**The deciding fact is that design-based costs nothing.** `reg_fit()` already takes `design_spec`, and
`tab_reg()` already builds a design whenever `wt` is given. Passing the model's own design to the
univariable fit gives an **exact** interval under **the same rule the `Model_` column uses** — which is
the entire point of putting the two side by side, and what the z8 Phase-B gap test will need. The Kish
route, by contrast, requires threading a new `se_scale` through `reg_fit()` → `reg_wald_finalize()`, a
path whose byte-identity is locked by `test-jmvtabreg-cache.R`.

**Recommendation: design-based for the numeric crude fit**, i.e. simply pass `design_spec`. Factor crude
rows keep their existing Kish-rescaled closed forms (they have no fit to give a design-based interval),
and the two rules agree within a few percent — measured above. Unweighted data: the two coincide
exactly.

**The documented discrepancy the maintainer asked for**, in one sentence for `?tab_reg` and the
vignette: *`tab()`'s numeric confidence intervals are descriptive and use the raw (or Kish-effective)
sample size; `tab_reg()`'s crude columns use the model's own design-based variance. On weighted data the
two will not match to the last digit — they answer the same question on different scales and under
different variance rules.*

---

## 9. `multiplier` and Q6

Confirmed by audit: `multiplier` is applied at **exactly one site**, inside `reg_fit()`, scaling
`td$estimate` and `td$std.error` on the native tidy *before* the interval is formed — so the CI and p
scale correctly and `exp()` afterwards yields OR^k. It matches on `td$term == v`, so factor levels
(whose term is `paste0(var, level)`) are structurally unreachable. It touches **only model columns
today**, because numeric predictors have no crude cells.

The moment numeric crude cells exist, that becomes a live hazard: `Model_OR` would be OR^k while
`Obs_OR` stayed per-1-unit, and `obs` / `color = "adjustment"` would compare a scaled estimate against
an unscaled one. **Routing the numeric crude through `reg_fit(multiplier = multiplier)` makes the
scaling identical by construction** — the single strongest argument for reusing the fitter rather than
hand-rolling the crude fit. It needs a fixture regardless (`multiplier = 10` ⇒ `Obs_OR` = the per-1-unit
value ^10).

The same rule then covers `multiplier = "sd"` (§5) for free.

---

## 10. Cost

`gss_simple`, n = 6819, measured (mean of 5 runs):

```
full model glm (18 parameters)     14.6 ms
univariable glm (2 parameters)     10.4 ms      (71 % of one full-model fit)
univariable lm  (gaussian)          0.8 ms
moments scan (mean + var)          ~0   ms
crude AME via marginaleffects     229   ms      <- the one number to watch
```

So *k* numeric predictors cost roughly *k* × 0.7 full-model fits on the coefficient path — 21 ms for
three numerics on a 7 000-row survey, dominated by the model itself. Round 1's "54 %" figure was
measured on a different model; 71 % is the corrected value.

**`effect = "ame"` / `"ame_ratio"` is the exception**: the crude counterpart is the univariable AME,
i.e. one `marginaleffects::avg_comparisons()` per numeric predictor at ~229 ms. For three numerics that
is ~0.7 s added to an interactive jamovi round-trip. Decide consciously: compute it, or restrict numeric
crude counterparts to the coefficient path in a first pass (the `at = "reference"` gate already sets the
precedent of writing no `obs` when the estimands do not match).

---

## 11. Incidental defects found during the audit

All pre-existing, none blocking, all cheap:

1. **The Constant row loses its bold when `empirical = TRUE`.** `emp_col()` sets
   `in_refrow = skeleton$is_ref & is_fac`, while the model column uses
   `(is_ref & var != "Constant") | var == "Constant"`. `tab_bold_rows()` requires a row to be an anchor
   in *every* discriminating column, so the Constant row drops out.
2. **`get_num()` has no `"OR_pct"` arm** (only lowercase `"or_pct"`), while `format()` does — so an
   `OR_pct` cell returns the raw **count**. Unreachable from R, but the jamovi display ComboBox writes
   `"OR_pct"` verbatim (`jmvtab.h.R`).
3. **Three disagreeing "is this predictor a factor" predicates** in `tab_reg.R`
   (`is.factor || is.character` twice, `!is.numeric` twice). They diverge for logical, `Date` and
   `haven_labelled` predictors, which then get crude rows on keys no skeleton row matches.
4. **`.claude/skills/vctrs-field/SKILL.md` is stale** — it says 18 fields / 9 attributes; the truth is
   21 / 11 (`n_eff`, `obs`, `gap_se`; `model_family`, `role`), and its line anchors are ~400 lines off.

---

## 12. Wording (Q3), settled

The maintainer asked whether social scientists would accept calling a numeric crude effect "observed",
and whether that would be a no-go for statisticians. The honest position:

- For **gaussian**, the crude slope is `cov/var` — a descriptive statistic, like a correlation. "Observed"
  is literally true, and §6.1 proves it needs no fit.
- For **binomial / poisson / rr**, the crude effect per unit is an ML estimate under a link and a
  linearity assumption. There is no closed form (§6.2–6.3), so "observed" is a stretch a statistician
  would notice.
- But the factor case is *also* a univariable model — merely a saturated one. So the word that covers
  both row kinds without lying is **unadjusted**.

**Adopt one sentence, everywhere** (`?tab_reg`, the EN/FR regression vignettes, the legend):

> The `Obs_*` columns show the **observed, unadjusted (univariable)** effect: with a categorical
> predictor that is exactly the observed contrast between levels; with a continuous predictor it is the
> univariable slope, which assumes the effect is linear on the model's scale — check that (splines,
> `cut()`) before trusting it.

This *replaces* five per-family recipes in the current documentation with one rule, so the docs get
shorter. It also matches the two packages users compare tabxplor with: **finalfit** labels the column
"OR (univariable)", **gtsummary**'s `tbl_uvregression()` calls it *unadjusted*; both apply it to
continuous and categorical predictors without distinction, and have for years.

Renaming the prefix `Obs_` → `Unadj_` is **not** recommended: it shipped in Last Phase g, it is right
for the 95 % case, and it is shorter in every column header.

---

## 13. The gap test for numeric rows (Q5)

Two different situations, and only one is open:

- **`color = "between_groups"` already works on numeric rows.** Verified on a `split_var` table: the
  `age` row carries `obs = 1.004` and `gap_se = 0.0027 / 0.0035`. `reg_write_group_gap()` loops every
  fmt column and keys on `reg_skel_key()`; it never looks at the predictor's class, and a numeric row
  has a model estimate in each group. **Nothing to do.**
- **`color = "adjustment"` is blocked only by the missing `obs`** — which is precisely what this feature
  writes. Its *significance* (`gap_se` on the adjustment path) was done for factors in z8 **Phase B** : it should be implemented for numeric variables too.

⚠ **One constraint this phase must respect.** For factors the crude effect is closed-form,
so Phase B must build its influence function from cell sums; for numerics it comes from a fit. We
will therefore have two IF paths, and this phase should make the numeric one reachable — either keep the
univariable fit alongside the crude effect, or store what is needed. Round 1 measured the composed
gap on `gss_simple` (`age` z = +5.32, p = 1.1e-07; `tvhours` z = −7.66, p = 1.9e-14), so the machinery is
known to work; it just needs the fit in hand.

⚠ The `at = "reference"` gate must be honoured: it deliberately writes no `obs` at all, and numeric rows
must follow the same rule.

---

## 14. Recommended solution

**Ship the effect column; leave the base column blank; make numeric rows readable with a standardised
unit and a self-describing label.**

1. **Effect column** — a numeric crude producer calling `reg_fit()` with one predictor, the model's own
   `family` / `design_spec` / `conf_level` / `method` / `trials` / `multiplier`, joined onto the skeleton
   by the existing `reg_skel_match()`. Writes the same `obs` vector the factor producer writes, so
   `color = "adjustment"` starts working on numeric rows. Gaussian could use `cov/var` but should not
   (one rule).
2. **Base column** — stays `NA`; `mean (σ)` of X and `mean(X | Y)` go in the html tooltip through the
   existing `empirical_tips` mechanism.
3. **`multiplier = "sd"`** (optionally `"2sd"`) — one new accepted value; the SD computed **once** on the
   estimation/union frame and stored in `reg_meta`, never per split group or per compared model.
4. Implement `color = "adjustment"` significance policy (`gap_se` on the adjustment path) for numeric variables.
5. **The `levels` label** for a numeric row becomes the unit (`per 1 year`, `per 1 SD (13.5 years)`)
   instead of the redundant variable name. The `multiplier` relabel already does half of this, and it
   touches only `disp_levels`, so the skeleton key is unaffected.
6. **Store `reg_meta$predictor_types`** and unify the three factor-detection predicates.
7. **Docs**: the §12 sentence; the §8 weighting note; the §6 bridge between `tab()`'s standardised mean
   difference and `tab_reg()`'s crude OR, with the linearity warning.

Deliberately **not** in scope: the "Sample" distribution column (§4.3 option 5) — a good idea, but a
different feature; and any approximate OR in `tab()` (§6.4).

---

## 15. Open questions for round 3

- **Q7 — `effect = "ame"` / `"ame_ratio"`.** The crude counterpart of an AME for a numeric predictor is
  the univariable AME, at ~229 ms per predictor (§10). Compute it, or ship numeric crude counterparts on
  the coefficient path first and leave AME rows blank (with the `at = "reference"` precedent as the
  model for "no `obs` when it would not be comparable")?
  **Maintainer’s decision: compute it (AME are costly anyway). Would it be useful to use parallelisation for `tab_reg()` in the future, like it done in `tab()` ? If so, please add a Phase for it in @CLAUDE.md**
- **Q8 — `multiplier = "sd"` default.** Explicit opt-in only (recommended), or auto-apply when a numeric
  predictor's per-unit effect falls inside the first colour break? Auto would fix the readability
  problem without the user knowing the lever exists, but it makes the displayed quantity depend on the
  data — which is exactly what round 1 rejected for auto-standardisation.
  **Maintainer’s decision: 1 unit is not a good default working for any numeric variable, so let’s finally use `multiplier = "sd"` as default. By the way, is multiplier vectorised on the number of numeric variables to ensure a different multiplier can be chosen for each numeric variable (of better, with a named character vector) ?**
- **Q9 — `"2sd"`.** Worth offering Gelman's divide-by-2-SD (which makes a continuous predictor's
  coefficient directly comparable to a binary one: measured 0.431 / 1.486 against factor contrasts
  spanning 0.66–2.23), or is one standardisation enough?
  **Maintainer’s decision: yes, useful since it reads as "what difference it makes to go from bottom to top of the 95% distrib". But not the default (less readable)**
- **Q10 — the blank base cell, revisited after the fact.** If, once §5 lands, the numeric rows still read
  as unfinished, the upgrade is the uniform "Sample" column (§4.3 option 5), not a dual-meaning
  `Obs_%`. Agreed as the escalation path?
  **Maintainer’s decision: no sample column needed.**
