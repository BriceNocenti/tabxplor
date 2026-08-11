# Model checks for `tab_reg()` — design

Date: 2026-08-10, reorganised 2026-08-11 after two research rounds. Status: **DESIGN COMPLETE, no R
code written.** Answers Last Phase z12; records twelve maintainer rulings (§3).

**Read Part I alone if you want the framework.** Part II specifies each check and the API, Part III is
the implementation, Part IV is the evidence, the refusals and the rejected alternatives. Numbers marked
**[M]** were measured on this box; §29 says on what.

---

# PART I — THE FRAMEWORK

## 1. The one idea

tabxplor's headline feature is a comparison: `Model_OR` beside `Obs_OR`, coloured by the gap between
them, tested by `gap_se`. **Every model check in this design is that same comparison, applied to
something other than an effect.**

| what is compared                                 | Observed                          | Model                           | the gap                    |
|--------------------------------------------------|-----------------------------------|---------------------------------|----------------------------|
| the **shape** of a continuous predictor's effect | the binned curve of the data      | one straight line               | curvature test             |
| the **spread** of the outcome                    | the empirical (sandwich) variance | the variance the family assumes | SE ratio                   |
| the **meaning** of an ordinal effect             | one odds ratio per cut            | one cumulative odds ratio       | Brant test                 |
| the **weight** of one respondent                 | the estimate without them         | the estimate with them          | dfbetas                    |
| the **separability** of two predictors           | —                                 | —                               | VIF *(the exception, §11)* |

A user who has understood `empirical = TRUE` has therefore already understood the checks: *where does
the model disagree with the data, and does it matter?* One verb, one vocabulary, one legend voice — no
second mental model to learn.

## 2. Why it must exist

The model used throughout the regression vignette, `?tab_reg` and the jamovi screenshots is
mis-specified, and **no tabxplor output reveals it**. On `married ~ race + age + rincome + relig`
(n = 12 960), letting `age` curve instead of run straight **[M]**:

| printed row              | OR, age linear | OR, age quadratic |                        change |
|--------------------------|---------------:|------------------:|------------------------------:|
| income $25 000 or more   |          1.863 |             1.419 |                   **−23.8 %** |
| income $15 000 to 24 999 |          1.273 |             1.056 | −17.0 % (**p 0.0001 → 0.40**) |
| income $10 000 to 14 999 |          1.140 |             1.021 |                       −10.4 % |
| race Black               |          0.416 |             0.398 |                        −4.3 % |

One income level's conclusion **flips at the 5 % threshold**, and the two models tell opposite stories
about age itself (P(married) at 85: **0.686** linear vs **0.219** quadratic **[M]**). ΔAIC = 296,
LRT p = 9.9e-67 **[M]**.

**The damage is not confined to the mis-specified row** — that is the whole argument. z5/z9 give that
row a crude twin and z8-B tests the gap between them; neither can say *"one slope is the wrong summary,
and it is bending the income effect you came here to read."*

(A second, smaller reason: the documented `lm_plots()` example **errors** in both the EN and FR
vignettes — a `tabxplor_tab` *is* a data frame, so it takes the data-frame branch and demands
`dependent`/`predictors` **[M]**.)

## 3. Maintainer rulings

| #       | Question                 | Ruling                                                                                                         |
|---------|--------------------------|----------------------------------------------------------------------------------------------------------------|
| **R1**  | Entry point              | Table + data, **and** a bare fit as a secondary form. ONE engine underneath.                                   |
| **R2**  | Two coordinated surfaces | Plots carry verdicts as subtitles **and** the table carries a check block, from the same fact table.           |
| **R3**  | `or_plot()`              | Keep the name, share the internals (theme / guard / i18n seam).                                                |
| **R4**  | Dependencies             | **No new Suggests except `car`.** `ggplot2` + `gridExtra` are already Suggests.                                |
| **R5**  | The in-table miniature   | **Both renderings, one stored curve**: a Unicode text sparkline everywhere, an inline `<svg>` upgrade in html. |
| **R6**  | Non-linear fits          | **`shape = c(age = "quadratic")`** — a named vector, the `reference =` / `multiplier =` idiom.                 |
| **R7**  | When checks are computed | **Always.** No opt-in gate.                                                                                    |
| **R8**  | A curved predictor's row | **The raw term rows** (`age`, `age²`), each with its own estimate and crude twin.                              |
| **R9**  | The core check set       | **Five: Linearity, Proportionality, Dispersion, Influence, Collinearity.**                                    |
| **R10** | The plot function        | **Teaching only, faceted across models.** The decision path never requires it.                                 |
| **R11** | Footer form              | **One row per check** in the GOF block, ordered most-important first.                                          |
| **R12** | Zero inflation           | **No zero-inflated models, and no zero-count check.**                                                          |

## 4. The five checks

One noun each — four of the five are `-ité` in French too, which is worth something for a class:

| # | check                                | the question, in one sentence                        | statistic                               | families            |
|---|--------------------------------------|------------------------------------------------------|-----------------------------------------|---------------------|
| 1 | **Linearity**                        | Is this predictor's effect really one straight line? | curvature test p, per numeric predictor | all                 |
| 2 | **Proportionality**                  | Is one odds ratio enough for every cut?              | Brant p                                 | ordinal             |
| 3 | **Dispersion** *(robust/model SE)*   | Are the standard errors wide enough?                 | max robust/model SE                     | all                 |
| 4 | **Influence** *(max dfbetas)*        | Does one respondent carry the result?                | max \|dfbetas\|                         | all                 |
| 5 | **Collinearity** *(max VIF)*         | Can the data tell these predictors apart?            | max GVIF                                | all but multinomial |

The order is the order they print, and it is the order of what each one threatens: **the estimate (1),
what the estimate means (2), its interval (3), whether it is real at all (4), why it is wide (5).**

**Every noun is a word the reader already knows, and the parenthesis names the instrument** — the
convention Last Phase m set for the crosstab summary (`"pvalue (Chi2, Welch F; Kish)"`). Nothing here
is a coined term: *dispersion* is what a Poisson user says, *dfbetas* is Belsley–Kuh–Welsch and lives
in base R as `stats::dfbetas()`, *VIF* needs no gloss.

Two of the five are new *ideas* rather than new implementations, and both are unifications:

- **Dispersion, measured on the SEs, replaces four textbook checks with one number.**
  `max |SE_robust / SE_model|` measured **1.43 where √φ = 1.40** under Poisson overdispersion, **3.91**
  under clustering, a per-coefficient deviation of **0.36** under heteroscedasticity, and
  **1.00 ± 0.01** on three correctly-specified replicates **[M]**. It is **orthogonal to Linearity**
  (under a mis-specified mean it reads 0.992 — it does not double-count **[M]**), and it never touches
  `df.residual`, so the `df.residual(svyglm) = design df` trap that forced dispersion to be **refused**
  on clustered fits (φ read 22.49 instead of 1.00 **[M]**) simply does not arise — one refusal leaves
  the design. §9 states exactly how close it is to φ, and where it deliberately differs.
- **Influence is reported as a reassurance, not an alarm.** Cook's D is unreadable at survey n
  (max 0.0009 at n = 12 990 **[M]**, and its conventional cutoff of 1 fires at no n we measured), so
  the row uses **dfbetas** — the same question, one coefficient at a time, on a scale that reads at
  any n: *"no single respondent moves any coefficient by more than 0.19 SE"* **[M]**. That is a
  sentence a reviewer wants, and it comes from `reg-influence.R`, which the package already owns and
  which is the **only** design-aware influence in the ecosystem (§18.1).

## 5. The three surfaces — each check appears once

The 2026-08-11 simplification: a check is **not repeated across surfaces**. It appears at the first
surface that can answer it reliably, and only there. The teaching plots (surface 3) may repeat anything,
because repetition is what teaching is.

Linearity looks like it breaks that rule by appearing on surfaces 1 and 2. It does not, and the reason
is §1: **the sparkline is the Observed side, the footer p is the gap.** Exactly as a numeric predictor's
row shows `Obs_OR` (a quantity you read) beside a gap the colour and the test judge, the sparkline shows
*the observed shape* and the footer row tests *the model's straight line against it*. One check, the
framework's own two parts — not one check twice. §7.1 states what each part is measured on, because
they are measured on different things.

| surface                          | what it is                                                        | who sees it                                  | which checks   |
|----------------------------------|-------------------------------------------------------------------|----------------------------------------------|----------------|
| **1. A footer row**              | one number + a threshold, in the GOF block                        | every export, always, for every model column | all five       |
| **2. The row's own sparkline**   | 10 bins of the observed curve, inside the predictor's level label | every export (text) + html (svg)             | Linearity only |
| **3. `reg_assumptions_plots()`** | the classic panels, faceted across models                         | on request, for teaching                     | any, freely    |

**Nothing on surfaces 1 and 2 needs the data or a refit.** Surface 1 is a function of the fit (computed
in the build, stored in the `test` attribute); surface 2 is a function of the raw columns (computed in
the build, stored in `meta`). So a table that has travelled to Excel still carries its verdicts, and a
user who never calls the plot function is never uninformed. That is the answer to *"is refitting the
only way?"* — **no, and the surfaces that matter never refit.**

The intermediate tier round 1 proposed — decision-grade point-cloud panels — is **deleted** (§14).

## 6. What the user does

Nothing, to be warned:

```text
  N                              12 960
  LR vs null                     <0.01%
  McFadden R2                     0.045
  AIC                            16 960
  Linearity: age                 <0.01% ***
  Linearity: tvhours              0.08% ***
  Dispersion (robust/model SE)     1.03
  Influence (max dfbetas)          0.19
  Collinearity (max VIF)           1.21
```

and the `age` row itself carries the shape:

```text
  var    levels                          n    Model_OR    Obs_OR
  age    age (per 1 SD) ▁▄▇▇█████▇  12 960     1.35***    1.46***
```

Something, to fix it:

```r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        shape = c(age = "quadratic"))     # or "cut5" -> five age groups, one OR each
```

Something else, to teach it:

```r
reg_assumptions_plots(t, gss_simple)      # the textbook panels, faceted across models
```

---

# PART II — THE CHECKS AND THE API

## 7. Check 1 — Linearity

### 7.1 Two parts, measured on different things — and that is the point

| part | what it is measured on | what it answers |
|---|---|---|
| the **sparkline** (surface 2) | the **raw** data: 10 weighted quantile bins of y against x, on the family's link scale. No fit. | *What shape does the relationship actually have?* |
| the **footer p** (surface 1) | the **fitted model**: the same model plus this predictor's centred squared term | *Is the straight line the model drew good enough, the other predictors being held equal?* |

They are the **Observed** and the **gap** of §1, not the same number twice. And they can disagree — the
disagreement is informative, exactly as a `Model_OR` far from its `Obs_OR` is:

| case | crude sparkline | adjusted (partial-residual) curve | correlation |
|---|---|---|---|
| the vignette's `age` (mild confounding) | `▁▄▇▇█████▇` | `▁▄▇██████▇` | **0.997** **[M]** |
| a constructed strong confounder, cor(x, z) = 0.94 | `▁▃▄▆████▇▆` | `▁▃▅▇███▇▅▂` | 0.842 **[M]** |
| the adversarial case (same, reversed) | `▁▃▅▆▆████▆` | `▇▇███████▁` | **0.191** **[M]** |

So the sparkline is labelled and documented as the **observed** shape, never as "the model's shape" —
the same honesty `Obs_*` columns already carry. When the two parts disagree, adjustment has changed the
shape, which is the same lesson `color = "adjustment"` teaches for effects, and the teaching panel
(surface 3) is where both curves can be drawn together.

⚠ **A collinearity caveat that belongs in the docs.** In the constructed case above, `x` was *truly*
linear yet its curvature test still returned p = 9.8e-122 **[M]** — with cor(x, z) = 0.94, `x²` partly
proxies the curved `z`. A curvature test on one predictor can pick up another's mis-specification when
the two are near-collinear. That is one more reason check 5 sits in the same block.

### 7.2 The statistic

One augmented refit per numeric predictor (the model plus its centred squared term)
and the existing `reg_term_tests()` dispatcher: `drop1()` unweighted, `survey::regTermTest()` on a
design, F for gaussian and quasipoisson. This is `car::residualPlots()`'s curvature test, it is
design-correct for free, and **it is not a new mechanism** — z8 already added `reg_fit(cross =)` for the
pooled interaction fit, and this is the same seam with `add_terms =`.

Cost, per numeric predictor: glm **17 ms**, svyglm 81, polr 110, multinom 277 **[M]**.

⚠ **Not a score test.** The no-refit Rao score test is 4× cheaper and agrees unweighted, but returns the
**identical p on a weights-only and on a stratified+clustered design** where the design-based Wald
differs by thirty orders of magnitude **[M]**. Rejected.

⚠ **Not Box–Tidwell**, although that is the test this audience was taught (Hosmer & Lemeshow, Menard,
the SPSS workflow). It needs `log(x)`, hence `x > 0`, which a centred or negative predictor is not, and
it does not generalise across families. Name it in the vignette, do not compute it.

**The sparkline** (surface 2) shows the fit-free empirical curve on the family's own link scale: 10
weighted quantile bins, min–max rescaled *within the predictor*, so it answers "is it a line?" and never
"is the effect big?". Measured legible: a straight line reads `▁▂▃▃▄▅▆▆▇█`, the real `age` curve
`▁▄▇▇█████▇`, an inverted U `▁▄▆▇██▇▆▄▁`, noise `▃▁▄▁▅█▂▅▅▃` **[M]**. The fit-free curve is the right
one: it correlates **0.997** with the partial-residual curve **[M]** and, unlike it, survives the jamovi
digest path where no fit exists.

## 8. Check 2 — Proportionality (ordinal)

Brant's test, already computed at fit time and stashed on the fit (`reg_fit_ordinal`), already a footer
row (`brant_po`). Nothing to build; it moves into the check block and gets its noun.

⚠ It rejects at survey n on departures the eye calls mild — p = 0.00089 for a spread of 0.05 logits
across cuts **[M]**. So the row must be read beside the parallel-lines lesson (surface 3), and the
vignette must say so. Weighted ordinal (`svyolr`) has no Brant fit: the row is **absent, not wrong**.

## 9. Check 3 — Dispersion *(robust/model SE)*

**The statistic**: `max_j |SE_robust,j / SE_model,j|`, reported as a ratio. The robust leg is the
sandwich the package already computes in `reg-influence.R` (`reg_if_from_parts` for lm/glm/svyglm;
`reg_if_from_score` + `reg_score_polr`/`reg_score_multinom` for the 3+-level families); on a design it
is `svyrecvar`, so the number *is* the design's effect on the SEs.

**The name.** *Dispersion* is the word a Poisson user already has, and the parenthesis says which
instrument produced the number — the Phase-m convention. The alternatives considered and dropped:
`SE reliability` (accurate, but a coined term nobody can look up), `Misspecification effect (meff)`
(Kish's exact term for this ratio — correct and citable, but jargon for the intended reader),
`Robust SE ratio` (precise, and reads as a method rather than as a question).

### 9.1 How close is it to the Pearson φ? Measured, and deliberately not identical

The honest answer to *"does it give exactly the dispersion coefficient?"* is **no — it is within a few
per cent, and where it differs it is the more useful of the two** **[M]** (negative-binomial counts,
n = 4 000, one Poisson fit per row):

| true θ | Pearson φ | √φ | max ratio | ratio / √φ |
|---:|---:|---:|---:|---:|
| ∞ (true Poisson) | 1.02 | 1.012 | 1.027 | 1.016 |
| 20 | 1.16 | 1.075 | 1.091 | 1.015 |
| 5 | 1.71 | 1.306 | 1.357 | 1.039 |
| 2 | 2.72 | 1.650 | 1.774 | 1.076 |
| 1 | 4.35 | 2.086 | 2.227 | 1.067 |
| 0.5 | 7.65 | 2.766 | 2.985 | 1.079 |

Grouped binomial (a summed score, m = 20, beta-binomial intra-cluster ρ) is closer still: ratio/√φ =
**0.997 / 1.018 / 1.023** for ρ = 0 / 0.02 / 0.08 **[M]**.

So: **the ratio ≈ √φ to within 1.5–8 %**, always slightly above it. Three reasons the small gap is a
feature, not an error:

1. **φ assumes one constant dispersion; the ratio does not.** On one negative-binomial fit the
   per-coefficient ratios were 1.529 / 1.605 / 1.663 / 1.639 against a single √φ = 1.588 **[M]** — the
   sandwich sees that the inflation is not the same for every coefficient, which is the thing φ cannot
   express.
2. **It answers the question the reader has.** φ describes *the outcome's variance*; the ratio
   describes *the standard errors actually printed in this table*. They diverge exactly where it
   matters: with `family = "quasipoisson"`, **φ stays 2.62 while the ratio drops to 1.03** **[M]** —
   the dispersion is unchanged, but the intervals have been fixed, and the row correctly says so.
3. **It has no `df.residual` in it**, so it is computable and honest on a clustered design, where φ
   reads 22.49 instead of 1.00 **[M]** and has to be refused.

**It replaces the existing `dispersion` footer row.** For a count model, φ itself remains one
`summary(fit)$dispersion` away and is named in the vignette, because that is the number a paper
reports. (§28.9 records the one-line alternative: an extra exact-φ row for count families only.)

### 9.2 One row, four honest readings

Stated in the vignette and in the tooltip:

| context                                         | ratio ≈ 1 means                        | ratio > 1 means                                  | what to do                                        |
|-------------------------------------------------|----------------------------------------|--------------------------------------------------|---------------------------------------------------|
| unweighted glm / lm                             | the family's variance assumption holds | it does not (overdispersion, heteroscedasticity) | `family = "quasipoisson"`, or `wt =` / a design   |
| count model                                     | φ ≈ 1                                  | φ ≈ ratio² (to within a few %)                   | `family = "quasipoisson"` — same IRR, honest SEs  |
| a survey design                                 | the design changed little              | the design mattered by that factor               | nothing: the printed SEs are already the design's |
| `family = "poisson"` on a binary outcome (`rr`) | —                                      | expected by construction                         | nothing: the sandwich *is* the estimator (z3)     |

## 10. Check 4 — Influence *(max dfbetas)*

**The statistic is `dfbetas`** — the standardized change in a coefficient when one observation is
dropped, in units of that coefficient's own SE (Belsley, Kuh & Welsch; base R has `stats::dfbetas()`).
The row prints `max_j max_i |dfbetas_ij|`: *"no single respondent moves any coefficient by more than
0.19 SE"* **[M]**. Naming it after the real statistic matters — a reader who wants to check what it is
can look it up, which "influence index" would not allow.

**It is the Cook's-distance question, one coefficient at a time.** Measured Spearman rank correlation
between `max_i |dfbetas|` and `cooks.distance()`, same model at three sizes: **0.804** (n = 120),
**0.941** (n = 1 000), **0.944** (n = 12 990) **[M]** — they rank the same observations. What differs is
readability, and that is why the row uses dfbetas:

| n | max Cook's D | obs over the conventional 1 | max \|dfbetas\| | obs over 1 | obs over 2/√n |
|---:|---:|---:|---:|---:|---:|
| 120 | 0.1163 | 0 | 0.640 | 0 | 24 |
| 1 000 | 0.0078 | 0 | 0.166 | 0 | 249 |
| 12 990 | 0.0009 | 0 | 0.049 | 0 | 2 630 |

Cook's D and its cutoff of 1 fire at **no** sample size here, and the n-dependent `2/√n` rule flags 20 %
of a large sample — neither is usable as a printed verdict. `|dfbetas| > 1` ("this respondent moves the
coefficient by a full standard error") is scale-free and reads at any n. Threshold: flag above **0.25**;
print the value always, because the reassurance is the point.

⚠ **Two precisions the docs must carry**, because both are easy to get wrong:

- **Influence is not outlyingness.** A respondent with a surprising `y` but an ordinary covariate profile
  moves nothing; a respondent with an extreme profile and a modest residual can move a lot. Influence =
  leverage × outlyingness. If a reader wants *outliers*, that is the Q-Q / residual panel (surface 3),
  not this row.
- **It rarely fires at survey n, and that is the finding.** With 13 000 respondents no single one should
  carry a coefficient; a near-zero value is *information*, not a missing check. It is the small-sample
  case — where Cook's distance is the usual tool — that this row exists to catch.

The one-step influence tabxplor already computes matches `stats::dfbetas()` at correlation **0.99999**,
max relative discrepancy **1.3 %** **[M]**, and unlike base R it works for `polr`/`multinom` (no method
exists there **[M]**) and is design-aware (§18.1). Cost 11.8 ms **[M]**.

## 11. Check 5 — Collinearity

`max` of `car::vif()`'s GVIF^(1/2·df), 3.6 ms **[M]**. **The one check that is not a model-vs-observed
comparison**: it is a property of the design matrix, it biases nothing, and a wide confidence interval
already shows what it shows. It is in the core because it is what jamovi's own *Assumption Checks* pane
puts first and what every social-science textbook teaches, so its absence would read as an omission.
State that reasoning in the vignette rather than pretending it is decision-critical.

Refused for multinomial: `car::vif()` warns *"No intercept: vifs may not be sensible"* (easystats #907),
and the hand-rolled `det(R₁₁)det(R₂₂)/det(R)` alternative returns 11.45 where `car` returns 1.01 on
`polr` **[M]** — it is not a drop-in.

## 12. `shape =` — fitting a predictor as something other than a line

The checks find a non-linearity; `shape =` is how the user fixes it without leaving the framework.
Today they cannot: `predictors = c("race", "poly(age, 2)")` errors **[M]**, and the formula escape hatch
silently disables `empirical =`, `color = "adjustment"`, `multiplier` and the per-predictor tests.

### 12.1 The grammar

```r
tab_reg(gss_simple, "married", c("race", "age", "rincome"),
        shape = c(age = "quadratic"))     # named over numeric predictors; the rest stay linear
```

| value                 | emitted terms                       | rows | why it is in the set                                  |
|-----------------------|-------------------------------------|------|-------------------------------------------------------|
| `"linear"` (default)  | `z`                                 | 1    | today's behaviour, byte-identical                     |
| `"quadratic"`         | `z + I(z^2)`                        | 2    | the standard remedy, the one every source names       |
| `"log"`               | `z` of `log(x)`                     | 1    | diminishing returns — the other shape social data has |
| `"sqrt"`              | `z` of `sqrt(x)`                    | 1    | the count-data cousin of `log`                        |
| `"cut5"` / an integer | a k-level factor of quantile groups | k    | **the sociologist's remedy**, §12.4                   |

`z = (x − x̄)/s`, with **x̄ and s frozen as literals in the formula** at build time — exactly as z9
freezes the `multiplier`, and for the same reason (`scale()` inside a formula re-scales on new data, so
`predict(newdata =)` disagrees with the fit **[M]**). For `"log"`/`"sqrt"` it is the transformed column
that is centred, so `multiplier = "sd"` keeps meaning "per 1 SD of the term as fitted" for every shape.

### 12.2 Why it integrates with nothing new

`shape` changes *which terms a predictor emits*, and nothing else. **The predictor stays one
predictor**, which is the property every downstream subsystem keys on:

- **the skeleton** — already emits one row per term for a factor; a curved numeric is the same shape.
- **the crude twin (z9)** — `reg_empirical_fit()` refits one predictor with the model's own family,
  design, CI method and multiplier; give it the same shape and its **term names are identical to the
  model's** **[M]**, so `reg_skel_match()` aligns them unchanged.
- **`color = "adjustment"` + the z8-B gap test** — per term, both legs on the same scale, same rows.
  Measured: model OR 1.577 vs crude 1.638 for the slope at the mean **[M]**.
- **`effect = "ame"`** — one row per predictor, exact to the g-computation truth **[M]**.
- **`multiplier`** — absorbed by the centring; both terms are already per-SD.
- **jamovi** — one dropdown per numeric predictor beside the existing scaling one. A shape change is a
  *different model*, hence a different cache key: the digest/reref contract is untouched.

⚠ **The one 1-to-1 that breaks, and it must be written as a rule, not an `if`:** the skeleton emits
**one row per model TERM on the coefficient path, one row per PREDICTOR on the marginal path.** Today
those coincide. A curved predictor is the first case where they differ (two coefficient rows, one AME
row), so `reg_skeleton()` must see the effect, and `reg_marginal_column()`'s key must find `age`, not
`age (per 1 SD, at the mean)`.

### 12.3 What the table shows (R8), and why centring is what makes it readable

```text
  var    levels                                    Model_OR    Obs_OR
  age    age (per 1 SD, at the mean) ▁▄▇▇█████▇       1.58***   1.64***
  age    age² (curvature)                             0.75***   0.73***
```

Both rows are readable **because the emitted variable is centred and scaled**: row 1 is the odds ratio
per 1 SD *at the mean of age*, row 2 says the slope flattens as you move away from it (`< 1` flattens or
turns over, `> 1` accelerates). Uncentred, the same model prints 1.184 and 0.99841 **[M]** — a number
nobody can read and a number that looks like nothing.

Centring is not merely nicer: it takes the collinearity from **VIF 38.7 to 1.2** **[M]**. Without it,
check 5 would flag every curved model as broken. The vertex (age 52.2 **[M]**) belongs in a panel
subtitle, not in a cell.

### 12.4 `cut` — the remedy this package renders best

Turning `age` into 5 quantile groups makes it a **factor**, so it inherits the entire factor machinery:
one OR per group, a *saturated* crude twin (the exact observed contrast, not a univariable fit),
per-level N, per-level colours, adjustment gaps, Woolf intervals — and the non-linearity becomes visible
**in the printed numbers themselves**, with no new estimand and no `marginaleffects` involvement. For a
literary-studies student reading an OR table this is the more teachable fix. **Teach it first**, with
`"quadratic"` as the parsimonious alternative.

### 12.5 What is excluded — and one exclusion is a wrong number, not a preference

- **`poly()` and `ns()`/`bs()` are never emitted.** `marginaleffects 0.32.0` returns **AME = 0.000000**
  for them, silently, through every contrast form, where `predict()` gives the correct +0.038 **[M]**.
  Root cause, reproduced in 12 lines: the basis is **re-evaluated on the perturbed data**, and an
  orthogonal basis absorbs a location shift exactly (max |ΔX| = 0.005, AME 0.002953 → 0.000000 **[M]**).
  `I(x^2)`, raw polynomials and `log()` are correct through every route **[M]**. Splines therefore stay
  in the formula escape hatch only, with the guard below.
- **Cubic and higher** — three rows nobody can read; the honest answer there is `cut` or another tool.
- **Arbitrary expressions in `predictors`** — every site that keys on a predictor *name* would have to
  parse and re-emit them, and the name the user reads becomes an expression.

### 12.6 The formula escape hatch — keep, fix two things

It stays (interactions have no other route). Two measured defects to fix in the same session:

1. **The refusal message blames the wrong thing.** `tab_reg(gss, married ~ race + poly(age,2),
   empirical = TRUE)` says *"`empirical` … is not available for any of these outcome families"* on a
   **binomial** model **[M]**. The cause is the compound formula, not the family (`tab_reg.R:5045`).
2. **`poly()`/`ns()` + `effect = "ame"` must warn.** Today it is a coin flip: the same model returns the
   correct AME through tabxplor and 0 through a direct `marginaleffects` call **[M]**, decided by
   whether `insight` can recover the data. Guard exactly: compare against
   `mean(predict(x + k)) − mean(predict(x))`, two lines, refuse on disagreement.

## 13. `reg_assumptions_plots()` — the teaching function

```r
reg_assumptions_plots(
  x,                     # a tab_reg() table, OR a fitted model (secondary form)
  data       = NULL,     # the data frame or survey.design the table was built from
  check      = "auto",   # "auto" | the check nouns | "all"
  predictors = NULL,     # which numeric predictors get a linearity facet (default: all)
  ncol = NULL, theme = NULL, lang = NULL, max_points = 2000, seed = 20260810, ...
)
```

**It is pedagogical, and the documentation says so in the first sentence.** Every decision-grade number
is already in the table, for every model column; this function exists to *show a class what a violation
looks like*, and to let a careful user look closer. Nothing in the workflow requires calling it.

**Faceted across models (R10).** One call diagnoses every model / outcome / split group in the table.
When that is a wall, the user passes fewer models — a wall is a legible failure mode, a silent "first
model only" is not.

**The panel set, per family.** Generous, because it costs nothing at build time:

| family           | panels                                                                                               |
|------------------|------------------------------------------------------------------------------------------------------|
| gaussian         | residuals vs fitted · Q-Q · scale–location · residuals vs leverage (LINE, the four `plot.lm` panels) |
| binomial, `rr`   | empirical logit per predictor · binned residuals · calibration · Q-Q of quantile residuals           |
| poisson          | log empirical means per predictor · mean–variance · observed vs expected counts                      |
| grouped binomial | as binomial, plus the deviance GOF (valid only when mᵢ is large)                                     |
| ordinal          | parallel lines (empirical cumulative logits, one line per cut) · Q-Q of quantile residuals           |
| multinomial      | calibration per category · baseline-category empirical logits — **no residual panel** (§18.2)        |
| all              | influence (‖IF‖, design-aware)                                                                       |

**Facets within a check, grid across checks**: the per-predictor linearity panels are homogeneous, so
they are one `ggplot` + `facet_wrap()`; heterogeneous panels go in a `gridExtra::arrangeGrob()` grid
(already a Suggest, already what `or_plot()` returns, and it returns a `gtable`, so the existing test
idiom carries over).

### 13.1 How it gets a fit

`reg_meta` gains **one field, `fit_spec`** (4.3 KB against a 23.9 KB table **[M]**): the `specs` list,
the design *names*, `method`, `conf_level`, `na`, `inverse_two_level_factors`. The function then calls
**`reg_fit()` itself** — the same fitter the table came from — so there is no second fitting path to
keep in sync. Refit cost **60 ms** **[M]**, and it is now a *teaching* cost, which changes nothing.

**A guard is required, not optional**: if `nrow(reg_complete_frame(data, drop_vars))` differs from the
fit's stored `nobs`, abort naming the discrepancy. A diagnostic plot of the wrong model is worse than no
plot.

### 13.2 The secondary form

A bare `lm`/`glm`/`svyglm`/`polr`/`multinom`/`svyolr` as `x` is diagnosed directly — ~10 lines, because
both forms reduce to the same internal quadruple `(fit, frame, family, weights)`.

---

# PART III — IMPLEMENTATION

## 14. What round 1 designed and this reorganisation deleted

Round 1 proposed eleven checks on four rungs. Five checks on three surfaces replace them. Each deletion
with the measurement that justifies it:

| deleted                                      | measured reason                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|----------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **calibration** (as a check)                 | A GLM is calibrated **exactly within every level of an included factor** by the score equations — measured gap `0.000000` for all three `race` levels, and `mean(fitted) − mean(y) = 3e-14` **[M]**. In-sample calibration can therefore only fail through continuous functional form, i.e. it *is* Linearity, re-tested — and insensitively: the max decile gap barely moved when the mis-specification was fixed (0.0477 → 0.0448 **[M]**). Kept as a teaching panel. |
| **binned residuals vs fitted** (as a check)  | Did not discriminate: 45 % of bins outside the ±2 SE band for the mis-specified model vs 40 % for the corrected one, while the per-predictor test separated them at p = 9.9e-67 **[M]**. Kept as a teaching panel — it is the classic lesson about why raw residuals are useless for a binary outcome.                                                                                                                                                                  |
| **zero counts** (R12)                        | On `tvhours`, φ = 2.04 (overdispersed) while zeros are **under**-represented, 277 observed vs 299 expected **[M]** — it disagrees in direction with the dispersion it is meant to explain, and it has no separate remedy.                                                                                                                                                                                                                                               |
| **zero-inflated models** (R12)               | A ZIP/hurdle count coefficient is conditional on the at-risk class, so it is **not comparable to the crude rate ratio `Obs_IRR` prints beside it** — it would silently break the model-vs-observed comparison the package is built on. `family = "quasipoisson"` already fixes the inference whatever the mechanism.                                                                                                                                                    |
| **Q-Q / normality** (as a check)             | Irrelevant to coefficient inference at survey n (CLT), and every formal test rejects — DHARMa's own vignette says so, and `performance`'s replacement is a KS test with the same pathology. Kept as the canonical teaching panel.                                                                                                                                                                                                                                       |
| **separation, predicted range, convergence** | Not assumptions but **error conditions**: they belong to `cli` warnings at build time, where the user can act, not to a footer row that is empty 99.9 % of the time.                                                                                                                                                                                                                                                                                                    |
| **the whole intermediate rung**              | With the above removed, nothing was left on it.                                                                                                                                                                                                                                                                                                                                                                                                                         |
| **the `dispersion` footer row**              | Superseded by Dispersion measured on the SEs (§9), which is family-general and design-correct, and which makes one refusal unnecessary. Measured: it tracks √φ to within 1.5-8 % **[M]**, and it correctly returns to ~1 once `quasipoisson` has fixed the intervals, where φ itself stays 2.62 **[M]**.                                                                                                                                                                                                                                                                                                                                  |

## 15. Where each thing computes and lives

Three stores, all of which already exist.

| what                 | store                    | shape                                                                       | size                                                                                |
|----------------------|--------------------------|-----------------------------------------------------------------------------|-------------------------------------------------------------------------------------|
| the five verdicts    | the **`test`** attribute | the existing `new_test_tibble()` schema, one row per (model column × check) | ~5 rows                                                                             |
| the sparkline curves | **`meta$assumptions`**   | `list(<var> = tibble(x, y, n, se))`                                         | **1.6 KB per curve**; 4 numerics × 3 models = 19.5 KB against a 32 KB table **[M]** |
| the teaching recipe  | **`meta$fit_spec`**      | strings + a formula                                                         | **4.3 KB** **[M]**                                                                  |

Two rules:

- **The curve is computed once per numeric predictor** — never per model, never per rendering. It is the
  *fit-free* curve, so it does not depend on the model at all: a 5-model comparison stores five
  references to one 1.6 KB tibble. The verdicts *do* depend on the model and are per column, as every
  `test` row already is.
- **The jamovi digest path** (which deliberately keeps no model frame) computes the curves — they need
  only the raw columns — but not the verdicts, which read the fit. One more `needs` value (`"fit"`),
  degrading to sparklines plus a note: the better half, for a live UI.

## 16. The footer rows (R11)

**This is not a new mechanism.** `dispersion` and `brant_po` are already `reg_footer_spec()` rows
produced by `reg_glance()` (`tab_reg.R:2744-2753`, `tab-test-display.R:185-208`). The block is more rows
in an existing spec.

```text
  Linearity: age                 <0.01% ***      (pvalue kind)
  Linearity: tvhours              0.08% ***
  Proportionality (Brant)          0.1% **       (ordinal only)
  Dispersion (robust/model SE)      1.03         (gof kind, 2 digits)
  Influence (max dfbetas)           0.19
  Collinearity (max VIF)            1.21
```

**One extension is needed and it is one line**: a row whose `row_var` is non-empty renders its label as
`paste0(spec$label, ": ", row_var)`. That is exactly what the *line* renderer already does with
`row_var`, so it is one rule applied to a second renderer, not a new concept — and it gives Linearity
its per-predictor rows without a discriminator per variable.

Cost of the whole block on the vignette's 4-predictor binomial: **~44 ms against a 380 ms build
(+12 %)** **[M]** — curvature refits 25 ms (two predictors), VIF 3.6, sandwich ~12, curves 3.0. For
scale, `effect = "ame"` on the same model costs **2 153 ms** **[M]**.

⚠ **The one cost worth deciding at implementation**: a multinomial with 3 numeric predictors pays
~830 ms for its Linearity rows **[M]**. R7 says always; the escape already exists (`stats = FALSE`), and
jamovi's live UI is where to judge it.

## 17. The miniature (R5)

**Where it goes.** A numeric predictor's `levels` cell is built at `tab_reg.R:3624-3639`, where the
multiplier already relabels it (`age` → `age (per 1 SD (13.5))`). The sparkline is appended **in that
same loop** — it is one more thing the row label says about a numeric predictor. Because it is plain
text in a plain character column, it reaches the console, markdown, html **and Excel** with no export
machinery: 10 characters, 30 bytes **[M]**.

**The html upgrade.** `render_kable_html()` swaps the glyph run for a **121-byte** inline
`<svg><polyline>` **[M]** built from the same stored bins, with `stroke="currentColor"` so z11's
light/dark/print themes carry it with no new colour rule. (For scale: `svglite` 1 084 B, a base64 PNG
843 B — and neither reaches the console or Excel.)

⚠ **Two traps, both real:**

- **The html engine escapes label cells.** `html_escape_br()` (`tab-render-html.R:131`) escapes
  everything and un-escapes exactly `<br>`; an `<svg>` would render as literal text. Fix it in **that
  same function** — one place decides what markup of ours survives escaping.
- **Fonts.** Block glyphs need a font that has them (skimr documents Windows failures and ships
  `fix_windows_histograms()`). Needs `options(tabxplor.spark = TRUE)` and an ASCII fallback. Excel is
  fine; pandoc→LaTeX is where to expect trouble.

**Honestly: this is a novelty.** MATLAB puts sparklines in Live Script table *headers*; `gt` and
`kableExtra` put them in dedicated *columns*; no statistical package puts one in a regression row label.

## 18. Weights, designs, and the refusals

### 18.1 Four measured rules

1. **Residuals are design-blind, and that is correct.** A weights-only design and a
   `strata = ~s, ids = ~psu, nest = TRUE` design give **identical residuals** (`all.equal` at 1e-10) and
   SEs of 0.001130 vs 0.001141 **[M]**. The design enters the *variance*, never the point estimates — so
   the teaching panels are design-invariant, and saying so in the docs prevents the reasonable-sounding
   bug report.
2. **The binning must be weighted.** Weighted and unweighted decile curves differ by up to **0.019** in
   probability **[M]**. For a survey package the weighted one is the estimand.
3. **Dispersion is where the design shows up**, and it is the only check that changes with it (§9).
4. **Influence is design-aware.** `reg_if_se(IF, fit$survey.design)` returns **0.0011408** against
   `sqrt(vcov(svyglm))` = **0.0011408**, where the IID version gives 0.0011299 **[M]**. A prebuilt
   `svyrep.design` needs `withReplicates`: it degrades to IID with a note, as z8-B already does.

### 18.2 What the design refuses to draw or compute

Each is a `needs` value in `REG_CHECKS`, and each is a measurement rather than caution:

| refusal                                    | reason                                                                                                                                                                                              |
|--------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Collinearity for multinomial               | `car::vif()` warns; the hand-rolled alternative is wrong for polr **[M]** (§11)                                                                                                                     |
| Any residual panel for multinomial         | order-dependent: refitting with the category order reversed gives residuals correlated **−0.705** **[M]**. A *test* would look stable (KS 0.0104 vs 0.0079) while every *plot* would be an artefact |
| Proportionality for `svyolr`               | no Brant fit exists — the row is absent, not approximated                                                                                                                                           |
| Deviance GOF on a Bernoulli fit            | the χ² approximation needs large mᵢ (BeyondMLR §6.5.6)                                                                                                                                              |
| A Linearity check for a factor predictor   | a factor has no functional form to mis-specify — `Obs_*` per level already *is* the saturated alternative, so a panel would be a second encoding of the same fact                                   |
| Emitting `poly()`/`ns()`                   | `marginaleffects` returns 0 silently **[M]** (§12.5)                                                                                                                                                |
| Everything on the `.fit_cache` digest path | no model frame — but the sparklines still compute (§15)                                                                                                                                             |

## 19. i18n and theming

`with_legend_lang(lang, function(lg) { ... })` around the **whole** label-building block, as
`reg_model_lines()` does. Literal `gettext()`/`gettextf()` only (potools "explicit" style),
`enc2utf8()` on every returned string, notation (OR, IRR, β, AME, φ, VIF) stays English while prose is
translated.

New msgids: the 5 nouns + per-family panel titles and captions + axis labels + verdict templates
≈ **35–40 strings**. Today's plot titles (`"Residuals vs Fitted"`, `"Normal Q-Q"`, `"Odds ratio (95 % CI,
log scale)"`, `"Ref."`) appear **nowhere** in `po/R-tabxplor.pot`; under R3, `or_plot()`'s labels join
the catalogue here.

⚠ The **footer** nouns are `gettext()`'d at render under the ambient locale, like every other
`reg_footer_spec()` label — not under the plot's `lang =`. Two mechanisms for two surfaces is the
existing state (Phase z2's glibc catalogue-caching limitation is why); do not "unify" them without
re-reading it.

**Theme.** A shared `reg_plot_theme(theme)` reading `tx_chrome_hex(theme)` — the `"light"`/`"dark"`/
`"print"` vocabulary z11 established — replacing the five hard-coded `"#c00000"` literals in today's
`tab_reg_plots.R`; `or_plot()` adopts it (R3). `theme = "print"` matters: a diagnostic panel is exactly
what ends up in a thesis appendix in greyscale, so the panels drop the accent hue and lean on line type.

## 20. Testing

Pinned against a reference, never a hand-written expectation:

| what                          | pinned against                                                                          |
|-------------------------------|-----------------------------------------------------------------------------------------|
| the weighted bins             | `stats::weighted.mean()` per bin, base R                                                |
| the quantile residual         | a direct `statmod`-style computation written in the test                                |
| the Q-Q band                  | a 999-replicate simulated envelope, to 2 decimals                                       |
| Influence                     | `stats::dfbetas()` (correlation 0.99999, max 1.3 % relative **[M]**)                                          |
| the design-based influence SE | `sqrt(vcov(svyglm))` (equal to 7 digits **[M]**)                                        |
| Collinearity                  | `car::vif()`                                                                            |
| Dispersion (the SE ratio)     | `sandwich::vcovHC(type = "HC0")` if available, else a hand-written sandwich in the test |

Fixtures that fail if a decision is quietly reverted:

| what                     | assertion                                                                                                                      |
|--------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| the `shape =` crude twin | the univariable fit's term names are **identical** to the model's **[M]**                                                      |
| the `shape =` AME        | equals `mean(predict(x + k)) − mean(predict(x))` to 1e-12 **[M]**                                                              |
| **the `poly()` refusal** | a `poly()` model returns `avg_comparisons` = 0 while the truth is non-zero **[M]**                                             |
| centring                 | `car::vif()` on the emitted terms is < 5 (38.7 uncentred **[M]**)                                                              |
| `shape = "linear"`       | byte-identical to a table built without `shape`                                                                                |
| the sparkline            | a straight-line and a saturating predictor give **different** glyph runs; `spark = FALSE` restores today's label byte-for-byte |
| the html `<svg>`         | present in the rendered html, not escaped into text                                                                            |
| Dispersion               | ≈ 1 on three correctly-specified replicates, ≈ √φ (within 8 %) on an overdispersed count, and ≈ 1 again under `quasipoisson` **[M]** |
| the multinomial refusals | no Collinearity row, no residual panel; plus the order-dependence property test (`abs(cor) < 0.9`)                             |
| the footer grain         | a comparison table has one Linearity row per (model column × numeric predictor)                                                |

Suite cost: the panels are ~1.5 s each at vignette scale, so the file uses n ≈ 2 000 and
`skip_on_cran()`, following the four heaviest files' precedent from z2.

## 21. Implementation order

Five steps, each shippable and verifiable alone; the two that move snapshots come last.

1. **The primitives, unwired.** `rd_bin()` (weighted quantile bins), `rd_resid()` (one randomised
   quantile residual, dispatched), `rd_qq()` (the analytic beta band) and the `REG_CHECKS` fact table,
   in a new `R/reg-assumptions.R`. Nothing calls them. *No user-visible change.*
2. **The five footer rows.** `reg_glance()` gains them, `reg_footer_spec()` gains their labels, plus the
   `row_var` label extension (§16), `reg_fit(add_terms =)`, and the `dispersion` row is replaced.
   **One conscious snapshot regen** (reg footers only — verify no crosstab snapshot moves and that
   `stats = FALSE` still yields nothing).
3. **The curves + the miniature.** `meta$assumptions`, the sparkline in the level label, the `<svg>`
   upgrade with the `html_escape_br()` extension, `options(tabxplor.spark)` and the ASCII fallback.
   **Second conscious regen**; fixture: `spark = FALSE` is byte-identical to today.
4. **`shape =`.** The resolver (frozen constants, closed vocabulary), the emission, the two skeleton
   rows, the crude twin, the `cut`→factor arm, and the two escape-hatch fixes (§12.6).
5. **`reg_assumptions_plots()` + removing `lm_plots()`.** The panels, the theme seam adopted by
   `or_plot()`, `meta$fit_spec` + the data guard, the ~40 msgids, the vignette section built on §2, and
   the nine `lm_plots` references (`R/tab_reg.R:3715`, `man/`, `_pkgdown.yml`, `NEWS.md`, both reg
   vignettes, `test-tab_reg-plots.R`, `CLAUDE.md`). `lm_plots()` has never been released, so this is a
   removal, not a deprecation.

**Verification, whole phase.** Full suite in both locales (the `LC_ALL=C.UTF-8` run matters: new msgids
and block glyphs are what it catches). Two conscious regens; **zero** crosstab churn — no `tab()` path
is touched. `dev/verify_golden_field_delta.R` is not needed: **no fmt field and no column attribute is
added.** The whole design is built from the 21 fields and 12 attributes that already exist.

---

# PART IV — EVIDENCE AND APPENDICES

## 22. The asset inventory

### 22.1 On the table

`reg_meta` carries 18 fields; it does **not** carry the data, the fits, the compound formula, `trials`,
`na` or `method`. No fit is ever attached to a returned table. The `.fit_cache` digest stores coef +
vcov + a reference-invariant glance and **deliberately discards the model frame** — Phase o measured
~10 MB per retained fit as the cause of the jamovi model-comparison freeze. This phase must not weaken
that, and does not: `fit_spec` is 4.3 KB **[M]**.

### 22.2 Diagnostic accessors, per class **[M]** (`—` = base R errors)

|                                    | `lm` | `glm` | `svyglm` | `polr`     | `multinom` | `svyolr`   |
|------------------------------------|------|-------|----------|------------|------------|------------|
| `residuals()`                      | ✓    | ✓     | ✓        | **NULL**   | matrix n×K | **NULL**   |
| `rstandard()` / `rstudent()`       | ✓    | ✓     | ✓        | —          | —          | —          |
| `hatvalues()` / `cooks.distance()` | ✓    | ✓     | ✓ ⚠      | —          | —          | —          |
| `fitted()`                         | ✓    | ✓     | ✓        | matrix n×K | matrix n×K | matrix n×K |
| `car::vif()`                       | ✓    | ✓     | ✓        | ✓          | **warns**  | ✓          |
| `broom::augment()`                 | ✓    | ✓     | ✓        | ✓ (4 cols) | —          | —          |

⚠ `svyglm`'s hat values sum to p and correlate 0.45 with the weight **[M]** — they fold the *working*
weights in but know nothing of strata or clusters.

**The universal substrate is `fitted()` + `model.frame()` + `model.matrix()` + the observed y**, plus
`reg-influence.R`. Nothing else is portable across the six classes.

### 22.3 Why no existing package covers this

Measured on a fresh install of `performance 0.17.1` / `DHARMa 0.5.0` / `see 0.14.1`, n = 400:

| model             | panels `check_model()` returns                                                               |
|-------------------|----------------------------------------------------------------------------------------------|
| `glm` binomial    | 7                                                                                            |
| `svyglm` gaussian | 8 — **design ignored**                                                                       |
| `svyglm` binomial | 4 — Q-Q silently dropped                                                                     |
| `MASS::polr`      | **2**                                                                                        |
| `nnet::multinom`  | **2** (its VIF is [known-broken, #907](https://github.com/easystats/performance/issues/907)) |
| `ordinal::clm`    | **1**                                                                                        |

The disqualifying result: *the same call returns byte-identical homogeneity and Cook's-distance panels
for a weights-only design and for a `strata = ~s, ids = ~c` design.* `insight::model_info()` reports the
clustered fit as plain `is_linear, gaussian`. `DHARMa` has never supported `svyglm`
([#321](https://github.com/florianhartig/DHARMa/issues/321), open since 2022), and a survey-weighted
binomial cannot `simulate()` at all. Rendering cost: **~29 s for one panel at n = 1e5**, with a silent
2 000-point subsample.

This is not a criticism of `performance` — it is a well-built package for the classes it targets. It is
an argument that **tabxplor's model set is outside that target**.

## 23. The primitives

### 23.1 `rd_bin(x, y, w, nbins)` — weighted quantile bins

Base R, `findInterval` + `rowsum`, O(n), one pass; returns `x`, `y`, `n` (Σw), `var`, `se`. **19 ms
against 370 ms for a loess at n = 6 803 and 2 030 ms at n = 21 483** **[M]**; weight-aware for free; and
for a binary outcome it *is* the diagnostic, because the raw residual takes exactly two values given p̂
(ROS §14.5).

Bin count: `arm`'s `floor(sqrt(n))` rule (clamped to `[5, 60]`) for the panels, **10 fixed** for the
sparkline so two predictors' glyph runs are comparable. Say which, once, in the roxygen.

⚠ **The ±2 SE band: `arm` does not implement its own book's formula.** Verified in its source: the band
is `2·sd(y[bin])/√n_bin` (empirical) while ROS §14.5 p. 253 specifies `2·√(p̄(1−p̄)/n_bin)` (theoretical).
Measured on correctly-specified data (n = 3 020, 54 bins): they agree on average (ratio 0.997) but differ
**±30 % per bin**, with coverage 98.1 % vs 92.6 %. **Use the theoretical band** where the family supplies
a variance function — it is what the source says, it reads better as a *reference*, and it stays correct
under weights (`p̄` is the weighted bin mean; `sd(y[bin])` ignores `w`). Document the divergence.

**Empirical-logit zero handling**: Haldane–Anscombe `(k + 0.5)/(n + 1)` uniformly. BeyondMLR uses three
mutually inconsistent strategies across its chapters; this one is symmetric, never infinite, needs no
arbitrary floor, and on the vignette's data no decile bin needs it at all **[M]**.

**The reference line must be a straight `lm`, not a loess** — the assumption *is* linearity, so the
comparator must be the line the model assumes. A loess would trace the curvature and hide the departure.

### 23.2 `rd_resid(fit, family, y, w)` — one residual, dispatched

| family                                                  | residual                                                  |
|---------------------------------------------------------|-----------------------------------------------------------|
| gaussian                                                | `rstandard()`                                             |
| binomial, `rr`, poisson, quasipoisson, grouped binomial | randomised quantile (Dunn & Smyth 1996)                   |
| ordinal                                                 | randomised quantile from the cumulative `fitted()` matrix |
| multinomial                                             | **none — refused** (§18.2)                                |

One function for five families, because `fitted()` gives cumulative probabilities for ordinal exactly as
`ppois` gives them for a count. Measured KS distance from uniform: binomial 0.0106, ordinal 0.0121,
poisson-on-`tvhours` **0.0617** — the last correctly flagging real overdispersion **[M]**.

⚠ **`qnorm(1) = Inf`**: clamp `u` to `[1e-10, 1 − 1e-10]`, or a naive `mean(r)` returns `Inf` **[M]**.
⚠ **Reproducibility**: a fixed default seed, exposed as an argument, with the caller's RNG state
restored (`withr::with_seed`; `withr` is already a Suggest). `seed = NULL` gives a fresh draw — the
honest way to check that a pattern is not a randomisation artefact.

### 23.3 `rd_qq(r, conf, max_pts)` — the analytic Q-Q band

The i-th of n uniform order statistics is `Beta(i, n−i+1)`, so the band is
`qnorm(qbeta(α/2, i, n−i+1))` … `qnorm(qbeta(1−α/2, i, n−i+1))`. **28 ms for all points, 9 ms thinned to
300, against 1 182 ms for a 19-replicate simulated envelope** **[M]**, agreeing to 0.19 on the most
extreme order statistic **[M]**.

⚠ It is **pointwise**, not simultaneous: under a true model ~5 % of points fall outside *at each
position*. Say so in the subtitle, not only the docs.

### 23.4 `REG_CHECKS` — the fact table

One row per check: `key` · `families` · `assumption` (the noun) · `stat` · `verdict` (thresholds →
ok/warn/bad) · `caption` (gettext'd) · `per_predictor` · `needs` · `panel`. **The footer rows and the
panel subtitles both read it**, which is what stops them drifting — the disease Phase 17 spent itself
removing. Adding a check is one row; the noun is the `assumption` column and `check =` takes the `key`,
so the footer label, the panel title, the argument value and the msgid all derive from one row.

## 24. Performance

| item                                         | cost                                                    |
|----------------------------------------------|---------------------------------------------------------|
| the whole footer block, 4-predictor binomial | **~44 ms on a 380 ms build (+12 %)** **[M]**            |
| curvature refit, per numeric predictor       | glm 17 ms · svyglm 81 · polr 110 · multinom 277 **[M]** |
| `car::vif()`                                 | 3.6 ms **[M]**                                          |
| the sandwich, one fit                        | ~12 ms **[M]**                                          |
| `rd_bin()` per curve                         | 1.5 ms **[M]**                                          |
| refit through `reg_fit()` (teaching only)    | 60 ms **[M]**                                           |
| a 6-panel grid at n = 6 803                  | 1 457 ms **[M]**                                        |
| for scale: `effect = "ame"`                  | **2 153 ms** **[M]**                                    |
| for scale: the stored curves                 | 19.5 KB on a 32 KB table **[M]**                        |

**Thinning policy — easy to get wrong.** `max_points` thins **the raw-point layer only, never the
statistics**. Bins, bands, verdicts and the influence ranking are always computed on the full data. And
the thinning must be **stratified toward the extremes**: the influence and Q-Q panels exist to surface
the rare extreme observation, so a uniform subsample defeats them.

⚠ **Never `geom_smooth(method = "auto")` in a package.** Verified in the ggplot2 source: it switches
loess → gam at **1 000 observations in the largest GROUP** (`max(table(interaction(group, PANEL)))`), so
a facetted 50 000-row plot gets loess and an unfacetted 1 200-row plot gets gam — and the message is
assembled dynamically, so it cannot be regex-suppressed. We avoid it by not smoothing (§23.1), but the
rule belongs in the file header for whoever adds a panel later.

## 25. Pedagogy

**Every panel says what it tests, in the source's own words.** Title = the assumption, not the plot
type; subtitle = the verdict from `REG_CHECKS`, i.e. a number *with a reading*:

```text
Linéarité — âge
Une droite ne suffit pas : la courbe monte puis s'aplatit (p < 0,001).
```

The assumption names come from the sources verbatim, because the student will meet them again: **LINE**
for gaussian (BeyondMLR §1.3 — Linear, Independent, Normal, Equal variance); *Poisson Response /
Independence / Mean = Variance / Linearity* for Poisson (§4.2.1); *Binary Response / Independence /
Variance Structure / Linearity* for logistic (§6.2.1). ⚠ The book has **no acronym** for the latter two —
inventing one would be a disservice.

**Show what "fine" looks like, on the plot**: the ±2 SE band, the analytic Q-Q band, the identity line,
the straight `lm` line, the VIF thresholds. The band *is* the "what noise looks like" device, at 28 ms
instead of 1 182 ms — and a better teaching object than a lineup, because it sits on the same panel as
the data. (A `nullabor` lineup is the strongest device in the visual-inference literature and is
deliberately **not** the default: 19 extra panels per check is the wrong cost. Opt-in, later.)

**Say out loud that p-values are useless here.** DHARMa: *"If you have a lot of data points, residual
diagnostics will nearly inevitably become significant."* Measured in this package: Brant gives
p = 0.00089 on a departure whose visible spread is 0.05 logits **[M]**. Consequence for the design: the
checks report **magnitudes** (the SE ratio, max dfbetas, max VIF) and p-values only where they carry
information a magnitude does not — Linearity and Proportionality.

⚠ **`plot.lm` uses three different residuals across its four panels** (verified in the R source): panel
1 raw (Pearson for glm), panel 2 standardized **deviance**, panels 3 and 5 standardized **Pearson**.
Nearly every ggplot2 reimplementation gets one wrong. Our dispatcher uses one residual per family
throughout — a deliberate divergence, to be **documented as one**.

⚠ **Independence is deliberately absent** from the checks. BeyondMLR is explicit that no residual plot
evaluates it, and jamovi's Durbin–Watson row is meaningless for survey data (rows are not a time
series). One sentence in the vignette; never a row, never a panel.

## 26. Rejected alternatives

| rejected                                             | why, measured or sourced                                                                                                                                                                                                                                                   |
|------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Depend on `performance` + `see`**                  | 2 panels for `polr`, 2 for `multinom`, 1 for `clm`, and `svyglm` diagnostics identical between a simple and a clustered design (§22.3). ~6 new packages for the families we least need help with, in a second visual language.                                             |
| **Depend on `DHARMa`**                               | No support for `svyglm`/`polr`/`multinom`/`svyolr`/`svy_vglm`; a survey-weighted binomial cannot `simulate()`.                                                                                                                                                             |
| **`keep_fits = TRUE` on `tab_reg()`**                | ~10 MB per model — the measured cause of the Phase-o jamovi freeze. A 4.3 KB recipe + a 60 ms refit does the same job **[M]**.                                                                                                                                             |
| **Keep `lm_plots()`'s fit-only contract**            | It is the contract that produced the broken vignette example **[M]**.                                                                                                                                                                                                      |
| **Emitting `poly()`/`ns()` for `shape =`**           | `marginaleffects` returns **AME = 0.000000**, silently **[M]** (§12.5). A wrong number, not an inconvenience.                                                                                                                                                              |
| **An uncentred `x + I(x^2)`**                        | VIF 38.7 vs 1.2 **[M]**, and coefficients no reader can interpret.                                                                                                                                                                                                         |
| **`scale()` in the emitted formula**                 | Prediction on a subset re-scales with the subset's mean **[M]**.                                                                                                                                                                                                           |
| **A Rao score test for Linearity**                   | 4× cheaper and agrees unweighted, but **design-blind**: identical p on a weights-only and a clustered design **[M]**.                                                                                                                                                      |
| **Box–Tidwell**                                      | Needs `x > 0`; does not generalise across families. Name it, don't compute it.                                                                                                                                                                                             |
| **Hosmer–Lemeshow**                                  | Arbitrary in `g` (p = 0.11 / 0.0499 / 0.64 for g = 9/10/11 on one model), always rejects at survey n, says nothing about *where*, computed in-sample so it can pass an overfitted model. Harrell: "obsolete". Lemeshow co-authored its replacement (the calibration belt). |
| **A randomised quantile residual for multinomial**   | cor = **−0.705** between two level orderings **[M]**.                                                                                                                                                                                                                      |
| **`arm`'s empirical ±2 SE band**                     | ±30 % per-bin disagreement with its own book's formula, and it ignores weights **[M]**.                                                                                                                                                                                    |
| **A 19-replicate simulated Q-Q envelope**            | 1 182 ms vs 28 ms for the analytic band **[M]**.                                                                                                                                                                                                                           |
| **`geom_smooth(method = "loess")` overlays**         | 370 → 2 030 ms as n goes 6 803 → 21 483 **[M]**; not weight-aware; smooths a two-valued residual for a binary outcome.                                                                                                                                                     |
| **A dedicated sparkline column**                     | Widens every reg table with a numeric predictor, for a mark that belongs to that predictor's own row.                                                                                                                                                                      |
| **`kableExtra::spec_plot` / `svglite` / base64 PNG** | 1 084 B and 843 B against **121 B** **[M]**; none reaches the console or Excel.                                                                                                                                                                                            |
| **Reuse `tab()` to build the bins**                  | Would route a plot through the fmt/colour pipeline to extract numbers it then throws away. `rd_bin()` is 12 lines of base R.                                                                                                                                               |
| **A jamovi panel in this phase**                     | Couples a new plotting API to the byte-locked `.fit_cache` contract for no benefit (§27).                                                                                                                                                                                  |

## 27. jamovi

**Nothing is added in this phase, and the reason is architectural.** The regression backend's fast path
exists because the digest stores coef + vcov and no model frame; every *panel* needs the frame. Forcing
the `fit` tier to serve diagnostics would re-persist ~10 MB per model — the measured cause of the
Phase-o freeze.

But the reorganisation improves jamovi anyway, for free: **the five footer rows and the sparklines are in
the table**, so the live UI shows the checks with no new plumbing, and the sparklines even survive the
digest path (§15). The clean follow-up, when wanted: `jmvtabreg.b.R` already holds `self$data` and its
`.plot` renderFun is a no-op stub — an `Image` calling `reg_assumptions_plots(table, self$data)` is a
60 ms refit and nothing cached.

## 28. Open questions

1. **`check = "auto"` panel count.** 6 panels in 3×2, or 4 in 2×2 (today's `lm_plots` shape)?
   **Recommendation: 6, `ncol = 3`.**
   **Maintainer’s decision: ok**
2. **Per-predictor Linearity — all, or the worst?** With 8 numeric predictors the faceted panel has 8
   facets. **Recommendation: all up to 6, then the worst 6 with a note** — the whole value is finding the
   one you were not looking for. The same question applies to the **footer rows** (§16): one row per
   numeric predictor is right for the 1–3 numerics this audience's models have; above that, consider one
   row naming the worst.
   **Maintainer’s decision: keep all numeric variables.**
3. **`shape = "cut5"` — quantile or equal-width?** §12.4 assumes quantiles (equal n per group, which is
   what a survey audience means by "age groups" and what keeps every group estimable), but base `cut()`
   is equal-width, so the value must not be called `"cut"` if it means quantiles.
   **Recommendation: `"quintiles"` / `"quartiles"` / an integer k**, no equal-width until asked.
   **Maintainer’s decision: ok for quantiles.**
4. **Multinomial Linearity: pay 277 ms/predictor, or gate it?** (§16) Decide with the live UI in front of
   you.
   **Maintainer’s decision: multinomial is aleardy long, so it’s ok to add a few seconds.**
5. **Should `reg_assumptions_plots()` accept `tab()` output?** A cross-table has no model, so no — but
   `color = "contrib"` *is* the departure from the log-linear independence model. **Recommendation: out
   of scope**, said in the roxygen so the question is closed rather than left open.
   **Maintainer’s decision: out of scope.**
6. **Name.** Now that it is explicitly the *teaching* function, `reg_check_plots()` is shorter than
   `reg_assumptions_plots()` and echoes `check_model()`, which the audience knows.
   **Recommendation: decide at implementation**; if the long name stays, `reg_assumptions()` remains free
   as a numbers-only companion.
   **Maintainer’s decision: `reg_check_plots()` is good**
7. **Vignette placement.** A `## Checking the model` section in both reg vignettes, built on §2 — which
   means leaving the vignette's own model linear and using it as the worked example.
   **Recommendation: leave it wrong and point at it**; correcting it would move every numeric result in
   the vignette for no pedagogical gain.
   **Maintainer’s decision: ok.**
8. **What was NOT verified**: `car::vif()` on `svyVGAM::svy_vglm`; `shape =` through a weighted ordinal;
   Gelman & Hill (2007) p. 97 itself (the ±2 SE formula is verified verbatim from ROS §14.5 p. 253, and
   both `arm` and `performance` cite p. 97 for matching content); the exact R version in which
   `plot.lm`'s glm Q-Q became a half-normal of |deviance|.
   **Maintainer’s decision: verify.**
9. **An exact φ row for count families?** §9.1 measured that the SE ratio is √φ to within 1.5–8 % and
   that the two deliberately diverge under `quasipoisson` (φ 2.62, ratio 1.03) — so the ratio is the
   right *check*. But φ is the number a paper reports, and it is one line to add back as a second row
   for poisson / quasipoisson / grouped binomial only, gated `needs = "design_free"` (it cannot be
   computed honestly on a clustered fit — §9.1, reason 3). **Recommendation: no** — the vignette can say
   `summary(fit)$dispersion`, and a second row in one family is how a five-row block becomes a seven-row
   one. Reopen if a reviewer asks for φ in a table.
   **Maintainer’s decision: yes add an exact φ row for count families, it’s needed, specially if the new Dispersion stat already takes it into account with quasipoisson.**

## 29. Sources

**BeyondMLR** (Roback & Legler), `~/github/BeyondMLR`: §1.3 LINE and §1.6.1 the four-panel reading; §3
distribution shapes; §4.2.1 the four Poisson assumptions, §4.4.8–4.4.9 deviance residuals and GOF,
§4.10.1 φ̂ and quasi-Poisson, §4.11 the observed-vs-modelled pmf; §6.2.1 the four logistic assumptions,
§6.5.2 / §6.7.2 the two empirical-logit constructions, §6.5.6 the GOF validity condition, §6.5.9
binomial overdispersion; §7.4 overdispersion; §11.3.2 the conditional-density + empirical-logit panel.
⚠ The book covers **no** multinomial, ordinal or survey-weighted model — those designs are extensions,
not citations.

**Residuals and bins.** Dunn & Smyth (1996) *JCGS* 5(1) 1–10. Gelman, Hill & Vehtari, *Regression and
Other Stories* §14.5 p. 253. `arm::binnedplot` / `binned.resids`, `performance::binned_residuals`,
`regressinator::binned_residuals` sources.

**Calibration and GOF.** Van Calster et al., BMC Medicine 2019;17:230. Harrell, RMS + datamethods.
Allison, *"Why I Don't Trust the Hosmer-Lemeshow Test"*. Austin & Steyerberg, Stat Med 2014;33:517–535.
Nattino, Finazzi & Bertolini (the calibration belt). TRIPOD+AI, BMJ 2024;385:q902.

**Functional form.** `car::residualPlots` (the curvature test = the t-test for `I(X^2)` in
`update(model, ~ . + I(X^2))`; Tukey's one-df nonadditivity test on the fitted values). Box–Tidwell as
the taught linearity-of-the-logit test (Hosmer & Lemeshow 1989; Menard 2002/2010), with splines as its
recommended remedy.

**Implementation traps.** R `stats::plot.lm` source. ggplot2 `StatSmooth$setup_params`. `performance`
[#376](https://github.com/easystats/performance/issues/376),
[#907](https://github.com/easystats/performance/issues/907),
[#260](https://github.com/easystats/performance/issues/260). DHARMa
[#321](https://github.com/florianhartig/DHARMa/issues/321) and its vignette. jamovi's *Assumption Checks*
pane (collinearity table, Durbin–Watson, normality, residual plots, Cook's distance) — the reference
software this audience knows. `skimr` unicode sparklines + `fix_windows_histograms()`;
`kableExtra::spec_plot`; `gtExtras::gt_plt_sparkline`. `marginaleffects` 0.32.0 on `poly()`/`ns()` —
measured here, not sourced; the mechanism is `makepredictcall`/`predvars` vs a re-evaluated basis.

**In-package.** `R/reg-influence.R` (z8-B, z9, z10) — the influence functions and the "return NULL rather
than a wrong number" contract this phase inherits. `dev/model_vs_observed_gap_test.md` §3, §13.
`dev/numeric_predictors_crude_counterparts.md`. `dev/black_and_white_publication_palette.md` §12.

⚠ **How the measurements were made.** Every **[M]** comes from throwaway scripts in this session's
scratchpad, on `gss_cat_data_formatting()` at n = 12 960 (the 4-predictor complete cases) or n = 6 803
(also dropping `tvhours`), with `marginaleffects 0.32.0`, `pillar 1.11.1`, R on WSL2/ext4; simulated
cases say so inline. The scripts are not kept — each claim names the design that produced it so it can be
re-derived. Round 1 reported n = 6 803 and ΔAIC = 1251 for §2; **both were wrong** (6 803 is what you get
by dropping NAs on *every* column, including a variable the model does not use), and the corrected
figures are in §2.

---

# PART V — IMPLEMENTATION RECORD

## z15-i (2026-08-11) — the primitives and the check block

Landed: `R/reg-assumptions.R` (the `REG_CHECKS` fact table, `reg_checks_for()`,
`reg_check_spec_entries()`, `reg_check_expand()`, `reg_check_rows()` and the five statistics),
`reg_fit(add_terms =)`, `reg_shape_term()`, the 13th `test` column `term`, `reg_footer_plan()`, the
`dispersion` / `phi` split, `car` as a Suggest, `tests/testthat/test-reg-checks.R`.

Suite green in both locales (FAIL 0, WARN 0; PASS 5209 / 5189). Zero display or export snapshot churn;
the 36 structural goldens regenerated with `dev/verify_golden_field_delta.R` proving over 1787 cells
that the only delta is the added empty `term` column.

### Three corrections to this design

1. **§16's "one line" footer extension does not exist, and would have corrupted three paths.** The
   claim was that a footer row whose `row_var` is non-empty could render `label: row_var`. But
   `row_var` on a reg footer row already means the **split-group level** — in `reg_footer_lines()`
   (the `is_split` switch *and* the cell key), in `test_grid_reg()` (the group key) and in
   `reg_spread_models()` (which re-keys by it and **drops the misses**). A predictor name there flips a
   plain table into "split" mode, emits one blank block per predictor, and is silently deleted on a
   spread table. The per-predictor key is therefore a new column, `term`, and the label rule lives in
   one shared `reg_footer_plan()` that both row renderers consume.

2. **The line form is wrong for anything keyed to a model column — measured, not argued.** §16 was
   right that these belong in the block, but the pre-existing `stats = "global"` line proved *why*: in
   a 3-model comparison it rendered as three sentences with nothing naming which model each described,
   and on a `split_var` table it printed the split level, repeated, instead of the predictors (the
   split branch overwrites `row_var` for every row of a group's test tibble). So `global` **migrated
   to rows** with the checks, and `reg_global_lines()` / `reg_term_test_line()` are deleted. Only the
   interaction test stays a line: it is pooled across groups and belongs to no column, and it reads
   correctly as one.

3. **`drop1()` cannot test a multinomial, and §7.2 assumed it could.** `nnet:::drop1.multinom`
   computes only `Df` and `AIC` — it has no `test` argument and returns **no p-value at all** (and it
   `cat()`s "trying - <term>" progress that no condition handler catches, which was leaking into the
   console). §24's "multinom 277 ms" was timing a call that cannot produce the number. The fix is not a
   second test but the same one computed differently: the likelihood ratio between the two nested fits,
   which reproduces `drop1()`'s LRT **to 1e-10** on a glm. A design fit stops before it, an LR being
   invalid there. Two further traps on that path: `multinom`/`polr` store `data = mdata`, a local of
   `reg_fit()`, so `drop1()`'s internal `update()` failed with *"object 'mdata' not found"* — cured by
   `reg_selfheal_call()`, extracted from the identical fix Phase 12d had already written inline for
   `brant::brant()`.

4. **φ can be computed honestly under a design after all** (so §9.1 reason 3 and open question 9's
   `needs = "design_free"` gate both fall away). The 22.49 measurement was not a property of the
   Pearson dispersion; it was `reg_dispersion()` reading `stats::df.residual(fit)`, which for an
   `svyglm` is the DESIGN degrees of freedom. Dividing by `n - rank` — computed rather than read —
   fixes it, and touches nothing else, because the SE-scaling caller is gated `!weighted` where the two
   denominators agree. Ruling 9's exact-φ row therefore ships for every count fit, as `stats = "phi"`.

### Two smaller deviations

- **`stats` carries the checks; there is no new argument.** R7 ("always, no opt-in gate") is satisfied
  by putting them in the *default set*, which also gives a per-check escape for free and keeps
  `stats = FALSE` meaning what it meant.
- **`rd_bin()` / `rd_resid()` / `rd_qq()` were not written** (§21 step 1 asked for them unwired). They
  have no caller until z15-iii, and shipping untested functions for two sessions is the dead weight the
  roadmap's own rules forbid. They land with the curves that use them.

### Confirmed by measurement during implementation

| claim | measured here |
|---|---|
| the influence route reproduces `stats::dfbetas()` | 0.214 vs 0.215, correlation **0.999999** |
| Dispersion ≈ √φ on an over-dispersed count, ≈ 1 under quasipoisson | ratio/√φ within 5 %; quasi within 10 % of 1 |
| Collinearity == `car::vif()` | equal to 1e-10, on both the matrix and the bare-vector shape |
| Linearity == `drop1()` on the augmented fit | equal to 1e-6 |
| centring leaves the curvature p unchanged but the pair's VIF does not | p equal to 1e-8; VIF > 20 raw, < 5 centred |
| the whole block's cost | **+88 ms on a 157 ms build (+56 %)**, of which ~72 ms is the one Linearity refit + its test and ~16 ms all four other checks. §24's "+12 % on a 380 ms build" was measured against a heavier baseline; the absolute figures agree |
| multinomial cost | 756 ms -> 1535 ms for one numeric predictor (ruling 4 accepted this) |

## z15-ii + z15-iii (2026-08-11) — `shape =`, the curves, the sparkline, the plots

Landed: the `shape` family and the plot primitives in `R/reg-assumptions.R`; `meta$assumptions` +
`get_assumptions()`; the row sparkline (`options(tabxplor.spark)`) with its html `<svg>` upgrade;
`reg_check_plots()` and the deletion of `lm_plots()`; `reg_plot_colors()`/`reg_plot_theme()` adopted by
`or_plot()`; `tests/testthat/test-reg-shape.R`; the FR catalogue (201 translated, 0 fuzzy).

### Five corrections to this design

1. **`shape` is a DATA RECODE for three of its five values, not five term emissions.** §12.1 specified
   `z = (x − x̄)/s` emitted terms for every value. But `log`/`sqrt`/`cut` recoding the COLUMN at one
   boundary makes every downstream subsystem work untouched — a quantile-cut `age` genuinely IS a
   factor, so it inherits the crude twin, the per-level N, the colours and the adjustment gap with no
   code, and `reg_predictor_types` records what it now is. Only `quadratic` emits a term. The cost is
   two lines (`data` and the design's `$variables`, the rule `reg_relevel_design()` already follows).

2. **The linear term stays RAW, and the multiplier does the centring's work for free.**
   §12.3 asked for `z + I(z^2)`. But `a·x + b·((x−m)/s)²` and `A·z + B·z²` are the same model with
   `A = a·s`, so with `multiplier = "sd"` (z9's default) the printed linear row ALREADY is the per-SD
   slope of the centred parametrisation — verified against a hand-built glm to 1e-6. No second scaling
   rule, and `reg_shape_term()` stays the ONE object the check and the cure share.

3. **`reg_shape_term()` must return the DEPARSED string** — the one implementation trap. A
   model-matrix column is named by the formula's own term label, which R produces by deparsing, and
   deparse drops the spaces around `/` that a pasted string keeps. The skeleton then missed the fit's
   term by two characters and the curvature row rendered EMPTY, with no error.

4. **The sparkline needs the MODELLED level, and §17's font trap hits our own plot backend.**
   `rd_link_y()` read `as.numeric(as.factor(y)) - 1`, i.e. the factor's first level — which for the
   vignette's own `married` is the COMPLEMENT of what the model fits, so the curve was upside down
   beside a correct odds ratio. It now takes `fits[[1]]$positive_level`. And a graphics device has no
   block glyphs: `tab_plot()` / `or_plot()` emitted one `mbcsToSbcs` conversion failure per label and
   drew garbage, so the plot medium strips the run (`tx_spark_strip()`) while console / markdown /
   Excel keep it and html upgrades it.

5. **The `<svg>` upgrade does not belong in `html_escape_br()`** (§17's "one place decides what markup
   of ours survives escaping"). The sparkline lives in the `levels` column, which is an ORDINARY TEXT
   CELL — the rowspanned label path that `html_escape_br()` serves never carries one, and putting it
   there would have escaped the markup back into text. It sits at the text-cell emission instead, and
   reads the polyline **out of the glyph run itself**: no lookup into `meta$assumptions`, no key to
   keep in sync, and it therefore survives transpose, `tab_spread` and any pipeline that moved the
   label.

### Two smaller deviations

- **The panel set is `REG_CHECKS`, extended rather than duplicated.** §13's panel table and §23.4's
  `panel` column would have been a second vocabulary beside the checks. Instead the fact table gained a
  `panel` field and TWO rows that are TAUGHT BUT NEVER SCORED (`residuals`, `normality` — §14's own
  measurements: non-discriminating as verdicts, canonical as lessons). They carry an EMPTY `types`,
  which IS the statement "a panel and no footer row", and `reg_checks_for(what = "panel")` is the same
  selection rule with a declared filter. `check =`, `stats =` and the panel titles are one vocabulary.
- **`fit_spec` rides `reg_meta`, and the guard reads the table's own `n` footer row** rather than a
  stored `nobs` (§13.1) — the N is already there, for every model column, so the guard costs no
  storage and stays silent when `stats = FALSE` left nothing to compare against.

### Confirmed by measurement during implementation

| claim | measured here |
|---|---|
| the quadratic pair == a hand-built `glm(y ~ ... + I(((x-m)/s)^2))` | equal to 1e-6, both rows |
| the crude twin's term names are IDENTICAL to the model's | `Obs_OR` fills BOTH shaped rows; the curvature row == the univariable fit to 1e-6 |
| centring keeps the pair estimable | `car::vif()` < 5 on the emitted terms |
| `shape = "linear"` is byte-identical to no shape | labels + `Model_OR` identical |
| `spark = FALSE` restores the old label byte-for-byte | `"age (per 10 units)"` |
| `rd_bin()` == `stats::weighted.mean()` per bin | 1e-10, and its logit band == the theoretical formula |
| the Q-Q band is the Beta order-statistic one | 1e-10; mean pointwise coverage 0.95 over 20 replicates (a SINGLE sample reads 0.85 — consecutive order statistics are correlated) |
| `rd_resid()` is standard normal under a correct model | KS 0.06 at n = 800 |
