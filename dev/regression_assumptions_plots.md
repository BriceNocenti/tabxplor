# Regression assumption plots — design study

Date: 2026-08-10, **second research round 2026-08-11** (§0.B, §3 R5–R8, §4.4, §5.6–§5.8, §6.1–§6.2,
§13, §17, §20). Status: **DESIGN COMPLETE, no R code written.** This report answers Last Phase z12 and
records the maintainer's eight rulings (§3). Implementation is a separate session.

The second round exists because round 1 designed the *plots* and left five questions open: which
assumptions belong in the footer as numbers, which need the eye, where the plot data lives, how a user
fixes a non-linearity once found, and how to name a test in one word. Those are now answered and
measured, and one round-1 headline number is corrected (§1.1).

Scope: replace `lm_plots()` with `reg_assumptions_plots()`, a model-assumption diagnostic covering
every family `tab_reg()` can fit — gaussian, binomial, poisson / quasipoisson, the z3 modified-Poisson
`rr`, grouped binomial (`trials =`), ordinal (`polr` / `svyolr`), multinomial (`multinom` /
`svy_vglm`) — weighted and design-based included.

Every number below marked **[M]** was measured on this box today, on `gss_cat_data_formatting()`
(n = 21 483 raw, 6 803 after the four-predictor complete-case filter) unless another n is named. The
probe scripts were one-off and are not kept; each measurement names the design that produced it.
Claims taken from a source rather than measured are cited inline; §19 lists the sources, and §17.9
lists what I could **not** verify.

---

## 0. Executive summary

**The feature is justified by a defect in tabxplor's own flagship example, and the package already
owns the hard half of the machinery.**

Twelve findings govern the plot design (§0.A); eight more, measured in the second round, govern where
the *numbers* live and how a user acts on them (§0.B).

### 0.A — round 1: the plots

**Why it must exist.**

1. **The model used throughout the regression vignette is mis-specified, and no tabxplor output
   reveals it — and the damage is not confined to the mis-specified row.** On
   `married ~ race + age + rincome + relig` (n = 12 960), letting `age` curve moves the **printed
   adjusted odds ratios of the other predictors by up to 23.8 %** (income "$25 000 or more":
   OR 1.86 → 1.42; median move 10.4 %) and **flips one level's 5 % significance** (income
   "$15 000 to 24 999": p = 0.0001 → 0.40) **[M]**. ΔAIC for the quadratic is **296**, for `ns(4)`
   **465**, LRT p = 9.9e-67 **[M]**. z9 gives that row a crude twin and z8-B tests the gap between
   them; neither can say *"one slope is the wrong summary — and it is bending the income effect you
   came here to read."* (§1.1 corrects the round-1 numbers, which were not reproducible.)
2. **The documented `lm_plots()` example errors, in both the EN and FR vignettes.** `m <- tab_reg(...)`
   then `lm_plots(m)` → *"With a data frame, supply `dependent` and `predictors`."* **[M]** A
   `tabxplor_tab` **is** a data frame, so it takes the data-frame branch. The maintainer wrote the
   call that the rest of the package's grammar implies (`or_plot(tabs)`), and the function's contract
   contradicted it. That is the interface bug this phase fixes, not a typo.

**What the package already owns.**

3. **`R/reg-influence.R` is a design-correct influence function for every class tabxplor fits** — and
   it is exactly dfbeta. `reg_coef_if_maker(glm)` reproduces `stats::dfbeta()` with **correlation
   1.0000** and a maximum discrepancy of **1.4 % of one SD** (one-step vs exact leave-one-out) **[M]**.
4. **It works where base R has no method at all.** `cooks.distance(polr)` and `cooks.distance(multinom)`
   both **error** ("no applicable method") **[M]**; `reg_score_polr()` / `reg_score_multinom()` +
   `reg_if_from_score()` return a per-observation influence for both (z10) **[M]**.
5. **`reg_if_se()` reproduces `SE(svyglm)` exactly on a stratified + clustered design** (0.0011408 vs
   0.0011408; the IID version gives 0.0011299, 1 % low) **[M]**. So the influence panel is the one
   diagnostic in the whole ecosystem that respects strata and clusters.
6. **No existing package covers tabxplor's families.** Measured by a subagent that installed
   `performance 0.17.1` + `DHARMa 0.5.0`: `check_model()` returns **2 panels for `polr`, 2 for
   `multinom`, 1 for `ordinal::clm`**, and for `svyglm` it returns 8 panels that are **identical
   between a simple design and a stratified+clustered one** — it silently ignores the design. A
   survey-weighted binomial `svyglm` cannot use the simulated-residual path at all (non-integer prior
   weights break `simulate()`), and DHARMa's own *"absolutely no guarantee that this will work"*
   warning is swallowed by `check_model()`'s `suppressWarnings()`. `DHARMa` has never supported
   `svyglm` ([florianhartig/DHARMa#321](https://github.com/florianhartig/DHARMa/issues/321), open and
   unanswered since 2022). **A dependency would not cover the hard cases; it would only cover the easy
   ones, in a second visual language.**

**The architecture is three primitives, not fourteen panels.**

7. **One weighted quantile-binning primitive underlies four panels** (binned residuals, empirical
   link, calibration, mean-variance). Binned means cost **19 ms where a loess smoother costs 370 ms at
   n = 6 803 and 2 030 ms at n = 21 483** **[M]** — and, unlike a smoother, they are weight-aware for
   free and they *are* the diagnostic for a binary outcome (Gelman & Hill's whole argument).
8. **One randomised-quantile-residual primitive serves binomial, poisson, `rr` and ordinal**, computed
   from `fitted()` alone. Measured KS distance from uniform: binomial 0.0106, ordinal 0.0121 (both
   correctly specified), poisson-on-`tvhours` **0.0617** — 6× larger, correctly flagging the
   over-dispersion **[M]**.
9. **One analytic Q-Q band replaces a simulation envelope: 28 ms vs 1 182 ms**, using the beta
   order-statistic quantiles `qnorm(qbeta(α, i, n−i+1))`, and it agrees with a 19-replicate simulated
   envelope to 0.19 on the extreme order statistic **[M]**.
10. **End-to-end: a 6-panel binomial grid builds and draws in 1 457 ms at n = 6 803** **[M]** — with
    no new dependency beyond `car` for VIF.

**Two traps that would ship as wrong numbers if not designed against.**

11. **`df.residual(svyglm)` on a clustered design is the DESIGN df, not n − p** — 949 instead of
    21 403 on a 120-PSU / 8-stratum design. A dispersion computed as `Σ(Pearson²)/df.residual` then
    reads **φ = 22.49** instead of 1.00, a factor of exactly n/nPSU **[M]**. Any dispersion panel must
    be refused for a design-based fit.
12. **A randomised quantile residual is order-dependent for a NOMINAL outcome.** Re-fitting the same
    multinomial with the category order reversed gives residuals correlated **−0.705** with the
    originals **[M]**. Multinomial therefore gets **no residual panel** — calibration per category
    instead. This is a refusal, and the report states it as one.

### 0.B — round 2: the numbers, the miniature, and the fix

13. **The assumption block is affordable as an always-on default.** On the vignette's 4-predictor
    binomial (2 numeric), the whole block costs **~44 ms against a 380 ms build (+12 %)**: curvature
    refits 25 ms (both predictors), `car::vif` 3.6 ms, dispersion 0.3 ms, the two binned curves
    3.0 ms, the separation probe 0.5 ms, one influence contrast 11.8 ms **[M]**. For scale,
    `effect = "ame"` on the same model costs **2 153 ms** **[M]** — the block is noise beside the
    feature the user already opts into. The one place that needs stating out loud is the
    per-predictor curvature refit by family: glm **17 ms**, svyglm **81 ms**, polr **110 ms**,
    multinom **277 ms** (n = 6 803) **[M]**.
14. **`marginaleffects` silently returns AME = 0 for `poly()` and `ns()`.** Measured on
    `marginaleffects 0.32.0`, every contrast form (`variables = "x"`, `list(x = 1)`, `list(x = sd)`,
    `avg_slopes`) returns **0.000000** where `predict(newdata = )` gives the correct +0.038 **[M]**.
    Root cause, reproduced in 12 lines: the orthogonal basis is **re-evaluated on the perturbed
    data**, and an orthogonal polynomial absorbs a location shift exactly — max |ΔX| across the design
    matrix 0.005, AME 0.002953 → **0.000000** **[M]**. `I(x^2)`, `poly(x, k, raw = TRUE)` and `log(x)`
    are correct through every route **[M]**. **So tabxplor must never emit `poly()`/`ns()`** — the
    single most consequential constraint on §5.8's grammar. (tabxplor's own compound-formula + `ame`
    path happens to return the *correct* value today — −0.003818911, exactly the g-computation truth
    **[M]** — because `insight` fails to recover its data and falls back to the `predvars` route. That
    is luck, not contract, and §17.4 records it as such.)
15. **Centring a curved predictor is required, not cosmetic.** Raw `age + I(age^2)` gives
    **VIF 38.7** and cor(x, x²) = 0.985; the centred/scaled `z + I(z^2)` gives **VIF 1.2** and
    cor = 0.266 **[M]**. Since round 2 adds a collinearity line to the footer, an uncentred emission
    would make the package scream 38.7 at a perfectly well-specified model — the exact "wrong number"
    failure `reg-influence.R` refuses. Centring also makes the linear coefficient readable: it is the
    **slope at the mean, per 1 SD** (OR 1.577), which is what `multiplier = "sd"` already promises.
    **Maintainer’s decision: ok to center all numeric predictors**, but it should be clear on docs for the user to know.
16. **A curved predictor's crude twin needs no new machinery.** Fitting the univariable model with the
    *same* frozen terms yields **term names identical to the model's** **[M]**, so z9's skeleton-key
    alignment, `Obs_*`, `color = "adjustment"` and the z8-B gap test all work unchanged: model
    OR 1.577 vs crude 1.638 (slope at the mean), 0.749 vs 0.730 (curvature) **[M]**.
17. **A text sparkline reaches every backend; an SVG reaches one.** A 10-bin Unicode block sparkline is
    **10 characters / 30 bytes**, is plain text (so console, markdown, html and Excel get it through
    the existing `levels` column with no export machinery), and is legible: a straight line reads
    `▁▂▃▃▄▅▆▆▇█`, the real `age` curve `▁▄▇▇█████▇`, an inverted U `▁▄▆▇██▇▆▄▁` **[M]**. A hand-rolled
    inline `<svg><polyline>` is **121 bytes** minimal (240 with `xmlns`), against 1 084 bytes for
    `svglite` and 843 for a base64 PNG **[M]** — 7–9× smaller, no new dependency, html only.
18. **The cheap curve is the right curve.** The fit-free empirical link curve and the
    partial-residual (component + residual) curve correlate **0.997** on the vignette model and give
    sparklines that differ in one bar (`▁▄▇▇█████▇` vs `▁▄▇██████▇`) **[M]**. So the miniature uses the
    fit-free one — which also means it survives the jamovi digest path, where no fit exists.
19. **`car` earns its dependency, and multinomial is the one refusal.** `car::vif()` works on
    lm / glm / svyglm / **polr / svyolr**; on `nnet::multinom` it warns *"No intercept: vifs may not be
    sensible"* (the open easystats #907) **[M]**. A hand-rolled `det(R₁₁)det(R₂₂)/det(R)` VIF
    reproduces `car`'s GVIF **exactly** for glm but returns 11.45 where `car` returns 1.01 for polr
    **[M]** — so the 15-line replacement is not a drop-in, and §17.8 closes in `car`'s favour.
20. **A score test would be 4× cheaper and is design-blind — rejected.** The no-refit Rao score test
    for an added quadratic costs 7.6 ms vs 17–31 ms for the refit and agrees on the decision
    (p 5.5e-37 vs 1.3e-34) **[M]**, but it returns the **identical p (1.15e-67) on a weights-only and
    on a stratified + clustered design**, where the design-based Wald gives 5.7e-43 and 4.0e-37
    **[M]**. The refit + the existing `reg_term_tests()` dispatcher (drop1 / regTermTest) is therefore
    the single rule, and it is a rule the package already owns.

---

## 1. What the phase is actually for

The maintainer's ask: *"assumptions tests and plots to be more rigorous about regressions, for both
numeric outcomes, and numeric predictors."* Those are two different jobs and both are needed:

- **numeric outcomes** (gaussian, poisson) — the classic distributional checks: is the error
  structure what the family claims?
- **numeric predictors** (every family) — the functional-form check: is *one slope* the right summary
  of this predictor's effect?

The second is the one with a measured defect in the shipped documentation (§0.1), and it is the one
that completes a story the package has been building since z5:

| Question the reader has                | What answers it today                                        |
|----------------------------------------|--------------------------------------------------------------|
| How big is this effect?                | `Model_OR` / `Model_β` / `Model_AME`                         |
| Is it confounded?                      | `Obs_*` beside it (z5 `empirical =`), `color = "adjustment"` |
| Is that gap real, or noise?            | z8-B's `gap_se` + `color_signif`                             |
| **Is the model's SHAPE right at all?** | **nothing**                                                  |

`reg_assumptions_plots()` is the fourth row. Framing it that way is not decoration — it decides the
default panel set (§6): the functional-form panels come **first**, because they are the ones that
change what the table means.

### 1.1 The motivating measurement, in full (CORRECTED 2026-08-11)

⚠ **Round 1's numbers here were not reproducible and are replaced.** Round 1 reported n = 6 803 and
ΔAIC = 1251; the vignette's model has **n = 12 960** complete cases (6 803 is what you get by dropping
NAs on *every* column of `gss_simple`, which also drops `tvhours`, a variable the model does not use),
and its ΔAIC is **296**. The qualitative conclusion is unchanged and the quantitative case is now
*stronger*, because round 2 measured the consequence that matters (the third table below).

`married ~ race + age + rincome + relig` on `gss_simple` (n = 12 960), deciles of `age` **[M]**:

| mean age | P(married) | empirical logit |    n |
|---------:|-----------:|----------------:|-----:|
|     21.8 |      0.139 |           −1.83 | 1155 |
|     26.5 |      0.322 |           −0.74 | 1170 |
|     31.1 |      0.474 |           −0.10 | 1561 |
|     35.5 |      0.536 |           +0.14 | 1267 |
|     39.5 |      0.553 |           +0.21 | 1271 |
|     43.5 |      0.564 |           +0.26 | 1270 |
|     47.5 |      0.555 |           +0.22 | 1245 |
|     51.9 |      0.557 |           +0.23 | 1413 |
|     57.3 |      0.584 |           +0.34 | 1309 |
|     66.8 |      0.537 |           +0.15 | 1299 |

The shape is a **saturating rise, not an inverted U** (round 1 said inverted U; the fitted quadratic
does turn over — vertex at age 52 — but the *empirical* curve only flattens over the observed range).
R² of a straight line through the ten points **0.500**; of a quadratic **0.885**. AIC 16 960 → 16 664
(**ΔAIC 296**); `ns(4)` 16 495 (**ΔAIC 465**); LRT p = 9.9e-67 **[M]**.

**What it costs the reader** — the adjusted effects the table is actually about **[M]**:

| printed row              | OR, age linear | OR, age quadratic |                        change |
|--------------------------|---------------:|------------------:|------------------------------:|
| income $25 000 or more   |          1.863 |             1.419 |                   **−23.8 %** |
| income $15 000 to 24 999 |          1.273 |             1.056 | −17.0 % (**p 0.0001 → 0.40**) |
| income $10 000 to 14 999 |          1.140 |             1.021 |                       −10.4 % |
| race Black               |          0.416 |             0.398 |                        −4.3 % |
| race Other               |          1.109 |             1.120 |                        +1.0 % |

One income level's conclusion **flips at the 5 % threshold**. And the two models tell opposite
substantive stories about age itself: linear says P(married) climbs monotonically to **0.686** at 85,
quadratic says it peaks near 52 and falls to **0.219** **[M]**.

**The AME is not a refuge.** `effect = "ame"` reports +0.0693 per SD under the linear specification
and **+0.0296** under the quadratic — a factor **2.3** **[M]**. Marginalising fixes the *interpretation*
of a curved fit; it does not repair a straight-line fit.

This is the example in `vignettes/tabxplor-reg.Rmd`, in the FR article, in `?tab_reg`, and in the
jamovi screenshots. It should become the worked example of the new vignette section — a package that
ships the tool to find its own documentation's mis-specification is making the argument better than
any synthetic fixture could.

---

## 2. The asset inventory — what exists, what is missing

### 2.1 On the table (measured against the live code)

`reg_meta` carries **18 fields**: `family`, `families`, `exponentiate`, `effect`, `at`, `do_exp`,
`eff_word`, `dependent`, `positive_level`, `predictors`, `predictor_types`, `multiplier`,
`crude_keys`, `split_var`, `comparison`, `wt`, `model_labels`, `conf_level`.

It does **not** carry: the data, the fits, the formula (compound mode), `trials`,
`ids`/`strata`/`fpc`/`nest`, `na`, `inverse_two_level_factors`, `method`. No fit is ever attached to a
returned table — both `new_tab()` calls store only `subtext`, `test` and `meta`.

The `.fit_cache` digest (`reg_build_digest`) stores `coef` + `vcov` + scalars + glance and
**deliberately discards the model frame** — so the jamovi fast path has no residuals, no fitted
values, no hat values. That is a load-bearing decision (Phase o measured ~10 MB per retained fit as
the cause of the model-comparison freeze) and this phase must not weaken it.

### 2.2 Diagnostic accessors, per class **[M]**

`—` = base R **errors** ("no applicable method").

|                                    | `lm` | `glm` | `svyglm` | `polr`     | `multinom` | `svyolr`   |
|------------------------------------|------|-------|----------|------------|------------|------------|
| `residuals()`                      | ✓    | ✓     | ✓        | **NULL**   | matrix n×K | **NULL**   |
| `rstandard()` / `rstudent()`       | ✓    | ✓     | ✓        | —          | —          | —          |
| `hatvalues()` / `cooks.distance()` | ✓    | ✓     | ✓ ⚠      | —          | —          | —          |
| `fitted()`                         | ✓    | ✓     | ✓        | matrix n×K | matrix n×K | matrix n×K |
| `model.frame()` / `model.matrix()` | ✓    | ✓     | ✓        | ✓          | ✓          | ✓          |
| `broom::augment()`                 | ✓    | ✓     | ✓        | ✓ (4 cols) | —          | —          |

⚠ `svyglm`'s hat values sum to p and correlate 0.45 with the weight **[M]** — they fold the *working*
weights in, but they know nothing of strata or clusters (§8.3).

**The universal substrate is `fitted()` + `model.frame()` + `model.matrix()` + the observed y.** Every
panel in §6 is built from those four, plus `reg-influence.R` for the influence panel. Nothing else is
portable across the six classes.

### 2.3 What the ecosystem offers (and why it is not enough)

Measured by a subagent on a fresh install of `performance 0.17.1` / `DHARMa 0.5.0` / `see 0.14.1`,
n = 400:

| model             | panels `check_model()` returns                                                                         |
|-------------------|--------------------------------------------------------------------------------------------------------|
| `glm` binomial    | 7                                                                                                      |
| `svyglm` gaussian | 8 — **design ignored**                                                                                 |
| `svyglm` binomial | 4 — Q-Q silently dropped                                                                               |
| `MASS::polr`      | **2**                                                                                                  |
| `nnet::multinom`  | **2** (and its VIF is [known-broken, #907, open](https://github.com/easystats/performance/issues/907)) |
| `ordinal::clm`    | **1**                                                                                                  |

The `svyglm` result is the disqualifying one: *the same call returns byte-identical homogeneity and
Cook's-distance panels for a weights-only design and for a `strata = ~s, ids = ~c` design.* It looks
like it worked. `insight::model_info()` reports the clustered fit as plain `is_linear, gaussian`.

Rendering cost, measured: **~29 s for one panel at n = 1e5**, with a silent 2 000-point subsample
under the default `verbose = FALSE`.

This is not a criticism of `performance` — it is a well-built package for the model classes it
targets. It is an argument that **tabxplor's model set is outside that target**, so a dependency buys
the easy families in a second visual language and leaves the hard ones exactly where they are.

---

## 3. Maintainer rulings (2026-08-10)

| #      | Question         | Ruling                                                                                                                                                                                                        |
|--------|------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **R1** | Entry point      | **Table + data, AND accept a bare fit as a secondary form.** `reg_assumptions_plots(x, data)` is primary; a fitted `lm`/`glm`/`svyglm`/`polr`/`multinom` as `x` is the secondary form. ONE engine underneath. |
| **R2** | The numeric side | **Plots carry their verdicts as subtitles, AND `tab_reg(stats = "assumptions")` adds a compact footer block** (φ, GOF p, Brant p, max VIF) so an exported table warns without the plots.                      |
| **R3** | `or_plot()`      | **Keep the name, share the internals.** It adopts the new theme / guard / i18n seam. Zero user-visible change.                                                                                                |
| **R4** | Dependencies     | **No new Suggests except `car`** (for `vif()`'s GVIF on multi-df factors). Everything else is built on `ggplot2` + `gridExtra` (both already Suggests) and the package's own `reg-influence.R`.               |

Two consequences of R2 worth stating up front: the word *assumptions* now names two coordinated
things (a `stats =` value and a plot function), which is the desired symmetry; and the footer block
must be **rendered from the same fact table as the panel subtitles** (§5.3), or the two will drift —
the §5 disease Phase 17 spent itself removing.

### 3.1 Round-2 rulings (2026-08-11)

| #      | Question                                                    | Ruling                                                                                                                                                                                             |
|--------|-------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **R5** | The in-table miniature                                      | **Both renderings, one stored curve.** A Unicode text sparkline in the numeric predictor's own row — console, markdown, html and Excel — *and* an inline `<svg>` upgrade in the html engine. §5.7. |
| **R6** | Fitting a predictor as something other than a line          | **`shape = c(age = "quadratic")`** — a named vector over numeric predictors, the same idiom as `reference =` / `multiplier =` / `family =`. `predictors` stays a pure list of column names. §5.8.  |
| **R7** | When the assumption statistics are computed                 | **Everything, always.** No `stats = "assumptions"` opt-in gate on the block; it rides the default footer. Measured price §0.B.13. §13.                                                             |
| **R8** | What a curved predictor's row shows on the coefficient path | **The raw term rows** — one row per model term (`age`, `age²`), each with its own estimate, crude twin and adjustment colour. No refusal, no mixed estimands in a column. §5.8.3.                  |

R5–R8 interlock more than they look. R8 (two coefficient rows) is only readable because R6 emits a
**centred** parameterisation, which makes row 1 "the slope at the mean" instead of a number nobody can
read — and only *safe* because R7 puts a collinearity line in the footer that an uncentred emission
would blow up (§0.B.15). R5's miniature is affordable only because the curve it draws is the fit-free
one (§0.B.18), which is also the only one available on the jamovi digest path.

---

## 4. The public API

```r
reg_assumptions_plots(
  x,                      # a tab_reg() table, OR a fitted model (secondary form)
  data      = NULL,       # the data frame, or a survey.design -- required for the table form
  model     = NULL,       # which model, when the table holds several (see 4.2)
  check     = "auto",     # "auto" | a character vector of check names | "all"
  predictors = NULL,      # which numeric predictors get a linearity panel (default: all)
  ncol      = NULL,       # grid columns (default: derived from the panel count)
  theme     = NULL,       # "light" | "dark" | "print"  -- tabxplor's own theme vocabulary
  lang      = NULL,       # NULL (auto) | "en" | "fr"
  max_points = 2000,      # thinning cap for the raw-point layers only (never for the statistics)
  seed      = 20260810,   # randomised quantile residuals; NULL = a fresh draw each call
  ...
)
```

Returns, invisibly, the assembled `gtable` (drawn on the current device) — the same contract as
`or_plot()` and today's `lm_plots()`, so existing user code that captures the return value keeps
working.

**`data` is optional, and which panels you get depends on it** (round 2, §4.4): the checks whose
inputs the table already carries — linearity, calibration, and every footer verdict — draw from the
stored curves with no `data` and no refit. `data` unlocks the point-level panels (Q-Q, influence,
binned residuals, scale–location). Calling without it emits one message naming what was skipped and
how to get it, never a silent half-grid.

### 4.1 How the table form gets a fit — the `fit_spec` field

`reg_meta` gains **one new field, `fit_spec`** — the recipe `reg_fit()` needs beyond `data`:

```r
fit_spec = list(
  specs  = <the existing internal `specs` list, one element per model/dependent>,
  design = list(wt = , ids = , strata = , fpc = , nest = , prebuilt = <lgl>),   # NAMES only
  method = , inverse_two_level_factors = , conf_level = , na =
)
```

Measured size: **4.3 KB**, against a 23.9 KB table **[M]**. `specs` already exists and is already
strings + a formula; storing it is a move, not a new encoding.

`reg_assumptions_plots()` then calls **`reg_fit()` itself** — the same fitter the table came from.
That is the whole point: there is no second fitting path to keep in sync, the complete-case frame is
`reg_complete_frame()`'s, the design is `reg_resolve_design()`'s, the multiplier is the frozen z9 one.
Refit cost on the vignette's model: **60 ms** **[M]**.

`data` may be a `survey.design`, exactly as `tab_reg()`'s own `data` argument may be — so a
design-based table diagnoses design-correctly with no extra grammar.

**A guard is required, not optional.** If the supplied `data` no longer matches the table, every panel
is silently about a different model. The check is cheap and specific:

```
nrow(reg_complete_frame(data, drop_vars))  ==  the fit's own nobs   (already in the glance/test attr)
```

On mismatch: abort naming the discrepancy (*"`data` has 6 210 complete cases for this model; the table
was built on 6 803. Pass the data frame the table was built from."*). Never a warning — a diagnostic
plot of the wrong model is worse than no plot.

### 4.2 Which model, when the table holds several

The table can hold several models three ways, and the rule is one rule: **`model =` selects, the first
is the default, and a message names the choice when there is more than one** — the exact idiom
`or_plot(column =)` already uses.

| table shape                       | `model =` accepts | default   |
|-----------------------------------|-------------------|-----------|
| several dependents                | a dependent name  | the first |
| `predictors = list(m1 = , m2 = )` | a model label     | the first |
| `split_var =`                     | a group level     | the first |

`reg_meta` already carries `dependent`, `model_labels` and `split_var`, so the selector needs no new
metadata. For `split_var` the refit is per group, which is what makes the panels comparable to the
side-by-side columns the table shows.

### 4.3 The secondary (fitted-model) form

If `x` inherits `lm`/`glm`/`svyglm`/`polr`/`multinom`/`svyolr`, it is diagnosed directly: the family
is read from the fit rather than from `reg_meta`, `data` is ignored (the fit knows its own frame), and
everything downstream is identical. This is what `lm_plots()` could do, preserved — and it is ~10 lines,
because both forms reduce to the same internal quadruple `(fit, frame, family, weights)`.

### 4.4 The assumption ladder — the answer to "footer, curve, or plot?"

The maintainer's first question was *which assumptions belong in the footer as a statistic, which need
the user's eye, and which are worth a plot only for teaching*. The answer is not a list of eleven
checks; it is **four rungs of output**, and each check is placed by two properties of the check itself
— *does one number decide it?* and *does its input survive in the table?* A check may reach more than
one rung (linearity reaches 1 and 2), but it reaches the **lowest** rung its two answers allow, and
never a higher one merely because a plot would look good.

| rung           | what it is                                                                                 | reaches                                      | cost                      | when it fires                                         |
|----------------|--------------------------------------------------------------------------------------------|----------------------------------------------|---------------------------|-------------------------------------------------------|
| **1. Verdict** | one number + a threshold, in the footer                                                    | every export, always                         | free–277 ms               | the answer is a magnitude and a magnitude decides     |
| **2. Shape**   | a 10-bin curve stored in `meta`, rendered as a sparkline in the row and as a faceted panel | every export (text) + html (svg) + the panel | 1.5 ms/predictor          | a number says *whether*, only a curve says *how*      |
| **3. Panel**   | a point-level plot, needs `data` and a refit                                               | `reg_assumptions_plots(x, data)`             | 60 ms refit + ~1.5 s grid | the diagnostic *is* the point cloud (extremes, tails) |
| **4. Lesson**  | a panel that a well-specified model does not need                                          | `check = "all"`                              | on request                | it teaches what "fine" looks like                     |

Applying the two properties, check by check:

| check                                          |   rung    | why that rung                                                                                                                                             |
|------------------------------------------------|:---------:|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| dispersion φ                                   |     1     | φ = 1.56 *is* the finding; no plot adds to it                                                                                                             |
| collinearity (max GVIF)                        |     1     | a threshold measure by construction (4 / 10)                                                                                                              |
| separation                                     |     1     | a yes/no fact about the fit                                                                                                                               |
| fitted > 1 (`rr`)                              |   1 + 3   | the share is the verdict; the histogram is the teaching version                                                                                           |
| zero counts (poisson)                          |     1     | observed 449 vs expected 570 **[M]** — two numbers, no plot needed                                                                                        |
| parallelism (Brant)                            |   1 + 2   | the p rejects at survey n on a mild departure (p = 0.00089 for a 0.05-logit spread **[M]**), so the *curve* is what decides — rung 2 is not optional here |
| **linearity**                                  | **1 + 2** | the test says "not a line"; only the curve says "it saturates after 45", which is what tells you to write `shape = "quadratic"` rather than `"log"`       |
| calibration                                    |     2     | the test for it (Hosmer–Lemeshow) is discredited (§7.2); the curve is the recommendation                                                                  |
| influence                                      |   1 + 3   | max ‖IF‖ is a verdict; *which* observations and whether they cluster is a point cloud                                                                     |
| Q-Q / normality                                |     3     | **the canonical "needs the eye" case**: every test rejects at survey n, and the decision is about the *magnitude and location* of the departure           |
| binned residuals vs fitted                     |     3     | a point-and-band plot by nature                                                                                                                           |
| scale–location, residuals vs fitted (gaussian) |     4     | LINE's canonical teaching pair; for a well-specified model they repeat what σ and φ already said                                                          |
| the lineup / `nullabor` protocol               |     4     | the strongest teaching device in the literature, the wrong default cost (§9.2)                                                                            |

Two rules fall out, and they are the whole design:

- **A check that reaches rung 2 or 3 still keeps its rung-1 verdict.** The footer never goes quiet
  because a plot exists. That is what makes an *exported* table honest on its own.
- **Nothing on rung 1 or 2 needs `data`.** Rung 1 is a function of the fit (computed at build, stored
  in `test`); rung 2 is a function of the raw variables (computed at build, stored in `meta`). Only
  rung 3 refits. This is the answer to "is refitting the only way?" — **no, and the two rungs that
  matter most are the ones that never refit.**

---

## 5. Architecture — three primitives and one fact table

The rule this phase must obey (Phase 17 mission, rules 1 and 5): *extend a shared model, never bolt a
special case onto a call site; facts live in ONE table.* Fourteen panels written fourteen times would
be exactly the ad hoc layer the roadmap forbids. They are not fourteen panels; they are three
primitives read through one fact table.

### 5.1 Primitive 1 — `rd_bin(x, y, w, nbins)`: weighted quantile bins

Base R, `findInterval` + `rowsum`, O(n), one pass. Returns `x` (weighted bin mean of the predictor),
`y` (weighted bin mean of the response/residual), `n` (Σw), `var`, `se`.

**It is the load-bearing primitive.** Four panels are `rd_bin()` plus a label:

| panel            | `x`         | `y`               | reading                            |
|------------------|-------------|-------------------|------------------------------------|
| binned residuals | fitted      | response residual | scatter about 0 within ±2 SE       |
| empirical link   | a predictor | the observed y    | on the link scale, a straight line |
| calibration      | fitted      | the observed y    | the diagonal                       |
| mean–variance    | fitted      | the response      | `var` vs `y`, the identity line    |

Three reasons it wins over a smoother, all measured or sourced:

- **Speed.** 19 ms vs loess 370 ms (n = 6 803) and 2 030 ms (n = 21 483) **[M]**. A six-panel grid at
  survey scale would cost ~11 s in loess alone.
- **Weights.** A weighted bin mean is one line; a weighted loess is not a thing `geom_smooth()` does
  correctly. On a strongly unequal weight vector the weighted and unweighted decile curves differ by
  up to 0.019 in probability **[M]** — the difference between a population curve and a sample curve,
  which for a survey package is the whole point.
- **Honesty.** For a binary outcome the raw residual takes exactly two values given `p̂`, so a residual
  scatter is two deterministic curves carrying no information about fit (Gelman, Hill & Vehtari,
  *Regression and Other Stories* §14.5). Binning is not a rendering convenience there; it is the
  diagnostic.

**Bin count.** The ecosystem disagrees — `arm` uses `floor(sqrt(n))` with 10 / `floor(n/2)` fallbacks
below 100; `performance` and `stevemisc` use `round(sqrt(n))` unconditionally; `regressinator` copies
`arm`. Adopt **`arm`'s rule** (it is the canonical implementation and the one `regressinator`
deliberately mirrors), clamp to `[5, 60]` so a survey-scale table does not draw 146 bins, and expose
`nbins` on the primitive. Say which rule in `?reg_assumptions_plots`; the disagreement is real and a
user comparing against `arm::binnedplot()` deserves to know.

**⚠ The ±2 SE band: `arm` does not implement its own book's formula.** Verified in `arm`'s source: the
band is `2 * sd(y[bin]) / sqrt(n_bin)` (the *empirical* SE), while ROS §14.5 p.253 specifies
`2 * sqrt(p̄(1−p̄)/n_bin)` (the *theoretical* binomial SE). `arm`'s `p <- xbar` sits in the code as dead
then commented-out. Measured on correctly-specified data, n = 3 020, 54 bins: the two agree on average
(ratio 0.997) but differ **±30 % per bin**, and the coverage differs (98.1 % inside the book band vs
92.6 % inside `arm`'s).

**Ruling for the implementation: use the theoretical band `2·sqrt(p̄(1−p̄)/n_j)` where the family
supplies a variance function, and the empirical `2·sd/√n` only where it does not.** Reasons: it is
what the cited source says; it is smoother across bins and therefore reads better as a *reference*
band; and it is the one that stays correct under weights, because `p̄` is the weighted bin mean while
`sd(y[bin])` silently ignores `w`. Document the divergence from `arm` in the roxygen — this is exactly
the kind of "two encodings of one rule" the house style requires to be stated once and stated
loudly.

### 5.2 Primitive 2 — `rd_resid(fit, family, y, w)`: one residual, dispatched

| family                | residual                                                  | why                                                              |
|-----------------------|-----------------------------------------------------------|------------------------------------------------------------------|
| gaussian              | `rstandard()`                                             | the classic; `plot.lm` panel 3/5 uses the same                   |
| binomial, `rr`        | randomised quantile                                       | deviance residuals for a 0/1 outcome are near-useless (§7.2)     |
| poisson, quasipoisson | randomised quantile                                       | discreteness; catches over-dispersion directly                   |
| ordinal               | randomised quantile from the cumulative `fitted()` matrix | works, and nothing else does                                     |
| grouped binomial      | randomised quantile (binomial CDF at the counts)          | m > 1 makes deviance usable too, but one rule is better than two |
| **multinomial**       | **none — refused**                                        | order-dependent (§7.6)                                           |

The randomised quantile residual (Dunn & Smyth 1996) needs only the predictive CDF evaluated at `y`
and at `y⁻`, then `qnorm(runif(F_lo, F_hi))`. That is **one function** for four families, because
`fitted()` gives the cumulative probabilities for ordinal exactly as `ppois` gives them for a count.
Measured KS distance from uniform: binomial 0.0106, ordinal 0.0121, poisson-on-`tvhours` 0.0617 **[M]**
— the last correctly flagging real over-dispersion (φ = 1.557).

**⚠ Two implementation traps, both measured.**

- **`qnorm(1) = Inf`.** With a Poisson mean well below the observed count, `ppois(y, mu)` rounds to 1
  and the residual is `+Inf`; a naive `mean(r)` then returns `Inf` **[M]**. Clamp `u` to
  `[1e-10, 1 − 1e-10]` before `qnorm`.
- **Reproducibility.** Randomisation means the plot changes each call. Follow DHARMa's design exactly
  — a **fixed default seed, exposed as an argument, and the caller's RNG state restored afterwards**
  (`withr::with_seed`, and `withr` is already a Suggest). A fixed seed without restoration would
  hijack the user's stream, which is a bug, not a convenience. `seed = NULL` gives a fresh draw, which
  is the honest way to check that a pattern is not a randomisation artefact — worth a sentence in the
  roxygen.

### 5.3 Primitive 3 — `rd_qq(r, conf, max_pts)`: the analytic Q-Q band

Pointwise band from the beta order statistics: the i-th of n order statistics of a uniform is
`Beta(i, n−i+1)`, so the band is `qnorm(qbeta(α/2, i, n−i+1))` … `qnorm(qbeta(1−α/2, i, n−i+1))`.

**28 ms for all n points, 9 ms thinned to 300 — against 1 182 ms for a 19-replicate simulated
envelope** **[M]**, and the two agree to 0.19 on the most extreme order statistic **[M]**.

⚠ It is **pointwise**, not simultaneous: under a true model roughly 5 % of points fall outside *at
each position*, so a handful of excursions is expected. Say so in the subtitle, not only the docs.

### 5.4 The fact table — `REG_CHECKS`

One row per check. This is the `MEASURES` / `REG_EMPIRICAL` pattern, and it is what makes R2's footer
block impossible to drift from the panel subtitles: **both read this table.**

| column          | meaning                                                                                                                                                                 |
|-----------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `key`           | `"linearity"`, `"binned_resid"`, `"qq"`, `"calibration"`, `"dispersion"`, `"zeros"`, `"influence"`, `"collinearity"`, `"parallel"`, `"separation"`, `"predicted_range"` |
| `families`      | the families it applies to (a character vector; the gate)                                                                                                               |
| `assumption`    | the assumption it tests, **named the way the source names it** (§9.1)                                                                                                   |
| `panel`         | the builder function                                                                                                                                                    |
| `stat`          | the numeric verdict function — `NULL` where the check is graphical only                                                                                                 |
| `verdict`       | thresholds → `"ok"` / `"warn"` / `"bad"`                                                                                                                                |
| `caption`       | the pedagogical one-liner, `gettext`'d                                                                                                                                  |
| `per_predictor` | logical — does this check produce one panel per numeric predictor?                                                                                                      |
| `needs`         | what it requires (`"design_free"`, `"m_gt_1"`, `"count"`, …) — the refusal gate (§8, §12)                                                                               |
| `footer`        | logical — does `stats = "assumptions"` print it?                                                                                                                        |

Adding a check is then **one row**, exactly as adding a colour measure became one `MEASURES` row in
17d. The `stat` + `verdict` columns are what `tab_reg(stats = "assumptions")` renders; `panel` +
`caption` are what the grid renders; neither can say something the other does not.

### 5.5 Grid assembly

`gridExtra::arrangeGrob()` — already a Suggest, already what `lm_plots()`/`or_plot()` return, and it
returns a `gtable` so the existing test idiom (`expect_s3_class(..., "gtable")`) carries over.

**Facets vs grid — both, in their proper places.** Heterogeneous panels (Q-Q beside a bar chart) cannot
be facetted; they go in the grid. But the **per-predictor linearity panels are homogeneous** — same
geometry, same y-scale meaning, one per numeric predictor — so they are ONE `ggplot` with
`facet_wrap(~ predictor, scales = "free_x")`. That is both prettier and cheaper, and it is the
maintainer's *"facets or grid_arrange in a visually clear way"* answered precisely: facets within a
check, grid across checks.

### 5.6 Where the numbers live — three stores that already exist

The maintainer's third question: *what is the most user-friendly way to store the assumption data — is
refitting the only way; can the curves be computed in `tab_reg()` and stored in the summary-stats
tibble; would that lose the individual points that are sometimes needed?* Measured answer: **three
stores, all of them already in the package, and none of them new.**

| what                  | store                                                      | shape                                                   | size                                                                                   | who reads it                                    |
|-----------------------|------------------------------------------------------------|---------------------------------------------------------|----------------------------------------------------------------------------------------|-------------------------------------------------|
| rung-1 verdicts       | the **`test`** attribute (one row per model column × stat) | the existing `new_test_tibble()` schema                 | ~10 rows                                                                               | the footer, the panel subtitles                 |
| rung-2 curves         | **`meta$assumptions`**                                     | `list(<model col> = list(<var> = tibble(x, y, n, se)))` | **1.6 KB per curve**; 4 numeric × 3 models = **19.5 KB** against a 32 KB table **[M]** | the sparkline, the linearity/calibration panels |
| the recipe for rung 3 | **`meta$fit_spec`** (§4.1)                                 | strings + a formula                                     | **4.3 KB** **[M]**                                                                     | `reg_assumptions_plots()` → `reg_fit()`         |

Three consequences worth stating, because each closes a question the maintainer asked:

1. **Refitting is not the only way, and it is not the main way.** Rungs 1–2 — the linearity verdict,
   its curve, dispersion, collinearity, calibration, parallelism — are computed *inside the build*,
   from the fit that already exists and the raw columns that are already in hand, and travel with the
   table into every export. The refit exists only for the point-level panels (§4.4 rung 3).
2. **Storing the curve loses nothing that a curve can show, and it is what makes the miniature free.**
   The binned curve *is* the diagnostic for the shape question — the point cloud adds nothing to it
   (for a binary outcome the raw residual takes exactly two values given p̂, §5.1). What the points are
   genuinely needed for is Q-Q and influence — which is exactly what rung 3 keeps. So the split is not
   a compromise, it is the honest boundary between "the shape of a relationship" and "the behaviour of
   individual observations".
3. **The curve is computed once per (model column × numeric predictor), never per rendering.** In a
   model-comparison table (`predictors = list(m1 = , m2 = )`) the *raw* empirical curve of `age` is the
   same object in every model that contains `age` — it does not depend on the model at all
   (§0.B.18: it is the fit-free curve). So it is computed once per predictor and referenced by each
   model column, and a 5-model comparison stores five references to one 1.6 KB tibble, not five copies.
   The rung-1 *verdicts* do depend on the model and are per column, as every `test` row already is.

⚠ **The jamovi digest path** (`reg_build_digest`, which deliberately keeps no model frame) can compute
the rung-2 curves — they need only the raw columns — but not the rung-1 statistics that read the fit.
That is the existing `needs` gate, one more value: `needs = "fit"`. It degrades to curves + a note, and
that is a better degradation than the alternative, because the curve is the item the live UI most wants.

### 5.7 The miniature (R5) — one curve, two renderings

**Where it goes.** A numeric predictor's `levels` cell is built at `tab_reg.R:3624-3639`, where the
multiplier already relabels it (`age` → `age (per 1 SD (13.5))`). The sparkline is appended **at that
same site, in that same loop** — it is one more thing the row label says about a numeric predictor, and
it is therefore plain text in a plain character column, which is why it reaches every backend with no
export machinery whatsoever:

```text
  var     levels                         n   Model_OR   Obs_OR
  age     age (per 1 SD) ▁▄▇▇█████▇  12 960   1.35***   1.46***
  race    White                       8 106       ref      ref
  race    Black                       2 517  1/2.40*** 1/2.68***
```

**The text rendering.** Eight block glyphs `▁▂▃▄▅▆▇█`, 10 quantile bins, min–max rescaled: 10
characters, 30 bytes **[M]**. Rescaling is *within the predictor*, so the sparkline answers "is it a
line?", never "is the effect big?" — the estimate column answers that, and conflating the two would be
the classic dual-encoding error. Legibility was measured, not assumed (§0.B.17): a straight line and a
saturating curve are distinguishable at a glance, and so is noise (`▃▁▄▁▅█▂▅▅▃`).

**The html upgrade.** `render_kable_html()` swaps the glyph run for a 121-byte inline
`<svg><polyline>` **[M]** built from the same stored bins, with the stroke inherited from
`currentColor` so `tab_css()`'s light/dark/print themes carry it for free (z11's palette applies
without a single new colour rule).

⚠ **Two implementation traps, both real:**

- **The html engine escapes label cells.** `html_escape_br()` (`tab-render-html.R:131`) HTML-escapes
  everything and then un-escapes exactly `<br>`. An `<svg>` written into the level label would render as
  literal text. The fix is one more un-escape in **that same function** — not a second raw-cell path —
  keeping "one place decides what markup of ours survives escaping".
- **Fonts.** Block glyphs need a font that has them (skimr documents Windows console failures and ships
  `fix_windows_histograms()` for exactly this). tabxplor needs the same escape: an option
  (`tabxplor.spark`, default `TRUE`) and an ASCII fallback. Excel is fine (Calibri has the block), and
  LaTeX/PDF via pandoc is the one place to expect trouble.

**What it is not.** Not a second computation (§5.6.3), not a new column (no table gets wider), not a
new fmt display token (the `levels` column is character, not `fmt`, so `format()` remains the only
string producer for *values* — the export-parity contract is untouched). And honestly: **no statistical
software does this today** (MATLAB puts sparklines in Live Script table *headers*; gt/kableExtra put
them in dedicated *columns*), so it is a real novelty, with the review risk that carries.

### 5.8 `shape =` (R6) — fitting a predictor as something other than a line

The plots find a non-linearity; `shape =` is how the user fixes it without leaving the framework.
Today they cannot: `predictors = c("race", "poly(age, 2)")` fails with *"objet 'poly(age, 2)'
introuvable"* **[M]**, and the formula escape hatch that does work silently disables `empirical =`,
`color = "adjustment"`, `multiplier`, and the per-predictor global test, while printing rows a reader
cannot interpret (`poly(age, 2) | 1`).

#### 5.8.1 The grammar

```r
tab_reg(gss_simple, "married", c("race", "age", "rincome"),
        shape = c(age = "quadratic"))        # named over numeric predictors; the rest stay linear
```

| value                     | emitted terms                       | rows added | why it is in the set                                           |
|---------------------------|-------------------------------------|------------|----------------------------------------------------------------|
| `"linear"` (default)      | `z`                                 | 1          | today's behaviour, byte-identical                              |
| `"quadratic"`             | `z + I(z^2)`                        | 2          | the standard remedy; the one every source names                |
| `"log"`                   | `I(log(x))`                         | 1          | diminishing returns — the other shape social data actually has |
| `"sqrt"`                  | `I(sqrt(x))`                        | 1          | the count-data cousin of `log`                                 |
| `"cut5"` / an integer `5` | a 5-level factor of quantile groups | 5          | **the sociologist's remedy**, see 5.8.4                        |

where `z = (x − x̄)/s` with **x̄ and s frozen as literals in the formula** at build time, exactly as z9
freezes the `multiplier`. For `"log"` and `"sqrt"` it is the *transformed* column that is centred and
scaled (`z = (log x − mean log x)/sd(log x)`), so `multiplier = "sd"` keeps meaning "per 1 SD of the
term as fitted" for every shape — one rule, and the row label stays true. Everything else is out —
see 5.8.5.

#### 5.8.2 Why it integrates with nothing new

`shape` changes *which terms a predictor emits*, and nothing else. The predictor remains **one
predictor**, which is the property every downstream subsystem keys on:

- **the skeleton** — `reg_skeleton()` already emits one row per term for a factor; a curved numeric
  emits two rows with the same `var`, which is the same shape.
- **the crude twin (z9)** — `reg_empirical_fit()` refits *one predictor* with the model's own family,
  design, CI method and multiplier; give it the same shape and its **term names are identical to the
  model's** **[M]**, so `reg_skel_match()` aligns them unchanged.
- **`color = "adjustment"` and the z8-B gap test** — per term, both legs on the same scale, same rows:
  `reg_coef_if_maker()` needs no change. Measured on the vignette model: model OR 1.577 vs crude 1.638
  for the slope at the mean **[M]**.
- **`effect = "ame"`** — one row per predictor, correct to the g-computation truth **[M]**.
- **`multiplier`** — absorbed by the centring: both terms are already per-SD.
- **jamovi** — the picker gains one dropdown per numeric predictor beside the existing scaling one; the
  digest/reref contract is untouched, because a shape change is a *different model*, hence a different
  cache key, not a reparametrisation.

⚠ **The one rule the implementation must state once, because it is the only 1-to-1 that breaks:** the
skeleton emits **one row per model TERM on the coefficient path, one row per PREDICTOR on the marginal
path**. Today those coincide (a numeric predictor is one term; a factor level is one term). A curved
predictor is the first case where they differ — two coefficient rows, one AME row — so
`reg_skeleton()` must take the effect into account, and `reg_marginal_column()`'s `(var, level)` key
must find `age`, not `age (per 1 SD, at the mean)`. Writing it as a rule, not as an `if`, is what keeps
it from becoming the next §2 disease.

#### 5.8.3 What the table shows (R8) — and why centring is what makes it readable

Two rows, each with its own estimate, crude twin, stars and adjustment colour:

```text
  var    levels                                    Model_OR    Obs_OR
  age    age (per 1 SD, at the mean) ▁▄▇▇█████▇       1.58***   1.64***
  age    age² (curvature)                             0.75***   0.73***
```

Both numbers are readable **because the emitted variable is centred and scaled**: row 1 is the odds
ratio per 1 SD *at the mean of age*, row 2 says the slope shrinks as you move away from it. The
uncentred emission prints 1.184 and 0.99841 **[M]** — a number nobody can read and a number that looks
like nothing. And centring is not merely nicer: it takes the collinearity from **VIF 38.7 to 1.2**
**[M]**, without which R7's new footer line would flag every curved model as broken.

The curvature row's sign is the finding: `< 1` = the effect flattens or turns over, `> 1` = it
accelerates. The vertex (age 52.2 **[M]**) belongs in the panel subtitle, not in a cell.

#### 5.8.4 `cut` — the remedy the package renders best

Worth its own note, because it is the one that costs *nothing at all*: turning `age` into 5 quantile
groups makes it a **factor**, so it inherits the entire factor machinery — one OR per group, a
*saturated* crude twin (the exact observed contrast, not a univariable fit), per-level N, per-level
colours, adjustment gaps, Woolf intervals — and the non-linearity becomes visible **in the printed
numbers themselves**, with no new estimand and no `marginaleffects` involvement. For tabxplor's
audience (a literary-studies student reading an OR table) this is the more teachable fix, and the reg
vignette already gestures at it (*"something worth checking (with `cut()`, or splines)"*). It should be
the remedy the vignette teaches **first**, with `"quadratic"` as the parsimonious alternative.

#### 5.8.5 What is deliberately excluded — and one of them is a wrong number, not a preference

- **`poly()` and `ns()`/`bs()` are never emitted.** §0.B.14: `marginaleffects` returns **AME = 0.000000**
  for them, silently, because the basis is re-evaluated on the perturbed data and an orthogonal basis
  absorbs a location shift exactly **[M]**. This is not a limitation to work around — it is a wrong
  number that would reach a user's table, and the package's rule is to return nothing rather than a
  wrong number. Splines therefore stay available **only** through the formula escape hatch, where
  §17.4's guard must warn.
- **Cubic and higher.** A white elephant for this audience: three rows nobody can read, and the honest
  answer at that point is `cut` or a spline in a different tool.
- **Arbitrary expressions in `predictors`.** Rejected in R6: every site that keys on a predictor *name*
  (skeleton, crude refit, multiplier, reference, `split_var`, the jamovi picker) would have to learn to
  parse and re-emit them, and the name the user reads becomes an expression.
- **`scale()` inside the formula.** Measured trap: prediction on a subset re-scales with the subset's
  own mean, so `predict(newdata =)` disagrees with the fit **[M]**. This is why the centring constants
  are frozen as literals.

#### 5.8.6 The formula escape hatch — keep, and fix two things

It stays (interactions have no other route, and Phase 17 already ruled "keep"). Two defects to fix in
the same session, both measured:

1. **The refusal message blames the wrong thing.** `tab_reg(gss, married ~ race + poly(age,2),
   empirical = TRUE)` says *"`empirical` … is not available for any of these outcome families"* on a
   **binomial** model **[M]**. The cause is the compound formula (`reg_crude_key(compound = TRUE)` →
   `NA`), not the family. One `cli` branch (`tab_reg.R:5045`).
2. **`poly()`/`ns()` + `effect = "ame"` must warn.** Today it is a coin flip: the same model returns the
   correct AME through tabxplor and 0 through a direct `marginaleffects` call **[M]**, decided by
   whether `insight` can recover the data. A cheap, exact guard exists — compare the returned AME with
   the g-computation difference `mean(predict(x + k)) − mean(predict(x))`, two lines — and refuse on
   disagreement.

---

## 6. The panels, per family

`auto` selects the rows of `REG_CHECKS` whose `families` matches and whose `needs` are satisfied.
Order matters: **functional form first**, because it is the check that changes what the table means.

| #  | check                                          |    gaussian    |     binomial      |    poisson/quasi     |         `rr`         |   grouped binom   |       ordinal       |    multinomial    |
|----|------------------------------------------------|:--------------:|:-----------------:|:--------------------:|:--------------------:|:-----------------:|:-------------------:|:-----------------:|
| 1  | **linearity** (per numeric predictor, faceted) |  ✓ resid vs x  | ✓ empirical logit | ✓ log empirical mean | ✓ log empirical mean | ✓ empirical logit | ✓ cumulative logits | ✓ baseline logits |
| 2  | **binned residuals** vs fitted                 |       ✓        |         ✓         |          ✓           |          ✓           |         ✓         |          —          |         —         |
| 3  | **Q-Q** of the residual                        | ✓ standardized |    ✓ quantile     |      ✓ quantile      |      ✓ quantile      |    ✓ quantile     |     ✓ quantile      |    **refused**    |
| 4  | **calibration** (predicted vs observed)        |       —        |         ✓         |          —           |          ✓           |         ✓         |      ✓ per cut      |  ✓ per category   |
| 5  | **scale–location**                             |       ✓        |         —         |          —           |          —           |         —         |          —          |         —         |
| 6  | **mean = variance**                            |       —        |         —         |          ✓           |          —           |         —         |          —          |         —         |
| 7  | **zero counts** obs vs expected                |       —        |         —         |          ✓           |          —           |         —         |          —          |         —         |
| 8  | **influence** ‖IF‖                             |       ✓        |         ✓         |          ✓           |          ✓           |         ✓         |          ✓          |         ✓         |
| 9  | **collinearity** (VIF)                         |       ✓        |         ✓         |          ✓           |          ✓           |         ✓         |          ✓          |         ✓         |
| 10 | **parallel lines** (proportional odds)         |       —        |         —         |          —           |          —           |         —         |          ✓          |         —         |
| 11 | **predicted range** (fitted > 1)               |       —        |         —         |          —           |          ✓           |         —         |          —          |         —         |

Row 8 is the row no other package can draw for the last two columns (§0.4). Row 11 exists because the
modified Poisson's fitted "probabilities" genuinely can exceed 1 — measured max **1.004**, 0.01 % of
rows, on `married ~ race + age + rincome + relig` **[M]** — and that is a real, plottable diagnostic
rather than a theoretical caveat.

Default grid: `check = "auto"` draws rows 1–3 plus the family-specific rows and row 8, capped at 6
panels; `check = "all"` draws every applicable row. `check = c("linearity", "qq")` draws exactly those.

### 6.1 Shared framework vs family-specific — and the seam is one fact that already exists

The maintainer's second question: *which assumptions are common to most families and deserve their own
framework, and which are specific and should ship only with their family?* The table above answers it
by rows, but the architectural answer is sharper than "these four are shared":

**Four checks are shared, and they differ across families by exactly ONE parameter — the link.**

| shared check | what varies with the family                                                                   | where that variation already lives                         |
|--------------|-----------------------------------------------------------------------------------------------|------------------------------------------------------------|
| linearity    | the y-scale the curve is drawn on: identity / logit / log / cumulative logit / baseline logit | **`REG_EMPIRICAL$link`** — the per-shape column z8-B added |
| calibration  | the same link, inverted                                                                       | the same                                                   |
| influence    | nothing (the IF is family-general by construction)                                            | `reg-influence.R`, four makers already dispatched          |
| collinearity | nothing (a property of the design matrix)                                                     | `car::vif`, one refusal (multinom, §0.B.19)                |

That is the whole cross-family framework: **one binning primitive + one link column**. The link is not
a new fact to invent — `REG_EMPIRICAL` gained `link` in z8-B *per shape row, not per family*, precisely
because a binomial model's crude twin is logit by default, identity under `effect = "ame"` and log
under `"ame_ratio"`. The linearity panel's y-transform is the *same* fact, read by a second consumer.
This is the difference between adding a framework and finding one.

**Six checks are family-specific, and each is specific for a stated reason** — never "we only
implemented it there":

| check                        | family                     | the reason it exists only there                                                                              |
|------------------------------|----------------------------|--------------------------------------------------------------------------------------------------------------|
| mean = variance (dispersion) | poisson, grouped binomial  | φ ≈ mean(1−μ) by construction for Bernoulli — measured 0.997 **[M]**, a constant of the fitted values (§7.4) |
| zero counts                  | poisson                    | undefined without a count                                                                                    |
| parallel lines (Brant)       | ordinal                    | the proportional-odds assumption exists nowhere else                                                         |
| predicted range              | `rr`                       | only a log link on a binary outcome can predict > 1 — measured max 1.004 **[M]**                             |
| normality, equal variance    | gaussian                   | for every other family the variance IS a function of the mean (`performance` #376)                           |
| separation                   | probability-scale families | a perfect predictor is a likelihood pathology of logit/MNL/ordinal                                           |

And **one check is refused everywhere it would be order-dependent**: no residual panel for multinomial
(§7.6, cor = −0.705 between level orderings **[M]**).

### 6.2 Naming — one word for the assumption, the test in parentheses

The maintainer's fifth question: *is there a concise, preferably one-word way to say what a test is
about, since test names are cryptic?* Yes — and the package already invented the convention in Last
Phase m, Item 7, for the crosstab summary: **the test type moved into the row NAME**
(`"pvalue (Chi2, Welch F; Kish)"`, `"effect size (Cramér's V, eta2)"`). The assumption block follows
it exactly, so there is one convention in the package rather than two:

| row label (EN)                      | FR                      | the cryptic name it replaces                                      |
|-------------------------------------|-------------------------|-------------------------------------------------------------------|
| `Linearity (Wald)` / `(LR)` / `(F)` | `Linéarité`             | "curvature test", "Tukey's test for nonadditivity", "Box–Tidwell" |
| `Collinearity (max VIF)`            | `Colinéarité (VIF max)` | "generalized variance inflation factor"                           |
| `Dispersion (Pearson)`              | `Dispersion`            | "φ̂", "overdispersion parameter"                                  |
| `Parallelism (Brant)`               | `Parallélisme (Brant)`  | "proportional-odds test"                                          |
| `Influence (max)`                   | `Influence (max)`       | "Cook's distance", "dfbeta", "‖IF‖"                               |
| `Separation`                        | `Séparation`            | "quasi-complete separation", "Hauck–Donner"                       |
| `Calibration`                       | `Calibrage`             | "Hosmer–Lemeshow" (which we do not compute, §7.2)                 |
| `Normality`                         | `Normalité`             | "Shapiro–Wilk", "KS"                                              |

Three rules make it work rather than merely look tidy:

1. **The noun is the assumption, never the plot type or the statistic.** `Linearity`, not "empirical
   logit plot"; `Parallelism`, not "Brant test". The student meets the noun in the footer, in the panel
   title, and in the vignette's section heading — three places, one word.
2. **The parenthesis names the test only when it varies.** `Linearity (Wald)` on a design, `(LR)`
   unweighted, `(F)` for gaussian/quasipoisson — because that *is* what `reg_term_tests()` dispatches
   (§13), and hiding it would make two identically-labelled rows carry different p-values.
3. **The noun is `REG_CHECKS`'s `assumption` column and the `check =` value is its `key`** (§5.4), so
   the footer row label, the panel title, the argument value and the translation msgid all derive from
   one row. `check = "linearity"` and the row that says `Linearity` cannot drift apart. The panel-only
   keys (`binned_resid`, `qq`, `calibration`) carry the same nouns — `Normality` is what the Q-Q panel
   is *about*, `qq` is only how it is drawn.

⚠ **`Independence` is deliberately absent.** BeyondMLR is explicit that no residual plot evaluates it,
and jamovi's Durbin–Watson row is meaningless for survey data (rows are not a time series). It gets a
sentence in the vignette, never a row and never a panel.

---

## 7. The statistics, family by family

### 7.1 Gaussian — LINE

BeyondMLR ch. 1 §1.3 gives the only mnemonic in the whole book, and the panel captions should use it
verbatim: **L**inear relationship, **I**ndependent errors, **N**ormally distributed responses at each
X, **E**qual variance. The book's own mapping, quoted: *"The upper left plot, Residuals vs. Fitted, can
be used to check the Linearity assumption… Normal Q-Q … the Normality assumption… Scale-Location … the
Equal Variance assumption… Residuals vs. Leverage … influential points."* And, importantly, *"There is
typically no residual plot to evaluate the Independence assumption"* — so **I** gets a caption saying
that, not a panel pretending to test it.

⚠ **`plot.lm` uses three different residuals across its four panels** (verified in the R source):
panel 1 raw (Pearson for glm), panel 2 standardized **deviance**, panels 3 and 5 standardized
**Pearson**. Nearly every ggplot2 reimplementation gets at least one wrong. Our dispatcher (§5.2) uses
one residual per family throughout, which is a deliberate divergence — simpler, defensible, and it
must be **documented as a divergence** rather than left to be discovered.

### 7.2 Binomial — why the standard panels are useless here

*"The data y are discrete and so are the residuals… As a result, plots of raw residuals from logistic
regression are generally not useful"* (ROS §14.5). Hence rows 2 and 4 replace the gaussian panels, and
row 3 uses a quantile residual.

`performance`'s maintainers reached the same conclusion in their own issue
[#376](https://github.com/easystats/performance/issues/376): for binomial models the constant-variance
plot *"should be omitted"*, the residual Q-Q is *"hard to interpret"*, and the linearity check should
be `binned_residuals()`. That is exactly the row-2/row-5 gating in §6, arrived at independently.

**The empirical logit plot** (row 1) is BeyondMLR's core logistic check, and the book uses **three
mutually inconsistent zero-handling strategies** across its chapters: Haldane `(Y+0.5)/(n+1)` (ch. 6
binomial), a hard floor `ifelse(p == 0, .01, p)` (ch. 6 binary — which notably does *not* handle
`p == 1`, so an all-success bin yields `Inf`), and dropping sparse bins (ch. 11).

**Use Haldane–Anscombe `(k + 0.5)/(n + 1)` uniformly.** It is symmetric (the ch. 6 binary version is
not), it never produces an infinity, it needs no arbitrary floor constant, and it is the one the book
uses where it is being careful. Verified on the vignette's data: no bin needs it at 10 deciles, so it
costs nothing in the common case **[M]**.

**The reference line must be a straight `lm`, not a loess.** The book does this deliberately and it is
a pedagogical point, not an aesthetic one: the assumption *is* linearity, so the comparator must be
the straight line the model assumes. A loess through the points would trace the curvature and hide the
very departure the panel exists to reveal.

**Hosmer–Lemeshow is deliberately excluded.** It is arbitrary in `g` (Allison's canonical example: the
same model gives p = 0.11, **0.0499**, 0.64 for g = 9, 10, 11), it rejects everything at survey n, it
does not say *where* the miscalibration is, and it is computed in-sample so it can pass an overfitted
model. Harrell: *"obsolete due to low power, arbitrariness, and not penalizing sufficiently for
overfitting."* Van Calster et al. (STRATOS, BMC Medicine 2019): it *"gives a P value that is
uninformative with respect to the type and extent of miscalibration."* Stanley Lemeshow himself
co-authored the calibration-belt papers that replace it. Note that three of the four R implementations
document it with **no methodological caveat at all**, and `performance_hosmer()`'s Details actively
instruct the user to accept the null (*"the reported p-value should be greater than 0.05"*).

Row 4 (the calibration panel) is the replacement, and it reaches the level Van Calster et al. call
**moderate calibration** — the practical target — without a test, a binning-dependent p-value, or a
new dependency.

### 7.3 Poisson — the four numbered assumptions

BeyondMLR ch. 4 §4.2.1 names them and gives **no** acronym: **1. Poisson Response**, **2.
Independence**, **3. Mean = Variance**, **4. Linearity**. Use those labels verbatim (§9.1).

Row 1 is the book's *"log of the empirical means"* plot: group the response by the predictor, take
`log(mean)`, plot against the predictor. Verbatim rationale, worth reusing as the caption: *"Our best
guess of λᵢ is the observed mean number … for each age. Because these means are computed for observed
data, they are referred to as empirical means. Taking the logs of the empirical means and plotting by
age provides a way to assess the linearity assumption."*

Row 6 the book renders as a **table**, not a plot. As a plot it is bin-variance against bin-mean with
the identity line — same information, and it composes with `rd_bin()`, which already returns `var`.

Row 7 is the book's rootogram substitute: observed zero count against `Σ dpois(0, μ̂ᵢ)`. Measured on
`tvhours ~ age + race`: 449 observed zeros against 570 expected **[M]** — i.e. *under*-representation
of zeros, the opposite of zero-inflation, which is a useful demonstration that the panel is not a
one-way alarm.

Dispersion `φ = Σ(Pearson²)/(n − p)`, threshold 1. Measured 1.557 on that model **[M]**. The book's
rule, verbatim: *"if there is no overdispersion, this estimate should be close to one. It will be
larger than one in the presence of overdispersion."*

### 7.4 Modified Poisson (`rr`) — where the book's rules invert

z3's `rr` arm fits a log link to a 0/1 outcome with a sandwich variance. Two of the standard checks
change meaning:

- **Dispersion is meaningless.** A Bernoulli response has `φ ≈ mean(1 − μ)` by construction, so a
  value below 1 is the *expected* state, not under-dispersion. Measured φ = 0.997 for the binomial fit
  on the same data **[M]**. Row 6 must be refused for `rr` and for binomial — the `needs = "count"`
  gate.
- **Predicted values above 1 are the real diagnostic** (row 11). Measured range 0.136 … **1.004** with
  0.01 % of rows above 1 **[M]**. A small excess is normal and is the documented price of the
  estimator; a large one means the log link is wrong for this data. The panel is a histogram of fitted
  values with a rule at 1 and the share above it in the subtitle.

Row 1 is the book's `log(empirical mean)` plot, which for a binary response is `log(p̂)` — a genuinely
different linearity check from the logit one, and the correct one for a log link.

### 7.5 Ordinal — the parallel-lines check the package half-owns

`tab_reg()` already computes the **Brant** proportional-odds test and stashes it as
`attr(fit, "brant_po")`. Row 10 is its graphical companion, and the two belong on the same panel: the
empirical cumulative logit `log(P(Y ≤ k)/P(Y > k))` per predictor level, one line per cut. Parallel
lines ⇔ proportional odds.

Measured on `rincome ~ race + age`: the spread of the cumulative logit across `race` is 0.462 / 0.458 /
0.555 across the three cuts (sd of the spreads 0.054), while the Brant omnibus gives **p = 0.00089**
**[M]** — the test rejects on a departure the eye would call mild, which is exactly the large-n
pathology (§9.3) and exactly why the plot must be shown beside the p-value rather than instead of it.

Row 3 works: the ordinal quantile residual from the cumulative `fitted()` matrix has KS distance
0.0121 from uniform on a well-specified fit **[M]**.

### 7.6 Multinomial — one refusal, stated as such

**A randomised quantile residual for a NOMINAL outcome is not well defined**, because it requires an
ordering of the categories and the outcome has none. Measured: refitting the same model with the
category order reversed gives residuals correlated **−0.705** with the originals **[M]**. The KS
statistic happens to be nearly invariant (0.0104 vs 0.0079), so a *test* would look stable while every
*plot* of residuals against a predictor would be an artefact of the level order.

So: **no residual panel, no Q-Q, for multinomial.** What it gets instead:

- **row 4, calibration per category** — predicted probability against observed frequency, faceted by
  category. Well defined, order-free, and it reads directly.
- **row 1, baseline-category empirical logits** — `log(p̂_j / p̂_ref)` against each numeric predictor,
  faceted by category. This is the multinomial model's own linear predictor, so a straight line is
  again the right comparator.
- **row 8, influence** — via `reg_score_multinom()`, where base R has nothing at all.

⚠ **IIA is not tested.** The Hausman–McFadden test is known to be unreliable (it can produce negative
test statistics and its conclusions depend on which alternative is dropped) and the package has no
existing machinery for it. Stating that IIA is assumed and untested is more honest than a panel
implying otherwise. Flag for the vignette, not the plot.

### 7.7 Grouped binomial (`trials =`)

`m > 1` makes two things possible that Bernoulli forbids: the deviance goodness-of-fit test is valid
(*"when the denominators mᵢ are large and a model fits, the residual deviance follows a χ² with n − p
degrees of freedom"*, BeyondMLR §6.5.6), and over-dispersion is a real quantity (the beta-binomial
case; the book's ch. 7 measures φ̂ = 0.894 vs 6.858 for a binomial vs a beta-binomial simulation).

The `needs = "m_gt_1"` gate turns both on. The book's own reading of the residual panel is a good
caption: *"This kind of plot for binomial regression would produce two linear trends with similar
negative slopes if there were equal sample sizes mᵢ for each observation."*

---

## 8. Weights and survey designs — four measured rules

This is where a naive implementation ships wrong numbers, so each rule below is a measurement.

### 8.1 Residuals are design-blind, and that is correct

Fitting `y ~ age + race` through `svyglm` on a weights-only design and on a
`strata = ~s, ids = ~psu, nest = TRUE` design gives **identical residuals** (`all.equal` TRUE at 1e-10)
and SEs of 0.001130 vs 0.001141 **[M]**. The design enters the *variance*, never the point estimates.

So every residual panel is design-invariant. That is not a limitation to apologise for; it is the
correct behaviour, and saying it explicitly in `?reg_assumptions_plots` prevents the reasonable-sounding
bug report that the panels "ignore the design".

### 8.2 The binning must be weighted

Weighted and unweighted decile curves on the same data (weights `exp(N(0, 0.8))`) differ by up to
**0.019** in probability **[M]**. For a survey package the weighted one is the estimand — the
population curve, not the sample curve. `rd_bin()` takes `w` for this reason, and it is the reason a
`geom_smooth()` overlay is not an acceptable substitute (§5.1).

### 8.3 Dispersion must be refused for a design-based fit — the `df.residual` trap

**[M]**, same data, same formula:

| fit                                 |      n | `df.residual` |  n − p | `Σ(Pearson²)/df.residual` |
|-------------------------------------|-------:|--------------:|-------:|--------------------------:|
| `glm` binomial                      | 21 407 |        21 403 | 21 403 |                  **1.00** |
| `svyglm`, `ids = ~1`                | 21 407 |        21 403 | 21 403 |                  **1.00** |
| `svyglm`, `strata = ~s, ids = ~psu` | 21 407 |       **949** | 21 403 |                 **22.49** |

`df.residual(svyglm)` on a clustered design is the **design** degrees of freedom (nPSU − nStrata), so
the ratio is inflated by exactly n/nPSU. A dispersion panel that did not know this would scream
over-dispersion on a perfectly well-specified survey model. `needs = "design_free"` gates it off.

### 8.4 The influence panel is the one that *is* design-aware

`reg_if_se(IF, fit$survey.design)` on the clustered design returns **0.0011408**, against
`sqrt(vcov(svyglm)[2,2])` = **0.0011408** and the IID version 0.0011299 **[M]**. That is
`survey::svyrecvar()` doing its job through machinery the package already owns and already tests
(z8-B).

So the influence panel plots `‖IF_i‖` — the design-based influence of observation i on the coefficient
vector — where base R would plot a design-blind Cook's distance, or, for `polr`/`multinom`, nothing at
all. Label the axis `‖influence‖` rather than "Cook's distance": it is a different quantity (an exact
one-step influence, not a leave-one-out refit) even though it reproduces `dfbeta` to correlation
1.0000 **[M]**.

⚠ A prebuilt `svyrep.design` needs `withReplicates`, not `svyrecvar` — z8-B already degrades there, and
the panel degrades the same way (IID influence, and a subtitle saying so).

---

## 9. Pedagogy — the plot as a teaching instrument

The maintainer's requirement: *"pedagogical, with a meaningful title, translated in French, as a good
teaching instrument for literary students, and visually polished."* Four concrete consequences.

### 9.1 Every panel says what it tests, in the source's own words

Title = the assumption, not the plot type. `"Linéarité : le logit varie-t-il régulièrement avec
l'âge ?"` beats `"Empirical logit plot"` for the intended reader. Subtitle = the verdict, from the
`REG_CHECKS` `stat`/`verdict` columns, so it is a *number with a reading*, not a number:

```
Linéarité — âge
Une droite ne suffit pas : la courbe monte puis redescend (ΔAIC = 1251).
```

The assumption **names** come from the sources verbatim, because the student will meet them again:
LINE for gaussian; *Poisson Response / Independence / Mean = Variance / Linearity* for Poisson; *Binary
Response / Independence / Variance Structure / Linearity* for logistic (BeyondMLR §4.2.1, §6.2.1 — and
note the book has **no acronym** for the latter two, so inventing one would be a disservice).

### 9.2 Show what "fine" looks like, on the plot

Every panel carries its reference: the ±2 SE band (rows 2, 4), the analytic Q-Q band (row 3), the
identity line (rows 4, 6), the straight `lm` line (row 1), the VIF thresholds at 4 and 10 (row 9).
The band *is* the "what noise looks like" device, at 28 ms instead of 1 182 ms for a simulated
envelope (§5.3) — and it is a better teaching object than a lineup, because it is on the same panel as
the data rather than requiring the reader to compare nineteen small multiples.

A lineup / `nullabor`-style protocol is genuinely the strongest pedagogy in the visual-inference
literature (Buja et al.), and it is **deliberately not the default**: 19 extra panels per check is the
wrong default cost, and the analytic band carries the same information for the cases that matter.
Worth an opt-in later; not this phase.

### 9.3 Say out loud that p-values are useless here

DHARMa's own vignette states it best: *"If you have a lot of data points, residual diagnostics will
nearly inevitably become significant, because having a perfectly fitting model is very unlikely."*
`performance` reached the same position and documents it (*"this formal test almost always yields
significant results… visual inspection is preferable"*) — while its replacement, `check_residuals()`,
is itself a KS test with exactly the same large-n pathology.

Measured in this package: the Brant test on `rincome ~ race + age` gives **p = 0.00089** on a
departure whose visible spread across cuts is 0.05 logits **[M]**. At survey n, a significant
assumption test means "n is large", not "the model is broken".

**Consequence for the design:** the panels report **effect magnitudes** (ΔAIC, φ, the share outside the
band, max VIF, the fitted-value excess), and p-values appear only where they carry information the
magnitude does not — Brant, and the deviance GOF on grouped data. `stats = "assumptions"` follows the
same rule.

### 9.4 Visual polish, and the print theme

The plots must look like tabxplor's tables, not like base R. That means a shared
`reg_plot_theme(theme)` reading `tx_chrome_hex(theme)` — the same `"light"` / `"dark"` / `"print"`
vocabulary z11 established — and a shared accent colour drawn from the palette rather than the five
hard-coded `"#c00000"` literals in today's `tab_reg_plots.R`. There is **no existing ggplot/palette
bridge in the package** (`tab_plot()` is `ggpubr::ggtexttable()`, not a chart), so this phase defines
it, and `or_plot()` adopts it under R3.

`theme = "print"` matters more than it sounds: a diagnostic panel is exactly the kind of figure that
ends up in a thesis appendix in greyscale. Under `"print"` the panels drop the accent hue for
black/grey and lean on line type — which the reference bands already support, since they are shape,
not colour.

---

## 10. Performance

Measured end-to-end: **a 6-panel binomial grid builds and draws in 1 457 ms at n = 6 803** **[M]**
(binned residuals + Q-Q + empirical logit + calibration + influence + VIF, `arrangeGrob` + `grid.draw`).

The budget, and where it goes:

| item                           | cost                                   | note                                        |
|--------------------------------|----------------------------------------|---------------------------------------------|
| refit through `reg_fit()`      | 60 ms                                  | the vignette's 4-predictor binomial **[M]** |
| `rd_bin()` per panel           | 19 ms                                  | vs 370 ms for a loess at the same n **[M]** |
| `rd_qq()`                      | 28 ms all points, 9 ms thinned **[M]** | vs 1 182 ms simulated **[M]**               |
| influence, one contrast        | 35 ms                                  | `reg_coef_if_maker` + one contrast **[M]**  |
| `hatvalues` + `cooks.distance` | 9 ms                                   | not used, listed for scale **[M]**          |
| `geom_point` n = 6 803         | 87 ms                                  | 38 ms thinned to 2 000 **[M]**              |

**The build-time budget (round 2)** is a different and much smaller one, because R7 puts it on every
call: the whole always-on block costs **~44 ms against a 380 ms build (+12 %)**, itemised in §0.B.13,
with the per-family curvature refit (§13.2) as the only line that can grow — 277 ms per numeric
predictor on a multinomial **[M]**. Two numbers keep that in proportion: `effect = "ame"` on the same
model costs **2 153 ms** **[M]**, and the stored curves add **19.5 KB to a 32 KB table** **[M]**.

**Thinning policy — the one that is easy to get wrong.** `max_points` thins **the raw-point layer
only, never the statistics**. Bins, bands, verdicts and the influence ranking are always computed on
the full data. And the thinning must be **stratified toward the extremes**, not uniform: the influence
and Q-Q panels exist precisely to surface the rare extreme observation, so a uniform random subsample
defeats the panel. Keep the top-`id.n` by |influence| / |residual| unconditionally, sample the bulk —
which is what `plot.lm`'s `id.n` already does for labels. (I found no published citation warning
against subsampling diagnostics specifically; the argument stands on its own logic and should be
stated as reasoning in the roxygen, not cited.)

⚠ **Never use `geom_smooth(method = "auto")` in a package.** Verified in the ggplot2 source: it
switches loess → gam at **1 000 observations in the largest GROUP**, `max(table(interaction(group,
PANEL)))` — so a facetted 50 000-row plot gets loess and an unfacetted 1 200-row plot gets gam, and the
emitted message is assembled dynamically so it cannot be regex-suppressed. We avoid the whole question
by not using a smoother (§5.1), but the rule belongs in the file header for whoever adds a panel later.

---

## 11. i18n and theming

`with_legend_lang(lang, function(lg) { ... })` wrapping the **whole label-building block**, exactly as
`reg_model_lines()` does — not each panel separately, so nested `gettext()` calls resolve under one
`LANGUAGE`. Literal `gettext()` / `gettextf()` only (potools "explicit" style extracts nothing else),
`enc2utf8()` on every returned string, no edge whitespace inside a msgid, and notation (OR, IRR, β,
AME, φ, VIF) stays English while prose is translated — the rule `reg_title()` already follows.

Today's plot titles (`"Residuals vs Fitted"`, `"Normal Q-Q"`, `"Odds ratio (95% CI, log scale)"`,
`"Ref."`) appear **nowhere** in `po/R-tabxplor.pot`. Under R3, `or_plot()`'s labels join the catalogue
in this phase too.

⚠ **The Phase z2 glibc caching limitation applies.** Once a French catalogue is loaded in a process,
switching `LANGUAGE` back to `"en"` does not restore English — neither `flush_gettext_cache()` nor
re-binding the domain clears it. This is reliable per-process (the normal case) but a vignette that
rendered a French then an English figure in one process would keep French. The three FR articles and
the EN vignettes already pin `tabxplor.lang` for exactly this reason; the new vignette sections must
do the same.

New msgids: roughly 11 assumption names + 11 captions + ~15 axis labels + the verdict templates ≈ **40
strings**, plus round 2's **8 footer nouns** (§6.2), the linearity line's head, and the `shape =` unit
suffixes (`"at the mean"`, `"curvature"`) ≈ **12 more**. `msgfmt` is installed since z5, so
`dev/update_translations.R` runs.

⚠ The footer nouns are `gettext()`'d **at render**, under the ambient locale, like every other
`reg_footer_spec()` label (`tab-test-display.R:185`) — not under the plot's `lang =`. Two mechanisms
for two surfaces is the existing state, documented in Phase z2; do not "unify" them here without
re-reading why (the footer rows are built long before a `lang` argument exists).

---

## 12. What the function refuses to draw

Refusals are a feature, and each one is a `needs` value in `REG_CHECKS`. The house rule from
`reg-influence.R` applies verbatim: **return nothing rather than a wrong number**, and say why.

| refusal                                     | gate                                | reason                                                                                                                       |
|---------------------------------------------|-------------------------------------|------------------------------------------------------------------------------------------------------------------------------|
| dispersion on a design-based fit            | `design_free`                       | `df.residual` is the design df — φ off by n/nPSU (§8.3) **[M]**                                                              |
| dispersion on a Bernoulli fit               | `count` \| `m_gt_1`                 | φ ≈ mean(1 − μ) by construction, not informative **[M]**                                                                     |
| deviance GOF on a Bernoulli fit             | `m_gt_1`                            | the χ² approximation needs large mᵢ (BeyondMLR §6.5.6)                                                                       |
| any residual panel for multinomial          | —                                   | order-dependent, cor = −0.705 between orderings (§7.6) **[M]**                                                               |
| zero-inflation on anything but a count      | `count`                             | undefined                                                                                                                    |
| scale–location for non-gaussian             | —                                   | the variance is a function of the mean by construction (`performance` #376)                                                  |
| linearity panel for a factor predictor      | `per_predictor` + `predictor_types` | a factor has no functional form to mis-specify; `Obs_*` vs `Model_*` already covers it                                       |
| everything, on the `.fit_cache` digest path | `fit`                               | no model frame exists there (§15) — but the rung-2 *curves* still compute, since they need only the raw columns (§5.6)       |
| collinearity for multinomial                | `vif_ok`                            | `car::vif()` warns "No intercept: vifs may not be sensible" **[M]**; the hand-rolled replacement is wrong for polr (§0.B.19) |
| `poly()` / `ns()` emitted by `shape =`      | —                                   | `marginaleffects` returns **AME = 0.000000** silently **[M]** (§0.B.14). A refusal that protects a number, not a panel       |

The last row of the table is worth its own sentence: **a factor predictor's "linearity" question is
already answered by `empirical = TRUE`.** The crude-vs-model comparison per level *is* the saturated
alternative to the model's parameterisation. Drawing a panel for it would be a second encoding of the
same fact. The plots handle what the table cannot: continuous predictors.

---

## 13. The footer block (R2 + R7) — always on, and not a new mechanism

**The most important thing about this section is what it does *not* build.** The footer already carries
two assumption statistics — `dispersion` and `brant_po` — as ordinary `reg_footer_spec()` rows produced
by `reg_glance()` (`tab_reg.R:2744-2753`, `tab-test-display.R:185-208`). The block is therefore not a
new footer, a new attribute or a new renderer: it is **more rows in an existing spec, plus one more
per-predictor line in an existing dispatcher**. R7 (always on) is affordable precisely because of that
— nothing has to be turned on, only computed (§0.B.13: +12 % of a build).

### 13.1 What renders where — the row/line question, closed

Round 1 flagged "row or line?" as open. It is decided by an existing invariant, not by taste: **a
footer ROW is keyed to exactly one model column; a per-predictor result cannot be** (z8-B established
this when the interaction test had to become a table-wide line, and z13 followed it for
`stats = "global"`).

| item                                                                      | grain                               | rendering                           | precedent it copies                            |
|---------------------------------------------------------------------------|-------------------------------------|-------------------------------------|------------------------------------------------|
| Dispersion, Collinearity, Separation, Predicted range, Zeros, Parallelism | one per model                       | **row** in `reg_footer_spec()`      | `dispersion`, `brant_po` (already there)       |
| **Linearity**                                                             | one per (model × numeric predictor) | **line** via `reg_term_test_line()` | `stats = "global"` (z13), `"interaction"` (z8) |
| Influence                                                                 | one per model                       | **row**                             | —                                              |

So the linearity line is literally `reg_global_lines()` with a different head and a different
discriminator triple — the shared `reg_term_test_line()` renderer already takes both as arguments. Its
discriminators (`linearity_lr` / `_f` / `_wald`) must be registered in the three places z8 documented:
`is_reg_footer()`, `reg_footer_lines()`'s carve-out, and `tab_footer_streams()`.

```text
  N                                12 960
  LR vs null                       <0.01%
  McFadden R2                       0.045
  AIC                              16 960
  Dispersion (Pearson)               1.56 *
  Collinearity (max VIF)             1.21
  Influence (max)                    0.04
  Overall association (LR): race p<0.01% ***, rincome p<0.01% ***, relig p = 0.3% ***.
  Linearity (LR): age p<0.01% ***, tvhours p = 0.08% ***.
```

### 13.2 How each statistic is computed (and what it costs)

| row/line        | statistic                                               | producer                                                                                                                                                    | measured cost                                            |
|-----------------|---------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------|
| Linearity       | p of the added centred quadratic, per numeric predictor | **one augmented refit + `reg_term_tests()`** (drop1 unweighted / `regTermTest` on a design / F for gaussian & quasipoisson) — the dispatcher already exists | glm **17 ms**, svyglm 81, polr 110, multinom 277 **[M]** |
| Collinearity    | max GVIF^(1/2·df)                                       | `car::vif()`                                                                                                                                                | **3.6 ms** **[M]**                                       |
| Dispersion      | Σ(Pearson²)/df.residual                                 | `reg_dispersion()` (exists)                                                                                                                                 | 0.3 ms **[M]**                                           |
| Influence       | max ‖IF‖ / n·(4/n) exceedances                          | `reg_coef_if_maker()` (exists)                                                                                                                              | 11.8 ms **[M]**                                          |
| Separation      | any fitted at 0/1, or max\|coef\| > 10                  | inline                                                                                                                                                      | 0.5 ms **[M]**                                           |
| Predicted range | share of fitted > 1                                     | inline                                                                                                                                                      | free                                                     |
| Zeros           | observed vs Σ dpois(0, μ̂)                              | inline                                                                                                                                                      | free                                                     |
| Parallelism     | Brant p                                                 | already stored on the fit                                                                                                                                   | free                                                     |

**The augmented refit reuses `reg_fit(cross =)`'s own idiom** — z8 already added an internal formal for
"fit the same model plus something", inheriting the binary prep, the grouped-binomial `cbind`, the
`rr` → svyglm route and the design. A second value (`add_terms =`) is the same seam, not a new one.

⚠ **The one cost worth a decision at implementation.** A multinomial with 3 numeric predictors pays
**~830 ms** for its linearity line **[M]**. R7 says always; the honest mitigations, in order of
preference, are (a) do it — a multinomial `tab_reg()` already costs seconds; (b) let the existing
`stats = FALSE` / `"none"` escape cover jamovi's live UI; (c) if latency bites, a `needs = "cheap_refit"`
gate. Do **not** reach for the score test (§0.B.20): it is design-blind.

### 13.3 Refusals keep the block honest

Every gate is a `needs` value in `REG_CHECKS` (§5.4, §12), and three of them are measured facts rather
than caution:

- **no dispersion for a design-based fit** — `df.residual(svyglm)` on a clustered design is the *design*
  df, giving φ = 22.49 where the truth is 1.00 **[M]** (§8.3). Already avoided by construction:
  `reg_glance()`'s weighted branch returns before the dispersion line — the design must be preserved,
  not re-derived.
- **no dispersion for a Bernoulli fit** — φ ≈ mean(1−μ), measured 0.997 **[M]**.
- **no collinearity for multinomial** — `car::vif()` warns "No intercept: vifs may not be sensible"
  **[M]**; the hand-rolled alternative is worse (§0.B.19).

### 13.4 Byte-identity

R7 makes the block unconditional, so **the reg GOF footer snapshots move once, consciously** — and only
the reg ones: no crosstab path is touched. The `stats =` vocabulary gains `"linearity"`,
`"collinearity"`, `"influence"`, `"separation"`, `"predicted_range"`, `"zeros"` (validated in
`reg_footer_stats()`'s `valid` vector), so `stats = c("n", "aic")` still yields exactly two rows and
`stats = FALSE` still yields none. The verification list is §20.

---

## 14. Removing `lm_plots()`

It is exported (`NAMESPACE:158`) and referenced in **nine files** beyond its own:

`R/tab_reg.R:3715` (the `?tab_reg` cross-reference) · `man/lm_plots.Rd` · `man/tab_reg.Rd:424` ·
`_pkgdown.yml:64,68-69` · `NEWS.md:60-61` · `vignettes/tabxplor-reg.Rmd:684,688,691` ·
`vignettes/articles/tabxplor-reg-fr.Rmd:712,716,719` · `tests/testthat/test-tab_reg-plots.R` ·
`CLAUDE.md:410-411,702,1677`.

It has been in exactly one CRAN-facing state: the 2.0.0 development line. **It has never been
released** (1.3.1 has no `lm_plots`), so this is a removal, not a deprecation — the same reasoning
that let Last Phase g hard-remove `tab_export(format = "kable")`.

Its four panel builders are not lost: `lm_plot_resid_fitted` / `_qq` / `_scale_location` /
`_resid_leverage` become the gaussian rows of `REG_CHECKS`, minus the hard-coded hexes and plus the
shared theme. The Cook's-distance contour maths in `lm_plot_resid_leverage()` is worth keeping as the
gaussian variant of row 8 (it is the classic panel, and for `lm` it is exactly right).

Both vignette chunks are rewritten — and the EN one becomes the §1.1 worked example, which is the best
possible replacement for a chunk that currently errors.

---

## 15. jamovi

**Nothing is added to jamovi in this phase, and the reason is architectural, not scheduling.**

The regression backend's fast path (`reref`) exists because the digest stores coef + vcov and no model
frame; every assumption panel needs the frame. Forcing the `fit` tier to serve diagnostics would
re-persist ~10 MB per model into `cache_state$state` — the measured cause of the Phase-o freeze,
which Phase o fixed by *dropping* the cache in comparison mode.

The clean path, when it is wanted: `jmvtabreg.b.R` already holds `self$data`, and the `.plot`
renderFun is currently a no-op stub. A future `Image` with `renderFun = .plot` would call
`reg_assumptions_plots(table, self$data)` — a refit, ~60 ms, nothing cached. That is a small, isolated
addition once the R side is settled; doing it in the same phase would couple a new plotting API to the
byte-locked cache contract for no benefit.

Recorded as a follow-up, not a gap.

---

## 16. Testing

The existing `test-tab_reg-plots.R` idiom (`expect_s3_class(..., "gtable")`) is the right smoke test
and extends to every family. Beyond it, the things that must be *pinned to a reference rather than to
a hand-written expectation* — the house rule that made z8-B's influence functions trustworthy:

| what                          | pinned against                                           |
|-------------------------------|----------------------------------------------------------|
| `rd_bin()` weighted means     | `stats::weighted.mean()` per bin, base R                 |
| `rd_resid()` gaussian         | `stats::rstandard()`                                     |
| `rd_resid()` binomial/poisson | a direct `statmod`-style computation written in the test |
| `rd_qq()` band                | a 999-replicate simulated envelope, to 2 decimals        |
| the influence panel           | `stats::dfbeta()` for `glm` (correlation 1.0000 **[M]**) |
| the design-based influence SE | `sqrt(vcov(svyglm))` (equal to 7 digits **[M]**)         |
| VIF                           | `car::vif()`                                             |
| dispersion                    | `summary(glm)$dispersion` for quasi families             |

Plus the refusals, each with a fixture that fails if the gate is removed: dispersion absent for a
clustered `svyglm`; no Q-Q for multinomial; no linearity panel for a factor predictor; the `data`
mismatch guard aborts.

And one property test worth having: **the multinomial residual is order-dependent** — build the
residual under two level orders and assert `abs(cor) < 0.9`. That is the measurement that justifies
the refusal, and it should be in the suite so the refusal cannot be quietly removed later.

**Round-2 fixtures** — each one fails if its design decision is quietly reverted:

| what                     | assertion                                                                                                                                                        |
|--------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| the `shape =` crude twin | the univariable fit's term names are **identical** to the model's, so `reg_skel_match()` finds them **[M]**                                                      |
| the `shape =` AME        | equals `mean(predict(x + k)) − mean(predict(x))` to 1e-12 **[M]** — the g-computation truth                                                                      |
| **the `poly()` refusal** | a model built with `poly()` returns `avg_comparisons` = 0 while the g-computation truth is non-zero — the property test that justifies never emitting it **[M]** |
| centring                 | `car::vif()` on the emitted terms is < 5 (it is 38.7 uncentred **[M]**)                                                                                          |
| `shape = "linear"`       | byte-identical to a table built without `shape`                                                                                                                  |
| the sparkline            | a straight-line predictor and a saturating one give **different** glyph runs; `spark = FALSE` restores today's label byte-for-byte                               |
| the html `<svg>`         | survives `html_escape_br()` (the trap in §5.7) — it is present in the rendered html, not escaped into text                                                       |
| the footer grain         | a *comparison* table has one linearity line per model column and one collinearity row per model column (§13.1)                                                   |
| the design refusal       | a clustered `svyglm` shows **no** dispersion row (φ would read 22.49 **[M]**)                                                                                    |

Suite cost: the panels are ~1.5 s each at vignette scale, so the file must use a small subsample
(n ≈ 2 000, where the whole grid is ~0.6 s) and `skip_on_cran()` — following the four heaviest files'
precedent from z2.

---

## 17. Open questions for the maintainer

1. **Default panel count.** `check = "auto"` capped at 6 panels in a 3×2 grid, or capped at 4 in 2×2
   (today's `lm_plots` shape)? 6 fits everything a binomial model needs; 4 is more readable on a
   laptop. **Recommendation: 6, `ncol = 3`.**
2. **Per-predictor linearity — all, or the worst?** With 8 numeric predictors the faceted panel has 8
   facets. Draw all (`predictors = NULL`), or default to the 3 with the largest ΔAIC and list the
   rest in the subtitle? **Recommendation: all up to 6, then the worst 6 with a note** — the whole
   value is finding the one you were not looking for.
3. ~~**The §13 footer block: row or line?**~~ **CLOSED (round 2, §13.1): both, by grain.** Per-model
   statistics are rows in `reg_footer_spec()` (copying `dispersion`/`brant_po`, already there);
   the per-predictor linearity test is a line through `reg_term_test_line()` (copying `stats =
   "global"`). The rule was never a matter of taste — a footer row is keyed to exactly one model
   column, so a per-predictor result cannot be one.
4. **`Obs_*` on the linearity panel?** For a numeric predictor, z9 gives a crude effect and z8-B a gap
   test. The linearity panel could overlay the *crude* empirical curve beside the *adjusted* model
   line — making the panel answer both "is it linear" and "is it confounded" at once. Cheap (both
   quantities exist), but it doubles what the panel says. **Recommendation: no, in v1** — one panel,
   one question; revisit after the vignette is written. (Round 2 note: the two curves are nearly the
   same object anyway — empirical vs partial-residual correlate **0.997** **[M]**, §0.B.18 — so the
   overlay would mostly draw one line twice.)
5. **Should `reg_assumptions_plots()` also accept `tab()` output?** A cross-table has no model, so no.
   But `color = "contrib"` *is* the departure from the log-linear independence model, and a
   residual-style panel for a cross-table is conceivable. **Recommendation: out of scope**, and say so
   in the roxygen so the question is closed rather than open.
6. **Name.** `reg_assumptions_plots()` is 22 characters and plural. Alternatives considered:
   `reg_check_plots()`, `reg_diagnostics()`, `reg_assumptions()`. The plural matches `lm_plots()` and
   the fact that it returns a grid. **Recommendation: keep the roadmap's name** — but note that if
   §13's `stats = "assumptions"` lands, `reg_assumptions()` would be free as the numbers-only
   companion, which is a tidy pair.
7. **Vignette placement.** A new `## Checking the model` section in `vignettes/tabxplor-reg.Rmd` (+ the
   FR article) built on §1.1's `age` finding — which means **correcting the vignette's own model** to
   `poly(age, 2)` or a spline in the sections that follow, or leaving it wrong and pointing at it.
   **Recommendation: leave the linear model in the earlier sections and use it as the worked example**;
   changing it would move every numeric result in the vignette for no pedagogical gain.
8. ~~**Does the `car` dependency earn itself?**~~ **CLOSED (round 2, §0.B.19): yes, and multinomial is
   the refusal.** `car::vif()` works on lm / glm / svyglm / **polr / svyolr**; on `nnet::multinom` it
   warns *"No intercept: vifs may not be sensible"* **[M]**. The 15-line `det(R₁₁)det(R₂₂)/det(R)`
   alternative reproduces `car`'s GVIF **exactly** for glm but returns **11.45 where `car` returns
   1.01** for polr **[M]** — so it is not a drop-in, and writing a correct one for polr's `zeta`
   parameterisation is work `car` has already done.
9. **What I could NOT verify.** Listed so nothing here is over-claimed:
   - Gelman & Hill (2007) p. 97 itself — not openly available. The `2·√(p(1−p)/n)` formula is verified
     verbatim from the successor text (ROS §14.5 p. 253, authors' own PDF), and `arm`'s and
     `performance`'s docs both cite p. 97 for matching content.
   - ~~Whether `car::vif()` supports `polr` / `multinom`~~ — verified in round 2 (§17.8).
   - The exact released-R version in which `plot.lm`'s glm Q-Q became a half-normal of |deviance|; the
     R-devel source shows `abs(rds)`, the history I could not pin. Irrelevant to us (we do not clone
     `plot.lm`) but relevant to anyone comparing output.
   - Three web-research agents were terminated early by a spend limit; the multinomial-diagnostics,
     modified-Poisson-diagnostics and general-ecosystem sweeps are therefore thinner than the
     Hosmer-Lemeshow, binned-residual and `performance` sweeps, which completed. The multinomial and
     `rr` designs above rest mainly on **local measurement**, which for the two decisive claims (the
     order-dependence, the fitted values above 1) is the stronger evidence anyway.
   - Round 2 did **not** test `car::vif()` on `svyVGAM::svy_vglm` (the weighted multinomial), nor the
     `shape =` emission through a *weighted* ordinal (`svyolr`). Both are `needs`-gated the same way as
     their unweighted twins; verify at implementation rather than assuming.

### 17.10 Open after round 2 — the four that implementation must settle

10. **Does the sparkline belong in the `levels` cell or in its own column?** §5.7 puts it in the level
    label, appended where the multiplier label already is — no new column, no wider table, every
    backend for free. The alternative (a dedicated `shape` character column) is easier to align and
    easier to drop, but it widens every reg table with a numeric predictor. **Recommendation: the
    level label**, with `options(tabxplor.spark = FALSE)` as the off switch.
11. **How many bins, and quantile or equal-width?** §5.1 fixed `arm`'s `floor(sqrt(n))` rule clamped to
    `[5, 60]` for the *panels*. The **sparkline** wants a fixed, small count so that two predictors'
    glyph runs are comparable — 10 reads well at n = 12 960 **[M]**. **Recommendation: 10 fixed for the
    miniature, the `arm` rule for the panel**, and say so once in the roxygen (two counts for two
    purposes is defensible; two *undocumented* counts is not).
12. **`shape = "cut5"` — quantile groups or equal-width?** §5.8.4 assumes quantiles (equal n per group,
    which is what a survey audience means by "age groups" and what keeps every group's OR estimable).
    Equal-width is what base `cut()` does, so the value name must not be `"cut"` alone if the semantics
    are quantiles. **Recommendation: `"quintiles"` / `"quartiles"` / an integer k for k quantile
    groups**, and no equal-width option until someone asks.
    **Maintainer’s decision: **
13. **Multinomial linearity: pay the 277 ms/predictor, or gate it?** §13.2. R7 says always; the measured
    worst case is ~830 ms for three numeric predictors, in a family that already costs seconds. Decide
    with the jamovi live UI in front of you, not from the number alone.

---

## 18. Rejected alternatives

| rejected                                            | why, measured or sourced                                                                                                                                                                                                                                                |
|-----------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Depend on `performance` + `see`**                 | 2 panels for `polr`, 2 for `multinom`, 1 for `clm`, and `svyglm` diagnostics **identical between a simple and a clustered design** — it silently ignores the design. ~6 new packages in the tree for the families we least need help with, in a second visual language. |
| **Depend on `DHARMa`**                              | Does not support `svyglm`, `polr`, `multinom`, `svyolr`, `svy_vglm` — [#321](https://github.com/florianhartig/DHARMa/issues/321) open and unanswered since 2022. Survey-weighted binomial cannot even `simulate()` (non-integer prior weights).                         |
| **Keep `lm_plots()`'s fit-only contract**           | It is the contract that produced the broken vignette example **[M]**, and it disconnects the plots from the workflow every other tabxplor function shares.                                                                                                              |
| **`keep_fits = TRUE` on `tab_reg()`**               | ~10 MB per model on the table — the measured cause of the Phase-o jamovi freeze. A 4.3 KB recipe + a 60 ms refit does the same job **[M]**.                                                                                                                             |
| **`geom_smooth(method = "loess")` overlays**        | 370 ms → 2 030 ms per panel as n goes 6 803 → 21 483 **[M]**; not weight-aware; and for a binary outcome it smooths a two-valued residual, which is the thing ROS §14.5 says is uninformative.                                                                          |
| **A 19-replicate simulated Q-Q envelope**           | 1 182 ms vs 28 ms for the analytic beta band, which agrees to 0.19 on the extreme order statistic **[M]**.                                                                                                                                                              |
| **Hosmer–Lemeshow**                                 | Arbitrary in `g` (p = 0.11 / 0.0499 / 0.64 for g = 9/10/11 on one model), always rejects at survey n, says nothing about *where*, and is computed in-sample so it can pass an overfitted model. Harrell: "obsolete". Lemeshow co-authored its replacement.              |
| **A randomised quantile residual for multinomial**  | cor = **−0.705** between two level orderings **[M]**.                                                                                                                                                                                                                   |
| **`arm`'s empirical ±2 SE band**                    | ±30 % per-bin disagreement with the book formula, and it ignores weights **[M]**.                                                                                                                                                                                       |
| **A `nullabor` lineup as the default**              | Best pedagogy in the literature, but 19 extra panels per check is the wrong default cost. Opt-in, later.                                                                                                                                                                |
| **Reuse `tab()` to build the empirical-logit bins** | Tempting — the binned proportions *are* a cross-table — but it would route a plot through the fmt/colour pipeline to extract numbers it then throws away. `rd_bin()` is 12 lines of base R.                                                                             |
| **A jamovi panel in this phase**                    | Couples a new plotting API to the byte-locked `.fit_cache` contract for no benefit (§15).                                                                                                                                                                               |

Round 2 adds seven, four of them measured wrong numbers rather than preferences:

| rejected                                                                   | why, measured or sourced                                                                                                                                                                                                                                                                                                                                                          |
|----------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Emitting `poly()` or `ns()` for `shape =`**                              | `marginaleffects 0.32.0` returns **AME = 0.000000, silently**, through every contrast form, because the basis is re-evaluated on the perturbed data and absorbs the shift exactly **[M]**. Raw powers give the correct +0.038 **[M]**. This is a wrong number reaching a user's table, not an inconvenience.                                                                      |
| **An uncentred `x + I(x^2)` emission**                                     | **VIF 38.7** vs 1.2 centred **[M]**, and coefficients (1.184, 0.99841) that no reader can interpret. It would make R7's own new collinearity line flag every curved model.                                                                                                                                                                                                        |
| **`scale()` inside the emitted formula**                                   | Prediction on a subset re-scales with the subset's mean, so `predict(newdata = )` disagrees with the fit **[M]**. Freeze the constants as literals instead — the z9 `multiplier` precedent.                                                                                                                                                                                       |
| **A Rao score test for linearity (no refit)**                              | 4× cheaper (7.6 vs 17–31 ms) and it agrees unweighted **[M]** — but **design-blind**: identical p on a weights-only and a stratified+clustered design, where the design Wald differs by 30 orders of magnitude **[M]**.                                                                                                                                                           |
| **Box–Tidwell as the linearity test**                                      | The test the audience was taught (Hosmer & Lemeshow, Menard, the SPSS workflow), so it is named in the vignette — but it needs `log(x)`, hence **x > 0**, which a centred or negative predictor is not, and it does not generalise across families. The added-quadratic curvature test asks the same question with no domain restriction and is what `car::residualPlots()` uses. |
| **A dedicated sparkline column**                                           | Widens every reg table that has a numeric predictor, for a mark that belongs to that predictor's own row. The level label already carries the multiplier suffix; this is one more thing the row says about itself.                                                                                                                                                                |
| **`kableExtra::spec_plot()` / `svglite` / a base64 PNG for the miniature** | 1 084 B and 843 B respectively against **121 B** for a hand-rolled `<polyline>` **[M]**, and the first writes files to disk. None reaches the console or Excel, which the 30-byte text rendering does.                                                                                                                                                                            |

---

## 19. Sources

**BeyondMLR** (Roback & Legler), `~/github/BeyondMLR`: §1.3 LINE and §1.6.1 the four-panel reading;
§3 distribution shapes; §4.2.1 the four Poisson assumptions, §4.4.8 deviance residuals, §4.4.9 the
deviance GOF, §4.10.1 φ̂ and quasi-Poisson, §4.11 the observed-vs-modelled pmf zero check; §6.2.1 the
four logistic assumptions, §6.5.2 and §6.7.2 the two empirical-logit constructions, §6.5.6 the GOF
validity condition, §6.5.9 binomial over-dispersion; §7.4 what over-dispersion looks like; §11.3.2 the
conditional-density + empirical-logit 2×3 panel. ⚠ The book covers **no** multinomial, ordinal, or
survey-weighted model — those designs above are extensions, not citations.

**Residuals and binned residuals.** Dunn & Smyth (1996) *JCGS* 5(1) 1–10, randomised quantile
residuals. Gelman, Hill & Vehtari, *Regression and Other Stories* §14.5 p. 253 (the ±2 SE formula,
verified verbatim from the authors' PDF). `arm::binnedplot` / `binned.resids` source.
`performance::binned_residuals` source. `regressinator::binned_residuals`.

**Calibration and GOF.** Van Calster et al., *"Calibration: the Achilles heel of predictive
analytics"*, BMC Medicine 2019;17:230 (the four-level hierarchy; the case against HL). Harrell, RMS
ch. on binary logistic + [datamethods](https://discourse.datamethods.org/t/goodness-of-fit-for-probit-model-hosmer-lemeshow/1680/2).
Allison, [*"Why I Don't Trust the Hosmer-Lemeshow Test"*](https://statisticalhorizons.com/hosmer-lemeshow/).
Austin & Steyerberg, Stat Med 2014;33(3):517–535 (loess calibration curves). Nattino, Finazzi &
Bertolini, Stat Med 2014/2016 + Stata Journal 2017 (the calibration belt, with Lemeshow as co-author).
TRIPOD+AI, BMJ 2024;385:q902.

**Implementation traps.** R `stats::plot.lm` source (the three-residual split; the undocumented
zero-weight drop). ggplot2 `StatSmooth$setup_params` (the 1 000-observation *per group* auto switch).
`performance` [#376](https://github.com/easystats/performance/issues/376) (GLM panels),
[#907](https://github.com/easystats/performance/issues/907) (multinom VIF),
[#260](https://github.com/easystats/performance/issues/260) (don't test assumptions not required).
DHARMa [#321](https://github.com/florianhartig/DHARMa/issues/321) (svyglm, unanswered) and the DHARMa
vignette (the large-n significance warning). Wilke, *Fundamentals of Data Visualization*, overlapping
points; ggplot2 book §5.5 (alpha's 1/500 floor).

**In-package.** `R/reg-influence.R` (z8-B, z9, z10) — the influence functions, and the "return NULL
rather than a wrong number" contract this phase inherits. `dev/model_vs_observed_gap_test.md` §3, §13.
`dev/numeric_predictors_crude_counterparts.md`. `dev/black_and_white_publication_palette.md` §12 (the
`"print"` theme vocabulary).

**Round 2 additions.** `car::residualPlots` docs (the curvature test = the t-test for `I(X^2)` in
`update(model, ~ . + I(X^2))`; Tukey's one-df nonadditivity test on the fitted values; lack-of-fit
default TRUE for `lm`, FALSE for `glm`). Box–Tidwell as the taught linearity-of-the-logit test
(Hosmer & Lemeshow 1989; Menard 2002/2010; the SPSS `x·ln(x)` workflow) and splines as its recommended
remedy. jamovi's own *Assumption Checks* pane (collinearity/VIF as a table, Durbin–Watson, normality,
residual plots, Cook's distance) — the reference software this package's audience already knows.
`easystats/performance` [#907](https://github.com/easystats/performance/issues/907) (multinom VIF,
open). `skimr` unicode block sparklines + `fix_windows_histograms()` (the font caveat);
`kableExtra::spec_plot`, `gtExtras::gt_plt_sparkline` (the file/column alternatives measured against in
§0.B.17). `marginaleffects` 0.32.0 behaviour on `poly()`/`ns()` — measured here, not sourced; the
mechanism is `stats::makepredictcall` / `predvars` vs a re-evaluated basis.

⚠ Every round-2 **[M]** was produced by throwaway scripts in this session's scratchpad, on
`gss_cat_data_formatting()` at n = 12 960 (the 4-predictor complete cases) or n = 6 803 (also dropping
`tvhours`), `marginaleffects 0.32.0`, `pillar 1.11.1`, R on WSL2/ext4. The scripts are not kept; each
claim names the design that produced it so it can be re-derived.

---

## 20. Implementation order (round 2)

Five steps, each shippable and verifiable on its own. The order is chosen so that every step's output
is testable before the next one depends on it, and so the two steps that move snapshots come last.

**Step 1 — the primitives, unwired.** `rd_bin()`, `rd_resid()`, `rd_qq()` (§5.1–§5.3) plus the
`REG_CHECKS` fact table (§5.4) with its `stat`/`verdict`/`needs` columns, in a new
`R/reg-assumptions.R`. Nothing calls them yet. Pinned against `stats::weighted.mean`,
`stats::rstandard`, a 999-replicate envelope and `car::vif` (§16). *No user-visible change.*

**Step 2 — the footer block (R7).** `reg_glance()` gains the rung-1 rows; `reg_footer_spec()` gains
their labels; `reg_linearity_rows()` mirrors `reg_global_rows()` and renders through
`reg_term_test_line()`; the three discriminator registrations (§13.1). Needs `reg_fit(add_terms =)` —
the twin of z8's `cross =`. **One conscious snapshot regen** (reg GOF footers only; verify no crosstab
snapshot moves, and that `stats = FALSE` still yields nothing).

**Step 3 — the stored curves + the miniature (R5).** `meta$assumptions` written in `reg_build()`
(§5.6), the text sparkline appended at `tab_reg.R:3624-3639` (§5.7), the `<svg>` upgrade in
`render_kable_html()` with the `html_escape_br()` whitelist extension, `options(tabxplor.spark)` and
the ASCII fallback. **Second conscious regen** (every reg table with a numeric predictor changes one
label cell). Fixture: the same table with `spark = FALSE` is byte-identical to today.

**Step 4 — `shape =` (R6/R8).** The resolver (frozen centring constants, the closed vocabulary), the
emission, the skeleton's two rows, the crude twin's matching terms, the `cut`→factor arm, and the two
escape-hatch fixes (§5.8.6). Fixtures: crude term names match the model's; the AME equals the
g-computation truth; `shape = "linear"` is byte-identical to today; a `poly()` in a user formula plus
`effect = "ame"` refuses rather than returning 0.

**Step 5 — `reg_assumptions_plots()` + `lm_plots()` removal.** The panels (§6), the theme seam adopted
by `or_plot()` (R3), `meta$fit_spec` + the data-match guard (§4.1), the `data`-optional rule (§4.4),
the ~40 new msgids (§11), the vignette section built on §1.1, and the nine `lm_plots` references
(§14).

**Verification, whole phase.** Full suite in both locales (the `LC_ALL=C.UTF-8` run matters here — the
new msgids and the block glyphs are exactly what a locale run catches). Two conscious regens, listed
above; **zero** crosstab churn — no `tab()` path is touched by any step. `dev/verify_golden_field_delta.R`
is not needed (no fmt field and no column attribute is added — the whole design is deliberately built
from the 21 fields and 12 attributes that exist).
