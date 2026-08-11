# Regression assumption plots — design study

Date: 2026-08-10. Status: **RESEARCH ONLY** — no R code written. This report answers Last Phase z12
and records the maintainer's four rulings (§3). Implementation is a separate session.

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

Twelve findings govern the design.

**Why it must exist.**

1. **The model used throughout the regression vignette is badly mis-specified, and no tabxplor output
   reveals it.** `married ~ race + age + rincome + relig`: `age` enters linearly, but the empirical
   logit is an inverted U (−1.76 at age 22, +0.26 at 43, −0.50 at 79). Adding a quadratic gives
   **ΔAIC = 1251** **[M]**. The linear term reports a log-OR of 0.145 per SD while the empirical-logit
   line through the deciles gives 0.225 — a **55 % discrepancy** that is pure functional-form error
   **[M]**. z9 gives that row a crude twin and z8-B tests the gap between them; neither can say *"one
   slope is the wrong summary."*
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

| Question the reader has | What answers it today |
|---|---|
| How big is this effect? | `Model_OR` / `Model_β` / `Model_AME` |
| Is it confounded? | `Obs_*` beside it (z5 `empirical =`), `color = "adjustment"` |
| Is that gap real, or noise? | z8-B's `gap_se` + `color_signif` |
| **Is the model's SHAPE right at all?** | **nothing** |

`reg_assumptions_plots()` is the fourth row. Framing it that way is not decoration — it decides the
default panel set (§6): the functional-form panels come **first**, because they are the ones that
change what the table means.

### 1.1 The motivating measurement, in full

`married ~ race + age + rincome + relig` on `gss_simple`, deciles of `age` **[M]**:

| mean age | P(married) | empirical logit | n |
|---:|---:|---:|---:|
| 22 | 0.147 | −1.76 | 2141 |
| 28 | 0.390 | −0.45 | 2141 |
| 33 | 0.524 | +0.10 | 2141 |
| 38 | 0.549 | +0.20 | 2141 |
| 43 | 0.564 | +0.26 | 2141 |
| 48 | 0.533 | +0.13 | 2141 |
| 53 | 0.556 | +0.23 | 2141 |
| 59 | 0.564 | +0.26 | 2140 |
| 67 | 0.509 | +0.04 | 2140 |
| 79 | 0.378 | −0.50 | 2140 |

`AIC` linear 28 933 → quadratic 27 682 (**ΔAIC = 1251**). R² of a straight line through the ten points
0.475; of a quadratic 0.834 (**gain 0.36**).

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

| | `lm` | `glm` | `svyglm` | `polr` | `multinom` | `svyolr` |
|---|---|---|---|---|---|---|
| `residuals()` | ✓ | ✓ | ✓ | **NULL** | matrix n×K | **NULL** |
| `rstandard()` / `rstudent()` | ✓ | ✓ | ✓ | — | — | — |
| `hatvalues()` / `cooks.distance()` | ✓ | ✓ | ✓ ⚠ | — | — | — |
| `fitted()` | ✓ | ✓ | ✓ | matrix n×K | matrix n×K | matrix n×K |
| `model.frame()` / `model.matrix()` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `broom::augment()` | ✓ | ✓ | ✓ | ✓ (4 cols) | — | — |

⚠ `svyglm`'s hat values sum to p and correlate 0.45 with the weight **[M]** — they fold the *working*
weights in, but they know nothing of strata or clusters (§8.3).

**The universal substrate is `fitted()` + `model.frame()` + `model.matrix()` + the observed y.** Every
panel in §6 is built from those four, plus `reg-influence.R` for the influence panel. Nothing else is
portable across the six classes.

### 2.3 What the ecosystem offers (and why it is not enough)

Measured by a subagent on a fresh install of `performance 0.17.1` / `DHARMa 0.5.0` / `see 0.14.1`,
n = 400:

| model | panels `check_model()` returns |
|---|---|
| `glm` binomial | 7 |
| `svyglm` gaussian | 8 — **design ignored** |
| `svyglm` binomial | 4 — Q-Q silently dropped |
| `MASS::polr` | **2** |
| `nnet::multinom` | **2** (and its VIF is [known-broken, #907, open](https://github.com/easystats/performance/issues/907)) |
| `ordinal::clm` | **1** |

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

| # | Question | Ruling |
|---|---|---|
| **R1** | Entry point | **Table + data, AND accept a bare fit as a secondary form.** `reg_assumptions_plots(x, data)` is primary; a fitted `lm`/`glm`/`svyglm`/`polr`/`multinom` as `x` is the secondary form. ONE engine underneath. |
| **R2** | The numeric side | **Plots carry their verdicts as subtitles, AND `tab_reg(stats = "assumptions")` adds a compact footer block** (φ, GOF p, Brant p, max VIF) so an exported table warns without the plots. |
| **R3** | `or_plot()` | **Keep the name, share the internals.** It adopts the new theme / guard / i18n seam. Zero user-visible change. |
| **R4** | Dependencies | **No new Suggests except `car`** (for `vif()`'s GVIF on multi-df factors). Everything else is built on `ggplot2` + `gridExtra` (both already Suggests) and the package's own `reg-influence.R`. |

Two consequences of R2 worth stating up front: the word *assumptions* now names two coordinated
things (a `stats =` value and a plot function), which is the desired symmetry; and the footer block
must be **rendered from the same fact table as the panel subtitles** (§5.3), or the two will drift —
the §5 disease Phase 17 spent itself removing.

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

| table shape | `model =` accepts | default |
|---|---|---|
| several dependents | a dependent name | the first |
| `predictors = list(m1 = , m2 = )` | a model label | the first |
| `split_var =` | a group level | the first |

`reg_meta` already carries `dependent`, `model_labels` and `split_var`, so the selector needs no new
metadata. For `split_var` the refit is per group, which is what makes the panels comparable to the
side-by-side columns the table shows.

### 4.3 The secondary (fitted-model) form

If `x` inherits `lm`/`glm`/`svyglm`/`polr`/`multinom`/`svyolr`, it is diagnosed directly: the family
is read from the fit rather than from `reg_meta`, `data` is ignored (the fit knows its own frame), and
everything downstream is identical. This is what `lm_plots()` could do, preserved — and it is ~10 lines,
because both forms reduce to the same internal quadruple `(fit, frame, family, weights)`.

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

| panel | `x` | `y` | reading |
|---|---|---|---|
| binned residuals | fitted | response residual | scatter about 0 within ±2 SE |
| empirical link | a predictor | the observed y | on the link scale, a straight line |
| calibration | fitted | the observed y | the diagonal |
| mean–variance | fitted | the response | `var` vs `y`, the identity line |

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

| family | residual | why |
|---|---|---|
| gaussian | `rstandard()` | the classic; `plot.lm` panel 3/5 uses the same |
| binomial, `rr` | randomised quantile | deviance residuals for a 0/1 outcome are near-useless (§7.2) |
| poisson, quasipoisson | randomised quantile | discreteness; catches over-dispersion directly |
| ordinal | randomised quantile from the cumulative `fitted()` matrix | works, and nothing else does |
| grouped binomial | randomised quantile (binomial CDF at the counts) | m > 1 makes deviance usable too, but one rule is better than two |
| **multinomial** | **none — refused** | order-dependent (§7.6) |

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

| column | meaning |
|---|---|
| `key` | `"linearity"`, `"binned_resid"`, `"qq"`, `"calibration"`, `"dispersion"`, `"zeros"`, `"influence"`, `"collinearity"`, `"parallel"`, `"separation"`, `"predicted_range"` |
| `families` | the families it applies to (a character vector; the gate) |
| `assumption` | the assumption it tests, **named the way the source names it** (§9.1) |
| `panel` | the builder function |
| `stat` | the numeric verdict function — `NULL` where the check is graphical only |
| `verdict` | thresholds → `"ok"` / `"warn"` / `"bad"` |
| `caption` | the pedagogical one-liner, `gettext`'d |
| `per_predictor` | logical — does this check produce one panel per numeric predictor? |
| `needs` | what it requires (`"design_free"`, `"m_gt_1"`, `"count"`, …) — the refusal gate (§8, §12) |
| `footer` | logical — does `stats = "assumptions"` print it? |

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

---

## 6. The panels, per family

`auto` selects the rows of `REG_CHECKS` whose `families` matches and whose `needs` are satisfied.
Order matters: **functional form first**, because it is the check that changes what the table means.

| # | check | gaussian | binomial | poisson/quasi | `rr` | grouped binom | ordinal | multinomial |
|---|---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| 1 | **linearity** (per numeric predictor, faceted) | ✓ resid vs x | ✓ empirical logit | ✓ log empirical mean | ✓ log empirical mean | ✓ empirical logit | ✓ cumulative logits | ✓ baseline logits |
| 2 | **binned residuals** vs fitted | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| 3 | **Q-Q** of the residual | ✓ standardized | ✓ quantile | ✓ quantile | ✓ quantile | ✓ quantile | ✓ quantile | **refused** |
| 4 | **calibration** (predicted vs observed) | — | ✓ | — | ✓ | ✓ | ✓ per cut | ✓ per category |
| 5 | **scale–location** | ✓ | — | — | — | — | — | — |
| 6 | **mean = variance** | — | — | ✓ | — | — | — | — |
| 7 | **zero counts** obs vs expected | — | — | ✓ | — | — | — | — |
| 8 | **influence** ‖IF‖ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 9 | **collinearity** (VIF) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 10 | **parallel lines** (proportional odds) | — | — | — | — | — | ✓ | — |
| 11 | **predicted range** (fitted > 1) | — | — | — | ✓ | — | — | — |

Row 8 is the row no other package can draw for the last two columns (§0.4). Row 11 exists because the
modified Poisson's fitted "probabilities" genuinely can exceed 1 — measured max **1.004**, 0.01 % of
rows, on `married ~ race + age + rincome + relig` **[M]** — and that is a real, plottable diagnostic
rather than a theoretical caveat.

Default grid: `check = "auto"` draws rows 1–3 plus the family-specific rows and row 8, capped at 6
panels; `check = "all"` draws every applicable row. `check = c("linearity", "qq")` draws exactly those.

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

| fit | n | `df.residual` | n − p | `Σ(Pearson²)/df.residual` |
|---|---:|---:|---:|---:|
| `glm` binomial | 21 407 | 21 403 | 21 403 | **1.00** |
| `svyglm`, `ids = ~1` | 21 407 | 21 403 | 21 403 | **1.00** |
| `svyglm`, `strata = ~s, ids = ~psu` | 21 407 | **949** | 21 403 | **22.49** |

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

| item | cost | note |
|---|---|---|
| refit through `reg_fit()` | 60 ms | the vignette's 4-predictor binomial **[M]** |
| `rd_bin()` per panel | 19 ms | vs 370 ms for a loess at the same n **[M]** |
| `rd_qq()` | 28 ms all points, 9 ms thinned **[M]** | vs 1 182 ms simulated **[M]** |
| influence, one contrast | 35 ms | `reg_coef_if_maker` + one contrast **[M]** |
| `hatvalues` + `cooks.distance` | 9 ms | not used, listed for scale **[M]** |
| `geom_point` n = 6 803 | 87 ms | 38 ms thinned to 2 000 **[M]** |

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
strings**. `msgfmt` is installed since z5, so `dev/update_translations.R` runs.

---

## 12. What the function refuses to draw

Refusals are a feature, and each one is a `needs` value in `REG_CHECKS`. The house rule from
`reg-influence.R` applies verbatim: **return nothing rather than a wrong number**, and say why.

| refusal | gate | reason |
|---|---|---|
| dispersion on a design-based fit | `design_free` | `df.residual` is the design df — φ off by n/nPSU (§8.3) **[M]** |
| dispersion on a Bernoulli fit | `count` \| `m_gt_1` | φ ≈ mean(1 − μ) by construction, not informative **[M]** |
| deviance GOF on a Bernoulli fit | `m_gt_1` | the χ² approximation needs large mᵢ (BeyondMLR §6.5.6) |
| any residual panel for multinomial | — | order-dependent, cor = −0.705 between orderings (§7.6) **[M]** |
| zero-inflation on anything but a count | `count` | undefined |
| scale–location for non-gaussian | — | the variance is a function of the mean by construction (`performance` #376) |
| linearity panel for a factor predictor | `per_predictor` + `predictor_types` | a factor has no functional form to mis-specify; `Obs_*` vs `Model_*` already covers it |
| everything, on the `.fit_cache` digest path | — | no model frame exists there (§15) |

The last row of the table is worth its own sentence: **a factor predictor's "linearity" question is
already answered by `empirical = TRUE`.** The crude-vs-model comparison per level *is* the saturated
alternative to the model's parameterisation. Drawing a panel for it would be a second encoding of the
same fact. The plots handle what the table cannot: continuous predictors.

---

## 13. `stats = "assumptions"` — the footer block (R2)

`tab_reg(stats = c("gof", "assumptions"))` appends a compact block under the GOF footer:

```
  N                            6803
  McFadden R2                  0.08
  AIC                         27682
  ---
  Dispersion (Pearson)         1.56  *
  Lack of fit (deviance)     p<0.001
  Max VIF                      1.04
  Proportional odds (Brant)   p=0.001  *
```

Rendered from the **`stat` and `verdict` columns of `REG_CHECKS`** (§5.4), which is what keeps it from
drifting from the panel subtitles. Only rows with `footer = TRUE` appear, and only where their `needs`
are met — so a design-based fit shows no dispersion line at all rather than a wrong one.

Two integration questions the implementation must settle, both flagged as open (§17):

- **Row or line?** z8-B established that a footer *row* is keyed to exactly one model column, which is
  why the interaction test had to become a table-wide *line* through `tab_footer_streams()`. The
  assumption block is per model, and a comparison table has several — so it is probably one row per
  model column, i.e. a genuine `reg_footer_spec()` extension. But `Max VIF` is per model while
  `Dispersion` is per model *and* family, and a mixed-family table has both. Needs a decision.
- **Byte-identity.** Adding rows moves the GOF footer snapshots for anyone who opts in — but only for
  those who opt in, since `stats` defaults to the current value. Verify that `stats = "gof"` is
  byte-identical before and after.

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

| what | pinned against |
|---|---|
| `rd_bin()` weighted means | `stats::weighted.mean()` per bin, base R |
| `rd_resid()` gaussian | `stats::rstandard()` |
| `rd_resid()` binomial/poisson | a direct `statmod`-style computation written in the test |
| `rd_qq()` band | a 999-replicate simulated envelope, to 2 decimals |
| the influence panel | `stats::dfbeta()` for `glm` (correlation 1.0000 **[M]**) |
| the design-based influence SE | `sqrt(vcov(svyglm))` (equal to 7 digits **[M]**) |
| VIF | `car::vif()` |
| dispersion | `summary(glm)$dispersion` for quasi families |

Plus the refusals, each with a fixture that fails if the gate is removed: dispersion absent for a
clustered `svyglm`; no Q-Q for multinomial; no linearity panel for a factor predictor; the `data`
mismatch guard aborts.

And one property test worth having: **the multinomial residual is order-dependent** — build the
residual under two level orders and assert `abs(cor) < 0.9`. That is the measurement that justifies
the refusal, and it should be in the suite so the refusal cannot be quietly removed later.

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
3. **The §13 footer block: row or line?** Per §13, `reg_footer_spec()` extension (one row per model
   column) vs a table-wide `tab_footer_streams()` line. The mixed-family case is what decides it.
4. **`Obs_*` on the linearity panel?** For a numeric predictor, z9 gives a crude effect and z8-B a gap
   test. The linearity panel could overlay the *crude* empirical curve beside the *adjusted* model
   line — making the panel answer both "is it linear" and "is it confounded" at once. Cheap (both
   quantities exist), but it doubles what the panel says. **Recommendation: no, in v1** — one panel,
   one question; revisit after the vignette is written.
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
8. **Does the `car` dependency earn itself?** `car::vif()`'s GVIF is the right thing for multi-df
   factors (measured: `race` GVIF 1.17 on 2 df → adjusted 1.04 **[M]**), and R4 approved it. The
   alternative is ~15 lines computing `det(R₁₁)·det(R₂₂)/det(R)` from `cov2cor(vcov(fit))` — which is
   literally what `performance` does, and it works for `polr`/`multinom` where `car::vif()` may not.
   **Flagging honestly: I did not test `car::vif()` on `polr` or `multinom`.** If it fails there, the
   15 lines are better than a dependency that covers only half the families. Test at implementation.
9. **What I could NOT verify.** Listed so nothing here is over-claimed:
   - Gelman & Hill (2007) p. 97 itself — not openly available. The `2·√(p(1−p)/n)` formula is verified
     verbatim from the successor text (ROS §14.5 p. 253, authors' own PDF), and `arm`'s and
     `performance`'s docs both cite p. 97 for matching content.
   - Whether `car::vif()` supports `polr` / `multinom` (§17.8).
   - The exact released-R version in which `plot.lm`'s glm Q-Q became a half-normal of |deviance|; the
     R-devel source shows `abs(rds)`, the history I could not pin. Irrelevant to us (we do not clone
     `plot.lm`) but relevant to anyone comparing output.
   - Three web-research agents were terminated early by a spend limit; the multinomial-diagnostics,
     modified-Poisson-diagnostics and general-ecosystem sweeps are therefore thinner than the
     Hosmer-Lemeshow, binned-residual and `performance` sweeps, which completed. The multinomial and
     `rr` designs above rest mainly on **local measurement**, which for the two decisive claims (the
     order-dependence, the fitted values above 1) is the stronger evidence anyway.

---

## 18. Rejected alternatives

| rejected | why, measured or sourced |
|---|---|
| **Depend on `performance` + `see`** | 2 panels for `polr`, 2 for `multinom`, 1 for `clm`, and `svyglm` diagnostics **identical between a simple and a clustered design** — it silently ignores the design. ~6 new packages in the tree for the families we least need help with, in a second visual language. |
| **Depend on `DHARMa`** | Does not support `svyglm`, `polr`, `multinom`, `svyolr`, `svy_vglm` — [#321](https://github.com/florianhartig/DHARMa/issues/321) open and unanswered since 2022. Survey-weighted binomial cannot even `simulate()` (non-integer prior weights). |
| **Keep `lm_plots()`'s fit-only contract** | It is the contract that produced the broken vignette example **[M]**, and it disconnects the plots from the workflow every other tabxplor function shares. |
| **`keep_fits = TRUE` on `tab_reg()`** | ~10 MB per model on the table — the measured cause of the Phase-o jamovi freeze. A 4.3 KB recipe + a 60 ms refit does the same job **[M]**. |
| **`geom_smooth(method = "loess")` overlays** | 370 ms → 2 030 ms per panel as n goes 6 803 → 21 483 **[M]**; not weight-aware; and for a binary outcome it smooths a two-valued residual, which is the thing ROS §14.5 says is uninformative. |
| **A 19-replicate simulated Q-Q envelope** | 1 182 ms vs 28 ms for the analytic beta band, which agrees to 0.19 on the extreme order statistic **[M]**. |
| **Hosmer–Lemeshow** | Arbitrary in `g` (p = 0.11 / 0.0499 / 0.64 for g = 9/10/11 on one model), always rejects at survey n, says nothing about *where*, and is computed in-sample so it can pass an overfitted model. Harrell: "obsolete". Lemeshow co-authored its replacement. |
| **A randomised quantile residual for multinomial** | cor = **−0.705** between two level orderings **[M]**. |
| **`arm`'s empirical ±2 SE band** | ±30 % per-bin disagreement with the book formula, and it ignores weights **[M]**. |
| **A `nullabor` lineup as the default** | Best pedagogy in the literature, but 19 extra panels per check is the wrong default cost. Opt-in, later. |
| **Reuse `tab()` to build the empirical-logit bins** | Tempting — the binned proportions *are* a cross-table — but it would route a plot through the fmt/colour pipeline to extract numbers it then throws away. `rd_bin()` is 12 lines of base R. |
| **A jamovi panel in this phase** | Couples a new plotting API to the byte-locked `.fit_cache` contract for no benefit (§15). |

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
