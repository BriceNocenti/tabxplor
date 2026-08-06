# A significance test for the model-vs-observed gap — design study

Date: 2026-08-05. Status: **PHASE A IMPLEMENTED** (2026-08-06). The `between_groups` half of §11 has
landed -- the 21st field `gap_se`, the `MEASURES` `bounds` closure, the three policies, the
`residual` -> `zscore` rename (Q4), the `at = "reference"` fix (Q8), and §5.3's aggregated
`predictor x split_var` test, which the maintainer pulled forward into this phase. **Phase B**
(`adjustment`, the influence functions of §3) is still a report.

Two implementation findings worth reading before Phase B:
  * The `bounds` closure must return the interval OF THE SCORE, not the raw gap interval: the score's
    sign is the null direction while the raw interval is signed up/down, and they disagree for a
    protective effect. Re-folding |gap| with the score's sign makes every existing plan branch work
    with no measure-specific code, which is what §7.2 promised but not quite how it described it.
  * §5.3's "interaction ROW in the GOF footer" is not implementable as a row: every footer row is keyed
    to exactly one model column, `reg_spread_models()` re-keys per split group, and `reg_footer_spec()`
    is a fixed discriminator->label list that cannot carry one label per predictor. It ships as a
    table-wide footer LINE through `tab_footer_streams()` instead.

Scope: give the two Last-Phase-z5 colour measures — `color = "adjustment"` (model estimate vs its
observed/crude counterpart) and `color = "between_groups"` (a `split_var` group vs the reference
group) — a significance test of their own, so `color_signif` stops being pinned to `ignore`.

Companion: `dev/model_vs_observed_effect_colour.md` (z5, the descriptive measure). This report
supersedes its §4 with measurements on the **real code paths** rather than a simulation, and answers
the six architecture questions the roadmap raised.

Every number below was measured on this box today. The scripts were one-off and are not kept; each
table names the design that produced it so any of them can be rebuilt.

---

## 0. Executive summary

**The test is sound, it is cheap, and it needs less new machinery than z5 expected — but it must not be
enabled on the conditional-odds-ratio path without a decision, because there it converges to
"significant everywhere" for a reason that is not confounding.**

Twelve measured findings govern the design.

**The estimator side is free.**

1. **Every `Obs_*` effect tabxplor computes is exactly a saturated one-factor GLM coefficient** —
   verified to 1e-10 on all five families, weighted and unweighted (§2.1). So crude and adjusted are
   two M-estimators on the same rows, differing only in ψ. This is the premise the whole method needs,
   and it is an identity, not an approximation.
2. **The crude influence function has a closed form** — one O(n) pass, no fit at all. It reproduces the
   fitted saturated GLM's influence function to 1e-13 and runs **21× faster** than fitting (§2.2). One
   line per link (`logit` / `log` / `identity`), which is one new column in the existing
   `REG_EMPIRICAL` fact table.
3. **The saturated fit's influence function reproduces the Woolf SE exactly** (ratio 1.0000, §2.1) —
   the crude side of the test is not merely consistent with the printed crude CI, it *is* it.

**The variance side is a `survey` call, not new mathematics.**

4. **`survey::svyglm(..., influence = TRUE)` already returns the influence functions** and they match a
   hand-rolled implementation to 2e-9 (§3.2). `survey` is an **Import**, not a Suggest. The weighted,
   the survey-design and the z3 `rr` paths all fit through `svyglm` already.
5. **`survey::svyrecvar()` on the difference of influence functions reproduces `svyglm`'s own SE
   exactly (ratio 1.0000) on a stratified + clustered design**, and the resulting gap SE matches a
   JKn replicate-weights bootstrap to **0.2 %** (§3.3). Strata, clusters and FPC come along for free.
   The IID version of the same quantity is **6 % too small** on that design — so the design *must* be
   respected, and respecting it costs one function call.
6. **The naive independent SE is 3.3× too large** on that design (§3.3); **Hausman's subtraction
   `Var(crude) − Var(adj)` goes negative for logistic and is 14 % too small for gaussian** (§3.6).
   Both rejected alternatives are now measured, not argued.
7. **Over-dispersion needs no φ term**: with a Pearson dispersion of 2.25 the sandwich gap SE matched
   the bootstrap to 0.8 %, and was byte-identical whether the family was declared `poisson` or
   `quasipoisson` (§3.5). One code path.
8. **`marginaleffects`' AME standard error is the delta term with the model-based vcov** (ratio 1.0000
   to a hand-computed delta term, §3.4). The full influence function adds an empirical-averaging term
   worth +0.12 % on the tested design. So the AME path needs its own influence function — which is 4
   lines given the coefficient one — but the discrepancy with what the cell prints is negligible.

**What the test rejects — the one finding that forces a decision.**

9. Under **zero confounding by construction** (X ⟂ Z), rejection rate at α = 0.05, by n:

   | n      | log OR (non-collapsible) | log RR (collapsible) | AME (collapsible) |
   |--------|--------------------------|----------------------|-------------------|
   | 500    | 0.160                    | 0.052                | 0.052             |
   | 2 000  | 0.580                    | 0.040                | 0.044             |
   | 8 000  | 0.992                    | 0.044                | 0.044             |
   | 32 000 | **1.000**                | 0.042                | 0.042             |

   The test is **exactly calibrated on every collapsible scale** and **consistent for a non-zero
   quantity on the odds-ratio scale**. On OR it is a valid test of a true statement ("the marginal and
   conditional odds ratios differ"), which is simply not the statement a reader will make of it (§4.1).
10. **Real data confirms it.** `gss_simple`, `married ~ race` adjusted for `rincome + relig`
    (n = 13 015): `raceBlack` has an OR gap of ×1.045, **z = +2.80, p = 0.005** — and on the collapsible
    RR scale the same comparison is ×1.006, **z = 0.82, p = 0.41**. The significant OR gap for that row
    is non-collapsibility. `raceOther` is significant on both scales (×0.890 / ×0.949, p ≈ 1e-7): real
    confounding (§4.3).
11. **There is a principled fix, and tabxplor already ships most of it.** Comparing the **marginally
    standardised** adjusted OR (g-computed, then put back on the odds scale) with the crude OR restores
    exact calibration: mean gap +0.0023 and rejection **0.052** versus +0.0733 and **0.912** for the
    conditional OR (§4.2). This is the same manoeuvre as `effect = "ame"` / `"ame_ratio"` /
    `family = "poisson"`.

**`between_groups` is the easy half and should ship first.**

12. The groups are disjoint, so `sqrt(SE_A² + SE_B²)` is exact, **and both SEs are recoverable from the
    Wald bounds already stored in the table** — verified to 5 significant digits (§5.1). The resulting
    p (0.0029) matched a pooled-model LRT interaction test (0.0035). **No influence function, no model
    frame, no jamovi consequence.** But §5.2 measures a trap the literature warns about and no scale
    escapes: with the *same* structural effect and unequal unobserved heterogeneity, all three scales
    reject at ≈ 77 %.

**The jamovi objection is void.** Influence functions recomputed from `data` + the stored coefficients,
with no fitted object, are **bit-identical** to those from the fit (0.000e+00) and 2.7× cheaper than a
refit; and the influence function is **exactly equivariant under a reference change** (8e-18), so the
digest/reref contract is untouched (§6).

**Recommended architecture** (§7), in one line each:

- **One new field, `gap_se`** (the 21st), holding the SE of the gap **on the test scale** — not a z, not
  a p. It is strictly more informative and it is what the CI-floor reading needs.
- **`sig_source` does NOT gain a third value.** Instead `MEASURES` gains a **`bounds` closure** (default:
  the stored `ci_inf`/`ci_sup`; `adjustment`/`between_groups`: derived from the raw gap ± crit·`gap_se`).
  Every existing policy branch then works unchanged — including `guaranteed_effect`, which becomes the
  *right* reading for this measure: **"adjustment moved this effect by at least ×1.1"**.
- **`force_policy` is deleted**, not extended. A phase-2 landing that *removes* a field is the signal
  that the measure became first-class.
- The z4 `residual` break scale is z-valued and would serve both measures — **consider renaming it
  `zscore`** while the option is still 2.0.0-new and free to change (§7.5). This is a now-or-never item.

**Recommended scope split**: `between_groups` first (no new statistics at all beyond the field), then
`adjustment`. §11.

---

## 1. What is being tested, and where it applies

| mode                                                | compared with                     | are the two estimates independent? | needs the model frame? |
|-----------------------------------------------------|-----------------------------------|------------------------------------|------------------------|
| `color = "adjustment"`, single model                | its own `Obs_*` crude effect      | **no** (same rows, r = 0.52–0.90)  | yes                    |
| `color = "adjustment"`, `predictors = list(...)`    | the one shared crude effect       | no                                 | yes                    |
| `color = "adjustment"`, several dependents          | that fit's own crude effect       | no                                 | yes                    |
| `color = "between_groups"` (`split_var`)            | the reference group's estimate    | **yes** (disjoint rows)            | **no**                 |

The null is the same in all four: **the two estimates are equal on the estimate's own test scale** —
the log ratio for a multiplicative effect (OR / RR / IRR), the plain difference for an additive one
(β / AME / risk difference). That is exactly the scale `fmt_adjustment_score()` already folds around 1
or 0, so the test and the colour cannot drift apart.

**What it is not.** It is not a test for confounding (§4), not a causal claim, and not a model-selection
device. The change-in-estimate criterion it makes precise is itself contested as a confounder-selection
rule (Maldonado & Greenland 1993 for the 10 % folklore; and a 2024 critique bluntly titled *"the change
in estimate fallacy"*). tabxplor's job is to say *how much two numbers in the table differ and whether
that difference is bigger than noise* — nothing more, and the docs must say so.

---

## 2. The estimator side: everything tabxplor calls "observed" is a saturated GLM

### 2.1 Measured identity

`reg_empirical()` builds each crude effect by hand from weighted cell sums. Compared with the
coefficient of a one-factor GLM at the matching link, on n = 4000, three-level predictor:

| crude column                     | matching saturated fit          | max abs. difference |
|----------------------------------|---------------------------------|---------------------|
| `Obs_OR` (binomial)              | `glm(y ~ x, binomial)`          | 9.2e-14             |
| `Obs_%` risk difference (= AME)  | `glm(y ~ x, gaussian)`          | 1.7e-14             |
| `Obs_RR` (Katz, the z3 `rr` arm) | `glm(y ~ x, poisson)`           | 1.8e-10             |
| `Obs_diff` (gaussian)            | `lm(y ~ x)`                     | 1.3e-13             |
| `Obs_IRR` (poisson counts)       | `glm(y ~ x, poisson)`           | 3.7e-10             |
| `Obs_OR` **weighted**            | `glm(y ~ x, binomial, weights)` | 8.9e-16             |
| `Obs_diff` **weighted**          | `lm(y ~ x, weights)`            | 9.8e-15             |

And the influence function of that saturated fit reproduces the **Woolf** standard error the crude
column already prints:

```
Woolf log-OR SE       0.07832   0.07913
influence-function SE 0.07832   0.07913     ratio 1.0000  1.0000
model-based glm SE    0.07832   0.07913
```

This matters more than it looks. It means the crude side of the gap test is not an approximation
bolted beside the crude column — it is *the same estimator*, so the test can never contradict the
interval the table prints next to it.

### 2.2 The crude influence function in closed form (no fit)

For a saturated one-factor model, level *l* against reference *r*, with weights *w*:

```
IF_i  =  1(x_i = l) · w_i (y_i − μ_l) / Σ_{x=l} w   ·  g'(μ_l)
       − 1(x_i = r) · w_i (y_i − μ_r) / Σ_{x=r} w   ·  g'(μ_r)

g'(μ) = 1/(μ(1−μ))   logit  (OR)
        1/μ          log    (RR, IRR)
        1            identity (risk difference, mean difference)
```

Measured against the fitted equivalent, n = 8000:

| crude effect              | max abs. difference | SE (closed form) | SE (fitted) |
|---------------------------|---------------------|------------------|-------------|
| log-OR (logit)            | 8.0e-17             | 0.055250         | 0.055250    |
| log-RR (log)              | 2.4e-13             | 0.029462         | 0.029462    |
| risk difference (identity)| 2.2e-17             | 0.013430         | 0.013430    |
| mean difference (identity)| 3.4e-17             | 0.027452         | 0.027452    |
| log-OR, **weighted**      | 6.9e-18             | 0.061694         | 0.061694    |

Cost: **0.00056 s vs 0.01156 s — 21× cheaper** than fitting the saturated model.

**Architectural consequence.** `REG_EMPIRICAL` (`R/tab_reg.R:1207`) already declares, per family, the
crude column's shape and CI method. It gains **one column — the link** — and the same row then drives
the crude column, its CI, *and* its influence function. No per-family test code, no new switch.

---

## 3. The variance side

### 3.1 The method, in one paragraph

Both estimators solve estimating equations `Σᵢ ψ(Oᵢ, θ̂) = 0` on the **same rows**, so both are
asymptotically linear: `θ̂ − θ ≈ Σᵢ IFᵢ`. The difference of two such estimators is therefore the sum of
the differences of their influence functions, and

```
Var(θ̂_adj − θ̂_crude) = Σᵢ ( IFᵢ^adj − IFᵢ^crude )²
```

carries the covariance between them **inside the square** — the quantity §3.6 shows cannot be obtained
any other way. This is *seemingly unrelated estimation*: Stata's `suest` (Weesie 1999) builds exactly
this joint sandwich by stacking the two score vectors and treating each observation as a cluster
contributing to both (the Rogers 1993 cluster modification), and Stata's own manual contrasts it with
`hausman` in precisely the terms §3.6 measures. Mize, Doan & Long (2019, *Sociological Methodology*)
is the canonical sociological statement and applies it to exactly our four modes — nested models,
different outcomes, groups within a sample, different model types — with a companion Stata package
(`mecompare`). **tabxplor would be, as far as this search found, the first R implementation to do it per-table-cell as a display.**

### 3.2 `survey` already computes them

```
survey::svyglm(y ~ x + z, design, family = quasibinomial(), influence = TRUE)
  attr(fit, "influence")   ->  n x p matrix
  max |survey influence − hand-rolled| = 2.2e-09
```

`survey` (an **Import**) has had `influence = TRUE` since 2020, added so `svyby()`/`svycontrast()`
could estimate covariances *between domain estimates* — the same cross-estimate covariance problem,
solved by the same object, blessed by the package author. On every path where `tab_reg()` already fits
through `svyglm` (weighted, survey design, and the z3 `rr` family, which always does) the influence
functions are **one argument**, not new code.

For the plain `glm` paths a six-line helper is needed:

```
U = X · w(y − μ)·μ'(η)/V(μ)      A = Xᵀ diag(w·μ'(η)²/V(μ)) X      IF = U A⁻¹
```

### 3.3 A real survey design: stratified, clustered, weighted

8 strata × 30 clusters × 12 units = 2880, with a cluster random effect so the design effect is real.

| quantity                                | value    | check                          |
|-----------------------------------------|----------|--------------------------------|
| `svyglm` SE (adjusted `xb`)             | 0.089010 | —                              |
| `svyrecvar(IF_adjusted)`                | 0.089010 | **ratio 1.0000**               |
| `svyglm` SE (crude `xb`)                | 0.081197 | —                              |
| `svyrecvar(IF_crude)`                   | 0.081197 | **ratio 1.0000**               |
| **gap** D                               | −0.42274 |                                |
| SE(gap), design-based `svyrecvar`       | 0.036922 | → z = −11.45                   |
| SE(gap), **JKn replicate weights**      | 0.037004 | **ratio 0.998** (validation)   |
| SE(gap), IID stacked IF (no design)     | 0.034612 | **6 % too small**              |
| SE(gap), naive independent              | 0.120481 | **3.3× too large**             |

Two conclusions. The design-based route is *exact* (it is how `survey` computes its own variances), and
ignoring the design is **not** a safe approximation — it would over-state significance by 6 % in SE
terms on a mild design and much more on a strongly clustered one.

### 3.4 The AME path, and what `marginaleffects` actually reports

An AME is not a GLM coefficient: `AME = (1/n) Σᵢ [μ(1, zᵢ) − μ(0, zᵢ)]` depends on the empirical
covariate distribution as well as on β. Its influence function has two terms:

```
IFᵢ^AME = (gᵢ − AME)/n   +   Gᵀ IFᵢ^β        gᵢ = μ(1,zᵢ) − μ(0,zᵢ),  G = ∂AME/∂β
          ^ empirical average   ^ delta method
```

Measured, n = 3000, logistic:

| quantity                                   | SE       | ratio vs `marginaleffects` |
|--------------------------------------------|----------|----------------------------|
| `marginaleffects::avg_comparisons`         | 0.017605 | 1.0000                     |
| delta term with the **model-based** vcov   | 0.017605 | **1.0000**                 |
| delta term alone (sandwich)                | 0.017617 | 1.0007                     |
| **full influence function**                | 0.017627 | 1.0012                     |
| nonparametric bootstrap (B = 500)          | 0.017323 | 0.984                      |

So `marginaleffects` reports the delta method with X held fixed. The omitted averaging term is +0.12 %
here; it grows with effect heterogeneity across covariate patterns. **Implication:** the AME gap test
needs its own influence function (four lines on top of the coefficient one), and the fact that it
differs slightly from the printed AME's SE is not a defect — they answer different questions — but it
should be one sentence in the docs.

The `ame_ratio` (marginal standardisation, z3) case is the same shape with a log applied; measured gap
SE 0.02019 against a bootstrap 0.02040, **ratio 0.990** (§3.7 table).

### 3.5 Over-dispersion needs nothing

Negative-binomial counts, Pearson dispersion **2.25**, n = 3000:

```
gap SE  sandwich (no phi anywhere) = 0.02058
        bootstrap (B = 400)        = 0.02042      ratio 1.008
family = poisson, identical IF     = 0.02058
```

The sandwich absorbs over-dispersion by construction — `(y − μ)²` is simply larger. The `quasipoisson`
arm, the φ scaling and `disp_known` are all irrelevant to the gap test. **One path, no family-specific
correction.**

### 3.6 The two rejected alternatives, measured

**CI overlap** was rejected in z5 §4.1 (correlation 0.944 ⇒ effective α ≈ 3e-31). Re-confirmed here:
the correlation between crude and adjusted ranges 0.52 (gaussian) to 0.90 (binomial) — never near
zero. **Do not revisit.** The docs should actively warn against the reader's own version of it ("the
two intervals overlap, so nothing changed"), which is the mistake this feature would otherwise teach.

**Hausman's subtraction**, `Var(D) = Var(crude) − Var(adj)`, n = 3000:

| family   | Var(crude) | Var(adj) | Hausman difference | true Var(D) |
|----------|------------|----------|--------------------|-------------|
| gaussian | 0.004170   | 0.001551 | +0.002619          | 0.003030    |
| binomial | 0.005679   | 0.007129 | **−0.001450**      | 0.001422    |

Negative for logistic (adjusting for a predictive covariate *raises* a logit coefficient's SE — the
same non-collapsibility rescaling seen from the variance side), and 14 % too small for gaussian where
it does have the right sign. **Unusable, on both counts.**

### 3.7 Every path, one table

n = 3000, B = 600 bootstrap replicates, real confounding present.

| path                                     | SE (influence) | SE (bootstrap) | ratio | corr(crude, adj) | SE naive  |
|------------------------------------------|----------------|----------------|-------|------------------|-----------|
| binomial OR (log scale)                  | 0.03862        | 0.03676        | 1.051 | 0.898            | 0.11332   |
| `rr` / modified Poisson (log)            | 0.01794        | 0.01755        | 1.023 | 0.898            | 0.05499   |
| poisson counts IRR (log)                 | 0.02456        | 0.02492        | 0.985 | 0.780            | 0.05288   |
| gaussian β                               | 0.05615        | 0.05643        | 0.995 | 0.524            | 0.07678   |
| binomial AME (risk difference)           | 0.00942        | 0.00914        | 1.030 | 0.867            | 0.02505   |
| binomial `ame_ratio` (log of adj. risks) | 0.02019        | 0.02040        | 0.990 | 0.838            | 0.05049   |

Ratios 0.985–1.051; the bootstrap's own Monte-Carlo error at B = 600 is ≈ 3 %, so these are
indistinguishable from exact. The naive column is 2–4× too large everywhere.

### 3.8 Where it stops holding

| path                                       | gap test | why                                                                             |
|--------------------------------------------|----------|---------------------------------------------------------------------------------|
| multinomial, ordinal                       | **no**   | no crude effect column at all (`obs` already NA) — degrade, as z5 does           |
| grouped binomial (`trials =`)              | **no**   | no crude 2×2 (`pos_i` NULL) — `obs` already NA                                   |
| compound `formula =` escape hatch          | **no**   | no crude companion                                                              |
| numeric predictors, `multiplier`           | **no**   | no crude twin (see the companion report on numeric predictors) — `obs` NA        |
| `method = "profile"`                       | caveat   | the printed model CI is profile-likelihood; the gap test is Wald. Different      |
|                                            |          | quantities, no contradiction, one sentence in the docs                           |
| `effect = "ame"` **+ `at = "reference"`**  | **BUG**  | see below                                                                        |
| `svyolr` / `svy_vglm` (weighted 3+ level)  | **no**   | no crude companion                                                              |

**A pre-existing z5 defect found while auditing this** (independent of the test, worth fixing either
way): `reg_empirical_columns()` receives `effect` but **not `at`**. With
`effect = "ame", at = "reference"` the model column is a **MER at the reference profile** while the
crude companion is a **marginal** risk difference over the whole sample. z5's `obs` is written anyway,
so `color = "adjustment"` currently scores a gap between two different estimands. It is silent today;
attaching a p-value to it would make it loud. **Recommendation: set `obs` to `NA` when
`at == "reference"`** (one condition, at the one place the crude effect is attached).

---

## 4. What the test rejects — the decision that cannot be deferred

### 4.1 Calibration, by scale and by n

X ⟂ Z by construction, so Z is **not a confounder** and the true crude and adjusted effects are the
same causal quantity. `Y ~ Bern(logit⁻¹(−0.5 + log2·X + log4·Z))`. 250 replications (120 at n = 32 000).

| n      | rejection α=.05 log OR | log RR | AME   | mean gap log OR | log RR  | AME     |
|--------|------------------------|--------|-------|-----------------|---------|---------|
| 500    | 0.160                  | 0.052  | 0.052 | +0.0668         | −0.0007 | −0.0004 |
| 2 000  | 0.580                  | 0.040  | 0.044 | +0.0725         | +0.0004 | +0.0003 |
| 8 000  | 0.992                  | 0.044  | 0.044 | +0.0712         | −0.0001 | −0.0001 |
| 32 000 | **1.000**              | 0.042  | 0.042 | +0.0717         | +0.0000 | +0.0000 |

Read the two halves separately.

- **Collapsible scales (RR, IRR, β, AME, `ame_ratio`): textbook.** The gap is 0 in expectation and the
  test holds its nominal size at every n. `grey_non_signif` and `guaranteed_effect` mean exactly what
  a user expects.
- **Odds ratio: the test is correct and the *interpretation* is the trap.** The gap converges to a
  fixed non-zero constant (+0.072 log units ≈ ×1.075) that has nothing to do with confounding, so the
  power to detect it converges to 1. At survey sizes **every** OR row will be "significant".

This is not a flaw in the test; it is the collapsibility fact of z5 §3 seen through a p-value. It is
nonetheless a **product** problem: a coloured, starred, "significant" cell is a claim, and the claim a
reader will make is "adjustment mattered here".

### 4.2 The principled fix, and it is already in the package

Compare the crude OR not with the *conditional* OR but with the **marginally standardised** OR —
g-compute the adjusted model's two average risks and put them back on the odds scale. n = 4000, 250
replications, same zero-confounding design:

| comparison                     | mean gap (log) | rejection rate |
|--------------------------------|----------------|----------------|
| conditional OR vs crude OR     | +0.0733        | **0.912**      |
| **marginal OR vs crude OR**    | +0.0023        | **0.052**      |

Exact calibration restored. This is the *same manoeuvre* as `effect = "ame"`, `effect = "ame_ratio"`
and `family = "poisson"` — three routes tabxplor already ships, all of which give a collapsible
marginal estimand and all of which the measurement above shows to be perfectly calibrated.

So the honest options for the OR path are, in increasing order of paternalism:

- **(a) ship with a caveat** (the z5 Q6 ruling for the descriptive measure): the legend already carries
  a non-collapsibility tail on the OR path; the significance policies inherit it.
- **(b) enable the policies only on collapsible scales**, informing once and falling back to `ignore`
  on the conditional-OR path, with the message naming the three fixes. This is what `force_policy`
  already does — it would become **conditional on the column** rather than static, i.e.
  `measure_policy(measure, policy, x)`.
- **(c) add a marginalisation mode** so the OR path *has* a clean comparison of its own. Statistically
  the best answer, but it changes what is compared without changing what the cell prints — a
  documentation problem at least as bad as the one it solves. **Not recommended.**

**Recommendation: (b), with the message pointing at `effect = "ame"` / `family = "poisson"`.** It is the
only option under which a coloured "significant gap" cell is *always* a true statement about
confounding, it needs no new user concept, and it turns the collapsibility ladder z3 and z5 built into
something the software actively uses rather than merely documents. (a) is defensible and consistent
with z5's ruling; it costs one sentence and buys a table full of stars that mean "your covariate
predicts the outcome".

### 4.3 The real-data version of the same thing

`gss_simple`, `married ~ race` adjusted for `rincome + relig`, complete cases n = 13 015:

| scale | term        | crude | model | ratio  | gap ± SE          | z     | p       |
|-------|-------------|-------|-------|--------|-------------------|-------|---------|
| OR    | `raceBlack` | 2.436 | 2.545 | ×1.045 | +0.0438 ± 0.0156  | +2.80 | 0.0051  |
| OR    | `raceOther` | 1.114 | 0.991 | ×0.890 | −0.1167 ± 0.0211  | −5.52 | 3.4e-08 |
| RR    | `raceBlack` | 1.442 | 1.450 | ×1.006 | +0.0060 ± 0.0073  | +0.82 | **0.41**|
| RR    | `raceOther` | 1.056 | 1.003 | ×0.949 | −0.0519 ± 0.0097  | −5.33 | 9.9e-08 |

`raceBlack` is "significantly adjusted" on the OR scale and **not adjusted at all** on the collapsible
one. `raceOther` is genuinely confounded and says so on both. This single table is the best teaching
example the vignette will get, and it argues for option (b) about as loudly as data can.

### 4.4 Multiplicity

Realistic table: three factor predictors (4 + 3 + 3 levels ⇒ **7 non-reference cells**), one true
confounder, tested on the collapsible RR scale so the true gap is 0 everywhere. 300 tables:

```
mean false positives per table      0.28
P(at least one false positive)      0.200
independent-cell prediction         0.302   ( = 1 − 0.95^7 )
```

The cells are positively dependent (they share the adjusted fit), so multiplicity is *milder* than
independence predicts — but **one table in five shows a spurious significant gap**. This is exactly the
package's existing position on every other per-cell significance (`new_colors_UI.md` W11: per-cell at
`conf_level`, uncorrected). Consistent; must be one sentence in `?tab_reg` and the vignette. No
correction is recommended — an FDR adjustment across a display grid would be a new concept for one
measure, and it would break the "every significant cell is coloured" invariant the
`guaranteed_effect` scale rests on.

### 4.5 Small samples

True gap 0, collapsible scale, 800 replications:

| n     | sd(z) | P(&#124;z&#124; > 1.96) | P(&#124;t_{n−3}&#124;) | nominal |
|-------|-------|-------------------------|------------------------|---------|
| 80    | 0.620 | 0.001                   | 0.001                  | 0.05    |
| 200   | 0.762 | 0.010                   | 0.010                  | 0.05    |
| 600   | 0.882 | 0.013                   | 0.013                  | 0.05    |
| 2 000 | 0.975 | 0.031                   | 0.031                  | 0.05    |

The test is **conservative** in small samples and approaches nominal from below. Two consequences:
**no small-sample correction is needed** (the error is in the safe direction), and **a t reference
distribution changes nothing** (identical to three decimals at every n) — so the gap test uses z, full
stop, and does not need to mirror `tab-agg.R`'s Rule B.

---

## 5. `between_groups` — the free half

### 5.1 Everything needed is already in the table

Two disjoint `split_var` groups of 1500, genuinely different effects:

```
SE recovered from the stored Wald bounds : A 0.10754 (model 0.10754) | B 0.11127 (model 0.11127)
gap D = +0.4603   SE = sqrt(SE_A^2 + SE_B^2) = 0.1547   z = +2.97   p = 0.00293
pooled-model LRT for the x:group interaction            p = 0.00347
bootstrap SE of the between-group gap = 0.1633          ratio quadrature/bootstrap = 0.95
```

The SE recovered from a Wald interval by `(hi − lo) / (2·crit)` is **exact** (5 significant digits), the
quadrature SE is within bootstrap noise (z5 §9.1 measured 1.012 on a different seed; here 0.95 at
B = 400, whose own Monte-Carlo error is ≈ 3.5 %), and the p matches the likelihood-ratio interaction
test to 3 decimals. This is the standard two-independent-estimates test (Altman & Bland 2003).

**No influence function. No model frame. No fit. No jamovi consequence.** The whole computation is one
subtraction and one square root at the single point in `reg_write_group_obs()` where the groups are
parallel tibbles — precisely where z5 already writes `obs`.

That makes `between_groups` a **strictly smaller phase** than `adjustment`: it needs the `gap_se` field
and the `bounds`-closure plumbing (§7), and nothing else. Shipping it first de-risks the field pass and
the colour-engine change, and leaves only the influence-function work for `adjustment`.

### 5.2 The trap that no scale escapes

The sociological literature (Allison 1999; Williams 2009; Mood 2010) warns that logit coefficients are
confounded with each group's residual variation, so a between-group difference in coefficients may
reflect unequal unobserved heterogeneity rather than a different effect. Measured: two groups,
**identical structural β = log 2**, unobserved-heterogeneity SD 0.2 vs 2.0, n = 3000, 250 replications:

| scale  | mean gap (B − A) | rejection rate |
|--------|------------------|----------------|
| log OR | −0.2752          | 0.772          |
| log RR | −0.1318          | 0.764          |
| AME    | −0.0674          | **0.772**      |

**All three reject equally.** This corrects the tempting advice "use AMEs and the problem goes away":
the marginal effect on the probability scale *genuinely is* smaller when the outcome is noisier, so the
AME test is correctly detecting a real difference in the marginal quantity — but not in the structural
one. The honest statement, which belongs in the docs verbatim:

> A between-group difference in an effect measure is a difference in **that measure**, not necessarily
> in an underlying causal effect. Groups can differ in outcome variability or base rate and show
> different effects on every scale.

That is a caveat, not a blocker, and unlike §4.1 it applies to every scale, so it cannot be engineered
away — it must be written down.

### 5.3 The stronger version, if it is ever wanted

A per-predictor **interaction row in the GOF footer** (one Wald test for `predictor × split_var`) is
the textbook answer to "do the effects differ between groups", costs a single extra fit, and is
aggregated so it carries no multiplicity inflation. It complements the per-cell colour rather than
competing with it. Out of scope here; recorded so it is not re-derived.

---

## 6. The jamovi consequence — measured, and it is not a consequence

z5 §4.4 deferred this phase partly because "influence functions need the model frame, which
`reg_build_digest` deliberately does not keep". Both halves of that worry are now measured away.

**(1) The frame is not needed — only `data` and the stored coefficients.**

```
max | IF(from the fitted object) − IF(from data + stored coef) | = 0.000e+00
one glm refit                  0.0079 s
one IF rebuild from coef       0.0029 s      (2.7x cheaper than a refit)
```

Bit-identical, because the influence function is an explicit function of `(X, y, w, β)` and the digest
already stores β. jamovi passes `data` on every run (that is how `reg_empirical()` — which the whole
feature depends on — already works on the digest path).

**(2) The reference change is exactly equivariant.**

`reg_reref_fit_res()` recomputes any factor reference as a *linear contrast* of the canonical
coefficients. Influence functions inherit that linearity exactly:

```
coef(c vs b) from a real refit                = +0.344083
contrast of the canonical coefficients        = +0.344083
max | IF(refit) − IF(contrast of canonical IF)| = 8.24e-18
SE from the contrast 0.081968   SE from the refit 0.081968
```

So the gap SE for any display reference is one linear combination of the canonical influence matrix.
**The digest stays `coef + vcov`; `reg_build_digest` does not change; the byte-identity lock in
`test-jmvtabreg-cache.R` is untouched.** The only cost is one `model.matrix()` + one `p × p` solve per
run — measured at 0.003 s for n = 5000.

**Schema:** the cached *carrier* stores fmt columns, which would gain a field ⇒ `JMVTAB_CACHE_SCHEMA`
bumps (8 → 9) exactly as it did for `obs`. `JMVREG_CFG` stores digests and raw fits, whose shape does
not change ⇒ **no reg-side bump**.

---

## 7. Architecture — where the second quantity lives

This is the section the roadmap asked for explicitly, and it has a better answer than z5 anticipated.

### 7.1 The field: one new, the 21st

Everything else was checked and rejected:

| candidate                                   | verdict                                                                                                                                                                                                                                    |
|---------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| reuse **`ctr`** (free on reg columns)       | **No.** `ctr` means "contribution to chi²" and z4 derives the standardized residual from `sign(ctr)` + `pvalue`. A second meaning in one field is the §2.5 disease Phase 17 spent itself removing, and `$ctr` is public through `mutate()`. |
| reuse **`pvalue`**                          | **No.** Occupied by the model estimate's own p, which the printed stars read.                                                                                                                                                             |
| reuse **`var`**                             | **No.** Occupied on gaussian columns (var(Y), the β/SD(Y) colour).                                                                                                                                                                        |
| **derive** it, like `fmt_resid()`           | **Impossible.** The gap SE needs the *covariance* between two estimators. No function of the other fields contains it — §3.6 is the measurement that proves it.                                                                            |
| a table **attribute**                       | **No.** Per-cell quantity; and the colour engine reads one column, never the table.                                                                                                                                                       |
| **one new field** `gap_se`                  | **Recommended.**                                                                                                                                                                                                                          |

**What it holds: the standard error of the gap, on the test scale** (log-ratio when `ci_type` is
`or`/`ratio`, plain difference when `diff`) — i.e. the same scale `fmt_adjustment_score()` folds. NA
everywhere else, exactly like `obs`.

**Why an SE and not a z or a p.** An SE is strictly more informative and is what the *interesting*
policy needs:

| stored     | can gate? | can print p? | can print a gap CI? | supports the CI-floor reading? |
|------------|-----------|--------------|---------------------|--------------------------------|
| `gap_p`    | yes       | yes          | no                  | **no**                         |
| `gap_z`    | yes       | yes          | no                  | **no**                         |
| **`gap_se`** | yes     | yes          | **yes**             | **yes**                        |

**Field-count check against z6.** `dev/empty_vctrs_fields_sparse_record.md` set the re-open threshold at
"~30 fields"; this is the 21st, and it explicitly named the gap SE as the expected next one. The cost
is the known `/vctrs-field` pass plus one conscious golden regeneration — the same session shape as
`n_eff` (Last Phase s) and `obs` (z5), both of which landed with a script proving the only delta was an
all-NA column.

### 7.2 `sig_source` does NOT gain a third value — `MEASURES` gains a `bounds` closure

The roadmap asked: "What does `sig_source` become — a third value, or a new fact?" **A new fact, and
not on `sig_source`.**

`fmt_color_plan()` currently reads the interval directly:

```r
has_ci     <- cit %in% c("diff", "diff_row", "diff_col", "or", "ratio")
ci_neutral <- if (ci_mult) 1 else 0
sig_pos    <- has_ci & get_ci_inf(x) > ci_neutral
sig_neg    <- has_ci & get_ci_sup(x) < ci_neutral
```

Give `MEASURES` a **`bounds`** field — a closure of the same kind as the existing `raw`:

```r
# default (every existing measure): the stored interval
bounds = function(x) list(lo = get_ci_inf(x), hi = get_ci_sup(x))

# adjustment / between_groups: the GAP's interval, derived
bounds = function(x) {
  crit <- conf_level_to_z(getOption("tabxplor.conf_level", 0.95))
  g <- fmt_gap_raw(x)                     # signed gap on the test scale
  s <- get_gap_se(x)
  if (multiplicative) list(lo = exp(g - crit*s), hi = exp(g + crit*s))   # folded around 1
  else                list(lo = g - crit*s,      hi = g + crit*s)        # folded around 0
}
```

Then **every existing branch works unchanged**, because the derived interval is already on the measure's
own scale and `ci_mult` already selects the right neutral:

- `ignore` — unchanged (the descriptive change-in-estimate; z5's output is byte-identical);
- `grey_non_signif` — greys a gap whose interval covers the neutral. Correct with no new code;
- `guaranteed_effect` — takes the bound nearest the neutral, i.e. **"the model moved this effect by at
  least ×1.1"**. This is the *right* reading for this measure and it needs no `guar` override at all.

That last point deserves emphasis. z5 §4.4 assumed `guaranteed_effect` would have to mean "|z| of the
gap on an absolute residual scale", copying z4's `contrib` shape. It does not: `contrib` needed the
override precisely *because* it has no interval, and the gap **does** have one. So `adjustment` takes
the plain, original meaning of `guaranteed_effect` — the CI floor — which is both simpler and more
useful (a |z| grows with n; a guaranteed ×1.1 does not). **One less special case than expected.**

Sanity checks against the existing hard-wired sites, all re-verified today:

- the `guaranteed_effect` diff↔ratio bound rescale (`fmt_class.R:2880-2886`) is gated on
  `measure == "ratio"` / `"diff"` ⇒ does not fire; the derived bounds are already on the target scale
  (this is the ratio-flood bug the `/color-mode` skill warns about, avoided by construction);
- the SD standardisation (`:2815`) is `measure == "diff"` only ⇒ does not fire;
- the p-value-row warning colour (`:2999`) is `measure == "diff"` only ⇒ does not fire;
- `fmt_stars_applicable` (`:1583`) excludes `contrib` only ⇒ stars keep printing the **model** p, which
  is right: the cell prints the model estimate.

**`sig_source` keeps two values** and its meaning sharpens to "which stored quantity, `bounds` or
`pvalue`". The one measure that reads a p-value stays the one that has no interval.

### 7.3 `force_policy` is deleted

z5 added `force_policy = "ignore"` to both measures with a comment saying exactly why: *"a valid gap
test needs the joint variance of two estimates fitted on the same rows — see
dev/model_vs_observed_effect_colour.md §4"*. When that test lands, the field is removed, and
`measure_policy()` becomes a two-line identity function with one caller left (or none).

**Unless option 4.2(b) is chosen**, in which case `force_policy` survives in a *better* form: a
predicate on the column rather than a constant —

```r
# "this measure has no honest test on a non-collapsible scale"
force_policy = function(x) if (reg_scale_noncollapsible(x)) "ignore" else NULL
```

which is one signature change to `measure_policy(measure, policy, x)` and reuses the
`reg_fam_logscale()` / `reg_fam_prob()` predicate family that Last Phase z3 introduced for exactly this
kind of question. That is the integrated shape; it is not an extra layer.

### 7.4 The three policies, stated

| policy               | `adjustment` reads                                             | `between_groups` reads                            |
|----------------------|----------------------------------------------------------------|---------------------------------------------------|
| `ignore` (default)   | the point gap (change in estimate) — **z5's behaviour, byte-identical** | the point gap between the two groups     |
| `grey_non_signif`    | grey unless the gap's interval excludes the neutral            | same                                              |
| `guaranteed_effect`  | the **CI floor of the gap**: "moved by at least ×1.1"          | same                                              |

The colour **direction** stays z5's away-from/toward-the-null rule (`fmt_adjustment_score`); the
**gate** is two-sided on the raw signed gap. These differ in sign for a protective effect, which is
correct and is exactly the z4 precedent (`contrib`'s residual sign is `sign(ctr)` while its gate is a
two-sided p). Worth one `# DESIGN:` comment so nobody "fixes" it later.

### 7.5 Break scales — one now-or-never simplification

z5 added `adj_ratio` (×1.10/1.25/1.50/2.00, the change-in-estimate literature) and `adj_diff`
(0.02/0.05/0.10/0.20, absolute). Under §7.2 the `guaranteed_effect` reading uses **the same two
scales** (a CI floor is on the measure's own scale), so **no new break scale is needed at all**.

That is worth stating plainly because it contradicts the z5 forecast. But it raises a neighbouring
question that must be settled *before release*, since `set_color_breaks()` names are public:

> z4 named its absolute-z scale **`residual`**, because `contrib` was its only user. It is not a
> residual scale — it is a **z scale** (`conf_level_to_z(c(0.95, 0.99, 0.9999, 1 − 2e-9))` =
> 1.96/2.58/3.89/6). If any later measure ever wants an absolute-z reading, it will either abuse a
> misnamed scale or add a duplicate.

**Recommendation: rename `residual` → `zscore` now, keeping `residual` as a read-side alias** (the
`rr → ratio` pattern from Phase 17d.4). It costs one line in `mk_color_scale()`'s `valid` vector plus
the alias, and it is impossible after the CRAN freeze. If the maintainer prefers no churn, the
alternative is to record that `residual` is the z scale and never add a second one — but then the name
lies in the docs forever.

### 7.6 Display and tooltip

Both ride machinery that exists, and both are most of the perceived value:

1. **Tooltip** (html): `crude 2.44 [2.21; 2.68] → model 2.55 [2.28; 2.84]; ×1.045 [1.014; 1.077],
   p = 0.005`. `tab_kable_print_tooltip()` already composes per-cell tooltips and the reg path already
   appends `reg_empirical_tips`. **This is where the gap CI belongs** — it is too wide for a cell.
2. **A `{gap_p}` or `{gap_ci}` display token** — possible through the composite `{}` grammar, but three
   numbers per effect is more table than anyone reads. **Recommend not adding one**; the colour *is*
   the display, which is the whole premise of a colour measure.
3. **A footer sentence** — "Adjustment moved 4 of 11 effects significantly (p < 0.05)." One
   `tab_footer_streams` token, uses the same threshold as the colours so it cannot disagree.
4. **Legend wording** — non-negotiable: the legend must name what the significance is *of* ("the gap",
   not "the effect"), because the cell simultaneously shows stars for the model estimate. One
   `legend_resolve_spec` fact; the terse form would read
   `adjustment (obs, signif.): ×1.1 ×1.25 ×1.5 ×2`.

---

## 8. Cost

`gss_simple`, n = 21 483, 4 factor predictors, 18 parameters:

```
model fit                                          0.073 s
influence matrix for the model + 4 crude glm fits  0.220 s   (301 % of the fit)
    of which the 4 crude fits dominate; with the closed form of SS2.2 they are ~0.002 s
memory of one n x p influence matrix               2.9 MB    (transient, freed after the SE)
```

Realistic budget: **one extra `p × p` solve and one O(n·p) pass per model** — the same order as one
IRLS iteration, i.e. roughly 1/8 of a fit — plus a negligible closed-form pass per crude effect.
`between_groups` costs a subtraction and a square root.

The memory line is the only thing to watch: at n = 5 M and p = 50 the influence matrix would be 2 GB.
Mitigation is trivial and should be in the plan — the SE only needs **one column at a time**
(`IF %*% c` for the contrast), so the implementation should form the difference vector per term rather
than materialising the full matrix twice. Worth a `# WARNING:` tag.

---

## 9. Honest assessment of the roadmap's own framing

Three points where the z7 brief's phrasing does not survive contact with the measurements:

1. **"Is checking both confidence intervals don't overlap a cheap way?"** — No, and it is worth being
   blunt: it is not a conservative approximation, it is a test with an effective α around 1e-31 (z5
   §4.1, re-confirmed here with correlations 0.52–0.90). It would colour nothing, ever. The docs should
   warn readers off their own version of it.
2. **"...the free `ctr` on reg columns, or a derived value like `fmt_resid()`?"** — `ctr` is a field
   overload the codebase's own rules forbid, and derivation is *mathematically impossible* here (§7.1).
   The question has one answer.
3. **"...or is the descriptive change-in-estimate the honest scope?"** (z5 Q8) — the measurements say
   the test is honest on collapsible scales and misleading-if-uncaveated on the conditional-OR one. So
   the honest scope is **not** "no test"; it is "a test, plus a rule about where it is enabled". §4.2.

And one thing I would **cut** from the obvious feature list: a materialised gap column, a per-cell FDR
correction, and any automatic "confounder detected" flag. All three add a concept, none adds a reading
the colour does not already give.

---

## 10. Two smaller findings worth recording

- **`reg_empirical_columns()` ignores `at`** — the `at = "reference"` mismatch of §3.8. A pre-existing
  z5 defect, cheap to fix, and it must be fixed *before* a p-value is attached.
- **`multiplier` is a non-issue.** z5 §8 flagged that a `multiplier = c(var = k)` would raise the model
  OR to the power k while the crude column is not scaled. Verified: `multiplier` only touches
  **continuous** predictors, which have no crude twin at all, so `obs` is NA there and no gap is ever
  scored. The flag can be closed. (It is, however, the exact seam the companion report on numeric
  predictors examines.)

---

## 11. Implementation sketch — two phases, in this order

**Phase A — `between_groups` (small; no new statistics).**

1. `/vctrs-field` pass for the 21st field `gap_se`; conscious regeneration of the 36 structural
   `_golden/*.rds` + `_snaps/fmt-contract.md` with the usual proving script (only delta = an all-NA
   column). Bump `JMVTAB_CACHE_SCHEMA`.
2. `MEASURES` gains the `bounds` closure with a default; `fmt_color_plan()` reads `md$bounds(x)` instead
   of `get_ci_inf`/`get_ci_sup` directly. Byte-identical for all four existing measures — that is the
   test.
3. `reg_write_group_obs()` also writes `gap_se = sqrt(SE_ref² + SE_group²)`, with each SE recovered
   from that column's stored Wald bounds. Delete `between_groups`' `force_policy`.
4. Legend + docs + one fixture per policy, with a hand-computed z.

**Phase B — `adjustment` (the influence functions).**

5. `REG_EMPIRICAL` gains a **link** column; one closed-form `reg_crude_if()` driven by it.
6. `reg_model_if()`: `attr(fit, "influence")` when the fit is an `svyglm` (weighted / survey / `rr`),
   the six-line sandwich otherwise, and the rebuild-from-`(data, coef)` variant for the digest path.
   Design-based variance via `survey::svyrecvar()` when a design exists.
7. `reg_ame_if()` for `effect = "ame"` / `"ame_ratio"` (the two-term influence function of §3.4).
8. Write `gap_se` at the same point in `reg_build()` where `obs` is written; per-term difference
   vectors, never the full matrix twice (§8).
9. The §4.2 decision: caveat, or conditional `force_policy`.
10. Fix `at = "reference"` ⇒ `obs = NA` (§3.8).
11. Docs: `?tab_reg`, the regression vignette EN + FR (the §4.3 `gss_simple` table is the example), the
    multiplicity sentence, the "intervals overlap ≠ no difference" warning, NEWS.

**Risks, ranked.** (i) A reader takes a significant OR gap for confounding — mitigated by §4.2(b), not
by code, and it is the reason §4 leads this report. (ii) Memory on very large n — mitigated by the
per-term loop. (iii) The `residual`/`zscore` rename is a now-or-never call that will otherwise be
regretted quietly for years.

---

## 12. Open questions for the maintainer

- **Q1 — the OR path.** §4.2: (a) ship the significance policies everywhere with a caveat, consistent
  with the z5 Q6 ruling; **(b) recommended** — enable them only on collapsible scales
  (RR/IRR/β/AME/`ame_ratio`), informing once on the conditional-OR path and naming the three fixes; or
  (c) add a marginalisation mode. This is the only decision that changes what users see.
  **Maintainer’s decision: (b)**
- **Q2 — the field.** Confirm the 21st field `gap_se``gap_se` (§7.1), and its name (`gap_se` / `obs_se` /
  something else). It is internal, but it is visible through `$` and `mutate()`.
  **Maintainer’s decision: ok for `gap_se`**
- **Q3 — the `bounds` closure.** Confirm §7.2 — no third `sig_source` value, and `guaranteed_effect`
  meaning the **CI floor of the gap** rather than an absolute |z|. This is a smaller change than z5
  forecast; it also removes the need for any new break scale.
  **Maintainer’s decision: ok**
- **Q4 — the `residual` → `zscore` rename** (§7.5). Now or never. Recommended, with `residual` kept as
  a read-side alias.
  **Maintainer’s decision: do it now (don’t even keep residual as a read-side alias, it seems useless)**
- **Q5 — scope and order.** Phase A (`between_groups`) alone in one session, then Phase B? Or both
  together? A alone is genuinely small and de-risks the field pass.
  **Maintainer’s decision: two distinct phases**
- **Q6 — the tooltip.** Confirm the gap CI + p goes in the html tooltip and **not** into a display
  token (§7.6). Recommended.
  **Maintainer’s decision: ok for the tooltip**
- **Q7 — the footer sentence** ("adjustment moved 4 of 11 effects significantly"): wanted, or noise?
  **Maintainer’s decision: noise, not wanted**
- **Q8 — the `at = "reference"` fix** (§3.8/§10): confirm `obs = NA` there. It is a z5 defect either
  way.
  **Maintainer’s decision: fix it**
---

## 13. References

### Comparing estimates from two models on the same data

- Weesie J. (1999) "Seemingly unrelated estimation and the cluster-adjusted sandwich estimator",
  *Stata Technical Bulletin* 52, 34-47. — Stata's `suest`; the stacked-score joint sandwich, built as a
  cluster modification of the sandwich estimator (Rogers 1993), and explicitly contrasted with
  `hausman`'s `V(b) − V(B)`.
- Mize T.D., Doan L., Long J.S. (2019) "A general framework for comparing predictions and marginal
  effects across models", *Sociological Methodology* 49(1), 152-189. — **the canonical statement of
  exactly this feature**: seemingly unrelated estimation to test equality of predictions and marginal
  effects across nested models, across outcomes, across groups, and across model types. Stata package
  `mecompare`.
- Stefanski L.A., Boos D.D. (2002) "The calculus of M-estimation", *The American Statistician* 56(1),
  29-38. — the stacking construction.
- Saul B.C., Hudgens M.G. (2020) "The calculus of M-estimation in R with geex", *JSS* 92(2).
- Schenker N., Gentleman J.F. (2001) "On judging the significance of differences by examining the
  overlap between confidence intervals", *The American Statistician* 55(3), 182-186. — §3.6.
- Altman D.G., Bland J.M. (2003) "Interaction revisited: the difference between two estimates",
  *BMJ* 326, 219. — the independent two-estimate test used for `between_groups`.

### Influence functions, sandwich and survey linearization

- Hampel F.R. (1974) "The influence curve and its role in robust estimation", *JASA* 69(346), 383-393.
- Huber P.J. (1967); White H. (1980) — the sandwich.
- Binder D.A. (1983) "On the variances of asymptotically normal estimators from complex surveys",
  *International Statistical Review* 51(3), 279-292. — what `survey::svyrecvar` implements.
- Lumley T., **`survey`** package: `svyglm(..., influence = TRUE)`, `svyrecvar()`, `withReplicates()`,
  `svycontrast()`. Influence functions were exposed specifically so `svyby`/`svycontrast` could
  estimate covariances *between* estimates — the same problem, the same object.

### Collapsibility, confounding, and what the test rejects

- Greenland S., Robins J.M., Pearl J. (1999) "Confounding and collapsibility in causal inference",
  *Statistical Science* 14(1), 29-46.
- Maldonado G., Greenland S. (1993) *American Journal of Epidemiology* 138(11), 923-936. — the 10 %
  change-in-estimate rule that `adj_ratio`'s first break encodes.
- Janes H., Dominici F., Zeger S. (2010) "On quantifying the magnitude of confounding",
  *Biostatistics* 11(3), 572-582; and the BMC Med Res Methodol (2021) treatment of non-collapsibility
  in quantifying confounding bias in logistic regression. — the crude-adjusted difference is
  confounding **plus** non-collapsibility, and the two must be separated.
- *"The change in estimate fallacy"*, **Global Epidemiology** (2024) — the recent critique of using a
  crude-vs-adjusted comparison as a confounder-selection rule. Read before writing the vignette
  paragraph.

### Comparing coefficients across nested models and across groups

- Karlson K.B., Holm A., Breen R. (2012) "Comparing regression coefficients between same-sample nested
  models using logit and probit", *Sociological Methodology* 42, 286-313; Kohler U., Karlson K.B.,
  Holm A. (2011) *Stata Journal* 11(3), 420-438. — the KHB decomposition into **confounding** vs
  **rescaling**; R package `khb` (R-Forge), `matchingMarkets::khb`. A `@seealso` pointer, not something
  to build.
- Mood C. (2010) "Logistic regression: why we cannot do what we think we can do, and what we can do
  about it", *European Sociological Review* 26(1), 67-82.
- Allison P.D. (1999) "Comparing logit and probit coefficients across groups", *Sociological Methods &
  Research* 28(2), 186-208; Williams R. (2009) "Using heterogeneous choice models to compare logit and
  probit coefficients across groups", ibid. 37(4), 531-559. — §5.2.
- Clogg C.C., Petkova E., Haritou A. (1995) *AJS* 100(5), 1261-1293, with Allison's comment. — the
  classic nested-coefficient test; correct for linear models.

### In-repo companions

`dev/model_vs_observed_effect_colour.md` (z5 — the descriptive measure this one completes),
`dev/chi2_cell_residuals_and_contributions.md` (z4 — the one-measure-several-readings precedent),
`dev/poisson_vs_logistic_binary_outcome.md` (z3 — why a collapsible scale is available at all),
`dev/new_colors_UI.md` (the colour framework brief),
`dev/empty_vctrs_fields_sparse_record.md` (z6 — the field-count threshold this phase tests).
