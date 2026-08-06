# Colouring the gap between the modelled effect and the observed effect — design study

Date: 2026-08-05. Status: **REPORT ONLY** (Last Phase z5, step 1). No code written; the plan and the
implementation are a separate step.

Scope: a `tab_reg()`-only colour measure that scores **how far each model estimate sits from its
observed (crude) counterpart** — the `Obs_*` column that `empirical = TRUE` already builds — plus the
two neighbouring comparisons the maintainer raised (`split_var` groups, `predictors = list(...)`
model comparison).

Every number below was measured on this box today; the scripts are one-off and were not kept.

---

## 0. Executive summary

**The feature is cheap, and it is already 90 % built.** `empirical = TRUE` computes the crude effect
for exactly the rows the model column occupies, aligned to the same skeleton by `reg_skel_match()`.
Nothing new has to be *computed*; one number has to be *carried* into the model column so the
per-column colour engine can see it.

Six findings govern the design:

1. **The comparison is only interpretable as "what adjustment did" on a *collapsible* scale.**
   Measured on a simulation with the covariate **independent of the exposure — zero confounding by
   construction** — the crude→adjusted OR still moved **+7.9 %**, while the RR moved **+0.26 %** and
   the AME **+0.0016** (absolute). The odds ratio is non-collapsible; a large part of "the model
   changed the effect" is arithmetic, not confounding. §3.
2. **Non-collapsibility is the same size as the threshold users will read it against.** The
   epidemiological rule of thumb for "meaningful confounding" is a 10 % change in estimate; the
   measured null-confounding OR change was 7.9 %. So on the OR scale the measure's first colour step
   would fire on an artefact. This is a documentation-and-defaults problem, not a blocker. §3.
3. **CI overlap is cheap but not sound — it is not even approximately a test here.** Crude and
   adjusted come from the *same rows*: measured bootstrap correlation **0.944**, so the naive
   independent SE is **4.2× too large**, and the non-overlap criterion demands a gap of **11.6 true
   SEs** — an effective α of about **3·10⁻³¹**. It would colour nothing, ever. §4.1. **Reject.**
4. **There is a cheap AND sound alternative**: the difference of the two estimators' **influence
   functions** (stacked M-estimation). It needs no refit and no resampling. Measured against an
   800-replicate bootstrap it reproduced SE(difference) to **1–2 %** unweighted and **2.4 %** weighted,
   and it reproduced `svyglm`'s own SE to **1.000** when applied to a single model. It ran **187×
   faster** than a 200-replicate bootstrap on n = 20 000. §4.2. But it needs the model frame, which the
   jamovi digest cache does not keep — so it is a **phase 2**, not a phase 1. §4.4.
5. **For `split_var` the cheap test *is* the right test.** The groups are disjoint, so the estimates
   are independent: measured bootstrap correlation **+0.041**, and `sqrt(SE_A² + SE_B²)` reproduced the
   true SE of the difference to **1.2 %**, giving a z whose p (0.00319) matched the LRT interaction
   p (0.00322) to three digits. This is a genuinely different, genuinely easy case. §9.
6. **One measure, read with the existing policy axis, is the right shape** — exactly the Last-Phase-z4
   `contrib` pattern. `ignore` = the descriptive change-in-estimate; and if phase 2 lands,
   `guaranteed_effect` = the standardised gap on an absolute z scale, through the `guar` override
   field that already exists in `MEASURES`.

**Recommended name: `color = "adjustment"`** (not `"observed"`, which reads as "colour the observed
value"). §5.1.

**Recommended storage: one new 20th fmt field, `obs`**, holding the observed counterpart of this
cell's estimate, on the model's own scale. The zero-field alternative (reuse the `ratio` field, which
is all-NA on every reg column — verified) works but re-creates the exact field-overload the 2.0.0
rewrite removed, and it forecloses the tooltip / display-token / phase-2 work. §6.

**Recommended default direction: toward vs away from the null**, not raw up/down — otherwise a
protective effect (OR < 1) colours backwards. §7.3.

---

## 1. The framework: which real question does this answer?

tabxplor's colour measures each answer one reader question. The existing four, plus the proposed
fifth:

| #     | the reader's question                                      | measure          | reference frame         |
|-------|------------------------------------------------------------|------------------|-------------------------|
| 1     | "How does this cell differ from a group I chose?"          | `diff`           | a chosen reference row  |
| 2     | "…relatively?"                                             | `ratio`          | a chosen reference row  |
| 3     | "…on the odds scale?"                                      | `or`             | a reference category    |
| 4     | "Which cells build the association in this table?"         | `contrib`        | independence            |
| **5** | **"Which effects did the model change, and by how much?"** | **`adjustment`** | **the observed effect** |

Question 5 is the one `empirical = TRUE` exists to serve. Today the table *shows* both numbers and
asks the reader to divide them mentally, column by column, row by row. Colour is precisely the right
device for "scan the table and see where they diverge" — the same argument that made `contrib` worth
having.

The measure's reference is **a column**, not a row. That is new for tabxplor, and §6 is entirely about
that.

### 1.1 What the measure is *not*

It is not a confounding test, and it must not be named or worded as one:

- On a **collapsible** scale (β, risk difference/AME, RR, IRR) with the same population and the same
  complete-case rows, the gap is confounding by the adjustment set — plus any effect-measure
  modification the model's additivity assumption averages over.
- On the **odds ratio** it is confounding **plus non-collapsibility**, and §3 shows those are the same
  order of magnitude.
- It is never a causal claim. "Adjustment moved this effect" is a statement about two regressions, not
  about a world.

---

## 2. What already exists (verified against the source today)

### 2.1 The crude companion

`REG_EMPIRICAL` (`R/tab_reg.R:1207-1233`) is a per-family fact table giving, for each family, a
**base** column and an **effect** column with their fmt shape and CI method:

| family key                        | base column                             | effect column (`do_exp = TRUE`) | effect column (`do_exp = FALSE`) | effect CI            |
|-----------------------------------|-----------------------------------------|---------------------------------|----------------------------------|----------------------|
| `binomial`                        | `Obs_%`                                 | `Obs_OR`                        | `Obs_log(OR)`                    | Woolf log-OR         |
| `rr`                              | `Obs_%`                                 | `Obs_RR`                        | `Obs_log(RR)`                    | Katz log-RR          |
| `gaussian`                        | `Obs_mean`                              | `Obs_diff`                      | (same)                           | pooled Student t     |
| `poisson`                         | `Obs_rate`                              | `Obs_IRR`                       | `Obs_log(IRR)`                   | quasi-Poisson ratio  |
| binomial + `effect = "ame"`       | `Obs_%`                                 | `Obs_diff`                      |                                  | Wald risk difference |
| binomial + `effect = "ame_ratio"` | `Obs_%`                                 | `Obs_RR`                        |                                  | Katz log-RR          |
| `multinomial`                     | — (tooltips only, `reg_empirical_tips`) |                                 |                                  |                      |
| `ordinal`                         | — (nothing)                             |                                 |                                  |                      |

Three properties make this feature cheap:

- **The rows already align.** Model columns match the skeleton by `term`; crude columns match it by
  `(var, level)` through `reg_skel_key()`/`reg_skel_match()` (`:558-569`). Both return vectors of
  length `nrow(skeleton)` in the same order. Rows with no crude counterpart (`Constant`, numeric
  predictors) are `NA` on the crude side and would simply be uncoloured.
- **The population already matches.** Since Phase 17h, both sides use `reg_complete_frame()`
  (`:789`), so the crude reference level, the complete-case rows and the n are the model's.
- **The scale already matches.** The whole point of the `rr` arm added in Last Phase z3 is that the
  crude companion is on the *same* scale as the model column (Katz RR beside a modified-Poisson RR,
  never a Woolf OR). So `Model / Obs` is always a like-for-like contrast — by construction, not by
  luck.

### 2.2 The colour engine, and the one constraint that shapes everything

`fmt_color_plan(x, channel, color, signif)` (`R/fmt_class.R:2761-2952`) takes **one fmt column**. Its
raw value comes from `md$raw(x)`, a closure over that column alone; its reference is broadcast *down
the column* by `get_ref_field()` (`:1683`). There is no table-level colour hook, and adding one would
break the invariant that the same object renders correctly to console / HTML / Excel / ggplot with a
per-medium palette.

**Therefore a cross-column measure must be resolved at build time into a per-cell field.** That is
already how `or` works in `tab()` (the 2×2 odds ratio against a reference *column* is computed in
`tab_apply_reference()` and stored in the `or` field). This measure follows the same precedent.

### 2.3 What a new measure costs (from the `/color-mode` checklist, re-verified)

Mandatory: one `MEASURES` row (`fmt_class.R:3108`); the `std_when` switch arm (`:2803`); the scale
keys in `default_color_scales()` (`tab_classes.R:3694`) and `mk_color_scale()`'s `valid` vector
(`:3624`) plus its `center`/`strict` rules (`:3629-3630`); `ok_measure` in `normalize_color_spec()`
(`tab.R:845`); the `ok` list in `resolve_color_channels()` (`fmt_class.R:1149`, and `:1154` +
`tab.R:872` if background-capable); a `legacy_union()` arm (`tab.R:886`); `legend_method_name()`
(`:3371`); the potools anchor (`:3440`) if the legend word is translated.

That is a well-trodden path. Nothing about it is novel here.

---

## 3. Statistical soundness — the collapsibility audit

This is the section that decides which families get the measure without a caveat.

### 3.1 What "collapsible" means, and where the term comes from

**The definition.** An effect measure is **collapsible** over a covariate Z when its *marginal*
(Z-ignored) value is a weighted average of its *stratum-specific* (Z-conditional) values. The
consequence is the one that matters here: if the measure is collapsible and Z is not a confounder,
then crude = adjusted, so any gap the table shows **is** confounding. If it is not collapsible, crude
≠ adjusted **even when Z is not a confounder at all**.

Collapsibility is a property of the *arithmetic of the measure*. Confounding is a property of the
*causal structure of the data*. They are logically independent — that separation is the entire point
of Greenland, Robins & Pearl (1999) — and conflating them is the commonest misreading of a
crude-vs-adjusted table.

**A worked example containing no sampling at all** (exact arithmetic, so nothing below is a simulation
artefact). Take Z ⟂ X with P(Z=0) = P(Z=1) = ½, and choose the risks so that the odds ratio for X is
**exactly 2 in each stratum**:

|              | X=0    | X=1    | stratum OR | stratum RR | stratum RD |
|--------------|--------|--------|------------|------------|------------|
| Z=0          | 0.10   | 0.1818 | 2.0000     | 1.8182     | +0.0818    |
| Z=1          | 0.60   | 0.7500 | 2.0000     | 1.2500     | +0.1500    |
| **marginal** | 0.3500 | 0.4659 | **1.6201** | 1.3312     | +0.1159    |

- **The risk difference collapses exactly**: +0.1159 = ½(+0.0818) + ½(+0.1500). Weights = P(Z=z).
- **The risk ratio collapses exactly**: 1.3312 = (0.1·1.8182 + 0.6·1.2500) / (0.1 + 0.6). Weights ∝
  the *unexposed* stratum risks.
- **The odds ratio does not collapse**: the marginal OR is 1.6201, not 2 — it keeps only **69.6 %** of
  the stratum log-OR. The proof is one line: a weighted average of 2 and 2 is 2 for *any* weights, so
  1.6201 cannot be a weighted average of the stratum ORs. Z is not a confounder here; the marginal and
  conditional odds ratios are simply **different quantities**.

**Why it happens.** The odds transformation `p ↦ p/(1−p)` is convex, so averaging risks and *then*
forming an odds ratio is not the same as forming odds ratios and *then* averaging them (Jensen).
Mixing over an outcome-predictive Z therefore always pulls a marginal OR **toward 1** when the stratum
ORs are equal and ≠ 1 — never away. That is the same phenomenon §3.2 measures from the other
direction (crude marginal 1.829 → adjusted conditional 1.974 with a true conditional OR of 2.000), and
the same arithmetic that appears elsewhere under other names: the population-averaged vs
subject-specific gap between GEE and GLMM (Zeger, Liang & Albert 1988), and the "unobserved
heterogeneity rescales logit coefficients" of the sociological literature (Mood 2010) that §10
revisits.

**Where the word comes from.** From contingency-table analysis, where the question was literally
"can this three-way table be *collapsed* over Z without distorting the X–Y association?" (Bishop 1971;
Bishop, Fienberg & Holland 1975; Whittemore 1978 — Simpson's paradox is the extreme case). Miettinen
& Cook (1981) separated it from confounding for epidemiology; Greenland, Robins & Pearl (1999) is the
canonical modern statement.

**The catalogue a tabxplor user needs**: risk difference — collapsible; risk ratio — collapsible; rate
ratio (Poisson counts) — collapsible; linear-model β — collapsible; **odds ratio — not**; hazard ratio
(Cox) — not.

### 3.2 The measurement

Simulated `n = 200 000`: `X ~ Bern(0.5)`, `Z ~ Bern(0.5)`, **X ⟂ Z by construction**, and
`Y ~ Bern(logit⁻¹(−0.5 + log(2)·X + log(4)·Z))`. Because X and Z are independent, Z is **not a
confounder**: the true crude and adjusted effects of X are the same causal quantity. Any crude →
adjusted movement is arithmetic.

| scale                                     | crude  | adjusted | change               |
|-------------------------------------------|--------|----------|----------------------|
| **odds ratio** (logistic)                 | 1.829  | 1.974    | **+7.9 %**           |
| **risk ratio** (Poisson)                  | 1.2598 | 1.2631   | **+0.26 %**          |
| **risk difference / AME** (g-computation) | 0.1416 | 0.1432   | **+0.0016 absolute** |

The conditional OR is 2.000 by construction; the *marginal* OR is 1.829. That gap is
non-collapsibility (Greenland, Robins & Pearl 1999): the marginal OR of a logistic model is attenuated
toward the null relative to the conditional one whenever the covariate predicts the outcome, whether
or not it confounds. RR and risk difference are collapsible — the marginal effect is a weighted
average of the stratum-specific effects — so they move only when there is real confounding.

### 3.3 Consequence for the measure

| family / effect                       | model column         | scale is…           | the gap means                                     |
|---------------------------------------|----------------------|---------------------|---------------------------------------------------|
| `gaussian`                            | `Model_β`            | collapsible         | confounding (+ modification averaged) — **clean** |
| `binomial`, `effect = "ame"`          | `Model_AME`          | collapsible         | confounding — **clean**                           |
| `binomial`, `effect = "ame_ratio"`    | `Model_RR`           | collapsible         | confounding — **clean**                           |
| `family = "poisson"` on binary (`rr`) | `Model_RR`           | collapsible         | confounding — **clean**                           |
| `poisson` counts                      | `Model_IRR`          | collapsible         | confounding — **clean**                           |
| `binomial` coefficient                | `Model_OR`           | **non-collapsible** | confounding **+ non-collapsibility** — caveat     |
| `binomial`, `exponentiate = FALSE`    | `Model_β` (log-odds) | **non-collapsible** | same, on the log scale                            |
| `multinomial`                         | (no Obs column)      | —                   | **not available**                                 |
| `ordinal`                             | (no Obs column)      | —                   | **not available**                                 |

**This is a strong argument for the feature, not against it.** Last Phase z3 added `family = "poisson"`
on a binary outcome precisely so a user could get a collapsible RR; and `effect = "ame"` /
`"ame_ratio"` give collapsible marginal quantities. The measure works cleanly on every one of them.
The recommendation is therefore:

- ship the measure for **all** families that have a crude companion (refusing OR would be
  paternalistic and would break the single most common table);
- but make the **legend and the docs name the caveat once, on the OR path only**, e.g. a legend tail
  "part of an OR gap is non-collapsibility, not confounding — see `?tab_reg`";
- and teach in the regression vignette that a *confounding* reading wants `effect = "ame"` or
  `family = "poisson"`.

### 3.4 Real-data sanity check (`gss_simple`, n = 13 015)

`married ~ race`, adjusted for `rincome`:

| scale      | Black                     | Other                          |
|------------|---------------------------|--------------------------------|
| OR change  | +5.3 %                    | +6.4 %                         |
| RR change  | +3.6 %                    | +3.2 %                         |
| AME change | +0.0151 (−7.1 % relative) | +0.0157 (**−60.5 %** relative) |

Two lessons. First, the OR change is roughly the RR change *plus* a non-collapsibility component of
the size §3.1-3.2 predict. Second — and this is a defaults decision, §7.2 — the **relative** change of an
*additive* effect is unstable: `Other`'s crude AME is −0.0259, near the null, so a +0.0157 absolute
shift reads as −60.5 %. Additive families must be coloured on the **absolute** gap, multiplicative
families on the **ratio**.

### 3.5 Other caveats worth one line each in the docs

- **The crude and adjusted estimates must share a population.** They do (`reg_complete_frame`), but if
  a future change breaks that, the gap silently becomes "confounding + selection".
- **Effect-measure modification.** A single adjusted coefficient is one number where the truth may be
  stratum-specific; the gap absorbs that. Not fixable, worth a sentence.
- **Multiplicity.** As with every other tabxplor significance (`new_colors_UI.md` W11), any phase-2
  test is per-cell at `conf_level`, uncorrected. Consistent with the rest of the package.
- **`split_var` + a marginal estimand.** An AME standardised within a group is standardised to *that*
  group's covariate distribution, so two groups' AMEs are not on a common standard. Already a known
  `split_var` caveat; it applies to the group-vs-group contrast of §9, not to model-vs-observed.

---

## 4. Significance — can this ride `color_signif`?

### 4.1 Confidence-interval overlap: cheap, and wrong

Measured, `n = 3000`, 600 bootstrap replicates, adjusted vs crude log-OR:

```
SE(crude) = 0.0766   SE(adjusted) = 0.0803   correlation = 0.944
TRUE SE(adjusted − crude)                    = 0.0264
naive independent  sqrt(SE1² + SE2²)         = 0.1110    (4.2× too large)
non-overlap needs a gap of 1.96·(SE1+SE2)    = 0.3075  = 11.6 true SEs
                                       ⇒ effective α ≈ 3·10⁻³¹
```

Two independent failures compound:

1. **The overlap criterion is conservative even for independent estimates** (Schenker & Gentleman
   2001): requiring `1.96·(SE₁+SE₂)` where the test needs `1.96·√(SE₁²+SE₂²)` gives a real α near
   0.005, not 0.05.
2. **These two estimates are not independent — they are computed from the same rows**, and the
   measured correlation is 0.944. Positive correlation makes the *true* SE of the difference much
   smaller than the independent one, so the criterion becomes more conservative still, not less.

The crude and adjusted CIs of a real table overlap almost completely by construction. A colour gated
on non-overlap would be blank on every table anyone will ever build. **Reject it — and say so in the
docs**, because "the intervals overlap so there is no difference" is the exact mistake this feature
would otherwise teach.

### 4.2 The robust, modern, and (surprisingly) cheap way: stacked influence functions

#### What these words mean, and where they come from

**M-estimation.** Nearly every estimator tabxplor touches is defined as the solution of an *estimating
equation*: find θ̂ such that `Σᵢ ψ(Oᵢ, θ̂) = 0`, where ψ is one function evaluated at one observation
at a time. The sample mean is `ψ = y − θ`. OLS and every GLM/MLE take ψ = the score,
`xᵢ(yᵢ − μᵢ)` for a canonical link. Robust regression, GEE and quantile regression are the same shape
with a different ψ. Huber (1964) called these **M-estimators** ("maximum-likelihood-*like*"), and the
point of the name is that everything below follows from the *shape of the definition*, not from any
distributional assumption.

The crude effect is one too, which is what makes this applicable here: a logistic regression on a
single factor is **saturated**, so its coefficients *are* the observed 2×2 log-odds ratios that
`reg_empirical()` computes by hand. Crude and adjusted are two M-estimators on the same rows,
differing only in ψ.

**The influence function.** Hampel (1974) asked: how much does θ̂ move if you add an infinitesimal
amount of data at one point `o`? The answer — the derivative of the estimator, viewed as a functional
of the data distribution, in the direction of a point mass at `o` — is the **influence function**
`IF(o)`. For an M-estimator it has a closed form,

```
IF(o) = A⁻¹ ψ(o, θ)          A = −E[∂ψ/∂θ]        (the "bread")
```

and it delivers the one property everything here rests on, **asymptotic linearity**:

```
θ̂ − θ  ≈  (1/n) Σᵢ IF(Oᵢ)
```

In words: *every M-estimator is, to first order, an average of one number per observation.* Once those
n numbers are in hand, the estimator's variance is simply their empirical variance — which, written
out, is the Huber–White sandwich `A⁻¹BA⁻¹` (Huber 1967; White 1980). Nothing new so far: this is
already how robust GLM standard errors and every `survey` standard error are computed.

**Stacking** is the step that solves *our* problem. Because both estimators are averages of
per-observation terms evaluated on **the same observations**, the difference is the average of the
differences:

```
(θ̂_adj − θ̂_crude) − (θ_adj − θ_crude)  ≈  (1/n) Σᵢ [ IFᵢ^adj − IFᵢ^crude ]
```

so `Var(θ̂_adj − θ̂_crude)` is the empirical variance of a single vector — **with the covariance
between the two estimators already inside it**, because the same row contributes to both terms. That
is exactly the quantity §4.1 showed we cannot get by pretending the estimates are independent, and it
arrives without a single refit. Formally one says the two estimating equations are *stacked*: treat
`(θ_adj, θ_crude)` as one parameter solving one system with `ψ* = (ψ_adj, ψ_crude)`, take the joint
sandwich, and read off any contrast. Stefanski & Boos (2002) is the standard pedagogical account (the
stacking construction is theirs); the R package **geex** (Saul & Hudgens 2020) implements it
generically.

**Where this already lives in our own stack.** The same object travels under several names the survey
literature will make familiar: *linearization* or *Taylor-series variance estimation* (Binder 1983),
the *delta method*, the *infinitesimal jackknife* (Jaeckel 1972; Efron 1982). `survey` computes the
variance of an `svyglm` coefficient by applying its design-variance formula to precisely these
estimated influence functions. That is why the weighted measurement below reproduces `svyglm`'s own
SE to a ratio of **1.000** rather than merely approximating it — and why strata, clusters and FPC
would come along for free through `svyrecvar`, instead of through new code of ours.

#### The method

Both estimators are M-estimators on the same rows. Stack their estimating equations and the joint
sandwich falls out of the per-observation influence functions, which for a GLM are available in closed
form from quantities the fit already carries:

```
IF_i  =  A⁻¹ · x_i · w_i · (y_i − μ_i)          A = Σ_i w_i v(μ_i) x_i x_iᵀ   (the bread)
Var(θ̂_adj − θ̂_crude)  =  Σ_i ( IF_i^adj[k] − IF_i^crude[k] )²
```

No refit, no resampling, one pass over the rows. Validated against an 800-replicate nonparametric
bootstrap (`n = 3000`, logistic):

| scenario                      | observed gap D | SE (influence) | SE (bootstrap) | ratio     | z      | p       |
|-------------------------------|----------------|----------------|----------------|-----------|--------|---------|
| no confounding (γ = 0)        | +0.0584        | 0.0251         | 0.0246         | **1.021** | +2.33  | 0.020   |
| real confounding (γ = 1.2)    | −0.3115        | 0.0299         | 0.0302         | **0.990** | −10.41 | 2·10⁻²⁵ |
| **survey-weighted** (γ = 1.2) | −0.2918        | 0.0312         | 0.0304         | **1.024** | −9.37  | —       |

The weighted row is the important one: with the bread taken as the *weighted* inverse information and
the influence functions centred and scaled by `n/(n−1)`, applying the same code to a single model
reproduced `svyglm`'s own SE **exactly** (ratio 1.000) — which is expected, because this *is* how
`survey` computes variances. So the method is design-based for free, and would extend to
strata/clusters through `survey::svyrecvar` rather than through new code.

Timing, `n = 20 000`, including both fits: **0.070 s** for the influence-function route vs **13.06 s**
for a 200-replicate bootstrap — **187×**. Even in a live jamovi UI the influence-function route is
free; the bootstrap is not.

Note the γ = 0 row: the gap is +0.0584 with p = 0.020 **when there is no confounding at all**. That is
non-collapsibility being correctly detected as a real, non-zero difference between two different
estimands. It is another reason the OR path needs the §3.3 caveat: the test is right, but what it
rejects is not "no confounding".

### 4.3 Rejected alternatives, for the record

- **Hausman-style `Var(D) = Var(crude) − Var(adj)`.** Requires one estimator to be efficient under the
  null. In logistic regression adjusting for a predictive covariate *increases* the coefficient's SE,
  so the subtraction routinely goes negative. Unusable.
- **Clogg–Petkova–Haritou (1995) / Allison (1995).** The classic sociological test for comparing
  coefficients between nested models. Correct for linear models; for logit it inherits the rescaling
  problem (§10) that KHB was invented to fix.
- **KHB (Karlson, Holm & Breen 2012).** The right tool for *decomposing* a nested logit comparison
  into confounding vs rescaling, via auxiliary residualised regressions. It answers a different, more
  ambitious question (mediation-style decomposition) and costs one auxiliary fit per covariate. Worth
  a `@seealso` pointer to the **khb** package, not worth building here.
- **Nonparametric bootstrap.** Gold standard and family-agnostic, but 187× the cost and it would have
  to be reimplemented for survey designs. A reasonable opt-in escape hatch later
  (`adjustment_se = "bootstrap"`), never the default.

### 4.4 …but it is a phase 2, and here is why

`fmt_color_plan` has room for exactly one significance source per measure (`sig_source ∈ {bounds,
pvalue}`), and on a model column **`ci_inf`, `ci_sup` and `pvalue` are all occupied by the model
estimate's own interval**, which the cell prints. A gap test therefore needs a *second* stored number
beyond the observed effect.

It also needs the **model frame**, which the jamovi `.fit_cache` digest path deliberately does not
keep (`reg_build_digest` stores coef + vcov only, and `reg_reref_fit_res` recomputes references from
contrasts). Influence functions need `X`, `μ` and `w`. So the digest path would have to either carry a
compact summary or degrade to "no gap test" — a real integration decision, not a detail.

**Recommendation:** ship phase 1 with `color_signif = "ignore"` only (the descriptive
change-in-estimate), and have the other two policies emit one clear message and fall back to `ignore`
rather than silently colouring something untested. Phase 2 adds the influence-function z and unlocks
them, following the z4 shape exactly: `grey_non_signif` greys where the gap is not significant, and
`guaranteed_effect` scores **|z| of the gap** on an absolute `residual`-style scale — through the
`guar` override field, which already exists and which `contrib` is currently the only user of.

---

## 5. The user-facing API

### 5.1 Name

| candidate                                  | verdict                                                                                                                                             |
|--------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------|
| `"observed"` (the maintainer's suggestion) | reads as "colour the observed value"; the measure is the *gap*, not the observation                                                                 |
| `"confounding"`                            | overclaims — false on the OR path (§3), and causal                                                                                                  |
| `"change"`                                 | the epidemiological term ("change in estimate") but vague in a colour argument                                                                      |
| **`"adjustment"`**                         | **recommended**: one word, teaches the concept, true on every family, and honest (it names the operation, not its interpretation). FR *ajustement*. |

Legend word: *adjustment (model vs observed)*; terse form `adjustment (obs): ×1.1 ×1.25 ×1.5 ×2`.

### 5.2 Argument shape — no new argument

`color = "adjustment"` on `tab_reg()`. Nothing else. It is a value on the existing measure axis, so it
composes with everything: the channel grammar, `color_signif`, `color_breaks`, `conf_level`.

**Auto-force `empirical = TRUE`**, with a `cli_inform`. There is precedent: `color = "contrib"` forces
`chi2 = TRUE` and `totrow = TRUE` in the resolve cascade (`R/tab-resolve.R:137-144`). `tab_reg()` has
no cascade, so this is three lines in the argument-validation block. Refusing instead would be an
avoidable error message for the obvious call.

### 5.3 Channel — allow both, and make the two-channel form the headline

`contrib` and `or` are banned from the background because they are whole-cell measures. `adjustment`
is a whole-cell measure too, but banning it would forfeit the single most useful reading this feature
enables:

```r
tab_reg(d, dependent = "married", predictors = c("race", "rincome"),
        empirical = TRUE, color = c("OR", "adjustment"))
#        text = the effect size          background = how much adjustment moved it
```

One glance answers both "how strong" and "how much of it survives adjustment". That is the
"readable at a glance" outcome the brief asks for, and it costs one entry in an allow-list.

Default unchanged: `color = TRUE` on a reg table keeps resolving to the effect measure alone. This
measure is opt-in, so no existing output moves.

### 5.4 Degradation

`multinomial` and `ordinal` have no crude effect column. `color = "adjustment"` there must inform once
and fall back to the family's normal measure — never error, never colour nothing silently. (The
multinomial crude numbers exist as tooltips, `reg_empirical_tips`; a tooltip cannot feed a colour.) 

---

## 6. Storage — the one real design decision

The observed effect must reach the model column as a per-cell number. Verified today, on every family,
by extracting each column's fields: **on a `Model_*` column the fields `n, wn, pct, mean, ratio, ctr,
var, tot_n, n_eff` are all `NA`** (plus `diff` for multiplicative families and `or` for additive
ones). So there is physical room. The question is which kind of room.

### Option A — a 20th fmt field, `obs` **(recommended)**

Holds the observed counterpart of this cell's estimate, on the model's own scale (an OR beside an OR,
an RR beside an RR, a β beside a crude mean difference, an AME beside a crude risk difference). `NA`
on every crosstab column, on `Constant`, on numeric predictors, and on every column that has no crude
twin.

- **Cost:** the `/vctrs-field` checklist — `new_fmt()`, `fmt()`, the three `vec_cast` methods,
  `vec_ptype2`, the `vec_arith` ± and ×÷ arms, the `vec_math` sum/mean arms, reset-to-NA on
  arithmetic — plus one conscious regeneration of the 36 structural `_golden/*.rds` and the
  fmt-contract snapshot. **This is exactly the Last-Phase-s `n_eff` pass**, which was done in one
  session with a script proving the only delta was the added all-NA column.
- **Gains beyond colour**, all of which Option B forecloses:
  - a `{obs}` display token → `Model_OR` can print `2.31 (obs 2.05)` through the existing composite
    `{}` grammar, with no new machinery (`resid` did exactly this in z4);
  - the HTML tooltip "crude 2.05 → adjusted 2.31 (×1.13)" for free;
  - phase 2 has a place to stand;
  - the number means one thing, so nothing downstream has to know which family it came from.

### Option B — reuse the free `ratio` field, pre-computed

Store the *score* itself: the ratio `θ_model / θ_obs` for multiplicative families, the difference
`θ_model − θ_obs` for additive ones, both in `ratio`; `raw = get_ratio`, and the fold centre (1 or 0)
comes from the scale, which is already how the engine works.

- **Zero new fields, zero golden churn.** Genuinely cheaper today.
- **But** it puts two different quantities (a ratio and a difference) in one field depending on the
  family — which is precisely the `mean`-overload that 2.0.0 §3 removed and that this codebase's own
  rules forbid ("facts live in ONE table", "no ad hoc layers"). `$ratio` is user-visible through
  `mutate()`, so the lie is public API.
- It also cannot serve the display token or the tooltip (a reader wants the crude *value*, not the
  ratio), and it leaves phase 2 with nowhere to put the z.

### Option C — a table-level colour hook reading the neighbouring column

Rejected. It would break the render-time-per-column invariant that lets one object render to four
media with four palettes, for no gain over Option A.

**Recommendation: Option A.** Option B is the fallback if the maintainer wants this in a single short
session and is willing to give up the tooltip and the display token.

---

## 7. The engine

### 7.1 One `MEASURES` row

```r
adjustment = list(
  word = "adjustment", word_i18n = TRUE,
  break_over = .lg_times, break_under = .lg_div,   # multiplicative arm; see 7.2
  break_scale = FALSE, ref_kind = "observed", threshold_mult = TRUE,
  unit_kind = "none", has_ref_lead = FALSE,
  raw = function(x) <see 7.2/7.3>,
  scale = c(std = "adj_diff", pct = "adj_ratio"),
  std_when = "additive",        # NEW switch arm: use_std <- !ci_mult
  sig_source = "bounds",        # phase 1: unused (ignore only)
  gate_row = "refrow")
```

### 7.2 Additive vs multiplicative dispatch

`std_when`'s existing three arms key on column *type*, which does not separate the two here:
`Model_OR` (multiplicative) and `Model_AME` (additive) are both `type = "row"`. What *does* separate
them is `ci_type`, which `fmt_color_plan` already computes as `ci_mult <- cit %in% c("or", "ratio")`
(`:2831`). So **one new `switch` arm** — the checklist's sanctioned extension point — selects the
scale:

| family / effect              | `ci_type`     | score             | scale                 |
|------------------------------|---------------|-------------------|-----------------------|
| OR / IRR / RR / `ame_ratio`  | `or`, `ratio` | `θ_model / θ_obs` | `adj_ratio`, centre 1 |
| β / AME / log(OR) / log(IRR) | `diff`        | `θ_model − θ_obs` | `adj_diff`, centre 0  |

Two new scale keys, both added to `mk_color_scale()`'s `valid` vector; `adj_ratio` also to the
multiplicative `center` list.

**Proposed defaults**, anchored on the change-in-estimate literature (Maldonado & Greenland 1993 — the
10 % rule) rather than invented:

```r
adj_ratio = c(1.10, 1.25, 1.50, 2.00)   # "adjustment moved the effect by ≥10 % / 25 % / 50 % / 2×"
adj_diff  = NULL -> standardized c(0.10, 0.25, 0.50, 1.00) of |θ_obs|   # see below
```

The additive default is the open question (§13, Q3). §3.4 measured why a plain relative change fails
on additive effects: a near-null crude effect makes the ratio explode (−60.5 % for a +0.0157 absolute
shift). Three candidates, in order of preference:

1. **Absolute, in the effect's own units** (`c(0.02, 0.05, 0.10, 0.20)` for an AME = 2/5/10/20
   percentage points) — honest, stable near zero, immediately readable, and it matches how `pct_diff`
   already works. Needs one scale per unit family, which the `std`/`pct` key pair gives.
2. **Standardised by SD(Y)**, reusing the `var` field that `Model_β` already carries — coherent with
   how numeric `diff` is standardised today, but the units are unfamiliar for a probability-scale AME.
3. **Relative to |θ_obs| with a floor** — rejected: an arbitrary floor is exactly the ad hoc layer this
   codebase forbids.

### 7.3 Direction — toward or away from the null, not up or down

The obvious `sign(log θ_model − log θ_obs)` colours a protective effect backwards: for a crude
OR = 0.50 attenuated to 0.60, the log difference is *positive*, so "attenuated" would render in the
over-representation pole, while an identical attenuation of a risky effect (2.00 → 1.67) would render
in the under pole. The two halves of a diverging palette would then mean nothing consistent.

**Recommend scoring the movement relative to the null:**

```
score = |log θ_model| − |log θ_obs|        (multiplicative)
score = |θ_model| − |θ_obs|                (additive)
```

so one pole always means **the model strengthened this effect** (suppression / negative confounding)
and the other always means **the model attenuated it** (the confounder explained part of the raw
association). This is the interpretable statement, it is symmetric for protective and risky effects,
and it degrades correctly through the null: crude OR 0.90 → adjusted 1.20 gives
|log 1.20| − |log 0.90| = +0.077 → "strengthened", which is right.

The *magnitude* fed to `findInterval` is then still the ratio or the difference, folded by the
existing per-side machinery; only the direction rule changes. This needs to be stated in the legend
("darker = adjustment strengthened / attenuated the effect"), not left implicit.

Counter-argument to weigh (§13, Q4): raw up/down is simpler to explain and matches what the two
printed numbers look like. Toward/away-from-null is what the reader actually wants to know.

### 7.4 Hard-wired sites a new measure does NOT inherit

Re-verified today; each needs a conscious decision:

- `fmt_color_plan:2815` — the SD standardisation is `measure == "diff"` only. Fine (this measure
  standardises differently or not at all).
- `:2880-2886` — the `guaranteed_effect` diff↔ratio bound rescale is `diff`/`ratio` only. Irrelevant
  in phase 1; phase 2 must not fall into it (the ratio-flood bug the skill warns about).
- `:2999` — the p-value-row warning colour is `measure == "diff"` only. Fine.
- `get_reference():4066` branches on `get_color(x) %in% c("OR","or")` for the reference-row anchor
  mask. A reg column's reference row is already handled by `in_refrow`; **verify** the anchor mask
  still bolds the reference row when the measure is `adjustment`.
- `fmt_stars_applicable:1583` excludes `contrib`. Stars belong to the *model estimate*, which is what
  the cell prints, so they must keep working here — i.e. do **not** add `adjustment` to that exclusion.
  Worth a fixture.

---

## 8. Reference resolution — every mode

The maintainer's requirement is "observed effects should be the reference columns, with the right one
per model". With Option A the reference is resolved **once, in `reg_build`, at the moment the crude
block is attached** — no matching by name, no heuristics, because the loop already holds both objects.

| mode                                       | crude block                                                                        | resolution                                                                                                                                                  |
|--------------------------------------------|------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| single model, single dependent             | `emp_by_fit[[1]]`                                                                  | the block's **effect** column (second element of `two(base, effect)`)                                                                                       |
| several dependents                         | one block per fit, inserted before that fit's first model column (`fit_first_idx`) | fit `i`'s model columns take `emp_by_fit[[i]]`'s effect column — the loop index already carries it                                                          |
| model comparison, `predictors = list(...)` | one block (`n_dep == 1`), placed first, unsuffixed                                 | **every** model column takes the same crude effect column — exactly the requested behaviour, and it makes the models' attenuation paths directly comparable |
| `split_var` (stacked)                      | built inside each recursive `reg_build` call on that group's subset                | automatic — each group's model column sees its own group's crude effect                                                                                     |
| `split_var` + `spread_models = TRUE`       | same, then `tab_spread` widens                                                     | automatic — the value was stored **before** the pivot, and `tab_spread` preserves fmt fields                                                                |
| `effect = "ame"` / `"ame_ratio"`           | `Obs_diff` / `Obs_RR`                                                              | the shape table already names them                                                                                                                          |
| `exponentiate = FALSE`                     | `Obs_log(OR)` / `Obs_log(RR)` / `Obs_log(IRR)`                                     | same, log scale on both sides                                                                                                                               |
| `multinomial` / `ordinal`                  | none                                                                               | degrade, §5.4                                                                                                                                               |

Two details to get right at implementation:

- **The reference row.** The crude columns' `in_refrow` excludes `Constant`; the model columns' does
  not (`reg_column:1062` vs `reg_empirical_columns:1256`). On the reference level the gap is
  `1 / 1` or `0 − 0` → neutral, and `gate_row = "refrow"` uncolours it anyway. But `Constant` has a
  model estimate and no crude twin → `obs = NA` → uncoloured. Correct by construction; assert it.
- **`multiplier`.** `tab_reg(multiplier = k)` raises a model OR to the power k. The crude column is
  **not** multiplied (verify at implementation). If so, the stored `obs` must be raised to the same
  power, or the gap is a pure artefact of the scaling argument.

---

## 9. `split_var` — the group-vs-group contrast

This is the maintainer's second question, and it is a **different and much easier** problem.

### 9.1 The groups are independent, so the cheap test is the right test

Measured, two disjoint simulated groups of 1500 with genuinely different effects
(log OR 0.69 vs 1.25), 500 bootstrap replicates:

```
bootstrap correlation between the two groups' estimates = +0.041     (theory: 0)
SE(difference): analytic sqrt(SE_A² + SE_B²) = 0.1717
                bootstrap                    = 0.1696     ratio 1.012
interaction z = +2.95, p = 0.00319
LRT for the interaction in a pooled model      p = 0.00322
```

The analytic SE is correct to 1.2 % and the resulting p matches the likelihood-ratio interaction test
to three digits. This is the standard "difference between two estimates" test (Altman & Bland 2003),
and unlike §4 it needs **nothing that is not already stored**: both groups' `ci_inf`/`ci_sup` are in
the table, and an SE is recoverable from a Wald interval.

Note the same run also shows the overlap trap in the *other* direction: the two CIs **did not**
overlap and the test **was** significant here, but the criteria disagree in general — non-overlap is
sufficient, not necessary. Overlap must not be taught as "no difference" for groups either.

### 9.2 Shape

The natural form is a **second value on the same measure axis**, not a second mechanism: with
`spread_models = TRUE` the first group is the reference column and each later group's cells score
their gap from it — which is structurally identical to §8's "the crude column is the reference". So:

- **phase 1** could reuse the very same `obs` field, filled with *the first group's estimate* instead
  of the crude one, under a distinct measure name (`"between_groups"` / `"interaction"`); or
- keep it out of z5 entirely and note that `color = "adjustment"` already works **within** each group
  (each group's model vs each group's own crude effect), which is itself a useful side-by-side
  reading: "adjustment matters more for women than for men".

The second is the honest scope for z5. The group contrast is a clean, independently valuable feature
whose statistics are settled (§9.1) and whose only real work is deciding the reference-group rule and
the break scale. **Recommend deferring it to its own phase**, with §9.1 recorded so the work is not
re-derived.

### 9.3 The stronger version, if it is ever wanted

A per-predictor **interaction row** in the GOF footer (one Wald test per predictor for
`predictor × split_var`) is the textbook answer to "do the effects differ between groups", is a single
extra fit, and is aggregated rather than per-cell (so no multiplicity inflation). It complements the
per-row colour rather than competing with it.

---

## 10. `predictors = list(...)` — model comparison

Mechanically this is free: §8 shows every model column already resolves to the same crude column, so
the attenuation path across `m1 → m2 → m3` is directly readable as a colour gradient across the row.
Measured on `gss_simple` (`married ~ race`, adding `rincome` then `relig`):

|                | m1    | m2     | m3      |
|----------------|-------|--------|---------|
| `raceBlack` OR | 0.411 | 0.432  | 0.393   |
| change vs m1   | —     | +5.2 % | −4.3 %  |
| `raceOther` OR | 0.898 | 0.955  | 1.009   |
| change vs m1   | —     | +6.4 % | +12.4 % |

Statistically this inherits §3's caveat **and adds one**: nested logit coefficients are not directly
comparable across models even in the absence of confounding, because each model has its own implicit
residual-variance scaling (Mood 2010). That is the *same* non-collapsibility phenomenon seen from the
sociological side, and it is what KHB (2012) exists to separate.

So the recommendation for the comparison case is the same as for the OR case, only louder:

- ship it (it costs nothing extra);
- document that a change between nested logit models mixes confounding with rescaling;
- point users who need the decomposition at **khb**;
- and note in the vignette that `effect = "ame"` or `family = "poisson"` make the comparison across
  models clean, because marginal collapsible effects rescale identically.

An optional refinement worth considering later: let the reference column be **the first model** rather
than the crude column when `empirical = FALSE` in comparison mode. It is the same code path with a
different index, and "what did adding this block of covariates do" is a common question.

---

## 11. Additional ways to make the comparison readable

Ordered by value-per-line-of-code. All of these ride machinery that already exists.

1. **The `{obs}` display token** (needs Option A). `display = "{or} ({obs})"` prints
   `2.31 (obs 2.05)` in one cell. The composite `{}` grammar, the padding, the Excel numFmt and the
   export parity all already work; `resid` added exactly one such token in z4. **Highest value.**
2. **The HTML tooltip.** `crude 2.05 [1.83; 2.30] → adjusted 2.31 [2.01; 2.66]; ×1.13 (+13 %)`.
   `tab_kable_print_tooltip` already composes per-cell tooltips and the reg path already appends
   `reg_empirical_tips`. Near-free, and it is where the CIs belong (they are too wide for a cell).
3. **A footer sentence.** "Adjustment moved 4 of 11 effects by more than 10 %." One line, uses the
   first break as its threshold so it can never disagree with the colours, and it goes through
   `tab_footer_streams` like every other footer token.
4. **Legend wording that names the direction.** Non-negotiable if §7.3 is adopted: the reader must be
   told which pole is "attenuated". One `legend_resolve_spec` fact.
5. **`or_plot()` gaining a crude overlay.** The forest plot already reads fmt fields with no refit; a
   second, lighter point-and-whisker per row for the crude estimate is the classic
   crude-vs-adjusted figure. Larger job, clearly separable, `tab_reg_plots.R` is already the home.
6. **Sorting by attenuation** — `arrange()` on the field already works once Option A lands; worth one
   vignette line, no code.

Deliberately **not** recommended: a materialised `Δ` column (three numbers per effect is more table
than anyone reads — the whole point of a colour measure is to avoid it), and any automatic
"confounder detected" flag (§1.1).

---

## 12. Implementation sketch, cost and risk

**Phase 1 — the measure (one session, byte-identical to today unless opted into).**

1. `/vctrs-field` pass for the 20th field `obs` (Option A). Conscious regen of the 36 structural
   goldens + the fmt-contract snapshot, with the usual proving script.
2. `reg_build`: at the point where `emp_by_fit[[i]]` is attached, write the crude effect vector into
   the fit's model columns' `obs` field. The row alignment is `reg_skel_match`'s existing index.
   ~15 lines, in one place.
3. One `MEASURES` row + one `std_when` arm + two scale keys + the five allow-list entries.
4. `tab_reg()`: `color = "adjustment"` forces `empirical = TRUE`; degrade for multinomial/ordinal.
5. Legend: word, ref phrase ("vs the observed effect"), direction sentence, `legend_method_name`
   returning `NA` (no CI in phase 1), the FR msgid + potools anchor.
6. The `{obs}` display token and the tooltip (§11.1-2) — small, and they are most of the perceived
   value.
7. Docs: `?tab_reg` (`color`), `?tab` (`color`), `set_color_breaks`, `?tabxplor-options` if a default
   moves, the regression vignette (EN + FR) with the collapsibility ladder of §3.1 and §3.3, NEWS.
8. Fixtures: one per family with a hand-computed gap; the reference row uncoloured; `Constant`
   uncoloured; multinomial degrades with a message; `multiplier` consistency; stars still work.

**Cost drivers:** the field pass and the golden regen (well understood, done twice recently), and the
`multiplier` / `in_refrow` details of §8. The colour arithmetic itself is trivial and the crude
numbers are already computed — **the runtime cost of the whole feature is one `match()` and a vector
divide.**

**Phase 2 — the gap test (separate, only if wanted).** The influence-function SE (§4.2), a second
stored number, the `guaranteed_effect` absolute-z reading through the `guar` field, and a decision
about the jamovi digest path (§4.4).

**Risks, ranked.**

- *Users read the OR gap as confounding.* Mitigated by legend + vignette, not by code. The single
  biggest risk, and the reason §3 leads this report.
- *Direction convention surprises.* §7.3; needs a maintainer ruling before implementation.
- *Additive break scale.* §7.2; needs a ruling.
- *Field overload if Option B is chosen under time pressure.* Recorded so the trade is conscious.

---

## 13. Open questions for the maintainer

- **Q1 — name.** `"adjustment"` (recommended) or something else? It is public API, and it is the word
  the vignette will teach.
  **Maintainer’s decision : `"adjustment"`**
- **Q2 — storage.** Option A (20th field `obs`, unlocks the token + tooltip + phase 2) or Option B
  (reuse the free `ratio` field, cheaper today, forecloses those)?
  **Maintainer’s decision : go for `obs` field.**
- **Q3 — the additive break scale.** Absolute in the effect's own units (recommended), SD-standardised,
  or relative-with-a-floor (not recommended)? §7.2.
  **Maintainer’s decision : absolute in the effect's own units**
- **Q4 — direction.** Toward/away from the null (recommended) or raw up/down? §7.3.
  **Maintainer’s decision : Toward/away from the null**
- **Q5 — background channel.** Allow `color = c("OR", "adjustment")` (recommended — it is the headline
  reading) or keep whole-cell measures text-only as `contrib`/`or` are?
  **Maintainer’s decision : Allow `color = c("OR", "adjustm**ent")`****
- **Q6 — the OR path.** Ship it with a caveat (recommended), or restrict the measure to collapsible
  families and tell OR users to switch to `effect = "ame"` / `family = "poisson"`?
  **Maintainer’s decision : Ship it with a caveat**
- **Q7 — scope.** Is the `split_var` group-vs-group contrast (§9) in z5, or its own phase? The
  statistics are settled either way; the work is a reference-group rule plus a break scale.
  **Maintainer’s decision : we do thWe `split_var` group-vs-group contrast (§9) now.**
- **Q8 — phase 2.** Is a significance test for the gap wanted at all, given §4.4's jamovi-cache
  consequence, or is the descriptive change-in-estimate the honest scope?
  **Maintainer’s decision : yes, it sounds useful but need more research, please add it as the next phase in @CLAUDE.md** (with reference to `dev/model_vs_observed_effect_colour.md`), and frame the instructions like the current thread with 1) study, statitical soundness and architecture questions ; 2) plan for implementation and implement.

---

## 14. References

### Collapsibility and confounding (§3)

- Greenland S., Robins J.M., Pearl J. (1999) "Confounding and collapsibility in causal inference",
  *Statistical Science* 14(1), 29-46. — **the canonical modern statement**: collapsibility is a
  property of the measure, confounding a property of the data.
- Bishop Y.M.M. (1971) "Effects of collapsing multidimensional contingency tables", *Biometrics*
  27(3), 545-562; Bishop Y.M.M., Fienberg S.E., Holland P.W. (1975) *Discrete Multivariate Analysis*;
  Whittemore A.S. (1978) "Collapsibility of multidimensional contingency tables", *JRSS-B* 40(3),
  328-340. — where the word comes from.
- Miettinen O.S., Cook E.F. (1981) "Confounding: essence and detection", *American Journal of
  Epidemiology* 114(4), 593-603. — the separation, for epidemiology.
- Zeger S.L., Liang K.-Y., Albert P.S. (1988) "Models for longitudinal data: a generalized estimating
  equation approach", *Biometrics* 44(4), 1049-1060. — the same arithmetic as population-averaged vs
  subject-specific.
- Maldonado G., Greenland S. (1993) "Simulation study of confounder-selection strategies",
  *American Journal of Epidemiology* 138(11), 923-936. — the 10 % change-in-estimate rule; §7.2.

### Comparing two estimates (§4.1, §9.1)

- Schenker N., Gentleman J.F. (2001) "On judging the significance of differences by examining the
  overlap between confidence intervals", *The American Statistician* 55(3), 182-186.
- Altman D.G., Bland J.M. (2003) "Interaction revisited: the difference between two estimates",
  *BMJ* 326, 219. — the independent two-estimate test.

### M-estimation, influence functions, sandwich variances (§4.2)

- Huber P.J. (1964) "Robust estimation of a location parameter", *Annals of Mathematical Statistics*
  35(1), 73-101. — M-estimators.
- Huber P.J. (1967) "The behavior of maximum likelihood estimates under nonstandard conditions",
  *Proc. Fifth Berkeley Symposium* 1, 221-233; White H. (1980) "A heteroskedasticity-consistent
  covariance matrix estimator…", *Econometrica* 48(4), 817-838. — the sandwich.
- Hampel F.R. (1974) "The influence curve and its role in robust estimation", *JASA* 69(346), 383-393.
  — the influence function.
- Jaeckel L.A. (1972) "The infinitesimal jackknife", Bell Labs memorandum; Efron B. (1982) *The
  Jackknife, the Bootstrap and Other Resampling Plans*. — the same object, resampling side.
- Binder D.A. (1983) "On the variances of asymptotically normal estimators from complex surveys",
  *International Statistical Review* 51(3), 279-292. — the same object, survey side ("linearization");
  this is what `survey::svyrecvar` implements.
- Stefanski L.A., Boos D.D. (2002) "The calculus of M-estimation", *The American Statistician* 56(1),
  29-38 (and Boos & Stefanski 2013, *Essential Statistical Inference*, ch. 7). — **the stacking
  construction used in §4.2.**
- Saul B.C., Hudgens M.G. (2020) "The calculus of M-estimation in R with geex", *Journal of
  Statistical Software* 92(2). — a generic implementation.

### Comparing coefficients across nested models (§10)

- Mood C. (2010) "Logistic regression: why we cannot do what we think we can do, and what we can do
  about it", *European Sociological Review* 26(1), 67-82. — cross-model comparability; §10.
- Karlson K.B., Holm A., Breen R. (2012) "Comparing regression coefficients between same-sample nested
  models using logit and probit", *Sociological Methodology* 42, 286-313. — the KHB decomposition;
  §4.3, §10. R package **khb**.
- Clogg C.C., Petkova E., Haritou A. (1995), *American Journal of Sociology* 100(5), 1261-1293, and
  Allison P.D. (1995) comment, ibid. 1294-1305. — the classic nested-coefficient test; §4.3.

### Already implemented in tabxplor

- Zou G. (2004) "A modified Poisson regression approach to prospective studies with binary data",
  *American Journal of Epidemiology* 159(7), 702-706. — Last Phase z3, `family = "poisson"` on a
  binary outcome; §3.3 is why it matters here.

**In-repo companions:** `dev/new_colors_UI.md` (the colour framework brief),
`dev/chi2_cell_residuals_and_contributions.md` (the z4 one-measure-three-readings precedent this
design follows), `dev/poisson_vs_logistic_binary_outcome.md` (the collapsibility argument that made
`family = "poisson"` on a binary outcome worth adding, and which §3 completes).
