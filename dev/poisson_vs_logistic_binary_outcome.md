# Risk ratios for a **binary** outcome — Poisson, marginal standardization, and the Goodman question

**Status:** research report / design study. No code changed.
**Date:** 2026-08-05.
**Questions asked:**

1. When the dependent variable is a binary factor, are there conditions where a Poisson regression is
   preferable to a logistic one? Would it be easy to implement in tabxplor's architecture? (**Parts 1–2**)
2. Is fitting a logistic model and computing a marginal risk ratio common practice, and is it robust?
   (**§1.5b, §2.4b**)
3. Is "modified Poisson" the same thing as a "log-linear model"? (**§1.6** — no, and it matters)
4. Should tabxplor add the *real* Goodman log-linear models for contingency-table counts? (**Part 3**)

**Short answers.**

1. **Yes, and the conditions are well defined and common in tabxplor's target audience.** The method is
   called **modified Poisson regression** (Zou 2004): a log-link Poisson fit on a 0/1 outcome with a
   **robust sandwich variance**. It estimates a **risk / prevalence ratio (RR/PR)** directly, instead of
   an odds ratio. It is the standard recommendation whenever the outcome is **common** (roughly >10 %)
   and the OR would be read as if it were a risk ratio — which, in social-science practice, is most of
   the time. It is *not* a better logistic regression; it answers a **different question**.
2. **Implementation is unusually cheap here — the feature is already ~70 % built, by accident.**
   `family = "poisson"` already exists end-to-end in `tab_reg()` (fit, CI, stars, colour, empirical
   companion, footer, jamovi UI). Measured below: a **weighted** `tab_reg(..., family = "poisson", wt =)`
   on a 0/1 outcome **already produces an exactly correct modified-Poisson table today** — it goes
   through `svyglm(quasipoisson)`, whose design-based variance *is* the sandwich. The real gaps are
   (a) a binary **factor** outcome is rejected, (b) the **unweighted** path uses a φ-scaling that is a
   *coincidental approximation* of the sandwich and is measurably wrong (**up to 18.5 % off** on real
   GSS data), and (c) everything is **labelled "IRR" / "incidence-rate ratio"**, which is wrong wording
   for a risk ratio. And critically: `survey` is already an **Import**, so the correct sandwich needs
   **no new dependency**.

3. **There is a second, equally good route that is even cheaper here: a ratio marginal effect**
   (marginal standardization / g-computation on the ordinary logistic fit). `reg_marginal()` **already
   has a `comparison =` parameter and already implements a multiplicative branch** (`"lnor"`), so adding
   the risk-ratio contrast is close to a copy. Measured: nominal 94.8 % coverage, unbiased, works on
   weighted `svyglm`, and it satisfies an exact coherence identity with the adjusted-% cell that the
   existing display grammar is built on. See §1.5b and §2.4b.
4. **"Modified Poisson" is *not* a "log-linear model"** in the sense a sociologist means — that phrase
   names Goodman's contingency-table models. See §1.6; this is a documentation-safety issue, not a
   technical one.

Everything numeric below was measured on this machine; the scripts are in Appendix B and are reproducible.

---

## Part 1 — The statistical question

### 1.1 The real issue is the estimand, not the fit

Logistic regression is not "biased" for binary outcomes — it is the maximum-likelihood fit of the correct
distribution, and it is excellent at what it does. The problem is what its coefficient *means*.

| Model                 | Link  | Coefficient exponentiates to        | Estimand                 |
|-----------------------|-------|-------------------------------------|--------------------------|
| Logistic (`binomial`) | logit | **Odds ratio** (OR)                 | ratio of *odds*          |
| Modified Poisson      | log   | **Risk / prevalence ratio** (RR/PR) | ratio of *probabilities* |
| Log-binomial          | log   | RR/PR                               | ratio of *probabilities* |

The OR ≈ RR approximation holds only when the outcome is **rare**. As prevalence rises, the OR moves away
from the RR, always **away from 1** — it exaggerates. Measured (Appendix B, probe 1), on a simulated
outcome with 43 % prevalence:

```
                   OR        RR
x1          2.5331650 1.6447072      <- same data, same predictor
x2b         1.4811688 1.2188571
x2c         0.6681698 0.7858936
```

An OR of **2.53** against an RR of **1.64**. A reader who narrates "respondents with x1 are 2.5 times more
likely" is wrong by a factor of ~1.5. In the 500-replication simulation (probe 4), with a true RR of 1.50,
the mean OR was **1.94** and the mean modified-Poisson RR was **1.51**.

This is the single most consequential point of the whole report, and it is a *communication* failure, not
a statistical one: the OR is correct, but almost nobody reads it correctly. This is exactly the audience
tabxplor targets — the package's own vignettes describe teaching "literary students", and its colour
grammar is built to make deviations legible to non-specialists. `Model_OR = 2.53` in a coloured table
*invites* the misreading.

### 1.2 Non-collapsibility — the second, subtler reason

The OR is **non-collapsible**: the marginal OR is not a weighted average of stratum-specific ORs, even
with no confounding at all. Practical consequences:

- Adding a covariate changes the OR **even when that covariate is not a confounder**. The estimand itself
  depends on the adjustment set.
- Therefore **comparing OR coefficients across nested models is not valid** — a staple of sociological
  practice ("model 1, model 2, model 3, see how the coefficient moves"). tabxplor's own `predictors =
  list(model1=, model2=, model3=)` comparison feature *encourages* precisely this reading.
- RR and risk difference **are** collapsible, so a Poisson/RR table supports the nested-model narrative
  that a logistic table does not.

This is an argument that applies **regardless of prevalence**, and is arguably a stronger reason for
tabxplor's use case than the rare-disease issue.

### 1.3 Why plain Poisson is wrong and the sandwich is mandatory

Fitting Poisson to a 0/1 outcome deliberately misspecifies the variance: Poisson assumes Var = μ, but the
truth is Bernoulli, Var = μ(1−μ). The **point estimates stay consistent** (the log-link score equation is
unbiased), but the **naive standard errors are too large** by roughly √(1−μ). Measured (probe 4), on GSS:

```
Pearson dispersion: 0.4868   mean(1-mu): 0.4867     <- exactly as theory predicts
```

Naive Poisson CIs are therefore **conservative** — coverage 98.4 % and 99.8 % instead of 95 % in the two
simulations. Zou's fix is to keep the Poisson estimating equation and replace the variance with the
**Huber–White sandwich**, which is valid under the misspecification.

**Talbot et al. (2023, *Epidemiology*)** give the cleanest justification and remove the usual objection
("why a count distribution for a binary outcome?"): the robust Poisson method **does not assume a Poisson
distribution at all**. It is a semiparametric estimator whose only real assumption is that
log(risk) is linear in the covariates. The Poisson likelihood is just a convenient vehicle for the score
equation. That reframing is worth putting in the tabxplor documentation verbatim, because the "Poisson for
a yes/no variable?" objection is the first thing a user will raise.

### 1.4 The honest limitations

A balanced report must state these; they shape the recommendation.

- **Fitted values can exceed 1.** The log link is unbounded above, so the model can predict risks > 100 %.
  This is the price of avoiding log-binomial's convergence failures. It rarely disturbs the coefficients
  but it does mean *predicted probabilities are not trustworthy* — the model is for **effect estimation,
  not prediction**. (Hagiwara & Matsuyama 2024 develop goodness-of-fit tests specifically for this case.)
- **Efficiency loss.** Robust Poisson is less efficient than a correctly-specified log-binomial; CIs are
  somewhat wider. Usually a modest price.
- **Small samples.** The sandwich is consistent but biased downward in small samples, which **inflates
  Type I error**. Zou's own simulations support n ≥ 100. Below that, HC3 or a Firth-type penalised variant
  (Uno et al. 2024) is preferable. For tabxplor's survey-scale data (n in the thousands) this is a
  non-issue, but a small-n caveat belongs in the docs.
- **The log-linearity assumption is stronger than logit-linearity** near the boundary, because the log
  link has to respect a ceiling it cannot represent.
- **It does not fix the analysis.** If the OR is genuinely the estimand of interest (case-control designs,
  where the RR is not even identified), logistic remains correct and Poisson is wrong.

### 1.5 The competing approaches

| Approach                                                       | Gives                                                     | Verdict for tabxplor                                                                                                                                                           |
|----------------------------------------------------------------|-----------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Log-binomial** (`glm(family=binomial(link="log"))`)          | RR, fully parametric, efficient, respects the [0,1] bound | **Rejected.** Notorious convergence failures — the optimum sits on the boundary of the constrained parameter space. Unacceptable in a live jamovi UI where every click refits. |
| **Modified Poisson** (Zou)                                     | RR + robust SE                                            | **Recommended.** Always converges, one line from the existing code, robust to outliers (Chen et al. 2014 find it *more* outlier-robust than log-binomial).                     |
| **Marginal standardization / g-computation** on a logistic fit | marginal RR (and RD)                                      | **Strong alternative, already ~90 % wired.** See §1.5b — this is the other serious candidate, and in tabxplor it is *cheaper* than modified Poisson.                           |
| **Cox / robust Poisson variants**                              | RR                                                        | Equivalent in practice; no reason to add.                                                                                                                                      |

### 1.5b Marginal standardization — is it common practice, and is it robust?

**Is it common practice?** **Yes — and in causal-inference-minded epidemiology it is now arguably the
*preferred* answer**, more modern than robust Poisson. The recipe is: fit an ordinary logistic model, use
it to predict everyone's risk twice (once as if all exposed, once as if all unexposed), average each, and
take the ratio. The names in the literature are *marginal standardization*, *regression standardization*,
*model-based standardization*, or *g-computation* (the parametric g-formula) — all the same procedure.
Evidence that it is established practice rather than a niche idea:

- Stata ships it as first-class (`margins`), which is why it is routine in health economics and epi.
- The R package [`risks`](https://stopsack.github.io/risks/articles/margstd.html) offers it as a headline
  method precisely as the fallback when log-binomial fails to converge.
- Recent simulation work finds "modified Poisson regression **and** regression standardization yield
  unbiased risk-ratio estimates with appropriate confidence intervals irrespective of the number of
  confounders" — i.e. the two are co-recommended, not ranked.
- It is used in registered trial analysis plans to report RRs and RDs from a logistic model.

Where it is *not* common is **sociology**, where neither approach is standard practice and the OR is still
reported (and mis-narrated) by default. So tabxplor would be importing a good habit from a neighbouring
field either way.

**Is it robust?** Measured here — yes, on every axis tested (probes 7–8):

| Check                            | Result                                                                                       |
|----------------------------------|----------------------------------------------------------------------------------------------|
| Bias                             | true marginal RR 1.3999, mean estimate over 500 reps **1.3981**                              |
| CI coverage (delta method, 95 %) | **0.948** — nominal                                                                          |
| Weighted `svyglm` fits           | **works**, returns design-based CIs (RR 1.3842 [1.313, 1.459])                               |
| Rare outcome (0.7 % prevalence)  | **no blow-up**: RR 3.21 [1.18, 8.72] vs OR 3.24 — converges to the OR exactly as theory says |
| Numeric predictors               | works (contrast label `mean(+1)`)                                                            |
| Coherence with the adjusted %    | **exact**: adjusted%(ref) × RR = adjusted%(level), to all printed digits                     |

That last row matters for tabxplor specifically. The existing `effect = "ame"` display grammar composes
`"{diff} ({pct})"` — the AME with the adjusted predicted probability in parentheses — and it holds because
adjusted%(ref) **+** AME = adjusted%(level). The ratio version has the exact multiplicative twin:
adjusted%(ref) **×** RR = adjusted%(level). So a `"{or} ({pct})"` cell would be *internally coherent by
construction*, the same way the current one is. That is a real design argument, not a coincidence.

**The caveats, honestly:**

1. **It is a different estimand — marginal, not conditional.** Modified Poisson answers "two otherwise
   identical individuals differing in X"; standardization answers "the whole population under X vs under
   the reference". Measured under strong baseline-risk heterogeneity (risk 0.10→0.89), marginal RR
   0.5143/0.8109 vs conditional RR 0.5110/0.8045 — **close but not equal** (0.6–0.8 % apart). Neither is
   wrong; for a *descriptive* sociology table the marginal one is arguably what the user actually means.
2. **The estimate is standardized to the sample's covariate distribution**, so it is not transportable and
   it *changes when you subset*. This has a concrete tabxplor consequence: under **`split_var`**, each
   split standardizes to its own subpopulation, so the columns are no longer standardized to a common
   population and are not strictly comparable. Modified Poisson does not have this problem. This needs a
   documented caveat, and it is the strongest argument *against* making standardization the default.
3. **It still leaves the model logistic** — so no fitted values above 1 (an advantage over Poisson), but
   also: the *underlying coefficients* remain non-collapsible ORs. The good news is that the **standardized
   RR output is comparable across nested models** (adding a covariate moves it only through confounding
   adjustment), so §1.2's nested-model argument is satisfied by this route too.
4. **`marginaleffects` is a Suggests.** Putting a core estimand behind an optional dependency is
   architecturally uncomfortable — a user without it gets no RR at all, whereas the Poisson route needs
   nothing new (`survey` is an Import).
5. **Cost.** It predicts n × levels times per predictor rather than reading off a coefficient. Irrelevant
   in a script; potentially relevant in the live jamovi UI, which refits on every click.
6. **No p-value duality subtlety**: the delta-method CI on the log scale exponentiates to an asymmetric
   positive interval, which fits tabxplor's existing `ci_type = "or"` shape without modification.

### 1.6 Terminology — "modified Poisson" is **not** a "log-linear model" (a trap for a sociology audience)

Asked directly: **no, they are not the same thing**, and the confusion is especially dangerous for
tabxplor's readership. Three distinct things share the log link and must be kept apart:

| Term                                                    | Outcome modelled                       | What it estimates                                                                    | Field                                    |
|---------------------------------------------------------|----------------------------------------|--------------------------------------------------------------------------------------|------------------------------------------|
| **Modified / robust Poisson** (Zou)                     | an **individual's** binary 0/1         | RR/PR of the *individual* outcome                                                    | epidemiology, public health              |
| **Log-linear model** (Goodman, Bishop–Fienberg–Holland) | **cell counts of a contingency table** | the *association structure* among categorical variables                              | **sociology**, categorical data analysis |
| **Log-binomial**                                        | an individual's binary 0/1             | RR/PR — same estimand as modified Poisson, different estimator (binomial likelihood) | epidemiology                             |

The trap: in the **generic GLM sense** modified Poisson *is* "log-linear" — it fits log(risk) = Xβ, linear
on the log scale. So the phrase is not *wrong*, merely ambiguous. But in **sociology "log-linear model" is
a term of art** with a specific referent: Goodman's models for contingency-table cell counts (mobility
tables, quasi-independence, RC association models, the whole Goodman/Hauser/Erikson–Goldthorpe lineage).
Those model **counts of cells**, not the probability of an individual outcome. That they *also* use a
Poisson likelihood with a log link is precisely why the confusion is so easy to fall into — the machinery
is identical, the question asked is completely different.

**Practical rule for tabxplor's documentation and French translation:** never write "log-linear" for this
feature. Use **"modified Poisson regression"** / **"robust Poisson"** (FR: *régression de Poisson
modifiée*), and if the estimand needs naming, say **"risk ratio"** / *rapport de risques* or
**"prevalence ratio"** / *rapport de prévalence*. A French sociologist reading "modèle log-linéaire" in a
tabxplor footer will think of Goodman and be actively misled. This is worth a `# WARNING:` tag next to
whichever wording function ends up carrying it.

(Note also: "log-binomial" ≠ "modified Poisson". Same estimand, different estimator — see §1.5.)

### 1.7 Decision table — when to use which

| Situation                                                                       | Use                                                                             |
|---------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
| Outcome **rare** (<10 %)                                                        | Logistic. OR ≈ RR anyway, and it is more efficient.                             |
| Outcome **common** (>10 %) and you will *speak* of "more likely" / "times more" | **Modified Poisson (RR)** *or* **ratio AME**                                    |
| **Cross-sectional** survey, prevalence ratio wanted                             | **Modified Poisson (PR)** — the canonical use case                              |
| Comparing coefficients **across nested models**                                 | Either RR route (both collapsible) — **not** the OR                             |
| You want a **population-level** ("if everyone were…") statement                 | **Ratio AME** (marginal standardization)                                        |
| You want an **individual-level** ("two similar people…") statement              | **Modified Poisson** (conditional RR)                                           |
| **Case-control** design                                                         | Logistic only — the RR is not identified                                        |
| You need **predicted probabilities**                                            | Logistic, or the ratio AME (Poisson can exceed 1)                               |
| Sub-population columns via **`split_var`**                                      | **Modified Poisson** (the ratio AME re-standardizes per split — §1.5b caveat 2) |
| n < 100                                                                         | Logistic, or modified Poisson with HC3 / Firth                                  |
| Reviewers/field expect ORs (clinical epi, much of quant. psych)                 | Logistic, possibly reporting both                                               |

For tabxplor's actual audience — French sociologists doing cross-sectional survey analysis on common
outcomes like "married", "voted", "has a diploma" — **rows 2, 3, 4 and 5 all apply simultaneously**. This
is a genuinely well-motivated feature, not a curiosity.

---

## Part 2 — Implementation in tabxplor

### 2.1 What already works today (measured, not inferred)

`family = "poisson"` is a first-class citizen of `tab_reg()`. Running it on a **numeric 0/1** outcome
today already produces a complete, coloured, exportable table (probe 3):

```
  var      levels               Obs_rate   Obs_IRR Model_IRR
1 Constant Reference population                    1/1.69***
2 race     White                 0.48         1         1
3 race     Black                 0.69***   1.44***   1.40***
4 rincome  4-$25000 or more      0.45***  1/1.41*** 1/1.38***
```

Everything downstream is in place: Wald CI ↔ p ↔ stars duality, `color = "OR"` on the ratio breaks, the
`Obs_` empirical companion, the model-fit footer, `split_var`, `spread_models`, the jamovi UI, all four
exporters.

**And the weighted path is already exactly right.** `reg_fit()` sends a weighted Poisson to
`survey::svyglm(family = quasipoisson)`, and svyglm's design-based linearization variance **is** the
sandwich. Measured (probe 5B) — `tab_reg(wt = "w")` against a hand-built `svyglm`:

```
tab_reg Model_IRR : 1.47***  1.08***
exp(coef(svyglm)) : 1.4677   1.0761      <- identical
```

So **`tab_reg(data, dependent = <0/1 numeric>, predictors = ..., family = "poisson", wt = "w")` is,
today, a correct modified Poisson regression.** It is simply mislabelled as an incidence-rate ratio.

### 2.2 The three real gaps

**Gap 1 — a binary factor outcome is rejected.** `reg_prep_binary()` (the 0/1 recode, `tab_reg.R:387`)
is called only inside the `"binomial"` arm of the `fam_obj` switch. A factor with `family = "poisson"`
hits the numeric guard:

```
ERROR: A "poisson" outcome must be numeric.  "married" is <factor>.
```

This is the only hard blocker, and it is a ~5-line fix.

**Gap 2 — the unweighted variance is an approximation, and a measurably wrong one.** `reg_fit()`
(~`tab_reg.R:918`) auto-scales unweighted Poisson SEs by √φ̂ (Pearson dispersion). On binary data
φ̂ ≈ mean(1−μ), so this *accidentally* corrects in the right direction and by roughly the right amount.
But φ̂ is a **single global average**, while the sandwich corrects **per coefficient**. Measured on real
GSS data (probe 4):

```
              naive   phi_scaled  sandwich_HC0   phi/sandwich
raceBlack   0.03146     0.02195       0.01852        1.185     <- 18.5% too wide
rincome.C   0.03234     0.02256       0.02065        1.092
raceOther   0.04223     0.02946       0.02927        1.006
```

The error ranges from 0.6 % to **18.5 %** across coefficients *of the same model* — it is not a constant
offset that could be calibrated away. In a coverage simulation with heterogeneous baseline risk (probe 5A,
600 reps), φ-scaling gave **97.7 %** coverage with SEs **11.3 % too wide**, against **96.3 %** for the
sandwich. φ-scaling is systematically conservative exactly when covariates matter — the realistic case.

φ-scaling is therefore fine as a *count-model* feature (what it was built for, decisions §48) but must
**not** be silently reused as the binary-outcome correction.

**Gap 3 — the wording is wrong throughout.** `reg_effect_word()` returns `"IRR"`, `reg_model_note()` says
"incidence-rate ratios", `reg_family_display_name()` says "Poisson regression", the `REG_EMPIRICAL$poisson`
companion is `Obs_rate` (a mean with ratio colour) + `Obs_IRR`. For a binary outcome every one of these
should read **RR / PR**, "risk ratio", "modified Poisson regression", and the companion should be
**`Obs_%` + `Obs_RR`**. Also, the Model-fit footer currently reports McFadden R², AIC and BIC computed
from a **Poisson likelihood on binary data**, which are not meaningful, plus a "Dispersion 0.49" line
that is just mean(1−μ) and will confuse everyone.

### 2.3 The decisive architectural break: no new dependency

The sandwich variance can be obtained **without adding `sandwich` to Suggests**, because `survey` is
already an **Import**. Verified exactly (probe 2) — `svyglm` on a constant-weight `ids = ~1` design
against a hand-computed HC0:

```
                   HC0        HC1     svyglm   ratio_svy_HC0
raceBlack   0.04589118 0.04592181 0.04589907       1.000172
```

The ratio is **√(n/(n−1)) = 1.000167** — svyglm returns HC0 scaled by n/(n−1), i.e. the Zou estimator to
four decimals. So routing an unweighted robust-Poisson fit through `svyglm` with a constant-weight design
**reuses the entire existing weighted code path** (`reg_make_design` / `reg_resolve_design` /
`reg_svyglm_env` / the `disp_known = FALSE` → t-reference branch / `reg_wald_finalize`). This is by far
the cheapest correct route, and it makes the weighted and unweighted paths *the same path*, which is
exactly the architectural direction CLAUDE.md mandates ("one resolver, one model").

### 2.4 Recommended design

**A new family value, not a flag on `poisson`.** Add `family = "rr"` (aliases `"risk_ratio"`,
`"prevalence_ratio"` / `"pr"`) rather than overloading `family = "poisson"`. Reasons:

- The estimand differs (RR vs IRR), so every label, legend, colour word and footer differs. A family
  value gives one dispatch key that `reg_effect_word` / `reg_model_note` / `REG_EMPIRICAL` /
  `reg_family_short` already switch on — no new plumbing, and it rides the Phase 15e **per-dependent
  family vector** for free (`family = c(married = "rr", income = "gaussian")` just works).
- It keeps `family = "poisson"` byte-identical for genuine counts — **zero golden churn**.
- It is self-documenting in the jamovi dropdown: *"risk ratio (modified Poisson)"* beside
  *"binomial (logistic)"*.

**Sketch of the changes** (line anchors from 2026-08-05, re-grep before editing):

| File / function                                                     | Change                                                                                                                                                                                                                                                                                                    |
|---------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `tab_reg.R` `valid_families` (~2701)                                | add `"rr"` (+ aliases normalised to `"rr"` at the boundary)                                                                                                                                                                                                                                               |
| `tab_reg.R` `reg_fit` `fam_obj` switch (~856)                       | `"rr"` arm: call `reg_prep_binary()` (reuse the binomial recode + `positive_level`), then `quasipoisson("log")`                                                                                                                                                                                           |
| `tab_reg.R` `reg_fit` fit dispatch (~886)                           | for `"rr"`, **always** go through `svyglm` — unweighted ⇒ a constant-weight design via the existing `reg_make_design`. One `if`, no new machinery                                                                                                                                                         |
| `tab_reg.R` `over_disp` (~918)                                      | exclude `"rr"` — the sandwich replaces φ-scaling (must **not** double-correct)                                                                                                                                                                                                                            |
| `tab_reg.R` `disp_known` (~951)                                     | `FALSE` for `"rr"` ⇒ t on `df.residual` (already the weighted behaviour; slightly conservative in small n, which is the desired direction)                                                                                                                                                                |
| `tab_reg.R` `reg_detect_family` (~155)                              | leave alone — auto-detect must stay `binomial` (back-compat). `"rr"` is opt-in                                                                                                                                                                                                                            |
| `tab_reg.R` `reg_effect_word` (~193)                                | `"rr"` → `"RR"` (or `"PR"`)                                                                                                                                                                                                                                                                               |
| `tab_reg.R` `reg_family_display_name` / `_short` / `reg_model_note` | "modified Poisson regression" / `"rr"` / "risk ratios (vs the reference category)" — all `gettext()`, so the French catalogue needs 3–4 new msgids                                                                                                                                                        |
| `tab_reg.R` `REG_EMPIRICAL` (~1146)                                 | new `rr` arm: `Obs_%` (`type="row"`, `display="pct"`, risk-diff colour) + `Obs_RR` — **the CI engine already exists**: `ci_katz_rr()` (`tab-agg.R:386`), the Katz log-RR interval, already used by `tab()` for ratio mode. Plus an `rr_log` twin for `exponentiate = FALSE`, mirroring `or_log`/`irr_log` |
| `tab_reg.R` `reg_empirical_columns` (~1166)                         | one `fam_key == "rr"` branch calling `ci_katz_rr` — structurally identical to the existing binomial branch                                                                                                                                                                                                |
| `tab_reg.R` `reg_glance` / `reg_gof_tibble` (~1588)                 | **suppress** McFadden/AIC/BIC/dispersion for `"rr"` (quasi-likelihood ⇒ no AIC; the Poisson-likelihood pseudo-R² on binary data is meaningless). Keep N and the Wald-vs-null                                                                                                                              |
| `tab_reg.R` roxygen                                                 | document the estimand, the >10 % rule, non-collapsibility, fitted-values->1 and the n≥100 caveat                                                                                                                                                                                                          |
| `jamovi/jmvtabreg.a.yaml` + `js/jmvtabreg.js`                       | one dropdown entry; `detectFamily` unchanged (binary still auto-selects binomial)                                                                                                                                                                                                                         |
| vignette `tabxplor-reg.Rmd`                                         | a short "Odds ratio or risk ratio?" section                                                                                                                                                                                                                                                               |

**Colour.** No new machinery: RR is multiplicative, so `color = "OR"` on the existing ratio breaks is
already correct, and `log_odds_scale()` (Phase 18g) already handles the non-exponentiated log-RR case.
One caveat: its `is_logcoef` gate is an **explicit family whitelist**
(`c("binomial", "poisson", "quasipoisson", "ordinal", "multinomial")`) and it is written **twice** —
`fmt_class.R:2752` (the colour plan) and `fmt_class.R:3654` (the legend mirror, which the comment at
`:3653` already flags as needing to track the first). `"rr"` must be added to **both**, or the
`exponentiate = FALSE` table will colour correctly but describe itself wrongly in the legend. This is a
sync-by-comment pair of exactly the kind Phase 17 rule 5 targets; folding it into one predicate while
adding `"rr"` would be the right move.

**Effort.** Roughly a **half-day to a day** for the R side including tests — genuinely small, because
every hard part (log-link fit, robust SE, CI↔p↔stars duality, ratio colour, Katz CI, exports, jamovi,
per-dependent family vectors) already exists. The jamovi side is one YAML entry plus the maintainer's
`prepare()` + rebuild.

### 2.4b The alternative route: a ratio AME (marginal standardization)

**The reading of the code in the question is correct, and the change is even smaller than stated.**
Confirming point by point:

- ✅ **`effect = "ame"` currently prints a percentage-point difference**, not a ratio. `reg_marginal()`
  calls `marginaleffects::avg_comparisons()` with no `comparison =` argument, so it returns the default
  additive contrast (a risk difference), which `reg_marginal_column(shape = "prob")` renders as the
  `"{diff} ({pct})"` cell.
- ✅ **The change is "add a ratio type of marginal effect + adapt the `empirical = TRUE` counterparts."**
  That is exactly the shape of the work.
- ➕ **But the ratio machinery is already there.** `reg_marginal()` **already takes a `comparison =`
  parameter**, and already implements a multiplicative branch for `comparison = "lnor"` (the Phase 12e-ii
  MNL "j vs rest" feature): it sets `do_exp <- identical(comparison, "lnor")`, exponentiates
  `est`/`lo`/`hi`, and parses the `"ln(odds(<Level>) / odds(<Ref>))"` contrast label. The risk-ratio
  contrast label is **structurally identical** — measured: `"ln(mean(low) / mean(high))"`, same
  `prefix + level + ") / …(<ref>))"` double-paren shape. And `reg_marginal_column()` **already has an
  `"or"` shape** for multiplicative marginal effects.

So the concrete R-side change is roughly:

| Site                           | Change                                                                                                                                               |
|--------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------|
| `reg_marginal()` `do_exp`      | `identical(comparison, "lnor")` → `comparison %in% c("lnor", "lnratioavg")`                                                                          |
| `reg_marginal()` label parsing | generalise the `pre`/`suf` pair: `"ln(odds("` / `"ln(mean("` and `") / odds("` / `") / mean("`. Two literals, one lookup                             |
| `tab_reg()` public arg         | `effect = "ame"` gains a ratio sibling — e.g. `effect = "rr"` (or an `ame_type =` modifier), threaded into `reg_marginal(comparison = "lnratioavg")` |
| `reg_marginal_column()`        | reuse the existing `shape = "or"`, or a `"prob_ratio"` shape composing `"{or} ({pct})"` — the coherence identity makes this well-defined             |
| `REG_EMPIRICAL$binomial`       | the `ame` row (`Obs_diff`, Wald risk-difference) gains an RR twin: `Obs_RR`, using **`ci_katz_rr()` which already exists** (`tab-agg.R:386`)         |
| colour                         | none — multiplicative ⇒ the existing `color = "OR"` ratio breaks                                                                                     |

**Numeric predictors need no work**: `reg_marginal()` already keys them on the variable name rather than
the contrast label (`level <- if (!is_fac) v`), so the differing `"mean(+1)"` label format is harmless.

**Estimated effort: smaller than the modified-Poisson route** — no new fit path, no variance question, no
family value, no jamovi family dropdown entry. Perhaps a half-day including tests.

**How the two routes compare for tabxplor:**

|                         | Modified Poisson (`family = "rr"`) | Ratio AME (marginal standardization)                     |
|-------------------------|------------------------------------|----------------------------------------------------------|
| Estimand                | conditional RR                     | marginal (standardized) RR                               |
| New dependency          | **none** (`survey` is an Import)   | `marginaleffects` (a **Suggests**)                       |
| Fitted values > 1       | possible                           | **impossible**                                           |
| Works under `split_var` | yes, comparably                    | yes, but each split standardizes to its own population ⚠ |
| Speed                   | coefficient read-off               | n × levels predictions per predictor                     |
| Nested-model comparison | valid                              | valid                                                    |
| Effort here             | ~1 day                             | ~half a day                                              |
| Fits the existing UI    | a new `family` value               | a new `effect` value                                     |

They are **not mutually exclusive, and ideally both ship** — they sit on orthogonal axes of `tab_reg()`
(`family =` vs `effect =`), which is exactly how the current architecture separates "what model" from
"what quantity". Offering both, with the docs explaining the conditional/marginal distinction in one
sentence, is the honest answer to the "everyone says *more likely*" problem.

If only one ships, the ratio AME is the **cheaper** and the **safer** (no >1 predictions, keeps the
familiar logistic fit); `family = "rr"` is the more **self-contained** (no optional dependency) and the
more **recognisable** to anyone who has met the epidemiological literature.

### 2.5 Risks and edge cases

- **Do not double-correct.** If `"rr"` reaches both `svyglm` *and* the φ-scaling branch, SEs are wrong
  twice over. The `over_disp` exclusion is the load-bearing line of the whole change.
- **`method = "profile"` must be blocked** for `"rr"` — a profile likelihood on a deliberately
  misspecified quasi-likelihood is meaningless. Mirror the existing weighted guard (which already emits
  "not defined for survey-weighted models; using Wald").
- **`effect = "ame"`** on an `"rr"` fit: `marginaleffects` will happily compute marginal effects on the
  response scale, but predictions may exceed 1. Either block it with a clear message or document it.
  Blocking is safer for a first version.
- **The `.fit_cache` byte-identity contract** (`reg_reref_fit_res`, jamovi Phase 15b) is locked by tests.
  The reref fast-path recomputes reference contrasts from a stored coef+vcov digest — that math is
  link-agnostic and should work unchanged for `"rr"`, but the digest path must be **verified, not
  assumed**, and the safe first move is to route `"rr"` down the raw-fit branch.
- **Existing `family = "poisson"` on a 0/1 numeric outcome** keeps working exactly as today. Consider one
  `cli_inform` nudging toward `family = "rr"`, but do **not** change its numbers — that would move goldens.
- **Fitted values > 1** should be detected and surfaced (a footer note or one-time message) rather than
  silently ignored; it is the method's one genuinely surprising behaviour.

### 2.6 Test plan

- **Parity:** `tab_reg(family = "rr")` coefficients + CIs identical to a hand-built
  `svyglm(y01 ~ ..., design = svydesign(ids=~1, weights=~1), family = quasipoisson)`, and — the honest
  external check — to `sandwich::vcovHC(type = "HC0")` up to n/(n−1) (test skipped if `sandwich` absent;
  it is not a dependency).
- **Weighted parity:** against `svyglm` on a real weighted design; plus the Kish `n_eff` interaction on
  the `Obs_` companion (Phase 18s).
- **Estimand:** on a common outcome, `RR < OR` when OR > 1 and `RR > OR` when OR < 1 (the "always away
  from 1" property) — a cheap, strong invariant.
- **Empirical companion:** `Obs_RR` equals a hand-computed crude risk ratio with a `ci_katz_rr` interval.
- **Non-regression:** `family = "poisson"` on genuine counts byte-identical (goldens must not move);
  `family = "binomial"` untouched.
- **Guards:** profile method blocked; factor outcome accepted and `positive_level` reported correctly;
  the `inverse_two_level_factors` level-flip honoured (it rides `reg_prep_binary()`, so reusing that
  function gets it for free — but it must be *asserted*, not assumed).

### 2.7 Recommendation

**Ship both routes, after 2.0.0 — and start with the ratio AME.**

Both are well-motivated for tabxplor's actual audience, both are cheap because the infrastructure exists,
and both directly serve the package's stated purpose of making effects legible to non-specialists: a risk
ratio is the number users already *think* they are reading off the OR column. They live on orthogonal
axes of `tab_reg()` (`family =` vs `effect =`), so neither blocks the other.

Suggested order:

1. **The ratio AME first** (§2.4b) — smaller, safer, no new fit path, no variance question, and it
   piggybacks on machinery (`comparison =`, the `"or"` shape, `ci_katz_rr`) that already exists and is
   already tested. It also cannot produce a risk above 100 %.
2. **`family = "rr"` second** (§2.4) — the recognisable epidemiological method, self-contained with no
   optional dependency, and immune to the `split_var` standardization-population caveat.

Both are **new public API surface**, and CLAUDE.md is explicit that the 2.0.0 release freezes these.
Adding either now would mean designing the estimand vocabulary, the French catalogue entries and the
jamovi dropdown under release pressure — and §1.6 shows the wording is exactly where the danger is. A
2.1.0 feature with room to get the terminology right is the better trade.

Two things worth doing **before** the release, because both are documentation rather than API:

- Add a sentence to `?tab_reg` noting that with a common outcome the OR is **not** a risk ratio and should
  not be narrated as one. Cheapest, highest-value item in this whole report.
- Make sure no existing or planned wording (R docs, vignettes, jamovi UI, the French catalogue) calls any
  of this a **"log-linear model"** — see §1.6. For a French sociology audience that phrase names Goodman's
  contingency-table models, not this.

---

## Part 3 — Should tabxplor add the *real* Goodman log-linear models?

Follow-up question, and a fair one given §1.6: if "log-linear model" means something specific and central
to sociology, and tabxplor's core object *is* the contingency table, should the package implement it?

**Verdict up front: no — with one narrow, cheap exception.** The specialist end is already excellently
served by a CRAN package written by a French sociologist for exactly this audience, and the descriptive
end is *already implemented in tabxplor under a different name*. What is genuinely missing is one small
thing: a test of whether an association is the **same across sub-tables**.

### 3.1 What the models actually are

A log-linear model treats the **cell counts** of an n-way contingency table as Poisson and models
log(expected count) as a sum of main effects and interactions. The interactions *are* the associations,
so the model family is a hierarchy you compare by deviance:

| Model (3-way, R × C × L) | Reads as                                                                             |
|--------------------------|--------------------------------------------------------------------------------------|
| `R + C + L`              | complete independence                                                                |
| `RC + L`                 | R and C associated, both independent of the layer                                    |
| `RL + CL`                | **conditional independence**: R ⫫ C given L                                          |
| `RC + RL + CL`           | **homogeneous association**: R–C association exists and is the *same* in every layer |
| `RCL` (saturated)        | the association differs by layer — fits perfectly, explains nothing                  |

The classic sociological refinements sit on top: **quasi-independence** (blank out the diagonal to model
mobility net of immobility), **uniform association** (one parameter for an ordinal × ordinal table),
**RC(M) association models** (Goodman 1979/1981 — estimated scores for row and column categories), and
**UNIDIFF / log-multiplicative layer effect** (Erikson–Goldthorpe, Xie — one parameter per layer scaling
a common association pattern; the workhorse of comparative mobility research).

### 3.2 The genuine use cases for this audience

These are real, and they are squarely sociological:

- **Intergenerational mobility tables** — origin class × destination class, the single biggest consumer of
  these models. Quasi-independence separates structural mobility (marginal change) from *fluidity*
  (association net of margins) — a distinction the raw percentages simply cannot make.
- **Assortative mating / homogamy** — spouse's education × education, same logic, diagonal-heavy tables.
- **Cross-national or cross-cohort comparison** — "is fluidity increasing?" is a UNIDIFF question, and
  it is the reason the model family survives.
- **Ordinal × ordinal association** with a single interpretable parameter (uniform association) instead
  of (R−1)(C−1) interaction terms.

The unifying feature: **they all analyse association *net of the marginal distributions*.** That is the
one thing percentages, chi², and Cramér's V cannot do, and it is why the family exists.

### 3.3 Assumptions

- Counts are **Poisson / multinomial** — i.e. **independent observations**. Survey clustering and weights
  break this and need Rao–Scott-type corrections (the Clogg–Eliason approach); naive weighted counts give
  badly anti-conservative tests. tabxplor has the infrastructure for this (`tab_robust_overlay()`,
  `svy_omnibus_one()`), but it would have to be wired in deliberately.
- **Fixed, meaningful categories** — the model is about *these* categories, and adding/merging levels
  changes the parameters. Not robust to the level-lumping tabxplor does casually (`lump_others`).
- **Adequate expected counts** — sparse tables (many small cells) make the asymptotic LR test unreliable;
  mobility tables are often large and sparse, which is a live problem in practice.
- **Hierarchy** — you normally include all lower-order terms of any interaction you fit.
- RC and UNIDIFF add **non-linear estimation** (they are log-*multiplicative*, not log-linear), needing
  `gnm`-style alternating least squares, with the attendant identification constraints and convergence
  questions, plus jackknife/bootstrap SEs because the asymptotic ones are unreliable.

### 3.4 When it is truly useful — and when it is overkill

**Truly useful** when *all* of these hold: the table is **square or ordered** with meaningful category
correspondence; you care about association **net of margins**; and you are **comparing** association
across groups, cohorts or countries. That is mobility research, homogamy research, and little else.

**Overkill** for the ordinary exploratory cross-tab, which is tabxplor's entire raison d'être. For a
2-way table, the saturated log-linear model *is* the table — it tells you nothing you did not have — and
the independence model is exactly the chi² test tabxplor already runs. The moment the question is "which
cells deviate, and by how much?", the answer is residuals and percentages, not a model hierarchy.

There is also an honest epistemic mismatch. Log-linear modelling is **confirmatory and specialist**: you
posit a hierarchy, fit several models, compare deviances, defend a choice. tabxplor is **exploratory and
beginner-facing**: "make deviations legible at a glance". Bolting a model-selection workflow onto a
package whose vignettes teach literary students would be adding a second, incompatible epistemic mode.

### 3.5 tabxplor already does the useful descriptive half — under a different name

This is the key finding, and it should change how the question is framed. `color = "contrib"` colours each
cell by its **contribution to chi²**, and its `MEASURES` row carries `ref_kind = "indep"` — *the
independence model*. So tabxplor already:

- fits the independence log-linear model (that is what chi² is),
- computes each cell's departure from it,
- and **renders the result as a colour heatmap** — which is precisely what Bucca (2020, *Socius*)
  proposes as the modern way to read log-linear association patterns.

In other words, the descriptively useful core of two-way log-linear analysis is **already shipped**. It is
simply not labelled in Goodman's vocabulary. Documenting that connection in the vignette ("this colouring
*is* the departure from the independence model") costs nothing and would give sociologist users the bridge
they need — far better value than implementing the model family.

One genuine refinement worth considering here: raw contributions to chi² are not standardized, so large
cells dominate. **Adjusted (Haberman) standardized residuals**, `(O−E)/√(E(1−p_row)(1−p_col))`, are
approximately N(0,1) and therefore directly interpretable ("|z| > 2 ⇒ notable"). That would be a small,
well-scoped addition to the existing colour engine — a new `MEASURES` row, not a new subsystem.

### 3.6 The ecosystem argument — this niche is already well served

The specialist models are covered on CRAN, and by exactly the right people:

- **[`logmult`](https://cran.r-project.org/web/packages/logmult/index.html)** — UNIDIFF, RC(M), RC(M)-L,
  skew-symmetric models, with graphical representations, jackknife/bootstrap SEs **and complex survey
  design support**. Its author is **Milan Bouchet-Valat, a sociologist at INED** — i.e. it was written
  by and for tabxplor's exact target audience, in French quantitative sociology.
- **`gnm`** — the general engine underneath (log-multiplicative / non-linear terms).
- **`MASS::loglm`**, **`stats::loglin`** — plain hierarchical log-linear fitting, already in base/Imports.
- **`vcdExtra`**, **`vcd`** — mosaic plots and model comparison for contingency tables.

Reimplementing any of this would mean duplicating a mature, survey-aware package written by a domain
expert, and would drag `gnm` (or a hand-rolled ALS) into the dependency graph. None of `logmult`, `gnm` or
`vcdExtra` is currently installed here — they would all be new dependencies. The right move is a
**`@seealso` pointer**, not an implementation.

### 3.7 The one narrow exception worth building

There is a real gap, and it is cheap. tabxplor computes a chi² **per sub-table** when `tab_vars` is set,
but it never tests whether those associations **differ from each other**. Measured on GSS
(race × party3 by marital status, probe 9):

```
per-subtable Cramér's V : 0.208  0.220  0.208  0.206  0.189   <- eyeball: "all the same"
homogeneous-association test : LR = 70.68, df = 16, p = 7.6e-09
```

The five effect sizes look interchangeable; the test says the association is *not* homogeneous. That is
exactly the kind of thing a colour-coded table cannot show and a user will get wrong by eye. It is
**one `stats::loglin()` call on counts tabxplor already has** — no new dependency (base `stats`),
measured at **0.15 ms per fit**, and it lands naturally as one more row in the existing `test` attribute
alongside `chi2`/`F`/Cramér's V, rendered by the machinery Phase 18j already built.

**But two honest caveats decide how to present it:**

1. **At survey scale it will almost always reject.** n = 21,483 here, and a Cramér's V spread of
   0.189–0.220 is substantively trivial. A bare p-value would mislead in the *opposite* direction from
   the eyeball. It must ship **with an effect size** — the natural one being the spread of the layer
   parameters, or simply presenting it beside the per-layer Cramér's V it is testing.
2. **Weighted data needs a Rao–Scott correction**, or the test is anti-conservative. The `test = "survey"`
   / `kish_neff` ladder from Phase 18j is the place this belongs, not a naive weighted-count fit.

So: worth doing, as a **small extension of the existing `test` attribute** (a "homogeneity of association"
row, gated on `tab_vars` being present), not as a log-linear modelling subsystem. Everything beyond it —
quasi-independence, RC, UNIDIFF — should point at `logmult`.

---

## Appendix A — References

- Zou, G. (2004). [A modified Poisson regression approach to prospective studies with binary data](https://pubmed.ncbi.nlm.nih.gov/15033648/). *American Journal of Epidemiology*, 159(7), 702–706. — the founding paper; robust variance; reliable down to n ≈ 100.
- Talbot, D., Mésidor, M., Chiu, Y., Simard, M., & Sirois, C. (2023). [An alternative perspective on the robust Poisson method for estimating risk or prevalence ratios](https://arxiv.org/abs/2112.00547). *Epidemiology*. — the method assumes **no** Poisson distribution, only log-linearity.
- Barros, A. J. D., & Hirakata, V. N. (2003). [Alternatives for logistic regression in cross-sectional studies](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-3-21). *BMC Medical Research Methodology*, 3, 21. — the >10 % prevalence threshold.
- Chen, W., Qian, L., Shi, J., & Franklin, M. (2014). [Comparison of robustness to outliers between robust Poisson models and log-binomial models](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-82). *BMC Medical Research Methodology*, 14, 82.
- Chen, W. et al. (2018). [Comparing performance between log-binomial and robust Poisson regression models for estimating risk ratios under model misspecification](https://link.springer.com/article/10.1186/s12874-018-0519-5). *BMC Medical Research Methodology*, 18, 63.
- Hagiwara, Y., & Matsuyama, Y. (2024). [Goodness-of-fit tests for modified Poisson regression possibly producing fitted values exceeding one](https://journals.sagepub.com/doi/10.1177/09622802241254220). *Statistical Methods in Medical Research*, 33(7), 1185–1196.
- Uno, H. et al. (2024). [Firth-type penalized methods of the modified Poisson and least-squares regression analyses for binary outcomes](https://onlinelibrary.wiley.com/doi/10.1002/bimj.202400004). *Biometrical Journal*. — the small-sample remedy.
- [Overestimation of relative risk and prevalence ratio: misuse of logistic modeling](https://pmc.ncbi.nlm.nih.gov/articles/PMC9689401/) (PMC9689401).
- Williamson, T. et al. (2013). [Log-binomial models: exploring failed convergence](https://pubmed.ncbi.nlm.nih.gov/24330636/). *Emerging Themes in Epidemiology*.
- [`survey::svyglm` documentation](https://rdrr.io/cran/survey/man/svyglm.html) — "svyglm always returns 'model-robust' standard errors"; use `quasipoisson` for relative-risk models.
- [`risks` package — marginal standardization](https://stopsack.github.io/risks/articles/margstd.html) — the g-computation alternative.
- Muller, C. J., & MacLehose, R. F. (2014). [Estimating predicted probabilities from logistic regression: different methods correspond to different target populations](https://academic.oup.com/ije/article/43/3/962/763470). *International Journal of Epidemiology*, 43(3), 962–970. — marginal vs conditional target populations (§1.5b caveat 1).
- Localio, A. R. et al. [Using marginal standardisation to estimate relative risk without dichotomising continuous outcomes](https://link.springer.com/article/10.1186/s12874-019-0778-9). *BMC Medical Research Methodology*.
- [Reflection on modern methods: risk ratio regression — simple concept yet complex computation](https://academic.oup.com/ije/article/52/1/309/6843281). *International Journal of Epidemiology*, 52(1). — the practical survey of all the routes.
- Goodman, L. A. (1970). Multivariate analysis of qualitative data: interactions among multiple classifications. *JASA*, 65(329), 226–256. — the *other* "log-linear model", for §1.6's terminology warning.

**Part 3 (Goodman log-linear models):**

- Goodman, L. A. (1979). Simple models for the analysis of association in cross-classifications having ordered categories. *JASA*, 74(367), 537–552. — the RC association model.
- Erikson, R., & Goldthorpe, J. H. (1992). *The Constant Flux*. Oxford: Clarendon Press. — UNIDIFF and the comparative-fluidity programme.
- Xie, Y. (1992). The log-multiplicative layer effect model for comparing mobility tables. *American Sociological Review*, 57(3), 380–395.
- Bucca, M. (2020). [Heatmaps for patterns of association in log-linear models](https://journals.sagepub.com/doi/10.1177/2378023119899219). *Socius*, 6. — reading log-linear association as a colour heatmap; the direct conceptual sibling of tabxplor's `color = "contrib"`.
- [Comparing patterns of intergenerational class mobility using log-linear models](https://www.frontiersin.org/journals/sociology/articles/10.3389/fsoc.2026.1757240/full). *Frontiers in Sociology* (2026). — evidence the family is still live practice.
- Bouchet-Valat, M. [`logmult`: log-multiplicative models, including association models](https://cran.r-project.org/web/packages/logmult/index.html), CRAN. [Reference vignette](https://cran.r-project.org/web/packages/logmult/vignettes/logmult.html). — UNIDIFF, RC(M), RC(M)-L, skew-symmetric, jackknife/bootstrap SEs, complex survey support. Author is an INED sociologist: tabxplor's exact audience.
- Clogg, C. C., & Eliason, S. R. (1987). Some common problems in log-linear analysis. *Sociological Methods & Research*, 16(1), 8–44. — weighting/survey-design corrections (§3.3).
- Haberman, S. J. (1973). The analysis of residuals in cross-classified tables. *Biometrics*, 29(1), 205–220. — adjusted standardized residuals (§3.5).

## Appendix B — Reproducible probes

All numbers in this report come from eight scripts run on 2026-08-05 (R 4.5, WSL2 Ubuntu). The essential
ones:

**Probe 2 — svyglm ≡ HC0 sandwich** (the no-new-dependency claim):

```r
pois <- glm(y ~ x1 + x2, data = d, family = poisson)
X <- model.matrix(pois); mu <- fitted(pois)
bread <- solve(t(X) %*% (X * mu)); meat <- t(X) %*% (X * (y - mu)^2)
hc0 <- sqrt(diag(bread %*% meat %*% bread))
sv  <- summary(svyglm(y ~ x1 + x2, design = svydesign(ids = ~1, weights = ~1, data = d),
                      family = quasipoisson))$coef[, 2]
sv / hc0   # 1.000169 == sqrt(n/(n-1))
```

**Probe 4 — φ-scaling vs sandwich on real GSS data**, and the 500-rep coverage simulation
(naive 0.984 / φ 0.948 / sandwich 0.948; mean OR 1.939 vs mean RR 1.511 for a true RR of 1.50).

**Probe 5A — coverage under heterogeneous baseline risk** (600 reps): φ 0.977 with SEs 11.3 % too wide,
sandwich 0.963.

**Probe 5B — the weighted path is already correct**: `tab_reg(family = "poisson", wt = "w")` returns
1.47 / 1.08, `exp(coef(svyglm(quasipoisson)))` returns 1.4677 / 1.0761.

**Probe 6 — g-computation equivalence**: marginal RR from a logistic fit = 1.6677 [1.546, 1.799];
robust Poisson RR = 1.6679 [1.546, 1.799].

**Probe 7 — the ratio-AME route** (§1.5b, §2.4b). The `lnratioavg` contrast label, the
marginal-vs-conditional divergence, and the coherence identity:

```r
avg_comparisons(lg, variables = "x", comparison = "lnratioavg", transform = exp)
#   contrast                     estimate
#   ln(mean(low) / mean(high))     0.5143   <- same prefix/suffix shape as the existing "lnor" branch
#   ln(mean(mid) / mean(high))     0.8109

# vs modified Poisson (conditional RR), baseline risk ranging 0.10 -> 0.89:
#   0.5110   0.8045                        <- 0.6-0.8 % apart: close, but a different estimand

# coherence: adjusted%(ref) * RR == adjusted%(level)
#   adjusted % : 0.6458  0.3321  0.5237
#   ref * RR   : 0.6458  0.3321  0.5237    <- exact
```

**Probe 8 — robustness of the ratio AME**: 95 % CI coverage **0.948** over 500 reps (true marginal
RR 1.3999, mean estimate 1.3981); works on a weighted `svyglm` (RR 1.3842 [1.313, 1.459]); at 0.7 %
prevalence returns RR 3.21 [1.18, 8.72] against OR 3.24 (converges to the OR, no blow-up); numeric
predictors carry the `mean(+1)` label, which the existing code never parses.

**Probe 9 — the homogeneity-of-association gap** (§3.7), on `gss_simple` race × party3 by marital:

```r
tb <- table(d$race, d$party3, d$marital)
stats::loglin(tb, list(c(1,2), c(1,3), c(2,3)), print = FALSE)   # homogeneous association
#   LR = 70.68, df = 16, p = 7.6e-09      <- the association is NOT the same across layers
stats::loglin(tb, list(c(1,3), c(2,3)), print = FALSE)           # conditional independence
#   LR = 1912.94, df = 20, p < 1e-300

# what tabxplor shows today (per-subtable, from get_test()):
#   Cramér's V  0.208  0.220  0.208  0.206  0.189   <- looks homogeneous by eye
```

Base `stats` only, no dependency; **0.15 ms per fit** (100 fits in 0.015 s). Equivalent to
`anova(glm(n ~ R*C + R*L + C*L, family = poisson), glm(n ~ R*C*L, ...), test = "LRT")`.
⚠ n = 21,483 here — see §3.7 caveat 1 on why this must ship with an effect size, not a bare p.
