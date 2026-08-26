# The regression subsystem — the derivations behind the table

> PURPOSE: the statistical arguments `tab_reg()` rests on — the gap test between an observed and a
> modelled effect, the risk ratio for a binary outcome, the one-column ordinal effect, why
> `predictors` is not R's formula language, and the specs of the two chart families.
> ROLE: what `R/reg-influence.R`, `R/reg-empirical.R`, `R/reg-estimand.R`, `R/reg-cross.R`,
> `R/reg-assumptions.R` and `R/plots.R` implement but are too short to derive. Each of those file
> headers is the current spec; this is why it is that spec, and what it costs.
> KEY CONSTRAINTS:
>   - Every measurement here was taken on `gss_cat` or by simulation, on the code that ships.
>     ⚠ A number is evidence for a design, never a value to read back — read values from the code.
>   - §2.5 and §4.4 turn on **collapsibility**. That one property decides which columns can be
>     tested and which can only be described; do not weaken either statement.
> See: `CLAUDE.md § tabxplor architecture` (the regression subsystem) · `dev/inference.md` (the same
> influence-function algebra, for a crosstab's cells) · `vignettes/articles/tabxplor-all-else-equal.Rmd`
> (what all of this is *for*).

---

## 1. The one idea

`tab_reg()`'s distinctive feature is a **comparison**: each modelled effect sits beside the observed
(crude) one — the same estimand, on the same people, with one predictor instead of all of them — so
*what did adjustment change* is read across the table.

Everything below serves that comparison:

- §2 makes the gap **testable**, which is what turns `color = "adjustment"` from a description into a
  test.
- §3 and §4 make the compared quantity **collapsible**, without which the gap means nothing.
- §5 keeps every row a row that *has* an observed counterpart.
- §6 draws the comparison, and §7 applies the very same verb — *where does the model disagree with
  the data, and does it matter?* — to the model's own assumptions.

---

## 2. The gap between an observed and a modelled effect

### 2.1 What is being tested, and what it is not

| mode                                        | compared with                  | the two estimates are…          |
|---------------------------------------------|--------------------------------|---------------------------------|
| `color = "adjustment"`, one model            | its own `Obs_*` crude effect   | **dependent** (same rows, r = 0.52–0.90) |
| `color = "adjustment"`, several `predictors` sets | the one shared crude effect | dependent                        |
| `color = "between_groups"` (`tab_vars`)      | the reference group's estimate | **independent** (disjoint rows)  |

The null is the same in all cases: **the two estimates are equal on the estimate's own test scale** —
the log ratio for a multiplicative effect, the plain difference for an additive one. That is exactly
the scale `fmt_adjustment_score()` folds around 1 or 0, so the test and the colour cannot drift
apart.

⚠ **What it is not.** It is not a test for confounding, not a causal claim, and not a model-selection
device. The change-in-estimate criterion it makes precise is itself contested as a
confounder-selection rule. The package's job is to say *how much two numbers in the table differ, and
whether that difference is bigger than noise* — nothing more, and the documentation must say so.

### 2.2 Everything called "observed" is a saturated GLM

`reg_empirical()` builds each crude effect by hand from weighted cell sums. Against the coefficient of
a one-factor GLM at the matching link (n = 4 000, three-level predictor):

| crude column                    | matching saturated fit          | max abs. difference |
|---------------------------------|---------------------------------|--------------------:|
| `Obs_OR` (binomial)             | `glm(y ~ x, binomial)`          |             9.2e-14 |
| `Obs_%` risk difference         | `glm(y ~ x, gaussian)`          |             1.7e-14 |
| `Obs_RR` (Katz)                 | `glm(y ~ x, poisson)`           |             1.8e-10 |
| `Obs_IRR` (poisson counts)      | `glm(y ~ x, poisson)`           |             3.7e-10 |
| `Obs_OR` **weighted**           | `glm(y ~ x, binomial, weights)` |             8.9e-16 |

And the influence function of that saturated fit reproduces the **Woolf** standard error the crude
column already prints, to `ratio 1.0000`.

This matters more than it looks: the crude side of the gap test is **not an approximation bolted
beside the crude column — it is the same estimator**, so the test can never contradict the interval
printed next to it.

**The closed form, so no refit is needed.** For a saturated one-factor model, level *l* against
reference *r*, with weights *w*:

```text
IF_i  =  1(x_i = l) · w_i (y_i − μ_l) / Σ_{x=l} w · g'(μ_l)
       − 1(x_i = r) · w_i (y_i − μ_r) / Σ_{x=r} w · g'(μ_r)

g'(μ) = 1/(μ(1−μ))   logit     (OR)
        1/μ          log       (RR, IRR)
        1            identity  (risk difference, mean difference)
```

Measured against the fitted equivalent at n = 8 000, the worst disagreement is 2.4e-13, and it is
**21× cheaper** than fitting the saturated model. ⚠ The architectural consequence is the one worth
keeping: `REG_EMPIRICAL` gains **one column — the link** — and the same row then drives the crude
column, its CI *and* its influence function. No per-family test code, no new branch. That is why
`REG_EMP_BY_LINK` indexes it by the measure's link.

### 2.3 The variance: only the difference carries the covariance

Both estimators solve estimating equations `Σᵢ ψ(Oᵢ, θ̂) = 0` on the **same rows**, so both are
asymptotically linear, `θ̂ − θ ≈ Σᵢ IFᵢ`. The difference of two such estimators is the sum of the
differences of their influence functions:

```text
Var(θ̂_adj − θ̂_crude) = Σᵢ ( IFᵢ^adj − IFᵢ^crude )²
```

which carries the covariance between them **inside the square** — and there is no other way to get
it. This is *seemingly unrelated estimation*: Stata's `suest` builds exactly this joint sandwich by
stacking the two score vectors and treating each observation as contributing to both.

Measured on a real design (8 strata × 30 clusters × 12 units, with a cluster random effect):

| SE of the gap                       | value    | verdict                       |
|-------------------------------------|---------:|-------------------------------|
| design-based, via `svyrecvar`       | 0.036922 | the answer                    |
| JKn replicate weights               | 0.037004 | ratio 0.998 — **validation**  |
| IID stacked IF, ignoring the design | 0.034612 | **6 % too small**             |
| naive `sqrt(se1² + se2²)`           | 0.120481 | **3.3× too large**            |

Two conclusions. The design-based route is *exact* — it is how `survey` computes its own variances —
and **ignoring the design is not a safe approximation**, even on a mild one. `survey` has exposed
`influence = TRUE` since 2020, added so `svyby()` / `svycontrast()` could estimate covariances between
domain estimates: the same problem, the same object. So on every path that already fits through
`svyglm` the influence functions are one argument. For the plain `glm` paths:

```text
U = X · w(y − μ)·μ'(η)/V(μ)      A = Xᵀ diag(w·μ'(η)²/V(μ)) X      IF = U A⁻¹
```

⚠ Over-dispersion needs nothing extra: the sandwich already absorbs φ.

### 2.4 The AME path differs slightly from the printed AME's SE, on purpose

An AME is not a GLM coefficient — `AME = (1/n) Σᵢ [μ(1, zᵢ) − μ(0, zᵢ)]` depends on the empirical
covariate distribution as well as on β — so its influence function has two terms:

```text
IFᵢ^AME = (gᵢ − AME)/n   +   Gᵀ IFᵢ^β        gᵢ = μ(1,zᵢ) − μ(0,zᵢ),  G = ∂AME/∂β
          ^ empirical average   ^ delta method
```

`marginaleffects` reports the delta term with X held fixed (measured `ratio 1.0000` against it); the
omitted averaging term is +0.12 % on a plain logistic and **grows with effect heterogeneity across
covariate patterns**. So the AME gap test carries its own influence function, and the small difference
from the printed AME's SE is not a defect — the two answer different questions.

### 2.5 What the test rejects — and the collapsibility trap

Calibration, on data where the adjuster is independent of the predictor **by construction**, so the
true crude and adjusted effects are the same quantity and an honest test must not reject:

| n      | rejection α=.05, log OR | log RR | AME   | mean gap, log OR |
|--------|------------------------:|-------:|------:|-----------------:|
| 500    |                   0.160 |  0.052 | 0.052 |          +0.0668 |
| 2 000  |                   0.580 |  0.040 | 0.044 |          +0.0725 |
| 8 000  |                   0.992 |  0.044 | 0.044 |          +0.0712 |
| 32 000 |               **1.000** |  0.042 | 0.042 |          +0.0717 |

Read the two halves separately.

- **Collapsible scales (RR, IRR, β, AME): textbook.** The gap is 0 in expectation and the test holds
  its nominal size at every n. `grey_non_signif` and `guaranteed_effect` mean exactly what a user
  expects.
- **Odds ratio: the test is correct and the *interpretation* is the trap.** The gap converges to a
  fixed non-zero constant (+0.072 log units ≈ ×1.075) that has nothing to do with confounding, so
  power converges to 1. **At survey sizes every OR row would be "significant"** — and the claim a
  reader would make is "adjustment mattered here".

⚠ **Hence the shipped rule: on a non-collapsible measure the movement is coloured but never tested.**
No `gap_se` is written there, and `fmt_gap_force_policy()` falls the column back to `ignore`, with the
legend carrying the non-collapsibility caveat. A coloured *and starred* gap cell is therefore always a
true statement about confounding.

The principled alternative, verified and deliberately **not** taken: comparing the crude OR with the
**marginally standardised** OR restores exact calibration (mean gap +0.0023, rejection 0.052). But it
would change *what is compared* without changing what the cell prints — a documentation problem at
least as bad as the one it solves. The user who wants a testable gap has three routes that already
exist and are perfectly calibrated: `effect = "marginal"`, `link = "ratio"`, `family = "poisson"`.

---

## 3. Risk ratios for a binary outcome

### 3.1 The issue is the estimand, not the fit

Logistic regression is not "biased" for a binary outcome — it is the maximum-likelihood fit of the
correct distribution. The problem is what its coefficient *means*.

| model                 | link  | exponentiates to                   | estimand              |
|-----------------------|-------|------------------------------------|-----------------------|
| logistic (`binomial`) | logit | **odds ratio** (OR)                | ratio of *odds*       |
| modified Poisson      | log   | **risk / prevalence ratio** (RR)   | ratio of *probabilities* |
| log-binomial          | log   | RR                                 | ratio of *probabilities* |

The OR ≈ RR approximation holds only when the outcome is **rare**. As prevalence rises the OR moves
away from 1 — it exaggerates. Measured at 43 % prevalence: an OR of **2.53** against an RR of
**1.64** on the same predictor. A reader narrating "2.5 times more likely" is wrong by a factor of
~1.5.

⚠ This is a **communication** failure, not a statistical one — which makes it exactly this package's
problem. `Model_OR = 2.53` in a coloured table *invites* the misreading, and the target reader is
precisely the one who will make it.

### 3.2 Non-collapsibility, the second and stronger reason

The OR is non-collapsible: the marginal OR is not a weighted average of stratum-specific ORs, even
with no confounding at all. So:

- adding a covariate changes the OR **even when that covariate is not a confounder** — the estimand
  itself depends on the adjustment set;
- therefore **comparing OR coefficients across nested models is not valid** — a staple of
  sociological practice, and something `predictors = list(m1 =, m2 =, m3 =)` actively encourages;
- RR and the risk difference **are** collapsible, so an RR table supports the nested-model narrative
  that a logistic table does not.

⚠ This argument applies **regardless of prevalence**, and is the stronger of the two for this
package's use case.

### 3.3 Why plain Poisson is wrong and the sandwich is mandatory

Fitting Poisson to a 0/1 outcome deliberately misspecifies the variance: Poisson assumes `Var = μ`,
the truth is Bernoulli, `Var = μ(1−μ)`. Point estimates stay consistent (the log-link score equation
is unbiased) but naive standard errors are too large by roughly `√(1−μ)` — measured on GSS, Pearson
dispersion 0.4868 against `mean(1−μ)` 0.4867, exactly as theory predicts, giving 98.4–99.8 % coverage
instead of 95 %. Zou's fix keeps the estimating equation and replaces the variance with the
**Huber–White sandwich**.

⚠ **The objection to pre-empt, and the answer to give verbatim:** *"Poisson for a yes/no variable?"*
The robust Poisson method **does not assume a Poisson distribution at all**. It is a semiparametric
estimator whose only real assumption is that log(risk) is linear in the covariates; the Poisson
likelihood is a convenient vehicle for the score equation.

This is why `survey::svyglm()` is the fitter for the `ratio` and `difference` links even with no
weights, and why `survey` is an Import rather than a Suggest: a misspecified likelihood needs robust
standard errors.

⚠ **"Modified Poisson" is not a "log-linear model"** — a real trap for a sociology audience, where
"log-linear model" means Goodman's models for contingency-table counts. Different object, different
literature.

---

## 4. One column for an ordinal model

### 4.1 The problem

An ordinal model fitted with four outcome categories reports four columns, all significant, all
saying the same single thing — *this group sits lower on the ladder* — because the model has only one
parameter per predictor level to say it with. The four numbers are not four findings; they are one
finding refracted through four category boundaries, and the refraction depends on where the cutpoints
happen to fall, not on anything the reader wants to know.

The reason to fit an ordinal model rather than a multinomial one is precisely that **one number is
claimed to be enough**. Reporting K columns throws that away, while the cumulative odds ratio keeps
one column at the price of a quantity this package's reader cannot interpret.

### 4.2 Why no average works

- The per-**level** marginal risk differences on `P(Y = k)` **sum to exactly 0** (measured 2.8e-17).
  Averaging them averages to zero, always. There is no summary there.
- The per-**cut** marginal effects on `P(Y ≥ k)` are informative but cut-dependent, and their sum is
  exactly the change in mean assigned score — so "average the cumulative effects" and "the mean-score
  effect" are the same measure, which needs arbitrary numeric scores.
- Structurally: **under proportional odds exactly one measure is cut-invariant, and it is the odds
  ratio.** That is what the PO assumption *says*. Any one-column alternative must therefore either be
  the odds ratio, or be a functional of the **whole** predicted distribution that never picks a cut.

### 4.3 The superiority pair

Take two people, one from the group and one from the reference group, and ask *who ends up higher on
the scale*. Three probabilities exhaust the answer:

```text
win  = P(Y_group >  Y_reference)
loss = P(Y_group <  Y_reference)
tie  = P(Y_group == Y_reference)          win + loss + tie = 1
```

From that one pair, three readings — the same difference/ratio duality the package already applies to
a level and its reference:

| reading         | formula                    | neutral | literature name                           |
|-----------------|----------------------------|---------|-------------------------------------------|
| the probability | `γ = win + tie/2`          | `0.5`   | ordinal superiority · probability of superiority · AUC |
| the difference  | `δ = win − loss = 2γ − 1`  | `0`     | Somers' D · Cliff's delta                 |
| the ratio       | `WR = win / loss`          | `1`     | win ratio · Agresti's generalised odds ratio |

`γ` is the natural `{base}` (a percentage, 50 % on the reference row), `δ` the `{est}` under
`measure = "difference"`, `WR` the `{est}` under `measure = "ratio"`. The sentence a reader gets is
*"all else equal, a Black respondent out-earns a White respondent 47 times out of 100 instead of
50"* — no odds, no log, no cutpoint, no latent variable.

**It is the exact K-category generalisation of what the package already prints.** At K = 2 the pair
collapses onto the binomial family's own measures: `δ` **is** the risk difference exactly, and `WR`
**is** the odds ratio exactly. So this is not a new concept bolted on; it is the concept the package
already teaches on a two-level outcome, written for K levels. Verified against `Hmisc::somers2` on
raw data: `γ` equals its `C` and `2γ − 1` its `Dxy` to every printed digit.

### 4.4 Why the marginal (unmatched) definition, and why it is decisive

Two ways to draw the pair: **matched** (the two people share covariates — under a probit link exactly
`Φ(β/√2)`) or **marginal** (drawn independently from the population, each forced to their own level).
The marginal one wins on three grounds, one decisive.

**(a) It is collapsible.** Simulated PO data with `Z` independent of `X`, so an honest crude/adjusted
comparison must show *no movement*:

| `β_z` | crude γ | adjusted γ, **marginal** | adjusted γ, matched | crude cumOR | adjusted cumOR |
|-------|--------:|-------------------------:|--------------------:|------------:|---------------:|
| 0     |  0.6216 |        0.6216 (−0.0000)  |    0.6216 (−0.0000) |        2.18 |           2.18 |
| 1     |  0.6023 |        0.6024 (+0.0001)  |    0.6150 (+0.0126) |        1.92 |           2.17 |
| 2     |  0.5750 |        0.5761 (+0.0012)  |    0.5990 (+0.0240) |        1.62 |           2.21 |
| 3     |  0.5583 |        0.5572 (−0.0010)  |    0.5819 (+0.0236) |        1.47 |           2.24 |

The odds ratio moves 1.47 → 2.24 (+52 %) with **zero** confounding. The marginal superiority measure
does not move at all; the matched one does, mildly. ⚠ **That is what makes `color = "adjustment"` a
genuine test on an ordinal column** — the one thing §2.5 cannot offer on the odds-ratio scale.

**(b) Its crude twin is the same formula.** The marginal `γ` applies the plain two-distribution
formula to the two g-computed marginal distributions; the observed column applies the same formula to
the two row percentages of the cross-table. One formula, two inputs — the package's stated identity
for the observed companion.

**(c) It costs nothing.** The pairwise double sum factorises exactly to a function of the two column
means, which are precisely the quantities already averaged to build the per-level columns. Verified:
brute-force pairwise mean over 20×20 distributions `= 0.4943717064`; the shortcut `= 0.4943717064`.

### 4.5 It is robust where the per-level columns are not

- **Proportional-odds violation.** On a model where `brant::brant()` firmly rejects PO
  (`X² = 65.21, df = 28, p = 8.5e-05`), `γ` from the PO fit and `γ` from an unconstrained
  `nnet::multinom()` agree to **at most 0.0033** across all 13 predictor levels. The one-column
  summary is therefore *more* robust than the four columns it replaces, not less.
- **Number of categories.** `γ` (and hence `δ`) is stable from K = 3 onwards — 0.6000 / 0.6060 /
  0.6087 / 0.6112 / 0.6125 for K = 3, 4, 5, 7, 10 at cumOR 2. ⚠ **The win ratio is markedly less
  stable** (2.00 → 1.63 over the same range) because it conditions on discordant pairs, whose share
  changes with K. That is why `δ` is the headline reading and `WR` the secondary one.

### 4.6 Inference

- **The estimate's interval: map the endpoints, do not use the delta method.** `γ` is strictly
  increasing in `β_j` and is `0.5` *exactly* at `β_j = 0`, so pushing the Wald interval of `β_j`
  through the functional costs two evaluations and **guarantees** the invariant the package requires
  — the interval excludes the neutral value exactly when the model's own test rejects, so colour,
  greying and stars cannot disagree. Worst disagreement against the delta method: 0.003.
- **The observed column: closed form, no refit.** `γ`'s crude variance has the standard
  DeLong / placement-value form, `placement_i(k) = F_other(k−1) + 0.5·p_other(k)`. Measured analytic
  SE 0.00664 against a 2 000-draw bootstrap 0.00650.
- **The gap SE** follows §2.3, with the crude side's gradients being the placement values — so
  DeLong's variance is *arrived at* rather than special-cased.
- **Weights and designs** need nothing special: `γ` is a smooth function of two weighted means, so
  weights enter by weighting the column means and a design-based variance goes through the same
  influence functions.

### 4.7 Caveats, and prior art

- **`γ` is a rank measure and cannot be turned back into a quantity.** It answers *how often*, not
  *how much*. Where categories have real numeric content, a mean-score reading would be a separate,
  opt-in measure requiring user-supplied scores.
- **Its ceiling depends on the marginal spread**: `δ` cannot exceed `1 − tie`. An outcome piled into
  one category compresses the measure — a property of the data, not a defect, but it must be said,
  and it is why `γ` (bounded 0–1, anchored at 50 %) reads better for a lay audience than `δ`.
- **The win ratio is non-transitive.** Harmless in a table where every level is compared to one
  common reference — which is this layout — but it forbids reading the table as a ranking of the
  levels among themselves.
- **The matched and marginal versions are different estimands, not two spellings.** Anyone comparing
  output against Agresti & Kateri's published `Φ(β/√2)` will find a gap, and the gap is the point.
- **Proportional odds still needs checking.** §4.5 shows the *summary* is robust to PO violation, not
  that the model is correct; when PO fails badly the per-level shape may be a real finding that the
  one-column summary hides. That is why `REG_CHECKS` keeps a proportionality check with "look at the
  per-level columns" as its cure.

⚠ **Prior art: nothing in the R ecosystem does this from a fitted, covariate-adjusted ordinal
model.** `marginaleffects` gives per-level marginal effects with no distribution-level functional;
`rms::orm` / `Hmisc::somers2` report `Dxy` / `c` for the model as a whole, not per predictor level;
`effectsize::p_superiority`, `orddom`, `RProbSup` are data-level and unadjusted; `genodds`, `WINS`,
`hce` are two-sample or trial-oriented; `pim` models the probabilistic index directly, but as a
separate model class rather than a reading of a model the user already fitted.

---

## 5. Why `predictors` is not R's formula language

R's formula language lets you write `a`, `b`, `a:b`, `a + a:b`, `a + b + a:b` and treat each as a
model of its own. `predictors` refuses two of those spellings. The reason is not taste, and the
measurement is the argument. `origin-inv` asks: does the fit change when a continuous parent is
shifted by a constant — which is exactly what `ref` does, at the mean, by default?

| parents           | RHS                           | logLik    | rank-deficient | origin-inv |
|-------------------|-------------------------------|----------:|----------------|------------|
| factor × factor   | `race:age4`                   | −7418.171 | **YES**        | yes        |
|                   | `race + age4 + race:age4`     | −7418.171 | —              | yes        |
| numeric × factor  | `age:race`                    | −7621.319 | —              | **NO**     |
|                   | `age + age:race`              | −7621.319 | —              | **NO**     |
|                   | `race + race:age`             | −7580.838 | —              | yes        |
| numeric × numeric | `age:tvhours`                 | −7758.718 | —              | **NO**     |
|                   | `age + tvhours + age:tvhours` | −7630.699 | —              | yes        |

Read down the `logLik` column and the design falls out:

- **factor × factor: there is only ONE model.** All spellings are the saturated cell model to the last
  digit; `race:age4` alone is a redundant parametrisation of it, and the one the parent rule refuses
  is **rank-deficient** — not a modelling choice but an error a fitter has to paper over.
- **numeric × factor: there are TWO models**, and the pair without the moderator's main effect is not
  origin-invariant.
- **numeric × numeric: there are FOUR models**, and exactly one is origin-invariant.

**"The terms plus the interaction" is a spelling, not a model.** On the classical star formula
`married ~ age + race + age:race` against what `tab_reg()` fits, `race + race:age`:

```text
logLik star −7580.837869   nested −7580.837869   max |fitted difference| = 9.99e-16
```

The same fit. What differs is what a **row** says: the star prints one slope plus two *differences of
slopes*; the nested prints the three slopes. Each is recoverable from the other by addition, so
nothing is lost — but only one of them can be read without arithmetic, and ⚠ **only one of them gives
every row a level, a count, an observed counterpart and a reference row.** That is the whole
argument: a row that cannot have an observed twin has no place in a table built around the
observed/modelled pair.

So the refusals forbid, for a categorical moderator, a redundant or rank-deficient re-spelling of the
model already being fitted; and for a continuous one, the origin-dependent spellings — which matter
here more than in a bare `glm()` precisely because `ref` shifts a continuous predictor's origin by
default.

---

## 6. The effect plot

`forest_plot()` draws the table's comparison. `tab_estimates()` is the chart model behind it, and
returns the tidy frame if you want to draw it yourself.

### 6.1 The scale rule

An estimate column's axis is fully determined by facts already stored, and **the dispatch order is
the contract** (it is `fmt_gap_scale_key()`'s order, and for the same reason: a Poisson count AME and
a raw Poisson coefficient are identical in every attribute except `var`):

| # | test                                            | key                        | neutral | transform | axis                             |
|---|-------------------------------------------------|----------------------------|---------|-----------|----------------------------------|
| 1 | `display_primary ∈ {pct, mean}`                 | `level_pct` / `level_mean` | —       | identity  | `%` / units of Y                 |
| 2 | `ci_type ∈ {or, ratio}`                         | `ratio`                    | 1       | **log10** | the effect word                  |
| 3 | `type == "coef"` and `var` not all NA           | `raw_diff`                 | 0       | identity  | units of Y, + SD(Y) second axis  |
| 4 | `type == "coef"` and a log-scale family         | `log_coef`                 | 0       | identity  | log-odds, + ratio second axis    |
| 5 | otherwise (probability scale)                   | `points`                   | 0       | identity  | percentage points                |

⚠ **Clause 1 must come first.** `Obs_%` and `Obs_diff` carry identical fields — both have `pct` *and*
`diff` non-NA — so only `display` distinguishes a level column from an effect column. `display` is a
stored *field*, not a rendered string, so reading it obeys the roles-are-stored rule.

### 6.2 The gap band *is* the test

The classic crude-vs-adjusted forest plot draws two point-and-whiskers per row and invites the reader
to compare overlap. Schenker & Gentleman (2001) is the standard citation for why that is wrong:
overlap-based judgement is materially more conservative than the correct test, and their prescription
is explicit — *"test by examining the confidence interval for the difference between the two
estimates."* Here it is worse than merely conservative, because the two estimators are **correlated**
(§2.3).

So draw, around the **observed** point, the interval

```text
band = obs ⊕ ± z(conf_level) · gap_se        ( ⊕ = × on a ratio scale, + on an additive one )
```

and put the modelled point where it falls. Then:

> **the modelled point lies outside the band ⟺ the gap test rejects at `conf_level`.**

⚠ That is an **identity, not an approximation**, because `gap_se` is stored on the estimate's own test
scale and the gap p-value is `2·pnorm(−|gap| / gap_se)` on that same scale.

### 6.3 The colour mapping invents nothing

Every channel maps to the geometry corresponding to what it paints in the table:

| in the table                              | in the plot                       |
|-------------------------------------------|-----------------------------------|
| cell **text** colour (channel 1)          | the **point's** colour            |
| cell **background** (channel 2)           | a **band behind the row**         |
| bold / italic / underline (print palette) | the point's stroke weight / shape |
| grey (slot 0)                             | grey point                        |
| the break ladder                          | the **gridlines**                 |
| the legend prose                          | the **caption**                   |
| the stars                                 | optional text beside the point    |

The row band is the literal translation: a two-channel table paints the cell's background, the plot
paints the row's, with the same hex — and the background ramp is very pale by design, which is
exactly what a behind-the-data band needs.

---

## 7. The model checks

### 7.1 The same comparison, applied to something other than an effect

| what is compared                             | Observed                      | Model                        | the gap        |
|----------------------------------------------|-------------------------------|------------------------------|----------------|
| the **shape** of a continuous predictor      | the binned curve of the data  | one straight line            | curvature test |
| the **spread** of the outcome                | the empirical (sandwich) variance | the variance the family assumes | SE ratio  |
| the **meaning** of an ordinal effect         | one odds ratio per cut        | one cumulative odds ratio    | Brant test     |
| the **weight** of one respondent             | the estimate without them     | the estimate with them       | dfbetas        |
| the **separability** of two predictors       | —                             | —                            | VIF *(the exception)* |

A user who has understood `empirical = TRUE` has therefore already understood the checks: *where does
the model disagree with the data, and does it matter?* One verb, one vocabulary, one legend voice.

### 7.2 Why they must exist

The model used throughout the vignettes is mis-specified, and without the checks **no output reveals
it**. Letting `age` curve instead of run straight, on `married ~ race + age + rincome + relig`:

| printed row              | OR, age linear | OR, age quadratic |                        change |
|--------------------------|---------------:|------------------:|------------------------------:|
| income $25 000 or more   |          1.863 |             1.419 |                   **−23.8 %** |
| income $15 000 to 24 999 |          1.273 |             1.056 | −17.0 % (**p 0.0001 → 0.40**) |
| race Black               |          0.416 |             0.398 |                        −4.3 % |

⚠ **The damage is not confined to the mis-specified row** — that is the whole argument. One income
level's conclusion flips at the 5 % threshold, and the two models tell opposite stories about age
itself (P(married) at 85: 0.686 linear vs 0.219 quadratic). The gap test of §2 gives that row a crude
twin and tests the difference; neither can say *"one slope is the wrong summary, and it is bending the
income effect you came here to read."*

### 7.3 The five checks

| # | check                     | the question, in one sentence                        | statistic                | families            |
|---|---------------------------|------------------------------------------------------|--------------------------|---------------------|
| 1 | **Linearity**             | Is this predictor's effect really one straight line? | curvature test p         | all                 |
| 2 | **Proportionality**       | Is one odds ratio enough for every cut?              | Brant p                  | ordinal             |
| 3 | **Dispersion**            | Are the standard errors wide enough?                 | max robust/model SE      | all                 |
| 4 | **Influence**             | Does one respondent carry the result?                | max \|dfbetas\|          | all                 |
| 5 | **Collinearity**          | Can the data tell these predictors apart?            | max GVIF                 | all but multinomial |

**The order is the order they print, and it is the order of what each one threatens**: the estimate
(1), what the estimate means (2), its interval (3), whether it is real at all (4), why it is wide (5).
Every noun is a word the reader already knows and the parenthesis names the instrument — the same
convention as the crosstab summary's `"pvalue (Chi2, Welch F; Kish)"`.

Two of the five are unifications worth keeping in mind:

- **Dispersion, measured on the SEs, replaces four textbook checks with one number.**
  `max |SE_robust / SE_model|` measured **1.43 where √φ = 1.40** under Poisson over-dispersion,
  **3.91** under clustering, and **1.00 ± 0.01** on correctly-specified replicates. It is orthogonal
  to Linearity (0.992 under a mis-specified mean — it does not double-count), and ⚠ it never touches
  `df.residual`, so the `df.residual(svyglm) = design df` trap that forces φ to be refused on
  clustered fits (φ read 22.49 instead of 1.00) simply does not arise.
- **Influence is reported as a reassurance, not an alarm.** Cook's D is unreadable at survey n (max
  0.0009 at n = 12 990, and its conventional cutoff of 1 fires at no n measured), so the row uses
  **dfbetas** — the same question, one coefficient at a time, on a scale that reads at any n: *"no
  single respondent moves any coefficient by more than 0.19 SE"*. That is a sentence a reviewer
  wants.

### 7.4 Three surfaces, each check appearing once

A check appears at the first surface that can answer it reliably, and only there. The teaching plots
may repeat anything, because repetition is what teaching is.

| surface                      | what it is                                                    | who sees it                      | which checks   |
|------------------------------|---------------------------------------------------------------|----------------------------------|----------------|
| **1. a footer row**          | one number + a threshold, in the model-fit block              | every export, always             | all five       |
| **2. the row's sparkline**   | bins of the observed curve, inside the predictor's level label | every export (text) + html (svg) | Linearity only |
| **3. `reg_check_plots()`**   | the classic panels, faceted across models                     | on request, for teaching         | any, freely    |

Linearity only looks like it breaks the rule. It does not: **the sparkline is the Observed side and
the footer p is the gap** — exactly as a numeric predictor's row shows `Obs_OR` beside a gap the
colour and the test judge. One check, the framework's own two parts.

⚠ **Nothing on surfaces 1 and 2 needs the data or a refit.** Surface 1 is a function of the fit
(computed in the build, stored in the `test` attribute); surface 2 is a function of the raw columns
(computed in the build, stored in `meta`). So a table that has travelled to Excel still carries its
verdicts, and a user who never calls the plot function is never uninformed. That is the answer to
*"is refitting the only way?"* — no, and the surfaces that matter never refit.

---

## 8. Re-running the evidence

- `dev/verify_reg_invariants.R` — every reachable family × effect × measure.
- `dev/tests/testthat/` — the exhaustive sweeps and the secondary parity arms:
  `test-reg-influence-sweep.R`, `test-reg-empirical-sweep.R`, `test-reg-estimand-sweep.R`,
  `test-reg-assumptions-sweep.R`.
- `dev/vif_car_parity.R` — the vendored `tx_vif()` against real `car::vif()` on 14 fit shapes. ⚠ This
  cannot be a test, because `car` is no longer a dependency: re-run it by hand after any change.

## 9. References

**The gap test.** Weesie (1999), *Seemingly unrelated estimation and the cluster-adjusted sandwich
estimator*, Stata Technical Bulletin 52. · Mize, Doan & Long (2019), *A general framework for comparing
predictions and marginal effects across models*, Sociological Methodology 49 — the canonical
sociological statement, covering all four of the modes above. · Schenker & Gentleman (2001), *On
judging the significance of differences by examining the overlap between confidence intervals*, The
American Statistician 55(3). · Lumley, `survey` (the `influence = TRUE` route). · Maldonado &
Greenland (1993) for the 10 % change-in-estimate rule, and its critics.

**Risk ratios.** Zou (2004), *A modified Poisson regression approach to prospective studies with
binary data*, AJE 159(7). · Talbot et al. (2023), *The change in estimate method…* and the
semiparametric reframing of robust Poisson, Epidemiology. · Greenland, Robins & Pearl (1999),
*Confounding and collapsibility in causal inference*, Statistical Science 14(1).

**Ordinal superiority.** Agresti & Kateri (2017), *Ordinal probability effect measures for group
comparisons in multinomial cumulative link models*, Biometrics 73. · Somers (1962). · Cliff (1993),
*Dominance statistics*. · DeLong, DeLong & Clarke-Pearson (1988), *Comparing the areas under two or
more correlated receiver operating characteristic curves*, Biometrics 44. · Harrell, *Regression
Modeling Strategies* (the concordance probability and its insensitivity to PO violation). · Brant
(1990), *Assessing proportionality in the proportional odds model*, Biometrics 46.

**Model checks.** Belsley, Kuh & Welsch (1980), *Regression Diagnostics* (dfbetas). · Fox & Monette
(1992), *Generalized collinearity diagnostics*, JASA 87 (the GVIF `tx_vif()` implements). · White
(1980), *A heteroskedasticity-consistent covariance matrix estimator*, Econometrica 48.
