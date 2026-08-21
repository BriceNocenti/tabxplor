# One column for an ordinal model — the ordinal superiority measures

`tab_reg(family = "ordinal")` reports a marginal, readable effect in **one column per model**, not one per outcome level. The measure is standard in the statistical literature, it is *exactly computable from the numbers `tab_reg()` already builds*, and it is the **exact generalisation to K categories of what tabxplor already does on a binary outcome**.

This file is the expert guide to that measure: why it and not another (§§1–3), what it looks like (§4), how it is inferred (§5), how it is built (§6), and what it cannot do (§7). Every figure was measured on `gss_cat_data_formatting()` or by simulation and is reproduced inline so it can be re-checked; §9 is the checklist those checks became.

---

## 1. The problem, reproduced

```r
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE, measure = "ratio")
```

gives four columns, one per level of `rincome`:

```text
 var      levels             n            1-Lt $10000       2-$10000 to 14999  3-$15000 to 24999  4-$25000 or more
 race     White              9 846                     1                  1                  1                  1
 race     Black              1 860   ×1.17*** (×1.43)   ×1.11*** (×1.27)   ×1.06*** (×1.12)   ÷1.09*** (÷1.22)
 race     Other              1 252   ×1.33*** (×1.40)   ×1.20*** (×1.25)   ×1.09*** (×1.12)   ÷1.17*** (÷1.21)
 marital  Widowed              401   ×2.39*** (×1.99)   ×1.68*** (×1.58)   ×1.21*** (×1.25)   ÷1.74*** (÷1.47)
```

Four numbers, all significant, all saying the same single thing — *this group sits lower on the income ladder* — because the ordinal model has only one parameter per predictor level to say it with. The four marginal risk ratios are not four findings; they are one finding refracted through four category boundaries, and the refraction is arbitrary: it depends on where the cutpoints happen to fall and on how the sample is spread over the categories, not on anything the reader wants to know.

The reason to fit an ordinal model rather than a multinomial one is precisely that **one number is claimed to be enough**. Reporting K columns throws that away and leaves the table as wide as the multinomial one it was meant to replace, while the default `measure = "odds_ratio"` keeps one column at the price of the cumulative odds ratio — a quantity the package's own target reader cannot interpret.

### Why the per-level route cannot be collapsed by averaging

Two measured facts close off the obvious shortcuts (`race` Black vs White, adjusted, g-computed over the 12 958 complete cases):

- the per-**level** marginal risk differences on `P(Y = k)` are `+0.0272, +0.0097, +0.0099, -0.0468` and **sum to exactly 0** (measured: 2.8e-17). Averaging them is averaging to zero, always. There is no summary there.
- the per-**cut** marginal effects on `P(Y >= k)` are informative but cut-dependent: risk differences `-0.0272, -0.0369, -0.0468`, risk ratios `0.968, 0.951, 0.919`. Their sum is exactly the change in the mean assigned score: measured `dE[score] = -0.11096910` and `sum_k dP(Y >= k) = -0.11096910`. So "average the cumulative effects" and "the mean-score effect" are the same measure, up to the constant K-1.

And the structural fact behind all of it: **under proportional odds exactly one measure is cut-invariant, and it is the odds ratio.** That is what the PO assumption *says*. Risk differences and risk ratios necessarily vary from cut to cut, which is exactly why they need K columns. Any one-column alternative must therefore either be the odds ratio, or be a functional of the *whole* predicted distribution that never picks a cut at all.

---

## 2. The candidate space

| Candidate | One column | Interpretable | Marginal | Verdict |
|---|---|---|---|---|
| Cumulative OR (status quo default) | ✓ | ✗ | ✗ (conditional) | keep as the `coefficient` route |
| Per-level marginal RR / RD | ✗ (K cols) | ✓ | ✓ | the problem being solved |
| Average of per-level effects | ✓ | — | — | ✗ identically zero |
| Mean assigned score, difference | ✓ | partly | ✓ | ✗ needs numeric scores, arbitrary |
| Mean assigned score, ratio | ✓ | ✗ | ✓ | ✗ scores have no true zero |
| Latent standardised effect `b*sqrt(3)/pi` | ✓ | partly | ✗ | ✗ assumes a latent logistic; not marginal |
| One chosen cut, e.g. `P(Y >= top)` | ✓ | ✓ | ✓ | fallback; discards the ordering |
| **Ordinal superiority pair (win, loss)** | ✓ | ✓ | ✓ | **recommended** |

---

## 3. The recommendation — the superiority pair

Take two people, one from the group, one from the reference group, and ask *who ends up higher on the scale*. Three probabilities exhaust the answer:

```text
win  = P(Y_group >  Y_reference)
loss = P(Y_group <  Y_reference)
tie  = P(Y_group == Y_reference)          win + loss + tie = 1
```

From that one pair, three readings — the same difference/ratio duality the package already applies to a level and its reference:

| Reading | Formula | Neutral | Literature name |
|---|---|---|---|
| the probability | `gamma = win + tie/2` | `0.5` | ordinal superiority · probability of superiority · AUC |
| the difference | `delta = win - loss = 2*gamma - 1` | `0` | Somers' D · Cliff's delta |
| the ratio | `WR = win / loss` | `1` | win ratio · Agresti's generalised odds ratio (alpha) |

`gamma` is the natural `{base}` (a percentage, `50 %` on the reference row), `delta` the natural `{est}` under `measure = "difference"`, `WR` the natural `{est}` under `measure = "ratio"`. `delta = 2*gamma - 1` is an identity, not an approximation (`2(win + tie/2) - 1 = 2*win + tie - 1 = win - loss`).

The sentence a reader gets is: *"all else equal, a Black respondent out-earns a White respondent 47 times out of 100 instead of 50"*. No odds, no log, no cutpoint, no latent variable.

### 3.1 It is the exact K-category generalisation of what tabxplor already prints

At **K = 2** the pair collapses onto the binomial family's own measures:

- `delta = p1(1-p0) - p0(1-p1) = p1 - p0` — **the risk difference, exactly**. Measured on a dichotomised `rincome` with the same four predictors: `win - loss = -0.02593767`, marginal risk difference `= -0.02593767`.
- `WR = p1(1-p0) / (p0(1-p1))` — **the odds ratio, exactly** (identically, when the two probabilities are constant; with covariates it becomes the average-of-products analogue, measured `0.8681` against a marginal OR of `0.8730`).

So the proposal is not a new concept bolted onto the regression subsystem. It is the concept the package already teaches on a two-level outcome, written for K levels. Verified against `Hmisc::somers2` on the raw data: crude `gamma = 0.438941` equals its `C`, and `2*gamma - 1 = -0.122118` equals its `Dxy`, to every printed digit.

### 3.2 The marginal (unmatched) definition is the right one, and it is collapsible

There are two ways to draw the pair, and the choice is consequential.

- **matched / conditional** (Agresti & Kateri 2017): the two people share the same covariates. Under a probit link this is exactly `Phi(beta/sqrt(2))`, a clean function of `beta` alone.
- **marginal / unmatched**: the two people are drawn independently from the population, each forced to their own level of the predictor, covariates drawn independently.

The marginal one wins on three grounds, one of which is decisive.

**(a) It is collapsible.** Simulated PO data, `K = 5`, `n = 100 000`, `beta_x = 0.8`, with `Z` **independent of** `X` — so there is no confounding and an honest crude/adjusted comparison must show *no movement*:

| `beta_z` | crude gamma | adjusted gamma, **marginal** | adjusted gamma, matched | crude cumOR | adjusted cumOR |
|---|---|---|---|---|---|
| 0 | 0.6216 | 0.6216 (`-0.0000`) | 0.6216 (`-0.0000`) | 2.18 | 2.18 |
| 1 | 0.6023 | 0.6024 (`+0.0001`) | 0.6150 (`+0.0126`) | 1.92 | 2.17 |
| 2 | 0.5750 | 0.5761 (`+0.0012`) | 0.5990 (`+0.0240`) | 1.62 | 2.21 |
| 3 | 0.5583 | 0.5572 (`-0.0010`) | 0.5819 (`+0.0236`) | 1.47 | 2.24 |

The odds ratio moves 1.47 → 2.24 (+52 %) with **zero** confounding — textbook non-collapsibility, and the reason `CLAUDE.md` records that a `color = "adjustment"` gap is coloured but never tested on an OR column. The **marginal** superiority measure does not move at all. The matched one does, mildly.

With genuine confounding (`X` drawn as `plogis(z)`), the marginal measure moves and moves in the right direction: crude `0.7357` → adjusted `0.5763` at `beta_z = 2`.

That makes the crude/adjusted distance on `gamma` a **pure confounding signal**, so `color = "adjustment"` on an ordinal column would become a genuine test rather than a description — the one thing the package cannot offer on the odds-ratio scale.

**(b) Its crude twin is the same formula.** The marginal `gamma` is obtained by applying the plain two-distribution formula to the two **standardised (g-computed) marginal distributions**; the observed column applies the same formula to the two **row percentages of the cross-table**. One formula, two inputs — which is exactly the package's stated identity for the observed companion (*"the observed column is not a summary of the cross-table, it IS the cross-table"*).

**(c) It costs nothing.** The pairwise double sum factorises exactly:

```text
gamma_marginal = f( colMeans(P_group), colMeans(P_reference) )
```

Verified numerically: brute-force pairwise mean over 20x20 distributions `= 0.4943717064`; the `colMeans` shortcut `= 0.4943717064`. Those two column-mean vectors are precisely the quantities `tab_reg()` already averages to build the current K marginal-RR columns. **The one-column measure is pure post-processing of numbers the package already has.**

### 3.3 It survives violation of proportional odds

Measured on the `gss` model. `brant::brant()` **rejects** proportional odds (omnibus `X2 = 65.21`, `df = 28`, `p = 8.5e-05`; `maritalDivorced p = 0.001`, `relig5 p = 0.002`, `raceBlack p = 0.02`). Yet `gamma` computed from the PO fit and `gamma` computed from an unconstrained `nnet::multinom()` on the same variables agree to **at most 0.0033** across all 13 predictor levels:

```text
race Black        0.4736 / 0.4721      marital Widowed   0.3461 / 0.3494
race Other        0.4512 / 0.4517      relig 4-Jewish    0.5911 / 0.5882
marital Separated 0.4040 / 0.4016      relig 8-None      0.5167 / 0.5151
```

This reproduces Harrell's result that the concordance probability is essentially insensitive to PO violation while the per-cut quantities are not. The one-column summary is therefore *more* robust than the four columns it replaces, not less.

### 3.4 It barely depends on the number of categories

A recurring worry about tie-based measures is that ties inflate with few categories and deflate the measure. Measured, with a uniform marginal distribution and equally spaced cutpoints:

| cumOR | K=2 | K=3 | K=4 | K=5 | K=7 | K=10 | K=20 | latent limit |
|---|---|---|---|---|---|---|---|---|
| gamma, OR 1.5 | 0.5500 | 0.5595 | 0.5629 | 0.5644 | 0.5658 | 0.5665 | 0.5670 | 0.5712 |
| gamma, OR 2 | 0.5833 | 0.6000 | 0.6060 | 0.6087 | 0.6112 | 0.6125 | 0.6134 | 0.6201 |
| gamma, OR 3 | 0.6250 | 0.6524 | 0.6625 | 0.6673 | 0.6715 | 0.6738 | 0.6755 | 0.6850 |
| WR, OR 2 | 2.000 | 1.857 | 1.788 | 1.747 | 1.700 | 1.666 | 1.627 | — |

`gamma` (and hence `delta`) is stable from K = 3 onwards — a `gamma` of 0.61 means much the same thing on a 4-point and on a 10-point scale. The **win ratio is markedly less stable** (2.00 → 1.63) because it conditions on discordant pairs, whose share changes with K. That is the reason to make `delta` the headline reading and to treat `WR` as the secondary one.

The `latent limit` column is `plogis(beta/sqrt(2))`, i.e. Agresti & Kateri's `Phi`-analogue for the logit link: it is the K → infinity limit of the observed-scale measure. Harrell's empirical calibration `logit(c) = 0.6453 * log(OR)` (R2 = 0.9965, MAD 0.0044) lands where it should — between K = 5 and K = 10 (`OR = 2`: Harrell 0.610, K=5 0.6087, K=10 0.6125). **Neither approximation is needed here**: `gamma` is computed exactly from the fitted distributions. They matter only as a sanity check and as documentation of what the literature's closed forms actually approximate. On the `gss` data with K = 4 and a skewed marginal, the two approximations are off by up to `0.054` and `0.041` respectively — a good reason not to use them.

---

## 4. What it looks like on real data

Adjusted marginal superiority for `rincome ~ race + marital + relig + age`, with the crude twin from the cross-table (`obs_`), 95 % CI by delta method, stars from the same CI:

| var | level | n | gamma | 95 % CI | | delta | WR | cumOR | obs_gamma | obs_delta |
|---|---|---|---|---|---|---|---|---|---|---|
| race | Black | 1 860 | 0.474 | 0.461 – 0.487 | \*\*\* | -0.052 | 0.846 | 0.819 | 0.439 | -0.122 |
| race | Other | 1 252 | 0.452 | 0.436 – 0.468 | \*\*\* | -0.096 | 0.738 | 0.695 | 0.444 | -0.111 |
| marital | Separated | 446 | 0.405 | 0.380 – 0.430 | \*\*\* | -0.191 | 0.543 | 0.490 | 0.387 | -0.227 |
| marital | Divorced | 2 152 | 0.486 | 0.473 – 0.498 | \*\* | -0.029 | 0.905 | 0.891 | 0.491 | -0.018 |
| marital | Widowed | 401 | 0.347 | 0.320 – 0.374 | \*\*\* | -0.305 | 0.387 | 0.326 | 0.388 | -0.225 |
| marital | Never married | 3 644 | 0.419 | 0.408 – 0.431 | \*\*\* | -0.162 | 0.592 | 0.543 | 0.388 | -0.224 |
| relig | 2-Catholic | 3 121 | 0.521 | 0.510 – 0.533 | \*\*\* | +0.043 | 1.149 | 1.181 | 0.511 | +0.023 |
| relig | 4-Jewish | 222 | 0.590 | 0.559 – 0.621 | \*\*\* | +0.179 | 1.933 | 2.166 | 0.597 | +0.193 |
| relig | 5-Buddhist/Hind. | 143 | 0.607 | 0.572 – 0.641 | \*\*\* | +0.213 | 2.259 | 2.585 | 0.580 | +0.161 |
| relig | 6-Muslim | 56 | 0.530 | 0.464 – 0.595 | | +0.059 | 1.215 | 1.261 | 0.501 | +0.003 |
| relig | 8-None | 2 374 | 0.516 | 0.504 – 0.528 | \*\*\* | +0.033 | 1.112 | 1.135 | 0.493 | -0.014 |
| age | +1 SD (13.5 yr) | 12 958 | 0.532 | 0.527 – 0.536 | \*\*\* | +0.064 | 1.242 | 1.294 | — | — |

Read across, the crude/adjusted pair now tells its story in two numbers rather than eight, and every one of the five things adjustment can do is visible in one column:

- **shrinks** — `race Black` 0.439 → 0.474: a third of the raw income gap is composition (marital status, religion, age).
- **grows** — `marital Widowed` 0.388 → 0.347: age was *masking* the widowhood penalty.
- **reverses** — `relig 8-None` 0.493 (below the neutral 0.5) → 0.516\*\*\* (above it).
- **holds** — `relig 4-Jewish` 0.597 → 0.590.
- **vanishes** — `relig 6-Muslim` moves but the CI still covers 0.5.

The significance pattern is identical to the four-column version's (`Muslim`, `Other christian` and `Other` unstarred, `Divorced` at `**`, the rest at `***`), which is what one wants: **the same model, the same test, one column instead of four.**

---

## 5. Inference

### 5.1 The estimate's interval — map the endpoints, do not use the delta method

`gamma_marginal` is a strictly increasing function of `beta_j` with every other parameter held at its estimate, and `gamma = 0.5` **exactly** at `beta_j = 0` (at `beta_j = 0` the two forced distributions are identical row by row, so the two standardised distributions are identical and the pair is a coin flip). Measured monotonicity, `raceBlack`:

```text
beta   -1.5    -1.0    -0.5     0.0     0.5     1.0     1.5
gamma  0.297   0.364   0.433   0.500   0.558   0.605   0.640
```

That is not luck: raising `beta_j` shifts every person's predicted distribution up in the first-order-stochastic-dominance sense, hence shifts the standardised distribution up, hence raises `gamma`.

So the interval should be built by **pushing the Wald interval of `beta_j` through the functional** — two extra evaluations per cell. It costs almost nothing, and it *guarantees* the invariant the package requires: the interval excludes the neutral value exactly when the model's own test rejects, so colour, greying and stars cannot disagree. Agresti & Kateri recommend the same construction.

The delta method (numeric gradient over `c(beta, zeta)`, `V = vcov(m)`) is the alternative; measured, the two agree closely:

| term | gamma | delta-method CI | endpoint-mapped CI |
|---|---|---|---|
| raceBlack | 0.4741 | 0.4613 – 0.4869 | 0.4611 – 0.4868 |
| maritalWidowed | 0.3474 | 0.3204 – 0.3744 | 0.3203 – 0.3750 |
| relig4-Jewish | 0.5897 | 0.5585 – 0.6209 | 0.5565 – 0.6183 |
| maritalDivorced | 0.4857 | 0.4734 – 0.4980 | 0.4731 – 0.4980 |

Worst disagreement `0.003`. Endpoint mapping is both cheaper and safer.

### 5.2 The observed column's interval — closed form, no refit

The crude `gamma` is a function of the two rows of counts already in the cross-table, and its variance has the standard DeLong / placement-value closed form:

```text
placement_i(k) = F_other(k-1) + 0.5 * p_other(k)
Var = var_p1(placement_2) / n1 + var_p2(placement_1) / n2
```

Measured on `race` Black vs White: analytic SE `0.00664`, 2 000-draw multinomial bootstrap SE `0.00650`. So the observed companion needs **no refit at all** — it belongs on the closed-form branch of `REG_EMPIRICAL`, beside the other saturated-model cases.

### 5.3 The gap SE, for `color = "adjustment"`

Both estimators run on the same rows, so only the difference of their influence functions carries the covariance — the pattern `reg-influence.R` already implements. The two pieces:

- crude: the placement-value residual above (an ordinary two-sample U-statistic influence function).
- adjusted: `grad(gamma)' %*% IF_theta(i)`, where `IF_theta(i) = V %*% score_i` is the per-observation influence of the polr fit.

`MASS::polr` does not expose per-observation scores, so this needs either a small analytic score for the cumulative logit likelihood or a `numDeriv` pass over the per-observation log-likelihood. **This is the one genuine implementation cost of the proposal**; everything else is arithmetic on numbers the package already has.

Because the measure is collapsible (§3.2), this gap SE would be a real test, and `color = "adjustment"` on an ordinal column would not need the "coloured but never tested" restriction that the odds-ratio columns carry.

### 5.4 Weights and survey designs

`gamma_marginal` is a smooth function of two weighted means of per-person quantities, so sampling weights enter by weighting the column means, and a design-based variance goes through the influence functions and `survey::svyrecvar` exactly as the existing basis machinery does. Nothing special is required.

### 5.5 Cost

Measured on the 12 958-row model:

| operation | time |
|---|---|
| one `predict.polr(type = "probs")` pass | 110 ms |
| one vectorised probability pass (matrix form, no `predict`) | 2.4 ms |
| whole table (13 levels), full numeric gradients, delta method | 1.45 s |
| whole table, endpoint mapping (2 evaluations per cell) | < 0.1 s |

The g-computation passes are the same ones the current `effect = "marginal"` route already performs. With endpoint mapping the one-column route is **cheaper** than the four-column one it replaces.

---

## 6. How it is built

Implemented in Phase 22c-vi. This section states the current design; §§1–5 are the measurements it rests on.

### 6.1 The architectural statement

An ordinal outcome's **level kind is a rank** (`REG_FAMILIES$ordinal$level = "rank"`), not `"pct"`. That one word is what makes the column count 1: a rank is compared by pairs of people rather than by shares, so its measures read the whole predicted distribution and answer once. `REG_LEVEL_MEASURES` gains one entry, and order is load-bearing — the first is the level's own measure, which `auto` falls back to on a prediction route:

```r
rank = list(difference = c(scale = "points",     word = "D"),
            ratio      = c(scale = "pct_ratio",  word = "WR"),
            odds_ratio = c(scale = "odds_ratio", word = "OR"))
```

⚠ **A level kind is not a `var_kind`.** The first three (`pct` / `mean` / `count`) happen to share `EST_SCALES`' words; `rank` does not. It asks *which measures exist*, while a `var_kind` says how a number formats and colours — and there is deliberately **no fourth `var_kind`**, because five places answer "percentage" for an unknown one instead of erroring.

The library then composes 5 ordinal rows instead of 10: conditional `cumOR` + `log(cumOR)`, marginal `mD`, `mWR` + `log(mWR)`. `at_reference` is refused on a rank by one clause in `reg_compose_row()` — a pair drawn at one profile is the *matched* comparison, a different and non-collapsible estimand (§3.2).

### 6.2 Where each fact lives

| Table | What it holds |
|---|---|
| `REG_FAMILIES$ordinal` | `level = "rank"`, and the `words` override that keeps the coefficient `cumOR` |
| `REG_LEVEL_MEASURES$rank` | the three cells above — the whole measure surface |
| `REG_ESTIMANDS` | composed: `conditional` → `cumOR`; `marginal` → `D` / `WR`; no `at_reference` row |
| `EST_SCALES` | **untouched** — `points` and `pct_ratio` already declare the right geometry |
| `COLOR_SCALES` | **untouched** — see §6.3 |
| `REG_WORDS` | `D` ("Somers' D") and `WR` ("win ratio"), both `noncollapsible = FALSE` |
| `REG_MEASURE_ALIASES` | `D = "difference"`, `WR = "ratio"` (the FK: a header word is a typable measure) |
| `REG_EMPIRICAL$ordinal` | `somers_d` / `win_ratio` / `win_ratio_log`, closed form; `cumor` declares `refit` |
| `REG_LINK_FUNS` | untouched — the measure is a functional of the distribution, not of a link |

Two ad-hoc predicates became declared facts on the way: the estimand row carries **`per_level`** (how many columns it needs), replacing the family-name test `reg_fam_percategory()`; and a crude shape carries **`refit`** (it has no closed form), so `reg_crude_saturated()` names no family.

### 6.3 The colour ladder needs no calibration

`points` and `pct_ratio` are already right. `CLAUDE.md` requires every ladder to be *the same ladder written in another measure at a 50 % reference cell*, and `pct_diff`'s declared anchor is *"5 points at a 50 % reference"* — which is literally what `D` measures. The mapping is near-invariant in K:

| package rung | ×1.1 | ×1.2 | ×1.5 | ×2 | ×4 |
|---|---|---|---|---|---|
| `D`, K=4 | 0.030 | 0.057 | 0.126 | 0.212 | 0.397 |
| `D`, K=5 | 0.030 | 0.058 | 0.129 | 0.217 | 0.410 |
| `D`, K=7 | 0.031 | 0.059 | 0.132 | 0.222 | 0.422 |
| `WR`, K=4 | 1.083 | 1.164 | 1.403 | 1.788 | 3.252 |

It also sits close to the conventional Somers' D reading guide (negligible < 0.10, weak 0.10–0.30, moderate 0.30–0.50, strong > 0.50). ⚠ The `WR` ladder is only approximate, since the win ratio drifts with K — one more reason `D` leads the list.

### 6.4 One estimator, two callers

`reg_rank_pair(p1, p0, link)` is the whole arithmetic: two distributions in, both readings out (`est` on the asked link, `alt` on the other), plus the gradient of `est` in each distribution. It runs on the model column's two **standardised** distributions and on its crude twin's two **counted** ones — one formula, two inputs, which is what makes the distance between the columns mean adjustment and nothing else.

- `reg_gcomp_rank_maker()` pushes the gradients through the fit's own per-category jacobians (`reg_prob_engine()$dmean`). Answering once, it is a **drop-in for `reg_gcomp_maker()`**, so nothing downstream branches.
- `reg_rank_se()` pushes them through the multinomial covariance of two independent samples. At `est = gamma` the gradients are the placement values, so this **is** DeLong's variance, arrived at rather than special-cased.
- `reg_crude_rank_if_maker()` reads the same gradient as a per-observation score, for the gap SE.

`mean1 = gamma`, `mean0 = 1/2`, so `{base}` prints the probability of superiority and the reference row's own base is a coin flip, exactly. The *other* reading rides in `alt`, because both readings are primitives of the pair rather than derivations from `(gamma, 1/2)` — `reg_fill_base()` prefers it over its own geometry rule.

### 6.5 The Constant row, and the footer

The Constant **value cell is void** on a rank column. The model's predicted outcome distribution is a fact about the outcome, not the level these effects move away from — that one is a coin flip, and the reference row already prints it. Instead `reg_outcome_scale()` stores the ordered levels and their shares, and the footer prints the scale:

```text
Model: ordinal logistic regression; mD = marginal Somers' D (how often someone from this group ends
up higher than someone from the reference group, as a difference in percentage points, wins minus
losses; sample-averaged); each cell shows the effect vs the reference level and, in parentheses, the
probability of superiority itself, 50 % being a coin flip.
rincome, from low to high: 1-Lt $10000 (17%) < 2-$10000 to 14999 (9%) < ... < 4-$25000 or more (57%).
```

That second line is necessary rather than decorative: a one-column table shows the outcome's name and none of its categories, so nothing else says what "higher" means.

### 6.6 What `measure = "ratio"` does

It gives the **win ratio**, the multiplicative reading of the same pair — so the two measure words behave on a rank exactly as they do on a level and its reference. One caveat to know: at K = 2 the superiority ratio is the *odds* ratio, not the risk ratio, so the ordinal `ratio` column is not the continuation of the binomial `ratio` column. A 2-level outcome is never `family = "ordinal"`, so the two never meet in one table.

The per-level breakdown is **not** reachable from an ordinal fit any more. Wanting a number per outcome category is a question the ordering does not help with, and `family = "multinomial"` answers it without assuming proportional odds — which is strictly better whenever that assumption is in doubt (§3.3, and the measured comparison in the roadmap's Phase 22c-vi summary).

---

## 7. Caveats

- **`gamma` is a rank measure and cannot be turned back into a quantity of income.** It answers *how often*, not *how much*. When the outcome's categories have real numeric content (income bands with known midpoints), a reader will also want the mean-score reading; that would be a separate, opt-in measure requiring user-supplied scores, and it is the honest home for a "ratio" with a true zero.
- **Its ceiling depends on the marginal spread.** `delta` cannot exceed `1 - tie`. On the `gss` example, 57 % of respondents sit in the top band, ties run at 0.32–0.46, and `delta` never exceeds 0.31 even where the cumulative OR is 0.33. An outcome piled into one category compresses the measure. This is a property of the data, not a defect, but it must be said in the legend, and it is why `gamma` (bounded 0–1, anchored at 50 %) reads better than `delta` for a lay audience.
- **The win ratio is non-transitive and K-dependent.** A > B and B > C does not guarantee A > C. The literature on hierarchical composite endpoints documents this at length. It is harmless in a table where every level is compared to one common reference — which is exactly tabxplor's layout — but it forbids reading the table as a ranking of the levels among themselves.
- **`gamma` is not a causal quantity by itself.** Like every g-computed contrast, it inherits whatever identification the model's covariate set provides, no more.
- **The matched and marginal versions are different estimands, not two spellings.** Anything comparing tabxplor's output to Agresti & Kateri's published `Phi(beta/sqrt(2))` will find a gap, and the gap is the point (§3.2). This must be stated once in the documentation.
- **Proportional odds still needs checking.** §3.3 shows the *summary* is robust to PO violation, not that the model is correct. When PO fails badly the per-level shape may be a real finding, and the one-column summary will hide it. `REG_CHECKS` should keep a proportionality check for this family, with "look at the per-level columns" as its cure.

---

## 8. Prior art

Nothing in the R ecosystem does this **from a fitted, covariate-adjusted ordinal model**:

- `marginaleffects` — per-level marginal effects for `polr`; no distribution-level functional.
- `rms::orm` / `Hmisc::somers2` — report `Dxy` / `c` for the *model as a whole* (discrimination), not per predictor level.
- `effectsize::p_superiority`, `orddom`, `RProbSup` — data-level, two-sample, unadjusted.
- `genodds`, `WINS`, `hce` — the generalised odds ratio / win statistics, two-sample or trial-oriented, with stratification rather than regression adjustment.
- `pim` (Thas et al., probabilistic index models) — models `P(Y < Y*)` directly with covariates; the closest thing, but a separate model class rather than a reading of a model the user already fitted.

The gap tabxplor would fill: **the adjusted superiority measure as a column of an ordinary regression table, beside its own crude twin.** That is a genuinely novel offering, and it is a natural one for this package precisely because the crude twin comes free from the cross-table.

---

## 9. What the tests pin

`tests/testthat/test-reg-rank.R`, plus the invariants `test-fact-keys.R` / `test-reg-estimand.R` / `test-reg-invariants.R` already assert about the library:

- `D == 2*gamma - 1 == win - loss`, and `gamma == 0.5` exactly when the two distributions are equal
- on two categories, `D` IS the risk difference and `WR` IS the odds ratio, to machine precision
- both analytic gradients against numeric differentiation (3e-10 on both links) — the one thing that fails silently
- the crude closed-form SE against a multinomial bootstrap, and the gap SE against a paired one
- the collapsibility simulation of §3.2: crude and adjusted agree where the covariate is independent of the exposure, while the cumulative OR moves
- one column and not K; `{base}` a probability of superiority; the reference row a coin flip; the footer naming the scale
- a weighted (`svyolr`) ordinal model reads on its rank measures, while a weighted multinomial one still cannot

---

## References

- Agresti, A. & Kateri, M. (2017). Ordinal probability effect measures for group comparisons in multinomial cumulative link models. *Biometrics* 73(1), 214–219.
- Agresti, A. & Tarantola, C. (2018). Simple ways to interpret effects in modeling ordinal categorical data. *Statistica Neerlandica* 72(3), 210–223.
- Agresti, A. (2010). *Analysis of Ordinal Categorical Data*, 2nd ed. Wiley.
- Harrell, F. E. Equivalence of Wilcoxon statistic and proportional odds model — <https://www.fharrell.com/post/powilcoxon/> (`logit(c) = 0.6453 log(OR)`, R2 = 0.9965, MAD 0.0044).
- Harrell, F. E. Violation of proportional odds is not fatal — <https://www.fharrell.com/post/po/>.
- Thas, O., De Neve, J., Clement, L. & Ottoy, J.-P. (2012). Probabilistic index models. *JRSS-B* 74(4), 623–671.
- Pocock, S. J. et al. (2012). The win ratio: a new approach to the analysis of composite endpoints. *European Heart Journal* 33(2), 176–182.
- Wang, D. & Pocock, S. (2016) and follow-ups on win-ratio ties; and the interpretational-challenges literature on non-transitivity and non-collapsibility of the win ratio (arXiv:2504.05909, arXiv:2309.01791).
- Mood, C. (2010). Logistic regression: why we cannot do what we think we can do. *European Sociological Review* 26(1), 67–82 — the non-collapsibility argument the package already relies on.
- McGraw, K. O. & Wong, S. P. (1992). A common language effect size statistic. *Psychological Bulletin* 111(2), 361–365.
