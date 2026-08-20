# Regression framework — mathematical review and edge-case stress test

## 1. What this document is

A deliberately adversarial review of the **inference layer of `tab_reg()`**: the closed forms behind the
`empirical = TRUE` crude column, the influence-function standard error behind `color = "adjustment"`, the
analytic g-computation that replaces `marginaleffects`, and the degrees-of-freedom plumbing that ties them
together.

The question it answers is not "do the parity tests pass" — they do — but the one behind it: **are the parity
tests pitched on the asymptotic stretch where every formula agrees?** Every claim below was reproduced against
the running package before being written down, and each one names the line that causes it.

### 1.1 Method

- **Snapshot.** The working tree at commit `78aa243` plus its uncommitted changes was copied to a scratch
  directory and loaded with `devtools::load_all()`, so a parallel editing session could not move the ground
  under a measurement. No package file was modified by this review.
- **Ground truths used.** `glm` / `lm` / `svyglm` / `MASS::polr` / `nnet::multinom` coefficient tables and
  `confint()`; `marginaleffects::avg_comparisons()`; `survey::svytable` / `svydesign` / `degf`; hand-computed
  Woolf, Katz, Wald and Newcombe intervals from the 2x2 tables; a non-parametric bootstrap; and a
  **Monte-Carlo sampling study** (400 replicates per cell) for quantities with no closed-form truth.
- **Stressors applied.** n from 21 000 down to 40; zero cells; complete and quasi-complete separation; a level
  with n = 1; an all-zero count group; negative and near-zero group means; 40 % missingness on a confounder
  only; cluster designs with 20-40 PSUs, including one built so the two compared groups' proportions are
  *negatively* correlated within PSU; empty interaction cells; and a systematic sweep of all 55 reachable
  `family x effect x measure` combinations.

### 1.2 How to read the severity column

| Level | Meaning |
|---|---|
| **A** | A printed number is wrong, or a printed interval does not belong to the printed estimate. |
| **B** | The number is right but the inference attached to it can be materially wrong (wrong width, wrong p). |
| **C** | Correct but fragile, or inconsistent across the package; a future change can turn it into A or B. |

### 1.3 Summary of findings

| # | Severity | Area | One line |
|---|---|---|---|
| A1 | **A** | multinomial crude | The estimate falls outside its own confidence interval. |
| A2 | **A** | summed score, `log_risk` | `Obs_log(RR)` prints log(OR) with a Woolf interval. |
| A3 | **A** | `marginal` x `log_*` | The model column is stamped `log_coef` but holds un-logged ratios. |
| B1 | **B** | survey designs | The crude interval is an `n_eff` plug-in, not the design-based variance; measured 28 % too narrow. |
| B2 | **B** | separation | A ~1e8 estimate is printed, unflagged, with p = 0.99 on one link and p < 0.001 on another. |
| B3 | **B** | crude risk difference | Plain Wald where `tab()` uses Newcombe for the same quantity; changes stars at small n. |
| C1 | **C** | degrees of freedom | Up to three different reference distributions inside one table. |
| C2 | **C** | `between_groups` gap | The recovered SE is inflated by t/z of the smaller group (+31 % at df = 5). |
| C3 | **C** | gap interval | A magnitude interval rendered as a signed one; a pinned null bound prints as `-0.0`. |
| C4 | **C** | estimand guard | `reg_same_estimand()` cannot see a mismatch between two `log_coef` columns. |

Section 8 lists, equally explicitly, **what was tested and found sound** — including the pieces most likely to
be wrong a priori, which are not.

---

## 2. A1 — the multinomial crude column reports an estimate outside its own interval

**Severity A. Confirmed. Every multinomial table with `empirical = "column"`.**

### 2.1 The observation

```r
d <- gss_cat_data_formatting() |>
  dplyr::filter(!is.na(party3), !is.na(race), !is.na(age))
tab_reg(d, outcome = "party3", predictors = c("race", "age"),
        family = "multinomial", empirical = "column")
```

| column | level | estimate | ci_inf | ci_sup | p |
|---|---|---:|---:|---:|---:|
| `Obs_2-Independent, other vs 1-Democrat` | Black | **0.416391** | 0.691391 | 0.848289 | 3.16e-07 |
| `Obs_2-Independent, other vs 1-Democrat` | Other | **1.272497** | 1.688638 | 2.072795 | 4.56e-33 |
| `Obs_3-Republican vs 1-Democrat` | Black | **0.098253** | 0.107269 | 0.140990 | 1.95e-198 |
| `Obs_3-Republican vs 1-Democrat` | Other | **0.367911** | 0.298493 | 0.378978 | 1.37e-71 |

In three of the four cells the point estimate lies **outside the bracket printed beside it**.

### 2.2 What each number is

Computed by hand from `table(race, party3)`:

| quantity | Black, "2-Independent" |
|---|---|
| odds vs the **reference category** (`1-Democrat`) — mlogit's estimand | 0.41639, CI [0.3751 ; 0.4622] |
| odds vs **everything else** (the collapsed 2x2) | 0.76583, CI [0.6914 ; 0.8483] |

The **estimate** is the first (correct: it matches what the model column estimates, and it is what `obs`
carries for the adjustment comparison). The **interval and the p-value** are the second. They are two
different estimands, printed as one cell.

### 2.3 Root cause

`R/reg-empirical.R`, `prob_effect()`, the Woolf arm:

```r
v  <- g$emp_ratio
ci <- na_ref(ci_or(prop * ndr, (1 - prop) * ndr, rprop * rndr, (1 - rprop) * rndr,
                   conf_level = conf_level, want_p = TRUE, df = degf))
```

`g$emp_ratio` is built from `wpos / wneg` where `wneg` is the **reference category's** weighted count
(`R/reg-empirical.R:278-280`), while `ci_or()` is fed `(1 - prop) * ndr`, i.e. **everything that is not this
category**. The comment above the call — *"WEIGHTED proportion x UNWEIGHTED base, so the base cancels out of
the log-OR"* — is true for a **binary** outcome, where "the other category" and "everything else" are the same
set. With three or more categories they are not, and nothing downstream notices.

### 2.4 Blast radius

- `empirical = "column"` on any 3+ level nominal outcome: the visible defect above.
- `empirical = TRUE` (the default, which folds the crude value into the model cell): **not affected** — only
  `obs` is used there, and `obs` carries the correct vs-reference-category value. The comparison and the
  colour are sound; it is the standalone crude column's bracket and star that are wrong.
- The same arm is reached by `measure = "log"` (`or_log`), which logs both, preserving the mismatch.

### 2.5 Suggested fix

Feed `ci_or()` the reference category's counts, not the complement:
`ci_or(prop * ndr, refcat_prop * ndr, rprop * rndr, refcat_rprop * rndr)`. The four counts are already on the
grid. A regression test should assert `ci_inf <= est <= ci_sup` on every crude cell of every family — see
section 9.

---

## 3. A2 — `Obs_log(RR)` on a summed score is log(OR) with a Woolf interval

**Severity A. Confirmed. `trials =` + `measure = "log_risk"`.**

### 3.1 The observation

With the `FactoMineR::tea` six-item battery (`trials = 6`), predictor `sex`, reference `F`:

| what | value | CI | p |
|---|---|---|---|
| printed `Obs_log(RR)`, level M | **-0.225087** | [-0.454673 ; 0.004499] | 0.054662 |
| hand log(RR) | -0.175970 | [-0.356500 ; 0.004600] | 0.056079 |
| hand log(OR) | **-0.225087** | [-0.454673 ; 0.004499] | 0.054662 |

The printed column is log(OR) to the last digit, including its p-value. Two further confirmations: the
`Obs_log(OR)` column obtained with `measure = "log_odds"` is **numerically identical**, and the stored
`ci_method` attribute reads `"katz"` while the arithmetic that ran is Woolf's.

The model column beside it is correct and different: `Model_log(RR)` = -0.256958 against
`Model_log(OR)` = -0.330557. So the pair shown to the reader is *model log-RR* next to *observed log-OR*.

### 3.2 Root cause

`R/reg-empirical.R`, `prob_effect()`:

```r
logged  <- identical(sh$scale, "log_coef")
base_sh <- if (logged) fam[[fam$coef]] else sh   # the exponentiated twin a logged shape logs
```

For a logged shape the arm is re-derived from the family's **coefficient** shape rather than from the shape's
own declaration. `REG_EMPIRICAL$grouped_binomial` declares `coef = "or"`, so `rr_log` — which correctly
declares `ci_method = "katz"`, `link = "log"` — is dispatched through `fam$or` and takes the Woolf branch.

`REG_EMPIRICAL$binomial` is unaffected because a binomial `log_risk` resolves `crude_fam = "rr"`, and
`REG_EMPIRICAL$rr` declares `coef = "rr"`, so the fallback lands on the right shape by luck of the
declaration. **Plain binomial `log_risk` was verified correct** against hand Katz values.

### 3.3 Suggested fix

`base_sh` should be the shape's own exponentiated twin, not the family's coefficient shape — e.g. resolve it
from the shape's key by stripping the `_log` suffix (`fam[[sub("_log$", "", sh_key)]]`), falling back to
`fam$coef` only when that key does not exist. That is a two-line change and it makes the declared
`ci_method` / `link` of every `*_log` row load-bearing rather than decorative.

---

## 4. A3 — `effect = "marginal"` / `"at_reference"` with a `log_*` measure leaves the estimate un-logged

**Severity A. Confirmed. Binomial, grouped binomial and multinomial.**

### 4.1 The observation

```r
tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
        effect = "marginal", measure = "log_risk", empirical = "column")
```

`Model_log(mRR)` is stamped `scale = "log_coef"` and contains:

| level | stored estimate | ci_inf | ci_sup | p |
|---|---:|---:|---:|---:|
| Population average | 0.4713412 | 0.464758 | 0.477924 | — |
| White (reference) | **1.0000000** | — | — | — |
| Black | **0.5623027** | 0.530487 | 0.596027 | 1.3e-83 |
| Other | **0.9737260** | 0.927991 | 1.021715 | 0.278 |

These are **risk ratios**, not log risk ratios. Three independent tells:

- the reference row holds **1**, where the neutral of a `log_coef` column is **0**;
- the Constant row holds 0.471, a probability, not a log;
- log(0.5623) = -0.5758 is the value the column claims to print.

Consequences that reach the reader:

- **Star vs interval contradiction.** For `Other`, the stored interval [0.928 ; 1.022] excludes the
  `log_coef` null of 0, so the tooltip's bracket reads "significant", while p = 0.278 prints no star.
- **The colour ladder is meaningless.** `log_coef` grades on an additive SD ladder; applied to ratios
  clustered around 1, every cell collapses to the neutral slot.
- **Multinomial is worse still**: the crude column is stamped `scale = "odds_ratio"` while the model column is
  `log_coef`, so the two halves of the same comparison are not even on the same scale. `reg_same_estimand()`
  correctly refuses the gap, so nothing is coloured — but the columns are printed side by side as a pair.

The same failure appears under `effect = "at_reference"`, and on the grouped-binomial and multinomial
families. The coefficient path is **not** affected: `measure = "log"` / `"log_odds"` / `"log_risk"` with
`effect = "coefficient"` were all verified exactly correct against hand values.

### 4.2 Root cause

`reg_wald_finalize()` (`R/tab_reg.R:823-840`) exponentiates when `do_exp` is TRUE:

```r
if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
```

On the coefficient path a `log_*` estimand sets `do_exp = FALSE`, so the value stays on the link scale and the
`log_coef` stamp is honest. On the marginal path (`reg_marginal_gcomp()`, `R/tab_reg.R:1676`, and
`reg_marginal_me()`, `:1811`/`:1834`) `ratio`/`do_exp` is derived from the *comparison* (`lnratioavg`) rather
than from the estimand's declared `scale`, so the ratio is exponentiated back while the column keeps the
`log_coef` scale the estimand row declares.

### 4.3 Suggested fix

Make the exponentiate decision read the estimand's **stored scale** — `EST_SCALES[[scale]]$mult` is FALSE for
`log_coef` — rather than the internal comparison key, at all three finalisers. A cheap invariant that would
have caught this at build time: **on a `log_coef` column, the reference cell's estimate must be 0**; on a
`mult` column it must be 1. That is one assertion, family-agnostic, and it fires on every case above.

---

## 5. B1 — under a complex design, the crude interval is an `n_eff` plug-in, not the design-based variance

**Severity B. Confirmed, in both directions, with a worked counter-example.**

### 5.1 What the design intends

The 22a-i decision record states the rule as: *"the crude interval is the univariable model's under the
table's own basis"*. Under `basis = "design"` that reading means the design-based univariable fit —
`svyglm(y ~ x, design)`. It is not what happens.

What happens is: the design variance of **each cell proportion** is computed (`survey-variance.R`), converted
to a Korn-Graubard effective sample size `n_eff = p(1-p) / Var_design(p)` (`R/reg-empirical.R:253`), and those
deflated counts are then fed to the ordinary **Woolf / Katz** closed form, whose own variance formula
(`1/a + 1/b + 1/c + 1/d`) assumes the two groups' proportions are **independent**.

Under cluster sampling the two groups usually appear in the *same* PSUs, so they are not independent. The
plug-in therefore reports `Var(logit p1) + Var(logit p0)` where the truth is
`Var(logit p1) + Var(logit p0) - 2 Cov`. **The sign of that covariance decides which way the error goes, and
nothing in the code can see it.**

### 5.2 Measured, both directions

**Case 1 — positively correlated groups (the plug-in is conservative).** 30 PSUs x 40, a cluster intercept
shared by both races (`degf = 29`):

| quantity | value |
|---|---|
| `Obs_OR` interval, implied SE(log OR) | **0.32491** |
| `svyglm(y ~ race, design)` SE(log OR) | 0.14568 |
| naive SRS Woolf SE(log OR) | 0.11252 |
| ratio crude / design-based | **2.23** |

**Case 2 — negatively correlated groups (the plug-in is anticonservative).** 40 PSUs x 60, a cluster shift
applied with opposite sign to the two races, so `cor(p_W, p_B)` across PSUs = **-0.941** (`degf = 39`):

| quantity | crude column | design-based `svyglm` |
|---|---:|---:|
| OR | 0.56693 | 0.56693 |
| CI | **[0.3034 ; 1.0594]** | [0.2390 ; 1.3447] |
| SE(log OR) | **0.30908** | 0.42664 |
| p | **0.0740** | 0.1914 |

The point estimate is exact; the interval is **28 % too narrow**, and the p-value crosses the 10 % threshold —
the cell is starred `*` when the design-based test says 0.19.

Case 2 is not exotic. Negative within-cluster correlation between two subgroups' outcome rates is what you get
whenever the clustering unit has a roughly fixed total to allocate (a school with a fixed number of places, a
firm with a fixed promotion quota, a household budget shared between members), or whenever a cluster-level
factor helps one group and hurts the other.

### 5.3 Two smaller design-basis observations

- The crude MEAN engines take `df_or_design()` (`R/tab-agg.R:628-632`), which **replaces** every cell's own df
  by the single design df. Measured on `age ~ race` with 25 PSUs, the crude Welch interval and `svyglm`'s
  agreed to 1.5 %, so this is behaving well; it is listed only because the override is unconditional.
- The flat closed form at `ids = ~1` (weights but no clusters) reproduces `svyglm(ids = ~1)` to about 0.03 %
  on both the estimate and the interval. **That path is sound**, which is worth stating: the problem is
  specific to a real cluster design.

### 5.4 Suggested fix

Two honest options, in increasing cost:

1. **Compute the crude column from the univariable design-based fit** whenever `basis == "design"`, exactly as
   the numeric-predictor arm already does (it calls `reg_empirical_fit()` and gets `svyglm` for free). The
   saturated closed form then remains the fast path for `basis %in% c("n", "weights")`, where it is provably
   right. This is the option that makes the documented rule true.
2. Keep the plug-in but **state its assumption in the legend** ("design-effect-adjusted Woolf interval;
   assumes the compared groups are independent across clusters") and add a `basis == "design"` note. Cheaper,
   but it leaves a number in the table that can be 28 % narrow with no way for the reader to tell.

Option 1 also removes the df mismatch of C1, since both columns would then come from the same fit family.

---

## 6. B2 — separation and empty cells print a ~1e8 estimate, unflagged

**Severity B. Confirmed on binomial (both links) and Poisson.**

### 6.1 The observation

A perfectly separated level (group B: 0 successes out of 20; group A: 20/40) plus a noise covariate:

```text
2 g   A   40 (50%)      1                1 (51%)
3 g   B   20 ( 0%)      0 1/367 415 925.97 ( 0%)
```

- The crude cell prints **`0`** with no interval and no p-value (Woolf's variance is infinite at a zero cell;
  the engine correctly returns NA rather than inventing a Haldane-Anscombe correction — but the cell is then
  silent about the strongest association in the table).
- The model cell prints a **twelve-digit odds ratio**.
- **No warning and no message is emitted** — verified with `withCallingHandlers()` capturing both. R's own
  `glm` was also silent on this data, so nothing upstream flags it either.
- The Wald p-value is **0.9935**: the textbook Hauck-Donner effect. The cell carries **no star**, while the
  footer of the same table reports `LR vs null <0.01%`. The table therefore says, simultaneously, that the
  model is overwhelmingly significant and that its only non-reference coefficient is not.

### 6.2 The link decides which wrong answer you get

The same data under `measure = "ratio"` (modified Poisson, robust SE):

```text
3 g   B   20 ( 0%)   x0 ÷329 356 539.9*** ( 0%)
```

Here the sandwich SE stays small, so the same non-identified coefficient is reported at **p < 0.001, with
three stars**. The two links give opposite verdicts on an identical, non-identified quantity.

Poisson behaves the same way: an all-zero group yields `1/837 337 493` with p = 0.987, and when the *reference*
group is the all-zero one the crude column produces `Inf` and `NaN` ratios.

In a Monte-Carlo of 400 samples of n = 120 drawn from real GSS data, **2 samples (0.25 %) produced an infinite
adjustment gap** because the crude cell was empty. At n = 60-80 with a rare outcome this is routine, and small
subgroups are exactly where an exploratory package gets used.

### 6.3 Suggested fix

Detect non-identification at the fit and say so. The cheapest reliable test is on the fitted values, not on the
coefficients: for a binomial fit, `any(fitted < 1e-8 | fitted > 1 - 1e-8)`; for Poisson, `any(fitted < 1e-8)`.
On a hit:

- emit one message naming the predictor and level ("`g = B` has no observed successes: its effect is not
  identified"), and
- render the cell as a non-value rather than a number — the display grammar already has `blank`, and
  `set_pvalue(x, NA_real_)` already removes the star, so both halves exist.

A Firth / penalised refit is the statistically complete answer but is a much larger change (`logistf` /
`brglm2` are not currently dependencies); refusing to print a meaningless number is the minimum.

---

## 7. B3, C1-C4 — inference details that change verdicts

### 7.1 B3 — the crude risk difference uses plain Wald where `tab()` uses Newcombe

`CI_METHODS$diff` declares `newcombe, ac, wald`, in that order, and `tab(ci = "ref")` uses Newcombe — the
golden fixture `f_ci_diff` is explicitly labelled "Newcombe diff-interval". Every family in `REG_EMPIRICAL`
declares `method_diff = "wald"`, so the crude column of `tab_reg()` uses the **third** option for the same
statistic.

Measured on n = 200 (White 73/73, Black 9/20):

| method | RD | CI | verdict at 95 % |
|---|---:|---|---|
| Wald (what `Obs_RD` prints) | -0.18966 | [-0.37655 ; **-0.00276**] | p = 0.0467, **starred** |
| Newcombe (what `tab()` prints) | -0.18966 | [-0.34884 ; **+0.00913**] | covers 0, not significant |

The Wald interval for a difference of proportions is the one method the literature agrees should not be the
default — it undercovers at small n and near the boundaries, which is precisely why `ci_prop_diff()` carries
`newcombe` and `ac` arms. Here it flips a star.

Two further consequences: the **tooltip** for the same cell is built with the Newcombe arm
(`R/reg-empirical.R:281-285`, feeding `reg-spec-build.R`), so hover and cell can disagree; and the same
observed contrast gets different intervals depending on which of the package's two producers drew it.

**Fix:** set `method_diff = "newcombe"` (or `"ac"`) in `REG_EMPIRICAL`. It is a one-token change per family and
it aligns the two producers. It moves goldens, so it wants a deliberate review pass.

### 7.2 C1 — the degrees-of-freedom zoo

Measured on one survey table (24 PSUs, 4 strata, `degf(design) = 20`, 3 coefficients):

| column | reference distribution | df |
|---|---|---|
| `Model_OR` (coefficient path) | t | **17** = `degf + 1 - p`, survey's own `df.residual` |
| `Obs_OR` (crude closed form) | t | **20** = `degf(design)` |
| a marginal / AME column of the same fit | **z** | Inf |
| a weighted multinomial or ordinal column | **z** | Inf |
| the stamped `degf` attribute, which the legend prints | — | **20** |

So a single table can refer three ways at once, and the legend's "20 design df" is not the df any model column
used. The gaps are small at 20 df (t(17)/z = 1.076) and negligible at 1000, but they are systematic and they
are largest exactly where survey users work — few PSUs.

`fmt_gap_bounds()` and `fmt_gap_p()` (`R/fmt_class.R:2731`, `:2762`) are z-based by explicit design decision;
the marginal path's `qnorm` at `R/tab_reg.R:1643` and `:1963` is the one that discards an available df.
`marginaleffects` is also z by default, so the AME column is *conventional*; it is only inconsistent with the
coefficient column standing next to it.

**Fix:** thread the column's own `get_degf()` into the marginal finalisers, and print the model column's df in
the legend rather than the stamped design df. If z is kept deliberately, say so in the legend.

### 7.3 C2 — the `between_groups` gap SE is inflated by t/z

`reg_gap_se_of()` (`R/tab_reg.R:2702-2711`) recovers each group's SE by dividing the printed interval's
half-width by **z**, with the stated intent that "the gap test is a z test throughout". But the interval it
reads was built with **t** on a gaussian, quasi or `svyglm` column, so the recovered SE is inflated by exactly
`t(df)/z`.

Measured with a deliberately tiny group (n = 22, df = 19):

| quantity | value |
|---|---|
| true SE from `lm` | 7.5249 |
| SE recovered by `reg_gap_se_of()` | **8.0357** |
| ratio | 1.0679 = `qt(.975, 19) / qnorm(.975)` exactly |

At df = 5 the factor is 1.31. The direction is conservative (the gap test loses power), so it costs
discoveries rather than creating false ones — but the same code is what `forest_plot()`'s gap band reads.

**Fix:** recover the SE with the column's own critical value (`conf_level_to_crit(get_conf_level(x),
get_degf(x))`), then test with z if that is the chosen convention. The two decisions are independent and
currently conflated.

### 7.4 C3 — the gap interval is a magnitude interval rendered as a signed one

`fmt_gap_bounds()` builds the interval of `|gap|` and re-signs it:

```r
lo <- ifelse(ok, p$sign * pmax(0, abs(g) - half), NA_real_)
hi <- ifelse(ok, p$sign * (abs(g) + half)       , NA_real_)
```

This is deliberate and documented (the DESIGN block above it explains the folding), and it is internally
consistent: the near bound is pinned exactly at the neutral **iff** p >= alpha. But the rendering is
ambiguous. A real tooltip from a real table:

```text
gap: -1.7 pts [-4.1 pts; -0.0 pts], p = 16.7%
```

`-0.0 pts` is the null, printed at one decimal. A reader cannot distinguish "pinned at the null, therefore
not significant" from "just barely excludes the null", and the p-value next to it says the opposite of what
the bracket appears to say. On a multiplicative scale the same cell reads `[x1.00; x1.35]`.

The interval also cannot express a **reversal**: it never crosses the neutral, so "the adjustment may have
moved the effect the other way" is inexpressible, even though 22b-iv made reversal a first-class case for the
colour.

**Fix (presentation only, no arithmetic change):** when the near bound is pinned, render it as the null with no
sign — `[0 pts; 4.1 pts]` / `[x1.00; x1.35]` — or append the same "not significant" cue the stars use. The
underlying folding can stay exactly as it is.

### 7.5 C4 — the estimand guard is blind between two `log_coef` columns

`reg_same_estimand()` compares the two columns' `scale`:

```r
identical(as.character(shape$scale)[1], as.character(get_scale(col))[1])
```

Every logged measure — log(OR), log(RR), log(IRR), log(cumOR) — shares the single scale `log_coef`. The guard
therefore cannot distinguish them, which is precisely why A2 slips through: `Obs_log(OR)` is accepted as the
twin of `Model_log(RR)`, and `obs` is populated with a log odds ratio on a log-risk-ratio column
(verified: model `-0.256958`, `obs` `-0.225087`).

**Fix:** compare the declared **word** as well as the scale — `REG_WORDS` already gives every estimand a
unique acronym, and `REG_EMPIRICAL$*$word` already carries it. One extra `identical()` closes the whole class,
including any future logged measure.

---

## 8. What was stress-tested and found sound

This half matters as much as the findings: the pieces most likely to be wrong a priori are right, and several
of my own initial hypotheses were falsified by measurement.

| Area | Test | Result |
|---|---|---|
| Binomial AME, analytic g-computation | vs `marginaleffects`, n = 12 990 / 120 / **40** | ~7 digits at every n: est, CI and p |
| **multinomial** AME (`nnet`) | vs `marginaleffects`, n = full / 600 / **150** | agrees to ~6 digits on all 6 category x level cells |
| **ordinal** AME (`MASS::polr`) | vs `marginaleffects`, n = full / **400** | agrees to ~6 digits on all 8 cells |
| Gaussian coefficient path | vs `lm` + `confint`, n = 60 | exact, t on residual df |
| Binomial coefficient path | vs `glm` + Wald, n = 21 483 / 250 / **60** | exact |
| Survey model column | vs `svyglm` + `confint.svyglm` | exact, including the t on `degf + 1 - p` |
| Crude OR / RR / log(OR) / log(RR), plain binomial | vs hand Woolf and Katz | exact, including the log variants |
| Crude column population | 40 % missing on a confounder only | uses the **model's** complete cases (0.370387, not 0.373555) |
| `multiplier` (k-unit contrasts) | k = 1, `"sd"`, 10, crude and model | exact `exp(k*b)`, `se x |k|`, p invariant; glm, mlogit, polr |
| Weights-only basis (`ids = ~1`) | vs `svyglm(ids = ~1)` | estimate exact, interval within 0.03 % |
| `conf_level` | 0.95 vs 0.99, model / crude / gap | propagates to all three; implied SE identical |
| Model comparison with unequal missingness | two models, one extra predictor 50 % missing | both refitted on the **common** 5 652 rows — correct |
| Interaction fits | combined factor, nested slopes, empty cell | empty cell dropped cleanly; saturated fit = crude column |
| `ci_method = "profile"` | vs `confint.glm` and LR p | CI and p are both LR-based, so the duality holds |
| Non-collapsible OR gap | default binomial OR + `color = "adjustment"` | correctly refused, **and clearly messaged** (quoted below) |
| `at_reference` gap | `obs` unpopulated | correctly refused, and the legend says *"no observed effect"* |

The non-collapsibility message reads, verbatim: *"`color_signif` does not apply to an odds-ratio
"adjustment" gap: part of it is non-collapsibility, not confounding. Use `effect = "marginal"` or
`measure = "ratio"` (risk ratios), for a gap the test can read."* That is exactly the right thing to say,
and it is said without prompting.

### 8.1 The adjustment gap SE — a Monte-Carlo verdict

The influence-function gap SE is the package's most novel closed form and has no external comparator, so it
was checked by simulation rather than by parity. 400 independent samples were drawn from the 12 990-row GSS
population at each size; the "true" gap is the population-level difference between the adjusted and crude
log risk ratios.

| n | level | sd(gap) across replicates | mean reported `gap_se` | ratio | 95 % coverage |
|---:|---|---:|---:|---:|---:|
| 1000 | Black | 0.01077 | 0.01157 | 1.075 | 0.945 |
| 1000 | Other | 0.01766 | 0.01948 | 1.103 | 0.968 |
| 300 | Black | 0.02194 | 0.02237 | 1.019 | 0.915 |
| 300 | Other | 0.03702 | 0.03662 | 0.989 | 0.932 |
| 120 | Black | — | 0.04462 | — | 0.962 |
| 120 | Other | — | 0.06423 | — | 0.954 |

**The gap SE is well calibrated.** Coverage sits near nominal throughout, with mild undercoverage at n = 300
(0.915 and 0.932 against 0.95; Monte-Carlo error is about 0.011). At n = 120 the sampling sd is undefined
because 2 of 400 replicates produced an infinite gap (an empty crude cell — B2).

Two structural caveats stand behind that good result rather than contradicting it, and both bite hardest at
small n:

- **The gap SE is an HC0 sandwich with no small-sample correction.** `reg_if_from_parts()` computes
  `A^-1 (sum U U') A^-1` with no HC1/HC2/HC3 adjustment. HC0 is known to be biased downward, roughly by a
  factor `(n - p)/n`. That is invisible at n = 1000 and is part of the 0.915 at n = 300.
- **The printed interval and the gap test use different variance philosophies.** The printed marginal-effect
  interval is a delta method on the model's own `vcov` (Fisher information, and for `lm` the classical
  sigma^2 (X'X)^-1); the gap SE is always the Huber-White sandwich, and it additionally includes the
  empirical-averaging term that the delta route drops. Under a misspecified model the two can differ
  materially, and one table shows both. This is a deliberate, documented choice; it deserves one legend
  sentence, because a reader comparing the printed CI to the gap CI is comparing two different estimators.

An earlier non-parametric bootstrap at n = 80-150 suggested ratios of 0.70-0.87, but the bootstrap standard
deviation of a difference of coefficients is itself heavy-tailed at those sizes; **the Monte-Carlo above
supersedes it** and is the number to quote.

---

## 9. Recommendations, in priority order

### 9.1 Before release

1. **Fix A1** (multinomial crude interval). A printed estimate outside its printed interval is the kind of
   defect that costs a package its credibility, and it is a four-argument change in one call.
2. **Fix A3** (`marginal` x `log_*` un-logged). It is wrong on three families, and the invariant that catches
   it — *the reference cell of a `log_coef` column is 0, of a `mult` column is 1* — is one assertion.
3. **Fix A2** (`grouped_binomial` `rr_log`). Two lines, and it makes every `*_log` row's declared `link` and
   `ci_method` load-bearing instead of decorative.
4. **Handle separation (B2).** Do not print a twelve-digit ratio. Detect it on the fitted values, message
   once, blank the cell. This is the finding most likely to be met by a real user, because it needs only a
   small subgroup and a rare outcome.
5. **Decide B1.** Either compute the crude column from the univariable design-based fit under
   `basis == "design"` (recommended: it makes the documented rule true and removes the df mismatch), or say in
   the legend that the interval assumes independence across clusters.

### 9.2 Worth doing, lower risk

6. `method_diff = "newcombe"` in `REG_EMPIRICAL` (B3), aligning `tab_reg()` with `tab()`.
7. Thread `get_degf()` into the marginal / Constant finalisers, and print the model's df in the legend (C1).
8. Recover the `between_groups` SE with the column's own critical value (C2).
9. Render a pinned gap bound as the unsigned null (C3).
10. Add the estimand `word` to `reg_same_estimand()` (C4).
11. One legend sentence distinguishing the model-based printed interval from the sandwich-based gap interval
    (section 8.1).

### 9.3 Three regression tests that would have caught most of this

These are cheap, family-agnostic and belong in the suite regardless of which fixes land:

- **Interval contains its estimate.** For every crude and model cell of every reachable
  `family x effect x measure`, assert `ci_inf <= est <= ci_sup` where `est` is read through
  `EST_SCALES[[scale]]$est_field`. Catches A1 and the multinomial half of A3.
- **The reference cell is the scale's neutral.** `est == EST_SCALES[[scale]]$neutral` on every `is_ref` row.
  Catches A3 on every family, at build time.
- **Star and interval agree.** `(p < alpha) == (the interval excludes the scale's neutral)` on every cell that
  has both. Catches A3's contradiction and any future engine mismatch.

The sweep harness used for this review implements all three over the 55-combination grid and completes in a
few minutes; it is a natural addition as a slow/optional test.

---

## 10. Reproduction

Every measurement in this document comes from a standalone script run against a `devtools::load_all()` copy of
the tree at `78aa243` + working changes, with `OMP_NUM_THREADS=1` and `setDTthreads(1L)`. The scripts are
short and self-contained; each section above states the data, the call and the comparator, which is enough to
rebuild them. The heaviest items are the Monte-Carlo of section 8.1 (400 x 3 `tab_reg()` calls, about 0.8 s
each) and the 55-cell grid sweep of section 9.3.

### 10.1 Coverage of this review

Tested: all 6 outcome families; all 55 reachable `effect x measure` combinations; `empirical` in
`TRUE`/`"column"`; `coefficient` / `marginal` / `at_reference`; unweighted, `wt =`, and `svydesign` bases;
factor and numeric predictors; both interaction arms; `tab_vars` grouping; model comparison; `ci_method` wald
and profile; `conf_level` 0.95 and 0.99; `multiplier` 1 / `"sd"` / 10; and n from 21 483 down to 40.

Not tested, and therefore not cleared:

- **Replicate-weight designs** (`svrepdesign`) — refused by the package, so out of scope, but the refusal path
  itself was not exercised.
- **Two-phase designs**, calibrated and PPS designs — the `reg_if_align()` row-space padding for calibrated /
  PPS designs was read but not run.
- **`shape =`** cures other than the default (quadratic, quantiles, log, sqrt) in combination with the crude
  column and the gap.
- The **jamovi digest path**, where `f$fit` is distilled away and several gates change behaviour.
- **Ordinal crude** columns beyond a smoke test: they come from a univariable `polr` refit rather than a
  closed form, so they inherit that fitter's properties, but the refit's population and reference handling
  were not verified line by line.
- Numerical behaviour under **extreme weights** (ratios beyond ~10:1) and under zero or negative weights.
