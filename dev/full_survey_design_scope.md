# Full survey design for `tab()` and for `tab_reg()`'s crude counterparts — scope study

Date: 2026-08-11. Status: **RESEARCH ONLY** — no R code written. Every number below is produced by
**`dev/survey_design_measurements.R`** (run it with `OMP_NUM_THREADS=1 Rscript
dev/survey_design_measurements.R`, ~1 minute), against the working tree at `ae49614` (Phase 18z13),
with `survey` 4.5, `marginaleffects` 0.32.0, R 4.6.1. Block numbers in that script are cited as
*(block N)*.

The question, as asked: *what would be missing for full survey design of `tab()`, and of the crude
(`Obs_*`) counterparts of `tab_reg()` — including a design object as `data`, with calibration — if the
opt-in is exactly "pass a design object"? Would it be simple, or a big change for something not really
useful?*

---

## 0. Executive summary

**The verdict has three parts, and they point in different directions.**

1. **It is NOT "not really useful".** Measured on the shape of table sociologists actually build — a
   crosstab whose row variable is geographically or socially *segregated* across the survey's primary
   sampling units (region, race, urban/rural, school, firm, quartier) — today's intervals and stars are
   **2.4× too narrow**, and `options(tabxplor.kish_neff = TRUE)` only takes that to **2.1×** (§3.2).
   A cell that prints `***` at a nominal p = 0.05 has a real design-based p of about **0.34**. That is
   not a rounding issue; it is the difference between a finding and no finding, in a package whose
   entire proposition is colour-coded significance.

2. **The statistical work is genuinely small**, because Phase 18s already built the seam without
   meaning to. **Every per-cell inference in `tab()` — factor CIs, mean CIs, the `color = "OR"`
   interval, the `contrib` residual, and every star — already reads one field, `n_eff`, as "the
   effective base for this cell's interval"** (§4.1). Replacing the Kish value in that field by a
   design-based one makes all of them design-based with **zero change** to `tab_ci()`, the CI engines,
   the colour engine, the fmt record, the exporters, or jamovi. Measured: Wilson on a design-based
   `n_eff` reproduces `survey`'s own interval **to four decimals** (§4.3). The same lever exists in
   `tab_reg()`'s crude block, where it is called `emp_n_ci` (§5).

3. **What is big is not the statistics but the surface** — and, more urgently, **the design path that
   already ships is broken in ways that produce wrong numbers, not merely narrow ones**. Nine defects
   are documented in §2.3. Three of them are point-estimate defects that exist *today*, independently
   of any new feature:

   - **D1** `tab_reg(design, empirical = TRUE)` computes every crude `Obs_*` column **unweighted**,
     beside a design-weighted `Model_*` column. Measured `Obs_%` 0.353 where the population value is
     0.198, `Obs_OR` 0.514 where it is 0.482 (§2.3). The one comparison the feature exists for is made
     across two different populations.
   - **D2** `tab_reg(design, effect = "ame")` returns the **sample-average** marginal effect, not the
     population-average one: −0.230955 against −0.203627 for the identical fit, a **13 %** error on the
     **model** column (§2.3).
   - **D3** `tab_reg(svrepdesign, …)` and `tab(svrepdesign, …)` both **error out**, on the documented
     "prebuilt design as `data`" path. Both are one-line fixes (§2.3).

   All three have the same root cause on the `tab_reg` side — one missing line that `tab()` already
   has.

**Recommendation.** Split the work. A small **hygiene phase** fixes D1–D9 and is worth doing whether or
not full design ever lands. Then **one phase** implements the design-based `n_eff` (Route A, §4.2),
which is where nearly all the statistical value is — an estimated 250–400 lines in one new file
(`reg-influence.R` is 472 for comparable work) plus one argument threaded through two leaves. Everything beyond that — design-based covariances
between rows, replicate designs, design-based effect sizes — is separable and can be declined
individually without making the result dishonest, provided the legend says what it did.

**Decisions taken (2026-08-11)** — the full list with reasoning is §11:

| # | decision |
|---|---|
| Q1 | the hygiene fixes (D1–D9) are **authorised on their own** → **z14-i** |
| Q2 | **Route A** (design-based `n_eff`) is the route → **z14-ii** |
| Q3 | Route A's conservatism on the difference is **accepted**; the row-to-row covariance is **not** stored |
| Q4 | `ids` / `strata` / `fpc` / `nest` are **removed** from `tab()` and `tab_reg()` — "if you know `survey::`, pass a design object as `data`" |
| Q5 | replicate designs (`svrepdesign`): **out** — a clear refusal, not an approximation (§7.5) |
| Q6 | the effect size **becomes weighted** under a design |
| Q7 | the footer says *"Design-based (survey): weighted estimates, intervals and tests account for the sample design."* |
| — | jamovi is **out of scope**: rungs 1–2 only, survey-design block deleted (§7.4) |

**One expectation to set before any of this is built (§7.6).** Verified on the released research file
of *Trajectoires et Origines 2* (Ined-Insee): it ships `poidsi` / `poidsin` and **no design variables at
all** — the sampling strata are described in the documentation, but the identifying and geographic
variables are withheld from the FPR (§9.1 of its guide) and reachable only through the CASD. On a file
like that a `svydesign(ids = ~1, weights = ~poidsi)` carries only the unequal weighting, so **rung 3
collapses onto rung 2** (measured: design/Kish = 0.98). The feature pays off exactly where design
information exists — a survey the user ran themselves, a CASD-accessed file, ESS/SHARE, or the OECD
assessments — and the docs must say so rather than implying every weighted survey gains from it.

---

## 1. Inventory — every inferential quantity, and what drives it today

`W` = the design's weights are used. `D` = the design's *structure* (strata / PSU / fpc / calibration)
is used. A quantity can be `W` without being `D`, and that is exactly today's state almost everywhere.

| # | Quantity | Where | Point est. | Variance / p | Gap |
|---|----------|-------|-----------|--------------|-----|
| 1 | Cell `%` / mean | `plain_core` / `num_core` | **W** ✓ | — | — |
| 2 | Cell CI (`ci = "cell"`) | `tab_ci` → `ci_wilson` / `ci_pivot` | W | raw `n`, or Kish `n_eff` | **no D** |
| 3 | Cell-vs-reference diff CI + stars | `tab_ci` → `ci_newcombe` / `ci_mean_diff2` | W | idem, two independent bases | **no D**, no covariance |
| 4 | Ratio CI (`ci = "ratio"`) | `ci_katz_rr` / `ci_mean_ratio` | W | idem | **no D** |
| 5 | `OR = "OR"` / `"cumOR"` interval | `tab_apply_reference` → `ci_or` | W | Woolf on effective counts | **no D** |
| 6 | `color = "contrib"` residual | `tab_contrib` | W (contribution) | raw `n` / Kish `n_eff` | **no D** |
| 7 | Whole-table chi2 / F p-value | `tab_robust_overlay` | — | **W + D** ✓ (`svychisq`, `svyglm`+`regTermTest`) | — |
| 8 | Cramér's V / φ / η² | `agg_chi2` / `agg_anova` | **unweighted** | — | **no W, no D** |
| 9 | Fisher exact | `agg_fisher` | unweighted | — | dropped in robust mode |
| 10 | `Model_*` estimate + CI | `reg_fit` → `svyglm` | **W + D** ✓ | **W + D** ✓ | — |
| 11 | `Model_AME` | `reg_marginal` | **W only with `wt=`; NOT with a design** (D2) | W + D | **defect** |
| 12 | `Obs_*` (factor rows, closed form) | `reg_empirical` | **W only with `wt=`; NOT with a design** (D1) | raw / Kish `n_eff` | **defect + no D** |
| 13 | `Obs_*` (numeric rows / ordinal) | `reg_empirical_fit` → `reg_fit` → `svyglm` | **W + D** ✓ | **W + D** ✓ | — |
| 14 | `gap_se` (`color = "adjustment"`) | `reg-influence.R` → `svyrecvar` | — | **W + D** ✓ incl. `postStrata` | — |
| 15 | Model GOF footer | `reg_glance` | W + D (reduced set) | — | — |
| 16 | `stats = "interaction"` / `"global"` | `regTermTest` / `drop1` | **W + D** ✓ | — | — |

Read the table by rows 12–14: inside one `empirical = TRUE` table under a design, the **crude column
is unweighted**, its **numeric rows are design-based**, and the **gap test between it and the model is
design-based**. Three different inferential regimes in three adjacent columns of one table.

---

## 2. What is design-based today — measured

### 2.1 `tab()`

A `survey.design` passed as `data` (Phase 18j) does three things: it extracts `$variables`,
materialises `weights(design)` into `.svy_weights` and uses that as `wt`, and keeps the design for the
whole-table p-value. Verified end-to-end *(blocks 7–8)*:

* Point estimates **are** design-weighted — `tab(des, x, y)` printed 0.66126 for a cell whose weighted
  proportion is 0.66126 and whose unweighted one is 0.48570. ✓
* The whole-table p **is** design-based — reproduced `svychisq(~x + y, des, "F")$p.value` exactly
  (9.84951e-27). ✓
* Everything else is not. On a mild 120-PSU design: the mean cell CI is **1.59×** too narrow and the
  proportion cell CI **1.39×** too narrow against `svymean` on the same design.
* **`ids=` / `strata=` reach the p-value and nothing else.** Same table, same data, with and without
  `ids = "psu"`: the cell intervals are `identical()`, while the p-value moves from
  **1.42e-57 to 9.85e-27** — thirty orders of magnitude. The table shows a design-corrected omnibus p
  above stars that ignore the design entirely.

### 2.2 `tab_reg()`

* `Model_*` coefficients and their intervals go through `svyglm` on the (subset) design, so they carry
  clusters, strata, fpc and calibration. ✓ Verified against a hand `svyglm`.
* `gap_se` goes through `reg_if_se()` → `survey::svyrecvar(..., postStrata = design$postStrata)`. ✓
  Design-based *including calibration*, and measured (separately) 2.6× smaller than the naive combination of the two
  printed intervals (0.01011 vs 0.02662) — the correlated-estimator property z8-B documented.
* Crude columns split by predictor kind: a **numeric** predictor's `Obs_OR` was **exactly** the
  design-based `svyglm(y ~ age, des)` interval (verified to 1e-6), because z9 routes it back through
  `reg_fit()`. A **factor** predictor's `Obs_OR` on the same table is the closed-form Woolf interval on
  weighted cells with a raw base.

### 2.3 The nine defects found while measuring

All are reproduced by *block 7* of `dev/survey_design_measurements.R`.

**D1 — the crude block is unweighted under a prebuilt design.** `tab_reg()` sets `wt <- NULL` when a
design is passed (`tab_reg.R:4547`), so `design_spec$wt` is `NULL` and every consumer of it silently
loses the weights. Measured on a design whose weights correlate with the outcome:

| | `Obs_%` (x = mid) | `Obs_OR` (mid vs low) | `Model_OR` |
|---|---|---|---|
| design as `data` | **0.35250** | **0.51412** | 0.47880 |
| `wt = "w"`, `ids = "psu"` | 0.19794 | 0.48175 | 0.47880 |
| no weights at all | 0.35250 | 0.51412 | — |

The design column is bit-identical to the *unweighted* one. `tab()` does not have this bug because it
materialises `.svy_weights` (`tab.R:630`); `tab_reg()` has no equivalent line.

**D2 — `effect = "ame"` under a design returns a sample-average, not a population-average, marginal
effect.** Same root cause: `reg_marginal(..., wt = design_spec$wt, ...)` passes `NULL`, and
`marginaleffects::avg_comparisons()` does **not** pick up an `svyglm`'s design weights by itself
(measured on a separate fixture: unit-level contrasts averaged unweighted −0.189159,
population-weighted −0.178305). End-to-end *(block 7)*: `tab_reg(des, effect = "ame")` = −0.230955
against `tab_reg(d, wt = "w", effect = "ame")` = −0.203627, a **13 %** error.
**This one hits the model column, not the crude one.**

**D3 — `tab_reg(svrepdesign, …)` errors.** `survey:::svyglm.svyrep.design` rebuilds its call and
evaluates it with `with(data, eval(g))`, so the `family = fam_obj` argument is looked up as a *symbol*
in the data frame's enclosure and `reg_fit()`'s local is invisible: `objet 'fam_obj' introuvable`.
Verified that `do.call(survey::svyglm, list(fml, design = …, family = fam_obj))` fixes it and that a
non-replicate `svydesign` never triggers it (which is why it was never seen).

**D4 — `tab(svrepdesign, …)` errors.** `weights(design_obj)` returns the **replicate weight matrix**
for a `svyrep.design`, so `data[[".svy_weights"]] <- as.double(weights(design_obj))` tries to write
n × R values into n rows (measured: *le tableau de remplacement a 120000 lignes, le tableau remplacé
en a 6000* — a 6000 × 20 replicate matrix). `weights(design, type = "sampling")` is the full-sample
vector and is `all.equal()` to the original weights.

**D5 — only two entry points accept a design.** `tab()` and `tab_reg()` do; `tab_num()`, `tab_plain()`,
`tab_many()` and `tab_counts()` all fail with an internal error from `tidyselect`/`dplyr`
(*"`x` must be a vector, not a `<survey.design2>` object"*). A user who reaches for `tab_num(des, …)`
— the natural thing after `tab(des, …)` works — gets a `vctrs` FAQ link.

**D6 — the effect size is unweighted under a design.** Measured Cramér's V = 0.20888 reported beside a
design-based p, where V on the weighted (population) table is 0.20068. The gap scales with how much the
weights matter — on a fixture with a stronger weight–outcome association it was 0.12202 against 0.08855,
a 38 % difference. Either way the p-value says "population, design-based" while the effect size beside
it says "sample, unweighted".

**D7 — the weight name leaks into the footer.** `tab(des, …)` prints *"Pondéré par `.svy_weights`."*
— an internal column name in user-facing output.

**D8 — `tab_reg()` under a design never says it is weighted.** `reg_meta$wt` is `NA` (same NULL as D1),
so no "Weighted by" line is emitted at all.

**D9 — `ids=`/`strata=` on `tab()` are silently inference-partial.** Documented in §51 as "only the
omnibus p is design-based", but nothing in the *table* says so; see the thirty-orders-of-magnitude
measurement in §2.1.

---

## 3. How wrong is the current approximation

Design of the measurements: synthetic surveys with a real intra-cluster correlation (PSU random
effects), strata that explain the outcome, unequal weights, and — separately — calibration to true
population totals. Everything is compared to `survey`'s own answer on the identical design.

### 3.1 A single cell (`ci = "cell"`)

n = 20 000, 400 PSUs, weight CV 0.60 (Kish deff 1.361), real PSU random effect. Entries are
SE(design) / SE(tabxplor) *(block 1)*:

| design | vs raw `n` | vs Kish `n_eff` |
|---|---|---|
| weights only | 1.14 | 0.98 |
| + strata | 1.14 | 0.98 |
| **+ clusters** | **1.70** | **1.46** |
| strata + clusters | 1.45 | 1.24 |
| + calibration | 1.45 | 1.24 |

Kish handles the unequal-weight part well (1.14 → 0.98) and is **blind to clustering by construction**
— it is a function of the weights alone, so the cluster row is where it fails.

### 3.2 The difference — which is what stars and colours actually use

tabxplor's stars come from the cell-vs-reference *difference*, never the cell interval. That difference
has its own design effect, and it is **not** the cell's:

| row variable | cell: design/raw | cell: design/Kish | **diff: design/raw** | **diff: design/Kish** |
|---|---|---|---|---|
| spread across PSUs | 1.63 | 1.40 | **1.23** | **1.05** |
| **segregated into PSUs** | 2.38 | 2.04 | **2.40** | **2.06** |

*(block 2.)* This is the single most important measurement in the study. When the row variable is
*spread* across PSUs (an attitude, a birth cohort), the cluster effect partly cancels out of the
difference and Kish alone gets within 5 %. When the row variable is **segregated** — region, race,
urban/rural, school, firm, *quartier*, i.e. the archetypal sociological crosstab — the difference
carries the **full** design effect and today's stars are **2.4× too generous**, **2.1× with Kish on**.
A reported z of 1.96 is a real z of 0.95, i.e. **p ≈ 0.34**.

### 3.3 Calibration — where Kish points the wrong way

Finite-population simulation, N = 300 000, unequal-probability sample of 3 023, calibrated on a
predictive auxiliary to **true** population totals:

```
uncalibrated : SE_design 0.01037 | Kish n_eff 2315 -> 0.01036   (design/Kish 1.00)
CALIBRATED   : SE_design 0.00959 | Kish n_eff 2316 -> 0.01036   (design/Kish 0.93)
             calibration moved the DESIGN SE by x0.925 and the Kish SE by x1.000
```
*(block 3.)*

Calibration **reduces** the true variance by 7.5 % while `n_eff` moves by one unit out of 2 315. Kish's
`deff = 1 + CV²(w)` is a property of the weights, and calibration changes the weights without changing
what they are worth. So on a calibrated design Kish is *conservative* — the opposite direction from
clustering. The two errors do not cancel in any predictable way; on the full design of §3.1 they
happened to partially offset (1.45 → 1.24), which is luck, not a property.

### 3.4 The odds ratio

Same pattern, measured against a saturated `svyglm` on the design (log-scale interval width ratios,
design / tabxplor):

| predictor | Woolf on raw `n` | Woolf on Kish `n_eff` | Woolf on **design** `n_eff` |
|---|---|---|---|
| spread across PSUs | 0.81 – 0.82 | 0.95 – 0.96 | 1.23 – 1.26 |
| **segregated** | **0.39 – 0.44** | **0.46 – 0.51** | **0.97 – 0.99** |

*(block 5b.)* Under a segregated predictor the crude `Obs_OR` interval is **less than half** the width
the design implies, and Kish recovers barely a tenth of the gap.

### 3.5 Summary

The current approximation is **excellent for an unclustered weighted survey with Kish on** and **badly
wrong for a clustered survey with a segregated row variable**, which is the majority of national
sociological surveys. Nothing in the current output distinguishes the two situations for the user.

---

## 4. Architecture — and the seam that already exists

### 4.1 Every per-cell inference in `tab()` already routes through one field

This is the finding that decides the whole question. `n_eff` (the 19th fmt field, Phase 18s) is
documented as *"the effective sample size used for this cell's confidence interval"*. Grep says it is
read by:

* `tab_ci()` — `dplyr::coalesce(get_n_eff(col), get_tot_n(col))` for the `"cell"`, `"row"` and `"col"`
  bases and `coalesce(get_n_eff(col), as.double(get_n(col)))` for `"mean"` (`tab.R:5694-5710`), and by
  the reference-cell base a few lines below. So **both the interval and the p-value** of every factor
  and numeric cell take it.
* `tab_apply_reference()` — `tabs_neff` is threaded into the `ci_or()` call for the `color = "OR"`
  significance (`tab.R:4105-4107`).
* `tab_contrib()` — `tne <- get_n_eff(...)` for the adjusted standardized residual, hence for z4's
  whole `color = "contrib"` significance and its `guaranteed_effect` scale (`tab.R:6225-6239`).
* the numeric leaf writes it from its own `_en` (`tab.R:5319-5322`), the factor leaf from
  `leaf_wide_pct(tabs_w2 = )` (`tab.R:4292-4323`).

There is **no per-cell inference in `tab()` that bypasses it.** The colour engine, the stars, the
legend and every exporter consume `ci_inf`/`ci_sup`/`pvalue`, which are all downstream of it.

### 4.2 Route A — put a design-based effective n in that field

Define, per cell:

```
proportion :  n_eff = p (1 − p)   / Var_design(p̂)
mean       :  n_eff = s²          / Var_design(x̄)
```

and write it into `n_eff` exactly where the Kish value is written today. This is not a workaround: it
is the standard survey device. `survey::svyciprop(method = "beta")` — the Korn–Graubard interval — is
defined as `binom.test` *"with an effective sample size based on the estimated variance of the
proportion"*. Route A is that construction, applied to tabxplor's existing Wilson / Newcombe / Katz /
Welch / Woolf engines.

What changes: `plain_core()` and `num_core()` gain one argument and one branch. What does **not**
change: `tab_ci()`, all nine `ci_*` engines, `fmt_class.R`, the colour engine, the break scales, the
legend, the four exporters, the fmt record (no new field, no new column attribute), the jamovi carrier,
`test-fmt-contract.R`, every golden.

`design_spec` is **already in the ctx** (`tab.R:1424`, `1530`) and `tab_transform()` does
`list2env(ctx, environment())` — so it is already in scope at the leaf call sites. The threading is two
lines.

### 4.3 Does it work? Measured

n = 20 000, 400 PSUs, 4 × 2 table, stratified + clustered *(block 5)*.

**Cell intervals — essentially exact.** `ci_wilson()`, tabxplor's own engine, on a design-based `n_eff`
against `survey`'s design Wald interval:

```
                     design-Wald        Wilson(n_eff)
spread      A     [0.4304;0.4730]     [0.4305;0.4731]
            B     [0.5390;0.5794]     [0.5389;0.5793]
SEGREGATED  A     [0.4369;0.5020]     [0.4371;0.5020]
            B     [0.5219;0.5912]     [0.5218;0.5909]
```

**Differences — right where it matters, conservative where it does not:**

| row variable | Newcombe on design `n_eff` / design truth | today (raw n) / design truth |
|---|---|---|
| spread | ×1.20 – ×1.30 (too wide) | ×0.82 – ×0.85 (too narrow) |
| **segregated** | **×0.98 – ×1.00** | ×0.40 – ×0.44 |

And for the odds ratio, §3.4: ×0.97–×0.99 segregated, ×1.23–×1.26 spread — the same shape.

### 4.4 Route A is not a one-way widener — it carries the design's precision GAINS too

Worth stating explicitly, because the intuition runs the other way. `n_eff` is inverted from
`Var_design(p̂)`, and that variance comes from `svyrecvar(..., postStrata = design$postStrata)`, which
carries **every** design feature at once — unequal weights and clustering (which inflate it),
**stratification, `fpc` and calibration / post-stratification** (which shrink it). So a design-based
`n_eff` can, and does, **exceed the raw n**, and the interval then comes out *narrower* than today's
*(block 5c)*:

| design | raw `n` | Kish `n_eff` | **design `n_eff`** | deff | interval width vs today |
|---|---|---|---|---|---|
| stratified, equal weights, predictive strata | 4 000 | 4 000 | **5 663** | 0.71 | **×0.84** |
| calibrated on a predictive auxiliary, equal weights | 4 000 | 4 000 | **5 748** | 0.70 | **×0.83** |

Kish cannot move at all in either case (×1.00): with equal weights `CV²(w) = 0`, so `deff = 1` by
construction, whatever the design actually gained. This is the mirror image of §3.3 — Kish is blind to
precision gains for exactly the same reason it is blind to clustering losses.

There is therefore **no anti-conservative correction that Route A leaves on the table** at the cell
level, and none that needs Route B: strata and calibration are *inside* the single-cell variance, and
§4.6 measured that call exact to 2.2e-16 on a calibrated design. The one gain Route A misses is a
different object entirely — see next.

### 4.5 What Route A cannot carry — state it, do not hide it

A per-cell effective n carries the cell's own design effect. It **cannot** carry the design
**covariance** between the cell and its reference row, because that is not a property of either cell.
Consequence, exactly as measured above:

* when the row variable is segregated across PSUs, the covariance is near zero and Route A is right to
  **within 3 %** (proportions and odds ratios alike);
* when it is spread, the covariance is positive and large, the true difference variance is *smaller*
  than the sum of the two cell variances, and Route A is **20–30 % conservative**.

That is the honest trade: **Route A never produces a star the design does not support**, and sometimes
withholds one the design would support. Given that today's error runs in the opposite direction and
reaches a factor of 2, this is the right way to be wrong.

**SETTLED (Q3): accepted, and the covariance is NOT stored.** The exact-difference option — one more
per-cell quantity plus a change to how `tab_ci()` combines two bases — stays out. §4.6's `svyrecvar`
call still *produces* the covariance as a by-product; it is simply discarded, so reopening this later
costs nothing that is being spent now.

Route A also cannot make the **whole-table effect size** design-based (D6) — that is a separate
computation on the weighted table, not an interval.

**Nor the `contrib` residual on a percentage table (z14-ii, ruling Q1).** Its base is the subtable's
GRAND total, which under a design has no effective n of its own (`p = 1` for the total column, so
`p(1-p)/Var` is undefined). Where the table's stored `type` says `n_eff` is already on that base — a
counts table, `pct = "all"` / `"all_tabs"` — contrib reads it per cell and is design-corrected for free;
on a row- or column-percentage table `n_eff` holds the row / column base instead, so contrib keeps the
unweighted grand total. The omnibus p above it is design-based either way (`svychisq`).

### 4.6 Producing the design variance — one `svyrecvar` call per column

The linearized influence function of a row-percentage cell is elementary. For row domain *i* and column
level *j*, with `W_i = Σ_{row=i} w`:

```
p_ij = Σ w·1{row=i, col=j} / W_i
z_ij(k) = 1{row_k = i} · ( 1{col_k = j} − p_ij ) / W_i        Var(p_ij) = svyrecvar(w·z_ij, …)
```

`pct = "col"` is the transpose; `pct = "all"` uses the whole table as the domain; a mean's influence
function is `1{row=i}(x − x̄_i)/W_i`. All four are two lines each.

Batching one **column level** at a time gives an `n × R` matrix whose `svyrecvar` is the full `R × R`
covariance — so a single call yields every cell variance of that column *and* every cell-vs-reference
covariance, should §4.5's exact-difference option ever be wanted. Verified against `survey`'s own
`svyby(covmat = TRUE)` +
`svycontrast`:

* cell SE, uncalibrated: max relative error **0** *(block 4)*
* cell SE, **calibrated** (`postStrata`): max relative error **2.22e-16** *(block 6)*
* difference SE: ratio **1.000000000** *(block 4)*

`reg-influence.R` already calls `svyrecvar` with `postStrata`, so calibration needs no new thinking —
only the same call from a second place.

**Replicate designs** (`svrepdesign`, `as.svrepdesign`, BRR/JK/bootstrap weights — how INSEE and many
national producers ship a design) do not go through `svyrecvar`. The analogue is
`survey::svrVar(θ̂_r, scale, rscales)` over the replicate weight matrix; verified to reproduce
`svyby`'s SE exactly. Computed cell-by-cell it is slow (0.07 s per cell at 100 replicates), but the
whole table is one sparse-indicator × replicate-weight matrix product — `n × nreps` work in total, i.e.
6 M operations for 60 000 rows and 100 replicates. Feasible; a separate branch, not a separate design.

### 4.7 Route B — delegate to `survey` entirely

Call `svyby(~col, ~row, design, svymean, covmat = TRUE)` and `svycontrast` per table, and write the
resulting estimates and intervals straight into the fmt fields, bypassing `tab_ci()`.

* **For**: exact, covariances included, replicate designs handled by the same call, no influence-function
  code at all.
* **Against**: it is a *second* computation of the point estimates (which the aggregate core already
  has, weighted, and which must stay bit-identical to what the cell displays), a second naming
  convention to align (`svyby`'s `row:colL2` keys), and it forks `tab_ci()` into "the normal path" and
  "the design path" — precisely the ad-hoc second layer Phase 17 spent itself removing. It also does not
  compose with `pct = "col"`, `comp = "all"`, `ref = <regex>`, or the `OR`/`contrib` measures without
  writing each of them a second time.
* Measured **5× slower** than the influence-function route on the same table (0.05 s vs 0.01 s), for
  the same numbers *(block 4)*.

**Rejected**, except as the test oracle — which is what it should be.

### 4.8 Route C — a PSU-augmented aggregate

The ultimate-cluster variance needs only the per-PSU sums of `w·z`, and those sums are
`Σ_{k∈psu} w·1{row=i,col=j}` and `Σ_{k∈psu} w·1{row=i}` — i.e. **weighted cell counts with `(stratum,
psu)` added to the data.table by-key**. So the design variance for the whole table is, in principle,
computable from the same aggregate the core already builds, with no `n`-length vectors at all, at a
cost of one extra key level (400 PSUs × 120 cells = 48 000 aggregate rows, trivial).

* **For**: it would preserve the "everything comes from the sufficient-statistics aggregate" invariant,
  it would compose with the `.fine` seam, and it would keep the jamovi tier-1/2 cache usable.
* **Against**: it re-implements `svyrecvar`. Multistage designs, `fpc` at several stages, lonely-PSU
  policy, and above all **calibration** (`postStrata`, which requires projecting `z` on the calibration
  model matrix — possible only if the calibration variables are in the by-key, and impossible for a
  continuous calibration variable) are all Lumley's code, correctly, and there is no reason to own a
  second copy of it.

**Rejected as the implementation**, but worth recording as the optimisation of last resort if the §6
cost ever becomes a problem. It is not one today.

---

## 5. `tab_reg()`'s crude counterparts — the same lever, after the same fix

### 5.1 The point estimate first

D1 and D2 must be fixed before any interval question is even meaningful. The fix is the line `tab()`
already has: when a design is passed, materialise `weights(design, type = "sampling")` into a column
and set `design_spec$wt` to its name. That single change repairs, at once:

* `reg_empirical()`'s weighted proportions, means, 2×2 legs and odds ratios (D1);
* `reg_marginal()`'s `wts` argument, hence the population-average AME and `ame_ratio` (D2);
* `reg_level_counts()`'s weighted n;
* `reg_meta$wt`, hence the "Weighted by" footer (D8).

It must **not** override the design for the *fit* — `reg_resolve_design()` keeps returning the prebuilt
design, since only it carries clusters, calibration and replicate weights.

### 5.2 Then the same effective-n lever

`reg_empirical()` already computes, per (var, level, category), an `emp_n_ci` documented as *"a SEPARATE
effective n for the intervals — the Kish n_eff when opted in, else the raw count"*, plus `emp_n_draw`
for proportions. Every crude interval consumes one of them:

* `ci_wilson(prop, n_draw)`, `ci_prop_diff(prop, n_draw, rprop, r_n_draw)`, `ci_katz_rr(...)`;
* `ci_or(prop·n_draw, (1−prop)·n_draw, rprop·r_n_draw, (1−rprop)·r_n_draw)` — note it feeds
  **effective counts**, so a design-based effective n flows straight into the Woolf interval;
* `ci_pivot(mean, sqrt(var/n_ci), df = n_ci − 1)` for the gaussian base;
* `ci_mean_diff2` / `ci_mean_ratio` for the numeric effects.

So `emp_n_ci` / `emp_n_draw` is `n_eff`'s twin, and §4.3/§3.4's measurements apply verbatim. The
influence functions needed to produce it for a crude cell are *already written*:
`reg_crude_if_maker()` in `reg-influence.R` returns exactly the per-observation influence of a crude
cell contrast, and `reg_if_se(d, design)` already turns it into a design-based SE with `postStrata`.
**The crude column's design-based variance is one call to code that exists**, on the same fit's design.

### 5.3 The inconsistency this closes

Today, for the same crude effect, the package computes:

* a **printed interval** from Woolf on a raw base — 1.2× too narrow under a spread predictor and
  **2.3–2.6× too narrow** under a segregated one (§3.4);
* a **gap SE** from `reg_crude_if_maker()` + `svyrecvar` — design-based.

Two variances for one estimator, one shown and one used. z8-B documented the *robust-vs-descriptive*
part of this on purpose; the *design-vs-no-design* part is not documented anywhere because it was not
known.

**z14-iii closes it, up to one named term.** Both quantities now come from the same influence function
and the same `svyrecvar`. They still differ in what they carry: the gap SE uses the *contrast*
influence, so it includes the covariance between the two cells; the printed interval is built on a
*per-cell* effective n, so Route A's discarded covariance (§4.5, ruling Q3) is exactly what separates
them. Measured on a stratified + clustered design: the `Obs_OR` log-width lands within **2–7 %** of the
design-based univariable `svyglm`, where the single-stage base was **15 %** out — and the direction is
not guaranteed, because the cell-to-cell covariance can go either way (§3.4 measured 0.97–0.99 with a
segregated predictor). "Conservative, never anti-conservative" is a statement about the *difference of
proportions* in `tab()`, not about a ratio here.

### 5.4 What stays out of reach in `tab_reg`

* `multinom` / `polr` (`nnet`, `MASS`) have no design-based fitter; z10's score-based influence
  functions give the *sandwich*, not the design variance. Weighted 3+ level already routes to `svyolr`
  / `svyVGAM` where possible; a prebuilt **replicate** design has neither.
* `method = "profile"` is undefined for `svyglm` and already falls back.
* AIC/BIC/McFadden are already reduced under a design (Rao-Scott AIC, `psrsq`) — no change.

---

## 6. Cost

n = 60 000, 1 000 PSUs, 15 × 8 table (120 cells), stratified + clustered:

| step | time |
|---|---|
| `tab()` today, weighted, `ci = "diff"`, `test = TRUE` | 0.25 s |
| design-variance pass (8 `svyrecvar` calls of 60 000 × 15) | **0.56 s** |
| same, on a **calibrated** design | **1.28 s** |

*(block 6.)* So a design-based table costs **≈ 3×** a weighted one, **≈ 6×** if calibrated. For an
opt-in on a 10 k–60 k-row survey producing a handful of tables, that is the same order as the existing
`tab_robust_overlay` and is not a design constraint. Memory is one `n × R` double matrix at a time
(60 000 × 15 = 7 MB).

Two architectural costs matter more than the seconds:

* **The microdata scan becomes mandatory.** A design-based variance cannot come from a count aggregate,
  so under a design the `.fine` seam must force `use_raw`, and a tier-1/2 aggregate cache cannot serve a
  design-based table. Moot for jamovi itself, where no design ever arrives (§7.4), but it constrains any
  future caching of the R path.
* **The parallel row-axis path** already ships `design_spec` inside the per-row_var ctx, so a prebuilt
  design — which carries `$variables`, i.e. the whole dataset — is serialised once per worker. That is
  a pre-existing cost, but full design makes it load-bearing; either strip `$variables` before dispatch
  and rebuild worker-side, or refuse to parallelise under a design.

---

## 7. The opt-in shape

### 7.1 "The opt-in is passing a design object" — yes, and it should mean everything

The maintainer's proposal is the right one and is already half-built. Passing a
`survey.design` / `svyrep.design` as `data` is unambiguous, self-documenting, needs no new argument,
and cannot be set by accident. It should imply, uniformly across `tab()`, `tab_num()`, `tab_plain()`,
`tab_many()`, `tab_counts()` and `tab_reg()`:

1. estimates use the design's weights (already true in `tab()`, **broken in `tab_reg()` — D1**);
2. every interval, star and colour threshold uses the design's variance (**new — Route A**);
3. the whole-table test is design-based (already true);
4. the effect size is computed on the weighted table (**new — D6, settled Q6**);
5. the footer says so, in one sentence, naming the design rather than `.svy_weights` (**D7/D8**).

Point 5 is not cosmetic. The legend already names the CI method; a design-based table must be
distinguishable from a Kish one — §3.2/§3.3 measured that the two differ by a factor of 2 in either
direction. **SETTLED (Q7)**, the agreed sentence, replacing *"Weighted by X"* whenever a design is
present, and sitting beside the existing CI-method clause:

> Design-based (survey): weighted estimates, intervals and tests account for the sample design.

### 7.2 `wt` stays, `ids` / `strata` / `fpc` / `nest` go (SETTLED, Q4)

The maintainer's instinct — *"it's better done with `survey::`, and what is to be done there is specific
to each survey"* — is supported by the measurements. The answer differs by argument.

* **`wt` must stay.** It is the 1.3.1 public API, it is what jamovi's weight selector produces, and the
  overwhelmingly common case is a single weight column with no design metadata at all. Removing it is
  not on the table.
* **`ids` / `strata` / `fpc` / `nest` should go — removed, not soft-deprecated, since they never were public.** They were added in
  Phase j to mirror `tab_reg`, they reach **only the omnibus p-value** (D9), they duplicate
  `svydesign()`'s own arguments one-for-one, and they cannot express the things that actually matter in
  a French survey: calibration, replicate weights, multistage `fpc`, `nest`ing subtleties. Every real
  use of them is better written as one `svydesign()` call the user keeps in their script. Let a design object
  be the only way to say "clustered". The same removal applies to their jamovi mirrors — see §7.4.
* **`test = "survey"` becomes redundant.** With `ids`/`strata` gone there is nothing left for it to
  build a design *from*, and a design passed as `data` already says "design-based". Decide its fate in
  z14-i: either drop the value, or keep it as a no-op alias for `TRUE` on a design. What it must not do
  is stay as a third thing the user can ask for and not get.

**The resulting position, in one sentence for the docs**: *if you know how to use `survey::`, pass a
design object as `data`; otherwise use `wt`, and at best `options(tabxplor.kish_neff = TRUE)`.*

### 7.3 What happens to `tabxplor.kish_neff`

It stays, and it becomes clearly the **middle rung of a three-rung ladder** the vignette already
teaches (Phase 18s):

| rung | input | what the intervals carry | available in |
|---|---|---|---|
| 1 | `wt = w` | weighted estimate, raw `n` — no design effect at all | R + jamovi |
| 2 | `wt = w` + `options(tabxplor.kish_neff = TRUE)` | unequal-weighting only; **blind to clustering, blind to calibration** | R + jamovi |
| 3 | a `survey` design as `data` | the real design effect: strata, clusters, fpc, calibration | **R only** (§7.4) |

The one documentation change §3 forces is to stop describing rung 2 as *"the design effect"*. It is
one component of it, and §3.2 shows it can leave 2.1× of the error on the table.

### 7.4 jamovi — out of scope (SETTLED)

**Maintainer ruling: a full survey design is an expert operation and belongs in R. The jamovi UI loses
every survey-design argument and offers rungs 1–2 only — plain weighted analysis, or Kish `n_eff` at
best.** The reasoning is the same as §7.2's, one step further: a design cannot be *built* in a
point-and-click pane, and a half-design (a strata picker with no calibration and no replicate weights)
is the worst of both worlds — it looks like design-based inference and is not.

Concretely:

* **`jmvtab` (Crosstables) needs no change.** Its `test_robust` selector already offers exactly
  `classic` / `kish` (`jamovi/jmvtab.a.yaml:205`), and it never exposed `ids`/`strata`. It is already at
  the target state.
* **`jmvtabreg` (Regressions) loses its "Advanced: survey design" block** — `ids`, `strata`, `fpc`,
  `nest` (`jamovi/jmvtabreg.a.yaml:359-391` and `jamovi/jmvtabreg.u.yaml:353-375`). `wt` stays: it is the
  weight selector, and rungs 1–2 are built on it. Removing options from `.a.yaml` needs the maintainer's
  `jmvtools::prepare()` to regenerate `R/jmvtabreg.h.R`; the R-side `tab_reg()` formals go at the same
  time (§7.2), so the two stay in step.
* **No design object ever reaches a `.b.R` backend**, so none of Route A's machinery is invoked there.
  That also removes the §6 concern about a design being serialised into the jamovi cache: it cannot
  happen.
* **The jamovi footer must not overclaim.** With a weight set it says weighted; under
  `test_robust = "kish"` it must say Kish's effective sample size and *not* "survey design" — §3.2/§3.3
  measured that the two can differ by a factor of 2 in either direction.

This ruling *shrinks* the work: the jamovi item of z14-i becomes a deletion, and the jamovi
question is closed before §11 has to ask it.

### 7.5 Q5 answered — what replicate designs are, and who actually ships them

**What they are.** A replicate design replaces the *formula* for the variance with a *set of alternative
weight columns*. The producer builds R re-weightings of the sample (typically 20–160), each one
imitating "what if this part of the sample had been drawn differently": drop a cluster and reweight the
rest (**jackknife**, JK1/JKn), flip half the strata up and half down (**balanced repeated replication**,
BRR; **Fay's BRR** softens the flip so no weight hits zero), or resample PSUs (**bootstrap**). The
analyst recomputes the statistic under each weight column and reads the variance off the spread:

```
Var(θ̂) = scale · Σ_r rscale_r · (θ̂_r − θ̂)²          # survey::svrVar()
```

**Why producers use them.** Two reasons, and the second is the one that matters here:

1. *They generalise mechanically.* Linearization needs an influence function per estimator; replication
   needs none — medians, Gini coefficients, model coefficients all work the same way. This is why the
   OECD ships them: its assessments compute dozens of statistic types.
2. *They protect confidentiality.* Strata and cluster identifiers are the most disclosive variables in a
   survey file — they locate people. Replicate weights let an outsider reproduce the producer's official
   standard errors **without ever seeing the design variables**. That is the trade: the design is
   released as numbers instead of as identifiers.

**Do French national surveys use them? Verified: no — they withhold the design instead.** On the
released research file of *Trajectoires et Origines 2* (Ined-Insee 2019-2020), the canonical French
survey of origins and discrimination:

* the sampling was genuinely complex — stratified by origin group (§5.4 of its guide), then weighted
  through nonresponse correction, weight-sharing and **calibration on margins** (§7, CALMAR-style);
* the file provides exactly **`poidsi`** ("the variable to use to weight the observations") and
  `poidsin`, its sample-size-normalised twin;
* it provides **no replicate weights and no design variables** — §9.1 "Variables non diffusées dans le
  FPR" withholds the identifying and geographic variables, which are reachable only through the CASD.

That is the standard shape of an INSEE/Ined *fichier de production et de recherche*. INSEE does use
bootstrap internally to compute the precision it publishes (the JMS 2022 work on the Enquête Emploi
2019-2028 is the reference), but that is the producer's own variance calculation, **not** a replicate
weight file handed to secondary analysts.

**Where a French social scientist does meet them: the OECD assessments.** PISA, PIAAC and TALIS ship
**80 Fay-BRR replicate weights** and require their use — the OECD publishes `repest` (Stata) and
`Rrepest` (R) for exactly this. French education sociology uses these heavily. SHARE and some Eurostat
files also ship replicate or bootstrap weights.

**What it changes for tabxplor.** Only the variance backend, not the concept:

| | ordinary design (`svydesign`) | replicate design (`svrepdesign`) |
|---|---|---|
| point estimates | full-sample weights | full-sample weights — **identical** |
| Route A's `n_eff` | `svyrecvar` on the influence function | `svrVar` over the R replicate estimates |
| cost | one `svyrecvar` per column level | one `n × R_rep` weighted crosstab, then a spread |
| `tab_reg()` model column | `svyglm` — works | `svyglm` — works **once D3 is fixed** |
| `tab_reg()` gap test / `color = "adjustment"` | `svyrecvar` — works | **cannot follow**: `reg_gap_se_columns()` already returns `NULL` for a `svyrep.design` (`tab_reg.R:2281`), because an influence function has no replicate analogue without recomputing the whole fit per replicate |

Measured: `svrVar` reproduced `svyby`'s replicate SE exactly on a hand-computed cell. The naive
cell-by-cell loop is slow (0.07 s per cell at 100 replicates), but the whole table is one
indicator × replicate-weight matrix product — `n × R_rep` work in total, ~6 M operations for 60 000 rows
and 100 replicates.

**Maintainer’s decision: OUT entirely for now** — a `svrepdesign` errors with a clear message pointing
at `svydesign()`. Defensible: it is not how French *national* files arrive. D3/D4 still land in z14-i
regardless — they are what turns today's raw R evaluation error into that clear message. The audience
this declines is the OECD-assessment one (PISA / PIAAC / TALIS); reopening costs ~40 lines (an `svrVar`
branch beside the `svyrecvar` one in z14-ii) and one degradation row, so nothing here forecloses it.


### 7.6 Reachability — the expectation to set before building

This follows directly from §7.5 and deserves saying plainly, because it bounds the payoff of the whole
feature. On a file that ships **one calibrated weight and nothing else** — the majority of French
research files — `svydesign(ids = ~1, weights = ~poidsi)` carries only the unequal weighting, and
Route A's `n_eff` then lands within **2 %** of the Kish one (§3.1, `weights_only` row: design/Kish =
0.98). Rung 3 collapses onto rung 2.

The feature therefore pays where design information actually exists:

* a survey the researcher ran themselves (they know their strata and clusters);
* a CASD-accessed file, where the design variables *are* released;
* ESS, SHARE, and the OECD assessments;
* any file with a `strate`/`grappe`/`nomen` column — cheap to check, and worth a one-line instruction in
  the vignette: *look for a stratum or cluster variable in your file; if there is none, `kish_neff` is
  already all the correction available to you.*

This does not weaken the case for building it — §3.2's 2.4× error is real wherever clustering exists —
but it does mean the docs must not imply that passing a design always sharpens a table.

---

## 8. What would still be missing after all this — the honest residue

Listing these is part of the answer to "is it full?".

1. **Exact difference variances** (§4.5) — **declined by Q3**: it needs the per-column `R × R`
   covariance rather than a per-cell scalar, i.e. a second stored quantity plus a change to how
   `tab_ci()` combines two bases. Route A stays conservative for a difference of proportions; for a
   crude **ratio** (`Obs_OR`, `Obs_RR`) the same missing covariance can go either way, a few percent
   (measured in z14-iii, §5.3) — down from 15–25 % before it.
2. **Design-based Fisher / exact tests** — do not exist; already dropped in robust mode.
3. **A crude COLUMN for a 3+ level outcome** (multinomial, ordinal): the crude effect is folded into
   the model cell as a point (`obs`), so there is no interval to make design-based. Its *tooltip*
   percentages are design-based since z14-iii. Any crude twin at all on a replicate design is refused
   outright at the boundary (§7.5).
4. **Quantiles / medians** — tabxplor has none, so nothing is missing; if `tab_num` ever gains them,
   `svyquantile` is a different machine.
5. **Domain-estimation subtleties**: tabxplor subsets by row/tab variable, which is domain estimation
   and is correct through `svyrecvar` on the full design; but a *user* who pre-filters the data frame
   before `tab()` loses the design. The design-as-`data` opt-in makes this visible — `subset(design, …)`
   is the correct idiom and should be in the vignette.
6. **Missing data**: design-based tests run on complete cases of (row, col) per subtable (documented in
   §51); Route A inherits that convention. Multiple imputation is out of scope.
7. **`na = "keep"`**: an explicit NA level is a legitimate domain, so nothing breaks, but the design
   variance is then computed for the NA domain too.

---

## 9. Comparable packages — what the ecosystem does

* **`survey`** (Lumley) is the reference implementation and the only one that owns the variance
  algebra. `svyby(influence = TRUE)` + `svycontrast` for domain contrasts is a 2020-era addition; before
  it, domain covariances were replicate-weights-only.
* **`srvyr`** is a tidyverse front-end to `survey`; it computes, it does not format.
* **`gtsummary::tbl_svysummary()`** takes a design and formats design-based summaries — but it is a
  *summary* table, not a colour-coded crosstab with per-cell reference comparisons, and it has no
  equivalent of tabxplor's cell-vs-reference stars.
* **`pollster`** does weighted crosstabs but supports *simple weights only* and explicitly refers users
  to `survey` for complex designs — i.e. exactly tabxplor's current position.
* **`questionr`** (Barnier), the reference among French sociologists, is weight-only.
* **`surveytable`** wraps `survey` with Korn–Graubard (`svyciprop_adjusted`) and reliability flags.

**Nobody occupies the position this study describes**: a colour-coded, reference-compared, exportable
crosstab whose *cell-level* significance is design-based. That is a real argument for doing it — and
also the reason no existing package can be copied.

---

## 10. Implementation roadmap — Phase 18z14, three subphases

One roadmap entry, **z14**, because it is one feature. Its subphases follow the repo's convention
(`-i`, `-ii`, `-iii`, and `-iiii` if a fourth is ever needed — cf. Phase `e-iiii`, `17f-i`/`17f-ii`):
**one subphase = one session**, ending with the suite, its own documentation, and a maintainer commit.
Inside a subphase, a **commit point** marks where the model pauses for the maintainer to commit while
the session's context stays warm.

**There is deliberately no documentation subphase.** CLAUDE.md § *The last step of every implementation*
already requires every phase to update its own file headers, `dev/tabxplor_architecture.md`, the
CLAUDE.md roadmap line and NEWS — and documentation written in a *fresh* session is documentation
written after the context that made it accurate is gone. So each subphase documents what it landed, and
the only genuinely cross-cutting piece (the vignettes, which describe the finished three-rung ladder)
sits at the end of the last subphase, on a marked split seam.

Each subphase names its own verification and byte-identity target. The rule throughout: **any table that
does not involve a survey design must be byte-identical**, so a golden or snapshot moving off the design
path is a defect, not a regen.

---

### z14-i — the design path made honest (D1–D9 + the argument removal) — **IMPLEMENTED 2026-08-11**

> **Implementation record.** Suite green in both locales (`fr_FR.UTF-8`: FAIL 0, WARN 0, SKIP 4,
> PASS 5025 = +46, exactly the new `test-survey-design-path.R`; CI-equivalent `LC_ALL=C.UTF-8`:
> FAIL 0, SKIP 8, PASS 5008). **Zero golden/snapshot churn.**
>
> **What the maintainer's rulings changed, against the plan written below.**
> - **`test = "survey"` is REMOVED, not kept.** `test` is now `TRUE`/`FALSE` only, validated at the
>   public boundary; the rung is *derived* from what was already passed (weights / weights + Kish /
>   a design object). A user with only a weight column who wants the design-based test writes
>   `svydesign(ids = ~1, weights = ~w, data = d)` and passes it — one line, and the §7.2 doctrine.
>   The rung is resolved in **`tab_setup()`** (`svy_test_mode()`), the one place holding both the
>   resolved weight and the `design_spec` — which also fixed a silent drift: only `tab()` had the
>   rule, so `tab_many()` was *always* classic whatever the input.
> - **D6 went further than Q6.** The chi2 **and** the effect size are computed on the weighted table
>   whenever `wt` is given, not only under a design (ruling: "if the user chose weights, they want
>   weighted"). This closes an inconsistency *inside* the package, not just against `survey`: the CIs
>   have always been `Wilson(weighted p, unweighted n)` and the ANOVA F has always used §14's weighted
>   group mean/var with the unweighted n. Only the factor chi2 was still fully unweighted. Implemented
>   as a rescale of the weighted counts to the raw n, so — since `get_wn()` falls back to `get_n()` —
>   the unweighted path is byte-identical **by construction** (measured: `chisq.test` parity to 10
>   decimals; weighted V equals V on `xtabs(w ~ x + y)`). Fisher's exact is skipped on a weighted
>   table (an exact test enumerates integer tables); the `!` weak flag still fires.
> - **The z14-i footer says only what z14-i delivers** — *"Weighted by the survey design."* The
>   settled Q7 sentence claims the intervals account for the design, which is only true after z14-ii,
>   and it replaces this string at the same line (`tab_weight_line()`).
>
> **Two defects found while auditing this region, beyond D1–D9. One was real, one was not — recording
> both, because the false one looks convincing.**
> - **D10 (real, severe).** `[` does **not** drop rows on a **calibrated or PPS** design: it keeps all
>   *n* and sets `prob <- Inf`. So assigning the shorter complete-case frame errored, and
>   `tab_reg(<calibrated design>, …)` failed on **any** incomplete case. Fixed by padding the recoded
>   frame back to full length (the padded rows carry zero weight). Verified: OR and CI equal a hand
>   `svyglm` on the same calibrated design to 8 decimals; the non-calibrated path is unchanged.
> - **~~D11~~ (retracted).** The double subset `des_pre[keep, ]` recycling a short logical is
>   **harmless**: `[` only ever sets `prob <- Inf`, which is absorbing, so a row already excluded
>   cannot be re-included — `cal[i, ][keep, ]` selects exactly the rows of `cal[i[keep], ]`
>   (verified identical). Group keys are also correctly labelled. The *one-integer-subset* rule was
>   adopted anyway (D10 needs it, and it is the domain idiom), but as a simplification, not a fix.
> - **What IS real in the overlay is the FRAME**, and it needed two halves, not one. Picking the right
>   rows is not enough: `svychisq`/`svyglm` read their variables off the **design**, so the test still
>   saw the original levels. Measured: `tab(des, x, y, other_if_less_than = 100, test = TRUE)`
>   displayed `a / b / Others` while reporting p = 0.00079 — exactly `svychisq` on the **unlumped**
>   4-level table. After the fix it reports 0.01317 = `svychisq` on the lumped one.
>   Both halves are now `svy_domain_design(design, rows, frame)`, **one helper shared with
>   `tab_reg()`** (it is the same operation `reg_subset_design` was doing, so that function is gone).
>
> **Also repaired, unlisted, same root cause as D1:** `reg_resolve_multiplier()`'s frozen SD for
> `multiplier = "sd"` was computed unweighted under a design, and `reg_gap_se_columns()` passed
> `wt = NULL` into `reg_ame_if_maker()`/`reg_crude_if_maker()` — i.e. the gap test fed **unweighted**
> influence vectors to a design-based `svyrecvar`. One line fixed both with the rest.
>
> **Deleted as dead:** `svy_test_vars()` and `reg_design_formula()` (no callers at all);
> `reg_make_design()`/`reg_subset_design()` (one-caller aliases); `tab_prepare_pop()`'s `design_extra`
> block and its `data0`; `jmvtab-cache.R`'s `strata = opts$strata, ids = opts$ids` pass-through (the
> `opts` list never had those keys, and its `test_robust == "survey"` branch was unreachable).
>
> **Maintainer step:** `jmvtools::prepare()` — `jamovi/jmvtabreg.{a,u}.yaml` lost the survey-design
> block, so `R/jmvtabreg.h.R` needs regenerating. The `.b.R` and the cache already ignore the four
> options, so nothing is broken until then.

**Original plan, for the record:**


Region: the `data`-is-a-design boundary in `tab_reg.R` and `tab.R`, `survey-design.R`, the four other
entry points, `agg_chi2`/`agg_anova` + the robust overlay, and the jamovi YAML mirrors. Independent of
Routes A/B/C — worth landing even if nothing else ever does. **Changes numbers that are currently
wrong**, so it earns its own NEWS entry.

**First commit point — the root cause and the crashes.** One line materialises
`weights(design, type = "sampling")` into a column and sets `design_spec$wt` to its name (fixes **D1**
crude columns unweighted, **D2** the sample-average AME, **D8** the missing "Weighted by" footer, all at
once), *without* overriding the design for the fit. Then **D4** (`type = "sampling"` in `tab()`'s weight
materialisation, so a `svyrep.design` stops writing an n × R matrix into n rows) and **D5** (`tab_num()`,
`tab_plain()`, `tab_many()`, `tab_counts()` accept a design, or refuse it with a real message — accept is
cheaper, since they all funnel through the same boundary).

**D3** (`do.call(survey::svyglm, …)`, the `fam_obj` lookup failure) lands here too even though Q5 puts
replicate designs out of scope: it is one line, and the refusal Q5 asks for must be a *clear message*,
not an R evaluation error from inside `survey`. Pair it with that refusal — a `svrepdesign` reaching
`tab()` or `tab_reg()` aborts pointing at `svydesign()`.

*Verification*: fixtures on a design whose weights correlate with the outcome, asserting `Obs_%` equals
the **weighted** proportion and `Model_AME` equals the population-weighted `avg_comparisons`;
`tab_num(design, …)` returns a table; a `svrepdesign` produces the intended refusal, not a crash.

**Second commit point — the honest surface of that path.** **D6**: the effect size is computed on the
weighted table when a design is present (`agg_chi2` / `agg_anova` + the carry-through in
`tab_robust_overlay`). **D7**: the footer names the design, not `.svy_weights`. **Q4**: remove `ids` /
`strata` / `fpc` / `nest` from `tab()` and `tab_reg()` (formals, roxygen, `svy_design_vars` call sites,
`tab_prepare_pop`'s `design_extra`) and delete their jamovi mirrors —
`jamovi/jmvtabreg.a.yaml:359-391` + `jamovi/jmvtabreg.u.yaml:353-375` (§7.4). Decide `test = "survey"`'s
fate in the same pass: with the arguments gone it can only mean "a design was passed", which the design
already says.

*Verification*: Cramér's V under a design equals V on the weighted table; the classic path is
byte-identical (the effect size moves **only** when a design is present).
*Documentation*: `?tab` / `?tab_reg` for the removed arguments and the corrected weighting claims; NEWS
(this subphase changes wrong numbers, so it is user-facing); the CLAUDE.md roadmap line.
*Maintainer step*: `jmvtools::prepare()` to regenerate `R/jmvtabreg.h.R` — the YAML edit is inert until
then, and the R formals must go in the same commit so the two cannot drift.

---

### z14-ii — Route A: design-based intervals in `tab()` — **IMPLEMENTED 2026-08-11**

> **Implementation record.** Suite green (`fr_FR.UTF-8` and the CI-equivalent `LC_ALL=C.UTF-8
> LANGUAGE=en`), zero golden or snapshot churn off the design path — the acceptance criterion held.
>
> **What the plan got wrong, and what that simplified.**
> - **There are not four influence functions, there is one.** Every quantity here is a ratio of two
>   weighted sums, so §4.6's four bases are four `(u, v)` domain pairs in a `switch` (`svy_uv_v()`),
>   not four formulas. A fifth base would be one line. The mean is the same expression with
>   `(u, v) = (x, 1)`.
> - **Row domains fall out of the wide table's own keys**, with `"Total"` read as "every level of this
>   variable" — the rule `leaf_totrow_tottab()` and `build_total_rows()` already assume. So a data row,
>   a subtable total row and a total-table row need no special case at all, and total rows get a
>   design-based base for free (load-bearing: it is what `ref = "tot"` compares against).
> - **`svy_domain_design()` is deliberately NOT reused.** It swaps `$variables` because
>   `svychisq`/`svyglm` read their data off the design; `svyrecvar()` never does. Its calibrated/PPS
>   WARNING still applies, which is what `svy_var_prep()`'s scatter index and `w = 1/prob` are for.
> - **`svy_test_mode()` became `svy_inference_mode()`** (and `ctx$test_mode` → `ctx$inference_mode`).
>   It now governs the intervals as well as the test, so the two leaves stopped re-reading
>   `options(tabxplor.kish_neff)` — the same ladder had been derived in three places.
>
> **Q1 (contrib), as ruled — and it needed no new plumbing.** `chi2_write_contrib()` reads the `n_eff`
> FIELD per cell where the column's stored `type` says its base is the whole table (`"n"` / `"all"` /
> `"all_tabs"`), else the grand cell as before. `contrib_adj_resid()` did not change: it uses `n_base`
> only as `sqrt(n_base)` and `e_f * n_base < 1`, both elementwise. Byte-identical under Kish (for those
> bases `leaf_wide_pct()`'s `Dmat` is constant across the subtable, so the per-cell value **is** the
> grand cell's) and the standard first-order per-cell correction `z_design = z_classic·√(n_eff/N)` under
> a design — asserted exactly in the tests. A row-/column-percentage table's contrib keeps the grand
> base, which is §8's new residue line.
>
> **Q7 (the footer), as ruled: blanket.** Ruling Q7's sentence replaces z14-i's placeholder for any
> table built from a design. `tab_reg()`'s categorical crude `Obs_*` intervals are still single-stage
> until z14-iii, and that exception is now NAMED in `?tab_reg` rather than left implicit; when the
> variance pass genuinely fails, `svy_var_degraded()` says the intervals fell back, so the sentence is
> never silently untrue. **(Superseded by z14-iii, which made those columns design-based; the
> `?tab_reg` exception is gone and the sentence is blanket with nothing left to qualify.)**
>
> **Verified against `survey`, never against a hard-coded number**: cell SE and mean SE equal
> `svyby(covmat = TRUE)` / `svyby(svymean)` to **1e-15** on weights-only, stratified,
> stratified+clustered and **calibrated** designs; the col% and all% bases equal `svyby` on the
> transpose and `svymean` on the interaction; `ci_wilson()` on the design `n_eff` reproduces survey's
> interval to 4 decimals; §4.4's gain case gives `n_eff` 5155 on n = 4000 and a ×0.88 width, where Kish
> sits at exactly 4000.
>
> **One measured surprise:** a total row never carries a *cell* CI (pre-2.0.0 behaviour, unrelated to
> this phase) — the first draft of the test asserted one and failed. Its design base is still written,
> and still matters.
>
> **Two test files moved consciously**: `test-survey-design-path.R`'s footer assertion and
> `test-i18n-fr.R`'s msgid, both because the z14-i string was replaced by ruling Q7's.

**Original plan, for the record:**

Region: a new `R/survey-variance.R`, then the two leaves in `tab.R`. Fresh session: it needs the
influence-function algebra and the leaf/`n_eff` plumbing in context, neither of which z14-i touches.

**First commit point — the variance module, standalone.** New `R/survey-variance.R` beside
`R/survey-design.R`, written in `reg-influence.R`'s style (**every function returns `NULL`, never a
wrong number**). The four influence functions — row %, col %, whole-table %, mean (§4.6) — and the
`n × R` batching that gives one `svyrecvar` call per column level. No `svrVar` branch: Q5 put replicate
designs out of scope, and z14-i already refuses them at the boundary. Nothing is wired yet, so the
package's behaviour cannot change.

*Verification*: `survey` is the oracle — every assertion is "equals `svyby(covmat = TRUE)` /
`svycontrast` / `svyciprop` on the same design to *x* digits", never a hard-coded number, on a
stratified, a clustered, a stratified+clustered and a **calibrated** design.
*Byte-identity*: total, by construction (no call site yet).

**Second commit point — wire it.** `design_spec` is already in the ctx and `tab_transform()` already
`list2env`s it, so this is one argument on `plain_core()` / `num_core()` and one branch at the `n_eff`
write site (design → Kish → raw). Force `use_raw` under a design (a count aggregate cannot carry a
design variance). Then the **Q7 footer sentence**, which must land in this subphase and not later:
without it a rung-3 table is indistinguishable from a rung-2 one.

*Verification*: the design path checked against `survey` end-to-end (cell CI, cell-vs-reference diff,
`color = "OR"`, the `contrib` residual — all four ride `n_eff`, §4.1); plus §4.4's gain case, asserting
`n_eff > n` and a **narrower** interval under a stratified equal-weight design.
*Byte-identity*: **zero golden or snapshot churn off the design path** — the acceptance criterion for
the whole subphase.
*Documentation*: `?tab`, `?tabxplor-options` (rung 2 is *not* "the design effect"), the architecture
guide's new module, NEWS, the CLAUDE.md roadmap line.

---

### z14-iii — the crude (`Obs_*`) columns, then the finished ladder — **IMPLEMENTED 2026-08-11**

> **Implementation record.** Suite green in both locales, **zero golden / snapshot / `_color_golden`
> churn** — the acceptance criterion held: no fmt field, no column attribute, no crosstab path.
>
> **The route is NOT the one this section planned, and the reason matters.** The plan said
> `reg_crude_if_maker()` + `reg_if_se()`. Those produce the *identical* influence vector —
> `svy_var_mean()`'s `Z = wf·d·(x−M)/B` **is** the crude maker's identity-link leg `w(y−μ)/Σw` — but
> the crude maker only exposes the level-vs-reference CONTRAST, never a single leg, and `reg_if_se()`
> has no scatter, so it cannot serve a CALIBRATED design at all (see the row-space defects below).
> `R/survey-variance.R` already owns "the design variance of a domain mean", batches every level into
> one `svyrecvar` call, and was validated to 1e-15 including calibration in z14-ii. So the producer is
> `svy_var_mean()` with **one** new optional argument, `wmult` — a per-row weight multiplier, because a
> grouped-binomial row is a cluster of `trials` draws (`p = Σw·succ / Σw·trials`), i.e. the general
> ratio form, not a second formula. No `tot` argument was needed: the crude domain keys are level
> INDICES, which also makes the domain identical by construction to the grid's own `ok & x == l` and
> makes a predictor level literally named `"Total"` unreachable.
>
> **`emp_n_draw` became per (level, CATEGORY)**, not per level. The plan assumed 3+ level outcomes
> expose no crude interval; they expose no crude *column*, but the multinomial **html tooltip** prints
> `emp_prop_inf/sup` and `emp_diff_inf/sup`, so a per-level scalar would have been an approximation in
> user-visible output. `svy_var_mean()` returns an R × K matrix anyway, so this cost one `flat()` and
> one `rep(..., times = nl)`. Off-design the Kish value is category-free → byte-identical. `emp_n_ci`
> stays per level: a mean has no category. The `* mean(draws)` factor is **dropped** on the design
> path — `V_p` already carries the trials weighting, so keeping it would double-count.
>
> **Three row-space defects, all measured, none previously known** — prerequisites, because the new
> crude variance would have inherited their wrong rows:
> - `tab_reg(<svydesign>, …, split_var = )` **errored** with unequal group sizes (the normal case):
>   `utils::modifyList()` recurses into list elements and a `survey.design2` IS a list whose
>   `$variables` is a data.frame, so the per-group design was merged into the old one column by column
>   (*"replacement has 413 rows, data has 800"*; silent recycling when the sizes happened to divide).
> - On a **calibrated** design the same path ran and was wrong: measured OR `1/2.17` and `1/3.13`
>   against `svyglm`'s `3.48` and `4.11` on the same groups, no warning. Both fixed by ONE rule —
>   the design is never re-subset per group, and `reg_resolve_design()` maps its complete-case mask
>   through `.svy_row` so every subset goes into the ORIGINAL design's row space.
> - `color = "adjustment"` lost its gap test on **every** calibrated design with an incomplete case
>   (`svy_domain_design()` pads the fit's design, so the model leg is n-long and the crude leg
>   `nrow(mdata)`-long; measured 400 vs 380 → the length guard skipped). Worse, `reg_ame_if_maker()`'s
>   own `emp + delta` **recycled** — a wrong number with only a warning. Fixed by `reg_if_align()` in
>   `R/reg-influence.R`, over the extracted `svy_row_at()`: the padded rows carry design weight 0, so
>   scattering with zeros is exact. Verified against a hand-stacked computation to 1e-10.
>
> **Verified against `survey`, never a hard-coded number**: the proportion base equals
> `p(1−p)/SE(svyby(svymean))²` and the mean base `s²/SE²` to **1e-8** on a stratified + clustered
> design; the multinomial base matches `SE(svyby(~party, ~x))` cell by cell; the `Obs_OR` log-width
> equals `2z·√(Var(logit p₁)+Var(logit p₀))` to 1e-3, i.e. the Woolf bracket on an effective base **is**
> the delta-method design variance. Against a univariable `svyglm` it lands **2–7 %** out where the
> single-stage base was **15 %** out — and NOT always wider, which is the correction to §4.5's wording
> for a *ratio* (see §5.3). A grouped binomial's base is respondent-level, not `n × trials`.
>
> **Nothing else changed**: no `ci_settings` field and no legend degradation clause (maintainer's
> ruling) — after this, nothing falls back structurally, so such a clause could never fire; the residue
> is stated once in `?tab_reg` and the runtime `svy_var_degraded()` covers a genuine failure.

**Original plan, for the record:**

Region: `tab_reg.R`'s empirical block (`reg_empirical`, `emp_col`, `REG_EMPIRICAL`) + `reg-influence.R`,
then the four vignettes. Fresh session: a different large context from z14-ii, even though it reuses
z14-ii's idea.

**First commit point — the numbers.** `emp_n_ci` / `emp_n_draw` become design-based, from
`reg_crude_if_maker()` + `reg_if_se()`, **which already exist and already handle `postStrata`** (§5.2).
Every crude interval then follows for free, because they all consume one of those two bases — including
the Woolf `ci_or()`, which is fed effective *counts*. Closes §5.3: the printed crude interval and the
gap SE stop being two different variances for one estimator.

*Verification*: `Obs_OR` equals the design-based univariable `svyglm` interval within tolerance under a
design (today it is 2.3–2.6× too narrow with a segregated predictor); off-design byte-identical.

**Second commit point — the honesty.** The degradation matrix stated in one place and surfaced in the
legend: which (family × effect) combinations get a design-based crude column and which fall back
(§5.4 — `multinom` / `polr` have no design-based fitter). `ci_settings` must keep naming the method
actually used, since the legend reads it from there.

*Verification*: a fixture per degraded combination asserting the fallback is *stated*, not silent.
*Documentation*: `?tab_reg`, NEWS, the CLAUDE.md roadmap line.

**⚑ Split seam — the vignettes.** The last piece is cross-cutting by nature: it describes the finished
three-rung ladder, which only exists once all three subphases have landed, and it is a different kind of
work (prose in four documents, plus French). Do it here while the whole feature is in context; **if the
session is running long, stop at this seam and it becomes `z14-iiii`.**

Content: the "Weights" sections of both intro vignettes and both regression vignettes — the ladder of
§7.3, the **reachability check of §7.6** (*look for a stratum or cluster variable in your file, or for calibration variables; if there
is none, `kish_neff` is already all the correction available to you*), and the honest statement that
Route A is exact for a cell and conservative for a difference. Then `po/R-fr.po` + `msgfmt` for any new
runtime string.

---

### Cross-subphase notes

* **No fmt field, no column attribute, no colour-engine change** in any subphase. If one finds itself
  wanting one, that is the signal to stop and re-read §4.2.
* **Replicate designs are out** (Q5), so there is no `svrVar` code anywhere in z14 — only the clear
  refusal built in z14-i.
* **Suggests/Imports**: none added. `survey` is already an Import.
* **jamovi cache**: untouched. No design ever reaches a `.b.R` backend (§7.4), so no schema bump is
  needed anywhere in z14.
* **Test strategy throughout**: `survey` is the oracle. Every assertion is "tabxplor's interval equals
  `svyby` / `svyciprop` / `svyglm` on the same design to *x* digits", never a hard-coded number.

---

## 11. Decisions taken (2026-08-11)

**Q1. Is z14-i authorised on its own, now?** D1 and D2 make `tab_reg(design, empirical = TRUE)` and
`tab_reg(design, effect = "ame")` print numbers that answer a different question than the column header
claims. → **Yes.**

**Q2. Route A (design-based `n_eff`), or nothing?** Route A gets ~90 % of the value for ~10 % of a full
rewrite and cannot make the fmt record or the colour engine grow. → **Route A** (z14-ii).

**Q3. Is Route A's conservatism on the difference acceptable?** (§4.5: exact to within 3 % when the row
variable is segregated, 20–30 % too wide when it is spread.) → **Accepted; the row-to-row covariance is
NOT stored in the fields.** §4.6's call still produces it as a by-product, so this stays reopenable at no
cost paid now.

**Q4. `ids` / `strata` / `fpc` / `nest` — removal?** → **Removed** from `tab()` and `tab_reg()`, and
from their jamovi mirrors. The package's position becomes: *if you know how to use `survey::`, pass a
design object as `data`; otherwise use `wt` and, at best, `kish_neff`.* Lands in z14-i.

**Q5. Replicate designs (`svrepdesign`) — in or out?** → **OUT for now**, with a clear refusal pointing
at `svydesign()`. The explanation asked for is **§7.5**, in three lines: replicate weights are the
producer's way of releasing a design *as numbers* rather than as disclosive identifiers; **French
national producers do not use them** (verified on Trajectoires et Origines 2 — one calibrated weight
`poidsi`, design variables withheld to the CASD); the OECD assessments (PISA / PIAAC / TALIS, 80 Fay-BRR
replicates) do, and are therefore not served. Reopenable at ~40 lines (an `svrVar` branch in what is now
z14-ii) if that audience ever matters.

**Q6. Does the effect size become weighted under a design?** → **Yes, obviously** — it is a population
quantity sitting beside a design-based p-value. Lands in z14-i.

**Q7. What does the footer say?** → the proposed sentence, verbatim:
*"Design-based (survey): weighted estimates, intervals and tests account for the sample design."*,
replacing *"Weighted by X"* whenever a design is present and sitting beside the existing CI-method
clause. Lands in z14-ii, with the table it describes.

**Settled earlier, recorded here for completeness**: jamovi is out of scope — rungs 1–2 only, and
`jmvtabreg`'s survey-design block is deleted (§7.4).

---

## 12. References

* Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Wiley. — the linearization /
  influence-function framework `svyrecvar` implements.
* Lumley, T. `survey` package documentation: [`svyciprop`](https://search.r-project.org/CRAN/refmans/survey/html/svyciprop.html)
  (logit / likelihood / beta / asin / xlogit; *"all methods undercover for probabilities close enough
  to zero or one, but beta, likelihood and logit are noticeably better"*),
  [`svychisq`](https://rdrr.io/rforge/survey/man/svychisq.html) (first- and second-order Rao–Scott).
* Korn, E. L. & Graubard, B. I. (1998). Confidence intervals for proportions with small expected number
  of positive counts. *Survey Methodology* 24, 193–201. — the effective-sample-size device Route A
  generalises.
* Rao, J. N. K. & Scott, A. J. (1984). On chi-squared tests for multiway contingency tables with cell
  proportions estimated from survey data. *Annals of Statistics* 12, 46–60.
* Kish, L. (1965). *Survey Sampling*. Wiley. — `deff = 1 + CV²(w)`, and its stated scope (unequal
  weighting only).
* Deville, J.-C. & Särndal, C.-E. (1992). Calibration estimators in survey sampling. *JASA* 87, 376–382.
  — why calibration *reduces* variance while leaving `CV²(w)` untouched (§3.3).
* [PracTools, *Design Effects and Effective Sample Size*](https://cran.r-project.org/web/packages/PracTools/vignettes/Design-effects.html)
  — on Kish's deff not accounting for estimators more efficient than the weight variance implies.
* [Practical Significance, *How is the survey R package estimating covariances between domain
  estimates?*](https://www.practicalsignificance.com/posts/survey-covariances-using-influence-functions/)
  — `svyby(influence = TRUE)` + `svycontrast`, the oracle used throughout §4.
* Weesie, J. (1999); Mize, Doan & Long (2019) — seemingly-unrelated estimation, already cited by
  `reg-influence.R`; the same algebra underlies §5.2.
* In-repo: `dev/tabxplor_2.0.0_decisions.md` §14 (weighted estimate + unweighted n) and §51 (Last
  Phase j, the robust omnibus overlay); `dev/model_vs_observed_gap_test.md` §3 (the influence-function
  machinery this study reuses).
