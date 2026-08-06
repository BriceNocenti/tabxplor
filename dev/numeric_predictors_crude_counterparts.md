# Crude (`Obs_*`) counterparts for numeric predictors — design study

Date: 2026-08-05. Status: **REPORT ONLY** (Last Phase z7, item 2). No code written.

Scope: `tab_reg(..., empirical = TRUE)` builds an observed/crude companion column beside each model
effect — but **only for factor predictors**. Numeric predictors get `NA`. This report asks whether they
should get one, what it would be, and whether the maintainer's suspicion (that mixing factors and
numerics in one column brings "formatting white elephants") is the real obstacle.

Every number was measured on this box today, on `gss_simple` (n = 6819 complete cases) and simulations.

---

## 0. Executive summary

**Yes, it has a meaning, it is one rule rather than five, and the obstacle is not formatting — it is one
word.**

1. **The crude effect of a numeric predictor is the coefficient of the univariable model.** That is not
   a new concept invented for numerics: it is *exactly what tabxplor already computes for factors*.
   Verified to 1e-10 on every family (companion report §2.1): `Obs_OR` **is** `exp(coef(glm(y ~ x)))`,
   `Obs_diff` **is** `coef(lm(y ~ x))`, and so on. The hand-rolled cell sums are a closed form of the
   univariable fit, not a different quantity. §2.
2. **The CI method extends with no new method either.** Measured, saturated one-factor fits:

   | crude effect            | model-based SE | sandwich SE | the method tabxplor uses |
   |-------------------------|----------------|-------------|--------------------------|
   | log OR (Woolf)          | 0.074469       | 0.074469    | **0.074469** (identical) |
   | log RR (Katz)           | 0.054786       | 0.040842    | **0.040842** = sandwich  |
   | mean difference (t)     | 0.046915       | 0.047106    | **0.046915** = model     |
   | log IRR (quasi-Poisson) | 0.030373       | 0.030306    | φ-scaled model-based     |

   Every `REG_EMPIRICAL` CI literal is *the univariable fit's variance under the same rule the model
   column uses*. Katz **is** the sandwich (which is what z3's `rr` model column uses); Woolf **is** all
   three at once. So "fit the univariable model and take its interval" reproduces today's numbers for
   factors and defines them for numerics. §2.2.
3. **The real numbers are worth having.** `gss_simple`, `married ~ age + tvhours + race + rincome`:

   | predictor | crude OR/unit | model OR/unit | ratio  | per 10 units crude → model |
   |-----------|---------------|---------------|--------|----------------------------|
   | `age`     | 0.9693        | 0.9721        | ×1.003 | 0.732 → 0.753              |
   | `tvhours` | 1.0993        | 1.0585        | ×0.963 | **2.576 → 1.765**          |

   `tvhours` loses a third of its effect to adjustment. That is precisely the reading `empirical = TRUE`
   and `color = "adjustment"` exist to give, and today it is invisible on exactly the rows where
   confounding is usually strongest. §3.
4. **The formatting objection does not survive inspection — the model column already does this.**
   `Model_OR` already holds "vs the reference level" on factor rows and "per one unit" on numeric rows,
   in one column, and nobody has complained. The crude column *refusing* to is the inconsistency. The
   "numeric variables become whole columns" constraint the brief invokes is a **`tab()`** constraint
   (a numeric *col_var*); in `tab_reg()` a predictor is a **row**. §4.
5. **There is one genuine white elephant, and it is in the other column.** `Obs_%` / `Obs_mean` /
   `Obs_rate` (the *base* column) shows the outcome per predictor level. A numeric predictor has no
   levels, and the only well-defined descriptive analogue — `mean(X | Y)` — is **conditioned the other
   way round and measured in the predictor's units**. Putting it in the same column is the field
   overload the codebase's own rules forbid. **Recommendation: leave the base column `NA` and put `mean(X | Y)` in the html tooltip**, reusing `reg_empirical_tips` exactly as multinomial crude numbers already do. §5.
6. **And one honest semantic cost.** For a factor the univariable fit is *saturated*, so `Obs_OR` is a
   genuinely observed 2×2 contrast with no functional-form assumption. For a numeric it assumes
   **linearity on the link scale**. The column would then mix "observed" and "univariable model". The
   fix is a sentence, not a redesign: document the crude column as **"the unadjusted (univariable)
   effect — exactly the observed contrast when the predictor is categorical, a univariable slope when
   it is continuous"**. That single rule is *simpler* than the five per-family recipes the docs carry
   today. §6.
7. **Cost**: one 2-parameter fit per numeric predictor — 0.0084 s vs 0.0156 s for the full model at
   n = 6819 (54 % of one model fit each). Not free, not close to mattering. §7.
8. **It composes with the phase-z8 gap test for free.** Measured on `gss_simple`:
   `age` gap z = +5.32 (p = 1e-07), `tvhours` gap z = −7.66 (p = 2e-14). The univariable fit yields an
   influence function through the same `if_glm()`; no extra machinery. §8.
9. **One integration wrinkle to decide**: Last Phase s made the factor crude CIs honour
   `options(tabxplor.kish_neff)`. A univariable *fit* has no raw n to swap, and under weights the
   design-based SE is available and better. Measured (deff 1.43): design-based 0.002254, Kish-rescaled
   0.002060, naive 0.001721 — an 9 % within-column inconsistency if the two row kinds use different
   rules. §9.

**Recommendation: do it**, effect column only, base column `NA` + tooltip, one documented rule. It
removes a blank the user notices, it makes `color = "adjustment"` work on the rows where adjustment
usually bites, and it *reduces* the number of concepts in the docs.

---

## 1. What is there today

```
  var      levels               Obs_%  Obs_OR  Model_OR
1 Constant Reference population   NA     NA      0.72
2 race     White                 49%    1.00    1.00
3 race     Black                 26%    0.36    0.43
4 race     Other                 46%    0.88    0.98
5 age      age                    NA     NA      1.01
6 tvhours  tvhours                NA     NA      0.91
```

`reg_empirical()` takes `fac_preds` — factors and characters only (`R/tab_reg.R:838`, `:2405`) — and
`reg_empirical_columns()` gates every cell on `is_fac <- skeleton$var %in% fac_preds` (`:1255`). Numeric
rows are blank in **both** crude columns. The user sees a table that promises a crude/model comparison
and delivers it for some rows.

---

## 2. The crude effect of a numeric predictor is not a new concept

### 2.1 For a factor, "observed" already *is* "univariable"

The companion report (`dev/model_vs_observed_gap_test.md` §2.1) measured this to settle the gap test,
and it settles this question too:

| crude column                     | equals                          | max abs. difference |
|----------------------------------|---------------------------------|---------------------|
| `Obs_OR` (binomial)              | `exp(coef(glm(y ~ x, binomial)))` | 9.2e-14           |
| `Obs_RR` (`rr`, Katz)            | `exp(coef(glm(y ~ x, poisson)))`  | 1.8e-10           |
| `Obs_diff` (gaussian)            | `coef(lm(y ~ x))`                 | 1.3e-13           |
| `Obs_IRR` (poisson counts)       | `exp(coef(glm(y ~ x, poisson)))`  | 3.7e-10           |
| `Obs_%` risk difference (`ame`)  | `coef(glm(y ~ x, gaussian))`      | 1.7e-14           |
| weighted variants                | the weighted fits                 | ≤ 9.8e-15         |

A one-factor GLM is **saturated**, so its coefficients *are* the raw cell contrasts. tabxplor computes
them from weighted cell sums because that is faster and gives exact closed-form intervals — not because
it is estimating something else.

**Therefore the rule "the crude effect is the univariable model's effect" is already the rule.** It
simply has a fast path for the case where the univariable model is saturated. Extending it to numeric
predictors is not a generalisation of the concept; it is removing a special case from the
implementation.

### 2.2 The CI method extends too

Simulated n = 3000, two-level predictor, deliberately heteroskedastic on the gaussian arm:

| crude effect            | model-based SE | sandwich SE | tabxplor's `REG_EMPIRICAL` method |
|-------------------------|----------------|-------------|-----------------------------------|
| log OR                  | 0.074469       | 0.074469    | Woolf **0.074469**                |
| log RR                  | 0.054786       | 0.040842    | Katz **0.040842**                 |
| mean difference         | 0.046915       | 0.047106    | pooled Student **0.046915**       |
| log IRR                 | 0.030373       | 0.030306    | quasi-Poisson (φ-scaled model)    |

Read the second row carefully: **Katz is the *sandwich* variance of the saturated log-link fit**, not
its model-based one — and the sandwich is exactly what z3's `rr` *model* column uses (it always fits
through `svyglm`). And in the first row all three coincide, because a saturated model cannot be
misspecified.

So each family's crude interval is already "the univariable fit's interval under the same variance rule
as the model column". For a numeric predictor the same rule gives:

| family              | numeric crude effect                          | its interval                     |
|---------------------|-----------------------------------------------|----------------------------------|
| `binomial`          | `exp(β)` from `glm(y ~ x, binomial)`          | Wald (= Woolf's generalisation)  |
| `rr`                | `exp(β)` from the modified-Poisson univariable | sandwich (= Katz's generalisation) |
| `gaussian`          | `β` from `lm(y ~ x)` — the slope              | Student t (= the pooled t)       |
| `poisson`           | `exp(β)`                                       | quasi-Poisson                    |
| `effect = "ame"`    | the univariable AME (average slope of the risk) | as the model column             |
| `effect="ame_ratio"`| the univariable marginal risk ratio            | as the model column             |
| `exponentiate=FALSE`| the same on the log scale (`Obs_log(OR)` etc.) | same                             |

**No new CI function, no new `REG_EMPIRICAL` row, no per-family branch.** The fact table already
declares everything needed; what changes is that `reg_empirical()` gains a second producer for the
rows it currently skips.

---

## 3. Is it worth having? Measured on real data

`gss_simple`, `married ~ age + tvhours + race + rincome`, complete cases n = 6819:

| predictor | crude OR / unit | 95 % CI          | model OR / unit | ratio  | per 10 units: crude → model |
|-----------|-----------------|------------------|-----------------|--------|------------------------------|
| `age`     | 0.9693          | [0.9657; 0.9729] | 0.9721          | ×1.003 | 0.732 → 0.753                |
| `tvhours` | 1.0993          | [1.0724; 1.1268] | 1.0585          | ×0.963 | **2.576 → 1.765**            |

Per unit the numbers look tame; per 10 units `tvhours` drops from ×2.58 to ×1.77 — **a third of the
association is confounded away**, and today the table renders that row's crude cell blank.

This is the argument that matters. Continuous predictors — age above all — are usually the *most*
confounded rows in a sociological model, and they are the rows the feature currently cannot show. A
`color = "adjustment"` table that is systematically blank exactly where adjustment matters most is a
worse product than one with no crude column at all, because the blank reads as "nothing happened here".

---

## 4. The formatting objection, examined

The brief asks whether the rationale for *not* doing it is that "mixing factors and numerics on the
same column will bring formatting white elephants, because the whole framework is made to treat column
numeric variables as full columns".

**Two different constraints are being conflated, and neither applies.**

- **"Numeric variables become whole columns" is a `tab()` fact, not a `tab_reg()` one.** In `tab()` a
  numeric *col_var* becomes a column of means (`tab_num`), because the column is the axis. In
  `tab_reg()` a predictor is a **row**; the columns are outcomes/models. There is no rule to break.
- **The heterogeneity already exists and is already accepted.** `Model_OR` holds "level vs reference"
  on factor rows and "per one unit" on numeric rows, in one column, with one header. That convention
  shipped in Phase 12c and nobody has objected. A crude column that refuses the same heterogeneity is
  not protecting an invariant — it is breaking the symmetry with the column beside it.

What *is* real, and small:

| worry                       | verdict                                                                                                                                                     |
|-----------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| unit clash in the cell      | **None.** An OR is an OR; `Obs_OR` digits = 2 suits both. Gaussian `Obs_diff` is "in Y units" on both row kinds (per level / per unit of X).                 |
| the reference row           | **None.** Numerics have no reference level, so no neutral `1.00` row — exactly as `Model_OR` already behaves. `in_refrow` machinery untouched.               |
| colour                      | A per-1-unit OR of 0.969 sits inside the first break and never colours; per 10 units (0.73) does. **Already true of the model column**; `multiplier` is the user's lever. One sentence in the docs. |
| the *base* column           | **This one is real** — §5.                                                                                                                                  |

---

## 5. The base column is where the white elephant actually lives

`Obs_%` / `Obs_mean` / `Obs_rate` answer *"what is the outcome, within each level of the predictor?"*
A numeric predictor has no levels, so the question has no answer without inventing one.

The maintainer's instinct — "wouldn't the right crude twin be a simple mean computable with
`tab_num()`?" — points at the genuinely well-defined quantity, but it is **conditioned the other way
round**. `tab_num()` already produces it:

```
  married             age    tvhours
1 01-Married     45 (σ12) 2.4 (σ1.8)
2 02-Not married 40 (σ14) 2.7 (σ2.3)
3 Total          42 (σ13) 2.6 (σ2.1)
```

That is `mean(X | Y)`; the base column shows `P(Y | X)`. For a factor they are the row% and col% views
of one cross-tab; for a numeric only the second exists. Putting a mean age (in years) into a column
whose other cells are outcome percentages is precisely the field-overload the 2.0.0 rewrite removed.

**Four options, ranked:**

| # | option                                                       | verdict                                                                                                                                      |
|---|--------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | **base cell stays `NA`; `mean(X \| Y)` goes in the tooltip** | **Recommended.** `reg_empirical_tips` + `empirical_tips` already carry exactly this kind of "too rich for a cell" crude number for multinomial. Zero layout cost, zero unit clash, and the number is where a curious reader looks. |
| 2 | base cell stays `NA`, nothing else                           | Honest and free. Loses a genuinely useful descriptive.                                                                                       |
| 3 | put `mean(X \| Y)` in the base column                        | **Reject.** Two quantities, two units, two conditioning directions in one column.                                                            |
| 4 | bin the numeric and show `P(Y \| bin)`                       | **Reject.** tabxplor must not choose a cut-point silently. The user's own `cut()` is the honest route and is often the better analysis anyway — worth one vignette line. |

---

## 6. The one honest semantic cost

For a **factor**, the univariable model is saturated: `Obs_OR` carries no functional-form assumption; it
is the observed 2×2 contrast, and the word "observed" is literally true.

For a **numeric**, the univariable coefficient assumes **linearity on the link scale**. A U-shaped
relation can give a crude slope near zero while the variable matters enormously. So the column would
mix "a description" with "a one-predictor model", under a prefix (`Obs_`, for *observed*) that promises
the former.

This is the only real cost of the feature, and the remedy is a definition, not a redesign:

> **The `Obs_*` columns show the *unadjusted* (univariable) effect: with a categorical predictor that
> is exactly the observed contrast between levels; with a continuous predictor it is the univariable
> slope, which assumes the effect is linear on the model's scale.**

Note that this sentence *replaces* five per-family recipes in the current documentation with one rule
— so the docs get shorter, not longer. It also matches the vocabulary of the two R packages users will
compare tabxplor with: **finalfit** labels the column *"OR (univariable)"* and **gtsummary**'s
`tbl_uvregression()` calls it *unadjusted*; both apply it to continuous and categorical predictors
without distinction, and both have done so for years without the world ending.

(Renaming the prefix `Obs_` → `Unadj_` is *not* recommended: it shipped in Last Phase g, it is the right
word for the 95 % case, and "unadjusted" is longer in every column header.)

---

## 7. Cost

`gss_simple`, n = 6819:

```
one univariable glm (2 parameters)      0.0084 s     (54 % of one full-model fit)
the full model (18 parameters)          0.0156 s
one univariable lm (gaussian)           0.0011 s
```

So *k* numeric predictors cost roughly *k*/2 extra model fits — 25 ms for three numerics on a
21 000-row survey. Compare: the factor crude effects stay closed-form and free (companion report §2.2
measured the closed-form influence function at 21× cheaper than fitting), so the total crude block
remains dominated by the model itself.

Two implementation notes that keep it there: the gaussian arm has a closed form too (slope = cov/var,
0.0011 s), and under a survey design the univariable fit must go through `svyglm` with the *same*
design object the model uses — which is one `reg_make_design()` call already in hand.

---

## 8. It composes with the phase-z8 gap test at zero cost

The companion report's whole machinery is "two M-estimators on the same rows". A univariable fit is an
M-estimator like any other, so `color = "adjustment"` + its significance test extend to numeric rows
with **no new code at all**. Measured on `gss_simple`:

| predictor | crude → model     | gap ± SE          | z     | p       |
|-----------|-------------------|-------------------|-------|---------|
| `age`     | 0.9693 → 0.9721   | +0.00288 ± 0.00054| +5.32 | 1.1e-07 |
| `tvhours` | 1.0993 → 1.0585   | −0.03782 ± 0.00494| −7.66 | 1.9e-14 |

Both highly significant, and `tvhours` is the substantively interesting one. Note this also **closes the
z5 §8 `multiplier` flag properly**: today the flag is moot (no numeric crude exists, so `obs` is `NA`
and nothing is scored); with this feature it becomes live, and the crude effect **must be raised to the
same power k** as the model coefficient or the whole comparison is an artefact of the scaling argument.
One line, but a mandatory one, and it needs a fixture.

---

## 9. The weighting wrinkle to decide

Last Phase s made the factor crude CIs honour `options(tabxplor.kish_neff = TRUE)`: they swap the raw n
for Kish's `n_eff`. A univariable **fit** has no n to swap. Measured on `gss_simple` with simulated
weights (design effect 1.43):

```
design-based SE (svyglm)               0.002254
naive weighted glm SE                  0.001721
naive x sqrt(n / n_eff)  (Kish rescale) 0.002060
```

The Kish rescale recovers most of the design effect (it is the same first-order approximation
`tab_robust_overlay()` already uses under `"kish"`), but it is **9 % below** the design-based value. So
under `kish_neff = TRUE` on weighted data, a single crude column would mix Kish-rescaled factor rows
with either naive or design-based numeric rows.

Three coherent resolutions:

1. **Kish-rescale the numeric fit's SE too** (`se × sqrt(n / n_eff)`) — within-column consistency, uses
   the package's own existing idiom, one multiplication. **Recommended.**
2. **Design-based for numerics** (`svyglm` with the model's design) — more rigorous per row, less
   consistent per column, and it would make the numeric rows' intervals *wider* than comparable factor
   rows for no reason a reader could see.
3. Document the split. Cheapest, weakest.

---

## 10. Ideas deliberately not recommended

- **A separate `Obs_slope` column** beside `Obs_OR`. Two effect columns per model doubles the width to
  express one thing.
- **Auto-standardising numeric predictors** (per SD) so the colour scale bites. It would change the
  *model* column too, silently. `multiplier` is the explicit lever and should stay the only one.
- **Guessing a binning** for the base column (§5, option 4).
- **A `empirical = "factors"` / `"all"` knob.** The whole point is that there is one rule. Adding a
  switch would re-import the special case the feature exists to delete. (The change *is* user-visible:
  cells that were blank become filled. That is additive, pre-release, and desirable.)

---

## 11. Open questions for the maintainer

- **Q1 — ship it?** §3 and §4 say yes; the cost is one fit per numeric predictor and one documented
  sentence. Recommended.
  **Maintainer’s decision: ship it**
- **Q2 — the base column.** `NA` + tooltip (recommended, §5 option 1), `NA` alone, or something else?
  **Maintainer’s decision: more study is needed here.** Not having the same level of polish for numeric predictors compared to factors would be a no-go for many regressions users. Since tabxplor framework allows different `display` fields on the same column, it’s actually possible to print both percentages and means on the same column, the user can differenciate percentages with the % symbol, every numeric predictor is on it’s own scale anyway, etc. The question is: what degraded features or white elephants would such a decision introduce (a column can only have on `type`, one `color`, etc.) ? Colors breaks and scale ? Would there be a way to use something inspired from the `color = c("diff", "ratio")` syntax, first value being text color, second value being background color, to use two scales for one column, with only text color for factors, only background colors for numeric ? Would it be reliable, and how to adapt it to the use case ? Or would the colors be wrong anyway due to other column attributes like type ?
- **Q3 — the wording.** Adopt the single rule of §6 — "`Obs_*` = the unadjusted (univariable) effect;
  exactly observed for a categorical predictor, a univariable slope for a continuous one" — in
  `?tab_reg`, the regression vignette (EN + FR) and the legend? Recommended; it shortens the docs.
  **Maintainer’s decision: couln’t quantitative social sciences colleagues agree that a raw mean on a numeric variable, or ratio of means, or difference of means, is actually "observed", even if "non-linearity on the link scale" make the whole observed mean a bit meaningless, and user shall anyway test every numeric variable for linearity and other assumptions before keeping it in a regression model (proof : it could be computed via `tab()` without model fit ?) ? So mix vocabulary to signify it’s the same, observed, unadjusted, univariable ? Or would it be a no-go for statistical colleagues ?**
- **Q4 — weighting** (§9): Kish-rescale the numeric crude SE (recommended), go design-based, or
  document the split?
  **Maintainer’s decision: Kish-rescale the numeric crude SE**
- **Q5 — order.** This is largely independent of the z8 gap test, but they share `reg_empirical`'s
  producer and the `multiplier` rule. Ship this **first** (it is smaller and it is a visible gap in a
  headline feature), or after?
  **Maintainer’s decision: z8 gap test was already introduced for factors, but postponed for numeric predictors. Implement it now.**
- **Q6 — the `multiplier` rule** (§8): confirm that the numeric crude effect is raised to the same
  power k as the model's. There is no defensible alternative, but it must be a conscious fixture.
  **Maintainer’s decision: the numeric crude effect obviously shall be raised at the same power for comparison. In every possible case, the empirical counterpart should match exactly the modelised effects, otherwise the whole observed versus modelised comparison is meaningless.**
