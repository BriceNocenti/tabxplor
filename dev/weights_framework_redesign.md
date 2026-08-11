# Weights, reorganised — one variance, three labelled positions, no hidden approximations

Date: 2026-08-11. Status: **DESIGN ONLY** — no R code was modified (a parallel session holds the
tree). Working tree at `cf4d994`, `survey` 4.5, R 4.6.1. Every number in this document is produced by
the scripts in **Appendix B**, which are new and independent of the stress test's Appendix A.

**Read `dev/weights_framework_stress_test.md` first** — it is the diagnosis (findings W1–W13, the
inventory of what each rung actually reaches). This document is the *cure*: a full reorganisation of
the weights subsystem, incorporating the maintainer's rulings of 2026-08-11 (§5.1).

Companion documents: `dev/full_survey_design_scope.md` (the z14 study — Route A and its
measurements), `dev/tabxplor_2.0.0_decisions.md` §14 (the weighted-inference convention) and §51
(the robust tests), `dev/chi2_cell_residuals_and_contributions.md` (the `contrib` measure).

---

## 0. The one-page answer

**The missing key is one sentence: a weight column *is* a survey design** — the flat one,
`ids = ~1`. And under that design every quantity tabxplor displays has an **exact closed-form
variance in the per-cell `Σw²` the aggregate core can compute in the same pass as `Σw`**.

That single fact reorganises everything, because it says the framework was never three things:

| what the code has today | what it actually is |
|---|---|
| rung 2 = "Kish's effective sample size", its own implementation, its own tests, its own jamovi selector, its own hand-rolled Rao-Scott chi² and hand-rolled weighted ANOVA | **the same formula as rung 3, evaluated at `ids = ~1`** — with the cell's own `Σw²` thrown away and replaced by its proportional share, which is exactly the assumption Kish's derivation makes and real weights violate |
| rung 3 = "the design-based rung", needing `survey::svyrecvar`, an influence matrix `O(rows × n)`, a 400 MB ceiling and a microdata scan | **still that** — but only when the design has structure. A flat design has an algebraic solution |
| "the model column has always silently been at a higher rung" (W2) | it was at **rung 2**, because `svyglm` at `ids = ~1` *is* the closed form. There was never a discrepancy to explain — only an approximation on the other side |

Measured, on five independent checks (§1.2, Appendix B): the closed form reproduces `survey`
**exactly** — ratio `1.000000` — for row / column / grand-total percentages, for means, for total
rows and subtable domains, for the Woolf `Obs_OR` bracket **against a univariable `svyglm`**, and —
the result that decides the architecture — for `svychisq(statistic = "F")`, the full Rao-Scott
second-order design χ², `F` ratio `1.0000000000` and `ndf` ratio `1.0000000000`.

**So the weighted rung stops needing the microdata at all.** It becomes a property of the aggregate
core: `O(cells)`, composing with `.fine`, the jamovi tier-1/2 cache and the parallel row axis, with
no size ceiling and nothing that can silently degrade.

### What the reorganisation is

| | today | after |
|---|---|---|
| **concepts** | 3 rungs × 2 functions × 2 leaves, each with its own encoding | **one variance definition**, two implementations of it (closed form / `svyrecvar`), selected by **one stored fact** |
| **the fact** | computed by `svy_inference_mode()` and thrown away; re-derived downstream by sniffing the string `".svy_weights"` | `meta$inference$basis`, stored, read by the footer, the tests, jamovi and `tab_reg()` |
| **the base** | `n_eff` written by the factor leaf as a property of the cell, by the numeric leaf as a by-product of a CI (W13) | `n_eff` always a property of the cell, written whenever the basis is not `"n"` |
| **the critical value** | `z` for proportions, `t(n_eff − 1)` for means, `degf(design)` never consulted (W7) | one `conf_level_to_crit(conf_level, df)`, `df` from the design |
| **the omnibus test** | 4 discriminators, 2 of them hand-rolled statistics on a microdata rescan | 2 discriminators; the weighted one is exact and comes from the aggregate |
| **`contrib` under weights** | design-corrected on a counts table, not on a percentage table (W3) | one per-cell exact residual, `pct`-independent by construction |
| **knobs** | `options(tabxplor.kish_neff)` — reaches `tab()` and `tab_reg()` differently, mislabelled in jamovi | **one option, `tab()`-only**, named for what it does, and *visible on the table* |
| **honesty** | a degraded design keeps claiming the design forever, in every export (W4) | the footer is generated from the stored basis; the claim cannot outlive the computation |

### What dies

`svy_omnibus_one()`'s entire `mode == "kish"` block (~35 lines of hand-rolled first-order Rao-Scott
and hand-rolled weighted ANOVA) · `leaf_wide_pct()`'s Kish `Ne` arm · `num_core()`'s Kish `_en`
branch · `num_moment_scan()`'s stray `getOption()` · `reg_empirical()`'s `neff_or_n()` Kish arm ·
`tab_weight_line()`'s `".svy_weights"` string sniff · `chi2_write_contrib()`'s
`type %in% c("n","all","all_tabs")` guess and its Total-column `n_eff` read · three `weighted <-`
predicates · two redundant `design_on` conjunctions · two of the four test discriminators ·
`tab_robust_overlay()`'s `mode` argument · the jamovi `test_robust` two-value selector.

Net: **≈150 lines deleted, ≈130 added**, and the mental model goes from "three rungs, and the two
functions climb them differently" to "**your estimate is weighted; your interval says which design
you gave me**".

---

## 1. The missing key: a weight column is a survey design

### 1.1 The closed form

Every quantity `tab()` displays is a ratio of two weighted sums, `p̂ = A/B` with `A = Σ u_k w_k` and
`B = Σ v_k w_k`, whose linearized influence contribution is `z_k = (u_k − p v_k)/B` — this is already
the one formula `R/survey-variance.R` is built on (`svy_uv_v()`: *"`pct` chooses `(u, v)`, it does
not choose a formula"*).

At `ids = ~1`, with no strata, no `fpc` and no calibration, `svyrecvar` reduces to a plain sum of
squares of `w_k z_k` with survey's finite-sample factor `n/(n−1)`; and because `Σ w_k z_k = (A − p B)/B`
is **exactly zero**, the centering `onestrat()` applies is a no-op. So, writing

* `A` = the cell's own `Σw²`,
* `S` = `Σw²` over the **base's own domain** (the same `Dmat()` selector `leaf_wide_pct()` already
  applies to `Σw`),
* `B` = `Σw` over that same domain,

the variance is, for a proportion and for a mean:

```
Var_design(p̂)  =  n/(n−1) · [ A·(1−p)² + (S − A)·p² ] / B²

Var_design(x̄)  =  n/(n−1) · [ Σw²x² − 2·x̄·Σw²x + x̄²·Σw² ] / B²
```

and the base every tabxplor engine already reads is Korn & Graubard's own device, unchanged:

```
n_eff  =  p(1−p) / Var_design(p̂)          n_eff  =  s² / Var_design(x̄)
```

**Four inputs, three of which already exist.** `A` is `tabs_w2`, accumulated today for Kish. `S` is
the same `Dmat` broadcast applied to `tabs_w2` instead of `tabs_wn`. `B` is `tabs_wn`. `n` is the
leaf's row count. For means, `num_moment_scan()` already accumulates `Σw`, `Σwx`, `Σwx²` and (on
Kish) `Σw²`; it needs two more sums in the same pass, `Σw²x` and `Σw²x²`.

**`Σw²` is additive across a partition** (verified to 2.3e-13, Appendix B block T), exactly as `Σw`
is — so the wide-table rollup, the `Total` column and the total rows all get the right `A`, `S`, `B`
by summation, with no special case. Total rows therefore carry an exact design base, which matters
because `ref = "tot"` makes them the reference of every difference in the table.

### 1.2 What becomes exact

| quantity | oracle | ratio |
|---|---|---|
| cell %, `pct = "row"` / `"col"` / `"all"` | `svyby(svymean)` / `svymean(~interaction)` | `1 1 1 1` each |
| cell mean | `svyby(svymean)` | `1 1 1 1` |
| a **total row** (domain = every level) | `svyby(svymean)` | `1` |
| the **total table** row | `svymean` | `1` |
| a cell inside a `tab_vars` subtable, `pct = "col"` | `svymean` on the subset design | `1` |
| `Obs_OR` bracket (Woolf on effective counts) | **univariable `svyglm`** SE(log OR) | `1 1 1` |
| `Obs_diff` bracket | `svycontrast` on the two domain means | `1 1 1` |
| **whole-table χ²** | **`svychisq(statistic = "F")`** | `F` `1.0000000000`, `ndf` `1.0000000000`, on 5 shapes |
| whole-table F, means | `svyglm` + `regTermTest` Wald | `1.00000000` |

The last two lines are the architectural result. The full **cell covariance matrix** at `ids = ~1`
also has a closed form in the same per-cell sums,

```
Cov(p̂_a, p̂_b)  =  n/(n−1) · [ δ_ab·A_a − p_a·A_b − p_b·A_a + p_a·p_b·S ] / B²
```

verified against `vcov(svymean(~interaction(row, col)))` to `1.6e-15` relative (block C). Since
`survey::svychisq` computes its Rao-Scott adjustment from exactly `(mean2, attr(mean2,"var"), N,
degf)`, **the design-based χ² of a weights-only table is reproducible from the aggregate, exactly**
— and the same is true of the numeric side, where the Wald χ²/(k−1) on the per-group design
variances *is* `svyglm`'s F to 8 decimals.

### 1.3 Kish was never a rung — it is this formula with one input discarded

Write `A ≈ p·S` (the cell's `Σw²` is its proportional share of the base's) and the bracket collapses:

```
A(1−p)² + (S−A)p²   →   pS(1−p)² + S(1−p)p²  =  S·p(1−p)
   ⇒   n_eff   →   (n−1)/n · B²/S            =   Kish, up to the finite-sample factor
```

So **Kish is the closed form under the assumption that the weights carry no information about the
cell** — which is the assumption the literature states ("the formula holds when all observations are
approximately uncorrelated and have the same variance for the response"; "not a good approximation
when weights are post-stratified or calibrated to known totals" — the normal case in French survey
practice). The stress test measured the consequence directly: with the outcome following the weight,
Kish is up to 17 % wrong in either direction; the closed form is exact by construction.

Measured on one 4-level crosstab (block N):

```
        raw n     Kish    EXACT   exact/Kish
A        1619  1183.27  1097.68        0.93
B        1190   866.53   855.46        0.99
C         789   558.97   597.03        1.07
D         402   292.55   327.92        1.12
```

Kish cannot move with the outcome — it is a property of the weights alone, so the same four numbers
appear whatever is being tabulated. The exact base moves, in both directions.

**The practical consequence for `tab_reg()`**: at the flat design, two disjoint row domains share no
cluster, so their estimates are **independent** and the covariance term Route A discards (ruling Q3)
is *exactly zero*. The Woolf bracket on exact effective counts is therefore precisely the delta-method
SE of the log odds ratio — which is why the `Obs_OR` column lands on the univariable `svyglm` answer
to ratio `1`, where Kish lands at `0.98–1.05` and the raw n at `0.83`.

### 1.4 Why "weighted estimate, unweighted n" is a *position*, not a rung

There are exactly three coherent readings of a weight column, and the package's default is none of
them:

| reading | correct `n` for the interval | who uses it |
|---|---|---|
| **frequency** weights (each row stands for `w` identical observations) | `Σw` | SPSS's default; `glm(weights=)` |
| **sampling** weights, design unknown | the flat-design `n_eff` (§1.1) | `survey` at `ids = ~1`; `tab_reg()`, always |
| **sampling** weights, design known | `svyrecvar`'s `n_eff` | `survey` with a real design |
| *— tabxplor's default —* | the **raw n** | corresponds to no probability model: the point estimate comes from one, the variance from another |

`?tab` is already honest about this ("the default interval is **too narrow**"). What the
reorganisation adds is that the table itself will now **say so** (§3.3): the position stops being
silent. That is the whole of what the maintainer's ruling (§5.1 Q1) requires, and it converts an
inconsistency into an explicit, labelled, teachable choice.

The magnitude, so the vignette can state it: the CI half-width multiplier is `√deff`, i.e.
`√(n/n_eff)`. On the fixtures here that is **×1.11 to ×1.21** — enough that stars disappear. For a
national file with a calibrated weight of `CV ≈ 0.5`, expect `×1.12`.

---

## 2. The target architecture

### 2.1 ONE fact: the inference basis

`wt` says **how the estimate is computed**. A new, *orthogonal* fact says **how the interval is
computed**. The two are independent, which is why the framework kept needing four encodings of one
thing — there was no slot for the second.

```r
meta$inference = list(
  basis = "n" | "weights" | "design" | "design_partial",
  degf  = <numeric>,            # the design's degrees of freedom (§2.4)
  note  = <short reason, only when basis == "design_partial">
)
```

absent when `basis == "n"` **and** the table is unweighted — so the "absent when unset" rule of
Phase 17b holds and no unweighted golden moves.

| basis | what the intervals carry | reached by |
|---|---|---|
| `"n"` | the raw sample size — no design effect | unweighted; **or** weighted with `tab()`'s option off (the default) |
| `"weights"` | the design effect of the weights, exactly (§1.1) | `wt` + `options(tabxplor.design_effect = TRUE)`; **always** in `tab_reg()` when weighted |
| `"design"` | the full design: strata, clusters, `fpc`, calibration | a `survey::svydesign` passed as `data` |
| `"design_partial"` | the weights only — a design was given but its variance could not be computed here | the `svyrecvar` producer returned `NULL` (§2.2) |

`svy_inference_mode()` becomes `svy_inference_basis()`, returns these four values, and is **the only
place the option or the design object is read**. Every consumer takes the resolved basis:
`reg_fit()`'s `weighted`, `reg_resolve_multiplier()`'s caller, `reg_empirical()`, `tab_counts()`,
both leaves, `num_moment_scan()`, `tab_robust_overlay()`, `tab_weight_line()`.

Note what `"design_partial"` buys, beyond honesty: the fallback is no longer "throw everything away
and use the raw n". A design whose `svyrecvar` pass fails still has weights, and the closed form
still applies to them — so the degrade ladder becomes **design → weights → n**, each step labelled.

### 2.2 ONE variance, two implementations

One definition — `n_eff = p(1−p)/Var_design` — with the implementation selected by the basis:

```
basis "weights"  ->  svy_flat_var_prop() / svy_flat_var_mean()    closed form, O(cells), on the
                                                                  wide Σw / Σw² matrices
basis "design"   ->  svy_var_prop()      / svy_var_mean()          svyrecvar, O(rows × n), microdata
                     (falls back to the flat form, and records "design_partial")
```

Both live in `R/survey-variance.R`, in two sections under one header — because they are one concept,
and a second module would invite the belief that they are two. The header states the boundary that
must never be crossed: **the closed form is the `ids = ~1` case and nothing else.** The rejection of
Route C (a PSU-augmented aggregate, `dev/full_survey_design_scope.md` §4.8) stands for real designs
and does not apply here, precisely because at `ids = ~1` there is no `svyrecvar` to re-implement, no
lonely-PSU policy, no multistage `fpc` and no calibration.

Two structural consequences:

* **`leaf_wide_pct()` loses its `tabs_w2` arm.** It computes percentages and `tot_n`; the variance
  module computes variances. The leaf calls one producer instead of carrying a second meaning.
* **A flat `svydesign` routes to the closed form.** `svydesign(ids = ~1, weights = ~w)` — the shape
  most users build — is detected (`svy_design_is_flat()`: no strata, no `fpc`, no `postStrata`,
  single-stage) and takes the algebraic path: same answer, no influence matrix, no 400 MB ceiling.

### 2.3 ONE base: `n_eff` is a property of the cell

The 21-field record does not change. What changes is *when* `n_eff` is written: **whenever the basis
is not `"n"`**, unconditionally — not, as the numeric leaf does today, only inside
`if (ci %in% c("cell","diff"))`.

That asymmetry is the whole of W13: the factor leaf treats `n_eff` as a base, the numeric leaf as a
by-product of having been asked for a CI, so `tab_num(design) |> tab_ci("cell")` silently returns
intervals **1.6× too narrow** while `tab_plain(design) |> tab_ci("cell")` returns design-based ones —
and the footer claims the design in both. `n_eff` is documented as *"the effective sample size used
for this cell's confidence interval"*; a base is a property of the cell, so the factor leaf is right
and the fix is to make the numeric leaf agree.

**Degenerate cells.** `p(1−p)/Var` is `0/0` at `p = 0` and `p = 1` (both give `Var = 0`), so those
cells have no base of their own — a property of Korn & Graubard's device that rung 3 already has. The
fallback is now free and defensible: use the **base domain's** `B²/S`, which is the exact effective
n of a quantity carrying no information about the weights, i.e. the right limit for a cell that
carries no information at all. Measured (block T): a `p = 0` cell in a 2435-row domain gets 1342.5
instead of falling back to 2435. One line, both bases, and Kish's formula survives only as what it
actually is — a degenerate-case limit, never a rung.

### 2.4 ONE critical value

```r
conf_level_to_crit(conf_level, df = Inf)      # qt(); -> qnorm() at df = Inf
conf_level_to_z(conf_level)                   # kept, exported since z4, now a one-line alias
```

Every interval takes its quantile from the design's degrees of freedom — `n − 1` at the flat design
(so `≈ z`, invisible), `#PSU − #strata` under a real one. This threads W7 in one function instead of
at nine call sites, and it covers the proportion engines too (Wilson / Newcombe / Katz / Woolf),
which are `z`-based by construction: substituting `t(degf)` for `z` is `survey`'s own `xlogit` idiom,
not an invention. Measured error today at 10 PSUs: **−15 %** on a proportion interval, **−6.8 %** on
a mean.

Korn–Graubard ships as an explicit `method_cell = "beta"` (`survey::svyciprop(method = "beta")`'s
interval, defined on exactly the `n_eff` this framework already computes), **not** as a default —
one interval *shape* at every position keeps the legend, the goldens and cross-table comparability
one story (§5.1 Q4).

### 2.5 ONE test rule: the omnibus comes from the same variance as the cells

| basis | factor | numeric |
|---|---|---|
| `"n"` | Pearson χ² on the weighted proportions rescaled to the raw n (**unchanged**) | Welch / classic F (**unchanged**) |
| `"weights"` | **exact Rao-Scott second-order F**, from the aggregate (`= svychisq`, ratio 1.0000000000) | **Wald F on the closed-form design variances** (`= svyglm + regTermTest`, ratio 1.00000000) |
| `"design"` | `survey::svychisq` (unchanged) | `svyglm` + `regTermTest` (unchanged) |

Two discriminators replace four: **`chi2` / `chi2_design`** and **`F_welch` / `F_classic` /
`F_design`** — because the flat and the full design run the *same estimator*, verified, so labelling
them differently would be a second encoding of the basis, which `meta$inference` already stores.

`tab_robust_overlay()` — the one architectural exception to "the test comes from the aggregate" —
shrinks to the `"design"` case only, and loses its `mode` argument. `svy_omnibus_one()`'s `kish`
block goes away entirely; it is not replaced, because the aggregate produces a better answer than the
statistics it hand-rolled.

`test$n` becomes **always the raw count** (W8: at rung 2 it silently became the effective n), and the
`test` tibble gains **`deff`** — the design effect, at exactly its natural grain, one per
(subtable × col_var), `NA` when the basis is `"n"`.

**Cost and the guard.** The Rao-Scott Δ is a `q × q` solve with `q = (rows−1)(cols−1)`; the full
`rc × rc` covariance is never materialised (the rank-2 structure of `CF` reduces every product to
`O(rc·q)`). Measured (block K): `10×5` → 1 ms, `20×8` → 6 ms, `40×10` → 0.10 s, `60×12` → 0.66 s,
`100×12` → 3.5 s. So: **exact when `q ≤ 400`** (which covers 40 rows × 11 columns, i.e. every
ordinary crosstab), first-order fallback above it using the mean of the per-cell design effects
(`O(cells)`, diagonal only) — **and the fallback is recorded**, so a table never claims more than it
computed. A Kronecker-structured contrast matrix would raise the ceiling if a real case ever needs
it; nothing measured here does.

### 2.6 ONE descriptive/inferential split, stated once

The package already has this rule and applies it inconsistently. Written down, it decides every
remaining question by itself:

> **An estimate describes the population: it is weighted, always, at every position.**
> **A base describes the sample: it follows the inference basis.**

Estimates: cell %, means, differences, ratios, odds ratios, Cramér's V / φ / η², the *relative*
contribution `ctr` and the signed absolute contribution `var`. These are weighted at every position
and are **never** design-corrected — which is why `ctr` keeps feeding a correspondence analysis
unchanged whatever the option says (§5.1 Q3), and why the effect sizes stayed weighted-only in z14-i.

Bases: `n_eff`, the χ²/F reference distribution, the `contrib` residual and its p-value, every
critical value. These follow the basis.

Displayed counts (`n`, `add_n`, `n_min`) stay raw at every position: they answer *how many people are
behind this cell*, not *how much information*.

### 2.7 `contrib`, resolved by that split (W3)

The colour measure has two readings (Last Phase z4) and the split cuts exactly between them:

* `ignore` / `grey_non_signif` colour the **relative contribution** — an estimate. Unchanged at every
  position, by construction, so the CA reading is safe.
* `guaranteed_effect` colours the **absolute adjusted standardized residual**, and the significance
  gate reads its p-value — inference. It follows the basis.

Today the residual reads the cell's own `n_eff` when the column's stored `type` is
`"n"`/`"all"`/`"all_tabs"`, and the *Total column's* `n_eff` otherwise — which is degenerate under a
design (`p = 1`), so the correction silently vanishes on percentage tables: measured `1.6e-11` vs
`0.052` for the same cell of the same data, in the two shapes.

The fix is not a better fallback, it is the right producer. Under the flat design the residual
`p̂_ij − p̂_i+·p̂_+j` is a smooth function of the cell proportions, so its exact variance is
`a′ CF a` with a sparse gradient — which expands, using the rank-2 structure of `CF`, to

```
Var = n/(n−1) · [ Σ a_k² A_k − 2(Σ a_k p_k)(Σ a_k A_k) + S(Σ a_k p_k)² ] / B²
```

i.e. `O(cells)` per cell, from the same `A`, `p`, `S`, `B`. It is computed where `Σw²` lives and
threaded into the test step as one extra per-`col_var` matrix, which **deletes** the
`type %in% c("n","all","all_tabs")` guess and the Total-column read. Being a property of the joint
distribution, it does not depend on `pct` — so a counts table and a row-percentage table of the same
data give the same residuals **by construction**, which is what the maintainer's ruling asks for.

Why not the cheap route (one design effect per table, dividing χ² and √-dividing every residual):
measured, it gives per-cell `|z|` between **0.61× and 1.37×** the exact value (block R) — enough to
cross a threshold in either direction. Kish is worse (0.52–1.17), the raw n worse still (up to 1.93).

Under a real design the same definition holds with `svyrecvar`: the residual's influence vector is a
linear combination of the cell influence vectors the producer already builds, so it is one more block
of columns, computed only when `contrib` colouring is on.

### 2.8 The module map

```
R/survey-design.R      the boundary + the resolver
  svy_is_design / svy_unwrap_data / svy_check_test          unchanged
  svy_inference_basis()          <- renamed, 4 values, THE resolver, the ONLY option read
  svy_design_is_flat()           <- new: routes a flat svydesign to the closed form
  svy_domain_design()                                       unchanged
  tab_robust_overlay()           <- design basis only; `mode` argument gone
  svy_omnibus_one()              <- the whole `kish` block DELETED

R/survey-variance.R    the variance, one definition, two implementations
  § the flat closed form (ids = ~1)   NEW
     svy_flat_var_prop()  svy_flat_var_mean()  svy_flat_resid_var()  svy_flat_raoscott()
  § the design (svyrecvar)            unchanged, + a "design_partial" return path
     svy_var_prep / svy_var_prop / svy_var_mean / svy_var_block / svy_row_at

R/tab.R
  plain_core()  one n_eff producer, selected by basis; leaf_wide_pct() loses its w2 arm
  num_core()    n_eff written whenever basis != "n", NOT gated on `ci`         (W13)
  tab_ci()      unchanged (already reads n_eff), but its critical value threads degf
  chi2_write_contrib()  consumes the residual matrix; the `type` guess DELETED  (W3)
  tab_setup()   stores meta$inference; aborts on wt + design                    (W10)

R/tab-agg.R
  num_moment_scan()   accumulates Σw², Σw²x, Σw²x² whenever weighted; getOption read DELETED
  agg_chi2 / agg_anova   unchanged (they compute the statistic; the correction is applied beside)

R/tab_reg.R
  reg_empirical()   the closed form, ALWAYS when weighted (never the option)    (W1, W2)
  reg_fit() / reg_resolve_multiplier()   read the resolved basis                (W12)

R/fmt_class.R
  tab_weight_line()   generated from meta$inference$basis; the string sniff DELETED (W4, W5)
```

---

## 3. The user-facing model

### 3.1 The whole story, in four sentences

1. `wt = w` — your estimates are weighted.
2. By default the confidence intervals and tests still use the **unweighted** number of respondents,
   and the table says so.
3. `options(tabxplor.design_effect = TRUE)` — they account for the unequal weighting instead. This is
   what `tab_reg()` always does, so turn it on when you want a `tab()` percentage to be comparable
   with a `tab_reg()` `Obs_*` column.
4. `survey::svydesign(...)` passed as `data` — everything accounts for the full design: strata,
   clusters, `fpc`, calibration.

Plus one boundary note: `tab_counts()` starts from counts you supply, so its `n` is the `n` you gave
it; there is no design effect to recover.

### 3.2 The option: scope, name, deprecation

Per the maintainer's ruling, the option **exists, defaults to `FALSE`, and its scope is `tab()`**
(and its leaves, `tab_num()` / `tab_plain()` / `tab_many()`). `tab_reg()` never reads it: its crude
`Obs_*` columns are always on the `"weights"` basis, so they always match the `Model_*` column beside
them. A `survey` design passed as `data` always overrides it — passing a design is an explicit
request.

**Rename**, because the name now describes an implementation the package no longer uses:

```r
options(tabxplor.design_effect = TRUE)      # new
options(tabxplor.kish_neff     = TRUE)      # deprecated synonym, still works
```

`tx_getOption(c("tabxplor.design_effect", "tabxplor.kish_neff"), FALSE)` — the synonym resolver
Phase 17j built for exactly this — makes the alias one line and zero breakage.
`tabxplor.design_effect` names the *reason* rather than the mechanism, and it is the word the footer,
the legend, the vignette and the jamovi label can all share. (`tabxplor.effective_n` would name the
mechanism and match the `n_eff` field; noted, not recommended.)

### 3.3 The footer: one sentence per position

Generated from `meta$inference$basis` — so the claim cannot outlive the computation, which is W4
closed structurally rather than by a `cli_inform()` that every export drops.

| basis | sentence |
|---|---|
| `"n"`, unweighted | *(nothing — as today)* |
| `"n"`, weighted | **"Weighted by w; confidence intervals and tests use the unweighted sample size."** |
| `"weights"` | **"Weighted by w; confidence intervals and tests account for the weighting."** |
| `"design"` | **"Design-based (survey): weighted estimates, intervals and tests account for the sample design."** *(unchanged)* |
| `"design_partial"` | **"Design-based (survey) estimates; this table's design variance could not be computed (too large), so its intervals account for the weighting only."** |

French drafts, for the maintainer's review:

```
Pondéré par w ; les intervalles de confiance et les tests utilisent l'effectif non pondéré.
Pondéré par w ; les intervalles de confiance et les tests tiennent compte de la pondération.
Fondé sur le plan de sondage (survey) : estimations, intervalles et tests tiennent compte du plan
  d'échantillonnage.
Estimations fondées sur le plan de sondage (survey) ; la variance du plan n'a pas pu être calculée
  pour ce tableau (trop grand) : les intervalles ne tiennent compte que de la pondération.
```

The degrade reason comes from a three-value enum, so it stays one clause: *too large* / *design not
supported* / *computation failed*.

The second sentence is the important one: it is the **default**, so it appears on every weighted
table, and it turns the package's least defensible position into a visible, teachable one. No
numbers in the footer — the design effect belongs at its own grain, in `test$deff`, beside the
p-value it corrects.

### 3.4 Docs and vignettes

* `?tab` — the ladder table gains a column ("what the interval assumes") and rung 2's row stops
  saying "Kish". The `contrib` residue paragraph is **deleted**, not rewritten (§2.7 removes it).
* `?tabxplor-options` — the renamed option, its `tab()`-only scope, and one sentence saying
  `tab_reg()` does not read it.
* `?tab_reg` — "will not agree to the last digit" becomes "agrees exactly" (measured ratio 1); the
  `empirical` paragraph states that the crude columns are always on the weighted basis.
* Both intro vignettes, § Weights — the three-row ladder stays but rung 2 becomes exact, and gains
  **one concise sentence**: *turn `tabxplor.design_effect` on if you want a `tab()` percentage
  interval to be comparable with the `Obs_*` column of a `tab_reg()` on the same data.*
* Both regression vignettes — one clause: the crude companions are design-weighted whatever the
  option says.
* The vignette sentence *"`tab_reg()` regression tables follow the same rule, models and observed
  companions alike"* — **corrected**, it is false today at the default position (W2).

### 3.5 jamovi

* **Crosstables**: `test_robust` (`classic` / `kish`, titled *"Type of p-value"*) becomes a single
  checkbox for the renamed option, described honestly — it moves every interval, star, colour
  threshold **and** the p-value, not only the p-value (W11).
* **Regressions**: **no control is added.** The earlier ruling (Q6, "yes to both") assumed the rung
  had to be selectable there; with the option scoped to `tab()` and `tab_reg()` always corrected,
  a selector would only offer a wrong answer. This resolves W11 by deletion.
* Needs a `jmvtools::prepare()`; `.a.yaml` changes so `.h.R` regenerates.

---

## 4. How the thirteen findings close

| # | closes by |
|---|---|
| **W1** — one `Obs_*` column, two rungs | the closed form puts factor rows on the same variance as the fit-based numeric/ordinal ones; **measured ratio 1.000** against the univariable `svyglm` (§1.2) |
| **W2** — the model column was never on rungs 1–2 | there is no longer a rung it could be above: `tab_reg()`'s crude and model columns are both on `"weights"`, always |
| **W3** — `contrib` design-corrected on counts, not on percentages | §2.7 — one exact per-cell residual producer, `pct`-independent by construction |
| **W4** — the design footer keeps its claim after a degrade | §2.1/§3.3 — `"design_partial"` is a stored state with its own sentence; and the fallback keeps the weights instead of discarding them |
| **W13** — the two leaves disagree on the step path | §2.3 — `n_eff` is written as a property of the cell, ungated by `ci` |
| **W5** — the rung is computed but never stored | §2.1 — `meta$inference`; the `".svy_weights"` string sniff is deleted |
| **W6** — rung 1 is not a rung | it remains the default **by ruling**, but it is now labelled on the table (§3.3), scoped to `tab()` only, and the vignette states the one sentence that makes the two functions comparable |
| **W7** — `degf` never consulted | §2.4 — one `conf_level_to_crit(conf_level, df)` |
| **W8** — `test$n` means three things | §2.5 — always the raw count; the effective information moves to `test$deff` |
| **W9** — `tab_counts()` cannot climb, silently | its basis is `"n"` by construction and the footer says so; §8.4 notes the one-argument extension that would let it climb |
| **W10** — `wt` silently overridden by a design | abort in `tab_setup()`, mirroring the weight-collision abort already there |
| **W11** — jamovi half-present and mislabelled | §3.5 — one honest checkbox in Crosstables, nothing in Regressions |
| **W12** — four white elephants | all four read the resolved basis (§2.1); the redundant conjunctions and the stray `getOption()` are deleted |

---

## 5. Decisions

### 5.1 Taken (maintainer, 2026-08-11)

1. **The option survives, opt-in, default `FALSE`, scope `tab()` only.** `tab_reg()`'s crude
   counterparts are always corrected, so they always match the model column. The vignette explains,
   concisely, that turning the option on is what makes a `tab()` percentage comparable with a
   `tab_reg()` `Obs_*` column.
   *Consequence recorded honestly*: W6 stays open by choice — the default position is still one that
   corresponds to no sampling model, and `tab()` and `tab_reg()` still differ by default. §3.3's
   footer sentence is what makes that choice explicit instead of silent; it is a hard requirement of
   this ruling, not a decoration.
2. **The omnibus test is exact, from the aggregate** — with the `q ≤ 400` guard, the first-order
   fallback, and a permanent parity test against `survey::svychisq` (§7).
3. **`contrib` gets the exact per-cell residual when the option is on.** When it is off, the residual
   keeps the raw-n base (today's behaviour), and — at every position, on or off — the **relative
   (Pearson) contributions are untouched**, because they are estimates, not inference (§2.6). The
   correspondence-analysis reading is therefore invariant by construction.
4. **`degf` always; Korn–Graubard opt-in** as `method_cell = "beta"`.

### 5.2 Recommended here, open to the maintainer

* **The option's new name**: `tabxplor.design_effect`, with `tabxplor.kish_neff` as a deprecated
  synonym through `tx_getOption()` (§3.2).
* **The stored fact's shape**: `meta$inference = list(basis, degf, note)` (§2.1), and `test$deff`
  as a new column of the `test` tibble (§2.5) — the same additive shape Phase j used for
  `effect_size` / `es_type` / `pvalue_exact`, provable minimal with
  `dev/verify_golden_field_delta.R`.
* **The test discriminators**: `chi2` / `chi2_design`, `F_welch` / `F_classic` / `F_design` — four
  values become two, because the flat and full designs run the same estimator.
* **`Σw²` accumulated whenever the table is weighted**, not only when the option is on. It costs one
  extra `value.var` in the factor dcast and three grouped sums per numeric `col_var` (measured as
  noise in the stress test's §9.4), and it buys a real UX win: **toggling the option becomes a jamovi
  cache HIT** instead of a full re-aggregate, because the aggregate has one shape.
* **The footer carries no numbers** (§3.3); the design effect lives in `test$deff` at its own grain.

---

## 6. Implementation plan

Three sessions, split where the understanding changes rather than where the line count does. The
first is worth running first whatever happens to the rest: it is almost pure subtraction, and every
later decision is easier to express once the basis is a fact on the table.

### z16-i — the fact, and the honesty that follows from it

`meta$inference` + `svy_inference_basis()` + every consumer reading it (§2.1) · the `degf` thread and
`conf_level_to_crit()` (§2.4) · `n_eff` written as a property of the cell in the numeric leaf (W13) ·
the abort on `wt` + design (W10) · `test$n` always raw + `test$deff` (W8) · the four footer sentences
and their French (§3.3) · the four white elephants (W12) · the documentation truth pass (§3.4).

*Values that move*: only the direct-`tab_num(design)` step path (W13, a bug fix) and any interval
under a real design with few PSUs (W7, a bug fix). Everything else byte-identical.

### z16-ii — the closed form

`svy_flat_var_prop()` / `svy_flat_var_mean()` (§1.1) · `svy_design_is_flat()` routing · the leaf
rewiring, `leaf_wide_pct()` losing its `w2` arm (§2.2) · `num_moment_scan()` gaining `Σw²x`, `Σw²x²`
· `reg_empirical()`'s always-on crude base (W1, W2) · the degenerate-cell fallback (§2.3) ·
`"design_partial"` (W4) · the option rename.

*Values that move*: every weighted `tab()` **with the option on** (Kish → exact), and every weighted
`tab_reg(empirical = TRUE)` (raw n → exact — this is the W1/W2 fix, and it makes the crude and model
columns agree). Reg tables are not snapshotted, so the sentinels are `test-tab_reg-empirical.R`'s
value assertions and `test-kish-descriptive.R`, which is renamed and rewritten around the new
identity.

### z16-iii — the tests and the residual

The exact Rao-Scott from the aggregate + the guard (§2.5) · the numeric Wald F · deleting
`svy_omnibus_one()`'s `kish` block and `tab_robust_overlay()`'s `mode` · the exact `contrib` residual
and its threading, deleting the `type` guess (§2.7) · `method_cell = "beta"` · the jamovi pass
(§3.5, needs `prepare()`).

*Values that move*: weighted omnibus p-values with the option on; `contrib` residual p-values (and
therefore `guaranteed_effect` colouring) on weighted tables — a conscious `_color_golden` regen, with
the relative-contribution goldens **proved unmoved** (§2.6).

---

## 7. Verification: the parity contract

The closed form is a second implementation of something `survey` already computes. That is only safe
if the identity is *tested*, permanently, not asserted in a comment. New
`tests/testthat/test-flat-design-parity.R`:

| # | assertion | measured here |
|---|---|---|
| 1 | `svy_flat_var_prop()` == `svyby(svymean)` variance, `pct = "row"/"col"/"all"` | ratio `1` |
| 2 | ... on a **total row**, a **total-table row**, and inside a `tab_vars` subtable | ratio `1` |
| 3 | `svy_flat_var_mean()` == `svyby(svymean)` variance | ratio `1` |
| 4 | the flat closed form == `svy_var_prop()`/`svy_var_mean()` on the *same* flat `svydesign` | ratio `1` |
| 5 | the exact Rao-Scott F, ndf, p == `survey::svychisq(statistic = "F")`, ≥ 5 table shapes | `1.0000000000` |
| 6 | the numeric Wald F == `svyglm` + `regTermTest` | `1.00000000` |
| 7 | `tab_reg(empirical=)`'s `Obs_OR` bracket == univariable `svyglm` SE(log OR) | ratio `1` |
| 8 | `Obs_diff` bracket == `svycontrast` on the two domain means | ratio `1` |
| 9 | unweighted output byte-identical to today | — |
| 10 | equal weights: `n_eff == n·(n−1)/n` (the documented discontinuity, §8.1) | `0.99975` at n = 4000 |
| 11 | the relative contribution `ctr` is identical at all four bases | (§2.6) |
| 12 | `q > 400` records the first-order fallback rather than claiming the exact test | — |

Plus the existing suite: `test-golden.R` / `test-export-parity.R` / `test-fmt-contract.R` unmoved
except where §6 lists a conscious regen, and `dev/verify_golden_field_delta.R` extended to prove the
`test$deff` column is the only delta in the structural goldens.

---

## 8. Caveats and residues, honestly

**8.1 The finite-sample factor.** The closed form includes survey's `n/(n−1)`, so a table weighted by
a **constant** gets `n_eff = n − 1` rather than `n` (measured ratio `0.99975` at n = 4000; 5 % at
n = 20). Including it is not optional: `svyglm` on the flat design includes it, and matching `svyglm`
exactly is the whole point of §1.2's line 7. Unweighted tables never enter this path, so no golden
moves; a constant-weight table is a declared design and gets the design's own answer.

**8.2 W6 stays open by ruling.** The default position remains one that corresponds to no sampling
model, and a weighted `tab()` still disagrees with `tab_reg()` unless the option is on. The mitigation
is the footer sentence (§3.3) — which means that sentence is load-bearing, not cosmetic: without it
this design would ship a silent wrong default *and* a documented discrepancy.

**8.3 The exact test has a ceiling.** `q > 400` falls back to the first-order correction. That is
recorded, not silent — but it is a second answer for the same question, and someone will eventually
ask why a 300-level geography tests differently. Stated in `?tab`.

**8.4 `tab_counts()` still cannot climb** — pre-aggregated counts carry no `Σw²`. The closed form
does open a door the influence-function route closed: it needs only *cell-level* sums, so a
`w2_counts =` argument would let a pre-aggregated table reach the `"weights"` basis exactly. Noted,
not proposed — one more argument for a rare input.

**8.5 The covariance between cells.** Route A's ruling Q3 (differences are conservative because the
cell-to-cell design covariance is discarded) is **exactly zero at the flat design** for disjoint
domains, so the residue disappears at the `"weights"` basis. It remains at `"design"`, unchanged.

**8.6 What was not measured.** Multinomial and ordinal crude values under a design against a `survey`
oracle; `svy_vglm` / `svyolr` under clustering; `fpc`-only and two-stage `fpc` designs; the cost of
the always-on `Σw²` accumulation at 8M rows (the stress test measured Kish as free, and the numeric
side gains two more sums — re-measure at implementation with `dev/benchmarks/run_bench.R`); the
`svrepdesign` refusal end-to-end.

**8.7 One generator.** All ratios here are identities (they hold generally); the *magnitudes*
(×1.11–×1.21 CI widening, Kish off by 7–12 %) come from the fixtures in Appendix B and are
illustrative, not population parameters.

---

## Appendix A — the mathematics

### A.1 The influence function and why the closed form exists

For `p̂ = A/B`, `A = Σ u_k w_k`, `B = Σ v_k w_k`, the linearized contribution is
`z_k = (u_k − p v_k)/B`, and

```
Σ_k w_k z_k = (A − p B)/B = 0        exactly, for every base
```

so `survey:::onestrat`'s centering is a no-op and, at `ids = ~1` with no `fpc`,

```
Var = n/(n−1) · Σ_k (w_k z_k)²
```

For a proportion, `u_k = 1{cell}`, `v_k = 1{base}`, so `w z` is `w(1−p)/B` inside the cell,
`−wp/B` in the base outside the cell, `0` outside the base:

```
Var(p̂) = n/(n−1) · [ A(1−p)² + (S−A)p² ] / B²          A = Σ_cell w²,  S = Σ_base w²,  B = Σ_base w
```

For a mean, `u_k = x_k·1{base}`, `v_k = 1{base}`, so `w z = w(x − x̄)/B` inside the base:

```
Var(x̄) = n/(n−1) · [ Σw²x² − 2x̄·Σw²x + x̄²·Σw² ] / B²
```

### A.2 The full cell covariance

With `v_k ≡ 1` (the `"all"` base) and disjoint cells `a`, `b`:

```
Cov(p̂_a, p̂_b) = n/(n−1) · [ δ_ab A_a − p_a A_b − p_b A_a + p_a p_b S ] / B²
```

— a diagonal plus a rank-2 update, so it never has to be materialised: for any `rc × q` matrix `G`,

```
G′ CF G = n/(n−1) · [ G′diag(A)G − (G′p)(G′A)′ − (G′A)(G′p)′ + S(G′p)(G′p)′ ] / B²
```

which is `O(rc·q)`. This is what makes both the Rao-Scott Δ (§2.5) and the per-cell residual variance
(§2.7) cheap.

### A.3 The Rao-Scott adjustment, from the aggregate

`survey::svychisq(statistic = "F")` needs exactly four inputs: the estimated cell proportions
`p`, their covariance `V`, the unweighted `N`, and `degf`. Given `p` and `CF` in closed form, its own
algebra follows verbatim:

```
C     = qr.resid(qr(X1), X12[, -(1:(nr+nc-1))])       # interaction contrasts ⟂ main effects
Δ     = solve( C′ (D⁻¹/N) C ,  C′ D⁻¹ CF D⁻¹ C )       # D = diag(p)
d0    = tr(Δ)² / tr(Δ²)
F     = X² / tr(Δ)          p = pf(F, d0, d0·degf, lower.tail = FALSE)
```

where `X²` is the Pearson statistic on the estimated proportions scaled to `N` — **which tabxplor's
`agg_chi2()` already computes**, since z14-i made the weighted χ² a rescale to the raw n. Verified
`F` ratio `1.0000000000` and `ndf` ratio `1.0000000000` on five shapes.

### A.4 The numeric side

At `ids = ~1` the group means are independent, so with `V = diag(Var(x̄_g))` from A.1 and any
full-rank contrast `C` of `k−1` rows,

```
F = (C x̄)′ (C V C′)⁻¹ (C x̄) / (k−1)        df1 = k−1,  df2 = degf − (k−1)
```

which reproduces `svyglm(x ~ grp)` + `regTermTest(method = "Wald")` to ratio `1.00000000`
(`df2 = n − k`, matching `regTermTest`'s own `ddf`). Welch on the same effective n lands at `0.998`
— close, but not the identity, so the Wald form is the one to implement. Welch on the raw n is
`1.62×`, i.e. badly anti-conservative.

### A.5 The residual

`g(p) = p_ij − p_i+ p_+j` has gradient `∂g/∂p_kl = δ_(kl=ij) − δ_(k=i) p_+j − δ_(l=j) p_i+`, so
`Var(ĝ) = a′ CF a` expands through A.2 into `O(rc)` arithmetic in `A`, `p`, `S`, `B`. The adjusted
standardized residual is `ĝ / √Var(ĝ)`, which reduces **exactly** to
`stats::chisq.test()$stdres` when all weights are equal — so the unweighted path is byte-identical,
as `contrib_adj_resid()` is already pinned to be.

---

## Appendix B — measurements and reproducers

Five self-contained scripts; blocks are cited above by letter. Run each with
`OMP_NUM_THREADS=1 Rscript <file>`. None of them loads tabxplor — they measure the *mathematics*
against `survey`, so they stay valid as the package changes.

### Block N / T — the closed form, all bases, totals, degeneracy

```r
suppressMessages(library(survey)); options(survey.lonely.psu = "adjust")
set.seed(7); n <- 4000
d <- data.frame(grp = factor(sample(c("A","B","C","D"), n, TRUE, prob = c(.4,.3,.2,.1))))
d$w <- exp(rnorm(n, 0, .55)) * c(A=.6,B=1,C=1.6,D=2.4)[as.character(d$grp)]; d$w <- d$w/mean(d$w)
lin <- -0.3 + 0.5*scale(log(d$w))[,1] + c(A=-.4,B=0,C=.3,D=.6)[as.character(d$grp)]
d$col <- factor(ifelse(rbinom(n,1,plogis(lin)) == 1, "yes", "no"))
d$x   <- round(rnorm(n, 50, 12) + 6*log(d$w))
des <- svydesign(ids = ~1, weights = ~w, data = d)
w <- d$w; N <- n; fac <- N/(N-1)

cf_prop <- function(cell, base) {
  A <- sum(w[cell]^2); S <- sum(w[base]^2); B <- sum(w[base]); p <- sum(w[cell])/B
  c(p = p, V = fac * (A*(1-p)^2 + (S-A)*p^2) / B^2)
}
lv <- levels(d$grp)
# row%  vs svyby;  col% vs svyby(~grp, ~col);  all% vs svymean(~interaction)
t(sapply(lv, function(g) cf_prop(d$grp == g & d$col == "yes", d$grp == g)))
# mean
t(sapply(lv, function(g) { m <- d$grp == g; ww <- w[m]; x <- d$x[m]
  B <- sum(ww); xb <- sum(ww*x)/B; c(m = xb, V = fac*sum(ww^2*(x-xb)^2)/B^2) }))
```

Result: every ratio to `survey` is `1` (§1.2). `n_eff` raw / Kish / exact per level:
`1619/1183/1098`, `1190/867/855`, `789/559/597`, `402/293/328`. Equal weights: `n_eff/n = 0.99975`
= `(n−1)/n` exactly (§8.1). A `p = 0` cell: `V = 0`, `n_eff` undefined, base's `B²/S = 1342.5`
against a raw 2435 (§2.3). Additivity of `Σw²` across cells: `max|diff| = 2.3e-13`.

### Block C — the covariance, and svychisq reproduced exactly

```r
cell <- interaction(d$grp, d$col, drop = TRUE, sep = "|"); lvl <- levels(cell)
A <- sapply(lvl, function(l) sum(w[cell == l]^2))
p <- sapply(lvl, function(l) sum(w[cell == l])) / sum(w); S <- sum(w^2); B <- sum(w)
CF <- fac*(diag(A) - outer(p,A) - outer(A,p) + outer(p,p)*S)/B^2
max(abs(CF - vcov(svymean(~cell, des)))) / max(abs(vcov(svymean(~cell, des))))   # 1.6e-15
```

then A.3's four lines reproduce `svychisq(~grp+col, des, statistic = "F")`: on `(n, rows, cols,
sd(log w))` = `(3000,4,2,.6)`, `(3000,4,3,.6)`, `(1500,5,4,.9)`, `(8000,3,3,.35)`, `(600,4,2,1.1)`,
**`F` ratio and `ndf` ratio are `1.0000000000` in all five**. The cheaper alternatives, on the same
tables, against the exact `p`: first-order `6.7e-37` vs `1.1e-30`; Kish `5.6e-27`; unadjusted
`2.3e-48`.

### Block W — the crude column against the univariable model

```r
eff <- t(sapply(lv, function(g) { m <- d$grp == g; ww <- w[m]; y <- d$y[m]
  B <- sum(ww); p <- sum(ww*y)/B; A1 <- sum(ww[y == 1]^2); S <- sum(ww^2)
  c(p = p, neff = p*(1-p) / (fac*(A1*(1-p)^2 + (S-A1)*p^2)/B^2)) }))
woolf <- function(p1,n1,p0,n0) sqrt(1/(p1*n1) + 1/((1-p1)*n1) + 1/(p0*n0) + 1/((1-p0)*n0))
# vs summary(svyglm(y ~ grp, des, quasibinomial()))$coefficients[-1, "Std. Error"]
```

`Woolf on exact n_eff / svyglm` = `1 1 1`. On Kish: `0.981 1.002 1.055`. On the raw n:
`0.833 0.840 0.877`. The difference bracket against `svycontrast`: `1 1 1` / `0.983 1.004 1.047` /
`0.836 0.841 0.871`.

### Block R — the contrib residual

Exact design residual (A.5) against the three shortcuts, as a ratio of `|z|`, over five table shapes:

```
approx / exact     /√d̄            Kish           raw n
                   0.61 – 1.37    0.52 – 1.17    0.70 – 1.93
```

### Block K — the cost of the exact test

```
rows x cols   q = (r-1)(c-1)   seconds
   5 x  3            8          0.003
  10 x  5           36          0.001
  20 x  8          133          0.006
  40 x 10          351          0.098
  60 x 12          649          0.664
 100 x 12         1089          3.525
```

→ the `q ≤ 400` guard (§2.5).

---

## Appendix C — site-by-site inventory

Line references are anchors from the 2026-08-11 audit; **re-grep before editing**.

| site | today | after |
|---|---|---|
| `R/survey-design.R:102` `svy_inference_mode()` | 3 values, reads the option | `svy_inference_basis()`, 4 values, the ONLY option read |
| `R/survey-design.R:177-209` `svy_omnibus_one()` kish block | hand-rolled Rao-Scott + hand-rolled weighted ANOVA | **deleted** |
| `R/survey-design.R:~230` `tab_robust_overlay()` | `mode` ∈ kish/survey | design basis only; `mode` argument gone |
| `R/survey-variance.R` | one section (`svyrecvar`) | two sections, one definition; `"design_partial"` return |
| `R/tab.R:3842`, `:5025` `design_on <- … && !is.null(…)` | redundant conjunction, twice | one basis read |
| `R/tab.R:3930` `kish <- identical(inference_mode,"kish")` | gates the `Σw²` dcast | gates on *weighted*, so the aggregate has one shape (§5.2) |
| `R/tab.R:4118-4141` `design_neff()` + the `pct == "no"` branch | design or Kish | one producer selected by basis; flat fallback |
| `R/tab.R:4368-4400` `leaf_wide_pct()` | also computes the Kish `Ne` | percentages and `tot_n` only |
| `R/tab.R:5258-5341` `num_core()` `_en` | inside `if (ci %in% c("cell","diff"))` | written whenever basis ≠ `"n"` (W13) |
| `R/tab.R:5810-5826` `tab_ci()` bases | `coalesce(n_eff, tot_n)` | unchanged; critical value threads `degf` |
| `R/tab.R:6369-6389` `chi2_write_contrib()` | `type %in% c("n","all","all_tabs")` guess + Total-column `n_eff` | consumes the residual matrix (§2.7) |
| `R/tab-agg.R:135` `num_moment_scan()` | `getOption("tabxplor.kish_neff")` | the resolved basis; + `Σw²x`, `Σw²x²` |
| `R/tab-agg.R` `zscore_formula` / `conf_level_to_z` | z only | `conf_level_to_crit(conf_level, df)` |
| `R/tab_reg.R:1187`, `:4636` `weighted <- …` | two spellings | the resolved basis |
| `R/tab_reg.R:1654-1658` `neff_or_n()` | Kish arm, option-gated | the closed form, always (W1, W2) |
| `R/fmt_class.R:4821` `tab_weight_line()` | `identical(as.character(wt)[1], svy_wt_col)` | reads `meta$inference$basis` (W4, W5) |
| `R/tab-test-display.R:137-159` | 4 discriminators, "; Kish" suffix | 2 discriminators, per-basis wording |
| `R/utils.R:123` `.onLoad` | `tabxplor.kish_neff = FALSE` | `tabxplor.design_effect = FALSE` (+ synonym) |
| `R/tab-counts.R:157-162` | its own `weighted` predicate | the resolved basis; footer says the base is the counts' own n |
| `jamovi/jmvtab.a.yaml:205-217` | `test_robust` classic/kish, *"Type of p-value"* | one honest checkbox |
| `jamovi/jmvtabreg.a.yaml` | nothing | **still nothing** (always corrected) |
