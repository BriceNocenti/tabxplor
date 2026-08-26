# The inference layer — cell residuals and design-based variance

> PURPOSE: the two statistical derivations behind a `tab()` table's colours and intervals — the χ²
> cell residual, and the variance a weight column alone can support.
> ROLE: what `R/tab-chi2.R`, `R/survey-variance.R` and `R/survey-design.R` implement but are too
> short to derive. Each of those file headers states the current design; this states why it is that
> design, what it assumes, and where the assumption breaks.
> KEY CONSTRAINTS:
>   - Every formula here is the one that ships. Read a number off the code, not off this file.
>   - The soundness section (§4) is the answer to a question a reviewer or a user *will* ask about a
>     shipped default. It has to stay honest about what the default cannot see.
> See: `CLAUDE.md § tabxplor architecture` (the inference layer) · `dev/regression.md` (the same
> influence-function algebra, for `tab_reg()`'s gap test).

---

## 1. The one idea

A cross-tab cell is coloured when its deviation is large **and** the deviation is statistically
solid. Both halves need a variance, and the package computes them from **sufficient statistics
only** — the aggregate, never a second pass over the microdata. That constraint is what makes the
two derivations below worth writing down: each is the closed form that lets an aggregate answer a
question normally answered from the observations.

- The **χ² residual** grades a cell against independence, on a scale comparable across tables.
- The **design-based variance** grades any cell against sampling noise, when the sample is weighted.

They meet in one field: both end as a number the ordinary CI machinery already reads (`pvalue` for
the first, `n_eff` for the second), which is why neither needed a new `fmt` field.

---

## 2. χ² cell residuals and contributions

### 2.1 The four questions a cell can answer

A cell supports exactly four questions users actually ask. The package serves those four, not the
cartesian product of every statistic that could be computed.

| the question                                                             | the right quantity                                     | how it is asked                      |
|--------------------------------------------------------------------------|--------------------------------------------------------|--------------------------------------|
| "How does this cell differ from a comparison group I chose?"             | difference / ratio / odds ratio against a reference     | `color = "difference"` / `"ratio"` / `"odds_ratio"` |
| "Which cells build the association **in this table**?"                   | relative contribution to χ² — a share of inertia        | `color = "contrib"`, `color_signif = "ignore"` |
| "…and which of those can I trust?"                                       | the same contribution, gated by a calibrated test       | `color = "contrib"`, `color_signif = "grey_non_signif"` |
| "Which cells are notably off independence, **comparably across tables**?" | the adjusted standardised residual, on an absolute scale | `color = "contrib"`, `color_signif = "guaranteed_effect"` |

The last three are **not three measures to choose between — they are one measure read three ways**,
which is why the residual lives on the `color_signif` axis and never becomes a second entry on the
`color` axis. One measure, one legend family, one break scale to learn.

**A share of inertia is intrinsically relative to its table, and that is a feature.** `contrib`'s
score is `share of χ² × k`, so its *b*-th break sits at `|r| ≥ √(b·χ²/k)` — a threshold that moves
with the table. Under `ignore` no significance is claimed, so there is nothing to be inconsistent
with: the reader is asking "which cells carry this association", and "1× the mean cell contribution"
is a scale-free answer, exactly as in a correspondence analysis. On a null table some cell is always
above the mean, because the shares sum to `k` by construction — an honest statement about where such
association as exists is concentrated. The incoherence appears only when significance enters, and it
is then a problem of the *gate*, not of the scale.

### 2.2 Three residuals, and the identity that matters

For an observed `o`, expected `e = r_i·c_j/N`, and margins `p_i· = r_i/N`, `p·_j = c_j/N`:

| name                                            | formula                       | null variance          | role                                                       |
|-------------------------------------------------|-------------------------------|------------------------|------------------------------------------------------------|
| Pearson                                         | `(o−e)/√e`                    | `(1−p_i·)(1−p·_j) < 1` | `Σ r² = χ²` — the decomposition. **Not** a calibrated test |
| **adjusted standardised** (Haberman 1973)       | `(o−e)/√(e(1−p_i·)(1−p·_j))`  | **1**                  | the calibrated test — the ±2 rule is only correct here      |
| moment-corrected (García-Pérez & Núñez-Antón)   | Pearson rescaled to unit variance | 1                  | indistinguishable from adjusted in practice; not offered    |

Two identities the design leans on:

- `r_pearson = sign(o−e)·√(raw contribution)` — the Pearson residual **is** the standardised raw χ²
  contribution. It orders cells identically to the `contrib` score, so it adds nothing.
- `raw contribution = (1−p_i·)(1−p·_j)·z²` — contribution and adjusted residual differ by a per-cell
  margin factor.

So the French tradition's "Pearson residual ≈ absolute contribution, divided by the total to get the
relative contribution" is correct — and it is precisely why the Pearson residual cannot double as
the test: it inherits the contribution's dependence on the margins. A 1.96 test on it under-rejects.

⚠ **The naming trap, which is why the formula must be named and not just referenced.**

| tool                           | its "standardized residual" | its "adjusted (standardized) residual"                  |
|--------------------------------|-----------------------------|---------------------------------------------------------|
| SPSS Crosstabs                 | Pearson `(o−e)/√e`          | Haberman — **the ±2 rule refers to this one**           |
| R `chisq.test()`               | `$residuals` = Pearson      | `$stdres` = Haberman (R's help calls it "standardized") |
| `questionr::chisq.residuals()` | default = Pearson           | `std = TRUE` → `$stdres` = Haberman                     |

R's `$stdres` and SPSS's "adjusted" are the same number. R's help calling it "standardized" is why
half the applied literature reports the wrong one, and why a French user arriving from `questionr`
needs the formula spelled out rather than the name.

### 2.3 Thresholds, and how many cells a routine table over-flags

±2 is the working rule (≈ the 95 % normal quantile); ±3 is recommended for large tables. Both fall
out of `conf_level` through `zscore_formula()` — the `zscore` break scale is declared as
`conf_level_to_z(c(0.95, 0.99, 0.9999, 1 - 2e-9))`, i.e. **the ladder is a sequence of confidence
levels**, not round numbers, and it is the one scale exempt from the shape rule
(`COLOR_SHAPE_EXEMPT`, `R/tab_classes.R`). Nothing hardcodes 1.96.

Multiple comparisons are real and quantified. On the package's own example tables:

| table              | cells | expected false positives at α = .05 | Bonferroni α | ⇒ \|z\| |
|--------------------|------:|------------------------------------:|-------------:|--------:|
| `relig × rincome`  |    32 |                                 1.6 |       0.0016 |    3.16 |
| `marital × rincome`|    20 |                                 1.0 |       0.0025 |    3.02 |

One or two spuriously flagged cells is the *expectation* on a routine table. Worth knowing:
**Bonferroni at k = 20–32 lands almost exactly on the classic ±3 rule**, so a reader who uses ±3 on a
survey-sized table is already correcting, without knowing it.

One finding that removes work rather than adding it: the modern recommendation is **not** to
condition cellwise residuals on a significant omnibus χ² (García-Pérez & Núñez-Antón 2014). No "only
grey when the table is significant" gate belongs here.

### 2.4 Sparse cells — where the risk actually is

The danger is a small **expected** count, not a small observed one. A rare row against a skewed
column distribution:

```text
             x      y      z      w
observed  1.00   0.00   0.00   3.00
expected  2.76   0.81   0.20   0.22
pearson  -1.06  -0.90  -0.45   5.90
adjusted -1.91  -1.01  -0.46   6.08        # min expected in the table: 0.20
```

`|z| = 6.08` at `e = 0.2`: the normal approximation simply does not hold. In a benign case
(`e = 3.18`, `o = 5`) the adjusted residual is a calm 1.49 — so this is **not** a general "small
counts get over-flagged" problem, it is a minimum-expected-count problem. The contribution inherits
it in a worse form, being `r²`-based: a sparse cell's inflated residual is *squared* into a large
share. `agg_chi2()` already computes `min_e` per table and already uses it to trigger Fisher's exact.

### 2.5 Why the residual is stored as a p-value

`MEASURES$<m>$raw` is a closure over **one column**, and the residual's ingredients — row totals,
column totals, `N` — live across columns. So it is computed at write time, in `R/tab-chi2.R`, and
stored. It is stored as the **p-value**, and the residual is recovered at render time:

```r
|z| = -qnorm(p / 2)          sign from sign(get_ctr(x))
```

which costs no new `fmt` field and keeps every field name truthful. Storing `z` in the field named
`pvalue` was rejected: it would give one field two meanings keyed by an attribute, and a user reading
`$pvalue` through `mutate()` would silently get a different quantity.

⚠ **It must be `-qnorm(p/2)`, never `qnorm(1 - p/2)`.** `1 - p/2` is exactly `1` in double precision
for `p < 2.2e-16`, i.e. for any `|z| > 8.2` — routine in survey-sized tables. The correct call is
exact down to `p ≈ 1e-300` (`|z| ≈ 37`); beyond that `p` underflows to 0 and `z` is `Inf`, which the
guaranteed factor `(1 − z_α/Inf)² = 1` handles gracefully as maximum colour. The warning sits at the
accessor in `R/fmt_class.R`.

---

## 3. Design-based variance, from the weights alone

### 3.1 The closed form, and why it is exact

Every quantity in a `tab()` table is a ratio of two weighted sums, `p̂ = A/B` with `A = Σ u_k w_k`
and `B = Σ v_k w_k`. Its linearized contribution is `z_k = (u_k − p v_k)/B`, and

```text
Σ_k w_k z_k = (A − p B)/B = 0        exactly, for every base
```

so the centering `survey` performs is a no-op, and at `ids = ~1` with no `fpc` the whole recursive
variance collapses to one sum:

```text
Var = n/(n−1) · Σ_k (w_k z_k)²
```

For a proportion, `u_k = 1{cell}` and `v_k = 1{base}`, so `w z` is `w(1−p)/B` inside the cell,
`−wp/B` in the base outside it, and `0` outside the base:

```text
Var(p̂) = n/(n−1) · [ A(1−p)² + (S−A)p² ] / B²     A = Σ_cell w², S = Σ_base w², B = Σ_base w
```

For a mean, `u_k = x_k·1{base}`, so `w z = w(x − x̄)/B`:

```text
Var(x̄) = n/(n−1) · [ Σw²x² − 2x̄·Σw²x + x̄²·Σw² ] / B²
```

Both are computable **from the aggregate alone** — three sums per cell — which is why a weighted
table costs no extra pass. This is the flat closed form in `R/survey-variance.R`; a real design goes
to `survey::svyrecvar()` instead, which owns the variance algebra throughout.

**Kish is its degenerate limit.** `deff = 1 + CV²(w)` is what the closed form reduces to when the
weights carry no information about the outcome. The exact form can therefore go *either way*, and
does: on a weighted NHANES race distribution the effective *n* is **11 336 against a raw n of
8 591** — an efficiency gain from informative weights that Kish's formula is structurally incapable
of reporting, because it can only ever widen.

### 3.2 The full cell covariance, without materialising it

With `v_k ≡ 1` and disjoint cells `a`, `b`:

```text
Cov(p̂_a, p̂_b) = n/(n−1) · [ δ_ab A_a − p_a A_b − p_b A_a + p_a p_b S ] / B²
```

— a diagonal plus a rank-2 update. So for any `rc × q` contrast matrix `G`:

```text
G′ CF G = n/(n−1) · [ G′diag(A)G − (G′p)(G′A)′ − (G′A)(G′p)′ + S(G′p)(G′p)′ ] / B²
```

which is `O(rc·q)`. That is what makes both the Rao-Scott adjustment and the per-cell residual
variance cheap enough to be on by default.

### 3.3 Rao-Scott, from the aggregate

`survey::svychisq(statistic = "F")` needs exactly four inputs: the estimated cell proportions `p`,
their covariance `V`, the unweighted `N`, and `degf`. Given `p` and the covariance in closed form,
its algebra follows verbatim:

```text
C  = qr.resid(qr(X1), X12[, -(1:(nr+nc-1))])     # interaction contrasts ⟂ main effects
Δ  = solve( C′ (D⁻¹/N) C ,  C′ D⁻¹ CF D⁻¹ C )    # D = diag(p)
d0 = tr(Δ)² / tr(Δ²)
F  = X² / tr(Δ)        p = pf(F, d0, d0·degf, lower.tail = FALSE)
```

`X²` is the Pearson statistic on the estimated proportions rescaled to `N`, which `agg_chi2()`
already computes.

⚠ The `contrib` residual's design correction is **first-order only**: one Rao-Scott δ̄ per table, not
a per-cell design residual. `?tab` says so.

---

## 4. Is a weights-only design effect sound?

**Yes — but "sound" here is narrower than it sounds, and the narrowness decides how to use it.**

The flat basis is not an *approximation* of your survey's variance. It is the **exact** variance of a
different, simpler design: single-stage, with-replacement, unequal-probability sampling. That
estimator is well defined and design-consistent for that design, and tabxplor's closed form
reproduces `survey` at that design to ratio `1.00000000`.

So the real question is not "is the estimator valid?" but **"how far is the design it assumes from
the design you actually have?"**

### 4.1 The four omissions, and their signs

| what the flat basis omits          | direction of the error   | measured size                                                          |
|------------------------------------|--------------------------|------------------------------------------------------------------------|
| **clustering / multi-stage**       | **intervals TOO NARROW** | reported SE ×0.10 to ×0.67 of the truth — the real SE can be **9×** it |
| stratification                     | intervals too wide       | ×1.005 alone, ×1.07 with the `fpc`                                     |
| calibration / post-stratification  | intervals too wide       | ×1.08, and only on variables the margins predict                       |
| finite population correction       | intervals too wide       | ×1.01; ×1.00 at national sampling rates                                |

The three conservative omissions are worth 0–8 % each. The one anti-conservative omission can be
worth an order of magnitude. **They do not cancel**: the dangerous one dominates whenever it is
present. Hence the one operational rule this section exists to state:

> The flat basis is sound *and materially right* when the sample has **no clustering** — a
> register-drawn, web, telephone or mail sample of individuals, however unequal its weights.
> It is sound *but materially wrong* when the sample is a **face-to-face area sample** — the standard
> French, European and American national household survey — because those are clustered by
> construction, and the flat basis cannot see it.

### 4.2 What softens it: tabxplor colours contrasts, not levels

A level (a marginal percentage) is the worst case. A *difference between two cells* and a
*regression coefficient* are far more robust, because the cluster effect largely cancels in a
within-cluster contrast — a known result (Skinner, Holt & Smith 1989) that happens to align with the
package's whole visual grammar: its colours score a cell **against a reference**, its stars test a
**difference**, `tab_reg()` reports **coefficients**, and `adjustment` / `between_groups` test **gaps
between two estimates**. Measured, as SE flat / SE full design:

```text
     file   outcome        by    level   difference
 apiclus1     api00     stype    0.395        0.883
   NHANES   HI_CHOL      race    0.990        0.925
   NHANES   HI_CHOL    agecat    1.014        0.905
```

`apiclus1` is the demonstration: the same file where a **mean** is 2.5× too precise gives a
**difference** 13 % too precise and a regression slope 8 % too precise.

**The mechanism, and its limit.** The cluster effect cancels to the extent that both compared groups
are present *inside* the same clusters:

| the comparison being coloured                                                                | cluster effect                                       |
|-----------------------------------------------------------------------------------------------|------------------------------------------------------|
| sex, age, education, occupation, opinion — **crossed** with PSUs                              | largely **cancels**; the flat basis is close to right |
| region, urban/rural, commune size, neighbourhood type, immigrant density — **nested** in PSUs | does **not** cancel; as wrong as for a level, or worse |

This maps a statistical condition onto a question a user can answer about their own table: *is the
row variable something that varies within a neighbourhood, or something that defines one?*

### 4.3 It is the convention, not an idiosyncrasy

| tool                     | given a weight and nothing else                                                              |
|--------------------------|-----------------------------------------------------------------------------------------------|
| R `survey`               | `svydesign(ids = ~1, weights = ~w)` — exactly this basis                                      |
| Stata                    | `svyset [pw=w]` — same estimator                                                              |
| SAS                      | `PROC SURVEYMEANS` with `WEIGHT` only                                                         |
| SPSS                     | bare `WEIGHT` treats weights as **frequencies** — the one common tool whose default is worse  |
| jamovi / JASP / PSPP     | weights-only, generally without even the flat correction                                      |

In applied sociology the honest picture is: mostly weights-only, mostly unstated — because design
variables are often not shipped with the file, and because the tool does not ask. For that
population the flat basis is a **raising** of the standard, not a lowering: the alternative actually
in use is the unweighted *n*.

### 4.4 The open caveats

Four residual problems, none of them fixed by the above:

1. **The degrees-of-freedom gap** — at the `"weights"` basis there is no design, so `degf` is absent
   and the interval is a `z` interval. Under the real clustered design the df is `#PSU − #strata`,
   often 15–60. Measured ×0.92. This is entirely separate from the variance error, does not go away
   when the variance happens to be right, and is structurally uncorrectable at the flat basis.
2. **`n_eff > n` surprises users.** It is *correct* under the flat design — informative weights can
   beat SRS — but a user seeing "effective n = 4 518" on a cell holding 469 respondents, with an
   interval *narrower* than the unweighted one, reads it as a bug. Nothing in the footer prepares
   them.
3. **`ci_beta` omits Korn & Graubard's df adjustment.** `survey::svyciprop(method = "beta")` shrinks
   the effective n by `(qt(α/2, n−1) / qt(α/2, degf))²` before the beta quantiles; `ci_beta()` applies
   Clopper–Pearson directly to `n_eff`. A no-op at the `"weights"` basis (`degf = n−1`); at the
   `"design"` basis with few PSUs it is not — on NHANES the factor is `(1.960/2.120)² = 0.855`.
   Opt-in only (`ci_method = "beta"`), design-basis only, but a genuine divergence from the method
   the option names.
4. **The `tab()` / `tab_reg()` asymmetry.** With default options, on the same weighted file, `tab()`
   gives raw-*n* intervals (`design_effect = FALSE`) and `tab_reg()` gives flat-design ones
   (`reg_inference()` forces the basis). The footers say which, but as two facts rather than one
   choice. ⚠ It is not arbitrary and cannot be removed cheaply: `tab_reg()`'s observed `Obs_*` columns
   must sit on the same basis as the `Model_*` columns beside them, and those come from `svyglm`,
   i.e. flat by construction. Making `tab_reg()` respect the option would break that pairing; making
   `tab()` default to `TRUE` would change every weighted table's stars.

⚠ Worth stating beside 4: the raw-*n* default corresponds to **no single probability model** — the
point estimate comes from the sampling-weight model, the interval from an SRS-of-the-respondents
model. Monte-Carlo coverage for raw-*n* runs 0.785–0.943 across every scenario and quantity, the
worst of the four bases in all cells, including the clustered scenario where the flat basis is also
wrong.

---

## 5. Re-running the evidence

- `dev/tests/testthat/test-survey-variance-sweep.R` and `test-survey-design-sweep.R` — the parity
  arm against `survey`, beyond the canonical block the shipped suite keeps.
- `dev/verify_reg_invariants.R` — every reachable family × effect × measure, for the regression side.
- The archived measurement reproducers, for the numbers quoted above:
  `dev/archive_2.0.0/survey_design_measurements.R`, `dev/archive_2.0.0/weights_stress_test.R`.

## 6. References

**Cell residuals.** Haberman (1973), *The analysis of residuals in cross-classified tables*,
Biometrics 29. · Sharpe (2015), *Your Chi-Square Test Is Statistically Significant: Now What?*, PARE
20(8). · García-Pérez & Núñez-Antón (2003), *Cellwise Residual Analysis in Two-Way Contingency
Tables*, EPM 63(5); (2014), *another nail in the coffin of conditional approaches to significance
testing*. · Benzécri, *L'analyse des données* (the contribution as a share of inertia). · Cibois
(1993), *Le PEM, pourcentage de l'écart maximum*, BMS 40. · IBM SPSS, *Interpreting adjusted
residuals in Crosstabs cell statistics*.

**Design-based variance.** Kish (1965), *Survey Sampling*; (1992), *Weighting: why, when and how?*,
JOS 8. · Lumley (2010), *Complex Surveys: A Guide to Analysis Using R*, Wiley. · Rao & Scott (1981,
1984), *The analysis of categorical data from complex sample surveys*, JASA 76 / AoS 12. · Korn &
Graubard (1998), *Confidence intervals for proportions with small expected number of positive
counts*, Survey Methodology 24. · Skinner, Holt & Smith (1989), *Analysis of Complex Surveys*,
Wiley. · Gabler, Häder & Lahiri (1999), *A model based justification of Kish's formula*, Survey
Methodology 25. · Little & Vartivarian (2005), *Does weighting for nonresponse increase the variance
of survey means?*, Survey Methodology 31(2). · Gelman (2007), *Struggles with survey weighting and
regression modeling*, Statistical Science 22(2). · Deville & Särndal (1992), *Calibration estimators
in survey sampling*, JASA 87.
