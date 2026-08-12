# Is a weights-only design effect statistically sound?

Scope, evidence and limits of tabxplor's **flat (`ids = ~1`) inference basis**

Date: 2026-08-12. Status: **STUDY ONLY — no R file was modified** (a parallel session holds the tree).
Working tree at `8b59c1f` (Last Phase z16-iiii), R 4.6.1, `survey` 4.5.

Companion documents: `dev/weights_framework_redesign.md` (the z16 design — §1 derives the closed form),
`dev/weights_framework_stress_test.md` (W1–W13), `dev/weights_framework_stress_test_2_post_z16.md`
(W-A…W-H), `dev/full_survey_design_scope.md` (the z14 Route-A study),
`dev/tabxplor_2.0.0_decisions.md` §14 and §51.

Every number below is produced by the four scripts in **Appendix A**, which are self-contained and
run against `survey`'s own shipped datasets (`api`, `nhanes`) plus one Monte-Carlo population — so
they are reproducible without any French file.

---

## 0. The one-page answer

**Yes, it is statistically sound — but "sound" here means something narrower than it sounds, and the
narrowness matters.**

The flat basis is not an *approximation* of your survey's variance. It is the **exact** variance of a
different, simpler design: single-stage, with-replacement, unequal-probability sampling. That
estimator is well defined, design-consistent for that design, and is exactly what every major
statistical package computes when you hand it a weight and nothing else (Stata `svyset [pw=w]`,
`survey::svydesign(ids = ~1, weights = ~w)`, SAS `PROC SURVEYMEANS` with only a `WEIGHT`). tabxplor's
closed form reproduces `survey` at that design to ratio `1.00000000` (§1.3, verified again here
against the package itself).

So the real question is not "is the estimator valid?" but **"how far is the design it assumes from
the design you actually have?"** — and there the answer is asymmetric in a way that decides
everything:

| what the flat basis omits              | direction of the error   | measured size, this document                                                                           |
|----------------------------------------|--------------------------|--------------------------------------------------------------------------------------------------------|
| **clustering / multi-stage selection** | **intervals TOO NARROW** | reported SE ×0.10 to ×0.67 of the truth — the real standard error can be **nine times** what it prints |
| stratification                         | intervals too wide       | ×1.005 (alone) to ×1.07 (with the `fpc`)                                                               |
| calibration / post-stratification      | intervals too wide       | ×1.08, and only on variables the margins predict                                                       |
| finite population correction           | intervals too wide       | ×1.01 here; ×1.00 at national sampling rates                                                           |

The three conservative omissions are worth **0–8 % each**, and only on the variables they touch. The
one anti-conservative omission can be worth an **order of magnitude**. They do not cancel; the
dangerous one dominates whenever it is present.

**Which leads to the one operational rule this document exists to state:**

> The flat basis is sound *and materially right* when the sample has **no clustering** — a
> register-drawn, web, telephone or mail sample of individuals, however unequal its weights.
> It is sound *but materially wrong* when the sample is a **face-to-face area sample** — the standard
> French, European and American national household survey — because those are clustered by
> construction, and the flat basis cannot see it.

Two facts strongly soften that, and they are the reason the current design is defensible rather than
merely conventional:

1. **The user's premise — "we only kept what widens intervals" — is half wrong, and z16 is why.**
   Kish's `deff = 1 + CV²(w)` can only ever widen. The **exact** flat form that replaced it can go
   either way, and does: measured on NHANES, the exact effective *n* for the race distribution is
   **11 336 against a raw n of 8 591** — an efficiency *gain* from informative weights, which Kish is
   structurally incapable of reporting (§3.1). So the framework does now carry "what narrows",
   as far as the weights alone can reveal it.
2. **Design effects are much smaller for what tabxplor actually colours.** A level (a marginal
   percentage) is the worst case. A *difference between two cells* and a *regression coefficient* are
   far more robust, because the cluster effect largely cancels in a within-cluster contrast. Measured
   on `apiclus1`: the mean is **×0.395** too narrow, the difference between two school types is
   **×0.883**, and the regression coefficient on `ell` is **×0.919** (§6). This is a known result
   (Skinner, Holt & Smith 1989) and it happens to align with tabxplor's whole visual grammar.

The residual, honest problems are in §7. The two worth the maintainer's attention are the
**degrees-of-freedom gap** (a second anti-conservatism, ~8 %, entirely separate from the variance)
and the **`tab()` / `tab_reg()` asymmetry** (a table and a regression on the same weighted file are
reported on two different bases unless the user sets an option).

---

## 1. What the flat basis actually is

### 1.1 The estimator

Every quantity tabxplor displays is a ratio of two weighted sums, `p̂ = A/B`, with linearized
influence contribution `z_k = (u_k − p̂ v_k)/B`. At `ids = ~1`, no strata, no `fpc`, no calibration,
`svyrecvar` collapses to a plain sum of squares with survey's `n/(n−1)` factor — and because
`Σ w_k z_k = 0` exactly, the centering is a no-op. Writing `A` for the cell's own `Σw²`, `S` for
`Σw²` over the base domain and `B` for `Σw` over that domain:

```
Var(p̂) = n/(n−1) · [ A(1−p)² + (S−A)p² ] / B²
Var(x̄) = n/(n−1) · [ Σw²x² − 2x̄·Σw²x + x̄²·Σw² ] / B²
n_eff  = p(1−p)/Var(p̂)          (Korn & Graubard's device)
```

This is the **Hansen–Hurwitz / with-replacement (WR) unequal-probability variance**. It is not a
heuristic and it is not "the weights part of the design effect": it is a complete design-based
variance, for the design `ids = ~1`.

### 1.2 What it is *not*

It is **not** Kish. Kish's `n_eff = (Σw)²/Σw²` is the formula above with the cell's own `A` discarded
and replaced by its proportional share `p·S`; the bracket then collapses to `S·p(1−p)` and the cell
drops out entirely. In tabxplor since z16 that expression survives only as the **degenerate-cell
limit** (`svy_flat_base_neff`, `R/survey-variance.R:200`), reached when `p ∈ {0,1}` or the sums are
unusable. Kish's number is a property of the weights alone and cannot move with the variable being
tabulated; the exact form moves, in both directions (§3.1).

It is also **not** "treating weights as frequency weights". That would divide by `Σw` and is what
`glm(weights=)` and SPSS's default `WEIGHT` command do; it is wrong by orders of magnitude on a
population-scale weight and tabxplor never does it.

### 1.3 Verified parity with `survey`, against the package itself

Loading tabxplor and comparing its stored `n_eff` with `survey`'s own variance inverted back to a
Korn–Graubard effective *n*, on `apistrat` (`pct = "row"`, `wt = pw`, `design_effect = TRUE`):

```
 stype pct_tab pct_svy neff_tab neff_svy n_raw ratio
     E    0.91    0.91    99.50    99.50    91     1
     H    0.52    0.52    49.75    49.75    26     1
     M    0.70    0.70    49.75    49.75    35     1
```

Ratio `1` on every cell. The implementation is right; everything that follows is about the
*statistical position*, not about a bug.

### 1.4 Who gets which basis today

`svy_inference_basis(design_spec, wt, force)` (`R/survey-design.R:141`) resolves exactly one basis per
build:

| basis              | trigger                                                                             | interval based on                     |
|--------------------|-------------------------------------------------------------------------------------|---------------------------------------|
| `"design"`         | a `survey::svydesign` passed as `data`                                              | `svyrecvar` on the real design        |
| `"weights"`        | `wt` given **and** (`options(tabxplor.design_effect = TRUE)` **or** `force = TRUE`) | the flat closed form                  |
| `"n"`              | everything else — **including the default weighted call**                           | the raw unweighted count              |
| `"design_partial"` | `"design"` but a producer bailed during this build                                  | the flat form, and the footer says so |

**`tab_reg()` passes `force = TRUE`** (`R/tab_reg.R:1644`, `:3559`). So:

> On the same weighted file, with default options, `tab()` reports intervals on the **raw n** and
> `tab_reg()` reports them on the **flat design**. The crosstab is narrower than the regression
> beside it, for the same data, for a reason no footer explains as a choice.

That asymmetry is deliberate and defensible in its parts — `tab_reg()`'s crude `Obs_*` columns must
match the `Model_*` columns beside them, which come from `svyglm` and are therefore flat by
construction — but it is a package-level inconsistency, and it is the one thing in this study that is
an *integration* question rather than a statistical one (§7.4).

---

## 2. Is it sound? The formal answer

### 2.1 Yes — for a design that is not yours

Design-based inference conditions on the sampling mechanism. The flat estimator is unbiased (to
first order) for the variance under WR unequal-probability sampling. Handing it a file drawn under a
different mechanism does not make it "wrong software"; it makes it **an answer to a different
question**, whose distance from your question is a property of your design, not of the estimator.

That framing is the standard one, and it is why the major packages default to it silently rather
than refusing: with only a weight, WR-unequal-probability is the *weakest assumption under which the
weight has a probability interpretation at all*.

### 2.2 The four omissions, and their signs

| omission                              | mechanism                                                                             | sign                  | why                                                                                                                                                  |
|---------------------------------------|---------------------------------------------------------------------------------------|-----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------|
| **stratification**                    | strata absorb between-stratum variance                                                | **conservative**      | the flat estimator charges you for variance the design removed                                                                                       |
| **clustering**                        | units in a PSU are correlated (ICC > 0)                                               | **anti-conservative** | the flat estimator counts *n* independent draws where there are effectively `n/(1+(b̄−1)ρ)`                                                          |
| **calibration / post-stratification** | calibrated weights are estimated; the residual, not the variable, drives the variance | **conservative**      | Deville–Särndal (1992): calibration reduces variance for variables correlated with the margins; treating the weights as fixed forgoes that reduction |
| **fpc**                               | sampling without replacement from a finite frame                                      | **conservative**      | omitting `(1−f)` overstates by that factor                                                                                                           |

Two caveats on the signs. Stratification is conservative **for proportional / near-optimal
allocation**; a deliberately disproportionate design (oversampling a small group) can in principle
run the other way. And calibration's conservatism only materialises for variables actually
correlated with the calibration margins — for an unrelated variable, the g-weight correction is
near-neutral.

### 2.3 Measured, on real files whose designs are published

`survey`'s own datasets, so the "true" design is not a modelling assumption but the one the data
producer documented. `se_ratio = SE(flat) / SE(published design)`; `halfwidth_ratio` additionally
carries the `t` critical value at each design's own degrees of freedom.

```
                      file              quantity   se_ratio  degf_full  degf_flat  halfwidth_ratio
        apiclus1 (cluster)                ~api00      0.332         14        182            0.305
        apiclus1 (cluster) ~I(sch.wide == "Yes")      1.207         14        182            1.110
        apiclus1 (cluster)                ~meals      0.314         14        182                –
        apiclus2 (2-stage)                ~api00      0.665         39        125                –
        apiclus2 (2-stage)                ~meals      0.525         39        125                –
     apistrat (stratified)                ~api00      1.019        197        199            1.019
     apistrat (stratified)                ~meals      1.026        197        199                –
     apistrat (stratified) ~I(sch.wide == "Yes")      1.071        197        199            1.071
 apistrat (strat., no fpc)                ~api00      1.005        197        199                –
                    NHANES              ~HI_CHOL      0.864         16       8590            0.799
                    NHANES         ~I(race == 1)      0.112         16       8590            0.104
                    NHANES               ~agecat      0.790         16       8590            0.730
```

Read this table twice.

* **`apistrat`** — stratified, no clusters. The flat basis is **1.9 % to 7.1 % conservative**. Drop
  the `fpc` from the comparison and it is 0.5 %. This is the good case, and it is quantitatively
  boring: ignoring stratification costs almost nothing here.
* **`apiclus1`** — a one-stage cluster sample. The flat basis reports a standard error **one third**
  of the truth for a school-level mean. A 95 % interval is three times too short. Every star in that
  table would be wrong.
* **NHANES** — the archetype of a public-use file, stratified multistage. `HI_CHOL` is mildly
  affected (×0.86); the **distribution of `race`** is off by a factor of **nine**, because NHANES
  oversamples by race within PSUs, so that variable is precisely the one the design is built around.
* **The `degf` column is a second, independent error.** At the flat basis the design df is `n−1`
  (182, 8 590); the real design has `#PSU − #strata` (14, 16). `qt(.975, 14) = 2.145` against
  `qt(.975, 182) = 1.973`. That is another **8–9 % too narrow**, on top of the variance, and it
  applies *even when the variance happens to come out right*.

### 2.4 Measured, against the Monte-Carlo truth

A fixed finite population of 300 000 in 3 000 PSUs and 6 strata; unequal selection (one person per
household ⇒ weight ∝ household size, `CV(w) ≈ 0.56`, Kish deff 1.31); age-dependent nonresponse and
a linear calibration on age × sex. 400 replicates. Two designs, **same n ≈ 2 400**:

* **A** — stratified two-stage cluster (40 PSUs/stratum × 10 individuals): the face-to-face survey.
* **B** — stratified one-stage individual sample: the register/web survey.

`r_x = mean(SE_x) / MonteCarloSD`, so **1.000 is correct and < 1 is too narrow**:

```
    scenario quantity  mc_se r_full r_flat r_kish r_rawn   deff(flat/rawn)
   A-cluster     y_hi 0.0203  0.982  0.746  0.746  0.639   1.362
   A-cluster     y_lo 0.0143  1.077  1.058  1.057  0.906   1.362
   A-cluster    y_cal 0.0143  0.979  1.059  1.046  0.896   1.395
   A-cluster     y_hh 0.0158  0.965  0.967  0.966  0.828   1.363
   A-cluster      dif 0.0323  0.966  0.960     NA     NA      NA
   A-cluster      reg 0.1210  1.056  1.061     NA  0.910   1.359
 B-nocluster     y_hi 0.0146  1.024  1.042  1.042  0.891   1.366
 B-nocluster     y_lo 0.0143  1.059  1.062  1.062  0.908   1.366
 B-nocluster    y_cal 0.0137  1.017  1.103  1.090  0.933   1.399
 B-nocluster     y_hh 0.0143  1.064  1.067  1.066  0.912   1.369
 B-nocluster      dif 0.0317  0.977  0.980     NA     NA      NA
 B-nocluster      reg 0.1320  0.974  0.977     NA  0.836   1.364
```

95 % coverage:

```
    scenario quantity c_full c_flat c_kish c_rawn
   A-cluster     y_hi  0.953  0.848  0.848  0.785
   A-cluster     y_lo  0.975  0.968  0.968  0.910
   A-cluster    y_cal  0.950  0.965  0.965  0.938
   A-cluster     y_hh  0.932  0.932  0.932  0.887
   A-cluster      dif  0.953  0.950     NA     NA
   A-cluster      reg  0.970  0.973     NA  0.943
 B-nocluster     y_hi  0.958  0.963  0.958  0.917
 B-nocluster     y_lo  0.953  0.953  0.955  0.922
 B-nocluster    y_cal  0.955  0.965  0.965  0.938
 B-nocluster     y_hh  0.960  0.960  0.960  0.930
 B-nocluster      dif  0.948  0.950     NA     NA
 B-nocluster      reg  0.930  0.930     NA  0.887
```

Four things this settles.

1. **Scenario B is the case for the option.** With no clusters, the flat basis attains nominal
   coverage on every quantity (0.953–0.965) while the package's **default** raw-n basis sits at
   **0.917–0.938**. Switching `tabxplor.design_effect` on is straightforwardly right there.
2. **Scenario A is the case against relying on it.** `y_hi`, the strongly geographic variable, gets
   84.8 % coverage instead of 95 %. The full design gets 95.3 %.
3. **`y_cal` isolates the calibration cost**: flat 1.059 against full 0.979 in A, 1.103 against 1.017
   in B — the flat basis is about **8 % too wide** on the variable the calibration was designed to
   help, and only on that variable. That is the whole of "what narrows" that is being forgone,
   measured. It is real, it is one-directional, and it is an order of magnitude smaller than the
   clustering error.
4. **The difference and the regression coefficient survive scenario A** (0.960 and 1.061) while the
   level does not (0.746). This is §6.

---

## 3. The user's specific worry: "only what widens, not what narrows"

### 3.1 The premise is half wrong — and z16 is the reason

With Kish, the worry was exactly right: `deff = 1 + CV²(w) ≥ 1` by construction, so the *only*
possible effect of the weights on the interval was to widen it, whatever the data said. That is the
standard criticism of Kish's measure (Spencer 2000; Park & Lee 2001), and it is why the exact form
replaced it.

The exact flat variance is not bounded below by the SRS variance. Measured:

```
     file              quantity     n     Kish  exact_flat  exact_FULL
 apiclus1 ~I(sch.wide == "Yes")   183    183.0       182.0       265.1
 apistrat ~I(sch.wide == "Yes")   200    168.6       209.6       240.4
   NHANES              ~HI_CHOL  8591   5376.7      4501.3      3357.3
   NHANES         ~I(race == 1)  8591   5376.7     11335.5       143.3
```

* On `apistrat`, Kish says "you have 168.6 observations' worth"; the exact form says **209.6 — more
  than the raw 200**; the true design value is 240.4. The exact form moves *towards* the truth in
  the direction Kish forbids.
* On NHANES `race`, the exact flat form claims **11 336 effective observations from 8 591 rows**. On
  a live table this happens often: **11 of 25 cells** in a weighted NHANES age × race crosstab have
  `n_eff` above their own base *n*.

So the framework does capture efficiency gains — everything about them that is visible in the
weights alone. What it cannot capture is the gain from *structure*, because structure is exactly the
information that was not shipped.

This is confirmed inside the package, not only in `survey`: on `apistrat` with `wt = pw` and the
option on, the Total row's stored `n_eff` is **209.6 against a base of 200**. It is also the reason
`?tab` currently carries a sentence that z16 made false — see **B.1**.

### 3.2 The omissions are not symmetric in magnitude

Collecting the measurements:

| omitted feature              | direction                              | measured range                   | when it bites                                           |
|------------------------------|----------------------------------------|----------------------------------|---------------------------------------------------------|
| informative weights          | either way — **captured, not omitted** | ×0.6 to ×1.4                     | always                                                  |
| finite population correction | wider                                  | ×1.00–1.01                       | high sampling fraction (business surveys, small strata) |
| stratification               | wider                                  | ×1.005 alone, ×1.07 with the fpc | strata predictive of the outcome                        |
| calibration                  | wider                                  | ×1.00–1.08                       | variables correlated with the margins                   |
| **clustering**               | **narrower**                           | **×0.10–0.67**                   | any area / face-to-face sample                          |
| **design df**                | **narrower**                           | **×0.92**                        | few PSUs (NHANES: 16 df)                                |

(The stratification row separates the two only for `api00`, where both a with-fpc and a no-fpc
comparison were run; the other apistrat figures in §2.3 are the combined stratification + fpc
effect.)

The user's intuition of an unfair trade is therefore correct in *sign* but inverted in *stakes*: the
package is not systematically conservative from having kept only the widening term. It is
systematically **anti-conservative on clustered files**, because the single largest term is the one
it cannot see, and the conservative terms it also cannot see are far too small to compensate.

### 3.3 The one case where they genuinely do offset

There is a real class of surveys — heavily calibrated, lightly clustered (large PSUs, small
within-PSU take, low ICC on attitudes) — where a ~5 % conservative calibration omission and a ~5 %
anti-conservative clustering omission roughly cancel and the flat interval is close to right by
accident. Several French household surveys with a "logement" sampling frame and a small number of
dwellings per cluster are plausibly in that class. This is not something a package can detect, and
it should not be advertised as a justification: the cancellation is unverifiable from the file.

---

## 4. Is it common practice?

### 4.1 What the software does

| tool                     | with a weight and nothing else                                                                                                                                                    |
|--------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **R `survey`**           | `svydesign(ids = ~1, weights = ~w)` — exactly tabxplor's flat basis. `~1` "indicates no clusters"; omitting `fpc` means with-replacement                                          |
| **Stata**                | `svyset [pw=w]` — same estimator; Stata additionally *ignores* later-stage design variables when the first-stage fraction is small, on the same WR argument                       |
| **SAS**                  | `PROC SURVEYMEANS` with `WEIGHT` only                                                                                                                                             |
| **SPSS**                 | Complex Samples requires a plan file; the bare `WEIGHT` command instead treats weights as frequencies — the one common tool whose weights-only default is *worse* than tabxplor's |
| **jamovi / JASP / PSPP** | weights-only, and generally without even the flat design correction                                                                                                               |

So tabxplor's flat basis is not an idiosyncratic choice; it is *the* convention, and its opt-in
version is stricter than the two point-and-click tools tabxplor competes with.

### 4.2 United States / United Kingdom

The professional norm is unambiguous: **where design variables ship, use them.** NCHS's own NHANES
tutorial devotes a module to variance estimation and instructs analysts to use `SDMVSTRA`/`SDMVPSU`;
ONS's methodology working paper 9 exists to give social-survey users the design information; the
NLSY documentation warns that respondents "come in geographic clusters" so SRS standard errors "may
be too small"; IPUMS publishes a user note on precisely this. The recurring sentence across all of
them is that ignoring clustering "will likely lead to standard errors that are underestimated,
possibly leading to results that seem to be statistically significant, when in fact, they are not"
(IPUMS).

At the same time, weights-only analysis is extremely common in applied work, both because analysts
skip the step and because a large and growing share of data — opt-in web panels, quota samples,
convenience samples with post-stratification — has **no design to declare**. For those, the flat
basis is not a shortcut; it is the only defensible thing available, and even then the weights are
model-derived rather than sampling-derived.

### 4.3 Europe

The **European Social Survey** is the reference case and it is deliberately structured around this
problem. It ships `anweight` (design × post-stratification × population size), states that "weights
should always be used", and publishes clustering and stratification identifiers **separately**, in a
Sample Design Data File (`psu`, `stratum`). Two consequences for tabxplor's users:

* The SDDF is a *separate* file that must be merged. A user who downloads the main ESS file and
  applies `anweight` is, by default, in tabxplor's flat basis.
* ESS's own precision work explicitly decomposes `deff = deffp × deffc` — the weighting component
  and the **clustering** component, `deffc = 1 + (b̄ − 1)ρ`. The flat basis is, in ESS's own
  vocabulary, an exact `deffp` and a `deffc` silently set to 1.

That decomposition is a very usable teaching device for the vignette, because it names precisely
what the option does and does not buy.

### 4.4 France

This is the case the question is really about, and it is the least favourable one.

* **The designs are clustered.** INSEE's *Enquête Emploi* is drawn as "groupes de logements
  voisins" — clusters ("grappes") of **about 20 contiguous dwellings**, six clusters per sector,
  each cluster surveyed for six consecutive quarters. INSEE's own documentation names the
  consequence: within a cluster, "les ménages ont tendance à avoir des caractéristiques
  socio-économiques proches", the *effet de grappe*, "qui conduit, à taille d'échantillon identique,
  à une précision plus faible". A cluster of 20 dwellings with even a modest ρ produces a `deffc`
  well above 1 for anything geographic — housing, income, immigration, unemployment, voting.
* **The designs are calibrated.** *Calage sur marges* (Deville–Särndal 1992, implemented in INSEE's
  Calmar since the 1990s, and in the R package **`icarus`**) is standard on essentially every INSEE
  household survey, with margins frequently taken from the Enquête Emploi. So the conservative
  omission is real too — but, per §2.4, worth ~8 % against the clustering's potential hundreds.
* **The design variables are typically not in the FPR.** *Fichiers de production et de recherche*
  are defined as an intermediate confidentiality level between public and secure-access data; the
  PSU is a fine geographic unit and is disclosive, so it is generally not diffused. INSEE's own
  answer to this is not to publish PSUs but to publish **precision tables** and to do variance
  estimation internally — historically the SAS macro *Poulpe*, today the R package **`gustave`**
  (InseeFr), which exists precisely to let a producer ship a *variance wrapper* (`qvar()`,
  handling stratified SRS, reweighting in homogeneous response groups, and calibration) rather than
  the design variables themselves.

**The practical French position is therefore: a researcher on Quetelet-Progedo data usually cannot
do better than the flat basis, whatever they know about the design.** That is an argument *for*
offering it — a wrong-but-labelled interval beats an unlabelled raw-n one — and simultaneously the
strongest argument for the footer never overstating what was computed.

> ⚠ I verified the sampling structure (Enquête Emploi clusters of ~20 dwellings), the FPR
> confidentiality tier and INSEE's variance-estimation tooling from published INSEE and
> Comité-du-secret documentation. I did **not** audit the variable list of any specific FPR
> extraction. Before this claim reaches a vignette, it is worth checking one real file (e.g. an
> `Enquête Emploi` or `SRCV/EU-SILC` FPR) for the presence of a cluster or stratum identifier — some
> files do ship a coarsened one, and a few ship replicate weights.

### 4.5 What applied sociology does

Honestly: mostly weights-only, and mostly without saying so. The two structural reasons above (no
design variables shipped; the tool doesn't ask) mean that the flat basis is not a lowering of the
standard in the population tabxplor serves — it is a **raising** of it, since the alternative
actually in use is the unweighted *n*, which is tabxplor's own current default (§7.4).

---

## 5. Grounded criticisms in the literature

Ordered by how much they bear on the current design.

**5.1 Kish's `1 + CV²(w)` is a poor measure when the weights are informative.** Kish (1965, 1992)
derives it under "all observations approximately uncorrelated with the same response variance"; it
is a property of the weights only and cannot detect that unequal probabilities may be *more*
efficient than equal ones. **Spencer (2000)** gives the corrected approximation for the case where
the measurement correlates with the selection probability, and finds Kish's value can be
substantially too high; **Park & Lee (2001, 2004)** extend it to the ratio-mean. **Gabler, Häder &
Lahiri (1999)** give the model-based conditions under which Kish's version is justified.
→ *Status in tabxplor: addressed.* z16 replaced Kish with the exact form, which is Spencer's concern
resolved not approximated. Kish survives only as the degenerate-cell limit.

**5.2 Weighting does not necessarily increase variance.** **Little & Vartivarian (2005)** show the
"bias–variance trade-off" framing of weighting is an oversimplification: a covariate that predicts
the *outcome* (not merely the response propensity) reduces variance as well as bias.
→ *Status: partly addressed.* The exact form captures this at the estimator level (`n_eff > n` does
happen). What it cannot capture is the variance reduction from the *calibration step itself*, which
requires the g-weights and hence the design object. Measured cost: ~8 % (§2.4).

**5.3 Ignoring clustering understates standard errors.** The oldest and most consequential result in
the field (Kish 1965; Skinner, Holt & Smith 1989; Rao & Scott 1981, 1984 for the χ² case;
Lumley 2010). Every major producer restates it in its user documentation.
→ *Status: unaddressed by construction, and unaddressable without the design variables.* This is the
single real limitation, and it must be stated in the docs rather than mitigated in code.

**5.4 Design effects for analytic statistics are smaller than for descriptive ones.** Skinner, Holt
& Smith (1989); Holt, Smith & Winter (1980), *Regression Analysis of Data from Complex Surveys*; the
UN *Household Sample Surveys* handbook chapter 6. Design effects for regression coefficients "are
often found to be less than design effects for the mean of the dependent variable" and "are like
those for differences between means".
→ *Status: this is the load-bearing mitigation for tabxplor specifically.* See §6.

**5.5 Whether to weight a regression at all.** A live methodological dispute orthogonal to the
variance question: **Solon, Haider & Wooldridge (2015)**, *What Are We Weighting For?* (JHR
50(2):301–316) distinguish descriptive from causal targets and find that each of the three usual
motives for weighting "sometimes does not apply in situations where practitioners often assume it
does"; **Gelman (2007)**, *Struggles with Survey Weighting and Regression Modeling*; **Bollen et al.
(2016)**, *Are Survey Weights Needed?* (Annual Review of Statistics); **Winship & Radbill (1994)**.
The consensus for tabxplor's audience is the uncontested half: **weight descriptive statistics**;
for regression, weighting protects against informative sampling at a cost in efficiency.
→ *Status: out of scope but worth one sentence in the reg vignette*, because a user who reads that
`tab_reg()` is "design-based" may over-read it as "therefore correct for causal inference".

**5.6 The with-replacement assumption and the fpc.** Omitting `(1−f)` is conservative and negligible
at national sampling rates (f < 1 %), but not for business surveys, small-region studies or
census-like extractions where strata are sampled at high rates.
→ *Status: acceptable, one doc sentence.*

**5.7 Estimated weights treated as fixed.** Nonresponse-adjusted and calibrated weights are random.
Deville & Särndal (1992) and the subsequent literature (Deville, Särndal & Sautory 1993; Demnati &
Rao 2004) give the correct treatment; treating them as fixed is conservative for calibration.
→ *Status: acceptable, and the direction is the safe one.*

---

## 6. The mitigation that actually matters: levels vs differences

tabxplor almost never asks the user to read a level in isolation. Its colours score a **cell against
a reference**, its stars test a **difference**, `tab_reg()` reports **coefficients**, and the
`adjustment` / `between_groups` measures test **gaps between two estimates**. The literature (§5.4)
says design effects are smaller for exactly those quantities. Measured here:

```
     file outcome       by  lvl_ratio  dif_ratio     (SE flat / SE full design)
   NHANES HI_CHOL RIAGENDR      1.071      1.253
   NHANES HI_CHOL     race      0.990      0.925
   NHANES HI_CHOL   agecat      1.014      0.905
 apiclus1   api00    stype      0.395      0.883
```

and, for regression coefficients:

```
     file                   term  se_flat/se_full
 apiclus1                  meals            0.651
 apiclus1                    ell            0.919
 apistrat                  meals            1.029
 apistrat                    ell            1.014
   NHANES  factor(agecat)(19,39]            1.056
   NHANES  factor(agecat)(39,59]            0.960
   NHANES factor(agecat)(59,Inf]            0.983
   NHANES      factor(RIAGENDR)2            1.122
```

The `apiclus1` line is the demonstration: the same file where a **mean** is 2.5× too precise gives a
**difference** that is 13 % too precise and a regression slope that is 8 % too precise. The
Monte-Carlo agrees — scenario A, `dif` 0.960 and `reg` 1.061 against `y_hi` 0.746.

**The mechanism, and its limit.** The cluster effect cancels in a contrast to the extent that both
compared groups are present *inside* the same clusters. So:

| the comparison tabxplor colours                                                               | cluster effect                                                           |
|-----------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| sex, age, education, occupation, opinion — **crossed** with PSUs                              | largely **cancels**; the flat basis is close to right                    |
| region, urban/rural, commune size, neighbourhood type, immigrant density — **nested** in PSUs | does **not** cancel; the flat basis is as wrong as for a level, or worse |

This is the single most useful sentence the vignette could carry, because it maps a statistical
condition onto a question the user can actually answer about their own table: *is the row variable
something that varies within a neighbourhood, or something that defines one?*

---

## 7. Caveats and open points specific to tabxplor's implementation

Each is a finding, not a prescription; §8 collects them for decision.

### 7.1 The degrees-of-freedom gap is a second, invisible anti-conservatism

At the `"weights"` basis there is no design, so `degf` is absent and `conf_level_to_crit()` falls to
`Inf` — a `z` interval. Under the real clustered design the df is `#PSU − #strata`, often 15–60.
Measured: **×0.92** on NHANES and apiclus1 (§2.3). This is *entirely separate* from the variance
error and does not go away when the variance happens to be right. It is also the one part that is
structurally uncorrectable at the flat basis: with no PSUs, `n−1` is genuinely the right df for the
design being assumed.

### 7.2 `n_eff > n` will surprise users, and is currently unexplained

11 of 25 cells in a weighted NHANES crosstab. It is *correct* under the flat design — informative
weights can beat SRS, which is Spencer's point — but a user who sees "effective n = 4 518" on a cell
containing 469 respondents, and an interval *narrower* than the unweighted one, will read it as a
bug. Nothing in the current footer or docs prepares them. (Note it is bounded in practice by the
degenerate fallback `B²/S`, but not in general.)

### 7.3 `ci_beta` omits Korn & Graubard's df adjustment

`survey::svyciprop(method = "beta")` shrinks the effective n before the beta quantiles:

```r
n.eff <- n.eff * (qt(alpha/2, nrow(design) - 1) / qt(alpha/2, degf(design)))^2
```

`ci_beta()` (`R/tab-agg.R:390`) takes no `df` and applies Clopper–Pearson directly to `n_eff`. At the
`"weights"` basis this is a no-op (`degf = n−1`, factor 1). At the `"design"` basis with few PSUs it
is not: on NHANES the factor is `(1.960/2.120)² = 0.855`, so the effective n is ~15 % too large and
the interval correspondingly too narrow. Small, opt-in-only (`method_cell = "beta"`), design-basis
only — but it is a genuine divergence from the method the option names.

### 7.4 The `tab()` / `tab_reg()` basis asymmetry

Restating §1.4 because it is the largest *integration* finding: with default options, on the same
weighted file, `tab()` gives raw-n intervals and `tab_reg()` gives flat-design ones. The footers do
now say which — that is the z16 achievement — but they say it as two facts, not as one choice. Three
observations:

* The raw-n default corresponds to **no probability model at all**: the point estimate comes from
  the sampling-weight model, the interval from an SRS-of-the-respondents model.
* The Monte-Carlo puts raw-n coverage at **0.785–0.943** across every scenario and quantity — the
  worst of the four positions in all twelve cells, including the clustered scenario where the flat
  basis is also wrong.
* `tab_reg()`'s force is not arbitrary: its crude `Obs_*` columns must be on the same basis as the
  `Model_*` columns beside them, and those come from `svyglm`, i.e. flat by construction. Making
  `tab_reg()` respect the option would break that pairing; making `tab()` default to `TRUE` would
  change every weighted table's stars.

### 7.5 The `contrib` residual's base is first-order only

Already stated in `?tab` and in the z16-iiii record: one Rao–Scott δ̄ per table, not a per-cell
design residual. Unchanged by this study; noted so the list is complete.

### 7.6 Non-probability samples

For an opt-in web panel with model-derived (raking / propensity / MRP) weights, the flat basis
computes a **sampling** variance for something that has no sampling distribution. It will be too
narrow, sometimes wildly, and no design-based framework can fix that. The relevant literature is
different (AAPOR task-force reports on non-probability sampling; Mercer et al.). Worth one honest
sentence, because a growing share of tabxplor's users will be in this case and the footer's
"confidence intervals and tests account for the weighting" reads as more of a guarantee than it is.

### 7.7 Stale artefact

`R/jmvtab.h.R:146-152` and `:620-622` still declare a `test_robust` option with values
`c("classic", "kish")`, documented as a "first-order Rao-Scott rescale to the effective sample size".
It is dead — `jamovi/jmvtab.a.yaml` has `design_effect` instead and no `.b.R` reads `test_robust` —
but `.h.R` is generated, so it will keep saying "kish" until the maintainer's next
`jmvtools::prepare()`. (Already flagged in the z16 record as an open maintainer step; repeated here
because it is the last place in the package that names a retired method.)

---

## 8. Verdict, and the questions this raises

### 8.1 Verdict

**The flat basis is statistically sound, correctly implemented, and the right default *offer*.** It
is the industry-standard estimator for the information the user has; tabxplor's version of it is
exact rather than approximate, which is better than Kish and better than what SPSS, jamovi and JASP
do; and it is verified against `survey` to machine precision.

**Its soundness as a description of the user's actual survey is conditional on there being no
clustering**, and the French, European and American face-to-face household surveys that tabxplor's
audience uses are clustered by construction. On those files the flat interval can be several times
too narrow for a marginal distribution — though only ~10 % too narrow for the differences and
coefficients tabxplor actually colours.

**The asymmetry the question worries about is real but inverted**: the framework is not left
systematically conservative by keeping only the widening term. Since z16 it *does* carry the
narrowing term visible in the weights (`n_eff > n`), and what it omits is worth ~8 % conservative
against up to ~900 % anti-conservative.

### 8.2 What follows — for maintainer decision

No code change is recommended in this document; these are the questions the evidence raises,
ordered by how cheap and how clearly-right they are.

**Documentation-only, and clearly right** (all detailed in Appendix B — the *statements*, not the
computations, are what z16 left behind):

1. **`?tab` still says "a design can make an interval narrower, which weights alone never can"**
   (B.1). Measured false: the exact flat form does exactly that, on 11 of 25 cells of a weighted
   NHANES crosstab. The clause describes Kish, which z16 retired.
2. **The constant-weight identity `n_eff = n(n−1)/n`** (B.2) holds only where the cell's base is the
   whole leaf; the general form is `n_base · (N−1)/N`.
3. **`design_partial` is documented nowhere user-facing** (B.4) — four runtime bases, three
   documented positions.
4. **Two stale "Kish" strings in `jamovi/i18n/*.po*`** (B.6), plus the `svrepdesign` self-contradiction
   in `?tab_reg`.

**Documentation, and a judgement call:**

5. **Should the docs quantify the clustering risk, and carry §6's crossed-vs-nested rule?** (B.7).
   This is the single highest-value addition the evidence supports: the docs already say *what* the
   option is blind to, in seven places; they do not say that it is worth ×0.10–0.67 on a face-to-face
   survey, nor that it largely cancels in the differences tabxplor actually colours. The footer
   sentence must stay short, so this belongs in the Weights section of the two intro vignettes.
   **Maintainer’s decision: document. More generally, the weights part of the vignette should be redone, and explain very cleary and very concisely to non-experts users / literary students what the 3 weights position do (the facts should make them understand that to activate the design effect option is often the right choice without being authoritarian about it ; from the current file, insert the statements that are the most understandable by literary students, the most important statements to choose right in real-world use cases, like SEs are anticonservative with face-to-face survey with homes clusters like Enquête Emploi, calibration and strata can only narrow the intervals a bit but do not except a miracle, etc. Since many French users, including colleages, know nothing about survey-design, the section should be well thought, balanced between concision and useful informations for real-world use cases, etc.)**
6. **`n_eff > n`** (§7.2): document as expected behaviour, or surface it? A user seeing "effective
   n = 4 518" on a cell of 469 respondents will read it as a bug.
   **Maintainer’s decision: document.**
7. **Non-probability weights** (§7.6): one sentence, because the footer's "account for the weighting"
   reads as more of a guarantee than it is for a raked opt-in panel.
   **Maintainer’s decision: not needed.**

**Code, small and contained:**

8. **`ci_beta`'s missing df adjustment** (§7.3 / B.5): a genuine divergence from the method
   `?tab_ci` names, at the `"design"` basis only, worth ~15 % on NHANES. One argument and one line.
   **Maintainer’s decision: go.**

**Architectural, and genuinely open:**

9. **The `tab()` / `tab_reg()` basis asymmetry** (§7.4): leave, document as a deliberate pairing
   rule, or reconsider the `tab()` default. The Monte-Carlo puts the current `tab()` default last
   on coverage in all twelve cells tested (0.785–0.943) — but changing it moves every weighted
   table's stars, which is a release-scale decision, and the docs already state the position
   honestly in the footer. Recorded, not recommended.
   **Maintainer’s decision: leave. If should be documented enough, where the regression vignette present empirical=TRUE for different cases and say how to match the empirical counterpart using tab(), but without endless repetitions.**
10. **The degrees-of-freedom gap** (§7.1): structurally uncorrectable at the flat basis (with no
    PSUs, `n−1` *is* the right df for the design being assumed), so this is a documentation item at
    most — but it is a real second ×0.92 that no sentence currently mentions.
    **Maintainer’s decision: mention very briefly in a expert section.**
11. **A `deffc` escape hatch?** ESS's own `deff = deffp × deffc` decomposition suggests letting a
    user who *knows* their survey's published design effect apply it without design variables — e.g.
    a user-supplied divisor on `n_eff`. This would be a new knob against the roadmap's direction of
    travel, and it would let users assert precision they cannot verify from the file. Recorded as
    considered, **with a lean against**.
    **Maintainer’s decision: not needed.**

---

## 9. Reproducibility

Four scripts, all self-contained, all using data that ships with `survey` (plus one simulated
population). Runtime: ~1 s, ~3 s, ~1 s, and ~35 min (400 replicates) respectively. Reproduced in
**Appendix A**.

| script             | produces                                                                     |
|--------------------|------------------------------------------------------------------------------|
| `real_designs.R`   | §2.3 SE ratios, the `n_eff` ladder of §3.1, the regression ratios of §6      |
| `tabxplor_check.R` | §1.3 parity with the package, §2.3 half-width ratios, §7.2 `n_eff > n` count |
| `diffs.R`          | §6 level-vs-difference table                                                 |
| `sim2.R`           | §2.4 Monte-Carlo ratios and coverage                                         |

---

## 10. References

Bollen, K. A., Biemer, P. P., Karr, A. F., Tueller, S., & Berzofsky, M. E. (2016). Are survey weights
needed? A review of diagnostic tests in regression analysis. *Annual Review of Statistics and Its
Application*, 3, 375–392.

Deville, J.-C., & Särndal, C.-E. (1992). Calibration estimators in survey sampling. *JASA*, 87,
376–382.

Deville, J.-C., Särndal, C.-E., & Sautory, O. (1993). Generalized raking procedures in survey
sampling. *JASA*, 88, 1013–1020.

Gabler, S., Häder, S., & Lahiri, P. (1999). A model based justification of Kish's formula for design
effects for weighting and clustering. *Survey Methodology*, 25, 105–106.

Gelman, A. (2007). Struggles with survey weighting and regression modeling. *Statistical Science*,
22(2), 153–164.

Holt, D., Smith, T. M. F., & Winter, P. D. (1980). Regression analysis of data from complex surveys.
*JRSS A*, 143(4), 474–487.

Kish, L. (1965). *Survey Sampling*. Wiley. — (1992). Weighting: why, when and how? *Journal of
Official Statistics*, 8, 183–200.

Korn, E. L., & Graubard, B. I. (1998). Confidence intervals for proportions with small expected
number of positive counts estimated from survey data. *Survey Methodology*, 24, 193–201.

Little, R. J., & Vartivarian, S. (2005). Does weighting for nonresponse increase the variance of
survey means? *Survey Methodology*, 31(2), 161–168.

Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Wiley.

Park, I., & Lee, H. (2001, 2004). The design effect: do we know all about it? *ASA Proceedings /
Survey Methodology*.

Rao, J. N. K., & Scott, A. J. (1981, 1984). The analysis of categorical data from complex sample
surveys. *JASA* 76:221–230; *Annals of Statistics* 12:46–60.

Sautory, O. *Les méthodes de calage* / *Calibration methods*. INSEE.

Skinner, C. J., Holt, D., & Smith, T. M. F. (1989). *Analysis of Complex Surveys*. Wiley.

Solon, G., Haider, S. J., & Wooldridge, J. M. (2015). What are we weighting for? *Journal of Human
Resources*, 50(2), 301–316.

Spencer, B. D. (2000). An approximate design effect for unequal weighting when measurements may
correlate with selection probabilities. *Survey Methodology*, 26(2), 137–138.

Winship, C., & Radbill, L. (1994). Sampling weights and regression analysis. *Sociological Methods &
Research*, 23, 230–257.

**Web sources consulted** (accessed 2026-08-12):
INSEE, *Enquête emploi en continu — méthodologie* and *Traitement statistique* pages
(insee.fr/fr/metadonnees/source/…); INSEE, *Pour les enquêtes auprès des ménages, l'Insee rénove ses
échantillons*, Courrier des statistiques N4, 2020; Comité du secret statistique, *Fichiers de
production et de recherche (FPR)*; Progedo / Quetelet-Progedo Diffusion, data access conditions;
InseeFr/`gustave` and CRAN `icarus`; European Social Survey, *Weighting* and *Guide to Using Weights
and Sample Design Indicators with ESS Data*; ESS Round 9/11 *Sampling Guidelines*; NCHS, *NHANES
Tutorials — Variance Estimation Module*; ONS *Methodology Working Paper 9*; IPUMS *User Note:
Issues Concerning the Calculation of Standard Errors*; NLS *Standard Errors & Design Effects*;
UN *Household Sample Surveys in Developing and Transition Countries*, ch. 6 and 21; Stata
*svyset* manual; SAS/STAT *Survey Design Specification*; CRAN `PracTools` *Design Effects and
Effective Sample Size* vignette; `survey` package documentation.

---

## Appendix A — the scripts

### A.1 `real_designs.R`

```r
suppressPackageStartupMessages(library(survey))
options(survey.lonely.psu = "adjust")
data(api); data(nhanes)

row <- function(lab, full, flat, f) {
  a <- svymean(f, full, na.rm = TRUE); b <- svymean(f, flat, na.rm = TRUE)
  data.frame(file = lab, quantity = deparse(f),
             est = round(coef(a)[[1]], 4),
             se_full = signif(SE(a)[[1]], 4), se_flat = signif(SE(b)[[1]], 4),
             ratio = round(SE(b)[[1]] / SE(a)[[1]], 3),
             degf_full = degf(full), degf_flat = degf(flat))
}
full  <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
flat  <- svydesign(id = ~1,    weights = ~pw, data = apiclus1)
full2 <- svydesign(id = ~dnum + snum, fpc = ~fpc1 + fpc2, data = apiclus2)
flat2 <- svydesign(id = ~1, weights = ~pw, data = apiclus2)
full3 <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
flat3 <- svydesign(id = ~1, weights = ~pw, data = apistrat)
fullN <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
                   nest = TRUE, data = nhanes)
flatN <- svydesign(id = ~1, weights = ~WTMEC2YR, data = nhanes)

print(rbind(
  row("apiclus1 (cluster)",    full,  flat,  ~api00),
  row("apiclus1 (cluster)",    full,  flat,  ~I(sch.wide == "Yes")),
  row("apiclus1 (cluster)",    full,  flat,  ~meals),
  row("apiclus2 (2-stage)",    full2, flat2, ~api00),
  row("apiclus2 (2-stage)",    full2, flat2, ~meals),
  row("apistrat (stratified)", full3, flat3, ~api00),
  row("apistrat (stratified)", full3, flat3, ~meals),
  row("apistrat (stratified)", full3, flat3, ~I(sch.wide == "Yes")),
  row("apistrat (no fpc)",
      svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat), flat3, ~api00),
  row("NHANES", fullN, flatN, ~HI_CHOL),
  row("NHANES", fullN, flatN, ~I(race == 1)),
  row("NHANES", fullN, flatN, ~agecat)
), row.names = FALSE)

reg <- function(lab, full, flat, f, fam = gaussian()) {
  a <- svyglm(f, full, family = fam); b <- svyglm(f, flat, family = fam)
  co <- names(coef(a))[-1]
  data.frame(file = lab, term = co, se_full = signif(SE(a)[co], 4),
             se_flat = signif(SE(b)[co], 4), ratio = round(SE(b)[co] / SE(a)[co], 3))
}
print(rbind(
  reg("apiclus1", full,  flat,  api00 ~ meals + ell),
  reg("apistrat", full3, flat3, api00 ~ meals + ell),
  reg("NHANES",   fullN, flatN, HI_CHOL ~ factor(agecat) + factor(RIAGENDR), quasibinomial())
), row.names = FALSE)

neff_tab <- function(lab, full, flat, f) {
  w <- weights(flat, "sampling"); n <- length(w)
  p  <- coef(svymean(f, flat, na.rm = TRUE))[[1]]
  vf <- SE(svymean(f, flat, na.rm = TRUE))[[1]]^2
  vF <- SE(svymean(f, full, na.rm = TRUE))[[1]]^2
  data.frame(file = lab, quantity = deparse(f), n = n,
             kish = round(sum(w)^2 / sum(w^2), 1),
             exact_flat = round(p * (1 - p) / vf, 1),
             exact_full = round(p * (1 - p) / vF, 1))
}
print(rbind(
  neff_tab("apiclus1", full,  flat,  ~I(sch.wide == "Yes")),
  neff_tab("apistrat", full3, flat3, ~I(sch.wide == "Yes")),
  neff_tab("NHANES",   fullN, flatN, ~HI_CHOL),
  neff_tab("NHANES",   fullN, flatN, ~I(race == 1))
), row.names = FALSE)
```

### A.2 `tabxplor_check.R` (parity + half-widths + `n_eff > n`)

```r
suppressPackageStartupMessages({library(survey); library(dplyr)})
options(survey.lonely.psu = "adjust")
suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))
data(api); data(nhanes)

## 1. parity: tabxplor "weights" basis == survey ids = ~1
d <- apistrat |> mutate(swide = factor(sch.wide), st = factor(stype))
options(tabxplor.design_effect = TRUE)
tw <- tab(d, st, swide, wt = pw, pct = "row", ci = "cell")
options(tabxplor.design_effect = FALSE)
tn <- tab(d, st, swide, wt = pw, pct = "row", ci = "cell")
sv <- svyby(~swide, ~st, svydesign(ids = ~1, weights = ~pw, data = d), svymean)
k  <- seq_len(nrow(sv))
print(data.frame(
  stype = as.character(tw$st)[k],
  pct_tab = round(get_pct(tw$Yes)[k], 6), pct_svy = round(sv$swideYes, 6),
  neff_tab = round(get_n_eff(tw$Yes)[k], 2),
  neff_svy = round(sv$swideYes * (1 - sv$swideYes) / SE(sv)[, 2]^2, 2),
  n_raw = get_n(tn$Yes)[k]) |>
  transform(ratio = round(neff_tab / neff_svy, 8)), row.names = FALSE)

## 2. half-width ratio: variance AND degrees of freedom
hw <- function(lab, full, flat, f) {
  a <- svymean(f, full, na.rm = TRUE); b <- svymean(f, flat, na.rm = TRUE)
  data.frame(file = lab, quantity = deparse(f),
             se_ratio = round(SE(b)[[1]] / SE(a)[[1]], 3),
             crit_full = round(qt(.975, degf(full)), 3),
             crit_flat = round(qt(.975, degf(flat)), 3),
             halfwidth_ratio = round(qt(.975, degf(flat)) * SE(b)[[1]] /
                                    (qt(.975, degf(full)) * SE(a)[[1]]), 3))
}
# (designs as in A.1)

## 3. does n_eff exceed the raw n?
options(tabxplor.design_effect = TRUE)
tn2 <- tab(nhanes |> mutate(rc = factor(race), ag = factor(agecat)),
           ag, rc, wt = WTMEC2YR, pct = "row")
options(tabxplor.design_effect = FALSE)
cells <- do.call(rbind, lapply(names(tn2)[sapply(tn2, is_fmt)], function(nm) {
  x <- tn2[[nm]]
  data.frame(col = nm, n = get_n(x), tot_n = get_tot_n(x), n_eff = round(get_n_eff(x), 1))
}))
cat("cells with n_eff > base n:", sum(cells$n_eff > cells$tot_n, na.rm = TRUE),
    "/", sum(!is.na(cells$n_eff)), "\n")
```

### A.3 `diffs.R` (levels vs differences)

```r
suppressPackageStartupMessages(library(survey)); options(survey.lonely.psu = "adjust")
data(api); data(nhanes)
nh <- subset(nhanes, !is.na(HI_CHOL))
fN <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, nest = TRUE, data = nh)
zN <- svydesign(id = ~1, weights = ~WTMEC2YR, data = nh)
f1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
z1 <- svydesign(id = ~1,    weights = ~pw, data = apiclus1)

cmp <- function(lab, out, by, full, flat) {
  fo <- as.formula(paste0("~", out)); fb <- as.formula(paste0("~", by))
  a  <- svyby(fo, fb, full, svymean, na.rm = TRUE)
  ga <- svyby(fo, fb, full, svymean, na.rm = TRUE, covmat = TRUE)
  gb <- svyby(fo, fb, flat, svymean, na.rm = TRUE, covmat = TRUE)
  ka <- which.max(coef(a)); kb <- which.min(coef(a))
  L <- rep(0, length(coef(ga))); L[ka] <- 1; L[kb] <- -1
  data.frame(file = lab, outcome = out, by = by,
    lvl_ratio = round(SE(svyby(fo, fb, flat, svymean, na.rm = TRUE))[ka] / SE(a)[ka], 3),
    dif_ratio = round(sqrt(drop(L %*% vcov(gb) %*% L)) /
                      sqrt(drop(L %*% vcov(ga) %*% L)), 3))
}
print(rbind(
  cmp("NHANES", "HI_CHOL", "RIAGENDR", fN, zN),
  cmp("NHANES", "HI_CHOL", "race",     fN, zN),
  cmp("NHANES", "HI_CHOL", "agecat",   fN, zN),
  cmp("apiclus1", "api00", "stype",    f1, z1)
), row.names = FALSE)
```

### A.4 `sim2.R` (Monte-Carlo, 400 replicates)

```r
suppressPackageStartupMessages({library(survey); library(data.table)})
set.seed(20260812); options(survey.lonely.psu = "adjust")

NPSU_PER_STRATUM <- 500L; NSTRAT <- 6L; PSU_SIZE <- 100L
NPSU <- NSTRAT * NPSU_PER_STRATUM; N <- NPSU * PSU_SIZE
pop <- data.table(id = seq_len(N),
  stratum = rep(seq_len(NSTRAT), each = NPSU_PER_STRATUM * PSU_SIZE),
  psu     = rep(seq_len(NPSU),   each = PSU_SIZE))
pop[, hhsize := sample(1:5, .N, TRUE, prob = c(.35,.32,.16,.11,.06))]
pop[, age4 := sample(1:4, .N, TRUE)]; pop[, sex := sample(1:2, .N, TRUE)]
u_hi <- rnorm(NPSU, 0, 1.10); u_lo <- rnorm(NPSU, 0, 0.22)
strat_eff <- rnorm(NSTRAT, 0, 0.45)
pop[, `:=`(
  y_hi  = rbinom(.N, 1, plogis(-0.3 + u_hi[psu] + strat_eff[stratum])),
  y_lo  = rbinom(.N, 1, plogis(-0.2 + u_lo[psu])),
  y_cal = rbinom(.N, 1, plogis(-1.4 + 0.75*age4 + 0.5*u_lo[psu])),
  y_hh  = rbinom(.N, 1, plogis(-1.0 + 0.45*hhsize + 0.5*u_lo[psu])))]
pop[, x := rbinom(.N, 1, plogis(-0.5 + 0.3*(age4-2.5) + 0.4*u_lo[psu]))]
pop[, y_reg := rbinom(.N, 1, plogis(-0.8 + 0.55*x + u_lo[psu] + 0.2*(age4-2.5)))]
M_PSU <- 40L; K_IND <- 10L; NSAMP_B <- NSTRAT * M_PSU * K_IND

draw_A <- function() {                          # stratified two-stage cluster
  pf  <- unique(pop[, .(psu, stratum)])
  sel <- pf[, .(psu = sample(psu, M_PSU)), by = stratum]
  s   <- pop[psu %in% sel$psu]
  s[, denom := sum(1/hhsize), by = psu]         # NB: over the WHOLE psu
  s <- s[, .SD[sample(.N, K_IND, prob = 1/hhsize)], by = psu]
  s[, w_design := 1 / ((M_PSU/NPSU_PER_STRATUM) * (K_IND*(1/hhsize)/denom))]
  s
}
draw_B <- function() {                          # stratified one-stage, same n
  n_per <- NSAMP_B / NSTRAT
  s <- pop[, { d <- sum(1/hhsize)
               .SD[sample(.N, n_per, prob = 1/hhsize)][, denom := d] }, by = stratum]
  s[, w_design := 1 / (n_per*(1/hhsize)/denom)]
  s
}
finish <- function(s, clustered) {              # nonresponse + calibration
  s[, resp := rbinom(.N, 1, plogis(-0.4 + 0.35*age4))]; s <- s[resp == 1]
  s[, w_nr := w_design / mean(plogis(-0.4 + 0.35*age4)), by = age4]
  des <- if (clustered)
    svydesign(ids = ~psu, strata = ~stratum, weights = ~w_nr, data = s, nest = TRUE)
  else svydesign(ids = ~1, strata = ~stratum, weights = ~w_nr, data = s)
  t1 <- c(`(Intercept)` = N,
          setNames(as.numeric(xtabs(~factor(age4), pop))[-1], paste0("factor(age4)", 2:4)),
          setNames(as.numeric(xtabs(~factor(sex),  pop))[-1], "factor(sex)2"))
  cd <- try(calibrate(des, ~factor(age4) + factor(sex), population = t1,
                      calfun = "linear", bounds = c(0.05, 20)), silent = TRUE)
  if (inherits(cd, "try-error")) return(NULL)
  s[, w_cal := weights(cd, "sampling")]
  list(s = s, full = cd, flat = svydesign(ids = ~1, weights = ~w_cal, data = s))
}
# per replicate: svymean under both designs for y_hi/y_lo/y_cal/y_hh; the
# difference of two proportions across x (covmat = TRUE); svyglm(y_reg ~ x).
# Compare mean(SE) with sd(estimate) over 400 replicates, and 95 % coverage.
```

---

## Appendix B — consistency check against the shipped documentation

A full audit of every user-facing sentence about weights (`man/*.Rd`, the six vignettes, `NEWS.md`,
`po/R-fr.po`, the jamovi YAML) was run against §2's statistics. **The headline is reassuring: the
documentation is already scrupulous about the central limitation.** "Blind to clustering and to
calibration" (or its French twin *"aveugle aux grappes et au calage"*) appears in seven independent
places — `?tab`, `?tabxplor-options`, `?tab_reg`, and both intro and both reg vignettes — and the
runtime footer is precisely scoped: it says *"account for the weighting"* at the `"weights"` basis
and reserves *"account for the sample design"* for the basis where it is earned.

`vignettes/tabxplor.Rmd:169` is worth quoting, because it independently reaches this study's §4.4
conclusion:

> *"Many released research files ship one calibrated weight and nothing else — the design variables
> are withheld for confidentiality — and on such a file a design carries only the unequal weighting,
> i.e. exactly what the option already gives you."*

Six discrepancies between the prose and what §1–§3 measured. None is a computation error; four are
statements that z16 made obsolete.

**B.1 — `?tab` claims something the exact form disproves.** `man/tab.Rd:194-195` (roxygen
`R/tab.R:253-254`):

> *"a design can make an interval **narrower**, which weights alone never can"*

That was true of Kish (`deff = 1 + CV² ≥ 1`) and is **false of the exact flat form**, which is
precisely the improvement z16 delivered (§3.1). Verified inside the package, `apistrat`, `wt = pw`,
`design_effect = TRUE`:

```
    lev tot_n n_eff  narrower
      E   100  99.5     FALSE
      H    50  49.8     FALSE
      M    50  49.8     FALSE
  Total   200 209.6      TRUE
```

and on a weighted NHANES age × race crosstab, **11 of 25 cells**. The clause should go, or invert:
weights alone *can* narrow an interval — that is the point of using the exact variance instead of
Kish's bound. (`NEWS.md:45-46` says only "a design can *also* make an interval narrower", which is
fine.)

**B.2 — the constant-weight identity is stated with the wrong `n`.** `man/tab.Rd:495-496` and
`NEWS.md:164`:

> *"a table weighted by a constant gets `n_eff = n * (n-1)/n`, not `n`"*

Measured, `wt = 1`:

```
    lev   n  n_eff  n*(n-1)/n
      E 100  99.50         99
      H  50  49.75         49
      M  50  49.75         49
  Total 200 199.00        199
```

The identity holds only where the cell's base equals the whole leaf. `survey`'s `n/(n−1)` factor
uses the **leaf's row count** `N`, not the cell's base, so the general form is
`n_eff = n_base · (N−1)/N` — here `100 × 199/200 = 99.5`, not 99. The claim is right in spirit
(a constant weight does not give back exactly `n`) and wrong in the formula.

**B.3 — "too narrow" is the typical direction, not the universal one.** `vignettes/tabxplor.Rmd:141`
and `:398` (and the French mirrors) say the default raw-*n* interval "runs a little too narrow"
under unequal weights. By B.1 that is false for any cell where `n_eff > tot_n` — where the raw-n
default is too *wide*. Minor, but the vignette currently teaches a one-directional rule for a
two-directional quantity.

**B.4 — three documented positions, four runtime bases.** `design_partial` has a footer sentence
(`R/fmt_class.R:4843`), a French translation (`po/R-fr.po:360`) and an internal definition
(`R/survey-design.R:24`), but appears in **no** user-facing prose: `?tab`, `?tabxplor-options` and
both intro vignettes all say "**three positions**". A user whose design silently degrades gets a
footer sentence no document explains. (Note also that the `"n"` sentence is the `switch` *default*
branch, so any unrecognised basis string would also print "use the unweighted sample size".)

**B.5 — `method_cell = "beta"` is not quite the method it names.** `man/tab_ci.Rd:67-70` describes it
as *"Korn-Graubard: `survey::svyciprop(method = "beta")`'s Clopper-Pearson interval on the effective
sample size"*. Per §7.3, `svyciprop` additionally shrinks `n.eff` by
`(qt(α/2, n−1)/qt(α/2, degf))²` before the beta quantiles, and `ci_beta()` does not. Identical at the
`"weights"` basis; ~15 % apart at the `"design"` basis on NHANES.

**B.6 — three stale artefacts.** `man/tab_reg.Rd:41-44` lists `svrepdesign()` as an accepted `data`
value in the same paragraph that says it is unsupported (`:226` says "refused at the boundary").
And the retired Kish option still ships its label in three generated/translation files:
`R/jmvtab.h.R:620-622` → `man/jmvtab.Rd:117-119`, plus `jamovi/i18n/catalog.pot:494` and
`jamovi/i18n/fr.po:496` (`"Kish n_eff <i>(weighted)</i>"`). The `.h.R` one clears itself at the next
`jmvtools::prepare()` (already an open maintainer step); the two `.po`/`.pot` entries will not.

**B.7 — the gap, rather than an error.** No document quantifies the clustering risk, and none
carries §6's crossed-vs-nested rule. The docs say *what* the option is blind to, correctly and
repeatedly; they do not say *how much that costs* (a factor of 3–10 on a face-to-face survey for a
marginal percentage) nor *why it usually matters much less for tabxplor specifically* (the design
effect largely cancels in the cell-vs-reference differences the colours and stars actually test).
Those two sentences are, on this study's evidence, the highest-value addition available to the
Weights section.

---

## 11. Implementation record — Last Phase z16-iiiiii (2026-08-12)

Every §8.2 item with a maintainer ruling landed. Status of the study: **CLOSED**.

### The one code change (item 8)

`ci_beta()` (`R/tab-agg.R`) gained `df` and `n_raw`, and applies survey's own rescale
`n_eff * (qt(a, n_raw - 1) / qt(a, degf))^2` before the beta quantiles. Both numbers were already in
scope at the single call site (the `"beta"` arm of `tab_ci()`): `degf` is the local resolved off the
columns, `n_raw` is `get_tot_n(col)`, the cell's own unweighted base — so **no new field, no new
attribute, and no per-domain quantity** had to be computed.

**The guard is what keeps it byte-identical.** The rescale converts an interval referred to `n-1`
into one referred to the design's df; where there is no design, `degf` is `Inf` (the framework's
"refer to z"), so the factor is forced to 1 — which is also exactly what `survey` gives at
`ids = ~1`, where `degf == n-1`. It therefore fires only under a real `svydesign` **and** an opt-in
`ci_method = c(cell = "beta")`.

Measured on the new fixture (800 rows, 8 PSUs, 2 strata, `nest = TRUE`, row variable crossed with
the PSUs): factor **0.645**, `n_eff` 350 → 226, interval `[0.2866; 0.3881]` → `[0.2453; 0.3445]`
against `survey`'s `[0.2453; 0.3445]` — i.e. it had been **25 % too short**, larger than §7.3's
NHANES estimate of ~15 %. Parity is now exact to 1e-8.

Two things the earlier `R/tab-agg.R` comment (written from D5) got wrong and that are corrected in
the source: the rescale is **not** `degf/(degf+1)`, and making it exact needs **no** `n_psu` stored
beside `n_eff`. A third: it claimed `n_eff <= n`, which is B.1's error.

⚠ **The one residual approximation, stated not fixed.** tabxplor refers every interval to the
**whole** design's df (`svy_degf()`, captured once at the boundary), while `svyciprop` on a domain
uses that domain's. Verified equal whenever the row variable is crossed with the PSUs (the ordinary
case), and smaller when a domain drops whole PSUs (measured 6 vs 3). A per-domain df would be a new
quantity computed for one opt-in method, against the subsystem's one-rule principle.

New fixture: `tests/testthat/test-survey-variance.R`, "ci_method = c(cell = 'beta') IS
svyciprop(method = 'beta') under a REAL design" — it asserts parity, asserts the interval is wider
than the un-rescaled one (so it fails without the change), and asserts the no-op at the weights
basis. `test-flat-design-parity.R` #13 passes **unchanged**, which is the no-op proof.

### The stale statements (items 1–4)

- **B.1** — the "which weights alone never can" clause is gone from `?tab`; level 2 now states that
  being exact rather than a bound, it can make an interval narrower as well as wider.
- **B.2** — the `n_eff = n * (n-1)/n` identity is replaced in `?tab` and `NEWS.md` by the correct
  statement in words (an effective n a whisker below the raw one, `survey`'s `(N-1)/N` factor with
  `N` the table's respondent count).
- **B.4** — `design_partial` is documented in `?tab` (both the `@param test` ladder and the details),
  in `?tabxplor-options`, and as a bullet in the vignettes' fine print.
- **B.6** — `?tab_reg` no longer lists `svrepdesign()` as an accepted `data` value in the paragraph
  that refuses it; the two reg vignettes no longer advertise replicate weights as a `svydesign()`
  use case.
- **B.4b, B.6c, B.6d were already fixed** by the parallel z16-iiiii Pass 2 (`d11d45f`): the French
  `design_partial` msgid matches its source again, the dead `test_robust` / `"Kish n_eff"` entries
  are out of both jamovi catalogues and of `po/R-fr.po`, and the `design_effect` UI label is English
  in the English YAML with its French in `fr.po`. `R/jmvtab.h.R` still generates the retired
  `test_robust`; it is generated, and clears at the maintainer's next `jmvtools::prepare()`.

### The Weights section (items 5, 6, 10)

Rewritten in `vignettes/tabxplor.Rmd` and `vignettes/articles/tabxplor-fr.Rmd`, mirrored, as a
**fuller teaching section** (~40 → ~110 lines) for a reader who has never met a survey design.
Vocabulary settled with the maintainer: **"the three weighting levels" / "les trois niveaux de
pondération"**; `?tab` keeps *inference basis* as the technical name of the column attribute.

It carries, in order: one subsection per level, with a **runnable worked example** on `gss_simple`
plus a clearly-labelled synthetic weight (same percentages, wider brackets, the two footers, and the
16 292 White respondents worth ~11 800); the ladder table gaining a `level` column; a "what level 2
can and cannot see" table with the **asymmetry** (strata/calibration would narrow a few percent,
clusters can widen a lot); the sizes (three times too precise on a clustered school survey, nine
times on NHANES race); which surveys are clustered (*Enquête Emploi* ≈ 20 neighbouring dwellings; ESS
ships its clusters in a separate file); and §6's **crossed-vs-nested rule** as the question a reader
can actually answer — *does your row variable vary within a neighbourhood, or does it define one?* —
with the 13 % / 8 % measurements that make it the mitigation that matters here.

A `### The fine print` subsection closes it with item 6 (`n_eff` can exceed `n`, and why that is
correct), item 10 (the degrees-of-freedom gap, ~8 %, and why level 2 structurally cannot have it),
B.4, the cell-exact/difference-cautious rule and the cost.

Also: `tabxplor.design_effect` was in **no** options list anywhere — added to both intro vignettes'
Session options; the French *effective sample size* is now one term (`taille d'échantillon
effective`), recorded with the other survey terms in `dev/french_glossary.md`.

### The `tab()` / `tab_reg()` asymmetry (item 9)

The audit found the opposite of a gap: it was stated four times, in three different rationales, and
never where the reader first meets `empirical = TRUE`. So: **one sentence early** in both reg
vignettes, the two near-identical late paragraphs **merged into one**, and a single rationale kept
everywhere (`tab_reg()` has no choice — its observed columns must match a model column that is
design-based by construction; `tab()` does, and keeps the descriptive convention). The reg
vignettes' "only **two** ways" now reads "two ways to hand tabxplor your weights", so it no longer
collides with the intro's three levels.

### Declined, and staying declined

- **Item 7** (a non-probability-weights sentence) and **item 11** (a `deffc` escape hatch) — ruled
  out by the maintainer. Item 11 would let a user assert precision the file cannot verify.
- **Item 9's architectural half**: the `tab()` default stays `FALSE`. The Monte-Carlo puts it last on
  coverage in all twelve cells tested, and the vignette now says so plainly — but changing it moves
  every weighted table's stars, which is a release-scale decision.
- **`method_cell = "beta"` is still not taught in the vignettes.** It belongs to the CI-composition
  table, and pulling a design-basis-only method into a beginner section buys nothing; `?tab_ci`
  documents it.
- One caveat noticed and left alone: at the weights basis tabxplor refers intervals to **z**, where
  `survey` at `ids = ~1` would use `t(n-1)` — worth 0.1 % at n = 1000, 2.5 % at n = 50. Changing it
  would move every weighted interval with the option on, for no reachable gain.
