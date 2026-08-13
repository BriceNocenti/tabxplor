# The weights framework, stress-tested (round 2, post-z16) — three positions, one field, six leaks

**Date** 2026-08-12 · **Against** `dev` at `9588a12` (Phase 18z16 complete) · **Oracle**
`survey 4.5` · **Reproducer** `dev/weights_stress_test.R`, which produces every number below
(`OMP_NUM_THREADS=1 Rscript dev/weights_stress_test.R`, ~1 min)

**Relation to round 1.** `dev/weights_framework_stress_test.md` (2026-08-11, at `2cdfc60`) audited
the framework *before* the redesign and produced findings W1–W13; `dev/weights_framework_redesign.md`
is the design that answered them, implemented as Phase 18z16. **This document is round 2: the
same audit re-run against the result.** Twelve of round 1's thirteen findings are closed on the
paths it measured — verified here, not assumed. The thirteenth, **W11** (jamovi), is not testable
from R: `jmvtab.a.yaml` now carries the honest `design_effect` checkbox but the generated
`R/jmvtab.h.R` still declares the retired `test_robust` enum (including `"kish"`), so until the
maintainer runs `jmvtools::prepare()` the live module reads the option as absent and stays off —
the state the roadmap already records as open. The six new findings are labelled W‑A…W‑G to avoid
collision, and three of them are *residues* of closed ones surfacing on a path round 1 did not
reach (W‑A ← W4/W5 on the multi-`row_var` merge, W‑B ← W3 with the cause moved, W‑F = W6, left open
by ruling). Round 1 is superseded in vocabulary only — it says "rung" where the shipped code now
says *basis* — and should be kept for its pre-z16 measurements.

This is an independent audit, not a re-reading of the design. The question asked was: are the three
user-facing positions —

1. **weighted estimate, unweighted inference** (`wt =`, the default),
2. **weights as a design** (`wt =` + `options(tabxplor.design_effect = TRUE)`),
3. **full survey design** (a `survey::svydesign` as `data`)

— actually followed *everywhere meaningful*, in `tab()` **and** `tab_reg()`, with no statistically
unsound corner and no gating complexity that has outlived its reason?

---

## 0. The one-page answer

**The mathematics is sound and, where it is testable against `survey`, exact.** Every parity check
run here reproduces `survey` to ten significant figures — proportions, means, total rows, subtable
domains, and the `svyrecvar` path on a stratified + clustered design — and the crude `Obs_*` bases
inside `tab_reg()` agree with `tab()`'s cell bases *number for number*. z16's central claim (a weight column is the flat design; its
variance has a closed form) holds on the real code paths, not only in the design document.

**What leaks is the edges of that core, and all six leaks are of one kind: a fact that is true of
the numbers fails to reach the thing that reports it.** Ranked:

| #       | Finding                                                                                                                                                                                                                                                      | Kind                  | Severity |
|---------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------|----------|
| **W‑A** | `meta$inference` is **destroyed by the ≥2 `row_var` merge** — the footer then states the *opposite* of what was computed, and the exported step path loses `degf` (measured: intervals 9 % too narrow at 13 PSUs)                                            | honesty **+ numbers** | **high** |
| **W‑B** | `color = "contrib"` significance **silently ignores strata/clusters/calibration** — always the flat `B²/S`. Measured: the residual `z` overstated **×2.52**, two cells reading `p = 3.7e‑04` and `2.7e‑06` whose design‑honest values are `0.18` and `0.080` | statistical           | **high** |
| **W‑C** | The design‑degrade flag **leaks across calls into `tab_reg()`** (no reset), so a later reg table claims `design_partial` when nothing degraded                                                                                                               | correctness           | medium   |
| **W‑D** | The crude `Obs_*` columns compute the effective base, use it, then **throw it away** — `n_eff` is `NA` on every reg column, contradicting `?fmt`                                                                                                             | consistency           | medium   |
| **W‑E** | The same crude % difference is **Newcombe in `tab()`, Wald in `tab_reg()`** (and Newcombe again in `tab_reg()`'s own multinomial tooltip)                                                                                                                    | consistency           | low      |
| **W‑F** | `tab()` defaults to position 1 while `tab_reg()` is always ≥ 2 — z16 made this *legible*, so the same session now prints **two footers that contradict each other**                                                                                          | policy                | decision |

Plus one CRAN-facing hygiene defect (**W‑G.1**: `inference_basis` is an undeclared global) and a
short list of gating that can now collapse (**W‑G.2–6**).

**Nothing here requires new statistics.** W‑A, W‑C, W‑D and W‑G are plumbing; W‑B needs a decision
about *which* effective n a contribution residual means, and the honest quantity is already
computed and stored one attribute away (`test$deff`). W‑F is a maintainer ruling, and z16 changed
the argument that produced the current default.

---

## 1. Method

One fixture family throughout, deliberately built so the weights carry information about the
outcome (the assumption Kish makes and unequal weights break):

```r
n <- 4000
grp ~ Multinomial(.4,.3,.2,.1)
w   ~ lognormal(0, .55) × c(A=.6, B=1, C=1.6, D=2.4)[grp]      # weight correlated with grp
col ~ Bernoulli(plogis(-0.3 + 0.5·scale(log w) + effect(grp))) # outcome correlated with w
x   ~ N(50,12) + 6·log(w)
```

and, for the design positions, three real `svydesign`s over it: flat (`ids = ~1`), clustered
(`ids = ~psu`), clustered + stratified (`ids = ~psu, strata = ~str, nest = TRUE`). Oracles are
`svyby(svymean)`, `svymean(~interaction(...))`, `subset(design, …)`, `svychisq` and `svyglm` —
never a hard-coded number.

Every probe was run through `devtools::load_all()` on the working tree, so what is reported is the
behaviour of the shipped code, not of a reconstruction.

---

## 2. The coverage matrix — what actually honours what

`✓` = verified to change with the position and to match the oracle where one exists.
`—` = correctly inapplicable. `✗` = a leak (see §3).

### 2.1 `tab()`

| Quantity                                    | pos. 1 `wt`                   | pos. 2 `+ option`       | pos. 3 design                     |
|---------------------------------------------|-------------------------------|-------------------------|-----------------------------------|
| cell % CI (`pct = row/col/all/all_tabs`)    | raw `tot_n` ✓                 | ✓ exact                 | ✓ exact                           |
| cell % CI on a **Total row / total table**  | ✓                             | ✓ exact                 | ✓ exact                           |
| cell % CI inside a **`tab_vars` subtable**  | ✓                             | ✓ exact                 | ✓ exact                           |
| **Total column**                            | ✓                             | ✓                       | ✓                                 |
| counts table (`pct = "no"`)                 | —                             | ✓                       | ✓                                 |
| difference CI (`ci = "diff"`)               | ✓                             | ✓                       | ✓                                 |
| mean CI, mean-difference CI (`tab_num`)     | ✓                             | ✓ exact                 | ✓ exact                           |
| `color = "OR"` interval + stars             | ✓                             | ✓                       | ✓                                 |
| omnibus χ² / F                              | weighted, rescaled to raw n ✓ | `svychisq` ✓            | `svychisq` ✓                      |
| effect size (V / φ / η²)                    | weighted ✓                    | weighted ✓              | weighted ✓                        |
| `test$n` / `test$deff`                      | raw n / `NA` ✓                | raw n / δ̄ ✓            | raw n / δ̄ ✓                      |
| **`color = "contrib"` residual**            | raw n ✓                       | flat `B²/S` ✓           | flat `B²/S` **✗ W‑B**             |
| stored `meta$inference` (1 `row_var`)       | `"n"` ✓                       | `"weights"` ✓           | `"design"` ✓                      |
| **stored `meta$inference` (≥2 `row_var`s)** | absent ✓                      | **`NULL` ✗ W‑A**        | **`NULL` ✗ W‑A**                  |
| footer sentence (1 `row_var`)               | ✓                             | ✓                       | ✓                                 |
| **footer sentence (≥2 `row_var`s)**         | ✓                             | **inverted ✗ W‑A**      | rescued by a name-sniff **✗ W‑A** |
| `degf` → `t` critical value                 | —                             | —                       | ✓ (lost on merge, **W‑A**)        |
| `tab_counts()` (pre-aggregated)             | ✓                             | states `"n"`, says so ✓ | refused ✓                         |

### 2.2 `tab_reg()`

Position 2 is *forced* here (ruling 1): a weighted fit goes through `svyglm`, so its crude
companion must too. `options(tabxplor.design_effect)` is correctly never read.

| Quantity                                   | unweighted | `wt =`                          | design                 |
|--------------------------------------------|------------|---------------------------------|------------------------|
| model coefficients / CI                    | `glm`      | `svyglm` ✓                      | `svyglm` ✓             |
| AME / `ame_ratio`                          | ✓          | population-average ✓            | ✓                      |
| `Obs_%` / `Obs_OR` (binomial)              | ✓          | ✓ **== `tab()`'s base exactly** | ✓ (clustered ≠ flat ✓) |
| `Obs_mean` / `Obs_diff` (gaussian)         | ✓          | ✓                               | ✓                      |
| `Obs_rate` / `Obs_IRR` (poisson)           | ✓          | ✓                               | ✓                      |
| `Obs_cumOR` (ordinal)                      | ✓          | ✓                               | ✓                      |
| grouped binomial (`trials =`)              | ✓          | ✓                               | ✓                      |
| multinomial in-cell `{obs}`                | ✓          | ✓                               | ✓                      |
| gap test `gap_se` (`color = "adjustment"`) | ✓          | ✓                               | ✓                      |
| frozen SD (`multiplier = "sd"`)            | ✓          | weighted ✓                      | weighted ✓             |
| GOF footer (`n`, Wald, Nagelkerke, AIC)    | ✓          | reduced weighted set ✓          | ✓                      |
| stored `meta$inference`                    | absent ✓   | `"weights"` ✓                   | `"design"` ✓           |
| **`n_eff` field on `Obs_*`**               | —          | **`NA` ✗ W‑D**                  | **`NA` ✗ W‑D**         |
| **degrade flag reset**                     | —          | —                               | **never ✗ W‑C**        |

### 2.3 The parity evidence behind the `✓ exact`

| Check                                            | Oracle                           | Ratio                                                                  |
|--------------------------------------------------|----------------------------------|------------------------------------------------------------------------|
| cell %, `pct = row`                              | `svyby(~y, ~grp, des, svymean)`  | `1.0000000000`                                                         |
| cell %, clustered + stratified                   | same, on `desc`                  | `1.0000000000` (all 4 cells)                                           |
| cell mean                                        | `svyby(~x, ~grp, desf, svymean)` | `1.0000000000`                                                         |
| `reg_empirical()` effective n                    | `tab()`'s own `n_eff`            | identical to 9 s.f. (`1147.77113 / 887.82098 / 606.97590 / 288.18818`) |
| `tab()` diff CI vs hand `ci_prop_diff(newcombe)` | —                                | identical to 9 s.f.                                                    |

The `n/(n−1)` question that a reader will ask — *survey subsets a design for a domain, so shouldn't
the factor use the domain's n?* — is answered by `[.survey.design2`: it subsets `fpc$sampsize`
row-wise, which for `ids = ~1` holds the **original** n in every row, and `onestrat` pads with
zeros to `nPSU`. So the full-sample factor tabxplor uses is survey's own, and the parity above is
not a tolerance artefact.

---

## 3. The findings

### W‑A — `meta$inference` does not survive the ≥2 `row_var` merge (high)

**What.** `tab()` merges several `row_var`s through `tab_compact()`, which rebuilds the table with a
**hand-enumerated** `meta`:

```r
# R/tab_classes.R:1327-1330
tabs <- new_tab(tabs, subtext = subtext, test = tabs_chi2,
                meta = list(render_extras = render_extras_first,
                            ci_settings = ci_settings_first, vars = vars_merged))
```

Any `meta` sub-field not named there is dropped. Today that is `inference`.

**Measured** (English locale, same data, same options):

```
1 row_var, ON  : Weighted by w; confidence intervals and tests account for the weighting.
2 row_vars, ON : Weighted by w; confidence intervals and tests use the unweighted sample size.
```

The second sentence is false: the cells are byte-identical to the one-`row_var` table (CI widths
`0.05365 0.06547 0.07539 0.10141` in both). The footer states the exact opposite of what happened
— which is precisely the failure mode (W4/W6) the phase was built to close, reappearing on the
path most real tables take.

Under a design the sentence is still right, but **only by accident**: `tab_weight_line()` falls
back to `if (identical(wt, svy_wt_col)) "design"` — recognising the internal `.svy_weights` column
*name*. That is the name-sniff z16-i claimed to retire (W5) and Phase 17 rule 2 outlaws; it is
currently load-bearing. It also cannot express `design_partial`, so a ≥2-`row_var` table whose
variance degraded asserts a full design unconditionally.

**And it is not only a label.** `degf` rides the same slot, and `tab_ci()` reads it back for the
exported step path (`R/tab.R:5814`). Simulating the loss on a 13-PSU design:

```
step-path tab_ci widths WITH    stored degf : 0.197417 0.233169 0.184891
step-path tab_ci widths WITHOUT stored degf : 0.179414 0.212124 0.167982
=> anti-conservative by 9.1 %
```

**Fix.** Two parts, both small.

1. `tab_compact()` should start from `get_meta(tabs[[1]])` and *overwrite* the three fields it
   recomputes, instead of constructing a fresh list. That restores the Phase 17b invariant —
   *"adding a `meta` sub-field is one getter and one line, never a constructor formal"* — which
   `tab_compact()` is currently the single violator of. (`color_breaks` escapes today only because
   `finalize_color_tail()` re-stamps it *after* the merge; verified present on both paths. It would
   be the next casualty if that ordering ever changed.)
2. Define the bind rule. `tab_meta_bind()`'s "first non-NULL wins" is wrong for an inference basis:
   if one `row_var` degraded and another did not, the merged table must report the **weaker** of
   the two. Propose `tab_inference_bind()` with the total order
   `n < weights < design_partial < design` taking the **minimum**, and `degf` the minimum of the
   non-`NA`s. Then delete `tab_weight_line()`'s name-sniff fallback, which will have no remaining
   caller.

### W‑B — `color = "contrib"` significance ignores the sample design (high)

**What.** The residual's base is the total column's grand cell (`R/tab.R:6550`), deliberately, so
that a counts table and a percentage table of the same data agree (z16-iii, ruling Q3). But that
cell's proportion is 1, so its variance is 0, so it *always* takes the degenerate fallback
`svy_flat_base_neff(B, S) = B²/S` — the **weights-only** quantity, at **every** basis.

**Measured.** Identical to the last digit between a flat design and a stratified+clustered one:

```
grand-cell n_eff, weights basis : 2326.689782
grand-cell n_eff, DESIGN basis  : 2326.689782
B^2/S (flat, by hand)           : 2326.689782
contrib p identical across the two bases: TRUE
```

— while the **cell** intervals of those same two tables *do* differ, as they should (block `OK 4`:
`0.05365 0.06547 0.07539 0.10141` on the weights basis against `0.05207 0.06115 0.07146 0.10470`
under the stratified + clustered design). So one table reports design-corrected intervals and
weights-only residual significance, side by side, with nothing saying so.

**How much it matters depends on the design, and the bad case is ordinary.** Clustering inflates a
*marginal* proportion much more than it inflates an *association*, so a household-level cluster
with an individual-level `row_var` is nearly harmless — measured on exactly such a fixture, the
cells carry a real deff (`n_eff` 360/315/306) while δ̄ is only 1.21, so the residual is overstated
by ×1.018. But when the `row_var` is a **cluster-level** variable — a geography, a school, an
establishment, i.e. the commonest reason to have clusters at all — the association is clustered
too:

```
n                          : 3600
residual base USED (B^2/S) : 3186.9472
Rao-Scott mean deff        :    7.17013
design-honest base n/deff  :  502.08262
overstatement of |z|       : x 2.5194

contrib p (as shipped)     : 3.731e-04   2.201e-01   2.675e-06
contrib p (design-honest)  : 0.18388     0.64695     0.07958
```

**Two of the three cells cross the threshold**: as shipped they read `3.7e-04` and `2.7e-06`, and
the design-honest values are `0.18` and `0.080`. Under `color_signif = "grey_non_signif"` both are
coloured where both should be greyed, and under `guaranteed_effect` they score on the `zscore`
scale at two and a half times their warranted magnitude. Meanwhile the footer says *"weighted
estimates, intervals **and tests** account for the sample design"*, and `?tab` says *"strata,
clusters, `fpc` **and** calibration reach every interval, star **and colour threshold**"*.

**Fix — the honest number is already computed.** `svy_omnibus_one()` stores Rao-Scott's mean
generalized design effect as `test$deff` on the very same table, and `n / deff` **is** the
first-order effective n of an association — which is exactly what a contribution residual is. The
obstacle is ordering, not statistics: `chi2_write_contrib()` runs inside `tab_chi2()` during
`tab_transform`, while `tab_robust_overlay()` runs a few lines later in the same function. Three
options, in order of preference:

- **(a)** Move the overlay above the contrib write (both are already in `tab_transform`, both have
  `data` in hand) and pass `deff` into `chi2_write_contrib()` as the base divisor when it is
  finite. This makes positions 2 and 3 differ correctly *and* fixes a smaller incoherence that
  exists today even at position 2: the residual's `B²/S` (2326.7) and the omnibus test's implied
  effective n (2269.4) are two different effective sizes for the same table, differing 2.5 %.
- **(b)** Keep the base but stop claiming the design: record `design_partial`-style honesty for the
  residual specifically, and amend the footer/`?tab`. Cheap, but it leaves a wrong colour on screen.
- **(c)** Compute the grand cell's design variance non-degenerately. Rejected: there is no
  non-degenerate quantity there, which is why the fallback exists.

Recommend **(a)**, with **(b)**'s doc correction landing regardless.

### W‑C — the degrade flag leaks across calls into `tab_reg()` (medium)

`svy_degrade_reset()` is called in `tab_transform()` and in both leaf wrappers, so `tab()` is
immune. **`tab_reg()` never calls it**, yet `reg_inference()` → `leaf_inference()` reads
`svy_degrade_get()`.

```
flag after a forced degrade         : "size"
tab_reg basis AFTER the stale flag  : design_partial      <- wrong
tab_reg basis with the flag cleared : design
```

So one degraded table anywhere earlier in the session — a large design table hitting the 5e7
influence-matrix ceiling, a `svyrecvar` failure, an unsupported design — permanently mislabels
every subsequent `tab_reg()`, whose footer then says *"this table's design variance could not be
computed"* about a table where it was. One line at the top of `reg_build()` (or `tab_reg()`) fixes
it; a regression test belongs beside it, because the flag is process-scoped and nothing else
enforces the discipline.

### W‑D — the crude `Obs_*` columns discard the base they used (medium)

`reg_empirical()` computes exactly `tab()`'s effective base (verified identical, §2.3), feeds it to
`ci_wilson` / `ci_prop_diff` / `ci_or`, and then never writes it into the `n_eff` field:

```
reg_empirical emp_n_draw : 1147.77113  887.82098  606.97590  288.18818
tab()          n_eff     : 1147.77113  887.82098  606.97590  288.18818   <- identical
Obs_%  n_eff FIELD       : NA  NA  NA  NA  NA                            <- stored as NA
```

`?fmt` documents `n_eff` as *"the effective sample size used for this cell's confidence
interval"*, which is false on every regression column, and a user auditing a reg table with
`$n_eff` gets `NA` where the correction demonstrably happened. `emp_col()` already knows the value
(`n_draw` / `n_ci` per shape); this is one argument threaded to one `fmt()` call per shape.

For completeness: `Model_*` columns carry no `n` / `tot_n` / `n_eff` either. That is defensible (a
coefficient has no cell base) but should be *stated* in `?fmt`, so the field's contract reads
"populated for descriptive cells" rather than implying universality.

### W‑E — one quantity, two interval methods, across and inside `tab_reg()` (low)

The crude % difference against the reference level:

```
hand ci_prop_diff(newcombe)    : 0.084649437  0.092532249  0.114738594
tab()   ci = "diff" (newcombe) : 0.084649437  0.092532249  0.114738594
tab_reg Obs_%       (wald)     : 0.084798240  0.092749219  0.115187218   (+0.18 % … +0.39 %)
```

This is deliberate — Phase 16d chose Wald so the crude companion matches the model AME's Wald and
the merged legend can name one method — and each table names its own method honestly in
`ci_settings`. It is worth one sentence in `?tab_reg` all the same, because `empirical = TRUE`
exists precisely to be read beside a crosstab, and a reader who checks will find the two disagree.
**Maintainer’s decision: I confirm this difference is wanted.**

Related, and cheaper to fix: `reg_empirical()` computes `emp_diff_inf/sup` via
`ci_prop_diff(method = "newcombe")` on **every** call, and the only consumer is the multinomial
tooltip (`R/tab_reg.R:4146`). So within `tab_reg()` the same difference is Newcombe in a tooltip and
Wald in a column. Either route the tooltip through the shape's declared method (making
`REG_EMPIRICAL` the single source it was designed to be) or, if the divergence is wanted, say so
where the tooltip is built.
**Maintainer’s decision: this difference is not wanted, tab_reg() should be consistent (use the same test as the model where possible/meaningful, and using Wald when not possible since it’s what is used throughout tab_reg() anyway).**

### W‑F — `tab()` defaults to position 1, `tab_reg()` is always ≥ 2 (decision)

Not a defect: it is ruling 1, correctly implemented (`svy_inference_basis(..., force = TRUE)`), and
it is right *within* a reg table — a crude column that did not match the `svyglm` beside it would be
worse. But z16 made both positions *speak*, and the consequence is that one session, one dataset,
one weight now prints two footers that contradict each other:

```
tab()     : Weighted by w; confidence intervals and tests use the unweighted sample size.
tab_reg() : Weighted by w; confidence intervals and tests account for the weighting.
```

with genuinely different widths for the same crude percentage. §8.2 accepted this as W6's residue
under the mitigation "the footer says so" — but the mitigation makes the discrepancy *more*
visible, not less.

**The argument that produced the default has expired.** `tabxplor.design_effect` defaulted to
`FALSE` when it meant *Kish*, an approximation that was measured wrong by up to 17 % in either
direction. It now means *the exact flat-design variance*, reproducing `survey` to the last digit,
computed from sums the aggregate accumulates unconditionally (ruling 8) — so the cost of turning it
on is near zero and the statistical objection is gone. Three options for the maintainer:

- **(b) flip the default to `TRUE` before the CRAN freeze.** A weighted `tab()` would then agree
  with `tab_reg()`, with `survey`, and with what the word "weighted" implies to a survey analyst.
  Position 1 stays reachable (`design_effect = FALSE`) for anyone who wants the descriptive
  convention. Cost: every weighted table's intervals change (goldens: the weighted ones move; the
  unweighted ones cannot). This is the last release at which it is free.
- **(a) keep `FALSE`, document the pairing** explicitly in `?tab_reg` and the Weights vignette
  section — *"a weighted `tab_reg()` is always design-corrected; a weighted `tab()` is not unless
  you ask"* — so the two footers are pre-explained rather than discovered.
- **(c) let `tab_reg()` honour the option.** Rejected here: it would break "the crude column matches
  the model column", which is the whole point of `empirical =`.

Recommend **(b)**, with **(a)**'s documentation shipping either way.
**Maintainer’s decision: (b) not possible, I want to keep the tabxplor 1.3.1 numbers by default. Let’s do (a), concise documentation.**

---

## 4. Statistically sound, verified — recorded so nobody re-audits it

- **The closed form is `survey`, not an approximation of it.** Ratio `1` (to 10 s.f.) for `pct =
  "row"/"col"/"all"`, for a Total row, for a total-table row, for a `tab_vars` subtable domain, for
  a cell mean; and on a *stratified + clustered* design the influence-function path matches
  `svyby(svymean)` to the same precision on all four cells, with `degf` 198 == `survey::degf()`.
- **One option read, one resolver.** `getOption("tabxplor.design_effect")` has exactly one DECISION
  site (`svy_inference_basis()`, `R/survey-design.R:139`); the only other occurrence,
  `jmvtab.b.R:41`, saves it for `on.exit()`. The basis is resolved in
  `tab_setup()` and consumed everywhere else. This is the single biggest structural improvement in
  the subsystem and it holds.
- **`svy_abort_wt_design()` reaches all five entry points** (`tab`, `tab_many`, `tab_plain`,
  `tab_num`, `tab_reg`); `tab_counts()` refuses a design through the same `svy_is_design()`.
- **Ruling Q3 holds**: χ² and Cramér's V are weighted whenever `wt` is given (V 0.3187 weighted vs
  0.3103 unweighted on the base fixture), rescaled to the raw n, so unweighted output is
  byte-identical by construction.
  `test$n` is always the raw count; `deff` carries the effective information at its own grain.
- **The degrade path is honest where it is wired.** The 5e7 influence-matrix ceiling, an unsupported
  design and a `svyrecvar` failure all fall back to the flat closed form *and* record
  `design_partial`, whose sentence says "accounts for the weighting only". That is the right design
  — W‑B is a case that never reaches it, not a case where it fails.
- **`tab_counts()` states what it can carry.** Pre-aggregated counts have no per-observation `Σw²`,
  so `svy_degrade_unserved()` downgrades to `"n"` and the footer says the counts' own n. With
  `wt_counts =` the weight line appears and correctly says "unweighted sample size" — verified: the
  weighted basis is asked for, `svy_degrade_unserved()` fires, the table states `"n"`.
- **Every family's crude column moves with the position** — binomial, gaussian, poisson, grouped
  binomial, ordinal, multinomial (in-cell), AME — widening under weights, and widening further under
  a clustered design than a flat one (`Obs_%` 0.10784 clustered vs 0.09900 flat on a 3000-row check).
- **`n/(n−1)`** is survey's own factor on the domain, not a tabxplor choice (see §2.3).

---

## 5. White elephants and gating that can now collapse

### W‑G.1 — `inference_basis` is an undeclared global (CRAN-facing)

`R/fmt_class.R:51` still lists the **retired** `"inference_mode"` (the z14-ii name) in
`globalVariables()` and does **not** list the live `"inference_basis"`. Confirmed by
`codetools::findGlobals`:

```
tab_transform        undefined globals: data, design_spec, inference_basis, wt
tab_assemble_tables  undefined globals: data, design_spec, inference_basis, wt
```

`data` / `design_spec` / `wt` are declared; `inference_basis` is not. That is an `R CMD check`
*"no visible binding for global variable"* NOTE waiting on the release branch. One-token fix
(swap the stale name for the live one).

### W‑G.2 — five near-synonymous booleans in the leaf

`plain_core()` carries `weighted`, `has_w2`, `use_w2`, `design_on`, `design_flat`, `use_raw`,
`des_rows`; the pair `(use_w2 || design_on)` is written at four call sites, while `leaf_neff()`
internally gates on `has_w2` — *not* `use_w2` — for the fallback, which is correct and reads like a
bug. The relations are all derivable:

```
use_raw  ⇐ design_on            (forced, R/tab.R:3891)
has_w2   ⇔ weighted && use_raw  (the Σw² value.var exists exactly then)
use_w2   ⇔ has_w2 && basis == "weights"
```

so all four `(use_w2 || design_on)` sites are `want_neff <- !identical(inference_basis, "n")`, and
`leaf_neff()` needs only `design_on && !design_flat` to choose its implementation. `num_core()`
duplicates the same set (`R/tab.R:5130-5135`, `5342-5345`). This is the "unreadably complex gating"
the audit was asked to flag: it is not wrong, but it encodes the basis in five places when the basis
is now a single resolved value.

### W‑G.3 — an unreachable guard

`n_obs <- if (use_raw) nrow(data) else NA_real_` (`R/tab.R:4007`): when `use_raw` is `FALSE`,
`has_w2` is `FALSE`, so `n_obs` is never consumed. Either mark it defensive or drop the branch.

### W‑G.4 — the last hand-rolled Kish

`R/reg-assumptions.R:664` still computes `ne <- sw^2 / rowsum(w^2, g)` — Kish — for the assumption /
sparkline bands, ignoring a real design entirely. It is a *teaching* band, so the stakes are low,
but it is the only surviving use of the formula the framework retired, and `svy_flat_neff_rows()` is
the exact drop-in for the weighted case. Either route it through, or state in the header that the
bands are deliberately weights-only and design-blind.

### W‑G.5 — `exists(..., inherits = FALSE)` in `tab_apply_reference()`

Seven sites (`R/tab.R:4790-4800` and the `list(...)` tail), the idiom Phase 17e removed elsewhere in
favour of typed defaults. `tabs_neff` is threaded in as an argument already; the guard could be
`!is.null()`.

### W‑G.6 — stale comments that now describe a framework that no longer exists

All of these still say *Kish*, *opt-in*, *rung*, or the retired resolver name:

| Site                            | Says                                                                      | Should say                                       |
|---------------------------------|---------------------------------------------------------------------------|--------------------------------------------------|
| `R/tab.R:5125`                  | "Kish n_eff applies to the weighted mean CIs"                             | the flat closed form                             |
| `R/tab.R:5961`                  | "the effective n (Kish n_eff) when populated"                             | idem                                             |
| `R/tab.R:6449`                  | "`n_base` is the UNWEIGHTED n (or the Kish n_eff)"                        | idem — and see W‑B                               |
| `R/tab.R:739, 1381, 3739, 5016` | "the test **rung**"                                                       | the inference **basis**                          |
| `R/tab_reg.R:1628-1631`         | "`svy_inference_mode`", "three-rung ladder", "off-kish is byte-identical" | `svy_inference_basis`, the basis, forced         |
| `R/tab_reg.R:2208`              | "the CI base is the effective n (Kish n_eff, opt-in)"                     | forced, exact                                    |
| `R/fmt_class.R:1772-1773`       | "Kish n_eff when opted in"                                                | (the public roxygen above it is already correct) |

None of these change behaviour, but "Kish, opt-in" is now actively misleading: in `tab_reg()` the
correction is neither Kish nor opt-in.

---

## 6. Out of scope, found in passing

`tab_reg(trials = "<column name>")` fails with an opaque
`contrasts can be applied only to factors with 2 or more levels` from deep inside `glm()`.
`?tab_reg` documents `trials` as *an integer or a vector named by dependent* — a column name is not
supported — but a per-row item count is the natural shape for grouped-binomial data, and the error
names neither the argument nor the reason. Either accept a column name or validate the argument at
the boundary. Unrelated to weights; noted so it is not lost.

---

## 7. Suggested order of work

| Step | Content                                                                                 | Risk   | Byte-identity                                                               |
|------|-----------------------------------------------------------------------------------------|--------|-----------------------------------------------------------------------------|
| 1    | **W‑G.1** (globalVariables), **W‑G.6** (comments)                                       | none   | total                                                                       |
| 2    | **W‑C** (one `svy_degrade_reset()` + test)                                              | none   | total off the stale-flag path                                               |
| 3    | **W‑A** (`tab_compact()` carries `meta`; `tab_inference_bind()`; delete the name-sniff) | low    | metadata only for `tab()`; the step path's `degf` changes, which is the fix |
| 4    | **W‑D** (`emp_col()` writes `n_eff`)                                                    | low    | reg tables are not snapshotted; the field is additive                       |
| 5    | **W‑G.2/3/5** (collapse the gating)                                                     | low    | must be total — it is a rewrite of predicates, not of maths                 |
| 6    | **W‑B** (contrib base) — needs the ruling below first                                   | medium | `_color_golden` moves for weighted/design tables only                       |
| 7    | **W‑E** doc sentence; **W‑G.4** decision                                                | none   | total                                                                       |

## 8. Decisions needed from the maintainer

1. **W‑B**: which effective n does a contribution residual mean — the whole table's *weighting*
   deff (`B²/S`, today) or the association's *design* deff (`n/δ̄`, already stored)? Recommend the
   latter via option (a), which also removes the 4 % incoherence that exists at position 2.
   **Maintainer’s decision: (a) ; but the default weighted analysis with no design effect should of course keep the unweighted n for relative Pearson contributions, to match what a correspondence analysis does.**
2. **W‑A**: confirm the merge rule "the weakest basis wins" (`min` over
   `n < weights < design_partial < design`).
   **Maintainer’s decision: ok. Anyway, make this fix robust, for future not to loose metadata anymore.**
3. **W‑F**: flip `tabxplor.design_effect` to `TRUE` for 2.0.0, or keep `FALSE` and document the
   `tab()` / `tab_reg()` pairing? This is the last release where the choice is free.
   **Maintainer’s decision: keep FALSE and document.**
4. **W‑E**: make the multinomial tooltip use the shape's declared method (one source), or keep
   Newcombe there deliberately and say so?
   **Maintainer’s decision: the shape’s declared method.**
5. **W‑G.4**: route the assumption bands through the exact form, or declare them design-blind?
   **Maintainer’s decision: through the exact form when weighted.**

---

## 9. Implemented — Phase 18z16-iiii (2026-08-12)

Every finding above is closed. Suite green (FAIL 0, WARN 0, SKIP 4, PASS 5423); **zero golden or
snapshot churn** — the only fixture added is a new one, `_color_golden/c_contrib_wt_grey.rds`. Two
tests were consciously rewritten around a new, better identity (below). `dev/weights_stress_test.R`
re-run: every `W-*` block reports the fixed behaviour; the `OK 1-7` blocks did not move.

### What each fix turned out to be

**W‑A** — `tab_compact()` was the package's single "rebuild a `meta` from a literal" site, and
`tab_stack_tables()` hands it a plain tibble with no attributes, so that literal really was the only
source. The fix is not a longer literal but **`tab_meta_merge(metas, ...)`**: reduce the inputs' metas
through `tab_meta_bind()`, then overwrite only the three fields the merge recomputes. Every other
sub-field now rides along *by construction*, which is the robustness the ruling asked for. Two
supporting pieces: `tab_meta_bind()`'s hard-coded `color_breaks` branch became a DECLARED
`meta_bind_rules` table (net shorter — the special case moved inside the loop and the second pass
disappeared), and `inference` joined it with `tab_inference_bind()` (min over the declared
`inference_basis_order`, `degf` the min of the non-`NA`s). That also fixes `bind_rows()` of two tabs
with different bases, which took `x`'s claim unconditionally. `tab_weight_line()`'s name-sniff is
deleted; the guard that replaces it is **field-agnostic** (`test-meta-attr.R` stamps a `zz_probe`
sub-field nothing in the package knows about and asserts it survives compact / bind / transpose / a
dplyr verb), so it cannot rot as fields are added.

**W‑B** — the report's option (a) said "move the overlay above the contrib write, both are already in
`tab_transform`". They are not: the overlay runs in `tab_assemble_tables()`, and it must, because it
needs the numeric ANOVA rows bound first. So `tab_robust_overlay()` **split** into
`svy_omnibus_grid()` (the producer, now called in `tab_transform()`) and a thin joiner. Two
corrections to the design as sketched: the producer passes **δ̄, never a resolved `n_base`** — the
base's scale is a property of the table block and `chi2_write_contrib()` already reads it, whereas
`svy_omnibus_one()`'s `n` is the complete-case count, which differs under `na = "keep"`; and
`chi2_write_contrib()` therefore needs no knowledge of the basis at all (the lookup is `NULL` at basis
`"n"`, so the correspondence-analysis reading stands structurally). **It costs no new `svychisq`**:
`color = "contrib"` already forces `chi2 <- TRUE` (`tab-resolve.R:154`), so the overlay was already
running on exactly these tables.

The measured invariant is now exact and testable: **|z| shrinks by exactly `1/√δ̄`**, uniformly across
the cells of one (subtable × col_var). That is what the two rewritten tests assert
(`test-chi2-residuals.R`, `test-survey-variance.R`), and it is a better test than the one it replaces —
an identity rather than a coincidence between two definitions of an effective size. A third test was
added for the case the defect was really about: a flat design and a clustered one must now give
different residuals, in the ratio their two reported `deff`s predict.

**Two further defects found while implementing, both fixed here.**

* **The total-table (`Ensemble`) test row was silently dropped** on any weighted / design table with
  `tab_vars` + `totaltab = "table"`: `chi2_compute_test()` emits it, but the overlay's groups came from
  `unique(frame[tab_vars])`, which has no such level, and the overlay *replaces* the classic tibble.
  The producer now carries a total-table group (all rows, keyed by `totaltab_name`, built as a factor
  with the expanded level set so `vec_rbind` does not coerce the column to character), and the joiner
  `semi_join`s — replace, never invent.
* **W‑H, the same disease as W‑B.** The overlay ran on inputs that cannot *serve* the weighted basis:
  `tab_counts(wt_counts =)` and any pre-aggregated `.fine` carry no per-observation `Σw²`, so the
  leaves call `svy_degrade_unserved()` and the table states basis `"n"` — while its `test` carried a
  `chi2_design` row from `svychisq` run on aggregate rows (one "PSU" per aggregate row). A table whose
  footer says `"n"` must not carry a design-based p, and after W‑B that δ̄ would also have driven its
  colours. The producer is now gated on the same "can this input serve the basis" predicate the leaf
  uses. The ordinary `tab()` / `tab_many()` path always has `fine_fused = NULL`, so nothing moves there.

**W‑C** — one `svy_degrade_reset()` at the top of `tab_reg()` (not in `reg_build()`, which recurses per
split group and would clear a group's own degrade).

**W‑D** — `emp_col()` gained an `n_eff` argument and each arm passes **its own** base (`nv_dr` for a
proportion / odds / risk ratio, `nv_ci` for a mean, a rate and their ratios). It cannot be read off
`shape$type`: a poisson IRR is type `"row"` and takes `nv_ci`. `NA` when nothing corrected it, so an
unweighted reg table matches an unweighted `tab()`.

**W‑E** — `REG_EMPIRICAL[[key]]$method_diff` replaces the hard-coded `"newcombe"`; the fact table is
the single source it was designed to be.

**W‑F** — documented per the ruling, in `?tab_reg` (a new paragraph pairing the two defaults) and
already present in both regression vignettes.

**W‑G** — `.1` the `globalVariables()` swap; `.2` `use_w2` deleted for the pair
`want_neff` ("the basis asks") × `can_neff` / `num_served` ("this input can supply"), spelled the same
way in both leaves; `.3` `n_obs` moved into the `use_raw` block where it is the only place it means
anything; `.5` the six `exists(inherits = FALSE)` guards in `tab_apply_reference()` became
`NULL`-initialised locals; `.6` ten stale *Kish / opt-in / rung / `svy_inference_mode`* comments.

**W‑G.4** — per the ruling, `rd_bin()` now takes the **design variance** when a `svydesign` is passed
to `reg_check_plots()` (or is the `tab_reg()` build's own), the **exact flat closed form**
(`svy_flat_neff_rows`) on plain weights, and is unchanged unweighted (Kish at equal weights *is* n, so
the bands do not move). One rule for all three links: `ne = num / Var(mean of y in the bin)` with `num`
the numerator Korn–Graubard's device wants — `p(1-p)` for a share, the mean for a count, the within-bin
variance for a mean — so the three link arms were not touched. Verified: the design band **equals
`SE(svymean)` on the bin's own domain** to 1e-6.

**§6 `trials`** — validated at the boundary per the ruling (a column name, a non-numeric, a named
vector that misses a dependent, a non-positive count), and `FALSE` became the off switch symmetric
with `TRUE`.

### One thing this phase did NOT do

`?tab` and the vignettes still state the residual's honest residue: the correction is **first-order**
(one δ̄ for the whole table), not per cell. An exact per-cell design residual needs each cell's own
influence function, which the aggregate does not carry and which would take a second channel through
the col_var join — the ad hoc layer this roadmap exists to avoid.
