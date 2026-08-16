# Where a `tab_reg()` call spends its time — and why tabxplor does not parallelise it

**Phase 20f, 2026-08-16.** The measurement the brief asked for, its verdict, and what was done
instead. Written to be the *durable* answer to "why is `tab_reg()` not parallel", so that a later
cycle can re-open the question with the numbers rather than from scratch.

Re-runnable harness: `dev/benchmarks/phase20f_reg_profile.R`.
Results: `dev/benchmarks/results_2.0.0/phase20f_reg_profile{_before,}.txt` and the paired `.csv`.

⚠ Every timing here is **ext4 / WSL2, R 4.6.1, one thread**, min of 3 warm runs. It is not
comparable to the committed Windows baselines (`dev/benchmarks/baseline.csv`, the three
`tests/testthat/*benchmark_baseline.csv`).

---

## 1. The question

Phase 20f's brief: *is a process pool worth anything now that 20d and 20e have landed — and be
willing to answer no.* The case for one was built on a call measured at **15.3 s**, of which 85 %
was `marginaleffects::get_jacobian`. Phase 20d removed that (15.3 s → ~1.2 s), and Phase 20e then
found that `reg_stage_fit()` is **one of four** places a model is fitted, not the single seam
§5.4 of the plan-of-plans assumed. So the brief required a re-measurement before any design.

## 2. What the measurement found

**Post-20d/20e, a default `tab_reg()` call is 81–94 % model-check footer, at every data size.**

| shape (gss_cat 21 483 rows unless stated) | default | `stats = FALSE` | checks |
|---|---|---|---|
| binomial, 4 predictors, n = 2 000 | 0.14 s | 0.06 s | 54 % |
| binomial, 4 predictors | 0.65 s | 0.16 s | 75 % |
| binomial, 6 predictors (4 numeric), n = 200 000 | 12.27 s | 1.29 s | **89 %** |
| multinomial (3 levels), 4 predictors | 5.89 s | 1.10 s | 81 % |
| ordinal (16 levels), 3 predictors | 8.75 s | 1.73 s | 80 % |
| 3-model comparison, n = 200 000 | 13.55 s | 2.17 s | 84 % |
| `tab_vars` (4 groups), n = 200 000 | 4.93 s | 1.17 s | 76 % |

And **a large part of that share was work computed several times and read once** — the same
pathology 20d removed, one level down. Three, each traced rather than inferred:

1. **The Linearity check fitted the model twice per numeric predictor.** The augmented fit is
   irreducible (its likelihood *is* the test). The second was not: `reg_term_tests()` called
   `stats::drop1()`, which refits the reduced model — and the reduced model is `base_fit`, already in
   hand. At n = 200 000 that refit is 1.02 s against 0.028 s for comparing the two fits.
2. **`vcov()` was recomputed four times on one multinomial fit.** Traced: `nnet:::multinomHess` ran
   **7×** per default multinomial table (1× at `stats = FALSE`), at **0.757 s each**. `dispersion`
   +2, `influence` +2 — both on the *identical* object, because `reg_check_model_se()` and
   `reg_coef_if_maker()` each called `vcov()` and each check called both — and `linearity` +2 on the
   augmented refits, whose `tidy` was then discarded.
3. **The Brant proportional-odds test ran three times per ordinal table and was read once.** Traced:
   `brant::brant` **3×** by default, 1× at `stats = FALSE`, ~1.1 s each. `reg_fit_ordinal()` ran it
   on *every* polr fit — the reported one, both Linearity refits, every crude univariable fit — and
   `attr(fit, "brant_po")` had exactly one reader, the `proportionality` footer row.

## 3. The verdict: no process pool

Five reasons, in order of weight.

1. **The work was redundant, not distributable.** A pool would have spread computation that should
   not exist. This is 19o's own finding about 20d, repeating one level down.
2. **`tab_pmap()` dropped worker conditions entirely**, and `dev/verify_reg_specs.R` compares every
   `cli_inform` / `cli_warn` / abort **in order** as part of IDENTICAL. Of the four fitting sites,
   `_fit`, `_split` and `_empirical` are unsuppressed or only message-suppressed, so parallelising
   them was a *correctness* regression, not a speedup. (Only the Linearity loop is fully silent —
   and it is exactly the loop the de-duplication halved.)
3. **jamovi — the interactive path where responsiveness matters most — can never use a pool.**
   `tab_parallel_workers()` returns `0L` whenever `cache_env` is set, and `.fit_cache` is an
   environment that cannot cross a process boundary. At teaching scale (n = 2 000) the checks were
   94 % of the call, so the de-duplication is the *only* lever that reaches that path at all.
4. **The common call was already fast**: a one-model binomial coefficient table is 0.16 s without the
   check footer, and daemon spin-up is the same order as any saving.
5. **The one payload that would have qualified** — the silent Linearity refits — is now ~1 s of a
   3.4 s call at n = 200 000 and ~0.1 s at n = 21 483. A new `parallel` formal, a worker, a threshold
   option and a parity test to win ~1.3× on large data only.

### Also measured and declined

- **A Rao score (LM) test for Linearity**, which needs no augmented fit at all. Already rejected in
  z15 and the reason still holds: it is design-blind — it returns the *identical* p on a weights-only
  and on a stratified+clustered design, where the design-based Wald differs by thirty orders of
  magnitude. Two tests for one row is the disease this package spent Phase 19 curing.
- **`glm(start = c(coef(base), 0))`** for the augmented fit. Measured at n = 200 000: 4 IRLS
  iterations with and without, 1.03 s against 1.04 s. R's default start is already good; there is
  nothing here.
- **`anova(base, aug, test = "F")`** as the F arm's engine. ⚠ It is *not* what `drop1` computes on a
  quasi-likelihood: `anova.glm` gave 14.25 where `drop1` gives 12.47, because `drop1.glm` at its
  default `scale = 0` estimates the dispersion as `deviance/df.residual` of the augmented fit, not as
  the Pearson dispersion `summary()` reports. `reg_nested_test()` implements drop1's own formula, and
  `test-reg-checks.R` pins it with `expect_identical()` so no tidier substitution can slip in.

## 4. What was done instead

Four changes, all in `R/reg-assumptions.R`, `R/reg-influence.R` and `R/tab_reg.R`. No new option, no
new formal, no new fact table, no concurrency.

| # | change | effect |
|---|---|---|
| D1 | `reg_check_linearity_rows()` compares the two fits it holds (`reg_nested_test()`) instead of calling `drop1()`. Bit-identical on both arms. | one model fit per numeric predictor instead of two |
| D2 | `reg_check_influence_pass()` — Dispersion and Influence are one `vcov`, one influence closure and one sweep of `p` contrasts, read two ways | `vcov()` per fit 4 → 1 |
| D3 | the Brant test moves from `reg_fit_ordinal()` to the `proportionality` row that reads it | `brant::brant` 3 → 1, and only when asked |
| D4 | `REG_CHECKS$cost` (`"free"` / `"refit"`); the two fit-based checks leave the default `stats` set, and `stats = "all"` starts meaning *all* | the default footer costs nothing beyond the model |

### The result

`stats = "all"` is the honest comparison — it computes **strictly more** than today's default did:

| shape | before (default) | after (default) | after (`stats = "all"`) |
|---|---|---|---|
| binomial, 4 predictors, n = 2 000 | 0.14 s | **0.08 s** | 0.12 s |
| binomial, 4 predictors | 0.65 s | **0.32 s** | 0.67 s |
| binomial, 6 predictors, n = 200 000 | 12.27 s | **3.44 s** | 7.90 s |
| multinomial (3 levels) | 5.89 s | **1.78 s** | 3.95 s |
| ordinal (16 levels) | 8.75 s | **1.45 s** | 4.13 s |
| 3-model comparison, n = 200 000 | 13.55 s | **4.61 s** | 9.41 s |
| `tab_vars` (4 groups), n = 200 000 | 4.93 s | **1.73 s** | 3.75 s |

3.6× on the default call (2.6–6.0× across shapes), and **1.3–2.1× even when every check is asked
for**, which is the pure de-duplication.

The three counts the harness tracks, before → after: `multinomHess` 7 → 2, `brant` 3 → 0 (1 when the
check is asked for), `reg_term_tests` (the `drop1` refit) 3 → 1 on a binomial table and 2 → 0 on a
multinomial one.

## 5. The four fitting sites, for whoever re-opens this

Fits per call, so a future measurement starts from a map rather than a grep. `S` = specs (outcomes ×
models), `N` = numeric predictors, `G` = `tab_vars` groups, `F₂` = factor predictors with 2+
coefficients.

| site | file | loop | fits | speaks? |
|---|---|---|---|---|
| `reg_stage_fit()` | `R/tab_reg.R` | over `specs` | `S` | **unsuppressed** |
| `reg_stage_footer()` — Linearity | `R/reg-assumptions.R` | numeric predictors × specs | `S·N`, **opt-in since 20f** | fully silent |
| `reg_stage_empirical()` — crude | `R/reg-empirical.R` | predictors × specs | `S·N` (all predictors for ordinal) | messages muffled, **warnings escape** |
| `reg_stage_split()` — recursion | `R/tab_reg.R` | over `tab_vars` levels | `G ×` the whole pipeline | **unsuppressed** |

Plus engine-internal refits that are not `reg_fit()` calls and are easy to miss: `drop1()` inside
`reg_global_rows()` (one reduced model per multi-coefficient term, `F₂` per spec — still in the
default set, because it is a *test* rather than a diagnostic and the only cheaper route, a Wald test,
is a different number); the null `multinom` / `polr` inside `reg_glance()`; and `AIC.svyglm` →
`regTermTest(method = "LRT")` on weighted tables.

**If parallelism is re-opened**, the two prerequisites are now in place and the third is not:
`tab_pmap()` relays worker conditions in unit order (Phase 20f), `reg_build()` is staged (20e) — but
a worker must still return `reg_build_digest()` and never a fit (~10 MB each; ~41.5 MB per jamovi
round-trip was Phase o's measured freeze), and the jamovi path stays serial by construction.

*It was re-opened, one axis out, in Phase 20f-ii. §6.*

---

## 6. Phase 20f-ii — the MODEL axis

§1–§5 asked whether a pool helps **inside one model build**. 20f-ii asks it where a call builds
**several independent models**. Harness: `dev/benchmarks/phase20f2_reg_model_axis.R`; results
`dev/benchmarks/results_2.0.0/phase20f2_*`. Same platform caveat as above.

### 6.1 The three axes are not the same shape

This is the structural finding, and it decides more than the timings do.

| axis | loop | one unit returns | parallel shape |
|---|---|---|---|
| **G** — `tab_vars` groups | `reg_stage_split()`, a recursive `reg_build()` per level | `list(data, test)` — **finished tibbles** | ✅ already `tab_pmap`-shaped. Fit-free products; the one cross-unit step (`reg_write_group_gap()`) is a post-loop barrier matching by KEY, not position; and the message stream is **already unit-major**, so a relay preserves order exactly |
| **R** — several outcomes × a models list | `tab_reg()`'s own recursion | a finished `tabxplor_tab` (its `fit_spec` is ~4 KB of strings) | ✅ no cross-unit dependency at all |
| **S** — several outcomes in ONE table · a models list | `reg_stage_fit()` + six more per-spec loops in `_columns` / `_footer` / `_rows` / `_empirical` / `_tips` | the **raw fit** — and `emp_by_fit[[i]]` carries `$frame` + `$fits`, **60–100 MB at n = 200 000** | ❌ blocked as written: six to ten times the payload §5's constraint was written about |

### 6.2 The ceiling, and why balance decides it

`whole` is the real call; `units` are the same models built one at a time.
`ceiling = max(max unit, (whole − sum units) + max unit)` — a perfect pool with a core per unit.
⚠ it is **conservative**: a unit built alone re-runs the argument boundary the real call runs once,
so `sum units` can exceed `whole` (it does on five of the eight rows), and the clamp is what keeps
the ceiling from falling below the longest unit, which no number of extra cores can shorten.

| shape (n = 200 000 unless stated) | whole | max unit | ceiling | balance | speedup |
|---|---|---|---|---|---|
| G `tab_vars` 4 groups (race — **uneven**) | 1.73 s | 0.94 s | 1.41 s | 2.23 | **1.23×** |
| G `tab_vars` 8 groups (year — **even**) | 1.93 s | 0.28 s | 0.85 s | 1.63 | **2.28×** |
| G `tab_vars` 4 groups (race), n = 21 483 | 0.29 s | 0.13 s | 0.19 s | 1.70 | 1.53× |
| S 2 outcomes, one table | 5.21 s | 2.79 s | 2.79 s | 1.04 | 1.87× |
| S 4 outcomes, one table | 9.14 s | 3.19 s | 3.19 s | 1.25 | **2.86×** |
| S 3-model comparison, **unbalanced** | 4.24 s | 3.21 s | 3.21 s | 2.09 | 1.32× |
| S 3-model comparison, **balanced** | 7.21 s | 3.10 s | 3.10 s | 1.19 | **2.33×** |
| R 2 outcomes × a models list | 10.27 s | 5.44 s | 5.44 s | 1.05 | 1.89× |

**Balance, not unit count, is the variable.** The same axis at the same size gives 1.23× over four
uneven race groups and 2.28× over eight even survey waves; the same three-model comparison gives
1.32× when one model dominates and 2.33× when they are alike. Two units cannot reach 2× at all once
the shared remainder is counted (S 2 outcomes: 1.87× at a balance of 1.04, which is as even as an
axis gets).

⚠ **These ceilings carry ±0.1–0.35× of run-to-run noise**, which is worth more than a footnote when
the decision bar is 2×. A second run of the same harness on the same tree gave G-8-even **2.11×**
(against 2.28×), S-4-outcomes **2.52×** (against 2.86×) and the unbalanced comparison **1.41×**
(against 1.32×). So "2.86×" honestly reads "about 2.5–2.9×, before any implementation overhead" —
and only the S axis is clear of the bar by more than the noise.

### 6.3 Transport is NOT the obstacle — which is worth recording, because it was assumed to be

| | |
|---|---|
| the 200 000-row fixture, serialized | 16.0 MB (gss_cat itself: 1.3 MB) |
| `daemons(4)` spin-up | 0.59 s — once per session |
| first 4-task round-trip | 1.67 s — dispatcher connection setup, once per pool |
| `everywhere()` ship of the 200 000-row frame | **0.05 s** — once per dispatch |
| **warm** 4-task round-trip | **0.003 s** — once per dispatch |

The `big_df` figure §26 of the decisions doc records ("transfer is the killer", 6.8 s) is a 161 MB
fixture; at survey scale the ship is two orders cheaper and a warm dispatch is free. So the honest
statement is that **the model axis is bounded by Amdahl and by balance, not by serialisation**.

### 6.4 The redundancy: one more "computed k times, read once"

Call counts, one instrumented run each (n = 21 483), before → after
(`results_2.0.0/phase20f2_redundancy_{before,after}.csv`). `reg_fit` is the whole cost unit; the two
`reg_empirical*` columns are what 20f-ii changed.

| shape | `reg_fit` | `reg_empirical` | `reg_empirical_fit` |
|---|---|---|---|
| 1 model, `empirical` | 3 | 1 | 1 |
| **3-model comparison, `empirical`** | 9 → **5** | 3 → **1** | 3 → **1** |
| **3-model comparison, `color = "adjustment"`** | 9 → **5** | 3 → **1** | 3 → **1** |
| 2 outcomes, `empirical` (must NOT change) | 6 | 2 | 2 |
| `tab_vars` 4 groups | 3 | 0 | 0 |
| 2 outcomes × a models list | 4 | 0 | 0 |

The "must NOT change" row is the half of the contract that says the fix did not over-reach: two
outcomes are two genuinely different crude blocks, and they stay two. `reg_skeleton` is 3 for four
`tab_vars` groups and 2 for a two-outcome recursion — per-unit rebuilds on the FULL frame, left
alone here and noted for whoever restructures those axes.

In **comparison mode** every input to the `_empirical` loop is table-wide or per-*outcome*, and a
models list is refused unless it has exactly one outcome — so specs 2..S recomputed spec 1 exactly,
and only spec 1 was ever read (`reg_stage_assemble()` takes `emp_by_fit[[1]]` as every column's
`obs` *and* as its gap-test crude leg). The one other reader, `reg_stage_tips()`'s numeric block,
emitted duplicate rows for a column name every spec resolves identically, which
`tab_export_prep()`'s `match()` then discarded first-wins. Fixed with the idiom the `add_n` loop 70
lines up already uses: `if (i > 1L && n_outcomes <= 1L) break`.

**This is 20f-i's finding repeating one axis out**, and it is worth more than the pool would have
been on the same shape: a free `(S−1)/S` of the whole crude stage on every model comparison with a
crude companion — which `color = "adjustment"` turns on automatically.

### 6.5 What the S axis would cost

The S axis is where the ≥2× shapes are (2.86× at four outcomes, 2.33× at three balanced models), and
it is the one that cannot be dispatched as written. Making it dispatchable is a `reg_build()`
restructure of 20e's size — the six per-spec loop *bodies* lifted out of the table-scalar stages into
one `reg_spec_build()` returning a declared product, the stages becoming cross-spec assemblers
("20e one grain finer"). Four things constrain it, all verified in the code rather than assumed:

1. **`reg_compare_rows()` cannot be ported.** It needs two fit *objects*: `stats::anova(m_lo, m_hi)`
   — the `method = "Wald"` → `regTermTest` arm on a survey fit — plus `reg_compare_guard()` and
   `reg_aic_value()`. Re-implementing survey's Wald arithmetic would make tabxplor a second producer
   of a survey quantity, the same class as §3's measured `drop1` vs `anova` divergence (12.47 against
   14.25). It stays, and **forces the serial path** — a fact about the statistic, not a limitation:
   a between-model test needs the models together. It returns early on `compare == "none"`, which is
   the default, so this excludes far less than it sounds.
2. **Comparison mode with a crude block cannot go parallel either**: spec 1's block is every column's
   `obs`, and it carries the 60–100 MB frame of §6.1.
3. **A compound formula** takes its shared skeleton from `fits[[1]]`.
4. **The message stream turns stage-major → spec-major**, so `dev/verify_reg_specs.R` stops printing
   IDENTICAL for multi-spec cases and prints *"(same set, different ORDER)"* instead. Detectable, not
   silent — but it is the one irreducible price, and it must be declared.

What is left parallel after those: **several outcomes** (any `empirical`), and **the default models
list**, where `compare = "none"` and `empirical = FALSE` mean there is no shared crude block at all.

### 6.6 Verdict

- **G and R could be dispatched today**, reusing `tab_pmap()` with no restructure — but they clear
  a ≥2× bar only for an *even* axis at survey scale, where the whole saving is about a second.
- **S is where the 2×+ shapes are**, and it needs §6.5's restructure first.
- **What shipped in 20f-ii regardless of any of that**: §6.4's de-duplication, and a guard on a
  latent defect found beside it — `compare` was gated nowhere, so
  `outcome = c("a","b"), stats = "compare_baseline"` reached `reg_compare_rows()` with two different
  responses, where `anova.glmlist`'s own `sameresp` filter silently dropped a model and the surviving
  row was labelled with the wrong outcome.
