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
