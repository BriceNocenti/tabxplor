# The weights framework, stress-tested — where the three rungs stop, and what to do about it

Date: 2026-08-11. Status: **RESEARCH ONLY** — no R code was modified. Working tree at `2cdfc60`
(Phase 18z14-iii), `survey` 4.5, R 4.6.1. Every number below is produced by the reproducer in
**Appendix A** (run it as `OMP_NUM_THREADS=1 Rscript <file>`, ~4 minutes); block letters in that
appendix are cited as *(block X)*.

The question, as asked: *the package offers three levels of use for survey weights — (1) weighted
estimates + unweighted n, (2) weights + Kish `n_eff`, (3) a full survey design. Are all three
followed everywhere meaningful, in `tab()` **and** in `tab_reg()`? Are there inconsistencies or
statistically unsound places? Where is there room for more consistency, simplification, or
integration into one readable ecosystem?*

**➜ The cure is `dev/weights_framework_redesign.md`** (2026-08-11): the maintainer's rulings on §6,
plus the finding that turns §9's proposal into the whole reorganisation — a weight column IS a
survey design, its variance has a closed form in the per-cell `Σw²`, and that closed form
reproduces `survey` *exactly*, `svychisq` included. Read this document for the diagnosis, that one
for what replaces it.

Companion documents: `dev/full_survey_design_scope.md` (the z14 study — the ladder's design and its
measurements), `dev/model_vs_observed_gap_test.md` §3.8 (where the gap test stops holding),
`dev/tabxplor_2.0.0_decisions.md` §14 (the weighted-inference convention) and §51 (the robust tests).

---

## 0. Executive summary

**The machinery is sound; the ladder is not one ladder.**

Three separate verifications say the *statistics* are right. The design variance `tab()` writes into
`n_eff` reproduces `survey` **exactly** — variance ratio `1.000000` on weights-only, stratified,
clustered, **calibrated**, and `tab_vars` subtable domains alike (§4, block F). The Kish rung is not
an approximation of nothing: on a weights-only design it lands on the linearization answer to **four
significant figures** (§3, W6). And the machinery composes: a design reaches every `tab()` surface I
could construct (§4).

What fails is **coherence across the three rungs, across the two functions, and across the two
leaves**. Thirteen findings, five of them severe:

| # | finding | severity |
|---|---------|----------|
| **W1** | one `Obs_*` column of `tab_reg()` mixes **two rungs at once** — factor rows on rung 1, numeric rows on the linearization rung — differing by 10 %, with no marker | **severe** |
| **W2** | the `Model_*` column has **never been on rung 1 or 2**; `?tab_reg` and the vignette say it follows the same ladder as the crude one | **severe** |
| **W3** | `color = "contrib"` under a design is design-corrected on a counts table and **not** on a percentage table — measured p `0.052` vs `1.6e-11` for the same cell of the same data | **severe** |
| **W4** | when the design variance cannot be computed, the intervals silently fall back to the raw n **but the footer keeps claiming they are design-based** — in every export, forever | **severe** |
| **W13** | on the exported step path, `tab_num(design)` then `tab_ci()` returns **raw-n** intervals (1.6× too narrow) where `tab_plain(design)` then `tab_ci()` returns design-based ones — and the footer claims design-based in both | **severe** |
| **W5** | the rung is *computed* (`svy_inference_mode()`) but **never stored**; downstream re-derives it by string-sniffing the weight name, and rung 2 is invisible on the table | major |
| **W6** | rung 1 is not a defensible inferential position — it is "no correction", it is the **default**, and it is the only rung at which `tab()` and `tab_reg()` disagree | major |
| **W7** | the design's **degrees of freedom** are never consulted: −15 % on a proportion interval at 10 PSUs, −6.8 % on a mean | moderate |
| **W8** | the `n` column of the `test` attribute means three different things across the three rungs | moderate |
| **W9** | `tab_counts()` cannot climb past rung 1 and says so nowhere on the table | minor |
| **W10** | `wt =` is silently ignored when `data` is a design | minor |
| **W11** | jamovi: Regressions has **no** rung control at all; Crosstables' control is labelled "Type of p-value" but also moves every confidence interval | minor |
| **W12** | four small white elephants left by z14 (a redundant conjunction, a stray `getOption()`, three `weighted` predicates, one dead branch) | cosmetic |

**The one-sentence diagnosis.** Phase 18z14 built the rung ladder for `tab()`'s main pipeline and
did it well; it did **not** extend it to `tab_reg()` — where the model column had always been at a
*higher* rung than the ladder describes — nor to the exported step path, and it left the rung as a
build-time local instead of a stored fact, so nothing downstream (footer, legend, exporter, jamovi)
can say which rung produced the numbers, and three code paths can claim a rung they did not reach.

**§9 answers a proposal made after the findings above, and it changes the conclusion.** Replacing
Kish by a **minimal (weights-only) design** `n_eff` is not merely equivalent to Kish — it is *exact*
where Kish is an approximation (Kish is measured up to 17 % wrong once the outcome follows the
weight), and it makes `empirical = TRUE` match the univariable minimal-design model **to ratio
1.000**, because at `ids = ~1` the covariance ruling Q3 discards is exactly zero. Implemented
literally (synthesize a design, run Route A) it is 15–80× slower than Kish and inherits a silent size
ceiling — but it does not have to be: at `ids = ~1` the design variance has a **closed form in the
per-cell `Σw²` the aggregate already computes**, verified exact for row / column / grand-total
percentages and for means. That makes the proposal *cheaper* than what it replaces, and it collapses
three rungs of implementation into two. §9.7 lists the four caveats, one of which (the `contrib`
regression) is blocking.

**The cheapest structural fix is also the biggest one.** `reg_crude_if_maker()` +
`reg_if_se()` — the influence-function pair Phase z8-B built for the gap test — *already compute*, on
every `empirical = TRUE` table, the design-consistent standard error of exactly the crude quantity
whose interval W1/W2 say is at the wrong rung. Routing the crude column's own interval through the SE
it is already being tested against costs no new statistics, removes a rung from the ladder, and makes
"the interval you see and the test that greys it" one number instead of two (§5, P3).

---

## 1. Method

Everything is measured on a synthetic survey where each design feature really bites — the same
generator shape as `dev/survey_design_measurements.R`, so the two studies are comparable: 6 strata ×
40 PSUs × 25 respondents (n = 6000, 240 PSUs, `degf` = 234), a PSU random effect (real ICC), unequal
weights (Kish `n_eff` = 4922), and a row variable **segregated** across PSUs — region / race /
urban-rural / school / *quartier*, the shape a sociologist's row variable actually has.

Four instruments:

1. **The flip test.** Build the same table three times — `wt` only, `wt` + `kish_neff`, a design
   object — and diff **every field of every `fmt` column**. What moves is what the rung reaches;
   what does not move is what it does not. This is the only way to answer "is the rung followed
   everywhere" without trusting a comment.
2. **The oracle.** `survey` itself (`svyby(svymean)`, `svyglm`, `svychisq`) recomputes each quantity;
   ratios are reported to 6 digits.
3. **The regime probe.** For one crude effect, compare the SE tabxplor's printed bracket *implies*
   with the SE the same effect gets from an unweighted `glm`, a weights-only `svyglm`, and a full
   `svyglm` — which pins each printed interval to a rung, objectively.
4. **A forced degrade.** `svy_var_prop()` is replaced by `function(...) NULL` in the namespace to
   simulate a design whose variance cannot be computed, and the resulting table is rendered.

---

## 2. The ladder as specified, and as implemented

### 2.1 What the documentation promises

`?tab` (R/tab.R:238-256, 406-431) and both intro vignettes state the ladder cleanly, and — for
`tab()` alone — accurately. The vignette's table:

| you pass | what the intervals carry |
|---|---|
| `wt = w` | the weighted estimate, on the raw *n* — no design effect at all |
| `wt = w` + `options(tabxplor.kish_neff = TRUE)` | unequal weighting only — blind to clustering, blind to calibration |
| a `survey` design as `data` | the real design effect: strata, clusters, `fpc`, calibration |

followed by: *"`tab_reg()` regression tables follow the same rule, models and observed companions
alike."* — **that last sentence is false at rungs 1 and 2** (W2).

`?tab_reg`'s own text is more careful but still understates: it says a continuous predictor's crude
companion "uses the model's own variance, like the `Model_*` column beside it" and that the two rules
"will not agree to the last digit". Measured, at rung 1 they disagree by **10 %**, systematically, in
one direction (W1).

### 2.2 What is actually implemented — the full inventory

`W` = the weights are used. `D` = the design *structure* is used. `K` = Kish's `n_eff` is used when
opted in. **Bold** marks a cell where the implemented rung differs from the documented one.

| # | quantity | rung 1 (`wt`) | rung 2 (`+kish`) | rung 3 (design) | evidence |
|---|----------|---------------|------------------|-----------------|----------|
| **`tab()`** |
| 1 | cell % / mean | W | W | W + D | block A |
| 2 | cell CI (`ci = "cell"`) | raw n | K | D exact | A, F |
| 3 | diff CI + stars | raw n | K | D (conservative on the covariance, ruling Q3) | A |
| 4 | ratio CI (Katz / mean-ratio) | raw n | K | D | A |
| 5 | `OR` / `cumOR` interval | raw n | K | D | A, E |
| 6 | `contrib` residual, **counts / `pct="all"`** table | raw n | K | D | E |
| 7 | `contrib` residual, **`pct="row"/"col"`** table | raw n | K | **raw n** | **E (W3)** |
| 8 | whole-table chi2 / F | W (rescaled to raw n) | W + K (Rao-Scott 1st order) | W + D (`svychisq`) | A |
| 9 | Cramér's V / φ / η² | W | W | W (no D — descriptive, correct) | E |
| 10 | Fisher exact | dropped when weighted | dropped | dropped | E |
| 11 | `n_min`, displayed `n`, `add_n` | raw n | raw n | raw n (correct) | F |
| 12 | degrees of freedom of the pivots | z / t(n−1) | z / t(n_eff−1) | **z / t(n_eff−1)**, never `degf` | **F (W7)** |
| 12b | step path: `tab_plain()` + `tab_ci()` | raw n | K | D (== `tab()`) | H |
| 12c | step path: `tab_num()` + `tab_ci()` | raw n | **raw n** | **raw n** (1.6× too narrow) | **H (W13)** |
| **`tab_reg()`** |
| 13 | `Model_*` estimate + CI + p | **W + linearization** | **W + linearization** (Kish does not reach it) | W + D | **C (W2)** |
| 14 | `Obs_*`, **factor** rows | raw n | K | D | C |
| 15 | `Obs_*`, **numeric** rows | **W + linearization** | **W + linearization** | W + D | **C (W1)** |
| 16 | `Obs_*`, ordinal rows (`Obs_cumOR`) | **W + linearization** (univariable `polr`/`svyolr`) | idem | W + D | code |
| 17 | `gap_se` (`color = "adjustment"`) | **W + linearization** | **W + linearization** | W + D | C |
| 18 | `obs` folded in-cell (multinomial) | W | W | W + D (tooltip bases) | code |
| 19 | GOF footer / `compare` / `stats` | W + D | W + D | W + D | code |
| 20 | frozen SD for `multiplier = "sd"` | W | W | W | code |
| **elsewhere** |
| 21 | `tab_counts()` | raw n | **raw n** (unreachable) | refused (correct) | **E (W9)** |
| 22 | jamovi Crosstables | ✓ | ✓ (mislabelled) | out of scope (ruled) | **W11** |
| 23 | jamovi Regressions | ✓ | **absent** | out of scope (ruled) | **W11** |

Read rows 13–17 together: **inside one `empirical = TRUE` table at rung 1 there are two inferential
regimes, and the boundary runs through the middle of a single column.**

---

## 3. The findings

### W1 — one `Obs_*` column, two rungs (severe)

`tab_reg(d, y ~ grp + num, family = "binomial", wt = "w", empirical = TRUE)` — default rung 1. The
`Obs_OR` column's implied SE(log OR), against the same crude effect computed three ways *(block C)*:

| row | tabxplor `Obs_OR` | unweighted `glm` | weights-only `svyglm` | verdict |
|---|---|---|---|---|
| `grp` B (factor) | **0.07412** | 0.07438 | 0.08136 | sits on the **unweighted** answer |
| `num` (numeric) | **0.00224** | 0.00204 | 0.00224 | sits on the **weighted linearization** |

The factor rows are on rung 1 (×0.911 of the design-consistent SE — too narrow). The numeric row is
two rungs above them, in the same column, in the same table, under the same call. Nothing marks the
boundary; nothing in the legend or the footer distinguishes the two.

**Why.** Phase z9 gave numeric predictors a crude twin by *re-calling `reg_fit()`* with one predictor
(`reg_empirical_numeric()` → `reg_empirical_fit()`), and `reg_fit()` routes any weighted model through
`svyglm` (R/tab_reg.R:1187, 1281). Factor rows keep the closed-form path (`reg_empirical()`), whose
CI base is `emp_n_ci` / `emp_n_draw` — the Kish-or-raw ladder (R/tab_reg.R:1622-1626). Two producers,
two variance rules, one column. Phase z10 then extended the *fit* producer to ordinal predictors
(row 16), widening the split rather than closing it.

**This is D1's variance twin.** Phase z14-i fixed the *point estimates* being computed on two
different populations; the *variances* are still computed under two different regimes.

### W2 — the model column was never on rungs 1–2 (severe)

Same table, the SE of the *same* effect at each rung *(block C)*:

```
rung             Obs_OR    Model_OR   Obs/Model
1 (wt only)     0.07412    0.08137      0.911
2 (wt + kish)   0.08135    0.08137      1.000
3 (design)      0.13863    0.14135      0.981
```

`Model_OR` **does not move at all** between rungs 1 and 2 — the flip test confirms it: turning
`kish_neff` on moves `Obs_% ci_inf/ci_sup/pvalue` and `Obs_OR ci_inf/ci_sup/pvalue` and **nothing
else** *(block C)*. It cannot move: a weighted `tab_reg()` has always fitted through `svyglm`, whose
variance is the Binder linearization — i.e. the design-based answer for a flat `ids = ~1` design.

So the ladder for `tab_reg()` is really:

| rung | `Model_*` | `Obs_*` factor | `Obs_*` numeric | `gap_se` |
|---|---|---|---|---|
| 1 | linearization | raw n | linearization | linearization |
| 2 | linearization | Kish ( ≈ linearization) | linearization | linearization |
| 3 | design | design | design | design |

**Rung 2 is where `tab_reg()` becomes coherent, and rung 1 — the default — is the only incoherent
one.** That is the opposite of how the ladder is presented ("rung 1 = the honest default; climb if
you want more").

### W3 — `contrib` under a design: corrected on counts, not on percentages (severe)

Same data, same design, same colour measure, same cells — only the `pct` argument differs
*(block E)*. Adjusted standardized-residual p-values of the `yes` column:

```
counts table (pct = "no"), weights only :  1.56e-11  2.67e-05  0.0155  6.98e-18
counts table (pct = "no"), DESIGN       :  0.0518    0.237     0.513   0.0234
row %       (pct = "row"), weights only :  1.56e-11  2.67e-05  0.0155  6.98e-18
row %       (pct = "row"), DESIGN       :  1.56e-11  2.67e-05  0.0155  6.98e-18     <- unchanged
```

On a counts table the design moves the first cell from `1.6e-11` to `0.052` — from "the strongest
result on the page" to "not significant". On a percentage table of the same data the design changes
**nothing**: the residual keeps a base that assumes an SRS of 6000.

**Why.** `chi2_write_contrib()` (R/tab.R:6371-6376) reads the cell's own `n_eff` only when the
column's stored `type` is `"n"` / `"all"` / `"all_tabs"`; on a `"row"` / `"col"` column it reads the
Total column's `n_eff`, which under a design is `NA` (the total column is p = 1, so
`p(1−p)/Var` is degenerate) and falls back to the raw `n`. Under Kish the same read *does* work,
because Kish's total-column `n_eff` is the table's Kish n — which is why this is a **design-only**
defect and the flip test at rung 2 shows no problem.

This is documented in `?tab` as a residue ("design-corrected on a counts / `pct = "all"` table but
keeps the unweighted base on a row- or column-percentage one"), and `dev/full_survey_design_scope.md`
§4.5 records the reasoning. **The measurement says the residue is not small**, and the trap is that
`color = TRUE` picks `contrib` automatically on a counts table and `diff` on a percentage one — so a
user comparing the same data in the two shapes sees two irreconcilable significance patterns, both
labelled "design-based" in the footer.

**And the fix is available and cheap** (§5, P4): the degeneracy is only in the *total column*. The
cell's own joint proportion on the `"all"` base is not degenerate; one extra `svy_var_prop(base =
"all")` call, made only when `contrib` colouring is on, gives every cell exactly the base the counts
table already uses. No new concept, no new field.

### W4 — the design footer keeps its claim after a silent degrade (severe)

`svy_var_prop()` / `svy_var_mean()` return `NULL` rather than a wrong number — the right discipline —
and the leaf then falls back to Kish or the raw n. The user is told by `svy_var_degraded()`, a
`cli_inform()`. Forced degrade *(block G)*:

```
n_eff after forced degrade : NA NA NA NA NA
CI half-width              : 0.0288 0.0282 0.0272 0.0271     (the design ones are ~0.048)
footer still says          : "Design-based (survey): weighted estimates, intervals and tests
                              account for the sample design."
```

A console message is not a property of the table. `suppressMessages()`, an Rmd chunk, `tab_export()`,
`tab_html()`, jamovi's backend — all of them drop it, and what survives is a table whose footer
asserts, permanently and in every export, something that is **not true of its numbers**. The z14-ii
comment (R/fmt_class.R:4816-4820) states the opposite as the design intent: *"a table whose design
variance could NOT be computed says so at build time … so the sentence is never silently untrue."*
The build says so; the *table* does not.

**How reachable is it?** `svy_var_prop()` bails when `R × nfr > 5e7` (R/survey-variance.R:181) —
about 400 MB of influence matrix, a guard that should exist. At a realistic French survey scale of
60 000 rows that trips above **833 wide rows** — a `tab()` over a fine geography, a merged multi-row_var
table, or `tab_vars × row_var` with many levels. It also fires on any `svyrecvar` error. This is not
an exotic path.

`tab_reg()` has the identical structure (R/tab_reg.R:1646) and the identical claim.

### W13 — the two leaves disagree on the exported step path (severe)

`tab_plain()`, `tab_num()` and `tab_ci()` are exported (superseded, not deprecated — Phase 17f
quarantined them to `R/tab-steps-legacy.R` with parity tests, and `?tab_ci`'s own example is the step
chain). Same design, same data, cell intervals *(block H)*:

```
tab_plain(des, grp, col, pct="row", tot="col") |> tab_ci("cell")   half = 0.0470 0.0485 0.0427 0.0398
tab(des, grp, col, pct="row", ci="cell")                           half = 0.0470 0.0485 0.0427 0.0398   identical

tab_num(des, grp, num, tot="row")             |> tab_ci("cell")    half = 0.6517 0.6618 0.6111 0.6688
tab(des, grp, num, ci="cell")                                      half = 1.0469 1.0853 1.2320 1.3539   1.6x wider
n_eff on the step path: NA NA NA NA      |  via tab(): 551.3 558.5 395.0 364.2
```

The factor leaf writes `n_eff` **at build**, unconditionally, inside `leaf_wide_pct()` — so whatever
consumes the table later (including a hand-written `tab_ci()`) finds the design base waiting. The
numeric leaf writes its `_en` only inside `if (ci %in% c("cell", "diff"))` (R/tab.R:5257-5280), and
`tab_num()`'s own `ci` defaults to `"no"` — so a direct `tab_num(design)` discards the design
variance, and the `tab_ci()` that follows silently falls back to the raw n.

Two things make this worse than a step-path curiosity. First, the intervals are not merely different,
they are **1.6× too narrow** — the full clustering effect, restored. Second, the table still carries
`wt = ".svy_weights"`, so its footer says *"Design-based (survey): weighted estimates, intervals and
tests account for the sample design."* — W4's claim-vs-fact defect again, on a path that needs no
degrade to trigger it.

**Why it is a one-line class of fix, not a special case**: the asymmetry exists because the numeric
leaf treats `n_eff` as a by-product of computing a CI, while the factor leaf treats it as a property
of the cell. The second is right — `n_eff` is documented as "the effective sample size used for this
cell's confidence interval", i.e. a base, not a result. Writing it whenever the rung is `"kish"` or
`"survey"`, regardless of `ci`, makes the two leaves say the same thing.

### W5 — the rung is computed but never stored (major)

`svy_inference_mode()` (R/survey-design.R:102-106) is the ladder's single resolver, called in
`tab_setup()` and the two leaves. It resolves to `"survey"` / `"kish"` / `"classic"`, rides `ctx`, and
then **disappears**. It is not written to `meta`. Verified *(block E)*: `get_ci_settings()` is
**byte-identical at all three rungs** —

```
$conf_level 0.95  $method_cell "wilson"  $method_diff "newcombe"
$method_ratio "katz"  $method_mean_diff "welch"  $method_mean_ratio "robust"
```

— it records *which formula*, never *on which base*. Four consequences:

1. **The footer cannot name rung 2.** Rendered footers *(block E)*:
   ```
   rung 1: "Weighted by w."
   rung 2: "Weighted by w."          <- identical, though every interval moved
   rung 3: "Design-based (survey): …"
   ```
   Measured, rung 2's intervals on this table are ×1.10 of rung 1's on the row-percentage table, and
   the `pct = "col"` Kish base is 2330.8 where the design's is 245.9 *(block B)*. A reader of an
   exported table cannot tell which of the two they are looking at.
2. **Rung 3 is detected by a string sniff.** `tab_weight_line()` decides "this table is design-based"
   with `identical(as.character(wt)[1], svy_wt_col)` (R/fmt_class.R:4821) — i.e. by recognising the
   internal sentinel column name `.svy_weights`. That is precisely the pattern Phase 17's rule 2
   outlaws ("roles are stored, never guessed"); it survived because there was nowhere to store the
   fact.
3. **W4 has no home.** "Design asked for, design not delivered" is a fourth state of the same fact
   and there is no field to put it in.
4. **`tab_reg()` cannot share it.** `reg_empirical()` calls `svy_inference_mode()` for itself
   (R/tab_reg.R:1622); every other reg consumer branches on `design_spec$design` / `design_spec$wt`
   directly (7 sites), and `reg_fit()` keeps its own `weighted <- !is.null(design) || !is.null(wt)`
   (R/tab_reg.R:1187), duplicated at R/tab_reg.R:4636. One ladder, four encodings.

### W6 — rung 1 is not a rung (major)

Rung 2 reproduces the linearization answer to four significant figures on a weights-only design
*(block C)*: Kish `0.08135` vs `svyglm(ids = ~1)` `0.08136`. That is not a coincidence — for
`ids = ~1` with no fpc the Binder linearization of a domain mean and the Kish rescale are the same
first-order correction.

So the ladder's real shape is:

* **rung 2 = the correct single-stage answer**, and it is what `tab_reg()`'s model column has always
  silently used;
* **rung 1 = no correction at all**, and it is the default.

`?tab` is admirably honest about this ("under unequal weights this carries no design effect, so the
default interval is **too narrow**") — but a default that the documentation describes as wrong, that
one half of the package silently declines to use, and that costs one `Σw²` accumulation to fix, is
hard to defend. The measured cost of rung 2 on this table is one extra `w2` column in the aggregate
scan; the measured benefit is that `Obs_*` and `Model_*` stop disagreeing by 10 %.

**Caveat, stated honestly**: making Kish the default is a *silent numerical change* for every existing
weighted user — intervals widen, stars disappear. It is a CRAN-visible behaviour change on `tab()`,
not an internal one. §6 Q2 puts the options.

### W7 — the design's degrees of freedom are never consulted (moderate)

Under a design, `survey` refers every interval to `t(degf)` where `degf = #PSU − #strata`. tabxplor
refers proportions to `z` and means to `t(n_eff − 1)` — `n_eff` being an *effective sample size*,
which has nothing to do with the design's df. Measured *(block F)*, multiplier error:

| PSUs | `degf` | mean `n_eff` | t(n_eff−1) vs t(degf) | z vs t(degf) (proportions) |
|---|---|---|---|---|
| 10 | 8 | 14.7 | 2.149 vs 2.306 → **−6.8 %** | 1.960 vs 2.306 → **−15.0 %** |
| 20 | 18 | 43.2 | −4.0 % | −6.7 % |
| 40 | 38 | 81.9 | −1.7 % | −3.2 % |
| 100 | 98 | 202.6 | −0.6 % | −1.2 % |

Negligible on a national file (hundreds of PSUs); material on a survey the researcher ran themselves
— which §7.6 of the z14 study identifies as *exactly* the audience rung 3 pays off for. The model
column is unaffected (`svyglm` uses `degf(design) − p`), so this is another crude-vs-model seam.

Note the direction: this is **anti**-conservative, unlike the Route-A covariance omission, so the two
do not cancel — they apply to different quantities (df to every interval, the covariance to
differences only).

### W8 — the `test` attribute's `n` column means three things (moderate)

*(block F)*, same table, same data:

```
rung  test        n        min_e     effect_size
1     chi2        6000.0   615.714   0.13456
2     chi2_kish   4928.4   615.714   0.13456
3     chi2_svy    6000.0   615.714   0.13456
```

At rung 2 `n` silently becomes the effective sample size; at rungs 1 and 3 it is the raw count. A
consumer reading `test$n` (the display, an export, a user script) gets a different quantity depending
on a global option. `test$test` discriminates, so it is recoverable — but a column whose *meaning*
changes is the same disease as a role guessed from a label.

### W9 — `tab_counts()` cannot climb, and says so nowhere (minor)

*(block E)*: with `options(tabxplor.kish_neff = TRUE)` and `wt_counts =` supplied, `n_eff` is present
in **0 of 3** fmt columns. Correct — pre-aggregated counts carry no per-observation weights, so
`Σw²` is genuinely unrecoverable — and `?tab_counts` says so. But the *option is global*: a user who
sets it once at the top of a script gets Kish everywhere except here, with no signal on the table.
A design is properly refused with a clear message (verified); rung 2 fails silently.

### W10 — `wt =` is silently overridden by a design (minor)

`tab(des, x, y, wt = w)` builds without a word and stores `wt = ".svy_weights"` *(block E)*.
R/tab.R:635-637 documents the override in a comment. Correct behaviour, but a user who passes both
has a mental model worth correcting, and every other collision in `tab()` (a weight that is also a
row_var, a row_var that is also a tab_var) aborts with a message.

### W11 — jamovi: the ladder is half-present and mislabelled (minor)

* `jmvtab` (Crosstables) has `test_robust` = `classic` / `kish`, titled **"Type of p-value"** with
  the description *"For a weighted table, a more robust p-value"* (jamovi/jmvtab.a.yaml:205-217). It
  sets `options(tabxplor.kish_neff)` around the build (R/jmvtab.b.R:40-42), which since Phase 18s
  moves **every confidence interval, star and colour threshold in the table**, not only the p-value.
  The control does more than its label admits.
* `jmvtabreg` (Regressions) has **no rung control whatsoever** — grep finds no `kish` in
  `jamovi/jmvtabreg.a.yaml`. So in jamovi, Crosstables reaches rung 2 and Regressions cannot, which
  is the reverse of where the inconsistency bites (W1/W2 are regression findings).

Ruling §7.4 of the z14 study ("jamovi is out of scope: rungs 1–2 only") is *about rung 3*. Rung 2 is
in scope by that same ruling, and Regressions does not have it.

### W12 — white elephants left by z14 (cosmetic, but they are the ones that grow back)

1. **A redundant conjunction, twice.** `design_on <- identical(inference_mode, "survey") &&
   !is.null(design_spec$design)` (R/tab.R:3842, 5025). `svy_inference_mode()` returns `"survey"`
   *only* when `design_spec$design` is non-NULL — the second half can never be FALSE when the first is
   TRUE. Two readers now have to prove that to themselves.
2. **A stray option read.** `num_moment_scan()` (R/tab-agg.R:135) still does
   `isTRUE(getOption("tabxplor.kish_neff"))` to decide whether to accumulate `Σw²`, instead of taking
   the resolved mode. It is the one surviving direct read outside `svy_inference_mode()` — z14-ii
   retired the others. Harmless today (it over-accumulates under a design), but it is a second
   encoding of the rung.
3. **Three `weighted` predicates.** `reg_fit()` R/tab_reg.R:1187, `reg_resolve_multiplier`'s caller
   R/tab_reg.R:4636, `tab_counts` R/tab-counts.R:157-162 — each spelling "is this weighted" its own
   way.
4. **Three `kish` gates** spelled three ways (`kish`, `kish_neff_on`, `identical(...)`), plus the
   `reg_empirical()` one.

None of these is a bug. All four are the "two encodings of one fact" pattern that Phase 17 spent
itself removing, re-accumulating in the newest subsystem.

---

## 4. What is exact — so nobody re-litigates it

Recorded deliberately, because the findings above should not cast doubt on the parts that are right.

**The design variance producer is exact.** `p(1−p)/n_eff` against `svyby(svymean)`, variance ratio
*(block F)*:

| design | ratio |
|---|---|
| weights only (`ids = ~1`) | `1.000000` |
| stratified + clustered, 240 PSUs | `1.000000` |
| 10 PSUs (`degf` 8) | `1.000000` |
| **calibrated** (`calibrate()` on a continuous auxiliary) | `1.000000` |
| **`tab_vars` subtable domains** | `1.000000` (all 8) |

Means likewise (`s²/n_eff` vs `SE(svyby)²`, ratio `1.000000` in all four).

**A design reaches every `tab()` surface I could construct** *(block E)*: `pct = "row"/"col"/"all"`,
counts, means, mixed col_vars, `tab_vars`, `comp = "all"`, two row_vars, `tab_plain()` /
`tab_num()` called directly, and `OR = "cumOR"`. The only `NA` `n_eff` cells are the degenerate
p = 1 ones (the Total column of a row-percentage table, the Total row of a column-percentage one),
which have no interval to compute. Total rows and total tables **do** get a design base — the
"`Total` = every level" rule works as designed.

**Rung 3 governs the whole-table test coherently** with the cells: `chi2_svy` (Rao-Scott F,
`p = 1.4e-07`) beside design-based cell intervals, where rung 1 gave `chi2` (`p = 1.5e-26`) beside
raw-n intervals. The test and the cells describe the same sample at every rung — which is exactly
what z14-ii's `svy_inference_mode()` was for, and it works.

**The effect size is weighted and not design-corrected**, which is right: Cramér's V / η² are
descriptive population parameters, and z14-i's ruling Q6 made them weighted. Verified: V moves
`0.2048 → 0.2493` when weights are added *(block E)*.

**Fisher is dropped when weighted** (`pvalue_exact` `0.4379 → NA`), correctly.

**`n_min`, the displayed `n` and `add_n` stay on the raw count** at every rung — correct: they answer
"how many people are behind this cell", not "how much information".

---

## 5. Proposals

Ranked by (value / blast radius). Each states what it removes, not only what it adds.

### P1 — store the rung (unblocks W4, W5, W8, W9, W11; enables P3–P5)

Add **one field** to the metadata that already exists: `meta$ci_settings$inference`, one of
`"classic"` / `"kish"` / `"design"` / `"design_degraded"`, written where
`svy_inference_mode()` is already resolved (`tab_setup()`, the two leaves, `reg_build`), absent when
`"classic"` (so the "absent when unset" rule holds and no golden moves for unweighted tables).

What it **removes**:

* `tab_weight_line()`'s `identical(as.character(wt)[1], svy_wt_col)` string sniff — the last
  role-guessed-from-a-name in the weights subsystem;
* the impossibility of naming rung 2 in the footer;
* the impossibility of recording a degrade (`svy_var_degraded()` writes `"design_degraded"` instead
  of only informing, and the footer then says so — W4 becomes structurally unrepresentable);
* jamovi's need to re-derive anything.

What it **adds**: three footer sentences instead of two, one per rung, plus the degraded one. It is
the same shape as `role` (Phase 17c) and `conf_level` (Phase z13): a fact that was being re-derived
becomes a stored fact.

Cost: ~40 lines + one `gettext` string per new sentence + FR translations. No fmt field, no column
attribute, no cache-schema change (`meta` already rides the carrier — though a `JMVTAB_CACHE_SCHEMA`
bump is cheap insurance).

### P2 — one weighted-inference resolver, read by `tab_reg()` too (W5, W12)

Promote `svy_inference_mode()` to *the* answer for both functions and have every consumer read it:
`reg_fit()`'s `weighted`, `reg_resolve_design()`, `reg_empirical()`, `reg_gap_se_columns()`'s `des`,
`reg_resolve_multiplier()`. Delete the two `weighted <- !is.null(...) || !is.null(...)` copies, the
`num_moment_scan()` option read, and the two redundant `design_on` conjunctions.

This is pure subtraction and byte-identical: the mode already computes to the same answer at every
site. It is the precondition for P3 being one rule rather than a fifth branch.

### P3 — route the crude interval through the SE the gap test already builds (W1, W2, W6)

**The observation that makes this cheap**: on every `empirical = TRUE` table with
`color = "adjustment"`, `reg_gap_se_columns()` already calls
`reg_crude_if_maker()` → `reg_if_se(·, des)` — the influence function of *the crude estimate itself*,
and its design-consistent standard error, on exactly the rows the crude column occupies
(R/reg-influence.R, R/tab_reg.R:2270-2320). The number W1 says the crude column's bracket *should*
carry is already computed a few lines away, and is already what the gap test compares against.

So the crude column's own interval can be built from `reg_if_se(crude_if(...), des)` instead of from
`ci_wilson`/`ci_or` on an effective n. Consequences:

* factor rows, numeric rows and ordinal rows land on **one** variance rule (W1 closed);
* the rule is the same one the model column uses (W2 closed);
* rung 1 and rung 2 stop differing for `tab_reg()` — the ladder collapses to *weighted* vs *design*
  there, which is what it has always actually been (W6 closed for `tab_reg()`);
* "the interval you see" and "the test that greys it" become one number, instead of two computed
  under different assumptions;
* `emp_n_ci` / `emp_n_draw`'s Kish branch can then be deleted from the crude path.

**Caveats, honestly.** (a) The influence-function SE is a *Wald* SE on the link scale; the current
`Obs_%` bracket is **Wilson**, which is better behaved at extreme proportions and small n — the
package chose Wilson deliberately (§14). A hybrid is possible (keep Wilson's *shape*, take its base
from the IF variance via Korn–Graubard's own device: `n_eff = p(1−p)/Var_IF` — which is exactly what
rung 3 already does, so this is *not a new mechanism*, just applying it one rung lower). (b) It makes
`empirical = TRUE` cost one influence-function evaluation per predictor even when
`color = "adjustment"` is off. (c) `reg_crude_if_maker()` has a closed form only where the univariable
model is saturated (`reg_crude_saturated()`); elsewhere the fit path already supplies it.

**Recommended form**: keep every current CI *engine*, and change only the *base* they are given —
`n_eff = p(1−p)/Var_IF` for factor rows, exactly as rung 3 computes it from `Var_design`. Then P3 is
"rung 3's rule, applied at rung 1/2 with the flat design", the code is the code that already exists,
and Wilson/Woolf/Newcombe keep their shapes.

### P3b — write `n_eff` as a base, not as a by-product (W13)

Move the numeric leaf's `_en` computation out of its `if (ci %in% c("cell", "diff"))` guard and gate
it on the rung instead (`inference_mode != "classic"`), as the factor leaf already does. Then
`tab_num(design)` carries the design base whatever the caller does with it next, and the two exported
leaves stop disagreeing.

Cost: a handful of lines. Byte-identical at rung 1 (`"classic"` writes nothing, as today) and on the
`tab()` pipeline (which always asks for a CI when one is wanted); it changes only the *direct*
`tab_num()` entry, which today returns a table that has silently thrown its design variance away.

### P4 — `contrib` on percentage tables under a design (W3)

Compute `n_eff` on the `"all"` base **in addition** whenever `contrib` colouring is on, and let
`chi2_write_contrib()` read that. The degeneracy that blocks it today is only in the *total column*
(p = 1); each cell's own joint proportion is perfectly well defined, and `svy_var_prop(base = "all")`
already produces it — the counts table takes exactly this path.

Cost: one extra `svy_var_prop()` call, gated on `do_ctr && design_on` (so no cost on any other
table), plus one extra column of `n_eff` to carry — or, simpler, write the `"all"`-base value into
`n_eff` only for the contrib consumer via a second small matrix. Removes a documented residue rather
than documenting it harder.

### P5 — design degrees of freedom (W7)

Store `degf(design)` in `design_spec` at the boundary (one line in `svy_unwrap_data()`), and pass it
as the `df` of the pivots that already take one: `ci_pivot()` (mean cell), `ci_mean_diff2()`,
`ci_mean_ratio()`. For proportions the engines are z-based by construction (Wilson, Newcombe, Katz,
Woolf) — the honest options are (a) leave them at z and **document** the ≤15 %-below-30-PSUs effect,
or (b) offer Korn–Graubard (`survey::svyciprop(method = "beta")`) as the design-rung cell method,
which is the textbook answer and would slot into `method_cell` as a third value.

Recommended: (a) for 2.0.0 — it is one `df` thread plus a doc sentence; keep (b) as a named future
option, since it changes the *shape* of the cell interval and would need its own legend wording.

### P6 — name the rung on the table (W5, W8, W9, W11)

Once P1 stores it: a per-rung footer sentence (`"Weighted by w."` → `"Weighted by w; intervals use
Kish's effective sample size."` → the design sentence → the degraded one); `test$n` always the raw
count with the effective n moved to its own column; `tab_counts()` says "intervals use the counts'
own n" when a rung was requested it cannot supply; jamovi's `test_robust` retitled from "Type of
p-value" to what it does, and mirrored into `jmvtabreg`.

### P7 — abort on `wt` + a design (W10)

Three lines, mirroring the weight-collision abort already in `tab_setup()`.

### P8 — documentation truth pass

The vignette sentence *"`tab_reg()` regression tables follow the same rule, models and observed
companions alike"* must go or be corrected, whichever way Q1/Q2 are decided; `?tab_reg`'s "will not
agree to the last digit" must become the measured 10 %; `?tab`'s contrib residue paragraph either
disappears (if P4 lands) or gains the measured magnitude.

---

## 6. Open questions for the maintainer

**Q1 — `tab_reg()`'s crude columns: align upward (P3), or document the split?**
*Recommendation: align upward, in the "same engines, design-derived base" form.* It removes a rung
rather than adding an explanation, it reuses machinery that already runs, and it makes the
`empirical = TRUE` comparison — the feature's whole purpose — an apples-to-apples one at every rung.
Blast radius: `test-tab_reg-empirical.R` value assertions move; no golden (reg tables are not
snapshotted); the jamovi reref byte-identity contract is untouched (it concerns the model fit).
**Maintainer’s decision: align upward**

**Q2 — should Kish become the default when `wt` is given (W6)?**
Three options: **(a)** keep rung 1 as the default (status quo; the split in Q1 then has to be
explained rather than removed); **(b)** make Kish the default for `wt`, keeping
`options(tabxplor.kish_neff = FALSE)` as the opt-out — one silent numerical change for every existing
weighted user, all goldens with weights regenerate, and the documentation becomes shorter by one rung;
**(c)** keep the default but *warn once* per session when a weighted table is built at rung 1.
*Recommendation: (b) if Q1 is answered "align upward" — with (a) the two would still disagree.* This
is the one CRAN-visible behaviour change in the report and the maintainer's call, not mine.
**Maintainer’s decision: this option should only work for `tab()`, the rule being it’s always on with `tab_reg()` when there are weights**. The discrepancy should be documented on the vignette, to precise with concision that the option must be on for the `tab()` version to match the `tab_reg()` empirical column ? By the way, should the `tabxplor.kish_neff` option be renamed now that we use not only Kish, but a closed-form equivalent to a minimal survey-design with only weights (it was never public) ?

**Q3 — `contrib` under a design on percentage tables (P4): fix or keep documenting?**
*Recommendation: fix.* The measured gap (`1.6e-11` vs `0.052`) is too large for a documentation
sentence, and the fix reuses the existing producer.
**Maintainer’s decision: fix. Ensure the result is the same that with counts (since contrib is independant from row/col percentages and have no reference) ?**

**Q4 — the degrade claim (W4): store the state (P1) or drop the sentence when degraded?**
*Recommendation: store it.* Dropping the sentence would silently look like an unweighted table; a
fourth footer sentence ("the sample design could not be applied to this table's intervals") is the
honest artefact.
**Maintainer’s decision: ok (also give the reason with extreme concision if it can be done).**

**Q5 — design degrees of freedom (W7): thread `degf` into the t-pivots, offer Korn–Graubard, or document?** *Recommendation: thread `degf` (means) + document (proportions) now; Korn–Graubard as a named `method_cell` value later.*
**Maintainer’s decision: implement both. Should we make it default with the tabxplor.kish_neff option (or it’s new name) ? And with full design object passed as `data` ?**

**Q6 — jamovi (W11): add the rung selector to Regressions, and retitle Crosstables'?**
*Recommendation: yes to both.* Ruling §7.4 excluded rung **3** from jamovi; rung 2 is in scope by that
same ruling and its absence in Regressions is an oversight, not a decision. Needs a
`jmvtools::prepare()`.
**Maintainer’s decision: yes to both.**

**Q7 — scope and sequencing.** These split cleanly into three sessions:
* **z15-i (metadata + truth)**: P1, P2, P3b, P6, P7, P8 — no numbers move except the footer and the
  direct-`tab_num()` step path (W13, which is a bug fix); otherwise byte-identical on values.
* **z15-ii (the crude alignment)**: P3 (+ Q2 if answered (b)) — the value-moving one.
* **z15-iii (the two residues)**: P4, P5.
*Recommendation: run z15-i first regardless of how Q1/Q2 go* — it is pure subtraction plus one stored
field, and every later decision is easier to express once the rung is a fact on the table.
**Maintainer’s decision: let’s rethink it after the design is finished, but anyway it’s z16 (z15 is assumptions checks framework).**

---

## 7. Honest caveats — what this study does **not** establish

1. **One generator, one seed.** All measurements use a single synthetic survey (6 strata × 40 PSUs ×
   25, segregated row variable) plus small variants for the df block. The *ratios* are stable in
   shape but the exact percentages are not population parameters. The oracle checks (§4) are
   identities, so those hold generally.
2. **Not tested against an oracle**: the multinomial / ordinal crude values under a design (rows 16
   and 18 of §2.2) — I verified the code path exists and reads `svy_inference_mode()`, not that its
   numbers match `survey`; `svy_vglm` / `svyolr` paths under a clustered design; `fpc`-only designs;
   two-stage `fpc`. `dev/full_survey_design_scope.md` §5 and z10's own tests cover parts of this.
3. **Not tested**: `tab_spread()` / `tab_compact()` / `tab_transpose()` carrying the (non-existent)
   rung metadata — moot until P1 lands, but it is where a new `meta` field would need its bind rule.
4. **The `svrepdesign` refusal path** was read, not exercised end-to-end.
5. **Performance**: I did not measure P3's or P4's cost. P4 is one extra `svy_var_prop()` on tables
   that already pay for one; P3 replaces closed-form arithmetic with an influence-function evaluation
   per predictor level and *could* be material on a wide `tab_reg()` — it should be benchmarked before
   it ships.
6. **W6's recommendation is a judgement call, not a measurement.** The measurement says Kish ≈ the
   linearization for `ids = ~1`; whether that justifies changing a CRAN default is a maintainer
   decision about user expectations, not a statistical one.

---

## Appendix A — the reproducer

Save as `dev/weights_framework_measurements.R` and run with
`OMP_NUM_THREADS=1 Rscript dev/weights_framework_measurements.R`. Blocks are cited above by letter.

```r
# dev/weights_framework_measurements.R -- reproducer for dev/weights_framework_stress_test.md
# Blocks: A flip test (tab)   B where the base reaches   C tab_reg regimes
#         D/F oracle + degrees of freedom                E coverage sweep + footers + contrib
#         G forced degrade
suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressMessages(library(survey))
options(survey.lonely.psu = "adjust")

make_survey <- function(seed, S = 6, P = 40, M = 25, segregated = TRUE, sigma_u = 0.55) {
  set.seed(seed)
  d <- expand.grid(m = seq_len(M), p = seq_len(P), s = seq_len(S)); n <- nrow(d)
  d$strat <- factor(d$s); d$psu <- factor(paste0(d$s, "-", d$p))
  u  <- rnorm(nlevels(d$psu), 0, sigma_u)
  pg <- sample(c("A","B","C","D"), nlevels(d$psu), TRUE)
  d$grp <- factor(if (segregated)
    ifelse(runif(n) < 0.85, pg[as.integer(d$psu)], sample(c("A","B","C","D"), n, TRUE))
    else sample(c("A","B","C","D"), n, TRUE))
  lin <- u[as.integer(d$psu)] + seq(-0.8, 0.8, length.out = S)[d$s] +
    c(A = -0.4, B = 0, C = 0.3, D = 0.6)[as.character(d$grp)]
  d$y   <- rbinom(n, 1, plogis(lin))
  d$col <- factor(ifelse(d$y == 1, "yes", "no"))
  d$z   <- factor(sample(c("p","q"), n, TRUE))
  d$x2  <- factor(sample(c("lo","hi"), n, TRUE))
  d$num <- round(rnorm(n, 50, 12) + 8 * u[as.integer(d$psu)])
  w <- c(.5,.7,1,1,1.2,1.5,2,2.6)[d$s] * exp(rnorm(n, 0, .3)); d$w <- w / mean(w); d
}
d    <- make_survey(11)
des  <- svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE)
flat <- svydesign(~1, weights = ~w, data = d)
z    <- qnorm(.975)

FIELDS <- c("n","wn","pct","mean","var","ctr","diff","ratio","ci_inf","ci_sup","pvalue",
            "or","tot_n","n_eff","obs","gap_se")
snap <- function(tb) { fm <- names(tb)[vapply(tb, is_fmt, logical(1))]; out <- list()
  for (nm in fm) for (f in FIELDS) { v <- tryCatch(vctrs::field(tb[[nm]], f), error = function(e) NULL)
    if (!is.null(v)) out[[paste(nm, f)]] <- as.numeric(v) }; out }
diffs <- function(a, b, tol = 1e-9) { k <- union(names(a), names(b))
  sort(k[vapply(k, function(x) { va <- a[[x]]; vb <- b[[x]]
    if (is.null(va) || is.null(vb) || length(va) != length(vb)) return(TRUE)
    ok <- is.finite(va) & is.finite(vb)
    (any(ok) && any(abs(va[ok]-vb[ok]) > tol*pmax(1,abs(vb[ok])))) ||
      !identical(is.na(va), is.na(vb)) }, logical(1))]) }
rung <- function(f) { options(tabxplor.kish_neff = FALSE); a <- f(d)
  options(tabxplor.kish_neff = TRUE);  b <- f(d)
  options(tabxplor.kish_neff = FALSE); c3 <- f(des); list(r1 = a, r2 = b, r3 = c3) }

## ---- A. flip test: what does each rung reach in tab()? --------------------------------------
for (mk in list(
  row  = function(x) suppressMessages(tab(x, grp, col, wt = w, pct = "row",
                                          color = TRUE, stars = TRUE, test = TRUE)),
  cnt  = function(x) suppressMessages(tab(x, grp, col, wt = w, color = TRUE, test = TRUE)),
  mean = function(x) suppressMessages(tab(x, grp, num, wt = w, color = TRUE, stars = TRUE)),
  OR   = function(x) suppressMessages(tab(x, grp, col, wt = w, pct = "row", OR = "OR",
                                          color = "OR", stars = TRUE)))) {
  r <- rung(mk)
  cat("\n-- moves 1->2:", paste(diffs(snap(r$r1), snap(r$r2)), collapse = ", "),
      "\n-- moves 1->3:", paste(diffs(snap(r$r1), snap(r$r3)), collapse = ", "), "\n")
  print(as.data.frame(get_test(r$r2))); print(as.data.frame(get_test(r$r3)))
}

## ---- B. where the design base reaches (total rows / total cols) ------------------------------
show <- function(tb, lab) { cat("\n--", lab, "\n"); fm <- names(tb)[vapply(tb, is_fmt, logical(1))]
  m <- sapply(fm, function(nm) get_n_eff(tb[[nm]])); rownames(m) <- as.character(tb[[1]])
  print(round(m, 1)) }
show(suppressMessages(tab(des, grp, col, pct = "row", tot = c("row","col"), color = TRUE)), "design row%")
show(suppressMessages(tab(des, grp, col, pct = "col", tot = c("row","col"), color = TRUE)), "design col%")
options(tabxplor.kish_neff = TRUE)
show(suppressMessages(tab(d, grp, col, wt = w, pct = "col", tot = c("row","col"), color = TRUE)), "kish col%")
options(tabxplor.kish_neff = FALSE)

## ---- C. tab_reg: the regime split ------------------------------------------------------------
mkr <- function(x, ...) suppressMessages(suppressWarnings(
  tab_reg(x, dependent = "y", predictors = c("grp","x2"), family = "binomial",
          empirical = TRUE, stars = TRUE, ...)))
options(tabxplor.kish_neff = FALSE); r1 <- mkr(d, wt = "w")
options(tabxplor.kish_neff = TRUE);  r2 <- mkr(d, wt = "w")
options(tabxplor.kish_neff = FALSE); r3 <- mkr(des)
cat("kish moves :", paste(diffs(snap(r1), snap(r2)), collapse = ", "), "\n")
cat("design moves:", paste(diffs(snap(r1), snap(r3)), collapse = ", "), "\n")
se <- function(tb, i) (log(get_ci_sup(tb)[i]) - log(get_ci_inf(tb)[i])) / (2*z)
i1 <- which(as.character(r1$var)=="grp" & as.character(r1$levels)=="B")
i3 <- which(as.character(r3$var)=="grp" & as.character(r3$levels)=="B")
print(data.frame(rung = c("1","2","3"),
  Obs   = c(se(r1[["Obs_OR"]],i1),   se(r2[["Obs_OR"]],i1),   se(r3[["Obs_OR"]],i3)),
  Model = c(se(r1[["Model_OR"]],i1), se(r2[["Model_OR"]],i1), se(r3[["Model_OR"]],i3))), digits = 4)
cat("oracle SE(log OR grpB): glm", summary(glm(y~grp, d, family=binomial()))$coef["grpB","Std. Error"],
    "| svyglm(flat)", summary(suppressWarnings(svyglm(y~grp, flat, family=quasibinomial())))$coef["grpB","Std. Error"],
    "| svyglm(design)", summary(suppressWarnings(svyglm(y~grp, des, family=quasibinomial())))$coef["grpB","Std. Error"], "\n")
# W1: two rungs in ONE column (factor rows vs the numeric row)
rn <- suppressMessages(suppressWarnings(tab_reg(d, dependent="y", predictors=c("grp","num"),
        family="binomial", wt="w", empirical=TRUE, multiplier=1, stars=TRUE)))
print(data.frame(var=as.character(rn$var), lev=as.character(rn$levels),
                 se=(log(get_ci_sup(rn[["Obs_OR"]]))-log(get_ci_inf(rn[["Obs_OR"]])))/(2*z)), digits=4)
cat("oracle numeric: glm", summary(glm(y~num, d, family=binomial()))$coef["num","Std. Error"],
    "| svyglm(flat)", summary(suppressWarnings(svyglm(y~num, flat, family=quasibinomial())))$coef["num","Std. Error"], "\n")

## ---- D/F. oracle + degrees of freedom --------------------------------------------------------
oracle <- function(dd, de, lab) { cat("\n##", lab, "\n")
  tp <- suppressMessages(tab(de, grp, col, pct = "row")); cy <- tp[["yes"]]
  k <- as.character(tp$grp) != "Total"; p <- get_pct(cy)[k]; ne <- get_n_eff(cy)[k]
  sb <- svyby(~col, ~grp, de, svymean); vs <- as.matrix(SE(sb))[, ncol(as.matrix(SE(sb)))]^2
  cat("prop Var ratio:", round((p*(1-p)/ne)/vs, 6), "\n")
  tm <- suppressMessages(tab(de, grp, num, color = TRUE, stars = TRUE)); cn <- tm[["num"]]
  k2 <- as.character(tm$grp) != "Total"
  sm <- svyby(~num, ~grp, de, svymean)
  cat("mean Var ratio:", round((get_var(cn)[k2]/get_n_eff(cn)[k2])/as.numeric(SE(sm))^2, 6), "\n") }
oracle(d, flat, "weights only"); oracle(d, des, "stratified + clustered")
cal <- calibrate(des, ~ num, c(`(Intercept)` = nrow(d), num = sum(d$num)))
oracle(d, cal, "CALIBRATED")
# tab_vars domains
tb <- as.data.frame(suppressMessages(tab(des, grp, col, z, pct = "row"))); tb$key <- paste(tb$z, tb$grp)
sb <- svyby(~col, ~z+grp, des, svymean); sb$key <- paste(sb$z, sb$grp); m <- match(sb$key, tb$key)
cy <- suppressMessages(tab(des, grp, col, z, pct = "row"))[["yes"]]
cat("tab_vars Var ratio:", round((get_pct(cy)[m]*(1-get_pct(cy)[m])/get_n_eff(cy)[m]) /
      as.matrix(SE(sb))[, ncol(as.matrix(SE(sb)))]^2, 6), "\n")
for (P in c(5, 10, 20, 50)) { dd <- make_survey(7, S = 2, P = P, M = 60)
  de <- svydesign(~psu, strata = ~strat, weights = ~w, data = dd, nest = TRUE); DF <- degf(de)
  ne <- get_n_eff(suppressMessages(tab(de, grp, num, color = TRUE, stars = TRUE))[["num"]])[1]
  cat(sprintf("%3d PSUs degf %3d n_eff %6.1f | t(n_eff-1)/t(degf) %+.1f%% | z/t(degf) %+.1f%%\n",
      nlevels(dd$psu), DF, ne, 100*(qt(.975,ne-1)/qt(.975,DF)-1), 100*(qnorm(.975)/qt(.975,DF)-1))) }

## ---- E. coverage sweep, footers, contrib, tab_counts, Fisher ---------------------------------
hn <- function(tb, what) { fm <- names(tb)[vapply(tb, is_fmt, logical(1))]
  ok <- vapply(fm, function(nm) any(is.finite(get_n_eff(tb[[nm]]))), logical(1))
  cat(sprintf("%-30s %d/%d: %s\n", what, sum(ok), length(ok), paste(fm[ok], collapse = ","))) }
hn(suppressMessages(tab(des, grp, col, pct = "row")), "pct=row")
hn(suppressMessages(tab(des, grp, col, pct = "col")), "pct=col")
hn(suppressMessages(tab(des, grp, col, pct = "all")), "pct=all")
hn(suppressMessages(tab(des, grp, col)),              "counts")
hn(suppressMessages(tab(des, grp, num, color = TRUE, stars = TRUE)), "mean")
hn(suppressMessages(tab(des, grp, c(col, num), pct = "row", color = TRUE, stars = TRUE)),
                                                                     "mixed col_vars")
hn(suppressMessages(tab(des, grp, col, z, pct = "row")),             "tab_vars")
hn(suppressMessages(tab(des, grp, col, z, pct = "row", comp = "all")), "comp=all")
hn(suppressMessages(tab(des, c(grp, x2), col, pct = "row")),         "2 row_vars")
hn(suppressMessages(tab_plain(des, grp, col, pct = "row", tot = "col")), "tab_plain")
hn(suppressMessages(tab_num(des, grp, num, color = TRUE, stars = TRUE)), "tab_num")
d$ord <- factor(sample(c("low","mid","high"), nrow(d), TRUE),
                levels = c("low","mid","high"), ordered = TRUE)
hn(suppressMessages(tab(svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE),
     grp, ord, pct = "row", OR = "cumOR", color = "OR", stars = TRUE)), "OR = cumOR")
# NOTE: a bare tab(des, grp, num) shows 0/1 -- with no colour and no stars it computes no interval
# at all, so there is no base to write. Absence of `n_eff` there is correct, not a gap.
foot <- function(tb) render_footer(tab_footer_streams(tb, "text"), "text")
r <- rung(function(x) suppressMessages(tab(x, grp, col, wt = w, pct = "row",
                                           color = TRUE, test = TRUE)))
cat("\nrung1:", foot(r$r1), "\nrung2:", foot(r$r2), "\nrung3:", foot(r$r3), "\n")
ctr <- function(tb) paste(signif(get_pvalue(tb[["yes"]]), 3), collapse = " ")
options(tabxplor.kish_neff = FALSE)
cat("contrib counts wt :", ctr(suppressMessages(tab(d, grp, col, wt = w, color = "contrib",
      color_signif = "grey_non_signif"))), "\n")
cat("contrib counts des:", ctr(suppressMessages(tab(des, grp, col, color = "contrib",
      color_signif = "grey_non_signif"))), "\n")
cat("contrib row%   wt :", ctr(suppressMessages(tab(d, grp, col, wt = w, pct = "row",
      color = "contrib", color_signif = "grey_non_signif"))), "\n")
cat("contrib row%   des:", ctr(suppressMessages(tab(des, grp, col, pct = "row",
      color = "contrib", color_signif = "grey_non_signif"))), "\n")
agg <- as.data.frame(dplyr::count(d, grp, col, wt = w, name = "wn"))
agg$n <- as.data.frame(dplyr::count(d, grp, col))$n
options(tabxplor.kish_neff = TRUE)
hn(suppressMessages(tab_counts(agg, grp, col, counts = n, wt_counts = wn, pct = "row")),
   "tab_counts + kish")
options(tabxplor.kish_neff = FALSE)
small <- d[d$psu %in% levels(d$psu)[1:2], ]
print(as.data.frame(get_test(suppressMessages(tab(small, grp, col, test = TRUE))))[
  , c("test","pvalue","effect_size","pvalue_exact")])
print(as.data.frame(get_test(suppressMessages(tab(small, grp, col, wt = w, test = TRUE))))[
  , c("test","pvalue","effect_size","pvalue_exact")])

## ---- H. the exported step path: the two leaves disagree --------------------------------------
half <- function(tb, nm) round((get_ci_sup(tb[[nm]]) - get_ci_inf(tb[[nm]]))/2, 4)
p1 <- suppressMessages(tab_ci(tab_plain(des, grp, col, pct = "row", tot = "col"), "cell"))
p2 <- suppressMessages(tab(des, grp, col, pct = "row", ci = "cell"))
cat("\ntab_plain+tab_ci:", half(p1, "yes"), "\ntab(ci=cell)    :", half(p2, "yes"), "\n")
n1 <- suppressMessages(tab_ci(tab_num(des, grp, num, tot = "row"), "cell"))
n2 <- suppressMessages(tab(des, grp, num, ci = "cell"))
cat("tab_num+tab_ci  :", half(n1, "num"), "\ntab(ci=cell)    :", half(n2, "num"),
    "\nn_eff step:", get_n_eff(n1[["num"]]), "| tab():", round(get_n_eff(n2[["num"]]), 1), "\n")

## ---- G. the forced degrade: does the footer still claim the design? --------------------------
ns <- asNamespace("tabxplor"); unlockBinding("svy_var_prop", ns)
assign("svy_var_prop", function(...) NULL, envir = ns)
tb <- suppressMessages(tab(des, grp, col, pct = "row", color = TRUE, stars = TRUE))
cat("n_eff:", get_n_eff(tb[["yes"]]), "\nhalf-width:",
    round((get_ci_sup(tb[["yes"]]) - get_ci_inf(tb[["yes"]]))/2, 4), "\nfooter:", foot(tb), "\n")
```

---

## 9. The maintainer's proposal — replace Kish by a minimal-design `n_eff`

*Added 2026-08-11 after the findings above, in answer to: "the only way to do weighted regression
models is a minimal survey design (weights only, no cluster, no strata); that same minimal design
gives univariable SEs very close to Kish's. Could we remove the Kish implementation altogether and
replace it by a **minimal-design** `n_eff` — the machinery added today — used by `tab()` and by
`tab_reg(empirical = TRUE)`, with a real design object still giving the full clusters/strata/
calibration answer?"*

### 9.1 The short answer

**The diagnosis is exactly right, the proposal is statistically strictly better than Kish, and it can
be implemented at Kish's cost — cheaper than the machinery it would reuse.** Three results, in
increasing order of consequence:

1. **It is exact where Kish is an approximation.** Kish's `n_eff = (Σw)²/Σw²` is the design effect of
   a mean *whose variable is uncorrelated with the weights*. When the outcome follows the weight —
   the normal case, since weights correct for differential nonresponse — Kish is measured **up to
   17 % wrong**, in either direction. The minimal-design `n_eff` is exact by construction (§9.2).
2. **It makes `empirical = TRUE` match the univariable minimal-design model *exactly*, not
   approximately** — ratio `1.000` on the crude odds ratio and on the crude difference — because at
   `ids = ~1` the design covariance between two disjoint rows is **exactly zero**, so ruling Q3's
   conservatism (the one residue rung 3 keeps) vanishes at rung 2 (§9.3).
3. **It does not need the influence-function machinery at all.** Implemented as "build a flat design
   and take Route A's path" it is **15× to 80× slower** than Kish (§9.4) — which would be
   disqualifying. But at `ids = ~1` the design variance has a **closed form in the per-cell weighted
   sums the aggregate core already computes** (§9.5), verified exact to every digit against `survey`
   for row / column / grand-total percentages *and* for means. That form is `O(cells)`, not
   `O(rows × n)`; it needs no `svyrecvar`, no influence matrix, no size ceiling, and — unlike Route A
   — it does **not** force the raw microdata scan, so it composes with the aggregate-core invariant.

**So the recommendation is: adopt the proposal, but implement it as a closed form on `Σw²`, not as a
synthesized design object.** That turns what looked like a trade (exactness for speed) into a strict
improvement, and it deletes more code than it adds (§9.6). Four caveats survive and one of them is a
hard prerequisite (§9.7).

### 9.2 Reliability — Kish is exact only under its own assumption

Same 6000-row weighted sample, weights driven by a design stratum, outcome driven by the weight with
strength `rho`. SE relative to `survey`'s own answer on the identical flat design *(block M1)*:

| | `n_eff` Kish | `n_eff` min-design | SE(Kish)/truth | SE(min-design)/truth |
|---|---|---|---|---|
| **rho = 0** (outcome ⟂ weight — Kish's own assumption) | 933.7 … 1012.1 | 938.6 … 1016.4 | 1.000 – 1.003 | **1.000** |
| **rho = 1** (differential nonresponse) | 933.7 … 1012.1 | 928.9 … **1184.1** | 0.998 – **1.082** | **1.000** |
| **rho = 2.5** (strong) | 933.7 … 1012.1 | 1044.4 … **1355.5** | 1.058 – **1.168** | **1.000** |

Means behave the same (Kish 1.024–1.058, min-design 1.000 throughout).

Two things to read off this. Kish's `n_eff` **cannot move with the outcome** — it is a property of
the weights alone, so the same four numbers appear in all three rows. And the direction of its error
is not fixed: here it is *conservative* (too few effective cases, intervals too wide), which is the
mirror image of §3.3 of `dev/full_survey_design_scope.md` where Kish was blind to a precision *gain*.
Both are the same blindness.

### 9.3 Does it make `empirical = TRUE` match the univariable model? Yes — exactly

SE(log OR) of the crude effect, against a univariable `svyglm` on the *same* flat design *(block M2)*:

```
        truth (univ. flat svyglm)   rung 1           Kish             MIN-DESIGN
grpB           0.09118              0.07401 x0.812   0.09191 x1.008   0.09118  x1.000
grpC           0.09035              0.07469 x0.827   0.09325 x1.032   0.09035  x1.000
grpD           0.09086              0.07609 x0.837   0.09436 x1.039   0.09086  x1.000
```

and the crude **difference** bracket against `svycontrast` on the two domain means *(block M2b)*:

```
grpB  0.02264 vs 0.02264   grpC  0.02167 vs 0.02167   grpD  0.02121 vs 0.02121   (ratio 1.0000)
```

**Why it is an identity and not a coincidence.** Under `ids = ~1` every observation is its own
primary sampling unit, so two disjoint row domains share no cluster and their estimated proportions
are **independent**. The covariance term that Route A discards (ruling Q3, `§4.5` of the z14 study —
worth 1.4 % on the crude OR at rung 3, measured `0.13863` vs `0.14055`) is *exactly zero* here. The
Woolf bracket built on effective counts is then precisely the delta-method SE of the log odds ratio,
and Newcombe on two independent effective bases is precisely the difference SE.

So the proposal buys something rung 3 does **not** have: at the minimal design, the crude column and
the univariable model are the same number, not merely the same order.

### 9.4 Cost, if implemented the obvious way — disqualifying

Implemented as "synthesize `svydesign(ids = ~1, weights = ~w)` and take Route A's path" *(block M3b,
median of 5)*:

| table | rung 1 | Kish | min-design via Route A |
|---|---|---|---|
| 4×2, n = 5 000 | 0.090 s | 0.084 s | 0.121 s (×1.4) |
| 4×2, n = 50 000 | 0.083 s | 0.082 s | 0.352 s (×4.3) |
| 4×2, n = 200 000 | 0.082 s | 0.083 s | **1.171 s (×14.1)** |
| 50 rows × 30 000 obs | — | 0.056 s | 0.876 s (×15.6) |
| **300 rows × 30 000 obs** | — | 0.077 s | **6.151 s (×80)** |

(median of 5; wall-clock varies ±20 % between runs, the ratios do not)

Kish is free (×0.7–1.2 = measurement noise): it is one extra `Σw²` column in the data.table scan.
Route A is `O(rows × n)` — one influence-matrix column per wide row, one `svyrecvar` per column level
— and it carries the `5e7` guard, which at n = 30 000 trips above 1 667 wide rows and at n = 60 000
above 833, degrading **silently** (W4). Making rung 2 depend on that would put a live jamovi table,
a `tab_many()` over a fine geography, or any 200 000-row file on the wrong side of it.

**This is the reason not to implement the proposal literally.** It is not a reason to reject it.

### 9.5 The closed form — the decisive finding

At `ids = ~1`, with no strata, no `fpc` and no calibration, `svyrecvar` reduces to a plain sum of
squares of the influence contributions, and every quantity `tab()` displays is a ratio of two weighted
sums. Writing `A` for the cell's `Σw²`, `S` for `Σw²` over the **base's own domain** and `B` for `Σw`
over that same domain (i.e. exactly `leaf_wide_pct()`'s `Dmat` selector, applied to `Σw²` instead of
`Σw`):

```
Var_design(p̂) = n/(n−1) · [ A·(1−p)² + (S − A)·p² ] / B²
Var_design(x̄) = n/(n−1) · [ Σw²x² − 2·x̄·Σw²x + x̄²·Σw² ] / B²
```

Verified against `survey` *(block M5b)* — ratio printed to 10 significant digits:

| quantity | ratio to `survey` |
|---|---|
| `pct = "row"` (4 cells) | `1 1 1 1` |
| `pct = "col"` (4 cells) | `1 1 1 1` |
| `pct = "all"` (4 cells) | `1 1 1 1` |
| mean (4 cells) | `1 1 1 1` |

and Kish on the identical cells lands at `0.988 / 1.061 / 1.138 / 1.187`.

**One formula, three bases, plus the mean — and the same `Dmat` selector the leaf already applies.**
The four inputs are all present or one scan away:

| input | where it already is |
|---|---|
| `A` — per-cell `Σw²` | `tabs_w2`, accumulated today for Kish (`plain_core`, `has_w2`) |
| `S` — `Σw²` over the base's domain | the same `Dmat` broadcast `leaf_wide_pct()` already does for `Σw` (its `"col"` rollup of `tabs_w2` exists) |
| `B` — `Σw` over the base's domain | `tabs_wn` |
| `n` | the leaf's own row count |

For means, `num_moment_scan()` accumulates `Σw`, `Σwx`, `Σwx²` and (on Kish) `Σw²`; it needs two more
in the same pass, `Σw²x` and `Σw²x²`.

**Consequences beyond speed.** The closed form is a **per-cell sufficient statistic**, so unlike
Route A it does not force `use_raw`: it composes with the `.fine` seam (`Σw²` is additive across a
partition, exactly as the current `"col"` rollup already relies on), keeps the jamovi tier-1/2
aggregate cache usable, and has no size ceiling to degrade past. It is also the *only* part of
`dev/full_survey_design_scope.md`'s Route C ("a PSU-augmented aggregate", rejected §4.8) that is
safe to own — because at `ids = ~1` there is no `svyrecvar` to re-implement, no lonely-PSU policy, no
multistage `fpc` and no calibration. **The rejection of Route C stands for rung 3 and does not apply
here**, and that boundary must be stated in the code, or someone will later try to extend the closed
form to a real design.

### 9.6 What it deletes

Rung 2 stops being its own implementation and becomes *the same formula as rung 3, with the design
you did not have to write*. Removable:

* the Kish arm of `leaf_wide_pct(tabs_w2 = )` (the `Σw²` accumulation itself **stays** — it becomes
  the closed form's input, so the scan does not change);
* `num_core()`'s Kish `_en` branch, and `num_moment_scan()`'s direct `getOption()` read (W12.2);
* `svy_omnibus_one()`'s entire `mode == "kish"` block — about 35 lines containing a hand-rolled
  first-order Rao-Scott chi-square **and** a hand-rolled per-group weighted ANOVA (see the caveat in
  §9.7.4 — this one is a replacement, not a straight deletion);
* `reg_empirical()`'s `neff_or_n()` Kish arm (R/tab_reg.R:1622-1626);
* one of the three inference rungs from every doc, vignette table and jamovi label.

`svy_inference_mode()` then answers a genuinely binary question — *is there a design (given or
implied)?* — and the option decides only whether a **missing** design is implied. That is one concept
where there are currently two, which is what makes the vignette's three-row table collapse to two.

### 9.7 Caveats — four, one of them blocking

**9.7.1 (blocking) `color = "contrib"` on a percentage table would REGRESS.** Measured *(block M4b)*,
cell p-values on the same weighted data:

```
row %,  rung 1 (raw n)  : 2.09e-21  0.000226  5.96e-06  4.67e-17
row %,  Kish            : 1.70e-14  0.00290   0.000256  1.21e-11     <- corrected
row %,  min-design      : 2.09e-21  0.000226  5.96e-06  4.67e-17     <- identical to raw n
counts, min-design      : 2.17e-12  0.00428   0.000612  7.94e-11     <- corrected
```

W3 explained why: the contrib residual reads the **Total column's** `n_eff`, which is degenerate
under any design-based rule (`p = 1`) but perfectly well defined under Kish. So removing Kish would
strip the correction from every percentage table's contrib colouring, on the exact measure
`color = TRUE` picks automatically for counts. **P4 becomes a prerequisite, not an optional extra.**
The good news: with the closed form P4 is three lines, not a second variance pass — the `"all"`-base
variance is the same formula with `S` and `B` taken over the subtable.

**9.7.2 The correction stops being monotone.** Today `n_eff ≤ n` by construction, so
`kish_neff = TRUE` can only widen an interval — which is what `test-kish-descriptive.R` asserts and
what `?tab` and both vignettes promise ("intervals widen honestly"). The exact `n_eff` can be
substantially **larger** than the Kish one (measured 1355 vs 994, +36 %), so turning the option on may
make an interval *narrower*. That is correct behaviour — Kish was over-correcting — but it is a
documentation and test change, and it removes a reassuring simplification from the user's mental
model.

**9.7.3 Cells at 0 % or 100 % lose their base.** `n_eff = p(1−p)/Var` is `0/0` there, so the interval
falls back to the raw `n` *(block M5)*: a 0 % cell shows `n_eff` `NA` under the min-design where Kish
gave 17.5. This is a property of Korn & Graubard's device, so **rung 3 already has it**; the proposal
merely extends it to rung 2. It bites precisely on rare-outcome cells, where the base matters most.
Mitigation options for the implementation phase: keep the Kish value as the fallback *at those cells
only* (cheap — `Σw²` is still there, so this costs nothing and needs no second code path), or accept
the raw `n` and say so.

**9.7.4 The omnibus test needs a decision.** The *cells* have a closed form; the Rao-Scott
**second-order** correction (`svychisq`) does not — it needs the full cell covariance matrix. Three
options, in increasing cost: (a) keep a first-order rescale but built on the **exact per-cell design
effects** the closed form now gives (`X² / d̄`, `d̄` the mean cell deff) — strictly better than today's
single Kish deff, still `O(cells)`; (b) call `survey::svychisq()` on a synthesized flat design —
exact, but it builds its own influence matrix, so §9.4's cost returns for wide tables; (c) leave the
omnibus test on Kish while the intervals move — rejected, that re-opens the very
"test and cells describe different samples" seam z14-ii closed. *Recommendation: (a).* For reference,
on the test table `chi2_kish` gives `p = 1.6e-20` (statistic 95.3, n rescaled to 3912) and `svychisq`
on the flat design `p = 4.2e-22` (F = 34.4) *(block M6)* — the same conclusion, so this is a precision
choice, not a substantive one.

**9.7.5 (minor) Two user-visible surfaces move.** The `test` attribute's discriminators `chi2_kish` /
`F_kish` are replaced (they are 2 of the 4 robust values, and `test_pvalue_descriptor()` renders
them), and every weighted golden with the option on regenerates. `tab_counts()` is unchanged — it
still has no per-cell `Σw²` — though the closed form does open a door the influence-function route
closed: a pre-aggregated table *could* climb if a `w2_counts =` column were ever supplied, since the
formula needs only cell-level sums. Not a proposal, an observation.

**9.7.6 What it still cannot carry.** Clustering, stratification, `fpc` and calibration. Those remain
rung 3 and still need `R/survey-variance.R`. The closed form is specifically the `ids = ~1` case and
must be scoped as such in the code, or it will be mistaken for a general variance engine.

### 9.8 How this changes the report's proposals

| was | becomes |
|---|---|
| **P3** (route the crude interval through the gap test's influence SE) | **superseded for the factor rows**: the closed form gives the same number more cheaply, and — being exact at `ids = ~1` — it makes the crude column match the univariable model without touching `reg-influence.R`. P3 survives only as the answer for **numeric and ordinal** predictors, which have no closed form (their crude twin is a fit); i.e. it shrinks to "make the factor rows join the rung the numeric rows are already on", which the closed form does. **W1 and W2 close together.** |
| **Q2** (should Kish be the default?) | becomes *"should the exact minimal-design correction be the default when `wt` is given?"* — a stronger case, since the answer is no longer an approximation, and it is what `tab_reg()`'s model column has always done. The cost objection disappears with §9.5. |
| **P4** (`contrib` on percentage tables) | promoted from "nice to have" to **prerequisite** (§9.7.1), and simplified to three lines. |
| **P1** (store the rung) | unchanged and still first: with rung 2 becoming design-based, the footer must distinguish *"weights only"* from *"weights, clusters and strata"*, and §9.7.3's per-cell fallbacks need somewhere to be recorded. |
| **P5** (design degrees of freedom) | narrows to rung 3: at `ids = ~1`, `degf = n − 1`, so W7 does not arise at rung 2. |
| **new P9** | the closed form itself: `svy_flat_var_prop()` / `svy_flat_var_mean()` beside `R/survey-variance.R`, with the `ids = ~1`-only scope written into the header, consumed by both leaves and by `reg_empirical()`. |

**Sequencing, revised.** z15-i (P1, P2, P3b, P6, P7, P8) is unchanged and still goes first — it is
what makes the rest honest. z15-ii becomes **P9 + P4 together** (they cannot ship apart, §9.7.1),
with Q2 deciding the default. z15-iii keeps P5 and the residual P3 (numeric/ordinal crude rows).

### 9.9 Reproducer for §9

Append to Appendix A.

```r
## ---- M1. Kish vs minimal-design n_eff, against survey's own answer ---------------------------
mk <- function(seed, n = 6000, rho = 1) { set.seed(seed)
  s <- sample(1:6, n, TRUE); w <- c(.4,.6,1,1.3,2,3)[s]*exp(rnorm(n,0,.3)); w <- w/mean(w)
  grp <- factor(sample(c("A","B","C","D"), n, TRUE))
  lin <- rho*(log(w)*1.2) + c(A=-.4,B=0,C=.3,D=.6)[as.character(grp)]
  y <- rbinom(n, 1, plogis(lin))
  data.frame(grp, x2 = factor(sample(c("lo","hi"), n, TRUE)),
             col = factor(ifelse(y==1,"yes","no")), y,
             num = round(rnorm(n,50,12) + rho*15*log(w)), w) }
cmp <- function(d, lab) { fl <- svydesign(~1, weights = ~w, data = d)
  options(tabxplor.kish_neff = TRUE)
  tk <- suppressMessages(tab(d, grp, col, wt = w, pct = "row", color = TRUE, stars = TRUE))
  options(tabxplor.kish_neff = FALSE)
  tf <- suppressMessages(tab(fl, grp, col, pct = "row", color = TRUE, stars = TRUE))
  k <- as.character(tk$grp) != "Total"; p <- get_pct(tk[["yes"]])[k]
  sb <- svyby(~col, ~grp, fl, svymean); vt <- as.matrix(SE(sb))[, ncol(as.matrix(SE(sb)))]^2
  cat("\n--", lab, "\n"); print(data.frame(row = as.character(tk$grp)[k],
    kish = round(get_n_eff(tk[["yes"]])[k],1), min_design = round(get_n_eff(tf[["yes"]])[k],1),
    SE_kish_truth = round(sqrt((p*(1-p)/get_n_eff(tk[["yes"]])[k])/vt),4),
    SE_min_truth  = round(sqrt((p*(1-p)/get_n_eff(tf[["yes"]])[k])/vt),4)), row.names = FALSE) }
cmp(mk(1, rho = 0), "rho=0 outcome INDEPENDENT of weight"); cmp(mk(1, rho = 1), "rho=1")
cmp(mk(1, rho = 2.5), "rho=2.5 strong")

## ---- M2/M2b. does the crude match the univariable minimal-design model? ----------------------
d <- mk(1, rho = 1); fl <- svydesign(~1, weights = ~w, data = d)
rr <- function(dat, ...) suppressMessages(suppressWarnings(tab_reg(dat, dependent = "y",
        predictors = "grp", family = "binomial", empirical = TRUE, stars = TRUE, ...)))
options(tabxplor.kish_neff = FALSE); a1 <- rr(d, wt = "w")
options(tabxplor.kish_neff = TRUE);  a2 <- rr(d, wt = "w")
options(tabxplor.kish_neff = FALSE); a3 <- rr(fl)
u <- suppressWarnings(svyglm(y ~ grp, design = fl, family = quasibinomial()))
seo <- function(tb, i) (log(get_ci_sup(tb)[i]) - log(get_ci_inf(tb)[i]))/(2*z)
for (lv in c("B","C","D")) { i <- which(as.character(a1$levels) == lv)
  tr <- summary(u)$coef[paste0("grp",lv),"Std. Error"]
  cat(sprintf("grp%s truth %.5f | rung1 %.5f x%.3f | kish %.5f x%.3f | MIN %.5f x%.3f\n", lv, tr,
      seo(a1[["Obs_OR"]],i), seo(a1[["Obs_OR"]],i)/tr, seo(a2[["Obs_OR"]],i), seo(a2[["Obs_OR"]],i)/tr,
      seo(a3[["Obs_OR"]],i), seo(a3[["Obs_OR"]],i)/tr)) }
sb <- svyby(~col, ~grp, fl, svymean, covmat = TRUE); nm <- names(coef(sb))
for (lv in c("B","C","D")) { i <- which(as.character(a3$levels) == lv)
  cn <- setNames(rep(0, length(nm)), nm)
  cn[grepl(paste0("^",lv), nm) & grepl("yes", nm)] <- 1
  cn[grepl("^A", nm) & grepl("yes", nm)] <- -1
  h <- (get_ci_sup(a3[["Obs_%"]])[i] - get_ci_inf(a3[["Obs_%"]])[i])/2
  cat(sprintf("  diff grp%s: tabxplor %.5f | survey %.5f  ratio %.4f\n", lv, h/z,
      as.numeric(SE(svycontrast(sb, list(d = cn)))), (h/z)/as.numeric(SE(svycontrast(sb, list(d = cn)))))) }

## ---- M3b. cost of the LITERAL implementation (synthesized design + Route A) ------------------
tmf <- function(f, k = 5) round(median(replicate(k, system.time(f())[["elapsed"]])), 3)
for (n in c(5000, 50000, 200000)) { dd <- mk(2, n = n); fl <- svydesign(~1, weights=~w, data=dd)
  f1 <- function() { options(tabxplor.kish_neff=FALSE)
    suppressMessages(tab(dd, grp, col, wt=w, pct="row", color=TRUE, stars=TRUE)) }
  f2 <- function() { options(tabxplor.kish_neff=TRUE)
    suppressMessages(tab(dd, grp, col, wt=w, pct="row", color=TRUE, stars=TRUE)) }
  f3 <- function() { options(tabxplor.kish_neff=FALSE)
    suppressMessages(tab(fl, grp, col, pct="row", color=TRUE, stars=TRUE)) }
  cat(sprintf("n=%7d rung1 %.3f | kish %.3f | MIN %.3f\n", n, tmf(f1), tmf(f2), tmf(f3))) }
options(tabxplor.kish_neff = FALSE)
for (G in c(50, 300)) { dd <- mk(3, n = 30000); dd$geo <- factor(sample(G, 30000, TRUE))
  fl <- svydesign(~1, weights = ~w, data = dd)
  f2 <- function() { options(tabxplor.kish_neff=TRUE); suppressMessages(tab(dd, geo, col, wt=w, pct="row")) }
  f3 <- function() { options(tabxplor.kish_neff=FALSE); suppressMessages(tab(fl, geo, col, pct="row")) }
  cat(sprintf("%3d rows x 30000 obs: kish %.3f | MIN %.3f\n", G, tmf(f2,3), tmf(f3,3))) }
options(tabxplor.kish_neff = FALSE)

## ---- M4b. the contrib regression -------------------------------------------------------------
d <- mk(1, rho = 1); fl <- svydesign(~1, weights = ~w, data = d)
ctr <- function(tb) paste(signif(get_pvalue(tb[["yes"]]), 3), collapse = " ")
options(tabxplor.kish_neff = FALSE)
cat("row% raw n :", ctr(suppressMessages(tab(d, grp, col, wt=w, pct="row",
      color="contrib", color_signif="grey_non_signif"))), "\n")
options(tabxplor.kish_neff = TRUE)
cat("row% kish  :", ctr(suppressMessages(tab(d, grp, col, wt=w, pct="row",
      color="contrib", color_signif="grey_non_signif"))), "\n")
options(tabxplor.kish_neff = FALSE)
cat("row% MIN   :", ctr(suppressMessages(tab(fl, grp, col, pct="row",
      color="contrib", color_signif="grey_non_signif"))), "\n")
cat("cnts MIN   :", ctr(suppressMessages(tab(fl, grp, col,
      color="contrib", color_signif="grey_non_signif"))), "\n")

## ---- M5b. THE CLOSED FORM: exact at ids = ~1, from per-cell sums only ------------------------
set.seed(4); n <- 5000
s <- sample(1:6, n, TRUE); w <- c(.4,.6,1,1.3,2,3)[s]*exp(rnorm(n,0,.3)); w <- w/mean(w)
grp <- factor(sample(c("A","B","C","D"), n, TRUE))
y <- rbinom(n, 1, plogis(1.2*log(w) + c(A=-.4,B=0,C=.3,D=.6)[as.character(grp)]))
col <- factor(ifelse(y==1,"yes","no")); x <- round(rnorm(n,50,12) + 15*log(w))
d <- data.frame(grp, col, x, w); des <- svydesign(~1, weights = ~w, data = d); f <- n/(n-1)
A  <- tapply(w^2, list(grp, col), sum)[, "yes"]          # per-CELL sum w^2
cf <- function(A, S, B, p) f*(A*(1-p)^2 + (S-A)*p^2)/B^2 # THE closed form, any base
Wr <- tapply(w, grp, sum); Tr <- tapply(w^2, grp, sum); pr <- tapply(w*(col=="yes"), grp, sum)/Wr
cat("pct=row ratio:", signif(cf(A, Tr, Wr, pr) /
  as.matrix(SE(svyby(~col, ~grp, des, svymean)))[,2]^2, 10), "\n")
Wc <- sum(w*(col=="yes")); Tc <- sum(w[col=="yes"]^2); pc <- tapply(w*(col=="yes"), grp, sum)/Wc
cat("pct=col ratio:", signif(cf(A, Tc, Wc, pc) /
  as.matrix(SE(svyby(~grp, ~col, des, svymean)))[2,]^2, 10), "\n")
Wa <- sum(w); Ta <- sum(w^2); pa <- tapply(w*(col=="yes"), grp, sum)/Wa
cat("pct=all ratio:", signif(cf(A, Ta, Wa, pa) / sapply(levels(grp), function(g)
  as.numeric(SE(svymean(~I(grp==g & col=="yes"), des)))[2]^2), 10), "\n")
S2 <- tapply(w^2, grp, sum); S2x <- tapply(w^2*x, grp, sum); S2xx <- tapply(w^2*x*x, grp, sum)
m  <- tapply(w*x, grp, sum)/Wr
cat("mean    ratio:", signif((f*(S2xx - 2*m*S2x + m^2*S2)/Wr^2) /
  as.numeric(SE(svyby(~x, ~grp, des, svymean)))^2, 10), "\n")

## ---- M6. omnibus: chi2_kish vs svychisq on the flat design -----------------------------------
d <- mk(1, rho = 1); fl <- svydesign(~1, weights = ~w, data = d)
options(tabxplor.kish_neff = TRUE)
print(as.data.frame(get_test(suppressMessages(tab(d, grp, col, wt=w, pct="row", test=TRUE)))))
options(tabxplor.kish_neff = FALSE)
print(as.data.frame(get_test(suppressMessages(tab(fl, grp, col, pct="row", test=TRUE)))))
```

---

## 8. References

* Kish, L. (1965) *Survey Sampling*. Wiley. — the effective sample size `(Σw)²/Σw²`.
* Korn, E. L. & Graubard, B. I. (1998) "Confidence intervals for proportions with small expected
  number of positive counts estimated from survey data", *Survey Methodology* 24(2), 193-201. — the
  effective-n device Route A implements.
* Binder, D. A. (1983) "On the variances of asymptotically normal estimators from complex surveys",
  *International Statistical Review* 51, 279-292. — the linearization `svyglm`/`svyrecvar` use.
* Rao, J. N. K. & Scott, A. J. (1984) "On chi-squared tests for multiway contingency tables with cell
  proportions estimated from survey data", *Annals of Statistics* 12, 46-60.
* Lumley, T. (2010) *Complex Surveys: A Guide to Analysis Using R*. Wiley. — `degf()`, `svyciprop()`,
  `svychisq()`.
* `dev/full_survey_design_scope.md` (2026-08-11) — the z14 scope study and its rulings.
* `dev/model_vs_observed_gap_test.md` §3.8 — where the gap test's variance stops holding.
