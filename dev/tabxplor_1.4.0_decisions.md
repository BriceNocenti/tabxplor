# tabxplor 1.4.0 — settled design decisions (grounded)

Detailed rationale behind the phase bullets in `CLAUDE.md` (§ 1.4.0 roadmap). CLAUDE.md holds the
**concise** decisions; **this file holds the grounding** (code `file:line` + statistics) so a fresh
session can implement without re-deriving it. Written 2026-07; all decision questions resolved 2026-07-07.

**How to read.** *Aim* (the governing rule) → *Status & open items* (what is settled, what is still open)
→ the numbered decisions **§1–§26** (each self-contained, with its own grounding) → *How the decisions cohere* (the
closing synthesis). The **§N numbers are stable cross-reference anchors** —
CLAUDE.md and this file both cite "§9", "§12", … — so **do not renumber them**; append new decisions as §27+.

## Aim (this governs every decision below)

1.4.0 = **refactor and simplify `tab()`/`tab_many()`**: strip the white-elephant flexibility that
real-world data analysis never uses, and **redesign the `tabxplor_fmt` vctrs-field architecture** (one
combined pass) to fit the simpler, faster model. Hard rule: the **public API stays retro-compatible**
(user-facing functions, their arguments, and the `tabxplor_fmt` fields users read with `$`/`mutate()`),
but the **internals may — and should — be radically redesigned** for consistency and performance —
remove legacy/dead paths, fuse them, and route everything through the one aggregate-core. Every section
below is one such simplification; none exists to add flexibility.

---

## Status & open items

**Resolved** (2026-07-07) — the four decisions that fixed the Phase 1 field set:

- **Q1 — deprecation posture** → soft-deprecate, **stay 1.4.0**; the one accepted break is numeric `$diff`
  ratio→difference (§3, §6).
- **Q2 — per-cell significance** → store a **`pvalue`** field (§1, §12).
- **Q3 — ratio field** → **rename the unused `rr` → `ratio`** (§3, §9).
- **Q4 — mean-table tests** → **ANOVA/Welch F** omnibus + per-cell **Welch t** (§12).

**Resolved** (2026-07-07, review session 2) — four consistency decisions from the roadmap review:

- **Q5 — weighted inference** → **weighted estimate + unweighted `n`** for every CI/test (one rule for
  proportions *and* means; for a 0/1 variable weighted-var + unweighted-n ≡ weighted-% + unweighted-n).
  Kish `n_eff` is a cheap opt-in. Fixes the §12 self-contradiction. → new **§14**.
- **Q6 — CI/stars duality** → the visible interval and the stars must be duals: **two-proportion score
  test ⇄ Newcombe (score) interval** when stars are on; AC stays the default when stars are off. → new
  **§15**, §1.
- **Q7 — `tab_many()` return type** → **preserve the list-default** for the soft-deprecated `tab_many`
  alias (only the unified `tab()` merges by default) — no silent break. → §6, §13.
- **Q8 — test-result placement** → whole-**table** test = table attribute; whole-**column** test = column
  attribute; per-**cell** significance = the `pvalue` field. Display: a p-value *row* for now; a future
  `!`-per-cell "weak-test" warning mode documented. → new **§16**.

**Resolved** (2026-07-07, review session 3) — closures from the consistency review:

- **Q9 — numeric diff-color scale** → **sd-standardized**: `color="diff"` on numeric columns colors
  Glass's Δ `(cell_mean − ref_mean)/sd_ref` against effect-size breaks (default `c(0.2, 0.5, 0.8, 1.2)`,
  new `mean_diff_breaks`); derived at color time from the stored `diff` + the reference `var` — no new
  field. → new **§18**.
- **Q10 — old serialized tabs** → **non-issue by usage** (tabs are exported — Excel/HTML/md — or
  re-created from their R code, never saved as `.rds`; maintainer-confirmed); no upgrade shim,
  documented unsupported. → §17.
- **Q11 — whole-table test slot** → **hard rename** of the `chi2` table attribute → **`test`**
  (constructor arg follows); `attr(x, "chi2")` → NULL is an accepted break. → §16, §17.
- **Q12 — stars vs explicit method** → the §15 AC→Newcombe switch is **default-sensitive**: it applies
  only when `method_diff` was left at its default; an explicit method is respected + one-time duality
  message. → §15.
- **G2 — chi2/F parity** → **closed**: the vectorised chi2 must match `chisq.test()` defaults
  **exactly, incl. Yates continuity correction on 2×2** — today's path calls it with defaults
  ([tab.R:5290](../R/tab.R#L5290)), so golden locks that behaviour; the planned "p equals `chisq.test`"
  parity test (CLAUDE.md Phase 3) enforces it. A `correct=` passthrough is a possible future knob, not
  1.4.0. → removed from *Still open*.

**Resolved** (2026-07-07, review session 4) — inference pins + precision closures from the deep review:

- **Q13 — omnibus F weighting** → the mean-table **Welch F follows the §14 rule** (weighted group
  means/variances + unweighted `n`), so the omnibus tests the numbers the table displays; chi2 stays
  fully unweighted (G2 parity) — a documented asymmetry, not a hidden one. → §12, §14.
- **Q14 — mean CI quantile** → mean intervals are **default-sensitive like AC→Newcombe**: today's normal
  quantile `z` when stars are off (verified: `stats::qnorm`, [tab.R:5591](../R/tab.R#L5591) — no churn),
  **Welch-t** (Welch–Satterthwaite df) when stars are on — the exact dual of the Welch-t `pvalue`. → §1, §15.
- **Q15 — per-column omnibus home** → **rows of the table-level `test` tibble keyed by col_var** (today's
  chi2 mechanism, which already stores per-column pvalue/df/statistic rows), NOT a new fmt column
  attribute — the 8-attribute contract of §9 holds. → §16.
- **Q16 — empirical-OR level reference** → **keep `ref2 = "first"`** (the maintainer's data convention
  puts the positive level first, e.g. "Oui"); glm-convention alignment is decided at tab_logit
  integration. → new **§19**.
- Plus five precision closures: the score test is **uncorrected** (Newcombe-10 dual — never `prop.test()`'s
  Yates default) → §15; **G1 drops the unweighted moment-sums** (superfluous under §14) → G1; **D3** — the
  Phase 2→5 interim color routing (numeric color keeps reading `ratio` until Phase 5) → § *Phasing*; the
  §10 `[min;max]` range is a **table-level display pre-pass** (`format()` is per-column) → §10;
  `totrow=FALSE` stays **cosmetic** during deprecation, mirroring `totcol` → §6.

**Resolved** (2026-07-09, Phase 7b) — argument↔computation map + cascade consolidation:

- **Argument-overwrite cascade → ONE pure resolver** `tab_resolve_settings()` (`R/tab-resolve.R`),
  shared by `tab_build()` + `tab_counts()`; the numeric `color="auto"` arm is `resolve_color_auto_num()`.
  Byte-identical (full suite green, no golden regen). It is a data-free function of (args, column
  classes) — the boundary the Jamovi `.js` mirrors and the Phase 7c cache keys on. Data-dependent
  resolution (`ref="auto"`/regex, `levels="auto"`, `na`-drop, leaf tot/totaltab) stays at the leaf.
  All cascade rules kept + consolidated, none discarded. → new `dev/tabxplor_argument_computation_map.md`.
- **col%+means reference mismatch** → confirmed **INTENDED behaviour**, not a bug: a mean's reference is
  a row, a factor's under `pct="col"` a column; no clean fix without white-elephant UI. Warn-only,
  documented. → §7 (Phase 7b clarification).
- **Audit vs `new_colors_UI.md` §12** → code is ahead of the doc: `get_ref_var()` already exists and the
  pct `ratio` field is already repointed (`mean=NA` for pct). Both stale doc lines corrected.

**Still open** — the only unsettled points; each names the phase that must close it.

### G1 (confirm in Phase 2) — the aggregate carries sufficient statistics, not counts

A **count-only** aggregate (`n`, `wn`) cannot yield a mean/variance/CI/t — the verified `weighted.var`
double-scan ([tab.R:5571](../R/tab.R#L5571)) is the symptom. The core must be a **sufficient-statistics
aggregate**, and it is **heterogeneous** — two branches, so every core transform (pct/diff/OR/CI/chi2/tests)
dispatches on branch (the "one vectorised impl each" is really one factor path + one numeric path each):
- **numeric col_var**, per `tab_vars × row_var × col_var` group: `n`, `Σwt`, `Σwt²`, `Σ(wt·x)`, `Σ(wt·x²)`,
  so mean `= Σwx/Σw` and weighted variance come in **one pass**. Unweighted moment-sums `Σx`/`Σx²` are
  **not** carried (trimmed, review session 4): §14 uses weighted dispersion everywhere, and with no `wt`
  the weighted sums *are* the unweighted ones — an unweighted-dispersion mode has no use case.
- **factor col_var**, per `… × col_var-cell`: `n`, `wn` (= `Σwt`), and `Σwt²`.

`Σwt²` is the **one extra sum** (additive, rolls up to the base like `wn`, cheap) that unlocks Kish
`n_eff = (Σwt)²/Σwt²` for the weighted-inference rule (§14) and design-effect reporting. Confirm the exact
schema when Phase 2 builds the core — near-certain (correctness + the headline perf win + §14 all depend
on it).

### C2 (Phase 1/3) — `tot_wn = wn/pct` recovery on empty cells

The `get_tot_wn()` accessor (§11) can't recover the weighted base of an empty cell (`pct==0`); the sibling
fallback is robust within a non-empty row but fails for a fully-empty row/group. Default lean: keep the
sibling fallback (stay at 18 fields); store `tot_wn` only if it bites in practice. Recovery is likewise
undefined on **count-only tables** (`pct="no"` never writes `pct`): there `get_tot_wn()` falls back to the
total row/col — guaranteed present once `totrow` is deprecated-on (§6). Count-only nuance (review
session 4): `tot_wn`/`get_tot_wn()` is a *proportion-cell* concept — a `pct="no"` table displays no base,
so the fallback only matters for post-hoc `tab_pct()` on a built count table; with the (soft-deprecated,
cosmetic) `totcol="no"` that path already fails today — document it, don't engineer for it.

### U4 (Phase 6) — the col%-reference (`refcol`) side — CLOSED (6d/6h, 2026-07-09)

§4 fixed the **row** `ref` as a per-row_var named vector. On the col% side: because the row_var axis is
globalised and `tab()`'s `pct` is a single scalar, the col% regime is table-wide, so `refcol` /
`diff_index(…, pct="col")` read one scalar `pct` (no per-row_var divergence to reconcile). A multi-element
`ref` under col% collapses to the single column reference with a one-time message (6d). The `fmt()`
`refcol`-cast bug was already fixed in Phase 1a. No further refcol change was needed.

### Phasing — split the Phase 1 field pass into 1a / 1b

The field pass (§9) precedes the Phase 2/3 core rewrite, so populating the new fields from the *old*
step-chain means throwaway glue. Prefer **1a** (contract: field defs + accessors + arithmetic/cast/format),
then **1b** (writers), folding 1b into Phases 2/3 where the core actually computes the fields. 1a keeps the
new fields **NA-defaulted** (not printed → golden unchanged); regenerate golden only after 1b writes them.

Two 1a refinements (review session 3): **(a)** dropping `ci` cannot wait for 1b — the untouched step-chain
writers/readers (`tab_ci`'s `set_ci`; display [fmt_class.R:1443](../R/fmt_class.R#L1443); color
[fmt_class.R:2012](../R/fmt_class.R#L2012)) still run until Phase 3, so 1a ships a **bounds-shim**:
`set_ci(x, v)` writes symmetric bounds `est ∓ v` / `est ± v` and `get_ci()` returns the upper arm —
today's output is reproduced byte-for-byte until Phase 3 writes real asymmetric bounds. **(b)** "golden
unchanged" in 1a means the **`_snaps/` display snapshots**: the RDS half of `test-golden.R` compares
structures, so adding NA-defaulted fields breaks object identity with nothing printed — regenerate the RDS
fixtures once at 1a (review: structure-only diff) and hold `_snaps/` byte-identical. The true 1a invariant
is *display byte-identity*, not zero fixture churn. `test-fmt-contract.R` gets its one deliberate 15→18
rewrite here too.

Third refinement (review session 4) — **D3, the Phase 2→5 color interim**: Phase 2's writers flip numeric
`diff` to a real difference (field + display — the §17 golden change), but the sd-standardized color scale
(§18) only lands in Phase 5. In between, the color layer must **keep reading the `ratio` field for numeric
columns** (byte-identical to today's ratio-coloring against `mean_breaks`) — never color a raw difference
against ratio-shaped breaks. Phase 5 then swaps the color source with the mode split. Silver lining: once
`diff` is a real difference, the mean `diff_ci`/`after_ci` color formulas (diff vs its own CI — same units)
become dimensionally *correct*; the suspected mean-diff_ci wrongness in the Phase 5 "to verify" list is the
old ratio-in-`diff` overload at work.

### D2 (Phase 2 vs Phase 6) — split §5 into *internal* globalisation and *argument-surface* deprecation — CLOSED (6c, 2026-07-09)

Resolution: the arg-surface change landed in Phase 6c — `tab()` asserts `OR/ci/chi2` (and already
`comp/pct/ref2/na`) scalar, so the row_var axis is globalised at the user surface. The **internal**
collapse was deliberately NOT forced: the engine `tab_build()` still recycles the row axis (a harmless
broadcast of one shared value), which also keeps `tab_many()`'s legacy per-row_var vectors working. Since
scalar input already produces uniform per-row_var behaviour, this is byte-identical; the physical removal
of the per-row_var threading is a future cosmetic cleanup, not a correctness item.

### S3 (Phase 6) — `tab()`'s NA / population-consistency semantics must migrate into the core — CLOSED (6g, 2026-07-09)

Resolution: expressed as the `na = "common_base"` boundary rule (§4 above / CLAUDE.md Phase 6g). `tab()`
maps it to a global drop of `{row_vars, first col_var, tab_vars}` + effective `na = "keep"` (so secondary
col_vars keep their NA level within the fixed population). Equals `na = "drop"` for one col_var (the old
`tab()` behaviour — S3 acceptance test). Microdata-only: `tab_counts()` rejects it (pre-aggregated counts
cannot reconstruct who was missing).

### S4 (Phase 6/7) — fate of `tab_spread()` / `tab_compact()` under `output_list` — CLOSED (6b/6f/6i, 2026-07-09)

Resolution: `tab_compact()` is kept (stays exported) as the **internal merge** invoked by
`output = "single"`; its `tabxplor.compact` option read was removed. `tab_spread()` is **kept active**
(maintainer's choice) and re-wired: `tab()` gains a `spread_vars` (+ `names_prefix` / `names_sort`)
argument that applies `tab_spread()` at the end.

---

## Decision map

- **Type system & fields**: §1 (CI as bounds), §2 (`tot_n` base), §3 (diff vs ratio; `rr`→`ratio`),
  §9 (the 18-field list + `/vctrs-field` touch-list), §11 (is `tot_n`/`tot_wn` enough?), §12 (`pvalue`).
- **References & axes**: §4 (`ref` as a per-row_var named vector), §5 (row_var-axis globalisation),
  §6 (`totrow`/`totcol` + singular-arg deprecations), §19 (empirical-OR level reference).
- **Display, output & export**: §7 (col% + several row_vars → transpose at export), §8 (exporter
  base+list methods; Excel engine → Phase 9), §10 (total-column base range), §13 (output-shape table),
  §18 (numeric diff-color scale — sd-standardized), §21 (exporter phasing 7 vs 9 + the backend seam),
  §22 (exporter feature parity — what to unify / extend / keep exporter-specific), §23 (tab_kable
  performance profile — empirical).
- **Inference policy** (review sessions 2–4): §14 (weighted estimate + unweighted `n`; omnibus F
  included, Q13), §15 (CI/stars duality — score ⇄ Newcombe, uncorrected pair; mean z ⇄ Welch-t, Q14),
  §16 (test-result placement — `test` tibble rows incl. per-column, Q15 + display future).
- **Retro-compat**: §17 (the consolidated accepted-breaks inventory).

---

## 1. Confidence intervals — store bounds `ci_inf` / `ci_sup`, not a half-width

### The bug in the current implementation

`tab_ci()` stores a **single scalar** = the **upper** half-width `ci = upr.ci - est`
([tab.R:4914-4931](../R/tab.R#L4914), `ci_base`/`ci_diff`), and the **default display** is the
`[inf;sup]` bracket (`options(tabxplor.ci_print = "ci")`, `utils.R:70`), reconstructed **symmetrically**
as `est - ci … est + ci` ([fmt_class.R:1488-1492](../R/fmt_class.R#L1488)) with a display-only `[0,100]`
clamp. So the printed lower bound is wrong whenever the interval is asymmetric around the estimate.

### Which intervals are symmetric, which are not (grounded)

| Estimator                         | tabxplor method                                    | Symmetric around the estimate?                                                      |
|-----------------------------------|----------------------------------------------------|-------------------------------------------------------------------------------------|
| **Wald** proportion               | not used                                           | **Yes** — `p̂ ± z·√(p̂(1−p̂)/n)`, a direct MoE. (Source of the "moe" intuition.)    |
| **Wilson** proportion cell        | `method_cell="wilson"` (default)                   | **No** — score-test inversion; center `(x+z²/2)/(n+z²)` shifted toward 0.5.         |
| **Agresti-Coull** proportion diff | `method_diff="ac"` (default)                       | **No** — Wald on padded `(x+2)/(n+4)`; symmetric around the padded value, not `p̂`. |
| **Mean / mean-diff** (t/z)        | `z·√(var/n)` ([tab.R:4906-4912](../R/tab.R#L4906)) | **Yes** — exact half-width, nothing lost.                                           |
| **Odds ratio**                    | log-scale Wald → `exp()`                           | **No** on the natural scale; **Yes** on the log scale.                              |

Concrete error: p̂=10%, n=100 → true Wilson `[5.5, 17.6]`, tabxplor currently prints `[2.4, 17.6]`
(upper correct because it stores the upper arm; lower ~3pp too low). Users see this by default.

Odds-ratio note: log-symmetry ⇒ `OR_inf · OR_sup = OR²`, so point + one bound reconstructs the other —
but that shortcut is moot once we store two bounds for proportions anyway.

### Decision

- **Add per-cell fields `ci_inf` and `ci_sup`** (absolute lower/upper bounds of the cell's interval —
  around the proportion for `ci="cell"`, around the difference for `ci="diff"`, around the mean, around
  the OR). Faithful for Wilson/AC/OR; for symmetric means `inf`/`sup` are just `est ∓ h`.
- **Drop the `ci` half-width field** (net −1). `$ci` / `get_ci()` **recompute** it from
  `ci_inf`/`ci_sup` (upper arm `ci_sup − est`, dispatching on type for `est`) so user code keeps
  working; the public `fmt(ci = )` constructor arg is **kept** (maps a symmetric `ci` → `inf`/`sup`).
  Caveats: low-level `vctrs::field(x,"ci")` and *setting* `ci` stop working (both rare/internal); all
  internal `set_ci`/`get_ci` call sites migrate to the bounds; `± moe` reads the bounds, while **per-cell
  significance reads the `pvalue` field** (§12), not the bounds.
- **Display**: the `[inf;sup]` bracket (default) reads `ci_inf`/`ci_sup` directly → now exact (keep the
  `[0,100]` render clamp). The compact `± moe` mode shows means exactly and, for asymmetric cells, the
  **conservative larger arm** `± max(est−inf, sup−est)` (keeps the one-number format, never understates).
- **`tab_logit`**: OR CIs go into `ci_inf`/`ci_sup` (filled from `broom::tidy(conf.int=)`); the
  `OR_inf`/`OR_sup` **sidecar columns retire** ([tab_logit.R:463-464,527](../R/tab_logit.R#L463)).
- **Significance / stars / color formulas** ([fmt_class.R:2191-2295](../R/fmt_class.R#L2191)) no longer
  read the single half-width `abs(diff) > ci`. Per-cell significance now comes from the stored **`pvalue`**
  field (§12, Q2 — the honest source for 90/95/99 stars at once, and the only correct one for asymmetric
  Wilson/AC proportion diffs and OR, where a single stored CI level cannot yield three thresholds). The
  `[inf;sup]` display still reads `ci_inf`/`ci_sup`; a "CI excludes 0" check (`ci_inf > 0 | ci_sup < 0`)
  remains a valid *visual* cue at the CI's own level, but the stars are driven by `pvalue`. Land both in
  Phase 3 with the CI/chi2 aggregate migration.
- **Interval *method* when stars are on** (§15): to keep the visible `[inf;sup]` bracket and the stars
  *coherent* (bracket-excludes-0 ⇔ starred), the stored **diff** interval switches from Agresti-Caffo to
  the **Newcombe** score interval — the near-exact dual of the two-proportion score test that fills
  `pvalue`. Wilson (`ci="cell"`) is already the score dual; OR intervals are already the log-OR Wald
  duals. **Mean intervals are z-based today** (`stats::qnorm`, [tab.R:5591](../R/tab.R#L5591)), so they
  follow the same default-sensitive rule (Q14, review session 4): `z` when stars are off (byte-parity),
  **Welch-t** when stars are on — the dual of the Welch-t `pvalue`. AC (and z for means) stay the default
  only when stars are off. Detail + grounding: §15.

Sources (asymmetry): Wikipedia *Binomial proportion confidence interval* (Wilson/Agresti-Coull);
Wikipedia *Odds ratio* + BU SPH PH717 Module 8 (log-scale CI, `exp` asymmetry). See the CI section of
the architecture guide once implemented.

---

## 2. Reference-base field — store `tot_n` (the cell's OWN base); recover `tot_wn`

Grounded split: **percentages use the weighted base** `get_wn(tot)`
([tab.R:5602-5608](../R/tab.R#L5602)); **CI/chi2/T use the unweighted base** `get_n`
([tab.R:4845-4847](../R/tab.R#L4845), chi2 p-value unweighted [tab.R:5258](../R/tab.R#L5258)). Today
both are recomputed on demand by looking up a total row/col cell. `detect_totcols`
([fmt_class.R:1271-1285](../R/fmt_class.R#L1271)) is **position-based** (each column → the first total
column at/after its index), so the **primary calc already uses each col_var's own base** (via
`tot_cols[[col]]`); the approximation the roadmap targets bites the **post-hoc / standalone recompute**
path (`tab_ci`/`tab_pct` on a built table), not the forward pass — corrected per the audit; see §11.

**`tot_n` is the cell's OWN percentage base — NOT the diff-reference's base.** Two distinct concepts,
easy to conflate (the `tot_n` *name* invites the confusion):
- **Base / denominator** = the cell's own row total (row%) / col total (col%), shown in the total
  **column** (row%). Drives `pct` (weighted base, *recovered* as `wn/pct`, §11) and `ci="cell"`
  (unweighted base = **`tot_n`**, stored). It has **nothing to do with the `ref` argument**.
- **Comparison reference** = the reference **row** set by `ref` (`first`/`tot`/…), flagged per cell by
  `in_refrow`. Drives `diff` (= `pct − ref_pct`, from stored pcts) and `ci="diff"`.

Worked example — `pct="row"`, base = total column, `ref="first"`: cell (i,j) has `pct_ij = wn_ij ÷
rowtotal_i`; `tot_n` on (i,j) = **row i's own** unweighted total. `diff_ij = pct_ij − pct_1j` (row 1 =
reference, found via `in_refrow`). `ci="cell"`(i,j) uses `tot_n_i`; `ci="diff"`(i,j) uses `tot_n_i`
**and** `tot_n_1` (the first-row cell's OWN `tot_n`, read after locating it by `in_refrow`). So the
reference row's base is just **`tot_n` read off the reference cell** — never a separate field, and `tot_n`
is never "the base picked by `ref`".

**Decision — store `tot_n` only** (materialises the existing local base transmute — named `x_n` at
[tab.R:2864-4877](../R/tab.R#L2864), with the reference base `ref_n` at [tab.R:4905](../R/tab.R#L4905));
`tot_wn` is recovered (§11). Bases are row/col totals per `pct`
("row"→row total, "col"→col total, "all"→grand total). This retires `detect_totcols` **on built tables**
(each proportion cell carries its own exact base). Whether that is load-bearing enough to keep the field
is analysed in §11.

---

## 3. Color diff vs ratio — rename the unused `rr` field to `ratio` (Q3, 2026-07-07)

Today numeric ratios ride the `diff` field (Mean-diff asymmetry constraint) and pct-column ratios ride
the `mean`-overload. The record already carries an `rr` (relative-risk) field that **has never been used**
by any code or by the maintainer. **Decision (Q3)**: **rename `rr` → `ratio`**, reposition it
**immediately after `diff`**, and make it the single home for every ratio-shaped comparison — no net-new
field.

- `diff` is ALWAYS a difference; `ratio` is ALWAYS a ratio. By column type:
  + **pct columns**: `ratio` = relative risk `cell_pct / ref_pct` (this is exactly what drove the old
    `mean`-overload "×2 rule", and it is also the RR step inside odds-ratio calculation — so reusing the
    RR field is semantically correct). `diff` = `cell_pct − ref_pct` (unchanged, safe).
  + **numeric/mean columns**: `ratio` = `cell_mean / ref_mean` (the OLD numeric-`diff` behaviour);
    `diff` = `cell_mean − ref_mean` (the flip).
- `color="diff"` colors `diff`; `color="ratio"` colors `ratio`; `color="diff_ratio"` uses both (text vs
  background — pick a background ramp so both stay readable).
- The `mean`-overload is removed: `mean` now holds only an actual cell mean (numeric columns), `NA` for pct.
- Retro-compat (Q1): pct `$diff` unchanged. Numeric `$diff` flips ratio→difference — **accepted** (numerics
  are rarely used; pct columns are the real retro-compat surface; the ratio is still available in
  `$ratio`). `$rr` disappears (was never used). `/color-mode`, `/vctrs-field`.

---

## 4. Reference ROW — row-vs-col semantics, and the named-vector `ref`

`ref` is **reinterpreted by `pct`** ([tab.R:5744-5748](../R/tab.R#L5744),
[tab.R:2519-2628](../R/tab.R#L2519)):
- `pct="row"` / means → `ref` selects a reference **row** (matched against row_var levels).
- `pct="col"` → `ref` selects a reference **column** (matched against column names); `ref2` drives the
  OR reference row ([tab.R:2649-2659](../R/tab.R#L2649)).

**Decision**: `ref` becomes a (optionally named) **character vector = one reference row per row_var**
(applied by name, else by order). It governs row%/means only. Under `pct="col"` it **collapses to the
single existing column-reference** (with a one-time message if a multi-element ref is passed with col%),
because a per-row_var *row* reference has no col% meaning. This is the common real use case (first-line
for ordinal factors, total row for nominal factors, in one multi-row_var table).

---

## 5. Vectorisation over row_vars — the simplification rule

A multi-row_var table is meant to be **mirror tables of the same shape** for different explanatory
variables. Divergent color/pct/etc. across row_vars in one merged table is misleading. So:

- **NO LONGER vectorised over row_vars (become global/scalar)**: `OR`, `pct`, `color`, `comp`, `ci`,
  `chi2`, and `ref2` (OR level-reference, column direction, follows OR).
- **STILL vectorised over row_vars**: `totaltab`, `ref` (as the named vector above).

Escape hatch for genuinely different tables: build each with its own `tab()`, `list()` them, and pass
the list to an export function (rendered **one-after-another**, not merged — see § 8). This rule
simplifies the aggregate-core, the tab()/tab_many() merge (Phase 6), and the whole workflow.

---

## 6. Deprecate `totrow` — there is always a total row

**Decision**: deprecate the `totrow` argument; always compute the total row. Users drop it after
calculation with `dplyr::filter(!is_totrow(.))`.
- Safe because `tot_n`/`tot_wn` put the base per-cell → the total row is no longer needed for any
  calculation.
- Keep the total **column** structure — col% coloring structurally depends on it surviving
  ([tab.R:2132-2135](../R/tab.R#L2132)); there is always exactly one. The `totcol` *argument* is
  soft-deprecated separately (next paragraph). Here, deprecate the total *row* only.
- Keep `is_totrow()` exported. Exporters that style the total row simply find none if removed. Results
  are already in the fields at calc time. Rework internal `tot="no"` shortcuts (e.g. the `ci="cell"`
  bug, [tab.R:3454](../R/tab.R#L3454)) to always include the total row (Phase 3).
- Soft-deprecation etiquette (review session 4): during the deprecation window `totrow = FALSE` **keeps
  working cosmetically** — the total row is always computed (no calculation depends on the arg any more),
  then filtered at output assembly alongside the deprecation warning; exactly the `totcol` treatment below.

**Soft-deprecate the `totcol` argument** (Q1 answer — no hard break; `totcol` is exported & documented,
[tab.R:636](../R/tab.R#L636), and `tab()` translates `tot="col"`→`totcol`, [tab.R:378](../R/tab.R#L378)).
The **default becomes always exactly one** total column, positioned like the current default — after the
last factor column, before numeric (mean) columns. Because `tot_n` + recovered `tot_wn` put the exact
base on **every** cell, `totcol` is now **purely cosmetic**: it only chooses which/how-many total
*columns are displayed*, never a calculation base. So the old values keep working behind a
`lifecycle::deprecate_soft()` — `totcol="each"` still renders per-col_var total columns, `totcol="no"`
still hides it (and `tab_plain()` remains the clean no-total path). With multiple col_vars of different N
(NA), the single displayed total ≠ each per-cell base, by design (the §10 range display). All-in-one
tables (the dominant use) already show one total → unaffected. Move/drop the column via dplyr as before.

Interim (Phases 2→6): the aggregate-core computes **one canonical base set**; while `totcol="each"` is
still live (and after, behind `deprecate_soft`), per-col_var total columns are produced by **cosmetic
replication at output assembly** — never as separate calculation bases.

Also drop the `tabxplor.compact` option (superseded by `output_list`, default `FALSE`).
Deprecate singular `row_var`/`col_var` args (keep working, soft) → only `row_vars`/`col_vars` remain.
The **col_var axis stays flexible**: `pct` / `levels` / `digits` remain settable per col_var (sup_cols
preserved); only the row_var axis is globalised (§5) and the `totcol` *argument* is soft-deprecated (above).

---

## 7. col% with several row_vars — transpose at export

Grounded problem: the compaction machinery is **row-percentage-shaped** — it promotes each block's
total **row** to a reference **row** ([tab_classes.R:1004-1016](../R/tab_classes.R#L1004)), but col%
coloring reads a reference **column** and never a row ([fmt_class.R:2825-2827](../R/fmt_class.R#L2825)).
So col% + several merged row_vars loses the per-block coloring story (pure col% factor tables stay
coherent via the shared Total column; **col% + means** additionally suffers a pre-existing row/col
reference mismatch — means referenced by row, factors by column).

**Phase 7b clarification (2026-07-09): the col%+means reference mismatch is INTENDED behaviour, not a
bug to fix.** A numeric (mean) variable only ever appears as a column, and a mean's reference is
meaningfully one of its *rows* (compare the mean across groups) — a mean has no column-percentage to be
referenced against a column by. So on a mixed `pct="col"` table the two column types legitimately use
different reference axes. There is no clean fix without white-elephant arguments/UI (the only real
difficulty is a UI setting both consistently). It is therefore kept, warn-only, and documented in
`dev/tabxplor_argument_computation_map.md` §8. This is separate from the multi-row_var transpose
workflow decided below.

**Decision** (Phase 8): keep `pct="col"` **single-row_var** as-is. The col%-multi-row_var path is a
**manual, opt-in** workflow, not automatic magic:
- The user inverts it themselves — swap row_vars↔col_vars and use `pct="row"` — so compaction/coloring/
  references all work on the built table.
- At **export only** (`tab_kable`/`tab_md`/`tab_xl`), an **opt-in transpose argument** flips the
  rendered grid back to the col% layout — a *layout* transform on already-formatted, already-colored
  cells (colors are per-cell fields → transpose trivially; the per-column attributes never move).
- **Console `print()` never transposes** — it always shows the real data-frame order. No note at the
  transpose step (the user drove the inversion deliberately).
- **Warn** when a user passes `pct="col"` with several row_vars, explaining the limitation and the
  invert-then-transpose-at-export workaround.

Caveats: transpose must stay at the display layer; numeric/mean col_vars don't invert cleanly
(factor-oriented path); exporters swap total-row↔total-column styling. `tab_transpose()` is the
mechanism (below).

**`tab_transpose()`** (a stub the maintainer added to `R/tab.R` — **already `@export`ed** at
[tab.R:1773](../R/tab.R#L1773) but undocumented, single-total-row/col only, unqualified verbs; so it is a
*broken public function today*) is to be **finished, documented, and possibly generalised** (tab_vars?) at
Phase 8 — it is the mechanism for the above. Do not wire it in before Phase 8.

**compact + tab_vars**: deferred. Merging tables that carry tab_vars needs compound
`group_by(tab_vars, row_var)`, interleaving row_vars within each tab_var block, per-(tab_var × row_var)
reference re-scoping, chi2 alignment, and two-level print/kable rendering
([tab_classes.R:969-975](../R/tab_classes.R#L969)) — revisit during Phase 8. Until then, tables with
tab_vars stay a list/grouped structure regardless of `output_list`.

---

## 8. Exporters — base method + list method

**Decision** (Phase 8): every exporter (`tab_xl`, `tab_kable`, `tab_md`, `tab_plot`) has (a) a base
method for a single `tabxplor_tab`, and (b) a method for a **list of tables** that renders them
**one-after-another, not merged** (kable: an HTML container holding several tables; xl: sheets/blocks;
md: sequential). This is the export side of the "different tables → list() → export" escape hatch in § 5.

**Excel engine (openxlsx → openxlsx2) — isolated to Phase 9 (follow-up decision, 2026-07-07).** Phase 8 builds the shared
exporter-prep helper and the base+list `tab_xl` methods on the **current openxlsx v1** engine. The engine
swap to **openxlsx2** (common styles created once; optional conditional formatting) is a full dependency
migration with its own parity risk, so it is **pulled out into its own Phase 9** (may ship in a 1.4.x
follow-up) — the exporter-prep unification must not be entangled with it. Precondition: `test-export-parity.R`
green on openxlsx v1, so Phase 9 verifies byte-for-byte against a known-good baseline. See CLAUDE.md Phase 9.

---

## 9. Resulting `tabxplor_fmt` field list after the Phase 1 pass

Current 15 per-cell fields, one combined vctrs surgery → **18 fields** (updated 2026-07-07 for the
`pvalue` (Q2) + `rr`→`ratio` (Q3) decisions):

- **Add** (+4): **`pvalue`** (§12 — per-cell significance, Q2), **`tot_n`** (§2 — the cell's own % base,
  renamed from the roadmap's `ref_n`), `ci_inf` + `ci_sup` (§1). `tot_wn` is NOT stored — recovered as
  `wn/pct` via `get_tot_wn()` (§11).
- **Rename** (net 0): **`rr` → `ratio`** (§3 — the `rr` field was never used; it becomes the single ratio
  home), **repositioned immediately after `diff`**.
- **Drop** (−1): `ci` — recomputed on access from the bounds; public `fmt(ci = )` arg kept (§1). The
  load-bearing internal `get_ci()` = raw `vctrs::field(x,"ci")` ([fmt_class.R:1144](../R/fmt_class.R#L1144))
  must be rewritten to derive from `ci_inf`/`ci_sup`; the overridable `$.tabxplor_fmt`
  ([fmt_class.R:1871](../R/fmt_class.R#L1871), already special-cases `wn`) gets a `ci` branch too.
- **Adjust semantics**: `diff` (numeric now = difference not ratio, §3), `ratio` (holds RR for pct /
  mean-ratio for numeric, §3), `mean` (pct overload removed → actual mean only, §3).
- **Net**: 15 − 1 (`ci`) + 4 (`pvalue`, `tot_n`, `ci_inf`, `ci_sup`) = **18**; `rr`→`ratio` is a rename,
  not a count change.
- **Proposed field order**: `n, display, digits, wn, pct, mean, diff, ratio, ctr, var, ci_inf, ci_sup,
  pvalue, or, tot_n, in_totrow, in_tottab, in_refrow`.
- Column attributes: the `ref` attribute gains per-row_var vector semantics (§4); the `totcol` attribute
  stays (marks the displayed total column) but the `totcol` *argument* is soft-deprecated (§6); no *new*
  attribute — per-column omnibus tests live as rows of the table-level `test` tibble, not as a ninth
  attribute (Q15, §16). Fix the `fmt()` `refcol`-cast bug (casts `totcol` instead of `refcol`,
  [fmt_class.R:274](../R/fmt_class.R#L274)) in the same pass.

Downstream touch-list per `/vctrs-field` (do all in one pass): `new_fmt()`, `fmt()`,
`format.tabxplor_fmt()`, `pillar_shaft`, `vec_arith`/`vec_cast`/`vec_ptype2`, `$.tabxplor_fmt`, the
`get_*`/`set_*` field factories, the `tab_pct`/`tab_ci`/`tab_chi2` writers, and the exporters (`tab_xl`
display bypass). Regenerate golden once.

---

## 10. Total-column base display when col_vars differ — DECIDED: A (display-time range)

**When it arises.** Only when col_vars have **different valid bases** — chiefly `na="drop"` with
different NA rates per question. With `na="keep"` (default) NA is a shown category, so every col_var's
row total = the full N → bases equal → `min == max` → a **normal scalar**. So this is an **edge-case
display refinement**, not pervasive complexity.

**Problem.** One total column (row%) must summarise K different row bases (one per col_var). What
number(s) to show in each total-column cell?

**Sub-questions answered:**
- *Reference totals or all totals?* — about the **percentage base** (row/col total), shown in **every**
  total-column cell (each row's own base range), **independent of the diff reference**.
- *Diff reference = first line, not total?* — **unaffected**: the percentage base is always the row/col
  total; the diff reference points at a *row* to subtract and is orthogonal. The base-range display
  lives in the total **column** and doesn't move.
- *Extra dedicated base-range fields needed?* — **No** under Option A: the cell's own base is already
  available (`tot_n` stored, weighted base recovered `wn/pct`), so the range is **derived at display**.
  Avoids field multiplication.

**Options:**
- **A — compute the range at display (recommended).** Store only per-cell `tot_n` (weighted base
  recovered `wn/pct`); the display layer reads each col_var's base from the data cells, takes `min`/`max`
  across col_vars per row, and renders `[min;max]` (default) / `min` (option) / scalar when equal. No
  overload, no new fields; calculations never read total-cell fields (holds once Phase 3 moves CI onto
  per-cell `tot_n`).
- **B — overload total cells (`min` in `n`/`wn`, `max` in `tot_n`/`tot_wn`).** The original idea. No new
  fields, but gives `n`/`tot_n` a **special meaning in total cells** — RED FLAG: reintroduces the
  overload pattern we're removing, `get_n(total_cell)` becomes misleading, and CI reads `get_n(tot)` as
  the base (would use `min` for all col_vars) until Phase 3 migrates it.
- **C — show `min` only + subtext note.** Simplest: total column shows the smallest (safest) valid
  base; legend/subtext notes bases vary. No range machinery, no overload.
- **D — dedicated `base_min`/`base_max` fields.** Cleanest semantics, +2 fields → 21 (against minimalism).

**Decided: A** — compute the range at display: render `[min;max]` (default) / `min` (global option) /
scalar when equal, from each col_var's per-cell base (`tot_n`, and the weighted base recovered `wn/pct`).
No overload, no new fields (18). C stays the fallback if the display logic proves heavy in an exporter.
(Phase 3 for the fmt/print side; the exporters mirror it in Phase 8.)

Implementation caveat (review session 4): the range is **cross-column** information —
`format.tabxplor_fmt()` formats one column at a time and cannot see sibling columns — so the `[min;max]`
must be computed by a **table-level display pre-pass** (print prep / the Phase 8 shared exporter-prep
helper) and injected into the total column's rendering, never inside the per-column format method.
`tab_xl` corollary: a `[min;max]` cell is text, not a number — either write it as a text cell in the
total column or fall back to Option C (`min` + subtext note) for Excel; decide with the exporter-prep
helper at Phase 8.

---

## 11. Red-flag check — is `tot_n`/`tot_wn` enough? (what still needs the totals)

Grounded check of whether "store each cell's base so calculations don't hunt for reference cells" holds.

**Cell-local via `tot_n`/`tot_wn`:** `pct = wn / tot_wn` (own weighted base) and `ci="cell"` (proportion
CI with base `tot_n`, own unweighted base). This is what retires `detect_totcols`. ✓

**Still locate the comparison reference (cheap — the stored `in_refrow` flag):** `diff` (= `pct −
ref_pct`), `OR`/`ratio` point, `ci="diff"`, the mean T-test each read the **reference cell's** `tot_n`/`pct`
after finding it by `in_refrow`. That lookup was always needed for `diff`; `tot_n` just puts the
reference's *base* on the reference cell instead of re-deriving it from a total column. Not a regression.

**Genuinely needs the TOTAL row/col — the real red flag, but NOT a break:** **chi2 p-value** and **chi2
contributions** (`ctr`/`var`, `color="contrib"`) need the FULL margins — row totals **and** col totals
**and** grand total. For row% a cell has its row total (`tot_n`) but **not** its column total (= the
total-row cell), so chi2 must read the total row/col. These are **whole-table statistics computed
upfront** (in the pipeline / aggregate-core, with totals present — always present now that `totrow` is
deprecated-on), results stored in `ctr`/`var` + the `chi2` attribute. `tot_n` was never meant to serve
them; they remain whole-table (as chi2 inherently is). (pct="all"/"all_tabs": base = grand total, which
*is* `tot_wn` in those modes → cell-local. ✓)

**Key refinement — `tot_wn` is redundant (recoverable).** The weighted base satisfies `tot_wn = wn /
pct` (up to the `pct` scale) for any non-empty proportion cell, and the total cell (`pct = 100%`) gives
it directly — `pct` is stored at full precision (display rounding is separate). So the weighted base is
**recoverable per cell → no field needed**. Only **`tot_n` (the *unweighted* base) is unrecoverable** in
a weighted table (weighted `pct` can't yield the unweighted row sum) → **must be stored**. In unweighted
tables even `tot_n = n/pct` (field always-present, simply redundant there). **Mean cells need no
`tot_n`** — the own `n` is the base (`ci_mean = z·√(var/n)`); `tot_n` is a *proportion-cell* concept.

⇒ **Decided: drop `tot_wn`** — recover it via a `get_tot_wn()` accessor (`wn/pct`, mirroring the
dropped-`ci` trick), with a `pct==0` fallback to a sibling/total cell.

### Does storing `tot_n` earn its place? — DECIDED: keep (renamed from `ref_n`)

Honest look at *where* a stored `tot_n` helps, given the aggregate-core:
- **Aggregate-core forward pass (Phase 2)** — **no benefit.** The core holds the aggregate (all counts),
  so it computes every base by rollup; writing `tot_n` into cells is ~free (base already computed) but
  not *needed* to compute anything.
- **CI display** — **no benefit.** The interval is stored (`ci_inf`/`ci_sup`).
- **Jamovi (Phase 7)** — **no benefit.** Recompute (e.g. new `conf_level`) runs off the **cached
  aggregate**, not off final-cell fields; display-only toggles don't recompute.
- **Statistics on a STANDALONE built table (no aggregate around)** — **this is the one real benefit.**
  `tab_ci`/`tab_pct` as retained (soft-deprecated) wrappers, and user post-processing, read the base
  from the cells. With `tot_n` stored → exact per col_var. Without it → they fall back to
  `detect_totcols` (the current approximation/bug) or must re-run `tab()`.
- **§10 total-column range** — needs each col_var's base; the *weighted* one is recovered (`wn/pct`); an
  *unweighted* range would need `tot_n`.

So storing it is **not** justified by performance or Jamovi — its value is making the **built table
self-sufficient for exact statistics** (retiring `detect_totcols` on built tables), for the
soft-deprecated `tab_ci`/`tab_pct` direct path and hand post-processing. It is ~free to store (the
forward pass computes the base anyway).

**Decided: keep it, renamed `ref_n` → `tot_n`** (→ 18 fields). `tot_n` = *the n the percentage was
computed on* (the cell's own row/col total), NOT the reference row/col's n. The rename removes the
"reference" ambiguity.

**Jamovi cache boundary (why the rename matters).** `tot_n` is **stable** — it only changes when the
percentage's base changes (i.e. when the aggregate / `pct` mode / NA handling changes), so it is a good
**cached** quantity. The *reference* row/col's n, by contrast, is re-read whenever the user changes
`ref` — which is a **display-level** change (no re-aggregation). Keeping the two names distinct (`tot_n`
= cached base vs the `in_refrow`-located reference base) mirrors the Phase-8 cache split: recompute
`tot_n`/pct only on base changes; re-resolve the reference on `ref` changes.


---

## 12. Per-cell significance `pvalue` — which test for each use case (Q2, 2026-07-07)

Decision (Q2): store one per-cell **`pvalue`** field. Stars render from it at any threshold (default
`*` p<0.10, `**` p<0.05, `***` p<0.01 — customisable via an option/argument). It is filled per cell by the
test matching the cell's estimator, so "CI excludes 0 ⇔ starred" stays coherent **and** three star levels
come from a single stored number (impossible from one stored CI level, and ill-defined for asymmetric
Wilson/AC/OR intervals — the reason Q2 chose a stored p over stored bounds-only).

**The rule (Q5 → §14): weighted estimate + unweighted `n`.** Every per-cell CI/test uses the **weighted
point estimate** (weighted `pct` / weighted mean — what the cell shows and colors) together with the
**unweighted sample size `tot_n`** (as chi2 already does, [tab.R:5285](../R/tab.R#L5285)). Weighting
represents structure; it does not manufacture independent individuals, so the *amount of information* is
the real unweighted `n`. For a 0/1 variable this is exactly "weighted % + unweighted n" (its weighted
variance is `p_w(1−p_w)`), so proportions and means obey **one** rule. Caveat: this sets the design effect
to 1 → **anti-conservative when weights vary a lot** (CIs too narrow, stars too generous); the cheap,
non-survey-design mitigation is Kish `n_eff = (Σw)²/Σw²` (opt-in, needs `Σw²` — G1). Full grounding, scope
limits (no clusters/strata) and the non-integer-count caveat: **§14**.

**Cross-cutting caveat — reference dependency.** When the comparison reference *contains* the cell
(`ref="tot"`, cell ⊆ total), the two groups are not independent, so an independent two-sample test is
mildly anti-conservative. Where the reference is detectably the total, test the cell against the
**complement** (reference − cell); when the reference is a disjoint row/subgroup (`ref="first"` or a
specific level) the two-sample test is exact. Implement the complement correction if cheap; otherwise
document the small, conservative-leaning approximation for `ref="tot"`.

| Use case                                                        | Estimator (what is compared)                                      | Test → `pvalue`                                                                                                                                                                                         | Base                                                                       | Notes                                                                                                                                                                                                                                                                    |
|-----------------------------------------------------------------|-------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Factor col_var, `ci="diff"`**                                 | cell proportion vs reference-row proportion                       | two-sided **two-proportion score test**, **uncorrected** (manual score `z` ≡ `prop.test(correct=FALSE)` — never the Yates-corrected `prop.test()` default, §15); dual of the **Newcombe** difference CI | weighted `pct`, unweighted `tot_n` of cell & of the `in_refrow` cell (§14) | When stars are on, the stored diff CI switches AC→Newcombe so bracket ⇄ stars agree (§15). `ref="tot"` → test vs complement (caveat).                                                                                                                                    |
| **Factor col_var, `OR=TRUE`** (empirical OR, no logit)          | 2×2 odds ratio: (cell level vs `ref2` level) × (row vs `ref` row) | **Wald test on log(OR)**, Woolf `SE=√(1/a+1/b+1/c+1/d)`, `z=log(OR)/SE`; dual of the log-scale OR CI (`exp(logOR ± z·SE)`)                                                                              | weighted OR point estimate; **unweighted** 2×2 counts for the SE (§14)     | Haldane–Anscombe +0.5 when any of a,b,c,d = 0. Empirical only — distinct from logit.                                                                                                                                                                                     |
| **Numeric col_var, `ci="diff"`**                                | cell mean vs reference-row mean                                   | **Welch two-sample t-test** (unequal variance): `t=(x̄c−x̄r)/√(s²c/nc+s²r/nr)`, Welch–Satterthwaite df                                                                                                  | weighted mean + weighted `var`, unweighted `n` (§14)                       | Confirms the maintainer's intuition: t-test p<5% ⇒ significant cell-vs-ref mean difference. Weighted var + **unweighted n** (Kish `n_eff` opt-in), *not* full survey design. Interval quantile: `z` stars-off / Welch-t stars-on (Q14, §15). `ref="tot"` caveat applies. |
| **`tab_logit()`**                                               | logistic-regression coefficient                                   | model **Wald z** p-value, straight from `broom::tidy(…)$p.value`                                                                                                                                        | model n                                                                    | Obvious case; also fills OR `ci_inf`/`ci_sup` from `conf.int=TRUE`.                                                                                                                                                                                                      |
| **`color="contrib"`** (chi2 cell contribution)                  | cell count vs its chi2 expected                                   | **standardized Pearson residual** → normal p (`abs(resid) > 1.96 ⇒ p<0.05`)                                                                                                                             | full margins (already computed for chi2)                                   | Optional: per-cell significance for the contribution mode; residual sign gives direction.                                                                                                                                                                                |
| **Ratio / relative-risk display** (if starred)                  | cell vs reference proportion ratio (`ratio` field)                | **Wald on log(RR)**, `SE=√((1−p1)/(n1·p1)+(1−p2)/(n2·p2))`                                                                                                                                              | weighted RR estimate; unweighted `tot_n` of both (§14)                     | Only if RR significance is surfaced; analogous to the OR row.                                                                                                                                                                                                            |
| **`ci="cell"`** (interval around the cell itself, no reference) | —                                                                 | **`pvalue = NA`** (H0: p=0 / μ=0 is not meaningful)                                                                                                                                                     | —                                                                          | No per-cell star; the interval is purely descriptive.                                                                                                                                                                                                                    |

**Whole-table omnibus tests are NOT the per-cell `pvalue`** — they live at table/column level (§16), one
number under each table:

- **Factor tables**: Pearson chi2 p (existing; **fully unweighted** — counts *and* n — for G2
  `chisq.test()` parity; the one documented exception to §14, see the Q13 note there).
- **Mean tables (Q4)**: **ANOVA / Welch's F** across the row_var groups — the true mirror of chi2 ("are
  the group means different at all"), computed from **weighted group means/variances + unweighted `n`**
  (Q13 — the §14 rule; the F is new, no legacy to match). Stored the same way chi2 is (§16 — a table-level
  attribute; per-column results as rows keyed by col_var, Q15), carrying the F result for numeric columns. The per-cell Welch-t
  above is the *cell-vs-reference* companion (stars), **not** the omnibus — the two coexist and answer
  different questions (Q4 confirmed both: ANOVA for the table line, t for the cell).

Where the field is written: fill `pvalue` in the same aggregate-core transform that fills `ci_inf`/`ci_sup`
(Phase 3), so the closed-form test statistics are computed once, **vectorised, from the
sufficient-statistics aggregate** (open item G1, *Status & open items*) — never per-cell `prop.test`/`t.test` calls in a loop.

---

## 13. Output shape — return-type truth table (2026-07-07)

`output_list` (default `FALSE`) replaces `compact`; the `tabxplor.compact` option is dropped (§6, Phase 6).
The exact return type by `row_vars` × `tab_vars` × `output_list`:

| row_vars | tab_vars | output_list     | Returns                                                                                                                                             |
|----------|----------|-----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------|
| 1        | none     | FALSE (default) | a single `tabxplor_tab`                                                                                                                             |
| 1        | none     | TRUE            | a length-1 `list(tabxplor_tab)`                                                                                                                     |
| ≥2       | none     | FALSE (default) | one **merged** `tabxplor_tab` (row_vars stacked — the old `compact=TRUE` shape, now the default)                                                    |
| ≥2       | none     | TRUE            | a `list` of one `tabxplor_tab` per row_var                                                                                                          |
| 1        | present  | FALSE           | a single `tabxplor_grouped_tab`                                                                                                                     |
| 1        | present  | TRUE            | a length-1 `list(tabxplor_grouped_tab)`                                                                                                             |
| ≥2       | present  | any             | a `list` of `tabxplor_grouped_tab` — merging across row_vars **with** tab_vars is deferred (§7), so these return a list even at `output_list=FALSE` |

Grounded: today's unwrap collapses a length-1 list to a bare tab ([tab.R:1540](../R/tab.R#L1540)); the
per-element class is `new_grouped_tab` when grouping vars exist, else `new_tab`
([tab.R:1498-1505](../R/tab.R#L1498)). 1.4.0 keeps that machinery. The table above is the **new unified
`tab()`** behaviour; `≥2 row_vars, no tab_vars, output_list=FALSE` **merges by default** (was a list;
`compact` merged). Exporters consume all shapes via the base method (single tab) + list method (§8).

**`tab_many()` alias exception (Q7).** `tab_many()` today defaults `compact=NULL → getOption("tabxplor.compact")`
= `FALSE`, so `tab_many(df, c(a,b), c)` **returns a list** ([tab.R:648-649](../R/tab.R#L648), option default
`FALSE` [tab.R:575](../R/tab.R#L575)). To avoid a silent return-type break for direct callers of the (now
soft-deprecated) alias, **`tab_many()` keeps the list-default** (behaves as `output_list=TRUE` for
`≥2 row_vars`); only the unified `tab()` merges by default. The shim maps `tab_many`'s `compact=` arg
(deprecated) onto `output_list` for back-compat.

---

## 14. Weighted inference — weighted estimate + unweighted `n` (Q5, 2026-07-07)

Resolves the §12 self-contradiction ("ALL tests use unweighted `tot_n`" vs the mean row's "weighted var +
effective n"). **One rule for every CI and every per-cell/omnibus test:**

- **Point estimate = weighted** (weighted `pct`, weighted mean, weighted OR/RR) — the number the cell
  displays and colors.
- **Dispersion = weighted** (weighted variance; for a 0/1 variable that is `p_w(1−p_w)`, so a proportion is
  just the 0/1-mean case — proportions and means obey the *same* rule).
- **Sample size `n` = the real UNWEIGHTED count** (`tot_n`). Rationale (the maintainer's): weighting makes
  the sample's *structure* resemble the population; it does **not** create independent individuals, so a
  margin of error must count real respondents. Formula-wise, score / Wald / Wilson / Newcombe / Welch-t all
  need only `(estimate, n)`, so weighted `p̂`/`x̄` + unweighted `n` drops straight in.

**Caveats (grounded — tell the user honestly):**
1. **Anti-conservative under variable weights.** Setting `n_eff = n` ignores the variance inflation from
   unequal weights (design effect `deff ≥ 1`): CIs come out **too narrow**, stars **too generous**, the
   more the weights vary. This is the one real statistical cost.
2. **Cheap mitigation, still not survey design.** Kish `n_eff = (Σw)² / Σw²` corrects *exactly* the
   unequal-weight inflation (and nothing else). It needs one extra sufficient statistic, `Σw²` (G1) — no
   strata/clusters, no full `survey`-package machinery. Ship unweighted `n` as the pragmatic **default**;
   expose Kish `n_eff` as an **opt-in** (`options()`/arg). CI expressions then replace `n` by `n_eff`,
   estimate unrounded (Korn–Graubard / Dean–Pagano convention).
3. **Scope limit.** Kish `deff` assumes a stratified SRS; it is *not* valid for clustered/multistage
   designs, and can mislead when subgroups are sampled at very different rates. Document that tabxplor's
   inference is a **single-stage unequal-weight approximation**, not a design-based analysis — a user who
   needs the latter should use `survey`/`srvyr`.
4. **Non-integer effective counts.** The weighted "count" `x_w = p_w · n` is generally non-integer; the
   score/Wald/Wilson/Newcombe formulas don't care (they take `p̂`, `n`). Only routines needing integer
   counts do — the OR Haldane–Anscombe `+0.5` zero-cell fix uses the **unweighted** integer 2×2 counts;
   Fisher-exact is not offered.

**Omnibus tests follow the same rule (Q13, review session 4).** The mean-table **Welch F** uses weighted
group means/variances + unweighted `n`, so the omnibus tests the numbers the table displays — coherent
with the per-cell Welch-t stars. The **one exception is chi2**: fully unweighted (counts *and* n), to
match `chisq.test()` exactly (G2). On weighted tables the chi2 can therefore disagree with the visible
weighted percentages (and with the weighted-estimate stars) — a documented asymmetry, the price of exact
legacy parity; a weighted (Rao-Scott-style) chi2 is out of 1.4.0 scope.

This rule is implemented once, in the aggregate-core transform that fills `ci_inf`/`ci_sup`/`pvalue`
(Phase 3), vectorised from the sufficient-statistics aggregate (G1) — never per-cell `prop.test`/`t.test`.

Sources: Kish effective sample size & design effect — <https://en.wikipedia.org/wiki/Design_effect> ;
CI with `n_eff` (estimate unrounded) — Korn & Graubard, Dean & Pagano, summarised in
<https://cran.r-project.org/web/packages/PracTools/vignettes/Design-effects.html>.

---

## 15. CI ⇄ stars must be duals — score test + Newcombe interval (Q6, 2026-07-07)

The stored interval and the printed stars must never contradict (a bracket that excludes 0 while the cell
is un-starred, or vice-versa, reads as a bug). They agree **only if the interval is the dual of the test**.

**Grounded — AC vs Newcombe for the difference of two proportions.** Both Agresti-Caffo (add 1 success + 1
failure per group, then Wald on the adjusted difference) and Newcombe's method 10 (combine the two single-
sample **Wilson score** intervals, MOVER "square-and-add") have **near-nominal coverage** and **converge as
n grows**; they differ materially only at small n / proportions near 0–1. Newcombe (and the equivalent
Miettinen–Nurminen score interval) is typically **slightly narrower** than AC and, being score-derived, is
the **near-exact dual of the two-proportion score test**. AC is a *Wald-on-adjusted-counts* interval and
has **no clean dual test** — its only exact dual would be a Wald test on the `+1/+1` padded counts, which
is nonstandard and inherits the Wald test's poor small-sample size (the very defect AC/score were built to
avoid). Aligning stars to a plain Wald test is therefore a **bad idea**. Coverage acceptability:
AC ≈ n ≥ 30/group, Newcombe ≈ n ≥ 40/group; both fine for typical crosstab n.

**Decision.** Significance stars for `ci="diff"` are **opt-in** (§ Phase 3). So:
- **Stars off** → keep the current default `method_diff="ac"` (fine coverage; no golden churn).
- **Stars on** → the `pvalue` comes from the **two-proportion score test**, and the stored **diff interval
  switches to Newcombe** (the score dual) so `[inf;sup]` and the stars are coherent by construction.
  `ci="cell"` already uses **Wilson** = the score dual (no change). Empirical OR uses the **log-OR Wald**
  interval/test pair (exact duals on the log scale). **Means are z-based today** (`stats::qnorm`,
  [tab.R:5591](../R/tab.R#L5591)) while the `pvalue` is Welch-t, so they get the same default-sensitive
  treatment (**Q14, review session 4**): mean intervals (cell and diff) keep `z` when stars are off
  (byte-parity, no golden churn) and switch to **Welch-t** quantiles (Welch–Satterthwaite df) when stars
  are on — the exact dual of the Welch-t `pvalue`. Means have no user-facing `method_*` argument, so the
  z→t swap is unconditional under stars (the Q12 explicit-method rule concerns `method_diff` only). Two
  swap-under-stars pairs, then: proportion-diff **AC→Newcombe** and mean **z→Welch-t**.
- The maximally-coherent end state (always Newcombe/Wilson, drop AC) is deferred: it would change default
  numeric output (golden) with no benefit when stars are off. Revisit only if AC's lack of a dual ever
  surprises a user.
- **Continuity correction — pinned (review session 4)**: the score test is the **uncorrected** score `z`
  (≡ `prop.test(correct=FALSE)`), and its dual is the **uncorrected** Newcombe method 10 — do **not**
  inherit `prop.test()`'s default Yates correction, and do not use Newcombe's continuity-corrected
  variant (method 11): duality holds for the uncorrected pair. (The G2 Yates lock concerns the *omnibus*
  `chisq.test()` only — a different test.) Pin the exact `DescTools::BinomDiffCI` method string for
  Newcombe when Phase 3 implements it.
- **Explicit-method conflict (Q12, review session 3)**: the AC→Newcombe switch is **default-sensitive** —
  it applies only when `method_diff` was left at its default. An explicitly passed `method_diff` is always
  respected (standard R argument etiquette); a one-time message then warns that the bracket and the stars
  are no longer exact duals (the stars stay score-test-based).

Caveat (`ref="tot"`): when the reference row *contains* the cell, both the score test **and** the Newcombe
difference interval inherit the non-independence bias (§12) — apply the same complement correction (cell
vs total−cell) to both, so they stay duals.

Sources: Fagerland, Lydersen & Laake (2011) *Recommended CIs for two independent binomial proportions*
<https://www.ms.uky.edu/~mai/sta635/FagerlandLydersenLaake2011---RecommendedCIsForTwoIndependent....pdf> ;
Newcombe (1998) eleven-methods comparison <https://pubmed.ncbi.nlm.nih.gov/9595617/> ;
`DescTools::BinomDiffCI` (methods `ac`, `score`/`mn`, Newcombe)
<https://search.r-project.org/CRAN/refmans/DescTools/html/BinomDiffCI.html>.

---

## 16. Where test results live in the data model (Q8, 2026-07-07)

Three scopes, three homes — no overload:

- **Whole-table test** (Pearson chi2 for factor tables; ANOVA/Welch F for mean tables, Q4) → a **table
  attribute**: the existing `chi2` attribute is **hard-renamed `test`** (Q11, review session 3) — one
  tibble holding every whole-table test with a discriminator column (`"chi2"`, `"F"`); the
  `new_tab()`/`new_grouped_tab()` constructor arg follows the rename (threaded through ~15 call sites);
  `attr(x, "chi2")` → NULL is an accepted break (§17). Lands in Phase 3 together with "properly remove
  chi2 attribute leftovers" (CLAUDE.md Phase 3).
- **Whole-column / whole-variable test** (chi2 per col_var; the new F per numeric column) → **rows of the
  same table-level `test` tibble, keyed by col_var** (Q15, review session 4 — today's chi2 mechanism,
  which already stores per-column pvalue/df/statistic rows). NOT a new fmt column attribute: one storage
  point, the 8-attribute contract (§9) holds, and the dplyr methods already carry the table attribute.
- **Per-cell significance** (cell vs its reference cell) → the **`pvalue` field** (§12) → the stars.

**Display, now vs future (document both):**
- **Now**: the whole-table p-value renders as a **cell in its own row** under the table (as chi2 already
  does); the mean-table ANOVA/F renders the same way for numeric variables — one uniform mechanism.
- **Future (documented, not built)**: drop the dedicated row and instead flag **each cell** of a table/
  column whose omnibus test is *non-significant* with a `!` warning glyph (the mirror image of the
  significance stars — `!` = "the whole table/column shows no significant association, read these cells with
  caution"). This keeps the warning where the user looks (the cells) and reclaims the row. Purely a display
  layer over the same stored attributes → switchable later without touching the data model.

This decides the earlier roadmap "to think about" ("pvalue lines: just in print(), not in the actual
table?") in favour of **stored-as-attribute, rendered-as-row-for-now**: the number lives in an attribute
(clean under dplyr/vctrs — no phantom row for verbs to trip on), and only the *rendering* is a row, so the
future `!`-glyph mode is a display swap, not a schema change.

---

## 17. Accepted retro-compatibility breaks — the consolidated inventory

The Aim says the public API stays retro-compatible; a handful of **small, deliberate** breaks are
accepted. Listed here so each is signed off consciously (not discovered post-release). Each ships with a
`NEWS.md` line; user-facing functions/args are soft-deprecated, never hard-removed.

| Break                                                                                 | Who it affects                                                                                                                       | Why accepted / mitigation                                                                                                                                                                                                                                             |
|---------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Numeric `$diff` flips **ratio → difference** (§3)                                     | code reading `$diff` on *numeric/mean* columns                                                                                       | The one substantive break; numerics are rarely `$`-extracted, pct `$diff` (the real surface) is unchanged, and the ratio survives in `$ratio`.                                                                                                                        |
| `$rr` **disappears** (renamed `ratio`, §3, §9)                                        | code reading `$rr`                                                                                                                   | The `rr` field was **never** used by any code or by the maintainer. `$ratio` replaces it.                                                                                                                                                                             |
| `$mean` on **pct** columns changes (overload removed, §3)                             | code reading `$mean` of a pct column                                                                                                 | Was an internal "×2-rule" ratio overload; the ratio now lives in `$ratio`; `$mean` on pct columns → `NA` (an honest value).                                                                                                                                           |
| Low-level `vctrs::field(x,"ci")` and **setting** `ci` stop working (§1, §9)           | code poking the raw `ci` field                                                                                                       | Rare/internal. `$ci` / `get_ci()` still work (recomputed from the bounds); the `fmt(ci=)` constructor arg is kept.                                                                                                                                                    |
| `tab_many()` `compact` arg **deprecated**; `tabxplor.compact` **option dropped** (§6) | `tab_many(compact=)` / option users                                                                                                  | Soft-deprecated arg still maps onto `output_list`; the option is replaced by the `output_list` arg.                                                                                                                                                                   |
| Changing the **CI confidence level on a built table** now needs a re-run (§1)         | post-hoc `conf_level` tweakers                                                                                                       | Bounds are stored at one level (can't rescale a stored asymmetric bound). Stars *are* re-thresholdable without re-run (the `pvalue` is level-free). Re-run `tab()` for a different CI level. Same for toggling Kish `n_eff` (§14) — it changes bounds *and* `pvalue`. |
| `attr(x, "chi2")` **renamed `test`** (§16, Q11)                                       | code reading the table attribute                                                                                                     | Rare usage; one attribute, one name — no dual-mirror divergence risk under dplyr verbs; NEWS line. Whole-table tests (chi2 + new ANOVA/F) live under `test` with a discriminator column.                                                                              |
| Numeric `color="diff"` **changes meaning** (§3, §18, Q9)                              | calls passing `color="diff"` on mean columns                                                                                         | Was ratio-coloring (`mean_breaks`); now colors the sd-standardized difference (Glass's Δ, `mean_diff_breaks`). The old behaviour is exactly `color="ratio"`. Pct-column coloring unchanged.                                                                           |
| Old **serialized** tabs (`.rds` from ≤1.3.1) unreadable by 1.4.0 accessors (§9, Q10)  | nobody in practice — tabs are exported (Excel/HTML/md) or re-created from their R code, never saved as `.rds` (maintainer-confirmed) | No upgrade shim (permanent complexity for a non-use-case); NEWS line says "rebuild with `tab()`".                                                                                                                                                                     |

**Explicitly NOT broken (guardrails held):** `tab_many()`'s **list return type** for `≥2 row_vars` (Q7,
§13) — preserved; pct `$diff`, `$pct`, `$n`, `$wn` and the other user-read fields — unchanged; every
public function/argument — kept (soft-deprecated at most).

**Explicitly out of scope (document, don't "fix"):** **multiple comparisons.** Stars decorate every cell,
so a large table runs many tests — tabxplor applies **no** correction (standard for exploratory crosstabs).
State this once in the CI/stars documentation so it is a conscious choice, not an omission; a
Bonferroni/BH option is a possible future `to think about`, not 1.4.0.

---

## 18. Numeric diff-coloring — sd-standardized (Glass's Δ) (Q9, review session 3)

§3 flips numeric `diff` to a real difference and adds the `"diff"`/`"ratio"`/`"diff_ratio"` color modes
(Phase 5). What §3 left open: **which breaks color a raw mean difference?** Raw differences are
unit-dependent (+5 is huge on a 0–10 satisfaction score, noise on an income in euros), so the
ratio-shaped `mean_breaks` that color means today (`type=="mean"` routes to `mean_breaks`,
[fmt_class.R:1949](../R/fmt_class.R#L1949)) cannot apply, and **no universal absolute default exists**.

**Decision (Q9)**: the numeric `"diff"` color mode reads the **sd-standardized difference**
`(cell_mean − ref_mean) / sd_ref` — **Glass's Δ**, the reference-group-sd effect size: the natural
standardizer when many cells compare to one reference, and under `ref="tot"` it reads as "how many
whole-population sds above the mean". Default breaks **`c(0.2, 0.5, 0.8, 1.2)`** (Cohen's
small/medium/large conventions + one extra intensity level), a new **`mean_diff_breaks`** element in
`set_color_breaks()` / `options("tabxplor.color_breaks")`, mirrored for negatives like the other breaks.

- **No new field.** The standardization is computed inside `fmt_color_selection()`/`color_formula()`
  from stored quantities: `diff` (§3) and the reference cell's `var` (located via `in_refrow` — the same
  lookup `diff` itself needs). `$diff` stays the raw difference (§17 unchanged); the sd scale is a
  color-layer quantity only.
- **Dispersion is weighted** (`var` is the weighted variance, §14). `sd_ref == 0` or `NA` → no color.
- **Scope**: only the pure `"diff"` mode needs the scale. `"ratio"` keeps ratio `mean_breaks`;
  `diff_ci`/`after_ci` compare `diff` against its own CI — same units, already scale-free.
- **Display/legend**: the subtext legend states the scale ("colored by (mean − ref)/sd(ref)"); all
  exporters inherit it via the shared `fmt_color_selection()` (color parity is structural).
- **Retro-compat**: `color="diff"` on numeric columns changes meaning (was ratio-coloring) — inventoried
  in §17; the old behaviour is exactly `color="ratio"`.

Source: Glass's Δ / standardized mean difference and Cohen's 0.2/0.5/0.8 conventions —
<https://en.wikipedia.org/wiki/Effect_size>.

---

## 19. Empirical-OR level reference — keep `ref2 = "first"` (Q16, review session 4)

Today `ref2 = "first"` ([tab.R:298](../R/tab.R#L298)): the empirical OR reads off the **first** level of
the col_var. The Phase 1 logit-prep note suspected this lands on the "No" column for No/Yes factors.
Resolution (Q16): the maintainer's own data convention puts the **positive level first** ("Oui"/"Yes"
first), so `"first"` is already the user-friendly side on tabxplor's real-world inputs — **keep the
default, zero churn**; document `ref2 = 2` as the idiom for data coded the other way.

Caveat to reconcile later: **R's modeling convention is the opposite** — `glm()` treats the *first*
factor level as the reference/failure and models `P(second level)`, so on glm-convention data the
empirical OR (read off level 1) and a logit OR (about level 2) would describe opposite sides. Decide the
alignment when `tab_logit` lands (Later phase): either re-level / flip at the tab_logit boundary so both
report the same side, or label the reported level explicitly in the header/legend. The other Phase 1 OR
display items (print `1/OR` for OR < 1, stars, OR+ME layouts, Excel rendering of `1/x`) are display-layer
questions that land with Phases 3/7 as already scheduled — they are open *display* choices, not blocking
architecture.

---

## 20. Phase 3a IMPLEMENTED — universal CI-inclusion (supersedes §12 score-test / §15 AC-swap)

Implemented 2026-07-08. During implementation the maintainer refined the CI/test framework; the
result **supersedes parts of §12 and §15**. The governing idea: **significance is read from the
same interval that is displayed** ("universal CI-inclusion") — the stored per-cell `pvalue` is the
CI-inversion p of the method drawing the bracket, so stars (`cut(pvalue)`) and the bracket can
**never disagree, for any method**. This dissolves §15's "AC has no clean dual test" problem: every
method (default or expert) gets self-consistent stars, with no doc caveat about which methods star.

Empirically validated (`dev/verify_ci_inclusion.R`, run against DescTools/prop.test/t.test):
Wilson == `BinomCI(wilson)` (2e-16); Newcombe-10 == `BinomDiffCI(method="score")` (0); AC ==
`BinomDiffCI(ac)` (0); Welch-t == `t.test(var.equal=FALSE)` on both bounds and p; and
Newcombe-inclusion agrees with the pooled score test **99.5 %** of 12,996 configs (rare 1-bin
boundary cases only), so nothing is lost by not computing a separate score test.

**Decisions as built:**
- **One knob `ci = "cell"|"diff"|"no"`.** Defaults per estimator: **Wilson** (cell prop), **Newcombe
  method 10** (prop diff — *new default, was AC*), **z / Welch-t** (mean cell / diff). Experts set
  `method_cell` (`"wilson"`) / `method_diff` (`"newcombe"|"ac"|"wald"`); stars follow that interval.
- **Stars = universal CI-inclusion**, `stars` arg default `TRUE` (`options("tabxplor.stars")`),
  thresholds `tabxplor.signif_levels` = `c(.10,.05,.01)`, labels `tabxplor.signif_labels`. `ci="cell"`
  never stars (pvalue = NA — H0 p=0/μ=0 not meaningful). `stars = FALSE` skips significance → one
  interval eval, `pvalue = NA`. **No pooled two-proportion score test is coded** (§12's factor-diff
  test); the Newcombe inversion-p is found by a vectorised bisection on z (`newcombe_pvalue()`).
- **§15's default-sensitive AC→Newcombe swap + "not exact duals" warning is retired.** The interval
  method is now independent of stars (Newcombe is the diff default whether or not stars show).
- **Bounds are real & asymmetric.** `ci_inf`/`ci_sup` store absolute bounds; `get_ci()` = upper arm
  (`ci_sup − ci_center`, retro-compatible with the old `upr.ci − est` for `color_formula`);
  `get_ci_moe()` = the conservative larger arm for the `± moe` display; `fmt(ci=)` now stores
  absolute symmetric bounds around the estimate (the Phase-1a relative-half-width shim is gone).
  `format()` reads the bounds directly (WS2 mean-scaling FIXME dissolved). Cell CIs are now also
  drawn on the total/reference column (per §1) — a visible change on the golden `f_ci_cell`.
- **Weighted (§14):** weighted estimate + **unweighted n** (get_n of the relevant 100 % total) is the
  default. **Kish n_eff opt-in** via `options("tabxplor.kish_neff" = TRUE)` for the **numeric** CIs:
  G1's `Σw²` accumulator is added to the weighted numeric scan **only when opted in** (byte-identical
  + zero-cost default), rolled up additively, and `n_eff = wn²/w2` replaces n in the mean CI.
  **Factor-side Kish is deferred** (no per-cell `Σw²` on the count path — open item).
- **Scope:** proportions + means only. **Empirical OR** (bounds, log-OR Wald p, 1/OR display) is
  **deferred to the tab_logit phase** (Q3 refined: not 3b). Whole-table **chi2** vectorisation + the
  `chi2`→`test` attribute rename + ANOVA/F stay **Phase 3b**.
- **Engine** (`R/tab-agg.R`, pure/vectorised/dependency-free): two shapes — `ci_pivot()` (symmetric:
  AC/Wald/mean-z/Welch-t, closed-form inversion p) and the score shape `ci_wilson()`/`ci_newcombe()`
  (+ `ci_prop_diff()` / `ci_mean_diff2()` dispatchers). `tab_ci()` (props) and `tab_num()` (means)
  both route through it; **DescTools is removed from the runtime** (Imports → Suggests, kept for the
  parity tests). Dead scalar helpers `ci_mean`/`ci_mean_diff` + the DescTools closures deleted.
- **tab_xl stars deferred to Phase 8** (the exporter-unification phase, then openxlsx2 in Phase 9) —
  the console/`tab_md`/`tab_kable` stars flow from the single `format()` source of truth.
- **CI *math* is unified now; CI *placement* is deferred to Phase 4/6.** All formulas live once in
  the `R/tab-agg.R` engine, called from exactly two sites — inline in `tab_num()` (means) and in the
  post-aggregation step `tab_ci()` (proportions; `tab_plain()` computes no CI). This asymmetry is
  deliberate, not a perf compromise: the CI runs on the small aggregate (not the N rows), so fusing
  proportion-CI into `tab_plain()` gains nothing, and `tab_ci()` must keep a **field-based** engine
  regardless (it has to work standalone on a built table that carries only `fmt` fields, no counts
  data.table) — computing it *also* in `tab_plain()`'s counts domain would duplicate the Wilson/
  Newcombe math held to byte-parity, the very thing 1.4.0 removes. The clean end state (keystone:
  `aggregate → [pct | diff | CI | chi2 | totals] → fmt`) has ONE CI transform for both types; it
  arrives when `tab_plain`/`tab_num` are refactored into shared aggregate-core subfunctions — i.e.
  the **Phase 4 factor-path reorg** (driven/validated by `as_tab_counts()`, per §Phasing) and the
  **Phase 6** `tab()`→`tab_many()` merge. Fold the proportion-CI step into that shared core then;
  do NOT force it earlier (a risky ~800-line `tab_plain` refactor for no gain).

Golden regenerated (conscious): `f_ci_cell`, `f_ci_diff` (display + struct), `f_color_afterci`,
`n_mean_ci` (struct). CI parity tests updated (`test-calculations.R`): AC/Welch pinned to
`stars=FALSE`, new Newcombe/`score`, Welch-t, Wilson-both-arms, and inclusion⇔stars tests.

---

## 21. Exporter phasing — Phase 8 (openxlsx v1 prep) vs Phase 9 (openxlsx2): keep split, add a backend seam

The maintainer's question (2026-07-08): the Phase 8 `tab_xl()` rewrite is *already* a big restructure (today's
`tab_xl` is **list-first — a bare df is wrapped to a one-element list at [tab_xl.R:91](../R/tab_xl.R#L91) and
the entire body is `purrr::map`/`pwalk`; there is no single-tab path**), so should the openxlsx→openxlsx2
engine swap ride along with it (§8 puts it in a separate Phase 9), or stay split?

### Decision — keep §8's split (Phase 8 on openxlsx v1, Phase 9 = openxlsx2), but factor a **backend seam** in Phase 8

Two *different* risk surfaces, and combining them forfeits the one thing that makes the swap safe — a
byte-verified v1 baseline:

- **Phase 8 risk = parity of the display bypass.** `tab_xl` does **not** go through `format.tabxplor_fmt()`
  (Export-Parity WARNING [tab_xl.R:541](../R/tab_xl.R#L541), [1087](../R/tab_xl.R#L1087)): it writes raw
  `get_num()` numbers and rebuilds display via `numfmt()` → Excel number-format codes. Adding stars (§16),
  the label attribute, and the base+list split (§8) all perturb *this* bypass. The regression oracle is
  `test-export-parity.R` (`format` vs the `tab_xl` bypass).
- **Phase 9 risk = engine semantics.** openxlsx2 has a different style model (shared styles created once —
  the real speed lever), a long-reshape write path, and its own conditional-formatting API. The oracle here
  must be **"byte-identical to the Phase-7 openxlsx-v1 output"** — which only exists once Phase 8 is green.

Entangling them means a broken cell can't be attributed to the restructure *or* the engine. So the split
stands. **Refinement:** structure Phase 8's rewrite around a **narrow write/style backend interface** —
`{new_workbook, add_sheet, write_data(numbers), apply_style(cells, fill|font|border|numfmt), freeze_panes,
set_widths/heights, conditional_format, save}` — implemented on openxlsx v1. Then **Phase 9 reimplements
only that ~12-call backend** against openxlsx2, leaving the shared prep, the color→style selection
(`fmt_color_selection` + `select_in_color_style`, already shared with the console) and the sheet/offset
orchestration untouched. This kills the "double-touch every `openxlsx::` call site" objection (the
orchestration is restructured **once**; only the leaf backend is re-pointed) **without** merging the two
parity risks. The live `openxlsx::` surface is small and already enumerated — 13 functions: `createStyle`,
`createWorkbook`, `addWorksheet`, `writeData`, `modifyBaseFont`, `showGridLines`, `freezePane`, `addStyle`,
`conditionalFormatting`, `setColWidths`, `setRowHeights`, `saveWorkbook`, `openXL` — so the seam is cheap to
draw.

### The perf premise for openxlsx2 is weaker than assumed — treat the swap as maintenance-driven, benchmark before committing

The roadmap frames Phase 9 partly as a speed win (shared styles). Grounded caveat: openxlsx2 is **not
reliably faster for styled writes**. Its maintainer's own position is *"if you need speed, go `writexl`"*
(discussion #1281), and a documented case wrote ~10K rows to a **preformatted** workbook in **2.5–3 min**
vs near-instant in openxlsx (issue #356) — openxlsx2 reshapes input to a long frame and per-cell style /
conditional-format application is the slow part, exactly `tab_xl`'s pattern (one `addStyle` per colored
cell, [tab_xl.R:1253](../R/tab_xl.R#L1253)). So: openxlsx2's genuine wins are **maintenance** (openxlsx v1
is lightly maintained), the **shared-styles-created-once** model (a real lever *if* `tab_xl` is rewritten to
build the ~11 palette styles + the `st_digits*` set **once** and reuse — which the v1 code already half-does,
[tab_xl.R:238](../R/tab_xl.R#L238)), and **less-awful conditional formatting** (the v1 conditional-format
path was slow enough that the diff/ratio colors are hard cell styles, not CF — [tab_xl.R:150-267](../R/tab_xl.R#L150)
is the commented-out CF attempt). Pin a small styled-write benchmark (a big `compact=TRUE` table, colors on)
on **both** engines before committing Phase 9; if openxlsx2 loses, Phase 9 legitimately slips to a 1.4.x
follow-up (or is dropped) — it does not block 1.4.0. This matches §8's "may ship in a 1.4.x follow-up".

Sources: openxlsx2 styled-write slowness — JanMarvin/openxlsx2 issue #356
<https://github.com/JanMarvin/openxlsx2/issues/356> ; "if you need speed go writexl" — discussion #1281
<https://github.com/JanMarvin/openxlsx2/discussions/1281>.

---

## 22. Exporter feature parity — what to unify, what to extend, what stays exporter-specific

Grounded inventory of `tab_xl` ([tab_xl.R](../R/tab_xl.R)) vs `tab_kable`
([tab_classes.R:461](../R/tab_classes.R#L461)) vs `tab_md` ([tab_md.R](../R/tab_md.R)). Legend ✓ has / ✗
lacks / ~ partial.

| Capability                                                      | xl                  | kable                  | md               | Extend to the others?                                |
|-----------------------------------------------------------------|---------------------|------------------------|------------------|------------------------------------------------------|
| Cell **colors** (`fmt_color_selection`)                         | ✓ hard cell style   | ✓ inline span          | ✗                | md → short pandoc spans (Phase 8 md item)            |
| text vs background color mode                                   | ✓                   | ✓                      | ✗                | —                                                    |
| **Significance stars**                                          | ✗ bypasses format() | ✓ via `format()`       | ✓ via `format()` | **→ xl (Phase 8, §16)** — mirror into `numfmt`/style |
| **Tooltips / hover extra stats**                                | ✗                   | ✓ (`title=` / popover) | ✗                | keep kable-only (see below)                          |
| **col_var spanning header**                                     | ~ (layout)          | ✗ (borders only)       | ✓ [tab_md.R:227  | **→ kable** via `add_header_above` (readability)     |
| **`n_min` greying** (hide small-n rows/cols)                    | ✓ tab_xl.R:1030     | ✗                      | ✗                | **→ kable & md** (data-quality signal)               |
| **`hide_near_zero` greying**                                    | ✓ tab_xl.R:1311     | ✗                      | ✗                | **→ kable** (grey text); md ~ (marker only)          |
| **`label` attribute** (question text)                           | ✗                   | ✗                      | ✗                | **→ all** (Phase 8 item — header/legend)             |
| clean **NA hiding**                                             | ✓                   | ~ fragile HTML surgery | ✓ (`na=""`)      | **fix kable** in shared prep                         |
| row/col-name **wrapping**                                       | ~                   | ✓ `tab_wrap_text`      | ~ truncate only  | md → wrap not truncate                               |
| freeze / col widths / num-format / sheets / `colnames_rotation` | ✓                   | ✗                      | ✗                | **Excel-only — no meaning** for kable/md             |
| interactivity (popover/JS)                                      | ✗                   | ✓                      | ✗                | **no meaning** for md; xl ~ (cell comment = clutter) |
| clipboard / plain-text file                                     | ✗                   | ✗                      | ✓                | md/console-shaped — **no meaning** for xl            |
| caption / title                                                 | ✓ `titles`          | ✓ `caption`            | ✗                | md → optional title line                             |

### Decisions

- **Unify (shared prep + shared display path).** The "canonical col_vars → validate → compact" preamble is
  **duplicated four times** (tab_md self-flags it [tab_md.R:47](../R/tab_md.R#L47); also `tab_kable`
  [tab_classes.R:486](../R/tab_classes.R#L486), `tab_compact`, and `tab_xl`'s inline non-compacting variant)
  → one prep helper (Phase 8, §8). **Stars, the `label` attribute, and NA-hiding must live in the shared
  path** so the `tab_xl` bypass stops silently diverging from `format.tabxplor_fmt()`.
- **Extend cross-exporter (meaningful):** stars → `tab_xl` (§16); `n_min`/`hide_near_zero` greying →
  `tab_kable` (grey text) and, as a marker, `tab_md`; the col_var **spanning header** → `tab_kable`
  (`add_header_above`, which it curiously never uses); the `label` attribute → all three.
- **Keep exporter-specific (no cross-meaning):** the Excel-engine features (freeze panes, widths, Excel
  number formats, multi-sheet, `colnames_rotation`) are meaningless for HTML/md; **tooltips/popovers** are
  meaningless for static md and add clutter as Excel cell comments, so they stay `tab_kable`-only (and see
  §23 — they are also a real cost); clipboard/plain-text-file is md/console-shaped, not Excel.

---

## 23. `tab_kable()` performance profile — empirical (2026-07-08)

Motivation: `tab_kable` is the Jamovi module's main display, re-rendered on every option change, and is a
standing roadmap perf concern ("Comment accélérer cette fonction? Faire une version plus light…"). This
section is the measured breakdown — **what is fast, what is slow** — for the light-mode Phase and Jamovi-cache work Phase.

### Method / caveats

Installed **tabxplor 1.3.1** (the exporters are untouched by phases 0–3a, so 1.3.1 `tab_kable` is
structurally representative; the live source could **not** be profiled — a parallel chi2 refactor had
`get_chi2` non-loadable), R 4.5.1, Ryzen 5800X, `forcats::gss_cat` (21 483 rows). Cold `Rscript`, medians of
3–4 warm reps; run-to-run noise ≈ ±0.3 s, so micro-costs below are **indicative, not strictly additive**.
Scripts are in the session scratchpad (not committed). Fixtures span **cell-count** (wide) vs **row-count**
(tall) so the two scaling axes separate.

### Wall time by table shape (`tab_kable`, seconds, median)

| Fixture | rows | cols | fmt cells | colored cols | default     | no tooltip | `get_data` (pre-kable) |
|---------|------|------|-----------|--------------|-------------|------------|------------------------|
| small   | 16   | 9    | 128       | 7            | 0.55        | 0.44       | 0.42                   |
| medium  | 16   | 22   | 336       | 20           | 1.39        | 1.15       | 1.16                   |
| large   | 31   | 53   | 1612      | 51           | **5.3–5.6** | 4.6–4.8    | 4.4                    |
| tall    | 69   | 13   | 759       | 11           | 2.18        | 1.96       | 1.72                   |

**Scaling is driven by (colored) COLUMNS, not rows.** The 53-column `large` (31 rows) costs **5.3 s**; the
69-row / 13-column `tall` — nearly as many cells — costs **2.2 s**. Cause: the dominant cost has a fixed
per-column overhead (below), so wide tables hurt far more than tall ones.

### Where the time goes (large table, ~5.3 s default)

`get_data=TRUE` returns immediately after the per-cell `cell_spec` mutate
([tab_classes.R:656](../R/tab_classes.R#L656)), i.e. **before** `knitr::kable` + the styling chain, so it
splits the run cleanly:

| Stage                      | s    | share    | note                                                                      |
|----------------------------|------|----------|---------------------------------------------------------------------------|
| **Pre-kable** (`get_data`) | ~4.4 | **~80%** | color selection + format + cell_spec + tooltips                           |
| Post-kable styling chain   | ~1.1 | ~20%     | `knitr::kable` + `kable_classic` + ~9 `row_spec` + ~5 `column_spec` + CSS |

Isolated micro-costs (large; over all fmt columns unless noted):

| Component                                                                     | s         | verdict                                    |
|-------------------------------------------------------------------------------|-----------|--------------------------------------------|
| **`fmt_color_selection` × 51 colored cols**                                   | **~4.3**  | **SLOW — the single dominant cost**        |
| `tab_kable_print_tooltip` × cols (~13 `format()`/`set_display()` passes each) | ~0.55–0.8 | SLOW-ish — removable (~15%)                |
| `format.tabxplor_fmt` × cols (cell value)                                     | ~0.06     | fast                                       |
| `kableExtra::cell_spec` × cols (the `<span>` build)                           | ~0.02     | fast                                       |
| `knitr::kable(format="html")`                                                 | ~0.05     | fast                                       |
| `kableExtra::kable_classic`                                                   | ~0.04     | fast                                       |
| one `row_spec` / `column_spec` call                                           | ~0.03     | each re-parses the whole HTML via **xml2** |
| `row_spec(1:nrow, …)` (one full-table pass)                                   | ~0.20     | scales with rows × #passes                 |
| `color_legend`, `get_reference`, NA-hiding `str_replace`                      | ≈0        | fast                                       |

**The headline is counter-intuitive: it is NOT `cell_spec`, NOT `format`, NOT `knitr::kable`, and only
secondarily the kableExtra styling passes. It is the color computation.** `fmt_color_selection` (+ its
style-mapping tail `select_in_color_style`/`get_color_style`/`html_color`→`is_r_color`→`grDevices::colors()`)
is ~4 s of the ~4.4 s pre-kable, ~75% of the whole render. `Rprof` under-labels it because its body is
`purrr::map` over vctrs/dplyr primitives (`vec_case_when`, `if_else`, `DataMask$new`), so the self-time
scatters onto `.Call`/`.External2`/`vec_case_when` rather than onto the frame name — the **direct isolation**
(one `lapply(cols, fmt_color_selection)` = 4.3 s) is the honest figure. Its cost is **per (colored column ×
break level)** fixed overhead, which is why it tracks column count.

Note: color is **shared with `tab_xl`** (same `fmt_color_selection`), so this optimization pays off in the
Excel path too — and it is exactly the recompute a Jamovi display-only toggle should **not** trigger.

### Browser-load / DOM weight (the Jamovi surface)

Actual browser paint time was not measured (no headless browser in this pass); the honest proxies are DOM
byte-size, per-cell node/attribute count, and attached-dependency weight — all of which drive parse/layout.

| Output                | raw kable HTML | self-contained (deps inlined) |
|-----------------------|----------------|-------------------------------|
| default (tooltips on) | 476 KB         | 1112 KB                       |
| tooltips off          | 331 KB         | 967 KB                        |

- **Tooltips are heavy on the DOM too**: they add a `title=` + `data-toggle` to **every** cell (1612 of them
  here) and inflate the raw HTML **+44 %** (331→476 KB). Off is both faster to build (~15 %) and lighter to
  render.
- **Dependency weight ≈ 630–640 KB** (bootstrap + jQuery + lightable CSS, the self-contained delta). In
  **Jamovi this framework is already loaded** — the module strips the kableExtra class and inlines lightable
  + bootstrap `cosmo.min.css` itself ([jmvtab.b.R:384](../R/jmvtab.b.R#L384)) — so the **marginal** browser
  cost per render is the **table DOM** (331–476 KB) + the per-cell tooltip attributes, not the framework.
- Tooltip **mechanism** matters: `popover=FALSE` uses the browser-native `title=` attribute (free to
  render); `popover=TRUE` needs bootstrap JS to initialise a widget on **every** cell (O(cells) event
  wiring) — the expensive interactive mode. Do **not** default popovers on at Jamovi scale.

### Optimization levers (ranked; for Phase 8 light-mode + Phase 7 cache)

1. **Cut / cache the color computation (biggest win).** `fmt_color_selection` is ~75 % of the render and is
   pure per-column overhead. (a) **Vectorise/batch** the per-column break loop (compute the break→style map
   once, apply across columns) and hoist `grDevices::colors()`/`is_r_color` out of the per-cell path.
   (b) In **Jamovi (Phase 7)**, colors change only when the aggregate / `color` / breaks change → cache the
   selection keyed on those; a pure display toggle (theme, tooltips, wrap) must **not** recompute it. This is
   the concrete payoff of the §11 "`tot_n` = cached quantity vs re-read on display change" cache split.
2. **`tooltips = FALSE` as the Jamovi/light default.** Saves ~15 % build time, ~44 % DOM bytes, and the ~13
   redundant `format()` passes/column in `tab_kable_print_tooltip`. Offer a "detailed" opt-in for the
   interactive desktop.
3. **Collapse the kableExtra styling chain.** ~14 `row_spec`/`column_spec` calls each re-parse+re-serialise
   the entire table HTML via **xml2** (~1.1 s). Batch them (fewer calls covering more rows/cols), or — the
   roadmap's "faster flat html / markdown-table-with-css-classes" idea — **emit the styled HTML directly**
   (inline styles on `<td>` while building, skipping kableExtra's xml round-trips entirely). A direct-HTML
   "light" renderer would drop both this 20 % and kableExtra's dependency footprint, which suits Jamovi
   (which already supplies bootstrap). Weigh against losing kableExtra's border/theme conveniences.
4. **Fix NA-hiding in the shared prep** (§22) so it is not a whole-string `str_replace` gated on
   `interactive()` ([tab_classes.R:763-772](../R/tab_classes.R#L763)) — cheap, and removes a correctness
   footgun under knitr.

These levers are **display-layer** (Phase 7/8) and independent of the aggregate-core work; they do not touch
the `tabxplor_fmt` contract.

---

## 24. Phase 3b IMPLEMENTED — Chi2/ANOVA on the vectorised engine + the tidy `test` attribute (2026-07-08)

Implemented 2026-07-08. The mean-table omnibus (the chi2 mirror, Q4/§12) and the chi2 vectorisation land
together; the `chi2` table attribute becomes the tidy **`test`** attribute (§16). Engine in `R/tab-agg.R`
(`agg_chi2()`, `agg_anova()`): every `(subtable × col_var)` is one `table_id`, ALL tables are stacked into
one long `data.table` and tested in ONE grouped pass — O(total cells/groups), independent of the number of
tables (the "many tests of the same kind on different tables" framework). This replaces `tab_chi2()`'s
per-(sub)table `group_split()` + `stats::chisq.test()` loop.

**Chi2 (factor col_vars).** Fully unweighted counts (chi2 stays the §14 exception, Q13/G2). Matches
`stats::chisq.test()` **exactly, incl. the Yates correction on 2×2** — in a 2×2 all four `|O−E|` are equal,
so the per-cell `pmin(0.5, |O−E|)` equals `chisq.test`'s scalar `min(0.5, abs(x−E))`. Empty rows/cols are
dropped before df / Yates (matching the old pre-chisq drop); df on the reduced matrix; a degenerate reduced
table (`df < 1`) yields `pvalue = NA` (the old path returned NA via `possibly()`). `min_e` (smallest expected
count) is stored as a cheap "low expected" flag for the future §16 `!`-glyph mode.

**ANOVA (mean col_vars) — Welch's F (default) + classic F.** Computed **only from per-group summary
statistics** `(n_i, x̄_i, s²_i)` — no microdata scan — so it rolls off the built fmt cells / the moment-sum
aggregate, vectorised over every mean (sub)table. Per §14/Q13: `x̄_i` and `s²_i` are the **weighted** group
estimates (what the cell shows), `n_i` the **unweighted** count. On unweighted data both F's reduce to
`stats::oneway.test()`; on weighted data it is the single-stage §14 approximation (documented, no external
reference). Both F's are cheap from the same summaries → both stored (rows `"F_welch"` / `"F_classic"`);
`options("tabxplor.anova")` (`"welch"` default | `"classic"`) picks the p-value shown.

- **Welch** (`oneway.test(var.equal = FALSE)`): `w_i = n_i/s²_i`, `x̄_w = Σ w_i x̄_i / Σ w_i`,
  `F = [Σ w_i(x̄_i−x̄_w)²/(k−1)] / [1 + (2(k−2)/(k²−1))·Σ(1−w_i/Σw)²/(n_i−1)]`, `df1 = k−1`,
  `df2 = (k²−1)/(3·Σ(1−w_i/Σw)²/(n_i−1))` (Welch–Satterthwaite), `p = 1 − F_{df1,df2}(F)`.
- **Classic** (`oneway.test(var.equal = TRUE)` / `aov`): `SSB = Σ n_i(x̄_i−x̄)²`, `SSW = Σ(n_i−1)s²_i`,
  `x̄ = Σ n_i x̄_i / N`, `N = Σ n_i`, `F = (SSB/(k−1))/(SSW/(N−k))`, `df1 = k−1`, `df2 = N−k`.
- Domain: groups with `n_i < 2`, non-finite `s²_i`, or `s²_i = 0` are dropped (the `oneway.test` domain);
  `k < 2` → `NA`. Numeric col_vars now get a whole-table test (previously skipped for all-means tables).

**Documented asymmetry (Q13).** chi2 is fully unweighted (counts *and* n — `chisq.test` parity, G2), the F
follows §14 (weighted estimates + unweighted n). On weighted tables the chi2 can disagree with the visible
weighted percentages while the F tests the numbers displayed — the price of exact chi2 legacy parity.

**The `test` attribute (§16, tidy).** One row per `(subtable × col_var × test-type)`, columns
`[tab_vars…] row_var col_var test statistic df1 df2 pvalue n variance min_e`. Adding a future test = adding
rows, never a schema change. Read with `get_test()`, which **falls back to the old `chi2` attribute** for
older objects; `get_chi2()` is a kept working alias; the low-level `new_tab(chi2 = )` argument is
soft-deprecated → maps to `test`; `new_test_tibble()` is the empty placeholder. This **softens the §17
accepted break**: `attr(x, "chi2")` returns NULL, but the accessor path keeps working.

**Contrib only when needed.** The per-cell contribution write (`ctr`/`var`, the kept `var_contrib`
machinery) now runs **only when `color == "contrib"`** (`calc = "p"` on the common path) — the old code
computed it on every call. Non-contrib factor tables' `var`/`ctr` become `NA` (conscious golden change; the
contrib path is byte-identical — `f_color_contrib` unchanged). This was the real cost the user flagged; the
"reuse the unweighted chi2 intermediates for the weighted contrib" micro-opt is deferred (contrib is now off
the common path entirely).

**`add_n = TRUE` fixed.** The test drops reserved add_n/add_pct rows (`row_var` "n" / "row_pct") and
`all_col_vars` columns, so `tab_chi2()` on a table already carrying them is not corrupted (the pipeline runs
the test before add_n, so this only mattered for a manual chi2 on a built table).

**Display.** `tab_pvalue_lines()` bakes the p-value row from the tidy attribute — **now for means too**
(F p-value); factor rows byte-identical (`_snaps` unchanged). `print_chi2()` renders the tidy attribute
(chi2 + F) as a readable colored block (mainly for tables that keep the attribute; the pipeline still bakes
rows and drops it, per the "rendered-as-row-for-now" §16 choice — the maintainer opted to keep body rows for
now, recovering the attribute-rendered block later).

Verified against `stats::chisq.test()` (incl. Yates) and `stats::oneway.test(var.equal = FALSE/TRUE)` in
`tests/testthat/test-calculations.R` (statistic, both dfs, and p to floating-point). Golden regenerated
(attr rename + var/ctr on non-contrib). **Suite green (950).** **Perf: chi2 ~2.5× faster** (gss_cat 9-tab
2.60 → 1.03 s chi2 share; whole call 3.07 → 1.48 s — `dev/benchmarks/results_1.4.0/phase3b_chi2_anova.txt`);
the tidy rewrite also fixed a pre-existing `tab_pvalue_lines` crash when a col_var name overlapped a row_var
name.

**Deferred:** the `tab_ci()` field-based simplification → Phase 4 (§20 placement); the
`tab_num(..., <tab_vars>, ci="cell")` grouping-set crash → FIXED Phase 6e (golden-locked; hardened 7d-i);
the φ² table-variance column populated in contrib mode; the `!`-per-cell weak-test glyph mode (§16).

Sources: Welch's ANOVA from group means/variances/sizes + the Satterthwaite denominator df — Welch (1951);
the `stats::oneway.test` formulation. See also the Sources list below.

---

## 25. Phase 4 IMPLEMENTED — `tab_counts()` via the `.fine` seam (supersedes the factor-path extraction)

Implemented 2026-07-08. The from-the-middle constructor `tab_counts()` (`R/tab-counts.R`, exported) builds a
`tabxplor_tab` from already-aggregated counts, **byte-identical** to the microdata `tab()` (locked in
`test-counts-parity.R`: long / wide / table / xtabs / matrix / freq+N × pct × chi2 × ci × weighted ×
tab_vars, plus a `tidyr::uncount()` oracle).

### The mechanism decision — reuse the existing `.fine` entry, do NOT extract the factor path

The roadmap's Phase 4 planned a ~600-line "factor-path reorg" (extract `tab_plain()`'s post-dcast pipeline
into shared `wide_*`/`fmt_assemble_factor` helpers) with `as_tab_counts()` as its second consumer. During
implementation the maintainer chose a lower-risk, less-code path (empirically proven byte-identical before
building): **`tab_plain()` already has a from-the-middle seam — its `.fine` pre-aggregate parameter** (the
opt-in scan-fusion path, `tab.R:2371-2379`, locked by `test-fuse-parity.R`). So `tab_counts()`:

1. `tab_counts_reshape()` — normalises any input SHAPE to canonical long tidy counts (all shape detection
   here): `table`/`xtabs`/`matrix` melt via `as.data.frame.table` (bare matrix → `as.table` first); wide
   `data.frame` via `pivot_longer(cols)`; freq+N via largest-remainder reconstruction; long tidy as-is.
2. `tab_counts_normalize()` — aggregates to the keyed `.fine` shape `[tab_vars…, row_var, col_var, n, (wn)]`
   (integer `n`, double `wn`), **drops `n==0` cells** so the aggregate is structurally identical to
   microdata's `.N`-per-observed-key (empty cells are recreated by `dcast(fill = 0)`; this is what drops
   `gss_cat`'s unused "Not applicable" race level and empty tab_var×row_var combos that `table()`/
   `pivot_wider()` surface but microdata never does). Sets `weighted` and `has_real_n`.
3. Routes through `tab_plain(data = <skeleton>, …, wt = <flag>, .fine = fine)` (the skeleton only serves the
   tidy-select of `tab_vars`; `wt` is a weighted/unweighted flag, never evaluated as a column on the `.fine`
   path), then the SAME finalize `tab_many()` applies: `tab_chi2` → `tab_ci` → `tab_add_n_pct` → rewrap with
   the `test` attribute → `tab_pvalue_lines`. **No math is forked** — the keystone's "reuse the core, don't
   fork" is met by `.fine` routing, not by the extraction.

The **only** extraction done: `tab_add_n_pct(tabs_text, add_n, add_pct)` — the `add_n`/`add_pct` block moved
verbatim out of `tab_many()` (`tab.R` ~L1239-1413) into a shared internal helper so `tab_counts()` and
`tab_many()` share one implementation (add_n's `add_n=TRUE` default was the only finalize gap).

### Inference at the boundary (§14 + the freq sharpening)

- **Weighted (§14):** the user gives the real unweighted count in `counts` and the weighted count in
  `wt_counts`. `.fine`'s `n` (unweighted) drives `tot_n`/CI/chi2; `wn` drives pct/estimates → weighted
  estimate + unweighted n, no special-casing (the same fields `tab_ci`/`tab_chi2` already read).
- **Base-less input** (counts not whole numbers — a `has_real_n = FALSE` test on integrality): frequency-only
  or weighted-only. pct/diff/colors still render; CI/chi2 disabled with a `cli::cli_warn`.
- **freq + real unweighted base N** (`input="pct"`, `base=`): CI is **exact** (proportion CI needs only
  `(p, n)`, fed as `(freq, N)`); chi2 uses the largest-remainder integer counts (exact when the frequencies
  are precise — verified: 2-decimal `gss_cat` reconstructs the exact table). **No warning.** This sharpens
  the roadmap's "warn/disable on frequency-only input": only *base-less* input disables inference; freq **with
  a real base** is a first-class, fully-valid input. Documented assumption (not a warning): `N` must be the
  real unweighted sample size, not a weighted/population figure.

### CI-placement fold — moot / deferred to Phase 6

Because `tab_counts()` reuses `tab_ci()` directly, there is no third CI call site to fold; the CI *math* is
already unified in `R/tab-agg.R` (Phase 3a). The clean "one CI transform in the shared core" end state lands
with the Phase 6 `tab()`→`tab_many()` merge, exactly as §20 splits the fold across Phase 4 and Phase 6.

### Not done (out of scope / deferred)

Non-integer-count auto-detection *inside* `tab()` (rejected — `tab()` stays microdata-only, user's choice);
the vignette/README `tab_counts()` example (before-release doc pass); empirical-OR on counts (rides the
tab_logit phase). Two pre-existing latent `tab_plain()` warnings cleaned up in passing (guarded
`tabs[, "wn"/"n" := NULL]` on a missing column — output-invariant, golden green).

---

## 26. Phase 6b RESEARCH — parallelising tab()/jmvtab over row_vars (2026-07-09)

**Verdict: a substantial, reliable win for tabxplor's PRIMARY workflow — worth building as a Suggests-only
opt-in; NOT a forced default, and NOT for big data / live jmvtab.** Parallelising the row_var/pair axis over
*many tables on a typical survey (10k–60k rows)* — i.e. the core "export dozens of colored exploratory tables
from one survey" use case — delivers **~2.5–3.3× at W=4** (commodity / university hardware) and **~4× at
W=8**, **byte-identical**, with **negligible setup (~1 s) and memory**, and it **wins even on a *fresh* call**
(§Confirmation). This is a real gain, not the marginal curiosity a first read of the 8M numbers suggested —
the 8M df is the *worst* case, not the target. It stays **opt-in** because it is neutral-to-negative *outside*
that regime: ≈break-even-to-loss on multi-million-row data (memory-bandwidth wall + W×df transfer), a loss for
few tables or fresh big-df calls, unusable via future.apply, and pointless for *live* jmvtab (the aggregate is
cached — nothing O(N) left to parallelise). Crucially, for the **batch export** workflow the Phase 7c cache
does **not** overlap (each table is built once, not re-toggled), so parallelism is **additive** there, not
redundant with the core refactor. Grounded PoC below; scripts `dev/benchmarks/parallel_poc_micro.R`
(Layer A, mechanics) + `parallel_poc_tab.R` (Layer B, real tables) + `parallel_poc_survey.R` (survey-range
confirmation) + `parallel_poc_mirai_dispatcher.R`; runs in
`dev/benchmarks/results_1.4.0/phase6b_{micro,tab,survey,mirai_dispatcher}.txt`.

### Method / caveats

Ryzen 5800X (**8 physical / 16 logical**; `data.table` default = **8 threads**), Windows 11 (**no `fork()`**
→ every worker is a separate process; the df must be serialized to it), R 4.5.1. Backends: **mirai 2.7.1**,
base **parallel** (PSOCK), **future.apply 1.20**, vs sequential `purrr::pmap`. Datasets: `big_df.rds`
(**8M rows, 336 MB in-memory**) and `forcats::gss_cat` (**21 k rows, 1 MB**). Workers `setDTthreads(1)`; df
pre-loaded once into persistent workers; `setup_s` (worker spawn + transfer + `load_all`) reported SEPARATELY
from `batch_s`, because the verdict differs for a fresh call vs reused workers. 2–3 reps (noisy at the small
end). *Env note:* mirai needs `nanonext ≥ 1.9.0`; the machine's main-lib `nanonext` (1.8.0) is DLL-locked by
a running btw/MCP session, so the PoC installed `nanonext 1.10.0 + mirai` into a temp lib and prepended it
via `R_LIBS_USER` (inherited by daemons). The seam parallelised: `tab_build()`'s outer
`purrr::pmap(list(row_vars, …))` — [tab.R:1304](../R/tab.R#L1304) / [:1379](../R/tab.R#L1379) /
[:1440](../R/tab.R#L1440); the PoC parallelises at (row_var × col_var) **pair** grain (one `tab()` per pair)
to get ≥12 independent units.

### Layer A — mechanics (pure grouped scan, 8M, batch of 16), `phase6b_micro.txt`

- **data.table's own threading barely scales the scan** — it is **memory-bandwidth-bound**: 1 thread 0.16 s
  → 2t **1.14×**, 4t **1.45×**, 8t **1.45×**, 12t **1.60×**. So "rely on data.table" gives ~1.5× on the
  scan *no matter the core count*, and there is little headroom left for either more threads or more
  processes.
- **Process-parallel batch (16 scans) peaks at ~1.7×** then *degrades* (same memory wall): mirai
  0.92/1.72/1.72/1.45× at W=2/4/8/12; parallel 0.88/1.45/1.78/1.66×. **future.apply loses** (0.69→0.34×) —
  it re-sends the 336 MB df as a global on *every* call.
- **Transfer (setup) is the killer**: 6.8 s (mirai W=8) → 17 s (parallel W=12) to ship the df — **4–10× the
  entire 1.74 s sequential batch**. **Oversubscription** (A5): W=8×DT=1 0.88 s vs W=8×DT=8 (64 logical)
  0.94 s — `setDTthreads(1)` in workers is the correct, mildly-better rule (muted only because the scan is
  already bandwidth-capped). **Memory** (A6): W×df resident — W=8 ≈ 3.0 GB, W=12 ≈ 4.3 GB for this df.

### Layer B — real colored + chi2 tables (`pct="row", color="diff", chi2=TRUE`), `phase6b_tab.txt`

**Byte-identity: 0 / 34 tables differ** from sequential (big 16 + fewtab 2 + small 16), workers running the
same dev source via `devtools::load_all()` → parallelising the pair axis is output-safe.

| dataset (seq batch)        | backend  | best speedup | at W | setup    | note                                   |
|----------------------------|----------|--------------|------|----------|----------------------------------------|
| big_8M, 16 tab (3.33 s)    | parallel | **2.49×**    | 8    | 11.2 s   | scales W2→8 (1.05→2.49×), dips at 12    |
| big_8M, 16 tab             | mirai    | 1.51×        | 12   | 8.9 s    | oddly **flat ~1.2×** across W           |
| big_8M, 16 tab             | future   | —            | —    | —        | **errors**: globals > 500 MB, resent    |
| big_8M, **2** tab (0.41 s) | both     | **0.5–0.6×** | 8    | 6–11 s   | **loss** even batch-only; setup 15–27×  |
| small_gss, 16 tab (2.55 s) | mirai    | **3.49×**    | 4    | **1.0 s**| net win even *fresh-call* (1.70 s)      |
| small_gss, 16 tab          | parallel | 2.56×        | 8    | 2.2 s    | scales cleanly                          |

- **The small df is the *sweet spot*, the 8M df the *worst case*** — the inverse of the naïve prior. Reason:
  per-table cost = a bandwidth-bound O(N) scan **+ an N-independent O(cells) fmt/chi2/vctrs overhead**
  (~0.16–0.19 s, the §23 finding). On 21 k rows the scan is ~0 so that CPU-bound overhead *is* the whole
  cost → it parallelises near-linearly and transfer is trivial (1 MB). On 8M rows the scan is large and
  bandwidth-capped (caps speedup) and the 336 MB × W transfer makes a *fresh* call a net loss. Confirmed by
  DT-threading on the full build: 1.19× (big) vs ~1.0× (small) — the scan is **not** tab()'s bottleneck.
- **`setup ≫ batch` on big df** (transfer 6–16 s vs a 3.3 s batch) → a fresh parallel call is a **3–4× net
  loss**; only **persistent workers reused across many builds** amortise it (break-even ~6 batches at W=8).
  On small df setup is ~1 s → wins immediately.
- **No universal backend.** mirai best on small, flat on big; parallel best on big, good on small; future
  unusable. mirai's big-df plateau is **not** the dispatcher (`dispatcher=FALSE` → same ~1.2×,
  `phase6b_mirai_dispatcher.txt`) and **not** thread-oversubscription (`getDTthreads()==1` verified in
  daemons) — an unexplained nanonext/scheduling divergence. Betting the package on one backend is unsafe →
  reinforces opt-in, not default.

### Confirmation — the survey sweet spot (10k–60k rows), the PRIMARY use case, `phase6b_survey.txt`

16 colored+chi2 tables at commodity worker counts; the target regime for "export dozens of exploratory
tables from one survey". **Byte-identical (0/16 every N).**

| N (rows) | seq(DT=8) | mirai W=4 | parallel W=4 | parallel W=8 | mirai W=4 setup | fresh-call W=4      |
|----------|-----------|-----------|--------------|--------------|-----------------|--------------------|
| 10 000   | 2.39 s    | **3.32×** | 2.99×        | **3.79×**    | 0.98 s          | wins (1.70 vs 2.39)|
| 30 000   | 2.49 s    | 2.65×     | 2.47×        | **4.22×**    | 0.97 s          | wins (1.91 vs 2.49)|
| 60 000   | 2.48 s    | 2.51×     | 2.70×        | **4.07×**    | 0.97 s          | wins (1.96 vs 2.48)|

- **The sequential batch is ~2.5 s FLAT from 10k→60k** — direct proof the per-table cost is the
  **N-independent O(cells) fmt/chi2 work**, not the scan (a 60k scan is ~nothing). That is precisely what
  parallelises cleanly, so the win is stable across the whole survey range, not a 21k artifact.
- **~2.5–3.3× at W=4** (a realistic core count on a low-end university PC) and **~4× at W=8**; **setup ~1 s**
  and **memory ~0** (0.4–5.7 MB × W) → the transfer/memory objections that sink the 8M case **vanish here**.
- **Wins even on a *fresh* call** (W=4 fresh 1.7–2.0 s < the ~2.5 s sequential batch) — no persistent-worker
  reuse required, unlike big df. This is the substantial, reliable gain for tabxplor's core workflow.

### Answers to the specific questions

- **(a) Does row_var parallelism help batch `tab()`?** **Yes — substantially (~2.5–4×) for the primary
  workflow**: many tables on a 10k–60k survey (§Confirmation), where it wins even fresh. The crossover is
  **df size + table count**, not cores: the sweet spot is small/medium df × many tables; ≈break-even-to-loss
  on multi-million rows (bandwidth wall + transfer), always a loss for few tables.
- **(b) Good package?** For this workload base **`parallel`** (zero new dep, scaled best on big, PSOCK
  persistent cluster) or **mirai** (best on small, lightest dep = only `nanonext`); **not** future.apply
  (per-call global resend). Any adoption must be **Suggests-only + opt-in**, mirroring the scan-fusion infra.
- **(c) Does it fight data.table's threading?** They compete for the same memory bandwidth; the rule is **one
  level** — `setDTthreads(1)` in workers (nesting = 64 logical threads, slightly worse). "Making DT threads
  bear fruit across many tables" does **not** work: DT threads help *within* one scan (~1.5×, bandwidth-capped),
  never *across* independent tables — that needs process-level parallelism, which then wants DT threads off.
- **(d) Memory / "good level without many df copies"?** Each process holds a **full df copy** (W×df: 3–4 GB
  at W=8–12 for the 336 MB df) — real cost on big data. The only cheap level is **pre-load once into
  persistent workers**; per-task df sends (future's model) are fatal.
- **(e) jmvtab live?** **No.** Per interaction the aggregate is cached (Phase 7c tiers 1–2) → nothing O(N) to
  parallelise; residual cost is the O(colored-columns) display paint (already vectorised, Phase 5). The only
  parallelisable jmvtab moment is the *first, uncached, many-var* build — i.e. the batch case above — and a
  live UI building one table per keystroke is the **few-tables loss** regime. Caching, not parallelism, is
  the live lever.
- **(f) Both at once, same results, shared functions?** Yes structurally: a single internal `tab_pmap()`
  dispatching the three `pmap(row_vars,…)` sites would serve both `tab()` batch and jmvtab first-build with
  **byte-identical** output — but per (a)/(e) it earns its keep only in the many-tables-small/medium-df batch
  export path, not live.

### Recommended opt-in shape (worth building for the survey batch-export workflow)

A Suggests-only, **opt-in** `options(tabxplor.parallel=)` / `tab(..., parallel=)` gating an internal
`tab_pmap()` at the `tab_build()` row_var/pair seam: a persistent worker pool (base-`parallel` — best on the
sweet spot at W=8 (~4×), zero new dep; or mirai — best at W=4 (~3.3×), lightest dep), `setDTthreads(1)` in
workers, df pre-loaded once, **byte-identical** sequential fallback. Primary audience: "export dozens of
colored exploratory tables from one survey (10k–60k rows)" — a **substantial ~2.5–4×** win there (§Confirmation),
robust even for a single fresh call, negligible memory. **Preconditions:** (i) it must not slow the current
single-threaded default path (so — opt-in, and skip parallel dispatch below a table-count / row-count
threshold where setup > gain, e.g. few tables); (ii) worker df availability solved by pre-load-once (never
per-task); (iii) land **after** the Phase 2 aggregate-core / Phase 7c cache — they lower the per-table fmt
overhead this parallelises (may shrink the absolute win, but the batch-export path does **not** overlap the
cache, so the gain persists). Not scheduled for this pass; no code changed
in this pass.

---

## 27. Phase 7e PROFILING — the O(cells) fmt build dominates at real-world scale (2026-07-10)

Grounded in the committed jmvtab benchmarks (`benchmark_jmvtab_ops()` / `benchmark_jmvtab_big_ops()` in
`tests/testthat/helper-benchmark.R`; baselines `jmvtab_benchmark_baseline.csv` +
`jmvtab_big_benchmark_baseline.csv`). Full `gss_cat` (21 483 rows), **warm cache**, `pct = "row"`,
`color = "diff"`, tooltips off. The tier-1/tier-2 cache works: the O(N) count/moment scan and the
chi2/ANOVA are reused, so the remaining floor is the **O(cells) tier-3/4 work** — `pct`/`diff`/CI +
the `fmt`-record assembly + colour — which the design **recomputes every run** (§ cache design: "fmt is
sub-ms, not worth caching"). At real-world scale that assumption breaks.

| interaction (warm) | small (1 row_var × 3 col_vars) | **big (3 row_vars × 3 col_vars)** |
|--------------------|-------------------------------|-----------------------------------|
| build (jmvtab_build) | ~0.23 s | **~0.95–1.15 s** |
| render (tab_kable, tooltips off) | ~0.28 s | ~0.60 s |
| → dominant cost | **render** | **build (the fmt assembly)** |
| total (R) | ~0.5 s | ~1.5 s (≈ 2 s in the Jamovi UI) |

- **The bottleneck FLIPS with table size.** On a single-row_var table the ~0.28 s render dominates; on a
  real-world table-of-tables (≈ 9 pair-tables × thousands of cells) the ~0.95 s **build** dominates.
- **Pure-display toggles are not free.** A `digits` change (tier-4 only, conceptually) still costs
  ~0.94 s on the big table because `jmvtab_build` re-runs the entire tier-3/4 pipeline — nothing below
  the aggregate is cached. Tier-1/tier-2 caching alone cannot make the big table instant.

**Two levers to reach "instant" on real tables (Phase 8 + the new Phase 7h):**
1. **Render rewrite** (CSS-only `<table>` builder, no kableExtra) → the ~0.6 s render. `format()` +
   colour-codes are only ~30 ms, so the render itself could drop to tens of ms.
2. **fmt-build optimisation (Phase 7h)** → the ~0.95 s build: either a faster `vctrs::new_rcrd`
   assembly of the `fmt` cells (the likely hot spot at thousands of cells), or **caching tier-3/4 for
   display-only toggles** (the design deferred this as "too cheap to cache"; this benchmark disproves it
   at scale). The two committed baselines track both levers precisely.

**Already applied (Phase 7e):** `tooltips = FALSE` on the Jamovi render (the hover JS is inert in Jamovi
and roughly doubled the render time: 570 → 250 ms on the small table).

---

## Sources (statistics)

- Binomial proportion CI (Wald / Wilson / Agresti-Coull asymmetry): <https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval>
- Odds-ratio CI symmetric on log scale, asymmetric after exp: <https://en.wikipedia.org/wiki/Odds_ratio> ; <https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module8-CategoricalData/PH717-Module8-CategoricalData5.html>
- Newcombe/Agresti-Coull difference-of-proportions coverage asymmetry: Newcombe (1998), summarised in the Wikipedia article above.

---

## How the decisions cohere

The decisions are **one move made in several places**: push every quantity a cell needs onto the
cell itself, so the table becomes self-describing and the step-by-step pipeline collapses onto a single
aggregate-core. The review-session-2 additions (§14–§17) do not alter that move — they **pin the inference
policy** the self-sufficient cells carry (weighted estimate + unweighted `n`, §14; interval ⇄ stars
duality, §15; where the test results live, §16) and **inventory the small accepted breaks** (§17).
Review session 3 closed the last color-scale gap (§18 — sd-standardized numeric diff-coloring), the
`test`-attribute rename (§16), the stars-vs-explicit-method rule (§15), chi2 parity (G2 — match
`chisq.test()` incl. Yates), and the serialization question (a non-issue, §17). Review session 4 pinned
the remaining inference details — the omnibus F follows §14 (Q13), the mean quantile becomes the second
swap-under-stars pair (Q14), per-column tests are `test`-tibble rows rather than a ninth attribute (Q15),
`ref2="first"` stays (Q16) — plus the uncorrected-score pin (§15), the D3 interim color routing
(§ *Phasing*), and the §10 table-level display pre-pass.

- **Self-sufficient cells.** `tot_n` (§2) + the recovered `tot_wn` (§11) give each cell its own base; the
  `ci_inf`/`ci_sup` bounds (§1) and `pvalue` (§12) give it its own interval and significance; `diff` vs
  `ratio` (§3) give it both comparison shapes. Together these **retire `detect_totcols`** and let the
  single total column (§6) be a pure **display anchor** — a range at display when col_var bases differ
  (§10) — never a calculation input.
- **One computation, fewer knobs.** Globalising the row_var axis (§5), the named-vector `ref` (§4), and
  the always-one-total-column rule (§6) drop the divergent-per-row_var flexibility real analysis never
  used. That is precisely what lets `tab()`/`tab_many()` fuse onto one core, and what lets the
  from-the-middle counts entry and the Jamovi cache **reuse that same core** rather than fork the math.
- **One display contract, many surfaces.** The bounds / `ratio` / `pvalue` fields, the output-shape rule
  (§13), and the exporter base+list methods (§8) all read the same per-cell fields, so console, kable, md
  and Excel stay in parity — which is why the Excel *engine* swap (Phase 9) is just a backend change
  behind an unchanged contract.

The **one combined field pass** (§9 — 15 → 18 fields) is the keystone that unlocks all of this, which is
why it comes first — and why the still-open points must be settled as their phase lands (see *Status &
open items*): **G1** (the sufficient-statistics aggregate, now also carrying `Σw²` for §14) and **C2** (the
`tot_wn` edge) at the core; **D2** (the §5 internal-vs-arg-surface split), **U4** (the col%-reference
side), **S3** (tab()'s NA semantics) and **S4** (`tab_spread`/`tab_compact` fate) as the later phases
reach them. The
public API
(user-facing functions, their arguments, the `tabxplor_fmt` fields) stays retro-compatible throughout;
only the internals are re-cut. That is the whole of 1.4.0.

