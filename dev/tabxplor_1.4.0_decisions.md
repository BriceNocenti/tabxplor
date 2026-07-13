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
  base+list methods; Excel engine → Phase 11), §10 (total-column base range), §13 (output-shape table),
  §18 (numeric diff-color scale — sd-standardized), §21 (exporter phasing 7 vs 9 + the backend seam),
  §22 (exporter feature parity — what to unify / extend / keep exporter-specific), §23 (tab_kable
  performance profile — empirical).
- **Inference policy** (review sessions 2–4): §14 (weighted estimate + unweighted `n`; omnibus F
  included, Q13), §15 (CI/stars duality — score ⇄ Newcombe, uncorrected pair; mean z ⇄ Welch-t, Q14),
  §16 (test-result placement — `test` tibble rows incl. per-column, Q15 + display future).
- **Retro-compat**: §17 (the consolidated accepted-breaks inventory).
- **Internal architecture & performance**: §23 (`tab_kable` profile), §26 (parallel research),
  §27 (O(cells) fmt-build profiling), §28 (parallel dispatch implemented), §29 (Phase 9 — the
  tab()/tab_build simplification: outer-map row axis + leaf public-wrapper/core split; clarity not speed).

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

**Decision** (Phase 10): keep `pct="col"` **single-row_var** as-is. The col%-multi-row_var path is a
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

**`tab_transpose()`** (a stub the maintainer added to `R/tab.R` — **fully commented-out and NOT exported**
at [tab.R:2133-2155](../R/tab.R#L2133); single-total-row/col only, unqualified verbs; a *clean slate*, not
a live broken function — the earlier "@export'ed at tab.R:1773" note was stale) is to be **finished,
documented, and possibly generalised** (tab_vars?) at Phase 10 — it is the mechanism for the above. Do not
wire it in before Phase 10. (Design: `dev/tabxplor_phase10_exporters.md` §8.)

**compact + tab_vars**: deferred. Merging tables that carry tab_vars needs compound
`group_by(tab_vars, row_var)`, interleaving row_vars within each tab_var block, per-(tab_var × row_var)
reference re-scoping, chi2 alignment, and two-level print/kable rendering
([tab_classes.R:969-975](../R/tab_classes.R#L969)) — revisit during Phase 10. Until then, tables with
tab_vars stay a list/grouped structure regardless of `output_list`.

---

## 8. Exporters — base method + list method

**Decision** (Phase 10): every exporter (`tab_xl`, `tab_kable`, `tab_md`, `tab_plot`) has (a) a base
method for a single `tabxplor_tab`, and (b) a method for a **list of tables** that renders them
**one-after-another, not merged** (kable: an HTML container holding several tables; xl: sheets/blocks;
md: sequential). This is the export side of the "different tables → list() → export" escape hatch in § 5.

**Excel engine (openxlsx → openxlsx2) — isolated to Phase 11 (follow-up decision, 2026-07-07).** Phase 10 builds the shared
exporter-prep helper and the base+list `tab_xl` methods on the **current openxlsx v1** engine. The engine
swap to **openxlsx2** (common styles created once; optional conditional formatting) is a full dependency
migration with its own parity risk, so it is **pulled out into its own Phase 11** (may ship in a 1.4.x
follow-up) — the exporter-prep unification must not be entangled with it. Precondition: `test-export-parity.R`
green on openxlsx v1, so Phase 11 verifies byte-for-byte against a known-good baseline. See CLAUDE.md Phase 11.

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
(Phase 3 for the fmt/print side; the exporters mirror it in Phase 10.)

Implementation caveat (review session 4): the range is **cross-column** information —
`format.tabxplor_fmt()` formats one column at a time and cannot see sibling columns — so the `[min;max]`
must be computed by a **table-level display pre-pass** (print prep / the Phase 10 shared exporter-prep
helper) and injected into the total column's rendering, never inside the per-column format method.
`tab_xl` corollary: a `[min;max]` cell is text, not a number — either write it as a text cell in the
total column or fall back to Option C (`min` + subtext note) for Excel; decide with the exporter-prep
helper at Phase 10.

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
- **tab_xl stars deferred to Phase 10** (the exporter-unification phase, then openxlsx2 in Phase 11) —
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

## 21. Exporter phasing — Phase 10 (openxlsx v1 prep) vs Phase 11 (openxlsx2): keep split, add a backend seam

> **SUPERSEDED / RESOLVED (Phase 10h, 2026-07-12).** The split held (10g stayed on openxlsx v1; the
> swap ran as its own Phase 10h). But the maintainer chose a **full clean migration to openxlsx2**
> over the dual-backend closure seam this section designed: `tab_xl()` was rewritten on openxlsx2
> only, `openxlsx` was dropped from Suggests, and there is a **single-engine** backend module
> `R/tab-xl-backend.R` (thin `xlb_*` wrappers + pure range coalescers), not an `xl_backend_openxlsx1/2`
> pair. The perf-premise caveat below was borne out in spirit — the openxlsx2 win is the
> **shared-style + largest-range** application (coalesced multi-area `dims`), not raw speed; hard cell
> styles were kept (conditional formatting deferred as experimental). The **styles-manager write
> optimization** then landed (2026-07-12): each cell's full style is PRECOMPOSED (create_font/fill/
> border + create_cell_style) and applied by id with `set_cell_style` -- ~1.4-1.8x faster than the
> per-aspect `wb_add_*` passes (single 0.34->0.24 s, 12 tables 5.5->3.0 s). `parallel=` was **dropped**
> from `tab_xl` (only ~1.09x, write-bound/Amdahl-capped); a parallel-write-merge via
> `wb_clone_worksheet(from=)` was studied (works via a save->load->clone border workaround, ~2.5-3x
> batch-only) but dominated by the styles-manager win. The byte-identity oracle here is
> relaxed to the value/code-path parity (`test-export-parity.R` + numFmt-code lock) + visual review,
> per the 10g "white elephant" waiver. Full record: CLAUDE.md § Phase 10h + `dev/tabxplor_phase10_exporters.md` (Status).

The maintainer's question (2026-07-08): the Phase 10 `tab_xl()` rewrite is *already* a big restructure (today's
`tab_xl` is **list-first — a bare df is wrapped to a one-element list at [tab_xl.R:91](../R/tab_xl.R#L91) and
the entire body is `purrr::map`/`pwalk`; there is no single-tab path**), so should the openxlsx→openxlsx2
engine swap ride along with it (§8 puts it in a separate Phase 11), or stay split?

### Decision — keep §8's split (Phase 10 on openxlsx v1, Phase 11 = openxlsx2), but factor a **backend seam** in Phase 10

Two *different* risk surfaces, and combining them forfeits the one thing that makes the swap safe — a
byte-verified v1 baseline:

- **Phase 10 risk = parity of the display bypass.** `tab_xl` does **not** go through `format.tabxplor_fmt()`
  (Export-Parity WARNING [tab_xl.R:541](../R/tab_xl.R#L541), [1087](../R/tab_xl.R#L1087)): it writes raw
  `get_num()` numbers and rebuilds display via `numfmt()` → Excel number-format codes. Adding stars (§16),
  the label attribute, and the base+list split (§8) all perturb *this* bypass. The regression oracle is
  `test-export-parity.R` (`format` vs the `tab_xl` bypass).
- **Phase 11 risk = engine semantics.** openxlsx2 has a different style model (shared styles created once —
  the real speed lever), a long-reshape write path, and its own conditional-formatting API. The oracle here
  must be **"byte-identical to the Phase-7 openxlsx-v1 output"** — which only exists once Phase 10 is green.

Entangling them means a broken cell can't be attributed to the restructure *or* the engine. So the split
stands. **Refinement:** structure Phase 10's rewrite around a **narrow write/style backend interface** —
`{new_workbook, add_sheet, write_data(numbers), apply_style(cells, fill|font|border|numfmt), freeze_panes,
set_widths/heights, conditional_format, save}` — implemented on openxlsx v1. Then **Phase 11 reimplements
only that ~12-call backend** against openxlsx2, leaving the shared prep, the color→style selection
(`fmt_color_selection` + `select_in_color_style`, already shared with the console) and the sheet/offset
orchestration untouched. This kills the "double-touch every `openxlsx::` call site" objection (the
orchestration is restructured **once**; only the leaf backend is re-pointed) **without** merging the two
parity risks. The live `openxlsx::` surface is small and already enumerated — 13 functions: `createStyle`,
`createWorkbook`, `addWorksheet`, `writeData`, `modifyBaseFont`, `showGridLines`, `freezePane`, `addStyle`,
`conditionalFormatting`, `setColWidths`, `setRowHeights`, `saveWorkbook`, `openXL` — so the seam is cheap to
draw.

### The perf premise for openxlsx2 is weaker than assumed — treat the swap as maintenance-driven, benchmark before committing

The roadmap frames Phase 11 partly as a speed win (shared styles). Grounded caveat: openxlsx2 is **not
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
on **both** engines before committing Phase 11; if openxlsx2 loses, Phase 11 legitimately slips to a 1.4.x
follow-up (or is dropped) — it does not block 1.4.0. This matches §8's "may ship in a 1.4.x follow-up".

Sources: openxlsx2 styled-write slowness — JanMarvin/openxlsx2 issue #356
<https://github.com/JanMarvin/openxlsx2/issues/356> ; "if you need speed go writexl" — discussion #1281
<https://github.com/JanMarvin/openxlsx2/discussions/1281>.

---

## 22. Exporter feature parity — what to unify, what to extend, what stays exporter-specific

Grounded inventory of `tab_xl` ([tab_xl.R](../R/tab_xl.R)) vs `tab_kable`
([tab_classes.R:461](../R/tab_classes.R#L461)) vs `tab_md` ([tab_md.R](../R/tab_md.R)). Legend ✓ has / ✗
lacks / ~ partial.

| Capability                                                      | xl                  | kable                  | md               | Extend to the others?                                 |
|-----------------------------------------------------------------|---------------------|------------------------|------------------|-------------------------------------------------------|
| Cell **colors** (`fmt_color_selection`)                         | ✓ hard cell style   | ✓ inline span          | ✗                | md → short pandoc spans (Phase 10 md item)            |
| text vs background color mode                                   | ✓                   | ✓                      | ✗                | —                                                     |
| **Significance stars**                                          | ✗ bypasses format() | ✓ via `format()`       | ✓ via `format()` | **→ xl (Phase 10, §16)** — mirror into `numfmt`/style |
| **Tooltips / hover extra stats**                                | ✗                   | ✓ (`title=` / popover) | ✗                | keep kable-only (see below)                           |
| **col_var spanning header**                                     | ~ (layout)          | ✗ (borders only)       | ✓ [tab_md.R:227  | **→ kable** via `add_header_above` (readability)      |
| **`n_min` greying** (hide small-n rows/cols)                    | ✓ tab_xl.R:1030     | ✗                      | ✗                | **→ kable & md** (data-quality signal)                |
| **`hide_near_zero` greying**                                    | ✓ tab_xl.R:1311     | ✗                      | ✗                | **→ kable** (grey text); md ~ (marker only)           |
| **`label` attribute** (question text)                           | ✗                   | ✗                      | ✗                | **→ all** (Phase 10 item — header/legend)             |
| clean **NA hiding**                                             | ✓                   | ~ fragile HTML surgery | ✓ (`na=""`)      | **fix kable** in shared prep                          |
| row/col-name **wrapping**                                       | ~                   | ✓ `tab_wrap_text`      | ~ truncate only  | md → wrap not truncate                                |
| freeze / col widths / num-format / sheets / `colnames_rotation` | ✓                   | ✗                      | ✗                | **Excel-only — no meaning** for kable/md              |
| interactivity (popover/JS)                                      | ✗                   | ✓                      | ✗                | **no meaning** for md; xl ~ (cell comment = clutter)  |
| clipboard / plain-text file                                     | ✗                   | ✗                      | ✓                | md/console-shaped — **no meaning** for xl             |
| caption / title                                                 | ✓ `titles`          | ✓ `caption`            | ✗                | md → optional title line                              |

### Decisions

- **Unify (shared prep + shared display path).** The "canonical col_vars → validate → compact" preamble is
  **duplicated four times** (tab_md self-flags it [tab_md.R:47](../R/tab_md.R#L47); also `tab_kable`
  [tab_classes.R:486](../R/tab_classes.R#L486), `tab_compact`, and `tab_xl`'s inline non-compacting variant)
  → one prep helper (Phase 10, §8). **Stars, the `label` attribute, and NA-hiding must live in the shared
  path** so the `tab_xl` bypass stops silently diverging from `format.tabxplor_fmt()`.
- **Extend cross-exporter (meaningful):** stars → `tab_xl` (§16); `n_min`/`hide_near_zero` greying →
  `tab_kable` (grey text) and, as a marker, `tab_md`; the col_var **spanning header** → `tab_kable`
  (`add_header_above`, which it curiously never uses); the `label` attribute → all three. **REFINED
  (Phase 10a, §33): the `label` is a `tab_kable` header TOOLTIP only** — not md/xl/console (avoids clutter;
  the maintainer's choice).
- **Keep exporter-specific (no cross-meaning):** the Excel-engine features (freeze panes, widths, Excel
  number formats, multi-sheet, `colnames_rotation`) are meaningless for HTML/md; **tooltips/popovers** are
  meaningless for static md and add clutter as Excel cell comments, so they stay `tab_kable`-only (and see
  §23 — they are also a real cost); clipboard/plain-text-file is md/console-shaped, not Excel.

---

## 23. `tab_kable()` performance profile — empirical (2026-07-08)

> **STALE ranking (flagged Phase 10a, §33) — re-profile before using the lever order below.** This profile
> pinned ~75 % of the render in `fmt_color_selection`, but that function was **deleted in Phase 5** (the
> engine is now the `findInterval`-based `fmt_color_channels`/`fmt_channel_codes`, 48–1290× faster). Lever 1
> ("cut/cache the color computation") is therefore largely obsolete; the current dominant cost is unknown
> until re-measured on the live engine. The structural remedy that survives is deriving each quantity once
> in the shared prep (Phase 10, `dev/tabxplor_phase10_exporters.md` §1-2). Levers 2-4 (tooltips-off,
> collapse the kableExtra styling chain, NA-hiding in prep) still apply.

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

### Optimization levers (ranked; for Phase 10 light-mode + Phase 7 cache)

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

| dataset (seq batch)        | backend  | best speedup | at W | setup     | note                                   |
|----------------------------|----------|--------------|------|-----------|----------------------------------------|
| big_8M, 16 tab (3.33 s)    | parallel | **2.49×**    | 8    | 11.2 s    | scales W2→8 (1.05→2.49×), dips at 12   |
| big_8M, 16 tab             | mirai    | 1.51×        | 12   | 8.9 s     | oddly **flat ~1.2×** across W          |
| big_8M, 16 tab             | future   | —            | —    | —         | **errors**: globals > 500 MB, resent   |
| big_8M, **2** tab (0.41 s) | both     | **0.5–0.6×** | 8    | 6–11 s    | **loss** even batch-only; setup 15–27× |
| small_gss, 16 tab (2.55 s) | mirai    | **3.49×**    | 4    | **1.0 s** | net win even *fresh-call* (1.70 s)     |
| small_gss, 16 tab          | parallel | 2.56×        | 8    | 2.2 s     | scales cleanly                         |

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
|----------|-----------|-----------|--------------|--------------|-----------------|---------------------|
| 10 000   | 2.39 s    | **3.32×** | 2.99×        | **3.79×**    | 0.98 s          | wins (1.70 vs 2.39) |
| 30 000   | 2.49 s    | 2.65×     | 2.47×        | **4.22×**    | 0.97 s          | wins (1.91 vs 2.49) |
| 60 000   | 2.48 s    | 2.51×     | 2.70×        | **4.07×**    | 0.97 s          | wins (1.96 vs 2.48) |

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

| interaction (warm)               | small (1 row_var × 3 col_vars) | **big (3 row_vars × 3 col_vars)** |
|----------------------------------|--------------------------------|-----------------------------------|
| build (jmvtab_build)             | ~0.23 s                        | **~0.95–1.15 s**                  |
| render (tab_kable, tooltips off) | ~0.28 s                        | ~0.60 s                           |
| → dominant cost                  | **render**                     | **build (the fmt assembly)**      |
| total (R)                        | ~0.5 s                         | ~1.5 s (≈ 2 s in the Jamovi UI)   |

- **The bottleneck FLIPS with table size.** On a single-row_var table the ~0.28 s render dominates; on a
  real-world table-of-tables (≈ 9 pair-tables × thousands of cells) the ~0.95 s **build** dominates.
- **Pure-display toggles are not free.** A `digits` change (tier-4 only, conceptually) still costs
  ~0.94 s on the big table because `jmvtab_build` re-runs the entire tier-3/4 pipeline — nothing below
  the aggregate is cached. Tier-1/tier-2 caching alone cannot make the big table instant.

**Two levers to reach "instant" on real tables (Phase 10 + the new Phase 7h):**
1. **Render rewrite** (CSS-only `<table>` builder, no kableExtra) → the ~0.6 s render. `format()` +
   colour-codes are only ~30 ms, so the render itself could drop to tens of ms.
2. **fmt-build optimisation (Phase 7h)** → the ~0.95 s build: either a faster `vctrs::new_rcrd`
   assembly of the `fmt` cells (the likely hot spot at thousands of cells), or **caching tier-3/4 for
   display-only toggles** (the design deferred this as "too cheap to cache"; this benchmark disproves it
   at scale). The two committed baselines track both levers precisely.

**Already applied (Phase 7e):** `tooltips = FALSE` on the Jamovi render (the hover JS is inert in Jamovi
and roughly doubled the render time: 570 → 250 ms on the small table).

---

## 28. Phase 8 IMPLEMENTED — opt-in parallel dispatch, FULL per-row_var granularity (2026-07-10)

`tab(..., parallel=)` / `tab_many(..., parallel=)`, engine **mirai** (Suggests-only; §26 chose it), NAMED
`"tabxplor"` compute profile. Infra in `R/tab-parallel.R`. Verification: full suite green (no golden regen),
`test-parallel-parity.R` (byte-exact), benchmark `dev/benchmarks/run_parallel.R` → `phase8_survey.txt` +
`phase8_profile.txt`.

### The granularity decision (measured, not assumed)

The first cut (as the §26/roadmap sketch proposed) dispatched only the **fmt BUILD** (`tab_plain`/`tab_num`)
and kept tests + assemble + aggregate serial on main — byte-identical by construction. But on the REAL API
(one `tab()`, many row_vars, few col_vars) it measured only **~1.15x**: the fmt build is ~25-30% of the
work; tests (~31%), assemble (~35%), aggregate (~10%) dominate and stayed serial (`phase8_profile.txt`,
Amdahl). The §26 PoC's 3.3x came from dispatching WHOLE independent per-table builds. So the granularity was
promoted to the **full per-row_var pipeline**.

### Byte-identity ceiling and the total-col fix

An independently-built single-row_var `tab()` was NOT byte-identical to its slice of the integrated
multi-row_var build: the lone total column read `Total_<lastcv>` (multi) vs `Total` (single) — a
cross-row_var coupling in `tab_assemble`'s lone-total rename-back (`length(totnames)==1` counted
OCCURRENCES, so N identical `Total_denom` copies skipped the rename-back). **Fix: `totnames |> unique()`**
(test the DISTINCT name). This is a small **cosmetic golden change** (multi-row_var mixed-col_var total →
`Total`, arguably the correct name) that touched NO existing golden (none covered it; now locked in
`test-tab.R`). It makes per-row_var == integrated-slice for **every na mode** (proto verified 4/4) — the
precondition that makes the dispatch byte-identical.

### Architecture

`tab_build()`: `tab_setup` + `tab_prepare_pop` run ONCE on main — the global `na="drop_all"/"common_base"`
population drop lives there and CANNOT move to a worker. Then, when `parallel` is on and ≥ `parallel_min`
row_vars: ship the prepared `data` once (`everywhere()`), `ctx_slice()` the ctx per row_var (subset the 19
row_var-indexed fields; scalars like `na_num`-under-`drop_all` are recycled, not sliced; `tab_row_names`
recomputed), dispatch `tab_build_one` = `tab_aggregate |> tab_transform |> tab_assemble_tables` per row_var,
then `tab_assemble_output` (merge/pvalue/unwrap) on main. Enabled by splitting `tab_assemble()` →
`tab_assemble_tables()` (per-row_var) + `tab_assemble_output()` (cross-row_var). jmvtab (cache_env) and the
default (parallel off) path take the unchanged serial full-ctx branch; the build-only `tab_pmap` seam inside
`tab_transform` remains but now runs serial (single row_var per worker).

### Result

W=8, 30k rows × 12 row_vars: **~2.15x merged / ~2.44x list**, byte-identical. Below the §26 PoC 3.3x: the
gap is the main-side merge (~0.3x) + returning finished tables (serialization) — overheads the PoC's
ship-once / independent-tables / no-merge measurement excluded. Future lever (deferred): a cross-call
data-ship hash-guard for repeated same-data calls (the primary one-big-call workflow already ships once).
Options `tabxplor.parallel` (FALSE), `tabxplor.parallel_min` (2L); `.onUnload` stops the pool;
`_R_CHECK_LIMIT_CORES_` cap 2.

---

## 29. Phase 9 ANALYSIS — the tab()/tab_build() simplification: it buys clarity, not speed (2026-07-11)

Creative-review pass, no code changed. The roadmap's Phase 9 asks two honest questions: (1) if
`tab_many()` stayed on the vectorised `tab_build()` but `tab()` were rewritten "much simpler from
shared functions", is there room for simplification *and* speed? (2) any final workflow
simplifications? Grounded answer below, with a fresh profile because the architecture has changed a
lot since the 2026-07 profile (§23/§27 predate the carve + Phase 8).

### The fresh profile — where tab()'s time actually goes (2026-07-11)

`tab(gss_cat, 5 row_vars × 3 col_vars, pct = "row", color = "diff", chi2 = TRUE)`, `forcats::gss_cat`
(21 483 rows), median wall time, `devtools::load_all` source. Two grounded decompositions:

**Stage split** (trace accumulators on the five `tab_build()` stages):

| stage                 | s/call      | share     | what it is                                                                                                                        |
|-----------------------|-------------|-----------|-----------------------------------------------------------------------------------------------------------------------------------|
| `tab_setup`           | **0.005**   | **0.2 %** | ALL the arg resolution + the row/col-axis recycling (`pct_vect`, `ref_vect`, `vec_recycle` × nrowvars) + `tab_resolve_settings()` |
| `tab_prepare_pop`     | 0.008       | 0.4 %     | select / na / lump / levels, once on the whole DB                                                                                 |
| `tab_aggregate`       | 0.001       | —         | scan-fusion OFF by default → the raw scans happen inside `tab_transform`                                                          |
| `tab_transform`       | ~0.7        | ~33 %     | `tab_plain`/`tab_num` (scan + **fmt-record build**) + chi2 + ci                                                                   |
| `tab_assemble_tables` | 0.05        | 2 %       | level-drop, add_n, totals, join, wrap                                                                                             |
| `tab_assemble_output` | merge ~0.72 | ~34 %     | `tab_compact()` (merge 5 tables → 1) + p-value lines                                                                              |

**Full-call diffs** (authoritative — no trace overhead):

| call                                                            | median s        |
|-----------------------------------------------------------------|-----------------|
| 1 row_var × 1 col_var (colored+chi2)                            | 0.12            |
| 1 row_var × 5 col_vars                                          | 0.46            |
| 5 row_vars × 3 col_vars, `output_list = TRUE` (**no merge**)    | **1.37**        |
| 5 row_vars × 3 col_vars, default (**merge**)                    | **2.09**        |
| ⇒ the merge (`tab_compact`) alone                               | **0.72 (34 %)** |
| `tab_pvalue_lines` (list or merged)                             | ≈ 0             |
| `format()` on the merged table (print/kable/md only, not build) | 0.10            |

**Rprof self/total** on the full call: `vec_case_when` **40 % total** (72 % of `tab_compact`); the
remainder is `tabxplor_fmt` record reconstruction — `structure` / `new_data_frame` / `list_unchop` /
`vec_restore_dispatch` / `df_list` / `vctrs::field`. `dplyr::case_when`/`if_else` over `fmt` vectors
(modern `if_else` is built on `vec_case_when`) each trigger a full vctrs record ptype2/cast round-trip;
`tab_compact`'s per-column `if_else(is_totrow & !any(is_refrow), as_refrow, .)` ([tab_classes.R:991](../R/tab_classes.R#L991))
pays that ~125 times (5 tables × ~25 fmt columns).

**The one-line reading: argument resolution + the entire row/col-axis vectorisation cost 0.2 %. The
O(cells) `tabxplor_fmt` machinery — the fmt build and the merge — is ~99 %, and it is bound by
`dplyr::case_when`/`if_else` + vctrs record reconstruction, not by control flow.**

### Finding 1 — the row-vectorisation is free at runtime but a real complexity tax

`tab_build()` is vectorised over BOTH axes: col_vars (genuinely used — per-col_var `pct`/`levels`/`digits`)
AND row_vars. But Phase 6 §5 **globalised the row axis at `tab()`'s surface** (OR/pct/color/comp/ci/chi2/ref2
are scalar there); only `ref` (named vector), `totaltab`, `totrow` stay per-row_var, and D2 kept the
*internal* per-row_var threading as "a harmless broadcast". The profile confirms it is harmless for
**speed** (0.2 %). It is not harmless for **complexity**: it forces `pct_vect` (a list-over-row_vars of
vectors-over-col_vars), the twin `ref_vect`, `vec_recycle(·, nrowvars)` on ~10 args
([tab.R:1176-1298](../R/tab.R#L1176)), and a `purrr::map`/`pmap`-over-row_vars inside *every* stage
(`tab_aggregate`, `tab_transform`, `tab_assemble_tables`). It also breeds latent axis-mismatch bugs — a
**live one**: [tab.R:1252](../R/tab.R#L1252) `all(pct == "row" & OR %in% …)` `&`-combines a **col_var-indexed**
`pct` with a **row_var-indexed** `OR`, throwing "longer object length is not a multiple of shorter" on any
multi-axis call (harmless today only because `OR = "no"` and `all()` collapses it).

### Finding 2 — do NOT fork a second `tab()` core; collapse the shared one to an OUTER MAP (Phase 8 already proved it safe)

The roadmap's phrasing ("keep `tab_many` on `tab_build`, rewrite `tab` simpler") invites forking a second,
simpler core for `tab()`. **Reject that** — it re-creates exactly the duplicated math 1.4.0 exists to
delete (the keystone: one core, reuse don't fork). The right move achieves the same simplicity without a
fork: **make the row_var axis a genuine outer `map`, not internal vectorisation.** Resolve the per-row_var
arg-sets ONCE at the top, then map a **scalar-over-row_vars** core over them:

```
tab_build:  prep_once(data)                                   # tab_prepare_pop, once (na/lump/levels)
            args_per_rv <- resolve(...)                       # list of scalar-per-row_var arg-sets
            tabs <- map/pmap(args_per_rv, build_one_table)    # serial map OR mirai — one code path
            assemble_output(tabs)                             # merge / pvalue / unwrap
```

This is **not speculative — Phase 8 already built and golden-locked it.** `tab_build_one()` +
`ctx_slice()` ([tab-parallel.R:262-292](../R/tab-parallel.R#L262)) already run the whole
`aggregate → transform → assemble_tables` pipeline on a **single-row_var** sliced ctx, and
`test-parallel-parity.R` proves per-row_var == integrated-slice **byte-exact** (the §28 total-col
decoupling fix was precisely what closed that gap). So the hard part — proving the row axis is a clean
outer product — is done and locked. Phase 9 only makes that proven structure the SOLE path:

- Pull the `map(row_vars, …)` out of `tab_aggregate` / `tab_transform` / `tab_assemble_tables` into ONE
  outer map in `tab_build`; the stages become scalar-over-row_vars (still vector-over-col_vars).
- Serial and parallel become the SAME dispatch (`purrr::map` vs `mirai_map`) — the current serial-branch /
  parallel-branch split ([tab.R:1069-1085](../R/tab.R#L1069)) collapses.
- `ctx_slice()` + `tabxplor_rowvar_fields` ([tab-parallel.R:249](../R/tab-parallel.R#L249)) DISAPPEAR: you
  build each per-row_var ctx directly instead of slicing a vectorised one (and the "add a new per-row_var
  field here too or it silently broadcasts" footgun goes with them).
- `pct_vect`/`ref_vect` lose a nesting level (per-col_var only); the length-mismatch class of bug
  ([tab.R:1252](../R/tab.R#L1252)) is designed out because the two axes never `&`-combine.
- `tab_many`'s per-row_var vectors (`pct = list(rv1 = …, rv2 = …)`, vectorised `ci`/`chi2`/`comp`) keep
  working with **more** flexibility, not less: they are just how `resolve()` fills `args_per_rv`. `tab()`
  fills it by broadcasting one scalar. Both feed the identical core — no fork.

**Cost/benefit, honest:** runtime ≈ unchanged (removes 0.2 %); the payoff is ~one nesting level and three
in-stage row-loops deleted, `ctx_slice` retired, one live latent bug designed out, and serial≡parallel by
construction. It is a **maintainability + correctness** refactor, medium risk (touches the hot path) but
**de-risked by Phase 8's existing byte-exact parity net**. Do not sell it as a speed win.

### Finding 3 — split each leaf into a public wrapper + a resolved-args internal core (this is the "different internal functions?" the roadmap asks about)

`tab_plain()` (~770 L) and `tab_num()` (~940 L) are exported AND on the `tab_build` hot path, so they carry
a **double life**: full NSE quosure handling + validation + `ref="auto"`/`tot`/`comp` re-resolution for
direct callers ([tab.R:2638](../R/tab.R#L2638), [tab.R:3682](../R/tab.R#L3682)) — all of which `tab_setup`
*already did* when called from `tab_build`. They also carry internal-only args (`.fine`, `.by_table`) on
the public surface, and a redundant second `relabel_levels_in_varnames()` ([tab.R:2676](../R/tab.R#L2676)).
The clean answer to the roadmap's "should we keep internal functions but different ones?" is **yes**: give
each leaf a thin public wrapper (parse + validate + resolve, for direct users) over an internal core
(`plain_core()` / `num_core()`) that assumes **already-resolved scalar settings** and does only the
data.table + fmt work. `tab_build`'s outer map calls the core; the internal args and the double resolution
leave the public surface. Runtime gain is small (the re-resolution is scalar, buried in the 0.2 %); the win
is a single documented internal contract for the core — which is also what makes the Phase 7 jmvtab
cache-injection seam and the Phase 8 worker call clean rather than "call the big exported function with
internal flags".

### Finding 4 — the ONLY real speed lever is the O(cells) fmt machinery, and it is orthogonal to the restructure

Because resolution is 0.2 %, **no restructuring of tab()/tab_many/tab_build moves the needle.** The ~99 %
lives in two O(cells) places, both `vec_case_when`/vctrs-bound:

1. **fmt-record build** (`tab_plain`/`tab_num`, the `pmap_dfc(~ new_fmt(...))` at
   [tab.R:3092](../R/tab.R#L3092) / ~L4231). Phase 7f-1 already hoisted the column-invariants; the residue
   is per-column `new_rcrd` construction over thousands of cells. This is the §27 finding, still true.
2. **the merge** `tab_compact()` — 0.72 s / 34 % of the default call, dominated by the per-column
   `if_else`-over-fmt ([tab_classes.R:991](../R/tab_classes.R#L991)) whose modern-dplyr `vec_case_when`
   detonates a full record ptype2/cast per column. Candidates: replace the `if_else`-over-fmt with a
   base-R masked assignment on the underlying fields (no per-column vctrs round-trip), and bind via one
   `vctrs::vec_rbind` of the already-aligned columns rather than `imap_dfr`.

These are **Phase 7f / Phase 10** territory (fmt build + exporter/display), NOT Phase 9's restructure —
but the profile pins them as the real budget, and the Finding-2 outer-map makes them easier to attack
(one scalar core to optimize, one merge site). A broader lever, if ever the fmt display/merge cost must
drop by an order of magnitude: audit the ~19 `dplyr::case_when` sites in `fmt_class.R` on hot display
paths and replace with base `switch`/vectorised indexing — `case_when` over `fmt` is the single most
expensive idiom in the package by this profile.

**Phase 9b update (2026-07-11) — lever #2 (the merge) attacked.** 9b-1 replaced the `if_else`-over-fmt
in `tab_compact` with a direct `in_refrow` field write (`promote_totrow_to_refrow`): **`tab_compact`
0.390→0.160 s (2.44×)** on the gss_cat 5×3 fixture, byte-identical (FAIL 0 | PASS 1339, no golden regen).
The removed 0.23 s is the `vec_case_when` share (~72 % of `tab_compact`) — the profile was exact. Lever #1
(the leaf fmt build) + the `tab_compact` `vec_rbind` remainder (0.16 s) are the gated plain-carrier rewrite
(9b-2/9b-3): carry the build as plain field-frames + col-meta, materialize `new_fmt` ONCE after the merge.
Full design + landmine ledger + go/no-go: `dev/tabxplor_phase9b_fmt_display_only.md`.

**Phase 9b-2 update (2026-07-11) — MEASURED, GO for 9b-3.** 9b-2 was re-scoped from "plain-field
CI/chi2 writers" to a throwaway decomposition (no `R/*.R` change): the writers are a **no-op on the
common `color="diff"` path** (`tab_ci` never runs — `ci` default `"no"`, `color="diff"` doesn't force a
CI, `R/tab-resolve.R:96`; `tab_chi2` writes nothing on `color_ctr="no"`), so they cannot gate 9b-3.
Harness `dev/benchmarks/phase9b2_fmt_cost_decomp.R`, 4 shapes on gss_cat (`output_list` isolates the
per-row_var build). Findings: on the common factor build (0.29 s/table) the fmt-record machinery is
`vec_restore` **29.7 %** + `vec_case_when` **18.1 %** + `new_rcrd` **12.9 %** by.total vs `[.data.table`
14.2 % (irreducible scan); the **materialize-once floor is ~0.5 %** (1.4 ms for 21 cols) and pushing
records through 6 reconstruct rounds is **54.5× slower** than plain field-frames + one materialize — the
fmt cost is almost entirely *redundant reconstruction*. Recoverable ~**30-48 %** on the common path (>25 %
bar) → **GO**; larger for CI (+28 %)/contrib (+64 %) and at big-table/warm-jmvtab scale (§27); numeric-only
tables gain ~nothing (cost = the data.table scan). Decision: **fold the plain writers into 9b-3**, not a
separate committable rung (they are a subset of the carrier win). Record:
`dev/benchmarks/results_1.4.0/phase9b2_decomposition.txt`; analysis `…phase9b_fmt_display_only.md` §5.

### Cleanup surfaced (fold into whichever Phase 9 work touches the file)

- **Dead code**: `tab.R` is 6 764 L, ~2 445 comment lines — a large fraction is commented-out legacy
  blocks (the `#By rows first` reduce, the whole `no_row_var` handling ~L3200-3234, the numeric
  pivot_wider stub, `tabs_bind` in `tab_classes.R`). Phase 9 is the moment to delete them.
- **`exists(…, inherits = FALSE)` guards** for the maybe-derived tables (`tabs_wn`/`tabs_diff`/`tabs_mean`/
  `tabs_rr`/`tabs_or`/`tabs_totn`, [tab.R:3040-3104](../R/tab.R#L3040)) are a fragile organic pattern;
  NULL-init + `is.null()` is the standard replacement (do it inside the `plain_core` split).
- The redundant second `relabel_levels_in_varnames()` ([tab.R:2676](../R/tab.R#L2676)) drops out once the
  core assumes a prepared population.
- The soft-deprecated standalone steps `tab_pct`/`tab_tot`/`tab_totaltab` are already OFF the `tab()` path
  (the math is inline in `tab_plain`); confirm and leave them as the exported superseded API.

### What you'd have to give up (the roadmap's two explicit questions)

- **"What to give up for meaningful simplification — even at the price of backward-compat?"** — Almost
  nothing user-facing. The only thing that goes is the **internal** per-row_var threading (Finding 2), and
  Phase 8's parity net shows that is byte-identical, so it costs zero backward-compat. Keep the col_var
  flexibility (per-col_var `pct`/`levels`/`digits` is genuinely used); keep `tab_many`'s per-row_var vectors
  (they become the `resolve()` input). The "jungle" is real but it is *internal* path duplication, not an
  API you must break — so the honest answer is: you do **not** need to sacrifice retro-compat to get the
  simplification; you need to make the row axis an outer map and split the leaves.
- **"What to give up for next-level performance?"** — The lever is the `tabxplor_fmt` **per-column vctrs
  round-trips**, not the field *contract* (that is user-facing — keep it). To go an order of magnitude
  faster on build+merge you would operate on the underlying fields as plain atomic vectors and reconstruct
  the `tabxplor_fmt` record **once** at the end, instead of paying a `vec_case_when`/ptype2/cast round-trip
  per column in `new_fmt` assembly and per column in `tab_compact`'s `if_else`-over-fmt. That is a display/
  build-layer rework (Phase 7f/10), independent of the Phase 9 restructure, and it gives up *using* vctrs
  generics on hot paths — never the fields users read with `$`/`mutate()`.

### Verdict & recommended Phase 9 scope (ranked)

1. **Do the outer-map row-axis collapse (Finding 2)** — the headline simplification, safe because Phase 8
   already locked its byte-identity net. Delete `ctx_slice`; unify serial/parallel dispatch; kill the
   [tab.R:1252](../R/tab.R#L1252) latent bug.
2. **Split the leaves into public-wrapper + resolved-core (Finding 3)** — removes the double resolution and
   clears `.fine`/`.by_table` off the public surface; gives the outer map + jmvtab cache + parallel worker
   one clean internal contract.
3. **Delete the dead code / `exists()` guards** while both files are open (Finding cleanup).
4. **Leave speed to Phase 7f/10 (Finding 4)** — but with the profile numbers now pinned, so those phases
   target the merge + fmt build + `case_when`-over-fmt, not the restructure.

**What NOT to do:** do not fork a second `tab()` core (re-duplicates the math — anti-keystone); do not
expect the restructure to speed anything up (0.2 %); do not touch the vctrs field contract or the public
args (retro-compat). The whole of Phase 9 is an internal re-cut of the *shared* engine so it reads like
"prep once → map a scalar core over row_vars → merge", which is what everyone already believes it does.

### Status — Phase 9a implemented (2026-07-11)

**Finding 2 (outer-map row-axis collapse): DONE, byte-identical** (full suite 1364 pass / 0 fail, no
golden regen). `tab_build()` = `tab_setup → tab_prepare_pop → tab_aggregate → tab_build_tables()`;
`tab_build_tables()` (shared by `tab_build` + `tab_counts`) resolves per-row_var ctxs via
`tab_rowvar_ctxs()` (replaced `ctx_slice()`/`tabxplor_rowvar_fields`) and maps the ONE whole-per-row_var
worker `tab_build_one()` (`transform → assemble_tables`) — serial `purrr::map` OR mirai, the sole
dispatch. `tab_transform`/`tab_assemble_tables` are scalar over one row_var; `tab_aggregate` stays a
whole-ctx pre-map step so `fine_fused` + the jmvtab `jmv_cache_aggregate` hook still fire once
(`jmv_cache_store_tests` moved to `tab_build_tables`, reading the gathered **pre-merge** tests — a
`!is.data.frame` guard preserves the old numeric-only skip + avoids a mixed-table ANOVA double-merge).
The [tab.R ex-L1252] `pct`-&-`OR` latent bug is designed out. Deleted `tab_build_rowvar`, `tabs_bind`,
the `#By rows first` block, and 223 commented dead lines in the leaves.

**Finding 3 (leaf wrapper/core split) + the `exists()`→NULL-init: DEFERRED** (maintainer decision).
Byte-identity pins all three moving parts in place: leaf-local resolution must stay in the core (§29-#2
drift risk; `ref="auto"` is type-specific per leaf), the relabel can't move (it renames level-collisions
vs `names(data)`, which differs before vs after the per-table `select`), and `.fine`/`.by_table` can't
leave the public surface (`test-num-fuse-parity.R` tests `tab_num(<NSE>, .fine=)` as a seam). With all
three pinned, the split collapses to a cosmetic NSE-boundary extraction (a thin wrapper forwarding every
arg to an unchanged ~800/940-line core) — poor risk/reward on the two most byte-sensitive functions.
`tab_plain`/`tab_num` kept whole; the `exists(…, inherits=FALSE)` guards are functional and left as-is.
Real speed remains Phase 7f/9b/10 territory (fmt build + merge + `case_when`-over-fmt), untouched here.

---

## 30. Phase 9c — where the time goes NOW, the pure-DT carrier verdict, and two clean wins (2026-07-11)

Phase 9c re-asks §29's questions after the whole carrier core (9b-4→9b-7) landed. §29's profile is
**stale**: it predates the carrier, so its headline "the merge is 34 %" no longer holds. A fresh
profile changes the answers.

### The fresh profile (post-9b-7)

`tab(gss_cat, 5 row_vars × 3 col_vars, pct="row", color="diff", chi2=TRUE)`, merged 0.77 s median,
`load_all` source. Two decompositions (Rprof self/total on the merged call, plus a list-path Rprof to
isolate the merge marginal):

| cost                                                                        | share (merged) | nature                                                                         |
|-----------------------------------------------------------------------------|----------------|--------------------------------------------------------------------------------|
| `[.data.table` wide-math (leaf scans + dcast + pct/diff/total)              | **~30 %**      | fixed per-op overhead × ~150 `[.data.table` calls over 15 tiny leaf tables     |
| `tab_apply_tests` (chi2/ANOVA marshalling)                                  | **~22 %**      | dplyr-on-small-tibbles + count-matrix extraction; `agg_*` math itself is cheap |
| compact **L3 reconcile** (`vec_ptype_common` → 9× `dplyr::if_else` per col) | **~7 %**       | the ENTIRE merge marginal (drops to 1.8 % in the list path)                    |
| redundant per-leaf `relabel_levels_in_varnames` + select/mutate narrowing   | **~5 %**       | each leaf re-narrows the 21 k-row data                                         |
| fmt materialize (`new_rcrd`, one per column)                                | **~3 %**       | irreducible                                                                    |

**Two facts reframe everything.** (1) The build is **N-INDEPENDENT**: replicating gss_cat to 215 k
rows leaves the merged call at ~0.81 s (≈ the 21 k-row 0.79 s). So the ~30 % `[.data.table` cost is
**not** the O(N) scan and **not** large-object copying — it is the fixed per-call overhead of
data.table's `[` invoked ~150× over *tiny* wide tables. (2) The §29 merge (34 %) is now ~7 % — the
9b-6 pvalue rewrite + 9b-1 compact field-write already banked it; what is left of the merge is the L3
attribute reconcile alone.

### Q3 (the maintainer's question) — pure data.table carrier for in-place `:=`? **NO — dropped.**

The premise ("modify in memory, the big data.table win") does not apply here, and the change would be
*less* reliable, not more:

1. data.table's `:=` avoids copying the **whole table** when writing a **column** — the benefit scales
   with **row count**. tabxplor build tables are **tiny** (O(cells): ~6–60 rows × ~5–25 cols); copying
   them is microseconds, and the build is **N-independent** (measured), so touching-large-data is not
   where the time is.
2. The pipeline's expensive operations are **row-CHANGING** (level-drop, total add/remove, the col_var
   join, compact `rbind`, pvalue `rbind`) — these **copy under data.table regardless of `:=`**. Only
   column-writes on a fixed row-set (ci/chi2) are `:=`-friendly, and 9b-5 already collapsed those to a
   single precompute-then-write pass.
3. The only genuine win from "carrier as one table" would be doing **fewer/bigger** operations
   (unifying the 15 leaf-math builds into one long-format pass) — but that is the **aggregate-core
   fork the keystone rejected** (re-duplicates the math), orthogonal to `:=`, and the largest
   byte-identity surface imaginable (18-field record, factor levels, L1 types).
4. **Reliability regression**: data.table reference semantics are a byte-identity footgun. The jmvtab
   tier-3 cache *stores* carriers that must not mutate → `copy()` everywhere → negating the very
   copy-avoidance that motivated it. The current **immutable field-frame carrier** (used only at the
   boundaries where reconstruction was costly) is both faster *and* safer.

**Decision: keep the field-frame carrier; do not pursue a mutable-data.table carrier.** Recorded so it
is not re-opened.

### Q1 — remaining levers, and what was implemented

Ranked by value/risk. Only the first was implemented in 9c (maintainer scoping):

1. **Compact L3 reconcile → base-R (IMPLEMENTED).** `vec_ptype2.tabxplor_fmt.tabxplor_fmt` picked each
   reconciled attribute with `dplyr::if_else` ×9; replaced with base-R `if/else`. **3.1× per call**
   (micro: 1039 → 335 µs), and since this method drives **every** `c()`/`vec_c()`/bind/group over fmt
   columns, the win generalises. Clean A/B (`dev/benchmarks/results_1.4.0/phase9c_ptype2_and_fusion.txt`):
   the default **merged call −7 % (0.760 → 0.705 s)** — the merge marginal 0.046 → ~0 — and a user
   **`c()` of two fmt columns 1.8×** (1.60 → 0.88 ms). Byte-identical (full suite FAIL 0, no golden
   regen). **Landmine**: `same_comp` CAN be NA (a count column's `comp_all` = NA bound with a pct
   column) — `dplyr::if_else(NA,…)` returned NA but bare `if (NA)` errors → `is.na()` checked first;
   `color` is length ≤ 2 → `ifelse` on the length-2 branch.
2. **Redundant per-leaf `relabel_levels_in_varnames`** (~5 %) — NOT done (needs a leaf public/core
   contract; the leaves are public, §29 Finding 3 deferred).
3. **`tab_apply_tests` marshalling** (~22 %) — NOT done; mostly dplyr-on-small-tibbles overhead, the
   `agg_*` math is already cheap; fiddly, golden-locked test parity.
4. **Leaf math on base-R/matrix** (the ~30 %) — NOT done; deferred to **Phase 9d** (below). The only
   lever big enough to move the ~30 %, but a real rewrite with float/NA byte-identity risk.

### Q2 — what to give up for simplicity: the scan-fusion opt-in (REMOVED)

The tab()-level opt-in scan-fusion — `options(tabxplor.fuse_min_rows)` (default `Inf` = off) + the
fused-`.fine` block in `tab_aggregate()` — was **removed**. Grounding: forcing it on (`fuse_min_rows=0`)
was **+1–7 %** on this fixture, and the build is N-independent, so fusing the O(N) scan buys nothing at
survey scale (the cost is O(cells)). It was dead by default and pure complexity. **Kept**: the
`.fine`/`fine_for_pair()`/`use_raw` seam in `tab_plain()` (now EXCLUSIVELY the jmvtab cache seam —
`jmv_cache_aggregate()` injects a per-pair `.fine` — plus `tab_counts()`'s injected count aggregate and
the numeric `fine_num`, all unaffected). `test-fuse-parity.R` was **rewritten** to drive
`tab_plain(.fine=)` directly (build a valid `.fine`, compare to the raw scan) — the factor analogue of
`test-num-fuse-parity.R`, so the seam stays locked with a focused test rather than only inside the
jmvtab suite; the one carve-parity fusion test was repointed (default == `.by_table`, both raw now).

### The honest ceiling

After 9c the merged call is ~0.70 s, of which ~30 % is data.table per-op overhead, ~22 % is test
marshalling, ~3 % is the irreducible fmt materialize, and the merge is free. **No further restructure
of tab()/tab_build moves the needle** (§29 Finding 4 still holds — resolution is 0.2 %). The remaining
real levers are all O(cells) display/build work: the leaf-math per-op overhead (Phase 9d), the test
marshalling, and `format.tabxplor_fmt`'s `case_when` (Phase 10b) — none of which need the carrier or a
representation change.

---

## 31. Phase 9d — leaf math on base-R / matrix (2026-07-11, DONE)

The §30 lever 4 ("~30 % `[.data.table` per-op overhead"), landed. `tab_plain()`'s three chained-`[`
blocks now run on plain numeric matrices / base-R group-sums instead of `copy()` +
`purrr::map(.SD, ~ eval(rlang::sym()))` / `keyby`. **Factor-only** (numeric stats already delegate to
base-R helpers; the profile is a factor fixture). **PoC-gated first**
(`dev/benchmarks/phase9d_leaf_math_parity.R`): every equivalence proven **byte-identical across 648
shapes** (`identical()`, the full pct × comp × OR × tab_vars × wt × na × ref × totaltab grid) BEFORE any
`tab.R` edit.

**The three blocks (each committed byte-identical, gate = ≥5 % end-to-end + `identical()`):**
- **F — `tab_apply_reference()`** internals → matrix sweep: `diff = P − P[refrow,]`, `ratio = P / P[refrow,]`,
  `rr = P / P[,refcol]`, `or = RR / RR[refrow,]`; per-comp-group reference index via
  `split()` + `which(refrows[rows])[1]` (NA → all-NA row, reproducing `x − nth(x, 0)`). **Signature +
  return shape unchanged** (`diff`/`ratio` col-indexable frames, `refrows` logical, `exists()` guards) →
  `jmv_tab3_reref` (jmvtab tier-3 re-ref) unaffected. ~118× isolated.
- **E — `leaf_wide_pct()`** (new helper): pct + `tot_n` as `M / D` where `D` = the row's Total (row) /
  the tab_vars-group's last (= total) row (col) / the grand Total (all/all_tabs); `P[is.na(P)] <- 0` ==
  `tidyr::replace_na`; `grp_last <- ave(seq_len(n), grp, max)` == `dplyr::last(.)`. ~1.5× isolated.
- **B/C — `build_total_rows()` / `finalize_total_rows()`** (new helpers): total-table + total-row group
  sums. **DECISIVE trap**: use **base `sum()` per `split()` group, NOT `rowsum()`/data.table-gforce** —
  the old `map(.SD, sum)` uses a LONG-DOUBLE accumulator; rowsum/gforce use plain double → 1-ULP drift →
  `identical()` FALSE. Conditional factor level-expansion (append `"Total"` only to columns in `totvars`),
  `check.names = FALSE` (value-cell names carry `$`/spaces, e.g. `"$25000 or more"`). ~4.5× isolated.

Region D (the `rowSums` Total column) stays as-is (already matrix, and must run first to put `"Total"`
in the matrix). `calculate_refrows()` / `diff_index()` unchanged (they return indices/logicals).

**Verified**: full suite **FAIL 0 / PASS 1400, NO golden regen**; the PoC 648/648 `identical()`.
**End-to-end (`dev/benchmarks/results_1.4.0/phase9d_{before,after,poc}.txt`):** no-tab_vars fixtures
common **−11 %** / ci **−7.4 %** per-row_var build (carried by E+F); **git-stash A/B with tab_vars**
(where B/C's `map2` multiplier bites): 1 tab_var **−20 %**, 2 tab_vars × 2 col_vars **−51 %**. contrib
(no ref, `pct="no"` → E/F skipped) and numeric (untouched) stay flat, as expected. Both gated blocks
clear ≥5 %.

---

## 32. Cumulative 1.3.1 → 1.4.0 benchmark + base/parallel stacking (2026-07-11)

Direct A/B of the CRAN release **tabxplor 1.3.1** (installed, `library()`) vs the **1.4.0-dev** source
(`load_all`), same machine (Ryzen 7 5800X, 8/16 cores), R 4.5.1, `median(system.time())`. API bridge:
1.3.1 merges via `tab_many(..., compact = TRUE)`, 1.4.0 via `tab(...)`; `tab_num()` identical in both.
Raw file: `dev/benchmarks/results_1.4.0/cumulative_1.3.1_vs_1.4.0.txt`.

### The four use cases

| use case (fixture)                                                                                | 1.3.1  | 1.4.0  | cumulative |
|---------------------------------------------------------------------------------------------------|--------|--------|------------|
| **Many factor tables, merged** (gss_cat 21k, 5 rv × 3 cv = 15 tables, pct row, colour diff, chi2) | 1.78 s | 0.59 s | **3.0×**   |
| **Many factor tables, list** (same)                                                               | 1.27 s | 0.61 s | **2.1×**   |
| **Numeric means, large** (8M replicated survey, `tab_num(age, tvhours)`, weighted, many groups)   | 0.65 s | 0.63 s | ~flat      |
| **Single big factor table** (2M rows, `tab(marital, race, wt, pct row)`)                          | 0.08 s | 0.06 s | ~1.3×      |

The cumulative win is concentrated in the **many-small-tables O(cells) path** (the package's core
"export dozens of coloured exploratory tables" workflow): **~2–3× serial**. It comes from Phase 3
(vectorised chi2/ANOVA), Phase 9b (the fmt-carrier: no more per-op record reconstruction), Phase 9c
(base-R `vec_ptype2`), and Phase 9d (base-R/matrix leaf math) compounding.

### Base improvements + parallelisation STACK — multiplicatively

Same fixture (43k rows, **8 factor row_vars × 2 col_vars**, chi2, list output), three-way on one machine:

| build                            | time            | gain                                   |
|----------------------------------|-----------------|----------------------------------------|
| 1.3.1 serial                     | 1.39 s          | baseline                               |
| 1.4.0 serial                     | 0.72 s          | base **1.93×**                         |
| 1.4.0 parallel W=8 (warm pool)   | 0.27 s          | parallel **2.67×** (over 1.4.0 serial) |
| **total 1.3.1 → 1.4.0 parallel** | **1.39 → 0.27** | **5.15× = 1.93 × 2.67**                |

**They stack cleanly (the product is exact).** The two levers are **orthogonal**: base improvements
shrink the per-table O(cells) work; `parallel=` runs the (now cheaper) per-`row_var` tables concurrently
on the mirai pool. So the parallel multiplier applies on top of the base multiplier.

**Caveat (Amdahl + fixed cost):** parallel carries a fixed overhead — cold pool warm ~1 s, ship the data
once, serialise the finished tables back, and the main-side merge/pvalue/unwrap stays serial. So the
2.67× is realised for **many tables + a warm/reused pool**; for a few tables or trivially cheap
per-table work the multiplier shrinks toward 1 (the §26 verdict). The faster 1.4.0 serial makes the
serial merge a relatively larger share, which is why the measured parallel efficiency here (2.67× at 8
workers) does not reach a theoretical 8× — but it still stacks multiplicatively with the base gain.

### Honest scoping — the numeric case

On **typical** survey numerics (low-cardinality `age`/`tvhours` by categorical groups) the mean-table
wall-time is **~unchanged** 1.3.1 → 1.4.0. The large documented numeric win (Phase 2: `tab_num` **5.6×**
unweighted / **8.3×** weighted, **6–11×** less allocation — `before_phase2_8M.csv` vs `after_rollup_8M.csv`)
was on a **heavier continuous-numeric** synthetic 8M fixture (`gen_big_df`) that stressed the old
`weighted.var` double-scan. The moment-sum core's **allocation-churn** reduction holds regardless, but the
wall-time headline does not generalise to light categorical-survey numerics. Big single factor count
tables were already fast in 1.3.1 (the aggregation is GForce data.table) and are ~unchanged.

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
  and Excel stay in parity — which is why the Excel *engine* swap (Phase 11) is just a backend change
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

---

## 33. Phase 10a + 10b DESIGN — unified exporter prep & display (2026-07-11)

Full brief: **`dev/tabxplor_phase10_exporters.md`** (the single self-contained Phase 10 architecture doc).
This section records only the decisions settled in the 10a/10b design session; the doc governs the
10c→10g implementation.

**10a — jamovi live-display engine: keep + optimize kableExtra first; home-built HTML table is Plan B.**
Grounded (web + the existing code): jamovi's results panel **only honors inline CSS** (CSS via
`htmlDependencies` never applies — [jamovi #1529](https://github.com/jamovi/jamovi/issues/1529)) and won't
reliably run htmlwidget JS, so interactive tables (reactable/DT) are out, `gt` is heavy (global rule avoids
it), and `tinytable`'s interactivity wouldn't fire live either. So keep kableExtra and make the win come
from the shared prep (colours/refs derived **once**), NA-hiding moved into prep, `tooltips = FALSE` (already
Phase 7e), and a "light" kableExtra path. **Only if that isn't enough** (re-profile on the current engine),
fall back to a dependency-free home-built `<table>` renderer — kept cheap by isolating the final HTML
generation behind a `render_kable_html()` seam (same idea as the tab_xl backend seam). Note: the §23
profile's #1 lever (`fmt_color_selection`) is **stale** — that function was deleted in Phase 5, so re-profile
before ranking levers.

**10b — the architecture** (detail in the doc): (1) a normalized **`tabxplor_render` ephemeral sidecar**
(NOT tab attributes — dplyr desyncs them) holding the derive-once quantities (reference/total masks, colour
slots/hex, stars, blank mask, bold rows, `[min;max]`, labels), consumed identically by the `format()`-string
backends and the `tab_xl` numeric bypass — number→glyph is the only backend-specific step, which is why the
bypass stays. (2) One **`tab_export_prep()`** helper (new `R/tab-export-prep.R`) replacing the 4×-duplicated
"canonical col_vars → validate → compact" preamble + the per-exporter role detection, with a base(single) vs
list(several, rendered one-after-another) split. (3) **`format()`/`get_reference()` rework** — `case_when` →
boolean algebra (byte-identical), a `.ref =` precompute arg so masks are derived once not 4×, and
`format(syntax = "excel")` folding `numfmt()` in (kills the bypass drift). (4) **Robust var detection** —
keep the col_var-attribute path; use `dplyr::group_vars()` (desync-resistant) validated against live factors
for row_var/tab_vars; graceful `degrade` → plain tibble + `cli_inform` instead of the current crash
([tab_classes.R:568/716](../R/tab_classes.R#L568)) + `test-edge-cases.R`. (5) **`[min;max]`** total-column
range as a table-level pre-pass (§10; Excel = text cells where bases differ). (6) **tab_xl backend seam**
(§21) on openxlsx v1, stars via the numfmt literal. (7) **`tab_transpose()`** finished (single-total,
namespaced) + opt-in transpose-at-export. (8) **`tab_plot()`** soft-deprecated.

**New this session (beyond §7/§8/§10/§21/§22):**

- **Opt-in multi-field display** (`pct (n)`, `pct ± ci`, …): the maintainer wants the **full flexible
  syntax**, but **opt-in with ZERO cost on the non-using path**. → a **new optional per-column attribute
  `display_spec`** (`NA` default), parsed **only inside `format()`** when non-NA; the stored `display` stays
  a single primary token (drives `get_num`/coloring/Excel unchanged). `/vctrs-field` change: **9 → 10
  attributes** (conscious `test-fmt-contract.R` + structural-golden regen; display byte-identical when
  unused). Excel falls back to the primary field.
- **`label` attribute → `tab_kable` header tooltip only** (narrows §22's "→ all"; the maintainer's choice —
  minimal clutter, keeps variable names).

**Flagged / carried into implementation:** `test-export-parity.R` covers only `n`/`wn`/`pct`/`mean`
([:28](../tests/testthat/test-export-parity.R#L28)) → **must be extended in 10g** to
`diff`/`ci`/`or`/stars/label/`[min;max]` or it won't guard the new shared surface; §7's tab_transpose
"@export'ed" note corrected (commented-out/unexported); §23 profile flagged stale; compact + tab_vars stays
deferred.

**Phase 10c DONE (2026-07-12)** — the `format()`/detection rework, byte-identical (full suite green;
conscious structural golden regen only for the new `display_spec` attribute). Detail in
`dev/tabxplor_phase10_exporters.md` (Status block). Governing scope decisions this session (with the
maintainer): (a) **`display_spec` = curated whitelist** `c("pct (n)", "n (pct)")` as its own isolated
step, NOT the full `field literal field` parser (which §33's "maintainer wants full flexible" note now
supersedes for 10c — the general grammar can come later if a concrete need appears). (b) **Defer the
prep-helpers to their consumer:** `numfmt()` → `format(syntax="excel")` moves to **10g** (atomic removal,
no duplicate-source-of-truth window); `tab_totcol_range()` → **10d** (built next to its wiring);
label-capture-in-build → **10e** (only `tab_kable` consumes it). So 10c shipped: `get_reference()` boolean
rewrite; `format()`/`pillar_shaft` `.ref` memoization + `if_else`→base + the **`x$var`→`get_var(x)`
~28 % lever** (not in the plan — surfaced by the re-profile, `dev/benchmarks/results_1.4.0/phase10c_profile.txt`,
`format()` ~2×); `tab_render_vars()` + graceful degrade guards + `tab_get_vars()` hardening +
`test-edge-cases.R`; `display_spec` (§6, 9→10 attr, `tab(display=)` / `set_display_spec()`). The re-profile
also confirmed the §23 stale ranking: `format()`'s residual cost is now all `stringr`/`stringi` (a future
lever), not the deleted `fmt_color_selection`.

**Phase 10d Part 1 DONE (2026-07-12)** — the shared exporter prep, byte-identical (full suite green PASS
1501 / FAIL 0, NO golden regen; kable/md A/B-verified `identical()` across 10 fixtures). New
`R/tab-export-prep.R`: `tab_export_prep()` builds the `tabxplor_render` model ONCE and `tab_kable`/
`tab_md`/`tab_plot` consume it, deleting the 4× duplicated blocks A (compact via `tab_check_same_col_vars`
- the existing `tab_compact`), B (degrade via `tab_render_vars`), C (role detection), D (bold rows via
`tab_bold_rows`) and the two-channel colour loop (now `fmt_col_ann()`). The derive-once win
(`get_reference` not 4×/col via `format(.ref=)`; `fmt_channel_codes` once) lives in the per-column `ann`.
Factoring principle honoured (§ Aim): factor only the genuinely-shared expensive quantities; the
medium-specific quirks stay LOCAL (md's tab_vars keep+blank + `str_trunc` + real-col_var span index + its
`new_group` trailing trim; kable's knitr `*`-escape + `row_spec`/`column_spec`; plot's ggpubr render) —
where kable and md genuinely differ (the `new_col_var` transition index, the `tab_bold_rows` empty-set
edge) each keeps its own tiny derivation reading the shared `col_var_map` rather than a false unification.
`tab_totcol_range()` built + populated INERT (consumption + golden regen is 10e/10f). `tab_plot()`
soft-deprecated (`lifecycle::badge("superseded")`). `tab_export_prep` returns a `tables` LIST but the
exporters render `tables[[1]]` (current list-compaction preserved); the true N-table list method is an
additive follow-up. Flagged/handled: the doc's "md drops tab_vars" was wrong (md keeps+blanks → prep uses
`drop_tab_vars = FALSE`); `format(syntax="excel")` is not in source (10g). Detail:
`dev/tabxplor_phase10_exporters.md` (Status block). **Part 2 (point E = `tab_transpose()`) follows after
the maintainer commits Part 1.**

**Phase 10d Part 2 DONE (2026-07-12)** — after Part 1 was committed. Two items:
(1) **`tab_md()` list method** (maintainer request "tab_vars too in tab_md()"): a non-mergeable list
(several row_vars and/or tab_vars → `tab()` returns a list; or differing col_vars) renders each table
one-after-another (each keeping its tab_vars sub-tables) instead of erroring — gated by
`tab_export_prep(list_method=)` (`tab_list_mergeable()` = same col_vars + no tab_vars); `tab_md` opts
in, `tab_kable`/`tab_plot` keep the historical error (no list renderer yet); `tab_md` split into a thin
wrapper + `md_render_one()`; single-table byte-identical.
(2) **`tab_transpose()` finished + exported** (`lifecycle` experimental). `tidyr` pivot (grid transpose
- per-cell fields ride along) + rebuild the flattened per-column attributes from a representative
real-col_var column (the 9 `fmt_col_attrs`) + swap the axis flags (`type` row↔col; per-cell
`in_totrow` field ↔ `totcol` attribute; `in_refrow` ↔ `refcol`) + re-key the `test` attribute
(row_var↔col_var). Verified structurally AND render-identical to a native `pct="col"` table, and
round-trips. Single row_var, ≤1 total row/col, no tab_vars (else `cli_abort`). The per-exporter
`transpose=` arg wiring stays 10e/10f/10g (mechanism ready). `test-transpose.R` (53). Full suite PASS
1566 / FAIL 0, no golden regen.


### 10e IMPLEMENTED (2026-07-12) — tab_kable hybrid engine + cheap tooltips + deferrals

**Hybrid engine (maintainer-approved this session).** `render_kable_html()` seam in
`R/tab-render-html.R`; `tab_kable()` = `option-resolve → tab_export_prep(list_method=TRUE) →
map(render_kable_html) → tab_kable_join`. New public arg `engine` (`getOption(
"tabxplor.tab_kable_engine","kableExtra")`). `"kableExtra"` = the legacy pipeline carved out verbatim,
**byte-identical** (git-stash A/B over an 8-fixture × 3-variant matrix → empty diff; the two `any_bg`
branches unified since `cell_spec(background=NULL)` ≡ omitting it). `"html"` = a dependency-free,
self-contained inline-CSS `<table>` renderer: styles go directly on `<td>` (no per-cell `<span>` → the
DOM-size win), vectorised `do.call(paste0, td_cols)` assembly, a scoped one-time `<style>` block; it
emits the SAME bootstrap `data-toggle`/`title` tooltip attributes so hover tooltips keep working in
jamovi (maintainer: kableExtra tooltips already work there). Serialization differs benignly (kableExtra
colours = `rgba(...)`, html = `#hex`; kableExtra emits empty `title=""`, html omits it) — cross-engine
CONTENT parity (cell text + non-empty tooltip content) is identical and tested (`test-render-html.R`).

**Cheap tooltips (byte-identical).** `tab_kable_print_tooltip()` `any()`-gates each of the ~9
`format(set_display(x, field))` fragments so the expensive pass runs only when the column carries that
field (a pct column has no or/mean/sd; a mean column has no rr/or/pct); a failed gate yields `rep("",
n)` = the original all-FALSE `if_else`/`case_when` output → tooltip string byte-identical. Also reuses
the prep's precomputed `.ref` (ref_cells).

**NA at source.** `format.tabxplor_fmt(na=)` is now honoured on the main path (a final
`if (!is.na(na)) out[na_out] <- na`, applied after every append). Default `na=NA` → no-op, byte-identical
for the console and every non-`na` caller; `tab_kable`/`tab_md` pass `na=""` → NA cells render `""` at
source. Retires tab_kable's `interactive()`-gated `str_replace_all(">NA</span>", …)` (byte-identical in
the always-CSS default; the only visible change is the rare non-CSS batch case where NA used to leak).

**Perf** (`dev/benchmarks/results_1.4.0/phase10e_{baseline,after}.txt`, gss_cat): cheap tooltips 0.50→
0.36 s (−29%) on the kableExtra big table; html engine 0.16 s (3.1× vs baseline, mem 8.5→1.3 MB), 0.072 s
w/o tooltips. The html engine WITH tooltips (0.16 s) beats the old jamovi kableExtra path WITHOUT them
(0.22 s), so jamovi can regain interactive tooltips cheaply (left OFF pending a live check). jamovi
`.render_html` / `tab_html_string` / `jmvtab_export` now use `engine="html"` (dropped lightable/cosmo
`includeCSS` + `scroll_box` + class-strip → `tab_render_scrollbox()`; html export no longer needs
kableExtra). Full suite 1601/0, no golden regen.

**Deferred (with blockers — honest scoping):**
- **Spanning col_var header** — the doc's "brings kable to parity with the console" rationale is FALSE:
  the console does not use a spanning header; it disambiguates multiple col_vars by suffixing the col_var
  name to colliding level names (`Other_race` vs `Other_relig`), which the kable already inherits from
  the same built columns. A spanning header would be redundant, so it is dropped (respecting the
  maintainer's "don't add clutter").
- **`[min;max]` total-column consumption** — `tab_totcol_range` only differs under `na="drop"` with
  diverging col_var NA rates; wiring it as "overwrite where `differ`" makes the `pct="row"` Total column
  show "100%" on most rows but a base-count range (`[15;17]`) on others (inconsistent within one column),
  and the base is already shown in the separate `n` column. The intended semantics (does Total always
  show the base? relation to the `n` column?) need a maintainer decision; helper stays INERT (as 10d).
- **Label header tooltip** — `tab_export_labels()` reads `attr(col, "label")` on the built tab, but the
  source `label` attribute does NOT survive `tab()` building (`prep$labels` is NULL even when the source
  had labels). Needs label propagation through the build pipeline first (out of a render change).
- **`transpose=` arg** — wire uniformly across kable/md/xl in 10f/10g (mechanism ready, 10d).
- **`kable_tabxplor_style()`** — an `@export`ed near-duplicate of the tab_kable styling for plain
  tibbles, now unused internally (the degrade path uses `kableExtra::kbl`). Candidate for soft-deprecation
  (maintainer's call — it is public API).

### 10g IMPLEMENTED (2026-07-12) — tab_xl onto the shared prep + format(syntax="excel"); 4132 → ~810 lines

**Maintainer steer (this session):** NO byte parity with the old Excel is required — "around the same
display and format" suffices; the old export was a "white elephant", so aggressive simplification is
welcome. Two feature-drop decisions taken by AskUserQuestion: **drop** the tab_xl `n_min` greying and
**defer** the per-table-writer split to the openxlsx2 phase (Phase 11, renamed **Phase 10h** in the
roadmap).

- **`format(x, syntax = "excel")`** (`fmt_class.R`, new internal `excel_numfmt_code`) folds the old
  inline `numfmt()` closure → `format()` is the ONE display source of truth. Crucially it is fed
  **format()'s OWN masks** — the x100 mask `pct_or_ci` (+ a `pvalue` add, whose `%` scaling comes from a
  separate render path), the standalone-`ci` marker (± prefix), the `pct_ci`/`mean_ci` TEXT mask — plus
  format()'s **adjusted** digits (n→0, or→≥2, mean-diff→≥1). Because the code's `%`-ness IS format()'s
  x100 decision, the bypass **cannot desync by construction**. This **fixed two latent old-`numfmt`
  bugs** the hand-maintained mask carried: a `diff` on a **pct** column now gets `0.0%` (was `#,##0.0`
  → Excel showed `-0.0`), and `pvalue` cells keep `%` scaling. tab_xl still writes the RAW `get_num()`
  value; Excel formats it.
- **tab_xl consumes `tab_export_prep(backend="xl", compact=FALSE, drop_tab_vars=remove_tab_vars,
  list_method=TRUE, compute=c("refs","bold"))`.** Deleted the two `tab_get_vars()` passes + the
  duplicated canonical-col_vars preamble + the copy-pasted bold/reference block. Per-table geometry is
  sourced from the prep `roles`/`bold_rows` (fmt/other/total cols; `totblock_top/bottom` → total-block
  borders; `bold_rows` → ref rows; `head(new_group, -1)` → between-group double borders), offset by the
  sheet `start`. **Colours stay on tab_xl's own two-channel `fmt_color_channels`** — the prep's
  `roles$color_cols` only reads the TEXT channel and would miss background-only columns (a latent prep
  narrowness, flagged; kable inherits it). Number styles built once per distinct code (memoised). The
  list-based styling loop was KEPT (re-sourced from the prep), not split into a per-table writer.
- **Per-table degrade**: a non-tabxplor member of a list is written as a plain sheet + message (no
  crash), extending the single-df degrade to lists.
- **Simplifications:** `hide_near_zero` (near-zero conditional formatting) + `n_min`
  (`insufficient_counts`, ~150 lines + offset maps + greying) **dropped** — soft-deprecated
  (`lifecycle`, kept inert, warn on non-default); `n_min` → `tab(n_min=)`. The ~2500-line dead tail
  (stale `tab_xl` duplicate, `rule_*`, `tab_xl_confidential*`, `xl_to_tab_CASD`) + interspersed dead
  comment blocks removed. `last_text_col` / `insufficient_counts` deleted.
- **Tests**: `test-export-parity.R` extended (diff/ctr/or displays; the number = `get_num` × 100 iff the
  Excel code carries `%` — tying the code's scaling to `format()`'s; threshold `<`/`>` cells skipped) +
  a `format(syntax="excel")` code lock; `test-tab_xl.R` gains a workbook read-back (values round-trip) +
  a plain-df degrade test + `skip_if_not_installed` guards. Full suite green (1725).
- **Deferred to the openxlsx2 phase (10h/11):** the ~12-closure backend seam, significance **stars** in
  Excel (numfmt literal), `[min;max]` total-column consumption (`tab_totcol_range` still INERT),
  `transpose=` arg, the per-table-writer split.
- **Pre-existing (NOT 10g):** `color="contrib"` + `comp="all"` errors in the shared colour engine
  (`fmt_color_plan` → `get_mean_contrib()` returns size 0) — `tab_kable` fails identically; a Phase 5
  issue to fix separately.

## 34. Phase 10i DESIGN — add_n / add_pct / pvalue-lines as display-time materializations (2026-07-12)

DESIGN-ONLY session (maintainer's choice, AskUserQuestion Q4): this section is the deliverable; no
product code, no golden regen this session. It records the verdict, the four settled decisions, the
grounded current-state, the target design, and the session/increment phasing for the follow-up work.

### The four decisions (this session)

1. **Display-only.** The built `tab()` object OMITS the `n` / `col_pct` columns and the p-value rows.
   They are rendered only by `print()` / `tab_kable` / `tab_md` / `tab_xl`. The whole-tab `test`
   attribute is **kept** (stop dropping it). `tab_pvalue_lines()` / `tab_add_n_pct()` stay exported as
   on-demand materializers for a user who wants real rows/cols.
2. **Composite recipe → the per-cell `display` field with a glue-style `{}` grammar** (`"{pct} (n={n})"`);
   **drop the `display_spec` per-column attribute** (10 → 9 attributes; a `/vctrs-field` change). `get_num()`
   gains a short-circuited gate so simple tokens (`"pct"`/`"diff"`/…) stay on the fast O(cells) path.
3. **add_pct = a real appended column/row at display** (uniform text + Excel). Only **add_n** gets the
   in-cell composite (text) / a real `n` column (Excel) — because add_n is a *re-display* of the total
   column while add_pct carries *new numbers* (the col%/row% distribution) that don't fold cleanly into
   one cell.
4. **Design-doc, then stop** for maintainer approval before any code.

### Why — current-state grounding (the complexity these three cause)

- **`tab_add_n_pct()`** ([../R/tab.R:6441-6617](../R/tab.R#L6441)) materializes REAL extra columns/rows
  at `tab_assemble_tables()` ([../R/tab.R:1777](../R/tab.R#L1777)) — per row_var, after the level-drop,
  before total-col removal. add_n col (`pct="row"`) = the last total column **cloned** with
  `set_display("n")` (a pure re-display of the base `get_n()`); add_pct col = `col_pct` = the total
  column's col% (`get_wn/last(get_wn)`, genuinely NEW numbers); the `pct="col"` variants append rows with
  reserved `row_var` `"n"`/`"row_pct"`. Reserved tags `col_var="all_col_vars"` + `row_var` `"n"`/`"row_pct"`.
- **pvalue rows** — the `test` attribute is ALREADY a whole-tab attribute (created at
  `tab_assemble_tables` via `new_tab/new_grouped_tab(test=)`). `tab_pvalue_lines()`
  ([../R/tab_classes.R:882-968](../R/tab_classes.R#L882)) bakes one p-value row per subtable × col_var
  and **DROPS the attribute** ([../R/tab_classes.R:966](../R/tab_classes.R#L966)). Runs at
  `tab_assemble_output` ([../R/tab.R:1887](../R/tab.R#L1887)/[1890](../R/tab.R#L1890)); `tab_xl` bakes it
  itself ([../R/tab_xl.R:120](../R/tab_xl.R#L120)). Row markers: `row_var="pvalue"` + all cells `n=NA`.
- **The special-casing tax** these synthetic rows/cols impose: chi² kept-rows mask
  ([../R/tab.R:5589](../R/tab.R#L5589)), contrib eligibility ([../R/tab.R:5738](../R/tab.R#L5738)),
  `tab_apply_n_min()` protect-list ([../R/tab.R:6640](../R/tab.R#L6640),[6653](../R/tab.R#L6653)),
  `tab_ci`'s `all_col_vars` vector append, `arrange` row-keep cases, the export-prep total-block
  whitelist ([../R/tab-export-prep.R:277-285](../R/tab-export-prep.R#L277), an un-i18n English-label
  match already flagged), and — crucially — the jmvtab tier-3 **re-ref** `jmv_tab3_reref()`
  ([../R/jmvtab-cache.R:650-724](../R/jmvtab-cache.R#L650)), which reconstructs a `data_mask`/`pval_mask`
  and drops the p-value rows before re-running `tab_ci()`, plus the `n=NA` skip in `jmv_reapply_digits`.
- **The composite mechanism already exists** as `display_spec` (Phase 10c, the 10th attribute), a curated
  whitelist `c("pct (n)","n (pct)")` parsed only in `format()` by re-entering `format()` per field
  ([../R/fmt_class.R:1993-2003](../R/fmt_class.R#L1993)) — the working proof that an in-cell composite
  needs NO extra rows/cols. `tab_totcol_range()` ([../R/tab-export-prep.R:144](../R/tab-export-prep.R#L144))
  already computes the cross-col_var `[min;max]` base per row (INERT since 10d).

### Verdict — worthwhile, NOT a white elephant

The simplification is real and cross-cutting: with the extras gone from the built table, every
reserved-row/col special-case above shrinks or disappears. **pvalue is the cleanest win** — the
attribute already exists, so deferral is mostly *stop dropping it* + *move the bake from build to
display*. **add_n unifies** with the deferred §10 `[min;max]` total-column range (already built).
**Positive for jmvtab live**: the cached carrier stops holding p-value rows, so the re-ref
`data_mask`/`pval_mask` dance and the `n=NA` digit-reapply skip vanish; materialization runs at the
O(cells) render, matching the cache philosophy ("fmt is recomputed, not cached"). The one genuine
cost is the visibility change (below), accepted as decision 1.

### The target design — extras become display-time materializations

**One model: three logical extras driven by table-level intent + the kept `test` attribute; the build
emits the "core" table only.**

- **Intent storage** — `add_n`/`add_pct` become a small table-level attribute (e.g.
  `render_extras = list(add_n=, add_pct=)`) carried by the tabxplor dplyr S3 methods exactly like
  `subtext`/`test` (`/dplyr-method`: extend `new_tab`/`new_grouped_tab` + `dplyr_reconstruct`). The build
  stops calling `tab_add_n_pct()` and stops baking p-value rows; it sets the flags and KEEPS `test`.
- **One shared display-time materializer** — called by `tab_export_prep()` (once, for every backend) AND
  by the console print methods, on the core table: **pvalue** → reuse `tab_pvalue_lines()` at display;
  **add_pct** → append a real `col_pct`/`row_pct` (text + xl), reusing the existing halves; **add_n** →
  text: an in-cell composite on the Total column (`{pct} (n={n})`, cross-col_var n from
  `tab_totcol_range()` → `min` or `[min;max]`); Excel: a real `n` column of numbers (only add_n differs
  by backend). Order on the core: n_min (build) → materialize extras (display).
- **The `{}` grammar** (replaces `display_spec`) — `display` field value = a simple token (fast path) OR a
  glue template `"{pct} (n={n})"`. `get_num()`: `if (!any(grepl("{", display, fixed=TRUE)))` → the current
  fast path; else primary = the first `{field}` (regex only on composite cells) → byte-identical when no
  composite is present, and Excel is automatic (get_num returns the primary). `format()` generalizes the
  10c composite branch into a `{field}` template parser (split literals + tokens; render each via
  `format(set_display(x, field))`; paste; stars ride the primary). Curated forms `"pct (n)"`/`"n (pct)"`
  stay as sugar mapped to `"{pct} ({n})"`. `tab(display=)` writes the FIELD; drop
  `display_spec`/`get/set_display_spec`/the 10th attribute.
- **Console print consolidation** — print calls the shared materializer then renders → ONE renderer path
  for console+kable+md+xl. `print_chi2()`'s attribute-block becomes redundant (removable/optional).
- **jmvtab re-ref simplification** — the tier-3 carrier no longer carries p-value rows / n / col_pct, so
  `jmv_tab3_reref` drops the `data_mask`/`pval_mask` + "filter p-value rows before `tab_ci`" logic and
  `jmv_reapply_digits` drops the `n=NA` skip. Verify `test` + the intent attribute survive
  `fmt_unwrap`/`fmt_wrap`.
- **Bonus cleanup** — the export-prep total-block whitelist (`c("n","pvalue","row_pct")`) is replaced by a
  proper ROLE tag set when the prep itself creates the rows (fixes the flagged i18n miss).

### Phasing (follow-up work; grouped by session, increments where a mid-commit helps)

- **Phase 10i-A — the `{}` display grammar (ONE session, ONE commit).** Self-contained display-layer
  refactor; a PREREQUISITE (add_n's in-cell uses it) so it lands first. `get_num()` gate + `format()`
  template parser; `tab(display=)` writes the field; drop `display_spec` (10→9, `/vctrs-field`); curated
  sugar. Verify: simple displays byte-identical; new composite `_snaps/`; `test-fmt-contract.R` 10→9
  conscious regen; benchmark `get_num()` (the gate must be negligible per the Phase 9d O(cells) ethos).
- **Phase 10i-B — display-only migration (ONE session, TWO increments, maintainer commit between).** Both
  increments share the materializer + reserved-marker removal + jmvtab simplification.
  + **Increment 1 — pvalue display-only** (+ its jmvtab coupling): stop dropping `test`; remove the
    build-time bake; call the bake in `tab_export_prep` + console print; consolidate `print_chi2`; remove
    the reserved-`"pvalue"`-row special-cases; simplify `jmv_tab3_reref` + `jmv_reapply_digits`. Golden RDS
    regen (no p-value rows in the built tab); rendered `_snaps/` + exports byte-identical. → commit.
  + **Increment 2 — add_n/add_pct display-only**: intent attribute + dplyr S3 carry; remove the build-time
    `tab_add_n_pct`; materializer gains add_n (in-cell text via the Phase-A grammar + `tab_totcol_range`;
    xl `n` column) + add_pct (appended col/row); remove the reserved-`"n"`/`"row_pct"`/`"all_col_vars"`
    special-cases; role-tag the total block. Golden RDS regen + **conscious console/kable/md snapshot
    regen** for add_n (base-n column → in-cell `(n=…)`, the headline visible change); Excel keeps a column.
    → commit.
- **End-of-Phase-B perf gate (mandatory)** — a **`git stash` A/B benchmark against the current version**
  must confirm the impact is **at least neutral**, measuring **build-table vs display/export performance
  SEPARATELY** (work MOVES from build to display: the build must get faster/neutral, the display must not
  regress net; spot-check jmvtab live). Save to `dev/benchmarks/results_1.4.0/`.

### Caveats (honest)

- **Programmatic visibility change (accepted, decision 1)** — the built tab no longer has the
  `n`/`col_pct` columns or p-value rows; `names()`/`$`/`as_tibble()`/dplyr see the core table. `print()`
  still shows them (print = display). Back-compat is light (add_n opt-out is a recent default; p-value
  ROWS only since 1.3.0 — NEWS.md:271-273); `get_n(tab$Total)` gives the count and the public
  `tab_pvalue_lines()`/`tab_add_n_pct()` materialize real rows/cols on demand. Document in NEWS.
- **Headline user-visible change** — with `add_n = TRUE` (default), the base-n moves from a separate
  console/kable/md `n` column to an in-cell `100% (n=114)` (a wide, intended `_snaps/` regen); Excel keeps
  the `n` column.
- **`get_num()` hot-path gate** — one `grepl(fixed=TRUE)` short-circuit; must be benchmarked (Phase 9d
  rewrote leaf math for ~30 %, so the O(cells) path is sacred). Fallback: test only the column's uniform
  display, or cache a per-column `is_composite` flag.
- **Golden surface is large** (Phase-9b scale) — the built-tab RDS fixtures change structurally (fewer
  rows/cols); the target is that RENDERED output (`_snaps/`, export-parity) stays byte-identical EXCEPT
  the intended add_n in-cell change. Conscious regen per phase/increment.

### Phase 10i-A DONE (2026-07-12) — the display `{}` grammar

Shipped as designed (the per-cell field variant, forced by the maintainer's correction: under
`pct="col"` add_n/add_pct are ROWS, so the composite can NOT be a per-column attribute). The composite
display is now a per-cell **`display`-FIELD** glue template (`"{pct} (n={n})"`); the Phase-10c
`display_spec` **attribute was DROPPED (10 → 9)**. Three performant SHARED helpers next to `get_num()`
(`R/fmt_class.R`) are the single source of truth: **`display_primary()`** (gated resolver: one fixed
`grepl`, composite → first `{field}` alias-resolved, malformed → best-effort no-crash),
**`parse_display_template()`** (literal/token split, once per unique template), **`validate_display_template()`**
(write-time: `{}`-only — a raw `{}` template validated and returned; the ONLY place a bad `display=`
value aborts; fields ∈ `c(pct,n,wn,mean,diff,ratio,ci,or,ctr,var)`, `ratio`→`rr`).
Every display-token DISPATCH consumer routes through `display_primary()`: `get_num`/`set_num`, the
`format()` masks (raw display kept only for the template expansion), `vec_ptype_abbr`/`vec_ptype_full`
(header shows the primary type), `tab_kable_print_tooltip`. **Excel needs no special-casing** — the
`format(syntax="excel")` early-return runs on the primary, so a composite exports the plain primary
number (§34 dec.3). `tab(display=)` writes the template into the FIELD only on genuine **value cells
where every template field is non-NA** (the Phase-10c `both` guard), so count-only columns (added-n),
p-value / blank / total-marker cells keep their own token and render normally — `"{pct} ({n})"`
byte-identical to Phase 10c's `"pct (n)"`.

- **Public surface — `{}`-only** (no curated sugar, decided 2026-07-12 for one consistent syntax now
  that `{}` is proven free): `tab(display="{pct} (n={n})")` / `"{n} ({pct})"` / `"{diff} [{ci}]"`; the
  first `{field}` is the primary (shown alone by Excel, used for colour). The old `pct (n)`/`n (pct)`/
  `pct_n` recipe strings now error → `{}` required. The internal composed tokens `pct_ci`/`mean_ci`/
  `or_pct` are KEPT as pipeline-set rendering modes: they use integrated CI/OR rendering (centered
  bracket, shared/forced digits, `ref:` decorations) that `{}` cannot express, and are never
  user-typed — so no user-facing inconsistency (empirically confirmed).
- **Benchmark verdict — Solution 2, ship as-is** (`dev/benchmarks/results_1.4.0/phase10iA_display_grammar.txt`;
  git-stash A/B). On no-composite tables the whole display/export pipeline is UNCHANGED
  (build/print/tab_md/tab_kable/tab_xl no measurable diff); the gate is ~11 ns/cell (one fixed grepl).
  Only an isolated `format()` of a 200k-cell column moved ~3% (within `system.time` noise). No
  Solution-3 dedicated fast tokens needed. Composite rendering cost (2 sub-formats + paste) is opt-in.
- **Tests**: new `test-display-grammar.R` (helpers + malformed `{pct`/`{}`/`{ pct }`/`{foo}`/`{pct}{`,
  consumer no-crash on injected bad templates, stars-ride-primary, grouped/list/`pct="col"`, the gate
  micro-benchmark); `test-fmt_class.R` Phase-10i-A section; `test-fmt-contract.R` 10 → 9 + snapshot.
  Golden RDS regenerated (attribute drop ONLY — verified every waldo diff mentions `display_spec`;
  `_snaps/golden.md` byte-identical). Full suite green (1793). NAMESPACE drops `get/set_display_spec`.

### Phase 10i-B Increment 1 DONE (2026-07-12) — p-value rows are display-only

Two maintainer decisions this session refined the §34 design: **(1) add_n in-cell base defaults to the
Total column's own `{n}`; a global `tabxplor.totcol_range` option (default `"off"`) will switch to the
cross-col_var `[min;max]` base via `tab_totcol_range()` — Increment 2. (2) p-value = block in the R
console, rows in exports** (not "rows everywhere"): the console keeps the compact `print_chi2()` block
(now live because `test` is no longer dropped), exporters materialise p-value rows.

Increment 1 (p-value) shipped: the built `tab()` keeps the `test` attribute and no longer carries the
"pvalue" body row (`tab_assemble_output()` no longer calls `tab_pvalue_lines()`). The ONE display-time
hydrator is **`tab_materialize_extras(tab, backend = c("text","xl"), pvalue = TRUE)`**
([R/tab_classes.R](../R/tab_classes.R), next to `tab_pvalue_lines`), idempotent (reused by `tab_export_prep`
after `tab_resolve_tables` before `prep_one_table`, and by `tab_xl` before `tab_transpose`); Increment-1
body just wraps `tab_pvalue_lines`. `print_chi2()` moved below the `print == "kable"` branch (kable-mode →
rows). `tab_apply_n_min()` dropped its now-dead `pline` protection. jmvtab simplified: the tier-3 carrier
holds no p-value rows, so `jmv_tab3_reref()` lost the `data_mask`/`pval_mask` + slice-out dance and
`jmv_reapply_digits()` lost the `n==NA` skip (byte-identical, `test-jmvtab-cache.R` green).

**Verification:** full suite green (1804). Exports byte-identical (`_snaps/golden.md`, export-parity,
color-golden all pass — exporters materialise the rows). ONLY 3 goldens regenerated (conscious):
`f_chi2`, `f_color_contrib`, `c_contrib` — each loses its "pvalue" row; unweighted chi2 tables also now
store raw `wn = NA` instead of the fallback `=n` that `tab_pvalue_lines`' `build_col` used to bake onto
every row (benign — `get_wn()` recovers it, exports re-materialise it, and it makes chi2 tables
consistent with non-chi2 ones). `test-n_min.R` updated (the p-value line is no longer a body row; it
now asserts the `test` attribute survives n_min instead). Increment 2 (add_n/add_pct + the
`render_extras` attribute + dead special-case removal + the perf gate) is next.

### Phase 10i-B Increment 2 DONE (2026-07-12) — add_n / add_pct rows/cols are display-only

The built `tab()` no longer bakes add_n / add_pct: `tab_assemble_tables()` drops the `tab_add_n_pct()`
call and stores the intent in a small **`render_extras = list(add_n=, add_pct=)` table attribute**
(`get/set_render_extras`), carried through EVERY dplyr verb + the `tab_cast`/`tab_ptype2`/`gtab_*`
reconcilers exactly like `subtext`/`test` (~37 threaded sites; the reconcilers take x's, a scalar intent).

**Materialiser (byte-identity de-risked empirically).** Before touching the build I captured 11
reference tables (Increment-1 build, add_n baked) and proved `tab_add_n_pct(list(final_table), …)`
reproduces the add_n `n` column / add_pct `col_pct` column / the pct="col" `n`/`row_pct` rows
**byte-identically** on the FINISHED table — its grouped outer-`mutate` reproduces the per-subtable
`last()` scoping for single / merged (grouped by `"row_var"`) / tab_vars (grouped by the tab_var) /
means / multi-col_var, so the Plan agent's "whole-table vs row_var scope" worry was moot and no
tab_add_n_pct rewrite was needed. `tab_materialize_extras()` therefore just calls `tab_add_n_pct()`
(xl-style columns/rows) + clears `render_extras` (idempotent). For TEXT backends
**`tab_fold_addn_incell()`** drops the `n` column and folds the base into the Total cell as
`{pct} (n={n})` (decision 1; default = the Total's own `{n}`, opt-in `options(tabxplor.totcol_range=)`
`"range"`/`"min"` → the cross-col_var base via the now-live `tab_totcol_range()`). The full-build A/B
(new core + `materialize("xl")` == the captured reference) is TRUE for all 11 shapes, before AND after
the special-case removals. Console print materialises the text extras (`pvalue = FALSE`).
`tab_transpose()` carries `render_extras` (orientation-agnostic → transpose(row% add_n) renders as a
native col% add_n table). §10 `[min;max]` total-column range: now wired as the add_n opt-in option.

**Dead special-cases removed** (the extras never exist at build now — chi2/ci/contrib run at STAGE 4,
before the STAGE-5 materialise that is gone): `chi2_compute_test`'s `c("n","row_pct")` row-exclusion,
contrib's `all_col_vars` exclusion, `tab_apply_n_min`'s `helper`/`helprow` (→ `protect = totrow|tottab`).
KEPT (harmless robustness / re-entrant safety): the `tab_ci`/`tab_pct` `all_col_vars` vector extensions
and `arrange`'s `%in% c("n","row_pct","pvalue")` guard. The export-prep total-block whitelist stays
(the materialiser still tags the rows) — the i18n-robust marker is a deferred bonus.

**Back-compat shim (maintainer request).** `$.tabxplor_tab` / `[[.tabxplor_tab` /
`pull.tabxplor_tab`(+grouped): reading `tabs$n` / `tabs[["n"]]` / `pull(tabs, "n")` (or `col_pct`) on a
core table reconstructs the column from the Total column (byte-identical to the old add_n/add_pct
column) with a `lifecycle::deprecate_soft`; **gated on `%in% names(x)`** so the existing-column fast path
pays nothing; only a genuine COLUMN reconstruction applies (pct="col" add_n was a ROW → NULL; add_n=FALSE
→ NULL). `pull` re-injects the captured quosure into `dplyr::pull(as_tibble(.data), !!vq)` to preserve
tidy-select NSE (a bare `NextMethod()` — and even `substitute()` — broke `pull(tabs, <col>)`).

**Verification:** full suite green (**1815**; new `test-display-extras.R`). Perf gate (git-stash A/B,
build vs display separately, `dev/benchmarks/results_1.4.0/phase10iB_display_only.txt`): BUILD 0.350 →
0.330 s (−6 %), DISPLAY 0.320 → 0.350 s (+9 %), NET neutral — work moves build → display as designed
(the jmvtab cached build is now cheaper). **Golden regen (conscious):** ALL `_golden/*.rds` (add_n/add_pct
cols + pct="col" rows removed, `render_extras` gained), `c_or` colour (its pct="col" add_n row), and the
`golden.md` + `render-html.md` display snapshots (add_n column → in-cell `100% (n=…)`; percentages / CI /
stars / colours unchanged). Excel keeps a real `n` column. Tests updated: `test-n_min.R`
(intent+`n`-absent), `test-calculations.R` (add_n-display-only chi2). Increment 3 (further pipeline
simplification) is optional / open.


## 35. Phase 10j — workflow integration & the performance floor (2026-07-12)

### Grounding — the build is at its floor

A fresh read of the code + `dev/benchmarks/results_1.4.0/` + §29-§31 confirms: `tab()` build arg/axis
resolution is ~0.2 %; the field-frame carrier banked the big wins; the mutable-DT / core-fork /
carrier-join routes are closed; ~99 % of build is O(cells) fmt work. Excel write is openxlsx2-bound
(~92 %, parallel-immune, at floor after 10h). jmvtab live is render-bound (10c already 2×'d `format()`).
**The only substantial build-perf lever left is the `tab_apply_tests`/`tab_chi2` marshalling (~22 %,
§30 lever #3), which is mostly dplyr-on-small-tibbles, golden-locked, and PoC-risky.** The genuinely
unfinished, clean, high-value work is EXPORT INTEGRATION.

### Decisions (maintainer, this session)

1. **Focus = export integration/simplification** (perf is a secondary, careful add-on). The build /
   Excel-write floor is accepted, not fought.
2. **Add a `tab_export(x, format=)` facade** (the four exporters stay as idiomatic wrappers).
3. **Keep the two-channel colour model** through the unification (text = 1st measure, background =
   2nd) — it is exactly what lets `tab_xl`'s duplicate colour pass be deleted.
4. **Attempt the `tab_apply_tests` base-R rewrite (Phase 10j-B), PoC-gated** — byte-identical proof
   before any source change (like 9d's 648-shape `identical()` proof), abandon if the recoverable share
   is small or the structural `tab_match_*` record ops resist base-R. A SEPARATE later session, run only
   after 10j-A is committed (different code area, higher risk).

### Phase 10j-A IMPLEMENTED (2026-07-12) — byte-identical, suite 1827/0, no golden regen

Three increments (maintainer commit between each):

- **A-i:** `tab_xl()` requests `compute += "colors"` and consumes the shared prep `ann` two-channel
  colour SLOTS (`text_slot`/`bg_slot`), deleting its private `fmt_color_channels()`/`color_cols` pass.
  The slots are **theme-independent** (they come from `fmt_color_plan`/`fmt_color_slots` on the data,
  not the palette), so xl keeping its own light-palette slot→hex map is byte-identical; equivalence
  proven empirically across factor/two-channel/numeric/contrib/grey. One colour derivation for all four.
- **A-ii:** shared **`resolve_export_opts()`** (theme/color_type/html_24_bit/color/color_legend/
  transpose preamble, once); exported **`tab_export()`** facade (mirrors `jmvtab_export`); argument
  unification — `color` (monochrome) + `transpose` on all four; `transpose` centralised in
  `tab_export_prep()` **after** materialise (matching xl's historical materialise→transpose, so xl drops
  its own pre-materialise+transpose and just passes `transpose=`); `tab_md(title→caption)` +
  `tab_xl(print_color_legend→color_legend)` soft-deprecated; `tab_xl` gains `theme`/`html_24_bit`/
  `color`/`caption` and becomes **theme-aware** (its palettes honour `theme`; default `"light"` == the
  old hardcode). **`fmt_col_ann()` now ALWAYS returns the full structure** — `want_colors=FALSE` (the
  new `color=FALSE`, no golden lock) yields a MONOCHROME column (zero slots, ref-based grey font,
  `back="none"`) instead of a partial list; this fixed `color=FALSE` breaking the home-built html engine
  / `tab_plot` / xl (md already guarded the NULL slots). `want_colors=TRUE` (every golden path) is
  unchanged → no golden regen.
- **A-iii:** `tab_plot()` list-method parity — a non-mergeable list is intercepted BEFORE the prep and
  the function **recurses per element** (`purrr::map(tabs, tab_plot, ...)`), returning a list of
  ggplots (no 250-line body extraction; respects "don't invest in the superseded tab_plot's display").
  Removed the dead `fmt_frame_fields` constant (no code consumer; `fmt_col_attrs` kept). New
  `test-export.R` (facade dispatch, monochrome, transpose, xl theme-awareness, deprecations, plot
  list-method).

Files: `R/tab-export.R` (new), `R/tab-export-prep.R`, `R/tab_classes.R`, `R/tab_md.R`, `R/tab_xl.R`,
`R/tab.R` (dead-constant), regenerated `NAMESPACE` + 5 `man/*.Rd`. Full detail: `dev/
tabxplor_phase10_exporters.md` (10j-A Status).

### Phase 10j-B IMPLEMENTED (2026-07-13) — PARTIAL GO; build at its floor confirmed

PoC-gated (B-i), then a scoped rewrite (B-ii). Full numbers + scripts: `dev/benchmarks/results_1.4.0/
phase10j_tests.txt`. The honest reframing of §30's "~22 %": the whole-table test path IS 26 %, but on the
tables that cost time the **`agg_chi2` engine dominates** `chi2_compute_test` (73 % on chunky many-subtable
shapes; already data.table, not a target) — the recoverable marshalling is much smaller than the coarse
share implied.

- **Byte-identity PROVEN (26/26 `identical()`)** for both candidate rewrites across factor/mixed/mean ×
  comp tab/all × 0-2 tab_vars × weighted × 2×2 Yates. Landmine: `agg_chi2`/`agg_anova` DROP degenerate
  subtables (n<2 / no valid cells); the live code recovers them as all-NA rows via `distinct(long)+
  left_join(engine)`, so a byte-identical rewrite MUST re-implement that shape (match the full tuple set,
  NA where the engine dropped).
- **LANDED:** `is_a_mean` → direct `get_type()` read (`tab_chi2()`, `R/tab.R`) — was a per-col_var
  `dplyr::select(ungroup(tabs))` reconstructing fmt columns to read the scalar `type` attr. **~3.15 % of the
  whole `tab()` call** (6.1× on the op, noise-free), byte-identical (suite 1842/0, no golden regen), a
  genuine simplification.
- **ABANDONED:** the `chi2_compute_test` marshalling rewrite (~6 %, byte-identical but engine-capped and a
  base-R re-shape of `distinct+left_join`, not a simplification); the shared `detect_totcols` (<1 %,
  CI-path risk). The bigger adjacent lever `tab_compact` (21.9 %) is a different task. **The build is at its
  floor (§35 confirmed).**

Also this session (independent correctness): the flagged `color="contrib"` + `comp="all"` colour crash
turned out to be THREE render bugs — `get_mean_contrib()` size-0 under comp="all" without a total table
(new `grand_totrow()` degrade, shared with `chi2_write_contrib()`'s seed protection), the kable tooltip's
NA `cond_ctr` on the Total column, and `tab_md()`'s NA-unsafe tab_var blanking on materialised p-value rows.
All byte-identical; +`c_contrib_all`/`c_contrib_all_notab` colour goldens + an exporter render-no-crash test.
Semantics confirmed: comp="all" = whole-table chi2+contribs, comp="tab" = per-subtable.

## 36. Phase 12a — tab_logit integration: location + engine + fmt (2026-07-13, DONE)

The two open questions the roadmap asked to be settled here, plus the fmt-field integration. Suite green
**1877/0**, no golden regen; `test-tab_logit.R` = 35 tests (glm/svyglm OR/CI/p parity, colours, exports).

### D1 — Location: keep inside tabxplor (NOT a `regxplor` subpackage)

Kept in tabxplor with `broom`/`survey` as `requireNamespace()`-guarded Suggests (the openxlsx2/ggplot2/mirai
pattern). Web-checked (CRAN policy + R-pkgs 2e): CRAN weighs *strong* deps (Imports/Depends), not Suggests
used conditionally — so guarded Suggests cost ~0 toward the "too many deps" worry. A subpackage would force
EXPORTING many tabxplor internals across a boundary (`fmt0`, `cleannames_condition`, `new_tab`, the colour
engine, `tab_apply_reference`) and double dev/CI/release friction (the maintainer's stated concern), for no
CRAN benefit — decisively so once D2 drops the tidymodels stack (the only "heavy" motivation). Verdict: the
regression table IS a tabxplor table; it belongs in tabxplor.

### D2 — Engine: direct `stats::glm` / `survey::svyglm` + `broom::tidy` (NOT parsnip)

The draft wrapped glm in parsnip/workflows/hardhat + a top-level `parsnip:::`-internals `svglm2` survey
engine. Web-checked: parsnip's glm engine only calls `stats::glm` — the abstraction buys pluggable engines
tab_logit doesn't need. Dropping it REMOVED `workflows`/`hardhat`/`poissonreg` from Suggests (net dep
reduction), killed the version-fragile `parsnip:::` calls + the load-time engine-registration side effect,
and unified the weights path (`survey::svyglm(quasibinomial, svydesign)` directly — correct design-based SEs,
vs the frequency-inflated N of `glm(weights=)`; this already fixes the 12b weight-inflation concern for the
*inference*, the normalization *policy* stays 12b). Statistics identical (same glm fit).

### Inference method — `method = "wald"` (default) vs `"profile"` (settled 2026-07-13)

Web-checked (Cytel; The Stats Geek; RMPH; r-statistics.co): the LR / profile-likelihood interval + test
is **statistically preferred** (more accurate for small samples / rare events / near-separation / skewed
likelihoods; more powerful; robust to Hauck-Donner), BUT the **Wald** z-test + interval is the **universal
software default** (R `summary.glm`, Stata, SPSS, SAS; base `confint.default`) and the **only option for
survey-weighted models** (profile is undefined for `svyglm` — the maintainer's primary data). **Verdict:
default `"wald"`** — reproducible/teachable, uniform across weighted+unweighted, one fit, dual-clean; **opt-in
`"profile"`** for accuracy on unweighted glm (weighted -> Wald + a message; needs MASS, added to Suggests).

**Both modes keep CI <-> p (hence CI <-> stars) EXACT duals** (§20 honoured):
- `"wald"`: CI computed in-house `exp(coef +/- crit*se)` (glm `qnorm`, svyglm `qt` with design df) + the
  Wald p from `broom::tidy`. NOT `broom::tidy(conf.int=)`, which silently switches to profile when MASS is
  loaded (would break the duality). Stored natural-scale in `ci_inf`/`ci_sup`; `pvalue` = the Wald p.
- `"profile"`: profile CI from `stats::confint` (MASS) + a **per-coefficient LR-test p** (`logit_lr_pvalues`:
  drop each model-matrix column, 1-df deviance chi-square = the exact dual of the profile CI; a test locks
  CI-excludes-1 <-> LR p<alpha).

### `color_signif` default = `"grey_non_signif"` (opt-in `"ignore"` / `"color_all_signif"`)

Odds-ratio significance drives the colour, as a `tab_logit()`/`multi_logit()` argument over the existing
`color_signif` fmt attribute + the Phase-12a OR gate (exclusion of 1): default greys ORs whose CI includes
1 (colours only the significant, by magnitude); `"ignore"` colours every OR by magnitude;
`"color_all_signif"` colours the significant by their conservative interval bound.

### fmt integration — OR is an ordinary fmt column, 4 inert reader patches (§1, §20)

No new `type`. An OR column is `type="row"`, `display="or"`, `color="OR"`,
`color_signif="grey_non_signif"`, with a NEW `ci_type="or"` (log-OR Wald exp() bounds, multiplicative
neutral 1). Reader patches in `fmt_class.R` (all inert for non-OR, golden-verified): `set_ci_type` enum
`+"or"`; `ci_center()` centres an OR CI on the OR; `fmt_color_plan()` significance gate tests **exclusion
of 1** (not 0) for the `"or"` measure; `format()` `disp_or` adds **`1/OR`** reciprocal display for OR<1
(`0.25 -> "1/4"`, everywhere incl. empirical OR — byte-identical for OR>=1) + a no-pct guard so a pure
model-OR reference row shows a bare "1". Stars + magnitude/greyed colours light up automatically from the
written `pvalue` + bounds. Excel keeps the raw OR number (no `1/x` string). tab_logit_2.R (or_plot/lm_plots)
deferred to a later display phase; visible OR CI bracket + OR+ME/OR+PCT layouts stay 12b/12d.

---

## 37. Phase 12b — regression tables: statistical framework & design for `tab_reg` (2026-07-13, DESIGN)

Phase 12b settles the **statistics + architecture** for tabxplor's regression tables — generalising the
Phase-12a binary-logit `tab_logit()`/`multi_logit()` into a unified regression-table function `tab_reg()`.
This section is the permanent record: it keeps the trace of **four deep web-research passes** + a **git study
of the pre-package draft** + **two maintainer AskUserQuestion rounds** that settled every fork. **No product
code changes in 12b** — it governs 12c (tests), 12d (rewrite), 12e (jamovi UI). **No back-compat** on the
regression functions (the maintainer's explicit licence to redesign the API radically).

The governing aim (mirrors the 1.4.0 aim): **reuse the existing fmt / `test`-attribute machinery, not fork
it** — the fmt record already carries an additive shape (`display="diff"`, `ci_type="diff"`, neutral 0) and a
multiplicative shape (`display="or"`, `ci_type="or"`, neutral 1, `1/x` reciprocal), which are exactly the
gaussian-β and OR/IRR shapes, so the unified engine needs **near-zero new field plumbing** (verified against
`fmt_class.R`); and the whole-table `test` attribute (§16/§24, rendered by `print_chi2`/`tab_pvalue_lines`/
`tab_materialize_extras`) is the direct analogue of a regression model-summary footer. Add the **fewest new
dependencies** and **match ecosystem conventions** (gtsummary / parameters / sjPlot) without over-engineering.

### Research grounding (the four passes + git study)

- **Pass 1 — lm/glm table ecosystem + model-level stats.** Effect measure by family is universal: gaussian →
  raw β; logistic → OR = exp(β); poisson/quasipoisson/negbin → IRR = exp(β). Packages split only on *who*
  exponentiates: **manual** (`broom::tidy(exponentiate=)`, gtsummary, modelsummary, stargazer) vs
  **family-aware auto** (sjPlot `tab_model`, `parameters`/easystats, which offers `exponentiate="nongaussian"`
  — exp non-Gaussian, leave OLS raw — the cleanest mixed-family default). Mature tools **label each model
  column by its own measure** ("Beta"/"OR"/"IRR") rather than one global header. `broom::glance.lm` gives
  R²/adjR²/F+p/sigma/AIC/BIC/logLik/deviance/nobs; `glance.glm` gives null/residual deviance/AIC/BIC/logLik/
  nobs but **no R² and no omnibus test** (both must be computed). Pseudo-R² is genuinely contested:
  `performance::r2` defaults Tjur (logit)/Nagelkerke (GLM)/McFadden (multinomial); sjPlot prints Nagelkerke;
  **sociology/Stata/Long-Freese expect McFadden** — always print the measure's name. modelsummary's default
  footer (`Num.Obs./R2/R2 Adj./AIC/BIC/Log.Lik./RMSE/F`) is a good "how much is enough" yardstick.
- **Pass 2 — nominal ≥3 (MNL vs binary) + survey weights.** **Begg & Gray (1984)**: fitting K−1 binary logits
  of "j vs ref" *on the two-category subset {j, ref}* is **consistent** for the baseline-category MNL
  coefficients — same estimand, MNL merely more efficient (uses all categories jointly). The honesty
  condition: use the **subset** form ("j vs ref"), NOT "j vs pooled rest" (a different, incoherent model whose
  predicted probabilities don't sum to 1; Li et al. 2024; advocated mainly in ML — Rifkin & Klautau 2004).
  IIA is a non-issue for individual-attribute sociology models (Cheng-Long 2007; Allison). **Survey weights**:
  raw population-total weights fed to `glm(weights=)` shrink SEs to ~0 (base R treats them as precision/
  frequency weights, information ∝ Σw). The fix is **design-based** `survey::svyglm`: Horvitz-Thompson /
  Taylor-linearization sandwich SEs that are **invariant to weight scale** (svyglm even rescales weights to
  sum-to-n internally for stability, changing nothing) — so non-normalised weights are neutralised
  automatically, point estimates equal `glm(weights=)` (hence **match the weighted crosstabs**), and inference
  is honest. Kish `n_eff=(Σw)²/Σw²` is only a stopgap (ignores clustering). `svydesign(ids=~1, weights=~w)` is
  an acceptable honest default; caveat — it ignores clustering/stratification and can understate variance
  (Winship-Radbill note the weight-vs-not debate; for the *descriptive* "match my weighted tables" contract,
  weighting is right).
- **Pass 3 — MNL contrasts, ordinal, AME.** **exp(baseline-category MNL coef) IS the "OR (j vs reference
  level)"** — the same parameter as the Begg-Gray subset logit (Rodríguez GLM notes; Werth *Categorical
  Regression*: RRR "equivalent to odds ratios in logistic regression"). So **fit ONE MNL and label exp(coef)
  as "OR (j vs ref)"** — honest, efficient, keeping "vs ref" in the label. One MNL yields **any pairwise
  contrast** `exp(β_j − β_k)` and **any reference outcome level** with no refit (CI needs the *joint* vcov:
  `Var(β_j−β_k)=Var(β_j)+Var(β_k)−2Cov`). **"j vs rest" has no constant OR** — `log[P(j)/(1−P(j))] = x'β_j −
  logΣ_{k≠j}exp(x'β_k)` is not linear in x, so it is a **covariate-dependent marginal quantity**, not a
  coefficient; the clean single-reference summary is the **AME / averaged predicted probability on P(Y=j)**
  from one fit. **Proportional-odds ordinal** (`MASS::polr`, one cumulative OR per predictor level, one
  column) is a defensible default for ordered ≥3 outcomes **only if diagnosed**: the parallel-lines assumption
  is "often violated" (Williams gologit2; Long-Freese), tested by Brant/LR, over-rejects at large N; fall back
  to partial-PO / gologit / MNL. `nnet` **and** `MASS` are R **Recommended** packages (always installed) → MNL
  and ordinal are **dependency-free**. **AME** (average marginal effect = sample-average of per-obs `dP/dx`,
  vs MEM at the means / MER at representative values) is the sociology standard (**Mood 2010**; Long-Freese;
  Williams 2012; Bartus 2005) because probability-scale effects are **comparable across models/groups where
  ORs are not**; AME needs the **fitted model + data + vcov** (NOT reconstructable from a reported OR), and
  generalises: logit/MNL/ordinal → probability points, poisson → count, gaussian → the coefficient itself.
  Point estimates + predicted probabilities are safe in base R (`predict()`); **AME/contrast standard errors
  need the delta method** (averaging `predict(se.fit=)` is *wrong*) → depend on **`marginaleffects` (Suggests)**
  for inference, do not hand-roll delta-method SEs.
- **Pass 4 — model comparison, dispersion, dependency inventory** (agent completed 2026-07-13, confirming the
  analysis). Nested LR tests via `anova(m1, m2, test="LRT")` (**LRT ≡ Chisq**; chi² for binomial/poisson,
  **F** for gaussian/quasi — the doc's own rule; `anova()` **errors on an N-mismatch**, worth mirroring);
  survey → `anova.svyglm` (Rao-Scott **working**-LRT / Wald) and `regTermTest`. **No** mainstream table package
  auto-inserts a between-column nested test — they show **per-column GOF (AIC/BIC/pseudo-R²)** and leave formal
  tests to `anova()`/`lmtest::lrtest` — so an opt-in comparison footer is a genuine differentiator (prior art:
  Stata `nestreg` = sequential, `lrtest` = vs-baseline). **Dispersion is a Poisson / grouped-binomial issue,
  NOT ungrouped 0/1** (Bolker GLMM FAQ: for Bernoulli data the variance is fixed at p(1−p), so dispersion is
  not identifiable): print the **Pearson dispersion** `Σ(pearson resid)²/df.residual` (better-behaved than
  deviance/df), flag at **>1.5** (strong **>2**). `quasibinomial` returns ≈1 on 0/1 data (it does NOT fix
  overdispersion) — its real jobs are grouped/proportion binomial and silencing the non-integer-weight warning
  under svyglm. The cross-cutting rule: enforce a **central same-N / same-response / same-likelihood guard
  once** and degrade with a clear message (LR → AIC/BIC; ordinary → working/design-based under survey; else
  "not shown"). Overdispersion correction: quasipoisson (scaled SEs, **no AIC**) or `MASS::glm.nb` (has a
  likelihood/AIC, preferred for strong overdispersion). Dependency
  inventory: everything needed (McFadden `1−logLik/logLik_null`, Nagelkerke, AIC/BIC, LR-vs-null
  `null.deviance−deviance` on `df.null−df.residual`, MNL, ordinal, survey `psrsq`/`regTermTest`) is
  **base R + the Recommended `MASS`/`nnet` + the already-Suggested `broom`/`survey`** — the **only** genuinely
  new Suggests is `marginaleffects` (AME inference).
- **Git study (commit `6e47bab^`, the pre-package parsnip draft — `tab_logit.R` 1009 L + `tab_logit_2.R`
  706 L, fully commented-out).** Recovered features: `nb_questions` (summed-score → grouped binomial, the
  integer-outcome path — dropped by 12a); `split_var` (fit the model within each level of a grouping var →
  subtables); `multiplicator` (a continuous predictor's effect per k units, e.g. OR per 10 years); the
  `empirical_OR` companion (raw empirical odds/% beside the model OR); `readable_OR` (predicted probability +
  a `marginal_effect = prob − prob[reference]` — **confirming the old ME was MER-at-reference, not AME**);
  `or_plot` (finalfit-style OR forest plot) and `lm_plots` (2×2 glm/lm diagnostics).

### The decisions

#### D1 — Unified `tab_reg` engine + friendly wrappers; effect measure per family

One internal engine dispatching on `family`: `stats::lm`/`stats::glm`, `survey::svyglm` (weighted),
`nnet::multinom` (nominal), `MASS::polr` (ordinal), `MASS::glm.nb` (negbin). Public
`tab_reg(data, dependent, predictors, family = , effect = , wt = , …)`. **`predictors` dispatch** (maintainer:
one function, dispatch on argument type): a **character vector** = one model, and `dependent` may itself be a
vector → **one column per dependent**; a **named list of predictor sets** = **model-comparison mode** (one
`dependent` → **one column per model**, predictors absent from a model left blank). The two are mutually
exclusive (error if both a vector-of-dependents and a list-of-models). `tab_logit()` / `multi_logit()` are
kept as **thin binomial-family wrappers** with the curated binary-outcome UX; **`tab_reg(family="binomial")`
gives the identical UX** (the wrapper is discoverability sugar, not a different path).

Effect measure per family, **auto-labelled per column** (ecosystem: `parameters`/sjPlot): gaussian → **β**
(raw, additive — reuse the fmt `display="diff"`-shape, `ci_type="diff"`, neutral 0); binomial → **OR**;
poisson/quasipoisson/negbin → **IRR** (both multiplicative — reuse `display="or"`, `ci_type="or"`, neutral 1,
`1/x` reciprocal display). Default **`exponentiate = "nongaussian"`** (exp non-Gaussian, OLS raw); a mixed-
family table shows β/OR/IRR per column, never one global "Estimate" header. **Verdict:** the fmt already
carries both shapes, so a unified engine is *less* code than the two forked math paths, not more.

#### D2 — Family comes from the outcome, never the R storage type

Consensus (Pass 1): the GLM family is a modelling choice; the only safe data-driven rule is **2 distinct
non-missing values → binomial**. Integers are **not** silently Poisson (overdispersion / zero-inflation); a
double in [0,1] is not 0/1. **Verdict:** `family` is explicit and overridable; auto-detect **only** the 0/1 →
binomial case and **emit a message**. Outcome types supported:

- binary factor / 0-1 numeric → **binary logit** (as 12a);
- **summed-score integer 0..q → grouped binomial** (reinstated from the git draft): `nb_questions`/`trials`
  (default = the score's max) → model `cbind(score, q − score)` (proportion of "yes" out of q items). This is
  the **only** place `quasibinomial` and the dispersion flag genuinely apply (D7);
- count integer → **poisson**, with **quasipoisson / `MASS::glm.nb`** for overdispersion;
- continuous → **gaussian** (`lm`);
- nominal factor ≥3 → **MNL** (D3);
- ordered factor ≥3 → **proportional-odds** (D4).

#### D3 — Nominal ≥3 outcome: ONE efficient MNL, honest OR labelling, three effect flavours

Fit **one** `nnet::multinom` (dependency-free) and read `exp(β_j)` as **"OR (j vs reference level)"** — the
Begg-Gray result makes this the *same parameter* as the subset {j,ref} binary logit, estimated efficiently
(supersedes the roadmap's "three separate binary logits"). The **"vs reference" must stay in the label** (never
a bare "OR"). Three effect flavours, all from the single fit:

1. **"j vs reference-level" OR = `exp(β_j)`** — the default; a **constant, profile-free** coefficient. Any
   reference outcome level is available as `exp(β_j − β_k)` with no refit (CI from the **joint** vcov).
2. **"j vs rest" OR at the reference profile** (maintainer wants this) — there is **no constant** j-vs-rest OR
   (it is covariate-dependent), so it is computed as an **adjusted odds ratio at a fixed profile** (default:
   all other predictors at their reference), i.e. the ratio of the "j vs not-j" odds at (level m, others=ref)
   vs (ref, others=ref), with a **delta-method CI** from the MNL vcov. Documented as **profile-conditional**
   (changes with the held-fixed values; odds ratios don't average cleanly — the probability scale is preferred
   for a one-number summary).
3. **AME + predicted probabilities** (D5) — the clean, averageable single-reference summary on P(Y=j).

Also align `tab()`'s **empirical** OR for a 3+ level dependent (currently labelled "RRR"): RRR ≡ j-vs-ref OR,
so relabel and let the `OR` argument choose the reference outcome level; a "j vs rest" empirical flavour is
documented as at-profile / deferred.

#### D4 — Ordered ≥3 outcome: proportional-odds default, **diagnosed**

Default to the **cumulative-logit / proportional-odds** model (`MASS::polr`, dependency-free): one column, one
**cumulative OR per predictor level** — the parsimony/interpretability win the maintainer wants. But **not
silently**: auto-run a **Brant / LR proportional-odds test**, **warn on violation** (explicitly noting that the
test **over-rejects at survey-scale N**), and offer fallbacks — **partial proportional odds** (`ordinal::clm`
`nominal=` / `VGAM`, a Suggests upgrade), generalized ordered logit, or **MNL**. Agresti endorses the family
as the natural ordered-outcome default; Williams / Long-Freese require the assumption to be tested — the
diagnosed default honours both.

#### D5 — Interpretation MODE — an orthogonal `effect=` axis (OR ⟂ AME)

`effect=` is orthogonal to the family (which fixes the *native* coefficient):

- **`"coefficient"` (default)** — the native per-family effect: β / OR / IRR / cumulative-OR (= RRR for MNL).
- **`"ame"`** — **AME + predicted probabilities**, the sociology standard (Mood 2010): probability points for
  logit/MNL/ordinal, expected-count change for poisson, and *the coefficient itself* for gaussian. AME is the
  **sample average** of per-observation `dP/dx` (not MEM/MER); it needs the **fitted model + data + vcov**, so
  `tab_reg` must **retain (or refit) the model**, not "reconstruct from the OR". For MNL/ordinal it is a
  per-category AME (the coherent single-reference summary of D3-flavour 2's intent).

Display integration: an **AME reuses the additive `diff` shape** (neutral 0, coloured like a difference); a
**predicted probability reuses `pct`**; when both are shown, compose with the Phase-10i-A **`{}` grammar**
(e.g. `"{pct} (Δ{diff})"`). A **MER-at-reference** "adjusted prediction at a profile" is available as an
explicit option and **shares the machinery** with D3-flavour 2 (the reference-profile evaluation). **Note:**
the old draft's marginal effect was MER-at-reference — we make **AME the default** and keep MER as the opt-in.
**Dependency:** predicted-probability + AME **point** estimates in base R (`predict()`); **SEs/CIs and the
general cross-family/MNL/ordinal machinery via `marginaleffects` (Suggests, gated)** — hand-rolled delta-method
SEs are the reliability trap to avoid.

#### D6 — Survey weights: always `svyglm`, design-based, scale-invariant

Weighted models **always** go through `survey::svyglm` (never `glm(weights=)`). This one choice
simultaneously (i) **neutralises non-normalised population-total weights** (design-based sandwich SEs are
scale-invariant), (ii) gives **point estimates that match the weighted crosstabs**, and (iii) yields honest
inference — resolving the maintainer's discrepancy-vs-significance tension in a single move. **No weight
normalisation** (svyglm handles it; Kish `n_eff` is only a last-resort stopgap and is not needed). Accept, in
increasing flexibility (maintainer chose the widest): a **weight column** (default `svydesign(ids=~1,
weights=~w)`, `quasibinomial`/`quasipoisson` to silence the non-integer warning) → optional **`ids`/`strata`/
`fpc`** pass-through (correct clustered-design SEs — French national surveys are clustered) → a **prebuilt
`survey::svydesign` / `svrepdesign`** object (replicate weights, calibration). Document that `ids=~1` ignores
clustering/stratification and can understate variance. Under weights the **glance stats degrade** (D7): LR/
profile → **Wald / `regTermTest`** (Lumley-Scott 2014); pseudo-R² → **`survey::psrsq`** (Cox-Snell/Nagelkerke,
Lumley 2017 — McFadden is not survey-native); AIC → **Rao-Scott survey AIC** (Lumley-Scott 2015); suppress
naive R²/F/logLik/deviance.

#### D7 — Unified model / test-summary framework (footer), shared with `tab()`

**Generalise the existing `test` table-attribute** (§16/§24 — the tidy `[row_var col_var test statistic df1
df2 pvalue n variance min_e]` tibble) into ONE shared **GOF/summary** attribute used by **both `tab()` and
`tab_reg`**; the `test` vocabulary grows: today's `chi2`/`F_welch`/`F_classic` plus `lr_null`/`mcfadden_r2`/
`nagelkerke_r2`/`aic`/`bic`/`dispersion`/`n`/… (each row keyed by column, carrying `statistic`/`df`/`pvalue`/
`value`). This is the direct analogue of the crosstab chi2/ANOVA line, so it reuses the whole rendering path.

- **Default regression footer:** **N + LR-test-vs-null + McFadden R² + AIC/BIC**; lm: **N + R²/adjR² + F-test**
  (+ residual SE). All computable dependency-light from the fit (`null.deviance − deviance` on `df.null −
  df.residual`; `1 − logLik/logLik_null`; `AIC()/BIC()`). A shared **`stats=`** argument selects the set
  (per-context defaults: crosstab → the chi2/ANOVA test; regression → the above).
- **Dispersion flag** for **poisson / grouped-binomial only** (NOT ungrouped 0/1 — the dispersion parameter
  is not identifiable for Bernoulli data): print the **Pearson dispersion** `Σ(pearson resid)²/df.residual`
  (better-behaved than deviance/df), flag at **>1.5** (strong **>2**); correction = quasipoisson (scaled SEs,
  **no AIC**) or `MASS::glm.nb` (has AIC).
- **Multi-model comparison** — a **`compare = c("none", "null", "baseline", "sequential")`** argument (default
  `"null"`; `baseline=` picks the reference column): each column's LR test vs the null / a chosen baseline /
  the previous model (`anova(m1, m2, test="LRT")`; **LRT ≡ Chisq** — chi² for binomial/poisson, **F** for
  gaussian/quasi). A **central same-N / same-response / same-family guard** (mirrors `anova()`'s own error)
  **falls back to AIC/BIC + a message** on non-nesting or N-mismatch. Under survey there is no true likelihood
  → the **working (Rao-Scott) LR / Wald** test (`regTermTest` / `anova.svyglm`), **relabelled** so no true-LR
  claim is made. (No mainstream table package auto-inserts a between-column nested test — Stata `nestreg` /
  `lrtest` are the prior art — so this footer is a genuine differentiator.) A full model-summary line is kept
  per column.
- **Rendering, unified with crosstabs:** console → a footer block (generalise `print_chi2()`); exports →
  appended rows (generalise `tab_pvalue_lines()`/`tab_materialize_extras()`), with the maintainer's **border
  rule** — a box around **each model's whole summary block**, **no** internal borders between its lines. Solve
  the "which test?" ambiguity (the bare `pvalue` row name; different tests per column) by **embedding the
  label in the cell**: `"2.9% (Chi2)"`, `"0.4% (LR vs null)"`, `"F, Welch"` — self-documenting, generic row
  label. **Full-cell significance colour is the default**; **partial-colour** (numeric part only, suffix
  black) is a documented **future refinement** (feasible on all four backends — console crayon substring,
  kable `<span>`, Excel rich-text — but cross-backend cost, so not day-one). Excel keeps the raw value behind
  the formatted text.

#### D8 — Inference: CI ⇄ stars exact duals (carry 12a §20/§36)

`method="wald"` default (in-house `exp(coef ± crit·se)` + Wald p; glm `qnorm`, svyglm `qt` with design df),
`"profile"` opt-in (unweighted glm, `MASS`, LR-test p). Both keep **CI ⇄ p ⇄ stars exact duals**.
`color_signif` default **`"grey_non_signif"`** for OR/IRR (greys effects whose interval covers the neutral
value). For gaussian β the neutral is 0 (reuse the `diff` significance gate); for OR/IRR it is 1 (the Phase-12a
`"or"` gate). This extends 12a's four inert fmt reader-patches to IRR (same `ci_type="or"` machinery) and to
gaussian β (existing `ci_type="diff"`), with **no new fmt fields**.

#### D9 — Formulas: tidyselect default, formula escape-hatch for experts

Variables via **tidyselect / character vectors** by default (consistent with `tab()`); a **formula
escape-hatch** — `tab_reg(data, y ~ x1 + poly(x2, 2) + x1:x3)` — for power users, the RHS driving the model
and the row-skeleton built from the fitted terms. Documented caveats: compound terms (interactions, `poly()`,
`I()`) render as **term rows, best-effort**, not clean level rows; `^` in a formula means **factor crossing**,
not squaring (use `I(x^2)`/`poly()`).

#### D10 — Dropped features reinstated (git study; maintainer-chosen)

Reinstate: **`split_var`** (fit the regression within each level of a grouping variable → subtables — the
regression analogue of `tab()`'s `tab_vars`); **`multiplicator`** (a continuous predictor's effect per k
units, e.g. OR per decade of age); **`empirical_OR`** (raw empirical odds/% beside the model-adjusted OR —
the "OR + PCT" layout, connecting the model to the descriptive crosstab). The **summed-score binomial** goes
via D2. The old `readable_OR` **predicted-probability / absolute-odds** columns are **folded into the AME
mode** (D5), not revived separately. **`or_plot`** (OR forest plot) and **`lm_plots`** (glm/lm diagnostics)
stay **deferred to a later display phase**. (Confirmed: the old marginal effect was MER-at-reference,
superseded by AME.)

#### D11 — Dependency-light footprint

Engine: base `stats` (lm/glm) + **`nnet::multinom`** + **`MASS::polr`/`glm.nb`** — `nnet` and `MASS` are R
**Recommended** packages (always installed) → **zero new dependency** for MNL, ordinal, and negbin. `broom`
and `survey` are already Suggests. The **only new Suggests is `marginaleffects`** (gated, AME inference only;
point estimates via base `predict()`). `ordinal` (clm) / `VGAM` are optional Suggests for the partial-PO
fallback (D4). Explicitly **not** pulled in: parsnip/tidymodels/hardhat/poissonreg (dropped in 12a) and the
heavy `performance`/`parameters` stack (their footer stats are computed in-house from the fit).

### 12c-i DONE (2026-07-13) — `tab_reg` core engine + effect columns

Shipped the core (gaussian β / binomial OR / poisson IRR), the `tab_logit`/`multi_logit` binomial
wrappers, statistical-parity goldens, per-variable `reference=` levels, and `exponentiate="nongaussian"`.
Full suite green (FAIL 0, PASS 1927), colour/golden byte-identical, `devtools::document()` done.

- **Engine:** `R/tab_logit.R` → `R/tab_reg.R` (old files emptied, `git rm` pending). `tab_reg()` over ONE
  engine (`stats::lm`/`glm`, `survey::svyglm`, `broom::tidy`) with `reg_*` helpers; `predictors` char-vec =
  one model (dependent may be a vector → column per dependent) vs named list = model comparison;
  `exponentiate` drives the fmt shape (D1): additive β → `diff`; multiplicative OR/IRR → `or`. CI ⇄ p exact
  duals (z for fixed-dispersion glm, t(df) for lm/quasi/svyglm; profile+LR opt-in, D8).
- **fmt integration decision (the maintainer's `type` question):** the effect-size-gradient choice for β
  makes a dedicated `type` the clean routing point (the crosstab `get_ref_var()` standardization has a
  refrow-at-END grouping meaningless for a regression skeleton; `display="diff"` can't render a raw coef).
  Resolved with the FEWEST moving parts: **one new `type` VALUE `"coef"`** (β only) + **one new `display`
  TOKEN `"coef"`** (raw signed render) + **reuse the `var` FIELD** for var(Y) (the β/SD(Y) colour
  standardizes by its OWN `var`, not `get_ref_var()`). **No new fmt fields, no new attributes** — the
  18-field/9-attribute contract holds. OR/IRR keep `type="row"` (proven, unchanged). `fmt_color_plan()` also
  excludes reference rows from the diff/ratio/or colour (`gate & !is_refrow`): byte-identical for crosstabs
  (their ref cells are diff=0/OR=1 → already slot 0), it uncolours the regression **intercept** (in_refrow
  but a non-neutral baseline). Excel unchanged (a `coef` cell hits `excel_numfmt_code`'s plain-number branch).
- **β colour = effect-size gradient** (maintainer decision over single-tone / |z|): β/SD(Y) vs the
  `mean_diff` (Cohen 0.2/0.5/0.8/1.2) breaks — the additive twin of OR-by-ratio; verified (a large
  standardized β colours; a tiny-but-significant one stays grey — practical vs statistical significance).
- **Deferred to 12c-ii:** summed-score grouped binomial, formula escape-hatch, contrasts. **Cosmetic
  (Phase 13 legend redesign):** the β legend shows SD breaks as `%`, the IRR legend says "OR", a `coef`
  md cell reuses a `pXX` class (self-consistent — regression tables aren't mixed with pct columns).

### 12c-ii DONE (2026-07-13) — grouped binomial + formula escape-hatch

- **Summed-score grouped binomial (D2):** new `tab_reg(trials = )` arg (binomial only). A numeric
  summed-score outcome `0..q` is fit as `glm(cbind(score, trials-score) ~ ., binomial)` (weighted →
  `svyglm` quasibinomial), reusing the OR/`or` fmt shape unchanged. `trials`: `NULL` (default → binary
  logit), an integer / per-dependent named vector, or `TRUE` (observed max per dependent). Column
  label = `"<dep>: OR"` (a score has no positive level → skip `reg_positive_level`). `exponentiate =
  FALSE` gives the β (coef) shape. Validates integer-valued `0 ≤ score ≤ trials`; errors for a
  non-binomial family; the ordinary >2-level binomial abort now hints at `trials=`. Parity locked vs
  hand `glm(cbind(...))`.
- **Formula escape-hatch (D9):** `dependent` now accepts a model formula (`predictors` defaults to
  `NULL`; exactly one of formula / `predictors`). `reg_parse_formula()` classifies it: a **simple**
  formula (bare response ~ bare main-effect columns) reduces losslessly to the dependent+predictors
  character path (`identical()` to the equivalent call — zero new skeleton code); a **compound** one
  (interactions / `poly()` / `I()` / calls) is a single model fit **verbatim** with a best-effort
  skeleton read from the fitted terms (`reg_skeleton_from_fit()`: pure-factor main effects → level
  rows + reference; every other term → one row per assigned coefficient column, `term` = the
  model-matrix name so `reg_column()` aligns). `trials`/`inverse_two_level_factors` do not apply to a
  compound formula (the user controls the LHS); explicit `family` required when the response is a call.
- **Refactor:** `reg_build()` now fits-all then columns-all so the skeleton can come from the fit;
  `reg_fit()` gained `trials`/`formula` params and returns `$fit`. `reg_prep_binary` abort improved.
- **Not done (unchanged from 12c-i):** the Phase-13 legend cosmetics above.

### Phasing (12c → 12i — re-cut 2026-07-13; per-phase detail in the CLAUDE.md roadmap)

The build is re-cut into **fresh-session Phases with commit-and-verify increments** (the old monolithic
"12c tests / 12d rewrite / 12e jamovi" is dropped; tests are folded into every phase's gate). Each build
phase commits only with **statistical-parity goldens green** vs base `glm`/`lm`/`svyglm`/`nnet::multinom`/
`MASS::polr` (unweighted + survey):

- **12c — `tab_reg` core:** engine + family dispatch (binomial parity with 12a → gaussian β / poisson IRR),
  per-column effect labels, `exponentiate="nongaussian"`, tidyselect + per-variable named-vector references,
  summed-score grouped binomial, the formula escape-hatch, contrasts.
- **12d — nominal & ordinal outcomes:** one MNL (`nnet::multinom`, j-vs-ref OR) + proportional-odds
  (`MASS::polr`, diagnosed) + the "j vs rest OR at reference profile" flavour.
- **12e — AME / predicted-probability mode:** the `effect=` axis (base `predict()` points + `marginaleffects`
  Suggests for SEs); MER-at-reference opt-in; extended to MNL/ordinal.
- **12f — unified model/test-summary footer + model comparison** (cross-cutting, touches `tab()`): generalise
  the `test` attribute, the default footer stats, the dispersion flag, multi-model LR (vs null / baseline /
  sequential), in-cell test labels + border rule + shared `stats=` arg.
- **12g — survey design + companion features:** ids/strata/fpc + prebuilt design objects + degraded glance;
  `split_var`; `multiplicator`; `empirical_OR`.
- **12h — jamovi UI:** `jmvtab_reg` / `jmvtab_logit` (needs the maintainer's `.h.R` regen).
- **12i — display phase (deferred):** `or_plot` forest plot, `lm_plots` diagnostics, the visible OR-CI
  bracket, the OR+ME / OR+PCT composite layouts.

### Sources (Phase 12b)

**Ecosystem / effect measures (Pass 1):** gtsummary `tbl_regression` <https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html>
and `tbl_merge` <https://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.html> ; sjPlot `tab_model`
<https://strengejacke.github.io/sjPlot/articles/tab_model_estimates.html> ; modelsummary
<https://modelsummary.com/vignettes/modelsummary.html> + `gof_map` <https://github.com/vincentarelbundock/modelsummary/blob/main/R/gof_map.R> ;
parameters `compare_parameters` (`exponentiate="nongaussian"`) <https://easystats.github.io/parameters/reference/compare_parameters.html> ;
performance `r2` <https://easystats.github.io/performance/reference/r2.html> ; broom `glance.lm`/`glance.glm`/`glance.svyglm`
<https://broom.tidymodels.org/reference/glance.glm.html> ; overdispersion in counts
<https://www.theanalysisfactor.com/glm-r-overdispersion-count-regression/> ; UCLA pseudo-R²
<https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/> ; Long & Freese,
*Regression Models for Categorical Dependent Variables Using Stata*.

**MNL / one-vs-rest / survey weights (Pass 2-3):** Begg & Gray (1984), *Biometrika* 71:11-18 (via mlogitBMA
vignette <https://cran.r-project.org/web/packages/mlogitBMA/vignettes/conversion.pdf>) ; Rodríguez GLM notes
<https://grodri.github.io/glms/notes/c6s2> ; Werth *Categorical Regression* (RRR≡OR)
<https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html> ; Li et al. 2024
(dichotomized vs MNL, incoherent probabilities) <https://pmc.ncbi.nlm.nih.gov/articles/PMC10889078/> ;
Rifkin & Klautau (2004) "In Defense of One-Vs-All", *JMLR* 5:101-141 <https://www.jmlr.org/papers/volume5/rifkin04a/rifkin04a.pdf> ;
Cheng & Long (2007) IIA <https://journals.sagepub.com/doi/10.1177/0049124106292361> ; Allison IIA
<https://statisticalhorizons.com/iia/> ; Lumley `svyglm` <http://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svyglm.html> ;
CRAN survey manual <https://cran.r-project.org/web/packages/survey/survey.pdf> ; Lumley & Scott (2017)
"Fitting Regression Models to Survey Data" ; Displayr on sampling weights
<https://www.displayr.com/the-correct-treatment-of-sampling-weights-in-statistical-tests/> ; Winship & Radbill
(1994) <https://journals.sagepub.com/doi/10.1177/0049124194023002004> ; Stata pweights/aweights FAQ
<https://www.stata.com/support/faqs/statistics/weights-and-summary-statistics/>.

**Ordinal / AME (Pass 3):** Agresti *Analysis of Ordinal Categorical Data* <https://alanagresti.com/ordinal/ord.html> ;
Williams `gologit2` (proportional-odds "often violated", Brant/partial-PO) <https://www.stata.com/meeting/4nasug/gologit2.pdf> ;
UCLA ordinal logit (`MASS::polr`) <https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/> ; Mood (2010)
"Logistic Regression: Why We Cannot Do What We Think We Can Do", *Eur. Sociol. Rev.* 26(1):67-82
<https://academic.oup.com/esr/article-abstract/26/1/67/540767> ; Williams (2012) "Using the margins command",
*Stata Journal* ; Bartus (2005) marginal effects, *Stata Journal* ; Arel-Bundock et al. (2024) `marginaleffects`,
*JSS* 111(9) <https://marginaleffects.com> ; delta-method (why averaging `predict` SEs is wrong)
<https://cran.r-project.org/web/packages/modmarg/vignettes/delta-method.html> ; R "Recommended" packages
(`MASS`/`nnet` always installed) <https://cran.r-project.org/web/packages/MASS/index.html>.

**Model comparison / dispersion / survey glance (Pass 4):** `survey::regTermTest` <https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/regTermTest.html> ;
`survey::anova.svyglm` <http://r-survey.r-forge.r-project.org/pkgdown/docs/reference/anova.svyglm.html> ;
`survey::psrsq` (Lumley 2017 pseudo-R² under complex sampling) <https://rdrr.io/rforge/survey/man/psrsq.html> ;
Lumley & Scott (2015) "AIC and BIC for modelling with complex survey data", *JSSAM* 3(1) ; overdispersion /
R manual `anova.glm` (LRT≡Chisq; chi²-vs-F by family; same-data error) <https://stat.ethz.ch/R-manual/R-patched/RHOME/library/stats/html/anova.glm.html> ;
Stata `nestreg` <https://www.stata.com/manuals/rnestreg.pdf> + `lrtest` <https://www.stata.com/manuals/rlrtest.pdf> ;
RMPH §8.8 survey LR-vs-Wald <https://bookdown.org/rwnahhas/RMPH/survey-likelihood.html> ; Bolker GLMM FAQ
(Bernoulli overdispersion not identifiable; Pearson φ; >2 rule) <https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html> ;
Hilbe "Can binary logistic models be overdispersed?" <http://www.highstat.com/Books/BGS/GLMGLMM/pdfs/HILBE-Can_binary_logistic_models_be_overdispersed2Jul2013.pdf> ;
quasi-binomial ≈1 on 0/1 <https://randomeffect.net/post/2020/10/12/quasi-binomial-in-r-glm/> ; NB vs quasipoisson
<https://metricgate.com/blogs/negbin-vs-quasi-poisson-overdispersion/> ; R "Recommended" set incl. MASS/nnet
<https://cran.r-project.org/web/packages/nnet/nnet.pdf> ; pseudo-R² closed forms (DescTools `PseudoR2`)
<https://andrisignorell.github.io/DescTools/reference/PseudoR2.html> ; marginaleffects response-scale averaging
<https://larmarange.github.io/broom.helpers/articles/marginal_tidiers.html> ; `MASS::glm.nb`.

**Git study:** commit `6e47bab^` — `R/tab_logit.R` / `R/tab_logit_2.R` (pre-package parsnip draft:
`nb_questions`, `split_var`, `multiplicator`, `empirical_OR`, `readable_OR`, `or_plot`, `lm_plots`).
