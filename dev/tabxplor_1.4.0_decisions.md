# tabxplor 1.4.0 — settled design decisions (grounded)

Detailed rationale behind the phase bullets in `CLAUDE.md` (§ 1.4.0 roadmap). CLAUDE.md holds the
**concise** decisions; **this file holds the grounding** (code `file:line` + statistics) so a fresh
session can implement without re-deriving it. Written 2026-07; all decision questions resolved 2026-07-07.

**How to read.** *Aim* (the governing rule) → *Status & open items* (what is settled, what is still open)
→ the numbered decisions **§1–§13** (each self-contained, with its own grounding) → *How the decisions cohere* (the
closing synthesis). The **§N numbers are stable cross-reference anchors** —
CLAUDE.md and this file both cite "§9", "§12", … — so **do not renumber them**; append new decisions as §14+.

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

**Still open** — the only unsettled points; each names the phase that must close it.

### G1 (confirm in Phase 2) — the aggregate carries sufficient statistics, not counts

A **count-only** aggregate (`n`, `wn`) cannot yield a mean/variance/CI/t — the verified `weighted.var`
double-scan ([tab.R:5571](../R/tab.R#L5571)) is the symptom. The core must be a **sufficient-statistics
aggregate**, and it is **heterogeneous** — two branches, so every core transform (pct/diff/OR/CI/chi2/tests)
dispatches on branch (the "one vectorised impl each" is really one factor path + one numeric path each):
- **numeric col_var**, per `tab_vars × row_var × col_var` group: `n`, `Σwt`, `Σwt²`, `Σ(wt·x)`, `Σ(wt·x²)`
  (+ unweighted `Σx`, `Σx²` for the unweighted-`n` variant), so mean `= Σwx/Σw` and variance come in
  **one pass**.
- **factor col_var**, per `… × col_var-cell`: `n`, `wn` (= `Σwt`), and `Σwt²`.

`Σwt²` is the **one extra sum** (additive, rolls up to the base like `wn`, cheap) that unlocks Kish
`n_eff = (Σwt)²/Σwt²` for the weighted-inference rule (§14) and design-effect reporting. Confirm the exact
schema when Phase 2 builds the core — near-certain (correctness + the headline perf win + §14 all depend
on it).

### C2 (Phase 1/3) — `tot_wn = wn/pct` recovery on empty cells

The `get_tot_wn()` accessor (§11) can't recover the weighted base of an empty cell (`pct==0`); the sibling
fallback is robust within a non-empty row but fails for a fully-empty row/group. Default lean: keep the
sibling fallback (stay at 18 fields); store `tot_wn` only if it bites in practice.

### U4 (Phase 6) — the col%-reference (`refcol`) side

§4 fixes the **row** `ref` as a per-row_var named vector but leaves the col% side implicit. Spell out how
the `refcol` attribute / `diff_index(…, pct="col")` ([tab.R:2644](../R/tab.R#L2644),
[5774](../R/tab.R#L5774)) interacts with the globalised axis, and fix the `fmt()` `refcol`-cast bug (casts
`totcol` instead of `refcol`, [fmt_class.R:274](../R/fmt_class.R#L274)) in the same pass — already on the
§9 touch-list.

### Phasing — split the Phase 1 field pass into 1a / 1b

The field pass (§9) precedes the Phase 2/3 core rewrite, so populating the new fields from the *old*
step-chain means throwaway glue. Prefer **1a** (contract: field defs + accessors + arithmetic/cast/format),
then **1b** (writers), folding 1b into Phases 2/3 where the core actually computes the fields. 1a keeps the
new fields **NA-defaulted** (not printed → golden unchanged); regenerate golden only after 1b writes them.

### D2 (Phase 2 vs Phase 6) — split §5 into *internal* globalisation and *argument-surface* deprecation

Phase 2's aggregate-core is described (CLAUDE.md Phase 2) as already relying on "the globalised row_var
axis (§5)", but the row_var-axis globalisation is scheduled for **Phase 6**. Resolve the forward
dependency by splitting §5 in two: the **internal** collapse (the core assumes one shared setting per
row_var axis — `OR/pct/color/comp/ci/chi2/ref2`) lands **with the Phase 2 core**; the **argument-surface**
change (deprecation warnings, `tab()`/`tab_many()` merge, the named-vector `ref`) lands **Phase 6**.
Between the two, `tab_many()`'s per-row_var arg surface still exists but the core silently uses the first /
shared value (no divergent-per-row_var math) — document the interim.

### G2 (Phase 3) — omnibus/chi2 parity must match `chisq.test` exactly

The vectorised closed-form chi2 (and the new mean-table F) must reproduce `chisq.test()`'s defaults to keep
`test-golden.R` green — chiefly the **Yates continuity correction on 2×2 tables** (`correct=TRUE` by
default). Decide per test: match Yates on 2×2, or document a deliberate divergence. The planned
"p equals `chisq.test`" parity test (CLAUDE.md Phase 3) locks it.

### S3 (Phase 6) — `tab()`'s NA / population-consistency semantics must migrate into the core

`tab()`'s historical raison d'être (its CLAUDE.md Global-Architecture note) was *consistent `n` / NA
handling for a single row_var × col_var* — "who is in `n`?". Phase 6 turns `tab()` into a thin shim, so
that normalisation logic must be re-expressed as core/`as_tab_counts()` boundary rules (which rows count
toward each base under each `na` mode), not silently dropped. Spell it out when Phase 6 merges the two.

### S4 (Phase 6/7) — fate of `tab_spread()` / `tab_compact()` under `output_list`

`compact` (arg) is deprecated and `tabxplor.compact` (option) dropped (§6), but the underlying
`tab_compact()` (the merge engine, `tab_classes.R`) and `tab_spread()` (`tab.R`) functions are unaddressed.
Decide: `tab_compact()` becomes the internal merge invoked by `output_list=FALSE` (kept, maybe unexported);
`tab_spread()` — keep / soft-deprecate / retire. Name their fate in Phase 6 (merge) or Phase 7 (export).

---

## Decision map

- **Type system & fields**: §1 (CI as bounds), §2 (`tot_n` base), §3 (diff vs ratio; `rr`→`ratio`),
  §9 (the 18-field list + `/vctrs-field` touch-list), §11 (is `tot_n`/`tot_wn` enough?), §12 (`pvalue`).
- **References & axes**: §4 (`ref` as a per-row_var named vector), §5 (row_var-axis globalisation),
  §6 (`totrow`/`totcol` + singular-arg deprecations).
- **Display, output & export**: §7 (col% + several row_vars → transpose at export), §8 (exporter
  base+list methods; Excel engine → Phase 9), §10 (total-column base range), §13 (output-shape table).
- **Inference policy** (review session 2): §14 (weighted estimate + unweighted `n`), §15 (CI/stars
  duality — score test ⇄ Newcombe), §16 (test-result data-model placement + display future).
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
  `pvalue`. Wilson (`ci="cell"`) is already the score dual; means/OR intervals are already the dual of
  their Welch-t / log-OR tests. AC stays the default only when stars are off. Detail + grounding: §15.

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
  - **pct columns**: `ratio` = relative risk `cell_pct / ref_pct` (this is exactly what drove the old
    `mean`-overload "×2 rule", and it is also the RR step inside odds-ratio calculation — so reusing the
    RR field is semantically correct). `diff` = `cell_pct − ref_pct` (unchanged, safe).
  - **numeric/mean columns**: `ratio` = `cell_mean / ref_mean` (the OLD numeric-`diff` behaviour);
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

**Decision** (Phase 7): keep `pct="col"` **single-row_var** as-is. The col%-multi-row_var path is a
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
Phase 7 — it is the mechanism for the above. Do not wire it in before Phase 7.

**compact + tab_vars**: deferred. Merging tables that carry tab_vars needs compound
`group_by(tab_vars, row_var)`, interleaving row_vars within each tab_var block, per-(tab_var × row_var)
reference re-scoping, chi2 alignment, and two-level print/kable rendering
([tab_classes.R:969-975](../R/tab_classes.R#L969)) — revisit during Phase 7. Until then, tables with
tab_vars stay a list/grouped structure regardless of `output_list`.

---

## 8. Exporters — base method + list method

**Decision** (Phase 7): every exporter (`tab_xl`, `tab_kable`, `tab_md`, `tab_plot`) has (a) a base
method for a single `tabxplor_tab`, and (b) a method for a **list of tables** that renders them
**one-after-another, not merged** (kable: an HTML container holding several tables; xl: sheets/blocks;
md: sequential). This is the export side of the "different tables → list() → export" escape hatch in § 5.

**Excel engine (openxlsx → openxlsx2) — isolated to Phase 9 (follow-up decision, 2026-07-07).** Phase 7 builds the shared
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
  attribute. Fix the `fmt()` `refcol`-cast bug (casts `totcol` instead of `refcol`,
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
(Phase 3 for the fmt/print side; the exporters mirror it in Phase 7.)

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
- **Jamovi (Phase 8)** — **no benefit.** Recompute (e.g. new `conf_level`) runs off the **cached
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

| Use case | Estimator (what is compared) | Test → `pvalue` | Base | Notes |
|---|---|---|---|---|
| **Factor col_var, `ci="diff"`** | cell proportion vs reference-row proportion | two-sided **two-proportion score test** (`prop.test()` / manual score `z`); dual of the **Newcombe** difference CI | weighted `pct`, unweighted `tot_n` of cell & of the `in_refrow` cell (§14) | When stars are on, the stored diff CI switches AC→Newcombe so bracket ⇄ stars agree (§15). `ref="tot"` → test vs complement (caveat). |
| **Factor col_var, `OR=TRUE`** (empirical OR, no logit) | 2×2 odds ratio: (cell level vs `ref2` level) × (row vs `ref` row) | **Wald test on log(OR)**, Woolf `SE=√(1/a+1/b+1/c+1/d)`, `z=log(OR)/SE`; dual of the log-scale OR CI (`exp(logOR ± z·SE)`) | weighted OR point estimate; **unweighted** 2×2 counts for the SE (§14) | Haldane–Anscombe +0.5 when any of a,b,c,d = 0. Empirical only — distinct from logit. |
| **Numeric col_var, `ci="diff"`** | cell mean vs reference-row mean | **Welch two-sample t-test** (unequal variance): `t=(x̄c−x̄r)/√(s²c/nc+s²r/nr)`, Welch–Satterthwaite df | weighted mean + weighted `var`, unweighted `n` (§14) | Confirms the maintainer's intuition: t-test p<5% ⇒ significant cell-vs-ref mean difference. Weighted var + **unweighted n** (Kish `n_eff` opt-in), *not* full survey design. `ref="tot"` caveat applies. |
| **`tab_logit()`** | logistic-regression coefficient | model **Wald z** p-value, straight from `broom::tidy(…)$p.value` | model n | Obvious case; also fills OR `ci_inf`/`ci_sup` from `conf.int=TRUE`. |
| **`color="contrib"`** (chi2 cell contribution) | cell count vs its chi2 expected | **standardized Pearson residual** → normal p (`abs(resid) > 1.96 ⇒ p<0.05`) | full margins (already computed for chi2) | Optional: per-cell significance for the contribution mode; residual sign gives direction. |
| **Ratio / relative-risk display** (if starred) | cell vs reference proportion ratio (`ratio` field) | **Wald on log(RR)**, `SE=√((1−p1)/(n1·p1)+(1−p2)/(n2·p2))` | weighted RR estimate; unweighted `tot_n` of both (§14) | Only if RR significance is surfaced; analogous to the OR row. |
| **`ci="cell"`** (interval around the cell itself, no reference) | — | **`pvalue = NA`** (H0: p=0 / μ=0 is not meaningful) | — | No per-cell star; the interval is purely descriptive. |

**Whole-table omnibus tests are NOT the per-cell `pvalue`** — they live at table/column level (§16), one
number under each table:

- **Factor tables**: Pearson chi2 p (existing; unweighted counts — §14).
- **Mean tables (Q4)**: **ANOVA / Welch's F** across the row_var groups — the true mirror of chi2 ("are
  the group means different at all"). Stored the same way chi2 is (§16 — a table-level attribute; a
  per-column test on a column attribute), carrying the F result for numeric columns. The per-cell Welch-t
  above is the *cell-vs-reference* companion (stars), **not** the omnibus — the two coexist and answer
  different questions (Q4 confirmed both: ANOVA for the table line, t for the cell).

Where the field is written: fill `pvalue` in the same aggregate-core transform that fills `ci_inf`/`ci_sup`
(Phase 3), so the closed-form test statistics are computed once, **vectorised, from the
sufficient-statistics aggregate** (open item G1, *Status & open items*) — never per-cell `prop.test`/`t.test` calls in a loop.

---

## 13. Output shape — return-type truth table (2026-07-07)

`output_list` (default `FALSE`) replaces `compact`; the `tabxplor.compact` option is dropped (§6, Phase 6).
The exact return type by `row_vars` × `tab_vars` × `output_list`:

| row_vars | tab_vars | output_list | Returns |
|---|---|---|---|
| 1 | none | FALSE (default) | a single `tabxplor_tab` |
| 1 | none | TRUE | a length-1 `list(tabxplor_tab)` |
| ≥2 | none | FALSE (default) | one **merged** `tabxplor_tab` (row_vars stacked — the old `compact=TRUE` shape, now the default) |
| ≥2 | none | TRUE | a `list` of one `tabxplor_tab` per row_var |
| 1 | present | FALSE | a single `tabxplor_grouped_tab` |
| 1 | present | TRUE | a length-1 `list(tabxplor_grouped_tab)` |
| ≥2 | present | any | a `list` of `tabxplor_grouped_tab` — merging across row_vars **with** tab_vars is deferred (§7), so these return a list even at `output_list=FALSE` |

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
  `ci="cell"` already uses **Wilson** = the score dual (no change). Means use the **Welch-t** interval/test
  pair (exact duals); empirical OR uses the **log-OR Wald** interval/test pair (exact duals on the log
  scale). So only the *proportion-difference* method needs the AC→Newcombe swap-under-stars.
- The maximally-coherent end state (always Newcombe/Wilson, drop AC) is deferred: it would change default
  numeric output (golden) with no benefit when stars are off. Revisit only if AC's lack of a dual ever
  surprises a user.

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
  attribute** (the existing `chi2` attribute, generalised — rename to a neutral `test` slot, or add an `f`
  sibling — so it also holds the F result; "properly remove chi2 attribute leftovers", CLAUDE.md Phase 3).
- **Whole-column / whole-variable test** (a per-column omnibus, when meaningful) → a **column attribute**
  on that `tabxplor_fmt` column.
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

| Break | Who it affects | Why accepted / mitigation |
|---|---|---|
| Numeric `$diff` flips **ratio → difference** (§3) | code reading `$diff` on *numeric/mean* columns | The one substantive break; numerics are rarely `$`-extracted, pct `$diff` (the real surface) is unchanged, and the ratio survives in `$ratio`. |
| `$rr` **disappears** (renamed `ratio`, §3, §9) | code reading `$rr` | The `rr` field was **never** used by any code or by the maintainer. `$ratio` replaces it. |
| `$mean` on **pct** columns changes (overload removed, §3) | code reading `$mean` of a pct column | Was an internal "×2-rule" ratio overload; the ratio now lives in `$ratio`; `$mean` on pct columns → `NA` (an honest value). |
| Low-level `vctrs::field(x,"ci")` and **setting** `ci` stop working (§1, §9) | code poking the raw `ci` field | Rare/internal. `$ci` / `get_ci()` still work (recomputed from the bounds); the `fmt(ci=)` constructor arg is kept. |
| `tab_many()` `compact` arg **deprecated**; `tabxplor.compact` **option dropped** (§6) | `tab_many(compact=)` / option users | Soft-deprecated arg still maps onto `output_list`; the option is replaced by the `output_list` arg. |
| Changing the **CI confidence level on a built table** now needs a re-run (§1) | post-hoc `conf_level` tweakers | Bounds are stored at one level (can't rescale a stored asymmetric bound). Stars *are* re-thresholdable without re-run (the `pvalue` is level-free). Re-run `tab()` for a different CI level. |

**Explicitly NOT broken (guardrails held):** `tab_many()`'s **list return type** for `≥2 row_vars` (Q7,
§13) — preserved; pct `$diff`, `$pct`, `$n`, `$wn` and the other user-read fields — unchanged; every
public function/argument — kept (soft-deprecated at most).

**Explicitly out of scope (document, don't "fix"):** **multiple comparisons.** Stars decorate every cell,
so a large table runs many tests — tabxplor applies **no** correction (standard for exploratory crosstabs).
State this once in the CI/stars documentation so it is a conscious choice, not an omission; a
Bonferroni/BH option is a possible future `to think about`, not 1.4.0.

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
`tot_wn` edge) at the core; **D2** (the §5 internal-vs-arg-surface split), **G2** (chi2/F parity), **S3**
(tab()'s NA semantics) and **S4** (`tab_spread`/`tab_compact` fate) as the later phases reach them. The
public API
(user-facing functions, their arguments, the `tabxplor_fmt` fields) stays retro-compatible throughout;
only the internals are re-cut. That is the whole of 1.4.0.

