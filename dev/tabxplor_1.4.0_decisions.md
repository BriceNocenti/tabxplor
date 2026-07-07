# tabxplor 1.4.0 — settled design decisions (grounded)

Detailed rationale behind the phase bullets in `CLAUDE.md` (§ 1.4.0 roadmap). CLAUDE.md holds the
concise decisions; this file holds the grounding (code file:line + statistics) so a fresh session can
implement without re-deriving it. Written 2026-07 after the "other decisions to settle now" analysis.

## Aim (this governs every decision below)

1.4.0 = **refactor and simplify `tab()`/`tab_many()`**: strip the white-elephant flexibility that
real-world data analysis never uses, and **redesign the `tabxplor_fmt` vctrs-field architecture** (one
combined pass) to fit the simpler, faster model. Hard rule: the **public API stays retro-compatible**
(user-facing functions, their arguments, and the `tabxplor_fmt` fields users read with `$`/`mutate()`),
but the **internals may — and should — be radically redesigned** for consistency and performance —
remove legacy/dead paths, fuse them, and route everything through the one aggregate-core. Every section
below is one such simplification; none exists to add flexibility.

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

**All per-cell tests use the UNWEIGHTED base `tot_n`** (as chi2 already does, [tab.R:5285](../R/tab.R#L5285))
— significance is a statement about the real sample, not the reweighted one. The weighted estimate still
drives the displayed number and color; the p-value uses unweighted counts.

**Cross-cutting caveat — reference dependency.** When the comparison reference *contains* the cell
(`ref="tot"`, cell ⊆ total), the two groups are not independent, so an independent two-sample test is
mildly anti-conservative. Where the reference is detectably the total, test the cell against the
**complement** (reference − cell); when the reference is a disjoint row/subgroup (`ref="first"` or a
specific level) the two-sample test is exact. Implement the complement correction if cheap; otherwise
document the small, conservative-leaning approximation for `ref="tot"`.

| Use case | Estimator (what is compared) | Test → `pvalue` | Base | Notes |
|---|---|---|---|---|
| **Factor col_var, `ci="diff"`** | cell proportion vs reference-row proportion | two-sided **two-proportion score test** (`prop.test()` / manual score `z`); the Wilson-score dual of the Newcombe difference CI, agrees closely with the AC interval tabxplor defaults to | `tot_n` of cell & of the `in_refrow` cell | Matches `method_diff="ac"`. `ref="tot"` → test vs complement (caveat). |
| **Factor col_var, `OR=TRUE`** (empirical OR, no logit) | 2×2 odds ratio: (cell level vs `ref2` level) × (row vs `ref` row) | **Wald test on log(OR)**, Woolf `SE=√(1/a+1/b+1/c+1/d)`, `z=log(OR)/SE`; consistent with the log-scale OR CI (`exp(logOR ± z·SE)`) | the four 2×2 counts (unweighted) | Haldane–Anscombe +0.5 when any of a,b,c,d = 0. Empirical only — distinct from logit. |
| **Numeric col_var, `ci="diff"`** | cell mean vs reference-row mean | **Welch two-sample t-test** (unequal variance): `t=(x̄c−x̄r)/√(s²c/nc+s²r/nr)`, Welch–Satterthwaite df | `tot_n`/`n` + `var` field (both cells) | Confirms the maintainer's intuition: t-test p<5% ⇒ significant cell-vs-ref mean difference. Weighted: use the design/weighted `var` + effective n. `ref="tot"` caveat applies. |
| **`tab_logit()`** | logistic-regression coefficient | model **Wald z** p-value, straight from `broom::tidy(…)$p.value` | model n | Obvious case; also fills OR `ci_inf`/`ci_sup` from `conf.int=TRUE`. |
| **`color="contrib"`** (chi2 cell contribution) | cell count vs its chi2 expected | **standardized Pearson residual** → normal p (`abs(resid) > 1.96 ⇒ p<0.05`) | full margins (already computed for chi2) | Optional: per-cell significance for the contribution mode; residual sign gives direction. |
| **Ratio / relative-risk display** (if starred) | cell vs reference proportion ratio (`ratio` field) | **Wald on log(RR)**, `SE=√((1−p1)/(n1·p1)+(1−p2)/(n2·p2))` | `tot_n` of both | Only if RR significance is surfaced; analogous to the OR row. |
| **`ci="cell"`** (interval around the cell itself, no reference) | — | **`pvalue = NA`** (H0: p=0 / μ=0 is not meaningful) | — | No per-cell star; the interval is purely descriptive. |

**Whole-table omnibus tests are NOT the per-cell `pvalue`** — they stay column/table-level (the `chi2`
attribute / a test sidecar), one number under each table:

- **Factor tables**: Pearson chi2 p (existing; unweighted counts).
- **Mean tables (Q4)**: **ANOVA / Welch's F** across the row_var groups — the true mirror of chi2 ("are
  the group means different at all"). Store it exactly as chi2 is stored (extend the `chi2` attribute / a
  `test` sidecar to carry the F result for numeric columns). The per-cell Welch-t above is the
  *cell-vs-reference* companion (stars), **not** the omnibus — the two coexist and answer different
  questions (Q4 confirmed both: ANOVA for the table line, t for the cell).

Where the field is written: fill `pvalue` in the same aggregate-core transform that fills `ci_inf`/`ci_sup`
(Phase 3), so the closed-form test statistics are computed once, **vectorised, from the
sufficient-statistics aggregate** (review gap G1) — never per-cell `prop.test`/`t.test` calls in a loop.

---

## 13. Output shape — return-type truth table (review U1, 2026-07-07)

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
([tab.R:1498-1505](../R/tab.R#L1498)). 1.4.0 keeps that machinery. The one behaviour change vs today:
`≥2 row_vars, no tab_vars, output_list=FALSE` now **merges by default** (was a list; `compact` merged).
Exporters consume all shapes via the base method (single tab) + list method (§8).

---

## Sources (statistics)

- Binomial proportion CI (Wald / Wilson / Agresti-Coull asymmetry): <https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval>
- Odds-ratio CI symmetric on log scale, asymmetric after exp: <https://en.wikipedia.org/wiki/Odds_ratio> ; <https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module8-CategoricalData/PH717-Module8-CategoricalData5.html>
- Newcombe/Agresti-Coull difference-of-proportions coverage asymmetry: Newcombe (1998), summarised in the Wikipedia article above.

---

# Architecture review — consistency & soundness audit (2026-07-07)

Grounded second-pass review of this document + the CLAUDE.md 1.4.0 roadmap, verifying every load-bearing claim against the source (four parallel code sweeps of `fmt_class.R`, `tab.R`, `tab_classes.R`, `tab_xl.R`, `tab_md.R`). Ranked: **blocking gaps** (must be settled before the Phase 1 field set is frozen, or a second field surgery becomes necessary — the one thing the plan explicitly forbids), then **consistency issues**, then **under-specified / doc corrections**, then the **coherence & phasing** verdict.

> **Status (2026-07-07): all decision questions resolved** — Q1 deprecation posture → soft-deprecate, stay 1.4.0 (C1); Q2 → per-cell `pvalue` field (G2, §12); Q3 → `rr`→`ratio` rename (G3, §3); Q4 → ANOVA-F omnibus + per-cell Welch-t (G4, §12). Under-specified items **U1/U2/U3/U5/U6 actioned** (U3 → new Phase 9; U6 → `"ci"` in the mode list; U1 → §13 truth table). **Still open: G1** (sufficient-statistics aggregate schema — confirm in Phase 2) and **C2** (`tot_wn` recovery on fully-empty rows). The per-finding **Resolved / Status / → Actioned** lines below are the source of truth; the sections above (§1-§13) already reflect every resolution — this review is now a historical audit trail.

## Verdict

The direction is **sound**. The keystone (one aggregate-core), per-cell `tot_n`, CI-as-bounds, the diff/ratio split, the `tab()`/`tab_many()` merge, and the exporter-prep unification each target a **real, code-verified** problem (duplicated pct/total math, post-hoc `detect_totcols` approximation, the symmetric-bracket CI bug, the `mean`/`diff` overloads, three hand-rolled exporter preambles). The phasing is logical and the retro-compat guardrails are the right ones. Green-light — but four field-set-shaping questions and one version/deprecation-posture question must be answered first, because they change what the Phase 1 vctrs surgery must contain.

## Grounding — what checks out (evidence)

| Decisions-doc claim | Status | Evidence |
|---|---|---|
| 15 per-cell fields incl. `rr`, `or` | Confirmed | `new_rcrd()` list `fmt_class.R:1051-1056` |
| `ci` = single symmetric upper half-width; asym. bounds already lost | Confirmed | `tab_ci` `tab.R:4941` (`ci = upr.ci - est`); symmetric rebuild `fmt_class.R:1488-1489` |
| Significance = single `abs(diff) > ci` at one level (no p, no multi-level) | Confirmed | `color_formula` `fmt_class.R:2237-2252`; one `conf_level` `tab.R:4931` |
| `mean` field overloaded = pct ratio | Confirmed | written `tab.R:2580-2590`; read `fmt_class.R:2005-2009`; WARNING `fmt_class.R:2001` |
| numeric `diff` = ratio not difference | Confirmed | `diff_formula` `tab.R:5639-5652`; DESIGN note `fmt_class.R:2186` |
| pct base weighted (`wn`), CI/chi2 base unweighted (`n`) | Confirmed | `tab.R:2516-2522`; local `x_n`/`ref_n` `tab.R:2864-4877`, `4905` |
| mean path double-scans; `weighted.var` recomputes the mean | Confirmed | `tab.R:3283-3296`, `weighted.var` `tab.R:5571-5583` (has FIXME) |
| chi2 p on unweighted counts; contrib on weighted; both need full margins | Confirmed | p `tab.R:5286-5325`; `var_contrib` `tab.R:5657-5685` |
| No shared exporter prep (kable≈md duplicated, xl own, plot none) | Confirmed | `tab_classes.R:486-504` ≈ `tab_md.R:46-64`; `tab_xl.R:127-146` |
| `tab_xl` bypasses `format()`, uses **openxlsx v1** | Confirmed | `tab_xl.R:544-546,593-594`; `DESCRIPTION:30` (`openxlsx`, no openxlsx2) |
| mean/factor color-reference mismatch under col% | Confirmed & systematic | `fmt_class.R:2824-2828`, repeats at `:2830-2835`, `:2836-2841` |
| Output unwrap: bare tab if 1 row_var or compact | Confirmed | `tab.R:1540`, `1508-1511` |
| `OR/pct/color/comp/ci/chi2/ref2` per-row_var; `totaltab/ref` per-row_var; `levels/digits` per-col_var | Confirmed | recycling `tab.R:716-802` (`pct` per **both** axes, `:780-796`) |

Two **corrections** to the doc's own wording (facts, not judgment):

- **§2 mischaracterises `detect_totcols`.** It is purely **position-based** — each column maps to the first total column at/after its own index (`fmt_class.R:1271-1285`). There is **no** "different-NA-totals fallback" branch. The "shared last col_var total" is only the *implicit* outcome when a single trailing total exists. So the approximation the roadmap fears bites the **post-hoc / standalone recompute** path, **not** the primary calc (which already uses each col_var's own base via `tot_cols[[col]]`). This matches §11's honest conclusion — align §2's opening and Phase-1-bullet-#1's "introduces approximation in the default behaviour" wording with §11 (the benefit is *self-sufficiency for standalone stats*, not fixing the main path).
- The local base transmute the doc cites "as `tot_n` at `tab.R:4878-4900`" is actually named **`x_n`** (`tab.R:2864-4877`) with the reference base **`ref_n`** at `tab.R:4905`. Update the code-refs; the concept is unchanged.

## Blocking gaps — settle before freezing the Phase 1 field set

### G1. The aggregate must carry sufficient statistics, not counts — means break a count-only core

The keystone diagram says "count-aggregate (`n`, `wn` per cell)". That is **insufficient for numeric (mean) col_vars**: a mean/variance/CI/t-test cannot be recovered from counts. The verified `weighted.var` double-scan (`tab.R:5571`) is the symptom. The aggregate-core must be a **sufficient-statistics aggregate** carrying, per `tab_vars × row_var × numeric-col_var` group: `n`, `Σwt`, `Σ(wt·x)`, `Σ(wt·x²)` (and unweighted `Σx`, `Σx²` for the unweighted t-test), from which mean `= Σwx/Σw` and variance come in **one pass** — killing the 7.8 GB re-scan. Factor col_vars keep the count branch (`n`, `wn`). Decision needed: rename the keystone to a **"cell-aggregate (counts for factors, moment-sums for numerics)"** and specify the schema in Phase 2. This is near-certain (correctness + the headline perf win depend on it) — flagged for confirmation, not open debate.

**Status: OPEN** (near-certain) — now reflected inline in the Keystone (CLAUDE.md) and Phase 3 as "review gap G1"; the exact aggregate schema is to be fixed in Phase 2.

### G2. Per-cell significance stars conflict with CI-bounds-at-one-level

Phase 3 wants stars at 90/95/99% everywhere; Phase 1 stores only `ci_inf`/`ci_sup` at a **single** `conf_level` and explicitly bans `se`/`z`/`pvalue` fields. Verified: today's significance is exactly one `|diff| > ci` test at one level (`fmt_class.R:2237`). You **cannot** get three star thresholds from one interval — and for the **default** asymmetric Wilson/AC proportion diffs you cannot cleanly back a p-value out of the stored bounds at all (the SE→z shortcut only holds for symmetric means/Wald). So per-cell significance needs its **own** stored quantity (a p-value, or a test statistic/SE), decided **now** or Phase 3 forces the second field surgery the plan forbids. The doc's "stars/p stay sidecar" refers to the whole-table chi2 p; it does **not** answer where **per-cell diff/OR** significance lives.

**Resolved (Q2): store a per-cell `pvalue` field** — the honest single source for all star levels; the per-use-case tests are specified in §12; the field set is updated in §9.

### G3. The new `ratio` field overlaps the existing `rr` (and `or`) fields

§9 adds `ratio` as if net-new, but `rr` (relative risk) and `or` already exist (`fmt_class.R:1051-1056`). For pct columns the color-"ratio" is `cell_pct / ref_pct` — **that is a relative risk**. §3's own parenthetical ("would it be meaningful to store ratios in relative risks? is that the same calc as a step to odds ratios?") is exactly this unresolved question. Left unanswered, 1.4.0 ships three near-synonymous ratio fields (`ratio`, `rr`, `or`) with fuzzy boundaries. Decide whether color-ratio **reuses `rr`** (net fields 15→17, not 18) or is a **distinct `ratio`** (18) with `rr` reserved for logit/explicit RR.

**Resolved (Q3): rename the never-used `rr` → `ratio`** (placed after `diff`), used as the single ratio home — relative risk (pct), mean-ratio (numeric), and the RR step of OR calculation. A rename, not a new field, so the total stays 18 (the +1 there is `pvalue`). See §3, §9.

### G4. The means "T-test mirror of chi2" conflates two different tests

chi2 is a **whole-table omnibus** test (its p-value is the line under the factor table). Its true mirror for a mean table (numeric broken by a factor) is **ANOVA / Welch's F** ("are the group means different at all"), **not** a t-test. A **pairwise (Welch) t-test** answers a different question — one cell vs the reference — and is the correct engine for the **per-cell stars** (G2). The maintainer's own "est-ce le bon test statistique?" is answered: you need **both**, and they are different objects. Decide which land in 1.4.0 (recommend both: F-line + per-cell Welch-t).

**Resolved (Q4): both** — ANOVA / Welch F as the whole-table mirror (the table line under mean tables), and the Welch two-sample t per cell (cell vs reference) feeding the `pvalue` stars. See §12.

## Consistency issues vs the non-negotiable rule

### C1. Several planned changes are hard breaks against "soft-deprecate, never hard-break"

The governing rule (CLAUDE.md, decisions §Aim) forbids hard-breaking public API. Yet:

- **§6 removes the `totcol` argument entirely** — `totcol` is a **documented, exported** `tab_many()` arg accepting `"last"/"each"/"no"/"col"`/names/indexes (`tab.R:636,735-753`), and `tab()` translates `tot="col"`→`totcol` internally (`tab.R:378-381`). Removal is a hard break — and it is treated **inconsistently** with `totrow` (same status, only *deprecated*).
- **§3 flips numeric `$diff` from ratio to difference** — a **silent field-contract change** for any user doing `mutate(fmt, … diff …)` on a mean table; the field contract is explicitly "must not break".
- **Phase 7 turns exported `tab_plot()` internal** — removes it from NAMESPACE (`NAMESPACE:168`), a hard break.
- **Dropping the `ci` field** breaks `vctrs::field(x,"ci")` and `set`; `$ci` is recoverable (real overridable method `fmt_class.R:1871`) but internal `get_ci()` is raw `vctrs::field` (`fmt_class.R:1144`) and must be rewritten.

This forces a single decision: **stay 1.4.0 and soften all of these to soft-deprecation**, or **accept the breaks and release as 2.0.0**. The current doc is internally inconsistent (mixes "remove entirely" with a rule that forbids it).

**Resolved (Q1 — "Mixed"): stay 1.4.0.** `totcol` is soft-deprecated, not removed (§6 — now cosmetic-only, old values kept behind `deprecate_soft`); `tab_plot` stays exported-but-deprecated; dropping the `ci` field keeps `$ci`/`fmt(ci=)` working (§1, §9). **One exception accepted:** the numeric `$diff` ratio→difference flip lands as a documented change (numerics are rarely used and the ratio remains available in `$ratio`).

### C2. `tot_wn = wn/pct` recovery is fragile for empty cells

§11 drops `tot_wn` and recovers it as `wn/pct`. This fails on **empty cells** (`pct==0`). The fallback "read a sibling/total cell" is robust **within a non-empty row** (all siblings share the base) but fails for a **fully-empty row/group**, and the "total cell" fallback assumes the total row still exists — which the user may have dropped (`totrow` deprecated-on then `filter(!is_totrow)`). Low frequency, but decide explicitly: accept the sibling-fallback (document the fully-empty-row edge), or store `tot_wn` (→ one more field) for robustness.

**Status: OPEN** — default lean is the sibling-fallback recovery (keep 18 fields); revisit and store `tot_wn` only if fully-empty rows/groups prove to bite in practice.

## Under-specified — fill in (low risk)

- **U1. Output-shape truth table.** Define the exact return type for every combination of {1 vs ≥2 row_vars} × {tab_vars present?} × {`output_list` T/F}. Verified today: length-1 list unwraps to a bare `tab`/`grouped_tab` (`tab.R:1540`); with tab_vars you get a `grouped_tab` or a **list of grouped_tabs**. `output_list=FALSE` replaces `compact` as the single-table default. **→ Actioned:** tabulated as **§13**.
- **U2. Jamovi (Phase 8) must reuse the core, not reimplement it.** The current wording ("write new code with near the exact same behaviour as `tab_many()`, ensured by subfunctions") risks re-creating the **exact duplication 1.4.0 exists to remove**. Constraint to add: Jamovi calls the **same** aggregate-core + per-transform functions at cache-appropriate granularity; it never forks the math. **→ Actioned:** the reuse-the-core / never-fork constraint is folded into CLAUDE.md Phase 8.
- **U3. openxlsx → openxlsx2 scope.** Verified there is **no** openxlsx2 anywhere today (`DESCRIPTION:30`). A full API swap bundled into the same phase that *also* unifies exporter prep, adds list-methods, and integrates `tab_transpose()` is a large, parity-risking sub-project. **→ Actioned (approved):** the openxlsx2 migration is **split into its own Phase 9**; Phase 7 stays on openxlsx v1 (§8, CLAUDE.md Phase 9).
- **U4. col%-reference (`refcol`) side.** §4 makes the **row** `ref` a per-row_var named vector but only mentions the col% collapse in passing. The col% reference lives in the `refcol` attribute / `diff_index(…, pct="col")` (`tab.R:2644`, `5774`); spell out its interaction with the globalised axis, and fold in the known `fmt()` `refcol`-cast bug (casts `totcol` instead of `refcol`, `fmt_class.R:274`) so it is fixed in the same pass. **(Open — address in Phase 6/§4; the `refcol`-cast bug is already listed for the Phase 1 pass in §9.)**
- **U5. `tab_transpose()` is already exported** (`NAMESPACE`; `@export` at `tab.R:1773`) as an **undocumented, single-total-row/col-only stub** using unqualified verbs. It is therefore *already* a broken public function today — Phase 7 must finish + document it (or it should be un-exported in the interim). **→ Actioned:** flagged in the Phase 7 header (CLAUDE.md) and §7.
- **U6. `"ci"` color mode.** The roadmap's mode list (`diff, diff_ci, after_ci, contrib, OR`) omits the **`"ci"`** mode that the code implements (`fmt_class.R:2237`). **→ Actioned:** `"ci"` added to the Color System list and the Phase 5 scope (CLAUDE.md).
- **Doc drift:** the `/vctrs-field` skill body still says "13 fields" in one place (real count 15) — fix when the field pass lands.

## Coherence & synergy

**Positive, reinforcing:** `tot_n` + globalised row_var axis + "always exactly one total column" are genuinely synergetic — each cell self-computes its pct/diff/CI, which is what lets the single total column become a pure display anchor and retires `detect_totcols`. CI-as-bounds + significance-from-bounds + logit-OR-into-bounds share one representation. Exporter base+list methods + shared prep + `tab_transpose` cohere into one export story. The "different tables → `list()` → export sequentially" escape hatch (§5/§8) is consistent end to end.

**Tensions to manage:** (a) Phase 1 does field surgery on the **old** step-chain *before* the Phase 2/3 core rewrite — so the old `tab_pct`/`tab_ci` must be fitted with **throwaway glue** to populate `ci_inf`/`ci_sup`/`tot_n`/`ratio` just to keep golden green, then rewritten weeks later. Acceptable for small verifiable PRs, but consider splitting Phase 1 into **1a: field definitions + accessors + arithmetic/cast/format (contract)** and **1b: writers**, and folding 1b into Phases 2/3 where the core actually computes those fields. (b) The field set **cannot be frozen** until G1–G4 are answered — that is the real gate on starting Phase 1.

## Roadmap / phasing

Broadly the right path. Order is sound (safety-net → fields → core → CI/chi2 → counts → color → merge → exporters → Jamovi → Excel-engine). Adjustments **now applied**: G1–G4 resolved via Q1–Q4 so the field list is frozen (§9); **U3 openxlsx2 pulled out into a new Phase 9**; **U1 output-shape table added as §13**; the `"ci"` mode kept in Phase 5 (U6). Still **recommended**: split the Phase 1 field pass into **1a (contract: field defs + accessors + arithmetic/cast/format)** and **1b (writers)**, folding 1b into Phases 2/3 where the core computes those fields (avoids throwaway glue). Still **open**: **G1** aggregate schema (confirm in Phase 2) and **C2** (`tot_wn` recovery).

