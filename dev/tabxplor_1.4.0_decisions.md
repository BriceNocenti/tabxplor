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
  internal `set_ci`/`get_ci` call sites migrate to the bounds; significance/`± moe` read the bounds.
- **Display**: the `[inf;sup]` bracket (default) reads `ci_inf`/`ci_sup` directly → now exact (keep the
  `[0,100]` render clamp). The compact `± moe` mode shows means exactly and, for asymmetric cells, the
  **conservative larger arm** `± max(est−inf, sup−est)` (keeps the one-number format, never understates).
- **`tab_logit`**: OR CIs go into `ci_inf`/`ci_sup` (filled from `broom::tidy(conf.int=)`); the
  `OR_inf`/`OR_sup` **sidecar columns retire** ([tab_logit.R:463-464,527](../R/tab_logit.R#L463)).
- **Significance / color formulas** ([fmt_class.R:2191-2295](../R/fmt_class.R#L2191)) migrate from
  `abs(diff) > ci` to a bounds test (the diff's CI excludes 0: `ci_inf > 0 | ci_sup < 0`), which is also
  correct for asymmetric intervals. Land this in Phase 3 with the CI/chi2 aggregate migration.

Sources (asymmetry): Wikipedia *Binomial proportion confidence interval* (Wilson/Agresti-Coull);
Wikipedia *Odds ratio* + BU SPH PH717 Module 8 (log-scale CI, `exp` asymmetry). See the CI section of
the architecture guide once implemented.

---

## 2. Reference-base field — store `tot_n` (the cell's OWN base); recover `tot_wn`

Grounded split: **percentages use the weighted base** `get_wn(tot)`
([tab.R:5602-5608](../R/tab.R#L5602)); **CI/chi2/T use the unweighted base** `get_n`
([tab.R:4845-4847](../R/tab.R#L4845), chi2 p-value unweighted [tab.R:5258](../R/tab.R#L5258)). Today
both are recomputed on demand by looking up a total row/col cell; across col_vars with different NA
totals this falls back through `detect_totcols` to the shared "last col_var total column"
([fmt_class.R:1271-1285](../R/fmt_class.R#L1271)) — the approximation the roadmap targets.

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

**Decision — store `tot_n` only** (materialises the existing local `tot_n` transmute at
[tab.R:4878-4900](../R/tab.R#L4878)); `tot_wn` is recovered (§11). Bases are row/col totals per `pct`
("row"→row total, "col"→col total, "all"→grand total). This retires `detect_totcols` **on built tables**
(each proportion cell carries its own exact base). Whether that is load-bearing enough to keep the field
is analysed in §11.

---

## 3. Color diff vs ratio — add a `ratio` field

Today numeric ratios ride the `diff` field (Mean-diff asymmetry constraint) and pct-column ratios ride
the `mean`-overload. **Decision**: add a dedicated `ratio` field.
- Numeric: `color="diff"` → difference in `diff`; `color="ratio"` → ratio in `ratio`
  (old behaviour). `color="diff_ratio"` → both (text vs background).
- Pct columns carry `diff` + `ratio` simultaneously; the `mean`-overload is removed.
- Retro-compat: pct `$diff` unchanged (safe). Numeric `$diff` flips ratio→difference — accepted
  (numerics are rarely used; pct columns are the real retro-compat surface). `/color-mode`, `/vctrs-field`.

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
- Keep `totcol` (the total **column**) — col% coloring structurally depends on it surviving
  ([tab.R:2132-2135](../R/tab.R#L2132)). Deprecate the row only.
- Keep `is_totrow()` exported. Exporters that style the total row simply find none if removed. Results
  are already in the fields at calc time. Rework internal `tot="no"` shortcuts (e.g. the `ci="cell"`
  bug, [tab.R:3454](../R/tab.R#L3454)) to always include the total row (Phase 3).

**Remove the `totcol` argument entirely** (aligned with always-total-row): there is **always exactly
one** total column, positioned like the current default — after the last factor column, before numeric
(mean) columns. `tot_n`/`tot_wn` per cell carry the exact bases, so this single displayed total is a
**display anchor**, not the calculation base (with multiple col_vars of different N from NA, display ≠
per-cell base, by design). What goes away: per-col_var total columns (`totcol=<col_var>`) and the
no-total option (`totcol="no"`) — the no-total escape hatch is now `tab_plain()` directly. Move/drop the
column via dplyr. All-in-one tables (the dominant use) already show one total → unaffected.

Also drop the `tabxplor.compact` option (superseded by `output_list`, default `FALSE`).
Deprecate singular `row_var`/`col_var` args (keep working, soft) → only `row_vars`/`col_vars` remain.
The **col_var axis stays flexible**: `pct` / `levels` / `digits` remain settable per col_var (sup_cols
preserved); only the row_var axis is globalised (§5) and `totcol` is removed (above).

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

**`tab_transpose()`** (a stub the maintainer added to `R/tab.R`) is to be **integrated, exported,
documented, and possibly generalised** (tab_vars?) at Phase 7 — it is the mechanism for the above. Do
not wire it in before Phase 7.

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

---

## 9. Resulting `tabxplor_fmt` field list after the Phase 1 pass

Current 15 per-cell fields, one combined vctrs surgery → **18 fields**:

- **Add** (+4): `ratio` (§3), **`tot_n`** (§2 — the cell's own % base, renamed from the roadmap's
  `ref_n`), `ci_inf` + `ci_sup` (§1). `tot_wn` is NOT stored — recovered as `wn/pct` via `get_tot_wn()`
  (§11).
- **Drop** (−1): `ci` — recomputed on access from the bounds; public `fmt(ci = )` arg kept (§1).
- **Adjust semantics**: `diff` (numeric now = difference not ratio, §3), `mean` (pct overload removed, §3).
- Column attributes: the `ref` attribute gains per-row_var vector semantics (§4); the `totcol` attribute
  usage simplifies (always one total column, §6); no *new* attribute.

Downstream touch-list per `/vctrs-field` (do all in one pass): `new_fmt()`, `fmt()`,
`format.tabxplor_fmt()`, `pillar_shaft`, `vec_arith`/`vec_cast`/`vec_ptype2`, the `tab_pct`/`tab_ci`/
`tab_chi2` writers, and the exporters (`tab_xl` display bypass). Regenerate golden once.

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
ref_pct`), `OR`/`rr` point, `ci="diff"`, the mean T-test each read the **reference cell's** `tot_n`/`pct`
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

## Sources (statistics)

- Binomial proportion CI (Wald / Wilson / Agresti-Coull asymmetry): <https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval>
- Odds-ratio CI symmetric on log scale, asymmetric after exp: <https://en.wikipedia.org/wiki/Odds_ratio> ; <https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module8-CategoricalData/PH717-Module8-CategoricalData5.html>
- Newcombe/Agresti-Coull difference-of-proportions coverage asymmetry: Newcombe (1998), summarised in the Wikipedia article above.
