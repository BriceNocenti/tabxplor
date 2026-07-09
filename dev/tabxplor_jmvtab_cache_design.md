# tabxplor — jmvtab hierarchical multi-cache design (Phase 7c)

> SINGLE REFERENCE for the live-UI cache of the jamovi module. It specifies a hierarchical,
> content-addressed cache so each button change in `jmvtab` redoes only what genuinely changed, giving
> near-instant results on normal survey data. It governs Phase 7d (compute-function rework) and Phase 7e
> (module rewrite).
>
> Companion docs (read the matching one first):
> - `dev/tabxplor_argument_computation_map.md` — the Phase 7b argument ↔ computation map. This design is
>   its child: it turns the map's §3/§7 invalidation classification into a concrete cache and
>   **supersedes** the map's §7 "cache classification (seeds Phase 7c)".
> - `dev/tabxplor_1.4.0_decisions.md` — settled 1.4.0 architecture (esp. §2 `tot_n`, §11 cache-stability,
>   §14/§20 inference).
> - `dev/tabxplor_1.4.0_jamovi_dev.md` — jamovi module dev insights (state, `clearWith`, render model).
>
> STATUS (2026-07-09, Phase 7c): design settled. No product code changes here — the refactor it
> prescribes is Phase 7d; the module rewrite that consumes it is Phase 7e.

---

## 1. Purpose and scope

Today `jmvtab.b.R`'s `.run()` recomputes the **entire** `tab()` pipeline on every interaction — even a
pure display toggle re-does the O(N) count scan and the chi². The target is a live application where the
common workflow (choose variables → percentages → colours) feels instant, while expert options stay
available.

This document answers one question: **when the user changes one option, what may the cache reuse and what
must it recompute?** It defines the cache tiers, their keys and values, what is persisted across runs, how
the jamovi framework hosts it, and the compute-function seams Phase 7d must carve to make the tiers
callable.

Two constraints from the jamovi runtime shape everything (grounded in `tabxplor_1.4.0_jamovi_dev.md` and
jmvcore introspection):

- **`.run()` always re-runs in full.** There is no "which option changed" accessor in R
  (`self$options$changed` errors). So the cache is **content-addressed**: each run recomputes tier keys
  from the resolved settings and compares them to a persisted store. The client `.js` mirrors the argument
  cascade only for UI *enabling* (Phase 7g); the R backend decides reuse by key comparison.
- **The only cross-run persistence is a result element's `$state`** — an arbitrary R object serialized as
  a gzip-RDS to a disk file and restored (before `.run()`) on the next run, surviving the engine reset.
  R6 `private$` fields and R globals do **not** persist. We therefore use `$state`, **not** hand-rolled
  temp `.rds` files (which would fight the framework), and keep the store small because it is serialized
  every run.

---

## 2. The three computation layers (recap)

From the map §1/§3. Invalidation flows **downward only**.

```text
AGGREGATE       counts n / wn (factors)  |  moment-sums s1=Sum(w*x), s2=Sum(w*x^2) (numerics)
   |            keyed on tab_vars x row_var x col_var-cell, NA kept, raw levels
PER-TRANSFORM   pct . tot_n . diff . ratio . or . CI (ci_inf/ci_sup) . pvalue . chi2/ANOVA test . contrib
   |            one vectorised pass each over the aggregate
DISPLAY         format string . digits . colour paint (measure->channel->palette) . labels . cleannames
```

A change at the aggregate layer invalidates everything below; a per-transform change invalidates that
transform + its dependents + display; a display change re-renders only. The cache is this hierarchy made
explicit and persistent across interactions.

---

## 3. The cache tiers

Five tiers. Overview first, then per-tier keys and values.

| Tier | Persisted in `$state`? | Recompute cost | Role |
|------|------------------------|----------------|------|
| 0. Prepared population | optional (cheap) | O(N) filter/omit | listwise row removal only |
| 1. Aggregate (per pair) | **yes** — bounded LRU | O(N) scan (the bottleneck) | counts / moment sums |
| 2. Test (chi2 / ANOVA) | **yes** — small | O(cells), N-independent | whole-table omnibus tests |
| 3. Transforms | no — recomputed each run | O(cells), sub-ms | pct/diff/ratio/or/CI/pvalue |
| 4. Display / render | no — recomputed each run | O(cells) + kable | fmt paint + HTML |

**Why persist only tiers 1-2.** `fmt` assembly is O(output cells), not O(N): a table is ~10^2–10^3 cells
regardless of whether the microdata is 20k or 15M rows, so building the `new_fmt` records + pct + diff +
CI on a table is sub-millisecond. The only genuinely O(N) cost is the count/moment scan (tier 1), and the
only N-independent-but-expensive cost is the omnibus test (tier 2). Persisting the small tiers keeps the
`$state` blob small (fast to serialize every run) while caching exactly the two costs worth caching.
Persisting fmt tables (large: 18 fields × every cell) and doing in-place `vctrs::field()<-` updates was
considered and rejected — YAGNI, and `field<-` copies the whole column anyway.

### 3.1 Tier 0 — Prepared population

- **Key:** `{data-id, filter-hash, wt-var, population, zero-weight-removed}` where
  `population = "full"` for `na in {keep, drop}`; `population = hash(all selected vars)` for `drop_all`;
  `population = hash(row_var, first col_var, tab_vars)` for `common_base`.
- **Value:** the prepped data frame after **ordered-factor strip + listwise row removal only**. Note the
  demotion: `cleannames` and `other_if_less_than` are moved OUT of `tab_prepare` (see §5, §8).
- **Persist?** Optional. The prepared frame is roughly microdata-sized, so it is usually cheaper to
  re-prepare from the (already in-memory) `self$data` than to serialize it every run. Recommended: do not
  persist tier 0; recompute it, and persist only the small tier-1 aggregates derived from it.

### 3.2 Tier 1 — Aggregate (per pair, the reuse unit)

- **Factor × factor key:** `{row_var, col_var, grain = sorted(tab_vars), wt, population, other_if_less_than}`.
- **Factor × numeric key:** `{row_var, num_col_var, grain, wt, population, other_if_less_than}`.
- **Value:** factor → long counts `{n, wn}` keyed by `tab_vars × row_var × col_var`, **NA kept, raw
  levels**; numeric → per-measure moment columns `{n, wn, s1, s2}` (plus `w2` only when Kish `n_eff` is
  opted in) keyed by `tab_vars × row_var`.
- **Rollup on hit:** store at the **finest tab-var grain currently in play** and accept any cached entry
  whose `grain` is a **superset** of the request, rolling it down with the additive rollup that already
  exists (`num_rollup` / the `.fine` keyby-sum). So **dropping a tab_var is a cheap rollup, not a
  recompute**; only *adding* a tab_var (finer grain) forces a new scan.
- **Eviction:** one entry per pair; whole store bounded by **serialized bytes** (target: whole store a
  few MB) with a **per-entry size ceiling** above which an entry is not persisted at all (recomputing one
  scan next run beats serializing a large blob forever). Evict least-recently-used. Revert-to-prior-config
  is a hit **if not yet evicted** — best-effort, not guaranteed.

### 3.3 Tier 2 — Test (chi2 / ANOVA)

- **Key:** `hash(shaped aggregate) + comp`, where the *shaped aggregate* is the tier-1 aggregate after the
  na-shape and lump collapses but **before the level-drop** (chi2/ci run on full levels, then non-first
  levels are dropped — the ordering invariant).
- **Value:** the tidy `test` tibble (chi2 + ANOVA rows; contrib residuals `ctr`/`var` when
  `color = "contrib"`).
- **Why a content hash, not an enumerated param list:** any shaping that changes counts/dimensions
  (`na`-shape, `other_if_less_than`) auto-invalidates the test, while `pct`/`ref`/`ci`/`levels`/`color`/
  `digits` all reuse it. chi2 genuinely depends on `comp` (`comp = "all"` pools the total table), so `comp`
  stays in the key. This is the tier that makes changing `levels` cheap: the level-drop is display, the
  test is reused.

### 3.4 Tier 3 — Transforms (recomputed each run)

Pure function of the shaped aggregate + the resolved settings. Writes the per-cell fields:

- `pct` + `tot_n` — from `pct` (each cell's own unweighted base in `tot_n`; weighted base recovered as
  `wn/pct`).
- `diff` / `ratio` / `or` — from `ref` / `ref2` / `OR`.
- `ci_inf` / `ci_sup` / `pvalue` — from `ci` / `conf_level` / `method_cell` / `method_diff` / `stars`.

Not persisted: cheap to recompute, and this is where the interactive `ref`-change lands (see §4).

### 3.5 Tier 4 — Display / render (recomputed each run)

Pure function of tier 3. `display` string + `digits`; colour paint (`color` measure/channel,
`color_signif` policy — the findInterval engine reads fields, never recomputes them); labels
(`subtext`/`total_names`/`totaltab_name`); `add_n`/`add_pct`; total row/col *removal*; the **cleannames
relabel** (§5); `tab_kable` → HTML → `setContent`.

---

## 4. The five interaction goals as tier walks

Each maps to which tiers are reused vs recomputed. This is the acceptance spec for the cache.

- **(a) Add a row/col variable.** Only the new variable's pairs miss tier 1; every other pair is a tier-1
  hit. New pairs scan once; the rest reuse counts, tests, and recompute only the cheap tiers 3-4. Reverting
  to a prior variable set is a tier-1 hit if not yet evicted. *(Holds for `na in {keep, drop}`; see §5 for
  the `drop_all`/`common_base` limitation.)*
- **(b) Change the percentage type (`pct`).** Tier 1 (counts) and tier 2 (test) are untouched — `pct`
  never touches the aggregate. Recompute from tier 3 (`pct`/`tot_n` then `diff`/`ratio`/`or`/CI) + tier 4.
- **(c) Change the reference level (`ref` / `ref2`) — the instant path.** Tiers 1, 2, and the `pct`/`tot_n`
  part of tier 3 are all reused; only `diff`/`ratio`/`or` (plus the diff-CI when `color_signif` is not
  `"ignore"`) recompute, then tier 4 repaints. Instant because fmt assembly is O(cells). This is the
  headline responsiveness goal.
- **(d) Change display (`display` / `digits` / `color` / `color_signif` / labels).** If the measure/CI
  field the colour needs already exists, only tier 4 re-renders. **Caveat:** switching `color_signif` from
  `"ignore"` to a significance policy needs the CI (a tier-3 recompute), even though the paint itself is
  tier 4 — the cache must therefore know **which fields exist**, not just which colour was last shown.
- **(e) Reorder factor levels (Phase 7f feature).** A post-aggregate `fct_relevel` + arrange on the shaped
  aggregate — tiers 1-2 reused, tiers 3-4 recompute (fast). Note it shifts `ref = "first"` and the
  `common_base` first-col reference, so treat a reorder as a tier-3 input, not pure display.

---

## 5. Missing values and the count-dependent arguments

The organizing principle: **aggregation commutes with level *merges* (partition coarsening), never with
row *deletions*.** Every count-dependent argument is classified by that line.

**Post-aggregate (cheap, cache-reusing) — merges / relabels / NA-cell drops:**

- `na in {keep, drop}` — build the aggregate once with **NA kept**; for `drop`, delete the NA cells
  post-aggregate (already exactly how `tab_plain` works). A tier-3 shape collapse.
- `levels` (merge / "first") — a post-aggregate re-group; the level *drop* is display (tier 4), run after
  the test (tier 2) so the test sees full levels.
- `cleannames` — **display-tier**. jmvtab carries **full factor names through its whole pipeline**, and
  only if `cleannames = TRUE` strips the patterns as a cheap last step before `tab_kable()`. So cleannames
  is **not in any cache key** (cosmetic, like `digits`).

**Aggregate key (rebuilds the O(N) scan) — row deletions / count re-sums:**

- `wt` — weighted counts / moment sums.
- `filter` — pre-aggregate row deletion.
- `other_if_less_than` — lumps factor levels whose **unweighted** count is below the threshold; it
  genuinely re-sums counts, is rarely toggled live, and post-aggregate demotion is byte-identity-risky, so
  it stays in the aggregate key.
- `na in {drop_all, common_base}` — **population** modes: the kept population is a function of *several*
  variables at once. `drop_all` = listwise on `{row_var, ALL col_vars, tab_vars}`; `common_base` = listwise
  on `{row_var, FIRST col_var, tab_vars}` (secondary col_vars then keep their own NAs, like `keep`).

**Documented limitation (state it in the UI expectations).** The two population modes break per-pair reuse
by *different* amounts, so the tier-1 `population` component encodes each precisely:

- `drop_all` — population depends on the **whole** variable selection, so adding **any** col_var recomputes
  **every** pair. Goal (a) cannot hold; do not promise "instant add-variable".
- `common_base` — population depends only on `{row_var, FIRST col_var, tab_vars}`. Adding or removing a
  **secondary** col_var leaves the population unchanged, so those pairs are **still per-pair reusable**;
  only changing the row_var, the *first* col_var, or tab_vars invalidates. More cache-friendly than
  `drop_all`, less than `keep`/`drop`.

jmvtab defaults to `na = "keep"`, already the cache-friendly per-pair path; `keep` and `drop` are the fast
lane. Set the `population` key to `hash(all selected vars)` for `drop_all` and `hash(row_var, first col_var,
tab_vars)` for `common_base` so reuse is exactly as wide as each mode allows.

**cleannames cosmetic caveat.** Because cleannames is applied at display over full-name rows, if two raw
levels clean to the same string, jmvtab shows them as **separate same-labelled rows** (no
collision-summing). Accepted: jmvtab needs no back-compatibility. `tab()` keeps its pre-aggregate
cleannames (with summing) for retro-compatibility — a deliberate divergence between the two entry points.

---

## 6. Column-type asymmetry

- **Factor × factor** → counts `{n, wn}`; cache unit = one entry per (row_var × col_var).
- **Factor × numeric** → moment sums `{n, wn, s1, s2}` per measure; mean, var, CI and ANOVA are all
  recovered from `(n, s1, s2)` with **no re-scan** (`num_derive_stats`), and totals are additive rollups
  (`num_rollup`). Cache unit = one entry per (row_var × numeric measure); the moment columns are
  independent per measure, so **adding a measure is incremental** (append its columns; reuse the others).
- Measures `contrib` and `or` are factor-only (a mean has no chi2 decomposition / odds ratio); numeric
  `diff` colours Glass's Δ. `color = "auto"` resolves per type.
- The col% + means reference asymmetry (a mean is referenced by a row, a factor under `pct = "col"` by a
  column) is intended and documented in the map §8 — it is a display/transform property, orthogonal to the
  cache.

---

## 7. jamovi hosting mechanics

- **One host result element carries the whole store.** In `.run()`: read
  `self$results$<host>$state` (or initialise an empty store) → recompute tier keys from the resolved
  settings → serve hits / compute misses / write back → `self$results$<host>$setState(store)`. A dedicated
  hidden element (or the main table element's state) holds it.
- **Persist plain atomic-vector lists (or tibbles), not live `data.table`s.** `readRDS` invalidates a
  data.table's `.internal.selfref` (it deep-copies / warns on the first `:=`/`setkey`), and jamovi's
  bundled R may run a different data.table version. Store atomic vectors; rebuild the DT per run with
  `setDT()`.
- **Bound the store by serialized bytes**, with a per-entry ceiling (skip persisting oversized entries).
  The store is gzip-RDS'd every run, so serialize cost scales with its size — keep it to a few MB.
- **Do not rely on `clearWith` for the tiered logic.** `clearWith` is coarse (whole-element, whole-option)
  and cannot express per-pair reuse or revert; the manual content-addressed store does that. A minimal
  `clearWith` may remain only as a gross safety fallback (e.g. clear on data change).
- **Keep emitted HTML byte-stable** across reused runs — jamovi re-renders the whole result tree on every
  update, so unstable HTML causes visible churn.
- **Optional:** `private$.checkpoint(flush = TRUE)` can flush partial results mid-`.run()` to keep the UI
  responsive during a heavy first scan.

---

## 8. Compute-function seams Phase 7d must build

> STATUS (2026-07-10, Phase 7d-ii): **BUILT.** `tab_build()` is now the five-stage pipeline
> `ctx |> tab_setup |> tab_prepare_pop |> tab_aggregate |> tab_transform |> tab_assemble` (R/tab.R),
> each stage individually callable and threading a `ctx` list — the orchestration carve (the leaves
> `tab_plain()`/`tab_num()` are NOT split: the existing `.fine=` seam already gives the O(N)→O(cells)
> split, and tier 3 recomputes wholesale, per §3.4). `cleannames`/`other_if_less_than` extracted to
> `tab_lump_others()`/`tab_cleannames_relabel()` (tab_prepare() still composes them; §5 divergence
> is a 7e display step). `tab_resolve_settings()$cache_keys` emits the tier 0-2 symbolic key material.
> `tab_counts()` re-expressed on the same stages (single-pair ctx). Byte-identical (golden/fuse/counts
> parity green, no regen); new `test-carve-parity.R` locks the composition + the seam contract, and
> the mapping stage↔tier is in the map §10. 7e (the module) adds the store + the data hashes.

The tiers are only callable if `tab_build` is carved into three composable steps — the **same functions
`tab()` uses, no math fork** (Phase 7e drives them at cache granularity; reuse guarantees near-identical
behaviour). Grounded in the current pipeline order (map §2).

- **`tab_aggregate()`** — prepped data → tier-1 aggregate (factor counts / numeric moment sums, NA kept,
  raw levels). The cut inside today's `tab_plain` lands at "dcast'd counts + total *counts*"; the total
  *percentages/fmt* stay in transform, so the cache holds small counts, not large fmt records.
- **`tab_transform()`** — shaped aggregate → `pct`/`tot_n`/`diff`/`ratio`/`or` + CI + fmt assembly + the
  tier-2 test. It must span **both** the per-col `tab_plain` math **and** the post-join `tab_apply_tests`
  (the omnibus tests need the joined multi-col table). Preserve the **test-before-level-drop** invariant.
- **`tab_assemble()`** — join + `add_n`/`add_pct` + total row/col removal + level-drop + output shape +
  colour paint + cleannames relabel + render prep.
- **Split `tab_num()`** into a moment-sum producer (tier 1) + a mean/var/CI/colour transform (tier 3). The
  pure helpers already exist in `tab-agg.R` (`num_derive_stats`, `num_rollup`, `ci_mean_diff2`); this is
  re-wiring, not new math. Numeric currently has **no aggregate entry point** (factor has
  `tab_plain(.fine=)`) — 7d adds the parallel numeric plumbing and carries `tab_num`'s finer na modes.
- **Move `cleannames` + `other_if_less_than` OUT of `tab_prepare`** (jmvtab path): `tab_prepare` keeps only
  the ordered-strip + listwise removal (population). cleannames → display; `other_if_less_than` → its own
  pre-aggregate step feeding the tier-1 key. `tab()` retains its current order (retro-compatibility).
- **Extend `tab_resolve_settings()`** (`R/tab-resolve.R`, the pure settings boundary) to also **emit the
  per-tier cache keys**, so one place computes both the argument cascade and the cache-key material the
  `.js` mirrors.

These seams also serve the batch `tab()` workflow (many exploratory tables at once): the same aggregate is
reused across row_var/col_var pairs within a single call — the `.fine` fusion infrastructure, today
opt-in behind `options(tabxplor.fuse_min_rows=)`.

---

## 9. Caveats and open items (for 7d / 7e / 7f)

- **cleannames collision cosmetics** — separate same-labelled rows in jmvtab; documented divergence from
  `tab()` (§5). Revisit only if it confuses users.
- **`drop_all` / `common_base`** — no per-pair reuse (§5). Consider surfacing a subtle UI hint that these
  modes are slower on variable-set changes; do not change the default (`keep`).
- **Numeric aggregate plumbing gap** — the numeric moment-sum path has no `.fine`-style entry point today;
  7d must add it (§8). Until then the numeric tier-1 cache cannot be populated.
- **Byte-identity parity tests are 7d's obligation.** Every demotion must be proven equivalent against the
  golden `.rds` + `_snaps/` before it ships: post-aggregate `na`-shape and `levels` collapse; the numeric
  moment plumbing; and, for the jmvtab path specifically, the cleannames-at-display divergence (a *new*
  parity fixture, since it is intentionally not byte-identical to `tab()`'s cleannames).
- **`tab_num(..., <tab_vars>, ci = "cell")`** — was fixed in Phase 6e (golden-locked); Phase 7d-i
  preserved the fix across the `.fine` split + added `expect_no_error` regression coverage.
- **Store schema versioning** — tag the persisted store with a small integer version; on a version
  mismatch (module upgraded between sessions) discard and recompute rather than deserialize a stale shape.
