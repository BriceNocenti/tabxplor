#### Phase 9b — `tabxplor_fmt` as display-only, materialized at the end

Feasibility analysis + implementation record for the 1.4.0 roadmap's Phase 9b. Read
`dev/tabxplor_1.4.0_decisions.md` §27 (the fmt-build cost finding) and §29 (the fresh profile
+ "Finding 4 is the lever") first — this doc is the detailed follow-through on that lever.

##### 1. Grounding — where the time actually is

The §29 profile (`tab(gss_cat, 5 row_vars × 3 col_vars, pct="row", color="diff", chi2=TRUE)`,
21 483 rows) splits a `tab()` call as:

| stage                                                       | s/call | share | nature                |
|-------------------------------------------------------------|--------|-------|-----------------------|
| `tab_setup` (all arg resolution + row/col-axis recycling)   | 0.005  | 0.2 % | control flow          |
| `tab_prepare_pop`                                           | 0.008  | 0.4 % | data prep             |
| `tab_aggregate` (the real O(N) data work)                   | 0.001  | ~0 %  | data.table GForce     |
| `tab_transform` (leaf scan + fmt build + ci + chi2)         | ~0.7   | ~33 % | O(cells), vctrs-bound |
| `tab_assemble_tables`                                       | 0.05   | 2 %   | O(cells)              |
| `tab_assemble_output` merge (`tab_compact`) + p-value lines | ~0.72  | ~34 % | O(cells), vctrs-bound |

`Rprof` reading: **`vec_case_when` = 40 % of total (72 % of `tab_compact`)**; the rest is
`tabxplor_fmt` record reconstruction (`structure`/`new_data_frame`/`list_unchop`/
`vec_restore_dispatch`/`df_list`/`vctrs::field`). Argument resolution + the whole row/col-axis
vectorisation is **0.2 %**; the O(cells) `tabxplor_fmt` machinery is **~99 % of the build**, bound
by `dplyr::case_when`/`if_else`-over-fmt + record round-trips, not by control flow.

Two facts from the code exploration reframe the problem:

+ **The build math is already plain.** `R/tab-agg.R` (moment scan, `num_derive_stats`, the
  `ci_*` engine, `agg_chi2`/`agg_anova`) and `tab_apply_reference()` ([tab.R:3144](R/tab.R#L3144))
  operate entirely on plain `data.table`s / atomic vectors. `tab_num()` even computes CI + p-value
  on plain vectors *before* `new_fmt`.
+ **The record is materialized too early, then mutated in place.** `new_fmt` fires inside each
  leaf ([tab.R:3027](R/tab.R#L3027) `tab_plain`, [tab.R:4106](R/tab.R#L4106) `tab_num`); the
  resulting fmt columns are then rewritten by `tab_ci` (`set_ci_inf`/`set_ci_sup`/`set_pvalue`),
  `tab_chi2` (`set_var`/`set_ctr`/`set_color`), and the `tab_compact` merge (the
  `if_else(is_totrow & !any(is_refrow), as_refrow, .)` over fmt). **Every such touch detonates a
  full vctrs record ptype2/cast round-trip.**

So the premise — *carry the build as plain field-vectors, wrap `new_fmt` once at the end* — is
correct. The named `new_fmt` constructor is **not** the problem: the internal `new_fmt`
([fmt_class.R:1085](R/fmt_class.R#L1085)) is already lean (recycle `display`, then `new_rcrd`) and
does **no** casting. The win is *fewer* records + *no* round-trips between build and display, not a
faster constructor.

###### Two separable wins

1. **The merge (`tab_compact`)** — a single `if_else`-over-fmt hotspot, killable with a direct
   field write, no representation change. Safe, byte-identical. **This is Phase 9b-1 (done).**
2. **The leaf build + the ci/chi2/assemble round-trips** — recoverable only by deferring the
   single `new_fmt` materialization to end-of-build (the plain-carrier rewrite). Large, risky.
   **This is the gated Phase 9b-2/9b-3.**

Realistic ceiling of the whole phase: *roughly halving the build*. The aggregate is already ~0 %
and stays; `format()` for display (console/kable/md) is a **separate Phase 10b** concern and is
out of scope here.


##### 3. Phase 9b-2/9b-3 — the plain-carrier rewrite (designed, gated)

Not to be implemented until this design is approved. Carrier shape chosen (this session):
**unwrapped-fmt-columns** — minimal conceptual distance from today's record, lowest translation
risk, preserves the fast GForce wide math.

###### 3.1 The carrier

Between the leaf math and the single materialization, an in-build table is:

+ **field frames** — one small `data.table` per output column, holding that column's 18 raw fields
  (`n int`, `digits int`, `in_totrow/in_tottab/in_refrow logical`, `display char`, the 11 doubles)
  — i.e. exactly today's `vctrs::vec_data(fmt)`, un-classed;
+ **col-meta sidecar** — a tibble keyed by output column, one row per column carrying the 9 scalar
  attributes (`type`, `comp_all`, `ref`, `ci_type`, `col_var`, `totcol`, `refcol`, `color`,
  `color_signif`);
+ **row-meta sidecar** — the `levels` / `row_var` / `tab_vars` columns.

`new_fmt` is called **once per final output column**, after `tab_compact`, **before**
`tab_pvalue_lines`/`tab_apply_n_min` (which stay record-based — they are cheap, ~0). The wide
GForce math in the leaves is untouched; only the *carrier between leaf and display* changes.

###### 3.2 What changes

+ **Leaves** (`tab_plain`/`tab_num`): return the carrier (field frames + col-meta) instead of a
  tibble of fmt records. The math up to the current `new_fmt` is unchanged.
+ **`tab_ci`/`tab_chi2`**: become **plain-field writers**. The math already runs through the plain
  `R/tab-agg.R` engine; they write plain `ci_inf`/`ci_sup`/`pvalue`/`ctr`/`var` columns into the
  field frames instead of `set_*` on records. (`tab_num` already does exactly this for its numeric
  CI — the template.)
+ **Assemble/level-drop/add_n/total/merge**: operate on the field frames + col-meta (data.table
  ops), not dplyr-over-fmt.
+ **Materialize**: `imap(field_frames, ~ new_fmt(!!!.x, !!!col_meta[[.y]]))` with a final
  type-cast pass (see L1).

###### 3.3 Byte-identity landmine ledger (mitigations the rewrite MUST honor)

Ranked most-likely-to-break first (from the adversarial review):

+ **L1 — type coercion at materialization.** `new_fmt` does not cast. Re-cast every field to its
  contract type at the single materialization point (`n`/`digits`→`integer`, `in_*`→`logical`, the
  11 doubles, correct `NA_integer_` vs `NA_real_` flavor). Never let a data.table `:=`/join/
  `set_num(NA_real_)` on an `n`-cell silently promote `n` to double — the golden RDS + fmt-contract
  compare *storage type*, so this breaks with no visible text change. Universal — breaks first.
+ **L2 — join row/column reorder.** `data.table` `merge(all=TRUE)` sorts by key; the numeric×factor
  `full_join` ([tab.R:1804](R/tab.R#L1804)) and the factor `reduce(full_join)`
  ([tab.R:1709](R/tab.R#L1709)) preserve **factor-level** row order plus a specific `col_vars`
  column permutation ([tab.R:1806-1814](R/tab.R#L1806)). Replicate with `sort = FALSE` + an
  explicit reorder to the `row_var` factor levels and the `col_vars` order (text→0L, unknown→last,
  totals last).
+ **L3 — cross-subtable attribute reconciliation is NOT "take the first".** `vec_ptype2.tabxplor_fmt`
  ([fmt_class.R:2844-2852](R/fmt_class.R#L2844)) collapses any *differing* attribute to a neutral
  (`type→"mixed"`, `col_var→"several_vars"`, `ref`/`ci_type`/`color→""`, `totcol`/`refcol`/
  `comp_all→FALSE`, `color_signif→"ignore"`). Post-Phase-6 globalisation makes this a no-op on the
  common path, but it is reachable + golden-locked via per-row_var `ref` (§5 named vector) and
  per-col_var `refcol` (Phase 7g-iii). The carrier merge must run the **same per-attribute
  `if_else(same, x, neutral)`** across all stacked sub-tables, not copy sub-table 1's meta.
+ **L4 — grouped `as_refrow`.** `!any(is_refrow)` must be evaluated per `row_var`, never globally
  (already handled correctly in 9b-1; the carrier merge must keep it grouped).
+ **L5 — the materialization boundary.** `tab_pvalue_lines` (constructs fmt cells, adds rows),
  `tab_apply_n_min` (writes the `display`="blank" mask, reads `get_tot_n`/`get_n`), and
  `finalize_color_spec` run *after* the merge and touch fields. Materialize *after* `tab_compact`
  but *before* those. Reproduce the per-column derived display at materialization:
  `digits`-from-mean-magnitude ([tab.R:4108-4113](R/tab.R#L4108)) and the `display`/`type`/`ref`/
  `comp` mapping ([tab.R:2988-3009](R/tab.R#L2988)). Pass **absolute** `ci_inf`/`ci_sup`, never the
  `ci` half-width (avoids the `ci_center` shim drift).
+ **L6/L7 — field surgery in `tab_add_n_pct` and the `tab_ci` factor path.**
  `tab_add_n_pct` ([tab.R:6231](R/tab.R#L6231)) and the factor `tab_ci` ([tab.R:4977](R/tab.R#L4977))
  do broad, reference-lookup-driven, NA-typed field writes (`detect_totcols`/`detect_refcol` by
  name; reference row = `dplyr::last(which(is_totrow))` — **last**, not first; `x_n = NA` on the
  reference cell so it is never self-compared). The plain rewrite must reproduce the exact base
  column, the *last*-totrow reference selection, the NA flavors, and the
  `ci_prop_diff`/`ci_wilson`/`ci_wald`/`ci_mean_diff2` argument wiring (weighted estimate +
  unweighted base, §14).

Lower risk / already safe: numeric CI/pvalue is already plain-vector (template); the intra-column
`as_refrow` promotion combines two identical-attribute records (its own ptype2 is a no-op — the
only danger was grouping/staging, L4); `detect_totcols`/`detect_refcol` are pure attribute+name
reads, trivially portable to the col-meta sidecar.

###### 3.4 Phasing sketch (each byte-identical, golden-gated, benchmarked)

+ **9b-2** — **RE-SCOPED to a measurement (done 2026-07-11; see §5).** The originally-planned
  "plain-field writers, leaf still materializes" is a no-op on the common `color="diff"` path
  (`tab_ci` never runs; `tab_chi2` writes nothing) — so it cannot gate 9b-3. Replaced by a
  throwaway decomposition (`dev/benchmarks/phase9b2_fmt_cost_decomp.R`) that measured the
  recoverable fraction directly. **Verdict: GO for 9b-3; fold the writers into it.**
+ **9b-3** — move the single `new_fmt` materialization to *after* `tab_compact`; assemble + merge
  operate on the carrier; the L1-L7 mitigations land here. This is where the leaf-build + merge
  round-trips are actually recovered.
+ **9b-4 (optional)** — jmvtab tier-3 stores the carrier (plain field frames) instead of the
  materialized armed fmt table, and re-paints on the raw fields (see §4).

###### 3.5 Go/no-go verdict

**GO — confirmed by measurement (§5, 2026-07-11), strictly staged and gated.**

Original estimate (pre-measurement): after 9b-1, the merged call is ~1.55 s, of which the merge is
~0.31 s and the `output_list` (no-merge) build is ~1.24 s for 5 tables (~0.25 s/table). Eliminating
the early materialization + record round-trips *plausibly* takes 30-50 % off the per-table build —
a range, not a promise. **The §5 spike confirmed the recoverable share is ~30-48 % on the common
path** (`vec_restore` reconstruction alone is 29.7 % of the build; the build-once floor is a
negligible ~0.5 %; record ops are 54.5× slower than plain). Above the 25 % bar → proceed to 9b-3.

Risk: 9b-3 is the single largest byte-identity surface in 1.4.0 (7 named landmines, touches
`tab_plain`/`tab_num`/`tab_ci`/`tab_chi2`/`tab_assemble_tables`/`tab_add_n_pct`/`tab_compact` +
the jmvtab cache). It is worth it *only* as a golden-gated, multi-session, one-sub-phase-at-a-time
effort. If the 9b-2 spike shows <25 % recoverable, stop and keep only 9b-1.

##### 4. Interactions

+ **jmvtab tier-3 cache** (`R/jmvtab-cache.R`): today it stores the **materialized armed fmt table**
  and re-paints its fields on every hit (`finalize_color_spec`/`jmv_reapply_digits`/
  `jmv_apply_display`, all `set_*` on records). After 9b-3 the cache could store the **carrier**
  (plain field frames — smaller, no record overhead) and materialize + paint once on read; the
  re-paint would run on plain columns, faster. It does **not** need the "which fields exist"
  tracking the design sidestepped (§7c) — the carrier still carries the canonical superset of
  fields (diff builds diff+ratio+CI), so `color_signif` stays a pure re-paint. This is 9b-4,
  optional and independent.
+ **Exporters**: `tab_xl()` is already field-native (reads `get_num`/`get_display`/`get_digits`/
  `get_type`/`get_color`, bypasses `format`) — it needs no materialized `format()` output and could
  even read the carrier directly. `tab_md()`/`tab_kable()`/console go through
  `format.tabxplor_fmt` and therefore need the materialized fmt — which the final table still has,
  unchanged.
+ **Phase 10b** owns the `format.tabxplor_fmt`/`get_reference` `case_when`→base rewrite (a *display*
  speedup: console/kable/md/jmvtab render). Orthogonal to 9b — kept there.
+ **Phase 11** (openxlsx2) is unaffected: `tab_xl`'s field-native read is representation-stable.


##### 5. Phase 9b-2 — the measurement (2026-07-11): GO for 9b-3, fold in the writers

Harness `dev/benchmarks/phase9b2_fmt_cost_decomp.R` (throwaway, **no `R/*.R` change**); full record
`dev/benchmarks/results_1.4.0/phase9b2_decomposition.txt`. `forcats::gss_cat`, 5 row_vars × the four
representative shapes; `output_list=TRUE` isolates the no-merge per-row_var build.

Why re-scoped: the design's 9b-2 ("plain-field writers, leaf still materializes") recovers ~0 on the
common `color="diff"` path — **`tab_ci` never runs** (`ci` defaults `"no"`; `color="diff"`, unlike
`"diff_ci"`/`"after_ci"`, does not force a CI, `R/tab-resolve.R:96`) and **`tab_chi2` writes no fmt
records** on `color_ctr="no"` (only the `test` tibble). So it cannot gate 9b-3. A decomposition
measures the real lever (deferred materialization) directly.

Findings (per-table = `output_list` build / 5):

| shape | per_table | fmt-record machinery (by.total) | irreducible scan | 9b-3 gain |
|---|---|---|---|---|
| common (diff) | 0.288 s | `vec_restore` 29.7 % + `vec_case_when` 18.1 % + `new_rcrd` 12.9 % | `[.data.table` 14.2 % | large |
| ci (diff) | 0.369 s | +28 % vs common (the `tab_ci` writes) | — | large |
| contrib | 0.473 s | `vec_case_when` 32 %, `new_rcrd` 19 % (+64 % vs common) | — | largest |
| numeric | 0.029 s | ~1 % | `[.data.table` 35 % | ~none |

Decisive micro cross-check (one 8×21 table): **materialize-once floor = 1.4 ms** (0.5 % of the
288 ms build — building records once is ~free); pushing them through 6 reconstruct rounds is
**54.5× slower** than plain field-frames + one final materialize. So the fmt cost is almost entirely
*redundant reconstruction* (leaf materializes, then every join/slice/rbind rebuilds the record) —
exactly what deferral removes. Corroborates the `vec_restore` 29.7 %.

Verdict:
+ **GO for 9b-3.** Recoverable ~30 % (`vec_restore` alone) to ~48 % (+`vec_case_when`) on the common
  path, above the 25 % bar; larger for CI/contrib and (per §27) at big-table / warm-jmvtab scale.
+ **Numeric-only tables gain ~nothing** (cost = the data.table scan; `tab_num` already materializes
  once) — the win is factor-path-specific, i.e. the common exploratory workflow.
+ **Do NOT do a separate committable 9b-2** (local plain `tab_ci`/`tab_chi2` writers): the writer
  round-trips are a subset of the 9b-3 carrier win (9b-3 flows plain fields *through* ci/chi2) and a
  local unwrap→rewrap would be partly reworked by it. **Fold the plain writers into 9b-3.** L1-L7 of
  §3.3 remain the byte-identity ledger for that work.


##### 6. Phase 9b-3 — implementation (2026-07-11): increment 1 landed + corrected cost model

**Increment 1 (the single materialization seam) — DONE, byte-identical, perf-neutral.** Added
`fmt_materialize_col()` (`R/tab.R`, the ONE `new_fmt()` call, via `do.call` by exact field/attr
names so the historical `comp = x` partial-match to `comp_all` is reproduced exactly) + the
`fmt_frame_fields` / `fmt_col_attrs` contract constants. Both leaves (`tab_plain`/`tab_num`) now
build a per-column `frame`+`meta` and route through it at the SAME materialization point (the
`pmap_dfc` binding is kept, so column names/order are identical). Full suite green (1364/0), NO
golden regeneration; common per-table build 0.2648 vs baseline 0.2644 s (noise). This establishes
the single materialization point every later increment defers.

**Corrected cost model (profiling — `dev/benchmarks/results_1.4.0/phase9b3_profile.txt`).** The §5
decomposition was right that ~30% is recoverable record reconstruction, but the plan's *localization*
was wrong. Real-pipeline Rprof of the common build:

+ **The col_var full_join is NOT the target: 0.9%** (`vec_slice` 1.5%). A direct probe: `full_join`
  ~1.5 ms/join; `bind_cols` is 20× SLOWER (name-repair on the duplicated `Total`). **Do not
  reimplement the join (drop the L2 focus); keep the record `full_join` / materialize around it.**
+ **The reconstruction is PERVASIVE `dplyr`-over-fmt** (`mutate`/`across`/`transmute`/`is_*` each
  `vec_restore` every fmt column), not localized to slice/join. `new_rcrd` 12.6% is mostly the
  *rebuilds*, not the one-time materialize (floor 0.5%, §5 Part 3).
+ **The leaf tail is cheap on the common path** (default `total_names` make the renames no-ops;
  column renames/reorders don't row-reconstruct) — porting it banks little.
+ **`tab_apply_tests` / `tab_chi2` is the #1 recoverable chunk: 20%.** Isolated `tab_chi2` profile
  (43.6 ms/call, 8×5 table): `is_totrow.data.frame` 34.6% (called ~5×, each O(cols)), `transmute`
  33.6 / `select` 26.0 / `mutate` 26.5 (`dplyr`-over-fmt), helpers `tab_add_totcol_if_no` 19.7 /
  `tab_match_groups_and_totrows` 19.2 / `tab_match_comp_and_tottab` 17.2, `agg_chi2` 17.0 (the
  vectorised math). The `tabs[!is_totrow,]` slice alone = 4.0 ms.

**Pass 2 (2026-07-11) — the scan-primitive fold (byte-identical, ~11-15% factor-path).** Landing
from the `is_totrow.data.frame` 34.6% finding: `is_totrow`/`is_tottab`/`is_refrow` `.data.frame`
methods each built a full nrow×ncols logical tibble (`select(where(is_fmt)) |> map_df |>
if_all/if_any`) — replaced by one shared `fmt_row_flag()` (`R/fmt_class.R`) that reads the field per
fmt column and `reduce()`s (`&`=if_all / `|`=if_any). The old `partial` warning branch was **dead
code** (`if_all(-"complete") & !complete` ≡ FALSE) → not reproduced. **`is_totrow.data.frame` 28×
faster** (11.67→0.41 s / 3000× on a grouped table); per-table build **common 0.264→0.234 s (−11%),
ci 0.354→0.312 (−12%), contrib 0.445→0.380 (−15%)**, numeric neutral (few total rows). Full suite
green (1364/0), NO golden regen; verified byte-identical vs the old methods across grouped/ungrouped/
mixed/chi2 tables + both `partial` modes. A `vapply`-for-`map` swap on the other per-column scans
(`is_totcol`/`get_type`/`get_col_var`) was tried and **reverted** — no measurable gain (their cost is
S3 dispatch / call-count, which the carrier addresses, not the `purrr` wrapper).
`dev/benchmarks/results_1.4.0/phase9b3_pass2.txt`.

**Revised staging for the remaining core** (supersedes §3.4's join-first order):

1. **`tab_apply_tests` / `tab_chi2` on plain fields (the 20%, #1 value).** Compute the row masks
   (`is_totrow`/`is_tottab`) and col masks (`detect_totcols`/`get_col_var`) ONCE; extract the count /
   moment matrices once via `get_n`/`get_mean`/`get_var` (already cheap field reads); run
   `agg_chi2`/`agg_anova` on those; attach the `test` tibble without the fmt slice/regroup/rewrap.
   Golden-locked by `test-calculations.R` (chi2 vs `chisq.test` incl. Yates; Welch/classic F). This
   is the L6 read-path and delivers the biggest win; it needs the carrier (or a plain-field view) to
   exist at the tests boundary — i.e. the leaf→join must hand tests a plain-field carrier, not a
   materialized tab.
2. **Defer the leaf materialization** so the carrier reaches (1): leaf builds `frame`+`meta`, the
   record `full_join` runs on materialized leaves (cheap) then the result is unwrapped to a carrier,
   OR the leaves return a carrier and the join materializes around itself. Either keeps the tests +
   assemble on plain fields.
3. **`tab_assemble_tables` + `tab_add_n_pct`** (L7) on the carrier; `fmt_wrap` once at
   `tab_build_one` end. `tab_compact`/`pvalue`/`n_min` stay record-based (L3/L4 avoided).

Landmine ledger §3.3 still holds MINUS L2 (join not reimplemented). L1 (field types) + L5
(materialize boundary + derived display/digits) + L6 (`tab_ci`/`tab_chi2` plain read/write) + L7
(`tab_add_n_pct`) remain. The public `tab_ci()`/`tab_chi2()` stay exported as unwrap→shared-engine→
rewrap wrappers (maintainer, mid-turn 2026-07-11).
