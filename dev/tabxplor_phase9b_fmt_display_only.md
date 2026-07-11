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

*(Original design sketch — its `9b-2/9b-3/9b-4` labels predate the pass-based renumbering and are
**superseded by §7**: the carrier is now Phases 9b-4→9b-7, jmvtab tier-3 is 9b-8. Kept for the design
history; read §7 for the authoritative numbering.)*

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
  fields (diff builds diff+ratio+CI), so `color_signif` stays a pure re-paint. This is Phase 9b-8,
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
  round-trips are a subset of the carrier win (the carrier flows plain fields *through* ci/chi2) and a
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

**Pass 3 (2026-07-11) — `tab_pvalue_lines` masked-fill (byte-identical, the big one: ~25-34%).** After
pass 2 a fresh line-profile pinned **`tab_pvalue_lines` at ~34% of the per-table build** (`chi2=TRUE`
adds a p-value row): the block filling the new row's empty cells ran an `if_else` over EVERY cell of
EVERY fmt column — `!is.na(.$display)` (the `$.tabxplor_fmt` → `vec_proxy` pull), `fmt0(...) |>
mutate(n=NA)` (the `mutate.tabxplor_fmt` `vec_proxy` round-trip), and a per-column `vec_restore` — i.e.
the `vec_case_when` 20% (all from `dplyr::if_else`), the `mutate.tabxplor_fmt` 7%, and much of the
`vec_restore` 33%, ALL to fill ~1 row. Replaced by a **masked assignment**: `col[is.na(get_display(col))]
<- fmt0(first(get_display(col)), type)` (with `field(repl,"n") <- NA`), a no-op on columns with no empty
cell. Byte-identical — `col[mask] <-` casts the value to the column's ptype, keeping its attrs, exactly
like the former `if_else(...) |> vec_restore(.)` (attributes verified preserved; the old `.$display` ≡
`get_display`). **Cumulative baseline→pass3: common merged 1.58→1.17 (−26%) / per-table 0.264→0.174
(−34%); ci −25%; contrib −26%; numeric neutral (no p-value line).** Full suite green (1364/0), no golden
regen; verified byte-identical vs the old block on grouped + ungrouped tables.
`dev/benchmarks/results_1.4.0/phase9b3_pass3.txt`.

**Pass 4 (2026-07-11) — `new_test_tibble` memoization (byte-identical, ~3-6% common build).** A
post-pass-3 line-profile: the empty-placeholder `test` tibble (`new_test_tibble()`, `R/tab_classes.R`)
costs **~1.4 ms/call** (`tibble()` validation) and is built several times per table (~3% self). It is
STATELESS, so it's memoized (built once, the cached copy shared — R copy-on-modify keeps callers'
`bind_rows`/`mutate`/`attr<-` edits from touching the base). Byte-identical (same object `tibble()`
produced); full suite green (1364/0), no golden regen. Modest (`common` per-table 0.174→0.164) — the
remaining tab_pvalue_lines cost (`bind_rows`+`map2_df(vec_restore)` adding the p-value row, ~9% total)
is the vctrs **record combine** (ptype2/cast per column), which is inherent to the fmt type and only
removed by the deferred-materialization carrier (the carrier core, Phases 9b-4→9b-7), not an in-place tweak.
`dev/benchmarks/results_1.4.0/phase9b3_pass4.txt`.

**NOTE (2026-07-11):** the pass-3 commit `7d08a77` carried a concurrently-committed **broken color
palette** (`c()` arg 11 empty → `load_all` fails). Restored `R/tab_classes.R` to the pre-edit palette
(`93eda62`) + re-applied the pass-3 `tab_pvalue_lines` change; the maintainer keeps the new palettes
externally (Phase 12). The uncommitted `tab_classes.R` = pass-3 + pass-4 changes + original palette.

**Revised staging for the remaining core** (supersedes §3.4's join-first order; itself refined into the
A-E phasing of **§7** — read that as the authoritative forward plan):

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


##### 7. Phases 9b-4 → 9b-7 — the carrier core, phased (2026-07-11): the authoritative forward plan

Passes 2-4 harvested the **in-place** wins (~26% merged / ~34% per-table build, all byte-identical):
the redundant scans (`is_totrow` fold, §6 pass 2), the O(all-cells)-to-fill-one-row waste
(`tab_pvalue_lines` masked-fill, pass 3), the `tibble()` overhead (`new_test_tibble` memoization,
pass 4). What is left of the ~99% O(cells) cost is the **irreducible-in-place** part: the
`tabxplor_fmt` record being *reconstructed* (vctrs ptype2/cast/restore per column) at every
`dplyr`/`vctrs` step. The only way to remove it is to **stop materializing the record until the end**
— the carrier (§3.1: per-column **field-frames** = `vec_data(fmt)` + **col-meta** = the 9 attrs +
**row-meta** = the factor cols; the single `fmt_wrap` = `new_fmt` per column, extending the
`fmt_materialize_col` seam from increment 1).

###### 7.1 The boundary (Q1 — settle first)

*Where* is "the very end"? The pipeline is `tab_build_one` (per row_var: leaves → join →
`tab_apply_tests` → `tab_assemble_tables`) **→** `tab_assemble_output` (cross-row_var: `tab_compact`
→ `tab_pvalue_lines` → `n_min`). Two candidate materialization boundaries:

| | **Boundary A** — end of `tab_build_one` (before compact) | **Boundary B** — the true end (after `tab_pvalue_lines`) |
|---|---|---|
| materializations | one **per row_var** | one **per whole table** |
| recovers | leaf-tail + join + ci/chi2 + assemble reconstruction | + `tab_compact` `vec_rbind` + **`tab_pvalue_lines` bind_rows (~15% of the build!)** |
| landmines | L1, L5, L6, L7 (L2 optional) | + **L3** (cross-subtable attr reconcile) + **L4** (grouped `as_refrow`) on the carrier + p-value row on the carrier |
| Phase 8 | clean — workers return finished tabs | workers return carriers; re-lock `test-parallel-parity` |
| est. recoverable | ~20-25% of the build | ~35-45% |

The profiling input that reopened this: `tab_pvalue_lines` is ~15% of the build and sits *after*
Boundary A, so A leaves it (and compact's `vec_rbind`) record-based. **Recommendation: build toward A
first (the bulk, lower risk) via Phases 9b-4 → 9b-6, decide B after** (9b-7 = the B-only extension) —
B is the literal reading of "records ONCE at the very end" but costs the two subtlest landmines + the
parallel re-lock.

###### 7.2 Phases (each byte-identical, golden-gated, one committable step)

Session grouping: **steps A+B = Phase 9b-4** (one session — the foundation), **C = 9b-5**, **D = 9b-6**,
**E = 9b-7** (each its own session — one landmine's worth of byte-identity work apiece).

**Phase 9b-4 — DONE, 2026-07-11 (byte-identical, full suite green, NO golden regen).** Implemented as
the **lean post-join round-trip** (maintainer decision): the carrier reaches the tests boundary via a
post-join `vec_data`-unwrap, NOT via the leaves emitting a carrier. Two internal helpers next to
`fmt_materialize_col` (`R/tab.R`): **`fmt_unwrap(tab)`** decomposes a built table to a carrier
`list(is_fmt, factors, fmt = per-col list(frame = as.list(vec_data(col)), meta = the 9 attrs), attrs =
attributes(tab))`; **`fmt_wrap(carrier)`** is its exact inverse (materialize each fmt column via the
single `fmt_materialize_col()` seam, pass the factor columns through, restore `attrs` wholesale). A
byte-identical **no-op** `fmt_wrap(fmt_unwrap(tabs_text))` is inserted in `tab_transform()` right before
`tab_apply_tests()` — establishing + validating the carrier at the tests seam in the real pipeline.
`tabs_num` is untouched (it does not cross the tests boundary). New `test-carrier-parity.R` locks
`identical()` across factor/numeric/mixed/weighted/col%/add_pct/ci shapes + grouped + subtext/test attrs
(15 tests). **L1** held (the fmt-contract `typeof` lock is green — `new_fmt` does no cast, so
`vec_data → new_fmt` preserves storage types); **L5** N/A here (the derived-display/digits are already
in the fields being round-tripped, not recomputed). Bench (gss_cat 5×3, merged): the no-op adds +0.08 s
(+6.9%) — the temporary second materialization of each row_var's factor table, recovered by 9b-5
(`dev/benchmarks/results_1.4.0/phase9b4_carrier.txt`).

**Phase 9b-5 — DONE, 2026-07-11 (byte-identical, full suite green FAIL 0 | PASS 1354, NO golden regen).**
Both increments landed: `tab_chi2()` (increment 1) + `tab_ci()` (increment 2 — bulleted last). A
**measurement reframed the whole phase**: the tests-boundary cost is the WRITES, not the reads.

- **Increment 1 — the chi2 whole-table TEST on plain fields** (`chi2_compute_test()`, extracted from
  `tab_chi2`) + the 9b-4 no-op removed. The `tabs2 <- tabs[!is_totrow,]` record-slice (which
  reconstructed every fmt column just to read counts) is gone: the row mask + a **fmt-FREE** group
  view (`select(tabs, !where(is_fmt))[mask2,]`) drive `group_indices`/`group_keys`, and the
  `agg_chi2`/`agg_anova` marshalling reads `get_n(tabs[[cc]])[mask2]` (plain). Read-only → cell
  byte-identity by construction. **VERDICT: NO measurable win** — a clean git-stash A/B on a 40×15
  table was 0.1000 s == 0.1000 s. The §6 "tab_chi2 = 20%" was an ISOLATED `tab_chi2` with the DEFAULT
  `calc = c("ctr","p","var","counts")` (i.e. the CONTRIB writes), NOT the pipeline's `calc = "p"`
  test. On `color = "diff"` there are no contrib writes, so the test marshalling is cheap. Value =
  clarity + no-op removal + scaling on very large tables, not a demonstrable speedup.
- **The real lever = the WRITES** (maintainer chose to fold the chi2 half in now): contrib **+97 %** vs
  a plain build; the ci write-back **+55 %** (deferred to increment 2). **`chi2_write_contrib()`**
  moves the per-cell `var`/`ctr` + `comp_all`/contrib-`color` writes from the pre-9b-5 **~6 successive
  `mutate(across(where(is_fmt), set_*))` passes** (each a full fmt reconstruction) to **ONE**
  `mutate(across())` over **plain-precomputed vectors**. **Result: contrib per-table 0.2963 → 0.1747
  (−41 %, 1.7×) and ~30 % less memory** (`dev/benchmarks/results_1.4.0/phase9b5_chi2.txt`);
  common/ci/numeric flat.
- **Approach note (deviation from "shared carrier engine over `fmt_unwrap`/`fmt_wrap`").** The write
  path is NOT a carrier round-trip but a **precompute-then-single-write**: read the fields plainly
  (`get_wn`/`get_var`/`is_totrow` — no reconstruction), run the group sums through the SAME dplyr on
  **fmt-free** tibbles, then apply the **real setters** in one `mutate`. Fewer reconstructions than a
  carrier (1 vs unwrap+wrap) AND no `set_color`/`set_*` reproduction risk. Two byte-identity landmines
  handled: (a) `var_contrib` is **PER SUBTABLE** (the old writes were GROUPED mutates → each subtable
  uses its own last/total row) — computed via `group_indices` + the write runs **ungrouped then
  restores grouping** (the ctr divide + colour are row-wise); (b) the old `dplyr::if_else()` over fmt
  columns **MATERIALISES the `wn` field** (NA → the `n` fallback) as a vctrs side effect — reproduced
  explicitly with `set_wn(get_wn())` under `"ctr"` calc (matters for the `$wn` user contract on an
  unweighted `tab_plain() |> tab_chi2()`). `variances_by_group`/`cells_by_group` of the old path were
  **dead code** (computed, never used) and dropped. Locked by a 10-shape git-stash `identical()` A/B +
  `test-calculations.R` (variance-contributions, chi2+Yates, Welch/classic F) + `test-color-golden.R`.
- **Increment 2 — `tab_ci` write-back DONE (2026-07-11, byte-identical; 21-shape A/B + full suite;
  `phase9b5_ci.txt`; ci per-table −20 %, 1.25×; net −58 lines).** Same precompute-then-single-write.
  (2a) The reference-row selection + reference stats (the grouped `ref_rows`/`ref_to_na` transmutes +
  the `x_n`/`ref`/`ref_var`/`ref_n` transmutes) → a plain loop: **`group_last_pos(mask)`** = the
  per-subtable last-reference-row absolute index (the plain form of `.[dplyr::last(which(<mask>))]`
  under grouping), and the `ci_*` engine reads plain fields at those positions. Dead `tot_rows`
  dropped. (2b) The CI write (`with_groups(NULL)` mutate) + `comp_all` + `visible` display → ONE
  ungroup/mutate/regroup; **`ci_type`/`color` stays the positional `tabs[ci_yes_ref] <- map2_df`**
  (byte-identical, so the **L-IDX** recycle quirk needs no guard/reproduction). `wn`-materialise here
  is subtler than chi2's: the old `comp_all`/`visible` were **GROUPED mutates** whose per-group
  RECOMBINE fills `wn`, so `set_wn(get_wn())` is applied only when grouped, for comp_all's all-fmt
  (diff_row) / visible's own columns. Locked by the 21-shape A/B (incl. grouped-visible) +
  `test-golden`/`test-calculations`/`test-color-golden`.

*Why step A was dropped (leaf emits carrier + tail port).* Under **Q2 = keep the record `full_join`**
(the recommended lean below), each leaf materializes for the join regardless, so having the leaf *return*
a carrier and porting its post-`new_fmt` tail (`no_col_var` `set_display`/`set_type`/`as_totcol`,
total-renames, `finalize_color_spec`, the grouped/ungrouped decision) is delicate byte-identity work
that **never becomes load-bearing under Boundary A** (the leaf tail is pre-join; the carrier only lives
post-join, `tests → assemble → single wrap`). The lean round-trip reaches the exact same milestone (a
faithful carrier at the tests boundary) with ~1/5 the surface. If Boundary B / a carrier-side join is
ever chosen, the leaf-tail port returns as a scoped task then.

The original step-A/step-B design (kept for history):

+ **9b-4 (step A) — carrier infra + leaf emits carrier.** Build the carrier struct + `fmt_wrap()`.
  `tab_plain`/`tab_num` return a carrier; their post-`new_fmt` tail (totals-rename, no-col-var, wrap)
  runs on the carrier; `tab_transform` calls `fmt_wrap` immediately so **downstream is unchanged** —
  this validates the round-trip + the tail port with zero behaviour change. **L1** (the fmt-contract
  `typeof` lock catches any field-type drift), **L5**. Low payoff alone (the leaf tail is cheap on the
  common path, §6) — it is the foundation for B-D.
+ **9b-4 (step B) — carrier through the col_var join; materialize before `tab_apply_tests`.** Sub-decision
  **Q2**: reimplement the join on the carrier (**L2** row/col reorder) *vs* — since the join is
  **cheap (0.9%)** — keep the record `full_join` and cheaply `vec_data`-unwrap back to a carrier after
  it (2 cheap materializations, **no L2**). Lean the latter; the join is not worth the reorder
  landmine.
+ **9b-5 (step C) — `tab_ci`/`tab_chi2` shared carrier engine (the payoff, ~14%).** The CI/chi2 **math
  already runs on plain vectors** (`R/tab-agg.R` `ci_*`/`agg_chi2`/`agg_anova`); only the
  *orchestration* is `dplyr`-over-fmt (grouped reference-row selection = `last(which(is_totrow))` w/
  `is_refrow` fallback, `detect_totcols`/`detect_refcol`, `tab_match_*`, the `tabs[!is_totrow,]`
  slice). Extract it into ONE engine over the carrier: masks computed once, writes plain
  `ci_inf`/`ci_sup`/`pvalue`/`var`/`ctr` field columns + `ci_type`/`color`/`comp_all` col-meta. Public
  `tab_ci()`/`tab_chi2()` stay exported as **unwrap → engine → rewrap** wrappers (maintainer). **L6** —
  the hardest (grouped reference logic; chi2 byte-identity vs `chisq.test` incl. Yates, golden-locked
  by `test-calculations.R`), highest value.
+ **9b-6 (step D) — `tab_assemble_tables` + `tab_add_n_pct` on the carrier; `fmt_wrap` once at the end of
  `tab_build_one`.** Level-drop / total-removal / totaltab-removal → field-frame/row-meta ops;
  `tab_add_n_pct`'s add-n/pct columns + pct="col" base rows → plain field appends (the exact NA
  flavors + `detect_totcols` base-column selection of §3.3 L7). **L7**. Completes **Boundary A** →
  a shippable, lower-risk carrier. `tab_compact`/`pvalue`/`n_min` stay record-based, byte-identical by
  construction.
+ **9b-7 (step E, only if Boundary B) — carry past compact + pvalue.** `tab_compact` on the carrier
  (reproduce **L3**'s per-attribute `if_else(same, x, neutral)` reconcile across the stacked sub-tables
  + **L4**'s grouped `as_refrow` — both already isolated in 9b-1's `promote_totrow_to_refrow`) + add
  the p-value row *on the carrier* before the single `fmt_wrap`. Recovers the last ~15-25% (the pvalue
  `bind_rows` + the compact `vec_rbind`). Re-lock `test-parallel-parity` (workers now return carriers).

###### 7.3 Open questions to settle before committing code

+ **Q1 — Boundary A vs B** (§7.1): recovers ~20-25% vs ~35-45%, at a real L3/L4 + parallel-contract
  cost. The one that changes the whole shape.
+ **Q2 — carrier-join (L2) vs materialize-around-the-cheap-join** (9b-4 step B): lean the latter (join
  is 0.9%).
+ **Q3 — is the carrier worth it now?** It is the **largest byte-identity surface in 1.4.0** for
  another ~20-45%, and the value is **back-loaded** (9b-4 = low-payoff infrastructure; **9b-5 is the
  win**). Weigh vs pausing 9b at passes 2-4 (already banked ~26% cheaply) and landing the carrier with
  Phase 10.
+ **Q4 — sequence 9b-5 (the `tab_ci`/`tab_chi2` engine) with Phase 10 exporter-prep**: both touch the
  same fmt read paths (the engine and the exporter `get_*` reads) — possible consolidation.

Each phase's gate: `devtools::test()` FAIL 0 + NO golden regen (`test-golden`/`test-fmt-contract`/
`test-color-golden`/`test-calculations`/`test-fuse-parity`/`test-num-fuse-parity`/`test-carve-parity`/
`test-jmvtab-cache`/`test-parallel-parity`), plus a before/after benchmark into
`dev/benchmarks/results_1.4.0/`.
