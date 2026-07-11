# Phase 9b — `tabxplor_fmt` as display-only, materialized at the end

Feasibility analysis + implementation record for the 1.4.0 roadmap's Phase 9b. Read
`dev/tabxplor_1.4.0_decisions.md` §27 (the fmt-build cost finding) and §29 (the fresh profile
+ "Finding 4 is the lever") first — this doc is the detailed follow-through on that lever.

## 1. Grounding — where the time actually is

The §29 profile (`tab(gss_cat, 5 row_vars × 3 col_vars, pct="row", color="diff", chi2=TRUE)`,
21 483 rows) splits a `tab()` call as:

| stage | s/call | share | nature |
|-------|--------|-------|--------|
| `tab_setup` (all arg resolution + row/col-axis recycling) | 0.005 | 0.2 % | control flow |
| `tab_prepare_pop` | 0.008 | 0.4 % | data prep |
| `tab_aggregate` (the real O(N) data work) | 0.001 | ~0 % | data.table GForce |
| `tab_transform` (leaf scan + fmt build + ci + chi2) | ~0.7 | ~33 % | O(cells), vctrs-bound |
| `tab_assemble_tables` | 0.05 | 2 % | O(cells) |
| `tab_assemble_output` merge (`tab_compact`) + p-value lines | ~0.72 | ~34 % | O(cells), vctrs-bound |

`Rprof` reading: **`vec_case_when` = 40 % of total (72 % of `tab_compact`)**; the rest is
`tabxplor_fmt` record reconstruction (`structure`/`new_data_frame`/`list_unchop`/
`vec_restore_dispatch`/`df_list`/`vctrs::field`). Argument resolution + the whole row/col-axis
vectorisation is **0.2 %**; the O(cells) `tabxplor_fmt` machinery is **~99 % of the build**, bound
by `dplyr::case_when`/`if_else`-over-fmt + record round-trips, not by control flow.

Two facts from the code exploration reframe the problem:

- **The build math is already plain.** `R/tab-agg.R` (moment scan, `num_derive_stats`, the
  `ci_*` engine, `agg_chi2`/`agg_anova`) and `tab_apply_reference()` ([tab.R:3144](R/tab.R#L3144))
  operate entirely on plain `data.table`s / atomic vectors. `tab_num()` even computes CI + p-value
  on plain vectors *before* `new_fmt`.
- **The record is materialized too early, then mutated in place.** `new_fmt` fires inside each
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

### Two separable wins

1. **The merge (`tab_compact`)** — a single `if_else`-over-fmt hotspot, killable with a direct
   field write, no representation change. Safe, byte-identical. **This is Phase 9b-1 (done).**
2. **The leaf build + the ci/chi2/assemble round-trips** — recoverable only by deferring the
   single `new_fmt` materialization to end-of-build (the plain-carrier rewrite). Large, risky.
   **This is the gated Phase 9b-2/9b-3.**

Realistic ceiling of the whole phase: *roughly halving the build*. The aggregate is already ~0 %
and stays; `format()` for display (console/kable/md) is a **separate Phase 10b** concern and is
out of scope here.

## 2. Phase 9b-1 — surgical merge field-write (done, byte-identical)

`tab_compact()` promotes a merged sub-table's total row to its reference row when the sub-table
has no explicit reference (so each stacked sub-table colours against its OWN total). The old code
did this with `dplyr::across(where(is_fmt), ~ if_else(is_totrow(.) & !any(is_refrow(.)),
as_refrow(.), .))` — a `vec_case_when` over each fmt column, reconstructing the whole record.

`as_refrow` ([fmt_class.R:728](R/fmt_class.R#L728)) does *only* flip the `in_refrow` field, so the
promotion is byte-identically a direct masked field write. New internal helper
`promote_totrow_to_refrow()` (in `R/tab_classes.R`, next to `tab_compact`):

- `if (any(field(col, "in_refrow"))) return(col)` — sub-table already has a reference row;
- else set `field(col, "in_refrow")[in_totrow] <- TRUE`.

Kept inside the `imap` step, so `any(in_refrow)` stays grouped per `row_var` (landmine L4 below).

Result (`dev/benchmarks/results_1.4.0/phase9b1_tab_compact.txt`, gss_cat 5×3 fixture):

| metric | before | after | speedup |
|--------|--------|-------|---------|
| `tab_compact` isolated | 0.390 s | 0.160 s | **2.44×** |
| merge delta (merged − list call) | 0.530 s | 0.310 s | 1.71× |
| full merged call | 1.780 s | 1.550 s | 1.15× |
| `output_list` call (no merge) | 1.250 s | 1.240 s | ~1.00× |

The removed 0.23 s is the `vec_case_when` share (~72 % of `tab_compact`) — matches the profile. The
`output_list` call is unchanged: the win is entirely in the merge; the leaf build is untouched.
Full suite green (FAIL 0 | PASS 1339), **no golden regeneration**.

The 0.160 s remainder in `tab_compact` is the `imap_dfr`/`vec_rbind` reconstruction across
sub-tables (the L3 per-attribute reconciliation). It is deliberately **not** hand-rolled here — a
bespoke unwrap/rbindlist/rewrap would re-introduce the L3 reconciliation landmine ad-hoc. It is
the natural first win of the plain-carrier merge in 9b-3.

## 3. Phase 9b-2/9b-3 — the plain-carrier rewrite (designed, gated)

Not to be implemented until this design is approved. Carrier shape chosen (this session):
**unwrapped-fmt-columns** — minimal conceptual distance from today's record, lowest translation
risk, preserves the fast GForce wide math.

### 3.1 The carrier

Between the leaf math and the single materialization, an in-build table is:

- **field frames** — one small `data.table` per output column, holding that column's 18 raw fields
  (`n int`, `digits int`, `in_totrow/in_tottab/in_refrow logical`, `display char`, the 11 doubles)
  — i.e. exactly today's `vctrs::vec_data(fmt)`, un-classed;
- **col-meta sidecar** — a tibble keyed by output column, one row per column carrying the 9 scalar
  attributes (`type`, `comp_all`, `ref`, `ci_type`, `col_var`, `totcol`, `refcol`, `color`,
  `color_signif`);
- **row-meta sidecar** — the `levels` / `row_var` / `tab_vars` columns.

`new_fmt` is called **once per final output column**, after `tab_compact`, **before**
`tab_pvalue_lines`/`tab_apply_n_min` (which stay record-based — they are cheap, ~0). The wide
GForce math in the leaves is untouched; only the *carrier between leaf and display* changes.

### 3.2 What changes

- **Leaves** (`tab_plain`/`tab_num`): return the carrier (field frames + col-meta) instead of a
  tibble of fmt records. The math up to the current `new_fmt` is unchanged.
- **`tab_ci`/`tab_chi2`**: become **plain-field writers**. The math already runs through the plain
  `R/tab-agg.R` engine; they write plain `ci_inf`/`ci_sup`/`pvalue`/`ctr`/`var` columns into the
  field frames instead of `set_*` on records. (`tab_num` already does exactly this for its numeric
  CI — the template.)
- **Assemble/level-drop/add_n/total/merge**: operate on the field frames + col-meta (data.table
  ops), not dplyr-over-fmt.
- **Materialize**: `imap(field_frames, ~ new_fmt(!!!.x, !!!col_meta[[.y]]))` with a final
  type-cast pass (see L1).

### 3.3 Byte-identity landmine ledger (mitigations the rewrite MUST honor)

Ranked most-likely-to-break first (from the adversarial review):

- **L1 — type coercion at materialization.** `new_fmt` does not cast. Re-cast every field to its
  contract type at the single materialization point (`n`/`digits`→`integer`, `in_*`→`logical`, the
  11 doubles, correct `NA_integer_` vs `NA_real_` flavor). Never let a data.table `:=`/join/
  `set_num(NA_real_)` on an `n`-cell silently promote `n` to double — the golden RDS + fmt-contract
  compare *storage type*, so this breaks with no visible text change. Universal — breaks first.
- **L2 — join row/column reorder.** `data.table` `merge(all=TRUE)` sorts by key; the numeric×factor
  `full_join` ([tab.R:1804](R/tab.R#L1804)) and the factor `reduce(full_join)`
  ([tab.R:1709](R/tab.R#L1709)) preserve **factor-level** row order plus a specific `col_vars`
  column permutation ([tab.R:1806-1814](R/tab.R#L1806)). Replicate with `sort = FALSE` + an
  explicit reorder to the `row_var` factor levels and the `col_vars` order (text→0L, unknown→last,
  totals last).
- **L3 — cross-subtable attribute reconciliation is NOT "take the first".** `vec_ptype2.tabxplor_fmt`
  ([fmt_class.R:2844-2852](R/fmt_class.R#L2844)) collapses any *differing* attribute to a neutral
  (`type→"mixed"`, `col_var→"several_vars"`, `ref`/`ci_type`/`color→""`, `totcol`/`refcol`/
  `comp_all→FALSE`, `color_signif→"ignore"`). Post-Phase-6 globalisation makes this a no-op on the
  common path, but it is reachable + golden-locked via per-row_var `ref` (§5 named vector) and
  per-col_var `refcol` (Phase 7g-iii). The carrier merge must run the **same per-attribute
  `if_else(same, x, neutral)`** across all stacked sub-tables, not copy sub-table 1's meta.
- **L4 — grouped `as_refrow`.** `!any(is_refrow)` must be evaluated per `row_var`, never globally
  (already handled correctly in 9b-1; the carrier merge must keep it grouped).
- **L5 — the materialization boundary.** `tab_pvalue_lines` (constructs fmt cells, adds rows),
  `tab_apply_n_min` (writes the `display`="blank" mask, reads `get_tot_n`/`get_n`), and
  `finalize_color_spec` run *after* the merge and touch fields. Materialize *after* `tab_compact`
  but *before* those. Reproduce the per-column derived display at materialization:
  `digits`-from-mean-magnitude ([tab.R:4108-4113](R/tab.R#L4108)) and the `display`/`type`/`ref`/
  `comp` mapping ([tab.R:2988-3009](R/tab.R#L2988)). Pass **absolute** `ci_inf`/`ci_sup`, never the
  `ci` half-width (avoids the `ci_center` shim drift).
- **L6/L7 — field surgery in `tab_add_n_pct` and the `tab_ci` factor path.**
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

### 3.4 Phasing sketch (each byte-identical, golden-gated, benchmarked)

- **9b-2** — leaves return the carrier; `tab_ci`/`tab_chi2` write plain fields into the field
  frames. Materialize at the *end of each leaf* still (no boundary move yet) — this isolates the
  "plain-field writers" change from the "defer materialization" change. Golden must stay green.
- **9b-3** — move the single `new_fmt` materialization to *after* `tab_compact`; assemble + merge
  operate on the carrier; the L1-L7 mitigations land here. This is where the leaf-build + merge
  round-trips are actually recovered.
- **9b-4 (optional)** — jmvtab tier-3 stores the carrier (plain field frames) instead of the
  materialized armed fmt table, and re-paints on the raw fields (see §4).

### 3.5 Go/no-go verdict

**GO — but strictly staged and gated, and only now that 9b-1 has banked the safe merge win.**

Rationale + realistic estimate: after 9b-1, the merged call is 1.55 s, of which the merge is now
~0.31 s and the `output_list` (no-merge) build is ~1.24 s for 5 tables (~0.25 s/table). That
per-table 0.25 s is leaf fmt build + `tab_ci`/`tab_chi2` `set_*` round-trips + assemble — the
carrier's target. Eliminating the early materialization + the `set_*` record round-trips plausibly
takes 30-50 % off the per-table build; combined with the carrier merge (the 0.16 s `vec_rbind`
remainder), the 5×3 merged call could drop from ~1.55 s toward ~0.9-1.1 s. **This estimate is a
range, not a promise** — it must be confirmed by a 9b-2 spike before committing to 9b-3.

Risk: 9b-3 is the single largest byte-identity surface in 1.4.0 (7 named landmines, touches
`tab_plain`/`tab_num`/`tab_ci`/`tab_chi2`/`tab_assemble_tables`/`tab_add_n_pct`/`tab_compact` +
the jmvtab cache). It is worth it *only* as a golden-gated, multi-session, one-sub-phase-at-a-time
effort. If the 9b-2 spike shows <25 % recoverable, stop and keep only 9b-1.

## 4. Interactions

- **jmvtab tier-3 cache** (`R/jmvtab-cache.R`): today it stores the **materialized armed fmt table**
  and re-paints its fields on every hit (`finalize_color_spec`/`jmv_reapply_digits`/
  `jmv_apply_display`, all `set_*` on records). After 9b-3 the cache could store the **carrier**
  (plain field frames — smaller, no record overhead) and materialize + paint once on read; the
  re-paint would run on plain columns, faster. It does **not** need the "which fields exist"
  tracking the design sidestepped (§7c) — the carrier still carries the canonical superset of
  fields (diff builds diff+ratio+CI), so `color_signif` stays a pure re-paint. This is 9b-4,
  optional and independent.
- **Exporters**: `tab_xl()` is already field-native (reads `get_num`/`get_display`/`get_digits`/
  `get_type`/`get_color`, bypasses `format`) — it needs no materialized `format()` output and could
  even read the carrier directly. `tab_md()`/`tab_kable()`/console go through
  `format.tabxplor_fmt` and therefore need the materialized fmt — which the final table still has,
  unchanged.
- **Phase 10b** owns the `format.tabxplor_fmt`/`get_reference` `case_when`→base rewrite (a *display*
  speedup: console/kable/md/jmvtab render). Orthogonal to 9b — kept there.
- **Phase 11** (openxlsx2) is unaffected: `tab_xl`'s field-native read is representation-stable.

## 5. Status

- **9b-1 — DONE** (2026-07-11). `tab_compact` field-write fix; `tab_compact` 2.44× faster,
  byte-identical (FAIL 0 | PASS 1339, no golden regen). Record:
  `dev/benchmarks/results_1.4.0/phase9b1_tab_compact.txt`.
- **9b-2/9b-3/9b-4 — DESIGNED, GATED.** Awaiting go-ahead + a 9b-2 spike to confirm the recoverable
  fraction before the full carrier rewrite.
