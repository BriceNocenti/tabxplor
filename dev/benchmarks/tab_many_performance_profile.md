# tab_many() performance profile — findings and improvement map

Profiling study of `tab_many()` (and its `tab_plain()` / `tab_num()` workers) on a small and a
large dataset, done to steer the tabxplor 2.0.0 performance work. Every number below comes from the
runs described in the *Methodology* section and is reproducible from the scripts noted in the
appendix. Dates are absolute: this study was run on 2026-07-01.

## TL;DR — the five findings that matter

- **`tab_chi2()` is the single biggest cost, and it does not scale with the data.** Turning `chi2`
  off takes the small 9-table call from **2.39 s to 0.38 s** (chi2 = **84 %** of the call) and the
  15-million-row 15-table call from **3.95 s to 2.33 s** (chi2 = **41 %**). Its cost scales with the
  number of *cells* (rows × cols of each table), not with the number of observations.
- **Per-table fixed overhead dominates, not data volume.** One table over **15 M** rows takes
  0.39 s; one table over **21 k** rows takes 0.19 s. A 700× increase in rows adds only ~0.2 s. The
  bulk of a `tab_many()` call is constant per-table work in the fmt/vctrs/dplyr layer.
- **The finest-grain scan fusion (`.fine`) is not the lever.** It removes 14 of 15 sort passes yet
  changes wall-clock by only ~1.02–1.09× at 15 M, because aggregation is a minority of the total
  once chi2 and fmt construction are counted. Keep it for the Jamovi cache (WS7), not as the
  general speed-up.
- **`tab_num()` scans the raw data twice.** The total/subtotal block re-scans the full N rows
  (61–62 % of `tab_num` self-time on 8 M) *on top of* the main aggregation (37 %). Weighted means
  are worse: `weighted.var()` recomputes `weighted.mean()` internally, and the weighted 2-column
  mean over 8 M rows allocates **7.8 GB**.
- **`fmt` construction is the second fixed cost.** `new_fmt()` builds 15 per-cell fields (most of
  them all-`NA`) through `dplyr::case_when()` per output column; `vec_case_when` alone is 14–39 % of
  sampled time. Adding the planned `ref_n` field (WS1) makes this hot path heavier.

## Methodology

| Item    | Small              | Large factor                   | Large numeric               |
|---------|--------------------|--------------------------------|-----------------------------|
| Fixture | `forcats::gss_cat` | pc18-like full Cartesian       | `gen_big_df()`              |
| Rows    | 21,483             | 15,000,000                     | 8,000,000                   |
| Shape   | 1 & 9 factor×factor | 1 & 15 factor×factor, weighted | 2 numeric × 1 factor (means) |
| Source  | built-in           | `big_pc18_full_15M.rds`        | `big_df.rds`                |

- Tooling: `Rprof(line.profiling = TRUE, gc.profiling = TRUE)` for attribution, `bench::mark()` for
  allocations, and plain warm `system.time()` (median of 5, 2 warm-ups discarded) for authoritative
  wall-clock. `data.table` used 8 threads. Package loaded with `devtools::load_all()` (live source).
- Two caveats govern how to read the tables below:
  - **Rprof under-samples allocation-heavy R code.** The profiler's `sampling.time` came out ~35 %
    below the true warm wall-clock, and the gap is concentrated in the fmt/dplyr allocation churn
    that starves the sampler. So sampled shares *over-state* the well-sampled `data.table` C work
    (aggregation) and *under-state* fmt/chi2. Where a claim matters, it is anchored on the warm
    ablation numbers (chi2 on/off), which are real wall-clock, not on sampled shares.
  - **JIT warm-up costs ~3–5× on the first calls.** All headline numbers are steady-state (warm).
    First-call latency in a fresh session (relevant to Jamovi) is several times higher.

## Authoritative wall-clock and allocations

Warm wall-clock (median of 5) and `bench` allocation for the representative calls:

| Scenario                          | Rows       | Tabs | Warm s | Alloc MB | Note                     |
|-----------------------------------|------------|------|--------|----------|--------------------------|
| `tab_many` 1 tab, chi2             | 21,483     | 1    | 0.19   | 12.5     | small baseline           |
| `tab_many` 9 tabs, chi2            | 21,483     | 9    | 2.39   | 35.3     | small multi              |
| `tab_many` 9 tabs, **no chi2**     | 21,483     | 9    | 0.38   | —        | chi2 = 2.01 s = **84 %** |
| `tab_many` 1 tab, wt, chi2         | 15,000,000 | 1    | 0.39   | 238      | big single               |
| `tab_many` 15 tabs, wt, chi2       | 15,000,000 | 15   | 3.95   | 1,250    | big multi                |
| `tab_many` 15 tabs, wt, **no chi2**| 15,000,000 | 15   | 2.33   | 1,237    | chi2 = 1.62 s = **41 %** |
| `tab_many` 15 tabs, unweighted     | 15,000,000 | 15   | ~3.69  | 935      | wt adds ~0.4 s (bench)   |
| `tab_num` 2 num cols, unwt         | 8,000,000  | —    | 1.04   | 1,752    | means                    |
| `tab_num` 2 num cols, **weighted** | 8,000,000  | —    | 2.99   | **7,790**| weighted.var blow-up     |

Two derived facts drive the whole analysis:

- **Fixed per-table cost ≈ 0.19 s; the N-dependent scan adds ~0.20 s even at 15 M.** (1 table:
  0.19 s at 21 k → 0.39 s at 15 M.)
- **chi2 cost tracks cells, not rows.** 0.22 s/table on the high-cardinality gss tables vs
  0.11 s/table on the low-cardinality pc18 tables — and the same regardless of 21 k vs 15 M rows.

## Where the time goes — function attribution

Inclusive share of sampled time (`Rprof` `by.total`; read with the under-sampling caveat above).
Functions overlap because callers include callees (e.g. `tab_chi2` time includes its fmt work).

| Function                     | small 9-tab | big 15-tab (by_table) | Nature                          |
|------------------------------|-------------|-----------------------|---------------------------------|
| `tab_chi2`                    | 58.6 %      | 33.2 %                | fixed per-table (post-agg)      |
| `new_fmt` (via `vec_case_when`) | 32.6 %   | 12.9 %                | fixed per-column                |
| `tab_plain`                   | 13.4 %      | 54.2 %                | agg + pct + totals + fmt        |
| `forderv` (data.table sort)   | 0.3 %       | 26.0 % (self)         | N-dependent aggregation         |
| `[.data.table`                | 4.2 %       | 46.3 %                | N-dependent aggregation         |
| `relabel_levels_in_varnames`  | 0.9 %       | 0.5 %                 | already fixed in WS5            |

Reading: on **small** data almost everything is the fmt/vctrs/dplyr layer — `fmt_class.R` alone is
**52.7 %** of total self-time, and real data aggregation is ~5 %. On **big** data the aggregation
scan finally shows up (`forderv` + `[.data.table`), but `tab_chi2` (33 %) and `new_fmt` (13 %)
together still exceed it, and both are N-independent.

## tab_plain step breakdown

Self-time bucketed by source-line ranges inside `tab_plain()` (`R/tab.R:2009-2867`), summed across
all table calls in the run:

| Step (line range)                         | big 15-tab | big 15-tab, fused | small 9-tab |
|-------------------------------------------|------------|-------------------|-------------|
| 04 aggregation + dcast (2255-2293)         | **87.5 %** | 30.1 %            | 13.3 %      |
| 11 diff/mean/OR (2485-2669)                | 2.8 %      | 13.7 %            | 20.4 %      |
| 12 fmt build via `new_fmt` (2671-2749)     | 1.8 %      | 13.7 %            | 13.4 %      |
| 06 NA rows/cols (2309-2332)                | 2.0 %      | 8.2 %             | 12.5 %      |
| 08 total rows (2356-2402)                  | 1.3 %      | 4.1 %             | 10.4 %      |
| 10 percentages (2455-2484)                 | 0.3 %      | 4.1 %             | 9.3 %       |
| 09 split n/wn + Total col (2405-2453)      | 1.0 %      | 11.0 %            | 4.7 %       |
| 13 final assembly/rename (2750-2867)       | 1.3 %      | 9.6 %             | 7.3 %       |

- On big data, `tab_plain` **is** its aggregation line (`R/tab.R:2273`, the `data[, .N/sum, keyby]`
  scan) — 87.5 % of the function. Percentages, totals, diffs and fmt are noise at 15 M.
- With scan fusion, that 87.5 % collapses to 30 % (the rollup from the shared 207 k-row aggregate),
  and the residual `tab_plain` is now the fixed post-aggregation steps — but the big scan simply
  moved up into `tab_many` (the one `dt___[..., keyby=fine_keys]` build shows as `tab_many` self of
  3.09 s in the fused profile), so the total is unchanged.
- On small data the aggregation is only 13 %; the time is spread evenly across diff/mean/OR (20 %),
  fmt (13 %), NA handling (13 %), total rows (10 %) and percentages (9 %) — all fixed overhead.

Note the diff/mean/OR block (step 11) makes up to **five** `data.table::copy()` clones of the cell
table (`tabs_diff`, `tabs_mean`, `tabs_rr`, `tabs_or` from `tabs_pct`, `R/tab.R:2488-2566`). Cheap
at 15 M (cell tables are tiny) but part of the small-table fixed cost and the GC churn.

## tab_num step breakdown

Self-time bucketed inside `tab_num()` (`R/tab.R:2976-3920`):

| Step (line range)                          | big 8M unwt | big 8M weighted | small        |
|--------------------------------------------|-------------|-----------------|--------------|
| 06 total/subtotal **re-scan** (3331-3499)   | **62.1 %**  | **61.0 %**      | 28.3 %       |
| 04 main aggregation mean/var/n (3160-3275)  | 35.8 %      | 37.7 %          | 19.0 %       |
| 10 final assembly (3855-3920)               | 0.9 %       | 0.2 %           | 17.6 %       |
| 09 fmt build (3760-3854)                    | 0.3 %       | 0.5 %           | 12.7 %       |
| 08 diff/ci (3625-3758)                      | 0.3 %       | 0.0 %           | 9.6 %        |

- `tab_num` performs **two full-N scans**: the main aggregation, then a *separate* re-scan of the
  raw data for the totals via `purrr::map_dfr` over grouping sets (`R/tab.R:3379-3441`). The re-scan
  is the *larger* of the two (~62 %) because it recomputes mean/var/n over all N rows for each
  requested total level. On big data these two scans are ~98 % of the function.
- Weighted is 3× slower and 4.4× heavier (7.8 GB) because `weighted.var()` (`R/tab.R:5495`) calls
  `stats::weighted.mean()` **again** internally — the weighted mean is computed twice, once for the
  `_mean` column and once inside every `_var` — and each `map_if` over `.SD` materializes a
  full-length temporary per group.

## Memory and GC

- Big 15-table factor calls allocate ~1.25 GB and trigger ~10 GC/call: 15 independent `setDT`
  copies + 15 aggregations over 15 M + 15× fmt column construction.
- Weighting a factor table adds ~315 MB (the parallel `wn` fields and sums).
- `tab_num` weighted is the memory outlier at **7.8 GB** for 8 M rows — the double weighted-mean and
  per-group `.SD` temporaries described above.
- The `fmt` record itself is allocation-dense: every cell carries 15 fields
  (`R/fmt_class.R:1037-1045`), and unused ones (`ctr`, `var`, `ci`, `rr`, `or`, `mean` on a plain
  percentage table) are still allocated as full-length `NA_real_` vectors.

## Scan-fusion reassessment

Confirmed and quantified the CLAUDE.md "Perf findings": fusion is real but marginal.

| Metric                         | by_table (default) | fused (`.fine`) | ratio  |
|--------------------------------|--------------------|-----------------|--------|
| Warm wall-clock, 15 tab @ 15 M | 3.95 s (bench 4.07)| bench 4.00 s    | ~1.02× |
| `forderv` self share           | 26.0 %             | 8.3 %           | —      |
| Allocation                     | 1,250 MB           | 1,314 MB        | +5 %   |

Fusion removes 14 of the 15 sort passes (`forderv` 26 % → 8 %) but the freed time is a small slice
of the whole, so wall-clock barely moves and memory slightly *rises* (the 207 k shared aggregate is
kept live). Verdict: correct to keep it opt-in (`options(tabxplor.fuse_min_rows)`), valuable only
where the aggregate is reused across calls (Jamovi, WS7), not as a general-purpose speed-up.

## Bottlenecks, ranked

1. **`tab_chi2()` — 33–84 % of every real call, N-independent.** Biggest single lever. It re-runs a
   long chain of `dplyr::across(where(is_fmt))`, `group_split`, `rowSums` and per-column `set_var`
   over the fmt cell table (`R/tab.R:5052-5120`), once per row_var. It also rebuilds fmt columns.
2. **`new_fmt()` / `vec_case_when` — 13–39 %, per output column.** `dplyr::case_when()` for
   `display`, `color`, `type` runs per cell, per column, per table; 15 `NA` fields allocated
   unconditionally.
3. **`tab_num()` double full-N scan — ~98 % of `tab_num`.** Totals re-scan raw data instead of
   rolling up the aggregate; weighted mean computed twice.
4. **Aggregation scan — only genuine N-scaling cost, ~35–45 % sampled at 15 M (over-stated).** 15
   independent sorts+sums; fusion already explored.
5. **Per-table allocation / GC churn — up to 5 cell-table copies + full-width fmt records.**

## Improvement opportunities

Ordered by expected payoff for the *common* case (many modest tables), which is where users live.

- **Make `tab_chi2` vectorised and allocation-light (highest impact).** It operates on the small
  aggregated cell tables, so the cost is pure R/dplyr overhead, not maths. Options: compute
  contributions with matrix arithmetic on the numeric cell matrix instead of `across(where(is_fmt))`
  passes; compute over all row_vars' tables in one pass rather than one `tab_chi2` call per table;
  avoid re-wrapping results back into `fmt` mid-computation. Target: cut the 0.11–0.22 s/table by
  the majority. This alone would roughly halve the typical multi-table call.
- **Slim the `new_fmt()` hot path.** Replace the three `dplyr::case_when()` calls with a plain
  `switch`/vectorised assignment (the branches are column-scalar, not per-cell), and allocate the
  rarely-used fields lazily. Directly relevant to WS1 (`ref_n`): adding a 16th field to a per-cell
  record that is already the #2 hot spot should come with this slimming, or the fixed cost grows.
- **Roll up `tab_num` totals from the main aggregate instead of re-scanning.** Compute the total and
  subtotal rows from shared sufficient statistics (Σw, Σwx, Σwx² per group are additive across
  grouping sets), removing the second full-N scan (~60 % of `tab_num`). This mirrors the `.fine`
  idea but for the numeric path, which is currently *not* fused at all.
- **Fix `weighted.var()` to reuse the already-computed mean and share one pass.** Pass the mean in,
  or compute mean and variance together from Σw/Σwx/Σwx² in a single `data.table` aggregation. Kills
  the double weighted-mean and most of the 7.8 GB.
- **Reduce cell-table copies in `tab_plain` step 11.** `tabs_diff`/`tabs_mean`/`tabs_rr`/`tabs_or`
  are cloned from `tabs_pct`; compute in place or only when the corresponding output is requested
  (e.g. skip `tabs_mean` unless the "*2" ratio rule can fire). Minor wall-clock, real GC relief.

## Other findings relevant to 2.0.0

- **Bug (WS5): `tab_num(..., <tab_vars>, ci = "cell")` — FIXED (Phase 6e, golden-locked).** Was
  reproduced on `gss_cat` for both `comp` modes (`setorderv(tabs_tot, ...)` couldn't find the tab_var
  column when `ci = "cell"` forced `tot = "no"` → grand-total-only grouping-set). Fix: the grand total
  is now a length-1 list and `num_rollup()` keeps every tab_var present; locked by golden
  `n_ci_tabvars` / `n_ci_tabvars_all` and Phase 7d-i's `test-num-fuse-parity.R` `expect_no_error`.
- **WS1 (`ref_n`) cost sanity check.** `new_fmt` is already the #2 hot spot; a new per-cell field
  adds an allocation and a `vctrs::field` slot to every cell of every table. Pair it with the
  `new_fmt` slimming above so the net effect is neutral-to-positive.
- **WS2 (color diff/ratio split) is safe on perf.** `fmt_color_selection` / `color_formula` did not
  surface as a hot spot in any scenario (< 1 %); the split is a correctness/feature change, not a
  perf risk.
- **WS7 (Jamovi) — first-call latency.** JIT warm-up makes the first call in a session ~3–5× the
  steady-state cost, on top of the fixed per-table overhead. Interactive responsiveness will come
  from (a) the chi2/fmt fixed-cost cuts above and (b) caching the shared aggregate, far more than
  from the scan fusion itself.
- **Benchmark hygiene.** `bench::mark()` wall-clock matched warm `system.time` here, but its purpose
  is allocation; when timing allocation-heavy tabxplor calls, prefer warm `system.time` medians and
  treat `Rprof` sampled *shares* as structural, not absolute (it under-samples the fmt/dplyr layer).

## Appendix — reproduction

Scripts used (in the session scratchpad; not committed):

- `profile_tab_many.R` — Rprof + bench across the small/big factor scenarios, writes `*.Rprof` and
  `*_by{total,self}.csv`.
- `profile_tab_num.R` / `finalize.R` — `tab_num` line profiles, warm `system.time` timings, `bench`
  allocation, and the `ci = "cell"` bug repro.
- `analyze_steps.R` — buckets `summaryRprof(..., lines = "show")$by.line` into the step ranges of
  `tab_plain` / `tab_num` listed above.
- The committed `dev/benchmarks/run_fused_vs_bytable.R` is the fused-vs-by_table arbiter and remains the
  reference for that comparison.

To regenerate the intra-function step tables, the only non-obvious point is that
`summaryRprof(lines = "show")` (not `"both"`) returns the `$by.line` data frame that the bucketing
reads; `line.profiling = TRUE` must be set at `Rprof()` time.
