


### Phase 0 — Safety net (done — 2026-07-07)

Retro-compat tests + benchmarks BEFORE any refactor. Nothing below is safe without this. The net is GREEN on the current 15-field baseline; it deliberately locks *current* behavior so every 1.4.0 change is a conscious regeneration (never a silent drift). No safety-net assertion should fail on the current source — the "what must change later" is the tripwire ledger, not a red test.

- Retro-compat safety net: `test-fmt-contract.R` (locks the 15 fields + 8 attributes), `test-golden.R` (characterization matrix + `_golden/*.rds` + `_snaps/`), `test-export-parity.R` (format vs `tab_xl` display parity), `test-fuse-parity.R` (fused vs `.by_table`).
- **dplyr-verb coverage** in `test-tab_classes.R` (44→93 tests): class preservation for ~10 verbs on both tab classes, PLUS table-attribute (`subtext`/`chi2`) survival across every verb, the `group_by` flat→grouped upgrade, the `lv1_group_vars()` auto-downgrade, and `group_split`. Fixtures use `tab_plain()|>tab_chi2()` (the real chi2-attr populator — `tab(chi2=TRUE)` does NOT fill it) + a sentinel `subtext`.
- Perf: small `gss_cat` benchmark runs in-suite as `test-benchmark.R` — informational, NEVER fails; prints a comparison (median_s/base_s/diff_s + mem) against committed `tests/testthat/benchmark_baseline.csv` (ships with tests; regenerate via `dev/make_benchmark_baseline.R`). Visible under `devtools::check()`; `skip_on_cran`. Shared ops in `helper-benchmark.R`. The heavy 8M-row run is `dev/benchmarks/run_bench.R` (`.Rbuildignore`'d; `source("dev/benchmarks/run_bench.R")`), which builds the fixture via `gen_big_df.R` and writes/compares its own `dev/benchmarks/baseline.csv`. `bench` is Suggests-only (falls back to `system.time`).
- **"Before" tripwire cases** for decided-but-unbuilt changes (generated from current code so the later diff is conscious): `f_ci_diff` (Phase 3 Newcombe + stars), `f_or` (empirical OR), `n_mean_ci` (Phase 3 mean-CI bounds), `f_totcol_each` (Phase 6 one-total-col). `ref_n`→`tot_n` terminology reconciled; `refn_*` fixtures renamed `totn_*`. A per-fixture **tripwire ledger** (which phase regenerates which fixture, and why) heads `test-golden.R`.
- Skills `/color-mode`, `/dplyr-method`, `/vctrs-field` — all live.

**Golden regeneration protocol.**

`test-golden.R` compares against `saveRDS` fixtures + `_snaps/` snapshots produced by `dev/make_golden.R`. When a change **intentionally** alters output (e.g. `tot_n` making cross-`col_var` percentages exact), re-run `Rscript dev/make_golden.R` (and `testthat::snapshot_accept()` for display snapshots), **review the git diff of `_golden/`/`_snaps/`, and accept it consciously**. Never regenerate blindly to make a test pass.

### Phase 1 — Combined fmt field-contract pass

One vctrs-record surgery, BEFORE the core rewrite → **18 fields**: add `pvalue`, `tot_n`, `ci_inf`, `ci_sup`; **rename the unused `rr`→`ratio`** (placed after `diff`); **drop `ci`** (recomputed from the bounds on `$`/`get_ci()`; `fmt(ci=)` arg kept); numeric `diff` = difference; `mean`-overload removed. Fold the logit field/display prep below into it. Split **1a** (contract: field defs + accessors + the `set_ci`/`get_ci` **bounds-shim** keeping display byte-identical; regenerate RDS golden fixtures once, `_snaps/` untouched; `test-fmt-contract.R` rewritten 15→18) / **1b** (writers, folded into Phases 2-3) — decisions doc § *Status* (Phasing). Detail + caveats + touch-list: `dev/tabxplor_1.4.0_decisions.md` §1-3, §9, §12. Skill: `/vctrs-field`.

#### Done (Phase 1a — 2026-07-07)

Field contract reshaped 15→18 in `fmt_class.R` (`new_fmt`/`fmt`/factories/`get_num`/`set_num`/`$`/`vec_cast`/`vec_arith`/`vec_math`): `rr`→`ratio` (after `diff`), added `ci_inf`/`ci_sup`/`pvalue`/`tot_n` (NA-defaulted — writers deferred to 1b), dropped the `ci` field. **Bounds-shim**: `set_ci()` stores the half-width as `ci_sup = v`, `ci_inf = -v`; `get_ci()`/`$ci` read it back from `ci_sup` — display byte-identical (the `ci_type` attribute is set *after* `set_ci` in `tab_ci`, so an estimate-based center could not be recovered → half-width storage, not `est∓v`). `fmt(ci=)` arg kept (maps to the bounds). Also fixed the `fmt()` `refcol`-cast bug (§9). Two writer sites re-pointed off `ci=`: `tab_num`'s `new_fmt` (mean-CI → `ci_sup`/`ci_inf`) and the chi2 pvalue-line `mutate` in `tab_classes.R`. Golden RDS regenerated (structure); `_snaps/golden.md` unchanged (byte-identity verified); `test-fmt-contract.R` locks 18 fields + the shim. Suite green (232/0). **Deferred to 1b/2/3/5**: numeric `diff` flip, `mean`-overload removal, real `tot_n`/`pvalue`/asymmetric-CI writers, `ratio` repurposing.

#### To implement

1. Some careful modifications of vctrs fields for class `tabxplor_fmt`, along with changes in tables code to work with them. The main change would be to add a new field with the reference total count `ref_n`, for each fmt value, to do all relevant calculations with this data (instead of relying on, and introduces approximation when different columns variables do not have the same exact same total count due to missing values, as the default behaviour is to use only the total column of the last `col_var`). Would `ref_wn` be necessary too ? Then, all the use of totals should be fully rewritten and rethougth. **Resolved (§2, §11): the stored field is `tot_n`** (each cell's own unweighted base; `ref_n` renamed); `ref_wn`/`tot_wn` is NOT stored — recovered as `wn/pct` via `get_tot_wn()`; the full totals rewrite is Phase 2's aggregate-core.

**Logit field/display prep** (fold the *field* needs into this pass; the *display* items — 1/OR, stars — land in Phases 3/7):

Prepare tab_logit() integration into tabxplor_fmt class and `tab()` calculations and display :
- OR : column ref default to 2, or last (otherwise it's done for the "no" column, which is not user-friendly !) ? **Resolved (Q16, §19): keep `ref2="first"`** — the maintainer's data convention puts the positive level first ("Oui" first); glm-convention alignment decided at tab_logit integration.
- OR : when OR < 1, print 1/OR everywhere at display level for the user to be able to compare OR between 0 and 1 to OR > 1 meaningfully since it’s by construction symetric that way. For example, if `OR = 0.25`, we should calculate the inverse `1/0.25 = 4`, and print `1/4` (console + exports ; would a Excel cell format permits it ?)
- OR : print signif stars *** ** * (cf. above)
- OR : with 2 levels, no ref2 and all OR calculated (positive/negative levels) ; with 3 levels, ref2 needed
- rr / relative risks : **resolved (Q3)** — the renamed `ratio` field holds the relative risk `cell_pct/ref_pct` for pct columns (and `cell_mean/ref_mean` for numerics); it is the RR step feeding empirical OR. No `mean`/`diff` overload (§3, §12).
- how to intelligently print : OR + ME ; mod_OR + emp_OR ; OR + PCT ?

### Phase 2 — Aggregate core + math unification

Extract the canonical count-aggregate; one implementation each of pct/diff/OR/totals over it; route `tab_plain`/`tab_num` through it; re-make `tab_pct`/`tab_tot`/`tab_totaltab` as superseded thin wrappers. Per-cell `tot_n` (§2; the weighted base recovered as `wn/pct`) + the globalised row_var axis (§5) let each cell compute its pct/diff/CI/test from its own fields — retiring `detect_totcols` and building exactly one total column. Preserve the ordering invariant inside the core (non-first levels dropped only after chi2/ci). **D3 interim** (decisions § *Phasing*): Phase 2 flips numeric `diff` to a real difference (field + display — conscious golden change), but numeric *coloring* keeps reading `ratio` (old behaviour, `mean_breaks`) until Phase 5 lands the mode split. Byte-verify via golden; benchmark before/after.

#### Done (2026-07-08 — numeric moment-sum core + numeric diff/ratio flip)

Sub-phased (numeric first, per the review). Landed so far:

- **Numeric moment-sum aggregate** (`R/tab-agg.R`, new): `tab_num()`'s three N-scans (main + total rows + total table) now compute **sufficient moment sums** (`n`, `wn`, `s1 = Σ[w]x`, `s2 = Σ[w]x²`, double-coerced to avoid integer overflow) instead of per-group `mean`/`stats::var`/`weighted.mean`/`weighted.var` closures; `num_derive_stats()` derives mean/var in one pass afterwards, reproducing the **unweighted sample (n-1)** vs **weighted ML (÷Σw)** split exactly (incl. the degenerate n≤1 / all-NA NaN→NA edges). `weighted.var()` deleted (its double scan is gone). Output byte-identical (golden within waldo tolerance; `_snaps/` unchanged). **8M bench: `tab_num` unweighted 1.09→0.53 s / 1752→864 MB; weighted 2.94→1.01 s / 7790→2169 MB.**
- **Numeric `diff` → real difference** (§3, D3): the numeric `diff` field is now `cell_mean − ref_mean`; the ratio moved to the new `ratio` field; the color layer repoints numeric `"diff"`/`diff_ci`/`after_ci`/`ci` to read `ratio` (byte-identical coloring against `mean_breaks`). pct columns untouched. Numeric `diff` **display** (the rare `display="diff"`/diff-interval-on-means) deferred with the mean diff_ci display to Phase 5 (no golden exercises it). Golden regenerated consciously (only the numeric `.rds`).
- **`tot_n` written (factor path)** + **`get_tot_wn()` accessor** (§2, §11): `tab_plain()` now stores each cell's OWN unweighted percentage base in the `tot_n` field (row / column / grand total per `pct`, `NA` for `pct="no"` counts and for mean cells), built from the unweighted `tabs_n` and broadcast (same denominators as the pct). Because `tab_plain()` runs per col_var, each col_var's `tot_n` is its own base (cross-col_var exactness is automatic). The weighted base is recovered on demand by `get_tot_wn()` = `wn/pct` (with a same-column total-cell fallback for empty cells; `$tot_wn` works). Built tables are now self-sufficient for their base — Phase 3 will retire `detect_totcols()` in `tab_ci`/`tab_chi2` in favour of these. Conscious golden regen: every factor pct `.rds` gains `tot_n` (only that field changed; `f_counts` unchanged; display `_snaps/` byte-identical).
- **Safety net grown**: golden fixtures `f_selfcross` (`_colvarbis`), `totn_row_drop`, `n_mean_w` (weighted ML variance), `n_mean_sparse` (n≤1/all-NA edge), plus `n_mean_color` display snapshot (D3→Phase 5 tripwire); `num_derive_stats` + `tot_n`/`tot_wn` unit tests (the former replacing the deleted `weighted.var` tests). Full suite green (774). Benchmarks in `dev/benchmarks/results_1.4.0/`.

- **Totals rollup** (`num_rollup()`, R/tab-agg.R): `tab_num()`'s total-row and total-table blocks no longer re-scan N — they **sum the additive moment-sum columns of a captured `main_agg`** by each grouping key (`group_vars` subsets for total rows; `row_var` for the total table), relabeling collapsed keys `"Total"`. Byte-identical (golden + a direct-microdata computation check; new `n_mean_tottab` fixture locks the total-table path). **This removed the 2 extra N-scans**: on 8M rows `tab_num` unweighted 0.70→**0.20 s** / 864→**288 MB**, weighted 1.05→**0.35 s** / 2169→**718 MB**. **Combined Phase 2 vs the pre-1.4.0 baseline: `tab_num` ~5.6×/8.3× faster and ~6×/11× less memory (unweighted/weighted).**

**Deferred out of Phase 2 (decided 2026-07-08):**
- **Factor-path reorg** (extracting `tab_plain`'s pct/diff/OR/fmt-assembly into shared `wide_*`/`fmt_assemble_factor` helpers) → **moved to Phase 4**. Unlike the numeric path, `tab_plain`'s factor math is already the single live, GForce-optimal path, so the reorg is purely output-invariant with no benefit until a second consumer exists — Phase 4's `as_tab_counts()` is that consumer and should drive/validate the extracted interface (avoids premature abstraction + a risky 800-line refactor now).
- **Numeric `diff` display** for the rare `display="diff"`/diff-interval-on-means → **Phase 5** (with the mean diff_ci display rework); the byte-identity-critical color repoint is already done.
- **Superseded lifecycle badges** on `tab_pct`/`tab_tot`/`tab_totaltab` → **Phase 6**, where `lifecycle` is added as a dependency (for the `tab_many` `deprecate_soft`), so no new dependency is pulled in early.

**Phase 2 is otherwise complete**: numeric aggregate-core (moment sums) + totals rollup + numeric `diff`/`ratio` flip + `tot_n`/`get_tot_wn`, all byte-identical, full suite green, `tab_num` ~5.6×/8.3× faster and ~6×/11× less memory (unweighted/weighted) on 8M rows.

#### To verify

- `tab_many()` : are there still error with levels = "auto", when `col_vars` are numeric ?

### Phase 3 — CI + chi2 onto the aggregate (headline perf)

Split into **3a (CI, DONE)** and **3b (chi2, remaining)**.

#### Phase 3a — CI onto the aggregate, 2026-07-08 (Done)

Proportion-CI vectorised onto a **closed-form engine** (`R/tab-agg.R`: `ci_pivot`/`ci_wilson`/`ci_newcombe`/`ci_prop_diff`/`ci_mean_diff2` + `newcombe_pvalue`) — the per-cell `DescTools` loop is gone (`DescTools` Imports→Suggests). `tab_ci()` (props) and `tab_num()` (means) both route through it. **Real asymmetric `ci_inf`/`ci_sup` bounds** (fixes the Wilson/AC symmetric-bracket bug); `format()` reads them directly; `get_ci()` = upper arm, `get_ci_moe()` = larger arm for `± moe`. **Significance = universal CI-inclusion** (the maintainer's refinement, **supersedes §12 score-test + §15 AC-swap** — see decisions §20): the stored per-cell `pvalue` is the inversion p of the *displayed* interval, so `get_stars()` never disagrees with the bracket, for any method. **Defaults: Wilson (cell), Newcombe method-10 (diff, new default), z/Welch-t (means)**; `stars` arg default `TRUE` (`ci="cell"`→NA); expert `method_cell`/`method_diff`. **Weighted = weighted estimate + unweighted n (§14)**; **Kish n_eff opt-in** for numeric CIs via `options("tabxplor.kish_neff")` (G1 `Σw²` accumulator added to the numeric scan only when opted in; factor-side Kish deferred). Empirical **OR deferred to the tab_logit phase** (not 3b). Golden regenerated: `f_ci_cell`/`f_ci_diff`/`f_color_afterci`/`n_mean_ci`. Full suite green (800); `tab_ci` no perf regression (`dev/benchmarks/results_1.4.0/phase3a_after.txt`). Empirical validation: `dev/verify_ci_inclusion.R`. **tab_xl stars deferred to Phase 7** (exporter unification).

#### 3b — table-level tests: Chi2/ANOVA on the vectorised engine, 2026-07-08 (Done)

**Vectorised test engine** (`R/tab-agg.R`: `agg_chi2()`, `agg_anova()`) — every (subtable × col_var) is one `table_id`; ALL tables are stacked into one long `data.table` and tested in ONE grouped pass (the framework for many tests of the same kind on different tables). Replaces `tab_chi2()`'s per-(sub)table `group_split()` + `stats::chisq.test()` loop. **Chi2 == `chisq.test()` exactly, incl. Yates on 2×2** (G2), fully unweighted; empty rows/cols dropped like the old path (df on the reduced matrix; degenerate → NA). **ANOVA = Welch's F (default) + classic F** for mean col_vars — `agg_anova()` from per-group `(n, weighted mean, weighted var)` (§14), matching `stats::oneway.test(var.equal=FALSE/TRUE)`; option `tabxplor.anova` (`"welch"`/`"classic"`) picks the displayed p, both stored. Numeric col_vars now get a whole-table test (previously skipped) — ANOVA computed on `tabs_num`, merged into the per-row_var attribute.

**`chi2` attribute → tidy `test` attribute** (§16): one row per (subtable × col_var × test-type), cols `[tab_vars…] row_var col_var test statistic df1 df2 pvalue n variance min_e`. Back-compat: `get_test()` reads it and **falls back to the old `chi2` attr**; `get_chi2()` kept as a working alias; the `chi2=` constructor arg soft-deprecated → maps to `test`; `new_test_tibble()` is the empty placeholder. **Contrib only when needed**: the per-cell `ctr`/`var` write (kept `var_contrib` machinery) runs **only when `color=="contrib"`** (`calc="p"` on the common path) → non-contrib factor tables' `var`/`ctr` become NA (conscious golden change; the contrib path stays byte-identical). **`add_n=TRUE` fixed**: the test drops reserved add_n/add_pct rows (`row_var` "n"/"row_pct") and `all_col_vars` columns.

Display: `tab_pvalue_lines()` bakes the p-value row from the tidy attribute (now for **means too**, F p-value); factor rows byte-identical (`_snaps` unchanged). `print_chi2()` rewritten to render the tidy attribute (chi2 + F) as a readable colored block. Golden regenerated (attr rename + var/ctr on non-contrib). **Suite green (950)**; parity locked in `test-calculations.R` (chi2 vs `chisq.test` incl. Yates; Welch/classic F vs `oneway.test`; add_n). **Perf: chi2 ~2.5× faster** (9-tab gss_cat 2.60→1.03 s; whole call 3.07→1.48 s — `dev/benchmarks/results_1.4.0/phase3b_chi2_anova.txt`); the tidy rewrite also fixed a pre-existing `tab_pvalue_lines` crash on overlapping row/col var names. Full record + ANOVA formulas: `dev/tabxplor_1.4.0_decisions.md` **§24** (§16, §14, §20).

#### 3b — deferred (not blocking)

- `tab_ci()` field-based simplification (item 3) — CI *math* already unified (3a engine); folding proportion-CI *placement* into the shared core is **Phase 4**, per §20.
- `tab_num(..., <tab_vars>, ci="cell")` grouping-set crash — FIXED Phase 6e (golden-locked; hardened 7d-i).
- "reuse chi2 intermediates for unweighted contrib" micro-opt — deferred; contrib is now off the common path entirely (the real cost), the rare pass stays byte-identical.
- Future: `!`-per-cell weak-test glyph (Q8/§16, pure display swap over `test`); φ² variance column populated in contrib mode; no-sd means option; `option(tabxplor.ci_print)` → argument.

### Phase 4 — From-the-middle counts constructor (DONE — 2026-07-08)

**`tab_counts()` (`R/tab-counts.R`, exported)** builds a full `tabxplor_tab` from already-aggregated counts, byte-identical to the `tab()` a user would build from the underlying microdata. Shapes (all validated against microdata parity in `test-counts-parity.R`): **long tidy counts** (`counts=`, + `wt_counts=` for the §14 weighted case), **wide data.frame** (`cols=`/`col_name=`), **`table`/`xtabs`/`matrix`** object (auto-melt via `as.data.frame.table`; a bare matrix is coerced with `as.table`), **frequencies + base N** (`input="pct"`, `base=`; counts rebuilt with **largest-remainder** rounding so each row sums to N). `tab()` stays microdata-only (no auto-detection — user's choice).

**Mechanism — reuse, no fork, no big extraction (maintainer's choice over the roadmap's factor-path reorg).** `tab_counts_reshape()` normalises any shape → canonical long tidy counts; `tab_counts_normalize()` aggregates to the keyed `.fine` shape `[tab_vars…, row_var, col_var, n, (wn)]` (drops `n==0` cells so the aggregate is structurally identical to microdata's `.N`-per-observed-key). It then routes through **`tab_plain()`'s existing `.fine` pre-aggregate entry** (the scan-fusion path, locked by `test-fuse-parity.R`) + the shared finalize (`tab_chi2` → `tab_ci` → `tab_add_n_pct` → rewrap → `tab_pvalue_lines`) — the *same* calls `tab_many()` makes. **This deprecated the planned 600-line `tab_plain` factor-path extraction** (`.fine` already provides the from-the-middle seam byte-identically; empirically proven before building). The only extraction done: `tab_add_n_pct()` (the `add_n`/`add_pct` block, moved verbatim out of `tab_many()` into a shared helper so both callers share one implementation).

**§14 weighting**: weighted input carries real unweighted `n` (`counts=`) + weighted `wn` (`wt_counts=`) → pct/estimates weighted, `tot_n`/CI/chi2 use the unweighted `n`. **Base-less input** (non-integer counts: frequency-only / weighted-only) → pct/diff/colors still render, CI/chi2 disabled with a `cli::cli_warn`. **freq+N with a real unweighted base** → CI exact (`(p,n)` direct), chi2 exact when frequencies precise (largest-remainder) — no warning (sharpens the roadmap's "frequency-only" rule: only *base-less* input disables inference).

**CI-placement fold (was also Phase 4): now moot / deferred to Phase 6.** `tab_counts()` reuses `tab_ci()` directly, so there is no third CI call site to fold; the CI *math* is already unified in `R/tab-agg.R`. The "one CI transform in the shared core" is a Phase 6 (`tab`/`tab_many` merge) concern, exactly as §20 splits it.

Two pre-existing latent `tab_plain()` warnings cleaned up in passing (output-invariant, golden green): guarded `tabs[, "wn"/"n" := NULL]` against a missing column.

### Phase 5 — Color diff/ratio split

> **READ FIRST: `dev/new_colors_UI.md`** — the SINGLE, self-contained implementation brief for this
> phase (why + final framework + full API + statistics + engine + computation matrices + phasing +
> open flags W1-W10). It governs the Phase 5 implementation and supersedes the bullets below (kept as
> historical intent). Layered decision history: companion `dev/design_new_colors_UI_decision_process.md`.
>
> Settled framework (2026-07-08): three orthogonal axes — **measure × channel × significance-policy**.
> `color` = which measure(s) (`diff`/`ratio`/`contrib`/`or`, auto-dispatched by column type) on which
> channel (scalar→text; `c("diff","ratio")`→text+background; named `c(text=,background=)`). `color=TRUE`
> = per-type default sugar. Separate **`color_signif`** arg = `"ignore"`/`"grey_non_signif"`/
> `"color_all_signif"` (old diff/diff_ci/after_ci; old `ci` = color_all_signif + single-0 break). Breaks
> = a named `list(pct_diff, pct_ratio, mean_diff, mean_ratio, contrib)`, **hybrid global + per-table
> `tab(color_breaks=)` override**; length = number of color steps; empty per-type scale drops that
> measure for that type; `mean_diff` NULL→standardized(SD), unit breaks→absolute. Palette: global
> render-time, measure-group diverging ramps (additive vs multiplicative), text+background,
> colorblind-safe. Engine rewritten around `findInterval` (kills the `keep_last_break` bottleneck);
> significance = `ci_inf>0`/`ci_sup<0` from Phase 3a bounds; the two channels live in the EXISTING
> `color` per-column attribute WIDENED to length ≤ 2 (brief §9.1 — NOT a new `color_bg` attribute).
> col%+means reference fix DEFERRED to Phase 7 — Phase 5 only warns.

#### Done so far (Steps 0-4a, 2026-07-09) — Batch A "core"

- **Step 0 — color safety net.** `test-color-golden.R` + `dev/make_color_golden.R` +
  `helper-color-golden.R` capture per-cell hex (`fmt_get_color_code`, the signal every exporter +
  console share) across {measure × factor/mean × text/bg × theme × 24-bit}, incl. a synthetic
  factor-`diff` column sitting EXACTLY on every break + the x2 (the tie lock for the fold+findInterval
  byte-identity). `test-color-config.R` / `test-color-engine.R` added.
- **Step 1 — breaks list model.** `set_color_breaks(list(pct_diff, pct_ratio, mean_diff, mean_ratio,
  contrib))` (canonical scales `list(pos, center, strict, std)` in `options("tabxplor.color_breaks")`);
  `mk_color_scale()` validators; old `pct_breaks/mean_breaks/contrib_breaks` args soft-deprecated
  (`lifecycle`, now an Import) → mapped onto the scales; `.onLoad` reseeded. `legacy_color_breaks()`
  derives the old flat vectors for the pre-Phase-5 selection path (byte-identical) until Step 6.
- **Step 2 — palette/slots.** `set_color_style(custom_palette=)` length bug fixed (accepts 11).
  `color_slot_table(L, channel)` / `build_slots(K, channel)` replace `select_in_color_style`'s
  hex-sniff with an explicit channel arg (fixes the `bg_dark` `#000033e` typo); byte-identical to
  the old lookup for the text family.
- **Step 3 — findInterval engine.** `fmt_color_plan()` / `fmt_color_slots()` / `fmt_color_channels()`
  (in `R/fmt_class.R`) fold each measure's score to a magnitude that grows away from its neutral
  center, `findInterval(mag, pos_breaks, left.open=strict)`, split by direction into palette slots
  (0=uncolored); the legacy in-text x2 is a slot-11 override. Significance from the Phase-3a
  `ci_inf`/`ci_sup` bounds. `get_ref_var()` added for Glass's Δ. `pillar_shaft` + `fmt_get_color_code`
  rerouted (the console + golden). **Factor `diff` byte-identical (text)**; numeric `diff` now Glass's
  Δ (`mean_diff` breaks); pct CI-gated modes fixed (asymmetric-interval upper-arm bug + the `ci`-mode
  crash); a contrib 0/0 p-value-row miscolor fixed. **48–1290× faster** than `keep_last_break`
  (`dev/benchmarks/results_1.4.0/phase5_engine_micro.csv`). Old `fmt_color_selection`/`keep_last_break`/
  `select_in_color_style` kept ONLY for the exporters + `expect_color()` until Steps 5/6.
- **Step 4a — ratio field repoint (§3).** pct/factor `ratio` field now holds the reference-relative
  RR `cell_pct/ref_pct` (the x2 driver, off the `mean` overload); `mean` keeps the value during the
  transition (Step 6 → NA for pct). Coloring + display byte-identical; structural golden regenerated
  (the `ratio` field). suite green (1053).
- **Step 4b-c — two-channel storage + `color_signif` attribute.** The `color` per-column attribute
  is WIDENED to length ≤ 2 (text, background): `fmt_color_attr()` = full vector, `get_color()` = `[1]`
  (unchanged scalar contract), new `get_color_bg()` = `[2]` (NA when absent), `set_color()` +
  `resolve_color_channels()` parse scalar / `c(text,bg)` / named `c(text=,background=)` and reject
  `contrib`/`or` on background. The **vctrs reconcilers read the FULL attribute** (`vec_ptype2` +
  all `vec_cast`/arith), so the bg channel is not dropped on `c()`/cast/group. New per-column
  attribute **`color_signif`** ("ignore"/"grey_non_signif"/"color_all_signif") — 8→9 attrs, a
  conscious `test-fmt-contract.R` + goldens regen (default reproduces today's behaviour).
- **Step 4d — `tab()` arg parsing.** `normalize_color_spec()` + `finalize_color_spec()`
  (`R/tab.R`): `color` accepts `FALSE` / `TRUE` (per-type: factor→diff text + ratio bg, numeric→ratio,
  counts→contrib, OR→or) / a scalar / `c(text,bg)` / named; separate `color_signif` arg. It runs the
  existing pipeline on a text-channel "legacy" string (so ci/chi2 side effects still fire) then sets
  the final two-channel + policy attributes. **Old scalar strings pass through untouched** (engine
  decodes them → no golden churn; the deprecation *warning* is deferred to Step 6). Parsing lives in
  `tab()` only (the future merged base); `tab_num()`'s new-arg parsing is deferred to Step 6/Phase 6.
- **Step 4e — background rendering.** `pillar_shaft` stacks the text-channel crayon + the bg-channel
  crayon (bg palette); `fmt_color_channels()` returns both slot vectors. Verified end-to-end.
- **Batch A COMPLETE** (1068 tests green): findInterval engine + config + significance + ratio repoint
  + two-channel storage/args/rendering, factor-`diff` byte-identical, 48-1290× faster.

#### Done (Batch B — Steps 5-6, 2026-07-09)

Phase 5 is **COMPLETE**. Full suite green (**1085 tests, 0 failures**).

- **Step 5 — exporters + legend on the two-channel engine.** New shared `fmt_channel_codes()` helper
  (`R/fmt_class.R`, next to `fmt_color_channels`) = the single slot→hex mapping (text hex in the
  `color_type` palette, bg hex in the `"bg"` palette; NA where uncolored). `tab_kable()` /
  `tab_plot()` / `tab_xl()` rewritten onto it: text channel → font colour (`cell_spec(color=)` /
  `table_cell_font` / a `fontColour` style set), bg channel → fill (`background=` / `table_cell_bg` /
  an `fgFill` style set, stacked via `addStyle(stack=TRUE)`). The old string-splitting hacks are gone.
  `tab_color_legend()` fully **rewritten** to be driven by the SAME per-channel `fmt_color_plan` +
  canonical scales the cells use (so legend ⇔ cells can't disagree) — this fixes the numeric-`diff`
  legend (now shows the SD/Glass-Δ thresholds, not `×ratio`) and describes both channels + the
  significance policy. `brk_from_color`/`get_color_type` deleted.
- **Step 6 — deletions, cleanup, wiring, docs.** Deleted the old engine
  (`fmt_color_selection`/`keep_last_break`/`color_formula`/`select_in_color_style` + dead `*_brksup`).
  `mean = NA` for pct columns (the mean-overload is gone; `ratio` carries the RR — conscious golden
  regen, only the `mean` field changed). `get_color_breaks()` now returns the **canonical
  positive-scale list** (round-trips with `set_color_breaks`; `type="all"` mirrors); `legacy_color_breaks()`
  deleted. Old strings `"diff_ci"/"after_ci"/"ci"` **soft-deprecated** in `normalize_color_spec()`
  (`lifecycle::deprecate_soft` with `user_env` = the real caller; they still render byte-identically —
  warn-only, NOT decoded, to avoid attribute/golden churn). **`tab_num()` wired** to the new
  `color`/`color_signif` args (via `normalize_color_spec`/`finalize_color_spec`; no-op for plain
  strings, so `tab_many→tab_num` is unaffected). Tests: `expect_color()` → `fmt_color_channels`; the
  `select_in_color_style` oracle test dropped; `test-color-config.R` gained deprecation + two-channel
  render tests. Docs: `@param color`/`color_signif` on `tab()`/`tab_num()`, `/color-mode` skill +
  `dev/tabxplor_architecture.md` Color System rewritten, NEWS + Deprecations.
- **`tab_many()` new-arg wiring deferred to Phase 6** (per-row_var `color` axis collides with the
  two-channel spec; Phase 6 globalises the axis + consolidates parsing). Direct `tab_many()` callers
  keep old scalar strings (functional via the engine decode).

**Batch B deferred / flagged (see also W-flags in `dev/new_colors_UI.md`):**
- **W4 — measure-group palette hues** NOT built: diff/ratio distinguished by channel (text vs bg),
  not hue. The `color=TRUE` factor default (diff text + ×2 ratio bg) looks muted vs the eventual
  orange↔purple ratio ramp. Own perceptual-design + colorblind pass.
- **`color_type="bg"` global toggle is now vestigial**: it selects the TEXT channel's palette family
  only; the channel (not `color_type`) decides font-vs-fill. Consider deprecating it. Degenerate for
  `color_type="bg"` + two channels (both want the background) — flagged, default `"text"` unaffected.
- **W5 — coloured `tab_md`** (pandoc spans) still out of scope; `tab_md` stays monochrome.
- Numeric `color="diff"` = Glass's Δ (does NOT auto-remap to `"ratio"`).

Now the `ratio` field exists (Phase 1): implement `"diff"`/`"ratio"`/`"diff_ratio"` modes + legend text, **keeping the existing modes coherent in the same overhaul** (`diff_ci`, `ci`, `after_ci`, `contrib`, `OR` — do not drop the `ci` mode). **Numeric `"diff"` mode is sd-standardized (Q9, §18)**: color Glass's Δ = `diff/sd_ref` against new effect-size `mean_diff_breaks` (default `c(0.2, 0.5, 0.8, 1.2)`); derived from `diff` + the reference `var` at color time — no new field, `$diff` stays raw. Skill: `/color-mode`. Also fix the pre-existing **col% + means** row/col reference mismatch (means referenced by row, factors by column — `dev/tabxplor_1.4.0_decisions.md` §7).

#### To verify

- with mean, is diff_ci/after_ci formula wrong (in color calculation ok ? In printing wrong ?)

- verify seriously that pct ×2 rule calculations for "after_ci" are good

#### To implement

2. Some careful modifications of the color helpers. The core will be to differenciate differences (`diff`) and ratios (`ratio`) for both : factors should keep the same behaviour than currently with `color = "diff"` ; but numeric variables with `color = "diff"` color differences, and return to the former behaviour with `color = "ratio"`. Maybe adding a `color = "diff_ratio"` possibility to use both, one using text color and the other background color (if will select background colors to ensure readability and ease of understanding when both are used for the same number) ? Question is : how to do a complete overall, integration and simplification of the current colors functions ecosystem to make it word and increase it’s user-friendliness ?
- Where to store the values ? **Resolved (Q3):** in the renamed `ratio` field (was the unused `rr`) — for pct it IS the relative risk (`cell_pct/ref_pct`, the step toward empirical OR); for numerics it is `cell_mean/ref_mean`. `diff` stays a pure difference; `mean` holds only actual means. See §3, §12.

#### To think about

- with each color argument, use a different color palette. Too complicated, or worthwhile for clarity to the user ?

### Phase 6 — tab() → tab_many() merge and full refactor

#### Done (2026-07-09)

Sub-phases 6a–6i, each golden byte-identical (conscious NEW fixtures only; no committed fixture changed). Full suite green.

- **6a** `tab_apply_tests()` — shared chi2→capture-`test`→ci block (used by `tab_build` + `tab_counts`); `tab_many`'s two-batch chi2/ci passes became one per-table pass.
- **6b** internal engine **`tab_build(..., output)`** (no option reads); `tab()`/`tab_many()` are thin wrappers. **`output_list`** on `tab()`; **`tabxplor.compact` option dropped**; `tab_compact()` = internal merge for `output="single"`. §13 shapes honoured; `tab_many()` keeps its list-default. `tab()` gained plural `row_var`/`col_var` (tidyselect). `tab_prepare()` still runs ONCE on the whole DB (prep→aggregate→transform→assemble seam for Phase 10 Jamovi).
- **6c** globalise row axis (`tab()` asserts `OR/ci/chi2` scalar); ONE color parse (`normalize_color_spec`/`finalize_color_spec` in `tab()` + the `tab_many` wrapper — Phase-5 leftover closed). `tab_build` still recycles internally (harmless broadcast; D2 gradual collapse).
- **6d** named per-row_var `ref` vector (`resolve_ref_vector()`; collapses under col% + message).
- **6e** fixed the `tab_num(<tab_vars>, ci="cell")` KNOWN-BUG (`dplyr::last(group_vars)` → `group_vars[length(...)]`); `totrow`/`totcol` soft-deprecated (cosmetic); `tab()` default = one total column (`totcol="last"`).
- **6f** singular `row_var`/`col_var` → soft-deprecated aliases (bare args + `quo_is_missing`, NOT `is_present` which force-evaluates tidy-select); `tab_many()` soft-deprecated (silent for jmvtab); `tab_pct`/`tab_tot`/`tab_totaltab` badged superseded.
- **6g** `na = "common_base"` = global drop `{row_vars, FIRST col_var, tab_vars}` + effective `na="keep"`; equals `na="drop"` for one col_var (S3 acceptance); microdata-only (`tab_counts()` rejects).
- **6h** `tab_ci()` base recovery reads stored `get_tot_n()` (exact per-col_var) instead of `detect_totcols()`; `detect_totcols` retained for STRUCTURAL roles (total-col ID, chi2 margins, `tab_add_n_pct`, superseded `tab_pct`).
- **6i** `tab_spread()` kept active: new `spread_vars`/`names_prefix`/`names_sort` on `tab()`.

**Caveats remaining**:
- internal per-row_var recycling kept (arg surface globalised);
- `detect_totcols` not fully retired (structural);
- ≥2 row_vars + `tab_vars` still returns a list (§7);
- `tab_num()` keeps its own color parse; U4 satisfied via scalar-pct regime.

#### Original plan (historical intent)

Merge between `tab()` and `tab_many()`. Soft deprecate the `tab_many` alias to directly use `tab` alias from now on.

`tab_many()` code becomes the base; `tab()` the new alias for it. Add `output_list` (default `FALSE`), deprecate the `compact` argument, remove the `tabxplor.compact` option, keep multi-table when `tab_vars` present (compact-with-tab_vars deferred to Phase 7). `lifecycle::deprecate_soft("1.4.0", "tab_many()")`. **`tab_many()` soft deprecated function keeps its list-default** for ≥2 row_vars (Q7, §13) — only `tab()` merges by default; no silent return-type break. **Also here (§4-6):** globalise the row_var axis (`OR/pct/color/comp/ci/chi2`, `ref2` no longer per-row_var — but note **D2**: the *internal* collapse lands with the Phase 2 core, only the *arg-surface* deprecation lands here; keep per-row_var `totaltab` + `ref` as a named vector, row%/means only); keep col_var axis flexible (`pct/levels/digits` per col_var); soft-deprecate `totrow` (always a total row) and soft-deprecate `totcol` (default one total column; old values kept, cosmetic-only); soft-deprecate singular `row_var`/`col_var` arguments. **Decide `tab_spread`/`tab_compact` fate** (open item **S4**). Detail: `dev/tabxplor_1.4.0_decisions.md`.

- **Phase 5 leftover — wire the new `color`/`color_signif` forms into `tab_many()`** (done for `tab()`/`tab_num()` in Phase 5 via `normalize_color_spec`/`finalize_color_spec`). Deferred here because the new two-channel `color = c("diff","ratio")` collides with `tab_many()`'s per-row_var `color` vector (`tab.R` ~L841); the color-axis globalisation above resolves it. If a legacy per-row_var path is ever kept, the clean discriminator is that `"ratio"` was never a valid old color value. Move the parsing into the merged base so all three entry points share ONE parse site.

The original rationale for separating the two was : `tab_plain` is the core worker but lacks many advanced option ; `tab_many` is the most flexible for big tables, with many options ; `tab` was centered around the necessity to keep the whole population (who is in `n` ?) and NA handling consistent with having a single row variable and a single column variable. Since most of the time (with row percentages), only one total column was kept, the `n` count could be different for every col var : it won’t be the case anymore if the `tot_n` base total (§2 — renamed from `ref_n`) is stored in a vctrs field for each cell.
- In the new `tab()` function, I would want **an argument to get the same behaviour as the old tab `tab()`**. What would it be ? Would something like `na = "base_table"` (find a better name, more user-friendly and easily understandable) work : removing, for all col_vars, the missing value of the the row_var and the first col_vars (with several row vars : each by-row_vars subtable remove the individuals with missing value either in the carrent row variables or in the first column variable) ?



### Phase 7 — Jamovi jmvtab module total overhaul

The current jmvtab Jamovi module never embrassed the internal logic of Jamovi : it was just a R function with a choose all arguments first then run, where Jamovi is a live interactive statistical application where each button change rerun the analysis. Instead of simply wiring the whole `tab()` (or even `tab_plain()`) function into Jamovi, I want to use their internal steps, shared functions and aggregate core to **write an efficient, cached and modular version of the whole table construction pipeline, that would fully embrace Jamovi’s states and caching framework**.
- Input changes should work live for the user with **near instant results display** on normal sized survey df : **if a big amount of refactor** of the aggregate core, tab_plain internals and shared functions, etc., **are needed, this is the path I want to take** (without reducing the efficiency of the current `tab()` function on big tasks with at lot of tables and variables at the same time).

Use Jamovi states logic to avoid redoing calculations on each button change, with temp caching for base calculations (e.g. keep former variables' calculations when a new variable is added). **The standard basic usage of tab() and jmvtab(), for non-advanced users, is color-driven** : user choose variables, percentages, then color arguments, and depending on the color and color_signif all the needed calculations are computed. The expert user can tweak it and have access to more advanced options (like confidence level, type of confidence interval, etc.).

**No back-compatibility needed at all on jmvtab and jamovi UI** : the aim is no create a fully new user-friendly fast UI.

Look carefully at `dev/tabxplor_1.4.0_jamovi_dev.md` for detailed insights about jamovi module development.

#### Phase 7a — Wire new colors UI and new tab() version into jmvtab for baseline

The new color helpers UI in `dev/new_colors_UI.md`, already implemented, rely on a reworked `color` argument and a new `color_signif` argument. I first want to wire it, and the whole rewritten `tab()` function, into the current jmvtab UI, to establish a baseline before the full rewrite.

##### Done (2026-07-09)

Prerequisite fix — **`tab()` completed as the true `tab_many()` replacement**: added the **`levels`** argument (`"all"`/`"first"`/`"auto"`, per col_var — it was dropped from `tab()` by a Phase 6 oversight, contra decisions §6 "col_var axis keeps `pct`/`levels`/`digits`"); added **`na = "drop_all"`** (drop obs missing on row_var(s) / any col_var / tab_var — resolved natively by `tab_build`); **fixed the latent `na = "drop"` bug** (it globally dropped like `drop_all`, contradicting its docs — now per-col_var, distinct bases; no golden churn — golden multi-col_var `drop` cases already drove `tab_build` directly, and all `tab()`+`drop` tests are single-col_var); **soft-deprecated `sup_cols`** (fold into `col_vars` + `levels = "first"`). `stars`/`method_cell`/`method_diff` were already on `tab()`. `tab_build` stays the internal DRY engine; both `tab()` and `tab_many()` wrap it (not a jmvtab hook). Suite green (1101).

jmvtab baseline wired to **`tab()`** (not `tab_many(compact=TRUE)`): `.a.yaml`/`.u.yaml` replace the old `color` list with **`color`** (no/auto/diff/ratio/contrib/OR) + a new **`color_signif`** list (ignore/grey_non_signif/color_all_signif); `na` gains drop_all/common_base; `lvs`→`levels`; expert **`stars`/`method_cell`/`method_diff`** added in the collapsed CI box. `.b.R` fully rewritten (dead code stripped): maps `color` "no"→`FALSE` / "auto"→`TRUE` / else the measure string, forces `ci="diff"` when a `color_signif` policy is set with `ci="auto"`, and keeps the historical Excel export (redesign is 7f). `.js` stripped to the one live handler. Backend `tab()` wiring validated on 12 option combos (colors, levels, na, contrib/OR, methods).

✅ **CLOSED 2026-07-16 (migration C3)**: `jmvtab.h.R` regenerated + module built + installed on the WSL flatpak jamovi 2.7.36. See *Jamovi module development*.

#### Phase 7b — Draw a detailed map of required computations and interdependence between arguments

Before designing implementation, I want you to create a **reliable and up-to-date map** of the interdependence between arguments and the required computations to make each option work, then improve it and implement the improvement in `tab()`. Write down this map in a new very structured and very detailed .md file in `dev/`.
- In `tab()`, **carefully review the arguments overwrite logic** at the start : study it carefully, try to understand and write down the reason in comments, ask yourself if it’s sound and justified, and ask yourself if it must be kept or discarded.
- Read `dev/new_colors_UI.md` for a detailed description of the required computations for each of the new `color` and `color_signif` modes, confirming the actual code does it. Then, ask yourself how `tab()` code and functions used internally should be modified to really achieve it.
- After maintainer’s confirmation, rewrite and improve `tab()` arguments overwrite for the more consistent and user-friendly result possible : we’ll then use the same logic in jamovi UI in .js (or near the same, since live button changes with cache and .js implementation may need a specific way to handle it).


##### Done (2026-07-09)

- **The map**: `dev/tabxplor_argument_computation_map.md` (NEW) — argument catalogue (tab()+jmvtab, with axes G/RV/CV/DISP/AGG), the three computation layers, the arg→field dependency chain, the audited colour/`color_signif` matrix, and a **pure-display vs per-transform vs aggregate recompute** classification that seeds the Phase 7c cache. Governs 7c→7e.
- **Cascade consolidation (byte-identical, full suite green 1101/0)**: the argument-overwrite cascade — scattered across `tab_build`/`tab_plain`/`tab_num`/`tab_counts` with the `ci="diff"` forcing duplicated **4×** and `color="auto"` resolved twice — is now ONE pure resolver **`tab_resolve_settings()`** (`R/tab-resolve.R`), shared by `tab_build()` and `tab_counts()`; the numeric `color="auto"` arm is `resolve_color_auto_num()` (called by `tab_num()`). It is a data-free function of (args, column classes) → the boundary the jmvtab `.js` mirrors + the 7c cache keys on. **Data-dependent resolution stays at the leaf** (`ref="auto"`/regex, `levels="auto"`, `na`-drop, leaf tot/totaltab) — marked `# LEAF resolution (Phase 7b)`.
- **Judgment**: all cascade rules are sound → kept + consolidated, none discarded. **Inconsistencies found**: `ref="auto"` differs by column type (factor `first`-under-OR vs numeric always-`tot`) — INTENTIONAL, stays per-leaf (a mixed table needs both; non-observable today since `tab_num` has no OR). `tab_counts()` lacks `contrib→totrow` (tab_build has it) — preserved as-is, flagged.
- **Audit vs `new_colors_UI.md` §12**: code is AHEAD of the doc — `get_ref_var()` already exists; the pct `ratio` field is already repointed (`mean=NA` for pct). Both stale lines fixed in `new_colors_UI.md`.
- **col%+means reference asymmetry**: confirmed INTENDED (a mean's reference is a row, a factor's under `pct="col"` a column; no clean fix without white-elephant UI). Documented (map §8), warn-only, unchanged.

#### Phase 7c — Design a hierarchical multi-cache system for jmvtab
dev\tabxplor_argument_computation_map.md
The cache system should be carefully designed in a tree or hierarchical logic, with different steps and smart levels of caching, to only redo what’s really necessary at each step in a consistent systemic way. Taking all the arguments of `tab()` and `jmvtab()`, **I want you to design such a multi levels cache system for jmvtab, and write your result in a new file in `dev/`.**
- Do not rely on current implementation, do not try to stick too much to the current UI or code : first ask yourself, what would be the best solution. If big code refactors are needed in the aggregate code or anymore, we’ll do it.
- If cached objects are really big (no often the case with tables that are already summary ?), it may be a bad idea to keep them all in memory : only if it’s necessary, we can think about saving uncompressed .rds as temp files.

Exemples of the wanted behaviours :
- If the user just adds a new row or col variable, the former ones are not recalculated but cached and reused. Since counts and weighted counts are the bottleneck computation, a count that have already been calculated in the session shall stay cached a long time, and already be here if the user revert back to the same configuration afterwards.
- Changing the type of percentages do not require to recalculate the counts.
- If the user just **change the reference level**, for example with `pct = "row"`, if the user goes from `ref = "tot"` (total row) to `ref = 1` (first row), with `color = "diff"` only `diff` need to be recalculated, plus `ci` with `color_signif` not left at `"ignore"`. **I want this one to be particularly fast** :  user should really be able to change the reference level live and have the result instantly in feeling.
- If the user change the display, no fields need to be recalculated.
- (With new features below, if the user reorder the levels of a variable, it’s just a basic fct_relevel + arrange on the reordorer factors few millisecs operation.)
- Missing values are one of the most difficult thing to handle well in this caching system : we must think about it thoroughly and design a smart and balanced solution. For example, should we always keep missing values at the counts step, to then be able to remove them from counts, and just recalculate `pct` and everything after ?
- Please flag other arguments that would also be difficult to handle than missing values, since they rely on counts.
- The factor x factor and factor x numeric roads may be very different : some operations with means may need to recalculate the sd, etc.
- Etc. : please think carefully about all other arguments and all other situations that may arise, and what would be the most user-friendly way to handle them in each case.

##### Done (2026-07-09)

Design settled and written to **`dev/tabxplor_jmvtab_cache_design.md`** (the deliverable). A
**content-addressed 5-tier cache** (jamovi has no "which option changed" signal; `.run()` always re-runs
full), hosted in the **native result-element `$state`** (gzip-RDS to disk, survives the engine reset —
NOT hand-rolled temp `.rds`). **Persist only tiers 1-2** (per-pair count/moment aggregates as a
byte-bounded LRU of atomic-vector lists, not live `data.table`s; + chi2/ANOVA keyed on a hash of the
*shaped* aggregate + `comp`); **recompute tiers 3-4** (pct/diff/ratio/or/CI → fmt → colour → kable) every
run because fmt is **O(cells), not O(N)**. Aggregate built at **raw levels + NA kept**, so `na`(keep/drop)
- `levels` are cheap post-aggregate collapses; `cleannames` is **display-tier** (jmvtab carries full names
through, cleans only before `tab_kable()` — cosmetic, not a cache key; documented no-collision-summing
divergence from `tab()`); `other_if_less_than`/`wt`/`filter`/`na`(drop_all/common_base) stay
aggregate-invalidating (the drop_all/common_base population modes can't do per-pair reuse — documented
limitation). Per-pair entries stored at finest tab-grain and rolled up (dropping a tab_var = rollup).
Maintainer-confirmed choices (this session): aggregate+tests-only scope; per-pair bounded LRU;
na+levels+cleannames demotion. The doc closes with the **Phase 7d seams** it requires
(`tab_aggregate → tab_transform → tab_assemble`, `tab_num` split, cleannames→display, extend
`tab_resolve_settings()` to emit tier keys) and the byte-identity parity tests 7d must add. No product code
changed in 7c.


#### Phase 7d — Improve or redesign compute functions and table building workflows to work both with `tab()` and the new jamovi multi-level cache system

Study if the multi-level cache system designed in Phase 7c calls for a modification of the compute functions, of the table building workflow, etc. then implement these changes : it can be small improvements and adaptations but, if needed, I may be total refactors and architectural changes.
- Use `dev/benchmarks` or create new ones if needed. It should not reduce the performance of `tab()` with many variables in a significative way (one of `tab()` main use is a "create many exploratory tables with a tons of variables at once and export them for manual review with color helpers" workflow).

**Sub-phased (plan approved 2026-07-09): 7d-i numeric aggregate seam, then 7d-ii the three-stage carve.**

##### Done (7d-i — 2026-07-10): numeric aggregate-injection seam

The numeric leaf gained the same aggregate-injection seam factors already have via `tab_plain(.fine=)`.
The single O(N) moment-sum scan is now a shared `num_moment_scan()` (`R/tab-agg.R`, the aggregate MATH,
kept once — no fork) called by BOTH `tab_num()`'s raw path and the new **`tab_aggregate_num()`**
(`R/tab-agg.R`, the tier-1 producer: prepped microdata → finest-grain moment aggregate keyed by
`c(tab_vars, row_var)`, NA kept, raw levels). `tab_num()` gained `.fine`/`.by_table` (mirrors
`tab_plain`): `use_raw <- .by_table || is.null(.fine) || df || num`; when `.fine` is supplied it
`data.table::copy()`s it and skips the scan, everything from `num_derive_stats()` down unchanged.
`tab_build()` builds `fine_num` **per row_var** (never fused across row_vars — H1) and passes it to
`tab_num(.fine=)`; `.by_table = TRUE` forces the raw path. **Perf-neutral** (the scan is relocated, not
doubled — default==`.by_table` at ratio 1.00 on 515k rows,
`dev/benchmarks/results_1.4.0/phase7d_i_numeric_seam.txt`). **Byte-identical**: full suite green
(1116 pass, 0 fail), NO golden regeneration; new `test-num-fuse-parity.R` locks adopt-fine == inline
scan across unweighted/weighted, na keep/drop, comp all, ci cell/diff, mixed factor+numeric, several
row_vars, Kish `_w2` round-trip, and `.fine`-not-mutated. The `tab_num(<tab_vars>, ci="cell")`
KNOWN-BUG (already fixed 6e, golden-locked) was preserved + hardened (`intersect(tab_vars,
names(tabs_tot))` guard) + `expect_no_error` regression; stale markers de-staled.

##### Done (7d-ii — 2026-07-10): the three-stage carve

`tab_build()` is now the **five-stage pipeline** `ctx |> tab_setup |> tab_prepare_pop |> tab_aggregate
|> tab_transform |> tab_assemble` (each individually callable, threading a `ctx` list). **Orchestration
carve** (maintainer's choice): the leaves `tab_plain()`/`tab_num()` are UNCHANGED — the existing `.fine=`
seam already gives the O(N)→O(cells) split, and tier-3 recomputes wholesale (cache-design §3.4), so no
split of the two ~900-line leaves. `tab_setup` produces tier-0/1/2 cache keys via `tab_resolve_settings()$cache_keys`
(new `tab_cache_keys()` helper — symbolic/data-free; 7e adds the data hashes). `ctx` renames the locals
tab_build threaded, with one clarity win: the overloaded `chi2` split into `chi2_flags` (logical) +
`tests` (test tibbles). `ctx_update()` repacks NULL-safely (single-bracket `[<-`). `cleannames`/`other_if_less_than`
extracted to `tab_lump_others()`/`tab_cleannames_relabel()` (public `tab_prepare()` still composes them,
byte-identical; jmvtab defers cleannames to display in 7e). **`tab_counts()` re-expressed on the SAME
stages**: single-pair ctx → `tab_setup()` (incl. tab()'s `tot`→totrow/totcol translation) → `tab_transform`
→ `tab_assemble`, injecting its counts as the fused tier-1 — deleted the hand-inlined finalize; now
byte-identical to `tab()` for EVERY `tot` (contrib now forces a total row like tab() — a documented
convergence). **Byte-identical, perf-neutral** (48.3 vs 47.95 ms/call,
`dev/benchmarks/results_1.4.0/phase7d_ii_carve.txt`): full suite green (1150 pass, 0 fail), NO golden
regeneration. New `test-carve-parity.R` (stage composition == tab_build + the 7e seam contract) +
`test-cache-keys.R` (the `$cache_keys` shape) + non-default `tot` cases in `test-counts-parity.R`.
Pre-existing (NOT a carve regression): multi-row_var × multi-col_var + scalar `pct` errors "pct can't be
recycled" identically on the pre-carve code.


#### Phase 7e — Jamovi module full internal code rewrite with designed caching

Totally rewrite `jmvtab()` jamovi module code to implement the multi-level cache system designed in Phase 7c, using the modified functions implemented in Phase 7d if it was done. Use all the documentation create above carefully to design the most performant, reliable and user-friendly jamovi UI for live use.

The main improvement would be not to rely on `tab()` like now, but to drive the **same aggregate-core + per-transform subfunctions** (Phase 2) at cache-appropriate granularity — **reuse the core, never fork the math**: near-identical behaviour is *guaranteed* by sharing subfunctions, not re-implemented in parallel (which would recreate the very duplication 1.4.0 removes). Cache the prepared data / aggregate / per-transform results keyed by which input changed; pure-display toggles reuse cached numbers; reuse the `.fine` aggregate across interactions.


##### Done (2026-07-10)

New **`R/jmvtab-cache.R`**: the content-addressed multi-tier live cache. The module **reuses `tab()` end to end** (its color spec, `na` translation, totals, recycling) with the cache injected through a mutable `cache_env` — two new internal args `.cache` / `.defer_level_merge` on `tab()`/`tab_build()`; `tab_aggregate()`'s one-line hook delegates to `jmv_cache_aggregate()` (cache-injected tier-1 build + tier-2 keys), `tab_build()` calls `jmv_cache_store_tests()` after transform. **No math fork** — `jmv_cache_aggregate()` is byte-identical to `tab(cleannames = FALSE)`. Enablers: `tab_transform()` generalised so `.fine` is a per-pair named list (`fine_for_pair()`, dispatches on `is.data.table` → batch path unchanged) + a `cached_test` hook on `tab_apply_tests()` (`set_test()` added); `tab_prepare_pop()` `defer_level_merge` (full levels for a cacheable aggregate + test; the level-drop moves to `tab_assemble`). Store: tiers 1 (per-pair counts / per-row_var moment sums) + 2 (chi2/ANOVA) only — fmt is O(cells), recomputed; atomic-vector lists (never a live `data.table`), schema-versioned, per-entry byte ceiling + byte-bounded LRU; hosted on a hidden 0-size **Image** result element's `$state` (only Images persist `$state`). Data identity = **per-column** fingerprint (adding a variable reuses other pairs; opt-in `options(tabxplor.jmv_full_hash=)`). `jmvtab.b.R` is now a thin orchestrator over the engine-free `jmvtab_build()`. **Fixed the pre-existing `pct`-recycling BLOCKER** (multi×multi tables). Two documented divergences from `tab()`: cleannames-at-display (colliding levels stay separate) + `levels="first"` tests full levels. Locked by `test-jmvtab-cache.R` (41 tests); full suite green (1191). **Refinements vs the design doc**: tier-2 key-of-keys; contrib never uses the test cache; exact-grain keying (grain rollup + per-measure numeric caching deferred). Detail: `dev/tabxplor_jmvtab_cache_design.md` §8 STATUS.

✅ **CLOSED 2026-07-16 (migration C3)**: regenerated + built + installed; the hidden `cache_state` Image is in the compiled module.


#### Phase 7f — Optimizing the O(cells) fmt build

**Why (grounded, Phase 7e profiling — `dev/tabxplor_1.4.0_decisions.md` §27).** The Phase 7c cache persists tiers 1-2 (counts/moment scans + chi2/ANOVA) on the premise that everything below — the tier-3/4 **`fmt`-record assembly** (`pct`/`diff`/CI + `vctrs::new_rcrd` cells + colour) — is O(cells) and "too cheap to cache". The committed jmvtab benchmarks (`benchmark_jmvtab_ops()` small, `benchmark_jmvtab_big_ops()` big; baselines `tests/testthat/jmvtab_benchmark_baseline.csv` + `jmvtab_big_benchmark_baseline.csv`) disprove that **at real-world scale**:

- Small table (1 row_var × 3 col_vars): warm build ~0.23 s, render ~0.28 s → **render dominates**.
- Big table-of-tables (3 row_vars × 3 col_vars ≈ 9 pair-tables): warm build **~0.95–1.15 s**, render ~0.60 s → **the fmt BUILD dominates** (~1.5 s R, ~2 s in the Jamovi UI). The bottleneck flips with size.
- A pure-display toggle (`digits`) still costs ~0.94 s on the big table, because `jmvtab_build` re-runs the whole tier-3/4 pipeline — nothing below the aggregate is cached. Tier-1/2 caching alone can't make big tables instant.

**What (two levers to "instant" on real tables, with Phase 8).**
1. **Faster `fmt` assembly** — profile where the ~0.95 s goes (likely the per-cell `vctrs::new_rcrd` construction across thousands of cells) and cut the constant; this speeds up *every* `tab()`/`tab_num()` call, not just jamovi.
2. **Cache tier-3/4 for display-only toggles** — revisit the design's "don't cache fmt" call: a `digits`/`display`/`color`/`color_signif` change (when the needed fields already exist) should reuse the built `fmt` cells and only re-render, not rebuild. Needs the "which fields exist" tracking the cache design already anticipates.

The render lever (CSS-only rewrite killing the ~0.6 s kableExtra cost) is **Phase 8**. Already applied in 7e: `tooltips = FALSE` on the Jamovi render (570 → 250 ms small table). The two committed baselines track both levers.

##### Done (2026-07-10)

Both levers landed byte-identical (full suite green, 1235 pass / 0 fail; golden + all parity suites unchanged — no regeneration).

- **7f-1 — faster fmt assembly (universal).** The two `pmap_dfc(~ new_fmt(...))` closures (`tab_plain` `tab.R` ~L3140, `tab_num` ~L4231) hoisted their column-INVARIANT `display`/`colour`/`type`/`ref`/`comp`/`col_var` `case_when`/`if_else`/`switch` + the digits recycle OUT of the per-column loop (computed once; `tab_num`'s per-column digit case_when → base `if/else`); one shared `NA_reals` reused for the all-NA fields. Byte-identical; ~5-6 % on the big jmvtab table, more on normal-size tables (fmt/vctrs layer dominates).
- **7f-2/3 — tier-3 live cache (display / colour instant).** New store tier `tab3` in `R/jmvtab-cache.R` (schema 1→2; per-entry cap `JMVTAB_TAB3_MAX_ENTRY_BYTES` 2 MB; store budget → 12 MB) caches the **pre-`finalize` ARMED table** keyed by a data-dependent **base-key** {aggregate identity + pct + na + levels + structural opts} with a **transform-tuple** {ref/ref2/comp/OR/ci/arming/…}. `tab()` gained an internal `.return_armed` seam so `jmvtab_build` owns `normalize_color_spec` → cached `tab_build` → **`finalize_color_spec` applied FRESH** every interaction. On an exact-tuple hit the whole build is skipped and only the tier-4 layer runs: `finalize_color_spec` (colour measure/channel) + `jmv_reapply_digits` (digits; skips the fixed p-value line via `is.na(get_n())`; class-transparent via attribute capture/restore so the degenerate 0-group `tabxplor_grouped_tab` survives) + `jmv_apply_display` + cleannames. **Wins vs the 7e baseline: small build/digits 49×/47× (0.23→0.005 s), colour 28× (0.24→0.008 s); big 9-table build/digits 25× (0.96→0.039 s), colour 6× (1.12→0.19 s).** `pct`/`na`/`levels`/`ref` changes rebuild (base-key/tuple change) — fast via cached tiers 1-2 + 7f-1.
- **7f-4 — `tab_apply_reference()` carve (the "core carve", byte-identical).** The reference block (diff / ratio / rr / or + ref-col/row markers) extracted VERBATIM from `tab_plain` into the shared `tab_apply_reference()` (`R/tab.R`, after `tab_plain`), called by the fresh build and READY for the tier-3 re-ref (proven byte-identical: rebuilding the pct data.table from a cached table's ref-independent `pct` field + `tab_apply_reference` reproduces diff/ratio exactly). Realises the Phase-2/4-deferred factor-path reorg.
- **KEY finding — `color_signif` is a re-paint, not a reref.** For a diff/ratio colour `ci = "auto"` already computes the CI that grey / color_all merely GATE, so ignore↔grey↔color_all differ ONLY in the colour attribute `finalize_color_spec` overrides. The armed table is built canonically with `color_signif = "ignore"` (legacy colour = the base measure finalize refines to any policy) and `color_signif` is excluded from the tuple → the frequent color-driven significance toggle is **instant** with no fork/carve. Exception: NUMERIC means (`ci = "auto"` does NOT compute a mean CI) → nudge `ci = "diff"` (and split the tuple) only when a numeric col_var is present, so numeric ignore↔grey correctly rebuilds.



#### Phase 7g — Jamovi UI new features

##### Phase 7g-i – Export function, n_min, tests
Implement new user-friendly features :

1. A module-level **Export function** (not only Excel) with a user-friendly path selector.
- Best would be a true normal menu to search for folders and enter file name for normal people : maybe a hack could permits it.
- If not possible, it should at least have a user-friendly text bar for path : point to the specific `<USER>/<DOCUMENTS>` of the user by default, on Windows/Linux/Mac, not by adding a "~" incomprehensible by most user inside the text bar, but by fully writing the **real path** into the text bar (with an additional button to reset this text to it’s default `<USER>/<DOCUMENTS>` if needed).
- It’s difficult because Jamovi R session is locked inside Electron and can’t access many basic things.
- Default export should be Excel. But a new input list should permit to choose html (kable) or md instead. Changing the type should change the button’s text in something like the current "Export to Excel" of so, so it’s immediately meaningful for non-expert students (and Excel stay the very visible base workflow).

2. Un argument `n_min` : supprimer la ligne/colonne si son `n` est trop petit. It’s not as easy as it sounds to handle.
- Pragmatic solution : and `add_n` prints the right unweighted counts row or col for the users see, and it can be filter/select on (with the same `n` as the totrow or totcol that bears it).
- But this `add_n` column or row, that is really a duplicate of the total column or row with a different display, is chosen a bit randomly : with `pct = "row"`, it’s the last factor `col_var`. In reality, when there are several col_vars in the same table, each can have a `tot_n` a bit different depending on how missing values where handled. **What would be the more statistically consistent and clear for the user way to do that ?** With several `col_vars` and, for example, `pct = "row"`, there are several possibilities : show the minimun `tot_n` of the whole row in the total column ? Show a range [min;max] for clarity (and in that case : live calculations at display ? Or storing of min in `n` field and max in `tot_n` field) ? In that context, how to remove lines with `tot_n` < `n_min` ? Taking the minimum `tot_n` in the row may remove meaningful values > `n_min` ; taking the maximum would keep values below it. A rule could be : remove the whole line if the maximum `tot_n` in the whole row is below `n_min` ; then clear the cells with that, themselves, have `tot_n` < `n_min` (display "" instead of it’s normal value) ? Are ther caveats I’m not seeing ? We should think about it and design it arefully to really get the user-friendly and clear-for-the-user solution.
- The way I handled this in R in the past was just : `|> filter(Total >= 30)`, but with all these subtleties, maybe the `n_min` argument should be built in `tab()` and `jmvtab()`, with a shared function (public if it can be of some use for expert users ?) ?

3. Small UI changes :
- `method_cell` only have wilson : wald should be available too as an opt-in option (commonly used in teaching). Same in `tab()`, not onjy `jmvtab()`.
- `chi2` menu : change it to a proper tests menus ? chi2 button + anova button (make it clear in the very concise description which one is for which kind of variable) ?

###### Done (2026-07-10)

Shipped four features (maintainer scoping); every feature's LOGIC lives in plain testable R helpers (the jamovi `.a/.u/.r/.js` edits are inert until the maintainer regenerates `.h.R` in the live app — the closing gate). Full suite GREEN (360 blocks, 0 fail/0 error), **no golden regeneration** (all additive / byte-compatible).
- **1 — Export Excel/HTML/Markdown** (`R/jmvtab-export.R`, new, testable): `resolveExportPath(path, ext)` (Documents default, `~`→`USERPROFILE`, quote/backref-safe), `tab_html_string()` (self-contained inlined-CSS HTML), `jmvtab_export(tabs, format, path, replace)`. `.b.R` rewritten: `export_format` List + typed `path` + the `exportExcel` Action → dispatch → `jmvcore::Notice`. `.a.yaml` collapses `xl_path`/`xl_filename`→`path`; `.u.yaml` format ComboBox + full-width path + button-label JS (`export_format_changed`); `.r.yaml` drops `export_status`. Tests: `test-jmvtab-export.R`.
- **2 — `n_min`** (new public `tab(n_min = 0)` arg, threaded `tab()`/`tab_many()`/`tab_build`→ctx→`tab_assemble`): a **pure end-of-pipeline display filter** (recomputes nothing). Rule (§ user): drop a row only if its LARGEST base across col_vars `< n_min`, then blank cells whose OWN base `< n_min`; under `pct="col"` drop weak columns. Orientation read from each fmt column's `type`; base = `get_tot_n()`/`get_n()`. New helper `tab_apply_n_min()` (`R/tab.R`, internal; ungroup→filter→regroup for grouped tabs) never drops totals/add_n/p-value-line. New **`"blank"` display token** (`get_num`→NA, `format`→"", colour suppressed, `tab_xl` NA→empty). jmvtab: `.a.yaml` `n_min`, `.opts()`, tier-4 apply on the RETURNED copy + `"n_min"` in `jmv_tab3_base_key`'s `reapplied` (toggling never corrupts the cached armed table). Tests: `test-n_min.R` (21) + a jmvtab-cache non-corruption test.
- **3a — `method_cell = "wald"`**: new `ci_wald()` primitive (`R/tab-agg.R`, a `ci_pivot` on `sqrt(p(1-p)/n)`); `tab_ci()` cell arm now `switch(method_cell, wilson/wald)` (`R/tab.R` ~L5395); validation widened; roxygen + `.a.yaml` choice. Means-CI untouched; stars stay CI-inclusion. Tests: `test-calculations.R`.
- **3b — Tests menu**: `chi2` toggle relabelled "Statistical test — Chi-square (categorical) / ANOVA F (numeric)"; new `anova` List (welch/classic) sets `options(tabxplor.anova=)` around the build in `.run()` (baked into the p-value line → added to `.opts()` so it sits in the tier-3 base-key; a toggle rebuilds). Tests: `test-calculations.R` + a cache test.
- **Reference-level picker** (works via the existing fast rebuild; instant re-ref deferred): `.a.yaml` `refLevels` Array/Group{var:Variable, ref:Level}; `.u.yaml` ListBox (VariableLabel + LevelSelector) in the References box + free-text `ref` kept as expert fallback; `.js` `update`/`onChange_row_vars`/`onChange_refLevels` row-sync (from `logregbin`). New testable `jmvtab_ref_vector(refLevels, free_text_ref)` → named `ref` vector; wired in `.opts()`. Tests: `test-jmvtab-export.R` + a cache test.

##### Phase 7g-ii — user-friendly .js level-reordering UI

A simple, comptact, clear and user-friendly **level-reordering** UI for row/col/tab factors.
- Use one already existing in another common jamovi module if it’s possible. Build it from scratch in .js otherwise.

###### Done (2026-07-10)

No ready-made drag-sortable level control exists in jamovi (confirmed; `dev/…jamovi_dev.md` §13) — built
one as a **`CustomControl`** (`levelOrderCtrl`) with the maintainer's chosen UX: **▲▼ arrow buttons** (not
drag), **all selected factors stacked and grouped by axis** ("Row / Column / Table variables"; numeric
col_vars show a "no levels" note), in its **own collapsed CollapseBox before "References"**. `.js` reads
each column's levels via `requestData('column', {properties:['measureType','levels']})` (as `LevelSelector`
does), renders per-factor lists into a **detached fragment swapped in atomically** (no flicker), and writes
the order back to a new **`levelOrder`** Array option (one `{var, levels}` per reordered var); `.u.yaml`
adds `change` events on all three var boxes (shared `onChange_vars`). **R seam = internal only** (user
decision — R users keep `forcats::fct_relevel()` before `tab()`): new internal `tab(.levels_order=)` arg
(threaded through `tab_build`→`ctx`, `NULL` no-op for normal calls); `jmvtab_levels_order()` folds the
picker → named list; **`jmv_cache_aggregate()` relevels the shaped aggregate POST-fetch** (`jmv_relevel_cols`,
stored blob stays raw) + recomputes `remove_levels` for `levels="first"` — so a reorder is a **tier-3 input
(design §4e): tiers 1-2 reused, only the fmt rebuilds**, byte-identical to `tab()` on pre-releveled microdata
(new parity + cache-reuse tests in `test-jmvtab-cache.R`; full suite green, no golden regen). **OPEN —
maintainer step**: regenerate `jmvtab.h.R` from `.a.yaml` in the running app, then live-verify.

###### Iteration 2 (2026-07-10) — UX fixes from live test

- **Fixed a duplicate broken UI**: a `CustomControl` never claims its backing option (uicompiler
  `CustomControl.isOptionControl()===false`), so the compiler auto-generated a second (broken,
  `isSingleItem` crash) default control for `levelOrder`. Fix: **`hidden: true` on the `levelOrder` option**
  (uicompiler `insertMissingControls` skips hidden options) — the CustomControl is now the sole UI; the
  option stays accessible via `ui.levelOrder`.
- **Redesigned the control** (`js/jmvtab.js`): a **2-level collapsible `<details>` tree** — axis (open,
  left-indented) > `"<var> : N levels - reorder"` (collapsed; ONE click opens the list) — each tier a
  Material grey tint + border + ▸/▾ caret. The list is a **jamovi-style selectable list** (white bordered
  box, `color:#000`): click a level to SELECT it (first selected by default, highlighted in jamovi's own
  `#b5caef`), then an **Up/Down button pair BELOW the list** (or the **Up/Down arrow keys** when the list is
  focused) moves the selected level, which stays selected so it walks (fixes the "click all the way up"
  clunkiness). A **variable-signature gate** makes the frequent `updated` event a no-op unless the variable
  set changed, so reorder moves keep selection + open sections; collapse/selection state persists per var.
- **Re-regenerate `jmvtab.h.R` + recompile** (`jmvtools::install()`) after the `.a.yaml`/`.js` changes for
  the duplicate to disappear and the redesign to take effect.

###### Iteration 3 (2026-07-10) — stale-swap bug fix + polish

- **Fixed "2nd click does nothing, edits appear later"**: the async `requestData`+deferred-swap rebuild
  raced in-place edits (a snapshot read before the edit swapped in over it). Rewrote to a **synchronous
  `renderTree`** backed by a per-var `levelsCache` (placeholder + one-shot fetch → cache → re-render), so
  no async swap can clobber an edit; the `updated` handler now re-renders only on a variable-set change or
  a jamovi `$el` re-render (detected via a `data-tabx-tree` root marker), never on a plain move. Also fixed
  a `setValue`-by-reference **aliasing** bug (`writeOrder` now stores `lv.slice()`). Variable names **bold**.
- **General jamovi-UI technical findings** written to `dev/tabxplor_1.4.0_jamovi_dev.md` **§6.8**
  (CustomControl/option/`hidden`/`updated`/async-swap/`requestData`/colors/keyboard) for future UI work.


##### Phase 7g-iii — user-friendly .js reference-level picker

A per-variable **reference-level** picker (the `ref` reference point of comparison for the calculation of color helpers, of each `row_var` under  `pct="row"`, of each `col_var` under `pct="col"`).
- The field-level **ref re-ref** on the assembled table — **BUILT in Phase 9b-7** (`jmv_tab3_reref` / `jmv_tab3_rerefable` now live: a ref/ref2 change on the cached carrier recomputes diff/ratio/CI, no rebuild, ~3–4.5× faster; gated to pct="row" / one factor row_var / diff colour / comp="tab", else rebuild). `pct="col"` per-col_var re-ref remains a rebuild (not yet in the reref shape gate).

###### Done (2026-07-10)

The picker was **rebuilt as a Material `CustomControl` `refPickerCtrl`** (sibling of `levelOrderCtrl`; replaces jamovi's `ListBox`+`LevelSelector`, whose whitish look, natural-order-only levels and row_vars-only sync caused every reported problem). Per active-axis variable (row_vars under pct="row"/means, col_vars under pct="col") it renders one **compact line = a bold variable name + a native `<select>` drop-down** showing the current reference level `[Total, …levels in the reordered order…]` (shares `levelsCache`/`requestData`/`storedOrder` with the reorder tree). Stored by LABEL in `refLevels` (→ a reorder keeps the reference); the effective default (Total, or the first level under OR) is shown when unset; a **ref2 section** (the OR 2nd reference) shows only when OR is active. Re-renders on **explicit `change` events** on the `pct`/`OR`/`color` radios (`onChange_refopts`) + the variable boxes — a bare CustomControl does NOT get a reliable `updated` for other options (that was why ref2 first failed to appear on `OR`; `updated` is only the self-`setValue` skip-gate). Old `ref`/`ref2` text boxes commented out in `.u.yaml`; `refLevels.ref` → `String`, `refLevels`/`ref`/`ref2` `hidden`. `.b.R` filters `refLevels` to the active axis and dispatches (row-ref vs per-col_var col-ref).

**Backend (per-col_var col% references + fixes):** `tab()` now supports **one reference column per col_var under `pct="col"`** — a `ref` NAMED BY COL_VAR (impossible under the old single-ref collapse). Mechanism: `ref_vect` (per row_var × per col_var, the reference analogue of `pct_vect`) threaded into the factor leaf `tab_plain()` only; the col% math (`tab_apply_reference`) is unchanged (one col_var per leaf, so the leaf IS the per-col_var group). `resolve_ref_vector()` gained a `what=` arg (col_var warnings) and now name-matches a NAMED length-1 ref (fixed a latent recycle bug). `diff_index()` **exact-match-first** (then regex) — a chosen level label is matched literally, fixing metacharacter labels (e.g. `"$25000 or more"` — the reported "2nd row_var does nothing" bug) and substring collisions, while the stored `ref` attribute stays human-readable. `detect_refcol()` (fmt_class.R) makes `tab_ci()`'s diff-CI reference column follow the marked `refcol` (byte-identical for first/tot). Golden-locked: `f_col_ref_lvl/multi/partial/ci/or`; full suite green (1327), all existing goldens byte-identical.

✅ **CLOSED 2026-07-16 (migration C3)**: regenerated + built + installed; the picker is compiled into the `uijs` blob. ⚠ **Live-verify still owed** — C3 confirmed Crosstables runs on real data, but did not exercise the picker specifically. The field-level instant re-ref (`jmv_tab3_reref`) is **ON** (Phase 9b-7): a ref change re-refs the cached carrier instead of rebuilding (~3–4.5× faster, byte-identical).


#### Phase 7h — Jamovi UI js consistency and user-friendliness

jmvtab jamovi UI needs a **full .js customisation** of it’s buttons and other inputs behaviours for maximum user-friendliness. Look at `dev/tabxplor_argument_computation_map.md` for interdependency between arguments.
- Buttons should auto-change depending on other buttons choices to matchwhat really happens internally, but in a consistent, predictable and user-friendly way, it should feel natural, standard and not frustrate the user. For example, if the user coose `color_signif = "grey_non_signif"`, it should toggles `ci = "diff"` because it’s what `tab()` do internally, this computation is needed for the chosen color helpers.
- What is the current user-friendly standard / good practice for the behaviour of buttons in a UI ? Please make detailed web searches.
- Let’s say I choose `color_signif = "grey_non_signif"`, and it toggles `ci = "diff"` without the user chosen it deliberately. If the user then go back to default `color_signif = "ignore"`, should js  go back to `ci = "no"` automatically, or keep `ci = "diff"` ?
- The "grey out input when it makes no sense giving other inputs" logic should be improved : for example, the `totaltab` menu button should be greyed out when no tab_vars have been passed. What
- `digits` as text input is bad : better use a selectable list, the user click on it to display the list and choose the number of digit it wants ?
- `subtext` text input only takes about one third of the horizontal space in its row : it should take all the available space in its row in the menu. It can’t manage to set it’s size with .yaml only.
- What else, in jamovi UI, could be controlled with .js for the benefits of the user ?

##### Done (2026-07-10)

**Decisions** (maintainer-confirmed, grounded in a UX web-search — muted/disabled beats hiding; silently changing/reverting a user's field is an antipattern): (1) **CI coupling = re-paint, no toggle** — the `.js` never sets `ci` from `color_signif`; the backend already forces the needed CI (`ci="auto"` computes the diff CI the policy gates for factors; `jmvtab_build` nudges numeric means itself, `R/jmvtab-cache.R` ~L714-727), so an auto-toggle would be redundant and could overwrite a deliberate `ci="cell"`. The expert `stars`/`method_diff` reflect the effective state via their own `ci`-value enables. (2) **Inapplicable controls = grey only, keep value** (never reset — the backend forces the neutral behaviour internally, so the kept value returns intact). (3) **Full consistency pass**, no box restructuring.

**Greying matrix.** Value-based greying stays DECLARATIVE `enable:` in `.u.yaml` (jamovi auto-re-evaluates): `color_signif` policies `(!(color:no))` (was pct-gated); `stars` `(ci:diff||ci:auto)` (new, matches `method_diff`); `conf_level` widened to the CIOK pct-gate; `add_n`/`add_pct` `(pct:row||pct:col)` (new). tab_vars-presence greying (the DSL can't express emptiness of a Variables array) is IMPERATIVE: `applyVarEnables(ui)` in `js/jmvtab.js` `setEnabled`s `totaltab_1/2/3` + `comp_1/2` on `tab_vars.length>0`, called from `onUpdate` + `onChange_vars` (both fire on every var change). No control mixes declarative + imperative.

**Input fixes.** `digits` Number→List `"0".."6"` (TextBox→ComboBox; `.b.R` `as.integer(self$options$digits)`); `subtext` + export `path` fill their row via a `.js` DOM stretch (`stretchTextBox` in `onUpdate`) — jamovi 2.6.44's TextBox `width:` enum has **no `auto`** (the compiler rejects it), so `width: largest`=200px stays as the graceful fallback. Test menu label already clear (7g-i) — unchanged.

**Layout.** The standalone "Reorder levels" CollapseBox was folded into the bottom of "Levels and missing values" (its own full-width row below the na/levels/other grid; the `levelOrderCtrl` two-column reorder tree unchanged). jamovi grid cells don't span columns, so the ctrl sits in a single-column row wrapper to render full-width.

**Accepted limitations** (documented): `color="diff"`/`"ratio"` stay pct-greyed on a pure-numeric (means-only) table — benign, `color="auto"` already colours means; type-aware selectability would need imperative `.js` reading `measureType` (follow-up). `anova` enabled on `chi2` regardless of a numeric col_var (harmless no-op).

Suite green (375 blocks, 0 fail), no golden regen (UI-only + behaviour-preserving digits cast). ✅ **CLOSED 2026-07-16 (migration C3)**: regenerated + built + installed. ⚠ **Live-verify still owed** — each greying rule, the digits dropdown and the subtext width have not been exercised in the running app.


#### Phase 7i — test compatibility with Jamovi last solid version 2.7.37 (done)

Confirmation : jmvtab works well on jamovi 2.7.37


### Phase 8 – Parallelisation opt-in for the "many tables at once" survey workflow (DONE)

Phase 6b — 2026-07-09 researched whether parallelising `tab()`/`jmvtab()` over `row_vars` is a real perf win. **Verdict: a substantial, reliable win for the PRIMARY workflow — worth a Suggests-only opt-in; NOT a forced default, NOT for big data / live jmvtab.** Grounded PoC (mirai / base `parallel` / future.apply, W∈{1,2,4,8,12}). Parallelising the row_var/pair axis is **byte-identical** (0/82 tables checked). The key result **inverts the naïve prior**: the *small/typical survey* df is the sweet spot, the 8M df the worst case. On **10k–60k-row surveys × many tables** (tabxplor's core "export dozens of colored tables" use case): **~2.5–3.3× at W=4** (commodity/university PC), **~4× at W=8**, ~1 s setup, ~0 memory, **wins even on a fresh call** — because per-table cost is N-independent O(cells) fmt/chi2 work (seq batch flat ~2.5 s from 10k→60k). On 8M it ≈break-even-to-loss (memory-bandwidth wall + 336 MB×W transfer); few tables always lose; future.apply unusable (per-call df resend); data.table's own threading barely helps (~1.2×). jmvtab *live* = no (cached aggregate → nothing O(N) to parallelise). Recommended opt-in: `options(tabxplor.parallel=)` gating an internal `tab_pmap()` at the `tab_build()` seam, persistent pool + `setDTthreads(1)` + df pre-loaded once + byte-identical fallback, skip below a table-count threshold, **after** Phase 2/7c (the batch-export path does NOT overlap the cache, so the gain persists). Full findings + tables: `dev/tabxplor_1.4.0_decisions.md` **§26**; scripts `dev/benchmarks/parallel_poc_{micro,tab,survey,mirai_dispatcher}.R`, results in `results_1.4.0/phase6b_*.txt`.
- We should first choose only one parallelisation engine / package : either `mirai` or `parallel`. What would be the best choice for both performance and future-proofing ? Anyway the package should be in Suggest.
- If workers setup step is needed, it should be done the first time parallelisation is used and reused afterwards.
- ~~It should work on Windows / Linux / MAC, but for performance the main focus is Windows.~~ ⚠ **Superseded: dev + the primary run platform are now WSL2 Ubuntu.** It must still work on all three (CRAN), but see the WSL2 caveats below. `mirai` stays the right engine regardless — it is cross-platform, and a CRAN package cannot rely on `fork`.

⚠ **§26's numbers were measured on Windows PSOCK — re-measure before trusting them on WSL2.** The thresholds they justify (`tabxplor.parallel_min = 2L`, the "8M ≈ break-even-to-loss" verdict, the "~1 s setup", the "336 MB×W transfer") are all consequences of Windows having **no `fork`**: every worker needed a full data resend. Linux forks with copy-on-write, so setup and transfer are far cheaper and the break-evens plausibly move — the 8M verdict in particular may not survive. Nothing is broken; the numbers are simply from another platform.

⚠ **`detectCores(logical = FALSE)` is unreliable under WSL2 → `tab_parallel_workers()`'s "physical cores" intent is broken here.** Measured on this distro: it returns **12**, while `lscpu` reports 6 cores × 2 threads and the real host is an 8-core Ryzen 7 5800X3D. WSL2 does not expose SMT topology. The `min(..., 8L)` cap accidentally keeps the result sane (8 workers, not 11), so **this is a latent wrong-reason, not a live bug**.

⚠ **The deeper WSL2 change: the CPU is SHARED with Windows.** `.wslconfig` grants the distro 12 of the host's 16 threads *as a ceiling*, and heavy Windows-side CPU/GPU work runs concurrently. "Detect all cores and take n−1" was right on native Windows and is wrong here — it leaves no headroom. Prefer an explicit `parallel = <n>` over `parallel = TRUE` when the Windows side is busy.

New public `tab(..., parallel = )` (also `tab_many()`; `NULL`→`options("tabxplor.parallel")` off / `FALSE`
serial / `TRUE` auto workers / integer N). **Engine = `mirai`** (Suggests-only; R-core's official cluster
backend). All infra in **`R/tab-parallel.R`** (new): `tab_pmap()`/`tab_pmap_trampoline()` (serial branch IS
`purrr::map`, byte-identical, zero overhead; parallel ships `data` once via `everywhere()` +
`mirai_map()[.stop]`), a NAMED `"tabxplor"` compute profile (never clobbers a user's own `daemons()`),
`tab_parallel_workers()` (jmvtab→0, `_R_CHECK_LIMIT_CORES_` cap 2), `tab_pool_ensure()` (lazy warm/reuse),
exported **`tab_parallel_stop()`**, `ctx_slice()` + `tab_build_one()` (the per-row_var worker).

**Granularity = FULL per-row_var pipeline** (chosen over the build-only first cut, which measured only
~1.15x — profile `phase8_profile.txt`): `tab_build()` runs `tab_setup`+`tab_prepare_pop` ONCE on main (the
global `na="drop_all"/"common_base"` population drop lives here, so it cannot move), ships the prepared
`data`, then dispatches `tab_aggregate |> tab_transform |> tab_assemble_tables` per row_var to the pool;
main runs the cross-row_var `tab_assemble_output` (merge/pvalue/unwrap). Enabled by two changes: (1)
**`tab_assemble()` split** into `tab_assemble_tables()` (per-row_var finishing) + `tab_assemble_output()`
(output shape); (2) a **total-col decoupling fix** (`tab_assemble` ~L1770: `totnames |> unique()` so the
lone-total rename-back tests the DISTINCT name, not its count) — the leaked `Total_<lastcv>` suffix in
multi-row_var mixed-col_var tables becomes `Total`, making a per-row_var build byte-identical to a
standalone single-row_var one (the dispatch precondition; proven 4/4 across all na modes). This is a
conscious output change but touched NO existing golden (none covered the case; locked by a new
test-tab.R assertion). **jmvtab is always serial** (`cache_env`→0 workers; keeps its cache hooks); the
default (parallel off) path is unchanged.

**Byte-identical + measured**: full suite green (1336→1349 with new tests), NO golden regen;
`test-parallel-parity.R` (9 tests: weight/level-collision/NA, na drop/drop_all, option-override, threshold,
profile isolation, cleanup). **W=8, 30k rows × 12 row_vars: ~2.15x merged / ~2.44x list** (`run_parallel.R`
→ `phase8_survey.txt`); the gap to the §26 PoC 3.3x is the main-side merge + returning finished tables
(overheads the PoC's ship-once/independent-tables measurement excluded). Options `tabxplor.parallel`
(FALSE) + `tabxplor.parallel_min` (2L) in `.onLoad`; `.onUnload` stops the pool. `mirai (>= 2.5.0)` in
Suggests. Decisions §28.


### Phase 9 – possible simplifications and performance bottlenecks ?

The package have grown somewhat organically and I want you to review it for possible simplifications.
- The `tab()` functions have become a kind of jungle of their own, with many internal paths + many API functions.
`tab_build()` was supposed to be a simplification, but since it kept the fully vectorised arguments of the old tab_many() for retro-compatibility, it did not really simplified anything. So I would want to know : if `tab_many()` was to be kept on the current `tab_build()` path for backward-compat (merged in only `tab_many()` alias), but `tab()` was rewritten in a much simpler way from shared functions, would there be room left for simplification and performance gains ? Should we at the contrary keep internal functions, but a different one each time, just to be able to have internal arguments not passed in the public API ?
- Now that we are near the end, and some functions and workflows have grown organically, can you see final simplifications of the table building workflow ? What would we need to give up to really make meaningful simplifications ? What would we need to give up to really improve performance to the next level ?

##### Analysed (2026-07-11) — grounded verdict in `dev/tabxplor_1.4.0_decisions.md` §29

Fresh profile (`tab(5 row_vars × 3 col_vars, pct="row", color="diff", chi2=TRUE)`, gss_cat):
**arg resolution + the whole row/col-axis vectorisation = 0.2 %**; the O(cells) `tabxplor_fmt`
machinery = ~99 % (fmt build ~33 % + the `tab_compact` merge 0.72 s / 34 %), all `vec_case_when`/vctrs
record-reconstruction bound. Verdict: (1) **do NOT fork a second `tab()` core** (re-duplicates the math);
instead **collapse the shared engine's row axis to an OUTER `map`** — Phase 8 already built (`tab_build_one`/
`ctx_slice`) and byte-locked (`test-parallel-parity.R`) per-row_var == integrated-slice, so this is a
low-risk clarity/correctness refactor (kills the [tab.R:1252](R/tab.R#L1252) axis-mismatch latent bug,
retires `ctx_slice`, unifies serial/parallel), **not** a speed win. (2) **Split each leaf into a public
wrapper + a resolved-args internal core** (`plain_core`/`num_core`) — the roadmap's "different internal
functions" — removing the double `ref="auto"`/`tot` resolution and clearing `.fine`/`.by_table` off the
public surface. (3) Real speed is Phase 7f/10 territory (the merge + fmt build + `case_when`-over-fmt),
orthogonal to the restructure. **Implemented 2026-07-11: (1) done byte-identically (see Phase 9a Done);
(2) DEFERRED — byte-identity pins resolution + relabel + `.fine` in place, collapsing the split to a
cosmetic NSE-boundary extraction (poor risk/reward), so only the dead-code cleanup was done.**


#### Phase 9a — Internal clarify & simplification (DONE)

**Outer-map row-axis collapse landed byte-identical (full suite 1364 pass / 0 fail, NO golden regen).**
`tab_build()` is now `tab_setup → tab_prepare_pop → tab_aggregate → **tab_build_tables()**`. The new
`tab_build_tables()` (R/tab.R, shared by `tab_build` AND `tab_counts`) resolves one lean ctx per row_var
(`tab_rowvar_ctxs()`, replacing `ctx_slice()` + the `tabxplor_rowvar_fields` footgun) and maps the ONE
whole-per-row_var worker `tab_build_one()` (R/tab-parallel.R: transform → assemble_tables → one finished
tab + its pre-merge test) over it — serial `purrr::map` OR mirai, the **single dispatch** (the old
serial/parallel branch in `tab_build` is gone; the always-serial internal `tab_pmap` in `tab_transform`
is gone). `tab_transform()` + `tab_assemble_tables()` are now **scalar over ONE row_var** (the row loops
removed). `tab_aggregate` stays a whole-ctx pre-map step so the shared `fine_fused` + the jmvtab
`jmv_cache_aggregate` hook (wholesale `ce$hits` + shared-data relevel) still fire once; `jmv_cache_store_tests`
moved into `tab_build_tables` (reads the gathered pre-merge `tests` — a `!is.data.frame` guard skips the
numeric-only logical, matching the old `!is.list` short-circuit and avoiding a mixed-table ANOVA
double-merge). The latent [tab.R:1252] `pct`-&-`OR` axis-mismatch bug is designed out (`any(pct=="col")`,
scalar). `tab_counts()` now routes through `tab_build_tables` (dedup). Seam tests updated
(`test-carve-parity.R`); jmvtab-cache / counts / fuse / parallel-parity all green. **Deleted:** `ctx_slice`,
`tabxplor_rowvar_fields`, `tab_build_rowvar`, the old `tab_build_one`, `tabs_bind` (tab_classes.R), the
`#By rows first` block, and 223 commented dead lines inside `tab_plain`/`tab_num` (type stubs, `no_row_var`,
numeric `pivot_wider`, old `summarise`/`tabs_tot`/`tabs_totaltab`).

**Deferred (maintainer decision 2026-07-11): the leaf wrapper/core split + `exists()`→NULL-init.**
Implementing it revealed byte-identity pins all three moving parts in place: resolution stays in the core
(decision §29-#2, no drift), the relabel can't move (it renames level-collisions vs `names(data)`, which
differs before vs after the per-table `select`), and `.fine`/`.by_table` can't leave the public surface
(`test-num-fuse-parity.R` tests `tab_num(<NSE>, .fine=)` as a seam). With all three pinned the split
collapses to a cosmetic NSE-boundary extraction (thin wrapper forwarding every arg to an unchanged
~800/940-line core) — poor risk/reward on the two most byte-sensitive functions. Kept `tab_plain`/`tab_num`
whole; did the dead-code cleanup only. The `exists(…, inherits=FALSE)` guards are functional and left as-is.

Byte-identical internal re-cut — re-shape the shared engine so it reads *prep once → map a scalar core over row_vars → merge*. **No public API / vctrs-field change** (§29: this needs no backward-compat sacrifice). Full detail + code anchors + the fresh profile: `dev/tabxplor_1.4.0_decisions.md` §29.

1. **Outer-map the row axis (Finding 2).** Pull the `purrr::map`/`pmap`-over-row_vars OUT of `tab_aggregate`/`tab_transform`/`tab_assemble_tables` into ONE outer map in `tab_build()`: resolve a list of per-row_var *scalar* arg-sets in `tab_setup`, then `map`/`pmap(build_one_table)`. Serial and parallel become one dispatch (`purrr::map` vs `mirai_map`) — collapse the branch split ([tab.R ~L1069-1085](R/tab.R#L1069)). **Retire `ctx_slice()` + `tabxplor_rowvar_fields`** (build each per-row_var ctx directly, not by slicing a vectorised one). `pct_vect`/`ref_vect` lose a nesting level (per-col_var only). **Fix the live latent bug** at [tab.R:1252](R/tab.R#L1252) (col-indexed `pct` `&` row-indexed `OR` → length-mismatch warning). `tab_many`'s per-row_var vectors keep working — they are how the resolver fills the arg-list (more flexibility, not less).
- **Constraint:** preserve the jmvtab cache seam (the `jmv_cache_aggregate` hook in `tab_aggregate`, `jmv_cache_store_tests` after transform) and `tab_counts()`'s ctx-injection — both must still fire under the new structure (jmvtab is always serial; its per-pair cache is unaffected by a per-row_var outer loop). Re-validate `test-jmvtab-cache.R` + `test-counts-parity.R` + `test-carve-parity.R`.
2. **Split each leaf into public wrapper + resolved-args core (Finding 3).** `tab_plain()`/`tab_num()` → thin public wrapper (NSE parse + validate + `ref="auto"`/`tot`/`comp` resolution, for direct callers) over an internal `plain_core()`/`num_core()` that assumes **resolved scalar settings** and does only the data.table + fmt work. The outer map, the jmvtab cache and the parallel worker call the core. Removes the double resolution ([tab.R:2638](R/tab.R#L2638)/[:3682](R/tab.R#L3682)) and the redundant second `relabel_levels_in_varnames()` ([tab.R:2676](R/tab.R#L2676)); moves `.fine`/`.by_table` off the public surface into the core.
3. **Cleanup.** Delete the large commented-out legacy blocks in `tab.R`/`tab_classes.R` (dead `#By rows first` reduce, the `no_row_var` block ~L3200-3234, the numeric pivot_wider stub, `tabs_bind`); replace the `exists(…, inherits = FALSE)` guards ([tab.R ~L3040-3104](R/tab.R#L3040)) with NULL-init + `is.null()` (fold into the `plain_core` split).


#### Phase 9b — Performance: `tabxplor_fmt` as display-only, built at the end ?

To gain performance, should we use the ftm class and vctrs fields as user-facing and display only, to be built at the end of the workflow, but not used internally ? The §29 profile pins ~99 % of `tab()` in the O(cells) `tabxplor_fmt` machinery — the `pmap_dfc(new_fmt)` build and the `tab_compact` merge, both bound by `vec_case_when`/`if_else`-over-fmt + vctrs record round-trips.

Feasibility analysis written to `dev/tabxplor_phase9b_fmt_display_only.md`.

Decisions taken : **tiered** (bank the safe merge win first, gate the big rewrite); carrier = **unwrapped-fmt-columns** (a per-column raw field-frame = today's `vec_data(fmt)` + a col-meta attribute sidecar).
- **The idea to test (the design doc's core question).** Carry the build **internally as plain atomic field-vectors** (the fmt's `vec_data()` — n/wn/pct/diff/ratio/ci_inf/ci_sup/pvalue/… as plain numeric columns + the scalar per-column attributes), do ALL math/CI/chi2/merge/totals/level-drop/add_n on plain vectors/data.tables, and **materialize the `tabxplor_fmt` records ONCE at the very end** (for display, export, and user `$`/`mutate` access). The vctrs record becomes a **display / user-facing wrapper**, never an internal working type. **Hard constraint:** the final object is still a `tabxplor_tab` with real `tabxplor_fmt` columns — the vctrs **field contract users read with `$`/`mutate()` is unchanged** (§29: give up *using* vctrs generics on hot paths, never the fields).

**The carrier core (Phases 9b-4 → 9b-7).** The deferred-materialization rewrite that finishes the "records ONCE at the very end" goal: keep the build as plain field-vectors (the **carrier** = per-column **field-frames** = `vec_data(fmt)` + **col-meta** = the 9 attrs + **row-meta** = factor cols) and call `new_fmt` ONCE, so downstream ops run on plain data instead of reconstructing the record (vctrs ptype2/cast/restore) at every `dplyr`/`vctrs` step. Passes 2-4 already banked the in-place wins (~26%/~34%); the carrier is the remaining ~20-45%. Full brief + landmine ledger L1-L7 + per-phase detail: `dev/tabxplor_phase9b_fmt_display_only.md` §7.

**Boundary (Q1 — settle before 9b-4).** *Where* is "the very end"? **Boundary A** = end of `tab_build_one` (before `tab_compact`), one materialize **per row_var** — recovers leaf-tail + join + ci/chi2 + assemble reconstruction (~20-25%); parallel-clean (workers return finished tabs); leaves `tab_compact` + `tab_pvalue_lines` (**~15%!**) record-based; landmines L1/L5/L6/L7. **Boundary B** = the true end (after `tab_pvalue_lines`), one materialize **per whole table** — also recovers compact's `vec_rbind` + the pvalue `bind_rows` (~35-45% total), at the cost of **L3** (attr reconcile) + **L4** (grouped `as_refrow`) on the carrier + the p-value row on the carrier + re-locking `test-parallel-parity`. Build toward A first (9b-4→9b-6); 9b-7 is the B-only extension.

**Open questions (settle before committing carrier code):** **Q1** boundary A vs B. **Q2** carrier-join (L2) vs materialize-around-the-cheap-join (join is 0.9% → lean the latter, drops L2). **Q3** *worth it now?* — the **largest byte-identity surface in 1.4.0** for ~20-45%, value **back-loaded** (9b-4 is low-payoff infra; 9b-5 is the win); weigh vs pausing at passes 2-4. **Q4** sequence 9b-5 with **Phase 10** exporter-prep (same fmt read paths).

##### Phase 9b-1 — surgical `tab_compact` merge fix (DONE)

**9b-1 — surgical `tab_compact` merge fix (byte-identical).** The merge promoted totrow→refrow with `if_else(is_totrow & !any(is_refrow), as_refrow(.), .)` over each fmt column — a `vec_case_when` record round-trip (72 % of `tab_compact` per §29). Replaced by a direct `in_refrow` field write (new internal `promote_totrow_to_refrow()` in `R/tab_classes.R`, kept inside the per-sub-table `imap` so `any(in_refrow)` stays grouped per row_var). `as_refrow` only flips that field → byte-identical. **`tab_compact` 0.390→0.160 s (2.44×)** on the gss_cat 5×3 fixture; full merged call 1.78→1.55 s; `output_list` (no-merge) unchanged. Record: `dev/benchmarks/results_1.4.0/phase9b1_tab_compact.txt`.

##### Phase 9b-2 — measurement spike (DONE)

Harness `dev/benchmarks/phase9b2_fmt_cost_decomp.R` decomposed the per-table build across the 4 shapes. **Verdict: GO for 9b-3.** On the common factor path ~**30 %** (`vec_restore` reconstruction) to ~**48 %** (+`vec_case_when`) of the build is recoverable; the **materialize-once floor is ~0.5 %** (1.4 ms/21 cols) and pushing records through ops is **54.5× slower** than plain — so the fmt cost is almost entirely redundant reconstruction. Numeric-only tables gain ~nothing (cost = the data.table scan; `tab_num` already materializes once). **Fold the writers into 9b-3** — not a separate committable rung. Record: `dev/benchmarks/results_1.4.0/phase9b2_decomposition.txt`; full analysis `dev/tabxplor_phase9b_fmt_display_only.md` §5.

##### Phase 9b-3 — in-place fmt-reconstruction wins (DONE)

The four **byte-identical, in-place** optimizations toward the "materialize `tabxplor_fmt` records ONCE at the very end" goal — each a golden-gated committable step, no carrier yet. Cumulative **~26% off the common merged call / ~34% off the per-table build**. The deferred-materialization **carrier core** that finishes the job followed in **Phases 9b-4 → 9b-6** below (9b-4 tests-boundary round-trip, 9b-5 ci/chi2 writes, **9b-6 the Boundary-B local unwrap of `tab_compact`/`tab_pvalue_lines`** — which subsumed 9b-7; another −28..−30% on the merged call).

**Done (2026-07-11): pass 1 — the single materialization seam.** `fmt_materialize_col()` (`R/tab.R`, the ONE `new_fmt()` call via `do.call`; `fmt_frame_fields`/`fmt_col_attrs` contract constants); both leaves route through it (byte-identical, perf-neutral, full suite green, no golden regen).

**Done (2026-07-11): pass 2 — the scan-primitive fold** (byte-identical, **~11-15% factor-path**). `is_totrow`/`is_tottab`/`is_refrow` `.data.frame` methods each built a full nrow×ncols logical tibble (`select(where(is_fmt)) |> map_df |> if_all/if_any`); replaced by a shared `fmt_row_flag()` (`R/fmt_class.R`) that reads the field per fmt column and `reduce()`s. `is_totrow.data.frame` **28× faster**; per-table build common −11% / ci −12% / contrib −15%. The dead `partial` warning branch is dropped. Full suite green, no golden regen. Doc §6.

**Done (2026-07-11): pass 3 — `tab_pvalue_lines` masked-fill** (byte-identical, **the big one: ~25-34%**). A post-pass-2 line-profile pinned `tab_pvalue_lines` at **~34% of the per-table build** (`chi2=TRUE` adds a p-value row): the block filled the new row's empty cells with an `if_else` over EVERY fmt cell (the `$.tabxplor_fmt` `vec_proxy` pull + `mutate.tabxplor_fmt` round-trip + per-column `vec_restore` — the source of `vec_case_when` 20% + `mutate.tabxplor_fmt` 7% + much of `vec_restore` 33%). Replaced by a masked assignment `col[is.na(get_display(col))] <- fmt0(...)` (`R/tab_classes.R`), a no-op on columns with no empty cell. **Cumulative baseline→pass3: common merged −26% / per-table −34%; ci −25%; contrib −26%.** Full suite green, no golden regen. Doc §6.

**Done (2026-07-11): pass 4 — `new_test_tibble` memoization** (byte-identical, modest ~3-6% common build). The empty-placeholder `test` tibble costs ~1.4 ms/call (`tibble()` validation), built several times per table; it's stateless → memoized (`R/tab_classes.R`, cached copy shared safely via R copy-on-modify). Full suite green, no golden regen. The remaining `tab_pvalue_lines` cost (`bind_rows`+`vec_restore` adding the p-value row) is the vctrs **record combine**, inherent to the fmt type — only the deferred-materialization carrier removes it (the carrier core, Phases 9b-4→9b-7). Doc §6. **Corrected cost model** (profiling, `dev/benchmarks/results_1.4.0/phase9b3_profile.txt` + doc §6): the col_var **join is cheap (0.9%) — NOT the target** (drop the L2 focus; keep the record `full_join`); the ~30% reconstruction is **pervasive `dplyr`-over-fmt**; the **#1 recoverable chunk is `tab_apply_tests`/`tab_chi2` at 20%** (repeated `is_totrow` scans + `dplyr`-over-fmt group-matching). **Revised staging** (doc §6, supersedes the join-first order): (1) `tab_chi2`/`tab_apply_tests` on plain fields with row/col masks computed once (the 20%, needs the carrier at the tests boundary); (2) defer the leaf materialization so the carrier reaches the tests; (3) `tab_assemble_tables`+`tab_add_n_pct` on the carrier, `fmt_wrap` at `tab_build_one` end. Landmines: L1 (types) + L5 (boundary) + L6 (ci/chi2) + L7 (add_n); **L2 dropped**, L3/L4 avoided. Full brief: `dev/tabxplor_phase9b_fmt_display_only.md` §6.

##### Phase 9b-4 — carrier to the tests boundary (DONE)

Implemented as the **lean post-join round-trip** (maintainer decision, not the design's leaf-emits-carrier): two internal helpers next to `fmt_materialize_col` (`R/tab.R`) — **`fmt_unwrap(tab)`** decomposes a built table to a carrier `list(is_fmt, factors, fmt = per-col list(frame = as.list(vec_data(col)), meta = the 9 attrs), attrs = attributes(tab))`; **`fmt_wrap(carrier)`** is its exact inverse (materialize each fmt col via `fmt_materialize_col`, pass factor cols through, restore `attrs` wholesale). A byte-identical **no-op** `fmt_wrap(fmt_unwrap(tabs_text))` is inserted in `tab_transform()` right before `tab_apply_tests()` — establishing the carrier at the tests seam; `tabs_num` untouched. New `test-carrier-parity.R` (15 tests) locks `identical()` across factor/numeric/mixed/weighted/col%/add_pct/ci + grouped + subtext/test attrs. **L1** held (fmt-contract `typeof` lock green: `new_fmt` does no cast, so `vec_data → new_fmt` preserves types). Full suite green (FAIL 0, PASS 1354), NO golden regen. Bench: no-op adds +0.08 s / +6.9% (gss_cat 5×3 merged) — a temporary second materialization of each row_var's factor table, recovered by 9b-5. **Step A dropped** (leaf emits carrier + tail port): under Q2 (keep the record `full_join`) the leaf materializes for the join anyway, so the leaf-tail port is never load-bearing under Boundary A. Detail: `dev/tabxplor_phase9b_fmt_display_only.md` §7.2.

##### Phase 9b-5 — the tests-boundary WRITES on plain fields (DONE)

Both increments landed byte-identical (full suite FAIL 0 | PASS 1354, NO golden regen; git-stash `identical()` A/B: 10 contrib + 21 ci shapes). All in `R/tab.R`. The reframing that governs it: the chi2 whole-table **TEST is NOT the cost** (a 40×15 A/B was 0.1000 == 0.1000 s; the §6 "20%" was the DEFAULT-`calc` contrib writes, not the pipeline `calc="p"` test) — the O(cells) fmt cost is the **WRITES**. Approach throughout = **precompute-then-single-write** (real setters over plain vectors, NOT a `fmt_unwrap`/`fmt_wrap` round-trip). Recurring landmines: writes are **per subtable / grouped** (old grouped mutates) → run ungrouped then restore grouping; and combining fmt via `dplyr::if_else` / a grouped-mutate **recombine** **materialises the `wn` field** (NA→n) → reproduced with `set_wn(get_wn())` for exactly the columns/paths where the old code did.

- **Increment 1 — chi2** (`chi2_compute_test()` read-only test marshalling — no win, clarity + no-op removal; `chi2_write_contrib()` — the per-cell `var`/`ctr` + `comp_all`/contrib-`color`): **contrib per-table −41 % (1.7×), −30 % memory** (`dev/benchmarks/results_1.4.0/phase9b5_chi2.txt`). Dead `variances_by_group`/`cells_by_group` dropped.
- **Increment 2 — `tab_ci`** (net −58 lines): (a) the reference-row selection + `x_n`/`ref`/`ref_var`/`ref_n` (the grouped `ref_rows`/`ref_to_na` + ungrouped transmutes) → a plain loop with `group_last_pos(mask)` (per-subtable last-reference-row index) feeding the `ci_*` engine; (b) the CI write + `comp_all` + `visible` display → ONE ungroup/mutate/regroup; `ci_type`/`color` stays the positional `map2_df` (byte-identical, sidesteps the L-IDX quirk). **ci per-table −20 % (1.25×)** (`phase9b5_ci.txt`). Dead `tot_rows` dropped.

Combined: the two WRITE-heavy paths (contrib −44 %, ci −20 % vs pre-9b-5) recovered; the READ paths (chi2 test, common `color="diff"`) flat.

##### Phase 9b-6 — Boundary B via local unwrap (DONE)

**Re-scoped (maintainer, this session) from "step D / Boundary A" → "Boundary B via local unwrap".** Grounded finding: 9b-6-as-designed (carrier through `tab_assemble_tables`, materialize at `tab_build_one` end) buys **~0 % on the common path** (after 9b-5 everything inside `tab_build_one` is cheap: leaves materialize once; `tab_apply_tests` no longer reconstructs; `tab_assemble_tables` ~2 %; add_n on `pct="row"` adds one col; the join is 0.9 %). The real ~15-25 % was **Boundary B** — `tab_compact`'s `vec_rbind` + `tab_pvalue_lines`' `bind_rows` in `tab_assemble_output`. Both were rewritten to row-bind on **plain field-frames via a LOCAL `fmt_unwrap`→wrap** (the 9b-5 pattern), so `tab_build_one` keeps returning **records** (no `test-parallel-parity` re-lock) and **9b-6+9b-7 collapse into this one deliverable** (Boundary A skipped). New primitive `fmt_stack_frames()` (`R/tab.R`). Increment 1 = `tab_compact` (`tab_stack_tables()`: `vec_ptype_common` reconcile = **L3**, promote_totrow folded onto the field frame = **L4**; ~neutral perf, byte-identical, scales with #row_vars). Increment 2 = `tab_pvalue_lines` (**the win**: fmt-free skeleton for row order + per-column field append, subsuming the pass-3 masked fill). Byte-identity key: the old `vec_cast` materialised `wn` (NA→n; `get_wn` is the only getter with a fallback) — reproduced via `fr$wn <- get_wn(col)`. **Bench (gss_cat 5×3): merge_s −28..−30 %, list_s −8..−14 %, mem 51→45 MB; numeric ~flat** (`dev/benchmarks/results_1.4.0/phase9b6_boundaryB.txt`). Full suite FAIL 0, NO golden regen; 12-shape git-stash `identical()` A/B green (incl. per-row_var-ref L3, tab_vars-grouped pvalue, numeric ANOVA, list path). `fmt_unwrap`/`fmt_wrap` now load-bearing.

##### Phase 9b-7 — jmvtab tier-3 carrier + instant reference re-ref (DONE)

Scoped up (maintainer) from the literal "carrier + re-paint" (which barely moves the render-bound live UI) to **carrier + the deferred instant reference re-ref** — "change the reference level live → recompute only diff/ratio/CI, no rebuild" (cache-design §4c). All in `R/jmvtab-cache.R`; the reference-picker UI already exists (7g-iii) → NO `.h.R` regen. Byte-identical, full suite green (1433/0), NO golden regen.

- **Increment 1 — tier-3 stores the CARRIER** (`list(carrier = jmv_carrier_unwrap(armed), tuple)` = plain field-frames via `fmt_unwrap`, not a live tab — aligns tier-3 with the tiers-1-2 discipline; schema 2→3). `jmv_reapply_digits` rewritten onto the carrier (drops the snapshot/restore trick; the single `fmt_wrap` absorbs its reconstruction). A/B caught L1: `set_digits` casts to integer but `new_fmt` does not → `vec_cast(new_d, integer())`.
- **Increment 2 — `jmv_tab3_reref()`**: reconstruct `tabs_pct`+context from the carrier's ref-independent fields (data rows only) → `tab_apply_reference()` for diff/ratio → re-run the diff CI via `tab_ci()` on the DATA ROWS (p-value lines removed first — they'd drop one row/subtable) → copy CI back; p-value rows + table attrs (`test`/`groups`) verbatim. Gated by `jmv_tab3_rerefable` (only ref/ref2 differ, diff-armed, no OR) + `jmv_reref_shape_ok` (pct="row", one factor row_var, `!has_num_col`, levels="all", `!add_pct`, **comp="tab"** — comp="all" has a ref-DEPENDENT shape —, not auto+ci=diff); else the (fast, cached) rebuild.
- **Result** (`dev/benchmarks/results_1.4.0/phase9b7_reref.txt`): a ref change is **~3–4.5× faster** (reref vs rebuild). Locked by `test-jmvtab-cache.R` (reref == rebuild across 12 shapes + tab() anchor + fallbacks + $state). Detail + landmines: `dev/tabxplor_phase9b_fmt_display_only.md` §8.


##### Phase 9c — further simplifications ? (DONE)

Full analysis + fresh profile: `dev/tabxplor_1.4.0_decisions.md` §30. The three questions, answered:

- **Pure data.table carrier for in-place `:=`? — NO, dropped (maintainer-confirmed).** A fresh profile
  (post-9b-7) shows the build is **N-INDEPENDENT** (215k rows ≈ 21k rows) and O(cells): the tables are
  tiny, so copying is microseconds and `:=` copy-avoidance buys nothing; the expensive ops are
  row-CHANGING (level-drop/total/join/rbind) which copy regardless of `:=`; and a mutable DT is a
  byte-identity footgun (the jmvtab tier-3 cache stores carriers that must not mutate → `copy()`
  everywhere). The immutable **field-frame carrier stays** — faster *and* safer. Recorded so it is not
  re-opened.
- **Remaining perf levers — one clean win IMPLEMENTED.** `vec_ptype2.tabxplor_fmt.tabxplor_fmt` picked
  each reconciled attribute with `dplyr::if_else` ×9 → replaced with base-R `if/else` (**3.1× per
  call**; landmine: `same_comp` can be NA → `is.na()` first; `color` length ≤ 2 → `ifelse`). This drives
  every `c()`/bind/group over fmt AND is the compact merge's per-column reconcile: **merged call −7 %**
  (the merge marginal 0.046 → ~0 s), user `c()` of two fmt cols **1.8×**. Byte-identical (suite green,
  no golden regen). `dev/benchmarks/results_1.4.0/phase9c_ptype2_and_fusion.txt`. The other levers
  (per-leaf relabel ~5 %, `tab_apply_tests` marshalling ~22 %) were left; the big one (leaf-math ~30 %)
  is **Phase 9d** below.
- **Feature given up for simplicity — the tab()-level scan-fusion, REMOVED.** `options(tabxplor.fuse_min_rows)`
  + the fused-`.fine` block in `tab_aggregate()` were a NET NEGATIVE (+1–7 % when on) and dead by
  default (fusing an O(N) scan buys nothing when the build is N-independent). Removed. **Kept**: the
  `.fine`/`fine_for_pair()`/`use_raw` seam in `tab_plain()` (now EXCLUSIVELY the jmvtab cache seam +
  `tab_counts()`'s injected aggregate + the numeric `fine_num`). `test-fuse-parity.R` rewritten to drive
  `tab_plain(.fine=)` directly (the factor analogue of `test-num-fuse-parity.R`); the carve fusion test
  repointed (default == `.by_table`, both raw now).

##### Phase 9d — leaf math on base-R / matrix

`tab_plain()`'s three chained-`[.data.table` leaf blocks now run on plain numeric matrices / base-R
group-sums (the §30 lever 4, ~30 %). **Factor-only**; byte-identical (full suite FAIL 0 / PASS 1400, NO
golden regen; PoC-gated first — `dev/benchmarks/phase9d_leaf_math_parity.R` proves every equivalence
`identical()` across 648 shapes BEFORE the edit). Three new/rewritten pieces in `R/tab.R`:
**`tab_apply_reference()`** internals → matrix sweep (`P − P[refrow,]` / `P / P[refrow,]` / `P/P[,refcol]`;
signature + return shape UNCHANGED so `jmv_tab3_reref` is unaffected); **`leaf_wide_pct()`** (new) = pct +
`tot_n` via `M / D`; **`build_total_rows()` / `finalize_total_rows()`** (new) = total-table/row group
sums. **DECISIVE trap**: B/C sum with base `sum()` per `split()` group, NOT `rowsum()`/gforce (plain-double
accumulator drifts 1 ULP from the old `map(.SD, sum)` long-double → breaks `identical()`); `check.names =
FALSE` for `$`/space value-cell names. Region D (the `rowSums` Total column) kept. **Perf**: no-tab_vars
common −11 % / ci −7.4 % per-row_var build (E+F); git-stash A/B with tab_vars (B/C `map2` multiplier) 1
tab_var −20 %, 2 tab_vars × 2 col_vars −51 %. Detail: `dev/tabxplor_1.4.0_decisions.md` §31.




### Phase 10 — Unified exporter prep & display

Fully redesign exports to unify the different kind of exports in a common fast framework. One shared exporter-prep helper for `tab_xl`/`tab_kable`/`tab_md`/`tab_plot`; keep export parity (`format.tabxplor_fmt` vs the `tab_xl` bypass). **Full design brief: `dev/tabxplor_phase10_exporters.md`** (the single self-contained Phase 10 architecture doc — READ FIRST); decisions in `dev/tabxplor_1.4.0_decisions.md` §7-8, §10, §21-23, §33.
- Make it the faster possible (no useless computations if the result is not used afterwards, depending on the type of export and options chosen). Study the other performance gains made in Phase 9 and see if they can be of some use here too. If some features hurts speed, add an option to opt-out : for example in jamovi live UI where speed matters most.
- **Each exporter gets a base method (single tab) AND a list method** (several tabs rendered one-after-another, not merged — e.g. an HTML container is needed for kable).
- `tab_plot()` has a bad display and is hard to handle : **soft-deprecate** it (Q1 — keep exported, mark `lifecycle` experimental/superseded; do NOT hard-remove from NAMESPACE), keep it for future improvements

All export functions have **only a light backward-compatibility contract** : past arguments should not trigger errors but can, if really needed, be soft-deprecated and "wired to nothing". For this reason, whenever useful, their UI can be totally redesigned for user-friendliness, simplicity, performance and integration within the common prep framework.

New common features for all kind of exports
- Use variables `label` attribute more thoroughly in exports when it exists (in survey data formatting, I have the habit of putting the original questionnaire question in it, which can me meaningful information for the user) ? Where to print it, for useful additional information without clutter (not erasing variable names, which are real useful) ?
- **Integrate/export/document `tab_transpose()`** (a **fully commented-out / unexported** single-total stub at [tab.R:2133-2155] — a clean slate to finish) and the **opt-in transpose-at-export** for col% + several row_vars (console never transposes; warn on `pct="col"` with several row_vars).
- Revisit **compact-with-tab_vars** here (needs two-level nested rendering).

#### Phase 10a — design efficient Jamovi jmvtab display for live usage (DONE)

**DECIDED: keep + optimize kableExtra first; a dependency-free home-built `<table>` renderer is Plan B.** Grounded (web + code): jamovi's results panel ignores `htmlDependencies` and won't reliably run htmlwidget JS, so interactive tables (reactable/DT) are out, `gt` is heavy (global rule avoids it), `tinytable`'s interactivity wouldn't fire live.

⚠ **This section used to say jamovi "only honors inline CSS". That is true of `htmlDependencies` and was over-read into "no `<style>` tags" — RETRACTED in Phase 13d, from the capture, not inference.** `dev/jamovi/.../resultsview-*.js`: the Html element renders `e.html(r.content)` (jQuery, which inserts `<style>` as a live node), there is **no sanitizer** on that path (the `sanitize` hits are quill-delta-to-html, for the *annotation editor*; the `xss` hits are x86 mnemonics in a highlight.js keyword list), and jamovi itself does `this.$head.append('<style class="module-asset">'+t+'</style>')`. jamovi has its OWN stylesheet mechanism (`.module-asset`) and simply never processes htmltools deps. `html_style_block()`'s `border-collapse` has in fact been **load-bearing in jamovi since Phase 10e** — which is why the tables look right. Phase 13d moves cell colour into `<style>`-resolved classes and relies on this. The win comes from the shared prep (colours/refs derived ONCE), NA-hiding in prep, `tooltips=FALSE` (already Phase 7e), a "light" kableExtra path; the eventual home-built swap is isolated behind a `render_kable_html()` seam. The §23 profile's #1 lever (`fmt_color_selection`) is stale (deleted in Phase 5) → re-profile before ranking levers. Recorded in `dev/tabxplor_1.4.0_decisions.md` §33; full rationale in `dev/tabxplor_phase10_exporters.md` §10.

We must **make a grounded choice for jamovi jmvtab module base display of tables** : improve tab_kable() performance even without tooltips ? Fix tooltips calculation for them to be fast, since it gives a modern interactive look to the whole table ? Just make a faster flat html table ? Make it format with markdown tables with css classes ? : would it be possible to print .md inside html, with custom .css classes, in Jamovi, with a modern and professional look ? ; if a markdown js module is needed for it to be modern and professional-looking, can it (like, loaded when jmvtab UI loads ?) Jamovi own built-in table thing was unusable and without colors, formatting, etc. in the past, I wonder if it’s still the case. Otherwise, would there be a more modern option than kable for html tables in R (for example, js html tables with buttons in it to change number of digits, or even order of lines and cols, etc. ? ) ? What about the new types of tables Quarto tends to use nowadays ? Make web searches when needed, then write your detailed findings in `dev\tabxplor_1.4.0_decisions.md` (respecting it’s internal style and logic).

#### Phase 10b — study the right design and create a planned architecture document (DONE)

Wrote **`dev/tabxplor_phase10_exporters.md`** — the single self-contained Phase 10 architecture doc governing 10c→10g. Core:
1. a normalized **`tabxplor_render` ephemeral sidecar** (NOT tab attributes — dplyr desyncs them) holding the derive-once quantities (reference/total masks, colour slots/hex, stars, blank mask, bold rows, `[min;max]`, labels), consumed identically by the `format()`-string backends and the `tab_xl` numeric bypass;
2. one **`tab_export_prep()`** helper (new `R/tab-export-prep.R`) replacing the 4×-duplicated preamble + per-exporter role detection, base(single)+list(several) split;
3. **`format()`/`get_reference()`** `case_when`→boolean rewrite + a `.ref=` precompute arg (masks once, not 4×) + `format(syntax="excel")` folding `numfmt()` in;
4. robust var detection via `dplyr::group_vars()` + graceful `degrade` (fixes the no-factor crash) + `test-edge-cases.R`;
5. `[min;max]` table-level pre-pass;
6. tab_xl backend seam (openxlsx v1, ready for Phase 11);
7. `tab_transpose()` finished + opt-in transpose-at-export;
8. `tab_plot()` soft-deprecated.

**New decisions:** opt-in multi-field display (`pct (n)`/`pct ± ci`) via a new optional **`display_spec` attribute (9→10)** parsed only in `format()` (zero cost when unused); the `label` attribute → `tab_kable` header tooltip only. Sequencing + per-step golden/parity verification in the doc §12.

#### Phase 10c — rework format() for console display and exports that uses it (DONE)

Display of `tabxplor_tab` on console is quite long, and kable and fmt export uses it two, even in Jamovi display which must be the fastest possible : what are the performance bottlenecks and how to make it faster / remove useless stuffs and white elephants here ?
- Particularly, `format()`/`get_reference` display `case_when` must be changed for performance.

The `tabxplor_tab` class and the grouped one currently have a kind of bug that forbids them to work with every data.frame (like : with no `tabxplor_fmt` ; with no factors ; with factors after fmt columns and not before ; etc.) : it may come from the way `row_vars` and `tab_vars` are detected and from `tab_get_vars` etc. **I think this bug may only or essentilly happen for grouped tabs**. Obviously, these detections are absolutely needed to print colors etc., but currently, the failing mode is display error or export error.
- I would want a more user-friendly failing mode, still printing the df without the specialt tabxplor formattings and colors. Add testthat tests to be sure there cases do not throw error. Use messages if needed to explain to the user why it fails. Implement testthat tests with edge cases.
- More generally, I wonder if there’s a more reliable way to handle detection of row, col, tab vars, and the other informations needed for fmt and colors to compute, with smart fallbacks (no colors, no fmt formatting, etc.).

Passing a vector in display to display several fields, as an opt-in option ? (Won't work in Excel, but anyway Excel export do not use `format()` ?) Would it be possible to find a reliable syntax to command exactly the wanted fields and seps in a display ? Like `pct (n)` or `pct ± ci` ? Would it really be useful for data analysis users, or a white elephant with theoretical useless flexibility again ?

Scope confirmed with the maintainer: `display_spec` = a **curated** whitelist as its own isolated step (not the full parser)
- **`get_reference()` (`fmt_class.R`) `case_when` → base boolean composition** (branch selectors are scalar
  attributes; arms are per-cell boolean of the field masks). A/B-verified byte-identical + the
  subset-equivalence `get_reference(x[m]) == get_reference(x)[m]` the `.ref` memoization relies on.
- **`format()`/`pillar_shaft` (`fmt_class.R`)**: new `.ref = list(cells=, all_totals=)` precompute arg
  (masks derived ONCE, memoized when NULL — the 10d prep passes them in); hot `dplyr::if_else`→base
  `ifelse`, `str_detect("^-")`→`startsWith`; and the unconditional `x$var` (`$`→`dplyr::pull`, ~28 % of
  `format()` self-time) → `get_var(x)`. `format()` ~2× faster on the exporter path.
- **`tab_render_vars()` + `tab_degrade_inform()` (`tab.R`)**: robust, position-independent role detection
  via `dplyr::group_vars()` (fixes the factor-after-fmt miswrite) with a `degrade` fallback. Print routes
  `row_var` through it; `tab_kable`/`tab_md`/`tab_plot`/`tab_xl` gained a degrade guard rendering the plain
  frame + a message (no more `pull(integer(0))` crash). `tab_get_vars()` hardened. New render/degrade
  section in `test-edge-cases.R`. (Full exporter role-detection UNIFICATION stays in 10d.)
- **`display_spec` (§6, 9→10 per-column attribute)**: opt-in composite display `tab(display = "pct (n)")`
  / `set_display_spec()`, curated whitelist `c("pct (n)", "n (pct)")`, parsed only in `format()` (text
  backends; Excel keeps the primary field). `test-fmt-contract.R` 9→10 + snapshot accepted.


#### Phase 10d — common prep function (DONE 2026-07-12)

Design and implement the common prep function, looking carefully at all the changes and new features that will come next to ensure the shared prep function is ready for them.
- When a feature is export-type specific, like for example Excel only, it should be justified.
- `tab_totcol_range()`

**Part 1 (byte-identical; kable/md A/B `identical()` across 10 fixtures).**
- New **`R/tab-export-prep.R`**: `tab_export_prep()` builds the ephemeral `tabxplor_render` model ONCE and `tab_kable`/`tab_md`/`tab_plot` consume it, deleting the 4× duplicated blocks — A (compact via `tab_check_same_col_vars()` + the existing `tab_compact()`), B (degrade via `tab_render_vars()`), C (role detection), D (bold rows via `tab_bold_rows()`) — and the two-channel colour loop (now `fmt_col_ann()`, per-column `ann`).
- Derive-once win: `get_reference` once → `format(.ref=)`, `fmt_channel_codes` once.
- Medium-specific quirks stay LOCAL (md tab_vars keep+blank + `str_trunc` + span index + `new_group` trim; kable knitr `*`-escape + `row_spec`; plot ggpubr). `tab_totcol_range()` built + INERT (consumed in 10e/10f).
- `tab_plot()` soft-deprecated (`lifecycle` superseded).
- `test-export-prep.R`.

**Part 2.**
(a) **`tab_md()` list method**: a non-mergeable list (several row_vars and/or tab_vars → `tab()` returns a list; or differing col_vars) renders each table one-after-another (each keeping its tab_vars sub-tables) instead of erroring — gated by `tab_export_prep(list_method=)`; `tab_kable`/`tab_plot` keep the historical error. `tab_md` split into a thin wrapper + `md_render_one()`.
(b) **`tab_transpose()`** finished + exported (`lifecycle` experimental): `tidyr` pivot + rebuild flattened per-column attrs from a representative col_var column + swap axis flags (`type` row↔col; `in_totrow` field ↔ `totcol` attr; `in_refrow` ↔ `refcol`) + re-key `test`. Render-identical to a native `pct="col"` table; round-trips. `test-transpose.R` (53). The per-exporter `transpose=` arg is 10e/10f/10g. Detail: `dev/tabxplor_phase10_exporters.md` (Status) + decisions §33.

#### Phase 10e — rework tab_kable() (DONE 2026-07-12)

**Hybrid render engine behind a `render_kable_html()` seam** (new `R/tab-render-html.R`). `tab_kable()`
is now `option-resolve → tab_export_prep(list_method=TRUE) → map(render_kable_html) → tab_kable_join`;
its ~220-line monolith body was carved out. New public arg **`engine`** (`getOption(
"tabxplor.tab_kable_engine","kableExtra")`):
- **`"kableExtra"` (default)** = the legacy pipeline, **BYTE-IDENTICAL** to pre-10e (git-stash A/B empty
  diff over a fixture matrix; the two `any_bg` branches unified — `background=NULL` ≡ omit; totblock
  borders + legend read the prep, NA-regex retired). Locked by `test-render-html.R` structure snapshots.
- **`"html"`** = a dependency-free, self-contained inline-CSS `<table>` renderer (colours/bold on `<td>`,
  **no per-cell `<span>`** → DOM-size win; vectorised `do.call(paste0, …)` assembly; emits the same
  bootstrap tooltip `data-toggle`/`title` attributes so hover tooltips work in jamovi). Cross-engine
  content parity (same cell text + tooltip content) is tested.

Other changes: **cheap tooltips** — `tab_kable_print_tooltip()` `any()`-gates each of the ~9 `format(
set_display())` fragments (skips the ones the column has no field for) + reuses the prep's `.ref`
(byte-identical); **NA-hiding at source** — `format.tabxplor_fmt(na=)` is now honoured on the main path
(final overwrite; default `na=NA` → no-op / byte-identical), retiring the post-hoc `>NA</span>` regex;
**list method** — a non-mergeable list renders table-after-table (both engines) instead of erroring;
**total-block border roles** lifted into `prep_one_table()` (`roles$totblock_top/bottom`, shared, i18n
caveat flagged); **jamovi live render + `tab_html_string()`/`jmvtab_export()` switched to `engine="html"`**
(self-contained; dropped the lightable/cosmo `includeCSS` + `scroll_box` + class-strip → `tab_render_
scrollbox()`; html export no longer needs kableExtra).

**Perf** (gss_cat, `dev/benchmarks/results_1.4.0/phase10e_{baseline,after}.txt`): cheap tooltips cut the
kableExtra big-table render 0.50→0.36 s (−29%); the **html engine = 0.16 s (3.1× vs baseline, 5.8× less
memory)**, 0.072 s without tooltips. The html engine WITH tooltips (0.16 s) beats the old jamovi
kableExtra path WITHOUT tooltips (0.22 s). Full suite green (1601); no golden regen.

**DEFERRED with documented blockers (decisions §33)** — three doc-listed 10e "features" hit real issues:
1. **spanning col_var header** — its "parity with console" rationale is false (the console disambiguates col_vars by suffixing level names `Other_race`, which kable already inherits → a spanning header is redundant);
2. **`[min;max]` total column** — unsettled semantics (would make the `pct="row"` Total column show "100%" on most rows but a base-count range on others, overlapping the existing `n` column);
3. **label header tooltip** — the source `label` attribute does NOT survive `tab()` building (`prep$labels` is NULL), so it needs core-pipeline plumbing first. `transpose=` arg deferred to 10f/10g (uniform wiring). Flagged for the maintainer: `kable_tabxplor_style()` is an orphaned exported duplicate (candidate for soft-deprecation).

#### Phase 10f — tab_md() (DONE 2026-07-12)

Coloured markdown export via **break-derived pandoc bracketed spans**. A COLOURED table (any fmt column whose `fmt_color_channels` gives a non-zero slot) wraps EVERY fmt cell in `[<num>]{.class}` (uncoloured cells get the neutral `.n`) so numbers stay aligned in raw text (**uniform-span layout**, maintainer's choice); an UNCOLOURED table (or `color = FALSE`) is byte-identical to the plain layout. **Class names** (maintainer's choice over slot names — CSS-legal, readable, per-table): pct diff `p5/p10/p20/p30` + `m5/...`; sd mean diff `sd0_2/sdm0_2/...`; ratio/OR/`x2` rule `x2/x1_5/...` + `d2/...`; contrib `b1/bm1/...`; background = same names prefixed `bg`. Names are **palette-INDEPENDENT** (slot->break); `theme/color_type/html_24_bit` change only the CSS. New exported **`tab_md_css(tabs)`** generates CSS matching *that table's* breaks + palette (+ a `@media (prefers-color-scheme: dark)` block), reusing the SAME slot maps the spans use (cells <-> CSS can't disagree); `tab_md(css = TRUE)` embeds it inline. New `tab_md()` args: `color` (default TRUE), `theme/color_type/html_24_bit`, `title` (pandoc caption), `css`; `wrap_rows` default `50 -> NULL` (lossless). Shared `fmt_col_ann()` extended to carry per-cell `text_slot`/`bg_slot` (byte-neutral for kable/plot). Golden `_snaps/golden.md` regenerated for the 8 coloured display cases (numeric means colour by default); uncoloured cases byte-identical. Detail: `dev/tabxplor_phase10_exporters.md` (Status + Sec 12). **Deferred to 10g:** the `transpose=` arg.

##### Original plan (historical intent)

`tab_md()` current version was made for a specific use case and never totally integrated into tabxplor : the aim is to fully integrate it.
- color helpers must be handled with very shorts pandoc bracketed spans, everything padded and align to preserve human readability assuming monospace font (even out of preview mode). Examples for diffs : `.+5`, `.+10`, `.+20`, `.+30`, `.-5`, `.-10`, `.-20`, `.-30` etc. ; examples for ratios : `.x1.2`, `.x1.5`, `.x2`, `.x4`, `./1.2`, `./1.5`, `./2`, `./4`, etc. : would these names be valid css classes / pandoc bracketed spans ?.
- Is there a possibility to make them these css classes work inside jamovi, for exemple in a html rectangle, with a light yet modern markdown preview working with tables (natively, or by adding html/js new dependencies ? ; load these possible dependencies when the tabxplor function and menu load ? What about the css styles, should we load them at tabxplor UI startup or at table creation ?) ?
- Even if they do not work on jamovi, I want them to work in Positron IDE Viewer, be it with pandoc bracketed spans (prefered solution if workable) or another way
- Even if pandock bracketed spans not working on tab_kable inside Viewer, I still want an option to export as very simple markdown with pandoc bracketed spans, to use in markdown editors with customisable css working with bracketed spans (just for information, I have Positron IDE custom syntax highlighting in normal editor with a personal VS code extension). It shall really remain simple human readable padded/aligned markdown.

#### Phase 10g – rework tab_xl() (DONE)

`tab_xl()` reworked onto the shared prep + `format(syntax="excel")`, **4132 → ~810 lines**. Full suite
green. Detail: `dev/tabxplor_phase10_exporters.md` (Status). Maintainer steer this session: NO byte
parity with the old Excel needed ("around the same" suffices); the old export was a "white elephant", so
aggressive simplification is welcome.

- **`format(x, syntax="excel")`** (new, `fmt_class.R` `excel_numfmt_code`) folds the old inline
  `numfmt()`, fed `format()`'s OWN x100/ci/TEXT masks + adjusted digits → the Excel bypass can't desync.
  **Fixed two latent old-`numfmt` desyncs**: `diff` **pct** displays now get a `%` code (was showing
  `-0.0`), and `pvalue` cells keep `%` scaling (Excel now matches the console).
- **Consumes `tab_export_prep(backend="xl", compact=FALSE, drop_tab_vars=remove_tab_vars,
  list_method=TRUE, compute=c("refs","bold"))`** — killed the two `tab_get_vars()` passes + the
  duplicated preamble + copy-pasted bold/reference logic. Geometry from `roles`/`bold_rows` (offset by
  sheet `start`); colours stay on tab_xl's two-channel `fmt_color_channels` (the prep's text-only
  `color_cols` misses bg-only cols); number styles memoised per distinct code; per-table degrade added.
- **Simplifications (maintainer-approved):** `hide_near_zero` + `n_min` greying (`insufficient_counts`)
  **dropped** — soft-deprecated (kept, inert, warn; `n_min` → `tab(n_min=)`). ~2500-line dead tail +
  interspersed dead comments deleted.
- **Deferred to the openxlsx2 phase (Phase 11, renamed Phase 10h in this roadmap):** backend closure
  seam, significance **stars** in Excel, `[min;max]` total-column (still INERT), `transpose=` arg, and
  the per-table-writer split (Phase 11 rewrites the write/style path, so an extraction now is throwaway).
- **Pre-existing (NOT 10g):** `color="contrib"` + `comp="all"` errors in the shared colour engine
  (`get_mean_contrib()` size 0), for `tab_kable` too — a Phase 5 issue, fix separately.


### Phase 10h — Excel engine migration (DONE)

**Full clean migration** (maintainer decision, over the design doc's dual-backend seam): `tab_xl()` rewritten on **openxlsx2 only**; `openxlsx` dropped from Suggests, `openxlsx2` added; `jmvtab-export.R` guard swapped (it already routes through `tab_xl()`). Full suite green (**1748**, no golden regen); `test-export-parity.R` (value/code-path parity) + the numFmt-code lock unchanged and green.
- **New `R/tab-xl-backend.R`**: ~14 thin `xlb_*` openxlsx2 wrappers (in-place R6 `$` methods) + **pure range coalescers** `xl_runs`/`xl_rect_dims`/`xl_coalesce` (base-R A1 math, unit-tested in `test-xl-backend.R`). The coalescers turn per-cell style targets into the fewest **multi-area `dims`** so each shared style is applied ONCE over the largest range (the perf lever the maintainer asked for; numFmt codes + colour slots each grouped + coalesced).
- **`tab_xl.R` rewrite** (single-tab-first + list): orchestrator `tab_xl()` → pure per-table **`tab_xl_plan_one()`** (raw `get_num` values + numFmt codes + colour slots + a unified font plan + geometry — parallel-safe) → per-sheet writer **`xl_write_table()`** (issues the openxlsx2 calls). Sheet grouping (`sheets="auto"/"tabs"/"unique"` + start offsets) kept.
- **Stars** folded into the numFmt literal (`0.0%"***"`, gated by `getOption("tabxplor.stars")`), cell stays a real number. **`transpose=`** (maps `tab_transpose()` before prep) wired. **`conditional_format=`** accepted but experimental (message + falls back to hard styles — deferred: CF can't reproduce field-derived colours without hidden helper columns, and the coalesced hard-style path is fast/exact/small). `n_min`/`hide_near_zero` stay accepted-but-inert. **NO `parallel=` on `tab_xl`** (a benchmark showed only ~1.09× — the openxlsx2 write is serial and dominates ~92%; Amdahl-capped, so removed; the plan builder is still pure and called serially via `purrr::pmap`). `dev/benchmarks/results_1.4.0/phase10h_openxlsx2.txt`.
- **openxlsx2 style findings** (probe-verified, in the backend header): `wb_add_*` **merge across aspects** automatically (== v1 `addStyle(stack=TRUE)`); **within an aspect** the default replaces, so borders pass `update=TRUE` (only drawn sides). `wb_add_font(update=)` is **buggy over large ranges** when the sheet has scattered cells → all font needs are aggregated per cell into ONE complete descriptor applied with `update=FALSE`; cross-aspect merge preserves numFmt/fill/border/alignment. Borders reject multi-area `dims` (fills accept it) → `xlb_border` applies per rectangle. `wb_add_data(na=NULL, apply_cell_style=FALSE)` → blank NA cells, raw numbers.
- **Styles-manager write optimization (DONE, 2026-07-12)** — replaced the ~40 per-aspect `wb_add_*` passes with a **precompose**: `tab_xl_plan_one` builds a per-cell full-style grid (`xl_build_styles`: font+fill+border+alignment, borders painted onto 4 side matrices, alignment onto zone matrices), groups into the fewest DISTINCT styles; `xl_apply_styles` registers deduped fonts/fills/borders + a composed cell xf ONCE and applies by id with `set_cell_style` over each style's coalesced dims. numFmt stays a separate grouped `wb_add_numfmt` merging pass. **single 0.34→0.24 s (~1.4×), 12 tables 5.5→3.0 s (~1.8×)**; fidelity verified; suite green, no golden regen. The dropped per-aspect wrappers (`xlb_font/fill/border/align`) + `xl_rect_dims` were removed. Drove it: `set_cell_style` is 1.7× cheaper/call than `wb_add_font`; the profile pinned the cost in openxlsx2's per-call data.frame churn (`mapply`/`[.data.frame`/`read_xf`).
- **Parallel-write-merge studied, NOT pursued** (maintainer chose styles-manager only): each worker builds its sheet in its own wb, main merges via `wb_clone_worksheet(from=)` — works only via a save→`wb_load`→clone workaround (clone fails on in-memory borders, the same openxlsx2 styles bug), ~2.5–3× for batches only, but dominated by the styles-manager win (which also helps single-table export) + adds mirai/temp-file/merge machinery. Detail: `dev/benchmarks/results_1.4.0/phase10h_openxlsx2.txt`.


#### Phase 10i – additional rows/columns and pvalue lines simplification ?

`add_n`, `add_pct` and pvalue_lines add complexity in the whole workflow. I want to **study the possibility to only add these additional rows or columns at display time**, using `tabxplor_tab` level attributes to know it must be done (or column-attributes, or global options, what would be best ?) ? This is a design task : just study if it would possible possible and reliable.
- Distinguish between display modes that can use `display_spec` to print several informations in the same cell (console, kable, md ; for example print `add_n` as : `"100% (n= 114)"`), and display modes that needs to create new columns/rows (Excel ; for example print `add_n` by adding a new row or column efficiently, at the end ? Would it be a good idea to do it without redoing the whole fmt reconstruction, which is always a performance bottleneck ?).
  + The main caveat, if I understand it well, is that `display_spec` is a column attribute ? Would there be a reliable way to use the already existing display vctrs field at it’s place (removing `display_spec` as a column attribute totally), ensuring simple displays like `pct` or `diff` stay on a fast track for maximum performance, compared to more complex display like `pct (n)` (that of course themselves need to be the fastest possible).
  + Also, for reliability, keep simple displays as they are, but require complex display to add tags for fields they want displayed, for more reliability ? For example : {`{pct} ({n})`,  `{pct> (n={n})`, `{pct} ({ratio})`, etc. What would be the most standard and reliable tag for this, if `{}` is not a good standard ? Display of `add_n` in console should be, for the total column of row percentages, something like : `{pct> (n={n})` (with `100%` in pct, and with everything padded and aligned for human readability). Check if it can be done fast enough, without hindering performance.
- Print at display/export. Is the data necessary for this already available ?
  + `add_n` must check all `tot_n` attributes, and display the smaller in a new `n` column or row (depending on pct type like now), or the interval min and max. Default to minimum. Global option to set min max instead ? Or would it be a good idea to do everything in a display spec like `{pct} [n:{n_min}-{n_max}]` (or is it a new white elephant that will reduce performance at display for nothing, since n_min and n_max does not even exist on the cell fields ?) ?
  + `add_pct` : does it have every data needed ?
  + pvalue_lines : we should store the global tests table as whole `tabxplor_tab`-level attribute, like in a former version of tabxplor (table is still there but removed at pvalue lines creation); the default behaviour should be "print pvalue as lines in the display/export if they were done and summary table is here". Ensure the test table display in console is fast (I think it may have been a display bottleneck in the past). global options should . pvalue_lines can’t really use the `<>` syntax, to instead of putting them in the display of another line or column, it’s better to actually create new lines or columns like now, but to do it at display/export efficiently.
- Since `add_n`, `add_pct` and pvalue_lines as actual rows and columns in the data were exceptions that added complexity to the pipeline, their removal at all steps before display/export calls for a **huge code simplication**.

**DESIGN SETTLED (2026-07-12) — see `dev/tabxplor_1.4.0_decisions.md` §34 (the full findings + phasing).**
Verdict: worthwhile, not a white elephant. Decisions:
1. **display-only** — the built tab omits the `n`/`col_pct` columns and p-value rows (kept only via `print()`/exporters); the `test` attribute is KEPT (stop dropping it); `tab_pvalue_lines()`/`tab_add_n_pct()` stay as on-demand materializers.
2. the composite recipe moves to the per-cell **`display` field with a glue `{}` grammar** (`"{pct} (n={n})"`), **dropping the `display_spec` attribute** (10→9), with a short-circuited `get_num()` gate.
3. **add_pct = a real appended column/row** at display; only **add_n** goes in-cell (text) / an `n` column (Excel).


##### Phase 10i-A – consistent display `{}` grammar (DONE 2026-07-12)

The composite display is now a per-cell **`display`-FIELD** `{}` template (`"{pct} (n={n})"`); the
Phase-10c **`display_spec` attribute is DROPPED (10 → 9)** — forced per-cell because under `pct="col"`
add_n/add_pct are ROWS, not columns. Three shared helpers next to `get_num()` (`R/fmt_class.R`):
`display_primary()` (gated resolver — one fixed `grepl`, composite → first `{field}`, malformed →
no-crash), `parse_display_template()`, `validate_display_template()` (**`{}`-only**, no curated sugar;
fields ∈ `pct,n,wn,mean,diff,ratio,ci,or,ctr,var`, `ratio`→`rr`). `tab(display="{pct} (n={n})")`; the
old `pct (n)`/`pct_n` strings now error. The internal `pct_ci`/`mean_ci`/`or_pct` tokens are KEPT
(pipeline-set integrated rendering; `{}` can't express them, never user-typed). Every display-token
consumer (`get_num`/`set_num`, `format()` masks, `vec_ptype_abbr`/`vec_ptype_full`,
`tab_kable_print_tooltip`) resolves composites to the primary; Excel exports the primary automatically
(no special-case). `tab(display=)` writes the template only on value cells where every field is non-NA →
`"{pct} ({n})"` byte-identical to Phase 10c; count-only/pvalue/blank cells keep their token. Benchmark
(`results_1.4.0/phase10iA_display_grammar.txt`): Solution 2 shipped, gate negligible (~11 ns/cell,
pipeline unchanged; sugar vs `{}` = one-time ~0.2 ms validation, no per-cell cost). Tests:
`test-display-grammar.R` + `test-fmt_class.R` + `test-fmt-contract.R` 10→9 (+ snapshot); goldens
regenerated (attr drop only). Full suite green.


##### Phase 10i-B – display-only migration

###### Increment 1 DONE (2026-07-12) — p-value rows are display-only

The built `tab()` no longer bakes p-value rows: `tab_assemble_output()` stops calling `tab_pvalue_lines()`,
so the whole-table `test` attribute is KEPT and the rows are materialised at DISPLAY. New maintainer
decision (this session): **p-value = block in the R console, rows in exports** — the console shows the
compact `# <col>: Chi2=… p=…` block (`print_chi2()`, which now fires for a normal `tab()` because `test`
survives; moved *below* the `print == "kable"` branch so kable-mode still gets rows), while
kable/md/Excel/jamovi materialise p-value ROWS. New shared idempotent materialiser
**`tab_materialize_extras(tab, backend, pvalue)`** ([R/tab_classes.R](R/tab_classes.R), next to
`tab_pvalue_lines`) is the ONE display-time hydrator, called by `tab_export_prep()` (after
`tab_resolve_tables`, before `prep_one_table`) and by `tab_xl()` (before `tab_transpose`); Increment-1
body = `tab_pvalue_lines`. `tab_apply_n_min()` dropped its dead `pline` protection. jmvtab simplified:
the tier-3 carrier no longer holds p-value rows, so `jmv_tab3_reref()` lost its `data_mask`/`pval_mask`
- "drop p-value rows before tab_ci" dance and `jmv_reapply_digits()` lost its `n==NA` skip.
**Byte-identical exports** (`_snaps/`, export-parity green); the only golden changes are the 3
chi2-driven fixtures (`f_chi2`, `f_color_contrib`, `c_contrib`) losing the "pvalue" row + a benign `wn`
change (unweighted chi2 tables now store raw `wn=NA` instead of the fallback `tab_pvalue_lines` baked;
`get_wn()` still recovers it). Suite green (1804). The `render_extras` attribute + add_n/add_pct
migration is Increment 2.

###### Increment 2 DONE (2026-07-12) — add_n / add_pct rows/cols are display-only

The built `tab()` is now the "core" table: `tab_assemble_tables()` stops calling `tab_add_n_pct()` and
instead stores the intent in a small **`render_extras = list(add_n=, add_pct=)` table attribute**
(carried through every dplyr verb + the vctrs reconcilers exactly like `subtext`/`test` —
`get/set_render_extras`, ~37 threaded sites). `tab_materialize_extras()` grew the add_n/add_pct arm: it
**reuses `tab_add_n_pct()` verbatim** on the finished table (its grouped outer-mutate reproduces the
per-subtable scoping — proven byte-identical across single / merged / tab_vars / means / multi-col_var /
pct=row / pct=col), so Excel keeps a real `n` column; for TEXT backends **`tab_fold_addn_incell()`**
folds add_n into the Total cell as `{pct} (n={n})` (decision 1; default = the Total's own base, opt-in
`options(tabxplor.totcol_range=)` `"range"`/`"min"` → the cross-col_var base via `tab_totcol_range()`).
Console print materialises the text extras (`pvalue=FALSE`, block for p-value). `tab_transpose()` carries
`render_extras`. **Dead special-cases removed** (extras never exist at build now): `chi2_compute_test`'s
`c("n","row_pct")` row-exclusion, contrib's `all_col_vars` exclusion, `tab_apply_n_min`'s helper/helprow
(→ `protect = totrow|tottab`); KEPT the `tab_ci`/`tab_pct` `all_col_vars` vector extensions (harmless
robustness) + `arrange`'s guard. **Back-compat shim** (`$.tabxplor_tab` / `[[.tabxplor_tab` /
`pull.tabxplor_tab`+grouped): reading `tabs$n` / `tabs[["n"]]` / `pull(tabs,"n")` (or `col_pct`) on a
core table reconstructs the column from the Total column (byte-identical) with a `lifecycle::deprecate_soft`
— gated on `%in% names(x)` so the fast path is untouched; `pull` re-injects the quosure into
`dplyr::pull(as_tibble(.data), !!vq)` to preserve tidy-select NSE (a bare NextMethod broke it). **Perf
gate** (`dev/benchmarks/results_1.4.0/phase10iB_display_only.txt`): build −6 %, display +9 %, net neutral
(work moves build→display; the jmvtab cached build is now cheaper). **Golden regen (conscious):** ALL
`_golden/*.rds` (add_n/add_pct cols + pct=col rows removed, `render_extras` gained) + `c_or` colour + the
`golden.md`/`render-html.md` display snapshots (add_n column → in-cell). Suite green (1815); `test-display-
extras.R` added. **User-visible:** the add_n base moves to an in-cell `100% (n=…)` on console/kable/md
(Excel keeps the `n` column); the built object loses the `n`/`col_pct` columns + p-value rows (use the
`test` attribute / `get_n(tab$Total)` / the deprecated `$n`).


#### Phase 10j – workflows additional integration and performance improvement ?

Now that the carrier allow to only construct `tabxplor_fmt` vctrs fields at the end, that a common preparation function for exports is done, and that `add_n`, `add_pct` et pvaluelines are only added at display, can you think about new ways to simplify the build table pipeline and the export pipeline, integrate the package function’s ecosystem, and make additional performance gains for the main use cases (10-60k survey tables with many row_vars and col_vars, 1M+ big datasets, instant live tables in jamovi with cache) ? Can you identify some features whose removal would make the workflow faster, and that we can turn optional ? Can you think about some ways to integrate the different export function in the same framework, make their arguments, behaviours and styling match at maximum ?Can you think of some ways to make it faster and improve performance further ?

##### Grounding + scope (2026-07-12)

Fresh profile: build + Excel-write are at their FLOOR (§29-§31, §35); the clean high-value work is export INTEGRATION. Split into **Phase 10j-A (export framework, DONE)** + **Phase 10j-B (the one remaining build-perf lever — `tab_apply_tests`/`tab_chi2` base-R marshalling ~22 %, PoC-gated, a separate later session)**. Detail: `dev/tabxplor_phase10_exporters.md` (10j-A Status), decisions §35.

##### Phase 10j-A — Unified export framework (DONE)

Three byte-identical increments (no golden regen; suite 1827/0). New exported **`tab_export(x, format = c("kable","md","xl","plot"), path=, ...)`** facade (`R/tab-export.R`) + shared **`resolve_export_opts()`** (`R/tab-export-prep.R`). The four exporters unified: `color` (monochrome) + `transpose` on all (transpose centralised in `tab_export_prep()`, materialise→transpose); `tab_md(title→caption)` + `tab_xl(print_color_legend→color_legend)` soft-deprecated; `tab_xl` now consumes the prep's two-channel colour SLOTS (deleted its private `fmt_color_channels()` pass) and is **theme-aware**. `fmt_col_ann()` always returns the full monochrome-capable ann (fixed `color=FALSE` on html/plot/xl). `tab_plot()` gained list-method parity (non-mergeable list → list of ggplots). Removed the dead `fmt_frame_fields` constant. New `test-export.R`.

##### Phase 10j-B — build-perf lever: `tab_apply_tests` / `tab_chi2` marshalling (DONE — 2026-07-13)

PoC-first (B-i), then a maintainer-scoped partial rewrite (B-ii). Full record + numbers: `dev/benchmarks/results_1.4.0/phase10j_tests.txt` (+ scripts `phase10j_profile.R` / `phase10j_probe.R` / `phase10j_tests_parity.R`).

- **Profile** (fresh, gss_cat 4×3 factor chi2 fixture): the "~22 %" is real (26 %) but the honest decomposition reframes it — on the tables that cost time the **`agg_chi2` engine dominates** `chi2_compute_test` (73 % on chunky many-subtable shapes; already data.table, not a target). The single biggest CLEAN line was `is_a_mean` (4.6 % by.total) — a per-col_var `dplyr::select(ungroup(tabs))` reconstructing fmt columns just to read the scalar `type` attr.
- **PoC** proved BOTH candidate rewrites **byte-identical (26/26 `identical()`)** across factor/mixed/mean × comp tab/all × 0-2 tab_vars × weighted × 2×2 Yates. Landmine: `agg_chi2`/`agg_anova` DROP degenerate subtables → the live `distinct+left_join` recovers them as NA rows; a byte-identical rewrite must re-implement that shape.
- **LANDED (B-ii): `is_a_mean` → direct `get_type()` read** (`tab_chi2()`, `R/tab.R`). **~3.15 % of the whole `tab()` call** (6.1× on the op, noise-free isolated sum), byte-identical (full suite 1842/0, no golden regen), a genuine simplification.
- **ABANDONED: the `chi2_compute_test` marshalling rewrite** — byte-identity was proven but its ~6 % is engine-capped and forces a base-R re-implementation of `distinct+left_join` (same shape, less readable → not a simplification). The shared `detect_totcols` (<1 %, CI-path risk) was likewise skipped. Build is at its floor (§35).

**Also fixed this session (the flagged `contrib`+`comp="all"` crash → three render bugs):** `grand_totrow()` degrade in `get_mean_contrib()`/`chi2_write_contrib()` (colour engine), NA-safe `cond_ctr` (kable tooltip), NA-safe tab_var blanking (`tab_md`) — see "Last Phase a" above. Byte-identical, +2 colour goldens + an exporter render test.




### Phase 11 – Manual reviews

Final verification that statistical results are the same for tabxplor 1.3.1 (installed CRAN version) and tabxplor 1.4.0, with manual review of the maintainer. Create two Excel files in mirror, with one exact same sheet for each analysis, and in this sheet a first standard table with the revelant colors (often mostly pct display), and a second table with the relevant vctrs field (ex : contrib, chi2, etc.). Each time, the first col_vars is a factor and the second col_vars is a numeric variable. The use cases and calculations to review :
- `tab_vars = <x>, pct = "row", color = "diff"`  # diff of the numeric variable will be different
- `tab_vars = <x>, pct = "row", color = "ratio"` # only 1.4.0, to compare with the former "diff" with numeric variable
- `tab_vars = <x>, pct = "col", color = "diff"`
- `tab_vars = <x>, color = "contrib", comp = "all"`
- `pct = "row", color = "diff", color_signif = "grey_non_signif"` # ci method Agresti-Caffo for comparison
- `wt = <x>, pct = "row", color = "diff", color_signif = "grey_non_signif"` # ci method Agresti-Caffo for comparison # take any numeric var for the weights even if they are not weights.
- `pct = "col", color = "diff", color_signif = "grey_non_signif"` # ci method Agresti-Caffo for comparison
- `wt = <x>, color = "diff", color_signif = "grey_non_signif"` # ci method Agresti-Caffo for comparison
- `pct = "row", ci = "cell"` # ci cell method Wilson
- etc. # what other use cases would be important to review here ?

### Phase 11a — style-name collision fixed

The 1.4.0 review workbook degenerated on **every table after the first** (offset/missing borders, random
font sizes, subtext shown bold+oversized+coloured, first-column level names wrongly bold, numeric colours
absent). **One root cause**: `xl_apply_styles()` reset its name counter + font/fill/border caches **per
table**, re-minting the style names `txf1`/`txl1`/`txb1`/`txx1` in openxlsx2's **workbook-global**
`styles_mgr`, whose `get_*_id(name)` returns the **first** match — so from table 2 on, every table was
painted with **table 1's** style objects. **Fix** (`R/tab_xl.R` only, byte-identical single-table path,
299 export tests green, no golden regen): one **workbook-scoped `xl_style_registrar(wb)`** dedups
fonts/fills/borders/xfs by content + hands out globally-unique names; `xl_apply_styles` became a thin
apply loop threaded through `xl_write_table`. Verified against the 1.3.1 reference across all 15 sheets
(headers sz9-bold [thin,thin]; subtotals bold+left; level names non-bold; subtext sz9-normal-black).
**Secondary findings**: numeric `color="diff"` colours DO apply, just sparse by design (Glass's Δ,
`mean_diff_breaks` — intended Phase 5, ≠ 1.3.1's ratio); factor colour hex differs (Phase 5 palette,
intended); `ci="cell"` shows the raw proportion under Excel `@` text format — a real, separate
numeric-bypass limitation, **deferred** (contained fix: write `format(col)` for `code=="TEXT"` cells).



### Phase 12 — tab_logit integration and full redesign

Integration of `tab_logit.R` (currently commented out) into the package, then redesign and rewrite of `tab_logit` and `multi_logit`, and maybe extension to all `lm` + `glm` regression models inside the same unified framework.
- logit and regression models functions will be introduced in tabxplor 1.4.0 : **no backward-compatibility needed**, but the public API and internal workflows both need to be carefully redesigned for user-friendliness, consistency, performance and future-proofing.

#### Phase 12a – integrate current version in tabxplor framework cleanly (DONE)

The current `tab_logit.R` code, made outside of the package, was a way to use tabxplor vctrs fields former implementation to store the logit data, but the way to do it may have been pragmatic/messy/ad hoc : first, before modifying tab_logit() behaviour, I want to integrate it with the rest of the package.
- Do not hesitate to redesign it thoroughly for consistency with tabxplor package architecture. Fix ad hoc stuffs to make it fits perfectly inside tabxplor framework.
- Do not hesitate to break it into subfunctions when needed, convenient or future-proofing.
- Do ne hesitate to rethink the articulation between `tab_logit` and `multi_logit`, and the internal workflows in general.
- Integrate confidence intervals with the new `ci_inf` / `ci_sup` vctrs fields (check its in fact `exp()` bounds), and also with the new `color_signif` framework (with logistic regression, sensible default may be "grey_non_signif").
- All exports (kable, md, Excel) should work natively with the resulting tabxplor_tab (or grouped one, etc.).

The commented-out draft is now LIVE, clean, first-class tabxplor code (full suite green **1877**, no golden
regen; new `test-tab_logit.R` = 35 tests). Statistical behaviour unchanged (binary logit, 2-level only);
this was a structural + fmt-field integration, not the 12b statistical redesign.

- **Two foundational decisions settled (maintainer-approved; rationale in `dev/tabxplor_1.4.0_decisions.md` §36):**
  (1) **Location = keep inside tabxplor** (no `regxplor` subpackage). (2) **Engine = direct `stats::glm` /
  `survey::svyglm` + `broom::tidy`** — the parsnip/workflows/hardhat/poissonreg stack + the `parsnip:::`
  `svglm2` engine were dropped (parsnip's glm engine only called `stats::glm`), so dropping it REMOVED deps;
  `broom` + `survey` (already Suggests) are the only ones, `requireNamespace()`-guarded. This dissolved the
  "deps too heavy -> split the package" motivation.
- **`R/tab_logit.R` rewritten** (~330 L, was 1009 L of comments): internal `logit_fit()` (glm/svyglm on
  complete cases), `logit_skeleton()` (var/level/term rows), `logit_column()` (align a fit -> one OR fmt
  column), `logit_build()` (shared). `tab_logit(data, dependent, predictors, wt, ...)` = dependents as OR
  columns; `multi_logit(data, dependent, models, ...)` = named models as OR columns (blank where a predictor
  is absent). `R/tab_logit_2.R` emptied (or_plot/lm_plots deferred; `git rm` pending — rm was blocked).
- **Inference `method`** (arg, default `"wald"`; researched — decisions §36): `"wald"` = in-house Wald CI
  `exp(coef +/- crit*se)` (glm z, svyglm t w/ design df) + Wald p — universal software default, only option
  for weighted `svyglm`, one fit, dual-clean. `"profile"` (opt-in, unweighted glm; needs MASS) = profile CI
  (`stats::confint`) + per-coefficient **LR-test** p — more accurate small-sample; weighted -> Wald + message.
  Both keep **CI <-> stars EXACT duals** (NOT broom's `conf.int`, which switches to profile when MASS loads).
  Parity verified vs hand-run glm/svyglm (OR/CI/p). svyglm design-based SEs answer the 12b weight-inflation
  concern for the *inference* (normalization policy stays 12b). **`color_signif` arg** (default
  `"grey_non_signif"`; opt-in `"ignore"`/`"color_all_signif"`) drives OR colouring via the existing attribute.
- **OR columns are ordinary fmt** (no new type): `type="row"`, `display="or"`, `color="OR"`,
  `color_signif="grey_non_signif"`, and a **new `ci_type="or"`** (log-OR Wald exp() bounds, multiplicative
  neutral 1). Four localized `fmt_class.R` reader patches (all inert for non-OR): `set_ci_type` enum `+"or"`;
  `ci_center()` OR branch (centre = the OR); `fmt_color_plan()` significance gate tests **exclusion of 1**
  for the `"or"` measure (was hard-coded exclusion of 0); `format()` `disp_or` adds **`1/OR`** reciprocal
  display for OR<1 (`0.25 -> "1/4"`, everywhere incl. empirical OR — byte-identical for OR>=1) + a no-pct
  guard so a pure model-OR ref row shows bare "1". Stars/colours light up automatically from the written
  `pvalue`/bounds. Excel keeps the raw OR number. Follows `/vctrs-field` + `/color-mode`.

**Deferred (later phases, unchanged):** multinomial / 3+ level, weight-normalization policy, lm/glm +
`tab_reg` (12b); tidyselect + named-vector ref levels, contrasts (12d); `or_plot` forest plot + `lm_plots`
(display phase); visible OR CI bracket / OR+ME/OR+PCT layouts (12b/12d); `jmvtab_logit` UI (12e).


#### Phase 12b – design choices and statistical framework (DONE)

See details in `dev/tabxplor_1.4.0_decisions.md` §37.

 Grounded in 4 web-research passes + a git study of the pre-package draft + 2 maintainer decision rounds. Settled (built in 12d):
- **Unified `tab_reg(data, dependent, predictors, family=, effect=, wt=)`** over ONE glm/lm/svyglm/multinom/polr engine; `predictors` = char vector (one model; `dependent` may be a vector → column per dependent) OR a named list (comparison mode → column per model). `tab_logit`/`multi_logit` kept as binomial wrappers (same UX). Effect per family, per-column label: gaussian → β (fmt `diff` shape, neutral 0), binomial → OR, poisson/negbin → IRR (fmt `or` shape, neutral 1); `exponentiate="nongaussian"` default. **fmt already carries both shapes → ~no new fields.**
- **Family from the outcome, explicit** (only 0/1→binomial auto). **Summed-score integer 0..q → grouped binomial** (`nb_questions`/`trials`, reinstated) — the only place quasibinomial + dispersion apply; count → poisson (+quasipoisson/glm.nb); continuous → gaussian.
- **Nominal ≥3 → ONE MNL** (`nnet::multinom`), `exp(coef)` labelled "OR (j vs ref)" (= RRR, Begg-Gray; efficient + honest); any reference level / pairwise from one fit. Flavours: default j-vs-ref OR; opt-in "j vs rest OR at reference profile" (adjusted, delta-method CI); AME + predicted probabilities. **Ordered ≥3 → proportional-odds** (`MASS::polr`) default, **diagnosed** (Brant/LR test + warn + MNL/partial-PO fallback).
- **`effect=` mode** (⟂ family): `"coefficient"` (default) vs `"ame"` (AME — sample-average, the Mood-2010 standard; prob-points/count/β per family; needs model+data+vcov; base `predict()` points, `marginaleffects` Suggests for SEs). MER-at-reference opt-in (the old draft's ME was MER; default switches to AME).
- **Survey weights → `svyglm` always** (design-based, scale-invariant → fixes non-normalised weights, matches weighted crosstabs). Accept weight column + optional ids/strata/fpc + a prebuilt survey.design; no normalisation; glance degrades (Wald/regTermTest, psrsq, Rao-Scott AIC).
- **Unified model/test-summary footer**: generalise the `test` attribute (shared by `tab()` + `tab_reg`); default N + LR-vs-null + McFadden R² + AIC/BIC (lm: R²/adjR²/F); dispersion flag for poisson/grouped-binomial; multi-model LR vs null (opt-in vs baseline / sequential); in-cell test label (`"2.9% (Chi2)"`); border box per model block; shared `stats=` arg.
- **Formulas**: tidyselect default + a formula escape-hatch for experts. **Reinstate** `split_var`, `multiplicator`, `empirical_OR`; `or_plot`/`lm_plots` deferred to a display phase. **Deps**: nnet+MASS (Recommended, free) + broom/survey (Suggests); `marginaleffects` the only new Suggests.

The build below is re-cut (2026-07-13) from the settled §37 design. **A Phase = a fresh Claude Code session**; **increments (i/ii/…) = commit-and-verify pauses inside one session** (do the whole session at once if it fits). Every build Phase's verification gate = **statistical-soundness parity** vs base `glm`/`lm`/`svyglm`/`nnet::multinom`/`MASS::polr` (unweighted + survey) with green goldens — this folds the old standalone "tests" phase into each Phase. Final release gate = `devtools::check()`.

#### Phase 12c – `tab_reg` core engine + effect columns (DONE)

The foundational rewrite: ONE internal engine (family dispatch) + the shared effect-column machinery, reusing the fmt additive-`diff` / multiplicative-`or` shapes (≈ no new fields). `tab_logit`/`multi_logit` become binomial wrappers.

##### Phase 12c-i – unified engine for tab_reg() (DONE)

- engine skeleton; **binomial at parity with 12a** (`tab_reg(family="binomial")` ≡ `tab_logit`); `predictors` char-vector vs named-list dispatch; **tidyselect** variables + **per-variable reference levels via named vectors**.
- **gaussian (lm → β)** + **poisson (IRR)**; per-column effect labels (β/OR/IRR); `exponentiate="nongaussian"`.

`R/tab_logit.R` → **`R/tab_reg.R`** (family-generic engine; old file + `tab_logit_2.R` emptied, `git rm` pending — `rm` blocked). **`tab_reg(data, dependent, predictors, family=, exponentiate="nongaussian", wt=, reference=, method=, color=, color_signif=, …)`** over ONE engine (`reg_check_deps`/`reg_detect_family`/`reg_prep_binary`/`reg_apply_references`/`reg_skeleton`/`reg_fit`/`reg_lr_pvalues`/`reg_column`/`reg_build`): `stats::lm` (gaussian) / `glm` (binomial/poisson) / `survey::svyglm` (weighted) + `broom::tidy`. `predictors` char-vec = one model (dependent may be a vector → column per dependent); named list = model comparison. `exponentiate` drives the fmt shape: **additive β** → `diff`/type="coef"/display="coef"/ci_type="diff"/color="diff"; **multiplicative OR·IRR** → `or`/type="row"/display="or"/ci_type="or"/color="OR". CI ⇄ p exact duals (z for fixed-dispersion glm, t(df) for lm/quasi/svyglm; profile+LR opt-in). `tab_logit()`/`multi_logit()` are thin binomial wrappers (curated UX unchanged; `test-tab_logit.R` still green).
- **fmt integration (the maintainer's `type` question):** ONE new `type` VALUE **`"coef"`** (gaussian β only) + ONE new `display` TOKEN **`"coef"`** (raw signed render, no ×100/%/×) + **reuse the `var` field** for var(Y) (the β/SD(Y) effect-size colour, standardized like a mean-diff but by its OWN `var`, not `get_ref_var()`). **No new fmt fields, no new attributes** (18/9 contract intact). OR/IRR stay on the proven `type="row"` path. Also: `fmt_color_plan()` now excludes reference rows from colouring for the diff/ratio/or measures (`gate & !is_refrow`) — byte-identical for crosstabs (their ref cells are diff=0/OR=1 → already slot 0), it uncolours the regression **intercept** (in_refrow but a non-neutral baseline). Excel unchanged (a `coef` cell hits `excel_numfmt_code`'s plain-number branch).
- **β colour = effect-size gradient** (maintainer decision): β/SD(Y) vs the `mean_diff` (Cohen 0.2/0.5/0.8/1.2) breaks — verified end-to-end (a large standardized β colours, a tiny-but-significant one stays grey). Family `"auto"` detects only binary→binomial / continuous→gaussian (message), else aborts (§37 D2).
- **Verified:** full suite green (FAIL 0, PASS 1927), incl. colour/golden byte-identity; new `test-tab_reg.R` (β/CI/p vs lm, IRR/CI/p vs glm, coef shape, `exponentiate`, `reference`, family-auto, colour, exports); `test-tab_logit.R` (binomial wrapper) unchanged & green. `devtools::document()` done.
- **Done in 12c-ii (2026-07-13):** summed-score grouped binomial (`trials`) + the formula escape-hatch; contrasts = no-op (already `reference=`). **Known cosmetic (Phase 13 legend redesign):** the β legend shows the SD breaks as `%`, and the IRR legend says "OR"; a `coef` md cell reuses a `pXX` class name (self-consistent, no collision — regression tables aren't mixed with pct columns).

##### Phase 12c-ii: summed-score grouped binomial, formula escape-hatch (DONE — 2026-07-13)

- **`tab_reg(trials=)`** (binomial only): a summed-score outcome `0..q` → `glm(cbind(score, trials-score)
  ~ ., binomial)` (weighted → svyglm quasibinomial), reusing the OR/`or` fmt shape. `NULL` = binary logit,
  an int / per-dependent named vector, or `TRUE` (observed max). Label `"<dep>: OR"`; `exponentiate=FALSE`
  → β shape. Parity vs hand `glm(cbind(...))`.
- **Formula escape-hatch**: `dependent` accepts a model formula (`predictors` now defaults `NULL`; exactly
  one of the two). `reg_parse_formula()`: a **simple** `y ~ a + b` reduces losslessly to the char path
  (`identical()`); a **compound** one (interactions / `poly()` / `I()`) is fit verbatim with a best-effort
  skeleton from the fitted terms (`reg_skeleton_from_fit()`). `reg_build()` refactored fit-all→column-all;
  `reg_fit()` returns `$fit`.

#### Phase 12d – nominal & ordinal 3+ level outcomes (DONE — 2026-07-13)

Both families added to the SAME engine, byte-identically reusing the OR/`or` fmt shape (no new fmt
fields/attributes/tokens/color branches); full suite green (1949), NO golden regen. Detail:
`dev/tabxplor_1.4.0_decisions.md` §37 "12d DONE".
- **nominal ≥3 → ONE `nnet::multinom`** (`reg_fit_multinom`): `exp(β_j)` = "OR (j vs reference)" —
  `reg_build` splits the `y.level` tidy into **one OR column per non-reference category** (`"j vs ref:
  OR"`). Outcome baseline = the outcome factor's first level, set via `reference` keyed on the
  DEPENDENT (`reference = c(partyid = "Independent")`; MNL only). `tab()` empirical-OR **terminology**
  aligned (prose only: "relative risks ratio" → per-level OR vs the reference).
- **ordered ≥3 → `MASS::polr`** (`reg_fit_ordinal`): one **cumulative-OR column** (cut-point rows
  dropped → "Constant" NA). **Brant PO diagnostic** (`reg_ordinal_diagnostic`, via the `brant`
  Suggests) warns on violation; a missing `brant` skips with a hint. Landmine: brant rebuilds the model
  frame via `eval.parent(fit$call)` → the diagnostic self-heals the (copy) fit's `$call$data`/`$formula`
  so it works out of the fitting scope.
- Shared `reg_wald_from_tidy` (qnorm Wald) keeps CI ⇄ p ⇄ stars exact duals for both. `family="auto"`
  detects ordered→ordinal / unordered-factor→multinomial. `nnet`+`brant` → Suggests.
- **Deferred (per maintainer this session):** the **"j vs rest OR at reference profile"** flavour
  (adjusted, delta-method CI) → **12e** (shares the AME / `marginaleffects` machinery); weighted
  MNL/ordinal (svyolr; guarded error now) → **12g**; joint-vcov pairwise / partial-PO fallback (later).

#### Phase 12e – AME / predicted-probability interpretation mode

The orthogonal `effect=` axis (`"coefficient"` default vs `"ame"`).

##### Phase 12e-i – sample-average AME + adjusted predictions (DONE — 2026-07-13)

`tab_reg(effect = "ame")` shows the sample-average **marginal effect** with the adjusted **predicted probability** in parentheses, AME-first (`-8%*** (16%)`). **marginaleffects sole engine** (new gated Suggests): `reg_marginal()` wraps `avg_comparisons()`/`avg_predictions()` (RESPONSE scale, `newdata` = the fitted frame REQUIRED; factor AME keyed by `(var, level)` from the `"Level - Reference"` contrast label; `wts` = the weight column so a weighted AME is population-weighted, matching §14). `reg_marginal_column()` composes via the Phase-10i-A `{}` grammar (AME-first → stars ride the primary token natively, **no `fmt_class.R` change**): prob-scale (binomial/MNL/ordinal) → `type="row"` + `"{diff} ({pct})"` / reference `"({pct})"` / numeric `"diff"`; gaussian/poisson → raw `type="coef"` (+ `var`=var(Y)); Constant/out-of-model → `"blank"`. MNL/ordinal → **one AME column per outcome CATEGORY** (all levels). **No new fmt fields/attributes/tokens; `effect="coefficient"` byte-identical (no golden regen)**; full suite green (533 blocks). Parity locked vs marginaleffects per family + weighted svyglm (`test-tab_reg.R`). Detail: `dev/tabxplor_1.4.0_decisions.md` §37 "12e-i DONE".

##### Phase 12e-ii – opt-in marginal effect at reference (DONE — 2026-07-13)

New `at = c("average", "reference")`. `at="reference"` evaluates at the **reference profile** (other predictors at their reference = factor first level / numeric mean) via `marginaleffects::datagrid()` → `comparisons()`/`predictions()` (single row, no averaging/weights): `effect="ame"` → the marginal effect at reference (**MER**, label AME→MER) + adjusted prediction there; **MNL** `effect="coefficient"` → the **"j vs rest" OR at the profile** (`comparison="lnor"` → exp, new `reg_marginal_column()` `shape="or"`, one `or` column per outcome category). `at` no-ops on ordinary coefficients (profile-independent → message). Maintainer forks: reference-level baseline (documented odd-baseline caveat); include j-vs-rest OR now. **No new fmt fields; `at="average"` byte-identical to 12e-i; no golden regen; full suite green.** Parity locked vs marginaleffects at the datagrid (`test-tab_reg.R`). Detail: `dev/tabxplor_1.4.0_decisions.md` §37 "12e-ii DONE". (Deferred: custom `newdata=`/"typical"-mode baseline; empirical j-vs-rest on `tab()`.)

#### Phase 12f – unified model/test-summary footer + model comparison (DONE — 2026-07-14)

- **generalise the `test` attribute** → shared GOF/summary vocab; `tab_reg` footer (N / LR-vs-null / McFadden R² / AIC / BIC; lm: R²/adjR²/F/σ); **dispersion flag** (poisson / grouped-binomial). Crosstabs byte-identical.
- **multi-model comparison** (`compare = baseline / sequential`; LR / F; Δ-AIC + message when non-nested / N differs).
- **unified rendering**: in-cell test labels (`"2.9% (Chi2)"`); console block (`print_reg_footer`) + export rows (`reg_footer_lines`) + border box (whitelist); shared **`stats=`** arg.

`tab_reg` tables gained a **model-summary footer** + **model comparison**, and `tab()` crosstab p-value cells gained **in-cell test labels** — all stored in ONE `test` attribute with DISJOINT reg discriminators (so the crosstab renderers ignore the reg rows and vice versa → **crosstab `.rds` goldens byte-identical, no regen**; only `_snaps/render-html.md` re-accepted for the `(Chi2)` label).
- The footer is **display-only** (built object = coefficient skeleton).
- `reg_glance`/`reg_gof_tibble`/`reg_footer_stats`/`reg_compare_rows` (R/tab_reg.R) + `reg_footer_spec`/`print_reg_footer`/`reg_footer_lines`/`pvalue_line_fmt(label=)` (R/tab_classes.R) + ONE new fmt display token **`"gof"`** (forced uncoloured, R/fmt_class.R).
- `stats=`/`compare=`/`baseline=` args; `compare` default `"none"` (lr_null already in the footer)
- Weighted footer minimal (survey Wald/Nagelkerke/AIC; full glance → 12g).


#### Phase 12g – survey design + reinstated companion features (DONE — 2026-07-14)

Four increments, byte-identical for unweighted / no-new-arg calls (NO golden regen); full suite green (2194). New Suggests `svyVGAM`. Detail: `dev/tabxplor_1.4.0_decisions.md` §37 "12g DONE".
- **12g-i survey designs**: `tab_reg(wt=, ids=, strata=, fpc=, nest=)` builds a `survey::svydesign` per model (`reg_make_design`); a **prebuilt `survey.design`/`svyrep.design` passed as `data`** is subset()'d per model (`reg_subset_design`/`reg_resolve_design`, `reg_relevel_design` for `reference`). `reg_svyglm_env()` binds `survey::svyglm` into the fit's formula env so `AIC.svyglm`/`anova.svyglm` work unattached (fixed a silent-NA-AIC bug + the length-3 `AIC.svyglm` vector via `reg_aic_value`). Reduced weighted glance = n / Wald-vs-null (`regTermTest`) / Nagelkerke (`psrsq`, + selectable `cox_snell_r2`) / Rao-Scott AIC; weighted comparison via `anova.svyglm` Wald (`compare_*_wald`).
- **12g-ii weighted 3+ level**: guard lifted — ordinal → `survey::svyolr` (positive-weights hint on failure), nominal → `svyVGAM::svy_vglm`; both reuse OR/`or` shape; `effect="ame"`/MNL `at="reference"` refused for weighted. (`svyVGAM` MNL parity is skip-guarded — not on the Rscript libpath here.)
- **12g-iii `split_var`**: the `tab_vars` analogue — `reg_build` recurses per group on a shared skeleton and stacks into a grouped_tab `(split_var, var)`; **`tab_spread(split_var)` works with NO `tab_spread` change** (split_var placed first → `levels` stays row_var); console footer group-aware, export footer skipped for splits.
- **12g-iv `multiplicator`** (`c(var=k)`, numeric predictors → OR^k / β·k, p unchanged) + **`empirical_OR`** (single binary logit → `Emp. %`/`Emp. OR` from a direct weighted 2×2, `reg_empirical_or`).


#### Phase 12h – regression display phase

`or_plot` (OR forest plot, finalfit-style, already used in tab_logit.R tab_logit2.R gited drafts), `lm_plots` (2×2 glm/lm diagnostics),
The visible OR-CI bracket.
The OR+ME / OR+PCT composite cell layouts.
Excel numFmt-literal in-cell test label.
Per-group export footer for split tables (per-group GOF is in get_test())







### Phase 13 – Finalise display and colors API

Final redesign of color palette and color breaks management.

#### Phase 13a – Colors and breaks API final redesign (DONE)

Full colours/breaks redesign; suite GREEN (0 fail / 0 error, run with `NOT_CRAN=true` so snapshots fire), NO fmt field/attribute change (18 fields / 9 attrs intact). Governing decisions: this session's four forks. Files: `R/tab_classes.R` (config + palettes), `R/fmt_class.R` (engine), `R/tab.R` (arg parse + per-table breaks), exporters + legend.

- **`color` grammar = position×names.** `normalize_color_spec()`/`resolve_col_measures()`/`finalize_one_col()` (R/tab.R): **position = channel** (1st→text, 2nd→background), **names = column type** (`pct`/`mean`). Forms: `FALSE` / `TRUE` (smart per-type) / scalar / positional `c("diff","ratio")` / named `c(pct="diff", mean="ratio")` / `list(pct=c("diff","ratio"), mean="ratio")`. The old `c(text=,background=)` channel-name form is REMOVED. spec = `list(mode, legacy, text, bg, types, signif)` (`legacy` still drives the pipeline ci/chi2). **LANDMINE fixed**: `color="auto"` (internal default) must map to `legacy="auto"` in `legacy_union()` — else numeric fields lose ci.
- **Breaks notation.** Canonical scale reshaped `list(pos,center,strict,std)` → **`list(center, strict, std, over=list(breaks,slots), under=list(breaks,slots))`** (both sides POSITIVE magnitudes; engine folds + findInterval per side). Input = signed/reciprocal literals (one-sided auto-mirrors, two-sided as-is, `NA` skips a slot) OR `list(over=,under=)` (no mirror; omit a side = off, e.g. `pct_ratio=list(over=2)` = the "only x2" rule, the new factor default). Order-robust. `mk_color_scale`/`parse_color_side`/`intensity_slots` (drop 2nd→4th→1st) in R/tab_classes.R; deprecated `pct_breaks`/`mean_breaks`/`contrib_breaks` args DROPPED. New numeric default `mean_ratio=list(over=c(1.15,1.5,2,4), under=c(1.5,2,4))`.
- **`color_signif` rename** `"color_all_signif"` → **`"guaranteed_effect"`** everywhere.
- **COMPAT shims (Phase 13a, grep `COMPAT (Phase 13a)`)** — the removed surface degrades with NO error via thin entry-point shims + `lifecycle::deprecate_soft`: `set_color_style()` (→ options + `set_color_palette()`; `custom_palette`→4+4, `html_24_bit` inert), `color = c(text=,background=)` (→ positional), `color_signif = "color_all_signif"` (→ `guaranteed_effect`), `set_color_breaks(pct_breaks=/mean_breaks=/contrib_breaks=)` (→ new scales), inert `html_24_bit`/`...` on `get_color_style`/`fmt_get_color_code`. Locked by a test block in `test-color-config.R`.
- **Per-table `color_breaks=`** arg on `tab()`/`tab_num()`/`tab_many()` (table attribute set LAST; `push_color_breaks`/`pop_color_breaks` install it transiently at print + each exporter; robust fallback to global). NOT threaded through dplyr (a heavy chain drops it → global fallback, documented).
- **Engine.** `fmt_color_plan`/`fmt_color_slots`/`fmt_color_channels`: per-direction over/under breaks+slots; **x2/slot-11 override REMOVED** (ratio-on-bg replaces it); slot domain now **0 / 1-4 over / 5-8 under** (an 8-hex palette). `build_slots`/`color_slot_table` deleted. `pillar_shaft.tab_chi2_fmt` + `print_chi2`/`print_reg_footer` use `cs[[8]]`/`cs[[4]]` (unnamed palette).
- **Palettes.** 8 OKLCH base palettes (`default_*_colors`) → `tabxplor_palette_env` via `build_palettes()`; new **`set_color_palette()`** replaces `set_color_style()`/`custom_palette`. `get_color_style(mode,type,theme)` = 8-vec (4 over + 4 under); **24-bit console default**, curated 8-bit (`palette_8bit`) only in RStudio; exports always 24-bit. `html_24_bit` REMOVED internally, kept inert on exporters. Old 6×11-slot palettes + green_red/blue_red variants deleted.
- **Legend + exporters** (`tab_color_legend`, `fmt_channel_codes`, `md_slot_class_map`, tab_kable/xl/plot) rewired to the 8-slot palette + over/under; no x2 row.
- **Goldens**: `_color_golden/*.rds` regenerated (new palette + x2 removal); `_snaps/render-html.md` + `golden.md` accepted (conscious colour change); structure `.rds` + `test-fmt-contract` byte-identical. Tests: `test-color-config.R` / `test-color-engine.R` rewritten to the new API. **Deferred to 13b/13c**: colour legends redesign + French; light/dark kable CSS. **jmvtab**: `color_all_signif`→`guaranteed_effect` renamed in `.a.yaml`/`.u.yaml`/`.h.R`; the new `color`/breaks grammar is NOT yet wired into the jmvtab UI (a maintainer `jmvtab.h.R` regen + a jmvtab wiring pass are open).

##### History

**Redesign the more simple and user-friendly color and breaks management system possible, without thinking about soft-deprecating anything**.

Would it be possible / consistent to add this possibility:
`color = c(pct="diff", mean="ratio")`, to have diff for factors and ratio for means, passing internally the c("diff", "ratio") color while passing empty breaks for the not wanted ones ? If it’s a white elephant tell me.
- How are breaks passed in tab()/tab_many() handled, are they written as a per-column attribute, or was there another solution ? I can feel this part of the design was a bit shaky.
- Replace `color_signif = "color_all_signif"` with `color_signif = "guaranteed_effect"`, clearer. Need to be changed in `dev/` documentation about colors UI too (no traces of the old name altogether = better).

I want to change the current default color palettes system, to simplify it a lot. My new oklch color palettes, one made for Light mode and one made for Dark mode, are now saved in `tab_classes.R` "## NEW COLOR PALETTES (to wire to the code) ----" as : `default_text_colors`, `default_text_colors_neg`, `default_dark_text_colors`, `default_dark_text_colors_neg`, `default_background_colors`, `default_background_colors_neg`, `default_dark_background_colors`, `default_dark_background_colors_neg`.
- Text and background variants have been made to be usable together in a readable way (`color = c("diff", "ratio")`).
- At package load, I want to assign then to corresponding objects, but I want the user to be able to customise these objects using a special `set_` function, overwriting the crayon objects so they are only computed once (and not for each table or, worse, cell) : `text_colors`, `text_colors_neg`, `dark_text_colors`, `dark_text_colors_neg`, `background_colors`, `background_colors_neg`, `dark_background_colors`, `dark_background_colors_neg`
- Assume, and state in documentation, that they will always be **4 positive colors and 4 negative colors** in all color palettes, for simplification. They it’s a break choices that users state if they want only 3 or 2 or 1 colors for each side.
- Positive and negative colors are now separated in two different vectors for clarity.
- The `pos1`, `pos2`, `pos3` etc. names are removed : they introduced a useless complexity and some friction. Now, only rely on position.
- Instead of The 24 bits versions should be default : only fallback to 8 bits for consoles that do not handle 24 bit colors. Remove the old useless 24 bits palettes in `tab_classes.R` "## OLD COLOR PALETTES (TO DEPRECATE) ----" : only keep the 8 bits ones, that are needed for use inside RStudio R console (Positron IDE have 24 bits), remove their first value pos1/neg and their ratio value if still here, and fallback to them only if the user is in RStudio IDE (is there a way to reliably detect that ? If there’s a way to detect Positron or VScodiumi-based-IDE instead to go 24 bits in console, it’s also possible).
- Simplify the get and set color styles functions. Remove `html_24_bit = c("blue_red", "green_red", "no")` argument : default to 24 bit, fallback to 8 bits on RStudio. We only one palette (pos + neg) for each combination of light/dark and text/bg.

Color breaks and color palette management is very not user friendly (base is ok, but customisation for expert users is unclear)
- Now the `color = c("diff", "ratio")` argument is the right way to have both additive and multiplicative color helpers. So `color_style_` objects should now work without the old "ratio" value, and these `ratio` values should be removed from the `color_style_` entirely in the code.
- in set_color_breaks(), it’s not currently possible to also provide "negative" breaks (under-represented side) to pass asymmetrical breaks (asymmetrical both in number of breaks and breaks values). That would be necessary, since with `ratio`, breaks >1 are sound, but breaks <1, when they are close do 1 (like 1.2), so the user may want to use fewers breaks on this side
- The default for factors is `pct_diff = c(2)`, but it implement both a x2 and a /2 rule : f want to be able to enforce my classic `only x2` ratio rule, I want a possibility to force asymmetrical breaks, for example `pct_diff = c(2, asymmetrical=TRUE)` (with should be default to factors ; default for numeric variables should be `mean_ratio = c(-1.5, -2, -4, 1.15, 1.5, 2, 4)` )
- I would prefer the order to micmic that of the color palettes : first the negative values from closest to bound to farthest to bound, then the positive level from closest to bound to farthest to bound. If the order is wrong, the code should attempt to order it in the logical way for color management anyway.
- The way I want it done : the user directly provides a list of breaks with possibly named arguments giving the color, either via set_color_breaks() or breaks argument (to be renamed `color_breaks` ; keep `breaks` soft-deprecated if it was already on tab() in 1.3.1, remove altogether if it was not) ; with `color_breaks` argument, the breaks are stored as a vctrs based column-level arguments ; with `color_breaks` argument, at display or export, tabxplor internally create a temp object with the vector, and compute the crayon styles too (both must be created and loaded first, and shall never be recreated for each cell in a table). Can you see some possible caveats here ? Some further simplifications ?
  + For example `color_breaks = list(ratio_breaks = c(1/1.5, 1/2, 1/4, 1.2, 1.5, 2, 4))` (3 under-represented, 4 over-represented). Here no names provide colors, so base palettes are used.
  + Since color palettes have always 4 positive colors and 4 negative colors, passing `NA` should indicate which color is not used. Ex :  `color_breaks = list(ratio_breaks = c(NA, 1/1.5, 1/2, 1/4, 1.2, 1.5, 2, 4))` (here negative color 1 is not used). When no NA is provided to tell what colors are used exactly, a graceful fallback should select colors nonetheless (first excluding color n°2, then color n°4, then color n°1).
  + In `set_color_breaks()`, deprecate the old arguments (`set_color_breaks(pct_breaks = ...)`) and add the new ones (like `set_color_breaks(pct_diff = ..., pct_ratio = ...)`).
  + Providing the names to override the palette, but of course that can only be done in positive + negative mode : `color_breaks = list(pct_diff = c("#cb0000" = -30, "#ff3d00" = -20, "#FF8138" = -10, "#ffb300" = -5, "#C7D62C" = 5, "#83BB3F" = 10, "#3BA240" = 20, "#1b6e20" = 30))` ?  Of course if at least one is provided, then a name should be provided for **all** breaks (error otherwise). Would it be reliable or another white elephant ?
  + The function then auto-detects where is the boundary between over-represented and under-represented depending on the type (0 for additive, 1 for multiplicative), and handle robustly the creation of the relevant interval for each color, taking into account where is the boundary.
  + only giving the positive / over-represented side should still mirror it depending of the type, minus sign for additive and 1/x for multiplicative).
  + Rule should be : if no `color_breaks` are saved as column-level attributes (not NULL or empty) the current ones are used (so the user can save a table, load it in a fresh session, and use set_color_breaks() to choose how to display) ; if some `color_breaks` already exist at column-level they override any package level settings (to change that, the user can still remove columns attributes manually).
  + Make testthat tests to ensure it handles edge cases, and user’s errors or imprecisions (ex. not ordered) well.
- The aim is to **simplify** : please remove traces of the old implementation altogether, we do not need to soft-deprecate everything here (very small user-base + I think nobody ever used it).

#### Phase 13b – meaningful color legends (DONE)

Color legends
- Redesign color legends for simplicity : they should be understandable by non-experts, while at the same time having just the enough technical terms for the experts to know exactly what’s happening technically here.
- Even in Excel export, use styles inside the color legend cells to color the breaks with the relevant text or background color (+bold), to make it really usable (otherwise a legend that does not say what color is what is incomprehensible), while keeping the rest of the text in the cell black (+ plain).
- Make the color legend more easy to read for  non-expert users, and implement a French translation (detect OS language + override by optional argument in export functions ?) Here are meaningful exemples  in French, to generalise and translate (in every case, of course, it is only meaningful if each break is of the same color than in the table). They can be written via a script, knowing : ligne/colonne, reference Total or level name, type of ci ; and a string "Nuances de bleu"/"Nuance du jaune au rouge" that can be baked with tabxplor default color palette, and have a fallback to not saying which color it is in the sentence with custom palettes ?
  + pct diff : "Nuances de bleu pour les cases >= à la ligne Total +5; +10 ; +20 ; +30 points. Nuances du jaune au rouge : <= à la ligne Total -5 ; -10 ; -20 ; -30 points."
  + pct diff,  `color_signif="color_all_signif"` : "Nuances de bleu pour les cases >= à la ligne Total +0; +5 ; +15 ; +25 points, après soustraction de la marge d’erreur (intervalle de Wald avec ajustement d’Agresti et Caffo, seuil de confiance à 95 %). Nuances du jaune au rouge : <= à la ligne Total -0 ; -5 ; -15 ; -25 points. Grisé : chiffre non significativement différent de celui de la ligne Total après marge d’erreur."
  + ratio : "Nuances de bleu pour les cases >= à la colonne Total ×1,15 ; ×1,5  ; ×2 ; ×4. Nuances du jaune au rouge pour les cases <= à la colonne Total ÷1,15 ; ÷1,5 ; ÷2 ; ÷4."
  + OR, `color_signif="grey_non_signif"` : "Nuances de bleu : OR >= 1,15 ; 1,5 ; 2 ; 4. Nuances du jaune au rouge : OR <= 1/1,15 ; 1/1,5 ; 1/2 ; 1/4. Grisé : chiffre non significativement différent de celui de la modalité de référence (intervalle de Wald avec ajustement d’Agresti et Caffo, seuil de confiance à 95%)."
- Integrate `tab_reg()` into the colors legends reliably and usefully for the user. Currently the β legend shows the SD breaks as %, and the IRR legend says "OR".

`tab_color_legend()` (`R/fmt_class.R`) rewritten into a **token-stream**: `legend_specs(x)` (per col_var
group) -> `legend_tokens_terse`/`_prose` -> `legend_render_line(medium)` (console crayon / html
`text_spec` / md pandoc span / **excel `openxlsx2::fmt_txt` runs** / plain). **Console = terse compact,
exports = readable prose**; break-word colours come from the SAME 8-slot palette the cells use.
**French translation** via `gettext`/`gettextf` (domain `R-tabxplor`, `po/R-fr.po` filled + compiled to
`inst/po/fr/LC_MESSAGES/R-tabxplor.mo` via `tools::update_pkg_po`; `bindtextdomain` in `.onLoad`): auto
from R/OS locale (English fallback) + a `lang` arg (`"en"`/`"fr"`, sets the `LANGUAGE` env for the build,
NOT `Sys.setLanguage()` — R>=4.1) on `tab_kable`/`tab_md`/`tab_xl`/`tab_plot`/`tab_export`; FR typography
(space before `; :`). The CI method + level are named from a NEW display-only **`ci_settings`** table
attribute (`list(conf_level, method_cell, method_diff)`, set in `tab_assemble_tables`, carried through
dplyr like `render_extras`; `default_ci_settings()` fallback). Shade names ("blue"/"yellow-red") only for
the default palette (`legend_shade_names()`), else generic. **Excel legend cells** are coloured rich-text
(`xlb_write_richtext`, `R/tab-xl-backend.R`); **`tab_md()` gained a colour legend** (break-words in the
same pandoc classes as the cells). **tab_reg fixed**: β shows SD/Glass thresholds (not `%`), IRR says
"IRR" not "OR" (effect word from the column-name suffix, gated by `is_reg_footer`). Cell colours
UNCHANGED (`test-color-golden.R` green); conscious regen of `_snaps/golden.md` + `_snaps/render-html.md`
(legend-only) + the 4 CI `_golden/*.rds` (only the `ci_settings` attr added). `test-color-legend.R` (43).



#### Phase 13c – Exports and display improvements (DONE)

Six sub-phases, full suite green (2285), goldens regenerated consciously (`golden.md` + `render-html.md`:
composite padding, partial bold, spanning headers, suffix-stripped level names). No fmt field/attribute
change.
- **13c-i display core** (`R/fmt_class.R`, `R/tab_classes.R`, `R/utils.R`): composite `{}` tokens
  right-padded to a uniform per-column width (`100% (n=  849)`); ratio (`rr`) display shows a multiply /
  divide sign (`x2` / `/2` = the divide sign over `1/ratio`, new `div_sign`; text backends only — Excel
  stays numeric); the kable/console **ratio tooltip** now formats the `rr` field under a `ratio:` label
  (was the empty `or` field). Stars already right-pad/align (verified).
- **13c-ii composite partial bold** (`R/fmt_class.R`, `R/tab_md.R`, `R/tab-render-html.R`): a bold
  row/col bolds only a composite cell's FIRST field (`**100%** (n=…)`). `format(bold_split = TRUE)`
  attaches a per-cell `primary_nchar` attr (default off -> attribute-free / byte-identical); md wraps the
  prefix (`md_bold`), both html engines wrap the suffix in a `font-weight:normal` span (`html_cell_text`;
  `cell_spec(escape = FALSE)` proven byte-identical to `escape = TRUE`). **Excel N/A** — composites there
  resolve to their primary numeric value, no string.
- **13c-iii col_var spanning headers + suffix stripping** (`R/tab-export-prep.R` shared
  `tab_col_var_header()` -> per-column `label`/`clean` + `tab_header_runs()`): the col_var NAME spans its
  level columns (a Total column stands alone); level names drop the `_<col_var>` disambiguation suffix.
  md spanning row shown for a single col_var too (a VISUAL title row -> fewer pipes; the pipe-grid tests
  were scoped to the level-header + separator + data); kableExtra `add_header_above` + `col.names`; html
  engine `<thead>` colspan row; Excel span row + `xlb_merge` (below).
- **13c-iv `tabxplor_tabs` list class** (`R/tab_classes.R`, wrapped at `tab()`/`tab_many()` return): an
  S3 list (inherits `"list"`; `print`/`[`/`c`/`knit_print`) for multi-table results -> auto-prints like a
  single tab (honours `options("tabxplor.print")`) and `list |> tab_kable()` routes to the Viewer
  (`tab_kable_join` gives the joined kableExtra-engine HTML the `kableExtra` class). `is.list`/`[[`/`map`
  unaffected; a single tab is returned bare.
- **13c-v Excel** (`R/tab_xl.R`, `R/tab-xl-backend.R`, `R/fmt_class.R`): `excel_numfmt_code()` gains an
  explicit `+`/`-` sign for pct diff + contrib and a leading `x` for ratio (kept numeric). ci = "cell"
  intervals + OR (with `1/x`) export as **text** columns (`xl_materialize_data`, `@` numFmt); new
  `tab_xl(or_numeric = TRUE)` / `options(tabxplor.xl_or_numeric)` keeps OR numeric. Each numeric mean
  gets a sibling **`<var>_sd`** column (sqrt(var), uncoloured, sigma numFmt — injected in
  `tab_materialize_extras(backend = "xl")`; console/kable/md keep inline `(sd)`). The **col_var spanning
  header** shifts the geometry down one row (`span_off`, `+6` stacking, `xlb_merge`, clean level labels).
- **13c-vi**: transpose-at-export verified — both colour channels + numeric means/sd survive; no fix
  needed.

Caveats / deferred: **`+Nsd` sd-unit display for mean_diff** deferred (overlaps Phase 5's numeric-diff
DISPLAY). Excel ratio stays a real number, so `< 1` shows `x0.5` (the divide sign needs an inverted value
-> text only, not used). md col_var-name row is a visual title (not a valid pandoc pipe row -- matches the
pre-existing multi-col_var behaviour). Pre-existing test `tab_logit "color_signif='ignore' colours
non-sig ORs"` fails **in isolation** but passes in the full suite (a color-breaks-leak ordering artifact,
NOT from 13c).

---

Missing infos on exported tables, compared to what’s default in other statistical software ?
- Display the variable names for `col_vars` : not in console, but in html and Excel, add a second headers line above the main headers row with the levels ; when contiguous fmt columns have the same col_vars, merge the variable names headers cells into a single cell (name of the same variable only needs to be given once).
- For tab_md, just put it in the first column ?

Custom display formatting :
- For custom display like "{pct} (n={n})", I want the result padded/aligned for maximum human readability assuming monospace font : not only for n in totals, but for **all** custom displays. For example, current display is : "100% (n=849)", "100% (n=3 648)", "100% (n=519)", "100% (n=1 178)", "100% (n=1 066)", "100% (n=902)", "100% (n=1 025)", "100% (n=9 187)". I would want : "100% (n=  849)", "100% (n=3 648)", "100% (n=  519)", "100% (n=1 178)", "100% (n=1 066)", "100% (n=  902)", "100% (n=1 025)", "100% (n=9 187)".
- Also, for Total columns and rows "{pct} (n={n})", is there a simple and realiable way to keep the percentages in bold, but force the "(n={n})" part to plain not-bold ? Mostly in html, md and Excel exports. Keep it specifically for "{pct} (n={n})", or generalise for all custom displays (the first <token> can be bold or not-bold, the next ones are always no-bold), or would it be complicated and performance-reducing for display ?


Display problems and improvements :
- lists not working with `options(tabxplor.print = "kable")` auto-display (print() method), since they are not there own class ! Implement a new vctrs class "list_tabxplor_tab" with vctrs (that should still behave like a list in any other way)
- with `list(tab(...), tab(...), ...) |> tab_kable()`, the result appear in console by defaut, it should be auto-routed to Viewer via class like kableExtra output (reuse class used by kableExtra if more simple and still reliable ?)
- in kable output, with `color = c("diff", "ratio")`, tooltips have an empty `rr` field (it should be called "ratio", and print the actual ratio, or be invisible when there is no ratio ; `ratio` display printing should always have a `×` symbol when >=1 and a `÷` symbol with the inversed (`1/ratio`) when <1, for example 0.5 shall print `"÷2"` ; defaut 1 digit, removing trailing zeros (`3.333` go to  `3.3` but `2.0000` go to `2`, with the user-friendly padding), respecting padding for perfect aligment in monospace font for maximum human readability)
- When there are confidence intervals significance stars, they should display, in some way or another, in all exports types. They should be completely padded right everywhere, so the stars always align in monospace font. In tab_md() it’s not
- In exports AND in console display, with any significance star in the column, all numbers should be padded right to keep numbers alignment and readability.
- Does transpose at export work perfecty (colors and all) with `pct = "col"` and with numeric variables ? If not, calculate colors, and anything else relevant (other column-level attributes not usable after the transposition), before transposition ?


Excel formattings :
- `ci = "cell"` not working at all, Excel only shows the raw base number with all digits and no formatting. Export as pure text ?
- For numeric variables, by default, Excel should print a mean column with the base name (colored) + a sd column with the same name and suffix "_sd" (not colored, formatting with sigma symbol first).
- What other such cases should be handled ? What are the ones that will need to use the pure text + formatting approach (not ideal, since then user don’t have access to raw numbers in Excel) ? What are the ones were another solution is doable (several columns stay readable like in the mean + sd case, other workarounds exist, etc.) ?
- Ensure numbers formattings are used to : explicit `+` for `diff` and `contrib` ; explicit `+<number>sd`/`-<number>sd` for mean_diff with standardised diffs measured in sd (only if user does not provides the breaks scale, of course) ; leading `×` and `÷` symbols for `ratio` ; what else ?

#### Phase 13d – Light mode/Dark mode in kable exports (DONE)

`theme` gains **`"auto"`** (follow the reader's OS **and** the host page's dark toggle) on
`tab_kable`/`tab_md`/`tab_css`/`tab_export`; new `options(tabxplor.theme = "light")` +
`options(tabxplor.kable_css = TRUE)`. Full record + landmines: `dev/tabxplor_1.4.0_decisions.md` **§38**.

- **Why it was never a CSS job**: the html engine wrote `color:#hex` **inline on every `<td>`**, and an
  inline style beats any `@media` rule short of `!important`. Cells had to carry classes.
- **The keystone — classes named by palette SLOT, not by break** (`.p1-.p4`/`.m1-.m4` text,
  `.o1-.o4`/`.u1-.u4` bg, `+ .g1`/`.g2` html chrome). The stylesheet becomes a pure function of
  (palette, color_type, theme) → **table-independent**, so: one `tab_css()` styles a whole document
  (`options(tabxplor.kable_css = FALSE)` + one `results='asis'` chunk); **collisions are impossible**
  (the planned hash/scope wrapper was dropped as unneeded); ALL measures share one vocabulary; and the
  class is a pure function of the slot int, so `fmt_color_plan()`+palette leave the cell path.
  **DELETED**: `html_style_block`, `md_break_class`, `md_slot_class_map`, `md_css_rules`,
  `md_css_block`, the md `.n` neutral, the legend token's `cls` field, `tab_kable_join(theme=)`.
- **`render_html_engine()` is now THEME-AGNOSTIC** — markup byte-identical across light/dark/auto
  (test-locked); the theme lives only in the `<style>`. `tab_md_css()` = thin wrapper on
  `tab_css(chrome = FALSE)`; its `dark_mode` arg **deleted, not deprecated** (never shipped).
- **`"auto"` = 4 cascade layers, ORDER is the contract**: light base → `@media` dark → toggle-light →
  toggle-dark. `@media` only reports the OS; Quarto/Bootstrap/Tailwind toggle a CLASS it cannot see, so
  the hook layers must follow and out-specify it (ordering test).
- **Scope**: html engine only. kableExtra bakes its theme at render time (`kable_classic`/
  `kable_material_dark`) and its HTML is not ours → `"auto"` downgrades with a one-time message. Static
  backends (`tab_xl`, `tab_plot`) resolve `"auto"` → `"light"` via `resolve_export_opts(allow_auto=)`.
- **3 latent bugs fixed**: `theme="auto"` crashed (`get_color_style("auto")` → NULL palette; and
  `theme_cols`' `if_else` silently took the dark branch); the legend would have frozen light (**the
  discriminator is the ENGINE — "does our stylesheet ship?" — NOT the theme**: `html`+`light`+
  `css=FALSE`+a doc-level `tab_css("auto")` is real); `currentColor` borders took the CELL's hex —
  ⚠ but that third one was **not actually fixed here**; the border SHORTHAND out-specifies the explicit
  `border-color`, so it kept winning until Phase 14j (§40).
- **Browser-verified** (maintainer, 2026-07-16): OS toggle + `body.quarto-*` + `[data-bs-theme]` +
  `[data-theme]` all flip both ways. ONE known gap, consciously parked: **Tailwind's class strategy**
  (light = the ABSENCE of `html.dark`, so there is nothing to hook) leaves a dark island on a dark OS.
  Narrow — every other framework sets an explicit light class. Detail + the two possible fixes:
  decisions §38 + the hook block in `R/tab-css.R`.
- **Accepted**: light mode now owns `background`/`border-color` (symmetric; NEWS'd); dark islands on a
  light-only page are inherent to `prefers-color-scheme` (auto is opt-in, fg+bg always set together).
- **jamovi unaffected** (light-only) and its `<style>` support is now EVIDENCED, not assumed — see the
  Phase 10a retraction above.

##### Original plan (historical intent)

Native dark mode/light mode management for exported tables, specially html tables
- With kable or another html tables solution, use css exported and applied with the table ?
- Wire this css on standard html dark mode toggle, with a global option in R to use Dark mode in viewer. As a result, the table should autochange it’s formatted we the user change to dark light mode on whichever html page the table is embedded with. Overall background color should be "#111111", text and borders overall color "#ffffff". Do web searches to find current good practices about this. If relevant, ensure linewidth of borders, etc., fit for readability in Dark mode.
- Do thorough web searches to gather good practices on that matter : general good practices ; quarto and knitr good practices ; etc.. If there’s a red flag or impossibility, tell me.
- Don’t use in jmvtab jamovi module : there’s only a Light mode, and it should print the fastest possible in live js UI, so don’t waste time with the Dark+Light detection and autotoggle.
- Would it be easy to do the same, using css in the html part of tab_md exports ? Or would it be unreliable ? (Only for markdown embedded in html. For the use in interactive editor, of course, I’ll map the pandoc spans to the relevant dark or light colors myself.)
- Maybe an argument to choose between : light mode ; dark mode ; autodetection + autotoggle ? autodetection can default if it’s really reliable ; otherwise better keept light mode as default. Add a global option to handle.

---




### Phase 14 – manual review by maintainer and next improvements

#### Context : 14a to 14g

`dev/review_manual/tab_manual_review_pass_1.R` is the maintainer's first hands-on review of tabxplor
1.4.0 on real survey data (`pc18`). Its `#` comments are the spec. This plan turns them into phases.

Nine defects were **reproduced and root-caused** during planning (not guessed) — several have causes
neither the maintainer nor I had named, and three change the shape of the fix:

| # | Symptom (maintainer)                             | Verified root cause                                                                                                                                                                                                                                                                                              |
|---|--------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | `color_signif` greys out **every** cell          | `legacy_union()` ([tab.R:677-688](R/tab.R#L677)) returns early for `"auto"`/`"contrib"`/`"OR"` **before** the `signif` switch, and `color = TRUE` never calls it at all → `legacy = "auto"` → `tab_resolve_settings()` never forces `ci = "diff"` → no CI → all gates FALSE. Affects the DEFAULT `color = TRUE`. |
| 2 | mirai crash `rep()` invalid 'times'              | **Not a parallel bug.** `chi2_compute_test()` ([tab.R:5763](R/tab.R#L5763)): `vapply(lv_cols, ..., double(n_rows2))` returns a **vector, not a matrix**, when `n_rows2 == 1` → `ncol(M)` is `NULL` → `rep(times = NULL)`. Guard is `n_rows2 > 0`, must be `> 1`. mirai only reframes the error.                  |
| 3 | `n` row disappears with 2+ row_vars, `pct="col"` | `tab_add_n_pct()` ([tab.R:6721](R/tab.R#L6721)) does `dplyr::slice(..1, last_totrow)` with a **global** index on a **grouped** tab → slice is group-aware → 0 rows → `bind_rows` silently drops it. Verified: `nrow(slice(t2, 11)) == 0`.                                                                        |
| 4 | `diff: ××1.3`                                    | Double `×`: the tooltip prepends it ([tab_classes.R:2037](R/tab_classes.R#L2037)) **and** `format()` already does ([fmt_class.R:2009](R/fmt_class.R#L2009)).                                                                                                                                                     |
| 5 | "is it diff or ratio?"                           | It IS the diff — but a **multiplication sign on an additive difference**. The numeric-diff *display* flip was deferred at Phase 2 and never landed ([fmt_class.R:1876-1878](R/fmt_class.R#L1876) admits it).                                                                                                     |
| 6 | `ratio: ×1` on Total cols / ref rows             | `cond_rr` ([tab_classes.R:2088](R/tab_classes.R#L2088)) lacks the totcol / totrow / ref gates its sibling `ok_diff` has. Also `type=="mean"` is excluded → means show no ratio line at all, though ratio is what colours them.                                                                                   |
| 7 | kable padding `100% (n= 849)`                    | **Not (only) the font.** Both engines set `white-space: nowrap` on fmt columns, and CSS **collapses runs of ASCII spaces** — the pad dies before the font matters.                                                                                                                                               |
| 8 | dark mode loses all light formatting             | `theme="dark"` swaps kableExtra's **whole theme** → `kable_material_dark` (56px headers, `#FFFFFF60` th, its own padding).                                                                                                                                                                                       |
| 9 | col_var header grey + too big                    | `add_header_above()` runs **before** `kable_classic()` → `lightable_class` is NULL → the `#ddd` grey fallback fires; and `row_spec(0)` only targets the **last** `<thead>` row, so the spanning row is never styled.                                                                                             |

Two systemic findings worth naming once:

- **`tab_compact()`'s synthetic `levels` / `row_var` columns are a recurring root cause.** They make
  `tab_get_vars()` report `tab_vars = "row_var"` on a table with no tab_vars, which is why
  `tab_transpose()` aborts with a misleading message and why `tab_xl()`'s title reads
  *"levels by multi (tabbed by row_var)"*.
  + **This needs to be fixed at framework level, reliably**, by finding what solid and reliable property differenciate a table with a real tab_vars and several row_vars merged (the column currents names with several row_vars merged are one, but maybe not the most reliable).
- **Positron has no supported theme API — but a workaround exists and is verified on this machine.**
  Two research passes (§14g) agree there is no supported route and none scheduled
  ([positron#2986](https://github.com/posit-dev/positron/issues/2986), open 2 years, milestone
  "Future"), and that **no R package detects it** — thematic assumes light + warns, cli returns FALSE,
  the rest don't try. But your client `settings.json` **is** reachable from WSL (VS Code caches it
  server-side under `~/.positron-server/data/User/History/`, and that cache updates on live writes), and
  the chain `workbench.colorTheme` → extension `package.json` → `uiTheme: vs-dark` resolves your actual
  theme correctly today. §14g ships it best-effort, silent, degrading to today's behaviour.

##### Decisions taken with the maintainer

1. **Padding** → swap the pad char to **U+2007 FIGURE SPACE** for HTML + Excel only; console/md keep
   ASCII. Measured in DejaVu Sans: `U+2007` = 1303/2048 em = **exactly** the digit width (1303), while
   `U+0020` = 651 = exactly half — so the "2 spaces" intuition was numerically right for this font, but
   U+2007 is right *by definition* in any font with tabular figures, and never collapses. Goes next to
   `unbrk`/`sigma_sign`/`mult_sign` at [utils.R:1180-1183](R/utils.R#L1180) as a `\uXXXX` escape
   (R sources stay ASCII).
2. **kable engine** → `engine = "html"` becomes the default, but it needs **serious design work**
   → its own phase (§14e) with the maintainer's feedback list as the brief.
3. **md col_var names** → a **first BODY row**, lower-contrast, visually marked, with an option to
   remove it. Long names: whole name in the **first cell only**, deliberately **not** pipe-aligned for
   that row (parses fine; only the maintainer's own markdownlint flags it), followed by a separator row.
4. **XL title** → full names, elided past ~3 with a count. Sheet name likewise, cut to 25.
5. **List at export** → **never merge**. Delete the export-time compaction branch.
6. **XL legend** → precompute a **9th palette** (`-0.2` OKLCH lightness of the bg palette) in
   `dev/color_palette_tools.R`, baked as constants like the other 8.
7. **Ratio CI** → **Katz log-RR**, stored in the existing `ci_inf`/`ci_sup` with **`ci_type = "ratio"`**
   (the Phase-12a `ci_type = "or"` precedent). **No new fmt fields.** Trigger rule: the CI follows the
   **text channel**. `color = "ratio"` or `c("ratio","diff")` → Katz. `color = "diff"` / `TRUE` /
   `c("diff","ratio")` → exactly today's behaviour, unchanged.
8. **`tab_xl()`** → keeps returning `invisible(tabs_base)` **and** `cat()`s the path.
9. **Tests** → never use `pc18` (confidential). `forcats::gss_cat` and the like only.

---

#### Phase 14a — correctness bugs + the `test=` rename (DONE — 2026-07-16)

All five landed. Conscious golden regen: **only** `_color_golden/c_after_ci.rds` +
`c_mean_after_ci.rds` (the two `guaranteed_effect` fixtures; every changed cell has a CI excluding
the neutral, no cell LOST colour, direction always matches the sign of `diff` — verified cell by
cell). The other 10 colour goldens were re-written by `make_color_golden.R` but are **semantically
identical** (gzip mtime churn); restore them with
`git checkout -- tests/testthat/_color_golden/{c_ci,c_contrib,c_contrib_all,c_contrib_all_notab,c_diff,c_diff_ci,c_mean_diff,c_mean_diff_ci,c_or,c_syn_diff}.rds`.

- **`color_signif` forces its CI** — the policy now reaches `tab_resolve_settings()` as a real
  argument (`tab()`/`tab_many()` pass `color_spec$signif` -> `tab_build(color_signif=)` -> `ctx` ->
  `tab_setup`; `tab_counts()` passes `"ignore"`, it only takes legacy colour strings). The forcing sits
  in the ONE cascade (§7b), **before** the `color = "auto"` resolution, so the implicit form is
  byte-identical to the explicit `ci = "diff"` one (locked for factor / numeric-only / mixed).
  `pct_rowcol` was hoisted out of the auto `case_when` and is shared by both. Gated == an explicit
  diff, or an "auto" resolving to row/col pct or to the numeric arm; `contrib`/`OR` are never forced.
  Explicit `ci = "cell"` + a policy -> error.
- **`guaranteed_effect` break offset** — new `offset_guaranteed_breaks()` next to `fmt_color_plan()`
  (`R/fmt_class.R`), applied to `over`/`under` independently (asymmetric scales) inside the plan, so
  `legend_specs()` follows for free. The legend now prints `+0; +5; +15; +25`, as the Phase 13b spec
  always claimed.
- **`chi2_compute_test()` single-row crash** — `ncM <- length(lv_cols)`, not `ncol(M)`: `vapply()`
  returns a matrix only when `FUN.VALUE` has length > 1, so `n_rows2 == 1` made `M` a vector and
  `ncol(M)` NULL. NOT parallel-specific — mirai only reframed it.
- **`tab_add_n_pct()` pct="col"** — new shared `tab_append_pctcol_rows()` (used by BOTH the add_n and
  add_pct branches): slice on the UNGROUPED tab (the old global index vs `dplyr::slice()`'s
  group-relative one returned 0 rows -> `bind_rows` silently dropped the row), one row per sub-table,
  spliced after each sub-table (anchoring on the group END, not the total row, is what preserves the
  historical `Total | row_pct | n` order).
- **`chi2` -> `test`** on `tab()` + `tab_counts()` (`lifecycle::deprecated()` sentinel). `tab_build()`
  keeps the internal `chi2` name (it drives `tab_chi2()`; the ANOVA arm branches in `tab_transform()`)
  — only the PUBLIC surface is renamed. `tab_many()` keeps `chi2` (itself deprecated).

Tests: `test-color-engine.R` (offset + the "significant => coloured" invariant + strict/neutral edge +
the multiplicative arm), `test-color-config.R` (CI forcing, implicit==explicit, ci="cell" error,
contrib/OR not forced), `test-calculations.R` (single-row test + chisq.test parity + the rename),
`test-display-extras.R` (the n row per sub-table, order, base).
**Suite: FAIL 0 | WARN 0 | SKIP 4 | PASS 2426 in 48.9 s.**

**Landmines hit while doing it — read before the next rename:**

- **`auto_or` / `pct_rowcol` are `all()` over the FACTOR col_vars, so on a numeric-only table they are
  `all(logical(0))` == TRUE, vacuously.** A `!auto_or` guard therefore silently excludes the numeric
  arm. Cost me a regression that the filtered run did not catch (the probe predated the guard) and only
  the full suite did.
- **`chi2` is THREE different names.** Renaming it needs classification, not `sed`: `tab()`/
  `tab_counts()` = the deprecated public arg (-> `test`); `tab_build()`/`tab_resolve_settings()` = the
  INTERNAL arg (keeps `chi2`, it drives `tab_chi2()`); `jmv_opts()`/`mk()`/`jmvtab_build()` = the jamovi
  OPTION (keeps `chi2` -- its `.a.yaml`/`.h.R` surface is compiled, and `jmvtab_build` reads
  `opts$chi2`); plus `tab_many()` (kept), list names, sprintf labels and test titles. A line-scoped
  regex over-reached into all of the last three. `jmvtab_build()` also called `tab(chi2 = )` itself, so
  the package tripped its own deprecation (-> now `test = opts$chi2`).
- **`tests/testthat/setup.R`'s `lifecycle_verbosity = "quiet"` has never worked** --
  `testthat::local_reproducible_output()` re-sets it to `"warning"` inside every `test_that()`. See the
  corrected comment there. Quiet comes from not calling the deprecated surface, or `suppressWarnings()`.
- **A new `ctx` field must be added in FOUR places**, or something breaks quietly:
  (1) `tab_build()`'s ctx; (2) `tab_counts()`'s ctx; (3) `test-carve-parity.R`'s hand-built ctx (a THIRD
  builder mirroring `tab_build`'s -- otherwise `tab_setup()`'s `list2env(ctx)` leaves the local
  undefined and every stage-composition test errors); (4) **`utils::globalVariables()` in
  `R/fmt_class.R`** -- `list2env(ctx, environment())` is invisible to codetools, so R CMD check NOTEs
  `no visible binding for global variable`. Only `devtools::check()` catches (4); the suite is green
  without it.


#### Phase 14b — tooltips + the numeric-diff display (DONE — 2026-07-17)

All seven bullets landed. **Full suite FAIL 0 | WARN 0 | PASS 2725; `check()` 0/0/0; NO golden
regeneration** — every `_golden/*.rds` and `_color_golden/*.rds` is byte-identical, only
`_snaps/render-html.md` moved (tooltip text + placement, reviewed). New: `test-tooltips-14b.R`,
`test-ci-ratio-katz.R`. Maintainer forks this session: do all 7 at once; mean diff = **raw signed
difference + a `std diff:` tooltip line** (NOT sd units in the cell — the number must stay `$diff`,
Excel writes the raw field, and `scale$std` belongs to the *colour* scale, which `color = TRUE` does
not even consult for a mean); placement = **`"auto right"`**, not a last-N-columns rule.

- **Numeric-diff display (the Phase-2 D3 leftover)** — `format()` now signs EVERY diff (`diff_signed`
  = `ok & display == "diff"`); the mean branch's `mult_sign` is gone (`+1.2` / `-0.22`, the variable's
  own units). The Excel `signed` mask widened to `display %in% c("ctr", "diff")` — excluding means is
  what would desync the bypass now. `×`/`÷` belong to `rr` alone.
- **Tooltip** (`tab_kable_print_tooltip`, now TEXT-only): shared `comparable` gate (the base-cell
  exclusion the diff line had, NA-safe, now also gating ratio + reused by `cond_ctr`); ONE `ref` token
  for the diff+ratio group (`ref_grp`); `type == "mean"` added to the ratio gate; `tip_num()` trims the
  column padding off every interpolated value; new `std diff:` line (Glass's Δ, mean columns, where
  `sd_ref` resolves). The `ref & any(ok_diff)` tautology is gone (it sat *inside* `if (any(ok_diff))`).
- **Fragment join rewritten** — the old chain pasted all fragments with a fixed `" ; "` then rewrote
  the result (`str_replace_all(";  ; ", "; ")` ×3 + trims + an `"NA ;"` scrub). Non-overlapping
  matching means one pass cannot collapse adjacent empties, so it silently assumed <5 in a row; the
  10th fragment makes 9-empty runs reachable (a Total cell is `n:` only). **A/B proved the OLD side
  wrong** (`"f1: 5 ;"` / `"; f10: 5"`). Now an exact per-cell non-empty join.
- **Placement** — ONE builder `tab_tooltip_attrs()` (`R/tab-render-html.R`) for both engines: the
  kableExtra path passes it pre-classed (`cell_spec()` honours a `ke_tooltip`/`ke_popover` verbatim),
  the html path pastes it into the `<td>`. `data-placement="auto right"` = Bootstrap's auto token
  (prefer right, reorient on overflow) — measured at render time, so it also covers a scrolled table
  or a narrow pane. ⚠ **`kableExtra::spec_tooltip()` cannot emit it**: its `match.arg()` takes ONE
  token from `c("right","bottom","top","left","auto")`, so `"auto right"` errors and `c("auto","right")`
  silently yields a length-2 attribute. Hence the hand-built string. `.tooltip-inner{max-width:none;
  white-space:nowrap;}` added to `tab_css(chrome = TRUE)` — ⚠ NOT scopable (bootstrap moves the
  tooltip to `<body>`, which is what stops the table clipping it), documented in place.
- **Two pre-existing bugs fixed in passing**: the html engine's popover rendered its own escaped
  ATTRIBUTE STRING as its content (`tab_kable_print_tooltip(popover=)` returned `spec_popover()`
  attributes from a *text* builder, and the engine wrapped them again) — the arg is deleted, attrs
  live only in `tab_tooltip_attrs()`; and the html popover omitted `data-trigger`, so it needed a
  CLICK where kableExtra's opened on HOVER (the shared builder ends the drift).
- **Katz ratio CI** — `ci_katz_rr()` (`R/tab-agg.R`), `ci_type = "ratio"` (the 4-site Phase-12a "or"
  pattern: enum, `ci_center()`, the colour gate, `format()`'s bracket + a 2-digit bump so the bounds
  do not round equal and collapse to a point). Trigger = `color_pct_text_is_ratio(spec)` (R/tab.R)
  -> `tab_build(color_ratio_ci=)` -> ctx -> `tab_resolve_settings()`, which emits the new per-row_var
  **`ci_scale`** ("diff"/"ratio", only where `ci == "diff"`) -> `tab_apply_tests()` -> `tab_ci(ci_scale=)`.
  Threaded exactly like 14a's `color_signif`, and for the same reason: `legacy_union()` maps every
  ratio onto a diff-family string, so the legacy `color` cannot carry it. **Proportions only** (a mean
  ratio needs Fieller) — which is also what keeps `color = TRUE` untouched, since a mean's *text*
  channel already IS the ratio.
- **The significance gate is now CI-driven, not measure-driven** (`fmt_color_plan`): an interval is
  significant when it excludes ITS OWN neutral (0 additive / 1 multiplicative). Keying it on the
  measure only held while each measure had exactly one possible ci_type. It also fixes a latent
  mismatch: measure `"or"` + a difference ci_type tested the diff bounds against 1 -> never
  significant (the hazard 14a's cascade works around with `& !auto_or`).
- **`rescale_bound()`** replaces the ad-hoc diff->ratio conversion: `diff` and `ratio` are both affine
  in the cell proportion with the reference at its point estimate (`ratio - 1 = diff / p_ref`), so ONE
  helper maps a bound either way by a ratio of offsets from the neutrals. The diff->ratio direction is
  byte-identical to the expression it replaces; ratio->diff is the new mirror (the derived bg channel).
- Legend names Katz off the STORED `ci_type` (not `method_diff`, which never built it) + FR
  translation (`po/R-fr.po`, `.mo` recompiled — **`gettext` had to be apt-installed on this box**;
  `tools::update_pkg_po()` needs `msgfmt`/`msgmerge`/`msginit`).

#### Phase 14c — colour legends (DONE — 2026-07-17)

All four bullets landed, plus **two defects the item-4 re-verification turned up** and **one the
`tab_plot` legend had been carrying silently**. Full suite **FAIL 0 | WARN 0 | PASS 2751**; `document()`
clean. Golden `_golden/*.rds` + `_color_golden/*.rds` **all byte-identical — no colour regeneration**;
only the two legend-bearing display snapshots moved (`render-html.md`: 17 spans gain
`font-weight:bold;`; `golden.md`: 8 md legend lines gain `**`), each diffed token-by-token first.

- **Bold break-words, every medium.** Runs already did; console composes `crayon::bold`, html emits
  `font-weight:bold` **inline**, md wraps `**[+5]{.p1}**`. Inline/markup rather than left to the
  stylesheet, because it must hold on the **background** channel (whose `.o*`/`.u*` stay unbolded —
  they mirror the cells, where a fill alone does not bold) and on the **kableExtra** path (no
  stylesheet of ours ships there).
- **`tab_css()`/`tab_md_css()` bold the text slots** (`.p1..m4{font-weight:bold;}`, emitted once,
  `chrome`-independent — it is theme-independent so it must not sit in the 4×-emitted rule table).
  This is the maintainer's separate "like in kable" note, and it IS kable: `tab_export_prep()`'s
  `bold = !is.na(text_hex) | ref_alltot` already bolds every text-coloured cell in kableExtra AND the
  html engine, so the rule is a **no-op there** and exists for the one medium with no other way to say
  it — `tab_md()`'s bare `[42%]{.p2}` spans.
- **Excel bg-legend readability** (decision 6). A rich-text run carries a font colour but **no fill**,
  so a background break-word is drawn as text — and the background palette (L 0.85–0.97) is invisible
  on white. New 9th palette `default_bg_legend_colors`/`_neg` = the same hues at **−0.2 OKLCH
  lightness** (chroma kept, gamut-capped), baked from new `dev/color_palette_tools.R::darken_for_legend()`,
  reachable as `get_color_style(type = "bg_legend")` (color_code-only: it substitutes for a fill, and a
  console has one → crayon aborts). **Light only, deliberately**: the legend cell's page is white
  whatever the `theme`, the dark fills (L 0.20–0.35) already read there, and −0.2 collapses them to
  black (measured: `#001b1b` → `#000000`, slots 3/4 both → L 0.10) — so `bg_legend_dark` is the dark bg
  palette unchanged. `set_color_palette(bg_legend_colors=, bg_legend_colors_neg=)` added; setting
  `background_colors` without them makes them follow the fills verbatim (a custom green fill must never
  keep the default blue legend word).
- **Console `theme` divergence** fixed: it read `options(tabxplor.color_style_theme)` while `slot_hex()`
  right above used the resolved `pal` — the two could disagree.
- **`medium = "excel"` → `"runs"`** (internal fn, 2 call sites): the concept is "draws TEXT, cannot
  fill", which is Excel **and** `tab_plot`. Both now take the bg_legend palette.
- **BUG FOUND + FIXED — `tab_plot()`'s legend was raw HTML in black.** It scraped the legend back out of
  the *html* rendering with regexes (`^color: rgba...`) that stopped matching when Phase 13b replaced
  kableExtra's `text_spec` spans with inline hex; every token rendered as e.g.
  `color:#02A5B3 !important;">+5` in uniform black. Rewritten onto `medium = "runs"` (the structure it
  always wanted: text + hex per token) — **~45 lines of regex deleted**, adjacent same-colour runs
  folded into one ggtexttable cell.
- **BUG FOUND + FIXED — two `tab_reg` legend wordings** (item 4 asked to re-verify β/IRR; β/SD and
  IRR-vs-OR from 13b hold, but): (1) a β legend said *"not significantly different from **the Total
  row**"* — a reg table has no total row; `legend_ref_info()` read ref_type "tot" like any fmt column.
  `is_coef` now takes the same "reference category" branch as OR/IRR (imprecise for a numeric
  predictor's per-unit β, whose null is 0 — the same approximation the OR arm always made). (2) a
  Poisson **IRR** was described as a *"Wald interval on the log **odds-ratio**"*: `ci_type = "or"` is
  the multiplicative **shape**, shared by OR / IRR / cumulative OR, so the name now comes from the
  effect word (+ 2 new FR strings, `.mo` recompiled).

**Flagged for the maintainer** (not fixed here — judgment calls, see the questions block after 14g):
the darkened light legend hues are faint (L≈0.65–0.77 at C≈0.03), and `tab_plot()`'s legend block
still holds ~60 lines of half-commented dead code.


#### Phase 14d — transpose, list container, `tab_xl` (DONE — 2026-07-17)

Every bullet landed. Full suite **FAIL 0 | WARN 0 | PASS 2782**. Conscious golden regen: **the `vars`
attribute only** — 28 of 36 `_golden/*.rds` gained it and are otherwise `identical()` (proven by
stripping the attr and comparing); the 8 that did not are raw `tab_num()` leaves, which never reach the
stage that records roles (the documented heuristic-fallback case). No `_snaps/` moved.

- **The framework fix — `vars` recorded, not inferred.** New table attribute
  `list(row_vars, col_vars, tab_vars, compacted)`, written in `tab_assemble_tables()` / `tab_compact()`
  and re-keyed by `tab_transpose()`; read via new **`tab_vars_recorded()`**, which **validates it
  against the real columns** (a dplyr chain can rename/drop them) → NULL → the old heuristic, so
  hand-built tables still work. ⚠ **CONTRACT**: `tab_get_vars()`'s `row_var`/`tab_vars` stay **column**
  names (what every consumer indexes with); `row_vars` carries the **source** names. They differ only
  on a merged table — conflating them would have broken every `x[[row_var]]`.
- **PREREQUISITE (done first, byte-identical): `tab_attrs()` / `tab_restore()` / `tab_bind_attrs()`.**
  The ~34 dplyr S3 methods + vctrs reconcilers each named every attribute by hand, so `subtext` / `test`
  / `render_extras` / `ci_settings` had each paid the same ~34-site edit. A 5th attribute would have
  paid it a 5th time. Now: one `new_tab()` formal + a getter/setter + **one line in `tab_attrs()`**.
- **`tab_compact()` re-merge guard.** The heuristic used to catch an already-merged table *by accident*
  (reading its synthetic `row_var` column as a tab_var → the bail). Truthful roles remove that accident,
  so the guard is now explicit (`compacted` → no-op) — otherwise it would have merged a second time.
- **`tab_transpose()` with several row_vars.** Folds the `(row_var, levels)` pair into one key column so
  the existing single-row_var pivot runs unchanged; each old row_var becomes a **col_var** with its own
  total/reference column (exporters span its name over its levels for free). Levels are suffixed
  `_<var>` only where two row_vars share one (tab()'s own `Other_race` convention, which
  `tab_col_var_header()` already strips). The total-row guard is now per sub-table.
- **BUG FOUND — `dplyr::pull(tabs, all_of(row_var))` read the DATA MASK.** tidyselect resolves
  `row_var` against the columns first, and a merged table has a column literally *named* `row_var` — so
  it silently pulled that column instead of the local variable. Latent (a merged table never got past
  the old guard); now `tabs[[row_var]]`.
- **Never merge a list at export** (decision 5). Deleted the branch **and `tab_list_mergeable()`** —
  which re-ran `tab_get_vars()` over every tab immediately before `tab_compact()` re-ran the identical
  scan. `tab_resolve_tables()`'s `compact` arg is gone (dead; nothing read `meta$compact`).
- **`tab_xl`**: new shared `xl_finish()` → `cat`s the resolved path (decision 8) and **fixes the
  double-resolve** (`tab_xl_resolve_path()` is NOT pure — with `replace = FALSE` it auto-numbers past
  the file it just wrote, so the two degrade paths opened a file that never existed). `tab_get_titles()`
  rewritten per decision 4 (real names via `vars`, elide past 3 with "+N more", no NA fall-through);
  mean/sd headers → `mean` / `sd` under the col_var span, **gated on the split existing** so the text
  backends (sd inline) are untouched — their wording is 14e's.
- **`transpose` now runs BEFORE materialise**: the extras are ORIENTED (add_n is a column under row%, a
  row under col%), so materialising first baked the pre-transpose orientation in. `tab_md(transpose =
  TRUE)` of a row% table is now **byte-identical** to the native col% table (test-locked).

**Flagged for the maintainer** (see the questions block after 14g): a pre-existing golden drift
(`n_ci_tabvars*`'s `ci_sup` `NA`→`NaN`, invisible to `expect_equal`'s tolerance, reproduces on
unmodified HEAD) is now baked in; and the Excel mean/sd column WIDTH was not narrowed.


#### Phase 14e — the html engine becomes the default, and is designed properly (DONE — 2026-07-17)

`options(tabxplor.tab_kable_engine)` is now **`"html"`**. Full suite **FAIL 0 | PASS 2812**;
`check()` 0 errors / 0 warnings / 0 notes. Only `_snaps/render-html.md`'s 4 html-engine snapshots moved
(reviewed); no `_golden/*.rds`, no `_color_golden/*.rds`. A browser-checkable sample is written to
**`dev/review_manual/phase14e_html_engine.html`** (theme = "auto" + a composite-display table).

**The governing decision: the engine emits NO inline styles.** Every look — geometry included — is a
**role class** resolved by `tab_css()` (`tx-r`/`tx-l`, `tx-num`, `tx-br`/`tx-bl`, `tx-tot`/`tx-rv`,
`tx-b`, `tx-bt`/`tx-bb`/`tx-bb2`, `tx-span`, `tx-pill`). Three reasons, in order of weight: (1) **an
inline style cannot be overridden by a user's CSS**, so the maintainer's own rule — *"must continue to
work with common css customisation, as kableExtra does... a good, compact, readable default that can be
overwritten"* — was **impossible** while the engine wrote its own borders/widths; (2) it removes the
INLINE half of the coloured-border bug (`border-right:1px solid` is a shorthand → resets `border-color`
to `currentColor` = the cell's palette hex; inline it also beat the stylesheet's rule) — ⚠ **14e claimed
this fixed the bug and it did not**: a class still out-specifies `td{border-color:…}`, so the shorthand
kept winning until **Phase 14j** replaced it with longhands (§40); (3) the markup
shrinks. This extends 13d's colour rule to everything. **Consequence**: `css = FALSE` + no `tab_css()`
now renders *unstyled*, not merely uncoloured.

- **Viewer/knit routing**: `tab_kable_join()` claims the **`kableExtra` class** for the html output (it
  IS an html fragment with `format = "html"`) rather than duplicating `print.kableExtra` /
  `knit_print.kableExtra`. Ends the maintainer's hand `class<-` workaround. kableExtra is a Suggests →
  absent, the class is inert and it falls back to today's `cat()`.
- **BUG — a wrapped header rendered its `<br>` literally.** `tab_wrap_text()` wraps long header names
  on `<br>`, and the engine html-escaped the whole label. kableExtra never hit it (`kable(escape =
  FALSE)`). New `html_escape_br()`: escape, then restore **only the tag we inject** — a `<` in a user's
  own level name stays escaped (test-locked both ways).
- **Fonts** DejaVu Sans Condensed (text) / DejaVu Sans (numbers), mirroring `tab_xl`'s
  `font_text`/`font_num` — kableExtra used DejaVu Sans throughout. **Geometry**: `padding:3px 4px`
  (~1mm sides, was touching the border) + `line-height:1.1` (was 0.85, crammed). **Hover** →
  kableExtra's lightable yellow. **Dark** → `#CECDC3` on `#222222` (pure white on near-black glares).
- **Background = a PILL** (`<span class="tx-pill o3">`) hugging the text, rounded — a full-cell flood
  reads as a blocky grid **and** swallows the row hover (a child's background always paints over its
  row's, whatever the specificity; kableExtra escaped this only because it fills a `<span>`).
- **U+2007 figure space** (decision 1): new `format(pad =)`, defaulting to `fig_space` when
  `html = TRUE` and a plain space otherwise, threaded through all 6 alignment sites + the composite
  recursion; `tab_xl` passes it explicitly (⚠ `html = TRUE` is NOT the lever there — it would also
  switch on the html-only `<sub>` markup). Console/md keep ASCII, so their goldens are byte-identical.
- **Test-suite trap found**: the `kableExtra engine (default)` section relied on the DEFAULT, so
  flipping it made the whole section silently assert against the *other* engine. Every call there now
  pins `engine = "kableExtra"`.
- **Bug caught by our own CSS well-formedness test**: a rule accidentally split across two `c()`
  elements became two broken lines. Worth keeping that test.

**DEFERRED (flagged for the maintainer, see the questions block after 14g):** the **VS Code/Positron
webview hooks** (`body.vscode-dark` / `data-vscode-theme-kind`) — the roadmap itself demands a live DOM
check FIRST (R html usually lands in an *iframe*, and the class sits on the OUTER webview body, so the
hook may never match); `pct="col"` compactness and the `min-width:10em`/`5.5em` review (needs a visual
judgment); tooltip dark styling; `inst/tab.css` is now dead for the default engine (all
`.lightable-classic`-scoped) — kableExtra-only, left alone.

#### Phase 14f — `tab_md` (DONE — 2026-07-17)

Full suite **FAIL 0 | PASS 2850**. Conscious golden regen: `_snaps/golden.md` only (the md layout
changed on purpose — see below); no `.rds`, no `render-html.md`.

- ⛔ **THE FIND: `tab_md()`'s output was NOT VALID PANDOC — every normal table.** The 13c-iii col_var
  name row sat ABOVE the level header, i.e. a **two-row header**, which pipe tables do not have:
  pandoc gives up and renders the whole thing as a line-block + a paragraph of pipes (reproduced on
  pandoc 3.7 with tabxplor's own output; 0 `<td>` emitted). It had been shipping since 13c-iii because
  **nothing ever rendered the md** — every test asserted on the markdown string. Fixed by moving the
  name to the **first BODY row** (decision 3): italic, in the FIRST cell of its group, one cell per
  column (never merged — a pipe row must keep the cell count or pandoc shifts the data), that row
  deliberately not pipe-*aligned* (a long name overflows rather than widening every column below).
  New **`tab_md(col_var_names = FALSE)`** drops it. **New test renders through pandoc** across 6
  shapes — the test that was missing.
- **Two more invalidities**: the spacer column's delimiter cell was `| |` (not a legal delimiter →
  `md_insert_col_sep(fill = "-")`, since one helper builds all 4 row types); and a `|` in a level label
  opened a spurious cell (now escaped `\|`, label columns only — fmt cells are package-formatted).
- **Padding model rebuilt around the VISIBLE end.** The bold rows' `+4` entered `num_width`, which
  pads INSIDE the bracket → `[    38%]{.p2}`: four spaces pandoc discards, and which push the number
  *out* of line with the bold cell in the raw file. Now each cell pads by its own visible-end width
  (`md_extra()`: markup that PRECEDES the last visible character — 0 plain, 2 whole-bold since its
  closing `**` follows the value, **4 composite-bold** whose closing `**` sits mid-cell before the
  `(n=…)` tail), so the markup grows leftwards into the pad and every number shares a raw column. The
  attr is padded to `attr_width` so `}` lines up (verified: pandoc reads `{.m2   }` == `{.m2}`).
- **`css = TRUE` now wraps the table in a pandoc fenced div** `::: {.tabxplor-tab}` → pandoc emits
  `<div class="tabxplor-tab">`, the hook every `tab_css()` rule already matches (pandoc emits a BARE
  `<table>`, which none could reach) — so `chrome = TRUE` is meaningful for md for the first time and a
  rendered md table gets the layout, not just the colours. `.tabxplor-tab table` added to the
  border-collapse rule (the class is the table itself in html, a wrapping div in md).
- **The existing test suite earned its keep twice**: the pipe-grid test caught my first name-row draft
  merging cells, and the numbers-aligned test caught the composite-bold case. Both metrics needed
  fixing too (they measured the RAW end, so a bold cell could only agree by accident).
- `tab_export("html_md")` — **declined** (the maintainer's own note): `tab_kable(engine = "html")` IS
  "markdown rendered to a styled html table", and the real ask (md renders well in Quarto) is what the
  validity + fenced-div fixes deliver.

#### Phase 14g — console theme / IDE detection (DONE — 2026-07-17)

**It works, end to end, on your machine.** New **`R/tab-theme-detect.R`**: `tx_detect_theme()` →
`"light"`/`"dark"`, wired into `set_color_palette(theme = "auto")` (new value) and `.onLoad`. Verified
live here: `workbench.colorTheme = "Starless Monokai Atom"` → `izumii.starless-monokai/package.json` →
`uiTheme: vs-dark` → **dark** ✓. Full suite **FAIL 0 | PASS 2897**; `test-theme-detect.R` (41) drives
every probe from **injected fixtures**, so it never depends on the host IDE.

- **Your live test is NOT needed — don't bother running it.** The roadmap flagged
  `.ps.ui.evaluateWhenClause` as a confirmation step you'd have to try by hand. The History-cache chain
  resolves the theme on its own, so the private ark RPC (`# TODO: Unexport these methods`) is not used
  at all. One fewer thing depending on an unexported API.
- **A roadmap measurement was stale, in our favour**: `POSITRON=1` and `TERM_PROGRAM=vscode` ARE set in
  the Positron **integrated terminal** (recorded as empty → *"terminal-side detection is dead here"*).
  Since that is where you actually run R, detection works there — and it is right on the merits, the
  terminal's background being the editor theme's.
- All five traps encoded: `isAvailable()` lies in ark (gate on `hasFun()` + `RSTUDIO=="1"`); `$dark` can
  be NA (`isTRUE`); `readRStudioPreference()` always returns your default (unused); the theme NAME is
  never a signal (exact-name → `uiTheme`); `autoDetectColorScheme` → bail (colorTheme is then stale).
- **PRIVACY honoured**: two keys pulled by regex, the file never parsed (it is JSONC anyway), so the
  `claudeQuota.sessionKey` beside them never enters R. Test-locked with a fake secret in the fixture.
- **Never warns** (not just never errors): `readLines()` warns *before* it errors, so `tryCatch(error=)`
  let it through — `file.exists()` first. `expect_silent`-locked.
- **Cost**: the extension scan is one level deep — recursive cost **70 ms at every load**, now **9 ms**,
  and only inside Positron.
- ⚠ **`setup.R` now pins `tabxplor.color_style_theme = "light"`**: detection makes the default
  machine-dependent, which is exactly the CI-passes/local-fails divergence the 2026-07-15 green-up
  spent a day on. Two colour-legend tests that read the option were pinned too.

- **Not done** (deliberate): re-detecting at PRINT. The resolved value is stored, so switching your
  editor theme mid-session needs another `set_color_palette(theme = "auto")` — per-print detection
  would mean an rstudioapi RPC / a file scan on every table.

##### Original research (historical intent)

**The research paid off — there IS a workaround, and it works on your actual setup.** Upstream is a
dead end ([posit-dev/positron#2986](https://github.com/posit-dev/positron/issues/2986), *"Support
rstudioapi::getThemeInfo()"*, OPEN since 2024-05, motivated by `thematic`, bounced `Future` → `RC` →
`Post-RC` → back to `Future` in 2025-12, one maintainer reply in two years). Neither `cli` nor `crayon`
detects a background — verified in their installed sources; cli knows *how many* colours, never *which*
background. But two local oracles exist:

**Verified working on your machine** — your client `settings.json` IS reachable from WSL despite
`C:\Users` being unmounted: VS Code caches it server-side under
`~/.positron-server/data/User/History/<hash>/`, and that cache **updates on live writes** (snapshots
grew 582→585 lines as extensions called `configurationService.updateValue()` — the same path the theme
picker uses, so it is not a stale manual-save snapshot). The full chain resolves in R today:
`workbench.colorTheme = "Starless Monokai Atom"` → `izumii.starless-monokai/package.json` →
`uiTheme: vs-dark` → **DARK** ✓ (correct for you). `window.autoDetectColorScheme` is not set for you, so
`workbench.colorTheme` is authoritative.

**A second, live oracle** — `.ps.ui.evaluateWhenClause("config.workbench.colorTheme == '<name>'")`. The
mechanism is proven (ark itself ships `config.git.enabled && gitOpenRepositoryCount > 0` through this
exact RPC), and jennybc's own note on #2986 points at it. It can only *test equality*, never read the
value — so it is a **confirmation** step for a name the History cache already supplied, not a probe.
(Two research passes disagreed here and the conflict resolves cleanly: VS Code exposes **no theme-KIND
context key** — you cannot ask "is it dark?" — but it *does* expose `config.<setting>` keys, so you can
ask "is the theme named X?". Both statements are true; only the name-equality question is answerable.)

Design — `tx_console_theme()`, layered, every step `tryCatch`-wrapped, defaulting to `"light"`:

1. explicit `options("tabxplor.color_style_theme")` always wins;
2. **RStudio** → `rstudioapi::getThemeInfo()$dark`, re-checked at **print** (today it is one-shot at
   `.onLoad`, [tab_classes.R:3428-3437](R/tab_classes.R#L3428), so a mid-session switch is missed);
3. **Positron** → History-cache `workbench.colorTheme` → `uiTheme` (extension `package.json`, plus a
   small hardcoded table for builtins, which have **no** server-side `package.json` — 62 builtin
   extensions, zero with `uiTheme`); optionally confirmed via `evaluateWhenClause`;
4. terminal → `COLORFGBG`; else `"light"`.

Copy **`cli:::detect_dark_theme()`**'s shape ([cli/R/themes.R:326](https://github.com/r-lib/cli/blob/main/R/themes.R))
— `RSTUDIO` env → `getThemeInfo()$dark`; iTerm → AppleScript; Emacs → `ESS_BACKGROUND_MODE`; else FALSE —
and extend it with the Positron branch. That is the best-in-class prior art: **no R package detects
Positron's theme** (thematic assumes light + warns; cli returns FALSE; crayon/gt/reactable/colorspace/
ggthemes/unikn don't try — several don't even depend on rstudioapi).

Five traps the implementation MUST encode (each source-verified):

- **`getThemeInfo()` errors, it does not degrade.** ark fakes `isAvailable() → TRUE`
  ([ark init.R:103](https://github.com/posit-dev/ark/blob/main/crates/ark/src/modules/positron/init.R)),
  so `verifyAvailable()` passes and `findFun()` then `stop()`s. The usual
  `if (rstudioapi::isAvailable()) getThemeInfo()` idiom **breaks in Positron**. Gate on
  `rstudioapi::hasFun("getThemeInfo")` (thematic's guard) *and* `Sys.getenv("RSTUDIO") == "1"`, never on
  `isAvailable()`.
- **`$dark` can be `NA` even in RStudio** — [tidyverse#88](https://github.com/tidyverse/tidyverse/issues/88),
  [rstudio#4850](https://github.com/rstudio/rstudio/issues/4850); cli's NEWS records a crash from exactly
  this. Use `isTRUE()`, never `if (info$dark)`.
- **`readRStudioPreference()` lies silently.** Its ark shim is literally `function(name, default)
  default` — it *shipped*, so `hasFun()` returns TRUE and it always returns your default. Never use it.
- **Name regex fails on your own theme.** `"Starless Monokai Atom"` contains neither "dark" nor
  "light" yet is `vs-dark`. Exact-name → `uiTheme` resolution is mandatory; no substring guessing.
- **Detect Positron by `.Platform$GUI == "Positron"`** (ark force-rebinds `.Platform` in `baseenv()`,
  [ark positron.R](https://github.com/posit-dev/ark/blob/main/crates/ark/src/modules/positron/positron.R))
  or `Sys.getenv("POSITRON") == "1"` — but **only in the console**. Measured in your WSL2 integrated
  terminal: `.Platform$GUI = X11`, `POSITRON` empty, `RSTUDIO` empty, `TERM_PROGRAM` empty — only
  `VSCODE_*` is present, despite [positron#3842](https://github.com/posit-dev/positron/issues/3842)
  being closed. **Terminal-side detection is dead here**; don't build on it.

Bail to `"light"` (never guess) when: `window.autoDetectColorScheme` is TRUE (the active theme then comes
from `workbench.preferredDark/LightColorTheme` following the OS, so `workbench.colorTheme` is **stale and
wrong** — it is not set for you, but must be guarded); the theme name resolves to no `uiTheme`; or the
History cache is absent.

⚠ **Two things only the maintainer can settle** — surface both before implementing:

- **One live test.** `.ps.*` exists only inside ark, so it could not be executed from `Rscript`. Run in
  the Positron console: `as.environment("tools:positron")$.ps.ui.evaluateWhenClause(
  "config.workbench.colorTheme == 'Starless Monokai Atom'")` → expect `TRUE`.
- **Privacy.** That History `settings.json` also holds a live `claudeQuota.sessionKey`. The parser must
  read **only** `workbench.colorTheme` / `window.autoDetectColorScheme` and never echo, log or error
  with file contents.

Honest fragility (the researcher recommends against shipping it; I lean *ship it, gated and silent*
since it is best-effort and degrades to today's behaviour): `.ps.*` is private and carries
`# TODO: Unexport these methods` in ark's source; a client-only theme extension has no server-side
`package.json`; and the Positron console is *independently* themable (`positronConsole.background`), so
a correct global answer need not match the console. Note the **export** side is already correct — Phase
13d's `theme = "auto"` delegates to the browser via `prefers-color-scheme` + toggle hooks, the only
layer that can truly know. This phase closes the **console** gap only.

Also here: `tx_ide()` (rstudio/positron/vscode/terminal/jamovi), used to re-check the
`bit8 <- Sys.getenv("RSTUDIO") == "1"` 24-bit fallback; `set_color_palette(theme=)` must accept
`"auto"` ([tab_classes.R:3435](R/tab_classes.R#L3435) currently `stopifnot(theme %in% c("dark","light"))`).
Tests must not depend on the host IDE: unit-test the name→uiTheme resolver and the layering with
injected fixtures.

---

#### Context : Phases 14h to 14o

`dev/review_manual/tab_manual_review_pass_2.R` (+ the mid-session `tab_md_test_2.md`/`.htm`) is the
maintainer's second hands-on review of 1.4.0 on real survey data. Its `#` comments are the spec. Phases
14a–14g are committed; this plan turns pass 2 into phases 14h–14o, each a **fresh Claude Code session**.
The three hard ones (14m, 14n, 14o) **start with a design step, not with code**.

Every defect below was **reproduced and root-caused** during planning, not guessed. Five have causes
neither the review nor the roadmap had named, and three of those change the shape of the fix:

| #  | Symptom (maintainer)                                  | Verified root cause                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|----|-------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1  | `row_var` name repeats on every row (html, md, Excel) | **A Phase 14d regression.** `tab_md()`'s blanking loop ([tab_md.R:271](R/tab_md.R#L271)) is gated on `tab_vars`; 14d made `tab_compact()` correctly record `tab_vars = character(0)` ([tab_classes.R:1243](R/tab_classes.R#L1243)), so the loop went silent. The html engines never had blanking at all — they sidestep real tab_vars with `drop_tab_vars = TRUE`, which a compacted table never triggers. **md does not "already do it" — it stopped.**                                                                                                             |
| 2  | Excel title `levels by ROCK, JAZZ, CLASSIQUE +8 more` | `tab_render_vars()` DOES return `row_vars` (the source names, [tab.R:2653](R/tab.R#L2653)), but `prep_one_table()` rebuilds `vars` without it ([tab-export-prep.R:313](R/tab-export-prep.R#L313)) → `tab_xl`'s `.$vars$row_vars %||% .$vars$row_var` ([tab_xl.R:196](R/tab_xl.R#L196)) falls back to the literal `"levels"`. **One line.**                                                                                                                                                                                                                           |
| 3  | `(n=1 811)` padding wrong                             | The thousands separator is a **plain ASCII space** ([fmt_class.R:1992](R/fmt_class.R#L1992) `prettyNum(big.mark = " ")`) — half a digit wide in DejaVu Sans, and collapsed by CSS. `big.mark` never consulted `pad`, so the figure space that 14a-decision-1 introduced fixed the padding and the separator kept breaking it.                                                                                                                                                                                                                                        |
| 4  | Borders take the text colour                          | ✅ **FIXED in 14j.** **Not fixed in 13d/14e, only narrowed** — 3 docs + NEWS recorded it as fixed and nothing tested it. `.tabxplor-tab .tx-br{border-right:1px solid;}` (0,2,0 — [tab-css.R:199](R/tab-css.R#L199)) is a SHORTHAND: it resets `border-right-color` to `currentColor`, and it out-specifies `.tabxplor-tab td{border-color:…}` (0,1,1 — [tab-css.R:107](R/tab-css.R#L107)). Live on `tab(gss_cat, marital, c(race, relig), pct="row", color="diff")`: 6 of 140 `<td>`s carry both a border class and a colour class (incl. `.g1` grey → grey border). |
| 5  | "levels and Total columns very wide for nothing"      | ✅ **FIXED in 14j — but the roadmap's diagnosis below was WRONG.** The min-widths were a sideshow: the real cause was the colour legend in `<tfoot><td colspan>` (**327 chars on one line** vs a widest data cell of 23) deciding the table's max-content, so the table took the whole pane and auto layout padded every column. Both are fixed (`.tx-foot` + the min-widths deleted). Original note: the **only** widths in the stylesheet are `.tx-rv{min-width:10em}` and `.tx-tot{min-width:5.5em}` — exactly the two columns named.                              |
| 6  | Excel numbers in Condensed                            | The XML **already says** `DejaVu Sans` (verified: `Excel_test.xlsx` fonts 1–10). But `openxlsx2::create_font()` defaults **`scheme = "minor"`** and we never override it ([tab_xl.R:640](R/tab_xl.R#L640)), tagging every font as "the theme's minor font" — which `xlb_base_font()` sets to `font_text` = **DejaVu Sans Condensed** (verified in the file's `theme1.xml`).                                                                                                                                                                                          |
| 7  | `theme="auto"` always dark in the Viewer              | The dark layer is `@media (prefers-color-scheme: dark)` ([tab-css.R:222](R/tab-css.R#L222)), which in an Electron webview follows the **OS**, not Positron's colour theme — so toggling Positron cannot move it.                                                                                                                                                                                                                                                                                                                                                     |
| 8  | Transpose colours numerics wrongly                    | `tab_transpose()` copies **ONE representative column's** `fmt_col_attrs` onto every transposed column ([tab.R:2445-2456](R/tab.R#L2445)). A transposed column mixes variables; one fmt column cannot carry two `type`/`digits`/`color` values. **Unfixable at the object level.**                                                                                                                                                                                                                                                                                    |
| 9  | md spacers/separators bad in rendered html            | Verified against pandoc 3.7 on `tab_md_test_2.md`: spacers become real empty `<th></th>`/`<td></td>` columns with `padding:3px 4px`; the sub-table separators ([tab_md.R:490-511](R/tab_md.R#L490)) become literal rows of dashes.                                                                                                                                                                                                                                                                                                                                   |
| 10 | md rendered html: a border under **every** row        | **Not our CSS — the host's.** Quarto tags the table `class="caption-top table"`, and Bootstrap's `.table > :not(caption) > * > *` sets `border-bottom-width` on every cell (its `.table-borderless` sibling is in the file, so Bootstrap is confirmed present). `tab_css()` has no such rule: its border rules are all class-gated, and md-rendered html carries no classes — it only sets `border-color`, which is why the host's borders come out **black on every row**.                                                                                          |

##### Answering the review's two questions about the DOM capture

- **`dev/review_manual/Positron_Inspect_kable_theme_auto.html` is the OUTER workbench DOM** — our markup
  appears in it only as escaped console text (`&lt;table class="tabxplor-tab"&gt;`), so it cannot show the
  table's own `<body>`. What it *does* prove is decisive: the Viewer is a **cross-origin webview iframe**
  (`vscode-webview://…/index-external.html?…extensionId=positron.positron-r`). VS Code's
  `body.vscode-dark` sits on the outer host; it cannot reach our document. **Do not ship the `vscode-*`
  hooks the roadmap contemplated** — they would never fire.
- **Ask the maintainer for one live check** (14k): with `theme = "auto"` open in the Viewer, toggle
  **Windows** dark mode. If it flips, the webview follows the OS (expected); if it stays dark, something
  forces it and the diagnosis needs one more pass.

##### Settled this session (maintainer)

1. **One Total row**: collapse only when the total rows are identical **as displayed** — same rendered
   strings at the chosen digits. 17.22% and 17.31% both printing "17%" still collapse; the diffs/CI were
   computed per block beforehand and stay right.
2. **Transpose**: soft-deprecate `tab_transpose()`; flip only at export, on the render model.
3. **`theme="auto"`**: resolved R-side for the interactive Viewer, browser-side for files. ~~Option default
   `"light"` → `"auto"`.~~ **AMENDED 2026-07-17 (14k): the option default STAYS `"light"`; `"auto"` is
   opt-in only** — unlike the console, an export is read who-knows-where, so a dark table must be asked
   for, never inferred.
4. **Excel legend**: keep the lighter L ladder, boost chroma proportionally (tunable + preview).
5. **`var_names = c("both","rows","cols","none")`** — one shared arg; `tab_md(col_var_names=)` deprecated onto it.
6. **Title**: dependent first, decided by `pct`; max 2 names + "+N more".
7. **Figure space**: html + Excel only; console and md keep ASCII (monospace — an ASCII space is already
   exactly one digit wide there).
8. **`color_type`**: deprecate (confirmed vestigial).

---

#### Phase 14h — one digit-width space, everywhere it must align

Mechanical, cheap, no design. Do it first: 14i/14j/14m all read the padded output.

**Why** — finding 3, plus four siblings found with it:

- `big.mark = " "` ([fmt_class.R:1992](R/fmt_class.R#L1992)) and the same in `print_reg_footer`'s
  `fmt_val` ([tab_classes.R:714](R/tab_classes.R#L714)).
- The Excel star pad is ASCII: `formatC(st, width = -w)` ([tab_xl.R:425](R/tab_xl.R#L425)) — inconsistent
  with [:418](R/tab_xl.R#L418), which already passes `pad = fig_space`. Its width mask
  (`st[val & nzchar(st)]`) also differs from `format()`'s (`st[val]`, [fmt_class.R:2205](R/fmt_class.R#L2205));
  they agree only because `nchar("") == 0`.
- The mean/sd joiner is `unbrk` (U+202F, [fmt_class.R:2091](R/fmt_class.R#L2091)) — a narrow no-break space
  inside a cell whose digits must align. This is the review's "replace all unbreakable spaces with this
  good 1 digit sep, everywhere padding have to be aligned well".
- **sd-less mean cells are not padded**: `format()` right-pads the sd *text* ([:2089](R/fmt_class.R#L2089)),
  but a cell with no sd gets nothing → `1.0` and `1.7 (σ2.1)` do not align.
- **`bold_split` misses the mean/sd cell**: it covers `{}` templates only; the `disp_mean_sd` branch sets
  no `primary_nchar`, so a bold Total row bolds `4 (σ11)` whole — and bold DejaVu Sans is wider than plain,
  which is the review's "bold cells are not perfectly aligned with plain font weight cells".

**What**

1. `big.mark = pad` in `format.tabxplor_fmt()`. `pad` already resolves per backend
   ([fmt_class.R:1828](R/fmt_class.R#L1828): `" "` text / `fig_space` html; `tab_xl.R:418` passes it
   explicitly) → **console + md unchanged, html + Excel aligned, one line**. Same in `print_reg_footer`.
2. `pad`-ify the mean/sd joiner and the Excel star pad; unify the two star-width masks.
3. Pad an sd-less mean cell to the column's `unbrk (σ…)` width ([fmt_class.R:2085-2092](R/fmt_class.R#L2085)).
4. Extend `primary_nchar` to the mean/sd branch → `bold_split` bolds only the mean; `(σ…)` stays plain in
   md + both html engines. Excel needs nothing (mean and sd are separate columns there).
5. Delete `cross` ([utils.R:1185](R/utils.R#L1185)) — a dead byte-identical duplicate of `mult_sign`, zero
   call sites.

**Verify** — `test-display-grammar.R` / `test-fmt_class.R`: `format(html = TRUE)` of a 4-digit-`n` column
contains no ASCII space; a mean column with a mixed-NA sd has constant `nchar()`; `bold_split` splits a
mean cell at the mean. `test-tab_xl.R`: the star pad char. **Expected regen**: `_snaps/render-html.md`
only — `_snaps/golden.md` (console, `pad = " "`) must NOT move; if it does, the change leaked.

**Do not touch**: the U+202F in row LABELS ([tab_classes.R:2299-2315](R/tab_classes.R#L2299), the
`unbreakable_spaces` option). It is not padding; it is the separately-flagged "is this deliberate?" item.

##### Done (2026-07-17)

Full suite **FAIL 0 | WARN 0 | PASS 2923**; no `_golden/*.rds` and no `_color_golden/*.rds` touched.
Conscious regen of the two DISPLAY snapshots only (see below). New `test-digit-space.R` (26).

- **`big.mark = pad`** ([fmt_class.R](R/fmt_class.R)) — the one-line fix. `pad` already resolved per
  medium, so the console/markdown keep the ASCII space (already exactly one digit wide there) and
  html/Excel get the figure space. Proof: the whole `_snaps/render-html.md` diff is **28 ASCII spaces
  becoming U+2007** — mapping U+2007 to a space on both sides makes new and old byte-identical.
- **Excel star pad** ([tab_xl.R](R/tab_xl.R)) — `formatC(width = -w)` (ASCII) -> `str_pad(pad = fig_space)`,
  and the two star-width masks unified on `st[val]` ("" is width 0, so it IS the column max).
- **sd-less mean cells padded** to the `(sigma sd)` tail, so the MEANS align, not the cell edges.
  ⚠ Exact in the console/markdown (monospace) only: in html/Excel it lands within ~1 digit-width,
  because `(`, sigma and `)` are not digit-wide — **no run of spaces can match them**. An exact fix
  needs markup (a hidden tail), which belongs to 14j, not to `format()`.
- **`bold_split` reaches the mean/sd cell** — a bold row now renders `**47.2** (sigma17.3)`, the tail
  plain, exactly as a composite `{pct} (n={n})` cell does. `prim_nchar` moved above the
  `special_formatting` block (two branches write it now) and is attached only when something actually
  split, so the output stays attribute-free otherwise.
- **`cross` deleted** (a byte-identical duplicate of `mult_sign`, zero call sites). Two stale
  `fmt_class.R` header lines fixed in passing (`cross`; and "for type=mean, diff stores a RATIO" —
  false since Phase 2).

**BUG FOUND AND FIXED while building, which the suite did NOT catch**: the sd-less mask keyed on
`is.na(get_var(x))`, which is **also true of an EMPTY cell** — so padding pasted onto the NA and
produced the literal string `"NA       "`. Only the `na` argument (kable/md pass `""`) hid it; the
console, which keeps NA, printed it. Fixed with `!na_out`, regression-tested. The lesson for 14i/14j:
`is.na(var)` is not "has no sd", it is "has no sd **or is empty**".

**Deviation from the plan, deliberate**: the plan's "pad-ify the mean/sd joiner" was **not done**, and
`print_reg_footer`'s `big.mark` needed **no change**. The joiner (`unbrk`, U+202F) is a non-breaking
SEPARATOR, identical in every cell of the column, so it cannot misalign anything; making it `pad` would
lose the no-break property in md, and making it `fig_space` would move the console snapshot AND require
teaching the plot backend's three `unbrk`-strip sites ([tab_classes.R:1740](R/tab_classes.R#L1740),
[:1744](R/tab_classes.R#L1744), [:1871](R/tab_classes.R#L1871)) about a second exotic space — all for a
sub-glyph gain inside an approximation that is inherent (above). `print_reg_footer` is console-only, so
its ASCII `big.mark` was already the right glyph; the EXPORT footer renders through `format()`'s `gof`
token and got the fix for free.

**Snapshots regenerated (conscious)**: `_snaps/render-html.md` (the space swap, proven above) and
`_snaps/golden.md`. The latter was NOT expected to move; it did, for three reasons, each verified by
normalising every padding difference away and re-diffing — no number and no content changed:
(1) sd-less means are now padded; (2) bold mean cells split; (3) md's column budget grows by 2 on those
columns, because `md_extra()` correctly charges 4 markup columns for a partial-bold cell instead of 2.

---

#### Phase 14i — the variable-name model (one shared label column) (DONE — 2026-07-17)

Both findings fixed. Full suite **FAIL 0 | WARN 0 | PASS 3023** (+100); `document()` clean. Every
`_golden/*.rds` and `_color_golden/*.rds` **byte-identical**, `_snaps/render-html.md` unchanged; the
ONLY churn is one conscious `_snaps/golden.md` line (a tab_var label cell de-bolded — see below).
Browser/Excel sample: `dev/review_manual/phase14i_var_names.{html,md,xlsx}`.

**The shape: two roles, and both `var_names` drops live in the prep.** The insight that shrank the
phase — all four backends ALREADY gate the col_var span on `any(nzchar(cvh$label))` (md, kableExtra,
the html engine, and tab_xl's `has_span`, which also drives its geometry offset). So blanking
`col_var_header$label` in `prep_one_table()` drops the span row **everywhere with zero backend code**;
the row-side drop is the twin (drop the column before the role detection, and even `tab_plot` — which
reads no header model — inherits it). Two roles, deliberately distinct (conflating them would rotate
"Male"/"Female"), **mutually exclusive by construction** since `tab_compact()` bails on tab_vars:
- `roles$label_cols` + `roles$label_runs` — the leading factor cols whose value repeats down a block
  (the synthetic `row_var` col when `compacted`, else the kept `tab_vars`). ONE run model, four
  consumers: md blanks, the html engine `rowspan`s, Excel merges, tab_plot blanks.
- `roles$var_name_col` — the name-VALUED subset only: `var_names` drops it, its header always blanks,
  html/Excel rotate it, md italicises it, and it is never bold. A tab_var's values are LEVELS: merged
  and blanked, never dropped, never rotated.

- **Finding 2 (one line)**: `prep_one_table()`'s `vars` now carries `row_vars` + `compacted` (which
  `tab_render_vars()` has returned since 14d). The Excel title reads **"race, marital by relig"**, was
  "levels by relig". Unblocks 14l.
- **New shared `tab_label_runs()`** (`R/tab-export-prep.R`): per column `list(show, span)`. Runs come
  from the VALUES, not the grouping (`new_group` marks the full group COMBINATION for >= 2 tab_vars, so
  the outer tab_var's run would be cut; values also survive an ungrouping dplyr chain). NA = a
  continuation (md's rule verbatim: a materialised p-value row belongs to the block above). Nested
  outer -> inner, which md's naive per-column scan was not.
- **`var_names = c("both","rows","cols","none")`** + `options("tabxplor.var_names")`, on
  `tab_kable`/`tab_md`/`tab_xl`/`tab_plot`/`tab_export` via `resolve_export_opts()` (the formal sits
  **after `caption`** — every call site passes the ones above it positionally). It never touches a
  LEVEL column's header (`marital` on a single-row_var table, `year` on a kept tab_var): that header
  identifies the column, costs no width, and is the mirror of the col-side rule (which removes the span
  row, never the level names). **Maintainer's call this session.** `tab_md(col_var_names)` →
  `deprecate_soft` onto it (FALSE drops the col side of whatever `var_names` asks, so they compose);
  its use site and the `md_render_one()` formal are deleted — the prep's blank `label` is the gate now.
- **The literal `"row_var"` header is always dropped** (a bug fix, not a setting): one blank in
  `tab_col_var_header()`, whose suffix loop only ever visited LABELLED columns. md / kableExtra / html
  / xl all follow.
- **md**: name once, **italic** (the maintainer's call — it mirrors the `*ROCK*` col_var row and marks
  a NAME in a column that otherwise holds level labels; tab_var cells stay plain), never bold. ⚠ The
  bold exclusion had to reach the WIDTH pass too (`bold_rows_of()`): `md_extra()` and the `+4` charge
  markup width per column, so charging `**` the body no longer writes over-pads the column and the
  pipes stop lining up. **The one golden line**: a tab_var's `**Ensemble**` label cell is now
  `Ensemble` — exactly "bold not needed for row_vars names (or tab_vars names)"; the LEVEL
  (`**Total Ensemble**`) still bolds, and the width is unchanged.
- **html**: the roadmap's "watch out" was **free** — `td_html` is a list of per-column vectors joined by
  `do.call(paste0, ...)`, so a continuation row just contributes `""`. `rowspan` per run; `tx-vname`
  only where `span > 1` (a rotated 1-row cell just makes the row tall).
- **Excel**: `xlb_merge()` per run (`text_rotation` was already a per-cell matrix in the style dedup
  key, only `colnames_rotation` drove it) + 90 degrees + a narrow (3.5) name column. The label repeats
  are **blanked in the written data**: Excel keeps only a merged range's top-left value, so a repeat
  below it is an invisible ghost the user finds again on unmerging.

**Two deviations from the roadmap's letter, both deliberate:**
- **point 5's `writing-mode: sideways-lr` → `vertical-rl` + `rotate(180deg)`.** MDN still flags
  `sideways-*` experimental with patchy support. The replacement reads the same way (bottom-to-top,
  matching Excel's 90 degrees) and is supported since Chrome 8 / Safari 5.1. Test-locked.
- **point 6's md dash separator row → deferred to 14m** (maintainer's call): reusing `dash_line` today
  renders as a literal dash row in html, and 14m makes every separator row invisible at once.

**Found and fixed in passing**: `%||%` at `tab_xl.R:196` and `tab_classes.R:1244` is **base R >= 4.4
only** — DESCRIPTION says `R (>= 4.1)` and neither `data.table` nor `vctrs` (the only `import()`s)
exports it, so both errored on R 4.1-4.3. The package knows (three other sites carry the *"use explicit
is.null()"* comment); these two missed it. Step 1 deleted the `tab_xl` one outright.

**Flagged for the maintainer** (not fixed here): `prep$labels` and `prep$range_totcol` are both **dead**
— nothing reads either, and each costs a `compute` token on every kable/plot export. 14j item 5 already
schedules `tab_export_labels()`; `range_totcol` is scheduled nowhere.

##### Original plan (historical intent)

**Why** — findings 1 and 2. Today, in all three backends, a compacted table renders a column with the
literal header `row_var` and its value on every row.

**What**

1. **Pass `row_vars` + `compacted` through** `prep_one_table()`'s `vars`
   ([tab-export-prep.R:313](R/tab-export-prep.R#L313)). `tab_render_vars()` already returns both. This
   alone fixes the Excel title's `"levels"` and unblocks 14l.
2. **New shared role `roles$label_cols`** in `prep_one_table()` — the leading factor columns whose value
   repeats down a block: the synthetic `row_var` column when `compacted`, the `tab_vars` when kept. One
   definition, four consumers. This is the "shared function, be consistent between export types" the
   review asks for.
3. **New shared arg `var_names = c("both","rows","cols","none")`** (+ `options("tabxplor.var_names")`),
   resolved in `resolve_export_opts()`, on `tab_kable`/`tab_md`/`tab_xl`/`tab_plot`/`tab_export`.
   `tab_md(col_var_names=)` ([tab_md.R:82](R/tab_md.R#L82)) → `lifecycle::deprecate_soft` onto it.
   `"cols"` drops the row_var label column entirely; `"rows"` drops the col_var spanning row.
   The literal `"row_var"` **header** is always dropped (an internal name, never informative) — that is a
   bug fix, not a `var_names` setting.
4. **Render the name once**: md extends the existing blanking loop from `tab_vars` to `label_cols` (and
   blanks its header); html gives the label column a `rowspan` over the block; Excel merges the block's
   cells (`xlb_merge`).
5. **Vertical label** (html + Excel), so a long name costs no horizontal space and wraps into several
   vertical lines: html `writing-mode: sideways-lr` on a new class in `tab_css(chrome = TRUE)`; Excel
   reuses the **existing** `create_cell_style(text_rotation=)` machinery
   ([tab_xl.R:677](R/tab_xl.R#L677) — today only driven by `colnames_rotation`). The maintainer verified
   the Excel 90° result is good.
6. **md**: no bold on the label cell (exclude `label_cols` from the bold-row markup — the *level* stays
   bold when it is the reference row, which is wanted); keep the col_var name italic; add the **dash
   separator row under the col_var name row**, reusing `dash_line` from Step 12
   ([tab_md.R:490-511](R/tab_md.R#L490)).

**Watch out** — html `rowspan` breaks the engine's column-wise `paste0` assembly
([tab-render-html.R:319-359](R/tab-render-html.R#L319)): the label column must be built separately and
its repeat rows omitted. A 1-row block must fall back to horizontal text (a rotated cell in a 1-row block
is clipped in Excel and forces a tall row).

**Verify** — `test-export-prep.R` (`roles$label_cols` for compacted / tab_vars / plain; `vars$row_vars`
present); `test-tab_md.R` (name once, header blank, not bold, separator row); `test-render-html.R`
(rowspan, no repeat); `test-tab_xl.R` (merge + rotation); `test-export.R` (`var_names` on all four
exporters). gss_cat only.

---

#### Phase 14j — the html engine, pass 2 (borders + compactness) (DONE — 2026-07-17)

Both blocking defects fixed, and both had been **misdiagnosed in the records**. Full suite **FAIL 0 |
PASS 3046**; **no `_golden/*.rds` and no `_color_golden/*.rds` moved**. Browser sample:
`dev/review_manual/phase14j_html_engine.html`. Full record + the corrected history: decisions **§40**.

- **THE BORDER BUG WAS NEVER FIXED — 14e announced it, `NEWS.md` shipped the claim, and nothing tested
  it.** `.tx-br{border-right:1px solid}` is a SHORTHAND: it resets `border-right-color` to
  `currentColor` = the cell's palette hex, and at (0,2,0) it out-specifies `td{border-color:…}` (0,1,1).
  14e moved the geometry off inline styles, which removed the INLINE half only — a class still
  out-specifies the colour rule. The comment beside the code stated the mechanism correctly and drew the
  wrong conclusion. **Fix**: no border shorthand anywhere — `border-*-style` + `border-*-width` only, so
  the ONE `border-color` rule is the only thing that names a border colour. **Locked two ways**, since
  either alone missed it: `expect_no_match(css, "border-(top|right|bottom|left):")` per theme, AND a
  **multi-col_var** fixture asserting a `<td>` carries both a border class and a colour slot class (a
  single-col_var fixture never produces one — which is why two phases of tests saw nothing). Five stale
  records corrected: NEWS.md, CLAUDE.md ×2, architecture, decisions §38, + the code comments.
- **THE COMPACTNESS CAUSE WAS THE LEGEND, NOT THE MIN-WIDTHS.** Measured: the legend in
  `<tfoot><td colspan="7">` is **327 chars on one line** vs a widest data cell of 23, so IT decided the
  table's max-content; a table is `min(max-content, available)` wide, so it took the whole pane and auto
  layout spread the slack over every column ("a tvhours cell half numbers half blank"; pass-3's
  "genuinely occupy all horizontal space"). The 14e sample was already the experiment — its Table 1 has
  a legend and was called not compact, Table 2 has none and was called compact. Every pass-3 full-width
  example has `color = TRUE`, which is also the "inconsistent". **Fix** (maintainer's pick): keep the
  `<tfoot>`, wrap in `<div class="tx-foot">` + `width:0;min-width:100%` — `width:0` is definite so the
  cell contributes 0 to max-content; `min-width:100%` refills it once the table is sized by its data.
  The two `min-width`s are deleted too (the browser already content-sizes every column).
- **No `col_width` argument** (maintainer): `.tx-rv`/`.tx-tot`/`.tx-num` stay emitted, deliberately
  UNSTYLED — `.tx-rv{min-width:10em}` in the user's own CSS is the escape hatch, documented in a new
  `?tab_css` "Restyling a table" section. That is what 14e's no-inline-styles contract buys; a
  per-COLUMN width could not be a class and would break 13d's table-independent stylesheet.
- **`inst/tab.css` KEPT** (maintainer; the roadmap's "dead" holds only for the DEFAULT engine — it still
  styles `engine = "kableExtra"`). Only `.popover` ported to `tab_css(chrome = TRUE)`, **geometry only**
  (`max-width:none` + padding + nowrap; `.popover-body` BS4/5 + `.popover-content` BS3): bootstrap moves
  popovers to `<body>`, so the selector is as unscopable as `.tooltip-inner` — "one line, not 276px" is
  what every bootstrap popover wants, but tab.css's white-on-black is our taste and would repaint the
  HOST page's popovers. Unstyled, a popover inherits the host's theme. The html engine's popovers had
  never been styled at all.
- **`mean (sd)` header**: a numeric col_var's column is named after the variable → the name was said
  twice under its own span (three times in Excel, which splits a `_sd` sibling). The level header now
  names the STATISTIC: `mean (sd)` text / `mean`+`sd` Excel / `mean` when no sd shows (`ci = "cell"`),
  via `format()`'s OWN `disp_mean_sd` predicate so header and cells cannot drift. The `var_names`
  col-side drop MOVED into `tab_col_var_header(name_cols=)`, because it is one rule: *a level header may
  name the statistic only while the span names the variable*. Blanking the span afterwards (14i) left
  `var_names = "none"` + Excel headed `mean` with the variable named NOWHERE — latent bug, fixed. Both
  drops still live in the prep, so 14i's "no backend knows the argument exists" holds.
- **`tab_export_labels()` DELETED** + the `labels` slot (render model = `list(tables, meta)`): it walked
  every column of every table on 100% of exports and nothing read the result — `NULL` in practice anyway,
  the source `label` not surviving `tab()`. **`kable_tabxplor_style()`** soft-deprecated (exported, zero
  callers/tests, regex role detection hardcoded to "Total"/"Ensemble") + its latent `if (subtext != "")`
  length>1 error fixed. Cleanups: the duplicate `tx-bb` on the last row (`radd` appends, it is not a set
  union), `<tr class="">` → bare `<tr>`, the stale "kableExtra is the DEFAULT" header/doc/fallbacks.
- **NOT changed, deliberately**: padding (already `3px 4px`; the pass-2 padding complaint was the
  thousands separator, fixed in 14h) and hover (already kableExtra's `#FFFCE5`).

**Flagged for the maintainer**: `man/tab_css.Rd`'s "Two workflows" section ships raw markdown
(`**bold**`) into the help page, and `document()` emits 5 "could not resolve link" warnings whose topics
(`1`, `data-bs-theme=light`, …) are exactly the bracketed tokens of `tab_css(theme = "auto")`'s OUTPUT —
roxygen appears to EVALUATE the ```` ```{r, results="asis"} ```` chunk inside `\preformatted{}` at
document() time. **Pre-existing since 13d, reproduces at HEAD** (verified on a clean HEAD checkout).

---

#### Phase 14k — `theme = "auto"` resolution + the Positron Viewer (DONE — 2026-07-17)

Both Viewer defects fixed. Full suite **FAIL 0 | WARN 0 | PASS 3090**; `document()` clean (0 warnings,
was 89); **NO golden regeneration of any kind** — not one `_golden`/`_color_golden`/`_snaps` file moved.
Browser sample: `dev/review_manual/phase14k_viewer_page.html`. Full record: decisions **§41**.

- **THE SPLIT**: `"auto"` = *follow the reader — resolved by whoever can actually know*. A file or a
  knit keeps the 4-layer cascade (the browser is right there). An interactive Viewer print resolves in
  **R**, because the Viewer is an Electron webview whose `@media (prefers-color-scheme)` reports the
  **OS**, not Positron's theme — finding 7. `knit_print` is deliberately NOT overridden (dispatch walks
  the class vector to `knit_print.kableExtra`), so a Quarto page is never repainted.
- **THE ONE RULE**: *tabxplor paints a page only when tabxplor's own stylesheet ships with the table* —
  the 13d/14j legend discriminator. `engine == "html" && nzchar(css)` closes three holes at once, each
  of which would have made a table UNREADABLE, not merely ugly: `css = FALSE` (no stylesheet reaches the
  Viewer → a dark pane around an unstyled black-on-white table), the kableExtra engine (its
  `kable_material_dark` paints `#363640`, two-tone on our `#222222`; its degrade returns a bare `kbl()`),
  and it leaves the html degrade needing no guard (`render_html_degrade()` emits `class="tabxplor-tab"`).
- **No new mechanism**: `<div data-theme="dark">` makes the print page an explicit host toggle, so
  cascade layers 3/4 (0,2,x) beat the `@media` layer (0,1,x) both ways. Emitted only under `"auto"` —
  its absence proves the detector cannot leak into an explicit theme. No `!important` either:
  `save_html()` puts its `body{background-color:white}` + bootstrap in `<head>`, ours rides in the body.
- **`tx_page_style(theme)`** (R/tab-css.R) = the chrome of a page WE build; exactly two callers —
  `print.tabxplor_kable()` (passes a resolved theme) and `tab_html_string(standalone=TRUE)` (passes the
  intent, so `"auto"` keeps the `@media` cascade: that file is opened elsewhere).
  **`tx_kable_page(html, theme, detected = tx_detect_theme())`** (R/tab-render-html.R) = the pure seam;
  the probe is a DEFAULT ARG (the `tab-theme-detect.R` idiom), which is the only way to test this at all
  — testthat is never `interactive()`, so the gated-ON branch is unreachable from the suite.
- **Amendments**: item 2 (option default → auto) **reversed** — see settled decision 3 above. Item 4
  (dark tooltips) **skipped**, keeping 14j's geometry-only rule; the look, if it ever lands, is settled:
  both match the table (`#222222`/`#CECDC3`/1px `#707070`), in `tx_page_style()` only. Item 5 (no
  `vscode-*` hooks) **confirmed and recorded** beside `tx_dark_hooks`: the Viewer is a cross-origin
  webview iframe, so those hooks could never fire. The roadmap's OS-toggle live-check is **superseded**:
  the editor now wins by design, because the editor is the pane around the table.
- **Fixed in passing (§40's flag, pre-existing since 13d, verified on a clean HEAD clone)**: roxygen2
  (>= 7.1) EVALUATES a ` ```{r} ` chunk written in markdown, and `?tab_css`'s "Two workflows" section had
  one inside a raw-Rd `\preformatted{}` purely to SHOW it — so `document()` ran `tab_css()`, pasted the
  whole stylesheet into the help page, emitted **89** link warnings (one per bracketed CSS token) and
  leaked literal `**bold**`. Fixed with a four-backtick fence and no `{r}` info string. **89 → 0.**

---

#### Phase 14l — Excel, pass 2 (DONE — 2026-07-17)

Five items; full suite **FAIL 0 | WARN 0 | PASS 3134**; `document()` clean; **zero golden/snapshot churn
of any kind** (the acceptance gate — default `color_type` was already `"text"` everywhere, bg_legend is
legend-only). Full record: decisions **§42**. Two findings were PROVEN not guessed, and one contradicted
the roadmap plan.

- **Fonts** — the bug was PROVEN by unzipping `Excel_test.xlsx`: numbers were named `DejaVu Sans` yet
  drawn Condensed because `openxlsx2::create_font()` defaults `scheme = "minor"` (= "the theme's body
  font"), so Excel resolved from the theme (Condensed, written by `xlb_base_font`) and ignored the name.
  Fix = `scheme = ""` in the ONE `create_font()` call (`xl_style_registrar$font_id`). Fonts exposed as
  `options(tabxplor.xl_font_text / xl_font_num)`. Did NOT flip the base font (would widen every column —
  Excel measures width in the base-font digit). Honest limit: xlsx has no fallback list; the option is
  the escape hatch. One `scheme` survives (font 0, openxlsx2's base font — correct).
- **Title** — dependent-first, decided by the fmt `type` (`tab_title_rows_first()`: flip only when every
  directional col is `"col"`, so a mean/coef never votes); `max` 3→2. `tab_get_titles()`'s unused first
  param carried the table. `tab_reg` still mis-titles (no recorded `vars`; flagged, out of scope).
- **Legend chroma** — measurement CONTRADICTED the plan: APCA Lc is lightness-driven, so chroma alone
  can't fix faintness, and k>2.5 at by=0.2 caps out the gamut and flattens the ladder. Shipped
  `darken_for_legend(by=0.30, chroma_boost=2)` (Lc 55–75, in-gamut, proportions exact); constants
  regenerated by the tool; preview `dev/make_legend_preview.R` → `phase14l_legend.html`.
- **sd width** — `roles$sd_cols` (ONE definition, ungated by `var_names`), `tab_xl` width
  `max(5, colwidth*0.6)`.
- **`color_type` deprecated + inert** (~79 mentions): option + 7 public args + ~9 internal formals + 4
  branches → text family. Fixed the live `tab_xl` vs `tab_export` option inconsistency. `deprecate_warn`
  (not `_soft`) for the option (reaches indirect callers, dedups). Kept `get_color_style(type=)`,
  `set_color_style(type=)` custom_palette routing, `fmt_get_color_code(type=)`. Plan cross-check caught
  the A4 forwards (would have flooded snapshots with spurious warnings — deleted all four) + the A5
  sentinel-sequencing on `tab_xl`.

---

#### Phase 14m-ii — Monospace number font + number font conditional on significance stars (DONE)

Full suite **FAIL 0 | WARN 0 | PASS 3159**; `document()` clean; **no `.rds` golden / no snapshot moved**.
Full record: decisions **§44 + §44b**. The number font is now **conditional on stars**: proportional
**DejaVu Sans** by default, a monospace **Cascadia Mono** only when the table SHOWS significance stars
(where a proportional `*` breaks alignment). Trigger = `roles$has_stars` (computed in the prep). html:
`tab_css()` ships both `.tx-num` (DejaVu) and `.tx-has-stars .tx-num` (Cascadia + a body-only 1.1em size
bump, row height unchanged) and `render_html_engine()` adds the `tx-has-stars` class to the `<table>`;
Excel: `tab_xl()` gains `font_num_stars`, chosen per table; tab_plot: whole-body mono only when starred.
Options: `tabxplor.tab_kable_num_font(_stars)`, `tabxplor.xl_font_num(_stars)`, `tabxplor.plot_num_font`.
**L4** needs no code (star-padding works in mono). **Item A** (`tab_md()` figure-space) and **L5** (footer
`gof`/`pvalue` cells drop out of star-padding) are unchanged, orthogonal to the font — `_snaps/golden.md`
moved 48 lines (proven the pure ASCII→U+2007 swap in `n`-rows); `_snaps/render-html.md` did NOT move (its
snapshots strip the `<style>`, and the plain snapshot tables carry no `tx-has-stars`).

**Flagged**: (1) **tab_plot** whole-body mono (ggpubr 1.0.0 has no per-column font) fires only on a
starred plot now; reverts with `plot_num_font = ""`. (2) **Numbering tangle** : let’s say this it `14m-ii`, and next is `14m-iii`


#### Phase 14m-iii — `tab_md()`, pass 2 — (DONE)

Full design + specificity math + the verified pandoc constraints: **`dev/tabxplor_1.4.0_decisions.md`
§43** (read first). Findings 9 (spacer/separator cells render as ugly `<td>`s / literal dashes) + 10 (the
host draws a black border under every row) are ONE problem: `.tabxplor-tab` was built for the **html
engine** (where `.tabxplor-tab` IS the `<table>` and WE draw every border via per-cell classes); in **md**
`.tabxplor-tab` is a `<div>` WRAPPING a pandoc `<table>` we cannot class, so the HOST (Quarto/Bootstrap)
draws the borders and our `border-color` rule recolours them black. Confirmed against the maintainer's
real `tab_md_test_2.htm`.


**Organizing lever**: `.tabxplor-tab table …` is an **md-only selector** (needs a `table` descendant of
the div) — it never matches the html engine (where `.tabxplor-tab` IS the table), so md gets its own
chrome with zero risk to the html engine, no positional/`nth-child` rule (13d table-independence holds).

**Maintainer decisions (this session)** — (1) **blank-row separators**, not `.sep` dash rows: a rule is a
fully-empty row collapsed to a 1px border in CSS, no pandoc marker token in the raw `.md` (supersedes the
maintainer's own dash-row drawing); (2) **GFM-clean when plain**: the pandoc scaffold (the `:::` div +
the border-taming CSS) is gated on `styled = do_color || isTRUE(css)`; a plain uncoloured `tab_md()` stays
**byte-identical**.

**The mechanism (styled path only), four rules scoped `.tabxplor-tab table`** (details + specificity §43):
1. **Tame host borders** (10): `.tabxplor-tab table td,th{border-width:0;}` — width-only (does NOT touch
   the §40 `border-color` contract; a 0-width border never renders). Specificity (0,1,2) beats Bootstrap's
   `.table>:not(caption)>*>*` (0,1,1); place it **before** `.tabxplor-tab thead th` (tie → source order)
   so the header underline survives.
2. **Block rules as collapsed blank rows** (9 + the col_var-name underline): inject a fully-empty row after
   the col_var-name row and at each `roles$new_group` boundary;
   `.tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}`
   (border colour from the existing rule → theme-aware; pandoc keeps a fully-blank row as `<tr>` of
   `:empty` cells — verified).
3. **Collapse spacers** (9): `.tabxplor-tab table td:empty,th:empty{padding:0;}`.
4. **Decouple the `::: {.tabxplor-tab}` div from `<style>`**: emit the div whenever `styled` (not only
   `css = TRUE`), so the doc-level `tab_css()` workflow reaches the table; `<style>` still ships only with
   `css = TRUE`.

⚠ **DECISIVE 14m-i coupling (verified)**: a **figure-space** cell renders `<td> </td>` (NOT `:empty`); an
**ASCII / empty** cell renders `<td></td>` (`:empty`). So every `:empty` fix here REQUIRES blank/spacer
cells to stay ASCII-filled — former 14m-i's figure-space swap must be limited to padding **inside a value**
(thousands sep, `n=` alignment), never the pad of empty/spacer cells. **14v renamed 14m-i.**

**Cleanups**: the Step-12 dash-width arithmetic is MOOT (blank rows replace dash rows); remove the dead
`span` local ([tab_md.R ~L457](R/tab_md.R#L457)); `tab_md_css(tabs)` ignoring `tabs` is INTENTIONAL
(documented) — leave it.

**Verify** — a real pandoc/Bootstrap render (findings 9/10 gone; only provable in a Bootstrap host); a
fully-blank row survives as a `:has`-selected `<tr>`; the reset precedes `thead th`; the delimiter spacer
stays `-`; the gate (plain uncoloured = byte-identical, no `:::`; coloured carries the div even with
`css = FALSE`); no figure space in blank/spacer cells; a `levels="first"` + `tab_vars` snapshot.

**Flagged**: `:has()` (baseline since Dec 2023 → fine for 2026 Quarto; degrades to a blank gap row); the
plain path keeps dash separators (byte-clean) — unifying on blank rows there is a one-line gate.

---

#### Phase 14n — one Total row for several row_vars (DONE — 2026-07-17)

Both parts landed, DISPLAY-ONLY, in `R/tab_classes.R`; no fmt fields / attributes / public args; the core
`tab()` object keeps every Total row (`nrow(tab(...))` unchanged). Full suite **FAIL 0 | WARN 0 | PASS
3203**; `document()` clean; **no `.rds` golden and no `_snaps` moved** (both changes are display-only, and
no existing snapshot rendered a collapsing compacted table). Full record: decisions **§45**. Browser/Excel
samples: `dev/review_manual/phase14n_collapse.{html,xlsx}`.

- **Collapse (`tab_collapse_total_rows()`)** — the final step of `tab_materialize_extras()`, so it reaches
  the console + every export uniformly and all roles (`bold_rows` / `totblock_top`/`bottom` / `new_group` /
  references / tooltips) recompute on the collapsed table with ZERO per-backend code. Guard:
  `isTRUE(get_vars_attr()$compacted)` + `>= 2` Total rows — a single-row_var or a tab_vars table is never
  compacted, so both are untouched (a tab_vars table's per-subtable totals are real, not duplicates;
  `comp="all"` collapses via the same guard). Compares each block's whole **total BLOCK** (Total row +
  contiguous `"n"`/`"row_pct"` summary rows, gated to the same group; a `"pvalue"` row is block-specific
  and NOT swept in) "as displayed" via `format()` over EVERY fmt column — one canonical predicate for all
  backends. The BLOCK (not just the Total row) is what makes `pct="col"` correct: there the Total is always
  `"100%"` and the real base lives in the `n` row. Identical → drop all but the LAST block's total block
  (`tab[setdiff(seq_len(nrow), drop), ]`, global indices → class/attrs/grouping kept); different (only
  `na="drop"`) → keep all + `cli::cli_inform(.frequency="once")` naming `na="drop"`.
- **Per-block p-value rows (`tab_pvalue_lines()`)** — the `test` attr already carries a `row_var`
  discriminator, but the p-value rows were keyed on `tab_vars` only (empty for a compacted table), so two
  row_vars' tests collided into one col_var column → a `values not uniquely identified` list-col + a single
  mis-placed `row_var=NA` row. Fixed by keying on the table's GROUPING columns ∩ the test tibble (`row_var`
  for compacted, `tab_vars` otherwise → byte-identical there). **Also carries the `vars` attribute** through
  its `new_tab()` rebuild — a latent Phase 14d gap (the rebuild dropped `compacted`, which the collapse
  guard reads) that only this phase exposed. p-value rows SURVIVE the collapse: each variable keeps its own
  chi².

**Landmines / caveats (read before the next display-row change):**

- **`tab_pvalue_lines()`/`reg_footer_lines()` rebuild the tab with MORE rows via `new_tab()` and must
  re-list every table attribute by hand** (they cannot use `tab_restore()`, which preserves nrow). Phase
  14d added `vars` to `tab_attrs()` but NOT to these two rebuilds, so `compacted` was silently dropped
  after any materialised p-value row — invisible until a downstream reader (the collapse guard) needed it.
- **`add_n`/`add_pct`/`pvalue` summary rows are still detected by an English LABEL whitelist**
  (`{"n","row_pct","pvalue"}`, `R/tab-export-prep.R` `totblock_top/bottom`; the collapse reuses `"n"`/
  `"row_pct"`). The `row_pct` row's cells have display `"pct"` (indistinguishable from data by token), so a
  display-token sweep can't catch it — the real fix is a per-row role flag, still deferred.
- **The Phase 14a "one n row per sub-table" tests now assert the COLLAPSED count** under `na="keep"`; a
  non-collapsing `na="drop"` uneven fixture keeps the per-sub-table coverage. `test-render-html.R` /
  `test-tab_xl.R` "one-row block" fixtures moved off `levels=="Total"` (which the collapse drops) to a data
  level.
- **Not special-cased**: `add_n=FALSE` + `na="drop"` + `pct="row"` collapses silently if marginals round
  identical (follows the literal "identical as displayed" rule); a lone kept p-value row after a collapsed
  block still gets the `totblock` border box (cosmetic); transpose (14o) is unaffected (a transposed table
  has no `>= 2` Total ROWS → collapse no-ops; the flipped case is 14o's job).

##### Original plan (historical intent)

**Rule (settled)**: collapse when the per-block total rows are identical **as displayed** — same rendered
strings at the chosen digits. Otherwise keep them all and emit **one** message naming `na=` as the cause.
Rationale: the diffs and CI were computed per block beforehand and stay right, so a sub-tenth difference
behind the same printed "17%" is not a reason to show four identical-looking rows. Under `na="keep"` /
`"common_base"` / `"drop_all"` the totals are identical by construction; under `na="drop"` (the
maintainer's default) each row_var drops its own missing values, so they may genuinely differ.

**Design first (fresh session), thinking past the current implementation.** The framework was never
designed for several row_vars — `tab_compact()`'s synthetic `row_var`/`levels` columns are the scar, and
they are the root cause of findings 1, 2 and 8. Questions the design must answer **before** any code:

- **Where does the "as displayed" comparison live?** The rendered strings exist only in the prep — but
  Excel bypasses `format()` for values (it writes `get_num()` + a numFmt), so "as displayed" there means
  the numFmt-rounded value. One shared predicate for all four backends, or the rule silently diverges.
- **Display-only or build-time?** Display-only matches the 10i-B direction (add_n and p-value rows are
  already materialised by `tab_materialize_extras()`) and keeps the object honest: each block keeps its own
  reference row.
- The kept row is the **last** block's total, but the other blocks' `refrow` fields still point at their
  own (now hidden) rows. What then happens to bold / `tx-b`, the `totblock_top`/`bottom` borders, and the
  tooltips' `"ref"` marker?
- **tab_vars must keep their per-sub-table totals** (they are not duplicates — the review says so
  explicitly). And `comp = "all"`?
- **Do this BEFORE 14o**: one Total row → after the flip, one Total column, which is exactly what kills the
  `Total_DIPLOM` names the review saw.

**Verify** — `test-display-extras.R`: a gss_cat multi-row_var table with `na="keep"` collapses; a fixture
with genuinely different bases under `na="drop"` does not, and messages once; a tab_vars table is
unaffected; the collapse is display-only (`nrow(tab(...))` unchanged).

---

#### Phase 14o — transpose at the render level

**Why** — finding 8. `tab_transpose()` cannot be repaired at the object level; the review's own diagnosis
("colours must be calculated first from the not-transposed vctrs fields, then the transposition done not on
vctrs fields") is exactly right, and `Total_DIPLOM` is the tell.

**Design first (fresh session).** The flip belongs on the **render model**, where a cell is a string + slots
- roles and no per-column attribute is needed. Points to settle before code:

- `prep_one_table()` is per-**column** today (`ann` = a list per fmt column,
  [tab-export-prep.R:311-328](R/tab-export-prep.R#L311)). Transposing needs a per-**cell** matrix (text,
  text_slot, bg_slot, tooltip, bold, primary_nchar) + row/column role vectors. Decide: transpose a matrix
  built inside prep, or restructure `ann` into matrices for every backend.
- **Alignment**: `format()` pads per original column, and an original column becomes a transposed ROW.
  The composite inner-token alignment (`100% (n=  849)`) stays correct along that row — which is right,
  since a transposed column mixes variables. The **whole-cell** width must then be re-padded per transposed
  column.
- **Label columns**: the transposed table needs the (col_var, levels) pair mirroring (row_var, levels) —
  the review's "current first column name is CONCERTS, should be levels and second". Reuse 14i's `label_cols`.
- **Extras order**: `n` right after Total, numeric variables after both.
- `tab_transpose()` → `lifecycle::deprecate_soft` (settled). Re-point `test-transpose.R` (16 tests) at the
  render-level flip. Fix the stale "materialise → transpose" comments
  ([tab-export-prep.R:409-410](R/tab-export-prep.R#L409), [tab_xl.R:161-163](R/tab_xl.R#L161)) — 14d already
  reversed the order.

**Verify** — the pass-1 rule: `tab(pct="row") |> tab_export(transpose=TRUE)` renders like `tab(pct="col")`
for the 1×1 case; colours match the untransposed table cell-for-cell (the regression test that would have
caught finding 8); a mixed factor+numeric multi-row_var table transposes with no `Total_<var>` name and no
spurious numeric colour.

---

#### Phase 14 pass-3 roadmap Context (Phases 14p–14u)

`dev/review_manual/tab_manual_review_pass_3.R` is the maintainer's third hands-on review of tabxplor
1.4.0 on real survey data (`pc18` / `ct13_reg`) plus `gss_cat`. Its `#` comments are the spec. Phases
14a–14l are committed; 14m–14o are planned-but-unbuilt (design-first). This plan turns pass 3 into new
phases **14p–14u** (the maintainer pastes them into the CLAUDE.md roadmap; each phase = a fresh Claude
Code session; design-first phases start with a design task, not code).

Every defect was **reproduced and root-caused during planning** (three parallel Explore agents over the
color engine, `R/tab_reg.R`, and the tooltip/footer/`fct_recode_helper` paths). Several root causes were
new and change the shape of the fix. Tests must use `gss_cat`/`gss_cat`-derived data only — never `pc18`
or `ct13_reg` (confidential).

**Two mid-planning corrections from the maintainer (higher priority than the file's own items):**
- **A ≤1.3.1-breaking regression** not in the pass-3 file: `tab(relig)` and `tab(relig, pct="col")` — a
  single variable, no col_var — lost the `n` count column that 1.3.1 always showed; and the internal
  placeholder (`no_col_var`, sometimes the `Total` special name) is rendered as a col_var NAME (noise).
  "In the current state they would badly break past code from ≤1.3.1." → **Phase 14p** (elevated,
  do first). Same no-col_var `tab_plain(one_var)` shape as the `fct_recode_helper` bug.
- **The AME NA bug (Item E) is caused by ORDERED-FACTOR predictors, not by level names** — the maintainer
  verified it is not the `" - "` in the labels. `rincome` is `as.ordered()`. Fix: treat ordered factors
  as ordinary (unordered) factors in *predictors* (the `" - "` split found by the agent is a real but
  secondary latent fragility). → folded into **Phase 14r**.

**Settled with the maintainer this session (AskUserQuestion):**
1. **Empirical placement** — auto: **explicit columns when few** (binomial-coefficient, gaussian,
   poisson), **tooltip-only when many** (AME, multinomial). Statistically-adapted crude quantity per
   family.
2. **Number font** — make **DejaVu Sans Mono (monospace fallbacks) the default font for every
   number/fmt cell in every font-bearing export** (html engine, Excel, `tab_plot`), *always* (not only
   when stars are present). This is simpler and solider than the inline-block trick and dissolves the
   `*`-width problem: in a monospace font digits, `*`, `(`, `)`, `%`, space are all equal-width, so
   padding "just works". **md** keeps no font of its own → pad with figure space. **Text** (row labels,
   headers) stays DejaVu Sans Condensed — **except** Excel fmt-cells-shaped-as-text (ci="cell"/OR text),
   which get mono too (they carry stars). Revertible via options; the maintainer will visually review.

---

##### Root-cause table (for the implementing sessions)

| Item    | Symptom                                                                                                    | Verified root cause                                                                                                                                                                                                                                                                                                                                                   | File:line                                                      |
|---------|------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|
| REG     | `tab(relig)` / `tab(relig, pct="col")` lost the `n` count column (≤1.3.1-breaking)                         | `tab_plain()`'s no-col_var block produces the `n` count column, but it does not survive the `tab_build`/assemble/10i-B pipeline to `tab()`'s output                                                                                                                                                                                                                   | `R/tab.R:3576-3594`                                            |
| REG     | internal placeholder (`no_col_var`, `Total`) shown as a col_var NAME                                       | placeholder col_var names are not blanked in the col_var-header render                                                                                                                                                                                                                                                                                                | `R/tab.R:3487`, `R/tab-export-prep.R` (`tab_col_var_header`)   |
| C       | `fct_recode_helper(freq=TRUE)` errors `object 'pct' not found`                                             | `tab_plain(df, one_var, pct="col")` — same no-col_var shape; the single fmt column is named `"n"` (the injected `no_col_var` level); code refs bare `pct`/`n`                                                                                                                                                                                                         | `R/utils.R:282-304`                                            |
| B/J     | `grey_non_signif` legend says "Grey: not significantly different from the Total row" — statistically FALSE | Under `grey_non_signif` a cell is coloured only if significant **AND** effect ≥ first break, so an uncoloured cell may be significant-but-small (some carry stars). Only guarantee: **coloured ⇒ significant**                                                                                                                                                        | `R/fmt_class.R:3197-3203`                                      |
| D/J     | reg footer (GOF) + some reference cells render greyed/faint                                                | Greying paints every uncoloured non-`ref_alltot` cell grey (deliberate, to make coloured cells pop). `gof` cells and reg reference cells are NOT in the `ref_alltot` exclusion                                                                                                                                                                                        | `R/tab-export-prep.R:96-100`, `R/tab-render-html.R:337-338`    |
| L6      | footer tooltip shows nonsense (AIC "63 785" → `+6378526%`)                                                 | `gof` cell stores the stat in the `diff` field; the tooltip's `diff:` fragment fires (no `display`-kind gate)                                                                                                                                                                                                                                                         | `R/tab_classes.R:2182-2188`                                    |
| D       | reg tooltip `n:` is the whole-model N                                                                      | `n = rep(nobs, n_rows)` broadcast to every coefficient row                                                                                                                                                                                                                                                                                                            | `R/tab_reg.R:798,806,1023,1034`                                |
| E       | some significant AME cells are NA (`$20000 - 24999`, …)                                                    | **PRIMARY (maintainer-confirmed): the predictor is an ORDERED factor** → non-treatment contrasts → the marginaleffects AME does not key per-level to the skeleton → NA. SECONDARY (latent): `reg_marginal()` splits the contrast on the first `" - "` (`sub(" - .*$", "", contrast)`), truncating levels containing `" - "`. OR keys by `term`, unaffected either way | `R/tab_reg.R:959`, key `:999-1002`                             |
| G       | multinomial: borders drawn between a model's category columns                                              | each category column gets a DISTINCT `col_var` (its own per-category label)                                                                                                                                                                                                                                                                                           | `R/tab_reg.R:1059-1072`                                        |
| K       | vector-of-dependents + list-of-models errors                                                               | the two modes are mutually exclusive; guard forbids the combination                                                                                                                                                                                                                                                                                                   | `R/tab_reg.R:1797-1801`                                        |
| L1      | predictor row order not "complete model last"                                                              | `union_predictors = unique(flatten(models))` = first-appearance order; no complete-model concept                                                                                                                                                                                                                                                                      | `R/tab_reg.R:1877,1900`                                        |
| L2      | `compare="baseline"` warns "not nested or N differs"                                                       | nesting tests ONE direction only (`all(t_ref %in% t_full)`); and each model drops NA on its OWN vars → different N                                                                                                                                                                                                                                                    | guard `R/tab_reg.R:1247-1253`; drop `:631-632`                 |
| L3      | model name shown twice (col_var span + column header)                                                      | col_var span always drawn; no "column name == its col_var" collapse                                                                                                                                                                                                                                                                                                   | `R/tab-export-prep.R` (`tab_col_var_header`/`tab_header_runs`) |
| A/L4/L5 | stars/padding misalign in rendered html + Excel                                                            | `*` ≠ digit-width in proportional DejaVu Sans; padding uses digit-width figure space                                                                                                                                                                                                                                                                                  | `R/fmt_class.R:2235-2243`, `R/tab_xl.R:459-468`                |

---

#### Phase 14p — single-variable / no-col_var table correctness (ELEVATED — do first)

The ≤1.3.1-breaking regression the maintainer flagged mid-planning, plus the two other defects that share
the no-col_var `tab_plain(one_var)` shape (`fct_recode_helper` C, and the placeholder col_var noise).
**Reproduce against installed tabxplor 1.3.1 FIRST**, then fix. Regression-lock everything with tests —
the maintainer says these "would badly break past code from ≤1.3.1".

**Why + what**

1. **Restore the `n` count column for a single variable / no col_var** (`tab(relig)`, `tab(relig,
   pct="col")`). `tab_plain()`'s no-col_var block (`R/tab.R:3576-3594`) DOES build the `n` count column
   (renamed from the total; `set_type("n")`, `set_display("n")`), but it does not reach `tab()`'s output
   — the `tab_build`/`tab_assemble`/Phase-10i-B pipeline strips it (likely conflated with the display-only
   `add_n` `n` column that 10i-B removed). Root-cause where it is dropped and **restore it** so a one-way
   frequency table shows counts as in 1.3.1, WITHOUT undoing 10i-B for real crosstabs (the crosstab add_n
   `n` stays display-only; the no-col_var `n` is primary content and must survive). Decide the default
   shape to match 1.3.1 (levels + `n`, plus the pct column when a pct mode is set).
2. **Never render an internal placeholder as a col_var name** (`no_col_var`; sometimes the `Total`
   special name). Blank any col_var whose value is an internal placeholder in the col_var-header model
   (`tab_col_var_header()`/`tab_render_vars()`, `R/tab-export-prep.R`; note the existing partial guard at
   `R/tab.R:3487`). This is the col-var twin of the 14i variable-name blanking and overlaps L3/14s — do the
   general "placeholder col_var names are noise → blank" rule here since `tab(relig)` is where it bites.
3. **`fct_recode_helper(freq=TRUE)`** (Item C, `R/utils.R:282-304`): rides on the fixed shape. Stop
   referencing bare `pct`/`n` columns; use the single fmt column (named `"n"`) + accessors
   `get_pct(col)`/`get_n(col)` (or `format(col)` / `format(get_n(col))`). `is_totrow`/`get_pct`/`get_n`
   are vectorised over an fmt column (`R/fmt_class.R:518,1329,1314`). If step 1 restores a real `n` count
   column for `tab_plain(one_var)`, prefer reading that.

**Verify** — reproduce `tab(relig)` / `tab(relig, pct="col")` and compare to installed 1.3.1: the `n`
count column is present; no header shows `no_col_var`/`Total` as a variable name.
`fct_recode_helper(gss_cat, all_of("rincome"))` runs without error. New/expanded tests:
`test-tab.R` (single-variable frequency table has an `n` column across pct modes + weighted; placeholder
never appears as a col_var name), `test-fct-recode-helper.R` (freq TRUE/FALSE on 1 var and several
`gss_cat` factors — exported, currently untested).

##### Done (2026-07-18)

All three landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3212**; **no `.rds` golden and no
snapshot moved** (no existing snapshot rendered a bare `tab(one_var)`). Reproduced against **real CRAN
1.3.1** (installed in a temp lib — the machine's `1.3.1.9000` already carried the regression, so it was
useless as a reference). New `test-fct-recode-helper.R` (10); two new blocks in `test-tab.R`.

- **The `n` column was NOT dropped at build — it survives into `names(tab(relig))`.** The regression is
  at DISPLAY: `render_extras$add_n = TRUE` was set unconditionally, so `tab_materialize_extras()` ran
  `tab_add_n_pct()` + `tab_fold_addn_incell()`, whose first line returns `select(-any_of("n"))` when
  there is no `type == "row"` Total column to fold into — silently deleting the real frequency column.
  Fix ([R/tab.R](R/tab.R) `tab_assemble_tables`): gate the intent on a real col_var —
  `has_real_colvar = any(fmt & get_col_var(tab) != "no_col_var")`; a no-col_var table's `n`/`pct`/`wn`
  are primary content, not display extras, so `add_n`/`add_pct` are forced OFF (they stay ON for a
  numeric col_var, unchanged). This also means `add_n = FALSE` no longer drops the frequency `n` (it
  never should have — the `n` is not the add_n extra). The roadmap's "the fmt column is named `n`"
  diagnosis was wrong: the columns ARE `pct`/`n`, the object was fine, only the fold was wrong.
- **`no_col_var` sentinel** ([R/tab-export-prep.R](R/tab-export-prep.R)): added to the `real_col_vars`
  exclusion list (beside `all_col_vars`/`""`/`no`), so `tab_col_var_header()` never marks those columns
  `is_level` → no span label. One line; every backend (md/kable/html/xl) follows. (The "Total special
  name" case the review also named is already handled — a total column is excluded via `!totc`.)
- **`fct_recode_helper(freq = TRUE)`** ([R/utils.R](R/utils.R)): the real cause was **unqualified
  `filter`** — NOT imported, so it resolved to `stats::filter()`, which evaluated `!is_totrow(pct)`
  outside the data mask → "object 'pct' not found". Fixed by fully qualifying the non-base calls
  (`dplyr::filter`, `stringr::str_pad`/`str_length`, per the CLAUDE.md explicit-call rule); the columns
  `pct`/`n` were always there, so no accessor rewrite was needed.

---

#### Phase 14q — tab_reg readability: greying, footer, legend semantics

Groups Items **D (footer/ref greying)**, **J (ref greying + "why *** greyed" explanation)**,
**B (grey_non_signif legend)**, **I (ordinal Brant footer row)**. Colour/prep/footer only — NO tooltip
changes (those are 14r), so the two phases don't both touch the tooltip builder.

**Why + what**

1. **gof + reference cells escape greying.** Greying lives in `R/tab-export-prep.R:96-100`
   (`font = case_when(coloured ~ hex, ref_alltot ~ normal, TRUE ~ grey)`) and `R/tab-render-html.R:337`
   (`g1`/`g2` class). Add `display_primary(get_display(col)) %in% c("gof","blank")` to the "render normal"
   branch at BOTH sites so footer stats read black/bold. Reproduce and fix the reg **reference** cell
   greying (the "Emp. %" reference and the gaussian/OR reference show grey, must be black): confirm
   whether the reg reference row lands in `ref_alltot` (`get_reference(col,"all_totals")`) — the empirical
   `"Emp. %"` column is built with `ref="tot"` and may not set `in_refrow`, so it misses the exclusion.
   Fix by flagging the reg reference row (`as_refrow`/`in_refrow`) or extending the exclusion, whichever is
   cleaner. The maintainer's suggested "treat footer as total rows" is the same idea — but prefer the
   explicit `display`/`is-reference` gate over faking a total row (which would perturb other masks).
   Also, like in tab(), **reference row must by in bold**, including the text columns live "levels".
2. **grey_non_signif legend is false** (`R/fmt_class.R:3197-3203`). Reword the grey note so it is
   statistically true: the only guarantee is **coloured ⇒ significantly different from ‹ref› (‹method›)**;
   an uncoloured cell is *either* not significant *or* too small an effect to reach the first colour
   threshold. Propose EN wording: *"Coloured: significantly different from ‹the Total row› (‹Newcombe…›)
   and beyond the first colour threshold. Uncoloured: either not significant, or a difference too small
   to colour."* + FR (`po/R-fr.po`, recompile `.mo`). Do the terse console tag too (`:3133-3137`). Leave
   `guaranteed_effect` wording as-is (it is defensible) unless the same session confirms it also misreads.
   This *also answers Item J* ("why `***` but greyed") — significance ≠ colour is now stated; add a short
   sentence to `?tab`/`?color` (or the color-mode skill) so it is documented, not just legended.
3. **Brant PO p-value in the ordinal footer** (Item I). `reg_ordinal_diagnostic()`
   (`R/tab_reg.R:517-546`) already computes `bt["Omnibus","probability"]` but only warns and returns
   `invisible()`. Return the omnibus p; add a `brant_po = list(label = "Brant PO test", kind = "pvalue")`
   spec entry in `reg_footer_spec()` (`R/tab_classes.R:405-427`) + the `valid` list in
   `reg_footer_stats()` (`R/tab_reg.R:1217-1218`); emit a `brant_po` row from `reg_glance()`'s polr branch
   (or thread through `reg_gof_tibble()`). The `pvalue` kind renders in both `print_reg_footer` and
   `reg_footer_lines` with no extra work. Weighted (`svyolr`) → Brant degraded → skip the row.

**Verify** — a binomial/gaussian/OR reg table: footer stats and reference cells render black (not grey)
in console/kable/Excel; a significant-but-small cell stays uncoloured (intended) and the legend now says
so. Ordinal table shows a "Brant PO test p=…" footer line. Follow `/color-mode` for the legend edit.
Tests: `test-tab_reg-footer.R` (Brant row present for ordinal; reg reference + gof not greyed — assert on
`tab_export_prep()` roles / the render model, not a raw hex), `test-color-legend.R` (grey_non_signif
wording; add a FR case if the harness allows — see the CI gettext note in the roadmap).

##### Done (2026-07-18)

All three items landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3230**; `document()` clean; **NO
golden and NO snapshot moved** (the reg tables + the grey_non_signif legend are not snapshotted; the
legend wording is asserted directly). Browser sample: `dev/review_manual/phase14q_reg_readability.html`.

- **Greying (Items D/J).** The root cause was NOT that gof cells miss the exclusion generically — it was
  a MISMATCH: the empirical `Emp. %` column carries `ref_type = "tot"` yet marks its reference CATEGORY
  via `in_refrow`, so `get_reference("all_totals")` (which returns the total ROW under ref = "tot")
  returned empty and greyed the reference cells. Introduced ONE shared "black anchor" concept:
  + `fmt_col_ann()` ([R/tab-export-prep.R](R/tab-export-prep.R)) now computes `keep_black = ref_alltot |
    is_refrow(col)` and drives `font`/`bold` off it (returns the mask too). For a crosstab `is_refrow`
    is a subset of `ref_alltot`, so byte-identical there — only reg reference columns change.
  + The GOF FOOTER rows are un-greyed at the TABLE level in `prep_one_table()`: a footer row is one where
    EVERY fmt cell is a footer stat (display `gof`/`pvalue`/`blank`). A crosstab chi2 pvalue row is NOT
    (its other cells stay `pct`), so this never touches a crosstab and needs no reg gate — and it catches
    the `pvalue` footer rows (LR vs null) that a per-cell `%in% c("gof","blank")` rule would have missed.
    The whole footer row goes black + bold (font + keep_black + `bold_rows` union so LABELS bold too).
  + The html engine ([R/tab-render-html.R](R/tab-render-html.R)) reads `a$keep_black` instead of
    `a$ref_alltot`; the console `pillar_shaft` greying ([R/fmt_class.R](R/fmt_class.R)) ORs `is_refrow(x)`
    into its `totals` exempt set. Deliberately kept `ann$ref_alltot` semantic (feeds the reference
    intercept + `tab_bold_rows`); the styling decision is the separate `keep_black`.
- **Legend (Item B).** The `grey_non_signif` prose note was statistically false. Rewrote to state the true
  guarantee — *"Coloured: significantly different from ‹ref› (‹method›), by at least the first colour
  threshold. Uncoloured: either not significant, or too small a difference to colour."* — EN + FR
  (`po/R-fr.po` + `.mo` recompiled), and documented under `color_signif` in `?tab`. The terse console tag
  (`[significant only]`) was left — it already describes the colouring rule correctly (coloured ⇒
  significant). `guaranteed_effect` left as-is (defensible). This also answers Item J's `***`-but-grey.
- **Brant (Item I).** `reg_ordinal_diagnostic()` now RETURNS the omnibus p (still warns); `reg_fit_ordinal`
  stashes it as `attr(fit, "brant_po")` (computed once, at fit time); `reg_glance()` emits a `brant_po`
  row for unweighted ordinal; `reg_footer_spec()` gains `brant_po = list(label = "Brant PO test", kind =
  "pvalue")` + the default/valid stats lists. Weighted (svyolr) has no Brant fit → attr absent → skipped.

**Landmine for the next reg session**: the footer-row detection ("all fmt cells are gof/pvalue/blank")
is the robust, language-independent alternative to the `reg_footer_labels()` English-label match that
`tot_block` still uses — a real per-row role flag would retire both, but that is deferred.

---

#### Phase 14r — tab_reg tooltips + the AME NA bug

Groups **L6 (remove footer tooltips)**, **D (row-level n)**, **E (OR always in tooltip)**, **E (AME NA
bug)**. Tooltip builder + `reg_marginal`. Do this **before 14t** (empirical builds on a correct AME).

**Why + what**

1. **AME NA bug — PRIMARY cause: ordered-factor predictors** (maintainer-confirmed). When a predictor is
   an ordered factor (e.g. `as.ordered(rincome)`), the model uses non-treatment (polynomial) contrasts,
   so the marginaleffects AME does not key per-level to the skeleton → NA (while the OR still shows). Fix:
   **treat ordered factors as ordinary (unordered) factors in PREDICTORS**, coerced uniformly and early
   (in `reg_prep`/`reg_apply_references`, before skeleton + fit + `reg_marginal`), so contrasts are
   treatment-style and OR/AME both key per-level. Only PREDICTORS are de-ordered; a `family="ordinal"`
   DEPENDENT stays ordered. **SECONDARY (latent hardening):** `reg_marginal()` (`R/tab_reg.R:959`) splits
   the contrast on the first `" - "` (`sub(" - .*$", "", ac$contrast)`), truncating an unordered level
   that itself contains `" - "`; key on marginaleffects' **structured columns** (or strip the *known*
   reference suffix) instead — same care for the `lnor` branch (`:958`, `[^)]+` breaks on a `)`). The join
   key is `:999-1002`. Add a `gss_cat` regression test with an **ordered-factor predictor** asserting the
   AME is non-NA where the OR is significant (and a secondary case with a `" - "` unordered level).
2. **Row-level n in the tooltip** (Item D). `reg_effect_column`/`reg_marginal_column` set
   `n = rep(nobs, n_rows)` (`R/tab_reg.R:798,806,1023,1034`) → every row shows the whole-model N (already
   in the footer). Pass the **per-row level n** where it exists (e.g. the empirical/level count), else
   `NA_integer_`. `cond_n` (`R/tab_classes.R:2274`) then drops the fragment automatically where NA.
3. **OR always in the tooltip** (Item E). Even under `effect="ame"`, keep the model OR available in the
   tooltip. Store the coefficient OR in the column's `or` field at build time (display stays the AME);
   `cond_or` (`R/tab_classes.R:2258-2262`, `type %in% c("col","row") & !is.na(get_or)`) then surfaces it.
   General principle the maintainer states: any fmt field that helps interpret the model is a tooltip
   candidate — but keep it read-only in the tooltip, never displayed.
4. **No tooltips on footer/gof rows** (L6). Gate `tab_kable_print_tooltip()` (`R/tab_classes.R:2147`) so a
   cell with `display_primary(get_display(x)) %in% c("gof","blank")` returns `""` (kills the nonsense
   `diff: +6378526%` on AIC). Do it once at the top of the builder (both engines call it).

**Verify** — an AME reg table: no NA AME where the OR is significant; tooltip shows OR + a row-level n (or
none); a footer cell has an empty tooltip. Snapshot regen limited to `_snaps/render-html.md` (tooltip
text). Tests in `test-tab_reg-display.R`.

##### Done (2026-07-18)

All four landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3238**; **NO golden and NO snapshot moved**
(the reg tables + tooltips are not snapshotted). Sample: `dev/review_manual/phase14r_ame_tooltip.html`.

- **The AME NA bug has TWO independent causes, not one.** Verified: `marginaleffects::avg_comparisons()`
  produces the SAME `"Level - Reference"` labels + estimates for an ordered AND an unordered fit, so the
  ordered factor does NOT break the AME. The NA cells were the `" - "` SPLIT: `sub(" - .*$", "", contrast)`
  truncated `"$20000 - 24999 - $1000 to 2999"` → `"$20000"` → no skeleton match → NA (exactly the levels
  the maintainer flagged). The ordered factor SEPARATELY breaks the COEFFICIENT path: glm/polr give
  polynomial terms (`x.L`/`x.Q`) that don't align → an all-NA OR column (the "remove ordered to not break
  the model" the maintainer did by hand in Pass 4). So both the roadmap's PRIMARY (de-order) and SECONDARY
  (robust split) are real and both needed:
  + de-order in `reg_fit` ([R/tab_reg.R](R/tab_reg.R)): `factor(fct_drop(as.factor(.)), ordered = FALSE)`
    (was `as.factor()`, which KEEPS the ordered class). Predictors only; the ordinal outcome stays ordered.
  + `reg_marginal()` strips the KNOWN prefix + reference suffix by `substr` instead of splitting on the
    first `" - "` / first `")"` — handles a level containing `" - "` or `")"`. ⚠ The lnor contrast is
    `ln(odds(<Level>) / odds(<Ref>))` with a DOUBLE closing paren; the suffix must include both (a test
    caught the off-by-one).
- **Row-level n (D)**: the model effect columns (`reg_column` OR/β, `reg_marginal_column` AME) set
  `n = rep(NA_integer_, n_rows)` — the whole-model N is in the footer, not a per-cell tooltip. (⚠ `n`
  drives `fmt()`'s recycle size, so it must be `rep(NA, n_rows)`, not a scalar.) The empirical columns
  keep their real per-LEVEL n (`emp$emp_n`), which is what the maintainer wanted surfaced.
- **OR in the AME tooltip (E)**: the binomial single-outcome AME column carries the coefficient OR
  (`exp(tidy$estimate)`, keyed to the skeleton by term) in its `or` field via a new `reg_marginal_column
  (or_tip=)` arg. Read-only — the AME display / colour never read `or` (colour goldens byte-identical), so
  `cond_or` surfaces `OR: 0.42` on hover with zero display/colour impact.
- **No footer tooltips (L6)**: one line at the end of `tab_kable_print_tooltip()` blanks any cell whose
  display is `gof`/`blank` (kills the `diff: +6378526%` on an AIC stored in the `diff` field).

---

#### Phase 14s — tab_reg multinomial: one col_var per model + drop redundant name row

Groups **G** and **L3**. Both concern the col_var header of reg tables. Byte-identical for crosstabs.

**Why + what**

1. **One col_var per multinomial model** (Item G). `reg_columns_multinom()` (`R/tab_reg.R:1059-1072`)
   passes each per-category label as the column's `col_var`, so every category column is a distinct
   col_var → borders between them. Pass a **shared model id** (e.g. `sp$dependent` or the model's label) as
   `col_var` while keeping the per-category `lab` as the visible column NAME. Result: a spanning header
   names the model once over all its category columns, and inter-category borders disappear (borders are
   drawn between different col_vars). Apply the same to the MNL AME / vs-rest columns
   (`reg_marginal_column(col_var=…)` at `R/tab_reg.R:1435,1456`). The GOF footer keys by the make.unique'd
   output label (`fit_first_col`), so changing `col_var` is display/border-only and footer-safe.
2. **Drop the redundant variable-name row** (L3). Rule (maintainer's): if EVERY fmt column's own name
   equals its `col_var`, silently drop the col_var spanning-name row. Implement in the 14i/14j col_var
   header model (`tab_col_var_header()`/`tab_header_runs()`, `R/tab-export-prep.R`) so it composes with the
   existing `var_names` arg and touches no backend. This covers the single-model reg table where the
   column is named after the dependent and the col_var is the same. With (1) it also means a multinomial
   model's shared-col_var header shows once (meaningful) rather than duplicating each column name.

**Verify** — `tab_reg(gss_cat, "marital", c("race","rincome"), family="multinomial")` renders one span per
model, no borders between category columns; a single-model OR table shows no duplicate name row. Tests in
`test-tab_reg.R` + a render assertion in `test-render-html.R`/`test-export-prep.R` (`tab_header_runs`
collapse). Confirm crosstab goldens unchanged (the rule fires only when name==col_var for ALL columns —
a crosstab has level names ≠ col_var).

##### Done (2026-07-18)

Both landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3243**; **NO golden and NO snapshot moved**
(no reg table is snapshotted; crosstab headers are byte-identical). Sample:
`dev/review_manual/phase14s_mnl.html`.

- **G (one col_var per MNL model)**: the three MNL column builders (`reg_columns_multinom`, the MNL AME
  per-category, the MNL "vs rest") pass `sp$label` (the unique model id) as the `col_var` while keeping
  the per-category `lab` as the visible NAME. Borders are drawn at col_var TRANSITIONS (`new_col_var`),
  so a shared col_var removes the inter-category border (verified: `new_col_var` no longer lists the 2nd
  category column) and the model name spans the categories once. The GOF footer keys by the output LABEL
  (`fit_first_col`), so col_var is display/border-only — footer-safe.
- **L3 (drop the redundant name row)**: in `tab_col_var_header()` ([R/tab-export-prep.R](R/tab-export-prep.R)),
  after the level-header rewrites, blank the whole span `label` when `all(clean[level] == col_var[level])`.
  ⚠ Compare the CLEAN (displayed) header, NOT the raw column name: a numeric col_var has raw name ==
  col_var ("tvhours") but a clean header of "mean (sd)", so comparing raw names would have wrongly dropped
  its span (and lost the variable name). A crosstab (level "Black" != col_var "race") is never affected;
  a single-model reg ("Married: OR" == "Married: OR") drops the span, showing the name once.

---

#### Phase 14t — DESIGN-FIRST: the empirical (crude) framework across families/effects

Groups **F (rename `empirical_OR`→`empirical` + cross-family)**, **D (empirical relation)**,
**H (multinomial×AME empirical hack)**. **Start with a design + web-research task in a fresh session,
out of the box** — the statistical content must be sound/standard, and the placement uses the vctrs
fields (`/vctrs-field`). Do **after 14r** (correct AME + tooltip infra).

**Design step (first, before code):**
- **Statistical framework — what is the "empirical" analogue per family?** The rule (maintainer): the
  empirical value is the crude quantity that *is* the modelised quantity when there is a single predictor.
  Web-research + settle, per family/effect (write the result into `dev/tabxplor_1.4.0_decisions.md` §37):
  + binomial coefficient → crude OR + crude % per level, diff from ref (today's `empirical_OR`).
  + binomial AME → observed % per level (predicted-prob analogue) + empirical diff from ref.
  + gaussian → mean per level of the predictor + diff of means from ref.
  + poisson/IRR → crude rate + rate-ratio from ref.
  + multinomial → observed category % + empirical diff (per category).
  Confirm this is the standard "unadjusted vs adjusted" comparison (good practice), not a bespoke thing.
- **Placement (settled: auto columns-when-few / tooltip-when-many).** Binomial-coefficient, gaussian,
  poisson → explicit `"Emp. …"` columns (reuse `reg_empirical_columns`, `R/tab_reg.R:883-904`). AME and
  multinomial → **tooltip only** (a column per category × empirical would explode the layout). Design the
  **field hack** for the tooltip case: store the empirical pct/diff in fmt fields not otherwise displayed
  for that column type so the tooltip surfaces them WITHOUT disturbing `tab()`/reg display or other
  tooltips (the maintainer's explicit worry). Candidate: the `ratio` field (or a clearly-reserved reg
  slot) read only by a new tooltip fragment gated on a reg marker. Resolve with `/vctrs-field`; do NOT add
  a new fmt field if an unused one suffices.
- **Rename** `empirical_OR` → `empirical` (hard rename, no soft-deprecate — new in 1.4.0). It becomes
  family/effect-general; drop the "single binary logistic (coefficient)" guard, replacing it with
  per-family/per-effect dispatch (columns vs tooltip). `trials` stays; the empirical binomial base is the
  weighted 2×2 as today.

**Then implement** the designed framework + tests (`test-tab_reg.R`): empirical columns for binomial-coef/
gaussian/poisson (parity vs a hand crude computation), empirical tooltip for AME/multinomial (the field
carries the right value; `tab()` tooltips unaffected — assert a crosstab tooltip is byte-identical).

**Caveat to flag to the maintainer:** the multinomial×AME empirical-in-tooltip is a genuinely marginal
feature (a rarely-read crude-vs-adjusted check on a crowded table). If the field hack proves fragile,
make it opt-in or defer — surface this during the design step rather than forcing a hack.

##### Done (partial) + DESIGN (2026-07-18) — full design in `dev/tabxplor_1.4.0_decisions.md` §45

The tooltip field-hack IS fragile (proven, not guessed), so per the maintainer's own guidance the
fragile parts are DEFERRED with a written design; the solid, colour-safe core landed. Full suite
**FAIL 0 | WARN 0 | SKIP 4 | PASS 3246**; `document()` clean; **no golden / no snapshot moved**.

- **LANDED (solid)**: `empirical_OR` → **`empirical`** (rename; `tab_reg()` keeps `empirical_OR =
  lifecycle::deprecated()` warning-alias, the wrappers took the new name). The binomial crude `Emp. %`
  (coloured by crude risk-diff) + `Emp. OR` columns now show for BOTH `effect = "coefficient"` and
  `effect = "ame"` (widened from coefficient-only — answers the review's "base % + empirical diff" and
  un-blocks the `ame + empirical` error). Non-binomial / multinomial: a MESSAGE + ignore, not an abort.
- **DEFERRED (needs a maintainer visual/design call, §45)**: (1) gaussian/poisson explicit crude columns
  — the `Emp. mean` colour is under-specified (a `type="mean"` `color="diff"` column needs a reference
  variance the crude path lacks; options in §45). (2) the multinomial×AME crude-in-tooltip — a REAL
  field conflict: the tooltip reads `ratio`/`ctr`/`mean` for row/mean columns, so any stash makes a
  spurious "ratio:"/"contrib:" line. A clean fix needs a dedicated reg-only tooltip field (shared-builder
  cost) — the maintainer flagged this feature "marginal", so it stays deferred/opt-in.
- ⚠ **The roadmap's "§37" for this never existed** — the design is now §45.

---

#### Phase 14u — DESIGN-FIRST: tab_reg model-comparison structure

Groups **K (dependents × models → list of tabs)**, **L1 (complete-model ordering)**, **L2 (bidirectional
nesting + `na="drop_all"`)**. **Start with a short design task** — the three interact (a per-dependent
list, each a model comparison, on a shared complete-case population).

**Design + what**

1. **Vector-of-dependents × list-of-models → a list of tabs** (K). Today the two modes are exclusive
   (guard `R/tab_reg.R:1797-1801`); `reg_build()` already handles a multi-spec comparison. Relax the guard
   and, when BOTH are given, loop dependents on the outside — each iteration builds `specs` from the model
   list with that dependent, calls `reg_build`, and the results are wrapped as a `tabxplor_tabs` list (so
   `tab_export("xl")` yields one sheet per dependent). `trials` must accept a **vector** (one per
   dependent). Decide the per-table labelling (model-name labels within each dependent's table).
2. **Complete-model predictor ordering** (L1). Where `union_predictors` is built (`R/tab_reg.R:1877/1900`,
   or before `reg_skeleton` at `:1407`): if one model's predictor set is a **superset of every other
   model's** (a "complete" model), reorder the union to that model's own order (placed at the end as the
   maintainer expects). If no complete model exists, keep first-appearance order. Everything downstream
   keys by `(var,level)`/`term` and follows the skeleton's `fct_inorder`, so reordering the union suffices.
3. **Bidirectional nesting + `na="drop_all"`** (L2). Two fixes for the "not nested or N differs" warning:
   + `reg_compare_guard()` (`R/tab_reg.R:1247-1253`) tests `all(t_ref %in% t_full)` only — also accept the
     reverse (`all(t_full %in% t_ref)`), so `baseline="complet"` (the baseline is the *superset*) is
     recognised as nested. Pick the LR direction from whichever is the sub-model.
   + Add opt-in **`na = "drop_all"`** (mirroring `tab()`): pre-compute a shared complete-case mask over the
     union of all specs' predictors + dependent + design vars, and fit every model on that population
     (`reg_fit` currently drops NA per-model at `:631-632`). Equal N then holds for genuinely-nested specs,
     enabling the LR test. Document that it changes ALL estimates (shared population), hence opt-in.

**Verify** — `tab_reg(gss_cat, c("married", <2nd binary>), list(a=…, b=…), family="binomial", trials=c(…))`
returns a list of tabs, `tab_export("xl")` writes one sheet each; a comparison with a superset baseline
runs an LR test (no AIC-fallback warning) under `na="drop_all"`; a complete model's predictors sit last.
Tests in `test-tab_reg.R` (list shape, ordering, nesting both directions, drop_all equal-N).

##### Done (2026-07-18)

All four landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3257**; `document()` clean; **no golden /
no snapshot moved**. Sample: `dev/review_manual/phase14u_multi_dep.xlsx`.

- **K (dependents × models → list)**: a `tab_reg()` recursion at the TOP of the body (before the
  design/family/spec machinery) intercepts `is.list(predictors) && length(dependent) > 1` and loops the
  dependents, each an ordinary single-dependent comparison, wrapped as `new_tabxplor_tabs()`. Reuses
  every arg/message/family-detect. `trials` split per dependent (vector / named). Placed before the
  design extraction so a survey design recurses intact.
- **tab_xl one sheet per dependent** (K's acceptance): a `tabxplor_tabs` is an EXPLICIT collection of
  independent tables, so `sheets = "auto"` now defaults it to `"tabs"` (one sheet each) — the old
  col-var "auto" STACKING (same col_vars → one sheet) merged K's tables (all share the model-label
  col_vars). Also, a NAMED `tabxplor_tabs` uses its NAMES as titles/sheet-names (K → the dependents;
  output_list → the row_vars), which sidesteps the reg mis-titling for the sheet name. This also affects
  a several-row_vars `output_list` → xl (now one sheet each, named by row_var — cleaner, matches "never
  merge a list").
- **L1 (complete-model ordering)**: `reg_order_union()` — if one model's predictor set is a superset of
  every other's, use THAT model's own predictor order; else first-appearance. One line at the union.
- **L2 (bidirectional nesting)**: `reg_compare_guard()` returns a DIRECTION (`1`/`-1`/`0`) and
  `reg_compare_rows()` passes the SUB-model to `anova()` first — so a superset `baseline` is nested (LR),
  not the AIC fallback. `na = "drop_all"` (new arg) pre-filters `data` to the shared complete cases (the
  union of predictors + dependent + design vars) so nested models get equal N; ignored for a prebuilt
  design. Documented as opt-in (it changes all estimates).

**Landmine**: the reg tables still record no `vars` attribute, so their DERIVED title is mis-generated
("... by levels (tabbed by row_var)") — the sheet-name fix routes around it via the list names, but a
single unnamed reg table exported alone still mis-titles (the 14l flag, still open).

---

#### Phase 14v-i — improve the `empirical = TRUE` framework

`empirical = TRUE` (renamed from `empirical_OR`) adds the DESCRIPTIVE crude companion of the model effect: the *unadjusted* bivariate association between a factor predictor and the outcome, which IS the modelised quantity when there is a single predictor. This is the standard "crude vs adjusted" comparison (epidemiology / social science good practice): a large gap between the crude and the model column signals confounding /adjustment.

##### The family-appropriate crude quantity (design)

| family            | modelised (adjusted)          | empirical (crude / unadjusted)                            | placement          |
|-------------------|-------------------------------|-----------------------------------------------------------|--------------------|
| binomial (OR)     | model OR per level            | crude % (P(pos\|level)) + crude OR (risk-diff: tooltips)  | explicit columns ✅ |
| binomial (AME)    | avg marginal effect (model %) | observed % per level + observed risk-difference           | explicit columns ⏸ |
| gaussian          | beta (adjusted mean-diff)     | crude mean(Y\|level) + mean-diff ? **problem, see below** | explicit columns ⏸ |
| poisson (IRR)     | model IRR                     | crude rate (mean count) + crude rate-ratio                | explicit columns ⏸ |
| multinomial (RRR) | one RRR col per category      | observed relative-risk ratio (RRR), PER category          | tooltip only ⏸     |
| multinomial (AME) | AME (model %) per cat         | observed category % + crude diff, PER category            | tooltip only ⏸     |

Placement rule (settled with the maintainer): **explicit columns when few** (binomial, gaussian, poisson), apart from crude risk-diff with binomial OR (tooltips only) ; **tooltip-only when many** (multinomial — a column per category x empirical would explode the layout).

##### What landed in Phase 14t

- **Binomial**, both `effect = "coefficient"` AND `effect = "ame"`: the crude `Emp. %` (coloured by the
  crude risk-difference) + `Emp. OR` columns. Widening it to AME answers the review's "base % + empirical
  diff" and un-blocks the `effect = "ame" + empirical` error (now these columns, no error). Must be corrected, cf. CLAUDE.md.
- Other families / multinomial: `empirical = TRUE` temporarily emits a MESSAGE ("available only for a single
  binary logistic outcome; ignored") and proceeds, instead of aborting : to modify after implementation.


##### Binomial AME

There’s a problem with Binomial AME + empirical : currently it’s not user-friendly/not the right quantities, because **the empirical part does not compare well at all to the modelised part**. When it’s bimonial with `effect = "ame"`, the `empirical = TRUE` columns should be : "Emp. %" is ok, then "Emp. diff", not "Emp. OR", since the modelised OR is not displayed here, and the ame modelise the difference relative to the reference level. The column headers should also tell more explicitly what is in the parenthesis of AME ("model %").
- Current (`tab_reg(data, dependent = "married", predictors = c("rincome", "tvhours", "relig"), family = "binomial", effect = "ame", empirical = TRUE)`)  
    "| levels         | Emp. % | Emp. OR | Married: AME     |
     | Reference pop. |        |         |                  |
     | Lt $10000      | 37%    | 1       |          (35.4%) |
     | 10000 to 14999 | 41%    | 1.21    | +1.8%    (37.8%) |
     | 15000 to 24999 | 42%    | 1.27    | +5.1%*   (41.1%) |
     | 15000 to 24999 | 44%    | 1.37    | +6.3%***(42.9%) |
     | 25000 or more  | 55%    | 2.13    |+16.8%*** (54.3%) |
- It should be :
    "| levels         | Emp. % | Emp. diff | Model AME (model %) |"


Make empirical work with several dependent variables
`data |> tab_reg(dependent = c("married", "black"), predictors = c("rincome", "tvhours", "relig"), family = "binomial", effect = "ame", empirical = TRUE)`
- Message : "ℹ `empirical` (crude descriptive companion) is currently available only for a single binary logistic
 outcome; ignored here." Should be made to work with several dependent variables and only one set of predictors.


##### Gaussian / poisson : maintainer’s decisions

**Gaussian / poisson colour is genuinely under-specified.** An `Emp. mean` column of `type = "mean"`
  coloured by `color = "diff"` uses the sd-STANDARDIZED difference (Glass's Delta = diff / sd_ref, §18),
  but the crude path has no reference variance (`var` field), so the colour scale is undefined.
  
Question : but in tab() with numeric vars, the reference do have a standard deviation, right ?

Poisson : the related crude quantity is the mean, colour by the ratio.
- `tab(mutate(forcats::gss_cat, race = forcats::fct_rev(race)), "race", tvhours, ref = 1, color = "ratio") |> mutate(ratio = tvhours |> set_display("ratio")) |> tab_md()`, same `than color = TRUE`.
| race      |         mean (sd) |       ratio |
|:----------|------------------:|------------:|
|           |         *tvhours* |             |
|           |                   |             |
| **White** |    **2.8** (σ2.3) |       **1** |
| Black     | [4.2 (σ3.5)]{.p2} | [1.51]{.p2} |
| Other     |        2.8 (σ2.4) |        1.00 |
| Total     |        3.0 (σ2.6) |        1.08 |
- `tab_reg(mutate(forcats::gss_cat, race = forcats::fct_rev(race)), "tvhours", "race", family = "poisson") |> tab_md()`
|          | levels                   |   tvhours: IRR |
|:---------|:-------------------------|---------------:|
| Constant | **Reference population** |    **2.77***** |
|          |                          |                |
| race     | **White**                |          **1** |
|          | Black                    | [1.51***]{.p2} |
|          | Other                    |         1/1.00 |


Linear regression : the matching quantity is the difference from reference ; color by standardised differences.
- `tab(mutate(forcats::gss_cat, race = forcats::fct_rev(race)), "race", tvhours, ref = 1, color = "diff") |> mutate(diff = tvhours |> set_display("diff") |> set_digits(2)) |> tab_md()`
| race      |         mean (sd) |         diff |
|:----------|------------------:|-------------:|
|           |         *tvhours* |              |
|           |                   |              |
| **White** |    **2.8** (σ2.3) | **ref:2.77** |
| Black     | [4.2 (σ3.5)]{.p3} | [+1.41]{.p3} |
| Other     |        2.8 (σ2.4) |        -0.01 |
| Total     |        3.0 (σ2.6) |        +0.21 |
- `tab_reg(mutate(forcats::gss_cat, race = forcats::fct_rev(race)), "tvhours", "race", family = "gaussian") |> tab_md()`

|          | levels               |     tvhours: β |
|:---------|:---------------------|---------------:|
| Constant | Reference population |        2.77*** |
|          |                      |                |
| race     | White                |              0 |
|          | Black                | [1.41***]{.p3} |
|          | Other                |          -0.01 |
|          |                      |                |


By the way, there’s a bug to correct with `display = "ratio"` : it prints `n` instead of ratio (but ratio field is right in vctrs::vec_data) !
`tab(mutate(forcats::gss_cat, race = forcats::fct_rev(race)), "race", tvhours, ref = 1, color = "ratio") |> mutate(ratio = tvhours |> set_display("ratio")) |> tab_md()`

| race      |         mean (sd) |         ratio |
|:----------|------------------:|--------------:|
|           |         *tvhours* |               |
|           |                   |               |
| **White** |    **2.8** (σ2.3) |    **8610.0** |
| Black     | [4.2 (σ3.5)]{.p2} | [1700.0]{.p2} |
| Other     |        2.8 (σ2.4) |        1027.0 |
| Total     |        3.0 (σ2.6) |       11337.0 |


##### Multinomial : maintainer’s decisions and questions
- **The multinomial x AME tooltip is a REAL field conflict, not just fiddly.** The tooltip builder
  (`tab_kable_print_tooltip`) reads the `ratio` / `ctr` / `mean` fields for a `type = "row"`/`"mean"`
  column (`out_rr` / `out_ctr` / `out_mean`), so stashing the crude % / diff in any of them makes a
  SPURIOUS "ratio:" / "contrib:" line appear — exactly the "would mess with tab() tooltips or other
  tab_reg() tooltips" the maintainer worried about. A clean version needs EITHER a genuinely free field
  (none is safe for a row-type column) OR a new tooltip fragment gated on a reg marker (touches the
  shared builder).
- At the same time, this comparison of modelised quantities versus observed quantities is one of the best way to teach statistics at university : so we should find a way to do it.
- What would be the best way to store a tooltip fragments, already formatted, without having to create a new vctrs field ? Use a named vector as column-level attribute, with names as levels, then to retrieve at export join it with levels column by names ? Can you think about a more reliable way to do it without creating a new vctrs field ?


#### Phase 14v-ii — CI methods, over-dispersion, and empirical CIs


**Read `dev/tabxplor_1.4.0_decisions.md` §48 first** — it holds the full design, the maintainer's
settled choices, and the measured numbers that justify every default. This is the implementation brief.
A follow-up to 14v (§47), fixing/completing the crude-vs-model CI relation. All defaults are the
robust/heteroscedastic row (assumption-light, matching tab()'s existing Welch diff spirit); opt-ins
reproduce a regression's interval. Golden churn is EXPECTED here (reg poisson/grouped-binomial SEs
widen; the numeric ratio CI becomes a real ratio CI; empirical columns gain colour/CI/stars) — regen
consciously and diff each.

##### Part 1 — `ci = "ratio"` works everywhere + the new `method_*` args

- **The bug** (verified §48): `ci = "ratio"` on a NUMERIC mean silently stores `ci_type = "diff"` and the
  diff bounds (the diff interval mislabelled as a ratio). Make it compute a real ratio-of-means CI and
  set `ci_type = "ratio"`.
- **New args** (consistent with `method_cell` / `method_diff`; named for means/numeric), `match.arg`,
  first value default: `method_ratio = "katz"` (proportion ratio; one value for now, added so the expert
  sees every case); `method_mean_diff = c("welch", "student")` (numeric diff; `"student"` = pooled t =
  linear reg); `method_mean_ratio = c("robust", "quasipoisson", "poisson")` (numeric ratio; `"robust"` =
  delta-log per-group = modified Poisson, default; `"quasipoisson"` = Poisson SE × √φ = quasi-Poisson
  reg; `"poisson"` = naive Var=μ, reproduces a reg with no over-dispersion). roxygen: per arg, state the
  quantity + which regression it reproduces (the §48 tables).
- **Closed-form engines** in `R/tab-agg.R` beside `ci_pivot`/`ci_wilson`/`ci_newcombe`/`ci_katz` (formulas
  in §48). Quantile: z with stars off, matching t with stars on (§15 duality); ratio CIs are z on the
  log scale. Wire through the 14b `ci_scale` seam (`color = "ratio"` → `ci_scale = "ratio"` → `tab_ci()`);
  the numeric arm must dispatch on `method_mean_ratio`, not fall to the diff bounds.
- Record all five `method_*` in the `ci_settings` attribute so `tab_color_legend()` names the actual
  method (Welch/Student/robust-Poisson/quasi-Poisson/Poisson/Katz/Wilson/Newcombe). No new fmt field:
  the method is a `ci_settings` scalar; `ci_type` stays `"ratio"` regardless of method.

##### Part 2 — over-dispersion: MLE fit + dispersion-scaled SEs (maintainer's choice A)

- `family = "poisson"` and grouped/summed-score binomial (`trials`): keep the **MLE fit** (so
  AIC/McFadden/LR/BIC stay in the footer) but **scale the coefficient SEs by √φ** (φ = `reg_dispersion`
  Pearson dispersion) for the CIs/stars **by default**. Verified EXACT: Poisson SE × √φ = quasi-Poisson
  SE; auto-degrades to naive when φ ≈ 1. `reg_fit`/`reg_wald_from_tidy` (`R/tab_reg.R`): apply √φ to the
  SEs before CI/p; p quantile stays `t(df.residual)` (the quasi/lm branch).
- **Bernoulli binary**: unchanged (dispersion not identifiable). **gaussian/lm**: unchanged (no
  over-dispersion concept; heteroscedasticity is the analogue, handled tab-side). The explicit
  `family = "quasipoisson"` path stays (true quasi, NA GOF accepted).
- Footer: φ already shows (`reg_dispersion`); it now DRIVES the SEs — word it as the active adjustment.

##### Part 3 — empirical (crude) columns get CIs (same method as the model)

Each crude column (`Emp. %`/`OR`/`diff`/`mean`/`rate`/`IRR` + the multinomial tooltip) gains a **crude
CI computed with the SAME method as the model** (= the single-predictor model's interval, §47 parity),
used for: **colour** (significance-based — move off `color_signif = "ignore"` to a CI-driven policy),
**its own tooltip** (the CI text), and **significance stars** (store a `pvalue`). **NOT shown in-cell by
default** (a custom `display = "{or} {ci}"` adds the bracket, like model columns). Per-family crude CI in
§48 (Wilson %, Woolf log-OR, Newcombe risk-diff, Welch/Student mean-diff, robust/quasi rate-ratio). Add
a doc note that a crude star = the *unadjusted* association is significant (the maintainer accepts the
possible confusion). The empirical variance auto-absorbs a summed-score binomial's over-dispersion — no
special handling, and the user need not declare a summed score (§48 confirms it changes nothing vs a
plain numeric variable).

##### Verify

- **Parity (extend `test-tab_reg-empirical.R`)**: single-predictor model CI == empirical-column CI ==
  `tab()` CI under the matching `method_*` — gaussian (`student` == OLS), poisson (`quasipoisson` ==
  quasi-Poisson reg; `poisson` == naive), binomial OR (Woolf), AME (Newcombe). To 1e-6.
- `ci = "ratio"` on a numeric mean now stores `ci_type = "ratio"` and a ratio-scale bracket (regression
  test the exact bug); the three `method_mean_ratio` values give the three §48 intervals.
- Empirical columns render colour + stars + a CI tooltip; a crosstab is unaffected (no leakage).
- Full suite (sanctioned recipe); `document()` clean; conscious golden/snapshot regen (list what moved
  and why). Samples to `dev/review_manual/`.

##### Gotchas

- `ci_settings` must carry the new methods AND survive the reg footer/pvalue rebuilds (`reg_footer_lines`
  / `tab_pvalue_lines` — the recurring 14n/14v landmine: thread every table attribute through `new_tab()`).
- Empirical columns moving off `color_signif = "ignore"` changes their colouring (significant-only) —
  intended, but audit the 14v colour goldens/samples.
- Deferred (out of scope, note only): robust HC SEs on `lm`/`svyglm` (reverse-direction match); Fieller
  as a 4th `method_mean_ratio`.

###### DONE (2026-07-18)

Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3462**; `document()` clean. Conscious golden regen: **7
`_golden/*.rds`** only — 4 `ci_settings`-only (the attribute grew 3→6 fields), 3 mean-CI data
(`n_mean_ci`, `n_ci_tabvars`, `n_ci_tabvars_all`, rule B z→t). **No `_color_golden` and no `_snaps`
moved** (rule B did not flip any golden's significance; the numeric mean-CI *display* is not snapshotted).
Samples: `dev/review_manual/phase14v_ii_*.{html,md,xlsx}`.

**The maintainer chose rule B on principle** (method determines df, not the stars toggle): **t** where a
variance/dispersion is estimated (mean cell → t(n−1), welch-diff → Welch-t, student-diff → t(n1+n2−2),
quasipoisson-ratio → t(n1+n2−2)), **z** where the variance is a fixed function of the mean (robust
ratio, naive poisson, all proportion CIs). This **reverses the §15/§19 stars-gating** and reaches the
mean **cell** CI too (the largest churn; flagged and accepted). `ci_mean_diff2` now always uses the
method's df; `ci_pivot` guards `df ≤ 0` (n=1 → clean NA, no NaN warning — also fixes the pre-existing
`n_ci_tabvars` NaN drift).

- **Part 1**: `ci_mean_ratio` / `ci_or` engines (`R/tab-agg.R`); `method_ratio`/`method_mean_diff`/
  `method_mean_ratio` public args on `tab`/`tab_many`/`tab_ci` (+ `ci_settings` grows to 6 fields, named
  in `legend_method_name` + FR). The bug is fixed: a ratio-coloured **mean** stores `ci_type = "ratio"`
  and real ratio-of-means bounds (was the diff bounds mislabelled). Trigger =
  `color_pct_text_is_ratio()` generalised to means (`by_type` `mean = "ratio"`; flat `color = "ratio"`
  already fired), threaded into `tab_num` (path A, the pipeline mean CI) + `tab_ci` (path B).
- **Part 2**: `reg_fit` scales SEs by √φ for unweighted poisson / grouped-binomial (MLE fit kept → GOF
  footer intact), t(df.residual), p recomputed. `reg_dispersion` made pure; the over-dispersion warning
  moved into `reg_fit` (single fire, reworded to the active adjustment, still contains "dispersion").
- **Part 3**: empirical columns gain a crude CI + pvalue + significance colour (caller's `color_signif`),
  method-matched (Newcombe / Woolf `ci_or` / Student=OLS / quasi-Poisson / Wilson); `Emp. mean` stays
  uncoloured (cell CI for stars/tooltip). Multinomial tooltip carries Wilson + Newcombe CIs.
  `reg_footer_lines` now threads `ci_settings`. **Pre-existing bug fixed**: `reg_empirical` saw the RAW
  0/1-numeric outcome but `positive_level` is the labelled `"<dep>"` → crude base silently 0; now mirrors
  `reg_prep_binary`'s recode.

**Reg-legend prose deferred to 14w** (Q3): the empirical mean columns already name their method (Emp.
rate → "quasi-Poisson interval"), but the `Emp. IRR` column's `ci_type = "or"` still reads "log
odds-ratio" (should be rate-ratio) — a 14w refinement. **Concurrent maintainer change**: a new
`gss_cat_data_formatting()` in `R/utils.R` (theirs, untouched); this session's `document()` generated
its `.Rd` + NAMESPACE export.



#### Phase 14w — reg tables titles, legends, and headers

1. **[14u] The reg-table SHEET/TITLE mis-titling is still open** (the 14l flag). I want the above table title to be more informative, specific to regressions, of the type : "logistic regression : <dependent> by <explanatory_1, explanatory_2>, + <x> more", "linear regression : ...", "poisson regression : ...", "multinomial logistic regression : ...", "ordinal logistic regression : ..." ("tabbed by" with a split var). Sheet title : "linear_<dependent>_<explanatory_1>_etc", "logit...", "poisson...", "mlogit", "ologit...".

2. The legend of regression models is not clear, and sometimes not specific enough compared to crosstables.
- First, the model legend line must always come before the color legends line (otherwise the reading may not know what he’s reading.
- Also, model legend line should state something like : "Model: logistic regression. Marginal effects on..." (or "Model: linear regression. ..." etc.). For example, multinomal OR currently have "Multinomial odds ratios (each category vs the reference).", that could be improved to : "Multinomial logistic regression: odds ratios (each category vs the reference)."
- The colors legends should also be specific to models, when needed, not to be misleading ? Legend for Binomial Or is good ("Wald interval on the log odds-ratio"). But for Binomial AME color legend is the same than for a crosstable : it states "Newcombe" etc., which is false since colors used the model and AME own confidence interval, and pvalue for significance stars. It’s worth checking for other models too.
- Current example with Binomial AME legend :
    "Shades of blue: cells ≥ the Total row +5; +10; +20; +30 points. Shades of yellow to red: cells ≤ the Total row -5; -10; -20; -30 points. Coloured: significantly different from the Total row (Newcombe score interval, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
    Marginal effects on the probability scale (percentage points) (sample-averaged). Each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability."
- Current example with Binomial + empirical legend :
"Emp. % — Shades of blue: cells ≥ the Total row +5; +10; +20; +30 points. Shades of yellow to red: cells ≤ the Total row -5; -10; -20; -30 points.
01-Married: OR — Shades of blue: OR ≥ 1.15; 1.5; 2; 4. Shades of yellow to red: OR ≤ 1/1.5; 1/2; 1/4. Coloured: significantly different from the reference category (Wald interval on the log odds-ratio, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
Emp. OR — Shades of blue: OR ≥ 1.15; 1.5; 2; 4. Shades of yellow to red: OR ≤ 1/1.5; 1/2; 1/4"

3. Regression models col_var name and columns names could be more clear and less redundant. The col_var name should always be of style "<dependent>: <level>", so that modelised quantities and empirical quantities both appear under the same col_var header (no vertical borders between them in html or Excel, reads as same group). The exception would be multinomial, where it is more clear if the col_var field and col_var title just stay "<dependent>: <effect>" (no vertical borders between the AME of OR of the different level, reads as same model) ; but then, we can remove the repeated "OR" or "AME" on each header / level name.

Exemple with Binomial OR + empirical
- Current :
    "|       | Emp. % | Emp. OR | 01-Married: OR |
     |levels | Emp. % | Emp. OR | 01-Married: OR |"
- Should be something like (in html, Excel, and where col_var names are merged) :
    "|       | married: 01-Married         |
     |levels | Emp. % | Emp. OR | Model OR |"

Exemple with Binomial AME + empirical (see statistical corrections made in Phase 14-v above)
- Current (`tab_reg(data, dependent = "married", predictors = c("rincome", "tvhours", "relig"), family = "binomial", effect = "ame", empirical = TRUE)`)  
    "|                | Emp. % | Emp. OR | Married: AME     |
     | levels         | Emp. % | Emp. OR | Married: AME     |"
- It should be something like :
    "|                | married: 01-Married                      |
     | levels         | Emp. % | Emp. diff | Model AME (model %) |"

Exemple with Multinomial + OR
- Current :
    "|       | party3: OR                   |
     |levels | Ind vs Rep: OR | Dem vs Rep: OR |"
- Should be something like
    "|       | party3: OR            |
     |levels | Ind vs Rep | Dem vs Rep |"

Exemple with Multinomial + AME
- Current :
    "|        | party3: AME                    |
     | levels | Ind: AME | Dem: AME | Rep: AME |
- Should be something like
    "|        | party3: AME (model %) |
     | levels | Ind | Dem| Rep |

4. Lists of predictors use the column header / level name to give the name of the model, and remove duplicated col_var name row to keep vertical borders between different models. This is good, but side-effect is that reference level of dependent variable and which effect is displayed and are written nowhere. In this case, we shall use the title above the table to give the necessary informations about the model.
- Current :
    "married                        # title above table
     | levels |  demo  |  full  |"
- Should be something more like :
    "Logistic regressions (models comparison) : married, "01-Married" (OR)    # title above table
     | levels |  demo  |  full  |"

5. Emp. IRR's ci_type = "or" still reads "log odds-ratio" where it's, actually a rate-ratio.

Tell me where they would need a new column-level or table-level attribute to store model metadata.

##### Done (2026-07-18)

All five items landed on ONE new table-level attribute **`reg_meta`** (the answer to the metadata
question: family/effect/dependent/reference/predictors/split_var/comparison; NO column-level attribute
and NO new fmt field — the per-column effect word is derived from `reg_meta$family`/`effect` + the
column's `ci_type`/`type`, model-vs-empirical told apart by the `"Emp. "` name prefix). Carried like
`vars` (a `new_tab()` formal + `get/set_reg_meta` + one `tab_attrs()` line + threaded through
`reg_footer_lines`/`tab_pvalue_lines`; `is_reg` now reads `!is.null(get_reg_meta(x))`, surviving the
footer materialisation that drops `test`). Full record + the flagged bare-header case: decisions **§49**.

- **Titles (1/4)**: `reg_title()`/`reg_sheet_name()` (family display/short names) — "Logistic
  regression: `<dep>` by `<preds>`" / comparison "…(models comparison): `<dep>`, '`<ref>`' (`<eff>`)";
  Excel "logit_`<dep>`_`<pred>`" sheets. Caption in md/kable + Excel; console via the footer model line.
- **Legend (2/5)**: `reg_model_line()` printed BEFORE the colour legend (console/md/kable/xl);
  `legend_ref_info(is_reg=)` → "the reference category" (fixes AME's "Total row"); family-aware
  effect-word → Poisson reads "rate-ratio" not "odds-ratio" (fixes Emp. IRR). `legend_specs()` now one
  spec per coloured column + a `role` in `sig` (model + empirical get separate lines under a shared span).
- **Headers (3)**: single-outcome model + empirical share one outcome col_var ("`<dep>`: `<level>`" /
  numeric = the dep name), model column named "Model `<eff>`"; multinomial keeps "`<dep>`: `<eff>`" span
  + strips ": OR"/": AME" from category names; comparison keeps per-model col_vars. GOF/`empirical_tips`
  keys follow the renames. Crosstab goldens/snapshots byte-identical.

Tests: new `test-tab_reg-14w.R`; existing reg tests updated to the "Model `<eff>`" / stripped names.
**Suite: FAIL 0 | WARN 0 | SKIP 4 | PASS 3493.** Samples: `dev/review_manual/phase14w_reg.{html,md,xlsx}`.



#### Phase 14x — small improvements and fixes

1. [14t] Hard deprecate the `empirical_OR` alias. Maintainer did it manually, just verify he did not break anything.

2. **[14u] for now `na = "drop_all"` and the K multi-dependent mode are on `tab_reg()` only, not the wrappers.** Please forward `na=` (and/or the K mode)  through `tab_logit`/`multi_logit` too.

3. **[14q] the terse console policy tag still reads `[significant only]`** : rework it too.

4. Bug: markdown misalignment problem resurfacing
`tab(mutate(forcats::gss_cat, race = forcats::fct_rev(race)), "race", tvhours, ref = 1, color = "ratio") |> tab_md()`
| race      |         mean (sd) |                     |
|:----------|------------------:|---------------------|
|           |         *tvhours* |                     |
|           |                   |                     |
| **White** |    **2.8** (σ2.3) | <!-- misaligned --> |
| Black     | [4.2 (σ3.5)]{.p2} | <!-- misaligned --> |
| Other     |        2.8 (σ2.4) | <!-- misaligned --> |
| Total     |        3.0 (σ2.6) | <!-- misaligned --> |
- It’s caused by the special char before the opening parenthesis in "2.8 (σ2.4)". It’s not specific to this case, but broader : verify this special space use, and fix it.

5. Small change to crosstables color legends.
- `color_sign = "grey_non_signif"`. "Uncoloured: either not significant, or too small a difference to colour." A clearer text would be : "Uncoloured: either not significant, or difference under ±5 points."

6. NA values management with `levels="first"`
- By default, when `na="keep"`, I want NA columns to be taken into account at calculation, but to be discarded like the second column (only keep the first column of the related col_vars, discard second and other levels, discard na level) ; obviously, NA rows in row_vars should stay.

##### Done (2026-07-18)

All six landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3517**; `document()` clean. Conscious
golden regen: **`_snaps/golden.md` only** (verified byte-for-byte to be the mean/sd joiner glyph swap
U+202F -> U+2007 and NOTHING else -- mapping both spaces to one marker makes old == new); no `.rds`
golden, no `render-html.md` (it has no mean cells and no grey_non_signif legend).

1. **`empirical_OR` hard-deprecated** -> `lifecycle::deprecate_stop` (defunct: errors, points to
   `empirical`). Never CRAN-released, so no retro-compat debt. The 2 alias tests now expect `defunctError`.
2. **`na=` forwarded** to `tab_logit`/`multi_logit`; **`multi_logit` now accepts a VECTOR `dependent`**
   (K mode: reuses `tab_reg`'s dependents-x-models recursion -> a `tabxplor_tabs` list, one sheet each).
   K mode belongs to `multi_logit` (it has the models LIST); `tab_logit`'s `predictors` is one model, so
   it only gains `na=`.
3+5. **grey_non_signif legend names the FIRST THRESHOLD** (shared `legend_threshold_phrase()`, R/fmt_class.R):
   `"±5 points"` / `"×1.15"` / `"±0.2 SD"` per measure. Terse tag -> `[grey: non-significant or under <thr>]`;
   prose -> "Uncoloured: either not significant, or a difference under <thr>." + FR. ⚠ **The prose format
   string is ONE literal, not `paste0("a ","b")`**: `xgettext` extracts each constant separately, so a
   paste0-split message never matches the paste0-JOINED string `gettextf` looks up at runtime -- the
   translation silently dies. (The old code had this latent bug; a prior phase had manually combined the
   po entry, which `update_pkg_po` would re-split.)
4. **mean/sd joiner U+202F -> `pad`** (fmt_class.R:2119): ASCII space in console/md-source, figure space
   (digit-width, the maintainer's pick) in html + `tab_md`'s rendered target. The plot backend's
   `unbrk`-strip sites are for ROW LABELS (the `unbreakable_spaces` option), untouched.
6. **`levels="first"` NA handling unified** (tab.R tab_prepare_pop): the pre-merge no longer folds NA into
   a level -- NA stays NA so the leaf's `na` handling is authoritative -- and `remove_levels` always
   appends `"NA"` (every arity). Fixes (a) a 2-level col_var showing its NA column under `na="keep"` and
   (b) a 3+-level col_var keeping dropped-NA rows in the base under `na="drop"`. Now matches the jmvtab
   defer path. Also reaches `sup_cols` (they use `levels="first"`).

---
