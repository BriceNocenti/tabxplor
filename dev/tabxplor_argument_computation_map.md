# tabxplor — argument ↔ computation map

> SINGLE REFERENCE for how each `tab()` / `jmvtab()` option maps onto internal computations, and
> how the options force each other. It governs Phase 7c (cache design), 7d (compute-function
> rework) and 7e (Jamovi module rewrite): the cache keys on the *resolved settings* this document
> describes, and the Jamovi `.js` mirrors the *argument-overwrite cascade* documented here.
>
> Companion docs (read the matching one before touching a subsystem):
> - `dev/tabxplor_jmvtab_cache_design.md` — the Phase 7c hierarchical multi-cache design, built on
>   §3/§7 below. It **supersedes §7** of this file (the "cache classification seed" is now the concrete
>   5-tier cache); read it for the jmvtab live-UI cache.
> - `dev/new_colors_UI.md` — the colour/breaks framework (measure × channel × significance). §6 of
>   THIS file audits that its §8/§12 computation matrices match the code.
> - `dev/tabxplor_1.4.0_decisions.md` — the settled 1.4.0 architecture decisions (§2 `tot_n`, §5
>   globalised row-axis, §7 col%+means, §12/§14/§20 inference).
> - `dev/tabxplor_architecture.md` — the current pipeline / type system / exporters.
>
> STATUS (2026-07-09, Phase 7b): the argument-overwrite cascade is now consolidated in ONE pure
> resolver, `tab_resolve_settings()` (`R/tab-resolve.R`); this document is its specification and
> the map that governs the rest of Phase 7.

---

## 1. Purpose and scope

Two questions a live UI (and a cache) must answer for every option:

1. **Interdependence** — when the user sets option X, which *other* options does `tab()` silently
   force? (e.g. `color = "contrib"` forces `chi2 = TRUE` and a total row.)
2. **Required computation** — which `tabxplor_fmt` fields / pipeline passes does option X trigger,
   invalidate, or only re-display?

The answers decide (a) what the Jamovi `.js` must do when a button changes, and (b) what the cache
may reuse vs must recompute. Everything below is grounded in the code; function names are the stable
anchors (line numbers drift).

The pipeline has three conceptual layers — this whole document is organised around them:

```
AGGREGATE          counts n / wn (factors)      |  moment-sums s1=Σwx, s2=Σwx² (numerics)
   |               keyed on tab_vars × row_var × col_var-cell, NA kept
PER-TRANSFORM      pct · diff · ratio · or · tot_n · CI (ci_inf/ci_sup) · pvalue · chi2/ANOVA test
   |               contrib (ctr/var) — one vectorised pass each over the aggregate
DISPLAY            format string · digits · colour paint (measure→channel→palette) · labels
```

Invalidation flows DOWNWARD only: a change at the aggregate layer invalidates everything; a
per-transform change invalidates that transform + its dependents + display; a display change
invalidates nothing but the render.

---

## 2. Argument catalogue

Each argument has an **axis** — the granularity at which it varies. This is what the cache keys on
and what the `.js` exposes:

- **G** = global row-axis: one value shared by all row_vars / mirror tables (Phase 6 §5 globalised
  these). `.js`: a single control.
- **RV** = per-row_var: a (named) vector, one entry per row_var.
- **CV** = per-col_var: a vector, one entry per col_var.
- **DISP** = display-only: never changes a field, only the render.
- **AGG** = changes the aggregate itself (counts / moment-sums), so everything recomputes.

### 2.1 `tab()` arguments

| arg | default | axis | role |
|-----|---------|------|------|
| `row_vars` / `col_vars` / `tab_vars` | — | — (structure) | tidy-select; define the table grid. Changing them = new aggregate. |
| `wt` | — | AGG | weighting variable; weighted `wn` / moment-sums. |
| `pct` | `"no"` | CV | percentage direction: `no`/`row`/`col`/`all`/`all_tabs`. |
| `color` | `"no"` | G | colour measure(s) + channel — see `new_colors_UI.md`; parsed by `normalize_color_spec()`. |
| `color_signif` | `"ignore"` | G | significance policy: `ignore`/`grey_non_signif`/`color_all_signif`. |
| `OR` | `"no"` | G | odds-ratio mode: `no`/`OR`/`OR_pct`. |
| `chi2` | `FALSE` | G | whole-table χ²/ANOVA test → the `test` attribute. |
| `na` | `"keep"` | AGG | missing policy: `keep`/`drop` (per col_var)/`drop_all`/`common_base` (old-tab). |
| `levels` | `"all"` | CV | keep `all` levels / only `first` / `auto` (2-level→first). Level-drop is post-chi2/ci. |
| `cleannames` | `NULL`→opt | AGG | clean factor labels. |
| `other_if_less_than` / `other_level` | `0` / `"Others"` | AGG | lump small factor levels. |
| `ref` | `"auto"` | RV | reference row (row%/means) — named vector, one per row_var. |
| `ref2` | `"first"` | G | reference column for OR (2×2 numerator). |
| `comp` | `"tab"` | G | compare within subtable (`tab`) or vs the total table (`all`). |
| `ci` | `"no"` | G | confidence intervals: `no`/`cell`/`diff`. |
| `conf_level` | `0.95` | G | THE significance threshold (CI level = star level, decisions §20). |
| `stars` | `NULL` | G | significance stars (opt-in); gates whether `pvalue` is stored. |
| `method_cell` / `method_diff` | `wilson` / `newcombe` | G | CI primitives. `method_cell` ∈ `wilson`/`wald` (7g); `method_diff` ∈ `newcombe`/`ac`/`wald`. |
| `totaltab` | `"line"` | RV | total-table mode: `line`/`table`/`no`. |
| `tot` → `totrow`/`totcol` | `c("row","col")` | RV/CV | total row / one total column (Phase 6 §6 soft-deprecated the fine control). |
| `add_n` / `add_pct` | `TRUE` / `FALSE` | DISP | append an unweighted-n / pct row-or-column (reuses existing fields). |
| `digits` | `0` | CV / DISP | rounding (stored per fmt; display-only). |
| `n_min` | `0` | DISP | Phase 7g small-base filter (drop weak rows/cols, blank weak cells via the `"blank"` display token). Pure display — recomputes nothing; applied last in `tab_assemble`. jmvtab: tier-4 `reapplied`. |
| `subtext` / `total_names` / `totaltab_name` / `other_level` | — | DISP | labels. |
| `output_list` | `FALSE` | — (shape) | one merged tab vs a list of tabs. |
| `spread_vars` / `names_prefix` / `names_sort` | — | DISP-ish | reshape to wide via `tab_spread()`. |
| `filter` | — | AGG | row filter before aggregation. |
| `row_var` / `col_var` / `sup_cols` | — | — | soft-deprecated singular / supplementary-column aliases. |

### 2.2 `jmvtab()` options → `tab()` (`jamovi/jmvtab.a.yaml`, mapped in `R/jmvtab.b.R`)

Baseline wired by Phase 7a. One-to-one with `tab()` except:

- `color` is a **single radio** `no/auto/diff/ratio/contrib/OR` → `.b.R` maps `no→FALSE`, `auto→TRUE`,
  else the measure string (text channel only). The two-channel `c(text, bg)` / named forms and
  `color_breaks` / palette are **not exposed yet** (Phase 7f candidates).
- `color_signif` → `tab(color_signif=)` verbatim; `.b.R` also forces `ci="diff"` when a policy is
  set with `ci=="auto"` (the concrete realisation of "significance-gated colour needs a diff CI").
- `lvs` → `levels` (renamed to avoid `jmvcore::Options$levels()`).
- `display` / `ci_print` / `wrap_rows` / `wrap_cols` are **render-time** (applied post-build /
  `tab_kable()`), not `tab()` args.
- **`refLevels`** (Phase 7g Array picker) → `.b.R` folds it into a named `ref` vector via
  `jmvtab_ref_vector()`; the free-text `ref` box is the expert fallback.
- **`anova`** (Phase 7g, welch/classic) → sets `options(tabxplor.anova=)` around the build (baked into
  the p-value line), and is in `.opts()` so it sits in the tier-3 base-key (a toggle rebuilds).
- **`n_min`** (Phase 7g) → `tab(n_min=)`, applied tier-4 (see §2.1 / §7.1).
- **Export** (`export_format` + typed `path` + the `exportExcel` Action; Phase 7g) writes Excel/HTML/MD
  via `jmvtab_export()` (`R/jmvtab-export.R`) and reports a `jmvcore::Notice`.

The `.u.yaml` `enable:` conditions already encode interdependence (e.g. `diff`/`ratio`/`OR` colour
radios and `color_signif` policies are gated on `pct:row || pct:col`; `method_cell` on
`ci:cell||ci:auto`; `method_diff` on `ci:diff||ci:auto`). Phase 7g will make these consistent with
the resolver.

---

## 3. The three computation layers — which argument touches which

| layer | writes (fmt fields / attrs) | driven by |
|-------|-----------------------------|-----------|
| **Aggregate** | `n`, `wn` (factors); `mean`, `var` from moment-sums `s1`/`s2` (numerics) | `row/col/tab_vars`, `wt`, `na`, `filter`, `other_if_less_than`, `levels` (merge), `cleannames` |
| **Per-transform** | `pct`, `tot_n` | `pct` |
| | `diff`, `ratio` | `pct`(row/col) + `ref` |
| | `or` | `pct` + `ref` + `ref2` + (`OR` or `color=OR`) |
| | `ci_inf`, `ci_sup`, `pvalue` | `ci`, `conf_level`, `method_cell`, `method_diff`, `stars` |
| | `test` attribute (χ²/ANOVA) | `chi2` (or `color="contrib"`) |
| | `ctr`, `var` (contrib residuals) | `color="contrib"` (needs `chi2`) |
| **Display** | `display` string, `digits`; colour paint (`color` attr, `color_signif`) | `display`, `digits`, `color`, `color_signif`, palette, `ci_print` |

Aggregate note (numeric path): `tab_num()` now scans moment-sums (`num_derive_stats()`,
`num_rollup()` in `R/tab-agg.R`), so mean/var/CI/ANOVA are all recovered from `(n, s1, s2)` — no
re-scan. `tot_n` is each cell's OWN unweighted base (its row/col total); the weighted base is
recovered on demand by `get_tot_wn()` = `wn/pct`. This makes a built table self-sufficient for its
percentage base (no `detect_totcols()` needed by `tab_ci`).

---

## 4. Argument → computation dependency chain (X depends on Y)

Read "A ⇐ B" as "A requires B computed first". Function/field anchors in parentheses.

**Aggregate**
- `pct` ⇐ `n`/`wn` + a Total column (the denominator). `pct != "no"` forces a total column/row.
- `tot_n` ⇐ `n` + the `pct` direction (`tab_plain()` writes it in the `pct != "no"` block).

**Difference family**
- `diff` ⇐ `pct`(row/col) + a resolved `ref` (`calculate_refrows()`/`diff_index()`). `NA` when
  `ref = "no"` or `pct ∉ {row,col}`.
- `ratio` ⇐ same inputs as `diff` (the reference-relative ratio `p_cell/p_ref`).
- `or` ⇐ `pct`(row/col) + `ref` (rows) + `ref2` (reference column) + the `OR`/`color=OR` trigger.

**Confidence intervals**
- CI cell (proportion) ⇐ `pct` + `tot_n` (the base). CI cell (mean) ⇐ `mean` + `var` + `n`.
- CI diff (proportion) ⇐ `pct` + `tot_n` of the cell AND of the reference cell (via `ref`).
- CI diff (mean) ⇐ `mean` + `var` + `n` of cell AND reference (Welch-t).
- `pvalue` ⇐ CI computed with `want_p`, i.e. `stars == TRUE` AND `ci == "diff"`. Cell CIs carry no
  `pvalue`. (Since Phase 3a the stored per-cell `pvalue` is the inversion-p of the *displayed*
  interval, so stars can never disagree with the bracket — decisions §20.)
- `ci != "no"` ⇒ runs `tab_ci()` (factors) / the numeric CI block.

**Colour-driven forcings** (the cascade — now in `tab_resolve_settings()`)
- `color ∈ {diff_ci, after_ci}` ⇒ `ci = "diff"`. (Legacy strings; the new `color_signif` produces
  them — see §5.)
- `color_signif != "ignore"` with a `diff`/`ratio` measure ⇒ `ci` (→ significance boolean/bounds).
- `color = "contrib"` ⇒ `chi2 = TRUE` AND a total row (to store each cell's variance contribution).

**Whole-table tests**
- χ² `test` ⇐ `n` (unweighted counts; `agg_chi2()`). ANOVA `test` ⇐ per-group `mean`+`var`+`n`
  (`agg_anova()`, Welch + classic F).
- contrib colouring ⇐ χ² residuals (`ctr`/`var` written by `tab_chi2()` only when
  `color == "contrib"`).
- `comp = "all"` ⇐ a total table exists (forces `totaltab`).

---

## 5. The argument-overwrite cascade — review, and the resolver

### 5.1 What the cascade is, and where it lived (before Phase 7b)

The cascade is the set of rules where one argument silently forces another. It was scattered across
**four sites** with real duplication:

| rule | old sites |
|------|-----------|
| `color = "auto"` → concrete measure (factor arm) | `tab_build()`, `tab_counts()` |
| `color = "auto"` → concrete measure (numeric arm) | `tab_num()` (different logic) |
| `color = "contrib"` → `totrow = TRUE` | `tab_build()` only (NOT `tab_counts()` — see §5.4) |
| `color = "contrib"` → `chi2 = TRUE` | `tab_build()`, `tab_counts()` |
| diff-family colour requires `ref` (abort) | `tab_build()`, `tab_num()`, `tab_counts()` |
| `color ∈ {diff_ci, after_ci}` → `ci = "diff"` | `tab_build()`, `tab_num()`, `tab_counts()` — **4×** |
| split `color` → `color_diff_OR`/`color_ctr`/`color_ci`/`color_num` | `tab_build()`, `tab_counts()` |
| `ref = "auto"` → `first`/`tot` | `tab_plain()` (`first` under OR), `tab_num()` (always `tot`) |
| `pct = "all_tabs"` + no tab_vars → `"all"`; `comp = "all"` + no tab_vars → `"tab"` | leaf |
| `pct`-driven / `color`-driven `tot`/`totaltab` forcing (+ warnings) | leaf |

**Judgment: every rule is statistically sound — keep them; the problem was duplication, not
substance.** Discard none.

### 5.2 The consolidation (Phase 7b, byte-identical)

The genuinely-global, genuinely-duplicated **colour cascade** now lives in ONE pure function,
`tab_resolve_settings()` (`R/tab-resolve.R`), called by `tab_build()` and `tab_counts()`. It performs
(verbatim port): `color = "auto"` (factor arm), `contrib → totrow/chi2`, diff-family-requires-`ref`,
diff-family → `ci = "diff"`, and the four-way colour split. The numeric `color = "auto"` arm is a
sibling helper `resolve_color_auto_num()` in the same file, invoked by `tab_num()`.

The whole suite (golden / counts-parity / fuse-parity / fmt-contract / color-golden) stays green with
**no regeneration** — the extraction changes *where* values are decided, never *what* is computed.

### 5.3 The static-vs-data line (what stays at the leaf, and why)

`tab_resolve_settings()` is a **pure function of (argument values, column CLASS metadata)** — it never
reads a column's *values*. That is exactly the boundary the Jamovi `.js` can mirror and the cache can
key on. Three resolutions are **data-dependent** and stay at the leaf builders:

- `levels = "auto"` — needs the real `nlevels(fct_drop())`; entangled with the level-merge in
  `tab_build()` after `tab_prepare()`.
- `ref` literal/regex — matched against built row labels (`calculate_refrows()`/`diff_index()`); the
  resolver only inspects `ref`'s symbolic emptiness (`"no"`/`""`/`NA`), never a literal value.
- `na` dropping — a data-filtering operation.

Plus the leaf `tot`/`totaltab` forcing + warnings stay at `tab_plain()`/`tab_num()`: they matter only
for **direct** leaf callers (both are exported), and are no-ops under `tab_build()` (which pins `tot`
and pre-forces `totrow`).

### 5.4 Inconsistencies found (and their disposition)

- **`ref = "auto"` differs by column type — INTENTIONAL, stays at the leaf.** For a factor
  (`tab_plain`): `first` if OR/OR-colour else `tot`. For a mean (`tab_num`): always `tot`. On a
  *mixed* table the same `ref="auto"` must resolve differently for the factor leaf and the numeric
  leaf, so it cannot be pre-resolved to a single value in the resolver. The difference is currently
  non-observable — `tab_num()` has no `OR` argument, so the factor rule's `first` branch could never
  fire for means anyway. Documented at both leaves with `# LEAF resolution (Phase 7b)`. A future
  "OR for numerics" is the only thing that would make them diverge; it would then move the numeric
  goldens (tripwire ledger).
- **`color = "auto"` factor vs numeric arms — legitimately distinct, both now in `tab-resolve.R`.**
  Numerics have no `contrib`/`OR`; they key on `ref` + `ci`. Pure dedup, no behaviour change.
- **`ci = "diff"` forcing — was 4×, now 1×** in the resolver (+ the leaf `tab_num()` guard for direct
  callers).
- **`tab_counts()` does NOT force `contrib → totrow`** (unlike `tab_build()`). Preserved as-is
  (`totrow = NULL` opts out of that half of the resolver) to keep byte-identity; it is a latent
  difference for contrib-coloured `tab_counts()` tables, flagged in §9, not fixed in 7b.

---

## 6. Colour / `color_signif` required-computation matrix (audited vs code)

Reproduces `new_colors_UI.md` §8.2 / §12 with an **audit column** confirming the code does it. The
significance primitive is universal (decisions §20): `sig_pos = get_ci_inf(x) > 0`,
`sig_neg = get_ci_sup(x) < 0` from the stored asymmetric bounds; the stored `pvalue` is the inversion
of the *displayed* interval so stars ⇔ bracket always agree.

### 6.1 Percentages (factor col_var) — what `tab()` must compute

| measure | `ignore` | `grey_non_signif` | `color_all_signif` | audit |
|---------|----------|-------------------|--------------------|-------|
| `diff` | `ref` + `diff` | + prop-diff CI → `sig` | + prop-diff CI bounds | ✓ `tab_plain`+`tab_ci` (Newcombe) |
| `ratio` | `ref` + `ratio` | shares the diff `sig` | + RR CI bounds | ✓ `ratio` field = `p_cell/p_ref` |
| `contrib` | χ² decomp (`ctr`, margins) | + std. residuals → `sig` | + residuals graded | ✓ `tab_chi2` (`ctr`/`var`, only when contrib) |
| `or` | `ref2` + `or` (2×2) | + log-OR CI → `sig` | + log-OR CI bounds | ✓ `or` field; CI at tab_logit phase |

### 6.2 Means (numeric col_var) — no `contrib`/`or`, CI = Welch-t

| measure | `ignore` | `grey_non_signif` | `color_all_signif` | audit |
|---------|----------|-------------------|--------------------|-------|
| `diff` (standardized) | `ref`+`diff`+`sd_ref`(`var`) | + mean-diff CI → `sig` | + mean-diff CI bounds (÷`sd_ref`) | ✓ Glass Δ = `diff/sd_ref`, `get_ref_var()` |
| `diff` (absolute breaks) | `ref` + `diff` | + mean-diff CI → `sig` | + mean-diff CI bounds | ✓ raw `diff` when unit breaks |
| `ratio` | `ref` + `ratio` | shares the mean-diff `sig` | + ratio CI bounds | ✓ `ratio` field = `m_cell/m_ref` |

Cross-cutting: `ignore` computes NO CI (cheapest); `grey_non_signif` adds the significance boolean;
`color_all_signif` adds the CI bounds (which subsume the boolean); `diff`+`ratio` together share one
cell-vs-ref CI; `contrib` needs the χ² decomposition and no reference; standardized `diff` also needs
`sd_ref`.

### 6.3 Audit findings — code is AHEAD of `new_colors_UI.md` §12 (doc was stale)

The doc §12 (written pre-Phase-5) listed two items as TODO that are in fact **done**:

- **`get_ref_var()` exists** — `R/fmt_class.R` (mirror of `get_ref_means`), used for Glass's Δ
  (`sd_ref = sqrt(get_ref_var(x))`). Doc's "New helper needed" was stale.
- **pct `ratio` field already repointed** — `tab_plain()` writes `ratio = pct/ref` (the
  reference-relative RR, the ×2 driver) and sets `mean = NA` for pct columns (Phase 5 Batch A/B).
  Doc's "today it holds leftover column-referenced `tabs_rr` … ×2 in the overloaded `mean`" was stale.

Both stale lines are corrected in `new_colors_UI.md` §12 as part of Phase 7b. Everything else in
§8.2/§12 is implemented as specified.

---

## 7. Pure-display vs recompute — the cache classification (seeds Phase 7c)

The most forward-looking section: what a live UI may reuse vs must recompute when one option changes.
Three tiers, matching §1's downward invalidation.

### 7.1 DISPLAY-only — reuse ALL fields, re-render only

No field recomputes; the same `tabxplor_fmt` object renders differently.

- `display` (the format string), `digits` (rounding), `ci_print` (`ci` vs `moe` arm).
- **colour paint**: `color` measure/channel choice, `color_signif` policy, palette — all written as
  fmt *attributes* over an already-built table (`finalize_color_spec()`/`set_color()`/
  `set_color_signif()`), PROVIDED the measure's field already exists. The findInterval engine reads
  fields; it never recomputes them. → This is why "the standard usage is colour-driven": switching
  measure/policy is free *if the needed fields were computed*.
- `add_n`/`add_pct` (reuse existing `n`/`wn`/`pct`), `subtext`/label args, total row/col *removal*
  (rows/cols are computed as bases then dropped for display).

CAVEAT — the colour *fields* must exist. Switching `color_signif` from `ignore` to
`grey_non_signif` needs the CI (a per-transform recompute, tier 7.2), even though the *paint* itself
is display. The cache must therefore store "which fields were computed", not just "which colour was
shown".

### 7.2 PER-TRANSFORM recompute — reuse the aggregate, redo one transform + dependents

The aggregate (counts / moment-sums) is untouched; a transform and its dependents recompute.

- `pct` (row/col/all/no) → recomputes `pct`, `tot_n`, and invalidates `diff`/`ratio`/`or`/CI.
- `ref` / `ref2` → recomputes `diff`, `ratio`, `or`, and every diff-CI reference. **This is the
  fast path Phase 7c must make instant** (change reference level live): from a cached aggregate +
  cached counts, only `diff` (+ CI when `color_signif != "ignore"`) recomputes.
- `OR` → recomputes `or`/`ratio` + display.
- `ci` / `conf_level` / `method_cell` / `method_diff` / `stars` → recompute `ci_inf`/`ci_sup`/
  `pvalue`.
- `chi2` / contrib colour → recompute the `test` attribute and the `ctr`/`var` residual fields.
- `comp` (tab vs all) → changes the reference set/denominators → recomputes diff/ratio/CI/χ².

### 7.3 AGGREGATE recompute — the bottleneck; everything downstream redoes

Counts / weighted counts / moment-sums are the expensive scan. Changing any of these forces a full
recompute:

- `na`, `wt`, `filter`, `other_if_less_than`, `levels` (level merge), `cleannames`, and of course the
  variable set (`row/col/tab_vars`).

CACHE LEVER (Phase 7c): keep NA in the aggregate always, so an `na` change recomputes only `pct`
downward, not the counts. Adding/removing a variable reuses the other variables' cached counts. See
`dev/new_colors_UI.md` §W-flags and CLAUDE.md Phase 7c for the tree design; the `.fine` shared
finest-grain aggregate (`tab_build()` fused path, off by default) is the reusable infra for it.

---

## 8. Column-type asymmetries (factor path vs numeric path)

- **Aggregate**: factors → counts (`n`/`wn`, `tab_plain()`); numerics → moment-sums (`num_derive_stats`).
- **Measures**: `contrib` and `or` are factor-only (a mean has no χ² decomposition / odds ratio).
  `color = "auto"` therefore resolves per type (§5.4).
- **`diff` colour**: factor `diff` = percentage-point difference; numeric `diff` = Glass's Δ
  (`diff/sd_ref`) by default, raw when unit breaks given.
- **col% + means reference — INTENDED asymmetry (not a bug).** Under `pct = "col"`, a factor column
  compares against a reference COLUMN, but a mean compares against a reference ROW. This is correct:
  numeric (mean) variables only ever appear as columns, and a mean's reference is meaningfully one of
  its *rows* (compare the mean across groups) — a mean has no column-percentage to reference a column
  by. On a mixed `pct = "col"` table the two column types therefore use different reference axes.
  There is no clean fix without white-elephant arguments/UI (the only difficulty is a UI setting both
  consistently), so the behaviour is kept; `tab()` warns and this document is where the rationale
  lives. (Related but separate: `pct = "col"` with several row_vars is handled by manual invert +
  opt-in transpose at export — Phase 8, decisions §7.)

---

## 9. Open items and caveats (for 7c–7f)

- **`tab_counts()` lacks `contrib → totrow`** (§5.4). A contrib-coloured `tab_counts()` table may not
  get the total row `tab_build()` would force. Pre-existing; assess when Phase 7 touches contrib.
- **FIXED (Phase 6e, golden-locked)**: `tab_num(..., <tab_vars>, ci = "cell")` no longer errors — the
  grand-total grouping-set is a length-1 list and `num_rollup()` keeps every tab_var present; locked by
  golden `n_ci_tabvars` / `n_ci_tabvars_all` + the Phase 7d `test-num-fuse-parity.R` `expect_no_error`.
- **Jamovi surface gaps** (Phase 7f): no exposure of the two-channel `color`, `color_breaks`, palette,
  or the per-type empty-scale on/off.
- **`.u.yaml` enable-gates vs the resolver — reconciled/accepted in Phase 7h.** `contrib` is in fact
  always enabled (never pct-gated), so the counts case works. `ratio`/`diff` stay pct-gated → greyed on
  a pure-means table; **accepted** (documented) — `color="auto"` already colours means, so no user is
  blocked, and making them type-aware would need imperative `.js` reading `measureType`. `color_signif`
  policies re-gated `pct:row||pct:col` → `(!(color:no))`; `stars`/`conf_level`/`add_n`/`add_pct` gates
  added; `totaltab`/`comp` greyed on empty `tab_vars` (imperative). CI is a re-paint, not an auto-toggle
  (see CLAUDE.md Phase 7h + `dev/tabxplor_1.4.0_jamovi_dev.md` §6.9).
- **W2** (`new_colors_UI.md`): `color = TRUE` resolves numerics to `ratio` on the TEXT channel (diff
  off) — confirm this is what Jamovi `auto` should compute.
- **W5**: colouring the Jamovi HTML render (pandoc spans / kableExtra) is still open; the current
  HTML loses colours.

---

## 10. Where to change what (quick index)

Phase 7d-ii carved `tab_build()` into five stages matching the cache tiers (§7 / cache-design §3):

| stage (`R/tab.R`, internal) | cache tier | responsibility |
|-----------------------------|------------|----------------|
| `tab_setup(ctx)` | — | resolve + recycle args; masks; `tot_cols_type`; `pct_vect`; `tab_resolve_settings()` + `$cache_keys` |
| `tab_prepare_pop(ctx)` | 0 | prep the population once (select/filter/na/`tab_prepare()`/lump/levels-auto/lv1 pre-merge) |
| `tab_aggregate(ctx)` | 1 | numeric moment sums (`tab_aggregate_num()`) + fused factor `.fine` |
| `tab_transform(ctx)` | 3 (+2 test) | UNCHANGED `tab_num(.fine=)`/`tab_plain(.fine=)` + join + `tab_apply_tests()` |
| `tab_assemble(ctx)` | 4 | level-drop, add_n/pct, total removal, join, wrap, output shape, render |

| you want to change… | edit |
|---------------------|------|
| an argument-forcing rule (colour cascade) | `tab_resolve_settings()` — `R/tab-resolve.R` |
| a cache-key component (tiers 0-2) | `tab_cache_keys()` — `R/tab-resolve.R` |
| the numeric `color="auto"` rule | `resolve_color_auto_num()` — `R/tab-resolve.R` |
| a leaf ref/tot/totaltab resolution | `tab_plain()` / `tab_num()` — `R/tab.R` |
| a pipeline STAGE (which tier does what) | `tab_setup`/`tab_prepare_pop`/`tab_aggregate`/`tab_transform`/`tab_assemble` — `R/tab.R` |
| a per-transform computation | `tab-agg.R` (stats core) + the writer in `tab_plain`/`tab_num`/`tab_ci`/`tab_chi2` |
| a colour measure/channel/policy | the findInterval engine — `R/fmt_class.R`; use the `/color-mode` skill |
| the Jamovi option → tab() mapping | `R/jmvtab.b.R` + `jamovi/jmvtab.a.yaml`/`.u.yaml` |
