
This document is the **internal technical reference** for the tabxplor package. It describes its **current state**, but when in doubt that it’s up-to-date the code is the source of truth. It is intended for developers and AI assistants modifying the codebase. For user-facing documentation, see `vignette("tabxplor")`.

## Purpose and Design Philosophy

tabxplor creates, manipulates, and formats color-coded cross-tabulation tables for exploratory data analysis. Two core design principles underpin the entire package:

1. **Every cell carries all statistical data.** Each numeric cell is a `vctrs` record (`tabxplor_fmt`) storing count, weighted count, percentage, mean, difference, contribution to variance, confidence interval, odds ratio, and display/formatting metadata. This enables **lossless display switching**: users can change what is displayed (e.g., from percentages to differences to CI) without recalculating or losing data.

2. **Tables are tibbles with full dplyr compatibility.** Results inherit from `tibble` (via `tabxplor_tab` and `tabxplor_grouped_tab` S3 classes), so all dplyr verbs (`filter`, `mutate`, `select`, `arrange`, etc.) work out of the box while preserving table metadata and formatting.

**Performance strategy:** Aggregation is done with `data.table` internally for speed on large data frames. The user-facing API returns tibbles with `fmt` columns. Users never interact with `data.table` directly.

**CRAN stability:** This is a public CRAN package with external users. All public function signatures (argument names, defaults, return types) are part of the stable API. Internals (helper functions, class fields, color logic) can be changed freely, but public-facing arguments must not be removed or renamed without proper deprecation.

## Type System

### tabxplor_fmt — The Formatted Number Record

`tabxplor_fmt` is a `vctrs::new_rcrd()` record class defined in `R/fmt_class.R`. It is the **foundation of the entire package** — every numeric column in a tabxplor table is an `fmt` vector.

**Fields (per-cell, accessed via `vctrs::field()`):**

| Field | Type | Description |
| ----- | ---- | ----------- |
| `n` | integer | Unweighted count |
| `display` | character | Which field to show: "n", "wn", "pct", "mean", "diff", "ctr", "ci", "pct_ci", "mean_ci", "var", "pvalue", "or", "or_pct", "OR", "OR_pct", "rr" |
| `digits` | integer | Decimal places for display |
| `wn` | double | Weighted count |
| `pct` | double | Percentage (stored as 0–1, multiplied by 100 only in `format()`) |
| `mean` | double | Cell mean (for numeric column variables) |
| `diff` | double | Difference from reference. For type="mean" this is now a real difference too (Phase 2 flip); the mean/ref ratio moved to `ratio`. For pct columns: additive difference `cell_pct − ref_pct` |
| `ratio` | double | Ratio to reference. Written for numeric (mean) columns (`cell_mean / ref_mean`, Phase 2) AND, since Phase 5, for pct columns (the "×2 rule" reference-relative ratio the colour engine reads) — the old `mean`-field overload is gone. Renamed from `rr` (Phase 1a) |
| `ctr` | double | Contribution to chi-squared variance |
| `var` | double | Variance (used for CI calculation) |
| `ci_inf` | double | Lower confidence-interval bound (Phase 1a; real asymmetric bounds written in Phase 3) |
| `ci_sup` | double | Upper (absolute) confidence-interval bound. `get_ci()` = `ci_sup − ci_center` (upper arm) |
| `pvalue` | double | Per-cell significance p-value (Phase 3a: CI-inversion p; drives `get_stars()`). Phase 17c: also the HONEST home of a chi2/F test row's p (`display == "pvalue"` cells; was overloaded into `pct`/`var` with a fake `diff = -0.5`). `get_stars()` is gated to `""` on `gof`/`pvalue`/`blank` cells; `fmt_color_slots()` colours a non-significant test row (p > alpha) with the deepest under-slot on the `diff` channel |
| `or` | double | Odds ratio or relative risk ratio |
| `tot_n` | double | The cell's own (unweighted) percentage base — its row/column/grand total per `pct` (written by `tab_plain()` in Phase 2; `NA` for count tables and mean cells). The weighted base is recovered on demand by `get_tot_wn()` = `wn/pct` (not a stored field) |
| `in_totrow` | logical | Cell belongs to a total row |
| `in_tottab` | logical | Cell belongs to the total table |
| `in_refrow` | logical | Cell belongs to the reference row |

**Attributes (per-column, accessed via `attr()`):**

| Attribute | Type | Description |
| --------- | ---- | ----------- |
| `type` | character | Column type: "n", "mean", "row", "col", "all", "all_tabs" |
| `comp_all` | logical | Compare against total table (TRUE) or subtable (FALSE) |
| `ref` | character | Reference type: "tot" or "first" |
| `ci_type` | character | Which interval the `ci_inf`/`ci_sup` bounds are, i.e. their SCALE + centre. Additive (neutral 0): `"diff"`, `"diff_row"`, `"diff_col"` — centred on `diff`. Multiplicative (neutral 1): `"or"` (Phase 12a, log-OR Wald, centred on `or`), `"ratio"` (Phase 14b, Katz log-RR, centred on `ratio`). Descriptive: `"cell"` — centred on `pct` / `mean`. Absent: `""`, `"no"`. Read by `ci_center()`, `format()`'s bracket, `fmt_color_plan()`'s significance gate and the legend |
| `col_var` | character | Name of the column variable this belongs to |
| `totcol` | logical | This column is a total column |
| `refcol` | logical | This column is a reference column |
| `color` | character | Color scheme (length 1 text, or 2 with a background channel): "no", "diff", "ratio", "contrib", "or", "OR", … |
| `color_signif` | character | Significance policy: `"ignore"` / `"grey_non_signif"` / `"guaranteed_effect"` |
| `model_family` | character | Phase 15e: a reg column's own family (`"binomial"`/`"gaussian"`/`"poisson"`/…), `""` on crosstabs — lets one `tab_reg()` table mix families |
| `role` | character | Phase 17c: a reg column's role, `"model"` / `"emp"` (`""` on crosstabs) — read by the colour legend to name each column's effect without matching its `"Emp."` label (internal `get_role`) |

The attribute list is **derived** (Phase 17a): `fmt_col_attrs <- setdiff(names(formals(new_fmt)), c(fmt_field_names, "...", "class"))`, so adding an attribute (a `new_fmt()` formal that is not a field) needs no carry-site edit here — but every explicit reconstructor (`vec_cast`/`vec_ptype2`/`vec_arith`/`vec_math`) still hand-lists it beside `model_family`.

**Critical distinction:** Fields are per-cell vectors (every cell can have a different `n`, `pct`, etc.). Attributes are scalar values describing the entire column (all cells in the column share the same `type`, `color`, etc.). Do not confuse the two when modifying the class.

**Constructor chain:** `fmt()` (public, validates and coerces arguments) → `new_fmt()` (internal, calls `vctrs::new_rcrd()`).

**Adding a new field** requires updating: `new_fmt()`, `fmt()`, `format.tabxplor_fmt()`, `pillar_shaft.tabxplor_fmt()`, the relevant `vec_arith` methods, and possibly `tab_pct()`/`tab_ci()`/`tab_chi2()`. Expect ~8 functions across 3 files.

### tabxplor_tab — The Table Tibble

`tabxplor_tab` is a tibble subclass created via `tibble::new_tibble()` in `R/tab_classes.R` : it’s strenght is to work with normal `dplyr` workflows. It adds **three top-level attributes** beyond what a regular tibble carries — `subtext`, `test`, and a single **`meta`** list (Phase 17b gathered every other table attribute into it):

- `subtext` (character vector): Legend lines printed below the table.
- `test` (tidy tibble, 1.4.0 — renamed from `chi2`): whole-table test results, one row per
  (sub-table × col_var × test-type). Columns: `[tab_vars…]`, `row_var`, `col_var`, `test`
  (`"chi2"` / `"F_welch"` / `"F_classic"`; regression GOF reuses the same tibble with disjoint
  discriminators — `"n"`/`"lr_null"`/`"aic"`/… — keyed by fit column, split level in `row_var`),
  `statistic`, `df1`, `df2`, `pvalue`, `n`, `min_e` (smallest expected chi2 count, drives the weak-test
  flag). *(Phase 16a dropped the vestigial `variance` column — it was written `NA` everywhere.)* Chi-squared
  is filled for factor columns, ANOVA F for mean columns (both computed by the vectorised engine in
  `R/tab-agg.R` — `agg_chi2()` / `agg_anova()` — via `tab_chi2()`). Read it with `get_test()` (which also
  falls back to the old `chi2` attribute); `get_chi2()` is a kept alias. Rendered by the shared summary
  framework in `R/tab-test-display.R` (Phase 16a). Three shared layers, each used by both crosstab and
  regression: (1) CONTENT — `test_display_rows()`, `test_cell_label_weak()` (label + `min_e < 5` weak
  flag), the `test_fmt_*` formatters and the fmt-cell builders (`pvalue_line_fmt` / `reg_gof_cell` /
  `stat_line_fmt`) + `reg_footer_spec()`; (2) CONSOLE — `test_summary_grid()` → a backend-free grid,
  `test_render_console()` → the GFM block; (3) EXPORT — `tab_append_footer()`, the ONE fmt-frame append
  engine behind BOTH inline-row appenders (`tab_pvalue_lines()` / `reg_footer_lines()`, in
  `R/tab_classes.R` next to `tab_materialize_extras`, now thin arm-specific configs over it — a crosstab
  supplies the p-value/statistic rows keyed by grouping ∩ `test`, a regression the per-split GOF block).
The remaining metadata lives inside the ONE **`meta`** list (Phase 17b), each item an optional sub-field
(`NULL` when unset; an all-`NULL` meta attaches no attribute at all). Every legacy getter
(`get_render_extras`/`get_ci_settings`/`get_vars_attr`/`get_empirical_tips`/`get_reg_meta`/
`get_color_breaks_attr`) is a thin accessor into it; `set_meta_field(x, field, value)` writes one
sub-field (a `NULL` value removes it). The sub-fields:

- `render_extras` (Phase 10i-B) / `ci_settings` (Phase 13b) — the display-only intents, above.
- `color_breaks` (Phase 13a) — the per-table colour-break override; joined `meta` in Phase 17b so it now
  SURVIVES a dplyr pipeline (was a standalone attribute set last, silently dropped by any verb between
  build and render — defect 7). Still installed transiently at render by `push_color_breaks()`.
- `vars` (Phase 14d): `list(row_vars, col_vars, tab_vars, compacted, wt, caption, row_roles)` — the
  table's OWN record of its variable roles, written where the truth is known (`tab_assemble_tables()` /
  `tab_compact()` / `tab_plain()` at build since Phase 17b, and re-keyed by `tab_transpose()`). The
  `caption` sub-field is set by the exported `set_caption()` / read by `get_caption()` and every
  exporter's caption fallback (ahead of `reg_title`). The `row_roles` sub-field is the **row-role model**
  (Phase 17c, below). **The roles cannot be recovered from a built
  table**: `tab_compact()`
  renames column 1 to the literal `"levels"` and keeps the row-variable names only as levels of a
  synthetic column *named* `row_var`, so the "last factor is the row_var, the others are tab_vars"
  heuristic reported `row_var = "levels", tab_vars = "row_var"` on a merged table with no tab_vars —
  which is why `tab_transpose()` aborted over tab_vars that were never there and a `tab_xl` title read
  *"levels by multi (tabbed by row_var)"*. Sniffing for a column named `row_var` would be the ad-hoc
  layer this replaces. `tab_get_vars()` / `tab_render_vars()` read it via `tab_vars_recorded()`, which
  **validates it against the real columns** (a dplyr chain can rename or drop them) and returns NULL →
  the heuristic fallback, so hand-built tables (`tab_plain()`, `tab_num()`, older objects) still work.
  ⚠ `tab_get_vars()`'s `row_var`/`tab_vars` stay **column** names (what consumers index with);
  `row_vars` carries the **source** names, which differ on a merged table.

Constructors: `new_tab(tabs, subtext, test, meta)` (the old `chi2 =` argument still works, mapped to
`test`; Phase 17b replaced the five 1.4.0-new formals with the single `meta` list) and
`new_grouped_tab(tabs, groups, …)`.

**Adding a `meta` sub-field** is one getter + (rarely) one producer line — never a constructor formal.
`tab_attrs()` returns exactly **three** things (`subtext`, `test`, `meta`); `tab_restore(out, from)`
rebuilds a table from a template (used by every dplyr S3 method, with `lv1_group_vars()`'s
auto-downgrade) and `tab_bind_attrs(x, other)` reconciles a bind (the vctrs `ptype2`/`cast` pair:
`subtext` unions, the row-bound `test` rbinds, and `tab_meta_bind()` reconciles the `meta` sub-fields
element-wise — x wins, other fills a `NULL`, except `color_breaks` which merges per named scale). Before
Phase 14d each verb named every attribute by hand (~34-site edits, silent drops); Phase 17b collapsed
the six 1.4.0-new attrs into `meta` so the carry list is now three lines total. The jamovi tier-3 carrier
stores `attributes(tab)` verbatim, so `meta` round-trips transparently (schema bumped to invalidate
stores holding the old multi-attr shape).

### The row-role model (`meta$vars$row_roles`, Phase 17c)

Synthetic rows created at DISPLAY time — the add_n / add_pct base-`n` and `row_pct` rows, the chi2/F
`pvalue` rows, the regression `gof`/`blank` footer rows — used to be re-detected downstream by matching
their rendered English row label (`%in% c("n", "pvalue", "row_pct", …)`), which silently broke under
jamovi's gettext translation. Phase 17c stores each row's **kind** instead. `meta$vars$row_roles` is a
positional character vector (`"data"`/`"total"`/`"n"`/`"row_pct"`/`"pvalue"`/`"gof"`/`"blank"`), length
`nrow`, stamped entirely within the one uninterrupted materialise pass:

- **seeded** at the top of `tab_materialize_extras()` from the drift-free `is_totrow()` flag;
- **extended** by each row-adder — `tab_add_n_pct()` threads `role` through `tab_append_pctcol_rows()`
  (spliced by the SAME re-order as the rows), `tab_append_footer()` interleaves a `row_role(g)` closure
  per group exactly as it interleaves the non-fmt columns;
- **sliced** by `tab_collapse_total_rows()` alongside its row drop.

It is never persisted in the user-facing built table (materialise is display-only). Consumers read the
resolver **`tab_row_roles(tab)`** (`R/tab.R`, next to `tab_render_vars`): the stored vector when present
and length-matching, else a clearly-marked FALLBACK reproducing the old `is_totrow` + English-label
detection for hand-/step-built tables (a table with no stored vector never has a `row_pct` row, so the
fallback needs no `row_pct` case — exact by construction). The retired consumers: export-prep's tot-block
border (`tab_row_roles(tab) != "data"`), `tab_collapse_total_rows`'s summary-row sweep, and the transpose
absorb heuristic (fixed structurally on `col_var == "all_col_vars"`, since transposed "rows" come from
original columns). Column-side kinds stayed structural and English-free (`<var>_sd` suffix, `type=="n"`,
`col_var=="all_col_vars"`, `is_totcol()`), so there is deliberately **no** `col_roles` vector.

### tabxplor_grouped_tab — Subtabled Results

When `tab_vars` are provided, the result is a `tabxplor_grouped_tab` — a `grouped_df` subclass. It carries the same table attributes, plus `groups` data from dplyr.

This class requires a separate S3 method for **every dplyr verb** to preserve class and attributes through operations. See the dplyr Integration section below.

## Calculation Pipeline

Since Phase 6 (1.4.0) both public entry points are thin wrappers over the internal engine
`tab_build()` (`R/tab.R`). `tab()` and `tab_many()` differ only in the default output shape they pass
(`tab()` merges >=2 row_vars; `tab_many()` keeps a list). `tab_build()` is a thin **five-stage
pipeline** threading a `ctx` list (Phase 7d-ii); the stages match the jmvtab cache tiers so Phase 7e
can drive them at cache granularity — the SAME functions, no math fork:

```
tab(data, row_vars, col_vars, ..., output_list=FALSE)   tab_many(..., compact=)  [soft-deprecated]
  └────────────────────────┬───────────────────────────────────────┘
                           ▼
       tab_build(...)  = argument surface: defuse NSE args + apply `filter`, build ctx, run:
        ctx |> tab_setup |> tab_prepare_pop |> tab_aggregate |> tab_transform |> tab_assemble
         │
         ├─ tab_setup(ctx)        (no tier)  RESOLVE + build the SETTINGS SPINE: tidy-select var roles,
         │      factor/numeric masks, totcol→tot_cols_type, the arg-overwrite cascade + cache keys via
         │      tab_resolve_settings() (pure, data-free), then `ctx$settings` = the rows/cols/pairs star
         │      schema (Phase 17e — see "The settings spine" below)
         ├─ tab_prepare_pop(ctx)  (tier 0)   PREP the population ONCE: select+relabel, apply filter,
         │      na_text/na_num, tab_prepare() (ordered-strip + listwise removal + lump + cleannames),
         │      zero/NA-weight removal, levels="auto", lv1 non-first-level pre-merge
         ├─ tab_aggregate(ctx)    (tier 1)   the count/moment aggregates: per-row_var numeric moment
         │      sums via tab_aggregate_num() (NULL under .by_table). Factor `fine_fused` is NULL for plain
         │      tab()/tab_many() (Phase 9c removed the opt-in factor scan-fusion, §30); the factor `.fine`
         │      seam now feeds only from jmv_cache_aggregate() (jmvtab) + tab_counts()'s injected aggregate.
         │      Phase 7e: when a jmvtab cache_env is present this stage delegates to jmv_cache_aggregate()
         └─ tab_build_tables(ctx)  the OUTER row_var map + the output shape (Phase 9a). Resolves one lean
                ctx per row_var (tab_rowvar_ctxs) and maps tab_build_one() over it — serial purrr::map OR
                mirai, the single dispatch — then tab_assemble_output() once on main. Each unit runs:
                ├─ tab_transform(ctx)    (tier 3 + tier-2 test) SCALAR over one row_var: the UNCHANGED
                │      tab_num(.fine=)/tab_plain(.fine=) leaves (pct/diff/ratio/or/CI + fmt, O(cells);
                │      `.fine` may be a per-pair list, fine_for_pair) + factor join + tab_apply_tests()
                │      (chi2/ANOVA → capture `test` → tab_ci; BEFORE the level-drop — the ordering invariant)
                └─ tab_assemble_tables(ctx)  (tier 4) SCALAR: level-drop, tab_add_n_pct(), total col/row
                       removal, num+factor join, test-merge + rewrap (new_tab/new_grouped_tab)
             tab_assemble_output() [cross-row_var: output shape (§13), tab_compact() merge,
                tab_pvalue_lines(), tab_spread(), unwrap, optional tab_kable()]
```

The `ctx` list is built by the typed constructor **`new_ctx()`** (Phase 17e): every field has ONE default
in one place, so `tab_build()` and `tab_counts()` no longer hand-write (drifting) `list(...)` literals and
the scattered `exists(<field>, inherits = FALSE)` lean-ctx guards are gone (a leaner caller just omits a
field and inherits its default). `chi2` stays the per-row_var logical flag and `tests` holds the captured
whole-table test tibbles (no name overload).
`ctx_update()` repacks NULL-safely (single-bracket `[<-`, not `$<-` which deletes on NULL); `new_ctx()`
reuses it as its body, so an explicit `totcol = NULL` is a present-but-NULL key (the rule the downstream
`list2env()` needs, now encoded in the helper instead of comments).
`tab_apply_tests()` is the ONE place both `tab_build()` and `tab_counts()` build the chi2/ci calls
(Phase 6a). `tab_counts()` reuses the SAME stages: it holds its aggregate, so it builds a single-pair
ctx, runs `tab_setup()` (incl. the `tot`→totrow/totcol translation tab() uses) then `tab_transform()`
+ `tab_assemble()`, injecting its counts as the fused tier-1. `tab_lump_others()` / `tab_cleannames_relabel()`
are the two factor-relabel steps extracted from `tab_prepare()` (which still composes them for its
public callers); jmvtab (Phase 7e) runs cleannames at display instead (`jmvtab_cleannames_display()`).
`tab_compact()` (`R/tab_classes.R`) is the internal merge invoked when `output = "single"`.

**Row-axis dispatch (Phase 8 parallel + Phase 9a outer-map collapse, `R/tab-parallel.R`, Suggests-only
mirai).** `tab_build()` runs `tab_setup` + `tab_prepare_pop` + `tab_aggregate` ONCE on main (the global
`na="drop_all"/"common_base"` population drop lives in prep and cannot move; `tab_aggregate` builds the
shared `fine_fused` + the per-row_var numeric aggregates + fires the jmvtab hook), then hands off to
`tab_build_tables()`. That helper resolves one lean ctx per row_var (`tab_rowvar_ctxs()`, which Phase 17e
now slices from the settings spine by explicit KEY — `settings$rows[i, ]` for the per-row scalars,
`settings$pairs` filtered to this row_var for each pair's pct/ref — retiring the former
`if (length(x) == n) x[[i]] else x` heuristic) and maps
the whole-per-row_var worker `tab_build_one()` = `tab_transform |> tab_assemble_tables` over it via
`tab_pmap()`: `purrr::map` when serial (the default, byte-identical, zero overhead) or a NAMED `"tabxplor"`
daemon pool when `parallel` is on and there are ≥ `tabxplor.parallel_min` row_vars (the prepared `data` +
`fine_fused` shipped once via `everywhere()`). Main gathers the finished per-row_var tabs and runs
`tab_assemble_output()`. This is byte-identical because a single-row_var build equals its slice of the
integrated build — guaranteed by the `tab_assemble` total-col decoupling (`totnames |> unique()`, so the
lone-total rename-back tests the distinct name, not its occurrence count). jmvtab (cache_env) forces serial
and keeps its hooks (`jmv_cache_aggregate` in `tab_aggregate`; `jmv_cache_store_tests` in `tab_build_tables`,
reading the gathered pre-merge tests).

### The settings spine (`ctx$settings`, Phase 17e)

The argument-normalisation boundary is the historical "top bug factory": five documented bugs came from
recycling arguments across the `row_var × col_var` axes with vectorised `&` / length heuristics. Phase 17e
makes that class unrepresentable by combining the two axes ONCE, in `tab_setup()`, into a **star schema**
stored at `ctx$settings`:

- **`rows`** — one row per row_var; the per-row_var scalar settings (`color`, `OR`, `chi2`, `ref`, `ref2`,
  `comp`, `ci`, `ci_scale`, `totaltab`, `totrow`, `color_diff_OR/ctr/ci/num`).
- **`cols`** — one row per col_var; the per-col_var settings + the factor/numeric masks (`is_num`,
  `is_text`, `lvs`, `digits`).
- **`pairs`** — one row per (row_var × col_var), the fact table carrying `pct` and `ref`. Built
  `row-major` via `expand_grid(row_var, col_var)`, so it is byte-identical to (and REPLACES) the former
  `pct_vect` (5-branch nested list) and `ref_vect` (2-branch) ctx fields — those axes now meet only here.

`tab_rowvar_ctxs()` slices this by key (above), so the `length(x) == n` guessing is gone. The per-row_var
**population/aggregate** objects — `na_text`, `na_num` (a `tab_prepare_pop` na-policy detail) and `fine_num`
(a `tab_aggregate` product) — are NOT settings; they stay per-row_var objects sliced by index / by name.
The flat per-row scalar ctx fields remain alongside `rows` for the pre-slice stages + the jmvtab cache that
still read them directly; `rows` is a view assembled from them at build. The typed ctx (`new_ctx()`) and
this spine are the foundation the Phase 17f reference plan + leaf wrapper/core split build on.

**jmvtab live cache (Phase 7e, `R/jmvtab-cache.R`).** The jamovi module reuses this exact pipeline
via `jmvtab_build()`, which calls `tab()` with a content-addressed multi-tier store injected through a
mutable `cache_env` (the `.cache` arg on `tab()`/`tab_build()`). Only the two expensive costs are
persisted (in a hidden Image result element's `$state`): tier-1 aggregates (per-(row_var × col_var)
counts / per-row_var moment sums) and tier-2 omnibus tests (chi2/ANOVA). **Phase 7f adds a tier-3
`tab3` cache** of the pre-`finalize` ARMED table, keyed by a base-key {aggregate identity + pct +
na + levels + structural opts} with a stored transform-tuple {ref/ref2/comp/OR/ci/arming}: `tab()`
gained a `.return_armed` seam so `jmvtab_build()` applies `finalize_color_spec` + `jmv_reapply_digits`
+ `jmv_apply_display` FRESH each run, and on an exact-tuple hit the whole O(cells) build is skipped —
so display / colour toggles (digits, measure `diff`/`ratio`, `color_signif`) are instant. The
`color_signif` policy is a pure re-paint (the armed table is built canonically with
`color_signif="ignore"`); the field-level ref / expert-CI re-ref is deferred to Phase 7g (its
byte-identical foundation is the shared `tab_apply_reference()` carved out of `tab_plain`).
`tab_aggregate()`'s hook builds/looks-up the per-pair aggregates
(`jmv_cache_aggregate()`, byte-identical to `tab(cleannames = FALSE)`); `tab_transform()` adopts them
per pair (`fine_for_pair()`) and reuses a cached test via the `cached_test` hook on
`tab_apply_tests()`; `defer_level_merge` keeps full levels so `levels` is a display-time drop. Keys use
per-column fingerprints so adding a variable reuses the other pairs.
`tab_resolve_settings()` (`R/tab-resolve.R`, Phase 7b) is the ONE place the argument-overwrite cascade
lives — a pure function of (args, column classes) that draws the boundary between static arg resolution
(here) and data-dependent resolution kept at the leaf (`ref="auto"`/regex, `levels="auto"`, `na`-drop).
It is what the Jamovi `.js` mirrors and the Phase 7c cache keys on. Full arg↔computation map:
`dev/tabxplor_argument_computation_map.md`.

### tab() vs tab_many() (both wrap tab_build)

- `tab()` is the unified entry point: `row_vars`/`col_vars` accept one variable OR several (tidy-select);
  with several `row_vars` it **merges** by default (`output_list = TRUE` → a list). Singular
  `row_var`/`col_var` are soft-deprecated aliases.
- The row_var axis is **globalised** on `tab()`: `OR/pct/color/comp/ci/chi2/ref2` are scalar (one value
  for all row_vars). Still per-row_var: `totaltab` and `ref` (a named/ordered vector, one reference row
  per row_var). The col_var axis stays flexible: `pct/levels/digits` are per col_var. `levels`
  (`all`/`first`/`auto`) is a `tab()` argument again (Phase 7a — a Phase 6 oversight had hardcoded it);
  `sup_cols` is **soft-deprecated** (fold into `col_vars` + `levels = "first"`).
- `tab_many()` is a **soft-deprecated** alias keeping its historical list return; it maps the deprecated
  `compact` argument onto the output shape and still accepts per-row_var vectors (the engine recycles).
- `na` (microdata only, per Phase 7a): `"keep"` (NA as a level), `"drop"` (each col_var drops its OWN
  NA → bases can differ), `"drop_all"` (drop obs missing on `{row_vars, any col_var, tab_vars}` → one
  shared base; `tab_build` resolves it natively), `"common_base"` (reproduces the historical `tab()`
  population: drop NAs of `{row_vars, first col_var, tab_vars}` globally, keep secondary col_vars' NAs).

**`tab_counts()` (Phase 4) is the from-the-middle sibling of `tab()`**: same output, but the input is
already-aggregated counts (long / wide / `table` / freq+N). It does not scan microdata — it feeds a
count-aggregate into `tab_plain()`'s `.fine` entry, then runs the same finalize (`tab_apply_tests()` +
tail). See the `R/tab-counts.R` file guide below.

### tab_many() Vectorisation Philosophy

`tab_many()` processes multiple variables with a key asymmetry:

- **col_vars** all share the same percentage type and color settings (they form one table).
- **row_vars** can have different color, ref, OR, chi2, and CI settings (separate tables that are optionally compacted together).

Arguments vectorised over row_vars: `totaltab`, `totrow`, `ref`, `ref2`, `OR`, `comp`, `color`, `ci`, `chi2`.
Arguments vectorised over col_vars: `levels`, `digits`, `totcol`, `pct`.

### Compaction: what is lost when tables are bound (the `output_list` decision)

By default `tab_many()` returns one table: the per-row_var results are bound vertically with `tab_compact()` (`R/tab_classes.R`), which stacks each variable's levels as rows (a `row_var` factor column marks the source) sharing the **same** `col_var` columns. A list of separate tables is available on demand (from 1.4.0, `output_list = TRUE`; the old `compact` argument is deprecated). This section documents exactly what compaction costs, so the single-table default is a conscious trade-off rather than a hidden one.

The `fmt` record splits into per-cell **fields** and per-column **attributes** (see the Type System section). When blocks are row-bound, the two behave differently:

- **Per-cell fields** (`n`, `wn`, `pct`, `mean`, `diff`, `ctr`, `var`, `ci`, `rr`, `or`, `in_*`) are simply stacked as rows and are **fully preserved**.
- **Per-column attributes** (`type`, `comp_all`, `ref`, `ci_type`, `col_var`, `totcol`, `refcol`, `color`) are scalar — one value per column — so if two variables were computed with different settings for the *same* column, the merged column keeps only **one** value.

Concretely, for each argument vectorised over row_vars:

| row-vectorised arg | backed by | effect of compaction | genuinely needed? |
|---|---|---|---|
| `color` | `color` attribute | color **mode** collapses to one; the underlying `diff`/`or`/`rr` **values** stay (fields) | no — analysts use one colour scheme per table |
| `ref` / `ref2` | `ref` attribute + baked diffs | each block's diffs are already computed against its **own** total (compaction promotes each block's total to its reference row before binding), so the **displayed** result is preserved; only re-computability against a different ref collapses | displayed result preserved; divergent-ref recompute is not needed |
| `comp` | `comp_all` attribute | moot: compaction requires **no** `tab_vars`, and `comp` only matters with `tab_vars` | not applicable |
| `OR` | `or`/`rr` fields + color mode | values kept; only the colour-mode/`totcol` side collapses | no |
| `ci` | `ci_type` attribute + `ci` field | CI **values** kept; the CI **type** (cell / diff_row / diff_col) collapses to one | rarely differs per variable |
| `test` | table-level `test` tibble (Chi2 + ANOVA F) | **concatenated** across blocks, not lost | preserved |
| `totrow` / `totaltab` | rows + fields | total rows **stack as rows**; each block's total becomes its reference row | preserved |

Two structural limits of `tab_compact()`:

- It **errors** if the bound tables have different `col_vars`. In practice `tab_many()` gives every row_var the same `col_vars`, so this is not a real loss.
- It **refuses** tables that carry `tab_vars` (returns them unchanged). So when `tab_vars` are present, a multi-row_var call cannot be compacted and the multi-table structure is kept regardless of `output_list`.

**Bottom line.** What the single-table default gives up is per-row_var flexibility that real data analysis does not use — a *different* colour mode, reference, or CI type for the *same column* across variables. The one case analysts genuinely rely on — each variable coloured against its own total — survives, because it lives in the cell fields and each block's reference is baked in before binding. Everything else is opt-in recoverable with `output_list = TRUE`, which is also the entry point for manual per-table editing.

### The leaves — `tab_plain()` / `tab_num()` (wrapper + resolver + core, Phase 17f)

Since Phase 17f each leaf is a **public wrapper over a resolved-args core**, so the argument forcing runs once and the pipeline never re-does the leaf's resolution:

- **Public `tab_plain()`/`tab_num()`** = the CRAN surface. They defuse the NSE args, then `tab_num` additionally normalizes colour to a spec; then they call the shared resolver and the core. `tab_num` finalises the colour ONCE after the core returns (`tab_plain` never finalises — the outer `tab()`/`tab_many()` does).
- **Shared resolver `plain_resolve()`/`num_resolve()`** = the pure argument forcing (`pct`/`OR` → `tot` → `comp` → `ref = "auto"` → `totaltab`). `plain_resolve` also does the validate (digits cast, `total_names` recycle); `num_resolve` is forcing-only. **`tab_transform` (the pipeline) calls the resolver + core directly**, so there is no double `finalize_color_spec` and no `.color_deprecate` flag.
- **Compute core `plain_core()`/`num_core()`** = the data.table aggregation + fmt build, consuming already-resolved scalars. Returns the **pre-finalise** table. Shared tails `leaf_totrow_tottab()` (total-row/total-table flags) and `leaf_rename_totals()` (the `#Rename totals` block) live once. `df=`/`num=` build normally then extract `get_num()` per cell via `leaf_extract_raw()` (no duplicated raw scan).

`tab_plain()` is where raw cross-tabulation happens:

1. **data.table dcast**: `data.table::dcast(DT, row_var ~ col_var, fun.aggregate = sum)` for weighted counts. Column names are temporarily prefixed to avoid data.table reserved name conflicts.
2. **Wrap in fmt**: Raw counts are wrapped into `fmt` vectors via `new_fmt()`.
3. **Add totals**: Total rows and/or columns are added based on `tot` argument.
4. **Reference + tests**: the reference-relative fields come from the ONE executor `tab_apply_reference()`; `tab_transform` then runs `tab_apply_tests()` (chi2/CI). The superseded standalone steps `tab_pct()`/`tab_tot()`/`tab_totaltab()` now live in `R/tab-steps-legacy.R`, off the build path.
5. **Restore names**: Internal prefixes are removed; original column names restored.

### tab_num() — Numeric Column Variables

When a `col_var` is numeric (not a factor), `tab_num()` is used instead of `tab_plain()`. Since Phase 2 (1.4.0) each grouped `data.table` scan computes **sufficient moment sums** (`n`, weighted `n`, `Σ[w]x`, `Σ[w]x²`) in one pass; `num_derive_stats()` (`R/tab-agg.R`) then derives the mean and variance, reproducing the unweighted sample (n-1) vs weighted ML (÷Σw) definitions exactly. This removed the old `weighted.var()` double scan, and the total rows / total table are built as **roll-ups** of the additive moment-sum aggregate (`num_rollup()`) instead of two extra full-data scans. Net on 8M rows: `tab_num` ~5.6×/8.3× faster and ~6×/11× less memory (unweighted/weighted). The resulting `fmt` vectors have `type = "mean"` and `display = "mean"`.

Since Phase 7d-i, the moment scan itself lives in a shared `num_moment_scan()` (`R/tab-agg.R`) and `tab_num()` has the same aggregate-injection seam as `tab_plain()`: a `.fine` parameter (built by the tier-1 producer **`tab_aggregate_num()`**, `R/tab-agg.R`) plus `.by_table`. `tab_build()` builds one moment aggregate **per row_var** and hands it to `tab_num(.fine=)`, which `data.table::copy()`s it and skips the scan; `.by_table = TRUE` forces the table-by-table scan. This relocates the single O(N) scan out of `tab_num` (perf-neutral — no fork, no double scan) and makes the numeric tier-1 aggregate a first-class object the jmvtab cache (Phase 7e) will hold, mirroring the factor `.fine`.

### The Reference System

The `ref` argument controls which row serves as the comparison baseline for differences and colors:

- `"auto"`: defaults to `"first"` when OR requested, `"tot"` otherwise
- `"tot"`: the total row is the reference (differences = cell - total)
- `"first"`: the first non-total row is the reference
- integer: specific row index
- string: `diff_index()` tries an **EXACT** match against the row/column labels first, then falls
  back to a **regex** (Phase 7g-iii) — so a chosen level label with regex metacharacters (e.g.
  `"$25000 or more"`) or that is a substring of another level selects exactly its own row/column
- `"no"`: skip difference calculation entirely

`ref` is resolved **per axis variable** by `resolve_ref_vector()`: a scalar applies to all; a
**named** vector matches by name (unmatched → `"auto"`), including a named length-1 vector like
`c(race = "Black")`. Under `pct = "col"` a ref **named by col_var** gives each col_var its own
reference column (Phase 7g-iii): `tab_setup()` writes the per-(row_var × col_var) reference into the
settings spine's `pairs$ref` column (Phase 17e — it replaced the former standalone `ref_vect` nested
list, the reference analogue of `pct_vect`), which the slicer projects into the factor leaf `tab_plain()`
as this row_var's per-col_var reference vector — the col%
math is unchanged (one col_var per leaf, so the leaf *is* the per-col_var group). `tab_ci()` reads
the marked reference column via `detect_refcol()` (fmt_class.R) so the diff-CI reference matches
the diff/colour reference.

The `comp` argument adds another dimension:

- `comp = "tab"` (default): compare within each subtable's own total
- `comp = "all"`: compare against the total table's total (across all subtables)

### Mean diff vs ratio (Phase 2 flip)

For `type = "mean"` columns the `diff` field is a real **difference** (`cell_mean − ref_mean`), like pct columns; the mean/reference **ratio** lives in the `ratio` field (`cell_mean / ref_mean`). Since Phase 5, `color = "diff"` on numeric means colors the **sd-standardized** difference (Glass's Δ = `diff / sqrt(ref var)`) against the `mean_diff` scale (`c(0.2, 0.5, 0.8)`), while `color = "ratio"` colors the `ratio` field against `mean_ratio` (`c(1.2, 1.5, 2, 4)`). For **pct** columns the `mean` field is now `NA` (the old mean/×2 overload is gone; the `ratio` field carries the relative risk that drives the ×2 rule). So `mean` now means an actual mean on `type = "mean"` columns and nothing else — the field is single-purpose since Phase 5.

### Confidence Intervals & significance stars (Phase 3a)

Since Phase 3a the CI is **real asymmetric bounds** in `ci_inf`/`ci_sup`, plus a per-cell
significance `pvalue`, all computed by the **closed-form vectorised engine in `R/tab-agg.R`** (no
`DescTools` at runtime). Two interval *shapes*:

- **pivot** — `estimate ± q·se` with a continuous inversion p (`ci_pivot()`): serves Agresti-Caffo
  & Wald (proportion diff) and z / Welch-t (means).
- **score** — asymmetric Wilson (`ci_wilson()`, cell proportion) and its hybrid Newcombe-10
  (`ci_newcombe()`, proportion diff), the latter's inversion p found by a vectorised bisection.
- **multiplicative** (Phase 14b) — Katz's log-RR (`ci_katz_rr()`, proportion RATIO): the bounds are on
  the **ratio scale** (neutral 1, `ci_type = "ratio"`, centred on the ratio), its dual the log-RR Wald.

Defaults: **Wilson** (`ci="cell"`), **Newcombe** (`ci="diff"`, `method_diff="newcombe"`; also `"ac"`,
`"wald"`), Welch-t (means). `tab_ci()` (proportions) and `tab_num()` (means) both route through the
engine; the reference (`x_n`, `ref`, `ref_n`) is the **weighted estimate + unweighted n** of §14.

**Which scale (Phase 14b): the interval belongs to the measure the reader sees.** Only ONE interval is
stored per column, so it is the *text* channel's: `color = "ratio"` / `c("ratio","diff")` on percentage
columns gives Katz bounds (`tab_ci(ci_scale = "ratio")`, resolved once in `tab_resolve_settings()` →
`ci_scale`), and a background diff channel derives from them; every other `color` keeps the difference
interval, where a ratio channel derives instead. `fmt_color_plan()`'s `rescale_bound()` maps a bound
either way — `diff` and `ratio` are both affine in the cell proportion with the reference at its point
estimate (`ratio − 1 = diff / p_ref`). Percentage columns only: a mean keeps its difference interval
(a ratio of means needs Fieller's theorem), which is also why the numeric default is unaffected even
though a mean's text channel is the ratio under `color = TRUE`. `ci = "cell"` has no ratio counterpart.

**Significance reads the stored `ci_type`, not the measure** (Phase 14b): an interval is significant
when it excludes ITS OWN neutral — 0 for the additive `diff*` scales, 1 for the multiplicative `or` /
`ratio` ones. All three test the same null (`p1 = p2`), so whichever interval is stored answers it.

**Significance = universal CI-inclusion**: the stored `pvalue` is the inversion p of the *same*
interval that draws the bracket, so `get_stars()` (`*`/`**`/`***` from `options("tabxplor.signif_levels")`)
can never disagree with the bracket. Stars are **opt-in** (STORAGE-driven): `stars` arg default
`FALSE` (option `tabxplor.stars` = `FALSE`), `ci="diff"` only — a plain `tab()` stores no `pvalue`
(so `get_stars()` is `""`); `tab_reg()` sets `stars = TRUE` itself. Display is a separate opt-in:
`format(x, stars = FALSE)` by default — the MAIN display sites (`pillar_shaft`, `tab_kable`, `tab_md`,
and the `tab_xl` numFmt fold) pass `stars = TRUE`, while tooltip / character-cast re-renders keep the
default `FALSE`, so stars never leak onto secondary fields. When shown, `format()` **right-pads** each
value cell's star field to the column-max width so numbers stay aligned. `pvalue` feeds ONLY the stars
(colour significance reads the bounds), so not storing it when stars are off changes nothing else.
Kish `n_eff` opt-in for numeric CIs via `options("tabxplor.kish_neff")` (needs the `Σw²` accumulator,
added to the numeric scan only when opted in).

Accessors: `get_ci()` = upper arm (`ci_sup − ci_center`, retro-compatible with the `$ci` field extraction);
`get_ci_moe()` = larger arm for the `± moe` display; `fmt(ci=)` stores absolute symmetric bounds
around the estimate. `format()` reads `ci_inf`/`ci_sup` directly (× 100 for proportions, clamped to
`[0,100]`), then appends `get_stars()`. Two display modes via `options("tabxplor.ci_print")`:

- `"moe"`: `value ± margin` (e.g., `45% ±3`, the conservative larger arm)
- `"ci"`: `[lower; upper]` (e.g., `[42%; 48%]`) — the default

## Color System

The color system has three layers, all working together to determine which cells get which colors at which intensity.

### Layer 1 — Palettes (Phase 13a OKLCH)

Eight base palettes are defined as unnamed 4-hex vectors in `R/tab_classes.R` (`## NEW COLOR PALETTES`): `default_text_colors`, `default_text_colors_neg`, `default_background_colors`, `default_background_colors_neg`, and the `default_dark_*` counterparts — one per (light/dark theme × text/background channel × over/under side), each 4 hex codes ordered faint → strong (position-based, no `pos1..neg5` names, no `ratio` slot). At load `set_color_palette()` seeds them into an internal `tabxplor_palette_env` and pre-builds the terminal (ANSI) style functions with `cli` (`build_palettes()` → `e$ansi`; `crayon` was dropped in favour of `cli`); users override any of the eight with `set_color_palette(text_colors = , …)`.

A ninth palette (Phase 14c, light only) — `default_bg_legend_colors` / `_neg` — is the **font stand-in for the background palette**, used only where a fill is impossible: an Excel rich-text run and a `ggpubr` text label carry a font colour but no fill, so a background break-word in the colour legend would be drawn as text — and the background hues (L 0.85–0.97) are invisible on a white page. They are the same hues at −0.2 OKLCH lightness (chroma kept, capped to gamut), produced by `dev/color_palette_tools.R::darken_for_legend()`. There is no dark counterpart: an Excel legend cell sits on a white page whatever the `theme`, the dark fills (L 0.20–0.35) already read there, and −0.2 would collapse the ladder to black — so `build_palettes()` maps `bg_legend_dark` onto the dark background palette unchanged. `set_color_palette(background_colors =)` without `bg_legend_colors =` makes the legend palette follow the fills verbatim (readable only if they are).

**Console theme detection (Phase 14g, `R/tab-theme-detect.R`).** `tx_detect_theme()` → `"light"`/`"dark"`, feeding `set_color_palette(theme = "auto")` and `.onLoad`. Layered by confidence: RStudio's `getThemeInfo()$dark` → Positron's theme → `COLORFGBG` → `"light"`. It rests on **no supported API** and must therefore never error, never warn, and never guess — every probe is wrapped and returns NULL rather than a hunch. Shape borrowed from `cli:::detect_dark_theme()`, extended with the Positron branch (which cli, thematic and every other R package lack — [positron#2986](https://github.com/posit-dev/positron/issues/2986) has been open since 2024-05).

The Positron chain: VS Code caches the CLIENT's `settings.json` server-side under `~/.positron-server/data/User/History/<hash>/` (updated on live writes), so `workbench.colorTheme` → the declaring extension's `package.json` → `uiTheme: vs-dark` resolves the real theme. Traps, each source-verified and encoded in the code:

- **`tx_ide()` (which host) must not trust the env vars alone** (Phase 16f). The ark R *console* rebinds `.Platform$GUI == "Positron"`, so it is detected directly; but `POSITRON` / `TERM_PROGRAM` are **unstable** across Positron's other processes and versions (measured `POSITRON = "1"` in the integrated terminal in 2026-07, both empty in 2026-07-20 while only `VSCODE_CWD` was set → `tx_ide()` fell to `"vscode"` and dark mode was missed). Positron is a VS Code fork distinguished from plain VS Code by its **server-side cache**: a VS Code env var (`VSCODE_PID`/`VSCODE_CWD`/`TERM_PROGRAM`) **plus** `dir.exists(~/.positron-server)` → `"positron"`. The `.positron-server` root is `tx_positron_server_dir()` (one source of truth; `positron_dir` is injectable so tests mock it).
- `rstudioapi::isAvailable()` **lies in Positron** (ark fakes it TRUE, then `findFun()` stops) → gate on `hasFun()` **and** `RSTUDIO == "1"`. `readRStudioPreference()` is worse: ark's shim is `function(name, default) default`, so it always returns your default. `$dark` can be `NA` even in real RStudio → `isTRUE()`.
- **The theme NAME is not a signal** — this maintainer's *"Starless Monokai Atom"* contains neither "dark" nor "light" and is `vs-dark`. Exact-name → `uiTheme`, never a regex.
- `window.autoDetectColorScheme = true` → **bail**: the live theme then comes from `preferredDark/LightColorTheme` following the OS, so `colorTheme` is stale.
- **PRIVACY**: that settings file also holds credentials. Two keys are pulled by regex; it is never parsed (it is JSONC anyway), so nothing else enters R. Do not widen it.
- **Cost**: the extension scan is ONE level deep (`<ext_dir>/<pub.name-ver>/package.json`) — recursive walked every extension's source and cost 70 ms at load, vs 9 ms now, and only inside Positron.
- `.ps.ui.evaluateWhenClause` (ark's private RPC, carrying `# TODO: Unexport these methods`) is **not used**: the settings chain resolves without it.
- ⚠ The suite pins `tabxplor.color_style_theme = "light"` in `setup.R` — detection makes the default machine-dependent, which would pass on CI and fail on a dark editor.

`get_color_style(mode, type, theme)` returns an **8-element slot vector** = 4 over-represented intensities then 4 under-represented, for the given channel × theme. `type` is `"text"` / `"bg"` / `"bg_legend"` (the last is `mode = "color_code"` only — it exists to substitute for a fill, and a console has one, so asking for its terminal styles aborts). `mode = "crayon"` (the value is frozen for back-compat; the styles are now `cli`-built, stored in `e$ansi`) gives console functions (24-bit truecolor, or a curated 8-bit `palette_8bit` in the RStudio console); `mode = "color_code"` gives hex (exports, always 24-bit). There is no `html_24_bit` / green_red-blue_red variant / `set_color_style()` / `custom_palette` anymore.

### Layer 2 — Breaks (Phase 13a over/under scales)

Breaks live in `options("tabxplor.color_breaks")` as a **named list of six scales**, set by `set_color_breaks(list(...))` or `set_color_breaks(pct_diff = , …)`. Each scale is `list(center, strict, std, over = list(breaks, slots), under = list(breaks, slots))` — both sides carry POSITIVE magnitudes (the engine folds each cell to a magnitude and picks the side by direction) and the intensity `slots` (1..4) each break maps to.

| Scale | Applies to | Default | `center` | notes |
| ----- | ---------- | ------- | -------- | ----- |
| `pct_diff` | factor difference (pp) | `c(0.05, 0.1, 0.2, 0.3)` (mirrored) | 0 | additive |
| `pct_ratio` | factor relative risk (`color = "ratio"`) | `list(over = c(NA,1.5,2,4), under = c(NA,1.5,2,4))` (symmetric) | 1 | multiplicative; free to set asymmetrically (e.g. the "×2 rule") |
| `odds_ratio` | factor odds ratio (`color = "OR"`) | `list(over = c(1.2,1.5,2,4), under = c(1.2,1.5,2,4))` (symmetric) | 1 | multiplicative; Phase 16c — OR's own scale, so `pct_ratio` no longer serves OR |
| `mean_diff` | numeric difference | `NULL` → `c(0.2, 0.5, 0.8)`, `std = TRUE` | 0 | sd-standardized (Glass's Δ); data-unit values → absolute |
| `mean_ratio` | numeric ratio | `list(over = c(1.2,1.5,2,4), under = c(1.2,1.5,2,4))` (symmetric) | 1 | multiplicative |
| `contrib` | χ² contribution | `c(1, 2, 5, 10)` (mirrored) | 0 | inclusive (`strict = FALSE`) |

Input notation: a plain vector of **signed / reciprocal literals** (negatives for additive, `1/x` for multiplicative; a one-sided vector auto-mirrors, a two-sided one is used as-is, an `NA` skips an intensity slot), or a **`list(over =, under =)`** of magnitudes (no mirror; omit a side to switch it off). `mk_color_scale()` + `parse_color_side()` + `intensity_slots()` build the canonical shape; `get_color_breaks()` round-trips (a readable form; `type = "all"` gives the signed engine breaks). A per-table override: `tab(color_breaks = list(...))` stores a table attribute, installed transiently at render by `push_color_breaks()` / `pop_color_breaks()` (fallback to the global option when absent/malformed).

### Layer 3 — The vectorised `findInterval` engine (three axes)

Coloring is decomposed into three orthogonal per-column choices: **measure** (`diff`/`ratio`/`contrib`/`or`, in the `color` attribute `[1]` = text, `[2]` = background), **channel** (text vs background), and **significance policy** (the `color_signif` attribute: `ignore`/`grey_non_signif`/`guaranteed_effect`). All feed one engine in `R/fmt_class.R`:

1. `fmt_color_plan(x, channel, color, signif)` builds a plan: the measure is the stored `color` attribute (a CLEAN measure since Phase 17d — the legacy combined strings are decoded once at the boundary by `color_decode_legacy()`, never here) and the policy is the `signif` arg or the stored `color_signif`. It then reads the measure's ONE `MEASURES` row (Phase 17d — engine facts joined the legend facts) for: the scale keys for the measure×column-type (`scale = c(std=, pct=)` picked by `std_when`: `diff` → `pct_diff`/`mean_diff`, `ratio` → `pct_ratio`/`mean_ratio`, **`or` → `odds_ratio`** [Phase 16c], `contrib` → `contrib`), the per-cell `score` (`raw` getter: `get_diff`, standardized `get_diff / sqrt(get_ref_var)` for numeric diff; `get_ratio`; `get_ctr / get_mean_contrib`), the significance source (`sig_source`) and the row gate (`gate_row`). The `gate` (from the stored `get_ci_inf`/`get_ci_sup` bounds — EXCEPT `contrib`, which has no interval and instead reads the stored `get_pvalue()`: the standardized (Pearson) residual p-value written per cell by `chi2_write_contrib()` / `contrib_pvalue()` at chi2-time, where the grand total N is in hand; direction from the sign of `get_ctr / get_mean_contrib`, so `contrib` colours under `grey_non_signif` / `guaranteed_effect` instead of colouring nothing), and the per-side `over_breaks`/`over_slots` + `under_breaks`/`under_slots` (read straight from the scale).
2. `fmt_color_slots(x, plan)` folds `score` to a magnitude around `center`, then `findInterval()` **per direction** (`over_breaks` for over-cells, `under_breaks` for under-cells) → level → palette slot (0 = uncolored, **1..4 = over intensities, 5..8 = under**), zeroing ungated cells. The former in-text ×2 / slot-11 override is gone (the ×2 rule is now a 1-break `pct_ratio` scale on the background channel). **`guaranteed_effect` computes `score` = the guaranteed (CI-floor) magnitude ON THE MEASURE'S OWN SCALE** so the fold's `center` matches: for `diff` the floor is the stored difference bound (centre 0); for `or` the native OR bound (centre 1); for `ratio` — which has no native CI — the shared diff floor is converted to a guaranteed ratio `1 + (get_ratio - 1) * (guar_diff / get_diff)` (centre 1). Feeding the raw diff bound into a centre-1 fold was the "ratio floods /4" bug.
3. `fmt_color_channels(x)` → `list(text_slot, bg_slot)`, via `resolve_color_channel_plans(x)` — the shared arbiter (also used by `legend_specs()`, so cells and legend agree). Phase 16c: `fmt_color_plan()` flags a plan `degenerate` when the policy is `guaranteed_effect` and the scale has a single break per side (which `offset_guaranteed_breaks()` collapses to the neutral → one flat "×1" fill). The arbiter drops a degenerate channel and its legend line, but **never the last channel** (a degenerate text channel survives when no non-degenerate background does — "keep the first channel"). The single-channel golden `fmt_get_color_code()` is not arbitrated (it always renders the text measure).

Every consumer maps `(text_slot, bg_slot)` to colour the same way: `pillar_shaft.tabxplor_fmt()` (console, the reference two-channel consumer), `fmt_get_color_code()` (single-channel, the golden), the shared `fmt_channel_codes()` helper (text + bg hex, used by `tab_kable`/`tab_plot`/`tab_xl`), and `tab_color_legend()` (which reads the same scales, so legend and cells never disagree). The old combined strings (`"diff_ci"`/`"after_ci"`/`"ci"`) are decoded to `(measure = "diff", policy)` ONCE at the argument / storage boundary by `color_decode_legacy()` (in `normalize_color_spec()`, and in `tab_ci()` for the deprecated step path) — the engine never re-parses them, and `"ci"` folds into `after_ci` (the old `single0` one-shade mode is retired).

The `color`/`color_signif` **arguments** are parsed by `normalize_color_spec()` + `finalize_color_spec()` (`R/tab.R`), called by `tab()` and `tab_num()` (not yet `tab_many()` — Phase 6).

**Colour legend (Phase 13b, `R/fmt_class.R`).** `tab_color_legend(x, medium = c("console","html","md","runs","plain"), style = c("terse","prose"), lang=)` builds the legend as `legend_specs(x)` (per col_var group: measure/breaks/ref/method/policy/shade names + regression effect word) → `legend_tokens_terse`/`_prose` (a **token stream**: plain-text | coloured break-word tokens) → `legend_render_line(medium)` (console ANSI via `cli` / inline html span / md pandoc span / plain / **`"runs"`** = the token stream returned unrendered as `list(text, color, bold)`). Console = terse, exports = prose; the break-word colours come from the same 8-slot palette the cells use, so they can't disagree.

`"runs"` (Phase 14c) is the medium for consumers that draw the legend as coloured TEXT and cannot fill: `tab_xl()` (an `openxlsx2::fmt_txt` rich-text cell) and `tab_plot()` (`ggpubr::table_cell_font`). Both therefore draw a background break-word from the `bg_legend` palette. It replaced `"excel"`: `tab_plot()` used to recover the legend by scraping regexes back out of the *html* rendering, which had silently stopped matching (Phase 13b replaced kableExtra's `color: rgba(...)` spans with inline hex) — every token rendered as a raw html fragment in black.

**Phase 16d legend/footer changes.** `legend_specs()` now builds rich specs WITHOUT a `sig`, then (reg tables only) `legend_canonicalise_reg()` reconciles the empirical + model columns of each col_var — sharing one reference label and neutralising the model's additive effect word ("AME"/`β` → the neutral "cells", only when an empirical additive sibling exists, so a no-empirical table keeps its word) — and finally derives the `sig` **without `role`**, so an empirical companion folds into its model sibling under one legend line. `tab_color_legend()` shows a column-name prefix whenever a group is role-MIXED (an emp+model merge) or a col_var spans several lines; `legend_name_list()` normalises those names (undo the html-path `<br>`/U+202F wrap, protect intra-name spaces with U+00A0 so no medium re-breaks a name) and caps them at six + `… +N vars`. `contrib` is no longer lumped with `ratio` in `legend_break_label()` — it reads `×N` on BOTH sides ("×N the mean contribution"), direction by colour. A numeric-mean/coef diff carries `is_pct` + `is_std` (from the mean_diff scale's `std` flag, the same `color_scales()` source the cells read): factor pct → `×100`/"points", standardized → "SD", raw custom breaks → the bare value. Two new plain (uncoloured) footer helpers ride every footer site: `tab_weight_line()` ("Weighted by `<wt>`." — the weight NAME persisted in the `vars` attribute for a crosstab (only when weighted, so unweighted tables don't churn) / `reg_meta$wt` for a regression) OPENS the footer, and `tab_stars_legend()` (the significance-stars legend, gated by `fmt_stars_applicable()` = not a `contrib` residual-p column) CLOSES the colour block. (Phase 16e unified how these reach the backends — see below.)

Break-words are **bold in every medium** (Phase 14c): the runs carry `bold = TRUE`, the console composes `cli::style_bold`, html emits `font-weight:bold` **inline** and md wraps `**[+5]{.p1}**`. Inline/markup rather than left to `.p*`, because it must also hold on the background channel (whose `.o*`/`.u*` classes are deliberately unbolded, mirroring the cells) and on the kableExtra path (which ships no stylesheet of ours). Distinct from `tx_css_rules()`'s `.p1..m4{font-weight:bold;}`, which exists for the **cells**: `tab_export_prep()`'s `bold = !is.na(text_hex) | ref_alltot` already bolds every text-coloured cell in kableExtra and the html engine, and the stylesheet is the only way `tab_md()`'s bare `[42%]{.p2}` spans can say it. The prose is **translatable** via `gettext`/`gettextf` (domain `R-tabxplor`, French in `po/R-fr.po` → compiled `inst/po/fr/LC_MESSAGES/R-tabxplor.mo`); `lang` (`"en"`/`"fr"`, or auto from the R/OS locale) sets the `LANGUAGE` env for the build. The CI method + confidence level are named from the table's stored **`ci_settings`** attribute (`list(conf_level, method_cell, method_diff, method_ratio, method_mean_diff, method_mean_ratio)` since Phase 14v-ii — the legend picks the relevant method off the column's type/`ci_type`: mean-diff → Welch/Student, mean-ratio → robust/quasi/naive Poisson, proportion ratio → Katz; set in `tab_assemble_tables()`, carried through dplyr like `subtext`/`test`/`render_extras`; `default_ci_settings()` fallback). Shade names ("blue"/"yellow-red") appear only for the default palette (`legend_shade_names()`); a custom palette degrades to generic wording. Callers: console print (`tbl_format_footer`), `tab_kable`/`tab_md`/`tab_xl`/`tab_plot` (each with a `lang` arg; Excel writes coloured rich-text legend cells via `xlb_write_richtext`).

**Phase 16e legend/footer unification.** The legend engine is now data-driven: a static `MEASURES` table holds each measure's language-invariant facts (word / break glyph per side / reference kind / unit / whether the reference leads the sentence), `legend_reg_adapter()` (renamed from `legend_canonicalise_reg`) folds a reg table's facts into plain spec fields, and `legend_resolve_spec()` resolves every per-channel fact into the spec ONCE — so `legend_tokens_terse`/`_prose` are **dumb templates** with no `switch(measure)` and no `is_reg` branch. Grouping is by the **rendered body** (`legend_group_by_body()`: two columns share a line iff they render identically), replacing the hand-maintained `sig` string, so a legend line can never drift from what it describes (the 16d `is_pct`-in-sig patch is subsumed). The whole below-table footer is ONE ordered model: `tab_footer_streams(x, style, subtext, legend)` returns typed token-streams (weight → `Model:` → colour-legend group(s) → stars → user subtext; the plain one-liners wrap as single-`.lg_tok` streams — no plain-vs-legend kind split, since `legend_render_line()` already renders uncoloured tokens), and `render_footer(streams, medium)` renders + joins per medium (console applies the `# ` subtle prefix, role-aware: a legend keeps its colours). Every backend — console `tbl_format_footer`, `tab_md`, `tab_kable`, `tab_xl`, `tab_plot` — calls these two, replacing the previous 5× hand re-ordering + the `tab-export-prep` `reg_line`/`weight_line`/`stars_legend` fields (deleted; only `reg_title`, the caption, remains). `tab_plot()` gained footer parity (it drew only the colour legend before, silently dropping weight/`Model:`/stars/subtext) and now also draws its `caption`. `legend_export_style()` reads `options(tabxplor.legend_style = "terse")` to switch exports (md/html/Excel/plot) to the compact console legend form; the console is always terse.

## Export System

Four export formats, all in separate files, plus one facade.

**Unified facade (`R/tab-export.R`, Phase 10j-A).** `tab_export(x, format = c("kable","md","xl","plot"),
path=, ...)` dispatches to the four exporters (mirrors `jmvtab_export`); the four functions stay exported
and idiomatic. A shared **`resolve_export_opts()`** (`R/tab-export-prep.R`) resolves the canonical display
options ONCE (`theme`/`color`/`color_legend`/`transpose`/`caption`/`var_names`; `color_type`/`html_24_bit`
are deprecated + inert since Phase 14l), so every
exporter shares one set of names and defaults: `color = FALSE` renders monochrome, `transpose = TRUE`
flips the FINISHED render model (Phase 14o — `tx_transpose_render()` in `R/tab-transpose-render.R`,
after materialise; the old object-level `tab_transpose()` is soft-deprecated), `caption` is the single
caption name (`tab_md(title)` / `tab_xl(print_color_legend)` are soft-deprecated aliases). `tab_xl` is
now theme-aware.

**Render-level transpose (Phase 14o, `R/tab-transpose-render.R`).** A transposed column is
heterogeneous (a `%`, a mean, an `n`), so it cannot be a `tabxplor_fmt` column and cannot be
`format()`ted — which is why the object-level flip mis-coloured numeric cells. `tx_transpose_render(rd,
backend, meta)` instead flips the finished `prep_one_table()` model: cell strings + colour slots +
tooltips are computed per (correct, homogeneous) source column, collected into matrices and transposed
as plain data, producing a SYNTHETIC model whose `$tab` is a plain **character** tibble (`$transposed =
TRUE`, `$cells` the pre-formatted strings, `$color_src` the original fmt table for the legend) with
`roles`/`ann`/`col_var_header`/`label_runs` all flipped. Because every backend already falls back to
`as.character()` for a non-fmt column and reads colours from `roles`/`ann`, **md and plot need no
branch**; **html** injects `rd$cells` + the pre-built `rd$tooltips`; **Excel** writes the display TEXT
(a `transposed` flag routes `xl_materialize_data`/numFmt/`is_refcol`/`get_col_var` around the absent fmt
columns — editable numbers deferred). `tab_export_prep()` materialises **xl-style when transposing**
(`n` a COLUMN → an `n` ROW; 14n has collapsed the Total rows → one Total column), superseding 14d's
transpose-before-materialise. New leading `[variable-name, levels]` columns mirror `(row_var, levels)`;
a real `tab_vars` table aborts. **Phase 17g**: the synthetic model now carries `reg_title` / `caption` /
`empirical_tips` through the flip (they describe the source table, not the axes), so a transposed
regression table keeps its title/caption/tooltips (previously dropped); the total-block border formula
is shared with `prep_one_table()` via `roles_totblock_edges()`.

### Shared exporter prep + render-model (`R/tab-export-prep.R`, Phase 10d)

`tab_export_prep(tabs, backend, compact, drop_tab_vars, wrap, compute, ...)` computes ONCE the
derive-once quantities every text exporter used to re-derive per render, and returns an ephemeral
`tabxplor_render` (a class-tagged list — NOT tab attributes, which dplyr desyncs). `tab_kable`,
`tab_md` and `tab_plot` are now `prep <- tab_export_prep(...); render from prep$tables[[1]]`. It
factors: block A (list → `tab_check_same_col_vars()` + the existing `tab_compact()`), block B (degrade
via `tab_render_vars()`), role detection (`fmt_cols`/`other_cols`/`totcols`/`totrows`/`col_var_map`/
`new_group`/`row_var_col`/`align`), the per-column `ann` sidecar (reference masks via `get_reference`,
two-channel colour slots+codes via `fmt_col_ann()`→`fmt_channel_codes()`; `compute` gates only the
hex-mapping COST — Phase 10j: `fmt_col_ann()` ALWAYS returns the full structure, `want_colors=FALSE`
[`color=FALSE`] yielding a monochrome column [zero slots, grey font] so every backend, incl. `tab_xl`,
reads a consistent shape), and the bold-row set (`tab_bold_rows()`). The derive-once win — `get_reference` computed once and passed to
`format(.ref=)` (not 4×/column), `fmt_channel_codes` once — lives in `ann`. Medium-specific quirks stay
LOCAL to each exporter (md keeps+blanks tab_vars and uses `str_trunc` → `drop_tab_vars=FALSE`,
`wrap=NULL`; kable's knitr `*`-escape + `row_spec`/`column_spec`; plot's ggpubr render; the divergent
`new_col_var` transition index). `tab_totcol_range()` (the `[min;max]` total-column base pre-pass, §10)
is built and populated into the model but INERT until consumed (10e/10f). Byte-identical to the
pre-Phase-10d exporters (golden/color-golden/md-snapshot/A/B locked). `tab_plot()` is soft-deprecated
(`lifecycle` superseded) here.

**Phase 14i** adds the shared **variable-NAME model**, and moves both of its drops into the prep so no backend has to know the argument exists. `roles$label_cols` + `roles$label_runs` (from `tab_label_runs()`, per column `list(show, span)`) are the leading factor columns whose value repeats down a block — the synthetic `row_var` column when `compacted`, else the kept `tab_vars`, never both (`tab_compact()` bails on tab_vars). ONE run model, four consumers: md blanks the repeats, the html engine `rowspan`s the run, Excel merges it (`xlb_merge`, and blanks the written repeats — Excel keeps only a merge's top-left value), `tab_plot` blanks. `roles$var_name_col` is the name-VALUED subset (the merged table's `row_var` column, whose values ARE variable names): it alone is dropped by `var_names`, has its literal `"row_var"` header blanked unconditionally in `tab_col_var_header()` (a bug fix), renders **vertically** (`.tx-vname` = `writing-mode:vertical-rl` + `rotate(180deg)`, NOT the experimental `sideways-lr`; Excel `text_rotation = 90` + a narrow column), is **italic** in md and never bold. A tab_var's values are LEVELS: merged and blanked, never dropped, never rotated. The new shared **`var_names`** (`"both"`/`"rows"`/`"cols"`/`"none"`, `options("tabxplor.var_names")`, resolved by `resolve_export_opts()`) is two lines in `prep_one_table()`: dropping the `var_name_col` column, and blanking `col_var_header$label` — which every backend already gates its span row on (`any(nzchar(label))`), so the col side needs no backend code (it is what let `tab_md(col_var_names=)` be deprecated by deleting its gate). It never touches a LEVEL column's header (`marital`, a kept `year`): that header identifies the column and costs no width. Runs come from the VALUES, not the grouping (`new_group` is the full group COMBINATION for ≥2 tab_vars, so an outer tab_var's run would be cut); NA = a continuation (a p-value row belongs to the block above); nested outer→inner. ⚠ md's bold exclusion must reach the WIDTH pass (`bold_rows_of()`) — `md_extra()` and the `+4` charge markup width per column.

**Phase 14n** collapses the redundant per-block `Total` rows of a compacted several-`row_vars` table, DISPLAY-ONLY, as the final step of `tab_materialize_extras()` (so it reaches the console + every export uniformly, and all roles recompute on the collapsed table with no per-backend code). `tab_collapse_total_rows()` guards on `get_vars_attr()$compacted` + `>= 2` Total rows (a single-row_var or a tab_vars table is never compacted → untouched). It compares each block's whole **total block** (the Total row + its trailing `"n"`/`"row_pct"` summary rows — NOT the block-specific `"pvalue"` row) "as displayed" via `format()` over every fmt column (one canonical predicate across backends); identical → drop all but the last block's total block, different (only `na = "drop"` can make them differ) → keep all + a once-per-session message. Comparing the whole block, not just the Total row, is what makes it correct under `pct = "col"` (where the Total is always `"100%"` and the base lives in the `n` row). Alongside it, `tab_pvalue_lines()` now keys the p-value rows on the table's **grouping columns** ∩ the `test` tibble (the synthetic `row_var` for a compacted table, `tab_vars` otherwise — byte-identical there), so a compacted table gets one p-value row PER block instead of a single mis-keyed row, and carries the `vars` attribute through its rebuild (a Phase 14d gap the collapse guard exposed). Both changes are display-only: the core `tab()` object keeps every Total row.

**Phase 17g (export-stack integration).** The render model is now the one intermediate representation every backend consumes. (1) The **declarative materializer**: `tab_materialize_extras()` seeds the row roles then delegates to `tab_materialize(tab, backend, ctx)`, which runs the applicable specs from `materialize_specs()` — a DECLARED inventory of the synthetic extras (`add_n_pct` / `or_total` / `sd_twin` / `footer` / `collapse_totals`), each a `list(kind, when, apply)` with a per-backend policy — replacing the old imperative if/else passes. The two build-then-undo cycles are gone: the add_n `n` COLUMN is built for **xl only** (`tab_add_n_pct(..., backend =)`; text folds the base into the Total cell directly from its own `n` field via `tab_fold_addn_incell`, no throwaway column), and `collapse_totals` is a declared display slice reading the stored roles. `mat_add_n_pct` / `mat_sd_twin` are the two extracted apply helpers; `xl_materialize_data()` (in `tab_xl.R`) is the xl value-string-vs-number policy at write time. (2) **Shared footer/caption helpers** (`R/tab-export-prep.R`): `rd_footer(src, medium, theme, want_legend, subtext, lang, classes)` folds the `render_footer(tab_footer_streams(...))` sandwich every backend repeated; `rd_caption(rd, user_caption)` folds the `user → set_caption() → reg_title` fallback (md/kable/plot; xl keeps its own variant with the extra named-tabs / `tab_get_titles` tail). (3) **Single-sourced slot→hex**: `tab_xl()` consumes `ann$text_hex`/`ann$bg_hex` directly (dropping its private `text_pal`/`bg_pal` palette), so the theme-resolved hex flows only through `fmt_channel_codes()` — the same source the CSS side reads. (4) **md header** groups its spanning col_var-name row by the shared `tab_header_runs()` RLE (pandoc still can't colspan, so the width-padded per-column blanks stay md-local). (5) **`roles_totblock_edges(in_block)`** single-sources the total-block top/bottom border formula shared by `prep_one_table()` and `tx_transpose_render()` (the rest of the two role models are different computations — fmt-based vs flipped-positional — so they are not merged). (6) The **`output_kable` render moved** to `tab()`'s tail (post-`finalize_color_spec`), fixing the two-channel-colour crash. (7) `print.tabxplor_kable` **degrades gracefully** when kableExtra is absent (`kable_print_mode()` predicate → a one-time note + knitr print, no broken dispatch). (8) `tab_xl()` **dropped** the long-inert `n_min` / `hide_near_zero` / `conditional_format` args.

**Phase 13c** adds a shared **col_var HEADER model** to the render-model: `tab_col_var_header(tab, roles)`
returns per column a spanning `label` (the col_var NAME, blank over the row var / total / count columns)
+ a `clean` level label (the `_<col_var>` disambiguation suffix stripped, e.g. `Other_race`→`Other`);
`tab_header_runs(label)` run-length-encodes it into (label, span) cells. Every exporter renders a
spanning variable-name row above the level names from this ONE model: md a centred visual row (single
col_var too), kableExtra `add_header_above` + `col.names=clean`, the html engine a `<thead>` colspan row,
Excel a merged span row (`xlb_merge`) with a one-row geometry shift (`span_off` in `tab_xl_plan_one`,
`+6` stacking). Also 13c: **composite `{}` displays** are token-padded + partial-bolded (bold only the
first field in a total/reference row) via `format(bold_split=TRUE)`→`primary_nchar` (md `md_bold`, html
`html_cell_text`); the multi-table LIST return is now a **`tabxplor_tabs`** S3 class (auto-print + Viewer);
`tab_xl` writes ci="cell"/OR as text columns (`xl_materialize_data`, `or_numeric`), adds a mean `_sd`
twin column, and signs (`+`/`x`/`sigma`) in the numFmt.

### Render-time variable detection + graceful degrade (Phase 10c)

`tab_render_vars()` (`R/tab.R`) is the robust, position-independent role detector used by the print
methods and every exporter (and, from Phase 10d, by the shared export prep). It keeps the
`col_var`-attribute path for `col_vars` but places `row_var`/`tab_vars` from `dplyr::group_vars()`
(which survives `rename`/`select`/`relocate` — `tab_build()` groups by `tab_vars`, `tab_compact()` by
the literal `"row_var"` column), so a factor moved after the value columns is no longer miswritten.
`row_var` = the last factor NOT in the groups. When the object can't be read as a tabxplor table (not
a data frame / no `tabxplor_fmt` columns / no factor variable) it returns
`list(degrade = TRUE, reason = ...)`; each exporter then renders the plain frame (`tab_degrade_inform()`
+ a plain kable / pipe table / raw sheet) instead of crashing in role detection. It is byte-identical
to `tab_get_vars()` on every well-formed table. New `tests/testthat/test-edge-cases.R` locks this.

### The `format.tabxplor_fmt()` display method (`R/fmt_class.R`)

The single source of truth for the console (`pillar_shaft`), `tab_kable`, `tab_md`, `tab_plot`.
Phase 10c reworked it for speed and flexibility (byte-identical, golden-locked):

- `get_reference()` rewritten from `dplyr::case_when` (3 outer branches × `switch(mode)`) to direct
  base-R boolean composition of the per-cell masks (the branch selectors are all scalar column
  attributes) — no per-arm `rep(FALSE)`/`DataMask` allocation.
- `format()` / `pillar_shaft` accept a precomputed `.ref = list(cells =, all_totals =)` so the
  reference masks are derived ONCE (was up to 4× per column); when `NULL` they are memoized
  internally. The exporter prep (Phase 10d) passes them straight in.
- The unconditional `x$var` (`$.tabxplor_fmt` → `dplyr::pull`) accessor was replaced by `get_var(x)`
  (the `vctrs::field` accessor) — it was ~28 % of `format()` self-time. Overall `format()` is ~2×
  faster on the exporter path (`dev/benchmarks/results_1.4.0/phase10c_profile.txt`).
- Opt-in COMPOSITE display via a per-cell `display`-FIELD `{}` template (e.g. `"{pct} (n={n})"`,
  Phase 10i-A) — parsed only here, gated by one fixed `grepl` so the common path is byte-identical when
  no cell is a composite. `get_num()`/`format()`/`vec_ptype_abbr`/tooltip resolve a composite cell to its
  PRIMARY (first `{field}`) via the shared `display_primary()`; Excel exports the primary automatically.
  `{}`-only (no curated sugar) via `validate_display_template()`; e.g. `tab(display = "{pct} (n={n})")`.
  The internal `pct_ci`/`mean_ci`/`or_pct` tokens are pipeline-set integrated-rendering modes, not `{}`.
  (Replaced the dropped Phase-10c `display_spec` attribute; 9 attributes.)

### tab_xl() — Excel Export (`R/tab_xl.R` + `R/tab-xl-backend.R`, Phase 10h)

Exports to `.xlsx` via **openxlsx2** (Suggests-only; the ONE `requireNamespace()` guard is in `tab_xl()`).
Single-tab-first with a list method. Pipeline:

- `tab_xl()` (orchestrator) — deprecations (incl. `print_color_legend`→`color_legend`),
  `resolve_export_opts()`, degrade, `tab_export_prep(backend="xl", compact=FALSE,
  compute=c("refs","bold","colors"), transpose=)` (the prep materialises the display extras and, when
  `transpose=`, flips the render model — Phase 14o; the plan builder takes a `transposed` flag and
  writes the flipped char grid as coloured TEXT), sheet assignment
  (`sheets="auto"/"tabs"/"unique"`) + stacking offsets, then builds per-table **plans** (serial
  `purrr::pmap`) and assembles ONE workbook, writing each plan with `xl_write_table()`. Colours come
  from the shared prep `ann` (Phase 10j — the private `fmt_color_channels()` pass is gone); Phase 17g:
  xl consumes `ann$text_hex`/`ann$bg_hex` DIRECTLY (its private slot→hex palette is gone), so the
  theme-resolved hex flows only through `fmt_channel_codes()` — the same source the CSS side reads. (No
  `parallel=`: the openxlsx2 write dominates and is serial, so parallelising the plan build was measured
  not worth it.)
- `tab_xl_plan_one()` — pure per-table plan: the raw `get_num()` frame to write, the per-cell Excel
  **numFmt codes** from `format(x, syntax="excel")` (stars folded into the literal `0.0%"***"` when
  `getOption("tabxplor.stars")`), and a **precomposed per-cell style grid** via `xl_build_styles()` —
  each cell's full style (font+fill+border+alignment; borders painted onto 4 side matrices, alignment
  onto zone matrices), grouped into the fewest DISTINCT styles with a coalesced multi-area `dims`.
- `xl_write_table()` — the per-sheet writer: writes the raw values, then `xl_apply_styles()` registers
  each distinct style ONCE (deduped fonts/fills/borders + a composed cell xf) and applies it by id with
  `set_cell_style` over its coalesced `dims` (`xl_coalesce`) — far fewer + cheaper openxlsx2 calls than
  a `wb_add_*` per aspect (the "shared styles" fast path, ~1.4–1.8× faster). numFmt is the one exception,
  a grouped `wb_add_numfmt` pass that merges onto the composed xf.

**Export parity** (unchanged from 10g): the raw `get_num()` value is written and Excel formats it via the
`format(syntax="excel")` code — the same display source of truth as every other backend (verified by
`test-export-parity.R` + the numFmt-code lock). NOT byte-identical to the old openxlsx workbook (waived).
`conditional_format=` is accepted but experimental (message + hard-style fallback). `hide_near_zero` /
`n_min` are accepted-but-inert (use `tab(n_min=)`). `R/tab-xl-backend.R` documents the openxlsx2 style
model (merge across aspects; replace within; the font `update` bug; borders reject multi-area `dims`).

### tab_kable() — HTML Export (`R/tab_classes.R` + `R/tab-render-html.R`)

`tab_kable()` = `tab_export_prep(list_method=TRUE)` → map the `render_kable_html()` **engine seam**
(`R/tab-render-html.R`, Phase 10e) over the prepared tables → `tab_kable_join()`. The `engine` argument
(default `getOption("tabxplor.tab_kable_engine")`, **`"html"` since Phase 14e**) picks the backend, both
driven by the same `tabxplor_render` model so they cannot diverge in content:

- **`"html"` (the default)** — a dependency-free, self-contained `<table>` + one stylesheet, vectorised
  assembly. ~3x faster / ~6x lighter; emits the same bootstrap tooltip attributes; used by the jamovi
  live display (`tab_render_scrollbox()` replaces `kableExtra::scroll_box`).
- **`"kableExtra"` (legacy)** — the `knitr::kable` + `kable_classic`/`row_spec`/`column_spec` pipeline
  (byte-identical to pre-10e). Colour via inline CSS, HTML tooltips/popover, `inst/tab.css` injection.
  It bakes its theme at render time, so it cannot follow `theme = "auto"` (which downgrades to light).
  Since the "Last Phase b" dependency review `kableExtra` is **Suggests-only** (the default engine
  needs no external HTML library); this path is guarded (`render_kable_html()` dispatch +
  `kable_tabxplor_style()`) and aborts with a pointer to `engine = "html"` if the package is absent.

A list renders table-after-table (both engines; Phase 14d: it is never merged).

**The html engine emits NO inline styles** (Phase 14e). Every look — geometry included — is a **role
class** resolved by `tab_css()`: `tx-r`/`tx-l` (align), `tx-num` (numbers: nowrap + the number font --
proportional DejaVu Sans by default, but a MONOSPACE stack when the table shows stars, via the
`tx-has-stars` class on the `<table>`; Phase 14m-ii, `options("tabxplor.tab_kable_num_font" / "_stars")`),
`tx-br`/`tx-bl` (borders), `tx-b` (bold), `tx-bt`/`tx-bb`/`tx-bb2` (row rules), `tx-span` (the col_var
header), `tx-pill` (a background), `tx-lbl`/`tx-vname` (a variable name spanning its block, Phase 14i),
`tx-foot` (the footnote, Phase 14j), plus the colour slots (`.p*`/`.m*` on the `<td>`, `.o*`/`.u*` on
the pill span). `tx-tot`/`tx-rv` are emitted with **no rule of their own** since 14j: their min-widths
were deleted (the browser content-sizes every column, so a floor could only be too big), and they
remain as the hooks a user pins a width on — `?tab_css` "Restyling a table". Three reasons, in order of
weight:

1. **an inline style cannot be overridden by a user's CSS**, so "a good default you can restyle" (what
   kableExtra gives) was impossible while the engine wrote its own borders and widths;
2. it is **half** of the coloured-border fix. `border-right:1px solid` is a **shorthand** — it resets
   `border-color` to `currentColor`, i.e. the cell's own palette colour, so a `+20%` cell drew a blue
   border; inline, it also beat the stylesheet's `border-color` rule. Moving it into a class removed
   the *inline* precedence only: a class still out-specifies `td{border-color:…}`, so the shorthand
   kept winning and 14e recorded the bug as fixed while it was not. **Phase 14j finished it**: the
   stylesheet now uses `border-*-style`/`border-*-width` longhands exclusively, so nothing but the one
   `border-color` rule ever names a border colour (locked by test-render-html.R);
3. the markup shrinks (one short class vs a repeated style string per cell).

This extends Phase 13d's rule (colour must be a class, or `theme = "auto"` is impossible) to
everything else. Consequence: with `css = FALSE` **and** no `tab_css()` in the document, the table now
renders unstyled, not merely uncoloured.

Two engine-specific details worth knowing: a **background colour is a pill** (`<span class="tx-pill
o3">`) hugging the text rather than a full-cell fill — a flood reads as a blocky grid *and* swallows the
row hover (a child's background always paints over its row's, whatever the specificity); and header
labels go through **`html_escape_br()`**, which escapes and then restores the one tag the package itself
injects (`tab_wrap_text()` wraps long header names on `<br>`), so a `<` in a user's own level name is
still escaped. kableExtra never needed this — `knitr::kable(escape = FALSE)` passes col.names through.

**The Viewer page (Phase 14k, `print.tabxplor_kable`).** `theme = "auto"` means *follow the reader,
resolved by whoever can actually know*. For a file or a knit that is the browser (the 13d cascade,
untouched). For an interactive **Viewer** print it is not: the Viewer is an Electron webview whose
`@media (prefers-color-scheme)` reports the **operating system**, not the editor's colour theme — so the
table could not see the pane it sits in. `print.tabxplor_kable()` therefore resolves in R
(`tx_detect_theme()`), and says so by wrapping the table in `<div data-theme="light|dark">`: our page
becomes an explicit host toggle, and cascade layers 3/4 beat the `@media` layer in both directions. No
fifth layer, no second stylesheet. It also paints the page itself (`tx_page_style()` →
`html,body{background;color}`), which is what stops a dark table sitting in `html_print()`'s white pane;
being body-level, it wins on source order over both `save_html()`'s own `body{}` and bootstrap's, with
no `!important`. Everything else — a non-interactive print, a knit (`knit_print` is deliberately **not**
overridden), or a table we did not style — falls through to `print.kableExtra`, byte for byte.

The gate is one rule: **tabxplor paints a page only when tabxplor's own stylesheet ships with the
table** (`tab_kable_join()` attaches the `tabxplor_theme` attribute only for `engine = "html"` and
`nzchar(css)`; no attribute ⇒ print does nothing new). It is the same discriminator the colour legend
uses, and it is what keeps `css = FALSE` (an unstyled black-on-white table) and the kableExtra engine
(which bakes its own `#363640` dark table) from being painted into unreadability. `tx_page_style()` has
exactly two callers — this print method and `tab_html_string(standalone = TRUE)`, the other page the
package builds; the latter passes the *intent*, so `"auto"` keeps its `@media` cascade (that file is
opened elsewhere). **No `vscode-*` hooks**: the Viewer is a cross-origin webview iframe, so
`body.vscode-dark` sits on the outer workbench body and no selector of ours could ever reach it.

Tooltips (`tab_kable_print_tooltip()`) are `any()`-gated so each field's `format()` runs only when the
column has it. NA cells render blank at source (`format.tabxplor_fmt(na="")`), not via a post-hoc regex.

### tab_md() — Markdown Export (`R/tab_md.R`)

Lightweight standalone export (new in v1.3.1):

- Monospace-precise column alignment with pipe tables
- Bold formatting for total/reference rows
- Handles multi-table lists and compact tables
- Can copy to clipboard or write to file

**The output must be VALID PANDOC — and nothing checked that until Phase 14f**, so it was not: the
col_var name row sat above the level header, i.e. a **two-row header**, which pipe tables do not have.
Pandoc gave up on the whole table and emitted a line-block plus a paragraph of pipes. Every table
carrying a col_var name (every normal one) was affected, invisibly, because every test asserted on the
markdown *string*. `test-tab_md.R` now renders through pandoc (`skip_if` pandoc is absent). Three rules
follow, and they are the ones to keep:

- the col_var name is a **body row** (italic, first cell of its group, **one cell per column** — a
  merged cell desyncs the row's count and pandoc shifts the data); `col_var_names = FALSE` drops it;
- the thin spacer column between col_vars needs `-` on the **delimiter** row (`md_insert_col_sep(fill=)`;
  one helper builds all four row types, so the fill is a parameter);
- a `|` in a label is escaped (label columns only — fmt cells are package-formatted numbers).

**Padding aligns the VISIBLE end, not the raw one.** Markup (`[`, `**`) occupies raw columns but
vanishes when rendered, so each cell pads by `nchar(text) + md_extra()` — the markup that PRECEDES its
last visible character: 0 plain, 2 for a whole-cell bold (its closing `**` follows the value), **4 for
a composite bold** (`**100%** (n=…)`: both pairs precede the `)`). The markup then grows leftwards into
the pad and every number shares a raw column. Before, the bold rows' `+4` entered `num_width`, which
pads *inside* the bracket (`[    38%]{.p2}`) — spaces pandoc discards, and which pushed the number the
wrong way. The attr is padded to `attr_width` so `}` lines up (pandoc reads `{.m2  }` as `{.m2}`).

**`css = TRUE` wraps each table in a pandoc fenced div** `::: {.tabxplor-tab}` → `<div
class="tabxplor-tab">`. Pandoc emits a BARE `<table>` for a pipe table, which no `tab_css()` rule could
reach, so a rendered md table got the colours but none of the layout; the div is the hook every
existing selector already matches (hence `.tabxplor-tab table` in the border-collapse rule: the class
is the table itself in html, a wrapping div here).

### tab_plot() — ggplot Visualization (`R/tab_classes.R`)

Creates a `ggpubr::ggtexttable` from a tabxplor table (soft-deprecated / superseded — kept, not invested
in). Consumes `tab_export_prep(backend="plot")` like the others.

- Uses `ggpubr` and `cowplot` for layout; two-channel colours from the prep `ann` (font + cell fill)
- Phase 10j: a non-mergeable list renders as a **list of ggplots** (a per-element recursion at the top
  of `tab_plot()`, before the prep) instead of erroring; a single tab / mergeable list → one plot
- `color = FALSE` renders monochrome; `transpose = TRUE` transposes at export

## dplyr Integration

tabxplor provides 30+ S3 methods to ensure tables survive dplyr operations. This is the most maintenance-intensive part of the package.

### The Core Trio

Three methods form the backbone of class preservation for `tabxplor_grouped_tab`:

1. **`dplyr_row_slice()`**: Called when rows are filtered/sliced. Calls `NextMethod()`, then re-wraps with `new_tab()` or `new_grouped_tab()`.
2. **`dplyr_col_modify()`**: Called when columns are added/modified. Same re-wrapping logic.
3. **`dplyr_reconstruct()`**: Called to reconstruct the object after operations. Same pattern.

Each checks `lv1_group_vars()`: if only one grouping level remains, downgrades to plain `tabxplor_tab` (no longer grouped). Otherwise, preserves `tabxplor_grouped_tab`.

### Method List

Every dplyr verb that a user might call needs an S3 method:

- **Grouping:** `group_by`, `ungroup`, `rowwise`
- **Selection:** `select`, `relocate`, `rename`, `rename_with`
- **Filtering:** `arrange` (note: for `tabxplor_tab`, not grouped)
- **Mutation:** `mutate`, `summarise`
- **Internal:** `dplyr_row_slice`, `dplyr_col_modify`, `dplyr_reconstruct`

**If a method is missing**, the operation silently drops the `tabxplor_*` class, reverting to a plain `tbl_df`. This causes loss of `subtext`, `test` attributes and breaks colored printing. Always check `NAMESPACE` for the current method list.

### The mutate.tabxplor_fmt Method

A special `mutate()` method exists for the `fmt` class itself (not the table). This allows users to modify individual fields within `fmt` vectors using dplyr syntax:

```r
tab |> mutate(across(where(is_fmt), ~mutate(., pct = pct * 2)))
```

## Options System

All options are set in `.onLoad()` in `R/utils.R`. Users can override via `options()`. The
user-facing, always-current list is the `?tabxplor-options` help page (`R/tabxplor-options.R`); the
table below is a dev subset and lags it (e.g. it still lists the removed `color_style_type` /
`color_html_24_bit` / `compact`).

| Option | Default | Description |
| ------ | ------- | ----------- |
| `tabxplor.totcol_range` | `"off"` | Total-column in-cell base when col_var bases differ: off/range/min |
| `tabxplor.color_style_type` | `"text"` | Color type: "text" or "bg" |
| `tabxplor.color_style_theme` | auto-detect | "light" or "dark" (detects RStudio theme) |
| `tabxplor.color_html_24_bit` | `"no"` | "green_red", "blue_red", or "no" |
| `tabxplor.color_breaks` | (see Layer 2) | List of break vectors |
| `tabxplor.print` | `"console"` | "console" or "kable" |
| `tabxplor.ci_print` | `"ci"` | "ci" (brackets) or "moe" (±margin) |
| `tabxplor.compact` | `FALSE` | Compact table output by default |
| `tabxplor.cleannames` | `FALSE` | Clean factor names by default |
| `tabxplor.export_dir` | `NULL` | Default directory for tab_xl() export |
| `tabxplor.output_kable` | `FALSE` | Auto-output as kable |
| `tabxplor.kable_html_font` | DejaVu Sans | Font for the kableExtra engine (legacy) |
| `tabxplor.tab_kable_num_font` / `_stars` | DejaVu Sans / Cascadia stack | html `.tx-num` font: no-stars / with-stars (14m-ii) |
| `tabxplor.xl_font_text` | DejaVu Sans Condensed | Excel text font |
| `tabxplor.xl_font_num` / `_stars` | DejaVu Sans / Cascadia Mono | Excel number font: no-stars / with-stars (14m-ii) |
| `tabxplor.plot_num_font` | `"Cascadia Mono"` | tab_plot number font, applied only when stars (`""` = default) |
| `tabxplor.kable_popover` | `FALSE` | Show CI as HTML tooltip |
| `tabxplor.always_add_css_in_tab_kable` | `TRUE` | Inject custom CSS in kable |
| `tabxplor.tab_kable_engine` | `"html"` | `tab_kable()` engine (`"html"` since 14e / `"kableExtra"`) |

## File-by-File Guide

### R/fmt_class.R (3341 lines)

The foundation file. Contains:

- **Lines 1–940**: Public API for `fmt`: constructor `fmt()`, getters (`get_num`, `get_type`, `get_color`, `is_totrow`, `is_refrow`, etc.), setters (`set_num`, `set_type`, `set_display`, `as_totrow`, etc.).
- **Lines 941–1040**: Internal constructor `new_fmt()` and helper `fmt0()`.
- **Lines 1040–1340**: Internal field accessors via `fmt_field_factory()`, reference detection (`get_reference()`).
- **Lines 1340–1630**: `format.tabxplor_fmt()` — the central display method handling 20+ display modes.
- **Lines 1630–1870**: `pillar_shaft.tabxplor_fmt()` — console color rendering, `mutate.tabxplor_fmt()`.
- **`fmt_color_plan()` / `fmt_color_slots()` / `fmt_color_channels()` / `fmt_channel_codes()`** — the vectorised `findInterval` color engine + the shared exporter slot→hex helper (Layer 3 above); `fmt_color_plan()` reads the measure's engine facts from the `MEASURES` fact table (Phase 17d); the legacy strings are decoded once at the boundary by `color_decode_legacy()` (R/tab.R), not here; the level→intensity map lives with the scale (`mk_color_scale()` → `intensity_slots()`).
- **`tab_color_legend()`** — the color legend, driven by the same per-channel plan + canonical scales as the cells.
- **Lines 2670–2900**: `get_reference()` — identifies reference cells (totals, first row, or regex match).
- **Lines 2900–3341**: vctrs arithmetic (`vec_arith`), casting (`vec_cast`), type compatibility (`vec_ptype2`), comparison/equality proxies.

### R/tab.R (5809 lines)

The main API file. Contains:

- **Lines 1–280**: `tab()` roxygen documentation.
- **Lines 280–390**: `tab()` function body — argument processing, delegation to `tab_many()`.
- **Lines 390–1520**: `tab_many()` — the full-featured engine with vectorisation, per-row_var loop, pipeline chaining.
- **Lines 1520–1770**: `tab_spread()`, `tab_get_vars()`, `tab_get_wrapped_dimensions()`.
- **Lines 1770–1860**: `tab_prepare()` — data cleaning, NA handling, rare level collapsing.
- `tab_plain()` (factor) / `tab_num()` (numeric) — each a Phase 17f wrapper + `plain_resolve`/`num_resolve`
  + `plain_core`/`num_core` (the data.table aggregation core, total rows/cols, reference, fmt wrapping;
  numeric = moment sums + roll-ups). Shared tails `leaf_totrow_tottab()`/`leaf_rename_totals()`;
  `df=`/`num=` extract via `leaf_extract_raw()`.
- `tab_ci()` — confidence interval calculation (Wilson/Wald/AC methods).
- `tab_chi2()` — chi-squared (factors) + ANOVA F (means) via the vectorised engine
  `agg_chi2()` / `agg_anova()` in `R/tab-agg.R`; contributions to variance for `color="contrib"`.
- `tab_pct()`, `tab_tot()`, `tab_totaltab()` + `pct_formula()`/`diff_formula()` — the superseded
  dplyr-era step API, quarantined in **`R/tab-steps-legacy.R`** (Phase 17f), off the build path.
  Internal helpers `diff_index()`/`calculate_refrows()`/`quo_miss_na_null_empty_no()` stay in `R/tab.R`.
- `tab_add_n_pct(tabs_text, add_n, add_pct)` — the `add_n`/`add_pct` block, factored out of `tab_many()`'s
  finalize so `tab_many()` and `tab_counts()` share one implementation.

### R/tab-counts.R (~360 lines) — from-the-middle constructor (Phase 4)

`tab_counts()` (exported) builds a `tabxplor_tab` from already-aggregated counts, byte-identical to the
microdata `tab()`. It does **not** re-implement the pipeline: it normalises the input and reuses
`tab_plain()`'s `.fine` pre-aggregate entry + the shared finalize.

- `tab_counts_reshape()` — dispatch on input shape → canonical long tidy counts. `table`/`xtabs`/`matrix`
  (melt via `as.data.frame.table`; bare matrix coerced with `as.table`); wide `data.frame` (`pivot_longer(cols)`);
  frequencies + base N (`input="pct"`: `largest_remainder(freq × base)` per row); long tidy (as-is).
- `tab_counts_normalize()` — aggregate to the keyed `.fine` shape `[tab_vars…, row_var, col_var, n, (wn)]`;
  **drop `n==0` cells** so the aggregate is structurally identical to microdata's `.N`-per-observed-key
  (empty cells are recreated by `dcast(fill=0)`). Sets `weighted` and `has_real_n` (integrality of the counts).
- `tab_counts()` — validation + colour resolution (the SHARED `tab_resolve_settings()`, Phase 7b), then
  `tab_plain(…, .fine = fine)` → `tab_chi2` → `tab_ci` → `tab_add_n_pct` → rewrap (`test` attribute) →
  `tab_pvalue_lines`. Base-less input (non-integer counts) disables CI/chi2 with a message. Weighted =
  real unweighted `n` + weighted `wn` (§14). It passes `totrow = NULL` so the `contrib → totrow` forcing
  is skipped (totals are driven through its own `tot`).

### R/tab-resolve.R (~180 lines) — the argument-overwrite cascade (Phase 7b)

`tab_resolve_settings()` is the ONE pure, data-free resolver of the colour cascade shared by
`tab_build()` and `tab_counts()`: `color = "auto"` → a concrete measure (factor arm); `contrib` →
`chi2`/`totrow`; diff-family colour requires `ref` and forces `ci = "diff"`; and the split of the one
`color` argument into `color_diff_OR` (→ `tab_plain`), `color_ctr` (→ `tab_chi2`), `color_ci` (→
`tab_ci`) and `color_num` (→ `tab_num`). `resolve_color_auto_num()` is the numeric (means) arm, invoked
by `tab_num()`. The function reads only argument values + column CLASS metadata (never column values) —
that is the boundary the Jamovi `.js` mirrors and the Phase 7c cache keys on. Data-dependent resolution
(`ref = "auto"`/regex, `levels = "auto"`, `na`-drop, the leaf tot/totaltab forcing) deliberately stays
in the leaf builders. See `dev/tabxplor_argument_computation_map.md`.

### R/tab_classes.R (3554 lines)

Classes, dplyr methods, and colors. Contains:

- **Lines 1–200**: `new_tab()`, `new_grouped_tab()` constructors, `is_tab()`, validators.
- **Lines 200–900**: Print methods (`print.tabxplor_tab`, `tbl_sum`, `tbl_format_body`, `tbl_format_footer`), `tab_kable()`.
- **Lines 900–1200**: `tab_compact()` — merges multiple row_var tables.
- **Lines 1200–1500**: `tab_plot()` — ggplot visualization.
- **Lines 1500–2400**: Dplyr S3 methods (30+ methods for group_by, select, mutate, filter, arrange, rename, relocate, rowwise, summarise, ungroup, dplyr_row_slice, dplyr_col_modify, dplyr_reconstruct). Also `lv1_group_vars()` helper.
- **Lines 2400–2890**: Tab/grouped_tab vctrs casting methods (`vec_ptype2`, `vec_cast`).
- **Lines 2890–3100**: Color palette constants (6 palettes).
- **Palettes / styles**: `set_color_palette()`, `get_color_style()`, `build_palettes()`, `tabxplor_palette_env`, `palette_8bit`.
- **Lines 3210–3554**: `set_color_breaks()`, `get_color_breaks()`, color legend generation.

### R/tab_xl.R (~470 lines) + R/tab-xl-backend.R (~155 lines)

Excel export via **openxlsx2** (Phase 10h). `tab_xl()` orchestrates → `tab_xl_plan_one()` (pure
per-table plan) → `xl_write_table()` (per-sheet writer through the `xlb_*` backend). See the Export
System section above for the full pipeline. Key points:

- Two-channel colour from `fmt_color_channels()`: the text channel rides the unified font plan
  (bold + colour), the background channel is a fill pass; both applied over coalesced multi-area `dims`.
- numFmt codes from `format(syntax="excel")` (stars folded into the literal); font/border/alignment via
  the `xlb_*` wrappers (fonts `update=FALSE` complete-replace; borders `update=TRUE` per rectangle).
- `R/tab-xl-backend.R` holds the thin engine wrappers + the pure coalescers (`xl_runs`/`xl_rect_dims`/
  `xl_coalesce`) and documents the openxlsx2 style model.

### R/tab_md.R (366 lines)

Markdown export. Standalone file (does not modify existing code). Handles:

- Monospace padding for column alignment
- Bold formatting for total/reference rows
- Sub-table separators for grouped tables
- Clipboard and file output options

### R/utils.R (1306 lines)

Utilities and initialization:

- `.onLoad()` — sets all default options
- `quo_miss_na_null_empty_no()` — helper to check for missing/empty quosures
- Factor manipulation utilities (`fct_recode_helper`, etc.)
- `tx_str_wrap()` / `tx_str_trunc()` — stringi-based replacements for `stringr::str_wrap` / `str_trunc`
  (Last Phase b-ii dropped `stringr` and `magrittr`; the package uses base `|>` and `stringi` now)
- `score_from_lv1()` — scoring helper for survey data

### R/tab_reg.R (Phase 12c — LIVE; renamed from R/tab_logit.R)

Unified regression tables as native `tabxplor_tab` objects, over ONE family-dispatching engine. Public:
`tab_reg(data, dependent, predictors, family, exponentiate, wt, reference, method, color, color_signif,
...)`; `predictors` as a character vector = one model (`dependent` may be a vector → one column per
outcome), or a named list = model comparison (one column per model, blank where a predictor is absent).
`tab_logit()`/`multi_logit()` are thin **binomial-family wrappers** (curated binary-outcome UX). Internal
engine: `reg_detect_family()` (auto: binary→binomial / continuous→gaussian, else abort), `reg_fit()`
(complete-case `stats::lm` (gaussian) / `glm` (binomial/poisson) / `survey::svyglm` (weighted) →
`broom::tidy`; Wald CI in-house — z for fixed-dispersion glm, t(df.residual) for lm/quasi/svyglm — the
exact dual of the Wald p; `method="profile"` = `confint`+LR for unweighted binomial/poisson),
`reg_skeleton()` (var/level/term rows), `reg_column()` (align a fit → one fmt column), `reg_build()`
(assembler → `new_tab() |> group_by(var)`). `broom`/`survey`(/`MASS` for profile) are
`requireNamespace()`-guarded Suggests.

`exponentiate` (default `"nongaussian"`) drives the fmt shape: **multiplicative** OR/IRR → the `or` field,
`type="row"`, `display="or"`, `ci_type="or"`, `color="OR"` (neutral 1, `1/x` reciprocal); **additive**
gaussian β / log-odds → the `diff` field, `type="coef"`, `display="coef"` (raw signed render, no ×100/%/×),
`ci_type="diff"`, `color="diff"` (neutral 0), with the `var` field carrying var(Y) so the colour is the
effect-size **β/SD(Y)** against the `mean_diff` (Cohen) breaks. Reference rows (crosstab reference /
regression intercept + factor baselines) are never coloured (`fmt_color_plan`'s `gate & !is_refrow`).
No new fmt fields/attributes — `type` gained the value `"coef"`, `display` the token `"coef"`. See CLAUDE.md
Phase 12c + decisions §37. `R/tab_logit.R` and `R/tab_logit_2.R` are emptied (`git rm` pending; the
parsnip draft + or_plot/lm_plots deferred to a later display phase).

**Phase 14w — the reg display model.** `tab_reg()` sets ONE **`reg_meta`** record (list:
`family`/`effect`/`at`/`do_exp`/`eff_word`/`dependent`/`positive_level`/`predictors`/`split_var`/
`comparison`/`model_labels`/`conf_level`) via `set_reg_meta()` — since Phase 17b a sub-field of the
`meta` list (`get/set_reg_meta` are thin accessors into it), carried automatically by the ONE
`tab_attrs()` `meta` line + threaded through `reg_footer_lines`/`tab_pvalue_lines`). It drives: the reg
**title/caption** (`reg_title` / `reg_family_display_name` / `reg_family_short` / `reg_sheet_name`; Excel
title+sheet, md/kable caption); the **"Model:" legend line** (`reg_model_line`, ordered before the colour
legend at every footer site); and the **colour legend** (`legend_specs()` reads `is_reg = !is.null(reg_meta)`
— robust across footer materialisation, which drops `test` — derives the per-column effect word from
`family`+`ci_type` instead of the column-name suffix, and always uses "the reference category"). Header
rename (item 3): a single-outcome model column + its `empirical` companions share one outcome `col_var`
("`<dep>`: `<level>`" / the dep name for a numeric outcome), the model column is named "Model `<eff>`",
and a multinomial table strips ": OR"/": AME" from each category name — so one span covers a group with no
border between its columns (the 14s span-drop + `new_col_var` border logic are unchanged). See CLAUDE.md
Phase 14w + decisions §49.

**Phase 15e — mixed-family dependents in one table.** `family` is resolved **per dependent**
(`family_for(d)`; accepts scalar / positional vector / named vector; `"auto"` detects each outcome, an
ambiguous integer aborting for that outcome only), so one `tab_reg()` call can model several outcomes with
different families side by side (one column-group each). The per-dependent shape (`do_exp_for` /
`effect_shape_for` / `eff_word_for` / auto-`color_for`) rides on each **spec** (like `sp$trials`/
`sp$inverse`); `reg_build` reads `sp$*` (the scalar `reg_build` args stay the recycled default for direct
callers / a homogeneous table). Scope: vector-of-dependents mode (shared character `predictors`); model
comparison stays single-outcome; `split_var` composes unchanged; `at = "reference"` on a mixed table
degrades to `"average"`. The design question ("what goes column-level") is answered by a new **per-column
`model_family` fmt attribute** (the 10th; `""` on cross-tables; `get/set_model_family`; reconciled in
`vec_ptype2`/`vec_cast`/`vec_arith` like the other 9): each reg column self-describes its family, so it
survives dplyr and the colour legend reads it directly. Set at every reg `fmt()` site (`reg_column` /
`reg_marginal_column` / `reg_columns_multinom` / `reg_empirical_columns`). Rendering: `legend_reg_eff_word`
reads `get_model_family(col)` (dropping the scalar `meta$do_exp` that mislabelled a gaussian column in a
binomial-first table); `reg_gof_tibble` takes a **per-fit family vector** so each column shows its own stat
set (gaussian R² next to a logit McFadden — `test_grid_reg` already unions the rows and blanks the cross
cells); new `reg_model_lines()` emits ONE "Model:" footer line per distinct family present, each prefixed
by the outcomes it covers via `legend_name_list` (homogeneous → the single unprefixed `reg_model_line`,
byte-identical); `reg_title`/`reg_sheet_name` go generic ("Regression models"/"reg") when mixed. `reg_meta`
gains `families` (per dependent) + `exponentiate`; its scalar `family` stays the homogeneous fallback.
jamovi `jmvtab_reg_build()` now calls `tab_reg()` ONCE with per-dependent family/inverse/trials vectors
(the Phase 15d group-by-family / `tabxplor_tabs` stacking is gone). No fmt-field change; the only
structural regen was the goldens + the `fmt-contract` snapshot (the inert `model_family=""` attribute).
See CLAUDE.md Phase 15e.

### R/jmvtab.b.R and R/jmvtab.h.R

Jamovi module integration. `jmvtab.h.R` is auto-generated from `jamovi/jmvtab.a.yaml` by
`jmvtools::prepare()`/`install()` (never hand-edit). `jmvtab.b.R` is the R6 backend `jmvtabClass`
whose `.run()` bridges the Jamovi options to `tab()` (Phase 7a baseline — no longer `tab_many()`).
It maps the UI `color` measure (`no`→`FALSE`, `auto`→`TRUE`, else the measure string) + `color_signif`
policy onto `tab()`, forcing `ci = "diff"` when a policy needs significance data, then renders via
`tab_kable()` into the `html_table` HTML result (with manual lightable+bootstrap CSS injection, since
kableExtra classes don't survive in Jamovi). The full cache-aware rewrite is roadmap Phases 7c–7e.

### R/jmvtabreg.b.R, R/jmvtabreg-cache.R (Phase 15b — the Regressions analysis)

The `jmvtabreg` jamovi analysis wraps `tab_reg()`, mirroring `jmvtab`'s split: `jmvtabreg.b.R` is the
thin R6 backend (options → `jmvtab_reg_build()` → `tab_kable(engine="html")`), `jmvtabreg-cache.R` is
the engine-free build core + the live **fit cache**. Unlike crosstables (whose bottleneck is an O(N)
count scan), the regression bottleneck is model fitting, so the cache keys **fitted models**, not
aggregates. Two representations, both keyed reference-INDEPENDENTLY so a reference change is a hit:
a KB-sized **digest** (`coef` + `vcov` + reference-invariant `glance`) on the single-equation GLM
coefficient fast path, and the **raw fit** on the heavy paths (ame / profile / mnl-vs-rest / compound /
multinomial / ordinal / split). The seam is `tab_reg()`'s internal `.fit_cache` arg, threaded into
`reg_build`: on the fast path `reg_build_digest()` fits once at the canonical reference and
`reg_reref_fit_res()` reparametrizes to any factor-predictor reference by coefficient contrasts
(estimate = L'b, se = sqrt(L'VL); the same Wald finalize `reg_fit` uses via the shared `reg_wald_crit`)
— **no refit, byte-identical to a real refit** (`test-jmvtabreg-cache.R`). `.fit_cache = NULL` leaves
every ordinary `tab_reg()` call byte-unchanged. The store (2 tiers digest/fit, byte-bounded LRU) lives
in the hidden `cache_state` Image `$state`, reusing `jmv_hash`/`jmv_col_fp` from `jmvtab-cache.R`.
`jmvtabreg.h.R` is generated by `jmvtools::prepare()` (never hand-edit); the `.b.R`'s
`inherit = jmvtabregBase` is lazy so it loads before the header exists.

**Phase 15b-ii — the model-comparison "+" builder.** Three picker folders map the hidden Array UI
options into `tab_reg()` args: `jmvtab_reg_ref_vector()` (references), `jmvtab_reg_models()` (the
checkbox-grid model builder's `models` Array → `predictors`: an empty builder gives the flat predictor
pool = one model; ≥1 card gives a named list of subsets = model comparison), and
`jmvtab_reg_mult_vector()` (numeric-predictor `× k` scaling → `multiplicator`). Model comparison always
takes the **raw-fit** tier (the digest fast-path is single-model only), and a `reg_fit` value is
~9–11 MB on survey-scale data — so `JMVREG_MAX_FIT_BYTES` (24 MB) and `JMVREG_MAX_STORE_BYTES` (96 MB)
are sized to hold a handful of comparison fits; below those the cache would graceful-skip and every
display toggle would refit every model. `tab_reg.R` is unchanged (it was already feature-complete for
the named-list `predictors` / `compare` / `baseline` / `multiplicator` / `trials` args).
