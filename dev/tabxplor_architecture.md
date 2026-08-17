
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
| `n_eff` | double | Phase 18s: the effective sample size used for this cell's CI, `p(1-p)/Var_design(p)` (a mean: `s²/Var_design(x̄)`) -- from the closed-form flat-design variance under `options(tabxplor.design_effect = TRUE)` on weighted data, from `survey::svyrecvar` under a real design, else `NA` (the CI falls back to the raw unweighted base). Non-displayed |
| `obs` | double | Phase 18z5: the value this cell's estimate is COMPARED TO by the `tab_reg` colour measures -- the observed/crude effect (`color = "adjustment"`) or the reference group's estimate (`"between_groups"`), on the cell's own scale. `NA` everywhere else, which is what leaves those cells uncoloured. Displayable as `{obs}` |
| `gap_se` | double | Phase 18z8: the standard error of the GAP between the estimate and `obs`, on the estimate's own test scale (log-ratio for `or`/`ratio`, plain difference for `diff`). Written where the two estimates are independent (`tab_vars` groups, by quadrature from the two stored Wald intervals), which is what lets `color_signif` apply to `between_groups`. `NA` elsewhere -> the policies stay inert. Non-displayed |
| `row_kind` | character | What KIND of row the cell sits in: `data` (the neutral) / `total` / the five synthetic display rows `n`, `pct`, `pvalue`, `gof`, `blank`. Phase 19f; `is_totrow()` is the derived read `== "total"`, `$in_totrow` a read alias |
| `in_tottab` | logical | Cell belongs to the total table |
| `in_refrow` | logical | Cell belongs to the reference row |

**Attributes (per-column, accessed via `attr()`):**

| Attribute | Type | Description |
| --------- | ---- | ----------- |
| `scale` | character | **What this column ESTIMATES** — one key into the declared library `EST_SCALES` (Phase 19b / KEY 2): `level_n` / `level_pct` / `level_mean` (a level: no null to test against) · `points` / `mean_diff` / `raw_diff` (a difference) · `pct_ratio` / `mean_ratio` / `odds_ratio` (a ratio) · `log_coef` (a link-scale coefficient) · `mixed` (the bind neutral). The row carries which field holds the estimate, its null, its geometry, its colour ladder (`ladder`), what the column summarises (`var_kind`) and where its standardising SD comes from. It is **stored, never derived**: before 19b it was recomputed at every read by an order-dependent dispatch over `type`/`ci_type`/`display`/`model_family` plus a sniff of whether `var` happened to be non-NA |
| `comp_all` | logical | Compare against total table (TRUE) or subtable (FALSE) |
| `ref` | character | Reference type: "tot" or "first" |
| `pct_base` | character | For a percentage, what it is a percentage OF — and hence which axis its reference lies on: `"row"` / `"col"` / `"all"` / `"all_tabs"` / `"none"` (counts, means, coefficients). `type`'s other half. *(Phase 19b DELETED `ci_type`: the stored interval is always on the estimate's own `scale`, tested column by column, and "is there an interval here" is a data fact — `!all(is.na(ci_inf))` — not a second vocabulary. Making the incoherent state unrepresentable is what surfaces D21.)* |
| `col_var` | character | Name of the column variable this belongs to |
| `totcol` | logical | This column is a total column |
| `refcol` | logical | This column is a reference column |
| `color` | character | Color scheme (length 1 text, or 2 with a background channel): "no", "diff", "ratio", "contrib", "or", "OR", … |
| `color_signif` | character | Significance policy: `"ignore"` / `"grey_non_signif"` / `"guaranteed_effect"` |
| `model_family` | character | Phase 15e: a reg column's own family (`"binomial"`/`"gaussian"`/`"poisson"`/…), `""` on crosstabs — lets one `tab_reg()` table mix families |
| `role` | character | Phase 17c: a reg column's role, `"model"` / `"emp"` / `"n"` (`""` on crosstabs) — read by the colour legend to name each column's effect without matching its `"Emp."` label, and (z13) by `reg_spread_models()` and the `[dep]`-bracket strip; (z17) it is also the COLUMN AXIS of `tab_estimates()` / `forest_plot()` (internal `get_role`) |
| `conf_level` | double | Phase 18z13: the level THIS column's interval and its significance thresholds were computed at; `NA` = unknown. TWO accessors, and the split is load-bearing: the six reconcilers read the RAW `fmt_conf_level_attr()` so a bind carries "unknown" forward instead of freezing today's option into the result, while the four colour-engine thresholds (`fmt_gap_bounds`, the contrib residual gate, the `guaranteed_effect` origin, the p-value cell slot) read `get_conf_level()`, which falls back to `options("tabxplor.conf_level")`. Stamped by ONE sweep at each build tail (`tab_stamp_conf_level()` in `tab_assemble_tables` / `plain_core` / `num_core` / `tab_ci` / both `tab_reg` tails), never per `fmt()` call site. It is what makes a table built at `conf_level = 0.99` grey at 99 % rather than at the global option |

The attribute list is **derived** (Phase 17a): `fmt_col_attrs <- setdiff(names(formals(new_fmt)), c(fmt_field_names, "...", "class"))`, so adding an attribute (a `new_fmt()` formal that is not a field) needs no carry-site edit.

**How an attribute is CARRIED — the declared rule table (Phase 19a, "E1").** Until 19a the four reconstructor families still enumerated the attributes by hand, in **seven** blocks (`vec_ptype2.fmt.fmt`; the three `vec_cast.fmt.*` arms; `vec_arith.fmt.fmt` ×2; `vec_math` sum/mean) — so a 15th attribute meant eight edits, and `model_family` was silently dropped for two phases because one list was forgotten. They are now driven by **`fmt_attr_rules`**, defined beside `fmt_col_attrs` in the same shape `meta_bind_rules` + `tab_meta_bind()` use for the table-level `meta`. One row per attribute, four declared columns:

| column | values |
| --- | --- |
| `neutral` | what a mismatch collapses to (`"mixed"`, `"several_vars"`, `""`, `"ignore"`, `FALSE`, `NA_real_`, `"n"`) |
| `merge` | `same` · `comp3` (`comp_all`, 3-valued so NA-vs-set stays NA) · `elementwise` (`color`, per channel) · `min` (`degf`) · `weakest` (`basis`) |
| `arith` | `merge` · `neutral` (`totcol`/`refcol` — arithmetic destroys the position) · `x` (the display facts `color`/`color_signif`/`model_family`/`role`) |
| `scalar` | whether `new_fmt()` `[1]`-subsets it (`color` is carried whole) |

The **reader's default is derived from `new_fmt()`'s own formals** (`fmt_attr_default`), so "the reader's default is the constructor's default" is true by construction. `fmt_attrs_of()` reads a column's attributes in one `attributes()` call; `fmt_attrs_merge()` / `fmt_attrs_arith()` apply the rules through precomputed integer index vectors (no closure, no rule-string compare); `fmt_ptype_attrs()` splices onto a build-time zero-length prototype instead of re-running the 21-field constructor. Measured **~2× faster** than the straight-line code it replaced on `vec_ptype2` and `vec_ptype_common` (the compact merge's hottest path), with no end-to-end regression — `dev/benchmarks/e1_fmt_ptype2.R`.

A **build-time `stopifnot(setequal(names(fmt_attr_rules), fmt_col_attrs))`** is what makes the rule table exhaustive: it must run at install, because the index vectors are derived at the same moment and a missing row would make the loops silently *skip* an attribute. Mirrored in `test-fmt_class.R` for cached binary installs. Phase 20a added a fifth column, **`write`** — the attribute's own setter — and extended that assertion to it, so an attribute cannot ship with no validated way to write it.

**Reading and writing an attribute by name (Phase 20a, KEY 3).** The exported accessor family was the last hand-written mirror of `fmt_col_attrs`: ~23 functions written beside a table that declares 16 attributes exhaustively, neither exhaustive (four attributes had none) nor consistent (`set_diff_type()` wrote what `get_ref_type()` read). It stops growing with the table now:

- **`fmt_attr(x, name)` / `` `fmt_attr<-`(x, name, value) ``** cover all 16 and every future one, validated against `fmt_col_attrs`, dispatching on an fmt column or a data.frame. The getter is the **raw** stored value with the declared `neutral` default; the setter dispatches to `fmt_attr_rules[[name]]$write`, so it validates exactly as the named setter does and cannot become a second, laxer way in.
- The **named** accessors are the *taught* surface (a user writes `get_scale(x)`, and the vignettes do); `fmt_attr()` is the *programmatic* one (a helper loops over `fmt_col_attrs`). That split is stated in the header, and it is what makes having both legitimate rather than duplication.
- ⚠ The **hot path stays hand-written** (the `DISPLAY_TOKENS` precedent): `get_col_var()` (33 call sites in `R/`), `is_totrow()` (44) and `get_scale()` are `attr()` reads on O(columns) loops and are untouched.
- ⚠ `get_conf_level()` / `get_degf()` / `get_basis()` are **resolvers**, not raw reads (option fallback, `NA` → `Inf`, `""` → `"n"`), which is why they stay internal: "what does this column claim" and "what will the engine use" are two questions. **`tab_columns()`** (below) is the user-facing answer to the second.
- **The admission test**, stated beside the one governing a new attribute: *storing a fact is internal; exporting its accessor is a user contract — name the user story first.*

19a also made `vec_arith` reconcile `conf_level`/`degf`/`basis` with the same weakest-claim rule `vec_ptype2` applies — it used to take `x`'s blindly, so `design_column + n_column` claimed `"design"`.

**Critical distinction:** Fields are per-cell vectors (every cell can have a different `n`, `pct`, etc.). Attributes are scalar values describing the entire column (all cells in the column share the same `type`, `color`, etc.). Do not confuse the two when modifying the class.

**Constructor chain:** `fmt()` (public, validates and coerces arguments) → `new_fmt()` (internal, calls `vctrs::new_rcrd()`). `new_fmt()`'s field formals default to `NULL` and are filled in the body from ONE shared `nas`/`fls` vector (Phase 18z6): copy-on-write makes that invisible, but a fresh record costs 1 allocation instead of 17, and its `display` default is base-R rather than a `dplyr::case_when()` that cost more than half the constructor on all ~210 calls of a `tab_many()` build — including the size-0 `vec_ptype2` path, the compact merge's hottest fmt site.

**The record is deliberately DENSE:** every column carries all 21 fields, and an inapplicable measure is stored as `NA`, never as an absent field. That is the contract `test-fmt-contract.R` locks, and the colour engine reads it directly (`fmt_adjustment_score()`, `get_num()`'s `obs` arm and the tooltip builder all call `get_obs(x)` on *every* column and leave the cell uncoloured because the value is `NA`). Optional/sparse fields were measured and rejected in Phase 18z6 — feasible in vctrs, but worth ~0.03 % of build time and ≤92 KB, while replacing a fixed shape with a per-column variable one and adding a second encoding (`"obs" %in% fields(x)`) of a fact `is.na(get_obs(x))` already states. See `dev/empty_vctrs_fields_sparse_record.md`.

**Adding a new field** touches 9 sites, all in `R/fmt_class.R`: the roxygen field list + count, `fmt()` (formal, `vec_cast`+`vec_recycle`, pass-through), `new_fmt()` (formal, the shared-`nas` fill, the `new_rcrd()` list), the hand-maintained `fmt_field_names` vector (forget it and the field silently becomes a 12th *attribute* -- `test-fmt_class.R` catches that), the getter/setter factories, and the four reconstructors (`vec_cast.tabxplor_fmt.tabxplor_fmt` carries it; `vec_arith` +/- and */, and `vec_math` sum/mean reset it to `NA`). A DISPLAYED field additionally needs `get_num()`/`set_num()`, `format()`, `tab_xl` and **a `DISPLAY_TOKENS` row** (`R/tab-display.R`) -- since Phase 19m-iii a build-time `stopifnot()` at that file's tail ties the two switches to the table both ways, so a token read by `get_num()` with no row, or declared `settable` with no `set_num()` arm, fails the install rather than shipping (that check is what found `pct_ci`/`mean_ci`/`pvalue` writing nothing at all); a non-displayed one needs none of them (`pillar_shaft()` never lists fields). Then: `JMVTAB_CACHE_SCHEMA`, and one conscious golden regeneration proved by `dev/verify_golden_field_delta.R`. Follow the `/vctrs-field` skill.

### tabxplor_tab — The Table Tibble

`tabxplor_tab` is a tibble subclass created via `tibble::new_tibble()` in `R/tab_classes.R` : it’s strenght is to work with normal `dplyr` workflows. It adds **three top-level attributes** beyond what a regular tibble carries — `subtext`, `test`, and a single **`meta`** list (Phase 17b gathered every other table attribute into it):

- `subtext` (character vector): Legend lines printed below the table.
- `test` (tidy tibble, 2.0.0 — renamed from `chi2`): whole-table test results, one row per
  (sub-table × col_var × test-type). Columns: `[tab_vars…]`, `row_var`, `col_var`, `test`
  (`"chi2"` / `"F_welch"` / `"F_classic"`; regression GOF reuses the same tibble with disjoint
  discriminators — `"n"`/`"lr_null"`/`"aic"`/… — keyed by fit column, split level in `row_var`),
  `statistic`, `df1`, `df2`, `pvalue`, `n`, `min_e` (smallest expected chi2 count, drives the weak-test
  flag). *(Phase 16a dropped the vestigial `variance` column — it was written `NA` everywhere.)* Chi-squared
  is filled for factor columns, ANOVA F for mean columns (both computed by the vectorised engine in
  `R/tab-agg.R` — `agg_chi2()` / `agg_anova()` — via `tab_chi2()`). **Phase 18j** added three columns:
  `effect_size` + `es_type` (Cramer's V / phi for factors, eta² for means — a companion ON each test's
  row, not a separate row) and `pvalue_exact` (the Fisher-exact p on a small weak factor table, stored ON
  the chi2 row so the row count is unchanged; the display prefers it, labelled "(Fisher)"). The opt-in
  test RUNG is **derived, never asked for** (Phase 18z14-i): `test` is only `TRUE`/`FALSE`, and what
  you already passed decides how. `wt` alone → the chi2 AND Cramer's V are computed on the **weighted**
  table rescaled to the raw n (the convention the CIs and the ANOVA F already followed; unweighted input
  stays byte-identical because `get_wn()` falls back to `get_n()`, so the rescale factor is exactly 1 —
  and Fisher is skipped, an exact test needing whole observations). `wt` + `tabxplor.design_effect`, OR
  a `survey::svydesign` as `data`, → `"chi2_design"`/`"F_design"`: **the same estimator**
  (`survey::svychisq` / `svyglm` + `regTermTest`), run on the user's design or on the flat one a weight
  column defines. Two functions in `R/survey-design.R`: **`svy_omnibus_grid()`** PRODUCES the grid
  (one row per subtable × col_var, carrying Rao-Scott's mean generalized design effect `deff`) — the ONE
  test path that reads the microdata rather than the aggregate — and `tab_robust_overlay()` lays it over
  the classic rows. The split is z16-iv (W-B): the grid is produced in `tab_transform()` because the
  contrib residual's base needs it there, and joined in `tab_assemble_tables()` because only there are
  the numeric ANOVA rows bound; producing it ONCE is what makes a table's omnibus p and its cell colours
  describe the same design effect. The producer also carries the TOTAL-TABLE group, which the old
  overlay silently dropped, and is skipped for an input that cannot serve the weighted basis
  (pre-aggregated counts), whose footer says basis `"n"`. Two discriminators, not four, because there is one estimator;
  WHICH design a table used is `meta$inference$basis`, never a second encoding in `test$test`.
  `svy_inference_basis()` resolves the basis in `tab_setup()`, the one place holding both the resolved
  weight and the `design_spec`, so `tab()` / `tab_many()` / `tab_counts()` cannot drift. Read it with `get_test()` (which also
  falls back to the old `chi2` attribute); `get_chi2()` is a kept alias.

  **Phase 20c (KEY 5) declared its vocabulary.** The tibble carries **39 kinds of row** under one
  `test` discriminator, and until then only the regression half had a declaring table
  (`reg_footer_spec()`, 31 of them): the crosstab half was string literals in four consumers, the
  eight `compare_*` keys were `paste0()`-generated in `R/tab_reg.R` and hand-enumerated in
  `R/tab-test-display.R`, and the `interact_*` labels sat in a third literal map. **`TEST_ROWS`**
  (`R/tab-test-display.R`) is one row per discriminator, for both producers, with 13 columns — see the
  file header for each. Two rules govern reading it: *`TEST_ROWS` owns what a row IS, the producer
  owns what it COMPUTES* (nothing there decides a statistic), and the **`stat` column is the
  many-to-one** that keeps the user's `stats =` vocabulary smaller than the storage's
  (`linearity_lr`/`_f`/`_wald` all carry `"linearity"`; the four `compare_baseline*` rows all carry
  `"compare_baseline"`). Every previously hand-written vocabulary is DERIVED from it with contents
  **and order** intact, so no consumer moved. The seven model-check rows stay generated from
  `REG_CHECKS`, which owns `families` / `weighted_ok` / `panel` and the two taught-but-never-scored
  checks that have a panel and no row. ⚠ `reg_footer_spec()` must stay a **function** — a top-level
  list freezes `gettext()` at the build locale. ⚠ `reg_stat_keys()` is a **union** with
  `names(REG_CHECKS)`: deriving it from `stat` alone deletes those two panel-only keys. Its declared
  `dep` column is **`outcome`** since 20c, renamed with the argument.

  Rendered by the shared summary framework in `R/tab-test-display.R` (Phase 16a). Three shared layers, each used by both crosstab and
  regression: (1) CONTENT — `test_display_rows()`, `test_cell_label_weak()` (label + `min_e < 5` weak
  flag), the `test_fmt_*` formatters and the fmt-cell builders (`pvalue_line_fmt` / `reg_gof_cell` /
  `stat_line_fmt`) + `reg_footer_spec()`. Phase 18m: the crosstab summary is **p-value then effect
  size** (no statistic by default; `tabxplor.test_lines = "all"` adds it back); the test type is named in
  the p-value ROW (`test_pvalue_descriptor` → "pvalue (Chi2, Welch F; survey-design)"/"Fisher"/" !") and the
  measure in the effect-size ROW (`test_es_measure` → "Cramér's V, eta2"), so the cell is the bare p (no
  in-cell "(Chi2)"); (2) CONSOLE — `test_summary_grid()` → a backend-free grid,
  `test_render_console()` → the GFM block; (3) EXPORT — `tab_append_footer()`, the ONE fmt-frame append
  engine behind BOTH inline-row appenders (`tab_pvalue_lines()` / `reg_footer_lines()`, in
  `R/tab_classes.R` next to `tab_materialize_extras`, now thin arm-specific configs over it — a crosstab
  supplies the p-value/statistic rows keyed by grouping ∩ `test`, a regression the per-split GOF block).
The remaining metadata lives inside the ONE **`meta`** list (Phase 17b), each item an optional sub-field
(`NULL` when unset; an all-`NULL` meta attaches no attribute at all). Every legacy getter
(`get_render_extras`/`get_vars_attr`/`get_empirical_tips`/`reg_call`/
`get_color_breaks_attr`) is a thin accessor into it; `set_meta_field(x, field, value)` writes one
sub-field (a `NULL` value removes it). The sub-fields:

- `render_extras` (Phase 10i-B) — the display-only `add_n` / `add_pct` intent, above. *(Phase 19b DELETED `ci_settings`: which interval method a column's bounds were built with is a per-COLUMN attribute now, `ci_method` — see the attribute table.)*
- `color_breaks` (Phase 13a) — the per-table colour-break override; joined `meta` in Phase 17b so it now
  SURVIVES a dplyr pipeline (was a standalone attribute set last, silently dropped by any verb between
  build and render — defect 7). Still installed transiently at render by `push_color_breaks()`.
- `vars`: only what NO column can carry --- `wt`, `caption`, `var_labels`. Phase 19f (KEY 1) emptied
  it of the variable MODEL: `row_vars` / `tab_vars` / `compacted` come from the DECLARED index columns
  (`tab_declared_vars()`), `col_vars` from the fmt columns' own `col_var` attribute, and `row_roles`
  from the `row_kind` field. The `caption` sub-field is set by the exported `set_caption()` / read by
  `get_caption()` and every exporter's caption fallback (ahead of `reg_title`).

### Referential integrity between the fact tables (Phase 20a, KEY 2 --- `R/zzz-fact-keys.R`)

Phase 19 replaced ~15 vocabularies-written-in-their-consumers with ~15 **declared fact tables**
(`MEASURES`, `EST_SCALES`, `COLOR_SCALES`, `CI_GEOMS`, `CI_METHODS`, `DISPLAY_TOKENS`,
`REG_ESTIMANDS`, `REG_EMPIRICAL`, `REG_FAMILIES`, `REG_CHECKS`, `TAB_ARG_VALUES`, `TAB_OPS`,
`ROW_KINDS`, `fmt_attr_rules`, `meta_bind_rules`; Phase 20b adds `TAB_ARGS` and `TAB_OPTIONS`).
That closed one class of drift and opened another:

> **a key written by hand in one table and read by name in another is a FOREIGN KEY**, and until 20a
> none of them was checked.

19d renamed the colour measures to full words and did not reach `EST_SCALES$label_meas`; the forest
plot lost its glyphs and *errored on lookup*, and the fix shipped with a `WARNING:` comment telling
the next person to remember — which is hard rule 4's forbidden pattern one level up.

**`TAB_FOREIGN_KEYS`** declares **34 edges**, one row each: `from` (what a message names), `get` (a
closure returning the values), `to` (a closure returning the legal key set), `allow` (values that are
legal without being keys) and `orphan` (also report target rows nothing references). It is read only
through **`tx_check_foreign_keys()`**, which runs at **load** — a dangling key breaks the build at
the moment it is made, not at some later render.

Three constraints, all of which cost a debugging session to learn:

- ⚠ **The file must sort LAST.** There is no `Collate:` field, so R sources `R/` in C collation, and
  the tables are spread over seven files: `COLOR_SCALES` is in `tab_classes.R` and `REG_EMPIRICAL` in
  `tab_reg.R`, both *after* `reg-estimand.R`. `zzz-` is the only prefix that is last by construction.
- ⚠ **Read every table with `[[`, never `$`.** `MEASURES$adjustment` has `scale_from` and no `scale`,
  so `$scale` partial-matches to `"gap"` and a generic checker would validate the wrong string. The
  two readers (`tx_fk_scalar()` / `tx_fk_all()`) are the only way rows are read here.
- ⚠ **`allow` entries are stated facts, never a way to silence a real dangling key.** There are
  three: `DISPLAY_TOKENS$field`'s `"ci"` names a *derived* quantity (`get_ci()` is a shim over the
  bounds), not a 22nd record field; and `"woolf"` / `"katz"` / `"wald_log"` are the only interval of
  their geometry, so none of them is a `ci_method` a user chooses.

**What is here and what is not**: every *cross-table* edge is declared here; a table's *own*
consistency ("does it cover its own key set", "does `DISPLAY_TOKENS` agree with the two `switch`
bodies") stays beside the table, where its operands are in scope — the header lists those seven
blocks by name so the inventory is complete either way.

### The row model (Phase 19f, KEY 1 --- `R/row-model.R`)

A tabxplor COLUMN is exhaustively self-describing. A ROW had nothing, and "what is this row" was
re-derived from four unrelated sources: a per-cell logical `in_totrow`, a *display-time* positional
character vector `meta$vars$row_roles`, a magic-named label column with three naming conventions, and
comparisons of rendered `format()` strings. Phase 19f gives the row axis the same treatment, with
**two facts and two carriers**:

**1. `row_kind`, a FIELD of the record** (`ROW_KINDS` = `data` / `total` / `n` / `pct` / `pvalue` /
`gof` / `blank`). It replaces `in_totrow` --- the record stays at 21 fields --- and it cannot live
anywhere else: `fmt_color_plan()` calls `is_totrow()` on a LONE extracted column with no table in
scope (locked by `test-degraded-attrs.R`). Every producer stamps the rows it creates
(`tab_append_pctcol_rows(role =)`, `tab_append_footer(row_role =)`), so the kinds ride every slice,
bind, arrange and rebuild. `fmt_row_kind(tab)` reduces them to one per row ("first non-`data` wins");
`tab_row_roles()` is that read, and its label-matching fallback now fires only for a frame with no
fmt columns at all. `is_totrow()` / `as_totrow()` / `$in_totrow` are derived reads of it.

**2. `tabxplor_lvl`, a factor SUBCLASS on the index columns**, carrying three ordinary column
attributes: `role` (`"level"` / `"var"` / `"tab_var"`), `var` (the variable its labels belong to; `NA`
on a merged `levels` column) and `ordered` (a named logical, ONE ENTRY PER VARIABLE --- which is how a
merged table keeps the fact that some of its stacked variables were ordinal after the factor itself
must go plain). It IS a factor, so `is.factor()`, `levels()`, `as.character()`, `arrange()`'s factor
order, `filter(levels == "Total")`, `group_by()` and printing all keep working with no method written;
only `vec_c`/`bind_rows` (ptype2 + cast), `droplevels()` and `[` need one. Every producer declares its
index in ONE call, `tab_stamp_index()`: both leaves, `tab_compact()`, `tab_reg()`, the transpose.

Read back through **`tab_declared_vars(tabs)`** --- `row_var` (the level COLUMN), `tab_vars`,
`var_col` (the column naming each row's variable), `row_vars` (the SOURCE names) and `compacted`.
`tab_render_vars()` / `tab_get_vars()` call it first and keep the last-factor heuristic as a clearly
marked degraded path (a hand-built frame, or `mutate(levels = as.character(levels))`).

**What it deleted or unlocked.** `tab_reg()` stops punning a predictor as `tab_vars = "var"`;
`tab_collapse_total_rows()` compares a KEY (`n`/`wn`/`pct`/`mean`) instead of a rendered `format()`
pass; `tab_estimates()` gets real roles instead of an English-label fallback; the export prep's
variable-name column is `rv$var_col`, one rule for a merged crosstab and a regression; and
**`tab(d, c(marital, relig), race, tab_vars = black)` returns a table** --- `can_merge <-
length(tab_vars) == 0` is gone, since the row-variable axis is a declared column and no longer
competes with `tab_vars` for the single dplyr grouping slot.

### The regression estimand (Phase 19e, KEY 8b --- `R/reg-estimand.R`)

A `tab_reg()` table's ONE decision --- *what does this column estimate* --- used to be spread over
FOUR arguments (`family` x `effect` x `at` x `exponentiate`): 36 combinations for 9 distinct
estimands, three degrade blocks, two aborts, and ~19 cells in which an argument was silently ignored
(`exponentiate` was a no-op on the whole marginal path; a RISK RATIO could only be obtained by naming
the wrong distribution). It is now **two questions and one declared library**:

```r
effect  = c("coefficient", "marginal", "at_reference")            # WHICH contrast
measure = c("auto", "odds_ratio", "ratio", "difference", "log")   # WHICH effect measure
```

**`REG_ESTIMANDS`** holds one row per (family, effect, measure) the package can answer, plus the rows
that state why one CANNOT be answered. A row carries `builder` (which of `reg_build()`'s three column
builders runs --- the table-scalar `if` is gone), `fit` (the internal family key: `"rr"` = modified
Poisson, `"rd"` = identity link, `"mr"` = log-link pseudo-ML --- each a LINK chosen to reach a
measure, never a distribution a user should name), `exp`, `word` (the column header), `scale` (the
`EST_SCALES` key stamped on the column), `display`, `crude_fam` / `crude_shape` (which `REG_EMPIRICAL`
row pairs with it), `comparison` (the marginal contrast), `engine` (WHICH engine computes this row's
marginal quantities --- tabxplor's own g-computation or `marginaleffects`; `"auto"` resolves the rule
once, in `reg_marginal_engine()`), `status` and two closures `why` / `note`. Read ONLY through
`reg_measure_key()` / `reg_estimand()` / `reg_estimands_for()` / `reg_estimand_abort()`, with a
build-time `stopifnot` keeping it coherent with `EST_SCALES`. ⚠ a new column goes in `est_row()`'s
DEFAULTED TAIL: the first eight arguments are positional at all 36 call sites.

**The vocabulary is `tab()`'s.** `measure`'s values ARE `EST_SCALES$geometry`, which is what
`tab(color =)` resolves into --- *the argument names the geometry; the attribute names the row*. So
`tab_reg(color =)` no longer takes a geometry at all (**D25**): the ladder comes from the column's
stored `scale`, and what is left to choose is what to compare it TO --- the measures for which
`measure_own_ref()` is TRUE, a DERIVED allow-list. `TRUE` in the text slot means "the column's own
geometry", so `c(TRUE, "adjustment")` replaces `c("OR", "adjustment")`.

**Three states, not two.** A row with `status = "ok"` builds; `"impossible"` aborts with its own
reason (an odds ratio needs a probability to take the odds of); **no row** means "not offered", and
the message ENUMERATES what the outcome does offer, generated from the table. A fourth state exists
only at run time --- a link that does not converge, where the risk-difference fit falls back to the
linear probability model and says so. Four consumers, one table: the boundary resolver, the error
message, the exported `reg_measures()` lister, and `?tab_reg`'s section (a roxygen `@eval` of
`reg_measures_rd()`; Phase 19k adds the jamovi eligibility rule as the fifth).

**What it deleted.** `reg_effect_word()` (a four-argument nested switch) IS the `word` column;
`reg_model_note()` (six family arms x `do_exp`) IS the `note` closures; `reg_crude_shape()`'s
dispatch --- including its cross-family borrow --- IS `crude_fam` / `crude_shape`;
`do_exp_for` / `effect_shape_for` / `eff_word_for` are views of one row; and `reg_column()` writes the
estimate into the field its SCALE declares (`or` / `ratio` / `diff`) instead of choosing between two
hard-coded fmt() calls, which is what made a third shape (a risk difference in percentage points, a
ratio of means in the `ratio` field) unrepresentable.

### One word per question (Phase 20c, KEY 4)

`tab_reg()` is unreleased, so the six questions the two producers asked with two words each were
**renamed**, not aliased. A retired spelling lands in `...` and aborts as an unknown argument through
the shared `tab_check_dots()` (Phase 20j deleted the retired-name table --- the abort no longer names
the replacement, but a removed name still errors: no silent no-op).

| the question | was | is |
|---|---|---|
| which sub-populations | `split_var` | **`tab_vars`** (the storage has said so since 19f) |
| which predictor baseline | `reference` | **`ref`**, `c(var = "level")` --- the same grammar `tab()` takes |
| which OUTCOME level | `inverse_two_level_factors` (a logical) | **`outcome_level`**, `c(outcome = "level")` |
| what is being modelled | `dependent` | **`outcome`**, package-wide |
| how is the interval computed | `method` | **`ci_method`**, `CI_METHODS`' fifth slot `model` |
| what rides the model footer | `stats` + `compare` + `baseline` | **`stats`** |

**`ref` and `outcome_level` stay two arguments** because they ask opposite questions: *`ref` names
the level you compare AGAINST, `outcome_level` the level you MODEL*. The one non-uniformity ---
"modelled" at k = 2, "baseline" at k > 2 --- is forced by arithmetic (with two levels, singling one
out IS choosing what is estimated; with more you can only choose the pivot) and is therefore
**declared**, as a `REG_FAMILIES` column whose `NA` row plus a `why` closure IS the ordinal refusal.
The multinomial baseline moved out of `ref` with it, so an outcome named there now aborts pointing at
the argument that works.

`stats` absorbed the other two through a grammar the package already had: **a `stats` element is
always a KEY**, carried in the NAME when it has a parameter and in the value when it does not ---
`c("n", "aic", "compare_sequential")`, `c("n", compare_baseline = "Model 1")`,
`c(compare_baseline = 2)`. `reg_resolve_stats()` splits it back into the plain
`(stats, compare, baseline)` triple every producer below already speaks, so nothing downstream
changed. ⚠ a comparison key **adds** a row and restricts nothing.

**What `TAB_ARGS` does and does not do for this producer.** All 25 formals are declared, which is
what lets `tx_check_tab_args()` police the signature and makes the convergence a *checked* fact ---
but `tab_reg()` gets no `@eval tab_args_rd()`, because the phase measured the justification and it
was absent: the two producers share the **name and the grammar** of `wt` / `ref` / `na` / `display` /
`color` / `ci_method` / `tab_vars`, **not the prose**. Emitting the crosstab text into `?tab_reg`
would be wrong documentation, not deduplicated documentation.

### The regression argument boundary (Phase 19m-ii --- `R/reg-resolve.R`)

Phase 19i gave the four CROSSTAB producers one boundary (`tab_resolve_common_args()`). The regression
producer never got one: **738 of `tab_reg()`'s 821 lines** resolved 28 arguments before a single
`reg_build()` call, holding **30 of the package's ~190 user messages** in 13 % of the file. Inside
sat twelve ad-hoc local closures and two near-identical 14-field spec literals --- all there because
the per-dependent facts were never materialised, so each was recomputed on demand from a frame later
blocks kept mutating.

**One entry point, `reg_resolve_args()`, six declared stages, one typed return (`new_reg_args()`).**
`tab_reg()` is **147 lines** now, with **one** message left (the `trials`-length abort inside the
multi-dependent recursion, which is a dispatch over the call SHAPE, not resolution).

| stage | what it owns |
|---|---|
| `reg_validate_args()` | the checks that are PURE --- and four are NEW: `conf_level` (never validated here), `stats` (silently FILTERED, so a typo lost a footer row), `color_signif` (unvalidated, so an unknown policy was STORED on every column), `baseline`'s shape |
| `reg_prepare_data()` | the design unwrap, the formula escape hatch, the predictors dispatch, the labelled conversion, the `shape` recode, the predictor union, the five `split_var` refusals --- **every rewrite of `data`** |
| `reg_resolve_estimands()` | **the per-dependent TABLE**: `dep` / `family` / `rr_promoted` / `est` / `fit_family` / `trials` / `inverse` / `crude_key`, one row per outcome |
| `reg_resolve_output()` | `display`, `color`, `color_signif`, `empirical` --- and **the notes LAST** |
| `reg_resolve_fit_plan()` | `na_shared_vars`, the `reref` gate, the `reference` relevel, the multiplier and the shape terms on ONE frozen frame |
| `reg_resolve_specs()` | the labels, the positive levels, the ONE `new_reg_spec()` call site |

**`data` is INSIDE the boundary**, as a declared field of the record. A pure resolver is impossible
without a cycle: `family = "auto"`, `trials = TRUE` and `multiplier = "sd"` are ANSWERED by the data,
`shape` recodes it, `reference` relevels it --- and the relevel needs the families the estimand stage
resolves. Lifting them into a stage `tab_reg()` called itself would put the ORDERING in the caller.
`new_ctx()`'s `data = NULL` is the precedent.

**There is deliberately no `REG_ARG_VALUES`.** `TAB_ARG_VALUES` exists because FIVE producers had
re-implemented one boundary and drifted; `tab_reg()` is ONE, its vocabularies are already declared
once each (`REG_USER_FAMILIES` / `REG_EFFECTS_VALUES` / `REG_MEASURES_VALUES` / `REG_SHAPES` /
`reg_stat_keys()` / `REG_MULTIPLIER_KEYWORDS`), and `TAB_ARG_VALUES`' own exclusion rule --- *validating
it means REWRITING it, so it lives with its resolver* --- disqualifies eleven of the fifteen
candidates. The one genuine table-move was `COLOR_SIGNIF_VALUES`, written twice before.

**The per-dependent table is the key.** Nine of the twelve closures existed to re-derive one of its
columns; `est_for` even carried a `local()` memo cache, and `trials_for` was DEFINED TWICE. The
survivors are four one-line lookups plus four pure functions (`reg_eff_word(est, empirical)`,
`reg_trials_observed_max()`, `reg_color_auto_measure()`, `reg_color_for()`).

**THE ORDER IS THE DESIGN**, and the 23 constraints are written where they bind (`H1`..`H23`). Three
were violated: the `empirical` forcing and degrade straddled both the notes and the specs (so a
table's stored effect word could contradict its own column header), the `color_signif` default landed
22 lines after the note that reads it, and the frozen frame was built twice under a comment demanding
it be one. ⚠ The `reref` clause reads **13 resolved values across eight blocks** and is the one place
a wrong `TRUE` returns a stale-digest number rather than an error.

**Verification**: `dev/verify_reg_specs.R` --- 291 cases over 20 named axes, `save`/`check`, dumping
the **messages in order** as well as the specs, the whole `reg_call()`, every column's stored
attributes, every label and the `test` keys. It captures through `tab_reg()` alone (the resolver's
output is already stored at `reg_call(x)$fit_spec$specs`), so it runs unchanged on both sides of a
refactor.

### tabxplor_grouped_tab — Subtabled Results

When `tab_vars` are provided, the result is a `tabxplor_grouped_tab` — a `grouped_df` subclass. It carries the same table attributes, plus `groups` data from dplyr.

This class requires a separate S3 method for **every dplyr verb** to preserve class and attributes through operations. See the dplyr Integration section below.

## Calculation Pipeline

Since Phase 6 (2.0.0) both public entry points are thin wrappers over the internal engine
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
                ├─ tab_transform(ctx)    (tier 3 + tier-2 test) SCALAR over one row_var: the
                │      num_core(.fine=)/plain_core(.fine=) leaves, which compute the CELLS, their
                │      INTERVAL and their whole-table TEST (pct/diff/ratio/or + leaf_ci_plain +
                │      leaf_chi2 + fmt, O(cells); `.fine` may be a per-pair list, fine_for_pair), then
                │      the factor join, which binds the per-col_var `test` tibbles. Phase 19j (KEY 5):
                │      there is NO second pass — tab_apply_tests() is gone, and the ordering invariant
                │      (compute on the full level set, before the level-drop) is structural.
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
`list2env()` needs, now encoded in the helper instead of comments). Phase 19i extends the declaration
to every STAGE PRODUCT (`settings`, the four variable roles, `fine_num`/`fine_fused`, `tabs_text`/
`tabs_num`/`tests`, …): 54 declared fields against ~81 live ones had left 27 undeclared, and an
undeclared field is *absent*, so `list2env()` creates no binding and its own `is.null()` guard errors
instead of firing (the 19a D7 class). The `utils::globalVariables()` declaration those bindings need
is DERIVED from `new_ctx()` + `CTX_SETTINGS_LOCALS` at the end of `R/tab.R`, replacing a ~70-name
hand-kept mirror in `R/fmt_class.R` that had outlived one of the fields it named.
Since Phase 19j there is no post-join test pass at all: the LEAF computes the interval and the
whole-table test (`leaf_ci_plain()` / `leaf_chi2()` / `leaf_chi2_num()`, `R/tab-leaf.R`), which is what
makes the ordering invariant structural. `tab_counts()` reuses the SAME stages: it holds its aggregate, so it builds a single-pair
ctx, runs `tab_setup()` (incl. the `tot`→totrow/totcol translation tab() uses) then `tab_transform()`
+ `tab_assemble()`, injecting its counts as the fused tier-1. `tab_lump_others()` / `tab_cleannames_relabel()`
are the two factor-relabel steps extracted from `tab_prepare()` (which still composes them for its
public callers); jmvtab (Phase 7e) runs cleannames at display instead (`jmvtab_cleannames_display()`).
`tab_compact()` (`R/tab_classes.R`) is the internal merge invoked when `output = "single"`.

**Labelled-data interop (Phase k, `R/tab.R`, no `haven`/`labelled` dependency).** `val_labels_to_factor()`
is the shared base-R converter (keyed only off the `labels` attribute): a variable whose value labels are
*complete* (every observed value labelled) becomes a factor whose levels are the labels, in the labels-vector
order; an *incomplete* one is stripped to its underlying numeric/character type (so a coded numeric keeps its
`tab_num` means path); a variable with no `labels` attribute is returned unchanged (byte-identity).
`tab_apply_val_labels()` applies it across a set of columns by name-`[[` (never `data[vars]`, which
row-subsets a data.table). It runs in `tab_setup()` (before the numeric/text classification), `tab_prepare()`,
the two leaf cores, `tab_counts_normalize()`, and `tab_reg()` (before family detection / the skeleton), so
`cleannames` then strips any `"1-"`-style prefix off the derived levels for free. `capture_var_labels()`
reads each variable's `label` attribute BEFORE conversion strips it; the map rides `ctx`/`shared` into
`meta$vars$var_labels` (stored only when non-empty — absent-when-unset, no golden churn, unioned across a
`tab_compact()` merge). The opt-in `tabxplor.var_labels` swaps names for labels at export via
`var_label_display()` (`R/tab-export-prep.R`) — the col-var span, the single-row_var header, and the merged
`row_var` column values (+ the transpose mirror). Display only: the tibble keeps canonical names.

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
`fine_fused` shipped once via `everywhere()`, together with a snapshot of the `tabxplor.*` /
`datatable.*` / `cli.*` / `crayon.*` / `width` options). Main gathers the finished per-row_var tabs and
runs `tab_assemble_output()`. **A worker's `message`s and `warning`s ride back with its value and are
replayed on main in unit order** (`tab_pmap_trampoline()` collects them under `withCallingHandlers`,
`tab_pmap()` re-signals them with `rlang::cnd_signal()`) — before Phase 20f they were simply lost,
measured on `tab_transform()`'s several-numeric-col_vars notice: 2 messages serially, 0 in parallel.
The `cli.*` options ride along for the same reason: cli renders its text at signal time, so without
them a daemon would format with its own glyphs and wrap width. ⚠ Necessarily NOT identical in one
respect: the replay happens after collection, so worker conditions land after anything the caller
signalled around `tab_pmap()` rather than interleaved with it. This is byte-identical because a single-row_var build equals its slice of the
integrated build — guaranteed by the `tab_assemble` total-col decoupling (`totnames |> unique()`, so the
lone-total rename-back tests the distinct name, not its occurrence count). jmvtab (cache_env) forces serial
and keeps its hooks (`jmv_cache_aggregate` in `tab_aggregate`; `jmv_cache_store_tests` in `tab_build_tables`,
reading the gathered pre-merge tests).

### The survey-design boundary (`R/survey-design.R`, Phase 18z14-i)

A `survey::svydesign` object passed as `data` is unwrapped at **one** place, `svy_unwrap_data()`, called
by every public entry point that accepts one (`tab`, `tab_many`, `tab_plain`, `tab_num`, `tab_reg`) —
and by `tab_counts()`, which uses the same `svy_is_design()` predicate to *refuse* one (pre-aggregated
counts cannot carry per-observation weights). It returns `NULL` for an ordinary data frame, so the normal
path costs one `inherits()` and is byte-identical.

It returns the design's `$variables` plus two package-owned columns:

+ **`.svy_weights`** = `weights(design, type = "sampling")` — the `type` matters: the bare `weights()`
  returns the *n × R replicate matrix* for a `svyrep.design`. It becomes the resolved weight name on
  every path, which is what lets `tab_weight_line()` read "this table is design-based" as a *fact* in
  one field instead of printing an internal name. A user column of that name is refused.
+ **`.svy_row`** = the row's position in the ORIGINAL design. It rides through `tab_prepare_pop()`'s
  `select()` exactly as `.filter` does, so the design-based test can index the design *from the prepared
  microdata* — the table actually displayed, after `filter=`, rare-level lumping and relabelling. Before
  this, the overlay tested the design's own untouched frame.

**`svy_domain_design(design, rows, frame)`** is the one domain-estimation helper, shared by the overlay
and by `tab_reg()`'s per-model design: restrict to `rows` (integer positions) and swap in the prepared /
recoded `frame`. Both halves are needed — picking the rows is not enough, because `svychisq` / `svyglm`
read their variables off the design. **WARNING:** `[` does not drop rows on a *calibrated* or PPS design;
it keeps all *n* and sets `prob <- Inf`, so the frame is padded back to full length (the padded rows
carry zero weight). Assigning a shorter frame used to error outright.

Replicate-weight (`svrepdesign`) and two-phase designs are **refused** with a message pointing at
`svydesign()` — a clear refusal, never an approximation. The design reaches `tab()`'s workers through the
`.ship` payload (once per worker), not `shared` (which copied the whole dataset per row_var).

### The inference basis (Phase 18z16-i, moved onto the columns in z16-iiiii)

`wt` says how the **estimate** is computed. A second, orthogonal fact says how the **interval and the
test** are — and the framework needed four encodings of one thing because there was no slot for it.

**`svy_inference_basis(design_spec, wt, force =, can_serve =, design_effect =)`** is THE resolver and
the ONLY place the option or the design object is read. Four values: `"n"` (the raw sample size — unweighted, or weighted
with the option off, which is the default), `"weights"` (the design effect of the weights, exactly: the
flat `ids = ~1` design), `"design"` (strata, clusters, `fpc`, calibration), `"design_partial"` (a design
was given but its variance could not be computed here). It resolves ONCE in `tab_setup()`, inside the
**one `ctx$inference` object** (below). `force = TRUE` is `tab_reg()`'s own rule (its crude `Obs_*`
columns are always on the weighted basis, beside a model column that always was, so the option is
`tab()`-scoped); `design_effect` is the per-call argument of `tab()` / `tab_many()` / `tab_num()` /
`tab_plain()` (`NULL` = the option, which keeps this the one reader of it);
`can_serve` is the INPUT's half — z16-iiiii folded in the declared `ctx$agg_only` ("this call holds a
pre-aggregate, not microdata"), which pre-aggregated counts set, so the basis a table reports is
already one it can honour. That single fact used to be re-derived three incompatible ways downstream:
`has_w2` in one leaf, `num_served` in the other, and `is.null(fine_fused) || by_table` in the omnibus
gate — which now reads simply `basis != "n" && chi2`.

**The one build-time object: `ctx$inference`** (`new_inference()`, z16-iiiii) = `list(wt, design, basis,
degf, conf_level, method, agg_only)`, built in `tab_setup()` and carried whole. It replaced ~10 flat
formals on `plain_core()` / `num_core()` (and on the post-join test pass 19j deleted), each of which had to be threaded through
five layers by hand and could be forgotten at one of them; `ctx$design_spec` / `ctx$conf_level` /
`ctx$ci_method` / `ctx$design_effect` / `ctx$agg_only` are now `tab_setup` INPUTS that nothing
downstream reads. Only the design travels separately to the parallel workers (`.ship`), which is why
`tab_rowvar_ctxs()` empties `shared$inference$design` and `tab_build_one()` fills it back in.

**`ci_method` — one named vector, four slots.** `CI_METHODS` (`R/tab-agg.R`) declares the interval kinds
and their legal values, first = the default: `cell` (wilson/wald/beta), `diff` (newcombe/ac/wald),
`mean_diff` (welch/student), `mean_ratio` (robust/quasipoisson/poisson). `default_ci_method()` derives
from it and `resolve_ci_method()` validates against it, so `tab()`, `tab_many()`, `tab_num()`,
`tab_counts()` and `tab_ci()` cannot disagree about a legal value. The vector is partial (an unnamed
slot keeps its default) and rides `inference$method`; each column is then stamped with the ONE engine
that built ITS bounds (`ci_method`), which is what the legend names. It replaced five `method_*` arguments listed,
validated, threaded, cache-keyed and stored one by one across six files; `method_cell` / `method_diff`
survive as soft-deprecated aliases (CRAN-released), and `method_ratio` went with the rest — a
proportion ratio has exactly one method (Katz), so it was never a choice.

**Where it is stored: on the COLUMNS, as the 13th and 14th fmt attributes** (`degf`, `basis`;
`R/fmt_class.R`, beside `conf_level`, which is there for the same reason). It was
`meta$inference = list(basis, degf, note)` until z16-iiiii, and a table attribute proved to be the
fragile carrier: **three** rebuild sites were found dropping the whole of `meta` — `tab_compact()`
(z16-iv), and then `tab_spread()` and `reg_build()`'s `split_var` branch (z16-iiiii) — so a
design-based table printed the footer of an unweighted one, and `tab_ci()` on the exported step path
silently fell back to `z` (measured 9 % too narrow at 13 PSUs). A number must not depend on a table
attribute. One sweep writes all three facts at each build tail — **`tab_stamp_inference(tabs,
conf_level, degf, basis)`**, the former `tab_stamp_conf_level()` — fed by `leaf_inference()`, which
both leaves, `tab_assemble_tables()` and `tab_reg()` share.

Four things follow:

* **The bind rule fires by itself.** "A merge can only claim what its weakest part carried" moved from
  `tab_inference_bind()` — a function someone had to remember to call — into
  `vec_ptype2.tabxplor_fmt.tabxplor_fmt()`, where every `c()` / `vec_c()` / bind / group applies it:
  weakest `basis` (`basis_rank()`), smallest non-NA `degf` (the widest critical value).
* **The table-level answer is DERIVED**, `tab_inference_basis()` / `tab_inference_degf()` — the weakest
  basis and smallest `degf` over a table's fmt columns. `tab_weight_line()` generates ONE footer
  sentence per basis from it, and `tab_ci()` reads `degf` off the columns (per column, which is more
  correct than one table-wide number). The `"n"`-and-weighted sentence is the important one: it is the
  DEFAULT, so the package's least defensible position stops being silent.
* **A degrade is a STATE, not a console message — and a LOCAL, not global state.** The two producers
  answer `list(v =, reason =)` (`svy_var_out()`), so the reason travels WITH the answer to the one
  caller that knows whether it matters; each core keeps its own `degraded` / `unserved` locals and
  passes them to `leaf_inference()`, which turns them into basis `"design_partial"` / `"n"` on ITS OWN
  columns. That deleted the process-global `svy_degrade_env` and its five helpers plus `svy_var_bail()`
  — six functions, twelve sites, and the whole stale-flag hazard class W-C had to patch with a reset in
  four entry points (one degraded table used to mislabel every later one in the session). `tab_reg()`
  carries the same fact out on its crude grid, as `attr(grid, "degrade")`. A table cannot assert a
  design its numbers do not carry, in any export, ever. ⚠ The degrade REASON (`size` / `failed`) is
  named in `svy_var_degraded()`'s message, where it is actionable, and is not carried on the table: the
  CLAIM is a property of the numbers, the reason is a build event.
* `degf` (the design's `#PSU − #strata`, captured at the boundary) reaches `tab_reg()` too since
  z16-iiiii — its `design_spec` literal used to rebuild the spec and throw `degf` away, so its model
  columns sat on `t(degf)` (an `svyglm`'s `df.residual()` IS the design df) while its crude `Obs_*`
  columns sat on `z`: at `degf = 8`, a crude bracket 15 % narrower than the model bracket beside it, in
  a table whose whole premise is that the two are comparable.

### Design-based cell variances (`R/survey-variance.R`, Phase 18z14-ii)

Route A: a design passed as `data` writes a **design-based effective n** into the existing `n_eff` field
— `p(1-p)/Var_design(p)` for a proportion, `s²/Var_design(x̄)` for a mean (Korn–Graubard's device, what
`svyciprop(method = "beta")` is built on). Because Phase 18s already made `n_eff` the single base
every per-cell inference reads (`tab_ci`'s cell/diff/ratio bases, `tab_apply_reference`'s `color = "OR"`
interval, `chi2_write_contrib`'s residual), that one write makes all of them design-based with **no new
fmt field, no column attribute and no colour-engine change**. Measured exact against `survey` to 1e-15
on stratified / clustered / stratified+clustered / **calibrated** designs.

**ONE influence function, four domain pairs.** Every quantity is a ratio of two weighted sums,
`p = A/B` with `A = Σu·w`, `B = Σv·w`, whose linearized contribution is `z = (u − p·v)/B`; `pct` chooses
`(u, v)` (`svy_uv_v()`), it does not choose a formula. Row **domains** come from the wide table's own key
columns with `"Total"` read as "every level", so a data row, a subtable total row and a total-table row
need no special case. Matching goes through a group code over the distinct key tuples, so the weighted
sums are small matrix products and only the influence matrix is ever *n*-long (one `svyrecvar` call per
column level; 7 MB at 60 000 × 15). `svy_var_prep()` deliberately does **not** reuse
`svy_domain_design()` — that helper swaps `$variables` for `svychisq`/`svyglm`, which `svyrecvar` never
reads — but its calibrated/PPS warning still applies, hence the scatter index and `w = 1/prob`.

Every function returns `NULL` rather than a wrong number; the leaf then falls back to the flat closed
form (below) and records the step, so the footer says `"design_partial"` instead of claiming the design.
Two consequences elsewhere: `use_raw` is forced under a design (a count aggregate cannot carry a design
variance), and `chi2_write_contrib()` takes the raw n over the ASSOCIATION's design effect — ONE base
for the whole table, the standard first-order correction `z_design = z_classic/√δ̄`. Route A is exact for
a cell and conservative for a cell-vs-reference difference (it cannot carry the row-to-row design
covariance, ruling Q3) — so it never produces a star the design does not support.

### The flat closed form (`R/survey-variance.R` § the flat closed form, Phase 18z16-ii)

**A weight column IS a survey design** — the flat one — and at `ids = ~1`, with no strata, no `fpc` and
no calibration, `svyrecvar` reduces to a plain sum of squares of `w·z` with survey's `n/(n−1)` factor.
Because `Σ w·z = (A − p·B)/B` is exactly zero for every base, the centering is a no-op and the whole
variance collapses to per-cell sums the aggregate core computes in the same pass as `Σw`:

```
Var(p̂) = n/(n−1) · [ A(1−p)² + (S−A)p² ] / B²          A = the CELL's Σw²
Var(x̄) = n/(n−1) · [ Σw²x² − 2x̄·Σw²x + x̄²·Σw² ] / B²   S, B = Σw², Σw over the base's domain
```

So the weighted basis needs no microdata at all: `O(cells)`, no ceiling, nothing that can degrade.
`svy_flat_neff_prop()` / `_mean()` / `_rows()` (the per-observation ratio form, for `tab_reg()`'s crude
grid) are the producers; `leaf_dmat()` is the base-domain broadcast, extracted so `leaf_wide_pct()` and
the variance provably use the SAME base — the leaf lost its `Σw²` arm and now computes percentages and
`tot_n` only. `svy_design_is_flat()` routes a `svydesign(ids = ~1, weights = ~w)` here too (same answer,
no influence matrix). Degenerate cells (`p = 0` or `1`, so `p(1−p)/Var` is `0/0`) fall back to the base
domain's `B²/S` — **which is Kish**, surviving only as what it always was: this formula with each cell's
own `Σw²` discarded, i.e. the limit for a cell carrying no information. Measured up to 17 % wrong in
either direction once the outcome follows the weight, and unable to move with the outcome at all.

`num_moment_scan()` accumulates `Σw²`, `Σw²x`, `Σw²x²` whenever the table is WEIGHTED (never on an
option), and the factor scan its `Σw²` likewise — **including the jamovi cached tier-1 aggregate since
z16-iiiii**, without which `jmv_cache_aggregate()` emitted only `(n, wn)` and the live `design_effect`
checkbox corrected the MEAN cell intervals while leaving the PERCENTAGES on the raw *n*, corrected
neither p-value (a mixed table lost its numeric `F_design` too), and printed a footer denying the one
correction that had happened. `Σw²` is additive, so the `.fine` rollup is an identity regroup. So the
aggregate has ONE shape — toggling the option is
a jamovi cache HIT, not a re-aggregate. All three are additive, so the wide rollup, the Total column and
the total rows get the right `A`/`S`/`B` by summation, with no special case.

**The parity contract is a test, not a comment** (`tests/testthat/test-flat-design-parity.R`, 50
assertions, every one against `survey` itself): variance ratio `1.0000000000` for row/col/all
percentages, total rows, subtable domains and means; the `Obs_OR` bracket == the univariable `svyglm`
SE(log OR); the omnibus chi2 and F == `svychisq` / `svyglm + regTermTest` to `1e-10`; equal weights give
`n_eff = n·(n−1)/n`; and the relative contribution `ctr` is identical at every basis.

**Phase 18z14-iii — the same producer serves `tab_reg()`'s crude columns.** A crude `Obs_*` cell IS a
weighted mean over a domain (a predictor level), so `reg_empirical()` takes `design_spec` and asks
`svy_var_mean()` for `Var_design` per level, writing Korn–Graubard's device into the bases it already
had (`emp_n_draw` for every proportion-scale interval, `emp_n_ci` for the mean-scale ones); its basis
comes from the shared `svy_inference_basis()` rather than a local `getOption()` read, and since
z16-iiiii every crude interval is also referred to the design's own `degf`, the same reference
distribution the `Model_*` column beside it uses. Two small
generalisations were enough: **`wmult`** (a per-row weight multiplier, because a grouped-binomial row is
a cluster of `trials` draws — the general ratio form, not a second formula), and level-INDEX domain keys,
which make the domain identical by construction to the grid's own `ok & x == l` and put a predictor level
literally named `"Total"` out of reach. `emp_n_draw` became per (level, **category**) because the
multinomial html tooltip prints its intervals. On that base the Woolf and Katz brackets *are*
`Var_design(logit p)` and `Var_design(log p)`; a ratio between two cells still omits their covariance, so
it lands a few percent either side of the exact answer (measured 2–7 %, against 15 % for the single-stage
base) — "conservative, never anti-conservative" is a statement about a *difference of proportions*, not
about a ratio. `svy_row_at()` was extracted here as THE row-space rule, because `reg-influence.R` needs
the same one (see *The gap test* below).

### The settings spine (`ctx$settings`, Phase 17e)

The argument-normalisation boundary is the historical "top bug factory": five documented bugs came from
recycling arguments across the `row_var × col_var` axes with vectorised `&` / length heuristics. Phase 17e
makes that class unrepresentable by combining the two axes ONCE, in `tab_setup()`, into a **star schema**
stored at `ctx$settings`:

- **`rows`** — one row per row_var; the per-row_var scalar settings (`color` — ONE resolved measure
  since Phase 19c, `comparison`, `or_ci`, `chi2`, `ref`, `ref2`, `comp`, `ci`, `ci_scale`, `totaltab`,
  `totrow`, + `na_num` from `tab_prepare_pop`).
- **`cols`** — one row per col_var; the per-col_var settings + the factor/numeric masks (`is_num`,
  `is_text`, `lvs`, `digits`, + `lv1` from `tab_prepare_pop`).
- **`pairs`** — one row per (row_var × col_var), the fact table carrying `pct`, `ref`, `ref2` and the
  `na` policy. Built `row-major` via `expand_grid(row_var, col_var)`, so it is byte-identical to (and
  REPLACES) the former `pct_vect` (5-branch nested list) and `ref_vect` (2-branch) ctx fields — those
  axes now meet only here.

**Phase 19i makes it the ONLY carrier.** Until then the same ~15 facts existed twice: `tab_setup()`
built the spine *and* wrote every one of them flat into the ctx, and `tab_rowvar_ctxs()` sliced the
spine only to re-flatten it into those same names — so the spine advertised itself as the interface
while every consumer read the duplicate (`ctx$settings` had exactly one functional reader). Now:
`tab_setup()` writes the spine and then DELETES the raw inputs it owns (`SPINE_OWNED_INPUTS`);
`tab_rowvar_ctxs()` slices and stops; and each stage opens with
`list2env(ctx_settings_locals(ctx), environment())`, which projects the spine into the bare names the
resolution blocks have always read. Pre-slice a spine column projects to a VECTOR over row_vars,
post-slice to the scalar the per-row_var stages expect — the same property the flat duplicates had,
which is why they existed. A bare-name read of a fact the spine owns can no longer find a
pre-resolution value.

The line: the spine carries **settings** (values the user chose or a resolver derived), at one of the
three grains. It never carries built **objects** — `fine_num` (a moment aggregate, sliced by name),
`remove_levels`, `na_drop_all` and the stage products ride the ctx, and `new_ctx()` declares every one
of them (19i) so an absent field can no longer make its own NULL guard *error*. The typed ctx and this
spine are the foundation the Phase 17f reference plan + leaf wrapper/core split build on.

**jmvtab live cache (Phase 7e, `R/jmvtab-cache.R`).** Since Phase 19k the module forks no rule
either: the population descriptor is `tab_cache_keys()`, the digits floor `num_digits_floor()`, the
display writer `tab_apply_display()`, the `ci` anchor `resolve_leaf_ci()`, the family rule
`REG_OUTCOME_KINDS` (from which the `.js` block is GENERATED by `dev/generate_jamovi_js.R`), and the
option NAMES/VALUES are tab()'s own -- locked by `tests/testthat/test-jamovi-vocabulary.R`. It reuses
this exact pipeline
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
per pair (`fine_for_pair()`) and reuses a cached test via the `cached_tests` ctx field the leaf reads;
`defer_level_merge` keeps full levels so `levels` is a display-time drop. Keys use
per-column fingerprints so adding a variable reuses the other pairs.

**Shared cache kernel (Phase 17i).** The store machinery — `jmv_cache_config()` +
`jmv_store_new/migrate/env/fetch/put/evict/cached` (one byte-bounded LRU, O(n log n), canonical entry
`list(value, bytes, seq)`) plus the `jmv_hash`/`jmv_col_fp` primitives — lives once at the top of
`R/jmvtab-cache.R` and is consumed by BOTH module stores as *config*: `JMVTAB_CFG` (3 tiers agg/test/
tab3, schema 6) here and `JMVREG_CFG` (2 tiers digest/fit, schema 3) in `jmvtabreg-cache.R`. The two
stores stay decoupled (their tiers + persisted `$state` differ); only the implementation is unified,
replacing the previous two drifting copies (one evicting O(n²)). Each module keeps a set of one-line
`jmv_cache_*` / `jmvreg_*` wrappers so its call sites and tests are unchanged. Two access patterns are
kept deliberately distinct — functional `fetch`/`put` (crosstab, clock bumps on every touch) vs
env-mutating `cached` (reg, clock bumps only on hit/store) — since the reg tallies + eviction are
byte-locked by `test-jmvtabreg-cache.R`. `jmv_col_fp` fingerprints a column by class / factor levels /
NA-count (a same-shape value edit is a blind spot → a possible stale hit); `options(tabxplor.jmv_full_hash
= TRUE)` forces a full-value hash in both modules. The `.b.R` orchestrators share `jmv_backend_weights/
_notice/_export/_render_html` (in `R/jmvtab-export.R`).
`tab_resolve_settings()` (`R/tab-resolve.R`, Phase 7b) is the ONE place the argument-overwrite cascade
lives — a pure function of (args, column classes) that draws the boundary between static arg resolution
(here) and data-dependent resolution kept at the leaf (`ref="auto"`/regex, `levels="auto"`, `na`-drop).
It is what the Jamovi `.js` mirrors and the Phase 7c cache keys on. Full arg↔computation map:
`dev/tabxplor_argument_computation_map.md`.

### tab() — and tab_many(), a translating shim over it (Phase 19h)

- `tab()` is the unified entry point: `row_vars`/`col_vars` accept one variable OR several (tidy-select);
  with several `row_vars` it **merges** by default (`output_list = TRUE` → a list). Singular
  `row_var`/`col_var` are soft-deprecated aliases.
- `tab_many()` no longer wraps `tab_build()`: it is a 10-formal shim (`R/tab-deprecate.R`) that maps
  the renamed arguments onto `tab()`'s and unwraps a length-1 result itself.
- The row_var axis is **globalised** on `tab()`: `color/comp/ci/chi2/ref2` are scalar (one value
  for all row_vars). Still per-row_var: `totaltab` and `ref` (a named/ordered vector, one reference row
  per row_var). The col_var axis stays flexible: `pct/levels/digits` are per col_var (Phase 19h made
  `pct` join the other two; a per-row_var LIST is refused). `levels` (`all`/`first`/`auto`) is a
  `tab()` argument again (Phase 7a); `sup_cols` is **soft-deprecated**, folded into the col_var axis by
  the one `tab_deprecate_sup_cols()` helper (it used to be mirrored into three arguments of the
  `tab_build()` call).
- **`tab_many()` is a translating SHIM over `tab()`** (Phase 19h, KEY 7), 10 formals instead of 42: it
  maps the five renamed arguments (`tab_deprecate_many()` + `tab_deprecate_na_drop_all()`) and forwards
  `...` bare. Only the first five positional slots are accepted (the two functions' 6th formals differ,
  so an unnamed 6th argument is refused rather than silently set). It keeps its historical shape — a
  list for ≥2 row_vars, a bare table for one — by unwrapping a length-1 result ITSELF; `tab_build()`'s
  third `output` mode `"legacy"` is deleted, so what `tab()` returns is a function of `output_list`
  alone (`options(tabxplor.output_kable)` renders, and no longer decides a class).
- `na` (microdata only, per Phase 7a): `"keep"` (NA as a level), `"drop"` (each col_var drops its OWN
  NA → bases can differ), `"drop_all"` (drop obs missing on `{row_vars, any col_var, tab_vars}` → one
  shared base; `tab_build` resolves it natively), `"common_base"` (reproduces the historical `tab()`
  population: drop NAs of `{row_vars, first col_var, tab_vars}` globally, keep secondary col_vars' NAs).

**`tab_counts()` (Phase 4) is the from-the-middle sibling of `tab()`**: same output, but the input is
already-aggregated counts (long / wide / `table` / freq+N). It does not scan microdata — it feeds a
count-aggregate into `tab_plain()`'s `.fine` entry (which computes its cells, their interval and its
test), then runs the same finalize tail. See the `R/tab-counts.R` file guide below.

### tab_many() Vectorisation Philosophy

`tab_many()` processes multiple variables with a key asymmetry:

- **col_vars** all share the same percentage type and color settings (they form one table).
- **row_vars** can have different color, ref, OR, chi2, and CI settings (separate tables that are optionally compacted together).

Arguments vectorised over row_vars: `totaltab`, `totrow`, `ref`, `ref2`, `OR`, `comp`, `color`, `ci`, `chi2`.
Arguments vectorised over col_vars: `levels`, `digits`, `totcol`, `pct`.

### Compaction: what is lost when tables are bound (the `output_list` decision)

By default `tab_many()` returns one table: the per-row_var results are bound vertically with `tab_compact()` (`R/tab_classes.R`), which stacks each variable's levels as rows (a `row_var` factor column marks the source) sharing the **same** `col_var` columns. A list of separate tables is available on demand (from 2.0.0, `output_list = TRUE`; the old `compact` argument is deprecated). This section documents exactly what compaction costs, so the single-table default is a conscious trade-off rather than a hidden one.

The `fmt` record splits into per-cell **fields** and per-column **attributes** (see the Type System section). When blocks are row-bound, the two behave differently:

- **Per-cell fields** (`n`, `wn`, `pct`, `mean`, `diff`, `ctr`, `var`, `ci`, `rr`, `or`, `in_*`) are simply stacked as rows and are **fully preserved**.
- **Per-column attributes** (`scale`, `pct_base`, `comp_all`, `ref`, `col_var`, `totcol`, `refcol`, `color`) are scalar — one value per column — so if two variables were computed with different settings for the *same* column, the merged column keeps only **one** value.

Concretely, for each argument vectorised over row_vars:

| row-vectorised arg | backed by | effect of compaction | genuinely needed? |
|---|---|---|---|
| `color` | `color` attribute | color **mode** collapses to one; the underlying `diff`/`or`/`rr` **values** stay (fields) | no — analysts use one colour scheme per table |
| `ref` / `ref2` | `ref` attribute + baked diffs | each block's diffs are already computed against its **own** total (compaction promotes each block's total to its reference row before binding), so the **displayed** result is preserved; only re-computability against a different ref collapses | displayed result preserved; divergent-ref recompute is not needed |
| `comp` | `comp_all` attribute | moot: compaction requires **no** `tab_vars`, and `comp` only matters with `tab_vars` | not applicable |
| `OR` | `or`/`rr` fields + color mode | values kept; only the colour-mode/`totcol` side collapses | no |
| `ci` | `scale` attribute + the `ci_inf`/`ci_sup` fields | CI **values** kept; the estimate **scale** (level / points / ratio) collapses to one, and an unlike merge collapses to `mixed` | rarely differs per variable |
| `test` | table-level `test` tibble (Chi2 + ANOVA F) | **concatenated** across blocks, not lost | preserved |
| `totrow` / `totaltab` | rows + fields | total rows **stack as rows**; each block's total becomes its reference row | preserved |

**One capability predicate (Phase 19h, `R/tab-shape.R`).** `tab_shape(x)` reads the shape off the
DECLARED model — the row-index columns' stored roles (19f) and `meta$spec$kind` (19g), never a column
name — returning `container` / `kind` / `merged` / `grouped` / the three variable axes (+
`same_col_vars` / `same_tab_vars` for a list). `TAB_OPS` declares, one row per operation
(`compact` / `transpose_object` / `transpose_render`), which of those facts it requires, at which
`severity` (`"abort"`, or `"bail"` for `tab_compact()`'s message-and-return contract) and why; the
exported `tab_supports(x, op)` and the internal `tab_check_shape()` are its only readers, and the five
scattered aborts became one call each. `rd_shape(rd)` builds the same record from a finished render
model, for the transpose. Refusals that are NOT about shape (duplicated row keys, >1 total row/column)
stay local to `tab_transpose()`.

Two structural limits of `tab_compact()`:

- It **errors** if the bound tables have different `col_vars`. In practice `tab_many()` gives every row_var the same `col_vars`, so this is not a real loss.
- Since Phase 19f a table carrying `tab_vars` merges like any other (it groups by `(tab_vars, row_var)`); what is still refused is a LIST whose tables disagree about which `tab_vars` they have — there is no one sub-table axis to merge them on.

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
4. **Reference + tests**: the reference-relative fields come from the ONE executor `tab_apply_reference()`; the interval (`leaf_ci_plain()`) and the whole-table test (`leaf_chi2()` / `leaf_chi2_num()`) are computed by the leaf itself, in the same pass (Phase 19j). The superseded standalone steps `tab_pct()`/`tab_tot()`/`tab_totaltab()`/`tab_ci()`/`tab_chi2()` all live in `R/tab-steps-legacy.R`, off the build path.
5. **Restore names**: Internal prefixes are removed; original column names restored.

### tab_num() — Numeric Column Variables

When a `col_var` is numeric (not a factor), `tab_num()` is used instead of `tab_plain()`. Since Phase 2 (2.0.0) each grouped `data.table` scan computes **sufficient moment sums** (`n`, weighted `n`, `Σ[w]x`, `Σ[w]x²`) in one pass; `num_derive_stats()` (`R/tab-agg.R`) then derives the mean and variance, reproducing the unweighted sample (n-1) vs weighted ML (÷Σw) definitions exactly. This removed the old `weighted.var()` double scan, and the total rows / total table are built as **roll-ups** of the additive moment-sum aggregate (`num_rollup()`) instead of two extra full-data scans. Net on 8M rows: `tab_num` ~5.6×/8.3× faster and ~6×/11× less memory (unweighted/weighted). The resulting `fmt` vectors have `type = "mean"` and `display = "mean"`.

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
  the **ratio scale** (neutral 1, `scale = "pct_ratio"` / `"mean_ratio"`, centred on the ratio), its dual the log-RR Wald.

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

**Significance reads the stored `scale`, not the measure** (Phase 14b; Phase 19b made the scale a stored attribute and "does an interval exist" a data fact): an interval is significant
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
The `options("tabxplor.design_effect")` opt-in backs **every weighted descriptive CI** (Phase 18s,
made exact in z16-ii): factor proportions AND means (cell/diff/ratio + the `color = "OR"` interval) in
`tab()`/`tab_num()`. It rides the **19th fmt field `n_eff`** = the effective sample size used for a
cell's CI; the base is `coalesce(get_n_eff, tot_n/n)`, so on basis `"n"` (`n_eff` NA) it is
byte-identical. `Σw²` (and, numerically, `Σw²x` / `Σw²x²`) is accumulated whenever the table is
WEIGHTED — on the FACTOR count scan (`plain_core`'s `w2` dcast) and the numeric one
(`num_moment_scan`) — so the aggregate has ONE shape and the BASIS, not the aggregate, decides whether
it is used. It needs the microdata weights, so `tab_counts()` on pre-aggregated counts leaves `n_eff`
NA, states basis `"n"` and says so in its footer. `tab_reg()` never reads the option: its `empirical =`
companions are ALWAYS on the weighted basis (they pass a separate effective-n into the `ci_*` engines —
no field; the displayed `n` stays the raw count), beside model CIs that are design-based (`svyglm`) and
untouched. Turning the option on is what makes a `tab()` percentage interval comparable with an
`Obs_*` column.

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

**A third theme, `"print"` (Phase 18z11) — the black-and-white publication palette.** It exists for a measurement: converted to CIE L\*, the light background ramps are 97/93/90/82 (over) and 97/93/89/82 (under), i.e. **the same greyscale ramp**, so a greyscale print loses the over/under distinction entirely (the text channel keeps magnitude but not direction). Desaturating IS that conversion, hence a separate palette rather than a filter. Its shape follows Bertin: the **ordered** variable carries magnitude, the **selective** one carries direction. Concretely, `default_print_palette()` (a CURATED literal, composed independently of `e$base` so `set_color_palette()` provably cannot alter it) gives all 8 text slots `#000000` plus one grey fill ramp `#F5F5F5/#E4E4E4/#D0D0D0/#B8B8B8` used **identically on both sides** — greyscale cannot diverge, so the fill carries its own measure's magnitude and direction is read off the cell's own typography. `tx_chrome_hex("print")` darkens only `grey` (the `grey_non_signif` colour) to `#595959`: the light `#9f9f9f` is 1.41:1 on the deepest fill, i.e. invisible.

The typography itself is the second half of the palette: **`tx_palette_faces()`**, a fact table of 8 slot renderings per (family, theme) — `list(bold, italic, underline, semantic)` — stored in `e$face` beside `e$hex` and read through the SAME accessor (`get_color_style(mode = "face", …)`). Print is over = bold, under = italic, second intensity level (slots 3-4 / 7-8) adds an underline; light/dark are `bold` on all 8 text slots and nothing on the background ones — *today's behaviour as data*. That matters twice: it makes the phase byte-identical, and it makes `tx_css_render()`'s static `.p1,…,.m4{font-weight:bold;}` rule **the CSS baseline** every theme diffs against (`tx_face_decls()`), so light/dark emit no face rules at all and print can say `font-weight:normal` on its italic slots. WHY the face is stored rather than inferred: **six** sites used to derive "this cell is bold" from "this cell has a colour hex" (the static CSS rule, `fmt_col_ann()`'s `bold`, `tab_xl`'s hard-wired `bold = TRUE`, `tab_plot`'s hex-membership test, `legend_render_line()`'s `is_bold_tok`, and the console `pillar_shaft` — the last deliberately out of scope, the console being light/dark only). All of them collapse in a palette whose every text hex is black; the palette declaring the face removed five. `semantic = TRUE` (print only) additionally emits the face as real `<b>`/`<i>`/`<u>` **markup** in the html engine and the legend, because the two destinations that matter for a publication table carry tags and nothing else: GitHub's sanitizer strips `class` and `style`, and an HTML→Word paste keeps character formatting but no stylesheet. `theme = "print"` (alias `"bw"`, canonicalised by `tx_resolve_theme()`) reaches EVERY backend including the static ones — unlike `"auto"`, it is a palette, not a render intent. `tab_css()` also emits the print rules inside an **`@media print`** block by default (`print_rules` / `options(tabxplor.print_rules)`), so a coloured page prints publication-ready unasked; under `theme = "auto"` that block must ALSO be emitted hook-prefixed, since the cascade's layers 3/4 (`body.quarto-dark .tabxplor-tab .p1`, specificity 0,3,1) out-specify a plain `.tabxplor-tab .p1` whatever the source order — and it carries `print-color-adjust: exact` on `.tx-pill`, without which every browser drops the grey fills when printing. Markdown needed **no code**: its cells are bare slot spans whose bold has always come from the stylesheet, so `tab_md(css = TRUE)` (the default) carries print for free and the pipe grid is byte-identical across themes. Rationale, measurements, rulings and the rejected alternatives (`print_marks` / `print_shaded`, the `levels`+`pmin` engine cap, a `set_color_palette()` formal): `dev/black_and_white_publication_palette.md`.

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
| `residual` | χ² adjusted standardized residual, in z units | `conf_level_to_z(c(.95, .99, .9999, 1-2e-9))` = `c(1.96, 2.58, 3.89, 6)` (mirrored) | 0 | strict |

Input notation: a plain vector of **signed / reciprocal literals** (negatives for additive, `1/x` for multiplicative; a one-sided vector auto-mirrors, a two-sided one is used as-is, an `NA` skips an intensity slot), or a **`list(over =, under =)`** of magnitudes (no mirror; omit a side to switch it off). `mk_color_scale()` + `parse_color_side()` + `intensity_slots()` build the canonical shape; `get_color_breaks()` round-trips (a readable form; `type = "all"` gives the signed engine breaks). A per-table override: `tab(color_breaks = list(...))` stores a table attribute, installed transiently at render by `push_color_breaks()` / `pop_color_breaks()` (fallback to the global option when absent/malformed).

**Phase 19c — the scales are a fact table too.** `COLOR_SCALES` (`R/tab_classes.R`) holds one row per scale: `center` / `strict` / `std` / `settable` / `default` / `null_default` (what an empty value restores — `mean_diff`'s standardized ladder) / `derive` (a NON-settable scale built from a sibling) / `legacy` + `alias` (the pre-13a argument name and the short name `get_color_breaks()` takes). Before it, a scale's identity was four name-keyed lists inside `mk_color_scale()`, a second hand-written enumeration in `default_color_scales()` and two more name maps in `set_color_breaks()`/`get_color_breaks()` — seven places per added scale — and the two DERIVED scales could not be declared at all: `log_odds` and `adj_diff_log` lived as a `switch` arm inside `fmt_color_plan()`, which is now the one-line `color_scale_resolve(key, scales)`.

### Layer 3 — The vectorised `findInterval` engine (three axes)

Coloring is decomposed into three orthogonal per-column choices: **measure** (`diff`/`ratio`/`contrib`/`or`, in the `color` attribute `[1]` = text, `[2]` = background), **channel** (text vs background), and **significance policy** (the `color_signif` attribute: `ignore`/`grey_non_signif`/`guaranteed_effect`). All feed one engine in `R/fmt_class.R`:

1. `fmt_color_plan(x, channel, color, signif)` builds a plan: the measure is the stored `color` attribute (a CLEAN measure since Phase 17d — the legacy combined strings are decoded once at the boundary by `color_decode_legacy()`, never here) and the policy is the `signif` arg or the stored `color_signif`. It then reads the measure's ONE `MEASURES` row (Phase 17d — engine facts joined the legend facts) for: the scale keys for the measure×column-type (`scale = c(std=, pct=)` picked by `std_when`: `diff` → `pct_diff`/`mean_diff`, `ratio` → `pct_ratio`/`mean_ratio`, **`or` → `odds_ratio`** [Phase 16c], `contrib` → `contrib`), the per-cell `score` (`raw` getter: `get_diff`, standardized `get_diff / sqrt(get_ref_var)` for numeric diff; `get_ratio`; `get_ctr / get_mean_contrib`), the significance source (`sig_source`) and the row gate (`gate_row`). The `gate` (from the stored `get_ci_inf`/`get_ci_sup` bounds — EXCEPT `contrib`, which has no interval and instead reads the stored `get_pvalue()`: the **ADJUSTED standardized (Haberman) residual**'s p-value, written per cell by `chi2_write_contrib()` / `contrib_adj_resid()` / `contrib_pvalue()` at chi2-time, where the margins and the grand total are in hand; direction from the sign of `get_ctr / get_mean_contrib`, so `contrib` colours under `grey_non_signif` / `guaranteed_effect` instead of colouring nothing), and the per-side `over_breaks`/`over_slots` + `under_breaks`/`under_slots` (read straight from the scale).

**Phase 19c (KEY 4) — a measure declares what it needs, what it is called and where it may go.** `MEASURES` held the *arithmetic*; the *vocabulary* was still code, in five kinds of hand-written list across five files. Nine declared fields absorbed it, so `names(MEASURES)` **is** the allow-list and adding a measure is genuinely one row:

| field | replaces |
|---|---|
| `channels` (`text` / `bg`) | the two background allow-lists that **disagreed** — `color = c("OR","adjustment")` was legal in `tab_reg()` and refused by `tab()` (**D4**) |
| `producers` (`tab` / `reg`) | the argument-boundary measure list, and the hand-written "that is a `tab_reg` measure" hint, now GENERATED from the row |
| `applies_to` (`pct` / `num`) | the `color_num` recode + `resolve_col_measures()`'s `built %in% c("OR","contrib")` tests |
| `builds` (`diff` / `or` / `contrib`) | `color_diff_OR`, `legacy_union()`'s four `if`s, and `jmv_tab3_arming()` — the jamovi tier-3 "arming class" was a fourth copy of it |
| `requires` (`always` / `gated`) | five copies of "a comparison colour needs a reference and its interval", plus contrib→`chi2`/`totrow` and `adjustment`→`empirical` |
| `auto_for` (per channel, per context) | the THREE `color = TRUE` cascades — `tab()`'s per-table one, its per-column repaint, and `tab_reg()`'s — which could therefore answer differently |
| `method` / `subject` / `caveat` | `legend_method_name()`'s three leading arms, the `or` subject literal, and the non-collapsibility caveat's `"adjustment" %in%` gate |

Read through accessors only — `measure_key()` (the ONE spelling normaliser; `COLOR_ALIASES` is the declared alias table behind it and `color_decode_legacy()`), `measure_stored()`, `measure_builds()` (whose `"contrib"` value IS "the contribution pass stamps this measure" -- Phase 19l deleted the `measure_stage()` wrapper, which said the same thing in the vocabulary of a step 19j had removed), `measure_forces()` / `measure_requires()`, `measure_applies()` / `measure_kind_keyed()`, `measure_auto()`, `measure_validate()` (the ONE validator, shared by the argument boundary `normalize_color_spec(producer = "tab")` and the storage boundary `resolve_color_channels()`). `word` became a **closure** (`function() gettext("difference")`), which deleted both the `word_i18n` flag and the hand-maintained potools anchor: the literal is extracted statically from the closure body. Two defects fell out with the fossil, both on the documented *string* spelling of `color = TRUE`: `tab_num(color = "auto", ci = "diff")` stored the composite `"after_ci"` in the `color` attribute, which `fmt_color_plan()` cannot match — the table came out **entirely uncoloured**; and any `color = "auto"` beside a `color_signif` policy **aborted**, because the unresolved sentinel reached `set_color()`.

**Phase 18z4 — the two readings of `contrib`, and the one `MEASURES` accessor.** `contrib` is the only measure whose reading changes with the significance policy, because a contribution has no confidence interval to floor. The divergence is a **`guar` override list inside its `MEASURES` row**, never a `switch`: `measure_facts(measure, policy)` returns the row with `guar` folded in under `guaranteed_effect`, and it is now the ONLY way `MEASURES` is read (1 call in `fmt_color_plan()`, 5 in the legend, each passing `plan$policy` / `spec$policy`) — so the colour plan and the legend describing it cannot diverge. Under `ignore` / `grey_non_signif` the score is the RELATIVE contribution (`ctr / mean_contrib`, `contrib` scale, in multiples of the mean cell — the correspondence-analysis reading, intrinsically relative to its table); under `guaranteed_effect` it is the ABSOLUTE residual `fmt_resid(x)` on the 7th scale `zscore` (named `residual` until Phase 18z8 -- it is a z scale, and a second measure could want it), whose breaks `offset_guaranteed_breaks(breaks, center, origin = z(conf_level))` re-anchors at the significance threshold (the `break_origin = "threshold"` fact). That keeps the policy's invariant — a cell is coloured iff `|z| > z(conf_level)` — while the printed thresholds stay real |z| values a reader can name, comparable between tables; the legend therefore drops the "after subtracting the margin of error" phrasing for this measure (`guar_abs`, resolved per channel in `legend_resolve_spec()`). **`fmt_resid()`** derives the residual from `pvalue` + `sign(ctr)` — deliberately NOT a field of its own, since the p-value determines `|z|` exactly (`-qnorm(p/2)`; `qnorm(1 - p/2)` would saturate to `Inf` for every `|z| > 8.2`). It also backs the `resid` **display token** (`DISPLAY_TOKENS`, `settable = FALSE`: `get_num()` has an arm, `set_num()` deliberately does not; a `{}` composite keeps its p-value for that token instead of blanking it) and the html tooltip's "std. residual" fragment. Statistics: `contrib_adj_resid()` (R/tab.R) computes `(o/N - e_f) * sqrt(n_base) / sqrt(e_f (1-p_i)(1-p_j))` from the same weighted margins `var_contrib_ctr_signed()` uses (shared `contrib_zero_inner()` prologue), where **`n_base` is the unweighted `n` — or, on any basis but `"n"`, that `n` divided by Rao-Scott's mean generalized design effect δ̄, the one the table's own omnibus row reports — never the weighted total**: weighted estimate, unweighted or effective base, the same rule as every CI in the package. Phase 18z16-iv (W-B) replaced the grand cell's own `n_eff` there: that cell's proportion is 1, so its design variance is 0 and it ALWAYS took the degenerate weights-only `B²/S`, at every basis — a stratified+clustered table and a flat one gave residuals identical to the last digit while their cell intervals differed (measured |z| overstated ×2.52 on a cluster-level row_var), and even at basis `"weights"` the residual's base and the omnibus test's implied one were 2.5 % apart. δ̄ arrives from `svy_omnibus_grid()` through the leaf's `deff` argument → `chi2_write_contrib(deff =)`, keyed onto the built table's own groups by `svy_deff_lookup()` (the `svy_key_chr()` convention, no second key scheme); it is NULL at basis `"n"`, so the raw-n correspondence-analysis reading stands by construction, and a missing δ̄ falls through to the old ladder rather than to a wrong number. ONE base per table (z16-iii deleted the `type %in% c("n","all","all_tabs")` guess), which is what makes a counts table and a percentage table of the same data give IDENTICAL residuals. The CONTRIBUTION itself stays weighted and is identical at every basis (it estimates the population table's inertia decomposition), so the correspondence-analysis reading is invariant by construction. A cell whose expected count falls below 1 gets no residual (the normal approximation fails there). On an unweighted table this reduces exactly to `stats::chisq.test()$stdres`. `leaf_wide_pct()` is also called on the `pct = "no"` path (with the `"all"` base) when the basis asks for one, so a COUNTS table — where `color = TRUE`/`"auto"` picks `contrib` — carries an `n_eff` at all. Rationale, measurements and rejected alternatives: `dev/chi2_cell_residuals_and_contributions.md`.
2. `fmt_color_slots(x, plan)` folds `score` to a magnitude around `center`, then `findInterval()` **per direction** (`over_breaks` for over-cells, `under_breaks` for under-cells) → level → palette slot (0 = uncolored, **1..4 = over intensities, 5..8 = under**), zeroing ungated cells. The former in-text ×2 / slot-11 override is gone (the ×2 rule is now a 1-break `pct_ratio` scale on the background channel). **`guaranteed_effect` computes `score` = the guaranteed (CI-floor) magnitude ON THE MEASURE'S OWN SCALE** so the fold's `center` matches: for `diff` the floor is the stored difference bound (centre 0); for `or` the native OR bound (centre 1); for `ratio` — which has no native CI — the shared diff floor is converted to a guaranteed ratio `1 + (get_ratio - 1) * (guar_diff / get_diff)` (centre 1). Feeding the raw diff bound into a centre-1 fold was the "ratio floods /4" bug.
3. `fmt_color_channels(x)` → `list(text_slot, bg_slot)`, via `resolve_color_channel_plans(x)` — the shared arbiter (also used by `legend_specs()`, so cells and legend agree). Phase 16c: `fmt_color_plan()` flags a plan `degenerate` when the policy is `guaranteed_effect` and the scale has a single break per side (which `offset_guaranteed_breaks()` collapses to the neutral → one flat "×1" fill). The arbiter drops a degenerate channel and its legend line, but **never the last channel** (a degenerate text channel survives when no non-degenerate background does — "keep the first channel"). The single-channel golden `fmt_get_color_code()` is not arbitrated (it always renders the text measure).

Every consumer maps `(text_slot, bg_slot)` to colour the same way: `pillar_shaft.tabxplor_fmt()` (console, the reference two-channel consumer), `fmt_get_color_code()` (single-channel, the golden), the shared `fmt_channel_codes()` helper (text + bg hex, used by `tab_kable`/`tab_plot`/`tab_xl`), and `tab_color_legend()` (which reads the same scales, so legend and cells never disagree). The old combined strings (`"diff_ci"`/`"after_ci"`/`"ci"`) are decoded to `(measure = "diff", policy)` ONCE at the argument / storage boundary by `color_decode_legacy()` (in `normalize_color_spec()`, and in `tab_ci()` for the deprecated step path) — the engine never re-parses them, and `"ci"` folds into `after_ci` (the old `single0` one-shade mode is retired).

The `color`/`color_signif` **arguments** are parsed once at the front by `normalize_color_spec()` (`R/tab.R`), then reconciled with `ci` by **`ci_disable_signif()`** (D28's one rule: `ci = "cell"`/`"no"` informs and disables `stars`/`color_signif` — applied at the boundary too, because the STORED policy attribute comes from this spec, not from the resolver). Since Phase 19i both steps happen inside the ONE argument boundary **`tab_resolve_common_args()`** (`R/tab-resolve.R`), which every crosstab producer calls — so `tab_counts()`, which built and finalised a spec without ever applying D28, cannot store a gate it does not apply, and `tab_num()`, which handed the resolver the RAW `color_signif` instead of the decoded `color_spec$signif`, no longer drops the policy half of a composite colour; the built table is then finalised by the shared **`finalize_color_tail(result, color_spec, color_breaks, display)`** — `finalize_color_spec()` → `tab_apply_display()` → `set_color_breaks_attr()` — the ONE wrapper tail `tab()`, `tab_many()`, `tab_num()` and `tab_counts()` all run (and, since the 19d tail, `tab_plain()`/`tab_num()`'s own `display =` goes through the same `tab_apply_display()` → `display_write_col()`, the single per-column template writer that `set_display(col, "num_ci")` also calls) (so none can drift; `tab()` keeps its later `output_kable` / `as_tabxplor_tabs` steps).

**Colour legend (Phase 13b, `R/fmt_class.R`).** `tab_color_legend(x, medium = c("console","html","md","runs","plain"), style = c("terse","prose"), lang=)` builds the legend as `legend_specs(x)` (per col_var group: measure/breaks/ref/method/policy/shade names + regression effect word) → `legend_tokens_terse`/`_prose` (a **token stream**: plain-text | coloured break-word tokens) → `legend_render_line(medium)` (console ANSI via `cli` / inline html span / md pandoc span / plain / **`"runs"`** = the token stream returned unrendered as `list(text, color, bold)`). Console = terse, exports = prose; the break-word colours come from the same 8-slot palette the cells use, so they can't disagree.

`"runs"` (Phase 14c) is the medium for consumers that draw the legend as coloured TEXT and cannot fill: `tab_xl()` (an `openxlsx2::fmt_txt` rich-text cell) and `tab_plot()` (`ggpubr::table_cell_font`). Both therefore draw a background break-word from the `bg_legend` palette. It replaced `"excel"`: `tab_plot()` used to recover the legend by scraping regexes back out of the *html* rendering, which had silently stopped matching (Phase 13b replaced kableExtra's `color: rgba(...)` spans with inline hex) — every token rendered as a raw html fragment in black.

**Phase 16d legend/footer changes.** `legend_specs()` now builds rich specs WITHOUT a `sig`, then (reg tables only) `legend_canonicalise_reg()` reconciles the empirical + model columns of each col_var — sharing one reference label and neutralising the model's additive effect word ("AME"/`β` → the neutral "cells", only when an empirical additive sibling exists, so a no-empirical table keeps its word) — and finally derives the `sig` **without `role`**, so an empirical companion folds into its model sibling under one legend line. `tab_color_legend()` shows a column-name prefix whenever a group is role-MIXED (an emp+model merge) or a col_var spans several lines; `legend_name_list()` normalises those names (undo the html-path `<br>`/U+202F wrap, protect intra-name spaces with U+00A0 so no medium re-breaks a name) and caps them at six + `… +N vars`. `contrib` is no longer lumped with `ratio` in `legend_break_label()` — it reads `×N` on BOTH sides ("×N the mean contribution"), direction by colour. A numeric-mean/coef diff carries `is_pct` + `is_std` (from the mean_diff scale's `std` flag, the same `color_scales()` source the cells read): factor pct → `×100`/"points", standardized → "SD", raw custom breaks → the bare value. Two new plain (uncoloured) footer helpers ride every footer site: `tab_weight_line()` ("Weighted by `<wt>`." — the weight NAME persisted in the `vars` attribute for a crosstab (only when weighted, so unweighted tables don't churn) / `reg_meta$wt` for a regression) OPENS the footer, and `tab_stars_legend()` (the significance-stars legend, gated by `fmt_stars_applicable()` = not a `contrib` residual-p column) CLOSES the colour block. (Phase 16e unified how these reach the backends — see below.)

Break-words carry a per-channel weight (Phase 14c + Phase g): a **text**-colour break-word is bold in every medium (runs `bold = TRUE`, console `cli::style_bold`, html `font-weight:bold` inline, md `**[+5]{.p1}**`), a **background**-colour break-word is PLAIN (it mirrors a filled cell, which a fill alone does not bold) — the bold decision in `legend_render_line()` is `(coloured & channel != "bg") | token$b`. Variable NAMES carry `token$b = TRUE`, so they are bold everywhere. The stars token is `esc`-flagged so the md renderer backslash-escapes its `*` and the **html renderer entity-encodes it (`&#42;`, Phase 18x2)**: a knitted page's raw-html block goes THROUGH pandoc (Rmd → md → html on pkgdown/Quarto), whose markdown-in-html parsing paired the legend's `***: ... **: ... *:` runs as emphasis and swallowed the stars — an entity renders as `*` in every browser but is plain text to pandoc, the same round-trip that keeps the in-cell stars alive (unmatched runs, pandoc re-escapes them). Viewer/jamovi/standalone never re-parse, so they were unaffected either way (user subtext is left raw). Inline/markup rather than left to `.p*`, because it must also reach the kableExtra path (which ships no stylesheet of ours). Distinct from `tx_css_rules()`'s `.p1..m4{font-weight:bold;}`, which exists for the **cells**: `tab_export_prep()`'s `bold = !is.na(text_hex) | ref_alltot` already bolds every text-coloured cell in kableExtra and the html engine, and the stylesheet is the only way `tab_md()`'s bare `[42%]{.p2}` spans can say it. The prose is **translatable** via `gettext`/`gettextf` (domain `R-tabxplor`, French in `po/R-fr.po` → compiled `inst/po/fr/LC_MESSAGES/R-tabxplor.mo`); `lang` (`"en"`/`"fr"`, or auto from the R/OS locale) sets the `LANGUAGE` env for the build (`with_legend_lang()`). **Phase 18w** filled the French catalogue and extended gettext coverage beyond the colour legend to the whole below-table surface: the **regression wording** (`reg_family_display_name`/`reg_model_note`/`reg_model_line[s]`/`reg_title` in `R/tab_reg.R` — full `gettextf` templates so French controls the `« : ; »` typography, notation OR/IRR/β kept English; `reg_model_lines(x, lang)` runs under `with_legend_lang`, the caption `reg_title(meta, lang)` follows the ambient locale via `with_legend_lang(NULL)`), the **`test = TRUE` summary + GOF labels** (`test_pvalue_descriptor`/`test_es_measure`/`reg_footer_spec` in `R/tab-test-display.R`, ambient locale) and the **HTML tooltips** (word labels in `tab_kable_print_tooltip`, ambient locale; pure notation `ci`/`OR`/`n`/`sd` left English). English is byte-identical (`gettext("X")` returns the msgid under the en locale). Three i18n gotchas are handled: (a) potools cannot see `gettext(variable)`, so a string built dynamically would never be extracted — the package answers this by making such strings **closures** (`MEASURES$word`, `CI_METHOD_WORDED`, `REG_ESTIMANDS`' notes: the literal inside the closure body IS statically visible), which is why the hand-written `if (FALSE) c(gettext(...))` anchor beside `legend_measure_word()` was deleted in Phase 19l. Exactly ONE anchor survives and must not be removed — `reg_check_msgid_anchor()` (`R/reg-assumptions.R`), holding the 17 `REG_CHECKS` nouns and instruments, which are bare strings `gettext()`'d at render; (b) potools extracts R string tokens verbatim, so a non-ASCII msgid written as a `\uXXXX` escape lands in the catalogue as a literal backslash-u that R's runtime `gettext` never matches — `dev/update_translations.R` normalises those to real UTF-8 after extraction; (c) **`lang = "fr"` is a no-op wherever `LC_MESSAGES` is `C`/`POSIX`** — `with_legend_lang()` sets only the `LANGUAGE` env var, and GNU gettext ignores `LANGUAGE` entirely under the C locale. This is a property of the environment, not of tabxplor (macOS/Windows libintl honours `LANGUAGE` regardless), and it is not worked around: promoting `LC_MESSAGES` out of C would need a real locale the machine may not have installed. Practical scope: a user in any ordinary locale gets French; a bare container / minimal server / `R CMD check` on Linux / **the CRAN farm** silently gets English. The test suite therefore guards every French assertion with `skip_if_no_gettext()` (`tests/testthat/helper-i18n.R`: catalogue compiled → `capabilities("NLS")` → a real `gettext()` round-trip, so the platforms that *can* translate still do), and tests each i18n feature twice — an UNGUARDED English block (the guard-rail proving English stays byte-English, so the goldens never move) plus a GUARDED French one. Simulate CI with `LC_ALL=C.UTF-8` (not plain `C`, which is harsher than any runner). The sanctioned workflow is `Rscript dev/update_translations.R` (extract → normalise → merge → compile); terminology lives in `dev/french_glossary.md`; the pkgdown site is **ONE English site** (`_pkgdown.yml` + `dev/build_site.R`; the former bilingual config-swap and its language toggle were removed — R ships no bilingual `.Rd`, so the reference pages were always English anyway). The French translations of the three vignettes are ordinary pkgdown **articles** (`vignettes/articles/*-fr.Rmd`, `.Rbuildignore`'d so web-only, never on CRAN), listed in the navbar's "En français" group and rendering French legends via `options(tabxplor.lang = "fr")` **and** `Sys.setenv(LANGUAGE = "fr")` in their setup chunk — both levers, because the test-summary and model-fit row labels resolve through gettext on the ambient locale, which the option does not reach. **Phase 19n** also made the `en@quot` pseudo-locale DERIVED (`tools:::en_quote()` on the `.pot`, step 5 of `dev/update_translations.R`): it has no translator catalogue, so it had been generated once and left to rot at 136 of 235 msgids. The **reference index** is priority-tiered (the two headline `tab()`/`tab_reg()` first, then everyday functions — variants, regression shortcuts/plots, export, options/data, jamovi — then a clearly separated "Programming with tabxplor" tier: engine/reshape, the `fmt` type, the superseded step pipeline, helpers); group descriptions name the wrapper relationships (`tab_html()`/`tab_md()`/`tab_xl()`/`tab_plot()` are the per-format exporters `tab_export()` dispatches to). The **~80 S3 methods** (dplyr verbs, fmt accessors, vctrs/pillar/print/operator methods) carry `@keywords internal` in their roxygen, so pkgdown drops them from the index entirely (they still export — `@keywords internal` is doc-only, `NAMESPACE` unaffected — and remain reachable by `?method`); users call the generics (`get_scale()`, `mutate()`), never the dotted methods. The CI method + confidence level are named from the COLUMN's own stored **`ci_method`** and `conf_level` attributes (Phase 19b): one lookup in the declared `CI_METHOD_LABELS` table, keyed on the engine the producer stamped where the interval was computed. Before that the method rode a table-wide `meta$ci_settings` vector and the legend picked a slot back out of it BY MEASURE, through an eight-branch chain — an `est_scale_key()` dispatch written a second time in a third vocabulary, and one that could name a method the bounds were never built with (D8: a `ci = "cell"` mean's one-sample interval announced as "Welch t", a poisson crude IRR's Katz interval announced as Wald). Two labels still consult the effect word, because an OR, an IRR and an RR are the same interval on the same log scale (`wald_log`, `katz`). Shade names ("blue"/"yellow-red") appear only for the default palette (`legend_shade_names()`); a custom palette degrades to generic wording. Callers: console print (`tbl_format_footer`), `tab_kable`/`tab_md`/`tab_xl`/`tab_plot` (each with a `lang` arg; Excel writes coloured rich-text legend cells via `xlb_write_richtext`).

**Phase 18z5 — colouring the gap between a modelled and an observed effect.** `tab_reg(empirical = TRUE)` already prints the crude effect beside the adjusted one; two new measures colour how far apart they are. The engine takes ONE fmt column, so a cross-column comparison must resolve at build time into a per-cell field (the rule `or` has always followed) — hence the **20th field `obs`**: "the value this cell's estimate is compared to", on the cell's own scale. Nothing is recomputed: `reg_empirical_columns()` now returns `list(cols, effect)`, the effect vector being the local its shape was built from, and `reg_build()` writes it into the model columns with `set_obs()`. A single dependent has ONE crude block serving every model column — which is what makes `adjustment` work in model-COMPARISON mode; several dependents map each column to its fit via `fit_first_idx`/`fit_ncol`. `obs` stays NA on the Constant, numeric predictors, multinomial/ordinal and every cross-table, so those cells are uncoloured by construction. **`color = "adjustment"`** compares to the observed effect (and turns `empirical = TRUE` on, as `color = "contrib"` forces `chi2`); **`color = "between_groups"`** compares to the first `split_var` group, written by `reg_write_group_obs()` at the ONE point the groups are parallel, positionally-addressable tibbles — immediately after the split recursion's `parts` closes, before `vec_rbind`/`group_by`/`reg_spread_models` — so both the stacked and the spread output work from one pass; rows are matched by `reg_skel_key(var, level)`, not by position, because the compound-formula path builds a per-group skeleton (measured: 5 rows vs 7, different order). The existing reference machinery cannot express this: `fmt_broadcast_last()` groups by runs of `in_refrow`, which cross the split boundary (measured: one group's rows receive another's intercept). Both measures are `MEASURES` rows over ONE `fmt_adjustment_score()`, and both may ride the **background** channel — `color = c("OR", "adjustment")` is the headline reading (effect size in the text, what adjustment did to it in the fill). They share the two new scales **`adj_ratio`** (`×1.1/1.25/1.5/2`, the epidemiological 10 % change-in-estimate rule) and **`adj_diff`** (`±2/5/10/20` points, ABSOLUTE in the effect's own units — a relative change explodes near the null: measured −60 % for a +0.016 shift on a −0.026 crude AME), selected by a `scale_from = "gap"` fact keyed on the ESTIMATE's own stored scale rather than on the column kind (`Model_OR` and `Model_AME` used to be indistinguishable, both `type = "row"`). **The sign is away-from vs toward the NULL** (`|log est| − |log obs|`, or `|est| − |obs|`), never raw up/down: otherwise a protective effect (OR < 1) attenuated toward 1 lands on the opposite pole from a risky one attenuated toward 1, and the two halves of the palette stop meaning anything. **`color_signif` did not apply in z5**: a new optional `force_policy` fact, read through **`measure_policy()`** (the twin of `measure_facts()`, called by the plan AND the legend so a neutralised measure cannot be coloured under one policy while described under another), pinned both to `ignore`. Phase 18z8 removed the override from `between_groups` (below); `adjustment` keeps it, its two estimates being fitted on the SAME rows, where a valid gap test needs their joint variance (`dev/model_vs_observed_gap_test.md` §3). The legend gained a **per-channel reference phrase** (`measure_own_ref()`: these are the only measures whose baseline is another column, so the scalar `spec$ref_phrase` — resolved for the text measure — would describe the wrong comparison on the background) and, on the odds-ratio path only, a one-sentence **non-collapsibility caveat**: adjusting an OR for an outcome-predictive covariate moves it away from 1 even with zero confounding (measured +7.9 % on a simulation where the covariate is INDEPENDENT of the exposure, against +0.26 % for the risk ratio and ~0 for the AME) — about the size of the first colour step, so the sentence names the collapsible alternatives (`effect = "ame"`/`"ame_ratio"`, `family = "poisson"` on a binary outcome, a gaussian β). The gap is also readable as a number: the **`obs` display token** (a real stored field, so unlike the derived `resid` it round-trips — `get_num()` reads it and `set_num()` writes it; `set_display(t, "{or} (obs {obs})")`, since `tab_reg` has no `display` argument) and an html tooltip fragment whose label follows the column's own stored measure. Rationale, measurements and rejected alternatives (CI overlap: measured correlation 0.944 between crude and adjusted, so the non-overlap criterion needs 11.6 true SEs — an effective α of ~3·10⁻³¹): `dev/model_vs_observed_effect_colour.md`.

**Phase 18z8 -- a significance test for the gap.** The two z5 measures scored the SIZE of a gap and said honestly that it had no test. Both have one now, from DIFFERENT mathematics: `between_groups` compares two DISJOINT groups (Phase A, below), `adjustment` two estimates fitted on the SAME rows (Phase B, `R/reg-influence.R`). Phase A needed no new statistics: the two `tab_vars` groups are DISJOINT samples, so `SE(gap) = sqrt(SE_A² + SE_B²)` is exact (Altman & Bland 2003) and both standard errors are recoverable from the Wald intervals the table already prints -- which is what makes the test and those intervals impossible to disagree. The **21st field `gap_se`** carries it, on the estimate's own test scale (log-ratio for `or`/`ratio`, plain difference for `diff`); `reg_write_group_obs()` became **`reg_write_group_gap()`** and writes it beside `obs` at the same single point, dividing each printed half-width by `z(conf_level)` (`reg_gap_se_of()`: exact on the fixed-dispersion path, ≤0.1 % conservative where the bounds were built on a t reference -- §4.5 measured that a t reference changes the gap test by nothing at any n; `method = "profile"` writes no SE, its bounds not being `est ± crit·se`).

The colour engine absorbed it with **zero new branches**, via one new `MEASURES` fact: **`bounds`**, a closure returning the interval the two significance policies read (`measure_facts()` fills the default `fmt_stored_bounds` = the stored `ci_inf`/`ci_sup`, so every other measure and every future one needs no line). `fmt_color_plan()` binds `bd <- md$bounds(x)` once and uses it for both the `sig_pos`/`sig_neg` gate and the `guaranteed_effect` floor. The subtlety that makes this work: the score's sign is the NULL DIRECTION while a raw gap interval is signed up/down, and the two disagree for a protective effect -- so **`fmt_gap_bounds()` returns the interval OF THE SCORE**, i.e. the interval of `|gap|` re-folded with the score's own sign. A gap interval excluding 0 then puts both bounds strictly on the score's side (so `grey_non_signif`'s direction match works unchanged), one covering 0 pins the near bound exactly at the neutral (not significant), and the bound nearest the neutral IS the guaranteed gap, already signed (`guaranteed_effect` = "the effects differ by at least ×1.1"). Four small helpers over ONE decomposition `fmt_gap_parts()` (mult / est / obs / ok / null-sign): `fmt_adjustment_score()` (rewritten to read it, arithmetic untouched), `fmt_gap_raw()`, `fmt_gap_bounds()`, `fmt_gap_p()` -- the last two feeding the html tooltip's `gap: ×1.05 [×1.01; ×1.09], p = 0.5%` fragment, which is where three numbers belong (no display token was added). The legend's **interval NAME became per-channel** too (`legend_method_phrase(spec, lang, measure)`, resolved in `legend_resolve_spec()`'s `chan()` beside the reference phrase): a gap measure on the background runs its own test, so its "after subtracting the margin of error (...)" tail must not borrow the text channel's model interval; and one extra clause names the background's rule when the two channels test different things.

**The aggregated companion** (`stats = c(..., "interaction")`, automatic under `color = "between_groups"`) answers the same question ONCE per predictor, free of the per-cell multiplicity: one extra pooled fit `y ~ (predictors) * split_var` through the new internal `reg_fit(cross =)` (which inherits the binary prep, the grouped-binomial `cbind`, the family objects, the `rr` → `svyglm` route and the design resolution -- the `formula =` escape hatch deliberately disables the first two), then `drop1(scope = <the fit's OWN interaction term labels>)` unweighted (LR, or F for gaussian/quasi) / `survey::regTermTest()` per predictor weighted, mirroring `reg_compare_rows()`'s `use_f`/`use_wald` rule so the two extra-fit footer tests cannot claim different things. ⚠ The term labels must be taken from the fit verbatim: `terms()` orders the parts of an interaction by the variable's position in the formula, so a hand-built `age:party3` comes back as `party3:age` and `drop1()` rejects the scope. It is a table-wide **footer LINE**, not footer rows -- every footer row is keyed to exactly one model column, `reg_spread_models()` re-keys per split group, and `reg_footer_spec()` (a slice of `TEST_ROWS` since Phase 20c) is a fixed discriminator→label list that cannot carry one label per predictor. So the rows (`interact_lr`/`interact_f`/`interact_wald`, `row_var` = the predictor, `col_var` = the fit's first column) stay pure data, deliberately ABSENT from `reg_footer_spec()` -- both row consumers filter on it, so the existing GOF footer is row-for-row unchanged -- and `reg_interaction_lines()` renders them through `tab_footer_streams()` beside the weight / `Model:` lines, reaching every backend from one producer. Three consequences elsewhere: `is_reg_footer()` widens to the interaction discriminators (a `stats = FALSE` table carrying only them is still a reg table), `reg_spread_models()` re-keys only the GOF rows, and `reg_footer_lines()` -- which drops `test` for idempotency -- carries the interaction rows through in `attrs` (re-entry stays a no-op: with only those left, its own filter is empty and it returns early).

**Phase B -- `adjustment`, the influence functions (`R/reg-influence.R`, ~220 L).** The model and its observed counterpart solve estimating equations on the SAME observations, so they are correlated (measured r = 0.52-0.90) and no arithmetic on the two printed intervals recovers the variance of their difference: the naive `sqrt(se1^2 + se2^2)` is 2-4x too large and Hausman's `Var(crude) - Var(adj)` goes NEGATIVE for logistic. The only quantity that carries the covariance is `Var(sum_i (IF_i^adj - IF_i^crude))` -- seemingly-unrelated estimation (Weesie 1999; Mize, Doan & Long 2019). The new module is pure matrix math over `stats` + `survey`, the package's ONLY caller of `survey::svyrecvar()`, and every function returns NULL rather than a wrong number when its inputs do not support the computation. FOUR facts make it small: (1) ONE influence formula serves `lm`/`glm`/`svyglm` -- `U = X*(W*r)`, `A = X'WX`, `IF = U A^-1` with `W = fit$weights` (the IRLS working weights, already carrying the prior/design weights) and `r = residuals(type = "working")` -- verified bit-identical to `attr(svyglm(..., influence = TRUE), "influence")` (5e-17), so nothing ever passes `influence = TRUE`; (2) `reg_if_from_parts()` returns a CLOSURE over the contrast, not the matrix, because `U` is a pure ROW scaling of `X`, so `(U %*% c)_i == (W_i r_i)*(X %*% c)_i` (1.7e-18) and every term costs one length-n allocation -- the second `n x p` matrix is never built (⚠ peak memory is the ONE `model.matrix(fit)`, ~2 GB at n = 5M, p = 50); (3) every `Obs_*` effect IS the coefficient of a saturated one-factor GLM, so `reg_crude_if_maker()` is a closed-form two-cell expression with no fit at all (21x cheaper; for the unweighted binomial its SE is exactly the Woolf interval the `Obs_OR` column prints); (4) with a design the variance is `survey::svyrecvar()` on the difference vector, which reproduces `SE(svyglm)` exactly -- strata, clusters and FPC for free -- read off `fit$survey.design` (no `reg_fit()` signature change; a `svyrep.design` needs `withReplicates` and degrades). `reg_ame_if_maker()` adds the two-term marginal influence function for `effect = "ame"` / `"ame_ratio"` (`IF = wt_i(g_i - AME) + IF_beta %*% G`, and its log-ratio twin), reproducing `marginaleffects`' own SE to 5 digits -- the small excess being the empirical-averaging term `marginaleffects` omits by holding the covariates fixed.

`reg_gap_se_columns()` (R/tab_reg.R) is the gate and the loop; `set_obs_if()` writes `obs` and `gap_se` together at the ONE point z5 already wrote `obs`. The gate is six facts that already exist, and it returns NULL rather than a partial column, because `fmt_gap_force_policy` reads an all-NA `gap_se` as "no test here": the colour was asked for (`sp$color` -- the test costs ~1/8 of a fit); a crude twin exists (the REG_EMPIRICAL SHAPE ROW, which `two()` now returns beside `cols`/`effect`, and which gained the **`link`** fact driving `g'(mu)`: it sits on the shape, not the family, because a binomial model's crude twin is logit by default, IDENTITY under `effect = "ame"` and LOG under `"ame_ratio"`); a fitted object survives (NULL on jamovi's digest path); **`reg_same_estimand()`** -- the shape's `scale` equals the column's, which also closes a z5 defect where `effect = "ame"` + poisson paired an additive count AME with the crude rate RATIO and wrote that ratio into `obs`; the two frames have equal `nrow`, which PROVES row identity (the crude frame's var set is a subset of the model's and both are `reg_complete_frame()` subsets in order) and degrades a comparison model fitted on more rows under the default `na = "drop_by_model"`; and **`reg_estimand_collapsible()`** -- maintainer ruling Q1(b) -- which excludes a CONDITIONAL ODDS RATIO (`effect == "coefficient"` on a `reg_fam_prob()` family, `exponentiate` irrelevant), where the gap moves under adjustment with zero confounding and the test would read "significant" everywhere at survey sizes (measured rejection 1.000 at n = 32000; the same comparison on the collapsible RR scale holds its nominal 0.05).

**Phase 18z9 — the same test on CONTINUOUS predictor rows.** The loop gained a numeric arm. The model
leg is unchanged (a numeric predictor's skeleton `term` IS the variable name); the crude leg cannot be
`reg_crude_if_maker()`, whose closed form is cell-indicator arithmetic and needs levels — it is
`reg_coef_if_maker()` on the row's own **univariable fit**, kept on the crude record only when a spec
asked for `color = "adjustment"` (a build-time local that never reaches the jamovi `.fit_cache`, whose
persisted raw fits were Phase o's freeze). Both legs are then the same machinery on two fits solved over
the same rows, so no new mathematics: verified equal to a hand-stacked influence-function computation to
1e-12. `multiplier` scales the result by `|k|` — the influence functions are native-scale while the
stored estimate and `obs` are already scaled and `fmt_gap_raw()` reads the stored values, so the gap is
`k(b_model - b_crude)` on both the additive and the log branch and the resulting *z* is invariant.
(`reg_gap_se_of()` / `between_groups` needs no such handling: it RECOVERS the SE from the printed,
already-scaled interval.) `reg_ame_if_maker()`'s counterfactual gained a numeric arm — `(level, ref)` are
SHIFTS on the observed *x*, so `(k, 0)` is the *k*-unit forward difference the AME columns display; it
previously coerced the column to character. That arm is not optional: `reg_estimand_collapsible()`
already refuses the binomial COEFFICIENT path, so the AME arm is where a binomial continuous-predictor
gap test actually lives. Measured, the IF-based SE is ~6x smaller than naive quadrature — the
correlated-estimator property. ⚠ **Partial coverage stays a known reading**: once ANY row of a column
carries a `gap_se`, a row without one gets NA bounds and `fmt_color_plan()` coerces those to "not
significant", so under `grey_non_signif` it is GREYED rather than keeping its descriptive colour. That
predates z9 (a 0 %/100 % crude cell already yields no SE) and coverage is complete in every case
measured, but it is now more reachable; the honest fix would be a per-row `force_policy`.

`force_policy` therefore did NOT disappear as the study forecast -- it became a PREDICATE ON THE COLUMN, **`fmt_gap_force_policy(x)`** (an all-NA `gap_se` -> `"ignore"`), carried by BOTH gap measures and applied by `measure_policy(measure, policy, x)`, whose one call site is `fmt_color_plan()` (the legend reads `plan$policy`, so it inherits the resolution). That is what implements Q1(b) with no 12th column attribute and no display-string matching, and it fixed a live Phase-A hole: `between_groups` under `method = "profile"` writes no SE and was greying the whole column instead of falling back to the descriptive reading. Two legend consequences: `legend_resolve_spec()`'s `chan()` now resolves each channel under ITS OWN policy (they can genuinely differ -- an OR text channel greying by its Wald interval, an `adjustment` background with no test), and the "Background: the same rule..." clause gates on `spec$plan_bg$policy` instead of the text channel's, retiring a sentence that had been claiming a greying rule that was never applied. ⚠ The influence-function SE is the ROBUST (sandwich / design-based) variance on both legs; it equals the printed crude interval exactly only for the unweighted binomial (Woolf), and differs by a few percent from the pooled-Student `Obs_diff` and quasi-Poisson `Obs_IRR` brackets -- correct for a gap between two differently-specified estimators, and documented. §6's rebuild-from-`(data, coef)` was deliberately NOT built: jamovi's regression `color` is a checkbox, so one clause on the `reref` gate (`!("adjustment" %in% color)`) sends the measure down the refit path instead of adding a second encoding of `reg_fit()`'s model frame for no caller.

**Phase 18z14-iii — one row space.** `[` does not drop rows on a **calibrated or PPS** design: `survey` keeps all *n* and sets `prob = Inf`. So `svy_domain_design()` pads the fit's design back to full length, `svyglm` keeps those zero-weight rows in `model.matrix()`, and an influence leg built on the complete-case frame is SHORTER than its counterpart — measured 380 against 400. The closed-form crude leg then failed the length guard (the gap test vanished, silently, on every calibrated design with a missing value), and worse, `reg_ame_if_maker()`'s own `emp + delta` **recycled**: a wrong number with only a warning. **`reg_if_align(v, n, des_rows)`** scatters a frame-length leg into the design's row space over the extracted `svy_row_at()` (shared with `svy_var_prep()`), which is exact rather than approximate because the padded rows carry design weight 0. Three call sites: the closed-form crude leg in `reg_gap_se_columns()`, and the `emp` term of both AME makers. Two matching row-space fixes live in `tab_reg.R`: `reg_resolve_design()` maps its complete-case mask through `.svy_row` so every subset goes into the ORIGINAL design, and the `split_var` branch no longer re-subsets the design at all — ⚠ it used to hand a per-group design through `utils::modifyList()`, which **recurses into list elements**, and a `survey.design2` is a list whose `$variables` is a data.frame, so the two designs were merged column by column (an error whenever the groups were unequal, silent recycling when they divided); on a calibrated design the group-local positions then weighted the wrong respondents (measured OR `1/2.17` against `svyglm`'s `3.48`).

The **`at = "reference"` estimand mismatch** was fixed in passing (a z5 defect): there the model cell is a marginal effect AT the reference profile while the crude companion stays marginal over the whole sample, so no `obs` is written at all and `tab_reg()` says why once. The stratum-restricted crude effect would match the estimand but answers a different question (model FIT at one profile, not confounding) on a few percent of the rows. Rationale, measurements and rejected alternatives (CI overlap; Hausman's subtraction, which goes NEGATIVE for logistic): `dev/model_vs_observed_gap_test.md`.

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
`new_col_var` transition index). Byte-identical to the
pre-Phase-10d exporters (golden/color-golden/md-snapshot/A/B locked). `tab_plot()` is soft-deprecated
(`lifecycle` superseded) here.

**Phase 14i** adds the shared **variable-NAME model**, and moves both of its drops into the prep so no backend has to know the argument exists. `roles$label_cols` + `roles$label_runs` (from `tab_label_runs()`, per column `list(show, span)`) are the leading factor columns whose value repeats down a block — the synthetic `row_var` column when `compacted`, else the kept `tab_vars`, never both (`tab_compact()` bails on tab_vars). ONE run model, four consumers: md blanks the repeats, the html engine `rowspan`s the run, Excel merges it (`xlb_merge`, and blanks the written repeats — Excel keeps only a merge's top-left value), `tab_plot` blanks. `roles$var_name_col` is the name-VALUED subset (the merged table's `row_var` column, whose values ARE variable names): it alone is dropped by `var_names`, has its literal `"row_var"` header blanked unconditionally in `tab_col_var_header()` (a bug fix), renders **vertically** (`.tx-vname` = `writing-mode:vertical-rl` + `rotate(180deg)`, NOT the experimental `sideways-lr`; Excel `text_rotation = 90` + a narrow column), is **italic** in md and never bold. A tab_var's values are LEVELS: merged and blanked, never dropped, never rotated. The new shared **`var_names`** (`"both"`/`"rows"`/`"cols"`/`"none"`, `options("tabxplor.var_names")`, resolved by `resolve_export_opts()`) is two lines in `prep_one_table()`: dropping the `var_name_col` column, and blanking `col_var_header$label` — which every backend already gates its span row on (`any(nzchar(label))`), so the col side needs no backend code (it is what let `tab_md(col_var_names=)` be deprecated by deleting its gate). It never touches a LEVEL column's header (`marital`, a kept `year`): that header identifies the column and costs no width. Runs come from the VALUES, not the grouping (`new_group` is the full group COMBINATION for ≥2 tab_vars, so an outer tab_var's run would be cut); NA = a continuation (a p-value row belongs to the block above); nested outer→inner. ⚠ md's bold exclusion must reach the WIDTH pass (`bold_rows_of()`) — `md_extra()` and the `+4` charge markup width per column.

**Phase 14n** collapses the redundant per-block `Total` rows of a compacted several-`row_vars` table, DISPLAY-ONLY, as the final step of `tab_materialize_extras()` (so it reaches the console + every export uniformly, and all roles recompute on the collapsed table with no per-backend code). `tab_collapse_total_rows()` guards on `get_vars_attr()$compacted` + `>= 2` Total rows (a single-row_var or a tab_vars table is never compacted → untouched). It compares each block's whole **total block** (the Total row + its trailing `"n"`/`"row_pct"` summary rows — NOT the block-specific `"pvalue"` row) "as displayed" via `format()` over every fmt column (one canonical predicate across backends); identical → drop all but the last block's total block, different (only `na = "drop"` can make them differ) → keep all + a once-per-session message. Comparing the whole block, not just the Total row, is what makes it correct under `pct = "col"` (where the Total is always `"100%"` and the base lives in the `n` row). Alongside it, `tab_pvalue_lines()` now keys the p-value rows on the table's **grouping columns** ∩ the `test` tibble (the synthetic `row_var` for a compacted table, `tab_vars` otherwise — byte-identical there), so a compacted table gets one p-value row PER block instead of a single mis-keyed row, and carries the `vars` attribute through its rebuild (a Phase 14d gap the collapse guard exposed). Both changes are display-only: the core `tab()` object keeps every Total row.

**Phase 17g (export-stack integration).** The render model is now the one intermediate representation every backend consumes. (1) The **declarative materializer**: `tab_materialize_extras()` seeds the row roles then delegates to `tab_materialize(tab, backend, ctx)`, which runs the applicable specs from `materialize_specs()` — a DECLARED inventory of the synthetic extras (`add_n_pct` / `or_total` / `sd_twin` / `footer` / `collapse_totals`), each a named `list(when, apply)` with a per-backend policy (Phase 20h deleted the `kind` member: it had no reader, and none of its five values was a `ROW_KINDS` value) — replacing the old imperative if/else passes. The two build-then-undo cycles are gone: the add_n `n` COLUMN is built for **xl only** (`tab_add_n_pct(..., backend =)`; text folds the base into the Total cell directly from its own `n` field via `tab_fold_addn_incell`, no throwaway column), and `collapse_totals` is a declared display slice reading the stored roles. `mat_add_n_pct` / `mat_sd_twin` are the two extracted apply helpers. **Phase 19l: the synthetic columns DECLARE what they are.** The `add_n` `n` column and the `add_pct` `col_pct` column carry `col_var = ""` plus a stored `role` (`"n"` / `"pct"` -- the values a `tab_reg()` count column already used), read through the ONE predicate `fmt_is_helper_col()`; the Excel sd twin carries `role = "sd"`. They used to be found by the col_var tag `"all_col_vars"` and by re-minting their own `"<col_var>_sd"` name -- the first a string that LIES (they belong to no col_var, not to all of them) and whose other, opposite meaning is the legacy `tab_tot()` grand-total column, which keeps it. `set_role()` is the setter the attribute lacked; `xl_materialize_data()` (in `tab_xl.R`) is the xl value-string-vs-number policy at write time. (2) **Shared footer/caption helpers** (`R/tab-export-prep.R`): `rd_footer(src, medium, theme, want_legend, subtext, lang, classes)` folds the `render_footer(tab_footer_streams(...))` sandwich every backend repeated; `rd_caption(rd, user_caption)` folds the `user → set_caption() → reg_title` fallback (md/kable/plot; xl keeps its own variant with the extra named-tabs / `tab_get_titles` tail). (3) **Single-sourced slot→hex**: `tab_xl()` consumes `ann$text_hex`/`ann$bg_hex` directly (dropping its private `text_pal`/`bg_pal` palette), so the theme-resolved hex flows only through `fmt_channel_codes()` — the same source the CSS side reads. (4) **md header** groups its spanning col_var-name row by the shared `tab_header_runs()` RLE (pandoc still can't colspan, so the width-padded per-column blanks stay md-local). (5) **`roles_totblock_edges(in_block)`** single-sources the total-block top/bottom border formula shared by `prep_one_table()` and `tx_transpose_render()` (the rest of the two role models are different computations — fmt-based vs flipped-positional — so they are not merged). (6) The **`output_kable` render moved** to `tab()`'s tail (post-`finalize_color_spec`), fixing the two-channel-colour crash. (7) `print.tabxplor_kable` **degrades gracefully** when kableExtra is absent (`kable_print_mode()` predicate → a one-time note + knitr print, no broken dispatch). (8) `tab_xl()` **dropped** the long-inert `n_min` / `hide_near_zero` / `conditional_format` args.

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

**Missing-metadata contract (Phase k).** Two tiers of metadata with different guarantees. The
per-cell `tabxplor_fmt` FIELDS (18) and per-column ATTRIBUTES (11) are **required** — they are the
solid foundation and travel with the column, so a standalone extracted `tabxplor_fmt` column still
`format()`s and colours on its own (`format`/`pillar_shaft`/the colour engine read only column attrs,
cell fields and `getOption("tabxplor.color_breaks")`, never a table attr). The three TABLE-LEVEL
attributes (`subtext`, `test`, `meta`) are **optional and NULL-safe**: every getter returns `NULL`
when absent and every consumer treats `NULL` as "absent" (`legend_specs` reads the columns;
`test_summary_grid` → nothing; `reg_model_lines` → `character(0)`; `tab_footer_streams` guards each
line). So losing one removes only the behaviour it powered — a missing `test` drops the
statistic/effect-size/p-value summary; a missing `subtext` drops the note; a missing reg `meta` drops
the title/caption and the effect-specific legend wording (falling back to the generic crosstab
legend) — never an error. Because the exporters are class-agnostic (they detect fmt columns via
`is_fmt`, not the `tabxplor_tab` class), a table downgraded to a plain tibble in a pipeline — or
`as_tibble()`d, which keeps the attributes — still exports fully coloured. The one thing a *dropped
class* costs is the console auto-print footer/summary: a bare `print()` on a plain `tbl_df` runs
dplyr's own printer, which our S3 methods never intercept (the fmt columns still render via `pillar`).
`tests/testthat/test-degraded-attrs.R` locks the whole contract.

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
  faster on the exporter path (`dev/benchmarks/results_2.0.0/phase10c_profile.txt`).
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

### tab_html() — HTML Export (`R/tab_classes.R` + `R/tab-render-html.R`)

Phase g renamed `tab_kable()` → **`tab_html()`** (the output is an HTML table; the backend *engine* is
`engine =`), keeping `tab_kable <- tab_html` as a permanent exported alias; `tab_export()`'s first format
is `"html"`. The S3 class stays `tabxplor_kable` (internal).

**Auto-print routing.** `options(tabxplor.print = "html")` (taught value; `"kable"` is the pre-2.0.0
synonym — the ONE predicate is `tx_print_html()`, `R/tab_classes.R`) makes `print.tabxplor_tab` /
`print.tabxplor_tabs` render through `tab_html()`. In knitr, bare-value auto-print would capture that
html as escaped TEXT, so `knit_print.tabxplor_tab` / `.tabxplor_grouped_tab` (registered like the
pre-existing `.tabxplor_tabs`) route to `knitr::knit_print(tab_html(x))` (as-is html) when the option
asks for html, and fall through to the default text capture otherwise — this is what lets a vignette
render every bare `tab()` chunk as a live table. `tab_html(tooltips =)` now defaults to
`options(tabxplor.tab_kable_tooltips)` (seeded `TRUE`) so a many-table document can switch the hover
tooltips off once. NOTE for hand-written fansi output hooks (the vignettes): knitr marks as-is output
with `KNITR_ASIS_OUTPUT_TOKEN` — a custom output hook MUST hand those back to the default hook, or
the tables get escaped into text (the vignettes' hook does exactly that).

`tab_html()` = `tab_export_prep(list_method=TRUE)` → map the `render_kable_html()` **engine seam**
(`R/tab-render-html.R`, Phase 10e) over the prepared tables → `tab_kable_join()`. The `engine` argument
(default `getOption("tabxplor.tab_kable_engine")`, **`"html"` since Phase 14e**) picks the backend, both
driven by the same `tabxplor_render` model so they cannot diverge in content:

- **`"html"` (the default)** — a dependency-free, self-contained `<table>` + one stylesheet, vectorised
  assembly. ~3x faster / ~6x lighter; emits the same bootstrap tooltip attributes; used by the jamovi
  live display (`tab_render_scrollbox()` replaces `kableExtra::scroll_box`).
- **`"kableExtra"` (legacy)** — the `knitr::kable` + `kable_classic`/`row_spec`/`column_spec` pipeline
  (byte-identical to pre-10e). Colour via inline CSS, HTML tooltips/popover, `inst/tab.css` injection.
  It bakes its theme at render time, so it cannot follow `theme = "auto"` (which downgrades to light).
  Since the "Phase 18b" dependency review `kableExtra` is **Suggests-only** (the default engine
  needs no external HTML library); this path is guarded (`render_kable_html()` dispatch +
  `kable_tabxplor_style()`) and aborts with a pointer to `engine = "html"` if the package is absent.

A list renders table-after-table (both engines; Phase 14d: it is never merged).

**The html engine emits NO inline styles** (Phase 14e). Every look — geometry included — is a **role
class** resolved by `tab_css()`: `tx-r`/`tx-l` (align), `tx-num` (numbers: nowrap + the number font --
proportional DejaVu Sans by default, but a MONOSPACE stack when the table shows stars, via the
`tx-has-stars` class on the `<table>`; Phase 14m-ii, `options("tabxplor.tab_kable_num_font" / "_stars")`),
`tx-br`/`tx-bl` (borders), `tx-b` (bold), `tx-bt`/`tx-bb`/`tx-bb2` (row rules — Phase 18r: `tx-bb`
also matches `td.tx-bb`, the CELL-scoped twin used to close the ONE rowspanned label cell that covers
the table bottom, which the per-row rule can't reach), `tx-span` (the col_var header — Phase 18r:
the whole-table top edge is `> thead > tr:first-child > *:not(.tx-span)`, so a span/names row FLOATS
with no top border, closed only by the `.tx-span` border-bottom below it; a level-header-first row still
gets the top edge), `tx-pill` (a background), `tx-lbl`/`tx-vname` (a variable name spanning its block, Phase 14i),
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

**Bootstrap-host proofing (Phase 18x2)**: every CELL colour class is emitted under TWO selectors —
bare (`.p1`) AND scoped (`.tabxplor-tab .p1`), built by `tx_cell_sel()` in `tab-css.R`. Bootstrap-based
host pages (pkgdown stamps `class="table"` on every table; Quarto does the same) apply
`color`/`background-color` to every cell via `.table>:not(caption)>*>*` (0,1,1), which beats a bare
class (0,1,0) **on the same `<td>`** and washed every cell colour out of the pkgdown site (the legend
spans survived: there the host rule only hit the ancestor td, and a direct rule beats inheritance). The
scoped twin (0,2,0) wins with no `!important`, keeping "restyle with ordinary CSS". The bare selector
stays first for the `tab_md()` editor contract and the legend spans outside any wrapper. Covers
`.p*`/`.m*`/`.o*`/`.u*` and the greys `.g1`/`.g2`; the md flavour (`chrome = FALSE`) carries both too
(a Quarto site is Bootstrap, and styled md wraps tables in the `.tabxplor-tab` div).

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
  merged cell desyncs the row's count and pandoc shifts the data); `col_var_names = FALSE` drops it.
  Phase 18m: it now builds a per-column cell vector and routes through `md_insert_col_sep(sep_after)`
  like the body, so its spacer columns line up with theirs (was a hand-assembled line that only knew the
  col_var-group spacers);
- the thin spacer column between col_vars needs `-` on the **delimiter** row (`md_insert_col_sep(fill=)`;
  one helper builds all four row types, so the fill is a parameter). Phase 18m: the spacer set is
  `sep_after` — `new_col_var` (col_var groups) for a plain table, PLUS the interior boundaries
  (levels|numbers, numbers|Total) for a STYLED table, so the CSS `:empty`-spacer → border-left rule draws
  the same vertical rules the html/xl exports do. Styled tables also fill every blanked label / span /
  header cell with U+00A0 (not ""), so ONLY the real spacer columns stay `:empty` (no stray borders on
  the variable-name row); `tab-css.R` adds div-aware top/bottom/**left**/right table edges. Phase 18r:
  the U+00A0 fill removed the *accidental* left edge (it used to come from the first column's `:empty`
  cells catching the `td:empty` spacer rule), so the left edge is now drawn EXPLICITLY by a rule
  symmetric to the right edge (`> tr:has(td:not(:empty)) > *:first-child` + `> thead > tr > *:first-child`,
  border-left) — independent of cell emptiness;
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

**Since Phase 20b an option is DECLARED ONCE, in `TAB_OPTIONS` (`R/tab-options.R`)** — one row per
option, carrying its `default`, its `section` of the help page, the per-call `arg` that overrides
it, its `alias` chain, its `seed` policy (`always` / `if_unset` / `elsewhere` / `no`) and its `doc`.
`.onLoad()` (`R/utils.R`) seeds **from** that table, `?tabxplor-options` is `@eval`-generated from
it by `tab_options_rd()`, and both the DEFAULT and the "Per-call `x =`" sentence are rendered rather
than typed. Before, one option meant three hand-written places kept in step by a comment saying
"keep this in sync"; adding one is a single row now, and `TAB_ARGS$option` points back at these keys
through a checked foreign key.

⚠ **The file name is load-bearing**: `tab-options.R` must sort before `tab.R` (`'-' < '.'`), because
`tab.R`'s derived `globalVariables()` tail reaches `conf_level_default()` → `tx_option()` while the
namespace is still being *sourced*. That is also why every computed `default` is a closure.

The table below is a dev subset and lags the generated page.

**Option synonyms (Phase 17j).** An option may be read under more than one name — a renamed option's
old name, or a convenience alias — through the ONE resolver `tx_getOption(names, default)`
(`R/tab-options.R`, moved there in 20b from `R/utils.R`):
the first name set (non-NULL) wins, with the seeded/canonical name passed LAST so a user's explicit
legacy/alias value overrides the seeded default. Three synonym pairs exist: `tabxplor.tab_kable_css`
(seeded; was `tabxplor.kable_css`, a 2.0.0-new name renamed to join the `tab_kable_*` family) and the
two theme aliases `tabxplor.console_theme` → `tabxplor.color_style_theme` and `tabxplor.export_theme`
→ `tabxplor.theme` (both canonical names stay seeded/documented; `color_style_theme` is 1.3.1-public,
so it is aliased, never renamed). Aliases are silent (no deprecation) and unseeded.

| Option | Default | Description |
| ------ | ------- | ----------- |
| `tabxplor.color_style_theme` | auto-detect | "light" or "dark" console theme (alias `console_theme`) |
| `tabxplor.color_html_24_bit` | `"no"` | "green_red", "blue_red", or "no" |
| `tabxplor.color_breaks` | (see Layer 2) | List of break vectors |
| `tabxplor.print` | `"console"` | "console" or "html" (recommended; "kable" synonym) |
| `tabxplor.tab_kable_tooltips` | `TRUE` | Per-cell hover tooltips in html tables (off = document-wide) |
| `tabxplor.ci_print` | `"ci"` | "ci" (brackets) or "moe" (±margin) |
| `tabxplor.compact` | `FALSE` | Compact table output by default |
| `tabxplor.cleannames` | `FALSE` | Clean factor names by default |
| `tabxplor.export_dir` | `NULL` | Default directory for tab_xl() export |
| `tabxplor.output_kable` | `FALSE` | Auto-output as kable |
| `tabxplor.kable_html_font` | DejaVu Sans | Font for the kableExtra engine (legacy) |
| `tabxplor.tab_kable_num_font` | Cascadia stack | html/md `.tx-num` number font — MONOSPACE by default (Phase g; `_stars` retired) |
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
- `leaf_ci_plain()` — the factor leaf's cell / contrast interval, on matrices, **from the plan**
  (Phase 19j, KEY 5). Shared verbatim with the jamovi tier-3 re-reference. `num_core()` has the same
  block inline for means. Both route through `ci_dispatch()` / `CI_GEOMS` (`R/tab-agg.R`).
  **Phase 19m-i**: whether the cell that IS the reference keeps its own interval is a declared
  `CI_GEOMS` member (`ref_cell`, read through `ci_geom_ref_cell()`) — a CELL interval compares each
  cell to 0 %, not to a reference, so every cell keeps it; a CONTRAST interval blanks the row it
  would compare to itself. The rule was written in all three consumers and two of them were wrong,
  so a factor `ci = "cell"` table's total row showed no bracket while a numeric one's did.
- `leaf_chi2()` / `leaf_chi2_num()` / `leaf_test_view()` — the leaf's whole-table test, calling the
  same `chi2_compute_test()` / `chi2_write_contrib()` the superseded step calls: chi-squared (factors)
  + ANOVA F (means) via `agg_chi2()`/`agg_anova()`, and the contributions for `color = "contrib"`.
- `tab_pct()`, `tab_tot()`, `tab_totaltab()`, `tab_ci()`, `tab_chi2()` + `pct_formula()`/`diff_formula()`
  — the superseded dplyr-era step API, quarantined in **`R/tab-steps-legacy.R`** (17f for the trio,
  19j for the two tests), off the build path. They RECONSTRUCT a plan from fmt markers (that is their
  purpose, since they run on a table they did not build) but share the arithmetic with the leaves.
  Internal helpers `diff_index()`/`calculate_refrows()`/`quo_miss_na_null_empty_no()` stay in `R/tab.R`.
- `tab_add_n_pct(tabs_text, add_n, add_pct)` — the `add_n`/`add_pct` block, factored out of `tab_many()`'s
  finalize so `tab_many()` and `tab_counts()` share one implementation.

### R/tab-counts.R (~360 lines) — from-the-middle constructor (Phase 4)

`tab_counts()` (exported) builds a `tabxplor_tab` from already-aggregated counts, byte-identical to the
microdata `tab()`. It is the **thinnest wrapper it can be** — "`tab()` with the first steps already done":
it normalises the input to the `.fine` aggregate and reuses the SAME engine + the SAME colour boundary /
finalize tail, forking no math.

- `tab_counts_reshape()` — dispatch on input shape → canonical long tidy counts. `table`/`xtabs`/`matrix`
  (melt via `as.data.frame.table`; bare matrix coerced with `as.table`); wide `data.frame` (`pivot_longer(cols)`);
  frequencies + base N (`input="pct"`: `largest_remainder(freq × base)` per row); long tidy (as-is).
- `tab_counts_normalize(…, cleannames)` — aggregate to the keyed `.fine` shape `[tab_vars…, row_var, col_var, n, (wn)]`;
  **drop `n==0` cells** so the aggregate is structurally identical to microdata's `.N`-per-observed-key
  (empty cells are recreated by `dcast(fill=0)`). Sets `weighted` and `has_real_n` (integrality of the counts).
  `cleannames = TRUE` strips the cleannames regex off the key levels HERE via the SAME `tab_cleannames_relabel()`
  the microdata path runs pre-aggregate (a relabel commutes with the count sum → byte-identical; the keyby
  re-aggregation merges any collapsed level).
- `tab_counts()` — the SHARED `tab_resolve_common_args()` at the front (Phase 19i: validation, the
  colour spec, `stars`, `ci_method`, the `OR` route, `tot` → totrow/totcol, `total_names` — ~15
  copy-pasted lines gone, and with them two rules it had never had), then the same typed `new_ctx()`
  → `tab_setup()` (the SHARED `tab_resolve_settings()` colour cascade) → inject `fine` as `fine_fused`
  → `tab_build_tables()`, then the shared
  `finalize_color_tail(result, color_spec, color_breaks, display)`. Base-less input (non-integer counts)
  disables CI/chi2 with a message. Weighted = real unweighted `n` + weighted `wn` (§14). It starts PAST the
  microdata prep (`tab_prepare_pop`), so the `tab()` arguments resolved there are not offered
  (`levels = "first"`/`"auto"`, `other_if_less_than`, `na = "drop_all"`/`"common_base"`, survey design,
  `wt` — use `wt_counts`); `cleannames` is the exception (applied on the aggregate keys, above).

### R/tab-args.R — the argument surface as data (Phase 20b, KEY 1 + KEY 8)

**`TAB_ARGS`** declares every public argument of the crosstab producers ONCE: which `producers` take
it, its `status` (which may be NAMED, when an argument is deprecated on one producer and live on
another), its `default` (+ `default_for` where a producer legitimately differs), its vocabulary
(`values` here, or `values_from` naming the fact table that owns it), the `values_rd` renderer, its
`option` twin, and its `doc` — the roxygen prose itself.

> **THE RULE: the fact table owns the VOCABULARY, `TAB_ARGS` owns the ARGUMENT.** `MEASURES` knows
> what `difference` is; `TAB_ARGS` knows that `color` is an argument of four producers, that it names
> a measure, and how to say so in a help page.

Three things read it. **`tab_args_rd(producer)`** generates the `@param` blocks (`#' @eval`, the
fourth use of the `reg_measures_rd()` pattern) — ordered by `formals()`, with the declared set
asserted equal to the formals at load (`tx_check_tab_args()`, `R/zzz-fact-keys.R`), so an argument
added to a signature without a row breaks the build. **`tab_check_dots()`** validates the `...` the
three superseded producers now take, refusing an unnamed argument by position and an unknown one with
a suggestion. **`tab_dots_expand()`** fills an unsupplied argument from its declared default, which
is what let the leaves keep their own starting points (`tab_num()` at `color = "auto"`, `ref = "tot"`).

**`TAB_ARG_VALUES` is derived from it**, contents and order intact, so 19i's validators did not move.

⚠ Read a row with `[[`, never `$`: `r$values` partial-matches `values_from`.

**Phase 20h — `EXPORT_ARGS`, the render surface (KEY 8).** The exporters' half, in the same shape and
read through the same functions via **`arg_table_of(producer)`** (DERIVED from `EXPORT_PRODUCERS`,
never a hand-written mapping). It is a SECOND table because three names mean something else there —
`color` is a logical rather than a measure, and so are `subtext` and `stars` — and a named list cannot
hold two rows under one key. Its scope rule is narrower than `TAB_ARGS`': a row for an argument that is
**shared by ≥2 exporters** or is **an option's per-call twin**; a single-backend geometry argument
(`sheets`, `titles`, `colwidth`, the fonts) stays in its own roxygen, and `tx_check_tab_args()`
therefore checks the exporters **scoped**, reusing the `tab_build` idiom already in its body.

⚠ Only **9 of the 24 rows carry prose**, and the reason is the rule above ("it must remove a
DUPLICATE") applied honestly: `@param theme` is written seven times but the ACCEPTED VALUES differ by
backend — `allow_auto = TRUE` is passed only by `tab_html()`, `tab_md()` and `tab_css()`, so only they
take `"auto"` — and seven texts describing five value sets are not one duplicate. `theme`, `caption`,
`css`, `format`, `file`, `path`, `subtext` and the single-backend option twins are DECLARED (which is
what empties the foreign key) with `doc_in_producer = TRUE`, so their documentation stays where it is
true. ⚠ The reward is **anti-drift, not `man/` lines**: the exporters keep every formal by ruling, so
each page still documents each argument, and replacing the drifted short texts with the canonical
fuller ones made `man/` GROW by 23 lines. What it bought is 26 hand-written blocks → 9 declarations,
five corrected texts, and the `TAB_OPTIONS$arg` foreign key's eleven-name `allow` list → empty.

⚠ `tab_check_dots()` and `tab_dots_expand()` stay CROSSTAB-only: an exporter's `...` is a pass-through
to its backend, so refusing an unknown name there would refuse a legitimate backend argument.

### R/tab-resolve.R (~180 lines) — the argument-overwrite cascade (Phase 7b)

`tab_resolve_settings()` is the ONE pure, data-free resolver of the colour cascade shared by
`tab_build()` and `tab_counts()`: `color = "auto"` → a concrete measure (through the declared
`auto_for` contexts), then that measure's declared `requires` applied to `chi2` / `totrow` / `ci` /
`ref`. **Phase 19c (KEY 4)** deleted the split of the one `color` argument into four per-step
sub-passes (`color_diff_OR` → `tab_plain`, `color_ctr` → `tab_chi2`, `color_ci` → `tab_ci`,
`color_num` → `tab_num`): it was a fossil of the pre-2.0.0 four-step pipeline, four hand-written
recodes over measure literals, and one of them (`color_ci`) existed only to receive a legacy combined
string the cascade itself manufactured one step after 17d had decoded such strings away at the
boundary. The resolver returns ONE resolved measure and each consumer asks the `MEASURES` table what
it needs — `measure_builds()` (does the contribution pass stamp it?), `measure_applies()` (can it colour a mean?),
`measure_forces()` (does it force this build step?). `resolve_color_auto_num()` is the numeric (means)
arm, invoked by `tab_num()`. The function reads only argument values + column CLASS metadata (never column values) —
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
  (Phase 18b-ii dropped `stringr` and `magrittr`; the package uses base `|>` and `stringi` now)
- `score_from_lv1()` — scoring helper for survey data

### R/tab_reg.R (Phase 12c — LIVE; renamed from R/tab_logit.R)

Unified regression tables as native `tabxplor_tab` objects, over ONE family-dispatching engine. Public:
`tab_reg(data, dependent, predictors, family, effect, measure, display, wt, reference, method, color,
color_signif, ...)`; the estimand is **`effect` × `measure`** (Phase 19e / KEY 8b, `R/reg-estimand.R`),
not the retired `exponentiate` × `at`; `predictors` as a character vector = one model (`dependent` may be a vector → one column per
outcome), or a named list = model comparison (one column per model, blank where a predictor is absent).
A binary outcome is `tab_reg(family = "binomial")` (Phase 20a deleted the `tab_logit()` / `multi_logit()` wrappers: they exposed only ~20 of `tab_reg()`'s formals, so `effect`, `measure`, `compare`, `baseline`, `reference` and `color` were unreachable through them). Internal
engine: `reg_detect_family()` (auto: binary→binomial / continuous→gaussian, else abort), `reg_fit()`
(complete-case `stats::lm` (gaussian) / `glm` (binomial/poisson) / `survey::svyglm` (weighted) →
`broom::tidy`; Wald CI in-house — z for fixed-dispersion glm, t(df.residual) for lm/quasi/svyglm — the
exact dual of the Wald p; `method="profile"` = `confint`+LR for unweighted binomial/poisson),
`reg_skeleton()` (var/level/term rows), `reg_column()` (align a fit → one fmt column), `reg_build()`
(the staged build, below). `broom`/`survey`(/`MASS` for profile) are
`requireNamespace()`-guarded Suggests.

**Phase 20e (KEY 6) — the staged build, and `R/reg-empirical.R`.** `reg_build()` was 726 lines,
39 top-level locals, 7 local closures and eleven unnamed phases — the largest function in the
package — while `tab_build()` has had a typed ctx and named stages since 17e/19i. It is now **20
deparsed lines over named stages, each named after the part of the table it produces** (as reshaped
by 20f-iii, next section):

```
  ctx <- new_reg_ctx(...)                    declared keys; `shared` stays ONE nested record
  if (!is.null(tab_vars)) return(reg_stage_split(ctx))   the recursion, at the TOP (returns a table)
  reg_stage_setup      the skeleton (fit-free), the shape facts, the per-spec PLAN
                                                        (⚠ REWRITES `data` on the reref path)
  reg_stage_specs      ONE reg_spec_build() per model + the column layout their products imply
  reg_stage_footer     the products' rows + reg_compare_rows() -> the `test` tibble
  reg_stage_rows       labels, relabels, sparklines, the products' add_n columns -> `tab`
  reg_stage_assemble   the crude blocks + the model columns into `tab`
  reg_stage_tips       `meta$empirical_tips`, resolving the products' placeholders
  reg_stage_finalize   the inference basis, then reg_finalize()
```

The idiom is `new_ctx()`'s, fourth use: **the formals are the contract**, the body is
`as.list(environment())`, `globalVariables()` is derived from them, and a stage product is DECLARED
rather than left to appear (an undeclared key is *absent*, so its own `is.null()` guard errors
instead of firing). Each stage opens `list2env(reg_ctx_locals(ctx), environment())` —
`ctx_settings_locals()`'s twin, `c(ctx, ctx$shared)` — so `shared` is projected, never flattened
into a second carrier; `tx_check_reg_ctx()` (`R/zzz-fact-keys.R`, the only file that sees both
constructors) keeps the two name sets disjoint at load. ⚠ **no ctx key may start with a dot**:
`as.list(environment())` defaults to `all.names = FALSE`, so `.fit_cache` would be silently dropped
from the record — the ctx key is `fit_cache`, `reg_build()`'s formal keeps its dot.

⚠ **The stage order is the SOURCE order and is load-bearing**: every fit — the reported ones, the
footer's linearity refits, the crude univariable ones, the split branch's interaction refits — may
inform or warn, and `dev/verify_reg_specs.R` compares the message stream *in order*. On a
5-predictor `empirical = TRUE` table the model fits are a minority, which is why 20f measured all
four sites rather than parallelising the obvious one.

The seven closures became four named top-level functions (`reg_cols_coef` / `reg_cols_ame` /
`reg_cols_vsrest`, dispatched by `REG_BUILDERS`; `reg_emp_frame`, `reg_set_obs`, `reg_add_emp_cols`)
plus one one-line local. The same phase carved **`R/reg-empirical.R`** (~1190 L) out of `tab_reg.R`
(5630 → 4734): the whole observed/crude subsystem — `REG_EMPIRICAL`, `reg_empirical()`,
`reg_empirical_fit()`, `reg_empirical_columns()`, `reg_same_estimand`/`_frame`,
`reg_gap_se_columns()` — the producers whose *stage* is `reg_stage_empirical()`, the `tab-leaf.R` /
`tab.R` relationship.

**Phase 20f-iii — `reg_spec_build()`, and the parallelism it unlocks.** 20e named the *stages*;
six of them still carried their own `map(specs, …)`, so *"which parts of the table are per-model and
which are between-models"* took four files to answer. **`R/reg-spec-build.R`** is that answer:
`reg_spec_build(i, ctx, emp_shared)` produces everything one model contributes — its fit, its
columns, its GOF / global / check rows, its `add_n` count, its observed (crude) block, its
`obs`/`gap_se` and its two tooltip fragments — as one declared `new_reg_spec_product()` record, and
the stages above it become cross-spec **assemblers**.

**The payload rule** is what makes the S axis (several outcomes in one table, or a models list)
dispatchable: *the product carries no fit and nothing referencing one*. Since 20f-iiii there is ONE
exception — `fit`, whose only consumer is `reg_compare_rows()`, on a path that is serial anyway. A
crude block leaves the builder as its **columns** and nothing else (`reg_emp_slim()`); the 60–100 MB
`$frame`/`$fits` never travel. Two **placeholders** carry what a worker cannot know: the footer rows'
`col` (rewritten wholesale per product, since every row of one model shares one) and the tooltips'
`(column index, skeleton row)` pair — which also freed the tooltips from needing `reg_stage_rows()`
to have run.

**Phase 20f-iiii — the crude block belongs to the OUTCOME.** Building it inside the first spec and
handing it down the loop made a per-outcome fact look per-model, cost the loop its last piece of
carried state, and was one of the three reasons the models could not be dispatched. **`reg_stage_crude()`**
builds it once, before any model, for every one-outcome table; a several-outcome table keeps its
blocks per spec, where each spec *is* an outcome and the work stays on the parallel axis rather than
serialising into a pre-pass. It is **fit-free**, which is what makes it liftable: the two facts it
used to read off the model object have exact producers of their own — `reg_positive_level()` (the
function `reg_prep_binary()` itself calls to order the levels) and the outcome's first level (which
`reg_crude_yw()` already collapses any foreign `ref_category` to). `reg_crude_block()` is the shared
arithmetic, so the one-outcome and several-outcome paths cannot fork.

**`reg_specs_independent(ctx)`** is the one predicate: `NULL`, or the *reason*, reported when
`parallel` was explicitly asked for. **Two** reasons survive, both facts about the statistics and
both *measured* (`dev/tabxplor_reg_performance.md` §8): a model comparison is a test *between* fits
(`stats::anova(m_lo, m_hi)`, or survey's own `regTermTest` Wald arm — and returning the fits instead
was measured at **162 MB each** at n = 200 000, so a `reg_compare_digest()` would have to
re-implement a survey quantity); and an all-coefficient table with a compound formula reads its
shared skeleton off the first fit — which is *unreachable* from `tab_reg()`, since a compound formula
forces exactly one spec, and survives as the invariant for a direct `reg_build()` caller. Everything
else — several outcomes, a models list, a crude block — is independent.

`parallel` therefore becomes a shared argument of both producers, over the *same* option, worker
count rule, pool and `tab_parallel_stop()`, and `R/tab-parallel.R` needed no change: `tab_pmap()`
was already generic. Its `tab_reg()` units are the models (`reg_stage_specs`), the `tab_vars` groups
(`reg_build_group`) and the outcomes of a multi-outcome recursion (`reg_build_outcome`). Measured
ceilings and the reasons they are narrow: `dev/tabxplor_reg_performance.md` §6–§7.

Three footer producers moved with it: `reg_gof_tibble()` → per-spec **`reg_gof_rows()`**, and
`reg_global_rows()` / `reg_check_rows()` per spec. Each had exactly one caller and each loop body
was already a pure function of the index, so what changed is who holds the loop.

**Phase 17h — integration (all internal, byte-identical).** `reg_build(data, specs, shared, split_var,
.fit_cache, …)`: the per-dependent family/do_exp/effect_shape/eff_word/color live ONLY on the specs (read
as `sp$*`; the scalar formals + `sp_get()` are gone, the homogeneous scalar `family` is derived from
`specs[[1]]$family`), and every other per-call setting rides ONE `shared` list — so the split recursion
no longer re-lists ~30 positional args. Shared micro-helpers: `reg_wald_finalize()` (the one
est±crit·se → p-dual → exp assembly, behind `reg_wald_from_tidy` + the `reg_fit` Wald branch +
`reg_reref_fit_res`), `reg_skel_key()`/`reg_skel_match()` (the `"\r"` skeleton-align idiom),
`reg_cleanup()` (the cleannames strip), `reg_complete_frame()` (the ONE model complete-case frame —
`reg_fit` uses it for the fit, the empirical + multinomial-tip stages share it via `reg_emp_frame()`
because the reref/digest fit carries no `$data`). The crude-companion columns are driven by the
**`REG_EMPIRICAL`** fact table (per family: base + effect column SHAPE — fmt type/display/digits/ref/
scale/colour measure/name — plus the CI method literal, which Phase 19b now STAMPS on the column) through one `emp_col()` builder; the CI-method
`method_mean_diff`/`method_mean_ratio` read straight from `REG_EMPIRICAL`, so "the empirical CI matches
what the legend names" is data, not a hand-synced pair. The `predicted_unadjusted` control column was cut (its
Emp.% == unadjusted-prediction identity survives as a test-only assertion).

**Phase 18z10 — the last three families, and one rule instead of six inferences.** `empirical = TRUE`
was silent on grouped binomial (`trials =`), multinomial and ordinal. Those were not three features but
one missing fact, and the fix is mostly SUBTRACTION. The rule z9 stated now covers everything: *the
observed effect is the model's own effect, fitted with ONE predictor*; where that univariable model is
**saturated** it has a closed form, otherwise it is a real fit. `reg_crude_saturated(crude_key,
is_factor)` states exactly that (a factor predictor, under any family except ordinal), so nothing
re-derives it.

**The stored fact.** `reg_crude_key(family, trials, compound)` — the `REG_EMPIRICAL` key, or NA — is
computed ONCE at spec construction and stored on the spec (mirrored into `reg_meta$crude_keys`). It
retired six inferences in three different shapes: a duplicated family whitelist in `reg_build`, a
hand-written `quasipoisson -> poisson` alias, a lookup-miss return, a second silent fallthrough, a third
family list in `tab_reg()`, and — worst — `positive_level`-is-NULL used as a proxy for "grouped binomial
or compound formula", which is a SIDE EFFECT of `reg_fit()` skipping `reg_prep_binary()` on that path,
not a statement about crude twins (Phase 17 rule 2). Grouped-ness is now a role, so `trials` never has
to enter `reg_meta`. **`reg_crude_shape(crude_key, effect, do_exp)`** is the twin selection rule, read by
both the column builder and the footer wording (`reg_crude_in_cell()`), so the two cannot drift; each
family declares its coefficient-scale row as `coef` / `coef_log`.

**One merged grid.** `reg_empirical()` is now keyed **(var, level, category)** and absorbed
`reg_empirical_tips()`, which is deleted: they were the same computation at two key widths (the tips'
`sum(w[m & y == cat]) / sum(w[m])` is bit-identical to the old binary branch's `wpos/(wpos+wneg)`), and
the tips version was simply the general K-category form. Two PARTS, because one family needs both at
once: a CATEGORICAL part (the weighted share + its Wilson interval, its difference from the reference
LEVEL + Newcombe, the two 2×2 legs `emp_wpos`/`emp_wneg` against the reference CATEGORY, and the odds
and risk ratios built from them) and a NUMERIC part (weighted mean + variance). That is why the old
`emp_base` had to split into `emp_prop` / `emp_mean`: a grouped binomial shows a mean SCORE beside a
summed-count OR. ⚠ `emp_ratio` divides by the reference level's own `wpos/wneg`, not by the
algebraically-equal `prop/prop[ref_cat]` — the last bit differs, and an OR of 1−1e−16 renders as `1/1`.
`reg_crude_yw()` generalises `reg_crude_y()` into the ONE description of "what the crude estimator
averages, and with what weights": a grouped-binomial row is a CLUSTER of `trials` draws (`y = succ/tr`,
weight `w*tr`, so `sum(w*tr*y) = sum(w*succ)` is exactly the summed 2×2 leg, and `reg_if_se()` summing
over ROWS gives the cluster-robust variance the model leg also has); a categorical outcome contributes
the indicator of one category.

**Three shape facts, not a fifth arm.** A shape row may declare `visible = FALSE` (its number rides
IN-CELL via `obs` instead of drawing an `Obs_*` column), `per_category = TRUE` (one crude effect per
outcome category) and `from = "fit"`. `two()` became **`emit()`**, accepting 0, 1 or 2 columns — ordinal
emits ONE (a cumulative OR has no base share), multinomial ZERO. `reg_empirical_numeric()` generalised
to **`reg_empirical_fit()`**, keyed by skeleton row rather than by variable, and called with *every*
predictor under an ordinal outcome (proportional odds is a CONSTRAINT: the closed-form substitutes drift
2.4–5.4 %, and the drift IS the PO violation — a data-dependent offset the size of the first colour
break). The crude EFFECT is returned as a list keyed by outcome category (`""` for a single-column fit),
and `set_obs_if()` looks it up by the column's already-stored `emp_key`. ⚠ `l[[""]]` is a
subscript-out-of-bounds ERROR in R, so every such lookup goes through `cat_get()`.

**Display.** Where the crude effect draws no column, `set_obs_if()` folds it into the model cell as
`"{or} ({obs})"` / `"{diff} ({obs})"` — driven by `shape_visible()`, never by a family name. Three
reasons: `obs` is defined on the cell's OWN scale, so the bracket is the same kind of quantity as the
estimate; the printed bracket then IS what `color = "adjustment"` scores, so number and colour cannot
tell different stories; and the crude percentage is not lost (it stays in `empirical_tips`).
`reg_model_note()` gains an `obs_in_cell` clause so the footer names the bracket. This also required
fixing a shipping bug: `tab_kable_print_tooltip()` gated its lines on `display_primary()`, the FIRST
token only, so every composite cell repeated its own bracket on hover (an AME cell showed the adjusted
% in the cell and again in the tooltip). **`fmt_display_shows(display, token)`** — one helper on the
existing template parser — replaced the six primary-only gates.

**The gap test.** The coefficient paths of multinomial / ordinal stay blocked by
`reg_estimand_collapsible()` (they are conditional odds ratios), which needed no new code: an all-NA
`gap_se` already reads as `ignore`. Their MARGINAL paths get a real test, from a new score-based core in
`R/reg-influence.R`. `reg_coef_if_maker()` dispatches: `multinom`/`polr` have no working residuals or
IRLS weights, so they go through **`reg_if_from_score(S, bread)`**. ⚠ The two cores are deliberately NOT
merged — `reg_if_from_parts()` exists to avoid ever materialising `U = X*(W*r)` (peak memory ONE `n×p`),
and a multinomial score has no such structure. Two traps, both closed structurally rather than by
comment: `vcov(multinom)` is CATEGORY-MAJOR while `as.vector(coef())` is category-minor (measured: 2.7×
wrong SE), so the score columns are NAMED and a mismatch returns NULL; and `polr`'s bread is
`vcov(fit)`, never `solve(fit$Hessian)` — `polr` optimises over `(β, ζ₁, log Δζ)`, and substituting the
Hessian was measured up to 2× wrong here. `reg_ame_if_cat_maker()` adds the marginal IF per outcome
category, its jacobian from a LOCAL predicted-probability function (`reg_prob_engine()`: softmax /
cumulative logit). That local predictor is not a second implementation — it is the same arithmetic the
score functions already need, one producer with three consumers — and it is policed the way
`reg_crude_if_maker()` is: a test pins it to `marginaleffects::avg_comparisons()`, which it reproduces
to 10 decimals. `svyolr` is refused (its `fit$var` is the design-based sandwich, not the bread), which
is moot: `tab_reg()` already aborts a weighted 3+ level outcome with `effect = "ame"`.

**Phase 20d — the marginal effect is computed once.** An average marginal effect and both of its
variances are ONE counterfactual sweep read three ways, so the sweep became its own producer:
**`reg_gcomp_maker()`** (lm/glm/svyglm) and **`reg_gcomp_cat_maker()`** (multinom/polr, answering for
every outcome category at once) return `est` / an ANALYTIC `G` / the empirical term `emp` / the adjusted
means, and the two influence makers above are now their four-line wrappers — the 3+ level jacobian
stops being central differences (`reg_prob_engine()` gained `dmean()`, the derivative of its own
`probs()`), which is a ~1e-9 change in MNL/ordinal `gap_se` and a 2.4 s → 0.01 s change per contrast.
The second consumer is new: **`reg_marginal_gcomp()`** (`R/tab_reg.R`) prints `est ± crit·`
**`reg_delta_se(G, vcov(fit))`** through `reg_wald_finalize()`, replacing `marginaleffects`' numerical
jacobian — one full re-prediction per coefficient, 71 % of a 10 s call. ⚠ **The two standard errors are
different quantities and must not be swapped**: `reg_if_se()` is a sandwich variance *plus* the
empirical-averaging term (measured up to 3.6 % away) and answers *is this effect different from its
crude twin*; `reg_delta_se()` is what the interval PRINTS and reproduces `marginaleffects` to 1e-8 on
glm and weighted svyglm alike. Which engine runs is declared per estimand row (`REG_ESTIMANDS$engine`,
`"auto"` → everything but `at_reference`), the producer returns NULL rather than a wrong number, and
`reg_marginal()` then falls back for the WHOLE call so one column carries one convention. Measured:
binomial marginal 10.0 s → 1.2 s, multinomial marginal 45.2 s → 5.2 s.

**`tab(OR = "cumOR")` and the `ordered` un-block.** The descriptive twin: for an ORDERED col_var with 3+
levels under `pct = "row"`, cell *(i, j)* is the odds of falling at or below level *j* for row *i*
against the reference row — a plain 2×2 from the AGGREGATE with the exact Woolf interval, no
proportional-odds assumption. A *k*-level scale has *k−1* cuts, so the last column is empty by
construction, and the spread across a row IS the PO diagnostic. Nothing new in `fmt_class.R`: same `or`
field, `scale = "odds_ratio"` — a new *dichotomisation*, not a new measure. Eligibility is
per PAIR, so it resolves onto **`settings$pairs$OR`** (17e's spine exists for exactly this); an
ineligible pair degrades to `"no"` with one message. That move also deleted a live bug:
`tab-resolve.R`'s `auto_or` indexed the per-row_var SCALAR `OR` with a logical over col_vars, so with ≥2
factor col_vars `color = "auto"` silently resolved an OR table to `"diff"`. The shared Woolf block was
INVERTED rather than branched: each OR arm supplies its own 2×2 as a closure (`or_cells(N)`), so the CI
block is one `ci_or()` call for three OR flavours. The blanket `ordered`-strip in `tab_prepare()` is
gone; its FIXME guessed at MCA, but the measured cause was two vctrs bind sites in the TOTALS machinery,
both reachable only through `tab_vars` — `leaf_rename_totals()`'s two `if_else`s (now mask-assignments
on an expanded factor; ⚠ `sort(unique(.))` there is load-bearing, the old character branch sorted
alphabetically) and `num_rollup()`, which now gives every rollup piece ONE shared ptype (vctrs refuses
two ordered factors with different level sets). `tab_stack_tables()` un-orders the merged `levels`
column when several row_vars are stacked: different variables' orders are incomparable. Public-surface
change, accepted consciously: grouping columns come back `ordered`, with `NA` / `Total` appended as the
GREATEST levels — labels, not scale points.

**Phase 18z9 — the crude companion of a CONTINUOUS predictor.** Until z9 those rows were blank, and
that blank was a skeleton **key miss**, not a guard: `reg_empirical_columns()` joins on
`paste(var, level, sep = "\r")` and a numeric predictor's skeleton row is `var = p, level = p`, so only
`reg_build()`'s predicate kept it out. The rule the factor arm already applies — *the observed effect is
the univariable model's effect*, saturated (hence closed-form) for a factor — simply extends, which is
why `reg_empirical_numeric()` **re-calls `reg_fit()`** rather than hand-rolling: with one predictor and
the model's own family, `design_spec`, `conf_level`, `method`, `inverse` and `multiplier`, "crude and
model on the same scale, same power *k*, same CI rule" is structural. (No closed form exists: the
classical discriminant / exponential-tilt estimators are exact only for a NORMAL predictor and degrade
to 50–70 % error under skew — measured, `dev/numeric_predictors_crude_counterparts.md` §6.)

One new internal formal, **`reg_fit(drop_extra =)`**: variables joining the complete-case `drop_vars`
but NOT the formula, so each crude fit lands on exactly the model's population (the documented
`empirical` contract, and the row identity the gap test's influence functions need). Passing the
pre-filtered frame as `data` is *not* equivalent — `reg_resolve_design()` computes a PREBUILT design's
`keep_mask` from `data` itself, and a shorter mask recycles silently. The fit is always NATIVE-scale, so
ONE fit serves the exponentiated column, its log twin and the gap test.

The numeric rows are spliced in at exactly **two single sites**, both inside `emp_col()`'s twin `two()`
(`reg_num_overlay()`): the effect column and the returned effect vector. Doing it earlier would be a
live bug — on the binomial `ame` branch the base and effect columns are built from the SAME `rd_fields`
list, and `REG_EMPIRICAL$binomial$base` declares `color = "diff"`, so the AME would land in `Obs_%`'s
`diff` field and **colour a cell that displays nothing**. The estimate field is chosen by the shared
`fmt_est_of()` (fmt_class.R). Phase 19b retired the dispatch itself: the estimate field is the stored scale's declared `est_field`, so `fmt_est_field()` and its five copies are gone (D17).
The **base cell stays NA**: measured (§4.1), the univariable fit's only base-scale output,
`P(Y | X = mean X)`, is the marginal rate for EVERY numeric predictor (0.4738 for both `age` and
`tvhours` against an overall 0.4744) — a cell that looks per-predictor and is not. Its distribution
(mean, SD, and the mean within each outcome group) rides `empirical_tips` instead, attached to the
EFFECT column, which has visible content.

**`multiplier` is now the UNIT a continuous effect is reported per, and its default is `"sd"`.** Per one
unit a numeric row sits inside the first colour break and reads as "no effect" (measured: `age` 0.969
per year against 0.657 per SD, where the factor contrasts in the same table span 0.66–2.23). Grammar: a
scalar (`"sd"` / `"2sd"` / a number) applies to every continuous predictor, a named vector overrides per
variable and the rest keep the scalar; `multiplier = 1` restores per-1-unit. It is resolved **once**, in
`tab_reg()` (`reg_resolve_multiplier()` / `reg_predictor_sd()`), on the complete cases of the
**predictors** — deliberately not of the dependent — so one predictor keeps ONE unit across several
outcomes, across compared models and across `tab_vars` groups (a per-group SD would make
`between_groups` compare different quantities: measured 15.91 vs 12.22 across a 2-group split). Four
consumers see the same frozen numbers: `reg_fit()` (unchanged), `reg_marginal()`
(`variables = list(v = k)` — a *k*-unit forward difference, measured 0.020322 against `10 ×` the 1-unit
AME 0.020297; the keyword is never passed through, marginaleffects' own `"sd"` being a centred contrast
on the SD of its `newdata`), `reg_reref_fit_res()` (`est * k, se * |k|` in `reg_fit()`'s own order and
expressions — folding *k* into the contrast would compute `sqrt(k² V)` where `reg_fit` computes
`|k| sqrt(V)`: equal in exact arithmetic, not in IEEE754), and the row label. Because the digest is
native-scale it is multiplier-independent just as it is reference-independent, so **`multiplier` left
the reref gate** and a scaling change is a cache HIT — without that, the new default would have killed
the jamovi fast path for every table with a continuous predictor.

**One stored predictor kind.** `reg_is_factor_var()` (`factor || character || logical`) replaced five
disagreeing sites, and `reg_meta` gained `predictor_types` (Phase 17 rule 2: the `level == var`
convention that implicitly marked a numeric row is already broken by `cleannames` and by the multiplier
relabel). It fixes a measured bug: `glm` names a logical's coefficient `<var>TRUE` while `reg_skeleton()`
sent it down the numeric arm (`term = <var>`), so a **logical predictor rendered completely blank**.
`Date`/`POSIXct` stay numeric, where they already worked. ⚠ `haven_labelled` is `is.numeric()`-TRUE, so
the old predicates agreed there — only `logical` and `Date` ever diverged.

⚠ **The prose below still says `exponentiate` / `type = "coef"`. Both are gone** — 19e replaced the
argument with `measure` (whose `"log"` value is what `exponentiate = FALSE` meant) and 19b replaced the
`type` attribute with the stored `scale`. Read it for the COLOUR reasoning, which is unchanged; the
estimand vocabulary is `R/reg-estimand.R`'s.

The estimand row's `exp` flag drives the fmt shape: **multiplicative** OR/IRR → the `or` field,
`scale="odds_ratio"`, `display="or"`, `color="OR"` (neutral 1, `1/x` reciprocal); **additive**
gaussian β / log-odds → the `diff` field, `type="coef"`, `display="coef"` (raw signed render, no ×100/%/×),
`scale="raw_diff"` (or `"log_coef"` on a link-scale family), `color="diff"` (neutral 0). A **gaussian** β is coloured as the effect-size **β/SD(Y)**
(the `var` field carries var(Y)) against the `mean_diff` (Cohen) breaks. Phase g: a **non-gaussian**
coefficient (`measure = "log"` for binomial/poisson/…) has no var(Y) on the link scale, so instead of
greying out it colours on `log_odds_scale()` — the LOGGED `odds_ratio` breaks rounded to 1 dp
(`c(0.2, 0.4, 0.7, 1.4)`), center 0, `std = FALSE` (so the SD-division skips) — keyed on
`type=="coef" && model_family ∈ {binomial,poisson,quasipoisson,ordinal,multinomial}`; it reads the same
intensity as its exponentiated OR twin, and the legend drops the "SD" unit. With `empirical = TRUE`
the crude companion follows: `REG_EMPIRICAL`'s `or_log`/`irr_log` shapes build `Obs_log(OR)`/`Obs_log(IRR)`
(logged effect + logged CI) via `reg_empirical_columns(do_exp=)`. Reference rows (crosstab reference /
regression intercept + factor baselines) are never coloured (`fmt_color_plan`'s `gate & !is_refrow`).
No new fmt fields/attributes — `type` gained the value `"coef"`, `display` the token `"coef"`. Phase g also
snake-cased the reg column names (`Obs_*` / `Model_*`, was `Emp. *` / `Model *`), disambiguating several
outcomes with a console-only `[dep]` bracket that `tab_col_var_header()` strips in exports, and added
`tab_reg(spread_models = TRUE)`: a single non-multinomial model with a `split_var` auto-`tab_spread()`s to
side-by-side columns (since Phase 19h through the ONE `tab_spread()`; the split level is stored beside
the outcome, see the `col_group` note below). See CLAUDE.md Phase 12c + Phase 18g + decisions §37. `R/tab_logit.R` and `R/tab_logit_2.R` are emptied (`git rm` pending; the
parsnip draft + or_plot/lm_plots deferred to a later display phase).

**Phase 14w — the reg display model.** `tab_reg()` sets ONE **`reg_meta`** record (list:
`family`/`effect`/`at`/`do_exp`/`eff_word`/`dependent`/`positive_level`/`predictors`/`split_var`/
`comparison`/`model_labels`/`conf_level`) via `set_reg_meta()` — since Phase 17b a sub-field of the
`meta` list (`get/set_reg_meta` are thin accessors into it), carried automatically by the ONE
`tab_attrs()` `meta` line + threaded through `reg_footer_lines`/`tab_pvalue_lines`). It drives: the reg
**title/caption** (`reg_title` / `reg_family_display_name` / `reg_family_short` / `reg_sheet_name`;
since Phase 19m-i the family NAMES all come from the one declared **`REG_FAMILIES`** table in
`R/reg-estimand.R` — footer sentence, filename tag and the two jamovi picker labels, whose `ui = NA`
IS "not offered in the picker"; `REG_FIT_FAMILY` and `REG_FAMILY_MULT_WORD` derive from it; Excel
title+sheet, md/kable caption); the **"Model:" legend line** (`reg_model_line`, ordered before the colour
legend at every footer site); and the **colour legend** (`legend_specs()` reads `is_reg = !is.null(reg_meta)`
— robust across footer materialisation, which drops `test` — derives the per-column effect word from
`family`+the stored scale instead of the column-name suffix, and always uses "the reference category"). Header
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
in the hidden `cache_state` Image `$state`; since Phase 17i it rides the SHARED cache kernel in
`jmvtab-cache.R` (config `JMVREG_CFG`, schema 3) instead of a duplicated + O(n²)-evicting lifecycle —
only the store stays decoupled (see the *Shared cache kernel* note above).
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

### Phase 18z13 — the model-comparison framework's boundary

`dev/reg_comparison_framework_stress_test.md` measured that the z5/z8/z9/z10 statistics verify but the
boundary between them and the rest of `tab_reg()` leaked. Eleven fixes; the ones that changed a shape:

- **One population per outcome, by default** (`na = "drop_by_outcome"` | `"drop_by_model"` |
  `"drop_all"`). It needed no new mechanism: z9's `reg_fit(drop_extra =)` is exactly "variables the fit
  must be complete on without modelling", so the shared frame is `drop_extra = all_predictors`. The old
  `drop_all_models` pre-pass on `data` is DELETED, and with it the "ignored for a prebuilt survey
  design" caveat — pre-filtering `data` breaks a prebuilt design's keep_mask (`reg_resolve_design()`
  computes it from `data` itself), `drop_extra` does not. `emp_frame_of()` uses the same set, so
  "crude and model are on the same rows" is structural rather than checked.
- **`reg_same_frame()`** — the twin of `reg_same_estimand()`, gating the same two things (`obs` AND its
  gap SE). Before it, a model fitted on rows the crude block did not cover lost only the TEST and kept
  the descriptive colour: the code knew the two numbers were not comparable and coloured their
  difference anyway. It reads `f$nobs` when `f$data` is absent, so the jamovi digest path keeps `obs`.
- **`reg_color_notes()`** — THE producer of "the colour you asked for cannot be computed here". Four
  refusals used to say so in four hand-written blocks in `tab_reg()`'s body and two said nothing; each
  reason is now one entry, in two kinds (*no colour* / *no test*). Interpolated inside the function
  (`cli::format_inline`) because it names its own locals, and re-emitted as a VALUE (`"{note}"`) so cli
  does not glue a message that legitimately prints a literal `{obs}`.
- **`fmt_gap_scale_key()`** — the gap ladder follows the ESTIMATE's own scale, so a threshold means the
  same thing in every table (`adj_ratio` / `log_odds_scale(adj_ratio)` / `adj_diff` in points /
  `adj_diff_std` in SD(Y)). Since **Phase 19b** it is one lookup on the column's STORED `scale`, and the
  "⚠ the ORDER of the branches is the contract" warning is gone with the dispatch it guarded: a poisson
  count AME and a raw poisson coefficient used to be byte-identical in `(type, ci_type, model_family)`,
  told apart only by whether the per-cell `var` field happened to be non-NA. They are `raw_diff` and
  `log_coef` now, stated by the producer that knows which it is building.
- **`by_scale`** on a MEASURES row — presentation facts belonging to a SCALE rather than to the measure,
  folded in by `measure_facts(measure, policy, scale_key)` from the plan's new `scale_key`. Same
  mechanism as `guar` folds a per-POLICY override, so every pre-z13 measure resolves identically by
  construction (deriving the glyphs from `plan$center` was evaluated and rejected: 2 of the 4 legacy
  measures need an exception, and it cannot express `break_scale` / `unit_kind`). It also let
  `contrib`'s `guar` shed the glyph entries its scale swap already implied. `legend_unit_word()` is the
  extracted twin of the switch `chan()` and `legend_threshold_phrase()` each held.
- **`reg_term_tests()`** — the aggregated interaction test and the per-predictor GLOBAL test
  (`stats = "global"`, in the default set) are the SAME computation differing only in which fit and
  which terms are dropped. The global one costs NO extra fit (the model is in hand) and is emitted only
  for terms with 2+ coefficients. *(z15 made it footer ROWS and deleted `reg_term_test_line()`; only
  the interaction test is still a LINE — see below.)*
- **`reg_level_counts()` + `add_n = TRUE`** — the N behind each predictor level, on the model's own
  frame, as a real BUILT column (the count needs the model frame, which exists only at build time;
  `tab()`'s display-time `add_n` folds into a Total cell a reg table does not have). `role = "n"` is a
  third stored role with three consumers: `forest_plot()`'s model-column pick, `reg_spread_models()`'s GOF
  key (the `n` column comes first and would otherwise key every group's footer under its counts), and
  the `[dep]` bracket strip. Tests select reg columns through `tests/testthat/helper-reg.R`'s
  `reg_fmt_cols()`, never by position.

### Phase 18z15 — the model checks (`R/reg-assumptions.R`)

Every `tab_reg()` footer now carries five **model checks**, on the framework the package already had:
each is the `Model_* vs Obs_*` comparison applied to something other than an effect — the SHAPE of a
numeric predictor's effect (Linearity), the SPREAD of the outcome (Dispersion), the MEANING of an
ordinal effect (Proportionality), the WEIGHT of one respondent (Influence). Collinearity is the stated
exception (a property of the design matrix, biasing nothing) and is in because every textbook and
jamovi's own pane put it first.

- **`REG_CHECKS`** — one row per check (`noun`, `types` = discriminator → instrument, `kind`/`digits`,
  `families`, `weighted_ok`, `per_predictor`, `cost`), read by `reg_checks_for()` (THE selection rule,
  the `reg_crude_shape()` pattern), `reg_check_spec_entries()` (the `reg_footer_spec()` entries) and
  `reg_check_expand()` (a user's KEY → the `test` discriminators). `names(REG_CHECKS)` IS the `stats =`
  vocabulary, so the footer label and the argument value cannot drift.
  **`cost`** (Phase 20f) is what each check charges: `"free"` = arithmetic on the fit already in hand
  (Dispersion / Influence / Collinearity), `"refit"` = it fits a model (Linearity, once per numeric
  predictor; Proportionality's Brant logits). Only the free ones are in the DEFAULT set —
  `reg_checks_default()`, whose one caller is `reg_footer_stats()`'s default composition, while
  `reg_check_rows()` still asks `reg_checks_for()`, so a check named in `stats` is computed and shown.
  Measured before the split: the two costly ones were 87 % of a default binomial table at n = 200 000.
  ⚠ `cost` is INDEPENDENT of `panel`: a panel is always free, so `reg_check_plots()` never filters on
  it. `stats = "all"` means every statistic and every applicable check (before 20f it was a synonym of
  the default set — a name that already lied). See `dev/tabxplor_reg_performance.md`. ⚠ `noun` / the instruments are
  BARE MSGIDS: a top-level `gettext()` would evaluate once at load and freeze the build locale, so
  `reg_check_label()` translates at render and a dead-code anchor keeps potools able to extract them.
- **No new statistic engine.** Linearity = `reg_fit(add_terms =)` (the third sibling of `cross =` /
  `drop_extra =`: extra RHS terms, joining the formula and nothing else) + **`reg_nested_test()`**,
  which compares the augmented fit with the `base_fit` already in hand. ⚠ It IS what `drop1()` returns,
  bit for bit on both arms (`expect_identical` in `test-reg-checks.R`): the LR arm doubles the logLik
  difference, the F arm divides by `deviance/df.residual` of the AUGMENTED fit, which is `drop1.glm`'s
  own dispersion at `scale = 0` and neither the Pearson one nor `anova()`'s. Before Phase 20f the row
  went through `reg_term_tests()` → `drop1()`, refitting the reduced model — i.e. `base_fit` — at 1.02 s
  against 0.028 s on 200 000 rows. `reg_term_tests()` (the dispatcher `global`/`interaction` use) is
  still the route for the design arm, where `survey::regTermTest()` refits nothing anyway. Its squared term comes from `reg_shape_term()`, which
  the z15-ii `shape = "quadratic"` remedy will emit — so the check and its cure are one object.
  Dispersion and Influence are ONE pass, `reg_check_influence_pass()` (Phase 20f): one `vcov()`, one
  influence closure and one sweep of the `p` unit contrasts, read two ways — `reg_if_se(d)` for the
  first, `max|d|` for the second. Computed separately they were four `vcov()` calls per fit, which on a
  multinomial fit is four `nnet:::multinomHess` re-derivations at 0.757 s each. The two footer ROWS
  stay two declared rows; only the arithmetic merged. Both are `reg_coef_if_maker()` + `reg_if_se()`
  (`max SE_robust/SE_model`,
  and `max |IF_i(e_j)|/SE_j` = dfbetas to correlation 0.999999 against `stats::dfbetas()`, working for
  `polr`/`multinom` and design-aware, which base R is not). Proportionality is the Brant p already
  stashed on the fit. Collinearity is `car::vif()` (a new Suggest; absent → no row).
- **The 13th `test` column `term`** = which predictor a footer row is about. It could NOT be `row_var`,
  which on a reg footer row already means the SPLIT-GROUP LEVEL in `reg_footer_lines()`, in
  `test_grid_reg()` AND in `reg_spread_models()` (which re-keys by it and drops the misses) — a
  predictor name there flipped a plain table into "split" mode and was silently deleted on a spread
  one. The retrofit of the interaction/global rows onto `term` also FIXED a live defect: `reg_build`'s
  split branch tags every row of a group's test tibble with the group level, so the global line used to
  print the split level, repeated, instead of the predictors.
- **`reg_footer_plan(reg)`** — THE ordered `(test, term)` row plan with its rendered label
  (`"<label>: <term>"` when per-predictor), read by BOTH row renderers so the console grid and the
  exports cannot diverge. Built from the whole slice, never per split group, because
  `tab_append_footer()` needs a constant block height.
- **`stats = "global"` moved from a footer LINE to footer ROWS** (`reg_global_lines()` and the shared
  `reg_term_test_line()` deleted). Measured on the vignette's data: in a 3-model comparison the line
  rendered as three sentences with nothing naming which model each described. The interaction test
  stays a line — it is pooled across split groups and belongs to no column.
- **`dispersion` / `phi`** — the key `dispersion` now names the CHECK (max robust/model SE, every
  family); the exact Pearson dispersion keeps its own row as `phi`, count families only. ⚠
  `reg_dispersion()` divides by `n - rank`, computed here, NEVER `stats::df.residual(fit)` — for an
  `svyglm` that is the DESIGN df, so the weighted-Poisson row read ~22 instead of ~1. The SE-scaling
  caller is gated `!weighted`, where the two agree, so only the weighted row moved.

#### z15-ii — `shape =`, the cure (same file)

`shape` is how a user fixes what the Linearity row finds, without leaving the framework. THE design
rule, and it is what keeps the whole feature to ~60 lines: **a shape either RECODES THE COLUMN or ADDS
ONE TERM, and nothing else.**

- **`reg_resolve_shape()`** parses the named vector against a closed vocabulary (`REG_SHAPES` +
  `reg_shape_k()` for an integer / `"quartiles"` / `"quintiles"`); **`reg_shape_apply()`** performs the
  recodes at ONE boundary in `tab_reg()` — placed before family detection, the reference relevel, the
  frozen multiplier SD and the skeleton, so every one of them sees the predictor AS FITTED. The
  design's own `$variables` are recoded too (`reg_relevel_design()`'s rule: a prebuilt design reads its
  columns off `$variables`, not off `data`).
  - `log` / `sqrt`: the column is transformed; the row LABEL says which (`shape_labels`, applied beside
    the multiplier relabel — the variable name is unchanged everywhere else).
  - `quartiles` / `quintiles` / integer k: `reg_cut_quantiles()` (WEIGHTED breaks through
    `rd_wquantile()`, one producer shared with the curves). The predictor genuinely becomes a FACTOR,
    so it inherits one estimate per group, a SATURATED crude twin, per-level N, per-level colours and
    adjustment gaps with **no code at all**, and `reg_predictor_types` records what it now is.
- **`quadratic`** is the only arm that emits a term: `reg_shape_terms()` → `shape_terms` (named by
  variable) rides `shared` to THREE consumers — `reg_skeleton(shape_terms =)` (the extra `age²` row,
  COEFFICIENT PATH ONLY: the marginal path emits one row per PREDICTOR, since an AME already integrates
  the curvature), `reg_fit(add_terms = reg_shape_add(...))` for the model, and the same for
  `reg_empirical_fit()`, so the crude twin's term names are IDENTICAL to the model's and
  `reg_skel_match()` aligns them unchanged.
  - ⚠ `reg_shape_term()` returns the **deparsed** string. A model-matrix column is named by the
    formula's own term label, which R produces by deparsing, and deparse drops the spaces around `/`
    that a pasted string keeps — the skeleton then missed the fit's term by two characters and the
    curvature row rendered EMPTY.
  - The linear term stays RAW: `a*x + b*((x-m)/s)^2` and `A*z + B*z^2` are the same model with
    `A = a*s`, so `multiplier = "sd"` (the default) already prints the per-SD slope of the centred
    parametrisation. The multiplier relabel is keyed on `term == var` so the squared row does not claim
    a unit it does not carry.
  - A cured predictor gets NO Linearity row (`reg_check_linearity_rows()` sets `num` minus
    `names(shape_terms)`): adding the same term twice is a collinear duplicate the engine drops.
  - `reref` is off when any shape is set — a shape is a DIFFERENT MODEL, not a reparametrization of the
    canonical one (unlike `reference` / `multiplier`, which are exact transforms of it).
- **`poly()` / `ns()` are never emitted**, and the escape hatch that can still reach one is guarded:
  `reg_basis_vars()` + `reg_marginal_basis_ok()` compare the returned AME against
  `mean(predict(x + k)) - mean(predict(x))` and warn on disagreement (`predict()` carries the basis's
  frozen `predvars`, which the perturbed-frame route loses — the measured "AME = 0.000000, silently").
  Paid only where a basis exists.

#### z15-iii — the curves, the row sparkline and `reg_check_plots()`

- **The primitives** (`R/reg-assumptions.R`): `rd_wquantile` (weighted quantiles, one producer for the
  bins, the panels and `shape = "quintiles"`), `rd_link_y` (the per-observation outcome on the family's
  own link scale — ordinal/multinomial read as "beyond the first category", stated in the axis label),
  `rd_bin` (weighted quantile bins + the THEORETICAL `2*sqrt(p(1-p)/n_eff)` band, not `arm`'s empirical
  one, which its own book does not describe and which ignores weights), `rd_spark` / `rd_spark_glyphs` /
  `tx_spark_strip`, `rd_resid` (ONE randomised quantile residual for five families; multinomial
  refused), `rd_qq` (the analytic Beta order-statistic band), `rd_thin` (extremes-first).
- **`meta$assumptions`** (`reg_curves()`, `get_assumptions()`): one observed curve per continuous
  predictor, ~1.6 KB, computed ONCE — it contains no fit, so a 5-model comparison stores five
  references to one tibble. Drawn on `skeleton_data`, never on `data`: under `split_var` the groups
  share one skeleton and are pivoted into columns BY ROW, so a per-group curve would give one row two
  different labels. With several outcomes it is NULL rather than the first outcome's silently.
  ⚠ The binary outcome is read at the MODELLED level (`fits[[1]]$positive_level`), never the factor's
  first level — reading the level order drew the curve of the COMPLEMENT.
- **The sparkline** rides the numeric predictor's own `levels` label (a NBSP + 10 glyphs), gated by
  `options(tabxplor.spark)` (`TRUE` / `"ascii"` / `FALSE`). Per medium, ONE site each: html upgrades it
  to a 121-byte inline `<svg><polyline stroke="currentColor">` in `tx_spark_svg()`, called at the
  ordinary-text-cell emission — **the glyph run IS the data**, read straight out of the rendered string,
  so there is no key to keep in sync and it survives transpose / `tab_spread` / any pipeline; the plot
  medium STRIPS it (`tx_spark_strip()` in `tab_plot()` — a graphics device has no block
  glyphs and emits one `mbcsToSbcs` failure per label); console, markdown and Excel keep it.
- **`reg_check_plots()`** (`R/plots.R`, z17's rename of `tab_reg_plots.R`; `lm_plots()` deleted).
  The panel set IS `REG_CHECKS`: it gained a `panel` field and TWO taught-but-never-scored rows
  (`residuals`, `normality` — measured non-discriminating as verdicts, canonical as lessons), which
  carry an EMPTY `types` and so contribute a panel and no footer row. `reg_checks_for(what = "panel")`
  is the same selection rule with a declared filter; `reg_panel_build()` is the one dispatch of HOW.
  It refits through `reg_fit()` itself, from `reg_meta$fit_spec` (~4 KB of strings — never the fits:
  ~10 MB each was Phase o's measured jamovi freeze), and ABORTS when the data does not reproduce the
  stored N. `tx_plot_colors()` / `tx_plot_theme()` are the z11 `tx_chrome_hex()` vocabulary (the five
  hard-coded `"#c00000"` literals are gone); z17 renamed them off the `reg_` prefix, since a crosstab
  chart shares them.


### Phase 18z17 — `forest_plot()` and the estimate model

The package's first real data CHART. Everything it needed was already stored per cell and per column;
what was missing was one fact — **an estimate is a number plus a scale** — which four consumers each
re-derived half of (`format()`, `fmt_color_plan()`, the legend, and `or_plot()`'s private ladder).

- **`EST_SCALES` + `est_scale_key()` + `fmt_scale_of()`** (`R/fmt_class.R`, beside the colour engine).
  Nine declared scales (`or`, `pct_ratio`, `mean_ratio`, `raw_diff`, `mean_diff`, `log_coef`, `points`,
  `level_pct`, `level_mean`), each saying its neutral, transform, axis unit KEY, estimate field, the
  break ladder the ESTIMATE lives on and the `adj_*` ladder its GAP reads. ONE dispatch, whose ORDER is
  the contract (the `var` clause must precede the log-coefficient one — see the comment); a `kind`
  argument filters it for `forest_plot(what =)`, and a `display` clause covers the intervalless case
  (`tab(OR = TRUE)`'s reference column, whose OR bounds are NA by construction, used to read as a
  percentage and decide its panel's axis). **`fmt_gap_scale_key()` and `ci_center()` are now lookups on
  it**, so the three cannot drift; `fmt_center_field()` is the estimate-field half, which `ci_center()`
  calls WITHOUT a display because it answers about the INTERVAL, not the column.
- **`tab_estimates()`** (`R/plots.R`, internal, reachable as `forest_plot(return_data = TRUE)`): one
  long tibble, one row per (table row × plotted column). It computes NOTHING. Column axis = `role` +
  `col_var` + `is_totcol`; row axis = `tab_render_vars()` + `tab_row_roles()` over the declared label-block
  shapes; scale = `fmt_scale_of()`; colour = `resolve_color_channel_plans()` + `fmt_col_ann()` (the
  EXPORTERS' resolver, so a point is the cell's colour down to the greys). The facet key is derived
  once (ruling D7): `col_var`, unless a `col_var` holds several columns of the SAME role — multinomial
  categories, or a crosstab's column levels — in which case one panel per column; a crude block serving
  several models is replicated into each model panel.
- **The gap band** = `obs (± | ×/÷) z·gap_se`, so "the modelled point falls outside the bracket" is
  exactly `fmt_gap_p(x) < 1 - conf_level`, to machine precision (asserted cell by cell). Ruling D2 made
  `gap_se` a fact of validity rather than of who asked to colour it: the `sp$color` clause left
  `reg_gap_se_columns()`, which costs ~1/8 of a fit because the crude univariable models were already
  being fitted for the crude column.
- **`legend_guide_spec()`** (`R/fmt_class.R`): the colour legend as a real ggplot GUIDE, from the
  legend's own producers (`legend_specs` → `legend_resolve_spec` → `legend_break_tokens`, which already
  drops a break rendering identically to the previous one). ONE ggplot scale per aesthetic means one
  ladder per key list, so it returns NULL when the plotted columns form several `legend_group_by_body()`
  groups and the caption prints the prose legend instead. `fmt_point_palette()` is the one deviation
  from the table palettes and it is forced: `theme = "print"` gives every TEXT slot pure black and
  separates the directions by typography, which a point cannot carry — so a MARK borrows the print
  palette's dark grey ramp, and direction is read off the axis, which a table cannot do.
- **`forest_plot()`** returns ONE `ggplot` (never a `gtable`), so `+ theme()` / `ggsave()` work.
  `R/tab_reg_plots.R` became `R/plots.R`; `reg_plot_*` became `tx_plot_*`; `or_plot()` is DELETED
  (ruling D1, never released). Also `tab_export(format = "forest")`, and `ggplot2 (>= 3.5.0)` for
  `transform =` / `sec_axis(transform =)`.


## Phase 19n — `col_group`: which sub-population a column's block belongs to

The 16th per-column fmt attribute, and the last **welded** fact in the package. Two producers turn a
variable into side-by-side blocks — `tab(spread_vars =)` / `tab_spread()` (a `tab_var` level) and
`tab_reg(tab_vars =)` (a split group) — and both used to fold the level into the column's `col_var`
as `"{level}<br>{col_var}"`. Three backends then recovered it by **sniffing for that html tag**
(`tab_xl()`'s two-line span and its `wrap_text` flag, `legend_name_list()`'s name normaliser) while a
fourth un-escaped it back after `htmlEscape()` — and `tab_wrap_text(brk = "<br>")` produces the very
same tag for an unrelated reason (a long level label), which none of them could tell apart.

The two facts are stored apart now (`get_col_group()`; the setter is internal, writing being the
pipeline's job) and **composed where a two-line header is actually wanted**: `tab_col_var_header()`
returns `group` beside `label`, `tab_header_runs()` run-length-encodes the PAIR (RLE-ing the label
alone would merge two adjacent blocks of one variable into a single span), html emits a `<br>`,
Excel a newline + wrap, markdown the one-line `fmt_col_block()$label` form it can draw. `<br>` in a
header now means exactly one thing.

⚠ **The block identity is the PAIR, and it has three carriers, not one.** Besides the attribute and
the span, the `test` tibble keys its rows on `col` — which identified a block only while the weld
existed. It carries a **declared** `col_group` column too (declared, because `test_group_cols()` is
`setdiff(names(tt), names(new_test_tibble()))` and an undeclared column would be read as a grouping
variable), and `test_grid_crosstab()` keys `value_cols` on the pair through `tab_col_blocks()`. On
`col` alone, a spread table's two blocks collapse to one and the grid emits one p-value column for a
table that has two.

## Phase 19g (KEY 6) — one table identity

`meta$spec = list(kind, vars, call)` (`R/table-spec.R`) is what a table says about *itself*, for both
producers. `kind` (`"crosstab"`/`"regression"`) is stated by the producer and read through
`tab_kind()`/`tab_is_reg()`; the old `is_reg_footer()` sniff of the `test` tibble survives only as
`tab_kind()`'s fallback for a table that lost its metadata. `vars` (was `meta$vars`) keeps only what
no column can carry — `wt`, `caption`, `var_labels` — the row axis being derived from the declared
`tabxplor_lvl` index columns (19f) and the column axis from the fmt columns' own `col_var`. `call`
(was `meta$reg_meta`, read through `reg_call()`) is the producer's recipe, including `fit_spec`;
a crosstab records none yet (19i).

The `test` tibble is keyed uniformly: **`var`** (which variable — a crosstab's row variable, a
regression's predictor, `""` = whole table/model; it absorbed z15's `term`), **`col`** (which column
it keys under) and one column **named after the grouping variable** for the sub-population (tab_vars
for a crosstab, `split_var` for a regression), read by `test_group_cols()`.

`reg_build()` has ONE assembly tail (`reg_finalize()`, shared with the split branch) and ONE column
assembler (`reg_cols_ame` / `reg_cols_vsrest` / `reg_cols_coef` behind a **per-spec** choice, the
`REG_BUILDERS` vocabulary); its settings ride the typed `new_reg_shared()` record and its stage
products the typed `new_reg_ctx()` one (Phase 20e), both deriving their own `globalVariables()`
mirror. The
`stats =` / `check =` vocabulary is `reg_stat_keys()` with one validator.
