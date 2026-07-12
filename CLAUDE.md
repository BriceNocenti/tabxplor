# tabxplor — AI Assistant Guide

## Package Purpose

`tabxplor` is a public CRAN R package (v1.3.1) maintained by me for creating, manipulating, and formatting color-coded cross-tabulations. It uses `data.table` for fast aggregation internally and exposes a `tidyverse`-compatible API via custom `tibble` subclasses. Tables can be exported to Excel (with colors), HTML, Markdown, and ggplot. All numeric cells are stored as `vctrs` record vectors (`tabxplor_fmt`) carrying full statistical metadata, enabling lossless display switching.

---

## Repository Map

```
R/
├── fmt_class.R     (3341 L)  Core type: tabxplor_fmt vctrs record, getters/setters,
│                              format/pillar methods, vctrs arithmetic/casting,
│                              color selection logic (fmt_color_selection, color_formula)
├── tab.R           (~6200 L) Main API: tab(), tab_many(), tab_plain(), tab_num(),
│                              tab_apply_reference() (Phase 7f carve; Phase 9d: matrix-sweep internals),
│                              leaf_wide_pct() + build_total_rows()/finalize_total_rows() (Phase 9d:
│                              base-R/matrix leaf math for tab_plain pct/tot_n + total rows),
│                              tab_prepare(), tab_pct(), tab_ci(), tab_chi2(),
│                              tab_tot(), tab_totaltab(), tab_spread(), tab_get_vars(),
│                              tab_render_vars() (Phase 10c: robust group_vars-based role detection +
│                              graceful degrade, used by print + exporters),
│                              tab_add_n_pct() (shared add_n/add_pct, used by tab_many + tab_counts).
│                              tab_build() = staged pipeline: tab_setup / tab_prepare_pop / tab_aggregate
│                              / tab_build_tables (Phase 9a: the OUTER row_var map -> tab_build_one, +
│                              tab_rowvar_ctxs) ; tab_transform / tab_assemble_tables are SCALAR over one
│                              row_var ; tab_assemble_output (merge/pvalue/unwrap);
│                              tab_lump_others/tab_cleannames_relabel (extracted from tab_prepare)
├── tab-agg.R        (~470 L) Aggregate-core (Phase 2-3): num_derive_stats/num_rollup, num_moment_scan
│                              + tab_aggregate_num (numeric tier-1 producer, Phase 7d-i),
│                              CI engine (ci_pivot/ci_wilson/ci_newcombe/…), agg_chi2/agg_anova
├── tab-counts.R     (~360 L) tab_counts() from-the-middle constructor (Phase 4): reshape any
│                              input shape → count-aggregate → tab_plain(.fine) + shared finalize
├── tab-resolve.R    (~180 L) tab_resolve_settings() (Phase 7b): the ONE pure arg-overwrite
│                              cascade (color="auto"/forcing/split) shared by tab_build+tab_counts;
│                              resolve_color_auto_num() (numeric arm). The jmvtab .js / cache boundary.
├── tab-parallel.R   (~200 L) Phase 8/9a row-axis dispatch (Suggests-only mirai): tab_pmap() + trampoline,
│                              named "tabxplor" pool (tab_pool_ensure/tab_parallel_workers/
│                              tab_parallel_stop), tab_build_one() (the per-row_var worker, serial OR mirai).
├── tab_classes.R   (3554 L)  tabxplor_tab/grouped_tab classes, 30+ dplyr S3 methods,
│                              print methods, tab_kable(), tab_plot(), tab_compact(),
│                              color palettes, set_color_style(), set_color_breaks()
├── tab_xl.R        (4132 L)  Excel export via openxlsx (Suggests-only)
├── tab_md.R         (366 L)  Markdown export (standalone, new in v1.3.1)
├── tab-export-prep.R (~400 L) Phase 10d shared exporter prep: tab_export_prep() -> tabxplor_render
│                              model (roles/ann/bold/range/labels), consumed by kable/md/plot/xl
├── tab-render-html.R (~350 L) Phase 10e tab_kable render seam: render_kable_html() (kableExtra +
│                              home-built self-contained html engines) + tab_kable_join/scrollbox
├── utils.R         (1306 L)  Pipe re-export, .onLoad() options setup, factor utilities
├── tab_logit.R     (1009 L)  WIP — entirely commented out (future logistic regression)
├── tab_logit_2.R    (706 L)  WIP — entirely commented out (logit diagnostics/plots)
├── jmvtab-cache.R  (~800 L)  jmvtab live multi-tier cache: content-addressed store + hashing +
│                             jmv_cache_aggregate (tier 1-2, tab_aggregate hook) + the Phase 7f
│                             tier-3 CARRIER cache (Phase 9b-7: jmv_carrier_unwrap/wrap store, not a
│                             live tab; jmv_tab3_base_key/tuple, jmv_reapply_digits re-paint +
│                             jmv_tab3_reref/rerefable instant reference re-ref) + jmvtab_build
│                             (engine-free core; reuses tab() via .cache) + jmvtab_ref_vector (ref-picker)
│                             + jmvtab_levels_order/jmv_relevel_cols (7g-ii level-reorder,
│                             post-aggregate; .levels_order arg on tab())
├── jmvtab-export.R  (~120 L)  jmvtab export helpers (Phase 7g): resolveExportPath (typed path →
│                             Documents/USERPROFILE), tab_html_string (self-contained HTML),
│                             jmvtab_export (Excel/HTML/MD dispatch)
├── jmvtab.b.R       (~200 L)  Jamovi module backend (R6): thin orchestrator over jmvtab_build + $state
└── jmvtab.h.R       (605 L)  Jamovi module UI (auto-generated, do not edit)
```

**Other directories:**

| Directory         | Purpose                                                                                |
|-------------------|----------------------------------------------------------------------------------------|
| `vignettes/`      | User intro (`tabxplor.Rmd`)                                                            |
| `tests/testthat/` | testthat v3 tests                                                                      |
| `man/`            | Auto-generated by roxygen2 (never edit by hand)                                        |
| `inst/i18n/`      | Internationalization resources                                                         |
| `jamovi/`         | Jamovi module definition files                                                         |
| `po/`             | Translation files                                                                      |
| `dev/`            | architecture guide + dev scripts + perf harness (`dev/benchmarks/`), `.Rbuildignore`'d |

**Cross-cutting dependencies** (be careful when modifying):

- `fmt_class.R` — used by everything; the `tabxplor_fmt` class is the foundation
- `tab_get_vars()` in `tab.R` — used by all export functions (tab_xl, tab_kable, tab_md, tab_plot)
- `get_color_style()`/`set_color_breaks()` in `tab_classes.R` — shared between `fmt_class.R` (console) and `tab_xl.R` (Excel)
- `.onLoad()` in `utils.R` — sets all default options; changing defaults affects every user

---

## Global Architecture

```
tab() [user-friendly wrapper]
  └── tab_many() [full-featured, vectorised over row_vars and col_vars]
        └── per row_var:
              tab_prepare()  →  tab_plain() / tab_num()  →  tab_pct()
                →  tab_ci()  →  tab_chi2()  →  tab_totaltab()
                      →  tab_spread() / tab_compact()

Export:  tab_xl()  |  tab_kable()  |  tab_md()  |  tab_plot()
```

> **This is the *current* pipeline. 1.4.0 rewrites it around a single aggregate-core** (see roadmap § Keystone + `dev/tabxplor_1.4.0_decisions.md`): the step chain `tab_pct → tab_ci → tab_chi2 → …` collapses into one core, and `tab_pct`/`tab_tot`/`tab_ci`/`tab_chi2` become superseded thin wrappers.

**Ordering invariant** (in `tab_many()`, `tab.R` ~L1146): `tab_chi2()` and `tab_ci()` are independent (either order), but non-first levels (`levels="first"`) must be dropped **after both**, so chi2/ci are computed on the full set of levels. Do not move the level-drop above chi2/ci.

### Key Constraints

| Constraint               | Detail                                                                                                                                                                                                                                                                                                                                                                                            |
|--------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| CRAN stability           | Public function arguments must NOT change without deprecation. Internals can change freely.                                                                                                                                                                                                                                                                                                       |
| vctrs record contract    | Adding a field to `tabxplor_fmt` requires updating `new_fmt()`, `fmt()`, `format.tabxplor_fmt()`, `pillar_shaft.tabxplor_fmt()`, `vec_arith` methods, and possibly `tab_pct()`/`tab_ci()`/`tab_chi2()`. ~8 functions across 3 files.                                                                                                                                                              |
| NAMESPACE                | Auto-generated by roxygen2. Never edit `NAMESPACE` by hand. Run `devtools::document()` after changing `@export`/`@import`.                                                                                                                                                                                                                                                                        |
| data.table internals     | `tab_plain()`/`tab_num()` rename `col_var` to internal names to avoid data.table conflicts. The user's column names are restored afterward.                                                                                                                                                                                                                                                       |
| dplyr class preservation | 30+ S3 methods on `tabxplor_tab`/`tabxplor_grouped_tab` ensure class + attributes survive all dplyr verbs. Missing a method = silent class downgrade to `tbl_df`.                                                                                                                                                                                                                                 |
| Options as config        | All defaults set in `.onLoad()` in `utils.R`. Users override via `options()`. Functions read with `getOption()`.                                                                                                                                                                                                                                                                                  |
| Suggests-only guards     | `openxlsx`, `ggplot2`, `jmvcore`, `ggpubr`, `cowplot` are in Suggests. Every call must be guarded with `requireNamespace()` or equivalent.                                                                                                                                                                                                                                                        |
| Color break mirroring    | `set_color_breaks()` takes positive-only thresholds. Negative breaks are auto-mirrored internally. Any `pct_breaks` value > 1 triggers ratio comparison instead of difference (the "*2 rule").                                                                                                                                                                                                    |
| Mean-diff asymmetry      | For `type="mean"` columns, the `diff` field stores a **ratio** (cell_mean / ref_mean), NOT a difference. Thresholds like 1.15 mean "+15% above reference". This asymmetry propagates into `color_formula()` and `format.tabxplor_fmt()`. **(1.4.0 §3: numeric `diff` becomes a real difference; the ratio moves to the `ratio` field — the never-used `rr` field renamed, placed after `diff`.)** |
| tab_logit                | Entirely commented out (WIP). Do not try to use or integrate. Will be developed in the future.                                                                                                                                                                                                                                                                                                    |


---

## Design Decisions

### Type System

- **`tabxplor_fmt`**: vctrs record (`new_rcrd()`) with **18 per-cell fields** (was 15 before v1.4.0 Phase 1a) and 8 per-column attributes. The critical distinction: fields vary per cell (accessed via `vctrs::field()`), attributes are scalar describing the whole column (accessed via `attr()`). Constructor chain: `fmt()` (public, validates + coerces) -> `new_fmt()` (internal, calls `vctrs::new_rcrd()`). *(Phase 1a reshaped 15→18 in one combined pass — decisions doc §9; `ci` is now derived from the `ci_inf`/`ci_sup` bounds by `get_ci()`, a bounds-shim.)*
- **`mean` field overload** (cross-cutting): for **pct-type** columns the `mean` field carries the cell/reference **ratio** for the "*2 rule" (not an actual mean). Written by `tab_pct()`, read by `fmt_color_selection()`. The **`ratio` field** now exists (Phase 1a renamed the never-used `rr`→`ratio`; decisions doc §3); the overload removal + moving the ratio to `ratio` lands in **Phase 5** (color diff/ratio split), not yet done.
- **`tabxplor_tab`**: tibble subclass via `tibble::new_tibble()` with `subtext` (legend text) and `chi2` (test results tibble) attributes. *(1.4.0 §16: the `chi2` attribute is hard-renamed `test`, also carrying the new ANOVA/Welch F — lands Phase 3.)*
- **`tabxplor_grouped_tab`**: extends `grouped_df` for subtabled results (when `tab_vars` are present). Requires separate S3 method for every dplyr verb.

### Export Parity

Cell display values reach exporters by two **non-unified** paths — keep them in sync:

- **`format.tabxplor_fmt()`** (`fmt_class.R`) is the single source of truth for markdown (`tab_md()`), knitr/HTML (`tab_kable()`), and the console (`pillar_shaft`).
- **`tab_xl()`** (Excel) BYPASSES it: it reads `get_num()`/`get_display()`/`get_digits()` directly and delegates numeric formatting to Excel's engine. Any change to a display field or formatting rule must be mirrored in `tab_xl.R`.
- Color is safe: all exporters call the same `fmt_color_selection()`.

When adding or changing a `tabxplor_fmt` field, follow the `/vctrs-field` skill — it encodes the full ~11-step checklist across `fmt_class.R`, `tab.R`, and the exporters.

### Reference System

The `ref` argument controls which row serves as the comparison baseline for differences/colors:
- `"auto"`: defaults to `"first"` when OR requested, `"tot"` otherwise
- `"tot"`: total row is the reference
- `"first"`: first non-total row
- integer: specific row index
- regex string: matched against row labels
- `comp="tab"` compares within each subtable; `comp="all"` compares against the total table

Note: `ref` is **reinterpreted by `pct`** — a reference **row** under `pct="row"`/means, a reference **column** under `pct="col"`. 1.4.0 makes `ref` a per-row_var named vector (row%/means only) and stores each cell's own base as `tot_n` — see decisions doc §2, §4.

### Color System (3-layer)

1. **Palettes** (`tab_classes.R` ~L2892): 6 named color vectors (dark/light text, 24-bit blue-red/green-red, dark/light background), each with 11 hex codes: `pos1`-`pos5` (over-represented), `neg1`-`neg5` (under-represented), `ratio`. Hues are hand-tuned so intensity levels are eye-distinguishable on real tables; 8-bit variants target non-truecolor terminals; the 24-bit blue-red variant is more colorblind-friendly than green-red (fuller colorblind support is a future goal).
2. **Breaks** (`set_color_breaks()` in `tab_classes.R`): stored in `options("tabxplor.color_breaks")`. Default pct: `c(0.05, 0.1, 0.2, 2, 0.3)` — the `2` means "twice the reference" (ratio mode). Mirrored for negative. Mean breaks: `c(1.15, 1.5, 2, 4)` — always ratios. *(1.4.0 §18 adds `mean_diff_breaks` `c(0.2, 0.5, 0.8, 1.2)` — sd-standardized differences for the numeric diff mode, Phase 5.)*
3. **Selection** (`fmt_color_selection()` in `fmt_class.R`): iterates breaks, applies `color_formula()` per break level, `keep_last_break()` picks the strongest matching threshold per cell. Different boolean formulas for each color mode: `diff`, `diff_ci`, `ci`, `after_ci`, `contrib`, `OR` (+ the 1.4.0 additions `ratio`/`diff_ratio`, Phase 5).

### dplyr Integration

The `dplyr_row_slice()` / `dplyr_col_modify()` / `dplyr_reconstruct()` trio in `tab_classes.R` is the core mechanism preserving `tabxplor_grouped_tab` class through dplyr operations. When the table has only one grouping level left, `lv1_group_vars()` detects this and downgrades to plain `tabxplor_tab`. Every new dplyr verb needs a corresponding S3 method — check `NAMESPACE` for the full list.

### Deprecation and retro-compatibility

#### For main user-facing functions and arguments
- This package have a small but existing users base : **soft deprecate main user-facing functions and arguments carefully** to ensure retro-compatibility.
- Some user code rely on `tabxplor_fmt` vctrs fields extracted with `$` or calculated with `mutate()` method for `tabxplor_fmt` (see readme), so **the vctrs fields should not break**.

#### For internal code and internal functions
- **Do not hesitate to propose radical redesign of internal code and internal workflows** for quality, simplicity, structure, performance and future-proofing, specially when they are too convoluted or have grown organically.
- **Always try to simplify, integrate and create smart shared subfunctions** instead of adding a new layer of confusion and ad-hoc solutions inside the code: your main aim is to simplify, to remove traces of old implementations altogether when they have become useless, to clarify, to help me make relevant architectural choices instead of piling up ad-hoc solutions, to integrate the new features in the current code seamlessly.

---

## Key Dependency APIs to read up on

Before working on the `tabxplor_fmt` type system, arithmetic, or display, fetch the help pages for these via the `r-btw` MCP **docs** tools (or `?`) — the model's recall of their exact current contracts is the weakest link:

- `vctrs::new_rcrd`, `vctrs::field` — record type and per-cell field access
- `vctrs::vec_arith`, `vctrs::vec_cast`, `vctrs::vec_ptype2` — arithmetic and casting S3 contracts
- `pillar::pillar_shaft` — console display method
- `data.table` reference semantics (`:=`, `.SD`, `.N`) — internal aggregation
- `DescTools::BinomCI`, `DescTools::BinomDiffCI` — **now Suggests-only** (test parity only). Since Phase 3a the CI math is the closed-form engine in `R/tab-agg.R` (`ci_pivot`/`ci_wilson`/`ci_newcombe`); read it, not DescTools, before touching CI.

---

## Testing

```r
# In a temp .R file (outside tests/), then run:  Rscript that_file.R   (isolated; tests live source)
devtools::test("d:/Statistiques/github/tabxplor")                  # whole suite (~46s)
devtools::test("d:/Statistiques/github/tabxplor", filter = "tab")  # one/few files: regex on test-<name>.R
```

**Test files:**

| File                 | Coverage                                                            |
|----------------------|---------------------------------------------------------------------|
| `test-fmt_class.R`   | fmt creation, printing, type conversion, c(), arithmetic            |
| `test-tab.R`         | Core: plain tables, pct, totals, NA, CI, chi2, references, wrapping |
| `test-tab_classes.R` | Class preservation through dplyr verbs                              |
| `test-tab_xl.R`      | Basic Excel export                                                  |
| `test-tab_logit.R`   | Inactive (commented out, mirrors WIP code)                          |

---

## Jamovi module development

tabxplor currently use jamovi `2.6.44.0` (solid). Version 1.4.0 will also be tested on jamovi current "solid" version `2.7.37` afterwards.

After you modify jamovi functions and configuration files, regeneration of `.h.R` file is interactive step only. Ask the maintainer to install it using this code, but do not do it yourself :

```r
options(jamovi_home='C:/Program Files/jamovi 2.6.44.0')
devtools::load_all() ; jmvtools::install() ; devtools::load_all()
```

To know the real structure of the final .html and .js, check at this live capture done from dev console (for a basic table) :
- `dev/jamovi/dev_console_live_capture/Jamovi_tabxplor_1_3_1_basic_table.html` : the live html from tabxplor 1.3.1 jamovi module
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56680_MAIN_ELECTRON/` : the exported main election scripts
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56683_tabxplor_jmvtab_analysis_UI/` : the exported tabxplor jmvtab analysis UI scripts
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56684_results/` : the exported jamovi "results" panel scripts (where the actual table appears)

To **capture new html** in the dev console, **ask the maintainer whenever you need**.

Look at `D:/Statistiques/github/tabxplor/dev/tabxplor_1.4.0_jamovi_dev.md` and `@dev/jamovi/` for detailed informations.


---

## Common tabxplor package Development Issues

| Issue                                   | Solution                                                                              |
|-----------------------------------------|---------------------------------------------------------------------------------------|
| R CMD check NOTE about global variables | Add to `globalVariables()` call in `fmt_class.R` (for data.table's `:=`, `.SD`, `.N`) |
| magrittr `%>%` vs base R pipe           | Prefer base R pipe for new code, examples, etc. Package re-exports `%>%` for users.   |
| New vctrs type combination doesn't work | Need both `vec_ptype2.*` and `vec_cast.*` S3 methods for every type pair              |
| dplyr verb silently drops class         | Missing S3 method for `tabxplor_grouped_tab` — add one in `tab_classes.R`             |

---

## Architecture Technical Guide

For the full detailed technical reference, see `dev/tabxplor_architecture.md`, which documents every subsystem in depth. Read it whenever needed and keep it up-to-date.



---

## tabxplor version 1.4.0 roadmap : the current goal

Currently implementing tabxplor 1.4.0 (2.0.0 only if breaking changes land). **Update the sections below at the end of every work session.**

### The aim of 1.4.0 — read first, it governs every decision

This version exists to **refactor and simplify `tab()`/`tab_many()`** — the two functions that matter — by **stripping the white-elephant flexibility that real-world data analysis never uses**, and **redesigning the underlying `tabxplor_fmt` vctrs-field architecture** (one combined field pass) to fit the simpler, faster model. The governing rule, non-negotiable:

- **Public API stays retro-compatible.** User-facing functions, their arguments (soft-deprecate, never hard-break), and the `tabxplor_fmt` fields users read with `$`/`mutate()` keep working.
- **Internals are redesigned as radically as needed** for consistency, simplicity, and performance. Do **not** preserve internal structure, dead code, or the old step-by-step (`tab_pct`→`tab_ci`→…) paths for their own sake — remove them, fuse them, route everything through the one aggregate-core. Whenever a choice trades never-used internal flexibility for a single well-defined faster path, take it.

Every phase and decision below serves that aim: fewer knobs, one computation core, a field set shaped to the real use cases.

### Start here (reading order + where docs live)

This roadmap is the **plan of plans**: the phased implementation order plus every open question. A fresh session asked for a *part* of the work should read, in order:

1. **This roadmap** — the phase your task belongs to, its bullets, and its pointers; the full 1.4.0 analysis (grounding, keystone, decisions, verification) is right below.
2. **`dev/tabxplor_1.4.0_decisions.md`** – the **new architecture decisions** taken for version 1.4.0. **Always read carefully**.
3. **`dev/tabxplor_architecture.md`** — architecture guide (type system, pipeline, compaction loss, exporters). It describes the **current** architecture. Read the section matching the file you touch.
4. **Top of this CLAUDE.md** — Repository Map, Global Architecture, Key Constraints, Design Decisions.

**Other long-form 1.4.0 docs live in `dev/` (all `.Rbuildignore`'d), never inline here — read the matching ones before you start:**
- `dev/benchmarks/` — performance harness + saved results (documented under *Reference > Benchmarks*). Read/run when a phase touches perf (Phases 2, 3, 6, 8).
- `dev/benchmarks/tab_many_performance_profile.md` — the full 2026-07 profile. Read before optimizing `tab_many` / `tab_chi2` / `tab_num`.

### Settled architecture decisions (2026-07 planning session)

#### Why — current-state grounding

- **Two math paths, duplicated**: `tab_plain`/`tab_num` compute pct/diff/OR/totals inline with data.table (`tab.R` ~L2491-2678); the legacy `tab_pct`/`tab_tot`/`tab_totaltab` recompute the same math via dplyr and are **not called by `tab_many()`** — the percentage/total logic exists twice.
- **CI/chi2 outside the fast path**: `tab_ci` (proportions) uses `dplyr::across` + per-cell `DescTools::BinomCI` (`tab.R` ~L4934); `tab_chi2` uses `group_split` + per-column `chisq.test` (~L5274). `tab_num` already folds *mean*-CI into data.table via closed-form `ci_mean = zs*sqrt(var/n)` (~L3771) — the template to copy.
- **No from-the-middle entry**: only the low-level `fmt()` builds cells from numbers; abusing `wt=count` leaves `n=1` per cell and silently breaks CI/chi2.
- **Output type inconsistency**: `tab.R:1540` unwraps a length-1 list to a bare tab (bare tab if 1 row_var *or* `compact`; a list only if ≥ 2 row_vars *and* not compact).
- **Exporters**: no shared prep — `tab_kable`+`tab_md` duplicate a "canonical col_vars → validate → compact" preamble; `tab_xl` keeps a list-of-sheets; `tab_plot` needs a pre-compacted tab.

#### Keystone — the aggregate-core

One internal canonical representation — a keyed count-aggregate (`n`, `wn` per `tab_vars × row_var × col_var-cell`, NA kept; **for numeric col_vars this must be a sufficient-statistics aggregate carrying moment-sums `Σwt·x`, `Σwt·x²`, NOT counts — else means/var/CI/t can't be recovered and the `weighted.var` double-scan survives; plus `Σwt²` on both branches for Kish `n_eff` (§14 weighted inference); unweighted moment-sums dropped (review 4 — §14 uses weighted dispersion only); open item G1**) — and one pure core turning `(aggregate, settings)` → fmt columns. Both entry points converge on it:

```
microdata ─ tab_prepare ─┐
                         ├─► count-aggregate ─► [pct | diff | OR | CI | chi2 | totals] ─► fmt cols ─► tab
counts (long/wide/freq) ─┘   via as_tab_counts()   (one vectorised impl each)
```

Why it is the keystone — it simultaneously (a) kills the duplicated pct/total math; (b) makes from-the-middle reliable (validate once at the boundary, then the identical core runs); (c) lets CI/chi2 join the fast path (aggregate-based, `tab_num` mean-CI template); (d) gives `tot_n` (each cell's own % base) almost for free (a property of a proper aggregate, not "the last `col_var` total column"); (e) defines the clean Jamovi cache boundaries (aggregate | per-transform | display).

**Conceptual vs physical**: the core is always aggregate-based (conceptual). The physical shared finest-grain `.fine` aggregate (fusing per-table scans) is Jamovi-reuse + `tab_counts()`-injection only. *(Phase 9c: the tab()-level opt-in scan-fusion switch — `options(tabxplor.fuse_min_rows)` — was REMOVED as a net-negative (§30); the `.fine`/`fine_for_pair()`/`use_raw` seam in `tab_plain()`/`tab_num()` remains for jmvtab, `tab_counts()`, and the numeric `fine_num`.)*

**Retro-compat guardrails**: `tabxplor_fmt` fields are the user contract (extracted via `$`/`mutate`) — must not break. Public args must not change without deprecation. `tab_pct`/`tab_tot`/`tab_ci`/`tab_chi2` stay exported but become superseded thin wrappers over the core (`lifecycle::signal_stage`), so old user code keeps working.

#### The decisions

- **Output shape**: `output_list` (default `FALSE`) replaces `compact`; `compact` deprecated (arg), `tabxplor.compact` option removed. Compact-loss analysis persisted in `dev/tabxplor_architecture.md` ("Compaction: what is lost when tables are bound"). Verdict: single-table default only gives up per-row_var flexibility real analysis never uses (divergent color/ref/ci-type on the *same column*); each-variable-vs-own-total is preserved. When `tab_vars` present, compaction can't merge → keep multi-table regardless.
- **Field surgery = one combined pass** (before the core rewrite) → **18 fields**: add `pvalue`, `tot_n`, `ci_inf`, `ci_sup`; **rename the unused `rr`→`ratio`** (placed after `diff`); **drop `ci`** (recomputed on `$`/`get_ci()` from the bounds; `fmt(ci=)` arg kept); numeric `diff` becomes a difference; `mean`-overload removed. CI is stored as asymmetric **bounds** (the single upper-half-width + symmetric bracket is wrong for Wilson/AC proportions; means exact); OR CIs move off their sidecar into the fields. **Per-cell significance is a stored `pvalue`** (Q2 — three star levels can't come from one CI level, and are undefined from bounds for asymmetric proportions/OR; decisions §12): factor `ci="diff"` = two-proportion score test, numeric `ci="diff"` = Welch t, empirical `OR` = log-OR Wald, logit = model p. Do NOT pre-add se/z/coef (tab_logit never displays them). After this pass tab_logit needs no further field surgery. Detail: `dev/tabxplor_1.4.0_decisions.md` §1-3, §12.
- **From-the-middle constructor** (`as_tab_counts()`): support long tidy counts, wide count matrix, frequencies+base N. Validate once at the boundary → same core. Require real unweighted `n`; warn/disable CI/chi2 on frequency-only input.
- **Order**: 0 finish safety net → 1 combined field pass → 2 aggregate core + math unification → 3 CI/chi2 onto aggregate (headline perf) → 4 counts constructor → 5 color diff/ratio split → 6 tab()→tab_many() merge + output_list → 7 unified exporter prep (on openxlsx v1) → 8 Jamovi caching → 9 Excel engine swap openxlsx→openxlsx2 (isolated; may slip to a 1.4.x follow-up). Each phase: golden/parity green + **save before/after benchmarks** (`dev/benchmarks/results_1.4.0/`).

#### Resolved architecture decisions (2026-07)

Grounding (code refs + statistics + caveats) in `dev/tabxplor_1.4.0_decisions.md`. Summary:

1. **fmt fields** (Phase 1, §1-3, §12) — one combined pass → **18 fields**: add `pvalue`, `tot_n`, `ci_inf`, `ci_sup`; rename unused `rr`→`ratio` (after `diff`); drop `ci` (recomputed from bounds on `$`/`get_ci()`; `fmt(ci=)` arg kept); numeric `diff` = difference; `mean`-overload removed.
2. **CI = bounds + `pvalue`** (Phase 3, §1, §12) — store asymmetric `ci_inf`/`ci_sup`; the current upper-half-width + symmetric bracket mis-draws Wilson/AC proportion CIs (means exact). **Per-cell significance reads the stored `pvalue`** (three star levels need a real p, undefined from one CI level for asymmetric proportions), not the bounds; compact `± moe` shows the larger arm; tab_logit OR-CIs move into the fields (sidecar retired).
3. **`tot_n`** (Phase 1-2, §2 — renamed from the roadmap's `ref_n`) — each cell's OWN unweighted % base (its row/col total, *not* the diff-reference's n). Stored; the weighted base `tot_wn` is recovered as `wn/pct` (not a field). Retires `detect_totcols` on built tables. Only load-bearing for standalone `tab_ci`/`tab_pct` + post-processing (not the aggregate-core / Jamovi, which hold the aggregate); `tot_n` is a stable cache quantity (changes only with the base), vs the reference base which is re-read on `ref` change.
4. **Row_var-axis globalised** (Phase 6, §5) — `OR/pct/color/comp/ci/chi2` and `ref2` are no longer vectorised over row_vars (mirror tables share them). Still per-row_var: `totaltab` and `ref` (named vector = one reference row per row_var; row%/means only, collapses under col% + message). col_var axis stays flexible (`pct/levels/digits` per col_var). Different tables → `list()` → export sequentially.
5. **Totals** (Phase 6, §6) — deprecate `totrow` (always a total row) and **soft-deprecate `totcol`** (Q1: default = exactly one total column, after factor / before numeric cols; old values `each`/`no`/names kept behind `deprecate_soft`, now purely cosmetic — never a calc base); `tab_plain()` = the no-total escape hatch; move/drop via dplyr. The total column shows each row's base as a **display-time `[min;max]` range** across col_vars (scalar when equal; no field overload — §10).
6. **col% + several row_vars** (Phase 7, §7) — manual invert (row_vars↔col_vars, row%) + **opt-in transpose at export** (`tab_kable`/`tab_md`/`tab_xl`); console never transposes; warn on `pct="col"` with several row_vars. `tab_transpose()` integrated/exported here.
7. **Exporters** (Phase 7, §8) — every exporter gets a base method (single tab) **and** a list method (several tabs rendered one-after-another, not merged), plus one shared prep helper preserving export parity. Phase 7 stays on **openxlsx v1**; the **openxlsx2** engine swap is isolated to **Phase 9** (decisions §8).
8. **Deprecations** (Phase 6) — soft-deprecate singular `row_var`/`col_var` (only `row_vars`/`col_vars` remain); drop the `tabxplor.compact` option.
9. **Class model** — keep the `tabxplor_tab`/`tabxplor_grouped_tab` split; `output_list = TRUE` container is a plain list for now. `/dplyr-method` if verbs change.

**Review session 2 (2026-07-07)** — four consistency decisions from the roadmap review (detail: `dev/tabxplor_1.4.0_decisions.md` §14-17):

10. **Weighted inference (Q5, §14)** — one rule for every CI/test: **weighted estimate + unweighted `n`** (for a 0/1 var, weighted-var + unweighted-n ≡ weighted-% + unweighted-n → proportions and means unified). Fixes the §12 self-contradiction. Caveat: anti-conservative under variable weights (`deff→1`); Kish `n_eff=(Σw)²/Σw²` a cheap opt-in (needs `Σw²`, G1). NOT full survey design.
11. **CI ⇄ stars duality (Q6, §15)** — the bracket and the stars must be duals. Significance stars are opt-in; **when on**, `pvalue` = two-proportion **score test** and the stored diff interval switches **AC→Newcombe** (its score dual); `ci="cell"` already Wilson, means Welch-t, OR log-Wald (all duals). AC stays the no-stars default (less golden churn).
12. **`tab_many()` return type (Q7, §13)** — **preserve the list-default** for the soft-deprecated `tab_many` alias; only the unified `tab()` merges by default. No silent return-type break.
13. **Test-result placement (Q8, §16)** — whole-**table** test → table attribute (generalise `chi2`→`test` to also hold ANOVA/F); whole-**column** test → rows of the same `test` tibble keyed by col_var (Q15, review 4 — was: column attribute); per-**cell** significance → the `pvalue` field. Display: a p-value *row* for now; a future `!`-per-cell "weak-test" warning documented.

**Review session 3 (2026-07-07)** — closures from the consistency review (detail: `dev/tabxplor_1.4.0_decisions.md` §15-18 + *Status*):

14. **Numeric diff-color scale (Q9, §18)** — `color="diff"` on numeric columns colors the **sd-standardized** difference (Glass's Δ = `diff/sd_ref`, derived at color time from `diff` + the reference `var` — no new field); default breaks `c(0.2, 0.5, 0.8, 1.2)` as new `mean_diff_breaks`. `$diff` stays raw; `ratio` mode keeps `mean_breaks`; `diff_ci`/`after_ci` unaffected (diff vs its own CI is already unit-free).
15. **Whole-table test slot (Q11, §16-17)** — **hard rename** of the `chi2` table attribute → `test` (constructor arg follows; one tibble holding chi2 + ANOVA/F with a discriminator column); `attr(x, "chi2")` → NULL is an accepted §17 break. Lands in Phase 3 with the chi2-leftovers cleanup.
16. **Stars vs explicit method (Q12, §15)** — the AC→Newcombe switch is **default-sensitive**: only when `method_diff` was left default; an explicit method is respected + one-time message that bracket ⇄ stars are no longer exact duals.
17. **G2 closed + serialization non-issue (§ *Status*, §17)** — vectorised chi2 must match `chisq.test()` defaults **exactly, incl. Yates on 2×2** (today's path calls it with defaults, `tab.R` ~L5290; golden locks it). Old serialized tabs are a non-issue (tabs are exported or re-created from code, never saved as `.rds`) — documented unsupported, no upgrade shim.

**Review session 4 (2026-07-07)** — inference pins + precision closures from the deep review (detail: `dev/tabxplor_1.4.0_decisions.md` §14-16, §19 + *Status*):

18. **Omnibus F weighting (Q13, §14)** — the mean-table Welch F follows the §14 rule (weighted means/variances + unweighted `n`), testing the numbers the table displays; **chi2 stays fully unweighted** (G2 parity) — a documented asymmetry on weighted tables.
19. **Mean CI quantile (Q14, §15)** — a second swap-under-stars pair: mean intervals keep today's `z` (`qnorm`, verified `tab.R` ~L5591) when stars are off, switch to **Welch-t** when stars are on — the dual of the Welch-t `pvalue`.
20. **Per-column tests (Q15, §16)** — per-col_var chi2/F results are **rows of the table-level `test` tibble** (today's chi2 mechanism), NOT a new fmt column attribute — the 8-attribute contract holds.
21. **Empirical-OR reference (Q16, §19)** — keep `ref2="first"` (the maintainer's data puts the positive level first); glm-convention alignment decided at tab_logit integration. Precision closures: the score test is **uncorrected** (Newcombe-10 dual — never `prop.test()`'s Yates default, §15); G1 drops the unweighted moment-sums; **D3** interim — Phase 2 flips numeric `diff` field+display but numeric *color* keeps reading `ratio` until Phase 5; the §10 `[min;max]` range is a **table-level display pre-pass** (`format()` is per-column; Excel may fall back to `min`); `totrow=FALSE` stays cosmetic during deprecation (§6).

#### Verification (every phase)

- **Byte-identity**: `devtools::test("d:/Statistiques/github/tabxplor")` after each phase; `test-golden.R` + `test-export-parity.R` + `test-fmt-contract.R` + `test-fuse-parity.R` stay green. Intentional output changes → rerun `dev/make_golden.R`, review the `_golden/`/`_snaps/` diff consciously, `testthat::snapshot_accept()`.
- **Performance**: run the harness (see *Reference > Benchmarks*) before/after Phases 2, 3, 6; save to `dev/benchmarks/results_1.4.0/`; confirm the Phase 3 CI/chi2 win. When past benchmarks on the former tabxplor version are missing, use installed **tabxplor 1.3.1** version.
- **From-the-middle**: feed the same data as microdata / long counts / wide / freq+N → identical fmt tables where `n` is real; CI/chi2 warn+skip on freq-only.
- **Release gate**: `devtools::check()` (~3 min, run manually) before CRAN.

### Phase 0 — Safety net (done — 2026-07-07)

Retro-compat tests + benchmarks BEFORE any refactor. Nothing below is safe without this. The net is GREEN on the current 15-field baseline; it deliberately locks *current* behavior so every 1.4.0 change is a conscious regeneration (never a silent drift). No safety-net assertion should fail on the current source — the "what must change later" is the tripwire ledger, not a red test.

#### Done

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

**OPEN — maintainer step**: `jmvtab.h.R` (generated from `.a.yaml`) could NOT be regenerated headlessly (`jmvtools::prepare()` → "jamovi could not be accessed"; needs the running jamovi app). Regenerate + review in the running jamovi with:
`options(jamovi_home='C:/Program Files/jamovi 2.6.44.0'); devtools::load_all(); jmvtools::install(); devtools::load_all()`.

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

**OPEN — maintainer step**: regenerate `jmvtab.h.R` from the updated `jmvtab.r.yaml` (adds the hidden `cache_state` Image) in the running jamovi app (`jmvtools::prepare()` can't run headlessly), then live-verify.


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

**OPEN — maintainer step**: regenerate `jmvtab.h.R` from `.a.yaml` in the running jamovi app (`jmvtools::install()` can't run headlessly), then live-verify the picker. The field-level instant re-ref (`jmv_tab3_reref`) is now **ON** (Phase 9b-7): a ref change re-refs the cached carrier instead of rebuilding (~3–4.5× faster, byte-identical).


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

Suite green (375 blocks, 0 fail), no golden regen (UI-only + behaviour-preserving digits cast). **OPEN — maintainer step**: regenerate `jmvtab.h.R` from the new `digits` List in the running jamovi app (`jmvtools::install()` can't run headless), then live-verify each greying rule + the digits dropdown + subtext width.


#### Phase 7i — test compatibility with Jamovi last solid version 2.7.37 (done)

Confirmation : jmvtab works well on jamovi 2.7.37


### Phase 8 – Parallelisation opt-in for the "many tables at once" survey workflow

Phase 6b — 2026-07-09 researched whether parallelising `tab()`/`jmvtab()` over `row_vars` is a real perf win. **Verdict: a substantial, reliable win for the PRIMARY workflow — worth a Suggests-only opt-in; NOT a forced default, NOT for big data / live jmvtab.** Grounded PoC (mirai / base `parallel` / future.apply, W∈{1,2,4,8,12}). Parallelising the row_var/pair axis is **byte-identical** (0/82 tables checked). The key result **inverts the naïve prior**: the *small/typical survey* df is the sweet spot, the 8M df the worst case. On **10k–60k-row surveys × many tables** (tabxplor's core "export dozens of colored tables" use case): **~2.5–3.3× at W=4** (commodity/university PC), **~4× at W=8**, ~1 s setup, ~0 memory, **wins even on a fresh call** — because per-table cost is N-independent O(cells) fmt/chi2 work (seq batch flat ~2.5 s from 10k→60k). On 8M it ≈break-even-to-loss (memory-bandwidth wall + 336 MB×W transfer); few tables always lose; future.apply unusable (per-call df resend); data.table's own threading barely helps (~1.2×). jmvtab *live* = no (cached aggregate → nothing O(N) to parallelise). Recommended opt-in: `options(tabxplor.parallel=)` gating an internal `tab_pmap()` at the `tab_build()` seam, persistent pool + `setDTthreads(1)` + df pre-loaded once + byte-identical fallback, skip below a table-count threshold, **after** Phase 2/7c (the batch-export path does NOT overlap the cache, so the gain persists). Full findings + tables: `dev/tabxplor_1.4.0_decisions.md` **§26**; scripts `dev/benchmarks/parallel_poc_{micro,tab,survey,mirai_dispatcher}.R`, results in `results_1.4.0/phase6b_*.txt`.
- We should first choose only one parallelisation engine / package : either `mirai` or `parallel`. What would be the best choice for both performance and future-proofing ? Anyway the package should be in Suggest.
- If workers setup step is needed, it should be done the first time parallelisation is used and reused afterwards.
- It should work on Windows / Linux / MAC, but for performance the main focus is Windows.

#### Done (2026-07-10)

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


#### Phase 9a — Internal clarify & simplification

##### Done (2026-07-11)

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

##### Phase 9b-1 — surgical `tab_compact` merge fix (done — 2026-07-11)

**9b-1 — surgical `tab_compact` merge fix (byte-identical).** The merge promoted totrow→refrow with `if_else(is_totrow & !any(is_refrow), as_refrow(.), .)` over each fmt column — a `vec_case_when` record round-trip (72 % of `tab_compact` per §29). Replaced by a direct `in_refrow` field write (new internal `promote_totrow_to_refrow()` in `R/tab_classes.R`, kept inside the per-sub-table `imap` so `any(in_refrow)` stays grouped per row_var). `as_refrow` only flips that field → byte-identical. **`tab_compact` 0.390→0.160 s (2.44×)** on the gss_cat 5×3 fixture; full merged call 1.78→1.55 s; `output_list` (no-merge) unchanged. Record: `dev/benchmarks/results_1.4.0/phase9b1_tab_compact.txt`.

##### Phase 9b-2 — measurement spike (done — 2026-07-11)

Harness `dev/benchmarks/phase9b2_fmt_cost_decomp.R` decomposed the per-table build across the 4 shapes. **Verdict: GO for 9b-3.** On the common factor path ~**30 %** (`vec_restore` reconstruction) to ~**48 %** (+`vec_case_when`) of the build is recoverable; the **materialize-once floor is ~0.5 %** (1.4 ms/21 cols) and pushing records through ops is **54.5× slower** than plain — so the fmt cost is almost entirely redundant reconstruction. Numeric-only tables gain ~nothing (cost = the data.table scan; `tab_num` already materializes once). **Fold the writers into 9b-3** — not a separate committable rung. Record: `dev/benchmarks/results_1.4.0/phase9b2_decomposition.txt`; full analysis `dev/tabxplor_phase9b_fmt_display_only.md` §5.

##### Phase 9b-3 — in-place fmt-reconstruction wins (DONE: passes 1-4)

The four **byte-identical, in-place** optimizations toward the "materialize `tabxplor_fmt` records ONCE at the very end" goal — each a golden-gated committable step, no carrier yet. Cumulative **~26% off the common merged call / ~34% off the per-table build**. The deferred-materialization **carrier core** that finishes the job followed in **Phases 9b-4 → 9b-6** below (9b-4 tests-boundary round-trip, 9b-5 ci/chi2 writes, **9b-6 the Boundary-B local unwrap of `tab_compact`/`tab_pvalue_lines`** — which subsumed 9b-7; another −28..−30% on the merged call).

**Done (2026-07-11): pass 1 — the single materialization seam.** `fmt_materialize_col()` (`R/tab.R`, the ONE `new_fmt()` call via `do.call`; `fmt_frame_fields`/`fmt_col_attrs` contract constants); both leaves route through it (byte-identical, perf-neutral, full suite green, no golden regen).

**Done (2026-07-11): pass 2 — the scan-primitive fold** (byte-identical, **~11-15% factor-path**). `is_totrow`/`is_tottab`/`is_refrow` `.data.frame` methods each built a full nrow×ncols logical tibble (`select(where(is_fmt)) |> map_df |> if_all/if_any`); replaced by a shared `fmt_row_flag()` (`R/fmt_class.R`) that reads the field per fmt column and `reduce()`s. `is_totrow.data.frame` **28× faster**; per-table build common −11% / ci −12% / contrib −15%. The dead `partial` warning branch is dropped. Full suite green, no golden regen. Doc §6.

**Done (2026-07-11): pass 3 — `tab_pvalue_lines` masked-fill** (byte-identical, **the big one: ~25-34%**). A post-pass-2 line-profile pinned `tab_pvalue_lines` at **~34% of the per-table build** (`chi2=TRUE` adds a p-value row): the block filled the new row's empty cells with an `if_else` over EVERY fmt cell (the `$.tabxplor_fmt` `vec_proxy` pull + `mutate.tabxplor_fmt` round-trip + per-column `vec_restore` — the source of `vec_case_when` 20% + `mutate.tabxplor_fmt` 7% + much of `vec_restore` 33%). Replaced by a masked assignment `col[is.na(get_display(col))] <- fmt0(...)` (`R/tab_classes.R`), a no-op on columns with no empty cell. **Cumulative baseline→pass3: common merged −26% / per-table −34%; ci −25%; contrib −26%.** Full suite green, no golden regen. Doc §6.

**Done (2026-07-11): pass 4 — `new_test_tibble` memoization** (byte-identical, modest ~3-6% common build). The empty-placeholder `test` tibble costs ~1.4 ms/call (`tibble()` validation), built several times per table; it's stateless → memoized (`R/tab_classes.R`, cached copy shared safely via R copy-on-modify). Full suite green, no golden regen. The remaining `tab_pvalue_lines` cost (`bind_rows`+`vec_restore` adding the p-value row) is the vctrs **record combine**, inherent to the fmt type — only the deferred-materialization carrier removes it (the carrier core, Phases 9b-4→9b-7). Doc §6. **Corrected cost model** (profiling, `dev/benchmarks/results_1.4.0/phase9b3_profile.txt` + doc §6): the col_var **join is cheap (0.9%) — NOT the target** (drop the L2 focus; keep the record `full_join`); the ~30% reconstruction is **pervasive `dplyr`-over-fmt**; the **#1 recoverable chunk is `tab_apply_tests`/`tab_chi2` at 20%** (repeated `is_totrow` scans + `dplyr`-over-fmt group-matching). **Revised staging** (doc §6, supersedes the join-first order): (1) `tab_chi2`/`tab_apply_tests` on plain fields with row/col masks computed once (the 20%, needs the carrier at the tests boundary); (2) defer the leaf materialization so the carrier reaches the tests; (3) `tab_assemble_tables`+`tab_add_n_pct` on the carrier, `fmt_wrap` at `tab_build_one` end. Landmines: L1 (types) + L5 (boundary) + L6 (ci/chi2) + L7 (add_n); **L2 dropped**, L3/L4 avoided. Full brief: `dev/tabxplor_phase9b_fmt_display_only.md` §6.

##### Phase 9b-4 — carrier to the tests boundary (DONE — 2026-07-11)

Implemented as the **lean post-join round-trip** (maintainer decision, not the design's leaf-emits-carrier): two internal helpers next to `fmt_materialize_col` (`R/tab.R`) — **`fmt_unwrap(tab)`** decomposes a built table to a carrier `list(is_fmt, factors, fmt = per-col list(frame = as.list(vec_data(col)), meta = the 9 attrs), attrs = attributes(tab))`; **`fmt_wrap(carrier)`** is its exact inverse (materialize each fmt col via `fmt_materialize_col`, pass factor cols through, restore `attrs` wholesale). A byte-identical **no-op** `fmt_wrap(fmt_unwrap(tabs_text))` is inserted in `tab_transform()` right before `tab_apply_tests()` — establishing the carrier at the tests seam; `tabs_num` untouched. New `test-carrier-parity.R` (15 tests) locks `identical()` across factor/numeric/mixed/weighted/col%/add_pct/ci + grouped + subtext/test attrs. **L1** held (fmt-contract `typeof` lock green: `new_fmt` does no cast, so `vec_data → new_fmt` preserves types). Full suite green (FAIL 0, PASS 1354), NO golden regen. Bench: no-op adds +0.08 s / +6.9% (gss_cat 5×3 merged) — a temporary second materialization of each row_var's factor table, recovered by 9b-5. **Step A dropped** (leaf emits carrier + tail port): under Q2 (keep the record `full_join`) the leaf materializes for the join anyway, so the leaf-tail port is never load-bearing under Boundary A. Detail: `dev/tabxplor_phase9b_fmt_display_only.md` §7.2.

##### Phase 9b-5 — DONE (2026-07-11): the tests-boundary WRITES on plain fields

Both increments landed byte-identical (full suite FAIL 0 | PASS 1354, NO golden regen; git-stash `identical()` A/B: 10 contrib + 21 ci shapes). All in `R/tab.R`. The reframing that governs it: the chi2 whole-table **TEST is NOT the cost** (a 40×15 A/B was 0.1000 == 0.1000 s; the §6 "20%" was the DEFAULT-`calc` contrib writes, not the pipeline `calc="p"` test) — the O(cells) fmt cost is the **WRITES**. Approach throughout = **precompute-then-single-write** (real setters over plain vectors, NOT a `fmt_unwrap`/`fmt_wrap` round-trip). Recurring landmines: writes are **per subtable / grouped** (old grouped mutates) → run ungrouped then restore grouping; and combining fmt via `dplyr::if_else` / a grouped-mutate **recombine** **materialises the `wn` field** (NA→n) → reproduced with `set_wn(get_wn())` for exactly the columns/paths where the old code did.

- **Increment 1 — chi2** (`chi2_compute_test()` read-only test marshalling — no win, clarity + no-op removal; `chi2_write_contrib()` — the per-cell `var`/`ctr` + `comp_all`/contrib-`color`): **contrib per-table −41 % (1.7×), −30 % memory** (`dev/benchmarks/results_1.4.0/phase9b5_chi2.txt`). Dead `variances_by_group`/`cells_by_group` dropped.
- **Increment 2 — `tab_ci`** (net −58 lines): (a) the reference-row selection + `x_n`/`ref`/`ref_var`/`ref_n` (the grouped `ref_rows`/`ref_to_na` + ungrouped transmutes) → a plain loop with `group_last_pos(mask)` (per-subtable last-reference-row index) feeding the `ci_*` engine; (b) the CI write + `comp_all` + `visible` display → ONE ungroup/mutate/regroup; `ci_type`/`color` stays the positional `map2_df` (byte-identical, sidesteps the L-IDX quirk). **ci per-table −20 % (1.25×)** (`phase9b5_ci.txt`). Dead `tot_rows` dropped.

Combined: the two WRITE-heavy paths (contrib −44 %, ci −20 % vs pre-9b-5) recovered; the READ paths (chi2 test, common `color="diff"`) flat.

##### Phase 9b-6 — Boundary B via local unwrap (DONE — 2026-07-11)

**Re-scoped (maintainer, this session) from "step D / Boundary A" → "Boundary B via local unwrap".** Grounded finding: 9b-6-as-designed (carrier through `tab_assemble_tables`, materialize at `tab_build_one` end) buys **~0 % on the common path** (after 9b-5 everything inside `tab_build_one` is cheap: leaves materialize once; `tab_apply_tests` no longer reconstructs; `tab_assemble_tables` ~2 %; add_n on `pct="row"` adds one col; the join is 0.9 %). The real ~15-25 % was **Boundary B** — `tab_compact`'s `vec_rbind` + `tab_pvalue_lines`' `bind_rows` in `tab_assemble_output`. Both were rewritten to row-bind on **plain field-frames via a LOCAL `fmt_unwrap`→wrap** (the 9b-5 pattern), so `tab_build_one` keeps returning **records** (no `test-parallel-parity` re-lock) and **9b-6+9b-7 collapse into this one deliverable** (Boundary A skipped). New primitive `fmt_stack_frames()` (`R/tab.R`). Increment 1 = `tab_compact` (`tab_stack_tables()`: `vec_ptype_common` reconcile = **L3**, promote_totrow folded onto the field frame = **L4**; ~neutral perf, byte-identical, scales with #row_vars). Increment 2 = `tab_pvalue_lines` (**the win**: fmt-free skeleton for row order + per-column field append, subsuming the pass-3 masked fill). Byte-identity key: the old `vec_cast` materialised `wn` (NA→n; `get_wn` is the only getter with a fallback) — reproduced via `fr$wn <- get_wn(col)`. **Bench (gss_cat 5×3): merge_s −28..−30 %, list_s −8..−14 %, mem 51→45 MB; numeric ~flat** (`dev/benchmarks/results_1.4.0/phase9b6_boundaryB.txt`). Full suite FAIL 0, NO golden regen; 12-shape git-stash `identical()` A/B green (incl. per-row_var-ref L3, tab_vars-grouped pvalue, numeric ANOVA, list path). `fmt_unwrap`/`fmt_wrap` now load-bearing.

##### Phase 9b-7 — jmvtab tier-3 carrier + instant reference re-ref (DONE — 2026-07-11)

Scoped up (maintainer) from the literal "carrier + re-paint" (which barely moves the render-bound live UI) to **carrier + the deferred instant reference re-ref** — "change the reference level live → recompute only diff/ratio/CI, no rebuild" (cache-design §4c). All in `R/jmvtab-cache.R`; the reference-picker UI already exists (7g-iii) → NO `.h.R` regen. Byte-identical, full suite green (1433/0), NO golden regen.

- **Increment 1 — tier-3 stores the CARRIER** (`list(carrier = jmv_carrier_unwrap(armed), tuple)` = plain field-frames via `fmt_unwrap`, not a live tab — aligns tier-3 with the tiers-1-2 discipline; schema 2→3). `jmv_reapply_digits` rewritten onto the carrier (drops the snapshot/restore trick; the single `fmt_wrap` absorbs its reconstruction). A/B caught L1: `set_digits` casts to integer but `new_fmt` does not → `vec_cast(new_d, integer())`.
- **Increment 2 — `jmv_tab3_reref()`**: reconstruct `tabs_pct`+context from the carrier's ref-independent fields (data rows only) → `tab_apply_reference()` for diff/ratio → re-run the diff CI via `tab_ci()` on the DATA ROWS (p-value lines removed first — they'd drop one row/subtable) → copy CI back; p-value rows + table attrs (`test`/`groups`) verbatim. Gated by `jmv_tab3_rerefable` (only ref/ref2 differ, diff-armed, no OR) + `jmv_reref_shape_ok` (pct="row", one factor row_var, `!has_num_col`, levels="all", `!add_pct`, **comp="tab"** — comp="all" has a ref-DEPENDENT shape —, not auto+ci=diff); else the (fast, cached) rebuild.
- **Result** (`dev/benchmarks/results_1.4.0/phase9b7_reref.txt`): a ref change is **~3–4.5× faster** (reref vs rebuild). Locked by `test-jmvtab-cache.R` (reref == rebuild across 12 shapes + tab() anchor + fallbacks + $state). Detail + landmines: `dev/tabxplor_phase9b_fmt_display_only.md` §8.


##### Phase 9c — further simplifications ? (DONE — 2026-07-11)

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

##### Phase 9d — leaf math on base-R / matrix (DONE — 2026-07-11)

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

##### Phase 9d — original plan (historical intent)

The §30 profile pins the single largest remaining chunk of `tab()` at **~30 %**: the fixed per-op
overhead of ~150 `[.data.table` calls across the 15 tiny leaf tables (dcast + pct/diff/total math), NOT
the O(N) scan (the build is N-independent) and NOT copying. The only lever big enough to move it: once
the counts are dcasted to a tiny wide table, do the pct/diff/total arithmetic with **base-R / matrix ops**
(`rowSums`/`sweep`/vectorised indexing) instead of chained `[.data.table` calls — eliminating most of the
~150 per-op invocations. This is a real leaf-math rewrite with **float-order / NA byte-identity risk**
(golden-locked), so it belongs in its own phase, not folded into 9c. Orthogonal to the carrier and to the
row-axis restructure. Weigh against Phase 10b (`format.tabxplor_fmt` `case_when` → base) — both are
O(cells) display/build levers, independent of each other.



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

**DECIDED: keep + optimize kableExtra first; a dependency-free home-built `<table>` renderer is Plan B.** Grounded (web + code): jamovi's results panel only honors inline CSS (CSS via `htmlDependencies` never applies) and won't reliably run htmlwidget JS, so interactive tables (reactable/DT) are out, `gt` is heavy (global rule avoids it), `tinytable`'s interactivity wouldn't fire live. The win comes from the shared prep (colours/refs derived ONCE), NA-hiding in prep, `tooltips=FALSE` (already Phase 7e), a "light" kableExtra path; the eventual home-built swap is isolated behind a `render_kable_html()` seam. The §23 profile's #1 lever (`fmt_color_selection`) is stale (deleted in Phase 5) → re-profile before ranking levers. Recorded in `dev/tabxplor_1.4.0_decisions.md` §33; full rationale in `dev/tabxplor_phase10_exporters.md` §10.

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

#### Phase 10f — tab_md()

`tab_md()` current version was made for a specific use case and never totally integrated into tabxplor : the aim is to fully integrate it.
- color helpers must be handled with very shorts pandoc bracketed spans, everything padded and align to preserve human readability assuming monospace font (even out of preview mode). Examples for diffs : `.+5`, `.+10`, `.+20`, `.+30`, `.-5`, `.-10`, `.-20`, `.-30` etc. ; examples for ratios : `.x1.2`, `.x1.5`, `.x2`, `.x4`, `./1.2`, `./1.5`, `./2`, `./4`, etc. : would these names be valid css classes / pandoc bracketed spans ?.
- Is there a possibility to make them these css classes work inside jamovi, for exemple in a html rectangle, with a light yet modern markdown preview working with tables (natively, or by adding html/js new dependencies ? ; load these possible dependencies when the tabxplor function and menu load ? What about the css styles, should we load them at tabxplor UI startup or at table creation ?) ?
- Even if they do not work on jamovi, I want them to work in Positron IDE Viewer, be it with pandoc bracketed spans (prefered solution if workable) or another way
- Even if pandock bracketed spans not working on tab_kable inside Viewer, I still want an option to export as very simple markdown with pandoc bracketed spans, to use in markdown editors with customisable css working with bracketed spans (just for information, I have Positron IDE custom syntax highlighting in normal editor with a personal VS code extension). It shall really remain simple human readable padded/aligned markdown.

#### Phase 10g – rework tab_xl() (still openxlsx v1; engine swap is Phase 9)

- Make it work with every data.frame, even not made with tabxplor, with default settings (event without factors, etc.). Implement small fixture tests.
- To make it work with a "common preparation function" that would be the same than tab_kable() etc., Make the function for a single tab (sometime big with `compact=TRUE` ? ), then parallelize for list of tab ?
- Integrate numfmt() in format(type = "xl")
- avec tab_logit (references), on perd les bordures des groupes aussi ? Vérifier.
- Add the end it must work with tab_logit() and *** : significance stars used as formatting.
- Prep-helpers `numfmt`→`format(syntax="excel")`

**Stays on openxlsx v1** — the openxlsx2 engine swap is Phase 9.


#### Phase 10h – additional columns and pvalue lines simplification (optional)

`add_n`, `add_pct`, pvalue_lines : only add these additional rows or columns at display ?
- Distingish between display modes that can use `display_spec` to print several informations in the same cell (console, kable, md ; for example print `add_n` as : `"100% (n= 114)"`), and display modes that needs to create new columns/rows (Excel ; for example print `add_n` by adding an ).
  + The main caveat, if I understand it well, is that `display_spec` is a column attribute ? Would there be a reliable way to use the already existing display vctrs field at it’s place (removing `display_spec` as a column attribute), ensuring simple displays like `pct` or `diff` stay on a fast track for maximum performance, compared to more complex display like `pct (n)` (that of course themselves need to be the fastest possible).
- Since `add_n`, `add_pct` and `pvalue_lines` as actual rows and columns in the data were exceptions that added complexity to the code, their removal calls for a huge code simplication.



### Phase 11 — Excel engine migration (openxlsx → openxlsx2)

Isolated on purpose: a full dependency swap should not be entangled with the Phase 10 exporter-prep unification and its export-parity churn. Runs **after** Phase 10 (needs the unified single-tab + list `tab_xl` methods in place).
- Precondition: `test-export-parity.R` green on openxlsx v1 first, so the swap is verified byte-for-byte against a known-good baseline.
- Swap `tab_xl()` from `openxlsx` to `openxlsx2` (Suggests). Rule number 1: read openxlsx2 documentation thoroughly, then build a small set of **shared/common styles** created once and reused (the main openxlsx2 speed lever).
- Use Phase 10 shared exporter-prep helper : if it needs further modifications, we shall implement them here.
- Re-verify export parity (`format.tabxplor_fmt` vs the `tab_xl` numeric bypass).

Add an option to use **conditional formatting** instead of hard text colors. This was awful and very slow with openxlsx v1 — check whether openxlsx2 makes it less horrible / faster.



### Phase 12 – Manual reviews

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



### Phase 13 – Finalise color UI, redesign color palettes with manual fine-tuning for clarity

Color UI finalisation
- Would it be possible / consistent to add this possibility:
`color = c(pct="diff", mean="ratio")`, to have diff for factors and ratio for means, passing internally the c("diff", "ratio") color while passing empty breaks for the not wanted ones ?
- How are breaks passed in tab()/tab_many() handled, are they written as a per-column attribute, or was there another solution ? I can feel this part of the design was a bit shaky.
- Redesign color legends for simplicity : they should be understandable by non-experts, while at the same time having just the enough technical terms for the experts to know exactly what happens technically here.

Redesign color palettes with manual fine-tuning. Cf. other `dev/` documentation.

Native dark mode/light mode management for exported tables, specially html tables
- With kable or another html tables solution, use css exported and applied with the table ?
- Wire this css on standard html dark mode toggle, with a global option in R to use Dark mode in viewer. As a result, the table should autochange it’s formatted we the user change to dark light mode on whichever html page the table is embedded with. Do web searches to find current good practices about this.



### Phase 14 — tab_logit integration and full redesign

Integration of `tab_logit.R` (currently commented out) into the package, then redesign and rewrite of `tab_logit` and `multi_logit`, and maybe extension to all `lm` + `glm` regression models inside the same unified framework.
- logit and regression models functions will be introduced in tabxplor 1.4.0 : **no backward-compatibility needed**, but the public API and internal workflows both need to be carefully redesigned for user-friendliness, consistency, performance and future-proofing.

#### Phase 14a – integrate current version in tabxplor framework cleanly

An important design question should be answered first : should I keep the content of `tab_logit.R` inside tabxplor package, even if it makes the count of tabxplor dependencies very high (CRAN current policies on that matter ?) ? Or should I create a `regxplor` subpackage (name is `available::available()`) relying on tabxplor (with more frictions during dev, both human dev and Claude Code assisted dev, or not necessarily and there are reliable way to avoid them ?), and in this case, as a package always loading with tabxplor, or as a package just importing tabxplor ? Make detailed web searches about modern good practices and tidyverse good practices, then write your analysis in `dev\tabxplor_1.4.0_decisions.md` (respecting it’s internal style and logic).

The current `tab_logit.R` code, made outside of the package, was a way to use tabxplor vctrs fields former implementation to store the logit data, but the way to do it may have been pragmatic/messy/ad hoc : first, before modifying tab_logit() behaviour, I want to integrate it with the rest of the package.
- Do not hesitate to redesign it thoroughly for consistency with tabxplor package architecture. Fix ad hoc stuffs to make it fits perfectly inside tabxplor framework.
- Do not hesitate to break it into subfunctions when needed, convenient or future-proofing.
- Do ne hesitate to rethink the articulation between `tab_logit` and `multi_logit`, and the internal workflows in general.
- Integrate confidence intervals with the new `ci_inf` / `ci_sup` vctrs fields (check its in fact `exp()` bounds), and also with the new `color_signif` framework (with logistic regression, sensible default may be "grey_non_signif").
- All exports (kable, md, Excel) should work natively with the resulting tabxplor_tab (or grouped one, etc.).


#### Phase 14b – design choices and statistical framework

Statistical sanity 1 : how to handle dependent var factors with 3+ levels ?
- The function currently binarise all levels against the reference level chosen, and gives one column per non-reference level, instead of using multinomial logistic regression : I known it’s expert way it’s done, but at the same time I find multinomial logistic regressions very difficult to read, since relative risk ratios with their double reference are farther from experience and intuition and car be thoroughly misinterpreted with not enough knowledge of reference rows and cols (it’s also very difficult to teach to sociology students, and to put in a meaningful sentence it a scientific papers : contrary to odds ratio, that can be put in sentence quite cleanly still understanding what you compare with what). Please, make detailed web searches, and tell me what the statistical consensus is about that, what are the different possibilities and rationales make by and social scientists both (particularly in quantitative sociology).

Statistical sanity 2 : how to handle survey weights ?
- Most of my real world data are French national surveys, which always come with weights. The first version of `tab_logit` was made to handle this : on one hand, no weights is common practice, but if the percentages used by logistic regression do not match the percentages the user really have in it’s base empirical crosstables, there’s a bizarre discrepency ; 2. since weights are most of the time not normalised, using weighted counts that are many times higher than the real number of people in the sample destroys significativity tests and confidence intervals since they always pass (total weighted n gives the overall population measured in the national census).

What extension ? Full `lm`/`glm` set + keep current multinomial logistic reg ?
- Extend the function to numeric variables as predictors ? Extend it to ensure it also works for integer dependent variables as poisson regression ? Generalise the framework most common `lm` + `glm` regressions models, like multiple linear regressions for doubles (with the possibility to chose the type of regression per dependent var, since the R class of the dependent var, double or integer, do not clearly states if the underlying distribution is gaussian or poisson if its counts or binomial if it’s percentages, etc.) ? What would be caveats ? In this case, function should be renamed `tab_reg`. Many things should be reworked to keep both logistic models specificities, plus be closer to standard practices in term of `lm` + `glm` display.

Summary statistics ?
- What whole analysis/model level test and pvalue should be added, for example on a pvalue_line like chi2 test for crosstables and ANOVA for factor x numeric ? What other model level summary statistics should absolutely be added to keep with standard practices with `lm`/`glm` models ?

#### Phase 14c – implement testthat tests

Implement testthat tests
- They should include tests of statistical soundness, attesting the results matches base `glm` etc. results, unweighted +  survey weights design.

#### Phase 14d – tab_logit rewrite

Implement the design make in the former phase.
- Chose reference for each var with a vector (possibly named for simplicity) ? (permit to take ref in the middle while keeping order of ordinal vars, or useless white elephant ?) ?
- Implement things with contrasts ?

#### Phase 14e – tab_logit jamovi UI

Add a full tab_logit analysis in Jamovi to give it a user-friendly UI : name it `jmvtab_logit`
Are there some user-friendly pieces that we could reuse from other well known models/regressions Jamovi modules ?

What about `multi_logit`, who handles passing of multiple models (like different subsets of the same variables) for comparison ?
- Would it be possible to add it’s own jamovi analysis and UI, or would it be too complicated / useless ? Just one jamovi UI for logits, with possibility to choose predictors variables, then click on "+" button to add a subset of them, with the possibility to add any new set with "+" ?





### Last Phase — verif and package user-friendly documentation

#### Last Phase a – Bug corrections


#### Last Phase b – Create several vignettes

The current vignette should be the basis for non-expert users, while also permitting expert users to understand what this package is really interesting for.

All the part about "programming with tabxplor" and its vctrs fields should come in their own vignette, and it must be uptaded and extended.

If tab_logit() is implemented it should come with it’s own vignette


#### Last Phase c – full `pkgdown` documentation

Implement a full pkgdown documentation.
- Where ? On github pages ? Elsewhere with tidyverse ecosystem provided servers ?




### Reference — bugs, benchmarks, perf

#### Discovered bugs

In-code these are tagged for grep: `# KNOWN-BUG:` (bugs below), `# FIXME:` / `# FIXME(clarify):` / `# FIXME(future):` (suspect logic or future work, several tied to the Phase 5 color work), `# OBSOLETE:` (dead-code banners, e.g. the stale `tab_xl` duplicate). Fix each bug inside the phase that rewrites the relevant code, not as a separate pass.

- FIXED (Phase 1a): `fmt()` public constructor cast `totcol` into `refcol` (the `refcol` argument was silently ignored). Now casts `refcol`. Low impact (refcol is normally set internally).
- FIXED (Phase 7g-iii, golden-locked): two latent `ref` bugs surfaced by the reference picker. (1) `diff_index()` matched a level label as a REGEX, so a metacharacter label (e.g. `"$25000 or more"`) silently mismatched (the reported "picking the 2nd row_var does nothing" — `rincome` has `$` levels) and a substring label multi-matched — now EXACT-match-first, then regex. (2) `resolve_ref_vector()`'s `length(ref)==1` early return recycled even a NAMED length-1 ref, so `c(race = "Black")` leaked to every col_var — now only an UNNAMED length-1 recycles; a named one is name-matched. Both byte-identical on existing goldens (the goldens' refs are `first`/`tot`/non-substring labels).
- FIXED (Phase 6e, golden-locked; hardened Phase 7d-i): `tab_num(..., <tab_vars>, ci="cell")` used to error ("some columns don't belong to the data.table: [tab_var]") in the `tot="no"` grand-total-only grouping-set / `na="keep"` reorder path. 6e made the grand total a length-1 list so `num_rollup()` keeps every tab_var present; 7d-i added a defensive `intersect(tab_vars, names(tabs_tot))` guard at the reorder + an `expect_no_error` regression in `test-num-fuse-parity.R`. Locked by golden `n_ci_tabvars` / `n_ci_tabvars_all`, both `comp` modes.
- `set_color_style(custom_palette=)` (`tab_classes.R` ~L3120): length check requires 10 but the message says 11 and 11 names (`pos1..neg5, ratio`) are applied — the `ratio` slot ends up valueless, so custom palettes are broken for the ratio color. Fix by accepting length 11.
- **FIXED (Phase 7e)**: `tab(data, >=2 row_vars, >=2 col_vars)` used to error "pct can't be recycled" for ANY `pct` (the multi×multi tables jmvtab drives). `tab()` recycles `pct` to a per-col_var vector (`pct = c(rep(pct, length(col_var)), ...)`), but `pct_vect` only broadcasts a per-col_var vector when there is exactly ONE row_var (branch B); with ≥2 row_vars it falls to the `else` stop. Fix: add a branch `is.character(pct) & length(pct) == length(col_vars)` → `rep(list(pct), length(row_vars))`. Pre-existing (reproduces pre-7d-ii on `git stash`); low impact (multi×multi + output_list); fix with the recycling code.
- `tab()` errors on a `data.table` **input** (works on tibble/data.frame). `tab(as.data.table(gss), marital, race)` → `tab_num()` "Selections can't have missing values" from `tidyselect::eval_select(col_vars, data)` (`tab.R` ~L3203) — under a data.table input the numeric-col_var index path (`as.character(col_vars)[col_vars_num]`, `tab.R` ~L1304) yields an NA selection. Low impact (users pass tibbles/data.frames; `tab()` does its own `setDT` on a narrowed copy internally). Discovered in the Phase 6b PoC (§26). Fix belongs with the Phase 2/6 aggregate-core / col_var-classification code, not a separate pass.
- FIXED (this session): `set_num()` wrote `display=="diff"` via `set_pct()` (should be `set_diff()`), so setting the displayed value of a diff cell went to the wrong field. Now uses `set_diff()`.
- FIXED (workstream 5): `relabel_levels_in_varnames()` (`tab.R` ~L5592) made big weighted tables ~60× slower. Its `across(where(...))` predicate ran on **every** column with vectorised `&`/`|`, so the character branch `any(. %in% names(data))` coerced whole 8M-row numeric/factor columns to strings (~15s × 2 calls). Rewrote it to examine **only the `col_vars` targets** with short-circuit `&&`/`||` (numeric targets cost ~0); output byte-identical. 8M `tab(wt=)`: ~30s → ~0.2s; unweighted tables also faster + ~90% less memory.

#### Benchmarks (`dev/benchmarks/`)

The performance harness lives in `dev/benchmarks/` (`.Rbuildignore`'d). Per the scope decision, save every phase's before/after runs under `dev/benchmarks/results_1.4.0/`.

- `run_bench.R` — heavy 8M-row `tab()` harness: `source("dev/benchmarks/run_bench.R")`. Compares to `dev/benchmarks/baseline.csv`; writes `results_<stamp>.csv` (git-ignored).
- `run_fused_vs_bytable.R` — fused vs table-by-table arbiter on a 15M fixture (the `.by_table` flag). *(OBSOLETE since Phase 9c removed the tab()-level factor fusion — `.fine` now only reaches `tab_plain` via jmvtab / `tab_counts()`.)*
- `gen_big_df.R` — deterministic 8M fixture builder (cached to `big_df.rds`, git-ignored).
- `baseline.csv` — committed 8M baseline; reset consciously after a deliberate perf change.
- `tab_many_performance_profile.md` — the full 2026-07 profile (read before optimizing).
- In-suite counterpart: `tests/testthat/test-benchmark.R` (small `gss_cat`, informational, never fails, vs committed `tests/testthat/benchmark_baseline.csv`; regen via `dev/make_benchmark_baseline.R`). `bench` is Suggests-only (falls back to `system.time`).

#### Perf findings (condensed — full profile in `dev/benchmarks/tab_many_performance_profile.md`)

- **`tab_chi2` is the #1 cost** (84% of a small 9-tab call; N-independent, scales with *cells*) → the reason CI/chi2 move onto the aggregate in Phase 3.
- Per-table fixed fmt/vctrs overhead (~0.19 s/table) dominates over the scan; `tab_num` double-scans N and weighted `tab_num` allocates ~7.8 GB (`weighted.var` recomputes the mean) → Phases 1-3.
- Scan-fusion — the tab()-level opt-in (`options(tabxplor.fuse_min_rows=)` + the fused block in `tab_aggregate`) was **removed in Phase 9c** (§30): a NET NEGATIVE (+1–7 %) once the build is O(cells) / N-independent, so fusing the O(N) scan buys nothing at survey scale. The `.fine`/`.by_table`/`fine_for_pair()`/`use_raw` seam **remains** as the jmvtab-cache aggregate-injection seam (+ `tab_counts()` + numeric `fine_num`); `test-fuse-parity.R` now drives `tab_plain(.fine=)` directly.

---

## The last step of every implementation : Update instructions and relevant development files

After verification passes, always :

1. Ensure the file-header docstring/comment of any modified module is still accurate. Update or add `# DESIGN:` / `# WARNING:` tags next to changed logic.
2. Keep the tabxplor version 1.4.0 roadmap in CLAUDE.md and `dev/tabxplor_1.4.0_decisions.md` up-to-date as you build it or implement it.
3. Update `dev/tabxplor_architecture.md` whenever you modify the package structure for real (add modules, rename functions, change config fields). Do not add clutter and useless details. When there is nothing to change, skip it. Update other `dev/*md` file when relevant.
4. For package structure and architecture, also add the relevant CLAUDE.md update lines in your response : it should be minimalistic, concice, no bullshit, with nothing useless that would clutter the prompt, since the details are already in `dev/tabxplor_architecture.md`. When there is nothing to change, skip it.
5. `NEWS.md`: user-facing and CRAN-facing, tracking new functions, new arguments and arguments changes, deprecations, and important bugs fixes. Keep it minimalistic and no bullshit. Do not edit it when it’s not necessary.
6. (`README.Rmd` : user manual. Only update before release of new version to CRAN, never before.)


