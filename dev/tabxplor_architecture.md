
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
| `ratio` | double | Ratio to reference. **Written for numeric (mean) columns since Phase 2** (`cell_mean / ref_mean`); the pct-column ratio (the "×2 rule") still rides the `mean` overload until Phase 5. Renamed from `rr` (Phase 1a) |
| `ctr` | double | Contribution to chi-squared variance |
| `var` | double | Variance (used for CI calculation) |
| `ci_inf` | double | Lower confidence-interval bound (Phase 1a; real asymmetric bounds written in Phase 3) |
| `ci_sup` | double | Upper (absolute) confidence-interval bound. `get_ci()` = `ci_sup − ci_center` (upper arm) |
| `pvalue` | double | Per-cell significance p-value (Phase 3a: CI-inversion p; drives `get_stars()`) |
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
| `ci_type` | character | CI type: "", "no", "cell", "diff", "auto" |
| `col_var` | character | Name of the column variable this belongs to |
| `totcol` | logical | This column is a total column |
| `refcol` | logical | This column is a reference column |
| `color` | character | Color scheme: "no", "diff", "diff_ci", "after_ci", "contrib", "or", "OR" |

**Critical distinction:** Fields are per-cell vectors (every cell can have a different `n`, `pct`, etc.). Attributes are scalar values describing the entire column (all cells in the column share the same `type`, `color`, etc.). Do not confuse the two when modifying the class.

**Constructor chain:** `fmt()` (public, validates and coerces arguments) → `new_fmt()` (internal, calls `vctrs::new_rcrd()`).

**Adding a new field** requires updating: `new_fmt()`, `fmt()`, `format.tabxplor_fmt()`, `pillar_shaft.tabxplor_fmt()`, the relevant `vec_arith` methods, and possibly `tab_pct()`/`tab_ci()`/`tab_chi2()`. Expect ~8 functions across 3 files.

### tabxplor_tab — The Table Tibble

`tabxplor_tab` is a tibble subclass created via `tibble::new_tibble()` in `R/tab_classes.R` : it’s strenght is to work with normal `dplyr` workflows. It adds two attributes beyond what a regular tibble carries:

- `subtext` (character vector): Legend lines printed below the table.
- `test` (tidy tibble, 1.4.0 — renamed from `chi2`): whole-table test results, one row per
  (sub-table × col_var × test-type). Columns: `[tab_vars…]`, `row_var`, `col_var`, `test`
  (`"chi2"` / `"F_welch"` / `"F_classic"`), `statistic`, `df1`, `df2`, `pvalue`, `n`, `variance`,
  `min_e`. Chi-squared is filled for factor columns, ANOVA F for mean columns (both computed by the
  vectorised engine in `R/tab-agg.R` — `agg_chi2()` / `agg_anova()` — via `tab_chi2()`). Read it with
  `get_test()` (which also falls back to the old `chi2` attribute); `get_chi2()` is a kept alias.

Constructor: `new_tab(tabs, subtext, test)` (the old `chi2 =` argument still works, mapped to `test`).

### tabxplor_grouped_tab — Subtabled Results

When `tab_vars` are provided, the result is a `tabxplor_grouped_tab` — a `grouped_df` subclass. It carries the same `subtext` and `test` attributes, plus `groups` data from dplyr.

Constructor: `new_grouped_tab(tabs, groups, subtext, test)`.

This class requires a separate S3 method for **every dplyr verb** to preserve class and attributes through operations. See the dplyr Integration section below.

## Calculation Pipeline

Since Phase 6 (1.4.0) both public entry points are thin wrappers over the internal engine
`tab_build()` (`R/tab.R`). `tab()` and `tab_many()` differ only in the default output shape they pass
(`tab()` merges >=2 row_vars; `tab_many()` keeps a list). `tab_build()` runs the shared prep once on
the whole data, then the per-row_var pipeline:

```
tab(data, row_vars, col_vars, ..., output_list=FALSE)   tab_many(..., compact=)  [soft-deprecated]
  └────────────────────────┬───────────────────────────────────────┘
                           ▼
              tab_build(data, ..., output)
                ├─ PREP-ONCE (whole DB):   tab_prepare()  — select, filter, cleannames, lump rare
                │                            levels, zero/NA-weight removal, na population fix
                ├─ per row_var:
                │    tab_plain()  ──►  data.table aggregation (dcast), wraps in fmt, adds totals,
                │       or tab_num()   writes tot_n     (numeric: moment-sum aggregate → means/var)
                │    tab_apply_tests() ─►  tab_chi2() (chi2 / ANOVA F, +ctr) → capture `test` →
                │                          tab_ci() (Wilson / Newcombe / Welch-t; base from tot_n)
                │    level-drop, tab_add_n_pct()
                └─ ASSEMBLE:  merge num+factor, totrow/totcol cosmetic filter, rewrap (new_tab /
                     new_grouped_tab), output shape (§13), tab_pvalue_lines(), tab_spread()
```

`tab_apply_tests()` is the ONE place both `tab_build()` and `tab_counts()` build the chi2/ci calls
(Phase 6a). `tab_compact()` (`R/tab_classes.R`) is the internal merge invoked when `output = "single"`.

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

### tab_plain() — The Aggregation Core

`tab_plain()` is where raw cross-tabulation happens:

1. **data.table dcast**: `data.table::dcast(DT, row_var ~ col_var, fun.aggregate = sum)` for weighted counts. Column names are temporarily prefixed to avoid data.table reserved name conflicts.
2. **Wrap in fmt**: Raw counts are wrapped into `fmt` vectors via `new_fmt()`.
3. **Add totals**: Total rows and/or columns are added based on `tot` argument.
4. **Pipeline**: Chains to `tab_pct()`, `tab_ci()`, `tab_chi2()` if requested.
5. **Restore names**: Internal prefixes are removed; original column names restored.

### tab_num() — Numeric Column Variables

When a `col_var` is numeric (not a factor), `tab_num()` is used instead of `tab_plain()`. Since Phase 2 (1.4.0) each grouped `data.table` scan computes **sufficient moment sums** (`n`, weighted `n`, `Σ[w]x`, `Σ[w]x²`) in one pass; `num_derive_stats()` (`R/tab-agg.R`) then derives the mean and variance, reproducing the unweighted sample (n-1) vs weighted ML (÷Σw) definitions exactly. This removed the old `weighted.var()` double scan, and the total rows / total table are built as **roll-ups** of the additive moment-sum aggregate (`num_rollup()`) instead of two extra full-data scans. Net on 8M rows: `tab_num` ~5.6×/8.3× faster and ~6×/11× less memory (unweighted/weighted). The resulting `fmt` vectors have `type = "mean"` and `display = "mean"`.

### The Reference System

The `ref` argument controls which row serves as the comparison baseline for differences and colors:

- `"auto"`: defaults to `"first"` when OR requested, `"tot"` otherwise
- `"tot"`: the total row is the reference (differences = cell - total)
- `"first"`: the first non-total row is the reference
- integer: specific row index
- regex string: matched against row labels (must match exactly one row)
- `"no"`: skip difference calculation entirely

The `comp` argument adds another dimension:

- `comp = "tab"` (default): compare within each subtable's own total
- `comp = "all"`: compare against the total table's total (across all subtables)

### Mean diff vs ratio (Phase 2 flip)

For `type = "mean"` columns the `diff` field is a real **difference** (`cell_mean − ref_mean`), like pct columns; the mean/reference **ratio** lives in the `ratio` field (`cell_mean / ref_mean`). Since Phase 5, `color = "diff"` on numeric means colors the **sd-standardized** difference (Glass's Δ = `diff / sqrt(ref var)`) against the `mean_diff` scale (`c(0.2, 0.5, 0.8)`), while `color = "ratio"` colors the `ratio` field against `mean_ratio` (`c(1.15, 1.5, 2, 4)`). For **pct** columns the `mean` field is now `NA` (the old mean/×2 overload is gone; the `ratio` field carries the relative risk that drives the ×2 rule).

Note the remaining pct-column overload: for percentage columns the "×2 rule" ratio still rides the `mean` field (removed in Phase 5). So `mean` currently means an actual mean for `type = "mean"` and a pct ratio otherwise.

### Confidence Intervals & significance stars (Phase 3a)

Since Phase 3a the CI is **real asymmetric bounds** in `ci_inf`/`ci_sup`, plus a per-cell
significance `pvalue`, all computed by the **closed-form vectorised engine in `R/tab-agg.R`** (no
`DescTools` at runtime). Two interval *shapes*:

- **pivot** — `estimate ± q·se` with a continuous inversion p (`ci_pivot()`): serves Agresti-Caffo
  & Wald (proportion diff) and z / Welch-t (means).
- **score** — asymmetric Wilson (`ci_wilson()`, cell proportion) and its hybrid Newcombe-10
  (`ci_newcombe()`, proportion diff), the latter's inversion p found by a vectorised bisection.

Defaults: **Wilson** (`ci="cell"`), **Newcombe** (`ci="diff"`, `method_diff="newcombe"`; also `"ac"`,
`"wald"`), Welch-t (means). `tab_ci()` (proportions) and `tab_num()` (means) both route through the
engine; the reference (`x_n`, `ref`, `ref_n`) is the **weighted estimate + unweighted n** of §14.

**Significance = universal CI-inclusion**: the stored `pvalue` is the inversion p of the *same*
interval that draws the bracket, so `get_stars()` (`*`/`**`/`***` from `options("tabxplor.signif_levels")`)
can never disagree with the bracket. `stars` arg (default `TRUE`, `ci="diff"` only; `ci="cell"` →
`pvalue = NA`). Kish `n_eff` opt-in for numeric CIs via `options("tabxplor.kish_neff")` (needs the
`Σw²` accumulator, added to the numeric scan only when opted in).

Accessors: `get_ci()` = upper arm (`ci_sup − ci_center`, retro-compatible with the `$ci` field extraction);
`get_ci_moe()` = larger arm for the `± moe` display; `fmt(ci=)` stores absolute symmetric bounds
around the estimate. `format()` reads `ci_inf`/`ci_sup` directly (× 100 for proportions, clamped to
`[0,100]`), then appends `get_stars()`. Two display modes via `options("tabxplor.ci_print")`:

- `"moe"`: `value ± margin` (e.g., `45% ±3`, the conservative larger arm)
- `"ci"`: `[lower; upper]` (e.g., `[42%; 48%]`) — the default

## Color System

The color system has three layers, all working together to determine which cells get which colors at which intensity.

### Layer 1 — Palettes

Six predefined color palettes are defined as named character vectors in `R/tab_classes.R` (around line 2892). Each palette has 11 hex color codes:

- `pos1` through `pos5`: Increasing intensity for over-represented values (green/blue spectrum)
- `neg1` through `neg5`: Increasing intensity for under-represented values (yellow/orange/red spectrum)
- `ratio`: Special color for the "*2 rule" ratio comparison (purple/blue)

The palettes are:

| Palette | Use case |
| ------- | -------- |
| `color_style_text_dark` | Console text on dark background |
| `color_style_text_light` | Console text on light background |
| `color_style_text_light_24_blue_red` | HTML 24-bit (green→blue→red) |
| `color_style_text_light_24_green_red` | HTML 24-bit (green→red, traditional) |
| `color_style_bg_light` | Cell background on light theme |
| `color_style_bg_dark` | Cell background on dark theme |

Selection is done by `set_color_style(type, theme, html_24_bit)`, which sets `options("tabxplor.color_style")`. `get_color_style()` returns either crayon functions (for console) or hex codes (for HTML/Excel), depending on the `mode` parameter.

### Layer 2 — Breaks (Phase 5 canonical scales)

Breaks live in `options("tabxplor.color_breaks")` as a **named list of five positive-only scales**, set by `set_color_breaks(list(...))`. Each scale is `list(pos, center, strict, std)`:

| Scale | Applies to | Default `pos` | `center` | notes |
| ----- | ---------- | ------------- | -------- | ----- |
| `pct_diff` | factor difference (pp) | `c(0.05, 0.1, 0.2, 0.3)` | 0 | additive, mirror `c(x, -x)` |
| `pct_ratio` | factor relative risk | `c(2)` | 1 | multiplicative, mirror `c(x, 1/x)` — the "×2 rule" |
| `mean_diff` | numeric difference | `NULL` → `c(0.2, 0.5, 0.8)`, `std = TRUE` | 0 | sd-standardized (Glass's Δ) by default; data-unit values → absolute |
| `mean_ratio` | numeric ratio / OR | `c(1.15, 1.5, 2, 4)` | 1 | multiplicative |
| `contrib` | χ² contribution | `c(1, 2, 5, 10)` | 0 | inclusive (`strict = FALSE`) |

`center` is the neutral value each break is measured from; `strict` picks `>`/`<` vs `>=`/`<=`; `std` (mean_diff only) toggles standardized vs raw. Mirroring is applied by the engine. The old flat args `pct_breaks`/`mean_breaks`/`contrib_breaks` are soft-deprecated and mapped onto these. `get_color_breaks()` returns the positive scales (round-trips with `set_color_breaks()`); `type = "all"` mirrors.

### Layer 3 — The vectorised `findInterval` engine (three axes)

Coloring is decomposed into three orthogonal per-column choices: **measure** (`diff`/`ratio`/`contrib`/`or`, in the `color` attribute `[1]` = text, `[2]` = background), **channel** (text vs background), and **significance policy** (the `color_signif` attribute: `ignore`/`grey_non_signif`/`color_all_signif`). All feed one engine in `R/fmt_class.R`:

1. `fmt_color_plan(x, channel, color, signif)` builds a plan: it decodes the measure+policy (`color_measure_policy()`), picks the scale for the measure×column-type, computes the per-cell `score` (e.g. `get_diff`; standardized `get_diff / sqrt(get_ref_var)` for numeric diff; `get_ratio`; `get_ctr / get_mean_contrib`), the significance `gate` (from the stored `get_ci_inf`/`get_ci_sup` bounds), and the `pos_slots`/`neg_slots` maps (`build_slots()`/`color_slot_table()`).
2. `fmt_color_slots(x, plan)` folds `score` to a magnitude around `center`, then `findInterval(mag, pos_breaks)` → level → palette slot (0 = uncolored, 1..10 = grid, 11 = the legacy ×2 override), zeroing ungated cells. This C-level path replaced the old per-cell `keep_last_break` reduce (48–1290× faster).
3. `fmt_color_channels(x)` → `list(text_slot, bg_slot)`.

Every consumer maps `(text_slot, bg_slot)` to colour the same way: `pillar_shaft.tabxplor_fmt()` (console, the reference two-channel consumer), `fmt_get_color_code()` (single-channel, the golden), the shared `fmt_channel_codes()` helper (text + bg hex, used by `tab_kable`/`tab_plot`/`tab_xl`), and `tab_color_legend()` (which reads the same scales, so legend and cells never disagree). The old combined strings (`"diff_ci"`/`"after_ci"`/`"ci"`) are decoded to (measure, policy) by `color_measure_policy()`.

The `color`/`color_signif` **arguments** are parsed by `normalize_color_spec()` + `finalize_color_spec()` (`R/tab.R`), called by `tab()` and `tab_num()` (not yet `tab_many()` — Phase 6).

## Export System

Four export formats, all in separate files:

### tab_xl() — Excel Export (`R/tab_xl.R`)

Exports to `.xlsx` via `openxlsx` (Suggests-only dependency). Features:

- Full color formatting matching console output
- Column width auto-sizing, font control, rotated headers
- Sheet management: one sheet per table, or all on one sheet
- Color legend printed as subtext
- Chi-squared statistics displayed
- `hide_near_zero`: cells displaying as 0 are grayed out
- `n_min`: columns/rows with too few observations are grayed out

### tab_kable() — HTML/LaTeX Export (`R/tab_classes.R`)

Uses `kableExtra` for HTML table output. Supports:

- Color formatting via inline CSS
- HTML tooltips (popover) for confidence intervals
- Custom CSS injection via `inst/tab.css`

### tab_md() — Markdown Export (`R/tab_md.R`)

Lightweight standalone export (new in v1.3.1):

- Monospace-precise column alignment with pipe tables
- Bold formatting for total/reference rows
- Handles multi-table lists and compact tables
- Can copy to clipboard or write to file

### tab_plot() — ggplot Visualization (`R/tab_classes.R`)

Creates ggplot2 bar charts from tabxplor tables:

- Uses `ggpubr` and `cowplot` for layout
- Supports grouped/faceted plots by tab_vars
- Auto-maps colors to the table's color scheme

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

All options are set in `.onLoad()` in `R/utils.R`. Users can override via `options()`.

| Option | Default | Description |
| ------ | ------- | ----------- |
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
| `tabxplor.kable_html_font` | DejaVu Sans | Font for HTML kable output |
| `tabxplor.kable_popover` | `FALSE` | Show CI as HTML tooltip |
| `tabxplor.always_add_css_in_tab_kable` | `TRUE` | Inject custom CSS in kable |

## File-by-File Guide

### R/fmt_class.R (3341 lines)

The foundation file. Contains:

- **Lines 1–940**: Public API for `fmt`: constructor `fmt()`, getters (`get_num`, `get_type`, `get_color`, `is_totrow`, `is_refrow`, etc.), setters (`set_num`, `set_type`, `set_display`, `as_totrow`, etc.).
- **Lines 941–1040**: Internal constructor `new_fmt()` and helper `fmt0()`.
- **Lines 1040–1340**: Internal field accessors via `fmt_field_factory()`, reference detection (`get_reference()`).
- **Lines 1340–1630**: `format.tabxplor_fmt()` — the central display method handling 20+ display modes.
- **Lines 1630–1870**: `pillar_shaft.tabxplor_fmt()` — console color rendering, `mutate.tabxplor_fmt()`.
- **`fmt_color_plan()` / `fmt_color_slots()` / `fmt_color_channels()` / `fmt_channel_codes()`** — the vectorised `findInterval` color engine + the shared exporter slot→hex helper (Layer 3 above); `color_measure_policy()` decodes the legacy strings; `color_slot_table()` / `build_slots()` map levels to palette slots.
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
- **Lines 1860–2890**: `tab_plain()` — data.table aggregation core, total rows/cols, fmt wrapping.
- **Lines 2890–4200**: `tab_num()` — numeric variable means/variances, similar structure to tab_plain.
- **Lines 4200–4560**: `tab_pct()` — percentage calculation, difference computation.
- **Lines 4560–4910**: `tab_ci()` — confidence interval calculation (Wilson/Wald/AC methods).
- `tab_chi2()` — chi-squared (factors) + ANOVA F (means) via the vectorised engine
  `agg_chi2()` / `agg_anova()` in `R/tab-agg.R`; contributions to variance for `color="contrib"`.
- **Lines 5200–5809**: `tab_tot()`, `tab_totaltab()`, internal helpers (`diff_index`, `quo_miss_na_null_empty_no`, etc.).
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
- `tab_counts()` — validation + color resolution (mirrors `tab_many()`), then `tab_plain(…, .fine = fine)` →
  `tab_chi2` → `tab_ci` → `tab_add_n_pct` → rewrap (`test` attribute) → `tab_pvalue_lines`. Base-less input
  (non-integer counts) disables CI/chi2 with a message. Weighted = real unweighted `n` + weighted `wn` (§14).

### R/tab_classes.R (3554 lines)

Classes, dplyr methods, and colors. Contains:

- **Lines 1–200**: `new_tab()`, `new_grouped_tab()` constructors, `is_tab()`, validators.
- **Lines 200–900**: Print methods (`print.tabxplor_tab`, `tbl_sum`, `tbl_format_body`, `tbl_format_footer`), `tab_kable()`.
- **Lines 900–1200**: `tab_compact()` — merges multiple row_var tables.
- **Lines 1200–1500**: `tab_plot()` — ggplot visualization.
- **Lines 1500–2400**: Dplyr S3 methods (30+ methods for group_by, select, mutate, filter, arrange, rename, relocate, rowwise, summarise, ungroup, dplyr_row_slice, dplyr_col_modify, dplyr_reconstruct). Also `lv1_group_vars()` helper.
- **Lines 2400–2890**: Tab/grouped_tab vctrs casting methods (`vec_ptype2`, `vec_cast`).
- **Lines 2890–3100**: Color palette constants (6 palettes).
- **Lines 3100–3210**: `set_color_style()`, `get_color_style()`.
- **Lines 3210–3554**: `set_color_breaks()`, `get_color_breaks()`, color legend generation.

### R/tab_xl.R (4132 lines)

Excel export. Main function `tab_xl()` handles:

- Workbook creation, sheet management, column width calculation
- Two-channel color: font-colour styles (text channel) + fill styles (bg channel) driven by `fmt_color_channels()`, stacked with `openxlsx::addStyle(stack = TRUE)`
- Font, border, and number format styling
- Chi-squared statistics and color legend printing

### R/tab_md.R (366 lines)

Markdown export. Standalone file (does not modify existing code). Handles:

- Monospace padding for column alignment
- Bold formatting for total/reference rows
- Sub-table separators for grouped tables
- Clipboard and file output options

### R/utils.R (1306 lines)

Utilities and initialization:

- Pipe re-export (`%>%` from magrittr)
- `.onLoad()` — sets all default options
- `quo_miss_na_null_empty_no()` — helper to check for missing/empty quosures
- Factor manipulation utilities (`fct_recode_helper`, etc.)
- `score_from_lv1()` — scoring helper for survey data

### R/tab_logit.R and R/tab_logit_2.R (WIP)

Entirely commented out. Future logistic regression integration using parsnip/tidymodels. Contains draft code for `multi_logit()`, `readable_OR()`, `or_plot()`. Do not try to use or integrate these — they are a work in progress.

### R/jmvtab.b.R and R/jmvtab.h.R

Jamovi module integration. `jmvtab.h.R` is auto-generated from `jamovi/jmvtab.a.yaml` by
`jmvtools::prepare()`/`install()` (never hand-edit). `jmvtab.b.R` is the R6 backend `jmvtabClass`
whose `.run()` bridges the Jamovi options to `tab()` (Phase 7a baseline — no longer `tab_many()`).
It maps the UI `color` measure (`no`→`FALSE`, `auto`→`TRUE`, else the measure string) + `color_signif`
policy onto `tab()`, forcing `ci = "diff"` when a policy needs significance data, then renders via
`tab_kable()` into the `html_table` HTML result (with manual lightable+bootstrap CSS injection, since
kableExtra classes don't survive in Jamovi). The full cache-aware rewrite is roadmap Phases 7c–7e.
