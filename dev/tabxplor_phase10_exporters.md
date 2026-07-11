# tabxplor 1.4.0 — Phase 10: Unified exporter prep & display (architecture)

<!--
PURPOSE: The single, self-contained design brief for Phase 10 (exporter unification + display rework).
ROLE: Governs the Phase 10c→10g implementation. Expands CLAUDE.md roadmap "Phase 10" and
      dev/tabxplor_1.4.0_decisions.md §7, §8, §10, §21, §22, §23, §33.
KEY CONSTRAINTS:
  - Public exporter args are back-compat-LIGHT: old args soft-deprecate, never hard-error.
  - tabxplor_fmt FIELD contract (users read via $/mutate) unchanged; one new optional ATTRIBUTE only.
  - Display byte-identity is golden/parity-locked at every step (test-golden.R, _snaps/, test-export-parity.R).
  - Stay on openxlsx v1 (the openxlsx2 swap is Phase 11), behind a narrow backend seam.
See: CLAUDE.md § "Phase 10", dev/tabxplor_1.4.0_decisions.md §33 (the decision record).
-->

Status: **10c DONE (2026-07-12); 10d→10g not started.** 10a decision settled (below); this document is the
10b deliverable and governs 10d→10g. Read this first, then the matching `dev/tabxplor_1.4.0_decisions.md`
sections and `dev/tabxplor_architecture.md` "Export System".

**Phase 10c done (2026-07-12), all byte-identical (golden/color/export-parity green; conscious
structural regen only for the new `display_spec` attribute):**
- **§3 `get_reference()` + `format()` rework.** `get_reference()` `case_when` → base boolean (the branch
  selectors are scalar attributes; the arms are per-cell boolean of the field masks — A/B-verified across
  153 col×mode + the subset-equivalence the `.ref` memoization relies on). `format()`/`pillar_shaft` gained
  `.ref = list(cells =, all_totals =)` (masks derived ONCE, memoized when `NULL`) + the hot `dplyr::if_else`
  → base `ifelse` + `str_detect("^-")` → `startsWith`. **Bonus lever (not in the plan):** the unconditional
  `x$var` (`$.tabxplor_fmt` → `dplyr::pull`, ~28 % of `format()` self-time) → `get_var(x)`. Net `format()`
  ~2× faster on the exporter path (`dev/benchmarks/results_1.4.0/phase10c_profile.txt`). **`numfmt()` →
  `format(syntax="excel")` DEFERRED to 10g** (atomic removal, no duplicate-source-of-truth window).
- **§4 robust detection + graceful degrade.** New `tab_render_vars()` (`R/tab.R`) + `tab_degrade_inform()`;
  the print methods route `row_var` through it, and `tab_kable`/`tab_md`/`tab_plot`/`tab_xl` gained a
  degrade guard rendering the plain frame + a message (no more `pull(integer(0))` crash). `tab_get_vars()`
  hardened. New `test-edge-cases.R` section. The full role-detection UNIFICATION (replacing each exporter's
  inline detection) stays in **10d** (via `tab_export_prep`).
- **§6 `display_spec` (9→10 attribute).** Curated whitelist `c("pct (n)", "n (pct)")` (not the full parser),
  set via `tab(display = )` / `set_display_spec()`, parsed only in `format()` (text backends; Excel falls
  back to the primary field). `test-fmt-contract.R` 9→10 + snapshot accepted; structural RDS golden regen.
- **DEFERRED to their consumer (maintainer decision):** `tab_totcol_range()` → 10d (with the prep that
  wires it); label-capture-in-build → 10e (only `tab_kable` consumes it); `numfmt` fold → 10g.

---

## 0. Why — current-state grounding

Four exporters grew independently and each re-derives the same things every render:

- **The "canonical col_vars → validate → compact" preamble is duplicated 4×** — `tab_kable`
  ([tab_classes.R:510-528](../R/tab_classes.R#L510)), `tab_md` ([tab_md.R:46-64](../R/tab_md.R#L46)),
  inside `tab_compact()` ([tab_classes.R:1025-1035](../R/tab_classes.R#L1025)), and `tab_xl`'s inline
  non-compacting variant.
- **Per-exporter role detection duplicated** across kable/md/plot: `tab_get_vars()`, `get_subtext()`,
  `group_indices()`-based group boundaries, `ungroup()`+drop-tab_vars, `tab_wrap_text()`, and the
  `fmt_cols`/`other_cols`/`totcols` (`is_totcol`) / `totrows` (`is_totrow`) / `new_col_var` block
  (kable [tab_classes.R:570-582](../R/tab_classes.R#L570), plot
  [tab_classes.R:1272-1284](../R/tab_classes.R#L1272), md [tab_md.R:83-110](../R/tab_md.R#L83)).
- **Two non-unified display paths.** `format.tabxplor_fmt()`
  ([fmt_class.R:1608-1876](../R/fmt_class.R#L1608)) is the source of truth for kable/md/plot/console; but
  `tab_xl` **bypasses** it — it writes raw `get_num()` numbers and rebuilds display via `numfmt()`
  ([tab_xl.R:1076-1108](../R/tab_xl.R#L1076)) into Excel number-format codes. Stars, the `label`
  attribute, and NA-hiding therefore silently diverge between the two paths. The oracle is
  `test-export-parity.R` — **which today only covers `n`/`wn`/`pct`/`mean`**
  ([test-export-parity.R:28](../tests/testthat/test-export-parity.R#L28)), so it does NOT guard `diff`,
  `ci`, `or`, stars, composite display, or `[min;max]`.
- **The bold-row / reference logic is copy-pasted** (kable [tab_classes.R:711-716](../R/tab_classes.R#L711),
  md [tab_md.R:114-125](../R/tab_md.R#L114), plot [tab_classes.R:1287-1297](../R/tab_classes.R#L1287),
  xl [tab_xl.R:594-596](../R/tab_xl.R#L594)); each calls `get_reference()` again.
- **`format()`/`get_reference()` re-run `case_when` up to 4× per column.** `format()` calls
  `get_reference()` at [fmt_class.R:1793](../R/fmt_class.R#L1793) / [:1803](../R/fmt_class.R#L1803) /
  [:1829](../R/fmt_class.R#L1829); `pillar_shaft` again at [fmt_class.R:2009](../R/fmt_class.R#L2009); each
  exporter once more per column. `get_reference()` ([fmt_class.R:2581-2687](../R/fmt_class.R#L2581)) is
  3 outer branches × `switch(mode)` × `dplyr::case_when`, each allocating `rep(FALSE, length(x))`.
- **Var detection is fragile and crashes.** `tab_get_vars()` ([tab.R:2171-2198](../R/tab.R#L2171)) derives
  `col_vars` robustly (from fmt columns' `col_var` **attribute**) but `row_var` = *the last factor column*
  (heuristic) and `tab_vars` = *the other factors*. With no factors, `row_var = character(0)` →
  `which(names == …)` = `integer(0)` → `dplyr::pull(tabs, integer(0))` **errors** at
  [tab_classes.R:568/716](../R/tab_classes.R#L568) (and its twin 1256/1265). Factors positioned AFTER the
  fmt columns → silently-wrong detection.

**The Phase 5 color-engine rewrite already fixed the historical #1 cost.** The §23 profile (2026-07-08)
pinned ~75 % of `tab_kable` render in `fmt_color_selection` — but that function was **deleted in Phase 5**
(the engine is now the `findInterval`-based `fmt_color_channels`/`fmt_channel_codes`, 48–1290× faster). So
§23's *ranking of levers is stale*; the current dominant costs must be **re-measured** before 10e. What
remains structural is the *duplicated recompute across columns and exporters* — which the shared prep +
render-model removes by deriving each quantity once.

---

## 1. The normalized render-model — an ephemeral `tabxplor_render` sidecar

**Decision: a separate, ephemeral S3 list, NOT new tab columns/attributes.** The `tabxplor_tab` already
carries the raw per-cell data (`n`/`wn`/`pct`/`diff`/`ci_*`/`pvalue`/… in the `tabxplor_fmt` record). The
render-model holds only the **derive-once** quantities each exporter re-derives today, plus table-level
metadata. It is built once by the prep, consumed by one backend, then discarded. It is NOT stored on the
tab because dplyr `rename`/`select`/`relocate` desync bare attributes (the exact reason var-detection
option B-as-a-custom-attribute is rejected — §4).

```r
structure(
  list(
    tables = list(                      # length 1 when compacted; N for list / xl sheets
      list(
        tab   = <tabxplor_tab>,          # the tab actually rendered (canonical raw data)
        vars  = list(row_var, col_vars, col_vars_levels, tab_vars, degrade = FALSE),
        roles = list(fmt_cols, other_cols, row_var_col, totcols, totrows, no_totrows,
                     real_col_vars, has_multi_col_vars, new_col_var, new_group, align),
        ann   = <named list by fmt-col name>,   # per-column derive-once sidecar (below)
        bold_rows    = <int>,            # reference/total rows to embolden
        range_totcol = list(col = , text = , differ = ),   # §5 [min;max] injection
        subtext      = <chr>             # subtext incl. colour legend when requested
      )
      # ... one entry per rendered table
    ),
    labels = <named chr: var -> question text | NULL>,   # §7 label attribute (kable tooltip)
    meta   = list(backend, compact, theme, color_type, html_24_bit, transpose, compute = <flags>)
  ),
  class = "tabxplor_render"
)
```

Per-column sidecar `ann[[col]]` (each vector length `nrow(tab)`), gated by `meta$compute`:

```r
list(
  ref_cells  = <lgl>,   # get_reference(col, "cells")        computed ONCE
  ref_alltot = <lgl>,   # get_reference(col, "all_totals")   computed ONCE
  text_slot  = <int>, bg_slot = <int>,   # fmt_color_channels(col)  (xl -> style objects)
  text_hex   = <chr>, bg_hex  = <chr>,   # fmt_channel_codes(col)   (kable/plot; NULL for md/xl)
  stars      = <chr>,   # get_stars(col)  (xl only; text backends get stars inside the format() string)
  blank      = <lgl>,   # get_display(col) == "blank"
  has_color  = <lgl(1)>
)
```

**Both consumption paths read the SAME model** — this is the unification. Number → glyph is the *only*
backend-specific step (a rendered string vs a real Excel number), which is exactly why the `tab_xl` bypass
cannot fold into `format()` strings:

- **Text backends (kable/md/plot):** cell glyph = `format(col, special_formatting = TRUE, na = "",
  .ref = ann[[col]])` (the string already contains stars), coloured with `ann$text_hex`/`ann$bg_hex`
  (kable `cell_spec`, md short pandoc spans, plot fill). Bold rows from `bold_rows`. Total-column cells
  overwritten from `range_totcol$text`.
- **Excel backend:** raw `get_num(col)` + `get_digits(col)` + the Excel numfmt code from
  `format(col, syntax = "excel")` + `ann$text_slot`/`ann$bg_slot` → the pre-built openxlsx style objects
  (`font_styles`/`fill_styles`, [tab_xl.R:232-234](../R/tab_xl.R#L232)). Stars appended into the numfmt
  code (§9). `[min;max]` written as text cells (§5).

The **reference masks, colour slots, stars, blank mask, bold rows, `[min;max]`, and labels are computed
once and shared verbatim** across whichever backend runs.

---

## 2. The shared prep helper — new `R/tab-export-prep.R`

One entry point. All four exporters become `prep <- tab_export_prep(...); render_<backend>(prep)`.

```r
tab_export_prep <- function(
  tabs,
  backend    = c("kable", "md", "xl", "plot"),
  compact    = TRUE,                 # single-tab merge; FALSE keeps the list (xl sheets / list method)
  transpose  = FALSE,                # §8 opt-in transpose-at-export (applied FIRST)
  wrap       = NULL,                 # list(rows, cols, whitespace_only, ...) | NULL to skip (jamovi speed)
  compute    = c("refs", "colors", "codes", "stars", "range", "labels", "spans"),
  color_type = NULL, theme = "light", html_24_bit = NULL
) { ... }                            # -> tabxplor_render
```

Computes **once** (replacing the 4× duplication and the per-exporter role detection):

1. **Transpose** (`transpose = TRUE`) via `tab_transpose()` at the very start, so every downstream
   derivation runs on the transposed structure (§8).
2. **Canonical col_vars validation + compaction.** Extract the duplicated preamble into
   `tab_check_same_col_vars(tabs)` → `longest_col_vars` + validation, then `tab_compact()` (reuse the
   existing exported function; do NOT reimplement). `compact = FALSE` or tab_vars-present keeps the list →
   one render unit per table.
3. **Robust var detection** via `tab_render_vars()` (§4), returning `vars` or `degrade = TRUE`.
4. **`get_subtext()` + colour legend (`tab_color_legend`) + label extraction** into `render$labels`.
5. **Group boundaries** `dplyr::group_indices()` → `new_group` (once; today in kable:552 / md:75 / plot:1260).
6. **`ungroup()` + drop tab_vars** (kable/md/plot drop; xl keeps unless `remove_tab_vars`).
7. **`tab_wrap_text()`** once (skipped for xl — Excel wraps via cell style — and when `wrap = NULL`).
8. **Role detection once**: `fmt_cols`/`other_cols`/`totcols`/`totrows`/`no_totrows`, `real_col_vars`,
   `has_multi_col_vars`, `new_col_var` transitions, `row_var_col`, `align`.
9. **Per-column `ann`** — the expensive shared derivations (`get_reference` at the needed modes,
   `fmt_color_channels`/`fmt_channel_codes`, `get_stars`, blank mask), gated by `compute` so jamovi/live
   opts out of what a backend doesn't use (e.g. no `codes`, no `range`, no `spans`).
10. **`bold_rows`** — the `select(-where(all), -where(~ !any(.))); rowSums == ncol` logic (today copy-pasted
    in kable:711-713 / md:114-125 / plot:1287-1297 / xl:594-596), computed once from `ann$ref_alltot`.
11. **`tab_totcol_range()`** (§5) when `"range" %in% compute`.

**Base vs list split (decisions §8).** `tab_export_prep` always returns `tables` as a list. A single tab →
length 1; a list / `compact = FALSE` / tab_vars → N. Table-level `labels`/`meta` are shared. Each backend's
**base method** renders `tables[[1]]`; the **list method** maps the base renderer and assembles the
container:

- **kable** — `htmltools::tagList` (an HTML container holding several kables; they cannot be row-bound).
- **md** — the per-table strings joined by blank lines (sequential).
- **xl** — one write per table into sheets, driven by the existing `sheets = "auto"/"tabs"/"unique"` logic.
- **plot** — soft-deprecated (§11); list method stacks with `cowplot` or renders `tables[[1]]` + a message.

---

## 3. `format()` / `get_reference()` rework (10c, golden-locked byte-identical)

Three independent changes, each locked by `test-golden.R` RDS + `_snaps/` + `helper-color-golden.R`
(per-cell hex, which routes through the same masks):

1. **Kill the `case_when` in `get_reference()`** ([fmt_class.R:2581-2687](../R/fmt_class.R#L2581)). Rewrite
   each of the 3 outer branches × `switch(mode)` × `dplyr::case_when` arms as **direct boolean algebra**
   over the already-cheap primitive masks (`totrows`, `totcol`, `refrows`, `refcol`, `tottab_ref`) using
   `type`-indexed logical composition. Pure boolean logic ⇒ bit-for-bit identical output; removes the
   per-branch `DataMask` allocation and `rep(FALSE, length(x))`.
2. **Compute each reference mask ONCE, not up to 4×.** Add
   `format.tabxplor_fmt(x, ..., syntax = c("text","excel"), .ref = NULL)`: `.ref` is a small
   `list(cells =, all_totals =)` of precomputed vectors for this column. When `NULL` (console / standalone
   `format()`), it is computed internally once (memoize the three `disp_*` internal calls into one `cells`
   + one `all_totals`) — same output, back-compat. When supplied by the prep (from `ann[[col]]`), it is
   reused, eliminating the per-column recompute across `format()` + `pillar_shaft` + the exporter.
   `pillar_shaft` gets the same optional `.ref`.
3. **Fold `numfmt()` into `format(syntax = "excel")`.** `syntax = "excel"` returns the per-cell Excel
   number-format code (from the same `display`/`digits`/`type`) instead of the rendered string. `tab_xl`
   still writes raw `get_num()` numbers but sources its format codes from the one shared display spec — so
   a digits/display change can no longer silently desync the bypass. Retires `numfmt()`
   ([tab_xl.R:1076-1108](../R/tab_xl.R#L1076)) as a standalone.

**Re-profile before ranking further levers** — the §23 "fmt_color_selection = 75 %" figure predates the
Phase 5 engine rewrite and is stale; measure `format()` / kable on the current `findInterval` engine first.

---

## 4. Robust var detection + graceful degrade (10c)

**Recommendation: graceful-fallback (A) + validated group-metadata (B).**

- Pure A (fallback only) fixes the *crash* but keeps the fragile "last factor" heuristic → the
  factors-after-fmt **silent miswrite stays a correctness bug**.
- Pure B as a *new build-time role attribute* is unsafe: dplyr rename/select desyncs bare attributes.
- The escape already exists: **`tab_build` groups by `tab_vars` ([tab.R:1831](../R/tab.R#L1831)) and
  `tab_compact` groups by `row_var` ([tab_classes.R:1080](../R/tab_classes.R#L1080)).** dplyr *maintains*
  group metadata across rename/select, so `dplyr::group_vars()` is an authoritative, position-independent,
  desync-resistant signal — existing machinery, no parallel structure.

New `tab_render_vars(tabs)` (in `R/tab.R`, wrapping the hardened `tab_get_vars`):

- **`col_vars`** — keep the robust `col_var`-attribute path (already position-independent).
- **`tab_vars` / `row_var`** — prefer `dplyr::group_vars()`, **validated** against live factor columns:
  grouped-not-compacted → `group_vars() == tab_vars`, `row_var` = last factor not in the groups;
  grouped-compacted → `group_vars() == row_var`. Validate each named column still exists AND is a factor;
  else fall back to the heuristic; if nothing resolves (no factors / no fmt / plain df) →
  `list(degrade = TRUE, reason = …)`.

Exporters + print methods check `vars$degrade`; when TRUE they render the plain tibble (kable/md: unstyled
table; xl: `writeData` of the raw frame) and emit `cli::cli_inform()` explaining that tabxplor formatting
was skipped (no factors / no fmt columns / ambiguous layout). `tab_get_vars` keeps its public contract but
gains guards for the three malformed shapes. New `tests/testthat/test-edge-cases.R`: plain data.frame, tab
with no factors, tab with no fmt columns, factor after fmt columns, empty tab, grouped vs ungrouped —
asserting **no error** + a graceful message for all four exporters and `print()`.

---

## 5. Total-column base range `[min;max]` (decisions §10) — table-level pre-pass

The range is **cross-column** (each col_var's own base can differ under `na="drop"`), so `format()` (one
column at a time) cannot see siblings. `tab_totcol_range()` is a **table-level pre-pass** in
`tab_export_prep` (gated by `"range" %in% compute`): from each col_var's per-cell base (`get_tot_n()`, plus
the weighted base recovered `wn/pct`), it computes per row a scalar when the bases are equal, else
`[min;max]` (default) or `min` (global option). The result lands in `render$…$range_totcol` and is injected
into the total column's rendering — **never inside per-column `format()`**.

- **Text backends** overwrite the total-column cell strings with `range_totcol$text`.
- **Excel** writes `[min;max]` as **text cells** where `range_totcol$differ` is TRUE (a range is text, not a
  number); the common all-equal path stays a numeric cell. Documented fallback: Option-C (`min` + a subtext
  note) if the text-cell mix proves awkward for a given workbook.

---

## 6. Opt-in multi-field display (10c) — new optional attribute `display_spec`

**Decision (this session): full flexible syntax (`pct (n)`, `pct ± ci`, …), strictly opt-in, ZERO cost on
the non-using path.** Mechanism: a **new optional per-column attribute `display_spec`** (`NA` default),
parsed **only inside `format()`** when non-NA.

- The stored `display` attribute stays a **single primary token** (`"pct"`/`"n"`/`"diff"`/…) and keeps
  driving `get_num`/`set_num`/coloring/Excel unchanged — so `get_num` and the whole hot path are
  byte-identical when `display_spec` is unused (one scalar `is.na()` gate per column in `format()`).
- `display_spec` (e.g. `"pct (n)"`) is parsed into a `{field, literal}` token sequence and rendered by
  `format()` for **text backends only**. The composite `format()` reuses the existing per-field getters
  (`get_pct`/`get_n`/`get_ci_moe`/…) so no new field math.
- **Excel** falls back to the primary field (a composite string is not a number) — documented.
- Set from `tab(display = "pct (n)")` (per col_var, like `display` today) → stored as the attribute.
- **`/vctrs-field` change**: 9 → 10 per-column attributes. Touch-list: `new_fmt()` (default
  `display_spec = NA_character_`), the attribute accessor pair (`get_display_spec`/`set_display_spec`),
  `vec_ptype2`/`vec_cast`/`vec_arith` reconcilers (carry the attribute), `format()`, and
  `test-fmt-contract.R` (9→10, conscious regen) + goldens (structural only — display byte-identical when
  unused). See `/vctrs-field`.

---

## 7. The `label` attribute (decisions §22) — kable header tooltip only

**Decision (this session): render the variable `label` (survey question text) as a header tooltip in
`tab_kable` only** — NOT a cross-backend feature (this narrows §22's "label → all"). The render-model
carries `render$labels` (a named `var -> question` vector, captured in the prep from the source variables'
`label` attribute, `labelled`/`haven`-style, `Suggests`-guarded). Only `tab_kable` consumes it, via its
existing tooltip mechanism on the variable-name header cells. md / xl / console ignore it (no clutter). In
jamovi (`tooltips = FALSE`) the label tooltip is naturally absent — acceptable, the variable names show.

---

## 8. `tab_transpose()` (10d) — finish + opt-in transpose-at-export

The stub ([tab.R:2133-2155](../R/tab.R#L2133)) is **fully commented-out and NOT exported** — a clean slate
(decisions §7's "already `@export`ed at tab.R:1773" is **stale**; correct it). Finish it for the
**single-total-row / single-total-col** case (exactly the col%-invert use case — one total row + one Total
column):

- Namespaced verbs (`tidyr::pivot_longer`/`pivot_wider`, `dplyr::`, `forcats::`, `rlang::`).
- Colours / refs / `subtext` / `test` ride the per-cell fmt fields + table attributes → a pivot moves them
  intact; only the **axis flags** are swapped (`set_type` row↔col, `in_totrow`↔`in_totcol`,
  `in_refrow`↔`in_refcol`), and `test` is re-keyed by the new col_var (old row_var).
- Total-row↔total-col styling swap happens **automatically** in the render-model: after transpose,
  `roles$totrows`/`totcols` and `ann$ref_alltot` are recomputed from the transposed tab, so every backend
  follows without special-casing.
- **Opt-in `transpose = FALSE`** on `tab_kable`/`tab_md`/`tab_xl`, applied at the START of
  `tab_export_prep` (§2 step 1). Console `print()` never transposes. `cli::cli_warn` on `pct="col"` +
  several row_vars, pointing to invert-then-transpose.
- **Defer tab_vars generalization** (matches "compact + tab_vars deferred"): keep the single-total contract,
  replace the stub's `stop()` on ≥2 totals with a clear `cli::cli_abort` naming the limitation.

---

## 9. `tab_xl` backend seam (10g) — new `R/tab-xl-backend.R`

`xl_backend_openxlsx1()` = a list of ~12 closures wrapping all 13 live `openxlsx::` calls (decisions §21):
`new_workbook`, `base_font`, `add_sheet`, `write_data`, `make_style` (**memoized** on its args → the ~11
palette styles + `st_digits*` set created once, the openxlsx2 speed lever pre-installed), `apply_style`
(`stack = TRUE`), `freeze_panes`, `set_widths`, `set_heights`, `cond_format`, `save`, `open`. The `tab_xl`
orchestration (index maps, sheet/offset math, style grouping) calls **only** through the seam → Phase 11
supplies `xl_backend_openxlsx2()` and swaps one line, leaving the shared prep + colour→style selection +
orchestration untouched.

- **Number formats stay in `format(syntax = "excel")`** (§3), not a standalone `numfmt()`.
- **Stars via the Excel numfmt literal** (decisions §16/§22): append `"***"` into the numfmt code, grouping
  by `(digits, type, display, stars)` (~4 extra format groups) — preserves the real-number invariant that
  justifies the whole bypass; no text cell, no adjacent column.
- **Base(single-tab) + list(sheets) split** (§2). `tab_xl` today is list-first with no single-tab path
  ([tab_xl.R:91](../R/tab_xl.R#L91)); the base method is the new one.
- **Plain-data.frame support** via the `degrade` path (§4): a non-tabxplor df writes its raw cells.
- **Extend `test-export-parity.R`** to `diff`/`ci`/`or`/stars/label/`[min;max]` — the green v1 baseline that
  **gates Phase 11**.

---

## 10. jamovi live display (10a) — DECIDED: keep + optimize kableExtra; home-built HTML is Plan B

jamovi's results panel **only honors inline CSS** (CSS via `htmlDependencies` never applies —
[jamovi #1529](https://github.com/jamovi/jamovi/issues/1529)) and **won't reliably run htmlwidget JS**, so
interactive tables (reactable/DT) are out and the module already hand-inlines lightable + bootstrap CSS
([jmvtab.b.R:159-165](../R/jmvtab.b.R#L159)).

**Decision: keep kableExtra everywhere and optimize it first; only fall back to a dependency-free
home-built `<table>` renderer if that isn't fast/clean enough.** No reactable/JS, no tinytable, no gt. The
speed comes from the shared prep, not a new engine:

- Compute colours/refs **once** in the prep (fewer `format()`/`get_reference()` passes) — the structural win
  now that the Phase 5 engine already killed the old `fmt_color_selection` cost.
- Move NA-hiding into the prep (retire the `interactive()`-gated whole-string `str_replace`,
  [tab_classes.R:763-772](../R/tab_classes.R#L763)).
- Keep `tooltips = FALSE` for the jamovi/light path (already Phase 7e; ~44 % DOM, ~15 % build).
- A "light" kableExtra path (fewer `row_spec`/`column_spec` xml round-trips).

**Architectural implication — isolate final HTML generation behind a small `render_kable_html()` seam**
(same idea as the tab_xl backend seam), so the render-model is engine-agnostic and the eventual home-built
swap is one localized function, not a rewrite. **Fallback trigger:** if, after the prep + light-mode
optimizations, the jamovi live render is still the interaction bottleneck (re-profile on the current
engine), implement `render_kable_html()`'s home-built variant.

---

## 11. `tab_plot()` — soft-deprecate (decisions Q1)

Keep exported; mark `lifecycle` experimental/superseded (do NOT hard-remove from NAMESPACE). Refactor it to
consume `tab_export_prep(backend = "plot")` like the others (so it doesn't rot), but do not invest in its
display; it is kept for future improvement.

---

## 12. Sequencing (10c → 10g) and per-step verification

| Sub-phase | Ships | Verify |
|-----------|-------|--------|
| **10c** format() + detection | `get_reference` boolean rewrite; `format(syntax, .ref)` + `numfmt`→excel; `tab_render_vars` + graceful degrade + `tab_get_vars` guards; `tab_totcol_range()`; `display_spec` (9→10 attr); label capture in build | `test-golden.R`/`_snaps/`, `test-fmt-contract.R` (regen 9→10), `test-color-golden.R`, `test-export-parity.R`; **re-profile format()**; new `test-edge-cases.R` |
| **10d** shared prep | `tab_export_prep`, `tab_check_same_col_vars`, render-model + `ann`, base/list split, `tab_transpose`; refactor kable/md/plot to consume it | byte-identical: kable/md snapshots + jamovi `.render_html` unchanged; `test-benchmark.R` no regression |
| **10e** kable | NA-hiding in prep, `add_header_above` spanning header, `n_min`/`hide_near_zero` grey, label tooltip, light mode, `render_kable_html()` seam | kable snapshots + jamovi live render unchanged (except intended NA/tooltip); DOM-size check |
| **10f** md | short pandoc colour spans from `ann` slots, wrap-not-truncate, optional title, list method | `test-tab_md.R` snapshots (conscious regen for colour spans) |
| **10g** xl | base+list via seam, `format(syntax="excel")`, stars-in-numfmt, `[min;max]` text cells, plain-df support, tab_logit borders | **extend `test-export-parity.R`** (diff/ci/or/stars/label/range); `test-tab_xl.R` incl. a non-tabxplor df — the v1 baseline that gates Phase 11 |

Cross-step landings: `[min;max]` helper (10c) → wired in prep (10d) → consumed text (10e/10f) / text-cell
(10g). Label capture (10c/build) → rendered (10e only). Stars-in-xl (10g; text backends already have them).
`tab_transpose` mechanism (10d) → `transpose=` arg wired per exporter (10e/10f/10g).

---

## 13. Flagged inconsistencies / open items (confirm as we go)

- **`test-export-parity.R` is under-scoped** — only `n`/`wn`/`pct`/`mean`
  ([:28](../tests/testthat/test-export-parity.R#L28)). It stays green even if stars / `diff` / `ci` / `or` /
  `[min;max]` diverge. MUST be extended in 10g or it does not guard the new shared surface.
- **decisions §7 tab_transpose note is stale** ("already `@export`ed at tab.R:1773") — it is
  commented-out / unexported. Corrected in the decisions doc §33.
- **decisions §23 profile is stale** — `fmt_color_selection` (its #1 lever) was deleted in Phase 5;
  re-profile on the current engine before ranking 10e levers.
- **decisions §22 "label → all" is superseded** — this session narrows it to **kable header tooltip only**.
- **Excel `[min;max]` as text cells** breaks the all-numeric total column only under `na="drop"` with
  differing bases (rare); Option-C (`min` + note) is the documented fallback.
- **compact + tab_vars** stays deferred (two-level nested rendering) — not entangled with this unification.
- **Re-profile is a precondition for the 10e/10a lever ranking and the home-built-HTML fallback trigger.**
