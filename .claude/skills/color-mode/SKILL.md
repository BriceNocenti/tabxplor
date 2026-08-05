---
name: color-mode
description: Add, change, or split a color measure/channel/significance-policy of tabxplor tables (the `color=` / `color_signif=` arguments, e.g. "diff", "ratio", "contrib", "or", positional two-channel c(text, background), per-type c(pct=, mean=), and the "ignore"/"grey_non_signif"/"guaranteed_effect" policies). Use whenever touching the color engine, the per-measure scoring, the legend, color breaks, palettes, or how factors vs numerics are colored.
paths: ["R/fmt_class.R", "R/tab_classes.R", "R/tab.R"]
allowed-tools: Read, Grep, Edit
---

Since 2.0.0 coloring is decomposed into **three orthogonal axes** feeding **one vectorised
`findInterval` engine**. The `color` GRAMMAR (Phase 13a): **position picks the channel** (1st value
-> text, 2nd -> background), **names pick the column type** (`pct` / `mean`).

- **Axis M — measure**: `diff`, `ratio`, `contrib`, `or` (auto-dispatched by column type: for
  factors `diff` = pp-difference, for numeric means `diff` = standardized Glass's Δ; `ratio` = RR /
  mean-ratio; `contrib` = signed χ² contribution; `or` = empirical odds ratio).
- **Axis C — channel**: `text` colour and `background` fill. Stored in the EXISTING per-column
  `color` attribute, length ≤ 2: `get_color(x)` = text `[1]`, `get_color_bg(x)` = bg `[2]` (`NA` when
  absent). Only `diff`/`ratio` may go on the background.
- **Axis S — significance policy**: the per-column `color_signif` attribute
  (`get_color_signif(x)`): `"ignore"` / `"grey_non_signif"` / `"guaranteed_effect"`.

The old flat strings (`"diff_ci"`, `"after_ci"`, `"ci"`) are soft-deprecated compositions still
accepted at the boundary. Phase 17d: they are decoded ONCE by `color_decode_legacy()` (R/tab.R) in
`normalize_color_spec()` (and in `tab_ci()` for the deprecated step path) into the clean
`(measure = "diff", color_signif = policy)` pair — nothing downstream re-parses them, the stored
`color` attribute is always a clean measure, and `"ci"` folds into `after_ci` (the old single-shade
`single0` mode is gone). `"after_ci"`/`"diff_ci"` with `ci = "cell"` now error (use `ci = "diff"`).

Re-grep exact line numbers before editing; anchors below drift.

## The engine (R/fmt_class.R) — one path, every consumer

`fmt_color_plan(x, channel, color, signif)` → a per-(column,channel) plan
(`measure, policy, score, center, strict, over_breaks, over_slots, under_breaks, under_slots, gate`).
`fmt_color_slots(x, plan)` → per-direction `findInterval(mag, over/under_breaks)` → a palette **slot
integer** (0 = uncolored, **1..4 = over intensities, 5..8 = under**). `fmt_color_channels(x)` →
`list(text_slot, bg_slot)`. Significance is read from the stored bounds `get_ci_inf`/`get_ci_sup`.
There is **no x2/slot-11 override anymore** — the "×2 rule" is just a 1-break `pct_ratio` scale carried
on the background channel (`color = c("diff", "ratio")`, default `pct_ratio = list(over = 2)`).

- **Add/rename a measure** (Phase 17d): add ONE row to the `MEASURES` fact table (R/fmt_class.R) —
  its legend fields (word/break glyphs/ref_kind/…) AND its engine fields (`raw` getter closure,
  `scale = c(std=, pct=)` keys, `std_when`, `sig_source ∈ {bounds,pvalue}`, `gate_row ∈
  {refrow,totrow}`). `fmt_color_plan()` reads them; the only per-measure code left there is policy
  (the diff↔ratio bound rescale + the `guaranteed_effect` floor).
- **Never read `MEASURES[[m]]` directly** (Last Phase z4) — go through **`measure_facts(m, policy)`**,
  which folds in a row's optional `guar` list under `guaranteed_effect`. It is the only reason the
  colour plan and the legend describing it cannot diverge; both call it (1 site in `fmt_color_plan`,
  5 in the legend, each passing `plan$policy` / `spec$policy`).
- **A measure whose baseline is ANOTHER COLUMN** (Last Phase z5: `adjustment` = vs the observed/crude
  effect, `between_groups` = vs the first `split_var` group) cannot use the row-reference machinery at
  all -- `fmt_broadcast_last()` groups by runs of `in_refrow` and crosses a split boundary. Its
  counterpart is written into the per-cell **`obs` field at BUILD time** (`reg_build` /
  `reg_write_group_obs`, R/tab_reg.R), and `fmt_adjustment_score()` reads it. Three facts drive the
  rest: `ref_kind = "observed"/"group"` (checked by `measure_own_ref()`, which makes the measure name
  ITSELF in the legend instead of borrowing the column's effect word, and resolves its reference phrase
  PER CHANNEL); `std_when = "additive"` (the scale keys off the ESTIMATE's `ci_type`, since `Model_OR`
  and `Model_AME` are both `type = "row"`); and `force_policy = "ignore"`, applied by
  **`measure_policy(m, policy)`** -- the twin of `measure_facts()`, called by the plan AND the legend,
  for a measure with no significance test of its own. Scales `adj_ratio` / `adj_diff`. The SIGN is
  away-from/toward the NULL, never raw up/down (else a protective effect colours backwards).
- **`contrib` is the one measure that changes reading with the policy.** `ignore`/`grey_non_signif`
  score the relative contribution (`ctr / mean_contrib`, `contrib` scale — the CA reading, relative to
  the table); `guaranteed_effect` scores the ADJUSTED standardized residual (`fmt_resid()`) on the
  absolute 7th scale `residual`, whose first break is re-anchored to `z(conf_level)` via
  `offset_guaranteed_breaks(..., origin)` (the `break_origin = "threshold"` fact). Significance for
  BOTH comes from the stored residual p-value (`contrib_pvalue()` in R/tab.R), computed on the
  unweighted `n` / Kish `n_eff` base — never the weighted N, never the Pearson residual.
- **`fmt_resid()`** derives the residual from `pvalue` + `sign(ctr)` — there is no fmt field for it.
  It MUST be `-qnorm(p/2)`; `qnorm(1 - p/2)` saturates to Inf for every `|z| > 8.2`.
  Numeric standardized diff divides by `sqrt(get_ref_var(x))`.
  **`guaranteed_effect` floor MUST be on the measure's own scale** (the fold's `center`): `diff` → the
  stored diff bound (centre 0); `or` → the native OR bound (centre 1); `ratio` (no native CI) → convert
  the shared diff floor `1 + (get_ratio − 1)·(guar_diff/get_diff)` (centre 1). Feeding a diff bound
  (~0.05) into a centre-1 fold gives `1/0.05` → max under-colour on every cell (the ratio-flood bug).
- **slot lookup**: the level→intensity map lives WITH the scale now — `mk_color_scale()` precomputes
  `over$slots` / `under$slots` (1..4) via `intensity_slots(k)` (fewer than 4 breaks drop the 2nd, then
  4th, then 1st; an explicit `NA` in a break vector skips a slot). `fmt_color_plan()` reads them
  directly; `color_slot_table`/`build_slots` are gone.
- **Every consumer maps the slot integer → colour the same way**: console `pillar_shaft`
  (two-channel), `fmt_get_color_code()` (single-channel, golden), the shared exporter helper
  `fmt_channel_codes()` (text hex + bg hex, used by tab_kable/tab_plot/tab_xl), `tab_md`'s
  `md_slot_class_map()`, and `tab_color_legend()`. All index an **8-hex vector** (`get_color_style()`
  = 4 over + 4 under). Do NOT special-case an exporter — route through these.
- **The legend (Phase 13b, R/fmt_class.R)** is `legend_specs(x)` (per col_var group: measure/breaks/
  ref/method/policy/shade + reg effect word) → `legend_tokens_terse`/`_prose` (a TOKEN stream) →
  `legend_render_line(medium)` (console crayon / html `text_spec` / md pandoc span / excel `fmt_txt`
  runs / plain). `tab_color_legend(x, medium=, style=, lang=)`: console = terse, exports = prose
  (translatable via `gettext`, domain `R-tabxplor`, FR in `po/R-fr.po`; `lang` sets the `LANGUAGE`
  env for the build). It NAMES the CI method/level from the table's stored **`ci_settings`** attribute
  (`list(conf_level, method_cell, method_diff, method_ratio, method_mean_diff, method_mean_ratio)` since
  14v-ii — the legend picks the relevant one off the column type/ci_type, set in `tab_assemble_tables`, carried through dplyr;
  `default_ci_settings()` fallback). Shade names ("blue"/"yellow-red") come from `legend_shade_names()`
  (default palette only). Changing legend wording → regen `_snaps/golden.md` + `_snaps/render-html.md`
  (per-CELL hex `test-color-golden.R` must stay green — the legend never touches cell colours).

## Config (R/tab_classes.R)

- **Breaks** — the canonical option `"tabxplor.color_breaks"` is a named list of 5 scales
  (`pct_diff, pct_ratio, mean_diff, mean_ratio, contrib`), each
  `list(center, strict, std, over = list(breaks, slots), under = list(breaks, slots))`. Both sides are
  POSITIVE magnitudes; the engine folds each cell to a magnitude and picks the side by direction.
  `set_color_breaks(list(...))` or `set_color_breaks(pct_diff = ..., ...)`; input is signed/reciprocal
  literals (one-sided auto-mirrors, two-sided as-is, `NA` skips a slot) or `list(over =, under =)` (no
  mirror; omit a side to switch it off). Validators in `mk_color_scale()` + `parse_color_side()`;
  defaults in `default_color_scales()`; `.onLoad` seed in `utils.R`. `get_color_breaks()` round-trips;
  `type = "all"` gives the signed engine breaks. Per-table override: `tab(color_breaks = list(...))`
  stores a table attribute; render entry points install it transiently via
  `push_color_breaks()`/`pop_color_breaks()` (fallback to global if absent/malformed).
- **Palette** — 8 OKLCH base palettes (`default_*_colors`, light/dark × text/bg × pos/neg, 4 hex each),
  wired into `tabxplor_palette_env` by `build_palettes()`; customise with `set_color_palette()`.
  `get_color_style(mode, type, theme)` returns an 8-element slot vector (4 over + 4 under) — crayon
  functions for the console (24-bit, or the curated 8-bit `palette_8bit` in the RStudio console),
  hex for exports. NO `html_24_bit` / green_red/blue_red variants / `custom_palette` anymore.

## Arg parsing (R/tab.R)

`normalize_color_spec(color, color_signif)` → a spec `list(mode, legacy, text, bg, types, signif)`.
`mode` ∈ `off`/`auto`(TRUE)/`flat`(positional)/`by_type` (named / `list(pct=, mean=)`). `legacy` is the
single string fed to the pipeline for ci/chi2 side-effects. `finalize_color_spec()` →
`resolve_col_measures()` writes the two-channel + policy attributes per column. `tab()`/`tab_num()`/
`tab_many()` call these + set the `color_breaks` attribute last.

## Verify

- Temp `.R` file → `Rscript` (never `Rscript -e` on Windows); `devtools::test("d:/Statistiques/
  github/tabxplor")` with `Sys.setenv(NOT_CRAN="true")` so the snapshot tests run. Watch
  `test-color-golden.R` (per-cell hex), `test-color-config.R`, `test-color-engine.R`,
  `test-render-html.R`, `test-golden.R`, `test-tab_md.R`.
- Eyeball a console print + one `tab_kable()`/`tab_md()`, check the legend matches the cells (esp.
  numeric `diff` → SD thresholds; over-only ratio → no under side).
- Intentional output change → regenerate consciously (`dev/make_color_golden.R`;
  `snapshot_accept("render-html")`/`("golden")`). Document `@param` values, `devtools::document()`,
  add a NEWS bullet.
