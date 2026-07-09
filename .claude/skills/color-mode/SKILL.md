---
name: color-mode
description: Add, change, or split a color measure/channel/significance-policy of tabxplor tables (the `color=` / `color_signif=` arguments, e.g. "diff", "ratio", "contrib", "or", two-channel c(text, background), and the "ignore"/"grey_non_signif"/"color_all_signif" policies). Use whenever touching the color engine, the per-measure scoring, the legend, color breaks, palettes, or how factors vs numerics are colored.
paths: ["R/fmt_class.R", "R/tab_classes.R", "R/tab.R"]
allowed-tools: Read, Grep, Edit
---

Since 1.4.0 (Phase 5) coloring is decomposed into **three orthogonal axes** feeding **one
vectorised `findInterval` engine**. Governing brief: `dev/new_colors_UI.md`.

- **Axis M — measure**: `diff`, `ratio`, `contrib`, `or` (auto-dispatched by column type: for
  factors `diff` = pp-difference, for numeric means `diff` = standardized Glass's Δ; `ratio` = RR /
  mean-ratio; `contrib` = signed χ² contribution; `or` = empirical odds ratio).
- **Axis C — channel**: `text` colour and `background` fill. Stored in the EXISTING per-column
  `color` attribute, now length ≤ 2: `get_color(x)` = text measure `[1]`, `get_color_bg(x)` = bg
  measure `[2]` (`NA` when absent). Only `diff`/`ratio` may go on the background.
- **Axis S — significance policy**: the per-column `color_signif` attribute
  (`get_color_signif(x)`): `"ignore"` / `"grey_non_signif"` / `"color_all_signif"`.

The old flat strings (`"diff_ci"`, `"after_ci"`, `"ci"`) are soft-deprecated compositions decoded by
`color_measure_policy()` — they still work.

Re-grep exact line numbers before editing; anchors below drift.

## The engine (R/fmt_class.R) — one path, every consumer

`fmt_color_plan(x, channel, color, signif)` → a per-(column,channel) plan
(`measure, policy, score, center, strict, pos_breaks, pos_slots, neg_slots, gate, x2`).
`fmt_color_slots(x, plan)` → `findInterval(mag, pos_breaks)` → palette slot integer (0 = uncolored,
1..10 = grid, 11 = ratio/×2). `fmt_color_channels(x)` → `list(text_slot, bg_slot)`. Significance is
read from the Phase-3a stored bounds `get_ci_inf`/`get_ci_sup` (never re-derived).

- **Add/rename a measure**: `color_measure_policy()` (decode string → measure + policy), then the
  `switch(measure, …)` blocks in `fmt_color_plan()` (scale selection, `raw` per-cell quantity,
  `color_all_signif` floor). Numeric standardized diff divides by `sqrt(get_ref_var(x))`.
- **slot lookup**: `color_slot_table(L, channel)` / `build_slots(K, channel)` map level → palette
  slot (text vs bg families spread intensities differently). No hand-tuned hex-sniff anymore.
- **Every consumer maps `(text_slot, bg_slot)` → colour the same way**: console `pillar_shaft`
  (reference two-channel consumer), `fmt_get_color_code()` (single-channel, golden), the shared
  exporter helper `fmt_channel_codes()` (text hex + bg hex, used by tab_kable/tab_plot/tab_xl), and
  `tab_color_legend()`. Do NOT special-case an exporter — route through these.

## Config (R/tab_classes.R)

- **Breaks** — the canonical option `"tabxplor.color_breaks"` is a named list of 5 scales
  (`pct_diff, pct_ratio, mean_diff, mean_ratio, contrib`), each `list(pos, center, strict, std)`.
  `set_color_breaks(list(...))` (validators in `mk_color_scale()`, defaults in
  `default_color_scales()`, `.onLoad` seed in `utils.R`). `get_color_breaks()` returns the positive
  scales (round-trips); `type = "all"` mirrors. Old `pct_breaks`/`mean_breaks`/`contrib_breaks` args
  are soft-deprecated and mapped inside `set_color_breaks()`.
- **Palette** — `set_color_style()` / `get_color_style(mode, type, theme, html_24_bit)`: 6 named
  11-hex palettes (`pos1..pos5, neg1..neg5, ratio`); index by slot int. (Distinct per-measure hue
  ramps are flag W4 in `dev/new_colors_UI.md`, not yet built — text/bg are the current channels.)

## Arg parsing (R/tab.R)

`normalize_color_spec(color, color_signif)` parses `FALSE`/`TRUE`/scalar/`c(text, bg)`/named into a
spec (+ soft-deprecation of the old strings); `finalize_color_spec()` writes the two-channel +
policy attributes onto the built table. `tab()` and `tab_num()` call these; `tab_many()` does NOT
yet (Phase 6 — see CLAUDE.md roadmap). `color = TRUE` resolves per column type in `finalize_one_col()`.

## Verify

- Temp `.R` file → `Rscript` (never `Rscript -e` on Windows); `devtools::test("d:/Statistiques/
  github/tabxplor")`. Watch `test-color-golden.R` (per-cell hex; factor `diff` must stay
  byte-identical), `test-color-config.R`, `test-color-engine.R`, `test-exports.R`, `test-golden.R`.
- Eyeball a console print + one `tab_kable()`/`tab_xl()`, and check the legend matches the cells
  (esp. numeric `diff` → SD thresholds).
- Intentional output change → regenerate consciously (`dev/make_golden.R`, `dev/make_color_golden.R`,
  CLAUDE.md golden protocol). Document new `@param` values, `devtools::document()`, add a NEWS bullet.
