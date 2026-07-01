---
name: color-mode
description: Add, change, or split a color mode of tabxplor tables (the `color=` argument, e.g. "diff", "ratio", "diff_ci", "after_ci", "contrib", "ci", "OR"). Use whenever touching the color selection logic, the per-mode boolean formulas, the legend text, color breaks, or how factors vs numerics are colored.
paths: ["R/fmt_class.R", "R/tab_classes.R"]
allowed-tools: Read, Grep, Edit
---

A color mode is the value of the per-column `color` attribute of `tabxplor_fmt` (one of
`"diff"`, `"diff_ci"`, `"after_ci"`, `"contrib"`, `"ci"`, `"OR"`/`"or"`, or `""`/`"no"`).
It decides which field(s) drive the cell color and against which thresholds ("breaks").

The pipeline for one column: `set_color()` stores the mode → `fmt_color_selection()` picks the
break level per cell → `color_formula()` is the boolean mask for one break level →
`keep_last_break()` keeps the strongest matching level → `get_color_style()` maps level → hex/crayon.
Every exporter (console, tab_kable, tab_plot, tab_xl, tab_md) routes color through this same path,
so color is the one thing that is already unified — do NOT special-case an exporter.

Re-grep exact line numbers before editing; anchors below are approximate.

## Key locations (R/fmt_class.R unless noted)

1. `set_color()` validation (~L895-897): the `stopifnot(color %in% c(...))` whitelist. Add/rename the
   mode string here first, and map `"no"`→`""`.
2. **factor-vs-numeric divergence** — two places compute a `pct_diff`/`pct_ratio` flag that gates
   behaviour on column `type`:
   - `~L929-934` and `~L1760-1763`: `pct_diff <- color %in% c("diff","diff_ci","after_ci") & !type %in% c("n","mean")`.
   - `fmt_color_selection()` `~L1949` (`negative_breaks`) and `~L1956` (`pct_ratio <- !type %in% c("mean","n") & brk > 1`).
   This `brk > 1` / `type` gate is the current "×2 ratio" rule. **The 1.4.0 diff/ratio split lives here**:
   factors keep today's `"diff"` behaviour; numerics need `"diff"`=difference vs `"ratio"`=old ratio,
   maybe `"diff_ratio"`=text color for one + background for the other.
3. `fmt_color_selection()` (~L1894): reads the driving fields per mode — `diff` (~L1974), `ratio`
   (~L1981, = `mean` field), `ci` (~L1987), `ref_means_pct` (~L1993, via `get_ref_means()`/`get_ref_pct()`),
   `or` (~L2021), `ctr` (contrib). Add your mode's branch here to select the field it colors on.
4. `color_formula()` (~L2165): the boolean mask per mode. Add a branch matching your mode; mind the
   `neg` (direction) and `pct_ratio` (ratio vs difference) arguments.
5. `keep_last_break()` (~L2097): usually no change unless your mode needs different tie-breaking.
6. **Legend text** — `color_formula_chr()` (~L2339) and its callers (~L2527) build the human-readable
   legend/subtext. A new mode needs a legend branch or it prints wrong/empty.

## Breaks & palette (R/tab_classes.R)

7. `set_color_breaks()` (~L3229) / `get_color_breaks()` (~L3405): thresholds live in
   `options("tabxplor.color_breaks")`. Default pct `c(0.05,0.1,0.2,2,0.3)` — any value `> 1` triggers
   ratio comparison; negatives are auto-mirrored. Mean breaks `c(1.15,1.5,2,4)` are always ratios.
   If your mode needs its own break vector (like contrib/mean), add it here and read it in `fmt_color_selection()`.
8. `set_color_style()` (~L3086) / `get_color_style()` (~L3127): 6 named 11-hex palettes
   (`pos1..pos5`, `neg1..neg5`, `ratio`) — comment map ~L2909. Only touch if a mode needs a distinct
   color ramp (e.g. `"diff_ratio"` wanting a background ramp: it must select a background style so text
   stays readable when text+background encode two numbers at once).

## Attribute plumbing (if the mode is a genuinely new attribute value, not new logic)

The mode is stored in the existing `color` attribute, so no `new_fmt()` change is normally needed.
If you add a whole new attribute (e.g. a second color channel for `"diff_ratio"`), follow `/vctrs-field`.

## Verify

- `source("tests/testthat.R", encoding = "UTF-8")` — especially `test-tab.R` (the `expect_color()`
  helper), `test-golden.R` (per-mode color goldens — the factor `"diff"` output must stay byte-identical),
  and `test-exports.R`. Regenerate goldens ONLY if the change to a mode is intentional (see CLAUDE.md
  golden regeneration protocol).
- Eyeball a real table in the console for each affected mode, plus one `tab_kable()` / `tab_xl()` export.
- If you added a public mode string, document it in the `color` `@param` of `tab()`/`tab_many()` and
  `set_color()`, then `devtools::document()`. Add a NEWS.md bullet.
