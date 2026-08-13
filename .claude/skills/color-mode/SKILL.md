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
Phase 19c: the decoding is the declared **`COLOR_ALIASES`** table (alias → `(measure, policy)`), so
each spelling is a row rather than a switch arm and the soft-deprecation list DERIVES from it (the
aliases carrying a policy). ⚠ Order matters at the boundary: **decode first, normalise second** —
`measure_key()` resolves an alias to its MEASURE, so normalising first silently drops the policy half.
Phase 19c also deleted the resolver's own manufactured `"after_ci"`: `tab_resolve_settings()` returns
ONE resolved measure and each step asks `measure_stage()` / `measure_applies()` what it needs, instead
of reading one of four precomputed sub-passes (`color_diff_OR`/`color_ctr`/`color_ci`/`color_num`).

Re-grep exact line numbers before editing; anchors below drift.

## The engine (R/fmt_class.R) — one path, every consumer

`fmt_color_plan(x, channel, color, signif)` → a per-(column,channel) plan
(`measure, policy, score, center, strict, over_breaks, over_slots, under_breaks, under_slots, gate`).
`fmt_color_slots(x, plan)` → per-direction `findInterval(mag, over/under_breaks)` → a palette **slot
integer** (0 = uncolored, **1..4 = over intensities, 5..8 = under**). `fmt_color_channels(x)` →
`list(text_slot, bg_slot)`. Significance is read from the stored bounds `get_ci_inf`/`get_ci_sup`.
There is **no x2/slot-11 override anymore** — the "×2 rule" is just a 1-break `pct_ratio` scale carried
on the background channel (`color = c("diff", "ratio")`, default `pct_ratio = list(over = 2)`).

- **Add/rename a measure: ONE row in `MEASURES` (R/fmt_class.R), and nothing else.** Phase 19c (KEY 4)
  made this literally true by moving the measure's VOCABULARY into the row beside its arithmetic. The
  row's fields, and the ONE accessor that reads each:
  - *engine*: `raw` (getter closure), `scale = c(pct=, std=, log=)` (one key per LADDER — the COLUMN
    says which it reads, via `EST_SCALES$ladder`), `sig_source ∈ {bounds,pvalue}`, `bounds` (optional
    closure; default = the stored ci_inf/ci_sup), `gate_row ∈ {refrow,totrow}`, `scale_from = "gap"`,
    `force_policy`, `guar`, `by_scale`.
  - *vocabulary*: **`channels`** (`text`/`bg` eligibility — the ONE list; `measure_validate()`),
    **`producers`** (`tab`/`reg` — which producer can build it; the "that is a tab_reg measure" hint is
    generated from it), **`applies_to`** (`pct`/`num` — `measure_applies()`), **`builds`**
    (`diff`/`or`/`contrib`: which per-cell fields the pipeline must compute — `measure_builds()`, and
    `measure_stage()` derives from it which step stamps the colour), **`requires`** (named, values
    `"always"`/`"gated"`, keys `ref`/`ci`/`chi2`/`totrow`/`empirical`/`interaction` —
    `measure_forces()`), **`ref_auto`**, **`auto_for`** (`list(text=, bg=)` of context keys `pct` /
    `num` / `counts` / `or_table` / `reg_diff` / `reg_ratio` — `measure_auto()`, THE `color = TRUE`
    resolver for both producers).
  - *legend*: **`word`** (a CLOSURE — `function() gettext("difference")`, so gettext runs at render AND
    potools extracts the literal statically; there is no anchor to keep in sync any more),
    `break_over`/`break_under`/`break_scale`, `ref_kind`, `threshold_mult`, `unit_kind`,
    `has_ref_lead`, **`method`** (`NA` = no interval; a closure = its own test sentence), **`subject`**,
    **`caveat`** (`function(spec)`).
  - A build-time `stopifnot` demands `channels`/`producers`/`applies_to`/`builds` on every row, and a
    second one keeps `COLOR_BUILD_ORDER` exhaustive of the declared `builds` values.
  - The only per-measure CODE left in `fmt_color_plan()` is policy: the diff↔ratio bound rescale and
    the `guaranteed_effect` floor.
- **Break scales are a fact table too** — `COLOR_SCALES` (R/tab_classes.R): `center`/`strict`/`std`/
  `settable`/`default`/`null_default`/`derive`/`legacy`/`alias`. Adding a scale is one row;
  `default_color_scales()`, `mk_color_scale()`'s validation and `set_color_breaks()`/
  `get_color_breaks()`'s name maps all derive from it, and a DERIVED scale (`log_odds`,
  `adj_diff_log`) declares its parent instead of owning a `switch` arm.
- **Never read `MEASURES[[m]]` directly** (Phase 18z4) — go through **`measure_facts(m, policy)`**,
  which folds in a row's optional `guar` list under `guaranteed_effect`. It is the only reason the
  colour plan and the legend describing it cannot diverge; both call it (1 site in `fmt_color_plan`,
  5 in the legend, each passing `plan$policy` / `spec$policy`).
- **A measure whose baseline is ANOTHER COLUMN** (Phase 18z5: `adjustment` = vs the observed/crude
  effect, `between_groups` = vs the first `split_var` group) cannot use the row-reference machinery at
  all -- `fmt_broadcast_last()` groups by runs of `in_refrow` and crosses a split boundary. Its
  counterpart is written into the per-cell **`obs` field at BUILD time** (`reg_build` /
  `reg_write_group_obs`, R/tab_reg.R), and `fmt_adjustment_score()` reads it. Three facts drive the
  rest: `ref_kind = "observed"/"group"` (checked by `measure_own_ref()`, which makes the measure name
  ITSELF in the legend instead of borrowing the column's effect word, and resolves its reference phrase
  PER CHANNEL); `scale_from = "gap"` (the ladder keys off the ESTIMATE's own stored `scale`, since `Model_OR`
  and `Model_AME` are both `type = "row"`); and `force_policy = "ignore"`, applied by
  **`measure_policy(m, policy)`** -- the twin of `measure_facts()`, called by the plan AND the legend,
  for a measure with no significance test of its own. Scales `adj_ratio` / `adj_diff`. The SIGN is
  away-from/toward the NULL, never raw up/down (else a protective effect colours backwards).
- **`contrib` is the one measure that changes reading with the policy.** `ignore`/`grey_non_signif`
  score the relative contribution (`ctr / mean_contrib`, `contrib` scale — the CA reading, relative to
  the table); `guaranteed_effect` scores the ADJUSTED standardized residual (`fmt_resid()`) on the
  absolute 7th scale `zscore`, whose first break is re-anchored to `z(conf_level)` via
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
  env for the build). It NAMES the CI method/level from the COLUMN's own stored **`ci_method`** /
  **`conf_level`** attributes (Phase 19b), through the declared `CI_METHOD_LABELS` table
  (one row per engine, keyed on what the PRODUCER stamped where the interval was computed; `""` = no
  interval -> the legend names none, instead of falling back to a table-wide default it might not have
  used). Shade names ("blue"/"yellow-red") come from `legend_shade_names()`
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
- **A palette is hex AND face** (Phase 18z11). `mode = "face"` returns `tx_palette_faces()`'s row for
  that (family, theme): `list(bold, italic, underline, semantic)`, 8 logicals each. The colour palettes
  answer bold-on-all-8-text-slots / nothing-on-bg — today's rendering, as data — which is what makes
  `tx_css_render()`'s static `.p1..m4{font-weight:bold}` rule THE CSS baseline that `tx_face_decls()`
  diffs each theme against. Never re-derive "is this bold" from "does this have a hex": five sites used
  to, and all five broke on the third theme **`"print"`** (alias `"bw"`, `tx_resolve_theme()`), the
  black-and-white publication palette — every text slot `#000000`, over = bold / under = italic /
  2nd level underlined, one grey fill ramp identical on both sides (greyscale cannot diverge, so the
  fill carries magnitude and the typography carries direction). Consumers read the face through
  `fmt_channel_codes()`'s `text_face`/`bg_face` -> `ann$face_bold`/`_italic`/`_underline`.
  `semantic = TRUE` also emits it as `<b>`/`<i>`/`<u>` markup (GitHub and a Word paste keep tags, not
  classes). Adding a theme = one `default_*_palette()` literal + one `tx_palette_faces()` row + one
  `tx_chrome_hex()` arm + `tx_resolve_theme()`; the ENGINE must stay theme-blind (no `pmin` on slots --
  a palette expresses a lower resolution by REPEATING a face, and `legend_break_tokens()` collapses
  break-words that render identically). See `dev/black_and_white_publication_palette.md`.

## Arg parsing (R/tab.R)

`normalize_color_spec(color, color_signif)` → a spec `list(mode, legacy, text, bg, types, signif)`.
`mode` ∈ `off`/`auto`(TRUE)/`flat`(positional)/`by_type` (named / `list(pct=, mean=)`). `legacy` is the
single string fed to the pipeline for ci/chi2 side-effects. `finalize_color_spec()` →
`resolve_col_measures()` writes the two-channel + policy attributes per column. `tab()`/`tab_num()`/
`tab_many()` call these + set the `color_breaks` attribute last.

## Verify

- Temp `.R` file → `OMP_NUM_THREADS=1 Rscript <file>.R` with `Sys.setenv(TESTTHAT_CPUS="8",
  NOT_CRAN="true")` so the snapshot tests run; `devtools::test("~/github/tabxplor", filter = ...)`.
  Watch `test-color-golden.R` (per-cell hex), `test-color-config.R`, `test-color-engine.R`,
  `test-color-legend.R`, `test-render-html.R`, `test-golden.R`, `test-tab_md.R`.
- **`dev/verify_color_attrs.R`** is the characterization net for anything touching the RESOLVER: it
  builds ~290 tables over the colour × policy × pct × ci × OR argument space and dumps every stored
  colour fact plus the resolved slot vectors. `Rscript dev/verify_color_attrs.R save <f.rds>` before,
  `check <f.rds>` after — "IDENTICAL" is the gate. It exists because `color_ctr`/`color_ci`/
  `color_num` were asserted by NO test at all.
- Eyeball a console print + one `tab_kable()`/`tab_md()`, check the legend matches the cells (esp.
  numeric `diff` → SD thresholds; over-only ratio → no under side).
- Intentional output change → regenerate consciously (`dev/make_color_golden.R`;
  `snapshot_accept("render-html")`/`("golden")`). Document `@param` values, `devtools::document()`,
  add a NEWS bullet.
