# tabxplor color & breaks framework — design (Phase 5)

> STATUS: design only. No code has changed. This is the working brief for the Phase 5
> implementation ("color diff/ratio split" in the 2.0.0 roadmap), to be executed in a later
> session. It supersedes the terse Phase 5 bullets in `CLAUDE.md` and expands
> `dev/tabxplor_2.0.0_decisions.md` §3/§7/§12/§18/§20. Read this first, then those.
>
> Governing rule (from CLAUDE.md): public API stays retro-compatible; internals may be redesigned
> radically. The maintainer confirmed nobody but him has customized colors/breaks, so the config
> API may change *if justified* — but `get_color`/`set_color`, `fmt_get_color_code`, the
> `$diff`/`$ratio` fields, and old `set_color_breaks()` args must keep working (soft-deprecation).
>
> How to use this doc: sections 2-3 are the diagnosis (current system + statistical audit);
> section 4 states the UI problem; section 5 is the proposed framework (the core deliverable);
> sections 6-11 are the technical plan, phasing, benchmarks, open decisions, and file map.

## 1. Why this refactor exists

Three forces converge on the color subsystem:

1. **Performance.** Color is the #1 cost of console printing and `tab_kable()`. The engine builds
   one boolean vector per break then resolves them per cell with a `purrr::reduce` +
   `dplyr::mutate` + matrix transpose (`keep_last_break`), run twice per column, once per colored
   column, on every render, with no memoization. It is O(cells) heavyweight-dplyr where an O(cells)
   C-level `findInterval` would do.

2. **The diff/ratio split + native two-channel color.** Phase 2 already made `diff` a real
   difference and added a `ratio` field. Phase 5 must give each colorable quantity its own explicit
   breaks, make **two-channel coloring native** (one quantity in text color, another in background),
   keep the factor default byte-identical, and make the numeric default `ratio`.

3. **Statistical soundness.** The CI-gated color formulas (`diff_ci`/`ci`/`after_ci`) carry code
   FIXMEs flagging their percentage branches as unverified/wrong. Phase 3a already stores sound,
   real CI bounds and a per-cell p-value; the color modes should consume those instead of
   recomputing significance with ad-hoc algebra.

But the hardest part is neither performance nor statistics — it is the **UI** (section 5): how to
expose diff/ratio/significance/contribution/odds-ratio coloring, single vs two-channel, with
sensible per-type defaults, WITHOUT an explosion of arguments or cryptic strings.

## 2. Current state — full findings

### 2.1 The 3-layer system

- **Palettes** (`R/tab_classes.R:2906-3058`): 6 named character vectors (text dark/light, text
  light 24-bit blue-red / green-red, background light/dark), each **11 hex slots**:
  `pos1..pos5` (over-represented), `neg1..neg5` (under-represented), `ratio` (a single color for the
  "×2 rule"). Hand-tuned so intensities are eye-distinguishable.
- **Breaks** (`options("tabxplor.color_breaks")`, set by `set_color_breaks()`
  `R/tab_classes.R:3228`): a list `pct_breaks, pct_ci_breaks, mean_breaks, mean_ci_breaks,
  contrib_breaks`. Defaults: pct `c(0.05,0.1,0.2,2,0.3)`, mean `c(1.15,1.5,2,4)`, contrib
  `c(1,2,5,10)`. Positive-only input; negatives auto-mirrored (`c(.,-.)` additive, `c(.,1/.)`
  ratio). The **"×2 rule"** is encoded as a magic value `>1` interleaved inside `pct_breaks` (the
  `2`), flagged PROVISIONAL in the code.
- **Selection** (`R/fmt_class.R`): `fmt_color_selection()` -> `color_formula()` per break ->
  `keep_last_break()` -> per-cell strongest break; then `select_in_color_style()` maps the break
  count to palette slots; `get_color_style()` returns crayon fns (console) or hex (exporters).

### 2.2 The computation pipeline and the bottleneck

`fmt_color_selection(x)` (`R/fmt_class.R:2053-2240`) returns a **named list of boolean vectors**,
one per signed break. `keep_last_break()` (`R/fmt_class.R:2273-2322`) is the hotspot: a
`purrr::reduce` over EVERY cell doing a `dplyr::mutate` + `map_lgl`, then `arrange` +
`across(detect_index)` + `as.matrix |> t()`. Called twice per column (positive/negative halves),
once per colored column, across four render paths (`pillar_shaft` `:1902`, `fmt_get_color_code`
`:982`, `tab_kable` `tab_classes.R:585`, `tab_plot` `tab_classes.R:1333`), with no caching.
`select_in_color_style()` (`R/fmt_class.R:2805-2861`) additionally picks its slot-index family by a
FRAGILE hex-sniff `str_detect(pos1_hex, "#CCFFCC|#000033e")` (note the typo `e`).

### 2.3 Field semantics actually written (per column type)

| field (getter)        | pct/factor column                                                            | numeric/mean column                                   |
|-----------------------|------------------------------------------------------------------------------|-------------------------------------------------------|
| `diff` (`get_diff`)   | real pp difference `pct_cell - pct_ref`                                      | real difference `mean_cell - mean_ref` (Phase 2 flip) |
| `ratio` (`get_ratio`) | relative risk `pct_cell/pct_ref` (written only when OR requested — see note) | `mean_cell/mean_ref`                                  |
| `mean` (`get_mean`)   | OVERLOADED: `pct_cell/pct_ref` ratio driving the ×2 rule (`tabs_mean`)       | the actual mean                                       |
| `ci_inf`/`ci_sup`     | real absolute asymmetric bounds (Newcombe/Wilson/AC)                         | absolute bounds (Welch-t/z/pivot), symmetric          |
| `pvalue`              | CI-inversion p of the displayed interval; NA for cell CIs / stars off        | same                                                  |
| `var`                 | (contrib pass only)                                                          | weighted variance of the cell mean                    |
| `ctr`                 | signed relative chi2 contribution; on total rows = `1/n_cells`               | —                                                     |
| `or` (`get_or`)       | empirical odds ratio                                                         | —                                                     |

Writer facts (live `tab_plain`/`tab_num` fast path, `R/tab.R:2432-2549`, assembled at `:2654-2694`):
`diff=..4` (real difference), `mean=..5` (`tabs_mean`, the pct ratio overload for the ×2 rule,
referenced to the reference ROW per `ref`), `ratio=..6` (`tabs_rr`, cell / reference-COLUMN pct,
the leftover renamed-from-`rr`). **So today the ×2 color rule reads the `mean` overload, and the
`ratio` field holds a different (column-referenced) ratio that color never uses.** Phase 5 must
repoint `ratio` to the reference-relative ratio the color engine uses (cell/ref per `ref`, both pct
and numeric) and set `mean = NA` for pct.

### 2.4 Config surface (every knob)

- Options (seeded in `.onLoad`, `R/utils.R:49-55`): `tabxplor.color_style_type` ("text"/"bg"),
  `tabxplor.color_style_theme` ("light"/"dark"), `tabxplor.color_html_24_bit`
  ("blue_red"/"green_red"/"no"), `tabxplor.color_style` (custom palette or NULL),
  `tabxplor.color_breaks` (the list).
- Setters: `set_color_breaks(pct_breaks, mean_breaks, contrib_breaks)` `R/tab_classes.R:3228`;
  `set_color_style(type, theme, html_24_bit, custom_palette)` `R/tab_classes.R:3082`.
- Getters: `get_color_breaks()` `R/tab_classes.R:3399`, `get_color_style()`
  `R/tab_classes.R:3126`, internal `brk_from_color()` `R/fmt_class.R:2753`.
- Per-column color mode: scalar attribute `color` via `set_color`/`get_color`
  (`R/fmt_class.R:908-948`), whitelist `c("diff","diff_ci","after_ci","contrib","ci","OR","",NA)`.

### 2.5 Consumers (color is already unified — do NOT special-case an exporter)

`pillar_shaft.tabxplor_fmt` (console, crayon), `fmt_get_color_code` (exported per-cell hex),
`tab_kable` (`kableExtra::cell_spec(color=/background=)`), `tab_plot` (ggplot),
`tab_xl` (openxlsx `fontColour`/`fgFill` via `colorToStyle`, `addStyle(stack=TRUE)`), and the legend
(`tab_color_legend`). `tab_md` is monochrome. All route through
`fmt_color_selection -> select_in_color_style -> get_color_style`.

### 2.6 Known bugs / FIXMEs to fix in this refactor

- `set_color_style(custom_palette=)` length guard requires 10 but applies 11 names -> the `ratio`
  slot is dropped; custom palettes broken for the ratio color (`R/tab_classes.R:3102`). Fix: accept
  11 (and 21 for a full ratio ramp).
- The `select_in_color_style` hex-sniff `"#CCFFCC|#000033e"` (typo, fragile). Replace by passing the
  known channel `type` explicitly.
- The `ci` mode's pct break-indexing is broken: the unmirrored `2` makes `pct_ci_breaks` odd-length,
  so the negative direction is never selected for percentages (see 3.2, `ci`).
- The CI-gated modes use the CI **upper arm** as if symmetric — wrong for the default asymmetric
  proportion intervals (Wilson/Newcombe). See section 3.
- Dead code: the whole `*_brksup` / `pct_ratio_breaks` machinery in `set_color_breaks`,
  `fmt_color_selection`, `tab_xl` (former upper-break design) — delete.

## 3. Statistical audit — per-mode diagnosis and corrected formulas

### 3.1 The one significance primitive

Phase 3a made significance = **universal CI-inclusion** (§20): the stored `ci_inf`/`ci_sup` are real
absolute bounds and, for difference intervals, the p-value is their inversion. So per-cell
significance is exactly, for any method, pct or mean:

```r
sig_pos <- get_ci_inf(x) > 0     # whole difference interval above the reference
sig_neg <- get_ci_sup(x) < 0     # whole difference interval below the reference
```

Use the **bounds**, not `pvalue`, for the color gate: bounds always exist (p-value is NA when stars
are off), give direction for free, and can never contradict the printed `[inf;sup]` bracket or the
stars. This single primitive replaces all the ad-hoc `abs(diff) - ci > 0` algebra and makes every
CI-gated mode sound by construction and identical across pct and mean.

### 3.2 Per-mode verdicts and corrected formulas

`q` = the mode's intensity quantity per type: pct pp-difference `get_diff`; pct ratio `get_ratio`;
numeric Glass's delta `get_diff/sd_ref` (`sd_ref = sqrt(reference var)`, via a new `get_ref_var()`
mirroring `get_ref_means`, `R/fmt_class.R:1280`); numeric mean-ratio `get_ratio`.

| mode          | current status                                            | corrected formula (pos / neg)                                                                              | byte-identity                 |
|---------------|-----------------------------------------------------------|------------------------------------------------------------------------------------------------------------|-------------------------------|
| `diff` (pct)  | correct, golden-locked                                    | `q>brk` / `q<brk` (q=diff; ×2 uses ratio at the >1 break)                                                  | UNCHANGED                     |
| `diff` (mean) | ratio-coloring via D3 shim                                | Glass's delta `get_diff/sd_ref` vs `mean_diff_breaks`                                                      | CHANGES (§18)                 |
| `ratio` (new) | absent (falls through to FALSE)                           | `get_ratio>brk` / `<brk` (reciprocal breaks)                                                               | new mode                      |
| `diff_ci`     | pct branch WRONG (upper-arm asymmetry)                    | `q>brk & sig_pos` / `q<brk & sig_neg`                                                                      | pct CHANGES, mean same        |
| `ci`          | pct doubly broken (asymmetry + odd-length neg suppressed) | `sig_pos` / `sig_neg` (drop break vector)                                                                  | pct CHANGES, mean same        |
| `after_ci`    | pct wrong arm; mean not a standard measure                | grade the near-zero bound: `ci_inf>brk` / `ci_sup<brk` (/sd_ref for means)                                 | pct CHANGES, mean same-if-raw |
| `contrib`     | sound descriptive map                                     | `ctr >= brk*mean_ctr` / `<=` (unchanged); optional standardized Pearson residual for per-cell significance | UNCHANGED                     |
| `OR`          | correct (log-symmetric)                                   | `or>brk` / `or<brk` (reciprocal breaks)                                                                    | UNCHANGED                     |

Worked example of the pct `diff_ci` bug: Newcombe CI `[0.002, 0.13]`, `diff=0.05` -> interval
excludes 0 (significant), but `abs(diff) - (ci_sup-diff) = 0.05 - 0.08 = -0.03 <= 0` -> NOT colored.
The large upper arm of an asymmetric interval spuriously kills the color. `sig_pos = ci_inf>0` fixes
it. For symmetric intervals (means, Wald/AC pct) the corrected forms reduce EXACTLY to today's, so
means stay byte-identical.

Additional notes from the audit:

- The mean `diff_ci`/`ci`/`after_ci` formulas are algebraically OK TODAY only because `diff` still
  secretly holds a ratio there and the mean CI is symmetric; they are landmines that break the
  moment `diff` becomes a real difference. The corrected `sig_*` primitive removes the landmine.
- `after_ci` should grade the **interval bound nearest zero** (the honest "at least this big beyond
  noise"): `get_ci_inf(x) > brk` (pos) / `get_ci_sup(x) < brk` (neg), sd-standardized for means.
  For symmetric intervals this reduces exactly to today's `abs(diff) - ci > brk`.
- `contrib` is a sound descriptive share-of-chi2 map, not a significance test. Optionally expose the
  standardized Pearson residual `(o-e)/sqrt(e*(1-rowsum/N)*(1-colsum/N))` colored at `|r|>1.96` as a
  genuine per-cell significance (margins already computed by `agg_chi2`). See open decision D5.
- `OR` coloring is correct on the log-symmetric scale; `1/OR` display and OR significance (log-OR
  Wald with Haldane-Anscombe correction, unweighted 2x2) are Phase 7/9 concerns, not color bugs.

### 3.3 The "×2 ratio" rule

The concept is statistically sound: a +3pp shift is trivial off 40% but a doubling off 3%; a ratio
threshold catches small-base relative shifts an additive rule misses. But the current encoding is
not: it is asymmetric (only a `>1` "×2", no "/2"), it corrupts the break vector (the lone `2` breaks
the `ci` mode's negative direction), and it rides the overloaded `mean` field. Fix: an explicit,
reciprocally-mirrored `ratio_breaks` on the `ratio` field, usable as a full ramp, off the overload.

## 4. The UI problem — the "Borgesian catalogue"

### 4.1 Statement

Coloring must serve many combinations, and the current `color = <one string>` API cannot express
them without either per-column hand-tuning or an explosion of arguments/strings:

- Column TYPE dictates the sensible quantity: factor/pct -> difference (+×N ratio highlight);
  numeric/mean -> ratio; odds-ratio columns -> OR; counts -> none.
- `color = "diff"` today literally colors the `diff` field, so it produces the WRONG thing for a
  mean column (a difference, not the expected ratio).
- The maintainer wants, for factors: (default) 8 difference colors + 1 ×N ratio color, all text;
  and (opt-in, no expert tinkering) 8 difference text colors + 8 ratio background colors.
- Naive fixes all fail: vectorizing `color` over col_vars is not user-friendly; duplicating args
  (`color`, `color_mean`, `color_n`, `color_or`, ...) is an infinite catalogue; a single mega-string
  (`"diff_pct_and_ratio_mean"`) is unreadable; `color = TRUE` is friendly but uninformative about
  what the colors mean; a preset name like `"extended"` is short but cryptic.
- Coloring must be OPT-IN (off by default), yet the opt-in must stay readable.

### 4.2 Constraints and levels

- **C1 Opt-in:** default no color.
- **C2 Per-type dispatch:** one user choice must produce the right quantity for factor, numeric,
  and OR columns simultaneously.
- **C3 Two channels:** text + background, natively, for showing two quantities at once.
- **C4 Retro-compat:** scalar `color=` values keep working; `get_color`/`set_color` scalar contract
  holds.
- **C5 Two customization tiers:** basic (defaults / one word) and expert (full control of breaks +
  palette per case), without bloating `tab()`.
- **C6 Render-time & medium-aware:** the palette must be resolved per output medium at render time
  (the same table object renders to console/Excel/HTML/plot); therefore palette AND breaks live in
  **global options**, not baked into `tab()`. (Maintainer's decision: global only, no per-table
  `breaks=` argument — changing what colors mean per table is undesirable, and one global config is
  simpler and safer.)
- **C7 Statistical soundness:** significance-based coloring must match the Phase 3a CI/stars.

### 4.3 What is / isn't user-friendly

- Friendly: one readable word that "does the right thing" per column; `color = TRUE` as an on-switch;
  two channels as `c(text, bg)`; global `set_color_breaks()`/`set_color_style()` set once per report.
- Unfriendly: per-column vectors; a family of `color_*` arguments; cryptic composite strings;
  needing to know field internals.

## 5. Proposed framework 1 — first failed attempt at a solution

### 5.1 The key idea: separate INTENT from RENDERING, dispatch intent by column type

- **`color =`** answers "**what deviation do I want to see, and in which channel (text/background)?**"
  It is a small, closed vocabulary of *intents*. Each intent auto-dispatches to the quantity
  appropriate for each column's type. It does NOT name a field or a per-type mode.
- **breaks** answer "**how big is big?**" — per-quantity thresholds (global option).
- **palette/style** answers "**which hues, for which medium?**" — global, render-time.

This dissolves the catalogue: "show me the difference" (`color = "diff"`) means pp-difference for a
factor and a standardized (Glass's delta) difference for a mean — the user writes it once. No
`color_mean`/`color_or`, no mega-strings; significance stays inside the same small vocabulary.

### 5.2 The `color=` vocabulary and two channels

Scalar, or length-2 `c(text, background)`:

| value             | meaning (auto-dispatched per column type)                                                                |
|-------------------|----------------------------------------------------------------------------------------------------------|
| `FALSE` (default) | no color                                                                                                 |
| `TRUE` / `"auto"` | default scheme: factor -> `diff` (+×N highlight), numeric -> `ratio`, OR request -> `OR`, counts -> none |
| `"diff"`          | deviation as a difference: factor -> pp-diff (+×N highlight), numeric -> Glass's delta                   |
| `"ratio"`         | deviation as a ratio: factor -> relative risk, numeric -> mean ratio                                     |
| `"ci"`            | binary significant vs not (per type)                                                                     |
| `"diff_ci"`       | graded difference, gated to significant cells                                                            |
| `"after_ci"`      | graded by the CI-discounted (guaranteed-minimum) effect                                                  |
| `"contrib"`       | chi2 cell contributions (factors)                                                                        |
| `"OR"`            | odds ratios (factors)                                                                                    |

Two channels: `color = c("diff", "ratio")` -> difference in TEXT, ratio in BACKGROUND. A scalar =
text only (today's behavior). This replaces the mooted `"diff_ratio"` string. Because each channel routes to its type-appropriate
palette (text hues vs pale background fills), text-on-background stays readable automatically.

Composition rule (keeps it simple): at most one magnitude ramp (`diff`/`ratio`) per channel;
`contrib`/`OR` are single-channel (text) only; `diff_ci`/`after_ci`/`ci` occupy the text channel.
Invalid pairs error with a clear `cli` message. (Open decision D1: whether `ci` may act as a
background "significance shade" over a magnitude text ramp.)

### 5.3 Three tiers of customization

- **Basic:** `color = TRUE` (or one intent word). Tested default breaks + palette. Nothing else.
- **Intermediate:** pick intent(s), including two-channel `c("diff","ratio")`; optionally call
  `set_color_breaks()` / `set_color_style()` once at the top of a report to tune globally.
- **Expert:** fully customize per-quantity breaks and per-case palettes globally (including the
  ratio ramp and a custom palette); optionally register a named preset.

### 5.4 Defaults per column type (only for `color = TRUE`/`"auto"`)

Resolved once at build time (as today's `case_when`, `R/tab.R:2677-2688`), so the stored per-column
attribute is a concrete intent: factor -> `"diff"`; numeric -> `"ratio"`; OR request -> `"OR"`;
counts -> `"contrib"`. For an explicit intent word, the SAME word is stored on every colorable column and
the engine dispatches the quantity by `type` at render time. Note: the `color="auto"` resolver
(`R/tab.R:820-843`) needs a `type=="mean"` branch to yield `"ratio"` (today it yields `"diff"`).

### 5.5 Breaks model (per-quantity, global)

`options("tabxplor.color_breaks")` becomes a per-quantity list; each entry stores the user
positive-only vector plus a precomputed signed-sorted vector for `findInterval`:

- `diff_breaks` — pct pp-difference (default `c(0.05,0.1,0.2,0.3)`).
- `ratio_breaks` — pct/factor relative risk; the ×N rule (default `c(2)`; extend for a full ramp).
- `mean_diff_breaks` — NEW, numeric Glass's delta (default `c(0.2,0.5,0.8,1.2)`).
- `mean_ratio_breaks` — numeric mean ratio (default `c(1.15,1.5,2,4)`; = today's `mean_breaks`).
- `contrib_breaks` — default `c(1,2,5,10)`.
- CI-derived variants (`diff_ci_breaks`, `mean_ratio_ci_breaks`) derived internally as today
  (subtract first break / divide by first break); may shrink after the formula redesign since the
  significance GATE no longer needs a CI break vector (`ci` mode drops it entirely).

Decision: split `pct` ratio breaks vs `mean` ratio breaks (do NOT share) — different natural scales
and defaults; `color="ratio"` reads `ratio_breaks` for pct/factor columns and `mean_ratio_breaks`
for numeric columns (dispatch on `type`).

`set_color_breaks()` new signature (positive-only; mirrored automatically):

```r
set_color_breaks(
  diff        = c(0.05, 0.1, 0.2, 0.3),   # pct pp-difference
  ratio       = c(2),                      # pct/factor relative risk (extend for a full ramp)
  mean_diff   = c(0.2, 0.5, 0.8, 1.2),     # numeric Glass's delta
  mean_ratio  = c(1.15, 1.5, 2, 4),        # numeric mean ratio
  contrib     = c(1, 2, 5, 10)
)
```

Old-arg shim (reproduces today EXACTLY): `pct_breaks` -> `ratio <- pct_breaks[pct_breaks>1]`,
`diff <- pct_breaks[pct_breaks<=1]`; `mean_breaks` -> `mean_ratio`; both via
`lifecycle::deprecate_soft("2.0.0", ...)`. `get_color_breaks()` migrates an old-shape option on read.

### 5.6 Palette model (diff ramp + ratio ramp; text/bg)

Extend each of the 6 palettes so a channel can draw either a **diff +/-ramp** (`pos1..pos5`,
`neg1..neg5` — the existing slots) or a **ratio +/-ramp** (`rpos1..rpos5`, `rneg1..rneg5` — new).
Keep the current single `ratio` slot as the default-ratio alias so that with the default single
`ratio_breaks=c(2)` the output is byte-identical (the ratio ramp is only used when `ratio_breaks`
has several values). Default ratio-ramp hues can be derived programmatically (lightness ramp) from
the single `ratio` hue until the maintainer supplies hand-tuned ones (mark experimental). Each
channel picks its palette by its own type: text channel -> a text palette; background channel -> a
background palette. `set_color_style(custom_palette=)` accepts 11 (bug fix) or 21 (with ratio ramp).

### 5.7 Presets (rendering only — optional expert convenience)

`set_color_scheme(name_or_list)` bundles a breaks + palette configuration. Ship `"default"` (today's
look) and `"extended"` (full diff + full ratio ramps). Experts register their own by passing a
structured list. Crucially, preset names describe the LOOK/granularity, not the intent — the intent
is always the readable `color=` word — so they need not encode "diff_pct_and_ratio_mean". This is
secondary sugar over the intent/breaks/palette separation, not load-bearing (see open decision D6).

### 5.8 Worked examples

```r
# Basic — just color it well (per-type defaults):
tab(d, region, sex, color = TRUE)          # factors: diff + xN ; numerics: ratio

# One readable intent, dispatched per type:
tab(d, region, income, color = "diff")     # factor: pp-diff ; numeric: Glass's delta
tab(d, region, income, color = "ratio")    # factor: relative risk ; numeric: mean ratio

# Only significant deviations, graded:
tab(d, region, sex, color = "diff_ci")

# Two channels (the "8 diff + 8 ratio" request), native:
tab(d, region, sex, color = c("diff", "ratio"))       # diff in text, ratio in background
set_color_breaks(ratio = c(1.5, 2, 3))                # a full ratio ramp (global)

# Expert global tuning (once per report):
set_color_breaks(diff = c(0.03, 0.07, 0.15))
set_color_style(type = "bg", html_24_bit = "blue_red")
```

## 6. Technical architecture

### 6.1 Storage — two scalar attributes

Add a second scalar per-column attribute `color_bg` (the background channel); keep `color` as the
scalar TEXT channel. Rejected: a length-2 vector in one `color` attribute — it breaks the scalar
`==`/`if_else` assumption in `vec_ptype2.tabxplor_fmt` (`R/fmt_class.R:3107-3119`) and the ~15
`if (color == ...)` consumer sites. Two attributes keep `get_color`/`set_color`/`$` byte-identical
for the single-channel case and cost only the standard `/vctrs-field` "add an attribute" plumbing:
`new_fmt` (`:1096`), `vec_ptype2` (`:3087`), `vec_cast` tab/double/integer (`:3177/3221/3258`),
`vec_arith` both branches (`:3403`), plus `get_color_bg`/`set_color_bg` accessors. `set_color` gains
an optional `color_bg` and accepts a length-2 `color`; a `"a|b"` token encoding lets the length-2
value survive the per-row_var `vec_recycle` (`R/tab.R:741`) until Phase 6 globalizes `color`.

### 6.2 The vectorized engine (findInterval)

Replace `fmt_color_selection` + `color_formula` + `keep_last_break` + `select_in_color_style` with:

- `fmt_color_plan(x, channel)` -> a per-column plan: the mode, the per-cell `score` (the mode's
  quantity), a per-cell `gate` (from `sig_pos`/`sig_neg` for CI modes; `!is_totrow` for contrib), the
  sorted signed `breaks`, and a precomputed `slot_map` (level -> palette slot, replacing
  `select_in_color_style`, choosing the family by the explicit channel type — no hex-sniff).
- `fmt_color_slots(x, plan)` -> per-cell integer slot via
  `slot <- slot_map[findInterval(score, breaks) + 1L]`, then `slot[!gate] <- 0`. No per-cell loop,
  no reduce, no transpose. The factor-`diff` interleaved ×N precedence is reproduced with an
  explicit `pmax` over the diff level and the ratio-break flag (preserving byte-identity, including
  the quirk that a diff above the top break can override the ×N highlight — open decision D2).
- `fmt_color_channels(x)` -> `list(text_slot, bg_slot)`, each `integer(length(x))` (0 = uncolored).
  This is the single artifact every consumer maps to crayon/hex.

The per-mode spec that drives the engine (score, break_key, gate, center):

| mode       | type | score                                | break_key                       | gate                | center |
|------------|------|--------------------------------------|---------------------------------|---------------------|--------|
| `diff`     | pct  | `get_diff` (+ ratio at the >N break) | `diff_breaks` (+`ratio_breaks`) | —                   | 0      |
| `diff`     | mean | `get_diff/sd_ref` (Glass)            | `mean_diff_breaks`              | —                   | 0      |
| `ratio`    | pct  | `get_ratio`                          | `ratio_breaks`                  | —                   | 1      |
| `ratio`    | mean | `get_ratio`                          | `mean_ratio_breaks`             | —                   | 1      |
| `diff_ci`  | any  | mode-`diff` score                    | as `diff`                       | `sig_pos`/`sig_neg` | 0      |
| `ci`       | any  | —                                    | (none)                          | `sig_pos`/`sig_neg` | —      |
| `after_ci` | pct  | `ci_inf` (pos)/`ci_sup` (neg)        | `diff_ci_breaks`                | —                   | 0      |
| `after_ci` | mean | same /sd_ref                         | `mean_diff_breaks`              | —                   | 0      |
| `contrib`  | any  | `ctr/mean_ctr`                       | `contrib_breaks`                | `!is_totrow`        | 0      |
| `OR`       | any  | `get_or`                             | `mean_ratio_breaks`             | —                   | 1      |

### 6.3 Consumer changes

Each consumer swaps `fmt_color_selection + select_in_color_style + get_color_style[...]` for
`fmt_color_channels()` and applies both channels: text via crayon / `cell_spec(color=)` / openxlsx
`fontColour`; background via crayon bg / `cell_spec(background=)` / openxlsx `fgFill` (stacked
`addStyle`). `pillar_shaft`, `fmt_get_color_code` (+ a bg companion), `tab_kable`, `tab_plot`,
`tab_xl`, and the legend (`color_formula_chr`/`brk_from_color` gain `ratio`/`mean_diff` branches and
a two-channel rendering). `fmt_color_selection` is kept as a thin deprecated shim while
`expect_color()` migrates. Then delete `keep_last_break`, `color_formula`, `select_in_color_style`,
and the dead `*_brksup` code.

### 6.4 Retro-compat surface (must keep working)

`get_color`/`set_color` (scalar text), `fmt_get_color_code`, `$diff`/`$ratio`, `get_color_style`/
`set_color_style` (+ length-11 fix), `set_color_breaks(pct_breaks, mean_breaks, contrib_breaks)`
(shim), `get_color_breaks`, and all scalar `color=` values on `tab()`/`tab_many()`/`tab_num()`/
`tab_pct()`. New: `color = "ratio"`, `color = c(text, bg)`, `"diff_ratio"` alias.

## 7. col% + means reference question (analysis)

Factor columns color against a reference COLUMN; mean columns against a reference ROW
(`get_ref_means` is row-only, `R/fmt_class.R:1280`). Under `pct="col"` this mismatches. A row
reference IS reachable per-column (the `in_totrow`/`in_refrow` flags travel in the cells); the
blocker is a COLUMN reference for means, which needs cross-column data a single-column color pass
cannot see. The clean fix is at BUILD time (compute the mean `diff`/`ratio` against the reference
column under `pct="col"`, like factors), not in the color layer. **Recommendation: defer to Phase 7
(transpose-at-export), where decisions §7 already places it.** Phase 5 only: (a) make the engine
correctly consume whatever `diff`/`ratio` the build wrote (so a Phase 7 build fix auto-corrects
color), and (b) warn on `pct="col"` + means that mean coloring is row-referenced.

## 8. Implementation phasing (safety net FIRST)

- **Step 0 — Color characterization golden (no code change).** Colors are absent from all current
  snapshots (`tab_md` is monochrome; `.rds` store fields). Add `test-color-golden.R` + a
  `make_color_golden.R` capturing `fmt_get_color_code()` per-cell hex AND the console ANSI of
  `pillar_shaft`/legend, over {mode} x {factor/mean} x {text/bg} x {theme} x {24-bit}, for
  representative columns. Commit on current source. This is what makes "byte-identical factor diff"
  mechanically checkable.
- **Step 1 — Breaks model** (per-quantity option, `mirror_breaks`, new `set_color_breaks` + shims,
  `.onLoad` seed, `get_color_breaks` migration). Goldens stay green.
- **Step 2 — Palette/style** (ratio-ramp section, length-11/21 custom fix, explicit channel type;
  defaults reproduce today).
- **Step 3 — Engine, single (text) channel** (`fmt_color_plan`/`fmt_color_slots`/`color_slot_map`;
  route `pillar_shaft` + `fmt_get_color_code`). Gate: factor `diff`/`diff_ci`/`after_ci`/`ci`/
  `contrib`/`OR` byte-identical; consciously regenerate numeric `n_mean_color` (Glass's delta), the
  corrected pct CI-mode goldens, and `f_color_contrib` if reworked.
- **Step 4 — `color_bg` attribute + two channels** (vctrs plumbing, `c(text,bg)` parsing +
  validation, wire bg through all consumers). New goldens for `ratio`, `diff_ratio`.
- **Step 5 — Legend + exporters two-channel** (`tab_kable`/`tab_plot`/`tab_xl`/legend bg;
  export-parity).
- **Step 6 — Delete dead code**; docs (`@param color`, `devtools::document()`), NEWS, `/color-mode`
  skill, `dev/tabxplor_architecture.md`, close `dev/tabxplor_2.0.0_decisions.md` §3/§18/§20.

Each step: golden + parity green; before/after benchmarks to `dev/benchmarks/results_2.0.0/`.

## 9. Benchmark plan

The refactor exists for performance (driven by `keep_last_break`), so measure:

- Micro: `fmt_color_channels(col)` vs current `fmt_color_selection(col)+select_in_color_style+
  get_color_style` on tall single columns (500 / 5,000 rows), all modes, via `bench::mark` (time +
  `mem_alloc`). Biggest expected win.
- Column-count scaling: wide tables (50 / 200 colored columns).
- End-to-end: `print()` of a colored `tab()` on gss_cat and the 8M `big_df`; `tab_xl()` +
  `tab_kable()` on a medium table.
- Harness: extend `dev/benchmarks/run_bench.R` with colored ops (`color="diff"`, `"ratio"`,
  `c("diff","ratio")`, wide `tab_many(color="diff")`); add an in-suite micro case to
  `tests/testthat/test-benchmark.R`. Save `before/after_phase5_8M.csv` under
  `dev/benchmarks/results_2.0.0/`.

## 10. Open decisions for the maintainer (carry into the implementation session)

- **D1** Two-channel composition: may `ci` be a background "significance shade" over a magnitude
  text ramp, or restrict background to `{ratio, diff, mean_diff}` in 2.0.0?
- **D2** The interleave quirk (a diff above the top break overriding the ×N highlight): preserve for
  byte-identity (recommended now) or clean up consciously (regenerate `f_color_diff`)? Less relevant
  once the two-channel model is the recommended way to show ratio.
- **D3** `color="auto"`/`TRUE` numeric default = `"ratio"` (confirmed) — needs a `type=="mean"`
  branch in the resolver so numeric `"auto"` maps to `ratio`, not `diff`.
- **D4** Full ratio +/-ramp hues: derive from the single ratio hue (experimental) until hand-tuned
  per palette; how many intensities (cap at 5/direction like diff)?
- **D5** `contrib` per-cell significance: expose the standardized Pearson residual (|r|>1.96) as the
  contrib `pvalue`/stars, or keep contrib purely descriptive?
- **D6** Naming: is `set_color_scheme()` (presets) worth shipping, or keep only
  `set_color_breaks()`/`set_color_style()`?
- **D7** `fmt_color_selection` shim: may `expect_color()` migrate so the shim can be dropped within
  2.0.0?
- **D8** Glass's delta needs the reference `var`; when absent (e.g. some `pct="col"` cases) the cell
  is uncolored — acceptable, or fall back to `ratio`?

## 11. Key files and line references

- `R/fmt_class.R` — engine (`fmt_color_selection` 2053, `color_formula` 2341, `keep_last_break`
  2273, `select_in_color_style` 2805), `get/set_color` 908-948, `fmt_get_color_code` 973,
  `pillar_shaft` 1902, legend 2450-2785, reference helpers 1280-1326, `new_fmt`/vctrs methods
  1015/3087/3177/3339.
- `R/tab_classes.R` — palettes 2906-3058, `set_color_style` 3082, `get_color_style` 3126,
  `set_color_breaks` 3228, `get_color_breaks` 3399, `tab_kable` 585-659, `tab_plot` 1332-1361.
- `R/tab.R` — color arg resolution/vectorization 741/820-886/1027-1057, field build
  `diff=..4`/`mean=..5`/`ratio=..6` + color `case_when` 2654-2694, pct ratio build 2432-2549.
- `R/tab_xl.R` — `colorToStyle` 226-250, conditional-fmt map 1216-1260.
- `R/tab-agg.R` — sound CI/bounds/pvalue source (consume, no change).
- `R/utils.R` — `.onLoad` breaks/style seed 49-55.
- Tests: `tests/testthat/test-golden.R` + `helper-golden.R` (add the Step-0 color net),
  `test-tab.R` (`expect_color`), `test-exports.R`, `test-calculations.R`.
- Decisions: `dev/tabxplor_2.0.0_decisions.md` §3/§7/§12/§18/§20/§24.

## 12. Proposed framework 2 — radically new colors UI

> Blank slate. This section ignores retro-compatibility ENTIRELY. Everything — argument names,
> the `color=` vocabulary, the palette and breaks model, the exported setters, even which modes
> exist — is on the table. The only two north stars are **consistency** and **user-friendliness**,
> with a strict "meaningful customization only, no white elephants" filter. The findInterval engine
> of section 6.2 is retained (it is the right implementation regardless of surface); what changes is
> everything the user touches.
>
> Read framework 1 (section 5) as "the best you can do while protecting the old API"; read this as
> "what the API should have been." A future decision picks one, or a hybrid.

### 12.1 Design stance — start from the reader, not the fields

A colored cross-tab answers exactly three questions for the reader, at a glance:

1. **Direction** — is this cell above or below what the reference would predict?
2. **Magnitude** — by how much (how strong is the effect)?
3. **Reliability** — can I trust that deviation, or is it sampling noise?

Every item in today's catalogue (`diff`, `ratio`, `diff_ci`, `ci`, `after_ci`, `contrib`, `OR`) is
some point in the cross-product of {which magnitude measure} × {how reliability is folded in} ×
{which visual channel}. The catalogue is flat and therefore combinatorial; the design mistake is
encoding a 3-dimensional choice as one string. Fix that and the catalogue evaporates.

### 12.2 The core insight — three orthogonal axes

Expose the three axes as three independent choices the user composes, instead of a flat mode list:

- **Axis M — measure** (what "magnitude + direction" means): `diff`, `ratio`, `contrib`, `or`.
- **Axis C — channel** (where it is drawn): `text` color, `fill` (background). Two channels max —
  they are the only two a table cell reliably has across console/HTML/Excel.
- **Axis S — significance policy** (how reliability folds in): `all` (color every deviation) or
  `signif` (color only trustworthy deviations). Optionally `discount` (grade the CI-floored effect).

The whole old catalogue is now a composition:

| old mode                 | framework-2 composition                                                        |
|--------------------------|--------------------------------------------------------------------------------|
| `diff`                   | measure `diff`, policy `all`                                                   |
| `ratio`                  | measure `ratio`, policy `all`                                                  |
| `diff_ci`                | measure `diff`, policy `signif`                                                |
| `ci`                     | measure `diff`, policy `signif`, one intensity (a degenerate case — see 12.10) |
| `after_ci`               | measure `diff`, policy `discount`                                              |
| `contrib`                | measure `contrib`                                                              |
| `OR`                     | measure `or`                                                                   |
| the ×N "ratio highlight" | measure `diff` on `text` + measure `ratio` on `fill` (two channels)            |

Nothing is lost; the seven-plus modes become 4 measures × 2 policies × 2 channels, *composed*, none
of them a memorized string.

### 12.3 The `color=` argument — one argument, three shapes

`tab()` keeps a single `color` argument (opt-in, off by default). It accepts three escalating shapes:

```r
# 1. Switch — the 90% case
color = FALSE                       # default: no color
color = TRUE                        # the smart default scheme (see 12.9)

# 2. Shorthand string(s) — the 9% case
color = "diff"                      # measure diff, on text
color = "ratio"                     # measure ratio, on text
color = c("diff", "ratio")          # diff on text, ratio on fill  (the two-channel case)
color = "signif"                    # measure diff, policy signif, on text (only significant)

# 3. Spec object — the 1% expert case, fully explicit and self-documenting
color = color_cells(
  text   = "diff",                  # measure on the text color   (NULL = none)
  fill   = "ratio",                 # measure on the background    (NULL = none)
  policy = "signif"                 # "all" | "signif" | "discount"
)
```

Rules that keep it coherent:

- A bare string is a measure drawn on `text`. A length-2 string is `c(text, fill)`. `"signif"` is
  the one sugar that also sets a policy (because "only significant differences" is the single most
  requested non-default). Everything else needs the explicit `color_cells()` spec — which is
  discoverable (argument completion), reads like prose, and is the ONLY place policy and two
  measures meet. This is the ggplot2 `aes()` / `scale_*()` pattern: trivial cases stay trivial, full
  control lives in one composable constructor, and `tab()`'s signature gains exactly one argument.
- Per-column-type dispatch is automatic and invisible (12.4): the user writes `"diff"` once and each
  column is measured the right way. There is never a `color_mean`, `color_or`, or a per-column vector.

### 12.4 Measures, with automatic per-type dispatch

`text=`/`fill=` name a measure; the engine picks the concrete quantity from the column's type. The
user's vocabulary is type-free; the type-awareness is the engine's job.

| measure   | factor / % column                                                             | numeric / mean column                                | channels allowed |
|-----------|-------------------------------------------------------------------------------|------------------------------------------------------|------------------|
| `diff`    | pp difference vs reference (standardized to an effect size for breaks — 12.7) | standardized mean difference (Cohen's d / Glass's Δ) | text, fill       |
| `ratio`   | relative risk `p_cell/p_ref`                                                  | mean ratio `m_cell/m_ref`                            | text, fill       |
| `contrib` | signed χ² contribution (association structure)                                | — (n/a)                                              | text only        |
| `or`      | empirical odds ratio                                                          | — (n/a)                                              | text only        |

`contrib` and `or` are whole-cell association measures, not cell-vs-reference deviations; putting
them on `fill` while another measure is on `text` is meaningless, so it is rejected with a clear
message. Everything else composes freely.

### 12.5 Two channels across every medium

The two channels map cleanly to each output:

| channel | console           | HTML (`tab_kable`)       | Excel (`tab_xl`)           | markdown (`tab_md`) |
|---------|-------------------|--------------------------|----------------------------|---------------------|
| `text`  | crayon foreground | `cell_spec(color=)`      | `createStyle(fontColour=)` | pandoc span class   |
| `fill`  | crayon background | `cell_spec(background=)` | `createStyle(fgFill=)`     | pandoc span class   |

Console background IS available (crayon bg; tabxplor already ships bg palettes), so two-channel works
everywhere. `tab_md` gains color for the first time via short pandoc bracketed-span classes
(the roadmap already anticipates this) — the same class names drive HTML/CSS and could be styled
inside Jamovi. Because both channels resolve their palette at render time per medium (12.8), the same
table object still renders correctly to every target — no color is baked into the object.

### 12.6 Significance as a policy, not a mode

Reliability is orthogonal to measure, so it is one argument (`policy`), not four modes:

- `policy = "all"` (default) — color every deviation, ignore significance. Descriptive.
- `policy = "signif"` — color ONLY cells whose deviation is significant; the rest stay neutral.
  This is the honest default for inferential reading and subsumes old `diff_ci`.
- `policy = "discount"` (advanced, optional) — grade by the CI-floored effect (the guaranteed
  minimum), i.e. old `after_ci`. Kept only because it has a real, if niche, use; not in the shorthand
  vocabulary.

Implementation is the section-3.1 primitive: `signif = ci_inf > 0 | ci_sup < 0` — sound for every
method, identical across % and means, never contradicting the printed bracket or the stars. Policy
composes with any measure and either channel.

Relation to stars: stars (the existing `signif`/`stars` display) annotate the *exact* p per cell;
`policy` decides whether color is *shown* at all for non-significant cells. They are complementary
and both read from the same stored bounds, so they can never disagree. A reader can have graded
color for magnitude AND stars for exact reliability, with `policy="all"`; or hide noise with
`policy="signif"`.

### 12.7 Breaks — one universal "strength" ladder (the big simplification)

Today every measure/type needs its own break vector on its own raw scale (pp, ratio, Cohen's d, χ²
share), which is most of the config complexity. Framework 2 collapses this by coloring **standardized
effect strength** with ONE ladder for the whole table:

```r
set_color_strength(c(0.2, 0.5, 0.8))     # small / medium / large  (Cohen conventions) — global
```

- For a mean difference, strength = Cohen's d / Glass's Δ = `diff / sd_ref` (already decided in §18).
- For a proportion difference, strength = Cohen's h = `2·asin(√p_cell) − 2·asin(√p_ref)` — the
  standard effect size for proportions.
- For a ratio, strength = `|log(ratio)|` on a matched log ladder.
- For contrib, strength = contribution / mean-contribution (already a ratio).

Why this is the right radical move, not a clever white elephant:

- **One number set to learn.** "Color medium-and-larger effects" is `set_color_strength(0.5)`.
  No per-measure, per-type juggling. The legend reads "small / medium / large", universally.
- **It subsumes the ×N ratio hack on principled grounds.** Cohen's h grows fast for the same pp
  difference as the base rate approaches 0 or 1 — exactly the small-base sensitivity the ×N rule was
  invented for. A +3pp shift off 3% is a large h; off 40% it is small. So the small-base highlight
  becomes automatic and continuous, not a single bolt-on purple color.
- **It makes diff comparable across columns of different base rates and across % vs means** — a table
  mixing a 5%-base variable and a 50%-base variable colors "large" consistently.

Honest tradeoff (stated so the maintainer can decide): many analysts think in raw units ("≥10
points"). So framework 2 keeps a raw-scale override for those who want it, per measure:

```r
set_color_breaks(diff_pct = c(0.05, 0.1, 0.2), ratio = c(1.5, 2, 4), contrib = c(1, 2, 5))
# switches that measure from the universal strength ladder to explicit raw thresholds
```

Recommendation: universal strength ladder as the DEFAULT (clean, cross-type, subsumes ×N), raw
per-measure breaks as an opt-in for unit-thinkers. If the maintainer finds effect sizes
counter-intuitive for teaching, invert the default (raw pp for %, strength for means) — but the
universal ladder is the more coherent story.

### 12.8 Palettes — diverging ramps, not 11 named slots

The 6 palettes × 11 hand-tuned slots (`pos1..pos5, neg1..neg5, ratio`) become, per channel and
theme, a single **diverging color ramp**: strong-negative pole → neutral → strong-positive pole.

```r
set_color_palette(
  text  = c(low = "#B2182B", mid = "grey30", high = "#2166AC"),  # diverging, sampled at break count
  fill  = c(low = "#F4A582", mid = "white",  high = "#92C5DE"),  # paler, for backgrounds
  theme = "auto"                                                 # light/dark autodetected
)
```

- **Intensity = effect strength**, sampled from the ramp at however many breaks exist. No fixed
  five-per-side; two breaks give two shades, five give five, from the same ramp. The awkward "diff
  ramp + separate single ratio slot" disappears — ratio uses the same diverging ramp because
  intensity is strength, not measure-specific.
- **Colorblind-safe by default** (blue-red diverging, per the dataviz guidance), the current design's
  stated aspiration made real.
- **Text vs fill** get distinct ramps (saturated for text, pale for fill) so a two-channel cell stays
  legible — the readability guarantee falls out of the ramp choice, not a special mode.
- Custom palettes are just "give me your three poles" (or a full ramp), which fixes the length-11
  custom-palette bug by making length irrelevant.

### 12.9 The default scheme (`color = TRUE`)

Per column type, `color = TRUE` resolves to:

- **factor / %:** `text = "diff"` (strength-graded), `policy = "all"`. On rich media, optionally
  `fill = "ratio"` too (the modern replacement for the ×N highlight). The classic single-channel look
  is exactly `color = "diff"`.
- **numeric / mean:** `text = "ratio"` (the maintainer's chosen numeric default), `policy = "all"`.
- **odds-ratio columns:** `text = "or"`.
- **counts:** no color.

The fate of the beloved 8-diff + 1-×2 look: it is `color = "diff"` with the universal strength ladder
(the small-base cells that used to get the single purple now get strong diff-strength color
automatically — arguably better). If the maintainer wants the literal old look preserved as the
default, that is a one-line preset; but the recommendation is the strength-graded diff, because it is
the coherent version of the same intent.

### 12.10 Deliberately dropped (white elephants) and why

- **Binary `ci` mode** — a graded-but-gated `policy="signif"` strictly dominates "significant y/n
  with one shade". Dropped; `color="signif"` covers the intent better.
- **The interleaved ×N magic and the single `ratio` palette slot** — replaced by the universal
  strength ladder (12.7) + `fill="ratio"` (12.5). No positional `>1` encoding, no odd-length break
  vector, no `ci`-mode negative-direction bug (they cannot exist in this model).
- **`diff_ratio` as a string** — it is just `c("diff","ratio")`.
- **Per-column color vectors and per-type `color_*` arguments** — replaced by automatic dispatch.
- **The `auto` vs `TRUE` ambiguity** — `TRUE` is the documented default scheme; `auto` retired.
- **`after_ci`/`discount` from the shorthand** — kept only as an explicit expert `policy`, not a mode.
- **The `type="text"/"bg"` global toggle** — obviated: channels are chosen per measure in `color=`,
  and each channel has its own palette. No global "are we in text or bg mode" flag.

Kept because genuinely meaningful: measure choice (4), two channels, significance policy, the global
strength ladder (or raw per-measure breaks), and diverging palettes per channel/theme.

### 12.11 Worked examples (framework 2 vs today)

```r
# today                                   framework 2
tab(d, region, sex, color = "diff")     # tab(d, region, sex, color = "diff")            (same word, cleaner engine)
tab(d, region, sex, color = "diff_ci")  # tab(d, region, sex, color = "signif")          (readable)
tab(d, region, age, color = "diff")     # tab(d, region, age, color = "diff")            (auto: mean -> Cohen's d)
# (no clean way today)                   # tab(d, region, sex, color = c("diff","ratio")) (diff text + ratio fill)
# (no clean way today)                   # tab(d, region, sex,
                                         #     color = color_cells(text="diff", fill="ratio", policy="signif"))
set_color_breaks(pct_breaks=            # set_color_strength(c(0.2, 0.5, 0.8))            (one ladder, all measures)
  c(.05,.1,.2,2,.3), mean_breaks=...)   #   or set_color_breaks(diff_pct=c(.05,.1,.2))   (raw override)
set_color_style(type="bg", ...)         # set_color_palette(fill = c("#F4A582","white","#92C5DE"))
```

### 12.12 What it costs (no retro-compat) + implementation note

Breaking changes for the (tiny) user base, to budget consciously:

- `color=` string values change meaning: `"diff_ci"`->`"signif"`, `"after_ci"`/`"ci"` gone from the
  shorthand, `"OR"`->`"or"`. Old strings would error with a helpful "did you mean…" message.
- `set_color_breaks()`/`set_color_style()` replaced by `set_color_strength()` +
  `set_color_breaks(<per-measure>)` + `set_color_palette()`. A deprecation layer is optional (the
  premise is we may drop it), but a one-release soft-deprecation with translation is cheap insurance.
- `get_color()`/`set_color()` now carry a small spec (measure + channel + policy) per column, not a
  scalar string; user code reading `$` fields (`$diff`,`$ratio`) is unaffected (fields don't change).

Reassuring part: the **engine is the same** as framework 1 — findInterval over a signed break vector,
`sig_pos/sig_neg` from the stored bounds, per-cell integer slot per channel. Framework 2 is almost
entirely a *surface* redesign (arguments, palette representation, breaks representation, dropped
modes) on top of the identical fast core. So it is not more work to *compute*; it is a different, and
better, thing to *ask for*.

### 12.13 Open questions specific to framework 2

- **F1** Effect-size default vs raw-unit default for breaks (12.7): is Cohen's h/d as the universal
  ladder acceptable for the teaching audience, or should raw pp stay the default for % with strength
  only for means?
- **F2** Does `color = TRUE` include `fill = "ratio"` for factors (modern two-channel default), or
  stay single-channel `text="diff"` to echo the classic look?
- **F3** RESOLVED (§13): NOT a constructor — two `color` scalars (text, fill) + a separate
  `color_signif` argument. The maintainer prefers policy as its own top-level argument.
- **F4** REFINED (§13.1): policy C (old `after_ci`) is KEPT as the third policy value (real usage:
  small samples), reframed as "color the guaranteed/CI-floored effect", not dropped.
- **F5** Color for `tab_md` via pandoc span classes — worth building now (also unlocks Jamovi HTML
  coloring), or defer?
- **F6** How much of the old API to soft-deprecate vs hard-break, given the premise allows breaking.

## 13. Framework 2 — finalised: the policy argument, composition, scenarios, computation

> This section closes framework 2 with the maintainer's four finalising decisions. It resolves F3
> (significance policy is a SEPARATE second argument, not a constructor field) and refines F4. The
> mental model is now complete: **two `color` scalars** (text measure, fill measure) **+ one
> significance-policy argument**, all orthogonal.

### 13.1 The significance-policy argument — name and values

**The argument.** It answers "what is the policy of color with respect to significance / the
'is-0-inside-the-CI?' test." The name must start with `color_` (so it sorts next to `color` in
completion) and read as "significance", not abstract "policy". Recommendation, in order:

- **`color_signif`** (ACCEPTED) — immediately says "color × significance"; parallel to the
  existing stars/`signif` display, and the two are duals (both read the same stored bounds).
- `color_test` — "how color relates to the significance test"; also good, slightly more technical.
- `color_policy` (the maintainer's first idea) — findable but abstract; a reader can't guess it is
  about significance without the docs.
- `color_confidence` / `color_when` — considered and rejected (vaguer).

**The three values — real usage first (this is what the docs must teach, not the abstract math):**

- **A — no significance test.** Color every deviation by its observed size. *Usage:* exploratory /
  descriptive reading, or large samples where almost everything is significant anyway. (= old `diff`.)
- **B — grey out the non-significant.** Keep exactly the same colors as A (the observed effect size),
  but mute (grey) the cells whose deviation could be sampling noise. *Usage:* "let me read the real
  effect sizes without being distracted by noise" — the everyday inferential default. The colored
  cells are the significant ones; their color still encodes the OBSERVED magnitude. (= old `diff_ci`.)
- **C — color the guaranteed effect.** Color intensity is the CI-floored (conservative) effect, so a
  cell shows only if even its confidence bound clears the threshold. *Usage:* "with my small sample,
  show me only what I can defend, at the size I can defend" — stricter than B, and the colors are
  dimmer than B because they encode the guaranteed minimum, not the point estimate. (= old `after_ci`.)

The B-vs-C nub, stated once so naming can encode it: **B colors the OBSERVED effect and filters by
significance; C colors the GUARANTEED (CI-floored) effect.** Both hide noise; only C also shrinks the
intensity to what the CI proves.

**Five naming proposals (columns A / B / C). Pick one row; the argument is `color_signif = <value>`.**

| # | style                       | A (no test)        | B (grey non-signif)        | C (guaranteed)       |
|---|-----------------------------|--------------------|----------------------------|----------------------|
| 1 | short & memorable           | `"all"`            | `"focus"`                  | `"strict"`           |
| 2 | reading verbs (usage-first) | `"explore"`        | `"highlight"`              | `"confirm"`          |
| 3 | observed ↔ guaranteed       | `"observed"`       | `"observed_signif"`        | `"guaranteed"`       |
| 4 | explicit / self-documenting | `"ignore_signif"`  | `"grey_non_signif"`        | `"signif_floor"`     |
| 5 | CI framing (stats audience) | `"no_ci"`          | `"ci_gate"`                | `"ci_floor"`         |
| — | maintainer's old            | `"no_signif_test"` | `"only_color_when_signif"` | `"color_all_signif"` |
| — | ACCEPTED                    | `"ignore"`         | `"grey_non_signif"`        | `"color_all"`        |

Recommendation: **set 2 (`explore`/`highlight`/`confirm`)** for a teaching audience — each word names
the reader's goal, is short, and the trio reads as a natural escalation of rigour. If a more literal
style is preferred, **set 4** is the clearest self-documenting option. Avoid the maintainer's own set:
`only_color_when_signif` and `color_all_signif` both describe the shared "colored ⇒ significant"
outcome and so do not separate B from C (whose real difference is observed-vs-guaranteed intensity).

### 13.2 Composition matrix — measure × policy

Every measure works with every policy; the only thing that varies per measure is **which CI / test**
supplies the significance. (Rows = `color` measure, columns = `color_signif` policy.)

| measure ↓ / policy → | A no test                  | B grey non-signif                                | C guaranteed                 |
|----------------------|----------------------------|--------------------------------------------------|------------------------------|
| `diff`               | observed diff, all cells   | observed diff, grey where diff-CI ∋ 0            | diff CI-floor (conservative) |
| `ratio`              | observed ratio, all cells  | observed ratio, grey where cell=ref not rejected | ratio CI-floor               |
| `or`                 | observed OR, all cells     | observed OR, grey where OR-CI ∋ 1                | OR CI-floor                  |
| `contrib`            | χ² contribution share, all | contribution, grey where \|resid\| < 1.96        | significant residuals only   |

**All 12 cells are valid** — so, correcting section 12.4: **`or` takes the three policies too**, using
its own CI (`ci_inf`/`ci_sup`); this is exactly what `tab_logit()` will exploit. There is no "measure
that is its own policy".

The significance source per measure (this is the whole per-measure variation):

- **`diff` and `ratio` share one test** — "is this cell different from its reference?" (cell-vs-ref
  difference CI: Newcombe for %, Welch-t for means). So a table showing diff on text + ratio on fill
  computes that CI ONCE for both channels. Only policy C needs the bound in the measure's own scale
  (additive for diff, multiplicative for ratio); policy B needs only the shared boolean.
- **`or`** uses its own odds-ratio CI (log-OR Wald, Haldane–Anscombe on empty cells; the `tab_logit`
  model CI later).
- **`contrib`** uses the standardized Pearson residual (\|r\| > 1.96), computed from the χ² margins.

**What is special about `contrib` (the maintainer's intuition, made precise):** it is the only measure
that tests a cell against **independence** (both margins, symmetric in rows↔columns, reference-free),
whereas `diff`/`ratio`/`or` test against a **chosen reference** (a row, a column, or a level). That
symmetry is exactly why `contrib` is the natural — indeed the only — way to color **counts**: a raw
count cell has no reference to deviate from, so its only meaningful deviation is from the independence
model, i.e. the residual, i.e. `contrib`. So `contrib` is not "its own policy"; it is the
**reference-free, symmetric measure**, and "color counts by significance" is precisely
`color = "contrib", color_signif = "highlight"` (B) or `"confirm"` (C).

Composition with channels (§12.5): `diff`, `ratio` can sit on text OR fill; `contrib`, `or` are
whole-cell measures and are text-only by default. A cell CAN carry, say, `diff` on text (vs a
reference) and `contrib` on fill (vs independence) — coherent, but niche; allowed, not defaulted.

### 13.3 The historical scenarios, mapped to the new API

| what you wanted (past discussions)                                   | framework-2 call                                   | channels used   |
|----------------------------------------------------------------------|----------------------------------------------------|-----------------|
| 8 diff colors, one channel (text)                                    | `color = "diff"`                                   | text            |
| 8 diff + a single ×2-ratio highlight (see §14.1-C: ratio needs fill) | `color = c("diff","ratio")`, `pct_ratio = c(2)`    | text + fill     |
| 8 diff colors + 8 ratio colors                                       | `color = c("diff", "ratio")`                       | text + **fill** |
| only significant differences                                         | `color = "diff", color_signif = "highlight"`       | text            |
| only what's defensible on a small sample                             | `color = "diff", color_signif = "confirm"`         | text            |
| relative risk / ratio for factors                                    | `color = "ratio"`                                  | text            |
| mean ratio for numerics (default)                                    | `color = "ratio"` or `color = TRUE`                | text            |
| odds ratios with significance (à la tab_logit)                       | `color = "or", color_signif = "highlight"`         | text            |
| χ² contributions / color the counts                                  | `color = "contrib"` (+ `color_signif=` for gating) | text            |
| factor→diff AND numeric→ratio automatically                          | `color = TRUE`                                     | text (per-type) |

**Answering "is background compulsory for diff + ratio together?":** it depends on how much of the
ratio you want to show.

- A **single** ratio highlight (the old ×2 purple) CANNOT live in the text channel — one text color
  already encodes the diff, and a cell has only one text color. So even the lone ×2 rule uses the
  **fill** channel: `color = c("diff","ratio")` with `pct_ratio = c(2)`. (This corrects an earlier
  draft claim; see §14.1-C. The maintainer has accepted that the classic all-text look is retired.)
- Two **full** ramps (8 diff AND 8 ratio, each graded) **cannot** share one channel — a single text
  color cannot encode two independent quantities per cell. So **yes, showing a full ratio ramp
  alongside a full diff ramp requires the fill channel**: `color = c("diff", "ratio")`. This is the
  principled reason two-channel exists.

### 13.4 Computation matrix — what `tab()` must compute per choice

`tab()`/`tab_many()` reads the color intent FIRST, then computes only the fields the intent needs and
stores them in the `tabxplor_fmt` record; the render-time engine only ever reads fields. Rows =
measure, columns = policy; each cell lists the INCREMENTAL computation over policy A.

| measure ↓ / policy → | A (base, no CI)                                              | B adds                                            | C adds                                        |
|----------------------|--------------------------------------------------------------|---------------------------------------------------|-----------------------------------------------|
| `diff`               | reference + `diff` (+ `sd_ref` via `var` if strength breaks) | cell-vs-ref CI → `sig` boolean                    | cell-vs-ref CI **bounds** (`ci_inf`/`ci_sup`) |
| `ratio`              | reference + `ratio`                                          | *shares* the cell-vs-ref CI boolean (no new calc) | ratio-scale CI bounds                         |
| `or`                 | reference (ref2) + `or` (2×2 counts)                         | OR CI → `sig` boolean                             | OR CI bounds (log-OR Wald)                    |
| `contrib`            | χ² decomposition (`ctr`, margins) — no reference             | standardized residuals → `sig`                    | residuals graded (guaranteed)                 |

Reading rules for the planner:

- **Policy A costs no inference** — just the point-estimate field (+ `sd_ref` only if the strength
  ladder is on, §12.7). This is the cheapest path and should stay the default for `color = "diff"`.
- **Policy B** adds the significance **boolean** only; **policy C** adds the CI **bounds** (bounds
  subsume the boolean). Both come from the section-3.1 primitive `ci_inf>0 | ci_sup<0`.
- **`diff` + `ratio` on two channels share their cell-vs-ref CI** — compute it once; only policy C
  needs each channel's own-scale bound.
- **`contrib` needs no reference and no cell-vs-ref CI** — it needs the χ² decomposition; its B/C
  significance is the residual, not the CI. So a `contrib` table and a `diff` table compute disjoint
  inference machinery.
- The planner computes the **union** of requirements over both channels and the policy. Examples:
  + `color = "diff", color_signif = "explore"` → `diff` only (+`sd_ref` if strength). No CI. Cheapest.
  + `color = c("diff","ratio"), color_signif = "highlight"` → `diff` + `ratio` + one shared cell-vs-ref
    CI boolean.
  + `color = "or", color_signif = "confirm"` → `or` + OR CI bounds (the `tab_logit` path).
  + `color = "contrib"` → χ² decomposition; `+ color_signif="highlight"` adds residuals.

This is the "choose intent → compute exactly what's needed → store in fields → render reads fields"
loop the maintainer described, now fully specified.



## 14. Quasi-final choice — consistency review and full specification

> This section reviews the maintainer's near-final proposal, flags what is solid and what still needs
> improvement, and specifies every detail. It supersedes the earlier framework-2 subsections where
> they differ (notably: it CORRECTS §13.3's claim that a single ratio highlight can live in the text
> channel — it cannot; ratio always uses the fill channel).

### 14.1 Consistency verdict

**Solid and internally consistent (keep as-is):**

1. `color_signif` as a separate argument with three values mapping 1:1 to the three policies. ✓
2. Wiring the deprecated mode strings (`diff_ci`, `after_ci`, `ci`, …) to `color` + `color_signif`
   combinations for back-compat. ✓ (mapping in 14.2)
3. Ratio (incl. the lone ×2 rule) ALWAYS on the fill/background channel — this is forced, not a
   choice: one text color cannot carry a second graded quantity. ✓ (corrects §13.3)
4. Break-vector LENGTH controls the number of ratio steps: `pct_ratio_breaks = c(2)` → one ×2 bg
   color; `pct_ratio_breaks = c(1.5, 2, 3, 4)` → a graded ratio bg ramp. Elegant and needs no new
   knob. ✓
5. The hybrid breaks model (global default + opt-in table-level override) is consistent with the
   render-time architecture PROVIDED only thresholds are baked into the table, never the palette. ✓
6. Numeric diff: standardized by default, absolute when the user supplies unit breaks. ✓ (14.4)
7. Measure-group palettes (additive vs multiplicative) reconcile with §12.8 as "one diverging ramp
   PER group, strength-graded". ✓ (14.5)

**Needs improvement before locking (three real snags):**

- **A. `color_all` is a misleading name.** Its behavior colors FEWER cells (only significant, at the
  guaranteed floor), yet the words read as "color everything" — the opposite. This will confuse. It
  is a contraction of the maintainer's earlier `color_all_signif` ("all colored cells are
  significant"), but standalone it inverts its meaning. Recommend renaming to **`signif_floor`**
  (precise: intensity = the significant/CI floor) or `only_signif` / `guaranteed`. Keep `ignore` and
  `grey_non_signif` (both good — `ignore` = stance, `grey_non_signif` = action). See 14.2.
- **B. Two levers can enable/disable a measure.** The proposal lets an EMPTY break vector disable a
  measure ("`mean_diff_breaks` empty → don't compute numeric diff"), but `color` ALREADY decides which
  measures are active. Two independent on/off switches for the same thing is the kind of inconsistency
  this whole redesign exists to remove. Recommend: **`color` is the sole authority for which measures
  run**; breaks carry only thresholds. "Numeric ratio only, don't compute diff" is simply
  `color = "ratio"` (see 14.3). If a global "numerics default to ratio only" is wanted, that is the
  `color = TRUE` default (§12.9), not an empty-breaks trick.
- **C. The classic all-text default look is genuinely gone.** Because ratio must use the fill channel
  (point 3), the beloved "8 diff + 1 ×2, all in text" cannot be reproduced. The new factor default is
  **two-channel**: diff on text + the ×2 ratio on fill. The maintainer has accepted this ("we'll do
  exactly that") — recorded here so it is a conscious, documented change, not a surprise.

Minor watch-items: B and C (`grey_non_signif` vs `signif_floor`) color the SAME set of cells (the
significant ones) and differ only in intensity (observed vs guaranteed) — document the distinction
sharply so users see why both exist (C is the small-sample honest view). And the 4-palette design is a
real perceptual-color challenge (14.5).


### 14.2 `color_signif` — values, recommended naming, and back-compat wiring

Values (recommended spelling in bold; the maintainer's `color_all` flagged in 14.1-A):

| policy | maintainer's name   | recommended             | meaning (usage-first)                                         |
|--------|---------------------|-------------------------|---------------------------------------------------------------|
| A      | `"ignore"`          | **`"ignore"`**          | no test; color every deviation by observed size               |
| B      | `"grey_non_signif"` | **`"grey_non_signif"`** | observed size, but grey out the non-significant (focus)       |
| C      | `"color_all"`       | **`"signif_floor"`**    | color the guaranteed (CI-floored) effect; small-sample honest |

Back-compat: the old `color=` strings are soft-deprecated and rewritten to a `(color, color_signif)`
pair (and, for the ×2, a ratio channel):

| old `color=`       | new `color` | new `color_signif`  | notes                                                                                                                                                         |
|--------------------|-------------|---------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `"diff"` (factor)  | `"diff"`    | `"ignore"`          | text diff; the old in-text ×2 moves to fill (default scheme)                                                                                                  |
| `"diff_ci"`        | `"diff"`    | `"grey_non_signif"` |                                                                                                                                                               |
| `"after_ci"`       | `"diff"`    | `"signif_floor"`    |                                                                                                                                                               |
| `"ci"`             | `"diff"`    | `"signif_floor"`    | + a single significance break (14.6)                                                                                                                          |
| `"contrib"`        | `"contrib"` | `"ignore"`          |                                                                                                                                                               |
| `"OR"`/`"or"`      | `"or"`      | `"ignore"`          |                                                                                                                                                               |
| `"diff"` (numeric) | `"ratio"`   | `"ignore"`          | SEMANTIC CHANGE: old numeric "diff" was ratio-coloring (§3); new `color="diff"` on a numeric is Glass's Δ. The old look = `"ratio"`. Emit a one-time message. |

### 14.3 Breaks — hybrid global + table override, and how length shapes the ratio channel

**Hybrid model (the maintainer now adopts the approach initially declined).** Breaks live in global
options (`set_color_breaks()`), and `tab()` gains an optional `color_breaks =` argument that
supersedes the global values **for that table only**, stored as a table-level attribute and read at
render time. Palette stays global-only. This keeps the render-time / multi-medium guarantee (only
thresholds are baked, never hues), while letting an expert pin one table's breaks reproducibly.

```r
set_color_breaks(pct_ratio = c(2))                       # global: factors get the lone ×2 bg color
tab(d, region, sex, color = c("diff","ratio"),
    color_breaks = list(pct_ratio = c(1.5, 2, 3, 4)))    # THIS table: a full ratio bg ramp
```

**The per-quantity break vectors** (each positive-only, auto-mirrored; each independently global or
per-table):

- `pct_diff` — factor pp-difference (text). Default `c(0.05, 0.1, 0.2, 0.3)`.
- `pct_ratio` — factor relative risk (fill). Default `c(2)` = the lone ×2 rule; give more for a ramp.
- `mean_diff` — numeric difference (text). Default = the universal strength ladder `c(0.2,0.5,0.8)`
  as effect sizes; a vector in DATA UNITS switches to absolute (14.4).
- `mean_ratio` — numeric mean ratio (text or fill). Default `c(1.15, 1.5, 2, 4)`.
- `contrib` — χ² contribution. Default `c(1, 2, 5, 10)`.

**Length = number of colour steps** (the elegant part): one value → one shade (the ×2 highlight);
N values → N graded shades. So "8 diff text + 1 ratio bg" and "8 diff text + 8 ratio bg" differ ONLY
in `length(pct_ratio)`.

**On/off is `color`'s job, NOT the breaks' (14.1-B).** To NOT compute numeric diff, write
`color = "ratio"` for that column (or rely on the `color = TRUE` numeric default). Do not overload an
empty break vector to mean "measure off" — it conflates thresholds with intent. (If, despite this, the
maintainer wants empty-breaks-disables as a shortcut, define it as: empty ⇒ that measure is dropped
from an otherwise-active `color`, and document it loudly — but the recommendation is to let `color`
own on/off.)

### 14.4 Numeric diff — standardized by default, absolute on unit breaks

**What happens TODAY with `color="diff"` on a numeric and `breaks = c(200,500,1000,2000)`:** it does
not do what you want. Today numeric `diff` coloring reads the `ratio` field (the Phase-2 D3 interim,
§2.3/§3) and compares it to `mean_breaks`, which are interpreted as RATIOS (1.15 = +15%). Passing
`c(200,…)` would be read as "cell ≥ 200× the reference" — effectively never true — so you get no (or
nonsensical) colors. There is currently no way to color a raw salary difference.

**Framework-2 rule (clean resolution):**

- **Default (no user `mean_diff` breaks):** color the STANDARDIZED difference — Glass's Δ =
  `diff / sd_ref` — against the universal strength ladder `c(0.2, 0.5, 0.8)`. This is the only
  scale-free default that works across arbitrary numeric units (§18/§12.7). Needs `sd_ref` (the
  reference cell's `var`).
- **When the user supplies `mean_diff` breaks in data units** (e.g. `c(200,500,1000,2000)`): switch
  to the ABSOLUTE difference — color `diff` directly against those raw thresholds, no standardization,
  no `sd_ref` needed. The presence of a unit break vector is the switch.
- **Legend:** in standardized mode, translate back to the column's units so the reader sees real
  numbers, e.g. "medium = 0.5 SD ≈ ±210 €" (per column, since `sd_ref` varies); optionally also state
  the SD multiple. In absolute mode, print the literal unit thresholds ("±200 €, ±500 €, …"). This
  gives you both: a sensible default AND real-unit control, each legible in the legend.

So: `tab(d, region, salary, color = "diff")` → standardized, legend shows €-equivalents;
`tab(d, region, salary, color = "diff", color_breaks = list(mean_diff = c(200,500,1000,2000)))` →
absolute €.

### 14.5 Palettes — measure-group hues (2 to start, 4 the goal)

The proposal: distinct palettes per measure so a glance tells `diff` from `ratio` from `OR` from
`contrib`. This refines §12.8 (which used one measure-agnostic ramp): keep ONE diverging ramp *per
group*, strength-graded, but give each group a different hue family.

**Two-palette start (recommended):**

- **Additive group** (`diff`, `contrib`): the current light-text 24-bit **red → blue** diverging ramp
  (colorblind-friendlier), plus a background counterpart (paler).
- **Multiplicative group** (`ratio`, `OR`): a **red → violet-shifted blue** ramp (same luminance
  structure, hue rotated toward violet so it reads as "a different quantity" ?), plus a bg counterpart.

This pairs naturally with the channel usage: the common two-channel case is `diff` (additive, on
text) + `ratio` (multiplicative, on fill) — so the two groups already land on different channels AND
different hue families, doubly distinguishable. Within a group, `diff` and `contrib` never co-occur in
one table (both factor, one intent per table after Phase 6), and `ratio`/`OR` likewise, so sharing a
group palette causes no clash.

**The hard part (flag):** four (even two) diverging scales that are (a) mutually distinct AND
(b) internally step-distinct is a genuine perceptual-design task. Guidance: fix a common LUMINANCE
ramp for intensity (so "strong" reads equally strong across measures) and vary only HUE between
groups; validate with the `dataviz` skill's diverging-palette method and a colorblind check; cap at
~5 steps/side (beyond that, steps stop being distinguishable). Defer the full 4-palette split until
the 2-palette version is validated on real tables.

**Background counterparts:** every group needs a text ramp (saturated) and a fill ramp (pale) so a
two-channel cell stays legible — the readability guarantee of §12.8 still holds.

### 14.6 The old `ci` mode (binary significant/not), expressed

`ci` = "colour significant cells one shade per direction, ignore magnitude." In the new framework it
is `color = "diff"` (or `"ratio"`), `color_signif = "signif_floor"`, with a **single break at the
significance boundary**:

```r
tab(d, region, sex, color = "diff", color_signif = "signif_floor",
    color_breaks = list(pct_diff = c(0)))     # one step = "significant vs not", by direction
```

Why break `0` is consistent across both directions (the maintainer's worry): the engine handles the
positive and negative halves separately, and `signif_floor` colors pos when `ci_inf > brk` and neg
when `ci_sup < -brk`. With `brk = 0` this is `ci_inf > 0` (significant positive) and `ci_sup < 0`
(significant negative) — i.e. "the confidence interval clears 0", which is the SAME statement on both
sides; only the relevant bound differs (inf for over-, sup for under-representation). So `0` is the
natural, symmetric significance boundary; it is not ambiguous. A single positive break yields exactly
one shade per direction → the old binary `ci` look. (A convenience alias `color_signif = "signif_only"`
could hard-wire the single-0 break so users never type it — optional.)

### 14.7 Historical scenarios — full mapping (`color` + `color_signif` + breaks)

| scenario                                          | `color`             | `color_signif`                      | breaks (global or `color_breaks=`)   |
|---------------------------------------------------|---------------------|-------------------------------------|--------------------------------------|
| pct: 8 diff, text only                            | `"diff"`            | `"ignore"`                          | `pct_diff = c(.05,.1,.2,.3)`         |
| pct: 8 diff text + 1 ×2 ratio bg (factor default) | `c("diff","ratio")` | `"ignore"`                          | `pct_ratio = c(2)`                   |
| pct: 8 diff text + 8 ratio bg                     | `c("diff","ratio")` | `"ignore"`                          | `pct_ratio = c(1.3,1.5,2,3,4)`       |
| pct: only significant diffs                       | `"diff"`            | `"grey_non_signif"`                 | `pct_diff` default                   |
| pct: small-sample honest (guaranteed)             | `"diff"`            | `"signif_floor"`                    | `pct_diff` default                   |
| pct: binary significant/not (old `ci`)            | `"diff"`            | `"signif_floor"`                    | `pct_diff = c(0)`                    |
| numeric: mean ratio (default)                     | `"ratio"` or `TRUE` | `"ignore"`                          | `mean_ratio = c(1.15,1.5,2,4)`       |
| numeric: standardized diff (default)              | `"diff"`            | `"ignore"`                          | `mean_diff` = strength `c(.2,.5,.8)` |
| numeric: absolute salary diff                     | `"diff"`            | `"ignore"`                          | `mean_diff = c(200,500,1000,2000)`   |
| numeric: ratio only, skip diff calc               | `"ratio"`           | `"ignore"`                          | `mean_ratio` default                 |
| OR with significance (tab_logit)                  | `"or"`              | `"grey_non_signif"`                 | `mean_ratio` (OR breaks)             |
| χ² contributions                                  | `"contrib"`         | `"ignore"` (or `"grey_non_signif"`) | `contrib = c(1,2,5,10)`              |
| colour the counts by significance                 | `"contrib"`         | `"grey_non_signif"`                 | `contrib` default                    |

### 14.8 Computation matrices — split by col_var type

The planner reads `(color, color_signif, breaks)` and computes the union of what the active measures
and policy require. Two cases, because the available measures and the CI methods differ.

**A. Percentages (col_var is a factor).** Available measures: `diff` (pp, text), `ratio` (RR, fill),
`contrib`, `or` (2-level/logit). CI method = Newcombe (diff) / residuals (contrib) / log-OR (or).

| measure      | `ignore`                   | `grey_non_signif`                            | `signif_floor`        |
|--------------|----------------------------|----------------------------------------------|-----------------------|
| `diff` (pp)  | ref + `diff`               | + prop-diff CI → `sig`                       | + prop-diff CI bounds |
| `ratio` (RR) | ref + `ratio`              | *shares* the prop-diff `sig` (RR≠1 ⇔ diff≠0) | + RR CI bounds        |
| `contrib`    | χ² decomp (`ctr`, margins) | + std. Pearson residuals → `sig`             | + residuals graded    |
| `or`         | ref2 + `or` (2×2 counts)   | + log-OR CI → `sig`                          | + log-OR CI bounds    |

**B. Means (col_var is numeric).** Available measures: `diff` (text; standardized OR absolute per
14.4), `ratio` (mean ratio). No `contrib`/`or`. CI method = Welch-t (mean difference).

| measure                        | `ignore`                        | `grey_non_signif`            | `signif_floor`                    |
|--------------------------------|---------------------------------|------------------------------|-----------------------------------|
| `diff` (standardized)          | ref + `diff` + `sd_ref` (`var`) | + mean-diff CI → `sig`       | + mean-diff CI bounds (÷`sd_ref`) |
| `diff` (absolute, unit breaks) | ref + `diff`                    | + mean-diff CI → `sig`       | + mean-diff CI bounds             |
| `ratio` (mean ratio)           | ref + `ratio`                   | *shares* the mean-diff `sig` | + ratio CI bounds                 |

Cross-cutting reading rules: `ignore` never computes a CI (cheapest); `grey_non_signif` adds the
significance boolean; `signif_floor` adds the CI bounds (which subsume the boolean); `diff` and
`ratio` shown together share one cell-vs-reference CI; `contrib` needs the χ² decomposition and no
reference; the standardized-diff default additionally needs `sd_ref` (`var` of the reference cell),
which the absolute-diff mode does not.
