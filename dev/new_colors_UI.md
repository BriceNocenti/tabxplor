# tabxplor colours & breaks — new UI implementation brief

> **SUPERSEDED IN PART BY PHASE 13a (2026-07-14)** — this brief describes the Phase 5 framework; the
> Phase 13a redesign changed the public API and internals. Where they differ, Phase 13a wins (see
> CLAUDE.md > Phase 13a Done). Key changes: `color` grammar is now **position = channel, names = column
> type** (`c(pct=,mean=)` / `list(pct=,mean=)`; the `c(text=,background=)` form is REMOVED); the policy
> `"color_all_signif"` is renamed **`"guaranteed_effect"`**; break scales are
> `list(center,strict,std, over=list(breaks,slots), under=list(breaks,slots))` fed by signed/reciprocal
> literals or `list(over=,under=)` (the "×2 rule" is an over-only `pct_ratio`, no in-text slot-11); the
> palette is 8 OKLCH base colours (`set_color_palette()`, slots 1-4 over / 5-8 under; no
> `html_24_bit`/`set_color_style`); a per-table `color_breaks=` argument exists. The sections below are
> kept as historical intent.
>
> **ALSO SUPERSEDED IN PART BY LAST PHASE z4 (2026-08-05)** for the `contrib` measure only: its
> significance is now the ADJUSTED standardized residual (not the Pearson one this brief specified),
> on the package's inference base; and `guaranteed_effect` no longer scores a contribution at all --
> it scores that residual on a new, seventh, ABSOLUTE break scale `zscore`. The per-policy
> divergence is a `guar` override FIELD in the `MEASURES` fact table, read through the one
> `measure_facts(measure, policy)` accessor. Rationale, evidence and the rejected alternatives:
> `dev/chi2_cell_residuals_and_contributions.md`. The rows below are annotated "(z4)" where they moved.
>
> SINGLE STARTING POINT for implementing the redesigned colour/breaks framework (tabxplor 2.0.0,
> "Phase 5"). This file is self-contained: a fresh session should be able to implement from it alone.
> It records the WHY, the settled architecture, the full user-facing API, the statistics, the
> engine, the computation plan, phasing, and every remaining micro-choice / white-elephant flag.
>
> Companion (history + deeper rationale, NOT required reading): `dev/design_new_colors_UI_decision_process.md`
> (the layered decision log that led here — sections 3 = statistical audit, 12-14 = the design debate).
> Governing statistics already implemented in Phase 3a: `dev/tabxplor_2.0.0_decisions.md` §12/§14/§20.
> Skills to use while implementing: `/color-mode` (the colour pipeline) and `/vctrs-field` (widening
> the existing `color` attribute to hold up to two values — text, background).
>
> **STATUS (2026-07-09): IMPLEMENTED (Batch A + Batch B), Phase 5 complete.** The findInterval
> engine, five-scale breaks list, two-channel `color`/`color_signif` args, significance from the
> stored bounds, exporters + legend on the two channels, old-string soft-deprecation, and the docs
> all landed (full suite green). **Still open:** W4 (per-measure palette hues — only text/bg channels,
> not distinct hue ramps), W5 (coloured `tab_md`), and wiring the new args into `tab_many()` (Phase 6).
> The `col% + means` reference "fix" is RESOLVED as **intended behaviour** (Phase 7b): a mean's
> reference is meaningfully a row, a factor's under `pct="col"` a column — no clean fix without
> white-elephant UI, so it is documented (map doc §8), warn-only, not changed. The `color_type` export
> arg is now vestigial (selects the text-channel palette family only). See CLAUDE.md > Phase 5 for the
> done-record.

---

## 1. Why this exists (the three forces)

1. **Performance.** Colour is the #1 cost of console printing and `tab_kable()`. Today
   `fmt_color_selection()` builds one boolean vector per break, then `keep_last_break()` resolves them
   per cell with a `purrr::reduce` + `dplyr::mutate` + matrix transpose — run twice per column, once
   per coloured column, on every render, with no caching. It is O(cells) heavy-dplyr where a C-level
   `findInterval` would do. The rewrite replaces this with a vectorised engine (section 9).

2. **A combinatorial, cryptic API.** The current `color = <one string>` catalogue
   (`diff`, `diff_ci`, `after_ci`, `ci`, `contrib`, `OR`) conflates three independent choices into one
   flat list, cannot express two-quantity cells, colours means wrongly (`"diff"` on a mean produced a
   ratio), and offers no clean per-type behaviour. The redesign separates the three axes (section 3).

3. **Statistical soundness.** The CI-gated formulas carried FIXMEs (their percentage branches used the
   CI upper arm as if symmetric — wrong for the default asymmetric proportion intervals). Phase 3a now
   stores real bounds `ci_inf`/`ci_sup` and a per-cell `pvalue`; the colour engine must consume those
   (section 6/8), which makes every significance-gated mode correct by construction.

---

## 2. The architecture in one picture

Colouring is decomposed into **three orthogonal user choices** plus **two configuration layers**, all
feeding **one vectorised engine**:

```
USER CHOICES (per tab() call)
  color        = WHAT is measured, and on WHICH visual channel   (measure x channel)
  color_signif = HOW significance gates/scales the colour         (policy)
  conf_level   = the confidence level = ONE significance threshold (expert; default 0.95)

CONFIG LAYERS
  breaks       = HOW BIG is "big"  -> number & size of colour steps   (global option + per-table override)
  palette      = WHICH HUES, per output medium                        (global, render-time only)

ENGINE (render time, per column, per channel)
  score(cell)  -> findInterval(score, signed_breaks) -> level -> palette slot -> hex/crayon
  (significance folded in via the stored ci_inf/ci_sup bounds; see section 8)
```

Key invariant preserved from today: **colour is computed lazily at render time** from stored fields;
the SAME table object renders correctly to console / HTML / Excel / ggplot, each with its own palette.
Therefore the palette is always global/render-time; breaks default globally but may be pinned per
table (only thresholds are baked into the object, never hues — section 7).

---

## 3. The three orthogonal axes (the core idea)

A coloured cross-tab answers three reader questions; each is one axis:

- **Axis M — measure** (direction + magnitude): `diff`, `ratio`, `contrib`, `or`.
- **Axis C — channel** (where drawn): `text` colour, `background` fill. (Two, because a cell reliably
  has only these two across all media.)
- **Axis S — significance policy** (reliability): `ignore`, `grey_non_signif`, `color_all_signif`.

The old flat catalogue becomes a composition of these — e.g. old `diff_ci` = measure `diff` +
policy `grey_non_signif`; old `after_ci` = measure `diff` + policy `color_all_signif`; the old ×2
highlight = measure `diff` on text + measure `ratio` on background. Nothing is lost; nothing is a
memorised string.

---

## 4. The user-facing API (final)

### 4.1 `color` — measure(s) and channel(s)

`color` is opt-in (default `FALSE`). It accepts:

| form                                         | meaning                                         |
|----------------------------------------------|-------------------------------------------------|
| `FALSE`                                      | no colour (default)                             |
| `TRUE`                                       | the smart per-type default scheme (section 4.4) |
| `"diff"` (scalar)                            | measure `diff` on the **text** channel          |
| `c("diff","ratio")` (positional len-2)       | `diff` on **text**, `ratio` on **background**   |
| `c(text="diff", background="ratio")` (named) | explicit channel assignment                     |
| `c(background="ratio")` (named len-1)        | `ratio` on **background** only, text empty      |

Rules:

- Position/naming picks the channel; `text` is the primary/default channel (a scalar or unnamed
  single value goes to text). A named `background=` puts a measure on the fill with no text colour.
- **Two different graded measures cannot share one channel** (one text colour cannot encode two
  quantities). So showing `diff` AND `ratio` together requires two channels — but a LONE `ratio` can
  go on text (`color = "ratio"`) or background (`color = c(background="ratio")`); it is not forced to
  fill. (This corrects an earlier draft that said ratio is always fill.)
- Measures: `diff`, `ratio` may go on either channel; `contrib`, `or` are whole-cell measures and
  default to **text only** (background allowed but unusual — flag W6).
- A measure not listed in `color` never appears anywhere in the table (coarse on/off — section 7.4).

Measure meaning is **auto-dispatched by column type** (the user's vocabulary is type-free):

| measure   | factor / % column                        | numeric / mean column                                                                    | notes                           |
|-----------|------------------------------------------|------------------------------------------------------------------------------------------|---------------------------------|
| `diff`    | pp difference vs reference               | standardized mean difference (Glass's Δ) by default; absolute if unit breaks given (7.3) | text or background              |
| `ratio`   | relative risk `p_cell/p_ref`             | mean ratio `m_cell/m_ref`                                                                | text or background              |
| `contrib` | signed χ² contribution (vs independence) | n/a                                                                                      | text; symmetric, reference-free |
| `or`      | empirical odds ratio                     | n/a                                                                                      | text; `tab_logit`               |

### 4.2 `color_signif` — the significance policy

Separate scalar argument, default `"ignore"`. Three values:

| value                | what it does (usage-first — the docs must teach usage)                                                                                                                                                                                |
|----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `"ignore"`           | no significance test; colour every deviation by its **observed** size. Exploratory/descriptive, or large samples. (= old `diff`.)                                                                                                     |
| `"grey_non_signif"`  | colour by the **observed** size, but grey out (leave neutral) cells whose deviation is not significant. "Read the real effect sizes, hide the noise." (= old `diff_ci`.)                                                              |
| `"color_all_signif"` | colour by the **guaranteed** (CI-floored, conservative) effect; only cells whose confidence bound clears the threshold show, and colours are dimmer than observed. Small samples / "show only what I can defend." (= old `after_ci`.) |

Precise difference between the two significance policies (put this in the docs, because the NAMES do
not convey it — flag W1): both colour only significant cells; `grey_non_signif` keeps the OBSERVED
magnitude's colour, `color_all_signif` uses the GUARANTEED (CI-floor) magnitude's colour. Same-ish
cell set, different intensity basis.

Significance itself is the section-8 primitive `ci_inf > 0` (over) / `ci_sup < 0` (under) — sound for
every method, identical across % and means, never contradicting the printed bracket or the stars.

### 4.3 `breaks` — thresholds (global default + per-table override), passed as a named list

Breaks are a **named list** of positive-only scales (auto-mirrored). Set globally, or override for one
table. Scales not supplied keep their default.

```r
set_color_breaks(list(                    # GLOBAL default (render-time; affects all tables)
  pct_diff   = c(0.05, 0.1, 0.2, 0.3),    # factor pp-difference (text)
  pct_ratio  = c(2),                       # factor relative risk (the lone x2 rule; longer = a ramp)
  mean_diff  = NULL,                       # numeric difference: NULL -> standardized default (7.3)
  mean_ratio = c(1.15, 1.5, 2, 4),         # numeric mean ratio
  contrib    = c(1, 2, 5, 10)              # chi2 contribution
))

tab(d, region, sex, color = c("diff","ratio"),
    color_breaks = list(pct_ratio = c(1.3, 1.5, 2, 3, 4)))   # THIS table only: full ratio ramp
```

- **The five scales:** `pct_diff`, `pct_ratio`, `mean_diff`, `mean_ratio`, `contrib`. Each independently
  global or per-table.
- **Length = number of colour steps.** `pct_ratio = c(2)` → one ×2 colour; `pct_ratio = c(1.5,2,3,4)`
  → a graded ramp. This is how "8 diff + 1 ratio" vs "8 diff + 8 ratio" differ — only the length of
  `pct_ratio`.
- **Per-type on/off (fine lever, under `color`).** An empty/`NULL` scale drops that measure FOR THAT
  COLUMN TYPE, even when the measure is listed in `color`. Example: `color = c("diff","ratio")` +
  `mean_diff = NULL` → factors show diff+ratio, numerics show ratio only (diff not computed for
  numerics). This is the ONLY way to get per-type differences, because `color` is globalized
  (section 7.4).
- **`mean_diff` default = standardized** (Glass's Δ, section 7.3). Supplying `mean_diff` in DATA UNITS
  (e.g. `c(200,500,1000,2000)` euros) switches numeric diff to ABSOLUTE colouring.

### 4.4 `color = TRUE` — the smart default, resolved per type

`color = TRUE` is sugar, resolved PER COLUMN TYPE at build time (it is NOT a literal
`c("diff","ratio")` — see the numeric nuance below):

- **factor / %:** `text = diff`, `background = ratio` with `pct_ratio = c(2)` → the modernised classic
  look: a diff text ramp + the single ×2 highlight on the background.
- **numeric / mean:** `text = ratio` (diff off for numerics — `mean_diff` not part of TRUE's numeric
  resolution). Ratio goes on **text** because it is the sole measure for numerics.
- **odds-ratio / logit columns:** `text = or`.
- **counts:** no colour.

Why per-type sugar, not a literal vector: a literal positional `c("diff","ratio")` on a numeric with
`mean_diff = NULL` would leave `diff`'s text channel empty and `ratio` on the background (numeric cells
background-only). `TRUE` instead puts numeric ratio on text (more prominent), matching the maintainer's
"numeric default = ratio". So `TRUE` ≈ `c("diff","ratio")` for factors, and `= "ratio"` for numerics.
(This asymmetry is intentional; flag W2.)

### 4.5 `palette` — hues per medium (global, render-time)

`set_color_palette()` (global only) provides **diverging colour ramps** (negative pole → neutral →
positive pole), sampled at the break count. Measure-grouped so different measures read differently
(section 10). Start with two groups × two channels (text saturated, background pale) × theme
(light/dark, auto). Colourblind-safe by default (blue-red diverging).

### 4.6 `conf_level` — one confidence level for ALL significance

`conf_level` is the existing `tab()`/`tab_ci()`/`tab_counts()` argument (default `0.95`; also the
Jamovi option). It is the SINGLE significance threshold for the whole table — every significance
decision derives from it, so nothing can disagree:

- the CI bounds `ci_inf`/`ci_sup` for `diff`/`ratio` (Newcombe / Welch-t) and for `or` (log-OR Wald);
- the `contrib` standardized-residual threshold `z(conf_level) = qnorm(1 - (1-conf_level)/2)`
  (= 1.96 at 0.95), via the existing `zscore_formula()` helper — **never a hardcoded 1.96**;
- the significance stars.

So the user's mental model is a clean three-step: **(1) choose the MEASURE(s) and channel (`color`);
(2) [expert] choose the CONFIDENCE (`conf_level`); (3) choose HOW significance is displayed
(`color_signif`).** One confidence level ⇒ the printed CI bracket, the stars, the greying
(`grey_non_signif`), and the contrib residual all agree, for every measure. `conf_level` is
orthogonal to `color`/`color_signif` and needs no colour-specific plumbing — the colour engine just
reads the bounds/threshold that `conf_level` already produced.

---

## 5. The composition matrix (measure × policy)

All 12 combinations are valid. The only per-measure variation is WHICH significance test drives the
`grey_non_signif` / `color_all_signif` policies.

| measure ↓ / policy → | `ignore`                   | `grey_non_signif`                                | `color_all_signif`           |
|----------------------|----------------------------|--------------------------------------------------|------------------------------|
| `diff`               | observed diff, all cells   | observed diff, grey where diff-CI ∋ 0            | diff CI-floor (conservative) |
| `ratio`              | observed ratio, all cells  | observed ratio, grey where cell=ref not rejected | ratio CI-floor               |
| `or`                 | observed OR, all cells     | observed OR, grey where OR-CI ∋ 1                | OR CI-floor                  |
| `contrib`            | χ² contribution share, all | contribution, grey where \|resid\| < z(conf_level)        | the RESIDUAL itself, `zscore` scale (z4) |

Significance source per measure:

- **`diff` and `ratio` share one test** — "is the cell different from its reference?" (cell-vs-ref
  difference CI: Newcombe for %, Welch-t for means). `ratio` significant ⇔ `diff` significant. So a
  diff-text + ratio-background table computes that CI ONCE. Only `color_all_signif` needs each
  channel's own-scale bound (additive for diff, multiplicative for ratio).
- **`or`** uses its own odds-ratio CI (log-OR Wald; the `tab_logit` model CI later). This is why OR
  gets all three policies (the maintainer's correction to an earlier claim).
- **`contrib`** uses the **ADJUSTED standardized residual** (Haberman) — `|z| > z(conf_level)`, via
  `zscore_formula()`, never a hardcoded 1.96. **UPDATED Last Phase z4:** this said "Pearson residual"
  and the code implemented it, which was wrong: `(o-e)/sqrt(e)` has variance `(1-p_i)(1-p_j) < 1`, so
  testing it at 1.96 under-rejects (measured 1.10-3.09x too strict). Only the adjusted residual is
  ~N(0,1). It is also computed on the package's inference base (unweighted `n`, or Kish `n_eff`), not
  on the weighted N, and cells with an expected count below 1 get no residual at all.

What is special about `contrib`: it is the only measure that tests against **independence** (both
margins, symmetric in rows↔columns, reference-free), whereas `diff`/`ratio`/`or` test against a chosen
**reference**. That symmetry makes `contrib` the natural (only) way to colour **counts** (a raw count
has no reference). So "colour the counts by significance" = `color = "contrib",
color_signif = "grey_non_signif"`.

---

## 6. Significance measures & the old `ci` mode

The old binary `ci` mode ("colour significant cells, one shade per direction, ignore magnitude") is:

```r
tab(d, region, sex, color = "diff", color_signif = "color_all_signif",
    color_breaks = list(pct_diff = c(0)))   # single break at 0 = significant vs not, by direction
```

The single break `0` is consistent across directions: the engine colours the positive half when
`ci_inf > 0` and the negative half when `ci_sup < 0` — both mean "the CI clears 0"; only which bound is
relevant differs (inf for over-representation, sup for under-). So `0` is the symmetric significance
boundary, not ambiguous. One positive break → one shade per direction → the old `ci` look.

(A `color_signif = "signif_only"` alias that hard-wired the single-0 break was considered and
REJECTED — not worth the extra name; the explicit break above suffices. See W9.)

---

## 7. Breaks in detail

### 7.1 The five scales, defaults, mirroring

| scale        | applies to                    | default                                          | scale kind                         |
|--------------|-------------------------------|--------------------------------------------------|------------------------------------|
| `pct_diff`   | factor diff (pp)              | `c(0.05, 0.1, 0.2, 0.3)`                         | additive, mirror `c(x, -x)`        |
| `pct_ratio`  | factor ratio (RR)             | `c(2)`                                           | multiplicative, mirror `c(x, 1/x)` |
| `mean_diff`  | numeric diff                  | `NULL` → standardized `c(0.2, 0.5, 0.8)` (Glass) | additive (or standardized)         |
| `mean_ratio` | numeric ratio; also OR breaks | `c(1.15, 1.5, 2, 4)`                             | multiplicative                     |
| `contrib`    | χ² contribution               | `c(1, 2, 5, 10)`                                 | additive-ratio-to-mean             |
| `zscore`   | χ² adjusted std. residual     | `conf_level_to_z(c(.95,.99,.9999,1-2e-9))`       | additive, in z units (z4)          |

Mirroring is done once at set time (precompute the signed, sorted vector the engine's `findInterval`
reads). Additive scales mirror `c(x, -x)`; multiplicative scales mirror `c(x, 1/x)` centred at 1.

### 7.2 Hybrid global + per-table override

- Global: `set_color_breaks(list(...))` writes `options("tabxplor.color_breaks")`.
- Per-table: `tab(..., color_breaks = list(...))` supersedes the named scales for that table only,
  stored as a **table-level attribute** (like `subtext`/`test`) and read at render time. Only
  thresholds are baked — the palette is still resolved per medium, so multi-medium rendering and
  re-theming still work. (The maintainer initially declined the hybrid, then adopted it; it is safe
  precisely because it bakes thresholds, not hues.)

### 7.3 Numeric diff: standardized default, absolute on unit breaks

There is no universal absolute unit for a mean difference, so:

- **Default (`mean_diff = NULL`):** colour the STANDARDIZED difference Glass's Δ = `diff / sd_ref`
  (`sd_ref` = sqrt of the reference cell's weighted `var`) against `c(0.2, 0.5, 0.8)` (small/medium/
  large). The legend states the SD-based thresholds themselves (e.g. "coloured by (cell − ref)/sd(ref);
  small/medium/large = 0.2/0.5/0.8 SD"). It does NOT translate them into the column's data units —
  that would have to be per column, and even per subtable when `tab_vars` are present (`sd_ref`
  varies), a big white elephant (see W7). Users who want real-unit thresholds use the absolute mode.
- **When `mean_diff` is supplied in DATA UNITS** (e.g. `c(200,500,1000,2000)`): colour the ABSOLUTE
  `diff` directly against those thresholds — no standardization, no `sd_ref`. Legend prints the literal
  units.

NOTE the current (pre-Phase-5) behaviour, for context: numeric `color="diff"` reads the `ratio` field
against `mean_breaks` interpreted as ratios, so passing `c(200,...)` does nothing sensible. The rule
above is the fix.

The "universal effect-size ladder for ALL measures" idea from the design debate is **dropped** (flag:
it was a white elephant). Each scale keeps its own natural default: raw pp for `pct_diff`, ratios for
`pct_ratio`/`mean_ratio`, SD-standardized only for `mean_diff` (out of necessity). Small-base
sensitivity that a universal Cohen's-h ladder would have given is instead delivered by the ratio
channel (the ×2 rule / ratio ramp on background) — the coherent two-channel story.

### 7.4 On/off hierarchy (coarse `color`, fine per-type breaks)

Two hierarchized levers — NOT redundant (this resolves an earlier objection):

1. **Coarse (`color`):** a measure absent from `color` never appears in the table, any column.
2. **Fine (per-type break scale = empty/`NULL`):** for a measure that IS in `color`, an empty scale
   drops it for that COLUMN TYPE only. Since `color` is globalized (one setting for the whole table),
   the per-type scales (`pct_*` vs `mean_*`) are the only place per-type presence/absence lives.

Document the distinction so users don't confuse "no colour at all" (`color`) with "this measure off
for this type" (empty scale).

---

## 8. The statistics (consume Phase 3a; do not re-derive)

### 8.1 The one significance primitive

```r
sig_pos <- get_ci_inf(x) > 0     # difference interval entirely above the reference
sig_neg <- get_ci_sup(x) < 0     # difference interval entirely below the reference
```

Use the stored BOUNDS (always present), not `pvalue` (NA when stars are off). Sound for every method
(Wilson/Newcombe/AC/Wald/z/Welch-t), identical across % and means, dual to the printed bracket & stars.
The bounds are computed at the table's `conf_level` (§4.6); the `contrib` residual threshold uses the
matching `z(conf_level)` — so all significance in the table shares one confidence level.

### 8.2 Per-measure colour formulas

`q` = the measure's per-cell quantity (pct: `get_diff`; pct ratio: `get_ratio`; numeric diff:
`get_diff/sd_ref` standardized, or raw `get_diff` if absolute breaks; numeric ratio: `get_ratio`).

| measure   | `ignore` (pos / neg)                          | `grey_non_signif`                         | `color_all_signif`                                       |
|-----------|-----------------------------------------------|-------------------------------------------|----------------------------------------------------------|
| `diff`    | `q > brk` / `q < brk`                         | `q > brk & sig_pos` / `q < brk & sig_neg` | `ci_inf > brk` / `ci_sup < brk` (÷ `sd_ref` for numeric) |
| `ratio`   | `ratio > brk` / `ratio < brk` (recip. breaks) | `+ sig_pos/sig_neg` (shared cell-vs-ref)  | ratio CI-floor vs breaks                                 |
| `or`      | `or > brk` / `or < brk` (recip.)              | `+ OR-CI excludes 1`                      | OR CI-floor vs breaks                                    |
| `contrib` | `ctr >= brk*mean_ctr` / `ctr <= brk*mean_ctr` | `+ abs(resid) > z(conf_level)`                        | the residual `z` vs the `zscore` scale, anchored at `z(conf_level)` (z4) |

For symmetric intervals (means; Wald/AC %) the corrected forms reduce EXACTLY to today's algebra, so
mean colouring is byte-identical; the % CI-gated modes change (they fix the old upper-arm-asymmetry
bug). Factor `diff` with default breaks stays byte-identical.

---

## 9. The vectorised engine (replaces the hotspot)

Per column, per channel:

```r
fmt_color_plan(x, channel)   # -> list(measure, score = <per-cell numeric>, gate = <per-cell lgl>,
                             #         breaks = <signed sorted>, slot_map = <level -> palette slot>)
fmt_color_slots(x, plan)     # slot <- slot_map[findInterval(score, breaks) + 1L]; slot[!gate] <- 0L
fmt_color_channels(x)        # -> list(text_slot, bg_slot), each integer(length(x)); 0 = uncoloured
```

- `findInterval` (C-level) replaces the per-break `pmap` + `keep_last_break` per-cell reduce/transpose.
- `gate` carries the significance mask for `grey_non_signif`/`color_all_signif` (from section 8.1),
  and `!is_totrow` for `contrib`.
- `slot_map` is a precomputed integer lookup (level → palette slot) — replaces `select_in_color_style`
  and its fragile hex-sniff; the channel type is passed explicitly.
- Every consumer (console `pillar_shaft`, `fmt_get_color_code`, `tab_kable`, `tab_plot`, `tab_xl`,
  legend) maps the `(text_slot, bg_slot)` pair to crayon/hex: text via crayon fg / `cell_spec(color=)`
  / openxlsx `fontColour`; background via crayon bg / `cell_spec(background=)` / openxlsx `fgFill`
  (stacked `addStyle`). `tab_md` gains colour via short pandoc span classes (optional; flag W5).
- Delete after migration: `keep_last_break`, `color_formula`, `select_in_color_style`, the dead
  `*_brksup` code. Keep a thin `fmt_color_selection` shim only while `expect_color()` migrates.

### 9.1 Storage — one per-column attribute with max two values

The two channels live in the EXISTING single `color` per-column attribute, now allowed to hold ONE or
TWO values: `"diff"` (text only) or `c("diff","ratio")` (text + background). **No new attribute** — this
is lighter than adding a separate `color_bg`.

- `get_color(x)` returns the TEXT channel = `attr(x, "color")[1]` — unchanged scalar contract, so the
  ~15 `if (color == ...)` consumer sites and existing user code keep working untouched.
- `get_color_bg(x)` returns the BACKGROUND channel = `attr(x, "color")[2]` — which is `NA` when the
  attribute has length 1 (R returns `NA` for an out-of-range index, silently, no error).
- **Never call `length()` on the hot path** (it costs compute): just index `[1]` and `[2]`. `[2]` on a
  length-1 vector is `NA`; treat `NA`/`""` as "no background channel". Do not require, pad, or normalize
  the length; if a second value is absent, simply do not use one.
- `set_color(x, color)` stores the length-1-or-2 vector as-is (validating each element against the
  measure whitelist). It accepts a scalar, an unnamed `c(text, background)`, or a named
  `c(text=, background=)`.
- vctrs plumbing is a WIDENING of the existing attribute, not a new one, so `/vctrs-field` applies but
  nothing is added to `new_fmt`'s attribute list. The `color_x == color_y` reconciliation in
  `vec_ptype2.tabxplor_fmt`/`vec_cast` recycles cleanly for lengths 1 and 2 (2 is a multiple of 1, so
  there is no unequal-length error — the earlier "cannot store a length-2 vector" objection only bit
  lengths that are not multiples, which cannot occur with a max of 2). Keep the existing
  `if_else(same, color, "")` reconciliation shape; it yields a length-≤2 reconciled attribute.

---

## 10. Palettes — measure-group diverging ramps

Distinct hues per measure so a glance distinguishes measures; one diverging ramp per GROUP,
strength-graded, with text (saturated) and background (pale) variants, colourblind-safe.

- **Additive group** (`diff`, `contrib`): the current light-text 24-bit **red → blue** diverging ramp
  + a paler background counterpart.
- **Multiplicative group** (`ratio`, `or`): a **orange↔purple** diverging ramp (ColorBrewer PRGn
  style) — **purple** for ratio > 1 (keeps the current ×2 purple you like) and more **orange** for
  ratio < 1 — same luminance ramp for intensity, plus a paler background counterpart.

This pairs naturally with the common two-channel case (`diff` additive on text + `ratio` multiplicative
on background): different channel AND different hue family → doubly distinguishable. Within a group the
two measures never co-occur in one table (one intent per table after Phase 6), so sharing is safe.

The hard part (flag W4): 2 (eventually 4) diverging scales that are mutually distinct AND internally
step-distinct is a real perceptual task. Guidance: fix a common LUMINANCE ramp for intensity, vary HUE
between groups; validate with the `dataviz` skill's diverging-palette method + a colourblind check; cap
~5 steps/side. Defer the full 4-palette split (a per-measure palette each for diff/ratio/or/contrib)
until the 2-group version is validated on real tables. Fix the `set_color_style(custom_palette=)`
length bug in passing (accept the new ramp lengths).

---

## 11. Back-compatibility & deprecation

Old `color=` strings are soft-deprecated and rewritten to `(color, color_signif)` (+ ratio channel for
the ×2):

| old `color=`       | new `color` | new `color_signif`   | note                                                                                                          |
|--------------------|-------------|----------------------|---------------------------------------------------------------------------------------------------------------|
| `"diff"` (factor)  | `"diff"`    | `"ignore"`           | old in-text ×2 moves to background (default scheme)                                                           |
| `"diff_ci"`        | `"diff"`    | `"grey_non_signif"`  |                                                                                                               |
| `"after_ci"`       | `"diff"`    | `"color_all_signif"` |                                                                                                               |
| `"ci"`             | `"diff"`    | `"color_all_signif"` | + `color_breaks = list(pct_diff = c(0))` (section 6)                                                          |
| `"contrib"`        | `"contrib"` | `"ignore"`           |                                                                                                               |
| `"OR"`/`"or"`      | `"or"`      | `"ignore"`           |                                                                                                               |
| `"diff"` (numeric) | `"ratio"`   | `"ignore"`           | SEMANTIC CHANGE: old numeric "diff" was ratio-colouring; new numeric `"diff"` is Glass's Δ. One-time message. |

Old break arg names (`pct_breaks`, `mean_breaks`, `contrib_breaks`) soft-deprecated and wired into the
new list: `pct_breaks` splits into `pct_diff` (values ≤ 1) + `pct_ratio` (values > 1); `mean_breaks` →
`mean_ratio`; `contrib_breaks` → `contrib`. Keep `get_color`/`set_color`, `fmt_get_color_code`,
`$diff`/`$ratio`, and the scalar `color=` values working.

---

## 12. Computation matrices (what `tab()` must compute)

`tab()`/`tab_many()` reads `(color, color_signif, breaks)` FIRST, computes the union of what the active
measures + policy need, stores them in `tabxplor_fmt` fields; the engine only reads fields.

### 12.1 Percentages (col_var is a factor)

Available measures: `diff` (pp, text), `ratio` (RR), `contrib`, `or`. CI: Newcombe (diff) / residual
(contrib) / log-OR (or).

| measure   | `ignore`                   | `grey_non_signif`          | `color_all_signif`    |
|-----------|----------------------------|----------------------------|-----------------------|
| `diff`    | ref + `diff`               | + prop-diff CI → `sig`     | + prop-diff CI bounds |
| `ratio`   | ref + `ratio`              | shares the prop-diff `sig` | + RR CI bounds        |
| `contrib` | χ² decomp (`ctr`, margins) | + std. residuals → `sig`   | + residuals graded    |
| `or`      | ref2 + `or` (2×2 counts)   | + log-OR CI → `sig`        | + log-OR CI bounds    |

### 12.2 Means (col_var is numeric)

Available measures: `diff` (text; standardized or absolute), `ratio`. No `contrib`/`or`. CI: Welch-t.

| measure                        | `ignore`                        | `grey_non_signif`          | `color_all_signif`                 |
|--------------------------------|---------------------------------|----------------------------|------------------------------------|
| `diff` (standardized)          | ref + `diff` + `sd_ref` (`var`) | + mean-diff CI → `sig`     | + mean-diff CI bounds (÷ `sd_ref`) |
| `diff` (absolute, unit breaks) | ref + `diff`                    | + mean-diff CI → `sig`     | + mean-diff CI bounds              |
| `ratio`                        | ref + `ratio`                   | shares the mean-diff `sig` | + ratio CI bounds                  |

Cross-cutting: `ignore` computes no CI (cheapest); `grey_non_signif` adds the significance boolean;
`color_all_signif` adds the CI bounds (which subsume the boolean); `diff`+`ratio` together share one
cell-vs-ref CI; `contrib` needs the χ² decomposition and no reference; standardized `diff` additionally
needs `sd_ref`.

The `tabxplor_fmt` fields already exist (Phase 1a-3a): `diff`, `ratio`, `mean`, `var`, `ci_inf`,
`ci_sup`, `pvalue`, `ctr`, `or`, `tot_n`, `in_totrow`/`in_tottab`/`in_refrow`. Two items this
section once listed as TODO are now DONE (audited Phase 7b — the code was ahead of this doc):

- `get_ref_var()` **exists** (`R/fmt_class.R`, mirror of `get_ref_means`) and is used for
  `sd_ref = sqrt(get_ref_var(x))` (Glass's Δ).
- the pct `ratio` field is **already repointed** to the reference-relative RR `p_cell/p_ref` (the ×2
  driver) and `mean = NA` for pct columns — the `mean`-overload is gone (Phase 5 Batch A/B). It no
  longer holds the leftover column-referenced `tabs_rr`.

---

## 13. Implementation phasing (safety net FIRST)

- **Step 0 — Colour characterization golden (no code change).** Colours are absent from all current
  snapshots (`tab_md` is monochrome; `.rds` store fields). Add `test-color-golden.R` +
  `dev/make_color_golden.R` capturing `fmt_get_color_code()` per-cell hex AND the console ANSI of
  `pillar_shaft`/legend across {measure} × {factor/mean} × {text/bg} × {theme} × {24-bit}. Commit on
  current source. This makes "byte-identical factor diff" mechanically checkable.
- **Step 1 — Breaks list model** (`set_color_breaks(list(...))`, the five scales, mirroring, the
  `color_breaks=` table override, old-arg shim, `.onLoad` seed).
- **Step 2 — Palettes** (measure-group diverging ramps, text/bg, custom-palette length fix, explicit
  channel type; defaults reproduce today's factor look as closely as the bg-for-ratio change allows).
- **Step 3 — Engine, text channel** (`fmt_color_plan`/`fmt_color_slots`/`slot_map`; route
  `pillar_shaft` + `fmt_get_color_code`). Gate: factor `diff` byte-identical; consciously regenerate
  the numeric-diff (Glass) and corrected % CI-mode goldens.
- **Step 4 — widen the `color` attribute to two values + two channels** (`/vctrs-field`; `get_color`
  = `[1]`, new `get_color_bg` = `[2]`; the `color`/`color_signif` parsing + validation; wire the
  background channel through all consumers). See §9.1.
- **Step 5 — Legend + exporters two-channel** (`tab_kable`/`tab_plot`/`tab_xl`/legend; export-parity;
  optional `tab_md` spans).
- **Step 6 — Delete dead code; docs** (`@param color`/`color_signif`, `devtools::document()`, NEWS,
  `/color-mode` skill, `dev/tabxplor_architecture.md`).

Each step: golden + parity green; before/after benchmarks to `dev/benchmarks/results_2.0.0/`.

Benchmarks: micro `fmt_color_channels(col)` vs the old chain on tall single columns (500 / 5,000 rows,
all measures, `bench::mark` time + `mem_alloc`); wide-table column scaling; end-to-end coloured
`print()` on gss_cat and the 8M `big_df`; `tab_xl()`/`tab_kable()`. The win is removing
`keep_last_break`'s per-cell reduce.

### 13.1 Test coverage to build (currently MISSING — a real gap, not just a golden)

There is today **no testthat coverage** for how breaks/palettes are written, for the config
validators, or for the statistical/rendering edge cases — `test-tab.R`'s `expect_color()` only asserts
"≥ 1 coloured cell". Build these as first-class tests, not merely the Step-0 characterization golden.
Write the validation and engine-edge-case tests BEFORE the rewrite, so the new engine is coded against
a spec rather than "looks right".

- **Config-input validation** (`test-color-config.R`): `set_color_breaks(list(...))` /
  `tab(color_breaks=)` and `set_color_palette()` accept the good cases and give clear `cli` errors on
  the bad ones — wrong types, negative/zero/`NA` breaks, non-monotonic breaks, too many breaks (>
  palette steps), unknown list names, ratio breaks ≤ 1, a custom palette of the wrong length/format,
  an unknown `color` / `color_signif` value, a `color` vector longer than 2, `contrib`/`or` on the
  background channel. Also: per-table `color_breaks=` overrides the global; unsupplied scales fall
  back to global/default; an empty scale drops that measure for that column type (§7.4).
- **Engine edge cases** (`test-color-engine.R`): all-`NA` column; single-row / single-column table; a
  cell equal to its reference (`diff == 0`, `ratio == 1` → neutral/uncoloured); empty cells
  (`n == 0`); `NA` CI bounds (→ never significant → `grey_non_signif` greys, `color_all_signif` drops);
  `ratio` with `ref_pct == 0` / `ref_mean == 0` (division → `Inf`/`NaN` → uncoloured, no crash);
  standardized `diff` with `sd_ref == 0` or `NA` (Glass Δ undefined → uncoloured); a length-1 break
  vector (single shade) and the max length; the significance boundary (`ci_inf` exactly 0);
  `findInterval` at exact break values (tie side). Assert the resulting per-cell slot INTEGERS, not
  just "some colour".
- **Statistical correctness in edge cases** (extend `test-calculations.R`): the gate
  `ci_inf > 0` / `ci_sup < 0` agrees with `pvalue < 1 - conf_level` for every method
  (Wilson/Newcombe/AC/Welch-t/log-OR); the `contrib` residual threshold equals `z(conf_level)` and
  tracks `conf_level` (0.90 / 0.95 / 0.99); standardized `diff == diff / sqrt(ref var)`; absolute-diff
  mode colours the raw `diff`; `diff` and `ratio` on two channels use the SAME significance boolean.
- **Rendering / two-channel** (extend `test-exports.R`): a two-channel cell yields BOTH a text and a
  background colour in console, `tab_kable`, and `tab_xl`; `color = c(background="ratio")` colours only
  the background; export parity (`format` vs the `tab_xl` bypass) holds for every measure.
- **Back-compat** (extend `test-tab.R`): each deprecated `color=` string (`"diff_ci"`, `"after_ci"`,
  `"ci"`, `"OR"`, numeric `"diff"`) is rewritten to the correct `(color, color_signif[, breaks])` pair
  and emits the `deprecate_soft` message once; old break arg names map correctly.

Together with the Step-0 characterization golden (byte-identity) and the in-suite benchmark, these are
the full safety net.

---

## 14. Remaining inconsistencies, unsettled micro-choices, white-elephant flags

- **W1 — policy names don't telegraph the B/C difference.** `grey_non_signif` vs `color_all_signif`
  both mean "only significant coloured"; their real difference (observed vs guaranteed intensity) lives
  only in the docs. Accepted by the maintainer (repetition over misunderstanding). Watch whether
  non-experts confuse them; consider documenting by usage ("explore / focus / small-sample-honest").
- **W2 — `color = TRUE` is per-type sugar, not a literal `c("diff","ratio")`.** For numerics it yields
  `ratio` on TEXT (diff off), not `ratio` on background. Intentional but a subtle asymmetry to
  document. Micro-choice: confirm numeric ratio belongs on text (recommended) vs background.
- **W3 — B vs C may be a near-duplicate** for many users (same coloured set, subtle intensity
  difference). Kept because `color_all_signif` serves small-sample honesty. Reconsider if it proves
  confusing in practice.
- **W4 — the 2→4 palette perceptual design is unfinished.** Only the additive/multiplicative split is
  specified; the hand-tuned hues and the eventual 4-way split need real colour-design work + a
  colourblind pass.
- **W5 — coloured `tab_md`** (pandoc span classes) is proposed but optional; it also unlocks Jamovi
  HTML colouring. Decide whether to build now or defer.
- **W6 — `contrib`/`or` on the background channel** is allowed but unusual; and cross-frame cells (e.g.
  `diff` vs a reference on text + `contrib` vs independence on background) are coherent but niche.
  Default them text-only; decide whether to even permit background for them.
- **W7 — RESOLVED (abandoned).** Translating the standardized-diff legend into data units
  ("0.5 SD ≈ ±X €") would have to be per column, and even per subtable with `tab_vars` (`sd_ref`
  varies) — a big white elephant. The legend shows the SD-based thresholds themselves instead (§7.3);
  real-unit thresholds are the absolute mode. (Still confirm the fallback when `sd_ref` is NA/0 → cell
  uncoloured.)
- **W8 — empty-scale-as-off must be taught** so it isn't confused with `color`'s coarse on/off. It is
  the only per-type mechanism (since `color` is global), so it earns its place, but it is implicit.
- **W9 — RESOLVED (rejected).** A `color_signif = "signif_only"` alias (hard-wiring the single-0 break
  for the old `ci` look) is NOT worth the extra name; users write the explicit break (§6).
- **W10 — dropped white elephant (recorded so it is not re-introduced):** the "universal effect-size
  strength ladder for all measures" (Cohen's h/d unifying pct and mean breaks). Dropped in favour of
  per-scale natural defaults; small-base sensitivity is delivered by the ratio channel instead.
- **W11 — multiple comparisons.** All significance is per-cell at `conf_level`; a large table has many
  cells, so naive Type-I inflation applies to `grey_non_signif`/`color_all_signif`/`contrib` alike.
  This matches the existing per-cell CI/stars behaviour (no correction today), so it is consistent —
  but note it in the docs. A future opt-in adjustment (e.g. per-table Bonferroni/BH) would slot in at
  the `conf_level` layer, uniformly across measures; out of scope for Phase 5.

---

## 15. Key files & references

- `R/fmt_class.R` — engine to replace (`fmt_color_selection` 2053, `color_formula` 2341,
  `keep_last_break` 2273, `select_in_color_style` 2805), `get/set_color` 908-948, `fmt_get_color_code`
  973, `pillar_shaft` 1902, legend 2450-2785, reference helpers 1280-1326 (add `get_ref_var`),
  `new_fmt`/vctrs methods 1015/3087/3177/3339 (widen the existing `color` attribute to length ≤ 2;
  `get_color` = `[1]`, add `get_color_bg` = `[2]` — no new attribute).
- `R/tab_classes.R` — palettes 2906-3058, `set_color_style` 3082 (custom-palette length bug 3102),
  `get_color_style` 3126, `set_color_breaks` 3228, `get_color_breaks` 3399, `tab_kable` 585-659,
  `tab_plot` 1332-1361.
- `R/tab.R` — colour arg resolution/vectorization 741/820-886/1027-1057; field build
  `diff=..4`/`mean=..5`/`ratio=..6` + colour `case_when` 2654-2694; pct ratio build 2432-2549.
- `R/tab_xl.R` — `colorToStyle` 226-250, conditional-fmt map 1216-1260.
- `R/tab-agg.R` — sound CI/bounds/pvalue source (consume, no change).
- `R/utils.R` — `.onLoad` breaks/style seed 49-55.
- Tests — `tests/testthat/test-golden.R` + `helper-golden.R` (add the Step-0 colour net),
  `test-tab.R` (`expect_color`), `test-exports.R`, `test-calculations.R`.
- History/rationale — `dev/design_new_colors_UI_decision_process.md` (esp. §3 audit, §12-14 debate);
  `dev/tabxplor_2.0.0_decisions.md` §3/§7/§12/§14/§18/§20/§24.
