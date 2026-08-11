# `reg_plot()` — one effect plot for every tabxplor regression table

Design study, 2026-08-11. **Status: DESIGN PROPOSAL, no R code written.**
Sibling of `dev/regression_assumptions_plots.md` (the z15 *diagnostics* plot); this one is the
*results* plot. Nothing here has been implemented — §21 lists the decisions still open for the
maintainer.

Every number quoted below was measured on `gss_simple` (`gss_cat_data_formatting()`) at HEAD
(`cf4d994`) with `devtools::load_all()`. Probe scripts are named where they matter.

---

# PART I — THE DECISIONS

## 1. The one-paragraph answer

Yes, `or_plot()` generalises — and it becomes *smaller*, not bigger, because almost everything a
general effect plot needs is already stored and is currently re-derived by hand. The generalisation
turns on a single missing object: **an estimate is a number *plus a scale*, and tabxplor has never
assembled the two.** The number lives in `fmt` fields, the scale is implied by column attributes,
and four separate consumers re-derive the pairing independently (`format()` to make strings,
`fmt_color_plan()` to pick a break ladder, the legend to word the ladder, `or_plot()` to draw an
axis). Assemble it once — `reg_estimates()`, a long tibble, one row per (table row × estimate
column) — and `reg_plot()` is ~200 lines of plain ggplot2 over it, provably in agreement with the
printed table because every number and every colour comes from the accessor the table used.

The three consequences that make the feature worth doing rather than merely possible:

1. **The plot's layout is the table's own structure.** `facet_grid(var ~ col_var)` with
   `space = "free_y"` reproduces exactly what the console prints: predictor blocks down, outcome /
   model / category groups across. Nothing is invented; both keys are stored.
2. **The observed-vs-modelled comparison gets an honest visual test.** Not CI overlap (Schenker &
   Gentleman 2001) — a **gap band** drawn around the observed point at `obs ± z·gap_se`. I verified
   numerically that "the adjusted point falls outside the band" is *exactly* `fmt_gap_p(x) < alpha`,
   to machine precision, on every row of a poisson table (§9.2). It is the CI of the difference,
   which is the correction Schenker & Gentleman prescribe, drawn so the reader checks containment
   instead of overlap.
3. **The three `color_signif` policies have exact geometric meanings in a forest plot** — point
   position (`ignore`), whisker crossing the neutral (`grey_non_signif`), distance from the neutral
   to the near whisker end (`guaranteed_effect`). The plot therefore *teaches* the colour system it
   inherits, which is the strongest argument for building it at all given the audience (§13).

---

## 2. The missing key, stated precisely

### 2.1 What is stored today

A `tab_reg()` table is uniform across every family (measured, probe 2):

```
var (factor) | levels (factor) | n… (role "n") | Obs_*… (role "emp") | Model_*… (role "model")
grouped by `var`
```

Per **cell**, the 21 `fmt` fields carry `or`/`diff`/`pct`/`mean`, `ci_inf`, `ci_sup`, `pvalue`,
`obs`, `gap_se`, `n`, `var` (= var(Y)), `in_refrow`, `display`, `digits`.
Per **column**, the 12 attributes carry `type`, `ci_type`, `col_var`, `color`, `color_signif`,
`model_family`, `role`, `conf_level`, `ref`, `refcol`, `totcol`, `comp_all`.

That is everything a forest plot needs. Nothing must be recomputed and no model must be refitted —
`or_plot()` already proved the no-refit contract works.

### 2.2 What is *not* stored

Nothing answers, in one place:

> for this column: what is its neutral value, is its scale additive or multiplicative, what unit is
> its axis in, how should a break be labelled, and which colour ladder applies?

Four consumers each answer it their own way:

| consumer | how it re-derives the scale | file |
|---|---|---|
| `format.tabxplor_fmt()` | compound `(display × type × ci_type)` predicates | `R/fmt_class.R` |
| `fmt_color_plan()` | `md$std_when` + `fmt_gap_scale_key()` | `R/fmt_class.R:3205-3226` |
| the legend | `MEASURES$…$unit_kind` / `break_over` / `break_under` + `legend_unit_word()` | `R/fmt_class.R:3855` |
| `or_plot()` | a **hard-coded ladder** `c(1/8, 1/4, 1/2, 1/1.5, 1, 1.5, 2, 4, 8)` | `R/tab_reg_plots.R:245` |

The fourth is the disease in miniature: `or_plot()`'s ladder has no relationship to
`options(tabxplor.color_breaks)`, so a user who moves their break scale sees the table change and
the plot not. Adding five more families by copying that pattern would be five more ladders.

### 2.3 The proposal — one resolver, one model

**(a) `fmt_scale_of(x)` — the scale record.** Extend, do not add: `fmt_gap_scale_key()`
(`R/fmt_class.R:3147-3152`) is already a four-clause dispatch on `(ci_type, type, model_family,
has var)` whose *order is the contract* (its own comment says so). Generalise it to return the whole
record instead of one string:

```r
fmt_scale_of(x) -> list(
  key        = "ratio" | "points" | "raw_diff" | "log_coef" | "level_pct" | "level_mean",
  neutral    = 1 | 0 | NA,          # NA for a level column: no null to draw
  trans      = "log10" | "identity",
  kind       = "effect" | "level",
  unit       = <chr>,               # axis title, i18n-ready
  break_key  = "odds_ratio" | "pct_diff" | "mean_diff" | "log_odds" | NA,   # the ESTIMATE's ladder
  gap_key    = "adj_ratio" | "adj_diff" | "adj_diff_std" | "adj_diff_log",  # the GAP's ladder
  secondary  = NULL | list(trans =, name =)                                  # §7.4
)
```

`fmt_gap_scale_key()` becomes `fmt_scale_of(x)$gap_key` — a pure refactor, byte-identical by
construction, and the fragile "the order is the contract" comment stops being load-bearing in two
places. Three consumers then share one dispatch instead of two agreeing by comment.

**(b) `reg_estimates(x)` — the long model.** One row per (table row × estimate column):

| group | columns |
|---|---|
| keys | `row` (table row index), `var`, `level`, `column`, `role` (`model`/`emp`), `col_var`, `facet`, `series`, `outcome`, `model_label`, `group` (split level), `category` |
| estimate | `estimate`, `ci_inf`, `ci_sup`, `pvalue`, `stars`, `n`, `is_ref`, `is_constant`, `kind` |
| comparison | `obs`, `gap_se`, `gap`, `gap_lo`, `gap_hi`, `gap_p`, `gap_slot` |
| scale | `scale_key`, `neutral`, `trans`, `unit`, `breaks` (list-col) |
| colour | `measure`, `policy`, `slot_text`, `slot_bg`, `hex_text`, `hex_bg`, `bold`, `italic`, `underline` |

Sources, all existing, none new:
`get_num()` / `get_ci_inf()` / `get_ci_sup()` / `get_pvalue()` / `get_stars()` / `get_obs()` /
`get_gap_se()` / `is_refrow()`; `fmt_gap_raw()`, `fmt_gap_bounds()`, `fmt_gap_p()`
(`R/fmt_class.R:1941/1967/1983`); `resolve_color_channel_plans()` (`:3441`) for `measure`/`policy`/
`breaks`; `fmt_channel_codes(col, theme)` (`:3473`) for slots, hex and face; `fmt_scale_of()` for
the rest.

**This is the whole architecture.** `reg_plot()` is then a renderer with no statistics in it.

### 2.4 Why the model must be public (and the one argument against)

For: users already reach into columns with `$or` / `get_or()` — a documented long form is the
answer to "how do I get these numbers out?", it is `ggstats`' `return_data = TRUE` idiom which this
audience knows, and it makes the plot testable without a graphics device (the z15 problem: "ggplot
has no golden lock" — a tibble does).
Against: one more exported function on a surface that freezes at release.
**Recommendation: export it.** It is not a new layer; it is the layer four consumers were each
half-implementing. Open decision D3 (§21).

---

## 3. The five architectural decisions

**A1 — the plot reads the table, never the model.** Inherited from `or_plot()` and non-negotiable:
no refit, no `data =` argument, no `reg_meta$fit_spec`. This is the exact opposite of
`reg_check_plots()`, which *must* refit (diagnostics are about residuals, which the table does not
carry). The two functions are siblings with opposite contracts, and saying so in both help pages is
worth a sentence each.

**A2 — layout is the table's structure, not a guess.** Rows of the panel grid = `var` (the table's
own `dplyr::group_vars()`); columns = the estimate columns' `col_var`. §14 gives the measured
`col_var` vocabulary per mode and the one case where it needs a derived key.

**A3 — colour always means the measure; never the series.** The point's colour is the cell's
colour, from the same slot. Series (crude vs model, model 1 vs model 2) are encoded by position
(dodge) and shape. Breaking this would silently destroy the table↔plot link that justifies the
feature.

**A4 — the observed comparison is a *gap band*, not a second interval.** §9. The crude confidence
interval is **not drawn by default** — deliberately, so the figure cannot invite the overlap
fallacy it exists to replace. `observed = "ci"` restores the classic figure for those who want it,
with a documented caution.

**A5 — one `ggplot` object out.** Not a `gtable`. Users can `+ theme()`, `+ labs()`, `ggsave()`.
This is a real break with `or_plot()` (which returns a `gtable` from `gridExtra::grid.arrange`, and
is therefore un-modifiable) and with `reg_check_plots()` (whose panels are heterogeneous and
genuinely need a grid). Heterogeneous scales are handled by moving the unit into the strip label
rather than by splitting objects (§14.4).

---

## 4. The public surface

### 4.1 What ships

```r
reg_plot(
  x,                                    # a tab_reg() table, or a list of them
  columns   = NULL,                     # NULL = every model column; a character vector selects
  what      = c("effect", "level"),     # §11
  observed  = c("auto", "band", "point", "ci", "none"),   # §9
  facet     = NULL,                     # NULL = auto (§14); FALSE = single panel
  color     = TRUE,                     # use the table's own colours
  guide     = c("gridlines", "bands", "none"),            # §12.3
  labels    = c("none", "estimate", "estimate_ci"),       # §15.3
  stars     = NULL,                     # NULL = follow the table
  intercept = FALSE,                    # the Constant row
  size      = NULL,                     # NULL = constant; "n" maps the add_n column
  theme = NULL, lang = NULL, caption = NULL, legend = TRUE, subtext = TRUE,
  ...
)

reg_estimates(x, theme = NULL)          # the long model (§2.3b)
```

### 4.2 What is removed

| removed | why | cost |
|---|---|---|
| `or_plot()` | superseded in full by `reg_plot()`; never released (added in 2.0.0 Phase 12h, `dev/tabxplor_2.0.0_decisions.md:3192`) | **none** — a removal, not a deprecation, exactly like `lm_plots()` in z15-iii |
| `or_plot(point_size =)` | verified inert (§19.1) | none |
| the hard-coded ladder `R/tab_reg_plots.R:245` | replaced by the colour break scale | none |
| the hard-coded fills `#33648c` / `#b0b0b0` `R/tab_reg_plots.R:259` | replaced by palette slots | none |

⚠ This contradicts **z15 ruling R3** ("`or_plot()` keeps its name and shares the internals"), which
was decided when `or_plot()` was only expected to gain a shared theme. The maintainer's instruction
for *this* study ("no back-compatibility needed at all on regression functions") supersedes it, and
the removal is free because the function has never been on CRAN. Recorded as open decision **D1**.

After both removals `R/tab_reg_plots.R` contains no legacy at all: `reg_plot()` here,
`reg_check_plots()` from z15-iii, and the shared `reg_plot_theme()` / `reg_plot_deps()` /
`with_legend_lang()` seam. That is the whole regression display surface, two functions, symmetric
names, opposite contracts.

---

# PART II — SCALES

## 5. The scale rule

An estimate column's axis is fully determined by facts already stored. The dispatch **order is the
contract** (inherited from `fmt_gap_scale_key()`, `R/fmt_class.R:3140-3152`, whose comment explains
why: a poisson count AME and a raw poisson coefficient are identical in `type`, `ci_type` and
`model_family` — only `var` separates them):

| # | test | key | neutral | transform | axis |
|---|---|---|---|---|---|
| 1 | `display_primary ∈ {pct, mean}` | `level_pct` / `level_mean` | — | identity | `%` / units of Y |
| 2 | `ci_type ∈ {or, ratio}` | `ratio` | 1 | **log10** | the effect word (§6) |
| 3 | `type == "coef"` and `!all(is.na(get_var(x)))` | `raw_diff` | 0 | identity | units of Y, **+ SD(Y) secondary axis** |
| 4 | `type == "coef"` and `reg_fam_logscale(model_family)` | `log_coef` | 0 | identity | log-odds, **+ ratio secondary axis** |
| 5 | otherwise (`ci_type == "diff"`, probability scale) | `points` | 0 | identity, `%` | percentage points |

Clause 1 must come first: `Obs_%` and `Obs_diff` carry **identical fields** (measured — both have
`pct` *and* `diff` non-NA; only `display` differs), so only `display` distinguishes a level column
from an effect column. `display` is a stored *field*, not a rendered string, so reading it obeys the
roles-are-stored rule; the colour engine already reads `display_primary()` for its `pvalue`/`gof`
gates (`R/fmt_class.R:3411-3432`).

## 6. Every family and effect, and what each needs

Measured column shapes (probe 2 / probe 3), with the axis each implies:

| family / effect | model column | `ci_type` / `type` | scale | axis title | special need |
|---|---|---|---|---|---|
| binomial, coefficient | `Model_OR` | or / row | `ratio` | Odds ratio | log axis, `1/x` labels |
| binomial, `exponentiate = FALSE` | `Model_β` | diff / coef | `log_coef` | log-odds | **secondary axis `exp(.)` = OR** |
| binomial, `effect = "ame"` | `Model_AME (adjusted %)` | diff / row | `points` | percentage points | `%` labels; also carries the adjusted probability in `pct` (§11) |
| binomial, `effect = "ame_ratio"` | `Model_RR (adjusted %)` | or / row | `ratio` | Risk ratio | log axis |
| `family = "poisson"` on a binary outcome (modified Poisson, z3) | `Model_RR` | or / row | `ratio` | Risk ratio | log axis |
| poisson counts | `Model_IRR` | or / row | `ratio` | Incidence-rate ratio | log axis |
| gaussian | `Model_β` | diff / coef | `raw_diff` | units of Y | **secondary axis `. / SD(Y)`** — the colour is standardized, the number is not |
| multinomial, coefficient | one column per category | or / row | `ratio` | Odds ratio | facet or dodge by category; crude rides **in-cell** in `obs` (no `Obs_*` column) |
| multinomial, `ame` | one column per category | diff / row | `points` | percentage points | same |
| ordinal | `Model_OR` + `Obs_cumOR` | or / row | `ratio` | Cumulative odds ratio | log axis; PO caveat belongs in the caption |
| grouped binomial (`trials =`) | `Model_OR` | or / row | `ratio` | Odds ratio | base column is a mean score, not a proportion |

**The effect word** is not guessed from the column name: it comes from `reg_meta$eff_word` and the
column's own `model_family` + `role`, which is exactly what `legend_reg_eff_word()` already does for
the legend (`R/fmt_class.R:4064`). One producer, two consumers.

## 7. Breaks, labels, and not overlapping

### 7.1 The ladder is the colour ladder

Candidate breaks = the column's own break scale, read from the plan
(`resolve_color_channel_plans(col)$text$over_breaks` / `$under_breaks`, `R/fmt_class.R:3441`),
mirrored around `plan$center`, plus the neutral. Measured defaults: `odds_ratio` =
`1.2 / 1.5 / 2 / 4` → the axis ladder `1/4, 1/2, 1/1.5, 1/1.2, 1, 1.2, 1.5, 2, 4`.

Two properties follow for free and neither exists today:
* a user who calls `set_color_breaks(odds_ratio = c(1.5, 3))` sees the gridlines move with their
  table;
* the gridline labels use the **same glyphs the legend uses** (`legend_break_label()`,
  `R/fmt_class.R:3784-3792`), so `1/1.5` in the footer is `1/1.5` on the axis.

### 7.2 Reciprocal labels are the point of the log axis

Keep the table's `1/2` convention rather than `0.5`. On a log axis `1/2` and `2` are equidistant
from 1, and the reciprocal spelling makes that symmetry *readable* — which is the whole reason the
axis is logarithmic. `or_plot()` already does this (`R/tab_reg_plots.R:248-249`); the convention is
simply promoted from a private helper to the shared `legend_break_label()`.

### 7.3 Overlap: three cheap devices, in order

1. **Trim to the data range** (`range(c(estimate, ci_inf, ci_sup))` extended 20 %), as `or_plot()`
   does. Typically leaves 4-6 breaks.
2. **Cap at 7 labels per panel**, dropping the least round first (keep the neutral, then integer
   powers, then the rest). Deterministic, no measurement of text extents needed.
3. **`guide_axis(check.overlap = TRUE)`** as the safety net — ggplot2's own device, which drops
   overlapping labels at render time keeping first, last and middle. This is the only mechanism that
   can react to the actual panel width, and it costs one line.
   `n.dodge = 2` is the fallback for very narrow facets; do **not** rotate labels (unreadable at 45°
   for `1/1.5`).

### 7.4 Secondary axes — where a scale otherwise loses its meaning

Two columns print a number on one scale while the *colour* scores another. The secondary axis is
exactly the repair, and `ggplot2::sec_axis()` makes it exact rather than approximate:

* **`exponentiate = FALSE`** (measured: `Model_β`, `type = "coef"`, `model_family = "binomial"`,
  `scale_key = "log_odds"`, breaks `0.2 / 0.4 / 0.7 / 1.4`). The primary axis is the coefficient;
  `sec_axis(transform = exp, name = "odds ratio")` puts `1.2 / 1.5 / 2 / 4` above it. Without this,
  a log-coefficient plot is unreadable to the audience the package targets.
* **gaussian β** (colour standardized by `sqrt(get_var(x))` = SD(Y), `R/fmt_class.R:3235-3241`).
  Primary axis in units of Y; `sec_axis(transform = ~ . / sd_y, name = "SD of Y")` shows the ladder
  the colours actually use.

**Rule, general:** *whenever the colour ladder lives on a different scale from the printed estimate,
that ladder's scale becomes the secondary axis.* This is the cleanest single answer to the
maintainer's question "which other families need a special scale to carry their meaning".

---

# PART III — OBSERVED VS MODELLED

## 8. What is available, exactly (measured)

### 8.1 `obs` — the pairing key, and it is not colour-gated

`obs` is written by `set_obs_if()` (`R/tab_reg.R:3918-3948`) whenever a crude record exists **and**
`reg_same_estimand()` **and** `reg_same_frame()` hold. It is **not** gated on `color =`.

> Measured (probe 3): `tab_reg(..., empirical = TRUE)` with **no** colour argument →
> `obs` non-NA on 7 of 8 rows of `Model_OR`; `gap_se` non-NA on 0.

So the maintainer's instinct is right: **`obs` is the reliable pairing key**, better than matching
`Obs_*` columns by name, and it is the *only* carrier for multinomial (where the crude column is
deliberately invisible, `shape_visible = FALSE`, ruling Q4).

### 8.2 `gap_se` — available only when `color = "adjustment"` was asked for

`reg_gap_se_columns()` opens with `if (!"adjustment" %in% sp$color) return(NULL)`
(`R/tab_reg.R:2355`). The comment is explicit: *"nobody reads it otherwise, and it costs ~1/8 of a
fit"*.

That was true with one consumer. **`reg_plot()` is the second**, and the moment a fact has two
consumers, gating it on *who asked to colour* rather than on *whether it is valid* is the pattern
Phase 17 spent itself removing. Concretely: a user who builds a table without `color = "adjustment"`
and then calls `reg_plot(observed = "band")` gets no band and no explanation.

Three ways out, in preference order:

* **(i) a `gap_test =` argument on `tab_reg()`** — `"auto"` (today's behaviour: on when a gap measure
  is coloured), `TRUE`, `FALSE`. One clause changes: `if (!isTRUE(gap_test)) return(NULL)` after
  resolution. Cost when TRUE: ~1/8 of a fit per column, measured by z8-B.
* (ii) always compute it when `empirical = TRUE` and the six validity clauses hold. Simplest, no new
  argument, but pays the cost for everyone.
* (iii) leave it, and have `reg_plot()` degrade with a message naming the argument to add. Cheapest,
  worst.

**Recommendation: (i).** Open decision **D2**.

### 8.3 The availability matrix (measured, probe 3)

`tab_reg(dependent, c("race","age"), empirical = TRUE, color = "adjustment")`, counts of non-NA
cells out of the skeleton:

| family | effect | model column | `ci_type` | `obs` | `gap_se` |
|---|---|---|---|---|---|
| binomial | coefficient | `Model_OR` | or | 4 | **0** |
| binomial | ame | `Model_AME (adjusted %)` | diff | 4 | 3 |
| binomial | ame_ratio | `Model_RR (adjusted %)` | or | 4 | 3 |
| poisson (binary → `rr`) | coefficient | `Model_RR` | or | 4 | 3 |
| gaussian | coefficient | `Model_β` | diff | 4 | 3 |
| poisson (counts) | coefficient | `Model_IRR` | or | 4 | 3 |
| multinomial | coefficient | per category | or | 3 | **0** |
| multinomial | ame | per category | diff | 4 | 3 |
| ordinal | coefficient | `Model_OR` | or | 3 | **0** |
| ordinal | ame | per category | diff | 3 | 3 |

The three zeros are **ruling Q1(b)**, not a bug: `reg_estimand_collapsible()` refuses a gap test on
a conditional odds ratio, because a non-collapsible OR moves under adjustment with zero confounding
(z8-B measured rejection 1.000 at n = 32 000 against a nominal 0.05). The plot must therefore show
three states, not two — **tested & significant / tested & not / not tested** — which is precisely
what the table's own legend already says ("*or not tested*"). §9.4.

### 8.4 `between_groups` (`split_var`) — the baseline column carries nothing

Measured (probe 5), `split_var = "race"`, `color = "between_groups"`:

```
Model_OR_White   obs = 0/5   gap_se = 0/5     <- the baseline group: it IS the reference
Model_OR_Black   obs = 5/5   gap_se = 4/5     <- gap_se NA on the predictor's own reference row
Model_OR_Other   obs = 5/5   gap_se = 4/5
```

So a plot must handle "this panel is the baseline" — the legend already words it
(`legend_gap_baseline_word()`, `R/fmt_class.R:3705`, → *"reference group"*). In the plot the
baseline panel simply shows no crude marker and no band; a strip annotation says so.

## 9. The gap band

### 9.1 Why not the classic two-interval figure

The classic crude-vs-adjusted forest plot draws two point-and-whiskers per row and invites the
reader to compare overlap. Schenker & Gentleman (2001, *The American Statistician* 55(3)) is the
standard citation for why that is wrong: overlap-based judgement is materially more conservative
than the correct test, and their prescription is explicit — *"test by examining the confidence
interval for the difference between the two estimates."*

Here it is worse than merely conservative, because the two estimators are **correlated** (they solve
their estimating equations on the same observations; z8-B measured r = 0.52-0.90). That is the whole
reason `gap_se` exists and is computed by influence functions rather than by
`sqrt(se1² + se2²)` — which z8-B measured as 2-4× too large.

### 9.2 The band, and the proof that it *is* the test

Draw, around the **observed** point, the interval

```
band = obs ⊕ ± z(conf_level) · gap_se        ( ⊕ = × on a ratio scale, + on an additive one )
```

and draw the modelled point where it falls. Then:

> **the modelled point lies outside the band ⟺ the gap test rejects at `conf_level`.**

This is an identity, not an approximation, because `gap_se` is stored on the estimate's own test
scale and `fmt_gap_p()` is `2·pnorm(-|gap| / gap_se)` (`R/fmt_class.R:1983-1988`).

Measured (probe 4, modified-Poisson table, `color = "adjustment"`):

```
fmt_gap_p  : NA NA 2.292e-28 1.133e-34 NA 0.01125 0.007757 9.644e-28 1.353e-19
manual p   : NA NA 2.292e-28 1.133e-34 NA 0.01125 0.007757 9.644e-28 1.353e-19
```

with `manual p = 2·pnorm(-|log(est) - log(obs)| / gap_se)`, and the containment check agreeing on
every row:

| level | obs | est | band_lo | band_hi | outside | p |
|---|---|---|---|---|---|---|
| Black | 0.591 | 0.632 | 0.583 | 0.598 | TRUE | 2.3e-28 |
| Other | 0.949 | 1.041 | 0.935 | 0.963 | TRUE | 1.1e-34 |
| $10000-14999 | 1.121 | 1.095 | 1.101 | 1.142 | TRUE | 0.0113 |
| $15000-24999 | 1.187 | 1.163 | 1.169 | 1.204 | TRUE | 0.0078 |
| $25000+ | 1.510 | 1.398 | 1.489 | 1.531 | TRUE | 9.6e-28 |
| age (per SD) | 1.201 | 1.177 | 1.196 | 1.207 | TRUE | 1.4e-19 |

So the reader's visual operation ("is the dark point inside the pale bracket?") is exactly the
package's test. **No other visual device in this design is an identity; this one is.**

### 9.3 What is drawn

Per row, when `observed` resolves to `"band"` (the `"auto"` default whenever `gap_se` is present):

| layer | encodes | geometry |
|---|---|---|
| observed marker | the crude estimate | hollow/light point (no whisker) |
| **gap band** | the CI of the *difference* | a short thick bracket / `geom_errorbarh` around the observed marker |
| connector | direction and size of adjustment | segment observed → modelled, **coloured by the gap measure's slot** |
| modelled point + whisker | the model estimate and its own CI | filled point, `geom_linerange` |

`"point"` drops the band (when `gap_se` is absent), `"ci"` restores the classic crude interval,
`"none"` suppresses the crude entirely, `"auto"` picks `band` → `point` → `none` by availability.

### 9.4 Three states, honestly

| state | condition | rendering |
|---|---|---|
| tested, gap significant | `gap_se` present, point outside band | solid connector, band drawn |
| tested, gap not significant | `gap_se` present, point inside band | solid connector, band drawn, connector greyed under `grey_non_signif` |
| **not tested** | `gap_se` all-NA on the column | dotted connector, **no band**, and the caption says why |

The third state is already a first-class concept: `fmt_gap_force_policy()`
(`R/fmt_class.R:2005`) reads an all-NA `gap_se` as "no test here" and forces the measure to read
descriptively. `reg_plot()` reads the same predicate — it does not invent a fourth encoding.

### 9.5 The large-N caveat, stated up front

At survey sizes the band is *very* narrow (measured above: half-widths of 0.01-0.03 on the ratio
scale at n ≈ 13 000) and essentially every gap is "significant". That is not a defect of the band —
it is the same fact that makes the whole framework colour by *size* and star by *test*. Two
consequences for the design:

* the band must be drawn at its true width even when thinner than the marker (no minimum width — a
  minimum would lie);
* the documentation and the caption should lead with the *size* of the adjustment (the colour) and
  treat the star as secondary. The existing legend wording already does this.

### 9.6 Non-collapsibility, in the plot

For a conditional OR there is no test *and* the gap itself is partly mechanical. The table's legend
already ships one sentence about this on the non-collapsible path only. `reg_plot()` should inherit
that sentence in its caption (it comes free with `rd_footer()`, §13.4) and add nothing. Suggesting
`family = "poisson"` or `effect = "ame"` in the plot would be a second voice saying what `?tab_reg`
already says — rejected.

---

# PART IV — COLOUR

## 10. The mapping, complete

Nothing invented; every channel maps to the geometry that corresponds to what it paints in the
table.

| table | plot | source |
|---|---|---|
| cell **text** colour (channel 1) | the **point's** colour | `fmt_channel_codes(col, theme)$text` |
| cell **background** (channel 2) | a **band behind the row** | `…$bg` |
| bold / italic / underline (print palette) | the point's stroke weight / shape | `…$text_face` |
| grey (slot 0) | grey point | `theme_cols$grey` via `fmt_col_ann()` |
| the break ladder | the **gridlines** | `plan$over_breaks` / `$under_breaks` |
| the legend prose | the **caption** | `rd_footer(src, "plain" \| "runs")` |
| the stars | optional text beside the point | `get_stars()` |

The row band is the literal translation: a two-channel table paints the cell's background, and the
plot paints the row's background, with the same hex. The background palette
(`default_background_colors`, very pale by design: `#dffcff`, `#d7efff`, …) is exactly what a
behind-the-data band needs.

## 11. `what = "level"` — the second geometry, and its honest scope

Measured (probe 5): a `Model_AME` column carries **both** the AME in `diff` **and the adjusted
predicted probability in `pct`**; `Obs_%` carries the observed proportion in `pct`. So an
"observed % vs adjusted %" dot plot per level is *already fully stored* and costs one extra mapping:

```
race: White   observed 51%  ●———○  adjusted 50.5%
race: Black   observed 28%  ●—————————○ adjusted 28.4%
```

For the stated audience (literary students) this is very probably the most readable figure the
package can produce: no odds, no ratios, no logs, just percentages. It is also the figure that makes
"what adjustment did" concrete.

**Scope, honestly**: `pct` is filled only on the AME path. A coefficient table stores no adjusted
probability, and a gaussian `Model_β` stores no adjusted mean (only `Obs_mean` has a mean). So
`what = "level"` works for `effect = "ame"` / `"ame_ratio"` tables and must abort with a clear
message elsewhere, naming `effect = "ame"` as the way to get it. That is a real limitation and the
main reason this could be judged a white elephant — open decision **D4**.

## 12. The three `color_signif` policies become geometry

This is the strongest teaching argument for the whole feature, and it required no design work — it
is what a forest plot *is*.

| policy | what it does to a cell | what it is in the plot |
|---|---|---|
| `ignore` | colour by the raw quantity | **where the point sits** relative to the neutral line |
| `grey_non_signif` | grey unless the interval excludes the neutral *and* agrees in direction | **whether the whisker crosses the neutral line** |
| `guaranteed_effect` | score the CI bound nearest the neutral | **how far the near end of the whisker is from the neutral line** |

So a single figure explains all three policies, and a user who has seen the plot can read the
table's colours without reading the legend. Two concrete devices follow:

### 12.1 `guaranteed_effect` gets a shaded "guaranteed" region

Under this policy the score *is* `floor_q` = the near bound (`R/fmt_class.R:3294-3316`). Shading
from the neutral line to the near whisker end makes the coloured quantity literally visible: the
longer the shading, the deeper the slot. Nothing else in the package can show this.

### 12.2 `grey_non_signif` needs no device

A greyed point whose whisker crosses the line is self-explanatory. Do not add a second encoding
(shape, alpha) — the redundant-encoding literature finds the benefit is for *segmentation in dense
displays*, not for comprehension, and here it would compete with the series encoding (A3).

### 12.3 `guide = "bands"` — the panel background *is* the colour scale

Optional but genuinely striking: shade the panel between consecutive breaks with the corresponding
background-palette slot. A point then falls inside the band whose colour it has, and the reason a
cell is that colour becomes visible rather than explained. Costs one `geom_rect` layer and reuses
`get_color_style("color_code", type = "bg", theme)`.

Risk: clutter, and it doubles the ink when a bg channel is already painting rows. Default
`"gridlines"`; `"bands"` opt-in and documented as the teaching mode. Open decision **D5**.

## 13. Where the legend goes

`rd_footer(src, medium, theme, want_legend, subtext, lang)` (`R/tab-export-prep.R:681`) is the one
footer producer every backend shares. Its media are `console` / `html` / `md` / `runs` / `plain` —
**there is no `plot` medium**, and none is needed:

* `medium = "plain"` → a character vector → `labs(caption = paste(., collapse = "\n"))`. One line of
  code, loses colour. **The default.**
* `medium = "runs"` → `list(text, color, bold, italic, underline)` per token — what `tab_plot()`
  already consumes (`R/tab_classes.R:2024-2036`) and what a coloured caption would need. Only worth
  it if the coloured ladder in the caption is judged important; it costs a manual layout pass since
  ggplot2 captions are single-styled.
* a real ggplot **guide** (a discrete `scale_*_manual` with the break labels as keys) is buildable
  from `legend_break_tokens(plan, …)` (`R/fmt_class.R:3795`), whose tokens already carry the label
  and the slot. This is the most "ggplot-native" answer and the most work.

**Recommendation:** caption via `"plain"` for v1; the ladder is on the axis anyway (§7.1), which is
better than a legend key. Open decision **D6**.

The caption then automatically carries, in order: the weight line, the `Model:` line, the
interaction line, the colour legend, the stars legend, the user's `subtext` — including the
non-collapsibility sentence (§9.6) and the "or not tested" clause (§9.4), because they are already
in the legend. Nothing is written twice.

---

# PART V — LAYOUT

## 14. Facets

### 14.1 The measured `col_var` vocabulary

| table | `col_var` values (probe 4) |
|---|---|
| one model, one outcome, `empirical` | all estimate columns share `"married: 01-Married"` |
| **model comparison** | `"m1"`, `"m2"` on model columns; **`"Obs_%"`, `"Obs_OR"` on the crude ones** |
| two dependents | `"married: 01-Married"` / `"black: 01-Black"`, shared by crude and model |
| `split_var` (spread, the default) | `"White<br>married: 01-Married"`, … |
| multinomial | `"2-Independent, other vs 1-Democrat"`, … |

So `col_var` is the facet key in four modes out of five. In **comparison mode it breaks**: the single
crude block is not attached to any model's `col_var` (it is deliberately shared — one crude block
serves every model, which is what makes `color = "adjustment"` work across a comparison).

### 14.2 The derived key

```
facet(column) = col_var(column)                     for a model column
facet(column) = the col_var of the model columns it serves   for a crude column
```

and in comparison mode the crude series is **repeated in every panel** — which is correct, since
every model is compared against the same observed effect.

This is the one place a derived key is unavoidable. Two ways to make it a *stored* fact instead:
the crude block already knows which fits it serves (`emp_by_fit`), so `col_var` on the crude columns
could be set to the served model's `col_var`; or, better, `reg_estimates()` derives it once from
`role` + `fit_of_col` and nothing downstream ever guesses. Open decision **D7**.

### 14.3 The grid

```r
facet_grid(rows = vars(var), cols = vars(facet), scales = "free", space = "free_y", switch = "y")
```

* rows = `var` → the predictor blocks the table already groups by, each strip naming the predictor,
  row heights proportional to the number of levels (`space = "free_y"`);
* cols = `facet` → outcome / model / category;
* `scales = "free"` so a mixed-family table can have different x per column;
* y within a row = `levels`, in **table order** (reversed for the y axis so the plot reads
  top-to-bottom like the table).

The result is a literal transposition of the printed table into panels, which is what makes the
table↔plot link immediate.

`facet = FALSE` collapses to a single panel with `var: level` on the y axis (right for a
one-predictor table, and for `ggsave` at small sizes).

### 14.4 Heterogeneous scales, without splitting the object

A mixed-family table (possible since 15e) can hold an OR column and a β column. `facet_grid` has one
axis title for the whole plot, and ggplot2 cannot give per-facet axis titles.

**Solution: when the columns do not share a `scale_key`, the unit moves into the strip label** —
`"married — odds ratio"`, `"tvhours — hours"` — and the axis title is dropped. One `ggplot` is still
returned (A5). Rejected alternatives: `ggh4x` (a new Suggest, forbidden by z15 R4), and returning a
`gtable` from `gridExtra` (breaks A5 for a rare case).

### 14.5 Size, and the wall

Default to **no faceting when there is one estimate group**. When the grid would exceed ~12 panels,
inform (naming `columns =` to subset) and draw anyway — z15's stance, which is right: *"a wall is a
legible failure mode, a silent 'first model only' is not."* This is also the fix for `or_plot()`'s
current behaviour of silently plotting one column and saying so afterwards.

## 15. Rows, series, labels

### 15.1 Rows

Table order, top-to-bottom. Not sorted by effect size: the table's order is the user's order
(`predictors =`), and re-sorting would break the link. The `Constant` row is dropped by default
(`intercept = FALSE`, following `ggstats`); its "effect" is an intercept, meaningless on a forest
axis. Reference rows **are** drawn, at the neutral, without a whisker, marked (hollow + `ref.`),
matching the table's `1` / `0` cell and its bold.

### 15.2 Series

Within a panel: crude vs model (§9.3), and several model columns when `facet = FALSE`. Encoded by
**vertical dodge + shape**, never colour (A3).

### 15.3 Labels

`labels = "estimate"` prints the formatted estimate at the right edge of each panel, inside the
plot, via `geom_text` at a per-panel x position. This gives `finalfit`'s readability without the
second gtable panel that makes `or_plot()` un-modifiable. **Default `"none"`** — the numbers are in
the table the user already has, and a tabxplor plot exists to show the pattern, not to re-print the
table. (`or_plot()`'s whole left panel is a table the user printed two lines earlier.)

The formatted string must come from `format()` on the fmt column — the export-parity contract makes
`format()` the only string producer, and it already renders `1/2.45***`.

### 15.4 Point size

`size = NULL` (constant) by default. `size = "n"` maps the `add_n = TRUE` column when present.
See §19.1: `or_plot()`'s size-by-n is inert, and reviving it silently would be worse than leaving
it off.

---

# PART VI — INTEGRATION

## 16. Relationship to the rest of the package

| | `reg_plot()` | `reg_check_plots()` (z15-iii) | `tab_plot()` |
|---|---|---|---|
| input | a table | a table **+ its data** (or a fit) | a table |
| refits? | **never** | **always** | never |
| output | one `ggplot` | a `gtable` grid | a `ggpubr::ggtexttable` image *of the table* |
| about | the results | the assumptions | the table's appearance |
| status | new | new | frozen legacy |

`tab_plot()` is not a chart — it renders the table as an image. **`reg_plot()` would be the
package's first real data chart.** That is worth stating in the docs so users stop looking for a
chart in `tab_plot()`.

### 16.1 Sequencing with z15

z15-iii owns `reg_plot_theme()` (reading `tx_chrome_hex(theme)`, replacing the five hard-coded
`#c00000` literals) and the `lm_plots()` removal, and budgets ~40 msgids. `reg_plot()` should land
**after** z15-iii and adopt the same seam: `reg_plot_deps()`, `reg_plot_theme(theme)`,
`with_legend_lang(lang, …)`. If it lands first, it must *define* that seam and z15-iii adopts it —
but not two seams. Open decision **D8**.

### 16.2 Suggests

No new package. `ggplot2` (already a Suggest) is enough for everything in this design;
`gridExtra` becomes unnecessary for `reg_plot()` specifically (A5), though z15-iii still needs it.
`ggtext` would be needed **only** for a coloured caption (§13) — recommended against for v1.

### 16.3 i18n

All labels through `with_legend_lang()` + literal `gettext()`/`gettextf()`, per z15 §19. Notation
(OR, IRR, β, AME, RR) stays English; prose is translated. New msgids: ~12 (axis titles per scale,
"observed", "modelled", "reference", the not-tested note, the secondary-axis names). The legend and
footer arrive already translated through `rd_footer()`.

⚠ Inherit z15's warning verbatim: the footer nouns are `gettext()`'d at render under the **ambient**
locale, not the plot's `lang =` (Phase z2's glibc catalogue-caching limitation). Two mechanisms for
two surfaces is the existing state — do not "unify" them.

### 16.4 Tests

`ggplot_build()` on the returned object, asserting layer row counts, the resolved breaks, the
mapped colours against `fmt_channel_codes()`, and the band bounds against `fmt_gap_p()`. Because
`reg_estimates()` is a tibble, most assertions can be made on it directly, without a device — which
is the answer to "ggplot has no golden lock" (`dev/tabxplor_phase10_exporters.md:191`).

The load-bearing test: **for every family × effect in §8.3, the plotted estimate equals the value
`format()` prints in the table.** That is the whole no-drift claim, and it is one loop.

### 16.5 jamovi

Out of scope for the design, but the shape is right: `jmvtabreg.b.R`'s `.plot` renderFun is already
a no-op stub, and `reg_plot(table)` needs no data — so a jamovi Image is a two-line addition later,
unlike `reg_check_plots()` which needs the frame.

---

# PART VII — CAVEATS, DEFECTS, OPEN DECISIONS

## 17. Answers to the maintainer's five questions

**"Would it be possible to generalise `or_plot()` to all families and effects?"**
Yes, and the generalised version is smaller than the current one, because the per-family knowledge
is already stored (§5-6). The only genuinely per-family work is the two secondary axes (§7.4).

**"Facets when there is more than one `col_var`?"**
Yes — `col_var` is the facet key in four of five modes; comparison mode needs one derived key
(§14.2). Small plots are the default (no facet with one estimate group); big grids draw with an
`inform`, never a silent truncation.

**"Which families need a special scale?"**
Ratios need the log axis (already); `exponentiate = FALSE` needs a secondary `exp()` axis or it is
unreadable; gaussian β needs a secondary SD(Y) axis to make its colour ladder legible; AME needs a
percentage-point axis; multinomial/ordinal need per-category facets. Nothing else.

**"Observed-vs-modelled overlay — and how to convey the test without CI overlap?"**
The `obs` field is the right pairing key (§8.1) and works where no crude column exists. The test is
conveyed by the **gap band** (§9.2), which is an exact identity with `fmt_gap_p()`, and by refusing
to draw two comparable intervals by default.

**"Can colour carry more information, and link the table to the plot?"**
Yes: identical slots, identical hexes, the break ladder as gridlines, and — the part that is more
than decoration — the three significance policies have exact geometric readings in a forest plot
(§12), so the plot explains the table's colours rather than merely matching them.

## 18. Honest disagreements with the brief

**"A second, lighter point-and-whisker per row for the crude estimate is the classic figure."**
It is, and it is the figure whose reading Schenker & Gentleman showed to be wrong — worse here,
since the two estimators are correlated. I recommend **not** drawing the crude interval by default
and drawing the gap band instead, with `observed = "ci"` available and documented. The classic
figure is available; it is just not the default the package endorses.

**`what = "level"` may be a white elephant.** It is the most readable output in the design, but it
only exists on AME tables. If the maintainer expects coefficient tables to dominate, this is two
geometries where one would do, and should be cut (D4).

**`guide = "bands"` may be one device too many** when a background colour channel is already
painting rows (D5).

## 19. Defects found while measuring

### 19.1 `or_plot(point_size =)` is inert — verified

`or_plot()` maps point size to `get_n(oc)` (`R/tab_reg_plots.R:233`). Phase 14r moved the whole-model
N to the footer, so `reg_column()` writes `n = rep(NA_integer_, n_rows)` (`R/tab_reg.R`, both
branches). Measured on `tab_reg(..., family = "binomial")`:

```
Model_OR n field         : NA NA NA NA NA NA NA NA
point layer size values  : NA NA NA NA NA NA NA
```

So the documented `point_size` range has no effect on the default (model) column, and the size
aesthetic resolves to `NA` for every point. It is meaningful only when `column =` selects an `Obs_*`
column, which does carry per-cell `n`. Disappears with `or_plot()`; `size = "n"` (§15.4) is the
honest replacement, reading the `add_n = TRUE` column.

### 19.2 `or_plot()`'s ladder ignores the user's break scale

`R/tab_reg_plots.R:245` hard-codes `c(1/8, 1/4, 1/2, 1/1.5, 1, 1.5, 2, 4, 8)`. A user who calls
`set_color_breaks(odds_ratio = …)` moves the table's colours and legend but not the plot's
gridlines. Fixed by construction in §7.1.

### 19.3 `or_plot()`'s colours are outside the palette system

`scale_fill_manual(values = c(Estimate = "#33648c", Reference = "#b0b0b0"))`
(`R/tab_reg_plots.R:259`) — two literals that belong to no palette, ignore `theme = "dark"`, and
ignore `theme = "print"` (z11). The only plot in the package is the only renderer that does not
consume the colour engine. Fixed by §10.

### 19.4 The declared `ggplot2` floor is below what `or_plot()` already uses

`DESCRIPTION:39` declares `ggplot2 (>= 3.4.0)`, but `or_plot()` calls
`scale_x_continuous(transform = "log10")` (`R/tab_reg_plots.R:257`) — `transform =` is the
**3.5.0** spelling; before that the argument was `trans =`. The box here has 4.0.3, so this is
untested rather than observed, but on a 3.4.x install the log axis would at best be ignored.
Either bump the floor to `>= 3.5.0` or use `trans =`. This design compounds it: `sec_axis(transform =)`
(§7.4) is the same rename. **Recommendation: bump the floor**, which also unlocks
`guide_axis(check.overlap =)` semantics without caveats.

### 19.5 Multi-column tables get a message, not a plot

On an `empirical = TRUE` table `or_plot()` picks one column and informs afterwards
(`R/tab_reg_plots.R:218-221`). That is the behaviour faceting replaces.

## 20. Rejected alternatives

| rejected | why |
|---|---|
| a new `SCALES` fact table beside `MEASURES` | `MEASURES` is about the colour *measure*; the estimate scale is a different question with the *same* dispatch as `fmt_gap_scale_key()`. Generalising that one function is integration; a parallel table is a second encoding of one rule (§2.3a) |
| deriving the axis from the rendered string (`format()`) | matching rendered labels is exactly what the roles-are-stored rule forbids, and it is how `or_plot()`'s `^Emp\\.` prefix bug happened (`dev/reg_comparison_framework_stress_test.md:691`) |
| pairing crude and model by column **name** (`Obs_OR` ↔ `Model_OR`) | `obs` is stored, name-free, and is the only carrier for multinomial (§8.1) |
| `sqrt(se_model² + se_crude²)` for the band | measured 2-4× too large; the estimators are correlated (z8-B) |
| CI-overlap reading | Schenker & Gentleman 2001 (§9.1) |
| returning a `gtable` with a finalfit-style text panel | un-modifiable by the user; re-prints a table the user already has; `labels = "estimate"` gives the same information inside one `ggplot` (§15.3) |
| `ggh4x` for per-facet axis titles | new Suggest, forbidden by z15 R4; the strip label solves it (§14.4) |
| `patchwork` | not a Suggest anywhere in the repo; `gridExtra` is the established grid tool and A5 avoids needing either |
| a `"plot"` medium in `legend_render_line()` | `"plain"` and `"runs"` already cover it (§13) |
| sorting rows by effect size | breaks the table↔plot correspondence, which is the feature's reason to exist |
| extending `reg_plot()` to crosstabs now | the estimate model is class-agnostic and a crosstab dot plot is one facet mapping away, but it is a *second* feature with its own layout questions. Deliberately deferred, noted so the door stays open (D9) |

## 21. Open decisions for the maintainer

| # | decision | recommendation |
|---|---|---|
| **D1** | Remove `or_plot()` outright (contradicting z15 ruling R3, which predates the "no back-compat on reg functions" instruction), or keep it as an alias of `reg_plot(columns = <the OR column>)`? | **Remove.** Never released; the alias would preserve a broken `point_size` and a private ladder |
| **D2** | `gap_se` is gated on `color = "adjustment"` (`R/tab_reg.R:2355`). Add `tab_reg(gap_test =)`, always compute it when `empirical = TRUE` and valid, or leave it and degrade with a message? | **Add `gap_test = c("auto", TRUE, FALSE)`** (§8.2) |
| **D3** | Export `reg_estimates()`, or keep it internal behind `reg_plot(return_data = TRUE)`? | **Export** (§2.4) |
| **D4** | Ship `what = "level"` (observed vs adjusted percentages), knowing it only works on AME tables? | Ship it — it is the most readable figure for the target audience — but cut it without regret if AME tables are judged rare (§11) |
| **D5** | `guide = "bands"` (panel background = the colour scale) — ship, or gridlines only? | Ship, **not** as the default (§12.3) |
| **D6** | Caption via `rd_footer(medium = "plain")`, a coloured `"runs"` caption, or a real ggplot guide built from `legend_break_tokens()`? | `"plain"` for v1 (§13) |
| **D7** | Fix the comparison-mode facet key by storing the served `col_var` on crude columns, or by deriving it once in `reg_estimates()`? | Derive once (§14.2) — storing it would change a column attribute that the exporters' header machinery reads |
| **D8** | Land `reg_plot()` after z15-iii (adopting `reg_plot_theme()`), or before (defining the seam)? | **After** (§16.1) |
| **D9** | Should the estimate model / a future `reg_plot()` accept `tab()` crosstabs (cell estimates with CIs)? | Not now; keep the internals class-agnostic so it stays a small step (§20) |
| **D10** | Function name: `reg_plot()` (symmetric with `reg_check_plots()`), or a general name (`tab_chart()`) since the internal model is not regression-specific? | **`reg_plot()`** — the symmetry with `reg_check_plots()` is worth more than anticipating D9 |

## 22. Measurement appendix

Probe scripts (scratchpad, not committed): `probe1.R`-`probe5.R`.
Environment: WSL2 Ubuntu, `devtools::load_all()` at `cf4d994`, `OMP_NUM_THREADS=1`,
data = `gss_cat_data_formatting()` (21 407 rows; 12 990 complete on `rincome`).

Reproduced facts, in order of load-bearing-ness:

1. **gap band ≡ gap test** — `fmt_gap_p(x)` equals `2·pnorm(-|log(est) − log(obs)| / gap_se)` to all
   printed digits on all 9 rows of a modified-Poisson table, and `est ∉ obs·exp(±1.96·gap_se)`
   agrees with `p < 0.05` on every row (§9.2).
2. **`obs` is not colour-gated; `gap_se` is** — 7/8 vs 0/8 without `color = "adjustment"` (§8.1-8.2).
3. **the availability matrix** — 10 family × effect combinations, 16 model columns (§8.3).
4. **`split_var` baseline carries nothing** — `Model_OR_White` has `obs` 0/5 (§8.4).
5. **`exponentiate = FALSE` → `scale_key = "log_odds"`, breaks `0.2/0.4/0.7/1.4`** (§7.4).
6. **the AME column stores the adjusted probability in `pct`**, `Obs_%` the observed one; `Obs_%`
   and `Obs_diff` are field-identical and differ only in `display` (§5, §11).
7. **`or_plot()`'s size aesthetic is all-`NA`** on model columns (§19.1).
8. **`col_var` per mode** — five modes, measured verbatim (§14.1).

## 23. Sources

- Schenker, N. & Gentleman, J. F. (2001), *On Judging the Significance of Differences by Examining
  the Overlap Between Confidence Intervals*, The American Statistician 55(3):182-186 —
  https://www.tandfonline.com/doi/abs/10.1198/000313001317097960
- Larmarange, J., `ggstats::ggcoef_model()` family (`ggcoef_compare`, `ggcoef_faceted`,
  `ggcoef_dodged`, `ggcoef_table`) — the closest prior art, and from the same disciplinary audience:
  https://larmarange.github.io/ggstats/articles/ggcoef_model.html
  Adopted from it: variable strips as facets, reference rows drawn by default, log axis under
  `exponentiate`, `return_data`. Deliberately *not* adopted: significance by point shape (colour
  already carries it here), `stripped_rows` as decoration (the row band carries meaning here).
- `dotwhisker::dwplot()`, `sjPlot::plot_model()`, `modelsummary::modelplot()` — the same family of
  coefficient plots; none of them carries a crude-vs-adjusted comparison or a gap test.
- Forest-plot conventions (log scale so ratios are symmetric about 1; reference line at the null):
  https://en.wikipedia.org/wiki/Forest_plot
- Change-in-estimate practice and its limits (the 10 % rule the `adj_ratio` ladder encodes; and why
  it is not a test): https://cran.r-project.org/web/packages/chest/vignettes/chest-vignette.html
- Redundant visual encoding — benefit is for segmentation in dense displays, not comprehension
  (§12.2): https://www.tandfonline.com/doi/full/10.1080/15551393.2017.1343153
- Marginal effects vs odds ratios in sociology (why the AME/`what = "level"` geometry matters for
  this audience): https://sociologicalscience.com/articles-v10-10-332/

## 24. Internal cross-references

- `dev/regression_assumptions_plots.md` §3 (R3, R4), §13, §19, §21 — the sibling plot, its theme /
  guard / i18n seam, and the `lm_plots()` removal this design mirrors.
- `dev/model_vs_observed_gap_test.md` §3.8, §4, §5.3, §12 — `gap_se`, its influence-function
  derivation, where it stops holding, and ruling Q1(b).
- `dev/model_vs_observed_effect_colour.md` §11.5 — the crude-overlay item this design implements.
- `dev/reg_comparison_framework_stress_test.md` §8.2 and §11 — "a crude-vs-adjusted overlay in
  `or_plot()`" was listed as cheap QoL and explicitly **not taken** ("maintainer's scope choice;
  only the stale-prefix repair landed"); its capability matrix (~line 521) records that competitors
  (`gtsummary`, `finalfit`) have a forest plot but none has the comparison.
- `dev/black_and_white_publication_palette.md` §7.3 — the rule any new renderer inherits: the
  palette is the single source; a backend only translates a record into its own vocabulary.
- `CLAUDE.md` Repository Map — `R/tab_reg_plots.R`, `R/fmt_class.R` (colour engine + legend),
  `R/tab-export-prep.R` (`rd_footer`).
