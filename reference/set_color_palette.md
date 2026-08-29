# Colours: palettes, styles and breaks

Everything that decides what a coloured cell LOOKS like, and at which
value it changes shade. `set_color_palette()` sets the hues (and the
console's light/dark theme); `set_color_breaks()` sets the thresholds
each measure is read on; `get_color_style()` and `get_color_breaks()`
read them back. All of them act globally, through
[`options()`](https://rdrr.io/r/base/options.html), so one call at the
top of a script restyles every table it builds — see
[tabxplor-options](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md).
A single table can override the thresholds with `tab(color_breaks =)`.

Color breaks are a named list of the ten measure scales `pct_diff`,
`pct_ratio`, `odds_ratio`, `mean_diff`, `mean_ratio`, `contrib`,
`zscore`, `adj_ratio`, `adj_diff` and `adj_diff_std`. Each is a vector
of positive-only thresholds (the under-represented side is mirrored
automatically), 1 to 4 values, one per color step.

Every default is the same ladder in another measure, read at ONE
reference cell of 50 %: 5 / 10 / 20 / 30 percentage points is also 0.1 /
0.2 / 0.4 / 0.8 SD, x1.1 / x1.2 / x1.5 / x2 as a ratio and x1.2 / x1.5 /
x2 / x4 as an odds ratio – so a shade means the same size of deviation
whichever measure a table is read on. `pct_diff` colors percentage-point
differences, `pct_ratio` the relative risk, `odds_ratio` the odds ratio
(`color = "odds_ratio"`), `mean_diff` the standardized mean difference
(Glass's delta) by default (supply data-unit values for absolute
coloring), `mean_ratio` the mean ratio, `contrib` the chi2 contribution
(in multiples of the mean cell contribution) and `zscore` an absolute z
scale (the adjusted standardized residual) – the absolute scale
`color = "contrib"` switches to under
`color_signif = "guaranteed_effect"`. Its default
`c(1.96, 2.58, 3.89, 6)` is written as
[`conf_level_to_z`](https://bricenocenti.github.io/tabxplor/reference/conf_level_to_z.md)`(c(0.95, 0.99, 0.9999, 1 - 2e-9))`,
and its FIRST value is re-anchored to the significance threshold at
print time, so the remaining ones are read as spacings from it.
`adj_ratio`, `adj_diff` and `adj_diff_std` are the
[`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)-only
scales of `color = "adjustment"` / `"between_groups"` – how far a
modelled effect sits from the observed one (or from the reference
group's). Which one a column reads follows the estimate's own scale:
`adj_ratio` for a multiplicative effect (odds / risk / rate ratio),
`adj_diff` for a probability-scale marginal effect (in percentage
points), and `adj_diff_std` for an additive effect in the outcome's own
units (a gaussian beta, a count marginal effect), where the gap is
divided by SD(Y) so the same threshold means the same thing whatever
unit the outcome is recorded in. An empty/`NULL` scale drops that
measure for its column type.

Two rules shape a default, and a custom one is free to break them. A
ladder is MIRRORED unless the quantity it grades is bounded above: a
percentage ratio is capped at `1 / base`, so a cell can sit far below
its reference and never far above it, and `pct_ratio` is stricter below
(`list(over = c(1.1, 1.2, 1.5, 2), under = c(1.1, 1.25, 2, 4))`) – a
mean ratio, a rate ratio and a ratio of two estimates have no ceiling
and stay symmetric. And a fill is read at a glance, so on the BACKGROUND
channel the two ratio scales keep their two loudest rungs only: with the
default `color = TRUE` the text grades every deviation and the
background flags the ones whose RELATIVE size is out of proportion.

## Usage

``` r
set_color_palette(
  text_colors = NULL,
  text_colors_neg = NULL,
  background_colors = NULL,
  background_colors_neg = NULL,
  dark_text_colors = NULL,
  dark_text_colors_neg = NULL,
  dark_background_colors = NULL,
  dark_background_colors_neg = NULL,
  bg_legend_colors = NULL,
  bg_legend_colors_neg = NULL,
  theme = NULL
)

set_color_style(
  type = c("text", "bg"),
  theme = NULL,
  html_24_bit = NULL,
  custom_palette = NULL
)

get_color_style(
  mode = c("crayon", "color_code", "face"),
  type = NULL,
  theme = NULL,
  ...
)

set_color_breaks(breaks = NULL, ...)

get_color_breaks(brk, type = c("positive", "all"))
```

## Arguments

- text_colors, text_colors_neg, background_colors,
  background_colors_neg:

  Light-theme palettes (4 hex each): the text (font) and background
  (fill) colours for the over- (`*_colors`) and under-represented
  (`*_colors_neg`) sides.

- dark_text_colors, dark_text_colors_neg, dark_background_colors,
  dark_background_colors_neg:

  The dark-theme counterparts (4 hex each).

- bg_legend_colors, bg_legend_colors_neg:

  (4 hex each) The FONT stand-in for `background_colors` in the colour
  legend of media that cannot fill a run (Excel); the defaults are the
  background colours at -0.2 OKLCH lightness. Setting
  `background_colors` without these makes them follow it unchanged
  (readable only if your fills already are). There is no dark
  counterpart: an Excel legend cell is on a white page whatever the
  theme, and the dark fills read there as-is.

- theme:

  Which palette theme. In `set_color_palette()`: `"light"` or `"dark"`
  for the console / exports, or `"auto"` to detect the console's colour
  scheme now (the RStudio theme, the Positron theme, or `COLORFGBG`;
  `"light"` when it cannot be told). Detection is best-effort and
  resolved ONCE: call again after changing your editor's theme. (This is
  the console only —
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  /
  [`tab_html`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  take their own `theme = "auto"`, which follows the reader's browser.)
  In `get_color_style()`: `"light"`, `"dark"` or one of the
  black-and-white publication palettes (`"print_minimalistic"`,
  `"print_emphasis"`, `"print_marks"`); `"auto"` resolves to `"light"`
  there, a palette being always one definite thing. Both default to the
  current setting.

- type:

  Which palette, or which half of a break scale — the word means one
  thing per function, and both are given here because they share this
  page. In `get_color_style()` and the deprecated `set_color_style()`:
  `"text"` (font colour), `"bg"` (background fill), or `"bg_legend"`
  (`mode = "color_code"` only), the darker FONT stand-in for the
  background palette, for media that cannot fill a run (an Excel
  rich-text run) – see the colour legend. In `get_color_breaks()`:
  `"positive"` (the default) returns a readable form – a plain vector of
  magnitudes when the scale is symmetric, a `list(over =, under =)`
  otherwise – and `"all"` the signed / reciprocal thresholds the engine
  actually compares against (`c(-x, x)` for additive scales, `c(1/x, x)`
  for multiplicative ones).

- html_24_bit:

  **\[deprecated\]** Inert since 2.0.0 (exports are always 24-bit).

- custom_palette:

  **\[deprecated\]** A former 10/11-slot palette; its 4 over- and 4
  under-represented colours are mapped onto `set_color_palette()`.

- mode:

  By default, `get_color_style` returns a list of terminal (ANSI)
  coloring functions (the historical value `"crayon"`, now built with
  cli). Set to `"color_code"` to return html color codes, or `"face"` to
  return the palette's TYPOGRAPHY – a list `bold` / `italic` /
  `underline` of 8 logicals each (plus a `semantic` flag), which is how
  a publication palette says "over-represented cells are bold,
  under-represented ones italic". The colour palettes report bold on
  every text slot and nothing on the background ones, i.e. exactly how
  they have always been drawn.

- ...:

  Scales passed individually and named, e.g.
  `set_color_breaks(pct_diff = c(0.05, 0.1, 0.2), mean_ratio = c(1.15, 1.5, 2, 4))`.
  Each value is either a plain vector of signed / reciprocal literals
  (negatives, or ratios \< 1, are the under-represented side; a
  one-sided vector auto-mirrors; `NA` skips an intensity slot) or a
  `list(over =, under =)` of magnitudes (no mirror; omit a side to
  switch it off, e.g. `list(over = 2)` for the "only x2" rule). The old
  `pct_breaks` / `mean_breaks` / `contrib_breaks` arguments are
  soft-deprecated but still work (mapped onto the new scales).

- breaks:

  A named list of scales to set, e.g.
  `list(pct_diff = c(0.05, 0.1, 0.2, 0.3), pct_ratio = list(over = 2))`.
  Unset scales keep their current value.

- brk:

  When missing, return the full named list of the ten break scales – the
  same shape `set_color_breaks` accepts, so it round-trips (an
  asymmetric scale comes back as `list(over =, under =)`, a standardized
  one with `std = TRUE`). Specify one scale name to return only its
  breaks. The old aliases `"pct"` (-\> `pct_diff`) and `"mean"` (-\>
  `mean_ratio`) are still accepted.

## Value

Sets the internal color palettes (invisibly) and the option
`"tabxplor.color_style_theme"`.

A list of 8 terminal (ANSI) color-style functions, a vector of 8 color
html codes, or (`mode = "face"`) the palette's typography record.

Sets the global option "tabxplor.color_breaks" (a named list of scales)
and returns it invisibly.

The color breaks as a double vector or a `list(over =, under =)`, or a
named list of these.

## Details

`set_color_palette()` customises the palette used to print
[`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md). Each
palette is 4 hex codes ordered faint -\> strong. Provide only the ones
you want to change; the OKLCH defaults are used otherwise. The ANSI
styles are (re)built once, not per cell.

## Functions

- `set_color_style()`: **\[deprecated\]** Superseded by
  `set_color_palette()`. Kept as a back-compat shim: `type`/`theme`
  still take effect (as options); `custom_palette` maps its over/under
  colours onto the new 4+4 palette; `html_24_bit` is inert (exports are
  always 24-bit).

- `get_color_style()`: get the color palette as terminal (ANSI) style
  functions or html codes: an 8-element vector (4 over-represented
  intensities then 4 under-represented), indexed by the engine slot.

- `set_color_breaks()`: set the breaks used to print colors.

- `get_color_breaks()`: get the color breaks currently in use, in the
  canonical shape.

## Examples

``` r
set_color_palette(text_colors = c("#02a5b3", "#0891c9", "#0267c7", "#300dfd"))
set_color_breaks(
  pct_diff   = c(0.05, 0.15, 0.3),
  pct_ratio  = list(over = 2),
  mean_ratio = c(1.15, 2, 4),
  contrib    = c(1, 2, 5)
)
set_color_breaks(get_color_breaks())   # a no-op: the shape round-trips
```
