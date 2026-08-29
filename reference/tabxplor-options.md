# The tabxplor options, and their defaults

`tabxplor` reads its display, colour, statistics and export defaults
from [`options()`](https://rdrr.io/r/base/options.html), all prefixed
`tabxplor.`. Set any of them for a session with
[`options()`](https://rdrr.io/r/base/options.html), e.g.
`options(tabxplor.stars = TRUE)`, or once at the top of a script or
`.Rmd`. The defaults are established when the package loads
(`.onLoad()`); most also have a per-call argument on the relevant
function, which always wins over the option.

## Display and printing

- `tabxplor.print`:

  `"console"` (default): how a table auto-prints. `"html"` renders the
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  table (in the Viewer pane in RStudio/Positron, and as a real html
  table in rmarkdown/Quarto documents) — recommended when you work in an
  IDE with a Viewer. `"kable"` is an accepted synonym of `"html"` (the
  pre-2.0.0 name).

- `tabxplor.stars`:

  `FALSE` (default): whether cells show significance stars, and at which
  cut-offs. `FALSE` (no stars), `TRUE` (the default ladder
  `c("*" = 0.10, "**" = 0.05, "***" = 0.01)`), or a named numeric giving
  your own – names are the glyphs, values the p-value cut-offs, e.g.
  `options(tabxplor.stars = c("*" = 0.05, "**" = 0.01))`. Off for
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  on for
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md).
  The LADDER is a render-time reading of each cell's stored p-value, so
  it is this option alone — change it and every table already built
  shows the new glyphs. Per-call `stars =`.

- `tabxplor.ratio_print`:

  `"inverse"` (default): prints a multiplicative value below its
  reference as the inverse — an odds ratio of 0.37 as `1/2.67`, a mean
  ratio of 0.42 as `/2.4` — so "2.7 times less" reads as strongly as
  "2.7 times more", and the same in a bracket. `"raw"` prints the plain
  number (`0.37`), the convention of most journals.

- `tabxplor.n`:

  `"range"` (default): how many people a table says are behind its
  numbers. `"range"` puts the unweighted base beside the Total cell of a
  crosstab (`100% (9 838)`) and in the `n` column of a regression table,
  printed as `min-max` when the blocks rest on different populations —
  several column variables losing different `NA`s, or several models.
  `"min"` prints the smallest base only, `"no"` shows no count at all.
  It replaces the `add_n` argument, deprecated in 2.0.0. Per-call `n =`.

- `tabxplor.color_whole_cell`:

  `FALSE` (default): EXPERT. A cell that prints SEVERAL fields reads as
  one number with an aside — `1/1.63*** (31%)` — so the cell's rendering
  grades the number and the aside is set slightly back from the table's
  own text, following the theme. That covers the colour and, under
  `theme = "print"`, the typography (bold, italic, underline) alike. Set
  to `TRUE` to extend the primary's own rendering over the whole cell
  instead (the pre-2.0.0 look). There is nothing to choose beyond that:
  which grey an aside takes belongs to the theme's palette, not to a
  per-cell option — see
  [`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md).
  Console, html and Markdown; Excel renders a cell as a whole either
  way.

- `tabxplor.var_names`:

  `"both"` (default): which variable names the exporters annotate:
  `"both"`, `"rows"`, `"cols"`, `"none"`. Per-call `var_names =`.

- `tabxplor.var_labels`:

  `FALSE` (default): in *exports* (markdown / html / Excel / plot), show
  a variable's *label* (the `haven`/`labelled` `label` attribute, if it
  has one) instead of its name. Display only – the table structure keeps
  canonical names, so name-based
  [`select()`](https://dplyr.tidyverse.org/reference/select.html) and
  references still work; the console always shows names.

- `tabxplor.cleannames`:

  `FALSE` (default): clean up variable/level names in output. Also
  strips a `"1-"`-style prefix from `labelled` value labels turned into
  factor levels. Per-call `cleannames =`.

- `tabxplor.total_names`:

  `c(row = "Total", col = "Total", tab = "Ensemble", other = "Others")`
  (default): the four synthetic labels a table carries: `row` and `col`
  name the total row and the total column, `tab` the total *table* (the
  one made when there are `tab_vars`), and `other` the level
  `other_if_less_than` lumps small levels into. A partial vector is
  allowed –
  `options(tabxplor.total_names = c(tab = "Ensemble", other = "Autres"))`
  leaves the first two alone. It replaces the `total_names` /
  `totaltab_name` / `other_level` arguments, deprecated in 2.0.0.

- `tabxplor.shape_auto_max`:

  `12L` (default): where `shape = "auto"` draws the line for a numeric
  row or tab variable: a column with at most this many distinct
  **whole** values is a counted number or a short scale, and keeps one
  level per value; anything else is continuous and is cut into
  `"sd_bands"`. Raise it for a long scale, lower it to band more eagerly
  — or name the variable in `shape` and decide yourself.

## Colours and theme

- `tabxplor.color_breaks`:

  the colour-break scales (a named list of `pct_diff`, `pct_ratio`,
  `odds_ratio`, `mean_diff`, `mean_ratio`, `contrib`, `zscore`,
  `adj_ratio`, `adj_diff`, `adj_diff_std`). Set with
  [`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md).
  Per-call `color_breaks =`.

- `tabxplor.color_style_theme` (alias `tabxplor.console_theme`):

  the *console* palette theme, `"light"` or `"dark"`; set by
  [`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
  (which auto-detects the editor theme on load). NOT the export theme
  (`tabxplor.theme` / `tabxplor.export_theme`).

- `tabxplor.console_bold`:

  whether to embolden the reference / total (and coloured) cells in the
  *console*, `TRUE` or `FALSE`. Auto-detected at load: `TRUE` in
  Positron and VS Code (which render ANSI bold at a fixed glyph width),
  `FALSE` in RStudio and unknown consoles (there bold is drawn wider and
  would break column alignment). Override it for your own front-end /
  font.

- `tabxplor.theme` (alias `tabxplor.export_theme`):

  `"light"` (default): the *export* theme: `"light"`, `"dark"`, `"auto"`
  (follow the reader), or a black-and-white **publication** palette –
  `"print_ready"` picks one per table (marks for a cross-table, the
  emphasis ladder for a regression), or name it yourself:
  `"print_marks"`, `"print_emphasis"`, `"print_minimalistic"` (`"bw"` is
  a synonym of the last). See
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  for what each says. `"auto"` needs a stylesheet, so only
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  and
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  honour it; static backends resolve it to `"light"`. A publication
  palette reaches every backend, Excel included. Per-call `theme =`.

- `tabxplor.print_rules`:

  `TRUE` (default): every stylesheet
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  emits also carries a black-and-white publication palette inside an
  `@media print` block, so a table rendered in colour **prints** (or
  saves to PDF) publication-ready with no further action. Set `FALSE` if
  your printer is a colour one and the colours are the point, or name a
  palette (`"print_emphasis"`) to print in that one instead of the
  default `"print_minimalistic"`. `"print_marks"` and `"print_ready"`
  cannot be used here: their marks are cell text, and a print rule can
  restyle a page but not add characters to it. Per-call `print_rules =`.

- `tabxplor.background`:

  `"page"` (default): what a rendered table paints behind itself.
  `"page"` leaves it **transparent**, so the table sits on the page's
  own ground, whatever that is; `"theme"` paints the theme's own
  background, a card of its own; or name any CSS colour. Change it only
  where the page is not yours to follow — a dark table dropped into a
  light document, an html email. The interactive Viewer page paints
  itself either way, and a **publication** palette is always a sheet of
  white paper.

## Statistics and confidence intervals

- `tabxplor.anova`:

  `"welch"` (default): which one-way ANOVA F is shown for mean columns:
  `"welch"` (robust) or `"classic"` (pooled variance). Both are always
  stored in the `test` attribute. Per-call `anova =`.

- `tabxplor.design_effect`:

  `FALSE` (default): a weighted
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  estimates the population but bases every interval and test on the raw
  number of respondents, so they carry no design effect — and the
  table's footer says so. Set `TRUE` and the same intervals **account
  for the unequal weighting, exactly**: a weight column IS a survey
  design (the flat one, `ids = ~1`), whose variance has a closed form in
  the per-cell `sum(w^2)` the aggregate already computes, so the base
  becomes `n_eff = p(1-p) / Var_design(p)` in **every weighted
  descriptive confidence interval** — factor proportions *and* means
  (cell, difference, ratio and the `color = "odds_ratio"` significance)
  — and the whole-table tests (`test = TRUE`) become
  [`survey::svychisq`](https://rdrr.io/pkg/survey/man/svychisq.html) / a
  `svyglm` Wald F on that flat design. It reproduces `survey` to the
  last digit, Kish's `(sum w)^2 / sum(w^2)` being that same formula with
  each cell's own `sum(w^2)` discarded. Being exact rather than a bound,
  it can make an interval *narrower* as well as wider. It is blind to
  **clustering** and to **calibration**, which the weights do not record
  — and those are not symmetric: missing the calibration and the strata
  costs a few percent, in the safe direction, while missing the clusters
  of a face-to-face household survey can leave an interval several times
  too short (see the Weights section of
  [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)).
  It needs the microdata weights, so
  [`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
  on pre-aggregated counts cannot apply it (such a table states the raw
  basis in its footer rather than claiming a correction it does not
  have). **Scope:
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  and its leaves only.**
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  never reads it — its crude `empirical =` companions are always on the
  weighted basis, beside a model column
  ([`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html)) that
  always was. For the full design effect — strata, clusters, `fpc`,
  calibration — pass a
  [`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html)
  as `data`; the option is then not consulted at all. Per-call
  `design_effect =`.

- `tabxplor.conf_level`:

  `0.95` (default): confidence level for the intervals and significance
  tests. Since 2.0.0 each column records the level it was built at, so
  the colour thresholds follow the argument and this option is the
  fallback for a column that never recorded one (a hand-built
  [`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md),
  or a table from an older session). Per-call `conf_level =`.

- `tabxplor.legend_style`:

  `"prose"` (default): the colour-legend style in exports
  ([`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)):
  `"prose"` (full sentences) or `"terse"` (the compact one-line form the
  console uses). The console itself is always terse.

- `tabxplor.test_lines`:

  `"summary"` (default): how many crosstab test rows the exporters
  ([`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md))
  append: `"summary"` (p-value + effect size), `"all"` (+ the raw
  statistic), `"stat"` (p-value + statistic), or `"pvalue"` (the single
  p-value row). The p-value row name states the test used ("pvalue
  (Chi2, Welch F; survey-design)") and the effect-size row its measure
  ("Cramer's V, eta2"). N is never added – it is already shown by the
  `n` column. The console block always shows N + p-value + effect size.

- `tabxplor.shape_table` (alias `tabxplor.spark`):

  `"all"` (default): in a
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  table, each continuous predictor's OBSERVED SHAPE — the outcome binned
  against the predictor, on the model's scale, with no model in it —
  drawn as a small curve in a **shape table** below the footer, beside
  the range it is a picture of (`13-57% (OR 8.7)`). It is the eye-half
  of the `Linearity` footer row, and the free one: no fit is involved.
  With `tab_vars`, one curve per group; with several outcomes, one per
  outcome. `"all"` draws it in every medium; `"console"` only where you
  are working, so exported tables stay unchanged; `"no"` never. `TRUE` /
  `FALSE` are accepted for the first and the last. The curve is drawn TO
  SCALE on the predictor as the model sees it, so a `shape` transform
  visibly straightens it when it is the right cure, and every
  predictor's curve is the same width. Its vertical window is floored by
  the data's own sampling noise, so a curve smaller than that is greyed
  and marked `ns` — read it as a flat line whatever its shape. In HTML
  the glyphs become an inline SVG; a plot never draws them (no
  graphics-device font has them). An ordinal or multinomial outcome has
  one curve per cut or per category and this draws only the first:
  [`reg_check_plots()`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
  shows them all.

## HTML / [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md) export

- `tabxplor.tab_kable_css` (alias `tabxplor.kable_css`):

  `TRUE` (default): inline the stylesheet with each
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  /
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  table (self-contained). Set `FALSE` in a many-table document that
  emits
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  once at the top. Per-call `css =`.

- `tabxplor.tab_kable_tooltips`:

  `TRUE` (default): show the per-cell hover tooltips (counts, confidence
  intervals, differences...) in html tables. Set `FALSE` once per
  document when every table auto-prints and tooltips are unwanted.
  Per-call `tooltips =`.

- `tabxplor.kable_popover`:

  `FALSE` (default): use click popovers instead of hover tooltips.
  Per-call `popover =`.

- `tabxplor.tab_kable_num_font`:

  the HTML/markdown number-font CSS stack. Monospace by default so
  figures stay column-aligned (set a proportional stack to revert).

- `tabxplor.output_kable`:

  `FALSE` (default): make
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  render its result with
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  before returning it — a convenience for `.Rmd`/`.qmd` documents. Since
  2.0.0 it only *renders*: it no longer changes the shape of the built
  object (that is `output_list`).

## Excel / [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md) export

- `tabxplor.xl_font_text`:

  `"DejaVu Sans Condensed"` (default): text (labels/headers) font.
  Per-call `font_text =`.

- `tabxplor.xl_font_num`:

  `"DejaVu Sans"` (default): number font without stars. xlsx records ONE
  name (no fallback list), so set a font installed where the workbook is
  opened. Per-call `font_num =`.

- `tabxplor.xl_font_num_stars`:

  `"Cascadia Mono"` (default): number font with stars (monospace, so
  stars align). Per-call `font_num_stars =`.

- `tabxplor.xl_ratio_cells`:

  `"fold"` (default): what a ratio / odds-ratio cell HOLDS in the
  workbook: `"fold"` (the default) the signed fold, so Excel prints what
  the console prints and the cell stays a number; `"raw"` the
  untransformed ratio; `"text"` the exact display string. Per-call
  `ratio_cells =`.

## Plot, paths and language

- `tabxplor.export_dir`:

  `NULL` (default): default directory for exported files (`NULL` = the
  working / typed path).

- `tabxplor.lang`:

  `"auto"` (default): the colour-legend language: `"auto"` (follows the
  R/OS locale), `"en"` or `"fr"`. Per-call `lang =`.

## Parallel build

- `tabxplor.parallel`:

  `FALSE` (default): build the independent units of one call on parallel
  CPU cores (needs the `mirai` package): the per-`row_var` tables of a
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  the models / `tab_vars` groups / outcomes of a
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md).
  The result is byte-identical to the serial one. `FALSE` (default)
  never dispatches. `"auto"` (or `TRUE`) takes **half the cores this
  session may actually use, at least 2 and at most 4** — so 2 on a
  dual-core laptop, 2 on a 4-core machine, 4 on 8 cores or more. An
  integer takes that many verbatim. The count respects
  `options(mc.cores)`, a container's CPU quota and an HPC allocation (it
  reads them through the `parallelly` package when installed), and never
  exceeds 2 under `R CMD check`. It stays OPT-IN because starting the
  pool BLOCKS for about a second, so the first parallel table of a
  session is always slower than the serial one; from the third it is
  ahead. It pays off for MANY evenly sized units against a small or
  medium data frame — 24 tables run about 2.8x faster on 4 workers — and
  is a loss for few units or multi-million-row data, where shipping the
  population to each worker eats the gain. Set it once at the top of a
  script: `options(tabxplor.parallel = "auto")`. A model comparison
  (`stats = "compare_*"`) is always serial and says so when asked: it is
  a test BETWEEN the fits, so they are built together. For one call
  only, wrap it in
  `withr::with_options(list(tabxplor.parallel = "auto"), ...)`. The pool
  persists for the session; release it with
  [`tab_parallel_stop()`](https://bricenocenti.github.io/tabxplor/reference/tab_parallel_stop.md).

- `tabxplor.parallel_min`:

  `2L` (default): the smallest UNIT count worth dispatching – `row_var`s
  for
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  models for
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  (fewer runs serially, since the setup would outweigh the gain).
