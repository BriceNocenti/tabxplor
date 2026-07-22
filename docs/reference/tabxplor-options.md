# tabxplor global options

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

  `"console"` (default) or `"kable"`: how a table auto-prints.

- `tabxplor.stars`:

  `FALSE` (default): whether cells show significance stars
  (`*`/`**`/`***`). Off for
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  on for
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md).
  Per-call `stars =`.

- `tabxplor.signif_levels`:

  p-value cut-offs for the stars, default `c(0.10, 0.05, 0.01)`.

- `tabxplor.signif_labels`:

  the star labels, default `c("*", "**", "***")`.

- `tabxplor.ci_print`:

  `"ci"` (default) shows the `[inf; sup]` interval; `"moe"` shows the
  larger half-width (margin of error).

- `tabxplor.totcol_range`:

  how a Total column's in-cell base is shown when a table's column
  variables have differing bases (e.g. `na = "drop"`): `"off"` (default)
  each row's own base; `"range"` the per-row `[min;max]`; `"min"` the
  smallest (safest) base.

- `tabxplor.var_names`:

  which variable names the exporters annotate: `"both"` (default),
  `"rows"`, `"cols"`, `"none"`. Per-call `var_names =`.

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
  factor levels.

## Colours and theme

- `tabxplor.color_breaks`:

  the colour-break scales (a named list of `pct_diff`, `pct_ratio`,
  `odds_ratio`, `mean_diff`, `mean_ratio`, `contrib`). Set with
  [`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md).

- `tabxplor.color_style_theme` (alias `tabxplor.console_theme`):

  the *console* palette theme, `"light"` or `"dark"`; set by
  [`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
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

  the *export* theme, `"light"` (default), `"dark"` or `"auto"` (follow
  the reader). `"auto"` needs a stylesheet, so only
  `tab_kable(engine = "html")`,
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  and
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  honour it; static backends resolve it to `"light"`.

## Statistics and confidence intervals

- `tabxplor.anova`:

  which one-way ANOVA F is shown for mean columns: `"welch"` (default,
  robust) or `"classic"` (pooled variance). Both are always stored in
  the `test` attribute.

- `tabxplor.test_lines`:

  how many crosstab test rows the exporters
  ([`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md))
  append: `"summary"` (default: p-value + effect size), `"all"` (+ the
  raw statistic), `"stat"` (p-value + statistic), or `"pvalue"` (the
  single p-value row). The p-value row name states the test used
  ("pvalue (Chi2, Welch F; Kish)") and the effect-size row name its
  measure ("Cramer's V, eta2"). N is never added – it is already shown
  by `add_n`. The console summary block always shows N + p-value +
  effect size.

- `tabxplor.legend_style`:

  the colour-legend style in exports
  ([`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md),
  [`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)):
  `"prose"` (default, full sentences) or `"terse"` (the compact one-line
  form the console uses). The console itself is always terse.

- `tabxplor.kish_neff`:

  `FALSE` by default (weighted estimate, raw unweighted n). Set to
  `TRUE` to replace that raw n with Kish's effective sample size
  `n_eff = (sum w)^2 / sum(w^2)` in **every weighted descriptive
  confidence interval** – factor proportions *and* means (cell,
  difference, ratio and the `color = "OR"` significance) in
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) /
  [`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md)
  /
  [`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md),
  and the crude `empirical =` companions of
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md).
  Under unequal weights `n_eff < n`, so the intervals widen honestly
  (they otherwise carry no design effect and run too narrow). It also
  switches the whole-table tests (`test = TRUE`) to a first-order
  Rao-Scott correction – the factor chi-square rescaled to `n_eff`, the
  numeric F on per-group `n_eff`. This is a single-stage unequal-weight
  approximation, not a design-based analysis: it needs the microdata
  weights, so
  [`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
  on pre-aggregated counts cannot apply it. The regression *model* CIs
  of
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  are already fully design-based
  ([`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html)) and
  are unaffected. For a full design-based whole-table test (clusters /
  strata) use `test = "survey"` with `wt =`/`strata =`/`ids =`, or pass
  a [`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html)
  as `data`.

- `tabxplor.conf_level`:

  confidence level for the intervals and significance tests, default
  `0.95`. The per-call `conf_level =` argument of
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  [`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md),
  [`tab_ci()`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  and
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  overrides it.

## HTML / [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md) export

- `tabxplor.tab_kable_engine`:

  `"html"` (default, dependency-free) or `"kableExtra"` (the legacy
  engine; needs the suggested `kableExtra` package).

- `tabxplor.tab_kable_css` (formerly `tabxplor.kable_css`, still
  accepted):

  `TRUE` (default): inline the stylesheet with each
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  /
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  table (self-contained). Set `FALSE` in a many-table document that
  emits
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  once at the top.

- `tabxplor.always_add_css_in_tab_kable`:

  `TRUE` (default): always include `tab.css` in the kableExtra engine's
  output (a legacy-engine knob).

- `tabxplor.kable_html_font`:

  the CSS font stack for the kableExtra engine.

- `tabxplor.kable_popover`:

  `FALSE` (default): use click popovers instead of hover tooltips.

- `tabxplor.tab_kable_num_font`:

  the HTML/markdown number-font CSS stack. Monospace by default so
  figures stay column-aligned (set a proportional stack to revert).

- `tabxplor.output_kable`:

  `FALSE` (default): internal switch to return kable output.

## Excel / [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md) export

- `tabxplor.xl_font_text`:

  text (labels/headers) font, default `"DejaVu Sans Condensed"`.

- `tabxplor.xl_font_num`, `tabxplor.xl_font_num_stars`:

  number font without / with stars, defaults `"DejaVu Sans"` and
  `"Cascadia Mono"` (monospace, so stars align). xlsx records ONE name
  (no fallback list), so set a font installed where the workbook is
  opened.

- `tabxplor.xl_or_numeric`:

  `FALSE` (default): keep odds ratios as numbers rather than `1/x` text.
  Per-call `tab_xl(or_numeric =)`.

## Plot, paths and language

- `tabxplor.plot_num_font`:

  the
  [`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)
  number font, applied only when the table shows stars, default
  `"Cascadia Mono"` (`""` keeps the ggpubr default).

- `tabxplor.export_dir`:

  default directory for exported files (`NULL` = the working / typed
  path).

- `tabxplor.lang`:

  the colour-legend language: `"auto"` (default, follows the R/OS
  locale), `"en"` or `"fr"`. Per-call `lang =`.

## Parallel build

- `tabxplor.parallel`:

  `FALSE` (default): build the per-`row_var` tables of one
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  call on a background worker pool (needs the suggested `mirai`
  package). `TRUE` = auto workers, an integer = that many daemons.
  Per-call `parallel =`. Release the pool with
  [`tab_parallel_stop()`](https://bricenocenti.github.io/tabxplor/reference/tab_parallel_stop.md).

- `tabxplor.parallel_min`:

  `2L` (default): the smallest `row_var` count worth dispatching (fewer
  runs serially, since the setup would outweigh the gain).

## jamovi live cache

- `tabxplor.jmv_full_hash`:

  `FALSE` (default): the jamovi module caches (Crosstables and
  Regressions) fingerprint each data column cheaply by its class, factor
  levels and number of missing values, so an unrelated edit does not
  invalidate every table. A same-shape value edit (values changed but
  class, levels and NA-count unchanged) is therefore not detected and
  can serve a stale cached result until the next structural change. Set
  to `TRUE` to hash the full column values instead (exact, slightly
  slower) if you edit data in place and need every change caught.
