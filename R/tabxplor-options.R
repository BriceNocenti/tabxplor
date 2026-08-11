# PURPOSE: Single documentation page for every tabxplor.* global option.
# ROLE: A documentation-only object (`?tabxplor-options`). The DEFAULTS live in .onLoad() (utils.R);
#   this file only describes them, so users can discover and tune them in one place.
# KEY CONSTRAINTS:
#   - Keep this in sync with .onLoad() in R/utils.R (the single source of truth for defaults).
#   - Every option a user might set belongs here; purely internal knobs are noted as such.

#' tabxplor global options
#'
#' `tabxplor` reads its display, colour, statistics and export defaults from `options()`, all
#' prefixed `tabxplor.`. Set any of them for a session with [options()], e.g.
#' `options(tabxplor.stars = TRUE)`, or once at the top of a script or `.Rmd`. The defaults are
#' established when the package loads (`.onLoad()`); most also have a per-call argument on the
#' relevant function, which always wins over the option.
#'
#' @section Display and printing:
#' \describe{
#'   \item{`tabxplor.print`}{`"console"` (default) or `"html"`: how a table auto-prints.
#'     `"html"` renders the [tab_html()] table (in the Viewer pane in RStudio/Positron, and
#'     as a real html table in rmarkdown/Quarto documents) — recommended when you work in an
#'     IDE with a Viewer. `"kable"` is an accepted synonym of `"html"` (the pre-2.0.0 name).}
#'   \item{`tabxplor.stars`}{`FALSE` (default): whether cells show significance stars
#'     (`*`/`**`/`***`). Off for [tab()], on for [tab_reg()]. Per-call `stars =`.}
#'   \item{`tabxplor.signif_levels`}{p-value cut-offs for the stars, default `c(0.10, 0.05, 0.01)`.}
#'   \item{`tabxplor.signif_labels`}{the star labels, default `c("*", "**", "***")`.}
#'   \item{`tabxplor.ci_print`}{`"ci"` (default) shows the `[inf; sup]` interval; `"moe"` shows the
#'     larger half-width (margin of error).}
#'   \item{`tabxplor.var_names`}{which variable names the exporters annotate: `"both"` (default),
#'     `"rows"`, `"cols"`, `"none"`. Per-call `var_names =`.}
#'   \item{`tabxplor.var_labels`}{`FALSE` (default): in *exports* (markdown / html / Excel / plot),
#'     show a variable's *label* (the `haven`/`labelled` `label` attribute, if it has one) instead of
#'     its name. Display only -- the table structure keeps canonical names, so name-based `select()`
#'     and references still work; the console always shows names.}
#'   \item{`tabxplor.cleannames`}{`FALSE` (default): clean up variable/level names in output. Also
#'     strips a `"1-"`-style prefix from `labelled` value labels turned into factor levels.}
#' }
#'
#' @section Colours and theme:
#' \describe{
#'   \item{`tabxplor.color_breaks`}{the colour-break scales (a named list of `pct_diff`,
#'     `pct_ratio`, `odds_ratio`, `mean_diff`, `mean_ratio`, `contrib`, `zscore`). Set with [set_color_breaks()].}
#'   \item{`tabxplor.color_style_theme` (alias `tabxplor.console_theme`)}{the *console* palette theme,
#'     `"light"` or `"dark"`; set by [set_color_palette()] (which auto-detects the editor theme on load).
#'     NOT the export theme (`tabxplor.theme` / `tabxplor.export_theme`).}
#'   \item{`tabxplor.console_bold`}{whether to embolden the reference / total (and coloured) cells in the
#'     *console*, `TRUE` or `FALSE`. Auto-detected at load: `TRUE` in Positron and VS Code (which render
#'     ANSI bold at a fixed glyph width), `FALSE` in RStudio and unknown consoles (there bold is drawn
#'     wider and would break column alignment). Override it for your own front-end / font.}
#'   \item{`tabxplor.theme` (alias `tabxplor.export_theme`)}{the *export* theme, `"light"` (default),
#'     `"dark"`, `"print"` (the black-and-white publication palette; `"bw"` is a synonym) or `"auto"`
#'     (follow the reader). `"auto"` needs a stylesheet, so only `tab_kable(engine = "html")`,
#'     [tab_md()] and [tab_css()] honour it; static backends resolve it to `"light"`. `"print"` reaches
#'     every backend, Excel included.}
#'   \item{`tabxplor.print_rules`}{`TRUE` (default): every stylesheet [tab_css()] emits also carries the
#'     black-and-white publication palette inside an `@media print` block, so a table rendered in colour
#'     **prints** (or saves to PDF) publication-ready with no further action. Set `FALSE` if your printer
#'     is a colour one and the colours are the point.}
#' }
#'
#' @section Statistics and confidence intervals:
#' \describe{
#'   \item{`tabxplor.anova`}{which one-way ANOVA F is shown for mean columns: `"welch"` (default,
#'     robust) or `"classic"` (pooled variance). Both are always stored in the `test` attribute.}
#'   \item{`tabxplor.kish_neff`}{`FALSE` by default (weighted estimate, raw unweighted n). Set to
#'     `TRUE` to replace that raw n with Kish's effective sample size `n_eff = (sum w)^2 / sum(w^2)`
#'     in \strong{every weighted descriptive confidence interval} -- factor proportions \emph{and}
#'     means (cell, difference, ratio and the `color = "OR"` significance) in [tab()] / [tab_num()] /
#'     [tab_counts()], and the crude `empirical =` companions of [tab_reg()]. Under unequal weights
#'     `n_eff < n`, so the intervals widen honestly (they otherwise carry no design effect and run too
#'     narrow). It also switches the whole-table tests (`test = TRUE`) to a first-order Rao-Scott
#'     correction -- the factor chi-square rescaled to `n_eff`, the numeric F on per-group `n_eff`.
#'     This is a single-stage unequal-weight approximation, \strong{not the design effect}: Kish's
#'     `deff = 1 + CV^2(w)` is a property of the weights alone, so it is blind to \strong{clustering}
#'     (which inflates the variance) and to \strong{calibration} (which shrinks it), and it needs the
#'     microdata weights, so [tab_counts()] on pre-aggregated counts cannot apply it. The regression
#'     \emph{model} CIs of [tab_reg()] are already fully design-based (\code{survey::svyglm}) and are
#'     unaffected. For the real design effect -- in the tests \emph{and} in every interval -- pass a
#'     \code{survey::svydesign} as `data`; the option is then not consulted at all.}
#'   \item{`tabxplor.conf_level`}{confidence level for the intervals and significance tests, default
#'     `0.95`. The per-call `conf_level =` argument of [tab()], [tab_num()], [tab_ci()] and [tab_reg()]
#'     overrides it: since 2.0.0 each column records the level it was built at, so the colour
#'     thresholds follow the argument and this option is the fallback for a column that never recorded
#'     one (a hand-built [fmt()], or a table from an older session).}
#'   \item{`tabxplor.legend_style`}{the colour-legend style in exports ([tab_md()], [tab_kable()],
#'     [tab_xl()], [tab_plot()]): `"prose"` (default, full sentences) or `"terse"` (the compact
#'     one-line form the console uses). The console itself is always terse.}
#'   \item{`tabxplor.test_lines`}{how many crosstab test rows the exporters ([tab_md()], [tab_html()],
#'     [tab_xl()]) append: `"summary"` (default: p-value + effect size), `"all"` (+ the raw statistic),
#'     `"stat"` (p-value + statistic), or `"pvalue"` (the single p-value row). The p-value row name states
#'     the test used ("pvalue (Chi2, Welch F; Kish)") and the effect-size row name its measure ("Cramer's
#'     V, eta2"). N is never added -- it is already shown by `add_n`. The console summary block always
#'     shows N + p-value + effect size.}
#'   \item{`tabxplor.spark`}{`TRUE` (default): in a [tab_reg()] table, a continuous predictor's row
#'     label ends with a small curve showing the OBSERVED shape of its effect (ten bins of the outcome
#'     against the predictor, on the model's scale, with no model in it) --- the eye-half of the
#'     `Linearity` footer row. `"ascii"` uses a plain-text ladder for a console or a LaTeX font without
#'     block characters; `FALSE` removes it. In HTML the glyphs become an inline SVG; a plot never
#'     draws them (no graphics-device font has them).}
#' }
#'
#' @section HTML / `tab_kable()` export:
#' \describe{
#'   \item{`tabxplor.tab_kable_engine`}{`"html"` (default, dependency-free) or `"kableExtra"` (the
#'     legacy engine; needs the suggested `kableExtra` package).}
#'   \item{`tabxplor.tab_kable_css` (formerly `tabxplor.kable_css`, still accepted)}{`TRUE` (default):
#'     inline the stylesheet with each `tab_kable()` / [tab_md()] table (self-contained). Set `FALSE`
#'     in a many-table document that emits [tab_css()] once at the top.}
#'   \item{`tabxplor.always_add_css_in_tab_kable`}{`TRUE` (default): always include `tab.css` in the
#'     kableExtra engine's output (a legacy-engine knob).}
#'   \item{`tabxplor.kable_html_font`}{the CSS font stack for the kableExtra engine.}
#'   \item{`tabxplor.tab_kable_tooltips`}{`TRUE` (default): show the per-cell hover tooltips
#'     (counts, confidence intervals, differences...) in html tables. Set `FALSE` once per
#'     document when every table auto-prints and tooltips are unwanted. Per-call `tooltips =`.}
#'   \item{`tabxplor.kable_popover`}{`FALSE` (default): use click popovers instead of hover tooltips.}
#'   \item{`tabxplor.tab_kable_num_font`}{the HTML/markdown number-font CSS stack. Monospace by
#'     default so figures stay column-aligned (set a proportional stack to revert).}
#'   \item{`tabxplor.output_kable`}{`FALSE` (default): internal switch to return kable output.}
#' }
#'
#' @section Excel / `tab_xl()` export:
#' \describe{
#'   \item{`tabxplor.xl_font_text`}{text (labels/headers) font, default `"DejaVu Sans Condensed"`.}
#'   \item{`tabxplor.xl_font_num`, `tabxplor.xl_font_num_stars`}{number font without / with stars,
#'     defaults `"DejaVu Sans"` and `"Cascadia Mono"` (monospace, so stars align). xlsx records ONE
#'     name (no fallback list), so set a font installed where the workbook is opened.}
#'   \item{`tabxplor.xl_or_numeric`}{`FALSE` (default): keep odds ratios as numbers rather than
#'     `1/x` text. Per-call `tab_xl(or_numeric =)`.}
#' }
#'
#' @section Plot, paths and language:
#' \describe{
#'   \item{`tabxplor.plot_num_font`}{the [tab_plot()] number font, applied only when the table shows
#'     stars, default `"Cascadia Mono"` (`""` keeps the ggpubr default).}
#'   \item{`tabxplor.export_dir`}{default directory for exported files (`NULL` = the working / typed
#'     path).}
#'   \item{`tabxplor.lang`}{the colour-legend language: `"auto"` (default, follows the R/OS locale),
#'     `"en"` or `"fr"`. Per-call `lang =`.}
#' }
#'
#' @section Parallel build:
#' \describe{
#'   \item{`tabxplor.parallel`}{`FALSE` (default): build the per-`row_var` tables of one [tab()] call
#'     on parallel CPU cores (needs the `mirai` package). `TRUE` = auto select number of cores,
#'     integer = that many cores. Release the pool with [tab_parallel_stop()].}
#'   \item{`tabxplor.parallel_min`}{`2L` (default): the smallest `row_var` count worth dispatching
#'     (fewer runs serially, since the setup would outweigh the gain).}
#' }
#'
#' @section jamovi live cache:
#' \describe{
#'   \item{`tabxplor.jmv_full_hash`}{`FALSE` (default): the jamovi module caches (Crosstables and
#'     Regressions) fingerprint each data column cheaply by its class, factor levels and number of
#'     missing values, so an unrelated edit does not invalidate every table. A same-shape value edit
#'     (values changed but class, levels and NA-count unchanged) is therefore not detected and can serve
#'     a stale cached result until the next structural change. Set to `TRUE` to hash the full column
#'     values instead (exact, slightly slower) if you edit data in place and need every change caught.}
#' }
#'
#' @name tabxplor-options
#' @aliases tabxplor.options
NULL
