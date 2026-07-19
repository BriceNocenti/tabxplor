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
#'   \item{`tabxplor.print`}{`"console"` (default) or `"kable"`: how a table auto-prints.}
#'   \item{`tabxplor.stars`}{`FALSE` (default): whether cells show significance stars
#'     (`*`/`**`/`***`). Off for [tab()], on for [tab_reg()]. Per-call `stars =`.}
#'   \item{`tabxplor.signif_levels`}{p-value cut-offs for the stars, default `c(0.10, 0.05, 0.01)`.}
#'   \item{`tabxplor.signif_labels`}{the star labels, default `c("*", "**", "***")`.}
#'   \item{`tabxplor.ci_print`}{`"ci"` (default) shows the `[inf; sup]` interval; `"moe"` shows the
#'     larger half-width (margin of error).}
#'   \item{`tabxplor.totcol_range`}{how a Total column's in-cell base is shown when a table's column
#'     variables have differing bases (e.g. `na = "drop"`): `"off"` (default) each row's own base;
#'     `"range"` the per-row `[min;max]`; `"min"` the smallest (safest) base.}
#'   \item{`tabxplor.var_names`}{which variable names the exporters annotate: `"both"` (default),
#'     `"rows"`, `"cols"`, `"none"`. Per-call `var_names =`.}
#'   \item{`tabxplor.cleannames`}{`FALSE` (default): clean up variable/level names in output.}
#' }
#'
#' @section Colours and theme:
#' \describe{
#'   \item{`tabxplor.color_breaks`}{the colour-break scales (a named list of `pct_diff`,
#'     `pct_ratio`, `odds_ratio`, `mean_diff`, `mean_ratio`, `contrib`). Set with [set_color_breaks()].}
#'   \item{`tabxplor.color_style_theme`}{the *console* palette theme, `"light"` or `"dark"`; set by
#'     [set_color_palette()] (which auto-detects the editor theme on load). NOT the export theme
#'     (`tabxplor.theme`).}
#'   \item{`tabxplor.theme`}{the *export* theme, `"light"` (default), `"dark"` or `"auto"` (follow
#'     the reader). `"auto"` needs a stylesheet, so only `tab_kable(engine = "html")`, [tab_md()] and
#'     [tab_css()] honour it; static backends resolve it to `"light"`.}
#' }
#'
#' @section Statistics and confidence intervals:
#' \describe{
#'   \item{`tabxplor.anova`}{which one-way ANOVA F is shown for mean columns: `"welch"` (default,
#'     robust) or `"classic"` (pooled variance). Both are always stored in the `test` attribute.}
#'   \item{`tabxplor.test_lines`}{how many crosstab test rows the exporters ([tab_md()], [tab_kable()],
#'     [tab_xl()]) append: `"pvalue"` (default, the single p-value row) or `"stat"` (adds a
#'     test-statistic row above it). N is never added -- it is already shown by `add_n`. The console
#'     summary block always shows the full N / statistic / p-value table.}
#'   \item{`tabxplor.legend_style`}{the colour-legend style in exports ([tab_md()], [tab_kable()],
#'     [tab_xl()], [tab_plot()]): `"prose"` (default, full sentences) or `"terse"` (the compact
#'     one-line form the console uses). The console itself is always terse.}
#'   \item{`tabxplor.kish_neff`}{`FALSE` (default): use Kish's effective sample size
#'     `(sum w)^2 / sum w^2` for weighted numeric (mean) confidence intervals / significance.}
#'   \item{`tabxplor.conf_level`}{confidence level for the intervals and significance tests, default
#'     `0.95`. The per-call `conf_level =` argument of [tab()], [tab_num()], [tab_ci()] and [tab_reg()]
#'     overrides it.}
#' }
#'
#' @section HTML / `tab_kable()` export:
#' \describe{
#'   \item{`tabxplor.tab_kable_engine`}{`"html"` (default, dependency-free) or `"kableExtra"` (the
#'     legacy engine; needs the suggested `kableExtra` package).}
#'   \item{`tabxplor.kable_css`}{`TRUE` (default): inline the stylesheet with each `tab_kable()` /
#'     [tab_md()] table (self-contained). Set `FALSE` in a many-table document that emits
#'     [tab_css()] once at the top.}
#'   \item{`tabxplor.always_add_css_in_tab_kable`}{`TRUE` (default): always include `tab.css` in the
#'     kableExtra engine's output (a legacy-engine knob).}
#'   \item{`tabxplor.kable_html_font`}{the CSS font stack for the kableExtra engine.}
#'   \item{`tabxplor.kable_popover`}{`FALSE` (default): use click popovers instead of hover tooltips.}
#'   \item{`tabxplor.tab_kable_num_font`, `tabxplor.tab_kable_num_font_stars`}{the HTML-engine number
#'     font CSS stacks, without / with significance stars (the second is monospace so stars align).}
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
#'     on a background worker pool (needs the suggested `mirai` package). `TRUE` = auto workers, an
#'     integer = that many daemons. Per-call `parallel =`. Release the pool with [tab_parallel_stop()].}
#'   \item{`tabxplor.parallel_min`}{`2L` (default): the smallest `row_var` count worth dispatching
#'     (fewer runs serially, since the setup would outweigh the gain).}
#' }
#'
#' @name tabxplor-options
#' @aliases tabxplor.options
NULL
