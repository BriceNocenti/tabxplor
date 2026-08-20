# PURPOSE: THE global-option subsystem -- the declared table, the loader that seeds from it, the
#   readers, and the `?tabxplor-options` page GENERATED from it.
# ROLE: an option is declared ONCE, as one TAB_OPTIONS row: its default, its section, its prose, its
#   per-call argument twin, its synonyms, and whether .onLoad() seeds it. Adding an option is one row.
# KEY CONSTRAINTS:
#   - `default` is the ONLY statement of a default. The doc page renders it, so the prose must not
#     restate it -- and `arg` renders the "Per-call" sentence, so the prose must not restate that
#     either.
#   - ⚠ THE FILE NAME IS LOAD-BEARING. It must sort before `tab.R` in C collation ('-' < '.'), which
#     is why it is `tab-options.R` and not `tabxplor-options.R`: tab.R's derived globalVariables()
#     tail calls new_ctx() and new_inference(), and new_inference()'s `conf_level` formal default
#     reaches conf_level_default() -> tx_option() AT SOURCE TIME. Everything else here is read at
#     runtime only, and every computed `default` is a CLOSURE for the same reason -- a palette or a
#     front-end probe must run at .onLoad(), never while the namespace is still being sourced.
#   - TAB_ARGS (R/tab-args.R) points at these keys through its `option` column; the edge is checked
#     at load in R/zzz-fact-keys.R.
# See: CLAUDE.md § tabxplor architecture (the declarative architecture).

# Read a tabxplor option that accepts synonym names; the FIRST set name wins, then `default`.
#   Pass the seeded/canonical name LAST -- it is always present, so an explicit alias must win first.
#' @keywords internal
tx_getOption <- function(names, default = NULL) {
  for (nm in names) {
    v <- getOption(nm)
    if (!is.null(v)) return(v)
  }
  default
}

#' @keywords internal
#' @noRd
tx_option_name <- function(key) paste0("tabxplor.", key)

#' @keywords internal
#' @noRd
tx_option_names <- function(key) {
  r <- TAB_OPTIONS[[key]]
  tx_option_name(c(if (is.null(r)) character(0) else r$alias, key))
}

#' @keywords internal
#' @noRd
tx_option_default <- function(key) {
  v <- TAB_OPTIONS[[key]]$default
  if (is.function(v)) v() else v
}

#' @keywords internal
#' @noRd
tx_option <- function(key) tx_getOption(tx_option_names(key), tx_option_default(key))

# --- the declared table --------------------------------------------------------------------------
# One row per option:
#   default  the value, or a CLOSURE when it must be computed at load (a palette, a probe).
#   section  which block of ?tabxplor-options it appears in (TAB_OPTION_SECTIONS declares the order).
#   arg      the per-call argument that overrides it, or NA. Renders the "Per-call `x =`." sentence.
#   alias    accepted synonym names (a renamed option's old spelling), read FIRST by tx_getOption().
#   seed     "always" | "if_unset" (an .Rprofile choice survives load) | "elsewhere" (another
#            function seeds it) | "no" (read only if the user sets it -- a retired option).
#   doc      the prose. NOT the default and NOT the per-call argument: both are generated.
#' @keywords internal
#' @noRd
tx_opt <- function(default, section, doc, arg = NA_character_, alias = character(0),
                   seed = "always")
  list(default = default, section = section, doc = doc, arg = arg, alias = alias, seed = seed)

#' @keywords internal
#' @noRd
TAB_OPTION_SECTIONS <- c(
  display  = "Display and printing",
  colours  = "Colours and theme",
  stats    = "Statistics and confidence intervals",
  html     = "HTML / `tab_html()` export",
  excel    = "Excel / `tab_xl()` export",
  plot     = "Plot, paths and language",
  parallel = "Parallel build"
)

#' @keywords internal
#' @noRd
TAB_OPTIONS <- list(

  # --- display and printing ----------------------------------------------------------------------
  print = tx_opt(
    "console", "display",
    c("how a table auto-prints. `\"html\"` renders the [tab_html()]",
      "table (in the Viewer pane in RStudio/Positron, and as a real html table in rmarkdown/Quarto",
      "documents) --- recommended when you work in an IDE with a Viewer. `\"kable\"` is an accepted",
      "synonym of `\"html\"` (the pre-2.0.0 name).")),

  # `signif_levels` / `signif_labels` were a second and third name for one LADDER, with nothing
  #   tying their lengths together. Read only if a user set them (seed = "no"), and then they win.
  stars = tx_opt(
    FALSE, "display", arg = "stars",
    doc = c("whether cells show significance stars, and at which cut-offs. `FALSE` (no stars),",
            "`TRUE` (the default ladder `c(\"*\" = 0.10, \"**\" = 0.05, \"***\" = 0.01)`), or a named",
            "numeric giving your own -- names are the glyphs, values the p-value cut-offs, e.g.",
            "`options(tabxplor.stars = c(\"*\" = 0.05, \"**\" = 0.01))`. Off for [tab()], on for",
            "[tab_reg()]. The LADDER is a",
            "render-time reading of each cell's stored p-value, so it is this option alone --- change",
            "it and every table already built shows the new glyphs.")),

  ratio_print = tx_opt(
    "inverse", "display",
    doc = c("prints a multiplicative value below its reference as the inverse --- an odds ratio of",
            "0.37 as `1/2.67`, a mean ratio of 0.42 as `/2.4` --- so \"2.7 times less\" reads as",
            "strongly as \"2.7 times more\", and the same in a bracket. `\"raw\"` prints the plain",
            "number (`0.37`), the convention of most journals.")),

  n = tx_opt(
    "range", "display", arg = "n",
    doc = c("how many people a table says are behind its numbers. `\"range\"` puts the unweighted",
            "base beside the Total cell of a crosstab (`100% (9 838)`) and in the `n` column of a",
            "regression table, printed as `min-max` when the blocks rest on different populations",
            "--- several column variables losing different `NA`s, or several models. `\"min\"`",
            "prints the smallest base only, `\"no\"` shows no count at all. It replaces the `add_n`",
            "argument, deprecated in 2.0.0.")),

  color_whole_cell = tx_opt(
    FALSE, "display",
    doc = c("EXPERT. A cell that prints SEVERAL fields reads as one number with an aside ---",
            "`1/1.63*** (31%)` --- so the cell's rendering grades the number and the aside is set",
            "slightly back from the table's own text, following the theme. That covers the colour",
            "and, under `theme = \"print\"`, the typography (bold, italic, underline) alike. Set to",
            "`TRUE` to extend the primary's own rendering over the whole cell instead (the",
            "pre-2.0.0 look). There is nothing to choose beyond that: which grey an aside takes",
            "belongs to the theme's palette, not to a per-cell option --- see `set_color_palette()`.",
            "Console and html; Markdown and Excel render a cell as a whole either way.")),

  var_names = tx_opt(
    "both", "display", arg = "var_names",
    doc = paste("which variable names the exporters annotate: `\"both\"`, `\"rows\"`, `\"cols\"`,",
                "`\"none\"`.")),

  var_labels = tx_opt(
    FALSE, "display", seed = "if_unset",
    doc = c("in *exports* (markdown / html / Excel / plot), show a variable's *label* (the",
            "`haven`/`labelled` `label` attribute, if it has one) instead of its name. Display only",
            "-- the table structure keeps canonical names, so name-based `select()` and references",
            "still work; the console always shows names.")),

  cleannames = tx_opt(
    FALSE, "display", arg = "cleannames",
    doc = c("clean up variable/level names in output. Also strips a `\"1-\"`-style prefix from",
            "`labelled` value labels turned into factor levels.")),

  total_names = tx_opt(
    c(row = "Total", col = "Total", tab = "Ensemble", other = "Others"), "display",
    c("the four synthetic labels a table carries: `row` and `col` name the total row and the total",
      "column, `tab` the total \\emph{table} (the one made when there are `tab_vars`), and `other`",
      "the level `other_if_less_than` lumps small levels into. A partial vector is allowed --",
      "`options(tabxplor.total_names = c(tab = \"Ensemble\", other = \"Autres\"))` leaves the first",
      "two alone. It replaces the `total_names` / `totaltab_name` / `other_level` arguments,",
      "deprecated in 2.0.0.")),

  # --- colours and theme -------------------------------------------------------------------------
  color_breaks = tx_opt(
    function() default_color_scales(), "colours", arg = "color_breaks",
    doc = c("the colour-break scales (a named list of `pct_diff`, `pct_ratio`, `odds_ratio`,",
            "`mean_diff`, `mean_ratio`, `contrib`, `zscore`). Set with [set_color_breaks()].")),

  color_style_theme = tx_opt(
    NULL, "colours", alias = "console_theme", seed = "elsewhere",
    doc = c("the *console* palette theme, `\"light\"` or `\"dark\"`; set by [set_color_palette()]",
            "(which auto-detects the editor theme on load). NOT the export theme",
            "(`tabxplor.theme` / `tabxplor.export_theme`).")),

  console_bold = tx_opt(
    function() console_bold_default(), "colours", seed = "if_unset",
    doc = c("whether to embolden the reference / total (and coloured) cells in the *console*, `TRUE`",
            "or `FALSE`. Auto-detected at load: `TRUE` in Positron and VS Code (which render ANSI",
            "bold at a fixed glyph width), `FALSE` in RStudio and unknown consoles (there bold is",
            "drawn wider and would break column alignment). Override it for your own front-end /",
            "font.")),

  theme = tx_opt(
    "light", "colours", arg = "theme", alias = "export_theme",
    doc = c("the *export* theme: `\"light\"`, `\"dark\"`, `\"auto\"` (follow the reader), or a",
            "black-and-white **publication** palette -- `\"print_ready\"` picks one per table (marks",
            "for a cross-table, the emphasis ladder for a regression), or name it yourself:",
            "`\"print_marks\"`, `\"print_emphasis\"`, `\"print_minimalistic\"` (`\"bw\"` is a synonym",
            "of the last). See [tab_css()] for what each says.",
            "`\"auto\"` needs a stylesheet, so only [tab_html()], [tab_md()] and [tab_css()] honour",
            "it; static backends resolve it to `\"light\"`. A publication palette reaches every",
            "backend, Excel included.")),

  print_rules = tx_opt(
    TRUE, "colours", arg = "print_rules",
    doc = c("every stylesheet [tab_css()] emits also carries a black-and-white publication palette",
            "inside an `@media print` block, so a table rendered in colour **prints** (or saves to",
            "PDF) publication-ready with no further action. Set `FALSE` if your printer is a colour",
            "one and the colours are the point, or name a palette (`\"print_emphasis\"`) to print in",
            "that one instead of the default `\"print_minimalistic\"`. `\"print_marks\"` and",
            "`\"print_ready\"` cannot be used here: their marks are cell text, and a print rule can",
            "restyle a page but not add characters to it.")),

  # --- statistics and confidence intervals -------------------------------------------------------
  anova = tx_opt(
    "welch", "stats", arg = "anova",
    doc = c("which one-way ANOVA F is shown for mean columns: `\"welch\"` (robust) or `\"classic\"`",
            "(pooled variance). Both are always stored in the `test` attribute.")),

  design_effect = tx_opt(
    FALSE, "stats", arg = "design_effect",
    doc = c("a weighted [tab()] estimates the population but bases every interval and test on the",
            "raw number of respondents, so they carry no design effect --- and the table's footer",
            "says so. Set `TRUE` and the same intervals \\strong{account for the unequal weighting,",
            "exactly}: a weight column IS a survey design (the flat one, `ids = ~1`), whose variance",
            "has a closed form in the per-cell `sum(w^2)` the aggregate already computes, so the base",
            "becomes `n_eff = p(1-p) / Var_design(p)` in \\strong{every weighted descriptive",
            "confidence interval} --- factor proportions \\emph{and} means (cell, difference, ratio",
            "and the `color = \"odds_ratio\"` significance) --- and the whole-table tests",
            "(`test = TRUE`) become \\code{survey::svychisq} / a \\code{svyglm} Wald F on that flat",
            "design. It reproduces `survey` to the last digit, Kish's `(sum w)^2 / sum(w^2)` being",
            "that same formula with each cell's own `sum(w^2)` discarded. Being exact rather than a",
            "bound, it can make an interval \\emph{narrower} as well as wider. It is blind to",
            "\\strong{clustering} and to \\strong{calibration}, which the weights do not record ---",
            "and those are not symmetric: missing the calibration and the strata costs a few percent,",
            "in the safe direction, while missing the clusters of a face-to-face household survey can",
            "leave an interval several times too short (see the Weights section of",
            "\\code{vignette(\"tabxplor\")}). It needs the microdata weights, so [tab_counts()] on",
            "pre-aggregated counts cannot apply it (such a table states the raw basis in its footer",
            "rather than claiming a correction it does not have). \\strong{Scope: [tab()] and its",
            "leaves only.} [tab_reg()] never reads it --- its crude `empirical =` companions are",
            "always on the weighted basis, beside a model column (\\code{survey::svyglm}) that always",
            "was. For the full design effect --- strata, clusters, `fpc`, calibration --- pass a",
            "\\code{survey::svydesign} as `data`; the option is then not consulted at all.")),

  conf_level = tx_opt(
    0.95, "stats", arg = "conf_level",
    doc = c("confidence level for the intervals and significance tests. Since 2.0.0 each column",
            "records the level it was built at, so the colour thresholds follow the argument and this",
            "option is the fallback for a column that never recorded one (a hand-built [fmt()], or a",
            "table from an older session).")),

  legend_style = tx_opt(
    "prose", "stats",
    c("the colour-legend style in exports ([tab_md()], [tab_html()], [tab_xl()], [tab_plot()]):",
      "`\"prose\"` (full sentences) or `\"terse\"` (the compact one-line form the console uses). The",
      "console itself is always terse.")),

  test_lines = tx_opt(
    "summary", "stats",
    c("how many crosstab test rows the exporters ([tab_md()], [tab_html()], [tab_xl()]) append:",
      "`\"summary\"` (p-value + effect size), `\"all\"` (+ the raw statistic), `\"stat\"` (p-value +",
      "statistic), or `\"pvalue\"` (the single p-value row). The p-value row name states the test used",
      "(\"pvalue (Chi2, Welch F; survey-design)\") and the effect-size row its measure (\"Cramer's V,",
      "eta2\"). N is never added -- it is already shown by the `n` column. The console block always",
      "shows N + p-value + effect size.")),

  spark = tx_opt(
    TRUE, "stats",
    c("in a [tab_reg()] table, a continuous predictor's `n` cell --- empty, since a continuous",
      "predictor has no level to count --- shows a small curve of the OBSERVED shape of its effect",
      "(ten bins of the outcome against the predictor, on the model's scale, with no model in it)",
      "--- the eye-half of the `Linearity` footer row. With `tab_vars`, one curve per group.",
      "In HTML the glyphs become an inline SVG filling its cell; a plot never draws them (no",
      "graphics-device font has them). `FALSE` removes it, and so does `n = \"no\"` (there is then",
      "no cell to draw it in).",
      "",
      "\u26a0 In the CONSOLE the curve is drawn with the block characters `\u2581`...`\u2588`, which are",
      "East-Asian *ambiguous* width: a terminal set to treat those as wide, or one whose font lacks",
      "them and falls back to another face, draws them wider than a digit and the sparkline's row",
      "then looks shifted. The table is correctly aligned character by character --- nothing can",
      "measure the font from R --- so the fix is a console font that carries `U+2581`...`U+2588`",
      "(Cascadia Mono, DejaVu Sans Mono, JetBrains Mono), or `FALSE` to drop the curve.")),

  # --- HTML --------------------------------------------------------------------------------------
  tab_kable_css = tx_opt(
    TRUE, "html", arg = "css", alias = "kable_css",
    doc = c("inline the stylesheet with each [tab_html()] / [tab_md()] table (self-contained). Set",
            "`FALSE` in a many-table document that emits [tab_css()] once at the top.")),

  tab_kable_tooltips = tx_opt(
    TRUE, "html", arg = "tooltips",
    doc = c("show the per-cell hover tooltips (counts, confidence intervals, differences...) in html",
            "tables. Set `FALSE` once per document when every table auto-prints and tooltips are",
            "unwanted.")),

  kable_popover = tx_opt(
    FALSE, "html", arg = "popover",
    doc = "use click popovers instead of hover tooltips."),

  tab_kable_num_font = tx_opt(
    function() tx_num_font_html_stars, "html",
    c("the HTML/markdown number-font CSS stack. Monospace by default so figures stay",
      "column-aligned (set a proportional stack to revert).")),

  output_kable = tx_opt(
    FALSE, "html",
    c("make [tab()] render its result with [tab_html()] before returning it --- a convenience for",
      "`.Rmd`/`.qmd` documents. Since 2.0.0 it only *renders*: it no longer changes the shape of the",
      "built object (that is `output_list`).")),

  # --- Excel -------------------------------------------------------------------------------------
  xl_font_text = tx_opt(
    "DejaVu Sans Condensed", "excel", arg = "font_text",
    doc = "text (labels/headers) font."),

  xl_font_num = tx_opt(
    "DejaVu Sans", "excel", arg = "font_num",
    doc = c("number font without stars. xlsx records ONE name (no fallback list), so set a font",
            "installed where the workbook is opened.")),

  xl_font_num_stars = tx_opt(
    "Cascadia Mono", "excel", arg = "font_num_stars",
    doc = "number font with stars (monospace, so stars align)."),

  xl_or_numeric = tx_opt(
    FALSE, "excel", arg = "or_numeric",
    doc = "keep odds ratios as numbers rather than `1/x` text."),

  # --- plot, paths and language ------------------------------------------------------------------
  plot_num_font = tx_opt(
    "Cascadia Mono", "plot",
    "the [tab_plot()] number font, applied only when the table shows stars (`\"\"` keeps the ggpubr default)."),

  export_dir = tx_opt(
    NULL, "plot",
    "default directory for exported files (`NULL` = the working / typed path)."),


  lang = tx_opt(
    "auto", "plot", arg = "lang",
    doc = "the colour-legend language: `\"auto\"` (follows the R/OS locale), `\"en\"` or `\"fr\"`."),

  # --- parallel ----------------------------------------------------------------------------------
  # Deliberately no `parallel =` argument: a machine-level knob, not a per-table statistical
  #   choice, and one switch lets tab_pmap() enforce the nesting rule for every producer at once.
  parallel = tx_opt(
    FALSE, "parallel",
    doc = c("build the independent units of one call on parallel CPU cores (needs the `mirai`",
            "package): the per-`row_var` tables of a [tab()], the models / `tab_vars` groups /",
            "outcomes of a [tab_reg()]. `TRUE` = auto select number of cores, integer = that many",
            "cores; the result is byte-identical to the serial one. It pays off for MANY, evenly",
            "sized units against a small/medium data frame, and is a loss for few units or",
            "multi-million-row data --- which is why it stays opt-in. A model comparison",
            "(`stats = \"compare_*\"`) is always serial and says so when asked: it is a test",
            "BETWEEN the fits, so they are built together. For one call only, wrap it in",
            "`withr::with_options(list(tabxplor.parallel = TRUE), ...)`. The pool persists for the",
            "session; release it with [tab_parallel_stop()].")),

  parallel_min = tx_opt(
    2L, "parallel",
    c("the smallest UNIT count worth dispatching -- `row_var`s for [tab()], models for [tab_reg()]",
      "(fewer runs serially, since the setup would outweigh the gain)."))
)

stopifnot(
  all(vapply(TAB_OPTIONS, function(r) r$section %in% names(TAB_OPTION_SECTIONS), logical(1))),
  all(vapply(TAB_OPTIONS, function(r) r$seed %in% c("always", "if_unset", "elsewhere", "no"),
             logical(1)))
)

# tx_stars_ladder() -- glyph -> p-value cut-off, read at RENDER time from the stored per-cell
#   p-value. That is why it is an option and not a stored column attribute: changing it re-reads
#   every table that already exists.
#' @keywords internal
#' @noRd
tx_stars_ladder <- function() {
  v <- getOption("tabxplor.stars")
  if (is.numeric(v) && length(v) && !is.null(names(v))) return(sort(v, decreasing = TRUE))
  lev <- getOption("tabxplor.signif_levels") %||% TX_STARS_DEFAULT
  lab <- getOption("tabxplor.signif_labels") %||% names(TX_STARS_DEFAULT)
  n   <- min(length(lev), length(lab))
  sort(stats::setNames(lev[seq_len(n)], lab[seq_len(n)]), decreasing = TRUE)
}

#' @keywords internal
#' @noRd
TX_STARS_DEFAULT <- c("*" = 0.10, "**" = 0.05, "***" = 0.01)

# --- the loader ----------------------------------------------------------------------------------
#' @keywords internal
#' @noRd
tx_seed_options <- function() {
  for (key in names(TAB_OPTIONS)) {
    r  <- TAB_OPTIONS[[key]]
    if (r$seed %in% c("elsewhere", "no")) next
    nm <- tx_option_name(key)
    # "if_unset": a user's .Rprofile choice must survive the load (an auto-detected default is a
    # guess about their front-end, and they know better).
    if (identical(r$seed, "if_unset") && !is.null(getOption(nm))) next
    options(stats::setNames(list(tx_option_default(key)), nm))
  }
  invisible()
}

# --- the generated help page ---------------------------------------------------------------------
# The `#' @eval` generator behind ?tabxplor-options. The DEFAULT and the per-call ARGUMENT are
#   rendered from the table, so neither can drift from `.onLoad()` or from a signature.
#' @keywords internal
#' @noRd
tx_option_default_rd <- function(key) {
  r <- TAB_OPTIONS[[key]]
  if (is.function(r$default) || identical(r$seed, "elsewhere")) return("")
  paste0("`", paste(deparse(r$default, width.cutoff = 500L), collapse = ""), "` (default): ")
}

#' @keywords internal
#' @noRd
tab_options_rd <- function() {
  item <- function(key) {
    r     <- TAB_OPTIONS[[key]]
    label <- paste0("`", tx_option_name(key), "`")
    if (length(r$alias))
      label <- paste0(label, " (alias ",
                      paste0("`", tx_option_name(r$alias), "`", collapse = ", "), ")")
    body <- paste(r$doc, collapse = " ")
    if (!is.na(r$arg)) body <- paste0(body, " Per-call `", r$arg, " =`.")
    paste0("  \\item{", label, "}{", tx_option_default_rd(key), body, "}")
  }
  unlist(lapply(names(TAB_OPTION_SECTIONS), function(sec) {
    keys <- names(TAB_OPTIONS)[vapply(TAB_OPTIONS, function(r) identical(r$section, sec),
                                      logical(1))]
    c(paste0("@section ", TAB_OPTION_SECTIONS[[sec]], ":"),
      "\\describe{", vapply(keys, item, character(1)), "}", "")
  }), use.names = FALSE)
}

#' tabxplor global options
#'
#' `tabxplor` reads its display, colour, statistics and export defaults from `options()`, all
#' prefixed `tabxplor.`. Set any of them for a session with [options()], e.g.
#' `options(tabxplor.stars = TRUE)`, or once at the top of a script or `.Rmd`. The defaults are
#' established when the package loads (`.onLoad()`); most also have a per-call argument on the
#' relevant function, which always wins over the option.
#'
#' @eval tab_options_rd()
#'
#' @name tabxplor-options
#' @aliases tabxplor.options
NULL
