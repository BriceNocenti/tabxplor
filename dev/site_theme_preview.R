# PURPOSE: check the SHIPPED site theme against real markup, without rebuilding the site.
# ROLE: dev tool, .Rbuildignore'd.  Rscript dev/site_theme_preview.R  ->  dev/site_theme_preview.html
#   Open that file in a browser (\\wsl.localhost\dev\home\dev1\github\tabxplor\dev\) and flip the
#   controls at the top.
#
# ⚠ THE THEME IS DECIDED, AND IT IS NOT SET HERE. The dark chrome, the heading ladder, the prose
#   rules and the code colours come from the `txtheme` package (`template: package: txtheme` in
#   _pkgdown.yml); a colour changes in txtheme/R/aaa-palette.R and reaches this page through the
#   compiled stylesheet below. So this tool's job is no longer "choose the theme by eye" but "check
#   the shipped theme against markup a specimen page cannot produce" -- downlit's own code blocks,
#   pandoc's own document elements, and real tab()/tab_reg() tables.
#
# WHAT IT SHOWS, and the three layers it still lets you vary BY EYE (nothing it writes is a decision):
#   1. the page chrome   -- bootstrap 5.3 dark colours, exposed as the CSS custom properties they
#      compile to. The page opens on txtheme's own values.
#   2. the code colours  -- all 35 styles pkgdown bundles, plus `txtheme-dark`, read back from the
#      shipped inst/highlight/txtheme-dark.scss so this page cannot disagree with the site.
#   3. the TABLE colours -- tabxplor's own palette, which nothing above touches. It is set in R with
#      set_color_palette(); the tables below are rendered through tab_css(theme = "auto"), the same
#      call the vignettes make, so they follow the light/dark toggle exactly as the site does.
#
# WARNING: the page LINKS to docs/deps/.../bootstrap.min.css -- the real compiled site CSS -- so the
#   chrome is the site's own, not an approximation. Build the site at least once, or the page loads
#   unstyled. dev/build_site.R wipes docs/ before it builds, so re-run this script after a rebuild.

stopifnot(requireNamespace("pkgload", quietly = TRUE), requireNamespace("txtheme", quietly = TRUE))
pkgload::load_all(".", quiet = TRUE)

options(tabxplor.lang = "en", tabxplor.tab_kable_css = FALSE, tabxplor.tab_kable_tooltips = TRUE,
        tabxplor.cleannames = TRUE)
Sys.setenv(LANGUAGE = "en")

chroma_cap_js <- paste(readLines("dev/chroma_cap.js", warn = FALSE), collapse = "\n")

out_file  <- "dev/site_theme_preview.html"
style_dir <- system.file("highlight-styles", package = "pkgdown")


# === SECTION: the highlight styles ================================================================

# Each pkgdown style is flat CSS: `pre {…}` plus one `pre code span.XX {…}` per token class. Read
# them as (selector, declarations) pairs so any prefix can be put in front of the selector.
# txtheme ships its own theme in exactly that shape (inst/highlight/txtheme-dark.scss, generated from
# TX_TOKENS), which is why reading it costs no code here and cannot drift from the site.
read_style <- function(path) {
  txt   <- paste(readLines(path, warn = FALSE), collapse = "\n")
  txt   <- gsub("/\\*.*?\\*/", "", txt)                       # the aligned token-name comments
  rules <- regmatches(txt, gregexpr("[^{}]+\\{[^{}]*\\}", txt))[[1]]
  sel   <- trimws(sub("\\{.*$", "", rules))
  decl  <- trimws(gsub("^[^{]*\\{|\\}$", "", rules))
  data.frame(sel = sel, decl = decl, stringsAsFactors = FALSE)
}

bundled <- vapply(list.files(style_dir, "\\.scss$"), function(f) sub("\\.scss$", "", f), character(1))
styles  <- lapply(file.path(style_dir, paste0(bundled, ".scss")), read_style)
names(styles) <- bundled

styles[["txtheme-dark"]] <- read_style(
  system.file("highlight/txtheme-dark.scss", package = "txtheme"))

ports <- "txtheme-dark"
all_styles <- names(styles)
# Which styles suit a dark page: the bundled ones say so in their name, plus txtheme's. Only used to
# group the menus, so a wrong guess costs nothing.
dark_ish  <- c(grep("dark|monokai|dracula|nord|espresso|oblivion|radical|zenburn|ayu-mirage",
                    bundled, value = TRUE), ports)
light_ish <- setdiff(all_styles, dark_ish)


# The pandoc-span annotation classes, from txtheme's generated asset (its :root colours from
# TX_PALETTE, its typography from inst/prose/annotations.scss).
# ⚠ On the real site this stylesheet is OPT-IN (`template: params: txtheme: {annotations: true}`) and
#   tabxplor does not opt in: `.non, .error {text-decoration: underline double}` would decorate every
#   warned or errored example on a reference page, which emits <span class="warning"> of its own.
#   Here it is loaded unconditionally, because seeing the classes is the point.
annotation_css <- local({
  f <- system.file("pkgdown/BS5/assets/txtheme-annotations.css", package = "txtheme")
  if (nzchar(f)) paste(readLines(f, warn = FALSE), collapse = "\n") else ""
})


# === SECTION: the stylesheet ======================================================================

# WARNING: specificity is the whole trick. pkgdown's own rules are `pre code span.co` (0,1,2) for the
# light theme and `[data-bs-theme="dark"] pre code span.co` (0,2,2) for the dark one, and they are
# already in the compiled site CSS this page loads. Prefixing with the attribute the controls set
# puts every rule below one class-count higher, so the selection always wins.
# WARNING 2: the light layer is scoped `:not([data-bs-theme="dark"])`. Most light styles carry a
# `pre {background-color: #f1f3f5}` of their own, and unscoped it out-specifies bootstrap's
# `[data-bs-theme="dark"] pre` -- a white code box on a dark page. On the real site the light theme
# is emitted UNSCOPED but at (0,0,1), so it loses there and this never happens.
css_for <- function(name, prefix) {
  s <- styles[[name]]
  paste0(prefix, " ", s$sel, "{", s$decl, "}", collapse = "\n")
}
highlight_css <- paste(
  c(vapply(all_styles, function(n)
      css_for(n, sprintf('html[data-hl="%s"]:not([data-bs-theme="dark"])', n)), character(1)),
    vapply(all_styles, function(n)
      css_for(n, sprintf('html[data-bs-theme="dark"][data-hld="%s"]', n)), character(1))),
  collapse = "\n")


# The light palette, pinned inside one wrapper so a light-coloured table can be judged on a dark page.
# tab_css() emits its dark layer at `[data-bs-theme=dark] .tabxplor-tab .p1` (0,3,0) and its light one
# unprefixed, so the wrapper has to out-specify the DARK one: `html body` plus the class twice takes
# the prefix to (0,2,2) against the dark layer's (0,1,1), which settles it whatever the source order.
# `print_rules = FALSE`: an @media block nests braces, and the rule splitter below reads one level.
# WARNING: `style_tag = FALSE` -- tab_css() wraps itself in <style> by default, and a nested tag makes
# the parser drop the rule that follows it.
force_light_css <- local({
  txt   <- tab_css(theme = "light", print_rules = FALSE, style_tag = FALSE)
  rules <- regmatches(txt, gregexpr("[^{}]+\\{[^{}]*\\}", txt))[[1]]
  # ONLY the ladders: the text slots (.p1-.p4 over, .m1-.m4 under) and the background ones
  # (.o1-.o4, .u1-.u4). Everything else a light stylesheet says -- black ink, white borders, the
  # greyed cell, the footer -- would be pinned onto a dark page and unreadable, and it is not what
  # is being judged: the question is whether the LIGHT ladder reads on a dark ground.
  rules <- rules[grepl("\\.[pmou][1-4]([^0-9]|$)", sub("\\{.*$", "", rules))]
  out   <- vapply(rules, function(r) {
    sel  <- strsplit(trimws(sub("\\{.*$", "", r)), ",")[[1]]
    decl <- trimws(gsub("^[^{]*\\{|\\}$", "", r))
    paste0(paste0("html body .tx-force-light.tx-force-light ", trimws(sel), collapse = ","),
           "{", decl, "}")
  }, character(1))
  # A FILL comes with its ink. The text slots carry their own colour, but a background slot only
  # states a fill: on a light page the ink under it is the light chrome's black, and leaving the dark
  # ink there would put near-white on a pale blue -- a pairing that exists nowhere, and would read as
  # the light ladder failing when it is only the two halves being mismatched.
  # WARNING: the fill is a `.tx-pill` span INSIDE the cell, and a descendant's own colour beats its
  # ancestor's whatever the specificity -- so an unguarded ink rule here silently kills the text
  # channel on every stacked cell. The guard is the cell carrying NO text slot: there, and only
  # there, the ink under a light fill has to come with it.
  no_text <- paste0(":not(.", tx_slot_class("text", 1:8), ")", collapse = "")
  out <- c(out, paste0(
    "html body .tx-force-light.tx-force-light td", no_text, " .tx-pill,",
    paste0("html body .tx-force-light.tx-force-light td", no_text, ".",
           tx_slot_class("bg", 1:8), collapse = ","),
    "{color:", tx_chrome_hex("light")$text, ";}"))
  paste(out, collapse = "\n")
})


# === SECTION: what the page shows =================================================================

# Real code blocks, taken from a built article so the markup IS downlit's, autolinks included.
code_blocks <- local({
  src <- "docs/articles/tabxplor.html"
  if (!file.exists(src)) return(character(0))
  html <- paste(readLines(src, warn = FALSE), collapse = "\n")
  hits <- regmatches(html, gregexpr('<div class="sourceCode"[^>]*>.*?</pre></div>', html))[[1]]
  hits <- gsub('href="\\.\\./reference/[^"]*"', 'href="#"', hits)
  hits[c(4, 12, 20)][!is.na(hits[c(4, 12, 20)])]
})

# The document elements a vignette or a course page is made of, rendered by PANDOC from
# dev/site_theme_preview_elements.md rather than hand-written -- so the html is exactly what a real
# render produces: pandoc's own sourceCode spans (the same classes the styles above target), its
# table and figure markup, and MathML, which is what pkgdown emits by default and what needs no
# script. Edit the .md, not this file, to change what the section shows.
doc_elements <- local({
  src <- "dev/site_theme_preview_elements.md"
  if (!file.exists(src)) return("")
  out <- tryCatch(
    system2("pandoc", c("--mathml", "--from", "markdown", "--to", "html5", shQuote(src)),
            stdout = TRUE),
    warning = function(w) NULL, error = function(e) NULL)
  if (is.null(out) || !length(out))
    return("<p><em>This section is rendered by pandoc, which was not found.</em></p>")
  paste(out, collapse = "\n")
})

# One block exercising every token class, so no colour is judged on its absence.
token_zoo <- paste0(
  '<div class="sourceCode"><pre class="downlit sourceCode r"><code class="sourceCode R">',
  '<span><span class="co"># every token class downlit and pandoc can emit</span></span>\n',
  '<span><span class="va">x</span> <span class="op">&lt;-</span> <span class="fu">tab</span>',
  '<span class="op">(</span><span class="va">gss</span><span class="op">,</span> ',
  '<span class="va">race</span><span class="op">,</span> pct <span class="op">=</span> ',
  '<span class="st">"row"</span><span class="op">,</span> n <span class="op">=</span> ',
  '<span class="cn">TRUE</span><span class="op">)</span></span>\n',
  '<span><span class="kw">function</span><span class="op">(</span><span class="va">n</span> ',
  '<span class="op">=</span> <span class="dv">42L</span><span class="op">,</span> ',
  '<span class="va">p</span> <span class="op">=</span> <span class="fl">0.95</span>',
  '<span class="op">)</span> <span class="cf">if</span> <span class="op">(</span>',
  '<span class="va">n</span><span class="op">)</span> <span class="bu">sum</span>',
  '<span class="op">(</span><span class="va">n</span><span class="op">)</span></span>\n',
  '<span>tab<span class="op">(</span><span class="va">gss</span><span class="op">,</span> shape ',
  '<span class="op">=</span> <span class="fu">c</span><span class="op">(</span>tvhours ',
  '<span class="op">=</span> <span class="st">&quot;sqrt&quot;</span><span class="op">))</span>',
  '  <span class="co"># argument names: bare text out of downlit, wrapped by the shim</span></span>\n',
  '<span><span class="dt">integer</span><span class="op">(</span><span class="dv">0</span>',
  '<span class="op">)</span><span class="sc">\\n</span> <span class="at">attr</span> ',
  '<span class="wa">warning</span> <span class="er">error</span> <span class="in">note</span></span>',
  '</code></pre></div>')

# Three tables: a plain crosstab, one with the non-significant cells greyed, and a regression.
gss    <- gss_cat_data_formatting()
tables <- list(
  "A cross-table, coloured by deviation from the total row" =
    tab(gss, race, party3, pct = "row", color = "difference"),
  "The same, with every non-significant cell greyed out" =
    tab(gss, relig, c(married, income25k), pct = "row", levels = "first",
        color = "difference", color_signif = "grey_non_signif"),
  "A regression table, beside its observed counterpart" =
    tab_reg(gss, outcome = "married", predictors = c("race", "age"), empirical = TRUE),
  # BOTH CHANNELS, stacked. A positional pair is what asks for the second one: the text measure on
  # the cell, the background measure on a `.tx-pill` span INSIDE it (a fill hugs its text rather than
  # flooding the cell). `color = TRUE` is the text channel alone, which is why it showed no fill.
  "Several column variables, on both colour channels at once" =
    tab(gss, c(race, rincome), c(party3, relig, tvhours), pct = "row",
        color = c("difference", "ratio"), ref = 1)
)
# Rendered once more, wrapped, to be read in the LIGHT palette whatever the page is set to.
forced_light <- paste0(
  '<div class="tx-force-light">',
  as.character(tab_html(tables[[4]])), '\n\n',
  as.character(tab_html(tables[[1]])), '</div>')
tables_html <- paste(vapply(names(tables), function(nm) paste0(
  '<h3>', nm, '</h3>\n', as.character(tab_html(tables[[nm]]))), character(1)), collapse = "\n\n")


# === SECTION: the page ============================================================================

opt_tags <- function(selected) {
  opt <- function(nms) paste0('<option value="', nms, '"',
                              ifelse(nms == selected, " selected", ""), '>', nms, '</option>',
                              collapse = "")
  paste0('<optgroup label="the shipped theme">', opt(ports), '</optgroup>',
         '<optgroup label="dark">', opt(setdiff(dark_ish, ports)), '</optgroup>',
         '<optgroup label="light">', opt(light_ish), '</optgroup>')
}

# THE HEADING LADDER: `warm-95-10` -- the ladder the site actually ships, and txtheme's own
# TX_PALETTE re-derives its six hexes from this spec at load. The other ladders stay in the menu so
# the choice can be re-read against them; picking one here changes nothing but this page.
source("dev/heading_ladders.R")             # every ladder, and txtheme's OKLCH maths
HEADING_DEFAULT <- "warm-95-10"
HEADING_LADDER  <- LADDERS[[which(vapply(LADDERS, function(l) l$name, "") == HEADING_DEFAULT)]]$hex

# The dark chrome. Each row is (label, the CSS custom property bootstrap 5.3 reads, bootstrap's stock
# value, txtheme's). Only the eight a colour picker can hold are here; the `-rgb` twins and the two
# derived rgba() rules the real theme also writes are txtheme's business, not this page's.
chrome <- data.frame(stringsAsFactors = FALSE,
  label = c("page background", "body text", "emphasis", "headings", "panels, code background",
            "borders", "links", "inline code"),
  prop  = c("--bs-body-bg", "--bs-body-color", "--bs-emphasis-color", "--bs-heading-color",
            "--bs-tertiary-bg", "--bs-border-color", "--bs-link-color", "--bs-code-color"),
  # bootstrap 5.3's own dark values -- what the `bootstrap default` preset resets to.
  # `headings` is `inherit` upstream, which a colour input cannot hold, so it starts at the body text.
  stock = c("#212529", "#dee2e6", "#ffffff", "#dee2e6", "#2b3035", "#495057", "#6ea8fe", "#ffffff"),
  # THE DEFAULT the page opens on -- txtheme's shipped values, the preset below.
  start = c("#21252b", "#CDCBBC", "#fcfcfa", HEADING_LADDER[1], "#282c34", "#3e4451",
            "#61afef", "#fc9867"))

chrome_rows <- paste0(
  '<label class="tw"><span>', chrome$label, '</span>',
  '<input type="color" data-prop="', chrome$prop, '" data-stock="', chrome$stock,
  '" data-start="', chrome$start, '" value="', chrome$start, '">',
  '<code class="hex">', chrome$start, '</code></label>', collapse = "\n")

# The argument-name shim, read from the package that ships it to the real site -- so what this page
# does to a downlit block is exactly what a reader's browser does.
at_shim_js <- paste(readLines(system.file("pkgdown/BS5/assets/txtheme-at.js", package = "txtheme"),
                              warn = FALSE), collapse = "\n")

html <- paste0('<!doctype html>
<html lang="en" data-bs-theme="dark" data-hl="arrow-light" data-hld="txtheme-dark">
<head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>tabxplor - site theme preview</title>
<link href="../docs/deps/bootstrap-5.3.8/bootstrap.min.css" rel="stylesheet">
<style>', tab_css(theme = "auto", style_tag = FALSE), '</style>
<style id="highlight-styles">', highlight_css, '</style>
<style id="annotations">', annotation_css, '</style>
<style id="force-light">', force_light_css, '</style>
<style id="tweaks"></style>
<style>
  .panel{position:sticky;top:0;z-index:1000;background:var(--bs-tertiary-bg);
         border-bottom:1px solid var(--bs-border-color);padding:.75rem 1rem;margin-bottom:2rem;}
  .panel .grid{display:flex;flex-wrap:wrap;gap:1rem 1.5rem;align-items:flex-end;}
  .panel label{display:flex;flex-direction:column;font-size:.8rem;gap:.15rem;}
  .panel select,.panel button{font-size:.85rem;padding:.2rem .4rem;}
  label.tw{flex-direction:row;align-items:center;gap:.4rem;}
  label.tw span{min-width:11rem;}
  label.tw input[type=color]{width:2.5rem;height:1.6rem;padding:0;border:0;background:none;}
  .hex{font-size:.75rem;opacity:.7;}
  main h2{margin-top:2rem;}
</style>
</head>
<body class="template-article">
<div class="panel">
  <div class="grid">
    <label>page theme
      <select id="mode"><option value="light">light</option><option value="dark" selected>dark</option></select>
    </label>
    <label>code colours &mdash; light page
      <select id="hl">', opt_tags("arrow-light"), '</select>
    </label>
    <label>code colours &mdash; dark page
      <select id="hld">', opt_tags("txtheme-dark"), '</select>
    </label>
    <label>code background (dark)
      <select id="codebg">
        <option value="#1f1f1f" selected>#1f1f1f VS Code dark</option>
        <option value="">starless &mdash; the page\'s own</option>
        <option value="#272822">#272822 classic Monokai</option>
        <option value="#2e3440">#2e3440 Nord (pkgdown\'s monokai)</option>
        <option value="#21252b">#21252b Atom One Dark</option>
      </select>
    </label>
    <label>heading ladder
      <select id="ladder">', paste(unlist(lapply(
        unique(vapply(LADDERS, function(l) l$group, character(1))), function(g) {
          ls <- Filter(function(l) identical(l$group, g), LADDERS)
          paste0('<optgroup label="', g, '">', paste0(
            '<option value="', vapply(ls, function(l) paste(l$hex, collapse = ","), character(1)),
            '"', ifelse(vapply(ls, function(l) l$name, character(1)) == HEADING_DEFAULT, " selected", ""),
            '>', vapply(ls, function(l) l$name, character(1)), '</option>', collapse = ""), '</optgroup>')
        })), collapse = ""), '</select>
    </label>
    <label>prose overrides
      <select id="prose">
        <option value="on" selected>on &mdash; bold, quotes</option>
        <option value="">off &mdash; bootstrap only</option>
      </select>
    </label>
    <label>dark chrome
      <select id="preset">
        <option value="txtheme" selected>txtheme &mdash; what the site ships</option>
        <option value="monokai-pro">monokai pro &mdash; cyan links, neutral ground</option>
        <option value="">bootstrap default (stock, themed by nothing)</option>
      </select>
    </label>
    <label>chroma cap &mdash; <span id="capout">off</span>
      <input id="cap" type="range" min="0.04" max="0.30" step="0.01" value="0.30">
    </label>
    <button id="reset" class="btn btn-sm btn-outline-secondary">reset</button>
  </div>
  <details class="mt-2"><summary>the dark chrome, colour by colour</summary>
    <div class="grid mt-2" style="align-items:flex-start;">', chrome_rows, '</div>
  </details>
</div>

<div class="container template-article"><div class="row"><main id="main" class="col-md-9">
<h1>Site theme preview</h1>
<p class="lead">Everything on this page is the real thing: the site\'s own compiled stylesheet, the
shipped <code>txtheme</code> code theme, code blocks with downlit\'s own markup, and tables built by
<code>tab()</code> and <code>tab_reg()</code> through <code>tab_css(theme = "auto")</code>. Only the
controls above are new, and nothing they do is a decision &mdash; the theme is set in
<code>txtheme/R/aaa-palette.R</code>.</p>

<h2>Prose, links and inline code</h2>
<p>A paragraph of ordinary text, so the body colour can be judged against the background rather than
in isolation. It carries a <a href="#">link</a>, some <strong>bold</strong>, some <em>italic</em>,
and a piece of <code>inline_code()</code>, which takes its own colour
(<code>--bs-code-color</code>) and not the code block\'s.</p>
<blockquote class="blockquote"><p>A block quote. Under the prose overrides it takes the warm grey and
the italic the editor theme gives it, and its rule the same gold as <strong>bold text</strong>.</p></blockquote>
<ul><li>A list item.</li><li>Another, with <code>tab(pct = "row")</code> in it.</li></ul>

<h2>Code</h2>
<p>The block below exercises every token class, so no colour is judged on its absence:</p>
', token_zoo, '
<p>And these are real blocks from the introduction vignette:</p>
', paste(code_blocks, collapse = "\n"), '

', doc_elements, '

<h2>Tables</h2>
<p>tabxplor\'s colours are its own: nothing in <code>_pkgdown.yml</code> or in <code>txtheme</code>
reaches them. They are set in R with <code>set_color_palette()</code>, and they follow the toggle
above because the page emits <code>tab_css(theme = "auto")</code> &mdash; exactly what the vignettes
do on the site.</p>
', tables_html, '

<h2>The light palette, on whatever page you are looking at</h2>
<p>The same two tables, with <strong>only the over/under ladders</strong> pinned to the
<strong>light</strong> palette by a wrapper class &mdash; the text slots and the background ones. The
ink, the greyed cells, the borders and the footer stay with the page, because pinning those too is
what made this unreadable. Nothing in the ladders follows the toggle: that is the point.</p>
', forced_light, '

<div style="height:35vh"></div>
</main></div></div>
<script>', chroma_cap_js, '</script>
<script>', at_shim_js, '</script>
<script>
const $ = s => document.querySelector(s), root = document.documentElement;
const KEY = "tabxplor-theme-preview-3";   // bumped: an older stored state would mask the defaults
// `txtheme` is Starless Monokai Atom read as a page: the ground and the panel come from the editor,
// the base text is the warm #CDCBBC of the settings.json override (NOT One Dark #abb2bf, which is
// cooler and duller), emphasis is the #fcfcfa that warm text was demoted from, the headings take the
// warm-95-10 ladder\'s top rung, links keep the Atom blue, and inline code takes #fc9867 -- the
// theme\'s own markup.inline.raw colour, which the prose layer then tints at 20% behind it.
const presets = {
  txtheme:  {"--bs-body-bg":"#21252b","--bs-body-color":"#CDCBBC","--bs-emphasis-color":"#fcfcfa",
             "--bs-heading-color":"', HEADING_LADDER[1], '","--bs-tertiary-bg":"#282c34",
             "--bs-border-color":"#3e4451",
             "--bs-link-color":"#61afef","--bs-code-color":"#fc9867"},
  "monokai-pro":
            {"--bs-body-bg":"#1e2024","--bs-body-color":"#CDCBBC","--bs-emphasis-color":"#fcfcfa",
             "--bs-heading-color":"#fcfcfa","--bs-tertiary-bg":"#26282c","--bs-border-color":"#3a3d42",
             "--bs-link-color":"#78dce8","--bs-code-color":"#fc9867"},
};

function state() {
  const s = {mode: $("#mode").value, hl: $("#hl").value, hld: $("#hld").value,
             codebg: $("#codebg").value, prose: $("#prose").value,
             ladder: $("#ladder").value, chrome: {}};
  document.querySelectorAll("input[data-prop]").forEach(i => s.chrome[i.dataset.prop] = i.value);
  return s;
}
function apply() {
  const s = state();
  // The ladder OWNS the headings, so the chrome picker follows its top rung rather than being a
  // second, independent choice: it is only the fallback for anything the six rules do not reach.
  const ladder = $("#ladder").value.split(",");
  document.querySelectorAll("input[data-prop]").forEach(i => {
    if (i.dataset.prop === "--bs-heading-color") i.value = ladder[0];
  });
  s.chrome["--bs-heading-color"] = ladder[0];
  root.setAttribute("data-bs-theme", s.mode);
  root.setAttribute("data-hl", s.hl);
  root.setAttribute("data-hld", s.hld);
  let decls = "";
  document.querySelectorAll("input[data-prop]").forEach(i => {
    if (i.value && i.value.toLowerCase() !== i.dataset.stock.toLowerCase())
      decls += i.dataset.prop + ":" + i.value + ";";
  });
  let css = decls ? \'html[data-bs-theme="dark"]{\' + decls + "}" : "";
  const heads = ladder.map((c, i) =>
    \'html[data-bs-theme="dark"] h\' + (i + 1) + " { color: " + c + "; }").join("\\n") + "\\n";
  // The editor theme colours markdown itself, not only code: gold bold, a gold quote rule and a
  // warm grey italic quote. Bootstrap has no variable for any of the three -- they are plain CSS.
  const prose = $("#prose").value ?
    \'html[data-bs-theme="dark"] strong, html[data-bs-theme="dark"] b { color: #e6ae02; }\\n\' +
    \'html[data-bs-theme="dark"] blockquote { border-left-color: #e6ae02; color: #B7B5AC;\' + " font-style: italic; }\\n" +
    \'html[data-bs-theme="dark"] :not(pre) > code { background-color: \' + s.chrome["--bs-code-color"] + "33;" +
    " padding: 3px 5px; border-radius: 5px; }\\n" : "";
  css += heads + prose;
  if (s.codebg) css += \'html[data-bs-theme="dark"] pre{background-color:\' + s.codebg + " !important;}";
  window.txChromaCap.setLive($("#tweaks"), css);
  document.querySelectorAll("input[data-prop]").forEach(i =>
    i.nextElementSibling.textContent = i.value);
  localStorage.setItem(KEY, JSON.stringify(s));
}
function restore() {
  let s; try { s = JSON.parse(localStorage.getItem(KEY)); } catch(e) {}
  if (!s) return;
  if (s.mode) $("#mode").value = s.mode;
  if (s.hl) $("#hl").value = s.hl;
  if (s.hld) $("#hld").value = s.hld;
  if (s.codebg !== undefined) $("#codebg").value = s.codebg;
  if (s.prose !== undefined) $("#prose").value = s.prose;
  if (s.ladder) $("#ladder").value = s.ladder;
  document.querySelectorAll("input[data-prop]").forEach(i => {
    if (s.chrome && s.chrome[i.dataset.prop]) i.value = s.chrome[i.dataset.prop];
  });
}
$("#preset").addEventListener("change", e => {
  const p = presets[e.target.value];
  document.querySelectorAll("input[data-prop]").forEach(i =>
    i.value = p ? (p[i.dataset.prop] || i.dataset.stock) : i.dataset.stock);
  apply();
});
document.addEventListener("input", apply);
document.addEventListener("change", apply);
$("#reset").addEventListener("click", () => {
  localStorage.removeItem(KEY);
  document.querySelectorAll("input[data-prop]").forEach(i => i.value = i.dataset.start);
  $("#preset").value = "txtheme"; $("#codebg").value = "#1f1f1f"; $("#prose").value = "on";
  apply();
});
$("#cap").addEventListener("input", () => {
  const v = +$("#cap").value, off = v >= 0.30;   // nothing in these palettes exceeds 0.30
  $("#capout").textContent = off ? "off" : "C " + v.toFixed(2);
  window.txChromaCap.set(off ? null : v);
});
restore(); apply();
</script>
</body></html>')

writeLines(html, out_file)
message("written: ", out_file, "  (", round(file.size(out_file) / 1024), " KB, ",
        length(all_styles), " highlight styles)")
