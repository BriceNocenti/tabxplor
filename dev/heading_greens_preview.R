# PURPOSE: choose the heading green, as a LADDER over the six markdown levels, by eye.
# ROLE: dev tool, .Rbuildignore'd.  Rscript dev/heading_greens_preview.R -> dev/heading_greens_preview.html
#   Twelve proposals, each rendered as a full specimen -- h1 to h6 with real prose between them --
#   on the page's own dark chrome, with a light/dark toggle.
#
# WHY A LADDER AND NOT A COLOUR: on a dark ground prominence IS perceptual lightness, so six levels
#   want six values of L, not one green used six times. The rest of the palette fixes the room it
#   has to fit in (measured, not guessed):
#
#     body text   #CDCBBC   L 0.846  C 0.014  h 102   <- what a heading must out-rank
#     code string #a9dc76   L 0.836  C 0.142  h 131   <- the green already in every code block
#     .pertinent  #05ae30   L 0.653  C 0.204  h 145   <- the annotation green
#     .reflexivite#13c097   L 0.720  C 0.140  h 170   <- the annotation teal
#     page        #21252b   L 0.263
#
# ⚠ THE DIAGNOSIS THAT SHAPED THESE TWELVE: the current #92be62 is L 0.749, C 0.130, h 130.1 -- the
#   SAME hue as the code-string green, at LOWER lightness than the body text. A heading darker than
#   the prose it heads cannot lead the page, and one sitting on the string hue reads as a dimmed
#   code token. Every proposal below therefore starts at L >= 0.86, and most move off hue 130.
#
# ⚠ THE GAMUT IS NOT FLAT. Green chroma peaks near L 0.86 at hue 140 (max 0.277 in sRGB) and
#   collapses above L 0.92 (0.10 at hue 140). A "very light, very saturated" green does not exist:
#   ask for one and oklch_hex() reduces the chroma until it fits, as CSS Color 4 does.

source("dev/heading_ladders.R")   # the converter and every ladder: the one definition

# === SECTION: the page ============================================================================

chroma_cap_js <- paste(readLines("dev/chroma_cap.js", warn = FALSE), collapse = "\n")

PAGE_BG <- "#21252b"; PAGE_FG <- "#CDCBBC"; PANEL <- "#282c34"; BORDER <- "#3e4451"
LINK    <- "#61afef";  BOLD   <- "#e6ae02"; CODE  <- "#fc9867"

prose <- c(
  "A paragraph of ordinary prose sits under the heading, because a heading colour is only ever seen next to the text it leads. This one carries a <strong>bold run</strong>, a <a href=\"#\">link</a> and a piece of <code>inline_code()</code>, which are the three colours a green has to live beside.",
  "The second level opens a section within the first. Enough text follows it to put some distance between one heading and the next, since two headings seen back to back flatter a ladder that does not survive being spread over a page.",
  "A third level is where most documents stop. The question at this rung is no longer whether the colour is pleasant but whether it still reads as a heading at all.",
  "The fourth level is common in a long course document. It should still lead its paragraph without competing with the levels above it.",
  "The fifth is rare, and mostly wants to stay out of the way.",
  "The sixth is rarer still. If the last two rungs are hard to tell apart, that costs less than a muddy top.")

# The second style. A heading colour that survives an empty page is not the test; a course page runs
# annotation spans, inline code and chunks past it, and the green has to hold its rank among them.
annotation_css <- local({
  f <- "dev/annotation_classes.css"
  if (file.exists(f)) paste(readLines(f, warn = FALSE), collapse = "\n") else ""
})

chunk <- paste0(
  '<pre class="cb"><code><span class="co"># the table this section is about</span>\n',
  '<span class="va">gss</span> <span class="op">&lt;-</span> <span class="fu">gss_cat_data_formatting</span><span class="op">()</span>\n',
  '<span class="fu">tab</span><span class="op">(</span><span class="va">gss</span><span class="op">,</span> ',
  '<span class="va">race</span><span class="op">,</span> <span class="va">party3</span><span class="op">,</span> ',
  '<span class="at">pct</span> <span class="op">=</span> <span class="st">&quot;row&quot;</span><span class="op">,</span> ',
  '<span class="at">color</span> <span class="op">=</span> <span class="st">&quot;difference&quot;</span><span class="op">)</span>',
  '</code></pre>')

# A note long enough to wrap: the only way to see whether each line gets its own rounded box (that is
# what `box-decoration-break: clone` decides) and whether consecutive bands clear each other.
long_note <- paste0('<p><span class="comment">A note long enough to run over three full lines, ',
  'which is the only way to see what a wrapped annotation actually looks like: whether each line ',
  'gets its own rounded box or one long shape with two rounded ends, whether the bands of ',
  'consecutive lines clear each other, and whether the text still reads with the descenders ',
  'sitting where they do.</span></p>')

dense <- c(
  "The <span class=\"problematique\">question this section answers</span> is whether the gap survives adjustment. We read it from <code>tab()</code> first, then from <span class=\"concept\">the model</span>, and the two are put side by side rather than one after the other.",
  "In <span class=\"terrain\">the 2014 survey</span> the crude gap is twenty points. <span class=\"resultat\">It falls to eight once income enters</span>, which is the whole argument of the chapter; <code>tab_reg(empirical = TRUE)</code> is what puts both numbers in one table.",
  "<span class=\"reference\">Cibois (2014)</span> makes the same point with a different measure. <span class=\"reflexivite\">Whether the two agree depends on the base</span>, and the footer of every table says which one was used.",
  "A fourth level usually carries the detail: the <code>shape</code> argument, the <code>ref</code> it is measured from, and <span class=\"enjeu\">what the reader is expected to conclude</span> from the pair.",
  "<span class=\"pertinent\">This part is worth keeping</span>, though <span class=\"preciser\">the wording needs work</span> and <span class=\"non\">the second claim is wrong</span>. <span class=\"comment\">my own note in the margin</span>",
  "The last level closes the section, with <span class=\"structure\">a transition</span> into the next one and one final call to <code>tab_export()</code>.")

swatch_row <- function(l) {
  chips <- paste0(
    '<span class="chip" style="background:', l$hex, '" title="', l$hex, '"></span>', collapse = "")
  paste0('<div class="swrow"><a href="#', l$id, '"><b>', l$name, '</b></a><span class="chips">',
         chips, '</span></div>')
}

specimen <- function(l) {
  css <- paste0(vapply(1:6, function(k)
    sprintf("#%s h%d{color:%s;}", l$id, k, l$hex[k]), character(1)), collapse = "")
  nm    <- c("One", "Two", "Three", "Four", "Five", "Six")
  heads <- paste0(vapply(1:6, function(k) sprintf(
    "<h%d>%s. A heading at level %d</h%d>\n<p>%s</p>",
    k, nm[k], k, k, prose[k]), character(1)), collapse = "\n")
  # the dense twin: the same six headings, but read through a page that is actually busy
  heads_dense <- paste0(
    paste(vapply(1:6, function(k) sprintf(
      "<h%d>%s. A heading at level %d</h%d>\n<p>%s</p>%s",
      k, nm[k], k, k, dense[k], if (k %in% c(2, 4)) chunk else ""), character(1)), collapse = "\n"),
    "\n", long_note)
  numbers <- paste0(vapply(1:6, function(k) sprintf(
    "<tr><td>h%d</td><td><code>oklch(%.2f %.3f %.0f)</code></td><td><code>%s</code></td><td>%.1f:1</td></tr>",
    k, l$L[k], l$C[k], l$H[k], l$hex[k], contrast(l$hex[k], PAGE_BG)), character(1)), collapse = "")
  paste0('<style>', css, '</style>\n<section id="', l$id, '">\n',
         '<div class="lab"><b>', l$name, '</b> &mdash; ', l$note, '</div>\n',
         '<div class="sleek">', heads, '</div><div class="dense">', heads_dense, '</div>',
         '\n<details open><summary>numbers</summary><table class="nums"><tr><th>level</th>',
         '<th>oklch</th><th>hex</th><th>contrast on the page</th></tr>', numbers,
         '</table></details>\n</section>')
}

html <- paste0('<!doctype html>
<html lang="en" data-mode="dark" data-style="sleek">
<head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>tabxplor - heading greens</title>
<style>
  :root{--bg:', PAGE_BG, ';--fg:', PAGE_FG, ';--panel:', PANEL, ';--border:', BORDER, ';}
  html[data-mode="light"]{--bg:#ffffff;--fg:#2b2b2b;--panel:#f6f7f9;--border:#d7dae0;}
  body{background:var(--bg);color:var(--fg);margin:0;
       font-family:"DejaVu Sans","Source Sans Pro",system-ui,sans-serif;line-height:1.5;}
  .layout{display:flex;align-items:flex-start;}
  .wrap{flex:1 1 auto;min-width:0;max-width:46rem;margin:0 auto;padding:1rem 1.5rem 40vh;}
  .panel{flex:0 0 13rem;position:sticky;top:0;height:100vh;overflow-y:auto;
         background:var(--panel);border-right:1px solid var(--border);padding:.9rem 1rem;z-index:10;}
  .grp{font-size:.68rem;text-transform:uppercase;letter-spacing:.04em;opacity:.5;
       margin:1rem 0 .45rem;border-bottom:1px solid var(--border);padding-bottom:.15rem;}
  .grp:first-of-type{margin-top:.2rem;}
  .swrow{margin:0 0 .7rem;font-size:.78rem;}
  .swrow a{display:block;color:inherit;margin-bottom:.15rem;}
  .chips{display:flex;gap:2px;}
  .chip{flex:1 1 0;height:1.05rem;border-radius:3px;display:block;}
  @media (max-width:900px){
    .layout{display:block;}
    .panel{position:static;height:auto;border-right:0;border-bottom:1px solid var(--border);}
  }
  section{border-top:1px solid var(--border);padding-top:1.2rem;margin-top:2.5rem;}
  .lab{font-size:.8rem;opacity:.75;margin-bottom:1rem;}
  h1,h2,h3,h4,h5,h6{font-family:"DejaVu Sans",system-ui,sans-serif;font-weight:bold;line-height:1.25;}
  h1{font-size:1.9rem;} h2{font-size:1.55rem;} h3{font-size:1.3rem;}
  h4{font-size:1.12rem;} h5{font-size:1rem;} h6{font-size:.92rem;}
  p{margin:.4rem 0 1.1rem;}
  a{color:', LINK, ';} strong{color:', BOLD, ';}
  code{color:', CODE, ';background:', CODE, '33;padding:2px 4px;border-radius:4px;
       font-family:"Cascadia Code",Consolas,monospace;font-size:.87em;}
  html[data-style="sleek"] .dense{display:none;}
  html[data-style="dense"] .sleek{display:none;}
  /* a busy page needs the headings set off from what precedes them */
  html[data-style="dense"] h1,html[data-style="dense"] h2,html[data-style="dense"] h3,
  html[data-style="dense"] h4,html[data-style="dense"] h5,html[data-style="dense"] h6{
    margin-top:2.1em;}
  .cb{background:#1f1f1f;color:#CDCBBC;padding:.7rem .9rem;border-radius:6px;overflow-x:auto;
      margin:.2rem 0 1.1rem;font-family:"Cascadia Code",Consolas,monospace;
      font-size:.8rem;line-height:1.45;}
  .cb code{background:none;padding:0;color:inherit;font-size:inherit;}
  .cb .co{color:#8b8a8d;font-style:italic;} .cb .fu{color:#61afef;} .cb .st{color:#a9dc76;}
  .cb .va{color:#CDCBBC;} .cb .op{color:#939293;} .cb .dv{color:#ab9df2;}
  .cb .at{color:#fc9867;font-style:italic;} .cb .kw{color:#ff6188;}
  .nums{font-size:.75rem;border-collapse:collapse;margin-top:.5rem;}
  .nums td,.nums th{padding:1px 10px 1px 0;text-align:left;}
  details{opacity:.7;font-size:.8rem;}
  .btn{font:inherit;font-size:.78rem;width:100%;margin-bottom:.7rem;cursor:pointer;
       background:var(--bg);color:var(--fg);border:1px solid var(--border);border-radius:5px;
       padding:.28rem .4rem;text-align:left;}
</style>
<style>', annotation_css, '</style>
</head>
<body>
<div class="layout">
<nav class="panel">
  <label style="font-size:.8rem;display:block;margin-bottom:.4rem;">page
    <select id="mode"><option value="dark" selected>dark</option><option value="light">light</option></select>
  </label>
  <button id="style" class="btn">page style: <b>sleek</b></button>
  <label style="font-size:.8rem;display:block;margin-bottom:.5rem;">chroma cap
    <input id="cap" type="range" min="0.04" max="0.30" step="0.01" value="0.30" style="width:100%;">
    <span id="capout" style="opacity:.6;">off</span>
  </label>
  <div style="font-size:.75rem;opacity:.65;margin-bottom:.8rem;">
    twelve ladders over the six heading levels &mdash; jump to one, or scroll</div>
  ', paste(unlist(lapply(unique(vapply(LADDERS, function(l) l$group, character(1))), function(g) {
       ls <- Filter(function(l) identical(l$group, g), LADDERS)
       paste0('<div class="grp">', g, '</div>', paste(vapply(ls, swatch_row, character(1)), collapse = ""))
     })), collapse = ""), '
</nav>
<div class="wrap">
<p style="opacity:.75;font-size:.85rem;">The current <code>#92be62</code> is
<code>oklch(0.749 0.130 130)</code>: the code-string hue, <em>darker</em> than the body text it leads.
Each proposal below starts brighter than the prose and most move off that hue. The numbers under each
specimen give the exact <code>oklch()</code>, the hex, and the contrast against the page.</p>
',
paste(vapply(LADDERS, specimen, character(1)), collapse = "\n"), '
</div>
</div>
<script>', chroma_cap_js, '</script>
<script>
  const root = document.documentElement;
  const cap = document.querySelector("#cap"), capout = document.querySelector("#capout");
  cap.addEventListener("input", () => {
    const v = +cap.value;
    const off = v >= 0.30;                       // nothing in these palettes exceeds 0.30
    capout.textContent = off ? "off" : "every colour capped at C " + v.toFixed(2);
    window.txChromaCap.set(off ? null : v);
  });
  document.querySelector("#mode").addEventListener("change", e =>
    root.setAttribute("data-mode", e.target.value));
  document.querySelector("#style").addEventListener("click", e => {
    const next = root.getAttribute("data-style") === "sleek" ? "dense" : "sleek";
    root.setAttribute("data-style", next);
    e.currentTarget.innerHTML = "page style: <b>" + next + "</b>";
  });
</script>
</body></html>')

writeLines(html, "dev/heading_greens_preview.html")
message("written: dev/heading_greens_preview.html  (", length(LADDERS), " ladders)")
