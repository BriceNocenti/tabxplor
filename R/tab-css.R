# PURPOSE: The ONE CSS generator for every tabxplor stylesheet (Phase 13d).
# ROLE: Turns (palette, color_type, theme) into CSS rules consumed by BOTH media that can carry a
#   stylesheet: tab_kable(engine = "html") and tab_md()/tab_css(). Replaces the old per-table
#   md_css_rules()/md_css_block()/md_break_class()/md_slot_class_map() (tab_md.R) and the static,
#   hard-coded html_style_block() (tab-render-html.R).
# KEY CONSTRAINTS:
#   - The CSS is TABLE-INDEPENDENT: a pure function of (palette, color_type, theme). That is the whole
#     point of naming classes by palette SLOT rather than by break value -- it is what lets a document
#     emit the stylesheet ONCE (tab_css()) and reuse it for every table, and what makes class collisions
#     impossible (`.p3` means the same shade in every table, whatever its color_breaks).
#   - Cells and CSS read the SAME slot vocabulary (tx_slot_class), so they cannot disagree.
#   - tx_chrome_hex() is the single source of the chrome colours; tab_export_prep()'s `theme_cols`
#     reads it too, so the inline path and the CSS path cannot drift.
#   - Every LOOK is a role class here, geometry included (Phase 14e) -- the html engine emits no
#     inline style, so a user's own CSS can override any of it. Phase 14i adds `.tx-lbl` (a cell
#     rowspan'd over the block it names) and `.tx-vname` (a row-variable name, written vertically).
#   - NO border SHORTHAND (Phase 14j): a shorthand resets border-*-color to `currentColor` = the
#     cell's palette hex, and every border rule here out-specifies the ONE border-color rule. Always
#     border-*-style + border-*-width. Locked by test-render-html.R.
#   - NO column width (Phase 14j): the browser's auto table layout sizes each column to its content.
#     `.tx-rv`/`.tx-tot`/`.tx-num` are emitted UNSTYLED, as hooks for a user's own fixed-width CSS
#     (?tab_css). The table is sized by its data: `.tx-foot` keeps the footnote out of that sum.
# See: CLAUDE.md Phase 13d + 14e + 14i + 14j, dev/tabxplor_phase10_exporters.md.

# === SECTION: theme + slot vocabulary ==============================================================

# "auto" is a RENDER intent (follow the reader's colour scheme), never a palette. Every palette lookup
# must funnel through here: get_color_style(theme = "auto") would build the key "text_auto", find no
# palette, and error on a length-0 vector.
#' @keywords internal
tx_palette_theme <- function(theme) {
  if (is.null(theme) || is.na(theme[1])) return("light")
  if (identical(theme[1], "auto")) "light" else theme[1]
}

# The chrome colours (everything that is NOT a colour-measure slot), per theme. ONE literal table:
# tab_export_prep() builds `theme_cols` from it (the inline/kableExtra/plot/xl path) and tx_css_rules()
# emits it as CSS (the html path), so the two renderings cannot drift.
#   text  : the table's own font colour (also what a reference cell gets -- it inherits, no class)
#   grey  : an uncoloured cell in a column that HAS a colour measure
#   grey2 : an uncoloured cell in a column with no colour measure
#' @keywords internal
# Phase 14e: `hover` is kableExtra's lightable yellow (its `tbody tr:hover` -- more visible and more
# familiar than the grey wash we had). DARK: pure #FFFFFF on #111111 is a harsh, glare-y contrast for
# body text; #CECDC3 on #222222 is the (softer, warmer) pairing the maintainer asked for. The border
# stays the text colour, so it softens with it.
#' @keywords internal
tx_chrome_hex <- function(theme = "light") {
  if (identical(tx_palette_theme(theme), "dark")) {
    list(text = "#CECDC3", grey = "#707070", grey2 = "#EEEEEE",
         bg = "#222222", border = "#CECDC3", hover = "rgba(255,242,204,.10)")
  } else {
    list(text = "#000000", grey = "#9f9f9f", grey2 = "#111111",
         bg = "#ffffff", border = "#000000", hover = "#FFFCE5")
  }
}

# The chrome of a page tabxplor ITSELF builds -- there are exactly two: the standalone page
# print.tabxplor_kable() opens in the Viewer, and tab_html_string(standalone = TRUE). Reuses
# tx_chrome_hex(), so the pane can never drift from the table sitting in it.
#
# WHY it needs no !important, though it fights two rules it does not own: htmltools' save_html()
# builds the page as <head> + `<style>body{background-color:white;}</style>` + the html dependencies
# (bootstrap's own `body{}`) + </head><body> + OUR string. Ours is therefore LAST in document order at
# equal specificity (0,0,1), which is all it takes.
# WARNING: this must NEVER reach tab_css(). A knitted/Quarto page is the HOST's, not ours -- painting
# its html,body would repaint the whole document around the table (Phase 13d's rule).
#' @keywords internal
tx_page_style <- function(theme = "light") {
  decl <- function(t) {
    ch <- tx_chrome_hex(t)
    paste0("html,body{background:", ch$bg, ";color:", ch$text, ";}")
  }
  # "auto" is only ever reached by a page we WRITE (a file the reader opens elsewhere), so the reader's
  # OS is the only signal available -- no hooks: a standalone page has no framework toggle to follow.
  # An interactive print resolves "auto" R-side before calling here (only R can see the editor).
  if (identical(theme, "auto")) {
    paste0(decl("light"), "\n@media (prefers-color-scheme: dark){", decl("dark"), "}")
  } else {
    decl(tx_palette_theme(theme))
  }
}

# slot integer -> class name. The engine's slot domain (Phase 13a) is 0 = uncoloured, 1-4
# over-represented, 5-8 under-represented, per channel. Names are 2 chars and uniform-width, which
# keeps raw markdown aligned in a monospace font:
#   text : .p1 .p2 .p3 .p4 (over, "plus")   .m1 .m2 .m3 .m4 (under, "minus")
#   bg   : .o1 .o2 .o3 .o4 (over)           .u1 .u2 .u3 .u4 (under)
# DESIGN: keyed to the SLOT, not to the break value -- so the name is palette-shade identity, not a
# threshold. The threshold is the colour legend's job (it already renders it per table). This is what
# makes the stylesheet table-independent; do not reintroduce break-derived names.
# Vectorised over `slot`; slot 0 (and NA) -> "".
#' @keywords internal
tx_slot_class <- function(channel = c("text", "bg"), slot) {
  channel <- match.arg(channel)
  over    <- if (channel == "text") "p" else "o"
  under   <- if (channel == "text") "m" else "u"
  slot    <- as.integer(slot)
  out     <- rep("", length(slot))
  ok      <- !is.na(slot) & slot >= 1L & slot <= 8L
  if (!any(ok)) return(out)
  s <- slot[ok]
  out[ok] <- ifelse(s <= 4L, paste0(over, s), paste0(under, s - 4L))
  out
}

# === SECTION: rules ================================================================================

# The page-toggle hooks. `@media (prefers-color-scheme: dark)` only reports the OS: every framework
# that ships a dark TOGGLE translates the preference into a class/attribute via JS, which a media query
# cannot see. Emitting both directions lets an explicit toggle win over the OS either way.
# WARNING: keep these in ONE place -- they are the only part of the design that can rot upstream.
#
# KNOWN GAP (verified in a browser 2026-07-16, deliberately NOT fixed): the light list has no Tailwind
# entry, because Tailwind's class strategy expresses light as the ABSENCE of `html.dark` -- there is no
# `html.light` to match. So on a Tailwind class-strategy page with a dark OS, the page is light and our
# table follows the OS into dark: a dark island. Every other framework here sets an explicit light
# class/attribute, so it only affects Tailwind. Fixing it needs a signal that "a class strategy is in
# force" (a `color-scheme` probe, or an opt-out of the @media layer) -- see decisions 38.
#
# NOT HERE, deliberately (Phase 14k): the VS Code / Positron webview hooks `body.vscode-dark` and
# `[data-vscode-theme-kind]`. The Positron Viewer is a CROSS-ORIGIN webview IFRAME
# (vscode-webview://.../index-external.html), so those live on the OUTER workbench body and no
# selector of ours can ever reach them -- a rule that cannot fire is worse than no rule. The Viewer is
# handled where it CAN be: print.tabxplor_kable() resolves "auto" in R (tx_detect_theme()) and sets
# [data-theme] on the page it builds, so the hooks below do the work.
tx_dark_hooks  <- c("body.quarto-dark",  "[data-bs-theme=dark]",  "[data-theme=dark]", "html.dark")
tx_light_hooks <- c("body.quarto-light", "[data-bs-theme=light]", "[data-theme=light]")

# The theme-independent rule table: one row per (selector, property), carrying BOTH hex. Built from the
# same slot vocabulary the cells use. `chrome = FALSE` gives colour rules only (the tab_md contract:
# bare class selectors the user maps in their own editor CSS).
#' @keywords internal
tx_css_rules <- function(color_type = NULL, chrome = TRUE) {
  if (is.null(color_type)) color_type <- getOption("tabxplor.color_style_type")

  sel <- character(0); prop <- character(0); lt <- character(0); dk <- character(0)
  add <- function(s, p, l, d) {
    sel  <<- c(sel, s); prop <<- c(prop, p); lt <<- c(lt, l); dk <<- c(dk, d)
  }

  if (isTRUE(chrome)) {
    cl <- tx_chrome_hex("light")
    cd <- tx_chrome_hex("dark")
    add(".tabxplor-tab", "color",      cl$text,  cd$text)
    add(".tabxplor-tab", "background", cl$bg,    cd$bg)
    # THE one border-colour rule -- every border in this stylesheet takes its colour from here.
    # WARNING: that only holds because no rule below uses a border SHORTHAND. `border-right:1px solid`
    # would reset border-right-color to `currentColor` = the CELL's palette hex (a +20% cell drew a
    # blue border), and `.tabxplor-tab .tx-br` (0,2,0) out-specifies this (0,1,1), so the shorthand
    # would win however this rule is written. Phase 14e wrongly recorded the bug as fixed by moving the
    # geometry off inline styles: that removed the inline half and left the shorthand half. Longhands
    # (border-*-style / border-*-width) are what make this rule load-bearing -- keep it that way; the
    # invariant is locked by test-render-html.R ("no border shorthand in the stylesheet").
    add(".tabxplor-tab th,.tabxplor-tab td", "border-color", cl$border, cd$border)
    add(".tabxplor-tab tbody tr:hover", "background", cl$hover, cd$hover)
    add(".g1", "color", cl$grey,  cd$grey)
    add(".g2", "color", cl$grey2, cd$grey2)
  }

  for (ch in c("text", "bg")) {
    type <- if (ch == "text") color_type else "bg"
    pl   <- get_color_style("color_code", type = type, theme = "light")
    pd   <- get_color_style("color_code", type = type, theme = "dark")
    prp  <- if (ch == "text") "color" else "background-color"
    for (s in 1:8) {
      add(paste0(".", tx_slot_class(ch, s)), prp,
          toupper(unname(pl[s])), toupper(unname(pd[s])))
    }
  }
  list(sel = sel, prop = prop, light = lt, dark = dk)
}

# Prefix every part of a (possibly comma-separated) selector with every hook -> one comma-joined
# selector list. `.tabxplor-tab th,.tabxplor-tab td` + 2 hooks -> 4 parts.
tx_hook_sel <- function(sel, hooks) {
  vapply(sel, function(s) {
    parts <- trimws(strsplit(s, ",", fixed = TRUE)[[1]])
    paste0(as.vector(t(outer(hooks, parts, function(h, p) paste0(h, " ", p)))), collapse = ",")
  }, character(1), USE.NAMES = FALSE)
}

tx_css_layer <- function(rules, which = c("light", "dark"), hooks = NULL, indent = "") {
  which <- match.arg(which)
  hex   <- rules[[which]]
  keep  <- !is.na(hex) & nzchar(hex)
  if (!any(keep)) return(character(0))
  s    <- rules$sel[keep]
  decl <- paste0(rules$prop[keep], ":", hex, ";")
  # Fold the declarations of one selector into a single block (`.tabxplor-tab{color:X;background:Y;}`),
  # keeping first-appearance order. Purely cosmetic on the light layer; it matters under "auto", where
  # every rule is emitted four times with long hook selectors.
  grp <- factor(s, levels = unique(s))
  sel <- levels(grp)
  if (!is.null(hooks)) sel <- tx_hook_sel(sel, hooks)
  paste0(indent, sel, "{", vapply(split(decl, grp), paste0, character(1), collapse = ""), "}")
}

# Render the rule table for one theme. "light"/"dark" are a single static layer; "auto" is four cascade
# layers whose ORDER and specificity are the contract:
#   1 light base                      (0,1,0)  -- the default
#   2 @media prefers-color-scheme     (0,1,0)  -- same specificity, wins on SOURCE ORDER
#   3 explicit page toggle -> light   (0,2,x)  -- beats the OS
#   4 explicit page toggle -> dark    (0,2,x)  -- last, so a pathological tie resolves dark
# WARNING: do not reorder. Layer 3 before 4 and both after 2 is what makes an explicit host toggle
# (Quarto's body.quarto-dark) override the reader's OS preference in BOTH directions.
#' @keywords internal
tx_css_render <- function(rules, theme = "light", chrome = TRUE) {
  theme  <- match.arg(theme[1], c("light", "dark", "auto"))
  # Theme-INDEPENDENT, so it belongs here rather than in the rule table (which is emitted once per
  # cascade layer under "auto"). Phase 14c: a text-coloured cell is bold in every other medium
  # (tab_export_prep's `bold = !is.na(text_hex) | ref_alltot` drives kableExtra AND the html engine's
  # inline weight), so the stylesheet must carry it for the one medium that has no other way to say
  # it: tab_md(), whose cells are bare `[42%]{.p2}` spans. Harmlessly redundant on the html engine.
  # The background classes stay unbolded, exactly like the cells: a fill alone does not bold.
  bold_slots <- paste0(paste0(".", tx_slot_class("text", 1:8), collapse = ","), "{font-weight:bold;}")
  static <- c(bold_slots, if (isTRUE(chrome)) c(
    # Phase 14e: the html engine's GEOMETRY lives here, not inline on every cell. An inline style
    # cannot be overridden by a user's CSS, so "a good default you can restyle" (what kableExtra gives)
    # was impossible while the engine wrote its own borders and widths. The engine now emits ROLE
    # classes (tx-r/tx-l align, tx-num numbers, tx-br/tx-bl borders, tx-tot total col, tx-rv row-var
    # col, tx-b bold, tx-bt/tx-bb/tx-bb2 row rules, tx-span the col_var header, tx-pill a background)
    # and every look is decided here. Overriding is then ordinary CSS -- no !important needed, because
    # nothing of ours is inline any more.
    # DejaVu Sans Condensed for text and DejaVu Sans for numbers mirrors tab_xl()'s font_text/font_num,
    # so a table looks the same exported to Excel or to html. Both degrade through the stack.
    # `.tabxplor-tab` is the <table> itself (the html engine) OR a wrapping <div> (a markdown table
    # inside its pandoc fenced div, Phase 14f) -- `border-collapse` only means something on a table, so
    # name both. Every other rule below is a descendant selector and reaches the table either way.
    # WARNING (Phase 14j): NO border SHORTHAND anywhere below -- always border-*-style/-width. A
    # shorthand resets border-*-color to `currentColor`, i.e. the cell's own palette hex, and every
    # rule here out-specifies the one border-color rule above. Locked by test-render-html.R.
    paste0(".tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;",
           "border-top-width:0;border-bottom-width:0;",
           "margin:0;font-family:\"DejaVu Sans Condensed\",\"DejaVu Sans\",Arial,helvetica,sans-serif;}"),
    ".tabxplor-tab caption{text-align:center;font-weight:bold;font-size:120%;}",
    ".tabxplor-tab tfoot{font-size:80%;text-align:left;}",
    # readable-compact: a real vertical rhythm (line-height 0.85 crammed the rows) + ~1mm of side
    # padding, so text no longer touches the column borders.
    ".tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}",
    paste0(".tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;",
           "vertical-align:bottom;line-height:1;border-top-width:0;",
           "border-bottom-style:solid;border-bottom-width:1px;}"),
    paste0(".tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;",
           "border-bottom-style:solid;border-bottom-width:1px;}"),
    ".tabxplor-tab .tx-r{text-align:right;}",
    ".tabxplor-tab .tx-l{text-align:left;}",
    # thead th's `text-align:center` must beat the column's own alignment: same specificity (0,2,0)
    # vs (0,2,0), so SOURCE ORDER decides -- this pair must stay after .tx-r/.tx-l.
    ".tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}",
    paste0(".tabxplor-tab .tx-num{white-space:nowrap;",
           "font-family:\"DejaVu Sans\",Arial,helvetica,sans-serif;}"),
    ".tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}",
    ".tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}",
    # Phase 14j: `.tx-tot` (total column) and `.tx-rv` (the row-variable levels column) are still
    # EMITTED, deliberately with no rule of their own. They used to carry min-width:5.5em / 10em, which
    # is what made "levels and Total columns very wide for nothing": a browser's auto table layout
    # already sizes each column to its content, so a floor could only ever be too big. They remain as
    # hooks -- `.tx-rv{min-width:10em}` in the user's own stylesheet is the fixed-width escape hatch
    # (see ?tab_css), which is exactly what emitting roles instead of inline styles buys.
    # Phase 14i: a LABEL cell (`rowspan`ned over its block: a merged table's row-variable name, or a
    # kept tab_var's level) centres itself on the block it names rather than floating at its top.
    ".tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}",
    # ... and a row-variable NAME is written vertically, so a long one costs no column width and wraps
    # into several vertical lines instead of stretching the table sideways.
    # WARNING: NOT `writing-mode:sideways-lr`, which reads the same way but is still flagged
    # experimental with patchy support (Chrome shipped it late; MDN marks it so). `vertical-rl` +
    # rotate(180deg) is the universally-supported equivalent -- bottom-to-top, matching the 90-degree
    # rotation tab_xl writes into Excel.
    paste0(".tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);",
           "white-space:normal;padding:4px 2px;}"),
    ".tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}",
    ".tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}",
    # WARNING: `tx-bb` (1px) and `tx-bb2` (2px) have IDENTICAL specificity (0,3,1), so a row carrying
    # both -- the last row of a row_var block -- is decided by SOURCE ORDER here: tx-bb2 comes second
    # and wins, which is the intended thicker rule. Do not reorder this pair.
    ".tabxplor-tab tr.tx-bb>*{border-bottom-style:solid;border-bottom-width:1px;}",
    ".tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}",
    # Phase 14j: the footnote (subtext + colour legend) must not SIZE the table. Its cell spans every
    # column, and its prose is ~330 characters on one line, so its max-content dwarfed the data's --
    # and a table's used width is max(min-content, min(max-content, available)), so the table took the
    # full pane and auto layout spread the slack across every column ("a tvhours cell half numbers half
    # blank"). This was the real cause of the compactness complaint; the min-widths above were a
    # sideshow. `width:0` is a definite size, so the cell contributes 0 to max-content (a percentage
    # min-width resolves to 0 while sizing, against an indefinite containing block); at layout time the
    # cell's width IS definite, so min-width:100% resolves and the text fills it. If a browser ever
    # disagreed, the fallback is the old stretched table -- nothing breaks.
    ".tabxplor-tab .tx-foot{width:0;min-width:100%;}",
    # a background HUGS its text (rounded, inline) rather than flooding the cell: a full-cell fill
    # reads as a blocky grid AND swallows the row hover (a child's background always paints over its
    # row's, whatever the specificity).
    ".tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}",
    # Phase 14b: a cell tooltip is one line of "field: value ; field: value" prose, but bootstrap caps
    # .tooltip-inner at max-width:200px, so it wrapped to four lines and was unreadable.
    # WARNING: this selector is NOT scopable. Bootstrap moves the tooltip element to <body>
    # (data-container="body", which is what stops a table's overflow from clipping it), so it is never
    # a descendant of .tabxplor-tab and no ancestor selector can reach it. It therefore applies to any
    # other bootstrap tooltip on the host page. Accepted: a one-line tooltip is what every bootstrap
    # tooltip wants, the rule is unprefixed so a host stylesheet loaded later still wins, and it ships
    # only with chrome = TRUE (never from tab_md()'s colour-only stylesheet).
    ".tooltip-inner{max-width:none;white-space:nowrap;}",
    # Phase 14j: the same for a POPOVER (tab_kable(popover = TRUE)), which the html engine has emitted
    # since 10e with no styling at all -- bootstrap caps .popover at max-width:276px, so our one-line
    # prose wrapped. `.popover-body` is bootstrap 4/5, `.popover-content` bootstrap 3 (rmarkdown's
    # dependency, which is what kableExtra's print loads); naming both keeps this version-agnostic.
    # DESIGN: geometry ONLY. inst/tab.css (the kableExtra path) also paints .popover white-on-black,
    # and that is deliberately NOT ported: this selector is as unscopable as .tooltip-inner above, so a
    # colour override would repaint the HOST page's popovers. "One line, not 276px" is what every
    # bootstrap popover wants; a black background is our taste, imposed on someone else's page. Left
    # unstyled, the popover simply inherits the host's own theme.
    ".popover{max-width:none;}",
    ".popover-body,.popover-content{padding:6px;white-space:nowrap;}"
  ) else character(0))

  body <- if (identical(theme, "auto")) {
    dark_media <- tx_css_layer(rules, "dark", indent = "  ")
    c(tx_css_layer(rules, "light"),
      if (length(dark_media)) c("@media (prefers-color-scheme: dark) {", dark_media, "}"),
      tx_css_layer(rules, "light", hooks = tx_light_hooks),
      tx_css_layer(rules, "dark",  hooks = tx_dark_hooks))
  } else {
    tx_css_layer(rules, theme)
  }

  paste0(c(static, body), collapse = "\n")
}

# === SECTION: the public generator =================================================================

# WARNING (Phase 14k): in the "Two workflows" section below, the FOUR-backtick fence is load-bearing,
# and so is the fact that it carries no `{r}` info string. roxygen2 (>= 7.1) EVALUATES a ```{r} chunk
# written in roxygen markdown and splices its OUTPUT into the help page -- so the three-backtick chunk
# we want to SHOW the user has to be quoted by a longer fence. It was not: the section used raw-Rd
# \preformatted{} wrapped around a live chunk, so document() ran tab_css() and pasted the entire
# stylesheet into ?tab_css, emitted ~89 "could not resolve link" warnings (one per bracketed token of
# the CSS it printed: \link{1}, \link{data-bs-theme=light}, ...), and -- because \preformatted{} is Rd,
# not markdown -- leaked literal **bold** into the rendered page. Never mix raw Rd with a code fence.

#' Generate the tabxplor stylesheet
#'
#' The CSS that colours tabxplor tables. It is a **constant** -- a pure function of the colour palette,
#' the channel type and the theme -- so it does not take a table: one stylesheet styles every table in a
#' document, whatever their `color_breaks`.
#'
#' Cells carry classes named after the palette **slot** (`.p1`-`.p4` over-represented text, `.m1`-`.m4`
#' under-represented text, `.o1`-`.o4` / `.u1`-`.u4` for the background channel), so `tab_kable()` and
#' `tab_md()` share one vocabulary.
#'
#' @section Two workflows:
#' **Self-contained (the default).** `tab_kable(css = TRUE)` and `tab_md(css = TRUE)` inline the
#' stylesheet with the table, so a single file works anywhere (RStudio/Positron Viewer, jamovi, a
#' standalone `.html`). Nothing to do.
#'
#' **Once per document.** In an `.Rmd`/`.qmd` with many tables, emit it once and let every table reuse
#' it:
#'
#' ````
#' ```{r, results = "asis"}
#' options(tabxplor.kable_css = FALSE)
#' tab_css(theme = "auto")
#' ```
#' ````
#'
#' Every later `tab_kable()` then emits classes only. Two things to know: with `css = FALSE` and **no**
#' `tab_css()` call the tables render uncoloured; and one stylesheet means one `theme` and one
#' `color_type` for the whole document (a per-table `color_type` would need its own `css = TRUE`).
#'
#' @section Restyling a table:
#' Nothing is written inline on a cell, so **any** of the look can be overridden by adding your own
#' rules after the stylesheet -- no `!important` needed. Column widths in particular are left to the
#' browser (it sizes each column to its content); to pin one, style its role:
#' \preformatted{
#' .tabxplor-tab .tx-rv  { min-width: 10em; }   /* the row-variable levels column */
#' .tabxplor-tab .tx-tot { min-width: 5.5em; }  /* total columns                  */
#' .tabxplor-tab .tx-num { min-width: 4em; }    /* every number column            */
#' }
#' The roles a cell can carry: `.tx-l`/`.tx-r` (alignment), `.tx-num` (numbers), `.tx-rv` (the
#' row-variable levels column), `.tx-tot` (total columns), `.tx-bl`/`.tx-br` (side borders),
#' `.tx-b` (bold), `.tx-lbl`/`.tx-vname` (a variable name spanning its block), `.tx-pill` (a
#' background-coloured value), `.tx-span` (the variable-name header row), `.tx-foot` (the footnote).
#' Rows carry `.tx-bt`/`.tx-bb`/`.tx-bb2` (top / bottom / thick-bottom rules).
#'
#' @param theme `"light"`, `"dark"`, or -- opt-in -- `"auto"` to follow the reader's colour scheme
#'   (their operating system, and any dark-mode toggle of the host page: Quarto, Bootstrap 5.3,
#'   Tailwind). Defaults to `getOption("tabxplor.theme")`, i.e. `"light"`: a dark table is always a
#'   deliberate choice. `"auto"` emits every rule four times (a light base, the OS media query, then
#'   both toggle directions), which is also what lets [tab_kable()]'s own Viewer page force the
#'   editor's theme -- see its `theme` argument.
#' @param color_type `"text"` or `"bg"`: which palette family the text channel uses. Defaults to
#'   `getOption("tabxplor.color_style_type")`.
#' @param chrome When `TRUE` (default) also style the table itself (font/background/border colours,
#'   the greys) -- what `tab_kable(engine = "html")` needs. `FALSE` emits the colour classes only, which
#'   is what `tab_md()` wants: bare selectors you can map in your own editor's CSS.
#' @param style_tag Wrap the CSS in a `<style>` tag (default `TRUE`).
#' @param file Optional path to write to instead of returning.
#'
#' @return The CSS, invisibly when `file` is given. Printed as-is by `knitr` with `results = "asis"`.
#' @seealso [tab_kable()], [tab_md()], [set_color_palette()], [set_color_breaks()]
#' @export
#' @examples
#' cat(tab_css(theme = "auto"))
#' cat(tab_css(chrome = FALSE, style_tag = FALSE))  # the markdown flavour
tab_css <- function(theme = NULL, color_type = NULL, chrome = TRUE,
                    style_tag = TRUE, file = NULL) {
  o   <- resolve_export_opts(theme, color_type, allow_auto = TRUE)
  css <- tx_css_render(tx_css_rules(o$color_type, chrome = chrome), o$theme, chrome = chrome)
  if (isTRUE(style_tag)) css <- paste0("<style>\n", css, "\n</style>")
  if (!is.null(file)) {
    writeLines(css, file)
    return(invisible(css))
  }
  css
}
