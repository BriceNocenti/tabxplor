# PURPOSE: The ONE CSS generator for every tabxplor stylesheet (Phase 13d).
# ROLE: Turns (palette, theme) into CSS rules consumed by BOTH media that can carry a
#   stylesheet: tab_kable(engine = "html") and tab_md()/tab_css(). Replaces the old per-table
#   md_css_rules()/md_css_block()/md_break_class()/md_slot_class_map() (tab_md.R) and the static,
#   hard-coded html_style_block() (tab-render-html.R).
# KEY CONSTRAINTS:
#   - The CSS is TABLE-INDEPENDENT: a pure function of (palette, theme). That is the whole
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
#   - The three `.tabxplor-tab table ...` rules (Phase 14m-iii) are MD-ONLY by selector (a `table`
#     descendant exists only in the pandoc <div>+<table>, never in the html engine where .tabxplor-tab
#     IS the table): they tame the host's per-row borders and redraw our rules as collapsed blank rows.
# See: CLAUDE.md Phase 13d + 14e + 14i + 14j + 14m-iii, dev/tabxplor_phase10_exporters.md.

# === SECTION: theme + slot vocabulary ==============================================================

# "auto" is a RENDER intent (follow the reader's colour scheme), never a palette. Every palette lookup
# must funnel through here: get_color_style(theme = "auto") would build the key "text_auto", find no
# palette, and error on a length-0 vector.
# z11: "print" IS a palette (a real key `text_print`/`bg_print`), so it passes through untouched --
# which is why this function needed no change for it.
#' @keywords internal
tx_palette_theme <- function(theme) {
  if (is.null(theme) || is.na(theme[1])) return("light")
  if (identical(theme[1], "auto")) "light" else theme[1]
}

# The theme VALUE vocabulary and its one alias, Phase 18z11. tx_getOption() resolves option NAME
# synonyms; a VALUE alias needs this. "print" says WHY (the destination medium) and leaves room for the
# palette to change; "bw" says HOW and is the obvious guess -- accepted silently and canonicalised here,
# so exactly ONE spelling ever reaches the palette keys. Two callers: resolve_export_opts() (so
# options(tabxplor.theme = "bw") works too) and tx_css_render().
#' @keywords internal
tx_resolve_theme <- function(theme) {
  theme <- match.arg(theme[1], c("light", "dark", "auto", "print", "bw"))
  if (identical(theme, "bw")) "print" else theme
}

# tx_theme_option() -- Phase 19h: THE theme option pair, in one place. There are two independent
# axes and they were each spelled at two call sites:
#   "export"  what a rendered/exported table should look like  (default "light", "auto" opt-in)
#   "console" the palette the terminal is using                (auto-detected from the editor)
# The alias comes FIRST in each chain (tx_getOption takes the first name set, canonical last).
# WARNING: this is the drift render_footer() had -- called on the EXPORT path but reaching for the
# CONSOLE pair when its `theme` argument was NULL, so a footer rendered outside rd_footer() silently
# picked the console theme.
#' @keywords internal
tx_theme_option <- function(scope = c("export", "console")) {
  switch(match.arg(scope),
    export  = tx_getOption(c("tabxplor.export_theme", "tabxplor.theme"), "light"),
    console = tx_getOption(c("tabxplor.console_theme", "tabxplor.color_style_theme"), "light"))
}

# tx_theme_resolve() -- THE "auto" downgrade, in one place. `"auto"` means "follow the reader",
# which needs a stylesheet WE emit; a backend that bakes its colours (Excel, ggplot, kableExtra's
# lightable themes) cannot honour it and must render something definite instead. It was written out
# three times, with three different rules and only one of them saying so.
# `note` is the one-time cli explanation, when the caller has a reason worth naming.
#' @keywords internal
tx_theme_resolve <- function(theme = NULL, allow_auto = FALSE, note = NULL,
                             scope = "export") {
  if (is.null(theme)) theme <- tx_theme_option(scope)
  theme <- tx_resolve_theme(theme)
  if (identical(theme, "auto") && !isTRUE(allow_auto)) {
    if (!is.null(note))
      cli::cli_inform(note, .frequency = "once", .frequency_id = "tabxplor_theme_auto_downgrade")
    theme <- "light"
  }
  theme
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
  switch(
    tx_palette_theme(theme),
    dark = list(text = "#CECDC3", grey = "#707070", grey2 = "#EEEEEE",
                bg = "#222222", border = "#CECDC3", hover = "rgba(255,242,204,.10)"),
    # z11: the light chrome, with ONE deliberate change. `grey` (a non-significant cell under
    # color_signif = "grey_non_signif") must stay readable ON the print background fills: #9f9f9f is
    # 1.41:1 on the darkest fill #B8B8B8, i.e. invisible; #595959 is 3.53:1 on it and 7.0:1 on white,
    # still plainly "greyed" against the pure black of a significant cell. `grey2` stays #111111 so an
    # UNCOLOURED table prints byte-identically to the light theme. `hover` is meaningless on paper.
    print = list(text = "#000000", grey = "#595959", grey2 = "#111111",
                 bg = "#ffffff", border = "#000000", hover = "transparent"),
    list(text = "#000000", grey = "#9f9f9f", grey2 = "#111111",
         bg = "#ffffff", border = "#000000", hover = "#FFFCE5")
  )
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

# The typographic declarations one theme's face adds ON TOP of the CSS baseline, Phase 18z11.
# DESIGN: tx_css_render()'s static `.p1,...,.m4{font-weight:bold;}` rule IS the light palette's face
# expressed as CSS, so it is THE BASELINE and a theme states a face property only where it DIVERGES
# from it. Two consequences fall out with no branching: light/dark emit "" everywhere (their face IS
# the baseline -- tx_css_layer() drops empty values, so they are byte-identical and the static rule
# keeps being emitted exactly once, outside the cascade), and `print` can say "not bold" on its italic
# slots (the bare `.m1` ties the static rule on specificity and wins on SOURCE ORDER, the scoped
# `.tabxplor-tab .m1` wins outright).
#' @keywords internal
tx_face_decls <- function(face, base, s) {
  d <- function(f, b, yes, no) if (identical(f[s], b[s])) "" else if (isTRUE(f[s])) yes else no
  c("font-weight"     = d(face$bold,      base$bold,      "bold",      "normal"),
    "font-style"      = d(face$italic,    base$italic,    "italic",    "normal"),
    "text-decoration" = d(face$underline, base$underline, "underline", "none"))
}

# The theme-independent rule table: one row per (selector, property), carrying the value of EVERY theme
# (light / dark / print). Built from the same slot vocabulary the cells use. `chrome = FALSE` gives
# colour rules only (the tab_md contract: bare class selectors the user maps in their own editor CSS).
#' @keywords internal
tx_css_rules <- function(chrome = TRUE) {
  sel <- character(0); prop <- character(0); lt <- character(0); dk <- character(0)
  pr  <- character(0)
  # `pr` (print) defaults to "" = "this rule has no print value", which tx_css_layer() drops.
  add <- function(s, p, l, d, pv = "") {
    sel  <<- c(sel, s); prop <<- c(prop, p); lt <<- c(lt, l); dk <<- c(dk, d); pr <<- c(pr, pv)
  }

  if (isTRUE(chrome)) {
    cl <- tx_chrome_hex("light")
    cd <- tx_chrome_hex("dark")
    # WARNING (z11): every CHROME row must state its print value EXPLICITLY. A "" would let the
    # underlying layer survive into the @media print block, so a dark page would print white-on-#222.
    cp <- tx_chrome_hex("print")
    add(".tabxplor-tab", "color",      cl$text,  cd$text,  cp$text)
    add(".tabxplor-tab", "background", cl$bg,    cd$bg,    cp$bg)
    # THE one border-colour rule -- every border in this stylesheet takes its colour from here.
    # WARNING: that only holds because no rule below uses a border SHORTHAND. `border-right:1px solid`
    # would reset border-right-color to `currentColor` = the CELL's palette hex (a +20% cell drew a
    # blue border), and `.tabxplor-tab .tx-br` (0,2,0) out-specifies this (0,1,1), so the shorthand
    # would win however this rule is written. Phase 14e wrongly recorded the bug as fixed by moving the
    # geometry off inline styles: that removed the inline half and left the shorthand half. Longhands
    # (border-*-style / border-*-width) are what make this rule load-bearing -- keep it that way; the
    # invariant is locked by test-render-html.R ("no border shorthand in the stylesheet").
    add(".tabxplor-tab th,.tabxplor-tab td", "border-color", cl$border, cd$border, cp$border)
    add(".tabxplor-tab tbody tr:hover", "background", cl$hover, cd$hover, cp$hover)
    add(tx_cell_sel("g1"), "color", cl$grey,  cd$grey,  cp$grey)
    add(tx_cell_sel("g2"), "color", cl$grey2, cd$grey2, cp$grey2)
    # Phase 15d: the table title -- FULL-contrast in both themes (pure black in light, white in dark), not
    # the softened body grey. Theme-aware so a dark-mode page keeps it legible; jamovi results are light,
    # where it is the maintainer's requested pure black.
    add(".tabxplor-caption", "color", "#000000", "#FFFFFF", "#000000")
  }

  # Phase 14l: the text channel uses the text family and the bg channel the bg family -- the loop
  # variable IS the family now the color_type override is gone (it used to repoint the text channel
  # into the fill palette, i.e. fill-coloured font).
  for (ch in c("text", "bg")) {
    pl   <- get_color_style("color_code", type = ch, theme = "light")
    pd   <- get_color_style("color_code", type = ch, theme = "dark")
    pp   <- get_color_style("color_code", type = ch, theme = "print")
    fb   <- get_color_style("face", type = ch, theme = "light")   # THE baseline (see tx_face_decls)
    fp   <- get_color_style("face", type = ch, theme = "print")
    prp  <- if (ch == "text") "color" else "background-color"
    for (s in 1:8) {
      csel <- tx_cell_sel(tx_slot_class(ch, s))
      add(csel, prp, toupper(unname(pl[s])), toupper(unname(pd[s])), toupper(unname(pp[s])))
      # z11: the print face, as its divergence from the baseline. The bg channel emits nothing (its
      # print face equals its light face), so no per-channel special case is needed here.
      fd <- tx_face_decls(fp, fb, s)
      for (k in names(fd)) add(csel, k, "", "", fd[[k]])
    }
  }
  list(sel = sel, prop = prop, light = lt, dark = dk, print = pr)
}

# DESIGN: every CELL colour class is emitted under TWO selectors -- bare (".p1") AND scoped
# (".tabxplor-tab .p1"). The bare one keeps the tab_md()/editor contract (bare classes a user maps in
# their own CSS) and reaches the legend spans, which may sit outside any .tabxplor-tab wrapper. The
# scoped twin is the HOST-PROOFING: its specificity (0,2,0) out-specifies the table-cell colour rules
# of Bootstrap-flavoured host pages -- pkgdown stamps class="table" on every table, and Bootstrap 5's
# `.table>:not(caption)>*>*` (0,1,1) then sets color/background-color on the SAME <td> our class sits
# on, beating a bare class (0,1,0) and washing every cell colour out (legend spans survived because
# there the host rule only hit the ancestor td -- direct beats inherited). (0,2,0) wins against any
# element+single-class host rule with no !important, so "restyle with ordinary CSS" still holds; a
# pathological host (ID selectors / !important on cells) needs a user override, see ?tab_css.
tx_cell_sel <- function(cls) paste0(".", cls, ",.tabxplor-tab .", cls)

# Prefix every part of a (possibly comma-separated) selector with every hook -> one comma-joined
# selector list. `.tabxplor-tab th,.tabxplor-tab td` + 2 hooks -> 4 parts.
tx_hook_sel <- function(sel, hooks) {
  vapply(sel, function(s) {
    parts <- trimws(strsplit(s, ",", fixed = TRUE)[[1]])
    paste0(as.vector(t(outer(hooks, parts, function(h, p) paste0(h, " ", p)))), collapse = ",")
  }, character(1), USE.NAMES = FALSE)
}

tx_css_layer <- function(rules, which = c("light", "dark", "print"), hooks = NULL, indent = "") {
  which <- match.arg(which)
  # z11: `val` (not `hex`) -- a rule's value is a hex on a colour row and a keyword on a face row
  # ("bold" / "italic" / "normal"). The empty-value drop below is what makes the face rows free for
  # light/dark: their face IS the CSS baseline, so they carry "" and never reach the layer.
  val   <- rules[[which]]
  keep  <- !is.na(val) & nzchar(val)
  if (!any(keep)) return(character(0))
  s    <- rules$sel[keep]
  # WARNING (z11): `val[keep]`, not `val`. This was latent -- until the face rows arrived, EVERY rule
  # carried a value in every theme, so `keep` was all-TRUE and the unsubset vector happened to line up.
  # The first rule that applies to one theme only made it recycle silently onto the wrong selectors.
  decl <- paste0(rules$prop[keep], ":", val[keep], ";")
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
tx_css_render <- function(rules, theme = "light", chrome = TRUE, print_rules = TRUE) {
  theme  <- tx_resolve_theme(theme)
  # Theme-INDEPENDENT, so it belongs here rather than in the rule table (which is emitted once per
  # cascade layer under "auto"). Phase 14c: a text-coloured cell is bold in every other medium
  # (tab_export_prep's `bold` drives kableExtra AND the html engine's inline weight), so the stylesheet
  # must carry it for the one medium that has no other way to say it: tab_md(), whose cells are bare
  # `[42%]{.p2}` spans. Harmlessly redundant on the html engine.
  # The background classes stay unbolded, exactly like the cells: a fill alone does not bold.
  # z11: this rule IS the light palette's face (get_color_style("face", "text", "light")$bold, all TRUE
  # -- asserted by test-print-palette.R), which is what lets tx_face_decls() treat it as THE baseline
  # and emit only a theme's divergences. So it stays exactly here, emitted once outside the cascade.
  bold_slots <- paste0(paste0(".", tx_slot_class("text", 1:8), collapse = ","), "{font-weight:bold;}")
  static <- c(bold_slots, if (isTRUE(chrome)) c(
    # Phase 14e: the html engine's GEOMETRY lives here, not inline on every cell. An inline style
    # cannot be overridden by a user's CSS, so "a good default you can restyle" (what kableExtra gives)
    # was impossible while the engine wrote its own borders and widths. The engine now emits ROLE
    # classes (tx-r/tx-l align, tx-num numbers, tx-br/tx-bl borders, tx-tot total col, tx-rv row-var
    # col, tx-b bold, tx-bt/tx-bb/tx-bb2 row rules, tx-span the col_var header, tx-pill a background)
    # and every look is decided here. Overriding is then ordinary CSS -- no !important needed, because
    # nothing of ours is inline any more.
    # DejaVu Sans Condensed for text (the table-wide rule below) and a MONOSPACE stack for numbers (the
    # `.tx-num` rule below, Phase g -- numbers are monospace by default so they stay column-aligned).
    # Revert with options("tabxplor.tab_kable_num_font" = <proportional stack>). All degrade through
    # their stacks.
    # `.tabxplor-tab` is the <table> itself (the html engine) OR a wrapping <div> (a markdown table
    # inside its pandoc fenced div, Phase 14f) -- `border-collapse` only means something on a table, so
    # name both. Every other rule below is a descendant selector and reaches the table either way.
    # WARNING (Phase 14j): NO border SHORTHAND anywhere below -- always border-*-style/-width. A
    # shorthand resets border-*-color to `currentColor`, i.e. the cell's own palette hex, and every
    # rule here out-specifies the one border-color rule above. Locked by test-render-html.R.
    paste0(".tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;",
           "border-top-width:0;border-bottom-width:0;",
           "margin:0;font-family:\"DejaVu Sans Condensed\",\"DejaVu Sans\",Arial,helvetica,sans-serif;}"),
    # Phase 15d: the table TITLE is a `<div class="tabxplor-caption">` sibling emitted BEFORE the <table>
    # (render_html_engine), not a `<caption>` child -- a `<caption>` participates in the table's width, so
    # a long centred title widened / wrapped thin tables. As a block div it is LEFT-aligned, fills the
    # container, and `white-space:normal` lets it wrap only when it genuinely exceeds the table width
    # (never forcing extra width, its max-content being the longest word). Its colour is theme-aware
    # (full-contrast: pure black in light, white in dark -- the maintainer's "always black, not grey"),
    # added to the rule table below. font-size 110% = a touch bigger than the table, smaller than the old
    # 120%. The legacy kableExtra engine keeps a real <caption> styled in inst/tab.css.
    ".tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}",
    ".tabxplor-tab tfoot{font-size:80%;text-align:left;}",
    # readable-compact: a real vertical rhythm (line-height 0.85 crammed the rows) + ~1mm of side
    # padding, so text no longer touches the column borders.
    ".tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}",
    # Phase 14m-iii: the markdown chrome, scoped `.tabxplor-tab table` -- md-only BY SELECTOR (it needs a
    # `table` DESCENDANT of `.tabxplor-tab`; in md that is the pandoc <div> -> <table>, in the html engine
    # `.tabxplor-tab` IS the table with no nested one, so these three NEVER match there). In md we do NOT
    # draw the borders -- the host (Bootstrap/Quarto) does, under every row -- and the `border-color` rule
    # above then recolours the host's lines black. So: (1) reset the host's per-cell border WIDTHS to 0
    # (width-only: it does NOT touch the border-color contract above -- a 0-width border never renders,
    # whatever its colour); (2) redraw ONLY our own rules as a 1px border-top on a fully-blank row (all
    # cells :empty -- uniquely OUR blank separator; a data/name row has content); (3) collapse the
    # ASCII-empty spacer/blank cells to a hairline. See decisions §43.
    # WARNING: rule (1) MUST stay BEFORE `.tabxplor-tab thead th` below -- both are (0,1,2), so the tie is
    # broken by SOURCE ORDER and thead th's border-bottom (the header underline) must win.
    # WARNING: NO border shorthand (border-width is a WIDTH property, not the `border`/`border-top`
    # shorthand that resets border-*-color; longhands border-top-style/-width redraw the rule). §40 lock.
    ".tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}",
    paste0(".tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{",
           "border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}"),
    ".tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}",
    # Phase g (A4): approximate the html `.tx-br` vertical rule between col_var groups. md tables carry
    # no per-column class, but the col_var separator IS a thin all-blank spacer column, so its cells are
    # the only `:empty` cells inside a CONTENT row (a data/header row has non-empty cells; a blank
    # SEPARATOR row is all-empty and is handled by the border-top rule above, excluded here via
    # `:has(...:not(:empty))`). A left border on those spacers draws the vertical line. Best-effort:
    # relies on data cells never being truly empty (uncoloured md cells render a space, coloured ones a
    # `[..]{.class}` span).
    paste0(".tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,",
           ".tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{",
           "border-left-style:solid;border-left-width:1px;}"),
    # Phase 18m: the whole-table TOP and BOTTOM edges, md-only BY SELECTOR (they need a `table`
    # DESCENDANT of `.tabxplor-tab` -- the pandoc div>table; the html engine's `.tabxplor-tab` IS the
    # table with no nested one -> its edges come from `> thead`/`tr.tx-bb` instead). Longhands only (the
    # no-shorthand border-colour contract). (0,2,3) beats `thead th`'s border-top-width:0 (0,1,2).
    paste0(".tabxplor-tab table > thead > tr:first-child > *{",
           "border-top-style:solid;border-top-width:1px;}"),
    paste0(".tabxplor-tab table > tbody > tr:last-child > *{",
           "border-bottom-style:solid;border-bottom-width:1px;}"),
    # Phase 18m: the right edge of the table (the grand Total / last numeric column) -- md has no
    # column-AFTER-the-last to make a spacer of, so a border-right on each content row's last cell draws
    # it. `:has(td:not(:empty))` skips the blank separator rows (no stray right tick). Matches the html
    # engine's tx-br on the final column. The interior verticals (levels|numbers, numbers|Total, col_var
    # groups) are the :empty spacer columns md inserts, handled by the border-left rule above.
    paste0(".tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,",
           ".tabxplor-tab table > thead > tr > *:last-child{",
           "border-right-style:solid;border-right-width:1px;}"),
    # Phase 18r: the LEFT edge, symmetric to the right edge above. Before Phase m the leftmost
    # column's cells were `:empty` and caught the border-LEFT spacer rule -- an ACCIDENTAL left edge.
    # Phase m's U+00A0 fill made them non-empty (killing the "ragged" edge), which also removed the
    # only thing drawing the table's left side -> the first column had no left border at all. Draw it
    # explicitly here (independent of cell emptiness); `:has(td:not(:empty))` skips the all-blank
    # separator rows exactly like the right edge. Interior verticals stay the :empty spacer columns.
    paste0(".tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,",
           ".tabxplor-tab table > thead > tr > *:first-child{",
           "border-left-style:solid;border-left-width:1px;}"),
    # Phase g (A5): the md footer is a paragraph after the table INSIDE the `.tabxplor-tab` div (the html
    # engine puts its footer in <tfoot>, styled above). `.tabxplor-tab p` is md-only by selector -- the
    # html engine's `.tabxplor-tab` IS the <table> and has no descendant <p>.
    ".tabxplor-tab p{font-size:80%;}",
    paste0(".tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;",
           "vertical-align:bottom;line-height:1;border-top-width:0;",
           "border-bottom-style:solid;border-bottom-width:1px;}"),
    # Phase 15d: draw the table's TOP edge -- the top-most header row (the col_var spanning row, e.g. the
    # "model1 model2 model3" span in a model comparison) had only a border-BOTTOM, so the table was open
    # at the top. `> thead > tr:first-child > *` is html-engine-only BY SELECTOR: it needs thead as a
    # DIRECT child of `.tabxplor-tab` (true only when `.tabxplor-tab` IS the <table>; in md it wraps a
    # nested <table>, so this never matches there and md keeps its own chrome). (0,2,2) out-specifies the
    # `thead th` border-top-width:0 (0,1,2). Longhands only -- the border-colour contract (no shorthand).
    # Phase 18r: the col_var spanning-NAME row (all cells `.tx-span`) must FLOAT above the grid --
    # no top border boxing the variable names, closed only by the `.tx-span` border-BOTTOM below them.
    # `*:not(.tx-span)` draws the top edge ONLY when the first thead row is a level-header row (no span
    # present, e.g. a single col_var / span-dropped table); a names row gets none. This deliberately
    # narrows Phase 15d's universal top edge, per the maintainer's display review.
    paste0(".tabxplor-tab > thead > tr:first-child > *:not(.tx-span){",
           "border-top-style:solid;border-top-width:1px;}"),
    paste0(".tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;",
           "border-bottom-style:solid;border-bottom-width:1px;}"),
    ".tabxplor-tab .tx-r{text-align:right;}",
    ".tabxplor-tab .tx-l{text-align:left;}",
    # thead th's `text-align:center` must beat the column's own alignment: same specificity (0,2,0)
    # vs (0,2,0), so SOURCE ORDER decides -- this pair must stay after .tx-r/.tx-l.
    ".tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}",
    # Phase g: numbers are MONOSPACE by default (was: proportional unless the table showed stars).
    # Proportional digits drift out of column alignment -- worse under the bold references / significant
    # cells the html render adds -- so the monospace stack keeps every figure column-locked. The size
    # bump (Cascadia Mono reads small) keeps the row height: 1.1em x line-height 1. Revert to a
    # proportional stack with options("tabxplor.tab_kable_num_font" = ...).
    # Phase 15d: the number FONT is BODY-only (`td.tx-num`). A numeric column HEADER carries the same
    # `tx-num` class (align + nowrap), but a `<th>` stays in the table-wide condensed sans stack -- a
    # monospace header looks wrong. `td` in the selector keeps headers on the default; `th.tx-num`
    # inherits `.tabxplor-tab{font-family:...}`.
    ".tabxplor-tab .tx-num{white-space:nowrap;}",
    paste0(".tabxplor-tab td.tx-num{font-family:", tx_num_font("html"),
           ";font-size:1.1em;line-height:1;}"),
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
    # Phase 18r: `td.tx-bb` is the CELL-scoped twin of the row rule. A rowspanned label cell (a
    # merged table's vertical row-var name) is anchored in its block's FIRST row, so `tr.tx-bb>*`
    # (last-row direct children) never reaches the one that covers the table bottom -> the bottom-left
    # corner was left open. render_kable_html() tags that single cell `tx-bb` to close it at 1px (the
    # cell is not a direct child of the tx-bb2 bottom row, so it stays 1px, not 2px -- as asked).
    ".tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}",
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

  paste0(c(static, body, tx_print_block(rules, theme, chrome, print_rules)), collapse = "\n")
}

# The black-and-white publication palette as an AT-RULE, Phase 18z11: a page rendered in colour
# PRINTS (or saves to PDF) publication-ready, with no argument and no user awareness. Emitted last, so
# at equal specificity it wins on source order.
# WARNING 1: under theme = "auto" the un-hooked layer is NOT enough. Cascade layers 3/4 are
# hook-prefixed (`body.quarto-dark .tabxplor-tab .p1` = (0,3,1)) and out-specify a plain
# `.tabxplor-tab .p1` (0,2,0) WHATEVER the source order, so a Quarto-dark page would print dark. The
# hooked twin below matches their specificity and then wins on order. Do not remove it.
# WARNING 2: every browser DROPS background-color when printing unless the reader ticks "Background
# graphics", so without print-color-adjust the grey fills would silently vanish and only the typography
# would reach the paper.
#' @keywords internal
tx_print_block <- function(rules, theme, chrome = TRUE, print_rules = TRUE) {
  if (!isTRUE(print_rules)) return(character(0))
  inner <- c(
    if (isTRUE(chrome))
      "  .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}",
    # theme = "print" already IS the print palette: re-stating it would be dead weight.
    if (!identical(theme, "print")) c(
      tx_css_layer(rules, "print", indent = "  "),
      if (identical(theme, "auto"))
        tx_css_layer(rules, "print", hooks = c(tx_light_hooks, tx_dark_hooks), indent = "  "))
  )
  if (!length(inner)) return(character(0))
  c("@media print {", inner, "}")
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
#' options(tabxplor.tab_kable_css = FALSE)
#' tab_css(theme = "auto")
#' ```
#' ````
#'
#' Every later `tab_kable()` then emits classes only. Two things to know: with `css = FALSE` and **no**
#' `tab_css()` call the tables render uncoloured; and one stylesheet means one `theme` for the whole
#' document.
#'
#' @section Restyling a table:
#' Nothing is written inline on a cell, so **any** of the look can be overridden by adding your own
#' rules after the stylesheet -- no `!important` needed. The cell colour classes are also emitted
#' scoped (`.tabxplor-tab .p1`) so they survive host pages that style table cells themselves --
#' Bootstrap-based sites (including pkgdown) apply `color`/`background-color` to every cell of a
#' `.table`, which would otherwise wash the colours out. On a pathological host (ID selectors or
#' `!important` on cells), add your own stronger override after the stylesheet. Column widths in
#' particular are left to the browser (it sizes each column to its content); to pin one, style its
#' role:
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
#' @param theme `"light"`, `"dark"`, `"print"` (the black-and-white publication palette; `"bw"` is
#'   accepted as a synonym), or -- opt-in -- `"auto"` to follow the reader's colour scheme (their
#'   operating system, and any dark-mode toggle of the host page: Quarto, Bootstrap 5.3, Tailwind).
#'   Defaults to `getOption("tabxplor.theme")`, i.e. `"light"`: a dark table is always a
#'   deliberate choice. `"auto"` emits every rule four times (a light base, the OS media query, then
#'   both toggle directions), which is also what lets [tab_kable()]'s own Viewer page force the
#'   editor's theme -- see its `theme` argument.
#' @param print_rules Also emit the black-and-white publication palette inside an `@media print`
#'   block, so a coloured page prints (or saves to PDF) publication-ready with no further action.
#'   Defaults to `getOption("tabxplor.print_rules")`. Set to `FALSE` if your printer is a colour one
#'   and the colours are the point. It adds roughly 1.5 KB to a `light`/`dark` stylesheet and 6 KB to
#'   an `"auto"` one (where the rules must also be emitted against the page-toggle hooks, which would
#'   otherwise out-specify them).
#' @param color_type `r lifecycle::badge("deprecated")` Inert since 2.0.0: the text channel always uses
#'   the text palette. The colour CHANNEL is chosen by `color = c(text, background)` (see [tab()]).
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
tab_css <- function(theme = NULL, color_type = lifecycle::deprecated(), chrome = TRUE,
                    style_tag = TRUE, file = NULL, print_rules = NULL) {
  if (lifecycle::is_present(color_type)) lifecycle::deprecate_soft("2.0.0", "tab_css(color_type)")
  o   <- resolve_export_opts(theme = theme, allow_auto = TRUE)
  # z11: NULL -> option is the package idiom (cf. engine / popover / css / tooltips), and it is why
  # tab_html()/tab_md() need NO argument of their own -- they call tab_css() internally, so a user with
  # a colour printer sets options(tabxplor.print_rules = FALSE) once for a whole document.
  if (is.null(print_rules)) print_rules <- getOption("tabxplor.print_rules", TRUE)
  css <- tx_css_render(tx_css_rules(chrome = chrome), o$theme, chrome = chrome,
                       print_rules = print_rules)
  if (isTRUE(style_tag)) css <- paste0("<style>\n", css, "\n</style>")
  if (!is.null(file)) {
    writeLines(css, file)
    return(invisible(css))
  }
  css
}
