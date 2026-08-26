# PURPOSE: the ONE CSS generator -- every stylesheet tabxplor emits, for every theme.
# ROLE: turns (palette, theme) into rules, for the two media that can carry a stylesheet: the html
#   engine and tab_md(). Cells and CSS read the SAME slot vocabulary (tx_slot_class), so they cannot
#   disagree, and tx_chrome_hex() (R/tab-palettes.R) is the one source of the chrome colours, which
#   the exporter prep's `theme_cols` reads too.
# KEY CONSTRAINTS:
#   - THE CSS IS TABLE-INDEPENDENT: a pure function of (palette, theme). That is the whole point of
#     naming a class after a palette SLOT rather than a break value. It lets a document emit the
#     stylesheet ONCE and reuse it for every table, and it makes class collisions impossible --
#     `.p3` is the same shade in every table, whatever its color_breaks.
#   - "auto" is a RENDER intent, never a palette. Every palette lookup funnels through
#     tx_palette_theme(), or a key like "text_auto" gets built and errors on a length-0 vector.
#   - WARNING: NO BORDER SHORTHAND, anywhere. A shorthand resets border-*-color to `currentColor` --
#     the cell's own palette hex -- and every border rule here out-specifies the one border-color
#     rule. Always border-*-style plus border-*-width. Locked by test-render-html.R.
#   - WARNING: specificity is load-bearing in three places. `thead .tx-unit` is (0,2,1) like the
#     `thead .tx-r` / `.tx-l` pair, so it must be emitted AFTER them or the unit row re-centres
#     itself; `.tx-bb` and `.tx-bb2` are identical at (0,3,1), so their order decides; and the layer
#     order (the OS media query, then both page-toggle directions) is what lets a host's dark-mode
#     switch win over the base rules.
#   - THE HOST RESET is one rule for both media, `.tabxplor-tab th, .tabxplor-tab td
#     { border-width: 0 }`. A host page draws a border under every row (Bootstrap's
#     `.table > :not(caption) > * > *`, which pkgdown stamps on every table) and our border-colour
#     rule then paints it black. Every border we DRAW is a role class at (0,2,0) or better, so the
#     reset takes the host's and leaves ours. It is medium-agnostic on purpose: in the html engine
#     `.tabxplor-tab` IS the <table>, so a selector needing a `table` DESCENDANT never matches there.
#     The `.tabxplor-tab table ...` rules beside it are MD-ONLY by that same selector -- they redraw
#     our own chrome, which md carries no per-cell class for.
#   - THE COLOUR THEMES STATE `background-color: transparent` -- exactly what a cell has with no rule
#     at all -- so a host cannot override it and paint over the row hover. The background colour
#     CHANNEL (`.o3`, `.tx-pill`) is (0,2,0) and still wins.
#   - NO COLUMN WIDTH: the browser's auto table layout sizes each column to its content.
#     `.tx-rv` / `.tx-tot` / `.tx-num` are emitted UNSTYLED, as hooks for a reader's own CSS.
#   - WARNING: a browser does NOT grow a rowspanned cell to hold vertical text that overruns it. The
#     budget that prevents one is tab_vname_plan() (R/tab-export-prep.R), not a rule here -- a
#     max-height would truncate a variable name silently.
#   - WARNING: the tooltip selector cannot be scoped -- bootstrap moves the element to <body>.
#   - PRINT RULES: browsers drop background-color unless the reader enables "Background graphics", so
#     a publication palette speaks with ink and face. `print_marks` is REFUSED there: its signal is
#     cell text, and a print rule can restyle a page but not add characters to it.
# See: CLAUDE.md section "tabxplor architecture" (the colour system); R/tab-palettes.R (what a slot
#      IS); R/tab-export-prep.R (the three header rows).

# === SECTION: theme + slot vocabulary ==============================================================

# a print palette IS a palette (a real key `text_print`, ...), so it passes through untouched here.
tx_palette_theme <- function(theme) {
  if (is.null(theme) || is.na(theme[1])) return("light")
  # `print_ready` is a choice BETWEEN palettes (tx_theme_for_table); anything reaching a lookup still
  # holding it had no table to choose from, so it takes the declared fallback.
  if (identical(theme[1], "print_ready")) return(unname(PRINT_READY[["fallback"]]))
  if (identical(theme[1], "auto")) "light" else theme[1]
}

# tx_getOption() resolves option NAME synonyms; this is the VALUE alias: "bw" is the obvious guess
# for "print_minimalistic" and is accepted and canonicalised here, so exactly ONE spelling ever
# reaches the palette keys.
tx_resolve_theme <- function(theme) {
  theme <- match.arg(theme[1], c("light", "dark", "auto", "bw", "print_ready",
                                 names(PRINT_PALETTES)))
  if (identical(theme, "bw")) "print_minimalistic" else theme
}

# TWO independent axes, easy to conflate:
#   "export"  what a rendered/exported table should look like  (default "light", "auto" opt-in)
#   "console" the palette the terminal is using                (auto-detected from the editor)
# WARNING: reaching for the console pair on the export path (or vice-versa) silently picks the wrong
# theme -- render_footer() once did this when its `theme` argument was NULL.
tx_theme_option <- function(scope = c("export", "console")) {
  # the name chain and the default come from TAB_OPTIONS, so a renamed option reaches every reader.
  switch(match.arg(scope),
    export  = tx_option("theme"),
    console = tx_getOption(tx_option_names("color_style_theme"), "light"))
}

# "auto" means "follow the reader", which needs a stylesheet WE emit; a backend that bakes its
# colours (Excel, ggplot, kableExtra's lightable themes) cannot honour it and downgrades to "light"
# here instead. `note` is the one-time cli explanation, when the caller has a reason worth naming.
tx_theme_resolve <- function(theme = NULL, allow_auto = FALSE, note = NULL,
                             scope = "export") {
  if (is.null(theme)) theme <- tx_theme_option(scope)
  theme <- tx_resolve_theme(theme)
  if (identical(theme, "auto") && !isTRUE(allow_auto)) {
    if (!is.null(note)) tx_inform_once("theme_auto_downgrade", note)
    theme <- "light"
  }
  theme
}

# tx_chrome_hex() (R/tab-palettes.R) is the chrome PALETTE; tx_css_rules() emits it as CSS and
# tab_export_prep()'s `theme_cols` reads it inline, so the two renderings cannot drift.

# an R colour name or a hex code -> a CSS hex; anything unrecognised falls back to NULL (the caller
# then uses the theme's own text colour), so a typo can never emit invalid CSS.
#' @noRd
tx_css_color <- function(x) {
  if (is.null(x) || !nzchar(x)) return(NULL)
  if (grepl("^#[0-9A-Fa-f]{3,8}$", x)) return(x)
  tryCatch({
    v <- grDevices::col2rgb(x)
    sprintf("#%02X%02X%02X", v[1], v[2], v[3])
  }, error = function(e) NULL)
}

# ours is LAST in document order at equal specificity (0,0,1), which is all it takes -- no !important.
# WARNING: this must NEVER reach tab_css(). A knitted/Quarto page is the HOST's, not ours -- painting
# its html,body would repaint the whole document around the table.
tx_page_style <- function(theme = "light") {
  decl <- function(t) {
    ch <- tx_chrome_hex(t)
    paste0("html,body{background:", ch$bg, ";color:", ch$text, ";}")
  }
  # "auto" here means a standalone file with no framework toggle to follow, so only the OS signal
  # applies; an interactive print resolves "auto" in R before calling here.
  if (identical(theme, "auto")) {
    paste0(decl("light"), "\n@media (prefers-color-scheme: dark){", decl("dark"), "}")
  } else {
    decl(tx_palette_theme(theme))
  }
}

# slot integer -> class name. Slot 0 = uncoloured, 1-4 over-represented, 5-8 under-represented, per
# channel. Names are 2 chars and uniform-width, which keeps raw markdown aligned in monospace:
#   text : .p1-.p4 (over, "plus")   .m1-.m4 (under, "minus")
#   bg   : .o1-.o4 (over)           .u1-.u4 (under)
# DESIGN: keyed to the SLOT, not the break value, so the name is palette-shade identity, not a
# threshold -- this is what makes the stylesheet table-independent. Slot 0 (and NA) -> "".
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

# these hooks translate a framework's dark TOGGLE (a class/attribute set via JS) into something a
# media query cannot see; both directions let an explicit toggle win over the OS either way.
# WARNING: keep these in ONE place -- they are the only part of the design that can rot upstream.
# KNOWN GAP, deliberately not fixed: no Tailwind light entry -- Tailwind expresses light as the ABSENCE
# of `html.dark`, so a Tailwind page under a dark OS shows a dark table on a light page.
# NOT HERE, deliberately: the Positron Viewer is a cross-origin webview iframe, so no selector here can
# ever reach its outer workbench body -- print.tabxplor_kable() resolves "auto" in R instead and sets
# [data-theme] on the page it builds.
tx_dark_hooks  <- c("body.quarto-dark",  "[data-bs-theme=dark]",  "[data-theme=dark]", "html.dark")
tx_light_hooks <- c("body.quarto-light", "[data-bs-theme=light]", "[data-theme=light]")

# DESIGN: tx_css_render()'s static `.p1,...,.m4{font-weight:bold;}` rule IS the light palette's face
# expressed as CSS -- THE BASELINE -- so a theme states a face property only where it DIVERGES from it.
# light/dark then emit "" everywhere (tx_css_layer() drops empty values, so they stay byte-identical),
# and `print` can say "not bold" on its italic slots.
tx_face_decls <- function(face, base, s) {
  d <- function(f, b, yes, no) if (identical(f[s], b[s])) "" else if (isTRUE(f[s])) yes else no
  # underline is a three-value vocabulary: a doubled rule is one CSS declaration ("underline double").
  und <- if (identical(face$underline[s], base$underline[s])) ""
         else switch(face$underline[s], single = "underline", double = "underline double", "none")
  c("font-weight"     = d(face$bold,   base$bold,   "bold",   "normal"),
    "font-style"      = d(face$italic, base$italic, "italic", "normal"),
    "text-decoration" = und)
}

# The theme-independent rule table: one row per (selector, property), carrying the value of EVERY
# theme. `chrome = FALSE` gives colour rules only (the tab_md contract: bare classes a user maps in
# their own editor CSS). `print_theme` names WHICH black-and-white palette fills the "print" column --
# the layer's name, not the palette's, since it may be the page's own theme or a fallback.
tx_css_rules <- function(chrome = TRUE, print_theme = "print_minimalistic") {
  sel <- character(0); prop <- character(0); lt <- character(0); dk <- character(0)
  pr  <- character(0)
  # `pr` (print) defaults to "" = "this rule has no print value", which tx_css_layer() drops.
  add <- function(s, p, l, d, pv = "") {
    sel  <<- c(sel, s); prop <<- c(prop, p); lt <<- c(lt, l); dk <<- c(dk, d); pr <<- c(pr, pv)
  }

  if (isTRUE(chrome)) {
    cl <- tx_chrome_hex("light")
    cd <- tx_chrome_hex("dark")
    # WARNING: every CHROME row must state its print value EXPLICITLY -- a "" would let the underlying
    # layer survive into @media print, so a dark page would print white-on-#222.
    cp <- tx_chrome_hex(print_theme)
    add(".tabxplor-tab", "color",      cl$text,  cd$text,  cp$text)
    add(".tabxplor-tab", "background", cl$bg,    cd$bg,    cp$bg)
    # THE HOST PAINTS OUR CELLS DIRECTLY (pkgdown/Bootstrap's `.table>:not(caption)>*>*` sets color,
    # background-color and border-bottom-width on the same <td> our classes sit on), so the values it
    # can reach must be stated ON THE CELLS, at (0,1,1) -- ties the host's rule and wins on source
    # order, while still losing to `.tabxplor-tab .p1` (0,2,0).
    # a PUBLICATION PALETTE IS A SHEET OF PAPER, all-or-nothing -- print states its ink here; light/dark
    # carry "" and follow the page on purpose, `auto` most of all.
    add(".tabxplor-tab th,.tabxplor-tab td", "color",            "", "", cp$text)
    # "follow the page" must be SAID: Bootstrap's own background is opaque (`--bs-table-bg`), and an
    # opaque cell paints over its row hover. `transparent` is what a cell has with no rule at all.
    add(".tabxplor-tab th,.tabxplor-tab td", "background-color",
        "transparent", "transparent", cp$bg)
    # THE one border-colour rule -- every border in this stylesheet takes its colour from here.
    # WARNING: that only holds because no rule below uses a border SHORTHAND (`border-right:1px solid`
    # would reset border-right-color to the CELL's palette hex). Longhands only; locked by
    # test-render-html.R.
    add(".tabxplor-tab th,.tabxplor-tab td", "border-color", cl$border, cd$border, cp$border)
    add(".tabxplor-tab tbody tr:hover", "background", cl$hover, cd$hover, cp$hover)
    add(tx_cell_sel("g1"), "color", cl$grey,  cd$grey,  cp$grey)
    add(tx_cell_sel("g2"), "color", cl$grey2, cd$grey2, cp$grey2)
    # the unit row takes the chrome's `grey`, not the aside's `grey2`, and is NOT gated on
    # `color_whole_cell` -- it names the column, unrelated to where a colour stops inside a cell.
    add(".tabxplor-tab .tx-unit", "color", cl$grey, cd$grey, cp$grey)
    # the table title is FULL-contrast in both themes (pure black / white), not the softened body grey.
    add(".tabxplor-caption", "color", "#000000", "#FFFFFF", "#000000")
    # THE SECONDARY TOKENS (a composite cell's aside) are set back from the table's own text, resolved
    # per theme like every chrome rule. Under `color_whole_cell` no rule is emitted: the aside then
    # inherits the cell's own shade, though the span is still written so a stylesheet can restyle it.
    if (!color_whole_cell_opt()) {
      add(".tabxplor-tab .tx-sec", "color", color_secondary_hex("light"),
          color_secondary_hex("dark"), color_secondary_hex(print_theme))
      # the FACE reset, print-only (only that palette has one): the aside stops at the primary exactly
      # as the colour does.
      # WARNING: `display:inline-block` is the load-bearing property, not `text-decoration:none` --
      # a CSS text-decoration is drawn by the ancestor across every descendant and can only be
      # switched off by making the span an atomic inline box.
      sec_face <- c("font-style" = "normal", "text-decoration" = "none",
                    "display" = "inline-block")
      for (k in names(sec_face)) add(".tabxplor-tab .tx-sec", k, "", "", sec_face[[k]])
    }
    # THE EFFECT-SIZE MARKS sit where the stars sit and are NOT an aside: they replace the colour, so
    # they carry the deviation itself in the chrome's own `mark` ink, print-only like the sec face above.
    add(".tabxplor-tab .tx-mark", "color", "", "", tx_chrome_hex(print_theme)$mark)
    for (k in c("font-style", "text-decoration", "display"))
      add(".tabxplor-tab .tx-mark", k,
          "", "", c("font-style" = "normal", "text-decoration" = "none",
                    "display" = "inline-block")[[k]])
  }

  for (ch in c("text", "bg")) {
    pl   <- get_color_style("color_code", type = ch, theme = "light")
    pd   <- get_color_style("color_code", type = ch, theme = "dark")
    pp   <- get_color_style("color_code", type = ch, theme = print_theme)
    fb   <- get_color_style("face", type = ch, theme = "light")   # THE baseline (see tx_face_decls)
    fp   <- get_color_style("face", type = ch, theme = print_theme)
    prp  <- if (ch == "text") "color" else "background-color"
    for (s in 1:8) {
      csel <- tx_cell_sel(tx_slot_class(ch, s))
      add(csel, prp, toupper(unname(pl[s])), toupper(unname(pd[s])), toupper(unname(pp[s])))
      # the bg channel emits nothing here (its print face equals its light face) -- no special case.
      fd <- tx_face_decls(fp, fb, s)
      for (k in names(fd)) add(csel, k, "", "", fd[[k]])
    }
  }
  list(sel = sel, prop = prop, light = lt, dark = dk, print = pr)
}

# DESIGN: every CELL colour class is emitted under TWO selectors -- bare (".p1") AND scoped
# (".tabxplor-tab .p1"). The bare one keeps the tab_md()/editor contract and reaches legend spans that
# may sit outside any .tabxplor-tab wrapper. The scoped twin is HOST-PROOFING: its specificity (0,2,0)
# out-specifies a Bootstrap host's table-cell rules (pkgdown's `.table>:not(caption)>*>*`, (0,1,1)),
# which would otherwise wash every cell colour out. A pathological host (ID selectors / !important)
# still needs a user override, see ?tab_css.
tx_cell_sel <- function(cls) paste0(".", cls, ",.tabxplor-tab .", cls)

# prefixes every part of a (possibly comma-separated) selector with every hook.
# `.tabxplor-tab th,.tabxplor-tab td` + 2 hooks -> 4 parts.
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
# WARNING: do not reorder -- layer 3 before 4 and both after 2 is what makes an explicit host toggle
# override the reader's OS preference in BOTH directions.
tx_css_render <- function(rules, theme = "light", chrome = TRUE, print_rules = TRUE) {
  theme  <- tx_resolve_theme(theme)
  # theme-INDEPENDENT: a text-coloured cell is bold in every other medium, so the stylesheet must carry
  # it for tab_md(), whose cells are bare `[42%]{.p2}` spans (harmlessly redundant on the html engine).
  # This rule IS the light palette's face (asserted by test-print-palette.R), which is what lets
  # tx_face_decls() treat it as THE baseline and emit only a theme's divergences.
  bold_slots <- paste0(paste0(".", tx_slot_class("text", 1:8), collapse = ","), "{font-weight:bold;}")
  static <- c(bold_slots, if (isTRUE(chrome)) c(
    # the html engine's GEOMETRY lives here, never inline on a cell, so it stays user-restyleable with
    # no !important needed. The engine emits ROLE classes (tx-r/tx-l align, tx-num numbers, tx-br/tx-bl
    # borders, tx-tot/tx-rv, tx-b bold, tx-bt/tx-bb/tx-bb2 row rules, tx-span, tx-pill) and every look
    # is decided here.
    # DejaVu Sans Condensed for text, a MONOSPACE stack for numbers (`.tx-num`, so figures stay
    # column-aligned). Revert with options("tabxplor.tab_kable_num_font" = <proportional stack>).
    # `.tabxplor-tab` is the <table> itself (html engine) OR a wrapping <div> (a markdown table's
    # pandoc fenced div) -- name both, since `border-collapse` only means something on a table.
    # WARNING: NO border SHORTHAND anywhere below -- always border-*-style/-width, or it resets
    # border-*-color to the cell's own palette hex. Locked by test-render-html.R.
    paste0(".tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;",
           "border-top-width:0;border-bottom-width:0;",
           "margin:0;font-family:\"DejaVu Sans Condensed\",\"DejaVu Sans\",Arial,helvetica,sans-serif;}"),
    # the table TITLE is a `<div class="tabxplor-caption">` sibling emitted BEFORE the <table>, not a
    # `<caption>` child (see above). `width:0;min-width:100%` is the same idiom as `.tx-foot` below --
    # otherwise a long title would SIZE a shrink-to-fit container (jamovi's `.tx-scrollbox`). Its colour
    # (full-contrast) is added to the rule table below.
    paste0(".tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;",
           "width:0;min-width:100%;}"),
    ".tabxplor-tab tfoot{font-size:80%;text-align:left;}",
    # readable-compact: a real vertical rhythm (line-height 0.85 crammed the rows) + ~1mm of side
    # padding, so text no longer touches the column borders.
    ".tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}",
    # THE HOST RESET (see the file header). WARNING: width-only, so the border-COLOUR contract above
    # is untouched (`border-width` is not the `border`/`border-top` shorthand).
    ".tabxplor-tab th,.tabxplor-tab td{border-width:0;}",
    # the two rules below redraw OUR OWN chrome in markdown, md-only by selector (they need a `table`
    # DESCENDANT of `.tabxplor-tab`, absent in the html engine). A md table carries no per-cell class,
    # so a row separator is a border-top on a fully-blank row (all cells :empty, our own spacer).
    paste0(".tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{",
           "border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}"),
    ".tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}",
    # approximates `.tx-br` (the col_var vertical rule) in md: the separator column IS the only
    # `:empty` cells inside a content row, so a left border on them draws the line. Best-effort: relies
    # on data cells never being truly empty.
    paste0(".tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,",
           ".tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{",
           "border-left-style:solid;border-left-width:1px;}"),
    # the whole-table TOP/BOTTOM edges, md-only by selector (the html engine's edges come from
    # `> thead`/`tr.tx-bb` instead). Longhands only.
    paste0(".tabxplor-tab table > thead > tr:first-child > *{",
           "border-top-style:solid;border-top-width:1px;}"),
    paste0(".tabxplor-tab table > tbody > tr:last-child > *{",
           "border-bottom-style:solid;border-bottom-width:1px;}"),
    # the RIGHT edge (md has no column-after-the-last to spacer): matches the html engine's tx-br on
    # the final column. `:has(td:not(:empty))` skips the blank separator rows.
    paste0(".tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,",
           ".tabxplor-tab table > thead > tr > *:last-child{",
           "border-right-style:solid;border-right-width:1px;}"),
    # the LEFT edge, symmetric to the right edge -- drawn explicitly (independent of cell emptiness)
    # since the leftmost column's own cells are non-empty and draw nothing on their own.
    paste0(".tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,",
           ".tabxplor-tab table > thead > tr > *:first-child{",
           "border-left-style:solid;border-left-width:1px;}"),
    # the md footer is a paragraph after the table inside the `.tabxplor-tab` div; `.tabxplor-tab p` is
    # md-only by selector (the html engine's div IS the <table>, with no descendant <p>).
    ".tabxplor-tab p{font-size:80%;}",
    paste0(".tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;",
           "vertical-align:bottom;line-height:1;border-top-width:0;",
           "border-bottom-style:solid;border-bottom-width:1px;}"),
    # draws the table's TOP edge, html-engine-only by selector (needs thead as a DIRECT child of
    # `.tabxplor-tab`, true only when it IS the <table>). The col_var spanning-NAME row (`.tx-span`)
    # must FLOAT above the grid with no top border, so `*:not(.tx-span)` draws the edge only when the
    # first thead row is a plain level-header row.
    paste0(".tabxplor-tab > thead > tr:first-child > *:not(.tx-span){",
           "border-top-style:solid;border-top-width:1px;}"),
    paste0(".tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;",
           "border-bottom-style:solid;border-bottom-width:1px;}"),
    ".tabxplor-tab .tx-r{text-align:right;}",
    ".tabxplor-tab .tx-l{text-align:left;}",
    # same specificity as .tx-r/.tx-l (0,2,0), so SOURCE ORDER decides -- must stay after them.
    ".tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}",
    # THE UNIT ROW -- the console's own type tag ("<row%>", "<n>"). Discrete by design: small, regular
    # weight, italic, chrome grey, no rule of its own, reading as the header's second line.
    # specificity: (0,2,1) like the `thead .tx-r/.tx-l` pair above (see file header) -- left-aligned,
    # since the tag names the column, not the numbers under it.
    paste0(".tabxplor-tab thead .tx-unit{font-weight:normal;font-style:italic;font-size:80%;",
           "text-align:left;border-top-width:0;padding-top:0;}"),
    # no horizontal rule between the level names and the unit row -- `thead th` gives every header cell
    # a bottom rule, so the block is closed by the unit row's own instead.
    # ⚠ `:not([rowspan])` is load-bearing: an INDEX column's header spans both rows and has no unit
    # cell under it, so dropping this exception would leave the levels column open onto the data.
    ".tabxplor-tab thead tr:has(+ tr > .tx-unit) > th:not([rowspan]){border-bottom-width:0;}",
    # numbers are MONOSPACE by default -- proportional digits drift out of column alignment, worse
    # under bold references. Revert with options("tabxplor.tab_kable_num_font" = <proportional stack>).
    # BODY-only (`td.tx-num`): a numeric column HEADER carries the same class but a `<th>` stays in the
    # table-wide condensed sans stack -- a monospace header looks wrong.
    ".tabxplor-tab .tx-num{white-space:nowrap;}",
    paste0(".tabxplor-tab td.tx-num{font-family:", tx_num_font("html"),
           ";font-size:1.1em;line-height:1;}"),
    ".tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}",
    ".tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}",
    # `.tx-tot`/`.tx-rv` are EMITTED with no rule of their own (see NO COLUMN WIDTH, file header) --
    # they remain as hooks: `.tx-rv{min-width:10em}` in a user's stylesheet is the fixed-width escape.
    # a LABEL cell (rowspanned over its block) centres itself on the block it names.
    ".tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}",
    # a row-variable NAME is written vertically (see the rowspan overrun budget, file header) -- NOT
    # `writing-mode:sideways-lr`, which is still flagged experimental; `vertical-rl` + rotate(180deg)
    # is the universally-supported equivalent, matching the 90-degree rotation tab_xl writes to Excel.
    paste0(".tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);",
           "white-space:normal;padding:4px 2px;}"),
    ".tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}",
    ".tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}",
    # WARNING: tx-bb/tx-bb2 are identical at (0,3,1) -- see the file header. `td.tx-bb` is the
    # CELL-scoped twin: a rowspanned label cell is anchored in its block's FIRST row, so `tr.tx-bb>*`
    # never reaches it and the bottom-left corner was left open; render_kable_html() tags that cell
    # directly.
    ".tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}",
    ".tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}",
    # a row separator does not cross the variable-name column: a one-row block's name cell is a direct
    # child of the closing row and would draw a rule its neighbours did not. `tx-nb` opts it out; a
    # name cell that DOES close a boundary carries `tx-bb`/`tx-bb2` instead, never both.
    ".tabxplor-tab tr.tx-bb>.tx-nb,.tabxplor-tab tr.tx-bb2>.tx-nb{border-bottom-style:none;}",
    ".tabxplor-tab td.tx-bb2{border-bottom-style:solid;border-bottom-width:2px;}",
    ".tabxplor-tab tr.tx-bt2>*{border-top-style:solid;border-top-width:2px;}",
    # the footnote must not SIZE the table: `width:0` is a definite size (contributes 0 to
    # max-content), and once the cell's own width is definite `min-width:100%` resolves and the text
    # fills it -- the same idiom as `.tabxplor-caption` above.
    ".tabxplor-tab .tx-foot{width:0;min-width:100%;}",
    # a background HUGS its text (rounded, inline) rather than flooding the cell: a full fill reads as
    # a blocky grid and swallows the row hover.
    # ⚠ the negative margin CANCELS the padding's layout, so a filled number does not shift left of an
    # unfilled one in a right-aligned column -- the fill bleeds around the glyphs, never moves them.
    ".tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;margin:0 -4px;}",
    # a row sparkline is a PLOT and gets the whole cell: centred regardless of the column's text-align,
    # with no border of its own (the cell's own rule already draws the rectangle around it).
    ".tabxplor-tab .tx-spark{display:block;margin:0 auto;}",
    ".tabxplor-tab .tx-sparkcell{vertical-align:middle;text-align:center;padding:1px 2px;}",
    # a cell tooltip is one line of "field: value ; field: value" prose, but bootstrap caps
    # .tooltip-inner at 200px and wraps it to four. WARNING: this selector cannot be scoped (see the
    # file header) and so applies to any other bootstrap tooltip on the host page too -- accepted,
    # since a one-line tooltip is what every bootstrap tooltip wants.
    # `pre`, not `nowrap`: a regression cell's tooltip is TWO lines (its own numbers, then the observed
    # comparison), and `pre` also honours the newline.
    ".tooltip-inner{max-width:none;white-space:pre;}",
    # the same for a POPOVER: bootstrap caps `.popover` at 276px. `.popover-body`/`.popover-content`
    # names both bootstrap 4/5 and 3, version-agnostically. Geometry only, deliberately no colour --
    # this selector is as unscopable as .tooltip-inner, so a colour override would repaint the HOST
    # page's popovers too.
    ".popover{max-width:none;}",
    ".popover-body,.popover-content{padding:6px;white-space:pre;}"
  ) else character(0))

  body <- if (identical(theme, "auto")) {
    dark_media <- tx_css_layer(rules, "dark", indent = "  ")
    c(tx_css_layer(rules, "light"),
      if (length(dark_media)) c("@media (prefers-color-scheme: dark) {", dark_media, "}"),
      tx_css_layer(rules, "light", hooks = tx_light_hooks),
      tx_css_layer(rules, "dark",  hooks = tx_dark_hooks))
  } else {
    # every member of the print family is carried by the one "print" column of the rule table, which
    # tx_css_rules(print_theme=) has already filled with the palette actually asked for.
    tx_css_layer(rules, if (tx_is_print(theme)) "print" else theme)
  }

  paste0(c(static, body, tx_print_block(rules, theme, chrome, print_rules)), collapse = "\n")
}

# WHICH publication palette a COLOURED page falls back to when it is printed. `TRUE` = the default
# one, `FALSE` = no print layer, a palette name = that one, NULL out = "emit nothing".
# WARNING: a MARKS palette is refused -- its signal is cell TEXT, which a media query cannot add.
tx_print_rules_palette <- function(print_rules) {
  if (is.null(print_rules) || isFALSE(print_rules)) return(NULL)
  if (isTRUE(print_rules)) return("print_minimalistic")
  nm <- as.character(print_rules)[1]
  # `print_ready` is refused for the same reason its crosstab arm is: it CAN resolve to the marks.
  if (identical(nm, "print_ready")) cli::cli_abort(
    c("{.val print_ready} cannot be a print-media fallback: it may resolve to a palette that writes
       marks into the cells, which a print rule cannot add.",
      "i" = 'Name one palette: {.code print_rules = "print_emphasis"}.'))
  if (!tx_is_print(nm)) cli::cli_abort(
    c("{.arg print_rules} must be {.val TRUE}, {.val FALSE}, or a publication palette.",
      "i" = "Available: {.val {names(PRINT_PALETTES)}}."))
  if (print_palette_marks(print_palette_of(nm))) cli::cli_abort(
    c("{.val {nm}} cannot be a print-media fallback: its marks are cell text, which a print rule
       cannot add.",
      "i" = 'Render the table with {.code theme = "{nm}"} instead.'))
  nm
}

# The black-and-white publication palette as an AT-RULE: a page rendered in colour PRINTS (or saves to
# PDF) publication-ready, with no argument and no user awareness.
# WARNING 1: under theme = "auto" the un-hooked layer alone is not enough -- cascade layers 3/4 are
# hook-prefixed (0,3,1) and out-specify a plain rule (0,2,0) whatever the source order, so a
# Quarto-dark page would print dark without the hooked twin below.
# WARNING 2: every browser DROPS background-color when printing unless the reader ticks "Background
# graphics", so without print-color-adjust the grey fills would silently vanish.
tx_print_block <- function(rules, theme, chrome = TRUE, print_rules = TRUE) {
  if (!isTRUE(print_rules)) return(character(0))
  inner <- c(
    if (isTRUE(chrome))
      "  .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}",
    # a print theme already IS the publication palette: re-stating it would be dead weight.
    if (!tx_is_print(theme)) c(
      tx_css_layer(rules, "print", indent = "  "),
      if (identical(theme, "auto"))
        tx_css_layer(rules, "print", hooks = c(tx_light_hooks, tx_dark_hooks), indent = "  "))
  )
  if (!length(inner)) return(character(0))
  c("@media print {", inner, "}")
}

# === SECTION: the public generator =================================================================

# WARNING: in the "Two workflows" section below, the FOUR-backtick fence is load-bearing, and so is
# carrying no `{r}` info string -- roxygen2 (>= 7.1) EVALUATES a ```{r} chunk in roxygen markdown and
# splices its output into the page, so a three-backtick chunk meant to be SHOWN, not run, needs the
# longer fence. Never mix raw Rd (`\preformatted{}`) with a code fence for the same reason.

#' Generate the tabxplor stylesheet
#'
#' The CSS that colours tabxplor tables. It is a **constant** -- a pure function of the colour palette,
#' the channel type and the theme -- so it does not take a table: one stylesheet styles every table in a
#' document, whatever their `color_breaks`.
#'
#' Cells carry classes named after the palette **slot** (`.p1`-`.p4` over-represented text, `.m1`-`.m4`
#' under-represented text, `.o1`-`.o4` / `.u1`-`.u4` for the background channel), so [tab_html()] and
#' [tab_md()] share one vocabulary.
#'
#' @section Two workflows:
#' **Self-contained (the default).** `tab_html(css = TRUE)` and `tab_md(css = TRUE)` inline the
#' stylesheet with the table, so a single file works anywhere (the RStudio/Positron Viewer, jamovi, a
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
#' Every later [tab_html()] then emits classes only. Two things to know: with `css = FALSE` and **no**
#' `tab_css()` call the tables render uncoloured; and one stylesheet means one `theme` for the whole
#' document.
#'
#' @section Restyling a table:
#' Nothing is written inline on a cell, so **any** of the look can be overridden by adding your own
#' rules after the stylesheet -- no `!important` needed. The cell colour classes are also emitted
#' scoped (`.tabxplor-tab .p1`) so they survive host pages that style table cells themselves, such as
#' Bootstrap-based sites including pkgdown. Column widths in particular are left to the browser, which
#' sizes each column to its content; to pin one, style its role:
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
#' @param theme `"light"`, `"dark"`, a black-and-white publication palette (`"print_ready"`,
#'   `"print_marks"`, `"print_emphasis"`, `"print_minimalistic"`; `"bw"` is a synonym of the last --
#'   see the section below), or -- opt-in -- `"auto"` to follow the reader's colour scheme (their
#'   operating system, and any dark-mode toggle of the host page: Quarto, Bootstrap 5.3, Tailwind).
#'   Defaults to `getOption("tabxplor.theme")`, i.e. `"light"`: a dark table is always a deliberate
#'   choice. `"auto"` emits every rule four times (a light base, the OS media query, then both toggle
#'   directions), which is also what lets [tab_html()]'s own Viewer page force the editor's theme.
#' @param print_rules Also emit a black-and-white publication palette inside an `@media print`
#'   block, so a coloured page prints (or saves to PDF) publication-ready with no further action.
#'   Defaults to `getOption("tabxplor.print_rules")`. Set to `FALSE` if your printer is a colour one
#'   and the colours are the point, or name a palette (`"print_emphasis"`) to print in that one.
#'   `"print_marks"` cannot be used here: its marks are cell text, and a print rule can restyle a
#'   page but not add characters to it. It adds roughly 1.5 KB to a `light`/`dark` stylesheet and
#'   6 KB to an `"auto"` one.
#' @param ... Retired arguments, accepted and ignored with a deprecation message since 2.0.0
#'   (`color_type`): the text channel always uses the text palette, and the colour CHANNEL is chosen
#'   by `color = c(text, background)` (see [tab()]).
#' @param format Which output the stylesheet is for, in [tab_export()]'s own vocabulary.
#'   `"html"` (the default) is the full stylesheet [tab_html()] needs: the colour classes **and**
#'   the table's own look (font, background, border colours, the greys). `"md"` emits the colour
#'   classes only, which is what [tab_md()] wants --- bare selectors you can map in your own editor's
#'   or publisher's CSS.
#' @param style_tag Wrap the CSS in a `<style>` tag (default `TRUE`).
#' @param file Optional path to write to instead of returning.
#'
#' @eval print_palettes_rd()
#'
#' @return The CSS, invisibly when `file` is given. Printed as-is by `knitr` with `results = "asis"`.
#' @seealso [tab_html()], [tab_md()], [set_color_palette()], [set_color_breaks()]
#' @export
#' @examples
#' cat(tab_css(theme = "auto"))
#' cat(tab_css(format = "md", style_tag = FALSE))  # the markdown flavour
tab_css <- function(theme = NULL, format = c("html", "md"),
                    style_tag = TRUE, file = NULL, print_rules = NULL, ...) {
  # `chrome` is caught by NAME here rather than left to tx_deprecate_inert() (which would accept and
  # ignore it): a swallowed `chrome = FALSE` would silently emit the wrong stylesheet.
  dots <- rlang::list2(...)
  if ("chrome" %in% names(dots))
    cli::cli_abort(c("{.arg chrome} is now {.arg format}, which names the output it is for.",
                     "i" = 'chrome = TRUE  ->  format = "html"  (the default)',
                     "i" = 'chrome = FALSE ->  format = "md"'))
  tx_deprecate_inert(dots[setdiff(names(dots), "chrome")], "tab_css")
  format <- rlang::arg_match(format)
  chrome <- identical(format, "html")
  o   <- resolve_export_opts(theme = theme, allow_auto = TRUE)
  # NULL -> option is why tab_html()/tab_md() need no argument of their own: a user with a colour
  # printer sets options(tabxplor.print_rules = FALSE) once for a whole document.
  if (is.null(print_rules)) print_rules <- tx_option("print_rules")
  fallback <- tx_print_rules_palette(print_rules)
  # the stylesheet carries exactly ONE publication palette: the page's own if it is one, otherwise the
  # one a coloured page falls back to on paper.
  prt <- if (tx_is_print(o$theme)) o$theme else fallback %||% "print_minimalistic"
  css <- tx_css_render(tx_css_rules(chrome = chrome, print_theme = prt), o$theme, chrome = chrome,
                       print_rules = !is.null(fallback))
  if (isTRUE(style_tag)) css <- paste0("<style>\n", css, "\n</style>")
  if (!is.null(file)) {
    writeLines(css, file)
    return(invisible(css))
  }
  css
}
