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
# See: CLAUDE.md Phase 13d + dev/tabxplor_phase10_exporters.md.

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
tx_chrome_hex <- function(theme = "light") {
  if (identical(tx_palette_theme(theme), "dark")) {
    list(text = "#FFFFFF", grey = "#707070", grey2 = "#EEEEEE",
         bg = "#111111", border = "#FFFFFF", hover = "rgba(255,255,255,.06)")
  } else {
    list(text = "#000000", grey = "#9f9f9f", grey2 = "#111111",
         bg = "#ffffff", border = "#000000", hover = "rgba(0,0,0,.045)")
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
    # Borders are emitted colourless by the engine, so they would inherit `currentColor` = the CELL's
    # palette hex (a +20% cell got a red border). Set it explicitly: the spec is one border colour.
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
  static <- if (isTRUE(chrome)) c(
    ".tabxplor-tab{border-collapse:collapse;border-top:0;border-bottom:0;margin:0;}",
    ".tabxplor-tab caption{text-align:center;font-weight:bold;font-size:120%;}",
    ".tabxplor-tab tfoot{font-size:80%;text-align:left;}"
  ) else character(0)

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
#' \preformatted{
#' ```{r, results = "asis"}
#' options(tabxplor.kable_css = FALSE)
#' tab_css(theme = "auto")
#' ```
#' }
#' Every later `tab_kable()` then emits classes only. Two things to know: with `css = FALSE` and **no**
#' `tab_css()` call the tables render uncoloured; and one stylesheet means one `theme` and one
#' `color_type` for the whole document (a per-table `color_type` would need its own `css = TRUE`).
#'
#' @param theme `"light"`, `"dark"`, or `"auto"` to follow the reader's colour scheme (their operating
#'   system, and any dark-mode toggle of the host page: Quarto, Bootstrap 5.3, Tailwind). Defaults to
#'   `getOption("tabxplor.theme")` (`"light"`).
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
