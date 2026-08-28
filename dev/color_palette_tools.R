# PURPOSE: Colour-palette DESIGN tooling (not part of the shipped package).
# ROLE: Interactive helpers used to design and review the tabxplor OKLCH palettes
#   (Phase 13a). They render text x background combinations in the Viewer, show APCA
#   contrast, and simulate colour-vision deficiency. Nothing in R/ calls them.
# USAGE: devtools::load_all(); source("dev/color_palette_tools.R")
#   preview_color_grid()      - every text x background combination of two vectors
#   preview_luminance_grid()  - luminance shades of one text/background pair
#   plot_oklch_hue_strip_cvd() - hue strip + CVD simulation, base graphics
#   simulate_cvd_farver()     - Machado et al. (2009) CVD simulation of hex colours
# KEY CONSTRAINTS:
#   - Lives in dev/ (.Rbuildignore'd) ON PURPOSE: it is the sole reason the package
#     would otherwise depend on farver + colorspace, and it pulls in base graphics.
#   - This file's maths is about the TABLE palette (COLOR_RAMPS). The maths for the SITE
#     theme is txtheme's exported API; `.cg_apca()` duplicates txtheme::apca() on purpose,
#     because txtheme has no Imports and this file needs farver. Same Myndex reference vector.
#     Do NOT move it back into R/ -- see CLAUDE.md and the design notes at the end of
#     R/tab_classes.R.
#   - Depends on: farver, colorspace, knitr, kableExtra (none declared in DESCRIPTION).
#   - The palettes themselves live in R/tab_classes.R (default_*_colors / build_palettes).
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# colour_grid_preview.R
#
# Visual review of colour combinations in the Positron Viewer pane.
#
#   preview_color_grid()    - every text x background combination of two vectors
#   preview_luminance_grid() - luminance shades of one text/background pair
#
# Both render exactly like tab_kable(): a knitr::kable() + kableExtra::kable_classic()
# table whose cells are kableExtra::cell_spec() tiles (rounded background via
# background_as_tile, the same background "shape" tab_kable() draws). Each cell
# shows the sample text plus its APCA lightness-contrast value (Lc) of
# text-on-background, so you can eyeball legibility at a glance.
#
# The table backdrop is configurable via `table_bg` (both preview functions).
# When its oklch lightness drops below `dark_threshold`, the table auto-switches
# to dark styling: white + slightly thicker borders, light text for all the
# non-tile chrome (labels, caption, footnote), and a transparent table frame so
# the dark page shows through behind the coloured tiles.
#
# Dependencies: farver (oklch + rgb maths) + knitr + kableExtra (the tab_kable
# engine). Viewer routing reuses print.kableExtra (getOption("viewer") hook),
# the same route tab_kable() output takes - no pandoc needed.
# ---------------------------------------------------------------------------

# tab_kable() default HTML font stack.
.cg_font <- '"DejaVu Sans", "Arial", arial, helvetica, sans-serif'

# --- internal: dependency guard --------------------------------------------

#' Stop early with a clear message if a rendering dependency is missing.
#' @noRd
.cg_require <- function() {
  if (!requireNamespace("farver", quietly = TRUE)) {
    stop("Package 'farver' is required for oklch handling.", call. = FALSE)
  }
  # The grids now render as tab_kable()-style tables (same engine + display).
  for (pkg in c("knitr", "kableExtra")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required to render the colour grid.", pkg),
           call. = FALSE)
    }
  }
}

# --- internal: oklch / gamut maths -----------------------------------------

#' Decode one colour to its oklch (l, c, h) with clean, predictable names.
#'
#' farver may attach column names that get mangled by name-concatenation when
#' extracting single elements, so we strip names and reassign them explicitly.
#' @noRd
.cg_oklch <- function(hex) {
  m <- farver::decode_colour(hex, to = "oklch")
  stats::setNames(as.numeric(m[1, ]), c("l", "c", "h"))
}

#' Is oklch(l, c, h) inside the sRGB gamut?
#'
#' farver caps RGB output to [0, 255], so an out-of-gamut colour never returns
#' impossible RGB values - instead it loses chroma on a round-trip
#' oklch -> rgb -> oklch. We detect that chroma drop.
#' @noRd
.cg_in_gamut <- function(l, c, h, tol = 1e-3) {
  lch  <- matrix(c(l, c, h), ncol = 3)
  rgb  <- farver::convert_colour(lch, from = "oklch", to = "rgb")
  back <- farver::convert_colour(rgb, from = "rgb", to = "oklch")
  abs(back[1, 2] - c) <= tol
}

#' Largest in-gamut chroma for a given lightness/hue.
#'
#' The in-gamut chroma range is a single interval [0, cmax], so a bisection on
#' "is this chroma still in gamut?" converges on cmax.
#' @noRd
.cg_max_chroma <- function(l, h, hi = 0.4, iter = 28L) {
  lo <- 0
  for (i in seq_len(iter)) {
    mid <- (lo + hi) / 2
    if (.cg_in_gamut(l, mid, h)) lo <- mid else hi <- mid
  }
  lo
}

#' Build a hex colour at lightness `l`, keeping hue `h`; chroma set by `mode`.
#'
#' "fixed" keeps the source chroma but caps it to the gamut (so hue is never
#' distorted by RGB clipping); "max" uses the most vivid in-gamut chroma.
#' @noRd
.cg_shade <- function(l, h, base_c, mode) {
  if (base_c < 1e-4) {                    # achromatic source -> stay grey
    cc <- 0
  } else {
    cmax <- .cg_max_chroma(l, h)
    cc <- if (mode == "max") cmax else min(base_c, cmax)
  }
  farver::encode_colour(matrix(c(l, cc, h), ncol = 3), from = "oklch")
}

#' Pick black or white text for legibility over a background hex.
#' @noRd
.cg_readable_on <- function(bg_hex) {
  l <- farver::decode_colour(bg_hex, to = "oklch")[1, 1]
  if (l >= 0.6) "#000000" else "#ffffff"
}

# --- internal: theme (light / dark) ----------------------------------------

#' Resolve a light/dark theme from the table backdrop colour.
#'
#' `dark` triggers when the backdrop's oklch lightness is below `threshold`.
#' Dark mode uses white, slightly thicker borders and light "ink" for all the
#' non-tile chrome; light mode keeps the current lightable-classic appearance.
#' @noRd
.cg_theme <- function(table_bg, threshold = 0.5) {
  l    <- farver::decode_colour(table_bg, to = "oklch")[1, 1]
  dark <- isTRUE(l < threshold)
  list(
    dark          = dark,
    bg            = table_bg,
    ink           = if (dark) "#e8e8e8" else "#222222",  # non-tile text
    border        = if (dark) "#ffffff" else "#d9d9d9",  # frame + row rules
    border_w      = if (dark) "2px"     else "1px",      # a bit more linewidth
    square_border = if (dark) "#cfcfcf" else "#999999"   # row-label swatch square
  )
}

#' CSS injected AFTER the kable (so it wins over lightable's inline <style> at
#' equal specificity). Only border rules use !important; text-colour rules stay
#' non-important so inline cell_spec() tile colours still win.
#' @noRd
.cg_theme_css <- function(t) {
  css <- sprintf("body{background-color:%s;margin:0;padding:16px;}", t$bg)
  if (isTRUE(t$dark)) {
    css <- paste0(
      css,
      # let the dark page show through the frame; tiles keep their own fill
      ".lightable-classic,.lightable-classic thead,.lightable-classic tbody,",
      ".lightable-classic tr,.lightable-classic td,.lightable-classic th,",
      ".lightable-classic caption,.lightable-classic tfoot{",
      "background-color:transparent !important;}",
      # light ink for labels / corner / caption / footnote (tiles override inline)
      sprintf(".lightable-classic,.lightable-classic td,.lightable-classic th{color:%s;}",
              t$ink),
      sprintf(".lightable-classic caption{color:%s;}", t$ink),
      sprintf(".lightable-classic tfoot{color:%s;}", t$ink),
      # white, slightly thicker borders
      sprintf(".lightable-classic td,.lightable-classic th{border-color:%s !important;}",
              t$border),
      sprintf(paste0(".lightable-classic>tbody>tr>td,.lightable-classic>thead>tr>th",
                     "{border-bottom-width:%s !important;border-bottom-color:%s !important;}"),
              t$border_w, t$border)
    )
  }
  css
}

# --- internal: APCA contrast (APCA-W3 0.98G / 0.1.x constants) --------------

#' sRGB (0-255 triplet) to APCA screen luminance Y.
#' @noRd
.cg_srgb_to_y <- function(rgb) {
  lin <- (rgb / 255)^2.4                  # simple 2.4 TRC, per APCA-W3
  0.2126729 * lin[1] + 0.7151522 * lin[2] + 0.0721750 * lin[3]
}

#' APCA lightness contrast (Lc) of text-on-background, signed float.
#'
#' Positive => dark text on light background; negative => the reverse. Verified
#' against Myndex reference vectors (e.g. #888 on #fff -> ~63.06).
#' @noRd
.cg_apca <- function(text_hex, bg_hex) {
  txt <- as.numeric(farver::decode_colour(text_hex, to = "rgb"))
  bg  <- as.numeric(farver::decode_colour(bg_hex,  to = "rgb"))
  txt_y <- .cg_srgb_to_y(txt)
  bg_y  <- .cg_srgb_to_y(bg)

  soft_clamp <- function(y) if (y > 0.022) y else y + (0.022 - y)^1.414
  txt_y <- soft_clamp(txt_y)
  bg_y  <- soft_clamp(bg_y)

  if (abs(bg_y - txt_y) < 0.0005) return(0)

  if (bg_y > txt_y) {                     # BoW: dark text on light background
    sapc <- (bg_y^0.56 - txt_y^0.57) * 1.14
    if (sapc < 0.1) 0 else (sapc - 0.027) * 100
  } else {                                # WoB: light text on dark background
    sapc <- (bg_y^0.65 - txt_y^0.62) * 1.14
    if (sapc > -0.1) 0 else (sapc + 0.027) * 100
  }
}

# --- internal: HTML assembly + Viewer routing ------------------------------

#' Minimal HTML escaping for user-supplied sample text.
#' @noRd
.cg_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Build the swatch grid as a tab_kable()-style table and open the Viewer.
#'
#' Renders exactly like [tabxplor::tab_kable()]: each body cell is a
#' `kableExtra::cell_spec()` tile (rounded background via `background_as_tile`,
#' the same background "shape" tab_kable() draws), coloured `text` on `bg`, laid
#' out by `knitr::kable()` + `kableExtra::kable_classic()` with the tab_kable
#' DejaVu font stack. Column headers are tinted with the backdrop colour (a live
#' preview); row headers carry a small square of the text colour. `caption`
#' becomes the table caption, `subtitle` a footnote. The page/table backdrop is
#' `table_bg`; below `dark_threshold` oklch lightness the table auto-switches to
#' dark styling (white thicker borders + light chrome). Opening in the Viewer
#' reuses the `print.kableExtra` path (same dependencies + `getOption("viewer")`
#' hook tab_kable() uses), so no pandoc is needed.
#'
#' @param text_hex,bg_hex Character matrices [n_row x n_col] of cell colours.
#' @param row_swatch,col_swatch Hex used to tint the row squares / column headers.
#' @param table_bg Backdrop colour of the whole table/page.
#' @param dark_threshold oklch lightness of `table_bg` below which dark styling kicks in.
#' @noRd
.cg_kable_grid <- function(text_hex, bg_hex, row_labels, col_labels,
                           row_swatch, col_swatch, corner, sample_text,
                           show_contrast, swatch_padding, caption, subtitle,
                           font_size, table_bg = "#ffffff", dark_threshold = 0.5,
                           full_width = FALSE, browse = TRUE) {
  n_row <- nrow(text_hex)
  n_col <- ncol(text_hex)
  theme <- .cg_theme(table_bg, dark_threshold)
  sample_html <- .cg_escape(sample_text)
  tile_css <- paste0("display:inline-block;padding:", swatch_padding, ";")

  # body: one kableExtra tile per cell (the same engine + rounded-tile shape as
  # tab_kable()'s coloured-background branch, cell_spec(background = )).
  body <- outer(seq_len(n_row), seq_len(n_col), Vectorize(function(i, j) {
    lc_txt <- if (isTRUE(show_contrast)) {
      lc <- .cg_apca(text_hex[i, j], bg_hex[i, j])
      sprintf(' <span style="font-weight:400;opacity:.75;font-size:90%%;">(%d)</span>',
              as.integer(round(abs(lc))))
    } else ""
    kableExtra::cell_spec(paste0(sample_html, lc_txt), format = "html", escape = FALSE,
                          bold = TRUE, color = text_hex[i, j], background = bg_hex[i, j],
                          extra_css = tile_css)
  }))
body <- matrix(unlist(body), nrow = n_row, ncol = n_col)

  # row header: small text-colour square + label (always visible, even near-white).
  # Square border follows the theme so it stays visible on a dark backdrop too.
  rlab <- vapply(seq_len(n_row), function(i) sprintf(
    paste0('<span style="display:inline-block;width:12px;height:12px;',
           'border:1px solid %s;background-color:%s;vertical-align:middle;',
           'margin-right:6px;"></span>%s'),
    theme$square_border, row_swatch[i], row_labels[i]
  ), character(1))

  # column headers tinted with the backdrop colour (a live preview of the fill)
  col_head <- vapply(seq_len(n_col), function(j) kableExtra::cell_spec(
    col_labels[j], format = "html", escape = FALSE, bold = TRUE,
    color = .cg_readable_on(col_swatch[j]), background = col_swatch[j]
  ), character(1))

  df <- data.frame(rlab, body, check.names = FALSE, stringsAsFactors = FALSE)

  # font_size arrives as a CSS length ("16px"); kable_styling wants a bare number.
  fs <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(font_size))))
  if (length(fs) != 1L || is.na(fs)) fs <- NULL

  # header underline: theme-driven (white + thicker in dark mode, else as before)
  header_rule <- if (theme$dark) {
    sprintf("border-bottom:%s solid %s;", theme$border_w, theme$border)
  } else {
    "border-bottom:1px solid;"
  }

  out <- knitr::kable(
    df, format = "html", escape = FALSE,
    align = c("l", rep("c", n_col)),
    col.names = c(corner, col_head), caption = caption
  ) |>
    kableExtra::kable_classic(
      lightable_options = "hover", full_width = full_width,
      html_font = .cg_font, font_size = fs
    ) |>
    kableExtra::row_spec(
      0, bold = TRUE,
      extra_css = paste0(header_rule, "vertical-align:bottom;text-align:center;")
    ) |>
    kableExtra::column_spec(1, extra_css = "white-space:nowrap;")

  if (!is.null(subtitle) && any(nzchar(subtitle))) {
    out <- kableExtra::add_footnote(out, subtitle, notation = "none", escape = FALSE)
  }

  # Append the theme CSS AFTER lightable's inline <style> so it wins at equal
  # specificity; restore the kableExtra attributes paste0() strips.
  attrs <- attributes(out)
  out <- paste0(out, "\n<style>", .cg_theme_css(theme), "</style>\n")
  attributes(out) <- attrs

  # Reuse print.kableExtra: opens in the Viewer in interactive sessions (the same
  # route tab_kable() output takes), cat()s the HTML otherwise.
  if (isTRUE(browse)) print(out)
  invisible(out)
}

# max in-gamut oklch chroma for a given L + hue.
# farver clamps rgb to [0,255], so an out-of-gamut chroma shrinks on a
# oklch -> rgb -> oklch round-trip; bisect on that.
#' @noRd
max_chroma <- function(l, h, hi = 0.4, iter = 28L) {
  lo <- 0
  for (i in seq_len(iter)) {
    mid  <- (lo + hi) / 2
    lch  <- matrix(c(l, mid, h), ncol = 3)
    back <- farver::convert_colour(
      farver::convert_colour(lch, "oklch", "rgb"), "rgb", "oklch"
    )
    if (abs(back[1, 2] - mid) <= 1e-3) lo <- mid else hi <- mid
  }
  lo
}

# set luminance (scalar or one-per-colour), keep hue, cap chroma to gamut
#' @noRd
set_luminance <- function(cols, l = 0.95) {
  lch <- farver::decode_colour(cols, to = "oklch")   # cols: l, c, h
  l   <- rep_len(l, nrow(lch))
  h   <- lch[, 3]; h[is.na(h)] <- 0                  # achromatic -> hue 0
  cc  <- vapply(seq_len(nrow(lch)), function(i) {
    if (lch[i, 2] < 1e-4) 0                           # keep greys grey
    else min(lch[i, 2], max_chroma(l[i], h[i]))       # keep chroma, cap to gamut
  }, numeric(1))
  farver::encode_colour(cbind(l, cc, h), from = "oklch") |>
    setNames(names(cols))
}

# The Excel/plot legend fallback: shift luminance DOWN by `by`, scale chroma by `chroma_boost`, keep
# hue, cap chroma to gamut.
# An Excel rich-text run (and a ggpubr text label) carries a font colour but no fill, so a
# background-channel break-word in the colour legend must be drawn as TEXT -- and the background
# palette is far too light to read on a white sheet. Baked into R/tab_classes.R as
# default_bg_legend_colors / _neg (light only; the dark background palette is already dark enough to
# read on white, and darkening it collapses the whole ladder to black). Recipe:
#   darken_for_legend(default_background_colors)      -> default_bg_legend_colors
#   darken_for_legend(default_background_colors_neg)  -> default_bg_legend_colors_neg
#
# DESIGN (Phase 14l): TWO levers, because they fix two different complaints and neither substitutes
# for the other. MEASURED against the APCA bar in R/tab_classes.R's design notes (Lc >= 60 for
# larger/heavier text -- a legend break-word is bold):
#   * `by` (lightness) is what fixes FAINTNESS. APCA Lc is driven by lightness almost alone.
#   * `chroma_boost` is what fixes GREYNESS, and moves Lc essentially not at all:
#       by=0.2 k=1 -> Lc 39.6 45.4 50.4 59.9   (the original bake; 3 of 4 slots below the bar)
#       by=0.2 k=3 -> Lc 38.3 44.7 50.5 60.8   <- 3x the chroma, Lc UNCHANGED
#       by=0.3 k=2 -> Lc 55.3 60.8 65.4 74.4   <- shipped
# So do not reach for chroma to fix a faint palette; deepen `by` first.
# WARNING: the gamut ceiling makes a big `chroma_boost` self-defeating. At by=0.2 the max useful k per
# slot is 4.4 / 4.7 / 3.5 / 2.5 -- above ~2.5 the strong slots cap out while the faint ones keep
# rising, COMPRESSING the ladder into a pure-lightness ramp and destroying the chroma proportions the
# palette inherits from the fills. Deepening `by` raises the ceiling, which is the other reason
# by=0.3/k=2 wins: it is fully in-gamut on all 8 slots, so the proportions survive exactly.
#' @noRd
darken_for_legend <- function(cols, by = 0.30, chroma_boost = 2) {
  lch <- farver::decode_colour(cols, to = "oklch")
  l   <- pmax(lch[, 1] - by, 0)
  h   <- lch[, 3]; h[is.na(h)] <- 0                   # achromatic -> hue 0
  cc  <- vapply(seq_along(l), function(i) {
    if (lch[i, 2] < 1e-4) 0                                        # keep greys grey
    else min(lch[i, 2] * chroma_boost, max_chroma(l[i], h[i]))     # boost, cap to gamut
  }, numeric(1))
  unname(farver::encode_colour(cbind(l, cc, h), from = "oklch"))
}

# set chroma (scalar or one-per-colour), keep hue + luminance, cap to gamut
#' @noRd
set_chroma <- function(cols, c = 0.1) {
  lch <- farver::decode_colour(cols, to = "oklch")   # cols: l, c, h
  c   <- rep_len(c, nrow(lch))
  l   <- lch[, 1]
  h   <- lch[, 3]; h[is.na(h)] <- 0                  # grey has no hue -> 0
  cc  <- vapply(seq_len(nrow(lch)), function(i)
    min(c[i], max_chroma(l[i], h[i])),               # requested chroma, capped
    numeric(1)
  )
  farver::encode_colour(cbind(l, cc, h), from = "oklch") |>
    setNames(names(cols))
}



## --- all text x background combinations -----------------------

#' Preview every text x background colour combination in the Viewer
#'
#' Builds a [tabxplor::tab_kable()]-style table with one row per text colour and
#' one column per background colour (cells are `kableExtra::cell_spec()` tiles,
#' the same rounded-background shape tab_kable() draws), then opens it in the
#' Positron Viewer pane.
#'
#' @param text_colors A (named) character vector of hex text colours. Names
#'   become row labels; the hex value is used when unnamed.
#' @param background_colors A (named) character vector of hex background
#'   colours, used as columns.
#' @param sample_text Text shown in each cell. Default: a random whole-number
#'   percentage (e.g. "27%"), one value per call.
#' @param show_contrast Logical; append the APCA lightness-contrast value (Lc)
#'   of text-on-background to each cell, on the same line as sample_text.
#'   Default TRUE.
#' @param table_bg Backdrop colour of the whole table/page, e.g. "#1a1a1a" to
#'   preview dark mode. Default "#ffffff".
#' @param dark_threshold oklch lightness (0-1) of `table_bg` below which the
#'   table switches to dark styling (white + slightly thicker borders, light
#'   text, transparent frame). Default 0.5.
#' @param font_size CSS font-size for the table. Default "14px".
#' @param swatch_padding CSS padding for each swatch cell. Default "1px 1px".
#' @param browse Logical; open the result in the Viewer. Default TRUE.
#'
#' @return (Invisibly) the generated HTML as a single string.
#' @examples
#' \dontrun{
#' text_colors <- c(plain = "#888888", pos3 = "#0baedb", pos5 = "#265aff")
#' background_colors <- c(plain = "#ffffff", pos3 = "#91b837", pos5 = "#05ae30")
#' preview_color_grid(text_colors, background_colors)
#' preview_color_grid(text_colors, background_colors, table_bg = "#1a1a1a")  # dark
#' }
#' @keywords internal
preview_color_grid <- function(text_colors,
                                background_colors,
                                sample_text = paste0(sample(0:100, 1L), "%"),
                                show_contrast = TRUE,
                                table_bg = "#ffffff",
                                dark_threshold = 0.5,
                                font_size = "14px",
                                swatch_padding = "2px 1px",
                                browse = TRUE) {
  .cg_require()
  stopifnot(length(text_colors) >= 1, length(background_colors) >= 1,
            length(table_bg) == 1)

  row_labels <- names(text_colors)
  if (is.null(row_labels)) row_labels <- unname(text_colors)
  col_labels <- names(background_colors)
  if (is.null(col_labels)) col_labels <- unname(background_colors)

  n_row <- length(text_colors)
  n_col <- length(background_colors)

  # row = text colour (constant across a row); col = background (constant down a column)
  text_hex <- matrix(rep(unname(text_colors), times = n_col), nrow = n_row)
  bg_hex   <- matrix(rep(unname(background_colors), each = n_row), nrow = n_row)

  .cg_kable_grid(
    text_hex, bg_hex,
    row_labels = row_labels, col_labels = col_labels,
    row_swatch = unname(text_colors),
    col_swatch = unname(background_colors),
    corner = "text \u2193 / bg \u2192",
    sample_text = sample_text, show_contrast = show_contrast,
    swatch_padding = swatch_padding,
    caption  = "Text \u00d7 background colour grid",
    subtitle = sprintf("%d text colours \u00d7 %d backgrounds \u2014 cells show APCA Lc",
                       n_row, n_col),
    font_size = font_size, table_bg = table_bg, dark_threshold = dark_threshold,
    browse = browse
  )
}

## luminance shades of one pair -----------------------------

#' Preview luminance shades of one text/background pair in the Viewer
#'
#' For a single text colour and background colour, builds a grid of luminance
#' shades: rows vary the text colour's lightness, columns vary the background's
#' lightness. Every shade keeps its source oklch hue; chroma is either held at
#' the source value (capped to gamut) or pushed to the maximum available at that
#' lightness/hue. Rendered as a [tabxplor::tab_kable()]-style table (rounded
#' `cell_spec()` background tiles) and opened in the Positron Viewer.
#'
#' @param text_color Single hex string for the text colour.
#' @param background_color Single hex string for the background colour.
#' @param l_values Numeric oklch lightness values (0-1) for the shades.
#'   Default seq(0.35, 0.95, length.out = 7).
#' @param chroma "fixed" (keep source chroma, capped to gamut) or "max"
#'   (maximum in-gamut chroma per shade). Default "fixed".
#' @param table_bg Backdrop colour of the whole table/page, e.g. "#1a1a1a" to
#'   preview dark mode. Default "#ffffff".
#' @param dark_threshold oklch lightness (0-1) of `table_bg` below which the
#'   table switches to dark styling. Default 0.5.
#' @param sample_text,show_contrast,font_size,swatch_padding,browse See
#'   [preview_color_grid()].
#'
#' @return (Invisibly) the generated HTML as a single string.
#' @examples
#' \dontrun{
#' preview_luminance_grid("#59c5bf", "#b9c653")                 # fixed chroma
#' preview_luminance_grid("#59c5bf", "#b9c653", chroma = "max") # most vivid
#' preview_luminance_grid("#59c5bf", "#b9c653", table_bg = "#1a1a1a")  # dark
#' }
#' @keywords internal
preview_luminance_grid <- function(text_color,
                                   background_color,
                                   l_values = seq(0.35, 0.95, length.out = 7),
                                   chroma = c("fixed", "max"),
                                   sample_text = paste0(sample(0:100, 1L), "%"),
                                   show_contrast = TRUE,
                                   table_bg = "#ffffff",
                                   dark_threshold = 0.5,
                                   font_size = "16px",
                                   swatch_padding = "12px 16px",
                                   browse = TRUE) {
  .cg_require()
  chroma <- match.arg(chroma)
  stopifnot(length(text_color) == 1, length(background_color) == 1,
            length(table_bg) == 1)

  txt_lch <- .cg_oklch(text_color)
  bg_lch  <- .cg_oklch(background_color)

  # shade ramps: row = text lightness, col = background lightness
  txt_shades <- vapply(l_values, function(l)
    .cg_shade(l, txt_lch[["h"]], txt_lch[["c"]], chroma), character(1))
  bg_shades  <- vapply(l_values, function(l)
    .cg_shade(l, bg_lch[["h"]],  bg_lch[["c"]],  chroma), character(1))

  n <- length(l_values)
  text_hex <- matrix(rep(txt_shades, times = n), nrow = n)  # constant per row
  bg_hex   <- matrix(rep(bg_shades,  each  = n), nrow = n)  # constant per col

  lab <- sprintf("L=%.2f", l_values)
  .cg_kable_grid(
    text_hex, bg_hex,
    row_labels = lab, col_labels = lab,
    row_swatch = txt_shades, col_swatch = bg_shades,
    corner = "text \u2193 / bg \u2192",
    sample_text = sample_text, show_contrast = show_contrast,
    swatch_padding = swatch_padding,
    caption  = sprintf("Luminance shades \u2014 chroma: %s (cells show APCA Lc)", chroma),
    subtitle = sprintf(
      "text %s (hue %.0f\u00b0) \u00d7 background %s (hue %.0f\u00b0)",
      toupper(text_color), txt_lch[["h"]],
      toupper(background_color), bg_lch[["h"]]
    ),
    font_size = font_size, table_bg = table_bg, dark_threshold = dark_threshold,
    browse = browse
  )
}

#' @keywords internal
lcd_simulate_oklch <- function(
  colours,
  chroma_scale      = 0.60,  # how much to reduce chroma (0–1)
  lightness_center  = 0.50,  # where lightness is “anchored” (0–1 scale)
  lightness_compress = 0.90  # how much to compress lightness range (0–1)
) {
  # Decode hex (or R color names) directly to OKLCH
  oklch <- farver::decode_colour(colours, to = "oklch")
  L <- oklch[, 1]
  C <- oklch[, 2]
  H <- oklch[, 3]

  # Reduce chroma to simulate less vivid LCD color reproduction
  C_new <- C * chroma_scale
  C_new <- pmax(C_new, 0)       # no negative chroma

  # Compress lightness toward a center value
  # L is on 0–1 scale inside farver’s OKLCH representation
  L_new <- lightness_center + lightness_compress * (L - lightness_center)

  # Reassemble modified OKLCH
  oklch_new <- cbind(L_new, C_new, H)

  # Convert back to sRGB and clamp to valid range for encoding
  rgb_new <- farver::convert_colour(oklch_new, from = "oklch", to = "rgb")
  rgb_new <- pmin(pmax(rgb_new, 0), 255)

  # Encode back to hex
  farver::encode_colour(rgb_new, from = "rgb")
}

#' Simulate color vision deficiency for hex colors using farver + colorspace
#'
#' @param col Character vector of hex colors (e.g. "#03ab86").
#' @param type Type of CVD to simulate: "deutan" (green cone defective)
#'   or "protan" (red cone defective). These are the two most common
#'   congenital red–green deficiencies.
#' @param severity Numeric in [0, 1], Machado-style severity parameter
#'   (0 = normal vision, 1 = full dichromacy). Values around 1 correspond
#'   to deuteranopia/protanopia; values in (0, 1) emulate anomalous
#'   trichromacy.
#'
#' @return Character vector of hex colors representing how a trichromatic,
#'   color-normal viewer would see your input colors if they had the
#'   specified color vision deficiency.
#'
#' @details
#' The implementation follows the physiologically-based model of
#' Machado et al. (2009), using the RGB transform matrices provided by
#' colorspace::deutanomaly_cvd and colorspace::protanomaly_cvd
#' (interpolated by severity).
#'
#' Gamma-corrected sRGB is linearised, transformed in RGB, and then
#' re-gamma-corrected. Conversion between hex and RGB is handled by
#' farver::decode_colour() and farver::encode_colour().
#'
#' You can always inspect or design your palette in OKLCH using
#' farver::decode_colour(col, to = "oklch") before or after simulation;
#' the CVD model itself, however, operates in sRGB.
#'
#' @keywords internal
simulate_cvd_farver <- function(col,
                                type = c("deutan", "protan"),
                                severity = 1) {
  # Dependencies:
  # farver    >= 2.1.0  (for decode_colour / encode_colour)
  # colorspace >= 2.1.0 (for Machado CVD matrices)
  type <- match.arg(type)

  if (!requireNamespace("farver", quietly = TRUE)) {
    stop("Package 'farver' is required but not installed.")
  }
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required but not installed.")
  }

  # 1. Pick the appropriate list of CVD transform matrices
  #    from colorspace (Machado 2009 implementation).
  transform_list <- switch(
    type,
    deutan = colorspace::deutanomaly_cvd,
    protan = colorspace::protanomaly_cvd
  )

  # Interpolate matrix for given severity in [0, 1]
  M <- colorspace::interpolate_cvd_transform(transform_list,
                                             severity = severity)

  # 2. Decode hex to sRGB (0–255) using farver.
  rgb_255 <- farver::decode_colour(col, to = "rgb")

  # Normalise to 0–1
  rgb <- rgb_255 / 255

  # 3. Convert sRGB to linear RGB (per IEC 61966-2-1).
  srgb_to_linear <- function(x) {
    ifelse(x <= 0.04045,
           x / 12.92,
           ((x + 0.055) / 1.055) ^ 2.4)
  }
  rgb_lin <- srgb_to_linear(rgb)

  # 4. Apply 3×3 CVD transform matrix in linear RGB.
  # rgb_lin is n × 3; we want n × 3 back, so multiply by t(M).
  rgb_lin_sim <- as.matrix(rgb_lin) %*% t(M)

  # 5. Convert linear RGB back to gamma-corrected sRGB.
  linear_to_srgb <- function(x) {
    ifelse(x <= 0.0031308,
           12.92 * x,
           1.055 * (x ^ (1 / 2.4)) - 0.055)
  }
  rgb_sim <- linear_to_srgb(rgb_lin_sim)

  # Clamp to [0, 1] and scale to 0–255
  rgb_sim_clamped <- pmin(pmax(rgb_sim, 0), 1)
  rgb_sim_255 <- round(rgb_sim_clamped * 255)

  # 6. Encode back to hex using farver.
  col_sim <- farver::encode_colour(rgb_sim_255, from = "rgb")

  col_sim
}

plot_oklch_hue_strip_cvd <- function(
  L = 0.72,
  n = 360,
  type = c("deutan", "protan", "tritan"),
  severity = 1,
  h_range = c(0, 360),
  chroma_mode = c("fixed", "relative_max", "max"),
  C = 0.12,
  C_prop = 0.6,
  bg = "#ffffff",
  border = NA,
  main = NULL
) {
  type <- match.arg(type)
  chroma_mode <- match.arg(chroma_mode)

  if (!requireNamespace("farver", quietly = TRUE)) {
    stop("Package 'farver' is required.")
  }
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required.")
  }

  h <- seq(h_range[1], h_range[2], length.out = n)

  srgb_to_linear <- function(x) {
    ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)
  }

  linear_to_srgb <- function(x) {
    ifelse(x <= 0.0031308, 12.92 * x, 1.055 * (x^(1 / 2.4)) - 0.055)
  }

  simulate_hex_cvd <- function(col, type = "deutan", severity = 1) {
    transform_list <- switch(
      type,
      deutan = colorspace::deutanomaly_cvd,
      protan = colorspace::protanomaly_cvd,
      tritan = colorspace::tritanomaly_cvd
    )

    M <- colorspace::interpolate_cvd_transform(transform_list, severity = severity)

    rgb_255 <- farver::decode_colour(col, to = "rgb")
    rgb <- rgb_255 / 255
    rgb_lin <- srgb_to_linear(rgb)
    rgb_lin_sim <- as.matrix(rgb_lin) %*% t(M)
    rgb_sim <- linear_to_srgb(rgb_lin_sim)
    rgb_sim <- pmin(pmax(rgb_sim, 0), 1)
    rgb_sim_255 <- round(rgb_sim * 255)

    farver::encode_colour(rgb_sim_255, from = "rgb")
  }

  max_chroma_for_hue <- function(L, h, tol = 1e-4, max_iter = 22) {
    lo <- 0
    hi <- 0.4

    is_in_gamut <- function(C) {
      rgb <- farver::convert_colour(cbind(L, C, h), from = "oklch", to = "rgb")
      all(is.finite(rgb)) && all(rgb >= 0) && all(rgb <= 255)
    }

    while (is_in_gamut(hi) && hi < 1.5) {
      hi <- hi * 1.5
    }

    for (i in seq_len(max_iter)) {
      mid <- (lo + hi) / 2
      if (is_in_gamut(mid)) {
        lo <- mid
      } else {
        hi <- mid
      }
      if ((hi - lo) < tol) break
    }

    lo
  }

  c_max <- vapply(h, function(hh) max_chroma_for_hue(L, hh), numeric(1))

  chroma <- switch(
    chroma_mode,
    fixed = pmin(C, c_max),
    relative_max = pmin(C_prop * c_max, c_max),
    max = c_max
  )

  oklch_mat <- cbind(L = rep(L, length(h)), C = chroma, H = h)
  rgb_mat <- farver::convert_colour(oklch_mat, from = "oklch", to = "rgb")
  rgb_mat <- pmin(pmax(round(rgb_mat), 0), 255)
  hex_normal <- farver::encode_colour(rgb_mat, from = "rgb")
  hex_cvd <- simulate_hex_cvd(hex_normal, type = type, severity = severity)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(mar = c(2.2, 3.2, 3.2, 1.2), xaxs = "i", yaxs = "i", bg = bg)

  plot.new()
  plot.window(xlim = c(min(h), max(h)), ylim = c(0, 2))

  xleft <- h[-length(h)]
  xright <- h[-1]

  rect(xleft, 1.05, xright, 1.95, col = hex_normal[-length(hex_normal)], border = border)
  rect(xleft, 0.05, xright, 0.95, col = hex_cvd[-length(hex_cvd)], border = border)

  axis(1, at = seq(0, 360, by = 60), labels = seq(0, 360, by = 60), line = -0.5)
  mtext("Hue angle h", side = 1, line = 1.2)

  text(min(h) + 0.01 * diff(range(h)), 1.5,
       labels = sprintf("Normal vision — L = %.2f, chroma_mode = %s", L, chroma_mode),
       adj = c(0, 0.5), cex = 0.9)

  text(min(h) + 0.01 * diff(range(h)), 0.5,
       labels = sprintf("%s simulation — severity = %.2f", type, severity),
       adj = c(0, 0.5), cex = 0.9)

  if (is.null(main)) {
    main <- sprintf("OKLCH hue strip and %s simulation", type)
  }
  title(main = main)

  invisible(list(
    hue = h,
    L = L,
    c_max = c_max,
    chroma = chroma,
    hex_normal = hex_normal,
    hex_cvd = hex_cvd
  ))
}



# ==================================================================================================
# THE PALETTE REVIEW RECIPE (moved out of R/tab_classes.R by Phase 19l -- ~100 lines of commented-out
# code sitting in the package source, which is exactly what the tools in THIS file exist for).
# Run it after changing a palette: it previews every slot in light and dark, then under simulated
# colour-vision deficiency and the bad-LCD approximation.
#   source("~/github/tabxplor/dev/color_palette_tools.R", encoding = "UTF-8")
# The palette objects it reads (default_text_colors, default_background_colors, ...) are internal to
# tabxplor, so load the package first (devtools::load_all()).
# ==================================================================================================

# ### Color palettes visual tests, with color blind mode ----
# source("~/github/tabxplor/dev/color_palette_tools.R", encoding = "UTF-8")
# # Light palette
# light_text_palette <- c(plain= "#9f9f9f", default_text_colors, default_text_colors_neg)
# light_bg_palette   <- c(plain= "#ffffff",default_background_colors, default_background_colors_neg)
# preview_color_grid(light_text_palette, light_bg_palette) # #show_contrast = FALSE  
# #    Lc ≥ 75 for body text ; ≥ 60 for larger/heavier text ; ≥ 45 for large headlines ; below ~30 is decorative-only.


# #   color blindness
# preview_color_grid(simulate_cvd_farver(light_text_palette, type = "deutan", severity = 1), 
#                    simulate_cvd_farver(light_bg_palette, type = "deutan", severity = 1),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )
# preview_color_grid(simulate_cvd_farver(light_text_palette, type = "deutan", severity = 0.5), 
#                    simulate_cvd_farver(light_bg_palette, type = "deutan", severity = 0.5),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )
# preview_color_grid(simulate_cvd_farver(light_text_palette, type = "protan"), 
#                    simulate_cvd_farver(light_bg_palette, type = "protan"),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )
# preview_color_grid(simulate_cvd_farver(light_text_palette, type = "protan", severity = 0.5), 
#                    simulate_cvd_farver(light_bg_palette, type = "protan", severity = 0.5),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )

# #   bad LCD approximation
# preview_color_grid(lcd_simulate_oklch(light_text_palette), 
#                    lcd_simulate_oklch(light_bg_palette),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )

# # default_text_colors |> farver::decode_colour(to = "oklch") # Inspect OKLCH coordinates




# #   color blindness
# preview_color_grid(simulate_cvd_farver(dark_text_palette, type = "deutan", severity = 0.5), 
#                    simulate_cvd_farver(dark_bg_palette, type = "deutan"),
#                    table_bg = lcd_simulate_oklch("#111111")
#                    )
# preview_color_grid(simulate_cvd_farver(dark_text_palette, type = "deutan"), 
#                    simulate_cvd_farver(dark_bg_palette, type = "deutan"),
#                    table_bg = lcd_simulate_oklch("#111111")
#                    )

# preview_color_grid(simulate_cvd_farver(dark_text_palette, type = "protan"), 
#                    simulate_cvd_farver(dark_bg_palette, type = "protan"),
#                    table_bg = lcd_simulate_oklch("#111111")
#                    )

# #   bad LCD approximation
# preview_color_grid(lcd_simulate_oklch(dark_text_palette), 
#                   lcd_simulate_oklch(dark_bg_palette),
#                   table_bg = lcd_simulate_oklch("#111111")
#                   )
                   

# # Simuler une palette normale et une palette color blind cote-à-cote
# plot_oklch_hue_strip_cvd(L = 0.65,type = "deutan", severity = 1, C=0.16) # chroma_mode = "max"
# plot_oklch_hue_strip_cvd(L = 0.65,type = "deutan", severity = 0.5, C=0.16) 
# plot_oklch_hue_strip_cvd(L = 0.65,type = "protan", severity = 1, C=0.16)
# plot_oklch_hue_strip_cvd(L = 0.65,type = "tritan", severity = 1, C=0.16)


# # preview_color_grid(diff_colors, set_luminance(background_colors, 0.99)) 
# # set_luminance(background_colors, 0.99) |> farver::get_channel("l", space = "oklch")
# # # set_luminance(background_colors, c(0.99, 0.90, 0.85, 0.80, 0.72))

# # preview_color_grid(diff_colors, set_luminance(background_colors2, 0.95)) 

# # preview_color_grid(diff_colors, set_luminance(diff_colors, 0.95)) 
# # preview_color_grid(diff_colors, set_luminance(diff_colors, 0.8) |> set_chroma(0.12)) 


# # preview_luminance_grid("#59c5bf", "#b9c653")                  # fixed source chroma, capped to gamut
# # preview_luminance_grid("#59c5bf", "#b9c653", chroma = "max")  # most vivid shade at each L
# # preview_luminance_grid("#0185e4", "#68b430", l_values = seq(0.40, 0.90, by = 0.10)) # custom lightness ramp
# # # Lc ≥ 75 for body text ; ≥ 60 for larger/heavier text ; ≥ 45 for large headlines ; below ~30 is decorative-only.







## Color functions ----


# PURPOSE: the render-time colour palettes (Phase 13a). Ten OKLCH base palettes -- eight being one per
# (light/dark theme x text/background channel x over-/under-represented side), plus the two Phase-14c
# bg_legend sides (the font stand-in for the fills, light only) -- each 4 hex codes
# (faint -> strong), position-based (no pos1..neg5 names, no ratio slot). They are composed into
# 8-element slot vectors (4 over + 4 under) and pre-built once into ANSI style functions (cli), stored
# in an internal env and only rebuilt by set_color_palette(). The engine indexes them by the
# integer slot from fmt_color_slots() (1:4 = over intensities, 5:8 = under). See dev/colors.md.
