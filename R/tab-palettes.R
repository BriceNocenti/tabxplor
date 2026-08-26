# PURPOSE: EVERY palette tabxplor draws with, in one place -- the colour ramps, the chrome, and the
#   black-and-white publication palettes -- plus the store that keys them by (family, theme).
# ROLE: a palette answers one question: "slot 3 of the text channel, in this theme -- what does it
#   look like?" Everything upstream is theme-blind (the engine computes SLOTS, integers 0-8) and
#   everything downstream just draws what it is handed, so this file is the ONE boundary where a
#   number becomes an appearance. Read through get_color_style() / tx_chrome_hex(); written, for the
#   colour ramps only, by set_color_palette().
# KEY CONSTRAINTS:
#   - A palette is HEX **and** FACE. A backend must never derive "is this bold" from "does this have
#     a colour hex": that collapses on a publication palette whose every slot is near-black. The
#     face is declared beside the hex, always.
#   - A publication palette CANNOT be derived from a colour one: desaturating the two direction ramps
#     collapses them onto the same greyscale, so the direction is not degraded but GONE. Hence the
#     curated grids below -- and hence set_color_palette() provably cannot alter publication output.
#   - The working principle, in one line: an ORDERED channel (a colour ramp, an ink ladder, an
#     emphasis ladder, a repeated mark) carries MAGNITUDE; a SELECTIVE channel (hue, underline vs
#     italic, or the cell's own +/- symbol) carries DIRECTION.
#   - A palette's grid IS its definition. A colour rung's hex and its oklch coordinate, a print
#     slot's ink, weight, slant, rule and mark, sit on ONE ROW -- because the question a maintainer
#     asks is "what does break 2 look like", not "where is the italic list". Adding a palette, or
#     re-tuning a ramp, is one row and nothing else.
#   - The `oklch` column is how the ramps were chosen (chroma peaks, lightness steps, colour-blind
#     safety) and the only way to re-tune them. No code reads it; never drop it.
# See: CLAUDE.md section "tabxplor architecture" (the colour system);
#      dev/colors.md, which holds the ladder derivations and the measurement the publication
#      palettes answer (the shipped ramps converted to CIE L*), neither of which a header can hold.

# === SECTION: the colour ramps -- 8-bit fallback, then the 24-bit OKLCH palettes ====================

## 24-BIT OKLCH PALETTES ----

# OKLCH Chroma Peaks
# - Blue            H265 / L45  ; H180 to 265
# - to Orange Red   H28 / L62   ; H90 to 28   (avoid true red ?)
#
# - Green           H142 / L86  ; H110 to H160
# - to Violet Red   H325 / L70  ; H285 to H25

# THE COLOUR RAMPS -- one row per rung of every ramp, so a ladder reads DOWN a column: four rungs of
# rising intensity, over and under the reference, per channel and theme.
#   channel  "text" the ink, "bg" the fill, "bg_legend" the bg hues darkened to read as TEXT -- a
#            legend break-word cannot be drawn with a fill in every medium (an Excel run carries a
#            font colour only) and the pale fills are invisible on white. Light only. Regenerate
#            with dev/color_palette_tools.R::darken_for_legend().
#   theme    "light" / "dark".
#   dir      "over" / "under" -- which side of the reference the cell falls on.
#   rung     1-4, faint to strong.
#   hex      what ships.
#   oklch    "L C H", the coordinate the hex was tuned at: the chroma peaks above, the lightness
#            steps and the colour-blind safety are all read off this column, and nothing else does.
COLOR_RAMPS <- tibble::tribble(
  ~channel,    ~theme,  ~dir,    ~rung, ~hex,      ~oklch,
  "text",      "light", "over",  1L,    "#02a5b3", "0.66 0.1124 205",   # better for colour blindness
  "text",      "light", "over",  2L,    "#0891c9", "0.62 0.13   235",
  "text",      "light", "over",  3L,    "#0267c7", "0.52 0.17   255",
  "text",      "light", "over",  4L,    "#300dfd", "0.47 0.30   270",
  "text",      "light", "under", 1L,    "#dca331", "0.75 0.1400  80",
  "text",      "light", "under", 2L,    "#de7c01", "0.68 0.1596  60",
  "text",      "light", "under", 3L,    "#dd5301", "0.62 0.1868  42",
  "text",      "light", "under", 4L,    "#d60103", "0.55 0.2253  29",

  "bg",        "light", "over",  1L,    "#dffcff", "0.97 0.0304 205",   # better for colour blindness
  "bg",        "light", "over",  2L,    "#d7efff", "0.94 0.0336 235",
  "bg",        "light", "over",  3L,    "#cee3ff", "0.91 0.0439 255",
  "bg",        "light", "over",  4L,    "#bbccff", "0.85 0.0733 270",
  "bg",        "light", "under", 1L,    "#fff4e1", "0.97 0.0271  80",
  "bg",        "light", "under", 2L,    "#ffe6d3", "0.94 0.0374  60",
  "bg",        "light", "under", 3L,    "#ffd7c8", "0.91 0.0488  42",
  "bg",        "light", "under", 4L,    "#ffbaaf", "0.85 0.082   29",

  "bg_legend", "light", "over",  1L,    "#67A1A7", "0.67 0.0611 204",
  "bg_legend", "light", "over",  2L,    "#6492B0", "0.64 0.0674 238",
  "bg_legend", "light", "over",  3L,    "#5E85B8", "0.61 0.0896 255",
  "bg_legend", "light", "over",  4L,    "#5169C7", "0.55 0.1481 270",
  "bg_legend", "light", "under", 1L,    "#A7936F", "0.67 0.0553  82",
  "bg_legend", "light", "under", 2L,    "#AE815E", "0.64 0.0741  59",
  "bg_legend", "light", "under", 3L,    "#B56E53", "0.61 0.0989  41",
  "bg_legend", "light", "under", 4L,    "#BE4034", "0.55 0.1639  29",

  "text",      "dark",  "over",  1L,    "#028282", "0.55 0.0934 195",   # better for colour blindness
  "text",      "dark",  "over",  2L,    "#0286b1", "0.58 0.1151 230",
  "text",      "dark",  "over",  3L,    "#4687d8", "0.62 0.1400 255",
  "text",      "dark",  "over",  4L,    "#6987ff", "0.66 0.1797 270",
  "text",      "dark",  "under", 1L,    "#867002", "0.55 0.1124  95",
  "text",      "dark",  "under", 2L,    "#b87501", "0.62 0.1341  70",
  "text",      "dark",  "under", 3L,    "#ec6f02", "0.68 0.1792  50",
  "text",      "dark",  "under", 4L,    "#ff626b", "0.70 0.1906  20",

  "bg",        "dark",  "over",  1L,    "#002828", "0.25 0.0423 195",   # better for colour blindness
  "bg",        "dark",  "over",  2L,    "#012d3f", "0.28 0.0553 230",
  "bg",        "dark",  "over",  3L,    "#122e5d", "0.31 0.09   260",
  "bg",        "dark",  "over",  4L,    "#202e7a", "0.34 0.13   270",
  "bg",        "dark",  "under", 1L,    "#292100", "0.25 0.051   95",
  "bg",        "dark",  "under", 2L,    "#3b2300", "0.28 0.0602  70",
  "bg",        "dark",  "under", 3L,    "#4f2100", "0.31 0.0814  50",
  "bg",        "dark",  "under", 4L,    "#720119", "0.35 0.1401  20",
)

# The runner-up kept for each rung that has one -- only the two background channels were ever
# re-tuned. Swap a hex into the grid above and rebuild.
#   ~channel, ~theme,  ~dir,    ~rung, ~hex,      ~oklch,
#   "bg",     "light", "over",  2L,    "#d4f0ff", "0.94 0.0358 230",
#   "bg",     "light", "over",  3L,    "#d3e2ff", "0.91 0.0429 265",
#   "bg",     "light", "over",  4L,    "#c8c7ff", "0.85 0.0771 285",
#   "bg",     "light", "under", 1L,    "#ffeccd", "0.95 0.0456  80",
#   "bg",     "light", "under", 2L,    "#ffddc3", "0.92 0.051   60",
#   "bg",     "light", "under", 3L,    "#ffcebc", "0.89 0.0608  42",
#   "bg",     "light", "under", 4L,    "#ffbfb5", "0.86 0.0754 29.01",
#
#   "bg",     "dark",  "over",  1L,    "#001b1b", "0.20 0.0336 195",
#   "bg",     "dark",  "over",  2L,    "#002537", "0.25 0.0526 235",
#   "bg",     "dark",  "over",  3L,    "#132d5c", "0.30 0.0900 261",
#   "bg",     "dark",  "over",  4L,    "#17226d", "0.30 0.1300 270",
#   "bg",     "dark",  "under", 1L,    "#1c1600", "0.20 0.0407  95",
#   "bg",     "dark",  "under", 2L,    "#321c00", "0.25 0.0537  70",
#   "bg",     "dark",  "under", 3L,    "#4c1f00", "0.30 0.0792  50",
#   "bg",     "dark",  "under", 4L,    "#6b141f", "0.35 0.1200  20",
# )

# Faint -> strong, one ramp out of the grid. The ten names below are what build_palettes() and
# set_color_palette() know.
tx_ramp <- function(channel, theme, dir) {
  r <- COLOR_RAMPS[COLOR_RAMPS$channel == channel & COLOR_RAMPS$theme == theme &
                     COLOR_RAMPS$dir == dir, ]
  r$hex[order(r$rung)]
}

default_text_colors                <- tx_ramp("text",      "light", "over")
default_text_colors_neg            <- tx_ramp("text",      "light", "under")
default_background_colors          <- tx_ramp("bg",        "light", "over")
default_background_colors_neg      <- tx_ramp("bg",        "light", "under")
default_bg_legend_colors           <- tx_ramp("bg_legend", "light", "over")
default_bg_legend_colors_neg       <- tx_ramp("bg_legend", "light", "under")
default_dark_text_colors           <- tx_ramp("text",      "dark",  "over")
default_dark_text_colors_neg       <- tx_ramp("text",      "dark",  "under")
default_dark_background_colors     <- tx_ramp("bg",        "dark",  "over")
default_dark_background_colors_neg <- tx_ramp("bg",        "dark",  "under")


## 8-BIT FALLBACK PALETTES (RStudio console only) ----
# RStudio's console cannot render 24-bit truecolor, so there it falls back to these curated
# 256-colour ramps. Positron and modern terminals get the OKLCH palettes above.
palette_8bit <- list(
  text_light = c("#33FFFF", "#00CCFF", "#0066FF", "#0000FF",   # over (faint -> strong)
                 "#FF9933", "#FF6600", "#FF3333", "#FF0000"),  # under
  text_dark  = c("#CCFF33", "#99FF33", "#33FF33", "#00FF00",
                 "#FF9933", "#FF6633", "#FF3300", "#FF0000"),
  bg_light   = c("#F6F3FF", "#E9E3FF", "#DED3FF", "#D2C3FF",
                 "#fff8e6", "#ffeab1", "#fddb7c", "#ffce2d"),
  bg_dark    = c("#000066", "#000099", "#0000CC", "#0000FF",
                 "#660000", "#990000", "#CC0000", "#FF0000")
)



# === SECTION: the chrome -- everything that is NOT a colour-measure slot ===========================

# ONE resolver, per theme: tab_export_prep() builds `theme_cols` from it and tx_css_rules() emits it
# as CSS, so the two renderings cannot drift. The colour themes state their chrome here; a
# publication palette states its own two greys in PRINT_PALETTES, beside the ink ladder they sit next
# to.
#   text  : the table's own font colour (also what a reference cell gets -- it inherits, no class)
#   grey  : an uncoloured cell in a column that HAS a colour measure
#   grey2 : an uncoloured cell in a column with no colour measure -- and, by the same logic, the
#           SECONDARY tokens of a composite cell (color_secondary_hex()): both mean
#           "present, but nothing is being said about it"
#   mark  : a publication palette's effect-size MARKS, which sit where the stars sit but are not an
#           aside: they REPLACE the colour, so they carry the deviation itself and must read as
#           strongly as the number. Pure black under every print palette (a superscript glyph at
#           grey2 is too faint to be seen at all); `grey2` elsewhere, where nothing writes a mark.
# DARK: pure #FFFFFF on #111111 is a harsh, glare-y contrast for body text, so the pairing is the
# softer #CECDC3 on #222222. The border stays the text colour, so it softens with it.
tx_chrome_hex <- function(theme = "light") {
  pal <- print_palette_of(tx_palette_theme(theme))
  # On paper the chrome is fixed -- black ink, white ground, no hover -- and the only thing a
  # publication palette decides is how a cell is SET BACK: `grey` for a non-significant one, `grey2`
  # for an aside. It owns those two because they must sit beside ITS ink ladder (a palette whose first
  # rung is pure black can use the ordinary grey; one whose first rung is already grey cannot).
  if (!is.null(pal)) return(list(text = "#000000", grey = pal$grey, grey2 = pal$grey2,
                                 mark = "#000000",
                                 bg = "#ffffff", border = "#000000", hover = "transparent"))
  switch(
    tx_palette_theme(theme),
    dark = list(text = "#f0efe5", grey = "#707070", grey2 = "#CECDC3", # text = "#CECDC3", grey = "#707070", grey2 = "#bebebe",
                mark = "#CECDC3",
                bg = "#222222", border = "#CECDC3", hover = "rgba(255,242,204,.10)"),
    list(text = "#000000", grey = "#949494", grey2 = "#444444", mark = "#444444",
         bg = "#ffffff", border = "#000000", hover = "#FFFCE5")
  )
}

# === SECTION: the black-and-white publication palettes =============================================

# --- their shared background ramps ---

# The one grey FILL ramp (every publication palette's bg channel), and its dark stand-in for the media that
# have no fill at all (an Excel legend run, a ggplot label): there a fill must be spoken as ink, and
# the light ramp would be invisible on white.
PRINT_BG        <- c("#F5F5F5", "#E4E4E4", "#D0D0D0", "#B8B8B8")
PRINT_BG_LEGEND <- c("#767676", "#595959", "#3F3F3F", "#1A1A1A")

# The two mark glyphs of `theme = "print_marks"`. WARNING: U+207A / U+207B are East-Asian NEUTRAL, so
# every renderer draws them one cell wide and the column keeps its alignment. Do NOT swap in an arrow,
# a dagger or a block glyph -- those are AMBIGUOUS width and shift the column on a CJK terminal.
PRINT_MARKS <- c(over = "\u207A", under = "\u207B")

# --- the palette record ---

# THE face record, in the one place its shape is written: how each of the 8 slots is DRAWN, beside the
# hex that says what it is drawn WITH. Declared, never derived -- a backend that infers "bold" from
# "has a colour hex" collapses on an all-black publication palette.
#   bold / italic  .
#   underline      "" / "single" / "double" -- OOXML's own vocabulary, so Excel writes it verbatim
#   marks          the run of glyphs the cell wears after its value ("" = none). Drawn as a
#                  SUPPORTING piece, like the stars it stands in for -- the chrome's `grey2`, none of
#                  the cell's own face -- so neither annotation outshouts the number.
#   semantic       emit the face as MARKUP (<b>/<i>/<u>), not only as CSS -- true of the whole print
#                  family, whose destinations (GitHub, an HTML -> Word paste) carry tags and nothing else
# A length-1 argument is a CONSTANT face: that is how the colour palettes say their one fact (every
# text slot bold, nothing on a fill) without an 8-row grid of identical rows to say it.
face_record <- function(bold = FALSE, italic = FALSE, underline = "", marks = "",
                        semantic = FALSE) {
  slot8 <- function(v) if (length(v) == 8L) v else rep(v, 8L)
  list(bold = slot8(bold), italic = slot8(italic), underline = slot8(underline),
       marks = slot8(marks), semantic = semantic)
}

# One palette from its grid. `slots` is 8 rows -- the 4 OVER rungs then the 4 UNDER rungs, in the slot
# order the engine uses. Everything a backend can ask about a print slot is a column of it.
#   ink        the text colour
#   bold       .
#   italic     .
#   underline  "" / "single" / "double"  -- OOXML's own vocabulary, so Excel writes it verbatim
#   marks      how many times the direction glyph is repeated after the value (0 = none)
# and per palette: `grey` (a greyed-out cell), `grey2` (an aside, and an uncoloured column), `shade`
# (what the legend calls each direction face -- a CLOSURE so gettext() runs at render, NULL where the
# palette names none), `doc` (one line, read by the generated man-page section).
print_palette <- function(slots, doc, grey, grey2, shade = list(over = NULL, under = NULL)) {
  stopifnot(nrow(slots) == 8L,
            identical(slots$dir, rep(c("over", "under"), each = 4L)),
            identical(slots$rung, rep(1:4, times = 2L)),
            all(grepl("^#[0-9a-fA-F]{6}$", c(slots$ink, grey, grey2))),
            all(slots$underline %in% c("", "single", "double")),
            all(slots$marks %in% 0:4),
            identical(names(shade), c("over", "under")),
            all(vapply(shade, function(f) is.null(f) || is.function(f), logical(1))),
            # a mark ladder is ORDERED or it is not a ladder
            !is.unsorted(slots$marks[1:4]), !is.unsorted(slots$marks[5:8]))
  # DESIGN: the mark RUN is derived here rather than declared as text, so the grid stays readable
  # (a rank, not a row of glyphs) and the glyph pair stays stated once.
  list(doc = doc, grey = grey, grey2 = grey2, shade = shade,
       ink  = slots$ink,
       face = face_record(bold = slots$bold, italic = slots$italic, underline = slots$underline,
                          marks = strrep(PRINT_MARKS[slots$dir], slots$marks), semantic = TRUE))
}

# --- the three grids ---

PRINT_PALETTES <- list(

  # The general-purpose one: direction is the FACE, magnitude the INK, whose top rung is bold -- the
  # loudest signal a page has, spent on the strongest deviation rather than on mere direction.
  print_minimalistic = print_palette(
    doc   = "direction by underline (over) and italic (under); magnitude by an ink ladder.",
    # LIGHT on purpose: greyed means "deliberately harder to read", so it is held to the large-text
    # floor (3:1) rather than the 4.5:1 body-text one, and must stay lighter than rung 1 -- which is
    # what keeps the reading ladder greyed < rung 1 < rung 2 monotone.
    grey  = "#949494", grey2 = "#444444",
    shade = list(over = function() gettext("Underlined"), under = function() gettext("Italic")),
    tibble::tribble(
      ~dir,     ~rung, ~ink,      ~bold, ~italic, ~underline, ~marks,
      "over",   1L,    "#555555", FALSE, FALSE,   "single",   0L,
      "over",   2L,    "#000000", FALSE, FALSE,   "single",   0L,
      "over",   3L,    "#000000", TRUE,  FALSE,   "single",   0L,
      "over",   4L,    "#000000", TRUE,  FALSE,   "single",   0L,
      "under",  1L,    "#555555", FALSE, TRUE,    "",         0L,
      "under",  2L,    "#000000", FALSE, TRUE,    "",         0L,
      "under",  3L,    "#000000", TRUE,  TRUE,    "",         0L,
      "under",  4L,    "#000000", TRUE,  TRUE,    "",         0L)),

  # For a table whose CELLS already say which way they point (every tab_reg() measure prints its own
  # +/- or x/div glyph). Direction being spoken for, the typography spends everything on magnitude:
  # four emphasis rungs in pure black, italic a quiet second voice on the under side. Rung 1 is
  # undecorated, so the greyed cell can go back to the ordinary #888888.
  print_emphasis = print_palette(
    doc   = paste("magnitude by an emphasis ladder (bold, then underline, then double underline) in",
                  "pure black; direction by the cell's own measure symbol, plus italic under the null."),
    grey  = "#888888", grey2 = "#444444",
    shade = list(over = NULL, under = function() gettext("Italic")),
    tibble::tribble(
      ~dir,     ~rung, ~ink,      ~bold, ~italic, ~underline, ~marks,
      "over",   1L,    "#000000", FALSE, FALSE,   "",         0L,
      "over",   2L,    "#000000", TRUE,  FALSE,   "",         0L,
      "over",   3L,    "#000000", TRUE,  FALSE,   "single",   0L,
      "over",   4L,    "#000000", TRUE,  FALSE,   "double",   0L,
      "under",  1L,    "#000000", FALSE, TRUE,    "",         0L,
      "under",  2L,    "#000000", TRUE,  TRUE,    "",         0L,
      "under",  3L,    "#000000", TRUE,  TRUE,    "single",   0L,
      "under",  4L,    "#000000", TRUE,  TRUE,    "double",   0L)),

  # The cell says what it is in its OWN CHARACTERS -- the only encoding a plain-text copy survives and
  # a screen reader can read aloud. It REPLACES the significance stars, which sit in the same place
  # and would otherwise read as a second, contradictory run of symbols.
  print_marks = print_palette(
    doc   = paste("magnitude and direction by a repeated superscript mark after the value",
                  "(no significance stars: the marks take their place, do not use with `tab_reg()`)."),
    grey  = "#888888", grey2 = "#444444",
    shade = list(over = NULL, under = NULL),
    tibble::tribble(
      ~dir,     ~rung, ~ink,      ~bold, ~italic, ~underline, ~marks,
      "over",   1L,    "#000000", FALSE, FALSE,   "",         1L,
      "over",   2L,    "#000000", FALSE, FALSE,   "",         2L,
      "over",   3L,    "#000000", FALSE, FALSE,   "single",   3L,
      "over",   4L,    "#000000", FALSE, FALSE,   "single",   4L,
      "under",  1L,    "#000000", FALSE, FALSE,   "",         1L,
      "under",  2L,    "#000000", FALSE, FALSE,   "",         2L,
      "under",  3L,    "#000000", FALSE, FALSE,   "single",   3L,
      "under",  4L,    "#000000", FALSE, FALSE,   "single",   4L))
)

# --- print_ready: the palette a table should wear, chosen from what the table IS ---

# `print_ready` is not a palette but a CHOICE of one, made per TABLE, read off tab_is_reg(). A
# crosstab takes the MARKS -- nothing typographic competes with the numbers, and the glyphs survive a
# plain-text copy. A regression takes the EMPHASIS ladder, its cells already carrying their own
# direction symbol, which frees the typography to spend everything on magnitude.
# WARNING: the two members write DIFFERENT `.p1..m4` rules and a stylesheet is table-independent by
# contract, so a batch resolves to ONE of them and `fallback` serves a caller with no table in hand.
# It is the EMPHASIS member on purpose: marks are cell TEXT and survive a missing stylesheet.
PRINT_READY <- c(crosstab = "print_marks", regression = "print_emphasis",
                 fallback = "print_minimalistic")

# Resolve `print_ready` against the table(s) being rendered. Any other theme passes through. A batch
# takes the regression arm only when EVERY table is one -- `.p3` cannot mean two things at once.
tx_theme_for_table <- function(theme, tabs = NULL) {
  if (is.null(theme) || is.na(theme[1]) || !identical(theme[1], "print_ready")) return(theme)
  if (is.null(tabs)) return(unname(PRINT_READY[["fallback"]]))
  if (!is.list(tabs) || is.data.frame(tabs)) tabs <- list(tabs)
  is_reg <- function(t) tryCatch(isTRUE(tab_is_reg(t)), error = function(e) FALSE)
  reg <- vapply(tabs, is_reg, logical(1))
  unname(PRINT_READY[[if (length(reg) && all(reg)) "regression" else "crosstab"]])
}

# --- the accessors ---

# Is this theme one of the black-and-white palettes? THE predicate every "is it print" test funnels
# through, so adding a fourth palette needs no edit outside this file.
tx_is_print <- function(theme) {
  !is.null(theme) && !is.na(theme[1]) && theme[1] %in% names(PRINT_PALETTES)
}

# The palette record behind a theme, or NULL for the colour themes.
print_palette_of <- function(theme) {
  if (!tx_is_print(theme)) return(NULL)
  PRINT_PALETTES[[theme[1]]]
}

# The strongest rule a set of underline values asks for: a cell can be reached by several sources (a
# reference row, its own colour slot), and the logical aspects beside it merge with any().
face_underline_max <- function(v) {
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) "" else if ("double" %in% v) "double" else "single"
}

# Which palette family a GRAPHICS DEVICE reads. ggplot2's `fontface` has no underline and a plotted
# point has no face at all, so a publication palette -- black ink, magnitude in the typography --
# would collapse to one shade. There it borrows its own dark grey ramp (`bg_legend`), four ordered
# levels with bold and italic still beside them.
tx_plot_ink_family <- function(theme, channel = c("text", "bg")) {
  channel <- match.arg(channel)
  if (identical(channel, "bg")) "bg" else if (tx_is_print(theme)) "bg_legend" else "text"
}

# Does this palette annotate its cells with repeated marks? Derived, never declared: a palette that
# marks its cells must not also star them -- one cell position, one meaning.
print_palette_marks <- function(pal) !is.null(pal) && any(nzchar(pal$face$marks))

# The man-page catalogue, generated from the `doc` field so the taught list cannot drift from the
# grids above. Read by ?tab_css, the ONE page that documents the family.
print_palettes_rd <- function() {
  item <- function(nm) sprintf("  \\item{\\code{\"%s\"}}{%s}", nm, PRINT_PALETTES[[nm]]$doc)
  c("@section The black-and-white publication palettes:",
    "A greyscale print loses colour entirely --- both direction ramps become the same grey --- so",
    "these palettes say the same thing with something else. \\strong{\\code{theme = \"print_ready\"}",
    "is the one to reach for}: it picks per table, the marks for a cross-table and the emphasis",
    "ladder for a regression, whose cells already carry their own direction symbol. Name one",
    "yourself to override that. They share ONE grey fill ramp (a background colour measure keeps",
    "carrying its magnitude) and differ in the text channel:",
    "\\describe{", vapply(names(PRINT_PALETTES), item, character(1)), "}",
    "In all of them a non-significant cell is greyed out, and the significance stars stay --- except",
    "under \\code{\"print_marks\"}, where the marks take their place (one run of symbols after a",
    "value, not two). \\code{\"bw\"} is a synonym of \\code{\"print_minimalistic\"}.",
    "\\emph{One caveat}, and only for a document that emits \\code{\\link{tab_css}()} once and renders",
    "its tables with \\code{css = FALSE}: a stylesheet is table-independent, so it carries ONE of",
    "them. A cross-table is fine whatever it carries (its marks are cell text), but a regression's",
    "ladder is css and nothing else --- name it there, \\code{tab_css(theme = \"print_emphasis\")}.")
}

# === SECTION: the store, and the one assembly that keys every palette ==============================

tabxplor_palette_env <- new.env(parent = emptyenv())

default_palette_base <- function() {
  list(
    text_colors                = default_text_colors,
    text_colors_neg            = default_text_colors_neg,
    background_colors          = default_background_colors,
    background_colors_neg      = default_background_colors_neg,
    dark_text_colors           = default_dark_text_colors,
    dark_text_colors_neg       = default_dark_text_colors_neg,
    dark_background_colors     = default_dark_background_colors,
    dark_background_colors_neg = default_dark_background_colors_neg,
    bg_legend_colors           = default_bg_legend_colors,
    bg_legend_colors_neg       = default_bg_legend_colors_neg
  )
}

build_palettes <- function() {
  e <- tabxplor_palette_env
  if (is.null(e$base)) e$base <- default_palette_base()
  b <- e$base
  e$hex <- list(
    text_light = c(b$text_colors,            b$text_colors_neg),
    text_dark  = c(b$dark_text_colors,       b$dark_text_colors_neg),
    bg_light   = c(b$background_colors,       b$background_colors_neg),
    bg_dark    = c(b$dark_background_colors,  b$dark_background_colors_neg),
    bg_legend_light = c(b$bg_legend_colors,        b$bg_legend_colors_neg),
    bg_legend_dark  = c(b$dark_background_colors,  b$dark_background_colors_neg)
  )
  # WARNING: a print palette reads from PRINT_PALETTES, never from `b` -- that is what makes
  # set_color_palette() provably unable to touch publication output. Its fill ramp is SHARED by the
  # whole family: greyscale cannot diverge, so a fill carries magnitude whatever the text channel does.
  for (nm in names(PRINT_PALETTES)) {
    e$hex[[paste0("text_", nm)]]      <- PRINT_PALETTES[[nm]]$ink
    e$hex[[paste0("bg_", nm)]]        <- c(PRINT_BG,        PRINT_BG)
    e$hex[[paste0("bg_legend_", nm)]] <- c(PRINT_BG_LEGEND, PRINT_BG_LEGEND)
  }
  # THE FACE -- the twin of the hex, assembled here exactly as the hex is (see face_record() above).
  # WARNING: bold-on-every-text-slot is not decoration. It IS what tx_css_render()'s static
  # `.p1..m4{font-weight:bold}` rule says, i.e. THE baseline tx_face_decls() diffs every palette
  # against, so changing it changes what the print stylesheet has to restate.
  bold8 <- face_record(bold = TRUE)          # the colour palettes' one fact, and THE CSS baseline
  flat  <- face_record()                     # a fill says nothing typographically, in any palette
  e$face <- list(text_light = bold8, text_dark = bold8, bg_light = flat, bg_dark = flat,
                 bg_legend_light = flat, bg_legend_dark = flat)
  for (nm in names(PRINT_PALETTES)) {
    e$face[[paste0("text_", nm)]]      <- PRINT_PALETTES[[nm]]$face
    e$face[[paste0("bg_", nm)]]        <- flat
    e$face[[paste0("bg_legend_", nm)]] <- flat
  }

  bit8 <- isTRUE(Sys.getenv("RSTUDIO") == "1")
  ncol <- if (bit8) 256L else cli::num_ansi_colors()
  mk <- function(key, is_bg) {
    # palette_8bit has no print key -- without the is.null guard the RStudio console would build an EMPTY
    # style list and every slot lookup would abort ("subscript out of bounds").
    src <- if (bit8 && !is.null(palette_8bit[[key]])) palette_8bit[[key]] else e$hex[[key]]
    purrr::map(src, ~ cli::make_ansi_style(., bg = is_bg, colors = ncol))
  }
  e$ansi <- list(
    text_light = mk("text_light", FALSE), text_dark = mk("text_dark", FALSE),
    bg_light   = mk("bg_light",   TRUE),  bg_dark   = mk("bg_dark",   TRUE)
  )
  # Built so get_color_style("crayon", theme = <a print palette>) cannot error. The console does not
  # select one by default, but options(tabxplor.color_style_theme=) can. The FACE is deliberately NOT
  # baked here: the console applies bold separately via options(tabxplor.console_bold), so baking it
  # would double-apply.
  for (nm in names(PRINT_PALETTES)) {
    e$ansi[[paste0("text_", nm)]] <- mk(paste0("text_", nm), FALSE)
    e$ansi[[paste0("bg_", nm)]]   <- mk(paste0("bg_", nm),   TRUE)
  }
  invisible()
}
