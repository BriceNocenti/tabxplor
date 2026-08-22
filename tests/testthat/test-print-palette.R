# PURPOSE: lock the black-and-white publication palette (`theme = "print_minimalistic"`, Phase 18z11) -- the
# palette itself, its PERCEPTUAL measurements, the engine's theme-blindness, the CSS it generates, and
# the four backends' renderings.
# WHY the perceptual block exists: this palette is not a taste choice, it is the answer to a
# MEASUREMENT (dev/black_and_white_publication_palette.md SS1: converted to CIE L*, the colour
# palette's two background directions are the same grey ramp, so a greyscale print loses the over/under
# distinction). Those tests carry the measurement into the code, so nobody can prettify the greys back
# into unreadability.
# The luminance helper is written out here rather than reused from dev/color_palette_tools.R, which
# needs `farver` -- not a package dependency, and a test must not add one.

# --- pure-R sRGB relative luminance / WCAG contrast / CIE L* ---------------------------------------
zz_lum <- function(hex) {
  v <- grDevices::col2rgb(hex) / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  as.numeric(c(0.2126, 0.7152, 0.0722) %*% v)
}
zz_contrast <- function(a, b) {
  l1 <- zz_lum(a); l2 <- zz_lum(b)
  (pmax(l1, l2) + 0.05) / (pmin(l1, l2) + 0.05)
}
zz_lstar <- function(hex) {
  as.numeric(grDevices::convertColor(t(grDevices::col2rgb(hex) / 255), "sRGB", "Lab")[, "L"])
}

zz_tab <- function() {
  tab(forcats::gss_cat, marital, race, pct = "row", color = "diff")
}
# A fixture whose cells reach the SECOND typographic level (slots 3/4/7/8 = the underlined ones);
# zz_tab() only ever reaches slot 2, so it cannot exercise the underline.
zz_deep <- function() {
  tab(forcats::gss_cat, relig, race, pct = "row", color = "diff")
}
# medium = "runs" returns one run-LIST per footer stream; flatten to the runs themselves.
zz_runs <- function(x, theme) unlist(tab_color_legend(x, medium = "runs", theme = theme),
                                     recursive = FALSE)

# === SECTION: the palette itself ===================================================================

testthat::test_that("the colour palettes' face IS the CSS baseline (bold on every text slot)", {
  # THE load-bearing invariant. tx_css_render() emits a STATIC `.p1,...,.m4{font-weight:bold;}` rule
  # outside the theme cascade, and tx_face_decls() treats it as the baseline every theme diffs against.
  # That is only legitimate while the colour palettes really do report bold on all 8 text slots and
  # nothing on the background ones. If this fails, the print CSS is emitting the wrong divergences.
  for (th in c("light", "dark")) {
    ft <- get_color_style("face", type = "text", theme = th)
    testthat::expect_true(all(ft$bold), label = th)
    # `underline` is the three-value vocabulary ("" / "single" / "double"), `marks` the mark run.
    testthat::expect_false(any(ft$italic) || any(nzchar(ft$underline)) || any(nzchar(ft$marks)),
                           label = th)
    testthat::expect_false(isTRUE(ft$semantic), label = th)
    for (ty in c("bg", "bg_legend")) {
      fb <- get_color_style("face", type = ty, theme = th)
      testthat::expect_false(any(fb$bold) || any(fb$italic) || any(nzchar(fb$underline)),
                             label = paste(th, ty))
    }
  }
})

testthat::test_that("the print palette is typographic: an ink ladder, one grey fill ramp, a real face", {
  ft <- get_color_style("face", type = "text", theme = "print_minimalistic")
  # THE two axes, split: DIRECTION is the face -- over is UNDERLINED (slots 1-4), under is ITALIC
  # (slots 5-8) and never underlined. MAGNITUDE is the ink ramp, whose top rung adds the bold.
  testthat::expect_identical(ft$underline, c(rep("single", 4L), rep("", 4L)))
  testthat::expect_identical(ft$italic,    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  testthat::expect_identical(ft$bold,      c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
  # it says nothing with marks: those are print_marks' whole vocabulary, and the two must not mix.
  testthat::expect_false(any(nzchar(ft$marks)))
  # the face must survive without a stylesheet (GitHub strips class+style; a Word paste keeps tags)
  testthat::expect_true(isTRUE(ft$semantic))

  # the ink is the MAGNITUDE: 3 rungs over 4 slots (#555555, #000000, #000000 + bold), the SAME on
  # both sides -- a magnitude knows no direction. Slots 3 and 4 therefore render identically.
  ink <- unname(get_color_style("color_code", "text", "print_minimalistic"))
  testthat::expect_identical(ink[1:4], c("#555555", "#000000", "#000000", "#000000"))
  testthat::expect_identical(ink[1:4], ink[5:8])
  bg <- unname(get_color_style("color_code", "bg", "print_minimalistic"))
  # The two directions are DELIBERATELY the same ramp: greyscale cannot diverge (a diverging grey scale
  # needs a mid-grey neutral, i.e. shading every cell). The fill carries magnitude; direction is read
  # off the cell's own bold/italic. Asserted so nobody later "fixes" it into a fake divergence.
  testthat::expect_identical(bg[1:4], bg[5:8])
  testthat::expect_false(any(get_color_style("face", "bg", "print_minimalistic")$bold))
})

testthat::test_that("every publication palette is a complete, ordered grid", {
  # The grid IS the definition (R/tab-palettes.R): one row per break slot, carrying the ink, the
  # face and the mark. What is asserted here is what a backend is entitled to assume of ANY of them --
  # so a fourth palette is one new row there and needs no new test.
  for (nm in names(PRINT_PALETTES)) {
    ft  <- get_color_style("face", type = "text", theme = nm)
    ink <- unname(get_color_style("color_code", "text", nm))
    testthat::expect_length(ink, 8L)
    testthat::expect_true(all(grepl("^#[0-9a-fA-F]{6}$", ink)), label = nm)
    # a magnitude knows no direction: the same ramp on both sides, always.
    testthat::expect_identical(ink[1:4], ink[5:8], label = nm)
    testthat::expect_true(all(ft$underline %in% c("", "single", "double")), label = nm)
    testthat::expect_true(isTRUE(ft$semantic), label = nm)          # survives a class-stripping host
    # ONE background palette for the whole family -- the fill ramp is not a per-palette choice.
    testthat::expect_identical(unname(get_color_style("color_code", "bg", nm)),
                               c(PRINT_BG, PRINT_BG), label = nm)
    # the greyed-out cell: below the body-text floor on purpose, but never below the non-text one, and
    # always lighter than the ladder's first rung (greyed < rung 1 is the whole reading order).
    g <- tx_chrome_hex(nm)$grey
    testthat::expect_gte(zz_contrast(g, "#FFFFFF"), 3)
    testthat::expect_lt(zz_contrast(g, "#FFFFFF"), zz_contrast(ink[1], "#FFFFFF"))
  }
  # NOT asserted: that marks exclude typography. A marks palette may reinforce its top rungs with a
  # face if that reads better -- the two are different channels. What may never coexist is marks and
  # STARS, one place with two meanings, and that is derived rather than declared (fmt_cell_suffix)
  # and tested where it renders.
})

testthat::test_that("print_emphasis rules its top rungs, in CSS and in Excel", {
  # its ladder is EMPHASIS, not ink: bold, then a rule, then a doubled rule -- direction being carried
  # by the cell's own measure symbol, plus italic under the null.
  css <- tab_css(theme = "print_emphasis", style_tag = FALSE)
  testthat::expect_match(css, ".p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline double;}",
                         fixed = TRUE)
  testthat::expect_match(css, ".m4,.tabxplor-tab .m4{color:#000000;font-style:italic;text-decoration:underline double;}",
                         fixed = TRUE)
  # a doubled rule is OOXML's own vocabulary, so it is written verbatim rather than flattened.
  skip_if_not_installed("openxlsx2")
  f <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(zz_deep(), path = f, theme = "print_emphasis", open = FALSE, replace = TRUE))
  st <- paste(readLines(unzip(f, "xl/styles.xml", exdir = withr::local_tempdir()), warn = FALSE),
              collapse = "")
  testthat::expect_match(st, '<u val="double"', fixed = TRUE)
})

testthat::test_that("print_marks marks the cells instead of starring them", {
  x  <- zz_deep()
  fc <- names(purrr::keep(x, is_fmt))[2]
  # the mark is the SLOT's rendering, so it needs no direction logic of its own; a greyed cell (slot 0)
  # takes none, and the run grows with the magnitude.
  txt <- format(x[[fc]], stars = TRUE, theme = "print_marks")
  testthat::expect_true(any(grepl("\u207a", txt)))
  # and it REPLACES the stars: one place after the value, one meaning. The other palettes keep them.
  y <- fmt(n = 100, pct = 0.6, diff = 0.2, pvalue = 0.001, color = "diff", display = "pct",
           digits = 0)
  testthat::expect_match(format(y, stars = TRUE, theme = "light"), "*", fixed = TRUE)
  testthat::expect_match(format(y, stars = TRUE, theme = "print_minimalistic"), "*", fixed = TRUE)
  testthat::expect_false(grepl("*", format(y, stars = TRUE, theme = "print_marks"), fixed = TRUE))
  # ... in every backend: the numFmt literal keeps an Excel cell a real NUMBER.
  testthat::expect_match(tab_md(x, theme = "print_marks", css = FALSE, print = FALSE), "\u207a")
  testthat::expect_match(as.character(tab_html(x, theme = "print_marks", print = FALSE)), "\u207a")
  skip_if_not_installed("openxlsx2")
  f <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(x, path = f, theme = "print_marks", open = FALSE, replace = TRUE))
  st <- paste(readLines(unzip(f, "xl/styles.xml", exdir = withr::local_tempdir()), warn = FALSE,
                        encoding = "UTF-8"), collapse = "")
  testthat::expect_match(st, "formatCode=\"[^\"]*\u207a")
  # the legend carries the marks on the break-words (nothing else tells them apart) and drops the
  # stars line, since no star is drawn.
  lg <- tab_color_legend(x, medium = "plain", theme = "print_marks")
  testthat::expect_match(lg, "\u207a\u207a")
  testthat::expect_null(tab_stars_legend(x, theme = "print_marks"))
})

testthat::test_that("the cell suffix supports the number rather than competing with it", {
  # THE rule, in every theme: stars and marks are drawn like an aside -- grey2, no bold / italic /
  # underline -- so a run of symbols never shouts louder than the value it qualifies. Mechanically it
  # is one fact: the primary RANGE stops where the value stops, and every backend already paints what
  # falls outside it as secondary.
  y <- fmt(n = c(100, 100), pct = c(0.6, 0.3), diff = c(0.2, -0.1), pvalue = c(0.001, 0.4),
           display = "pct", digits = 0, color = "diff")
  o  <- format(y, stars = TRUE, bold_split = TRUE)
  pf <- attr(o, "primary_from"); pn <- attr(o, "primary_nchar")
  # substr() carries the attributes over, so strip them before comparing.
  testthat::expect_identical(as.character(substr(o, pf, pf + pn - 1L)), c("60%", "30%"))
  # ... in a composite, where the primary token sits among asides ...
  oc <- format(set_display(y, "{pct} (n={n})"), stars = TRUE, bold_split = TRUE)
  pf <- attr(oc, "primary_from"); pn <- attr(oc, "primary_nchar")
  testthat::expect_identical(as.character(substr(oc, pf, pf + pn - 1L)), c("60%", "30%"))
  # ... and in html, where it lands in the aside's own span and takes no face markup.
  x <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff", ci = "cell")
  x <- dplyr::mutate(x, dplyr::across(dplyr::where(is_fmt), ~ set_display(set_pvalue(., 0.001), "pct")))
  for (th in c("light", "print_minimalistic")) {
    h <- as.character(tab_html(x, theme = th, print = FALSE))
    testthat::expect_match(h, '<span class="tx-sec"[^>]*>\\*\\*\\*</span>', label = th)
    testthat::expect_no_match(h, "<[biu]>[^<]*\\*\\*\\*", label = th)
  }
})

testthat::test_that("set_color_palette() cannot alter the print palette", {
  # It is composed from PRINT_PALETTES, never from e$base -- a byte-property, because the
  # palette's correctness is a measurement set_color_palette()'s validator cannot check.
  before <- get_color_style("color_code", "bg", "print_minimalistic")
  withr::defer(set_color_palette(background_colors = tabxplor:::default_background_colors))
  set_color_palette(background_colors = c("#FF0000", "#FF3333", "#FF6666", "#FF9999"))
  testthat::expect_identical(get_color_style("color_code", "bg", "print_minimalistic"), before)
})

testthat::test_that("the print palette meets its perceptual specification", {
  fills <- unname(get_color_style("color_code", "bg", "print_minimalistic"))[1:4]
  L <- zz_lstar(fills)
  testthat::expect_true(all(diff(L) < 0))                       # strictly darkening
  testthat::expect_gte(min(abs(diff(L))), 4)                    # each step discriminable
  testthat::expect_gte(zz_contrast("#000000", fills[4]), 7)     # black on the darkest stays AAA

  # The non-significant grey is DELIBERATELY light: greyed out means "harder to read on purpose", so
  # it is held to the WCAG large-text / non-text floor (3:1 on white, measured 3.54) and merely to
  # VISIBLE on the deepest fill (1.79) -- never to the 4.5:1 body-text floor a cell that is meant to be
  # read must meet. Do not "fix" it back up: that is what would make a non-significant cell compete
  # with a significant one.
  grey <- tx_chrome_hex("print_minimalistic")$grey
  testthat::expect_gte(zz_contrast(grey, "#FFFFFF"), 3)
  testthat::expect_gte(zz_contrast(grey, fills[4]), 1.5)
  # ... and still reads as GREYED beside a significant cell's pure black.
  testthat::expect_lt(zz_contrast(grey, "#FFFFFF"), zz_contrast("#000000", "#FFFFFF"))
  # THE reading ladder, weakest to strongest: greyed < the ink's first rung < its second (bold rides
  # the second, so the last step is typographic rather than a third shade).
  ink <- unname(get_color_style("color_code", "text", "print_minimalistic"))
  testthat::expect_true(all(diff(zz_contrast(c(grey, ink[1], ink[2]), "#FFFFFF")) > 0))

  # The legend's font stand-in for the fills (an Excel run / ggpubr label cannot fill).
  testthat::expect_true(all(zz_contrast(get_color_style("color_code", "bg_legend", "print_minimalistic"),
                                        "#FFFFFF") >= 4.5))
})

# === SECTION: the engine stays theme-blind =========================================================

testthat::test_that("a theme changes the RENDERING, never the slots", {
  col <- zz_tab()[["Black"]]
  a <- fmt_channel_codes(col, "light")
  b <- fmt_channel_codes(col, "print_minimalistic")
  testthat::expect_identical(a$text_slot, b$text_slot)   # the engine never saw the theme
  testthat::expect_identical(a$bg_slot,   b$bg_slot)
  testthat::expect_false(identical(a$text_face$italic, b$text_face$italic))
  testthat::expect_true(any(b$text_face$italic))
})

testthat::test_that('"bw" is a silent alias for "print_minimalistic"', {
  testthat::expect_identical(tab_css(theme = "bw",   style_tag = FALSE),
                             tab_css(theme = "print_minimalistic", style_tag = FALSE))
  testthat::expect_identical(
    withr::with_options(list(tabxplor.theme = "bw"), resolve_export_opts()$theme), "print_minimalistic")
})

# === SECTION: the CSS ==============================================================================

testthat::test_that("the print stylesheet says exactly what the face table says", {
  css <- tab_css(theme = "print_minimalistic", style_tag = FALSE)
  ln  <- strsplit(css, "\n")[[1]]
  one <- function(cls) grep(paste0("^\\.", cls, ",\\.tabxplor-tab \\.", cls, "\\{"), ln, value = TRUE)

  testthat::expect_match(one("m1"), "font-weight:normal;")   # must beat the static bold baseline
  testthat::expect_match(one("m1"), "font-style:italic;")
  # the ink ladder's first rung (the stylesheet upper-cases every slot hex)
  testthat::expect_match(one("m1"), "#555555", fixed = TRUE)
  # the under side is NEVER underlined -- that is the whole direction signal
  testthat::expect_no_match(one("m1"), "text-decoration:underline;")
  testthat::expect_no_match(one("m3"), "text-decoration:underline;")
  # ... and the over side always is, at every rung
  testthat::expect_match(one("p1"), "text-decoration:underline;")
  testthat::expect_match(one("p3"), "text-decoration:underline;")
  testthat::expect_no_match(one("p1"), "font-style:")        # over-cells are underlined, never italic
  # bold is the ladder's TOP rung, on both sides: slots 1-2 must beat the static baseline, slots 3-4
  # inherit it and so state nothing at all.
  testthat::expect_match(one("p1"), "font-weight:normal;")
  testthat::expect_no_match(one("p3"), "font-weight:")
  testthat::expect_no_match(one("m3"), "font-weight:")

  # the aside of a composite cell is left out of the face, as it is left out of the colour. The
  # inline-block is what does it: a text-decoration cannot be switched off by a descendant.
  sec <- grep("^\\.tabxplor-tab \\.tx-sec\\{", ln, value = TRUE)
  testthat::expect_match(sec, "display:inline-block;")
  testthat::expect_match(sec, "font-style:normal;")
  testthat::expect_match(one("g1"), tx_chrome_hex("print_minimalistic")$grey, fixed = TRUE)
  # the background channel carries NO typography (a fill alone does not bold, in any palette)
  testthat::expect_no_match(one("o2"), "font-")

  # the static baseline rule is still emitted exactly ONCE, outside the cascade
  testthat::expect_length(grep("^\\.p1,\\.p2,.*\\.m4\\{font-weight:bold;\\}$", ln), 1L)
})

testthat::test_that("a publication palette is a SHEET: its chrome reaches the cells themselves", {
  # A host that colours the `<td>` DIRECTLY -- Bootstrap's `.table>:not(caption)>*>*`, which pkgdown
  # stamps on every table -- beats a colour merely inherited from the table element. Without this the
  # cells of a dark page would keep the page's own dark ground while the table around them went white:
  # half light, half dark, which is worse than either. Stated at (0,1,1) on purpose: it TIES the host
  # and wins on source order, and still loses to our own slot classes (0,2,0), so an ink ladder is not
  # flattened by it.
  for (nm in names(PRINT_PALETTES)) {
    ln <- strsplit(tab_css(theme = nm, style_tag = FALSE), "\n")[[1]]
    cell <- grep("^\\.tabxplor-tab th,\\.tabxplor-tab td\\{", ln, value = TRUE)
    testthat::expect_true(any(grepl("color:#", cell)), label = nm)
    testthat::expect_true(any(grepl("background-color:#", cell)), label = nm)
  }
  # the colour themes do NOT force it -- they follow the page on purpose, `auto` most of all.
  base <- strsplit(tab_css(theme = "light", style_tag = FALSE), "\n")[[1]]
  base <- base[seq_len(which(grepl("^@media print", base))[1] - 1L)]
  testthat::expect_false(any(grepl("^\\.tabxplor-tab th,\\.tabxplor-tab td\\{[^}]*background-color:", base)))
})

testthat::test_that("@media print carries the palette into any coloured page", {
  css <- tab_css(theme = "light", style_tag = FALSE)
  testthat::expect_match(css, "@media print \\{")
  blk <- sub("(?s).*@media print \\{", "", css, perl = TRUE)
  testthat::expect_match(blk, "font-style:italic;")
  # Browsers DROP background-color when printing unless the reader ticks "background graphics" --
  # without this the grey fills silently never reach the paper.
  testthat::expect_match(blk, "print-color-adjust:exact")

  # theme = "print_minimalistic" already IS the palette: re-stating it inside the at-rule would be dead weight.
  pcss <- tab_css(theme = "print_minimalistic", style_tag = FALSE)
  pblk <- sub("(?s).*@media print \\{", "", pcss, perl = TRUE)
  testthat::expect_no_match(pblk, "font-style:italic;")
})

testthat::test_that('under theme = "auto" the print block also out-specifies the page-toggle hooks', {
  # THE trap. Cascade layers 3/4 are hook-prefixed (`body.quarto-dark .tabxplor-tab .p1` = (0,3,1)) and
  # out-specify a plain `.tabxplor-tab .p1` (0,2,0) WHATEVER the source order -- so without a hooked
  # twin, a Quarto-dark page would print in dark colours. Fails against a naive one-layer version.
  css <- tab_css(theme = "auto", style_tag = FALSE)
  blk <- sub("(?s).*@media print \\{", "", css, perl = TRUE)
  testthat::expect_match(blk, "body.quarto-dark .tabxplor-tab .m1", fixed = TRUE)
  testthat::expect_match(blk, "[data-theme=light] .tabxplor-tab .m1", fixed = TRUE)
})

testthat::test_that("print_rules opts out, by argument and by option", {
  testthat::expect_no_match(tab_css(style_tag = FALSE, print_rules = FALSE), "@media print")
  # tab_html()/tab_md() carry no argument of their own -- they inherit the option through tab_css(),
  # which is what lets one setting cover a whole document.
  md <- withr::with_options(list(tabxplor.print_rules = FALSE), tab_md(zz_tab(), css = TRUE))
  testthat::expect_no_match(md, "@media print")
  testthat::expect_match(tab_md(zz_tab(), css = TRUE), "@media print")
})

# === SECTION: the backends =========================================================================

testthat::test_that("html cells wear the face as a class AND as markup", {
  strip <- function(h) gsub("(?s)<style>.*?</style>", "", as.character(h), perl = TRUE)
  p <- strip(tab_html(zz_tab(), theme = "print_minimalistic", tooltips = FALSE))
  l <- strip(tab_html(zz_tab(), theme = "light", tooltips = FALSE))

  # An under-represented cell at the ladder's first rungs is NOT bold (bold is the top rung, not the
  # direction) and it IS italic; an over-represented one is underlined.
  testthat::expect_match(p, '<td class="[^"]*\\bm[0-9]\\b[^"]*"><i>')
  testthat::expect_no_match(p, '<td class="[^"]*\\bm[12] tx-b"')
  testthat::expect_match(p, '<td class="[^"]*\\bp[0-9]\\b[^"]*"><u>')
  # The markup is what survives GitHub (class+style stripped) and an HTML -> Word paste.
  testthat::expect_true(grepl("<i>", p, fixed = TRUE) && grepl("<u>", p, fixed = TRUE))
  # ... and the colour palettes emit none of it, which is why they stayed byte-identical.
  testthat::expect_false(grepl("<i>", l, fixed = TRUE) || grepl("<b>", l, fixed = TRUE))
})

testthat::test_that("the face stops at the primary token, as the colour does", {
  # THE 22d-ii rule. A composite cell prints "24% (1 234)": the count is an aside, not what the measure
  # grades, so the <u>/<i> must close before it -- and the `tx-sec` span must sit OUTSIDE the markup.
  strip <- function(h) gsub("(?s)<style>.*?</style>", "", as.character(h), perl = TRUE)
  t <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff",
           display = "{pct} ({n})")
  h <- strip(tab_html(t, theme = "print_minimalistic", tooltips = FALSE))
  cells <- regmatches(h, gregexpr("<td[^>]*>(<[ubi]>)?[^<]*(</[ubi]>)?<span class=\"tx-sec\"",
                                  h, perl = TRUE))[[1]]
  testthat::expect_gt(length(cells), 0L)               # never vacuous
  # every composite cell closes its face before the aside opens
  testthat::expect_false(any(grepl("<[ubi]><span", cells)))

  # ... and the opt-out puts it back over the whole cell.
  hw <- withr::with_options(list(tabxplor.color_whole_cell = TRUE),
                            strip(tab_html(t, theme = "print_minimalistic", tooltips = FALSE)))
  testthat::expect_match(hw, "<td[^>]*><[ubi]>[^<]*<span", perl = TRUE)
})

testthat::test_that("markdown needs no code of its own: the stylesheet carries print", {
  # md cells are bare `[42%]{.m2}` spans and their bold has ALWAYS come from the stylesheet, so the
  # print scheme reaches the CELLS through tab_css() alone -- the pipe grid is byte-identical, and the
  # whole rendering difference lives in the stylesheet (plus the legend, which names the face).
  grid <- function(x) grep("^\\|", strsplit(x, "\n")[[1]], value = TRUE)
  testthat::expect_identical(grid(tab_md(zz_tab(), theme = "print_minimalistic", css = TRUE)),
                             grid(tab_md(zz_tab(), theme = "light", css = TRUE)))
  testthat::expect_match(tab_md(zz_tab(), theme = "print_minimalistic", css = TRUE), "font-style:italic;")
  # the legend follows the palette: its under-side break-words are italic in the raw markdown too
  testthat::expect_match(tab_md(zz_tab(), theme = "print_minimalistic", css = TRUE),
                         "*[-5]{.m1}*", fixed = TRUE)
})

testthat::test_that("Excel writes the face as real font attributes", {
  skip_if_not_installed("openxlsx2")
  # ⚠ the NUMBER font only. The unit row is header chrome and is italic in every theme (it is the
  # console's own type tag); what must not happen under a COLOUR palette is a cell wearing a FACE,
  # and a cell is written in `font_num` ("DejaVu Sans"), the header in `font_text` (its Condensed
  # sibling). Matching on the number font is what makes this assertion about the palette.
  fonts_of <- function(theme) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    tab_xl(zz_deep(), path = p, theme = theme, replace = TRUE)
    f <- as.character(openxlsx2::wb_load(p)$styles_mgr$styles$fonts)
    paste(grep('name val="DejaVu Sans"', f, fixed = TRUE, value = TRUE), collapse = " ")
  }
  testthat::expect_match(fonts_of("print_minimalistic"), "<i/>|<i />|<i val")
  testthat::expect_match(fonts_of("print_minimalistic"), "<u/>|<u />|<u val")
  # the byte-identity half: a colour palette emits neither
  testthat::expect_no_match(fonts_of("light"), "<i/>|<i />|<i val")
})

testthat::test_that("the legend wears the same face as the cells it describes", {
  # It MUST, and not only for looks: the html break-word carries an INLINE font-weight, which beats the
  # stylesheet -- so a hex-driven rule would have printed a bold word over italic cells.
  h <- tab_color_legend(zz_tab(), medium = "html", theme = "print_minimalistic", classes = TRUE)
  m <- regmatches(h, regexpr('<span class="m[0-9]"[^>]*>', h))
  testthat::expect_match(m, "font-style:italic;")
  testthat::expect_no_match(m, "font-weight:bold;")
  testthat::expect_match(regmatches(h, regexpr('<span class="p[0-9]"[^>]*>', h)),
                         "text-decoration:underline;")

  testthat::expect_true(any(vapply(zz_runs(zz_tab(), "print_minimalistic"),
                                   function(r) isTRUE(r$italic), logical(1))))
  testthat::expect_false(any(vapply(zz_runs(zz_tab(), "light"),
                                    function(r) isTRUE(r$italic), logical(1))))
})

testthat::test_that("the legend names the face, not a colour, and never promises unmade distinctions", {
  pl <- tab_color_legend(zz_tab(), medium = "plain", theme = "print_minimalistic")
  testthat::expect_match(pl, "Underlined")
  testthat::expect_match(pl, "Italic")
  # a COLOUR palette names no direction at all since 22f-i (the break-words are coloured); the
  # publication ones keep theirs, which is the whole point of this test.
  testthat::expect_no_match(tab_color_legend(zz_tab(), medium = "plain", theme = "light"),
                            "Underlined")

  # The ink ladder has 3 rungs and the break scale 4 slots, so slots 3&4 share a rendering and the
  # legend drops the repeated break-word -- keeping the LOWER threshold of the pair.
  n_breaks <- function(theme)
    sum(vapply(zz_runs(zz_tab(), theme), function(x) !is.na(x$color), logical(1)))
  testthat::expect_lt(n_breaks("print_minimalistic"), n_breaks("light"))
  testthat::expect_match(pl, "+5", fixed = TRUE)
  testthat::expect_match(pl, "+10", fixed = TRUE)
  testthat::expect_no_match(pl, "+30", fixed = TRUE)
})

testthat::test_that("tab_plot renders the print palette without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggpubr")
  testthat::expect_no_error(tab_plot(zz_tab(), theme = "print_minimalistic"))
})

testthat::test_that("a transposed table keeps the print face", {
  h <- gsub("(?s)<style>.*?</style>", "",
            as.character(tab_html(zz_deep(), theme = "print_minimalistic", transpose = TRUE, tooltips = FALSE)),
            perl = TRUE)
  testthat::expect_match(h, "<i>", fixed = TRUE)
})
