# PURPOSE: lock the black-and-white publication palette (`theme = "print"`, Phase 18z11) -- the
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
    testthat::expect_false(any(ft$italic) || any(ft$underline), label = th)
    testthat::expect_false(isTRUE(ft$semantic), label = th)
    for (ty in c("bg", "bg_legend")) {
      fb <- get_color_style("face", type = ty, theme = th)
      testthat::expect_false(any(fb$bold) || any(fb$italic) || any(fb$underline),
                             label = paste(th, ty))
    }
  }
})

testthat::test_that("the print palette is typographic: black text, one grey fill ramp, a real face", {
  ft <- get_color_style("face", type = "text", theme = "print")
  # over = bold (slots 1-4), under = italic (slots 5-8); the second intensity level adds an underline.
  testthat::expect_identical(ft$bold,      c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  testthat::expect_identical(ft$italic,    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  testthat::expect_identical(ft$underline, c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
  # the face must survive without a stylesheet (GitHub strips class+style; a Word paste keeps tags)
  testthat::expect_true(isTRUE(ft$semantic))

  testthat::expect_identical(unname(get_color_style("color_code", "text", "print")),
                             rep("#000000", 8L))
  bg <- unname(get_color_style("color_code", "bg", "print"))
  # The two directions are DELIBERATELY the same ramp: greyscale cannot diverge (a diverging grey scale
  # needs a mid-grey neutral, i.e. shading every cell). The fill carries magnitude; direction is read
  # off the cell's own bold/italic. Asserted so nobody later "fixes" it into a fake divergence.
  testthat::expect_identical(bg[1:4], bg[5:8])
  testthat::expect_false(any(get_color_style("face", "bg", "print")$bold))
})

testthat::test_that("set_color_palette() cannot alter the print palette", {
  # It is composed from default_print_palette(), never from e$base -- a byte-property, because the
  # palette's correctness is a measurement set_color_palette()'s validator cannot check.
  before <- get_color_style("color_code", "bg", "print")
  withr::defer(set_color_palette(background_colors = tabxplor:::default_background_colors))
  set_color_palette(background_colors = c("#FF0000", "#FF3333", "#FF6666", "#FF9999"))
  testthat::expect_identical(get_color_style("color_code", "bg", "print"), before)
})

testthat::test_that("the print palette meets its perceptual specification", {
  fills <- unname(get_color_style("color_code", "bg", "print"))[1:4]
  L <- zz_lstar(fills)
  testthat::expect_true(all(diff(L) < 0))                       # strictly darkening
  testthat::expect_gte(min(abs(diff(L))), 4)                    # each step discriminable
  testthat::expect_gte(zz_contrast("#000000", fills[4]), 7)     # black on the darkest stays AAA

  # The non-significant grey must stay legible ON the deepest fill -- the light theme's #9f9f9f is
  # 1.41:1 there (invisible), which is why print carries its own.
  grey <- tx_chrome_hex("print")$grey
  testthat::expect_gte(zz_contrast(grey, fills[4]), 3)
  testthat::expect_gte(zz_contrast(grey, "#FFFFFF"), 4.5)
  # ... and still reads as GREYED beside a significant cell's pure black.
  testthat::expect_lt(zz_contrast(grey, "#FFFFFF"), zz_contrast("#000000", "#FFFFFF"))

  # The legend's font stand-in for the fills (an Excel run / ggpubr label cannot fill).
  testthat::expect_true(all(zz_contrast(get_color_style("color_code", "bg_legend", "print"),
                                        "#FFFFFF") >= 4.5))
})

# === SECTION: the engine stays theme-blind =========================================================

testthat::test_that("a theme changes the RENDERING, never the slots", {
  col <- zz_tab()[["Black"]]
  a <- fmt_channel_codes(col, "light")
  b <- fmt_channel_codes(col, "print")
  testthat::expect_identical(a$text_slot, b$text_slot)   # the engine never saw the theme
  testthat::expect_identical(a$bg_slot,   b$bg_slot)
  testthat::expect_false(identical(a$text_face$italic, b$text_face$italic))
  testthat::expect_true(any(b$text_face$italic))
})

testthat::test_that('"bw" is a silent alias for "print"', {
  testthat::expect_identical(tab_css(theme = "bw",   style_tag = FALSE),
                             tab_css(theme = "print", style_tag = FALSE))
  testthat::expect_identical(
    withr::with_options(list(tabxplor.theme = "bw"), resolve_export_opts()$theme), "print")
})

# === SECTION: the CSS ==============================================================================

testthat::test_that("the print stylesheet says exactly what the face table says", {
  css <- tab_css(theme = "print", style_tag = FALSE)
  ln  <- strsplit(css, "\n")[[1]]
  one <- function(cls) grep(paste0("^\\.", cls, ",\\.tabxplor-tab \\.", cls, "\\{"), ln, value = TRUE)

  testthat::expect_match(one("m1"), "font-weight:normal;")   # must beat the static bold baseline
  testthat::expect_match(one("m1"), "font-style:italic;")
  testthat::expect_no_match(one("m1"), "text-decoration:underline;")
  testthat::expect_match(one("m3"), "text-decoration:underline;")
  testthat::expect_match(one("p3"), "text-decoration:underline;")
  testthat::expect_no_match(one("p1"), "text-decoration:underline;")
  testthat::expect_no_match(one("p1"), "font-style:")        # over-cells are bold, never italic
  testthat::expect_match(one("g1"), tx_chrome_hex("print")$grey, fixed = TRUE)
  # the background channel carries NO typography (a fill alone does not bold, in any palette)
  testthat::expect_no_match(one("o2"), "font-")

  # the static baseline rule is still emitted exactly ONCE, outside the cascade
  testthat::expect_length(grep("^\\.p1,\\.p2,.*\\.m4\\{font-weight:bold;\\}$", ln), 1L)
})

testthat::test_that("@media print carries the palette into any coloured page", {
  css <- tab_css(theme = "light", style_tag = FALSE)
  testthat::expect_match(css, "@media print \\{")
  blk <- sub("(?s).*@media print \\{", "", css, perl = TRUE)
  testthat::expect_match(blk, "font-style:italic;")
  # Browsers DROP background-color when printing unless the reader ticks "background graphics" --
  # without this the grey fills silently never reach the paper.
  testthat::expect_match(blk, "print-color-adjust:exact")

  # theme = "print" already IS the palette: re-stating it inside the at-rule would be dead weight.
  pcss <- tab_css(theme = "print", style_tag = FALSE)
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
  p <- strip(tab_html(zz_tab(), theme = "print", tooltips = FALSE))
  l <- strip(tab_html(zz_tab(), theme = "light", tooltips = FALSE))

  # An under-represented cell is NOT bold (it used to be: `ann$bold` was hex-driven, so every coloured
  # cell was bold whatever the palette said) and it IS italic.
  testthat::expect_match(p, '<td class="[^"]*\\bm[0-9]\\b[^"]*"><i>')
  testthat::expect_no_match(p, '<td class="[^"]*\\bm[0-9] tx-b"')
  testthat::expect_match(p, '<td class="[^"]*\\bp[0-9] tx-b"><b>')
  # The markup is what survives GitHub (class+style stripped) and an HTML -> Word paste.
  testthat::expect_true(grepl("<i>", p, fixed = TRUE) && grepl("<b>", p, fixed = TRUE))
  # ... and the colour palettes emit none of it, which is why they stayed byte-identical.
  testthat::expect_false(grepl("<i>", l, fixed = TRUE) || grepl("<b>", l, fixed = TRUE))
})

testthat::test_that("markdown needs no code of its own: the stylesheet carries print", {
  # md cells are bare `[42%]{.m2}` spans and their bold has ALWAYS come from the stylesheet, so the
  # print scheme reaches the CELLS through tab_css() alone -- the pipe grid is byte-identical, and the
  # whole rendering difference lives in the stylesheet (plus the legend, which names the face).
  grid <- function(x) grep("^\\|", strsplit(x, "\n")[[1]], value = TRUE)
  testthat::expect_identical(grid(tab_md(zz_tab(), theme = "print", css = TRUE)),
                             grid(tab_md(zz_tab(), theme = "light", css = TRUE)))
  testthat::expect_match(tab_md(zz_tab(), theme = "print", css = TRUE), "font-style:italic;")
  # the legend follows the palette: its under-side break-words are italic in the raw markdown too
  testthat::expect_match(tab_md(zz_tab(), theme = "print", css = TRUE),
                         "*[-5]{.m1}*", fixed = TRUE)
})

testthat::test_that("Excel writes the face as real font attributes", {
  skip_if_not_installed("openxlsx2")
  fonts_of <- function(theme) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    tab_xl(zz_deep(), path = p, theme = theme, replace = TRUE)
    paste(as.character(openxlsx2::wb_load(p)$styles_mgr$styles$fonts), collapse = " ")
  }
  testthat::expect_match(fonts_of("print"), "<i/>|<i />|<i val")
  testthat::expect_match(fonts_of("print"), "<u/>|<u />|<u val")
  # the byte-identity half: a colour palette emits neither
  testthat::expect_no_match(fonts_of("light"), "<i/>|<i />|<i val")
})

testthat::test_that("the legend wears the same face as the cells it describes", {
  # It MUST, and not only for looks: the html break-word carries an INLINE font-weight, which beats the
  # stylesheet -- so a hex-driven rule would have printed a bold word over italic cells.
  h <- tab_color_legend(zz_tab(), medium = "html", theme = "print", classes = TRUE)
  m <- regmatches(h, regexpr('<span class="m[0-9]"[^>]*>', h))
  testthat::expect_match(m, "font-style:italic;")
  testthat::expect_no_match(m, "font-weight:bold;")
  testthat::expect_match(regmatches(h, regexpr('<span class="p[0-9]"[^>]*>', h)), "font-weight:bold;")

  testthat::expect_true(any(vapply(zz_runs(zz_tab(), "print"),
                                   function(r) isTRUE(r$italic), logical(1))))
  testthat::expect_false(any(vapply(zz_runs(zz_tab(), "light"),
                                    function(r) isTRUE(r$italic), logical(1))))
})

testthat::test_that("the legend names the face, not a colour, and never promises unmade distinctions", {
  pl <- tab_color_legend(zz_tab(), medium = "plain", theme = "print")
  testthat::expect_match(pl, "Bold")
  testthat::expect_match(pl, "Italic")
  testthat::expect_no_match(pl, "Shades of blue")
  testthat::expect_match(tab_color_legend(zz_tab(), medium = "plain", theme = "light"),
                         "Shades of blue")

  # Typography honestly supports 2 levels per side, so slots 1&2 (and 3&4) share a rendering and the
  # legend collapses the repeated break-words -- keeping the LOWER threshold ("bold = at least +5").
  n_breaks <- function(theme)
    sum(vapply(zz_runs(zz_tab(), theme), function(x) !is.na(x$color), logical(1)))
  testthat::expect_lt(n_breaks("print"), n_breaks("light"))
  testthat::expect_match(pl, "+5", fixed = TRUE)
  testthat::expect_no_match(pl, "+10", fixed = TRUE)
})

testthat::test_that("tab_plot renders the print palette without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggpubr")
  testthat::expect_no_error(tab_plot(zz_tab(), theme = "print"))
})

testthat::test_that("a transposed table keeps the print face", {
  h <- gsub("(?s)<style>.*?</style>", "",
            as.character(tab_html(zz_deep(), theme = "print", transpose = TRUE, tooltips = FALSE)),
            perl = TRUE)
  testthat::expect_match(h, "<i>", fixed = TRUE)
})
