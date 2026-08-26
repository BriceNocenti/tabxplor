
# === SECTION: the black-and-white publication palettes ============================================

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
  tab(fx_gss(), marital, race, pct = "row", color = "diff")
}

# A fixture whose cells reach the SECOND typographic level (slots 3/4/7/8 = the underlined ones);
# zz_tab() only ever reaches slot 2, so it cannot exercise the underline.
zz_deep <- function() {
  tab(fx_gss(), relig, race, pct = "row", color = "diff")
}

# medium = "runs" returns one run-LIST per footer stream; flatten to the runs themselves.
zz_runs <- function(x, theme) unlist(tab_color_legend(x, medium = "runs", theme = theme),
                                     recursive = FALSE)


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
  x <- tab(fx_gss(), marital, race, pct = "row", color = "diff", ci = "cell")
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


testthat::test_that('"bw" is a silent alias for "print_minimalistic"', {
  testthat::expect_identical(tab_css(theme = "bw",   style_tag = FALSE),
                             tab_css(theme = "print_minimalistic", style_tag = FALSE))
  testthat::expect_identical(
    withr::with_options(list(tabxplor.theme = "bw"), resolve_export_opts()$theme), "print_minimalistic")
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
  # the colour themes do NOT force an INK -- they follow the page on purpose, `auto` most of all.
  # Their GROUND is stated all the same, as `transparent`: that is what a cell has with no rule at
  # all, but Bootstrap's own value is opaque (`--bs-table-bg` = `--bs-body-bg`) and an opaque cell
  # paints over its row, which is what hid the row hover on the pkgdown site.
  base <- strsplit(tab_css(theme = "light", style_tag = FALSE), "\n")[[1]]
  base <- base[seq_len(which(grepl("^@media print", base))[1] - 1L)]
  cell <- grep("^\\.tabxplor-tab th,\\.tabxplor-tab td\\{", base, value = TRUE)
  testthat::expect_false(any(grepl("[{;]color:#", cell)))   # the INK; border-color is not it
  testthat::expect_true(any(grepl("background-color:transparent", cell, fixed = TRUE)))
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
  t <- tab(fx_gss(), marital, race, pct = "row", color = "diff",
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


testthat::test_that("a transposed table keeps the print face", {
  h <- gsub("(?s)<style>.*?</style>", "",
            as.character(tab_html(zz_deep(), theme = "print_minimalistic", transpose = TRUE, tooltips = FALSE)),
            perl = TRUE)
  testthat::expect_match(h, "<i>", fixed = TRUE)
})


# === SECTION: breaks, palettes and the colour arguments ===========================================

reset_breaks <- function() options("tabxplor.color_breaks" = default_color_scales())


testthat::test_that("mk_color_scale metadata + notation: mirror / signed / list(over,under) / NA", {
  # metadata
  testthat::expect_equal(mk_color_scale("pct_diff",  c(0.1))$center, 0)
  testthat::expect_true (mk_color_scale("pct_diff",  c(0.1))$strict)
  testthat::expect_equal(mk_color_scale("pct_ratio", list(over = 2))$center, 1)
  testthat::expect_false(mk_color_scale("contrib",   c(1))$strict)      # inclusive
  testthat::expect_true (mk_color_scale("mean_diff", NULL)$std)          # NULL -> standardized
  testthat::expect_equal(mk_color_scale("mean_diff", NULL)$over$breaks, c(0.1, 0.2, 0.4, 0.8))
  testthat::expect_false(mk_color_scale("mean_diff", c(200, 500))$std)   # units -> absolute

  # one-sided plain vector auto-mirrors (over == under)
  sc <- mk_color_scale("pct_diff", c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(sc$over$breaks, sc$under$breaks)
  testthat::expect_equal(sc$over$slots, c(1L, 2L, 3L, 4L))

  # signed / reciprocal two-sided vector = as-is, no mirror
  sc2 <- mk_color_scale("pct_diff", c(-0.05, 0.1, 0.2))
  testthat::expect_equal(sc2$under$breaks, 0.05)
  testthat::expect_equal(sc2$over$breaks,  c(0.1, 0.2))
  sc3 <- mk_color_scale("mean_ratio", c(1/1.5, 1/2, 1/4, 1.15, 1.5, 2, 4))
  testthat::expect_equal(sc3$under$breaks, c(1.5, 2, 4))     # reciprocals -> magnitudes
  testthat::expect_equal(sc3$over$breaks,  c(1.15, 1.5, 2, 4))

  # list(over =, under =) -> no mirror; omit a side to switch it off
  ov <- mk_color_scale("pct_ratio", list(over = 2))
  testthat::expect_equal(ov$over$breaks, 2)
  testthat::expect_length(ov$under$breaks, 0L)

  # NA slot-skip on a one-sided vector: non-NA positions ARE the intensities
  na <- mk_color_scale("pct_diff", c(NA, 0.05, 0.1, 0.2))
  testthat::expect_equal(na$over$breaks, c(0.05, 0.1, 0.2))
  testthat::expect_equal(na$over$slots,  c(2L, 3L, 4L))       # intensity 1 skipped
})


testthat::test_that("an empty/NULL scale drops the measure for its column type (except mean_diff)", {
  testthat::expect_length(mk_color_scale("pct_ratio", numeric())$over$breaks, 0L)
  testthat::expect_length(mk_color_scale("mean_ratio", NULL)$over$breaks, 0L)
  # mean_diff NULL is the ONE exception: restores the standardized default (not "off")
  testthat::expect_length(mk_color_scale("mean_diff", NULL)$over$breaks, 4L)
})


testthat::test_that("get_color_breaks(type = 'all') gives the signed / reciprocal engine breaks", {
  reset_breaks()
  # additive mirror c(-x, x) ; multiplicative mirror c(1/x, x); ascending overall
  testthat::expect_equal(get_color_breaks("pct_diff", "all"),
                         c(-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(get_color_breaks("mean_ratio", "all"),
                         c(1/2, 1/1.5, 1/1.2, 1/1.1, 1.1, 1.2, 1.5, 2))
  testthat::expect_equal(get_color_breaks("contrib", "all"), c(-10, -5, -2, -1, 1, 2, 5, 10))
  # Phase 18z4: the absolute standardized-residual scale (color = "contrib" +
  # color_signif = "guaranteed_effect"), written in confidence levels but STORED as plain z.
  testthat::expect_equal(get_color_breaks("zscore", "all"),
                         c(-6, -3.89, -2.58, -1.96, 1.96, 2.58, 3.89, 6))
  testthat::expect_equal(mk_color_scale("zscore", c(2, 3))$center, 0)
  # pct_ratio is the one ASYMMETRIC default: a percentage ratio is capped at 1/base, so the under
  # side is stricter (it enters at the same relative deviation but reaches further).
  testthat::expect_equal(get_color_breaks("pct_ratio", "all"),
                         c(1/4, 1/2, 1/1.25, 1/1.1, 1.1, 1.2, 1.5, 2))
  # odds_ratio (Phase 16c): the dedicated OR scale, symmetric
  testthat::expect_equal(get_color_breaks("odds_ratio", "all"), c(1/4, 1/2, 1/1.5, 1/1.2, 1.2, 1.5, 2, 4))
})


# Phase 19c (KEY 4). The colour cascade used to resolve `color = "auto"` into the LEGACY COMBINED
# string "after_ci" on two paths, and the unresolved "auto" sentinel could then reach set_color().
# Both were live defects, on the DOCUMENTED string spelling of `color = TRUE`:
#   * tab_num(color = "auto", ci = "ref") stored "after_ci" in the `color` attribute, which
#     fmt_color_plan() cannot match against names(MEASURES) -> the table came out UNCOLOURED;
#   * any `color = "auto"` + a `color_signif` policy ABORTED ("Unknown color measure").
# Every assertion below fails on the pre-19c tree.
testthat::test_that("color = 'auto' behaves like color = TRUE, and colours numeric tables", {
  d <- fx_gss()
  col1 <- function(t) t[[names(t)[purrr::map_lgl(t, is_fmt)][1]]]

  # (1) a numeric auto table stores a real measure -- and colours. `ratio` is the declared auto for a
  # numeric column (MEASURES$ratio$auto_for), whatever interval was built: a Glass delta is
  # standardized, so it would stop saying which columns hold the biggest deviations.
  n1 <- col1(tab_num(d, race, c(age, tvhours), ci = "ref"))
  testthat::expect_equal(get_color(n1), "ratio")
  testthat::expect_true(any(fmt_color_channels(n1)$text_slot != 0L))

  # (2) the string "auto" + a policy is exactly the logical TRUE + that policy, both producers
  a <- col1(tab(d, marital, race, pct = "row", color = "auto",
                color_signif = "grey_non_signif"))
  b <- col1(tab(d, marital, race, pct = "row", color = TRUE,
                color_signif = "grey_non_signif"))
  testthat::expect_equal(c(get_color(a), get_color_bg(a), get_color_signif(a)),
                         c(get_color(b), get_color_bg(b), get_color_signif(b)))
  testthat::expect_equal(get_color(a), "difference")
  testthat::expect_equal(get_color_bg(a), "ratio")

  # ... and tab_num() agrees with tab() on the same numeric request
  n2 <- col1(tab_num(d, race, c(age, tvhours), ci = "ref", color_signif = "grey_non_signif"))
  n3 <- col1(tab(d, race, c(age, tvhours), color = TRUE, ci = "ref",
                 color_signif = "grey_non_signif"))
  testthat::expect_equal(get_color(n2), get_color(n3))
  testthat::expect_equal(fmt_color_channels(n2)$text_slot, fmt_color_channels(n3)$text_slot)

  # (3) `color = "auto"` IS `color = TRUE`: one request, one spec, both channels
  p <- col1(tab(d, marital, race, pct = "row", color = "auto"))
  testthat::expect_equal(get_color(p), "difference")
  testthat::expect_equal(get_color_bg(p), "ratio")
  testthat::expect_equal(attributes(p), attributes(col1(tab(d, marital, race, pct = "row",
                                                            color = TRUE))))
  # ... while a positional c("auto", <bg>) keeps its explicit background
  q <- col1(tab(d, marital, race, pct = "row", color = c("auto", "difference")))
  testthat::expect_equal(c(get_color(q), get_color_bg(q)), c("difference", "difference"))
})


testthat::test_that("old combined colour strings are soft-deprecated but still colour", {
  d <- fx_gss()
  for (m in c("diff_ci", "after_ci", "ci")) {
    lifecycle::expect_deprecated(tab(d, marital, race, pct = "row", ci = "ref", color = m))
  }
  withr::local_options(lifecycle_verbosity = "quiet")
  t  <- tab(d, marital, race, pct = "row", color = "diff_ci")
  fc <- t[[which(purrr::map_lgl(t, is_fmt))[2]]]
  testthat::expect_true(any(fmt_color_channels(fc)$text_slot != 0L))
  testthat::expect_no_condition(
    tab(d, marital, race, pct = "row", color = "diff", color_signif = "grey_non_signif"),
    class = "lifecycle_warning_deprecated"
  )
})


# --- COMPAT (Phase 13a): deprecated colour surfaces degrade with no error, mapped to the new API ---
testthat::test_that("deprecated colour arguments / functions are wired, not errors", {
  withr::defer({
    tabxplor_palette_env$base <- default_palette_base(); build_palettes()
    options("tabxplor.color_breaks" = default_color_scales(),
            "tabxplor.color_style_theme" = "light")
  })
  d <- fx_gss()

  # set_color_style() -> options + set_color_palette(), with a soft-deprecation. Phase 14l dropped its
  # `tabxplor.color_style_type` WRITE (that option is deprecated), so only the theme half survives.
  lifecycle::expect_deprecated(set_color_style(type = "bg", theme = "dark"))
  testthat::expect_equal(getOption("tabxplor.color_style_theme"), "dark")
  withr::local_options(lifecycle_verbosity = "quiet")
  set_color_style(type = "text", custom_palette = sprintf("#%06X", seq_len(11) * 1000L))
  testthat::expect_length(get_color_style("color_code", type = "text", theme = "light"), 8L)
  tabxplor_palette_env$base <- default_palette_base(); build_palettes()

  # color = c(text =, background =) -> positional channels
  lifecycle::expect_deprecated(
    tt <- tab(d, race, marital, pct = "row", color = c(text = "diff", background = "ratio")))
  testthat::expect_equal(unname(fmt_color_attr(tt$Married)), c("difference", "ratio"))

  # color_signif = "color_all_signif" -> "guaranteed_effect"
  lifecycle::expect_deprecated(
    g <- tab(d, race, marital, pct = "row", color = "diff", color_signif = "color_all_signif"))
  testthat::expect_equal(get_color_signif(g$Married), "guaranteed_effect")

  # set_color_breaks(pct_breaks =) -> pct_diff (<=1) + pct_ratio (>1)
  lifecycle::expect_deprecated(set_color_breaks(pct_breaks = c(0.05, 0.1, 0.2, 2, 0.3)))
  testthat::expect_equal(getOption("tabxplor.color_breaks")$pct_diff$over$breaks, c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(getOption("tabxplor.color_breaks")$pct_ratio$over$breaks, 2)

  # inert html_24_bit is absorbed, not an error
  testthat::expect_no_error(get_color_style("color_code", type = "text", html_24_bit = "blue_red"))
  testthat::expect_no_error(fmt_get_color_code(g$Married, html_24_bit = "blue_red"))
})


# --- Phase 14l: the `color_type` argument / option are deprecated + inert ------------------------
testthat::test_that("color_type is deprecated on every exporter and does nothing", {
  d  <- fx_gss()
  tb <- tab(d, marital, race, pct = "row", color = TRUE)

  # each of the 6 public surfaces warns once when color_type is explicitly passed
  lifecycle::expect_deprecated(tab_html(tb, color_type = "bg"))
  lifecycle::expect_deprecated(tab_kable(tb, color_type = "bg"))
  lifecycle::expect_deprecated(tab_md(tb, color_type = "bg", print = FALSE))
  lifecycle::expect_deprecated(tab_css(color_type = "bg"))
  lifecycle::expect_deprecated(tab_export(tb, "md", color_type = "bg", print = FALSE))
  p <- withr::local_tempfile(fileext = ".xlsx")
  lifecycle::expect_deprecated(suppressMessages(
    tab_xl(tb, color_type = "bg", path = p, open = FALSE, replace = TRUE)))
})


testthat::test_that("color_type is INERT: tab_css output is byte-identical with or without it", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # tab_css is a pure function of (palette, theme), so byte-equality is the strongest inert proof.
  testthat::expect_identical(tab_css(color_type = "bg", style_tag = FALSE),
                             tab_css(style_tag = FALSE))
})


testthat::test_that("color_type default is no longer a literal that warns", {
  testthat::skip_if_not_installed("openxlsx2")
  # tab_xl's default was the literal "text" (the one exporter that ignored the option). Now the
  # sentinel: a plain call must NOT warn about color_type.
  tb <- tab(fx_gss(), marital, race, pct = "row", color = TRUE)
  p  <- withr::local_tempfile(fileext = ".xlsx")
  n  <- 0
  withCallingHandlers(
    suppressMessages(tab_xl(tb, path = p, open = FALSE, replace = TRUE)),
    warning = function(w) { if (grepl("color_type", conditionMessage(w))) n <<- n + 1
                            invokeRestart("muffleWarning") })
  testthat::expect_equal(n, 0L)
})


testthat::test_that("the deleted color_style_type option is not read at all", {
  # Phase 20a: the option is GONE (never seeded, read only to warn about itself), so setting it must
  # change nothing and say nothing -- the colour CHANNEL is `color = c(text, background)`.
  withr::with_options(list(tabxplor.color_style_type = "bg"), {
    a <- get_color_style("color_code", theme = "light")
    b <- get_color_style("color_code", type = "text", theme = "light")
    testthat::expect_identical(a, b)
    testthat::expect_no_warning(get_color_style("color_code"))
  })
})


testthat::test_that("the color_type=bg_legend latent abort can't be reached via the option", {
  withr::local_options(lifecycle_verbosity = "quiet")
  tb <- tab(fx_gss(), marital, race, pct = "row", color = c("diff", "ratio"))
  # the option used to reach get_color_style("crayon", type = "bg_legend") via legend_render_line's
  # unvalidated fam("text"); hard-wiring "text" closes it. The DIRECT abort still stands (below).
  withr::with_options(list(tabxplor.color_style_type = "bg_legend"), {
    testthat::expect_no_error(print(tb))
    testthat::expect_no_error(tab_color_legend(tb, medium = "console"))
  })
  testthat::expect_error(get_color_style("crayon", type = "bg_legend"), "bg_legend")
})


# --- Phase 14a: a color_signif policy forces the difference CI it gates on ----------------------
# normalize_color_spec() can only fold the policy into the legacy colour string for an EXPLICIT
# diff/ratio measure; `color = TRUE`/"auto" must reach tab_resolve_settings() as "auto" (it
# dispatches per column type), so the policy could not ride the string -> ci stayed "no" ->
# fmt_color_plan()'s gate saw NA bounds -> EVERY cell grey, on the DEFAULT color = TRUE.

testthat::test_that("color = TRUE + a color_signif policy computes the difference CI", {
  for (pol in c("grey_non_signif", "guaranteed_effect")) {
    t <- tab(fx_gss(), race, marital, pct = "row", color = TRUE, color_signif = pol)
    fmt_cols <- t[purrr::map_lgl(t, is_fmt)]
    testthat::expect_true(any(!is.na(unlist(purrr::map(fmt_cols, get_ci_sup)))), label = pol)
    testthat::expect_true(any(get_scale(t) == "points"), label = pol)
  }
})


# Phase 19d / D28: `ci = "cell"` beside a policy INFORMS and disables -- one rule, both paths.
# It used to abort for `color_signif` and silently drop the stars.
testthat::test_that("an explicit ci = 'cell' with a color_signif policy informs and disables", {
  tabxplor:::tx_reset_messages()   # the note is once per session
  testthat::expect_message(
    t <- tab(fx_gss(), race, marital, pct = "row", color = TRUE, ci = "cell",
             color_signif = "grey_non_signif"),
    "cell"
  )
  testthat::expect_equal(get_color_signif(t$Married), "ignore")
  # ... but ci = "cell" is fine without a policy
  testthat::expect_no_error(
    tab(fx_gss(), race, marital, pct = "row", color = TRUE, ci = "cell"))
})


# Phase 19d-tail: `ci = "no"` is the OTHER value with nothing to test, and it is now the SAME rule --
# the anchor the user typed wins, the consumers that would read an interval are informed and
# disabled. It used to be answered the opposite way, and in two places that disagreed: the pipeline
# resolver silently upgraded an explicit "no" to "ref", the leaf resolver did not. So `tab()` built an
# interval where `tab_num()` built none, and the jamovi tuple recorded a `ci` its carrier contradicted.
testthat::test_that("an explicit ci = 'no' informs and disables, on both resolvers", {
  gss <- fx_gss()
  tabxplor:::tx_reset_messages()   # the note is once per session
  testthat::expect_message(
    t <- tab(gss, race, marital, pct = "row", color = TRUE, ci = "no",
             color_signif = "grey_non_signif"),
    'ci = "no"'
  )
  testthat::expect_equal(get_color_signif(t$Married), "ignore")
  testthat::expect_true(all(is.na(get_ci_inf(t$Married))))       # the user said no interval

  tabxplor:::tx_reset_messages()   # the note is once per session
  testthat::expect_message(
    s <- tab(gss, race, marital, pct = "row", ci = "no", stars = TRUE), 'ci = "no"')
  testthat::expect_true(all(is.na(get_ci_inf(s$Married))))

  # the numeric leaf, called directly, agrees cell for cell -- that is the whole point of one rule
  tabxplor:::tx_reset_messages()   # the note is once per session
  testthat::expect_message(
    n <- tab_num(gss, race, tvhours, ci = "no", stars = TRUE), 'ci = "no"')
  testthat::expect_true(all(is.na(get_ci_inf(n$tvhours))))

  # `ci = "auto"` is untouched: it still resolves to the reference interval when something reads it
  a <- tab(gss, race, marital, pct = "row", stars = TRUE)
  testthat::expect_false(all(is.na(get_ci_inf(a$Married))))
})


# === SECTION: the black-and-white publication palettes ============================================

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
  tab(fx_gss(), marital, race, pct = "row", color = "diff")
}


# A fixture whose cells reach the SECOND typographic level (slots 3/4/7/8 = the underlined ones);
# zz_tab() only ever reaches slot 2, so it cannot exercise the underline.
zz_deep <- function() {
  tab(fx_gss(), relig, race, pct = "row", color = "diff")
}


# medium = "runs" returns one run-LIST per footer stream; flatten to the runs themselves.
zz_runs <- function(x, theme) unlist(tab_color_legend(x, medium = "runs", theme = theme),
                                     recursive = FALSE)


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

  # The legend's font stand-in for the fills (an Excel run cannot fill).
  testthat::expect_true(all(zz_contrast(get_color_style("color_code", "bg_legend", "print_minimalistic"),
                                        "#FFFFFF") >= 4.5))
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


# === SECTION: breaks, palettes and the colour arguments ===========================================

reset_breaks <- function() options("tabxplor.color_breaks" = default_color_scales())


testthat::test_that("set_color_breaks() sets named scales, keeps the others (list or ... form)", {
  withr::defer(reset_breaks())
  reset_breaks()
  set_color_breaks(list(pct_diff = c(0.05, 0.15, 0.3), pct_ratio = list(over = 3)))
  cur <- getOption("tabxplor.color_breaks")
  testthat::expect_equal(cur$pct_diff$over$breaks, c(0.05, 0.15, 0.3))
  testthat::expect_equal(cur$pct_ratio$over$breaks, 3)
  testthat::expect_length(cur$pct_ratio$under$breaks, 0L)         # over-only -> no under
  testthat::expect_equal(cur$mean_ratio$over$breaks, c(1.1, 1.2, 1.5, 2))  # untouched (symmetric default)
  # the `...` named-scale form works too
  set_color_breaks(contrib = c(1, 2, 5))
  testthat::expect_equal(getOption("tabxplor.color_breaks")$contrib$over$breaks, c(1, 2, 5))
})


testthat::test_that("get_color_breaks returns a readable form and round-trips", {
  withr::defer(reset_breaks())
  reset_breaks()
  gb <- get_color_breaks()
  # Phase 18z5 added adj_ratio / adj_diff (the `adjustment` / `between_groups` measures); z13 added
  # adj_diff_std, the SD-standardized additive one (an arbitrary-unit outcome).
  testthat::expect_named(gb, c("pct_diff", "pct_ratio", "odds_ratio", "mean_diff", "mean_ratio",
                               "contrib", "zscore", "adj_ratio", "adj_diff", "adj_diff_std"))
  testthat::expect_equal(gb$pct_diff, c(0.05, 0.1, 0.2, 0.3))    # symmetric -> plain magnitudes
  testthat::expect_equal(gb$pct_ratio, list(over = c(1.1, 1.2, 1.5, 2),
                                            under = c(1.1, 1.25, 2, 4)))   # asymmetric -> both sides
  testthat::expect_equal(get_color_breaks("pct"),  c(0.05, 0.1, 0.2, 0.3))   # old alias
  testthat::expect_equal(get_color_breaks("mean"), c(1.1, 1.2, 1.5, 2))       # mean_ratio symmetric default
  # round-trips through set_color_breaks()
  set_color_breaks(get_color_breaks())
  testthat::expect_equal(get_color_breaks()$pct_diff, c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(get_color_breaks("pct_ratio"),
                         list(over = c(1.1, 1.2, 1.5, 2), under = c(1.1, 1.25, 2, 4)))
  # LOSSLESS: an NA slot-skip and a standardized `std` survive the round trip, so a call that looks
  # like a no-op cannot move a tint or turn a Glass delta ladder into an absolute one.
  set_color_breaks(pct_ratio = c(NA, 1.2, 1.5, 2))
  set_color_breaks(get_color_breaks())
  testthat::expect_equal(getOption("tabxplor.color_breaks")$pct_ratio$over$slots, 2:4)
  reset_breaks()
  testthat::expect_true(getOption("tabxplor.color_breaks")$mean_diff$std)
  set_color_breaks(get_color_breaks())
  testthat::expect_true(getOption("tabxplor.color_breaks")$mean_diff$std)
  # ... and the internal canonical shape is accepted too
  set_color_breaks(default_color_scales())
  testthat::expect_equal(getOption("tabxplor.color_breaks")[names(default_color_scales())],
                         default_color_scales())
})


testthat::test_that("tab() color argument errors are clear", {
  d <- fx_gss()
  testthat::expect_error(tab(d, marital, race, pct = "row", color = "diff", color_signif = "nope"),
                         "Unknown")
  testthat::expect_error(tab(d, marital, race, pct = "row", color = c("diff", "contrib")),
                         "background channel")
  testthat::expect_error(tab(d, marital, race, pct = "row", color = c(sex = "diff")),
                         "column type")   # unknown type key
})


testthat::test_that("two-channel colour: background channel renders independently of text", {
  withr::local_options(lifecycle_verbosity = "quiet")
  d <- fx_gss()
  col2 <- function(t) t[[which(purrr::map_lgl(t, is_fmt))[2]]]

  tt <- col2(tab(d, marital, race, pct = "row", color = TRUE))    # diff text + ratio bg
  testthat::expect_true(any(!is.na(fmt_channel_codes(tt, "light")$text)))

  # a lone diff measure on the background: text empty, background coloured
  bgo    <- col2(tab(d, marital, race, pct = "row", color = c("", "diff")))
  codesb <- fmt_channel_codes(bgo, "light")
  testthat::expect_true(all(is.na(codesb$text)))
  testthat::expect_true(any(!is.na(codesb$bg)))

  both <- col2(tab(d, marital, race, pct = "row", color = c("diff", "diff")))
  cb   <- fmt_channel_codes(both, "light")
  testthat::expect_true(any(!is.na(cb$text)))
  testthat::expect_true(any(!is.na(cb$bg)))
})


testthat::test_that("set_color_palette validates and rebuilds the palettes", {
  # reset the base palette to the OKLCH defaults (set_color_palette() accumulates, so re-seed the env)
  withr::defer({ tabxplor_palette_env$base <- default_palette_base(); build_palettes() })
  set_color_palette(text_colors = c("#111111", "#222222", "#333333", "#444444"))
  sty <- get_color_style("color_code", type = "text", theme = "light")
  testthat::expect_length(sty, 8L)                    # 4 over + 4 under
  testthat::expect_equal(toupper(sty[1]), "#111111")  # over intensity 1 overridden
  testthat::expect_error(set_color_palette(text_colors = c("#111", "#222")), "4 hex")  # wrong length
})


testthat::test_that("per-table color_breaks overrides the global at render", {
  d <- fx_gss()
  t_ov <- tab(d, race, marital, pct = "row", color = "diff",
              color_breaks = list(pct_diff = c(0.01, 0.02, 0.03)))
  testthat::expect_false(is.null(get_color_breaks_attr(t_ov)))   # Phase 17b: now in meta$color_breaks
  st <- push_color_breaks(t_ov)
  on.exit(pop_color_breaks(st), add = TRUE)
  # with tiny breaks, a strong cell reaches the top intensity (slot 4 over / 8 under)
  slots <- fmt_color_channels(t_ov$Married)$text_slot
  testthat::expect_true(any(slots %in% c(4L, 8L)))
  pop_color_breaks(st)
  # after pop the global default is restored (no top-intensity flooding on the default breaks)
  testthat::expect_equal(getOption("tabxplor.color_breaks")$pct_diff$over$breaks, c(0.05, 0.1, 0.2, 0.3))
})


testthat::test_that("an implicit color_signif CI == the explicit ci = 'diff' table", {
  # the user should not have to write ci = "ref" to get what color_signif asks for
  for (pol in c("grey_non_signif", "guaranteed_effect")) {
    for (cv in list(rlang::expr(marital), rlang::expr(tvhours), rlang::expr(c(marital, tvhours)))) {
      a <- tab(fx_gss(), race, !!cv, pct = "row", color = TRUE, color_signif = pol)
      b <- tab(fx_gss(), race, !!cv, pct = "row", color = TRUE, ci = "ref",
               color_signif = pol)
      testthat::expect_equal(a, b)
    }
  }
})


testthat::test_that("color_signif = 'ignore' does NOT force a CI", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = TRUE)
  fmt_cols <- t[purrr::map_lgl(t, is_fmt)]
  testthat::expect_true(all(is.na(unlist(purrr::map(fmt_cols, get_ci_sup)))))
})
