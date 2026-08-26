# PURPOSE: the palettes -- the colour ramps, the publication grids, and the breaks API.
# ROLE: the shipped CONTRACT for R/tab-palettes.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

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




# === SECTION: breaks, palettes and the colour arguments ===========================================

reset_breaks <- function() options("tabxplor.color_breaks" = default_color_scales())



testthat::test_that("set_color_breaks validates its input with clear errors", {
  withr::defer(reset_breaks())
  testthat::expect_error(set_color_breaks(list(pct_ratio = c(1))), "cannot equal 1")   # the neutral
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(0))), "cannot equal 0")
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(0.2, 0.1))), "strictly increasing")
  testthat::expect_error(set_color_breaks(list(nonsense  = c(1))), "Unknown color-break scale")
  testthat::expect_error(set_color_breaks(list(pct_diff  = "a")), "must be numeric")
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(0.01, 0.02, 0.03, 0.04, 0.05))),
                         "at most 4")
  testthat::expect_error(set_color_breaks(list(1, 2)), "must be named")          # unnamed
  # NA slot-skip is only allowed on a one-sided vector, not a two-sided one
  testthat::expect_error(set_color_breaks(list(pct_diff = c(-0.05, NA, 0.1))), "one-sided")
})



# --- the tab() color / color_signif argument grammar (position = channel, names = type) ---
testthat::test_that("tab() color argument forms set the right channels + policy", {
  d <- fx_gss()
  col1 <- function(t) t[[names(t)[purrr::map_lgl(t, is_fmt)][1]]]

  s <- col1(tab(d, marital, race, pct = "row", color = "diff"))          # scalar -> text only
  testthat::expect_equal(get_color(s), "difference")
  testthat::expect_true(is.na(get_color_bg(s)))

  tt <- col1(tab(d, marital, race, pct = "row", color = TRUE))           # per-type: factor -> diff + ratio
  testthat::expect_equal(get_color(tt), "difference")
  testthat::expect_equal(get_color_bg(tt), "ratio")

  v <- col1(tab(d, marital, race, pct = "row", color = c("diff", "ratio")))  # positional channels
  testthat::expect_equal(c(get_color(v), get_color_bg(v)), c("difference", "ratio"))

  pt <- col1(tab(d, marital, race, pct = "row", color = c(pct = "ratio")))   # per type (pct)
  testthat::expect_equal(get_color(pt), "ratio")

  lst <- col1(tab(d, marital, race, pct = "row",
                  color = list(pct = c("diff", "ratio"), mean = "ratio")))   # list per type
  testthat::expect_equal(c(get_color(lst), get_color_bg(lst)), c("difference", "ratio"))

  g <- col1(tab(d, marital, race, pct = "row", color = "diff", color_signif = "grey_non_signif"))
  testthat::expect_equal(get_color_signif(g), "grey_non_signif")

  off <- col1(tab(d, marital, race, pct = "row", color = FALSE))
  testthat::expect_equal(get_color(off), "")

  cnt <- col1(tab(d, marital, race, pct = "no", color = TRUE))           # counts -> contrib
  testthat::expect_equal(get_color(cnt), "contrib")
})



# Phase 19c: the vocabulary IS the MEASURES / COLOR_SCALES tables. These lock the accessors every
# consumer reads, so a row added with a missing field fails here rather than in a rendered legend.
testthat::test_that("the colour vocabulary is declared, not written out", {
  # MEASURE_COLOR_KEYS is the allow-list: MEASURES + the shared acronyms + the legacy spellings
  testthat::expect_setequal(names(MEASURES),
                            c("difference", "ratio", "odds_ratio", "contrib", "adjustment", "between_groups"))
  testthat::expect_equal(measure_key("OR"), "odds_ratio")
  testthat::expect_equal(measure_key("odds_ratio"), "odds_ratio")
  testthat::expect_equal(measure_key("after_ci"), "difference")   # an alias resolves to its measure
  testthat::expect_equal(measure_key("no"), "")
  testthat::expect_true(is.na(measure_key("nonesuch")))
  testthat::expect_equal(measure_key(NA_character_), "")
  testthat::expect_equal(measure_key(character(0)), "")

  # Phase 22c-v: ONE acronym vocabulary, so every spelling `tab_reg(measure =)` takes and every word
  # a header can print works here too -- with its DERIVED lowercase twin.
  testthat::expect_equal(vapply(c("RD", "diff", "rd", "RR", "IRR", "RoM", "rr", "irr", "rom", "or"),
                                measure_key, character(1), USE.NAMES = FALSE),
                         c("difference", "difference", "difference", "ratio", "ratio", "ratio",
                           "ratio", "ratio", "ratio", "odds_ratio"))
  # ...but NOT the three only a model estimates, and they are refused BY NAME, not as unknown
  testthat::expect_true(all(is.na(vapply(c("cumOR", "D", "WR"), measure_key, character(1)))))
  testthat::expect_error(tab(fx_gss(), race, marital, pct = "row", color = "cumOR"),
                         "tab_reg")
  # a one-letter acronym gets no lowercase twin -- `d` is a slip, not a spelling
  testthat::expect_true(is.na(measure_key("d")))
  # a stored colour attribute is ALWAYS canonical, whichever spelling was typed
  testthat::expect_equal(
    get_color(tab(fx_gss(), race, marital, pct = "row", color = "RR")[[3]]), "ratio")

  # who may NAME a measure is a declared fact, distinct from who can BUILD one
  testthat::expect_setequal(measure_nameable("tab"),
                            c("difference", "ratio", "odds_ratio", "contrib"))
  testthat::expect_setequal(measure_nameable("reg"), c("adjustment", "between_groups"))
  testthat::expect_setequal(measure_nameable("tab", channel = "bg"), c("difference", "ratio"))

  # the build classes: diff and ratio share one (the leaf computes both fields together)
  testthat::expect_equal(measure_builds("ratio"), measure_builds("difference"))
  testthat::expect_equal(measure_builds("contrib"), "contrib")
  # 19l: measure_stage() is gone -- it wrapped exactly this test and named its answer after the
  # chi2 STEP that 19j deleted. The contribution pass is still a separate computation in the leaf.
  testthat::expect_false(identical(measure_builds("difference"), "contrib"))

  # what each measure declares it needs
  testthat::expect_true(measure_forces("contrib", "chi2"))
  testthat::expect_true(measure_forces("contrib", "totrow"))
  testthat::expect_false(measure_forces("difference", "ci"))              # not gated -> no interval forced
  testthat::expect_true(measure_forces("difference", "ci", gated = TRUE))
  testthat::expect_true(measure_forces("odds_ratio", "ci", gated = TRUE))  # the leaf owns the Woolf bounds
  testthat::expect_true(measure_forces("adjustment", "empirical"))

  # where each may go, and who may ask for it
  testthat::expect_false("bg" %in% MEASURES$contrib$channels)
  testthat::expect_true("bg" %in% MEASURES$adjustment$channels)
  testthat::expect_equal(MEASURES$adjustment$producers, "reg")
  testthat::expect_false(measure_applies("contrib", "num"))
  testthat::expect_true(measure_applies("ratio", "num"))

  # the `color = TRUE` defaults, one table for the three cascades that used to answer separately
  testthat::expect_equal(measure_auto("pct", "text"), "difference")
  testthat::expect_equal(measure_auto("pct", "bg"),   "ratio")
  testthat::expect_equal(measure_auto("num", "text"), "ratio")
  testthat::expect_equal(measure_auto("counts", "text"), "contrib")
  testthat::expect_equal(measure_auto("reg_ratio", "text"), "odds_ratio")

  # every scale states its own geometry; a derived one names its parent
  testthat::expect_equal(COLOR_SCALES$pct_ratio$center, 1)
  testthat::expect_equal(COLOR_SCALES$pct_diff$center, 0)
  testthat::expect_false(COLOR_SCALES$contrib$strict)
  testthat::expect_true(COLOR_SCALES$adj_diff_std$std)
  testthat::expect_equal(COLOR_SCALES$log_odds$derive$from, "odds_ratio")
  testthat::expect_false(isTRUE(COLOR_SCALES$log_odds$settable))
  testthat::expect_error(mk_color_scale("log_odds", 2), "Unknown color-break scale")
})



# The ladders' SHAPE is declared and checked at load (tx_check_color_scales() runs at the bottom of
# R/tab_classes.R), so a drifting default fails the install rather than a user's table.
testthat::test_that("every ladder declares its quantity, anchor and sides -- and keeps the shape", {
  testthat::expect_true(tx_check_color_scales())
  for (nm in color_scale_names()) {
    r <- COLOR_SCALES[[nm]]
    testthat::expect_true(r$quantity %in% COLOR_QUANTITIES, info = nm)
    testthat::expect_true(r$sides %in% c("mirror", "asymmetric"), info = nm)
    testthat::expect_true(nzchar(r$anchor), info = nm)
  }
  # `pct_ratio` is the one asymmetric ladder, and the ONE reason is the ceiling: a percentage ratio
  # cannot exceed 1 / base, so a cell reaches much further below its reference than above it.
  testthat::expect_equal(COLOR_SCALES$pct_ratio$sides,  "asymmetric")
  testthat::expect_equal(COLOR_SCALES$mean_ratio$sides, "mirror")
  # the background keeps a ladder's loud rungs only -- declared, and only on the ratio scales
  testthat::expect_equal(COLOR_SCALES$pct_ratio$bg_keep, 2L)
  testthat::expect_null(COLOR_SCALES$pct_diff$bg_keep)

  # the check REFUSES a ladder off the grid, so it cannot be re-guessed
  bad <- COLOR_SCALES; bad$pct_diff$default <- c(0.05, 0.06, 0.2, 0.3)
  testthat::expect_error(tx_check_color_scales(bad), "off the shape rule")
  bad2 <- COLOR_SCALES; bad2$mean_ratio$sides <- "asymmetric"
  testthat::expect_error(tx_check_color_scales(bad2), "sides")
  bad3 <- COLOR_SCALES; bad3$pct_ratio$quantity <- NULL
  testthat::expect_error(tx_check_color_scales(bad3), "quantity")
})



testthat::test_that("contrib / OR never get a difference CI forced on them", {
  # contrib has no difference CI (documented gap)
  t <- tab(fx_gss(), race, marital, color = "contrib", color_signif = "grey_non_signif")
  testthat::expect_false(any(get_scale(t) == "points"))

  # OR is pct = "row", so it matches the diff-family predicate -- but it carries its OWN ci_type =
  # "or" bounds (centre 1). Forcing a difference CI (centre 0) would have its inf tested against the
  # OR neutral 1 -> never significant -> the policy would grey the WHOLE table.
  o <- tab(fx_gss(), marital, race, pct = "col", display = "{or}", ref = "first", color = TRUE,
           color_signif = "grey_non_signif")
  testthat::expect_false(any(get_scale(o) == "points"))
})
