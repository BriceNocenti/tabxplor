# PURPOSE: Phase 14h -- one digit-width space wherever numbers must line up.
# ROLE: locks the four alignment fixes: the thousands mark follows format()'s `pad` (it was a
#        hard-coded ASCII space, half a digit wide in a proportional font and collapsed by CSS);
#        a mean cell with no sd is padded to the "(sigma sd)" tail so the means align; a bold row
#        bolds only the mean of a "mean (sigma sd)" cell; the Excel star pad uses the same glyph.
# KEY CONSTRAINT: the CONSOLE is read in a MONOSPACE font, where an ASCII space is already exactly one
#        digit wide -- it keeps ASCII (format()'s default pad). html/Excel get the figure space.
#        Phase 14m-ii reopened this FOR MARKDOWN's value-internal padding only: a pandoc-rendered md
#        table lands in the host's PROPORTIONAL font, so tab_md() now pads INSIDE a value with a figure
#        space (its cell-edge + spacer padding stays ASCII -- see test-render-html.R).
# See: CLAUDE.md > Phase 14h + 14m-ii.

fig <- stringi::stri_unescape_unicode("\\u2007")   # FIGURE SPACE, one digit wide
sig <- stringi::stri_unescape_unicode("\\u03c3")   # sigma
nbs <- stringi::stri_unescape_unicode("\\u202f")   # Phase 14x: the OLD mean/sd joiner -- must be GONE
                                                   # now (replaced by `pad`: ASCII in console, fig in md/html)

# === the thousands mark follows `pad` =============================================

testthat::test_that("format(): the thousands mark is the pad glyph, per medium", {
  x <- fmt(n = c(849L, 3648L, 1811L), display = "n")

  # console / markdown: ASCII, as before
  con <- format(x)
  testthat::expect_identical(con, c("849", "3 648", "1 811"))
  testthat::expect_false(any(grepl(fig, con, fixed = TRUE)))

  # html: the figure space, and NO ascii space left to be collapsed by CSS
  h <- format(x, html = TRUE)
  testthat::expect_identical(h, c("849", paste0("3", fig, "648"), paste0("1", fig, "811")))
  testthat::expect_false(any(grepl(" ", h, fixed = TRUE)))

  # an explicit pad wins over both (the lever tab_xl() uses)
  testthat::expect_identical(format(x, pad = fig), h)
})

testthat::test_that("format(): a composite's mark and its padding are the SAME glyph", {
  # the reported bug: "100% (n=  849)" was padded with figure spaces while "(n=1 811)" separated
  # with an ASCII space -- so the digits the padding had just aligned fell out of line again.
  x <- fmt(n = c(849L, 3648L), pct = c(1, 1), scale = "level_pct", pct_type = "row", display = "{pct} (n={n})")
  h <- format(x, html = TRUE)
  # Phase g (A6): the html/nbsp medium joins the template literal " (n=" with a NON-BREAKING space so
  # the composite does not wrap; the inner digits keep the figure-space pad.
  nb <- intToUtf8(160L)
  testthat::expect_identical(h, c(paste0("100%", nb, "(n=", fig, fig, "849)"),
                                  paste0("100%", nb, "(n=3", fig, "648)")))
  testthat::expect_identical(length(unique(nchar(h))), 1L)   # one width -> aligned
  # markdown keeps ASCII on both counts
  testthat::expect_identical(format(x), c("100% (n=  849)", "100% (n=3 648)"))
})

# === mean (sigma sd): the sd-less cell is padded ==================================
# Phase 22c-iii: the sd tail is an ORDINARY COMPOSITE now (`{mean} (sigma{sd})`, the `mean_sd`
# preset) -- format() has no mean-specific branch left, and the generic per-token padding does the
# work. It aligns the MEANS too, which the hand-rolled tail never did.

mean_col <- function(digits = 1L) {
  fmt(mean = c(1.0, 1.7, 10.25), var = c(NA, 2.1^2, 3^2), n = rep(5L, 3),
      display = DISPLAY_PRESETS$mean_sd$template, scale = "level_mean", digits = digits)
}

testthat::test_that("format(): a mean with no sd is padded to the tail, so the means align", {
  f <- format(mean_col(), special_formatting = TRUE)
  testthat::expect_identical(f, c(paste0(" 1.0", strrep(" ", 7)),
                                  paste0(" 1.7 (", sig, "2.1)"),
                                  paste0("10.2 (", sig, "3.0)")))
  testthat::expect_false(any(grepl(nbs, f, fixed = TRUE)))
  # what alignment MEANS here: every cell is the same width AND the means occupy the same columns,
  # so the decimal points line up whether or not a cell has an sd.
  testthat::expect_identical(length(unique(nchar(f))), 1L)
  testthat::expect_identical(unique(substr(f, 5, 5)), " ")
})

testthat::test_that("format(): the sd-less pad follows `pad` (figure space in html)", {
  h <- format(mean_col(), special_formatting = TRUE, html = TRUE)
  testthat::expect_identical(h[1], paste0(fig, "1.0\u00a0", strrep(fig, 6)))
  testthat::expect_false(grepl(" ", h[1], fixed = TRUE))
  testthat::expect_identical(h[2], paste0(fig, "1.7\u00a0(", sig, "2.1)"))
  testthat::expect_false(any(grepl(nbs, h, fixed = TRUE)))
})

testthat::test_that("format(): an EMPTY mean cell stays NA -- it is not padded", {
  # REGRESSION: an empty cell also has an NA var, so the sd-less mask caught it and pasted onto the
  # NA -> the literal string "NA" + spaces. Only `na` (which kable/md pass as "") hid it; the
  # console, which keeps NA, printed "NA       ".
  x <- fmt(mean = c(1.0, NA, 2.5), var = c(NA, NA, 4), n = c(5L, 0L, 5L),
           display = DISPLAY_PRESETS$mean_sd$template, scale = "level_mean", digits = 1L)
  f <- format(x, special_formatting = TRUE)
  testthat::expect_true(is.na(f[2]))
  testthat::expect_false(any(grepl("NA", f[!is.na(f)], fixed = TRUE)))
  testthat::expect_identical(format(x, special_formatting = TRUE, na = "")[2], "")
})

testthat::test_that("format(): a mean column with no sd at all is untouched", {
  # the whole `(sigma{sd})` group is void down the column, so it leaves the template entirely
  x <- fmt(mean = c(1.0, 2.0), var = c(NA, NA), n = c(5L, 5L),
           display = DISPLAY_PRESETS$mean_sd$template, scale = "level_mean", digits = 1L)
  testthat::expect_identical(format(x, special_formatting = TRUE), c("1.0", "2.0"))
})

# === bold_split reaches the mean/sd cell ==========================================

testthat::test_that("format(bold_split): only the MEAN of a mean (sd) cell is the bold prefix", {
  b  <- format(mean_col(), special_formatting = TRUE, bold_split = TRUE)
  pn <- attr(b, "primary_nchar")
  testthat::expect_identical(pn, c(4L, 4L, 4L))
  # the prefix is exactly the mean -> the "(sigma sd)" tail stays plain in a bold row
  testthat::expect_identical(trimws(as.character(substr(b, 1, pn))), c("1.0", "1.7", "10.2"))
})

testthat::test_that("format(): primary_nchar is attached only when something splits", {
  # the contract: off by default -> attribute-free output; and no bare all-NA attribute either
  testthat::expect_null(attr(format(mean_col(), special_formatting = TRUE), "primary_nchar"))
  plain <- fmt(pct = c(0.4, 0.6), n = c(10L, 10L), scale = "level_pct", pct_type = "row")
  testthat::expect_null(attr(format(plain, bold_split = TRUE), "primary_nchar"))
})

testthat::test_that("tab_md(): a bold row bolds the mean, not the sd", {
  t <- tab(forcats::gss_cat, marital, tvhours, pct = "row", color = FALSE, display = "mean_sd")
  md <- tab_md(t, color = FALSE, css = FALSE, color_legend = FALSE)
  # bold closes BEFORE the joiner: "**3.0**<figsp>(sigma2.6)", never "**3.0 (sigma2.6)**".
  # Phase 14x: the joiner is now the FIGURE space (markdown renders in a proportional host font).
  jn <- stringi::stri_unescape_unicode("\\u00a0")   # the template literal's own space
  testthat::expect_match(md, paste0("\\*\\*[0-9.]+\\*\\*", jn, "\\(", sig), all = FALSE)
  testthat::expect_no_match(md, paste0("\\*\\*[0-9.]+", jn, "\\(", sig, "[0-9. ]+\\)\\*\\*"),
                            all = TRUE)
  testthat::expect_false(any(grepl(nbs, md, fixed = TRUE)))   # the narrow no-break space is gone
})

# === Phase 14m-ii: markdown value-internal padding is figure space ================

testthat::test_that("tab_md() pads a composite's (n=...) with figure space, not ASCII", {
  t  <- tab(forcats::gss_cat, marital, race, pct = "row", display = "{pct} (n={n})")
  md <- tab_md(t, print = FALSE, color = FALSE, css = FALSE)
  # the (n=...) alignment inside a value is a figure space now (survives pandoc + the host font)
  testthat::expect_match(md, paste0("(n=", fig), fixed = TRUE)
  # but the raw layout is byte-for-byte the old one bar the pad glyph: normalise the figure spaces
  # back to ASCII and it equals what format()'s ASCII pad produces at the same widths (nchar-stable).
  testthat::expect_false(grepl(fig, gsub(fig, " ", md, fixed = TRUE), fixed = TRUE))
})

testthat::test_that("format()'s DEFAULT pad (the console) stays ASCII", {
  # the console must NOT move to figure space -- a monospace ASCII space is already one digit wide.
  x <- fmt(n = c(849L, 3648L), pct = c(1, 1), scale = "level_pct", pct_type = "row", display = "{pct} (n={n})")
  testthat::expect_identical(format(x), c("100% (n=  849)", "100% (n=3 648)"))
})

# === footer summary stats are not star-padded (Phase 14m-ii, L5) ==================

testthat::test_that("format(): a gof / pvalue footer cell reaches the column edge (no star pad)", {
  # in a starred column, a "gof" (N/AIC) or "pvalue" summary cell reserves NO star column, so a
  # right-aligned summary number reaches the edge instead of lining up under the starred data.
  x <- fmt(scale = "points", n = c(100L, 100L, NA, NA),
           pct    = c(0.4, 0.6, NA, 0.03),          # pvalue cell stores its p in pct
           diff   = c(0.1, -0.1, 21483, NA),        # gof cell stores its stat in diff
           ci_inf = c(0.05, -0.2, NA, NA), ci_sup = c(0.15, -0.05, NA, NA),
           pvalue = c(0.0005, 0.5, NA, NA),
           display = c("diff", "diff", "gof", "pvalue"), digits = c(0L, 0L, 0L, 2L))
  f <- format(x, special_formatting = TRUE, na = "", stars = TRUE, html = TRUE)
  # the diff data cell IS star-padded to width 3 (stars left, fig pad right)
  testthat::expect_true(endsWith(f[1], paste0("***")) || endsWith(f[2], strrep(fig, 3)))
  # the gof + pvalue cells carry NO trailing figure-space star pad
  testthat::expect_false(endsWith(f[3], fig))
  testthat::expect_false(endsWith(f[4], fig))
})

# === the Excel star pad ===========================================================

testthat::test_that("tab_xl(): the star literal is padded with figure spaces", {
  testthat::skip_if_not_installed("openxlsx2")
  x <- fmt(n = rep(100L, 3), scale = "points", pct_type = "row", pct = c(0.4, 0.5, 0.6), diff = c(0.1, 0, -0.1),
           ci_inf = c(0.05, -0.10, -0.20), ci_sup = c(0.15, 0.10, -0.05),
           pvalue = c(0.0005, 0.5, 0.07), display = "pct")
  st <- get_stars(x)
  testthat::expect_identical(st, c("***", "", "*"))
  # the width every cell's star field is padded to = the column max ("" counts 0)
  w  <- max(nchar(st))
  padded <- stringi::stri_pad(st, w, side = "right", pad = fig)
  testthat::expect_identical(nchar(padded), rep(w, 3L))
  testthat::expect_false(any(grepl(" ", padded, fixed = TRUE)))
  # and format()'s own star pad agrees, glyph for glyph
  testthat::expect_identical(format(x, html = TRUE, stars = TRUE),
                             paste0(format(x, html = TRUE), padded))
})
