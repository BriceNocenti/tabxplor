# Phase 13b -- the meaningful colour legend (tab_color_legend): the prose/terse assemblers, per-medium
# rendering, the CI-method naming from the stored `ci_settings`, tab_reg beta/IRR, the custom-palette
# fallback, and the French translation. Cell colours are locked separately by test-color-golden.R.

gss <- forcats::gss_cat

# helper: the English plain-prose legend of a table (one string per colour group).
leg_en <- function(tab, ...) {
  suppressWarnings(tab_color_legend(tab, medium = "plain", style = "prose", lang = "en", ...))
}

testthat::test_that("pct diff prose names the shade, reference and thresholds", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  l  <- leg_en(tb)
  testthat::expect_length(l, 1)
  testthat::expect_match(l, "Shades of blue")
  testthat::expect_match(l, "Shades of yellow to red")
  testthat::expect_match(l, "the Total row")
  testthat::expect_match(l, "\\+5;.*\\+30 points", perl = TRUE)
  testthat::expect_match(l, "points\\.")
  testthat::expect_no_match(l, "Grey")            # ignore policy -> no significance note
})

testthat::test_that("ratio prose uses the Total column and the x/1 operators", {
  tb <- tab(gss, marital, race, pct = "col", color = "ratio")
  l  <- leg_en(tb)
  testthat::expect_match(l, "the Total column")
  testthat::expect_match(l, "\u00d72")            # x2 (the over-only default)
})

testthat::test_that("the CI method + confidence level come from the stored ci_settings", {
  # default method_diff = newcombe
  tb1 <- tab(gss, marital, race, pct = "row", color = "diff",
             color_signif = "grey_non_signif", ci = "diff")
  l1  <- leg_en(tb1)
  testthat::expect_match(l1, "Grey: not significantly different from the Total row")
  testthat::expect_match(l1, "Newcombe score interval, 95% confidence")

  # an explicit method_diff = "ac" + a non-default conf_level must be reflected
  tb2 <- tab(gss, marital, race, pct = "row", color = "diff",
             color_signif = "grey_non_signif", ci = "diff",
             method_diff = "ac", conf_level = 0.9)
  l2  <- leg_en(tb2)
  testthat::expect_match(l2, "Wald interval with Agresti-Caffo adjustment, 90% confidence")
})

testthat::test_that("guaranteed_effect annotates the margin of error on the over sentence", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff",
            color_signif = "guaranteed_effect", ci = "diff")
  l  <- leg_en(tb)
  testthat::expect_match(l, "after subtracting the margin of error \\(Newcombe", perl = TRUE)
  testthat::expect_match(l, "Grey: not significantly different from the Total row after the margin of error")
})

testthat::test_that("graceful fallback to package defaults when ci_settings is absent", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff",
            color_signif = "grey_non_signif", ci = "diff")
  attr(tb, "ci_settings") <- NULL                 # simulate a heavy dplyr chain dropping it
  l <- leg_en(tb)
  testthat::expect_match(l, "Newcombe score interval, 95% confidence")   # the defaults
})

testthat::test_that("numeric diff prose shows the standardized SD thresholds, not percents", {
  tb <- tab(gss, marital, tvhours, pct = "row", color = "diff")
  l  <- leg_en(tb)
  testthat::expect_match(l, "\\+0.2;.*\\+0.8 SD", perl = TRUE)
  testthat::expect_no_match(l, "points")
  testthat::expect_no_match(l, "\\+20%")          # the old beta-shows-percent bug
})

testthat::test_that("tab_reg: beta shows SD, IRR says IRR, OR says OR", {
  skip_if_not_installed("broom")
  b <- suppressWarnings(tab_reg(gss, "tvhours", c("marital", "race"), family = "gaussian"))
  lb <- leg_en(b)
  testthat::expect_match(lb, "\u03b2 \u2265")     # beta >=
  testthat::expect_match(lb, "SD")
  testthat::expect_no_match(lb, "\\+20%", perl = TRUE)   # the old beta-shows-percent bug (0.2 -> +20%)

  i <- suppressWarnings(tab_reg(gss, "tvhours", c("marital", "race"), family = "poisson"))
  li <- leg_en(i)
  testthat::expect_match(li, "IRR \u2265")
  testthat::expect_no_match(li, "OR \u2265")

  d2 <- dplyr::mutate(gss, married = as.integer(marital == "Married"))
  o  <- suppressWarnings(tab_logit(d2, "married", "race"))
  lo <- leg_en(o)
  testthat::expect_match(lo, "OR \u2265")
  testthat::expect_match(lo, "the reference category")
  testthat::expect_match(lo, "Wald interval on the log odds-ratio")
})

testthat::test_that("terse console form is compact and coloured-word based", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  l  <- suppressWarnings(tab_color_legend(tb, medium = "console", style = "terse",
                                          lang = "en", colored = FALSE))
  testthat::expect_length(l, 1)
  testthat::expect_match(l, "difference")
  testthat::expect_match(l, "Total")
  testthat::expect_no_match(l, "Shades of blue")  # terse omits the prose shade names
})

testthat::test_that("excel medium returns rich-text runs with hex + bold on the break-words", {
  tb   <- tab(gss, marital, race, pct = "row", color = "diff")
  runs <- suppressWarnings(tab_color_legend(tb, medium = "excel", style = "prose", lang = "en"))
  testthat::expect_type(runs, "list")
  flat <- unlist(runs, recursive = FALSE)
  cols <- vapply(flat, function(r) r$color, character(1))
  bold <- vapply(flat, function(r) isTRUE(r$bold), logical(1))
  testthat::expect_true(any(!is.na(cols)))                       # some coloured runs
  testthat::expect_true(all(bold[!is.na(cols)]))                 # coloured runs are bold
  testthat::expect_true(all(grepl("^#[0-9A-F]{6}$", cols[!is.na(cols)])))
})

testthat::test_that("md medium wraps break-words in the same pandoc classes as the cells", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  l  <- suppressWarnings(tab_color_legend(tb, medium = "md", style = "prose", lang = "en"))
  testthat::expect_match(l, "\\[\\+5\\]\\{\\.p5\\}", perl = TRUE)
  testthat::expect_match(l, "\\[-30\\]\\{\\.m30\\}", perl = TRUE)
})

testthat::test_that("a custom palette drops the baked colour-shade names", {
  old <- get("base", envir = tabxplor:::tabxplor_palette_env)
  on.exit({ assign("base", old, envir = tabxplor:::tabxplor_palette_env);
            tabxplor:::build_palettes() }, add = TRUE)
  set_color_palette(text_colors = c("#111111", "#222222", "#333333", "#444444"))
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  l  <- leg_en(tb)
  testthat::expect_no_match(l, "Shades of blue")
  testthat::expect_match(l, "the Total row")      # still describes the reference + thresholds
})

testthat::test_that("lang = 'fr' applies the French decimal comma (locale-independent)", {
  # the number formatting follows the resolved `lang` directly (not the gettext catalog), so this
  # holds even when the compiled .mo is absent.
  tb  <- tab(gss, marital, tvhours, comp = "all", color = "ratio")   # mean-ratio: decimal breaks
  l   <- suppressWarnings(tab_color_legend(tb, medium = "plain", style = "prose", lang = "fr"))
  testthat::expect_match(l, "1,15")               # FR decimal comma (not "1.15")
  testthat::expect_no_match(l, "1\\.15", perl = TRUE)
})

testthat::test_that("French catalog translates the prose when the .mo is available", {
  # guarded: only when the compiled catalog is bound (skip on a fresh checkout before update_pkg_po).
  po <- system.file("po", "fr", "LC_MESSAGES", "R-tabxplor.mo", package = "tabxplor")
  skip_if(!nzchar(po) || !file.exists(po), "R-tabxplor fr catalog not compiled")
  tb <- tab(gss, marital, race, pct = "row", color = "diff",
            color_signif = "grey_non_signif", ci = "diff")
  l  <- suppressWarnings(tab_color_legend(tb, medium = "plain", style = "prose", lang = "fr"))
  testthat::expect_match(l, "Nuances de bleu")
  testthat::expect_match(l, "la ligne Total")
  testthat::expect_match(l, "seuil de confiance \u00e0 95 %")
})
