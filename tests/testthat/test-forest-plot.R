# Phase 18z17-ii/iii -- forest_plot().
#
# The bulk of the contract is asserted on the MODEL (test-tab-estimates.R), which needs no device.
# What is left here is what only a built plot can show: that the gridlines are the colour ladder, that
# the mapped colours are the cell colours, that the gap band is drawn where the model says, that the
# guide carries the ladder and the caption therefore does NOT, and that every table shape draws.

skip_on_cran()   # several model fits + a graphics device per test

fp_data <- function() gss_cat_data_formatting()

# every drawing test opens a null device, as test-tab_reg-plots.R does
fp_dev <- function() {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  withr::defer(grDevices::dev.off(), envir = parent.frame())
}

fp_build <- function(p) ggplot2::ggplot_build(p)

# the built data of the layers drawn by one geom -- ggplot_build() returns them positionally, and
# several layers can share a row count, so they are picked by CLASS, never by shape.
# a break is on the ladder when it is a declared rung, or a x2 continuation of the outermost one
fp_on_ladder <- function(v, lad, tol = 1e-8) {
  if (any(abs(lad - v) < tol)) return(TRUE)
  mx <- max(lad[is.finite(lad)]); mn <- min(lad[is.finite(lad)])
  (v > mx || v < mn) && v > 0
}

fp_layer <- function(b, geom) {
  cls <- vapply(b$plot$layers, function(l) class(l$geom)[1], character(1))
  b$data[cls == geom]
}


test_that("every table shape draws one ggplot", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  q <- function(e) suppressWarnings(suppressMessages(e))
  shapes <- list(
    xt_diff  = tab(d, race, party3, pct = "row", ci = "ref", color = TRUE),
    xt_cell  = tab(d, race, party3, pct = "row", ci = "cell"),
    xt_or    = tab(d, race, party3, pct = "row", display = "{or}", ref = "first", color = "OR", stars = TRUE),
    xt_mean  = tab(d, race, tvhours, pct = "row", ci = "ref", color = TRUE),
    xt_many  = tab(d, c(race, relig), party3, pct = "row", ci = "ref", color = TRUE),
    xt_subs  = tab(d, race, party3, pct = "row", ci = "ref", tab_vars = black),
    reg_or   = q(tab_reg(d, "married", c("race", "rincome"), family = "binomial", empirical = TRUE)),
    reg_beta = q(tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE)),
    reg_log  = q(tab_reg(d, "married", "race", family = "binomial", measure = "log")),
    reg_mnl  = q(tab_reg(d, "party3", "race", family = "multinomial")),
    reg_cmp  = q(tab_reg(d, "married", list(m1 = "race", m2 = c("race", "rincome")),
                         family = "binomial", empirical = TRUE)))
  for (nm in names(shapes)) {
    p <- forest_plot(shapes[[nm]])
    expect_s3_class(p, "ggplot")
    expect_no_error(fp_build(p))
  }
  # and the options, on one of them
  t <- shapes$xt_diff
  for (a in list(list(guide = "bands"), list(facet = FALSE), list(color = FALSE),
                 list(center = "estimate"), list(center = "none"), list(footer = "full"),
                 list(footer = "none"), list(legend = "right"), list(legend = FALSE),
                 list(layout = "keep"), list(layout = "transpose"),
                 list(theme = "print_minimalistic"), list(theme = "dark"),
                 list(totals = TRUE), list(what = "level"), list(what = "effect")))
    expect_no_error(fp_build(do.call(forest_plot, c(list(t), a))))
})

test_that("the gridlines ARE the table's colour ladder", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- tab(d, race, party3, pct = "row", display = "{or}", ref = "first", color = "OR", stars = TRUE)
  b <- fp_build(forest_plot(t, lang = "en"))
  # a log10 axis reports its breaks TRANSFORMED, so they come back to the data scale first
  br <- 10^stats::na.omit(b$layout$panel_params[[1]]$x$get_breaks())
  lad <- tabxplor:::fmt_scale_of(t[["3-Republican"]])$breaks
  expect_true(all(vapply(br, function(v) fp_on_ladder(v, lad), logical(1))))
  expect_true(any(abs(br - 1) < 1e-8))                       # the neutral is always kept
  # the labels are the LEGEND's own glyphs: "1/2" on the axis exactly as in the footer
  lb <- b$layout$panel_params[[1]]$x$get_labels()
  expect_true(any(grepl("^1/", lb)))

  # and they follow set_color_breaks(), which or_plot()'s private ladder never did
  old <- getOption("tabxplor.color_breaks")
  on.exit(options(tabxplor.color_breaks = old), add = TRUE)
  set_color_breaks(odds_ratio = c(1.5, 3))
  br2 <- 10^stats::na.omit(fp_build(forest_plot(t))$layout$panel_params[[1]]$x$get_breaks())
  near <- function(v, x) any(abs(v - x) < 1e-8)
  lad2 <- tabxplor:::fmt_scale_of(t[["3-Republican"]])$breaks
  expect_true(all(vapply(br2, function(v) fp_on_ladder(v, lad2), logical(1))))
  expect_true(near(br2, 1.5) || near(br2, 1 / 1.5))          # the new ladder is on the axis
  expect_false(near(br2, 2))                                 # the old one is gone
})

test_that("the points are painted the cell's own colour", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = TRUE,
           color_signif = "grey_non_signif")
  e <- forest_plot(t, theme = "light", return_data = TRUE)
  b <- fp_build(forest_plot(t, theme = "light"))
  seg <- fp_layer(b, "GeomSegment")
  expect_gt(length(seg), 0L)
  expect_setequal(unique(stats::na.omit(seg[[1]]$colour)),
                  unique(stats::na.omit(e$point_hex[is.finite(e$ci_inf) & !e$is_ref])))
  pts <- fp_layer(b, "GeomPoint")                             # the square is FILLED with it
  expect_true(all(unique(stats::na.omit(e$point_hex)) %in%
                    unlist(lapply(pts, function(d) d$fill))))
  expect_true(any(e$slot_text > 0L))                          # never vacuous
})

test_that("the gap band is drawn exactly where the model says", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survey")
  fp_dev()
  d <- fp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial", link = "ratio",
                                empirical = TRUE, color = c(TRUE, "adjustment")))
  e <- forest_plot(t, return_data = TRUE)
  bd <- e[is.finite(e$gap_lo), , drop = FALSE]
  expect_gt(nrow(bd), 3L)
  b   <- fp_build(forest_plot(t, observed = "band"))
  hit <- Filter(function(l) nrow(l) == nrow(bd), fp_layer(b, "GeomLinerange"))
  expect_gt(length(hit), 0L)
  # a log10 axis stores its layer data transformed
  expect_equal(sort(10^hit[[1]]$xmin), sort(bd$gap_lo), tolerance = 1e-10)
  expect_equal(sort(10^hit[[1]]$xmax), sort(bd$gap_hi), tolerance = 1e-10)
})

test_that("the colour legend is the guide, and never printed twice", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = TRUE,
           color_signif = "grey_non_signif")
  p <- forest_plot(t, lang = "en")
  sc <- Filter(function(s) !inherits(s$name, "waiver") && !is.null(s$name), p$scales$scales)
  expect_gt(length(sc), 0L)
  gs <- tabxplor:::legend_guide_spec(t, unique(as.character(
    forest_plot(t, return_data = TRUE)$column)), "text", "light", "en")
  expect_identical(sc[[1]]$name, gs$title)
  expect_true(all(gs$keys$label %in% unlist(lapply(sc, `[[`, "labels"))))
  # the caption keeps the method and the stars, and drops the ladder the guide now carries
  cap <- p$labels$caption
  expect_true(grepl("Newcombe", cap))
  expect_false(grepl("Shades of", cap))

  # a crude column and its model twin are ONE ladder since the merge, so the guide can describe them
  # both -- the case that used to need the prose fallback
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  p2 <- forest_plot(r, observed = "ci", lang = "en")
  expect_false(is.null(tabxplor:::legend_guide_spec(
    r, c("Obs_OR", "Model_OR"), "text", "light", "en")))
  expect_false(grepl("Shades of", p2$labels$caption %||% ""))
})

test_that("what = 'level' draws the observed and adjusted levels, which every model column now has", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_s3_class(forest_plot(r, what = "level"), "ggplot")
  # a link-scale coefficient has no level to plot, and says so
  rl <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", measure = "log"))
  expect_error(forest_plot(rl, what = "level"), "percentage or a mean")
})

test_that("forest_plot maps a list of tables and returns its data", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  l <- list(tab(d, race, party3, pct = "row", ci = "ref"),
            tab(d, relig, party3, pct = "row", ci = "ref"))
  p <- forest_plot(l)
  expect_true(is.list(p) && !inherits(p, "ggplot"))
  expect_s3_class(p[[1]], "ggplot")
  e <- forest_plot(tab(d, race, party3, pct = "row", ci = "ref"), return_data = TRUE)
  expect_s3_class(e, "tbl_df")
  expect_true(all(c("estimate", "ci_inf", "scale_key", "point_hex") %in% names(e)))
})

# Phase 22e-ii -- the per-panel axis, and what it fixed.

test_that("a table that mixes units draws every estimate, each panel in its own transform", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- suppressMessages(tab_reg(d, c("married", "tvhours"), c("race", "rincome"),
                                family = c("binomial", "gaussian"), empirical = TRUE))
  e <- forest_plot(t, return_data = TRUE)
  expect_gt(length(unique(e$scale_key)), 1L)                   # a log scale beside an additive one
  # the whole point: NOTHING is dropped. A single log10 scale used to turn every negative mean
  # difference into NaN and silently remove it.
  expect_no_warning(b <- fp_build(forest_plot(t)))
  wsk <- fp_layer(b, "GeomSegment")[[1]]
  expect_identical(sum(!is.na(wsk$x)),
                   sum(is.finite(e$ci_inf) & is.finite(e$ci_sup) & !e$is_ref &
                         e$series == "modelled"))
  # each panel resolves its OWN breaks, from its own scale
  pp <- b$layout$panel_params
  brs <- lapply(pp, function(z) stats::na.omit(z$x$get_breaks()))
  expect_true(all(lengths(brs) > 1L))
  expect_gt(length(unique(vapply(brs, function(v) paste(round(v, 6), collapse = "|"),
                                 character(1)))), 1L)
})

test_that("the axis title speaks for every panel, or it does not exist", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  # one scale, one word: the title carries it
  t1 <- tab(d, race, party3, pct = "row", ci = "ref", color = "diff")
  expect_true(nzchar(forest_plot(t1, lang = "en")$labels$x %||% ""))
  # one scale, TWO words -- a multinomial risk ratio beside an ordinal win ratio. A single title
  # would name the other panel's quantity, so the word moves into the strip and the title goes.
  t2 <- suppressMessages(suppressWarnings(
    tab_reg(d, c("party3", "rincome"), c("race", "age"),
            family = c("multinomial", "ordinal"), measure = "ratio")))
  p2 <- suppressWarnings(forest_plot(t2, lang = "en"))
  expect_null(p2$labels$x)
  g  <- ggplot2::ggplot_gtable(fp_build(p2))
  lb <- unlist(lapply(g$grobs[grep("strip-t", g$layout$name)], function(gt)
    tryCatch(gt$grobs[[1]]$children[[2]]$children[[1]]$label, error = function(e) NULL)))
  expect_true(any(grepl("risk ratio$", lb)))
  expect_true(any(grepl("win ratio$", lb)))
})

test_that("panels on one scale share a range; panels on different ones do not", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  rg <- function(b) lapply(b$layout$panel_params, function(z) z$x.range)
  one <- fp_build(forest_plot(tab(d, race, party3, pct = "row", ci = "ref", color = "diff")))
  r1  <- unique(vapply(rg(one), function(v) paste(round(v, 9), collapse = "|"), character(1)))
  expect_length(r1, 1L)                                        # all percentage points: comparable
  mix <- suppressMessages(tab_reg(d, c("married", "tvhours"), "race",
                                  family = c("binomial", "gaussian")))
  r2  <- unique(vapply(rg(fp_build(forest_plot(mix))),
                       function(v) paste(round(v, 9), collapse = "|"), character(1)))
  expect_gt(length(r2), 1L)
})

test_that("the gap band never sets the range, and a band filling its panel is not drawn", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "relig"), family = "binomial",
                                empirical = TRUE, color = c(TRUE, "adjustment")))
  e <- forest_plot(t, return_data = TRUE)
  b <- fp_build(forest_plot(t, observed = "band"))
  rng <- b$layout$panel_params[[1]]$x.range
  # the range is the estimates and their intervals, never the band
  est <- log10(c(e$estimate, e$ci_inf, e$ci_sup))
  expect_true(all(est[is.finite(est)] >= rng[1] - 1e-8 & est[is.finite(est)] <= rng[2] + 1e-8))
  bands <- fp_layer(b, "GeomLinerange")
  for (l in bands) expect_true(all(l$xmin >= rng[1] - 1e-8 & l$xmax <= rng[2] + 1e-8))
})

test_that("`layout` reads the axis with more levels, and drops the reference panel", {
  skip_if_not_installed("ggplot2")
  d <- fp_data()
  wide <- tab(d, race, marital, pct = "row", ref = 1, ci = "ref", color = "diff")   # 3 x 6
  tall <- tab(d, marital, race, pct = "row", ref = 1, ci = "ref", color = "diff")   # 6 x 3
  # "keep" is the default; "auto" is the opt-in that picks the axis with more levels
  ew <- forest_plot(wide, layout = "auto", return_data = TRUE) # transposed: y = the 6 columns
  et <- forest_plot(tall, layout = "auto", return_data = TRUE) # kept:       y = the 6 rows
  expect_identical(as.character(forest_plot(wide, return_data = TRUE)$facet),
                   as.character(forest_plot(wide, layout = "keep", return_data = TRUE)$facet))
  expect_gt(length(unique(ew$level)), length(unique(ew$facet)))
  expect_gt(length(unique(et$level)), length(unique(et$facet)))
  # the reference level of the FACET axis carries no deviation, so it is not a panel
  expect_false("White" %in% as.character(ew$facet))
  expect_false(any(ew$is_ref))
  # ...but on the READING axis it stays, as the anchor row at the neutral
  expect_true(any(et$is_ref))
  # the two orientations are one swap: the transposed panels ARE the kept rows, minus the reference
  ek <- forest_plot(wide, layout = "keep", return_data = TRUE)
  expect_setequal(unique(as.character(ew$facet)),
                  setdiff(unique(as.character(ek$level)), unique(as.character(ek$level[ek$is_ref]))))
  expect_setequal(unique(as.character(ew$level)), unique(as.character(ek$facet)))
})

test_that("`center` and `footer` are the two layouts and the three footers", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = TRUE)
  # the value rides above the whisker in both modes that print it, and nowhere in "none"
  lab <- function(...) fp_layer(fp_build(forest_plot(t, ...)), "GeomLabel")
  expect_gt(length(lab(center = "n")), 0L)
  expect_gt(length(lab(center = "estimate")), 0L)
  expect_length(lab(center = "none"), 0L)
  # a square only where one is asked for
  expect_gt(length(fp_layer(fp_build(forest_plot(t, center = "n")), "GeomPoint")), 0L)
  expect_length(fp_layer(fp_build(forest_plot(t, center = "estimate")), "GeomPoint"), 0L)
  # the three footers: the console's, the exports' longer one, none. They differ only where the
  # ladder is IN them -- with a guide it is not, and neither style may print it twice (ruling D6).
  short <- forest_plot(t, footer = "short", legend = "none")$labels$caption
  full  <- forest_plot(t, footer = "full",  legend = "none")$labels$caption
  expect_true(nchar(full) > nchar(short))
  expect_identical(forest_plot(t, footer = "short")$labels$caption,
                   forest_plot(t, footer = "full")$labels$caption)
  expect_null(forest_plot(t, footer = "none")$labels$caption)
})
