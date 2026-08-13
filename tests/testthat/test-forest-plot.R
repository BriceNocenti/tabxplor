# Last Phase z17-ii/iii -- forest_plot().
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
    xt_diff  = tab(d, race, party3, pct = "row", ci = "diff", color = TRUE),
    xt_cell  = tab(d, race, party3, pct = "row", ci = "cell"),
    xt_or    = tab(d, race, party3, pct = "row", OR = TRUE, color = "OR", stars = TRUE),
    xt_mean  = tab(d, race, tvhours, pct = "row", ci = "diff", color = TRUE),
    xt_many  = tab(d, c(race, relig), party3, pct = "row", ci = "diff", color = TRUE),
    xt_subs  = tab(d, race, party3, pct = "row", ci = "diff", tab_vars = black),
    reg_or   = q(tab_reg(d, "married", c("race", "rincome"), family = "binomial", empirical = TRUE)),
    reg_beta = q(tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE)),
    reg_log  = q(tab_reg(d, "married", "race", family = "binomial", exponentiate = FALSE)),
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
                 list(labels = "estimate"), list(theme = "print"), list(theme = "dark"),
                 list(totals = TRUE), list(what = "level"), list(what = "effect")))
    expect_no_error(fp_build(do.call(forest_plot, c(list(t), a))))
})

test_that("the gridlines ARE the table's colour ladder", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- tab(d, race, party3, pct = "row", OR = TRUE, color = "OR", stars = TRUE)
  b <- fp_build(forest_plot(t, lang = "en"))
  # a log10 axis reports its breaks TRANSFORMED, so they come back to the data scale first
  br <- 10^stats::na.omit(b$layout$panel_params[[1]]$x$get_breaks())
  lad <- tabxplor:::fmt_scale_of(t[["3-Republican"]])$breaks
  expect_true(all(vapply(br, function(v) any(abs(lad - v) < 1e-8), logical(1))))
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
  expect_true(all(vapply(br2, function(v) any(abs(lad2 - v) < 1e-8), logical(1))))
  expect_true(near(br2, 1.5) || near(br2, 1 / 1.5))          # the new ladder is on the axis
  expect_false(near(br2, 2))                                 # the old one is gone
})

test_that("the points are painted the cell's own colour", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- tab(d, race, party3, pct = "row", ci = "diff", color = TRUE,
           color_signif = "grey_non_signif")
  e <- forest_plot(t, theme = "light", return_data = TRUE)
  b <- fp_build(forest_plot(t, theme = "light"))
  pts <- fp_layer(b, "GeomPoint")
  expect_gt(length(pts), 0L)
  expect_setequal(unique(stats::na.omit(pts[[1]]$colour)), unique(stats::na.omit(e$point_hex)))
  expect_true(any(e$slot_text > 0L))                          # never vacuous
})

test_that("the gap band is drawn exactly where the model says", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survey")
  fp_dev()
  d <- fp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "poisson",
                                empirical = TRUE, color = c("OR", "adjustment")))
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
  t <- tab(d, race, party3, pct = "row", ci = "diff", color = TRUE,
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

  # with several ladders no honest key list exists -> the prose legend comes back instead
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  p2 <- forest_plot(r, observed = "ci", lang = "en")
  expect_true(is.null(tabxplor:::legend_guide_spec(
    r, c("Obs_%", "Model_OR"), "text", "light", "en")) ||
      grepl("Shades of|Bold", p2$labels$caption %||% ""))
})

test_that("what = 'level' says which argument produces one", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_error(forest_plot(r, what = "level"), "effect = \"ame\"|percentage or a mean")
})

test_that("forest_plot maps a list of tables and returns its data", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  l <- list(tab(d, race, party3, pct = "row", ci = "diff"),
            tab(d, relig, party3, pct = "row", ci = "diff"))
  p <- forest_plot(l)
  expect_true(is.list(p) && !inherits(p, "ggplot"))
  expect_s3_class(p[[1]], "ggplot")
  e <- forest_plot(tab(d, race, party3, pct = "row", ci = "diff"), return_data = TRUE)
  expect_s3_class(e, "tbl_df")
  expect_true(all(c("estimate", "ci_inf", "scale_key", "point_hex") %in% names(e)))
})
