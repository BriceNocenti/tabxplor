
# === SECTION: forest_plot() =======================================================================

skip_on_cran()   # several model fits + a graphics device per test


fp_data <- function() fx_reg_fmt()


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


# the segment layers are several (the model whisker, the gap's acceptance bracket, the adjustment
# arrow, the crude's, the guide's key frame) and can share a row count: pick by LINEWIDTH, which is
# what actually distinguishes them (0.9 whisker, 1.3 arrow, 0.25 acceptance bracket).
fp_seg <- function(b, lw) {
  d <- Filter(function(z) isTRUE(all(abs(z$linewidth - lw) < 1e-8)), fp_layer(b, "GeomSegment"))
  expect_gt(length(d), 0L)
  d[[1]]
}

fp_wsk <- function(b, e) fp_seg(b, 0.9)


fp_layer <- function(b, geom) {
  cls <- vapply(b$plot$layers, function(l) class(l$geom)[1], character(1))
  b$data[cls == geom]
}


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
  # the method moved to the axis title -- there is one axis here, so it can speak for every panel --
  # and the ladder is the guide's, so the caption carries neither
  expect_true(grepl("Newcombe", p$labels$x))
  # ...so for a plain coloured crosstab the footer has nothing left to say at all
  cap <- p$labels$caption %||% ""
  expect_false(grepl("Newcombe", cap))
  expect_false(grepl("difference:", cap))

  # a crude column and its model twin are ONE ladder since the merge, so the guide can describe them
  # both -- the case that used to need the prose fallback
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  p2 <- forest_plot(r, observed = "ci", lang = "en")
  expect_false(is.null(tabxplor:::legend_guide_spec(
    r, c("Obs_OR", "Model_OR"), "text", "light", "en")))
  expect_false(grepl("OR \u2265", p2$labels$caption %||% ""))
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
  expect_identical(sum(!is.na(fp_wsk(b, e)$x)),
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


test_that("`color = \"adjustment\"` gives the gap its own geometry, and the whisker gives up its ink", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survey")
  fp_dev()
  d <- fp_data()
  t <- suppressMessages(tab_reg(d, "age", c("race", "rincome"), family = "gaussian",
                                color = "adjustment", empirical = TRUE))
  e <- forest_plot(t, return_data = TRUE)
  b <- fp_build(forest_plot(t))
  # the arrow: one per non-reference row, running FROM the observed value TO the model's
  keep <- is.finite(e$obs) & is.finite(e$estimate) & !e$is_ref & e$obs != e$estimate &
    e$series == "modelled"
  ar <- fp_seg(b, 1.3)
  expect_identical(nrow(ar), sum(keep))
  expect_equal(sort(ar$x),    sort(e$obs[keep]),      tolerance = 1e-8)
  expect_equal(sort(ar$xend), sort(e$estimate[keep]), tolerance = 1e-8)
  # and the acceptance bracket, around the observed value, is a layer of its own
  bk <- fp_seg(b, 0.25)
  expect_equal(sort(bk$x),    sort(e$gap_lo[keep]), tolerance = 1e-8)
  # the whisker recedes to neutral and the SQUARE carries the gap's colour: one statement, one ink
  cols <- tabxplor:::tx_plot_colors("light")
  expect_setequal(unique(stats::na.omit(fp_wsk(b, e)$colour)), cols$grey)
  sq <- Filter(function(z) nrow(z) == sum(e$series == "modelled"), fp_layer(b, "GeomPoint"))
  expect_gt(length(sq), 0L)
  expect_setequal(setdiff(unique(sq[[1]]$fill), cols$text),
                  setdiff(unique(ar$colour), cols$text))
  # the acceptance bracket exists only where the gap IS testable
  t2 <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                 color = "adjustment", empirical = TRUE))
  e2 <- forest_plot(t2, return_data = TRUE)
  expect_false(any(e2$gap_tested))                    # a non-collapsible measure: no test exists
  b2  <- fp_build(forest_plot(t2))
  # ...so no layer holds the acceptance region, while the arrows are still drawn
  lws <- unlist(lapply(fp_layer(b2, "GeomSegment"), function(z) unique(round(z$linewidth, 2))))
  expect_false(0.25 %in% lws)                         # no acceptance region anywhere
  expect_true(1.3 %in% lws)                           # the arrows are still drawn
})


test_that("`display`, `offset` and `label_offset` are the caller's, and the defaults do not move", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = TRUE)
  lab <- function(...) fp_layer(fp_build(forest_plot(t, ...)), "GeomLabel")[[1]]
  # the default is the cell's own primary token; a template swaps it, through set_display()
  expect_false(any(grepl("(", lab()$label, fixed = TRUE)))
  expect_true(all(grepl("(", lab(display = "{est} ({base})")$label, fixed = TRUE)))
  # the offsets move the rows they name, and nothing else
  o1 <- fp_layer(fp_build(forest_plot(t, offset = 0.15)), "GeomLabel")[[1]]$y
  o2 <- fp_layer(fp_build(forest_plot(t, label_offset = 0.5)), "GeomLabel")[[1]]$y
  expect_equal(o2 - o1, rep(0.2, length(o1)), tolerance = 1e-8)
})


# === SECTION: tab_estimates(): the chart model ====================================================

skip_on_cran()  # ~8 model fits; devtools / covr / r-lib-actions all set NOT_CRAN = true


te_data <- function() fx_reg_fmt()


est <- function(...) tabxplor:::tab_estimates(...)


# the model column, by ROLE -- so this file names no column the package might rename (and so it
# stays ASCII: the gaussian one is "Model_\u03b2").
mod_col <- function(t) {
  nm <- names(t)[vapply(t, is_fmt, logical(1))]
  nm[vapply(nm, function(n) identical(as.character(tabxplor:::get_role(t[[n]]))[1], "model"),
            logical(1))][1]
}


test_that("the record carries the ladder, the SD and the secondary axis", {
  d <- te_data()
  s <- tabxplor:::fmt_scale_of(
    tab(d, race, party3, pct = "row", display = "{or}", ref = "first", color = "OR", stars = TRUE)[["3-Republican"]])
  expect_equal(s$neutral, 1); expect_identical(s$trans, "log10"); expect_true(s$mult)
  expect_true(all(c(1.2, 1.5, 2, 4) %in% s$breaks))          # the odds_ratio ladder, both sides
  expect_true(all((1 / c(1.2, 1.5, 2, 4)) %in% s$breaks))   # `%in%` binds tighter than `/`
  expect_identical(sort(unique(s$break_dir)), c(-1L, 0L, 1L))

  # a gaussian beta: the ladder is in SD units, the axis in units of Y, so the record carries SD(Y)
  # and the secondary axis that makes the colour ladder legible
  g <- suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian"))
  sg <- tabxplor:::fmt_scale_of(g[[mod_col(g)]])
  expect_identical(sg$sec, "sd")
  expect_true(is.finite(sg$sd_y))
  expect_equal(sort(sg$breaks[sg$break_dir > 0]), sort(c(0.1, 0.2, 0.4, 0.8) * sg$sd_y))

  # measure = "log": the secondary axis is exp(), the ladder the logged odds_ratio one
  bl <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", measure = "log"))
  sb <- tabxplor:::fmt_scale_of(bl[[mod_col(bl)]])
  expect_identical(sb$sec, "exp")
  expect_equal(sort(sb$breaks[sb$break_dir > 0]), sort(round(log(c(1.2, 1.5, 2, 4)), 1)))

  # a user's break scale moves the plot's gridlines, which or_plot()'s private ladder never did
  old <- getOption("tabxplor.color_breaks")
  on.exit(options(tabxplor.color_breaks = old), add = TRUE)
  set_color_breaks(odds_ratio = c(1.5, 3))
  s2 <- tabxplor:::fmt_scale_of(
    tab(d, race, party3, pct = "row", display = "{or}", ref = "first", color = "OR", stars = TRUE)[["3-Republican"]])
  expect_true(all(c(1.5, 3) %in% s2$breaks))
  expect_false(4 %in% s2$breaks)
})


test_that("ci_center() and fmt_gap_scale_key() are the same dispatch", {
  d <- te_data()
  tabs <- list(tab(d, race, party3, pct = "row", ci = "cell"),
               tab(d, race, party3, pct = "row", ci = "ref"),
               tab(d, race, tvhours, pct = "row", ci = "cell"),
               tab(d, race, tvhours, pct = "row", ci = "ref"),
               tab(d, race, party3, pct = "row", display = "{or}", ref = "first", color = "OR", stars = TRUE),
               suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                        empirical = TRUE)),
               suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian",
                                        empirical = TRUE)))
  n <- 0L
  for (t in tabs) for (nm in names(t)[vapply(t, is_fmt, logical(1))]) {
    col <- t[[nm]]
    s   <- tabxplor:::fmt_scale_of(col)
    # they answer the same question wherever an interval exists. Where none does, ci_center() has no
    # subject and fmt_scale_of() falls back to what the column DISPLAYS -- which is what stops
    # `tab(display = "{or}", ref = "first")`'s reference column (OR bounds NA by construction) reading as a percentage.
    if (tabxplor:::fmt_has_interval(col)) {
      expect_identical(tabxplor:::ci_center(col), vctrs::field(col, s$est_field))
      n <- n + 1L
    }
    expect_identical(tabxplor:::fmt_gap_scale_key(col), s$gap_key)
  }
  expect_gt(n, 10L)
})


test_that("with no stored interval the scale follows the display", {
  d <- te_data()
  t <- tab(d, race, party3, pct = "row", display = "{or}", ref = "first", color = "OR", stars = TRUE)
  ref <- names(t)[vapply(t, function(c) is_fmt(c) && !tabxplor:::fmt_has_interval(c),
                         logical(1))]
  expect_gt(length(ref), 0L)                                  # the reference column
  for (nm in ref) {
    expect_identical(tabxplor:::fmt_scale_of(t[[nm]])$key, "odds_ratio")
    expect_identical(tabxplor:::fmt_scale_of(t[[nm]])$est_field, "or")
  }
  # so the whole table plots on ONE scale, and the axis is not decided by the reference column
  e <- est(t)
  expect_identical(unique(e$scale_key), "odds_ratio")
})


# --- the long model ---------------------------------------------------------------------------------

test_that("the row axis reads all four label-block shapes", {
  d <- te_data()
  e1 <- est(tab(d, race, party3, pct = "row", ci = "ref"))
  expect_identical(levels(e1$var), "race")                    # the variable names the block
  expect_setequal(as.character(unique(e1$level)), c("White", "Black", "Other"))
  expect_true(all(e1$group == ""))

  e2 <- est(tab(d, c(race, relig), party3, pct = "row", ci = "ref"))   # compacted
  expect_setequal(levels(e2$var), c("race", "relig"))

  e3 <- est(tab(d, race, party3, pct = "row", ci = "ref", tab_vars = black))
  expect_true(all(nzchar(e3$group)))                          # the sub-table level

  e4 <- est(suppressMessages(tab_reg(d, "married", c("race", "rincome"),
                                     family = "binomial")))   # var / levels
  expect_setequal(levels(e4$var), c("race", "rincome"))
  expect_false("Constant" %in% levels(e4$var))                # an intercept is not an effect
  e4i <- est(suppressMessages(tab_reg(d, "married", "race", family = "binomial")),
             intercept = TRUE)
  expect_true("Constant" %in% levels(e4i$var))
})


test_that("totals are dropped by default and restored on request", {
  d <- te_data()
  t <- tab(d, race, party3, pct = "row", ci = "ref")
  expect_false(any(est(t)$is_total))
  expect_false("Total" %in% est(t)$column)
  e <- est(t, totals = TRUE)
  expect_true(any(e$is_total))
  expect_true("Total" %in% e$column)
})


test_that("the facet key is one panel per estimate column, merging a crude twin", {
  d <- te_data()
  # a crosstab: one panel per column of the table (the maintainer's layout ruling)
  e <- est(tab(d, race, party3, pct = "row", ci = "ref"))
  xt <- tab(d, race, party3, pct = "row")
  expect_setequal(levels(e$facet), setdiff(names(xt)[vapply(xt, is_fmt, logical(1))], "Total"))
  # a model and its crude twin share a panel (same col_var, different roles)
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_length(levels(est(r, observed = "ci")$facet), 1L)
  # multinomial: two MODEL columns under one col_var -> one panel each
  m <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial"))
  expect_length(levels(est(m)$facet), 2L)
  # model comparison: the single crude block is repeated in every model panel (ruling D7)
  cp <- suppressMessages(tab_reg(d, "married", list(m1 = "race", m2 = c("race", "rincome")),
                                 family = "binomial", empirical = TRUE))
  ec <- est(cp, observed = "ci")
  expect_setequal(levels(ec$facet), c("m1", "m2"))
  expect_setequal(unique(ec$facet[ec$column == "Obs_OR"]), factor(c("m1", "m2"), levels(ec$facet)))
})


test_that("z17 D2: the gap band needs no colour argument", {
  d <- te_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial", link = "ratio",
                                empirical = TRUE))              # no `color = "adjustment"`
  e <- est(t)
  expect_true(any(is.finite(e$gap_se)))
  expect_true(any(is.finite(e$gap_lo) & is.finite(e$gap_hi)))
})


test_that("the gap slot comes from whichever channel carries the gap measure", {
  d <- te_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial", link = "ratio",
                                empirical = TRUE, color = c(TRUE, "adjustment")))
  e <- est(t)
  cd <- fmt_channel_codes(t[["Model_RR"]], tabxplor:::tx_plot_colors(NULL)$theme)
  expect_identical(e$gap_slot, cd$bg_slot[e$row])             # `adjustment` rides the background here
  # with no gap measure at all there is no gap slot
  t2 <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_true(all(is.na(est(t2)$gap_slot)))
})


# --- what = -----------------------------------------------------------------------------------------

test_that("what = 'level' pairs the observed and adjusted percentages", {
  skip_if_not_installed("marginaleffects")
  d <- te_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                effect = "marginal", measure = "difference", empirical = TRUE))
  e <- est(t, what = "level")
  expect_setequal(unique(e$series), c("observed", "modelled"))
  expect_identical(unique(e$kind), "level")
  expect_length(levels(e$facet), 1L)                          # ONE panel: the pairing is the point
  expect_true(all(is.finite(e$estimate)))
  expect_true(all(is.na(e$ci_inf)))                           # no interval stored on that scale
  # exactly one crude column is taken -- Obs_% and Obs_diff are field-identical, `display` separates
  expect_length(unique(e$column[e$series == "observed"]), 1L)
})


test_that("what = 'effect' on a ci = 'cell' table keeps the point and drops the whisker", {
  d <- te_data()
  t <- tab(d, race, party3, pct = "row", ci = "cell")
  e <- est(t, what = "effect")
  expect_identical(unique(e$scale_key), "points")
  for (nm in unique(e$column)) {
    s <- e[e$column == nm, , drop = FALSE]
    expect_equal(s$estimate, get_diff(t[[nm]])[s$row], tolerance = 0)
  }
  expect_true(all(is.na(e$ci_inf)))
  expect_equal(unique(e$neutral), 0)
})


# === SECTION: reg_check_plots() ===================================================================

skip_on_cran()


reg_plot_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}


test_that("reg_check_plots() finds its data again, and `auto` is not `all`", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  # the table records the NAME `data =` was written as, so it needs no second `data =`
  expect_s3_class(reg_check_plots(t), "gtable")
  # `auto` leaves out the two panels whose footer row says the whole thing; `all` restores them
  cx <- suppressMessages(tabxplor:::reg_plot_fits(t, d))[[1L]]
  expect_false(any(c("dispersion", "collinearity") %in% tabxplor:::reg_panel_keys(cx, "auto")))
  expect_true(all(c("dispersion", "collinearity") %in% tabxplor:::reg_panel_keys(cx, "all")))
})


test_that("reg_check_plots() draws every family, and facets a model comparison", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra")
  skip_if_not_installed("MASS"); skip_if_not_installed("nnet")
  d  <- reg_plot_data()
  ds <- withr::with_seed(1, d[sample(nrow(d), 3000L), ])
  ds$inc3 <- factor(dplyr::case_when(
    ds$rincome %in% "$25000 or more" ~ "3-high",
    ds$rincome %in% c("$20000 - 24999", "$15000 - 19999", "$10000 - 14999") ~ "2-mid",
    TRUE ~ "1-low"), ordered = TRUE)
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  q <- function(e) suppressWarnings(suppressMessages(e))
  expect_s3_class(q(reg_check_plots(q(tab_reg(ds, "tvhours", c("race", "age"),
                                              family = "gaussian", stats = FALSE)), ds)), "gtable")
  expect_s3_class(q(reg_check_plots(q(tab_reg(ds, "inc3", c("race", "age"),
                                              family = "ordinal", stats = FALSE)), ds)), "gtable")
  # a multinomial keeps its linearity / influence panels and refuses the residual ones
  expect_s3_class(q(reg_check_plots(q(tab_reg(ds, "partyid", c("race", "age"),
                                              family = "multinomial", stats = FALSE)), ds)), "gtable")
  # a comparison diagnoses EVERY model in one call (ruling R10): ONE TITLED GRID PER MODEL
  cmp <- q(reg_check_plots(q(tab_reg(ds, "married", list(m1 = "race", m2 = c("race", "age")),
                                     family = "binomial", stats = FALSE)), ds))
  expect_length(cmp, 2L)
  expect_s3_class(cmp[[1L]], "gtable")
  # and each grid takes the panel set of ITS OWN family, so a mixed-family table is diagnosed right
  mix <- q(tab_reg(ds, c("married", "inc3"), c("race", "age"),
                   family = c("binomial", "ordinal"), stats = FALSE))
  ctxs <- q(tabxplor:::reg_plot_fits(mix, ds))
  expect_false("proportionality" %in% tabxplor:::reg_panel_keys(ctxs[[1L]], "auto"))
  expect_true("proportionality"  %in% tabxplor:::reg_panel_keys(ctxs[[2L]], "auto"))
  expect_length(q(reg_check_plots(mix, ds)), 2L)
})


# ⚠ THE LOCK for the plotmath rule in rd_link_expr()'s WARNING: R draws a math-mode space (`~`),
# a function call's PARENTHESES and the operators `=` / `<` / `>` from the Adobe Symbol font, which
# `ragg` (Positron's and RStudio's device) renders as MISSING-GLYPH BOXES. Only calls plotmath draws
# with a RULE, or as ordinary text, are allowed in a label. Adding a construct outside this list
# means a formula that is perfect on cairo and a row of empty rectangles in the user's IDE.
plotmath_calls_ok <- function(e) {
  safe <- c("*", "-", "+", "[", "frac", "bar", "bold")   # `[` is a subscript: text, not a glyph
  bad  <- character(0)
  walk <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- deparse(x[[1L]])
    if (!h %in% safe) bad <<- c(bad, h)
    for (i in seq_along(x)[-1L]) walk(x[[i]])
  }
  walk(e)
  unique(bad)
}


test_that("no linearity label reaches for a glyph the Symbol font owns", {
  exprs <- c(
    lapply(c("mean", "logmean", "logit", "risk", "logrisk"),
           function(k) tabxplor:::rd_link_expr(k, k, "married", "1-Married")),
    list(tabxplor:::rd_link_cuts(factor(rep(c("a", "b", "c"), 4), ordered = TRUE),
                                 "ordinal", outcome = "inc")$expr,
         tabxplor:::rd_link_cuts(factor(rep(c("a", "b", "c"), 4)),
                                 "multinomial", outcome = "party")$expr,
         tabxplor:::reg_panel_head("linearity", "a question?")))
  for (e in exprs) expect_identical(plotmath_calls_ok(e), character(0))
})


test_that("an ordinal linearity panel draws one observed curve per cut", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("MASS")
  y <- factor(rep(c("1-low", "2-mid", "3-high"), each = 200L), ordered = TRUE)
  cu <- tabxplor:::rd_link_cuts(y, "ordinal", outcome = "inc3")
  expect_length(cu$curves, 2L)                                  # K - 1 cumulative cuts
  expect_identical(cu$curves[[1L]]$y, as.numeric(as.integer(y) > 1L))
  # a multinomial reads each category against the REFERENCE, on those rows only
  mn <- tabxplor:::rd_link_cuts(factor(as.character(y)), "multinomial", outcome = "party")
  expect_length(mn$curves, 2L)
  expect_length(mn$curves[[1L]]$keep, 400L)
  # everything else keeps exactly one curve, and it is rd_link_y()'s own
  expect_length(tabxplor:::rd_link_cuts(rnorm(50), "gaussian")$curves, 1L)
})


test_that("the randomised quantile residual is reproducible, and `seed = NULL` is a fresh draw", {
  d  <- reg_plot_data()
  ds <- withr::with_seed(1, tidyr::drop_na(d, married, age)[1:2000, ])
  f  <- stats::glm(I(married == "Married") ~ age, data = ds, family = stats::binomial())
  expect_identical(tabxplor:::rd_resid(f, "binomial", ds$married),
                   tabxplor:::rd_resid(f, "binomial", ds$married))
  expect_false(identical(tabxplor:::rd_resid(f, "binomial", ds$married),
                         tabxplor:::rd_resid(f, "binomial", ds$married, seed = NULL)))
  # and the caller's RNG stream is given back untouched
  set.seed(42); a <- stats::runif(3)
  set.seed(42); invisible(tabxplor:::rd_resid(f, "binomial", ds$married)); b <- stats::runif(3)
  expect_identical(a, b)
})


# Phase 18z17: `or_plot()` is GONE (ruling D1 -- never released, and superseded in full by
# forest_plot(), which reads the same table, obeys set_color_breaks() and returns a modifiable ggplot).
# Its tests moved to test-forest-plot.R, except this one, which was never about the drawing: telling a
# MODEL column from its observed twin by ROLE and not by a name prefix is the rule the whole plot
# system rests on, and it is worth a fixture of its own.

test_that("Phase 18z13: a model column is told from its observed twin by ROLE", {
  d <- reg_plot_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  or_cols <- names(t)[vapply(t, function(c) is_fmt(c) && identical(get_scale(c), "odds_ratio"), logical(1))]
  testthat::expect_true("Obs_OR" %in% or_cols)          # the fixture must actually have both
  testthat::expect_true("Model_OR" %in% or_cols)
  roles <- vapply(or_cols, function(n) as.character(tabxplor:::get_role(t[[n]]))[1], character(1))
  testthat::expect_identical(unname(roles[or_cols == "Model_OR"]), "model")
  testthat::expect_identical(unname(roles[or_cols == "Obs_OR"]),   "emp")
  # and that is what forest_plot() selects on: one model column, no message, the crude one left to
  # ride as `obs` (the pre-Phase-g "Emp." prefix filter had counted Obs_OR as a model column)
  skip_if_not_installed("ggplot2")
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  testthat::expect_no_message(p <- forest_plot(t))
  testthat::expect_identical(unique(as.character(forest_plot(t, return_data = TRUE)$column)),
                             "Model_OR")
})


# === Phase 22i: a declared panel must be a drawable panel =========================================
# ⚠ THE INVARIANT. `REG_CHECKS$<check>$families` is a PROMISE reg_panel_keys() makes to the user:
# ask for `check = "all"` and every key it returns must produce a grob. Nothing enforced it, and
# reg_check_plots() COMPACTS a NULL away -- so an ordinal residual panel was declared and silently
# dropped for ~94 commits (the fit's fitted() is the n x K probability matrix, not one number per
# row). This walks the promise instead of trusting it.
test_that("every panel a family DECLARES actually builds", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra")
  skip_if_not_installed("nnet"); skip_if_not_installed("MASS")
  d <- reg_plot_data()[1:1500, ]
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  cols <- tabxplor:::tx_plot_colors(NULL)
  opts <- list(predictors = NULL, max_points = 400L, nbins = 10L, conf = 0.95,
               seed = 1L, facet_ncol = NULL)
  cases <- list(
    list(outcome = "married",  family = "binomial"),
    list(outcome = "tvhours",  family = "gaussian"),
    list(outcome = "tvhours",  family = "poisson"),
    list(outcome = "marital",  family = "multinomial"),
    list(outcome = "rincome",  family = "ordinal")
  )
  for (cs in cases) {
    t  <- suppressMessages(suppressWarnings(
      tab_reg(d, cs$outcome, c("race", "age"), family = cs$family)))
    cx <- suppressMessages(tabxplor:::reg_plot_fits(t, d))[[1L]]
    keys <- tabxplor:::reg_panel_keys(cx, "all")
    for (k in keys) {
      g <- suppressWarnings(tabxplor:::reg_panel_build(k, cx, cols, opts))
      expect_false(is.null(g), info = paste0(cs$family, " declares '", k, "' and draws nothing"))
    }
  }
})


# === SECTION: forest_plot() =======================================================================

skip_on_cran()   # several model fits + a graphics device per test


fp_data <- function() fx_reg_fmt()


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


# the segment layers are several (the model whisker, the gap's acceptance bracket, the adjustment
# arrow, the crude's, the guide's key frame) and can share a row count: pick by LINEWIDTH, which is
# what actually distinguishes them (0.9 whisker, 1.3 arrow, 0.25 acceptance bracket).
fp_seg <- function(b, lw) {
  d <- Filter(function(z) isTRUE(all(abs(z$linewidth - lw) < 1e-8)), fp_layer(b, "GeomSegment"))
  expect_gt(length(d), 0L)
  d[[1]]
}


fp_wsk <- function(b, e) fp_seg(b, 0.9)


fp_layer <- function(b, geom) {
  cls <- vapply(b$plot$layers, function(l) class(l$geom)[1], character(1))
  b$data[cls == geom]
}


test_that("the points are painted the cell's own colour", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = TRUE,
           color_signif = "grey_non_signif")
  e <- forest_plot(t, theme = "light", return_data = TRUE)
  b <- fp_build(forest_plot(t, theme = "light"))
  expect_setequal(unique(stats::na.omit(fp_wsk(b, e)$colour)),
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
  # the gap is a thin capped bracket at the observed's own offset, not a band
  hit <- fp_seg(b, 0.25)
  expect_identical(nrow(hit), nrow(bd))
  # a log10 axis stores its layer data transformed
  expect_equal(sort(10^hit$x),    sort(bd$gap_lo), tolerance = 1e-10)
  expect_equal(sort(10^hit$xend), sort(bd$gap_hi), tolerance = 1e-10)
})


# === SECTION: tab_estimates(): the chart model ====================================================

skip_on_cran()  # ~8 model fits; devtools / covr / r-lib-actions all set NOT_CRAN = true


te_data <- function() fx_reg_fmt()


est <- function(...) tabxplor:::tab_estimates(...)


# the model column, by ROLE -- so this file names no column the package might rename (and so it
# stays ASCII: the gaussian one is "Model_\u03b2").
mod_col <- function(t) {
  nm <- names(t)[vapply(t, is_fmt, logical(1))]
  nm[vapply(nm, function(n) identical(as.character(tabxplor:::get_role(t[[n]]))[1], "model"),
            logical(1))][1]
}


# --- the scale record -------------------------------------------------------------------------------

test_that("fmt_scale_of() reads every column shape", {
  d <- te_data()
  key <- function(t, nm, kind = "auto") tabxplor:::fmt_scale_of(t[[nm]], kind)$key

  # crosstabs: the stored interval decides
  expect_identical(key(tab(d, race, party3, pct = "row", ci = "cell"), "1-Democrat"), "level_pct")
  expect_identical(key(tab(d, race, party3, pct = "row", ci = "ref"), "1-Democrat"), "points")
  expect_identical(key(tab(d, race, tvhours, pct = "row", ci = "cell"), "tvhours"),   "level_mean")
  expect_identical(key(tab(d, race, tvhours, pct = "row", ci = "ref"), "tvhours"),   "mean_diff")
  or <- tab(d, race, party3, pct = "row", display = "{or}", ref = "first", color = "OR", stars = TRUE)
  expect_identical(key(or, "2-Independent, other"), "odds_ratio")

  # regressions: one key per family x effect
  b  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_identical(key(b, "Model_OR"), "odds_ratio")
  bl <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", measure = "log"))
  expect_identical(key(bl, mod_col(bl)), "log_coef")
  g  <- suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian"))
  expect_identical(key(g, mod_col(g)), "raw_diff")

  # the `kind` override is a filter on the SAME dispatch, never a second one
  t <- tab(d, race, party3, pct = "row", ci = "ref")
  expect_identical(key(t, "1-Democrat", "level"),  "level_pct")
  expect_identical(key(t, "1-Democrat", "effect"), "points")
  expect_identical(key(tab(d, race, party3, pct = "row", ci = "cell"), "1-Democrat", "effect"),
                   "points")
})


test_that("columns are chosen by ROLE, never by name", {
  d <- te_data()
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_identical(unique(est(r)$column), "Model_OR")         # the models are the subject
  expect_false("n" %in% est(r)$column)                        # a count column carries no estimate
  # `observed = "ci"` pairs ONE crude column, by ci_type -- not every Obs_* column
  expect_setequal(unique(est(r, observed = "ci")$column), c("Obs_OR", "Model_OR"))
  expect_setequal(unique(est(r, observed = "ci")$series), c("observed", "modelled"))
  # `columns =` is honoured verbatim, and a typo is named
  expect_identical(unique(est(r, columns = "Obs_OR")$column), "Obs_OR")
  expect_error(est(r, columns = "Model_HR"), "no such column")
})


test_that("the gap band's containment IS the gap test", {
  skip_if_not_installed("survey")
  d <- te_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial", link = "ratio",
                                empirical = TRUE, color = c(TRUE, "adjustment")))
  e <- est(t)
  ok <- is.finite(e$gap_se)
  expect_gt(sum(ok), 3L)                                      # never vacuous
  outside <- e$estimate[ok] < e$gap_lo[ok] | e$estimate[ok] > e$gap_hi[ok]
  alpha   <- 1 - get_conf_level(t[["Model_RR"]])[1]
  expect_identical(outside, e$gap_p[ok] < alpha)
  # and the band is the observed effect, folded on the estimate's own scale
  expect_equal(e$gap_lo[ok] * e$gap_hi[ok], e$obs[ok]^2)      # multiplicative: geometric mean = obs
  expect_true(all(e$gap_tested[ok]))
})


# --- colour -----------------------------------------------------------------------------------------

test_that("the colours are the table's own slots and hexes", {
  d <- te_data()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = TRUE,
           color_signif = "grey_non_signif")
  e <- est(t, theme = "light")
  for (nm in unique(e$column)) {
    cd <- fmt_channel_codes(t[[nm]], "light")
    s  <- e[e$column == nm, , drop = FALSE]
    # the colour is the EXPORTERS' resolved one (fmt_col_ann): the cell's hex where it has one, the
    # theme's grey where it does not -- not the raw channel code, which is NA there.
    # read the chrome, never inline it: these literals had already drifted from tx_chrome_hex()
    # (`grey2` by two retunes), which is the very rule tab_export_prep() states at its own site.
    an <- tabxplor:::fmt_col_ann(t[[nm]], c(list(theme = "light"),
                                            tx_chrome_hex("light")[c("text", "grey", "grey2")]))
    expect_identical(s$slot_text, cd$text_slot[s$row])
    expect_identical(s$hex_text,  an$font[s$row])
    expect_identical(s$slot_bg,   cd$bg_slot[s$row])
  }
  expect_true(any(e$slot_text > 0L))                          # never vacuous
  expect_identical(unique(e$measure), "difference")
  expect_identical(unique(e$policy),  "grey_non_signif")
  # the print palette speaks through the FACE, which the model carries too
  ep <- est(t, theme = "print_minimalistic")
  expect_true(any(ep$bold) || any(ep$italic))
})


# === SECTION: reg_check_plots() ===================================================================

skip_on_cran()


reg_plot_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}


test_that("reg_check_plots() refuses a table without its data, and a wrong data set", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  rm(d)                                    # the recorded name must no longer resolve
  expect_error(reg_check_plots(t), "data.+required|required.+data")
  d <- reg_plot_data()
  # THE guard: a diagnostic plot of the wrong model is worse than no plot
  expect_error(reg_check_plots(t, d[1:500, ]), "does not reproduce")
  expect_error(reg_check_plots(tab(d, race, marital), d), "not a")
  expect_error(reg_check_plots(t, d, check = "nope"), "Unknown")
})


# ⚠ THE LOCK for the plotmath rule in rd_link_expr()'s WARNING: R draws a math-mode space (`~`),
# a function call's PARENTHESES and the operators `=` / `<` / `>` from the Adobe Symbol font, which
# `ragg` (Positron's and RStudio's device) renders as MISSING-GLYPH BOXES. Only calls plotmath draws
# with a RULE, or as ordinary text, are allowed in a label. Adding a construct outside this list
# means a formula that is perfect on cairo and a row of empty rectangles in the user's IDE.
plotmath_calls_ok <- function(e) {
  safe <- c("*", "-", "+", "[", "frac", "bar", "bold")   # `[` is a subscript: text, not a glyph
  bad  <- character(0)
  walk <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- deparse(x[[1L]])
    if (!h %in% safe) bad <<- c(bad, h)
    for (i in seq_along(x)[-1L]) walk(x[[i]])
  }
  walk(e)
  unique(bad)
}


# === SECTION: forest_plot() =======================================================================

skip_on_cran()   # several model fits + a graphics device per test


fp_data <- function() fx_reg_fmt()


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


# the segment layers are several (the model whisker, the gap's acceptance bracket, the adjustment
# arrow, the crude's, the guide's key frame) and can share a row count: pick by LINEWIDTH, which is
# what actually distinguishes them (0.9 whisker, 1.3 arrow, 0.25 acceptance bracket).
fp_seg <- function(b, lw) {
  d <- Filter(function(z) isTRUE(all(abs(z$linewidth - lw) < 1e-8)), fp_layer(b, "GeomSegment"))
  expect_gt(length(d), 0L)
  d[[1]]
}


fp_wsk <- function(b, e) fp_seg(b, 0.9)


fp_layer <- function(b, geom) {
  cls <- vapply(b$plot$layers, function(l) class(l$geom)[1], character(1))
  b$data[cls == geom]
}


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


# === SECTION: tab_estimates(): the chart model ====================================================

skip_on_cran()  # ~8 model fits; devtools / covr / r-lib-actions all set NOT_CRAN = true


te_data <- function() fx_reg_fmt()


est <- function(...) tabxplor:::tab_estimates(...)


# the model column, by ROLE -- so this file names no column the package might rename (and so it
# stays ASCII: the gaussian one is "Model_\u03b2").
mod_col <- function(t) {
  nm <- names(t)[vapply(t, is_fmt, logical(1))]
  nm[vapply(nm, function(n) identical(as.character(tabxplor:::get_role(t[[n]]))[1], "model"),
            logical(1))][1]
}


# === SECTION: reg_check_plots() ===================================================================

skip_on_cran()


reg_plot_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}


# reg_check_plots ---------------------------------------------------------------------------------

test_that("reg_check_plots() draws the check panels of a tab_reg table and of a bare fit", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  expect_s3_class(reg_check_plots(t, d), "gtable")
  # the secondary form (ruling R1): a bare fit, no table, same engine
  expect_s3_class(reg_check_plots(stats::lm(tvhours ~ age + race, data = d)), "gtable")
  # one named check
  expect_s3_class(reg_check_plots(t, d, check = "linearity"), "gtable")
})


# ⚠ THE LOCK for the plotmath rule in rd_link_expr()'s WARNING: R draws a math-mode space (`~`),
# a function call's PARENTHESES and the operators `=` / `<` / `>` from the Adobe Symbol font, which
# `ragg` (Positron's and RStudio's device) renders as MISSING-GLYPH BOXES. Only calls plotmath draws
# with a RULE, or as ordinary text, are allowed in a label. Adding a construct outside this list
# means a formula that is perfect on cairo and a row of empty rectangles in the user's IDE.
plotmath_calls_ok <- function(e) {
  safe <- c("*", "-", "+", "[", "frac", "bar", "bold")   # `[` is a subscript: text, not a glyph
  bad  <- character(0)
  walk <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- deparse(x[[1L]])
    if (!h %in% safe) bad <<- c(bad, h)
    for (i in seq_along(x)[-1L]) walk(x[[i]])
  }
  walk(e)
  unique(bad)
}


# The EXHAUSTIVE shape x option sweep the shipped suite keeps a slice of.
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
