# PURPOSE: forest_plot(), reg_check_plots(), and that the plotted estimate IS the stored number.
# ROLE: the shipped CONTRACT for R/plots.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

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





test_that("every table shape draws one ggplot", {
  skip_if_not_installed("ggplot2")
  fp_dev()
  d <- fp_data()
  q <- function(e) suppressWarnings(suppressMessages(e))
  # One shape per KIND forest_plot has to lay out -- a difference, a mean, a model column and a
  # per-category one. The exhaustive shape x option sweep is dev/tests/testthat/test-plots-sweep.R:
  # every ggplot_build() here costs more than the fit that fed it.
  shapes <- list(
    xt_diff = tab(d, race, party3, pct = "row", ci = "ref", color = TRUE),
    xt_mean = tab(d, race, tvhours, pct = "row", ci = "ref", color = TRUE),
    reg_or  = q(tab_reg(d, "married", c("race", "rincome"), family = "binomial", empirical = TRUE)),
    reg_mnl = q(tab_reg(d, "party3", "race", family = "multinomial")))
  for (nm in names(shapes)) {
    p <- forest_plot(shapes[[nm]])
    expect_s3_class(p, "ggplot")
    expect_no_error(fp_build(p))
  }
  # and the layout axes, on one of them
  for (a in list(list(facet = FALSE), list(layout = "transpose"),
                 list(what = "level"), list(theme = "dark")))
    expect_no_error(fp_build(do.call(forest_plot, c(list(shapes$xt_diff), a))))
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





# --- no drift ---------------------------------------------------------------------------------------

test_that("the plotted estimate IS the number the table stores and prints", {
  d <- te_data()
  tabs <- list(
    or   = suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE)),
    beta = suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE)),
    logc = suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                    measure = "log", empirical = TRUE)),
    irr  = suppressWarnings(suppressMessages(
             tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE))),
    mnl  = suppressMessages(tab_reg(d, "party3", "race", family = "multinomial")),
    xt_c = tab(d, race, party3, pct = "row", ci = "cell"),
    xt_d = tab(d, race, party3, pct = "row", ci = "ref"),
    xt_m = tab(d, race, tvhours, pct = "row", ci = "ref"),
    xt_o = tab(d, race, party3, pct = "row", display = "{or}", ref = "first", color = "OR", stars = TRUE))
  if (requireNamespace("marginaleffects", quietly = TRUE))
    tabs$ame <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                         effect = "marginal", measure = "difference", empirical = TRUE))
  n_checked <- 0L
  for (t in tabs) {
    e <- est(t, observed = "ci")
    for (nm in unique(e$column)) {
      col <- t[[nm]]
      s   <- e[e$column == nm & !duplicated(e$row), , drop = FALSE]
      # (a) the estimate is exactly the field the STORED interval is centred on (where there is one:
      #     an intervalless column follows its `display` instead -- see the fixture above)
      if (tabxplor:::fmt_has_interval(col))
        expect_equal(s$estimate, tabxplor:::ci_center(col)[s$row], tolerance = 0)
      else
        expect_equal(s$estimate,
                     vctrs::field(col, tabxplor:::fmt_scale_of(col)$est_field)[s$row], tolerance = 0)
      # (b) on a model column that is also the primary number format() prints (the reference cell
      #     prints "1" / "0" through a different display token, so it is excluded)
      if (identical(as.character(get_role(col))[1], "model")) {
        k <- !s$is_ref
        expect_equal(s$estimate[k], get_num(col)[s$row][k], tolerance = 0)
      }
      n_checked <- n_checked + 1L
    }
  }
  expect_gt(n_checked, 12L)                                   # never vacuous
})





# === SECTION: reg_check_plots() ===================================================================

skip_on_cran()




reg_plot_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}




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




test_that("reg_check_plots() panel set follows REG_CHECKS, family by family", {
  # no fit needed: the selector IS the fact table
  expect_true(all(c("linearity", "residuals", "normality", "dispersion", "influence",
                    "collinearity") %in% reg_checks_for("binomial", what = "panel")))
  # a multinomial refuses every residual panel (two level orderings give residuals correlated -0.705)
  # and collinearity (a block vcov has no single correlation matrix)
  mn <- reg_checks_for("multinomial", what = "panel")
  expect_false(any(c("residuals", "normality", "collinearity") %in% mn))
  # proportionality is ordinal-only, and unweighted-only (svyolr has no Brant fit)
  expect_true("proportionality" %in% reg_checks_for("ordinal", what = "panel"))
  expect_false("proportionality" %in% reg_checks_for("ordinal", weighted = TRUE, what = "panel"))
  # the two taught-but-unscored checks contribute a panel and NO footer row
  expect_false(any(c("residuals", "normality") %in% reg_checks_for("binomial", what = "footer")))
  # the DEFAULT grid is a declared subset of the same list
  expect_true(all(tabxplor:::reg_panels_default("binomial") %in%
                    reg_checks_for("binomial", what = "panel")))
  # a panel's reference line IS the check's own flag, where it declares one
  expect_identical(tabxplor:::reg_panel_marks("influence"), REG_CHECKS$influence$flag)
})
