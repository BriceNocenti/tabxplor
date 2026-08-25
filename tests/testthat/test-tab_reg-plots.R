# Phase 12h / Phase 18z15: the regression model CHECKS, drawn -- reg_check_plots(). Smoke tests:
# each builds a gtable without error on a null device (visual correctness is checked manually).
# Guarded by the plotting Suggests (ggplot2 / gridExtra). The results plot is test-forest-plot.R.
#
# CRAN time: a multi-panel grid is seconds of CPU. skip_on_cran() trims the CRAN check without
# weakening our own CI (devtools / covr / r-lib-actions all set NOT_CRAN=true).
skip_on_cran()

reg_plot_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}

# reg_check_plots ---------------------------------------------------------------------------------

test_that("reg_check_plots() draws the check panels of a tab_reg table and of a bare fit", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  expect_s3_class(reg_check_plots(t, d), "gtable")
  # the secondary form (ruling R1): a bare fit, no table, same engine
  expect_s3_class(reg_check_plots(stats::lm(tvhours ~ age + race, data = d)), "gtable")
  # one named check
  expect_s3_class(reg_check_plots(t, d, check = "linearity"), "gtable")
})

test_that("reg_check_plots() finds its data again, and `auto` is not `all`", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  # the table records the NAME `data =` was written as, so it needs no second `data =`
  expect_message(expect_s3_class(reg_check_plots(t), "gtable"), "\\bd\\b")
  # `auto` leaves out the two panels whose footer row says the whole thing; `all` restores them
  cx <- suppressMessages(tabxplor:::reg_plot_fits(t, d))[[1L]]
  expect_false(any(c("dispersion", "collinearity") %in% tabxplor:::reg_panel_keys(cx, "auto")))
  expect_true(all(c("dispersion", "collinearity") %in% tabxplor:::reg_panel_keys(cx, "all")))
})

test_that("reg_check_plots() refuses a table without its data, and a wrong data set", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
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

test_that("reg_check_plots() draws every family, and facets a model comparison", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
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
  skip_if_not_installed("broom")
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

test_that("reg_check_plots() panel set follows REG_CHECKS, family by family", {
  # no fit needed: the selector IS the fact table
  expect_true(all(c("linearity", "residuals", "normality", "dispersion", "influence",
                    "collinearity") %in% reg_checks_for("binomial", what = "panel")))
  # a multinomial refuses every residual panel (two level orderings give residuals correlated -0.705)
  # and collinearity (car::vif() warns there)
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


# Phase 18z17: `or_plot()` is GONE (ruling D1 -- never released, and superseded in full by
# forest_plot(), which reads the same table, obeys set_color_breaks() and returns a modifiable ggplot).
# Its tests moved to test-forest-plot.R, except this one, which was never about the drawing: telling a
# MODEL column from its observed twin by ROLE and not by a name prefix is the rule the whole plot
# system rests on, and it is worth a fixture of its own.

test_that("Phase 18z13: a model column is told from its observed twin by ROLE", {
  skip_if_not_installed("broom")
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
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
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
