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

test_that("reg_check_plots() refuses a table without its data, and a wrong data set", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  expect_error(reg_check_plots(t), "data.+required|required.+data")
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
  # a comparison diagnoses EVERY model in one call (ruling R10), as facets
  expect_s3_class(q(reg_check_plots(q(tab_reg(ds, "married", list(m1 = "race", m2 = c("race", "age")),
                                              family = "binomial", stats = FALSE)), ds)), "gtable")
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
  or_cols <- names(t)[vapply(t, function(c) is_fmt(c) && identical(get_ci_type(c), "or"), logical(1))]
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
