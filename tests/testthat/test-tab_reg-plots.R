# Phase 12h / Last Phase z15: regression display plots -- reg_check_plots() (the model checks, drawn)
# + or_plot() (OR forest plot). Smoke tests: each builds a ggplot/gtable object without error on a null
# device (visual correctness is checked manually). Guarded by the plotting Suggests (ggplot2/gridExtra).
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

# or_plot ---------------------------------------------------------------------------------------

test_that("or_plot() builds a forest plot from a tab_logit table", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  expect_s3_class(or_plot(tab_logit(d, "married", c("race", "age"))), "gtable")
  # OR < 1 rows + empirical: defaults to the MODEL odds-ratio column (not "Obs_OR"), no message
  expect_s3_class(or_plot(tab_logit(d, "married", "race", empirical = TRUE)), "gtable")
})

test_that("or_plot() picks the first model column (message) and rejects a bad column", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  t <- multi_logit(d, "married", list(m1 = "race", m2 = c("race", "age")))
  expect_message(or_plot(t), "Several odds-ratio")
  expect_error(or_plot(t, column = "nope"), "not an odds-ratio")
})

test_that("or_plot() errors on a table with no odds-ratio column", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra")
  expect_error(or_plot(tab(reg_plot_data(), race, marital)), "odds-ratio")
})

test_that("Last Phase z13: or_plot() tells a model column from its observed twin by ROLE", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra"); skip_if_not_installed("broom")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  or_cols <- names(t)[vapply(t, function(c) is_fmt(c) && identical(get_ci_type(c), "or"), logical(1))]
  testthat::expect_true("Obs_OR" %in% or_cols)          # the fixture must actually have both
  testthat::expect_true("Model_OR" %in% or_cols)
  # ONE model column -> no "several columns" message, and the model one is the default. The old filter
  # matched the pre-Phase-g "Emp." prefix, so Obs_OR counted as a model column: the message fired and
  # whichever came first was plotted.
  testthat::expect_no_message(p <- or_plot(t))
  testthat::expect_s3_class(p, "gtable")
})
