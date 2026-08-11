# Phase 12h: regression display plots -- lm_plots() (glm/lm diagnostics) + or_plot() (OR forest plot).
# Smoke tests: each builds a ggplot/gtable object without error on a null device (visual correctness is
# checked manually). Guarded by the plotting Suggests (ggplot2 / gridExtra).

reg_plot_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}

# lm_plots --------------------------------------------------------------------------------------

test_that("lm_plots() builds a 2x2 diagnostic panel for lm, glm and the data-frame form", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra")
  d <- reg_plot_data()
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  expect_s3_class(lm_plots(stats::lm(tvhours ~ age + race, data = d)), "gtable")
  expect_s3_class(lm_plots(stats::glm(married ~ age + race, data = d,
                                      family = stats::binomial())), "gtable")
  expect_s3_class(lm_plots(d, "tvhours", c("age", "race")), "gtable")
})

test_that("lm_plots() errors helpfully on a data frame without variables", {
  skip_if_not_installed("ggplot2"); skip_if_not_installed("gridExtra")
  expect_error(lm_plots(reg_plot_data()), "dependent")
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
