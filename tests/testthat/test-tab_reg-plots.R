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
  # OR < 1 rows + empirical_OR: defaults to the MODEL odds-ratio column (not "Emp. OR"), no message
  expect_s3_class(or_plot(tab_logit(d, "married", "race", empirical_OR = TRUE)), "gtable")
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
