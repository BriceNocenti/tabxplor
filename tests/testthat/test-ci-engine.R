# PURPOSE: Validate the Phase 3a confidence-interval / p-value ENGINE (R/tab-agg.R) directly, on
#          plain numeric inputs (NOT through tab()/tab_num() and NOT on tabled data), against
#          trusted, widely used R packages. This pins the exactness of the p-value formulas.
# ROLE: Companion to test-calculations.R (which checks the same maths end-to-end through tables).
# KEY REFERENCES:
#   - pivot / mean p-values  -> stats::t.test() (base R): EXACT, one-sample and Welch two-sample.
#   - Newcombe / AC / Wald    -> DescTools::BinomDiffCI(): these p-values invert a confidence
#     interval that has no standalone "p-value function", so they are checked by DUALITY -- the
#     p we return is exactly the level at which DescTools' independent interval touches zero.
# See: dev/tabxplor_1.4.0_decisions.md §20; dev/verify_ci_inclusion.R.

# === SECTION: pivot p-values vs stats::t.test (exact) =================================

testthat::test_that("ci_pivot() p-value and interval equal a one-sample t-test", {
  set.seed(101)
  for (i in seq_len(25)) {
    n <- sample(3:60, 1)
    x <- stats::rnorm(n, mean = 0.5, sd = 2)
    res <- ci_pivot(mean(x), stats::sd(x) / sqrt(n), df = n - 1,
                    conf_level = 0.95, want_p = TRUE)
    tt <- stats::t.test(x)                       # H0: mean = 0
    testthat::expect_equal(res$pvalue, tt$p.value, tolerance = 1e-10)
    testthat::expect_equal(c(res$inf, res$sup), as.numeric(tt$conf.int), tolerance = 1e-10)
  }
})

testthat::test_that("ci_mean_diff2() p-value and interval equal a Welch two-sample t-test", {
  set.seed(202)
  for (i in seq_len(25)) {
    n1 <- sample(4:50, 1); n2 <- sample(4:50, 1)
    x <- stats::rnorm(n1, mean = 5, sd = 2)
    y <- stats::rnorm(n2, mean = 6, sd = 4)
    res <- ci_mean_diff2(mean(x), stats::var(x), n1, mean(y), stats::var(y), n2,
                         conf_level = 0.95, want_p = TRUE)
    tt <- stats::t.test(x, y, var.equal = FALSE)  # Welch, H0: equal means
    testthat::expect_equal(res$pvalue, tt$p.value, tolerance = 1e-10)
    testthat::expect_equal(c(res$inf, res$sup), as.numeric(tt$conf.int), tolerance = 1e-10)
  }
})

testthat::test_that("ci_pivot() reproduces the t-test across confidence levels", {
  set.seed(303)
  x <- stats::rnorm(30, 1, 3)
  for (cl in c(0.80, 0.90, 0.99)) {
    res <- ci_pivot(mean(x), stats::sd(x) / sqrt(30), df = 29, conf_level = cl, want_p = TRUE)
    tt  <- stats::t.test(x, conf.level = cl)
    testthat::expect_equal(res$pvalue, tt$p.value, tolerance = 1e-10)  # p is level-independent
    testthat::expect_equal(c(res$inf, res$sup), as.numeric(tt$conf.int), tolerance = 1e-10)
  }
})

# === SECTION: proportion-difference p-values vs DescTools (duality) ===================

# For a p-value that inverts interval `method`, the DescTools interval computed at confidence
# level (1 - p) must have its NEAR bound sitting exactly on zero. This checks our inversion
# against DescTools' independent implementation of the same interval.
duality_near_zero <- function(x1, n1, x2, n2, method, pvalue) {
  ci <- DescTools::BinomDiffCI(x1, n1, x2, n2, conf.level = 1 - pvalue, method = method)
  min(abs(ci[, "lwr.ci"]), abs(ci[, "upr.ci"]))
}

# Cases chosen to give a moderate p (roughly 0.002 - 0.25) so 1 - p is a safe confidence level.
prop_cases <- list(
  c(35, 50, 20, 50), c(30, 50, 20, 50), c(26, 50, 20, 50),
  c(40, 80, 28, 80), c(12, 30, 18, 30), c(60, 100, 45, 100)
)

testthat::test_that("newcombe_pvalue() is the exact inversion of DescTools score interval", {
  testthat::skip_if_not_installed("DescTools")
  for (g in prop_cases) {
    x1 <- g[1]; n1 <- g[2]; x2 <- g[3]; n2 <- g[4]
    p <- newcombe_pvalue(x1 / n1, n1, x2 / n2, n2)
    testthat::expect_true(p > 1e-3 && p < 0.5, label = paste0("p in range [", paste(g, collapse = "/"), "]"))
    testthat::expect_equal(duality_near_zero(x1, n1, x2, n2, "score", p), 0,
                           tolerance = 1e-4, label = paste0("Newcombe [", paste(g, collapse = "/"), "]"))
  }
})

testthat::test_that("ci_prop_diff(method='ac') p-value is the exact inversion of DescTools ac", {
  testthat::skip_if_not_installed("DescTools")
  for (g in prop_cases) {
    x1 <- g[1]; n1 <- g[2]; x2 <- g[3]; n2 <- g[4]
    p <- ci_prop_diff(x1 / n1, n1, x2 / n2, n2, method = "ac", want_p = TRUE)$pvalue
    testthat::expect_equal(duality_near_zero(x1, n1, x2, n2, "ac", p), 0,
                           tolerance = 1e-4, label = paste0("AC [", paste(g, collapse = "/"), "]"))
  }
})

testthat::test_that("ci_prop_diff(method='wald') p-value is the exact inversion of DescTools wald", {
  testthat::skip_if_not_installed("DescTools")
  for (g in prop_cases) {
    x1 <- g[1]; n1 <- g[2]; x2 <- g[3]; n2 <- g[4]
    p <- ci_prop_diff(x1 / n1, n1, x2 / n2, n2, method = "wald", want_p = TRUE)$pvalue
    testthat::expect_equal(duality_near_zero(x1, n1, x2, n2, "wald", p), 0,
                           tolerance = 1e-4, label = paste0("Wald [", paste(g, collapse = "/"), "]"))
  }
})

# === SECTION: opt-out and cell-interval sanity =======================================

testthat::test_that("want_p = FALSE and cell intervals carry no p-value", {
  testthat::expect_true(is.na(ci_pivot(1.2, 0.3, df = 10, want_p = FALSE)$pvalue))
  testthat::expect_true(is.na(ci_newcombe(0.6, 50, 0.4, 50, want_p = FALSE)$pvalue))
  testthat::expect_true(all(is.na(ci_wilson(c(0.3, 0.7), c(40, 40))$pvalue)))  # cell CI: no H0
})

# === SECTION: stars agree with the p-value threshold (universal inclusion) ============

testthat::test_that("get_stars() maps p-values to the documented thresholds", {
  x <- fmt(n = rep(30L, 5), type = "row", pct = rep(0.5, 5),
           ci_type = "diff", pvalue = c(0.20, 0.08, 0.03, 0.005, NA))
  testthat::expect_identical(get_stars(x), c("", "*", "**", "***", ""))
})
