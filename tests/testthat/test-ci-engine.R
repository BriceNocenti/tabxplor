# PURPOSE: Validate the Phase 3a confidence-interval / p-value ENGINE (R/tab-agg.R) directly, on
#          plain numeric inputs (NOT through tab()/tab_num() and NOT on tabled data), against
#          trusted, widely used R packages. This pins the exactness of the p-value formulas.
# ROLE: Companion to test-calculations.R (which checks the same maths end-to-end through tables).
# KEY REFERENCES:
#   - pivot / mean p-values  -> stats::t.test() (base R): EXACT, one-sample and Welch two-sample.
#   - Newcombe / AC / Wald    -> DescTools::BinomDiffCI(): these p-values invert a confidence
#     interval that has no standalone "p-value function", so they are checked by DUALITY -- the
#     p we return is exactly the level at which DescTools' independent interval touches zero.
# See: dev/tabxplor_2.0.0_decisions.md §20; dev/verify_ci_inclusion.R.

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

# === SECTION: 14v-ii engines (student diff, ratio-of-means, Woolf OR) ================

testthat::test_that("ci_mean_diff2(method='student') equals a pooled two-sample t-test = OLS", {
  set.seed(212)
  for (i in seq_len(20)) {
    n1 <- sample(4:50, 1); n2 <- sample(4:50, 1)
    x <- stats::rnorm(n1, 5, 2); y <- stats::rnorm(n2, 6, 2)
    res <- ci_mean_diff2(mean(x), stats::var(x), n1, mean(y), stats::var(y), n2,
                         conf_level = 0.95, want_p = TRUE, method = "student")
    tt <- stats::t.test(x, y, var.equal = TRUE)     # pooled Student, = the two-group OLS coef
    testthat::expect_equal(res$pvalue, tt$p.value, tolerance = 1e-10)
    testthat::expect_equal(c(res$inf, res$sup), as.numeric(tt$conf.int), tolerance = 1e-10)
  }
})

testthat::test_that("ci_mean_ratio() matches exp(log(R) +/- q*se) for all three methods", {
  set.seed(213)
  z <- stats::qnorm(0.975)
  for (i in seq_len(20)) {
    n1 <- sample(20:200, 1); n2 <- sample(20:200, 1)
    x <- abs(stats::rnorm(n1, 5, 2)) + 0.5; y <- abs(stats::rnorm(n2, 4, 2)) + 0.5
    m1 <- mean(x); v1 <- stats::var(x); m2 <- mean(y); v2 <- stats::var(y)
    lr <- log(m1 / m2)
    # robust (delta on log, each group's own variance) -> z
    rob <- ci_mean_ratio(m1, v1, n1, m2, v2, n2, method = "robust")
    se_r <- sqrt((v1 / n1) / m1^2 + (v2 / n2) / m2^2)
    testthat::expect_equal(c(rob$inf, rob$sup), exp(lr + c(-1, 1) * z * se_r), tolerance = 1e-9)
    # naive poisson (S = m*n) -> z
    poi <- ci_mean_ratio(m1, v1, n1, m2, v2, n2, method = "poisson")
    se_p <- sqrt(1 / (m1 * n1) + 1 / (m2 * n2))
    testthat::expect_equal(c(poi$inf, poi$sup), exp(lr + c(-1, 1) * z * se_p), tolerance = 1e-9)
    # quasipoisson (poisson * sqrt(pooled phi)) -> t(n1+n2-2)
    qp  <- ci_mean_ratio(m1, v1, n1, m2, v2, n2, method = "quasipoisson", want_p = TRUE)
    phi <- ((n1 - 1) * v1 / m1 + (n2 - 1) * v2 / m2) / (n1 + n2 - 2)
    se_q <- se_p * sqrt(phi); crit <- stats::qt(0.975, df = n1 + n2 - 2)
    testthat::expect_equal(c(qp$inf, qp$sup), exp(lr + c(-1, 1) * crit * se_q), tolerance = 1e-9)
    testthat::expect_equal(qp$pvalue, 2 * stats::pt(-abs(lr / se_q), df = n1 + n2 - 2), tolerance = 1e-9)
  }
})

testthat::test_that("ci_mean_ratio(method='quasipoisson') equals a quasi-Poisson regression", {
  set.seed(214)
  n <- 300
  grp <- factor(sample(c("a", "b"), n, TRUE))
  y   <- stats::rpois(n, lambda = ifelse(grp == "a", 3, 5)) + stats::rpois(n, 2)  # over-dispersed
  a <- y[grp == "a"]; b <- y[grp == "b"]
  res <- ci_mean_ratio(mean(b), stats::var(b), length(b), mean(a), stats::var(a), length(a),
                       method = "quasipoisson", want_p = TRUE)
  fq  <- stats::glm(y ~ grp, family = stats::quasipoisson())
  co  <- summary(fq)$coefficients["grpb", ]
  crit <- stats::qt(0.975, df = stats::df.residual(fq))
  testthat::expect_equal(c(res$inf, res$sup),
                         exp(co["Estimate"] + c(-1, 1) * crit * co["Std. Error"]) |> unname(),
                         tolerance = 1e-6)
  testthat::expect_equal(res$pvalue, unname(co["Pr(>|t|)"]), tolerance = 1e-6)
})

testthat::test_that("ci_or() is Woolf's log-OR Wald and matches a logistic regression", {
  # hand Woolf on a 2x2
  a <- 30; b <- 70; cc <- 45; d <- 55
  r <- ci_or(a, b, cc, d)
  lor <- log((a * d) / (b * cc)); se <- sqrt(1 / a + 1 / b + 1 / cc + 1 / d)
  z <- stats::qnorm(0.975)
  testthat::expect_equal(c(r$inf, r$sup), exp(lor + c(-1, 1) * z * se), tolerance = 1e-12)
  testthat::expect_equal(r$pvalue, 2 * stats::pnorm(-abs(lor / se)), tolerance = 1e-12)
  # a saturated logit on the 2x2 reproduces the same OR + Wald interval (confint.default = z-Wald)
  dd  <- data.frame(y = c(1, 0, 1, 0), g = factor(c("x", "x", "r", "r")), w = c(a, b, cc, d))
  fit <- stats::glm(y ~ g, weights = w, family = stats::binomial(), data = dd)
  testthat::expect_equal(unname(exp(stats::coef(fit)["gx"])), (a * d) / (b * cc), tolerance = 1e-6)
  # cross-check vs the logit Wald interval (a hair looser -- confint.default's SE has a tiny
  # finite-sample difference from the closed-form Woolf SE checked exactly above).
  testthat::expect_equal(c(r$inf, r$sup),
                         unname(exp(stats::confint.default(fit)["gx", ])), tolerance = 1e-4)
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
  x <- fmt(n = rep(30L, 5), scale = "points", pct_base = "row", pct = rep(0.5, 5), pvalue = c(0.20, 0.08, 0.03, 0.005, NA))
  testthat::expect_identical(get_stars(x), c("", "*", "**", "***", ""))
})
