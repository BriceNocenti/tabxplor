# The RANK level: an ordinal model read as a probability of superiority, in one column.
# What is pinned here is what fails SILENTLY -- the two analytic gradients, the identity that lets
# one pair carry two readings, and the collapsibility the `adjustment` colour rests on.

testthat::test_that("the superiority pair's two readings are readings of ONE pair", {
  set.seed(1)
  p1 <- c(.25, .30, .20, .25); p0 <- c(.40, .25, .20, .15)
  pr <- tabxplor:::reg_rank_pair(p1, p0, "identity")
  # 2*gamma - 1 == win - loss is an identity, not an approximation
  testthat::expect_equal(2 * pr$gamma - 1, pr$win - pr$loss, tolerance = 1e-14)
  testthat::expect_equal(pr$est, pr$win - pr$loss, tolerance = 1e-14)
  testthat::expect_equal(pr$alt, pr$win / pr$loss, tolerance = 1e-14)
  # comparing a distribution with itself is a coin flip, exactly
  testthat::expect_equal(tabxplor:::reg_rank_pair(p1, p1, "identity")$gamma, 0.5, tolerance = 1e-14)
  testthat::expect_equal(tabxplor:::reg_rank_pair(p1, p1, "identity")$est,   0,   tolerance = 1e-14)
  testthat::expect_equal(tabxplor:::reg_rank_pair(p1, p1, "log")$est,        0,   tolerance = 1e-14)
  # the log reading is the log of the other one's ratio
  lg <- tabxplor:::reg_rank_pair(p1, p0, "log")
  testthat::expect_equal(exp(lg$est), pr$win / pr$loss, tolerance = 1e-12)
  testthat::expect_equal(lg$alt, pr$win - pr$loss, tolerance = 1e-14)
})

testthat::test_that("on two categories the pair IS the binomial family's own measures", {
  a <- 0.62; b <- 0.44
  pr <- tabxplor:::reg_rank_pair(c(1 - a, a), c(1 - b, b), "identity")
  testthat::expect_equal(pr$est, a - b, tolerance = 1e-14)                       # the risk difference
  testthat::expect_equal(pr$alt, (a / (1 - a)) / (b / (1 - b)), tolerance = 1e-12)  # the odds ratio
})

testthat::test_that("the analytic gradients match numeric differentiation, on both links", {
  testthat::skip_if_not_installed("MASS")
  set.seed(3)
  n <- 600
  d <- data.frame(x = factor(sample(c("a", "b"), n, TRUE)), z = stats::rnorm(n))
  eta <- 0.8 * (d$x == "b") + 0.6 * d$z
  cp  <- sapply(c(-1, 0, 1), function(k) stats::plogis(k - eta))
  P   <- cbind(cp[, 1], cp[, -1] - cp[, -3], 1 - cp[, 3])
  d$y <- factor(apply(P, 1, function(p) sample.int(4L, 1L, prob = p)), ordered = TRUE)
  m   <- MASS::polr(y ~ x + z, data = d, Hess = TRUE)
  eng <- tabxplor:::reg_prob_engine(m)

  for (lnk in c("identity", "log")) {
    p <- tabxplor:::reg_gcomp_rank_maker(m, d, NULL, lnk)("x", "b", "a")
    fn <- function(th) {
      X1 <- eng$mm(transform(d, x = factor("b", levels = levels(d$x))))
      X0 <- eng$mm(transform(d, x = factor("a", levels = levels(d$x))))
      tabxplor:::reg_rank_pair(colMeans(eng$probs(th, X1)),
                               colMeans(eng$probs(th, X0)), lnk)$est
    }
    th <- eng$theta; h <- 1e-6
    num <- vapply(seq_along(th), function(j) {
      u <- th; v <- th; u[j] <- u[j] + h; v[j] <- v[j] - h; (fn(u) - fn(v)) / (2 * h)
    }, numeric(1))
    testthat::expect_equal(p$est, fn(th), tolerance = 1e-12)
    testthat::expect_lt(max(abs(p$G - num)), 1e-6)
    testthat::expect_equal(p$mean0, 0.5, tolerance = 1e-14)   # `{base}` on the reference row
  }
})

testthat::test_that("the crude closed form's SE matches a multinomial bootstrap", {
  set.seed(5)
  y1 <- c(120, 200, 160, 90); y0 <- c(300, 340, 410, 500)
  p1 <- y1 / sum(y1); p0 <- y0 / sum(y0)
  pr <- tabxplor:::reg_rank_pair(p1, p0, "identity")
  se <- tabxplor:::reg_rank_se(pr, p1, p0, sum(y1), sum(y0))
  bs <- replicate(1500, tabxplor:::reg_rank_pair(
    stats::rmultinom(1, sum(y1), p1)[, 1] / sum(y1),
    stats::rmultinom(1, sum(y0), p0)[, 1] / sum(y0), "identity")$est)
  testthat::expect_equal(se, stats::sd(bs), tolerance = 0.08)
})

testthat::test_that("the marginal superiority measure is COLLAPSIBLE, where the cumOR is not", {
  # This is what makes `color = \"adjustment\"` a test here rather than a description: with the
  # covariate INDEPENDENT of the exposure there is no confounding, so an honest crude/adjusted
  # comparison must not move -- and the cumulative odds ratio does.
  testthat::skip_if_not_installed("MASS")
  set.seed(9)
  n <- 4000
  x <- stats::rbinom(n, 1, .5); z <- stats::rnorm(n)
  eta <- 0.8 * x + 1.5 * z
  cp  <- sapply(c(-1.5, -.5, .5, 1.5), function(k) stats::plogis(k - eta))
  P   <- cbind(cp[, 1], cp[, -1] - cp[, -4], 1 - cp[, 4])
  d   <- data.frame(y = factor(apply(P, 1, function(p) sample.int(5L, 1L, prob = p)), ordered = TRUE),
                    x = factor(x), z = z)
  m   <- MASS::polr(y ~ x + z, data = d, Hess = TRUE)
  adj <- tabxplor:::reg_gcomp_rank_maker(m, d, NULL, "identity")("x", "1", "0")$est
  tb  <- prop.table(table(d$x, d$y), 1)
  crd <- tabxplor:::reg_rank_pair(as.numeric(tb["1", ]), as.numeric(tb["0", ]), "identity")$est
  testthat::expect_lt(abs(adj - crd), 0.02)                        # the pair barely moves
  or_crude <- exp(stats::coef(MASS::polr(y ~ x, data = d, Hess = TRUE)))
  testthat::expect_gt(exp(stats::coef(m))[["x1"]] / or_crude, 1.2) # the cumOR moves a lot
})

testthat::test_that("tab_reg() draws ONE column, with its crude twin and a tested gap", {
  testthat::skip_if_not_installed("MASS")
  testthat::skip_if_not_installed("broom")
  d <- gss_cat_data_formatting()
  t <- suppressWarnings(tab_reg(d, outcome = "rincome", predictors = c("race", "marital"),
                                family = "ordinal", effect = "marginal", empirical = TRUE,
                                color = "adjustment", cleannames = FALSE))
  testthat::expect_true(all(c("Model_mD", "Obs_D") %in% names(t)))
  mo <- t[["Model_mD"]]; ob <- t[["Obs_D"]]
  # the crude twin is a real column with a real interval, which a 3+ level outcome never had before
  testthat::expect_true(any(!is.na(get_ci_inf(ob))))
  # both columns read on the same scale, and both carry the probability of superiority as `{base}`
  testthat::expect_identical(get_scale(mo), get_scale(ob))
  testthat::expect_true(all(get_pct(mo)[!is.na(get_pct(mo))] >= 0 &
                            get_pct(mo)[!is.na(get_pct(mo))] <= 1))
  # collapsible, so the gap between them is TESTED rather than merely coloured
  testthat::expect_true(any(is.finite(vctrs::field(mo, "gap_se"))))
  # the footer says what "higher" means -- the only place a one-column table can
  testthat::expect_true(any(grepl("from low to high", tabxplor:::reg_model_lines(t), fixed = TRUE)))
})

testthat::test_that("`measure = \"ratio\"` on an ordered outcome builds one win-ratio column", {
  testthat::skip_if_not_installed("MASS")
  testthat::skip_if_not_installed("broom")
  d <- gss_cat_data_formatting()
  t <- suppressWarnings(tab_reg(d, outcome = "rincome", predictors = "race",
                                family = "ordinal", measure = "ratio", cleannames = FALSE))
  testthat::expect_true("Model_mWR" %in% names(t))
  testthat::expect_identical(get_scale(t[["Model_mWR"]]), "pct_ratio")
  # the reference row is the neutral of a ratio, and its base a coin flip
  i <- which(as.character(t$levels) == "White")
  testthat::expect_equal(vctrs::field(t[["Model_mWR"]], "ratio")[i], 1, tolerance = 1e-12)
  testthat::expect_equal(get_pct(t[["Model_mWR"]])[i], 0.5, tolerance = 1e-12)
})

testthat::test_that("a WEIGHTED ordinal model can be read on its rank measures", {
  testthat::skip_if_not_installed("MASS")
  testthat::skip_if_not_installed("survey")
  testthat::skip_if_not_installed("broom")
  d <- gss_cat_data_formatting()[c("rincome", "race", "marital")]
  d <- d[stats::complete.cases(d), ]
  set.seed(2); d$w <- stats::runif(nrow(d), .5, 2)
  # svyolr is NOT a polr subclass and its coef() carries the thresholds too: the engine must read
  # both parameterisations, and take its variance from svyolr's own design-based vcov().
  t <- suppressMessages(suppressWarnings(
    tab_reg(d, outcome = "rincome", predictors = c("race", "marital"), family = "ordinal",
            effect = "marginal", wt = "w", cleannames = FALSE)))
  testthat::expect_true("Model_mD" %in% names(t))
  testthat::expect_true(any(is.finite(get_ci_inf(t[["Model_mD"]]))))
  # ... while a weighted MULTINOMIAL marginal quantity still has no method at all
  testthat::expect_error(suppressMessages(
    tab_reg(d, outcome = "rincome", predictors = "race", family = "multinomial",
            effect = "marginal", wt = "w")), "coefficients")
})
