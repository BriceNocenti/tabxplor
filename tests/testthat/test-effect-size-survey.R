# Last Phase j: the whole-table effect sizes (Cramer's V / phi / eta^2), auto Fisher's exact on small
# weak tables, and the opt-in robust omnibus tests (Kish n_eff first-order Rao-Scott; survey-design
# svychisq / svyglm F). Parity is checked against the reference implementations; every test asserts a
# non-vacuous result (a real statistic, not just "no error").

gss <- forcats::gss_cat

# ---- Cramer's V / phi (factor) --------------------------------------------------------------------

test_that("Cramer's V matches the uncorrected chi2 formula (and DescTools)", {
  t  <- tab(gss, marital, race, pct = "row", test = TRUE)
  te <- get_test(t)
  v  <- te$effect_size[te$test == "chi2"]
  expect_equal(te$es_type[te$test == "chi2"], "cramer_v")
  expect_true(is.finite(v) && v > 0)

  # manual: sqrt(X2_uncorrected / (N * (min(r,c) - 1))) on the same (empty-margin-dropped) table
  m  <- table(gss$marital, gss$race)
  m  <- m[rowSums(m) > 0, colSums(m) > 0]
  x2 <- unname(suppressWarnings(stats::chisq.test(m, correct = FALSE)$statistic))
  expect_equal(unname(v), sqrt(x2 / (sum(m) * (min(dim(m)) - 1))), tolerance = 1e-6)

  skip_if_not_installed("DescTools")
  expect_equal(unname(v), DescTools::CramerV(m, correct = FALSE), tolerance = 1e-6)
})

test_that("a 2x2 table reports phi", {
  d  <- gss[gss$marital %in% c("Married", "Divorced") & gss$race %in% c("White", "Black"), ]
  d$marital <- droplevels(d$marital); d$race <- droplevels(d$race)
  te <- get_test(tab(d, marital, race, pct = "row", test = TRUE))
  expect_equal(te$es_type[te$test == "chi2"], "phi")
  expect_true(te$effect_size[te$test == "chi2"] > 0)
})

# ---- eta^2 (numeric) ------------------------------------------------------------------------------

test_that("eta^2 matches SSB / SST from lm", {
  te  <- get_test(tab(gss, marital, tvhours, test = TRUE))
  e   <- unique(te$effect_size[te$test %in% c("F_welch", "F_classic")])
  expect_length(e, 1L)
  d   <- gss[!is.na(gss$tvhours), ]
  av  <- stats::anova(stats::lm(tvhours ~ marital, d))
  expect_equal(e, av$`Sum Sq`[1] / sum(av$`Sum Sq`), tolerance = 1e-6)
  expect_equal(te$es_type[te$test == "F_welch"], "eta2")
})

# ---- Fisher's exact (auto on small weak tables) ---------------------------------------------------

test_that("a small sparse table gets an exact Fisher p (matching fisher.test)", {
  # a deliberately tiny, sparse 3x2 table -> min_e < 5 and N small enough for the exact test
  set.seed(1)  # only affects nothing here; kept for clarity
  d <- data.frame(
    g = factor(rep(c("a", "b", "c"), c(6, 6, 6))),
    y = factor(c("no","no","no","no","no","yes", "no","no","yes","yes","yes","yes",
                 "yes","yes","yes","yes","yes","no")))
  te <- get_test(tab(d, g, y, pct = "row", test = TRUE))
  pe <- te$pvalue_exact[te$test == "chi2"]
  expect_true(is.finite(pe))
  m  <- table(d$g, d$y)
  expect_equal(pe, stats::fisher.test(m)$p.value, tolerance = 1e-8)
})

test_that("a large table's chi2 is NOT overridden by a simulated Fisher p (pvalue_exact stays NA)", {
  # marital x race is weak (one rare category) but N huge -> exact infeasible -> keep the chi2
  te <- get_test(tab(gss, marital, race, pct = "row", test = TRUE))
  expect_true(is.na(te$pvalue_exact[te$test == "chi2"]))
})

# ---- Kish n_eff (first-order Rao-Scott), opt-in ---------------------------------------------------

test_that("Kish factor chi2 rescales the weighted chi2 to n_eff", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  withr::local_options(tabxplor.kish_neff = TRUE)
  te <- get_test(tab(apistrat, stype, awards, wt = pw, test = TRUE))
  expect_equal(te$test[1], "chi2_kish")
  expect_true(is.finite(te$pvalue) && te$pvalue > 0)
  # n reported is the effective n = (sum w)^2 / sum w^2, below the raw 200
  expect_true(te$n[1] < nrow(apistrat) && te$n[1] > 1)
})

# ---- Survey design (Rao-Scott), opt-in ------------------------------------------------------------

test_that("survey factor test matches survey::svychisq", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  des <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  te  <- suppressMessages(get_test(tab(des, sch.wide, awards, pct = "row", test = TRUE)))
  ref <- survey::svychisq(~sch.wide + awards, des, statistic = "F")
  expect_equal(te$test[1], "chi2_svy")
  expect_equal(te$statistic[1], unname(ref$statistic), tolerance = 1e-6)
  expect_equal(te$pvalue[1],    unname(ref$p.value),   tolerance = 1e-6)
})

test_that("survey numeric F matches svyglm + regTermTest", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  des <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  te  <- suppressMessages(get_test(tab(des, stype, api00, test = TRUE)))
  fit <- survey::svyglm(api00 ~ stype, des)
  ref <- survey::regTermTest(fit, ~stype, method = "Wald")
  expect_equal(te$test[1], "F_svy")
  expect_equal(te$pvalue[1], as.double(ref$p), tolerance = 1e-6)
})

test_that("survey test also works from wt + strata args (no design object)", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  te  <- get_test(tab(apistrat, sch.wide, awards, wt = pw, strata = "stype", fpc = "fpc",
                      test = "survey"))
  des <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  ref <- survey::svychisq(~sch.wide + awards, des, statistic = "F")
  expect_equal(te$statistic[1], unname(ref$statistic), tolerance = 1e-6)
  expect_equal(te$pvalue[1],    unname(ref$p.value),   tolerance = 1e-6)
})

test_that("the classic default path is unaffected (no robust columns, effect size present)", {
  te <- get_test(tab(gss, marital, race, pct = "row", test = TRUE))
  expect_true(all(te$test %in% c("chi2", "F_welch", "F_classic")))
  expect_true(is.finite(te$effect_size[te$test == "chi2"]))
})
