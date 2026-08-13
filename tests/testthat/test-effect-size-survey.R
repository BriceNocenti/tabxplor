# Phase 18j: the whole-table effect sizes (Cramer's V / phi / eta^2), auto Fisher's exact on small
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
  withr::local_options(tabxplor.design_effect = TRUE)
  te <- get_test(tab(apistrat, stype, awards, wt = pw, test = TRUE))
  expect_equal(te$test[1], "chi2_design")
  expect_true(is.finite(te$pvalue) && te$pvalue > 0)
  # Phase 18z16-i (W8): `n` is ALWAYS the raw count -- it used to become the effective n here, so
  # one column meant two things depending on a global option. The correction now lives in `deff`.
  expect_equal(te$n[1], nrow(apistrat))
  # `deff` is Rao-Scott's mean generalized design effect: >1 when the weighting costs information,
  # <1 when it buys some (apistrat is a stratified sample, so its weights can). Finite is the claim.
  expect_true(is.finite(te$deff[1]) && te$deff[1] > 0)
})

# ---- Survey design (Rao-Scott), opt-in ------------------------------------------------------------

test_that("survey factor test matches survey::svychisq", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  des <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  te  <- suppressMessages(get_test(tab(des, sch.wide, awards, pct = "row", test = TRUE)))
  ref <- survey::svychisq(~sch.wide + awards, des, statistic = "F")
  expect_equal(te$test[1], "chi2_design")
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
  expect_equal(te$test[1], "F_design")
  expect_equal(te$pvalue[1], as.double(ref$p), tolerance = 1e-6)
})

test_that("the test RUNG is derived from the input, and `test` is TRUE/FALSE only", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  # Phase 18z14-i: ids/strata/fpc/nest are gone -- a design is expressed by BUILDING one. The rung
  # follows what was passed, so there is no `test = "survey"` to ask for and not get.
  des <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  te  <- suppressMessages(get_test(tab(des, sch.wide, awards, test = TRUE)))
  ref <- survey::svychisq(~sch.wide + awards, des, statistic = "F")
  expect_equal(te$test[1],      "chi2_design")
  expect_equal(te$statistic[1], unname(ref$statistic), tolerance = 1e-6)
  expect_equal(te$pvalue[1],    unname(ref$p.value),   tolerance = 1e-6)

  # weights alone -> a weighted chi2; weights + the kish option -> the same rescaled to n_eff
  expect_equal(get_test(tab(apistrat, sch.wide, awards, wt = pw, test = TRUE))$test[1], "chi2")
  withr::local_options(tabxplor.design_effect = TRUE)
  expect_equal(get_test(tab(apistrat, sch.wide, awards, wt = pw, test = TRUE))$test[1], "chi2_design")

  expect_error(tab(apistrat, sch.wide, awards, wt = pw, test = "survey"), "TRUE.*FALSE")
})

test_that("the classic default path is unaffected (no robust columns, effect size present)", {
  te <- get_test(tab(gss, marital, race, pct = "row", test = TRUE))
  expect_true(all(te$test %in% c("chi2", "F_welch", "F_classic")))
  expect_true(is.finite(te$effect_size[te$test == "chi2"]))
})

# === Phase 18z16-iv: the robust omnibus GRID (producer / joiner split) =========================

test_that("a design table with tab_vars keeps its TOTAL-TABLE test row", {
  skip_if_not_installed("survey")
  d <- gss[!is.na(gss$tvhours) & gss$tvhours > 0, ]
  d <- d[d$year %in% c(2000, 2006), ]
  # non-vacuous: the classic path HAS an Ensemble row, so its absence would be a loss, not a shape
  cls <- get_test(tab(d, marital, race, tab_vars = year, pct = "row", test = TRUE,
                      totaltab = "table"))
  expect_true("Ensemble" %in% as.character(cls$year))
  # the overlay used to REPLACE the classic tibble with groups taken from unique(frame[tab_vars]),
  # which has no such level -- so the whole-table test silently vanished under weights / a design.
  rob <- withr::with_options(list(tabxplor.design_effect = TRUE),
                             get_test(tab(d, marital, race, tab_vars = year, wt = tvhours,
                                          pct = "row", test = TRUE, totaltab = "table")))
  expect_true("Ensemble" %in% as.character(rob$year))
  expect_identical(as.character(rob$year), as.character(cls$year))
  expect_true(is.factor(rob$year))                       # not coerced to character by the extra row
  expect_true(all(rob$test == "chi2_design"))
  ens <- rob[as.character(rob$year) == "Ensemble", ]
  expect_equal(ens$n[[1]], nrow(d[!is.na(d$marital) & !is.na(d$race), ]))
})

test_that("an input that cannot serve the weighted basis gets NO design-based test (W-H)", {
  skip_if_not_installed("survey")
  d   <- gss[!is.na(gss$tvhours) & gss$tvhours > 0, ]
  cnt <- as.data.frame(dplyr::count(d, marital, race, name = "n"))
  cnt$wn <- as.data.frame(dplyr::count(d, marital, race, wt = tvhours, name = "wn"))$wn
  withr::local_options(list(tabxplor.design_effect = TRUE))
  t <- tab_counts(cnt, marital, race, counts = n, wt_counts = wn, pct = "row", test = TRUE)
  # pre-aggregated counts carry no per-observation Sigma w^2, so the leaves state basis "n" -- and the
  # whole-table test must say the same thing. It used to run svychisq on the AGGREGATE rows (one
  # "PSU" per aggregate row) and report chi2_design under a footer that said "unweighted sample size".
  expect_identical(tabxplor:::tab_inference_basis(t), "n")
  expect_true(all(get_test(t)$test == "chi2"))
  expect_true(all(is.na(get_test(t)$deff)))
})
