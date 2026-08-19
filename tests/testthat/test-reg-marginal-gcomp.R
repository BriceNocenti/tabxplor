# PURPOSE: Phase 20d -- the ANALYTIC marginal engine. `reg_gcomp_maker()` / `reg_gcomp_cat_maker()`
#          (R/reg-influence.R) compute an average marginal effect, its adjusted predictions and an
#          ANALYTIC jacobian in one counterfactual sweep; `reg_marginal_gcomp()` turns that into the
#          printed estimate + delta-method interval. It replaced `marginaleffects`' numerical jacobian,
#          which cost one full re-prediction per model coefficient (measured 10.0 s -> 1.2 s on a
#          4-predictor logit over 13 000 rows, and 45.2 s -> 5.2 s on a 3-level multinomial).
# ROLE: the numerical lock. The whole claim of the phase is *identical maths by a cheaper route*, so
#       every declared `engine = "gcomp"` row is pinned against the engine it replaced, on the SAME
#       fit, through the SAME entry point -- estimate, both bounds, the p-value and the adjusted
#       prediction alike. A route with no fixture here must stay declared "marginaleffects".
# KEY CONSTRAINTS:
#   - The two engines are compared through `reg_marginal()` itself, not against a hand-written formula:
#     what must agree is the whole returned object, keys and labels included.
#   - The fast route is ALL-OR-NOTHING per call. If any contrast refuses, `reg_marginal()` falls back
#     for the whole call, so one column can never mix two conventions.
#   - `reg_delta_se()` (what a marginal effect PRINTS) and `reg_if_se()` (what the adjustment-gap test
#     needs) are NOT interchangeable: the influence-function SE is a sandwich variance plus the
#     empirical-averaging term, measured up to 3.6 % away. That separation is asserted here.
# See: R/reg-influence.R's reg_gcomp_maker() note, and CLAUDE.md Phase 20d.

skip_if_no_me <- function() testthat::skip_if_not_installed("marginaleffects")

mg_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- as.integer(d$marital == "Married")
  d$hours   <- as.integer(pmax(0, round(d$tvhours)))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d <- d[!is.na(d$race) & !is.na(d$tvhours) & !is.na(d$age), , drop = FALSE]
  d <- d[seq(1, nrow(d), by = 3), , drop = FALSE]        # thinned: parity is exact at any n
  d$w <- 0.5 + (seq_len(nrow(d)) %% 7) / 4               # a deterministic, non-degenerate weight
  tibble::as_tibble(d)
}

# The whole point of the phase, asserted as one function: the two engines must answer identically
# through reg_marginal() -- same keys, same estimates, same bounds, same p-values, same predictions.
expect_engines_agree <- function(fit, data, predictors, ..., tol = 1e-6, want_pred = TRUE) {
  fast <- tabxplor:::reg_marginal(fit, data, predictors, 0.95, ...,
                                  want_pred = want_pred, engine = "gcomp")
  slow <- tabxplor:::reg_marginal(fit, data, predictors, 0.95, ...,
                                  want_pred = want_pred, engine = "marginaleffects")
  key <- function(x) paste(x$var, x$level, x$group, sep = "\r")
  testthat::expect_setequal(key(fast$ame), key(slow$ame))
  i <- match(key(slow$ame), key(fast$ame))
  for (f in c("ame", "ame_lo", "ame_hi", "ame_p"))
    testthat::expect_equal(fast$ame[[f]][i], slow$ame[[f]], tolerance = tol, info = f)
  if (want_pred && nrow(slow$pred)) {
    testthat::expect_setequal(key(fast$pred), key(slow$pred))
    j <- match(key(slow$pred), key(fast$pred))
    testthat::expect_equal(fast$pred$pred[j], slow$pred$pred, tolerance = tol, info = "pred")
  }
  invisible(fast)
}

# --- the declaration -------------------------------------------------------------------------------

testthat::test_that("the estimand table declares which engine computes each marginal quantity", {
  eng <- function(...) tabxplor:::reg_marginal_engine(tabxplor:::reg_estimand(...))
  for (fam in c("gaussian", "binomial", "poisson", "multinomial", "ordinal")) {
    testthat::expect_equal(eng(fam, "marginal", "difference"), "gcomp")
    # a one-row profile grid is not something g-computation builds: declared numeric, on purpose.
    testthat::expect_equal(eng(fam, "at_reference", "difference"), "marginaleffects")
  }
  testthat::expect_equal(eng("multinomial", "at_reference", "odds_ratio"), "marginaleffects")
  # an explicit value overrides the rule, which is what makes a row opt-out-able
  testthat::expect_equal(
    tabxplor:::reg_marginal_engine(list(effect = "marginal", engine = "marginaleffects")),
    "marginaleffects")
  testthat::expect_true(all(c("gcomp", "marginaleffects") %in% tabxplor:::REG_MARGINAL_ENGINES))
})

# --- single-equation parity ------------------------------------------------------------------------

testthat::test_that("gaussian / binomial / poisson marginal effects match marginaleffects exactly", {
  skip_if_no_me()
  d <- mg_data()
  preds <- c("race", "party3", "age")
  fits <- list(
    gaussian = stats::lm(hours ~ race + party3 + age, data = d),
    binomial = stats::glm(married ~ race + party3 + age, data = d, family = stats::binomial()),
    poisson  = stats::glm(hours ~ race + party3 + age, data = d, family = stats::poisson()))
  for (nm in names(fits)) {
    f <- fits[[nm]]
    expect_engines_agree(f, f$model, preds)                # additive: `comparison = NULL`
  }
})

testthat::test_that("the RATIO contrast (comparison = 'lnratioavg') matches too", {
  skip_if_no_me()
  d <- mg_data()
  f <- stats::glm(married ~ race + party3 + age, data = d, family = stats::binomial())
  expect_engines_agree(f, f$model, c("race", "party3", "age"), comparison = "lnratioavg")
  g <- stats::glm(hours ~ race + age, data = d, family = stats::poisson())
  expect_engines_agree(g, g$model, c("race", "age"), comparison = "lnratioavg")
})

testthat::test_that("a numeric predictor's `multiplier` is the same k-unit forward difference", {
  skip_if_no_me()
  d <- mg_data()
  f <- stats::glm(married ~ race + age, data = d, family = stats::binomial())
  expect_engines_agree(f, f$model, c("race", "age"), multiplier = c(age = 10))
  # and it is NOT k x the 1-unit effect (the honest nonlinear quantity, Phase 18z9)
  one <- tabxplor:::reg_marginal(f, f$model, "age", 0.95, engine = "gcomp")$ame$ame
  ten <- tabxplor:::reg_marginal(f, f$model, "age", 0.95, multiplier = c(age = 10),
                                 engine = "gcomp")$ame$ame
  testthat::expect_false(isTRUE(all.equal(ten, 10 * one, tolerance = 1e-8)))
})

testthat::test_that("a weighted survey design agrees, and takes its variance from the design", {
  skip_if_no_me()
  testthat::skip_if_not_installed("survey")
  d   <- mg_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  f   <- survey::svyglm(married ~ race + age, design = des, family = stats::quasibinomial())
  dd  <- f$model
  dd$w <- d$w[as.integer(rownames(dd))]
  expect_engines_agree(f, dd, c("race", "age"), wt = "w")
})

# --- 3+ level parity -------------------------------------------------------------------------------

testthat::test_that("multinomial and ordinal marginal effects match, every outcome category", {
  skip_if_no_me()
  testthat::skip_if_not_installed("nnet")
  testthat::skip_if_not_installed("MASS")
  d <- mg_data()
  m <- nnet::multinom(party3 ~ race + age, data = d, trace = FALSE)
  expect_engines_agree(m, d, c("race", "age"))
  expect_engines_agree(m, d, c("race", "age"), comparison = "lnratioavg")

  d$inc3 <- factor(dplyr::ntile(d$age, 3), labels = c("low", "mid", "high"))
  p <- MASS::polr(inc3 ~ race + hours, data = d, Hess = TRUE)
  expect_engines_agree(p, d, c("race", "hours"))
})

# --- the refusals ----------------------------------------------------------------------------------

testthat::test_that("the fast route refuses rather than guessing, and the call falls back whole", {
  skip_if_no_me()
  d <- mg_data()
  f <- stats::glm(married ~ race + age, data = d, family = stats::binomial())
  # a predictor that is not in the model has no counterfactual: refuse (the compound-formula path)
  testthat::expect_null(
    tabxplor:::reg_marginal_gcomp(f, f$model, c("race", "party3"), 0.95))
  # an absent factor level is no answer, not an NA column
  g <- tabxplor:::reg_gcomp_maker(f, f$model, NULL, FALSE)
  testthat::expect_null(g("race", "Martian", "White"))
  testthat::expect_false(is.null(g("race", "Black", "White")))
  # `at = "reference"` never takes the fast route even when asked
  fast <- tabxplor:::reg_marginal(f, f$model, "race", 0.95, at = "reference", engine = "gcomp")
  slow <- tabxplor:::reg_marginal(f, f$model, "race", 0.95, at = "reference",
                                  engine = "marginaleffects")
  testthat::expect_equal(fast$ame$ame, slow$ame$ame)
})

testthat::test_that("want_se = FALSE keeps the estimate and drops only the interval", {
  skip_if_no_me()
  d <- mg_data()
  f <- stats::glm(married ~ race + age, data = d, family = stats::binomial())
  for (e in c("gcomp", "marginaleffects")) {
    no_se <- tabxplor:::reg_marginal(f, f$model, "race", 0.95, engine = e, want_se = FALSE)
    with  <- tabxplor:::reg_marginal(f, f$model, "race", 0.95, engine = e)
    testthat::expect_equal(no_se$ame$ame, with$ame$ame)
    testthat::expect_true(all(is.na(no_se$ame$ame_lo)))
    testthat::expect_true(all(is.na(no_se$ame$ame_p)))
    testthat::expect_equal(no_se$pred$pred, with$pred$pred)
  }
})

# --- the counterfactual's own contract -------------------------------------------------------------

testthat::test_that("the counterfactual preserves the column, an ORDERED factor included", {
  d <- mg_data()
  d$ord <- factor(dplyr::ntile(d$age, 3), labels = c("a", "b", "c"), ordered = TRUE)
  cf <- tabxplor:::reg_counterfactual(d, "ord", "b")
  testthat::expect_true(is.ordered(cf$ord))                     # `factor()` would drop this
  testthat::expect_identical(levels(cf$ord), levels(d$ord))
  testthat::expect_true(all(cf$ord == "b"))
  # ...and that is what keeps polynomial contrasts polynomial. An ordered predictor fitted as such
  # gives a DIFFERENT design matrix from a de-ordered one, so a dropped class is a wrong number, not
  # a wrong label. (Phase 14r's reg_fit() de-orders predictors, so tab_reg() never reaches this.)
  skip_if_no_me()
  f <- stats::glm(married ~ ord + race, data = d, family = stats::binomial())
  expect_engines_agree(f, f$model, c("ord", "race"))
  # numeric arm: the level is a SHIFT, not a value
  testthat::expect_equal(tabxplor:::reg_counterfactual(d, "age", 10)$age, d$age + 10)
})

# --- the two standard errors are different quantities, on purpose ----------------------------------

testthat::test_that("the printed delta-method SE is not the influence-function SE", {
  skip_if_no_me()
  d <- mg_data()
  f <- stats::glm(married ~ race + party3 + age, data = d, family = stats::binomial())
  dd  <- f$model
  ref <- levels(dd$race)[1]                                   # the fit's own baseline, not a guess
  g   <- tabxplor:::reg_gcomp_maker(f, dd, NULL, FALSE)
  p   <- g("race", "Black", ref)
  se_print <- tabxplor:::reg_delta_se(p$G, stats::vcov(f))
  se_gap   <- tabxplor:::reg_if_se(tabxplor:::reg_ame_if_maker(
    f, dd, NULL, FALSE, tabxplor:::reg_coef_if_maker(f))("race", "Black", ref))
  me <- as.data.frame(marginaleffects::avg_comparisons(f, newdata = dd, variables = "race"))
  me <- me[me$contrast == paste("Black", "-", ref), ]
  # the printed one IS marginaleffects'; the gap one is a sandwich variance plus the
  # empirical-averaging term, and it must NOT be substituted for the printed interval
  testthat::expect_equal(se_print, me$std.error, tolerance = 1e-6)
  testthat::expect_true(is.finite(se_gap) && se_gap > 0)
  testthat::expect_false(isTRUE(all.equal(se_gap, se_print, tolerance = 1e-6)))
})

# --- end to end ------------------------------------------------------------------------------------

testthat::test_that("every predictor keeps its crude twin, not just the first", {
  skip_if_no_me()
  testthat::skip_if_not_installed("MASS")
  # The regression lock for a variable-shadowing defect Phase 20d exposed: reg_empirical_fit()'s
  # marginal branch assigned its per-predictor estimates to `est`, which is also its ESTIMAND-ROW
  # argument. Harmless while nothing read that argument twice; the moment the engine is read off it
  # per predictor, every predictor after the first aborted inside a tryCatch and lost its `obs` in
  # silence. `obs` is what `color = "adjustment"` scores, so the loss was invisible in the values.
  d <- mg_data()
  d$inc3 <- factor(dplyr::ntile(d$age, 3), labels = c("low", "mid", "high"), ordered = TRUE)
  x <- suppressMessages(suppressWarnings(
    tab_reg(d, outcome = "inc3", predictors = c("race", "party3"),
            effect = "marginal", empirical = TRUE)))
  mods <- names(x)[purrr::map_lgl(x, is_fmt) & grepl("[^n]", names(x))]
  obs  <- get_obs(x[[utils::tail(mods, 1)]])
  keep <- !is_totrow(x[[utils::tail(mods, 1)]]) & as.character(x$var) %in% c("race", "party3")
  for (v in c("race", "party3"))
    testthat::expect_true(any(!is.na(obs[keep & as.character(x$var) == v])),
                          info = paste("no crude value on", v))
})

testthat::test_that("a built AME table carries the analytic interval and its stars", {
  skip_if_no_me()
  d <- mg_data()
  d$married <- factor(d$married, labels = c("no", "yes"))
  x <- suppressMessages(tab_reg(d, outcome = "married", predictors = c("race", "age"),
                                effect = "marginal"))
  col <- x[[grep("^Model", names(x))[1]]]
  fin <- !is.na(get_pvalue(col))   # a reference cell carries the neutral, and no interval
  testthat::expect_true(any(fin))
  testthat::expect_true(all(is.finite(get_ci_inf(col)[fin])))
  testthat::expect_true(all(get_ci_inf(col)[fin] <= get_diff(col)[fin]))
  testthat::expect_true(all(get_ci_sup(col)[fin] >= get_diff(col)[fin]))
  testthat::expect_true(all(!is.na(get_pvalue(col)[fin])))
})
