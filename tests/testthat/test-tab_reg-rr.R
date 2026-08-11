# Last Phase z3: the two RISK-RATIO routes.
#   (1) family = "poisson" on a BINARY outcome  -> modified Poisson (Zou 2004), a CONDITIONAL risk ratio
#       with a robust sandwich variance (internal family key "rr").
#   (2) effect = "ame_ratio"                    -> marginal standardization / g-computation on the
#       ordinary logistic fit, a MARGINAL risk ratio.
# The governing claims these tests pin:
#   - the modified-Poisson SE is the SANDWICH, not the naive Poisson SE and not the phi-scaled one;
#   - with ONE predictor, both routes reproduce the CRUDE risk ratio exactly (the empirical companion);
#   - a genuine COUNT poisson model is untouched by any of it.
# gss_cat-derived data only.

rr_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$inc3    <- factor(dplyr::case_when(d$rincome %in% c("$25000 or more") ~ "hi",
                                       d$rincome %in% c("Not applicable", "No answer",
                                                        "Don't know", "Refused") ~ NA_character_,
                                       TRUE ~ "lo"),
                      levels = c("lo", "hi"))
  d <- d[!is.na(d$married) & !is.na(d$race) & !is.na(d$inc3) & !is.na(d$tvhours), , drop = FALSE]
  tibble::as_tibble(d)
}

# The 0/1 numeric the "rr" arm actually fits: reg_prep_binary picks the modelled ("positive") level,
# honouring inverse_two_level_factors, then coerces to numeric.
rr_y01 <- function(d, dep = "married", inverse = TRUE)
  as.numeric(as.character(d[[dep]]) == reg_positive_level(d, dep, inverse))

# ---- (1) modified Poisson: the fit and its variance ------------------------------------------------

test_that("family='poisson' on a binary FACTOR fits (it used to abort) and is named a risk ratio", {
  d <- rr_data()
  expect_message(t <- tab_reg(d, "married", "race", family = "poisson"), "modified Poisson")
  # the column is Model_RR (not Model_IRR, not Model_exp(beta)), and the estimand prose says so
  expect_true("Model_RR" %in% names(t))
  expect_false(any(grepl("IRR", names(t))))
  note <- reg_model_note("rr", do_exp = TRUE)
  expect_match(note, "risk ratios")
  expect_no_match(note, "incidence-rate")
  # Sociology terminology trap: "log-linear model" means Goodman's contingency-table models.
  expect_no_match(reg_family_display_name("rr"), "log-linear")
  expect_equal(reg_family_display_name("rr"), "modified Poisson regression")
})

test_that("modified Poisson == svyglm(quasipoisson) on a constant-weight design, to the last digit", {
  skip_if_not_installed("survey")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", cleannames = FALSE))
  d$y <- rr_y01(d)
  sv <- survey::svyglm(y ~ race, family = stats::quasipoisson("log"),
                       design = survey::svydesign(ids = ~1, weights = ~1, data = d))
  ci <- stats::confint(sv)
  # skeleton rows: 1 = Constant, 2..4 = the race levels (2 = reference)
  expect_equal(unname(get_or(t$Model_RR)[3:4]),     unname(exp(stats::coef(sv))[2:3]),  tolerance = 1e-10)
  expect_equal(unname(get_ci_inf(t$Model_RR)[3:4]), unname(exp(ci[2:3, 1])),            tolerance = 1e-10)
  expect_equal(unname(get_ci_sup(t$Model_RR)[3:4]), unname(exp(ci[2:3, 2])),            tolerance = 1e-10)
})

test_that("the SE is the HC0 sandwich (not the naive Poisson SE, not the phi-scaled one)", {
  skip_if_not_installed("survey")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "inc3"), family = "poisson",
                                cleannames = FALSE))
  d$y <- rr_y01(d)
  g  <- stats::glm(y ~ race + inc3, data = d, family = stats::poisson("log"))
  X  <- stats::model.matrix(g); mu <- stats::fitted(g); n <- nrow(X)
  # Zou (2004)'s Huber-White sandwich, hand-computed -- no `sandwich` dependency.
  bread <- solve(t(X) %*% (X * mu)); meat <- t(X) %*% (X * (d$y - mu)^2)
  hc0   <- sqrt(diag(bread %*% meat %*% bread))

  # recover the model's own SE from the stored interval: the CI is symmetric on the LOG scale, so
  # se == (log(sup) - log(inf)) / (2 * crit).
  crit   <- stats::qt(0.975, df = stats::df.residual(g))
  se_tab <- (log(get_ci_sup(t$Model_RR)) - log(get_ci_inf(t$Model_RR))) / (2 * crit)
  # the non-NA rows are the Constant (the intercept) + each non-reference level, in model-matrix order
  se_tab <- se_tab[!is.na(se_tab)]
  # The design-based variance is the sandwich up to survey's own finite-sample factor, measured at
  # sqrt(n/(n-1)) to six digits (a ~1e-6 residual comes from survey's internal df handling and is not
  # worth reproducing by hand -- the EXACT contract is pinned against svyglm itself in the test above).
  # 1e-4 here is still ~100x tighter than the phi gap and ~1000x tighter than the naive gap below.
  expect_equal(unname(se_tab), unname(hc0 * sqrt(n / (n - 1))), tolerance = 1e-4)

  # ...and it is NOT the naive Poisson SE, nor the phi-scaled SE a COUNT poisson would use. Both gaps
  # are large on real data (measured: naive ~49% too wide, phi ~9% off, and phi is off in BOTH
  # directions across coefficients of the same fit, so it is not a calibratable offset). Rule 7: these
  # fail if the "rr" arm ever falls back onto the plain glm / phi-scaling path.
  naive <- summary(g)$coef[, 2]
  phi   <- sum(stats::residuals(g, "pearson")^2) / stats::df.residual(g)
  expect_gt(max(abs(se_tab / naive - 1)), 0.10)
  expect_gt(max(abs(se_tab / (naive * sqrt(phi)) - 1)), 0.01)
})

test_that("the modelled level is the binomial one, and inverse_two_level_factors flips it", {
  d <- rr_data()
  t1 <- suppressMessages(tab_reg(d, "married", "race", family = "poisson"))
  t2 <- suppressMessages(tab_reg(d, "married", "race", family = "poisson",
                                 inverse_two_level_factors = FALSE))
  expect_equal(get_reg_meta(t1)$positive_level, reg_positive_level(d, "married", TRUE))
  expect_equal(get_reg_meta(t2)$positive_level, reg_positive_level(d, "married", FALSE))
  expect_false(identical(get_reg_meta(t1)$positive_level, get_reg_meta(t2)$positive_level))
})

test_that("the estimand invariant holds: the OR is always further from 1 than the RR", {
  d  <- rr_data()
  or <- get_or(suppressMessages(tab_reg(d, "married", "race", family = "binomial"))$Model_OR)[3:4]
  rr <- get_or(suppressMessages(tab_reg(d, "married", "race", family = "poisson"))$Model_RR)[3:4]
  # The OR always EXAGGERATES, away from 1, whichever side the effect falls on -- stated
  # direction-agnostically as |log(OR)| > |log(RR)|, and both must sit on the same side of 1.
  expect_true(all(abs(log(or)) > abs(log(rr))))
  expect_true(all(sign(log(or)) == sign(log(rr))))
})

# ---- (1b) the crude companion --------------------------------------------------------------------

test_that("Obs_RR is the crude RISK ratio with a Katz interval (never the crude ODDS ratio)", {
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  lv <- levels(d$race)
  p  <- vapply(lv, function(l) mean(rr_y01(d)[d$race == l]), numeric(1))
  nn <- vapply(lv, function(l) sum(d$race == l), numeric(1))
  expect_equal(unname(get_or(t$Obs_RR)[3:4]), unname(p[-1] / p[1]), tolerance = 1e-10)
  ci <- ci_katz_rr(p[-1], nn[-1], p[1], nn[1], conf_level = 0.95, want_p = TRUE)
  expect_equal(unname(get_ci_inf(t$Obs_RR)[3:4]), unname(ci$inf), tolerance = 1e-10)
  # the crude ODDS ratio would be a DIFFERENT number -- guarding the emp_ratio trap
  odds <- (p[-1] / (1 - p[-1])) / (p[1] / (1 - p[1]))
  expect_gt(max(abs(get_or(t$Obs_RR)[3:4] / odds - 1)), 0.01)
})

test_that("with ONE predictor the model RR == the crude Obs_RR exactly", {
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  # rows 2..4 are the race levels; row 1 is the Constant (a model intercept the crude column has no
  # counterpart for, hence NA there).
  expect_equal(get_or(t$Model_RR)[2:4], get_or(t$Obs_RR)[2:4], tolerance = 1e-9)
})

# ---- (1c) the footer, the guards, and the un-exponentiated scale ----------------------------------

test_that("the footer reports n + Wald-vs-null only (no AIC/BIC/McFadden/dispersion)", {
  d  <- rr_data()
  t  <- suppressMessages(tab_reg(d, "married", "race", family = "poisson"))
  tt <- get_test(t)
  expect_true(all(c("n", "wald_null") %in% tt$test))
  # a quasi-likelihood has no AIC/BIC/McFadden; binary Pearson dispersion is just mean(1-mu).
  expect_false(any(c("aic", "bic", "mcfadden_r2", "lr_null", "dispersion") %in% tt$test))
  # z13: "global" (the per-predictor overall test) joins every default set -- it renders as a footer
  # LINE, so it adds no GOF row and none of the quasi-likelihood stats above.
  expect_equal(reg_footer_stats("rr", weighted = FALSE, grouped = FALSE, stats = NULL),
               c("n", "wald_null", "global"))
})

test_that("method='profile' is refused for a modified Poisson and degrades to the robust Wald", {
  d <- rr_data()
  # it SAYS so (a profile likelihood on a deliberately misspecified quasi-likelihood is meaningless)...
  expect_message(tab_reg(d, "married", "race", family = "poisson", method = "profile"),
                 "quasi-likelihood")
  # ...and the interval it returns is exactly the robust Wald one.
  tp <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", method = "profile"))
  tw <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", method = "wald"))
  expect_equal(get_ci_inf(tp$Model_RR), get_ci_inf(tw$Model_RR), tolerance = 1e-12)
  expect_equal(get_ci_sup(tp$Model_RR), get_ci_sup(tw$Model_RR), tolerance = 1e-12)
})

test_that("exponentiate=FALSE colours the log-RR coefficient on the log scale (is_logcoef)", {
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", exponentiate = FALSE,
                                empirical = TRUE, cleannames = FALSE))
  cf <- t[[grep("^Model", names(t), value = TRUE)[1]]]
  expect_equal(get_model_family(cf), "rr")
  expect_true(reg_fam_logscale("rr"))
  # the crude twin is the LOGGED risk ratio, matching the model's link scale
  expect_true("Obs_log(RR)" %in% names(t))
  expect_equal(get_diff(t[["Obs_log(RR)"]])[3:4],
               log(get_or(suppressMessages(tab_reg(d, "married", "race", family = "poisson",
                                                   empirical = TRUE, cleannames = FALSE))$Obs_RR))[3:4],
               tolerance = 1e-10)
})

# ---- (1d) NON-REGRESSION: a genuine count model is untouched --------------------------------------

test_that("a real COUNT poisson keeps its IRR, its dispersion row and its over-dispersion warning", {
  d <- rr_data()
  expect_warning(t <- tab_reg(d, "tvhours", "race", family = "poisson"), "Over-dispersion")
  expect_true("Model_IRR" %in% names(t))
  expect_false(any(grepl("_RR", names(t))))
  expect_true("dispersion" %in% get_test(t)$test)
  expect_equal(get_model_family(t$Model_IRR), "poisson")
})

# ---- (2) effect = "ame_ratio": the marginal risk ratio --------------------------------------------

test_that("ame_ratio == marginaleffects' lnratioavg contrast, exponentiated", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "ame_ratio",
                                cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  d$y <- rr_y01(d)
  lg <- stats::glm(y ~ race, data = d, family = stats::binomial())
  r  <- as.data.frame(marginaleffects::avg_comparisons(
    lg, variables = "race", comparison = "lnratioavg", newdata = d))
  expect_equal(unname(get_or(t[[nm]])[3:4]), unname(exp(r$estimate)),  tolerance = 1e-10)
  expect_equal(unname(get_ci_inf(t[[nm]])[3:4]), unname(exp(r$conf.low)), tolerance = 1e-10)
})

test_that("the ame_ratio cell is coherent: adjusted%(ref) * RR == adjusted%(level)", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "inc3"), family = "binomial",
                                effect = "ame_ratio", cleannames = FALSE))
  nm  <- grep("^Model", names(t), value = TRUE)[1]
  pct <- get_pct(t[[nm]]); or <- get_or(t[[nm]])
  # this is the identity the "prob_ratio" shape exists for -- the multiplicative twin of the AME's
  # adjusted%(ref) + AME == adjusted%(level).
  for (v in unique(as.character(t$var))) {
    i <- which(as.character(t$var) == v & !is.na(pct))
    if (length(i) < 2) next
    expect_equal(pct[i], pct[i[1]] * or[i], tolerance = 1e-10)
  }
})

test_that("ame_ratio: label parsing survives a level containing ' - ' and ')'", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # the Phase 14r regression class, re-armed for the new "ln(mean(L) / mean(R))" prefix/suffix
  d$tricky <- factor(ifelse(d$race == "White", "a (x) - b", "c - d (y)"))
  t <- suppressMessages(tab_reg(d, "married", "tricky", family = "binomial",
                                effect = "ame_ratio", cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_false(any(is.na(get_or(t[[nm]])[2:3])))   # both levels keyed to the skeleton
})

test_that("ame_ratio: numeric predictors work and the crude twin is the Katz Obs_RR", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "tvhours"), family = "binomial",
                                effect = "ame_ratio", empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  i  <- which(as.character(t$var) == "tvhours")
  expect_false(is.na(get_or(t[[nm]])[i]))
  expect_true("Obs_RR" %in% names(t))
  lv <- levels(d$race)
  p  <- vapply(lv, function(l) mean(rr_y01(d)[d$race == l]), numeric(1))
  expect_equal(unname(get_or(t$Obs_RR)[3:4]), unname(p[-1] / p[1]), tolerance = 1e-10)
})

test_that("ame_ratio is refused for gaussian / poisson outcomes, naming the outcome", {
  d <- rr_data()
  expect_error(suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian",
                                        effect = "ame_ratio")),
               "probability-scale")
  expect_error(suppressMessages(tab_reg(d, "tvhours", "race", family = "poisson",
                                        effect = "ame_ratio")),
               "probability-scale")
})

test_that("ame_ratio colours as a RATIO even with exponentiate = FALSE", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # `exponentiate` is ignored for marginal effects, so effect_shape is "additive" here -- the colour
  # ladder must still pick the multiplicative measure (rule 7: fails on the old `effect != "ame"` test).
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "ame_ratio",
                                exponentiate = FALSE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(get_color(t[[nm]])[1], "OR")
  expect_equal(get_ci_type(t[[nm]]), "or")
})

test_that("ame_ratio: the legend names RR, not OR, on both the model and the crude column", {
  skip_if_not_installed("marginaleffects")
  d  <- rr_data()
  t  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "ame_ratio",
                                 empirical = TRUE, cleannames = FALSE))
  md <- get_reg_meta(t)
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(legend_reg_eff_word(t[[nm]], md), "RR")
  expect_equal(legend_reg_eff_word(t$Obs_RR, md), "RR")   # crude twin, same estimand
  expect_true(is.na(legend_reg_eff_word(t[["Obs_%"]], md)))   # a crude % has no effect word
})

test_that("ame_ratio: with ONE predictor the marginal RR == the crude RR", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # a saturated single-predictor model reproduces the observed rates, so g-computation returns the
  # crude ratio exactly -- the same identity the coefficient path's OR tests use.
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "ame_ratio",
                                empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(get_or(t[[nm]])[3:4], get_or(t$Obs_RR)[3:4], tolerance = 1e-9)
})

test_that("effect='ame' is byte-unchanged by the ame_ratio addition", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "ame",
                                empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  # still an additive risk DIFFERENCE with its "{diff} ({pct})" cell and diff colour
  expect_equal(get_ci_type(t[[nm]]), "diff")
  expect_equal(get_color(t[[nm]])[1], "diff")
  expect_true("Obs_diff" %in% names(t))
  expect_false("Obs_RR" %in% names(t))
})
