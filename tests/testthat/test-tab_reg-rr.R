# Phase 18z3: the two RISK-RATIO routes.
#   (1) family = "poisson" on a BINARY outcome  -> modified Poisson (Zou 2004), a CONDITIONAL risk ratio
#       with a robust sandwich variance (internal family key "rr").
#   (2) effect = "marginal", measure = "ratio"                    -> marginal standardization / g-computation on the
#       ordinary logistic fit, a MARGINAL risk ratio.
# The governing claims these tests pin:
#   - the modified-Poisson SE is the SANDWICH, not the naive Poisson SE and not the phi-scaled one;
#   - with ONE predictor, both routes reproduce the CRUDE risk ratio exactly (the empirical companion);
#   - a genuine COUNT poisson model is untouched by any of it.
# gss_cat-derived data only.

est_of <- function(x) tabxplor:::fmt_est_of(x)

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
# honouring the modelled level (`outcome_level`), then coerces to numeric.
rr_y01 <- function(d, dep = "married", inverse = TRUE)
  as.numeric(as.character(d[[dep]]) == reg_positive_level(d, dep, inverse))

# ---- (1) modified Poisson: the fit and its variance ------------------------------------------------

test_that("family='poisson' on a binary FACTOR fits (it used to abort) and is named a risk ratio", {
  d <- rr_data()
  expect_message(t <- tab_reg(d, "married", "race", family = "poisson"), "modified Poisson")
  # the column is Model_RR (not Model_IRR, not Model_exp(beta)), and the estimand prose says so
  expect_true("Model_RR" %in% names(t))
  expect_false(any(grepl("IRR", names(t))))
  note <- reg_estimand_note(reg_estimand("binomial", "coefficient", "ratio"))
  expect_match(note, "RR = risk ratio")
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
  expect_equal(unname(est_of(t$Model_RR)[3:4]),     unname(exp(stats::coef(sv))[2:3]),  tolerance = 1e-10)
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

test_that("the modelled level is the binomial one, and `outcome_level` names it", {
  d <- rr_data()                                        # married: levels "no", "yes"
  t1 <- suppressMessages(tab_reg(d, "married", "race", family = "poisson"))
  t2 <- suppressMessages(tab_reg(d, "married", "race", family = "poisson",
                                 outcome_level = c(married = "yes")))
  expect_equal(reg_call(t1)$positive_level, "no")       # the FIRST level, by default
  expect_equal(reg_call(t2)$positive_level, "yes")      # the one that was named
  expect_false(identical(reg_call(t1)$positive_level, reg_call(t2)$positive_level))
})

test_that("the estimand invariant holds: the OR is always further from 1 than the RR", {
  d  <- rr_data()
  or <- get_or(suppressMessages(tab_reg(d, "married", "race", family = "binomial"))$Model_OR)[3:4]
  rr <- est_of(suppressMessages(tab_reg(d, "married", "race", family = "poisson"))$Model_RR)[3:4]
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
  expect_equal(unname(est_of(t$Obs_RR)[3:4]), unname(p[-1] / p[1]), tolerance = 1e-10)
  ci <- ci_katz_rr(p[-1], nn[-1], p[1], nn[1], conf_level = 0.95, want_p = TRUE)
  expect_equal(unname(get_ci_inf(t$Obs_RR)[3:4]), unname(ci$inf), tolerance = 1e-10)
  # the crude ODDS ratio would be a DIFFERENT number -- guarding the emp_ratio trap
  odds <- (p[-1] / (1 - p[-1])) / (p[1] / (1 - p[1]))
  expect_gt(max(abs(est_of(t$Obs_RR)[3:4] / odds - 1)), 0.01)
})

test_that("with ONE predictor the model RR == the crude Obs_RR exactly", {
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  # rows 2..4 are the race levels; row 1 is the Constant (a model intercept the crude column has no
  # counterpart for, hence NA there).
  expect_equal(est_of(t$Model_RR)[2:4], est_of(t$Obs_RR)[2:4], tolerance = 1e-9)
})

# ---- (1c) the footer, the guards, and the un-exponentiated scale ----------------------------------

test_that("the footer reports n + Wald-vs-null only (no AIC/BIC/McFadden/dispersion)", {
  d  <- rr_data()
  t  <- suppressMessages(tab_reg(d, "married", "race", family = "poisson"))
  tt <- get_test(t)
  expect_true(all(c("n", "wald_null") %in% tt$test))
  # a quasi-likelihood has no AIC/BIC/McFadden; binary Pearson dispersion (`phi`, z15) is just
  # mean(1-mu), so it is not reported either.
  expect_false(any(c("aic", "bic", "mcfadden_r2", "lr_null", "phi") %in% tt$test))
  # z13's "global" and the FREE checks join every default set (20f: the ones that refit are opt-in);
  # neither brings a quasi-likelihood statistic with it.
  # 22b-ix: the crossed-pair interaction test joins every glm default set (it produces no row
  # unless `predictors` actually declares an `a:b` pair).
  expect_equal(reg_footer_stats("rr", weighted = FALSE, grouped = FALSE, stats = NULL),
               c("n", "wald_null", "global", "interaction",
                 tabxplor:::reg_check_expand(tabxplor:::reg_checks_default("rr"))))
  # and `stats = "all"` adds exactly the costly ones on top -- nothing else
  expect_setequal(setdiff(reg_footer_stats("rr", FALSE, FALSE, "all"),
                          reg_footer_stats("rr", FALSE, FALSE, NULL)),
                  tabxplor:::reg_check_expand(intersect(tabxplor:::reg_checks_costly(),
                                                        tabxplor:::reg_checks_for("rr"))))
})

test_that("method='profile' is refused for a modified Poisson and degrades to the robust Wald", {
  d <- rr_data()
  # it SAYS so (a profile likelihood on a deliberately misspecified quasi-likelihood is meaningless)...
  expect_message(tab_reg(d, "married", "race", family = "poisson", ci_method = "profile"),
                 "quasi-likelihood")
  # ...and the interval it returns is exactly the robust Wald one.
  tp <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", ci_method = "profile"))
  tw <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", ci_method = "wald"))
  expect_equal(get_ci_inf(tp$Model_RR), get_ci_inf(tw$Model_RR), tolerance = 1e-12)
  expect_equal(get_ci_sup(tp$Model_RR), get_ci_sup(tw$Model_RR), tolerance = 1e-12)
})

test_that("measure = log colours the log-RR coefficient on the log scale (is_logcoef)", {
  d <- rr_data()
  # `measure = "log"` logs the family's DEFAULT estimand (a binomial's odds ratio); `log_risk` pins
  # the modified-Poisson fit, which is what `family = "poisson", exponentiate = FALSE` used to mean.
  t <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", measure = "log_risk",
                                empirical = TRUE, cleannames = FALSE))
  cf <- t[[grep("^Model", names(t), value = TRUE)[1]]]
  expect_equal(get_model_family(cf), "rr")
  # 19l: reg_fam_logscale() is gone -- "this coefficient lives on a log scale" is the column's own
  # STORED scale, which is what the colour engine and the legend have read since 19b.
  expect_equal(get_scale(cf), "log_coef")
  # the crude twin is the LOGGED risk ratio, matching the model's link scale
  expect_true("Obs_log(RR)" %in% names(t))
  expect_equal(get_diff(t[["Obs_log(RR)"]])[3:4],
               log(est_of(suppressMessages(tab_reg(d, "married", "race", family = "poisson",
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

# ---- (2) effect = "marginal", measure = "ratio": the marginal risk ratio --------------------------------------------

test_that("ame_ratio == marginaleffects' lnratioavg contrast, exponentiated", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "ratio",
                                cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  d$y <- rr_y01(d)
  lg <- stats::glm(y ~ race, data = d, family = stats::binomial())
  r  <- as.data.frame(marginaleffects::avg_comparisons(
    lg, variables = "race", comparison = "lnratioavg", newdata = d))
  expect_equal(unname(est_of(t[[nm]])[3:4]), unname(exp(r$estimate)),  tolerance = 1e-10)
  # Phase 20d: the BOUND is looser than the estimate on purpose. Ours comes from an analytic jacobian,
  # marginaleffects' from a finite-difference one, and its own step-size choice (fdforward vs fdcenter)
  # moves this bound by ~4e-9 -- more than we differ from it. The oracle is the approximation here.
  expect_equal(unname(get_ci_inf(t[[nm]])[3:4]), unname(exp(r$conf.low)), tolerance = 1e-7)
})

test_that("the ame_ratio cell is coherent: adjusted%(ref) * RR == adjusted%(level)", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "inc3"), family = "binomial",
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  nm  <- grep("^Model", names(t), value = TRUE)[1]
  pct <- get_pct(t[[nm]]); or <- est_of(t[[nm]])
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
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_false(any(is.na(est_of(t[[nm]])[2:3])))   # both levels keyed to the skeleton
})

test_that("ame_ratio: numeric predictors work and the crude twin is the Katz Obs_RR", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "tvhours"), family = "binomial",
                                effect = "marginal", measure = "ratio", empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  i  <- which(as.character(t$var) == "tvhours")
  expect_false(is.na(est_of(t[[nm]])[i]))
  expect_true("Obs_RR" %in% names(t))
  lv <- levels(d$race)
  p  <- vapply(lv, function(l) mean(rr_y01(d)[d$race == l]), numeric(1))
  expect_equal(unname(est_of(t$Obs_RR)[3:4]), unname(p[-1] / p[1]), tolerance = 1e-10)
})

# Phase 19e (capability gap closed): a marginal RATIO used to be refused for gaussian / poisson
# outcomes ("needs a probability-scale outcome"). A ratio of adjusted MEANS is a sound estimand --
# tab() has given one for years -- so it is offered now, on the mean_ratio scale.
test_that("a marginal ratio is available for gaussian / poisson outcomes", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  tg <- suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian",
                                 effect = "marginal", measure = "ratio", cleannames = FALSE))
  mg <- tg[[grep("^Model", names(tg), value = TRUE)[1]]]
  expect_equal(get_scale(mg), "mean_ratio")
  expect_true(all(get_ratio(mg)[!is.na(get_ratio(mg))] > 0))
})

test_that("a marginal ratio colours as a RATIO (its stored scale, not the contrast)", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # a marginal effect is not a coefficient, but the colour ladder must still pick the MULTIPLICATIVE
  # measure -- and for a RISK ratio that is `ratio` on `pct_ratio`, not the odds ratio's own scale.
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(get_color(t[[nm]])[1], "ratio")
  expect_equal(get_scale(t[[nm]]), "pct_ratio")
})

test_that("ame_ratio: the legend names RR, not OR, on both the model and the crude column", {
  skip_if_not_installed("marginaleffects")
  d  <- rr_data()
  t  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "ratio",
                                 empirical = TRUE, cleannames = FALSE))
  md <- reg_call(t)
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(legend_reg_eff_word(t[[nm]], md), "RR")
  expect_equal(legend_reg_eff_word(t$Obs_RR, md), "RR")   # crude twin, same estimand
  # the base-count column (drawn at display) has no effect word
  m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  expect_true(is.na(legend_reg_eff_word(m[["n"]], md)))
})

test_that("ame_ratio: with ONE predictor the marginal RR == the crude RR", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # a saturated single-predictor model reproduces the observed rates, so g-computation returns the
  # crude ratio exactly -- the same identity the coefficient path's OR tests use.
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "ratio",
                                empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(est_of(t[[nm]])[3:4], est_of(t$Obs_RR)[3:4], tolerance = 1e-9)
})

test_that("effect='ame' is byte-unchanged by the ame_ratio addition", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal",
                                empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  # still an additive risk DIFFERENCE with its "{diff} ({pct})" cell and diff colour
  expect_equal(get_scale(t[[nm]]), "points")
  expect_equal(get_color(t[[nm]])[1], "difference")
  expect_true("Obs_RD" %in% names(t))
  expect_false("Obs_RR" %in% names(t))
})


# --- A LOGGED CRUDE COLUMN RUNS ITS OWN ARITHMETIC -------------------------------------------------

test_that("a logged crude column is the log of ITS measure, never of the family's default", {
  # `measure = "log_risk"` declares the Katz log-RR engine on its own shape. The arm used to be
  # re-derived from the crude family's COEFFICIENT shape, so wherever the two differ -- a summed
  # score (whose block's coefficient is an odds ratio) or a borrowed shape (a binary marginal ratio's
  # crude twin lives in another block) -- `Obs_log(RR)` printed Woolf's log(OR).
  skip_if_not_installed("FactoMineR")
  e <- new.env(); utils::data("tea", package = "FactoMineR", envir = e); tea <- e$tea
  items <- c("home", "tearoom", "work", "friends", "resto", "pub")
  tea$tea_where <- rowSums(vapply(items, function(v) as.integer(tea[[v]] == v),
                                  integer(nrow(tea))))
  tea$sex <- factor(tea$sex)
  obs <- function(m) {
    t <- suppressMessages(tab_reg(tea, "tea_where", "sex", family = "binomial", trials = 6,
                                  measure = m, empirical = "column", stats = FALSE))
    t[[grep("^Obs_", names(t))[[1]]]]
  }
  a  <- sum(tea$tea_where[tea$sex == "M"]);      b  <- sum(6 - tea$tea_where[tea$sex == "M"])
  cc <- sum(tea$tea_where[tea$sex == "F"]);      dd <- sum(6 - tea$tea_where[tea$sex == "F"])
  p1 <- a / (a + b); p0 <- cc / (cc + dd)
  i  <- 3L                                         # the non-reference level's row
  lrr <- obs("log_risk"); lor <- obs("log_odds")
  kz  <- ci_katz_rr(p1, a + b, p0, cc + dd)
  wf  <- ci_or(a, b, cc, dd)
  expect_identical(get_ci_method(lrr), "katz")
  expect_equal(get_diff(lrr)[i],    log(p1 / p0),    tolerance = 1e-10)
  expect_equal(get_ci_inf(lrr)[i],  log(kz$inf),     tolerance = 1e-10)
  expect_equal(get_ci_sup(lrr)[i],  log(kz$sup),     tolerance = 1e-10)
  # ...and it is a DIFFERENT number from the odds-ratio twin, which is what went unnoticed
  expect_identical(get_ci_method(lor), "woolf")
  expect_equal(get_diff(lor)[i], log((a * dd) / (b * cc)), tolerance = 1e-10)
  expect_gt(abs(get_diff(lrr)[i] - get_diff(lor)[i]), 0.05)
})

test_that("a binary MARGINAL log risk ratio takes the borrowed block's Katz arm", {
  # `crude_fam = "rr"` while `crude_key` is "binomial": the shape is borrowed across blocks, so the
  # family in hand is the wrong place to look up which arithmetic to run.
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal",
                                measure = "log_risk", empirical = "column", stats = FALSE))
  o <- t[[grep("^Obs_", names(t))[[1]]]]
  expect_identical(get_scale(o), "log_coef")
  expect_identical(get_ci_method(o), "katz")
  # ⚠ the modelled level is the outcome's FIRST, which is `tab_reg()`'s documented default.
  tb <- table(d$race, d$married)
  p  <- tb[, levels(d$married)[[1]]] / rowSums(tb)
  i  <- which(as.character(t$levels) == names(p)[[2]])
  expect_equal(get_diff(o)[i], log(p[[2]] / p[[1]]), tolerance = 1e-10)
})
