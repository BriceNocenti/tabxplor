# Phase 18z15 -- the five model checks in the regression footer (R/reg-assumptions.R).
#
# Every statistic is pinned against an INDEPENDENT reference, never a hand-written number:
#   Dispersion    a hand-written HC0 sandwich (the `sandwich` package is not a dependency)
#   Influence     stats::dfbetas()
#   Collinearity  car::vif()
#   Linearity     stats::drop1() on a hand-built augmented fit
# so a change of algebra fails here even when the printed table still looks plausible.

chk_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}

# The model tab_reg() actually fits for chk_data(): complete cases, the binary outcome reversed so the
# FIRST level is modelled, factors dropped. Mirrors test-tab_reg-footer.R's own reference recipe.
chk_fit <- function(preds = c("race", "age", "rincome"), family = stats::binomial()) {
  d  <- chk_data()
  dm <- tidyr::drop_na(d, dplyr::all_of(c("married", preds)))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  for (p in preds) if (is.factor(dm[[p]])) dm[[p]] <- forcats::fct_drop(dm[[p]])
  list(fit = stats::glm(stats::reformulate(preds, "married"), data = dm, family = family), data = dm)
}

# The HC0 sandwich, written out: IF = X (W r) A^-1, SE = sqrt(colSums(IF^2)). This IS what
# reg_if_se(reg_coef_if_maker(fit)(e)) computes without a design, and it is what `sandwich` would give.
chk_sandwich_se <- function(fit) {
  X  <- stats::model.matrix(fit)
  W  <- fit$weights
  r  <- stats::residuals(fit, type = "working")
  Ai <- solve(crossprod(X * sqrt(W)))
  IF <- (X * (W * r)) %*% Ai
  sqrt(colSums(IF^2))
}

# ---- Dispersion (robust / model SE) ----------------------------------------------------------

test_that("Dispersion is max(robust/model SE), equal to a hand-written HC0 sandwich", {
  f  <- chk_fit()$fit
  se_mod <- sqrt(diag(stats::vcov(f)))
  ref    <- max(chk_sandwich_se(f) / se_mod)
  expect_equal(tabxplor:::reg_check_dispersion(f), ref, tolerance = 1e-8)
  # a correctly-specified binomial: the two variance estimators agree to O(1/n)
  expect_lt(abs(ref - 1), 0.15)
})

test_that("Dispersion tracks sqrt(phi) on an over-dispersed count, and returns to ~1 under quasi", {
  d  <- chk_data()
  dm <- tidyr::drop_na(d, tvhours, age, race)
  p  <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  q  <- stats::glm(tvhours ~ age + race, data = dm, family = stats::quasipoisson())
  phi <- sum(stats::residuals(p, "pearson")^2) / stats::df.residual(p)
  expect_gt(phi, 1.5)                                    # the fixture really is over-dispersed
  # a poisson vcov() assumes phi = 1, so the robust SEs are ~sqrt(phi) wider (SS9.1: within 1.5-8 %)
  r_pois <- tabxplor:::reg_check_dispersion(p)
  expect_gt(r_pois / sqrt(phi), 0.95)
  expect_lt(r_pois / sqrt(phi), 1.10)
  # a quasipoisson vcov() ALREADY carries the estimated dispersion, so the check returns to ~1: the
  # intervals have been fixed, and the row says so while the `phi` row still reports the dispersion.
  expect_lt(abs(tabxplor:::reg_check_dispersion(q) - 1), 0.10)
})

# ---- Influence (max dfbetas) -----------------------------------------------------------------

test_that("Influence equals max |stats::dfbetas()|", {
  f   <- chk_fit()$fit
  ref <- max(abs(stats::dfbetas(f)))
  got <- tabxplor:::reg_check_influence(f)
  expect_equal(got, ref, tolerance = 0.05)               # the (1 - h_i) correction, ~1 % on real data
  expect_gt(stats::cor(as.vector(abs(stats::dfbetas(f))),
                       as.vector(abs(sweep(
                         (stats::model.matrix(f) * (f$weights * stats::residuals(f, "working"))) %*%
                           solve(crossprod(stats::model.matrix(f) * sqrt(f$weights))),
                         2, sqrt(diag(stats::vcov(f))), "/")))), 0.999)
})

# ---- Collinearity (max VIF) ------------------------------------------------------------------

test_that("Collinearity equals car::vif(), on one VIF scale whatever the term degrees of freedom", {
  skip_if_not_installed("car")
  # several multi-df factors -> car returns the (GVIF, Df, GVIF^(1/(2Df))) matrix
  f1 <- chk_fit()$fit
  v1 <- car::vif(f1)
  expect_true(is.matrix(v1))
  expect_equal(tabxplor:::reg_check_collinearity(f1), max(v1[, 3]^2), tolerance = 1e-10)
  # only 1-df terms -> car returns a bare VIF vector; the same helper must give the same scale
  f2 <- chk_fit(preds = c("age", "tvhours"))$fit
  v2 <- car::vif(f2)
  expect_false(is.matrix(v2))
  expect_equal(tabxplor:::reg_check_collinearity(f2), max(v2), tolerance = 1e-10)
})

# ---- Linearity -------------------------------------------------------------------------------

test_that("Linearity is drop1() on the model plus the predictor's centred squared term", {
  skip_if_not_installed("broom")
  # Phase 20f: it costs a fit, so it is asked for by name (REG_CHECKS$cost == "refit").
  t  <- suppressMessages(tab_reg(chk_data(), "married", c("race", "age", "rincome"),
                                 family = "binomial", cleannames = FALSE,
                                 stats = c("n", "linearity")))
  tt <- get_test(t)
  li <- tt[tt$test %in% tabxplor:::reg_check_types() & startsWith(tt$test, "linearity"), ]
  expect_identical(nrow(li), 1L)                          # one numeric predictor
  expect_identical(li$var, "age")
  expect_identical(li$test, "linearity_lr")

  cf <- chk_fit()
  dm <- cf$data
  dm$z <- (dm$age - mean(dm$age)) / stats::sd(dm$age)
  aug <- stats::glm(married ~ race + age + rincome + I(z^2), data = dm, family = stats::binomial())
  d1  <- stats::drop1(aug, scope = "I(z^2)", test = "Chisq")
  expect_equal(li$pvalue, d1[["Pr(>Chi)"]][2], tolerance = 1e-6)
  expect_equal(li$statistic, d1[["LRT"]][2], tolerance = 1e-6)
})

test_that("the curvature p is invariant to the centring, which exists for the Collinearity check", {
  skip_if_not_installed("car")
  cf <- chk_fit(preds = c("age", "race"))
  dm  <- cf$data
  dm$z <- (dm$age - mean(dm$age)) / stats::sd(dm$age)
  raw <- stats::glm(married ~ age + race + I(age^2), data = dm, family = stats::binomial())
  ctr <- stats::glm(married ~ age + race + I(z^2),   data = dm, family = stats::binomial())
  expect_equal(stats::drop1(raw, scope = "I(age^2)", test = "Chisq")[["Pr(>Chi)"]][2],
               stats::drop1(ctr, scope = "I(z^2)",   test = "Chisq")[["Pr(>Chi)"]][2],
               tolerance = 1e-8)
  # but the collinearity of the emitted pair is not invariant -- which is why the term is centred
  expect_gt(max(car::vif(raw)[, 3]^2), 20)
  expect_lt(max(car::vif(ctr)[, 3]^2), 5)
})

test_that("reg_nested_test() IS drop1's test, to the last bit, on both arms", {
  # Phase 20f: the Linearity check takes this route INSTEAD of drop1(), so "equal" is not enough --
  # if the two ever diverged the check would silently start answering a different question. Every
  # assertion here is `expect_identical()` on the double, not a tolerance.
  cf <- chk_fit(preds = c("race", "age"))
  dm <- cf$data
  dm$z <- (dm$age - mean(dm$age)) / stats::sd(dm$age)

  # -- the LR arm (binomial) ------------------------------------------------------------------------
  aug <- stats::glm(married ~ race + age + I(z^2), data = dm, family = stats::binomial())
  d1  <- stats::drop1(aug, scope = "I(z^2)", test = "Chisq")
  lr  <- tabxplor:::reg_nested_test(cf$fit, aug, use_f = FALSE)
  expect_identical(lr$stat, d1[["LRT"]][2])
  expect_identical(lr$p,    d1[["Pr(>Chi)"]][2])
  expect_true(is.na(lr$df2))

  # -- the F arm: lm, gaussian glm and quasipoisson ------------------------------------------------
  # ⚠ quasipoisson is the one that pins the DISPERSION: drop1.glm estimates it as deviance/df.residual
  # of the augmented fit, which is neither summary()'s Pearson dispersion nor what anova() uses (14.25
  # against 12.47 on this shape). This assertion is what stops a "tidier" substitution.
  # ⚠ the outcome must be genuinely UNEXPLAINED by the predictors: a numeric copy of `age` gives an
  # exact fit (RSS ~ 1e-25), and then both routes are comparing floating-point noise.
  dm$num <- as.numeric(dm$year)
  dm$cnt <- as.integer(dm$year - min(dm$year))
  cases <- list(
    lm         = list(stats::lm(num ~ race + age, dm),
                      stats::lm(num ~ race + age + I(z^2), dm)),
    gaussianglm = list(stats::glm(num ~ race + age, dm, family = stats::gaussian()),
                       stats::glm(num ~ race + age + I(z^2), dm, family = stats::gaussian())),
    quasipoisson = list(suppressWarnings(stats::glm(cnt ~ race + age, dm, family = stats::quasipoisson())),
                        suppressWarnings(stats::glm(cnt ~ race + age + I(z^2), dm,
                                                    family = stats::quasipoisson())))
  )
  for (nm in names(cases)) {
    b <- cases[[nm]][[1]]; a <- cases[[nm]][[2]]
    ref <- suppressWarnings(stats::drop1(a, scope = "I(z^2)", test = "F"))
    got <- tabxplor:::reg_nested_test(b, a, use_f = TRUE)
    expect_identical(got$stat, ref[["F value"]][2], info = nm)
    expect_identical(got$p,    ref[[grep("^Pr\\(", names(ref), value = TRUE)[1]]][2], info = nm)
    expect_identical(got$df2,  as.numeric(stats::df.residual(a)), info = nm)
  }

  # -- it refuses rather than guesses ---------------------------------------------------------------
  expect_null(tabxplor:::reg_nested_test(aug, cf$fit))       # not nested the right way round
  half <- stats::glm(married ~ race + age, data = dm[seq_len(nrow(dm) %/% 2L), ],
                     family = stats::binomial())
  expect_null(tabxplor:::reg_nested_test(half, aug))         # different rows
})

test_that("the nested test is what carries multinomial Linearity", {
  skip_if_not_installed("nnet")
  # nnet:::drop1.multinom returns only Df and AIC -- it has no `test` argument and no p-value at all,
  # so without this route the multinomial arm silently produced no row
  d  <- chk_data()
  dn <- tidyr::drop_na(d, marital, race, age)
  zz <- (dn$age - mean(dn$age)) / stats::sd(dn$age); dn$z <- zz
  b <- nnet::multinom(marital ~ race + age,          dn, trace = FALSE)
  a <- nnet::multinom(marital ~ race + age + I(z^2), dn, trace = FALSE)
  ref <- tabxplor:::reg_nested_test(b, a)
  expect_identical(ref$df, a$edf - b$edf)                 # one extra coefficient per category

  out <- utils::capture.output(t <- suppressMessages(suppressWarnings(
    tab_reg(d, "marital", c("race", "age"), family = "multinomial", cleannames = FALSE,
            stats = c("n", "linearity")))))
  tt <- get_test(t)
  li <- tt[startsWith(tt$test, "linearity"), , drop = FALSE]
  expect_identical(nrow(li), 1L)
  expect_equal(li$pvalue, ref$p, tolerance = 1e-6)
  # drop1.multinom prints its progress with cat(); none of it may reach the user
  expect_false(any(grepl("trying", out)))
})

# ---- the footer grain and the `stats =` vocabulary --------------------------------------------

test_that("a comparison table carries one check row per (model column x numeric predictor)", {
  skip_if_not_installed("broom")
  t <- suppressMessages(tab_reg(
    chk_data(), "married",
    list(m1 = c("race", "age"), m2 = c("race", "age", "tvhours")),
    family = "binomial", cleannames = FALSE, stats = c("n", "linearity", "dispersion")))
  tt <- get_test(t)
  li <- tt[startsWith(tt$test, "linearity"), , drop = FALSE]
  # age is in both models, tvhours only in m2 -> 3 rows, and the plan lays out 2 labelled rows
  # Phase 19g re-keyed the `test` tibble: the column a row belongs to is `col` (was `col_var`).
  expect_setequal(paste(li$col, li$var), c("m1 age", "m2 age", "m2 tvhours"))
  plan <- tabxplor:::reg_footer_plan(tt)
  expect_setequal(plan$label[startsWith(plan$test, "linearity")],
                  c("Linearity (LR): age", "Linearity (LR): tvhours"))
  # the whole-model checks stay one row each
  expect_identical(sum(tt$test == "dispersion"), 2L)
  expect_true(all(tt$var[tt$test == "dispersion"] == ""))
  # and they render, per model column
  md <- gsub(intToUtf8(160L), " ", tab_md(t, print = FALSE), fixed = TRUE)
  expect_true(any(grepl("Linearity (LR): tvhours", md, fixed = TRUE)))
  expect_true(any(grepl("Dispersion (robust/model SE)", md, fixed = TRUE)))
})

test_that("the FREE checks are the default set, the costly ones are opt-in, and stats='all' is all", {
  skip_if_not_installed("broom")
  d <- chk_data()
  full <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial")))
  # Phase 20f: the three that are arithmetic on the fit in hand ride the default footer...
  expect_true(all(c("dispersion", "influence", "collinearity") %in% full$test))
  # ...and the one that refits does not (it was 87 % of a default call at n = 200 000)
  expect_false(any(startsWith(full$test, "linearity")))

  # a `stats =` vector takes the check KEY; only what it names survives
  one <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                           stats = c("n", "influence"))))
  expect_true("influence" %in% one$test)
  expect_false(any(c("dispersion", "collinearity") %in% one$test))
  expect_false(any(startsWith(one$test, "linearity")))

  # naming the costly one brings it back -- opting in needs no new argument
  opt <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                           stats = c("n", "linearity"))))
  expect_true(any(startsWith(opt$test, "linearity")))

  # `stats = "all"` MEANS all: strictly more than the default, and every applicable check in it.
  # (It used to be a synonym of NULL, i.e. of the default set -- a name that already lied.)
  all_t <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                             stats = "all")))
  expect_true(all(setdiff(full$test, "") %in% all_t$test))
  expect_gt(length(unique(all_t$test)), length(unique(full$test)))
  expect_true(any(startsWith(all_t$test, "linearity")))

  none <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                            stats = FALSE)))
  expect_false(any(tabxplor:::reg_check_types() %in% none$test))
})

test_that("REG_CHECKS declares its cost AND its default, and the readers agree with them", {
  # the declared facts, and the derived sets. ⚠ the default set is DECLARED (footer_default), not
  # "the applicable checks minus the costly ones": Proportionality is a refit AND a default, because
  # a cumulative odds ratio that fails it is not one number but a fiction.
  expect_setequal(tabxplor:::reg_checks_costly(), c("linearity", "proportionality"))
  dflt <- tabxplor:::reg_checks_default("ordinal")
  expect_true(all(dflt %in% tabxplor:::reg_checks_for("ordinal")))
  expect_setequal(dflt, c("proportionality", "dispersion", "influence", "collinearity"))
  # linearity is the costly check that is NOT a default -- `stats =` reaches it by name
  expect_false("linearity" %in% dflt)
  expect_true("linearity" %in% tabxplor:::reg_checks_for("ordinal"))
  # a panel is always free: reg_check_plots() keeps every panel whatever `cost` says
  expect_true(all(c("linearity", "proportionality") %in%
                    tabxplor:::reg_checks_for("ordinal", what = "panel")))
})

test_that("a check absent for a family produces no row, never a wrong number", {
  skip_if_not_installed("nnet")
  # multinomial: car::vif() warns on an intercept-free fit and the hand-rolled substitute is wrong,
  # so Collinearity is refused -- while the others still compute from the score-based influence
  expect_false("collinearity" %in% tabxplor:::reg_checks_for("multinomial"))
  t <- suppressMessages(suppressWarnings(
    tab_reg(chk_data(), "marital", "race", family = "multinomial", cleannames = FALSE)))
  tt <- get_test(t)
  expect_false("collinearity" %in% tt$test)
  expect_true("dispersion" %in% tt$test)
  # Proportionality is ordinal-only, and refused (absent, not approximated) on a weighted fit
  expect_false("proportionality" %in% tabxplor:::reg_checks_for("binomial"))
  expect_true( "proportionality" %in% tabxplor:::reg_checks_for("ordinal"))
  expect_false("proportionality" %in% tabxplor:::reg_checks_for("ordinal", weighted = TRUE))
  # the jamovi digest path keeps no model frame -> every check degrades to absent
  expect_length(tabxplor:::reg_checks_for("binomial", has_fit = FALSE), 0L)
})

# ---- the fact table is the one source ---------------------------------------------------------

test_that("REG_CHECKS drives the footer labels and the `stats =` vocabulary from one row", {
  spec <- tabxplor:::reg_footer_spec()
  for (k in names(tabxplor:::REG_CHECKS)) {
    ck <- tabxplor:::REG_CHECKS[[k]]
    for (d in names(ck$types)) {
      expect_true(d %in% names(spec))
      expect_identical(spec[[d]]$label, paste0(ck$noun, " (", ck$types[[d]], ")"))
      expect_identical(spec[[d]]$kind, ck$kind)
    }
    # the KEY is what a user writes; the discriminators are what a `test` row carries. Phase 18z15:
    # a TAUGHT-BUT-NEVER-SCORED check (residuals / normality) carries none, so it expands to nothing --
    # which is exactly how it contributes a panel and no footer row.
    if (length(ck$types)) expect_setequal(tabxplor:::reg_check_expand(k), names(ck$types))
    else                  expect_length(tabxplor:::reg_check_expand(k), 0L)
  }
})

test_that("the Pearson dispersion row never reads df.residual, so a weighted count fit is honest", {
  skip_if_not_installed("survey")
  d  <- chk_data()
  dm <- tidyr::drop_na(d, tvhours, age, race)
  dm$w <- 1                                              # constant weights: phi must not change
  des <- survey::svydesign(ids = ~1, weights = ~w, data = dm)
  p   <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  sp  <- suppressWarnings(survey::svyglm(tvhours ~ age + race, design = des,
                                         family = stats::quasipoisson()))
  # df.residual(svyglm) is the DESIGN df, which used to inflate this by ~20x
  expect_equal(tabxplor:::reg_dispersion(sp), tabxplor:::reg_dispersion(p), tolerance = 1e-6)
})
