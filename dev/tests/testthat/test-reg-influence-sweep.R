
# === SECTION: gap_se: the influence-function SE of the gap ========================================

gapb_data <- function() {
  d <- fx_gss()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d <- d[!is.na(d$race) & !is.na(d$party3) & !is.na(d$tvhours), , drop = FALSE]
  tibble::as_tibble(d)
}


# the canonical builder: a COLLAPSIBLE binary-outcome table -- `link = "ratio"` is the modified
# Poisson, whose coefficient is a CONDITIONAL risk ratio, which is the estimand the ruling points
# users at. `relig` is in the default predictor set because it is what
# makes some gaps actually REACH the first adj_ratio break (x1.1) -- a fixture that colours nothing
# would let the policy tests pass vacuously.
gapb_tab <- function(d, policy = "ignore", preds = c("race", "party3", "relig"), ...)
  suppressMessages(tab_reg(d, outcome = "married", predictors = preds, family = "binomial",
                           link = "ratio",
                           empirical = TRUE, color = c(TRUE, "adjustment"),
                           color_signif = policy, ...))


gapb_model_col <- function(t) t[[grep("^Model", names(t), value = TRUE)[[1]]]]


# --- the two legs each reproduce a number the table already prints --------------------------------
# Neither influence function is a free parameter: the model one IS svyglm's own standard error, the
# crude one IS the Woolf interval of the Obs_OR column. If either drifted, this catches it without any
# remembered constant.

test_that("the model influence function reproduces the fit's own design-based standard error", {
  skip_if_not_installed("survey")
  d  <- gapb_data()
  dd <- tidyr::drop_na(d[, c("married", "race", "party3")])
  dd$y <- as.numeric(dd$married == "yes"); dd$.w <- 1
  des <- survey::svydesign(ids = ~1, weights = ~.w, data = dd)
  f   <- survey::svyglm(y ~ race + party3, design = des, family = stats::quasipoisson())
  mk  <- tabxplor:::reg_coef_if_maker(f)
  testthat::expect_false(is.null(mk))
  for (tm in c("raceBlack", "raceWhite", "party3Dem")) {
    L <- stats::setNames(1, tm)
    # 1e-5, not 1e-10: the influence function is rebuilt from (terms, coef, family, frame), while
    # glm stores the IRLS weights of the PREVIOUS iteration -- a lag by construction, of the order
    # of the fit's own convergence tolerance (measured ~2e-6 on this quasipoisson).
    testthat::expect_equal(tabxplor:::reg_if_se(mk(L), f$survey.design),
                           unname(survey::SE(f)[tm]), tolerance = 1e-5)
  }
  # without a design it is the plain sum of squares
  testthat::expect_equal(tabxplor:::reg_if_se(mk(stats::setNames(1, "raceBlack"))),
                         sqrt(sum(mk(stats::setNames(1, "raceBlack"))^2)))
})


test_that("the crude influence function reproduces the Woolf interval the Obs_OR column prints", {
  d  <- gapb_data()
  dd <- tidyr::drop_na(d[, c("married", "race")])
  ci <- tabxplor:::reg_crude_if_maker(dd, "married", "binomial", "yes", NULL, "logit")
  tb <- table(dd$race, dd$married)
  ref <- levels(dd$race)[1]
  for (lv in levels(dd$race)[-1]) {
    woolf <- sqrt(1 / tb[ref, "yes"] + 1 / tb[ref, "no"] + 1 / tb[lv, "yes"] + 1 / tb[lv, "no"])
    testthat::expect_equal(tabxplor:::reg_if_se(ci("race", lv, ref)), woolf, tolerance = 1e-10)
  }
  # the LINK is the crude estimator's, not the family's: the same cells on a log link give the Katz
  # (risk-ratio) SE, and on identity the risk-difference one -- three different, correct answers.
  li <- tabxplor:::reg_crude_if_maker(dd, "married", "binomial", "yes", NULL, "log")
  id <- tabxplor:::reg_crude_if_maker(dd, "married", "binomial", "yes", NULL, "identity")
  lv <- levels(dd$race)[2]
  testthat::expect_false(isTRUE(all.equal(tabxplor:::reg_if_se(ci("race", lv, ref)),
                                          tabxplor:::reg_if_se(li("race", lv, ref)))))
  testthat::expect_true(is.finite(tabxplor:::reg_if_se(id("race", lv, ref))))
  # an unknown link is not guessed
  testthat::expect_null(tabxplor:::reg_crude_if_maker(dd, "married", "binomial", "yes", NULL, "probit"))
})


test_that("the marginal influence function reproduces marginaleffects' own standard error", {
  skip_if_not_installed("marginaleffects")
  d  <- gapb_data()
  dd <- tidyr::drop_na(d[, c("married", "race", "party3")])
  dd$y <- as.numeric(dd$married == "yes")
  f   <- stats::glm(y ~ race + party3, stats::binomial, data = dd)
  mk  <- tabxplor:::reg_coef_if_maker(f)
  ref <- levels(dd$race)[1]; lv <- levels(dd$race)[2]
  # the three REPORTED links, each its own contrast from the one sweep (22b-xv-1)
  for (lk in c("identity", "log", "logit")) {
    am <- tabxplor:::reg_ame_if_maker(f, dd, NULL, link = lk, coef_if = mk)
    me <- as.data.frame(do.call(marginaleffects::avg_comparisons, c(
      list(f, variables = "race", newdata = dd),
      switch(lk, log = list(comparison = "lnratioavg"),
             logit = list(comparison = "lnoravg"), list()))))
    got <- tabxplor:::reg_if_se(am("race", lv, ref))
    # marginaleffects reports the DELTA term (covariates held fixed); the full influence function adds
    # the empirical-averaging term, worth ~0.1 %. Agreement to 2 % is the honest assertion.
    testthat::expect_equal(got, me$std.error[me$contrast == me$contrast[1]][1], tolerance = 0.02)
  }
})


test_that("the covariance is real: the gap SE sits strictly inside the two naive bounds", {
  # The only piece the printed table cannot anchor is the covariance between the two estimators. It is
  # bounded by construction: |se1 - se2| <= SE(gap) <= se1 + se2, and the design study measured the
  # naive independent bound to be 2-4x too large. This catches a covariance term dropped or
  # sign-flipped without re-deriving the mathematics.
  d <- gapb_data()
  t <- gapb_tab(d)
  x <- gapb_model_col(t)
  o <- t[[grep("^Obs_(RR|OR|IRR)", names(t), value = TRUE)[[1]]]]
  # reg_gap_se_of() recovers with the column's OWN critical value (22b-xiii-2 / C2), so it needs no
  # crit argument: a t-referred interval would otherwise come back inflated by t/z.
  se_m <- tabxplor:::reg_gap_se_of(x)
  se_o <- tabxplor:::reg_gap_se_of(o)
  g    <- get_gap_se(x)
  k    <- which(is.finite(g) & is.finite(se_m) & is.finite(se_o))
  testthat::expect_gt(length(k), 3L)
  testthat::expect_true(all(g[k] <= se_m[k] + se_o[k] + 1e-9))
  testthat::expect_true(all(g[k] >= abs(se_m[k] - se_o[k]) - 1e-9))
  # and it is MUCH tighter than the independent quadrature -- that is the whole point
  testthat::expect_true(median(g[k] / sqrt(se_m[k]^2 + se_o[k]^2)) < 0.6)
})


# Phase 18z17 (ruling D2). The gap SE used to be gated on `color = "adjustment"` -- a fact withheld
# because nobody had asked to COLOUR it, which held only while the colour engine was its one reader.
# forest_plot() is the second, so the premise is now validity alone: `empirical = TRUE` + the five
# correctness clauses. Nothing RENDERS differently (a gap measure is still the only consumer of the
# stored value), which is why the goldens do not move.
test_that("z17 D2: gap_se is written without asking for the colour", {
  d <- gapb_data()
  plain  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio", empirical = TRUE))
  asked  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio", empirical = TRUE,
                                     color = c(TRUE, "adjustment")))
  gp <- get_gap_se(gapb_model_col(plain))
  ga <- get_gap_se(gapb_model_col(asked))
  testthat::expect_true(any(!is.na(gp)))          # would have been all-NA before z17
  testthat::expect_equal(gp, ga)                  # and it is the SAME number the colour scores
  # the rendered table is untouched: no measure reads `gap_se` unless it was asked for
  testthat::expect_identical(format(gapb_model_col(plain)), format(gapb_model_col(asked)))
})


# Phase 18z13 (D1). Before it, the crude block was built on the UNION of predictors while each model
# dropped its own NA rows, so a smaller model was fitted on MORE people. The framework detected that
# (it withheld the gap SE) and coloured the gap anyway -- so `m1 = race`, which IS the crude model and
# whose true adjustment gap is exactly zero, rendered a coloured cell claiming a 16 % move. Two halves:
# the default now puts every model of an outcome on one population, and where they still differ the
# `obs` write is gated by the same predicate that gates the test.
adjgap_inc_data <- function() {
  d <- gapb_data()
  d$inc <- factor(dplyr::case_when(d$rincome %in% c("Not applicable", "No answer", "Don't know",
                                                    "Refused") ~ NA_character_,
                                   d$rincome %in% "$25000 or more" ~ "hi", TRUE ~ "lo"),
                  levels = c("lo", "hi"))
  d
}

adjgap_mods <- list(m1 = "race", m2 = c("race", "inc"))


test_that("D1: every compared model shares its outcome's population, so every column is testable", {
  d <- adjgap_inc_data()
  testthat::expect_true(any(is.na(d$inc)))                       # the fixture must actually bite
  t <- suppressMessages(tab_reg(d, "married", predictors = adjgap_mods,
                                family = "binomial", link = "ratio",
                                empirical = TRUE, color = c(TRUE, "adjustment")))
  fc <- grep("^m1|^m2", names(t), value = TRUE)
  testthat::expect_length(fc, 2L)
  # both models now solve their equations on the crude block's rows -> both carry a real gap SE (D5:
  # one significance policy governs the table instead of two).
  for (nm in fc) testthat::expect_false(all(is.na(get_gap_se(t[[nm]]))))
  # m1 IS the crude model: its estimate and its `obs` are the same number, so the gap is exactly 0 and
  # nothing is coloured. That equality is the whole of D1.
  # the union skeleton also carries m2's `inc` rows, which m1 does not estimate -> exclude them
  i <- which(!is_refrow(t[[fc[[1]]]]) & as.character(t$var) != "Constant" &
               !is.na(tabxplor:::fmt_est_of(t[[fc[[1]]]])))
  testthat::expect_gt(length(i), 0L)
  testthat::expect_equal(tabxplor:::fmt_est_of(t[[fc[[1]]]])[i], get_obs(t[[fc[[1]]]])[i],
                         tolerance = 1e-8)
  # ⚠ 22b-xviii: a model that IS its own crude twin has NO gap -- the score, its interval and its p
  # go NA together, rather than a score of 1 and a z of 20 built out of floating-point dust.
  testthat::expect_true(all(is.na(tabxplor:::fmt_adjustment_score(t[[fc[[1]]]])[i])))
  testthat::expect_true(all(is.na(fmt_gap_p(t[[fc[[1]]]])[i])))
  testthat::expect_true(all(fmt_color_channels(t[[fc[[1]]]])$bg_slot == 0L))
})


test_that("D1: under the opt-in per-model drop, a model on other rows gets NO obs at all", {
  d <- adjgap_inc_data()
  t <- suppressMessages(tab_reg(d, "married", predictors = adjgap_mods, na = "drop_by_model",
                                family = "binomial", link = "ratio",
                                empirical = TRUE, color = c(TRUE, "adjustment")))
  fc <- grep("^m1|^m2", names(t), value = TRUE)
  # m1 is fitted on more rows than the observed block -> no observed value, hence no colour and no
  # test. It used to keep the colour while losing only the test.
  testthat::expect_true(all(is.na(get_obs(t[[fc[[1]]]]))))
  testthat::expect_true(all(is.na(get_gap_se(t[[fc[[1]]]]))))
  testthat::expect_true(all(fmt_color_channels(t[[fc[[1]]]])$bg_slot == 0L))
  testthat::expect_false(all(is.na(get_obs(t[[fc[[2]]]]))))      # m2: the same rows
  # ... and the choice is named
  testthat::expect_message(
    tab_reg(d, "married", predictors = adjgap_mods, na = "drop_by_model",
            family = "binomial", link = "ratio", empirical = TRUE,
            color = c(TRUE, "adjustment")),
    "drop_by_model")
})


test_that("a crude companion on another scale writes neither obs nor a gap SE (a z5 defect)", {
  # a poisson marginal effect is a difference of expected COUNTS and pairs with the observed mean
  # difference, so `adjustment` works here. (It used to fall back to the crude rate RATIO, which
  # reg_same_estimand() refused to pair -- the gate is still what protects a real mismatch, asserted
  # directly below.)
  skip_if_not_installed("marginaleffects")
  d <- gapb_data()
  # (tvhours is over-dispersed -> reg_fit warns and phi-scales; irrelevant here, and suppressed so the
  #  assertion is about the estimand mismatch alone)
  t <- suppressWarnings(suppressMessages(tab_reg(d, "tvhours", "race", family = "poisson",
                                                 effect = "marginal", measure = "difference",
                                                 empirical = TRUE,
                                                 color = c(TRUE, "adjustment"))))
  x <- gapb_model_col(t)
  testthat::expect_identical(get_scale(x), "raw_diff")   # a count AME, in the outcome's own units
  testthat::expect_false(all(is.na(get_obs(x))))
  testthat::expect_false(all(is.na(get_gap_se(x))))
  # the gate itself: a crude shape on another scale -- or on the same scale under another MEASURE,
  # which every logged one shares -- is still refused, so no future fall-back can silently write one
  # estimand into another's `obs`.
  est <- tabxplor:::reg_estimand("poisson", measure = "difference", effect = "marginal")
  sc  <- get_scale(x)
  testthat::expect_false(tabxplor:::reg_same_estimand(list(scale = "mean_ratio", word = "diff"),
                                                      sc, est))
  testthat::expect_false(tabxplor:::reg_same_estimand(list(scale = "raw_diff", word = "IRR"),
                                                      sc, est))
  testthat::expect_true(tabxplor:::reg_same_estimand(list(scale = "raw_diff", word = "diff"),
                                                     sc, est))
  # two log_coef columns are told apart by the WORD alone: log(OR) is not log(RR).
  lg <- tabxplor:::reg_estimand("binomial", measure = "log_odds", effect = "conditional")
  testthat::expect_true(tabxplor:::reg_same_estimand(list(scale = "log_coef", word = "OR"),
                                                     "log_coef", lg))
  testthat::expect_false(tabxplor:::reg_same_estimand(list(scale = "log_coef", word = "RR"),
                                                      "log_coef", lg))
  # the coefficient path on the same data DOES match scales, so it keeps both
  t2 <- suppressWarnings(suppressMessages(tab_reg(d, "tvhours", "race", family = "poisson",
                                                  empirical = TRUE, color = c(TRUE, "adjustment"))))
  testthat::expect_false(all(is.na(get_obs(gapb_model_col(t2)))))
  testthat::expect_false(all(is.na(get_gap_se(gapb_model_col(t2)))))
})


# --- the policies ---------------------------------------------------------------------------------

test_that("`force_policy` is a predicate on the column, not on the measure", {
  d <- gapb_data()
  pol <- function(t) {
    x <- gapb_model_col(t)
    tabxplor:::fmt_color_plan(x, "bg", color = get_color_bg(x))$policy
  }
  # collapsible + a gap_se -> the user's policy applies
  testthat::expect_identical(pol(gapb_tab(d, "grey_non_signif")), "grey_non_signif")
  testthat::expect_identical(pol(gapb_tab(d, "guaranteed_effect")), "guaranteed_effect")
  # no gap_se -> `ignore`, whatever was asked for
  testthat::expect_identical(pol(suppressMessages(tab_reg(
    d, "married", c("race", "party3"), family = "binomial", empirical = TRUE,
    color = c(TRUE, "adjustment"), color_signif = "guaranteed_effect"))), "ignore")
  # with no column to ask, the caller's policy stands (the accessor must not force on a NULL)
  testthat::expect_identical(
    tabxplor:::measure_policy("adjustment", "grey_non_signif"), "grey_non_signif")
})


test_that("the same predicate fixes between_groups under method = 'profile'", {
  # A profile-likelihood bracket is not est +/- crit*se, so no SE is recoverable -- and before z8-B
  # that read as `grey_non_signif` and greyed the WHOLE column instead of falling back to the
  # descriptive reading.
  d  <- gapb_data()
  pr <- suppressMessages(tab_reg(d, "married", "race", tab_vars = "party3", family = "binomial",
                                 color = c(TRUE, "between_groups"),
                                 color_signif = "grey_non_signif", ci_method = "profile"))
  ig <- suppressMessages(tab_reg(d, "married", "race", tab_vars = "party3", family = "binomial",
                                 color = c(TRUE, "between_groups"),
                                 color_signif = "ignore", ci_method = "profile"))
  cn <- names(pr)[vapply(pr, is_fmt, logical(1))]
  for (nm in cn) {
    testthat::expect_true(all(is.na(get_gap_se(pr[[nm]]))))
    testthat::expect_identical(fmt_color_channels(pr[[nm]])$bg, fmt_color_channels(ig[[nm]])$bg)
  }
  testthat::expect_gt(sum(vapply(cn, function(nm) sum(fmt_color_channels(pr[[nm]])$bg != 0L),
                                 integer(1))), 0L)   # and it really colours something
})


# --- Phase 18z13 (D3): the gap interval follows the table's conf_level -----------------------------

test_that("D3: conf_level reaches the gap interval, not only the printed one", {
  d <- gapb_data()
  w <- function(cl) {
    t <- suppressMessages(tab_reg(d, "married", c("race", "party3"),
                                  family = "binomial", link = "ratio",
                                  empirical = TRUE, color = c(TRUE, "adjustment"),
                                  color_signif = "grey_non_signif", conf_level = cl,
                                  cleannames = FALSE))
    x <- t[["Model_RR"]]
    b <- tabxplor:::fmt_gap_bounds(x)
    i <- which(is.finite(b$lo))[[1]]
    # the FAR bound is |gap| + z*se on the log scale, never clamped (the near one pins at the neutral
    # when the interval covers it), so its distance from the gap is exactly the half-width. Which of
    # lo/hi is the far one depends on the score's sign, which fmt_gap_bounds() re-orders by magnitude.
    far <- max(abs(log(b$lo[i])), abs(log(b$hi[i])))
    c(model = log(get_ci_sup(x)[i]) - log(get_ci_inf(x)[i]),
      gap   = far - abs(tabxplor:::fmt_gap_raw(x)[i]))
  }
  a <- w(0.95); b <- w(0.99)
  # z5/z8 manufactured the gap interval in the colour engine, which read the OPTION -- so the printed
  # interval and the stars moved to 99 % while the gap greying silently stayed at 95 %.
  testthat::expect_gt(b[["gap"]], a[["gap"]])
  testthat::expect_equal(b[["gap"]] / a[["gap"]],
                         tabxplor:::zscore_formula(0.99) / tabxplor:::zscore_formula(0.95),
                         tolerance = 1e-8)
  testthat::expect_identical(get_conf_level(t_dummy <- fmt(1, conf_level = 0.9)), 0.9)
})


test_that("D3: an unrecorded level stays unknown through a bind (it must not bake in the option)", {
  a <- fmt(1); b <- fmt(2)
  testthat::expect_true(is.na(tabxplor:::fmt_conf_level_attr(a)))
  withr::local_options(tabxplor.conf_level = 0.99)
  # the RESOLVED read follows the option ...
  testthat::expect_identical(get_conf_level(a), 0.99)
  # ... but a reconcile must carry "unknown" forward, or a later options() change would stop applying
  testthat::expect_true(is.na(tabxplor:::fmt_conf_level_attr(c(a, b))))
  # two columns built at DIFFERENT levels reconcile to unknown, never to one of the two
  x <- fmt(1, conf_level = 0.95); y <- fmt(2, conf_level = 0.99)
  testthat::expect_identical(tabxplor:::fmt_conf_level_attr(c(x, x)), 0.95)
  testthat::expect_true(is.na(tabxplor:::fmt_conf_level_attr(c(x, y))))
})


# --- 22b-xiii-2 (Part 5): the crude influence leg reads its shape's CONTRAST ----------------------

test_that("the crude influence function builds the contrast its estimate is of", {
  # A categorical outcome offers two contrasts and they are different quantities. The maker used to
  # build category-vs-REST unconditionally: right for a marginal probability contrast, wrong for a
  # multinomial's own conditional ODDS ratio, which is category-vs-PIVOT -- and which is what the
  # crude column's interval computes since 22b-xiii-1. Both arms are asserted against hand values, so
  # neither can drift into the other.
  d <- gapb_data()
  d <- d[stats::complete.cases(d[c("party3", "race")]), ]

  # vs PIVOT (logit, 3 categories): the SE is exactly Woolf on the {category, pivot} x {level, ref}
  # 2x2 -- the same table the crude interval is built from.
  mk <- tabxplor:::reg_crude_if_maker(d, "party3", "multinomial", NULL, NULL, "logit",
                                      category = "Rep", ref_category = "Ind")
  v  <- mk("race", "Black", "White")
  tb <- table(d$race, d$party3)[c("White", "Black"), c("Ind", "Rep")]
  expect_equal(tabxplor:::reg_if_se(v), sqrt(sum(1 / tb)), tolerance = 1e-8)

  # vs REST (identity): the SE of a difference of two independent proportions -- untouched.
  mk2 <- tabxplor:::reg_crude_if_maker(d, "party3", "multinomial", NULL, NULL, "identity",
                                       category = "Rep", ref_category = "Ind")
  v2  <- mk2("race", "Black", "White")
  p   <- tapply(d$party3 == "Rep", d$race, mean)
  n   <- table(d$race)
  expect_equal(tabxplor:::reg_if_se(v2),
               sqrt(sum((p * (1 - p) / n)[c("Black", "White")])), tolerance = 1e-8)
  # NON-VACUOUS: the two contrasts really are different numbers.
  expect_false(isTRUE(all.equal(tabxplor:::reg_if_se(v), tabxplor:::reg_if_se(v2))))

  # a BINARY outcome has only one contrast ("the rest" IS the pivot), so it needs no arm of its own:
  # the logit leg is still exactly Woolf.
  b  <- tabxplor:::reg_crude_if_maker(d, "married", "binomial", "yes", NULL, "logit")
  vb <- b("race", "Black", "White")
  tb2 <- table(d$race, d$married)[c("White", "Black"), c("yes", "no")]
  expect_equal(tabxplor:::reg_if_se(vb), sqrt(sum(1 / tb2)), tolerance = 1e-8)
})


# === SECTION: color = 'adjustment' ================================================================

adj_data <- function() {
  d <- fx_gss()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d$inc3    <- factor(dplyr::case_when(d$rincome %in% c("$25000 or more") ~ "hi",
                                       d$rincome %in% c("Not applicable", "No answer",
                                                        "Don't know", "Refused") ~ NA_character_,
                                       TRUE ~ "lo"),
                      levels = c("lo", "hi"))
  d <- d[!is.na(d$tvhours) & !is.na(d$race) & !is.na(d$party3), , drop = FALSE]
  tibble::as_tibble(d)
}


test_that("a NUMERIC predictor gets an obs, and `adjustment` colours it", {
  # Phase 18z9 inverted this test's premise: the univariable fit IS the numeric row's crude twin.
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  i <- which(as.character(t$var) == "age")
  testthat::expect_true(all(!is.na(get_obs(t$Model_OR)[i])))
  testthat::expect_true(all(!is.na(get_or(t$Obs_OR)[i])))
})


test_that("multiplier scales obs by the SAME k as the estimate (SS9 Q6)", {
  # Both columns go through reg_fit(multiplier=), so an OR^k model cell is compared to an OR^k crude
  # one -- the desync this test used to be safe from only because numeric rows had no twin at all.
  d  <- adj_data()
  t1 <- tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
                empirical = TRUE, multiplier = c(age = 1))
  t10 <- tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
                 empirical = TRUE, multiplier = c(age = 10))
  i <- which(as.character(t1$var) == "age")
  testthat::expect_equal(get_obs(t10$Model_OR)[i], get_obs(t1$Model_OR)[i]^10, tolerance = 1e-8)
  testthat::expect_equal(get_or(t10$Model_OR)[i],  get_or(t1$Model_OR)[i]^10,  tolerance = 1e-8)
  # and the crude column itself is the same k-scaled quantity
  testthat::expect_equal(get_or(t10$Obs_OR)[i], get_obs(t10$Model_OR)[i], tolerance = 1e-12)
})


test_that("an additive effect scores the absolute gap, signed by the null rule", {
  add <- fmt(n = rep(1L, 2), diff = c(0.30, -0.30), obs = c(0.20, -0.20),
             scale = "points", pct_type = "row", display = "diff", color = "adjustment")
  # both moved 0.10 FURTHER from 0 -> same pole, same magnitude (never +0.10 vs -0.10)
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(add), c(0.10, 0.10))
})


# --- reference resolution across the modes -----------------------------------------------------------

test_that("model comparison: every model column is scored against the ONE crude column", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married",
               predictors = list(m1 = "race", m2 = c("race", "party3")),
               family = "binomial", empirical = TRUE)
  testthat::expect_equal(get_obs(t$m1), get_or(t[["Obs_OR"]]))
  testthat::expect_equal(get_obs(t$m2), get_or(t[["Obs_OR"]]))
})


test_that("several dependents: each fit takes its OWN crude block", {
  d <- adj_data(); d$black <- factor(as.integer(d$race == "Black"), labels = c("no", "yes"))
  t <- tab_reg(d, outcome = c("married", "black"), predictors = "party3",
               family = "binomial", empirical = TRUE)
  testthat::expect_equal(get_obs(t[["Model_OR [married]"]]), get_or(t[["Obs_OR [married]"]]))
  testthat::expect_equal(get_obs(t[["Model_OR [black]"]]),   get_or(t[["Obs_OR [black]"]]))
  # and they are genuinely different outcomes, so the two crude vectors must NOT coincide
  testthat::expect_false(isTRUE(all.equal(get_obs(t[["Model_OR [married]"]]),
                                          get_obs(t[["Model_OR [black]"]]))))
})


# --- between_groups ----------------------------------------------------------------------------------

# Phase 18z8 pinned `color_signif = "ignore"` here: it is the DESCRIPTIVE reading this file locks
# (z5's), and it is now one policy among three -- tab_reg()'s default became grey_non_signif, which
# greys a gap the new test finds non-significant. The policies themselves are tested in
# test-between-groups-gap.R.
test_that("between_groups carries the reference group's estimate, stacked AND spread", {
  d <- adj_data()
  sp <- tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                family = "binomial", color = c(TRUE, "between_groups"), color_signif = "ignore")
  fmt_cols <- reg_fmt_cols(sp)
  testthat::expect_length(fmt_cols, 3L)                            # one column per group
  ref <- get_or(sp[[fmt_cols[[1]]]])
  testthat::expect_true(all(is.na(get_obs(sp[[fmt_cols[[1]]]]))))  # not compared to itself
  testthat::expect_equal(get_obs(sp[[fmt_cols[[2]]]]), ref)
  testthat::expect_equal(get_obs(sp[[fmt_cols[[3]]]]), ref)
  testthat::expect_identical(fmt_color_channels(sp[[fmt_cols[[1]]]])$bg_slot,
                             integer(length(ref)))                 # the baseline stays uncoloured
  # a non-baseline group HAS a gap to grade; whether it is painted is the policy's business, and a
  # gap measure always tests its own interval (22b-xviii), which this fixture's gaps do not clear.
  testthat::expect_true(any(!is.na(tabxplor:::fmt_adjustment_score(sp[[fmt_cols[[3]]]]))))

  # the STACKED shape (several models per group, so no side-by-side layout): each group is a block
  # of rows, and `obs` is filled from the first group's block.
  st <- tab_reg(d, outcome = "married", predictors = list(m1 = "race", m2 = "race"),
                tab_vars = "party3", family = "binomial",
                color = c(TRUE, "between_groups"), color_signif = "ignore")
  col <- st[[reg_fmt_cols(st)[[1]]]]
  k   <- length(ref)
  testthat::expect_true(all(is.na(get_obs(col)[seq_len(k)])))      # first group's block
  testthat::expect_equal(get_obs(col)[k + seq_len(k)], ref)        # second group's block
})


test_that("between_groups is off by default and needs no empirical companion", {
  d <- adj_data()
  # ⚠ `empirical = FALSE`: the crude companion is the DEFAULT since 22g-ii, so what is under test --
  # that `between_groups` does not turn it on by itself -- has to start from off.
  t <- tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
               family = "binomial", empirical = FALSE)              # no `color` -> auto
  testthat::expect_true(all(vapply(t[reg_fmt_cols(t)],
                                   function(c) all(is.na(get_obs(c))), logical(1))))
})


# Phase 18z8-B: `force_policy` is a PREDICATE ON THE COLUMN for both gap measures -- a gap measure
# reads under `ignore` exactly where no `gap_se` was written. On a CONDITIONAL ODDS RATIO that is by
# design (maintainer ruling Q1(b): the gap is part non-collapsibility, so the test would read
# "significant" everywhere); on a collapsible estimand the policy applies normally -- see
# test-adjustment-gap.R for that half.
test_that("color_signif does not apply to an odds-ratio `adjustment` gap: it reads under `ignore`", {
  d <- adj_data()
  testthat::expect_message(
    t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
                 empirical = TRUE, color = c(TRUE, "adjustment"),
                 color_signif = "guaranteed_effect"),
    "non-collapsibility")
  testthat::expect_true(all(is.na(get_gap_se(t$Model_OR))))    # the reason it reads under `ignore`
  pl <- tabxplor:::fmt_color_plan(t$Model_OR, "bg", color = get_color_bg(t$Model_OR))
  testthat::expect_identical(pl$policy, "ignore")
  testthat::expect_identical(pl$measure, "adjustment")
  # the TEXT channel keeps the user's policy -- the neutralisation is per measure, not per column
  testthat::expect_identical(
    tabxplor:::fmt_color_plan(t$Model_OR, "text", color = get_color(t$Model_OR))$policy,
    "guaranteed_effect")
  # ... while `between_groups` now HONOURS the policy (its gap has a test of its own)
  b  <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                                 family = "binomial", color = c(TRUE, "between_groups"),
                                 color_signif = "guaranteed_effect"))
  bc <- b[[reg_fmt_cols(b)[[2]]]]
  testthat::expect_identical(
    tabxplor:::fmt_color_plan(bc, "bg", color = get_color_bg(bc))$policy, "guaranteed_effect")
})


test_that("the legend names each channel's own baseline, and warns only on a non-collapsible scale", {
  skip_if_no_gettext <- get0("skip_if_no_gettext", ifnotfound = function() invisible(NULL))
  d <- adj_data()
  leg <- function(t) tab_color_legend(t, medium = "plain", style = "prose", lang = "en")
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  l <- leg(t)
  testthat::expect_true(any(grepl("than the observed column", l, fixed = TRUE)))
  testthat::expect_true(any(grepl("non-collapsibility", l, fixed = TRUE)))
  # a COLLAPSIBLE estimand earns no caveat -- that contrast is the point of the sentence
  t2 <- suppressMessages(tab_reg(d, outcome = "married", predictors = c("race", "party3"),
                                 family = "binomial", link = "ratio", empirical = TRUE, color = c(TRUE, "adjustment")))
  testthat::expect_false(any(grepl("non-collapsibility", leg(t2), fixed = TRUE)))
  # and the group measure names ITS baseline, not the observed effect
  t3 <- tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                family = "binomial", color = c(TRUE, "between_groups"))
  testthat::expect_true(any(grepl("reference group", leg(t3), fixed = TRUE)))
})


test_that("the composite needs no set_pvalue exception (unlike the derived resid token)", {
  # A composite blanks the p-value of every NON-primary token so stars ride the primary. `resid` needed
  # an exception because it is DERIVED from that p-value; `obs` is a stored field, so it must survive
  # untouched -- if it did not, the whole template would silently collapse to the bare primary.
  d <- adj_data()
  x <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
               family = "binomial", empirical = TRUE)$Model_OR
  ok <- !is.na(get_obs(x))
  testthat::expect_equal(format(set_display(x, "{or} ({obs})"))[ok],
                         format(set_display(set_pvalue(x, NA_real_), "{or} ({obs})"))[ok])
  testthat::expect_true(all(grepl("(", format(set_display(x, "{or} ({obs})"))[ok], fixed = TRUE)))
})


test_that("the tooltip carries the comparison value once, and never on a cross-table", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
               family = "binomial", empirical = TRUE)
  tip <- tabxplor:::tab_tooltip_text(t$Model_OR)
  # a REFERENCE cell's crude value is the neutral itself: it collapses into the one "ref" token,
  # like every other comparison there.
  ok  <- !is.na(get_obs(t$Model_OR)) & !is_refrow(t$Model_OR)
  testthat::expect_true(all(grepl("obs: ", tip[ok], fixed = TRUE)))
  testthat::expect_equal(lengths(regmatches(tip[ok], gregexpr("obs: ", tip[ok], fixed = TRUE))),
                         rep(1L, sum(ok)))
  testthat::expect_false(any(grepl("obs: ", tip[!ok], fixed = TRUE)))
  # a cross-table has no `obs` -> the fragment never appears (the render snapshots must not move)
  ct <- tab(d, race, party3, color = TRUE)
  testthat::expect_false(any(grepl("obs: ", tabxplor:::tab_tooltip_text(ct[[2]]), fixed = TRUE)))
})


test_that("stars still ride the model estimate under color = 'adjustment'", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  testthat::expect_true(any(grepl("*", format(t$Model_OR, stars = TRUE), fixed = TRUE)))
  # the reference row keeps its bold anchor when the measure rides the TEXT channel
  t2 <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
                empirical = TRUE, color = "adjustment")
  testthat::expect_true(any(is_refrow(t2$Model_OR)))
  testthat::expect_identical(fmt_color_channels(t2$Model_OR)$text_slot[is_refrow(t2$Model_OR)],
                             integer(sum(is_refrow(t2$Model_OR))))   # a baseline is never coloured
})


# --- exports still build ------------------------------------------------------------------------------

test_that("every exporter renders an adjustment-coloured table without error", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  testthat::expect_no_error(format(t$Model_OR))
  testthat::expect_no_error(tab_md(t))
  testthat::expect_no_error(tab_html(t))
  testthat::expect_no_error(print(t))
  skip_if_not_installed("openxlsx2")
  testthat::expect_no_error(tab_xl(t, path = withr::local_tempfile(fileext = ".xlsx"), open = FALSE))
})


# --- Phase 18z13 (D2 / D4): the gap ladder reads the ESTIMATE's own scale -------------------------

test_that("D2: the additive gap is unit-invariant (hours / minutes / days colour identically)", {
  d <- adj_data()
  d$tv_hr  <- d$tvhours
  d$tv_min <- d$tvhours * 60
  d$tv_day <- d$tvhours / 24
  slots <- function(v) {
    t  <- suppressMessages(suppressWarnings(
      tab_reg(d, v, c("race", "party3"), family = "gaussian",
              empirical = TRUE, color = c(TRUE, "adjustment"), cleannames = FALSE)))
    mc <- grep("^Model_", names(t), value = TRUE)[[1]]
    fmt_color_channels(t[[mc]])$bg_slot
  }
  s_hr <- slots("tv_hr")
  testthat::expect_true(any(s_hr > 0L))                    # the fixture must actually colour something
  # z5 scored the raw difference against an ABSOLUTE ladder calibrated for percentage points, so the
  # same substantive adjustment saturated in minutes and vanished in days. Standardized by SD(Y), the
  # reading no longer depends on the unit the outcome happens to be recorded in.
  testthat::expect_identical(slots("tv_min"), s_hr)
  testthat::expect_identical(slots("tv_day"), s_hr)
})


test_that("D4: the gap's break glyphs follow the selected scale, not the measure", {
  d   <- adj_data()
  leg <- function(t) paste(tab_color_legend(t, medium = "plain", style = "terse"), collapse = " | ")

  # multiplicative estimate -> a multiplicative ladder
  t_mult <- suppressMessages(tab_reg(d, "married", c("race", "party3"), family = "binomial", link = "ratio",
                                     empirical = TRUE, color = c(TRUE, "adjustment"),
                                     cleannames = FALSE))
  l_mult <- leg(t_mult)
  testthat::expect_match(l_mult, "\u00d71.1", fixed = TRUE)   # x1.1
  testthat::expect_match(l_mult, "\u00f71.1", fixed = TRUE)   # div 1.1

  # additive estimate -> a signed ladder in the outcome's SD, never "x0.05"
  t_add <- suppressMessages(suppressWarnings(
    tab_reg(d, "tvhours", c("race", "party3"), family = "gaussian",
            empirical = TRUE, color = c(TRUE, "adjustment"), cleannames = FALSE)))
  # "+0.05" can only come from the gap ladder: `diff`'s own standardized breaks are 0.2/0.5/0.8.
  l_add <- leg(t_add)
  testthat::expect_match(l_add, "+0.05", fixed = TRUE)
  testthat::expect_match(l_add, "-0.05", fixed = TRUE)
  testthat::expect_false(grepl("\u00d70.05", l_add, fixed = TRUE))  # the z5 rendering of "+0.05"
})


# === SECTION: color = 'between_groups' and the interaction line ===================================

gap_data <- function() {
  d <- fx_gss()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d <- d[!is.na(d$race) & !is.na(d$party3) & !is.na(d$age), , drop = FALSE]
  tibble::as_tibble(d)
}


gap_tab <- function(d, policy = "ignore", preds = "race", ...)
  suppressMessages(tab_reg(d, outcome = "married", predictors = preds, tab_vars = "party3",
                           family = "binomial", color = c(TRUE, "between_groups"),
                           color_signif = policy, ...))


test_that("the gap p-value is the z test of the quadrature SE", {
  d  <- gap_data()
  sp <- gap_tab(d)
  g  <- log(get_or(sp[[reg_group_col(sp, "Rep")]])) - log(get_obs(sp[[reg_group_col(sp, "Rep")]]))
  testthat::expect_equal(fmt_gap_p(sp[[reg_group_col(sp, "Rep")]]),
                         2 * stats::pnorm(-abs(g / get_gap_se(sp[[reg_group_col(sp, "Rep")]]))))
})


test_that("`grey_non_signif` greys exactly the non-significant gaps", {
  d  <- gap_data()
  ig <- gap_tab(d, "ignore")
  gn <- gap_tab(d, "grey_non_signif")
  seen <- 0L
  for (nm in c("Dem", "Rep")) {
    p   <- fmt_gap_p(gn[[reg_group_col(gn, nm)]])
    sig <- !is.na(p) & p < 0.05
    s_i <- fmt_color_channels(ig[[reg_group_col(ig, nm)]])$bg_slot
    s_g <- fmt_color_channels(gn[[reg_group_col(gn, nm)]])$bg_slot
    testthat::expect_true(all(s_g[!sig] == 0L))       # every non-significant cell is grey
    testthat::expect_identical(s_g[sig], s_i[sig])    # a significant one keeps the observed intensity
    seen <- seen + sum(sig & s_i > 0L)
  }
  testthat::expect_gt(seen, 0L)                       # the fixture must actually exercise the gate
})


test_that("`guaranteed_effect` colours the CI FLOOR of the gap, on the null-direction pole", {
  d  <- gap_data()
  ig <- gap_tab(d, "ignore")
  gu <- gap_tab(d, "guaranteed_effect")
  checked <- 0L
  for (nm in c("Dem", "Rep")) {
    # is_refrow excludes the regression Constant (a baseline is never an effect -- MEASURES$gate_row),
    # which the gap test can perfectly well find significant.
    p   <- fmt_gap_p(gu[[reg_group_col(gu, nm)]])
    sig <- !is.na(p) & p < 0.05 & !is_refrow(gu[[reg_group_col(gu, nm)]])
    if (!any(sig)) next
    s_i <- fmt_color_channels(ig[[reg_group_col(ig, nm)]])$bg_slot
    s_g <- fmt_color_channels(gu[[reg_group_col(gu, nm)]])$bg_slot
    testthat::expect_true(all(s_g[!sig] == 0L))
    testthat::expect_true(all(s_g[sig] > 0L))         # coloured <=> significant, the mode's invariant
    # the floor is dimmer than the point estimate, and on the SAME side of the palette
    over_i <- s_i[sig] %in% 1:4; over_g <- s_g[sig] %in% 1:4
    testthat::expect_identical(over_g, over_i)
    checked <- checked + sum(sig)
  }
  testthat::expect_gt(checked, 0L)
})


test_that("the score's sign wins over the raw gap's, so a protective effect folds correctly", {
  # est 0.50 attenuated to 0.60 -> the raw gap is POSITIVE (log .6 > log .5) but the effect moved
  # TOWARD the null, so both the colour and its interval must land on the under side.
  x <- fmt(n = 1L, or = 0.60, obs = 0.50, gap_se = 0.05, scale = "odds_ratio", pct_type = "row",
           display = "or", color = "between_groups", color_signif = "grey_non_signif")
  testthat::expect_lt(fmt_adjustment_score(x), 1)              # attenuated
  testthat::expect_gt(fmt_gap_raw(x), 0)                       # ... though the raw gap is positive
  b <- tabxplor:::fmt_gap_bounds(x)
  testthat::expect_lt(b$hi, 1)                                 # the whole interval sits below 1
  testthat::expect_true(fmt_color_channels(x)$text_slot %in% 5:8)
})


# --- A. legend + tooltip --------------------------------------------------------------------------

test_that("the legend names the gap's own test, per channel", {
  d   <- gap_data()
  leg <- function(t) tab_color_legend(t, medium = "plain", style = "prose", lang = "en")
  l   <- leg(gap_tab(d, "guaranteed_effect"))
  testthat::expect_true(any(grepl("reference group's effect", l, fixed = TRUE)))
  testthat::expect_true(any(grepl("two independent estimates", l, fixed = TRUE)))
  # the background's own tail must NOT borrow the model's interval name. Phase 18z13 (D7): pick the
  # line that HAS a background -- the reference group's own column now says "reference group" instead of
  # printing a ladder no cell of it can reach, and forms its own legend line.
  with_bg <- grep("the reference group's effect", l, fixed = TRUE, value = TRUE)
  testthat::expect_gt(length(with_bg), 0L)
  bg <- sub(".*Background colour", "", with_bg[[1]])
  testthat::expect_true(grepl("two independent estimates", bg, fixed = TRUE))
  # ... and the baseline column says what it is, rather than naming unreachable thresholds
  testthat::expect_true(any(grepl("reference group", l, fixed = TRUE)))
})


test_that("the tooltip carries the gap, its interval and its p", {
  d <- gap_data()
  h <- as.character(tab_html(gap_tab(d, "ignore"), tooltips = TRUE))
  tips <- unlist(regmatches(h, gregexpr('title="[^"]*"', h)))
  gaps <- grep("gap: ", tips, fixed = TRUE, value = TRUE)
  testthat::expect_gt(length(gaps), 0L)
  testthat::expect_true(all(grepl("p = ", gaps, fixed = TRUE)))
  testthat::expect_true(all(grepl("[", gaps, fixed = TRUE)))
  # never on a table with no counterpart
  h2 <- as.character(tab_html(tab(d, race, party3, pct = "row"), tooltips = TRUE))
  testthat::expect_false(grepl("gap: ", h2, fixed = TRUE))
})


test_that("`color = 'between_groups'` turns the interaction test on; `stats=` asks for it alone", {
  d <- gap_data()
  # silently: the cost of the extra fit is an internal fact, not a statistical caveat.
  t <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                                family = "binomial", color = c(TRUE, "between_groups")))
  testthat::expect_length(tabxplor:::reg_interaction_lines(t, "en"), 1L)
  # off by default
  t0 <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                                 family = "binomial"))
  testthat::expect_length(tabxplor:::reg_interaction_lines(t0, "en"), 0L)
})


test_that("the interaction rows leave the GOF footer row-for-row unchanged", {
  d <- gap_data()
  base <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race",
                                   tab_vars = "party3", family = "binomial"))
  with <- gap_tab(d, "ignore")
  gof  <- function(t) { tt <- get_test(t); tt[tt$test %in% tabxplor:::reg_footer_test_types(), ] }
  testthat::expect_equal(gof(base), gof(with))
  # and the rendered footer BLOCK has the same rows
  rows <- function(t) grep("Model fit|^\\| *\\|", strsplit(tab_md(t), "\n")[[1]], value = TRUE)
  testthat::expect_identical(length(rows(base)), length(rows(with)))
})


test_that("the statistic follows compare=: F for gaussian, design-based Wald when weighted", {
  d <- gap_data()
  gs <- suppressMessages(tab_reg(d[!is.na(d$tvhours), ], outcome = "tvhours", predictors = "race",
                                 tab_vars = "party3", family = "gaussian",
                                 stats = c("n", "group_interaction")))
  testthat::expect_identical(unique(get_test(gs)$test[get_test(gs)$test %in%
                                                        tabxplor:::reg_interaction_types()]),
                             "group_interact_f")
  d$w <- 1 + (as.integer(d$race) %% 3) / 2                    # deterministic weights
  wt <- suppressWarnings(suppressMessages(
    tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
            family = "binomial", wt = "w", stats = c("n", "group_interaction"))))
  it <- get_test(wt); it <- it[it$test %in% tabxplor:::reg_interaction_types(), ]
  testthat::expect_identical(unique(it$test), "group_interact_wald")
  testthat::expect_true(all(!is.na(it$pvalue)))
})


# --- the at = "reference" estimand fix (a z5 defect) -----------------------------------------------

test_that("at = 'reference' writes no `obs`: the two columns are different estimands", {
  skip_if_not_installed("marginaleffects")
  d <- gap_data()
  testthat::expect_message(
    t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
                 effect = "at_reference", measure = "difference", empirical = TRUE),
    "at one profile")
  mcol <- reg_fmt_cols(t)[[1]]
  testthat::expect_true(all(is.na(get_obs(t[[mcol]]))))
  testthat::expect_true("Obs_RD" %in% names(t))         # the crude column is still shown
})


# --- Phase 18z13: D7 (the reference group is choosable) / D11 (no writes without a reader) --------

test_that("D7: `reference` picks the split_var baseline instead of the first level", {
  d  <- gap_data()
  # the baseline group is the one with no `obs` (a group is not compared to itself)
  base_of <- function(t) {
    fc <- reg_fmt_cols(t)
    fc[vapply(fc, function(nm) all(is.na(get_obs(t[[reg_group_col(t, nm)]]))), logical(1))]
  }
  b0 <- base_of(gap_tab(d))
  b1 <- base_of(gap_tab(d, ref = c(party3 = "Rep")))
  testthat::expect_true(grepl("Ind", b0[[1]], fixed = TRUE))   # the first level, by default
  testthat::expect_true(grepl("Rep", b1[[1]], fixed = TRUE))   # ... and it is choosable
  # z5/z8 sent `ref = NULL` into the split recursion and left tab_vars out of the relevelable
  # set, so the only way to move the baseline was to relevel the data upstream.
  testthat::expect_false(identical(b0, b1))
})


test_that("D11: obs / gap_se are written only where a gap measure reads them", {
  d  <- gap_data()
  # ⚠ `empirical = "column"`, not TRUE: since 22g-ii `tab_vars` resolve TRUE to "tooltip" (the
  # crude value computed, no column), and the crude COLUMNS are what this asserts about.
  sp <- suppressMessages(tab_reg(d, "married", list(m1 = "race", m2 = "race"),
                                 tab_vars = "party3", family = "binomial", link = "ratio",
                                 empirical = "column", color = c(TRUE, "between_groups")))
  fc <- reg_fmt_cols(sp)
  mdl <- fc[get_role(sp[fc]) == "model"]
  emp <- fc[get_role(sp[fc]) == "emp"]
  testthat::expect_gt(length(mdl), 0L)
  testthat::expect_gt(length(emp), 0L)
  # the model columns declare the measure, so they carry the comparison...
  testthat::expect_true(any(vapply(mdl, function(nm) any(!is.na(get_obs(sp[[reg_group_col(sp, nm)]]))), logical(1))))
  # ... and so do the crude companions, which take the model column's measure: `between_groups`
  # compares a cell to the SAME cell in another group, and a crude effect has a crude counterpart
  # there just as a modelled one does. (Contrast `adjustment`, whose baseline IS the crude column:
  # there its own `obs` stays empty and it is marked `refcol` instead.)
  for (nm in emp) {
    testthat::expect_true(any(!is.na(get_obs(sp[[reg_group_col(sp, nm)]]))), info = nm)
    testthat::expect_false(isTRUE(is_refcol(sp[[reg_group_col(sp, nm)]])), info = nm)
  }
})


# --- Phase 22b-iv: the reference GROUP is a reading anchor ---------------------------------------

test_that("`between_groups` marks the reference group's columns refcol, spread only", {
  d <- gap_data()
  sp <- suppressMessages(tab_reg(d, "married", "race", tab_vars = "party3", family = "binomial",
                                 color = "between_groups", empirical = TRUE))
  fc  <- names(sp)[vapply(sp, is_fmt, logical(1))]
  ref <- levels(forcats::fct_drop(as.factor(d$party3)))[[1]]
  marked <- fc[vapply(sp[fc], function(cl) isTRUE(is_refcol(cl)), logical(1))]
  testthat::expect_gt(length(marked), 0L)
  # exactly the model / crude columns of the FIRST group, and nothing else
  testthat::expect_true(all(vapply(sp[marked], function(cl)
    identical(get_col_group(cl), ref) && get_role(cl) %in% c("model", "emp"), logical(1))))
  # get_reference() bolds them whole, so the exporter picks them up as reference COLUMNS
  testthat::expect_true(all(marked %in%
    tabxplor:::tab_export_prep(sp, backend = "md")$tables[[1]]$bold_cols))
  # the STACKED shape has no reference column at all: there the reference group is a block of rows
  st <- suppressMessages(tab_reg(d, "married", list(m1 = "race", m2 = "race"), tab_vars = "party3",
                                 family = "binomial", color = "between_groups"))
  fst <- names(st)[vapply(st, is_fmt, logical(1))]
  testthat::expect_false(any(vapply(st[fst], function(cl) isTRUE(is_refcol(cl)), logical(1))))
})


# --- 22b-xiii-2 (C2): the SE is recovered with the crit that BUILT the interval -------------------

test_that("a t-referred column's gap SE is not inflated by t/z", {
  # reg_gap_se_of() used to divide every printed half-width by z, on the stated ground that "the gap
  # test is a z test throughout". But the interval it reads was built with t on any gaussian, quasi
  # or svyglm column, so the recovered SE came back inflated by exactly qt(df)/z -- +31 % at 5 df,
  # which costs discoveries and mis-sizes forest_plot()'s gap band. Recovering and testing are two
  # decisions: recover with the column's own critical value, test with z.
  d  <- gap_data()
  sp <- suppressMessages(tab_reg(d, outcome = "age", predictors = "race", tab_vars = "party3",
                                 family = "gaussian", color = c(TRUE, "between_groups")))
  fc <- reg_fmt_cols(sp)
  hand <- function(g) {
    f <- stats::lm(age ~ race, data = d[d$party3 == g, ])
    summary(f)$coefficients[, "Std. Error"]
  }
  se_ref <- hand("Ind")
  seen <- 0L
  for (g in c("Dem", "Rep")) {
    expected <- sqrt(hand(g)^2 + se_ref^2)
    got      <- get_gap_se(sp[[reg_group_col(sp, g)]])
    testthat::expect_equal(got[c(1L, 3L, 4L)], unname(expected), tolerance = 1e-6)
    seen <- seen + 1L
  }
  testthat::expect_identical(seen, 2L)
  # NON-VACUOUS: a gaussian column really is on t, so the old z recovery was a different number.
  mc <- sp[[reg_group_col(sp, "Dem")]]
  testthat::expect_true(is.finite(get_degf(mc)))
  testthat::expect_gt(conf_level_to_crit(0.95, get_degf(mc)), zscore_formula(0.95))
})


test_that("a pinned gap bound renders as the null, never as a negative zero", {
  # C3: `p$sign * pmax(0, |gap| - half)` is IEEE -0 whenever the near bound is pinned and the score is
  # negative (an ATTENUATED gap), and sprintf("%+.1f", -0) prints "-0.0" -- which reads as "just
  # excludes the null" when it IS the null, right beside a p-value saying the opposite.
  # Hand-built, because a fixture only pins a bound by luck: cell 1 is attenuated and not
  # significant (so its near bound pins), cell 2 is amplified and significant (so it does not).
  x  <- fmt(n = c(10L, 10L), diff = c(0.10, 0.30), obs = c(0.12, 0.10),
            gap_se = c(0.05, 0.02), scale = "points", color = "adjustment")
  bd <- tabxplor:::fmt_gap_bounds(x)
  pv <- tabxplor:::fmt_gap_p(x)
  testthat::expect_gt(pv[[1]], 0.05); testthat::expect_lt(pv[[2]], 0.05)
  pinned <- c(bd$lo, bd$hi)[c(bd$lo, bd$hi) == 0]
  testthat::expect_length(pinned, 1L)                       # cell 1 pins, cell 2 does not
  # rendered through format(), like every other interval in the package: the pinned bound must be a
  # bare "0", never "-0".
  ci <- tabxplor:::fmt_gap_text(x)$ci
  testthat::expect_identical(ci[[1]], "[-12;0]%")
  testthat::expect_false(any(grepl("-0[];]", ci)))
  # the multiplicative branch pins at exp(0) == 1, where a signed zero never showed.
  m  <- fmt(n = c(10L, 10L), or = c(1.10, 2.00), obs = c(1.20, 1.10),
            gap_se = c(0.30, 0.05), scale = "odds_ratio", color = "adjustment")
  mb <- tabxplor:::fmt_gap_bounds(m)
  testthat::expect_true(any(c(mb$lo, mb$hi) == 1))
})


# --- Phase 22b-xviii: a model that IS its own crude twin has no gap ------------------------------

test_that("a gap at machine precision is NA, not a z of 20", {
  # a UNIVARIABLE model equals its own crude fit exactly, so both the gap and its SE are floating-
  # point dust -- and their ratio was a p of 1e-92 that painted the column at full strength.
  d <- gap_data()
  t <- suppressMessages(tab_reg(d, "age", "race", family = "gaussian", empirical = TRUE,
                                stats = FALSE))
  col <- t[["Model_diff"]]
  eff <- !is_refrow(col) & !is.na(get_obs(col))
  testthat::expect_true(any(eff))
  testthat::expect_true(all(is.na(fmt_gap_p(col)[eff])))
  testthat::expect_true(all(is.na(fmt_adjustment_score(col)[eff])))
  testthat::expect_true(all(fmt_color_channels(set_color(col, "adjustment"))$bg_slot == 0L))
  # the tolerance is RELATIVE: a genuinely tiny gap with an honest SE keeps its interval
  x <- fmt(n = c(10L, 10L), diff = c(1e-6, 1e-6), obs = c(0, 0), gap_se = c(1e-7, 1e-7),
           scale = "points", color = "adjustment")
  testthat::expect_false(any(is.na(fmt_gap_p(x))))
})


# === SECTION: the marginal engine against marginaleffects =========================================

skip_if_no_me <- function() testthat::skip_if_not_installed("marginaleffects")


mg_data <- function() {
  d <- fx_gss()
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


testthat::test_that("the RATIO contrast (the log link) matches too", {
  skip_if_no_me()
  d <- mg_data()
  f <- stats::glm(married ~ race + party3 + age, data = d, family = stats::binomial())
  expect_engines_agree(f, f$model, c("race", "party3", "age"), link = "log")
  g <- stats::glm(hours ~ race + age, data = d, family = stats::poisson())
  expect_engines_agree(g, g$model, c("race", "age"), link = "log")
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


# --- the refusals ----------------------------------------------------------------------------------

testthat::test_that("the fast route refuses rather than guessing, and the call falls back whole", {
  skip_if_no_me()
  d <- mg_data()
  f <- stats::glm(married ~ race + age, data = d, family = stats::binomial())
  # a predictor that is not in the model has no counterfactual: refuse (the compound-formula path)
  testthat::expect_null(
    tabxplor:::reg_marginal_gcomp(f, f$model, c("race", "party3"), 0.95))
  # an absent factor level is no answer, not an NA column
  g <- tabxplor:::reg_gcomp_maker(f, f$model, NULL, "identity")
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
  g   <- tabxplor:::reg_gcomp_maker(f, dd, NULL, "identity")
  p   <- g("race", "Black", ref)
  se_print <- tabxplor:::reg_delta_se(p$G, stats::vcov(f))
  se_gap   <- tabxplor:::reg_if_se(tabxplor:::reg_ame_if_maker(
    f, dd, NULL, "identity", tabxplor:::reg_coef_if_maker(f))("race", "Black", ref))
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
            effect = "marginal", measure = "difference", empirical = TRUE)))
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
                                effect = "marginal", measure = "difference"))
  col <- x[[grep("^Model", names(x))[1]]]
  fin <- !is.na(get_pvalue(col))   # a reference cell carries the neutral, and no interval
  testthat::expect_true(any(fin))
  testthat::expect_true(all(is.finite(get_ci_inf(col)[fin])))
  testthat::expect_true(all(get_ci_inf(col)[fin] <= get_diff(col)[fin]))
  testthat::expect_true(all(get_ci_sup(col)[fin] >= get_diff(col)[fin]))
  testthat::expect_true(all(!is.na(get_pvalue(col)[fin])))
})


# === SECTION: gap_se: the influence-function SE of the gap ========================================

gapb_data <- function() {
  d <- fx_gss()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d <- d[!is.na(d$race) & !is.na(d$party3) & !is.na(d$tvhours), , drop = FALSE]
  tibble::as_tibble(d)
}


# the canonical builder: a COLLAPSIBLE binary-outcome table -- `link = "ratio"` is the modified
# Poisson, whose coefficient is a CONDITIONAL risk ratio, which is the estimand the ruling points
# users at. `relig` is in the default predictor set because it is what
# makes some gaps actually REACH the first adj_ratio break (x1.1) -- a fixture that colours nothing
# would let the policy tests pass vacuously.
gapb_tab <- function(d, policy = "ignore", preds = c("race", "party3", "relig"), ...)
  suppressMessages(tab_reg(d, outcome = "married", predictors = preds, family = "binomial",
                           link = "ratio",
                           empirical = TRUE, color = c(TRUE, "adjustment"),
                           color_signif = policy, ...))


gapb_model_col <- function(t) t[[grep("^Model", names(t), value = TRUE)[[1]]]]


# Phase 18z13 (D1). Before it, the crude block was built on the UNION of predictors while each model
# dropped its own NA rows, so a smaller model was fitted on MORE people. The framework detected that
# (it withheld the gap SE) and coloured the gap anyway -- so `m1 = race`, which IS the crude model and
# whose true adjustment gap is exactly zero, rendered a coloured cell claiming a 16 % move. Two halves:
# the default now puts every model of an outcome on one population, and where they still differ the
# `obs` write is gated by the same predicate that gates the test.
adjgap_inc_data <- function() {
  d <- gapb_data()
  d$inc <- factor(dplyr::case_when(d$rincome %in% c("Not applicable", "No answer", "Don't know",
                                                    "Refused") ~ NA_character_,
                                   d$rincome %in% "$25000 or more" ~ "hi", TRUE ~ "lo"),
                  levels = c("lo", "hi"))
  d
}


adjgap_mods <- list(m1 = "race", m2 = c("race", "inc"))


test_that("`ignore` is byte-identical to the descriptive z5 reading", {
  d <- gapb_data()
  t <- gapb_tab(d, "ignore")
  x <- gapb_model_col(t)
  testthat::expect_false(all(is.na(get_gap_se(x))))          # the SE exists ...
  testthat::expect_identical(fmt_color_channels(x)$bg,       # ... and changes nothing under `ignore`
                             fmt_color_channels(set_gap_se(x, NA_real_))$bg)
})


# --- legend + tooltip -----------------------------------------------------------------------------

test_that("the legend names the gap's own test, and only caveats a non-collapsible scale", {
  d   <- gapb_data()
  leg <- function(t) tab_color_legend(t, medium = "plain", style = "prose", lang = "en")
  l   <- leg(gapb_tab(d, "grey_non_signif"))
  testthat::expect_true(any(grepl("two estimates fitted on the same sample", l, fixed = TRUE)))
  testthat::expect_true(any(grepl("than the observed column", l, fixed = TRUE)))
  testthat::expect_false(any(grepl("non-collapsibility", l, fixed = TRUE)))
  # the OR path: the caveat fires, and the background clause must NOT claim a greying that never
  # happened (the text channel is greyed by its Wald interval, the background is not gated at all)
  o <- suppressMessages(tab_reg(d, "married", c("race", "party3"), family = "binomial",
                                empirical = TRUE, color = c(TRUE, "adjustment"),
                                color_signif = "grey_non_signif"))
  lo <- leg(o)
  testthat::expect_true(any(grepl("non-collapsibility", lo, fixed = TRUE)))
  testthat::expect_false(any(grepl("Background: the same rule", lo, fixed = TRUE)))
  # ... while a scale that IS tested says so
  testthat::expect_true(any(grepl("Background: the same rule", l, fixed = TRUE)))
})


test_that("the tooltip carries the gap, its interval and its p", {
  d <- gapb_data()
  h <- tab_html(gapb_tab(d, "grey_non_signif"), tooltips = TRUE)
  s <- paste(as.character(h), collapse = "")
  testthat::expect_true(grepl("gap: ", s, fixed = TRUE))
  testthat::expect_true(grepl("obs: ", s, fixed = TRUE))
})


test_that("every exporter renders an adjustment-tested table without error", {
  d <- gapb_data()
  t <- gapb_tab(d, "grey_non_signif")
  testthat::expect_no_error(format(t))
  testthat::expect_no_error(tab_md(t))
  testthat::expect_no_error(tab_html(t))
  testthat::expect_no_error(utils::capture.output(print(t)))
})


# === SECTION: color = 'adjustment' ================================================================

adj_data <- function() {
  d <- fx_gss()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d$inc3    <- factor(dplyr::case_when(d$rincome %in% c("$25000 or more") ~ "hi",
                                       d$rincome %in% c("Not applicable", "No answer",
                                                        "Don't know", "Refused") ~ NA_character_,
                                       TRUE ~ "lo"),
                      levels = c("lo", "hi"))
  d <- d[!is.na(d$tvhours) & !is.na(d$race) & !is.na(d$party3), , drop = FALSE]
  tibble::as_tibble(d)
}


test_that("obs is NA (-> uncoloured) wherever there is no crude counterpart", {
  d <- adj_data()
  # The Constant has no crude counterpart. (Phase 18z9: a NUMERIC predictor now HAS one -- its
  # univariable fit -- so it is no longer part of this list; see the next test.)
  t <- tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  o <- get_obs(t$Model_OR)
  testthat::expect_true(is.na(o[[1]]))                              # Constant
  testthat::expect_identical(fmt_color_channels(t$Model_OR)$bg_slot[[1L]], 0L)

  # a compound formula has no predictor structure to be crude about -- the one remaining gap.
  t <- suppressMessages(tab_reg(d, married ~ race * age, family = "binomial",
                                empirical = TRUE, color = c(TRUE, "adjustment")))
  testthat::expect_true(all(is.na(get_obs(t[[ncol(t)]]))))

  # a plain cross-table never fills the field.
  testthat::expect_true(all(is.na(get_obs(tab(d, race, party3, color = TRUE)[[2]]))))
})


test_that("a MULTINOMIAL model gets one obs PER OUTCOME CATEGORY (Phase 18z10)", {
  # z10 inverted this test's premise: the univariable multinomial IS saturated, so its crude OR is the
  # {category j, reference category} x {level, reference level} Woolf ratio -- which is exactly what
  # tab(pct = "row", display = "{or}", ref = "first") prints. Each model column carries its own category's `obs`.
  skip_if_not_installed("nnet")
  d <- adj_data()
  t <- suppressMessages(tab_reg(d, outcome = "party3", predictors = "race",
                                family = "multinomial", empirical = TRUE, cleannames = FALSE))
  mcols <- reg_fmt_cols(t)
  testthat::expect_gt(length(mcols), 1L)
  obs <- lapply(mcols, function(nm) get_obs(t[[nm]]))
  testthat::expect_true(all(vapply(obs, function(o) any(!is.na(o)), logical(1))))
  # the categories really differ -- one shared vector would be the bug this keys against
  testthat::expect_false(isTRUE(all.equal(obs[[1]], obs[[2]])))

  # ... and each equals the crude OR tab() shows for that category
  ct <- tab(d, race, party3, pct = "row", display = "{or}", ref = "first", na = "drop", ref2 = 1)
  lv <- levels(forcats::fct_drop(stats::na.omit(d$race)))
  for (j in seq_along(mcols)) {
    cat_j <- sub(" vs .*$", "", mcols[[j]])
    if (!cat_j %in% names(ct)) next
    got  <- get_obs(t[[mcols[[j]]]])[match(lv, as.character(t$levels))]
    want <- get_or(ct[[cat_j]])[match(lv, as.character(ct[[1]]))]
    testthat::expect_equal(unname(got), unname(want), tolerance = 1e-8)
  }
})


# --- the direction rule ------------------------------------------------------------------------------
# The bug this prevents: with a raw sign, a protective effect attenuated toward 1 moves UP while a
# risky one attenuated toward 1 moves DOWN, so the two halves of the palette would mean nothing.

test_that("the score is toward/away from the null, not raw up/down", {
  mk <- function(est, obs) fmt(n = rep(1L, length(est)), or = est, obs = obs,
                              scale = "odds_ratio", pct_type = "row", display = "or", ref = "1",
                              color = "adjustment")
  # both ATTENUATED by the same factor 1.2, one protective one risky -> same side, same magnitude
  s <- tabxplor:::fmt_adjustment_score(mk(c(0.5 * 1.2, 2 / 1.2), c(0.5, 2)))
  testthat::expect_equal(s[[1]], s[[2]])
  testthat::expect_lt(s[[1]], 1)                                   # attenuated = the under side
  # both STRENGTHENED -> the over side
  s <- tabxplor:::fmt_adjustment_score(mk(c(0.5 / 1.2, 2 * 1.2), c(0.5, 2)))
  testthat::expect_equal(s[[1]], s[[2]])
  testthat::expect_gt(s[[1]], 1)
  # a REVERSAL (the two on opposite sides of the null) reads as attenuation: whatever the observed
  # effect claimed, the model says it is not that. Its magnitude is the FULL move, so a big flip is
  # a big move -- and a perfect mirror can no longer score as the deepest "strengthened".
  testthat::expect_lt(tabxplor:::fmt_adjustment_score(mk(1.2, 0.9)), 1)
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(mk(1.2, 0.9)), 0.9 / 1.2)
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(mk(0.5, 2)), 0.25)
  # equal estimates are neutral, whatever the scale
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(mk(2, 2)), 1)
  add <- fmt(n = c(1L, 1L), diff = c(0.1, -0.1), obs = c(0.1, -0.1),
             scale = "points", pct_type = "row", display = "diff", color = "adjustment")
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(add), c(0, 0))
})


# --- the API boundary --------------------------------------------------------------------------------

test_that("color = 'adjustment' turns empirical on, and the two measures are exclusive", {
  d <- adj_data()
  testthat::expect_message(
    # the note fires where it says something: `empirical` was turned OFF and the colour turns it
    # back on. It is silent on the default, which already has the companion.
    t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), empirical = FALSE,
                 family = "binomial", color = c(TRUE, "adjustment")),
    "empirical")
  testthat::expect_true("Obs_OR" %in% names(t))
  testthat::expect_error(
    tab_reg(d, outcome = "married", predictors = "race", family = "binomial",
            color = c("adjustment", "between_groups")),
    "cannot be used together")
  # tab() names them rather than emitting a bare "unknown measure"
  testthat::expect_error(tab(d, race, party3, color = "adjustment"), "tab_reg")
})


# --- the {obs} display token -------------------------------------------------------------------------

test_that("{obs} renders bare and in a composite, and round-trips through get_num/set_num", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
               family = "binomial", empirical = TRUE)
  x <- t$Model_OR
  bare <- format(set_display(x, "obs"))
  testthat::expect_true(any(grepl("^\\s*\\d+\\.\\d{2}$", bare)))    # an OR-scale number, 2 decimals
  comp <- format(set_display(x, "{or} (obs {obs})"))
  testthat::expect_true(all(grepl("(obs ", comp[!is.na(get_obs(x))], fixed = TRUE)))
  testthat::expect_equal(get_num(set_display(x, "obs")), get_obs(x))
  v <- seq_along(x) + 0
  testthat::expect_equal(get_obs(set_num(set_display(x, "obs"), v)), v)
  # Excel shows the PRIMARY token only, and its code matches the OR mask (bare, 2 decimals) -- with
  # the second section an odds ratio takes on every medium, so a cell below the neutral reads "1/x"
  testthat::expect_equal(unique(format(set_display(x, "obs"), syntax = "excel")),
                         "#,##0.00;\\1\\/#,##0.00")
  # an AME column's obs is a probability difference -> x100, signed, "%" (both media agree)
  a <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
               effect = "marginal", measure = "difference", empirical = TRUE)
  ac <- a[[grep("^Model_", names(a), value = TRUE)[[1]]]]
  testthat::expect_true(any(grepl("%$", format(set_display(ac, "obs")))))
  testthat::expect_true(any(grepl("^\\+", trimws(format(set_display(ac, "obs")), whitespace = "[\\h\\v]"))))
  aok <- !is.na(get_obs(ac))            # an empty cell writes no number, so its code is irrelevant
  testthat::expect_true(all(grepl("%", format(set_display(ac, "obs"), syntax = "excel")[aok])))
})


# === SECTION: color = 'between_groups' and the interaction line ===================================

gap_data <- function() {
  d <- fx_gss()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d <- d[!is.na(d$race) & !is.na(d$party3) & !is.na(d$age), , drop = FALSE]
  tibble::as_tibble(d)
}


gap_tab <- function(d, policy = "ignore", preds = "race", ...)
  suppressMessages(tab_reg(d, outcome = "married", predictors = preds, tab_vars = "party3",
                           family = "binomial", color = c(TRUE, "between_groups"),
                           color_signif = policy, ...))


test_that("gap_se is NA on every table that has no counterpart", {
  d <- gap_data()
  # a crosstab
  ct <- tab(d, race, party3, pct = "row")
  testthat::expect_true(all(vapply(ct[reg_fmt_cols(ct)],
                                   function(c) all(is.na(get_gap_se(c))), logical(1))))
  # a reg table with no split_var
  t <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", family = "binomial"))
  testthat::expect_true(all(is.na(get_gap_se(t$Model_OR))))
  # profile-likelihood bounds are not est +/- crit*se, so no SE is recovered from them
  pr <- gap_tab(d, ci_method = "profile")
  testthat::expect_true(all(is.na(get_gap_se(pr[[reg_group_col(pr, "Dem")]]))))
})


# --- A. the three policies ------------------------------------------------------------------------

test_that("a gap measure gates on its OWN interval -- `ignore` cannot turn the test off", {
  d  <- gap_data()
  sp <- gap_tab(d, "ignore")
  # ⚠ 22b-xviii: there is no meaningful "ignore" for a COMPARISON of two estimates. Stripping the
  # gap SE removes the test, and only then does the column fall back to colouring every movement --
  # so a column WITH standard errors must colour strictly fewer cells than the same one without.
  seen <- 0L
  for (nm in reg_fmt_cols(sp)) {
    col  <- sp[[reg_group_col(sp, nm)]]
    bare <- set_gap_se(col, rep(NA_real_, length(col)))
    tested   <- fmt_color_channels(col)$bg_slot
    describe <- fmt_color_channels(bare)$bg_slot
    testthat::expect_true(all(tested == 0L | tested == describe))
    seen <- seen + sum(describe != 0L & tested == 0L)
  }
  testthat::expect_gt(seen, 0L)   # at least one non-significant movement really is greyed
})


test_that("the footer line reaches every medium, once per model", {
  d <- gap_data()
  t <- gap_tab(d, "ignore", preds = c("race", "age"))
  ln <- tabxplor:::reg_interaction_lines(t, "en")
  testthat::expect_length(ln, 1L)
  testthat::expect_match(ln, "Interaction with party3")
  testthat::expect_match(ln, "race p = ")
  # it survives footer MATERIALISATION (which drops `test`) into md and html
  testthat::expect_true(any(grepl("Interaction with party3",
                                  strsplit(tab_md(t), "\n")[[1]], fixed = TRUE)))
  h <- as.character(tab_html(t))
  testthat::expect_true(grepl("Interaction with party3", h, fixed = TRUE))
  # and its p-values are entity-safe: a bare "<0.01%" in a raw-html footer is at a parser's mercy,
  # and the stars must not read as markdown emphasis on a knitted page
  line <- regmatches(h, regexpr("Interaction with party3.*?<br>", h, perl = TRUE))
  testthat::expect_length(line, 1L)
  testthat::expect_match(line, "&lt;0")                       # the p-value, entity-encoded
  testthat::expect_false(grepl("<0", line, fixed = TRUE))
  testthat::expect_false(grepl("*",  line, fixed = TRUE))     # stars are &#42;
})


test_that("an unsupported engine degrades to no line, never to an error", {
  d <- gap_data()
  mn <- suppressWarnings(suppressMessages(
    tab_reg(d, outcome = "party3", predictors = "race", tab_vars = "marital",
            family = "multinomial", stats = c("n", "group_interaction"))))
  testthat::expect_length(tabxplor:::reg_interaction_lines(mn, "en"), 0L)
  testthat::expect_no_error(tab_md(mn))
})


# === SECTION: the marginal engine against marginaleffects =========================================

skip_if_no_me <- function() testthat::skip_if_not_installed("marginaleffects")


mg_data <- function() {
  d <- fx_gss()
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
  eng <- function(fam, eff, m)
    tabxplor:::reg_marginal_engine(tabxplor:::reg_estimand(fam, measure = m, effect = eff))
  for (fam in c("gaussian", "binomial", "poisson", "multinomial", "ordinal")) {
    testthat::expect_equal(eng(fam, "marginal", "difference"), "gcomp")
    # a one-row profile grid is not something g-computation builds: declared numeric, on purpose.
    testthat::expect_equal(eng(fam, "at_reference", "difference"), "marginaleffects")
  }
  testthat::expect_equal(eng("multinomial", "at_reference", "odds_ratio"), "marginaleffects")
  # the rule is DERIVED from the contrast alone -- there is no per-row override to forget to set
  testthat::expect_equal(tabxplor:::reg_marginal_engine(list(effect = "marginal")), "gcomp")
  testthat::expect_true(all(c("gcomp", "marginaleffects") %in% tabxplor:::REG_MARGINAL_ENGINES))
})


# --- 3+ level parity -------------------------------------------------------------------------------

testthat::test_that("multinomial and ordinal marginal effects match, every outcome category", {
  skip_if_no_me()
  testthat::skip_if_not_installed("nnet")
  testthat::skip_if_not_installed("MASS")
  d <- mg_data()
  m <- nnet::multinom(party3 ~ race + age, data = d, trace = FALSE)
  expect_engines_agree(m, d, c("race", "age"))
  expect_engines_agree(m, d, c("race", "age"), link = "log")

  d$inc3 <- factor(dplyr::ntile(d$age, 3), labels = c("low", "mid", "high"))
  p <- MASS::polr(inc3 ~ race + hours, data = d, Hess = TRUE)
  expect_engines_agree(p, d, c("race", "hours"))
})
