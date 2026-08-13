# PURPOSE: Phase 18z8-B -- the significance test of the `color = "adjustment"` gap: the model effect
#          against its OBSERVED (crude) counterpart, both fitted on the SAME rows, so the standard
#          error of their difference needs the difference of their INFLUENCE FUNCTIONS
#          (R/reg-influence.R). This is what unpins `color_signif` from `ignore`.
# ROLE: the behavioural lock. The governing claim is that neither leg is invented: our model influence
#       function reproduces the model's OWN design-based standard error exactly, and our crude one
#       reproduces the Woolf interval the `Obs_OR` column already prints. Only the COVARIANCE between
#       them is new, and it is bounded on both sides by quantities the table shows.
# KEY CONSTRAINTS:
#   - Gated to COLLAPSIBLE estimands (maintainer ruling Q1(b)). A conditional odds ratio moves under
#     adjustment with ZERO confounding, so no `gap_se` is written there at all and the colours stay
#     descriptive -- the same table on a risk-ratio scale must show the contrast.
#   - Both estimators must solve their equations on the SAME observations: no gap SE when the model's
#     complete cases differ from the observed block's (the default per-model NA drop, in comparison
#     mode), when the fitted object was distilled away (jamovi's digest path), or at a profile.
#   - `color_signif = "ignore"` must be BYTE-IDENTICAL to z5 wherever a gap_se now exists.
#   - The crude and model columns must be the SAME estimand -- this closes a z5 defect where
#     effect = "ame" + family = "poisson" wrote a rate RATIO into an additive AME column's `obs`.
# See: dev/model_vs_observed_gap_test.md (SS2 the estimator side, SS3 the variance side, SS4 what the
#      test rejects and why the OR path is off, SS7 the architecture).

gapb_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d <- d[!is.na(d$race) & !is.na(d$party3) & !is.na(d$tvhours), , drop = FALSE]
  tibble::as_tibble(d)
}

# the canonical builder: a COLLAPSIBLE binary-outcome table (modified Poisson -> risk ratios), which is
# the estimand the ruling points users at. `relig` is in the default predictor set because it is what
# makes some gaps actually REACH the first adj_ratio break (x1.1) -- a fixture that colours nothing
# would let the policy tests pass vacuously.
gapb_tab <- function(d, policy = "ignore", preds = c("race", "party3", "relig"), ...)
  suppressMessages(tab_reg(d, dependent = "married", predictors = preds, family = "poisson",
                           empirical = TRUE, color = c("OR", "adjustment"),
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
    testthat::expect_equal(tabxplor:::reg_if_se(mk(L), f$survey.design),
                           unname(survey::SE(f)[tm]), tolerance = 1e-10)
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
  for (ratio in c(FALSE, TRUE)) {
    am <- tabxplor:::reg_ame_if_maker(f, dd, NULL, ratio = ratio, coef_if = mk)
    me <- as.data.frame(do.call(marginaleffects::avg_comparisons, c(
      list(f, variables = "race", newdata = dd),
      if (ratio) list(comparison = "lnratioavg") else list())))
    got <- tabxplor:::reg_if_se(am("race", lv, ref))
    # marginaleffects reports the DELTA term (covariates held fixed); the full influence function adds
    # the empirical-averaging term, worth ~0.1 %. Agreement to 2 % is the honest assertion.
    testthat::expect_equal(got, me$std.error[me$contrast == me$contrast[1]][1], tolerance = 0.02)
  }
})

# --- end to end: the gap SE written into the column -----------------------------------------------

test_that("gap_se is the influence-function SE of the difference, and its p its z test", {
  skip_if_not_installed("survey")
  d <- gapb_data()
  t <- gapb_tab(d, preds = "race")
  x <- gapb_model_col(t)

  dd <- tidyr::drop_na(d[, c("married", "race")])
  dd$y <- as.numeric(dd$married == "no")     # the modelled level (inverse_two_level_factors default)
  dd$.w <- 1
  des <- survey::svydesign(ids = ~1, weights = ~.w, data = dd)
  fs  <- survey::svyglm(y ~ race, design = des, family = stats::quasipoisson())
  mk  <- tabxplor:::reg_coef_if_maker(fs)
  ci  <- tabxplor:::reg_crude_if_maker(dd, "married", "rr", "no", NULL, "log")
  ref <- levels(dd$race)[1]
  seen <- 0L
  for (lv in levels(dd$race)[-1]) {
    k <- which(as.character(t$var) == "race" & as.character(t$levels) == lv)
    d_i <- mk(stats::setNames(1, paste0("race", lv))) - ci("race", lv, ref)
    testthat::expect_equal(get_gap_se(x)[k], tabxplor:::reg_if_se(d_i, fs$survey.design),
                           tolerance = 1e-10)
    seen <- seen + 1L
  }
  testthat::expect_gt(seen, 0L)
  g <- log(get_or(x)) - log(get_obs(x))
  testthat::expect_equal(fmt_gap_p(x), 2 * stats::pnorm(-abs(g / get_gap_se(x))))
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
  z <- conf_level_to_z(0.95, digits = 12)
  se_m <- tabxplor:::reg_gap_se_of(x, z)
  se_o <- tabxplor:::reg_gap_se_of(o, z)
  g    <- get_gap_se(x)
  k    <- which(is.finite(g) & is.finite(se_m) & is.finite(se_o))
  testthat::expect_gt(length(k), 3L)
  testthat::expect_true(all(g[k] <= se_m[k] + se_o[k] + 1e-9))
  testthat::expect_true(all(g[k] >= abs(se_m[k] - se_o[k]) - 1e-9))
  # and it is MUCH tighter than the independent quadrature -- that is the whole point
  testthat::expect_true(median(g[k] / sqrt(se_m[k]^2 + se_o[k]^2)) < 0.6)
})

# --- the gate: every clause, one fixture each ------------------------------------------------------

test_that("no gap_se where the gap has no honest test", {
  d <- gapb_data()
  none <- function(t) all(is.na(get_gap_se(gapb_model_col(t))))

  # (1) a CONDITIONAL odds ratio -- non-collapsible, ruling Q1(b). Both exponentiate directions.
  testthat::expect_true(none(suppressMessages(tab_reg(
    d, "married", c("race", "party3"), family = "binomial", empirical = TRUE,
    color = c("OR", "adjustment")))))
  testthat::expect_true(none(suppressMessages(tab_reg(
    d, "married", c("race", "party3"), family = "binomial", exponentiate = FALSE,
    empirical = TRUE, color = c("diff", "adjustment")))))
  # (2) at the reference profile the model cell is a different estimand (a z5 defect z8-A fixed)
  skip_if_not_installed("marginaleffects")
  testthat::expect_true(none(suppressMessages(tab_reg(
    d, "married", c("race", "party3"), family = "binomial", effect = "ame", at = "reference",
    empirical = TRUE, color = c("diff", "adjustment")))))
  # (3) no crude twin at all: multinomial
  m <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE,
                                color = c("OR", "adjustment")))
  testthat::expect_true(all(vapply(m[vapply(m, is_fmt, logical(1))],
                                   function(c) all(is.na(get_gap_se(c))), logical(1))))
})

# Phase 18z17 (ruling D2). The gap SE used to be gated on `color = "adjustment"` -- a fact withheld
# because nobody had asked to COLOUR it, which held only while the colour engine was its one reader.
# forest_plot() is the second, so the premise is now validity alone: `empirical = TRUE` + the five
# correctness clauses. Nothing RENDERS differently (a gap measure is still the only consumer of the
# stored value), which is why the goldens do not move.
test_that("z17 D2: gap_se is written without asking for the colour", {
  d <- gapb_data()
  plain  <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", empirical = TRUE))
  asked  <- suppressMessages(tab_reg(d, "married", "race", family = "poisson", empirical = TRUE,
                                     color = c("OR", "adjustment")))
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
                                family = "poisson", empirical = TRUE, color = c("OR", "adjustment")))
  fc <- grep("^m1|^m2", names(t), value = TRUE)
  testthat::expect_length(fc, 2L)
  # both models now solve their equations on the crude block's rows -> both carry a real gap SE (D5:
  # one significance policy governs the table instead of two).
  for (nm in fc) testthat::expect_false(all(is.na(get_gap_se(t[[nm]]))))
  # m1 IS the crude model: its estimate and its `obs` are the same number, so the gap is exactly 0 and
  # nothing is coloured. That equality is the whole of D1.
  # the union skeleton also carries m2's `inc` rows, which m1 does not estimate -> exclude them
  i <- which(!is_refrow(t[[fc[[1]]]]) & as.character(t$var) != "Constant" &
               !is.na(get_or(t[[fc[[1]]]])))
  testthat::expect_gt(length(i), 0L)
  testthat::expect_equal(get_or(t[[fc[[1]]]])[i], get_obs(t[[fc[[1]]]])[i], tolerance = 1e-8)
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(t[[fc[[1]]]])[i], rep(1, length(i)),
                         tolerance = 1e-8)
  testthat::expect_true(all(fmt_color_channels(t[[fc[[1]]]])$bg_slot == 0L))
})

test_that("D1: under the opt-in per-model drop, a model on other rows gets NO obs at all", {
  d <- adjgap_inc_data()
  t <- suppressMessages(tab_reg(d, "married", predictors = adjgap_mods, na = "drop_by_model",
                                family = "poisson", empirical = TRUE, color = c("OR", "adjustment")))
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
            family = "poisson", empirical = TRUE, color = c("OR", "adjustment")),
    "drop_by_model")
})

test_that("a crude companion on another scale writes neither obs nor a gap SE (a z5 defect)", {
  # reg_empirical_columns() ignores `effect` on the poisson branch, so effect = "ame" pairs an ADDITIVE
  # count AME with the crude rate RATIO. z5 wrote that ratio into `obs` and scored the difference of
  # two scales; reg_same_estimand() now gates both.
  skip_if_not_installed("marginaleffects")
  d <- gapb_data()
  # (tvhours is over-dispersed -> reg_fit warns and phi-scales; irrelevant here, and suppressed so the
  #  assertion is about the estimand mismatch alone)
  t <- suppressWarnings(suppressMessages(tab_reg(d, "tvhours", "race", family = "poisson",
                                                 effect = "ame", empirical = TRUE,
                                                 color = c("diff", "adjustment"))))
  x <- gapb_model_col(t)
  testthat::expect_identical(as.character(get_ci_type(x))[1], "diff")
  testthat::expect_true(all(is.na(get_obs(x))))
  testthat::expect_true(all(is.na(get_gap_se(x))))
  # the coefficient path on the same data DOES match scales, so it keeps both
  t2 <- suppressWarnings(suppressMessages(tab_reg(d, "tvhours", "race", family = "poisson",
                                                  empirical = TRUE, color = c("OR", "adjustment"))))
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
    color = c("OR", "adjustment"), color_signif = "guaranteed_effect"))), "ignore")
  # with no column to ask, the caller's policy stands (the accessor must not force on a NULL)
  testthat::expect_identical(
    tabxplor:::measure_policy("adjustment", "grey_non_signif"), "grey_non_signif")
})

test_that("the same predicate fixes between_groups under method = 'profile'", {
  # A profile-likelihood bracket is not est +/- crit*se, so no SE is recoverable -- and before z8-B
  # that read as `grey_non_signif` and greyed the WHOLE column instead of falling back to the
  # descriptive reading.
  d  <- gapb_data()
  pr <- suppressMessages(tab_reg(d, "married", "race", split_var = "party3", family = "binomial",
                                 color = c("OR", "between_groups"),
                                 color_signif = "grey_non_signif", method = "profile"))
  ig <- suppressMessages(tab_reg(d, "married", "race", split_var = "party3", family = "binomial",
                                 color = c("OR", "between_groups"),
                                 color_signif = "ignore", method = "profile"))
  cn <- names(pr)[vapply(pr, is_fmt, logical(1))]
  for (nm in cn) {
    testthat::expect_true(all(is.na(get_gap_se(pr[[nm]]))))
    testthat::expect_identical(fmt_color_channels(pr[[nm]])$bg, fmt_color_channels(ig[[nm]])$bg)
  }
  testthat::expect_gt(sum(vapply(cn, function(nm) sum(fmt_color_channels(pr[[nm]])$bg != 0L),
                                 integer(1))), 0L)   # and it really colours something
})

test_that("`ignore` is byte-identical to the descriptive z5 reading", {
  d <- gapb_data()
  t <- gapb_tab(d, "ignore")
  x <- gapb_model_col(t)
  testthat::expect_false(all(is.na(get_gap_se(x))))          # the SE exists ...
  testthat::expect_identical(fmt_color_channels(x)$bg,       # ... and changes nothing under `ignore`
                             fmt_color_channels(set_gap_se(x, NA_real_))$bg)
})

test_that("`grey_non_signif` greys exactly the non-significant gaps", {
  d <- gapb_data()
  x <- gapb_model_col(gapb_tab(d, "grey_non_signif"))
  bg <- fmt_color_channels(x)$bg
  sc <- tabxplor:::fmt_adjustment_score(x)
  pv <- fmt_gap_p(x)
  big <- !is.na(sc) & (sc >= 1.10 | sc <= 1 / 1.10)          # reaches the first adj_ratio break
  testthat::expect_gt(sum(big), 0L)                          # the fixture must bite
  testthat::expect_true(all(bg[big & !is.na(pv) & pv >= 0.05] == 0L))
  testthat::expect_true(all(bg[!is.na(pv) & pv >= 0.05] == 0L))
})

test_that("`guaranteed_effect` colours the CI floor, on the null-direction pole", {
  d  <- gapb_data()
  x  <- gapb_model_col(gapb_tab(d, "guaranteed_effect"))
  xi <- gapb_model_col(gapb_tab(d, "ignore"))
  bg <- fmt_color_channels(x)$bg
  pv <- fmt_gap_p(x)
  sig <- !is.na(pv) & pv < 0.05 & !is.na(tabxplor:::fmt_adjustment_score(x))
  testthat::expect_gt(sum(sig), 0L)
  testthat::expect_true(all(bg[!sig] == 0L))                 # coloured => significant
  # the floor is dimmer than the point estimate, and on the SAME side of the palette
  bi <- fmt_color_channels(xi)$bg
  k  <- which(bg != 0L & bi != 0L)
  testthat::expect_true(all((bg[k] <= 4L) == (bi[k] <= 4L)))
})

# --- legend + tooltip -----------------------------------------------------------------------------

test_that("the legend names the gap's own test, and only caveats a non-collapsible scale", {
  d   <- gapb_data()
  leg <- function(t) tab_color_legend(t, medium = "plain", style = "prose", lang = "en")
  l   <- leg(gapb_tab(d, "grey_non_signif"))
  testthat::expect_true(any(grepl("two estimates fitted on the same sample", l, fixed = TRUE)))
  testthat::expect_true(any(grepl("observed (crude) effect", l, fixed = TRUE)))
  testthat::expect_false(any(grepl("non-collapsibility", l, fixed = TRUE)))
  # the OR path: the caveat fires, and the background clause must NOT claim a greying that never
  # happened (the text channel is greyed by its Wald interval, the background is not gated at all)
  o <- suppressMessages(tab_reg(d, "married", c("race", "party3"), family = "binomial",
                                empirical = TRUE, color = c("OR", "adjustment"),
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

# --- Phase 18z13 (D3): the gap interval follows the table's conf_level -----------------------------

test_that("D3: conf_level reaches the gap interval, not only the printed one", {
  skip_if_not_installed("broom")
  d <- gapb_data()
  w <- function(cl) {
    t <- suppressMessages(tab_reg(d, "married", c("race", "party3"), family = "poisson",
                                  empirical = TRUE, color = c("OR", "adjustment"),
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
