# PURPOSE: the gap between a modelled effect and its observed twin -- its SE, its test and its colour.
# ROLE: the shipped CONTRACT for R/reg-influence.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: gap_se: the influence-function SE of the gap ========================================

gapb_data <- function() {
  d <- fx_reg_df()
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
  # ⚠ the p is asserted on a MULTIVARIABLE table: with one predictor the model IS its own crude twin,
  # so the gap and its SE are both floating-point dust and 22b-xviii's guard yields NA rather than a
  # z of 20. The SE assertion above needs the univariable fixture (that is what the closed form is);
  # the z test needs a gap that exists.
  t2 <- gapb_tab(d, preds = c("race", "party3"))
  x2 <- gapb_model_col(t2)
  g  <- log(tabxplor:::fmt_est_of(x2)) - log(get_obs(x2))
  testthat::expect_equal(fmt_gap_p(x2), 2 * stats::pnorm(-abs(g / get_gap_se(x2))))
  testthat::expect_true(any(!is.na(fmt_gap_p(x2))))
})



# --- the gate: every clause, one fixture each ------------------------------------------------------

test_that("no gap_se where the gap has no honest test", {
  d <- gapb_data()
  none <- function(t) all(is.na(get_gap_se(gapb_model_col(t))))

  # (1) a CONDITIONAL odds ratio -- non-collapsible, ruling Q1(b). Both exponentiate directions.
  testthat::expect_true(none(suppressMessages(tab_reg(
    d, "married", c("race", "party3"), family = "binomial", empirical = TRUE,
    color = c(TRUE, "adjustment")))))
  testthat::expect_true(none(suppressMessages(tab_reg(
    d, "married", c("race", "party3"), family = "binomial", measure = "log",
    empirical = TRUE, color = c(TRUE, "adjustment")))))
  # (2) at the reference profile the model cell is a different estimand (a z5 defect z8-A fixed)
  skip_if_not_installed("marginaleffects")
  testthat::expect_true(none(suppressMessages(tab_reg(
    d, "married", c("race", "party3"), family = "binomial", effect = "at_reference", measure = "difference",
    empirical = TRUE, color = c(TRUE, "adjustment")))))
  # (3) no crude twin at all: multinomial
  m <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE,
                                color = c(TRUE, "adjustment")))
  testthat::expect_true(all(vapply(m[vapply(m, is_fmt, logical(1))],
                                   function(c) all(is.na(get_gap_se(c))), logical(1))))
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




# === SECTION: color = 'adjustment' ================================================================

adj_data <- function() {
  d <- fx_reg_df()
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



# --- the field carries the crude effect, per family -------------------------------------------------
# One claim, seven shapes: get_obs(<model column>) IS the estimate of the Obs_* column beside it. If a
# family ever routed the crude effect through a different field, this catches it.

test_that("obs == the Obs_* effect column, for every family / effect shape", {
  d <- adj_data()
  chk <- function(t, mcol, ocol, getter) {
    testthat::expect_true(all(!is.na(get_obs(t[[mcol]])[-1])))      # -1 = the Constant row
    testthat::expect_equal(get_obs(t[[mcol]]), getter(t[[ocol]]))
  }
  chk(tab_reg(d, outcome = "married", predictors = c("race", "party3"),
              family = "binomial", empirical = TRUE), "Model_OR", "Obs_OR", get_or)
  chk(suppressMessages(tab_reg(d, outcome = "married", predictors = c("race", "party3"),
                               family = "binomial", link = "ratio", empirical = TRUE)), "Model_RR", "Obs_RR",
      tabxplor:::fmt_est_of)
  chk(suppressWarnings(tab_reg(d, outcome = "tvhours", predictors = c("race", "party3"),
                               family = "poisson", empirical = TRUE)),   # tvhours is over-dispersed
      "Model_IRR", "Obs_IRR", get_ratio)
  chk(tab_reg(d, outcome = "age", predictors = c("race", "party3"),
              family = "gaussian", empirical = TRUE), "Model_diff", "Obs_diff", get_diff)
  chk(tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
              measure = "log", empirical = TRUE), "Model_log(OR)", "Obs_log(OR)", get_diff)
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
               family = "binomial", effect = "marginal", measure = "difference", empirical = TRUE)
  chk(t, grep("^Model_", names(t), value = TRUE)[[1]], "Obs_RD", get_diff)
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
               family = "binomial", effect = "marginal", measure = "ratio", empirical = TRUE)
  chk(t, grep("^Model_", names(t), value = TRUE)[[1]], "Obs_RR", tabxplor:::fmt_est_of)
})




# === SECTION: color = 'between_groups' and the interaction line ===================================

gap_data <- function() {
  d <- fx_reg_df()
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



# --- A. gap_se IS sqrt(SE_A^2 + SE_B^2), from the printed intervals --------------------------------

test_that("gap_se equals the quadrature of the two groups' model SEs", {
  d  <- gap_data()
  sp <- gap_tab(d)
  fc <- reg_fmt_cols(sp)
  testthat::expect_length(fc, 3L)

  hand <- function(g) {
    f <- stats::glm(married ~ race, stats::binomial, data = d[d$party3 == g, ])
    summary(f)$coefficients[, "Std. Error"]                 # (Intercept), raceBlack, raceWhite
  }
  se_ref <- hand("Ind")
  # skeleton rows are Constant, then race Other (the reference level, no SE) / Black / White
  for (g in c("Dem", "Rep")) {
    expected <- sqrt(hand(g)^2 + se_ref^2)
    got      <- get_gap_se(sp[[reg_group_col(sp, g)]])
    testthat::expect_equal(got[c(1L, 3L, 4L)], unname(expected), tolerance = 1e-6)
    testthat::expect_true(is.na(got[2L]))                   # the reference level has no interval
  }
  testthat::expect_true(all(is.na(get_gap_se(sp[[reg_group_col(sp, "Ind")]]))))  # a group is not compared to itself
})



# --- B. the aggregated interaction test ------------------------------------------------------------

test_that("the interaction test IS drop1() on the pooled model", {
  d <- gap_data()
  t <- suppressMessages(tab_reg(d, outcome = "married", predictors = c("race", "age"),
                                tab_vars = "party3", family = "binomial",
                                stats = c("n", "group_interaction")))
  it <- get_test(t)
  it <- it[it$test %in% tabxplor:::reg_interaction_types(), , drop = FALSE]
  # Phase 19g: the predictor rides `var`; the split-group level rides a column named after split_var
  testthat::expect_identical(sort(it$var), c("age", "race"))
  testthat::expect_identical(unique(it$test), "group_interact_lr")

  g  <- stats::glm(married ~ (race + age) * party3, stats::binomial, data = d)
  d1 <- stats::drop1(g, scope = c("race:party3", "age:party3"), test = "Chisq")
  testthat::expect_equal(it$pvalue[match(c("race", "age"), it$var)],
                         as.numeric(d1[["Pr(>Chi)"]][match(c("race:party3", "age:party3"),
                                                           rownames(d1))]))
  testthat::expect_equal(it$statistic[match(c("race", "age"), it$var)],
                         as.numeric(d1[["LRT"]][match(c("race:party3", "age:party3"), rownames(d1))]))
})




# === SECTION: the marginal engine against marginaleffects =========================================

skip_if_no_me <- function() testthat::skip_if_not_installed("marginaleffects")



mg_data <- function() {
  d <- fx_reg_df()
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
