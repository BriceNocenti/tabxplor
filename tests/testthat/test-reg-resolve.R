# Phase 19m-ii: tab_reg()'s ARGUMENT BOUNDARY -- reg_resolve_args() and the defects the extraction
# exposed. Every test here fails on the pre-19m-ii tree.
#
# ROLE: this is the boundary's own file. Statistical soundness stays in test-tab_reg.R; what is
# asserted here is what was RESOLVED and what the user was TOLD. The wide characterization sweep is
# dev/verify_reg_specs.R (291 cases, save/check) -- these are the cases that must never regress.

skip_on_cran()

rr_data <- function() {
  g <- forcats::gss_cat
  g <- g[seq(1L, nrow(g), by = 6L), , drop = FALSE]
  g$married <- factor(ifelse(g$marital == "Married", "Married", "Not married"))
  g$party3  <- forcats::fct_lump_n(g$partyid, 2)
  g$score   <- pmin(g$tvhours, 6L)
  as.data.frame(g)
}

# every cli_inform this call emits, as plain strings (the notes are what several tests here are about)
capture_msg <- function(expr) {
  out <- character()
  withCallingHandlers(force(expr),
                      message = function(m) { out <<- c(out, conditionMessage(m))
                                              invokeRestart("muffleMessage") })
  out
}

# === S1 reg_validate_args(): the four arguments the reg boundary never checked ====================

test_that("`conf_level` is validated, with the 95-vs-0.95 hint", {
  skip_if_not_installed("broom")
  d <- rr_data()
  # it reached the interval engine as a probability: `qnorm(1 - (1 - 95)/2)` -> NaN, a warning, and
  # a table full of NaN bounds.
  expect_error(tab_reg(d, "married", "race", family = "binomial", conf_level = 95),
               "0\\.95")
  expect_error(tab_reg(d, "married", "race", family = "binomial", conf_level = -1),
               "probability")
  expect_error(tab_reg(d, "married", "race", family = "binomial", conf_level = c(0.9, 0.95)),
               "single probability")
})

test_that("`stats` names are validated instead of silently filtered", {
  skip_if_not_installed("broom")
  d <- rr_data()
  # reg_footer_stats() did `stats[stats %in% reg_stat_keys()]`, so a typo produced a MISSING footer
  # row and no message. reg_validate_stat_keys() has carried `arg = "stats"` since 19g, uncalled.
  expect_error(tab_reg(d, "married", "race", family = "binomial", stats = c("n", "typo")),
               "Unknown")
  # the declared special values still pass through untouched
  for (s in list(NULL, TRUE, FALSE, "all", "none", c("n", "aic")))
    expect_no_error(suppressMessages(
      tab_reg(d, "married", "race", family = "binomial", stats = s)))
})

test_that("`color_signif` is validated on the reg path too", {
  skip_if_not_installed("broom")
  d <- rr_data()
  # it went straight to fmt(), which CASTS without validating -- so the unknown policy was stored on
  # every column and merely painted as "ignore".
  expect_error(tab_reg(d, "married", "race", family = "binomial", color_signif = "grey"),
               "Unknown .*color_signif")
  for (s in COLOR_SIGNIF_VALUES)
    expect_no_error(suppressMessages(
      tab_reg(d, "married", "race", family = "binomial", color_signif = s)))
})

test_that("`baseline` is shape-checked, and says so when `compare` cannot use it", {
  skip_if_not_installed("broom")
  d <- rr_data(); M <- list(m1 = "race", m2 = c("race", "age"))
  expect_error(tab_reg(d, "married", M, family = "binomial", baseline = c("m1", "m2")),
               "single model name")
  # a bogus baseline under compare = "none" used to be dropped in silence
  expect_message(tab_reg(d, "married", M, family = "binomial", baseline = "m1"),
                 "compare")
})

test_that("the scalar logicals are refused when they are not scalar logicals", {
  skip_if_not_installed("broom")
  d <- rr_data()
  expect_error(tab_reg(d, "married", "race", family = "binomial", empirical = "yes"), "TRUE")
  expect_error(tab_reg(d, "married", "race", family = "binomial", add_n = c(TRUE, FALSE)), "single")
})

# === S2: the split_var refusals now precede the colour/family informs (H23) =======================

test_that("a `split_var` that is also a predictor aborts before anything is announced", {
  skip_if_not_installed("broom")
  d <- rr_data()
  # the same abort fired ~500 lines later, so it arrived after up to eight informs about families,
  # colours and forcings the call was never going to produce.
  expect_silent(expect_error(
    tab_reg(d, "married", c("race", "age"), family = "binomial",
            split_var = "race", color = "adjustment"),
    "cannot also be the outcome or a predictor"))
})

# === S4: the four output arguments, resolved in an order that is not wrong ========================

test_that("the `color = \"adjustment\"` note fires on the DEFAULT color_signif, not only on an explicit one", {
  skip_if_not_installed("broom")
  d <- rr_data()
  # reg_color_notes() tested `!is.null(color_signif)` while the default "grey_non_signif" was applied
  # 22 lines LATER -- so the identical effective state was silent one way and noisy the other.
  msg <- function(...) paste(capture_msg(tab_reg(d, "married", c("race", "age"),
                                                 family = "binomial", color = "adjustment", ...)),
                             collapse = " ")
  expect_match(msg(),                               "non-collapsibility")
  expect_match(msg(color_signif = "grey_non_signif"), "non-collapsibility")
  # and it stays silent where the policy genuinely does not apply
  expect_no_match(msg(color_signif = "ignore"), "non-collapsibility")
})

test_that("`empirical` is FINAL before the effect word is recorded (H22)", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # `color = "adjustment"` FORCES empirical on. The eager `eff_word` was computed BEFORE that
  # forcing and stored in reg_call, while the specs and the column header were computed after --
  # so the table's own narrative record contradicted its own column ("AME" vs "AME (adjusted %)").
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                effect = "marginal", color = "adjustment", empirical = FALSE))
  mcol <- grep("^Model", names(t), value = TRUE)[[1]]
  expect_identical(paste0("Model_", reg_call(t)$eff_word), mcol)
  expect_match(reg_call(t)$eff_word, "adjusted %", fixed = TRUE)
})

# === defect 1: reg_per_dep() is THE per-dependent slicer =========================================
# Three copies of one cascade, and two of them RAISED where the declared slicer defaults. A PARTIAL
# named vector is the documented shape ("unknown dependent -> the default"), not a user error.

test_that("a PARTIAL named `family` defaults the unnamed dependents instead of erroring", {
  skip_if_not_installed("broom"); skip_if_not_installed("nnet")
  d <- rr_data()
  # `party3` is not named -> "auto" -> detected. Before: `family[["party3"]]` = subscript out of bounds.
  expect_no_error(
    t <- suppressMessages(tab_reg(d, c("married", "party3"), c("race", "age"),
                                  family = c(married = "binomial"))))
  fam <- reg_call(t)$families
  expect_identical(unname(fam[["married"]]), "binomial")
  expect_identical(unname(fam[["party3"]]),  "multinomial")
})

test_that("a SHORTER positional `family` defaults the surplus dependents instead of erroring", {
  skip_if_not_installed("broom"); skip_if_not_installed("nnet")
  d <- rr_data()
  # length 2 against 3 dependents: the third falls back to "auto" (reg_per_dep's `i <= length(x)`).
  expect_no_error(
    t <- suppressMessages(tab_reg(d, c("married", "party3", "tvhours"), "race",
                                  family = c("binomial", "multinomial"))))
  expect_identical(unname(reg_call(t)$families[["tvhours"]]), "gaussian")
})

test_that("a PARTIAL named `inverse_two_level_factors` defaults to TRUE instead of erroring", {
  skip_if_not_installed("broom"); skip_if_not_installed("nnet")
  d <- rr_data()
  expect_no_error(
    suppressMessages(tab_reg(d, c("married", "party3"), "race",
                             inverse_two_level_factors = c(married = FALSE))))
})

test_that("a POSITIONAL `inverse_two_level_factors` is read positionally, not by name", {
  skip_if_not_installed("broom")
  d <- rr_data()
  # `c(TRUE, FALSE)[["married"]]` was a subscript error: the length>1 branch assumed names.
  expect_no_error(
    a <- suppressMessages(tab_reg(d, c("married", "score"), "race",
                                  family = c("binomial", "gaussian"),
                                  inverse_two_level_factors = c(FALSE, TRUE))))
  b <- suppressMessages(tab_reg(d, c("married", "score"), "race",
                                family = c("binomial", "gaussian"),
                                inverse_two_level_factors = c(married = FALSE)))
  # both spell "married models the OTHER level" (positive_level is aligned to `dependent`, unnamed);
  # the positional one no longer errors on the way
  expect_identical(reg_call(a)$positive_level[[1]], reg_call(b)$positive_level[[1]])
})

# === defect 8: a formula `dependent` is not a vector of three dependents ==========================

test_that("a formula `dependent` beside `predictors` gives the teachable message, not a stopifnot", {
  skip_if_not_installed("broom")
  d <- rr_data()
  # `length(y ~ x)` is 3 (`~`, lhs, rhs), so this used to enter the multi-dependent recursion and
  # die on the internal `stopifnot(is.character(dependent))`.
  expect_error(tab_reg(d, married ~ race, list(m1 = "race")),
               "formula.*or.*predictors|predictors.*not both")
  expect_error(tab_reg(d, married ~ race, c("race", "age")),
               "formula.*or.*predictors|predictors.*not both")
})

# === the `test` tibble's `dep` key (19m-i's "missing join key") ==================================

test_that("every reg footer row states WHICH DEPENDENT it is about; every crosstab row states none", {
  skip_if_not_installed("broom")
  d <- rr_data()
  t  <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  tt <- attr(t, "test", exact = TRUE)
  expect_true("dep" %in% names(tt))
  expect_identical(unique(tt$dep), "married")

  # a crosstab row is about no dependent -- NA, not "": `var = ""` already means "the whole table"
  ct <- tab(d, marital, race, test = TRUE)
  expect_true("dep" %in% names(attr(ct, "test", exact = TRUE)))
  expect_true(all(is.na(attr(ct, "test", exact = TRUE)$dep)))
})

test_that("`dep` is DECLARED in the schema, so it is not read as a grouping variable", {
  # test_group_cols() is `setdiff(names(tt), names(new_test_tibble()))` minus dot-prefixed names, so
  # an undeclared column would split the reg footer into one block per outcome (19g's own defect).
  expect_true("dep" %in% names(new_test_tibble()))
  skip_if_not_installed("broom")
  t <- suppressMessages(tab_reg(rr_data(), "married", c("race", "age"), family = "binomial"))
  expect_length(test_group_cols(attr(t, "test", exact = TRUE)), 0L)
})

test_that("a multi-outcome footer heads its columns by outcome; a model COMPARISON does not", {
  skip_if_not_installed("broom")
  d <- rr_data()
  # one model per outcome: the dependent IDENTIFIES the column, so it is the header
  t1 <- suppressMessages(tab_reg(d, c("married", "tvhours"), "race",
                                 family = c("binomial", "gaussian")))
  g1 <- test_grid_reg(t1, attr(t1, "test", exact = TRUE))
  expect_true(all(c("married", "tvhours") %in% unlist(g1)))
  # a comparison: every column has the SAME outcome, so the model label is the header
  t2 <- suppressMessages(tab_reg(d, "married", list(m1 = "race", m2 = c("race", "age")),
                                 family = "binomial"))
  g2 <- test_grid_reg(t2, attr(t2, "test", exact = TRUE))
  expect_false("married" %in% unlist(g2))
})

# === the four pure helpers ========================================================================
# They were closures over tab_reg()'s mutating frame; as functions of their arguments they are
# testable on their own, which is the point.

test_that("reg_trials_observed_max() answers only where a trial count exists", {
  expect_equal(reg_trials_observed_max(c(0L, 3L, 6L)), 6)   # max() keeps the input's type
  expect_true(is.na(reg_trials_observed_max(factor(c("a", "b")))))   # a factor is a plain logit
  expect_true(is.na(reg_trials_observed_max(c(0L, 1L))))             # 0/1 has no trial count
  expect_true(is.na(reg_trials_observed_max(c("a", "b"))))
  expect_true(is.na(reg_trials_observed_max(c(NA_real_, NA_real_)))) # all-NA: no finite max
})

test_that("reg_eff_word() takes `empirical` as an argument, so it cannot be read at the wrong time", {
  e <- reg_estimand("binomial", "marginal", "difference")
  expect_true(nzchar(reg_eff_word(e, FALSE)))
  expect_identical(reg_eff_word(e, TRUE), paste0(reg_eff_word(e, FALSE), " (adjusted %)"))
  # a coefficient estimand never takes the parenthetical, whatever `empirical` says
  cf <- reg_estimand("binomial", "coefficient", "auto")
  expect_identical(reg_eff_word(cf, TRUE), reg_eff_word(cf, FALSE))
})

test_that("reg_color_for() fills only the auto slots, and is idempotent", {
  e  <- reg_estimand("binomial", "coefficient", "auto")
  # the bare-TRUE sentinel
  one <- reg_color_for(reg_normalize_color(TRUE), e)
  expect_false(any(is.na(one)))
  expect_identical(reg_color_for(one, e), one)                       # idempotent: no NA left to fill
  # an explicit measure keeps its own slot; only the auto one follows the column
  two <- reg_color_for(reg_normalize_color(c(TRUE, "adjustment")), e)
  expect_identical(two[[2]], "adjustment")
  expect_identical(two[[1]], one[[1]])
  # nothing auto -> unchanged
  expect_identical(reg_color_for("adjustment", e), "adjustment")
})

test_that("reg_color_auto_measure() reads the estimand's stored SCALE, not its arguments", {
  or  <- reg_estimand("binomial", "coefficient", "auto")             # odds_ratio scale
  lg  <- reg_estimand("binomial", "coefficient", "log")              # log_odds scale
  expect_true(nzchar(reg_color_auto_measure(or)))
  expect_true(nzchar(reg_color_auto_measure(lg)))
  # a ratio geometry and an additive one do not answer the same context
  bt  <- reg_estimand("gaussian", "coefficient", "auto")             # raw_diff scale
  expect_false(identical(reg_color_auto_measure(or), reg_color_auto_measure(bt)))
})
