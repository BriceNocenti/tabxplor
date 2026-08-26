
# === SECTION: the regression argument boundary ====================================================

skip_on_cran()


rr_data <- function() {
  g <- fx_reg_df()
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


test_that("a comparison key ADDS a row and restricts nothing", {
  d <- rr_data(); M <- list(m1 = "race", m2 = c("race", "age"))
  only <- suppressMessages(tab_reg(d, "married", M, family = "binomial",
                                   stats = "compare_sequential"))
  both <- suppressMessages(tab_reg(d, "married", M, family = "binomial",
                                   stats = c("n", "aic", "compare_sequential")))
  # naming only the comparison keeps the per-family default statistics beside it
  expect_true("mcfadden_r2" %in% get_test(only)$test)
  expect_false("mcfadden_r2" %in% get_test(both)$test)
  expect_true(any(grepl("^compare_seq", get_test(only)$test)))
  expect_true(any(grepl("^compare_seq", get_test(both)$test)))
})


test_that("the scalar logicals are refused when they are not scalar logicals", {
  d <- rr_data()
  expect_error(tab_reg(d, "married", "race", family = "binomial", empirical = "yes"), "TRUE")
  expect_error(tab_reg(d, "married", "race", family = "binomial", n = "yes"), "Unknown")
})


# === S4: the four output arguments, resolved in an order that is not wrong ========================

test_that("the `color = \"adjustment\"` note fires on the DEFAULT color_signif, not only on an explicit one", {
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
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # `color = "adjustment"` FORCES empirical on, and the table's own narrative record must still name
  # the column it built: the header word is a pure function of the resolved estimand, so the two
  # cannot drift whichever order the forcing runs in.
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                effect = "marginal", color = "adjustment", empirical = FALSE))
  mcol <- grep("^Model", names(t), value = TRUE)[[1]]
  expect_identical(paste0("Model_", reg_call(t)$eff_word), mcol)
  expect_identical(reg_call(t)$eff_word, "mRR")   # auto never marginalises an odds ratio
})


# === defect 1: reg_per_dep() is THE per-dependent slicer =========================================
# Three copies of one cascade, and two of them RAISED where the declared slicer defaults. A PARTIAL
# named vector is the documented shape ("unknown dependent -> the default"), not a user error.

test_that("a PARTIAL named `family` defaults the unnamed dependents instead of erroring", {
  skip_if_not_installed("nnet")
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
  skip_if_not_installed("nnet")
  d <- rr_data()
  # length 2 against 3 dependents: the third falls back to "auto" (reg_per_dep's `i <= length(x)`).
  expect_no_error(
    t <- suppressMessages(tab_reg(d, c("married", "party3", "tvhours"), "race",
                                  family = c("binomial", "multinomial"))))
  expect_identical(unname(reg_call(t)$families[["tvhours"]]), "gaussian")
})


test_that("a PARTIAL named `outcome_level` leaves the other outcomes at their default", {
  skip_if_not_installed("nnet")
  d <- rr_data()
  expect_no_error(
    suppressMessages(tab_reg(d, c("married", "party3"), "race",
                             outcome_level = c(married = "Not married"))))
})


# === the `test` tibble's `outcome` key (19m-i's "missing join key"; `dep` until 20c) ============

test_that("every reg footer row states WHICH OUTCOME it is about; every crosstab row states none", {
  d <- rr_data()
  t  <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  tt <- attr(t, "test", exact = TRUE)
  expect_true("outcome" %in% names(tt))
  expect_identical(unique(tt$outcome), "married")

  # a crosstab row is about no outcome -- NA, not "": `var = ""` already means "the whole table"
  ct <- tab(d, marital, race, test = TRUE)
  expect_true("outcome" %in% names(attr(ct, "test", exact = TRUE)))
  expect_true(all(is.na(attr(ct, "test", exact = TRUE)$outcome)))
})


test_that("`outcome` is DECLARED in the schema, so it is not read as a grouping variable", {
  # test_group_cols() is `setdiff(names(tt), names(new_test_tibble()))` minus dot-prefixed names, so
  # an undeclared column would split the reg footer into one block per outcome (19g's own defect).
  expect_true("outcome" %in% names(new_test_tibble()))
  t <- suppressMessages(tab_reg(rr_data(), "married", c("race", "age"), family = "binomial"))
  expect_length(test_group_cols(attr(t, "test", exact = TRUE)), 0L)
})


test_that("a multi-outcome footer heads its columns by outcome; a model COMPARISON does not", {
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


test_that("reg_color_auto_measure() reads the estimand's stored SCALE, not its arguments", {
  or  <- reg_estimand("binomial", measure = "auto", effect = "conditional")             # odds_ratio scale
  lg  <- reg_estimand("binomial", measure = "log", effect = "conditional")              # log_odds scale
  expect_true(nzchar(reg_color_auto_measure(or)))
  expect_true(nzchar(reg_color_auto_measure(lg)))
  # a ratio geometry and an additive one do not answer the same context
  bt  <- reg_estimand("gaussian", measure = "auto", effect = "conditional")             # raw_diff scale
  expect_false(identical(reg_color_auto_measure(or), reg_color_auto_measure(bt)))
})



# === SECTION: multiplier, shape and ref: one grammar ==============================================

anc_data <- function() {
  d <- fx_reg_df()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  tibble::as_tibble(d[!is.na(d$age) & !is.na(d$tvhours), , drop = FALSE])
}


cst <- function(t, col) {
  x <- t[[col]]
  get_num(x[as.character(t$var) == "Constant"])
}


test_that('the default anchor is the WEIGHTED mean of the predictors\' complete cases', {
  d  <- anc_data()
  d$w <- runif(nrow(d), 0.5, 2)
  t1 <- tab_reg(d, "married", c("race", "age"), family = "binomial", wt = "w", stats = FALSE,
                multiplier = 1)
  a  <- unname(reg_call(t1)$fit_spec$prep$anchors[["age"]])
  fr <- d[!is.na(d$race) & !is.na(d$age), ]
  expect_equal(a, sum(fr$w * fr$age) / sum(fr$w), tolerance = 1e-10)
  expect_false(isTRUE(all.equal(a, mean(fr$age))))           # NOT the unweighted one (defect A2-1)
})


test_that("`shape` recodes first, the anchor applies to the result", {
  d  <- anc_data()
  t1 <- tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE,
                shape = c(age = "log"), ref = c(age = "mean"))
  fr <- d[!is.na(d$race) & !is.na(d$age), ]
  expect_equal(unname(reg_call(t1)$fit_spec$prep$anchors[["age"]]), mean(log(fr$age)), tolerance = 1e-10)
  # a bare NUMBER on a transformed column is refused rather than silently subtracted from log(x)
  expect_error(tab_reg(d, "married", c("race", "age"), family = "binomial",
                       shape = c(age = "log"), ref = c(age = 30)), "log")
  # a quantile shape makes it a FACTOR, which is never shifted
  t2 <- suppressWarnings(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 stats = FALSE, shape = c(age = "quartiles")))
  expect_false("age" %in% names(reg_call(t2)$fit_spec$prep$anchors))
})


# ---- the Constant row ----------------------------------------------------------------------------

test_that("the Constant row holds the baseline THIS contrast is read against", {
  skip_if_not_installed("marginaleffects")
  d  <- anc_data()
  p  <- c("race", "age")
  tc <- tab_reg(d, "married", p, family = "binomial", stats = FALSE)
  ta <- tab_reg(d, "married", p, family = "binomial", effect = "at_reference", measure = "difference", stats = FALSE)
  tm <- tab_reg(d, "married", p, family = "binomial", effect = "marginal", measure = "difference", stats = FALSE)

  # the anchored intercept IS the prediction at the reference profile (the study's section 3.2)
  odds <- cst(tc, "Model_OR")
  expect_equal(odds / (1 + odds), cst(ta, "Model_refRD"), tolerance = 1e-8)

  # the marginal row is the model's AVERAGE prediction, and it is not the at-profile one
  fr <- tidyr::drop_na(d, tidyselect::all_of(c("married", p)))
  # tab_reg models the outcome's FIRST level unless `outcome_level` says otherwise
  g  <- stats::glm(as.integer(married == "no") ~ race + age, data = fr, family = stats::binomial())
  expect_equal(cst(tm, "Model_mRD"), mean(stats::fitted(g)), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(cst(tm, "Model_mRD"), cst(ta, "Model_refRD"))))

  # a baseline is not an effect: it carries its interval, and no star
  expect_true(is.finite(get_ci_inf(tm[["Model_mRD"]])[1]))
  expect_true(is.na(get_pvalue(tm[["Model_mRD"]])[1]))
  expect_true(is.finite(get_pvalue(tc[["Model_OR"]])[1]))     # the tested intercept keeps its own
  expect_no_match(paste(get_subtext(tm), collapse = " "), "Constant")
})


test_that("the Constant row is written on the column's own geometry, and labelled by contrast", {
  skip_if_not_installed("marginaleffects")
  d <- anc_data()
  lab <- function(t) as.character(t$levels)[as.character(t$var) == "Constant"]

  tm <- tab_reg(d, "married", c("race", "age"), family = "binomial", effect = "marginal",
                measure = "ratio", stats = FALSE)
  expect_identical(get_scale(tm[["Model_mRR"]]), "pct_ratio")
  expect_true(cst(tm, "Model_mRR") > 0 && cst(tm, "Model_mRR") < 1)   # a baseline RISK, not an odds
  expect_identical(lab(tm), "Population average")

  tp <- suppressWarnings(tab_reg(d, "tvhours", c("race", "age"), family = "poisson",
                                 effect = "at_reference", measure = "ratio", stats = FALSE))
  expect_identical(get_scale(tp[["Model_refIRR"]]), "mean_ratio")
  expect_true(cst(tp, "Model_refIRR") > 1)                            # a baseline RATE, not a ratio
  expect_identical(lab(tp), "Reference profile")

  ta <- tab_reg(d, "married", c("race", "age"), family = "binomial", effect = "at_reference", measure = "difference",
                stats = FALSE)
  expect_identical(lab(ta), "Reference profile")
  expect_identical(lab(tab_reg(d, "married", c("race", "age"), family = "binomial",
                               stats = FALSE)), "Reference profile")
})


# ---- the two descriptive readers ------------------------------------------------------------------

test_that("a shifted column is read back in the user's own units", {
  d  <- anc_data()
  t1 <- tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE,
                empirical = TRUE)
  tips <- paste(get_empirical_tips(t1)$tip, collapse = " ")
  fr   <- d[!is.na(d$race) & !is.na(d$age), ]
  expect_true(grepl(format(signif(mean(fr$age), 3)), tips, fixed = TRUE))
  expect_false(grepl("age: mean 0 ", tips, fixed = TRUE))
})


test_that("reg_check_plots() replays the preparation, so the refit IS the table's model", {
  skip_if_not_installed("ggplot2")
  d  <- anc_data()
  t1 <- suppressWarnings(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 shape = c(age = "log"), multiplier = 1))
  cx <- tabxplor:::reg_plot_fits(t1, d)
  expect_length(cx, 1L)
  or <- get_or(t1[["Model_OR"]])
  expect_equal(sort(unname(stats::coef(cx[[1]]$fit))),
               sort(log(or[!is.na(get_pvalue(t1[["Model_OR"]]))])), tolerance = 1e-8)
})
