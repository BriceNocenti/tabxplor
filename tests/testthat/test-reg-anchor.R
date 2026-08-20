# Phase 22b-viii: the shared per-predictor grammar, and `ref` as a continuous predictor's ANCHOR.
#
# The two claims everything else rests on: one grammar in `multiplier` / `shape` / `ref`, and an
# anchor that is a REPARAMETRIZATION -- every estimate is invariant, only the intercept moves.

anc_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  tibble::as_tibble(d[!is.na(d$age) & !is.na(d$tvhours), , drop = FALSE])
}

cst <- function(t, col) {
  x <- t[[col]]
  get_num(x[as.character(t$var) == "Constant"])
}

# ---- the shared grammar -------------------------------------------------------------------------

test_that("one grammar: a bare scalar, `default =`, and per-variable overrides", {
  skip_if_not_installed("broom")
  d <- anc_data()

  # multiplier: an unnamed value is the fallback -- the 1.x form discarded it as soon as a name appeared
  m <- tab_reg(d, "married", c("race", "age", "tvhours"), family = "binomial", stats = FALSE,
               multiplier = c("2sd", age = 10))
  k <- reg_call(m)$multiplier
  expect_equal(unname(k[["age"]]), 10)
  expect_equal(unname(k[["tvhours"]]),
               2 * tabxplor:::reg_predictor_sd(d$tvhours[!is.na(d$race)]), tolerance = 1e-8)
  expect_equal(k, reg_call(tab_reg(d, "married", c("race", "age", "tvhours"), family = "binomial",
                                   stats = FALSE,
                                   multiplier = c(default = "2sd", age = 10)))$multiplier)

  # shape: a bare scalar cuts EVERY continuous predictor -- inexpressible before this phase
  s <- suppressWarnings(tab_reg(d, "married", c("race", "age", "tvhours"), family = "binomial",
                                stats = FALSE, shape = "quartiles"))
  expect_true(all(c("age", "tvhours") %in% as.character(s$var)))
  expect_equal(sum(as.character(s$var) == "age"), 4L)      # four quantile groups, one row each

  # ref: the value names the kind it applies to, so both defaults fit in one unnamed pair
  r  <- tab_reg(d, "married", c("race", "relig", "age"), family = "binomial", stats = FALSE,
                ref = c("median", "last", race = "Black"))
  fr <- d[!is.na(d$race) & !is.na(d$relig) & !is.na(d$age), ]
  expect_equal(unname(reg_call(r)$fit_spec$prep$anchors[["age"]]),
               tabxplor:::rd_wquantile(fr$age, 0.5), tolerance = 1e-8)
  expect_identical(as.character(r$levels)[as.character(r$var) == "race"][[1]], "Black")
  lv <- as.character(r$levels)[as.character(r$var) == "relig"]
  expect_identical(lv[[1]], utils::tail(levels(forcats::fct_drop(d$relig)), 1))  # `last`, every OTHER factor
})

test_that("the grammar's refusals name the eligible set and the two vocabularies", {
  skip_if_not_installed("broom")
  d <- anc_data()
  f <- function(...) tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE, ...)
  expect_error(f(ref = c(nope = "mean")), "predictor")
  expect_error(f(ref = "banana"), "default")                 # matches neither vocabulary
  expect_error(f(ref = c("mean", "median")), "same kind")    # two defaults for one kind
  expect_error(f(multiplier = c("sd", "2sd")), "same kind")
  expect_error(f(ref = c(age = "quartile")), "must be a number")
  expect_error(f(shape = c(race = "quadratic")), "continuous")
})

# ---- the anchor is a reparametrization -----------------------------------------------------------

test_that("only the intercept moves: every estimate is invariant under the anchor", {
  skip_if_not_installed("broom")
  d  <- anc_data()
  t0 <- tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE,
                multiplier = 1, ref = c(age = 0))
  for (a in list(NULL, c(age = "median"), c(age = 40))) {
    t1 <- do.call(tab_reg, c(list(d, "married", c("race", "age"), family = "binomial",
                                  stats = FALSE, multiplier = 1), if (!is.null(a)) list(ref = a)))
    keep <- as.character(t1$var) != "Constant"
    for (g in list(get_or, get_ci_inf, get_ci_sup, get_pvalue))
      expect_equal(g(t1[["Model_OR"]])[keep], g(t0[["Model_OR"]])[keep], tolerance = 1e-9)
  }
  # and the intercept really does move, in the direction the anchor says
  expect_false(isTRUE(all.equal(cst(t0, "Model_OR"),
                                cst(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                            stats = FALSE, multiplier = 1), "Model_OR"))))
})

test_that('the default anchor is the WEIGHTED mean of the predictors\' complete cases', {
  skip_if_not_installed("broom")
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
  skip_if_not_installed("broom")
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
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d  <- anc_data()
  p  <- c("race", "age")
  tc <- tab_reg(d, "married", p, family = "binomial", stats = FALSE)
  ta <- tab_reg(d, "married", p, family = "binomial", effect = "at_reference", stats = FALSE)
  tm <- tab_reg(d, "married", p, family = "binomial", effect = "marginal", stats = FALSE)

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
  skip_if_not_installed("broom")
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

  ta <- tab_reg(d, "married", c("race", "age"), family = "binomial", effect = "at_reference",
                stats = FALSE)
  expect_identical(lab(ta), "Reference profile")
  expect_identical(lab(tab_reg(d, "married", c("race", "age"), family = "binomial",
                               stats = FALSE)), "Reference profile")
})

# ---- the two descriptive readers ------------------------------------------------------------------

test_that("a shifted column is read back in the user's own units", {
  skip_if_not_installed("broom")
  d  <- anc_data()
  t1 <- tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE,
                empirical = TRUE)
  tips <- paste(get_empirical_tips(t1)$tip, collapse = " ")
  fr   <- d[!is.na(d$race) & !is.na(d$age), ]
  expect_true(grepl(format(signif(mean(fr$age), 3)), tips, fixed = TRUE))
  expect_false(grepl("age: mean 0 ", tips, fixed = TRUE))
})

test_that("reg_check_plots() replays the preparation, so the refit IS the table's model", {
  skip_if_not_installed("broom")
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
