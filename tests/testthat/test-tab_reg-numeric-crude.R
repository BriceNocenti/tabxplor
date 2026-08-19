# Phase 18z9: crude (`Obs_*`) counterparts for NUMERIC predictors.
#
# The governing claim is the same one the factor arm rests on: the crude effect IS the univariable
# model's effect. For a factor that model is saturated (hence tabxplor's closed-form cell sums); for a
# numeric there is no closed form, so the crude column is the univariable fit -- on the model's own
# complete-case population, family, design, CI rule and `multiplier`.
#
# Also here: the predictor-kind unification (`reg_is_factor_var`) and the two incidental defects the
# audit found (dev/numeric_predictors_crude_counterparts.md SS11).

# NOTE: `multiplier = 1` in every PARITY fixture below. Since Phase 18z9 the default is "sd", so a
# numeric row is per-1-SD; these tests assert equality with a hand-fitted coef(), which is per 1 unit --
# the scaling is the subject of its own section (5) instead.

num_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d <- d[!is.na(d$tvhours) & !is.na(d$age) & !is.na(d$race), , drop = FALSE]
  tibble::as_tibble(d)
}

# the univariable fit tabxplor's crude column must reproduce, on the MODEL's complete-case frame
crude_glm <- function(d, dep, v, preds, family = stats::binomial()) {
  dm <- tidyr::drop_na(d, tidyselect::all_of(unique(c(dep, preds))))
  stats::glm(stats::as.formula(paste0("`", dep, "` ~ `", v, "`")), data = dm, family = family)
}


# --- 1. the predictor-kind predicate ---------------------------------------------------------------

test_that("reg_is_factor_var(): logical is a factor, Date/numeric are not", {
  expect_true (reg_is_factor_var(factor("a")))
  expect_true (reg_is_factor_var("a"))
  expect_true (reg_is_factor_var(c(TRUE, FALSE)))
  expect_false(reg_is_factor_var(1:3))
  expect_false(reg_is_factor_var(Sys.Date() + 1:3))
})

test_that("a LOGICAL predictor produces a real (non-NA) model row", {
  # Before z9 a logical took reg_skeleton()'s NUMERIC arm (term = "x") while glm names its
  # coefficient "xTRUE" -> no tidy match -> a silently all-NA row.
  d <- num_data()
  d$old <- d$age >= 50
  t  <- tab_reg(d, "married", c("old", "race"), family = "binomial", cleannames = FALSE)
  is_old <- as.character(t$var) == "old"
  expect_equal(sum(is_old), 2L)                      # FALSE / TRUE, not one blank numeric row
  or <- get_or(t[["Model_OR"]])[is_old]
  expect_true(any(!is.na(or)))                       # the row carries an estimate

  # same 0/1 recode tab_reg used (inverse_two_level_factors picks the modelled level)
  dm <- d
  dm$y <- as.integer(dm$married == reg_call(t)$positive_level)
  ref  <- stats::glm(y ~ old + race, data = dm, family = stats::binomial())
  expect_equal(or[!is.na(or) & or != 1],
               unname(exp(stats::coef(ref)["oldTRUE"])), tolerance = 1e-8)
})

test_that("reg_meta stores the predictor-kind map", {
  d  <- num_data()
  t  <- tab_reg(d, "married", c("age", "race"), family = "binomial", cleannames = FALSE)
  pt <- reg_call(t)$predictor_types
  expect_identical(pt[["age"]],  "numeric")
  expect_identical(pt[["race"]], "factor")
})


# --- 2. the incidental defects (SS11) --------------------------------------------------------------

test_that("the Constant row keeps its bold under empirical = TRUE", {
  # tab_bold_rows() ANDs `anchor` across every discriminating column; emp_col() used to exclude the
  # Constant, so the shared bold dropped it.
  d <- num_data()
  t <- tab_reg(d, "married", "race", family = "binomial", empirical = TRUE, cleannames = FALSE)
  rd <- tab_export_prep(t)
  const_row <- which(as.character(t$var) == "Constant")
  expect_true(all(const_row %in% rd$tables[[1]]$bold_rows))
  # the mechanism: every column must flag the Constant as an anchor, crude columns included.
  # Phase 19h: the anchor signal is `keep_black` -- the shipped `ann$anchor` slot was a duplicate of
  # it that no backend read (and the transpose silently dropped), so it is a prep-internal local now.
  # On a Constant row the two are the same value: the footer override only touches GOF footer rows.
  expect_true(all(purrr::map_lgl(rd$tables[[1]]$ann, ~ .x$keep_black[const_row])))
})

test_that("get_num()/set_num() handle the 'OR_pct' spelling like format() does", {
  x <- fmt(n = 10L, or = 2.5, pct = 0.4, display = "OR_pct", digits = 2L, scale = "level_pct", pct_type = "row")
  expect_equal(get_num(x), 2.5)                       # was falling through to the raw count (10)
  y <- set_num(x, 3)
  expect_equal(get_or(y), 3)
})

test_that("set_num() writes back an 'OR'-displayed column (mask parity)", {
  x <- fmt(n = 10L, or = 2.5, display = "OR", digits = 2L, scale = "level_pct", pct_type = "row")
  expect_equal(get_or(set_num(x, 4)), 4)
})


# --- 3. the crude effect column IS the univariable fit ---------------------------------------------

test_that("binomial: Obs_OR for a numeric == exp(coef(glm(y ~ x))) on the MODEL's population", {
  d <- num_data()
  t <- tab_reg(d, "married", c("age", "tvhours", "race"), family = "binomial",
               empirical = TRUE, multiplier = 1, cleannames = FALSE)
  dm <- tidyr::drop_na(d, "married", "age", "tvhours", "race")
  dm$y <- as.integer(dm$married == reg_call(t)$positive_level)

  for (v in c("age", "tvhours")) {
    i <- which(as.character(t$var) == v)
    g <- stats::glm(stats::as.formula(paste("y ~", v)), data = dm, family = stats::binomial())
    ci <- stats::confint.default(g, level = 0.95)[v, ]
    expect_equal(get_or(t[["Obs_OR"]])[i],     unname(exp(stats::coef(g)[v])), tolerance = 1e-10)
    expect_equal(get_ci_inf(t[["Obs_OR"]])[i], unname(exp(ci[[1]])),           tolerance = 1e-10)
    expect_equal(get_ci_sup(t[["Obs_OR"]])[i], unname(exp(ci[[2]])),           tolerance = 1e-10)
    expect_equal(get_pvalue(t[["Obs_OR"]])[i],
                 unname(summary(g)$coefficients[v, 4]),                        tolerance = 1e-10)
  }
})

test_that("the crude fit uses the MODEL's complete-case population, not its own", {
  # `drop_extra`: a univariable fit would otherwise drop on fewer variables and land on ~2x the sample.
  d <- num_data()
  d$tvhours[1:2000] <- NA                              # make the populations differ sharply
  t  <- tab_reg(d, "married", c("age", "tvhours"), family = "binomial",
                empirical = TRUE, multiplier = 1, cleannames = FALSE)
  dm <- tidyr::drop_na(d, "married", "age", "tvhours")
  dm$y  <- as.integer(dm$married == reg_call(t)$positive_level)
  gsmall <- stats::glm(y ~ age, data = dm, family = stats::binomial())          # model population
  gbig   <- stats::glm(y ~ age, data = transform(tidyr::drop_na(d, "married", "age"),
                                                 y = as.integer(married == reg_call(t)$positive_level)),
                       family = stats::binomial())                              # the wrong one
  i <- which(as.character(t$var) == "age")
  expect_equal(get_or(t[["Obs_OR"]])[i], unname(exp(stats::coef(gsmall)["age"])), tolerance = 1e-10)
  expect_false(isTRUE(all.equal(unname(exp(stats::coef(gsmall)["age"])),
                                unname(exp(stats::coef(gbig)["age"])), tolerance = 1e-6)))
})

test_that("a numeric predictor's crude cell carries the effect and NO level", {
  d <- num_data()
  t <- tab_reg(d, "married", c("age", "race"), family = "binomial",
               empirical = TRUE, cleannames = FALSE)
  i <- which(as.character(t$var) == "age")
  # a continuous predictor has no levels, so no share to print beside its effect -- `{base}` renders
  # void there and the cell keeps its estimate alone.
  expect_true(is.na(get_pct(t[["Obs_OR"]])[i]))
  expect_false(is.na(get_or(t[["Obs_OR"]])[i]))
})

test_that("gaussian / poisson / rr numeric crude effects match their univariable fits", {
  d <- num_data()
  dm <- tidyr::drop_na(d, "tvhours", "age", "race")

  tg <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian",
                empirical = TRUE, multiplier = 1, cleannames = FALSE)
  ig <- which(as.character(tg$var) == "age")
  expect_equal(get_diff(tg[["Obs_diff"]])[ig],
               unname(stats::coef(stats::lm(tvhours ~ age, data = dm))["age"]), tolerance = 1e-10)

  tp <- suppressWarnings(tab_reg(d, "tvhours", c("age", "race"), family = "poisson",
                                 empirical = TRUE, multiplier = 1, cleannames = FALSE))
  ip <- which(as.character(tp$var) == "age")
  gp <- stats::glm(tvhours ~ age, data = dm, family = stats::quasipoisson())
  expect_equal(get_ratio(tp[["Obs_IRR"]])[ip], unname(exp(stats::coef(gp)["age"])), tolerance = 1e-10)

  tr <- tab_reg(d, "married", c("age", "race"), family = "poisson",   # binary -> modified Poisson
                empirical = TRUE, cleannames = FALSE)
  ir <- which(as.character(tr$var) == "age")
  expect_true(!is.na(tabxplor:::fmt_est_of(tr[["Obs_RR"]])[ir]))
  expect_true(tabxplor:::fmt_est_of(tr[["Obs_RR"]])[ir] != 1)
})

test_that("measure = log gives the LOGGED crude effect for a numeric row", {
  d <- num_data()
  t <- tab_reg(d, "married", c("age", "race"), family = "binomial", measure = "log",
               empirical = TRUE, multiplier = 1, cleannames = FALSE)
  dm <- tidyr::drop_na(d, "married", "age", "race")
  dm$y <- as.integer(dm$married == reg_call(t)$positive_level)
  g <- stats::glm(y ~ age, data = dm, family = stats::binomial())
  i <- which(as.character(t$var) == "age")
  expect_equal(get_diff(t[["Obs_log(OR)"]])[i], unname(stats::coef(g)["age"]), tolerance = 1e-10)
})

test_that("a model with ONLY numeric predictors builds its crude columns", {
  # reg_empirical() over character(0) used to return a 0x0 tibble -> reg_empirical_columns() errored.
  d <- num_data()
  expect_no_error(
    t <- tab_reg(d, "married", c("age", "tvhours"), family = "binomial",
                 empirical = TRUE, cleannames = FALSE))
  expect_true("Obs_OR" %in% names(t))
  expect_true(all(!is.na(get_or(t[["Obs_OR"]])[as.character(t$var) %in% c("age", "tvhours")])))
})


# --- 4. the crude AME arm --------------------------------------------------------------------------

test_that("effect = 'ame' / 'ame_ratio': the numeric crude cell is the UNIVARIABLE marginal effect", {
  skip_if_not_installed("marginaleffects")
  d  <- num_data()
  dm <- tidyr::drop_na(d, "married", "age", "race")

  for (eff in c("difference", "ratio")) {
    t <- tab_reg(d, "married", c("age", "race"), family = "binomial",
                 effect = "marginal", measure = eff,
                 empirical = TRUE, multiplier = 1, cleannames = FALSE)
    i  <- which(as.character(t$var) == "age")
    dm$y <- as.integer(dm$married == reg_call(t)$positive_level)
    g  <- stats::glm(y ~ age, data = dm, family = stats::binomial())
    m  <- if (eff == "ratio")
      marginaleffects::avg_comparisons(g, variables = "age", comparison = "lnratioavg")
    else marginaleffects::avg_comparisons(g, variables = "age")

    # Phase 20d: the ESTIMATE is exact (analytic g-computation, rel diff 0); the BOUND is looser on
    # purpose -- ours comes from an analytic jacobian and marginaleffects' from a finite-difference
    # one, whose own step-size choice (fdforward vs fdcenter) moves it by ~4e-9, more than we differ
    # from it. The oracle is the approximation here.
    if (eff == "ratio") {
      expect_equal(tabxplor:::fmt_est_of(t[["Obs_RR"]])[i], exp(m$estimate), tolerance = 1e-10)
      expect_equal(get_ci_inf(t[["Obs_RR"]])[i], exp(m$conf.low), tolerance = 1e-7)
    } else {
      expect_equal(get_diff(t[["Obs_RD"]])[i],  m$estimate, tolerance = 1e-10)
      expect_equal(get_ci_inf(t[["Obs_RD"]])[i], m$conf.low, tolerance = 1e-7)
    }
    # and it reaches the model column's `obs`, so `adjustment` can score it
    model_col <- names(t)[purrr::map_lgl(t, is_fmt)]
    expect_true(!is.na(get_obs(t[[model_col[[length(model_col)]]]])[i]))
  }
})

test_that("poisson + effect='ame' pairs with the observed mean DIFFERENCE, numeric rows included", {
  # a poisson marginal effect is a difference of expected COUNTS, so its crude counterpart is the
  # observed difference of means -- REG_EMPIRICAL$poisson$diff. It used to fall back to the rate-ratio
  # shape, which reg_same_estimand() then rightly refused to pair, leaving the column unusable.
  skip_if_not_installed("marginaleffects")
  d <- num_data()
  t <- suppressWarnings(tab_reg(d, "tvhours", c("age", "race"), family = "poisson",
                                effect = "marginal", empirical = TRUE, cleannames = FALSE))
  i  <- which(as.character(t$var) == "age")
  mc <- names(t)[purrr::map_lgl(t, is_fmt)]
  expect_true("Obs_diff" %in% names(t))
  expect_identical(get_scale(t[["Obs_diff"]]), "raw_diff")
  expect_false(any(is.na(get_obs(t[[mc[[length(mc)]]]])[i])))
})

test_that("at = 'reference' writes no obs on a numeric row either", {
  skip_if_not_installed("marginaleffects")
  d <- num_data()
  t <- suppressWarnings(tab_reg(d, "married", c("age", "race"), family = "binomial",
                                effect = "at_reference", empirical = TRUE,
                                cleannames = FALSE))
  i  <- which(as.character(t$var) == "age")
  mc <- names(t)[purrr::map_lgl(t, is_fmt)]
  expect_true(all(is.na(get_obs(t[[mc[[length(mc)]]]])[i])))
})


# --- 5. the `multiplier` grammar -------------------------------------------------------------------

or_of <- function(t, v, col = "Model_OR") get_or(t[[col]])[as.character(t$var) == v]

test_that("scalar 'sd' / '2sd' / a number scale EVERY numeric predictor", {
  d  <- num_data()
  p  <- c("age", "tvhours", "race")
  t1 <- tab_reg(d, "married", p, family = "binomial", multiplier = 1,     cleannames = FALSE)
  ts <- tab_reg(d, "married", p, family = "binomial", multiplier = "sd",  cleannames = FALSE)
  t2 <- tab_reg(d, "married", p, family = "binomial", multiplier = "2sd", cleannames = FALSE)
  k  <- reg_call(ts)$multiplier
  expect_named(k, c("age", "tvhours"))
  for (v in c("age", "tvhours")) {
    expect_equal(or_of(ts, v), or_of(t1, v)^k[[v]],       tolerance = 1e-8)
    expect_equal(or_of(t2, v), or_of(t1, v)^(2 * k[[v]]), tolerance = 1e-8)
  }
})

test_that("a NAMED vector overrides per variable; unnamed predictors keep the scalar default", {
  d <- num_data()
  p <- c("age", "tvhours", "race")
  t1 <- tab_reg(d, "married", p, family = "binomial", multiplier = 1, cleannames = FALSE)
  tn <- tab_reg(d, "married", p, family = "binomial", multiplier = c(age = 10),
                cleannames = FALSE)
  ts <- tab_reg(d, "married", p, family = "binomial", multiplier = "sd", cleannames = FALSE)
  expect_equal(or_of(tn, "age"), or_of(t1, "age")^10, tolerance = 1e-8)
  # tvhours is NOT named -> it keeps the SCALAR DEFAULT ("sd"), not per 1 unit
  expect_equal(or_of(tn, "tvhours"), or_of(ts, "tvhours"), tolerance = 1e-12)

  tm <- tab_reg(d, "married", p, family = "binomial",
                multiplier = c(age = "2sd", tvhours = 5), cleannames = FALSE)
  k  <- reg_call(tm)$multiplier
  expect_equal(unname(k[["tvhours"]]), 5)
  expect_equal(or_of(tm, "tvhours"), or_of(t1, "tvhours")^5, tolerance = 1e-8)
})

test_that("multiplier = 1 is per-1-unit everywhere (and stores nothing)", {
  d <- num_data()
  t <- tab_reg(d, "married", c("age", "race"), family = "binomial", multiplier = 1,
               cleannames = FALSE)
  expect_null(reg_call(t)$multiplier)
  expect_false(any(grepl("per ", as.character(t$levels), fixed = TRUE)))
})

test_that("the numeric row's label names its unit", {
  d <- num_data()
  t <- tab_reg(d, "married", c("age", "race"), family = "binomial", multiplier = "sd",
               cleannames = FALSE)
  lab <- as.character(t$levels)[as.character(t$var) == "age"]
  expect_match(lab, "^age \\(per 1 SD \\(.+\\)\\)")
  t10 <- tab_reg(d, "married", c("age", "race"), family = "binomial", multiplier = c(age = 10),
                 cleannames = FALSE)
  expect_match(as.character(t10$levels)[as.character(t10$var) == "age"], "^age \\(per 10 units\\)")
  # Phase 18z15: the label now ends with the OBSERVED-shape sparkline. options(tabxplor.spark =
  # FALSE) restores it byte-for-byte -- the fixture that keeps the option honest.
  withr::with_options(list(tabxplor.spark = FALSE), {
    t0 <- tab_reg(d, "married", c("age", "race"), family = "binomial",
                  multiplier = c(age = 10), cleannames = FALSE)
    expect_identical(as.character(t0$levels)[as.character(t0$var) == "age"], "age (per 10 units)")
  })
})

test_that("the SD is frozen ONCE: same unit across split groups, compared models and dependents", {
  d <- num_data()
  base_k <- function(t) reg_call(t)$multiplier[["age"]]

  plain <- tab_reg(d, "married", c("age", "race"), family = "binomial", multiplier = "sd",
                   cleannames = FALSE)
  d$grp <- factor(ifelse(d$year < 2006, "early", "late"))
  spl   <- tab_reg(d, "married", list(m1 = c("age", "race"), m2 = c("age", "race")),
                   family = "binomial", multiplier = "sd", tab_vars = "grp", cleannames = FALSE)
  expect_equal(base_k(spl), base_k(plain), tolerance = 1e-12)

  cmp <- tab_reg(d, "married", list(m1 = "age", m2 = c("age", "race")), family = "binomial",
                 multiplier = "sd", cleannames = FALSE)
  # the compared models share ONE frozen SD (the union frame), so both columns are on one unit
  expect_equal(base_k(cmp), base_k(plain), tolerance = 1e-12)

  two_dep <- suppressWarnings(tab_reg(d, c("married", "tvhours"), c("age", "race"),
                                      family = c("binomial", "poisson"), multiplier = "sd",
                                      cleannames = FALSE))
  expect_equal(base_k(two_dep), base_k(plain), tolerance = 1e-12)
})

test_that("multiplier rejects a non-numeric predictor name and a bad value", {
  d <- num_data()
  expect_error(tab_reg(d, "married", c("age", "race"), family = "binomial",
                       multiplier = c(race = 2)), "numeric predictor")
  expect_error(tab_reg(d, "married", c("age", "race"), family = "binomial",
                       multiplier = list(age = 2)), "must be a number")
})


# --- 7. the gap test on numeric rows ---------------------------------------------------------------

test_that("the numeric coefficient gap SE == a hand-stacked influence-function computation", {
  skip_if_not_installed("survey")
  d <- num_data()
  # `rr` (modified Poisson on a binary outcome) is collapsible, so the COEFFICIENT gap test fires
  # (a conditional OR is not -- reg_estimand_collapsible()).
  t <- tab_reg(d, "married", c("age", "tvhours", "race"), family = "poisson", empirical = TRUE,
               color = c(TRUE, "adjustment"), multiplier = 1, cleannames = FALSE)
  i  <- which(as.character(t$var) == "age")
  se <- get_gap_se(t[["Model_RR"]])[i]
  expect_true(is.finite(se) && se > 0)

  dm <- tidyr::drop_na(d, "married", "age", "tvhours", "race")
  dm$y <- as.numeric(dm$married == reg_call(t)$positive_level)
  des  <- suppressWarnings(survey::svydesign(ids = ~1, data = dm))
  fm   <- survey::svyglm(y ~ age + tvhours + race, design = des, family = stats::quasipoisson())
  fc   <- survey::svyglm(y ~ age,                  design = des, family = stats::quasipoisson())
  ifm  <- reg_coef_if_maker(fm)(stats::setNames(1, "age"))
  ifc  <- reg_coef_if_maker(fc)(stats::setNames(1, "age"))
  expect_equal(se, reg_if_se(ifm - ifc, fm$survey.design), tolerance = 1e-12)
})

test_that("multiplier scales the numeric gap SE by |k| (so the z is invariant)", {
  d <- num_data()
  mk <- function(k) tab_reg(d, "married", c("age", "tvhours", "race"), family = "poisson",
                            empirical = TRUE, color = c(TRUE, "adjustment"),
                            multiplier = if (identical(k, 1)) 1 else c(age = k), cleannames = FALSE)
  i   <- which(as.character(mk(1)$var) == "age")
  t1  <- mk(1); t10 <- mk(10)
  expect_equal(get_gap_se(t10[["Model_RR"]])[i], 10 * get_gap_se(t1[["Model_RR"]])[i],
               tolerance = 1e-10)
  z <- function(t) {
    c <- t[["Model_RR"]]
    (log(get_or(c)[i]) - log(get_obs(c)[i])) / get_gap_se(c)[i]
  }
  expect_equal(z(t10), z(t1), tolerance = 1e-8)
})

test_that("effect = 'ame' / 'ame_ratio': numeric rows get a gap SE too (the IF numeric arm)", {
  skip_if_not_installed("marginaleffects")
  d <- num_data()
  for (eff in c("difference", "ratio")) {
    t <- tab_reg(d, "married", c("age", "race"), family = "binomial",
                 effect = "marginal", measure = eff,
                 empirical = TRUE, color = c(TRUE, "adjustment"), multiplier = 1,
                 cleannames = FALSE)
    mc <- names(t)[purrr::map_lgl(t, is_fmt)]
    col <- t[[mc[[length(mc)]]]]
    i   <- which(as.character(t$var) == "age")
    se  <- get_gap_se(col)[i]
    expect_true(is.finite(se) && se > 0, info = eff)
    # every row of the column is covered -- factor rows too, so no cell is silently greyed
    expect_true(all(is.finite(get_gap_se(col)[as.character(t$var) == "race" &
                                                !is_refrow(col)])), info = eff)
    # the two estimators share their rows, so the IF SE must be SMALLER than naive quadrature
    se_m <- (get_ci_sup(col)[i] - get_ci_inf(col)[i]) / (2 * stats::qnorm(0.975))
    ec   <- if (eff == "ratio") "Obs_RR" else "Obs_RD"
    se_c <- if (eff == "ratio")
      (log(get_ci_sup(t[[ec]])[i]) - log(get_ci_inf(t[[ec]])[i])) / (2 * stats::qnorm(0.975))
    else (get_ci_sup(t[[ec]])[i] - get_ci_inf(t[[ec]])[i]) / (2 * stats::qnorm(0.975))
    expect_lt(se, sqrt(se_m^2 + se_c^2))
  }
})

test_that("a conditional OR still gets NO gap test on a numeric row (collapsibility ruling)", {
  d <- num_data()
  t <- tab_reg(d, "married", c("age", "race"), family = "binomial", empirical = TRUE,
               color = c(TRUE, "adjustment"), multiplier = 1, cleannames = FALSE)
  expect_true(all(is.na(get_gap_se(t[["Model_OR"]]))))
})


# --- 8. the numeric distribution tooltip -----------------------------------------------------------

test_that("a numeric row carries its distribution in the crude column's tooltip", {
  d  <- num_data()
  t  <- tab_reg(d, "married", c("age", "race"), family = "binomial", empirical = TRUE,
                cleannames = FALSE)
  tp <- get_empirical_tips(t)
  expect_true(!is.null(tp) && nrow(tp) >= 1L)
  row <- tp[tp$var == "age", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$col, "Obs_OR")                  # the EFFECT column, which has visible content
  expect_match(row$tip, "mean .* \\(SD .*\\)")
  expect_match(row$tip, "mean if yes")                 # binary outcome -> mean(X | Y) per group

  # gaussian: no per-group split, just the predictor's own distribution
  tg  <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", empirical = TRUE,
                 cleannames = FALSE)
  rg  <- get_empirical_tips(tg)
  expect_identical(rg$col[rg$var == "age"], "Obs_diff")
  expect_no_match(rg$tip[rg$var == "age"], "mean if")
})

test_that("a compound formula gets NO numeric crude column (the estimand would not match)", {
  d <- num_data()
  t <- suppressWarnings(tab_reg(d, married ~ race * age, family = "binomial",
                                empirical = TRUE, cleannames = FALSE))
  num_rows <- which(as.character(t$var) == "age")
  if (length(num_rows) && "Obs_OR" %in% names(t))
    expect_true(all(is.na(get_or(t[["Obs_OR"]])[num_rows])))
  else succeed()
})
