# PURPOSE: the table tab_reg() builds, and its estimates against the engine that produced them.
# ROLE: the shipped CONTRACT for R/tab_reg.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the table tab_reg() builds, per family ==============================================

skip_on_cran()




reg_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}




test_that("family='auto' detects binary -> binomial, and an integer outcome -> gaussian", {
  d <- reg_data()
  tabxplor:::tx_reset_messages()   # the note is once per session
  expect_message(tab_reg(d, "married", "race", cleannames = FALSE), "binary")
  # Phase 18z13 (D10): an integer-valued numeric used to abort as "ambiguous", which caught every
  # integer-STORED continuous outcome -- age in years, a Likert sum, income in whole units. It now
  # reads as gaussian (which always fits) and the message names poisson for a genuine count. The R side
  # and the jamovi family selector agree on that rule.
  tabxplor:::tx_reset_messages()   # the note is once per session
  expect_message(t <- tab_reg(d, "tvhours", "race"), "gaussian")
  tabxplor:::tx_reset_messages()   # the note is once per session
  expect_message(tab_reg(d, "tvhours", "race"), "poisson")            # ... naming the count alternative
  expect_identical(get_model_family(t[["Model_diff"]]), "gaussian")
})




# ---- gaussian beta: parity + additive fmt shape ---------------------------------------------

test_that("tab_reg() gaussian betas / CI / p match stats::lm; fmt uses the additive coef shape", {
  d   <- reg_data()
  # Phase 18z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", so a numeric predictor's row would otherwise be per-1-SD).
  t1  <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", multiplier = 1,
                 ref = c(age = 0), empirical = FALSE, cleannames = FALSE)
  col <- t1[["Model_diff"]]

  expect_identical(tabxplor:::fmt_var_kind(col), "coef")
  # row 1 is the Constant, a BASELINE: it renders the scale's `const_display` (the mean the
  # coefficients add to), not the effect token every other row carries.
  expect_identical(get_display(col)[1], "mean")
  expect_identical(get_display(col)[2], "est")
  expect_identical(get_scale(col), "raw_diff")
  expect_identical(get_color(col), "difference")
  expect_identical(get_color_signif(col), "grey_non_signif")

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  m  <- stats::lm(tvhours ~ age + race, data = dm)
  co <- summary(m)$coefficients
  tq <- stats::qt(0.975, stats::df.residual(m))
  bm <- co[, "Estimate"]
  lo <- co[, "Estimate"] - tq * co[, "Std. Error"]
  hi <- co[, "Estimate"] + tq * co[, "Std. Error"]
  pm <- co[, 4]

  keep <- !is.na(get_pvalue(col))     # the estimated coefs (ref levels and the Constant carry no test)
  expect_equal(sum(keep), length(bm) - 1L)
  expect_equal(sort(get_diff(col)[keep]),   sort(unname(bm[-1])), tolerance = 1e-6)  # beta in `diff`
  expect_equal(sort(get_ci_inf(col)[keep]), sort(unname(lo[-1])), tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(unname(hi[-1])), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(unname(pm[-1])), tolerance = 1e-6)
  # the intercept is still the fit's own, on the baseline row's own field
  expect_equal(get_mean(col)[1], unname(bm[1]), tolerance = 1e-6)

  # reference-level betas are 0 (the additive neutral), no CI/p; var field carries var(Y)
  ref_lvls <- is_refrow(col) & as.character(t1$var) != "Constant"
  expect_true(all(get_diff(col)[ref_lvls] == 0))
  expect_true(all(is.na(get_pvalue(col)[ref_lvls])))
  expect_equal(unique(get_var(col)), stats::var(dm$tvhours), tolerance = 1e-6)
})




# ---- poisson IRR: parity + multiplicative fmt shape -----------------------------------------

test_that("tab_reg() poisson IRR / CI / p match glm(poisson); fmt uses the OR shape", {
  d   <- reg_data()
  # suppressWarnings: this fixture is genuinely over-dispersed, so the Phase 12f dispersion flag
  # fires. That is correct and asserted in test-tab_reg-footer.R; here it is incidental noise.
  # Phase 18z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", so a numeric predictor's row would otherwise be per-1-SD).
  t1  <- suppressWarnings(tab_reg(d, "tvhours", c("age", "race"), family = "poisson", multiplier = 1,
                                  ref = c(age = 0), empirical = FALSE, cleannames = FALSE))
  col <- t1[["Model_IRR"]]

  # a rate ratio's own scale: odds_ratio's ladder and glyphs, a MEAN as the level it sits on
  expect_identical(get_pct_type(col), "none")
  expect_identical(get_display(col)[1], "mean")   # the Constant: the baseline mean count
  expect_identical(get_display(col)[2], "est")
  expect_identical(get_scale(col), "mean_ratio")

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  # 14v-ii: an unweighted over-dispersed Poisson is fit by MLE (so the IRR = exp(coef) is the Poisson
  # estimate) but its SEs are scaled by sqrt(dispersion) and the interval uses t(df.residual) -- exactly
  # a quasi-Poisson fit's Wald interval. So the CI/p reference is quasipoisson, the point estimate poisson.
  m   <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  mq  <- stats::glm(tvhours ~ age + race, data = dm, family = stats::quasipoisson())
  coq <- summary(mq)$coefficients
  crit <- stats::qt(0.975, df = stats::df.residual(mq))
  irr <- exp(stats::coef(m))
  lo  <- exp(coq[, 1] - crit * coq[, 2])
  hi  <- exp(coq[, 1] + crit * coq[, 2])
  pm  <- coq[, 4]

  keep <- !is.na(get_pvalue(col))     # the Constant is a baseline mean count: no test, no ratio
  expect_equal(sort(get_ratio(col)[keep]),  sort(unname(irr)[-1]), tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(unname(lo)[-1]),  tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(unname(hi)[-1]),  tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(unname(pm)[-1]),  tolerance = 1e-6)
  expect_equal(get_mean(col)[1], unname(irr)[1], tolerance = 1e-6)
})




# ---- summed-score grouped binomial (Phase 12c-ii) -------------------------------------------

gb_data <- function() {
  reg_data() |>
    dplyr::mutate(score = pmin(as.integer(tvhours), 10L))   # a summed score 0..10 ("yes" out of 10)
}




# ---- nominal (multinomial) & ordinal (proportional-odds) 3+ level outcomes (Phase 12d) ------
# gss_cat is a fixed dataset (deterministic), so the fits and the Brant p-value are stable.

mnl_data <- function() {                                    # nominal 3-level party, Ind = reference
  fx_reg_df() |>
    dplyr::mutate(party3 = factor(dplyr::case_when(
        grepl("democrat", partyid)   ~ "Dem",
        grepl("republican", partyid) ~ "Rep",
        partyid %in% c("Independent", "Ind,near rep", "Ind,near dem") ~ "Ind"),
      levels = c("Ind", "Dem", "Rep")))
}




ord_data <- function() {                                    # ordered spectrum Rep < Ind < Dem
  mnl_data() |>
    dplyr::mutate(spectrum = factor(as.character(party3),
                                    levels = c("Rep", "Ind", "Dem"), ordered = TRUE))
}




# The WHOLE frame: this fixture exists to VIOLATE proportional odds, and the Brant test only
# rejects when it has the power to. On the sample it does not warn at all.
ord_income_data <- function() {                             # ordered income, known to violate PO
  fx_gss() |>
    dplyr::mutate(income3 = factor(dplyr::case_when(
        rincome %in% c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999",
                       "$5000 to 5999") ~ "1-low",
        rincome %in% c("$6000 to 6999", "$7000 to 7999", "$8000 to 9999",
                       "$10000 - 14999") ~ "2-mid",
        rincome %in% c("$15000 - 19999", "$20000 - 24999", "$25000 or more") ~ "3-high"),
      levels = c("1-low", "2-mid", "3-high"), ordered = TRUE))
}




test_that("tab_reg() multinomial OR / CI / p match nnet::multinom; one OR column per category", {
  skip_if_not_installed("nnet")
  d  <- mnl_data()
  # `multiplier = 1`: the parity claim is against nnet's own PER-UNIT coefficient. Since Phase 22b-v
  # the default scales a continuous predictor per SD on every family, multinomial included -- which
  # the block after this one checks.
  t1 <- tab_reg(d, "party3", c("race", "age"), family = "multinomial", cleannames = FALSE,
                empirical = FALSE, multiplier = 1, ref = c(age = 0))

  # one OR column per non-reference outcome category, "vs <ref>" in the label
  expect_true(all(c("Dem vs Ind", "Rep vs Ind") %in% names(t1)))
  col1 <- t1[["Dem vs Ind"]]
  expect_identical(get_pct_type(col1), "row")
  expect_identical(get_display(col1)[1], "or")     # the Constant: a baseline odds
  expect_identical(get_display(col1)[2], "est")
  expect_identical(get_scale(col1), "odds_ratio")

  dm <- d |> dplyr::filter(!is.na(party3), !is.na(race), !is.na(age))
  dm$race   <- forcats::fct_drop(dm$race)
  dm$party3 <- forcats::fct_drop(dm$party3)
  m  <- nnet::multinom(party3 ~ race + age, data = dm, trace = FALSE)
  # the reference comes through vcov(), never through summary() -- so it is an INDEPENDENT route to
  # the same numbers as reg_tidy_multinom(), which reads the summary's standard.errors matrix.
  cf <- stats::coef(m); V <- stats::vcov(m)                 # nnet names V "<level>:<term>"
  td <- dplyr::bind_rows(lapply(rownames(cf), function(r) tibble::tibble(
    y.level = r, term = colnames(cf), estimate = unname(cf[r, ]),
    std.error = unname(sqrt(diag(V))[paste0(r, ":", colnames(cf))]))))
  td$p.value <- 2 * stats::pnorm(-abs(td$estimate / td$std.error))
  z  <- stats::qnorm(0.975)

  for (j in c("Dem", "Rep")) {
    tj   <- td[td$y.level == j, ]
    col  <- t1[[paste0(j, " vs Ind")]]
    keep <- !is.na(get_pvalue(col))
    expect_equal(sum(keep), nrow(tj))
    expect_equal(sort(get_or(col)[keep]),     sort(exp(tj$estimate)),                   tolerance = 1e-6)
    expect_equal(sort(get_ci_inf(col)[keep]), sort(exp(tj$estimate - z * tj$std.error)), tolerance = 1e-6)
    expect_equal(sort(get_ci_sup(col)[keep]), sort(exp(tj$estimate + z * tj$std.error)), tolerance = 1e-6)
    expect_equal(sort(get_pvalue(col)[keep]), sort(tj$p.value),                          tolerance = 1e-6)
  }
  ref <- is_refrow(col1) & as.character(t1$var) == "race"   # reference predictor level -> OR 1, no p
  expect_true(all(get_or(col1)[ref] == 1))
  expect_true(all(is.na(get_pvalue(col1)[ref])))

  # Phase 14s (G): every category column of ONE model shares a single col_var (the model label), so no
  # border is drawn between them; the visible column NAMES stay per-category.
  cvs <- vapply(c("Dem vs Ind", "Rep vs Ind"), function(nm) get_col_var(t1[[nm]])[1], character(1))
  expect_equal(length(unique(cvs)), 1L)                     # shared col_var
  expect_false(identical(unname(cvs[1]), "Dem vs Ind")) # not the per-category name
})




test_that("tab_reg() ordinal cumulative OR / CI / p match MASS::polr; single column, Constant NA", {
  skip_if_not_installed("MASS")
  d   <- ord_data()
  t1  <- suppressWarnings(tab_reg(d, "spectrum", c("race", "age"),   # per unit: polr's own scale
                                  family = "ordinal", cleannames = FALSE, multiplier = 1,
                                  ref = c(age = 0)))
  col <- t1[["Model_cumOR"]]            # an ordinal odds ratio is CUMULATIVE, and says so
  expect_identical(get_pct_type(col), "row")
  # a cumulative logit has THRESHOLDS, not one intercept: there is no baseline to place, so the row
  # keeps the ordinary token and stays empty
  expect_identical(get_display(col)[1], "est")
  expect_true(is.na(get_or(col)[1]))
  expect_identical(get_scale(col), "odds_ratio")

  dm <- d |> dplyr::filter(!is.na(spectrum), !is.na(race), !is.na(age))
  dm$race <- forcats::fct_drop(dm$race)
  o  <- MASS::polr(spectrum ~ race + age, data = dm, Hess = TRUE, method = "logistic")
  # coef() is MASS's own answer to which rows are slopes; vcov() still carries the cut-points, so
  # subsetting it by those names is what drops them -- an independent route to reg_tidy_polr().
  cf <- stats::coef(o)
  td <- tibble::tibble(term = names(cf), estimate = unname(cf),
                       std.error = unname(sqrt(diag(stats::vcov(o)))[names(cf)]))
  expect_false(any(names(o$zeta) %in% td$term))             # the cut-points are gone
  z  <- stats::qnorm(0.975)
  keep <- !is.na(get_pvalue(col))
  expect_equal(sum(keep), nrow(td))
  expect_equal(sort(get_or(col)[keep]),     sort(exp(td$estimate)),                   tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(exp(td$estimate - z * td$std.error)), tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(exp(td$estimate + z * td$std.error)), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]),
               sort(2 * stats::pnorm(-abs(td$estimate / td$std.error))), tolerance = 1e-6)
  # a cumulative logit has no single intercept -> the "Constant" cell is blank
  expect_true(is.na(get_or(col)[as.character(t1$var) == "Constant"]))
})




test_that("ordinal PO diagnostic warns when the parallel-lines assumption is violated", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("brant")
  # Phase 20f: the Brant test is the Proportionality CHECK's statistic and it fits J-1 binary logits,
  # so it runs in exactly one place, where its row is built -- not on every polr fit in the build.
  # ⚠ 22b-xviii: it is now an ordinal DEFAULT (a failing parallel-lines assumption makes a cumulative
  # odds ratio a fiction, which is too important to be opt-in), so a default table warns too.
  expect_warning(
    tab_reg(ord_income_data(), "income3", c("race", "age"), family = "ordinal",
            stats = c("n", "proportionality")),
    "proportional-odds"
  )
  expect_warning(
    suppressMessages(tab_reg(ord_income_data(), "income3", c("race", "age"), family = "ordinal")),
    "proportional-odds"
  )
  # and `stats = FALSE` still pays nothing for it
  expect_no_warning(
    suppressMessages(tab_reg(ord_income_data(), "income3", c("race", "age"), family = "ordinal",
                             stats = FALSE))
  )
})




# === Phase 14u: model-comparison structure (K / L1 / L2 / na = "drop_all") ====================

reg_2dep_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "01-Married", "02-Not married")),
      widowed = factor(dplyr::if_else(marital == "Widowed", "01-Widowed", "02-Not"))
    )
}





# ---- `.levels_order`: a predictor's level order is DISPLAY (Phase 22g-x) ---------------------
# Every factor PREDICTOR is fitted under treatment contrasts -- reg_fit_frame() strips `ordered` for
# exactly that reason -- so its order decides ONE thing, which level the others are compared to, and
# that one thing is `ref =`. The rest permutes the row skeleton and must move no number, in any
# family, an ORDERED predictor and an ORDINAL outcome included.
test_that(".levels_order permutes the rows and changes no estimate", {
  d  <- reg_data()
  lv <- levels(d$rincome)
  ord <- list(rincome = c(lv[[1]], rev(lv[-1])))
  cases <- list(
    binomial = function(...) tab_reg(d, "married", c("race", "rincome"), stats = "no",
                                     empirical = FALSE, cleannames = FALSE, ...),
    ordinal  = function(...) tab_reg(d, "partyid", c("race", "rincome"), family = "ordinal",
                                     stats = "no", empirical = FALSE, cleannames = FALSE, ...))
  for (nm in names(cases)) {
    f  <- cases[[nm]]
    t0 <- suppressWarnings(suppressMessages(f()))
    t1 <- suppressWarnings(suppressMessages(f(.levels_order = ord)))
    k  <- as.character(t0$var) == "rincome"
    expect_equal(as.character(t1$levels)[k], c(lv[[1]], rev(lv[-1])), info = nm)  # ref still first
    # the same numbers under the same labels: match one block onto the other by level
    col <- names(t0)[vapply(t0, is_fmt, logical(1))]
    col <- col[[length(col)]]
    m   <- match(as.character(t1$levels)[k], as.character(t0$levels)[k])
    expect_equal(get_num(t1[[col]][k]), get_num(t0[[col]][k])[m], info = nm)
  }
})





# === SECTION: what the table says it fitted =======================================================

test_that("reg_formulas() names the measure to type and the R call that ran", {
  d <- fx_reg_df(); d$married <- as.integer(d$marital == "Married")
  f <- function(...) suppressMessages(
    reg_formulas(tab_reg(d, "married", c("race", "age"), empirical = FALSE, ...)))

  # `link` is the word `link =` takes, so it ROUND-TRIPS; `fit` is the R call, in glm vocabulary --
  # neither column ever prints an internal key ("rr" / "rd" / "mr").
  or <- f(family = "binomial")
  expect_identical(or$link, "odds_ratio")
  expect_identical(or$fit,  'glm(binomial("logit"))')
  expect_identical(or$family, "binomial")            # the OUTCOME family, not the fitter's

  rr <- f(family = "binomial", link = "ratio")
  expect_identical(rr$link, "ratio")                 # a modified Poisson, fitted for the sandwich
  expect_identical(rr$fit,  'svyglm(quasipoisson("log"))')
  expect_identical(rr$family, "binomial")
  expect_identical(suppressMessages(
    reg_formulas(tab_reg(d, "married", c("race", "age"), empirical = FALSE,
                         family = "binomial", link = rr$link)))$fit, rr$fit)

  rd <- f(family = "binomial", link = "difference")
  expect_identical(rd$fit, 'svyglm(binomial("identity"))')

  expect_identical(f(family = "gaussian")$fit, "lm()")
  expect_identical(suppressMessages(
    reg_formulas(tab_reg(d, "tvhours", "race", family = "poisson", empirical = FALSE)))$fit,
    'glm(poisson("log"))')
  expect_identical(suppressMessages(
    reg_formulas(tab_reg(d, "marital", "race", family = "multinomial", empirical = FALSE)))$fit,
    "multinom()")

  # a weighted fit names the fitter that really ran
  d$w <- rep(c(1, 2), length.out = nrow(d))
  w <- suppressMessages(reg_formulas(tab_reg(d, "married", "race", family = "binomial",
                                             wt = "w", empirical = FALSE)))
  expect_identical(w$fit, 'svyglm(quasibinomial("logit"))')
})


# === SECTION: logistic: OR, CI and p against glm and svyglm =======================================

logit_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}




test_that("tab_reg() odds ratios / CI / p match stats::glm (unweighted)", {
  data <- logit_data()
  t1   <- tab_reg(data, "married", c("race", "rincome"), cleannames = FALSE)
  col  <- t1[["Model_OR"]]

  d <- data |> dplyr::filter(!is.na(race), !is.na(rincome), !is.na(married))
  d$married <- forcats::fct_rev(forcats::fct_drop(factor(d$married)))   # glm models "Married"
  g  <- stats::glm(married ~ race + rincome, data = d, family = stats::binomial())
  co <- summary(g)$coefficients
  z  <- stats::qnorm(0.975)
  orm <- exp(co[, "Estimate"])
  lom <- exp(co[, "Estimate"] - z * co[, "Std. Error"])
  him <- exp(co[, "Estimate"] + z * co[, "Std. Error"])
  pm  <- co[, "Pr(>|z|)"]

  keep <- !is.na(get_pvalue(col))            # intercept + estimated coefs (ref levels are NA)
  expect_equal(sum(keep), length(orm))
  expect_equal(sort(get_or(col)[keep]),     sort(unname(orm)), tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(unname(lom)), tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(unname(him)), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(unname(pm)),  tolerance = 1e-6)

  # spot-check term correspondence (not just the multiset)
  i <- which(as.character(t1$levels) == "Black")
  expect_equal(get_or(col)[i], unname(orm["raceBlack"]), tolerance = 1e-6)
})




test_that("tab_reg() matches survey::svyglm with survey weights", {
  skip_if_not_installed("survey")
  data <- logit_data() |>
    dplyr::filter(!is.na(tvhours)) |>
    dplyr::mutate(w = tvhours + 1)                 # strictly positive weights
  col <- tab_reg(data, "married", c("race", "rincome"), wt = "w",
                   cleannames = FALSE)[["Model_OR"]]

  dw <- data |> dplyr::filter(!is.na(race), !is.na(rincome), !is.na(married))
  dw$married <- forcats::fct_rev(forcats::fct_drop(factor(dw$married)))
  des <- survey::svydesign(ids = ~1, weights = ~w, data = dw)
  sg  <- survey::svyglm(married ~ race + rincome, design = des,
                        family = stats::quasibinomial())
  co  <- summary(sg)$coefficients
  tq  <- stats::qt(0.975, sg$df.residual)
  orw <- exp(co[, 1])
  low <- exp(co[, 1] - tq * co[, 2])
  hiw <- exp(co[, 1] + tq * co[, 2])
  pw  <- co[, 4]

  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_or(col)[keep]),     sort(unname(orw)), tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(unname(low)), tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(unname(hiw)), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(unname(pw)),  tolerance = 1e-6)
})




test_that("tab_reg() output exports through every backend without error", {
  t1 <- tab_reg(logit_data(), "married", c("race", "rincome"))
  expect_no_error(tab_kable(t1))
  expect_no_error(tab_md(t1))
  skip_if_not_installed("openxlsx2")
  xf <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(tab_xl(t1, path = xf, replace = TRUE))
  expect_true(file.exists(xf))
})
