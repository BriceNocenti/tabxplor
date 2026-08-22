# Phase 12c: tab_reg() -- unified regression tables (gaussian beta / binomial OR / poisson IRR).
# Statistical soundness is checked against hand-run stats::lm / glm / svyglm, comparing the fmt
# fields tab_reg stores (the CI is the exact dual of the stored p / the significance stars).
# Binary outcomes are exercised by test-tab_reg-binomial.R.
#
# CRAN time: this is the suite's heaviest file (~34 s serial, dozens of model fits). skip_on_cran()
# trims the CRAN check without weakening our own CI -- devtools, covr AND r-lib/actions all set
# NOT_CRAN=true, so this fires ONLY on the CRAN farm (see helper-benchmark.R for the same reasoning).
skip_on_cran()

reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}

# ---- family dispatch + wrapper equivalence --------------------------------------------------

test_that("tab_reg(family='binomial') is identical to tab_reg()", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  t1 <- tab_reg(d, "married", c("race", "rincome"), family = "binomial", cleannames = FALSE)
  t2 <- tab_reg(d, "married", c("race", "rincome"), cleannames = FALSE)
  expect_equal(get_or(t1[["Model_OR"]]),     get_or(t2[["Model_OR"]]))
  expect_equal(get_pvalue(t1[["Model_OR"]]), get_pvalue(t2[["Model_OR"]]))
})

test_that("family='auto' detects binary -> binomial, and an integer outcome -> gaussian", {
  skip_if_not_installed("broom")
  d <- reg_data()
  expect_message(tab_reg(d, "married", "race", cleannames = FALSE), "binary")
  # Phase 18z13 (D10): an integer-valued numeric used to abort as "ambiguous", which caught every
  # integer-STORED continuous outcome -- age in years, a Likert sum, income in whole units. It now
  # reads as gaussian (which always fits) and the message names poisson for a genuine count. The R side
  # and the jamovi family selector agree on that rule.
  expect_message(t <- tab_reg(d, "tvhours", "race"), "gaussian")
  expect_message(tab_reg(d, "tvhours", "race"), "poisson")            # ... naming the count alternative
  expect_identical(get_model_family(t[["Model_diff"]]), "gaussian")
})

test_that("family='auto' detects a continuous outcome -> gaussian (message)", {
  skip_if_not_installed("broom")
  d <- reg_data() |> dplyr::mutate(score = age + 0.5)                 # non-integer -> continuous
  expect_message(col_tab <- tab_reg(d, "score", "race"), "continuous")
  expect_identical(tabxplor:::fmt_var_kind(col_tab[["Model_diff"]]), "coef")
})

# ---- gaussian beta: parity + additive fmt shape ---------------------------------------------

test_that("tab_reg() gaussian betas / CI / p match stats::lm; fmt uses the additive coef shape", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  # Phase 18z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", so a numeric predictor's row would otherwise be per-1-SD).
  t1  <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", multiplier = 1,
                 ref = c(age = 0), cleannames = FALSE)
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

test_that("gaussian beta renders raw (no % / x glyph), reference shows 0", {
  skip_if_not_installed("broom")
  t1  <- tab_reg(reg_data(), "tvhours", "race", family = "gaussian", cleannames = FALSE)
  col <- t1[["Model_diff"]]
  txt <- format(col, special_formatting = TRUE)
  expect_false(any(grepl("%", txt)))                 # no percentage suffix
  ref <- which(is_refrow(col) & as.character(t1$var) != "Constant")
  expect_true(all(txt[ref] == "0"))                  # reference beta shown as bare "0"
})

# ---- poisson IRR: parity + multiplicative fmt shape -----------------------------------------

test_that("tab_reg() poisson IRR / CI / p match glm(poisson); fmt uses the OR shape", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  # suppressWarnings: this fixture is genuinely over-dispersed, so the Phase 12f dispersion flag
  # fires. That is correct and asserted in test-tab_reg-footer.R; here it is incidental noise.
  # Phase 18z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", so a numeric predictor's row would otherwise be per-1-SD).
  t1  <- suppressWarnings(tab_reg(d, "tvhours", c("age", "race"), family = "poisson", multiplier = 1,
                                  ref = c(age = 0), cleannames = FALSE))
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

# ---- exponentiate + references --------------------------------------------------------------

test_that("measure = log on a logit yields raw log-odds (additive coef shape)", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  col <- tab_reg(d, "married", "race", family = "binomial", measure = "log",
                 cleannames = FALSE)[["Model_log(OR)"]]
  expect_identical(tabxplor:::fmt_var_kind(col), "coef")
  expect_identical(get_scale(col), "log_coef")   # a link-scale (log-odds) coefficient

  dm <- d |> dplyr::filter(!is.na(race), !is.na(married))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  g  <- stats::glm(married ~ race, data = dm, family = stats::binomial())
  bm <- summary(g)$coefficients[, "Estimate"]                          # log-odds coefficients
  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_diff(col)[keep]), sort(unname(bm)), tolerance = 1e-6)
})

test_that("ref= relevels a factor predictor's baseline", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  t1 <- tab_reg(d, "married", "race", family = "binomial",
                ref = c(race = "White"), cleannames = FALSE)
  col <- t1[["Model_OR"]]
  white <- which(as.character(t1$levels) == "White" & as.character(t1$var) == "race")
  other <- which(as.character(t1$var) == "race" & as.character(t1$levels) != "White")
  expect_equal(get_or(col)[white], 1)                  # White is now the reference (OR == 1)
  expect_true(all(!is.na(get_or(col)[other])))         # the other race levels get a modelled OR
})

# ---- several dependents + colour ------------------------------------------------------------

test_that("a character `predictors` with several dependents gives one column per dependent", {
  skip_if_not_installed("broom")
  d  <- reg_data() |>
    dplyr::mutate(has_tv = factor(dplyr::if_else(tvhours > 0, "Some TV", "No TV"),
                                  levels = c("Some TV", "No TV")))   # positive level = "Some TV"
  t1 <- tab_reg(d, c("married", "has_tv"), "race", family = "binomial", cleannames = FALSE)
  expect_true(all(c("Model_OR [married]", "Model_OR [has_tv]") %in% names(t1)))
})

# ---- Phase 15e: several dependents with DIFFERENT families in one table ----------------------

test_that("mixed binomial + gaussian: per-column families + byte-parity vs standalone builds", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  mix <- tab_reg(d, c("married", "tvhours"), c("age", "race"),
                 family = c("binomial", "gaussian"), cleannames = FALSE)
  bin <- tab_reg(d, "married", c("age", "race"), family = "binomial", cleannames = FALSE)
  gau <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", cleannames = FALSE)

  or_col   <- "Model_OR [married]"
  beta_col <- "Model_diff [tvhours]"
  expect_true(all(c(or_col, beta_col) %in% names(mix)))

  # each column self-describes its own family (the robust per-column attribute)
  expect_identical(get_model_family(mix[[or_col]]),   "binomial")
  expect_identical(get_model_family(mix[[beta_col]]), "gaussian")
  # and keeps its own fmt shape
  expect_identical(get_scale(mix[[or_col]]), "odds_ratio")
  expect_identical(tabxplor:::fmt_var_kind(mix[[beta_col]]), "coef")

  # a mixed build must NOT perturb any per-column value (identical to the standalone single-family col)
  expect_equal(get_or(mix[[or_col]]),        get_or(bin[["Model_OR"]]))
  expect_equal(get_pvalue(mix[[or_col]]),    get_pvalue(bin[["Model_OR"]]))
  expect_equal(get_diff(mix[[beta_col]]),    get_diff(gau[["Model_diff"]]))
  expect_equal(get_ci_inf(mix[[beta_col]]),  get_ci_inf(gau[["Model_diff"]]))
})

test_that("mixed binomial + poisson: legend effect words are OR and IRR per column", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  mix <- suppressWarnings(tab_reg(d, c("married", "tvhours"), c("age", "race"),
                                  family = c("binomial", "poisson"), cleannames = FALSE))
  meta <- reg_call(mix)
  # Phase 18z13: reg_fmt_cols() skips the per-level `n` columns, which also carry a "[dep]" bracket.
  mixc    <- reg_fmt_cols(mix)
  or_col  <- mix[[grep("married", mixc, value = TRUE)[1]]]
  irr_col <- mix[[grep("tvhours", mixc, value = TRUE)[1]]]
  expect_identical(get_model_family(irr_col), "poisson")
  # the per-column effect word reads the column's OWN family, not the table scalar
  expect_identical(tabxplor:::legend_reg_eff_word(or_col,  meta), "OR")
  expect_identical(tabxplor:::legend_reg_eff_word(irr_col, meta), "IRR")
})

test_that("Phase 17c: reg columns carry a stored `role` (model vs emp), not an 'Emp.' name match", {
  skip_if_not_installed("broom")
  m <- suppressWarnings(tab_reg(reg_data(), "married", c("age", "race"),
                                family = "binomial", empirical = TRUE, cleannames = FALSE))
  role <- tabxplor:::get_role(m)
  fmt_cols <- reg_fmt_cols(m)                            # z13: the `n` column has its own role
  emp   <- fmt_cols[startsWith(fmt_cols, "Obs_")]        # the crude companion columns
  model <- setdiff(fmt_cols, emp)                        # the model-estimate column(s)
  expect_true(length(emp) >= 1L && length(model) >= 1L)
  expect_true(all(role[emp]   == "emp"))                 # written by reg_empirical_columns
  expect_true(all(role[model] == "model"))               # written by reg_column
  # and the legend reads the role, not the label: rename an Emp. column, role is unchanged
  names(m)[match(emp[1], names(m))] <- "Crude"
  expect_identical(tabxplor:::get_role(m[["Crude"]]), "emp")
})

test_that("mixed-family 'Model:' footer = one line per family; homogeneous = one unprefixed line", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  mix <- tab_reg(d, c("married", "tvhours"), c("age", "race"),
                 family = c("binomial", "gaussian"), cleannames = FALSE)
  hom <- tab_reg(d, "married", c("age", "race"), family = "binomial", cleannames = FALSE)

  ml <- tabxplor:::reg_model_lines(mix)
  expect_length(ml, 2L)
  expect_true(any(grepl("logistic regression", ml)) && any(grepl("linear regression", ml)))
  expect_true(any(grepl("married", ml)) && any(grepl("tvhours", ml)))

  mlh <- tabxplor:::reg_model_lines(hom)
  expect_length(mlh, 1L)
  expect_false(grepl("^Model \\(", mlh))                       # no per-family prefix when homogeneous
  expect_identical(mlh, tabxplor:::reg_model_line(reg_call(hom)))
})

test_that("mixed-family caption is generic; homogeneous keeps its family name", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  mix <- tab_reg(d, c("married", "tvhours"), c("age", "race"),
                 family = c("binomial", "gaussian"), cleannames = FALSE)
  hom <- tab_reg(d, "married", c("age", "race"), family = "binomial", cleannames = FALSE)
  expect_match(tabxplor:::reg_title(reg_call(mix)), "^Regression models")
  expect_match(tabxplor:::reg_title(reg_call(hom)), "^Logistic regression")
})

test_that("mixed-family GOF footer keeps each outcome's own stat set", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  mix <- tab_reg(d, c("married", "tvhours"), c("age", "race"),
                 family = c("binomial", "gaussian"), cleannames = FALSE)
  tst <- get_test(mix)
  or_col   <- "Model_OR [married]"
  beta_col <- "Model_diff [tvhours]"
  # gaussian stats keyed to the gaussian column, glm stats to the logit column
  expect_true("r2"          %in% tst$test[tst$col == beta_col])
  expect_true("mcfadden_r2" %in% tst$test[tst$col == or_col])
  expect_false("r2"          %in% tst$test[tst$col == or_col])
  expect_false("mcfadden_r2" %in% tst$test[tst$col == beta_col])
})

test_that("auto colour default is per-family (OR for the logit, diff for the gaussian)", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  mix <- tab_reg(d, c("married", "tvhours"), c("age", "race"),
                 family = c("binomial", "gaussian"), cleannames = FALSE)
  expect_identical(get_color(mix[["Model_OR [married]"]]),   "odds_ratio")
  expect_identical(get_color(mix[["Model_diff [tvhours]"]]), "difference")
})

test_that("family accepts a named vector; auto-detection is per dependent (ambiguous integer names itself)", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  # named vector keyed by dependent
  mix <- tab_reg(d, c("married", "tvhours"), c("age", "race"),
                 family = c(tvhours = "gaussian", married = "binomial"), cleannames = FALSE)
  expect_identical(get_model_family(mix[["Model_OR [married]"]]),   "binomial")
  expect_identical(get_model_family(mix[["Model_diff [tvhours]"]]), "gaussian")
  # Phase 18z13 (D10): auto-detection resolves each outcome on its own -- binary -> binomial,
  # integer-valued numeric -> gaussian -- so a mixed pair needs no explicit `family` at all.
  auto <- suppressMessages(tab_reg(d, c("married", "tvhours"), "race", cleannames = FALSE))
  expect_identical(get_model_family(auto[["Model_OR [married]"]]), "binomial")
  expect_identical(get_model_family(auto[["Model_diff [tvhours]"]]), "gaussian")
})

test_that("mixed-family table exports through md / kable without error", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  mix <- tab_reg(d, c("married", "tvhours"), c("age", "race"),
                 family = c("binomial", "gaussian"), cleannames = FALSE)
  expect_no_error(tab_md(mix))
  expect_no_error(tab_kable(mix))
})

test_that("colour: gaussian beta greys non-significant / reference, colours a large standardized beta", {
  skip_if_not_installed("broom")
  t1  <- tab_reg(reg_data(), "tvhours", c("age", "race"), family = "gaussian", cleannames = FALSE)
  col <- t1[["Model_diff"]]
  txt <- fmt_color_channels(col)$text
  sig <- !is.na(get_ci_inf(col)) & (get_ci_inf(col) > 0 | get_ci_sup(col) < 0)

  expect_true(all(txt[!sig] == 0))                     # non-significant cells uncoloured (grey)
  expect_true(all(txt[is_refrow(col)] == 0))           # reference cells uncoloured
  std  <- abs(get_diff(col)) / sqrt(get_var(col))      # the beta/SD(Y) effect size the colour uses
  cand <- which(sig & !is_refrow(col) & std > 0.21)    # significant AND past the first (0.2) break
  skip_if(length(cand) == 0)
  expect_true(all(txt[cand] != 0))
})

# ---- summed-score grouped binomial (Phase 12c-ii) -------------------------------------------

gb_data <- function() {
  reg_data() |>
    dplyr::mutate(score = pmin(as.integer(tvhours), 10L))   # a summed score 0..10 ("yes" out of 10)
}

test_that("grouped binomial (trials=) matches glm(cbind(s, q-s)); OR fmt shape", {
  skip_if_not_installed("broom")
  d   <- gb_data()
  # suppressWarnings: the grouped-binomial fixture is over-dispersed -> the Phase 12f dispersion
  # flag fires (correct; asserted in test-tab_reg-footer.R). This test is about the OR/CI/p parity.
  t1  <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                  cleannames = FALSE))
  col <- t1[["Model_OR"]]

  # a summed score's odds ratio sits on the mean SCORE, which is a mean and not a percentage
  expect_identical(get_pct_type(col), "none")
  expect_identical(get_display(col)[1], "or")    # the Constant: the baseline per-item odds
  expect_identical(get_display(col)[2], "est")
  expect_identical(get_scale(col), "score_odds_ratio")

  dm <- d |> dplyr::filter(!is.na(score), !is.na(race))
  dm$race <- forcats::fct_drop(factor(dm$race))
  # 14v-ii: a grouped/summed-score binomial is over-dispersible, so tab_reg scales its SEs by
  # sqrt(dispersion) + t(df.residual) = a quasi-binomial Wald interval; the OR = exp(coef) stays the
  # binomial MLE estimate.
  g   <- stats::glm(cbind(score, 10 - score) ~ race, data = dm, family = stats::binomial())
  gq  <- stats::glm(cbind(score, 10 - score) ~ race, data = dm, family = stats::quasibinomial())
  coq <- summary(gq)$coefficients
  crit <- stats::qt(0.975, df = stats::df.residual(gq))

  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_or(col)[keep]),     sort(exp(unname(stats::coef(g)))),          tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(exp(unname(coq[, 1] - crit * coq[, 2]))), tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(exp(unname(coq[, 1] + crit * coq[, 2]))), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(unname(coq[, 4])),               tolerance = 1e-6)
  # the first race level is the reference (OR == 1, no CI/p)
  ref <- is_refrow(col) & as.character(t1$var) == "race"
  expect_true(all(get_or(col)[ref] == 1))
})


test_that("trials=TRUE uses the observed max score; measure = log gives the coef shape", {
  skip_if_not_installed("broom")
  d  <- gb_data()
  # suppressWarnings: over-dispersed fixture -> the dispersion flag (asserted in test-tab_reg-footer.R).
  auto <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = TRUE,
                                   cleannames = FALSE))
  ten  <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                   cleannames = FALSE))
  expect_equal(max(d$score, na.rm = TRUE), 10L)
  expect_equal(get_or(auto[["Model_OR"]]), get_or(ten[["Model_OR"]]))

  b <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                measure = "log",
                                cleannames = FALSE))[["Model_log(OR)"]]
  expect_identical(tabxplor:::fmt_var_kind(b), "coef")
  expect_identical(get_scale(b), "log_coef")
})

test_that("trials= reaches the rr / rd links too (link = ratio / difference)", {
  skip_if_not_installed("broom")
  # The regression this guards: `link = "ratio"` / `"difference"` resolve the fit to the internal
  # keys `rr` / `rd`, which reg_is_grouped_binomial() used to miss (it tested the FIT key against
  # "binomial"), so both dropped `trials` and met the raw 0..q score -- an abort on an estimand
  # reg_measures() reports as available.
  d  <- gb_data()
  dm <- d |> dplyr::filter(!is.na(score), !is.na(race))
  dm$race <- forcats::fct_drop(factor(dm$race))
  dm$fail <- 10L - dm$score
  dm$tr   <- 10

  # rr: the modified Poisson models the success COUNT with log(trials) as offset, so exp(coef) is a
  # PER-ITEM risk ratio (and the intercept a per-item risk, not an expected count).
  rr <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                 link = "ratio", cleannames = FALSE))[["Model_RR"]]
  g_rr <- stats::glm(score ~ race + offset(log(tr)), data = dm, family = stats::quasipoisson("log"))
  # the Constant row is kept in the comparison: exp(intercept) is the per-item risk at the reference
  # profile, and it is the value that proves the offset was applied (without it the intercept would
  # be an expected count out of 10).
  # a summed score's RISK ratio has its own scale, `ratio` field and "/ x" glyphs -- borrowing the
  # odds-ratio row printed every one of them as "1/x".
  expect_identical(get_scale(rr), "score_ratio")
  expect_equal(sort(get_ratio(rr)[!is.na(get_pvalue(rr))]),
               sort(exp(unname(stats::coef(g_rr))[-1])), tolerance = 1e-6)
  # the intercept sits on the row's baseline field, in the column's own unit (the mean SCORE)
  expect_equal(get_mean(rr)[[1]], exp(unname(stats::coef(g_rr))[[1]]) * 10, tolerance = 1e-6)

  # rd: the identity link takes the two-column response directly.
  rd <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                 link = "difference", cleannames = FALSE))[["Model_RD"]]
  g_rd <- stats::glm(cbind(score, fail) ~ race, data = dm, family = stats::binomial("identity"),
                     start = stats::coef(stats::lm(I(score / 10) ~ race, data = dm)))
  # a SUMMED SCORE's additive effect is a difference of mean SCORES, not of per-item probabilities:
  # the fit reports the latter and E[score] = trials x p converts it exactly, so the column sits on
  # `raw_diff` (one unit throughout: places out of 10) rather than on the probability-scale `points`.
  expect_identical(get_scale(rd), "raw_diff")
  expect_equal(sort(get_diff(rd)[!is.na(get_pvalue(rd))]),
               sort(unname(stats::coef(g_rd))[-1] * 10), tolerance = 1e-6)

  # the two links, fitted independently, agree on that reference-profile value -- and both baseline
  # rows now state it in the same unit, the mean score
  expect_equal(get_mean(rr)[[1]], get_mean(rd)[[1]], tolerance = 1e-6)
})


test_that("trials errors outside the binomial family; ordinary >2-level binomial still aborts", {
  skip_if_not_installed("broom")
  d <- gb_data()
  expect_error(tab_reg(d, "score", "race", family = "poisson", trials = 10), "trials")
  expect_error(tab_reg(d, "score", "race", family = "binomial"), "binary|trials")  # no trials -> abort
})

test_that("trials rejects a column name / a bad count AT THE BOUNDARY (Phase 18z16-iv)", {
  skip_if_not_installed("broom")
  d <- gb_data()
  d$q <- 10L                                    # a per-row item-count column, the natural mistake
  # used to die inside glm() with "contrasts can be applied only to factors with 2 or more levels"
  expect_error(tab_reg(d, "score", "race", family = "binomial", trials = "q"),
               "not a column name")
  # Phase 19k: a named vector may LEAVE an outcome out (that entry falls back to its observed
  # maximum, which is what lets explicit and automatic counts mix), but a name matching NO outcome
  # is a typo and says so.
  expect_error(tab_reg(d, "score", "race", family = "binomial", trials = c(other = 10)),
               "not an outcome")
  expect_error(tab_reg(d, "score", "race", family = "binomial", trials = 0),
               "positive item count")
  # FALSE is the off switch, symmetric with TRUE -> the same abort as no `trials` at all
  expect_error(tab_reg(d, "score", "race", family = "binomial", trials = FALSE), "binary|trials")
})

# ---- formula escape-hatch (Phase 12c-ii) ----------------------------------------------------

test_that("a simple formula reduces to the dependent+predictors path (identical)", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  t1 <- tab_reg(d, married ~ race + rincome, family = "binomial", cleannames = FALSE)
  t2 <- tab_reg(d, "married", c("race", "rincome"), family = "binomial", cleannames = FALSE)
  expect_identical(t1, t2)
})

test_that("a compound formula (poly) fits with best-effort term rows; coefs match lm", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  t1  <- tab_reg(d, tvhours ~ race + poly(age, 2), family = "gaussian", cleannames = FALSE)
  col <- t1[["Model_diff"]]

  expect_true(any(grepl("poly\\(age, 2\\)", as.character(t1$var))))   # poly -> its own term block
  expect_true(any(as.character(t1$var) == "race"))                    # race still a factor block
  ref <- is_refrow(col) & as.character(t1$var) == "race"
  expect_true(all(get_diff(col)[ref] == 0))                          # factor reference level = 0

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  m  <- stats::lm(tvhours ~ race + poly(age, 2), data = dm)
  co <- summary(m)$coefficients
  keep <- !is.na(get_pvalue(col))                     # the Constant is a baseline: no test
  expect_equal(sort(get_diff(col)[keep]), sort(unname(co[, "Estimate"])[-1]), tolerance = 1e-6)
})

test_that("a compound formula with an interaction renders and exports without error", {
  skip_if_not_installed("broom")
  t1 <- tab_reg(reg_data(), tvhours ~ race * rincome, family = "gaussian", cleannames = FALSE)
  expect_s3_class(t1, "tabxplor_grouped_tab")
  expect_true(any(grepl(":", as.character(t1$var))))                  # the interaction term rows
  expect_no_error(tab_kable(t1))
  expect_no_error(tab_md(t1))
})

test_that("a compound formula reaches the 3+ level engines too (Phase 20f-iiii)", {
  # TWO measured defects, both silent. (1) reg_fit_multinom() / reg_fit_ordinal() BUILT the formula
  # from the bare predictors and never saw the user's, so the interaction was dropped from the MODEL
  # -- `party3 ~ race * age` fitted `race + age`. (2) reg_skeleton_from_fit() read its coefficient
  # names from coef(), which is a MATRIX for nnet::multinom (names() NULL -> every non-factor term
  # produced zero rows) and drops MASS::polr's intercept (one short, so assign was misaligned).
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  d <- reg_data()[seq(1, nrow(reg_data()), 6), ]
  d$party3 <- forcats::fct_lump_n(d$partyid, 2)
  d$inc3   <- factor(forcats::fct_lump_n(d$rincome, 2), ordered = TRUE)
  d <- as.data.frame(d)
  ref <- tab_reg(d, married ~ race * age, family = "binomial", cleannames = FALSE, stats = FALSE)
  expect_true(sum(grepl(":", as.character(ref$var))) >= 2L)
  for (f in list(list("multinomial", stats::as.formula("party3 ~ race * age")),
                 list("ordinal",     stats::as.formula("inc3 ~ race * age"))))  {
    t <- suppressMessages(
      tab_reg(d, f[[2]], family = f[[1]], cleannames = FALSE, stats = FALSE))
    # the SAME row axis as the glm arm on the same RHS: the interaction is fitted and shown
    expect_identical(as.character(t$var), as.character(ref$var))
    expect_identical(as.character(t$levels), as.character(ref$levels))
  }
})

test_that("formula errors: predictors both supplied, and a call-LHS with family='auto'", {
  skip_if_not_installed("broom")
  d <- reg_data()
  expect_error(tab_reg(d, married ~ race, predictors = "race"), "either")
  expect_error(tab_reg(d, I(tvhours > 2) ~ race), "auto-detect|explicit")
})

# ---- exports --------------------------------------------------------------------------------

test_that("gaussian tab_reg output exports through every backend without error", {
  skip_if_not_installed("broom")
  t1 <- tab_reg(reg_data(), "tvhours", c("age", "race"), family = "gaussian")
  expect_no_error(tab_kable(t1))
  expect_no_error(tab_md(t1))
  skip_if_not_installed("openxlsx2")
  xf <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(tab_xl(t1, path = xf, replace = TRUE))
  expect_true(file.exists(xf))
})

# ---- nominal (multinomial) & ordinal (proportional-odds) 3+ level outcomes (Phase 12d) ------
# gss_cat is a fixed dataset (deterministic), so the fits and the Brant p-value are stable.

mnl_data <- function() {                                    # nominal 3-level party, Ind = reference
  forcats::gss_cat |>
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

ord_income_data <- function() {                             # ordered income, known to violate PO
  forcats::gss_cat |>
    dplyr::mutate(income3 = factor(dplyr::case_when(
        rincome %in% c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999",
                       "$5000 to 5999") ~ "1-low",
        rincome %in% c("$6000 to 6999", "$7000 to 7999", "$8000 to 9999",
                       "$10000 - 14999") ~ "2-mid",
        rincome %in% c("$15000 - 19999", "$20000 - 24999", "$25000 or more") ~ "3-high"),
      levels = c("1-low", "2-mid", "3-high"), ordered = TRUE))
}

test_that("tab_reg() multinomial OR / CI / p match nnet::multinom; one OR column per category", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  d  <- mnl_data()
  # `multiplier = 1`: the parity claim is against nnet's own PER-UNIT coefficient. Since Phase 22b-v
  # the default scales a continuous predictor per SD on every family, multinomial included -- which
  # the block after this one checks.
  t1 <- tab_reg(d, "party3", c("race", "age"), family = "multinomial", cleannames = FALSE,
                multiplier = 1, ref = c(age = 0))

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
  td <- broom::tidy(m)                                      # y.level, term, estimate, std.error, ...
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

test_that("outcome_level= sets the multinomial baseline category", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  t1 <- tab_reg(mnl_data(), "party3", "race", family = "multinomial",
                outcome_level = c(party3 = "Dem"), cleannames = FALSE)
  expect_true(all(c("Ind vs Dem", "Rep vs Dem") %in% names(t1)))
})

test_that("tab_reg() ordinal cumulative OR / CI / p match MASS::polr; single column, Constant NA", {
  skip_if_not_installed("broom")
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
  td <- broom::tidy(o)
  td <- td[td$coef.type == "coefficient", ]                 # drop the cut-point ("scale") rows
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

test_that("multiplier reaches the 3+ level engines: per-SD by DEFAULT on ordinal and multinomial", {
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  skip_if_not_installed("nnet")
  # a per-ONE-unit effect beside a factor contrast is unreadable, so the default scales every family.
  # The whole claim: est^k, se x |k|, p UNCHANGED -- and the level states the unit.
  d  <- ord_data()
  t1 <- suppressWarnings(tab_reg(d, "spectrum", c("race", "age"), family = "ordinal",
                                 cleannames = FALSE))
  tk <- suppressWarnings(tab_reg(d, "spectrum", c("race", "age"), family = "ordinal",
                                 cleannames = FALSE, multiplier = 1))
  i  <- which(as.character(t1$var) == "age")
  k  <- reg_call(t1)$multiplier[["age"]]
  expect_gt(k, 1)
  expect_equal(get_or(t1[["Model_cumOR"]])[i], get_or(tk[["Model_cumOR"]])[i]^k, tolerance = 1e-8)
  expect_equal(get_pvalue(t1[["Model_cumOR"]])[i], get_pvalue(tk[["Model_cumOR"]])[i])
  expect_match(as.character(t1$levels)[i], "^per [0-9.]+ \\(SD\\)")
  # a factor level is untouched by the rescale
  j <- which(as.character(t1$var) == "race" & !is_refrow(t1[["Model_cumOR"]]))
  expect_equal(get_or(t1[["Model_cumOR"]])[j], get_or(tk[["Model_cumOR"]])[j])

  m1 <- tab_reg(mnl_data(), "party3", c("race", "age"), family = "multinomial", cleannames = FALSE)
  mk <- tab_reg(mnl_data(), "party3", c("race", "age"), family = "multinomial", cleannames = FALSE,
                multiplier = 1)
  ia <- which(as.character(m1$var) == "age")
  km <- reg_call(m1)$multiplier[["age"]]
  expect_equal(get_or(m1[["Dem vs Ind"]])[ia], get_or(mk[["Dem vs Ind"]])[ia]^km, tolerance = 1e-8)
  expect_equal(get_pvalue(m1[["Rep vs Ind"]])[ia], get_pvalue(mk[["Rep vs Ind"]])[ia])
})

test_that("family='auto' detects nominal -> multinomial and ordered -> ordinal (messages)", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  expect_message(tab_reg(mnl_data(), "party3", "race"), "multinomial")
  expect_message(suppressWarnings(tab_reg(ord_data(), "spectrum", "race")), "ordinal")
})

test_that("weighted 3+ level: ordinal works (svyolr), MNL needs svyVGAM (Phase 12g)", {
  skip_if_not_installed("broom")
  skip_if_not_installed("survey")
  # weighted ordinal is now supported via survey::svyolr (positive weights: svyolr's start-value
  # glm.fit step cannot take zero weights, and gss_cat's tvhours has zeros).
  ord_w <- dplyr::mutate(ord_data(), w = tvhours + 1)
  t_ord <- suppressMessages(tab_reg(ord_w, "spectrum", "race", family = "ordinal", wt = "w"))
  expect_s3_class(t_ord, "tabxplor_tab")
  # weighted multinomial via svyVGAM when available, else a clear install-hint error
  mnl_w <- dplyr::mutate(mnl_data(), w = tvhours + 1)
  if (requireNamespace("svyVGAM", quietly = TRUE)) {
    t_mnl <- suppressMessages(tab_reg(mnl_w, "party3", "race", family = "multinomial", wt = "w"))
    expect_s3_class(t_mnl, "tabxplor_tab")
  } else {
    expect_error(
      tab_reg(mnl_w, "party3", "race", family = "multinomial", wt = "w"),
      "svyVGAM"
    )
  }
})

test_that("ordinal PO diagnostic warns when the parallel-lines assumption is violated", {
  skip_if_not_installed("broom")
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

test_that("multinomial + ordinal tab_reg output exports without error", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  mnl <- tab_reg(mnl_data(), "party3", c("race", "age"), family = "multinomial")
  ord <- suppressWarnings(tab_reg(ord_data(), "spectrum", "race", family = "ordinal"))
  expect_no_error(tab_kable(mnl))
  expect_no_error(tab_md(mnl))
  expect_no_error(tab_kable(ord))
  expect_no_error(tab_md(ord))
})

# ---- effect = "marginal": average marginal effects + adjusted predictions (Phase 12e-i) ----------
# Parity is checked against marginaleffects run on the SAME model tab_reg fits (binomial: fct_rev to
# model the positive level; factor predictors fct_drop'd), aligning the AME by the "Level - Reference"
# contrast label. The composed cell is AME-first ("-8%*** (16%)") with the prediction in parentheses.
#
# ⚠ every block below NAMES `measure = "difference"`: since the cascade (22b-xv-1) a bare
# `effect = "marginal"` reports the level's own measure -- a RATIO on a probability -- because
# `"auto"` never predicts an odds ratio and never guesses which side of it to fall back to. These
# assertions are about the ADDITIVE marginal effect, so they ask for it.

test_that("binomial AME: diff/pct/CI/p match marginaleffects; AME-first composed cell", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data()
  # Phase 18z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", so a numeric predictor's row would otherwise be per-1-SD).
  t1  <- tab_reg(d, "married", c("race", "age"), family = "binomial", effect = "marginal", measure = "difference", multiplier = 1,
                 cleannames = FALSE)
  col <- t1[["Model_mRD"]]

  expect_identical(get_pct_type(col), "row")
  expect_identical(get_scale(col), "points")   # a binomial AME is a risk difference, in points
  expect_identical(get_color(col), "difference")

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(age))
  dm$race    <- forcats::fct_drop(dm$race)
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))     # glm models P(Married)
  g   <- stats::glm(married ~ race + age, data = dm, family = stats::binomial())
  acr <- as.data.frame(marginaleffects::avg_comparisons(g, variables = "race", newdata = dm))
  aca <- as.data.frame(marginaleffects::avg_comparisons(g, variables = "age",  newdata = dm))
  # decisions doc S50 (change A): the adjusted % is the marginal-STANDARDIZED prediction (variables=),
  # which coheres with the AME -- NOT the observed-group average (by=).
  ap  <- as.data.frame(marginaleffects::avg_predictions(g, variables = "race", newdata = dm))

  # a reference cell carries the measure's NEUTRAL, so "an estimated effect" is one with a p-value
  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_diff(col)[keep]),   sort(c(acr$estimate, aca$estimate)),   tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(c(acr$conf.low, aca$conf.low)),   tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(c(acr$conf.high, aca$conf.high)), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(c(acr$p.value, aca$p.value)),     tolerance = 1e-6)
  lvl_pct <- !is.na(get_pct(col)) & as.character(t1$var) != "Constant"   # the baseline is its own
  expect_equal(sort(get_pct(col)[lvl_pct]), sort(ap$estimate),                    tolerance = 1e-6)

  # every value cell shows THE ESTIMATE; the layout beside it is the table's `display` (here none)
  disp   <- get_display(col)
  ref    <- which(as.character(t1$levels) == "Other" & as.character(t1$var) == "race")
  blk    <- which(as.character(t1$var) == "race" & as.character(t1$levels) == "Black")
  agerow <- which(as.character(t1$var) == "age")
  expect_identical(disp[c(ref, blk, agerow)], rep("est", 3L))
  expect_identical(get_diff(col)[ref], 0)               # the reference level carries the neutral
  expect_true(is.na(get_pvalue(col)[ref]))

  # rendered cell: the AME, with its stars. `display = "est_base"` adds the adjusted prediction.
  txt <- format(col, special_formatting = TRUE, stars = TRUE)
  expect_match(trimws(txt[blk]), "^-[0-9.]+%\\*+$")
  bs  <- format(set_display(col, "est_base"), special_formatting = TRUE, stars = TRUE)
  expect_match(trimws(bs[blk]), "^-[0-9.]+%\\*+ \\([0-9.]+%\\)$")
})

test_that("a gaussian marginal difference builds, and IS the coefficient", {
  skip_if_not_installed("broom")
  # The identity link is collapsible, so averaging changes nothing -- which used to be a REFUSAL and
  # is now a demonstrable fact: two routes, two headers, one number.
  d  <- reg_data()
  co <- suppressMessages(tab_reg(d, "tvhours", c("age", "race"), family = "gaussian",
                                 multiplier = 1, cleannames = FALSE))
  ma <- suppressMessages(tab_reg(d, "tvhours", c("age", "race"), family = "gaussian",
                                 effect = "marginal", measure = "difference", multiplier = 1,
                                 cleannames = FALSE))
  expect_equal(get_diff(co[["Model_diff"]]), get_diff(ma[["Model_mdiff"]]), tolerance = 1e-8)
})

test_that("the gaussian coefficient matches marginaleffects' AME", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data()
  # Phase 18z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", so a numeric predictor's row would otherwise be per-1-SD).
  col <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", multiplier = 1,
                 cleannames = FALSE)[["Model_diff"]]
  expect_identical(tabxplor:::fmt_var_kind(col), "coef")
  expect_identical(get_scale(col), "raw_diff")

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  dm$race <- forcats::fct_drop(dm$race)
  m   <- stats::lm(tvhours ~ age + race, data = dm)
  ac  <- rbind(as.data.frame(marginaleffects::avg_comparisons(m, variables = "age",  newdata = dm)),
               as.data.frame(marginaleffects::avg_comparisons(m, variables = "race", newdata = dm)))
  keep <- !is.na(get_pvalue(col)) & !is_refrow(col)       # reference betas are the additive neutral 0
  expect_equal(sort(get_diff(col)[keep]), sort(ac$estimate), tolerance = 1e-6)
  # the p refers to t(df.residual), as the model's own does; marginaleffects refers to z, so the two
  # differ in the third digit -- assert against the fit itself.
  expect_equal(sort(get_pvalue(col)[keep]),
               sort(unname(summary(m)$coefficients[-1, 4])), tolerance = 1e-8)
})

test_that("poisson AME is a raw count-change and matches marginaleffects", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data()
  # suppressWarnings: over-dispersed poisson fixture -> the dispersion flag (asserted in
  # test-tab_reg-footer.R). This test is about the AME scale and its marginaleffects parity.
  # Phase 18z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", so a numeric predictor's row would otherwise be per-1-SD).
  col <- suppressWarnings(tab_reg(d, "tvhours", c("age", "race"), family = "poisson", multiplier = 1,
                                  effect = "marginal", measure = "difference", cleannames = FALSE))[["Model_mdiff"]]
  expect_identical(tabxplor:::fmt_var_kind(col), "coef")

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  dm$race <- forcats::fct_drop(dm$race)
  m   <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  ac  <- rbind(as.data.frame(marginaleffects::avg_comparisons(m, variables = "age",  newdata = dm)),
               as.data.frame(marginaleffects::avg_comparisons(m, variables = "race", newdata = dm)))
  keep <- !is.na(get_pvalue(col)) & !is_refrow(col)
  expect_equal(sort(get_diff(col)[keep]), sort(ac$estimate), tolerance = 1e-6)
})

test_that("multinomial AME: one column per outcome category, matches marginaleffects", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  skip_if_not_installed("marginaleffects")
  d  <- mnl_data()
  t1 <- tab_reg(d, "party3", c("race", "age"), family = "multinomial", effect = "marginal", measure = "difference",
                cleannames = FALSE, multiplier = 1, ref = c(age = 0))   # per-unit, raw origin
  expect_true(all(c("Ind", "Dem", "Rep") %in% names(t1)))   # every outcome category

  dm <- d |> dplyr::filter(!is.na(party3), !is.na(race), !is.na(age))
  dm$race   <- forcats::fct_drop(dm$race)
  dm$party3 <- forcats::fct_drop(dm$party3)
  m  <- nnet::multinom(party3 ~ race + age, data = dm, trace = FALSE)
  ac <- rbind(as.data.frame(marginaleffects::avg_comparisons(m, variables = "race", newdata = dm)),
              as.data.frame(marginaleffects::avg_comparisons(m, variables = "age",  newdata = dm)))
  for (j in c("Ind", "Dem", "Rep")) {
    col  <- t1[[j]]
    keep <- !is.na(get_pvalue(col))
    acj  <- ac[ac$group == j, ]
    expect_equal(sort(get_diff(col)[keep]),   sort(acj$estimate), tolerance = 1e-6)
    expect_equal(sort(get_pvalue(col)[keep]), sort(acj$p.value),  tolerance = 1e-6)
  }
})

test_that("ordinal marginal: ONE column, and Somers' D matches a hand-computed pair", {
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  d  <- ord_data()
  t1 <- suppressWarnings(tab_reg(d, "spectrum", "race", family = "ordinal", effect = "marginal",
                                 measure = "difference", cleannames = FALSE))
  # the whole point of the phase: an ordinal model reports on the ORDER, in one column
  expect_true("Model_mD" %in% names(t1))
  expect_false(any(c("Rep", "Ind", "Dem") %in% names(t1)))

  dm <- d |> dplyr::filter(!is.na(spectrum), !is.na(race))
  dm$race <- forcats::fct_drop(dm$race)
  o  <- MASS::polr(spectrum ~ race, data = dm, Hess = TRUE, method = "logistic")
  pm <- function(lv) colMeans(predict(o, newdata = transform(
    dm, race = factor(lv, levels = levels(dm$race))), type = "probs"))
  hand <- function(lv) {                         # win - loss on the two standardised distributions
    p1 <- pm(lv); p0 <- pm(levels(dm$race)[[1]]); K <- length(p1)
    sum(p1 * c(0, cumsum(p0)[-K])) - sum(p0 * c(0, cumsum(p1)[-K]))
  }
  col <- t1[["Model_mD"]]
  for (lv in levels(dm$race)[-1]) {
    i <- which(as.character(t1$levels) == lv)
    expect_equal(get_diff(col)[i], hand(lv), tolerance = 1e-8)
  }
  # `{base}` is the probability of superiority, and the reference row's own is a coin flip
  iref <- which(as.character(t1$levels) == levels(dm$race)[[1]])
  expect_equal(get_pct(col)[iref], 0.5, tolerance = 1e-12)
  expect_true(is.na(get_pct(col)[1]))            # the Constant row: a rank has no baseline
})

test_that("weighted binomial AME (svyglm) is population-weighted and matches marginaleffects", {
  skip_if_not_installed("broom")
  skip_if_not_installed("survey")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data() |> dplyr::filter(!is.na(tvhours))
  # suppressWarnings: the fixture uses tvhours as a stand-in weight and it contains zeros, so
  # svyglm() warns ("observations with zero weight not used for calculating dispersion"). That is
  # upstream (survey/stats), not tabxplor, and it fires identically on the hand-run oracle below.
  col <- suppressWarnings(tab_reg(d, "married", "race", family = "binomial", wt = "tvhours",
                                  effect = "marginal", measure = "difference", cleannames = FALSE))[["Model_mRD"]]

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race))
  dm$race    <- forcats::fct_drop(dm$race)
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  des <- survey::svydesign(ids = ~1, weights = ~tvhours, data = dm)
  g   <- suppressWarnings(
    survey::svyglm(married ~ race, design = des, family = stats::quasibinomial()))
  ac  <- as.data.frame(marginaleffects::avg_comparisons(g, variables = "race", newdata = dm,
                                                        wts = "tvhours"))
  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_diff(col)[keep]), sort(ac$estimate), tolerance = 1e-6)
})

test_that("AME tables export through every backend without error", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  t1 <- tab_reg(reg_data(), "married", c("race", "age"), family = "binomial", effect = "marginal", measure = "difference")
  expect_no_error(tab_kable(t1))
  expect_no_error(tab_md(t1))
  skip_if_not_installed("openxlsx2")
  xf <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(tab_xl(t1, path = xf, replace = TRUE))
  expect_true(file.exists(xf))
})

# ---- the `at` profile axis (Phase 12e-ii) ---------------------------------------------------
# at="reference": marginal effects + adjusted predictions at the reference profile (others at their
# first level / mean); MNL coefficient + at="reference": the "j vs rest" OR at that profile. Parity is
# checked against marginaleffects comparisons()/predictions() at a datagrid built the same way.

test_that("at the reference profile (binomial): effect/prediction/CI match marginaleffects there", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data()
  t1  <- tab_reg(d, "married", c("race", "age"), family = "binomial", effect = "at_reference", measure = "difference",
                 multiplier = 1, ref = c(age = 0), cleannames = FALSE)
  col <- t1[["Model_refRD"]]                         # the marker switches m -> ref at the profile
  expect_identical(get_pct_type(col), "row")

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(age))
  dm$race    <- forcats::fct_drop(dm$race)
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  g    <- stats::glm(married ~ race + age, data = dm, family = stats::binomial())
  grid <- marginaleffects::datagrid(model = g, race = levels(dm$race)[1], age = 0)
  acr  <- as.data.frame(marginaleffects::comparisons(g, variables = "race", newdata = grid))
  aca  <- as.data.frame(marginaleffects::comparisons(g, variables = "age",  newdata = grid))
  pg   <- marginaleffects::datagrid(model = g, race = levels(dm$race), age = 0)
  ap   <- as.data.frame(marginaleffects::predictions(g, newdata = pg))

  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_diff(col)[keep]),   sort(c(acr$estimate, aca$estimate)),   tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(c(acr$conf.low, aca$conf.low)),   tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(c(acr$p.value, aca$p.value)),     tolerance = 1e-6)
  lvl_pct <- !is.na(get_pct(col)) & as.character(t1$var) != "Constant"   # the baseline is its own
  expect_equal(sort(get_pct(col)[lvl_pct]), sort(ap$estimate),                    tolerance = 1e-6)
})

test_that("MNL 'j vs rest' OR at the reference profile matches marginaleffects (comparison='lnor')", {
  # ⚠ `measure = "odds_ratio"` is NAMED: a 3+ category outcome has to be asked "versus what?" before
  # a predicted odds ratio means anything, so the cascade never resolves to one on its own.
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  skip_if_not_installed("marginaleffects")
  d  <- mnl_data()
  t1 <- tab_reg(d, "party3", c("race", "age"), family = "multinomial",
                effect = "at_reference", measure = "odds_ratio",
                cleannames = FALSE, ref = c(age = 0))
  expect_true(all(c("Ind vs rest", "Dem vs rest", "Rep vs rest") %in% names(t1)))
  expect_identical(get_scale(t1[["Dem vs rest"]]), "odds_ratio")

  dm <- d |> dplyr::filter(!is.na(party3), !is.na(race), !is.na(age))
  dm$race   <- forcats::fct_drop(dm$race)
  dm$party3 <- forcats::fct_drop(dm$party3)
  m    <- nnet::multinom(party3 ~ race + age, data = dm, trace = FALSE)
  grid <- marginaleffects::datagrid(model = m, race = levels(dm$race)[1], age = 0)
  lc   <- rbind(
    as.data.frame(marginaleffects::comparisons(m, variables = "race", newdata = grid, comparison = "lnor")),
    as.data.frame(marginaleffects::comparisons(m, variables = "age",  newdata = grid, comparison = "lnor")))
  for (j in c("Ind", "Dem", "Rep")) {
    col  <- t1[[paste0(j, " vs rest")]]
    vals <- get_or(col)[!is.na(get_or(col)) & !is_refrow(col)]   # exclude the neutral reference OR = 1
    expect_equal(sort(vals), sort(exp(lc[lc$group == j, ]$estimate)), tolerance = 1e-6)
  }
  ref <- is_refrow(t1[["Ind vs rest"]]) & as.character(t1$var) == "race"
  expect_true(all(get_or(t1[["Ind vs rest"]])[ref] == 1))    # reference predictor level -> OR 1
})

# `at` / `exponentiate` / `estimate_display` were removed in 2.0.0. tab_reg() is unreleased, so they
# are not deprecated: each lands in `...` and aborts as an unknown argument (the shared tab_check_dots
# guard), and a removed `effect` VALUE aborts as an unknown effect value. The point is no silent no-op.
test_that("a removed argument or effect value aborts (no silent no-op)", {
  skip_if_not_installed("broom")
  d <- reg_data()
  expect_error(
    tab_reg(d, "married", "race", family = "binomial", at = "reference", cleannames = FALSE),
    "[Uu]nknown argument"
  )
  expect_error(
    tab_reg(d, "married", "race", family = "binomial", exponentiate = FALSE),
    "[Uu]nknown argument"
  )
  expect_error(
    tab_reg(d, "married", "race", family = "binomial", effect = "ame_ratio"),
    "[Uu]nknown .*effect"
  )
})

test_that("an at-reference table exports through every backend without error", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  t1 <- tab_reg(reg_data(), "married", c("race", "age"), family = "binomial", effect = "at_reference", measure = "difference")
  expect_no_error(tab_kable(t1))
  expect_no_error(tab_md(t1))
  skip_if_not_installed("nnet")
  # the vs-rest builder is the one a 3+ category outcome reaches by NAMING the odds ratio
  t2 <- tab_reg(mnl_data(), "party3", "race", family = "multinomial",
                effect = "at_reference", measure = "odds_ratio")
  expect_no_error(tab_kable(t2))
  expect_no_error(tab_md(t2))
})

# === Phase 14u: model-comparison structure (K / L1 / L2 / na = "drop_all") ====================

reg_2dep_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "01-Married", "02-Not married")),
      widowed = factor(dplyr::if_else(marital == "Widowed", "01-Widowed", "02-Not"))
    )
}

# Phase 20i: the "attach a crude value?" decision is PER SPEC (the estimand's declared `obs`,
# withheld exactly at the reference profile). A multi-outcome table with a MIXED per-outcome `effect`
# must withhold `obs` only on the at_reference columns; a table-scalar `any(!obs)` gate used to blank
# the crude value (and `color = "adjustment"`) on the coefficient columns too.
test_that("per-spec obs: a mixed-effect multi-outcome table keeps `obs` on the coefficient columns", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d <- reg_2dep_data()
  r <- suppressWarnings(tab_reg(
    d, outcome = c("married", "widowed"), predictors = c("race", "age"),
    family = "binomial", effect = c(married = "at_reference", widowed = "conditional"),
    empirical = TRUE, cleannames = FALSE))
  expect_s3_class(r, "tabxplor_tab")   # ONE table (n_outcomes > 1, bracketed columns), not a list
  model_cols <- names(r)[purrr::map_lgl(
    r, ~ is_fmt(.) && identical(as.character(get_role(.))[1], "model"))]
  has_obs <- purrr::map_lgl(model_cols, ~ any(!is.na(get_obs(r[[.]]))))
  # the coefficient outcome (widowed) carries its crude OR -> FAILS before the per-spec fix (all
  # blanked table-wide); the at_reference outcome (married) still withholds it.
  expect_true(all(has_obs[grepl("widowed", model_cols)]))
  expect_false(any(has_obs[grepl("married", model_cols)]))
})

test_that("K: several dependents x a list of models -> a tabxplor_tabs, one per dependent", {
  skip_if_not_installed("broom")
  d <- reg_2dep_data()
  r <- suppressWarnings(tab_reg(
    d, outcome = c("married", "widowed"),
    predictors = list(demo = c("race", "age"), full = c("race", "age", "rincome")),
    family = "binomial", cleannames = FALSE))
  expect_s3_class(r, "tabxplor_tabs")
  expect_length(r, 2L)
  expect_equal(names(r), c("married", "widowed"))
  expect_true(all(purrr::map_lgl(r, ~ inherits(., "tabxplor_tab") || inherits(., "tabxplor_grouped_tab"))))
  # each is a model comparison: one OR column per model
  expect_true(all(c("demo", "full") %in% names(r[[1]])))

  # tab_export("xl") writes one sheet per dependent
  skip_if_not_installed("openxlsx2")
  f <- withr::local_tempfile(fileext = ".xlsx")
  tab_xl(r, path = f, replace = TRUE, open = FALSE)
  wb <- openxlsx2::wb_load(f)
  # Phase 14w (item 1): a reg table's sheet name is the compact "<short>_<dep>_..." tag (a comparison
  # collapses the predictors to "compare"), not the bare dependent.
  expect_setequal(openxlsx2::wb_get_sheet_names(wb),
                  c("logit_married_compare", "logit_widowed_compare"))
})

test_that("L1: a complete model's predictor order is kept (at the end)", {
  skip_if_not_installed("broom")
  d <- reg_2dep_data()
  # `complete` is a superset of `a` -> the union takes `complete`'s own order (race, rincome, age)
  r <- tab_reg(d, "married",
               predictors = list(a = c("age", "race"), complete = c("race", "rincome", "age")),
               family = "binomial", cleannames = FALSE)
  ord <- unique(as.character(r$var)[as.character(r$var) %in% c("race", "age", "rincome")])
  expect_equal(ord, c("race", "rincome", "age"))
  # no complete model -> first-appearance order
  r2 <- tab_reg(d, "married",
                predictors = list(a = c("age", "race"), b = c("rincome")),
                family = "binomial", cleannames = FALSE)
  ord2 <- unique(as.character(r2$var)[as.character(r2$var) %in% c("race", "age", "rincome")])
  expect_equal(ord2, c("age", "race", "rincome"))
})

test_that("L2: a SUPERSET baseline is recognised as nested (LR, not the AIC fallback) under drop_all", {
  skip_if_not_installed("broom")
  d <- reg_2dep_data()
  r <- tab_reg(d, "married",
               predictors = list(small = c("race", "age"), complete = c("race", "age", "rincome")),
               family = "binomial", stats = c(compare_baseline = "complete"),
               na = "drop_all", cleannames = FALSE)
  cmp <- get_test(r) |> dplyr::filter(grepl("^compare", test))
  expect_true("compare_baseline" %in% cmp$test)            # LR test
  expect_false(any(grepl("_aic$", cmp$test)))              # NOT the AIC fallback
})

test_that("na = 'drop_all' fits every model on one shared complete-case population (equal N)", {
  skip_if_not_installed("broom")
  d <- reg_2dep_data()                                     # rincome has NAs -> N would differ per model
  r <- tab_reg(d, "married",
               predictors = list(a = "race", b = c("race", "rincome")),
               family = "binomial", stats = "n", na = "drop_all", cleannames = FALSE)
  ns <- get_test(r) |> dplyr::filter(test == "n")
  expect_equal(length(unique(ns$statistic)), 1L)           # both models share N
})

test_that("Phase h: a predictor dropped from one comparison model keeps its reference-row bold", {
  skip_if_not_installed("broom")
  d <- reg_2dep_data()
  r <- tab_reg(d, "married",
               predictors = list(a = c("race", "age"), b = c("rincome", "age")),
               family = "binomial", cleannames = FALSE)
  race_ref <- is_refrow(r[["a"]]) & as.character(r$var) == "race"
  expect_true(any(race_ref))
  # race is absent from model `b`; its reference-row flag must survive (union-skeleton fact), else the
  # shared cross-column bold (tab_bold_rows ANDs in_refrow) drops it -- the pass-4 maintainer report.
  expect_true(all(is_refrow(r[["b"]])[race_ref]))
  prep <- tab_export_prep(r, backend = "kable")
  bold <- if (!is.null(prep$tables)) prep$tables[[1]]$bold_rows else prep$bold_rows
  expect_true(all(which(race_ref) %in% bold))
})

# ---- Phase 18z13 (SS7.1): the N behind each predictor level -------------------------------------

test_that("the `n` column gives every predictor level its unadjusted N, on the model's own frame", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                cleannames = FALSE))
  # the count is a DISPLAY fact: stored on the model columns, given a column of its own at render
  expect_false("n" %in% names(t))
  m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  expect_identical(which(names(m) == "n"), 3L)              # right after var / levels
  expect_identical(tabxplor:::get_role(m[["n"]]), "n")      # a stored role, not a name match

  # the numbers ARE counts of the model's complete cases -- the same frame the crude companion uses,
  # so an Obs_* block and this column can never count different people
  fr <- tidyr::drop_na(d[, c("married", "race", "age")])
  nn <- get_n(m[["n"]])
  # the baseline row is a PROFILE, and `age` is continuous, so nobody is at it: the cell is empty and
  # the model N is the first "Model fit" footer row instead
  expect_true(is.na(nn[as.character(t$var) == "Constant"]))
  race_rows <- as.character(t$var) == "race"
  expect_equal(sort(nn[race_rows]),
               sort(unname(as.integer(table(forcats::fct_drop(fr$race))))))
  expect_equal(sum(nn[race_rows]), nrow(fr))
  # a numeric predictor's count would be nrow(frame) for every one of them -> deliberately blank
  expect_true(all(is.na(nn[as.character(t$var) == "age"])))

  # and it opts out
  expect_false("n" %in% names(tabxplor:::tab_materialize_extras(suppressMessages(
    tab_reg(d, "married", "race", family = "binomial", n = "no", cleannames = FALSE)),
    backend = "text", pvalue = FALSE)))
})

test_that("the `n` column does not disturb the reference-row bold", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", cleannames = FALSE))
  m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  # tab_bold_rows() ANDs in_refrow across every DISCRIMINATING column, so a column that omitted the
  # flag would silently un-bold every reference row -- the defect Phase 18h fixed for the crude
  # companions. Check the flag, and the rendering it drives.
  expect_true(any(is_refrow(m[["n"]])))
  expect_identical(is_refrow(m[["n"]]), is_refrow(m[["Model_OR"]]))
  md <- tab_md(t, print = FALSE)
  expect_true(grepl("\\*\\*Other\\*\\*", md))          # the reference level, still bold
})
