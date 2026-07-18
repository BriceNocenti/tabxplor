# Phase 12c: tab_reg() -- unified regression tables (gaussian beta / binomial OR / poisson IRR).
# Statistical soundness is checked against hand-run stats::lm / glm / svyglm, comparing the fmt
# fields tab_reg stores (the CI is the exact dual of the stored p / the significance stars).
# tab_logit()/multi_logit() are exercised by test-tab_logit.R (the binomial wrappers).

reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}

# ---- family dispatch + wrapper equivalence --------------------------------------------------

test_that("tab_reg(family='binomial') is identical to tab_logit()", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  t1 <- tab_reg(d, "married", c("race", "rincome"), family = "binomial", cleannames = FALSE)
  t2 <- tab_logit(d, "married", c("race", "rincome"), cleannames = FALSE)
  expect_equal(get_or(t1[["Married: OR"]]),     get_or(t2[["Married: OR"]]))
  expect_equal(get_pvalue(t1[["Married: OR"]]), get_pvalue(t2[["Married: OR"]]))
})

test_that("family='auto' detects binary -> binomial (message) and ambiguous integer aborts", {
  skip_if_not_installed("broom")
  d <- reg_data()
  expect_message(tab_reg(d, "married", "race", cleannames = FALSE), "binary")
  expect_error(tab_reg(d, "tvhours", "race"), "auto-detect|family")   # integer count is ambiguous
})

test_that("family='auto' detects a continuous outcome -> gaussian (message)", {
  skip_if_not_installed("broom")
  d <- reg_data() |> dplyr::mutate(score = age + 0.5)                 # non-integer -> continuous
  expect_message(col_tab <- tab_reg(d, "score", "race"), "continuous")
  expect_identical(get_type(col_tab[["score: \u03b2"]]), "coef")
})

# ---- gaussian beta: parity + additive fmt shape ---------------------------------------------

test_that("tab_reg() gaussian betas / CI / p match stats::lm; fmt uses the additive coef shape", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  t1  <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", cleannames = FALSE)
  col <- t1[["tvhours: \u03b2"]]

  expect_identical(get_type(col), "coef")
  expect_identical(get_display(col)[1], "coef")
  expect_identical(get_ci_type(col), "diff")
  expect_identical(get_color(col), "diff")
  expect_identical(get_color_signif(col), "grey_non_signif")

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  m  <- stats::lm(tvhours ~ age + race, data = dm)
  co <- summary(m)$coefficients
  tq <- stats::qt(0.975, stats::df.residual(m))
  bm <- co[, "Estimate"]
  lo <- co[, "Estimate"] - tq * co[, "Std. Error"]
  hi <- co[, "Estimate"] + tq * co[, "Std. Error"]
  pm <- co[, 4]

  keep <- !is.na(get_pvalue(col))                    # intercept + estimated coefs (ref levels NA)
  expect_equal(sum(keep), length(bm))
  expect_equal(sort(get_diff(col)[keep]),   sort(unname(bm)), tolerance = 1e-6)  # beta in `diff`
  expect_equal(sort(get_ci_inf(col)[keep]), sort(unname(lo)), tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(unname(hi)), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(unname(pm)), tolerance = 1e-6)

  # reference-level betas are 0 (the additive neutral), no CI/p; var field carries var(Y)
  ref_lvls <- is_refrow(col) & as.character(t1$var) != "Constant"
  expect_true(all(get_diff(col)[ref_lvls] == 0))
  expect_true(all(is.na(get_pvalue(col)[ref_lvls])))
  expect_equal(unique(get_var(col)), stats::var(dm$tvhours), tolerance = 1e-6)
})

test_that("gaussian beta renders raw (no % / x glyph), reference shows 0", {
  skip_if_not_installed("broom")
  t1  <- tab_reg(reg_data(), "tvhours", "race", family = "gaussian", cleannames = FALSE)
  col <- t1[["tvhours: \u03b2"]]
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
  t1  <- suppressWarnings(tab_reg(d, "tvhours", c("age", "race"), family = "poisson",
                                  cleannames = FALSE))
  col <- t1[["tvhours: IRR"]]

  expect_identical(get_type(col), "row")
  expect_identical(get_display(col)[1], "or")
  expect_identical(get_ci_type(col), "or")

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

  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_or(col)[keep]),     sort(unname(irr)), tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(unname(lo)),  tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(unname(hi)),  tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(unname(pm)),  tolerance = 1e-6)
})

# ---- exponentiate + references --------------------------------------------------------------

test_that("exponentiate=FALSE on a logit yields raw log-odds (additive coef shape)", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  col <- tab_reg(d, "married", "race", family = "binomial", exponentiate = FALSE,
                 cleannames = FALSE)[["Married: \u03b2"]]
  expect_identical(get_type(col), "coef")
  expect_identical(get_ci_type(col), "diff")

  dm <- d |> dplyr::filter(!is.na(race), !is.na(married))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  g  <- stats::glm(married ~ race, data = dm, family = stats::binomial())
  bm <- summary(g)$coefficients[, "Estimate"]                          # log-odds coefficients
  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_diff(col)[keep]), sort(unname(bm)), tolerance = 1e-6)
})

test_that("reference= relevels a factor predictor's baseline", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  t1 <- tab_reg(d, "married", "race", family = "binomial",
                reference = c(race = "White"), cleannames = FALSE)
  col <- t1[["Married: OR"]]
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
  expect_true(all(c("Married: OR", "Some TV: OR") %in% names(t1)))
})

test_that("colour: gaussian beta greys non-significant / reference, colours a large standardized beta", {
  skip_if_not_installed("broom")
  t1  <- tab_reg(reg_data(), "tvhours", c("age", "race"), family = "gaussian", cleannames = FALSE)
  col <- t1[["tvhours: \u03b2"]]
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
  col <- t1[["score: OR"]]

  expect_identical(get_type(col), "row")
  expect_identical(get_display(col)[1], "or")
  expect_identical(get_ci_type(col), "or")

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

test_that("trials=TRUE uses the observed max score; exponentiate=FALSE gives the coef shape", {
  skip_if_not_installed("broom")
  d  <- gb_data()
  # suppressWarnings: over-dispersed fixture -> the dispersion flag (asserted in test-tab_reg-footer.R).
  auto <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = TRUE,
                                   cleannames = FALSE))
  ten  <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                   cleannames = FALSE))
  expect_equal(max(d$score, na.rm = TRUE), 10L)
  expect_equal(get_or(auto[["score: OR"]]), get_or(ten[["score: OR"]]))

  b <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                exponentiate = FALSE,
                                cleannames = FALSE))[["score: \u03b2"]]
  expect_identical(get_type(b), "coef")
  expect_identical(get_ci_type(b), "diff")
})

test_that("trials errors outside the binomial family; ordinary >2-level binomial still aborts", {
  skip_if_not_installed("broom")
  d <- gb_data()
  expect_error(tab_reg(d, "score", "race", family = "poisson", trials = 10), "trials")
  expect_error(tab_reg(d, "score", "race", family = "binomial"), "binary|trials")  # no trials -> abort
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
  col <- t1[["tvhours: \u03b2"]]

  expect_true(any(grepl("poly\\(age, 2\\)", as.character(t1$var))))   # poly -> its own term block
  expect_true(any(as.character(t1$var) == "race"))                    # race still a factor block
  ref <- is_refrow(col) & as.character(t1$var) == "race"
  expect_true(all(get_diff(col)[ref] == 0))                          # factor reference level = 0

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  m  <- stats::lm(tvhours ~ race + poly(age, 2), data = dm)
  co <- summary(m)$coefficients
  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_diff(col)[keep]), sort(unname(co[, "Estimate"])), tolerance = 1e-6)
})

test_that("a compound formula with an interaction renders and exports without error", {
  skip_if_not_installed("broom")
  t1 <- tab_reg(reg_data(), tvhours ~ race * rincome, family = "gaussian", cleannames = FALSE)
  expect_s3_class(t1, "tabxplor_grouped_tab")
  expect_true(any(grepl(":", as.character(t1$var))))                  # the interaction term rows
  expect_no_error(tab_kable(t1))
  expect_no_error(tab_md(t1))
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
  t1 <- tab_reg(d, "party3", c("race", "age"), family = "multinomial", cleannames = FALSE)

  # one OR column per non-reference outcome category, "vs <ref>" in the label
  expect_true(all(c("Dem vs Ind: OR", "Rep vs Ind: OR") %in% names(t1)))
  col1 <- t1[["Dem vs Ind: OR"]]
  expect_identical(get_type(col1), "row")
  expect_identical(get_display(col1)[1], "or")
  expect_identical(get_ci_type(col1), "or")

  dm <- d |> dplyr::filter(!is.na(party3), !is.na(race), !is.na(age))
  dm$race   <- forcats::fct_drop(dm$race)
  dm$party3 <- forcats::fct_drop(dm$party3)
  m  <- nnet::multinom(party3 ~ race + age, data = dm, trace = FALSE)
  td <- broom::tidy(m)                                      # y.level, term, estimate, std.error, ...
  z  <- stats::qnorm(0.975)

  for (j in c("Dem", "Rep")) {
    tj   <- td[td$y.level == j, ]
    col  <- t1[[paste0(j, " vs Ind: OR")]]
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
  cvs <- vapply(c("Dem vs Ind: OR", "Rep vs Ind: OR"), function(nm) get_col_var(t1[[nm]])[1], character(1))
  expect_equal(length(unique(cvs)), 1L)                     # shared col_var
  expect_false(identical(unname(cvs[1]), "Dem vs Ind: OR")) # not the per-category name
})

test_that("reference= keyed by the outcome sets the multinomial baseline category", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  t1 <- tab_reg(mnl_data(), "party3", "race", family = "multinomial",
                reference = c(party3 = "Dem"), cleannames = FALSE)
  expect_true(all(c("Ind vs Dem: OR", "Rep vs Dem: OR") %in% names(t1)))
})

test_that("tab_reg() ordinal cumulative OR / CI / p match MASS::polr; single column, Constant NA", {
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  d   <- ord_data()
  t1  <- suppressWarnings(tab_reg(d, "spectrum", c("race", "age"),
                                  family = "ordinal", cleannames = FALSE))
  col <- t1[["spectrum: OR"]]
  expect_identical(get_type(col), "row")
  expect_identical(get_display(col)[1], "or")
  expect_identical(get_ci_type(col), "or")

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
  expect_warning(
    tab_reg(ord_income_data(), "income3", c("race", "age"), family = "ordinal"),
    "proportional-odds"
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

# ---- effect = "ame": average marginal effects + adjusted predictions (Phase 12e-i) ----------
# Parity is checked against marginaleffects run on the SAME model tab_reg fits (binomial: fct_rev to
# model the positive level; factor predictors fct_drop'd), aligning the AME by the "Level - Reference"
# contrast label. The composed cell is AME-first ("-8%*** (16%)") with the prediction in parentheses.

test_that("binomial AME: diff/pct/CI/p match marginaleffects; AME-first composed cell", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data()
  t1  <- tab_reg(d, "married", c("race", "age"), family = "binomial", effect = "ame",
                 cleannames = FALSE)
  col <- t1[["Married: AME"]]

  expect_identical(get_type(col), "row")
  expect_identical(get_ci_type(col), "diff")
  expect_identical(get_color(col), "diff")

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(age))
  dm$race    <- forcats::fct_drop(dm$race)
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))     # glm models P(Married)
  g   <- stats::glm(married ~ race + age, data = dm, family = stats::binomial())
  acr <- as.data.frame(marginaleffects::avg_comparisons(g, variables = "race", newdata = dm))
  aca <- as.data.frame(marginaleffects::avg_comparisons(g, variables = "age",  newdata = dm))
  ap  <- as.data.frame(marginaleffects::avg_predictions(g, by = "race", newdata = dm))

  keep <- !is.na(get_diff(col))
  expect_equal(sort(get_diff(col)[keep]),   sort(c(acr$estimate, aca$estimate)),   tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(c(acr$conf.low, aca$conf.low)),   tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(c(acr$conf.high, aca$conf.high)), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(c(acr$p.value, aca$p.value)),     tolerance = 1e-6)
  expect_equal(sort(get_pct(col)[!is.na(get_pct(col))]), sort(ap$estimate),        tolerance = 1e-6)

  # displays: reference level -> prediction only; non-ref factor -> composite AME-first; numeric -> AME
  disp   <- get_display(col)
  ref    <- which(as.character(t1$levels) == "Other" & as.character(t1$var) == "race")
  blk    <- which(as.character(t1$var) == "race" & as.character(t1$levels) == "Black")
  agerow <- which(as.character(t1$var) == "age")
  expect_identical(disp[ref],    "({pct})")
  expect_identical(disp[blk],    "{diff} ({pct})")
  expect_identical(disp[agerow], "diff")
  expect_true(is.na(get_diff(col)[ref]))                # the reference level has no marginal effect
  expect_true(is.na(get_pvalue(col)[ref]))

  # rendered cell: AME first (a "-" here), stars on the AME, adjusted prediction in parentheses.
  # stars are opt-in in format() (they show at the MAIN display; tab_reg stores the pvalue by default).
  txt <- format(col, special_formatting = TRUE, stars = TRUE)
  expect_match(trimws(txt[blk]), "^-[0-9.]+%\\*+ \\([0-9.]+%\\)$")
  expect_match(trimws(txt[ref]), "^\\([0-9.]+%\\)$")
})

test_that("gaussian AME uses the coef shape and matches marginaleffects", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data()
  col <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", effect = "ame",
                 cleannames = FALSE)[["tvhours: AME"]]
  expect_identical(get_type(col), "coef")
  expect_identical(get_ci_type(col), "diff")

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  dm$race <- forcats::fct_drop(dm$race)
  m   <- stats::lm(tvhours ~ age + race, data = dm)
  ac  <- rbind(as.data.frame(marginaleffects::avg_comparisons(m, variables = "age",  newdata = dm)),
               as.data.frame(marginaleffects::avg_comparisons(m, variables = "race", newdata = dm)))
  keep <- !is.na(get_diff(col)) & !is_refrow(col)       # reference betas are the additive neutral 0
  expect_equal(sort(get_diff(col)[keep]),   sort(ac$estimate),  tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(ac$p.value),   tolerance = 1e-6)
})

test_that("poisson AME is a raw count-change and matches marginaleffects", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data()
  # suppressWarnings: over-dispersed poisson fixture -> the dispersion flag (asserted in
  # test-tab_reg-footer.R). This test is about the AME scale and its marginaleffects parity.
  col <- suppressWarnings(tab_reg(d, "tvhours", c("age", "race"), family = "poisson",
                                  effect = "ame", cleannames = FALSE))[["tvhours: AME"]]
  expect_identical(get_type(col), "coef")

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  dm$race <- forcats::fct_drop(dm$race)
  m   <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  ac  <- rbind(as.data.frame(marginaleffects::avg_comparisons(m, variables = "age",  newdata = dm)),
               as.data.frame(marginaleffects::avg_comparisons(m, variables = "race", newdata = dm)))
  keep <- !is.na(get_diff(col)) & !is_refrow(col)
  expect_equal(sort(get_diff(col)[keep]), sort(ac$estimate), tolerance = 1e-6)
})

test_that("multinomial AME: one column per outcome category, matches marginaleffects", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  skip_if_not_installed("marginaleffects")
  d  <- mnl_data()
  t1 <- tab_reg(d, "party3", c("race", "age"), family = "multinomial", effect = "ame",
                cleannames = FALSE)
  expect_true(all(c("Ind: AME", "Dem: AME", "Rep: AME") %in% names(t1)))   # every outcome category

  dm <- d |> dplyr::filter(!is.na(party3), !is.na(race), !is.na(age))
  dm$race   <- forcats::fct_drop(dm$race)
  dm$party3 <- forcats::fct_drop(dm$party3)
  m  <- nnet::multinom(party3 ~ race + age, data = dm, trace = FALSE)
  ac <- rbind(as.data.frame(marginaleffects::avg_comparisons(m, variables = "race", newdata = dm)),
              as.data.frame(marginaleffects::avg_comparisons(m, variables = "age",  newdata = dm)))
  for (j in c("Ind", "Dem", "Rep")) {
    col  <- t1[[paste0(j, ": AME")]]
    keep <- !is.na(get_diff(col))
    acj  <- ac[ac$group == j, ]
    expect_equal(sort(get_diff(col)[keep]),   sort(acj$estimate), tolerance = 1e-6)
    expect_equal(sort(get_pvalue(col)[keep]), sort(acj$p.value),  tolerance = 1e-6)
  }
})

test_that("ordinal AME: one column per outcome category, matches marginaleffects", {
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  d  <- ord_data()
  t1 <- suppressWarnings(tab_reg(d, "spectrum", "race", family = "ordinal", effect = "ame",
                                 cleannames = FALSE))
  expect_true(all(c("Rep: AME", "Ind: AME", "Dem: AME") %in% names(t1)))

  dm <- d |> dplyr::filter(!is.na(spectrum), !is.na(race))
  dm$race <- forcats::fct_drop(dm$race)
  o  <- MASS::polr(spectrum ~ race, data = dm, Hess = TRUE, method = "logistic")
  ac <- as.data.frame(marginaleffects::avg_comparisons(o, variables = "race", newdata = dm))
  for (j in c("Rep", "Ind", "Dem")) {
    col  <- t1[[paste0(j, ": AME")]]
    keep <- !is.na(get_diff(col))
    acj  <- ac[ac$group == j, ]
    expect_equal(sort(get_diff(col)[keep]), sort(acj$estimate), tolerance = 1e-6)
  }
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
                                  effect = "ame", cleannames = FALSE))[["Married: AME"]]

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race))
  dm$race    <- forcats::fct_drop(dm$race)
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  des <- survey::svydesign(ids = ~1, weights = ~tvhours, data = dm)
  g   <- suppressWarnings(
    survey::svyglm(married ~ race, design = des, family = stats::quasibinomial()))
  ac  <- as.data.frame(marginaleffects::avg_comparisons(g, variables = "race", newdata = dm,
                                                        wts = "tvhours"))
  keep <- !is.na(get_diff(col))
  expect_equal(sort(get_diff(col)[keep]), sort(ac$estimate), tolerance = 1e-6)
})

test_that("AME tables export through every backend without error", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  t1 <- tab_reg(reg_data(), "married", c("race", "age"), family = "binomial", effect = "ame")
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

test_that("MER-at-reference (binomial): effect/prediction/CI match marginaleffects at the profile", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d   <- reg_data()
  t1  <- tab_reg(d, "married", c("race", "age"), family = "binomial", effect = "ame",
                 at = "reference", cleannames = FALSE)
  col <- t1[["Married: MER"]]                           # the label switches AME -> MER at reference
  expect_identical(get_type(col), "row")

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(age))
  dm$race    <- forcats::fct_drop(dm$race)
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  g    <- stats::glm(married ~ race + age, data = dm, family = stats::binomial())
  grid <- marginaleffects::datagrid(model = g, race = levels(dm$race)[1], age = mean(dm$age))
  acr  <- as.data.frame(marginaleffects::comparisons(g, variables = "race", newdata = grid))
  aca  <- as.data.frame(marginaleffects::comparisons(g, variables = "age",  newdata = grid))
  pg   <- marginaleffects::datagrid(model = g, race = levels(dm$race), age = mean(dm$age))
  ap   <- as.data.frame(marginaleffects::predictions(g, newdata = pg))

  keep <- !is.na(get_diff(col))
  expect_equal(sort(get_diff(col)[keep]),   sort(c(acr$estimate, aca$estimate)),   tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(c(acr$conf.low, aca$conf.low)),   tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(c(acr$p.value, aca$p.value)),     tolerance = 1e-6)
  expect_equal(sort(get_pct(col)[!is.na(get_pct(col))]), sort(ap$estimate),        tolerance = 1e-6)
})

test_that("MNL 'j vs rest' OR at the reference profile matches marginaleffects (comparison='lnor')", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  skip_if_not_installed("marginaleffects")
  d  <- mnl_data()
  t1 <- tab_reg(d, "party3", c("race", "age"), family = "multinomial", at = "reference",
                cleannames = FALSE)
  expect_true(all(c("Ind vs rest: OR", "Dem vs rest: OR", "Rep vs rest: OR") %in% names(t1)))
  expect_identical(get_ci_type(t1[["Dem vs rest: OR"]]), "or")

  dm <- d |> dplyr::filter(!is.na(party3), !is.na(race), !is.na(age))
  dm$race   <- forcats::fct_drop(dm$race)
  dm$party3 <- forcats::fct_drop(dm$party3)
  m    <- nnet::multinom(party3 ~ race + age, data = dm, trace = FALSE)
  grid <- marginaleffects::datagrid(model = m, race = levels(dm$race)[1], age = mean(dm$age))
  lc   <- rbind(
    as.data.frame(marginaleffects::comparisons(m, variables = "race", newdata = grid, comparison = "lnor")),
    as.data.frame(marginaleffects::comparisons(m, variables = "age",  newdata = grid, comparison = "lnor")))
  for (j in c("Ind", "Dem", "Rep")) {
    col  <- t1[[paste0(j, " vs rest: OR")]]
    vals <- get_or(col)[!is.na(get_or(col)) & !is_refrow(col)]   # exclude the neutral reference OR = 1
    expect_equal(sort(vals), sort(exp(lc[lc$group == j, ]$estimate)), tolerance = 1e-6)
  }
  ref <- is_refrow(t1[["Ind vs rest: OR"]]) & as.character(t1$var) == "race"
  expect_true(all(get_or(t1[["Ind vs rest: OR"]])[ref] == 1))    # reference predictor level -> OR 1
})

test_that("at='reference' is a no-op (with a message) for non-multinomial coefficients", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d <- reg_data()
  expect_message(
    t1 <- tab_reg(d, "married", "race", family = "binomial", at = "reference", cleannames = FALSE),
    "profile-independent"
  )
  t2 <- tab_reg(d, "married", "race", family = "binomial", cleannames = FALSE)
  expect_equal(get_or(t1[["Married: OR"]]), get_or(t2[["Married: OR"]]))   # identical coefficients
})

test_that("MER-at-reference exports through every backend without error", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  t1 <- tab_reg(reg_data(), "married", c("race", "age"), family = "binomial", effect = "ame",
                at = "reference")
  expect_no_error(tab_kable(t1))
  expect_no_error(tab_md(t1))
  skip_if_not_installed("nnet")
  t2 <- tab_reg(mnl_data(), "party3", "race", family = "multinomial", at = "reference")
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

test_that("K: several dependents x a list of models -> a tabxplor_tabs, one per dependent", {
  skip_if_not_installed("broom")
  d <- reg_2dep_data()
  r <- suppressWarnings(tab_reg(
    d, dependent = c("married", "widowed"),
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
  expect_setequal(openxlsx2::wb_get_sheet_names(wb), c("married", "widowed"))
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
               family = "binomial", compare = "baseline", baseline = "complete",
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
