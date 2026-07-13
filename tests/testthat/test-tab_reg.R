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
  t1  <- tab_reg(d, "tvhours", c("age", "race"), family = "poisson", cleannames = FALSE)
  col <- t1[["tvhours: IRR"]]

  expect_identical(get_type(col), "row")
  expect_identical(get_display(col)[1], "or")
  expect_identical(get_ci_type(col), "or")

  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  m  <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  co <- summary(m)$coefficients
  z  <- stats::qnorm(0.975)
  irr <- exp(co[, 1])
  lo  <- exp(co[, 1] - z * co[, 2])
  hi  <- exp(co[, 1] + z * co[, 2])
  pm  <- co[, 4]

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
  t1  <- tab_reg(d, "score", "race", family = "binomial", trials = 10, cleannames = FALSE)
  col <- t1[["score: OR"]]

  expect_identical(get_type(col), "row")
  expect_identical(get_display(col)[1], "or")
  expect_identical(get_ci_type(col), "or")

  dm <- d |> dplyr::filter(!is.na(score), !is.na(race))
  dm$race <- forcats::fct_drop(factor(dm$race))
  g  <- stats::glm(cbind(score, 10 - score) ~ race, data = dm, family = stats::binomial())
  co <- summary(g)$coefficients
  z  <- stats::qnorm(0.975)

  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_or(col)[keep]),     sort(exp(unname(co[, 1]))),          tolerance = 1e-6)
  expect_equal(sort(get_ci_inf(col)[keep]), sort(exp(unname(co[, 1] - z * co[, 2]))), tolerance = 1e-6)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(exp(unname(co[, 1] + z * co[, 2]))), tolerance = 1e-6)
  expect_equal(sort(get_pvalue(col)[keep]), sort(unname(co[, 4])),               tolerance = 1e-6)
  # the first race level is the reference (OR == 1, no CI/p)
  ref <- is_refrow(col) & as.character(t1$var) == "race"
  expect_true(all(get_or(col)[ref] == 1))
})

test_that("trials=TRUE uses the observed max score; exponentiate=FALSE gives the coef shape", {
  skip_if_not_installed("broom")
  d  <- gb_data()
  auto <- tab_reg(d, "score", "race", family = "binomial", trials = TRUE, cleannames = FALSE)
  ten  <- tab_reg(d, "score", "race", family = "binomial", trials = 10,   cleannames = FALSE)
  expect_equal(max(d$score, na.rm = TRUE), 10L)
  expect_equal(get_or(auto[["score: OR"]]), get_or(ten[["score: OR"]]))

  b <- tab_reg(d, "score", "race", family = "binomial", trials = 10, exponentiate = FALSE,
               cleannames = FALSE)[["score: \u03b2"]]
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

test_that("weighted multinomial / ordinal are deferred (error)", {
  skip_if_not_installed("broom")
  expect_error(
    tab_reg(mnl_data(), "party3", "race", family = "multinomial", wt = "tvhours"),
    "not yet supported"
  )
  expect_error(
    tab_reg(ord_data(), "spectrum", "race", family = "ordinal", wt = "tvhours"),
    "not yet supported"
  )
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
