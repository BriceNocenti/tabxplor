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
