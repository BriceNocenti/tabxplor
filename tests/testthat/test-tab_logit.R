# Phase 12a: tab_logit() / multi_logit() -- odds-ratio tables from binary logit models.
# Statistical soundness is checked against a hand-run stats::glm / survey::svyglm + the log-OR
# Wald interval that tab_logit stores (the exact dual of the Wald p-value).

logit_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}

test_that("tab_logit() returns a grouped odds-ratio tab with the right structure", {
  skip_if_not_installed("broom")
  t1 <- tab_logit(logit_data(), "married", c("race", "rincome"))

  expect_s3_class(t1, "tabxplor_grouped_tab")
  expect_identical(dplyr::group_vars(t1), "var")
  expect_true("Married: OR" %in% names(t1))
  col <- t1[["Married: OR"]]
  expect_true(is_fmt(col))
  expect_identical(get_display(col)[1], "or")
  expect_identical(get_type(col), "row")
  expect_identical(get_ci_type(col), "or")
  expect_identical(get_color(col), "OR")
  expect_identical(get_color_signif(col), "grey_non_signif")
  # reference rows (factor baselines) carry OR == 1, no CI/p
  ref_lvls <- is_refrow(col) & as.character(t1$var) != "Constant"
  expect_true(all(get_or(col)[ref_lvls] == 1))
  expect_true(all(is.na(get_pvalue(col)[ref_lvls])))
})

test_that("tab_logit() odds ratios / CI / p match stats::glm (unweighted)", {
  skip_if_not_installed("broom")
  data <- logit_data()
  t1   <- tab_logit(data, "married", c("race", "rincome"), cleannames = FALSE)
  col  <- t1[["Married: OR"]]

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

test_that("tab_logit() matches survey::svyglm with survey weights", {
  skip_if_not_installed("broom")
  skip_if_not_installed("survey")
  data <- logit_data() |>
    dplyr::filter(!is.na(tvhours)) |>
    dplyr::mutate(w = tvhours + 1)                 # strictly positive weights
  col <- tab_logit(data, "married", c("race", "rincome"), wt = "w",
                   cleannames = FALSE)[["Married: OR"]]

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

test_that("colour: grey_non_signif greys CI-includes-1 cells, colours large significant OR", {
  skip_if_not_installed("broom")
  t1  <- tab_logit(logit_data(), "married", c("race", "rincome"), cleannames = FALSE)
  col <- t1[["Married: OR"]]
  txt <- fmt_color_channels(col)$text
  sig <- !is.na(get_ci_inf(col)) & (get_ci_inf(col) > 1 | get_ci_sup(col) < 1)

  expect_true(all(txt[!sig] == 0))                 # only significant cells coloured
  expect_true(all(txt[is_refrow(col)] == 0))       # reference cells uncoloured
  big <- which(as.character(t1$levels) == "Black") # OR ~ 1/2.36, p < 0.001, |effect| > break
  expect_true(txt[big] != 0)
})

test_that("multi_logit() puts one OR column per model, blank where a predictor is absent", {
  skip_if_not_installed("broom")
  t2 <- multi_logit(logit_data(), "married",
                    models = list(demographic = c("race", "age"),
                                  full        = c("race", "age", "rincome")))
  expect_s3_class(t2, "tabxplor_grouped_tab")
  expect_true(all(c("demographic", "full") %in% names(t2)))

  # rincome rows exist (union skeleton) but are empty in the demographic column
  rincome_rows <- as.character(t2$var) == "rincome"
  expect_true(any(rincome_rows))
  expect_true(all(is.na(get_or(t2[["demographic"]])[rincome_rows])))
  expect_false(all(is.na(get_or(t2[["full"]])[rincome_rows])))
})

test_that("a 3+ level dependent errors cleanly", {
  skip_if_not_installed("broom")
  expect_error(tab_logit(logit_data(), "marital", "race"), "binary|2 level")
})

test_that("tab_logit() output exports through every backend without error", {
  skip_if_not_installed("broom")
  t1 <- tab_logit(logit_data(), "married", c("race", "rincome"))
  expect_no_error(tab_kable(t1))
  expect_no_error(tab_md(t1))
  skip_if_not_installed("openxlsx2")
  xf <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(tab_xl(t1, path = xf, replace = TRUE))
  expect_true(file.exists(xf))
})

test_that("1/OR display renders OR < 1 as a reciprocal", {
  skip_if_not_installed("broom")
  t1  <- tab_logit(logit_data(), "married", c("race", "rincome"), cleannames = FALSE)
  txt <- format(t1[["Married: OR"]], special_formatting = TRUE)
  i <- which(as.character(t1$levels) == "Black")   # OR well below 1
  expect_match(txt[i], "^1/")
})

test_that("method = 'profile' uses profile-likelihood CI + LR-test p (dual)", {
  skip_if_not_installed("broom")
  skip_if_not_installed("MASS")
  data <- logit_data()
  col  <- tab_logit(data, "married", c("race", "rincome"), method = "profile",
                    cleannames = FALSE)[["Married: OR"]]

  d <- data |> dplyr::filter(!is.na(race), !is.na(rincome), !is.na(married))
  d$married <- forcats::fct_rev(forcats::fct_drop(factor(d$married)))
  g  <- stats::glm(married ~ race + rincome, data = d, family = stats::binomial())
  ci <- suppressMessages(exp(stats::confint(g)))                 # profile CI

  keep <- !is.na(get_pvalue(col))
  expect_equal(sort(get_ci_inf(col)[keep]), sort(unname(ci[, 1])), tolerance = 1e-4)
  expect_equal(sort(get_ci_sup(col)[keep]), sort(unname(ci[, 2])), tolerance = 1e-4)

  # CI and LR p are exact duals: a clearly significant term excludes 1, a clearly non-sig one includes it
  p     <- get_pvalue(col)[keep]
  excl1 <- get_ci_inf(col)[keep] > 1 | get_ci_sup(col)[keep] < 1
  expect_true(all(excl1[p < 0.01]))
  expect_true(all(!excl1[p > 0.10]))
})

test_that("method = 'profile' falls back to Wald for weighted models (with a message)", {
  skip_if_not_installed("broom")
  skip_if_not_installed("survey")
  data <- logit_data() |>
    dplyr::filter(!is.na(tvhours)) |>
    dplyr::mutate(w = tvhours + 1)
  expect_message(
    tw <- tab_logit(data, "married", c("race", "rincome"), wt = "w",
                    method = "profile", cleannames = FALSE),
    "not defined for survey"
  )
  colw <- tab_logit(data, "married", c("race", "rincome"), wt = "w",
                    method = "wald", cleannames = FALSE)[["Married: OR"]]
  expect_equal(get_ci_inf(tw[["Married: OR"]]), get_ci_inf(colw))
  expect_equal(get_pvalue(tw[["Married: OR"]]), get_pvalue(colw))
})

test_that("color_signif = 'ignore' colours non-significant odds ratios too", {
  skip_if_not_installed("broom")
  col <- tab_logit(logit_data(), "married", c("race", "rincome"),
                   color_signif = "ignore", cleannames = FALSE)[["Married: OR"]]
  expect_identical(get_color_signif(col), "ignore")

  txt <- fmt_color_channels(col)$text
  orr <- get_or(col)
  mag <- ifelse(orr >= 1, orr, 1 / orr)
  sig <- !is.na(get_ci_inf(col)) & (get_ci_inf(col) > 1 | get_ci_sup(col) < 1)
  cand <- which(!is.na(orr) & mag > 1.16 & !sig & !is_refrow(col))   # non-sig but large |OR|
  skip_if(length(cand) == 0)
  expect_true(all(txt[cand] != 0))                                   # coloured under "ignore"
})
