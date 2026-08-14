# === Phase 19e (KEY 8b): the estimand surface ======================================================
#
# `effect` (which contrast) x `measure` (which effect measure) replaced a four-argument product
# (`family` x `effect` x `at` x `exponentiate`). Everything here is the contract of that move:
#
#   1. the resolver's THREE states (available / not defined / not offered), and that the message of
#      each names what the outcome DOES offer;
#   2. BYTE-IDENTITY with the retired spellings -- every one of them had an exact new equivalent, so
#      no number may move;
#   3. the two capability gaps 19e closed, checked against hand-fitted models;
#   4. the retired arguments aborting with their mapping, D25's colour refusal, D6's forwarding.

est_data <- function() {
  d <- forcats::gss_cat[!is.na(forcats::gss_cat$tvhours), ]
  d <- d[d$race != "Not applicable", ]
  d$race    <- droplevels(d$race)
  d$married <- as.integer(d$marital == "Married")
  d[seq_len(min(nrow(d), 4000)), ]
}

fmtcols <- function(t) names(t)[vapply(t, is_fmt, logical(1))]
render  <- function(t) lapply(t[fmtcols(t)], format)


# --- 1. the resolver ------------------------------------------------------------------------------

test_that("the estimand library resolves every family x contrast, and states its default", {
  for (fam in names(REG_ESTIMANDS)) {
    for (eff in REG_EFFECTS_VALUES) {
      r <- reg_estimand(fam, eff, "auto")
      expect_identical(r$status, "ok", info = paste(fam, eff))
      expect_true(r$scale %in% EST_SCALE_KEYS)
      expect_true(nzchar(r$word))
      expect_true(r$builder %in% c("coef", "ame", "vsrest"))
    }
  }
})

test_that("the three states are distinct, and each says what IS offered", {
  # available
  expect_identical(reg_estimand("binomial", "coefficient", "odds_ratio")$status, "ok")
  # not defined -- true whatever anyone implements
  imp <- reg_estimand("gaussian", "coefficient", "odds_ratio")
  expect_identical(imp$status, "impossible")
  expect_match(imp$why(), "odds")
  # not offered -- tabxplor does not build it (yet)
  expect_identical(reg_estimand("ordinal", "coefficient", "difference")$status, "not_offered")
  # and the enumerated message is generated from the table
  lines <- reg_estimand_offer_lines("binomial", "coefficient")
  expect_true(any(grepl("odds_ratio", lines)))
  expect_true(any(grepl("reg_measures", lines)))
})

test_that("the measure aliases work both ways and `log` pins its base", {
  expect_identical(reg_measure_key("OR")$measure,  "odds_ratio")
  expect_identical(reg_measure_key("IRR")$measure, "ratio")
  expect_identical(reg_measure_key("RD")$measure,  "difference")
  expect_null(reg_measure_key("nonsense"))
  # bare "log" logs the family's DEFAULT estimand; log_risk pins the modified-Poisson fit
  expect_identical(reg_estimand("binomial", "coefficient", "log")$fit,      "binomial")
  expect_identical(reg_estimand("binomial", "coefficient", "log_risk")$fit, "rr")
  # a log of an additive coefficient is not a thing, and says so rather than silently answering
  expect_identical(reg_estimand("gaussian", "coefficient", "log")$status, "impossible")
})

test_that("reg_measures() lists an outcome's estimands with a status for each", {
  skip_if_not_installed("broom")
  m <- suppressMessages(reg_measures(est_data(), "married"))
  expect_true(all(c("effect", "measure", "status", "header") %in% names(m)))
  expect_setequal(unique(m$status), c("available", "not offered"))
  expect_identical(m$header[m$effect == "coefficient" & m$measure == "odds_ratio"], "Model_OR")
  # a continuous outcome is where the THIRD state shows: an odds ratio of a mean is not a thing
  g <- suppressMessages(reg_measures(est_data(), "tvhours"))
  expect_identical(g$status[g$effect == "coefficient" & g$measure == "odds_ratio"], "not defined")
  expect_match(g$note[g$effect == "coefficient" & g$measure == "odds_ratio"], "odds")
  # the generated ?tab_reg section reads the same table
  expect_true(any(grepl("Model_OR", reg_measures_rd())))
})


# --- 2. byte-identity with the retired spellings --------------------------------------------------

test_that("measure = 'ratio' on a binary outcome == the old family = 'poisson' route", {
  skip_if_not_installed("broom")
  skip_if_not_installed("survey")
  d <- est_data()
  a <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                measure = "ratio", empirical = TRUE, cleannames = FALSE))
  b <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "poisson",
                                empirical = TRUE, cleannames = FALSE))
  expect_identical(names(a), names(b))
  expect_identical(render(a), render(b))
})

test_that("measure = 'log' == the old exponentiate = FALSE", {
  skip_if_not_installed("broom")
  d <- est_data()
  t <- suppressMessages(tab_reg(d, "married", c("race"), family = "binomial", measure = "log",
                                cleannames = FALSE))
  cf <- t[[grep("^Model", names(t), value = TRUE)[1]]]
  expect_identical(get_scale(cf), "log_coef")
  # the same numbers as the exponentiated column, logged
  e <- suppressMessages(tab_reg(d, "married", c("race"), family = "binomial", cleannames = FALSE))
  ec <- e[[grep("^Model", names(e), value = TRUE)[1]]]
  keep <- !is.na(get_or(ec)) & get_or(ec) > 0
  expect_equal(get_diff(cf)[keep], log(get_or(ec)[keep]), tolerance = 1e-10)
})

test_that("effect = 'marginal' + measure = 'ratio' is the old ame_ratio, exponentiated lnratioavg", {
  skip_if_not_installed("marginaleffects")
  d <- est_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  col <- t[[grep("^Model", names(t), value = TRUE)[1]]]
  expect_identical(get_scale(col), "odds_ratio")
  g  <- stats::glm(married ~ race, data = d, family = stats::binomial())
  m  <- marginaleffects::avg_comparisons(g, variables = "race", comparison = "lnratioavg")
  i  <- which(as.character(t$var) == "race" & !is_refrow(col))
  expect_equal(sort(get_or(col)[i]), sort(exp(m$estimate)), tolerance = 1e-8)
})


# --- 3. the two capability gaps 19e closed --------------------------------------------------------

test_that("measure = 'difference' on a binary outcome is the identity-link risk difference", {
  skip_if_not_installed("broom")
  skip_if_not_installed("survey")
  d  <- est_data()
  t  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                 measure = "difference", empirical = TRUE, cleannames = FALSE))
  mc <- t[[grep("^Model", names(t), value = TRUE)[1]]]
  expect_identical(get_scale(mc), "points")            # percentage points, not a ratio
  expect_true("Model_RD" %in% names(t))
  expect_true("Obs_diff" %in% names(t))                # the crude twin is the crude risk difference
  # the coefficients ARE an identity-link binomial glm's
  dd <- stats::na.omit(d[, c("married", "race")])
  g  <- suppressWarnings(stats::glm(married ~ race, data = dd, family = stats::binomial("identity"),
                                    start = stats::coef(stats::lm(married ~ race, dd))))
  i  <- which(as.character(t$var) == "race" & !is_refrow(mc))
  expect_equal(sort(get_diff(mc)[i]), sort(unname(stats::coef(g))[-1]), tolerance = 1e-6)
})

test_that("measure = 'ratio' on a continuous outcome is the ratio of adjusted means", {
  skip_if_not_installed("broom")
  skip_if_not_installed("survey")
  d  <- est_data()
  t  <- suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian",
                                 measure = "ratio", empirical = TRUE, cleannames = FALSE))
  mc <- t[[grep("^Model", names(t), value = TRUE)[1]]]
  expect_identical(get_scale(mc), "mean_ratio")        # the ratio field, not `or`
  expect_true("Obs_MR" %in% names(t))
  dd <- stats::na.omit(d[, c("tvhours", "race")])
  g  <- suppressWarnings(stats::glm(tvhours ~ race, data = dd, family = stats::quasipoisson("log")))
  i  <- which(as.character(t$var) == "race" & !is_refrow(mc))
  expect_equal(sort(get_ratio(mc)[i]), sort(exp(unname(stats::coef(g)))[-1]), tolerance = 1e-6)
  # a negative outcome has no ratio of means, and says so rather than fitting nonsense
  d2 <- d; d2$neg <- d2$tvhours - 5
  expect_error(suppressMessages(tab_reg(d2, "neg", "race", family = "gaussian", measure = "ratio")),
               "non-negative")
})

test_that("a marginal ratio is available for a count outcome too", {
  skip_if_not_installed("marginaleffects")
  d <- est_data()
  t <- suppressMessages(tab_reg(d, "tvhours", "race", family = "poisson",
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  mc <- t[[grep("^Model", names(t), value = TRUE)[1]]]
  expect_identical(get_scale(mc), "mean_ratio")
})


# --- 4. the retired surface, D25 and D6 -----------------------------------------------------------

test_that("every retired estimand argument aborts naming its replacement", {
  d <- est_data()
  expect_error(tab_reg(d, "married", "race", family = "binomial", exponentiate = FALSE),
               'measure = "log"')
  expect_error(tab_reg(d, "married", "race", family = "binomial", at = "reference"),
               "at_reference")
  expect_error(tab_reg(d, "married", "race", family = "binomial", estimate_display = "ci"),
               "display")
  expect_error(tab_reg(d, "married", "race", family = "binomial", effect = "ame"),
               "marginal")
  expect_error(tab_reg(d, "married", "race", family = "binomial", effect = "ame_ratio"),
               'measure = "ratio"')
  # an unknown measure enumerates the legal ones
  expect_error(tab_reg(d, "married", "race", family = "binomial", measure = "nonsense"),
               "odds_ratio")
})

test_that("D25: a reg colour cannot contradict what the column estimates", {
  d <- est_data()
  expect_error(tab_reg(d, "married", "race", family = "binomial", color = "difference"),
               "measure")
  expect_error(tab_reg(d, "married", "race", family = "binomial", color = "odds_ratio"),
               "adjustment")
  # what remains is what to compare the effect TO
  expect_identical(reg_normalize_color(TRUE),  NA_character_)
  expect_identical(reg_normalize_color(FALSE), "no")
  expect_identical(reg_normalize_color(c(TRUE, "adjustment")), c(NA_character_, "adjustment"))
})

test_that("D6: the multi-dependent x model-list recursion forwards every argument", {
  skip_if_not_installed("broom")
  d <- est_data()
  tabs <- suppressMessages(tab_reg(
    d, c("married", "tvhours"), list(m1 = "race", m2 = c("race", "age")),
    family = c("binomial", "gaussian"), cleannames = FALSE))
  expect_length(tabs, 2L)
  # ... and a POSITIONAL family vector reached each recursion whole, so the second outcome was
  # fitted with the first's family
  expect_identical(get_model_family(tabs[[1]][[grep("^m1", names(tabs[[1]]))[1]]]), "binomial")
  expect_identical(get_model_family(tabs[[2]][[grep("^m1", names(tabs[[2]]))[1]]]), "gaussian")
})

test_that("the estimand is stored in the table's recipe, per dependent", {
  skip_if_not_installed("broom")
  d <- est_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", measure = "ratio",
                                cleannames = FALSE))
  rc <- reg_call(t)
  expect_identical(rc$measure, "ratio")
  expect_identical(rc$effect,  "coefficient")
  expect_identical(unname(rc$measures[["married"]]), "ratio")
  # ... and the footer sentence is generated from it
  expect_match(reg_model_lines(t)[[1]], "risk ratio|rapports de risque")
})
