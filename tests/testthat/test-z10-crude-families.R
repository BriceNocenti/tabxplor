# PURPOSE: Phase 18z10 -- the observed (crude) counterpart of the three families that had none:
#          grouped binomial (`trials =`), multinomial and ordinal; and the model-vs-observed gap TEST
#          on their marginal paths.
# ROLE: the behavioural lock. The governing claim is ONE rule, not three features: "the observed effect
#       is the model's own effect, fitted with ONE predictor". Where that univariable model is
#       SATURATED it has a closed form (grouped binomial, multinomial); where it is not (ordinal:
#       proportional odds is a constraint) it is a real fit. So every assertion below compares the
#       crude column to a UNIVARIABLE FIT of the same family, never to a hand-written expectation.
# KEY CONSTRAINTS:
#   - The coefficient-scale gap test stays BLOCKED for multinomial / ordinal (and binomial), by the
#     pre-existing collapsibility gate: a conditional odds ratio moves under adjustment with zero
#     confounding. Only `effect = "ame"` / `"ame_ratio"` carry a real test.
#   - A multinomial crude effect is per OUTCOME CATEGORY and rides IN-CELL (`{or} ({obs})`), because one
#     Obs_* column per category would double the table's width.
#   - `reg_empirical()` and `reg_empirical_tips()` were the same computation at two key widths; the
#     merged grid must reproduce BOTH.
# See: dev/model_vs_observed_gap_test.md SS13.

z10_data <- function() {
  d <- forcats::gss_cat
  d$race   <- forcats::fct_drop(d$race)
  d$party3 <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                      grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                      TRUE ~ "Ind"),
                     levels = c("Ind", "Dem", "Rep"))
  d$mar3   <- factor(dplyr::case_when(d$marital == "Married"       ~ "M",
                                      d$marital == "Never married" ~ "N", TRUE ~ "O"),
                     levels = c("N", "M", "O"))
  d$ord    <- factor(dplyr::case_when(d$age < 35 ~ "lo", d$age < 55 ~ "mid", TRUE ~ "hi"),
                     levels = c("lo", "mid", "hi"), ordered = TRUE)
  d$score  <- pmin(as.integer(d$tvhours), 10L)
  tibble::as_tibble(tidyr::drop_na(d[, c("race", "party3", "mar3", "ord", "score")]))
}


# --- the stored fact that replaced six inferences ---------------------------------------------------

test_that("crude_key is stored per outcome, and NA only where there is genuinely no counterpart", {
  expect_identical(tabxplor:::reg_crude_key("binomial", NULL, FALSE),    "binomial")
  expect_identical(tabxplor:::reg_crude_key("binomial", 10L,  FALSE),    "grouped_binomial")
  expect_identical(tabxplor:::reg_crude_key("quasipoisson", NULL, FALSE), "poisson")
  expect_identical(tabxplor:::reg_crude_key("multinomial", NULL, FALSE), "multinomial")
  expect_identical(tabxplor:::reg_crude_key("ordinal", NULL, FALSE),     "ordinal")
  expect_true(is.na(tabxplor:::reg_crude_key("binomial", NULL, TRUE)))   # compound formula

  d <- z10_data()
  t <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE))
  expect_identical(unname(get_reg_meta(t)$crude_keys), "multinomial")
})


# --- grouped binomial (`trials =`) ------------------------------------------------------------------

test_that("a grouped binomial's crude OR is the univariable glm(cbind(s, q - s))", {
  skip_if_not_installed("broom")
  d <- z10_data()
  t <- suppressWarnings(suppressMessages(
    tab_reg(d, "score", c("race", "mar3"), family = "binomial", trials = 10, empirical = TRUE,
            cleannames = FALSE)))
  expect_true(all(c("Obs_mean", "Obs_OR") %in% names(t)))

  uni <- stats::glm(cbind(score, 10L - score) ~ race, data = d, family = stats::binomial())
  lv  <- levels(d$race)[-1]
  i   <- match(lv, as.character(t$levels)[as.character(t$var) == "race"])
  got <- get_or(t[["Obs_OR"]])[as.character(t$var) == "race"][i]
  expect_equal(unname(got), unname(exp(stats::coef(uni))[-1]), tolerance = 1e-6)
})

test_that("the grouped binomial's BASE column is the mean SCORE, not a share of respondents", {
  d <- z10_data()
  t <- suppressWarnings(suppressMessages(
    tab_reg(d, "score", "race", family = "binomial", trials = 10, empirical = TRUE,
            cleannames = FALSE)))
  want <- as.vector(tapply(d$score, d$race, mean))
  names(want) <- levels(d$race)
  k    <- as.character(t$var) == "race"
  got  <- get_mean(t[["Obs_mean"]])[k]
  expect_equal(unname(got), unname(want[as.character(t$levels)[k]]), tolerance = 1e-8)
  expect_gt(max(want), 1)                       # a SCORE out of 10, not a proportion in [0, 1]
  expect_identical(tabxplor:::fmt_var_kind(t[["Obs_mean"]]), "mean")
})


# --- multinomial -------------------------------------------------------------------------------------

test_that("the multinomial crude OR is the one tab(pct = 'row', OR = 'OR') prints", {
  skip_if_not_installed("nnet")
  d <- z10_data()
  t  <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE,
                                 cleannames = FALSE))
  ct <- tab(d, race, party3, pct = "row", display = "{or}", ref = "first", na = "drop", ref2 = 1)
  lv <- levels(d$race)

  seen <- 0L
  for (nm in setdiff(names(t), c("var", "levels"))) {
    cat_j <- sub(" vs .*$", "", nm)
    if (!cat_j %in% names(ct)) next
    got  <- get_obs(t[[nm]])[match(lv, as.character(t$levels))]
    want <- get_or(ct[[cat_j]])[match(lv, as.character(ct[[1]]))]
    expect_equal(unname(got), unname(want), tolerance = 1e-8)
    seen <- seen + 1L
  }
  expect_gt(seen, 1L)
})

test_that("multinomial draws NO Obs_* column: the crude number rides in-cell", {
  skip_if_not_installed("nnet")
  d <- z10_data()
  t <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE,
                                cleannames = FALSE))
  expect_false(any(grepl("^Obs_", names(t))))
  x <- t[[reg_fmt_cols(t)[[1]]]]
  expect_true(any(grepl("{obs}", get_display(x), fixed = TRUE)))
  # ... and the rendered cell really shows two numbers
  expect_true(any(grepl("(", format(x), fixed = TRUE)))
  # the footer names the bracket, so the reader is told what it is
  expect_match(paste(tabxplor:::reg_model_lines(t, lang = "en"), collapse = " "),
               "observed", fixed = TRUE)
})

test_that("the in-cell fold does not duplicate itself in the html tooltip", {
  skip_if_not_installed("nnet")
  d <- z10_data()
  t <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE,
                                cleannames = FALSE))
  tips <- tabxplor:::tab_kable_print_tooltip(t[[reg_fmt_cols(t)[[1]]]])
  expect_false(any(grepl("obs:", tips, fixed = TRUE)))     # already in the cell
})

test_that("fmt_display_shows reads the WHOLE template, not just the first token", {
  # the gate that made the line above possible (and fixed a shipping duplication: an AME column
  # displaying "{diff} ({pct})" repeated its own bracket on hover).
  expect_true (tabxplor:::fmt_display_shows("{diff} ({pct})", "pct"))
  expect_true (tabxplor:::fmt_display_shows("{diff} ({pct})", "diff"))
  expect_false(tabxplor:::fmt_display_shows("{diff} ({pct})", "obs"))
  expect_true (tabxplor:::fmt_display_shows("pct", "pct"))
  expect_false(display_primary("{diff} ({pct})") == "pct")   # the old, first-token-only rule
})


# --- ordinal ------------------------------------------------------------------------------------------

test_that("the ordinal crude is a UNIVARIABLE proportional-odds fit (no closed form exists)", {
  skip_if_not_installed("MASS")
  d <- z10_data()
  t <- suppressWarnings(suppressMessages(
    tab_reg(d, "ord", c("race", "mar3"), empirical = TRUE, cleannames = FALSE)))
  expect_true("Obs_cumOR" %in% names(t))

  for (v in c("race", "mar3")) {
    uni <- suppressWarnings(MASS::polr(stats::as.formula(paste("ord ~", v)), data = d))
    k   <- as.character(t$var) == v & !is_refrow(t[["Obs_cumOR"]])
    expect_equal(unname(get_or(t[["Obs_cumOR"]])[k]), unname(exp(stats::coef(uni))),
                 tolerance = 1e-6)
    expect_gt(sum(k), 0L)
  }
  # an ordinal cumulative OR has no single base share, so it emits ONE column, not two
  expect_false(any(grepl("^Obs_(mean|%|rate)", names(t))))
})


# --- the gap test: which paths get one, and which correctly degrade -----------------------------------

test_that("the marginal paths of a 3+ level outcome get a real gap SE", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("marginaleffects")
  d <- z10_data()
  for (eff in c("ame", "ame_ratio")) {
    t <- suppressMessages(tab_reg(d, "party3", c("race", "mar3"), family = "multinomial",
                                  effect = eff, empirical = TRUE,
                                  color = c("OR", "adjustment"), cleannames = FALSE))
    g <- get_gap_se(t[[reg_fmt_cols(t)[[1]]]])
    expect_true(any(!is.na(g)))
    expect_true(all(g[!is.na(g)] > 0))
    # every non-reference, in-model row is covered
    expect_equal(sum(!is.na(g)), sum(!is_refrow(t[[reg_fmt_cols(t)[[1]]]]) & as.character(t$var) != "Constant"))
  }
})

test_that("the coefficient path of a 3+ level outcome stays blocked (non-collapsibility)", {
  skip_if_not_installed("nnet")
  d <- z10_data()
  t <- suppressMessages(tab_reg(d, "party3", c("race", "mar3"), family = "multinomial",
                                empirical = TRUE, color = c("OR", "adjustment"),
                                cleannames = FALSE))
  expect_true(all(is.na(get_gap_se(t[[reg_fmt_cols(t)[[1]]]]))))               # obs yes, test no
  expect_true(any(!is.na(get_obs(t[[reg_fmt_cols(t)[[1]]]]))))
  # ... so the column reads under `ignore`, whatever policy was asked for (an all-NA gap_se is what
  # MEASURES$adjustment$force_policy reads as "no test here").
  expect_identical(tabxplor:::resolve_color_channel_plans(t[[reg_fmt_cols(t)[[1]]]])$bg$policy, "ignore")
})

test_that("the multinom / polr scores are the right ones (colSums 0, SE near the model's)", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  d <- z10_data()
  m <- nnet::multinom(party3 ~ race + mar3, data = d, trace = FALSE)
  sc <- tabxplor:::reg_score_multinom(m)
  expect_false(is.null(sc))
  expect_identical(colnames(sc$S), rownames(stats::vcov(m)))   # the category-major ordering trap
  expect_lt(max(abs(colSums(sc$S))), 1e-1)                     # a score is 0 at the MLE
  se_if <- sqrt(colSums((sc$S %*% sc$bread)^2))
  expect_equal(unname(se_if), unname(sqrt(diag(sc$bread))), tolerance = 0.05)

  p  <- suppressWarnings(MASS::polr(ord ~ race + mar3, data = d, Hess = TRUE))
  sp <- tabxplor:::reg_score_polr(p)
  expect_false(is.null(sp))
  expect_identical(colnames(sp$S), rownames(stats::vcov(p)))
  expect_lt(max(abs(colSums(sp$S))), 1e-1)
  expect_equal(unname(sqrt(colSums((sp$S %*% sp$bread)^2))), unname(sqrt(diag(sp$bread))),
               tolerance = 0.06)
  # TRAP 1: solve(fit$Hessian) is NOT the bread -- polr optimises over (beta, zeta1, log d zeta)
  expect_false(isTRUE(all.equal(unname(solve(p$Hessian)), unname(stats::vcov(p)),
                                tolerance = 1e-3)))
})

test_that("the local AME reproduces marginaleffects exactly (the duplication is policed)", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("marginaleffects")
  d   <- z10_data()
  m   <- nnet::multinom(party3 ~ race + mar3, data = d, trace = FALSE)
  eng <- tabxplor:::reg_prob_engine(m)
  expect_false(is.null(eng))
  me  <- as.data.frame(marginaleffects::avg_comparisons(m, variables = "race", newdata = d))
  me  <- me[me$group == "Rep", , drop = FALSE]
  ref <- levels(d$race)[1]
  seen <- 0L
  for (lv in levels(d$race)[-1]) {
    d1 <- d; d1$race <- factor(lv,  levels = levels(d$race))
    d0 <- d; d0$race <- factor(ref, levels = levels(d$race))
    ours <- mean(eng$probs(eng$theta, eng$mm(d1))[, "Rep"] -
                   eng$probs(eng$theta, eng$mm(d0))[, "Rep"])
    r <- me[grepl(lv, me$contrast, fixed = TRUE), , drop = FALSE]
    expect_equal(ours, r$estimate[[1]], tolerance = 1e-8)
    seen <- seen + 1L
  }
  expect_gt(seen, 0L)
})

test_that("the closed-form crude leg of a multinomial AME is the textbook risk-difference SE", {
  d  <- z10_data()
  ci <- tabxplor:::reg_crude_if_maker(d, "party3", "multinomial", NULL, NULL, "identity",
                                      category = "Rep", ref_category = "Ind")
  expect_false(is.null(ci))
  lv <- levels(d$race)[2]; rf <- levels(d$race)[1]
  m1 <- d$race == lv; m0 <- d$race == rf
  p1 <- mean(d$party3[m1] == "Rep"); p0 <- mean(d$party3[m0] == "Rep")
  expect_equal(tabxplor:::reg_if_se(ci("race", lv, rf)),
               sqrt(p1 * (1 - p1) / sum(m1) + p0 * (1 - p0) / sum(m0)), tolerance = 1e-10)
})


# --- the merged grid reproduces both former producers --------------------------------------------------

test_that("the merged crude grid carries the tooltip quantities AND the binary ones", {
  d <- z10_data()
  g <- tabxplor:::reg_empirical(d, "race", "party3", "multinomial", NULL, NULL,
                                ref_category = "Ind")
  expect_true(all(c("var", "level", "category", "emp_prop", "emp_prop_inf", "emp_prop_sup",
                    "emp_diff", "emp_diff_inf", "emp_diff_sup", "emp_ratio") %in% names(g)))
  # the categorical part IS the weighted share, level by level (what the tooltip prints)
  for (l in levels(d$race)) for (k in levels(d$party3)) {
    want <- mean(d$party3[d$race == l] == k)
    got  <- g$emp_prop[g$var == "race" & g$level == l & g$category == k]
    expect_equal(got, want, tolerance = 1e-12)
  }
  # a binary outcome's grid is the same computation at K = 2
  d$bin <- factor(as.integer(d$party3 == "Rep"), labels = c("no", "yes"))
  gb <- tabxplor:::reg_empirical(d, "race", "bin", "binomial", "yes", NULL)
  k  <- gb$var == "race" & gb$category == "1"
  want <- as.vector(tapply(d$bin == "yes", d$race, mean)); names(want) <- levels(d$race)
  expect_equal(gb$emp_prop[k], unname(want[gb$level[k]]), tolerance = 1e-12)
})
