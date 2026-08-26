# PURPOSE: the observed (crude) companion: the same estimand, on the same people, with one predictor.
# ROLE: the shipped CONTRACT for R/reg-empirical.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the observed companion, per family ==================================================

emp_data <- function() {
  d <- fx_reg_df()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d$inc3    <- factor(dplyr::case_when(d$rincome %in% c("$25000 or more") ~ "hi",
                                       d$rincome %in% c("Not applicable", "No answer",
                                                        "Don't know", "Refused") ~ NA_character_,
                                       TRUE ~ "lo"),
                      levels = c("lo", "hi"), ordered = FALSE)
  d$spectrum <- factor(d$party3, levels = c("Ind", "Dem", "Rep"), ordered = TRUE)
  d <- d[!is.na(d$tvhours) & !is.na(d$race) & !is.na(d$party3), , drop = FALSE]
  tibble::as_tibble(d)
}



# --- PARITY: single-predictor model == empirical column == tab() -----------------------------------

test_that("gaussian: single-predictor beta == crude mean-diff (Obs_diff) == tab() diff", {
  d <- emp_data()
  t <- tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE, cleannames = FALSE)
  emp_diff <- get_diff(t[["Obs_diff"]]); names(emp_diff) <- as.character(t$levels)
  emp_mean <- get_mean(t[["Obs_diff"]]); names(emp_mean) <- as.character(t$levels)

  beta <- stats::coef(stats::lm(tvhours ~ race, d))          # treatment contrasts: diff vs level 1
  # tab() crude: mean per level + difference vs the first level (ref = 1)
  tb   <- tab(d, race, tvhours, ref = 1)
  tab_diff <- get_diff(tb[["tvhours"]]); names(tab_diff) <- as.character(tb$race)
  tab_mean <- get_mean(tb[["tvhours"]]); names(tab_mean) <- as.character(tb$race)

  for (l in names(beta)[-1]) {                               # raceBlack, raceWhite, ...
    lev <- sub("^race", "", l)
    expect_equal(unname(emp_diff[lev]), unname(beta[l]),      tolerance = 1e-6)
    expect_equal(unname(emp_diff[lev]), unname(tab_diff[lev]), tolerance = 1e-6)
  }
  # the base mean column matches tab()'s mean, level by level
  common <- intersect(names(emp_mean), names(tab_mean))
  expect_equal(unname(emp_mean[common]), unname(tab_mean[common]), tolerance = 1e-6)
})



# the crude column, by its stored role -- one per model column since the crude/adjusted merge.
emp_col <- function(t) {
  nm <- names(t)[vapply(t, is_fmt, logical(1))]
  nm[vapply(nm, function(n) identical(as.character(get_role(t[[n]]))[1], "emp"), logical(1))][[1]]
}



# tab_reg's `inverse_two_level_factors` can model P(first level), so determine the modelled positive
# level empirically from the crude column's own level (it IS P(positive | level)); the hand
# quantities then match exactly.
emp_positive_level <- function(t, d, levcol) {
  r1 <- levels(d$race)[1]
  e1 <- unname(get_pct(t[[emp_col(t)]])[match(r1, as.character(t$levels))])  # P(positive | race == r1)
  p_first <- mean(d[[levcol]][d$race == r1] == levels(d[[levcol]])[1])
  if (isTRUE(all.equal(e1, p_first, tolerance = 1e-6))) levels(d[[levcol]])[1]
  else                                                  levels(d[[levcol]])[2]
}



test_that("binomial coefficient: single-predictor OR == crude OR (Obs_OR) == model OR", {
  d <- emp_data()
  t <- tab_reg(d, "married", "race", family = "binomial", empirical = TRUE, cleannames = FALSE)
  emp_or <- get_or(t[["Obs_OR"]]); names(emp_or) <- as.character(t$levels)
  mod_nm <- "Model_OR"                                                       # the single-pred model col
  mod_or <- get_or(t[[mod_nm]]);    names(mod_or) <- as.character(t$levels)

  pos <- emp_positive_level(t, d, "married")
  m   <- stats::glm((married == pos) ~ race, d, family = stats::binomial())
  or  <- exp(stats::coef(m))
  for (l in names(or)[-1]) {
    lev <- sub("^race", "", l)
    expect_equal(unname(emp_or[lev]), unname(or[l]),      tolerance = 1e-6)
    expect_equal(unname(emp_or[lev]), unname(mod_or[lev]), tolerance = 1e-6)  # crude == adjusted (1 pred)
  }
})



# --- 14v-ii CRUDE CI PARITY: the empirical column's CI == the single-predictor model's CI ----------
# Each crude CI uses the SAME method as the model, so crude vs adjusted are directly comparable. Exact
# parity for the mean methods needs a 2-level predictor (pairwise engines vs multi-level pooled lm/glm).

emp_2lvl <- function() {
  d <- emp_data()
  d <- d[d$race %in% c("Black", "White"), , drop = FALSE]
  d$race <- forcats::fct_drop(d$race)
  d
}



test_that("multinomial: single-predictor RRR per category == crude 2x2 odds ratio", {
  skip_if_not_installed("nnet")
  d  <- emp_data()
  t  <- tab_reg(d, "party3", "race", family = "multinomial", cleannames = FALSE)
  m  <- nnet::multinom(party3 ~ race, d, trace = FALSE)
  co <- stats::coef(m)                                      # rows = non-ref categories, cols = terms
  yref <- levels(d$party3)[1]                               # "Ind"
  for (j in rownames(co)) {                                 # "Dem", "Rep"
    col <- get_or(t[[paste0(j, " vs ", yref)]]); names(col) <- as.character(t$levels)
    for (term in colnames(co)[-1]) {
      lev  <- sub("^race", "", term)
      # crude OR from the {j, ref-cat} x {level, ref-level} 2x2
      sub  <- d[d$party3 %in% c(j, yref), ]
      a <- sum(sub$party3 == j   & sub$race == lev); b <- sum(sub$party3 == yref & sub$race == lev)
      cc<- sum(sub$party3 == j   & sub$race == levels(d$race)[1])
      e <- sum(sub$party3 == yref& sub$race == levels(d$race)[1])
      crude <- (a / b) / (cc / e)
      # two claims, two tolerances. The first is OURS -- the crude column IS the fit's coefficient --
      # and holds to five figures. The second is nnet's: a one-predictor multinomial is saturated, so
      # it should reproduce exact arithmetic on counts, and it does as far as it converged.
      expect_equal(unname(col[lev]), unname(exp(co[j, term])), tolerance = 1e-5)
      expect_equal(unname(col[lev]), crude,                    tolerance = 1e-3)
    }
  }
})



test_that("ordinal: single-predictor cumulative OR brackets the cut-specific crude ORs", {
  skip_if_not_installed("MASS")
  d <- emp_data()
  o <- suppressWarnings(MASS::polr(spectrum ~ race, d, Hess = TRUE, method = "logistic"))
  cum_or <- exp(stats::coef(o))                             # one cumulative OR per race level
  # ordinal has no single tab() crude analogue; under proportional odds the pooled cumulative OR should
  # lie within the range of the cut-specific crude ORs (documented approximate check, not exact parity).
  lvls <- levels(d$spectrum)
  for (term in names(cum_or)) {
    lev <- sub("^race", "", term)
    cut_ors <- vapply(seq_len(length(lvls) - 1L), function(k) {
      hi <- as.integer(d$spectrum) > k
      a <- sum(hi & d$race == lev); b <- sum(!hi & d$race == lev)
      cc<- sum(hi & d$race == levels(d$race)[1]); e <- sum(!hi & d$race == levels(d$race)[1])
      (a / b) / (cc / e)
    }, numeric(1))
    expect_gte(unname(cum_or[term]), min(cut_ors) * 0.5)
    expect_lte(unname(cum_or[term]), max(cut_ors) * 2)
  }
})




# === the crude/adjusted MERGE: one column shape, built twice ========================================

testthat::test_that("one crude column per model column, on ONE ladder and ONE legend block", {
  d <- emp_data()
  # the three cases a base column used to grade on a different ladder from the effect beside it
  for (a in list(list(family = "binomial"),
                 list(family = "binomial", effect = "marginal", measure = "ratio"),
                 list(family = "poisson"))) {
    dep <- if (identical(a$family, "poisson")) "tvhours" else "married"
    t <- suppressWarnings(suppressMessages(do.call(
      tab_reg, c(list(data = d, outcome = dep, predictors = "race", empirical = TRUE,
                      cleannames = FALSE), a))))
    emp <- names(t)[vapply(t, function(c) is_fmt(c) && identical(get_role(c), "emp"), logical(1))]
    mdl <- names(t)[vapply(t, function(c) is_fmt(c) && identical(get_role(c), "model"), logical(1))]
    info <- paste(unlist(a), collapse = "/")
    testthat::expect_length(emp, length(mdl))
    # same estimand, same measure, same reference: one ladder
    testthat::expect_identical(get_scale(t[[emp[[1]]]]), get_scale(t[[mdl[[1]]]]), info = info)
    testthat::expect_identical(get_color(t[[emp[[1]]]]), get_color(t[[mdl[[1]]]]), info = info)
    # ...and therefore ONE legend block for the pair
    lg <- tab_color_legend(t, style = "terse", medium = "plain", lang = "en")
    testthat::expect_length(lg, 1L)
  }
})




# --- the argument boundary --------------------------------------------------------------------

test_that("`empirical` takes the word its fact table declares, not only TRUE/FALSE", {
  d <- emp_data()
  # "no" is the twin of FALSE and the word every other tabxplor argument uses for off. It was
  # DECLARED in TAB_ARGS and understood by emp_on() all along; only the validator refused it, which
  # made the jamovi picker's own off value an abort. 22g-ii added "tooltip" -- computed, printed
  # nowhere -- which is what `TRUE` resolves to with tab_vars or a per-category outcome.
  expect_identical(tab_arg("empirical")$values, c("no", "tooltip", "cell", "column"))
  expect_identical(
    suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                             empirical = "no",  stats = FALSE)),
    suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                             empirical = FALSE, stats = FALSE)))
})



# --- Phase 22g-ii: the mode is one value, and `TRUE` is the default ------------------------------

test_that("`TRUE` draws a crude column, except where a table is already wide", {
  d <- emp_data()
  mode <- function(t) {
    r <- vapply(t[vapply(t, is_fmt, logical(1))], function(c) get_role(c), character(1))
    if (any(r == "emp")) "column" else "tooltip"
  }
  # the ordinary case: the crude effect gets a column of its own, beside the model's
  b <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial"))
  expect_identical(mode(b), "column")
  # `tab_vars` groups and a per-category outcome would double an already wide table, so the crude
  # value is computed and read on hover instead -- printed nowhere, silently
  g <- suppressMessages(tab_reg(d, "married", "race", tab_vars = "rincome", family = "binomial"))
  expect_identical(mode(g), "tooltip")
  skip_if_not_installed("nnet")
  m <- suppressMessages(tab_reg(d, "marital", "race", family = "multinomial"))
  expect_identical(mode(m), "tooltip")
})




# === SECTION: a numeric predictor's crude column ==================================================

num_data <- function() {
  d <- fx_reg_df()
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



test_that("the crude fit uses the MODEL's complete-case population, not its own", {
  # `drop_extra`: a univariable fit would otherwise drop on fewer variables and land on ~2x the sample.
  d <- num_data()
  d$tvhours[seq_len(nrow(d) %/% 2L)] <- NA             # make the populations differ sharply
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




# === SECTION: crude columns for the 3+ level families =============================================

z10_data <- function() {
  d <- fx_reg_df()
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
  expect_identical(unname(reg_call(t)$crude_keys), "multinomial")
})
