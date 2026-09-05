
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


# --- 14v-ii CRUDE CI PARITY: the empirical column's CI == the single-predictor model's CI ----------
# Each crude CI uses the SAME method as the model, so crude vs adjusted are directly comparable. Exact
# parity for the mean methods needs a 2-level predictor (pairwise engines vs multi-level pooled lm/glm).

emp_2lvl <- function() {
  d <- emp_data()
  d <- d[d$race %in% c("Black", "White"), , drop = FALSE]
  d$race <- forcats::fct_drop(d$race)
  d
}


test_that("gaussian Obs_diff CI == OLS lm coefficient CI (Student, 2-level)", {
  d <- emp_2lvl()
  t <- tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE, cleannames = FALSE)
  ed <- t[["Obs_diff"]]; k <- which(!is.na(get_ci_inf(ed)))            # the non-reference level
  lev <- as.character(t$levels)[k]
  ols <- stats::confint(stats::lm(tvhours ~ race, d))[paste0("race", lev), ]
  expect_equal(get_ci_inf(ed)[k], unname(ols[1]), tolerance = 1e-6)
  expect_equal(get_ci_sup(ed)[k], unname(ols[2]), tolerance = 1e-6)
})


test_that("poisson Obs_IRR CI == quasi-Poisson regression CI (2-level)", {
  d <- emp_2lvl()
  t <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  ei <- t[["Obs_IRR"]]; k <- which(!is.na(get_ci_inf(ei)))
  lev <- as.character(t$levels)[k]
  fq <- stats::glm(tvhours ~ race, d, family = stats::quasipoisson())
  co <- summary(fq)$coefficients[paste0("race", lev), ]
  crit <- stats::qt(0.975, df = stats::df.residual(fq))
  expect_equal(get_ci_inf(ei)[k], unname(exp(co[1] - crit * co[2])), tolerance = 1e-6)
  expect_equal(get_ci_sup(ei)[k], unname(exp(co[1] + crit * co[2])), tolerance = 1e-6)
})


test_that("binomial Obs_OR CI == crude logistic-regression CI (Woolf = Wald, per level)", {
  d <- emp_data()
  t <- tab_reg(d, "married", "race", family = "binomial", empirical = TRUE, cleannames = FALSE)
  eo <- t[["Obs_OR"]]
  pos <- emp_positive_level(t, d, "married")
  fit <- stats::glm((married == pos) ~ race, d, family = stats::binomial())
  ci  <- exp(stats::confint.default(fit))
  for (k in which(!is.na(get_ci_inf(eo)))) {
    lev <- as.character(t$levels)[k]
    expect_equal(get_ci_inf(eo)[k], unname(ci[paste0("race", lev), 1]), tolerance = 1e-6, label = lev)
    expect_equal(get_ci_sup(eo)[k], unname(ci[paste0("race", lev), 2]), tolerance = 1e-6, label = lev)
  }
})


# Phase 16d: the crude risk-difference companion uses the two-proportion WALD interval (matching the
# reg's method_diff = "wald" and the model AME's Wald delta interval), not Newcombe.
test_that("binomial AME Obs_RD CI == Wald risk-difference CI", {
  d <- emp_data()
  t <- tab_reg(d, "married", "race", effect = "marginal", measure = "difference", family = "binomial", empirical = TRUE,
               cleannames = FALSE)
  ed  <- t[["Obs_RD"]]
  pos <- emp_positive_level(t, d, "married")
  y   <- as.integer(d$married == pos)
  pr  <- tapply(y, d$race, mean); nn <- tapply(y, d$race, length)
  r1  <- levels(d$race)[1]
  for (k in which(!is.na(get_ci_inf(ed)))) {
    lev <- as.character(t$levels)[k]
    hand <- ci_prop_diff(pr[[lev]], nn[[lev]], pr[[r1]], nn[[r1]], method = "wald", want_p = TRUE)
    expect_equal(get_ci_inf(ed)[k], hand$inf, tolerance = 1e-6, label = lev)
    expect_equal(get_ci_sup(ed)[k], hand$sup, tolerance = 1e-6, label = lev)
  }
})


# --- FEATURE tests --------------------------------------------------------------------------------

test_that("gaussian empirical: Obs_diff carries the level, coloured by SD(Y) (matches beta)", {
  d <- emp_data()
  t <- tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE, cleannames = FALSE)
  expect_false("Obs_mean" %in% names(t))                    # merged into the crude effect column
  expect_true(any(is.finite(get_mean(t[["Obs_diff"]]))))    # ...which carries the observed mean
  expect_identical(tabxplor:::fmt_var_kind(t[["Obs_diff"]]), "coef")
  # the crude column takes the MODEL column's measure, so both grade on one ladder
  expect_identical(get_color(t[["Obs_diff"]]), get_color(t[["Model_diff"]]))
  # var = var(Y) (constant), so the std-diff colour matches the model beta column exactly
  vy <- stats::var(d$tvhours)
  vv <- get_var(t[["Obs_diff"]]); vv <- vv[!is.na(vv)]
  expect_equal(unique(round(vv, 8)), round(vy, 8))
})


test_that("poisson empirical: one Obs_IRR carrying the observed rate", {
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  expect_false("Obs_rate" %in% names(t))
  expect_true("Obs_IRR" %in% names(t))
  expect_identical(get_color(t[["Obs_IRR"]]), get_color(t[["Model_IRR"]]))
  # an incidence-rate ratio IS a ratio of means, so it sits on `mean_ratio` -- whose level is a mean
  expect_identical(get_scale(t[["Obs_IRR"]]), "mean_ratio")
  expect_identical(get_pct_type(t[["Obs_IRR"]]), "none")
  expect_true(any(is.finite(get_mean(t[["Obs_IRR"]]))))
})


test_that("Phase h: quasipoisson empirical rides the poisson crude path (Obs_IRR)", {
  d <- emp_data()
  tq <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "quasipoisson", empirical = TRUE,
                                 cleannames = FALSE))
  expect_true("Obs_IRR" %in% names(tq))                        # was a no-op before Phase h
  # same crude descriptives as a poisson-declared model (the empirical shape is family-agnostic here).
  tp <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                 cleannames = FALSE))
  expect_equal(get_mean(tq[["Obs_IRR"]]),  get_mean(tp[["Obs_IRR"]]),  tolerance = 1e-9)
  expect_equal(get_ratio(tq[["Obs_IRR"]]), get_ratio(tp[["Obs_IRR"]]), tolerance = 1e-9)
})


# ---- Phase g: measure = "log" colours the coef + logs the empirical companion ----------------

test_that("measure = log: a binomial coefficient is coloured (log_odds scale), not all grey", {
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "married", c("race", "inc3"), family = "binomial",
                                measure = "log", cleannames = FALSE))
  bc <- t[["Model_log(OR)"]]
  expect_identical(tabxplor:::fmt_var_kind(bc), "coef")
  expect_identical(get_model_family(bc), "binomial")
  # the fix: log-odds coefficients are coloured against the LOGGED odds_ratio scale, so a non-trivial
  # coefficient gets a non-zero palette slot (pre-g fed sqrt(NA) and greyed every cell out)
  expect_gt(sum(fmt_color_channels(bc)$text != 0), 0L)
  # the legend expresses beta (log-odds) units, NOT "SD"
  expect_no_match(tab_color_legend(t, medium = "md"), "SD")
})


test_that("measure = log + empirical: Obs_log(OR) / Obs_log(IRR), logged effect + logged CI", {
  d <- emp_data()
  # binomial: Obs_log(OR); the logged empirical == log of the OR-version, same colour as the model
  tb  <- suppressWarnings(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE,
                                  measure = "log", cleannames = FALSE))
  tbo <- suppressWarnings(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE,  cleannames = FALSE))
  expect_true("Obs_log(OR)" %in% names(tb))
  expect_false("Obs_OR" %in% names(tb))
  lc <- tb[["Obs_log(OR)"]]
  expect_identical(tabxplor:::fmt_var_kind(lc), "coef")
  expect_identical(get_scale(lc), "log_coef")
  or <- get_or(tbo[["Obs_OR"]]); df <- get_diff(lc); k <- !is.na(or) & !is.na(df)
  expect_equal(df[k], log(or[k]), tolerance = 1e-8)                 # value: diff == log(OR)
  expect_equal(get_ci_inf(lc)[k], log(get_ci_inf(tbo[["Obs_OR"]])[k]), tolerance = 1e-8)  # logged CI
  expect_identical(fmt_color_channels(lc)$text, fmt_color_channels(tb[["Model_log(OR)"]])$text)
  # poisson: Obs_log(IRR)
  tp <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                 measure = "log", cleannames = FALSE))
  expect_true("Obs_log(IRR)" %in% names(tp))
  expect_identical(tabxplor:::fmt_var_kind(tp[["Obs_log(IRR)"]]), "coef")
})


test_that("empirical works with a VECTOR of dependents (one crude companion per dependent)", {
  d  <- emp_data()
  d$black <- factor(as.integer(d$race == "Black"), labels = c("no", "yes"))
  expect_no_message(
    t <- tab_reg(d, c("married", "black"), "inc3", family = "binomial", empirical = TRUE,
                 cleannames = FALSE),
    message = "not available")
  # per-dependent empirical columns, names disambiguated by a [dependent] bracket (Phase g)
  expect_true(any(grepl("Obs_OR \\[married\\]", names(t))))
  expect_true(any(grepl("Obs_OR \\[black\\]",   names(t))))
})


test_that("multinomial empirical: tooltip-only via the empirical_tips attribute; no crosstab leak", {
  skip_if_not_installed("nnet")
  d <- emp_data()
  t <- tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE, cleannames = FALSE)
  et <- tabxplor:::get_empirical_tips(t)
  expect_false(is.null(et))                                  # attribute set
  expect_true(all(c("col", "var", "level", "tip") %in% names(et)))
  expect_false(any(grepl("^Emp\\.", names(t))))              # NOT columns (tooltip only)

  # the html render carries the "crude:" fragment; a crosstab carries none (no field leak).
  # Use the WRAP the real tab_kable() path uses -- tab_wrap_text renames columns (spaces -> U+202F), so
  # the emp_tips keys must follow it (the wrap-rekey; a no-wrap prep would silently miss that bug).
  rd <- tabxplor:::tab_export_prep(
    t, backend = "kable", compute = c("refs", "colors", "bold", "range"),
    wrap = list(rows = 35, cols = 15, exdent = 2, whitespace_only = FALSE,
                unbreakable_spaces = TRUE, brk = "<br>"))$tables[[1]]
  h  <- paste(as.character(tabxplor:::render_html_engine(
    rd, meta = list(theme = "light"), subtext = "", caption = NULL,
    tooltips = TRUE, popover = FALSE, get_data = FALSE)), collapse = "\n")
  expect_match(h, "obs%: ", fixed = TRUE)

  ct <- tab(d, party3, race, pct = "row")
  expect_null(tabxplor:::get_empirical_tips(ct))
})


test_that("ordinal empirical is the UNIVARIABLE proportional-odds fit (Obs_cumOR)", {
  # Phase 18z10 inverted this test's premise. Proportional odds is a CONSTRAINT, so the univariable
  # ordinal model is not saturated and has no closed form -- but it is still "the model's own effect
  # fitted with one predictor", which is the rule every other family follows. The closed-form
  # substitutes were measured to drift 2.4-5.4 % (the PO violation itself), which is why a fit is the
  # only honest crude counterpart here.
  skip_if_not_installed("MASS")
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "spectrum", "race", family = "ordinal",
                                empirical = TRUE, cleannames = FALSE))
  expect_true("Obs_cumOR" %in% names(t))

  dm <- tidyr::drop_na(d[, c("spectrum", "race")])
  uni <- suppressWarnings(MASS::polr(spectrum ~ race, data = dm))
  i   <- which(as.character(t$var) == "race" & !is_refrow(t[["Obs_cumOR"]]))
  expect_equal(unname(get_or(t[["Obs_cumOR"]])[i]), unname(exp(stats::coef(uni))),
               tolerance = 1e-6)
  expect_gt(length(i), 0L)
})


test_that("display = 'ratio' renders the ratio field; legacy 'rr' still works", {
  d <- tab(fx_reg_df(), race, tvhours, ref = 1, color = "ratio")
  r_ratio <- format(set_display(d[["tvhours"]], "ratio"))
  r_rr    <- format(set_display(d[["tvhours"]], "rr"))
  expect_identical(r_ratio, r_rr)                            # canonical == legacy
  expect_false(any(grepl("^\\s*[0-9]{3,}", r_ratio)))       # NOT the `n` count (the old bug)
  expect_true(any(grepl("\u00d7", r_ratio)))                # shows a ratio (multiply sign)
})


test_that("change A: adjusted % coheres with the AME; unadjusted prediction == the crude % (identity)", {
  skip_if_not_installed("marginaleffects")
  d <- emp_data()
  t <- tab_reg(d, "married", c("race", "inc3"), family = "binomial", effect = "marginal", measure = "difference",
               empirical = TRUE, cleannames = FALSE)

  # change A: adjusted%(reference) + AME(level) == adjusted%(level) -- the standardized prediction and
  # the AME are the SAME estimand (avg_predictions(variables=) / avg_comparisons(variables=)).
  amecol    <- t[["Model_mRD"]]
  race_rows <- which(as.character(t$var) == "race")
  rl  <- as.character(t$levels)[race_rows]
  adj <- get_pct(amecol)[race_rows];  names(adj) <- rl
  ame <- get_diff(amecol)[race_rows]; names(ame) <- rl
  # a reference cell carries the measure's NEUTRAL now, so it is identified by its row, not by an NA
  est <- !is.na(get_pvalue(amecol)[race_rows])
  ref <- rl[!est][1]
  for (lv in rl[est]) {
    expect_equal(unname(adj[ref] + ame[lv]), unname(adj[lv]), tolerance = 1e-6)
  }

  # score-equation identity: the model's UNADJUSTED predicted % (avg_predictions(by = v), the observed-
  # group average) equals the same-frame crude % exactly. Phase 17h cut the `predicted_unadjusted` control
  # column; the identity is asserted here directly, by refitting on the model's complete-case frame.
  # inverse_two_level_factors = TRUE (default) models the FIRST level ("no") as the event, so refit on
  # that same modelled event to match the crude column's orientation.
  dm    <- d[stats::complete.cases(d[, c("married", "race", "inc3")]), , drop = FALSE]
  fit   <- stats::glm((married == "no") ~ race + inc3, data = dm, family = stats::binomial())
  ap    <- as.data.frame(marginaleffects::avg_predictions(fit, by = "race"))
  unadj <- ap$estimate; names(unadj) <- as.character(ap$race)
  emp   <- get_pct(t[[emp_col(t)]])[race_rows]; names(emp) <- rl
  expect_equal(emp[names(unadj)], unadj, tolerance = 1e-6)
})


# === the crude interval IS the univariable model's, under the table's own basis =====================
#
# Unweighted that model is lm / glm and its interval is MODEL-BASED (one dispersion pooled over the
# predictor's levels); weighted it is svyglm and its interval is the SANDWICH, which the per-group
# forms reproduce. The pairwise forms this replaced were right in NEITHER basis on a k > 2 predictor.

test_that("gaussian Obs_diff == the univariable lm coefficient CI on a 3-LEVEL predictor", {
  d <- tidyr::drop_na(fx_reg_df()[, c("tvhours", "race", "age")])
  d$race <- forcats::fct_drop(d$race)
  t  <- tab_reg(d, "tvhours", c("race", "age"), family = "gaussian", empirical = TRUE,
                cleannames = FALSE)
  oc <- t[["Obs_diff"]]
  is_race <- as.character(t$var) == "race" & !is.na(get_ci_inf(oc))
  ref <- stats::confint(stats::lm(tvhours ~ race, data = d))[-1, , drop = FALSE]
  expect_equal(get_ci_inf(oc)[is_race], unname(ref[, 1]), tolerance = 1e-8)
  expect_equal(get_ci_sup(oc)[is_race], unname(ref[, 2]), tolerance = 1e-8)
  expect_identical(unique(get_ci_method(oc)), "ols")     # and it says which interval it ran
})


test_that("poisson Obs_IRR == the univariable quasi-Poisson CI on a 3-LEVEL predictor", {
  d <- tidyr::drop_na(fx_reg_df()[, c("tvhours", "race", "age")])
  d <- d[d$tvhours > 0, ]; d$race <- forcats::fct_drop(d$race)
  t  <- suppressWarnings(tab_reg(d, "tvhours", c("race", "age"), family = "poisson",
                                 empirical = TRUE, cleannames = FALSE))
  oc <- t[["Obs_IRR"]]
  is_race <- as.character(t$var) == "race" & !is.na(get_ci_inf(oc))
  fit <- stats::glm(tvhours ~ race, data = d, family = stats::quasipoisson())
  ci  <- exp(stats::confint.default(fit))[-1, , drop = FALSE]
  expect_equal(get_ci_inf(oc)[is_race], unname(ci[, 1]), tolerance = 5e-3)
  expect_identical(unique(get_ci_method(oc)), "quasipoisson")
})


test_that("a WEIGHTED table takes the sandwich twin instead", {
  d <- tidyr::drop_na(fx_reg_df()[, c("tvhours", "race", "age")])
  d$race <- forcats::fct_drop(d$race)
  set.seed(1); d$w <- stats::runif(nrow(d), 0.3, 3)
  t <- suppressWarnings(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                                wt = "w", empirical = TRUE, cleannames = FALSE))
  expect_identical(unique(get_ci_method(t[["Obs_diff"]])), "welch")
})


testthat::test_that("a crude column is never its own baseline (ruling Q1(b))", {
  d <- emp_data()
  for (a in list(list(family = "binomial"), list(family = "gaussian"), list(family = "binomial", link = "ratio"))) {
    dep <- if (identical(a$family, "binomial")) "married" else "tvhours"
    t <- suppressWarnings(suppressMessages(do.call(
      tab_reg, c(list(data = d, outcome = dep, predictors = "race", empirical = TRUE,
                      cleannames = FALSE), a))))
    for (nm in names(t)[vapply(t, function(c) is_fmt(c) && identical(get_role(c), "emp"),
                               logical(1))]) {
      testthat::expect_true(all(is.na(get_obs(t[[nm]]))),    info = nm)
      testthat::expect_true(all(is.na(get_gap_se(t[[nm]]))), info = nm)
    }
  }
})


# --- THE CRUDE INTERVAL IS ITS OWN ESTIMATE'S ------------------------------------------------------

test_that("a multinomial crude odds ratio brackets the estimand it prints", {
  # The estimate is category j against the PIVOT (the reference outcome category) -- nnet::multinom's
  # own estimand. Its interval used to be built against "everything else", so the two were different
  # odds ratios printed as one cell, and the estimate fell OUTSIDE its own bracket.
  skip_if_not_installed("nnet")
  d <- emp_data()
  t <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial",
                                empirical = "column", cleannames = FALSE, stats = FALSE))
  tb  <- table(d$race, d$party3)
  piv <- colnames(tb)[[1]]; rlv <- rownames(tb)[[1]]
  cols <- names(t)[vapply(t, function(x) is_fmt(x) && identical(get_role(x), "emp"), logical(1))]
  expect_gt(length(cols), 0L)
  done <- 0L
  for (cn in cols) {
    col   <- t[[cn]]
    cat_j <- sub(" vs .*$", "", sub("^Obs_", "", cn))   # "Obs_Dem vs Ind" -> "Dem"
    expect_true(cat_j %in% colnames(tb))
    for (lv in setdiff(rownames(tb), rlv)) {
      i <- which(as.character(t$var) == "race" & as.character(t$levels) == lv)
      if (!length(i)) next
      hand <- ci_or(tb[lv, cat_j], tb[lv, piv], tb[rlv, cat_j], tb[rlv, piv])
      expect_equal(get_or(col)[i], (tb[lv, cat_j] * tb[rlv, piv]) / (tb[lv, piv] * tb[rlv, cat_j]),
                   tolerance = 1e-10)
      expect_equal(get_ci_inf(col)[i], hand$inf,    tolerance = 1e-10)
      expect_equal(get_ci_sup(col)[i], hand$sup,    tolerance = 1e-10)
      expect_equal(get_pvalue(col)[i], hand$pvalue, tolerance = 1e-10)
      done <- done + 1L
    }
  }
  expect_gt(done, 3L)   # the comparison must actually have run: this test went vacuous once
})


test_that("a multinomial crude column answers for its NUMERIC predictors too", {
  # 22b-xiii-2. A 3+ level fit answers PER OUTCOME CATEGORY, but reg_empirical_fit()'s coefficient
  # branch stamped `category = ""` while emit() looks its rows up under the category key -- so every
  # fit-derived row missed, and each crude column left its numeric predictors entirely empty.
  skip_if_not_installed("nnet")
  d  <- emp_data()
  dm <- d[stats::complete.cases(d[c("party3", "race", "tvhours")]), ]
  t  <- suppressMessages(tab_reg(dm, "party3", c("race", "tvhours"), family = "multinomial",
                                 empirical = "column", cleannames = FALSE, stats = FALSE))
  i <- which(as.character(t$var) == "tvhours")
  expect_length(i, 1L)
  cols <- names(t)[vapply(t, function(x) is_fmt(x) && identical(get_role(x), "emp"), logical(1))]
  expect_gt(length(cols), 1L)
  # the univariable multinom, per 2 SD -- tab_reg's own default multiplier. ⚠ loose tolerance: two
  # independent nnet::multinom runs agree only to its optimiser's own convergence tolerance.
  m  <- nnet::multinom(party3 ~ tvhours, data = dm, trace = FALSE)
  k  <- 2 * stats::sd(dm$tvhours)
  hand <- exp(summary(m)$coefficients[, "tvhours"] * k)
  for (cn in cols) {
    cat_j <- sub(" vs .*$", "", sub("^Obs_", "", cn))
    expect_true(is.finite(get_or(t[[cn]])[i]))
    expect_equal(get_or(t[[cn]])[i], unname(hand[[cat_j]]), tolerance = 1e-4)
    expect_true(is.finite(get_ci_inf(t[[cn]])[i]))
  }
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
  # Phase 19h: the anchor signal is `ann$anchor` -- the cells kept at full strength.
  # it that no backend read (and the transpose silently dropped), so it is a prep-internal local now.
  # On a Constant row the two are the same value: the footer override only touches GOF footer rows.
  expect_true(all(purrr::map_lgl(rd$tables[[1]]$ann, ~ .x$anchor[const_row])))
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

  tr <- tab_reg(d, "married", c("age", "race"), family = "binomial", link = "ratio",   # binary -> modified Poisson
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

test_that("a marginal effect, additive or ratio: the numeric crude cell is the UNIVARIABLE one", {
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


test_that("a poisson marginal difference pairs with the observed mean DIFFERENCE, numeric rows included", {
  # a poisson marginal effect is a difference of expected COUNTS, so its crude counterpart is the
  # observed difference of means -- REG_EMPIRICAL$poisson$diff. It used to fall back to the rate-ratio
  # shape, which reg_same_estimand() then rightly refused to pair, leaving the column unusable.
  skip_if_not_installed("marginaleffects")
  d <- num_data()
  t <- suppressWarnings(tab_reg(d, "tvhours", c("age", "race"), family = "poisson",
                                effect = "marginal", measure = "difference", empirical = TRUE, cleannames = FALSE))
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
                                effect = "at_reference", measure = "difference", empirical = TRUE,
                                cleannames = FALSE))
  i  <- which(as.character(t$var) == "age")
  mc <- names(t)[purrr::map_lgl(t, is_fmt)]
  expect_true(all(is.na(get_obs(t[[mc[[length(mc)]]]])[i])))
})


# --- 5. the `multiplier` grammar -------------------------------------------------------------------

or_of <- function(t, v, col = "Model_OR") get_or(t[[col]])[as.character(t$var) == v]


test_that("multiplier = 1 is per-1-unit everywhere -- scaling nothing, and SAYING so", {
  d <- num_data()
  t <- tab_reg(d, "married", c("age", "race"), family = "binomial", multiplier = 1,
               cleannames = FALSE)
  # the scaling factor is dropped (multiplying by 1 changes no number)...
  expect_null(reg_call(t)$multiplier)
  # ...but the LABEL is descriptive, and "per 1" is what the user asked to read (Phase 22g-v)
  expect_true(any(grepl("per 1", as.character(t$levels), fixed = TRUE)))
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
                       multiplier = c(age = "banana")), "must be a number")
})


# --- 7. the gap test on numeric rows ---------------------------------------------------------------

test_that("the numeric coefficient gap SE == a hand-stacked influence-function computation", {
  skip_if_not_installed("survey")
  d <- num_data()
  # `rr` (modified Poisson on a binary outcome) is collapsible, so the COEFFICIENT gap test fires
  # (a conditional OR is not -- reg_estimand_collapsible()).
  t <- tab_reg(d, "married", c("age", "tvhours", "race"), family = "binomial", link = "ratio", empirical = TRUE,
               color = c(TRUE, "adjustment"), multiplier = 1, cleannames = FALSE)
  i  <- which(as.character(t$var) == "age")
  se <- get_gap_se(t[["Model_RR"]])[i]
  expect_true(is.finite(se) && se > 0)

  dm <- tidyr::drop_na(d, "married", "age", "tvhours", "race")
  dm$y <- as.numeric(dm$married == reg_call(t)$positive_level)
  des  <- suppressWarnings(survey::svydesign(ids = ~1, data = dm))
  fm   <- survey::svyglm(y ~ age + tvhours + race, design = des, family = stats::quasipoisson())
  fc   <- survey::svyglm(y ~ age,                  design = des, family = stats::quasipoisson())
  ifm  <- reg_coef_if_maker(fm, dm)(stats::setNames(1, "age"))
  ifc  <- reg_coef_if_maker(fc, dm)(stats::setNames(1, "age"))
  expect_equal(se, reg_if_se(ifm - ifc, fm$survey.design), tolerance = 1e-12)
})


test_that("multiplier scales the numeric gap SE by |k| (so the z is invariant)", {
  d <- num_data()
  mk <- function(k) tab_reg(d, "married", c("age", "tvhours", "race"), family = "binomial", link = "ratio",
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


test_that("a marginal effect, additive or ratio: numeric rows get a gap SE too (the IF numeric arm)", {
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
  expect_match(row$tip, "mean .* \\(sd .*\\)")
  expect_match(row$tip, " ; yes ")                 # binary outcome -> mean(X | Y) per group

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


# --- grouped binomial (`trials =`) ------------------------------------------------------------------

test_that("a grouped binomial's crude OR is the univariable glm(cbind(s, q - s))", {
  d <- z10_data()
  t <- suppressWarnings(suppressMessages(
    tab_reg(d, "score", c("race", "mar3"), family = "binomial", trials = 10, empirical = TRUE,
            cleannames = FALSE)))
  expect_true("Obs_OR" %in% names(t))

  uni <- stats::glm(cbind(score, 10L - score) ~ race, data = d, family = stats::binomial())
  lv  <- levels(d$race)[-1]
  i   <- match(lv, as.character(t$levels)[as.character(t$var) == "race"])
  got <- get_or(t[["Obs_OR"]])[as.character(t$var) == "race"][i]
  expect_equal(unname(got), unname(exp(stats::coef(uni))[-1]), tolerance = 1e-6)
})


test_that("the grouped binomial's LEVEL is the mean SCORE, not a share of respondents", {
  d <- z10_data()
  t <- suppressWarnings(suppressMessages(
    tab_reg(d, "score", "race", family = "binomial", trials = 10, empirical = TRUE,
            cleannames = FALSE)))
  # a summed score's odds ratio sits on the mean SCORE -- the average number of "yes" out of
  # `trials`, which is what a reader of a battery of items wants. That is `score_ratio`, the one
  # scale whose estimate is an odds ratio and whose level is a mean.
  expect_identical(get_scale(t[["Obs_OR"]]), "score_odds_ratio")
  expect_identical(tabxplor:::fmt_var_kind(t[["Obs_OR"]]), "mean")
  want <- as.vector(tapply(d$score, d$race, mean))
  names(want) <- levels(d$race)
  k    <- as.character(t$var) == "race"
  got  <- get_mean(t[["Obs_OR"]])[k]
  expect_equal(unname(got), unname(want[as.character(t$levels)[k]]), tolerance = 1e-8)
  expect_gt(max(want), 1)                       # a SCORE out of 10, not a proportion in [0, 1]
  # and the MODEL column matches it: a single predictor is saturated, so its adjusted score is the
  # observed one -- which is the whole point of putting the two side by side.
  expect_equal(get_mean(t[["Model_OR"]])[k], got, tolerance = 1e-6)
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


# 22g-ii: a per-category outcome draws no crude column either way -- one model column would need
# several of them. What changed is WHERE the number goes by default: `TRUE` now resolves to
# "tooltip" (computed, printed nowhere, read on hover), and `"cell"` is what folds it into the cell.
test_that("multinomial: the default computes the crude number and prints it nowhere", {
  skip_if_not_installed("nnet")
  d <- z10_data()
  t <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE,
                                cleannames = FALSE))
  expect_false(any(grepl("^Obs_", names(t))))
  x <- t[[reg_fmt_cols(t)[[1]]]]
  expect_false(any(grepl("{obs}", get_display(x), fixed = TRUE)))
  expect_true(any(!is.na(get_obs(x))))                     # ...computed all the same
  # ...and the hover is where it is read, on its own line, which is the point of the mode
  tips <- tabxplor:::tab_tooltip_text(x)
  expect_true(any(grepl("obs:", tips, fixed = TRUE)))
  expect_true(any(grepl("\n", tips, fixed = TRUE)))
})


test_that("multinomial, `empirical = \"cell\"`: the crude number rides IN the cell", {
  skip_if_not_installed("nnet")
  d <- z10_data()
  t <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial", empirical = "cell",
                                cleannames = FALSE))
  expect_false(any(grepl("^Obs_", names(t))))
  x <- t[[reg_fmt_cols(t)[[1]]]]
  # ONE layout for the whole column (the `est_obs` preset), the aside FIRST as everywhere else
  expect_true(all(get_display(x)[!is.na(get_obs(x))] == "({obs}) {est}"))
  expect_true(any(grepl("(", format(x), fixed = TRUE)))
  # the footer names the bracket, so the reader is told what it is
  expect_match(paste(tabxplor:::reg_model_lines(t, lang = "en"), collapse = " "),
               "observed", fixed = TRUE)
  # ...and the tooltip does not repeat what the cell already shows
  tips <- tabxplor:::tab_tooltip_text(x)
  expect_false(any(grepl("obs:", tips, fixed = TRUE)))
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
  for (meas in c("difference", "ratio")) {
    t <- suppressMessages(tab_reg(d, "party3", c("race", "mar3"), family = "multinomial",
                                  effect = "marginal", measure = meas, empirical = TRUE,
                                  color = c(TRUE, "adjustment"), cleannames = FALSE))
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
                                empirical = TRUE, color = c(TRUE, "adjustment"),
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


test_that("poisson: single-predictor IRR == crude rate-ratio (Obs_IRR) == tab() ratio", {
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  # an IRR is a ratio of MEANS, so it lives in `ratio` (the field its `mean_ratio` scale names)
  emp_irr <- get_ratio(t[["Obs_IRR"]]); names(emp_irr) <- as.character(t$levels)

  m   <- suppressWarnings(stats::glm(tvhours ~ race, d, family = stats::poisson()))
  irr <- exp(stats::coef(m))
  means <- tapply(d$tvhours, d$race, mean)                   # crude rate = mean count per level
  for (l in names(irr)[-1]) {
    lev <- sub("^race", "", l)
    expect_equal(unname(emp_irr[lev]), unname(irr[l]),                       tolerance = 1e-6)
    expect_equal(unname(emp_irr[lev]), unname(means[lev] / means[1]),        tolerance = 1e-6)
  }
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


test_that("binomial AME: single-predictor risk-diff (Obs_RD) == observed risk difference", {
  d <- emp_data()
  t <- tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "difference", empirical = TRUE,
               cleannames = FALSE)
  emp_diff <- get_diff(t[["Obs_RD"]]); names(emp_diff) <- as.character(t$levels)

  # a saturated single-predictor logit reproduces the observed proportions -> AME == observed risk diff
  pos  <- emp_positive_level(t, d, "married")
  p    <- tapply(as.integer(d$married == pos), d$race, mean)
  rdif <- p - p[1]
  for (lev in names(rdif)[-1]) {
    expect_equal(unname(emp_diff[lev]), unname(rdif[lev]), tolerance = 1e-6)
  }
  # the marginal column pairs with the crude RISK DIFFERENCE, not the crude odds ratio, and both
  # name the same measure with the contrast marked on the model side only
  expect_true("Obs_RD" %in% names(t))
  expect_false("Obs_OR" %in% names(t))
  expect_true("Model_mRD" %in% names(t))
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


# --- decisions doc S50: adjusted % (marginal standardization) + empirical on the model frame ---------

test_that("change B: empirical companions use the model's complete-case frame, not full data", {
  d <- emp_data()
  expect_true(anyNA(d$inc3) && !anyNA(d$race))     # differential missingness: inc3 has NAs, race none
  t  <- tab_reg(d, "married", c("race", "inc3"), family = "binomial", empirical = TRUE,
                cleannames = FALSE)
  dm <- d[stats::complete.cases(d[, c("married", "race", "inc3")]), , drop = FALSE]
  expect_lt(nrow(dm), nrow(d))                      # listwise deletion actually bites

  # the crude cell counts for the race predictor sum to the MODEL frame N, not the full-data N
  race_rows <- as.character(t$var) == "race"
  n_emp <- sum(get_n(t[[emp_col(t)]])[race_rows], na.rm = TRUE)
  expect_equal(n_emp, nrow(dm))
  expect_lt(n_emp, nrow(d))
})


testthat::test_that("under a gap measure the crude column IS the reference column", {
  skip_if_not_installed("marginaleffects")
  d <- emp_data()
  t <- suppressWarnings(suppressMessages(
    tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "ratio",
            empirical = TRUE, color = "adjustment", cleannames = FALSE)))
  emp <- names(t)[vapply(t, function(c) is_fmt(c) && identical(get_role(c), "emp"), logical(1))][[1]]
  mdl <- names(t)[vapply(t, function(c) is_fmt(c) && identical(get_role(c), "model"), logical(1))][[1]]
  # marked as the baseline, and read as one by the reference subsystem (so it bolds)...
  testthat::expect_true(isTRUE(is_refcol(t[[emp]])))
  testthat::expect_true(all(get_reference(t[[emp]], "all_totals")))
  testthat::expect_false(any(get_reference(t[[mdl]], "all_totals") & !is_refrow(t[[mdl]])))
  # ...uncoloured by construction (no `obs` to score), and named as such in the legend
  testthat::expect_true(all(fmt_color_channels(t[[emp]])$text_slot == 0L))
  testthat::expect_match(paste(tab_color_legend(t, style = "terse", medium = "plain", lang = "en"),
                               collapse = " | "), "observed effect")
})


testthat::test_that("`display` is post-hoc: it changes no number, on either column", {
  d <- emp_data()
  base <- tab_reg(d, "married", "race", family = "binomial", empirical = TRUE, cleannames = FALSE)
  same <- tab_reg(d, "married", "race", family = "binomial", empirical = TRUE, cleannames = FALSE,
                  display = "est_base")
  est_cols <- names(base)[vapply(base, function(c) is_fmt(c) && nzchar(get_role(c)) &&
                                   get_role(c) != "n", logical(1))]
  # NOT dplyr::across(): on a grouped tab it runs per sub-table, and a display is a COLUMN fact
  post <- base
  for (nm in est_cols) post[[nm]] <- set_display(post[[nm]], "est_base")
  for (nm in est_cols) {
    for (f in c("or", "pct", "diff", "ci_inf", "ci_sup", "pvalue", "n")) {
      testthat::expect_equal(vctrs::field(base[[nm]], f), vctrs::field(same[[nm]], f), info = nm)
    }
    testthat::expect_identical(format(same[[nm]]), format(post[[nm]]), info = nm)
  }
})


test_that("`tooltip` computes everything `column` does, and draws nothing", {
  d  <- emp_data()
  mc <- function(t) t[[names(t)[vapply(t, function(x)
    is_fmt(x) && identical(get_role(x), "model"), logical(1))][[1]]]]
  a <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                measure = "difference", empirical = "column"))
  b <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                measure = "difference", empirical = "tooltip"))
  expect_true(any(grepl("^Obs_", names(a))))
  expect_false(any(grepl("^Obs_", names(b))))
  # the two fields `color = "adjustment"` and the hover read are identical
  expect_identical(get_obs(mc(a)),    get_obs(mc(b)))
  expect_identical(get_gap_se(mc(a)), get_gap_se(mc(b)))
})


test_that("`cell` is the `est_obs` LAYOUT, not a per-cell rewrite", {
  d <- emp_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = "cell"))
  x <- t[[names(t)[vapply(t, function(z)
    is_fmt(z) && identical(get_role(z), "model"), logical(1))][[1]]]]
  # ONE template for every cell that has a crude counterpart, and the aside comes FIRST
  expect_true(all(get_display(x)[!is.na(get_obs(x))] == "({obs}) {est}"))
  expect_identical(tabxplor:::display_resolve("est_obs", "model"), "({obs}) {est}")
  # a cell with no counterpart prunes the bracket rather than printing an empty one
  expect_false(any(grepl("()", format(x), fixed = TRUE)))
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


# --- 5. the `multiplier` grammar -------------------------------------------------------------------

or_of <- function(t, v, col = "Model_OR") get_or(t[[col]])[as.character(t$var) == v]


test_that("a NAMED vector overrides per variable; unnamed predictors keep the scalar default", {
  d <- num_data()
  p <- c("age", "tvhours", "race")
  t1 <- tab_reg(d, "married", p, family = "binomial", multiplier = 1, cleannames = FALSE)
  tn <- tab_reg(d, "married", p, family = "binomial", multiplier = c(age = 10),
                cleannames = FALSE)
  ts <- tab_reg(d, "married", p, family = "binomial", cleannames = FALSE)
  expect_equal(or_of(tn, "age"), or_of(t1, "age")^10, tolerance = 1e-8)
  # tvhours is NOT named -> it keeps the SCALAR DEFAULT ("2sd"), not per 1 unit
  expect_equal(or_of(tn, "tvhours"), or_of(ts, "tvhours"), tolerance = 1e-12)

  tm <- tab_reg(d, "married", p, family = "binomial",
                multiplier = c(age = "2sd", tvhours = 5), cleannames = FALSE)
  k  <- reg_call(tm)$multiplier
  expect_equal(unname(k[["tvhours"]]), 5)
  expect_equal(or_of(tm, "tvhours"), or_of(t1, "tvhours")^5, tolerance = 1e-8)
})


test_that("the numeric row's label names its unit AND its anchor", {
  d <- num_data()
  t <- tab_reg(d, "married", c("age", "race"), family = "binomial", multiplier = "sd",
               cleannames = FALSE)
  # the level carries the two facts that place the row: the unit its effect is per, and the anchor
  # the Constant row sits at. The `var` column already names the variable, and the sparkline lives
  # in the base-count cell, so nothing else shares this string.
  lab <- as.character(t$levels)[as.character(t$var) == "age"]
  expect_match(lab, "^per [0-9.]+ \\(SD\\), at [0-9.]+ \\(mean\\)$")
  t10 <- tab_reg(d, "married", c("age", "race"), family = "binomial", multiplier = c(age = 10),
                 ref = c(age = "min"), cleannames = FALSE)
  expect_identical(as.character(t10$levels)[as.character(t10$var) == "age"],
                   paste0("per 10, at ", format(signif(min(d$age), 3)), " (min)"))
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
