# Phase 14v: the empirical (crude / unadjusted) companion framework.
# The governing statistical claim: the crude quantity IS the model quantity when there is ONE
# predictor. These tests prove, per family, that a SINGLE-predictor model estimate == the empirical
# column == the tab() (no-model) quantity. gss_cat-derived data only (never pc18 / ct13).

emp_data <- function() {
  d <- forcats::gss_cat
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
  emp_mean <- get_mean(t[["Obs_mean"]]); names(emp_mean) <- as.character(t$levels)

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

test_that("poisson: single-predictor IRR == crude rate-ratio (Obs_IRR) == tab() ratio", {
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  emp_irr <- get_or(t[["Obs_IRR"]]); names(emp_irr) <- as.character(t$levels)

  m   <- suppressWarnings(stats::glm(tvhours ~ race, d, family = stats::poisson()))
  irr <- exp(stats::coef(m))
  means <- tapply(d$tvhours, d$race, mean)                   # crude rate = mean count per level
  for (l in names(irr)[-1]) {
    lev <- sub("^race", "", l)
    expect_equal(unname(emp_irr[lev]), unname(irr[l]),                       tolerance = 1e-6)
    expect_equal(unname(emp_irr[lev]), unname(means[lev] / means[1]),        tolerance = 1e-6)
  }
})

# tab_reg's `inverse_two_level_factors` can model P(first level), so determine the modelled positive
# level empirically from Obs_% (it IS P(positive | level)); the hand quantities then match exactly.
emp_positive_level <- function(t, d, levcol) {
  r1 <- levels(d$race)[1]
  e1 <- unname(get_pct(t[["Obs_%"]])[match(r1, as.character(t$levels))])   # P(positive | race == r1)
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

test_that("binomial AME: single-predictor risk-diff (Obs_diff) == observed risk difference", {
  d <- emp_data()
  t <- tab_reg(d, "married", "race", family = "binomial", effect = "marginal", empirical = TRUE,
               cleannames = FALSE)
  emp_diff <- get_diff(t[["Obs_diff"]]); names(emp_diff) <- as.character(t$levels)

  # a saturated single-predictor logit reproduces the observed proportions -> AME == observed risk diff
  pos  <- emp_positive_level(t, d, "married")
  p    <- tapply(as.integer(d$married == pos), d$race, mean)
  rdif <- p - p[1]
  for (lev in names(rdif)[-1]) {
    expect_equal(unname(emp_diff[lev]), unname(rdif[lev]), tolerance = 1e-6)
  }
  # the AME column carries "Obs_diff", NOT "Obs_OR", and the header names the (adjusted %)
  expect_true("Obs_diff" %in% names(t))
  expect_false("Obs_OR"  %in% names(t))
  expect_true(any(grepl("adjusted %", names(t), fixed = TRUE)))
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
test_that("binomial AME Obs_diff CI == Wald risk-difference CI", {
  d <- emp_data()
  t <- tab_reg(d, "married", "race", effect = "marginal", family = "binomial", empirical = TRUE,
               cleannames = FALSE)
  ed  <- t[["Obs_diff"]]
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
      expect_equal(unname(col[lev]), unname(exp(co[j, term])), tolerance = 1e-5)
      expect_equal(unname(col[lev]), crude,                    tolerance = 1e-5)
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

# --- FEATURE tests --------------------------------------------------------------------------------

test_that("gaussian empirical: Obs_mean uncoloured, Obs_diff coloured by SD(Y) (matches beta)", {
  d <- emp_data()
  t <- tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE, cleannames = FALSE)
  expect_identical(get_color(t[["Obs_mean"]]), "")          # base descriptive: uncoloured
  expect_identical(tabxplor:::fmt_var_kind(t[["Obs_diff"]]), "coef")
  expect_identical(get_color(t[["Obs_diff"]]), "diff")
  # var = var(Y) (constant), so the std-diff colour matches the model beta column exactly
  vy <- stats::var(d$tvhours)
  vv <- get_var(t[["Obs_diff"]]); vv <- vv[!is.na(vv)]
  expect_equal(unique(round(vv, 8)), round(vy, 8))
})

test_that("poisson empirical: Obs_rate (ratio colour) + Obs_IRR", {
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  expect_true(all(c("Obs_rate", "Obs_IRR") %in% names(t)))
  expect_identical(get_color(t[["Obs_rate"]]), "ratio")
  expect_identical(get_pct_base(t[["Obs_IRR"]]), "row")
})

test_that("Phase h: quasipoisson empirical rides the poisson crude path (Obs_rate + Obs_IRR)", {
  d <- emp_data()
  tq <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "quasipoisson", empirical = TRUE,
                                 cleannames = FALSE))
  expect_true(all(c("Obs_rate", "Obs_IRR") %in% names(tq)))    # was a no-op before Phase h
  # same crude descriptives as a poisson-declared model (the empirical shape is family-agnostic here).
  tp <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                 cleannames = FALSE))
  expect_equal(get_mean(tq[["Obs_rate"]]), get_mean(tp[["Obs_rate"]]), tolerance = 1e-9)
  expect_equal(get_or(tq[["Obs_IRR"]]),    get_or(tp[["Obs_IRR"]]),    tolerance = 1e-9)
})

# ---- Phase g: measure = "log" colours the coef + logs the empirical companion ----------------

test_that("measure = log: a binomial coefficient is coloured (log_odds scale), not all grey", {
  skip_if_not_installed("broom")
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "married", c("race", "inc3"), family = "binomial",
                                measure = "log", cleannames = FALSE))
  bc <- t[["Model_\u03b2"]]
  expect_identical(tabxplor:::fmt_var_kind(bc), "coef")
  expect_identical(get_model_family(bc), "binomial")
  # the fix: log-odds coefficients are coloured against the LOGGED odds_ratio scale, so a non-trivial
  # coefficient gets a non-zero palette slot (pre-g fed sqrt(NA) and greyed every cell out)
  expect_gt(sum(fmt_color_channels(bc)$text != 0), 0L)
  # the legend expresses beta (log-odds) units, NOT "SD"
  expect_no_match(tab_color_legend(t, medium = "md"), "SD")
})

test_that("measure = log + empirical: Obs_log(OR) / Obs_log(IRR), logged effect + logged CI", {
  skip_if_not_installed("broom")
  d <- emp_data()
  # binomial: Obs_% + Obs_log(OR); the logged empirical == log of the OR-version, same colour as the model
  tb  <- suppressWarnings(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE,
                                  measure = "log", cleannames = FALSE))
  tbo <- suppressWarnings(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE,  cleannames = FALSE))
  expect_true(all(c("Obs_%", "Obs_log(OR)") %in% names(tb)))
  expect_false("Obs_OR" %in% names(tb))
  lc <- tb[["Obs_log(OR)"]]
  expect_identical(tabxplor:::fmt_var_kind(lc), "coef")
  expect_identical(get_scale(lc), "log_coef")
  or <- get_or(tbo[["Obs_OR"]]); df <- get_diff(lc); k <- !is.na(or) & !is.na(df)
  expect_equal(df[k], log(or[k]), tolerance = 1e-8)                 # value: diff == log(OR)
  expect_equal(get_ci_inf(lc)[k], log(get_ci_inf(tbo[["Obs_OR"]])[k]), tolerance = 1e-8)  # logged CI
  expect_identical(fmt_color_channels(lc)$text, fmt_color_channels(tb[["Model_\u03b2"]])$text)
  # poisson: Obs_rate + Obs_log(IRR)
  tp <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                 measure = "log", cleannames = FALSE))
  expect_true(all(c("Obs_rate", "Obs_log(IRR)") %in% names(tp)))
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
  expect_true(any(grepl("Obs_% \\[married\\]", names(t))))
  expect_true(any(grepl("Obs_% \\[black\\]",   names(t))))
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
  expect_match(h, "crude:")

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
  d <- tab(forcats::gss_cat, race, tvhours, ref = 1, color = "ratio")
  r_ratio <- format(set_display(d[["tvhours"]], "ratio"))
  r_rr    <- format(set_display(d[["tvhours"]], "rr"))
  expect_identical(r_ratio, r_rr)                            # canonical == legacy
  expect_false(any(grepl("^\\s*[0-9]{3,}", r_ratio)))       # NOT the `n` count (the old bug)
  expect_true(any(grepl("\u00d7", r_ratio)))                # shows a ratio (multiply sign)
})


# --- decisions doc S50: adjusted % (marginal standardization) + empirical on the model frame ---------

test_that("change B: empirical companions use the model's complete-case frame, not full data", {
  skip_if_not_installed("broom")
  d <- emp_data()
  expect_true(anyNA(d$inc3) && !anyNA(d$race))     # differential missingness: inc3 has NAs, race none
  t  <- tab_reg(d, "married", c("race", "inc3"), family = "binomial", empirical = TRUE,
                cleannames = FALSE)
  dm <- d[stats::complete.cases(d[, c("married", "race", "inc3")]), , drop = FALSE]
  expect_lt(nrow(dm), nrow(d))                      # listwise deletion actually bites

  # the crude Emp.% cell counts for the race predictor sum to the MODEL frame N, not the full-data N
  race_rows <- as.character(t$var) == "race"
  n_emp <- sum(get_n(t[["Obs_%"]])[race_rows], na.rm = TRUE)
  expect_equal(n_emp, nrow(dm))
  expect_lt(n_emp, nrow(d))
})

test_that("change A: adjusted % coheres with the AME; unadjusted prediction == Obs_% (identity)", {
  skip_if_not_installed("marginaleffects")
  d <- emp_data()
  t <- tab_reg(d, "married", c("race", "inc3"), family = "binomial", effect = "marginal",
               empirical = TRUE, cleannames = FALSE)

  # change A: adjusted%(reference) + AME(level) == adjusted%(level) -- the standardized prediction and
  # the AME are the SAME estimand (avg_predictions(variables=) / avg_comparisons(variables=)).
  amecol    <- t[["Model_AME (adjusted %)"]]
  race_rows <- which(as.character(t$var) == "race")
  rl  <- as.character(t$levels)[race_rows]
  adj <- get_pct(amecol)[race_rows];  names(adj) <- rl
  ame <- get_diff(amecol)[race_rows]; names(ame) <- rl
  ref <- rl[is.na(ame)][1]
  for (lv in rl[!is.na(ame)]) {
    expect_equal(unname(adj[ref] + ame[lv]), unname(adj[lv]), tolerance = 1e-6)
  }

  # score-equation identity: the model's UNADJUSTED predicted % (avg_predictions(by = v), the observed-
  # group average) equals the same-frame Obs_% exactly. Phase 17h cut the `predicted_unadjusted` control
  # column; the identity is asserted here directly, by refitting on the model's complete-case frame.
  # inverse_two_level_factors = TRUE (default) models the FIRST level ("no") as the event, so refit on
  # that same modelled event to match the Obs_% orientation.
  dm    <- d[stats::complete.cases(d[, c("married", "race", "inc3")]), , drop = FALSE]
  fit   <- stats::glm((married == "no") ~ race + inc3, data = dm, family = stats::binomial())
  ap    <- as.data.frame(marginaleffects::avg_predictions(fit, by = "race"))
  unadj <- ap$estimate; names(unadj) <- as.character(ap$race)
  emp   <- get_pct(t[["Obs_%"]])[race_rows]; names(emp) <- rl
  expect_equal(emp[names(unadj)], unadj, tolerance = 1e-6)
})
