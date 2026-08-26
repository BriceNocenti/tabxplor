# PURPOSE: the family -> link -> measure -> effect cascade, its composed header words and its refusals.
# ROLE: the shipped CONTRACT for R/reg-estimand.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the family -> link -> measure -> effect cascade =====================================

est_data <- function() {
  d <- fx_reg_df()[!is.na(fx_reg_df()$tvhours), ]
  d <- d[d$race != "Not applicable", ]
  d$race    <- droplevels(d$race)
  d$married <- as.integer(d$marital == "Married")
  d[seq_len(min(nrow(d), 4000)), ]
}



fmtcols <- function(t) names(t)[vapply(t, is_fmt, logical(1))]


render  <- function(t) lapply(t[fmtcols(t)], format)


modcol  <- function(t) t[[grep("^Model", names(t), value = TRUE)[[1]]]]




# --- 1. the cascade -------------------------------------------------------------------------------

test_that("the default call is the family's own link, measure and coefficient", {
  want <- list(gaussian    = c("difference",  "difference", "gaussian",    "diff"),
               binomial    = c("odds_ratio",  "odds_ratio", "binomial",    "OR"),
               poisson     = c("ratio",       "ratio",      "poisson",     "IRR"),
               multinomial = c("odds_ratio",  "odds_ratio", "multinomial", "OR"),
               ordinal     = c("odds_ratio",  "odds_ratio", "ordinal",     "cumOR"))
  for (fam in names(want)) {
    e <- reg_estimand(fam)
    expect_identical(c(e$link, e$measure, e$fit, reg_word(e)), want[[fam]], info = fam)
    expect_identical(e$effect, "conditional", info = fam)
  }
})



test_that("`measure = \"auto\"` follows the link -- except that it never PREDICTS an odds ratio", {
  # follow from the left, on both prediction routes
  expect_identical(reg_estimand("poisson",  effect = "marginal")$measure,     "ratio")
  expect_identical(reg_estimand("gaussian", effect = "marginal")$measure,     "difference")
  expect_identical(reg_estimand("gaussian", effect = "at_reference")$measure, "difference")
  expect_identical(reg_estimand("binomial", link = "ratio", effect = "marginal")$measure, "ratio")
  # ... and the one clause: a non-collapsible link falls back to the LEVEL's own measure, on BOTH
  # prediction routes. A marginal odds ratio is a specialist quantity: asked for by name, never auto.
  for (fam in c("binomial", "multinomial"))
    for (eff in c("marginal", "at_reference"))
      expect_identical(reg_estimand(fam, effect = eff)$measure, "ratio", info = paste(fam, eff))
  # an ORDINAL outcome's level is a RANK, whose own measure is Somers' D; and its pair is drawn from
  # the population, so there is no at_reference row for the fallback to land on at all.
  expect_identical(reg_estimand("ordinal", effect = "marginal")$measure, "difference")
  expect_identical(reg_estimand("ordinal", effect = "at_reference")$status, "not_offered")
  # the clause reads REG_WORDS' declared flag, so it is the same fact the adjustment caveat reads
  expect_true(reg_word_noncollapsible("OR"))
  expect_false(reg_word_noncollapsible("RR"))
})




# --- 2. the composed library ----------------------------------------------------------------------

test_that("every composed row is buildable, and a coefficient row IS the model's own measure", {
  for (fam in names(REG_ESTIMANDS)) for (r in REG_ESTIMANDS[[fam]]$rows) {
    info <- paste(fam, r$link, r$effect, r$measure)
    expect_identical(r$status, "ok",            info = info)
    expect_true(r$scale %in% EST_SCALE_KEYS,    info = info)
    expect_true(r$word  %in% names(REG_WORDS),  info = info)
    expect_true(r$builder %in% REG_BUILDERS,    info = info)
    expect_true(r$fit %in% names(REG_FAMILIES), info = info)
    expect_true(r$link %in% names(REG_FAMILIES[[fam]]$fits), info = info)
    expect_identical(unname(r$fit), unname(REG_FAMILIES[[fam]]$fits[[r$link]]), info = info)
    if (identical(r$effect, "conditional"))
      expect_identical(r$base_measure, r$link, info = info)
  }
})



test_that("every buildable estimand has a distinct, composed header word", {
  seen <- list()
  for (fam in setdiff(names(REG_ESTIMANDS), "quasipoisson")) {
    for (lk in names(REG_FAMILIES[[fam]]$fits))
      for (eff in REG_CONTRAST_VALUES) for (m in setdiff(REG_MEASURES_VALUES, "auto")) {
        r <- reg_estimand(fam, link = lk, measure = m, effect = eff)
        if (!identical(r$status, "ok")) next
        w <- reg_word(r)
        # the word is COMPOSED: the base acronym is declared, the marker and the log wrapper are not
        expect_identical(reg_word_base(w), r$word, info = paste(fam, lk, eff, m))
        expect_true(nzchar(reg_word_long(r)), info = paste(fam, lk, eff, m))
        # ... and it identifies the estimand: one word never names two (effect, measure) pairs
        k <- paste(fam, w)
        if (!is.null(seen[[k]])) expect_identical(seen[[k]], c(eff, r$base_measure), info = k)
        seen[[k]] <- c(eff, r$base_measure)
      }
  }
})




# --- 3. the four refusals, each naming its cure ---------------------------------------------------

test_that("a measure the LEVEL cannot carry is refused as not defined", {
  imp <- reg_estimand("gaussian", measure = "odds_ratio", effect = "conditional")
  expect_identical(imp$status, "impossible")
  expect_match(imp$why(), "odds")
  expect_match(reg_estimand("poisson", measure = "odds_ratio")$why(), "count")
  expect_error(reg_estimand_abort(imp), "not defined")
})



test_that("reg_measures() factors the grid: one row per model, then the predictions once", {
  m <- suppressMessages(reg_measures(est_data(), "married", link = "all"))
  expect_true(all(c("link", "measure", "effect", "header", "reads_as") %in% names(m)))
  # ONE family -> no `family` column; the conditional block is one row per fittable link
  expect_false("family" %in% names(m))
  expect_identical(m$link[m$effect == "conditional"], names(REG_FAMILIES$binomial$fits))
  # `link = "all"` is what raises the question the `base_link` column answers
  expect_identical(m$base_link, c(TRUE, FALSE, FALSE, NA, NA, NA))
  # ...and the DEFAULT reads the family's own model alone, which is what makes the table readable
  d <- suppressMessages(reg_measures(est_data(), "married"))
  expect_false("base_link" %in% names(d))
  expect_identical(d$link[d$effect == "conditional"], reg_family_link("binomial"))
  expect_lt(nrow(d), nrow(m))
  expect_identical(m$header[m$effect == "conditional" & m$measure == "odds_ratio"], "Model_OR")
  # ...and the prediction block is listed ONCE, at no link in particular
  expect_true(all(m$link[m$effect != "conditional"] == "(any)"))
  expect_false(any(duplicated(m[m$link == "(any)", c("measure")])))
  # a measure this outcome cannot carry has NO row (the old "not defined" status): a mean has no odds
  g <- suppressMessages(reg_measures(est_data(), "tvhours", family = "gaussian", link = "all"))
  expect_false("odds_ratio" %in% g$measure)
  # `family = "auto"` lists every family the outcome KIND offers, the detected one first
  a <- suppressMessages(reg_measures(est_data(), "tvhours"))
  expect_identical(unique(a$family), REG_OUTCOME_KINDS$numeric$offers)
  # a raw coefficient is not a row: it is a reading of any conditional one (Phase 22g-v)
  expect_false("raw_coefficient" %in% m$measure)
  # the generated ?tab_reg section reads the same table
  expect_true(any(grepl("link = ", reg_measures_rd(), fixed = TRUE)))
})




# --- 5. the retired surface -----------------------------------------------------------------------

test_that("a retired estimand argument, effect value or family spelling aborts (no silent no-op)", {
  d <- est_data()
  expect_error(tab_reg(d, "married", "race", family = "binomial", exponentiate = FALSE),
               "[Uu]nknown argument")
  expect_error(tab_reg(d, "married", "race", family = "binomial", at = "reference"),
               "[Uu]nknown argument")
  expect_error(tab_reg(d, "married", "race", family = "binomial", effect = "ame"),
               "[Uu]nknown .*effect")
  # `effect = "coefficient"` named the artefact; the value names the quantity now
  expect_error(tab_reg(d, "married", "race", family = "binomial", effect = "coefficient"),
               "conditional")
  # `family` answers ONE question, and never secretly picks a link
  expect_error(suppressMessages(tab_reg(d, "married", "race", family = "poisson")),
               'link = "ratio"', fixed = TRUE)
  # an unknown measure or link enumerates the legal ones
  expect_error(tab_reg(d, "married", "race", family = "binomial", measure = "nonsense"),
               "odds_ratio")
  expect_error(tab_reg(d, "married", "race", family = "binomial", link = "nonsense"),
               "[Uu]nknown .*link")
})




# === SECTION: the invariants every regression cell satisfies ======================================

inv_data <- function(n = 2000) {
  d <- fx_reg_fmt()
  d <- d[!is.na(d$married) & !is.na(d$party3) & !is.na(d$rincome) &
           !is.na(d$race) & !is.na(d$age) & !is.na(d$tvhours), ]
  withr::with_seed(20260820, d[sample(nrow(d), min(n, nrow(d))), ])
}



inv_tea <- function() {
  tea   <- as.data.frame(facto_tea)
  items <- c("home", "tearoom", "work", "friends", "resto", "pub")
  tea$tea_where <- rowSums(vapply(items, function(v) as.integer(tea[[v]] == v),
                                  integer(nrow(tea))))
  tea$sex <- factor(tea$sex); tea$SPC <- factor(tea$SPC)
  tea
}



# every fmt column that carries an estimate: the model columns and their crude twins.
inv_cols <- function(t)
  names(t)[vapply(t, function(x) is_fmt(x) && get_role(x) %in% c("model", "emp"), logical(1))]



inv_check <- function(t, tag) {
  cols <- inv_cols(t)
  expect_true(length(cols) > 0L, info = tag)
  for (cn in cols) {
    col  <- t[[cn]]
    scr  <- EST_SCALES[[get_scale(col)]]
    est  <- fmt_est_of(col)
    lo   <- get_ci_inf(col); hi <- get_ci_sup(col); p <- get_pvalue(col)
    who  <- paste0(tag, " / ", cn, " [", get_scale(col), "]")
    # a cell with no interval says nothing; every invariant is about the cells that have one.
    ok <- is.finite(est) & is.finite(lo) & is.finite(hi)
    expect_true(all(est[ok] >= lo[ok] - 1e-9), info = paste(who, "-- estimate below its interval"))
    expect_true(all(est[ok] <= hi[ok] + 1e-9), info = paste(who, "-- estimate above its interval"))
    if (is.na(scr$neutral)) next
    # the Constant is a BASELINE, not a comparison: it is a reference row with no neutral to hold.
    ref <- is_refrow(col) & as.character(t$var) != "Constant" & is.finite(est)
    expect_true(all(abs(est[ref] - scr$neutral) < 1e-9),
                info = paste(who, "-- a reference cell is not the scale's neutral"))
    okp <- ok & is.finite(p)
    expect_identical(p[okp] < 0.05,
                     lo[okp] > scr$neutral + 1e-12 | hi[okp] < scr$neutral - 1e-12,
                     info = paste(who, "-- a star disagrees with its interval"))
  }
}



test_that("every family's cells hold one estimand: interval, neutral and star agree", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  d <- inv_data()
  # family x LINK x contrast x measure -> the one case that exercises each producer. Since the
  # cascade, `link` and `measure` are separate axes, so both routes to a ratio are swept: the
  # model's own coefficient (`link`) and the same measure read off its predictions (`measure`).
  cases <- list(
    list(tag = "gaussian coef diff",   a = list(d, "age",     c("race", "tvhours"),
                                                family = "gaussian")),
    list(tag = "gaussian coef RoM",    a = list(d, "age",     c("race", "tvhours"),
                                                family = "gaussian", link = "ratio")),
    list(tag = "gaussian marg RoM",    a = list(d, "age",     c("race", "tvhours"),
                                                family = "gaussian", measure = "ratio")),
    list(tag = "binomial coef OR",     a = list(d, "married", c("race", "age"),
                                                family = "binomial")),
    list(tag = "binomial coef RR",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", link = "ratio")),
    list(tag = "binomial coef RD",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", link = "difference")),
    list(tag = "binomial marg RR",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", effect = "marginal")),
    list(tag = "binomial marg RD",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", effect = "marginal",
                                                measure = "difference")),
    # the estimand the generalised marginal engine added (Karlson & Jann 2023)
    list(tag = "binomial marg OR",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", effect = "marginal",
                                                measure = "odds_ratio")),
    # fit on ONE scale, report on ANOTHER -- the capability only the cascade opens
    list(tag = "binomial rr -> mRD",   a = list(d, "married", c("race", "age"),
                                                family = "binomial", link = "ratio",
                                                measure = "difference")),
    list(tag = "poisson coef IRR",     a = list(d, "tvhours", c("race", "age"),
                                                family = "poisson")),
    list(tag = "multinomial coef OR",  a = list(d, "party3",  c("race", "age"),
                                                family = "multinomial")),
    list(tag = "ordinal coef cumOR",   a = list(d, "rincome", c("race", "age"),
                                                family = "ordinal")),
    # the two LOGGED contrasts: `log_coef` is one shared scale, so a wrong twin lands here
    list(tag = "binomial coef log(OR)", a = list(d, "married", c("race", "age"),
                                                 family = "binomial", measure = "log")),
    # ⚠ a raw coefficient is the model's OWN (Phase 22g-v), so the log of a RISK ratio is asked of
    # the model that estimates one -- the modified Poisson -- and not of a logit model's predictions
    list(tag = "binomial coef log(RR)", a = list(d, "married", c("race", "age"),
                                                 family = "binomial", link = "ratio",
                                                 measure = "log_risk")),
    # a SUMMED SCORE, whose crude effect sits on the mean score rather than a share
    list(tag = "grouped binomial RR",   a = list(inv_tea(), "tea_where", c("sex", "SPC"),
                                                 family = "binomial", trials = 6,
                                                 link = "ratio"))
  )
  for (cs in cases) {
    t <- suppressWarnings(suppressMessages(
      do.call(tab_reg, c(cs$a, list(empirical = "column", stats = FALSE)))))
    inv_check(t, cs$tag)
  }
})




# === SECTION: the Constant row ====================================================================

bl_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}


bl_first <- function(t) t[[grep("^Model", names(t))[[1]]]]


bl_cst   <- function(t) which(as.character(t$var) == "Constant")




# === SECTION: the modified-Poisson risk ratio =====================================================

est_of <- function(x) tabxplor:::fmt_est_of(x)



rr_data <- function() {
  d <- fx_reg_df()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$inc3    <- factor(dplyr::case_when(d$rincome %in% c("$25000 or more") ~ "hi",
                                       d$rincome %in% c("Not applicable", "No answer",
                                                        "Don't know", "Refused") ~ NA_character_,
                                       TRUE ~ "lo"),
                      levels = c("lo", "hi"))
  d <- d[!is.na(d$married) & !is.na(d$race) & !is.na(d$inc3) & !is.na(d$tvhours), , drop = FALSE]
  tibble::as_tibble(d)
}



# The 0/1 numeric the "rr" arm actually fits: reg_prep_binary picks the modelled ("positive") level,
# honouring the modelled level (`outcome_level`), then coerces to numeric.
rr_y01 <- function(d, dep = "married", inverse = TRUE)
  as.numeric(as.character(d[[dep]]) == reg_positive_level(d, dep, inverse))



# ---- (1) modified Poisson: the fit and its variance ------------------------------------------------

test_that("link='ratio' on a binary outcome fits the modified Poisson, and is named a risk ratio", {
  d <- rr_data()
  # `link` names the measure the MODEL estimates, so the conditional risk ratio is one argument away
  # -- and `family` no longer secretly picks a link (the retired `family = "poisson"` spelling).
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio"))
  expect_error(suppressMessages(tab_reg(d, "married", "race", family = "poisson")),
               'link = "ratio"', fixed = TRUE)
  # the column is Model_RR (not Model_IRR, not Model_mRR: the coefficient is unmarked), and the
  # estimand prose says so
  expect_true("Model_RR" %in% names(t))
  expect_false(any(grepl("IRR", names(t))))
  note <- reg_estimand_note(reg_estimand("binomial", link = "ratio"))
  expect_match(note, "RR: risk ratio")
  expect_no_match(note, "incidence-rate")
  # Sociology terminology trap: "log-linear model" means Goodman's contingency-table models.
  expect_no_match(reg_family_display_name("rr"), "log-linear")
  expect_equal(reg_family_display_name("rr"), "modified Poisson regression")
})



test_that("the estimand invariant holds: the OR is always further from 1 than the RR", {
  d  <- rr_data()
  or <- get_or(suppressMessages(tab_reg(d, "married", "race", family = "binomial"))$Model_OR)[3:4]
  rr <- est_of(suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio"))$Model_RR)[3:4]
  # The OR always EXAGGERATES, away from 1, whichever side the effect falls on -- stated
  # direction-agnostically as |log(OR)| > |log(RR)|, and both must sit on the same side of 1.
  expect_true(all(abs(log(or)) > abs(log(rr))))
  expect_true(all(sign(log(or)) == sign(log(rr))))
})
