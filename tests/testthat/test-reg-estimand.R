# === Phase 22b-xv-1: THE CASCADE ==================================================================
#
# `family` -> `link` -> `measure` -> `effect`, each "auto" following from the left, over a library
# that is COMPOSED from four facts per family rather than written row by row. What is under test:
#
#   1. the cascade itself -- what each "auto" resolves to, and the one clause that qualifies it;
#   2. the composed library's own invariants (a coefficient IS the model's measure; the crude
#      companion pairs; no two estimands share a header);
#   3. the four typed refusals, each naming its cure;
#   4. the two routes to a risk ratio -- `link` (conditional) and `measure` (marginal) -- which are
#      different estimands, and the capability that only the cascade opens (fit on one scale, report
#      on another);
#   5. the retired surface: the `effect` value and the family spelling that now abort.

est_data <- function() {
  d <- forcats::gss_cat[!is.na(forcats::gss_cat$tvhours), ]
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

test_that("`effect = \"auto\"` takes the coefficient when the measure IS the model's, else the predictions", {
  # measure == link -> read it off the coefficients
  expect_identical(reg_estimand("binomial", measure = "odds_ratio")$effect, "conditional")
  expect_identical(reg_estimand("binomial", link = "ratio", measure = "ratio")$effect, "conditional")
  # measure != link -> from the model's predictions, sample-averaged
  expect_identical(reg_estimand("binomial", measure = "ratio")$effect,      "marginal")
  expect_identical(reg_estimand("binomial", measure = "difference")$effect, "marginal")
  expect_identical(reg_estimand("multinomial", measure = "difference")$effect, "marginal")
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

test_that("`link` takes `measure`'s own words, plus the glm spellings, silently", {
  expect_identical(reg_link_key("ratio"),      "ratio")
  expect_identical(reg_link_key("log"),        "ratio")        # the LOG LINK, not "un-exponentiated"
  expect_identical(reg_link_key("logit"),      "odds_ratio")
  expect_identical(reg_link_key("identity"),   "difference")
  expect_identical(reg_link_key("RR"),         "ratio")        # the acronyms work here too
  expect_identical(reg_link_key("IRR"),        "ratio")        # ...every one of them, since 22c-v
  expect_identical(reg_link_key("RoM"),        "ratio")
  # 22c-v: what reg_formulas() prints in `fit` is typeable back into `link`
  expect_identical(reg_link_key("rr"),         "ratio")
  expect_identical(reg_link_key("rd"),         "difference")
  expect_identical(reg_link_key("mr"),         "ratio")
  expect_identical(reg_link_key(NULL),         "auto")
  expect_null(reg_link_key("nonsense"))
  expect_setequal(REG_LINKS_VALUES, c("auto", "odds_ratio", "ratio", "difference"))
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

# The refusal mechanism 22b-xv-1 DELETED: a marginal contrast on a collapsible link targeting the
# link's own measure used to be refused as "the coefficient under another name". It builds now --
# and section 4 shows the two agree on an additive model and diverge under a non-linear shape.
test_that("no cell is refused as redundant any more", {
  for (fam in names(REG_ESTIMANDS)) for (eff in REG_CONTRAST_VALUES) {
    r <- reg_estimand(fam, effect = eff)
    expect_false(identical(r$status, "redundant"), info = paste(fam, eff))
  }
  expect_identical(reg_estimand("gaussian", measure = "difference", effect = "marginal")$status, "ok")
  expect_identical(reg_estimand("poisson",  measure = "ratio",      effect = "marginal")$status, "ok")
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

test_that("the marker rides the measure, and the log wraps the whole token", {
  w <- function(fam, eff, m) reg_word(reg_estimand(fam, measure = m, effect = eff))
  expect_identical(w("binomial", "conditional",  "odds_ratio"), "OR")
  expect_identical(w("binomial", "marginal",     "ratio"),      "mRR")
  expect_identical(w("binomial", "at_reference", "difference"), "refRD")
  expect_identical(w("binomial", "conditional",  "log"),        "log(OR)")
  expect_identical(w("ordinal",  "conditional",  "odds_ratio"), "cumOR")   # cumulative, and it says so
  expect_identical(w("gaussian", "conditional",  "difference"), "diff")    # never a bare greek letter
  expect_identical(w("poisson",  "at_reference", "ratio"),      "refIRR")  # ONE ratio word per family
  # the log wraps the MARKED token; `log_odds` pins the odds base auto would not reach at a profile
  expect_identical(w("multinomial", "at_reference", "log_odds"), "log(refOR)")
  # the expansion is one declared string per acronym, wrapped the way each form is spoken
  expect_identical(reg_word_long(reg_estimand("binomial", measure = "ratio", effect = "marginal")),
                   "marginal risk ratio")
  expect_identical(reg_word_long(reg_estimand("binomial", measure = "log", effect = "conditional")),
                   "log odds ratio")
})

test_that("the colour legend names the measure, never the contrast (one block per comparison)", {
  # ⚠ load-bearing: legend_group_by_body() groups by the rendered sentence, so a crude column reading
  # "RR" beside a model column reading "mRR" would split the block the crude/adjusted merge produces.
  # ⚠ a CONDITIONAL risk ratio is the modified Poisson, i.e. `link`'s: on the logit fit the same
  # measure is only reachable from the predictions, which is the cascade's whole point.
  expect_identical(reg_legend_word(reg_estimand("binomial", link = "ratio", measure = "ratio")), "RR")
  for (eff in c("marginal", "at_reference")) {
    r <- reg_estimand("binomial", measure = "ratio", effect = eff)
    expect_identical(reg_legend_word(r), "RR", info = eff)
  }
  expect_identical(reg_legend_word(reg_estimand("ordinal", measure = "odds_ratio")), "cumOR")
})

test_that("the crude column names the measure alone, from its OWN shape", {
  # it is a univariable effect: no contrast marker, and never the model's word when the two differ
  cw <- function(fam, eff, m, ...) {
    e <- reg_estimand(fam, measure = m, effect = eff, ...)
    k <- if (identical(e$crude_fam, "auto")) reg_crude_key(e$fit) else e$crude_fam
    reg_crude_col_name(reg_crude_shape(k, e))
  }
  expect_identical(cw("binomial", "marginal",     "ratio"),      "Obs_RR")
  expect_identical(cw("binomial", "at_reference", "difference"), "Obs_RD")
  expect_identical(cw("binomial", "conditional",  "log"),        "Obs_log(OR)")
  expect_identical(cw("gaussian", "marginal",     "difference"), "Obs_diff")
  # a poisson AME is additive, and so is its crude companion: the observed mean difference
  expect_identical(cw("poisson",  "marginal",     "difference"), "Obs_diff")
  # a NON-default link keeps the pairing: the crude twin follows the REPORTED measure, not the fit
  expect_identical(cw("binomial", "conditional",  "ratio", link = "ratio"),      "Obs_RR")
  expect_identical(cw("binomial", "marginal",     "difference", link = "ratio"), "Obs_RD")
})


# --- 3. the four refusals, each naming its cure ---------------------------------------------------

test_that("a measure the LEVEL cannot carry is refused as not defined", {
  imp <- reg_estimand("gaussian", measure = "odds_ratio", effect = "conditional")
  expect_identical(imp$status, "impossible")
  expect_match(imp$why(), "odds")
  expect_match(reg_estimand("poisson", measure = "odds_ratio")$why(), "count")
  expect_error(reg_estimand_abort(imp), "not defined")
})

test_that("a link the family cannot fit is refused, naming the ones it can", {
  r <- reg_estimand("multinomial", link = "difference")
  expect_identical(r$status, "no_link")
  expect_error(reg_estimand_abort(r), "fits no")
  expect_error(reg_estimand_abort(r), 'link = "odds_ratio"', fixed = TRUE)
})

test_that("a coefficient that is not the model's measure names its TWO cures", {
  r <- reg_estimand("binomial", measure = "difference", effect = "conditional")
  expect_identical(r$status, "no_coefficient")
  expect_error(reg_estimand_abort(r), "cannot be read off its coefficients")
  expect_error(reg_estimand_abort(r), 'link = "difference"', fixed = TRUE)
  # where the family fits no such link, the predictions are the only route, and it says so
  expect_error(reg_estimand_abort(reg_estimand("multinomial", measure = "difference",
                                               effect = "conditional")),
               "fits no")
})

test_that("a PREDICTED odds ratio needs a percentage and its complement", {
  expect_identical(reg_estimand("binomial", measure = "odds_ratio", effect = "marginal")$status, "ok")
  for (fam in c("multinomial", "ordinal")) {
    r <- reg_estimand(fam, measure = "odds_ratio", effect = "marginal")
    expect_identical(r$status, "not_offered", info = fam)
    expect_match(r$why(), "complement", info = fam)
  }
  # the vs-rest builder IS the one answer a 3+ category outcome has to "versus what?"
  expect_identical(reg_estimand("multinomial", measure = "odds_ratio",
                                effect = "at_reference")$builder, "vsrest")
})

test_that("the measure aliases work both ways and `log` pins its base", {
  expect_identical(reg_measure_key("OR")$measure,  "odds_ratio")
  expect_identical(reg_measure_key("IRR")$measure, "ratio")
  expect_identical(reg_measure_key("RD")$measure,  "difference")
  expect_identical(reg_measure_key("RoM")$measure, "ratio")
  expect_identical(reg_measure_key("rom")$measure, "ratio")   # the twin is DERIVED, not listed
  expect_null(reg_measure_key("nonsense"))
  # 22c-v: ONE vocabulary, so no case folding and no spelling that is not a header word
  expect_null(reg_measure_key("Difference"))
  expect_null(reg_measure_key("DIFF"))
  expect_null(reg_measure_key("MR"))          # a fit key names a MODEL: it belongs to `link`
  expect_null(reg_measure_key("risk_ratio"))  # the taught long form is the concept word
  expect_null(reg_measure_key("d"))           # a one-letter acronym gets no lowercase twin
  # bare "coefficient" takes whatever the cascade would report; log_risk pins the risk-ratio base
  expect_identical(reg_estimand("binomial", measure = "coefficient")$word,  "OR")
  expect_identical(reg_estimand("binomial", measure = "log")$word,          "OR")   # a spelling
  expect_identical(reg_estimand("binomial", measure = "log_risk")$word,     "RR")
  expect_identical(reg_estimand("binomial", link = "ratio", measure = "coefficient")$fit, "rr")
  # Phase 22g-iii: `coefficient` is TOTAL. On a link that is ALREADY additive there is nothing to
  # un-exponentiate, so it falls through to the additive row itself rather than refusing -- which is
  # what lets one table mixing a logistic and a linear outcome be asked for its coefficients.
  g <- reg_estimand("gaussian", measure = "coefficient", effect = "conditional")
  expect_identical(g$status, "ok")
  expect_identical(g, reg_estimand("gaussian", measure = "difference", effect = "conditional"))
})

test_that("reg_measures() lists an outcome's estimands at ONE link, with a status for each", {
  skip_if_not_installed("broom")
  m <- suppressMessages(reg_measures(est_data(), "married"))
  expect_true(all(c("effect", "measure", "status", "header") %in% names(m)))
  expect_identical(m$header[m$effect == "conditional" & m$measure == "odds_ratio"], "Model_OR")
  # the SAME grid at another model: the measure that had no coefficient now has one
  r <- suppressMessages(reg_measures(est_data(), "married", link = "ratio"))
  expect_identical(r$header[r$effect == "conditional" & r$measure == "ratio"], "Model_RR")
  expect_identical(m$status[m$effect == "conditional" & m$measure == "ratio"], "not offered")
  # a continuous outcome is where "not defined" shows: an odds ratio of a mean is not a thing
  g <- suppressMessages(reg_measures(est_data(), "tvhours"))
  expect_identical(g$status[g$effect == "conditional" & g$measure == "odds_ratio"], "not defined")
  expect_match(g$note[g$effect == "conditional" & g$measure == "odds_ratio"], "odds")
  # the generated ?tab_reg section reads the same table
  expect_true(any(grepl("link = ", reg_measures_rd(), fixed = TRUE)))
})


# --- 4. the two routes to a ratio, and what only the cascade opens --------------------------------

test_that("`link` names the model and `measure` the report: two different risk ratios", {
  skip_if_not_installed("broom"); skip_if_not_installed("survey")
  d <- est_data()
  cond <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                   link = "ratio", empirical = TRUE, cleannames = FALSE))
  marg <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                   measure = "ratio", empirical = TRUE, cleannames = FALSE))
  expect_true("Model_RR"  %in% names(cond))     # the modified Poisson's own coefficient
  expect_true("Model_mRR" %in% names(marg))     # g-computed from the logistic fit
  expect_identical(reg_formulas(cond)$fit, "rr")
  expect_identical(reg_formulas(marg)$fit, "binomial")
  # both are risk ratios, so both sit on the same scale and pair with the same crude column
  expect_identical(get_scale(modcol(cond)), "pct_ratio")
  expect_identical(get_scale(modcol(marg)), "pct_ratio")
  expect_true(all(c("Obs_RR") %in% names(cond)) && "Obs_RR" %in% names(marg))
})

test_that("a prediction route may run on a NON-default fit -- link and measure are separate axes", {
  skip_if_not_installed("broom"); skip_if_not_installed("survey")
  d <- est_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                link = "ratio", measure = "difference", cleannames = FALSE))
  expect_true("Model_mRD" %in% names(t))
  expect_identical(get_scale(modcol(t)), "points")
  expect_identical(reg_formulas(t)$fit, "rr")        # the model is the modified Poisson...
  expect_identical(reg_call(t)$link, "ratio")        # ...and the table remembers it
  expect_identical(reg_call(t)$measure, "difference")
})

test_that("measure = 'log' == the old exponentiate = FALSE", {
  skip_if_not_installed("broom")
  d <- est_data()
  t <- suppressMessages(tab_reg(d, "married", c("race"), family = "binomial", measure = "log",
                                cleannames = FALSE))
  cf <- modcol(t)
  expect_identical(get_scale(cf), "log_coef")
  # the same numbers as the exponentiated column, logged
  e  <- suppressMessages(tab_reg(d, "married", c("race"), family = "binomial", cleannames = FALSE))
  ec <- modcol(e)
  keep <- !is.na(get_or(ec)) & get_or(ec) > 0
  expect_equal(get_diff(cf)[keep], log(get_or(ec)[keep]), tolerance = 1e-10)
})

test_that("link = 'difference' on a binary outcome is the identity-link risk difference", {
  skip_if_not_installed("broom"); skip_if_not_installed("survey")
  d  <- est_data()
  t  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "difference",
                                 empirical = TRUE, cleannames = FALSE))
  mc <- modcol(t)
  expect_identical(get_scale(mc), "points")            # percentage points, not a ratio
  expect_true("Model_RD" %in% names(t))
  expect_true("Obs_RD" %in% names(t))                  # the crude twin is the crude risk difference
  # the coefficients ARE an identity-link binomial glm's
  dd <- stats::na.omit(d[, c("married", "race")])
  g  <- suppressWarnings(stats::glm(married ~ race, data = dd, family = stats::binomial("identity"),
                                    start = stats::coef(stats::lm(married ~ race, dd))))
  i  <- which(as.character(t$var) == "race" & !is_refrow(mc))
  expect_equal(sort(get_diff(mc)[i]), sort(unname(stats::coef(g))[-1]), tolerance = 1e-6)
})

test_that("link = 'ratio' on a continuous outcome is the ratio of adjusted means", {
  skip_if_not_installed("broom"); skip_if_not_installed("survey")
  d  <- est_data()
  t  <- suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian", link = "ratio",
                                 empirical = TRUE, cleannames = FALSE))
  mc <- modcol(t)
  expect_identical(get_scale(mc), "mean_ratio")        # the ratio field, not `or`
  expect_true("Obs_RoM" %in% names(t))
  dd <- stats::na.omit(d[, c("tvhours", "race")])
  g  <- suppressWarnings(stats::glm(tvhours ~ race, data = dd, family = stats::quasipoisson("log")))
  i  <- which(as.character(t$var) == "race" & !is_refrow(mc))
  expect_equal(sort(get_ratio(mc)[i]), sort(exp(unname(stats::coef(g)))[-1]), tolerance = 1e-6)
  # a negative outcome has no ratio of means, and says so rather than fitting nonsense
  d2 <- d; d2$neg <- d2$tvhours - 5
  expect_error(suppressMessages(tab_reg(d2, "neg", "race", family = "gaussian", link = "ratio")),
               "non-negative")
})

test_that("effect = 'marginal' + measure = 'ratio' is the exponentiated lnratioavg", {
  skip_if_not_installed("marginaleffects")
  d <- est_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  col <- modcol(t)
  # a RISK ratio is a ratio of percentages, so it sits on `pct_ratio` and prints "x2" like every
  # other ratio -- `odds_ratio` and its "1/x" notation are the odds ratio's alone.
  expect_identical(get_scale(col), "pct_ratio")
  g  <- stats::glm(married ~ race, data = d, family = stats::binomial())
  m  <- marginaleffects::avg_comparisons(g, variables = "race", comparison = "lnratioavg")
  i  <- which(as.character(t$var) == "race" & !is_refrow(col))
  expect_equal(sort(tabxplor:::fmt_est_of(col)[i]), sort(exp(m$estimate)), tolerance = 1e-8)
})

# The one estimand the generalised marginal engine ADDS (Karlson & Jann 2023): the odds ratio of the
# two adjusted predictions, which is the odds-flavoured measure that behaves under adjustment.
test_that("the MARGINAL odds ratio matches marginaleffects' own lnoravg", {
  skip_if_not_installed("marginaleffects")
  d  <- est_data()
  dd <- stats::na.omit(d[, c("married", "race", "age")])
  for (fm in c("married ~ race + age", "married ~ race * age")) {
    g  <- stats::glm(stats::as.formula(fm), data = dd, family = stats::binomial())
    me <- marginaleffects::avg_comparisons(g, variables = "race", comparison = "lnoravg")
    gc <- tabxplor:::reg_gcomp_maker(g, dd, NULL, "logit")
    lv <- levels(dd$race)
    est <- vapply(lv[-1], function(l) gc("race", l, lv[[1]])$est, numeric(1))
    se  <- vapply(lv[-1], function(l)
      tabxplor:::reg_delta_se(gc("race", l, lv[[1]])$G, stats::vcov(g)), numeric(1))
    expect_equal(sort(unname(est)), sort(me$estimate),  tolerance = 1e-8, info = fm)
    expect_equal(sort(unname(se)),  sort(me$std.error), tolerance = 1e-6, info = fm)
  }
  t <- suppressMessages(tab_reg(dd, "married", c("race", "age"), family = "binomial",
                                effect = "marginal", measure = "odds_ratio", cleannames = FALSE))
  expect_true("Model_mOR" %in% names(t))
  expect_identical(get_scale(modcol(t)), "odds_ratio")
})

# What the deleted redundancy refusal used to hide, now demonstrable rather than asserted.
test_that("a linear model's marginal effect IS its coefficient -- until the shape is not linear", {
  skip_if_not_installed("broom")
  d <- stats::na.omit(est_data()[, c("tvhours", "race", "age")])
  co <- suppressMessages(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                                 cleannames = FALSE))
  ma <- suppressMessages(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                                 effect = "marginal", measure = "difference", cleannames = FALSE))
  expect_equal(get_diff(co$Model_diff), get_diff(ma$Model_mdiff), tolerance = 1e-10)
  # a non-linear shape breaks the identity: the coefficient is the linear term, the AME the average
  ci <- suppressMessages(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                                 shape = c(age = "quadratic"), cleannames = FALSE))
  mi <- suppressMessages(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                                 shape = c(age = "quadratic"), effect = "marginal", measure = "difference",
                                 cleannames = FALSE))
  age_of <- function(t, nm) get_diff(t[[nm]])[as.character(t$var) == "age"][[1]]
  expect_false(isTRUE(all.equal(age_of(ci, "Model_diff"), age_of(mi, "Model_mdiff"))))
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
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio",
                                cleannames = FALSE))
  rc <- reg_call(t)
  expect_identical(rc$link,    "ratio")
  expect_identical(rc$measure, "ratio")
  expect_identical(rc$effect,  "conditional")
  expect_identical(unname(rc$links[["married"]]),    "ratio")
  expect_identical(unname(rc$measures[["married"]]), "ratio")
  # ... and the footer sentence is generated from it
  expect_match(reg_model_lines(t)[[1]], "risk ratio|rapports de risque")
})
