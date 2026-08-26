
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


test_that("the marker rides the measure, and the log wraps the whole token", {
  w <- function(fam, eff, m) reg_word(reg_estimand(fam, measure = m, effect = eff))
  expect_identical(w("binomial", "conditional",  "odds_ratio"), "OR")
  expect_identical(w("binomial", "marginal",     "ratio"),      "mRR")
  expect_identical(w("binomial", "at_reference", "difference"), "refRD")
  expect_identical(w("binomial", "conditional",  "log"),        "log(OR)")
  expect_identical(w("ordinal",  "conditional",  "odds_ratio"), "cumOR")   # cumulative, and it says so
  expect_identical(w("gaussian", "conditional",  "difference"), "diff")    # never a bare greek letter
  expect_identical(w("poisson",  "at_reference", "ratio"),      "refIRR")  # ONE ratio word per family
  # Phase 22g-v: a raw coefficient is the model's OWN, so there is no marginal or at-reference one
  expect_identical(w("multinomial", "at_reference", "log_odds"), "")
  # the expansion is one declared string per acronym, wrapped the way each form is spoken
  expect_identical(reg_word_long(reg_estimand("binomial", measure = "ratio", effect = "marginal")),
                   "marginal risk ratio")
  expect_identical(reg_word_long(reg_estimand("binomial", measure = "log", effect = "conditional")),
                   "log odds ratio")
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
  # Phase 22g-v: `log_risk` pins the RISK-ratio base, which a logit model does not estimate -- so it
  # is refused, naming the model that does rather than logging a marginal ratio
  expect_identical(reg_estimand("binomial", measure = "log_risk")$status, "no_raw_coefficient")
  expect_identical(reg_estimand("binomial", link = "ratio", measure = "log_risk")$word, "RR")
  expect_identical(reg_estimand("binomial", link = "ratio", measure = "coefficient")$fit, "rr")
  # Phase 22g-iii: `coefficient` is TOTAL. On a link that is ALREADY additive there is nothing to
  # un-exponentiate, so it falls through to the additive row itself rather than refusing -- which is
  # what lets one table mixing a logistic and a linear outcome be asked for its coefficients.
  g <- reg_estimand("gaussian", measure = "coefficient", effect = "conditional")
  expect_identical(g$status, "ok")
  expect_identical(g, reg_estimand("gaussian", measure = "difference", effect = "conditional"))
})


test_that("a prediction route may run on a NON-default fit -- link and measure are separate axes", {
  skip_if_not_installed("survey")
  d <- est_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                link = "ratio", measure = "difference", cleannames = FALSE))
  expect_true("Model_mRD" %in% names(t))
  expect_identical(get_scale(modcol(t)), "points")
  expect_identical(reg_formulas(t)$fit, 'svyglm(quasipoisson("log"))')  # the modified Poisson...
  expect_identical(reg_call(t)$link, "ratio")        # ...and the table remembers it
  expect_identical(reg_call(t)$measure, "difference")
})


test_that("measure = 'log' == the old exponentiate = FALSE", {
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
  skip_if_not_installed("survey")
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
  skip_if_not_installed("survey")
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


test_that("D25: a reg colour cannot contradict what the column estimates", {
  d <- est_data()
  expect_error(tab_reg(d, "married", "race", family = "binomial", color = "difference"),
               "not a .*tab_reg.* colour")
  expect_error(tab_reg(d, "married", "race", family = "binomial", color = "odds_ratio"),
               "adjustment")
  # what remains is what to compare the effect TO
  expect_identical(reg_normalize_color(TRUE),  NA_character_)
  expect_identical(reg_normalize_color(FALSE), "no")
  expect_identical(reg_normalize_color(c(TRUE, "adjustment")), c(NA_character_, "adjustment"))
})


test_that("D6: the multi-dependent x model-list recursion forwards every argument", {
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


test_that("a logged column is the log of its exponentiated twin, cell for cell", {
  skip_if_not_installed("marginaleffects")
  d <- inv_data()
  # the path derives its contrast from `comparison` but its SCALE from the estimand: the two were one
  # flag, which printed ratios on a column stamped `log_coef`.
  # ⚠ Phase 22g-v: at the model that ESTIMATES a risk ratio, a raw coefficient having no marginal
  # form -- which leaves the pair (log column, exponentiated twin) exactly as it was.
  arg <- list(d, "married", c("race", "age"), family = "binomial", link = "ratio",
              empirical = "column", stats = FALSE)
  lg <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "log_risk"))))
  rr <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "ratio"))))
  for (role in c("model", "emp")) {
    lc <- lg[[inv_cols(lg)[vapply(inv_cols(lg), function(n) get_role(lg[[n]]) == role,
                                  logical(1))][[1]]]]
    rc <- rr[[inv_cols(rr)[vapply(inv_cols(rr), function(n) get_role(rr[[n]]) == role,
                                  logical(1))][[1]]]]
    expect_identical(get_scale(lc), "log_coef")
    expect_identical(get_scale(rc), "pct_ratio")
    fin <- is.finite(get_ratio(rc)) & is.finite(get_diff(lc))
    expect_equal(get_diff(lc)[fin], log(get_ratio(rc))[fin], tolerance = 1e-12)
    expect_equal(get_ci_inf(lc), log(get_ci_inf(rc)), tolerance = 1e-12)
    # ⚠ the EFFECT rows only: the two Constant rows are different quantities -- a log column's is the
    # fit's own intercept and carries its test, a ratio column's is the baseline LEVEL and carries
    # none (see test-reg-baseline.R).
    expect_equal(get_pvalue(lc)[fin], get_pvalue(rc)[fin], tolerance = 1e-12)
  }
})


test_that("every reachable estimand pairs with its declared crude shape", {
  # reg_same_estimand() is the gate that withholds `obs` and the gap SE. It must refuse a mismatch
  # WITHOUT refusing anything the package can legitimately build, so the whole grid is swept.
  for (f in names(REG_ESTIMANDS)) for (r in REG_ESTIMANDS[[f]]$rows) {
    # `trials =` is the one caller-supplied fact that moves a block, and it applies to a BINARY
    # outcome only -- the argument boundary refuses it elsewhere.
    tris <- c(list(NA), if (reg_is_grouped_binomial(r$fit, 6)) list(6))
    for (tri in tris) {
      key <- reg_crude_key(r$fit, if (is.na(tri)) NULL else tri)
      if (is.na(key)) next
      sh <- reg_crude_shape(key, r)
      expect_false(is.null(sh),
                   info = paste(f, r$link, r$effect, r$measure, "-- no crude shape resolves"))
      expect_true(reg_same_estimand(sh, reg_scale_of(r, tri), r),
                  info = paste(f, r$link, r$effect, r$measure, "trials:", tri))
    }
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


test_that("an odds column keeps the baseline ODDS, with its level as the cell's aside", {
  skip_if_not_installed("marginaleffects")
  t   <- suppressMessages(tab_reg(bl_data(), "married", c("race", "rincome"),
                                  family = "binomial", empirical = TRUE, stats = FALSE))
  col <- bl_first(t); i <- bl_cst(t)
  o   <- get_or(col)[i]
  expect_true(is.finite(o))
  # the level beside it IS that odds read as a probability, so the two cannot disagree
  expect_equal(get_pct(col)[i], o / (1 + o), tolerance = 1e-9)
  expect_match(format(col)[i], "\\([0-9]+%\\)")
})


test_that("the marginal and at-reference baselines land on the same field as the coefficient one", {
  skip_if_not_installed("marginaleffects")
  d <- bl_data()
  for (eff in c("marginal", "at_reference")) {
    t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                  effect = eff, stats = FALSE))
    col <- bl_first(t); i <- bl_cst(t)
    expect_identical(get_display(col)[i], EST_SCALES[[get_scale(col)]]$const_display)
    expect_true(is.finite(get_num(col)[i]))
    expect_false(grepl("^[+]", trimws(format(col)[i])))     # a baseline never wears a sign
  }
})


test_that("a summed score's RISK ratio has its own scale, not the odds ratio's", {
  d  <- bl_data() |> dplyr::mutate(score = pmin(as.integer(tvhours), 10L))
  or <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                 stats = FALSE))[["Model_OR"]]
  rr <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                 link = "ratio", stats = FALSE))[["Model_RR"]]
  expect_identical(get_scale(or), "score_odds_ratio")
  expect_identical(get_scale(rr), "score_ratio")
  expect_identical(fmt_center_field(or), "or")
  expect_identical(fmt_center_field(rr), "ratio")
  # the reported defect: sharing one row printed every RR with the odds ratio's "1/x" glyph
  f <- format(rr)[!is.na(get_ratio(rr)) & get_ratio(rr) < 1]
  if (length(f)) expect_true(all(grepl(div_glyph, f, fixed = TRUE)))
  expect_false(any(grepl("1/", format(rr), fixed = TRUE)))
  # both baselines state the mean SCORE, the level a battery of items is read in
  expect_true(all(is.finite(c(get_mean(or)[bl_cst(or)], get_mean(rr)[1L]))))
})


test_that("the baseline's own base is the profile's count, or the population, or nothing", {
  d  <- bl_data()
  nn <- function(t) {
    m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
    get_n(m[["n"]])[as.character(t$var) == "Constant"]
  }
  # every predictor categorical -> the reference profile IS a subgroup, and it is counted
  t1 <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                 stats = FALSE))
  fr <- tidyr::drop_na(d[, c("married", "race", "rincome")])
  expect_equal(nn(t1), sum(fr$race == levels(forcats::fct_drop(fr$race))[[1]] &
                             fr$rincome == levels(forcats::fct_drop(fr$rincome))[[1]]))
  # a continuous predictor -> nobody is at the mean, by definition
  t2 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 stats = FALSE))
  expect_true(is.na(nn(t2)))
  # ...and under `marginal` the row IS the population
  skip_if_not_installed("marginaleffects")
  t3 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 effect = "marginal", measure = "difference", stats = FALSE))
  expect_equal(nn(t3), nrow(tidyr::drop_na(d[, c("married", "race", "age")])))
})


test_that("a model check past its convention is MARKED, at the faintest shade", {
  d <- bl_data()
  set.seed(1)
  d$age2 <- as.numeric(d$age) + stats::rnorm(nrow(d), 0, 0.01)   # collinear on purpose
  t <- suppressMessages(tab_reg(d, "married", c("race", "age", "age2"), family = "binomial"))
  m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = TRUE)
  col <- m[[grep("^Model", names(m))[[1]]]]
  disp <- get_display(col)
  expect_true("gof_warn" %in% disp)                    # max VIF >= 10
  slot <- fmt_color_channels(col)$text_slot[disp == "gof_warn"]
  pl   <- tabxplor:::resolve_color_channel_plans(col)$text
  expect_identical(unique(slot), min(pl$under_slots[pl$under_slots > 0L]))
  # an ordinary model-fit number is not marked, and takes no colour at all
  expect_true(any(disp == "gof"))
  expect_true(all(fmt_color_channels(col)$text_slot[disp == "gof"] == 0L))
})


test_that("a logged RISK-scale column's baseline is the log of what its twin shows", {
  # `log_coef` is one row shared by every logged measure, so the column cannot say on its own whether
  # its exponential is an odds or a level -- and the baseline differs by exactly that. The estimand
  # records what it is the log OF (`log_of`), and the baseline is built on that scale, then logged.
  # ⚠ Phase 22g-v: read at the model that ESTIMATES a risk ratio (the modified Poisson), since a raw
  # coefficient is the model's own and has no marginal form.
  skip_if_not_installed("marginaleffects")
  d <- bl_data()
  arg <- list(d, "married", c("race", "rincome"), family = "binomial", link = "ratio",
              stats = FALSE)
  lg <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "log_risk"))))
  rr <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "ratio"))))
  i  <- bl_cst(lg)
  lc <- bl_first(lg); rc <- bl_first(rr)
  expect_identical(get_scale(lc), "log_coef")
  # the twin shows the baseline LEVEL (a risk ratio multiplies the level), so the log column shows
  # its log -- and the interval with it, so `Constant + effect` is coherent on the link scale.
  expect_equal(get_diff(lc)[i], log(get_pct(rc)[i]), tolerance = 1e-10)
  expect_true(is.finite(get_ci_inf(lc)[i]) && is.finite(get_ci_sup(lc)[i]))
  expect_true(get_ci_inf(lc)[i] <= get_diff(lc)[i] && get_diff(lc)[i] <= get_ci_sup(lc)[i])
  # ⚠ WHICH baseline carries a test depends on what it IS: a conditional one is the fit's own
  # intercept and carries the intercept's (the footer says "from 1 for the Constant"), while a
  # marginal one is a population average and carries none.
  expect_false(is.na(get_pvalue(lc)[i]))
  mg <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "ratio", effect = "marginal"))))
  expect_true(is.na(get_pvalue(bl_first(mg))[bl_cst(mg)]))
})


test_that("an odds-scale baseline logs to the log(OR) column's own Constant", {
  # the other arm of the same rule: an odds ratio multiplies ODDS, so its logged twin's baseline is
  # the log-odds, not the log of the probability.
  skip_if_not_installed("nnet")
  d <- bl_data()
  d$p3 <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                  grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                  TRUE ~ "Ind"), levels = c("Ind", "Dem", "Rep"))
  # ⚠ Phase 22g-v: on the model's OWN coefficients, `at_reference` having no logged form
  or <- suppressMessages(tab_reg(d, "p3", "race", family = "multinomial",
                                 measure = "odds_ratio", stats = FALSE))
  lg <- suppressMessages(tab_reg(d, "p3", "race", family = "multinomial",
                                 measure = "log_odds", stats = FALSE))
  i  <- bl_cst(or)
  oc <- or[[names(or)[vapply(or, is_fmt, logical(1))][[1]]]]
  lc <- lg[[names(lg)[vapply(lg, is_fmt, logical(1))][[1]]]]
  expect_identical(get_scale(oc), "odds_ratio")
  expect_identical(get_scale(lc), "log_coef")
  expect_equal(get_diff(lc)[i], log(get_or(oc)[i]), tolerance = 1e-10)
})


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


test_that("modified Poisson == svyglm(quasipoisson) on a constant-weight design, to the last digit", {
  skip_if_not_installed("survey")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio", cleannames = FALSE))
  d$y <- rr_y01(d)
  sv <- survey::svyglm(y ~ race, family = stats::quasipoisson("log"),
                       design = survey::svydesign(ids = ~1, weights = ~1, data = d))
  ci <- stats::confint(sv)
  # skeleton rows: 1 = Constant, 2..4 = the race levels (2 = reference)
  expect_equal(unname(est_of(t$Model_RR)[3:4]),     unname(exp(stats::coef(sv))[2:3]),  tolerance = 1e-10)
  expect_equal(unname(get_ci_inf(t$Model_RR)[3:4]), unname(exp(ci[2:3, 1])),            tolerance = 1e-10)
  expect_equal(unname(get_ci_sup(t$Model_RR)[3:4]), unname(exp(ci[2:3, 2])),            tolerance = 1e-10)
})


test_that("the SE is the HC0 sandwich (not the naive Poisson SE, not the phi-scaled one)", {
  skip_if_not_installed("survey")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "inc3"), family = "binomial", link = "ratio",
                                cleannames = FALSE))
  d$y <- rr_y01(d)
  g  <- stats::glm(y ~ race + inc3, data = d, family = stats::poisson("log"))
  X  <- stats::model.matrix(g); mu <- stats::fitted(g); n <- nrow(X)
  # Zou (2004)'s Huber-White sandwich, hand-computed -- no `sandwich` dependency.
  bread <- solve(t(X) %*% (X * mu)); meat <- t(X) %*% (X * (d$y - mu)^2)
  hc0   <- sqrt(diag(bread %*% meat %*% bread))

  # recover the model's own SE from the stored interval: the CI is symmetric on the LOG scale, so
  # se == (log(sup) - log(inf)) / (2 * crit).
  crit   <- stats::qt(0.975, df = stats::df.residual(g))
  se_tab <- (log(get_ci_sup(t$Model_RR)) - log(get_ci_inf(t$Model_RR))) / (2 * crit)
  # the non-NA rows are the Constant (the intercept) + each non-reference level, in model-matrix order
  se_tab <- se_tab[!is.na(se_tab)]
  # The design-based variance is the sandwich up to survey's own finite-sample factor, measured at
  # sqrt(n/(n-1)) to six digits (a ~1e-6 residual comes from survey's internal df handling and is not
  # worth reproducing by hand -- the EXACT contract is pinned against svyglm itself in the test above).
  # 1e-4 here is still ~100x tighter than the phi gap and ~1000x tighter than the naive gap below.
  expect_equal(unname(se_tab), unname(hc0 * sqrt(n / (n - 1))), tolerance = 1e-4)

  # ...and it is NOT the naive Poisson SE, nor the phi-scaled SE a COUNT poisson would use. Both gaps
  # are large on real data (measured: naive ~49% too wide, phi ~9% off, and phi is off in BOTH
  # directions across coefficients of the same fit, so it is not a calibratable offset). Rule 7: these
  # fail if the "rr" arm ever falls back onto the plain glm / phi-scaling path.
  naive <- summary(g)$coef[, 2]
  phi   <- sum(stats::residuals(g, "pearson")^2) / stats::df.residual(g)
  expect_gt(max(abs(se_tab / naive - 1)), 0.10)
  expect_gt(max(abs(se_tab / (naive * sqrt(phi)) - 1)), 0.01)
})


test_that("the modelled level is the binomial one, and `outcome_level` names it", {
  d <- rr_data()                                        # married: levels "no", "yes"
  t1 <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio"))
  t2 <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio",
                                 outcome_level = c(married = "yes")))
  expect_equal(reg_call(t1)$positive_level, "no")       # the FIRST level, by default
  expect_equal(reg_call(t2)$positive_level, "yes")      # the one that was named
  expect_false(identical(reg_call(t1)$positive_level, reg_call(t2)$positive_level))
})


# ---- (1b) the crude companion --------------------------------------------------------------------

test_that("Obs_RR is the crude RISK ratio with a Katz interval (never the crude ODDS ratio)", {
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio", empirical = TRUE,
                                cleannames = FALSE))
  lv <- levels(d$race)
  p  <- vapply(lv, function(l) mean(rr_y01(d)[d$race == l]), numeric(1))
  nn <- vapply(lv, function(l) sum(d$race == l), numeric(1))
  expect_equal(unname(est_of(t$Obs_RR)[3:4]), unname(p[-1] / p[1]), tolerance = 1e-10)
  ci <- ci_katz_rr(p[-1], nn[-1], p[1], nn[1], conf_level = 0.95, want_p = TRUE)
  expect_equal(unname(get_ci_inf(t$Obs_RR)[3:4]), unname(ci$inf), tolerance = 1e-10)
  # the crude ODDS ratio would be a DIFFERENT number -- guarding the emp_ratio trap
  odds <- (p[-1] / (1 - p[-1])) / (p[1] / (1 - p[1]))
  expect_gt(max(abs(est_of(t$Obs_RR)[3:4] / odds - 1)), 0.01)
})


test_that("with ONE predictor the model RR == the crude Obs_RR exactly", {
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio", empirical = TRUE,
                                cleannames = FALSE))
  # rows 2..4 are the race levels; row 1 is the Constant (a model intercept the crude column has no
  # counterpart for, hence NA there).
  expect_equal(est_of(t$Model_RR)[2:4], est_of(t$Obs_RR)[2:4], tolerance = 1e-9)
})


# ---- (1c) the footer, the guards, and the un-exponentiated scale ----------------------------------

test_that("the footer reports n + Wald-vs-null only (no AIC/BIC/McFadden/dispersion)", {
  d  <- rr_data()
  t  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio"))
  tt <- get_test(t)
  expect_true(all(c("n", "wald_null") %in% tt$test))
  # a quasi-likelihood has no AIC/BIC/McFadden; binary Pearson dispersion (`phi`, z15) is just
  # mean(1-mu), so it is not reported either.
  expect_false(any(c("aic", "bic", "mcfadden_r2", "lr_null", "phi") %in% tt$test))
  # the DEFAULT checks join every default set (22b-xviii: `footer_default`, a declared fact, not
  # "the free ones"); none brings a quasi-likelihood statistic with it.
  # 22b-ix: the crossed-pair interaction test joins every glm default set (it produces no row
  # unless `predictors` actually declares an `a:b` pair).
  # ⚠ 22b-xviii: "global" is NOT a default -- one row per multi-level predictor, a drop1() refit each.
  expect_equal(reg_footer_stats("rr", weighted = FALSE, grouped = FALSE, stats = NULL),
               c("n", "wald_null", "interaction",
                 tabxplor:::reg_check_expand(tabxplor:::reg_checks_default("rr"))))
  # and `stats = "all"` adds exactly the opt-in ones on top -- nothing else
  expect_setequal(setdiff(reg_footer_stats("rr", FALSE, FALSE, "all"),
                          reg_footer_stats("rr", FALSE, FALSE, NULL)),
                  c("global",
                    tabxplor:::reg_check_expand(setdiff(tabxplor:::reg_checks_for("rr"),
                                                        tabxplor:::reg_checks_default("rr")))))
})


test_that("method='profile' is refused for a modified Poisson and degrades to the robust Wald", {
  d <- rr_data()
  # it SAYS so (a profile likelihood on a deliberately misspecified quasi-likelihood is meaningless)...
  expect_message(tab_reg(d, "married", "race", family = "binomial", link = "ratio", ci_method = "profile"),
                 "quasi-likelihood")
  # ...and the interval it returns is exactly the robust Wald one.
  tp <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio", ci_method = "profile"))
  tw <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio", ci_method = "wald"))
  expect_equal(get_ci_inf(tp$Model_RR), get_ci_inf(tw$Model_RR), tolerance = 1e-12)
  expect_equal(get_ci_sup(tp$Model_RR), get_ci_sup(tw$Model_RR), tolerance = 1e-12)
})


test_that("measure = log colours the log-RR coefficient on the log scale (is_logcoef)", {
  d <- rr_data()
  # `measure = "log"` logs the family's DEFAULT estimand (a binomial's odds ratio); `log_risk` pins
  # the modified-Poisson fit, which is what `family = "binomial", link = "ratio", exponentiate = FALSE` used to mean.
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio", measure = "log_risk",
                                empirical = TRUE, cleannames = FALSE))
  cf <- t[[grep("^Model", names(t), value = TRUE)[1]]]
  expect_equal(get_model_family(cf), "rr")
  # 19l: reg_fam_logscale() is gone -- "this coefficient lives on a log scale" is the column's own
  # STORED scale, which is what the colour engine and the legend have read since 19b.
  expect_equal(get_scale(cf), "log_coef")
  # the crude twin is the LOGGED risk ratio, matching the model's link scale
  expect_true("Obs_log(RR)" %in% names(t))
  expect_equal(get_diff(t[["Obs_log(RR)"]])[3:4],
               log(est_of(suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "ratio",
                                                   empirical = TRUE, cleannames = FALSE))$Obs_RR))[3:4],
               tolerance = 1e-10)
})


# ---- (1d) NON-REGRESSION: a genuine count model is untouched --------------------------------------

test_that("a real COUNT poisson keeps its IRR, its dispersion row and its over-dispersion warning", {
  d <- rr_data()
  expect_warning(t <- tab_reg(d, "tvhours", "race", family = "poisson"), "Over-dispersion")
  expect_true("Model_IRR" %in% names(t))
  expect_false(any(grepl("_RR", names(t))))
  expect_true("dispersion" %in% get_test(t)$test)
  expect_equal(get_model_family(t$Model_IRR), "poisson")
})


# ---- (2) measure = "ratio": the MARGINAL risk ratio ----------------------------------------------

test_that("a marginal ratio == marginaleffects' lnratioavg contrast, exponentiated", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "ratio",
                                cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  d$y <- rr_y01(d)
  lg <- stats::glm(y ~ race, data = d, family = stats::binomial())
  r  <- as.data.frame(marginaleffects::avg_comparisons(
    lg, variables = "race", comparison = "lnratioavg", newdata = d))
  expect_equal(unname(est_of(t[[nm]])[3:4]), unname(exp(r$estimate)),  tolerance = 1e-10)
  # Phase 20d: the BOUND is looser than the estimate on purpose. Ours comes from an analytic jacobian,
  # marginaleffects' from a finite-difference one, and its own step-size choice (fdforward vs fdcenter)
  # moves this bound by ~4e-9 -- more than we differ from it. The oracle is the approximation here.
  expect_equal(unname(get_ci_inf(t[[nm]])[3:4]), unname(exp(r$conf.low)), tolerance = 1e-7)
})


test_that("the marginal-ratio cell is coherent: adjusted%(ref) * RR == adjusted%(level)", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "inc3"), family = "binomial",
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  nm  <- grep("^Model", names(t), value = TRUE)[1]
  pct <- get_pct(t[[nm]]); or <- est_of(t[[nm]])
  # this is the identity the "prob_ratio" shape exists for -- the multiplicative twin of the AME's
  # adjusted%(ref) + AME == adjusted%(level).
  for (v in unique(as.character(t$var))) {
    i <- which(as.character(t$var) == v & !is.na(pct))
    if (length(i) < 2) next
    expect_equal(pct[i], pct[i[1]] * or[i], tolerance = 1e-10)
  }
})


test_that("a marginal ratio: label parsing survives a level containing ' - ' and ')'", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # the Phase 14r regression class, re-armed for the new "ln(mean(L) / mean(R))" prefix/suffix
  d$tricky <- factor(ifelse(d$race == "White", "a (x) - b", "c - d (y)"))
  t <- suppressMessages(tab_reg(d, "married", "tricky", family = "binomial",
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_false(any(is.na(est_of(t[[nm]])[2:3])))   # both levels keyed to the skeleton
})


test_that("a marginal ratio: numeric predictors work and the crude twin is the Katz Obs_RR", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "tvhours"), family = "binomial",
                                effect = "marginal", measure = "ratio", empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  i  <- which(as.character(t$var) == "tvhours")
  expect_false(is.na(est_of(t[[nm]])[i]))
  expect_true("Obs_RR" %in% names(t))
  lv <- levels(d$race)
  p  <- vapply(lv, function(l) mean(rr_y01(d)[d$race == l]), numeric(1))
  expect_equal(unname(est_of(t$Obs_RR)[3:4]), unname(p[-1] / p[1]), tolerance = 1e-10)
})


# Phase 19e (capability gap closed): a marginal RATIO used to be refused for gaussian / poisson
# outcomes ("needs a probability-scale outcome"). A ratio of adjusted MEANS is a sound estimand --
# tab() has given one for years -- so it is offered now, on the mean_ratio scale.
test_that("a marginal ratio is available for gaussian / poisson outcomes", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  tg <- suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian",
                                 effect = "marginal", measure = "ratio", cleannames = FALSE))
  mg <- tg[[grep("^Model", names(tg), value = TRUE)[1]]]
  expect_equal(get_scale(mg), "mean_ratio")
  expect_true(all(get_ratio(mg)[!is.na(get_ratio(mg))] > 0))
})


test_that("a marginal ratio colours as a RATIO (its stored scale, not the contrast)", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # a marginal effect is not a coefficient, but the colour ladder must still pick the MULTIPLICATIVE
  # measure -- and for a RISK ratio that is `ratio` on `pct_ratio`, not the odds ratio's own scale.
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                effect = "marginal", measure = "ratio", cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(get_color(t[[nm]])[1], "ratio")
  expect_equal(get_scale(t[[nm]]), "pct_ratio")
})


test_that("a marginal ratio: the legend names RR, not OR, on both the model and the crude column", {
  skip_if_not_installed("marginaleffects")
  d  <- rr_data()
  t  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "ratio",
                                 empirical = TRUE, cleannames = FALSE))
  md <- reg_call(t)
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(legend_reg_eff_word(t[[nm]], md), "RR")
  expect_equal(legend_reg_eff_word(t$Obs_RR, md), "RR")   # crude twin, same estimand
  # the base-count column (drawn at display) has no effect word
  m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  expect_true(is.na(legend_reg_eff_word(m[["n"]], md)))
})


test_that("a marginal ratio: with ONE predictor the marginal RR == the crude RR", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  # a saturated single-predictor model reproduces the observed rates, so g-computation returns the
  # crude ratio exactly -- the same identity the coefficient path's OR tests use.
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "ratio",
                                empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  expect_equal(est_of(t[[nm]])[3:4], est_of(t$Obs_RR)[3:4], tolerance = 1e-9)
})


test_that("a marginal risk DIFFERENCE is untouched by the marginal ratio beside it", {
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal", measure = "difference",
                                empirical = TRUE, cleannames = FALSE))
  nm <- grep("^Model", names(t), value = TRUE)[1]
  # still an additive risk DIFFERENCE with its "{diff} ({pct})" cell and diff colour
  expect_equal(get_scale(t[[nm]]), "points")
  expect_equal(get_color(t[[nm]])[1], "difference")
  expect_true("Obs_RD" %in% names(t))
  expect_false("Obs_RR" %in% names(t))
})


# --- A LOGGED CRUDE COLUMN RUNS ITS OWN ARITHMETIC -------------------------------------------------

test_that("a logged crude column is the log of ITS measure, never of the family's default", {
  # `measure = "log_risk"` declares the Katz log-RR engine on its own shape. The arm used to be
  # re-derived from the crude family's COEFFICIENT shape, so wherever the two differ -- a summed
  # score (whose block's coefficient is an odds ratio) or a borrowed shape (a binary marginal ratio's
  # crude twin lives in another block) -- `Obs_log(RR)` printed Woolf's log(OR).
  tea   <- as.data.frame(facto_tea)
  items <- c("home", "tearoom", "work", "friends", "resto", "pub")
  tea$tea_where <- rowSums(vapply(items, function(v) as.integer(tea[[v]] == v),
                                  integer(nrow(tea))))
  tea$sex <- factor(tea$sex)
  # ⚠ each logged measure is asked of the model whose own coefficient it is (Phase 22g-v): the
  # log RR of the modified Poisson, the log OR of the logistic. What is under test is the CRUDE
  # twin's arithmetic, which is per-measure either way.
  obs <- function(m, lk = "auto") {
    t <- suppressMessages(tab_reg(tea, "tea_where", "sex", family = "binomial", trials = 6,
                                  link = lk, measure = m, empirical = "column", stats = FALSE))
    t[[grep("^Obs_", names(t))[[1]]]]
  }
  a  <- sum(tea$tea_where[tea$sex == "M"]);      b  <- sum(6 - tea$tea_where[tea$sex == "M"])
  cc <- sum(tea$tea_where[tea$sex == "F"]);      dd <- sum(6 - tea$tea_where[tea$sex == "F"])
  p1 <- a / (a + b); p0 <- cc / (cc + dd)
  i  <- 3L                                         # the non-reference level's row
  lrr <- obs("log_risk", lk = "ratio"); lor <- obs("log_odds")
  kz  <- ci_katz_rr(p1, a + b, p0, cc + dd)
  wf  <- ci_or(a, b, cc, dd)
  expect_identical(get_ci_method(lrr), "katz")
  expect_equal(get_diff(lrr)[i],    log(p1 / p0),    tolerance = 1e-10)
  expect_equal(get_ci_inf(lrr)[i],  log(kz$inf),     tolerance = 1e-10)
  expect_equal(get_ci_sup(lrr)[i],  log(kz$sup),     tolerance = 1e-10)
  # ...and it is a DIFFERENT number from the odds-ratio twin, which is what went unnoticed
  expect_identical(get_ci_method(lor), "woolf")
  expect_equal(get_diff(lor)[i], log((a * dd) / (b * cc)), tolerance = 1e-10)
  expect_gt(abs(get_diff(lrr)[i] - get_diff(lor)[i]), 0.05)
})


test_that("a binary MARGINAL risk ratio takes the borrowed block's Katz arm", {
  # `crude_fam = "rr"` while `crude_key` is "binomial": the shape is borrowed across blocks, so the
  # family in hand is the wrong place to look up which arithmetic to run.
  # ⚠ Phase 22g-v: this used to be asked as `measure = "log_risk"`, i.e. the LOG of a marginal risk
  # ratio. A raw coefficient is the model's own and has no marginal form, so the borrowed block is
  # exercised through the ratio itself -- the same lookup, one exponential later.
  skip_if_not_installed("marginaleffects")
  d <- rr_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", effect = "marginal",
                                measure = "ratio", empirical = "column", stats = FALSE))
  o <- t[[grep("^Obs_", names(t))[[1]]]]
  expect_identical(get_scale(o), "pct_ratio")
  expect_identical(get_ci_method(o), "katz")
  expect_error(tab_reg(d, "married", "race", family = "binomial", effect = "marginal",
                       measure = "log_risk"), "no \"marginal\" form")
  # ⚠ the modelled level is the outcome's FIRST, which is `tab_reg()`'s documented default.
  tb <- table(d$race, d$married)
  p  <- tb[, levels(d$married)[[1]]] / rowSums(tb)
  i  <- which(as.character(t$levels) == names(p)[[2]])
  expect_equal(get_ratio(o)[i], p[[2]] / p[[1]], tolerance = 1e-10)
})


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


test_that("`effect = \"auto\"` takes the coefficient when the measure IS the model's, else the predictions", {
  # measure == link -> read it off the coefficients
  expect_identical(reg_estimand("binomial", measure = "odds_ratio")$effect, "conditional")
  expect_identical(reg_estimand("binomial", link = "ratio", measure = "ratio")$effect, "conditional")
  # measure != link -> from the model's predictions, sample-averaged
  expect_identical(reg_estimand("binomial", measure = "ratio")$effect,      "marginal")
  expect_identical(reg_estimand("binomial", measure = "difference")$effect, "marginal")
  expect_identical(reg_estimand("multinomial", measure = "difference")$effect, "marginal")
})


test_that("`link` takes `measure`'s own words, plus the glm spellings, silently", {
  expect_identical(reg_link_key("ratio"),      "ratio")
  expect_identical(reg_link_key("log"),        "ratio")        # the LOG LINK, not "un-exponentiated"
  expect_identical(reg_link_key("logit"),      "odds_ratio")
  expect_identical(reg_link_key("identity"),   "difference")
  expect_identical(reg_link_key("RR"),         "ratio")        # the acronyms work here too
  expect_identical(reg_link_key("IRR"),        "ratio")        # ...every one of them, since 22c-v
  expect_identical(reg_link_key("RoM"),        "ratio")
  # the internal fit keys stay typeable into `link` (reg_formulas() now prints the measure itself)
  expect_identical(reg_link_key("rr"),         "ratio")
  expect_identical(reg_link_key("rd"),         "difference")
  expect_identical(reg_link_key("mr"),         "ratio")
  expect_identical(reg_link_key(NULL),         "auto")
  expect_null(reg_link_key("nonsense"))
  expect_setequal(REG_LINKS_VALUES, c("auto", "odds_ratio", "ratio", "difference"))
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


# --- 4. the two routes to a ratio, and what only the cascade opens --------------------------------

test_that("`link` names the model and `measure` the report: two different risk ratios", {
  skip_if_not_installed("survey")
  d <- est_data()
  cond <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                   link = "ratio", empirical = TRUE, cleannames = FALSE))
  marg <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                   measure = "ratio", empirical = TRUE, cleannames = FALSE))
  expect_true("Model_RR"  %in% names(cond))     # the modified Poisson's own coefficient
  expect_true("Model_mRR" %in% names(marg))     # g-computed from the logistic fit
  expect_identical(reg_formulas(cond)$fit, 'svyglm(quasipoisson("log"))')
  expect_identical(reg_formulas(marg)$fit, 'glm(binomial("logit"))')
  # `link` says which word rebuilds each of them
  expect_identical(reg_formulas(cond)$link, "ratio")
  expect_identical(reg_formulas(marg)$link, "odds_ratio")
  # both are risk ratios, so both sit on the same scale and pair with the same crude column
  expect_identical(get_scale(modcol(cond)), "pct_ratio")
  expect_identical(get_scale(modcol(marg)), "pct_ratio")
  expect_true(all(c("Obs_RR") %in% names(cond)) && "Obs_RR" %in% names(marg))
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


# === SECTION: the Constant row ====================================================================

bl_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}


bl_first <- function(t) t[[grep("^Model", names(t))[[1]]]]


bl_cst   <- function(t) which(as.character(t$var) == "Constant")


test_that("the baseline row renders a level, never an effect", {
  d <- bl_data()
  # family x measure -> what the Constant is stamped with, and what it prints
  cases <- list(
    list(a = list(family = "binomial"),                        tok = "or",   pat = "^1/[0-9]"),
    list(a = list(family = "binomial", measure = "ratio"),     tok = "pct",  pat = "^[0-9]+%$"),
    list(a = list(family = "binomial", measure = "difference"),tok = "pct",  pat = "^[0-9.]+%$"),
    list(a = list(family = "binomial", measure = "log"),       tok = "coef", pat = "^-?[0-9.]+$"),
    list(a = list(family = "gaussian"),                        tok = "mean", pat = "^[0-9.]+$"),
    list(a = list(family = "gaussian", measure = "ratio"),     tok = "mean", pat = "^[0-9.]+$"),
    list(a = list(family = "poisson"),                         tok = "mean", pat = "^[0-9.]+$"),
    list(a = list(family = "poisson",  measure = "log"),       tok = "coef", pat = "^[0-9.]+$")
  )
  for (cs in cases) {
    y <- if (cs$a$family == "gaussian") "age" else if (cs$a$family == "poisson") "tvhours" else "married"
    # ⚠ `empirical = FALSE`: what is under test is the token the BASELINE row is stamped with, and
    # the default crude companion puts a layout (`est_base`) over every cell of the column.
    t <- suppressWarnings(do.call(
      tab_reg, c(list(d, y, c("race", "rincome")), cs$a,
                 list(empirical = FALSE, stats = FALSE))))
    col <- bl_first(t); i <- bl_cst(t)
    expect_identical(get_display(col)[i], cs$tok, info = cs$a$family)
    expect_match(format(col)[i] |> trimws(), cs$pat)
    # ...and a baseline shown as a LEVEL carries no test: there is no null for a percentage or a mean
    expect_identical(is.na(get_pvalue(col)[i]),
                     identical(EST_SCALES[[get_scale(col)]]$const_display,
                               EST_SCALES[[get_scale(col)]]$base_display))
  }
})


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
