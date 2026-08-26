# PURPOSE: the regression argument boundary: one grammar per axis, and what it refuses.
# ROLE: the shipped CONTRACT for R/reg-resolve.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the regression argument boundary ====================================================

skip_on_cran()


rr_data <- function() {
  g <- fx_reg_df()
  g <- g[seq(1L, nrow(g), by = 6L), , drop = FALSE]
  g$married <- factor(ifelse(g$marital == "Married", "Married", "Not married"))
  g$party3  <- forcats::fct_lump_n(g$partyid, 2)
  g$score   <- pmin(g$tvhours, 6L)
  as.data.frame(g)
}


# every cli_inform this call emits, as plain strings (the notes are what several tests here are about)
capture_msg <- function(expr) {
  out <- character()
  withCallingHandlers(force(expr),
                      message = function(m) { out <<- c(out, conditionMessage(m))
                                              invokeRestart("muffleMessage") })
  out
}


# === S1 reg_validate_args(): the four arguments the reg boundary never checked ====================

test_that("`conf_level` is validated, with the 95-vs-0.95 hint", {
  d <- rr_data()
  # it reached the interval engine as a probability: `qnorm(1 - (1 - 95)/2)` -> NaN, a warning, and
  # a table full of NaN bounds.
  expect_error(tab_reg(d, "married", "race", family = "binomial", conf_level = 95),
               "0\\.95")
  expect_error(tab_reg(d, "married", "race", family = "binomial", conf_level = -1),
               "probability")
  expect_error(tab_reg(d, "married", "race", family = "binomial", conf_level = c(0.9, 0.95)),
               "single probability")
})


test_that("`stats` names are validated instead of silently filtered", {
  d <- rr_data()
  # reg_footer_stats() did `stats[stats %in% reg_stat_keys()]`, so a typo produced a MISSING footer
  # row and no message. reg_validate_stat_keys() has carried `arg = "stats"` since 19g, uncalled.
  expect_error(tab_reg(d, "married", "race", family = "binomial", stats = c("n", "typo")),
               "Unknown")
  # the declared special values still pass through untouched
  for (s in list(NULL, TRUE, FALSE, "all", "none", c("n", "aic")))
    expect_no_error(suppressMessages(
      tab_reg(d, "married", "race", family = "binomial", stats = s)))
})


test_that("`color_signif` is validated on the reg path too", {
  d <- rr_data()
  # it went straight to fmt(), which CASTS without validating -- so the unknown policy was stored on
  # every column and merely painted as "ignore".
  expect_error(tab_reg(d, "married", "race", family = "binomial", color_signif = "grey"),
               "Unknown .*color_signif")
  for (s in COLOR_SIGNIF_VALUES)
    expect_no_error(suppressMessages(
      tab_reg(d, "married", "race", family = "binomial", color_signif = s)))
})


# Phase 20c: `compare` + `baseline` are two `stats =` keys, so the two things this used to check are
# unrepresentable -- a baseline is a single string by grammar, and naming one IS asking for the
# comparison. What is checkable instead is the grammar itself.
test_that("the model-comparison keys are refused when they contradict each other", {
  d <- rr_data(); M <- list(m1 = "race", m2 = c("race", "age"))
  expect_error(tab_reg(d, "married", M, family = "binomial",
                       stats = c("compare_baseline", "compare_sequential")),
               "more than one model comparison")
  expect_error(tab_reg(d, "married", M, family = "binomial",
                       stats = c(compare_sequential = "m1")),
               "sequential comparison has none")
  # the retired spellings abort as unknown arguments (Phase 20j), not silently ignored
  expect_error(tab_reg(d, "married", M, family = "binomial", compare = "baseline"),
               "[Uu]nknown argument")
  expect_error(tab_reg(d, "married", M, family = "binomial", baseline = "m1"),
               "[Uu]nknown argument")
})


# === S2: the split_var refusals now precede the colour/family informs (H23) =======================

test_that("a `split_var` that is also a predictor aborts before anything is announced", {
  d <- rr_data()
  # the same abort fired ~500 lines later, so it arrived after up to eight informs about families,
  # colours and forcings the call was never going to produce.
  expect_silent(expect_error(
    tab_reg(d, "married", c("race", "age"), family = "binomial",
            tab_vars = "race", color = "adjustment"),
    "cannot also be the outcome or a predictor"))
})


test_that("`outcome_level` names the level, on a factor and on a 0/1 numeric outcome", {
  d <- rr_data()
  a <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                outcome_level = c(married = "Not married")))
  expect_identical(reg_call(a)$positive_level[[1]], "Not married")

  # Phase 20c: on a 0/1 numeric outcome the old logical was a SILENT NO-OP -- that branch returns
  # before ever reaching the level reversal. Naming the level works there, in both spellings.
  d$bin <- as.numeric(d$married == "Married")
  b <- suppressMessages(tab_reg(d, "bin", "race", family = "binomial"))
  expect_identical(reg_call(b)$positive_level[[1]], "bin")            # the 1s, by default
  z <- suppressMessages(tab_reg(d, "bin", "race", family = "binomial",
                                outcome_level = c(bin = "0")))
  expect_identical(reg_call(z)$positive_level[[1]], "Not bin")        # the 0s, as asked
})


test_that("`outcome_level` is refused where the family has no level to single out", {
  skip_if_not_installed("MASS")
  d <- rr_data()
  d$inc3 <- factor(forcats::fct_lump_n(d$rincome, 2), ordered = TRUE)
  expect_error(suppressMessages(tab_reg(d, "inc3", "race", family = "ordinal",
                                        outcome_level = c(inc3 = "Other"))),
               "keep the order")
  expect_error(suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                        outcome_level = c(married = "nope"))),
               "not a level")
  # `reference` was renamed to `ref` (Phase 20c) and its table deleted (Phase 20j): it aborts as an
  # unknown argument now. (The outcome's level is `outcome_level`; `ref` names what you compare against.)
  expect_error(suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                        reference = c(married = "Married"))),
               "[Uu]nknown argument")
})


# === defect 8: a formula `dependent` is not a vector of three dependents ==========================

test_that("a formula `dependent` beside `predictors` gives the teachable message, not a stopifnot", {
  d <- rr_data()
  # `length(y ~ x)` is 3 (`~`, lhs, rhs), so this used to enter the multi-dependent recursion and
  # die on the internal `stopifnot(is.character(dependent))`.
  expect_error(tab_reg(d, married ~ race, list(m1 = "race")),
               "formula.*or.*predictors|predictors.*not both")
  expect_error(tab_reg(d, married ~ race, c("race", "age")),
               "formula.*or.*predictors|predictors.*not both")
})


test_that("reg_word() composes the header: marker o log-wrap o base acronym", {
  expect_identical(reg_word(reg_estimand("binomial", measure = "odds_ratio", effect = "conditional")), "OR")
  expect_identical(reg_word(reg_estimand("binomial", measure = "difference", effect = "marginal")),    "mRD")
  expect_identical(reg_word(reg_estimand("binomial", measure = "ratio", effect = "at_reference")),     "refRR")
  expect_identical(reg_word(reg_estimand("binomial", measure = "log", effect = "conditional")),        "log(OR)")
  expect_identical(reg_word(reg_estimand("ordinal", measure = "odds_ratio", effect = "conditional")), "cumOR")
  # the expansion follows the same two rules, in the order each is spoken
  expect_identical(reg_word_long(reg_estimand("binomial", measure = "ratio", effect = "marginal")),
                   "marginal risk ratio")
  expect_identical(reg_word_long(reg_estimand("binomial", measure = "difference", effect = "at_reference")),
                   "risk difference at the reference profile")
  # and the base acronym is recoverable from any composed word
  expect_identical(reg_word_base("log(cumOR)"), "cumOR")
  expect_identical(reg_word_base("mRR"),        "RR")
  expect_identical(reg_word_base("refRD"),      "RD")
})


test_that("reg_color_for() fills only the auto slots, and is idempotent", {
  e  <- reg_estimand("binomial", measure = "auto", effect = "conditional")
  # the bare-TRUE sentinel
  one <- reg_color_for(reg_normalize_color(TRUE), e)
  expect_false(any(is.na(one)))
  expect_identical(reg_color_for(one, e), one)                       # idempotent: no NA left to fill
  # an explicit measure keeps its own slot; only the auto one follows the column
  two <- reg_color_for(reg_normalize_color(c(TRUE, "adjustment")), e)
  expect_identical(two[[2]], "adjustment")
  expect_identical(two[[1]], one[[1]])
  # nothing auto -> unchanged
  expect_identical(reg_color_for("adjustment", e), "adjustment")
})



# --- Phase 20c (KEY 4): one word per question ------------------------------------------------------
# `tab_reg()` is unreleased, so each of these was a RENAME. Phase 20j deletes the retired-name table:
# an old spelling now lands in `...` and aborts as an unknown argument through the SHARED
# tab_check_dots() -- one dots-validator for both producers. The safety property is what this pins:
# a removed name still ABORTS (never a silent no-op), it just no longer names its replacement.
test_that("a retired spelling aborts as an unknown argument (no silent no-op)", {
  d <- rr_data()
  retired <- list(
    dependent                 = list(dependent = "married"),
    split_var                 = list(split_var = "race"),
    reference                 = list(reference = c(race = "White")),
    method                    = list(method = "profile"),
    compare                   = list(compare = "baseline"),
    baseline                  = list(baseline = "m1"),
    inverse_two_level_factors = list(inverse_two_level_factors = FALSE)
  )
  for (nm in names(retired)) {
    args <- c(list(d, "married", "race", family = "binomial"), retired[[nm]])
    expect_error(do.call(tab_reg, args), "[Uu]nknown argument", info = nm)
  }
})


test_that("the two producers now ask the shared questions with the shared word", {
  d <- rr_data()
  # `tab_vars`, `ref` and `ci_method` are the SAME argument on both producers -- declared, so
  # tx_check_tab_args() polices tab_reg()'s signature against TAB_ARGS like a crosstab's.
  # ⚠ REACHABLE, not necessarily a formal: 22g-ii moved `ci_method` onto `...` (its TAB_ARGS row
  # says so, `dots = "tab_reg"`), which tx_check_tab_args() accepts and tab_dots_expand() refills.
  for (k in c("tab_vars", "ref", "ci_method", "na", "color", "color_signif", "display",
              "conf_level", "stars", "wt", "n", "cleannames", "subtext")) {
    fm <- names(formals(tab_reg))
    expect_true(k %in% fm ||
                  ("..." %in% fm &&
                     "tab_reg" %in% (tabxplor:::TAB_ARGS[[k]][["dots"]] %||% character())), info = k)
    expect_true("tab_reg" %in% tabxplor:::TAB_ARGS[[k]][["producers"]], info = k)
    expect_true("tab"     %in% tabxplor:::TAB_ARGS[[k]][["producers"]], info = k)
  }
  # `ci_method`'s fifth slot IS the regression's, so a bare "profile" means it
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                ci_method = c(model = "profile")))
  expect_identical(reg_call(t)$fit_spec$method, "profile")
  # and an unknown slot is refused by the ONE validator both producers share
  expect_error(tab_reg(d, "married", "race", family = "binomial", ci_method = c(nope = "wald")),
               "Unknown")
})



# === SECTION: multiplier, shape and ref: one grammar ==============================================

anc_data <- function() {
  d <- fx_reg_df()
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  tibble::as_tibble(d[!is.na(d$age) & !is.na(d$tvhours), , drop = FALSE])
}


cst <- function(t, col) {
  x <- t[[col]]
  get_num(x[as.character(t$var) == "Constant"])
}


# ---- the shared grammar -------------------------------------------------------------------------

test_that("one grammar: a bare scalar, `default =`, and per-variable overrides", {
  d <- anc_data()

  # multiplier: an unnamed value is the fallback -- the 1.x form discarded it as soon as a name appeared
  m <- tab_reg(d, "married", c("race", "age", "tvhours"), family = "binomial", stats = FALSE,
               multiplier = c("2sd", age = 10))
  k <- reg_call(m)$multiplier
  expect_equal(unname(k[["age"]]), 10)
  expect_equal(unname(k[["tvhours"]]),
               2 * tabxplor:::wtd_sd(d$tvhours[!is.na(d$race)]), tolerance = 1e-8)
  expect_equal(k, reg_call(tab_reg(d, "married", c("race", "age", "tvhours"), family = "binomial",
                                   stats = FALSE,
                                   multiplier = c(default = "2sd", age = 10)))$multiplier)

  # shape: a bare scalar cuts EVERY continuous predictor -- inexpressible before this phase
  s <- suppressWarnings(tab_reg(d, "married", c("race", "age", "tvhours"), family = "binomial",
                                stats = FALSE, shape = "quartiles"))
  expect_true(all(c("age", "tvhours") %in% as.character(s$var)))
  expect_equal(sum(as.character(s$var) == "age"), 4L)      # four quantile groups, one row each

  # ref: the value names the kind it applies to, so both defaults fit in one unnamed pair
  r  <- tab_reg(d, "married", c("race", "relig", "age"), family = "binomial", stats = FALSE,
                ref = c("median", "last", race = "Black"))
  fr <- d[!is.na(d$race) & !is.na(d$relig) & !is.na(d$age), ]
  expect_equal(unname(reg_call(r)$fit_spec$prep$anchors[["age"]]),
               tabxplor:::shape_wquantile(fr$age, 0.5), tolerance = 1e-8)
  expect_identical(as.character(r$levels)[as.character(r$var) == "race"][[1]], "Black")
  lv <- as.character(r$levels)[as.character(r$var) == "relig"]
  expect_identical(lv[[1]], utils::tail(levels(forcats::fct_drop(d$relig)), 1))  # `last`, every OTHER factor
})


test_that("the grammar's refusals name the eligible set and the two vocabularies", {
  d <- anc_data()
  f <- function(...) tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE, ...)
  expect_error(f(ref = c(nope = "mean")), "predictor")
  expect_error(f(ref = "banana"), "default")                 # matches neither vocabulary
  expect_error(f(ref = c("mean", "median")), "same kind")    # two defaults for one kind
  expect_error(f(multiplier = c("sd", "2sd")), "same kind")
  expect_error(f(ref = c(age = "quartile")), "must be a number")
  expect_error(f(shape = c(race = "quadratic")), "continuous")
})


# ---- the anchor is a reparametrization -----------------------------------------------------------

test_that("only the intercept moves: every estimate is invariant under the anchor", {
  d  <- anc_data()
  t0 <- tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE,
                multiplier = 1, ref = c(age = 0))
  for (a in list(NULL, c(age = "median"), c(age = 40))) {
    t1 <- do.call(tab_reg, c(list(d, "married", c("race", "age"), family = "binomial",
                                  stats = FALSE, multiplier = 1), if (!is.null(a)) list(ref = a)))
    keep <- as.character(t1$var) != "Constant"
    for (g in list(get_or, get_ci_inf, get_ci_sup, get_pvalue))
      expect_equal(g(t1[["Model_OR"]])[keep], g(t0[["Model_OR"]])[keep], tolerance = 1e-9)
  }
  # and the intercept really does move, in the direction the anchor says
  expect_false(isTRUE(all.equal(cst(t0, "Model_OR"),
                                cst(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                            stats = FALSE, multiplier = 1), "Model_OR"))))
})
