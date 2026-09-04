# PURPOSE: the design boundary: what a survey design changes, and what it refuses.
# ROLE: the shipped CONTRACT for R/survey-design.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the design boundary, end to end =====================================================

skip_if_not_installed("survey")



# A design whose weights correlate with the outcome (so weighted != unweighted by a wide margin) and
# whose PSUs carry a real cluster effect.
svy_fixture <- function(n = 1500, seed = 4) {
  set.seed(seed)
  b <- data.frame(psu = factor(rep(seq_len(n / 50), each = 50)))
  b$hidden <- stats::rnorm(n)
  b$w      <- exp(0.9 * b$hidden); b$w <- b$w / mean(b$w)
  b$x <- factor(sample(c("low", "mid", "high"), n, TRUE), levels = c("low", "mid", "high"))
  b$z <- factor(sample(c("u", "v"), n, TRUE))
  b$y <- factor(ifelse(stats::rbinom(
    n, 1, stats::plogis(-.3 + .8 * (b$x == "mid") + 1.4 * (b$x == "high") +
                          .5 * (b$z == "v") + 1.1 * b$hidden)) == 1, "yes", "no"),
    levels = c("no", "yes"))
  b$num <- round(stats::rnorm(n, 50, 12) + 8 * b$hidden)
  b
}



mid_cell <- function(tab, pattern, getter) {
  col <- tab[[grep(pattern, names(tab))[1]]]
  unname(getter(col)[which(as.character(tab$levels) == "mid")])
}



# ---- D1 / D2: the crude columns and the AME are design-WEIGHTED -----------------------------------

test_that("D1 the crude Obs_* columns under a design are weighted, not unweighted", {
  b   <- svy_fixture()
  des <- survey::svydesign(~psu, weights = ~w, data = b)
  t_des <- suppressMessages(
    tab_reg(des, outcome = "y", predictors = c("x", "z"), family = "binomial", empirical = TRUE))
  t_wt  <- tab_reg(b, outcome = "y", predictors = c("x", "z"), family = "binomial",
                   empirical = TRUE, wt = "w")
  t_un  <- tab_reg(b, outcome = "y", predictors = c("x", "z"), family = "binomial",
                   empirical = TRUE)

  # the crude column carries BOTH the observed level and the crude effect, in one cell
  for (col in c("pct", "or")) {
    get <- if (col == "pct") get_pct else get_or
    col <- "^Obs_OR"
    expect_equal(mid_cell(t_des, col, get), mid_cell(t_wt, col, get), tolerance = 1e-10)
    # not vacuous: the weighted and unweighted values really do differ here
    expect_false(isTRUE(all.equal(mid_cell(t_des, col, get), mid_cell(t_un, col, get),
                                  tolerance = 1e-3)))
  }
  # and the crude % really is the design-weighted proportion
  lev <- reg_call(t_des)$positive_level
  oracle <- with(b[b$x == "mid", ], sum(w * (y == lev)) / sum(w))
  expect_equal(mid_cell(t_des, "^Obs_OR", get_pct), oracle, tolerance = 1e-8)
})



# ---- D4 / Q5: replicate and two-phase designs are refused, not crashed ----------------------------

test_that("D4 a replicate design is refused with a message pointing at svydesign()", {
  b   <- svy_fixture(n = 1000)
  des <- survey::svydesign(~psu, weights = ~w, data = b)
  rp  <- suppressWarnings(survey::as.svrepdesign(des, type = "bootstrap", replicates = 10))
  expect_error(tab(rp, x, y, pct = "row"), "svydesign")
  expect_error(tab_reg(rp, outcome = "y", predictors = "x", family = "binomial"), "svydesign")
})



# ---- D5: every microdata entry point accepts a design; tab_counts refuses one ---------------------

test_that("D5 tab_num / tab_plain / tab_many accept a design, tab_counts refuses it", {
  b   <- svy_fixture(n = 1000)
  des <- survey::svydesign(~psu, weights = ~w, data = b)
  expect_s3_class(suppressMessages(tab_plain(des, x, y, pct = "row")), "tabxplor_tab")
  expect_s3_class(suppressMessages(tab_num(des, x, num)), "tabxplor_tab")
  expect_s3_class(suppressWarnings(suppressMessages(tab_many(des, x, y, pct = "row"))),
                  "tabxplor_tab")
  # the estimates really are design-weighted (tab_plain agrees with tab)
  tp <- suppressMessages(tab_plain(des, x, y, pct = "row"))
  tt <- suppressMessages(tab(des, x, y, pct = "row"))
  expect_equal(get_pct(tp[[levels(b$y)[1]]])[1], get_pct(tt[[levels(b$y)[1]]])[1], tolerance = 1e-10)
  expect_error(tab_counts(des), "pre-aggregated counts")
})



# ---- D6 / ruling Q3: the test AND the effect size follow the weights ------------------------------

cramer_v <- function(M) {
  N <- sum(M); E <- outer(rowSums(M), colSums(M)) / N
  sqrt(sum((M - E)^2 / E) / (N * (min(dim(M)) - 1)))
}



# ---- D7 / D8: the footer names the design, never the internal column ------------------------------

test_that("D7/D8 the footer says 'survey design', and tab_reg emits a weight line at all", {
  b   <- svy_fixture(n = 1000)
  des <- survey::svydesign(~psu, weights = ~w, data = b)
  tt  <- suppressMessages(tab(des, x, y, pct = "row", test = TRUE))
  line <- tabxplor:::tab_weight_line(tt, lang = "en")
  expect_true(!is.null(line))
  expect_false(grepl(".svy_weights", line, fixed = TRUE))
  # z14-ii replaced z14-i's placeholder ("Weighted by the survey design.") by ruling Q7's sentence,
  # now that the intervals account for the design too (test-survey-variance.R pins the wording).
  expect_match(line, "sample design")

  # ...and the SHORT half of it where the table shows no interval, star, test or gated colour:
  # the caveat has nothing to qualify there (v2.0.1 phase 4).
  plain <- suppressMessages(tab(des, x, y, pct = "row"))
  expect_false(tabxplor:::tab_shows_inference(plain))
  expect_equal(tabxplor:::tab_weight_line(plain, lang = "en"),
               "Design-based (survey): weighted estimates.")
  expect_false(grepl(".svy_weights", tabxplor:::tab_weight_line(plain, lang = "en"), fixed = TRUE))

  tr <- suppressMessages(tab_reg(des, outcome = "y", predictors = "x", family = "binomial"))
  line_reg <- tabxplor:::tab_weight_line(tr, lang = "en")
  expect_true(!is.null(line_reg))                    # D8: there used to be no line at all
  expect_false(grepl(".svy_weights", line_reg, fixed = TRUE))

  # an ordinary weight still names its column
  expect_match(tabxplor:::tab_weight_line(tab(b, x, y, wt = w, pct = "row"), lang = "en"), "w")
})



# ---- D10 + the row alignment: calibrated designs -------------------------------------------------

calib_fixture <- function(m = 400, seed = 9, na_rows = 30) {
  set.seed(seed)
  d <- data.frame(psu = factor(rep(seq_len(m / 10), each = 10)),
                  aux = factor(sample(c("p", "q"), m, TRUE)),
                  x   = factor(sample(c("a", "b", "c"), m, TRUE)),
                  w   = stats::runif(m, .5, 3))
  d$y <- factor(ifelse(stats::rbinom(m, 1, .4) == 1, "yes", "no"), levels = c("no", "yes"))
  if (na_rows > 0) d$x[seq_len(na_rows)] <- NA
  d
}



# ---- ruling Q2: the rung is derived, `test` is TRUE/FALSE -----------------------------------------

test_that("Q2 the test rung follows the input and `test` takes no other value", {
  b   <- svy_fixture(n = 1000)
  des <- survey::svydesign(~psu, weights = ~w, data = b)

  expect_equal(get_test(tab(b, x, y, pct = "row", test = TRUE))$test[1], "chi2")
  expect_equal(get_test(tab(b, x, y, wt = w, pct = "row", test = TRUE))$test[1], "chi2")
  withr::with_options(list(tabxplor.design_effect = TRUE), {
    expect_equal(get_test(tab(b, x, y, wt = w, pct = "row", test = TRUE))$test[1], "chi2_design")
  })
  expect_equal(suppressMessages(get_test(tab(des, x, y, pct = "row", test = TRUE)))$test[1],
               "chi2_design")

  expect_error(tab(b, x, y, test = "survey"), "TRUE")
  expect_error(tab(b, x, y, test = "surveyy"), "TRUE")
  expect_error(tab_counts(data.frame(r = "a", c = "b", n = 1L),
                          r, c, counts = n, test = "survey"), "TRUE")
})




# === SECTION: crude columns under a design ========================================================

skip_if_not_installed("survey")



# Weights correlated with the outcome AND a real PSU cluster effect, so design != weighted !=
# unweighted by a wide margin -- the "segregated predictor" shape S3.4 measured at 2.3-2.6x.
svc_fixture <- function(n = 1500, seed = 3) {
  set.seed(seed)
  d <- data.frame(strat = factor(rep(1:4, each = n / 4)),
                  psu   = factor(rep(seq_len(n / 25), each = 25)))
  d$h <- stats::rnorm(n)
  d$w <- exp(0.7 * d$h); d$w <- d$w / mean(d$w)
  d$x <- factor(sample(c("a", "b", "c"), n, TRUE), levels = c("a", "b", "c"))
  d$y <- factor(ifelse(stats::rbinom(
    n, 1, stats::plogis(-.2 + .9 * (d$x == "b") + .8 * d$h)) == 1, "yes", "no"),
    levels = c("no", "yes"))
  d$num  <- round(stats::rnorm(n, 10, 3) + 2 * (d$x == "b") + d$h, 4)
  d$succ <- stats::rbinom(n, 8L, stats::plogis(-.3 + .7 * (d$x == "b") + .4 * d$h))
  d
}



svc_des <- function(d) suppressMessages(
  survey::svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE))



# the crude grid the columns are built from, straight off the boundary
svc_grid <- function(des, dep, key, positive = NULL, ...) {
  sv <- suppressMessages(svy_unwrap_data(des, "tab_reg"))
  reg_empirical(sv$data, "x", dep, key, positive, ".svy_weights",
                design_spec = list(design = des, wt = ".svy_weights"), ...)
}



svc_se <- function(o) as.numeric(unlist(survey::SE(o)))



# a single-model split_var table auto-spreads: one Model_OR column PER GROUP (Phase g).
svc_split_or <- function(tt, g, level = "b") {
  tb <- tibble::as_tibble(dplyr::ungroup(tt))
  unname(get_num(tb[[paste0("Model_OR_", g)]])[as.character(tb$levels) == level])
}




# ---- the intervals the columns print ------------------------------------------------------------

test_that("under a real design Obs_OR IS the univariable design-based fit, exactly", {
  # Phase 22b-xiii-2 (B1). A structured design breaks the closed form's own assumption: the two
  # compared groups share PSUs, so Woolf's 1/a+1/b+1/c+1/d drops a covariance the design carries and
  # the interval lands anywhere from 28 % narrow to 2.2x wide. There the crude column is REFIT
  # through the table's own fitter, which is what D22 asked for in the first place -- so the bracket
  # is not "close to" svyglm's any more, it IS svyglm's.
  d <- svc_fixture(); des <- svc_des(d)
  tt <- suppressMessages(tab_reg(des, "y", "x", family = "binomial", empirical = TRUE))
  oc <- tt[["Obs_OR"]]
  k  <- which(as.character(tt$levels) %in% c("b", "c"))

  # tab_reg models the outcome's FIRST level, so the OR is the inverse of I(y == "yes")'s.
  fit <- suppressWarnings(survey::svyglm(I(y == "no") ~ x, design = des,
                                         family = stats::quasibinomial()))
  ci  <- suppressMessages(stats::confint(fit))
  expect_equal(unname(get_num(oc)[k]),     unname(exp(stats::coef(fit)[2:3])), tolerance = 1e-9)
  expect_equal(unname(get_ci_inf(oc)[k]),  unname(exp(ci[2:3, 1])),            tolerance = 1e-9)
  expect_equal(unname(get_ci_sup(oc)[k]),  unname(exp(ci[2:3, 2])),            tolerance = 1e-9)
  # and it SAYS so: the same method word and the same df as the model column beside it, so the two
  # still fold into one legend block, and no effective base -- none was used.
  expect_identical(fmt_attr(oc, "ci_method"), fmt_attr(tt[["Model_OR"]], "ci_method"))
  expect_identical(get_degf(oc), as.double(stats::df.residual(fit)))
  expect_true(all(is.na(get_n_eff(oc))))
  # NON-VACUOUS: the closed form on the same grid is a different, measurably wrong interval.
  g  <- svc_grid(des, "y", "binomial", "yes")
  cf <- reg_empirical_columns(
    tibble::tibble(var = "x", level = levels(d$x), is_ref = c(TRUE, FALSE, FALSE)),
    g, "x", "binomial", "binomial", reg_estimand("binomial"), NA_real_, weighted = TRUE,
    degf = svy_degf(des))$cols[[1]]
  lw_cf <- log(get_ci_sup(cf) / get_ci_inf(cf))[2:3]
  lw_ok <- log(get_ci_sup(oc) / get_ci_inf(oc))[k]
  expect_true(any(abs(lw_cf / lw_ok - 1) > 0.02))
})




# === SECTION: weighted models: svyglm, svyolr, svyVGAM ============================================

skip_if_not_installed("survey")



# A small deterministic clustered + stratified fixture (psu nested in strata).
reg_survey_data <- function() {
  set.seed(42)
  n <- 1200L
  strata <- sample(c("A", "B", "C"), n, replace = TRUE)
  psu    <- paste0(strata, "-", sample(1:5, n, replace = TRUE))   # psu nested in strata
  x1     <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE))
  x2     <- rnorm(n)
  eta    <- -0.3 + 0.8 * (x1 == "hi") - 0.5 * (x1 == "mid") + 0.4 * x2
  y      <- rbinom(n, 1, plogis(eta))
  w      <- runif(n, 0.4, 3)
  tibble::tibble(y = factor(y), yb = factor(dplyr::if_else(y == 1, "event", "no")),
                 x1 = x1, x2 = x2, w = w, psu = psu, strata = strata)
}



or_col <- function(tab) {
  nm <- grep("^Model_", names(tab), value = TRUE)[1]
  vapply(tab[[nm]], tabxplor::get_num, numeric(1))
}



test_that("a clustered, stratified design matches a hand svyglm", {
  # Phase 18z14-i: clustering / stratification are expressed by BUILDING the design and passing it
  # as `data` -- the ids/strata/fpc/nest arguments are gone (they reached only the omnibus p-value,
  # and svydesign() says all four better).
  d   <- reg_survey_data()
  # tab_reg models the FIRST level of the 2-level factor as the event; match that coding by hand.
  d01  <- dplyr::mutate(d, y01 = as.integer(y == levels(y)[1]))
  des2 <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d01, nest = TRUE)
  hand <- survey::svyglm(y01 ~ x1 + x2, design = des2, family = quasibinomial())

  des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d, nest = TRUE)
  # Phase 18z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", which would compare a per-SD OR to a per-unit coefficient).
  tab <- suppressMessages(tab_reg(des, "y", c("x1", "x2"), multiplier = 1, ref = c(x2 = 0)))
  tv  <- or_col(tab)
  # skeleton = Constant, x1(ref), x1 mid, x1 hi, x2 -> drop the reference row (OR = 1) for the term match
  hand_or <- exp(stats::coef(hand))
  expect_equal(unname(tv[tv != 1]), unname(hand_or), tolerance = 1e-6)
})



test_that("passing wt alongside a design object ABORTS (Phase 18z16-i, W10)", {
  d   <- reg_survey_data()
  d01 <- dplyr::mutate(d, y01 = as.integer(y == 1))
  des <- survey::svydesign(ids = ~psu, weights = ~w, data = d01)
  # it used to be silently ignored with a console note nothing downstream could see; every other
  # variable-role collision in the package aborts, and now so does this one, in tab() too.
  expect_error(tab_reg(des, "y01", "x1", wt = "w"), "cannot be used when")
  expect_error(suppressMessages(tab(des, x1, y01, wt = w)), "cannot be used when")
})



# --- Phase 12g-ii: weighted 3+ level outcomes ------------------------------------------------------
reg_survey_multi_data <- function() {
  set.seed(7); n <- 900L
  x1 <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE)); x2 <- rnorm(n)
  lp <- 0.6 * (x1 == "hi") - 0.4 * (x1 == "mid") + 0.3 * x2
  yo <- cut(lp + rnorm(n), breaks = c(-Inf, -0.5, 0.5, Inf),
            labels = c("low", "mid", "high"), ordered = TRUE)
  yn <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
  w  <- runif(n, 0.5, 3)
  tibble::tibble(yo = yo, yn = yn, x1 = x1, x2 = x2, w = w)
}



test_that("weighted ordinal (svyolr) matches a hand svyolr cumulative OR", {
  d   <- reg_survey_multi_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  hand <- survey::svyolr(yo ~ x1 + x2, design = des)

  tab <- tab_reg(d, "yo", c("x1", "x2"), family = "ordinal", wt = "w", multiplier = 1)
  oc  <- vapply(tab[[grep("^Model_", names(tab), value = TRUE)[1]]], tabxplor::get_num, numeric(1))
  # skeleton = Constant (NA), x1 ref (1), x1lo, x1mid, x2 -> drop NA + reference
  oc  <- oc[!is.na(oc) & oc != 1]
  expect_equal(unname(oc), unname(exp(hand$coefficients)), tolerance = 1e-5)
})



test_that("weighted multinomial (svyVGAM) matches a hand svy_vglm OR", {
  skip_if_not_installed("svyVGAM")
  skip_if_not_installed("VGAM")
  d   <- reg_survey_multi_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  hand <- svyVGAM::svy_vglm(yn ~ x1 + x2, design = des,
                            family = VGAM::multinomial(refLevel = 1))
  hand_or <- exp(stats::coef(hand))

  tab <- tab_reg(d, "yn", c("x1", "x2"), family = "multinomial", wt = "w", ref = c(x2 = 0),
                 multiplier = 1)
  # one OR column per non-reference outcome category ("B", "C"); 14w strips the trailing ": OR"
  or_cols <- grep(" vs ", names(tab), value = TRUE)
  expect_length(or_cols, 2L)
  tv <- unlist(lapply(or_cols, function(nm) {
    v <- vapply(tab[[nm]], tabxplor::get_num, numeric(1)); v[!is.na(v) & v != 1]
  }))
  expect_equal(sort(unname(tv)), sort(unname(hand_or)), tolerance = 1e-4)
})



test_that("weighted multinomial errors clearly without svyVGAM", {
  skip_if("svyVGAM" %in% rownames(utils::installed.packages()))
  d <- reg_survey_multi_data()
  expect_error(tab_reg(d, "yn", c("x1", "x2"), family = "multinomial", wt = "w"), "svyVGAM")
})



# --- Phase 12g-iii: split_var (stacked grouped subtables + tab_spread) ------------------------------
reg_split_data <- function() {
  set.seed(11); n <- 1500L
  g  <- factor(sample(c("north", "south"), n, replace = TRUE))
  x1 <- factor(sample(c("a", "b", "c"), n, replace = TRUE)); x2 <- rnorm(n)
  y  <- rbinom(n, 1, plogis(-0.2 + 0.5 * (x1 == "b") + 0.3 * x2 + 0.4 * (g == "south")))
  tibble::tibble(y = factor(y), g = g, x1 = x1, x2 = x2, w = runif(n, 0.5, 3))
}
