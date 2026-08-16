# PURPOSE: Phase 18z14-i -- the survey-design path made honest.
# ROLE: Locks the defects documented in dev/full_survey_design_scope.md S2.3 (D1-D10), the removal of
#   the ids/strata/fpc/nest arguments (ruling Q4) and the DERIVED test rung (ruling Q2).
# DESIGN: `survey` is the oracle throughout -- every assertion is "equals svychisq / svyglm /
#   avg_comparisons on the same design", never a hard-coded number. Each fixture is built so it FAILS
#   before its fix: the weighted quantity is asserted to differ from the unweighted one, so a
#   regression that silently drops the weights cannot pass vacuously.

skip_if_not_installed("survey")

# A design whose weights correlate with the outcome (so weighted != unweighted by a wide margin) and
# whose PSUs carry a real cluster effect.
svy_fixture <- function(n = 6000, seed = 4) {
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

  for (col in c("^Obs_%", "^Obs_OR")) {
    get <- if (col == "^Obs_%") get_pct else get_or
    expect_equal(mid_cell(t_des, col, get), mid_cell(t_wt, col, get), tolerance = 1e-10)
    # not vacuous: the weighted and unweighted values really do differ here
    expect_false(isTRUE(all.equal(mid_cell(t_des, col, get), mid_cell(t_un, col, get),
                                  tolerance = 1e-3)))
  }
  # and the crude % really is the design-weighted proportion
  lev <- reg_call(t_des)$positive_level
  oracle <- with(b[b$x == "mid", ], sum(w * (y == lev)) / sum(w))
  expect_equal(mid_cell(t_des, "^Obs_%", get_pct), oracle, tolerance = 1e-8)
})

test_that("D2 effect = 'ame' under a design is the POPULATION-average marginal effect", {
  skip_if_not_installed("marginaleffects")
  b   <- svy_fixture()
  des <- survey::svydesign(~psu, weights = ~w, data = b)
  ame <- function(t) {
    col <- t[[grep("^Model_AME", names(t))[1]]]
    unname(get_diff(col)[which(as.character(t$levels) == "high")])
  }
  a_des <- ame(suppressMessages(
    tab_reg(des, outcome = "y", predictors = c("x", "z"), family = "binomial", effect = "marginal")))
  a_wt  <- ame(tab_reg(b, outcome = "y", predictors = c("x", "z"), family = "binomial",
                       effect = "marginal", wt = "w"))
  a_un  <- ame(tab_reg(b, outcome = "y", predictors = c("x", "z"), family = "binomial",
                       effect = "marginal"))
  expect_equal(a_des, a_wt, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(a_des, a_un, tolerance = 1e-3)))
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

test_that("D6 a weighted table's chi2 and effect size are computed on the WEIGHTED table", {
  suppressWarnings(utils::data("api", package = "survey"))
  d  <- get("apistrat")
  te <- get_test(tab(d, sch.wide, awards, wt = pw, test = TRUE))
  Mw <- as.matrix(stats::xtabs(pw ~ sch.wide + awards, d))
  Mn <- as.matrix(table(d$sch.wide, d$awards))
  Ms <- Mw * sum(Mn) / sum(Mw)                       # weighted counts rescaled to the raw n
  ref <- suppressWarnings(stats::chisq.test(Ms))

  expect_equal(te$effect_size[1], cramer_v(Mw), tolerance = 1e-8)
  expect_equal(te$statistic[1],   unname(ref$statistic), tolerance = 1e-8)
  expect_equal(te$pvalue[1],      ref$p.value, tolerance = 1e-8)
  expect_equal(te$n[1],           sum(Mn))          # the reported n stays the sample size
  # not vacuous: the weighted V differs from the unweighted one
  expect_false(isTRUE(all.equal(te$effect_size[1], cramer_v(Mn), tolerance = 1e-3)))
})

test_that("an UNWEIGHTED table is byte-identical to chisq.test (nothing moved)", {
  suppressWarnings(utils::data("api", package = "survey"))
  d  <- get("apistrat")
  te <- get_test(tab(d, sch.wide, awards, test = TRUE))
  Mn <- as.matrix(table(d$sch.wide, d$awards))
  expect_equal(te$statistic[1],   unname(suppressWarnings(stats::chisq.test(Mn))$statistic))
  expect_equal(te$effect_size[1], cramer_v(Mn))
})

# ---- D7 / D8: the footer names the design, never the internal column ------------------------------

test_that("D7/D8 the footer says 'survey design', and tab_reg emits a weight line at all", {
  b   <- svy_fixture(n = 1000)
  des <- survey::svydesign(~psu, weights = ~w, data = b)
  tt  <- suppressMessages(tab(des, x, y, pct = "row"))
  line <- tabxplor:::tab_weight_line(tt, lang = "en")
  expect_true(!is.null(line))
  expect_false(grepl(".svy_weights", line, fixed = TRUE))
  # z14-ii replaced z14-i's placeholder ("Weighted by the survey design.") by ruling Q7's sentence,
  # now that the intervals account for the design too (test-survey-variance.R pins the wording).
  expect_match(line, "sample design")

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

test_that("D10 tab_reg() on a CALIBRATED design with incomplete cases works and is exact", {
  d   <- calib_fixture()
  des <- survey::svydesign(~psu, weights = ~w, data = d)
  cal <- survey::calibrate(des, ~aux, c(`(Intercept)` = sum(d$w),
                                        auxq = sum(d$w[d$aux == "q"]) * 1.05))
  # `[` does NOT drop rows on a calibrated design -- it marks them prob = Inf. Assigning the shorter
  # complete-case frame used to abort here.
  tr <- suppressMessages(suppressWarnings(
    tab_reg(cal, outcome = "y", predictors = "x", family = "binomial")))
  expect_s3_class(tr, "tabxplor_tab")

  lev  <- reg_call(tr)$positive_level
  cal2 <- cal; cal2$variables$.pos <- as.integer(d$y == lev)
  hand <- suppressWarnings(survey::svyglm(.pos ~ x, design = cal2[which(!is.na(d$x)), ],
                                          family = stats::quasibinomial()))
  or_col <- tr[[grep("^Model_OR", names(tr))[1]]]
  expect_equal(unname(get_or(or_col)[3:4]),
               unname(exp(stats::coef(hand))[c("xb", "xc")]), tolerance = 1e-8)
})

test_that("the design-based p describes the table SHOWN, not the original frame", {
  # The overlay used to test `design$variables` -- the ORIGINAL frame -- so `other_if_less_than`
  # lumping, `filter=` and level relabelling were invisible to it.
  d <- calib_fixture(m = 600, seed = 11, na_rows = 0)
  # an UNBALANCED row variable, so lumping merges the two rare levels and leaves 3 shown levels
  set.seed(11)
  d$x <- factor(sample(c("a", "b", "c", "d"), nrow(d), TRUE, prob = c(.55, .3, .1, .05)))
  thr <- 100
  des <- survey::svydesign(~psu, weights = ~w, data = d)
  lumped <- suppressMessages(tab(des, x, y, pct = "row", other_if_less_than = thr, test = TRUE))
  shown  <- setdiff(as.character(unique(lumped$x)), "Total")
  expect_true("Others" %in% shown)                   # the fixture really does lump
  expect_true(length(shown) >= 3)

  d2 <- d
  d2$x <- factor(ifelse(d2$x %in% names(which(table(d2$x) < thr)), "Others", as.character(d2$x)))
  des2 <- survey::svydesign(~psu, weights = ~w, data = d2)
  ref  <- survey::svychisq(~x + y, des2, statistic = "F")
  expect_equal(get_test(lumped)$pvalue[1], unname(ref$p.value), tolerance = 1e-6)
})

test_that("subtable p-values on a design match svychisq on the hand-subset design", {
  d   <- calib_fixture(m = 600, seed = 13, na_rows = 0)
  d$g <- factor(rep(c("g1", "g2"), length.out = nrow(d)))
  des <- survey::svydesign(~psu, weights = ~w, data = d)
  cal <- survey::calibrate(des, ~aux, c(`(Intercept)` = sum(d$w),
                                        auxq = sum(d$w[d$aux == "q"]) * 1.05))
  tt  <- suppressMessages(tab(cal, x, y, g, pct = "row", test = TRUE))
  got <- get_test(tt)
  for (lv in levels(d$g)) {
    ref <- survey::svychisq(~x + y, cal[which(d$g == lv), ], statistic = "F")
    expect_equal(got$pvalue[as.character(got$g) == lv][1], unname(ref$p.value), tolerance = 1e-6)
  }
})

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

test_that("the reserved weight name cannot be forged", {
  b <- svy_fixture(n = 200)
  names(b)[names(b) == "w"] <- ".svy_weights"
  expect_error(tab(b, x, y, wt = .svy_weights, pct = "row"), "reserve")
})
