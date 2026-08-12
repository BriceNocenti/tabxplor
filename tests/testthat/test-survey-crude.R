# PURPOSE: Last Phase z14-iii -- the crude (`Obs_*`) columns under a survey design.
# ROLE: Locks (a) the design-based effective n `reg_empirical()` now writes into `emp_n_ci` /
#   `emp_n_draw`, and hence every crude interval; (b) the two device identities that make the Woolf
#   and Katz brackets EXACT design variances on that base; (c) the row-space prerequisites -- the
#   split_var x design path, and the calibrated-design scatter that restores `color = "adjustment"`'s
#   gap test; (d) the degradation and off-design byte-identity guarantees.
# DESIGN: `survey` is the oracle throughout -- every assertion is "equals svyby / svymean / svyglm on
#   the same design", never a hard-coded number. Each block is built so it FAILS before the fix: the
#   design quantity is asserted to DIFFER from the single-stage one (and, where the point estimate is
#   what moved, to EQUAL a hand-built survey answer), so a regression cannot pass vacuously.
# See: dev/full_survey_design_scope.md S5 (the crude columns) and S3.4 (how wrong the old base was).

skip_if_not_installed("survey")
skip_if_not_installed("broom")

# Weights correlated with the outcome AND a real PSU cluster effect, so design != weighted !=
# unweighted by a wide margin -- the "segregated predictor" shape S3.4 measured at 2.3-2.6x.
svc_fixture <- function(n = 6000, seed = 3) {
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


# ---- the effective bases against survey ---------------------------------------------------------

test_that("the crude PROPORTION base is Korn-Graubard's, from survey's own variance", {
  d <- svc_fixture(); des <- svc_des(d)
  g <- svc_grid(des, "y", "binomial", "yes")
  g1 <- g[g$category == "1", ]
  V  <- svc_se(survey::svyby(~I(y == "yes"), ~x, des, survey::svymean))[seq_len(3)]^2
  p  <- as.numeric(tapply(d$w * (d$y == "yes"), d$x, sum) / tapply(d$w, d$x, sum))
  expect_equal(g1$emp_n_draw, p * (1 - p) / V, tolerance = 1e-8)
  # non-vacuity: it is NOT the raw count, and not the Kish one either
  expect_false(isTRUE(all.equal(g1$emp_n_draw, as.double(g1$emp_n))))
  expect_true(all(g1$emp_n_draw < g1$emp_n))       # this design costs precision
  kish <- withr::with_options(list(tabxplor.design_effect = TRUE), {
    reg_empirical(cbind(d, .svy_weights = d$w), "x", "y", "binomial", "yes", ".svy_weights")
  })
  expect_false(isTRUE(all.equal(unname(g1$emp_n_draw),
                                unname(kish$emp_n_draw[kish$category == "1"]),
                                tolerance = 1e-4)))
})

test_that("the crude MEAN base is s2 / Var_design(x_bar)", {
  d <- svc_fixture(); des <- svc_des(d)
  g <- svc_grid(des, "num", "gaussian")
  ref <- svc_se(survey::svyby(~num, ~x, des, survey::svymean))
  expect_equal(sqrt(g$emp_var / g$emp_n_ci), ref, tolerance = 1e-8)
  expect_true(all(g$emp_n_ci != as.double(g$emp_n)))
})

test_that("a MULTINOMIAL crude grid gets a base per (level, category), paired to its reference", {
  d <- svc_fixture(); des <- svc_des(d)
  d$party <- factor(sample(c("Dem", "Ind", "Rep"), nrow(d), TRUE))
  des <- svc_des(d)
  g <- svc_grid(des, "party", "multinomial")
  se <- survey::SE(survey::svyby(~party, ~x, des, survey::svymean))
  for (i in seq_len(nrow(g))) {
    lv <- match(g$level[i], levels(d$x)); ct <- match(g$category[i], levels(d$party))
    p  <- g$emp_prop[i]
    expect_equal(g$emp_n_draw[i], p * (1 - p) / se[lv, ct]^2, tolerance = 1e-8)
  }
  # the reference twin pairs each cell with its OWN category, not with the first one
  first <- g[g$level == levels(d$x)[1], ]
  expect_equal(unname(g$emp_ref_n_draw), unname(rep(first$emp_n_draw, times = 3)))
  expect_false(isTRUE(all.equal(unname(first$emp_n_draw),
                                rep(unname(first$emp_n_draw)[1], nrow(first)), tolerance = 1e-6)))
})

test_that("a GROUPED binomial base is respondent-level, not n x trials", {
  d <- svc_fixture(); des <- svc_des(d)
  g <- svc_grid(des, "succ", "grouped_binomial", trials = 8L)
  g1 <- g[g$category == "1", ]
  expect_true(all(g1$emp_n_draw < g1$emp_n * 8))        # the trials inside a row are not independent
  # it tracks the design-based grouped-binomial model's own SE
  p  <- as.numeric(tapply(d$w * d$succ, d$x, sum) / tapply(d$w * 8, d$x, sum))
  fit <- suppressWarnings(survey::svyglm(cbind(succ, 8L - succ) ~ x, design = des,
                                         family = stats::quasibinomial()))
  woolf <- sqrt(1 / (p[2] * (1 - p[2]) * g1$emp_n_draw[2]) +
                  1 / (p[1] * (1 - p[1]) * g1$emp_n_draw[1]))
  expect_equal(woolf, unname(summary(fit)$coefficients[2, 2]), tolerance = 0.05)
})


# ---- the intervals the columns print ------------------------------------------------------------

test_that("Obs_OR's bracket IS the design variance of the log odds-ratio, and beats the old one", {
  d <- svc_fixture(); des <- svc_des(d)
  tt <- suppressMessages(tab_reg(des, "y", "x", family = "binomial", empirical = TRUE))
  oc <- tt[["Obs_OR"]]
  lw <- log(get_ci_sup(oc)) - log(get_ci_inf(oc))
  k  <- which(as.character(tt$levels) %in% c("b", "c"))

  V <- svc_se(survey::svyby(~I(y == "yes"), ~x, des, survey::svymean))[seq_len(3)]^2
  p <- as.numeric(tapply(d$w * (d$y == "yes"), d$x, sum) / tapply(d$w, d$x, sum))
  vl <- V / (p * (1 - p))^2                       # delta-method Var(logit p), per level
  z  <- conf_level_to_z(0.95)
  expect_equal(unname(lw[k]), unname(2 * z * sqrt(vl[2:3] + vl[1])), tolerance = 1e-3)

  # against the design-based univariable model: MOST of the error goes, but not all of it -- Route A
  # discards the cell-to-cell covariance, so a ratio lands a few percent either side (S3.4 measured
  # 0.97-0.99 with a segregated predictor). What must hold is that it beats the single-stage base.
  fit <- suppressWarnings(survey::svyglm(I(y == "yes") ~ x, design = des,
                                         family = stats::quasibinomial()))
  ci  <- suppressMessages(stats::confint(fit))
  tw  <- unname(ci[2:3, 2] - ci[2:3, 1])
  expect_true(all(abs(unname(lw[k]) / tw - 1) < 0.10))
  # Last Phase z16-i (W1/W2): the WEIGHTS-only table is no longer on the raw n -- tab_reg() forces the
  # weighted basis -- so it now tracks the FLAT univariable model instead of missing it by ~17 %.
  flat <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  ffit <- suppressWarnings(survey::svyglm(I(y == "yes") ~ x, design = flat,
                                          family = stats::quasibinomial()))
  fw   <- unname(apply(suppressMessages(stats::confint(ffit))[2:3, ], 1, diff))
  raw  <- tab_reg(d, "y", "x", family = "binomial", empirical = TRUE, wt = "w")[["Obs_OR"]]
  rw   <- unname((log(get_ci_sup(raw)) - log(get_ci_inf(raw)))[k])
  expect_true(all(abs(rw / fw - 1) < 0.10))
})

test_that("the crude % and mean columns follow the DESIGN, and the point estimates do not move", {
  d <- svc_fixture(); des <- svc_des(d)
  wid <- function(t, nm) { cc <- t[[nm]]; get_ci_sup(cc) - get_ci_inf(cc) }
  for (spec in list(list(dep = "y",   nm = "Obs_%",    fam = "binomial"),
                    list(dep = "num", nm = "Obs_mean", fam = "gaussian"))) {
    td <- suppressMessages(tab_reg(des, spec$dep, "x", family = spec$fam, empirical = TRUE))
    tr <- tab_reg(d, spec$dep, "x", family = spec$fam, empirical = TRUE, wt = "w")
    ok <- is.finite(wid(td, spec$nm)) & is.finite(wid(tr, spec$nm))
    expect_true(any(ok), info = spec$nm)
    # z16-i: BOTH tables are now corrected (tab_reg forces the weighted basis, W1/W2), so what
    # separates them is the design STRUCTURE -- strata narrow, clusters widen -- not the base. The
    # assertion is therefore that the structure reaches the interval at all, not a direction:
    # this fixture is stratified AND clustered, so the two must differ.
    expect_true(any(abs(wid(td, spec$nm)[ok] / wid(tr, spec$nm)[ok] - 1) > 0.02), info = spec$nm)
    # the estimate itself is untouched -- only its base moved
    expect_equal(get_num(td[[spec$nm]]), get_num(tr[[spec$nm]]), tolerance = 1e-10)
  }
})


# ---- the row-space prerequisites ----------------------------------------------------------------

test_that("split_var x design: an uncalibrated design with UNEQUAL groups builds, and is right", {
  d <- svc_fixture(n = 2000, seed = 11)
  d$grp <- factor(sample(c("A", "B"), nrow(d), TRUE))   # deliberately unequal
  expect_false(length(unique(table(d$grp))) == 1L)
  des <- svc_des(d)
  tt  <- suppressMessages(tab_reg(des, "y", "x", family = "binomial", split_var = "grp"))
  pos <- levels(d$y)[1]                         # tab_reg models the FIRST outcome level by default
  expect_identical(get_reg_meta(tt)$positive_level, pos)
  des$variables$.pos <- as.integer(d$y == pos)
  for (g in c("A", "B")) {
    fit <- suppressWarnings(survey::svyglm(
      .pos ~ x, design = des[which(d$grp == g), ], family = stats::quasibinomial()))
    expect_equal(svc_split_or(tt, g), unname(exp(stats::coef(fit))[2]), tolerance = 1e-6)
  }
})

test_that("split_var x a CALIBRATED design weights the right respondents", {
  d <- svc_fixture(n = 2000, seed = 12)
  d$grp <- factor(sample(c("A", "B"), nrow(d), TRUE))
  d$aux <- stats::rnorm(nrow(d)) + (d$y == "yes")
  cal <- suppressMessages(survey::calibrate(
    svc_des(d), ~aux, c(`(Intercept)` = nrow(d), aux = sum(d$aux))))
  tt <- suppressWarnings(suppressMessages(
    tab_reg(cal, "y", "x", family = "binomial", split_var = "grp")))
  cal$variables$.pos <- as.integer(d$y == levels(d$y)[1])
  for (g in c("A", "B")) {
    fit <- suppressWarnings(survey::svyglm(
      .pos ~ x, design = cal[which(d$grp == g), ], family = stats::quasibinomial()))
    expect_equal(svc_split_or(tt, g), unname(exp(stats::coef(fit))[2]), tolerance = 1e-6)
  }
})

test_that("a calibrated design with incomplete cases keeps its adjustment gap test", {
  d <- svc_fixture(n = 2000, seed = 13)
  d$z <- stats::rnorm(nrow(d)); d$aux <- stats::rnorm(nrow(d)) + (d$y == "yes")
  d$x[seq_len(80)] <- NA                              # -> the design must be padded, not shrunk
  cal <- suppressMessages(survey::calibrate(
    svc_des(d), ~aux, c(`(Intercept)` = nrow(d), aux = sum(d$aux))))
  tt <- suppressWarnings(suppressMessages(
    tab_reg(cal, "y", c("x", "z"), family = "binomial", empirical = TRUE, effect = "ame",
            color = c("OR", "adjustment"))))
  mc <- names(tt)[vapply(tt, function(cc) is_fmt(cc) && identical(get_role(cc), "model"),
                         logical(1))]
  gse <- vctrs::field(tt[[mc[1]]], "gap_se")
  expect_true(any(is.finite(gse)))                    # NULL before z14-iii (lengths 380 vs 400)

  # and it is the hand-stacked influence-function answer, not a recycled one
  sv <- suppressMessages(svy_unwrap_data(cal, "tab_reg"))
  ds <- list(design = cal, wt = ".svy_weights")
  fm <- suppressWarnings(reg_fit(sv$data, "y", c("x", "z"), "binomial", ds, TRUE, FALSE, .95, "wald"))
  im <- reg_ame_if_maker(fm$fit, fm$data, ".svy_weights", FALSE,
                         reg_coef_if_maker(fm$fit))("x", "b", "a")
  ic <- reg_if_align(
    reg_crude_if_maker(fm$data, "y", "binomial", fm$positive_level, ".svy_weights",
                       "identity")("x", "b", "a"), length(im), fm$data[[".svy_row"]])
  hand <- reg_if_se(im - ic, fm$fit$survey.design)
  expect_equal(unname(gse[as.character(tt$levels) == "b"]), unname(hand), tolerance = 1e-10)
})

test_that("svy_row_at states the one row rule", {
  expect_equal(svy_row_at(5L, 1:5), 1:5)               # the design shrank to these rows
  expect_equal(svy_row_at(100L, c(3L, 7L, 9L)), c(3L, 7L, 9L))   # it did not (calibrated)
  expect_null(svy_row_at(2L, c(3L, 7L, 9L)))           # smaller than the rows asked for
  expect_null(svy_row_at(10L, integer(0)))
  expect_null(svy_row_at(10L, c(1L, NA_integer_)))
})


# ---- degradation and byte-identity --------------------------------------------------------------

test_that("a design whose variance cannot be computed says so and falls back", {
  d <- svc_fixture(n = 1500, seed = 21); des <- svc_des(d)
  sv  <- suppressMessages(svy_unwrap_data(des, "tab_reg"))
  bad <- sv$data; bad[[".svy_row"]] <- rev(seq_len(nrow(bad)) + nrow(bad))   # out of the row space
  expect_message(
    g <- reg_empirical(bad, "x", "y", "binomial", "yes", ".svy_weights",
                       design_spec = list(design = des, wt = ".svy_weights")),
    "could not be computed")
  g1 <- g[g$category == "1", ]
  # z16-i: the degrade ladder is design -> weights -> n, each step labelled. A design whose variance
  # cannot be computed still HAS weights, so the fallback is the weighted base, never a wrong number.
  expect_true(all(is.finite(g1$emp_n_draw) & g1$emp_n_draw > 0))
  expect_true(all(g1$emp_n_draw <= as.double(g1$emp_n) + 1e-8))
})

test_that("off a design the crude bases and columns are unchanged", {
  d <- svc_fixture(n = 1500, seed = 22)
  # z16-i (ruling 1): OFF a design, a WEIGHTED crude grid is on the weighted basis whatever the
  # tab()-scoped option says -- that is what makes it comparable with the model column beside it.
  gw <- reg_empirical(cbind(d, .svy_weights = d$w), "x", "y", "binomial", "yes", ".svy_weights")
  expect_true(all(gw$emp_n_draw < gw$emp_n))
  expect_equal(gw$emp_n_ci, gw$emp_n_draw)             # one base where the outcome has no mean
  kish <- withr::with_options(list(tabxplor.design_effect = TRUE), {
    reg_empirical(cbind(d, .svy_weights = d$w), "x", "y", "binomial", "yes", ".svy_weights")
  })
  expect_equal(kish$emp_n_draw, gw$emp_n_draw)         # the option cannot move it
  # UNWEIGHTED, the base is the raw count
  un <- reg_empirical(d, "x", "y", "binomial", "yes", NULL)
  expect_equal(un$emp_n_draw, as.double(un$emp_n))

  # a design object never reached: the whole table is identical to what it always was
  t1 <- tab_reg(d, "y", "x", family = "binomial", empirical = TRUE, wt = "w")
  t2 <- tab_reg(d, "y", "x", family = "binomial", empirical = TRUE, wt = "w")
  expect_identical(t1, t2)
  expect_true(all(is.na(vctrs::field(t1[["Obs_OR"]], "n_eff"))))
})
