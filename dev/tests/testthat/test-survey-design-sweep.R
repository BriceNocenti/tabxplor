
# === SECTION: the design boundary, end to end =====================================================

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


# ---- D6 / ruling Q3: the test AND the effect size follow the weights ------------------------------

cramer_v <- function(M) {
  N <- sum(M); E <- outer(rowSums(M), colSums(M)) / N
  sqrt(sum((M - E)^2 / E) / (N * (min(dim(M)) - 1)))
}


test_that("an UNWEIGHTED table is byte-identical to chisq.test (nothing moved)", {
  suppressWarnings(utils::data("api", package = "survey"))
  d  <- get("apistrat")
  te <- get_test(tab(d, sch.wide, awards, test = TRUE))
  Mn <- as.matrix(table(d$sch.wide, d$awards))
  expect_equal(te$statistic[1],   unname(suppressWarnings(stats::chisq.test(Mn))$statistic))
  expect_equal(te$effect_size[1], cramer_v(Mn))
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


test_that("the reserved weight name cannot be forged", {
  b <- svy_fixture(n = 200)
  names(b)[names(b) == "w"] <- ".svy_weights"
  expect_error(tab(b, x, y, wt = .svy_weights, pct = "row"), "reserve")
})


# === SECTION: crude columns under a design ========================================================

skip_if_not_installed("survey")


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


test_that("a WEIGHTS-only table keeps the closed form, and it tracks the flat univariable model", {
  d <- svc_fixture()
  # Phase 18z16-i (W1/W2): the WEIGHTS-only table is no longer on the raw n -- tab_reg() forces the
  # weighted basis -- so it now tracks the FLAT univariable model instead of missing it by ~17 %.
  # ⚠ NO refit here (22b-xiii-2): with no clusters the closed form's independence assumption is TRUE,
  # so it stays -- which is what keeps a flat design and a `wt =` table telling the same story.
  flat <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  ffit <- suppressWarnings(survey::svyglm(I(y == "yes") ~ x, design = flat,
                                          family = stats::quasibinomial()))
  fw   <- unname(apply(suppressMessages(stats::confint(ffit))[2:3, ], 1, diff))
  tw   <- tab_reg(d, "y", "x", family = "binomial", empirical = TRUE, wt = "w")
  raw  <- tw[["Obs_OR"]]
  k    <- which(as.character(tw$levels) %in% c("b", "c"))
  rw   <- unname((log(get_ci_sup(raw)) - log(get_ci_inf(raw)))[k])
  expect_true(all(abs(rw / fw - 1) < 0.10))
  expect_identical(fmt_attr(raw, "ci_method"), "woolf")          # the closed form, still
  expect_true(any(is.finite(get_n_eff(raw))))                    # on its own effective base
})


test_that("the crude columns follow the DESIGN, and the point estimates do not move", {
  d <- svc_fixture(); des <- svc_des(d)
  wid <- function(t, nm) { cc <- t[[nm]]; get_ci_sup(cc) - get_ci_inf(cc) }
  for (spec in list(list(dep = "y",   nm = "Obs_OR",   fam = "binomial"),
                    list(dep = "num", nm = "Obs_diff", fam = "gaussian"))) {
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
  tt  <- suppressMessages(tab_reg(des, "y", "x", family = "binomial", tab_vars = "grp"))
  pos <- levels(d$y)[1]                         # tab_reg models the FIRST outcome level by default
  expect_identical(reg_call(tt)$positive_level, pos)
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
    tab_reg(cal, "y", "x", family = "binomial", tab_vars = "grp")))
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
    tab_reg(cal, "y", c("x", "z"), family = "binomial", empirical = TRUE, effect = "marginal", measure = "difference",
            color = c(TRUE, "adjustment"))))
  mc <- names(tt)[vapply(tt, function(cc) is_fmt(cc) && identical(get_role(cc), "model"),
                         logical(1))]
  gse <- vctrs::field(tt[[mc[1]]], "gap_se")
  expect_true(any(is.finite(gse)))                    # NULL before z14-iii (lengths 380 vs 400)

  # and it is the hand-stacked influence-function answer, not a recycled one
  sv <- suppressMessages(svy_unwrap_data(cal, "tab_reg"))
  ds <- list(design = cal, wt = ".svy_weights")
  fm <- suppressWarnings(reg_fit(sv$data, "y", c("x", "z"), "binomial", ds, TRUE, FALSE, .95, "wald"))
  im <- reg_ame_if_maker(fm$digest, fm$data, ".svy_weights", "identity",
                         reg_coef_if_maker(fm$digest, fm$data))("x", "b", "a")
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


test_that("a lonely-PSU design still gets a gap SE -- one lonely.psu policy, everywhere (defect 5)", {
  # reg_if_se() called survey::svyrecvar() with NO lonely-PSU policy while svy_var_recvar() and
  # svy_omnibus_one() both say "adjust". survey's default is "fail", so on a design with a
  # single-PSU stratum the call errored, the tryCatch returned NA and `color = "adjustment"` lost its
  # test -- on a design whose cell intervals and omnibus p had just been computed successfully.
  d <- svc_fixture(n = 1200, seed = 44)
  d$strat <- factor(ifelse(as.integer(d$psu) == 1L, "5", as.character(d$strat)))  # stratum 5: 1 PSU
  des <- suppressMessages(
    survey::svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE))
  sv <- suppressMessages(svy_unwrap_data(des, "tab_reg"))
  ds <- list(design = des, wt = ".svy_weights")
  # survey's DEFAULT policy, which is what reg_if_se() used to inherit
  old <- options(survey.lonely.psu = "fail"); on.exit(options(old), add = TRUE)
  fm <- local({
    o <- options(survey.lonely.psu = "adjust"); on.exit(options(o), add = TRUE)
    suppressWarnings(reg_fit(sv$data, "y", "x", "binomial", ds, TRUE, FALSE, .95, "wald"))
  })
  d_if <- reg_coef_if_maker(fm$digest, fm$data)(stats::setNames(1, "xb"))
  expect_error(survey::svyrecvar(as.matrix(d_if), des$cluster, des$strata, des$fpc,   # the old call
                                 postStrata = des$postStrata), "one PSU")
  se <- suppressWarnings(reg_if_se(d_if, fm$fit$survey.design))                       # the new one
  expect_true(is.finite(se) && se > 0)
  # and it is the same answer the package's other svyrecvar caller gives on the same influence vector
  expect_equal(se, sqrt(as.numeric(svy_var_recvar(as.matrix(d_if), fm$fit$survey.design))))
})


test_that("a degrade cannot escape the build it happened in (W-C)", {
  # It used to: the flag was a process-global environment, and reg_inference() read it. tab() reset it
  # per call from z16-i; tab_reg() never did, so ONE degraded table anywhere earlier in the session
  # permanently made every later reg table claim "design_partial" -- whose footer then denies a
  # variance that WAS computed. z16-iiiii makes it a local of the build, so the hazard is structural
  # rather than patched: there is nothing left to reset, and nothing left to go stale.
  d <- svc_fixture(n = 1200, seed = 31); des <- svc_des(d)
  clean <- suppressMessages(tab_reg(des, "y", "x", family = "binomial"))
  expect_identical(tab_inference_basis(clean), "design")     # non-vacuous: it IS "design" when clean

  # a real degrade, in its own build: the crude grid's rows fall outside the design's row space
  sv  <- suppressMessages(svy_unwrap_data(des, "tab_reg"))
  bad <- sv$data; bad[[".svy_row"]] <- rev(seq_len(nrow(bad)) + nrow(bad))
  expect_message(reg_empirical(bad, "x", "y", "binomial", "yes", ".svy_weights",
                               design_spec = list(design = des, wt = ".svy_weights")),
                 "could not be computed")

  after <- suppressMessages(tab_reg(des, "y", "x", family = "binomial"))
  expect_identical(tab_inference_basis(after), "design")
  expect_identical(tab_inference_degf(after), tab_inference_degf(clean))
  # and no reset was needed anywhere -- the helpers that made one necessary are gone
  expect_false(exists("svy_degrade_reset", envir = asNamespace("tabxplor"), inherits = FALSE))
})


test_that("the crude bracket is referred to the SAME degrees of freedom as the model (D4)", {
  # Phase 18z16-iiiii. An svyglm's df.residual IS the design df, so the Model_* columns were
  # already on t(degf) while every crude Obs_* interval beside them was on z -- at a small degf the
  # crude bracket printed NARROWER than the model bracket it exists to be compared with, in a table
  # whose whole premise is that the two are comparable.
  d <- svc_fixture(n = 900, seed = 12)
  d$psu <- factor(rep(seq_len(10), each = nrow(d) / 10))      # 10 clusters, no strata -> degf 9
  des <- suppressMessages(survey::svydesign(~psu, weights = ~w, data = d))
  dg <- svy_degf(des)
  expect_lt(dg, 30)                                           # non-vacuous: t(degf) != z here

  sv <- suppressMessages(svy_unwrap_data(des, "tab_reg"))
  ds <- list(design = des, wt = ".svy_weights", degf = dg)
  g  <- reg_empirical(sv$data, "x", "y", "binomial", "yes", ".svy_weights", design_spec = ds)
  cz <- reg_empirical_columns(
    tibble::tibble(var = "x", level = levels(d$x), is_ref = c(TRUE, FALSE, FALSE)),
    g, "x", "binomial", "binomial", reg_estimand("binomial"), NA_real_, weighted = TRUE)
  ct <- reg_empirical_columns(
    tibble::tibble(var = "x", level = levels(d$x), is_ref = c(TRUE, FALSE, FALSE)),
    g, "x", "binomial", "binomial", reg_estimand("binomial"), NA_real_, weighted = TRUE,
    degf = dg)
  or_z <- cz$cols[[1]]; or_t <- ct$cols[[1]]
  w_z <- get_ci_sup(or_z) / get_ci_inf(or_z)
  w_t <- get_ci_sup(or_t) / get_ci_inf(or_t)
  expect_true(all(w_t[-1] > w_z[-1]))                         # t(8) is WIDER than z, everywhere
  # and it is exactly the t/z ratio on the log scale, i.e. the same rule tab()'s own cells follow
  expect_equal(unname(log(w_t[-1]) / log(w_z[-1])),
               rep(conf_level_to_crit(0.95, dg) / conf_level_to_crit(0.95, Inf), 2),
               tolerance = 1e-10)
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
})


test_that("the crude columns STORE the effective base they used (W-D)", {
  d <- svc_fixture(n = 1500, seed = 23); des <- svc_des(d)
  # UNWEIGHTED: nothing corrected the base -> NA, exactly as an unweighted tab() cell carries NA
  un <- tab_reg(d, "y", "x", family = "binomial", empirical = TRUE)
  expect_true(all(is.na(get_n_eff(un[["Obs_OR"]]))))
  # WEIGHTED: the field carries the number reg_empirical() actually fed to ci_or() -- ?fmt says
  # `n_eff` IS "the effective sample size used for this cell's CI", which was false here until z16-iv.
  wt <- tab_reg(d, "y", "x", family = "binomial", empirical = TRUE, wt = "w")
  g  <- reg_empirical(cbind(d, .svy_weights = d$w), "x", "y", "binomial", "yes", ".svy_weights")
  g1 <- g[g$category == g$category[[1]], ]
  ne <- get_n_eff(wt[["Obs_OR"]]); ne <- ne[is.finite(ne)]
  expect_equal(sort(ne), sort(g1$emp_n_draw), tolerance = 1e-10)
  expect_true(all(ne < as.double(get_n(wt[["Obs_OR"]])[is.finite(get_n_eff(wt[["Obs_OR"]]))])))
  # a MEAN column takes the mean's own base (n_ci), not the draw base -- each column stores ITS OWN
  gg <- suppressWarnings(tab_reg(d, "num", "x", family = "gaussian", empirical = TRUE, wt = "w"))
  nm <- get_n_eff(gg[["Obs_diff"]]); expect_true(any(is.finite(nm)))
  # under a real DESIGN there is no effective base to store: 22b-xiii-2 refits the crude column from
  # the univariable design-based fit, and `n_eff` IS "the base this cell's CI was computed on".
  dsg <- suppressMessages(tab_reg(des, "y", "x", family = "binomial", empirical = TRUE))
  expect_true(all(is.na(get_n_eff(dsg[["Obs_OR"]]))))
  # the GRID still computes it -- it is the design-based effective base, and it is what the flat
  # closed form would have used; only the column no longer claims the interval came from it.
  gd <- svc_grid(des, "y", "binomial", "yes")
  expect_true(any(is.finite(gd$emp_n_draw)))
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


test_that("cox_snell_r2 is selectable via stats= for weighted models", {
  d   <- reg_survey_data()
  tab <- tab_reg(d, "y", "x1", wt = "w",
                   stats = c("n", "nagelkerke_r2", "cox_snell_r2"))
  tst <- tabxplor:::get_test(tab)
  expect_true("cox_snell_r2" %in% tst$test)
})


test_that("weighted model comparison emits a design-based Wald row", {
  d   <- reg_survey_data()
  des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d, nest = TRUE)
  tab <- suppressMessages(tab_reg(des, "y",
                     predictors = list(base = "x1", full = c("x1", "x2")),
                     stats = "compare_baseline"))
  tst <- tabxplor:::get_test(tab)
  expect_true("compare_baseline_wald" %in% tst$test)
  wr <- tst[tst$test == "compare_baseline_wald", ]
  expect_true(is.finite(wr$pvalue) && wr$pvalue >= 0 && wr$pvalue <= 1)
})


test_that("an unweighted binomial fit is unchanged by the design plumbing", {
  d  <- reg_survey_data()
  t0 <- tab_reg(d, "y", c("x1", "x2"), multiplier = 1, ref = c(x2 = 0))
  hand <- stats::glm(as.integer(y == levels(y)[1]) ~ x1 + x2, data = d, family = binomial())
  tv <- or_col(t0)
  expect_equal(unname(tv[tv != 1]), unname(exp(stats::coef(hand))), tolerance = 1e-6)
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


test_that("a weighted NOMINAL outcome can only be read on its coefficients", {
  skip_if_not_installed("svyVGAM")
  d <- reg_survey_multi_data()
  # marginaleffects has no survey method for a multinomial fit, so the refusal covers BOTH ways of
  # asking: naming the contrast, and naming a measure the model does not estimate.
  for (a in list(list(effect = "marginal"), list(measure = "difference")))
    expect_error(do.call(tab_reg, c(list(d, "yn", "x1", family = "multinomial", wt = "w"), a)),
                 "only be read on its coefficients")
})


test_that("a weighted ORDINAL outcome CAN be read on its rank measures", {
  d <- reg_survey_multi_data()
  # the exception to the rule above: a rank estimand runs on tabxplor's own g-computation over
  # svyolr's (beta, zeta), and takes its variance from that fit's already design-based vcov().
  for (a in list(list(effect = "marginal"), list(measure = "difference"), list(measure = "ratio"))) {
    t <- suppressMessages(suppressWarnings(
      do.call(tab_reg, c(list(d, "yo", "x1", family = "ordinal", wt = "w"), a))))
    col <- t[[grep("^Model_", names(t), value = TRUE)[[1]]]]
    expect_true(any(is.finite(get_ci_inf(col))))
  }
})


# --- Phase 12g-iii: split_var (stacked grouped subtables + tab_spread) ------------------------------
reg_split_data <- function() {
  set.seed(11); n <- 1500L
  g  <- factor(sample(c("north", "south"), n, replace = TRUE))
  x1 <- factor(sample(c("a", "b", "c"), n, replace = TRUE)); x2 <- rnorm(n)
  y  <- rbinom(n, 1, plogis(-0.2 + 0.5 * (x1 == "b") + 0.3 * x2 + 0.4 * (g == "south")))
  tibble::tibble(y = factor(y), g = g, x1 = x1, x2 = x2, w = runif(n, 0.5, 3))
}


test_that("each split group equals a manual per-subset fit", {
  d <- reg_split_data()
  # the groups are side by side now, so each group's estimates are its OWN column
  t <- dplyr::ungroup(tab_reg(d, "y", c("x1", "x2"), tab_vars = "g", multiplier = 1,
                              ref = c(x2 = 0)))
  for (grp in c("north", "south")) {
    sub  <- dplyr::filter(d, g == grp)
    hand <- stats::glm(as.integer(y == levels(y)[1]) ~ x1 + x2, data = sub, family = binomial())
    col  <- grep(paste0("^Model_.*", grp, "$"), names(t), value = TRUE)[1]
    tv   <- vapply(t[[col]], tabxplor::get_num, numeric(1))
    expect_equal(unname(tv[!is.na(tv) & tv != 1]), unname(exp(stats::coef(hand))), tolerance = 1e-6)
  }
})


test_that("tab_spread pivots split groups into side-by-side columns", {
  d  <- reg_split_data()
  # a models list stays stacked, so tab_spread() has something to pivot -- and this is the public
  # route for "full control of the layout" now that the auto-spread has no opt-out.
  t  <- tab_reg(d, "y", list(m1 = "x1", m2 = c("x1", "x2")), family = "binomial", tab_vars = "g")
  sp <- tab_spread(t, g)
  expect_s3_class(sp, "tabxplor_tab")
  # one OR column per split level (north / south), sharing the var/level stub
  expect_true(any(grepl("north", names(sp))) && any(grepl("south", names(sp))))
  expect_true(all(c("var", "levels") %in% names(sp)))
})


test_that("split_var footer carries per-group GOF", {
  d   <- reg_split_data()
  # a models list keeps the STACKED shape, where each group is a row block with its own footer
  t   <- tab_reg(d, "y", list(m1 = "x1", m2 = "x1"), family = "binomial", tab_vars = "g")
  tst <- tabxplor:::get_test(t)
  # Phase 19g: the split level rides a column NAMED after the split variable, like a crosstab's tab_var
  expect_setequal(unique(tst$g), c("north", "south"))   # tagged per split group
  expect_true(all(c("n", "lr_null") %in% tst$test))
})


test_that("split_var works with survey weights (per-group svyglm)", {
  d <- reg_split_data()
  t <- tab_reg(d, "y", list(m1 = c("x1", "x2"), m2 = c("x1", "x2")), family = "binomial",
               wt = "w", tab_vars = "g", multiplier = 1, ref = c(x2 = 0))
  expect_s3_class(t, "tabxplor_grouped_tab")
  sub  <- dplyr::filter(d, g == "north")
  des  <- survey::svydesign(ids = ~1, weights = ~w,
                            data = dplyr::mutate(sub, y01 = as.integer(y == levels(y)[1])))
  hand <- survey::svyglm(y01 ~ x1 + x2, design = des, family = quasibinomial())
  tt   <- dplyr::ungroup(t)
  tv   <- vapply(tt[["m1"]][tt$g == "north"], tabxplor::get_num, numeric(1))
  expect_equal(unname(tv[tv != 1]), unname(exp(stats::coef(hand))), tolerance = 1e-5)
})


test_that("split_var rejects an invalid grouping column", {
  d <- reg_split_data()
  expect_error(tab_reg(d, "y", "x1", tab_vars = "x1"), "cannot also be")   # a predictor
  # since 22b-vi the role is tidy-selected, so an absent column is tidyselect's own refusal
  expect_error(tab_reg(d, "y", "x1", tab_vars = "nope"), "doesn't exist")
  expect_error(tab_reg(d, "y", "x1", tab_vars = "x2"), "factor or character")
})


test_that("multiplier rejects non-numeric predictors / wrong families", {
  d <- reg_split_data()
  expect_error(suppressWarnings(tab_reg(d, "y", c("x1", "x2"), multiplier = c(x1 = 2))),
               "numeric predictor")
})


test_that("empirical crude OR matches the weighted 2x2 odds ratio", {
  d   <- reg_split_data()
  t   <- suppressWarnings(tab_reg(d, "y", "x1", empirical = TRUE))
  expect_true("Obs_OR" %in% names(t))
  eo  <- vapply(dplyr::ungroup(t)[["Obs_OR"]], tabxplor::get_num, numeric(1))
  # hand crude OR of each x1 level vs the reference "a", positive outcome = first level of y
  pos <- levels(d$y)[1]; lv <- levels(d$x1); ref <- lv[1]
  hand <- vapply(lv, function(l) {
    a <- sum(d$x1 == l & d$y == pos); b <- sum(d$x1 == l & d$y != pos)
    c <- sum(d$x1 == ref & d$y == pos); e <- sum(d$x1 == ref & d$y != pos)
    (a / b) / (c / e)
  }, numeric(1))
  eo_fac <- eo[!is.na(eo)]                          # drop the Constant NA
  expect_equal(unname(eo_fac), unname(hand), tolerance = 1e-8)
})


test_that("empirical: gaussian now produces crude columns (Phase 14v)", {
  d <- reg_split_data()
  # Phase 14v: gaussian empirical is now wired (crude mean + mean-difference), no longer ignored.
  tg <- tab_reg(d, "x2", "x1", family = "gaussian", empirical = TRUE)
  expect_true("Obs_diff" %in% names(tg))
  expect_true(any(is.finite(get_mean(tg[["Obs_diff"]]))))     # the crude mean rides in the same cell
})


# --- 22b-xiii-2 (C1 / G7): ONE reference distribution per table, taken from the fit ---------------

test_that("every column of a fit refers to the SAME distribution, and stores which", {
  # A table used to refer three ways at once: the coefficient column to t(df.residual), the crude
  # column to t(degf(design)), and every marginal / baseline column to z. The df is now decided once
  # per fit and read back by everything it produces -- and stamped, so the gap SE can recover an SE
  # with the very critical value that built the interval.
  d   <- reg_survey_data()
  des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d, nest = TRUE)
  d01 <- dplyr::mutate(d, y01 = as.integer(y == levels(y)[1]))
  hand <- survey::svyglm(y01 ~ x1 + x2,
                         design = survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w,
                                                    data = d01, nest = TRUE),
                         family = quasibinomial())
  dfr <- as.double(stats::df.residual(hand))
  expect_lt(dfr, as.double(survey::degf(des)))          # non-vacuous: the two really differ

  degf_of <- function(t, role) {
    cols <- names(t)[vapply(t, function(x) is_fmt(x) && identical(get_role(x), role), logical(1))]
    unique(vapply(cols, function(n) get_degf(t[[n]]), numeric(1), USE.NAMES = FALSE))
  }
  for (eff in c("conditional", "marginal", "at_reference")) {
    if (eff != "conditional") skip_if_not_installed("marginaleffects")
    t <- suppressMessages(suppressWarnings(
      tab_reg(des, "y", c("x1", "x2"), effect = eff, empirical = "column")))
    # EVERY column of the model's own fit -- the estimate, the marginal sweep, the baseline row.
    expect_identical(degf_of(t, "model"), dfr, info = eff)
    # ⚠ the crude column is a DIFFERENT fit (one predictor, hence fewer parameters), so it refers to
    # more df -- the weakest of its own univariable fits. One reference per FIT, not per table.
    expect_length(degf_of(t, "emp"), 1L)
    expect_gt(degf_of(t, "emp"), dfr)
  }
  # and the interval really is est +/- qt(df) * se: recover the SE from the coefficient column and
  # compare with the fit's own.
  t  <- suppressMessages(tab_reg(des, "y", c("x1", "x2"), multiplier = 1, ref = c(x2 = 0)))
  mc <- t[[grep("^Model_", names(t), value = TRUE)[[1]]]]
  k  <- which(is.finite(get_ci_inf(mc)) & as.character(t$var) != "Constant")
  se <- (log(get_ci_sup(mc)[k]) - log(get_ci_inf(mc)[k])) / (2 * stats::qt(.975, dfr))
  expect_equal(sort(se), sort(unname(sqrt(diag(stats::vcov(hand)))[-1])), tolerance = 1e-8)
  # the design's own df is a TABLE fact now, and it is what the "Model:" footer names.
  expect_identical(reg_call(t)$design_degf, as.double(survey::degf(des)))
  expect_match(tabxplor:::reg_model_lines(t)[[1]],
               paste0("t\\(", dfr, "\\) on ", survey::degf(des), " design df"))
})


test_that("a family that FIXES its dispersion stays on z, weighted or not", {
  d <- reg_survey_data()
  t <- suppressMessages(tab_reg(d, "y", c("x1", "x2")))          # unweighted binomial
  mc <- t[[grep("^Model_", names(t), value = TRUE)[[1]]]]
  expect_identical(get_degf(mc), Inf)                            # NA stamp -> refer to z
  g <- suppressWarnings(suppressMessages(tab_reg(d, "x2", "x1", family = "gaussian")))
  gc <- g[[grep("^Model_", names(g), value = TRUE)[[1]]]]        # lm: dispersion ESTIMATED -> t
  expect_identical(get_degf(gc), as.double(stats::df.residual(stats::lm(x2 ~ x1, data = d))))
})


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


test_that("D2 a marginal effect under a design is the POPULATION-average one", {
  skip_if_not_installed("marginaleffects")
  b   <- svy_fixture()
  des <- survey::svydesign(~psu, weights = ~w, data = b)
  ame <- function(t) {
    col <- t[[grep("^Model_mRD", names(t))[1]]]
    unname(get_diff(col)[which(as.character(t$levels) == "high")])
  }
  a_des <- ame(suppressMessages(
    tab_reg(des, outcome = "y", predictors = c("x", "z"), family = "binomial", effect = "marginal", measure = "difference")))
  a_wt  <- ame(tab_reg(b, outcome = "y", predictors = c("x", "z"), family = "binomial",
                       effect = "marginal", measure = "difference", wt = "w"))
  a_un  <- ame(tab_reg(b, outcome = "y", predictors = c("x", "z"), family = "binomial",
                       effect = "marginal", measure = "difference"))
  expect_equal(a_des, a_wt, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(a_des, a_un, tolerance = 1e-3)))
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
  # z16-iiiii: the reason travels OUT with the grid it describes -- that return value is what let the
  # process-global degrade environment go, and it is what reg_build() stamps as "design_partial".
  expect_true(isTRUE(attr(g, "degrade")))
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


test_that("a prebuilt survey design passed as `data` equals the hand svyglm", {
  d    <- reg_survey_data()
  d01  <- dplyr::mutate(d, y01 = as.integer(y == 1))
  des  <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d01, nest = TRUE)
  hand <- survey::svyglm(y01 ~ x1 + x2, design = des, family = quasibinomial())

  tab <- tab_reg(des, "y01", c("x1", "x2"), multiplier = 1, ref = c(x2 = 0))
  tv  <- or_col(tab)
  expect_equal(unname(tv[tv != 1]), unname(exp(stats::coef(hand))), tolerance = 1e-6)
})


test_that("weighted footer is the reduced survey set (n / wald_null / nagelkerke_r2 / aic)", {
  d   <- reg_survey_data()
  tab <- tab_reg(d, "y", c("x1", "x2"), wt = "w")
  tst <- tabxplor:::get_test(tab)
  # z13's overall-association rows and z15's model-check rows are in every default set; this asserts
  # the model-FIT statistics only.
  expect_setequal(setdiff(unique(tst$test),
                          c(tabxplor:::reg_global_types(), tabxplor:::reg_check_types())),
                  c("n", "wald_null", "nagelkerke_r2", "aic"))
  # no naive glm stats leak in under weights (`phi` is the exact Pearson dispersion, z15)
  expect_false(any(c("lr_null", "mcfadden_r2", "bic", "phi", "r2") %in% tst$test))
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


# --- Phase 12g-iii: split_var (stacked grouped subtables + tab_spread) ------------------------------
reg_split_data <- function() {
  set.seed(11); n <- 1500L
  g  <- factor(sample(c("north", "south"), n, replace = TRUE))
  x1 <- factor(sample(c("a", "b", "c"), n, replace = TRUE)); x2 <- rnorm(n)
  y  <- rbinom(n, 1, plogis(-0.2 + 0.5 * (x1 == "b") + 0.3 * x2 + 0.4 * (g == "south")))
  tibble::tibble(y = factor(y), g = g, x1 = x1, x2 = x2, w = runif(n, 0.5, 3))
}


# Phase 19h (KEY 7): `spread_models` is gone from the user surface. The groups go side by side
# whenever that is unambiguous -- ONE column per group -- and stay stacked otherwise, which is what a
# models list produces (one column per model, so a side-by-side layout has no single column to key on).
test_that("split_var stacks one model per group (grouped by split_var + var)", {
  d <- reg_split_data()
  t <- tab_reg(d, "y", list(m1 = "x1", m2 = c("x1", "x2")), family = "binomial", tab_vars = "g")
  expect_s3_class(t, "tabxplor_grouped_tab")
  expect_setequal(dplyr::group_vars(t), c("g", "var"))
  expect_true("g" %in% names(t))
  expect_setequal(levels(dplyr::pull(dplyr::ungroup(t), g)), c("north", "south"))
})


test_that("Phase g: split_var + a single model auto-spreads to side-by-side columns", {
  d <- reg_split_data()
  # default spread_models = TRUE: the sub-models sit side by side (no stacked `g` row-column)
  t <- tab_reg(d, "y", c("x1", "x2"), tab_vars = "g")
  expect_false("g" %in% names(t))
  # Phase 19n: each split level's column carries its sub-population in `col_group`, BESIDE the
  # outcome its `col_var` names -- the pair is the block identity, and it is what gives the export a
  # two-line span and a border between the blocks. Until 19n the two were welded into `col_var` as
  # "{level}<br>{outcome}", so the backends recovered them by sniffing for an html tag.
  fc <- names(t)[vapply(t, is_fmt, logical(1))]
  cv <- vapply(fc, function(nm) tabxplor:::get_col_var(t[[nm]]), character(1))
  cg <- vapply(fc, function(nm) get_col_group(t[[nm]]), character(1))
  expect_false(any(grepl("<br>", cv)))          # the weld is gone from the stored name
  expect_true(all(nzchar(cg)))
  expect_setequal(unique(cg), c("north", "south"))
  # works with a crude COLUMN (they spread too, level-suffixed). ⚠ `empirical = "column"`, not
  # TRUE: since 22g-ii `tab_vars` resolve TRUE to "tooltip" -- computed, no column -- so drawing one
  # is what has to be asked for here.
  te <- suppressWarnings(tab_reg(d, "y", "x1", tab_vars = "g", empirical = "column"))
  expect_true(any(grepl("^Obs_", names(te))))
  # several models per group cannot go side by side, so they stay stacked
  expect_true("g" %in% names(
    tab_reg(d, "y", list(m1 = "x1", m2 = c("x1", "x2")), family = "binomial", tab_vars = "g")))
})


# --- Phase 12g-iv: multiplier + empirical ----------------------------------------------------
test_that("multiplier scales a continuous predictor's OR to OR^k, p unchanged", {
  d <- reg_split_data()
  t0  <- suppressWarnings(tab_reg(d, "y", c("x1", "x2"), multiplier = 1))
  t10 <- suppressWarnings(tab_reg(d, "y", c("x1", "x2"), multiplier = c(x2 = 10)))
  oc  <- grep("^Model_", names(t0), value = TRUE)[1]
  or0  <- vapply(t0[[oc]],  tabxplor::get_num, numeric(1))
  or10 <- vapply(t10[[oc]], tabxplor::get_num, numeric(1))
  # last row = x2; other rows (Constant, x1 levels) unchanged
  expect_equal(or10[length(or10)], or0[length(or0)]^10, tolerance = 1e-8)
  expect_equal(or10[-length(or10)], or0[-length(or0)], tolerance = 1e-10)
  # stars (pvalue) unchanged by rescaling
  expect_equal(tabxplor:::get_pvalue(t0[[oc]]), tabxplor:::get_pvalue(t10[[oc]]))
})
