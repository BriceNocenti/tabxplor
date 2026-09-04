
# === SECTION: design-based cell variance ==========================================================

skip_if_not_installed("survey")


# A design whose weights correlate with the outcome AND whose PSUs carry a real cluster effect, so
# design != weighted != unweighted by a wide margin (S3.2's "segregated" shape is what stars use).
svv_fixture <- function(n = 6000, seed = 3) {
  set.seed(seed)
  d <- data.frame(strat = factor(rep(1:4, each = n / 4)),
                  psu   = factor(rep(seq_len(n / 25), each = 25)))
  d$h <- stats::rnorm(n)
  d$w <- exp(0.7 * d$h); d$w <- d$w / mean(d$w)
  d$g <- factor(sample(c("a", "b", "c"), n, TRUE), levels = c("a", "b", "c"))
  d$col <- factor(ifelse(stats::rbinom(
    n, 1, stats::plogis(-.2 + .9 * (d$g == "b") + .8 * d$h)) == 1, "yes", "no"),
    levels = c("no", "yes"))
  d$x <- round(stats::rnorm(n, 50, 10) + 6 * d$h, 3)
  d
}


svv_des <- function(d) suppressMessages(
  survey::svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE))


svv_cell <- function(tab, col, getter, level) unname(
  getter(tab[[col]])[which(as.character(tab$levels) == level)])


test_that("the col% and all% bases are the transposed / whole-table domains survey computes", {
  d <- svv_fixture(); des <- svv_des(d)
  RL <- levels(d$g); CL <- levels(d$col)
  prep <- tabxplor:::svy_var_prep(des, seq_len(nrow(d)))
  keys <- list(g = RL); mkeys <- list(g = tabxplor:::svy_key_chr(d$g))
  mcol <- tabxplor:::svy_key_chr(d$col)

  Vc  <- tabxplor:::svy_var_prop(prep, keys, 0L, mkeys, mcol, CL, "col")$v
  byc <- survey::svyby(~g, ~col, des, survey::svymean, covmat = TRUE)
  expect_equal(sqrt(Vc[2, 2]), unname(survey::SE(byc)[2, 2]), tolerance = 1e-8)

  Va <- tabxplor:::svy_var_prop(prep, keys, 0L, mkeys, mcol, CL, "all")$v
  dc <- des; dc$variables$cell <- interaction(d$g, d$col, sep = "|")
  ba <- survey::svymean(~cell, dc)
  k  <- which(names(stats::coef(ba)) == paste0("cell", RL[2], "|", CL[2]))
  expect_equal(sqrt(Va[2, 2]), unname(survey::SE(ba)[[k]]), tolerance = 1e-8)
})


# ---- end to end: the four consumers of n_eff ----------------------------------------------------

test_that("a cell interval under a design is survey's, and is NOT the single-stage one", {
  d <- svv_fixture(); des <- svv_des(d)
  tt <- suppressMessages(tab(des, g, col, pct = "row", ci = "cell"))
  cl <- tt[["yes"]]
  for (i in seq_along(levels(d$g))) {
    lv  <- levels(d$g)[i]
    ref <- stats::confint(survey::svymean(~col, subset(des, g == lv)))["colyes", ]
    expect_equal(get_ci_inf(cl)[i], unname(ref[1]), tolerance = 1e-3)
    expect_equal(get_ci_sup(cl)[i], unname(ref[2]), tolerance = 1e-3)
  }
  # non-vacuity: the same table without the design is measurably narrower
  raw <- tab(d, g, col, wt = w, pct = "row", ci = "cell")[["yes"]]
  dat <- !is_totrow(cl)                      # a total row never carries a cell CI (pre-existing)
  expect_true(all(get_ci_sup(cl)[dat] - get_ci_inf(cl)[dat] >
                    get_ci_sup(raw)[dat] - get_ci_inf(raw)[dat]))
  expect_true(all(is.na(get_n_eff(raw))))
  expect_true(all(is.finite(get_n_eff(cl))))
})


# Phase 18z16-iiiii (S8.2 item 8). FEW PSUs is the whole point: beta quantiles carry no degrees of
# freedom of their own, so Korn-Graubard rescales the effective base by (qt(a, n-1) / qt(a, degf))^2 --
# worth exactly 1 at the flat basis (which is why test-flat-design-parity.R #13 could never see it)
# and 0.645 here, i.e. an interval that was 25 % too short.
svv_kg_fixture <- function(n = 800, seed = 11) {
  set.seed(seed)
  d <- data.frame(psu   = factor(rep(1:8, each = n / 8)),
                  strat = factor(rep(1:2, each = n / 2)))
  d$w   <- stats::runif(n, 0.5, 3)
  # CROSSED with the PSUs, so degf(subset) == degf(design) and survey is an exact oracle here
  d$g   <- factor(sample(c("A", "B"), n, TRUE))
  d$col <- factor(ifelse(stats::rbinom(n, 1, 0.3) == 1, "yes", "no"), levels = c("no", "yes"))
  d
}


test_that("the cell-vs-reference difference and its stars use the design base", {
  d <- svv_fixture(); des <- svv_des(d)
  dsg <- suppressMessages(tab(des, g, col, pct = "row", ci = "ref", ref = 1, stars = TRUE))
  raw <- tab(d, g, col, wt = w, pct = "row", ci = "ref", ref = 1, stars = TRUE)
  wd  <- function(t) { cl <- t[["yes"]]; (get_ci_sup(cl) - get_ci_inf(cl))[2:3] }
  expect_true(all(wd(dsg) > wd(raw)))            # a clustered design widens the difference
  # the two bases the difference combines are both the design's
  expect_true(all(is.finite(get_n_eff(dsg[["yes"]]))))
})


test_that("the color = 'OR' interval rides the design base too", {
  d <- svv_fixture(); des <- svv_des(d)
  o_d <- suppressMessages(tab(des, g, col, pct = "row", display = "{or}", ref = 1,
                              color = "OR", color_signif = "grey_non_signif"))[["yes"]]
  o_r <- tab(d, g, col, wt = w, pct = "row", display = "{or}", ref = 1,
             color = "OR", color_signif = "grey_non_signif")[["yes"]]
  dw <- log(get_ci_sup(o_d)) - log(get_ci_inf(o_d))
  rw <- log(get_ci_sup(o_r)) - log(get_ci_inf(o_r))
  expect_true(all(dw[2:3] > rw[2:3]))
  # and it lands near the design-based saturated model, which the raw base does not
  d2 <- des; d2$variables$.pos <- as.integer(d$col == "yes")
  fit <- suppressWarnings(survey::svyglm(.pos ~ g, design = d2, family = stats::quasibinomial()))
  ci  <- suppressMessages(stats::confint(fit))
  tw  <- (ci[2:3, 2] - ci[2:3, 1])
  expect_true(all(abs(dw[2:3] / tw - 1) < abs(rw[2:3] / tw - 1)))
})


test_that("the contrib residual of a FLAT design differs from a clustered one's (W-B)", {
  # The defect this closes: both used to take the degenerate B^2/S, so a stratified + clustered table
  # and a flat one gave residuals identical to the last digit while their CELL intervals differed --
  # one table reporting design-corrected intervals and weights-only significance, side by side.
  d <- svv_fixture()
  flat <- suppressMessages(tab(survey::svydesign(ids = ~1, weights = ~w, data = d),
                               g, col, pct = "row", color = "contrib", test = TRUE))
  clus <- suppressMessages(tab(svv_des(d), g, col, pct = "row", color = "contrib", test = TRUE))
  zf <- fmt_resid(flat[["yes"]]); zc <- fmt_resid(clus[["yes"]])
  k  <- is.finite(zf) & is.finite(zc)
  expect_true(any(k))
  expect_false(isTRUE(all.equal(zf[k], zc[k])))
  # the two design effects are the two the tests report, and nothing else
  expect_equal(abs(zc[k] / zf[k]),
               rep(sqrt(get_test(flat)$deff[[1]] / get_test(clus)$deff[[1]]), sum(k)),
               tolerance = 1e-6)
})


test_that("total rows get a design base of their own", {
  # A total row is a DOMAIN like any other ("Total" in a key = every level), so it needs no special
  # case -- and its base is load-bearing: it is what ref = "tot" compares every other row against.
  # (It carries no cell CI of its own, which is pre-2.0.0 behaviour and unrelated.)
  d <- svv_fixture(); des <- svv_des(d)
  cl <- suppressMessages(tab(des, g, col, pct = "row", ci = "cell"))[["yes"]]
  tr <- which(is_totrow(cl))
  expect_length(tr, 1L)
  m  <- survey::svymean(~col, des)
  p  <- unname(stats::coef(m)[["colyes"]])
  expect_equal(get_n_eff(cl)[tr], p * (1 - p) / unname(survey::SE(m)[["colyes"]])^2,
               tolerance = 1e-6)
  expect_true(get_n_eff(cl)[tr] < get_tot_n(cl)[tr])   # clustering costs the total row too
})


test_that("a mean table's interval is design-based, matching svyby's standard error", {
  d <- svv_fixture(); des <- svv_des(d)
  tn <- suppressMessages(tab_num(des, g, x, ci = "cell"))
  se_ref <- unname(survey::SE(survey::svyby(~x, ~g, des, survey::svymean)))
  rows   <- which(!is_totrow(tn$x))
  se_tab <- sqrt(get_var(tn$x)[rows] / get_n_eff(tn$x)[rows])
  expect_equal(se_tab, se_ref, tolerance = 1e-8)
  # non-vacuity: the single-stage base is a different number
  raw <- tab_num(d, g, x, wt = w, ci = "cell")
  expect_true(all(is.na(get_n_eff(raw$x))))
  expect_true(all(get_n_eff(tn$x)[rows] != get_n(tn$x)[rows]))
})


# ---- the design's precision GAINS, which Kish provably cannot carry ------------------------------

test_that("a stratified equal-weight design NARROWS the interval (n_eff > n), where Kish cannot", {
  set.seed(11); m <- 4000
  q <- data.frame(st = factor(rep(1:8, each = m / 8)), w = 1)
  q$y <- factor(ifelse(stats::rbinom(
    m, 1, rep(seq(.15, .85, length.out = 8), each = m / 8)) == 1, "yes", "no"),
    levels = c("no", "yes"))
  q$g <- factor("all")
  des <- survey::svydesign(~1, strata = ~st, weights = ~w, data = q)

  dsg <- suppressMessages(tab(des, g, y, pct = "row", ci = "cell"))[["yes"]]
  kis <- withr::with_options(list(tabxplor.design_effect = TRUE),
                             tab(q, g, y, wt = w, pct = "row", ci = "cell"))[["yes"]]
  i <- which(!is_totrow(dsg))[1]
  expect_gt(get_n_eff(dsg)[i], get_tot_n(dsg)[i])                   # the design GAINED precision
  # equal weights carry no design effect, so the closed form returns the sample size itself -- up to
  # survey's own finite-sample factor n/(n-1), which svyglm at ids = ~1 also applies (S8.1).
  expect_equal(get_n_eff(kis)[i], get_tot_n(kis)[i] - 1)
  expect_lt(get_ci_sup(dsg)[i] - get_ci_inf(dsg)[i],
            get_ci_sup(kis)[i] - get_ci_inf(kis)[i])
})


test_that("tab_plain() and tab_num() on a design take the same path as tab()", {
  d <- svv_fixture(); des <- svv_des(d)
  p <- suppressMessages(tab_plain(des, g, col, pct = "row"))
  expect_true(all(is.finite(get_n_eff(p[["yes"]]))))
  n <- suppressMessages(tab_num(des, g, x, ci = "cell"))
  expect_true(all(is.finite(get_n_eff(n$x))))
})


test_that("the producers answer 'no value', never a wrong number, on inputs they cannot serve", {
  d <- svv_fixture(600); des <- svv_des(d)
  prep <- tabxplor:::svy_var_prep(des, seq_len(nrow(d)))
  RL <- levels(d$g); CL <- levels(d$col)
  mk <- list(g = tabxplor:::svy_key_chr(d$g)); mc <- tabxplor:::svy_key_chr(d$col)
  expect_null(tabxplor:::svy_var_prop(NULL, list(g = RL), 0L, mk, mc, CL, "row")$v)
  expect_null(tabxplor:::svy_var_prop(prep, list(g = RL), 0L, list(g = mk$g[-1]), mc, CL, "row")$v)
  expect_null(tabxplor:::svy_var_prop(prep, list(g = RL), 0L, mk, mc, CL, "nonsense")$v)
  expect_null(tabxplor:::svy_var_mean(prep, list(g = RL), 0L, mk, list(x = d$x[-1]))$v)
  # z16-iiiii: the answer carries its own REASON, which is what replaced the process-global flag.
  big <- tabxplor:::svy_var_prop(prep, list(g = rep(RL, 4e5)), 0L, mk, mc, CL, "row")
  expect_null(big$v)
  expect_identical(big$reason, "size")
  # and a lonely-PSU design (one cluster per stratum) still BUILDS -- the lonely.psu policy is the
  # overlay's, answered in one place (svy_var_recvar)
  d2 <- d; d2$psu <- factor(seq_len(nrow(d2)))
  des2 <- survey::svydesign(~psu, strata = ~strat, weights = ~w, data = d2)
  expect_s3_class(suppressMessages(suppressWarnings(
    tab(des2, g, col, pct = "row", ci = "cell"))), "tabxplor_tab")
})


test_that("the French design-based footer is translated", {
  skip_if_no_gettext()
  d <- svv_fixture(800); des <- svv_des(d)
  tt <- suppressMessages(tab(des, g, col, pct = "row"))
  fr <- tabxplor:::tab_weight_line(tt, lang = "fr")
  expect_true(grepl("plan d'\u00e9chantillonnage", fr, fixed = TRUE))
  expect_false(grepl("Design-based", fr, fixed = TRUE))
})


# === SECTION: the flat closed form equals survey ==================================================

skip_if_not_installed("survey")


fdp_fixture <- function(n = 4000, seed = 7) {
  set.seed(seed)
  d <- data.frame(grp = factor(sample(c("A", "B", "C", "D"), n, TRUE, prob = c(.4, .3, .2, .1))))
  d$w <- exp(stats::rnorm(n, 0, .55)) * c(A = .6, B = 1, C = 1.6, D = 2.4)[as.character(d$grp)]
  d$w <- d$w / mean(d$w)
  lin <- -0.3 + 0.5 * scale(log(d$w))[, 1] + c(A = -.4, B = 0, C = .3, D = .6)[as.character(d$grp)]
  d$col <- factor(ifelse(stats::rbinom(n, 1, stats::plogis(lin)) == 1, "yes", "no"),
                  levels = c("no", "yes"))
  d$x   <- round(stats::rnorm(n, 50, 12) + 6 * log(d$w), 4)
  d$sub   <- factor(rep(c("s1", "s2"), length.out = n))
  d$yes01 <- as.numeric(d$col == "yes")     # a numeric indicator: svymean returns ONE column for it
  d
}

fdp_des <- function(d) survey::svydesign(ids = ~1, weights = ~w, data = as.data.frame(d))

fdp_on  <- function(expr) withr::with_options(list(tabxplor.design_effect = TRUE), expr)

# the design variance our n_eff IMPLIES, cell by cell -- the quantity survey is the oracle for
fdp_var_prop <- function(col) { p <- get_pct(col);  p * (1 - p) / get_n_eff(col) }

fdp_var_mean <- function(col) get_var(col) / get_n_eff(col)

fdp_se       <- function(o) as.numeric(unlist(survey::SE(o)))


test_that("1. cell % == svyby(svymean) variance, for pct = row / col / all", {
  d <- fdp_fixture(); des <- fdp_des(d)
  row <- fdp_on(tab(d, grp, col, wt = w, pct = "row", ci = "cell"))
  expect_equal(fdp_var_prop(row[["yes"]])[1:4],
               fdp_se(survey::svyby(~yes01, ~grp, des, survey::svymean))^2,
               tolerance = 1e-9, ignore_attr = TRUE)

  cl <- fdp_on(tab(d, grp, col, wt = w, pct = "col", ci = "cell"))
  expect_equal(fdp_var_prop(cl[["yes"]])[1:4],
               fdp_se(survey::svyby(~grp, ~col, des, survey::svymean))[c(2, 4, 6, 8)]^2,
               tolerance = 1e-9, ignore_attr = TRUE)

  al <- fdp_on(tab(d, grp, col, wt = w, pct = "all", ci = "cell"))
  expect_equal(fdp_var_prop(al[["yes"]])[1:4],
               as.numeric(survey::SE(survey::svymean(~interaction(grp, col, drop = TRUE), des)))[5:8]^2,
               tolerance = 1e-9, ignore_attr = TRUE)
})


test_that("2. ... on a TOTAL row, a total-TABLE row, and inside a tab_vars subtable", {
  d <- fdp_fixture(); des <- fdp_des(d)
  row <- fdp_on(tab(d, grp, col, wt = w, pct = "row", ci = "cell"))
  # the total row's domain is "every level", so it is a plain svymean
  expect_equal(fdp_var_prop(row[["yes"]])[5],
               as.numeric(survey::SE(survey::svymean(~yes01, des)))^2,
               tolerance = 1e-9)

  sub <- fdp_on(tab(d, grp, col, sub, wt = w, pct = "row", ci = "cell", totaltab = "line"))
  yes <- sub[["yes"]]
  # the first data row of subtable s1: a domain of the SAME flat design
  d1  <- subset(des, grp == "A" & sub == "s1")
  expect_equal(fdp_var_prop(yes)[1],
               as.numeric(survey::SE(survey::svymean(~yes01, d1)))^2, tolerance = 1e-9)
  # and the total-TABLE row (all keys "Total") is the whole sample again
  last <- length(yes)
  expect_equal(fdp_var_prop(yes)[last],
               as.numeric(survey::SE(survey::svymean(~yes01, des)))^2, tolerance = 1e-9)
})


test_that("3. cell MEAN == svyby(svymean) variance, data rows and total row", {
  d <- fdp_fixture(); des <- fdp_des(d)
  m <- fdp_on(tab(d, grp, x, wt = w, ci = "cell"))
  expect_equal(fdp_var_mean(m$x)[1:4],
               fdp_se(survey::svyby(~x, ~grp, des, survey::svymean))^2,
               tolerance = 1e-9, ignore_attr = TRUE)
  expect_equal(fdp_var_mean(m$x)[5],
               as.numeric(survey::SE(survey::svymean(~x, des)))^2, tolerance = 1e-9)
})


test_that("7. tab_reg(empirical=)'s Obs_OR bracket == the univariable svyglm SE(log OR)", {
  d <- fdp_fixture(3000, seed = 5); des <- fdp_des(d)
  tt <- tab_reg(d, "col", "grp", family = "binomial", empirical = TRUE, wt = "w")
  oc <- tt[["Obs_OR"]]
  se_ours <- (log(get_ci_sup(oc)) - log(get_ci_inf(oc))) / (2 * conf_level_to_z(0.95, digits = Inf))
  fit <- suppressWarnings(survey::svyglm(I(col == "yes") ~ grp, design = des,
                                         family = stats::quasibinomial()))
  se_svy <- summary(fit)$coefficients[-1, "Std. Error"]
  k <- which(is.finite(se_ours))
  expect_equal(unname(se_ours[k]), unname(se_svy), tolerance = 1e-6)
})


test_that("8. a mean-DIFFERENCE bracket is exactly the two domain design variances (S8.5)", {
  d <- fdp_fixture(3000, seed = 5); des <- fdp_des(d)
  # at the flat design two DISJOINT domains share no cluster, so their estimates are INDEPENDENT and
  # Var(diff) = Var1 + Var2 exactly -- the covariance Route A discards is zero here. Welch's SE is
  # sqrt(v1/n1_eff + v2/n2_eff), i.e. that sum, so the identity is exact rather than approximate.
  tt <- fdp_on(tab(d, grp, x, wt = w, ci = "ref", ref = "first"))$x
  v  <- fdp_se(survey::svyby(~x, ~grp, des, survey::svymean))^2
  half <- (get_ci_sup(tt) - get_ci_inf(tt)) / 2
  k    <- which(is.finite(half) & !is_totrow(tt))     # the total row's own reference is elsewhere
  se_svy <- sqrt(v[k] + v[1])                         # each non-reference level against level A
  # Welch's t at these effective n is within a whisker of z; the SE itself must be exact, so divide
  # the bracket by the quantile the engine used rather than asserting the quantile.
  expect_equal(unname(half[k] / se_svy), rep(conf_level_to_z(0.95, digits = Inf), length(k)),
               tolerance = 0.01)
  # ... and the crude Obs_diff column of tab_reg() is the same quantity with a POOLED (Student) SE,
  # which REG_EMPIRICAL picks so it matches the model AME beside it -- close, deliberately not equal.
  dc <- tab_reg(d, "x", "grp", family = "gaussian", empirical = TRUE, wt = "w")[["Obs_diff"]]
  hr <- (get_ci_sup(dc) - get_ci_inf(dc)) / 2
  expect_true(all(abs(hr[is.finite(hr)] / (se_svy * conf_level_to_z(0.95, digits = Inf)) - 1) < 0.07))
})


test_that("11. the RELATIVE contribution `ctr` is identical at every basis (S2.6)", {
  d <- fdp_fixture(1500, seed = 9); des <- fdp_des(d)
  ctr <- function(t) get_ctr(t[[2]])
  a <- tab(d, grp, col, wt = w, color = "contrib")
  b <- fdp_on(tab(d, grp, col, wt = w, color = "contrib"))
  cc <- suppressMessages(tab(des, grp, col, color = "contrib"))
  # an estimate describes the population: it is weighted at every position, and NEVER design-corrected
  expect_equal(ctr(a), ctr(b))
  expect_equal(ctr(a), ctr(cc))
})


test_that("6. the weighted omnibus F IS svyglm + regTermTest on the flat design", {
  d <- fdp_fixture(2000, seed = 17); des <- fdp_des(d)
  te  <- fdp_on(get_test(tab(d, grp, x, wt = w, test = TRUE)))
  fit <- survey::svyglm(x ~ grp, design = des)
  rt  <- survey::regTermTest(fit, ~grp, method = "Wald")
  expect_equal(te$test[1], "F_design")
  expect_equal(te$statistic[1], as.double(rt$Ftest), tolerance = 1e-10)
  expect_equal(te$df1[1],       as.double(rt$df),    tolerance = 1e-10)
  expect_equal(te$pvalue[1],    as.double(rt$p),     tolerance = 1e-10)
})


test_that("12. the weights and the design basis run the SAME estimator (ruling 7)", {
  d <- fdp_fixture(2000, seed = 19); des <- fdp_des(d)
  a <- fdp_on(get_test(tab(d, grp, col, wt = w, test = TRUE)))
  b <- suppressMessages(get_test(tab(des, grp, col, test = TRUE)))
  # two ways in, one estimator -- which is exactly why there are two discriminators and not four
  expect_equal(a$test, b$test)
  expect_equal(a$statistic, b$statistic, tolerance = 1e-10)
  expect_equal(a$pvalue,    b$pvalue,    tolerance = 1e-10)
  expect_equal(a$deff,      b$deff,      tolerance = 1e-10)
})


test_that("13. ci_method = c(cell = 'beta') IS survey::svyciprop(method = 'beta')", {
  d <- fdp_fixture(1200, seed = 23); des <- fdp_des(d)
  tt <- fdp_on(tab(d, grp, col, wt = w, pct = "row", ci = "cell",
                   ci_method = c(cell = "beta")))[["yes"]]
  ref <- survey::svyciprop(~yes01, subset(des, grp == "A"), method = "beta")
  expect_equal(c(get_ci_inf(tt)[1], get_ci_sup(tt)[1]),
               as.numeric(attr(ref, "ci")), tolerance = 1e-6, ignore_attr = TRUE)
})


# === SECTION: the design-effect option ============================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


kish_data <- function(n = 500L, seed = 20260722L) {
  set.seed(seed)
  tibble::tibble(
    g = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
    y = factor(sample(c("yes", "no"), n, replace = TRUE)),
    x = stats::rnorm(n, 10, 3),
    w = stats::rgamma(n, shape = 0.3, rate = 0.3)      # heavy-tailed -> strong design effect
  )
}


ci_hw <- function(col) (get_ci_sup(col) - get_ci_inf(col)) / 2


testthat::test_that("factor proportion cell CI: n_eff carried + interval widens under kish", {
  d <- kish_data()
  off <- withr::with_options(list(tabxplor.design_effect = FALSE),
                             tab(d, g, y, wt = w, pct = "row", ci = "cell", na = "drop"))
  on  <- withr::with_options(list(tabxplor.design_effect = TRUE),
                             tab(d, g, y, wt = w, pct = "row", ci = "cell", na = "drop"))

  ne_off <- get_n_eff(off[["yes"]]); ne_on <- get_n_eff(on[["yes"]])
  testthat::expect_true(all(is.na(ne_off)))                       # off: field is NA (fallback to tot_n)
  fin <- is.finite(ne_on)
  testthat::expect_gt(sum(fin), 0L)                               # on: populated
  testthat::expect_true(all(ne_on[fin] < get_tot_n(on[["yes"]])[fin]))   # n_eff < n (design effect)

  hw_off <- ci_hw(off[["yes"]]); hw_on <- ci_hw(on[["yes"]])
  ok <- is.finite(hw_off) & is.finite(hw_on)
  testthat::expect_true(all(hw_on[ok] >= hw_off[ok] - 1e-9))      # never narrower
  testthat::expect_gt(sum(hw_on[ok] > hw_off[ok] + 1e-6), 0L)     # strictly wider somewhere
})


testthat::test_that("factor proportion diff CI widens under kish", {
  d <- kish_data()
  off <- withr::with_options(list(tabxplor.design_effect = FALSE),
                             tab(d, g, y, wt = w, pct = "row", ci = "ref", na = "drop"))
  on  <- withr::with_options(list(tabxplor.design_effect = TRUE),
                             tab(d, g, y, wt = w, pct = "row", ci = "ref", na = "drop"))
  hw_off <- ci_hw(off[["yes"]]); hw_on <- ci_hw(on[["yes"]])
  ok <- is.finite(hw_off) & is.finite(hw_on)
  testthat::expect_gt(sum(hw_on[ok] > hw_off[ok] + 1e-6), 0L)
})


testthat::test_that("colour-OR significance interval widens under kish", {
  d <- kish_data()
  off <- withr::with_options(list(tabxplor.design_effect = FALSE),
                             tab(d, g, y, wt = w, pct = "row", color = "OR",
                                 color_signif = "grey_non_signif", na = "drop"))
  on  <- withr::with_options(list(tabxplor.design_effect = TRUE),
                             tab(d, g, y, wt = w, pct = "row", color = "OR",
                                 color_signif = "grey_non_signif", na = "drop"))
  hw_off <- ci_hw(off[["yes"]]); hw_on <- ci_hw(on[["yes"]])
  ok <- is.finite(hw_off) & is.finite(hw_on)
  testthat::expect_gt(sum(ok), 0L)
  testthat::expect_gt(sum(hw_on[ok] > hw_off[ok] + 1e-6), 0L)
})


testthat::test_that("mean cell CI: n_eff surfaced + interval widens under kish", {
  d <- kish_data()
  off <- withr::with_options(list(tabxplor.design_effect = FALSE),
                             tab_num(d, g, x, wt = w, ci = "cell", na = "drop"))
  on  <- withr::with_options(list(tabxplor.design_effect = TRUE),
                             tab_num(d, g, x, wt = w, ci = "cell", na = "drop"))
  mcol <- names(on)[purrr::map_lgl(on, ~ is_fmt(.) && tabxplor:::fmt_var_kind(.) == "mean")][1]
  testthat::expect_true(all(is.na(get_n_eff(off[[mcol]]))))
  testthat::expect_gt(sum(is.finite(get_n_eff(on[[mcol]]))), 0L)
  hw_off <- ci_hw(off[[mcol]]); hw_on <- ci_hw(on[[mcol]])
  ok <- is.finite(hw_off) & is.finite(hw_on)
  testthat::expect_gt(sum(hw_on[ok] > hw_off[ok] + 1e-6), 0L)
})


testthat::test_that("counts-data (no per-obs weights) gracefully keeps raw n (n_eff NA)", {
  # tab_counts routes through the .fine path -> Sum(w^2) is unrecoverable -> n_eff stays NA.
  cnt <- tibble::tibble(
    g = factor(rep(c("a", "b"), each = 2)),
    y = factor(rep(c("yes", "no"), 2)),
    count  = c(30, 70, 55, 45),
    wcount = c(40, 60, 50, 50)
  )
  on <- withr::with_options(
    list(tabxplor.design_effect = TRUE),
    tab_counts(cnt, row_var = g, col_var = y, counts = count, wt_counts = wcount,
               pct = "row", ci = "cell")
  )
  ycol <- on[["yes"]]
  testthat::expect_true(all(is.na(get_n_eff(ycol))))             # no correction possible -> NA
})


# === SECTION: effect sizes and the omnibus tests ==================================================

gss <- fx_gss()


# ---- Cramer's V / phi (factor) --------------------------------------------------------------------

test_that("Cramer's V matches the uncorrected chi2 formula (and DescTools)", {
  t  <- tab(gss, marital, race, pct = "row", test = TRUE)
  te <- get_test(t)
  v  <- te$effect_size[te$test == "chi2"]
  expect_equal(te$es_type[te$test == "chi2"], "cramer_v")
  expect_true(is.finite(v) && v > 0)

  # manual: sqrt(X2_uncorrected / (N * (min(r,c) - 1))) on the same (empty-margin-dropped) table
  m  <- table(gss$marital, gss$race)
  m  <- m[rowSums(m) > 0, colSums(m) > 0]
  x2 <- unname(suppressWarnings(stats::chisq.test(m, correct = FALSE)$statistic))
  expect_equal(unname(v), sqrt(x2 / (sum(m) * (min(dim(m)) - 1))), tolerance = 1e-6)

  skip_if_not_installed("DescTools")
  expect_equal(unname(v), DescTools::CramerV(m, correct = FALSE), tolerance = 1e-6)
})


test_that("a 2x2 table reports phi", {
  d  <- gss[gss$marital %in% c("Married", "Divorced") & gss$race %in% c("White", "Black"), ]
  d$marital <- droplevels(d$marital); d$race <- droplevels(d$race)
  te <- get_test(tab(d, marital, race, pct = "row", test = TRUE))
  expect_equal(te$es_type[te$test == "chi2"], "phi")
  expect_true(te$effect_size[te$test == "chi2"] > 0)
})


# ---- Fisher's exact (auto on small weak tables) ---------------------------------------------------

test_that("a small sparse table gets an exact Fisher p (matching fisher.test)", {
  # a deliberately tiny, sparse 3x2 table -> min_e < 5 and N small enough for the exact test
  set.seed(1)  # only affects nothing here; kept for clarity
  d <- data.frame(
    g = factor(rep(c("a", "b", "c"), c(6, 6, 6))),
    y = factor(c("no","no","no","no","no","yes", "no","no","yes","yes","yes","yes",
                 "yes","yes","yes","yes","yes","no")))
  te <- get_test(tab(d, g, y, pct = "row", test = TRUE))
  pe <- te$pvalue_exact[te$test == "chi2"]
  expect_true(is.finite(pe))
  m  <- table(d$g, d$y)
  expect_equal(pe, stats::fisher.test(m)$p.value, tolerance = 1e-8)
})


test_that("a large table's chi2 is NOT overridden by a simulated Fisher p (pvalue_exact stays NA)", {
  # marital x race is weak (one rare category) but N huge -> exact infeasible -> keep the chi2
  te <- get_test(tab(gss, marital, race, pct = "row", test = TRUE))
  expect_true(is.na(te$pvalue_exact[te$test == "chi2"]))
})


# ---- Kish n_eff (first-order Rao-Scott), opt-in ---------------------------------------------------

test_that("Kish factor chi2 rescales the weighted chi2 to n_eff", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  withr::local_options(tabxplor.design_effect = TRUE)
  te <- get_test(tab(apistrat, stype, awards, wt = pw, test = TRUE))
  expect_equal(te$test[1], "chi2_design")
  expect_true(is.finite(te$pvalue) && te$pvalue > 0)
  # Phase 18z16-i (W8): `n` is ALWAYS the raw count -- it used to become the effective n here, so
  # one column meant two things depending on a global option. The correction now lives in `deff`.
  expect_equal(te$n[1], nrow(apistrat))
  # `deff` is Rao-Scott's mean generalized design effect: >1 when the weighting costs information,
  # <1 when it buys some (apistrat is a stratified sample, so its weights can). Finite is the claim.
  expect_true(is.finite(te$deff[1]) && te$deff[1] > 0)
})


test_that("survey numeric F matches svyglm + regTermTest", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  des <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  te  <- suppressMessages(get_test(tab(des, stype, api00, test = TRUE)))
  fit <- survey::svyglm(api00 ~ stype, des)
  ref <- survey::regTermTest(fit, ~stype, method = "Wald")
  expect_equal(te$test[1], "F_design")
  expect_equal(te$pvalue[1], as.double(ref$p), tolerance = 1e-6)
})


test_that("the classic default path is unaffected (no robust columns, effect size present)", {
  te <- get_test(tab(gss, marital, race, pct = "row", test = TRUE))
  expect_true(all(te$test %in% c("chi2", "F_welch", "F_classic")))
  expect_true(is.finite(te$effect_size[te$test == "chi2"]))
})


# === Phase 18z16-iv: the robust omnibus GRID (producer / joiner split) =========================

test_that("a design table with tab_vars keeps its TOTAL-TABLE test row", {
  skip_if_not_installed("survey")
  d <- gss[!is.na(gss$tvhours) & gss$tvhours > 0, ]
  d <- d[d$year %in% c(2000, 2006), ]
  # non-vacuous: the classic path HAS an Ensemble row, so its absence would be a loss, not a shape
  cls <- get_test(tab(d, marital, race, tab_vars = year, pct = "row", test = TRUE,
                      totaltab = "table"))
  expect_true("Ensemble" %in% as.character(cls$year))
  # the overlay used to REPLACE the classic tibble with groups taken from unique(frame[tab_vars]),
  # which has no such level -- so the whole-table test silently vanished under weights / a design.
  rob <- withr::with_options(list(tabxplor.design_effect = TRUE),
                             get_test(tab(d, marital, race, tab_vars = year, wt = tvhours,
                                          pct = "row", test = TRUE, totaltab = "table")))
  expect_true("Ensemble" %in% as.character(rob$year))
  expect_identical(as.character(rob$year), as.character(cls$year))
  expect_true(is.factor(rob$year))                       # not coerced to character by the extra row
  expect_true(all(rob$test == "chi2_design"))
  ens <- rob[as.character(rob$year) == "Ensemble", ]
  expect_equal(ens$n[[1]], nrow(d[!is.na(d$marital) & !is.na(d$race), ]))
})


test_that("an input that cannot serve the weighted basis gets NO design-based test (W-H)", {
  skip_if_not_installed("survey")
  d   <- gss[!is.na(gss$tvhours) & gss$tvhours > 0, ]
  cnt <- as.data.frame(dplyr::count(d, marital, race, name = "n"))
  cnt$wn <- as.data.frame(dplyr::count(d, marital, race, wt = tvhours, name = "wn"))$wn
  withr::local_options(list(tabxplor.design_effect = TRUE))
  t <- tab_counts(cnt, marital, race, counts = n, wt_counts = wn, pct = "row", test = TRUE)
  # pre-aggregated counts carry no per-observation Sigma w^2, so the leaves state basis "n" -- and the
  # whole-table test must say the same thing. It used to run svychisq on the AGGREGATE rows (one
  # "PSU" per aggregate row) and report chi2_design under a footer that said "unweighted sample size".
  expect_identical(tabxplor:::tab_inference_basis(t), "n")
  expect_true(all(get_test(t)$test == "chi2"))
  expect_true(all(is.na(get_test(t)$deff)))
})


# === SECTION: design-based cell variance ==========================================================

skip_if_not_installed("survey")


# A design whose weights correlate with the outcome AND whose PSUs carry a real cluster effect, so
# design != weighted != unweighted by a wide margin (S3.2's "segregated" shape is what stars use).
svv_fixture <- function(n = 1500, seed = 3) {
  set.seed(seed)
  d <- data.frame(strat = factor(rep(1:4, each = n / 4)),
                  psu   = factor(rep(seq_len(n / 25), each = 25)))
  d$h <- stats::rnorm(n)
  d$w <- exp(0.7 * d$h); d$w <- d$w / mean(d$w)
  d$g <- factor(sample(c("a", "b", "c"), n, TRUE), levels = c("a", "b", "c"))
  d$col <- factor(ifelse(stats::rbinom(
    n, 1, stats::plogis(-.2 + .9 * (d$g == "b") + .8 * d$h)) == 1, "yes", "no"),
    levels = c("no", "yes"))
  d$x <- round(stats::rnorm(n, 50, 10) + 6 * d$h, 3)
  d
}


svv_des <- function(d) suppressMessages(
  survey::svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE))


svv_cell <- function(tab, col, getter, level) unname(
  getter(tab[[col]])[which(as.character(tab$levels) == level)])


# Phase 18z16-iiiii (S8.2 item 8). FEW PSUs is the whole point: beta quantiles carry no degrees of
# freedom of their own, so Korn-Graubard rescales the effective base by (qt(a, n-1) / qt(a, degf))^2 --
# worth exactly 1 at the flat basis (which is why test-flat-design-parity.R #13 could never see it)
# and 0.645 here, i.e. an interval that was 25 % too short.
svv_kg_fixture <- function(n = 800, seed = 11) {
  set.seed(seed)
  d <- data.frame(psu   = factor(rep(1:8, each = n / 8)),
                  strat = factor(rep(1:2, each = n / 2)))
  d$w   <- stats::runif(n, 0.5, 3)
  # CROSSED with the PSUs, so degf(subset) == degf(design) and survey is an exact oracle here
  d$g   <- factor(sample(c("A", "B"), n, TRUE))
  d$col <- factor(ifelse(stats::rbinom(n, 1, 0.3) == 1, "yes", "no"), levels = c("no", "yes"))
  d
}


test_that("ci_method = c(cell = 'beta') IS svyciprop(method = 'beta') under a REAL design", {
  d   <- svv_kg_fixture()
  des <- suppressMessages(
    survey::svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE))
  tt  <- suppressMessages(
    tab(des, g, col, pct = "row", ci = "cell", ci_method = c(cell = "beta")))
  i   <- which(as.character(tt[[1]]) == "A")
  ref <- as.numeric(attr(survey::svyciprop(~I(col == "yes"), subset(des, g == "A"),
                                           method = "beta"), "ci"))
  expect_equal(get_ci_inf(tt$yes)[i], ref[1], tolerance = 1e-8)
  expect_equal(get_ci_sup(tt$yes)[i], ref[2], tolerance = 1e-8)

  # FAILS WITHOUT THE RESCALE: un-rescaled Clopper-Pearson on the same base is strictly narrower
  raw <- tabxplor:::ci_beta(get_pct(tt$yes)[i], get_n_eff(tt$yes)[i])
  expect_lt(raw$sup - raw$inf, get_ci_sup(tt$yes)[i] - get_ci_inf(tt$yes)[i])

  # ... and it is a NO-OP wherever there is no design to refer to: same call, weights basis
  flat <- withr::with_options(
    list(tabxplor.design_effect = TRUE),
    tab(d, g, col, wt = w, pct = "row", ci = "cell", ci_method = c(cell = "beta")))
  j <- which(as.character(flat[[1]]) == "A")
  expect_equal(get_ci_inf(flat$yes)[j],
               tabxplor:::ci_beta(get_pct(flat$yes)[j], get_n_eff(flat$yes)[j])$inf,
               tolerance = 1e-12)
})


test_that("contrib's residual is design-corrected, and identical at every table SHAPE (W3, W-B)", {
  d <- svv_fixture(); des <- svv_des(d)
  dsg <- suppressMessages(tab(des, g, col, pct = "no", color = "contrib", test = TRUE))
  raw <- tab(d, g, col, wt = w, pct = "no", color = "contrib")
  cl  <- dsg[["yes"]]; rl <- raw[["yes"]]
  expect_true(all(is.finite(get_n_eff(cl))))
  expect_true(all(is.na(get_n_eff(rl))))
  # Phase 18z16-iv (W-B): z_design = z_classic / sqrt(delta-bar) -- the standard FIRST-ORDER
  # correction, on Rao-Scott's mean generalized design effect of the table's OWN omnibus test. Before,
  # the base was the grand cell's `n_eff`, which is degenerate there (its proportion is 1, so its
  # design variance is 0) and always collapsed to the weights-only B^2/S -- so this ratio was blind to
  # strata and clusters. One base for the whole table, so the ratio is the same in every cell.
  keep <- !is_totrow(cl) & is.finite(fmt_resid(cl)) & is.finite(fmt_resid(rl))
  expect_true(any(keep))
  dbar <- get_test(dsg)$deff[[1]]
  expect_true(is.finite(dbar) && dbar > 0)
  expect_equal(abs(fmt_resid(cl)[keep] / fmt_resid(rl)[keep]),
               rep(1 / sqrt(dbar), sum(keep)), tolerance = 1e-6)
  # the design really is what moved it: the weights-only base gives a DIFFERENT number
  expect_false(isTRUE(all.equal(
    get_n(dsg[["Total"]])[length(dsg[["Total"]])] / dbar,
    get_n_eff(dsg[["Total"]])[length(dsg[["Total"]])], tolerance = 1e-6)))
  # W3 / ruling Q3: a ROW-PERCENTAGE table of the same data gives the SAME residuals -- the residual
  # is a property of the joint distribution and must not depend on `pct`.
  pctt <- suppressMessages(tab(des, g, col, pct = "row", color = "contrib", test = TRUE))
  expect_equal(fmt_resid(pctt[["yes"]]), fmt_resid(cl))
})


test_that("the footer says design-based, in English and in French", {
  # ⚠ v2.0.1 phase 4: each sentence has a LONG and a SHORT half, and the table decides -- the caveat
  # about what the intervals rest on is printed only where an interval, a star, a test or a
  # significance-gated colour is actually on screen (tab_shows_inference()).
  d <- svv_fixture(800); des <- svv_des(d)
  tt <- suppressMessages(tab(des, g, col, pct = "row", test = TRUE))
  expect_equal(tabxplor:::tab_weight_line(tt, lang = "en"),
               "Design-based (survey): weighted estimates, intervals and tests account for the sample design.")
  expect_equal(tabxplor:::tab_weight_line(suppressMessages(tab(des, g, col, pct = "row")), lang = "en"),
               "Design-based (survey): weighted estimates.")
  # Phase 18z16-i: the DEFAULT weighted position now says what it does (S8.2 -- load-bearing).
  expect_equal(tabxplor:::tab_weight_line(tab(d, g, col, wt = w, pct = "row", test = TRUE), lang = "en"),
               "Weighted by w; confidence intervals and tests use the unweighted sample size.")
  expect_equal(tabxplor:::tab_weight_line(tab(d, g, col, wt = w, pct = "row"), lang = "en"),
               "Weighted by w.")
  expect_equal(
    withr::with_options(list(tabxplor.design_effect = TRUE),
                        tabxplor:::tab_weight_line(tab(d, g, col, wt = w, pct = "row", test = TRUE),
                                                   lang = "en")),
    "Weighted by w; confidence intervals and tests account for the weighting.")
})


# === SECTION: the flat closed form equals survey ==================================================

skip_if_not_installed("survey")


fdp_fixture <- function(n = 1200, seed = 7) {
  set.seed(seed)
  d <- data.frame(grp = factor(sample(c("A", "B", "C", "D"), n, TRUE, prob = c(.4, .3, .2, .1))))
  d$w <- exp(stats::rnorm(n, 0, .55)) * c(A = .6, B = 1, C = 1.6, D = 2.4)[as.character(d$grp)]
  d$w <- d$w / mean(d$w)
  lin <- -0.3 + 0.5 * scale(log(d$w))[, 1] + c(A = -.4, B = 0, C = .3, D = .6)[as.character(d$grp)]
  d$col <- factor(ifelse(stats::rbinom(n, 1, stats::plogis(lin)) == 1, "yes", "no"),
                  levels = c("no", "yes"))
  d$x   <- round(stats::rnorm(n, 50, 12) + 6 * log(d$w), 4)
  d$sub   <- factor(rep(c("s1", "s2"), length.out = n))
  d$yes01 <- as.numeric(d$col == "yes")     # a numeric indicator: svymean returns ONE column for it
  d
}


fdp_des <- function(d) survey::svydesign(ids = ~1, weights = ~w, data = as.data.frame(d))


fdp_on  <- function(expr) withr::with_options(list(tabxplor.design_effect = TRUE), expr)


# the design variance our n_eff IMPLIES, cell by cell -- the quantity survey is the oracle for
fdp_var_prop <- function(col) { p <- get_pct(col);  p * (1 - p) / get_n_eff(col) }


fdp_var_mean <- function(col) get_var(col) / get_n_eff(col)


fdp_se       <- function(o) as.numeric(unlist(survey::SE(o)))


test_that("9. unweighted output is untouched by the option, and carries no n_eff", {
  d <- fdp_fixture(800, seed = 3)
  off <- tab(d, grp, col, pct = "row", ci = "cell")
  on  <- fdp_on(tab(d, grp, col, pct = "row", ci = "cell"))
  expect_identical(tab_md(off), tab_md(on))
  expect_true(all(is.na(get_n_eff(on[["yes"]]))))
  expect_identical(tabxplor:::tab_inference_basis(on), "n")   # nothing to serve -> no claim
})


test_that("10. equal weights give n_eff = n * (N-1)/N (the finite-sample factor, S8.1)", {
  d <- fdp_fixture(2000, seed = 4); d$w <- 1
  t1 <- fdp_on(tab(d, grp, col, wt = w, pct = "row", ci = "cell"))[["yes"]]
  # A = p*S under equal weights, so the bracket collapses to S*p(1-p) and n_eff -> base * (N-1)/N,
  # survey's own factor at ids = ~1. It is not optional: matching svyglm exactly is the whole point.
  expect_equal(get_n_eff(t1) / get_tot_n(t1), rep(1999 / 2000, length(t1)), tolerance = 1e-12)
})


# === SECTION: the design-effect option ============================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


kish_data <- function(n = 500L, seed = 20260722L) {
  set.seed(seed)
  tibble::tibble(
    g = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
    y = factor(sample(c("yes", "no"), n, replace = TRUE)),
    x = stats::rnorm(n, 10, 3),
    w = stats::rgamma(n, shape = 0.3, rate = 0.3)      # heavy-tailed -> strong design effect
  )
}


ci_hw <- function(col) (get_ci_sup(col) - get_ci_inf(col)) / 2


testthat::test_that("off-kish output is byte-identical (n_eff NA, display unchanged)", {
  d <- kish_data()
  a <- withr::with_options(list(tabxplor.design_effect = FALSE),
                           tab(d, g, y, wt = w, pct = "row", ci = "ref", na = "drop"))
  b <- withr::with_options(list(tabxplor.design_effect = FALSE),
                           tab(d, g, y, wt = w, pct = "row", ci = "ref", na = "drop"))
  testthat::expect_identical(a[["yes"]], b[["yes"]])
  testthat::expect_true(all(is.na(get_n_eff(a[["yes"]]))))
  testthat::expect_identical(tab_md(a), tab_md(b))
})


# === SECTION: effect sizes and the omnibus tests ==================================================

gss <- fx_gss()


# ---- eta^2 (numeric) ------------------------------------------------------------------------------

test_that("eta^2 matches SSB / SST from lm", {
  te  <- get_test(tab(gss, marital, tvhours, test = TRUE))
  e   <- unique(te$effect_size[te$test %in% c("F_welch", "F_classic")])
  expect_length(e, 1L)
  d   <- gss[!is.na(gss$tvhours), ]
  av  <- stats::anova(stats::lm(tvhours ~ marital, d))
  expect_equal(e, av$`Sum Sq`[1] / sum(av$`Sum Sq`), tolerance = 1e-6)
  expect_equal(te$es_type[te$test == "F_welch"], "eta2")
})
