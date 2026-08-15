# PURPOSE: THE parity contract of Phase 18z16 (dev/weights_framework_redesign.md S7). The flat
#   closed form is a SECOND implementation of something `survey` already computes; that is only safe
#   if the identity is tested, permanently, rather than asserted in a comment.
# ROLE: every assertion here is "equals survey on the same design", never a hard-coded number -- so
#   these tests stay valid as the package changes, and they fail the moment the algebra drifts.
#   The USER-VISIBLE behaviour of the option lives in test-design-effect.R; this file is the maths.
# KEY CONSTRAINTS:
#   - The oracle is a REAL survey::svydesign(ids = ~1, weights = ~w) built on the same data, so the
#     finite-sample factor n/(n-1) is part of the identity (S8.1), not an approximation to tolerate.
#   - The fixture makes the weights carry information about the outcome, which is exactly what Kish
#     assumed away -- so a regression to Kish's (Sum w)^2 / Sum(w^2) fails these tests, it does not
#     merely lose a digit.
# See: dev/weights_framework_redesign.md S1 (the closed form) and S7 (this contract).

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

test_that("4. the closed form == svyrecvar on the SAME flat design (both leaves)", {
  d <- fdp_fixture(1500, seed = 11); des <- fdp_des(d)
  expect_true(tabxplor:::svy_design_is_flat(des))
  # a stratified / clustered design is NOT flat and must keep the svyrecvar path
  expect_false(tabxplor:::svy_design_is_flat(
    survey::svydesign(ids = ~1, strata = ~grp, weights = ~w, data = as.data.frame(d))))

  # force the influence-function producer even though the design is flat, and compare
  by_w  <- fdp_on(tab(d, grp, col, wt = w, pct = "row", ci = "cell"))[["yes"]]
  prep  <- tabxplor:::svy_var_prep(des, seq_len(nrow(d)))
  keys  <- list(c(levels(d$grp), "Total"))
  V <- tabxplor:::svy_var_prop(
    prep, keys, 0L, list(tabxplor:::svy_key_chr(d$grp)), tabxplor:::svy_key_chr(d$col),
    c("no", "yes", "Total"), "row")$v
  expect_equal(fdp_var_prop(by_w), V[, 2], tolerance = 1e-9, ignore_attr = TRUE)

  by_m <- fdp_on(tab(d, grp, x, wt = w, ci = "cell"))$x
  Vm <- tabxplor:::svy_var_mean(prep, keys, 0L, list(tabxplor:::svy_key_chr(d$grp)), list(x = d$x))$v
  expect_equal(fdp_var_mean(by_m), Vm[, 1], tolerance = 1e-9, ignore_attr = TRUE)
})

test_that("7. tab_reg(empirical=)'s Obs_OR bracket == the univariable svyglm SE(log OR)", {
  skip_if_not_installed("broom")
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
  skip_if_not_installed("broom")
  dc <- tab_reg(d, "x", "grp", family = "gaussian", empirical = TRUE, wt = "w")[["Obs_diff"]]
  hr <- (get_ci_sup(dc) - get_ci_inf(dc)) / 2
  expect_true(all(abs(hr[is.finite(hr)] / (se_svy * conf_level_to_z(0.95, digits = Inf)) - 1) < 0.07))
})

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

test_that("5. the weighted omnibus chi2 IS survey::svychisq on the flat design", {
  d <- fdp_fixture(2500, seed = 13)
  for (shape in list(c(4, 2), c(4, 3), c(5, 4), c(3, 3))) {
    set.seed(100 + shape[1] * shape[2])
    dd <- d
    dd$r <- factor(sample(letters[seq_len(shape[1])], nrow(dd), TRUE))
    dd$c <- factor(sample(LETTERS[seq_len(shape[2])], nrow(dd), TRUE,
                          prob = seq_len(shape[2])))
    des <- fdp_des(dd)
    te  <- fdp_on(get_test(tab(dd, r, c, wt = w, test = TRUE)))
    ref <- survey::svychisq(~r + c, des, statistic = "F")
    expect_equal(te$test[1], "chi2_design")
    expect_equal(te$statistic[1], unname(ref$statistic),          tolerance = 1e-10)
    expect_equal(te$df1[1],       unname(ref$parameter[["ndf"]]), tolerance = 1e-10)
    expect_equal(te$pvalue[1],    unname(ref$p.value),            tolerance = 1e-10)
    expect_equal(te$n[1],         nrow(dd))              # W8: always the RAW count
  }
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
