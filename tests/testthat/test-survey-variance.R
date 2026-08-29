# PURPOSE: design-based cell variance, and the effect sizes and omnibus tests that read it.
# ROLE: the shipped CONTRACT for R/survey-variance.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

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




# ---- the module against survey ------------------------------------------------------------------

test_that("svy_var_prop / svy_var_mean equal survey's own variance on every design shape", {
  d  <- svv_fixture()
  RL <- levels(d$g); CL <- levels(d$col); n <- nrow(d)
  keys  <- list(g = RL)
  mkeys <- list(g = tabxplor:::svy_key_chr(d$g))
  mcol  <- tabxplor:::svy_key_chr(d$col)

  des  <- svv_des(d)
  cal  <- suppressWarnings(survey::calibrate(
    des, ~h, c(`(Intercept)` = sum(d$w), h = sum(d$w * d$h) * 1.05)))
  designs <- list(
    "weights only"         = survey::svydesign(~1, weights = ~w, data = d),
    "stratified"           = survey::svydesign(~1, strata = ~strat, weights = ~w, data = d),
    "stratified+clustered" = des,
    "calibrated"           = cal)

  for (nm in names(designs)) {
    dsg  <- designs[[nm]]
    prep <- tabxplor:::svy_var_prep(dsg, seq_len(n))
    expect_false(is.null(prep), info = nm)

    V  <- tabxplor:::svy_var_prop(prep, keys, 0L, mkeys, mcol, CL, "row")$v
    by <- survey::svyby(~col, ~g, dsg, survey::svymean, covmat = TRUE)
    Vs <- stats::vcov(by); ix <- names(stats::coef(by))
    se_ref <- vapply(seq_along(RL), function(i) {
      k <- which(ix == paste0(RL[i], ":col", CL[2])); sqrt(Vs[k, k]) }, numeric(1))
    expect_equal(sqrt(V[, 2]), se_ref, tolerance = 1e-8, info = nm)

    Vm <- tabxplor:::svy_var_mean(prep, keys, 0L, mkeys, list(x = d$x))$v
    expect_equal(sqrt(Vm[, 1]), unname(survey::SE(survey::svyby(~x, ~g, dsg, survey::svymean))),
                 tolerance = 1e-8, info = nm)
  }
})



test_that("svy_var_prep returns NULL rather than a wrong number on inputs it cannot serve", {
  d <- svv_fixture(600); des <- svv_des(d)
  expect_null(tabxplor:::svy_var_prep(NULL, seq_len(nrow(d))))
  expect_null(tabxplor:::svy_var_prep(des, NULL))
  expect_null(tabxplor:::svy_var_prep(des, c(1L, NA_integer_)))
  rp <- suppressWarnings(survey::as.svrepdesign(des, type = "bootstrap", replicates = 4))
  expect_null(tabxplor:::svy_var_prep(rp, seq_len(nrow(d))))   # ruling Q5: never approximated
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




# ---- degradation, byte-identity, and the footer --------------------------------------------------

test_that("the basis is ONE resolved fact, and it is stored on the table", {
  d <- svv_fixture(1200)
  expect_identical(tabxplor:::svy_inference_basis(NULL, character()), "n")
  expect_identical(tabxplor:::svy_inference_basis(NULL, "w"), "n")
  withr::with_options(list(tabxplor.design_effect = TRUE), {
    expect_identical(tabxplor:::svy_inference_basis(NULL, "w"), "weights")
    expect_identical(tabxplor:::svy_inference_basis(NULL, character()), "n")
    k <- tab(d, g, col, wt = w, pct = "row", ci = "cell")
    expect_identical(tabxplor:::tab_inference_basis(k), "weights")
    expect_true(all(is.finite(get_n_eff(k[["yes"]]))))
  })
  # tab_reg() FORCES it (ruling 1): its crude columns must match the model column beside them.
  expect_identical(tabxplor:::svy_inference_basis(NULL, "w", force = TRUE), "weights")
  expect_identical(tabxplor:::svy_inference_basis(list(design = 1), character()), "design")
  plain <- tab(d, g, col, pct = "row", ci = "cell")
  expect_true(all(is.na(get_n_eff(plain[["yes"]]))))
  expect_identical(tabxplor:::tab_inference_basis(plain), "n")
  expect_true(is.na(tabxplor:::fmt_degf_attr(plain[["yes"]])))   # no design df to refer to
  # a weighted table at the default basis carries the fact on its columns, so its footer can say so
  w1 <- tab(d, g, col, wt = w, pct = "row")
  expect_identical(tabxplor:::tab_inference_basis(w1), "n")
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



testthat::test_that("tab_reg never reads the option: its crude CIs are ALWAYS corrected (ruling 1)", {
  testthat::skip_if_not_installed("survey")
  d <- kish_data(600L)
  mk <- function(v) withr::with_options(
    list(tabxplor.design_effect = v),
    tab_reg(d, outcome = "y", predictors = "g", family = "binomial", wt = "w",
            empirical = TRUE))
  off <- mk(FALSE); on <- mk(TRUE)
  cn <- names(on)
  obsor  <- grep("Obs_OR",  cn, value = TRUE, fixed = TRUE)[1]
  model  <- grep("Model_OR", cn, value = TRUE, fixed = TRUE)[1]

  # W1/W2: the tab()-scoped option cannot move a regression table at all -- crude and model columns
  # are on ONE basis, which is why they are comparable.
  testthat::expect_equal(get_ci_inf(off[[obsor]]), get_ci_inf(on[[obsor]]))
  testthat::expect_equal(get_ci_sup(off[[obsor]]),  get_ci_sup(on[[obsor]]))
  testthat::expect_equal(get_ci_inf(off[[model]]),  get_ci_inf(on[[model]]))
  # and the crude base IS corrected: n_eff < n wherever the weights are unequal
  testthat::expect_true(any(ci_hw(off[[obsor]]) > 0, na.rm = TRUE))
  # displayed count untouched
  testthat::expect_identical(get_n(off[[obsor]]), get_n(on[[obsor]]))
  # the footer names the weighted basis, whatever the option says
  testthat::expect_identical(tabxplor:::tab_inference_basis(off), "weights")
})




# === SECTION: effect sizes and the omnibus tests ==================================================

gss <- fx_gss()



# ---- Survey design (Rao-Scott), opt-in ------------------------------------------------------------

test_that("survey factor test matches survey::svychisq", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  des <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  te  <- suppressMessages(get_test(tab(des, sch.wide, awards, pct = "row", test = TRUE)))
  ref <- survey::svychisq(~sch.wide + awards, des, statistic = "F")
  expect_equal(te$test[1], "chi2_design")
  expect_equal(te$statistic[1], unname(ref$statistic), tolerance = 1e-6)
  expect_equal(te$pvalue[1],    unname(ref$p.value),   tolerance = 1e-6)
})



test_that("the test RUNG is derived from the input, and `test` is TRUE/FALSE only", {
  skip_if_not_installed("survey")
  suppressWarnings(utils::data("api", package = "survey"))
  # Phase 18z14-i: ids/strata/fpc/nest are gone -- a design is expressed by BUILDING one. The rung
  # follows what was passed, so there is no `test = "survey"` to ask for and not get.
  des <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
  te  <- suppressMessages(get_test(tab(des, sch.wide, awards, test = TRUE)))
  ref <- survey::svychisq(~sch.wide + awards, des, statistic = "F")
  expect_equal(te$test[1],      "chi2_design")
  expect_equal(te$statistic[1], unname(ref$statistic), tolerance = 1e-6)
  expect_equal(te$pvalue[1],    unname(ref$p.value),   tolerance = 1e-6)

  # weights alone -> a weighted chi2; weights + the kish option -> the same rescaled to n_eff
  expect_equal(get_test(tab(apistrat, sch.wide, awards, wt = pw, test = TRUE))$test[1], "chi2")
  withr::local_options(tabxplor.design_effect = TRUE)
  expect_equal(get_test(tab(apistrat, sch.wide, awards, wt = pw, test = TRUE))$test[1], "chi2_design")

  expect_error(tab(apistrat, sch.wide, awards, wt = pw, test = "survey"), "TRUE.*FALSE")
})
