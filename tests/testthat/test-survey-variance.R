# PURPOSE: Phase 18z14-ii -- Route A, design-based intervals in tab().
# ROLE: Locks (a) R/survey-variance.R against survey itself, on a stratified, a clustered, a
#   stratified+clustered and a CALIBRATED design, for proportions and for means; (b) the four
#   consumers of the `n_eff` field end to end (cell CI, cell-vs-reference difference, the
#   color = "OR" interval, the contrib residual); (c) the degradation and byte-identity guarantees.
# DESIGN: `survey` is the oracle throughout -- every assertion is "equals svyby / svymean / svyglm on
#   the same design", never a hard-coded number. Each fixture is built so it FAILS before the fix:
#   the design quantity is asserted to DIFFER from the single-stage one, so a regression that quietly
#   stops writing the design base cannot pass vacuously.
# See: dev/full_survey_design_scope.md S4 (Route A, the measurements).

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

test_that("svy_var_prep returns NULL rather than a wrong number on inputs it cannot serve", {
  d <- svv_fixture(600); des <- svv_des(d)
  expect_null(tabxplor:::svy_var_prep(NULL, seq_len(nrow(d))))
  expect_null(tabxplor:::svy_var_prep(des, NULL))
  expect_null(tabxplor:::svy_var_prep(des, c(1L, NA_integer_)))
  rp <- suppressWarnings(survey::as.svrepdesign(des, type = "bootstrap", replicates = 4))
  expect_null(tabxplor:::svy_var_prep(rp, seq_len(nrow(d))))   # ruling Q5: never approximated
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

test_that("the footer says design-based, in English and in French", {
  d <- svv_fixture(800); des <- svv_des(d)
  tt <- suppressMessages(tab(des, g, col, pct = "row"))
  expect_equal(tabxplor:::tab_weight_line(tt, lang = "en"),
               "Design-based (survey): weighted estimates, intervals and tests account for the sample design.")
  # Phase 18z16-i: the DEFAULT weighted position now says what it does (S8.2 -- load-bearing).
  expect_equal(tabxplor:::tab_weight_line(tab(d, g, col, wt = w, pct = "row"), lang = "en"),
               "Weighted by w; confidence intervals and tests use the unweighted sample size.")
  expect_equal(
    withr::with_options(list(tabxplor.design_effect = TRUE),
                        tabxplor:::tab_weight_line(tab(d, g, col, wt = w, pct = "row"), lang = "en")),
    "Weighted by w; confidence intervals and tests account for the weighting.")
})

test_that("the French design-based footer is translated", {
  skip_if_no_gettext()
  d <- svv_fixture(800); des <- svv_des(d)
  tt <- suppressMessages(tab(des, g, col, pct = "row"))
  fr <- tabxplor:::tab_weight_line(tt, lang = "fr")
  expect_true(grepl("plan de sondage", fr, fixed = TRUE))
  expect_false(grepl("Design-based", fr, fixed = TRUE))
})
