
testthat::test_that("ci_mean_ratio(method='quasipoisson') equals a quasi-Poisson regression", {
  set.seed(214)
  n <- 300
  grp <- factor(sample(c("a", "b"), n, TRUE))
  y   <- stats::rpois(n, lambda = ifelse(grp == "a", 3, 5)) + stats::rpois(n, 2)  # over-dispersed
  a <- y[grp == "a"]; b <- y[grp == "b"]
  res <- ci_mean_ratio(mean(b), stats::var(b), length(b), mean(a), stats::var(a), length(a),
                       method = "quasipoisson", want_p = TRUE)
  fq  <- stats::glm(y ~ grp, family = stats::quasipoisson())
  co  <- summary(fq)$coefficients["grpb", ]
  crit <- stats::qt(0.975, df = stats::df.residual(fq))
  testthat::expect_equal(c(res$inf, res$sup),
                         exp(co["Estimate"] + c(-1, 1) * crit * co["Std. Error"]) |> unname(),
                         tolerance = 1e-6)
  testthat::expect_equal(res$pvalue, unname(co["Pr(>|t|)"]), tolerance = 1e-6)
})


testthat::test_that("ci_pivot() reproduces the t-test across confidence levels", {
  set.seed(303)
  x <- stats::rnorm(30, 1, 3)
  for (cl in c(0.80, 0.90, 0.99)) {
    res <- ci_pivot(mean(x), stats::sd(x) / sqrt(30), df = 29, conf_level = cl, want_p = TRUE)
    tt  <- stats::t.test(x, conf.level = cl)
    testthat::expect_equal(res$pvalue, tt$p.value, tolerance = 1e-10)  # p is level-independent
    testthat::expect_equal(c(res$inf, res$sup), as.numeric(tt$conf.int), tolerance = 1e-10)
  }
})


# === SECTION: proportion-difference p-values vs DescTools (duality) ===================

# For a p-value that inverts interval `method`, the DescTools interval computed at confidence
# level (1 - p) must have its NEAR bound sitting exactly on zero. This checks our inversion
# against DescTools' independent implementation of the same interval.
duality_near_zero <- function(x1, n1, x2, n2, method, pvalue) {
  ci <- DescTools::BinomDiffCI(x1, n1, x2, n2, conf.level = 1 - pvalue, method = method)
  min(abs(ci[, "lwr.ci"]), abs(ci[, "upr.ci"]))
}


# Cases chosen to give a moderate p (roughly 0.002 - 0.25) so 1 - p is a safe confidence level.
prop_cases <- list(
  c(35, 50, 20, 50), c(30, 50, 20, 50), c(26, 50, 20, 50),
  c(40, 80, 28, 80), c(12, 30, 18, 30), c(60, 100, 45, 100)
)


testthat::test_that("newcombe_pvalue() is the exact inversion of DescTools score interval", {
  testthat::skip_if_not_installed("DescTools")
  for (g in prop_cases) {
    x1 <- g[1]; n1 <- g[2]; x2 <- g[3]; n2 <- g[4]
    p <- newcombe_pvalue(x1 / n1, n1, x2 / n2, n2)
    testthat::expect_true(p > 1e-3 && p < 0.5, label = paste0("p in range [", paste(g, collapse = "/"), "]"))
    testthat::expect_equal(duality_near_zero(x1, n1, x2, n2, "score", p), 0,
                           tolerance = 1e-4, label = paste0("Newcombe [", paste(g, collapse = "/"), "]"))
  }
})


testthat::test_that("ci_prop_diff(method='ac') p-value is the exact inversion of DescTools ac", {
  testthat::skip_if_not_installed("DescTools")
  for (g in prop_cases) {
    x1 <- g[1]; n1 <- g[2]; x2 <- g[3]; n2 <- g[4]
    p <- ci_prop_diff(x1 / n1, n1, x2 / n2, n2, method = "ac", want_p = TRUE)$pvalue
    testthat::expect_equal(duality_near_zero(x1, n1, x2, n2, "ac", p), 0,
                           tolerance = 1e-4, label = paste0("AC [", paste(g, collapse = "/"), "]"))
  }
})


testthat::test_that("ci_prop_diff(method='wald') p-value is the exact inversion of DescTools wald", {
  testthat::skip_if_not_installed("DescTools")
  for (g in prop_cases) {
    x1 <- g[1]; n1 <- g[2]; x2 <- g[3]; n2 <- g[4]
    p <- ci_prop_diff(x1 / n1, n1, x2 / n2, n2, method = "wald", want_p = TRUE)$pvalue
    testthat::expect_equal(duality_near_zero(x1, n1, x2, n2, "wald", p), 0,
                           tolerance = 1e-4, label = paste0("Wald [", paste(g, collapse = "/"), "]"))
  }
})


# === SECTION: opt-out and cell-interval sanity =======================================

testthat::test_that("want_p = FALSE and cell intervals carry no p-value", {
  testthat::expect_true(is.na(ci_pivot(1.2, 0.3, df = 10, want_p = FALSE)$pvalue))
  testthat::expect_true(is.na(ci_newcombe(0.6, 50, 0.4, 50, want_p = FALSE)$pvalue))
  testthat::expect_true(all(is.na(ci_wilson(c(0.3, 0.7), c(40, 40))$pvalue)))  # cell CI: no H0
})


# === SECTION: the MODEL-based methods (a dispersion pooled over the whole variable) ===
#
# `ols` and `quasipoisson` reproduce a MODEL's coefficient interval, not a two-sample test, so their
# variance is pooled over every level of the predictor -- which the elementwise engines cannot see.
# ci_pool_disp() computes it for them; with no `pool` they fall back to the pair, which IS the level
# set when the variable has two levels.

ci_pool_fixture <- function(y, g) {
  s <- split(y, g)
  list(n = vapply(s, length, 1L), m = vapply(s, mean, 1.0), v = vapply(s, stats::var, 1.0))
}


testthat::test_that("mean_diff = 'ols' IS the linear model's coefficient interval (k = 3)", {
  d  <- tidyr::drop_na(fx_gss()[, c("tvhours", "race")])
  d$race <- forcats::fct_drop(d$race)
  f  <- ci_pool_fixture(d$tvhours, d$race)
  p  <- ci_pool_disp(n = f$n, mean = f$m, var = f$v, by = rep("a", length(f$n)),
                     use = rep(TRUE, length(f$n)), kind = "mean_diff")
  got <- ci_mean_diff2(f$m[-1], f$v[-1], f$n[-1], f$m[1], f$v[1], f$n[1], method = "ols",
                       pool = list(disp = p$disp[-1], df = p$df[-1]))
  ref <- stats::confint(stats::lm(tvhours ~ race, data = d))[-1, , drop = FALSE]
  testthat::expect_equal(unname(got$inf), unname(ref[, 1]), tolerance = 1e-10)
  testthat::expect_equal(unname(got$sup), unname(ref[, 2]), tolerance = 1e-10)
  # the pooled dispersion IS the model's residual variance, and its df the model's
  testthat::expect_equal(p$disp[[1]], stats::sigma(stats::lm(tvhours ~ race, data = d))^2,
                         tolerance = 1e-10)
  testthat::expect_equal(p$df[[1]], stats::df.residual(stats::lm(tvhours ~ race, data = d)))
})


testthat::test_that("mean_ratio = 'quasipoisson' IS the quasi-Poisson dispersion (k = 3)", {
  d  <- tidyr::drop_na(fx_gss()[, c("tvhours", "race")])
  d  <- d[d$tvhours > 0, ]; d$race <- forcats::fct_drop(d$race)
  f  <- ci_pool_fixture(d$tvhours, d$race)
  p  <- ci_pool_disp(n = f$n, mean = f$m, var = f$v, by = rep("a", length(f$n)),
                     use = rep(TRUE, length(f$n)), kind = "mean_ratio")
  fit <- stats::glm(tvhours ~ race, data = d, family = stats::quasipoisson())
  testthat::expect_equal(p$disp[[1]], summary(fit)$dispersion, tolerance = 1e-7)
  got <- ci_mean_ratio(f$m[-1], f$v[-1], f$n[-1], f$m[1], f$v[1], f$n[1], method = "quasipoisson",
                       pool = list(disp = p$disp[-1], df = p$df[-1]))
  se  <- (log(got$sup) - log(got$inf)) / (2 * stats::qt(0.975, p$df[[1]]))
  testthat::expect_equal(unname(se), unname(summary(fit)$coefficients[-1, 2]), tolerance = 1e-6)
})


testthat::test_that("with no `pool` the two model methods fall back to the pair", {
  a <- ci_mean_diff2(3.5, 4, 100, 2.5, 3, 120, method = "ols")
  testthat::expect_equal(a, ci_mean_diff2(3.5, 4, 100, 2.5, 3, 120, method = "student"))
  # and ci_pool_disp() excludes the rows that are NOT levels (a total row is a mixture of them)
  p <- ci_pool_disp(n = c(10, 10, 20), mean = c(1, 3, 2), var = c(2, 2, 3),
                    by = rep("a", 3), use = c(TRUE, TRUE, FALSE), kind = "mean_diff")
  testthat::expect_equal(p$disp[[1]], 2)          # only the two levels, not the total
  testthat::expect_equal(p$df[[1]], 18)           # N - k = 20 - 2
})



# === SECTION: the Katz log-RR interval ============================================================

d <- fx_gss()


katz_hand <- function(p1, n1, p2, n2, conf_level = 0.95) {
  se <- sqrt((1 - p1) / (n1 * p1) + (1 - p2) / (n2 * p2))
  z  <- stats::qnorm(1 - (1 - conf_level) / 2)
  list(inf = exp(log(p1 / p2) - z * se), sup = exp(log(p1 / p2) + z * se),
       pvalue = 2 * stats::pnorm(-abs(log(p1 / p2) / se)))
}


testthat::test_that("ci_katz_rr honours conf_level and widens with it", {
  a <- ci_katz_rr(0.3, 400, 0.25, 800, conf_level = 0.90)
  b <- ci_katz_rr(0.3, 400, 0.25, 800, conf_level = 0.99)
  testthat::expect_equal(a$inf, katz_hand(0.3, 400, 0.25, 800, 0.90)$inf)
  testthat::expect_true(b$inf < a$inf && b$sup > a$sup)
  testthat::expect_equal(a$pvalue, b$pvalue)          # the p does not depend on the level
})


testthat::test_that("ci_katz_rr degrades to NA rather than +/-Inf on an empty cell or reference", {
  got <- ci_katz_rr(c(0, 0.3, 0.3), c(100, 100, 100), c(0.2, 0, 0.2), c(100, 100, 0))
  testthat::expect_true(all(is.na(got$inf)))
  testthat::expect_true(all(is.na(got$sup)))
  testthat::expect_true(all(is.na(got$pvalue)))
})


testthat::test_that("ci_katz_rr skips the p-value when stars are off", {
  testthat::expect_true(all(is.na(ci_katz_rr(0.3, 400, 0.25, 800, want_p = FALSE)$pvalue)))
})



# --- the trigger rule ---------------------------------------------------------------------

testthat::test_that("only a ratio TEXT channel switches the stored interval to Katz", {
  ratio_cases <- list("ratio", c("ratio", "diff"), c(pct = "ratio"),
                      list(pct = c("ratio", "diff")))
  diff_cases  <- list(TRUE, "diff", c("diff", "ratio"), c(pct = "diff"))
  for (cc in ratio_cases) {
    t <- tab(d, race, marital, pct = "row", color = cc, color_signif = "grey_non_signif")
    testthat::expect_identical(get_scale(t$Married), "pct_ratio")
  }
  for (cc in diff_cases) {                       # the defaults must be untouched
    t <- tab(d, race, marital, pct = "row", color = cc, color_signif = "grey_non_signif")
    testthat::expect_identical(get_scale(t$Married), "points")
  }
})


testthat::test_that("ci = 'cell' has no ratio counterpart (a one-proportion interval)", {
  t <- tab(d, race, marital, pct = "row", color = "ratio", ci = "cell")
  testthat::expect_identical(get_scale(t$Married), "level_pct")
})


testthat::test_that("a MEAN now gets a ratio-of-means interval under a ratio colour (14v-ii)", {
  # Was: a mean kept the difference interval whatever the colour (a ratio of means "needed Fieller").
  # 14v-ii ships ci_mean_ratio, so a ratio-coloured mean owns a real ratio interval (ci_type "ratio").
  t <- tab(d, race, c(marital, tvhours), pct = "row", color = "ratio",
           color_signif = "grey_non_signif")
  testthat::expect_identical(get_scale(t$Married), "pct_ratio")
  testthat::expect_identical(get_scale(t$tvhours), "mean_ratio")
})


testthat::test_that("tab_resolve_settings only asks for the ratio scale where a diff CI is built", {
  s <- tab_resolve_settings(color = "diff", ci = c("ref", "cell", "no"),
                            chi2 = FALSE, ref = "tot",
                            pct_vect = list("row", "row", "row"), col_vars_text = TRUE,
                            color_ratio_ci = TRUE)
  testthat::expect_identical(s$ci_scale, c("ratio", "diff", "diff"))
  s0 <- tab_resolve_settings(color = "diff", ci = "ref", chi2 = FALSE, ref = "tot",
                             pct_vect = list("row"), col_vars_text = TRUE)
  testthat::expect_identical(s0$ci_scale, "diff")     # the default is unchanged
})



# --- the stored interval ------------------------------------------------------------------

testthat::test_that("the stored bounds are the Katz ones, centred on the ratio", {
  t   <- tab(d, race, marital, pct = "row", color = "ratio",
             color_signif = "grey_non_signif", stars = TRUE)
  col <- t$Married
  # ref = "tot" marks the reference as the TOTAL row (tab_ci's own ref_mask), not via in_refrow.
  r   <- which(get_reference(col, mode = "cells"))
  i   <- setdiff(seq_along(col), r)
  exp <- katz_hand(get_pct(col)[i], get_tot_n(col)[i],
                   get_pct(col)[r], get_tot_n(col)[r])
  testthat::expect_equal(get_ci_inf(col)[i],  exp$inf)
  testthat::expect_equal(get_ci_sup(col)[i],  exp$sup)
  testthat::expect_equal(get_pvalue(col)[i],  exp$pvalue)
  testthat::expect_equal(ci_center(col), get_ratio(col))    # get_ci() reads back off this
  testthat::expect_true(is.na(get_ci_inf(col)[r]))          # a reference is never self-compared
})


testthat::test_that("the ratio bracket renders on the ratio scale: no x100, no %", {
  t <- tab(d, race, marital, pct = "row", color = "ratio", color_signif = "grey_non_signif")
  b <- format(set_display(t$Married, "ci"))
  b <- b[!is.na(b)]
  testthat::expect_false(any(grepl("%", b, fixed = TRUE)))
})


testthat::test_that("the legend names Katz, not a diff method it was not built with", {
  t <- tab(d, race, marital, pct = "row", color = "ratio", color_signif = "grey_non_signif")
  lg <- tab_color_legend(t, medium = "plain", lang = "en")
  testthat::expect_match(paste(lg, collapse = " "), "Katz")
  testthat::expect_false(grepl("Newcombe", paste(lg, collapse = " ")))
})



# --- the colour engine reads the stored scale ---------------------------------------------

testthat::test_that("grey_non_signif greys exactly the cells whose ratio interval includes 1", {
  t   <- tab(d, race, marital, pct = "row", color = "ratio", color_signif = "grey_non_signif")
  for (cc in t[purrr::map_lgl(t, is_fmt)]) {
    if (!identical(get_scale(cc), "pct_ratio")) next
    sl  <- fmt_color_channels(cc)$text
    sig <- (get_ci_inf(cc) > 1 | get_ci_sup(cc) < 1) %in% TRUE
    testthat::expect_true(all(sl[!sig] == 0L | is.na(sl[!sig])))
  }
})


testthat::test_that("guaranteed_effect colours the right SIDE on a ratio interval", {
  t <- tab(d, c(race, marital), c(partyid, relig), pct = "row", color = "ratio",
           color_signif = "guaranteed_effect", output_list = TRUE)
  n <- 0L
  for (one in t) for (cc in one[purrr::map_lgl(one, is_fmt)]) {
    if (!identical(get_scale(cc), "pct_ratio")) next
    sl <- fmt_color_channels(cc)$text; rr <- get_ratio(cc)
    k  <- !is.na(sl) & sl > 0L & !is.na(rr)
    n  <- n + sum(k)
    testthat::expect_identical(sl[k] <= 4L, rr[k] > 1)     # slots 1-4 over, 5-8 under
  }
  testthat::expect_gt(n, 0L)
})


testthat::test_that("a derived diff background channel agrees with the ratio text channel", {
  t <- tab(d, race, marital, pct = "row", color = c("ratio", "diff"),
           color_signif = "guaranteed_effect")
  for (cc in t[purrr::map_lgl(t, is_fmt)]) {
    if (!identical(get_scale(cc), "pct_ratio")) next
    ch <- fmt_color_channels(cc)
    k  <- !is.na(ch$text) & !is.na(ch$bg) & ch$text > 0L & ch$bg > 0L
    testthat::expect_identical(ch$text[k] <= 4L, ch$bg[k] <= 4L)   # same direction
  }
})


testthat::test_that("the significance gate keys on the stored ci_type, not on the measure", {
  # a diff-measure column riding a ratio interval is still gated (both test p1 = p2)
  t  <- tab(d, race, marital, pct = "row", color = c("ratio", "diff"),
            color_signif = "grey_non_signif")
  cc <- t$Married
  testthat::expect_identical(get_scale(cc), "pct_ratio")
  bg <- fmt_color_channels(cc)$bg
  testthat::expect_true(any(!is.na(bg) & bg > 0L))     # not greyed wholesale by a neutral mismatch
})



# --- 14v-ii: numeric ratio-of-means (the ci_type="diff" bug fix) ---------------------------

testthat::test_that("a ratio-coloured MEAN stores ci_type='ratio' + ratio-scale bounds (14v-ii)", {
  # Regression lock for the §48 bug: tab(mean, color='ratio', ci='diff') used to store the DIFFERENCE
  # bounds mislabelled as a ratio. It must now store a real ratio-of-means interval (centred on the
  # cell/reference ratio, neutral 1).
  d2 <- fx_gss() |> dplyr::mutate(race = forcats::fct_rev(race)) |>
    dplyr::filter(!is.na(tvhours))
  t   <- tab(d2, race, tvhours, ref = 1, color = "ratio", ci = "ref", stars = TRUE)
  col <- t$tvhours
  testthat::expect_identical(get_scale(col), "mean_ratio")
  testthat::expect_equal(ci_center(col), get_ratio(col))          # centred on the ratio, not the diff
  # the stored bounds bracket the ratio, not the difference; a diff CI would bracket get_diff (~1.4)
  k <- !is.na(get_ci_inf(col))
  testthat::expect_true(all(get_ci_inf(col)[k] <= get_ratio(col)[k] &
                            get_ratio(col)[k] <= get_ci_sup(col)[k]))
})


testthat::test_that("the three mean_ratio methods give the three decisions-48 intervals", {
  d2 <- fx_gss() |> dplyr::mutate(race = forcats::fct_rev(race)) |>
    dplyr::filter(!is.na(tvhours))
  g  <- d2 |> dplyr::filter(race %in% c("White", "Black"))
  hand <- function(method, want_p = FALSE) {
    gb <- g$tvhours[g$race == "Black"]; gw <- g$tvhours[g$race == "White"]
    ci_mean_ratio(mean(gb), stats::var(gb), length(gb),
                  mean(gw), stats::var(gw), length(gw), method = method, want_p = want_p)
  }
  for (m in c("robust", "quasipoisson", "poisson")) {
    t   <- tab(g |> dplyr::mutate(race = forcats::fct_drop(race)), race, tvhours, ref = 1,
               color = "ratio", ci = "ref", ci_method = c(mean_ratio = m), stars = TRUE)
    col <- t$tvhours
    k   <- which(as.character(t$race) == "Black")  # Black vs White = ref (Total row also has a CI)
    ref <- hand(m, want_p = TRUE)
    testthat::expect_equal(get_ci_inf(col)[k], ref$inf, tolerance = 1e-6, label = m)
    testthat::expect_equal(get_ci_sup(col)[k], ref$sup, tolerance = 1e-6, label = m)
  }
})


testthat::test_that("the ratio-of-means bracket renders bare (no %, >= 2 digits)", {
  d2 <- fx_gss() |> dplyr::mutate(race = forcats::fct_rev(race)) |>
    dplyr::filter(!is.na(tvhours))
  t <- tab(d2, race, tvhours, ref = 1, color = "ratio", ci = "ref")
  b <- format(set_display(t$tvhours, "ci"))
  b <- b[!is.na(b) & nzchar(trimws(b))]
  testthat::expect_false(any(grepl("%", b, fixed = TRUE)))
})


testthat::test_that("the legend names the ratio-of-means method (Welch/Student/robust/quasi/Poisson)", {
  d2 <- fx_gss() |> dplyr::mutate(race = forcats::fct_rev(race))
  leg <- function(...) paste(tab_color_legend(
    tab(d2, race, tvhours, ref = 1, ci = "ref", color_signif = "grey_non_signif", ...),
    medium = "plain", lang = "en"), collapse = " ")
  testthat::expect_match(leg(color = "ratio"),                                   "robust-Poisson")
  testthat::expect_match(leg(color = "ratio", ci_method = c(mean_ratio = "quasipoisson")),
                         "quasi-Poisson")
  testthat::expect_match(leg(color = "ratio", ci_method = c(mean_ratio = "poisson")),
                         "Poisson interval")
  testthat::expect_match(leg(color = "diff"),                                    "Welch t interval")
  testthat::expect_match(leg(color = "diff", ci_method = c(mean_diff = "student")),
                         "Student t interval")
})



# === SECTION: a Woolf interval for the empirical OR ===============================================

or_data <- function() {
  data.frame(
    g = factor(rep(c("a", "b", "c"), each = 100)),
    y = factor(c(rep(c("yes", "no"), c(30, 70)),    # group a: 30 yes / 70 no
                 rep(c("yes", "no"), c(50, 50)),    # group b: 50 / 50
                 rep(c("yes", "no"), c(60, 40))),   # group c: 60 / 40
               levels = c("no", "yes"))             # ref2 = 1 -> "no" is the baseline level
  )
}


woolf <- function(a, b, cc, dd, conf = 0.95) {
  lor <- log((a * dd) / (b * cc)); se <- sqrt(1/a + 1/b + 1/cc + 1/dd); z <- stats::qnorm(1 - (1 - conf) / 2)
  list(or = exp(lor), inf = exp(lor - z * se), sup = exp(lor + z * se),
       p = 2 * stats::pnorm(-abs(lor / se)))
}


test_that("color_signif = 'ignore' (default) leaves the empirical OR without a CI (byte-unchanged)", {
  t <- tab(or_data(), g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1)
  yes <- t[["yes"]]
  expect_false(tabxplor:::fmt_has_interval(yes))
  expect_true(all(is.na(get_ci_inf(yes))))
  expect_true(all(is.na(get_ci_sup(yes))))
  expect_true(all(is.na(get_pvalue(yes))))
})


test_that("a colour policy gives the empirical OR a Woolf interval (matches the closed form)", {
  t   <- tab(or_data(), g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
             color_signif = "grey_non_signif")
  yes <- t[["yes"]]
  expect_identical(get_scale(yes), "odds_ratio")

  # group c (row 3) vs the reference row (group a, row 1), on {yes, no=ref2 level}
  w <- woolf(a = 60, b = 40, cc = 30, dd = 70)
  expect_equal(get_or(yes)[3],     w$or,  tolerance = 1e-8)
  expect_equal(get_ci_inf(yes)[3], w$inf, tolerance = 1e-8)
  expect_equal(get_ci_sup(yes)[3], w$sup, tolerance = 1e-8)

  # the reference row carries no interval (OR = 1 by construction)
  expect_true(is.na(get_ci_inf(yes)[1]))            # ref row (group a)
  # Phase 16c: a BINARY col_var references the OTHER level, so BOTH columns carry reciprocal ORs +
  # intervals (no column is forced to "1"); ref2 is ignored. The "no" column is the exact reciprocal
  # of "yes", with reciprocal-swapped bounds; only the reference row stays NA.
  no <- t[["no"]]
  expect_identical(get_scale(no), "odds_ratio")
  expect_equal(get_or(no)[3],     1 / w$or,  tolerance = 1e-8)
  expect_equal(get_ci_inf(no)[3], 1 / w$sup, tolerance = 1e-8)
  expect_equal(get_ci_sup(no)[3], 1 / w$inf, tolerance = 1e-8)
  expect_true(is.na(get_ci_inf(no)[1]))             # ref row still NA
})


test_that("stars = TRUE populates the CI-inversion pvalue (dual of the interval)", {
  t   <- tab(or_data(), g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
             color_signif = "grey_non_signif", stars = TRUE)
  yes <- t[["yes"]]
  w   <- woolf(a = 60, b = 40, cc = 30, dd = 70)
  expect_equal(get_pvalue(yes)[3], w$p, tolerance = 1e-8)
  expect_true(is.na(get_pvalue(yes)[1]))            # no p on the reference row
})


test_that("color_signif actually gates the OR colour (greys a big-but-non-significant OR)", {
  # a big observed OR (= 3) on a SMALL sample -> its CI comfortably contains 1 (not significant)
  d <- data.frame(
    g = factor(rep(c("ref", "noisy"), each = 20), levels = c("ref", "noisy")),
    y = factor(c(rep(c("yes", "no"), c(10, 10)),     # ref  : 10 / 10
                 rep(c("yes", "no"), c(15,  5))),     # noisy: 15 /  5  -> OR = 3, n small
               levels = c("no", "yes"))
  )
  slot_ignore <- fmt_color_channels(
    tab(d, g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1)[["yes"]])[[1]][2]
  slot_grey <- fmt_color_channels(
    tab(d, g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
        color_signif = "grey_non_signif")[["yes"]])[[1]][2]
  expect_gt(slot_ignore, 0)   # ignore colours the big observed OR
  expect_identical(slot_grey, 0L)   # grey_non_signif greys it (CI contains 1)
})


test_that("3+ level factor: OR of each level vs the ref2 baseline is the conditional 2x2 OR", {
  # y3: levels d1 (baseline via ref2), d2, d3
  set.seed(1)  # NB: only labels the fixture; counts below are deterministic
  d <- data.frame(
    g  = factor(rep(c("ref", "x"), each = 120)),
    y3 = factor(c(rep(c("d1", "d2", "d3"), c(60, 40, 20)),   # group ref
                  rep(c("d1", "d2", "d3"), c(30, 40, 50))),  # group x
                levels = c("d1", "d2", "d3"))
  )
  t <- tab(d, g, y3, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
           color_signif = "grey_non_signif")

  # OR of d3 vs d1 (baseline), group x (row 2) vs ref row (group ref, row 1):
  #   conditional 2x2 on {d3, d1}: a = 50, b = 30 (x) ; c = 20, d = 60 (ref)
  w <- woolf(a = 50, b = 30, cc = 20, dd = 60)
  d3 <- t[["d3"]]
  expect_identical(get_scale(d3), "odds_ratio")
  expect_equal(get_or(d3)[2],     w$or,  tolerance = 1e-8)   # = (50*60)/(30*20) = 5
  expect_equal(get_ci_inf(d3)[2], w$inf, tolerance = 1e-8)
  expect_equal(get_ci_sup(d3)[2], w$sup, tolerance = 1e-8)

  # the baseline level (d1 = ref2) column has no interval
  expect_true(all(is.na(get_ci_inf(t[["d1"]]))))
})



# --- Phase 16c: binary-factor odds ratios + the OR total column -----------------------------------

test_that("Phase 16c: a binary col_var references the complement (no column forced to '1')", {
  t   <- tab(or_data(), g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
             color_signif = "grey_non_signif")
  yes <- get_or(t[["yes"]]); no <- get_or(t[["no"]])
  # neither level column is a constant OR = 1 (the old forced-ref2 behaviour) ...
  expect_false(all(yes[!is.na(yes)] == 1))
  expect_false(all(no[!is.na(no)]  == 1))
  # ... the two are exact reciprocals, and both carry an OR interval (ref2 is ignored for binary)
  expect_equal(no[!is.na(no)], (1 / yes)[!is.na(yes)], tolerance = 1e-8)
  expect_identical(get_scale(t[["yes"]]), "odds_ratio")
  expect_identical(get_scale(t[["no"]]),  "odds_ratio")
})


test_that("Phase 16c: an OR table's total column shows only the base n, not 100%", {
  t   <- tab(or_data(), g, y, pct = "row", display = "{or}", ref = "first")
  # console/text: the Total cell folds to the base count alone (no "100%" / "{pct}")
  mt  <- tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  d   <- as.character(get_display(mt[["Total"]]))
  expect_true (all(grepl("n_range", d, fixed = TRUE)))
  expect_false(any(grepl("{pct}",   d, fixed = TRUE)))
  # n = "no" drops the meaningless % total column entirely
  t0  <- tab(or_data(), g, y, pct = "row", display = "{or}", ref = "first", n = "no")
  mt0 <- tab_materialize_extras(t0, backend = "text", pvalue = FALSE)
  expect_false("Total" %in% names(mt0))
  # Excel exports only the base-n column, no % total
  mx  <- tab_materialize_extras(t, backend = "xl", pvalue = FALSE)
  expect_false("Total" %in% names(mx))
  expect_true ("n" %in% names(mx))
})



# === SECTION: the arithmetic against base R =======================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())



skip_on_cran()


# === SECTION: Data setup ====================================================

gss <- fx_gss() |> dplyr::filter(race != "Not applicable") |>
  dplyr::mutate(race = droplevels(race))

sw  <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)


testthat::test_that("total row counts match colSums of table()", {
  tabs <- tab(gss, race, marital)
  ct <- table(gss$race, gss$marital)

  marital_levels <- levels(gss$marital)
  tot_row <- tabs |> dplyr::filter(is_totrow(dplyr::pick(where(is_fmt))[[1]]))

  for (m in marital_levels) {
    tab_tot <- tot_row |> dplyr::pull(!!m) |> get_n()
    testthat::expect_equal(tab_tot, as.integer(sum(ct[, m])),
                           label = paste0("total count [", m, "]"))
  }
})


testthat::test_that("row percentages sum to 1 for each row", {
  tabs <- tab(gss, race, marital, pct = "row")

  # Get actual row labels from the table (not factor levels, which may include dropped ones)
  tab_races <- tabs |>
    dplyr::filter(!is_totrow(dplyr::pick(where(is_fmt))[[1]])) |>
    dplyr::pull(race) |>
    as.character()

  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  non_tot_cols <- fmt_cols[!purrr::map_lgl(tabs[fmt_cols], is_totcol)]

  for (r in tab_races) {
    row_data <- tabs |> dplyr::filter(race == r)
    if (nrow(row_data) == 0) next
    pct_vals <- purrr::map_dbl(non_tot_cols, ~ get_pct(row_data[[.]])[1])
    pct_sum <- sum(pct_vals, na.rm = TRUE)
    testthat::expect_equal(pct_sum, 1, tolerance = 1e-10,
                           label = paste0("row pct sum [", r, "]"))
  }
})


testthat::test_that("col percentages sum to 1 for each column (excluding totals)", {
  tabs <- tab(gss, race, marital, pct = "col")
  marital_levels <- levels(gss$marital)

  for (m in marital_levels) {
    col_vec <- tabs |> dplyr::pull(!!m)
    # Non-total rows only
    non_tot <- !is_totrow(col_vec) & !is_tottab(col_vec)
    pct_vals <- get_pct(col_vec[non_tot])
    pct_sum <- sum(pct_vals, na.rm = TRUE)
    if (all(is.na(pct_vals))) next
    testthat::expect_equal(pct_sum, 1, tolerance = 1e-10,
                           label = paste0("col pct sum [", m, "]"))
  }
})


testthat::test_that("overall percentages sum to 1 across all cells", {
  tabs <- tab(gss, race, marital, pct = "all")

  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  non_tot_cols <- fmt_cols[!purrr::map_lgl(tabs[fmt_cols], is_totcol)]

  # Sum pct for non-total rows and non-total columns
  total <- 0
  for (i in seq_len(nrow(tabs))) {
    row_vec <- tabs[[non_tot_cols[1]]][i]
    if (is_totrow(row_vec) || is_tottab(row_vec)) next
    for (col in non_tot_cols) {
      total <- total + get_pct(tabs[[col]][i])
    }
  }
  testthat::expect_equal(total, 1, tolerance = 1e-10)
})


testthat::test_that("weighted row percentages match manual calculation", {
  d <- dplyr::storms |>
    dplyr::mutate(
      status_f  = factor(status),
      category_f = factor(ifelse(is.na(category), "NA", as.character(category)))
    )
  tabs <- tab(d, status_f, category_f, wt = wind, pct = "row")

  cell_pct <- tabs |>
    dplyr::filter(status_f == "hurricane") |>
    dplyr::pull(`4`) |>
    get_pct()

  wn_cell <- sum(d$wind[d$status_f == "hurricane" & d$category_f == "4"],
                 na.rm = TRUE)
  wn_row  <- sum(d$wind[d$status_f == "hurricane"], na.rm = TRUE)
  expected_pct <- wn_cell / wn_row

  testthat::expect_equal(cell_pct, expected_pct, tolerance = 1e-8,
                         label = "weighted row pct [hurricane, 4]")
})


testthat::test_that("col pct diffs equal cell_pct minus ref_col_pct (ref=tot)", {
  tabs <- tab(gss, race, marital, pct = "col", color = "diff")
  ct <- table(gss$race, gss$marital)
  cp <- prop.table(ct, 2)

  # For col pct with ref="tot", the reference column is the Total column
  tot_col_pct <- rowSums(ct) / sum(ct)

  race_levels <- levels(gss$race)
  marital_levels <- levels(gss$marital)

  for (r in race_levels) {
    if (sum(ct[r, ]) == 0) next
    for (m in marital_levels) {
      tab_diff <- tabs |>
        dplyr::filter(race == r) |>
        dplyr::pull(!!m) |>
        get_diff()
      expected_diff <- cp[r, m] - tot_col_pct[r]
      testthat::expect_equal(tab_diff, unname(expected_diff), tolerance = 1e-10,
                             label = paste0("col pct diff [", r, ", ", m, "]"))
    }
  }
})


# === SECTION: Weighted variance ===============================================

# Since 2.0.0 (Phase 2) the weighted variance is derived from moment sums by
# num_derive_stats() (R/tab-agg.R), which replaced the old weighted.var() helper (and its
# weighted.mean double-scan). These tests pin the SAME definitions on the new implementation:
# weighted = ML (population, Sigma-w denominator); unweighted = sample (n-1) like stats::var().

# one-row moment-sum aggregate for a single numeric col_var "v" from raw x [, wt]
make_moment_agg <- function(x, wt = NULL) {
  ok <- !is.na(x)
  if (is.null(wt)) {
    data.table::data.table(
      v_n  = sum(ok),
      v_s1 = sum(as.double(x), na.rm = TRUE),
      v_s2 = sum(as.double(x) * as.double(x), na.rm = TRUE)
    )
  } else {
    data.table::data.table(
      v_n  = sum(ok),
      v_wn = sum(as.integer(ok) * wt, na.rm = TRUE),
      v_s1 = sum(wt * x, na.rm = TRUE),
      v_s2 = sum(wt * x * x, na.rm = TRUE)
    )
  }
}


testthat::test_that("num_derive_stats weighted variance matches the population (ML) formula", {
  x  <- c(10, 20, 30, 40, 50)
  wt <- c(1, 2, 3, 4, 5)
  wmean <- stats::weighted.mean(x, wt)
  out <- tabxplor:::num_derive_stats(make_moment_agg(x, wt), "v", weighted = TRUE)
  testthat::expect_equal(out$v_mean, round(wmean, 10),                          tolerance = 1e-10)
  testthat::expect_equal(out$v_var,  round(sum(wt * (x - wmean)^2) / sum(wt), 10), tolerance = 1e-10)
})


testthat::test_that("num_derive_stats weighted variance with equal weights equals population variance", {
  x  <- c(10, 20, 30, 40, 50)
  wt <- rep(1, 5)
  out <- tabxplor:::num_derive_stats(make_moment_agg(x, wt), "v", weighted = TRUE)
  testthat::expect_equal(out$v_var, round(sum((x - mean(x))^2) / length(x), 10), tolerance = 1e-10)
})


testthat::test_that("num_derive_stats unweighted variance matches stats::var (sample n-1)", {
  x   <- c(10, 20, 30, 40, 50)
  out <- tabxplor:::num_derive_stats(make_moment_agg(x), "v", weighted = FALSE)
  testthat::expect_equal(out$v_mean, mean(x),       tolerance = 1e-10)
  testthat::expect_equal(out$v_var,  stats::var(x), tolerance = 1e-10)
})


testthat::test_that("num_derive_stats reproduces the degenerate NA edges", {
  # unweighted: n = 1 and all-NA both give NA, like stats::var()/mean()
  out1 <- tabxplor:::num_derive_stats(make_moment_agg(c(5, NA, NA)), "v", weighted = FALSE)
  testthat::expect_true(is.na(out1$v_var))
  out2 <- tabxplor:::num_derive_stats(make_moment_agg(c(NA_real_, NA_real_)), "v", weighted = FALSE)
  testthat::expect_true(is.na(out2$v_mean) && is.na(out2$v_var))
  # weighted: a single observation gives var 0 (like weighted.var), never NA
  out3 <- tabxplor:::num_derive_stats(make_moment_agg(c(5, NA), wt = c(2, 3)), "v", weighted = TRUE)
  testthat::expect_equal(out3$v_var, 0)
})


# === SECTION: Z-score formula =================================================

testthat::test_that("zscore_formula matches qnorm", {
  testthat::expect_equal(
    tabxplor:::zscore_formula(0.95),
    stats::qnorm(0.025, lower.tail = FALSE),
    tolerance = 1e-15
  )
  testthat::expect_equal(
    tabxplor:::zscore_formula(0.99),
    stats::qnorm(0.005, lower.tail = FALSE),
    tolerance = 1e-15
  )
  testthat::expect_equal(
    tabxplor:::zscore_formula(0.90),
    stats::qnorm(0.05, lower.tail = FALSE),
    tolerance = 1e-15
  )
})


# === SECTION: Proportion confidence intervals =================================

testthat::test_that("cell CI for proportions matches DescTools::BinomCI wilson", {
  testthat::skip_if_not_installed("DescTools")
  tabs <- tab(gss, race, marital, pct = "row", ci = "cell", conf_level = 0.95)
  ct <- table(gss$race, gss$marital)

  test_cells <- list(
    c("White", "Married"),
    c("Black", "Never married"),
    c("Other", "Divorced")
  )

  for (cell in test_cells) {
    r <- cell[1]; m <- cell[2]
    tab_ci <- tabs |>
      dplyr::filter(race == r) |>
      dplyr::pull(!!m) |>
      get_ci()

    successes <- ct[r, m]
    n_total   <- sum(ct[r, ])
    bci <- DescTools::BinomCI(successes, n_total,
                              conf.level = 0.95, method = "wilson")
    expected_ci <- bci[, "upr.ci"] - bci[, "est"]

    testthat::expect_equal(tab_ci, unname(expected_ci), tolerance = 1e-6,
                           label = paste0("prop CI [", r, ", ", m, "]"))

    # Phase 3a: the real asymmetric lower bound is now stored (was discarded pre-3a).
    col <- tabs |> dplyr::filter(race == r) |> dplyr::pull(!!m)
    testthat::expect_equal(get_ci_inf(col), unname(bci[, "lwr.ci"]), tolerance = 1e-6,
                           label = paste0("prop CI inf [", r, ", ", m, "]"))
    testthat::expect_equal(get_ci_sup(col), unname(bci[, "upr.ci"]), tolerance = 1e-6,
                           label = paste0("prop CI sup [", r, ", ", m, "]"))
  }
})


testthat::test_that("cell CI ci_method = c(cell = 'wald') matches p +- z*sqrt(p(1-p)/n) (Phase 7g)", {
  tabs <- tab(gss, race, marital, pct = "row", ci = "cell", conf_level = 0.95,
              ci_method = c(cell = "wald"))
  ct   <- table(gss$race, gss$marital)
  z    <- stats::qnorm(0.975)

  for (cell in list(c("White", "Married"), c("Black", "Never married"), c("Other", "Divorced"))) {
    r <- cell[1]; m <- cell[2]
    col <- tabs |> dplyr::filter(race == r) |> dplyr::pull(!!m)

    p    <- ct[r, m] / sum(ct[r, ])
    n    <- sum(ct[r, ])
    half <- z * sqrt(p * (1 - p) / n)

    testthat::expect_equal(get_ci_inf(col), p - half, tolerance = 1e-9,
                           label = paste0("wald CI inf [", r, ", ", m, "]"))
    testthat::expect_equal(get_ci_sup(col), p + half, tolerance = 1e-9,
                           label = paste0("wald CI sup [", r, ", ", m, "]"))
  }

  # Cell CIs carry no p-value, so stars stay NA regardless of the method.
  cn <- tabs |> dplyr::filter(race == "White") |> dplyr::pull("Married")
  testthat::expect_true(all(is.na(get_pvalue(cn))))
})


testthat::test_that("the released method_cell / method_diff are soft-deprecated aliases of ci_method", {
  # Phase 18z16-iiiii: five `method_*` arguments folded into ONE named vector. `method_cell` and
  # `method_diff` are CRAN-released (1.2.0), so they keep working with one nudge; `method_ratio` /
  # `method_mean_diff` / `method_mean_ratio` were 2.0.0-new and are simply gone.
  withr::local_options(lifecycle_verbosity = "warning")
  testthat::expect_warning(
    old <- tab(gss, race, marital, pct = "row", ci = "cell", method_cell = "wald"), "deprecated")
  new <- tab(gss, race, marital, pct = "row", ci = "cell", ci_method = c(cell = "wald"))
  testthat::expect_equal(old, new)
  # Phase 19b: the resolved method is stamped on the COLUMNS that used it
  testthat::expect_true(all(get_ci_method(new)[purrr::map_lgl(new, is_fmt)] == "wald"))
  # the vector is PARTIAL: an unnamed slot keeps its default, exactly like `ref` / `pct`.
  # Phase 20c: the 5th slot is `model`, tab_reg()'s -- one grammar, one vocabulary, both producers.
  testthat::expect_identical(unname(tabxplor:::resolve_ci_method(c(cell = "wald"))),
                             c("wald", "newcombe", "welch", "robust", "wald"))
  testthat::expect_identical(tabxplor:::ci_slots_of("reg"), "model")
  # and one validator answers for every entry point
  testthat::expect_error(tab(gss, race, marital, ci_method = c(diff = "wilson")), "newcombe")
  testthat::expect_error(tab(gss, race, marital, ci_method = c(nope = "wald")), "Unknown")
  testthat::expect_error(tab(gss, race, marital, ci_method = "wald"), "must be named")
})


testthat::test_that("wald and wilson cell CIs differ but both centre near the estimate (Phase 7g)", {
  wilson <- tab(gss, race, marital, pct = "row", ci = "cell", ci_method = c(cell = "wilson"))
  wald   <- tab(gss, race, marital, pct = "row", ci = "cell", ci_method = c(cell = "wald"))
  col_wi <- wilson |> dplyr::filter(race == "Black") |> dplyr::pull("Divorced")
  col_wa <- wald   |> dplyr::filter(race == "Black") |> dplyr::pull("Divorced")
  # Different intervals (wald symmetric about p, wilson shifted) but same point estimate.
  testthat::expect_false(isTRUE(all.equal(get_ci_inf(col_wi), get_ci_inf(col_wa))))
  testthat::expect_equal(get_pct(col_wi), get_pct(col_wa))
})


testthat::test_that("diff CI for proportions (method='ac') matches DescTools::BinomDiffCI ac", {
  testthat::skip_if_not_installed("DescTools")
  # Phase 3a: AC is now the expert opt-in (default is Newcombe, tested below). get_ci() is the
  # upper arm (ci_sup - diff), matching DescTools' upr.ci - est.
  tabs <- tab(gss, race, marital, pct = "row", ci = "ref", conf_level = 0.95,
              ci_method = c(diff = "ac"), stars = FALSE)
  ct <- table(gss$race, gss$marital)

  test_cells <- list(
    c("White", "Married"),
    c("Black", "Never married")
  )

  for (cell in test_cells) {
    r <- cell[1]; m <- cell[2]
    tab_ci <- tabs |>
      dplyr::filter(race == r) |>
      dplyr::pull(!!m) |>
      get_ci()

    if (is.na(tab_ci)) next

    x1 <- ct[r, m]
    n1 <- sum(ct[r, ])
    x2 <- sum(ct[, m])
    n2 <- sum(ct)

    bdci <- DescTools::BinomDiffCI(x1 = x1, n1 = n1, x2 = x2, n2 = n2,
                                   conf.level = 0.95, method = "ac")
    expected_ci <- bdci[, "upr.ci"] - bdci[, "est"]

    testthat::expect_equal(tab_ci, unname(expected_ci), tolerance = 1e-4,
                           label = paste0("prop diff CI [", r, ", ", m, "]"))
  }
})


# Phase 3a new-default parity: the proportion-diff interval is Newcombe method 10, matching
# DescTools::BinomDiffCI(method = "score") on BOTH bounds; and the per-cell pvalue (universal
# CI-inclusion) agrees with the bracket's own 0-exclusion at each star level.
testthat::test_that("diff CI for proportions (default) matches DescTools BinomDiffCI score", {
  testthat::skip_if_not_installed("DescTools")
  tabs <- tab(gss, race, marital, pct = "row", ci = "ref", conf_level = 0.95)
  ct <- table(gss$race, gss$marital)
  for (cell in list(c("White", "Married"), c("Black", "Never married"), c("Other", "Divorced"))) {
    r <- cell[1]; m <- cell[2]
    col <- tabs |> dplyr::filter(race == r) |> dplyr::pull(!!m)
    if (is.na(get_ci_sup(col))) next
    x1 <- ct[r, m]; n1 <- sum(ct[r, ]); x2 <- sum(ct[, m]); n2 <- sum(ct)
    bd <- DescTools::BinomDiffCI(x1, n1, x2, n2, conf.level = 0.95, method = "score")
    testthat::expect_equal(get_ci_inf(col), unname(bd[, "lwr.ci"]), tolerance = 1e-6,
                           label = paste0("Newcombe inf [", r, ", ", m, "]"))
    testthat::expect_equal(get_ci_sup(col), unname(bd[, "upr.ci"]), tolerance = 1e-6,
                           label = paste0("Newcombe sup [", r, ", ", m, "]"))
  }
})


testthat::test_that("mean diff CI (stars on) matches Welch t.test", {
  d <- sw |> dplyr::filter(!is.na(height) & !is.na(sex))
  tabs <- tab_num(d, sex, height, na = "drop", ci = "ref", conf_level = 0.95, stars = TRUE)
  ref <- d |> dplyr::pull(height)                 # total row = reference (ref = "tot")
  for (s in levels(d$sex)) {
    col <- tabs |> dplyr::filter(sex == s) |> dplyr::pull(height)
    if (length(col) == 0) next
    grp <- d |> dplyr::filter(sex == s) |> dplyr::pull(height)
    if (length(grp) <= 1 || is.na(get_ci_sup(col))) next
    tt <- stats::t.test(grp, ref, var.equal = FALSE)  # cell vs total
    hw <- as.numeric(diff(tt$conf.int) / 2)
    testthat::expect_equal(get_ci(col), hw, tolerance = 1e-4, label = paste0("Welch hw [", s, "]"))
    testthat::expect_equal(get_pvalue(col), tt$p.value, tolerance = 1e-4,
                           label = paste0("Welch p [", s, "]"))
  }
})


testthat::test_that("vectorised chi2 applies the Yates correction on 2x2 like chisq.test", {
  d <- gss |>
    dplyr::filter(marital %in% c("Married", "Divorced"), race %in% c("Black", "White")) |>
    dplyr::mutate(marital = forcats::fct_drop(marital), race = forcats::fct_drop(race))
  tabs     <- tab(d, race, marital, pct = "row", test = TRUE)
  chi2_row <- get_test(tabs) |> dplyr::filter(.data$test == "chi2")
  ref      <- suppressWarnings(stats::chisq.test(table(d$race, d$marital)))  # Yates on 2x2 (default)

  testthat::expect_equal(chi2_row$df1, 1)
  testthat::expect_equal(chi2_row$statistic, unname(ref$statistic), tolerance = 1e-9)
  testthat::expect_equal(chi2_row$pvalue,    ref$p.value,           tolerance = 1e-10)
})


testthat::test_that("chi2 is unaffected by the base count (now a display-only column)", {
  ref_tab  <- tab(gss, race, marital, pct = "row", test = TRUE)
  ref_row  <- get_test(ref_tab) |> dplyr::filter(.data$test == "chi2")
  # Phase 10i-B: the base count is display-only, so `tab(n = "range")` builds the SAME core table chi2 sees.
  addn_tab <- tab(gss, race, marital, pct = "row", n = "range", test = TRUE)
  addn_row <- get_test(addn_tab) |> dplyr::filter(.data$test == "chi2")

  testthat::expect_equal(addn_row$statistic, ref_row$statistic, tolerance = 1e-6)
  testthat::expect_equal(addn_row$pvalue,    ref_row$pvalue,    tolerance = 1e-9)
})


testthat::test_that("ANOVA classic F matches stats::oneway.test(var.equal = TRUE)", {
  d    <- gss |> dplyr::filter(!is.na(tvhours))
  tabs <- tab(d, marital, tvhours, pct = "row", test = TRUE)
  cl   <- get_test(tabs) |> dplyr::filter(.data$test == "F_classic")
  oc   <- stats::oneway.test(tvhours ~ marital, data = d, var.equal = TRUE)

  testthat::expect_equal(cl$statistic, unname(oc$statistic),            tolerance = 1e-8)
  testthat::expect_equal(cl$df1,       unname(oc$parameter[["num df"]]),   tolerance = 1e-8)
  testthat::expect_equal(cl$df2,       unname(oc$parameter[["denom df"]]), tolerance = 1e-8)
  testthat::expect_equal(cl$pvalue,    oc$p.value,                      tolerance = 1e-8)
})


testthat::test_that("ANOVA Welch F matches oneway.test on another variable (3 groups)", {
  d    <- gss |> dplyr::filter(!is.na(tvhours), race != "Not applicable") |>
    dplyr::mutate(race = forcats::fct_drop(race))
  tabs <- tab(d, race, tvhours, pct = "row", test = TRUE)
  w    <- get_test(tabs) |> dplyr::filter(.data$test == "F_welch")
  ow   <- stats::oneway.test(tvhours ~ race, data = d, var.equal = FALSE)

  testthat::expect_equal(w$statistic, unname(ow$statistic), tolerance = 1e-8)
  testthat::expect_equal(w$df2,       unname(ow$parameter[["denom df"]]), tolerance = 1e-8)
  testthat::expect_equal(w$pvalue,    ow$p.value,           tolerance = 1e-8)
})


testthat::test_that("the tabxplor.anova option selects the displayed F (welch vs classic) (Phase 7g)", {
  d    <- gss |> dplyr::filter(!is.na(tvhours), race != "Not applicable") |>
    dplyr::mutate(race = forcats::fct_drop(race))
  tt   <- get_test(tab(d, race, tvhours, pct = "row", test = TRUE))

  welch   <- test_display_rows(tt, anova = "welch")
  classic <- test_display_rows(tt, anova = "classic")
  testthat::expect_true(all(welch$test   %in% c("chi2", "F_welch")))
  testthat::expect_true(all(classic$test %in% c("chi2", "F_classic")))
  # the two F variants have different statistics/p-values on unequal-variance groups
  testthat::expect_false(isTRUE(all.equal(welch$statistic, classic$statistic)))
})


# === SECTION: Odds ratios =====================================================

testthat::test_that("OR calculation produces finite numeric values", {
  tabs <- tab(gss, race, marital, pct = "row", display = "{or}", ref = "first")

  # Check that OR values exist and are numeric for non-total cells
  race_levels <- as.character(unique(
    tabs$race[!is_totrow(tabs[[names(tabs)[purrr::map_lgl(tabs, is_fmt)][1]]])]
  ))

  for (r in race_levels) {
    or_val <- tabs |>
      dplyr::filter(race == r) |>
      dplyr::pull(Married) |>
      get_or()
    testthat::expect_true(is.numeric(or_val),
                          label = paste0("OR numeric [", r, "]"))
  }
})


testthat::test_that("OR ref column values are all 1", {
  tabs <- tab(gss, race, marital, pct = "row", display = "{or}", ref = "first")

  # The ref column (first non-total column, default ref2="first") should have OR=1
  # because RR = cell_pct / ref_col_pct = itself / itself = 1 for the ref column
  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  non_tot_cols <- fmt_cols[!purrr::map_lgl(tabs[fmt_cols], is_totcol)]
  ref_col_name <- non_tot_cols[1]

  or_vals <- get_or(tabs[[ref_col_name]])
  # Non-total, non-tottab rows should have OR = 1 for the reference column
  non_tot_mask <- !is_totrow(tabs[[ref_col_name]]) & !is_tottab(tabs[[ref_col_name]])
  ref_ors <- or_vals[non_tot_mask]
  ref_ors <- ref_ors[!is.na(ref_ors)]

  if (length(ref_ors) > 0) {
    testthat::expect_true(all(abs(ref_ors - 1) < 1e-8),
                          label = "OR ref column should all be 1")
  }
})


# === SECTION: Tab with tab_vars (grouped tables) ==============================

testthat::test_that("grouped table counts match filtered table() for each group", {
  gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
  tabs <- tab(gss_sub, race, marital, year)

  ct_2000 <- table(
    gss_sub$race[gss_sub$year == 2000],
    gss_sub$marital[gss_sub$year == 2000]
  )

  tab_n <- tabs |>
    dplyr::filter(year == 2000 & race == "White") |>
    dplyr::pull(Married) |>
    get_n()

  testthat::expect_equal(tab_n, as.integer(ct_2000["White", "Married"]),
                         label = "grouped count [2000, White, Married]")

  ct_2014 <- table(
    gss_sub$race[gss_sub$year == 2014],
    gss_sub$marital[gss_sub$year == 2014]
  )

  tab_n2 <- tabs |>
    dplyr::filter(year == 2014 & race == "Black") |>
    dplyr::pull(`Never married`) |>
    get_n()

  testthat::expect_equal(tab_n2, as.integer(ct_2014["Black", "Never married"]),
                         label = "grouped count [2014, Black, Never married]")
})


# === SECTION: Supplementary numeric columns ===================================

testthat::test_that("supplementary numeric column means match base R", {
  # sup_cols is soft-deprecated (Phase 7a) but must keep computing correct means.
  tabs <- suppressWarnings(tab(gss, race, marital, pct = "row", sup_cols = tvhours))

  # sup_cols mean: tvhours mean for each row_var level (race), across all marital
  tab_mean <- tabs |>
    dplyr::filter(race == "White") |>
    dplyr::pull(tvhours) |>
    get_mean()

  expected <- mean(gss$tvhours[gss$race == "White"], na.rm = TRUE)
  testthat::expect_equal(tab_mean, expected, tolerance = 1e-4,
                         label = "sup_col mean [White, tvhours]")
})


# === SECTION: Consistency checks ==============================================

testthat::test_that("total column n equals sum of non-total column n per row", {
  tabs <- tab(gss, race, marital)

  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  tot_cols <- fmt_cols[purrr::map_lgl(tabs[fmt_cols], is_totcol)]
  non_tot_cols <- fmt_cols[!purrr::map_lgl(tabs[fmt_cols], is_totcol)]

  if (length(tot_cols) > 0) {
    for (i in seq_len(nrow(tabs))) {
      row_sum <- sum(purrr::map_int(non_tot_cols, ~ get_n(tabs[[.]][i])))
      tot_n   <- get_n(tabs[[tot_cols[1]]][i])
      testthat::expect_equal(row_sum, tot_n, label = paste0("row sum [", i, "]"))
    }
  }
})


testthat::test_that("all pct values are between 0 and 1 (inclusive)", {
  tabs <- tab(gss, race, marital, pct = "row")

  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  for (col in fmt_cols) {
    pcts <- get_pct(tabs[[col]])
    pcts <- pcts[!is.na(pcts)]
    if (length(pcts) == 0) next
    testthat::expect_true(all(pcts >= 0 & pcts <= 1),
                          label = paste0("pct in [0,1] for col ", col))
  }
})



# --- Phase 14a: a row_var with exactly ONE non-total row -----------------------------------------
# vapply() returns a MATRIX only when FUN.VALUE has length > 1, so a single non-total row made
# chi2_compute_test()'s `M` a plain vector, ncol(M) NULL, and every rep(times = ncM) die with
# "invalid 'times' argument". It surfaced through mirai ("In index: 3 ... error in rep()") but was
# never parallel-specific -- the serial map hits the identical line.

testthat::test_that("test = TRUE survives a row_var with a single non-total row", {
  d <- dplyr::mutate(fx_gss(), one = factor("only"))
  testthat::expect_no_error(t <- tab(d, one, marital, pct = "row", test = TRUE))
  te <- get_test(t)
  # a 1-row table is degenerate (df = 0) -> NA, like any other degenerate table
  testthat::expect_equal(nrow(te), 1L)
  testthat::expect_true(is.na(te$pvalue))

  # and inside a multi-row_var table, where only ONE row_var is degenerate
  testthat::expect_no_error(tab(d, c(race, one), c(marital, relig), pct = "row", test = TRUE))
  # with a numeric col_var alongside (the ANOVA arm)
  testthat::expect_no_error(tab(d, c(race, one), c(marital, tvhours), pct = "row", test = TRUE))
})


testthat::test_that("a normal chi2 still matches stats::chisq.test after the single-row fix", {
  t  <- tab(fx_gss(), race, marital, pct = "row", test = TRUE)
  te <- get_test(t)
  m  <- table(fx_gss()$race, fx_gss()$marital)
  m  <- m[rowSums(m) > 0, colSums(m) > 0, drop = FALSE]   # tab_chi2 drops empty rows/cols
  ref <- suppressWarnings(stats::chisq.test(m))
  testthat::expect_equal(te$statistic, unname(ref$statistic))
  testthat::expect_equal(te$df1, unname(ref$parameter))
  testthat::expect_equal(te$pvalue, ref$p.value)
})



# --- Phase 14a: chi2 renamed test ---------------------------------------------------------------

testthat::test_that("tab(chi2 = ) is soft-deprecated but identical to tab(test = )", {
  lifecycle::expect_deprecated(
    old <- tab(fx_gss(), race, marital, pct = "row", chi2 = TRUE))
  new <- tab(fx_gss(), race, marital, pct = "row", test = TRUE)
  testthat::expect_equal(old, new)
  # the default path never nudges
  testthat::expect_no_condition(tab(fx_gss(), race, marital, pct = "row"),
                                class = "lifecycle_warning_deprecated")
})
