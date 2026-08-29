# PURPOSE: the aggregate core and the interval engine: the numbers, against base R and the reference tests.
# ROLE: the shipped CONTRACT for R/tab-agg.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the interval engine vs t-test and glm ===============================================

testthat::test_that("ci_pivot() p-value and interval equal a one-sample t-test", {
  set.seed(101)
  for (i in seq_len(25)) {
    n <- sample(3:60, 1)
    x <- stats::rnorm(n, mean = 0.5, sd = 2)
    res <- ci_pivot(mean(x), stats::sd(x) / sqrt(n), df = n - 1,
                    conf_level = 0.95, want_p = TRUE)
    tt <- stats::t.test(x)                       # H0: mean = 0
    testthat::expect_equal(res$pvalue, tt$p.value, tolerance = 1e-10)
    testthat::expect_equal(c(res$inf, res$sup), as.numeric(tt$conf.int), tolerance = 1e-10)
  }
})


testthat::test_that("ci_mean_diff2() p-value and interval equal a Welch two-sample t-test", {
  set.seed(202)
  for (i in seq_len(25)) {
    n1 <- sample(4:50, 1); n2 <- sample(4:50, 1)
    x <- stats::rnorm(n1, mean = 5, sd = 2)
    y <- stats::rnorm(n2, mean = 6, sd = 4)
    res <- ci_mean_diff2(mean(x), stats::var(x), n1, mean(y), stats::var(y), n2,
                         conf_level = 0.95, want_p = TRUE)
    tt <- stats::t.test(x, y, var.equal = FALSE)  # Welch, H0: equal means
    testthat::expect_equal(res$pvalue, tt$p.value, tolerance = 1e-10)
    testthat::expect_equal(c(res$inf, res$sup), as.numeric(tt$conf.int), tolerance = 1e-10)
  }
})


# === SECTION: 14v-ii engines (student diff, ratio-of-means, Woolf OR) ================

testthat::test_that("ci_mean_diff2(method='student') equals a pooled two-sample t-test = OLS", {
  set.seed(212)
  for (i in seq_len(20)) {
    n1 <- sample(4:50, 1); n2 <- sample(4:50, 1)
    x <- stats::rnorm(n1, 5, 2); y <- stats::rnorm(n2, 6, 2)
    res <- ci_mean_diff2(mean(x), stats::var(x), n1, mean(y), stats::var(y), n2,
                         conf_level = 0.95, want_p = TRUE, method = "student")
    tt <- stats::t.test(x, y, var.equal = TRUE)     # pooled Student, = the two-group OLS coef
    testthat::expect_equal(res$pvalue, tt$p.value, tolerance = 1e-10)
    testthat::expect_equal(c(res$inf, res$sup), as.numeric(tt$conf.int), tolerance = 1e-10)
  }
})


testthat::test_that("ci_mean_ratio() matches exp(log(R) +/- q*se) for all three methods", {
  set.seed(213)
  z <- stats::qnorm(0.975)
  for (i in seq_len(20)) {
    n1 <- sample(20:200, 1); n2 <- sample(20:200, 1)
    x <- abs(stats::rnorm(n1, 5, 2)) + 0.5; y <- abs(stats::rnorm(n2, 4, 2)) + 0.5
    m1 <- mean(x); v1 <- stats::var(x); m2 <- mean(y); v2 <- stats::var(y)
    lr <- log(m1 / m2)
    # robust (delta on log, each group's own variance) -> z
    rob <- ci_mean_ratio(m1, v1, n1, m2, v2, n2, method = "robust")
    se_r <- sqrt((v1 / n1) / m1^2 + (v2 / n2) / m2^2)
    testthat::expect_equal(c(rob$inf, rob$sup), exp(lr + c(-1, 1) * z * se_r), tolerance = 1e-9)
    # naive poisson (S = m*n) -> z
    poi <- ci_mean_ratio(m1, v1, n1, m2, v2, n2, method = "poisson")
    se_p <- sqrt(1 / (m1 * n1) + 1 / (m2 * n2))
    testthat::expect_equal(c(poi$inf, poi$sup), exp(lr + c(-1, 1) * z * se_p), tolerance = 1e-9)
    # quasipoisson (poisson * sqrt(pooled phi)) -> t(n1+n2-2)
    qp  <- ci_mean_ratio(m1, v1, n1, m2, v2, n2, method = "quasipoisson", want_p = TRUE)
    phi <- ((n1 - 1) * v1 / m1 + (n2 - 1) * v2 / m2) / (n1 + n2 - 2)
    se_q <- se_p * sqrt(phi); crit <- stats::qt(0.975, df = n1 + n2 - 2)
    testthat::expect_equal(c(qp$inf, qp$sup), exp(lr + c(-1, 1) * crit * se_q), tolerance = 1e-9)
    testthat::expect_equal(qp$pvalue, 2 * stats::pt(-abs(lr / se_q), df = n1 + n2 - 2), tolerance = 1e-9)
  }
})


testthat::test_that("ci_or() is Woolf's log-OR Wald and matches a logistic regression", {
  # hand Woolf on a 2x2
  a <- 30; b <- 70; cc <- 45; d <- 55
  r <- ci_or(a, b, cc, d)
  lor <- log((a * d) / (b * cc)); se <- sqrt(1 / a + 1 / b + 1 / cc + 1 / d)
  z <- stats::qnorm(0.975)
  testthat::expect_equal(c(r$inf, r$sup), exp(lor + c(-1, 1) * z * se), tolerance = 1e-12)
  testthat::expect_equal(r$pvalue, 2 * stats::pnorm(-abs(lor / se)), tolerance = 1e-12)
  # a saturated logit on the 2x2 reproduces the same OR + Wald interval (confint.default = z-Wald)
  dd  <- data.frame(y = c(1, 0, 1, 0), g = factor(c("x", "x", "r", "r")), w = c(a, b, cc, d))
  fit <- stats::glm(y ~ g, weights = w, family = stats::binomial(), data = dd)
  testthat::expect_equal(unname(exp(stats::coef(fit)["gx"])), (a * d) / (b * cc), tolerance = 1e-6)
  # cross-check vs the logit Wald interval (a hair looser -- confint.default's SE has a tiny
  # finite-sample difference from the closed-form Woolf SE checked exactly above).
  testthat::expect_equal(c(r$inf, r$sup),
                         unname(exp(stats::confint.default(fit)["gx", ])), tolerance = 1e-4)
})


# Cases chosen to give a moderate p (roughly 0.002 - 0.25) so 1 - p is a safe confidence level.
prop_cases <- list(
  c(35, 50, 20, 50), c(30, 50, 20, 50), c(26, 50, 20, 50),
  c(40, 80, 28, 80), c(12, 30, 18, 30), c(60, 100, 45, 100)
)


# === SECTION: stars agree with the p-value threshold (universal inclusion) ============

testthat::test_that("get_stars() maps p-values to the documented thresholds", {
  x <- fmt(n = rep(30L, 5), scale = "points", pct_type = "row", pct = rep(0.5, 5), pvalue = c(0.20, 0.08, 0.03, 0.005, NA))
  testthat::expect_identical(get_stars(x), c("", "*", "**", "***", ""))
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



# === SECTION: the Katz log-RR interval ============================================================

d <- fx_gss()


katz_hand <- function(p1, n1, p2, n2, conf_level = 0.95) {
  se <- sqrt((1 - p1) / (n1 * p1) + (1 - p2) / (n2 * p2))
  z  <- stats::qnorm(1 - (1 - conf_level) / 2)
  list(inf = exp(log(p1 / p2) - z * se), sup = exp(log(p1 / p2) + z * se),
       pvalue = 2 * stats::pnorm(-abs(log(p1 / p2) / se)))
}



# --- the engine ---------------------------------------------------------------------------

testthat::test_that("ci_katz_rr matches a hand-computed log-RR interval + its Wald dual", {
  p1 <- c(0.30, 0.55, 0.10); n1 <- c(400, 250, 900)
  p2 <- c(0.25, 0.50, 0.12); n2 <- c(800, 800, 800)
  got <- ci_katz_rr(p1, n1, p2, n2, conf_level = 0.95, want_p = TRUE)
  exp <- katz_hand(p1, n1, p2, n2, 0.95)
  testthat::expect_equal(got$inf,    exp$inf)
  testthat::expect_equal(got$sup,    exp$sup)
  testthat::expect_equal(got$pvalue, exp$pvalue)
})


testthat::test_that("CI <-> stars stay exact duals on the ratio scale", {
  checked <- 0L
  for (cl in c(0.90, 0.95, 0.99)) {
    tt <- tab(d, c(race, marital), c(partyid, relig), pct = "row", color = "ratio",
              color_signif = "grey_non_signif", stars = TRUE, conf_level = cl,
              output_list = TRUE)
    for (one in tt) for (cc in one[purrr::map_lgl(one, is_fmt)]) {
      if (!identical(get_scale(cc), "pct_ratio")) next
      p <- get_pvalue(cc); lo <- get_ci_inf(cc); hi <- get_ci_sup(cc)
      k <- !is.na(p) & !is.na(lo)
      checked <- checked + sum(k)
      testthat::expect_identical(p[k] < 1 - cl, lo[k] > 1 | hi[k] < 1)
    }
  }
  testthat::expect_gt(checked, 100L)      # the duality check above must not be vacuous
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



# === SECTION: the arithmetic against base R =======================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())



skip_on_cran()


# === SECTION: Data setup ====================================================

gss <- fx_gss() |> dplyr::filter(race != "Not applicable") |>
  dplyr::mutate(race = droplevels(race))

sw  <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)


# === SECTION: Unweighted counts ==============================================

testthat::test_that("unweighted counts match base R table()", {
  tabs <- tab(gss, race, marital)
  ct <- table(gss$race, gss$marital)

  race_levels <- levels(gss$race)
  marital_levels <- levels(gss$marital)

  for (r in race_levels) {
    if (sum(ct[r, ]) == 0) next
    for (m in marital_levels) {
      tab_n <- tabs |>
        dplyr::filter(race == r) |>
        dplyr::pull(!!m) |>
        get_n()
      testthat::expect_equal(tab_n, as.integer(ct[r, m]),
                             label = paste0("count [", r, ", ", m, "]"))
    }
  }
})


# === SECTION: Row percentages =================================================

testthat::test_that("row percentages match prop.table(, 1)", {
  tabs <- tab(gss, race, marital, pct = "row")
  ct <- table(gss$race, gss$marital)
  rp <- prop.table(ct, 1)

  race_levels <- levels(gss$race)
  marital_levels <- levels(gss$marital)

  for (r in race_levels) {
    if (sum(ct[r, ]) == 0) next
    for (m in marital_levels) {
      tab_pct <- tabs |>
        dplyr::filter(race == r) |>
        dplyr::pull(!!m) |>
        get_pct()
      testthat::expect_equal(tab_pct, unname(rp[r, m]), tolerance = 1e-10,
                             label = paste0("row pct [", r, ", ", m, "]"))
    }
  }
})


# === SECTION: Column percentages ==============================================

testthat::test_that("col percentages match prop.table(, 2)", {
  tabs <- tab(gss, race, marital, pct = "col")
  ct <- table(gss$race, gss$marital)
  cp <- prop.table(ct, 2)

  race_levels <- levels(gss$race)
  marital_levels <- levels(gss$marital)

  for (r in race_levels) {
    if (sum(ct[r, ]) == 0) next
    for (m in marital_levels) {
      tab_pct <- tabs |>
        dplyr::filter(race == r) |>
        dplyr::pull(!!m) |>
        get_pct()
      testthat::expect_equal(tab_pct, unname(cp[r, m]), tolerance = 1e-10,
                             label = paste0("col pct [", r, ", ", m, "]"))
    }
  }
})


# === SECTION: Overall percentages =============================================

testthat::test_that("overall percentages match prop.table()", {
  tabs <- tab(gss, race, marital, pct = "all")
  ct <- table(gss$race, gss$marital)
  ap <- prop.table(ct)

  race_levels <- levels(gss$race)
  marital_levels <- levels(gss$marital)

  for (r in race_levels) {
    if (sum(ct[r, ]) == 0) next
    for (m in marital_levels) {
      tab_pct <- tabs |>
        dplyr::filter(race == r) |>
        dplyr::pull(!!m) |>
        get_pct()
      testthat::expect_equal(tab_pct, unname(ap[r, m]), tolerance = 1e-10,
                             label = paste0("all pct [", r, ", ", m, "]"))
    }
  }
})


# === SECTION: Weighted counts and percentages =================================

testthat::test_that("weighted counts match manual sum of weights", {
  d <- dplyr::storms |>
    dplyr::mutate(
      status_f  = factor(status),
      category_f = factor(ifelse(is.na(category), "NA", as.character(category)))
    )
  tabs <- tab(d, status_f, category_f, wt = wind)

  cell_wn <- tabs |>
    dplyr::filter(status_f == "hurricane") |>
    dplyr::pull(`4`) |>
    get_wn()

  expected_wn <- sum(d$wind[d$status_f == "hurricane" & d$category_f == "4"],
                     na.rm = TRUE)
  testthat::expect_equal(cell_wn, expected_wn, tolerance = 1e-8,
                         label = "weighted count [hurricane, 4]")
})


# === SECTION: Percentage differences ==========================================

testthat::test_that("row pct diffs equal cell_pct minus total_row_pct", {
  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  ct <- table(gss$race, gss$marital)
  rp <- prop.table(ct, 1)

  # Total row pct = overall column proportions (marginal)
  tot_pct <- colSums(ct) / sum(ct)

  race_levels <- levels(gss$race)
  marital_levels <- levels(gss$marital)

  for (r in race_levels) {
    if (sum(ct[r, ]) == 0) next
    for (m in marital_levels) {
      tab_diff <- tabs |>
        dplyr::filter(race == r) |>
        dplyr::pull(!!m) |>
        get_diff()
      expected_diff <- rp[r, m] - tot_pct[m]
      testthat::expect_equal(tab_diff, unname(expected_diff), tolerance = 1e-10,
                             label = paste0("row pct diff [", r, ", ", m, "]"))
    }
  }
})


# === SECTION: Means and numeric statistics ====================================

testthat::test_that("tab_num means match base R mean()", {
  tabs <- tab_num(sw, sex, height, na = "drop")
  sex_levels <- levels(sw$sex)

  for (s in sex_levels) {
    tab_row <- tabs |> dplyr::filter(sex == s)
    if (nrow(tab_row) == 0) next
    tab_mean <- tab_row |> dplyr::pull(height) |> get_mean()

    d <- sw |> dplyr::filter(!is.na(height) & !is.na(sex) & sex == s)
    if (nrow(d) == 0) next
    expected_mean <- mean(d$height, na.rm = TRUE)

    testthat::expect_equal(tab_mean, expected_mean, tolerance = 1e-6,
                           label = paste0("mean height [", s, "]"))
  }
})


testthat::test_that("tab_num variance matches stats::var() (sample, n-1 denominator)", {
  # DESIGN: Unweighted tab_num reports the sample (Bessel-corrected, n-1) variance; weighted
  # tab_num reports the ML (population, Sigma-w denominator) variance. Since 2.0.0 both are
  # derived from moment sums by num_derive_stats() (was: stats::var() / weighted.var()).
  tabs <- tab_num(sw, sex, height, na = "drop")
  sex_levels <- levels(sw$sex)

  for (s in sex_levels) {
    tab_row <- tabs |> dplyr::filter(sex == s)
    if (nrow(tab_row) == 0) next
    tab_var <- tab_row |> dplyr::pull(height) |> get_var()

    d <- sw |> dplyr::filter(!is.na(height) & !is.na(sex) & sex == s)
    if (nrow(d) <= 1) next
    # stats::var uses n-1 denominator (sample variance)
    expected_var <- stats::var(d$height, na.rm = TRUE)

    testthat::expect_equal(tab_var, expected_var, tolerance = 1e-4,
                           label = paste0("variance height [", s, "]"))
  }
})


# Since 2.0.0 (Phase 2) the numeric `diff` field is a real DIFFERENCE (mean - ref_mean) and the
# cell/reference RATIO moved to the new `ratio` field (was the old numeric-`diff` overload).
testthat::test_that("mean diff is a difference and mean ratio is a ratio (Phase 2 flip)", {
  tabs <- tab_num(sw, sex, height, na = "drop")

  tot_mean <- tabs |>
    dplyr::filter(is_totrow(height)) |>
    dplyr::pull(height) |>
    get_mean()

  sex_levels <- levels(sw$sex)

  for (s in sex_levels) {
    tab_row <- tabs |> dplyr::filter(sex == s)
    if (nrow(tab_row) == 0) next
    cell      <- tab_row |> dplyr::pull(height)
    tab_diff  <- get_diff(cell)
    tab_ratio <- get_ratio(cell)
    tab_mean  <- get_mean(cell)

    if (is.na(tab_mean) || is.na(tot_mean) || tot_mean == 0) next

    testthat::expect_equal(tab_diff,  tab_mean - tot_mean, tolerance = 1e-8,
                           label = paste0("mean diff [", s, "]"))
    testthat::expect_equal(tab_ratio, tab_mean / tot_mean, tolerance = 1e-8,
                           label = paste0("mean ratio [", s, "]"))
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


# === SECTION: tot_n / tot_wn base =============================================

testthat::test_that("tot_n stores each cell's own unweighted base; tot_wn recovers wn/pct", {
  df <- tibble::tibble(
    grp = factor(rep(c("A", "B", "C"), length.out = 90)),
    q   = factor(rep(c("yes", "no"),   length.out = 90)),
    wt  = rep(c(1, 2, 3), length.out = 90)
  )

  # col%: each cell's base is its column total (unweighted n and weighted wn of the total row).
  tc <- tab(df, grp, q, wt = wt, pct = "col")
  cell <- tc[["yes"]]
  base_n  <- get_n(cell)[is_totrow(cell)][1]
  base_wn <- get_wn(cell)[is_totrow(cell)][1]
  testthat::expect_equal(unique(get_tot_n(cell)),  base_n)                      # stored field
  testthat::expect_equal(unique(get_tot_wn(cell)[is.finite(get_tot_wn(cell))]),
                         base_wn, tolerance = 1e-9)                             # recovered wn/pct
  testthat::expect_equal(cell$tot_wn, get_tot_wn(cell))                          # $ accessor

  # counts (pct = "no"): no base -> tot_n is NA.
  tn <- tab(df, grp, q, pct = "no")
  testthat::expect_true(all(is.na(get_tot_n(tn[["yes"]]))))

  # unweighted: the weighted base equals the unweighted base.
  tu <- tab(df, grp, q, pct = "col")
  testthat::expect_equal(get_tot_wn(tu[["yes"]]), get_tot_n(tu[["yes"]]))
})


# === SECTION: Mean confidence intervals =======================================

testthat::test_that("mean cell CI matches t(n-1) * sqrt(var/n) using stats::var() (rule B)", {
  # Rule B (14v-ii, decisions §48): a mean cell interval estimates the variance, so it is the
  # one-sample Student t(n-1), not z. (Was z = qnorm before 14v-ii.)
  tabs <- tab_num(sw, sex, height, na = "drop", ci = "cell", conf_level = 0.95)
  sex_levels <- levels(sw$sex)

  for (s in sex_levels) {
    tab_row <- tabs |> dplyr::filter(sex == s)
    if (nrow(tab_row) == 0) next
    tab_ci <- tab_row |> dplyr::pull(height) |> get_ci()

    d <- sw |> dplyr::filter(!is.na(height) & !is.na(sex) & sex == s)
    if (nrow(d) <= 1 || is.na(tab_ci)) next
    n <- nrow(d)
    # tab_num uses stats::var() (n-1 denominator) for unweighted data
    v <- stats::var(d$height)
    expected_ci <- stats::qt(0.025, df = n - 1, lower.tail = FALSE) * sqrt(v / n)

    testthat::expect_equal(tab_ci, expected_ci, tolerance = 1e-4,
                           label = paste0("mean CI [", s, "]"))
  }
})


testthat::test_that("mean diff CI matches Welch-t * sqrt(var1/n1 + var2/n2) (rule B)", {
  # Rule B (14v-ii): the mean-diff interval is always Welch-t -- the df no longer flips with stars
  # (stars on/off give the same bracket now; only the pvalue is added when stars are on).
  tabs <- tab_num(sw, sex, height, na = "drop", ci = "ref", conf_level = 0.95, stars = FALSE)

  # Reference: total row stats
  d_all <- sw |> dplyr::filter(!is.na(height) & !is.na(sex))
  n_ref <- nrow(d_all)
  var_ref <- stats::var(d_all$height)

  sex_levels <- levels(sw$sex)

  for (s in sex_levels) {
    tab_row <- tabs |> dplyr::filter(sex == s)
    if (nrow(tab_row) == 0) next
    tab_ci <- tab_row |> dplyr::pull(height) |> get_ci()

    d <- sw |> dplyr::filter(!is.na(height) & !is.na(sex) & sex == s)
    if (nrow(d) <= 1 || is.na(tab_ci)) next
    n <- nrow(d)
    v <- stats::var(d$height)

    se2 <- v / n + var_ref / n_ref
    df  <- se2^2 / ((v / n)^2 / (n - 1) + (var_ref / n_ref)^2 / (n_ref - 1))
    expected_ci <- stats::qt(0.025, df = df, lower.tail = FALSE) * sqrt(se2)
    testthat::expect_equal(tab_ci, expected_ci, tolerance = 1e-4,
                           label = paste0("mean diff CI [", s, "]"))
  }
})


testthat::test_that("option tabxplor.conf_level is the interval default; per-call arg overrides it (Phase 18c)", {
  ci_width <- function() {
    col <- tab(gss, race, marital, pct = "row", ci = "cell")[["Divorced"]]
    mean(get_ci_sup(col) - get_ci_inf(col), na.rm = TRUE)
  }
  w90 <- withr::with_options(list(tabxplor.conf_level = 0.90), ci_width())
  w95 <- withr::with_options(list(tabxplor.conf_level = 0.95), ci_width())
  w99 <- withr::with_options(list(tabxplor.conf_level = 0.99), ci_width())

  # the option widens the interval monotonically (it now genuinely drives the CIs)
  testthat::expect_true(w90 < w95 && w95 < w99)

  # a per-call conf_level = argument wins over the option
  override <- withr::with_options(list(tabxplor.conf_level = 0.99), {
    col <- tab(gss, race, marital, pct = "row", ci = "cell", conf_level = 0.90)[["Divorced"]]
    mean(get_ci_sup(col) - get_ci_inf(col), na.rm = TRUE)
  })
  testthat::expect_equal(override, w90)
})


testthat::test_that("stars agree with the CI bracket's own 0-exclusion (universal inclusion)", {
  cn <- tab(gss, race, marital, pct = "row", ci = "ref") |>
    dplyr::filter(race == "White") |> dplyr::pull("Married")
  p <- get_pvalue(cn); ok <- !is.na(p)
  excl95 <- get_ci_inf(cn) > 0 | get_ci_sup(cn) < 0
  testthat::expect_equal((p < 0.05)[ok], excl95[ok])
})


# === SECTION: Chi-squared test ================================================

testthat::test_that("chi2 statistic and p-value match stats::chisq.test", {
  # Phase 3b: the `test` attribute is a TIDY tibble (one row per subtable x col_var x test-type);
  # the chi2 stats live in the row where `test == "chi2"`.
  # Phase 20h: built by `tab(test = TRUE)`, not by the deprecated step chain -- since 19j both call
  # chi2_compute_test(), so the assertion is the same and it now covers the LIVE path.
  tabs      <- tab(gss, race, marital, pct = "row", test = TRUE)
  chi2_row  <- get_test(tabs) |> dplyr::filter(.data$test == "chi2")

  ct  <- table(gss$race, gss$marital)
  ct  <- ct[rowSums(ct) > 0, colSums(ct) > 0, drop = FALSE]   # engine drops empty margins
  ref <- suppressWarnings(stats::chisq.test(ct))

  testthat::expect_equal(chi2_row$statistic, unname(ref$statistic),
                         tolerance = 1e-9, label = "chi2 statistic")
  testthat::expect_equal(chi2_row$pvalue, ref$p.value,
                         tolerance = 1e-10, label = "chi2 p-value")
  testthat::expect_equal(chi2_row$df1, unname(as.double(ref$parameter)),
                         tolerance = 1e-10, label = "chi2 df")
})


# === SECTION: ANOVA / one-way F (Phase 3b) ====================================

testthat::test_that("ANOVA Welch F matches stats::oneway.test(var.equal = FALSE)", {
  d    <- gss |> dplyr::filter(!is.na(tvhours))
  tabs <- tab(d, marital, tvhours, pct = "row", test = TRUE)
  w    <- get_test(tabs) |> dplyr::filter(.data$test == "F_welch")
  ow   <- stats::oneway.test(tvhours ~ marital, data = d, var.equal = FALSE)

  testthat::expect_equal(w$statistic, unname(ow$statistic),            tolerance = 1e-8, label = "Welch F")
  testthat::expect_equal(w$df1,       unname(ow$parameter[["num df"]]),   tolerance = 1e-8, label = "Welch df1")
  testthat::expect_equal(w$df2,       unname(ow$parameter[["denom df"]]), tolerance = 1e-8, label = "Welch df2")
  testthat::expect_equal(w$pvalue,    ow$p.value,                      tolerance = 1e-8, label = "Welch p")
})


# === SECTION: Variance contributions (chi2 contributions) =====================

testthat::test_that("variance contributions match (O-E)^2/E / total_chi2", {
  # Phase 20h: `color = "contrib"` is what asks the build for the contributions (the step chain's
  # tab_chi2(color = TRUE) did the same through chi2_write_contrib(); measured identical).
  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "contrib")

  ct <- table(gss$race, gss$marital)
  # Remove columns/rows with 0 marginal (would cause NaN in expected)
  ct <- ct[rowSums(ct) > 0, colSums(ct) > 0]
  ref_chi2 <- suppressWarnings(stats::chisq.test(ct))
  total_chi2 <- unname(ref_chi2$statistic)

  test_cells <- list(
    c("White", "Married"),
    c("Black", "Never married")
  )

  for (cell in test_cells) {
    r <- cell[1]; m <- cell[2]
    tab_ctr <- tabs |>
      dplyr::filter(race == r) |>
      dplyr::pull(!!m) |>
      get_ctr()

    observed <- ct[r, m]
    expected_count <- ref_chi2$expected[r, m]
    if (expected_count == 0) next
    raw_contrib <- (observed - expected_count)^2 / expected_count
    expected_ctr <- raw_contrib / total_chi2

    testthat::expect_equal(abs(tab_ctr), expected_ctr, tolerance = 1e-4,
                           label = paste0("ctr [", r, ", ", m, "]"))
  }
})


testthat::test_that("test = TRUE names the test per column type (chi2 for factors, F for means)", {
  te <- get_test(tab(fx_gss(), race, c(marital, tvhours), pct = "row", test = TRUE))
  testthat::expect_equal(te$test[te$col == "marital"], "chi2")
  testthat::expect_true(all(c("F_welch", "F_classic") %in% te$test[te$col == "tvhours"]))
})
