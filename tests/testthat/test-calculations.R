# PURPOSE: Validate tabxplor statistical calculations against standard R packages.
# ROLE: Ensures numerical correctness of percentages, CIs, chi2, contributions, OR.
# KEY CONSTRAINTS:
#   - Must run via test_check("tabxplor"), never in isolation.
#   - Comparisons use tolerance to allow for floating-point differences.
#   - gss_cat "Not applicable" race level has 0 observations — tabxplor drops it.
#   - Unweighted tab_num uses stats::var() (sample variance, n-1 denominator).

# === SECTION: Data setup ====================================================

gss <- forcats::gss_cat |> dplyr::filter(race != "Not applicable") |>
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
  # tab_num reports the ML (population, Sigma-w denominator) variance. Since 1.4.0 both are
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

# Since 1.4.0 (Phase 2) the numeric `diff` field is a real DIFFERENCE (mean - ref_mean) and the
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

# Since 1.4.0 (Phase 2) the weighted variance is derived from moment sums by
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

# === SECTION: Mean confidence intervals =======================================

testthat::test_that("mean CI matches z * sqrt(var/n) using stats::var()", {
  tabs <- tab_num(sw, sex, height, na = "drop", ci = "cell", conf_level = 0.95)
  sex_levels <- levels(sw$sex)
  z <- stats::qnorm(0.025, lower.tail = FALSE)

  for (s in sex_levels) {
    tab_row <- tabs |> dplyr::filter(sex == s)
    if (nrow(tab_row) == 0) next
    tab_ci <- tab_row |> dplyr::pull(height) |> get_ci()

    d <- sw |> dplyr::filter(!is.na(height) & !is.na(sex) & sex == s)
    if (nrow(d) <= 1 || is.na(tab_ci)) next
    n <- nrow(d)
    # tab_num uses stats::var() (n-1 denominator) for unweighted data
    v <- stats::var(d$height)
    expected_ci <- z * sqrt(v / n)

    testthat::expect_equal(tab_ci, expected_ci, tolerance = 1e-4,
                           label = paste0("mean CI [", s, "]"))
  }
})

testthat::test_that("mean diff CI (stars off) matches z * sqrt(var1/n1 + var2/n2)", {
  # Phase 3a: with stars OFF the mean-diff interval keeps the z quantile (stars ON -> Welch-t,
  # tested separately below).
  tabs <- tab_num(sw, sex, height, na = "drop", ci = "diff", conf_level = 0.95, stars = FALSE)
  z <- stats::qnorm(0.025, lower.tail = FALSE)

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

    expected_ci <- z * sqrt(v / n + var_ref / n_ref)
    testthat::expect_equal(tab_ci, expected_ci, tolerance = 1e-4,
                           label = paste0("mean diff CI [", s, "]"))
  }
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

testthat::test_that("diff CI for proportions (method='ac') matches DescTools::BinomDiffCI ac", {
  testthat::skip_if_not_installed("DescTools")
  # Phase 3a: AC is now the expert opt-in (default is Newcombe, tested below). get_ci() is the
  # upper arm (ci_sup - diff), matching DescTools' upr.ci - est.
  tabs <- tab(gss, race, marital, pct = "row", ci = "diff", conf_level = 0.95,
              method_diff = "ac", stars = FALSE)
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
  tabs <- tab(gss, race, marital, pct = "row", ci = "diff", conf_level = 0.95)
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

testthat::test_that("stars agree with the CI bracket's own 0-exclusion (universal inclusion)", {
  cn <- tab(gss, race, marital, pct = "row", ci = "diff") |>
    dplyr::filter(race == "White") |> dplyr::pull("Married")
  p <- get_pvalue(cn); ok <- !is.na(p)
  excl95 <- get_ci_inf(cn) > 0 | get_ci_sup(cn) < 0
  testthat::expect_equal((p < 0.05)[ok], excl95[ok])
})

testthat::test_that("mean diff CI (stars on) matches Welch t.test", {
  d <- sw |> dplyr::filter(!is.na(height) & !is.na(sex))
  tabs <- tab_num(d, sex, height, na = "drop", ci = "diff", conf_level = 0.95, stars = TRUE)
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

# === SECTION: Chi-squared test ================================================

testthat::test_that("chi2 statistic and p-value match stats::chisq.test", {
  # Phase 3b: the `test` attribute is a TIDY tibble (one row per subtable x col_var x test-type);
  # the chi2 stats live in the row where `test == "chi2"`. tab_plain() |> tab_chi2() keeps it.
  tabs      <- tab_plain(gss, race, marital, pct = "row") |> tab_chi2()
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

testthat::test_that("vectorised chi2 applies the Yates correction on 2x2 like chisq.test", {
  d <- gss |>
    dplyr::filter(marital %in% c("Married", "Divorced"), race %in% c("Black", "White")) |>
    dplyr::mutate(marital = forcats::fct_drop(marital), race = forcats::fct_drop(race))
  tabs     <- tab_plain(d, race, marital, pct = "row") |> tab_chi2()
  chi2_row <- get_test(tabs) |> dplyr::filter(.data$test == "chi2")
  ref      <- suppressWarnings(stats::chisq.test(table(d$race, d$marital)))  # Yates on 2x2 (default)

  testthat::expect_equal(chi2_row$df1, 1)
  testthat::expect_equal(chi2_row$statistic, unname(ref$statistic), tolerance = 1e-9)
  testthat::expect_equal(chi2_row$pvalue,    ref$p.value,           tolerance = 1e-10)
})

testthat::test_that("chi2 is unaffected by add_n columns and rows", {
  ref_tab  <- tab_plain(gss, race, marital, pct = "row") |> tab_chi2()
  ref_row  <- get_test(ref_tab) |> dplyr::filter(.data$test == "chi2")
  # a built table already carrying add_n column + row, then chi2 on it (the historical bug case)
  addn_tab <- tab(gss, race, marital, pct = "row", add_n = TRUE) |> tab_chi2()
  addn_row <- get_test(addn_tab) |> dplyr::filter(.data$test == "chi2")

  testthat::expect_equal(addn_row$statistic, ref_row$statistic, tolerance = 1e-6)
  testthat::expect_equal(addn_row$pvalue,    ref_row$pvalue,    tolerance = 1e-9)
})

# === SECTION: ANOVA / one-way F (Phase 3b) ====================================

testthat::test_that("ANOVA Welch F matches stats::oneway.test(var.equal = FALSE)", {
  d    <- gss |> dplyr::filter(!is.na(tvhours))
  tabs <- tab(d, marital, tvhours, pct = "row") |> tab_chi2()   # chi2 default FALSE -> clean attr
  w    <- get_test(tabs) |> dplyr::filter(.data$test == "F_welch")
  ow   <- stats::oneway.test(tvhours ~ marital, data = d, var.equal = FALSE)

  testthat::expect_equal(w$statistic, unname(ow$statistic),            tolerance = 1e-8, label = "Welch F")
  testthat::expect_equal(w$df1,       unname(ow$parameter[["num df"]]),   tolerance = 1e-8, label = "Welch df1")
  testthat::expect_equal(w$df2,       unname(ow$parameter[["denom df"]]), tolerance = 1e-8, label = "Welch df2")
  testthat::expect_equal(w$pvalue,    ow$p.value,                      tolerance = 1e-8, label = "Welch p")
})

testthat::test_that("ANOVA classic F matches stats::oneway.test(var.equal = TRUE)", {
  d    <- gss |> dplyr::filter(!is.na(tvhours))
  tabs <- tab(d, marital, tvhours, pct = "row") |> tab_chi2()
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
  tabs <- tab(d, race, tvhours, pct = "row") |> tab_chi2()
  w    <- get_test(tabs) |> dplyr::filter(.data$test == "F_welch")
  ow   <- stats::oneway.test(tvhours ~ race, data = d, var.equal = FALSE)

  testthat::expect_equal(w$statistic, unname(ow$statistic), tolerance = 1e-8)
  testthat::expect_equal(w$df2,       unname(ow$parameter[["denom df"]]), tolerance = 1e-8)
  testthat::expect_equal(w$pvalue,    ow$p.value,           tolerance = 1e-8)
})

# === SECTION: Variance contributions (chi2 contributions) =====================

testthat::test_that("variance contributions match (O-E)^2/E / total_chi2", {
  # Use pipe approach for chi2 (tab_many chi2=TRUE doesn't populate chi2 attr)
  tabs <- tab_plain(gss, race, marital, pct = "row") |>
    tab_chi2(color = TRUE)

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

# === SECTION: Odds ratios =====================================================

testthat::test_that("OR calculation produces finite numeric values", {
  tabs <- tab(gss, race, marital, pct = "row", OR = "OR")

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
  tabs <- tab(gss, race, marital, pct = "row", OR = "OR")

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
  tabs <- tab(gss, race, marital, pct = "row", sup_cols = tvhours)

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
