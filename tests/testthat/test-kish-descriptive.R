# PURPOSE: Lock the Last-Phase-s extension -- options(tabxplor.kish_neff = TRUE) replaces the raw
#          unweighted n with Kish's effective sample size n_eff = (Sum w)^2 / Sum(w^2) in ALL weighted
#          DESCRIPTIVE confidence intervals: tab()/tab_num() factor proportions AND means (+ the
#          colour-OR significance), and tab_reg()'s empirical (`empirical = TRUE`) companions. The
#          model CIs of tab_reg() stay design-based (svyglm) and MUST be untouched by the option.
# ROLE: Guardrail for the descriptive-CI weight correction. FAILING-FIRST: each `widens` assertion
#       fails on the pre-Phase-s code (where kish touched only the mean CI + the omnibus p-value).
# KEY CONSTRAINTS:
#   - Off-kish output is byte-identical (n_eff is NA -> tab_ci coalesces to the raw base).
#   - The DISPLAYED `n` (raw count) never changes; only the CI base does.

# Deterministic weighted data with deliberately UNEQUAL weights (so n_eff << n, deff > 1).
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
  off <- withr::with_options(list(tabxplor.kish_neff = FALSE),
                             tab(d, g, y, wt = w, pct = "row", ci = "cell", na = "drop"))
  on  <- withr::with_options(list(tabxplor.kish_neff = TRUE),
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
  off <- withr::with_options(list(tabxplor.kish_neff = FALSE),
                             tab(d, g, y, wt = w, pct = "row", ci = "diff", na = "drop"))
  on  <- withr::with_options(list(tabxplor.kish_neff = TRUE),
                             tab(d, g, y, wt = w, pct = "row", ci = "diff", na = "drop"))
  hw_off <- ci_hw(off[["yes"]]); hw_on <- ci_hw(on[["yes"]])
  ok <- is.finite(hw_off) & is.finite(hw_on)
  testthat::expect_gt(sum(hw_on[ok] > hw_off[ok] + 1e-6), 0L)
})

testthat::test_that("colour-OR significance interval widens under kish", {
  d <- kish_data()
  off <- withr::with_options(list(tabxplor.kish_neff = FALSE),
                             tab(d, g, y, wt = w, pct = "row", color = "OR",
                                 color_signif = "grey_non_signif", na = "drop"))
  on  <- withr::with_options(list(tabxplor.kish_neff = TRUE),
                             tab(d, g, y, wt = w, pct = "row", color = "OR",
                                 color_signif = "grey_non_signif", na = "drop"))
  hw_off <- ci_hw(off[["yes"]]); hw_on <- ci_hw(on[["yes"]])
  ok <- is.finite(hw_off) & is.finite(hw_on)
  testthat::expect_gt(sum(ok), 0L)
  testthat::expect_gt(sum(hw_on[ok] > hw_off[ok] + 1e-6), 0L)
})

testthat::test_that("mean cell CI: n_eff surfaced + interval widens under kish", {
  d <- kish_data()
  off <- withr::with_options(list(tabxplor.kish_neff = FALSE),
                             tab_num(d, g, x, wt = w, ci = "cell", na = "drop"))
  on  <- withr::with_options(list(tabxplor.kish_neff = TRUE),
                             tab_num(d, g, x, wt = w, ci = "cell", na = "drop"))
  mcol <- names(on)[purrr::map_lgl(on, ~ is_fmt(.) && get_type(.) == "mean")][1]
  testthat::expect_true(all(is.na(get_n_eff(off[[mcol]]))))
  testthat::expect_gt(sum(is.finite(get_n_eff(on[[mcol]]))), 0L)
  hw_off <- ci_hw(off[[mcol]]); hw_on <- ci_hw(on[[mcol]])
  ok <- is.finite(hw_off) & is.finite(hw_on)
  testthat::expect_gt(sum(hw_on[ok] > hw_off[ok] + 1e-6), 0L)
})

testthat::test_that("off-kish output is byte-identical (n_eff NA, display unchanged)", {
  d <- kish_data()
  a <- withr::with_options(list(tabxplor.kish_neff = FALSE),
                           tab(d, g, y, wt = w, pct = "row", ci = "diff", na = "drop"))
  b <- withr::with_options(list(tabxplor.kish_neff = FALSE),
                           tab(d, g, y, wt = w, pct = "row", ci = "diff", na = "drop"))
  testthat::expect_identical(a[["yes"]], b[["yes"]])
  testthat::expect_true(all(is.na(get_n_eff(a[["yes"]]))))
  testthat::expect_identical(tab_md(a), tab_md(b))
})

testthat::test_that("tab_reg empirical CIs widen under kish; model CI + displayed n unchanged", {
  testthat::skip_if_not_installed("survey")
  d <- kish_data(600L)
  off <- withr::with_options(list(tabxplor.kish_neff = FALSE),
                             tab_reg(d, dependent = "y", predictors = "g",
                                     family = "binomial", wt = "w", empirical = TRUE))
  on  <- withr::with_options(list(tabxplor.kish_neff = TRUE),
                             tab_reg(d, dependent = "y", predictors = "g",
                                     family = "binomial", wt = "w", empirical = TRUE))
  cn <- names(on)
  obspct <- grep("Obs_%",   cn, value = TRUE, fixed = TRUE)[1]
  obsor  <- grep("Obs_OR",  cn, value = TRUE, fixed = TRUE)[1]
  model  <- grep("Model_OR", cn, value = TRUE, fixed = TRUE)[1]

  # empirical companion CIs widen
  testthat::expect_gt(sum(ci_hw(on[[obspct]]) > ci_hw(off[[obspct]]) + 1e-6, na.rm = TRUE), 0L)
  testthat::expect_gt(sum(ci_hw(on[[obsor]])  > ci_hw(off[[obsor]])  + 1e-6, na.rm = TRUE), 0L)
  # displayed count untouched
  testthat::expect_identical(get_n(off[[obspct]]), get_n(on[[obspct]]))
  # the design-based MODEL CI must NOT move with the option
  testthat::expect_equal(get_ci_inf(off[[model]]), get_ci_inf(on[[model]]))
  testthat::expect_equal(get_ci_sup(off[[model]]), get_ci_sup(on[[model]]))
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
    list(tabxplor.kish_neff = TRUE),
    tab_counts(cnt, row_var = g, col_var = y, counts = count, wt_counts = wcount,
               pct = "row", ci = "cell")
  )
  ycol <- on[["yes"]]
  testthat::expect_true(all(is.na(get_n_eff(ycol))))             # no correction possible -> NA
})
