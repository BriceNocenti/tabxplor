# PURPOSE: tab_counts(): building the same table from already-aggregated counts.
# ROLE: the shipped CONTRACT for R/tab-counts.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: tab_counts() equals tab() on the same microdata =====================================

skip_on_cran()


# gss_cat + a deterministic weight + deterministic NAs (mirrors test-fuse-parity.R's make_gss()).
counts_gss <- function() {
  gss <- fx_gss()
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss
}


testthat::test_that("long counts == microdata across pct / chi2 / ci configs", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)                                  # unweighted long counts

  grid <- expand.grid(pct = c("no", "row", "col"), chi2 = c(FALSE, TRUE),
                      ci = c("no", "cell", "ref"), stringsAsFactors = FALSE)
  grid <- grid[!(grid$pct == "no" & grid$ci == "ref"), ]                   # a reference CI needs one

  for (i in seq_len(nrow(grid))) {
    p <- grid$pct[i]; k <- grid$chi2[i]; cc <- grid$ci[i]
    testthat::expect_equal(
      tab_counts(cu, marital, race, counts = n, pct = p, test = k, ci = cc),
      tab(gss, marital, race, pct = p, test = k, ci = cc),
      info = sprintf("pct=%s chi2=%s ci=%s", p, k, cc)
    )
  }
})


testthat::test_that("weighted counts (real n + weighted wn) == weighted microdata (weighted est + unweighted n)", {
  gss <- counts_gss()
  cw  <- gss |>
    dplyr::group_by(marital, race) |>
    dplyr::summarise(n = dplyr::n(), wn = sum(w), .groups = "drop")

  # Phase 16d: the `vars$wt` (weight column NAME for the "Weighted by <wt>." footer) legitimately differs
  # by ENTRY POINT -- the microdata path names the weight "w", the from-the-middle path names its weighted-
  # count column "wn" -- while the NUMBERS are identical. Strip that footer-only detail before comparing.
  # Phase 17b: `vars` now lives in the `meta` list -- strip via the getter/setter, not attr("vars").
  strip_wt <- function(t) { v <- get_vars_attr(t); v$wt <- NULL; set_vars_attr(t, v) }
  for (cc in c("no", "cell", "ref")) {
    testthat::expect_equal(
      strip_wt(tab_counts(cw, marital, race, counts = n, wt_counts = wn, pct = "row", ci = cc, test = TRUE)),
      strip_wt(tab(gss, marital, race, wt = w, pct = "row", ci = cc, test = TRUE)),
      info = paste0("ci=", cc))
  }
})


testthat::test_that("tab_vars (subtables) == microdata", {
  gss <- counts_gss()
  c3  <- dplyr::count(gss, year, marital, race)                           # tab_vars = year
  testthat::expect_equal(
    tab_counts(c3, marital, race, year, counts = n, pct = "row", test = TRUE),
    tab(gss, marital, race, year, pct = "row", test = TRUE))
})


testthat::test_that("table / xtabs objects == microdata (empty levels dropped)", {
  gss <- counts_gss()
  ref <- tab(gss, marital, race, pct = "row", test = TRUE, ci = "cell")
  testthat::expect_equal(
    tab_counts(table(marital = gss$marital, race = gss$race),
               pct = "row", test = TRUE, ci = "cell"), ref)
  testthat::expect_equal(
    tab_counts(stats::xtabs(~ marital + race, data = gss),
               pct = "row", test = TRUE, ci = "cell"), ref)
  # a bare matrix with named dimnames (coerced via as.table())
  m <- unclass(table(gss$marital, gss$race)); names(dimnames(m)) <- c("marital", "race")
  testthat::expect_equal(tab_counts(m, pct = "row", test = TRUE, ci = "cell"), ref)
  # a 3D table becomes tab_vars, and empty tab_var x row_var combinations are dropped
  testthat::expect_equal(
    tab_counts(table(year = gss$year, marital = gss$marital, race = gss$race),
               row_var = marital, col_var = race, tab_vars = year, pct = "row", test = TRUE),
    tab(gss, marital, race, year, pct = "row", test = TRUE))
})


testthat::test_that("frequencies + base N: full precision == microdata, exactly", {
  gss  <- counts_gss()
  ref  <- tab(gss, marital, race, pct = "row", test = TRUE, ci = "cell")
  freq <- dplyr::count(gss, marital, race) |>
    dplyr::group_by(marital) |>
    dplyr::mutate(N = sum(n), pct = n / N) |>            # full-precision proportions
    dplyr::ungroup() |> dplyr::select(marital, race, pct, N) |>
    tidyr::pivot_wider(names_from = race, values_from = pct, values_fill = 0)
  testthat::expect_equal(
    tab_counts(freq, row_var = marital, cols = !c(marital, N), col_name = "race",
               base = N, input = "pct", pct = "row", test = TRUE, ci = "cell"),
    ref)
})


testthat::test_that("base-less (non-integer) counts disable CI/chi2 with a message", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)
  cu$n <- cu$n + 0.5                                     # fractional -> not a real unweighted n
  testthat::expect_warning(
    out <- tab_counts(cu, marital, race, counts = n, pct = "row", ci = "cell", test = TRUE),
    "not whole numbers")
  testthat::expect_true(all(is.na(get_ci_inf(out[[2]]))))           # CI skipped
  testthat::expect_equal(nrow(dplyr::filter(get_test(out), test == "chi2")), 0L)  # chi2 skipped
})
