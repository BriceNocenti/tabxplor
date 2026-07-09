# PURPOSE: tab_counts() (the from-the-middle constructor) must build a table byte-identical to the
#          one tab() builds from the underlying microdata, whenever the count `n` is real. The
#          counts are produced from the SAME microdata via dplyr::count() (unweighted) or
#          count()+summarise (weighted, real n + weighted wn), so any divergence is a real bug.
# See: R/tab-counts.R; CLAUDE.md > 1.4.0 roadmap > Phase 4; decisions doc §20.

# gss_cat + a deterministic weight + deterministic NAs (mirrors test-fuse-parity.R's make_gss()).
counts_gss <- function() {
  gss <- forcats::gss_cat
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss
}

testthat::test_that("long counts == microdata across pct / chi2 / ci configs", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)                                  # unweighted long counts

  grid <- expand.grid(pct = c("no", "row", "col"), chi2 = c(FALSE, TRUE),
                      ci = c("no", "cell", "diff"), stringsAsFactors = FALSE)
  grid <- grid[!(grid$pct == "no" & grid$ci == "diff"), ]                  # diff CI needs a reference

  for (i in seq_len(nrow(grid))) {
    p <- grid$pct[i]; k <- grid$chi2[i]; cc <- grid$ci[i]
    testthat::expect_equal(
      tab_counts(cu, marital, race, counts = n, pct = p, chi2 = k, ci = cc),
      tab(gss, marital, race, pct = p, chi2 = k, ci = cc),
      info = sprintf("pct=%s chi2=%s ci=%s", p, k, cc)
    )
  }
})

testthat::test_that("long counts == microdata with color = 'diff'", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row", color = "diff", ci = "cell", chi2 = TRUE),
    tab(gss, marital, race, pct = "row", color = "diff", ci = "cell", chi2 = TRUE))
})

# Phase 7d-ii: tab_counts() now routes through the SAME tab_setup()/tab_transform()/tab_assemble()
# stages as tab(), translating `tot` -> (totrow, totcol) exactly as tab()'s wrapper does. Lock the
# non-default `tot` values (previously only the default c("row","col") was covered).
testthat::test_that("non-default tot == microdata (routed tot -> totrow/totcol translation)", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)
  for (t in list("both", "row", "col", "no")) {
    tt <- if (identical(t, "both")) c("row", "col") else t
    testthat::expect_equal(
      tab_counts(cu, marital, race, counts = n, pct = "row", tot = tt),
      tab(gss, marital, race, pct = "row", tot = tt),
      info = sprintf("tot=%s", paste(tt, collapse = ","))
    )
  }
  # tot with tab_vars + col% too
  cuw <- dplyr::count(gss, marital, race, year)
  testthat::expect_equal(
    tab_counts(cuw, marital, race, year, counts = n, pct = "col", tot = "col"),
    tab(gss, marital, race, year, pct = "col", tot = "col"))
})

testthat::test_that("weighted counts (real n + weighted wn) == weighted microdata (weighted est + unweighted n)", {
  gss <- counts_gss()
  cw  <- gss |>
    dplyr::group_by(marital, race) |>
    dplyr::summarise(n = dplyr::n(), wn = sum(w), .groups = "drop")

  for (cc in c("no", "cell", "diff")) {
    testthat::expect_equal(
      tab_counts(cw, marital, race, counts = n, wt_counts = wn, pct = "row", ci = cc, chi2 = TRUE),
      tab(gss, marital, race, wt = w, pct = "row", ci = cc, chi2 = TRUE),
      info = paste0("ci=", cc))
  }
})

testthat::test_that("tab_vars (subtables) == microdata", {
  gss <- counts_gss()
  c3  <- dplyr::count(gss, year, marital, race)                           # tab_vars = year
  testthat::expect_equal(
    tab_counts(c3, marital, race, year, counts = n, pct = "row", chi2 = TRUE),
    tab(gss, marital, race, year, pct = "row", chi2 = TRUE))
})

testthat::test_that("na = 'keep'/'drop' == microdata when the counts carry the NA level", {
  gss <- counts_gss()
  gss$marital[seq(1L, nrow(gss), by = 500L)] <- NA                        # deterministic NAs
  cu <- dplyr::count(gss, marital, race)                                  # count() keeps the NA group
  for (na in c("keep", "drop")) {
    testthat::expect_equal(
      tab_counts(cu, marital, race, counts = n, pct = "row", na = na),
      tab(gss, marital, race, pct = "row", na = na),
      info = paste0("na=", na))
  }
})

testthat::test_that("uncount() oracle: aggregate path == a genuine microdata path", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row", ci = "cell", chi2 = TRUE),
    tab(tidyr::uncount(cu, n), marital, race, pct = "row", ci = "cell", chi2 = TRUE))
})

testthat::test_that("wide data.frame == microdata", {
  gss  <- counts_gss()
  ref  <- tab(gss, marital, race, pct = "row", chi2 = TRUE, ci = "cell")
  wide <- tidyr::pivot_wider(dplyr::count(gss, marital, race),
                             names_from = race, values_from = n, values_fill = 0)
  testthat::expect_equal(
    tab_counts(wide, row_var = marital, cols = !marital, col_name = "race",
               pct = "row", chi2 = TRUE, ci = "cell"),
    ref)
})

testthat::test_that("table / xtabs objects == microdata (empty levels dropped)", {
  gss <- counts_gss()
  ref <- tab(gss, marital, race, pct = "row", chi2 = TRUE, ci = "cell")
  testthat::expect_equal(
    tab_counts(table(marital = gss$marital, race = gss$race),
               pct = "row", chi2 = TRUE, ci = "cell"), ref)
  testthat::expect_equal(
    tab_counts(stats::xtabs(~ marital + race, data = gss),
               pct = "row", chi2 = TRUE, ci = "cell"), ref)
  # a bare matrix with named dimnames (coerced via as.table())
  m <- unclass(table(gss$marital, gss$race)); names(dimnames(m)) <- c("marital", "race")
  testthat::expect_equal(tab_counts(m, pct = "row", chi2 = TRUE, ci = "cell"), ref)
  # a 3D table becomes tab_vars, and empty tab_var x row_var combinations are dropped
  testthat::expect_equal(
    tab_counts(table(year = gss$year, marital = gss$marital, race = gss$race),
               row_var = marital, col_var = race, tab_vars = year, pct = "row", chi2 = TRUE),
    tab(gss, marital, race, year, pct = "row", chi2 = TRUE))
})

testthat::test_that("frequencies + base N: full precision == microdata, exactly", {
  gss  <- counts_gss()
  ref  <- tab(gss, marital, race, pct = "row", chi2 = TRUE, ci = "cell")
  freq <- dplyr::count(gss, marital, race) |>
    dplyr::group_by(marital) |>
    dplyr::mutate(N = sum(n), pct = n / N) |>            # full-precision proportions
    dplyr::ungroup() |> dplyr::select(marital, race, pct, N) |>
    tidyr::pivot_wider(names_from = race, values_from = pct, values_fill = 0)
  testthat::expect_equal(
    tab_counts(freq, row_var = marital, cols = !c(marital, N), col_name = "race",
               base = N, input = "pct", pct = "row", chi2 = TRUE, ci = "cell"),
    ref)
})

testthat::test_that("base-less (non-integer) counts disable CI/chi2 with a message", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)
  cu$n <- cu$n + 0.5                                     # fractional -> not a real unweighted n
  testthat::expect_warning(
    out <- tab_counts(cu, marital, race, counts = n, pct = "row", ci = "cell", chi2 = TRUE),
    "not whole numbers")
  testthat::expect_true(all(is.na(get_ci_inf(out[[2]]))))           # CI skipped
  testthat::expect_equal(nrow(dplyr::filter(get_test(out), test == "chi2")), 0L)  # chi2 skipped
})
