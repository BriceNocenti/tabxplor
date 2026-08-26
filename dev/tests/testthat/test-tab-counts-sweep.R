
# === SECTION: tab_counts() equals tab() on the same microdata =====================================

skip_on_cran()


# gss_cat + a deterministic weight + deterministic NAs (mirrors test-fuse-parity.R's make_gss()).
counts_gss <- function() {
  gss <- fx_gss()
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss
}


testthat::test_that("long counts == microdata with color = 'diff'", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row", color = "diff", ci = "cell", test = TRUE),
    tab(gss, marital, race, pct = "row", color = "diff", ci = "cell", test = TRUE))
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
    tab_counts(cu, marital, race, counts = n, pct = "row", ci = "cell", test = TRUE),
    tab(tidyr::uncount(cu, n), marital, race, pct = "row", ci = "cell", test = TRUE))
})


testthat::test_that("wide data.frame == microdata", {
  gss  <- counts_gss()
  ref  <- tab(gss, marital, race, pct = "row", test = TRUE, ci = "cell")
  wide <- tidyr::pivot_wider(dplyr::count(gss, marital, race),
                             names_from = race, values_from = n, values_fill = 0)
  testthat::expect_equal(
    tab_counts(wide, row_var = marital, cols = !marital, col_name = "race",
               pct = "row", test = TRUE, ci = "cell"),
    ref)
})


# Phase p: the modernised colour + argument surface. Each case CRASHED or DIVERGED before the fix
# (tab_counts() did not run normalize_color_spec() / finalize_color_tail() and hardwired several args).
# tab_counts(...) must equal tab(...) on the same microdata.
testthat::test_that("modern color forms == microdata (TRUE / two-channel / per-type / ratio / signif)", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)

  # color = TRUE (the headline fix: used to crash in dplyr::recode on a logical)
  testthat::expect_no_error(
    ct <- tab_counts(cu, marital, race, counts = n, pct = "row", color = TRUE))
  testthat::expect_equal(ct, tab(gss, marital, race, pct = "row", color = TRUE))

  # two-channel c(text, background)
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row",
               color = c("diff", "ratio"), ci = "cell", test = TRUE),
    tab(gss, marital, race, pct = "row", color = c("diff", "ratio"), ci = "cell", test = TRUE))

  # per-type list(pct = ...)
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row",
               color = list(pct = c("diff", "ratio")), ci = "cell", test = TRUE),
    tab(gss, marital, race, pct = "row", color = list(pct = c("diff", "ratio")), ci = "cell", test = TRUE))

  # the ratio measure (owns its CI via color_pct_text_is_ratio)
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row", color = "ratio", ci = "cell", test = TRUE),
    tab(gss, marital, race, pct = "row", color = "ratio", ci = "cell", test = TRUE))

  # color_signif policies (gate on the difference CI)
  for (pol in c("grey_non_signif", "guaranteed_effect")) {
    testthat::expect_equal(
      tab_counts(cu, marital, race, counts = n, pct = "row",
                 color = "diff", color_signif = pol, ci = "ref", test = TRUE),
      tab(gss, marital, race, pct = "row", color = "diff", color_signif = pol, ci = "ref", test = TRUE),
      info = pol)
  }
})


testthat::test_that("display / cleannames / n_min / common_totrow / color_breaks == microdata", {
  gss <- counts_gss()
  cu  <- dplyr::count(gss, marital, race)

  # composite display recipe
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row", display = "{pct} (n={n})"),
    tab(gss, marital, race, pct = "row", display = "{pct} (n={n})"))

  # n_min display filter
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row", n_min = 200),
    tab(gss, marital, race, pct = "row", n_min = 200))

  # common_totrow
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row", common_totrow = TRUE),
    tab(gss, marital, race, pct = "row", common_totrow = TRUE))

  # per-table color_breaks override survives + matches
  testthat::expect_equal(
    tab_counts(cu, marital, race, counts = n, pct = "row", color = TRUE,
               color_breaks = list(pct_diff = c(0.05, 0.1, 0.2))),
    tab(gss, marital, race, pct = "row", color = TRUE,
        color_breaks = list(pct_diff = c(0.05, 0.1, 0.2))))

  # cleannames strips the "1-" prefix pre-aggregate, exactly as the microdata path (Phase p)
  gss2 <- dplyr::mutate(gss, race = forcats::fct_relabel(race, ~ paste0("1-", .)))
  cu2  <- dplyr::count(gss2, marital, race)
  testthat::expect_equal(
    tab_counts(cu2, marital, race, counts = n, pct = "row", cleannames = TRUE),
    tab(gss2, marital, race, pct = "row", cleannames = TRUE))
})
