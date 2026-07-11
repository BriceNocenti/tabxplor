# PURPOSE: Phase 9b-4 -- the fmt CARRIER round-trip. fmt_unwrap()/fmt_wrap() decompose a built
#          tabxplor_tab into plain field-frames (vec_data()) + the 9 col attrs + the table's own
#          attributes, then materialize it back via the single fmt_materialize_col() seam. This locks
#          that the round-trip is byte-identical (identical()) for the representative table shapes --
#          the property Phase 9b-5 relies on to run chi2/ci on the carrier's plain fields. In-pipeline,
#          tab_transform() inserts the composed no-op fmt_wrap(fmt_unwrap()) at the tests boundary, so
#          the full golden/parity suites also cover it on every table they build.
# See: R/tab.R (fmt_unwrap / fmt_wrap / fmt_materialize_col); dev/tabxplor_phase9b_fmt_display_only.md §7.

carrier_gss <- function() {
  gss <- forcats::gss_cat
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss$marital[seq(1L, nrow(gss), by = 500L)] <- NA
  gss
}

roundtrip <- function(tab) tabxplor:::fmt_wrap(tabxplor:::fmt_unwrap(tab))

testthat::test_that("fmt carrier round-trip is identical() (factor / numeric / mixed shapes)", {
  gss <- carrier_gss()

  tabs <- list(
    plain     = tab(gss, marital, race),
    pct_color = tab(gss, marital, race, pct = "row", color = "diff"),
    chi2      = tab(gss, marital, race, pct = "row", color = "diff", chi2 = TRUE),
    ci_cell   = tab(gss, marital, race, pct = "row", ci = "cell", stars = TRUE),
    ci_diff   = tab(gss, marital, race, pct = "row", color = "diff", ci = "diff"),
    weighted  = tab(gss, marital, race, wt = w, pct = "row", color = "diff", chi2 = TRUE),
    numeric   = tab(gss, marital, age, pct = "row", ci = "cell", chi2 = TRUE),
    mixed     = tab(gss, marital, c(race, age), pct = "row", chi2 = TRUE),
    add_pct   = tab(gss, marital, race, pct = "row", add_pct = TRUE),
    col_pct   = tab(gss, marital, race, pct = "col", color = "diff")
  )

  for (nm in names(tabs)) {
    testthat::expect_identical(roundtrip(tabs[[nm]]), tabs[[nm]], info = nm)
  }
})

testthat::test_that("round-trip preserves grouped_tab class + groups/subtext/test attributes", {
  gss <- carrier_gss()

  # grouped_tab: a multi-level tab_var (year), no total table -> grouped by the 8 year levels.
  g <- tab_plain(gss, marital, race, year, pct = "row", totaltab = "no")
  testthat::expect_true(dplyr::is_grouped_df(g))
  testthat::expect_identical(roundtrip(g), g)

  # a chi2 table carries both a non-empty subtext (legend) and a populated `test` attribute;
  # identical() covers them, plus targeted checks that neither is dropped by the round-trip.
  t  <- tab(gss, marital, race, pct = "row", color = "diff", chi2 = TRUE)
  rt <- roundtrip(t)
  testthat::expect_identical(attr(rt, "subtext"),   attr(t, "subtext"))
  testthat::expect_identical(tabxplor:::get_test(rt), tabxplor:::get_test(t))
  testthat::expect_identical(rt, t)
})
