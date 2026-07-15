
tabs <- tab(forcats::gss_cat, race, marital)

testthat::test_that("dplyr::rowwise preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::rowwise(tabs), "tabxplor_tab")
})

testthat::test_that("dplyr::mutate preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::mutate(tabs, Married = sum(Married)), "tabxplor_tab")
})

testthat::test_that("dplyr::transmute preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::transmute(tabs, race = race, Married = sum(Married)),
                  "tabxplor_tab")
})

testthat::test_that("dplyr::filter preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::filter(tabs, is_totrow(Married)), "tabxplor_tab")
})

testthat::test_that("dplyr::slice preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::slice(tabs, 1:2), "tabxplor_tab")
})

testthat::test_that("dplyr::arrange preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::arrange(tabs, Married), "tabxplor_tab")
})

testthat::test_that("dplyr::distinct preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::distinct(tabs), "tabxplor_tab")
})

testthat::test_that("dplyr::select preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::select(tabs, race, Married), "tabxplor_tab")
})

testthat::test_that("dplyr::rename, rename_with and relocate preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::relocate   (tabs, Divorced , .after = Married),
                            "tabxplor_tab")
  testthat::expect_s3_class(dplyr::rename     (tabs, new_name = race), "tabxplor_tab")
  testthat::expect_s3_class(dplyr::rename_with(tabs, toupper), "tabxplor_tab")
})

testthat::test_that("[<- and [[<- preserves class tabxplor_tab", {
  tabs[4]     <- dplyr::mutate(tabs[4], dplyr::across(.cols = dplyr::everything(), .fns = ~ set_display(., "ctr")))
  tabs[[2]]   <- tabs[[2]] %>% set_digits(3)
  tabs[[2, 1]] <- factor("White")
  testthat::expect_s3_class(tabs, "tabxplor_tab")
})



grouped_tabs <- forcats::gss_cat %>%
  dplyr::filter(year %in% c(2000, 2014)) %>%
  tab(race, marital, year)

testthat::test_that("dplyr::ungroup preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::ungroup(grouped_tabs), "tabxplor_tab")
})

testthat::test_that("dplyr::summarise, preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::summarise (grouped_tabs, Married = sum(Married)),
                            "tabxplor_tab")
})


testthat::test_that("dplyr::rowwise preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::rowwise(grouped_tabs), "tabxplor_grouped_tab")
})

testthat::test_that("dplyr::mutate preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::mutate(grouped_tabs, Married = sum(Married)),
                  "tabxplor_grouped_tab")
})

testthat::test_that("dplyr::transmute preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::transmute(grouped_tabs, year = year, race = race,
                                   Married = sum(Married)), "tabxplor_grouped_tab")
})

testthat::test_that("dplyr::filter preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::filter(grouped_tabs, is_totrow(Married)),
                            "tabxplor_grouped_tab")
})

testthat::test_that("dplyr::slice preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::slice(grouped_tabs, 1:2), "tabxplor_grouped_tab")
})

testthat::test_that("dplyr::arrange preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::arrange(grouped_tabs, Married), "tabxplor_grouped_tab")
})

testthat::test_that("dplyr::distinct preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::distinct(grouped_tabs), "tabxplor_grouped_tab")
})

testthat::test_that("dplyr::select preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::select(grouped_tabs, year, race, Married),
                            "tabxplor_grouped_tab")
})

testthat::test_that("dplyr::rename, rename_with and relocate preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::relocate   (grouped_tabs, Divorced , .after = Married),
                  "tabxplor_grouped_tab")
  testthat::expect_s3_class(dplyr::rename     (grouped_tabs, new_name = year),
                  "tabxplor_grouped_tab")
  testthat::expect_s3_class(dplyr::rename_with(grouped_tabs, toupper), "tabxplor_grouped_tab")
})

testthat::test_that("[<- and [[<- preserves class tabxplor_grouped_tab", {
  grouped_tabs[4]     <- dplyr::mutate(grouped_tabs[4],
                                       dplyr::across(.cols = dplyr::everything(), .fns = ~ set_display(., "ctr")))
  grouped_tabs[[2]]   <- grouped_tabs[[2]] %>% forcats::fct_recode("k\u00e9k\u00e9" = "Black")
  grouped_tabs[[2,2]] <- factor("White")
  testthat::expect_s3_class(grouped_tabs, "tabxplor_grouped_tab")
})


# --- Data-driven verb-coverage registry ----------------------------------------------------
# Extensible guardrail for the 1.4.0 refactors (esp. the tab()/tab_many() merge): each verb is
# checked to preserve BOTH tab classes. A failure names the exact verb whose class-preserving
# S3 method is missing/broken. To add a new verb, append one closure here (works identically
# for a flat and a grouped tab) -- see the `/dplyr-method` skill. Complements the explicit
# per-verb tests above.
verb_coverage <- list(
  mutate      = function(x) dplyr::mutate(x, Married = sum(Married)),
  filter      = function(x) dplyr::filter(x, is_totrow(Married)),
  slice       = function(x) dplyr::slice(x, 1:2),
  arrange     = function(x) dplyr::arrange(x, Married),
  distinct    = function(x) dplyr::distinct(x),
  select      = function(x) dplyr::select(x, dplyr::everything()),
  relocate    = function(x) dplyr::relocate(x, Divorced, .after = Married),
  rename_with = function(x) dplyr::rename_with(x, toupper),
  rowwise     = function(x) dplyr::rowwise(x)
)

cov_flat    <- tab(forcats::gss_cat, race, marital)
cov_grouped <- forcats::gss_cat %>%
  dplyr::filter(year %in% c(2000, 2014)) %>%
  tab(race, marital, year)

for (vname in names(verb_coverage)) {
  local({
    v <- vname
    testthat::test_that(paste0("verb-coverage keeps tabxplor_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_flat), "tabxplor_tab")
    })
    testthat::test_that(paste0("verb-coverage keeps tabxplor_grouped_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_grouped), "tabxplor_grouped_tab")
    })
  })
}


# --- Table-attribute survival + class up/down-grade (1.4.0 tab()/tab_many() merge net) -------
# The blocks above check only that the tab CLASS survives a verb; they do NOT check the two
# table-level attributes: `subtext` (the legend) and `chi2` (the test-results tibble that
# Phase 3 renames to `test`). A verb method could silently reset either to its new_tab()
# default and every test above would still pass. These blocks close that hole -- the most
# valuable coverage before the Phase 6 class-model rewrite touches every reattach site.
#
# tab_plain() |> tab_chi2() is the REAL populator of the chi2 attribute (tab(chi2 = TRUE) does
# NOT fill it for simple tables -- see the DESIGN note in test-calculations.R). subtext has no
# lightweight real populator (the subtext= arg stores whole population data), so a sentinel is
# set directly; that still faithfully exercises the carry path (methods do
# `subtext = get_subtext(.data)`). Both attributes are thus non-default, so "survives" is a
# real assertion, not a vacuous empty == empty.
cov_flat_attr <- tab_plain(forcats::gss_cat, race, marital, pct = "row") |> tab_chi2()
attr(cov_flat_attr, "subtext") <- "phase0 sentinel subtext"

cov_grouped_attr <- dplyr::filter(forcats::gss_cat, year %in% c(2000, 2014)) |>
  tab_plain(race, marital, year, pct = "row") |>
  tab_chi2()
attr(cov_grouped_attr, "subtext") <- "phase0 sentinel subtext"

testthat::test_that("attr fixtures are non-trivial (guards the survival tests below)", {
  testthat::expect_gt(nrow(get_chi2(cov_flat_attr)),    0L)
  testthat::expect_gt(nrow(get_chi2(cov_grouped_attr)), 0L)
  testthat::expect_true(any(nzchar(get_subtext(cov_flat_attr))))
  testthat::expect_true(any(nzchar(get_subtext(cov_grouped_attr))))
})

attr_fixtures <- list(tabxplor_tab = cov_flat_attr, tabxplor_grouped_tab = cov_grouped_attr)
for (cls in names(attr_fixtures)) {
  for (vname in names(verb_coverage)) {
    local({
      fx    <- attr_fixtures[[cls]]
      klass <- cls
      v     <- vname
      testthat::test_that(paste0("verb keeps subtext + chi2 (", klass, "): ", v), {
        out <- verb_coverage[[v]](fx)
        testthat::expect_identical(get_subtext(out), get_subtext(fx))
        testthat::expect_identical(get_chi2(out),    get_chi2(fx))
      })
    })
  }
}

testthat::test_that("group_by upgrades tabxplor_tab -> tabxplor_grouped_tab, keeping attrs", {
  # group_by.tabxplor_tab (tab_classes.R) has a registered method but was untested: the
  # flat -> grouped UPGRADE. race has >1 level, so it does not immediately re-downgrade.
  gb <- dplyr::group_by(cov_flat_attr, race)
  testthat::expect_s3_class(gb, "tabxplor_grouped_tab")
  testthat::expect_identical(get_subtext(gb), get_subtext(cov_flat_attr))
  testthat::expect_identical(get_chi2(gb),    get_chi2(cov_flat_attr))
})

testthat::test_that("grouped_tab auto-downgrades to plain tabxplor_tab at one group left", {
  # lv1_group_vars(): filtering a 2-group tab down to a single group drops the other group,
  # so n_groups() == 1 -> the dplyr_row_slice/reconstruct trio downgrades grouped -> plain.
  # Only the explicit ungroup() path was tested before; this pins the AUTOMATIC trip.
  one <- dplyr::filter(cov_grouped_attr, year == 2000)
  testthat::expect_lte(dplyr::n_groups(one), 1L)
  testthat::expect_s3_class(one, "tabxplor_tab")
  testthat::expect_false(inherits(one, "tabxplor_grouped_tab"))
  testthat::expect_identical(get_subtext(one), get_subtext(cov_grouped_attr))
  testthat::expect_identical(get_chi2(one),    get_chi2(cov_grouped_attr))
})

testthat::test_that("group_split on a grouped tab returns class-preserving tabs", {
  parts <- dplyr::group_split(cov_grouped_attr)
  testthat::expect_true(all(vapply(parts, is_tab, logical(1))))
})

