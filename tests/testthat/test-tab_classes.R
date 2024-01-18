
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
  grouped_tabs[[2]]   <- grouped_tabs[[2]] %>% forcats::fct_recode("kéké" = "Black")
  grouped_tabs[[2,2]] <- factor("White")
  testthat::expect_s3_class(grouped_tabs, "tabxplor_grouped_tab")
})

