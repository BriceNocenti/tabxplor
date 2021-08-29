
tabs <- tab(forcats::gss_cat, race, marital)

test_that("dplyr::rowwise preserves class tab", {
  expect_s3_class(dplyr::rowwise(tabs), "tab")
})

test_that("dplyr::mutate preserves class tab", {
  expect_s3_class(dplyr::mutate(tabs, Married = sum(Married)), "tab")
})

test_that("dplyr::transmute preserves class tab", {
  expect_s3_class(dplyr::transmute(tabs, race = race, Married = sum(Married)), "tab")
})

test_that("dplyr::filter preserves class tab", {
  expect_s3_class(dplyr::filter(tabs, is_totrow(Married)), "tab")
})

test_that("dplyr::slice preserves class tab", {
  expect_s3_class(dplyr::slice(tabs, 1:2), "tab")
})

test_that("dplyr::arrange preserves class tab", {
  expect_s3_class(dplyr::arrange(tabs, Married), "tab")
})

test_that("dplyr::distinct preserves class tab", {
  expect_s3_class(dplyr::distinct(tabs), "tab")
})

test_that("dplyr::select preserves class tab", {
  expect_s3_class(dplyr::select(tabs, race, Married), "tab")
})

test_that("dplyr::rename, rename_with and relocate preserves class tab", {
  expect_s3_class(dplyr::relocate   (tabs, Divorced , .after = Married), "tab")
  expect_s3_class(dplyr::rename     (tabs, new_name = race), "tab")
  expect_s3_class(dplyr::rename_with(tabs, toupper), "tab")
})

test_that("[<- and [[<- preserves class tab", {
  tabs[4]     <- dplyr::mutate(tabs[4], dplyr::across(.fns = ~ set_display(., "ctr")))
  tabs[[2]]   <- tabs[[2]] |> set_digits(3)
  tabs[[2, 1]] <- factor("White")
  expect_s3_class(tabs, "tab")
})



grouped_tabs <- forcats::gss_cat |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)

test_that("dplyr::ungroup preserves class tab", {
  expect_s3_class(dplyr::ungroup(grouped_tabs), "tab")
})

test_that("dplyr::summarise, preserves class tab", {
  expect_s3_class(dplyr::summarise (grouped_tabs, Married = sum(Married)), "tab")
})


test_that("dplyr::rowwise preserves class grouped_tab", {
  expect_s3_class(dplyr::rowwise(grouped_tabs), "grouped_tab")
})

test_that("dplyr::mutate preserves class grouped_tab", {
  expect_s3_class(dplyr::mutate(grouped_tabs, Married = sum(Married)), "grouped_tab")
})

test_that("dplyr::transmute preserves class grouped_tab", {
  expect_s3_class(dplyr::transmute(grouped_tabs, year = year, race = race, Married = sum(Married)), "grouped_tab")
})

test_that("dplyr::filter preserves class grouped_tab", {
  expect_s3_class(dplyr::filter(grouped_tabs, is_totrow(Married)), "grouped_tab")
})

test_that("dplyr::slice preserves class grouped_tab", {
  expect_s3_class(dplyr::slice(grouped_tabs, 1:2), "grouped_tab")
})

test_that("dplyr::arrange preserves class grouped_tab", {
  expect_s3_class(dplyr::arrange(grouped_tabs, Married), "grouped_tab")
})

test_that("dplyr::distinct preserves class grouped_tab", {
  expect_s3_class(dplyr::distinct(grouped_tabs), "grouped_tab")
})

test_that("dplyr::select preserves class grouped_tab", {
  expect_s3_class(dplyr::select(grouped_tabs, year, race, Married), "grouped_tab")
})

test_that("dplyr::rename, rename_with and relocate preserves class grouped_tab", {
  expect_s3_class(dplyr::relocate   (grouped_tabs, Divorced , .after = Married), "grouped_tab")
  expect_s3_class(dplyr::rename     (grouped_tabs, new_name = year), "grouped_tab")
  expect_s3_class(dplyr::rename_with(grouped_tabs, toupper), "grouped_tab")
})

test_that("[<- and [[<- preserves class grouped_tab", {
  grouped_tabs[4]     <- dplyr::mutate(grouped_tabs[4], dplyr::across(.fns = ~ set_display(., "ctr")))
  grouped_tabs[[2]]   <- grouped_tabs[[2]] |> forcats::fct_recode("kéké" = "Black")
  grouped_tabs[[2,2]] <- factor("White")
  expect_s3_class(grouped_tabs, "grouped_tab")
})

