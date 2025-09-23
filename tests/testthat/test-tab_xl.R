testthat::test_that("tab_xl creates an Excel file", {
  tabs <-
    purrr::pmap(
      tibble::tribble(
        ~row_var, ~col_vars       , ~pct , ~filter              , ~subtext               ,
        "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
        "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
        NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
      ),
      .f = tab_many,
      data = forcats::gss_cat, color = "auto", chi2 = TRUE)

  test_path <- file.path(tempdir(), "tab_xl_test.xlsx")

  tabs %>%
    tab_xl(path = test_path, sheets = "unique",
           replace = TRUE, open = FALSE) %>%
    testthat::expect_invisible()

 testthat::expect_true(file.exists(test_path))

 file.remove(test_path)
})

testthat::test_that("tab_xl work with  after_ci", {
  tabs <-tab(forcats::gss_cat, race, marital, pct = "row", color = "after_ci")

  test_path <- file.path(tempdir(), "tab_xl_test.xlsx")

  tabs %>%
    tab_xl(path = test_path, sheets = "unique",
           replace = TRUE, open = FALSE) %>%
    testthat::expect_invisible()

  testthat::expect_true(file.exists(test_path))

  file.remove(test_path)
})


