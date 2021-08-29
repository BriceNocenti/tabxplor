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

  tabs %>%
    tab_xl(path = "tests\\tab_xl_test", sheets = "unique",
           replace = TRUE, open = FALSE) %>%
    testthat::expect_invisible()

 testthat::expect_true(file.exists("tests\\tab_xl_test.xlsx"))

 file.remove("tests\\tab_xl_test.xlsx")
})


