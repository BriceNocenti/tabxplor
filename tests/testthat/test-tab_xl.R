testthat::test_that("tab_xl creates an Excel file", {
  testthat::skip_if_not_installed("openxlsx2")
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
  testthat::skip_if_not_installed("openxlsx2")
  withr::local_options(lifecycle_verbosity = "quiet")
  tabs <-tab(forcats::gss_cat, race, marital, pct = "row", color = "after_ci")

  test_path <- file.path(tempdir(), "tab_xl_test.xlsx")

  tabs %>%
    tab_xl(path = test_path, sheets = "unique",
           replace = TRUE, open = FALSE) %>%
    testthat::expect_invisible()

  testthat::expect_true(file.exists(test_path))

  file.remove(test_path)
})

# Phase 10g: read the written workbook back and confirm the raw get_num() values reached the file
# (Excel stores the raw value; the "%" is a display-only numFmt). Closes the "no test inspects the
# written file" gap.
testthat::test_that("tab_xl writes get_num() values that round-trip from the file", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  testthat::expect_true(file.exists(p))

  wb <- openxlsx2::wb_load(p)
  testthat::expect_gte(length(openxlsx2::wb_get_sheet_names(wb)), 1)

  d    <- openxlsx2::wb_to_df(p, sheet = 1, col_names = FALSE, convert = TRUE)
  vals <- round(suppressWarnings(as.numeric(unlist(d, use.names = FALSE))), 6)
  vals <- vals[!is.na(vals)]

  fmt_names <- names(tb)[purrr::map_lgl(tb, is_fmt)]
  want <- round(get_num(tb[[fmt_names[[1]]]]), 6)
  want <- want[!is.na(want)]
  testthat::expect_true(all(want %in% vals))
})

# Phase 10g: a non-tabxplor data.frame degrades gracefully (plain sheet + message, still writes).
testthat::test_that("tab_xl degrades to a plain sheet for a non-tabxplor data.frame", {
  testthat::skip_if_not_installed("openxlsx2")
  p <- withr::local_tempfile(fileext = ".xlsx")
  testthat::expect_message(
    tab_xl(tibble::tibble(a = 1:3, b = letters[1:3]), path = p, open = FALSE),
    "skipped"
  )
  testthat::expect_true(file.exists(p))
})

# Phase 10h: significance stars are folded into the Excel numFmt code (0.0%"***"), keeping the cell a
# real number. Gated by the same option as the text path (getOption("tabxplor.stars")).
testthat::test_that("tab_xl folds significance stars into the numFmt code", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff",
            color_signif = "color_all_signif")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  codes <- openxlsx2::wb_load(p)$styles_mgr$styles$numFmts
  testthat::expect_true(any(grepl("\\*", codes)))                 # a code carries the star literal

  # with stars off, no star literal is written
  withr::local_options(tabxplor.stars = FALSE)
  p2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p2, sheets = "unique", replace = TRUE, open = FALSE))
  codes2 <- openxlsx2::wb_load(p2)$styles_mgr$styles$numFmts
  testthat::expect_false(any(grepl("\\*", codes2)))
})

# Phase 10h: transpose = TRUE exports the transposed table (still a valid, readable workbook).
testthat::test_that("tab_xl(transpose = TRUE) writes a valid workbook", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, replace = TRUE, open = FALSE, transpose = TRUE))
  testthat::expect_true(file.exists(p) && file.size(p) > 0)
})

# Phase 10h: conditional_format is experimental (message + falls back to hard styles, still writes).
testthat::test_that("tab_xl(conditional_format = TRUE) informs and falls back", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", color = "diff")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  testthat::expect_message(
    tab_xl(tb, path = p, replace = TRUE, open = FALSE, conditional_format = TRUE),
    "experimental"
  )
  testthat::expect_true(file.exists(p))
})
