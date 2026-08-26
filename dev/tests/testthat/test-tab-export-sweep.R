
# === SECTION: the tab_export() facade =============================================================

t_row <- tab(fx_gss(), race, marital, pct = "row", color = c("diff", "ratio"))


testthat::test_that("transpose swaps the table axes at export", {
  t_col <- tab(fx_gss(), race, marital, pct = "col", color = "diff")
  base  <- tab_md(t_col, print = FALSE)
  trans <- tab_md(t_col, transpose = TRUE, print = FALSE)
  testthat::expect_false(identical(base, trans))
})


testthat::test_that("tab_xl is theme-aware (dark palette differs from light)", {
  fl <- tempfile(fileext = ".xlsx"); fd <- tempfile(fileext = ".xlsx")
  tab_xl(t_row, path = fl, theme = "light", open = FALSE, replace = TRUE)
  tab_xl(t_row, path = fd, theme = "dark",  open = FALSE, replace = TRUE)
  # a coloured table written under two themes produces two different workbooks
  testthat::expect_false(identical(readBin(fl, "raw", file.size(fl)),
                                   readBin(fd, "raw", file.size(fd))))
})


testthat::test_that("deprecated tab_xl(print_color_legend) still feeds color_legend", {
  f <- tempfile(fileext = ".xlsx")
  lifecycle::expect_deprecated(
    tab_xl(t_row, path = f, print_color_legend = FALSE, open = FALSE, replace = TRUE)
  )
})


testthat::test_that("contrib tables render through every exporter (comp tab/all, +/- tab_vars)", {
  # Regression for two Phase 10j-B crashes: the colour engine (get_mean_contrib size 0 under
  # comp = "all" without a total table) and the kable tooltip (cond_ctr NA on the Total column,
  # which broke ANY contrib table via tab_kable, incl. the default comp = "tab").
  gss <- fx_gss()
  contribs <- list(
    tab     = tab(gss, marital, race, pct = "row", color = "contrib"),
    all     = suppressWarnings(tab(gss, marital, race, pct = "row", color = "contrib", comp = "all")),
    all_tab = tab(gss, marital, race, tab_vars = year, pct = "row", color = "contrib", comp = "all")
  )
  for (nm in names(contribs)) {
    t <- contribs[[nm]]
    testthat::expect_no_error(as.character(tab_kable(t)))                    # crash 2 (tooltip)
    testthat::expect_type(tab_md(t, print = FALSE), "character")
    f <- tempfile(fileext = ".xlsx")
    testthat::expect_no_error(tab_xl(t, path = f, open = FALSE, replace = TRUE))  # crash 1
  }
})


# === SECTION: every backend runs ==================================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


gss <- fx_gss()


testthat::test_that("tab_kable works with grouped tables", {
  testthat::skip_if_not_installed("kableExtra")

  gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
  tabs <- tab(gss_sub, race, marital, year, pct = "row", color = "diff")
  result <- tab_kable(tabs)
  testthat::expect_true(!is.null(result))
})


testthat::test_that("tab_kable accepts theme='light' and theme='dark'", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  testthat::expect_no_error(tab_kable(tabs, theme = "light"))
  testthat::expect_no_error(tab_kable(tabs, theme = "dark"))
})


testthat::test_that("tab_kable get_data returns a data.frame", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  result <- tab_kable(tabs, get_data = TRUE)
  testthat::expect_s3_class(result, "data.frame")
})


testthat::test_that("tab_kable works with counts (no color)", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital)
  testthat::expect_no_error(tab_kable(tabs))
})


testthat::test_that("tab_kable works with chi2 subtext", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "diff")
  testthat::expect_no_error(tab_kable(tabs))
})


# Phase 20h: the prepared starwars fixture, built ONCE at top level -- where the file-level
# lifecycle line above actually bites (testthat re-enables the warning inside every
# test_that()). It was written verbatim in each block below.
sw_prepared <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)


testthat::test_that("tab_kable works with numeric tables", {
  testthat::skip_if_not_installed("kableExtra")

  sw <- sw_prepared
  tabs <- tab_num(sw, sex, height, na = "drop", color = "diff")
  testthat::expect_no_error(tab_kable(tabs))
})


testthat::test_that("tab_kable with contrib color works", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "contrib")
  testthat::expect_no_error(tab_kable(tabs))
})


testthat::test_that("tab_xl works with contrib color type", {
  testthat::skip_if_not_installed("openxlsx2")

  tabs <- tab(gss, race, marital, pct = "row", test = TRUE, color = "contrib")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  tab_xl(tabs, path = tmp, open = FALSE)
  testthat::expect_true(file.exists(tmp))
})


testthat::test_that("tab_xl works with grouped tables", {
  testthat::skip_if_not_installed("openxlsx2")

  gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
  tabs <- tab(gss_sub, race, marital, year, pct = "row", color = "diff")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  tab_xl(tabs, path = tmp, open = FALSE)
  testthat::expect_true(file.exists(tmp))
})


testthat::test_that("tab_xl works with numeric tables", {
  testthat::skip_if_not_installed("openxlsx2")

  sw <- sw_prepared
  tabs <- tab_num(sw, sex, height, na = "drop", color = "diff")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  tab_xl(tabs, path = tmp, open = FALSE)
  testthat::expect_true(file.exists(tmp))
})


# === SECTION: the tab_export() facade =============================================================

t_row <- tab(fx_gss(), race, marital, pct = "row", color = c("diff", "ratio"))


testthat::test_that("color = FALSE renders monochrome (no colour spans)", {
  col <- tab_md(t_row, color = TRUE,  print = FALSE)
  mon <- tab_md(t_row, color = FALSE, print = FALSE)
  # the COLOUR-slot spans (.p1/.m2/.o3/.u4), not the chrome ones a monochrome table still carries
  # (`.tx-unit`, the header's unit line -- Phase 22c-ii).
  slot <- "\\]\\{\\.[pmou][0-9]"
  testthat::expect_true(grepl(slot, col))                  # coloured pandoc spans present
  testthat::expect_false(grepl(slot, mon))                 # none when monochrome
})


# === SECTION: every backend runs ==================================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


gss <- fx_gss()


# === SECTION: tab_kable =======================================================

testthat::test_that("tab_kable returns a kable object", {
  testthat::skip_if_not_installed("kableExtra")

  tabs <- tab(gss, race, marital, pct = "row", color = "diff")
  result <- tab_kable(tabs)
  testthat::expect_true(
    inherits(result, "knitr_kable") | inherits(result, "kableExtra") |
      is.character(result)
  )
})


# Phase 20h: the prepared starwars fixture, built ONCE at top level -- where the file-level
# lifecycle line above actually bites (testthat re-enables the warning inside every
# test_that()). It was written verbatim in each block below.
sw_prepared <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)
