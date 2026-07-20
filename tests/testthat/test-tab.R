data <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)

# starwars |> dplyr::select(where(is.character)) |> purrr::map(~ as.factor(.) |>
#   levels())

# dplyr::storms
# forcats::gss_cat

testthat::test_that("tab_plain works with missing variables, NAs, etc.", {
  tab_plain(data, sex)                                                     |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, col_var = hair_color)                                    |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, wt = mass)                              |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, wt = mass)                      |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, row_var = hair_color, col_var = NULL, gender, wt = mass) |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, row_var = NULL, col_var = hair_color, gender, wt = mass) |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, wt = mass, na = "drop")         |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, sex)                      |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, sex, gender, na = "drop") |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, sex, gender, wt = mass)   |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, "gender", "sex", NA_character_) |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, "gender", NA_character_)        |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, NA_character_, "sex")           |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, "gender", "sex", NULL)          |> testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("tab_plain works with num and df", {
  tab_plain(data, sex, hair_color, num = TRUE)                    |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, df = TRUE)                     |> testthat::expect_s3_class("data.frame")

  tab_plain(data, sex, hair_color, gender, wt = mass, num = TRUE) |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, wt = mass, df = TRUE)  |> testthat::expect_s3_class("data.frame")

})

testthat::test_that("tab_plain works with totals and total table", {
  tab_plain(data, sex, hair_color, tot = c("row", "col"))         |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, tot = c("row", "col")) |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, totaltab = "line")     |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, totaltab = "table")    |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, totaltab = "no")       |> testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("tab_plain works with pct and diffs", {
  tab_plain(data, sex, hair_color, pct = "row")                       |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, pct = "col")                       |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, pct = "all")                       |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, pct = "all_tabs")                  |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, hair_color, gender, pct = "row")               |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "col")               |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "all")               |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "all_tabs")          |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, hair_color, pct = "row", ref = "^male")        |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "row", ref = 2)     |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, hair_color, gender, pct = "row", comp = "all") |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "row", ref = "tot",
            comp = "all")                                             |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "row", ref = 3,
            comp = "all", totaltab = "table")                         |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, hair_color, pct = "col", ref = "brown")       |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, pct = "col", ref = 3)             |> testthat::expect_s3_class("tabxplor_tab")

  #warnings
  tab_plain(data, sex, hair_color, pct = "row", ref = 47)                |> testthat::expect_warning()
  tab_plain(data, sex, hair_color, pct = "row", ref = "no_existing_cat") |> testthat::expect_warning()
  tab_plain(data, sex, hair_color, pct = "col", ref = 47)                |> testthat::expect_warning()
  tab_plain(data, sex, hair_color, pct = "col", ref = "no_existing_cat") |> testthat::expect_warning()
  #tab_plain(data, sex, hair_color, pct = "col", comp = "all")             |> testthat::expect_warning()
  #tab_plain(data, sex, hair_color, gender, pct = "col", ref = "black", comp = "all") |> testthat::expect_warning()
  #tab_plain(data, sex, hair_color, gender, pct = "col", comp = "all")     |> testthat::expect_warning()
})

testthat::test_that("tab_plain works with OR", {
  tab_plain(data, sex, hair_color, pct = "row", OR = "OR")            |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, pct = "col", OR = "OR_pct")        |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, hair_color, pct = "row", OR = "OR", ref = "^male")       |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "row", OR = "OR", ref = 2)     |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, hair_color, gender, pct = "row", OR = "OR", ref = "tot",
            comp = "all")                                             |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "row", OR = "OR", ref = 3,
            comp = "all", totaltab = "table")                         |> testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("tab_num works with missing, NULL, NA, etc.", {
  # set_color_breaks(mean_breaks = c(1.05, 1.10, 1.20, 1.50))
  tab_num(data, sex, height, na = "drop")                                       |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, height, wt = mass)                                         |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, height, ref = "no", ci = "no", tot = "row")                |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year))                                     |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), gender, tot = "row",totaltab = "table") |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), c(gender, eye_color), comp = "all") |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, col_var = birth_year)                                           |> testthat::expect_s3_class("tabxplor_tab")
  })


testthat::test_that("tab_num works with diff and ci", {
  withr::local_options(lifecycle_verbosity = "quiet")
  tab_num(data, sex, c(height, birth_year), na = "drop", ref = "no")        |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), na = "drop")

  tab_num(data, sex, c(height, birth_year), na = "drop", color = "diff")     |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), na = "drop", color = "diff_ci")  |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), na = "drop", color = "after_ci") |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), na = "drop", color = "")         |> testthat::expect_s3_class("tabxplor_tab")

  tab_num(data, sex, c(height, birth_year), na = "drop", ref = "^male")     |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), na = "drop", ref = 3,
          color = "diff_ci", tot = "row")                                    |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), na = "drop", color = "after_ci") |> testthat::expect_s3_class("tabxplor_tab")

  tab_num(data, sex, c(height, birth_year), na = "drop", ci = "cell")        |> testthat::expect_s3_class("tabxplor_tab")

  tab_num(data, sex, c(height, birth_year), na = "drop", ci = "diff")        |> testthat::expect_s3_class("tabxplor_tab")

})

testthat::test_that("tab_num works with with df and num", {
  tab_num(data, sex, c(height, birth_year), na = "drop",
          tot = "row", totaltab = "table", num = TRUE)         |> testthat::expect_s3_class("tabxplor_tab")
  tab_num(data, sex, c(height, birth_year), gender, na = "drop",
          tot = "row", totaltab = "table", num = TRUE)         |> testthat::expect_s3_class("tabxplor_tab")

  tab_num(data, sex, c(height, birth_year), na = "drop",
          tot = "row", totaltab = "table", df = TRUE)          |> testthat::expect_s3_class("data.frame")
  tab_num(data, sex, c(height, birth_year), gender, na = "drop",
          tot = "row", totaltab = "table",df = TRUE) |> testthat::expect_s3_class("data.frame")
})


testthat::test_that("tab et tab_many works with missing, NULL, NA, etc., in variables", {
  tab(data, "gender", "sex", NA_character_) |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", NA_character_)        |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, NA_character_, "sex")           |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", "sex", NULL)          |> testthat::expect_s3_class("tabxplor_tab")
  #tab(data, "gender", "sex", "")           |> testthat::expect_s3_class("tabxplor_tab")
  #tab(data, "gender", "sex", "no")         |> testthat::expect_s3_class("tabxplor_tab")

  tab(data, "gender")                                                     |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", wt = mass)                                          |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", col_vars = NULL         , tab_vars = NULL)          |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", col_vars = NA_character_, tab_vars = NA_character_) |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", col_vars = ""           , tab_vars = "")            |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", col_vars = "no"         , tab_vars = "no")          |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, gender, col_vars = hair_color , tab_vars = sex)               |> testthat::expect_s3_class("tabxplor_tab")
})


testthat::test_that("tab works with numeric variables", {
  tab(data, sex, mass)         |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, sex, mass, gender) |> testthat::expect_s3_class("tabxplor_grouped_tab")
})

testthat::test_that("tab works with several col_vars", {
  tab(data, sex, c(hair_color, eye_color), pct = "row")            |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, sex, c(hair_color, mass, gender), pct = "row")         |> testthat::expect_s3_class("tabxplor_tab")
})

# Phase 8 total-col decoupling (tab_assemble ~L1770): with several row_vars + several factor col_vars,
# the lone kept total column must read "Total", not the internal "Total_<lastcv>" that leaked before the
# dedup fix. This makes a multi-row_var per-row_var table identical to a standalone single-row_var build
# -- the precondition for the per-row_var parallel dispatch (test-parallel-parity.R).
testthat::test_that("Phase 8: multi-row_var total column is 'Total' (not 'Total_<col_var>')", {
  multi  <- tab(data, c(sex, gender), c(hair_color, eye_color), pct = "row")
  testthat::expect_true("Total" %in% names(multi))
  testthat::expect_false(any(grepl("^Total_", names(multi))))
})

# Coverage of the soft-deprecated tab_many() alias's own features that tab() intentionally does
# NOT expose (per-col_var pct vector, per-row_var pct list, list return). suppressWarnings() keeps
# the deprecation nudge out (see also the dedicated deprecation test below).
testthat::test_that("tab_many() (deprecated alias) per-variable pct vectorisation still works", {
  suppressWarnings({
    tab_many(data, sex, c(hair_color, mass, gender), pct = c("row", NA, "col"))   |> testthat::expect_s3_class("tabxplor_tab")
    tab_many(data, c(sex, gender), hair_color, pct = c("row", "col")) |> length() |> testthat::expect_equal(2)
    tab_many(data, c(sex, eye_color), c(hair_color, mass, gender),
             pct = list(sex = list("row", "col", "col"), eye_color = list("col", "row", "row"))
    ) |>
      length() |> testthat::expect_equal(2)
  })
})

testthat::test_that("Phase 6: output_list / merge / deprecations / KNOWN-BUG fix", {
  gss <- forcats::gss_cat
  # §13 output shape via tab()
  tab(gss, marital, race, pct = "row")                     |> testthat::expect_s3_class("tabxplor_tab")
  tab(gss, marital, race, pct = "row", output_list = TRUE) |> testthat::expect_type("list")
  merged <- tab(gss, c(marital, relig), race, pct = "row")
  testthat::expect_true(is.data.frame(merged) && "row_var" %in% names(merged))
  tab(gss, c(marital, relig), race, pct = "row", output_list = TRUE) |> length() |> testthat::expect_equal(2)

  # row_var axis globalised on tab(): OR/ci/chi2 must be scalar
  testthat::expect_error(tab(gss, c(marital, relig), race, pct = "col", OR = c("OR", "no")))

  # totrow / totcol soft-deprecated on tab_many (Phase 6e). Each call raises TWO deprecations --
  # tab_many() itself (Phase 6f) plus the argument -- so both must be caught, innermost first, or
  # the uncaught one surfaces as a test warning.
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(tab_many(gss, marital, race, totrow = FALSE), "totrow"),
    "tab_many")
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(tab_many(gss, marital, race, totcol = "no"), "totcol"),
    "tab_many")

  # Deliberate user-facing warnings. Asserted here because other suites (test-jmvtab-cache.R)
  # suppress them as incidental, so without this they would be uncovered.
  # comp = "all" with a ref that is not the total row forces the full total table (a `ref = "tot"`
  # comparison only needs a total LINE, and warns differently).
  testthat::expect_warning(
    tab(gss, marital, race, tab_vars = year, pct = "row", color = "diff", comp = "all",
        ref = "Married"),
    "full total table")
  testthat::expect_warning(
    tab(gss, marital, race, pct = "row", color = "diff", ref = "no-such-level"),
    "no rows were found as reference")

  # KNOWN-BUG fixed: tab_num(<tab_vars>, ci="cell") no longer crashes (both comp modes)
  testthat::expect_no_error(tab_num(gss, race, age, marital, ci = "cell"))
  testthat::expect_no_error(tab_num(gss, race, age, marital, ci = "cell", comp = "all"))

  # na = "common_base" (Phase 6g): for a single col_var it equals the old-tab() na = "drop"
  cb <- tab(gss, marital, race, pct = "row", na = "common_base")
  dr <- tab(gss, marital, race, pct = "row", na = "drop")
  testthat::expect_equal(vctrs::vec_data(cb), vctrs::vec_data(dr))

  # spread_vars (Phase 6i): pivot a tab_var into columns; must be among tab_vars
  sp <- tab(gss, marital, race, relig, pct = "row", spread_vars = relig)
  testthat::expect_s3_class(sp, "tabxplor_tab")
  testthat::expect_gt(ncol(sp), ncol(tab(gss, marital, race, relig, pct = "row")))
  testthat::expect_error(tab(gss, marital, race, relig, spread_vars = marital))
})

testthat::test_that("tab drops NA consistently with na = 'drop'", {
  tabs1 <- tab(data, gender, hair_color, sex, na = "drop")
  testthat::expect_true(all(!stringi::stri_detect_regex(dplyr::pull(tabs1, sex), "^NA")))
})

# Coverage of tab_many()-only controls that tab() intentionally does not expose: `levels`
# (per-col_var level selection), `na = "drop_all"`, and `na_drop_all =`. suppressWarnings()
# keeps the soft-deprecation nudge out of these dedicated alias tests.
testthat::test_that("tab_many() (deprecated alias) levels / na_drop_all features still work", {
  suppressWarnings({
    tabs1 <- tab_many(data, sex, c(hair_color, eye_color), pct = "row", levels = "first")
    testthat::expect_false("brown_hair_color" %in% names(tabs1))

    tabs2 <- tab_many(data, sex, c(hair_color, eye_color), pct = "row", levels = c("first", "all"))
    testthat::expect_false("brown_hair_color" %in% names(tabs2))
    testthat::expect_true("orange" %in% names(tabs2))

    tabs3 <- tab_many(data, gender, hair_color, sex, na = "drop_all")
    testthat::expect_true(all(!stringi::stri_detect_regex(dplyr::pull(tabs3, sex), "^NA")))

    tabs4 <- tab_many(data, gender, hair_color, sex, na_drop_all = gender)
    testthat::expect_true(all(!stringi::stri_detect_regex(dplyr::pull(tabs4, sex), "^NA")))
    testthat::expect_true(any(stringi::stri_detect_regex(names(tabs4), "^NA")))
  })
})


testthat::test_that("all tab functions works with no tab_vars", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data |> #with no tab_vars
    tab_plain(sex, hair_color, wt = mass, pct = "row") |>
    #tab_totaltab() |>
    #tab_tot() |>
    #tab_pct() |>
    tab_ci("diff", color = "after_ci") |>
    tab_chi2() |>
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("all tab functions works with no col_var", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data |>
    tab_plain(sex, pct = "col") |>
    #tab_totaltab() |>
    #tab_tot() |>
    #tab_pct("col") |>
    tab_ci("diff", color = "after_ci") |>
    tab_chi2() |>
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("all tab functions works with no row_var", {
  data |>
    tab_plain(col_var = hair_color, tot = c("row", "col"), pct = "row") |>
    #tab_totaltab() |> error
    #tab_tot() |>
    #tab_pct() |>
    tab_ci() |>
    tab_chi2() |>
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("all tab functions works with totaltab = 'line'", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data |>
    tab_plain(sex, hair_color, gender, pct = "row") |>
    #tab_totaltab("line") |>
    #tab_tot() |>
    #tab_pct() |>
    tab_ci("diff", color = "after_ci") |>
    tab_chi2() |>
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("tab_num works (with color)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  testthat::expect_true(
    !is.na(tab_prepare(data, sex, mass) |>
             tab_num(sex, mass, tot = "row", ref = "tot", color = "after_ci") |>

             tab_chi2() |>
             dplyr::pull(mass) |> vec_data() |> dplyr::pull(var) |> dplyr::last())
  )
})

testthat::test_that("tab_many work with tribble", {

  tibble::tribble(
    ~row_var, ~col_vars                           , ~tab_vars     , ~levels,
    "sex"   , "hair_color"                        , NA_character_ , "all"  ,
    "sex"   , c("mass", "hair_color", "eye_color"), "gender"      , "first",
    "sex"   , c("hair_color", "eye_color", "mass"), "gender"      , "all"  ,
  ) |>
    purrr::pmap(tab_many, data = data, totcol = "no", totaltab = "no") |>
    testthat::expect_type("list")

  # not needed, since the opportunity of proceeding that way is not clear ?
  # purrr::map(tabs, ~ tab_totaltab(.) |>
  #              tab_tot() |>
  #              tab_pct() |>
  #              tab_ci() |>
  #              tab_chi2()
  # )
})

testthat::test_that("tab work with tribble (even many tab_vars)", {
  tibble::tribble(
    ~row_var, ~col_var    , ~tab_vars                 ,
    "sex"   , "hair_color", NA_character_             ,
    "sex"   , "mass"      , "gender"                  ,
    "sex"   , "eye_color" , c("gender",  "hair_color"),
  ) |>
    purrr::pmap(tab, data = data) |>
    testthat::expect_type("list")
})


# tabs <- tab_many(data, "sex", c("hair_color", "eye_color", "mass"), "gender",
#                  totaltab = "line", totcol = "no")
#
# testthat::test_that("tab_totaltab works with all arguments (and with tab_tot)", {
#   testthat::expect_true(
#     nrow(tabs |> tab_totaltab("line") |> tab_totaltab("no") |> tab_totaltab("table")|>
#            dplyr::filter_at(1, ~ stringi::stri_detect_regex(., "^Ensemble")) ) != 0,
#   )
#
#   testthat::expect_identical(
#     nrow(tabs |>
#            tab_totaltab() |> tab_tot() |>
#            dplyr::filter_at(1, ~ stringi::stri_detect_regex(., "^Ensemble")) ),
#
#     nrow(tabs |> tab_totaltab(name = "Overall", data = data) |>
#            dplyr::filter_at(1, ~ stringi::stri_detect_regex(., "^Overall"))  ) + 1L
#   )
#
#   testthat::expect_identical(
#     nrow(tabs |> tab_totaltab("line") |> tab_tot() |>
#            dplyr::filter_at(1, ~ stringi::stri_detect_regex(., "^Ensemble")) ),
#     1L
#   )
# })


# tabs <- tabs |> tab_totaltab()
#
# testthat::test_that("tab_tot works with all arguments", {
#   tabs |> tab_tot("col") |> tab_tot("row") |> tab_tot("no") |>
#     testthat::expect_s3_class("tabxplor_tab")
#
#   tabs |> tab_tot(totcol = "each") |> testthat::expect_s3_class("tabxplor_tab")
# })
# #tab_tot("row") can't be done on different groups of rows independently
# # tabs[is_tottab(tabs),] <- tabs[is_tottab(tabs),] |> tab_tot("row")
#
# testthat::test_that("tab_pct works with groups, ungroup, and warnings", {
#
#   tabs |> tab_tot("col") |> dplyr::ungroup() |> tab_pct("row") |>
#     testthat::expect_warning("no groups nor total row")
#
#   tabs |> tab_tot("row")  |> tab_pct("col") |>
#     testthat::expect_warning("no total column")
#
#   testthat::expect_false( # return col_all
#     tabs |> tab_tot() |> dplyr::ungroup() |> tab_pct("col") |> dplyr::ungroup() |>
#       dplyr::select(where(~is_fmt(.) & ! get_type(.) == "mean")) |>
#       dplyr::filter(is_totrow(.) & ! is_tottab(.)) |>
#       dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ get_pct(.) == 1)) |>
#       dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = all)) |>
#       purrr::map_lgl(~ . ) |> all()
#   )
#
#   testthat::expect_equal(
#     tabs |> tab_tot() |> dplyr::ungroup() |> tab_pct("all") |>
#       dplyr::mutate(dplyr::across(where(is_fmt), get_pct)) |>
#       `attr<-`("groups", NULL),
#
#     tabs |> tab_tot() |> dplyr::ungroup() |> tab_pct("all_tabs") |>
#       dplyr::mutate(dplyr::across(where(is_fmt), get_pct)) |>
#       `attr<-`("groups", NULL)
#   )
#
# })

# testthat::test_that("tab_pct works with tot = 'each'", {
#   tabs2 <- tabs |> tab_tot(totcol = "each")
#   tabs2 |> tab_pct("row")      |> testthat::expect_s3_class("tabxplor_tab")
#   tabs2 |> tab_pct("col")      |> testthat::expect_s3_class("tabxplor_tab")
#   tabs2 |> tab_pct("all")      |> testthat::expect_s3_class("tabxplor_tab")
#   tabs2 |> tab_pct("all_tabs") |> testthat::expect_s3_class("tabxplor_tab")
# })
#
#
# tabs <- tabs |> tab_tot() |>
#   dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp_all(., NA)))
#
# testthat::test_that("tab_ci works (with tab_pct)", {
#   tabs |> tab_pct("row") |> tab_ci("diff", comp = "all") |>
#     testthat::expect_warning("comp were set to 'tab'")
#
#   tabs |> tab_pct("row", comp = "all") |> tab_ci("diff", color = "after_ci") |>
#     testthat::expect_s3_class("tabxplor_tab")
#
#   tabs |> tab_pct("col") |> tab_ci(color = "diff_ci") |> testthat::expect_s3_class("tabxplor_tab")
#
#   testthat::expect_true(
#     tabs |> tab_pct("row") |> tab_ci("cell", visible = TRUE) |>
#       dplyr::ungroup() |>
#       dplyr::mutate(dplyr::across(
#         where(is_fmt), ~ stringi::stri_detect_regex(format(.),
#                                              stringi::stri_unescape_unicode("\\u00b1")))) |>
#       dplyr::summarise(dplyr::across(where(is.logical), any)) |>
#       purrr::map_lgl(~ .) |> any()
#   )
#
#   # tabs |> tab_pct("all")               |> tab_ci("cell", visible = TRUE)  |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#
#   tabs |> tab_pct("all_tabs") |> tab_ci("cell", color = "after_ci") |>
#     testthat::expect_s3_class("tabxplor_tab")
# })
# Can we sum variances for means ? Answer : no, weighted mean is an approximation
# tabs1 <- tab_plain(data, PE0, REVMENSC, PR0, EMP_ADM_ENT) |> tab_ci() |>
#   dplyr::mutate(sd = get_sd(REVMENSC), wn = get_wn(REVMENSC), n = get_n(REVMENSC))
# tabs2 <- tab_plain(data, EMP_ADM_ENT, REVMENSC, PR0) |> tab_ci() |>
#   dplyr::mutate(sd = get_sd(REVMENSC), wn = get_wn(REVMENSC), n = get_n(REVMENSC))
#
# tabs2
# tabs1 |> dplyr::summarise(REVMENSC = mean(REVMENSC),
#                            sd = sqrt(sum(sd ^ 2 * wn)/sum(wn)),
#                            wn = sum(wn), n = sum(n) )


# tabs <- tabs |> tab_pct("row") |> tab_ci("diff", color = "after_ci") |> tab_chi2()
#
# testthat::test_that("tab_chi2 table is the expected one", {
#
#   tabs |> get_chi2() |>
#     dplyr::select(where(is_fmt)) |>
#     dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = get_num)) |>
#     purrr::map(~ .) |>
#     testthat::expect_snapshot_value()
#
# })

# testthat::test_that("tab_chi2 contributions to variance work", {
# ctr <- tabs |> dplyr::ungroup() |>
#     dplyr::transmute(dplyr::across(where(is_fmt), ~ set_display(., "ctr")))
#
# ctr |> dplyr::filter(is_totrow(.)) |>
#   dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ get_ctr(.)))
#
# ctr |> dplyr::filter(!is_totrow(.)) |>
#   dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ get_ctr(.)))
# })

#' @keywords internal
expect_color <- function(object) {
  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect() -- a cell is coloured if either channel returns a non-zero palette slot
  ch <- fmt_color_channels(act$val)
  act$color <- ch$text_slot != 0L | ch$bg_slot != 0L
  testthat::expect(
    any(act$color),
    sprintf("%s doesn't return any colored cell.", act$lab)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

testthat::test_that("printing colors works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # Phase 14l: the `tabxplor.color_style_type` option is deprecated + inert (it repointed the text
  # channel into the fill palette -- the CHANNEL is now `color = c(text, background)`), so the legs no
  # longer toggle it. `color_style_theme` (light/dark) is what makes them distinct.
  withr::defer(options("tabxplor.color_style_theme" = "light"))
  options("tabxplor.color_style_theme" = "dark")
  tab(data, sex, hair_color, pct = "row", color = "diff"    ) |> print() |>
    testthat::expect_output()
  set_color_breaks(list(pct_diff = c(0.05, 0.15, 0.3), pct_ratio = list(over = 2),
                        mean_ratio = c(1.15, 2, 4), contrib = c(1, 2, 5)))
  tab(data, sex, hair_color, pct = "row", color = "diff_ci" ) |> print() |>
    testthat::expect_output()
  options("tabxplor.color_style_theme" = "light")
  tab(data, sex, hair_color, pct = "row", color = "after_ci") |> print() |>
    testthat::expect_output()

  set_color_breaks(list(pct_diff = c(0.05, 0.1, 0.2, 0.3), pct_ratio = list(over = 2),
                        mean_ratio = c(1.15, 1.5, 2, 4), contrib = c(1, 2, 5, 10)))
  tab(data, sex, hair_color, pct = "row", color = "contrib" ) |> print() |>
    testthat::expect_output()
})


testthat::test_that("tab colors are calculated with counts and pct", {
  withr::local_options(lifecycle_verbosity = "quiet")
  tab(data, sex, hair_color, pct = "row") # must not have colors
  tab(data, sex, hair_color, pct = "row", color = "diff"    )  |> dplyr::pull(brown) |> expect_color()
  tab(data, sex, hair_color, pct = "row", color = "diff_ci" )  |> dplyr::pull(`NA`)  |> expect_color()
  tab(data, sex, hair_color, pct = "row", color = "after_ci")  |> dplyr::pull(`NA`)  |> expect_color()
  tab(data, sex, hair_color, pct = "row", color = "contrib" )  |> dplyr::pull(`NA`) |> expect_color()
  tab(data, sex, hair_color, pct = "no" , color = "contrib" )  |> dplyr::pull(`NA`) |> expect_color()
  tab(data, sex, hair_color, pct = "row", color = "OR"      )  |> dplyr::pull(brown) |> expect_color()

  tab(data, sex, hair_color, pct = "row"     , color = "auto") |> dplyr::pull(brown) |> expect_color()
  tab(data, sex, hair_color, pct = "col"     , color = "auto") |> dplyr::pull(brown) |> expect_color()
  tab(data, sex, hair_color, pct = "all"     , color = "auto") |> dplyr::pull(`NA`) |> expect_color()
  tab(data, sex, hair_color, pct = "all_tabs", color = "auto") |> dplyr::pull(`NA`) |> expect_color()

  # breakss <- get_color_breaks()
  # set_color_breaks(pct_breaks = c(0.05, 0.10, 0.20, 0.30, 2.00) )
  # set_color_breaks(pct_breaks = c(0.05, 0.10, 0.20, 2.00, 0.30) )
 })

testthat::test_that("tab colors are calculated with text supplementary columns", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # Phase 14x: sup_cols use levels = "first", so their NA column is now discarded (like the non-first
  # levels); check the displayed first-level column. (diff_ci colours nothing for this fixture -- the
  # only significant sup cell was the now-dropped NA column -- so it stays a build check.)
  tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "diff"    ) |> dplyr::pull(black_eye_color) |> expect_color()
  tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "auto"    ) |> dplyr::pull(black_eye_color) |> expect_color()
  tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "diff_ci" ) |> testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("tab colors are calculated with mean supplementary columns", {
  withr::local_options(lifecycle_verbosity = "quiet")
  tab(dplyr::storms, category, wind, color = "auto")                         |> dplyr::pull(wind) |> expect_color()
  tab(dplyr::storms, category, status, sup_cols =  wind, color = "diff"    ) |> dplyr::pull(wind) |> expect_color()
  tab(dplyr::storms, category, status, sup_cols =  wind, color = "diff_ci" ) |> dplyr::pull(wind) |> expect_color()
  tab(dplyr::storms, category, status, sup_cols =  wind, color = "after_ci") |> dplyr::pull(wind) |> expect_color()

  tab(dplyr::storms, category, status, sup_cols =  wind, color = "auto"    ) |> testthat::expect_s3_class("tabxplor_tab")
  tab(dplyr::storms, category, status, sup_cols = c("pressure", "wind")) |> testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("tab works with and without add_n and add_pct", {
  tab(data, "sex", "hair_color", pct = "row", color = "diff", add_n   = FALSE)                 |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "row", color = "diff", add_n   = FALSE, add_pct = TRUE) |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "row", color = "diff", add_pct = TRUE)                  |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "col", color = "diff", add_n   = FALSE)                 |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "col", color = "diff", add_n   = FALSE, add_pct = TRUE) |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "col", color = "diff", add_pct = TRUE)                  |> testthat::expect_s3_class("tabxplor_tab")
})


# Phase 14p: single-variable / no-col_var frequency tables keep their `n` / `pct` / `wn` columns at
# DISPLAY (a <=1.3.1-breaking regression: the add_n intent used to fold + drop the real `n` column).
testthat::test_that("single-variable frequency table keeps its n column (Phase 14p)", {
  gss <- forcats::gss_cat
  disp <- function(x) names(tabxplor:::tab_materialize_extras(x, backend = "text", pvalue = FALSE))

  # plain count: levels + n
  testthat::expect_setequal(disp(tab(gss, relig)), c("relig", "n"))
  # add_n = FALSE must NOT drop the frequency n (it is primary content, not the display extra)
  testthat::expect_setequal(disp(tab(gss, relig, add_n = FALSE)), c("relig", "n"))
  # pct modes: pct + n both survive
  testthat::expect_setequal(disp(tab(gss, relig, pct = "col")), c("relig", "pct", "n"))
  testthat::expect_setequal(disp(tab(gss, relig, pct = "row")), c("relig", "pct", "n"))
  # weighted: n + weighted wn both survive
  testthat::expect_true(all(c("n", "wn") %in%
                              disp(suppressMessages(tab(gss, relig, wt = tvhours)))))

  # a real crosstab still folds add_n into the Total cell (unchanged) -> no separate `n` column
  x <- tab(gss, relig, marital, pct = "row")
  testthat::expect_false("n" %in% disp(x))
  testthat::expect_true("Total" %in% disp(x))
})

# Phase 14p: the internal `no_col_var` sentinel must never surface as a spanning col_var name.
testthat::test_that("no_col_var placeholder is not rendered as a col_var name (Phase 14p)", {
  gss <- forcats::gss_cat
  k1  <- as.character(tab_kable(tab(gss, relig), engine = "html"))
  testthat::expect_false(grepl("no_col_var", k1))
  k2  <- as.character(tab_kable(tab(gss, relig, pct = "col"), engine = "html"))
  testthat::expect_false(grepl("no_col_var", k2))
  m1  <- paste(tab_md(tab(gss, relig, pct = "col")), collapse = "\n")
  testthat::expect_false(grepl("no_col_var", m1))
})







# #Performance profiles 2021 -------------------------------------------------------------
# # install.packages("profvis")
# library(profvis)
#
# #Decomposed :
# profvis({  #90 ms
#   data <-  tab_prepare(ct2013acm, !!row_var, !!col_var, !!!tab_vars, other_if_less_than = 30)
# })
#
# profvis({  #10 ms
#   dat_group123 <-dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var)
# })
#
# profvis({ #180 ms (essentially summarise, which calls vec_assert in new_fmt)
#   tabs <-  tab_plain(dat_group123, !!row_var, !!col_var, !!!tab_vars, wt = !!wt, is_grouped = TRUE)
# })        #100 ms with no vec_assert
#
# profvis({ #240 ms (essentially across and two summarise, with new_fmt as well)
#   tabs <-  tab_totaltab(tabs)
# })        #120 ms with no vec_assert
#
# profvis({ #440 ms (summarise at start, mutate at end, with a long vctrs::vctrs in middle !)
#   tabs <-  tab_tot(tabs)
# })        #250  ms with no vec_assert
#
# profvis({ #170 / 90 ms (a mutate with vec_ptype2 and, above all, a long vec_cast)
#   tabs <-  tab_pct(tabs)
# })        #80 ms with no vec_assert
#
# profvis({ #200 ms (two mutate with vec_ptype2 and vec_cast)
#   tabs <-  tab_ci(tabs, "diff")
# })        #110 ms with no vec_assert
#
# profvis(print(tabs)) #120 / 60 ms (70 ms with no vec_assert)
#
# #=> vec_assert for nem_fmt takes nearly half the computing time...
# # Keep them to program, remove most of them after, or is it a stupid idea ?
#
#
# #Whole :
# profvis({
#   data <-  tab_prepare(ct2013acm, !!row_var, !!col_var, !!!tab_vars,
#     other_if_less_than = 30)
#   dat_group123 <-dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var)
#   tabs <-  tab_plain(dat_group123, !!row_var, !!col_var, !!!tab_vars, wt = !!wt,
#     is_grouped = TRUE)
#   tabs <-  tab_totaltab(tabs)
#   tabs <-  tab_tot(tabs)
#   tabs <-  tab_pct(tabs)
#   tabs <-  tab_ci(tabs, "diff")
#   print(tabs)
# })



# --- Phase 14x: levels = "first" NA handling (unified across factor arity + na modes) ---------------
# A 2-level col_var used to keep its NA column visible under na = "keep" (no pre-merge fired), and a
# 3+-level col_var used to keep the NA rows IN the base under na = "drop" (the pre-merge folded NA into
# a real level, so the leaf found nothing to drop). Both are now consistent with levels = "all".

testthat::test_that("levels = 'first' discards the NA column for every factor arity (na = 'keep')", {
  d <- tibble::tibble(
    g     = rep(c("A", "B"), each = 10),
    two   = factor(c("x","x","x","x", "y","y","y","y", NA, NA,
                     "x","x","x","x","x","x", "y","y", NA, NA)),
    three = factor(c("p","p","p", "q","q","q", "r","r", NA, NA,
                     "p","p","p","p","p", "q", "r","r", NA, NA))
  )

  # 2-level: only "x" kept; "y" AND the NA column are dropped; NA still counts in the base (row total).
  t2 <- tab(d, g, two, pct = "row", levels = "first", na = "keep")
  testthat::expect_true("x" %in% names(t2))
  testthat::expect_false(any(c("y", "NA") %in% names(t2)))
  testthat::expect_equal(get_n(t2[["Total"]]), c(10, 10, 20))     # base INCLUDES the 2 NA per group
  testthat::expect_equal(get_pct(t2[["x"]])[1:2], c(0.4, 0.6))    # 4/10 , 6/10

  # 3-level: same rule -- only "p" kept, "q"/"r"/NA dropped, NA counted in the base.
  t3 <- tab(d, g, three, pct = "row", levels = "first", na = "keep")
  testthat::expect_true("p" %in% names(t3))
  testthat::expect_false(any(c("q", "r", "NA") %in% names(t3)))
  testthat::expect_equal(get_n(t3[["Total"]]), c(10, 10, 20))
})

testthat::test_that("levels = 'first' + na = 'drop' drops NA from the base for every arity", {
  d <- tibble::tibble(
    g     = rep(c("A", "B"), each = 10),
    two   = factor(c("x","x","x","x", "y","y","y","y", NA, NA,
                     "x","x","x","x","x","x", "y","y", NA, NA)),
    three = factor(c("p","p","p", "q","q","q", "r","r", NA, NA,
                     "p","p","p","p","p", "q", "r","r", NA, NA))
  )
  # base now EXCLUDES the NA rows (was the 3+-level bug: base stayed at 10).
  t2 <- tab(d, g, two,   pct = "row", levels = "first", na = "drop")
  testthat::expect_equal(get_n(t2[["Total"]]), c(8, 8, 16))
  t3 <- tab(d, g, three, pct = "row", levels = "first", na = "drop")
  testthat::expect_equal(get_n(t3[["Total"]]), c(8, 8, 16))       # 3+-level base bug fixed
})

testthat::test_that("levels = 'first' keeps NA rows in the row_var (na = 'keep')", {
  d <- tibble::tibble(
    g   = factor(c("A","A","B","B", NA, NA)),                    # a row_var with NA
    two = factor(c("x","y","x","y","x","y"))
  )
  t <- tab(d, g, two, pct = "row", levels = "first", na = "keep")
  testthat::expect_true("NA" %in% as.character(t[[1]]))          # the NA row_var group stays
})


# ---- Phase 17a janitorial fixes: failing-first fixture ----

test_that("mean-table ref matches an exact label with regex metacharacters (Defect 3, Phase 17a)", {
  # rincome's "$25000 or more" begins with `$` (a regex end-anchor), so a pure-regex reference match
  # (pre-17a diff_index_mean) fails to find the row. diff_index_mean now tries an EXACT match first.
  d  <- forcats::gss_cat |> dplyr::filter(!is.na(tvhours))
  tt <- tab_num(d, "rincome", "tvhours", ref = "$25000 or more", comp = "tab")

  ref_idx <- which(is_refrow(tt$tvhours))
  expect_length(ref_idx, 1L)                                       # exactly one reference row
  expect_identical(as.character(tt$rincome)[ref_idx], "$25000 or more")
  expect_equal(get_diff(tt$tvhours)[ref_idx], 0)                   # a row compared to itself
})
