
data <- dplyr::starwars %>%
  tab_prepare("sex", c("hair_color", "eye_color", "mass"), "gender",
              rare_to_other = TRUE, n_min = 5, na = "drop")

# starwars %>% dplyr::select(where(is.character)) %>% purrr::map(~ as.factor(.) %>%
#   levels())

# dplyr::storms
# forcats::gss_cat

testthat::test_that("tab works with missing, NULL, NA, etc., in variables", {
  tab(data, "gender", "sex", NA_character_) %>% testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", NA_character_)        %>% testthat::expect_s3_class("tabxplor_tab")
  tab(data, NA_character_, "sex")           %>% testthat::expect_s3_class("tabxplor_tab")
  tab(data, "gender", "sex", NULL)          %>% testthat::expect_s3_class("tabxplor_tab")
  #tab(data, "gender", "sex", "")           %>% testthat::expect_s3_class("tabxplor_tab")
  #tab(data, "gender", "sex", "no")         %>% testthat::expect_s3_class("tabxplor_tab")

  tab_many(data, "gender", col_vars = NULL         , tab_vars = NULL)          %>%
    testthat::expect_s3_class("tabxplor_tab")
  tab_many(data, "gender", col_vars = NA_character_, tab_vars = NA_character_) %>%
    testthat::expect_s3_class("tabxplor_tab")
  tab_many(data, "gender", col_vars = ""           , tab_vars = "")            %>%
    testthat::expect_s3_class("tabxplor_tab")
  tab_many(data, "gender", col_vars = "no"         , tab_vars = "no")          %>%
    testthat::expect_s3_class("tabxplor_tab")
  tab_many(data, gender, col_vars = hair_color , tab_vars = sex)           %>%
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("all tab functions works with no tab_vars", {
  data %>% #with no tab_vars
    tab_core(sex, hair_color, wt = mass) %>%
    tab_totaltab() %>%
    tab_tot() %>%
    tab_pct() %>%
    tab_ci("diff", color = "after_ci") %>%
    tab_chi2() %>%
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("all tab functions works with no col_var", {
  data %>%
    tab_core(sex) %>%
    tab_totaltab() %>%
    tab_tot() %>%
    tab_pct("col") %>%
    tab_ci("diff", color = "after_ci") %>%
    tab_chi2() %>%
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("all tab functions works with no row_var", {
  data %>%
    tab_core(col_var = hair_color) %>%
    tab_totaltab() %>%
    tab_tot() %>%
    tab_pct() %>%
    tab_ci() %>%
    tab_chi2() %>%
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("all tab functions works with totaltab = 'line'", {
  data %>%
    tab_core(sex, hair_color, gender) %>%
    tab_totaltab("line") %>%
    tab_tot() %>%
    tab_pct() %>%
    tab_ci("diff", color = "after_ci") %>%
    tab_chi2() %>%
    testthat::expect_s3_class("tabxplor_tab")
})

testthat::test_that("all tab functions work with only one mean column (with color)", {
  testthat::expect_true(
    !is.na(tab_prepare(data, sex, mass) %>%
             tab_core(sex, mass) %>%
             tab_tot("row", data = data) %>%
             tab_pct(color = TRUE)%>%
             tab_ci("diff", color = "after_ci") %>%
             tab_chi2()%>%
             dplyr::pull(mass)%>% vec_data()%>% dplyr::pull(var) %>% dplyr::last())
  )
})

testthat::test_that("tab_many work with tribble", {

  tibble::tribble(
    ~row_var, ~col_vars                           , ~tab_vars     , ~levels,
    "sex"   , "hair_color"                        , NA_character_ , "all"  ,
    "sex"   , c("mass", "hair_color", "eye_color"), "gender"      , "first",
    "sex"   , c("hair_color", "eye_color", "mass"), "gender"      , "all"  ,
  ) %>%
    purrr::pmap(tab_many, data = data, totrow = FALSE, totcol = "no", totaltab = "no") %>%
    testthat::expect_type("list")

  # not needed, since the opportunity of proceeding that way is not clear ?
  # purrr::map(tabs, ~ tab_totaltab(.) %>%
  #              tab_tot() %>%
  #              tab_pct() %>%
  #              tab_ci() %>%
  #              tab_chi2()
  # )
})

testthat::test_that("tab work with tribble", {
  tibble::tribble(
    ~row_var, ~col_var    , ~tab_vars                 ,
    "sex"   , "hair_color", NA_character_             ,
    "sex"   , "mass"      , "gender"                  ,
    "sex"   , "eye_color" , c("gender",  "hair_color"),
  ) %>%
    purrr::pmap(tab, data = data) %>%
    testthat::expect_type("list")
})

tab(data = data, sex, mass, gender)


tabs <- tab_many(data, "sex", c("hair_color", "eye_color", "mass"), "gender",
                 totaltab = "no", totrow = FALSE, totcol = "no")

testthat::test_that("tab_totaltab works with all arguments (and with tab_tot)", {
  testthat::expect_true(
    nrow(tabs %>% tab_totaltab("line") %>% tab_totaltab("no") %>% tab_totaltab("table")%>%
           dplyr::filter_at(1, ~ stringr::str_detect(., "^Ensemble")) ) != 0,
  )

  testthat::expect_identical(
    nrow(tabs %>%
           tab_totaltab() %>% tab_tot() %>%
           dplyr::filter_at(1, ~ stringr::str_detect(., "^Ensemble")) ),

    nrow(tabs %>% tab_totaltab(name = "Overall", data = data) %>%
           dplyr::filter_at(1, ~ stringr::str_detect(., "^Overall"))  ) + 1L
  )

  testthat::expect_identical(
    nrow(tabs %>% tab_totaltab("line") %>% tab_tot() %>%
           dplyr::filter_at(1, ~ stringr::str_detect(., "^Ensemble")) ),
    1L
  )
})


tabs <- tabs %>% tab_totaltab()

testthat::test_that("tab_tot works with all arguments", {
  tabs %>% tab_tot("col") %>% tab_tot("row") %>% tab_tot("no") %>%
    testthat::expect_s3_class("tabxplor_tab")

  tabs %>% tab_tot(totcol = "each") %>% testthat::expect_s3_class("tabxplor_tab")
})
#tab_tot("row") can't be done on different groups of rows independently
# tabs[is_tottab(tabs),] <- tabs[is_tottab(tabs),] %>% tab_tot("row")

testthat::test_that("tab_pct works with groups, ungroup, and warnings", {

  tabs %>% tab_tot("col") %>% dplyr::ungroup() %>% tab_pct("row") %>%
    testthat::expect_warning("no groups nor total row")

  tabs %>% tab_tot("row")  %>% tab_pct("col") %>%
    testthat::expect_warning("no total column")

  testthat::expect_false( # return col_all
    tabs %>% tab_tot() %>% dplyr::ungroup() %>% tab_pct("col") %>% dplyr::ungroup() %>%
      dplyr::select(where(~is_fmt(.) & ! get_type(.) == "mean")) %>%
      dplyr::filter(is_totrow(.) & ! is_tottab(.)) %>%
      dplyr::mutate(dplyr::across(.fns = ~ get_pct(.) == 1)) %>%
      dplyr::summarise(dplyr::across(.fns = all)) %>%
      purrr::map_lgl(~ . ) %>% all()
  )

  testthat::expect_equal(
    tabs %>% tab_tot() %>% dplyr::ungroup() %>% tab_pct("all") %>%
      dplyr::mutate(dplyr::across(where(is_fmt), get_pct)) %>%
      `attr<-`("groups", NULL),

    tabs %>% tab_tot() %>% dplyr::ungroup() %>% tab_pct("all_tabs") %>%
      dplyr::mutate(dplyr::across(where(is_fmt), get_pct)) %>%
      `attr<-`("groups", NULL)
  )

})

testthat::test_that("tab_pct works with tot = 'each'", {
  tabs2 <- tabs %>% tab_tot(totcol = "each")
  tabs2 %>% tab_pct("row")      %>% testthat::expect_s3_class("tabxplor_tab")
  tabs2 %>% tab_pct("col")      %>% testthat::expect_s3_class("tabxplor_tab")
  tabs2 %>% tab_pct("all")      %>% testthat::expect_s3_class("tabxplor_tab")
  tabs2 %>% tab_pct("all_tabs") %>% testthat::expect_s3_class("tabxplor_tab")
})


tabs <- tabs %>% tab_tot() %>%
  dplyr::mutate(dplyr::across(where(is_fmt), ~ set_comp(., NA)))

testthat::test_that("tab_ci works (with tab_pct)", {
  tabs %>% tab_pct("row") %>% tab_ci("diff", comp = "all") %>%
    testthat::expect_warning("comp were set to 'tab'")

  tabs %>% tab_pct("row", comp = "all") %>% tab_ci("diff", color = "after_ci") %>%
    testthat::expect_s3_class("tabxplor_tab")

  tabs %>% tab_pct("col") %>% tab_ci(color = "diff_ci") %>% testthat::expect_s3_class("tabxplor_tab")

  testthat::expect_true(
  tabs %>% tab_pct("row") %>% tab_ci("cell", visible = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(
      where(is_fmt), ~ stringr::str_detect(format(.),
                                           stringi::stri_unescape_unicode("\\u00b1")))) %>%
    dplyr::summarise(dplyr::across(where(is.logical), any)) %>%
    purrr::map_lgl(~ .) %>% any()
  )

  # tabs %>% tab_pct("all")               %>% tab_ci("cell", visible = TRUE)  %>%
  #   testthat::expect_s3_class("tabxplor_tab")

  tabs %>% tab_pct("all_tabs") %>% tab_ci("cell", color = "after_ci") %>%
    testthat::expect_s3_class("tabxplor_tab")
})
# Can we sum variances for means ? Answer : no, weighted mean is an approximation
# tabs1 <- tab_core(data, PE0, REVMENSC, PR0, EMP_ADM_ENT) %>% tab_ci() %>%
#   dplyr::mutate(sd = get_sd(REVMENSC), wn = get_wn(REVMENSC), n = get_n(REVMENSC))
# tabs2 <- tab_core(data, EMP_ADM_ENT, REVMENSC, PR0) %>% tab_ci() %>%
#   dplyr::mutate(sd = get_sd(REVMENSC), wn = get_wn(REVMENSC), n = get_n(REVMENSC))
#
# tabs2
# tabs1 %>% dplyr::summarise(REVMENSC = mean(REVMENSC),
#                            sd = sqrt(sum(sd ^ 2 * wn)/sum(wn)),
#                            wn = sum(wn), n = sum(n) )


# tabs <- tabs %>% tab_pct("row") %>% tab_ci("diff", color = "after_ci") %>% tab_chi2()
#
# testthat::test_that("tab_chi2 table is the expected one", {
#
#   tabs %>% get_chi2() %>%
#     dplyr::select(where(is_fmt)) %>%
#     dplyr::mutate(dplyr::across(.fns = get_num)) %>%
#     purrr::map(~ .) %>%
#     testthat::expect_snapshot_value()
#
# })

# testthat::test_that("tab_chi2 contributions to variance work", {
# ctr <- tabs %>% dplyr::ungroup() %>%
#     dplyr::transmute(dplyr::across(where(is_fmt), ~ set_display(., "ctr")))
#
# ctr %>% dplyr::filter(is_totrow(.)) %>%
#   dplyr::mutate(dplyr::across(.fns = ~ get_ctr(.)))
#
# ctr %>% dplyr::filter(!is_totrow(.)) %>%
#   dplyr::mutate(dplyr::across(.fns = ~ get_ctr(.)))
# })

#' @keywords internal
expect_color <- function(object) {
  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  act$color <- fmt_color_selection(act$val) %>% purrr::flatten_lgl()
  testthat::expect(
    any(act$color),
    sprintf("%s doesn't return any colored cell.", act$lab)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

testthat::test_that("printing colors works", {
  set_color_style(type = "bg", theme = "dark")
  tab(data, sex, hair_color, pct = "row", color = "diff"    ) %>% print() %>%
    testthat::expect_output()
  set_color_style(type = "text", theme = "dark")
  set_color_breaks(pct_breaks = c(0.05, 0.15, 0.3),
                   mean_breaks = c(1.15,  2, 4),
                   contrib_breaks = c(1, 2, 5)     )
  tab(data, sex, hair_color, pct = "row", color = "diff_ci" ) %>% print() %>%
    testthat::expect_output()
  set_color_style(type = "bg", theme = "light")
  tab(data, sex, hair_color, pct = "row", color = "after_ci") %>% print() %>%
    testthat::expect_output()

  set_color_style(type = "text")
  set_color_breaks(pct_breaks = c(0.05, 0.1, 0.2, 0.3),
                   mean_breaks = c(1.15, 1.5, 2, 4),
                   contrib_breaks = c(1, 2, 5, 10)     )
  tab(data, sex, hair_color, pct = "row", color = "contrib" ) %>% print() %>%
    testthat::expect_output()
})


testthat::test_that("tab colors are calculated with counts and pct", {

tab(data, sex, hair_color, pct = "row", color = "diff"    )  %>% dplyr::pull(brown) %>%
  expect_color()
tab(data, sex, hair_color, pct = "row", color = "diff_ci" )  %>% dplyr::pull(brown) %>%
  expect_color()
tab(data, sex, hair_color, pct = "row", color = "after_ci")  %>% dplyr::pull(brown) %>%
  expect_color()
tab(data, sex, hair_color, pct = "row", color = "contrib" )  %>% dplyr::pull(brown) %>%
  expect_color()
tab(data, sex, hair_color, pct = "no" , color = "contrib" )  %>% dplyr::pull(brown) %>%
  expect_color()

tab(data, sex, hair_color, pct = "row"     , color = "auto") %>% dplyr::pull(brown) %>%
  expect_color()
tab(data, sex, hair_color, pct = "col"     , color = "auto") %>% dplyr::pull(brown) %>%
  expect_color()
tab(data, sex, hair_color, pct = "all"     , color = "auto") %>% dplyr::pull(brown) %>%
  expect_color()
tab(data, sex, hair_color, pct = "all_tabs", color = "auto") %>% dplyr::pull(brown) %>%
  expect_color()

tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "diff"    ) %>%
  dplyr::pull(black_eye_color) %>% expect_color()
tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "diff_ci" ) %>%
  dplyr::pull(black_eye_color) %>% expect_color()
tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "auto"    ) %>%
  dplyr::pull(black_eye_color) %>% expect_color()
})

testthat::test_that("tab colors are calculated with mean columns", {
  tab(data, sex, mass, color = "auto") %>% dplyr::pull(mass) %>% expect_color()

  tab(data, sex, hair_color, pct = "row", sup_cols = mass, color = "diff"    ) %>%
    dplyr::pull(mass) %>% expect_color()
  tab(data, sex, hair_color, pct = "row", sup_cols = mass, color = "diff_ci" ) %>%
    dplyr::pull(mass) %>% expect_color()
  tab(data, sex, hair_color, pct = "row", sup_cols = mass, color = "after_ci") %>%
    dplyr::pull(mass) %>% expect_color()
  tab(data, sex, hair_color, pct = "row", sup_cols = mass, color = "auto"    ) %>%
    dplyr::pull(mass) %>% expect_color()
})



# #Performance profiles-------------------------------------------------------------------
# # install.packages("profvis")
# library(profvis)
#
# #Decomposed :
# profvis({  #90 ms
#   data <-  tab_prepare(ct2013acm, !!row_var, !!col_var, !!!tab_vars, rare_to_other = TRUE)
# })
#
# profvis({  #10 ms
#   dat_group123 <-dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var)
# })
#
# profvis({ #180 ms (essentially summarise, which calls vec_assert in new_fmt)
#   tabs <-  tab_core(dat_group123, !!row_var, !!col_var, !!!tab_vars, wt = !!wt, is_grouped = TRUE)
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
#     rare_to_other = TRUE)
#   dat_group123 <-dplyr::group_by(data, !!!tab_vars, !!row_var, !!col_var)
#   tabs <-  tab_core(dat_group123, !!row_var, !!col_var, !!!tab_vars, wt = !!wt,
#     is_grouped = TRUE)
#   tabs <-  tab_totaltab(tabs)
#   tabs <-  tab_tot(tabs)
#   tabs <-  tab_pct(tabs)
#   tabs <-  tab_ci(tabs, "diff")
#   print(tabs)
# })

