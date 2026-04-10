# data <- forcats::gss_cat |>
# dplyr::mutate(
#   married = dplyr::case_when(
#     marital == "Married"                                                ~ factor("1-Married"),
#     marital %in% c("Never married", "Separated", "Divorced", "Widowed") ~ factor("2-Not married"),
#     TRUE                                                                ~ factor(NA_character_)
#   ),

#   white = dplyr::case_when(
#     race == "White"               ~ factor("1-White"),
#     race %in% c("Black", "Other") ~ factor("2-Non-white"),
#     TRUE                          ~ factor(NA_character_)
#   ),
  
# )




# testthat::test_that("tab_logit works", {

#   tab_logit(
#     data,
#     dependent = "married",
#     predictors = c("race", "rincome", "partyid", "relig"),
#     cleannames = TRUE,
#     subtext = "Test subtext"
#   ) #|>
#     #testthat::expect_s3_class("tabxplor_tab")

#   data |>
#     dplyr::filter(!is.na(.data$tvhours) & .data$tvhours > 0) |>
#     tab_logit(
#       dependent = c("married", "white"),
#       predictors = c("rincome", "partyid", "relig"),
#       inverse_two_level_factors = TRUE,
#       wt = "tvhours"
#     ) #|>
#     #testthat::expect_s3_class("tabxplor_tab")

#   tab_logit(
#     data,
#     dependent = "married",
#     predictors = c("race", "rincome", "partyid", "relig"),
#     full_table = TRUE
#   ) #|>
#     #testthat::expect_s3_class("tabxplor_tab")

#   data |>
#     dplyr::filter(!is.na(.data$race) & !is.na(married)) |>
#     tab_logit(
#       dependent = "married",
#       predictors = c("rincome", "partyid", "relig"),
#       split_var = "race",
#       cleannames = TRUE,
#     ) #|>
#     #testthat::expect_s3_class("tabxplor_tab")



# # Basic logistic regression
# tab_logit(
#   data,
#   dependent = "married",
#   predictors = c("race", "rincome")
# )

# # Multiple predictors with clean names
# tab_logit(
#   data,
#   dependent = "married",
#   predictors = c("race", "rincome", "partyid"),
#   cleannames = TRUE
# )

# # Stratified analysis by race
# data |>
#   dplyr::filter(!is.na(race) & !is.na(married)) |>
#   tab_logit(
#     dependent = "married",
#     predictors = c("rincome", "partyid"),
#     split_var = "race"
#   )

# # Full model diagnostics
# tab_logit(
#   data,
#   dependent = "married",
#   predictors = c("race", "rincome", "relig"),
#   full_table = TRUE,
#   subtext = "Model fit statistics included"
# )

# # Weighted regression
# data |>
#   dplyr::filter(!is.na(tvhours) & tvhours > 0) |>
#   tab_logit(
#     dependent = "married",
#     predictors = c("rincome", "partyid"),
#     wt = "tvhours"
#   )




# # Multi logistic regressions tests

# # Define predictor sequences
# predictor_models <- list(
#   "demographic" = c("race"),
#   "socioeconomic" = c("rincome", "relig"),
#   "full" = c("race", "rincome", "relig")
# )

# # Run logistic regressions on marriage status
# results <- multi_logit(
#   data,
#   dependent = "married",
#   predictors_sequence = predictor_models #,
#   #marginal_effects = TRUE
# )

# # View results
# results$married

# # Multiple outcomes with grouping
# predictor_models_split <- list(
#   "demographic" = c("age", "rincome"),
#   "full" = c("age", "rincome", "relig")
# )

# results_by_race <- multi_logit(
#   data,
#   dependent = "married",
#   predictors_sequence = predictor_models_split,
#   split_var = "race",
#   marginal_effects = FALSE
# )

  
  
# data <- forcats::gss_cat |>
# dplyr::mutate(
#   married = dplyr::case_when(
#     marital == "Married"                                                ~ factor("1-Married"),
#     marital %in% c("Never married", "Separated", "Divorced", "Widowed") ~ factor("2-Not married"),
#     TRUE                                                                ~ factor(NA_character_)
#   ),
#   white = dplyr::case_when(
#     race == "White"               ~ factor("1-White"),
#     race %in% c("Black", "Other") ~ factor("2-Non-white"),
#     TRUE                          ~ factor(NA_character_)
#   ),
# )
# dependent <-  "married"
# predictors_sequence <- predictor_models 
# split_var = NULL
# wt = NULL
# nb_questions = NULL
# odds_ratios = TRUE
# marginal_effects = FALSE
# signif = TRUE
# add_pct = FALSE
# add_n = FALSE
# empirical_odds_ratio = TRUE
# inverse_two_level_factors = TRUE
# cleannames = TRUE
# ci = FALSE
# subtext = ""







#   # data, dependent, predictors, split_var = NULL, wt = NULL,
#   # full_table = FALSE,
#   # inverse_two_level_factors = TRUE,
#   # cleannames = NULL,
#   # subtext = ""

#   # tab_plain(data, col_var = hair_color) |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, sex, hair_color, wt = mass) |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, sex, hair_color, gender, wt = mass) |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, row_var = hair_color, col_var = NULL, gender, wt = mass) |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, row_var = NULL, col_var = hair_color, gender, wt = mass) |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, sex, hair_color, gender, wt = mass, na = "drop") |>
#   #   testthat::expect_s3_class("tabxplor_tab")

#   # tab_plain(data, sex, sex) |> testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, sex, sex, gender, na = "drop") |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, sex, sex, gender, wt = mass) |>
#   #   testthat::expect_s3_class("tabxplor_tab")

#   # tab_plain(data, "gender", "sex", NA_character_) |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, "gender", NA_character_) |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, NA_character_, "sex") |>
#   #   testthat::expect_s3_class("tabxplor_tab")
#   # tab_plain(data, "gender", "sex", NULL) |>
#   #   testthat::expect_s3_class("tabxplor_tab")
# })


# # testthat::test_that("tab_plain works with num and df", {
# #   tab_plain(data, sex, hair_color, num = TRUE)                    |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, df = TRUE)                     |> testthat::expect_s3_class("data.frame")

# #   tab_plain(data, sex, hair_color, gender, wt = mass, num = TRUE) |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, wt = mass, df = TRUE)  |> testthat::expect_s3_class("data.frame")

# # })

# # testthat::test_that("tab_plain works with totals and total table", {
# #   tab_plain(data, sex, hair_color, tot = c("row", "col"))         |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, tot = c("row", "col")) |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, totaltab = "line")     |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, totaltab = "table")    |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, totaltab = "no")       |> testthat::expect_s3_class("tabxplor_tab")
# # })

# # testthat::test_that("tab_plain works with pct and diffs", {
# #   tab_plain(data, sex, hair_color, pct = "row")                       |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, pct = "col")                       |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, pct = "all")                       |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, pct = "all_tabs")                  |> testthat::expect_s3_class("tabxplor_tab")

# #   tab_plain(data, sex, hair_color, gender, pct = "row")               |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, pct = "col")               |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, pct = "all")               |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, pct = "all_tabs")          |> testthat::expect_s3_class("tabxplor_tab")

# #   tab_plain(data, sex, hair_color, pct = "row", ref = "^male")        |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, pct = "row", ref = 2)     |> testthat::expect_s3_class("tabxplor_tab")

# #   tab_plain(data, sex, hair_color, gender, pct = "row", comp = "all") |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, pct = "row", ref = "tot",
# #             comp = "all")                                             |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, pct = "row", ref = 3,
# #             comp = "all", totaltab = "table")                         |> testthat::expect_s3_class("tabxplor_tab")

# #   tab_plain(data, sex, hair_color, pct = "col", ref = "brown")       |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, pct = "col", ref = 3)             |> testthat::expect_s3_class("tabxplor_tab")

# #   #warnings
# #   tab_plain(data, sex, hair_color, pct = "row", ref = 47)                |> testthat::expect_warning()
# #   tab_plain(data, sex, hair_color, pct = "row", ref = "no_existing_cat") |> testthat::expect_warning()
# #   tab_plain(data, sex, hair_color, pct = "col", ref = 47)                |> testthat::expect_warning()
# #   tab_plain(data, sex, hair_color, pct = "col", ref = "no_existing_cat") |> testthat::expect_warning()
# #   #tab_plain(data, sex, hair_color, pct = "col", comp = "all")             |> testthat::expect_warning()
# #   #tab_plain(data, sex, hair_color, gender, pct = "col", ref = "black", comp = "all") |> testthat::expect_warning()
# #   #tab_plain(data, sex, hair_color, gender, pct = "col", comp = "all")     |> testthat::expect_warning()
# # })

# # testthat::test_that("tab_plain works with OR", {
# #   tab_plain(data, sex, hair_color, pct = "row", OR = "OR")            |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, pct = "col", OR = "OR_pct")        |> testthat::expect_s3_class("tabxplor_tab")

# #   tab_plain(data, sex, hair_color, pct = "row", OR = "OR", ref = "^male")       |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, pct = "row", OR = "OR", ref = 2)     |> testthat::expect_s3_class("tabxplor_tab")

# #   tab_plain(data, sex, hair_color, gender, pct = "row", OR = "OR", ref = "tot",
# #             comp = "all")                                             |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_plain(data, sex, hair_color, gender, pct = "row", OR = "OR", ref = 3,
# #             comp = "all", totaltab = "table")                         |> testthat::expect_s3_class("tabxplor_tab")
# # })

# # testthat::test_that("tab_num works with missing, NULL, NA, etc.", {
# #   # set_color_breaks(mean_breaks = c(1.05, 1.10, 1.20, 1.50))
# #   tab_num(data, sex, height, na = "drop")                                       %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, height, wt = mass)                                         %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, height, ref = "no", ci = "no", tot = "row")                %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year))                                     %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), gender, tot = "row",totaltab = "table") %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), c(gender, eye_color), comp = "all") %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, col_var = birth_year)                                           %>% testthat::expect_s3_class("tabxplor_tab")
# #   })


# # testthat::test_that("tab_num works with diff and ci", {
# #   tab_num(data, sex, c(height, birth_year), na = "drop", ref = "no")        %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), na = "drop")

# #   tab_num(data, sex, c(height, birth_year), na = "drop", color = "diff")     %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), na = "drop", color = "diff_ci")  %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), na = "drop", color = "after_ci") %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), na = "drop", color = "")         %>% testthat::expect_s3_class("tabxplor_tab")

# #   tab_num(data, sex, c(height, birth_year), na = "drop", ref = "^male")     %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), na = "drop", ref = 3,
# #           color = "diff_ci", tot = "row")                                    %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), na = "drop", color = "after_ci") %>% testthat::expect_s3_class("tabxplor_tab")

# #   tab_num(data, sex, c(height, birth_year), na = "drop", ci = "cell")        %>% testthat::expect_s3_class("tabxplor_tab")

# #   tab_num(data, sex, c(height, birth_year), na = "drop", ci = "diff")        %>% testthat::expect_s3_class("tabxplor_tab")

# # })

# # testthat::test_that("tab_num works with with df and num", {
# #   tab_num(data, sex, c(height, birth_year), na = "drop",
# #           tot = "row", totaltab = "table", num = TRUE)         %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_num(data, sex, c(height, birth_year), gender, na = "drop",
# #           tot = "row", totaltab = "table", num = TRUE)         %>% testthat::expect_s3_class("tabxplor_tab")

# #   tab_num(data, sex, c(height, birth_year), na = "drop",
# #           tot = "row", totaltab = "table", df = TRUE)          %>% testthat::expect_s3_class("data.frame")
# #   tab_num(data, sex, c(height, birth_year), gender, na = "drop",
# #           tot = "row", totaltab = "table",df = TRUE) %>% testthat::expect_s3_class("data.frame")
# # })


# # testthat::test_that("tab et tab_many works with missing, NULL, NA, etc., in variables", {
# #   tab(data, "gender", "sex", NA_character_) %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab(data, "gender", NA_character_)        %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab(data, NA_character_, "sex")           %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab(data, "gender", "sex", NULL)          %>% testthat::expect_s3_class("tabxplor_tab")
# #   #tab(data, "gender", "sex", "")           %>% testthat::expect_s3_class("tabxplor_tab")
# #   #tab(data, "gender", "sex", "no")         %>% testthat::expect_s3_class("tabxplor_tab")

# #   tab_many(data, "gender")                                                     %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "gender", wt = mass)                                          %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "gender", col_vars = NULL         , tab_vars = NULL)          %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "gender", col_vars = NA_character_, tab_vars = NA_character_) %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "gender", col_vars = ""           , tab_vars = "")            %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "gender", col_vars = "no"         , tab_vars = "no")          %>% testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, gender, col_vars = hair_color , tab_vars = sex)               %>% testthat::expect_s3_class("tabxplor_tab")
# # })


# # testthat::test_that("tab_many works with numeric variables", {
# #   tab_many(data, sex, mass)         |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, sex, mass, gender) |> testthat::expect_s3_class("tabxplor_tab")
# # })

# # testthat::test_that("vectorisation of pct in tab_many works", {
# #   tab_many(data, sex, c(hair_color, eye_color), pct = "row")
# #   tab_many(data, sex, c(hair_color, mass, gender), pct = "row")                 |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, sex, c(hair_color, mass, gender), pct = c("row", NA, "col"))   |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, c(sex, gender), hair_color, pct = c("row", "col")) |> length() |> testthat::expect_equal(2)
# #   tab_many(data, c(sex, eye_color), c(hair_color, mass, gender),
# #            pct = list(sex = list("row", "col", "col"), eye_color = list("col", "row", "row"))
# #   ) |>
# #     length() |> testthat::expect_equal(2)
# # })

# # testthat::test_that("tab_many works with levels = 'first'", {
# #   tabs1 <- tab_many(data, sex, c(hair_color, eye_color), pct = "row", levels = "first")
# #   testthat::expect_false("brown_hair_color" %in% names(tabs1))

# #   tabs2 <- tab_many(data, sex, c(hair_color, eye_color), pct = "row", levels = c("first", "all"))
# #   testthat::expect_false("brown_hair_color" %in% names(tabs2))
# #   testthat::expect_true("orange" %in% names(tabs2))
# # })

# # testthat::test_that("tab_many na arguments work the right way", {
# #   tabs1 <- tab_many(data, gender, hair_color, sex, na = "drop_all")
# #   testthat::expect_true(all(!stringr::str_detect(dplyr::pull(tabs1,  sex), "^NA")))

# #   tabs2 <- tab_many(data, gender, hair_color, sex, na_drop_all = gender)
# #   testthat::expect_true(all(!stringr::str_detect(dplyr::pull(tabs2,  sex), "^NA")))
# #   testthat::expect_true(any(stringr::str_detect(names(tabs2), "^NA")))
# # })


# # testthat::test_that("all tab functions works with no tab_vars", {
# #   data %>% #with no tab_vars
# #     tab_plain(sex, hair_color, wt = mass, pct = "row") %>%
# #     #tab_totaltab() %>%
# #     #tab_tot() %>%
# #     #tab_pct() %>%
# #     tab_ci("diff", color = "after_ci") %>%
# #     tab_chi2() %>%
# #     testthat::expect_s3_class("tabxplor_tab")
# # })

# # testthat::test_that("all tab functions works with no col_var", {
# #   data %>%
# #     tab_plain(sex, pct = "col") %>%
# #     #tab_totaltab() %>%
# #     #tab_tot() %>%
# #     #tab_pct("col") %>%
# #     tab_ci("diff", color = "after_ci") %>%
# #     tab_chi2() %>%
# #     testthat::expect_s3_class("tabxplor_tab")
# # })

# # testthat::test_that("all tab functions works with no row_var", {
# #   data %>%
# #     tab_plain(col_var = hair_color, tot = c("row", "col"), pct = "row") %>%
# #     #tab_totaltab() %>% error
# #     #tab_tot() %>%
# #     #tab_pct() %>%
# #     tab_ci() %>%
# #     tab_chi2() %>%
# #     testthat::expect_s3_class("tabxplor_tab")
# # })

# # testthat::test_that("all tab functions works with totaltab = 'line'", {
# #   data %>%
# #     tab_plain(sex, hair_color, gender, pct = "row") %>%
# #     #tab_totaltab("line") %>%
# #     #tab_tot() %>%
# #     #tab_pct() %>%
# #     tab_ci("diff", color = "after_ci") %>%
# #     tab_chi2() %>%
# #     testthat::expect_s3_class("tabxplor_tab")
# # })

# # testthat::test_that("tab_num works (with color)", {
# #   testthat::expect_true(
# #     !is.na(tab_prepare(data, sex, mass) %>%
# #              tab_num(sex, mass, tot = "row", ref = "tot", color = "after_ci") %>%

# #              tab_chi2() %>%
# #              dplyr::pull(mass) %>% vec_data() %>% dplyr::pull(var) %>% dplyr::last())
# #   )
# # })

# # testthat::test_that("tab_many work with tribble", {

# #   tibble::tribble(
# #     ~row_var, ~col_vars                           , ~tab_vars     , ~levels,
# #     "sex"   , "hair_color"                        , NA_character_ , "all"  ,
# #     "sex"   , c("mass", "hair_color", "eye_color"), "gender"      , "first",
# #     "sex"   , c("hair_color", "eye_color", "mass"), "gender"      , "all"  ,
# #   ) %>%
# #     purrr::pmap(tab_many, data = data, totcol = "no", totaltab = "no") %>%
# #     testthat::expect_type("list")

# #   # not needed, since the opportunity of proceeding that way is not clear ?
# #   # purrr::map(tabs, ~ tab_totaltab(.) %>%
# #   #              tab_tot() %>%
# #   #              tab_pct() %>%
# #   #              tab_ci() %>%
# #   #              tab_chi2()
# #   # )
# # })

# # testthat::test_that("tab work with tribble (even many tab_vars)", {
# #   tibble::tribble(
# #     ~row_var, ~col_var    , ~tab_vars                 ,
# #     "sex"   , "hair_color", NA_character_             ,
# #     "sex"   , "mass"      , "gender"                  ,
# #     "sex"   , "eye_color" , c("gender",  "hair_color"),
# #   ) %>%
# #     purrr::pmap(tab, data = data) %>%
# #     testthat::expect_type("list")
# # })



# # #' @keywords internal
# # expect_color <- function(object) {
# #   # 1. Capture object and label
# #   act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

# #   # 2. Call expect()
# #   act$color <- fmt_color_selection(act$val) %>% purrr::flatten_lgl()
# #   testthat::expect(
# #     any(act$color),
# #     sprintf("%s doesn't return any colored cell.", act$lab)
# #   )

# #   # 3. Invisibly return the value
# #   invisible(act$val)
# # }

# # testthat::test_that("printing colors works", {
# #   set_color_style(type = "bg", theme = "dark")
# #   tab(data, sex, hair_color, pct = "row", color = "diff"    ) %>% print() %>%
# #     testthat::expect_output()
# #   set_color_style(type = "text", theme = "dark")
# #   set_color_breaks(pct_breaks = c(0.05, 0.15, 0.3),
# #                    mean_breaks = c(1.15,  2, 4),
# #                    contrib_breaks = c(1, 2, 5)     )
# #   tab(data, sex, hair_color, pct = "row", color = "diff_ci" ) %>% print() %>%
# #     testthat::expect_output()
# #   set_color_style(type = "bg", theme = "light")
# #   tab(data, sex, hair_color, pct = "row", color = "after_ci") %>% print() %>%
# #     testthat::expect_output()

# #   set_color_style(type = "text")
# #   set_color_breaks(pct_breaks = c(0.05, 0.1, 0.2, 0.3),
# #                    mean_breaks = c(1.15, 1.5, 2, 4),
# #                    contrib_breaks = c(1, 2, 5, 10)     )
# #   tab(data, sex, hair_color, pct = "row", color = "contrib" ) %>% print() %>%
# #     testthat::expect_output()
# #   set_color_style(type = "text", theme = "dark")
# # })


# # testthat::test_that("tab colors are calculated with counts and pct", {
# #   tab(data, sex, hair_color, pct = "row") # must not have colors
# #   tab(data, sex, hair_color, pct = "row", color = "diff"    )  %>% dplyr::pull(brown) %>% expect_color()
# #   tab(data, sex, hair_color, pct = "row", color = "diff_ci" )  %>% dplyr::pull(`NA`)  %>% expect_color()
# #   tab(data, sex, hair_color, pct = "row", color = "after_ci")  %>% dplyr::pull(`NA`)  %>% expect_color()
# #   tab(data, sex, hair_color, pct = "row", color = "contrib" )  %>% dplyr::pull(`NA`) %>% expect_color()
# #   tab(data, sex, hair_color, pct = "no" , color = "contrib" )  %>% dplyr::pull(`NA`) %>% expect_color()
# #   tab(data, sex, hair_color, pct = "row", color = "OR"      )  %>% dplyr::pull(brown) %>% expect_color()

# #   tab(data, sex, hair_color, pct = "row"     , color = "auto") %>% dplyr::pull(brown) %>% expect_color()
# #   tab(data, sex, hair_color, pct = "col"     , color = "auto") %>% dplyr::pull(brown) %>% expect_color()
# #   tab(data, sex, hair_color, pct = "all"     , color = "auto") %>% dplyr::pull(`NA`) %>% expect_color()
# #   tab(data, sex, hair_color, pct = "all_tabs", color = "auto") %>% dplyr::pull(`NA`) %>% expect_color()

# #   # breakss <- get_color_breaks()
# #   # set_color_breaks(pct_breaks = c(0.05, 0.10, 0.20, 0.30, 2.00) )
# #   # set_color_breaks(pct_breaks = c(0.05, 0.10, 0.20, 2.00, 0.30) )
# #  })

# # testthat::test_that("tab colors are calculated with text supplementary columns", {
# #   tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "diff"    ) %>% dplyr::pull(black_eye_color) %>% expect_color()
# #   tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "diff_ci" ) %>% dplyr::pull(`NA`) %>% expect_color()
# #   tab(data, sex, hair_color, pct = "row", sup_cols = eye_color, color = "auto"    ) %>% dplyr::pull(black_eye_color) %>% expect_color()
# # })

# # testthat::test_that("tab colors are calculated with mean supplementary columns", {
# #   tab(dplyr::storms, category, wind, color = "auto")                         %>% dplyr::pull(wind) %>% expect_color()
# #   tab(dplyr::storms, category, status, sup_cols =  wind, color = "diff"    ) %>% dplyr::pull(wind) %>% expect_color()
# #   tab(dplyr::storms, category, status, sup_cols =  wind, color = "diff_ci" ) %>% dplyr::pull(wind) %>% expect_color()
# #   tab(dplyr::storms, category, status, sup_cols =  wind, color = "after_ci") %>% dplyr::pull(wind) %>% expect_color()

# #   tab(dplyr::storms, category, status, sup_cols =  wind, color = "auto"    ) |> testthat::expect_s3_class("tabxplor_tab")
# #   tab(dplyr::storms, category, status, sup_cols = c("pressure", "wind")) |> testthat::expect_s3_class("tabxplor_tab")
# # })

# # testthat::test_that("tab_many works with and without add_n and add_pct", {
# #   tab_many(data, "sex", "hair_color", pct = "row", color = "diff", add_n   = FALSE)                 |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "sex", "hair_color", pct = "row", color = "diff", add_n   = FALSE, add_pct = TRUE) |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "sex", "hair_color", pct = "row", color = "diff", add_pct = TRUE)                  |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "sex", "hair_color", pct = "col", color = "diff", add_n   = FALSE)                 |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "sex", "hair_color", pct = "col", color = "diff", add_n   = FALSE, add_pct = TRUE) |> testthat::expect_s3_class("tabxplor_tab")
# #   tab_many(data, "sex", "hair_color", pct = "col", color = "diff", add_pct = TRUE)                  |> testthat::expect_s3_class("tabxplor_tab")
# # })



