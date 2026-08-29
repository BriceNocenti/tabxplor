# PURPOSE: tab(), tab_plain() and tab_num() end to end -- every argument, every arity, the labelled path.
# ROLE: the shipped CONTRACT for R/tab.R, R/tab-leaf.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: tab(), tab_plain() and tab_num() end to end =========================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())




data <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)



# starwars |> dplyr::select(where(is.character)) |> purrr::map(~ as.factor(.) |>
#   levels())

# dplyr::storms
# fx_gss()

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

  tab_num(data, sex, c(height, birth_year), na = "drop", ci = "ref")        |> testthat::expect_s3_class("tabxplor_tab")

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



# Phase 19h (KEY 7): `pct` is per COL_VAR on tab() too now -- it was the odd one out among the
# col_var-vectorised arguments (`levels`, `digits`), size-1-asserted although the engine has always
# recycled it. The per-ROW_VAR list form stays refused: Phase 6 globalised the row axis on purpose.
# suppressWarnings() keeps tab_many()'s deprecation nudge out (see the dedicated test below).
testthat::test_that("pct is vectorised over col_vars, on tab() and its tab_many() shim", {
  suppressWarnings({
    tab(data, sex, c(hair_color, mass, gender), pct = c("row", NA, "col")) |>
      testthat::expect_s3_class("tabxplor_tab")
    tab_many(data, sex, c(hair_color, mass, gender), pct = c("row", NA, "col")) |>
      testthat::expect_s3_class("tabxplor_tab")
    # the shim is lossless: same table either way
    testthat::expect_equal(
      tab_many(data, sex, c(hair_color, mass, gender), pct = c("row", NA, "col")),
      tab(data, sex, c(hair_color, mass, gender), pct = c("row", NA, "col"))
    )
    tab_many(data, c(sex, gender), hair_color, pct = c("row", "col")) |> length() |>
      testthat::expect_equal(2)
  })
})



testthat::test_that("a per-row_var `pct` list is refused, and says why", {
  suppressWarnings(
    testthat::expect_error(
      tab(data, c(sex, eye_color), c(hair_color, mass, gender),
          pct = list(sex = list("row", "col", "col"), eye_color = list("col", "row", "row"))),
      "must be a character vector"
    )
  )
})



testthat::test_that("tab drops NA consistently with na = 'drop'", {
  tabs1 <- tab(data, gender, hair_color, sex, na = "drop")
  testthat::expect_true(all(!grepl("^NA", dplyr::pull(tabs1, sex), perl = TRUE)))
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
  withr::local_options(lifecycle_verbosity = "quiet")   # it chains the steps ON PURPOSE
  data |>
    tab_plain(col_var = hair_color, tot = c("row", "col"), pct = "row") |>
    #tab_totaltab() |> error
    #tab_tot() |>
    #tab_pct() |>
    tab_ci() |>
    tab_chi2() |>
    testthat::expect_s3_class("tabxplor_tab")
})




# tabs <- tab_many(data, "sex", c("hair_color", "eye_color", "mass"), "gender",
#                  totaltab = "line", totcol = "no")
#
# testthat::test_that("tab_totaltab works with all arguments (and with tab_tot)", {
#   testthat::expect_true(
#     nrow(tabs |> tab_totaltab("line") |> tab_totaltab("no") |> tab_totaltab("table")|>
#            dplyr::filter_at(1, ~ grepl("^Ensemble", ., perl = TRUE)) ) != 0,
#   )
#
#   testthat::expect_identical(
#     nrow(tabs |>
#            tab_totaltab() |> tab_tot() |>
#            dplyr::filter_at(1, ~ grepl("^Ensemble", ., perl = TRUE)) ),
#
#     nrow(tabs |> tab_totaltab(name = "Overall", data = data) |>
#            dplyr::filter_at(1, ~ grepl("^Overall", ., perl = TRUE))  ) + 1L
#   )
#
#   testthat::expect_identical(
#     nrow(tabs |> tab_totaltab("line") |> tab_tot() |>
#            dplyr::filter_at(1, ~ grepl("^Ensemble", ., perl = TRUE)) ),
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
#       dplyr::select(where(~is_fmt(.) & ! tabxplor:::fmt_var_kind(.) == "mean")) |>
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
#         where(is_fmt), ~ grepl(#                                              "\u00b1", format(.), perl = TRUE))) |>
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




# Phase 14p: single-variable / no-col_var frequency tables keep their `n` / `pct` / `wn` columns at
# DISPLAY (a <=1.3.1-breaking regression: the base-count intent used to fold + drop the real `n` column).
testthat::test_that("single-variable frequency table keeps its n column (Phase 14p)", {
  gss <- fx_gss()
  disp <- function(x) names(tabxplor:::tab_materialize_extras(x, backend = "text", pvalue = FALSE))

  # plain count: levels + n
  testthat::expect_setequal(disp(tab(gss, relig)), c("relig", "n"))
  # n = "no" must NOT drop the frequency n (it is primary content, not the display extra)
  testthat::expect_setequal(disp(tab(gss, relig, n = "no")), c("relig", "n"))
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



# === Phase 22c-ii: the degenerate margin has no odds ratio ==========================================

testthat::test_that("no odds ratio on the margin the percentage sums to", {
  g <- fx_gss_fmt()
  # an odds ratio needs a 2x2; on the row-% Total column every cell IS the whole block, so the number
  # the sweep divided compared nothing -- and it was colouring that column, and printing a bogus
  # interval in its tooltip.
  t <- tab(g, race, party3, pct = "row", na = "drop_all", color = "OR",
           color_signif = "grey_non_signif")
  tot <- t$Total
  testthat::expect_true(all(is.na(get_or(tot))))
  testthat::expect_true(all(is.na(get_ci_inf(tot))) && all(is.na(get_ci_sup(tot))))
  testthat::expect_true(all(is.na(get_pvalue(tot))))
  testthat::expect_true(all(unlist(tabxplor:::fmt_color_channels(tot)) == 0L))
  # ... and it is out of the colour legend, which names the columns a ladder actually reads
  testthat::expect_false(grepl("Total", paste(tab_color_legend(t, medium = "plain"), collapse = " ")))
  # the DATA columns keep theirs
  testthat::expect_false(all(is.na(get_or(t[[2]]))))
  # symmetric under pct = "col": there the degenerate margin is the Total ROW
  t2 <- tab(g, race, party3, pct = "col", na = "drop_all", color = "OR")
  tr <- is_totrow(t2[[2]])
  testthat::expect_true(all(is.na(get_or(t2[[2]])[tr])))
  testthat::expect_false(all(is.na(get_or(t2[[2]])[!tr])))
})




# === SECTION: n_min: dropping and blanking weak bases =============================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())




gss <- fx_gss() |> dplyr::filter(race != "Not applicable") |>
  dplyr::mutate(race = droplevels(race))



# A controlled two-col_var fixture: under na = "drop" each col_var keeps its own non-NA base, so
# row "A" has a large base on c1 (50) but a tiny base on c2 (5) -> row kept, c2 cell blanked.
nmin_df <- tibble::tibble(
  g  = factor(rep(c("A", "B"), each = 50)),
  c1 = factor(rep(c("a", "b"), 50)),                                       # base 50 in each group
  c2 = factor(c(rep("p", 5), rep(NA_character_, 45),                       # group A: base 5
                rep("p", 40), rep("q", 10)))                               # group B: base 50
)



testthat::test_that("pct='row': a whole row is dropped only when its max base < n_min", {
  base <- tab(gss, race, marital, pct = "row")
  # "Other" race has the smallest base; a threshold between it and the next drops only that row.
  other_n <- base |> dplyr::filter(race == "Other") |> dplyr::pull(Total) |> get_tot_n()
  black_n <- base |> dplyr::filter(race == "Black") |> dplyr::pull(Total) |> get_tot_n()
  thr     <- ceiling((other_n + black_n) / 2)

  out <- tab(gss, race, marital, pct = "row", n_min = thr)
  testthat::expect_false("Other" %in% as.character(out$race))   # dropped (base < thr)
  testthat::expect_true("Black" %in% as.character(out$race))    # kept   (base >= thr)
  testthat::expect_true("Total" %in% as.character(out$race))    # total row always survives
})



testthat::test_that("a kept row blanks only the cells whose OWN base < n_min", {
  # Two col_vars (c1 a/b, c2 p/q). Under na = "drop" each keeps its own base: row A has c1 base
  # 50 but c2 base 5 -> row A stays (max 50 >= 10) but its c2 cells (base 5) blank.
  out <- tab(nmin_df, g, col_vars = c(c1, c2), pct = "row", na = "drop", n_min = 10)

  row_a <- dplyr::filter(out, g == "A")
  # The c2 "p" cell for A must render as an empty string (its base 5 < 10).
  testthat::expect_true(all(format(dplyr::pull(row_a, "p")) == ""))
  # c1 cells for A are untouched (base 50, non-empty).
  testthat::expect_false(all(format(dplyr::pull(row_a, "a")) == ""))

  # row B: c2 base 50 -> nothing blanked.
  row_b <- dplyr::filter(out, g == "B")
  testthat::expect_false(any(format(dplyr::pull(row_b, "p")) == ""))
})



# Phase 20h: built at top level, where the file-level lifecycle line bites.
sw_mass <- dplyr::starwars |> tab_prepare("sex", "mass", other_if_less_than = 0)



testthat::test_that("format() renders the 'blank' display token as an empty string", {
  col <- tab(gss, race, marital, pct = "row") |> dplyr::pull("Married")
  blanked <- set_display(col, "blank")
  testthat::expect_true(all(format(blanked) == ""))
  # get_num() of a blank cell is NA (non-destructive: the pct field is untouched).
  testthat::expect_true(all(is.na(get_num(blanked))))
  testthat::expect_equal(get_pct(blanked), get_pct(col))
})




# === SECTION: haven-style value labels become levels ==============================================

mklab <- function(codes, labels, label = NULL) {
  x <- structure(codes, labels = labels)
  if (!is.null(label)) attr(x, "label") <- label
  x
}



test_that("val_labels_to_factor: complete labels -> factor in labels order", {
  x <- mklab(c(2, 1, 1, 2, 1), c(No = 1, Yes = 2), "Agreement")
  f <- val_labels_to_factor(x)
  expect_s3_class(f, "factor")
  expect_identical(levels(f), c("No", "Yes"))
  expect_identical(as.character(f), c("Yes", "No", "No", "Yes", "No"))
  expect_null(attr(f, "labels"))
})



test_that("tab(): a labelled row/col var uses value labels as levels", {
  set.seed(1)
  n <- 200
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe"),
    avis = mklab(sample(c(1, 2, 3), n, TRUE),
                 c("1-Pour" = 1, "2-Contre" = 2, "3-NSP" = 3), "Avis")
  )
  t1 <- tab(df, sexe, avis, pct = "row")
  expect_true(all(c("Homme", "Femme") %in% levels(t1[[1]])))
  expect_true(all(c("1-Pour", "2-Contre", "3-NSP") %in% names(t1)))
})



test_that("variable labels are stored in meta$vars$var_labels (absent when none)", {
  set.seed(3)
  n <- 80
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe de l'enquete"),
    plain = factor(sample(c("x", "y"), n, TRUE))
  )
  t <- tab(df, sexe, plain, pct = "row")
  va <- get_vars_attr(t)
  expect_identical(va$var_labels[["sexe"]], "Sexe de l'enquete")

  g <- tab(fx_gss(), race, marital, pct = "row")   # no labels anywhere
  expect_null(get_vars_attr(g)$var_labels)
})
