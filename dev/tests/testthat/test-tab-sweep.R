
# === SECTION: tab(), tab_plain() and tab_num() end to end =========================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


data <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)


# Phase 8 total-col decoupling (tab_assemble ~L1770): with several row_vars + several factor col_vars,
# the lone kept total column must read "Total", not the internal "Total_<lastcv>" that leaked before the
# dedup fix. This makes a multi-row_var per-row_var table identical to a standalone single-row_var build
# -- the precondition for the per-row_var parallel dispatch (test-parallel-parity.R).
testthat::test_that("Phase 8: multi-row_var total column is 'Total' (not 'Total_<col_var>')", {
  multi  <- tab(data, c(sex, gender), c(hair_color, eye_color), pct = "row")
  testthat::expect_true("Total" %in% names(multi))
  testthat::expect_false(any(grepl("^Total_", names(multi))))
})


testthat::test_that("Phase 6: output_list / merge / deprecations / KNOWN-BUG fix", {
  gss <- fx_gss()
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

  # Phase 19h (KEY 7): the two extra `totcol` values are accepted SPELLINGS of the base behaviour --
  # exactly one total column -- and must never error. Before, "each" built one total per col_var and
  # "all_col_vars" could not produce its own tot_cols_type at all (the identical() arms compared a
  # character against a list of symbols, so both were dead and every call fell through to "some").
  suppressWarnings({
    each_lst <- tab_many(gss, marital, c(race, relig), pct = "row", totcol = "each")
    all_cv   <- tab_many(gss, marital, c(race, relig), pct = "row", totcol = "all_col_vars")
    base     <- tab(gss, marital, c(race, relig), pct = "row")
  })
  testthat::expect_equal(each_lst, base)
  testthat::expect_equal(all_cv,   base)
  testthat::expect_equal(sum(is_totcol(base)), 1L)
  # ... and an unknown value still aborts, naming the argument that replaced it
  testthat::expect_error(suppressWarnings(tab_many(gss, marital, race, totcol = "tabel")), "totcol")

  # comp = "all" with a ref that is not the total row forces the full total table -- silently, since
  # the added table is then plainly there. A ref matching NOTHING still warns: the cells go empty.
  testthat::expect_no_warning(
    tab(gss, marital, race, tab_vars = year, pct = "row", color = "diff", comp = "all",
        ref = "Married"))
  testthat::expect_warning(
    tab(gss, marital, race, pct = "row", color = "diff", ref = "no-such-level"),
    "matches no row")

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


testthat::test_that("tab_num works (with color)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  testthat::expect_true(
    !is.na(tab_prepare(data, sex, mass) |>
             tab_num(sex, mass, tot = "row", ref = "tot", color = "after_ci") |>

             tab_chi2() |>
             dplyr::pull(mass) |> vec_data() |> dplyr::pull(var) |> dplyr::last())
  )
})


testthat::test_that("tab() works with tribble + pmap (the batch idiom)", {

  tibble::tribble(
    ~row_vars, ~col_vars                          , ~tab_vars     , ~levels,
    "sex"   , "hair_color"                        , NA_character_ , "all"  ,
    "sex"   , c("mass", "hair_color", "eye_color"), "gender"      , "first",
    "sex"   , c("hair_color", "eye_color", "mass"), "gender"      , "all"  ,
  ) |>
    purrr::pmap(tab, data = data, tot = "row", totaltab = "no", output_list = TRUE) |>
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


# Phase 14p: the internal `no_col_var` sentinel must never surface as a spanning col_var name.
testthat::test_that("no_col_var placeholder is not rendered as a col_var name (Phase 14p)", {
  gss <- fx_gss()
  k1  <- as.character(tab_kable(tab(gss, relig)))
  testthat::expect_false(grepl("no_col_var", k1))
  k2  <- as.character(tab_kable(tab(gss, relig, pct = "col")))
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


# Phase 19d-tail: under `levels = "first"` the table SHOWS one level against the merged rest, so its
# odds ratio is the TRUE binary one -- that level against everything else, not a level-vs-ref2 ratio.
# That is what makes showing a single column meaningful. tab() merges before the leaf; the jamovi
# path defers the merge (the aggregate and the whole-table test must see every level), and the
# surviving level is also `ref2`, so every column referenced ITSELF and `or` came out 1 everywhere --
# invisible until 19d made the odds ratio unconditional. The leaf is told the col_var is shown
# dichotomised and rebuilds the complement, so both paths land on the same number.
testthat::test_that("levels = 'first' gives the true binary odds ratio, on both merge paths", {
  gss  <- fx_gss()
  t    <- tab(gss, marital, race, pct = "row", levels = "first", cleannames = FALSE)
  kept <- names(t)[[2]]
  p    <- get_pct(t[[kept]])
  ra   <- length(p)                                  # ref = "tot" -> the Total row, last
  odds <- function(x) x / (1 - x)
  testthat::expect_equal(as.numeric(get_or(t[[kept]])), as.numeric(odds(p) / odds(p[ra])))
  # ... and it is NOT the degenerate self-reference the deferred path used to produce
  testthat::expect_false(all(get_or(t[[kept]]) == 1))

  # a genuinely 2-level col_var is the same statement, and its pre-19d value must not move
  d <- dplyr::mutate(gss, bin = factor(ifelse(race == "White", "white", "other")))
  b <- tab(d, marital, bin, pct = "row", levels = "first", cleannames = FALSE)
  pb <- get_pct(b[[2]])
  testthat::expect_equal(as.numeric(get_or(b[[2]])),
                         as.numeric(odds(pb) / odds(pb[length(pb)])))
})


# ---- Phase 17a janitorial fixes: failing-first fixture ----

test_that("mean-table ref matches an exact label with regex metacharacters (Defect 3, Phase 17a)", {
  # rincome's "$25000 or more" begins with `$` (a regex end-anchor), so a pure-regex reference match
  # fails to find the row. Phase 17f routes tab_num through the shared calculate_refrows()/diff_index(),
  # which try an EXACT label match first (the fix Phase 17a had ported into the now-deleted diff_index_mean).
  d  <- fx_gss() |> dplyr::filter(!is.na(tvhours))
  tt <- tab_num(d, "rincome", "tvhours", ref = "$25000 or more", comp = "tab")

  ref_idx <- which(is_refrow(tt$tvhours))
  expect_length(ref_idx, 1L)                                       # exactly one reference row
  expect_identical(as.character(tt$rincome)[ref_idx], "$25000 or more")
  expect_equal(get_diff(tt$tvhours)[ref_idx], 0)                   # a row compared to itself
})


# ---- Phase 19a (D27): ref / ref2 = "last" ----

test_that('ref2 = "last" resolves to the last LEVEL, never the total column (D27)', {
  # Before 19a "last" was not a sentinel: it fell through to the regex matcher, matched nothing,
  # and first(integer(0)) -> replace_na(0) gave index 0 -> the "no columns were found as reference
  # for comparison" warning and an ALL-NA `or` field. It becomes blocking in 19d, where the odds
  # ratio is computed unconditionally and ref2 is therefore always in force.
  d <- fx_gss()
  expect_silent(a <- tab(d, marital, race, pct = "row", display = "{or}", ref = "first", ref2 = "last"))
  b <- tab(d, marital, race, pct = "row", display = "{or}", ref = "first", ref2 = 3L)   # race: Other | Black | White
  expect_identical(a, b)
  expect_false(all(is.na(get_or(a[[2]]))))                        # non-vacuous: a real `or`
  expect_true(all(get_or(a[[4]]) == 1))                           # White references itself

  # ... and it is NOT the total column: that would make the OR an odds against a total.
  expect_false(identical(a, tab(d, marital, race, pct = "row", display = "{or}", ref = "first", ref2 = 4L)))
})


test_that('ref = "last" picks the last level on both axes, and is not "tot" (D27)', {
  d <- fx_gss()
  # the stored `ref` ATTRIBUTE records the spec the user typed ("last" vs "6"), so compare the
  # per-cell data, which is what "the same reference" means.
  same_cells <- function(x, y) expect_identical(
    lapply(x[-1], vctrs::vec_data), lapply(y[-1], vctrs::vec_data))

  # ROW axis (pct = "row"): the last LEVEL ("Married"), not the Total row that follows it
  r <- tab(d, marital, race, pct = "row", ref = "last")
  expect_identical(as.character(r[[1]])[is_refrow(r[[2]])], "Married")
  same_cells(r, tab(d, marital, race, pct = "row", ref = 6L))
  expect_false(identical(get_diff(r[[2]]), get_diff(tab(d, marital, race,
                                                        pct = "row", ref = "tot")[[2]])))
  # per SUBTABLE when there are tab_vars, like every other ref value
  g <- tab(d, marital, race, year, pct = "row", ref = "last", totaltab = "no")
  expect_identical(unique(as.character(g[[2]])[is_refrow(g[[3]])]), "Married")

  # COLUMN axis (pct = "col"): the last non-total column
  cl <- tab(d, marital, race, pct = "col", ref = "last")
  same_cells(cl, tab(d, marital, race, pct = "col", ref = 3L))
  expect_true(all(get_diff(cl[["White"]]) == 0, na.rm = TRUE))    # White references itself
})


# Phase 22g-vi: an odds ratio is read against a CATEGORY, never against the marginal percentage --
# which includes the cell itself. `ref = "tot"` there was not a choice but a leftover from the table
# the user was reading a moment ago, and it also left the reference row computed yet never MARKED
# (plain_core() wipes `refrows` under "tot").
testthat::test_that('an odds ratio silently reads `ref = "tot"` as its own first level', {
  g  <- fx_gss_fmt()
  or <- function(...) tab(g, race, party3, pct = "row", na = "drop_all", ...)
  a  <- or(color = "odds_ratio", ref = "tot")
  b  <- or(color = "odds_ratio", ref = "first")
  testthat::expect_identical(get_or(a[[2]]), get_or(b[[2]]))
  # ...and the row it fell back to is marked as the reference, which "tot" used to erase
  testthat::expect_identical(which(is_refrow(a[[2]])), 1L)
  # the DISPLAY names the measure just as `color` does, and gets the same answer
  testthat::expect_identical(get_or(or(display = "or", ref = "tot")[[2]]),
                             get_or(b[[2]]))
  # ⚠ no other measure moves: only one that DECLARES a reference of its own overrides "tot"
  d <- or(color = "difference", ref = "tot")
  testthat::expect_true(!any(is_refrow(d[[2]])))
  testthat::expect_identical(get_diff(d[[2]]),
                             get_diff(or(color = "difference")[[2]]))
})


# === Phase 22i: a positional reference names a GROUP, never the missing-value level ===============
# ⚠ `na = "keep"` appends the level TAB_NA_LEVEL last (fct_na_value_to_level sorts na.last), so
# `ref = "last"` selected it -- the missing values became the thing every other row was compared to.
# It is a level but not a GROUP; naming it explicitly still selects it.
test_that("ref = 'last' skips the NA level on both axes, and ref = 'NA' still names it", {
  d <- fx_gss()
  d$age[1:400] <- NA
  refrow <- function(t) {
    rv <- tabxplor:::tab_render_vars(t)
    fm <- t[[which(vapply(t, is_fmt, logical(1)))[1]]]
    as.character(t[[rv$row_var]])[which(is_refrow(fm))[1]]
  }
  # the ROW axis (row %)
  t <- tab(d, age, marital, pct = "row", ref = "last", na = "keep", shape = c(age = "quartiles"))
  expect_true("NA" %in% levels(droplevels(t[[1]])))     # the level IS there
  expect_false(refrow(t) == "NA")                       # it is simply not the reference
  expect_equal(refrow(t), "60 to 89")                   # the last BAND is
  # naming it is still the way to get it
  t2 <- tab(d, age, marital, pct = "row", ref = "NA", na = "keep", shape = c(age = "quartiles"))
  expect_equal(refrow(t2), "NA")
  # the COLUMN axis (col %)
  t3 <- tab(d, marital, age, pct = "col", ref = "last", na = "keep", shape = c(age = "quartiles"))
  refcol <- names(t3)[vapply(t3, function(x) is_fmt(x) && isTRUE(is_refcol(x)), logical(1))]
  expect_equal(refcol, "60 to 89")
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


# Phase 20h: built at top level, where the file-level lifecycle line bites.
sw_mass <- dplyr::starwars |> tab_prepare("sex", "mass", other_if_less_than = 0)


testthat::test_that("n_min preserves class and table attributes", {
  base <- tab(gss, race, marital, tab_vars = year, pct = "row", test = TRUE)
  # tab_vars -> a list of grouped tabs; apply n_min and check the first survives as a tab.
  out  <- tab(gss, race, marital, tab_vars = year, pct = "row", test = TRUE,
              n_min = 200, output_list = TRUE)
  testthat::expect_true(inherits(out[[1]], "tabxplor_tab") ||
                          inherits(out[[1]], "tabxplor_grouped_tab"))
  testthat::expect_false(is.null(get_test(out[[1]])))   # test attribute survives
})


# === SECTION: haven-style value labels become levels ==============================================

mklab <- function(codes, labels, label = NULL) {
  x <- structure(codes, labels = labels)
  if (!is.null(label)) attr(x, "label") <- label
  x
}


test_that("cleannames strips a value-label prefix turned into a factor level", {
  set.seed(2)
  n <- 120
  df <- tibble::tibble(
    avis = mklab(sample(c(1, 2, 3), n, TRUE),
                 c("1-Pour" = 1, "2-Contre" = 2, "3-NSP" = 3), "Avis"),
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe")
  )
  t2 <- tab(df, avis, sexe, pct = "row", cleannames = TRUE)
  expect_true("Pour" %in% levels(t2[[1]]))
  expect_false(any(grepl("^[0-9]-", levels(t2[[1]]))))
})


test_that("merged (>=2 row_vars) name column swaps to labels under the option", {
  set.seed(5)
  n <- 160
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe de l'enquete"),
    zone = mklab(sample(c(1, 2), n, TRUE), c(Ville = 1, Campagne = 2), "Zone d'habitat"),
    avis = mklab(sample(c(1, 2), n, TRUE), c(Oui = 1, Non = 2), "Avis")
  )
  t <- tab(df, c(sexe, zone), avis, pct = "row")
  md_on <- withr::with_options(list(tabxplor.var_labels = TRUE), tab_md(t, css = FALSE))
  expect_true(any(grepl("Sexe de l'enquete", md_on)))
  expect_true(any(grepl("Zone d'habitat", md_on)))
})


test_that("tab_num() with a labelled grouping var uses value labels", {
  set.seed(6)
  n <- 150
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe"),
    age  = rnorm(n, 45, 12)
  )
  t3 <- tab_num(df, sexe, age)
  expect_true(all(c("Homme", "Femme") %in% levels(t3[[1]])))
})


test_that("tab_counts() with a labelled key uses value labels", {
  cnts <- tibble::tibble(
    g = mklab(c(1, 1, 2, 2), c(A = 1, B = 2), "Groupe"),
    h = factor(c("p", "q", "p", "q")),
    n = c(10, 20, 30, 40)
  )
  tc <- tab_counts(cnts, g, h, counts = n)
  expect_true(all(c("A", "B") %in% levels(tc[[1]])))
})


test_that("tab_reg(): a labelled predictor shows value-label levels; labels stored", {
  set.seed(7)
  n <- 250
  df <- tibble::tibble(
    bin  = mklab(sample(c(0, 1), n, TRUE), c(Non = 0, Oui = 1), "Reponse binaire"),
    avis = mklab(sample(c(1, 2, 3), n, TRUE), c(Pour = 1, Contre = 2, NSP = 3), "Avis")
  )
  tr <- tab_reg(df, outcome = "bin", predictors = "avis", family = "binomial")
  expect_true(any(c("Contre", "NSP") %in% as.character(tr$levels)))
  expect_identical(get_vars_attr(tr)$var_labels[["avis"]], "Avis")
})


# === SECTION: tab(), tab_plain() and tab_num() end to end =========================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


data <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)


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


testthat::test_that("tab_plain works with OR", {
  tab_plain(data, sex, hair_color, pct = "row", display = "{or}", ref = "first")            |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, pct = "col", display = "{or} ({pct})", ref = "first")        |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, hair_color, pct = "row", display = "{or}", ref = "^male")       |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "row", display = "{or}", ref = 2)     |> testthat::expect_s3_class("tabxplor_tab")

  tab_plain(data, sex, hair_color, gender, pct = "row", display = "{or}", ref = "tot",
            comp = "all")                                             |> testthat::expect_s3_class("tabxplor_tab")
  tab_plain(data, sex, hair_color, gender, pct = "row", display = "{or}", ref = 3,
            comp = "all", totaltab = "table")                         |> testthat::expect_s3_class("tabxplor_tab")
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


testthat::test_that("tab works with numeric variables", {
  tab(data, sex, mass)         |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, sex, mass, gender) |> testthat::expect_s3_class("tabxplor_grouped_tab")
})


testthat::test_that("tab works with several col_vars", {
  tab(data, sex, c(hair_color, eye_color), pct = "row")            |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, sex, c(hair_color, mass, gender), pct = "row")         |> testthat::expect_s3_class("tabxplor_tab")
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
    testthat::expect_true(all(!grepl("^NA", dplyr::pull(tabs3, sex), perl = TRUE)))

    tabs4 <- tab_many(data, gender, hair_color, sex, na_drop_all = gender)
    testthat::expect_true(all(!grepl("^NA", dplyr::pull(tabs4, sex), perl = TRUE)))
    testthat::expect_true(any(grepl("^NA", names(tabs4), perl = TRUE)))
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


testthat::test_that("tab works with and without the base count and add_pct", {
  tab(data, "sex", "hair_color", pct = "row", color = "diff", n       = "no")                 |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "row", color = "diff", n       = "no", add_pct = TRUE) |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "row", color = "diff", add_pct = TRUE)                  |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "col", color = "diff", n       = "no")                 |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "col", color = "diff", n       = "no", add_pct = TRUE) |> testthat::expect_s3_class("tabxplor_tab")
  tab(data, "sex", "hair_color", pct = "col", color = "diff", add_pct = TRUE)                  |> testthat::expect_s3_class("tabxplor_tab")
})


testthat::test_that("levels = 'first' keeps NA rows in the row_var (na = 'keep')", {
  d <- tibble::tibble(
    g   = factor(c("A","A","B","B", NA, NA)),                    # a row_var with NA
    two = factor(c("x","y","x","y","x","y"))
  )
  t <- tab(d, g, two, pct = "row", levels = "first", na = "keep")
  testthat::expect_true("NA" %in% as.character(t[[1]]))          # the NA row_var group stays
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


testthat::test_that("n_min = 0 is a no-op (byte-identical to no filter)", {
  base <- tab(gss, race, marital, pct = "row")
  same <- tab(gss, race, marital, pct = "row", n_min = 0)
  testthat::expect_equal(base, same)
})


testthat::test_that("pct='col': weak columns are dropped, totals kept", {
  base <- tab(gss, race, marital, pct = "col")
  full_cols <- names(base)
  out  <- tab(gss, race, marital, pct = "col", n_min = 5000)
  # Small marital categories (base < 5000) drop; "Married" (large) and "Total" survive.
  testthat::expect_true("Married" %in% names(out))
  testthat::expect_true("Total"   %in% names(out))
  testthat::expect_false("Separated" %in% names(out))  # a small column
  testthat::expect_lt(ncol(out), ncol(base))
})


# Phase 20h: built at top level, where the file-level lifecycle line bites.
sw_mass <- dplyr::starwars |> tab_prepare("sex", "mass", other_if_less_than = 0)


testthat::test_that("means use the n base (not tot_n) for the drop", {
  sw <- sw_mass
  # Drop sex groups whose n is below the threshold; the numeric mean column drives it.
  out <- tab(sw, sex, mass, pct = "no", n_min = 5)
  # "hermaphroditic"/"none" are tiny groups -> dropped; "male" (large) survives; Total kept.
  testthat::expect_true("male"  %in% as.character(out$sex))
  testthat::expect_true("Total" %in% as.character(out$sex))
})


testthat::test_that("n_min never drops the total row, and keeps the base-count intent + test attr", {
  out <- tab(gss, race, marital, pct = "row", test = TRUE, n = "range", n_min = 3000)
  # total row present
  testthat::expect_true(any(is_totrow(out$Total)))
  # Phase 10i-B: the base count / add_pct / p-value are now DISPLAY-only -- the built "core" table has NO `n`
  # column and no p-value body row; n_min runs on the core and must PRESERVE the display intent (the
  # `render_extras` attribute) + the `test` attribute so the extras/p-values still materialise at
  # display. No body cell has an NA n now.
  testthat::expect_false("n" %in% names(out))
  testthat::expect_identical(get_render_extras(out)$n, "range")
  testthat::expect_false(any(is.na(get_n(out$Total))))
  testthat::expect_false(is.null(get_test(out)))
  testthat::expect_gt(nrow(get_test(out)), 0)
})


# === SECTION: haven-style value labels become levels ==============================================

mklab <- function(codes, labels, label = NULL) {
  x <- structure(codes, labels = labels)
  if (!is.null(label)) attr(x, "label") <- label
  x
}


test_that("val_labels_to_factor: incomplete labels -> underlying numeric, labels dropped", {
  y <- mklab(c(10, 20, 98), c(refused = 98), "Income")
  z <- val_labels_to_factor(y)
  expect_true(is.numeric(z))
  expect_identical(unname(z), c(10, 20, 98))
  expect_null(attr(z, "labels"))
})


test_that("val_labels_to_factor: no `labels` attr -> unchanged (byte-identity)", {
  p <- factor(c("a", "b"))
  expect_identical(val_labels_to_factor(p), p)
  expect_identical(val_labels_to_factor(1:3), 1:3)
  expect_identical(val_labels_to_factor(c("x", "y")), c("x", "y"))
})


test_that("val_labels_to_factor: an unobserved labelled level is dropped, duplicate labels merge", {
  x <- mklab(c(1, 1, 2), c(No = 1, Yes = 2, Maybe = 3))   # 3 never observed
  f <- val_labels_to_factor(x)
  expect_identical(levels(f), c("No", "Yes"))
  d <- mklab(c(1, 2, 3), c(Lo = 1, Hi = 2, Hi = 3))       # 2 and 3 share the "Hi" label
  fd <- val_labels_to_factor(d)
  expect_identical(levels(fd), c("Lo", "Hi"))
  expect_identical(as.character(fd), c("Lo", "Hi", "Hi"))
})


test_that("tabxplor.var_labels swaps names for labels in exports only, structure unchanged", {
  set.seed(4)
  n <- 120
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe de l'enquete"),
    avis = mklab(sample(c(1, 2), n, TRUE), c(Oui = 1, Non = 2), "Avis exprime")
  )
  t <- tab(df, sexe, avis, pct = "row")

  md_off <- withr::with_options(list(tabxplor.var_labels = FALSE), tab_md(t, css = FALSE))
  md_on  <- withr::with_options(list(tabxplor.var_labels = TRUE),  tab_md(t, css = FALSE))
  expect_true(any(grepl("sexe", md_off)))
  expect_false(any(grepl("Sexe de l'enquete", md_off)))
  expect_true(any(grepl("Sexe de l'enquete", md_on)))
  expect_true(any(grepl("Avis exprime", md_on)))

  # structure keeps canonical names -> select() by the real name still works with the option on
  withr::with_options(list(tabxplor.var_labels = TRUE), {
    expect_identical(names(dplyr::select(t, sexe)), "sexe")
  })
})
