
# === SECTION: the superseded step chain equals tab() ==============================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())

gss <- fx_gss()

# every fmt cell of a table, as plain numbers -- the value-level identity used below
step_nums <- function(t) unlist(lapply(t[vapply(t, is_fmt, logical(1))], get_num), use.names = FALSE)


testthat::test_that("the step chain reproduces tab() exactly, row and column percentages", {
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  chain <- function(pct) {
    tab_plain(gss, marital, race) |> tab_totaltab() |> tab_tot() |> tab_pct(pct)
  }
  testthat::expect_identical(step_nums(chain("row")),
                             step_nums(tab(gss, marital, race, pct = "row", test = FALSE)))
  testthat::expect_identical(step_nums(chain("col")),
                             step_nums(tab(gss, marital, race, pct = "col", test = FALSE)))
})


testthat::test_that("tab_tot() adds the total row / column, with the true counts", {
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  plain <- tab_plain(gss, marital, race)                    # 6 x 4, no totals yet
  testthat::expect_identical(dim(plain), c(6L, 4L))

  both <- tab_tot(plain)                                    # default: row AND col
  testthat::expect_identical(dim(both), c(7L, 5L))
  testthat::expect_identical(as.character(both$marital[7]), "Total")
  testthat::expect_identical(names(both)[5], "Total")

  testthat::expect_identical(dim(tab_tot(plain, "row")), c(7L, 4L))
  testthat::expect_identical(dim(tab_tot(plain, "col")), c(6L, 5L))

  # ground truth from base R, not from another tabxplor call
  testthat::expect_equal(get_num(both$White)[7], sum(gss$race == "White"))
  testthat::expect_equal(get_num(both[[5]])[1], sum(gss$marital == "No answer"))
})


testthat::test_that("tab_totaltab() adds the 'Ensemble' total table over tab_vars", {
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  gt <- tab_plain(gss, marital, race, year) |> tab_totaltab() |> tab_tot() |> tab_pct("row")
  testthat::expect_s3_class(gt, "tabxplor_grouped_tab")
  testthat::expect_true("Ensemble" %in% as.character(gt$year))

  # totaltab = "no" leaves the sub-tables alone; "table" appends one more
  no <- tab_plain(gss, marital, race, year) |> tab_totaltab("no")
  testthat::expect_false("Ensemble" %in% as.character(no$year))
  testthat::expect_gt(nrow(gt), nrow(no))
})


testthat::test_that("the step chain composes with tab_ci() / tab_chi2() in every table shape", {
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  # Restores the four cases that had been commented out in test-tab.R (:272-307).
  #
  # WARNING -- why those lines were commented out, and the rule this file now pins:
  # tab_plain(pct = "row"/"col") ALREADY appends the Total column, so feeding its result to
  # tab_tot() makes tab_tot() sum an existing Total into a new one and abort. The steps are an
  # either/or with tab_plain's own arguments: either build raw counts with tab_plain() and let the
  # trio add the totals and percentages (the documented chain, used here), or let tab_plain(tot=,
  # pct=) do it and skip tab_tot(). The old block did both at once, which is why it never ran.
  no_tabvars <- tab_plain(gss, marital, race) |>
    tab_totaltab() |> tab_tot() |> tab_pct() |> tab_ci("diff") |> tab_chi2()
  testthat::expect_s3_class(no_tabvars, "tabxplor_tab")

  no_colvar <- tab_plain(gss, marital) |>
    tab_totaltab() |> tab_tot() |> tab_pct("col") |> tab_ci("diff") |> tab_chi2()
  testthat::expect_s3_class(no_colvar, "tabxplor_tab")

  # With no row_var the totals must come from tab_plain(tot=): there is no row axis for tab_tot()
  # to total over. The "# error" note the old block carried on this case is accurate, and stands.
  no_rowvar <- tab_plain(gss, col_var = race, tot = c("row", "col"), pct = "row") |>
    tab_pct() |> tab_ci() |> tab_chi2()
  testthat::expect_s3_class(no_rowvar, "tabxplor_tab")

  as_line <- tab_plain(gss, marital, race, year) |>
    tab_totaltab("line") |> tab_tot() |> tab_pct() |> tab_ci("diff") |> tab_chi2()
  testthat::expect_s3_class(as_line, "tabxplor_grouped_tab")
})


# === SECTION: the deprecation itself (Phase 20a) ==================================================
# The chaining API is deprecated; the COMPUTATIONS are not -- they moved into the leaf in 19j and are
# shared. These tests pin both halves of that sentence, because a reader of the warning could easily
# take it to mean the interval or the test is going away.

testthat::test_that("each legacy step warns, naming the tab() argument that replaces it", {
  withr::local_options(lifecycle_verbosity = "warning")   # this block asserts the warning
  base <- tab_plain(fx_gss(), marital, race)
  msg <- function(expr) tryCatch({ force(expr); NA_character_ },
                                 warning = function(w) conditionMessage(w))
  testthat::expect_match(msg(tab_totaltab(base, "no")),       "totaltab")
  testthat::expect_match(msg(tab_tot(base, "row")),           "\\btot\\b")
  testthat::expect_match(msg(tab_pct(base, "row")),           "\\bpct\\b")
  testthat::expect_match(msg(tab_ci(tab_pct(base, "row"))),   "\\bci\\b")
  testthat::expect_match(msg(tab_chi2(tab_pct(base, "row"))), "\\btest\\b")
})

testthat::test_that("the warning says the ARITHMETIC is shared, not going away", {
  withr::local_options(lifecycle_verbosity = "warning")
  base <- tab_plain(fx_gss(), marital, race)
  w <- tryCatch(tab_ci(tab_pct(base, "row")), warning = function(w) conditionMessage(w))
  testthat::expect_match(w, "arithmetic is shared|one pass")
  testthat::expect_s3_class(
    tryCatch(tab_pct(base, "row"), warning = function(w) w), "lifecycle_warning_deprecated")
})


# === Phase 10: `ref` on the deprecated step path actually applies =================================

testthat::test_that("tab_pct(ref =) writes the difference it promises", {
  # The gate read fmt_kind_label(), which RENDERS "row%" and so never matched "row": `ref` was a
  # silent no-op on every percentage table, and on a mixed one the switch fell through to NULL.
  d <- fx_gss()
  t <- suppressWarnings(
    tab_plain(d, race, marital) |> tab_tot() |> tab_pct("row", ref = "tot", color = TRUE))
  col <- purrr::keep(t, is_fmt)[[1]]
  testthat::expect_true(any(!is.na(get_diff(col))))
  testthat::expect_identical(get_ref_type(col), "tot")
  testthat::expect_identical(get_color(col), "difference")
  # a column percentage takes its reference on the other axis, and still gets one
  t2 <- suppressWarnings(
    tab_plain(d, race, marital) |> tab_tot() |> tab_pct("col", ref = "tot"))
  testthat::expect_true(any(!is.na(get_diff(purrr::keep(t2, is_fmt)[[1]]))))
})
