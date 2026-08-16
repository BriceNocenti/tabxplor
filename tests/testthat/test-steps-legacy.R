# PURPOSE: keep the superseded dplyr-era step pipeline honest -- tab_pct() / tab_tot() /
#          tab_totaltab() (R/tab-steps-legacy.R, quarantined out of the live tab() path in Phase
#          17f, but still EXPORTED public API that 1.3.1 users' scripts call).
# ROLE: this file exists because coverage said the trio was at 0% -- its only test calls had been
#       commented out in test-tab.R (the four "all tab functions works with ..." blocks). It is
#       deliberately small: enough to catch a real break, not a second full test suite.
# KEY CONSTRAINT: the strongest assertion available is PARITY -- the documented step chain
#       tab_plain() |> tab_totaltab() |> tab_tot() |> tab_pct() must produce values identical to
#       the modern one-call tab(). That is what a 1.3.1 script depends on, and it pins the trio to
#       the aggregate core rather than to a hand-copied expectation that could drift.
# NOTE: since Phase 20a the five steps are HARD-deprecated (defunct in 2.1.0), so every call here
#       warns. That is asserted once, below, and quieted for the parity blocks -- what they check is
#       the arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.

# The parity blocks below call the deprecated steps on purpose; the two deprecation tests at the
# END of this file use lifecycle::expect_deprecated(), which is unaffected by this option.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())

gss <- forcats::gss_cat

# every fmt cell of a table, as plain numbers -- the value-level identity used below
step_nums <- function(t) unlist(lapply(t[vapply(t, is_fmt, logical(1))], get_num), use.names = FALSE)


testthat::test_that("the step chain reproduces tab() exactly, row and column percentages", {
  chain <- function(pct) {
    tab_plain(gss, marital, race) |> tab_totaltab() |> tab_tot() |> tab_pct(pct)
  }
  testthat::expect_identical(step_nums(chain("row")),
                             step_nums(tab(gss, marital, race, pct = "row", test = FALSE)))
  testthat::expect_identical(step_nums(chain("col")),
                             step_nums(tab(gss, marital, race, pct = "col", test = FALSE)))
})


testthat::test_that("tab_tot() adds the total row / column, with the true counts", {
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
  gt <- tab_plain(gss, marital, race, year) |> tab_totaltab() |> tab_tot() |> tab_pct("row")
  testthat::expect_s3_class(gt, "tabxplor_grouped_tab")
  testthat::expect_true("Ensemble" %in% as.character(gt$year))

  # totaltab = "no" leaves the sub-tables alone; "table" appends one more
  no <- tab_plain(gss, marital, race, year) |> tab_totaltab("no")
  testthat::expect_false("Ensemble" %in% as.character(no$year))
  testthat::expect_gt(nrow(gt), nrow(no))
})


testthat::test_that("the step chain composes with tab_ci() / tab_chi2() in every table shape", {
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
  base <- tab_plain(forcats::gss_cat, marital, race)
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
  base <- tab_plain(forcats::gss_cat, marital, race)
  w <- tryCatch(tab_ci(tab_pct(base, "row")), warning = function(w) conditionMessage(w))
  testthat::expect_match(w, "arithmetic is shared|one pass")
  testthat::expect_s3_class(
    tryCatch(tab_pct(base, "row"), warning = function(w) w), "lifecycle_warning_deprecated")
})
