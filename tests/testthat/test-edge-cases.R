# PURPOSE: Test boundary conditions, degenerate inputs, and error handling.
# ROLE: Ensures tabxplor handles edge cases gracefully without NaN, Inf, or crashes.
# KEY CONSTRAINTS:
#   - Must run via test_check("tabxplor"), never in isolation.

# === SECTION: Data setup ====================================================

# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())



sw <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)


# === SECTION: Single-category variables =======================================

testthat::test_that("tab handles single-category row_var", {
  d <- dplyr::tibble(
    x = factor(rep("only_level", 20)),
    y = factor(sample(c("a", "b"), 20, replace = TRUE))
  )
  result <- tab(d, x, y, pct = "row")
  testthat::expect_s3_class(result, "tabxplor_tab")

  # Row pct of "only_level" for non-total columns should sum to 1
  fmt_cols <- names(result)[purrr::map_lgl(result, is_fmt)]
  non_tot <- fmt_cols[!purrr::map_lgl(result[fmt_cols], is_totcol)]
  row_data <- result |> dplyr::filter(x == "only_level")
  pct_vals <- purrr::map_dbl(non_tot, ~ get_pct(row_data[[.]])[1])
  pct_sum <- sum(pct_vals, na.rm = TRUE)
  testthat::expect_equal(pct_sum, 1, tolerance = 1e-10)
})


# === SECTION: Zero-count cells ================================================

testthat::test_that("tab handles zero-count cells without NaN", {
  d <- dplyr::tibble(
    x = factor(c("a", "a", "b", "b"), levels = c("a", "b")),
    y = factor(c("p", "p", "q", "q"), levels = c("p", "q"))
  )
  # a/q and b/p have zero counts
  result <- tab(d, x, y, pct = "row")

  zero_pct <- result |>
    dplyr::filter(x == "a") |>
    dplyr::pull(q) |>
    get_pct()
  testthat::expect_equal(zero_pct, 0)

  # Full row pct should still sum to 1
  fmt_cols <- names(result)[purrr::map_lgl(result, is_fmt)]
  non_tot <- fmt_cols[!purrr::map_lgl(result[fmt_cols], is_totcol)]
  row_data <- result |> dplyr::filter(x == "a")
  pct_vals <- purrr::map_dbl(non_tot, ~ get_pct(row_data[[.]])[1])
  pct_sum <- sum(pct_vals, na.rm = TRUE)
  testthat::expect_equal(pct_sum, 1, tolerance = 1e-10)
})


# === SECTION: NA handling =====================================================

testthat::test_that("na = 'drop' removes NAs from counts", {
  result_keep <- tab(sw, sex, hair_color, na = "keep")
  result_drop <- tab(sw, sex, hair_color, na = "drop")

  # Get total count from the Total column, total row
  get_grand_total <- function(tabs) {
    fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
    tot_col <- fmt_cols[purrr::map_lgl(tabs[fmt_cols], is_totcol)]
    if (length(tot_col) == 0) return(NA_integer_)
    tabs |>
      dplyr::filter(is_totrow(dplyr::pick(where(is_fmt))[[1]])) |>
      dplyr::pull(!!tot_col[1]) |>
      get_n()
  }

  n_keep <- get_grand_total(result_keep)
  n_drop <- get_grand_total(result_drop)
  testthat::expect_lte(n_drop, n_keep)
})


testthat::test_that("na = 'keep' includes NA as a factor level", {
  result <- tab(sw, sex, hair_color, na = "keep")
  # Should have an NA level in the output
  testthat::expect_s3_class(result, "tabxplor_tab")
})


# === SECTION: Weighted edge cases =============================================

testthat::test_that("weighted tab handles zero weights", {
  d <- dplyr::tibble(
    x = factor(c("a", "a", "b", "b")),
    y = factor(c("p", "q", "p", "q")),
    w = c(0, 1, 1, 0)
  )
  result <- tab(d, x, y, wt = w, pct = "row")
  testthat::expect_s3_class(result, "tabxplor_tab")

  # Weighted count for zero-weight cells should be 0
  wn_zero <- result |>
    dplyr::filter(x == "a") |>
    dplyr::pull(p) |>
    get_wn()
  testthat::expect_equal(wn_zero, 0)
})


# === SECTION: other_if_less_than ==============================================

testthat::test_that("other_if_less_than collapses rare categories", {
  # With threshold = 100, almost all categories collapse
  result <- tab(sw, sex, hair_color, other_if_less_than = 100)
  testthat::expect_s3_class(result, "tabxplor_tab")
})


# === SECTION: Different pct types with same data ==============================

gss <- fx_gss()


testthat::test_that("all pct types produce valid tables on same data", {
  for (pct_type in c("row", "col", "all")) {
    result <- tab(gss, race, marital, pct = pct_type)
    testthat::expect_s3_class(result, "tabxplor_tab")
    # All pct values should be in [0, 1]
    fmt_cols <- names(result)[purrr::map_lgl(result, is_fmt)]
    for (col in fmt_cols) {
      pcts <- get_pct(result[[col]])
      pcts <- pcts[!is.na(pcts)]
      testthat::expect_true(all(pcts >= 0 & pcts <= 1))
    }
  }
})


# === SECTION: Complex pipelines ===============================================

gss <- fx_gss()


testthat::test_that("tab with all options combined does not error", {
  testthat::expect_no_error(
    tab(gss, race, marital, pct = "row", test = TRUE, ci = "cell",
        conf_level = 0.95, color = "diff")
  )
})


testthat::test_that("tab with no col_var works", {
  result <- tab(gss, race)
  testthat::expect_s3_class(result, "tabxplor_tab")
})


testthat::test_that("tab with no row_var works", {
  result <- tab(gss, col_vars = marital)
  testthat::expect_s3_class(result, "tabxplor_tab")
  # singular col_var still works as a soft-deprecated alias (Phase 6f)
  lifecycle::expect_deprecated(res2 <- tab(gss, col_var = marital))
  testthat::expect_s3_class(res2, "tabxplor_tab")
})


# === SECTION: Phase 10c -- render var detection + graceful degrade =============

tb10c <- tab(gss, marital, race, pct = "row", color = "diff")


testthat::test_that("tab_render_vars degrades on malformed shapes with a reason", {
  plain_df  <- tibble::tibble(a = factor(c("x", "y")), b = 1:2)
  no_fmt    <- dplyr::mutate(tb10c, dplyr::across(dplyr::where(is_fmt), get_num))
  no_factor <- dplyr::mutate(tb10c, dplyr::across(dplyr::where(is.factor), as.character))

  testthat::expect_true(tab_render_vars(plain_df)$degrade)
  testthat::expect_true(tab_render_vars(no_fmt)$degrade)
  testthat::expect_true(tab_render_vars(no_factor)$degrade)
  testthat::expect_true(tab_render_vars(42)$degrade)          # not a data frame
  testthat::expect_match(tab_render_vars(no_factor)$reason, "factor")
  testthat::expect_match(tab_render_vars(no_fmt)$reason, "tabxplor_fmt")
})


# Truly-malformed shapes that used to CRASH role detection (dplyr::pull(tabs, integer(0))).
# (empty_tab is a VALID 0-row table -- it keeps fmt + factor columns, so it takes the normal path,
#  not the degrade path; tested for no-error separately.)
degrade_shapes <- list(
  plain_df  = tibble::tibble(a = factor(c("x", "y")), b = c(1.5, 2.5)),
  no_fmt    = dplyr::mutate(tb10c, dplyr::across(dplyr::where(is_fmt), get_num)),
  no_factor = dplyr::mutate(tb10c, dplyr::across(dplyr::where(is.factor), as.character))
)

empty_tab <- dplyr::filter(tb10c, FALSE)


testthat::test_that("tab_kable degrades gracefully (message, no error)", {
  for (nm in names(degrade_shapes)) {
    testthat::expect_message(out <- tab_kable(degrade_shapes[[nm]]), "skipped", info = nm)
    testthat::expect_s3_class(out, "knitr_kable")
  }
})


testthat::test_that("tab_md degrades gracefully (message, no error)", {
  for (nm in names(degrade_shapes)) {
    testthat::expect_message(out <- tab_md(degrade_shapes[[nm]], print = FALSE),
                             "skipped", info = nm)
    testthat::expect_type(out, "character")
  }
})


testthat::test_that("print methods never crash on malformed / empty tabxplor tables", {
  testthat::expect_no_error(utils::capture.output(print(degrade_shapes$no_fmt)))
  testthat::expect_no_error(utils::capture.output(print(degrade_shapes$no_factor)))
  testthat::expect_no_error(utils::capture.output(print(empty_tab)))
})


testthat::test_that("tab_xl degrades gracefully (writes the raw frame, no error)", {
  testthat::skip_if_not_installed("openxlsx2")
  for (nm in names(degrade_shapes)) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    testthat::expect_message(tab_xl(degrade_shapes[[nm]], path = p, open = FALSE),
                             "skipped", info = nm)
    testthat::expect_true(file.exists(p))
  }
})


# Fix 4: a weight that is ALSO a table variable is rejected early with a clear message.
testthat::test_that("a weight used as a table variable errors clearly", {
  d <- tibble::tibble(grp = factor(c("a", "b")), wt = c(1, 2), val = c(3, 4))
  testthat::expect_error(tab(d, grp, wt, val, wt = wt), "also used as a row, column or tab variable")
  testthat::expect_error(tab(d, wt, val, wt = wt),      "also used as a row, column or tab variable")
})


# Fix 2: a variable used as BOTH a tab_var and a row/col var aborts with a clear
# message (mirroring the weight-collision guard), not an obscure tidyselect error.
testthat::test_that("a variable used as tab_var and row/col var errors clearly", {
  gss <- fx_gss()
  testthat::expect_error(
    tab(gss, marital, race, tab_vars = marital),
    "both as a tab variable and as a row or column variable"
  )
  testthat::expect_error(
    tab(gss, marital, race, tab_vars = race),
    "both as a tab variable and as a row or column variable"
  )
})


# Bug B: a logical col_var is accepted (matches tab_plain: FALSE/TRUE levels); a
# Date (or other unsupported) col_var aborts with a clear type message.
testthat::test_that("a logical col_var works and a Date col_var errors clearly", {
  d <- tibble::tibble(r = factor(rep(c("a", "b"), 50)),
                      lg = rep(c(TRUE, FALSE), 50))
  res <- tab(d, r, lg)
  testthat::expect_s3_class(res, "tabxplor_tab")
  testthat::expect_true(all(c("FALSE", "TRUE") %in% names(res)))  # parity with tab_plain
  dd <- tibble::tibble(r = factor(rep(c("a", "b"), 50)),
                       dt = rep(as.Date("2020-01-01") + 0:1, 50))
  testthat::expect_error(tab(dd, r, dt),
                         "must be a factor, character or numeric")
})


testthat::test_that("20h: the refusal follows the OPTION, not the English default", {
  d <- tibble::tibble(g = factor(rep(c("A", "Ensemble", "B"), each = 20)),
                      q = factor(rep(c("yes", "no"), 30)))
  # "Ensemble" is the default total-TAB label, so it is reserved out of the box...
  testthat::expect_error(tab(d, g, q, pct = "row"), "own total")
  # ...and moving tab()'s own labels is what makes the level legal again.
  withr::local_options(tabxplor.total_names = c(row = "TOT", col = "TOT", tab = "ALL"))
  testthat::expect_s3_class(tab(d, g, q, pct = "row"), "tabxplor_tab")
})
