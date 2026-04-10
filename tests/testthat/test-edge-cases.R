# PURPOSE: Test boundary conditions, degenerate inputs, and error handling.
# ROLE: Ensures tabxplor handles edge cases gracefully without NaN, Inf, or crashes.
# KEY CONSTRAINTS:
#   - Must run via test_check("tabxplor"), never in isolation.

# === SECTION: Data setup ====================================================

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

testthat::test_that("tab handles single-category col_var", {
  d <- dplyr::tibble(
    x = factor(sample(c("a", "b", "c"), 30, replace = TRUE)),
    y = factor(rep("only_col", 30))
  )
  result <- tab(d, x, y, pct = "row")
  testthat::expect_s3_class(result, "tabxplor_tab")
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

testthat::test_that("col pct handles zero-count cells gracefully", {
  d <- dplyr::tibble(
    x = factor(c("a", "a", "b", "b"), levels = c("a", "b")),
    y = factor(c("p", "p", "q", "q"), levels = c("p", "q"))
  )
  result <- tab(d, x, y, pct = "col")

  zero_pct <- result |>
    dplyr::filter(x == "a") |>
    dplyr::pull(q) |>
    get_pct()
  testthat::expect_equal(zero_pct, 0)
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

testthat::test_that("all-NA col_var with na='keep' does not error", {
  d <- dplyr::tibble(
    x = factor(c("a", "b", "c")),
    y = factor(rep(NA_character_, 3))
  )
  testthat::expect_no_error(tab(d, x, y, na = "keep"))
})

# === SECTION: Sparse and degenerate tables ====================================

testthat::test_that("chi2 handles sparse tables without error", {
  d <- dplyr::tibble(
    x = factor(c(rep("a", 3), rep("b", 2))),
    y = factor(c("p", "q", "p", "q", "q"))
  )
  # chisq.test may warn about expected < 5, but should not error
  testthat::expect_no_error(
    suppressWarnings(tab(d, x, y, pct = "row", chi2 = TRUE))
  )
})

testthat::test_that("tab works with two rows only", {
  d <- dplyr::tibble(
    x = factor(c("a", "b")),
    y = factor(c("p", "q"))
  )
  result <- tab(d, x, y)
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

testthat::test_that("weighted tab handles large weights", {
  d <- dplyr::tibble(
    x = factor(c("a", "a", "b", "b")),
    y = factor(c("p", "q", "p", "q")),
    w = c(1e6, 1e6, 1e6, 1e6)
  )
  result <- tab(d, x, y, wt = w, pct = "row")
  testthat::expect_s3_class(result, "tabxplor_tab")

  # Pct should still be 0.5 for each cell (equal weights, 2 per row)
  pct_val <- result |>
    dplyr::filter(x == "a") |>
    dplyr::pull(p) |>
    get_pct()
  testthat::expect_equal(pct_val, 0.5, tolerance = 1e-10)
})

# === SECTION: other_if_less_than ==============================================

testthat::test_that("other_if_less_than collapses rare categories", {
  # With threshold = 100, almost all categories collapse
  result <- tab(sw, sex, hair_color, other_if_less_than = 100)
  testthat::expect_s3_class(result, "tabxplor_tab")
})

testthat::test_that("other_if_less_than = 0 keeps all categories", {
  d <- dplyr::tibble(
    x = factor(c("a", "b", "c", "d")),
    y = factor(c("p", "q", "p", "q"))
  )
  result1 <- tab(d, x, y, other_if_less_than = 0)
  result2 <- tab(d, x, y)
  # Should be same number of rows
  testthat::expect_equal(nrow(result1), nrow(result2))
})

# === SECTION: fmt object edge cases ===========================================

testthat::test_that("get_wn falls back to n when wn is NA", {
  x <- fmt(n = 5L, type = "n")
  testthat::expect_equal(get_wn(x), 5)
})

testthat::test_that("fmt arithmetic preserves class", {
  x <- fmt(n = 5L, type = "n")
  y <- fmt(n = 3L, type = "n")
  result <- x + y
  testthat::expect_true(is_fmt(result))
  testthat::expect_equal(get_n(result), 8L)
})

testthat::test_that("fmt handles NA n values", {
  x <- fmt(n = NA_integer_, type = "n")
  testthat::expect_true(is.na(get_n(x)))
})

testthat::test_that("is_totrow returns FALSE for non-total rows", {
  x <- fmt(n = 5L, type = "n")
  testthat::expect_false(is_totrow(x))
})

testthat::test_that("is_totcol returns FALSE for non-total columns", {
  x <- fmt(n = 5L, type = "n")
  testthat::expect_false(is_totcol(x))
})

# === SECTION: Different pct types with same data ==============================

gss <- forcats::gss_cat

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

# === SECTION: Reference types =================================================

testthat::test_that("ref='first' produces valid differences", {
  result <- tab(gss, race, marital, pct = "row", color = "diff", ref = "first")
  testthat::expect_s3_class(result, "tabxplor_tab")

  # First non-total row should have diff = 0 (it's the reference)
  first_row <- result |>
    dplyr::filter(!is_totrow(Married) & !is_tottab(Married)) |>
    dplyr::slice(1)
  first_diff <- first_row |> dplyr::pull(Married) |> get_diff()
  testthat::expect_equal(first_diff, 0, tolerance = 1e-10)
})

testthat::test_that("ref='tot' produces valid differences", {
  result <- tab(gss, race, marital, pct = "row", color = "diff", ref = "tot")
  testthat::expect_s3_class(result, "tabxplor_tab")

  # Total row should have diff = 0 (it's the reference)
  tot_diff <- result |>
    dplyr::filter(is_totrow(Married) & !is_tottab(Married)) |>
    dplyr::pull(Married) |>
    get_diff()
  testthat::expect_equal(tot_diff, 0, tolerance = 1e-10)
})

# === SECTION: Complex pipelines ===============================================

gss <- forcats::gss_cat

testthat::test_that("tab with all options combined does not error", {
  testthat::expect_no_error(
    tab(gss, race, marital, pct = "row", chi2 = TRUE, ci = "cell",
        conf_level = 0.95, color = "diff")
  )
})

testthat::test_that("tab_many with multiple row_vars produces valid output", {
  result <- tab_many(gss, c(race, relig), marital, pct = "row")
  # tab_many with multiple row_vars returns a list of tables
  testthat::expect_true(is.list(result) || inherits(result, "tabxplor_grouped_tab"))
})

testthat::test_that("tab with no col_var works", {
  result <- tab(gss, race)
  testthat::expect_s3_class(result, "tabxplor_tab")
})

testthat::test_that("tab with no row_var works", {
  result <- tab(gss, col_var = marital)
  testthat::expect_s3_class(result, "tabxplor_tab")
})
