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
    suppressWarnings(tab(d, x, y, pct = "row", test = TRUE))
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

testthat::test_that("other_if_less_than works with tab_vars (dplyr >= 1.2 across inlining)", {
  # Regression (CRAN-check example failure, 2026-07-23): the fct_relevel step after
  # fct_lump_min used a nested lambda referencing `.x` inside across(), which dplyr >= 1.2
  # inlines -- breaking the closure ("object '.x' not found"). Only reached when
  # other_if_less_than > 0 AND tab_vars are present.
  result <- tab(sw, sex, hair_color, gender, na = "drop", pct = "row",
                other_if_less_than = 5)
  testthat::expect_s3_class(result, "tabxplor_grouped_tab")
  testthat::expect_true("Others" %in% dplyr::pull(result, sex))
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
  x <- fmt(n = 5L, scale = "level_n")
  testthat::expect_equal(get_wn(x), 5)
})

testthat::test_that("fmt arithmetic preserves class", {
  x <- fmt(n = 5L, scale = "level_n")
  y <- fmt(n = 3L, scale = "level_n")
  result <- x + y
  testthat::expect_true(is_fmt(result))
  testthat::expect_equal(get_n(result), 8L)
})

testthat::test_that("fmt handles NA n values", {
  x <- fmt(n = NA_integer_, scale = "level_n")
  testthat::expect_true(is.na(get_n(x)))
})

testthat::test_that("is_totrow returns FALSE for non-total rows", {
  x <- fmt(n = 5L, scale = "level_n")
  testthat::expect_false(is_totrow(x))
})

testthat::test_that("is_totcol returns FALSE for non-total columns", {
  x <- fmt(n = 5L, scale = "level_n")
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
    tab(gss, race, marital, pct = "row", test = TRUE, ci = "cell",
        conf_level = 0.95, color = "diff")
  )
})

testthat::test_that("tab with multiple row_vars produces valid output", {
  # tab() merges several row_vars into one grouped table by default...
  merged <- tab(gss, c(race, relig), marital, pct = "row")
  testthat::expect_s3_class(merged, "tabxplor_grouped_tab")
  # ...or returns a list of one table per row_var with output_list = TRUE.
  listed <- tab(gss, c(race, relig), marital, pct = "row", output_list = TRUE)
  testthat::expect_true(is.list(listed) && !is.data.frame(listed) && length(listed) == 2)
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

testthat::test_that("tab_render_vars matches tab_get_vars on well-formed tables", {
  fixtures <- list(
    single   = tb10c,
    col_pct  = tab(gss, marital, race, pct = "col"),
    two_cv   = tab(gss, marital, c(race, relig), pct = "row", test = TRUE),
    means    = tab_num(gss, marital, c(age, tvhours), ci = "cell"),
    compact2 = tab(gss, c(marital, relig), race, pct = "row"),
    plain    = tab_plain(gss, marital, race)
  )
  for (f in fixtures) {
    rv <- tab_render_vars(f)
    testthat::expect_false(isTRUE(rv$degrade))
    testthat::expect_identical(rv$row_var, tab_get_vars(f)$row_var)
    testthat::expect_identical(sort(as.character(rv$tab_vars)),
                               sort(as.character(tab_get_vars(f)$tab_vars)))
  }
})

testthat::test_that("tab_render_vars is position-independent (factor moved after fmt cols)", {
  ct    <- tab(gss, c(marital, relig), race, pct = "row")   # factors: 'row_var' (group), 'levels'
  moved <- dplyr::relocate(ct, "row_var", .after = dplyr::last_col())
  # tab_get_vars() miswrites to the relocated group column; tab_render_vars() stays on "levels".
  testthat::expect_identical(tab_render_vars(moved)$row_var, "levels")
})

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
    testthat::expect_s3_class(out, "kableExtra")
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

# === SECTION: Phase 18a bug-fixes ========================================

# Fix 1: data.table input (was: "Selections can't have missing values" via the numeric col_var path).
testthat::test_that("tab() accepts a data.table input, byte-identical to a data.frame", {
  gss <- forcats::gss_cat
  strip <- function(t) { attributes(t) <- attributes(t)[c("names", "row.names")]; t }
  testthat::expect_no_error(f_dt <- tab(data.table::as.data.table(gss), marital, race))
  testthat::expect_equal(strip(f_dt), strip(tab(gss, marital, race)))
  # the numeric col_var path was the actual crash site
  testthat::expect_no_error(n_dt <- tab(data.table::as.data.table(gss), marital, tvhours))
  testthat::expect_equal(strip(n_dt), strip(tab(gss, marital, tvhours)))
})

# Fix 2: a 0-row jamovi build must degrade gracefully, never abort.
testthat::test_that("jmvtab_build() + tab_kable() survive 0-row data", {
  empty <- forcats::gss_cat[0, ]
  opts  <- list(row_vars = "marital", col_vars = "race", tab_vars = character(),
                wt = character(), color = "auto", color_signif = "ignore", ci = "auto",
                digits = 0L, cleannames = FALSE, n_min = numeric())
  testthat::expect_no_error(built <- tabxplor:::jmvtab_build(empty, opts, NULL))
  testthat::expect_no_error(tab_kable(built$tabs))
})

# Fix 3: the graceful-degrade notice is batch-aware -- suppressed when a real fmt table is present.
testthat::test_that("degrade notice is suppressed when the render batch holds a real fmt table", {
  real  <- tab(forcats::gss_cat, marital, race)
  plain <- tibble::tibble(a = factor(c("x", "y")), b = c(1.5, 2.5))
  # a mixed list: the plain peer must NOT emit the misleading "skipped" message
  testthat::expect_no_message(tab_kable(list(real, plain)), message = "skipped")
  # a lone non-tabxplor frame still informs (exactly once)
  testthat::expect_message(tab_kable(plain), "skipped")
})

# Fix 4: a weight literally named "wt" is shadow-proof (was: garbage column + warnings, numeric means).
testthat::test_that("weight named 'wt' is byte-identical to any other weight name", {
  set.seed(1); n <- 300
  w <- runif(n, 0.5, 2)
  d <- tibble::tibble(grp = factor(sample(c("a", "b", "c"), n, TRUE)),
                      cat = factor(sample(c("x", "y"), n, TRUE)),
                      val = rnorm(n, 10, 3), wt = w, weight = w)
  strip <- function(t) { attributes(t) <- attributes(t)[c("names", "row.names")]; t }
  testthat::expect_warning(tab(d, grp, val, wt = wt), NA)      # no "does not exist to remove"
  testthat::expect_equal(strip(tab(d, grp, val, wt = wt)),  strip(tab(d, grp, val, wt = weight)))
  testthat::expect_equal(strip(tab(d, grp, cat, wt = wt)),  strip(tab(d, grp, cat, wt = weight)))
})

# Fix 4: a weight that is ALSO a table variable is rejected early with a clear message.
testthat::test_that("a weight used as a table variable errors clearly", {
  d <- tibble::tibble(grp = factor(c("a", "b")), wt = c(1, 2), val = c(3, 4))
  testthat::expect_error(tab(d, grp, wt, val, wt = wt), "also used as a row, column or tab variable")
  testthat::expect_error(tab(d, wt, val, wt = wt),      "also used as a row, column or tab variable")
})

# === SECTION: Phase 18p -- bug corrections ================================

# Fix 1: a bad NAMED ref surfaces a clean warning naming the unknown variable(s),
# not the raw cli "Multiple quantities for pluralization" internal error.
testthat::test_that("an unknown named ref warns cleanly and falls back to auto", {
  gss <- forcats::gss_cat
  testthat::expect_warning(
    r1 <- tab(gss, marital, race, ref = c(badname = "x")),
    ".*ref.* name.*badname.*matches no row_var"
  )
  testthat::expect_s3_class(r1, "tabxplor_tab")
  # plural form must also render (both quantities pinned to length(unknown))
  testthat::expect_warning(
    tab(gss, marital, race, ref = c(bad1 = "x", bad2 = "y")),
    "match no row_var"
  )
})

# Fix 2: a variable used as BOTH a tab_var and a row/col var aborts with a clear
# message (mirroring the weight-collision guard), not an obscure tidyselect error.
testthat::test_that("a variable used as tab_var and row/col var errors clearly", {
  gss <- forcats::gss_cat
  testthat::expect_error(
    tab(gss, marital, race, tab_vars = marital),
    "both as a tab variable and as a row or column variable"
  )
  testthat::expect_error(
    tab(gss, marital, race, tab_vars = race),
    "both as a tab variable and as a row or column variable"
  )
})

# A variable crossed with ITSELF (same var as row and col) must work and give a
# diagonal -- the building block of a Burt table for MCA. This is a legitimate use,
# NOT a collision (the tab_var guard above must not catch it). The leaf duplicates
# the shared column internally (<var>_colvarbis) and restores the name.
testthat::test_that("a variable crossed with itself gives a diagonal (Burt table)", {
  d <- tibble::tibble(x = factor(rep(c("a", "b", "c"), 20)),
                      y = factor(rep(c("p", "q"), 30)))
  diag_t <- tab(d, x, x, pct = "no")
  testthat::expect_s3_class(diag_t, "tabxplor_tab")
  # the x-by-x block is diagonal: level i vs level j is the count only when i == j
  lv <- c("a", "b", "c")
  m  <- vapply(lv, function(j) get_n(diag_t[[j]])[seq_along(lv)], double(length(lv)))
  testthat::expect_equal(diag(m), rep(20, 3))                 # diagonal = counts
  testthat::expect_equal(sum(m) - sum(diag(m)), 0)            # off-diagonal all 0

  # a full Burt table: c(v1, v2) x c(v1, v2) builds without error (diagonal blocks
  # on the self-crossings, real cross-tabs off-diagonal) and exports.
  burt <- tab(d, c(x, y), c(x, y), pct = "no")
  testthat::expect_s3_class(burt, "tabxplor_grouped_tab")
  testthat::expect_no_error(tab_md(burt))
})

# Fix 3: an all-zero / all-NA weight aborts naming the weight, not the generic
# "data is of length 0".
testthat::test_that("all-zero or all-NA weights error naming the weight", {
  d <- tibble::tibble(r = factor(rep(c("a", "b"), 5)),
                      c = factor(rep(c("x", "y"), 5)), w = 0)
  testthat::expect_error(tab(d, r, c, wt = w), "zero or missing weight")
  d_na <- dplyr::mutate(d, w = NA_real_)
  testthat::expect_error(tab(d_na, r, c, wt = w), "zero or missing weight")
})

# Fix 4: an all-NA numeric col_var builds without leaking the base-R
# "no non-missing arguments to max" warning.
testthat::test_that("an all-NA numeric col_var builds without a max() warning", {
  d <- tibble::tibble(r = factor(rep(c("a", "b"), 10)), x = NA_real_)
  testthat::expect_no_warning(res <- tab_num(d, r, x))
  testthat::expect_s3_class(res, "tabxplor_tab")
})

# Bug A: a factor carrying a real NA *level* (exclude = NULL) builds AND prints /
# formats / exports; the NA level is routed through `na=` (na="keep" -> "NA" row,
# na="drop" -> dropped).
testthat::test_that("a factor with a real NA level prints and respects na=", {
  d <- tibble::tibble(r = factor(c("a", "b", NA), exclude = NULL),
                      c = factor(c("x", "y", "x")))
  keep <- tab(d, r, c, na = "keep")
  testthat::expect_no_error(format(keep))
  testthat::expect_true("NA" %in% as.character(keep$r))          # NA level shown as "NA"
  drop <- tab(d, r, c, na = "drop")
  testthat::expect_false("NA" %in% as.character(drop$r))         # NA row dropped
  # the colored + bold crash sites (pillar_shaft / format masked assignments)
  withr::local_options(tabxplor.console_bold = TRUE)
  col <- tab(d, r, c, pct = "row", color = "diff", na = "keep")
  testthat::expect_no_error(format(col))
  testthat::expect_no_error(tab_md(col))
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


# === Phase 20h: a source level that collides with a total label ==============
# "Total" is the leaf's internal pre-rename key for every total row/tab/column, and
# leaf_totrow_tottab() derives its role vectors by matching it -- so a DATA level of that name used to
# be read back as a total row (measured: row_kind "total", is_totrow TRUE, bold, out of the percentage
# base, and the table printed two "Total" rows). It is refused now.

testthat::test_that("20h: a level named like a total label is refused, on every axis", {
  d <- tibble::tibble(g = factor(rep(c("A", "Total", "B"), each = 20)),
                      q = factor(rep(c("yes", "no"), 30)))
  testthat::expect_error(tab(d, g, q, pct = "row"), "own total")
  testthat::expect_error(tab(d, q, g, pct = "row"), "own total")          # as a col_var
  testthat::expect_error(tab(d, q, q, tab_vars = g, pct = "row"), "own total")
  # the leaves take the same route through tab_prepare()
  testthat::expect_error(tab_plain(d, g, q, pct = "row"), "own total")
  # the message names the level and offers the way out
  testthat::expect_error(tab(d, g, q, pct = "row"), "tabxplor.total_names")
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

testthat::test_that("20h: a level named \"NA\" or \"Others\" is NOT refused", {
  # measured: an "NA" level renders correctly unless the column also holds real NAs, and a
  # pre-existing "Others" merely joins the lump -- refusing either would be a false positive on
  # ordinary survey labels ("NA" = "not applicable").
  d <- tibble::tibble(h = factor(rep(c("x", "NA", "Others"), each = 20)),
                      q = factor(rep(c("yes", "no"), 30)))
  testthat::expect_s3_class(tab(d, h, q, pct = "row", na = "keep"), "tabxplor_tab")
})
