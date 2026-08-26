# PURPOSE: the render-level transpose.
# ROLE: the shipped CONTRACT for R/tab-transpose-render.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the render-level transpose ==========================================================

gss <- fx_gss()



# a small wrapper: the object-level tab_transpose() is soft-deprecated (use transpose = TRUE), but still
# supported for the single-row_var round-trip -- silence the deprecation where we test it on purpose.
xpose <- function(...) {
  withr::local_options(lifecycle_verbosity = "quiet")
  tab_transpose(...)
}



# === SECTION: DEPRECATED object-level tab_transpose() (single row_var) ========

testthat::test_that("transpose of a row% table == a native col% table (structure + render)", {
  orig   <- tab(gss, marital, race, pct = "row", color = "diff")
  tr     <- xpose(orig)
  native <- tab(gss, race, marital, pct = "col", color = "diff")

  testthat::expect_identical(names(tr), names(native))
  testthat::expect_identical(tab_get_vars(tr)$row_var, "race")
  testthat::expect_identical(tab_get_vars(tr)$col_vars, "marital")
  # axis flags per fmt column
  for (nm in names(tr)[purrr::map_lgl(tr, is_fmt)]) {
    testthat::expect_identical(get_scale(tr[[nm]]),    get_scale(native[[nm]]))
    testthat::expect_identical(get_pct_type(tr[[nm]]), get_pct_type(native[[nm]]))
    testthat::expect_identical(get_col_var(tr[[nm]]), get_col_var(native[[nm]]))
    testthat::expect_identical(is_totcol(tr[[nm]]),  is_totcol(native[[nm]]))
    testthat::expect_identical(is_refcol(tr[[nm]]),  is_refcol(native[[nm]]))
    testthat::expect_identical(tabxplor:::get_row_kind(tr[[nm]]),
                               tabxplor:::get_row_kind(native[[nm]]))
  }
  # rendered markdown is identical
  testthat::expect_identical(tab_md(tr, print = FALSE), tab_md(native, print = FALSE))
})



# === SECTION: RENDER-LEVEL transpose = TRUE (Phase 14o -- finding 8) ==========

# the transposed render model for one table (post-materialise, post-flip)
tx_prep <- function(t, backend = "kable", color = TRUE) {
  compute <- if (color) c("refs", "colors", "bold") else c("refs", "bold")
  tab_export_prep(t, backend = backend, transpose = TRUE, compute = compute)$tables[[1]]
}


# the untransposed reference must materialise the SAME way the transposed flip does (xl-style: `n` a
# column), so the slot grids line up cell-for-cell. backend = "xl" does that -- but it ALSO splits
# every composite's aside into a column of its own, which a transpose does not (the flipped cell
# keeps its bracket), so those columns are dropped below by their declared role.
plain_prep <- function(t) {
  tab_export_prep(t, backend = "xl", compute = c("refs", "colors", "bold"))$tables[[1]]
}


drop_asides <- function(rdu, i) i[!vapply(rdu$tab[i], function(c)
  identical(tabxplor:::get_role(c), "aside"), logical(1))]



testthat::test_that("numeric cells keep their OWN colour on transpose (the finding-8 regression)", {
  # A multi-row_var table with a numeric (mean) col_var. The object-level flip stamped one factor
  # column's colour onto every transposed column, so the mean cells were coloured by a diff scale.
  # The render-level flip computes each cell's slot on its source column, so slots match cell-for-cell.
  t   <- tab(gss, c(marital, race), c(relig, tvhours), pct = "row", color = TRUE, na = "drop")
  rd  <- tx_prep(t)
  rdu <- plain_prep(t)

  # untransposed slot matrix [orig row, orig data col] (drop the Excel aside columns -- none flipped)
  data_i <- drop_asides(rdu, unname(rdu$roles$fmt_cols))
  onm    <- names(rdu$tab)
  U <- vapply(data_i, function(j) rdu$ann[[onm[j]]]$text_slot, integer(nrow(rdu$tab)))
  # transposed: ann keyed by data-column name; recompute the row order to map d -> new row
  cvm     <- rdu$roles$col_var_map
  is_tot  <- data_i %in% rdu$roles$totcols
  is_n    <- unname(cvm[data_i]) %in% "all_col_vars"
  types   <- vapply(data_i, function(j) tabxplor:::fmt_var_kind(rdu$tab[[j]]), character(1))
  is_mean <- types %in% "mean" & !is_tot & !is_n
  is_fac  <- !is_tot & !is_n & !is_mean
  order_i <- c(data_i[is_fac], data_i[is_tot], data_i[is_n], data_i[is_mean])
  new_row_of <- match(data_i, order_i)
  dnames  <- names(rd$ann)
  Tm <- vapply(dnames, function(nm) rd$ann[[nm]]$text_slot, integer(nrow(rd$tab)))

  mism <- 0L
  for (dc in seq_along(data_i)) for (r in seq_len(nrow(U)))
    if (U[r, dc] != Tm[new_row_of[dc], r]) mism <- mism + 1L
  testthat::expect_identical(mism, 0L)
})



testthat::test_that("transpose = TRUE aborts on a REAL tab_vars table", {
  testthat::expect_error(
    tab_md(tab(gss, marital, race, year, pct = "row"), transpose = TRUE, print = FALSE),
    "tab_vars")
})



testthat::test_that("every exporter accepts transpose = TRUE without error", {
  t <- tab(gss, c(marital, race), c(relig, tvhours), pct = "row", color = TRUE, na = "drop")
  testthat::expect_no_error(tab_md(t, transpose = TRUE, print = FALSE))
  testthat::expect_no_error(tab_kable(t, transpose = TRUE))
  testthat::expect_no_error(tab_xl(t, path = withr::local_tempfile(fileext = ".xlsx"),
                                   transpose = TRUE, replace = TRUE, open = FALSE))
})
