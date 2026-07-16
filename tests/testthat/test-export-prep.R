# PURPOSE: Lock the Phase 10d shared exporter-prep (tab_export_prep + the render-model + ann +
#          tab_check_same_col_vars + tab_bold_rows + tab_totcol_range). The byte-identity of the
#          kable/md/plot OUTPUT is covered by test-golden.R / test-color-golden.R / test-tab_md.R;
#          this file locks the prep's INTERNAL derive-once quantities and the degrade / base-list split.
# ROLE: Phase 10d.

gss <- forcats::gss_cat

t_basic <- tab(gss, race, marital, pct = "row", color = "diff", test = TRUE)
t_multi <- tab(gss, race, c(marital, relig), pct = "row", color = "diff")
t_tv    <- tab(gss, race, marital, year, pct = "row", color = "diff")

# === SECTION: render-model shape =============================================

testthat::test_that("tab_export_prep returns a tabxplor_render with tables/labels/meta", {
  p <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)
  testthat::expect_s3_class(p, "tabxplor_render")
  testthat::expect_named(p, c("tables", "labels", "meta"))
  testthat::expect_length(p$tables, 1L)
  rd <- p$tables[[1]]
  testthat::expect_named(rd, c("tab", "vars", "roles", "ann", "bold_rows",
                               "bold_cols", "range_totcol", "col_var_header", "subtext"))
  testthat::expect_false(rd$vars$degrade)
})

testthat::test_that("vars are detected correctly", {
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_identical(rd$vars$row_var, "race")
  testthat::expect_true("marital" %in% rd$vars$col_vars)  # also carries "all_col_vars" (the Total)
  testthat::expect_length(rd$vars$tab_vars, 0L)
})

testthat::test_that("roles: fmt_cols / totcols / row_var_col match the built table", {
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  tab <- rd$tab
  testthat::expect_identical(rd$roles$fmt_cols, which(purrr::map_lgl(tab, is_fmt)))
  testthat::expect_identical(rd$roles$totcols, which(is_totcol(tab)))
  testthat::expect_identical(rd$roles$totrows, which(is_totrow(tab)))
  testthat::expect_identical(unname(rd$roles$row_var_col), which(names(tab) == "race"))
})

# === SECTION: ann + colours gated by compute =================================

testthat::test_that("ann shape is uniform; compute without 'colors' yields a monochrome column", {
  # Phase 10j: fmt_col_ann() ALWAYS returns the full structure, so every backend reads a consistent
  # shape. `want_colors = FALSE` (compute without "colors", i.e. a color = FALSE export) does not drop
  # fields -- it forces a MONOCHROME column: no colour slots, no colour flag.
  cols <- c("ref_alltot", "ref_cells", "font", "back", "bold", "text_slot", "bg_slot")

  rk <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  a1 <- rk$ann[[1]]
  testthat::expect_true(all(cols %in% names(a1)))
  testthat::expect_length(a1$font, nrow(rk$tab))

  rm <- tabxplor:::tab_export_prep(t_basic, backend = "md", drop_tab_vars = FALSE,
                                   wrap = NULL, compute = c("refs", "bold"))$tables[[1]]
  am <- rm$ann[[1]]
  testthat::expect_true(all(cols %in% names(am)))
  testthat::expect_false(am$has_color)
  testthat::expect_true(all(am$text_slot == 0L))
  testthat::expect_true(all(am$bg_slot == 0L))
})

testthat::test_that("bold_rows flags the reference/total row(s), reused by ann$ref_alltot", {
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  # a row is bold iff it is a reference/total cell in EVERY discriminating column
  refref <- as.data.frame(purrr::map(rd$ann, "ref_alltot"))
  keep   <- purrr::map_lgl(refref, ~ any(.) & !all(.))
  refref <- refref[, keep, drop = FALSE]
  expected <- which(rowSums(refref) == ncol(refref))
  testthat::expect_identical(rd$bold_rows, expected)
  testthat::expect_gt(length(rd$bold_rows), 0L)  # there is always a total row
})

# === SECTION: graceful degrade ===============================================

testthat::test_that("degrade path flags non-tabxplor inputs with a reason", {
  for (obj in list(iris, tibble::tibble(a = 1:3), datasets::mtcars)) {
    rd <- tabxplor:::tab_export_prep(obj, backend = "md", wrap = NULL)$tables[[1]]
    testthat::expect_true(isTRUE(rd$vars$degrade))
    testthat::expect_type(rd$vars$reason, "character")
  }
})

# === SECTION: tab_check_same_col_vars (block A) ==============================

testthat::test_that("tab_check_same_col_vars accepts same col_vars, rejects mismatch / tab_vars", {
  ok <- list(tab(gss, race, marital, pct = "row"),
             tab(gss, relig, marital, pct = "row"))
  testthat::expect_identical(tabxplor:::tab_check_same_col_vars(ok), "marital")

  bad <- list(tab(gss, race, marital, pct = "row"),
              tab(gss, race, relig, pct = "row"))
  testthat::expect_error(tabxplor:::tab_check_same_col_vars(bad), "same col_vars")

  withtv <- list(tab(gss, race, marital, year, pct = "row"))
  testthat::expect_error(tabxplor:::tab_check_same_col_vars(withtv), "no tab_vars")
})

# === SECTION: base vs list split =============================================

testthat::test_that("a list with matching col_vars compacts to ONE render table", {
  lst <- list(tab(gss, race, marital, pct = "row", color = "diff"),
              tab(gss, relig, marital, pct = "row", color = "diff"))
  p <- tabxplor:::tab_export_prep(lst, backend = "md", drop_tab_vars = FALSE, wrap = NULL)
  testthat::expect_length(p$tables, 1L)
  testthat::expect_false(p$tables[[1]]$vars$degrade)
})

testthat::test_that("tab_list_mergeable: same col_vars + no tab_vars only", {
  same <- list(tab(gss, race, marital, pct = "row"), tab(gss, relig, marital, pct = "row"))
  testthat::expect_true(tabxplor:::tab_list_mergeable(same))
  diffcv <- list(tab(gss, race, marital, pct = "row"), tab(gss, race, relig, pct = "row"))
  testthat::expect_false(tabxplor:::tab_list_mergeable(diffcv))
  withtv <- list(tab(gss, race, marital, year, pct = "row"))
  testthat::expect_false(tabxplor:::tab_list_mergeable(withtv))
})

testthat::test_that("list_method keeps a non-mergeable list as N tables; else it errors", {
  tv_list <- list(tab(gss, race, marital, year, pct = "row", color = "diff"),
                  tab(gss, relig, marital, year, pct = "row", color = "diff"))
  # list_method = TRUE (tab_md) -> one render table per input, each not degraded
  p <- tabxplor:::tab_export_prep(tv_list, backend = "md", drop_tab_vars = FALSE,
                                  wrap = NULL, list_method = TRUE)
  testthat::expect_length(p$tables, 2L)
  testthat::expect_false(p$tables[[1]]$vars$degrade)
  testthat::expect_false(p$tables[[2]]$vars$degrade)
  # list_method = FALSE (tab_kable / tab_plot) -> historical error
  testthat::expect_error(
    tabxplor:::tab_export_prep(tv_list, backend = "kable", wrap = NULL, list_method = FALSE),
    "no tab_vars"
  )
})

# === SECTION: tab_bold_rows edge (md vs kable style) =========================

testthat::test_that("tab_bold_rows: no discriminating column -> md integer(0), kable all rows", {
  # all-FALSE (no reference) columns -> not discriminating
  none <- list(c(FALSE, FALSE, FALSE), c(FALSE, FALSE, FALSE))
  testthat::expect_identical(tabxplor:::tab_bold_rows(none, md_style = TRUE), integer(0))
  testthat::expect_identical(tabxplor:::tab_bold_rows(none, md_style = FALSE), 1:3)
  # a normal discriminating column -> both agree
  disc <- list(c(FALSE, FALSE, TRUE), c(FALSE, FALSE, TRUE))
  testthat::expect_identical(tabxplor:::tab_bold_rows(disc, md_style = TRUE), 3L)
  testthat::expect_identical(tabxplor:::tab_bold_rows(disc, md_style = FALSE), 3L)
})

# === SECTION: tab_totcol_range (block B, inert in Part 1) ====================

testthat::test_that("tab_totcol_range is scalar when col_var bases are equal (na='keep')", {
  rd <- tabxplor:::tab_export_prep(t_multi, backend = "kable", wrap = NULL)$tables[[1]]
  rng <- rd$range_totcol
  testthat::expect_named(rng, c("col", "text", "differ"))
  testthat::expect_length(rng$text, nrow(rd$tab))
  testthat::expect_false(any(rng$differ))  # na='keep' -> every col_var's base = full N
})

testthat::test_that("tab_totcol_range yields [min;max] when bases differ (na='drop')", {
  set.seed(1)
  d <- tibble::tibble(
    g  = factor(sample(c("A", "B"), 400, TRUE)),
    q1 = factor(sample(c("yes", "no"), 400, TRUE)),
    q2 = factor(sample(c("yes", "no", NA), 400, TRUE, prob = c(0.4, 0.4, 0.2)))
  )
  tt <- tab(d, g, c(q1, q2), pct = "row", na = "drop")
  rd <- tabxplor:::tab_export_prep(tt, backend = "kable", wrap = NULL)$tables[[1]]
  rng <- rd$range_totcol
  testthat::expect_true(any(rng$differ))
  testthat::expect_true(any(grepl("^\\[.*;.*\\]$", rng$text[rng$differ])))
})
