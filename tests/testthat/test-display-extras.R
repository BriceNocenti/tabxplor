# PURPOSE: Phase 10i-B -- add_n / add_pct / p-value rows are DISPLAY-only. The built tab() is the
#          "core" table (no `n` / `col_pct` column, no p-value rows) carrying the intent (the
#          `render_extras` attribute + the kept `test` attribute); tab_materialize_extras() re-creates
#          the extras at display, byte-identically to the pre-migration built table.
# See: CLAUDE.md Phase 10i-B ; dev/tabxplor_1.4.0_decisions.md §34.

gss <- forcats::gss_cat

# --- the built "core" table carries the intent, not the extras --------------------------------
testthat::test_that("built tab() is the core table: no n/col_pct column, no p-value rows, intent kept", {
  t <- tab(gss, marital, race, pct = "row", add_n = TRUE, test = TRUE)
  testthat::expect_false("n" %in% names(t))
  testthat::expect_false("col_pct" %in% names(t))
  testthat::expect_identical(get_render_extras(t), list(add_n = TRUE, add_pct = FALSE))
  testthat::expect_false(is.null(get_test(t)))                 # test attribute KEPT (was dropped pre-10i-B)
  rv <- tab_get_vars(t)$row_var
  testthat::expect_false(any(as.character(t[[rv]]) == "pvalue"))
})

# --- render_extras survives dplyr verbs (carried like subtext/test) ----------------------------
testthat::test_that("render_extras is carried through dplyr verbs", {
  t  <- tab(gss, marital, race, pct = "row", add_n = TRUE, add_pct = TRUE)
  re <- list(add_n = TRUE, add_pct = TRUE)
  testthat::expect_identical(get_render_extras(dplyr::filter(t, TRUE)), re)
  testthat::expect_identical(get_render_extras(dplyr::arrange(t, Total)), re)
  testthat::expect_identical(get_render_extras(dplyr::mutate(t, .z = 1L)), re)
  testthat::expect_identical(get_render_extras(dplyr::select(t, marital, Total)), re)
  tg <- tab(gss, marital, race, tab_vars = year, pct = "row", add_n = TRUE, output_list = TRUE)[[1]]
  testthat::expect_identical(get_render_extras(dplyr::slice(tg, 1)), list(add_n = TRUE, add_pct = FALSE))
  testthat::expect_identical(get_render_extras(dplyr::ungroup(tg)), list(add_n = TRUE, add_pct = FALSE))
})

# --- materialiser (xl backend) reproduces the extras as real columns/rows ----------------------
testthat::test_that("tab_materialize_extras('xl') re-creates the add_n `n` column", {
  t   <- tab(gss, marital, race, pct = "row", add_n = TRUE)
  hyd <- tabxplor:::tab_materialize_extras(t, backend = "xl", pvalue = FALSE)
  testthat::expect_true("n" %in% names(hyd))
  testthat::expect_identical(get_type(hyd$n), "n")
  testthat::expect_equal(get_n(hyd$n), get_n(t$Total))          # the base counts of the Total column
})

testthat::test_that("materialiser is idempotent (clears render_extras after consuming)", {
  t   <- tab(gss, marital, race, pct = "row", add_n = TRUE)
  h1  <- tabxplor:::tab_materialize_extras(t, backend = "xl", pvalue = FALSE)
  h2  <- tabxplor:::tab_materialize_extras(h1, backend = "xl", pvalue = FALSE)   # no-op
  testthat::expect_identical(names(h1), names(h2))
  testthat::expect_null(get_render_extras(h1))
})

# --- text backend folds add_n IN-CELL on the Total column (no separate `n` column) -------------
testthat::test_that("tab_materialize_extras('text') folds add_n into the Total cell", {
  t   <- tab(gss, marital, race, pct = "row", add_n = TRUE)
  hyd <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  testthat::expect_false("n" %in% names(hyd))
  # the Total cell shows the {pct} (n={n}) composite
  testthat::expect_match(format(hyd$Total)[1], "\\(n=", perl = TRUE)
})

# --- transpose carries the intent: transpose(row% add_n) renders like a native col% add_n ------
testthat::test_that("tab_transpose carries render_extras (transpose == native col%)", {
  tr     <- tab_transpose(tab(gss, marital, race, pct = "row", add_n = TRUE))
  native <- tab(gss, race, marital, pct = "col", add_n = TRUE)
  testthat::expect_identical(get_render_extras(tr), get_render_extras(native))
  testthat::expect_identical(tab_md(tr, print = FALSE), tab_md(native, print = FALSE))
})

# --- back-compat shim: $n / [[ / pull reconstruct the deprecated column ------------------------
testthat::test_that("$n / [[ / pull reconstruct the display-only add_n column with a deprecation", {
  t  <- tab(gss, marital, race, pct = "row", add_n = TRUE)
  xl <- tabxplor:::tab_materialize_extras(t, backend = "xl", pvalue = FALSE)$n

  testthat::expect_warning(n1 <- t$n, class = "lifecycle_warning_deprecated")
  testthat::expect_identical(n1, xl)
  testthat::expect_identical(suppressWarnings(t[["n"]]),         xl)
  testthat::expect_identical(suppressWarnings(dplyr::pull(t, n)), xl)
  testthat::expect_identical(suppressWarnings(dplyr::pull(t, "n")), xl)

  # fast path: an existing column is returned with NO deprecation warning
  testthat::expect_no_warning(tot <- t$Total)
  testthat::expect_true(is_fmt(tot))
  # add_n = FALSE -> never had an `n` column -> NULL (no reconstruction)
  testthat::expect_null(suppressWarnings(tab(gss, marital, race, pct = "row", add_n = FALSE)$n))
  # a genuinely unknown column -> NULL (base tbl_df behaviour)
  testthat::expect_null(suppressWarnings(t$zzz_unknown))
  # pct = "col": add_n was a ROW, so `$n` must NOT reconstruct a column
  testthat::expect_null(suppressWarnings(tab(gss, marital, race, pct = "col", add_n = TRUE)$n))
})

# --- pull() of a normal (existing) column is untouched by the shim -----------------------------
testthat::test_that("pull() of an existing column keeps tidy-select NSE (shim does not break it)", {
  tabs <- tab(gss, race, c(age, tvhours), comp = "all")
  testthat::expect_true(is_fmt(dplyr::pull(dplyr::filter(tabs, race == "White"), tvhours)))
  testthat::expect_true(is_fmt(dplyr::pull(tabs, age)))
})


# --- Phase 14a: the pct = "col" add_n / add_pct ROW on a merged multi-row_var table --------------
# `last_totrow` is a GLOBAL index (is_totrow.data.frame is not group-aware), but a merged
# multi-row_var tab is a grouped_df where dplyr::slice() indexes WITHIN each group -- no group had
# that many rows, so slice() returned 0 rows and bind_rows() silently dropped the extra.

row_labels <- function(tt) {
  m <- tabxplor:::tab_materialize_extras(tt, backend = "text", pvalue = FALSE)
  as.character(m[[tab_get_vars(m)$row_var]])
}

testthat::test_that("pct = 'col' add_n adds one n row per sub-table, whatever the row_var count", {
  g <- forcats::gss_cat
  testthat::expect_equal(sum(row_labels(tab(g, race, marital, pct = "col")) == "n"), 1L)
  testthat::expect_equal(sum(row_labels(tab(g, race, c(marital, relig), pct = "col")) == "n"), 1L)
  # the regression: 2+ row_vars used to lose the row entirely
  testthat::expect_equal(sum(row_labels(tab(g, c(race, marital), relig, pct = "col")) == "n"), 2L)
  testthat::expect_equal(
    sum(row_labels(tab(g, c(race, marital), c(relig, partyid), pct = "col")) == "n"), 2L)
  testthat::expect_equal(
    sum(row_labels(tab(g, c(race, marital, partyid), relig, pct = "col")) == "n"), 3L)
})

testthat::test_that("each n row sits in its OWN sub-table, right after that sub-table's Total", {
  m <- tabxplor:::tab_materialize_extras(
    tab(forcats::gss_cat, c(race, marital), relig, pct = "col"),
    backend = "text", pvalue = FALSE)
  lv <- as.character(m$levels)
  # the n row directly follows its Total row, and stays inside its group
  testthat::expect_equal(lv[which(lv == "n") - 1L], c("Total", "Total"))
  testthat::expect_equal(as.character(m$row_var)[lv == "n"], c("race", "marital"))
})

testthat::test_that("add_pct keeps the historical Total | row_pct | n order", {
  lv <- row_labels(tab(forcats::gss_cat, c(race, marital), relig, pct = "col", add_pct = TRUE))
  i  <- which(lv == "Total")[1]
  testthat::expect_equal(lv[i:(i + 2L)], c("Total", "row_pct", "n"))
})

testthat::test_that("the n row carries the column's real unweighted base", {
  m <- tabxplor:::tab_materialize_extras(
    tab(forcats::gss_cat, c(race, marital), relig, pct = "col"),
    backend = "text", pvalue = FALSE)
  lv <- as.character(m$levels)
  testthat::expect_equal(get_num(m[["Protestant"]])[lv == "n"],
                         get_n(m[["Protestant"]])[lv == "Total"])
})
