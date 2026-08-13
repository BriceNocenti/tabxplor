# Phase 19f (KEY 1) -- the row model: the `row_kind` field and the declared `tabxplor_lvl` index.
# Each block fails without the change it names.

gss <- forcats::gss_cat

# --- the shared half: row_kind replaces in_totrow -----------------------------------------------

testthat::test_that("row_kind is a real field, and is_totrow() is derived from it", {
  t <- tab(gss, marital, race, pct = "row")
  k <- tabxplor:::get_row_kind(t$Black)
  testthat::expect_true(all(k %in% tabxplor:::ROW_KINDS))
  testthat::expect_identical(k == "total", tabxplor:::is_totrow(t$Black))
  testthat::expect_identical(t$Black$in_totrow, tabxplor:::is_totrow(t$Black))  # the $ read alias
})

testthat::test_that("the synthetic display rows carry their kind IN the record", {
  # Before 19f these were a positional vector created at render and living one render pass, so any
  # consumer outside that pass had to match English row labels.
  m <- tabxplor:::tab_materialize_extras(
    tab(gss, race, relig, pct = "col", add_n = TRUE, add_pct = TRUE, test = TRUE),
    backend = "text", pvalue = TRUE)
  rr <- tabxplor:::fmt_row_kind(m)
  testthat::expect_length(rr, nrow(m))
  testthat::expect_true(all(c("total", "n", "pct", "pvalue") %in% rr))
  # and it survives an ordinary dplyr slice, which a parallel vector could not
  testthat::expect_identical(tabxplor:::fmt_row_kind(m[rr != "data", ]), rr[rr != "data"])
})

# --- the declared index columns ------------------------------------------------------------------

testthat::test_that("every producer declares its index columns", {
  shapes <- list(
    single  = tab(gss, marital, race, pct = "row"),
    merged  = tab(gss, c(marital, relig), race, pct = "row"),
    tabbed  = tab(gss, marital, race, tab_vars = year, pct = "row"),
    numeric = tab_num(gss, race, age)
  )
  for (nm in names(shapes)) {
    v <- tabxplor:::tab_declared_vars(shapes[[nm]])
    testthat::expect_false(is.null(v), info = nm)
    testthat::expect_length(v$row_var, 1L)
  }
  testthat::expect_equal(tabxplor:::tab_declared_vars(shapes$merged)$row_vars,
                         c("marital", "relig"))
  testthat::expect_equal(tabxplor:::tab_declared_vars(shapes$tabbed)$tab_vars, "year")
  # tab_num() recorded NO variable model at all before 19f
  testthat::expect_equal(tabxplor:::tab_declared_vars(shapes$numeric)$row_var, "race")
})

testthat::test_that("a regression declares its predictor as a `var`, not a fake sub-table", {
  d <- gss[1:3000, ]; d$y <- d$marital == "Married"
  r <- tab_reg(d, "y", c("race", "age"))
  v <- tabxplor:::tab_declared_vars(r)
  testthat::expect_equal(v$var_col, "var")
  testthat::expect_length(v$tab_vars, 0L)          # was tab_vars = "var" -- the pun
  testthat::expect_equal(v$row_var, "levels")
})

testthat::test_that("the declaration survives every dplyr verb (15/15 with ~4 methods)", {
  t <- tab(gss, marital, race, pct = "row")
  verbs <- list(
    `[`       = function(x) x[1:3, ],
    filter    = function(x) dplyr::filter(x, TRUE),
    arrange   = function(x) dplyr::arrange(x, marital),
    mutate    = function(x) dplyr::mutate(x, zz = 1),
    slice     = function(x) dplyr::slice(x, 1:2),
    group_by  = function(x) dplyr::ungroup(dplyr::group_by(x, marital)),
    bind_rows = function(x) dplyr::bind_rows(x, x),
    fct_drop  = function(x) dplyr::mutate(x, marital = forcats::fct_drop(marital))
  )
  for (nm in names(verbs)) {
    out <- verbs[[nm]](t)
    testthat::expect_true(tabxplor:::is_lvl(out$marital), info = nm)
    testthat::expect_equal(tabxplor:::lvl_var(out$marital), "marital", info = nm)
  }
  testthat::expect_true(is.factor(t$marital))   # it IS a factor: no is.factor migration
})

testthat::test_that("degraded mode: stripping the declaration falls back, and says nothing wrong", {
  t <- tab(gss, marital, race, pct = "row")
  t$marital <- factor(as.character(t$marital), levels = levels(t$marital))
  testthat::expect_null(tabxplor:::tab_declared_vars(t))
  testthat::expect_equal(tab_get_vars(t)$row_var, "marital")   # the heuristic, as fallback only
  testthat::expect_silent(format(t$Black))
})

# --- what the declaration unlocks ----------------------------------------------------------------

testthat::test_that("a merged table remembers which of its variables were ordinal", {
  g <- gss
  g$ord <- factor(g$race, ordered = TRUE)
  m <- tab(g, c(ord, marital), relig, pct = "row")
  testthat::expect_equal(tabxplor:::lvl_ordered(m$levels)[["ord"]], TRUE)
  testthat::expect_false(tabxplor:::lvl_ordered(m$levels)[["marital"]])
  # the display column itself must stay PLAIN (two variables' levels, no order across them)
  testthat::expect_false(is.ordered(m$levels))
})

testthat::test_that("tab_vars and several row_vars finally compose (the list fallback is gone)", {
  g <- dplyr::filter(gss, year %in% c(2000, 2014))
  t <- tab(g, c(marital, relig), race, tab_vars = year, pct = "row")
  testthat::expect_s3_class(t, "tabxplor_tab")          # was a LIST of tables
  testthat::expect_false(is.null(tabxplor:::tab_declared_vars(t)))
  v <- tabxplor:::tab_declared_vars(t)
  testthat::expect_equal(v$tab_vars, "year")
  testthat::expect_equal(v$row_vars, c("marital", "relig"))
  # sub-table axis is the OUTER one: every row_var block sits inside one year
  testthat::expect_equal(dplyr::group_vars(t), c("year", "row_var"))
  yr <- as.character(t$year)                      # each year (+ the total table) is ONE contiguous run
  testthat::expect_equal(rle(yr)$values, unique(yr))
  testthat::expect_silent(tab_md(t, print = FALSE))
})
