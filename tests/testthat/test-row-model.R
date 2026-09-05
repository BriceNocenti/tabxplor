# Phase 19f (KEY 1) -- the row model: the `row_kind` field and the declared `tabxplor_lvl` index.
# Each block fails without the change it names.

gss <- fx_gss()

# --- the shared half: row_kind replaces in_totrow -----------------------------------------------

testthat::test_that("row_kind is a real field, and is_totrow() is derived from it", {
  t <- tab(gss, marital, race, pct = "row")
  k <- tabxplor:::get_row_kind(t$Black)
  testthat::expect_true(all(k %in% names(tabxplor:::ROW_KINDS)))
  testthat::expect_identical(k == "total", tabxplor:::is_totrow(t$Black))
  testthat::expect_identical(t$Black$in_totrow, tabxplor:::is_totrow(t$Black))  # the $ read alias
})

testthat::test_that("the synthetic display rows carry their kind IN the record", {
  # Before 19f these were a positional vector created at render and living one render pass, so any
  # consumer outside that pass had to match English row labels.
  m <- tabxplor:::tab_materialize_extras(
    tab(gss, race, relig, pct = "col", n = "range", add_pct = TRUE, test = TRUE),
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

testthat::test_that("tab_vars and several row_vars compose, row_var-major", {
  g <- dplyr::filter(gss, year %in% c(2000, 2014))
  t <- tab(g, c(marital, relig), race, tab_vars = year, pct = "row")
  testthat::expect_s3_class(t, "tabxplor_tab")          # was a LIST of tables
  testthat::expect_false(is.null(tabxplor:::tab_declared_vars(t)))
  v <- tabxplor:::tab_declared_vars(t)
  testthat::expect_equal(v$tab_vars, "year")
  testthat::expect_equal(v$row_vars, c("marital", "relig"))
  # THE ROW_VAR IS THE OUTER AXIS: two row_vars are two tables over the same population, the tab_vars
  # are the sub-populations INSIDE each. Row order is column order (tab_label_order reads position).
  testthat::expect_equal(dplyr::group_vars(t), c("row_var", "year"))
  testthat::expect_equal(names(t)[1:3], c("row_var", "year", "levels"))
  rv <- as.character(t$row_var)                   # each VARIABLE is ONE contiguous run...
  testthat::expect_equal(rle(rv)$values, unique(rv))
  yr <- as.character(t$year)                      # ...and every year repeats inside each of them
  testthat::expect_gt(length(rle(yr)$values), dplyr::n_distinct(yr))
  testthat::expect_silent(tab_md(t, print = FALSE))
})


# --- Phase 20g-ii: the declared LEVEL COLLAPSE spec ----------------------------------------
# The row model owns the SPEC; tab_collapse_levels() (R/tab.R) applies it pre-aggregate. What is
# checked here is the validation -- each refusal exists because the alternative is silently wrong.

testthat::test_that("new_lvl_collapse(): the canonical shape, and an empty label defaults", {
  s <- tabxplor:::new_lvl_collapse(
    list(marital = list(`Not married` = c("Divorced", "Separated"),
                        c("Married", "Widowed"))))                 # no label
  testthat::expect_identical(names(s), "marital")
  testthat::expect_identical(s$marital[["Not married"]], c("Divorced", "Separated"))
  testthat::expect_identical(s$marital[["Married, Widowed"]], c("Married", "Widowed"))
  testthat::expect_null(tabxplor:::new_lvl_collapse(NULL))
  testthat::expect_null(tabxplor:::new_lvl_collapse(list()))
  # a collapse never RENAMES: a group down to one level is dropped, not applied
  testthat::expect_null(tabxplor:::new_lvl_collapse(list(marital = list(x = "Married"))))
})

testthat::test_that("new_lvl_collapse(): the two refusals", {
  # one level in two groups -- fct_collapse() would silently give it to the LAST
  testthat::expect_error(
    tabxplor:::new_lvl_collapse(list(m = list(a = c("x", "y"), b = c("y", "z")))),
    "more than one merged group")
  # a merged label colliding with a label tab() mints itself
  testthat::expect_error(
    tabxplor:::new_lvl_collapse(list(m = list(Total = c("x", "y")))), "cannot name a merged level")
  testthat::expect_error(
    tabxplor:::new_lvl_collapse(list(m = list(Others = c("x", "y")))), "cannot name a merged level")
})

testthat::test_that("tab_collapse_levels(): drift is tolerated, `ordered` survives", {
  d <- data.frame(f = factor(c("a", "b", "c", "a"), levels = c("a", "b", "c")),
                  o = factor(c("a", "b", "c", "a"), levels = c("a", "b", "c"), ordered = TRUE),
                  n = 1:4)
  s <- tabxplor:::new_lvl_collapse(list(f = list(bc = c("b", "c", "gone")),
                                        o = list(bc = c("b", "c")),
                                        n = list(x = c("1", "2"))))
  out <- tabxplor:::tab_collapse_levels(d, s)
  testthat::expect_identical(levels(out$f), c("a", "bc"))     # the absent level is filtered out
  testthat::expect_true(is.ordered(out$o))                     # Phase 18z10's ordered survival
  testthat::expect_identical(levels(out$o), c("a", "bc"))      # merged AT the first constituent
  testthat::expect_identical(out$n, 1:4)                       # a numeric column is left alone
  # a group whose levels have ALL drifted away is a no-op, never an error
  testthat::expect_identical(
    tabxplor:::tab_collapse_levels(d, list(f = list(z = c("q", "r"))))$f, d$f)
})

testthat::test_that("tab(.levels_collapse=) is tab() on pre-collapsed data, and merges before lumping", {
  sp <- list(marital = list(`Not married` = c("Never married", "Divorced", "Separated")))
  pre <- dplyr::mutate(gss, marital = forcats::fct_collapse(
    marital, `Not married` = c("Never married", "Divorced", "Separated")))
  testthat::expect_equal(tab(gss, marital, race, pct = "row", test = TRUE, .levels_collapse = sp),
                         tab(pre, marital, race, pct = "row", test = TRUE))
  # merge-then-lump: the merged level's COMBINED count (9 542) clears a threshold its parts do not
  testthat::expect_equal(
    tab(gss, marital, race, pct = "row", other_if_less_than = 2000L, .levels_collapse = sp),
    tab(pre, marital, race, pct = "row", other_if_less_than = 2000L))
  testthat::expect_true("Not married" %in% levels(
    tab(gss, marital, race, pct = "row", other_if_less_than = 2000L, .levels_collapse = sp)$marital))
})


# --- the grid: a kind says how its rows READ ----------------------------------------------------

testthat::test_that("ROW_KINDS is a grid, and its order is still the tie-break", {
  # the order IS fmt_row_kind()'s "first non-data wins" tie-break, so it is part of the contract.
  testthat::expect_identical(names(tabxplor:::ROW_KINDS),
                             c("data", "total", "n", "pct", "pvalue", "gof", "blank"))
  testthat::expect_true(all(purrr::map_lgl(
    tabxplor:::ROW_KINDS, ~ is.logical(.x$graded) && length(.x$graded) == 1L &&
      is.character(.x$doc) && nzchar(.x$doc))))
})

testthat::test_that("row_kind_graded() reads the grid, and folds NA and the unknown to `data`", {
  testthat::expect_identical(
    tabxplor:::row_kind_graded(names(tabxplor:::ROW_KINDS)),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  # NA-filled cells are what dplyr::bind_rows() leaves behind, exactly as is_totrow() folds them.
  testthat::expect_identical(tabxplor:::row_kind_graded(c(NA_character_, "nope")), c(TRUE, TRUE))
  testthat::expect_length(tabxplor:::row_kind_graded(character(0)), 0L)
})

testthat::test_that("fmt_row_look() is the anchor rule: a reference cell, or an ungraded row", {
  t <- tab(gss, marital, race, pct = "row", color = "diff")
  lk <- tabxplor:::fmt_row_look(t$Black)
  testthat::expect_true(all(lk$graded))                       # data + total are graded
  testthat::expect_identical(lk$anchor, tabxplor:::get_reference(t$Black, mode = "all_totals") |
                                          tabxplor:::is_refrow(t$Black))
  # an ungraded row is an anchor whatever the reference says
  col <- tabxplor:::set_row_kind(t$Black, c(rep("data", length(t$Black) - 1L), "n"))
  testthat::expect_false(tabxplor:::fmt_row_look(col)$graded[[length(col)]])
  testthat::expect_true(tabxplor:::fmt_row_look(col)$anchor[[length(col)]])
})
