# PURPOSE: KEY 3 (Phase 20a) -- the generic column-attribute accessor and the column inspector.
# ROLE: the accessor family stops growing with `fmt_col_attrs`. These tests hold the two properties
#       that make that safe: fmt_attr() reaches EVERY declared attribute (so a 17th needs no new
#       export), and it writes through the attribute's own setter (so it cannot become a second,
#       laxer way to write one).
# KEY CONSTRAINTS:
#   - fmt_attr() is the RAW read: the stored value with the declared neutral default. The named
#     getters may resolve further (get_conf_level() falls back to the option) -- that is deliberate.
#   - `color` is the one non-scalar attribute (text + background channels).

testthat::test_that("fmt_attr() reaches every declared attribute", {
  x <- fmt(n = c(10, 20), pct = c(0.3, 0.7), scale = "level_pct", pct_base = "row")
  for (a in tabxplor:::fmt_col_attrs)
    testthat::expect_no_error(fmt_attr(x, a), message = paste("attribute", a))
  # ...and each one agrees with its own named accessor where there is one
  testthat::expect_identical(fmt_attr(x, "scale"),    get_scale(x))
  testthat::expect_identical(fmt_attr(x, "pct_base"), get_pct_base(x))
  testthat::expect_identical(fmt_attr(x, "col_var"),  get_col_var(x))
  testthat::expect_identical(fmt_attr(x, "totcol"),   is_totcol(x))
})

testthat::test_that("an unset attribute reads its declared neutral", {
  x <- fmt(n = 1)
  testthat::expect_identical(fmt_attr(x, "col_group"), "")
  testthat::expect_identical(fmt_attr(x, "conf_level"), NA_real_)
  testthat::expect_identical(fmt_attr(x, "role"), "")
})

testthat::test_that("fmt_attr<-() writes through the attribute's own setter", {
  x <- fmt(n = 1)
  fmt_attr(x, "col_var") <- "region"
  testthat::expect_identical(get_col_var(x), "region")
  # the validation is the setter's, not a second one: set_scale() checks EST_SCALE_KEYS
  testthat::expect_error(`fmt_attr<-`(x, "scale", "not_a_scale"))
  # every declared attribute has a writer -- the build-time assert beside fmt_attr_rules
  testthat::expect_true(all(vapply(tabxplor:::fmt_attr_rules,
                                   function(r) is.function(r$write), logical(1))))
})

testthat::test_that("an unknown attribute name aborts naming the set", {
  x <- fmt(n = 1)
  testthat::expect_error(fmt_attr(x, "colour"), "Unknown")
  testthat::expect_error(fmt_attr(x, "colour"), "col_var")
})

testthat::test_that("fmt_attr() on a data.frame reads every fmt column", {
  t <- tab(forcats::gss_cat, marital, race, pct = "row")
  cv <- fmt_attr(t, "col_var")
  testthat::expect_true(all(cv == "race"))
  testthat::expect_length(cv, sum(vapply(t, is_fmt, logical(1))))
  # ...and refuses to write there, pointing at the verb that does it
  testthat::expect_error(`fmt_attr<-`(t, "col_var", "x"), "one")
})

testthat::test_that("set_ref_type() replaces set_diff_type(), which still works", {
  x <- fmt(n = 1)
  x <- set_ref_type(x, "first")
  testthat::expect_identical(get_ref_type(x), "first")
  lifecycle::expect_deprecated(y <- set_diff_type(fmt(n = 1), "tot"))
  testthat::expect_identical(get_ref_type(y), "tot")
})

testthat::test_that("tab_columns() reports one row per fmt column with its stored facts", {
  t  <- tab(forcats::gss_cat, marital, race, pct = "row", ci = "ref", color = "diff")
  tc <- tab_columns(t)
  testthat::expect_s3_class(tc, "tbl_df")
  testthat::expect_equal(nrow(tc), sum(vapply(t, is_fmt, logical(1))))
  testthat::expect_true(all(c("column", "col_var", "scale", "conf_level", "degf", "basis",
                              "ci_method", "color", "color_bg") %in% names(tc)))
  testthat::expect_true(all(tc$col_var == "race"))
  testthat::expect_true(all(tc$color == "difference"))
  # the four inference facts, side by side -- the user story the getters could not answer
  testthat::expect_true(all(tc$conf_level == 0.95))
  testthat::expect_true(all(tc$basis == "n"))
  testthat::expect_true(any(tc$totcol))
  # a table with no fmt column is an empty answer, not an error
  testthat::expect_equal(nrow(tab_columns(data.frame(a = 1))), 0L)
  testthat::expect_error(tab_columns("not a table"))
})
