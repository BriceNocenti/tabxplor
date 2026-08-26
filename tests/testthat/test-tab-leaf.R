
# === SECTION: the leaf computes from the plan =====================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


gss <- fx_gss()

test_that("a computation step does not decide the table's SHAPE (comp = 'all')", {
  # tab_chi2() used to ungroup() the table it returned, so whether a comp = "all" table came back
  # GROUPED depended on whether a test happened to run -- and the jamovi tier-2 test cache, which
  # skips the step, therefore returned a different CLASS from a fresh build. Now the ungrouping is a
  # local view (leaf_test_view), so all four combinations agree.
  mk <- function(...) suppressWarnings(
    tab(gss, marital, race, tab_vars = year, pct = "row", comp = "all", ...))
  expect_s3_class(mk(),                          "tabxplor_grouped_tab")
  expect_s3_class(mk(test = TRUE),               "tabxplor_grouped_tab")
  expect_s3_class(mk(ci = "cell"),               "tabxplor_grouped_tab")
  expect_s3_class(mk(ci = "ref", test = TRUE),   "tabxplor_grouped_tab")
})

test_that("tab_plain() computes its own interval, and agrees with tab() cell for cell", {
  # `ci` is a real argument on the leaf now (it had none: the step chain was the only way to get a
  # factor cell interval), resolved by the SAME resolve_leaf_ci() the pipeline uses.
  for (v in c("cell", "ref")) {
    a <- tab_plain(gss, marital, race, pct = "row", tot = c("row", "col"), ci = v)
    b <- tab(gss, marital, race, pct = "row", ci = v)
    expect_equal(get_ci_inf(a$White), get_ci_inf(b$White), info = v)
    expect_equal(get_ci_sup(a$White), get_ci_sup(b$White), info = v)
  }
  # ... and the default is exactly the pre-19j behaviour: no contrast interval unless one is asked for
  d <- tab_plain(gss, marital, race, pct = "row", tot = c("row", "col"))
  expect_true(all(is.na(get_ci_inf(d$White))))
})

test_that("the leaf stamps the scale and the method the bounds were actually built with (D8)", {
  # both come from the ONE CI_GEOMS row that chose the engine -- so the column cannot estimate one
  # thing, name a second method and carry a third geometry's bounds.
  cell <- tab(gss, marital, race, pct = "row", ci = "cell")
  expect_equal(tabxplor:::get_ci_method(cell$White), "wilson")
  expect_equal(get_scale(cell$White), "level_pct")          # a level with its own interval is a level

  diff <- tab(gss, marital, race, pct = "row", ci = "ref")
  expect_equal(tabxplor:::get_ci_method(diff$White), "newcombe")
  expect_equal(get_scale(diff$White), "points")

  rat <- tab(gss, marital, race, pct = "row", ci = "ref", color = "ratio")
  expect_equal(tabxplor:::get_ci_method(rat$White), "katz")
  expect_equal(get_scale(rat$White), "pct_ratio")
})

test_that("ci_dispatch() is the ONE engine rule, shared by the leaves and the superseded step", {
  # Phase 20h: the step is HALF the subject (leaf vs step parity), so its call stays.
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  # the step must reach the same numbers as the build it supersedes
  built <- tab(gss, marital, race, pct = "row", ci = "cell")
  stepd <- tab_plain(gss, marital, race, pct = "row", tot = c("row", "col")) |> tab_ci(ci = "cell")
  expect_equal(get_ci_inf(built$White), get_ci_inf(stepd$White))
  expect_equal(tabxplor:::get_ci_method(built$White), tabxplor:::get_ci_method(stepd$White))

  # and CI_GEOMS answers about a geometry the same way whoever asks
  expect_equal(tabxplor:::ci_geom_scale("diff", "pct",  "ratio"), "pct_ratio")
  expect_equal(tabxplor:::ci_geom_scale("diff", "mean", "diff" ), "mean_diff")
  expect_true(is.na(tabxplor:::ci_geom_scale("cell", "mean")))    # a mean keeps its level scale
  expect_equal(tabxplor:::ci_geom_method("cell", "mean"), "student")
  expect_equal(tabxplor:::ci_geom_method("diff", "pct", "ratio"), "katz")
  expect_null(tabxplor:::ci_geom("no", "pct"))
})

test_that("the whole-table test is the leaf's, per col_var, and unchanged", {
  one <- tab(gss, marital, race, pct = "row", test = TRUE)
  two <- tab(gss, marital, c(race, relig), pct = "row", test = TRUE)
  t1  <- get_test(one); t2 <- get_test(two)
  expect_equal(nrow(t1), 1L)
  expect_equal(sort(t2$col), c("race", "relig"))
  # a col_var's test does not depend on which OTHER col_vars share the table -- the leaf computes it
  # on its own contingency table, where the joined step computed all of them in one batched call.
  # (The value itself is pinned against stats::chisq.test in test-calculations.R.)
  expect_equal(t1$statistic, t2$statistic[t2$col == "race"])
  expect_equal(t1$df1, t2$df1[t2$col == "race"])
})
