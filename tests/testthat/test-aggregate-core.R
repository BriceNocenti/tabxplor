# Phase 19j (KEY 5) -- one aggregate core.
#
# The interval and the whole-table test are computed IN THE LEAF, from the plan, instead of by
# tab_ci() / tab_chi2() running on the assembled table and re-deriving that plan from fmt markers.
# These are the fixtures that fail without that move; the byte-identity of everything else is proved
# by test-golden.R + dev/verify_golden_field_delta.R, not here.

# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


gss <- forcats::gss_cat

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
