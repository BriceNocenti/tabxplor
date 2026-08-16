# Phase 20c (KEY 5): TEST_ROWS -- the `test` attribute's discriminator vocabulary, declared.
# What these lock is the DERIVATION: every vocabulary that used to be a literal now comes off the
# table, with its contents AND ORDER intact, so no consumer moved. The table's own shape is a
# build-time stopifnot at the tail of R/tab-test-display.R (it runs at load, not here).

test_that("every discriminator a producer can write is a declared row", {
  skip_if_not_installed("broom")
  gss <- gss_cat_data_formatting()
  # one table of each kind the package builds -- this is the exhaustiveness gate, and it is a test
  # rather than a source scan because chi2_compute_test() is full of unrelated string constants.
  tabs <- list(
    crosstab = tab(gss, marital, race, test = TRUE),
    numeric  = tab(gss, marital, tvhours, test = TRUE),
    lm       = suppressMessages(tab_reg(gss, "tvhours", c("race", "age"), family = "gaussian")),
    glm      = suppressMessages(tab_reg(gss, "married", c("race", "age"), family = "binomial")),
    compare  = suppressMessages(tab_reg(gss, "married",
                                        list(a = "race", b = c("race", "age")),
                                        family = "binomial", stats = "compare_sequential")),
    split    = suppressMessages(tab_reg(gss, "married", "age", family = "binomial",
                                        tab_vars = "race", stats = c("n", "interaction")))
  )
  for (nm in names(tabs)) {
    tt <- get_test(tabs[[nm]])
    if (is.null(tt) || !nrow(tt)) next
    expect_true(all(tt$test %in% names(tabxplor:::TEST_ROWS)), info = nm)
  }
})

test_that("the derived vocabularies keep their contents and their order", {
  # the reg footer spec IS the reg grid slice, in declaration order
  expect_identical(names(tabxplor:::reg_footer_spec()), tabxplor:::TEST_FOOTER_KEYS)
  expect_identical(tabxplor:::reg_footer_test_types(), tabxplor:::TEST_FOOTER_KEYS)
  # the three instrument blocks
  expect_identical(tabxplor:::reg_global_types(),      c("global_lr", "global_f", "global_wald"))
  expect_identical(tabxplor:::reg_interaction_types(), c("interact_lr", "interact_f", "interact_wald"))
  # the goodness-of-fit block is reg_glance()'s, not "every row whose stat is its own name"
  expect_identical(tabxplor:::REG_GOF_KEYS,
                   c("n", "lr_null", "wald_null", "f_model", "r2", "r2_adj", "mcfadden_r2",
                     "nagelkerke_r2", "cox_snell_r2", "sigma", "aic", "bic", "phi"))
  # ⚠ the `stats =` vocabulary is a UNION: `residuals` / `normality` are panel-only checks with no
  # test row at all, so deriving from TEST_ROWS alone would silently delete two `check =` values.
  expect_true(all(c("residuals", "normality") %in% tabxplor:::reg_stat_keys()))
  expect_true(all(c("compare_baseline", "compare_sequential") %in% tabxplor:::reg_stat_keys()))
  # the (stat, method) lookup is total, which is what replaced the paste0()-built discriminators
  expect_identical(tabxplor:::test_row_key("compare_baseline", "aic"), "compare_baseline_aic")
  expect_identical(tabxplor:::test_row_key("compare_sequential", "lr"), "compare_seq")
})

test_that("the crosstab half is selected from the table, and names its own test", {
  # `anova` picks one F row; the chi2 rows carry no `anova` slot, so they always show
  expect_identical(tabxplor:::test_crosstab_displayed("welch"),
                   c("chi2", "chi2_design", "F_welch", "F_design"))
  expect_identical(tabxplor:::test_crosstab_displayed("classic"),
                   c("chi2", "chi2_design", "F_classic", "F_design"))
  expect_identical(tabxplor:::test_cell_label("F_welch"), "F, Welch")
  expect_true(is.na(tabxplor:::test_cell_label("aic")))       # a reg row has no in-cell label
})

test_that("a design-based numeric test is not called a Welch F (20c defect)", {
  # After the survey overlay a design table carries ONLY `F_design`, and the descriptor's numeric
  # arm read `if (any(num == "F_classic")) "ANOVA F" else "Welch F"` -- so it claimed a Welch F for
  # a svyglm + regTermTest Wald F. Each row declares its own `word` now.
  expect_identical(tabxplor:::test_pvalue_descriptor("F_design"), "pvalue (F; survey-design)")
  expect_identical(tabxplor:::test_pvalue_descriptor("F_welch"),  "pvalue (Welch F)")
  expect_identical(tabxplor:::test_pvalue_descriptor("F_classic"), "pvalue (ANOVA F)")
})
