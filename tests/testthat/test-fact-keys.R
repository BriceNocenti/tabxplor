# PURPOSE: the declared fact tables and the foreign keys checked at load.
# ROLE: the shipped CONTRACT for R/zzz-fact-keys.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the declared foreign keys ===========================================================

testthat::test_that("every declared foreign key resolves", {
  testthat::expect_no_error(tabxplor:::tx_check_foreign_keys())
})

testthat::test_that("no target table has an unreferenced row", {
  # `orphan = TRUE` edges point at COLOR_SCALES: a break ladder no measure and no scale references
  # is dead weight, and this is where that is noticed.
  testthat::expect_equal(tabxplor:::tx_check_foreign_keys(), list())
})

testthat::test_that("the checker catches a dangling key", {
  broken <- list(tabxplor:::tx_fk("FAKE$key", function() c("difference", "not_a_measure"),
                                  function() names(tabxplor:::MEASURES)))
  testthat::expect_error(tabxplor:::tx_check_foreign_keys(broken), "not_a_measure")
  testthat::expect_error(tabxplor:::tx_check_foreign_keys(broken), "dangling key in FAKE[$]key")
})

testthat::test_that("an `allow` entry is legal but does not open the gate", {
  ok  <- list(tabxplor:::tx_fk("FAKE$key", function() "ci", function() tabxplor:::fmt_field_names,
                               allow = "ci"))
  bad <- list(tabxplor:::tx_fk("FAKE$key", function() c("ci", "nope"),
                               function() tabxplor:::fmt_field_names, allow = "ci"))
  testthat::expect_no_error(tabxplor:::tx_check_foreign_keys(ok))
  testthat::expect_error(tabxplor:::tx_check_foreign_keys(bad), "nope")
})

testthat::test_that("the row readers are exact, never partial-matching", {
  # THE hazard the file header names: MEASURES$adjustment has `scale_from` and no `scale`, so `$`
  # would return "gap" and a generic checker would validate the wrong string.
  tbl <- list(a = list(scale_from = "gap"), b = list(scale = "pct_diff"))
  testthat::expect_identical(tabxplor:::tx_fk_scalar(tbl, "scale"), c(NA_character_, "pct_diff"))
  testthat::expect_identical(tabxplor:::tx_fk_all(tbl, "scale"), "pct_diff")
})

testthat::test_that("the edge inventory covers the tables a rename would break", {
  froms <- vapply(tabxplor:::TAB_FOREIGN_KEYS, function(k) k$from, character(1))
  # the edge 19d actually broke, and the four that were never checked at all
  testthat::expect_true("EST_SCALES$label_meas" %in% froms)
  testthat::expect_true("REG_ESTIMANDS$rows$crude_fam" %in% froms)
  testthat::expect_true("REG_ESTIMANDS$rows$crude_shape" %in% froms)
  testthat::expect_true("DISPLAY_TOKENS$field" %in% froms)
  testthat::expect_gt(length(froms), 25L)
})

testthat::test_that("the crude shape of every reachable estimand exists IN ITS OWN BLOCK", {
  # The bare-name edge above only asks "does some block declare this shape". Two things it cannot
  # see, and both drew another estimand's column: the BLOCK is chosen by `crude_fam`/`crude_key`
  # (a borrow crosses families), and a `measure = "log_*"` request composes the `_log` twin by
  # string concatenation, so an undeclared one silently resolved to its block's coefficient shape.
  reach <- tabxplor:::tx_fk_emp_reachable()
  testthat::expect_gt(length(reach), 20L)
  testthat::expect_true(all(grepl("\\.", reach)))
  testthat::expect_true(all(reach %in% tabxplor:::tx_fk_emp_shape_keys()))
  # the pairs the qualified form exists for: same shape NAME, different blocks and different engines
  keys <- tabxplor:::tx_fk_emp_shape_keys()
  testthat::expect_true(all(c("binomial.or_log", "rr.rr_log", "grouped_binomial.rr_log",
                              "multinomial.ame_ratio_log", "ordinal.win_ratio_log") %in% keys))
  # and the edge itself refuses a block that does not declare the shape asked of it
  broken <- list(tabxplor:::tx_fk("FAKE", function() "binomial.rr_log",
                                  tabxplor:::tx_fk_emp_shape_keys))
  testthat::expect_error(tabxplor:::tx_check_foreign_keys(broken), "binomial.rr_log")
})


# === SECTION: TEST_ROWS: the footer vocabulary ====================================================

test_that("every discriminator a producer can write is a declared row", {
  gss <- fx_reg_fmt()
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
                                        tab_vars = "race", stats = c("n", "group_interaction")))
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
  expect_identical(tabxplor:::reg_interaction_types(), c("group_interact_lr", "group_interact_f", "group_interact_wald"))
  # the goodness-of-fit block is reg_glance()'s, not "every row whose stat is its own name"
  # ⚠ `phi` sits with the CHECK it refines (right under Dispersion), so the display order is not
  # the producer's -- it is still one of reg_glance()'s rows.
  expect_identical(tabxplor:::REG_GOF_KEYS,
                   c("n", "phi", "lr_null", "wald_null", "f_model", "r2", "r2_adj", "mcfadden_r2",
                     "nagelkerke_r2", "cox_snell_r2", "sigma", "aic", "bic"))
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
