# PURPOSE: the test attribute: the summary grid, the console renderer and the export footer.
# ROLE: the shipped CONTRACT for R/tab-test-display.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the test attribute and its renderers ================================================

# the crosstab tests read the WHOLE frame: a chi2 on race x relig x party3 needs the expected
# counts, and on a sample the footer would carry the weak-test flag. They fit no model.
gss <- fx_gss_fmt()




# === formatters =====================================================================================

test_that("test_fmt_pvalue / stat / num / is_nonsig / weak-label format as specified", {
  expect_equal(test_fmt_pvalue(c(0.5, 0.001, 1e-6, NA)), c("50%", "0.1%", "<0.01%", NA))
  expect_equal(test_fmt_stat(1911, 6, NA), "1911 (df 6)")
  expect_equal(test_fmt_stat(127, 2, 2029.3), "127 (df 2; 2029)")
  expect_equal(test_fmt_stat(9.78, 8, 486), "9.78 (df 8; 486)")
  expect_equal(test_fmt_num(21483), "21 483")
  expect_equal(test_is_nonsig(c(0.04, 0.06, NA)), c(FALSE, TRUE, FALSE))
  # weak-test flag: min_e < 5 appends " !" (chi2 only; F has NA min_e)
  expect_equal(test_cell_label_weak("chi2", 3),   "Chi2 !")
  expect_equal(test_cell_label_weak("chi2", 10),  "Chi2")
  expect_equal(test_cell_label_weak("F_welch", NA_real_), "F, Welch")
  expect_equal(test_pvalue_label("chi2", 3), "(Chi2 !)")
})




# === the display grid ===============================================================================

test_that("crosstab grid: col_vars are columns, row_vars are groups, N/pvalue/effect-size rows", {
  t <- tab(gss, c(race, relig), c(party3, tvhours), pct = "row", test = TRUE)
  g <- test_summary_grid(t)
  expect_equal(g$stat_header, "Tests")
  expect_setequal(g$value_headers, c("party3", "tvhours"))
  expect_length(g$groups, 2L)                              # race, relig
  expect_equal(g$groups[[1]]$label_lines[[1]], "race")
  # Phase 18m: no statistic row; order = N, p-value, effect size; the test type / measure name the rows
  labs <- vapply(g$groups[[1]]$rows, `[[`, character(1), "label")
  expect_equal(labs[[1]], "N")
  expect_match(labs[[2]], "^pvalue \\(Chi2, Welch F")      # both factor + numeric col_vars
  expect_match(labs[[3]], "eta2")                          # Cramer's V + eta2 (measure names)
  expect_false(any(labs == "statistic"))
})





# === SECTION: the model-fit footer ================================================================

reg_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}




# ---- footer parity: binomial (glm) ----------------------------------------------------------

test_that("binomial footer N/LR-null/McFadden/AIC/BIC match a hand-fit glm; display-only", {
  d   <- reg_data()
  t1  <- tab_reg(d, "married", c("race", "rincome"), family = "binomial", cleannames = FALSE)
  tst <- get_test(t1)
  cv  <- "Model_OR"
  gv  <- function(s) tst$statistic[tst$col == cv & tst$test == s]

  # the footer is DISPLAY-ONLY: the built object is the coefficient skeleton, no "Model fit" rows
  expect_false(any(as.character(t1$var) == "Model fit"))
  expect_true(tab_is_reg(t1))

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(rincome))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  dm$race    <- forcats::fct_drop(dm$race); dm$rincome <- forcats::fct_drop(dm$rincome)
  g  <- stats::glm(married ~ race + rincome, data = dm, family = stats::binomial())
  g0 <- stats::glm(married ~ 1,              data = dm, family = stats::binomial())

  expect_equal(gv("n"),   as.numeric(stats::nobs(g)))
  expect_equal(gv("aic"), stats::AIC(g), tolerance = 1e-6)
  expect_equal(gv("bic"), stats::BIC(g), tolerance = 1e-6)
  lr <- g$null.deviance - g$deviance
  expect_equal(gv("lr_null"), lr, tolerance = 1e-6)
  expect_equal(tst$pvalue[tst$col == cv & tst$test == "lr_null"],
               stats::pchisq(lr, g$df.null - g$df.residual, lower.tail = FALSE), tolerance = 1e-6)
  expect_equal(gv("mcfadden_r2"),
               1 - as.numeric(stats::logLik(g)) / as.numeric(stats::logLik(g0)), tolerance = 1e-6)
})




# ---- Phase 18z13 (SS7.2): the per-predictor global test ------------------------------------------

test_that("stats='global' IS drop1() on the fit already in hand, as per-predictor footer rows", {
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                cleannames = FALSE, stats = c("n", "global")))
  tt <- get_test(t)
  g  <- tt[tt$test %in% tabxplor:::reg_global_types(), , drop = FALSE]
  # Phase 19g: the predictor rides `var`, the same dimension a crosstab test row uses; a split-group
  # level rides a column named after the split variable, so the two can no longer collide
  expect_setequal(g$var, c("race", "rincome"))           # one per multi-level predictor
  expect_identical(tabxplor:::test_group_cols(g), character(0))   # no split -> no group column
  expect_identical(unique(g$test), "global_lr")

  # the numbers ARE drop1's, no extra fit involved
  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(rincome))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  dm$race <- forcats::fct_drop(dm$race); dm$rincome <- forcats::fct_drop(dm$rincome)
  fit <- stats::glm(married ~ race + rincome, data = dm, family = stats::binomial())
  d1  <- stats::drop1(fit, scope = c("race", "rincome"), test = "Chisq")
  expect_equal(g$pvalue[match(rownames(d1)[-1], g$var)],
               d1[["Pr(>Chi)"]][-1], tolerance = 1e-8)
  expect_equal(g$statistic[match(rownames(d1)[-1], g$var)],
               d1[["LRT"]][-1], tolerance = 1e-8)

  # z15: they are GOF ROWS now -- one per (model column x predictor), labelled "<stat>: <predictor>"
  expect_true(all(tabxplor:::reg_global_types() %in% names(tabxplor:::reg_footer_spec())))
  md <- gsub(intToUtf8(160L), " ", tab_md(t, print = FALSE), fixed = TRUE)
  expect_true(any(grepl("Overall association (LR): race", md, fixed = TRUE)))
  expect_true(any(grepl("Overall association (LR): rincome", md, fixed = TRUE)))
})




# --- Phase 22g-ii: the comparison is the DEFAULT -------------------------------------------------
# Writing several `predictors` sets is something a reader does on purpose, and the row saying whether
# the added variables bought anything is the point of writing them. Which comparison is read off the
# models: a chain where each nests in the next is tested against the PREVIOUS, anything else against
# the first. Naming any footer statistic drops it, as naming any other argument value does.

cmp_rows <- function(t) {
  tt <- get_test(t)
  if (is.null(tt)) character(0) else unique(tt$test[grepl("compare", tt$test)])
}



cmp_msgs <- function(expr) {
  m <- character(0)
  withCallingHandlers(expr, message = function(e) {
    m <<- c(m, conditionMessage(e)); invokeRestart("muffleMessage") })
  m
}




# ---- Phase 22g-v: NOTHING IS NOTHING -----------------------------------------------------------

test_that("stats = NULL / FALSE / \"no\" / \"none\" all hide the footer; \"auto\" is the default", {
  d <- reg_data()
  f <- function(...) nrow(get_test(tab_reg(d, "married", "race", family = "binomial",
                                          empirical = FALSE, ...)))
  # R cannot tell a missing argument from an explicit NULL, which is why the default is a WORD
  for (v in list(NULL, FALSE, "no", "none")) expect_identical(f(stats = v), 0L)
  expect_gt(f(), 0L)
  expect_identical(f(stats = "auto"), f())
  # ⚠ a NAMED footer set keeps what it names -- it drops only the comparison
  expect_setequal(unique(get_test(tab_reg(d, "married", "race", family = "binomial",
                                          stats = c("n", "aic")))$test), c("n", "aic"))
})
