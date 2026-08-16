# PURPOSE: The defects Phase 19l's sweeps found -- one fixture each, each of which FAILS without the
#   fix (rule 7). They have nothing else in common, which is why they live together: a deletion pass
#   finds bugs in the places nobody looks, not in one subsystem.
# See: CLAUDE.md Phase 19l (pass 2).

gss <- forcats::gss_cat

# === 1. REG_CHECK_FAMILIES was missing the internal LINK keys ======================================
# 19e added two estimands whose FIT key is a link, not a distribution: `rd` (identity-link binomial,
# measure = "difference") and `mr` (log-link gaussian, measure = "ratio"). reg_checks_for() filters on
# that key, so both got ZERO assumption checks and ZERO diagnostic panels -- silently.
testthat::test_that("every estimand's fit gets its assumption checks (19l)", {
  d <- gss[1:800, ]
  d$y   <- factor(d$marital == "Married")
  d$num <- ifelse(is.na(d$tvhours), 1, d$tvhours)
  all_checks <- names(tabxplor:::REG_CHECKS)

  # compare the CHECKS, not their discriminators: `linearity` legitimately fires as an LR test on an
  # ordinary likelihood and as a Wald one on an svyglm fit, which is what rd / rr / mr are.
  got <- function(t) sort(unique(sub("_(lr|wald|f)$", "", get_test(t)$test)))
  base <- got(suppressWarnings(
    tab_reg(d, "y", c("race", "age"), family = "binomial", stats = all_checks)))
  testthat::expect_true(length(base) > 0)                       # the reference: a plain logit

  for (m in c("difference", "ratio")) {                         # fit keys "rd" and "rr"
    t <- suppressWarnings(
      tab_reg(d, "y", c("race", "age"), family = "binomial", measure = m, stats = all_checks))
    testthat::expect_setequal(got(t), base)
  }
  # a ratio of MEANS -- fit key "mr", the gaussian one
  t <- suppressWarnings(
    tab_reg(d, "num", c("race", "age"), family = "gaussian", measure = "ratio", stats = all_checks))
  testthat::expect_setequal(got(t), base)

  # the rule itself: every link key REG_FIT_FAMILY declares is answerable (build-time stopifnot too)
  testthat::expect_true(
    all(names(tabxplor:::REG_FIT_FAMILY) %in% tabxplor:::REG_CHECK_FAMILIES))
  # ... and each reads as its DISTRIBUTION, not as an unknown family falling through every arm
  testthat::expect_identical(tabxplor:::reg_check_family_of("rd"), "binomial")
  testthat::expect_identical(tabxplor:::reg_check_family_of("mr"), "gaussian")
  testthat::expect_identical(tabxplor:::reg_check_family_of("poisson"), "poisson")
})


# === 2. A transposed table with NO col_var was an out-of-bounds abort ==============================
# `compacted2 <- length(real_col_vars) > 1` sent length 0 down the ELSE branch, which indexed [[1]].
# A table with no col_var carries the "no_col_var" sentinel, which roles$real_col_vars filters out.
testthat::test_that("a no-col_var table transposes instead of aborting (19l)", {
  t <- tab(gss, marital)                        # one row_var, NO col_var
  # its fmt columns carry the "no_col_var" sentinel, which the render model filters out entirely --
  # leaving roles$real_col_vars EMPTY, the length the transpose could not handle.
  testthat::expect_true(all(get_col_var(t)[purrr::map_lgl(t, is_fmt)] == "no_col_var"))

  h <- as.character(tab_html(t, transpose = TRUE))
  testthat::expect_match(h, "<table")
  testthat::expect_match(h, "marital")          # the flipped row levels became the columns
  testthat::expect_no_error(tab_md(t, transpose = TRUE, print = FALSE, css = FALSE))
})


# === 3. The lone-total rename read a regex built from the USER's total name ========================
# `paste0("^", total_names[2], "_")` interpolated the user's string unescaped, so a total named with
# any regex metacharacter was a pattern. It is the stored `totcol` flag now.
testthat::test_that("the lone total column is found by its stored flag, not by its name (19l)", {
  t <- tab(gss, marital, race, pct = "row")
  testthat::expect_true("Total" %in% names(t))              # unqualified: exactly one total column
  tot <- which(purrr::map_lgl(t, ~ is_fmt(.) && is_totcol(.)))
  testthat::expect_length(tot, 1L)

  # a total name carrying regex metacharacters must behave exactly like a plain one
  withr::local_options(tabxplor.total_names = c(row = "Total", col = "Total (n)"))
  t2 <- tab(gss, marital, race, pct = "row")
  tot2 <- which(purrr::map_lgl(t2, ~ is_fmt(.) && is_totcol(.)))
  testthat::expect_length(tot2, 1L)
  testthat::expect_identical(unname(get_num(t[[tot]])), unname(get_num(t2[[tot2]])))
})


# === 4. "Is this a regression" is the STORED kind, not "does it still carry the recipe" ============
# legend_specs() asked `!is.null(reg_call(x))`. reg_spec() sets kind = "regression" with no `call`,
# and spec_bind() takes `sx$call %||% sy$call` -- so a reg table can legitimately have no `call`.
testthat::test_that("the legend reads the stored table kind (19l)", {
  d <- gss[1:600, ]; d$y <- factor(d$marital == "Married")
  t <- suppressWarnings(tab_reg(d, "y", "race"))
  testthat::expect_true(tab_is_reg(t))

  stripped <- tabxplor:::set_meta_field(t, "spec", tabxplor:::new_spec(kind = "regression"))
  testthat::expect_null(tabxplor:::reg_call(stripped))       # the recipe is gone ...
  testthat::expect_true(tab_is_reg(stripped))                # ... the KIND is not

  # the legend must still treat it as a regression (it used to fall back to the crosstab wording)
  testthat::expect_no_error(tab_color_legend(stripped, medium = "console"))
})


# === 5. One acronym per estimand ==================================================================
# 19l renamed the model column Model_MR -> Model_RoM and left its crude companion behind.
testthat::test_that("the crude companion of a ratio of means is named like its model column (19l)", {
  d <- gss[1:600, ]
  d$num <- ifelse(is.na(d$tvhours), 1, d$tvhours)
  t <- suppressWarnings(tab_reg(d, "num", "race", family = "gaussian", measure = "ratio",
                                empirical = TRUE))
  testthat::expect_true(any(grepl("RoM", names(t))))
  testthat::expect_false(any(grepl("_MR$|Obs_MR", names(t))))
})
