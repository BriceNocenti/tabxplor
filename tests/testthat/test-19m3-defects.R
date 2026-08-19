# Phase 19m-iii -- the defects the ONE display relation exposed, and the two rule-2 repairs beside it.
# Each block fails on the pre-phase tree. Rule 7: a claimed fix ships with the fixture.

# === 1. ARITHMETIC on a get-only display token ======================================================
# get_num() had arms set_num() lacked, and vec_arith() writes through set_num() -- so `x * 2` on a
# column displaying one of them returned x UNCHANGED, with no warning, on a value ?fmt documents.
# Declaring `settable` is what made the gap visible.

testthat::test_that("arithmetic writes back on every settable display", {
  testthat::expect_equal(get_num(fmt(n = c(10, 20), pvalue = c(.02, .5), display = "pvalue") * 2),
                         c(0.04, 1))
  testthat::expect_equal(get_num(fmt(n = c(10, 20), or = c(2, 4), display = "or_pct") * 2),
                         c(4, 8))
  # `moe` and `ci` are one interval in two notations, so they write back to the same field
  ci <- fmt(n = c(10, 20), pct = c(.25, .5), ci_inf = c(.2, .4), ci_sup = c(.3, .6),
            scale = "level_pct", pct_type = "row")
  testthat::expect_equal(get_num(set_display(ci, "moe") * 2), get_num(set_display(ci, "ci") * 2))
})

testthat::test_that("the two DECLARED read-only tokens stay no-ops, on purpose", {
  # `resid` is DERIVED (fmt_resid(): p-value + sign(ctr)), `blank` shows nothing -- neither has a
  # field to write, so `settable = FALSE` is the honest state rather than an omission.
  testthat::expect_false("resid" %in% tabxplor:::DISPLAY_SETTABLE)
  testthat::expect_false("blank" %in% tabxplor:::DISPLAY_SETTABLE)
  r <- fmt(n = c(10, 20), pvalue = c(.02, .5), ctr = c(1, -1), display = "resid")
  testthat::expect_identical(get_num(r * 2), get_num(r))
})

# === 2. DISPLAY_TOKENS is the one relation, and it cannot drift ======================================

testthat::test_that("the derived vocabularies reproduce what they replaced", {
  # the exact contents AND order the pre-phase hand-written vectors had (both reach a user: the
  # "Valid fields" abort and ?tab's generated section).
  testthat::expect_identical(
    tabxplor:::DISPLAY_USER_FIELDS,
    c("pct", "n", "wn", "mean", "est", "base", "diff", "ratio", "ci", "moe", "or", "ctr", "var",
      "resid", "obs", "coef", "gap"))
  testthat::expect_identical(
    tabxplor:::DISPLAY_BARE_TOKENS,
    c("pct", "n", "wn", "mean", "est", "base", "diff", "ratio", "ci", "moe", "or"))
  testthat::expect_identical(tabxplor:::DISPLAY_ALIASES, c(rr = "ratio"))
  # every token carrying a VALUE of the table is re-templatable; only the four that carry none are not
  testthat::expect_setequal(
    setdiff(names(tabxplor:::DISPLAY_TOKENS), tabxplor:::DISPLAY_VALUE_CELLS),
    c("pvalue", "gof", "n_range", "blank", "rr"))
  testthat::expect_setequal(tabxplor:::DISPLAY_FOOTER_TOKENS, c("gof", "pvalue", "blank"))
  # `pvalue` is a footer token that IS coloured (a significance warning) -- the one disagreement
  # that makes these two facts two columns rather than one "numberless" flag.
  testthat::expect_setequal(tabxplor:::DISPLAY_NO_COLOR, c("blank", "gof", "n_range"))
  testthat::expect_true("pvalue" %in% tabxplor:::DISPLAY_FOOTER_TOKENS)
  testthat::expect_false("pvalue" %in% tabxplor:::DISPLAY_NO_COLOR)
  testthat::expect_identical(
    tabxplor:::DISPLAY_TOKEN_GEOMETRY,
    c(pct = "level", n = "level", wn = "level", mean = "level", base = "level",
      diff = "difference", ratio = "ratio", or = "ratio", n_range = "level"))
  testthat::expect_identical(
    tabxplor:::DISPLAY_COMPARISON,
    c(diff = "difference", ratio = "ratio", or = "odds_ratio"))
})

testthat::test_that("the build-time guard ties get_num()/set_num() to the table, both ways", {
  d <- names(tabxplor:::DISPLAY_TOKENS)[is.na(vapply(
    tabxplor:::DISPLAY_TOKENS, function(r) r$alias %||% NA_character_, character(1)))]
  # the scale-relative `est` / `base` are handled by the ONE resolver both maps run first, so it
  # counts as part of each -- exactly as the build-time guard in R/tab-display.R does.
  resolver <- tabxplor:::display_switch_tokens(tabxplor:::fmt_resolve_scale_tokens)
  read    <- c(tabxplor:::display_switch_tokens(tabxplor:::get_num), resolver)
  written <- c(tabxplor:::display_switch_tokens(tabxplor:::set_num), resolver)
  testthat::expect_true(all(read %in% d))                       # no undeclared arm
  testthat::expect_true(all(setdiff(d, "n") %in% read))         # no unhandled row ("n" = default)
  testthat::expect_true(all(written %in% d))
  # THE one that would have caught defect 1 above.
  testthat::expect_true(all(tabxplor:::DISPLAY_SETTABLE %in% written))
})

testthat::test_that("`OR` / `OR_pct` are rows, not aliases of `or` / `or_pct`", {
  # aliasing them would change what display_primary() returns, and fmt_display_shows() compares
  # against the RAW display -- so a stored "OR" must come back "OR".
  testthat::expect_identical(tabxplor:::display_primary(c("OR", "OR_pct")), c("OR", "OR_pct"))
  testthat::expect_true(all(is.na(vapply(tabxplor:::DISPLAY_TOKENS[c("OR", "OR_pct")],
                                         function(r) r$alias, character(1)))))
  # but they read the same field, which is why they render identically
  testthat::expect_identical(
    get_num(fmt(n = 10, or = 2.5, display = "OR")),
    get_num(fmt(n = 10, or = 2.5, display = "or")))
})

testthat::test_that("the generated help sections are built from the table", {
  user <- tabxplor:::display_tokens_rd(user_only = TRUE)
  full <- tabxplor:::display_tokens_rd(user_only = FALSE)
  testthat::expect_true(startsWith(user[[1]], "@section"))
  # every user field appears in the ?tab section, every token in the ?fmt one -- ?fmt's hand-written
  # list named ELEVEN of the twenty-two before this phase.
  for (tk in tabxplor:::DISPLAY_USER_FIELDS)
    testthat::expect_true(any(grepl(paste0("\\code{", tk, "}"), user, fixed = TRUE)))
  for (tk in names(tabxplor:::DISPLAY_TOKENS))
    testthat::expect_true(any(grepl(paste0("\\code{", tk, "}"), full, fixed = TRUE)))
  # ?fmt names every token, ?tab only the ones a user may type
  testthat::expect_gt(length(tabxplor:::DISPLAY_TOKENS), length(tabxplor:::DISPLAY_USER_FIELDS))
})

# === 3. the rule-2 repairs ===========================================================================

testthat::test_that("reg_check_model_se() names its own numbers (the dispersion panel's join key)", {
  # 19m-i left the panel joining `se` to a SECOND read, names(coef(fit)), by length coincidence.
  # vcov()'s dimnames come through sqrt(diag(.)) untouched, so the key was in `se` all along.
  fit <- stats::lm(mpg ~ cyl + wt, data = datasets::mtcars)
  se  <- tabxplor:::reg_check_model_se(fit)
  testthat::expect_identical(names(se), rownames(stats::vcov(fit)))
  testthat::expect_identical(names(se), names(stats::coef(fit)))
  testthat::expect_length(se, length(stats::coef(fit)))
})

testthat::test_that("the survey variance producers stopped promising a configurable Total key", {
  # "Total" is the LEAF's internal pre-rename key, not a user label -- so a `tot=` / `tot_lab=`
  # parameter no caller ever set was a false promise (and `total_names[1]` there would be a bug:
  # these producers run long before leaf_rename_totals()).
  testthat::expect_false("tot"     %in% names(formals(tabxplor:::svy_group_map)))
  testthat::expect_false("tot_lab" %in% names(formals(tabxplor:::svy_var_prop)))
})

testthat::test_that("a renamed total column still keeps its odds ratio (19m-i, still closed)", {
  # the regression this phase must not undo while touching the Total convention.
  t1 <- tab(forcats::gss_cat, marital, race, pct = "row", display = "{or}", ref = "first")
  # 20b: the totals are renamed through the option now, not through `total_names =`.
  withr::local_options(tabxplor.total_names = c(row = "Total", col = "Ensemble"))
  t2 <- tab(forcats::gss_cat, marital, race, pct = "row", display = "{or}", ref = "first")
  testthat::expect_equal(get_or(t1[[2]]), get_or(t2[[2]]))
})
