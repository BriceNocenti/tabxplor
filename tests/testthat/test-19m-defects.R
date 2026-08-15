# Phase 19m-i (Harvest 2, open integration 1): the defects the phase fixes, each with the assertion
# that fails without its fix. Patterned on test-19l-defects.R -- one block per finding, the reason
# stated where the reader meets it.

gss <- forcats::gss_cat


# --- A1: tab_collapse_total_rows() keyed on group_vars()[1], not on the declared variable column ---
# tab_compact() groups by c(merge_tab_vars, "row_var"), so with tab_vars the FIRST grouping variable
# is the tab_var. The block sweep then stopped distinguishing row_var blocks, and the blank sentinel
# that gives the shared Total its own group landed in the tab_var -- corrupting the sub-table key.
# The shape is reachable only since 19f lifted the "tab_vars x several row_vars" refusal.
test_that("19m-i A1: a compacted + tab_vars table collapses per SUB-TABLE, keyed on the variable column", {
  t  <- tab(gss, c(marital, race), relig, tab_vars = year, common_totrow = TRUE)
  tm <- suppressMessages(tab_materialize_extras(t))

  expect_identical(tab_declared_vars(tm)$var_col, "row_var")
  # the blank goes in the VARIABLE column, never in the tab_var
  expect_gt(sum(as.character(tm$row_var) == ""), 0L)
  expect_identical(sum(as.character(tm$year) == ""), 0L)
  # every year keeps its own levels
  expect_setequal(setdiff(unique(as.character(tm$year)), ""),
                  unique(as.character(t$year)))

  # non-vacuity: the collapse really removed the duplicate totals -- ONE surviving Total per year
  fmt1  <- names(t)[purrr::map_lgl(t, is_fmt)][[1]]
  n_tot <- sum(is_totrow(tm[[fmt1]]))
  expect_lt(n_tot, sum(is_totrow(t[[fmt1]])))
  expect_identical(n_tot, dplyr::n_distinct(as.character(t$year)))
})

test_that("19m-i A1: without tab_vars the collapse is unchanged (one shared Total)", {
  t  <- tab(gss, c(marital, race), relig, common_totrow = TRUE)
  tm <- suppressMessages(tab_materialize_extras(t))
  fmt1 <- names(t)[purrr::map_lgl(t, is_fmt)][[1]]
  expect_identical(sum(is_totrow(tm[[fmt1]])), 1L)
  expect_gt(sum(as.character(tm$row_var) == ""), 0L)
})


# --- A2: tab_apply_reference() re-derived the TOTAL COLUMN from the literal "Total" ---------------
# It takes the DECLARED row facts (tottab_vector / totrow_vector) and re-derived the column one, on
# the leaf's own pre-rename convention. Its second caller (jmv_tab3_reref) passes POST-rename names,
# so with total_names = c("Total", "Ensemble") nothing matched. The re-ref half is locked in
# test-jmvtab-cache.R (a binary col_var, where it changes the odds ratio); here we lock the formal
# itself -- the fact is passed in, not guessed.
test_that("19m-i A2: tab_apply_reference() takes the total column as a DECLARED fact", {
  expect_true("totcol_vector" %in% names(formals(tab_apply_reference)))
  # and the leaf's own build is unaffected: a renamed total column still gets its odds ratio
  a <- tab(gss, marital, race, pct = "row", display = "{or}", ref = "first")
  b <- tab(gss, marital, race, pct = "row", display = "{or}", ref = "first",
           total_names = c("Total", "Ensemble"))
  expect_equal(get_or(a[[2]]), get_or(b[[2]]))
})


# --- D: "is this a regression" vs "does it still carry its recipe" --------------------------------
# Covered by test-degraded-attrs.R, which already builds the divergent state (a meta-stripped reg
# table). Nothing to repeat here.


# --- C: the silent length-fallback guards --------------------------------------------------------
# Every `ann` field is length(col) BY CONSTRUCTION. ABSENT stays a real state (a degraded model);
# SHORT was silently replaced by a neutral, which is how D1's grey footer survived two phases.
test_that("19m-i C: a short annotation vector is an error, an absent one is not", {
  # html_face_wrap(): the face flags
  expect_identical(html_face_wrap(c("a", "b"), NULL, NULL, NULL), c("a", "b"))
  expect_identical(html_face_wrap(c("a", "b"), c(TRUE, FALSE), NULL, NULL), c("<b>a</b>", "b"))
  expect_error(html_face_wrap(c("a", "b"), TRUE, NULL, NULL))
})


# --- G2: the col_var placeholder set, and tab_shape()'s report ------------------------------------
# `col_var` takes six values that are not a variable name. Eight filters spelled between two and six
# of them; the exported shape reader spelled two, so it reported the internal sentinel "no_col_var"
# as if it were a variable.
test_that("19m-i G2: no placeholder col_var reaches an exported reader", {
  t0 <- tab(gss, marital)                     # no col_var: every column carries "no_col_var"
  expect_identical(tab_shape(t0)$col_vars, character(0))
  expect_identical(tab_shape(tab(gss, marital, race))$col_vars, "race")
  expect_false(any(is_real_col_var(TAB_PLACEHOLDER_COL_VARS)))
  expect_true(all(is_real_col_var(c("race", "marital"))))
})

test_that("19m-i G2: a no-col_var table nests inside a col_var one (compact + kable accept it)", {
  l <- list(tab(gss, marital), tab(gss, relig, race))
  expect_true(tab_supports(l, "compact"))
  merged <- suppressMessages(tab_compact(l))
  expect_s3_class(merged, "tabxplor_tab")
  expect_gt(nrow(merged), 0L)
  expect_no_error(suppressMessages(tab_md(merged, css = FALSE)))
})

# Found while implementing the above, and PRE-EXISTING: tab_stack_tables() bound on the FIRST
# table's column names, so the declared nesting rule ("every table's col_vars a subset of the
# widest") depended on list ORDER -- narrow-first silently dropped the wider table's extra columns,
# wide-first errored. It binds on the UNION now, padding a table that lacks a column with NA cells.
test_that("19m-i G2: a nested compact keeps every column, in either list order", {
  wide   <- tab(gss, relig, c(race, partyid), pct = "row")
  narrow <- tab(gss, marital, race, pct = "row")
  a <- suppressMessages(tab_compact(list(narrow, wide)))
  b <- suppressMessages(tab_compact(list(wide, narrow)))
  expect_setequal(names(a), names(b))
  expect_true(all(levels(gss$partyid) %in% names(a)))     # the wide table's columns survive
  expect_identical(nrow(a), nrow(b))
})

test_that("19m-i G2: transposing a no-col_var table names its label column, not the sentinel", {
  # tab_transpose() is soft-deprecated; the deprecation is not this test's subject.
  tt <- suppressWarnings(suppressMessages(tab_transpose(tab(gss, marital))))
  expect_false("no_col_var" %in% names(tt))
  expect_identical(names(tt)[[1]], "variables")
})


# --- G3: one rule, one place ---------------------------------------------------------------------
test_that("19m-i G3: fmt()'s and new_fmt()'s display default is ONE declared rule", {
  # they disagreed for the bind neutral: fmt() said "pct", new_fmt() "n". new_fmt()'s is the
  # deliberate one (a neutral claims no percentage), and it is now declared on the EST_SCALES row.
  expect_identical(get_display(fmt(1L, scale = "mixed")), "n")
  for (k in names(EST_SCALES))
    expect_identical(get_display(fmt(1L, scale = k)), est_default_display(k), info = k)
})

# --- G5: a CELL interval has no reference, so every cell keeps it --------------------------------
# The rule was written three times and two of them were wrong: leaf_ci_plain() and tab_ci() blanked
# the reference cell under BOTH kinds, num_core() only under a contrast. So a FACTOR ci = "cell"
# table's Total row showed no bracket while a numeric one's did -- the same argument, two answers,
# and the one `vignettes/tabxplor.Rmd` teaches ("cell intervals compare each cell to 0 %, not to a
# reference ... purely descriptive") is the numeric one.
test_that("19m-i G5: ci = 'cell' keeps the reference row's own interval, on both leaves", {
  f <- tab(gss, marital, race, pct = "row", ci = "cell")
  j <- names(f)[purrr::map_lgl(f, is_fmt)][[1]]
  tot <- is_totrow(f[[j]])
  expect_true(any(tot))
  expect_true(all(!is.na(get_ci_inf(f[[j]])[tot])))     # fails without the fix
  expect_true(all(!is.na(get_ci_sup(f[[j]])[tot])))

  n <- tab_num(gss, race, age, ci = "cell", tot = "row", digits = 1)
  k <- names(n)[purrr::map_lgl(n, is_fmt)][[1]]
  expect_true(all(!is.na(get_ci_inf(n[[k]])[is_totrow(n[[k]])])))   # unchanged
})

test_that("19m-i G5: a CONTRAST interval still blanks the row it compares to itself", {
  # the negative half -- the fix must not over-reach
  d <- tab(gss, marital, race, pct = "row", ci = "ref", ref = "tot")
  j <- names(d)[purrr::map_lgl(d, is_fmt)][[1]]
  tot <- is_totrow(d[[j]])
  expect_true(all(is.na(get_ci_inf(d[[j]])[tot])))
  expect_true(all(is.na(get_pvalue(d[[j]])[tot])))
  # and every cell/diff geometry states it once
  expect_identical(ci_geom_ref_cell("cell", "pct"),  "keep")
  expect_identical(ci_geom_ref_cell("cell", "mean"), "keep")
  expect_identical(ci_geom_ref_cell("diff", "pct"),  "na")
  expect_identical(ci_geom_ref_cell("diff", "mean"), "na")
})


# --- G4: one per-family table ---------------------------------------------------------------------
test_that("19m-i G4: the four family name tables are one, and REG_FIT_FAMILY derives from it", {
  expect_setequal(names(REG_FAMILIES),
                  c("gaussian", "binomial", "poisson", "multinomial", "ordinal",
                    "quasipoisson", "rr", "rd", "mr"))
  # the readers keep their own defaults
  expect_identical(reg_family_display_name("wibble"), "regression")
  expect_identical(reg_family_short("wibble"), "reg")
  expect_identical(reg_family_short("gaussian"), "linear")
  # `ui = NA` IS "not offered in the picker" -- the fact the JS generator used to hardcode
  expect_false("quasipoisson" %in% names(reg_family_ui_labels()))
  expect_setequal(names(reg_family_ui_labels()),
                  c("gaussian", "binomial", "poisson", "multinomial", "ordinal"))
  expect_setequal(names(reg_family_ui_labels(binary = TRUE)), c("binomial", "poisson"))
  # REG_FIT_FAMILY is now the `outcome` column
  expect_identical(REG_FIT_FAMILY, c(rr = "binomial", rd = "binomial", mr = "gaussian"))
})

test_that("19m-i G4: the multiplicative effect word is DERIVED, total and singleton", {
  expect_identical(reg_family_mult_word("binomial"), "OR")
  expect_identical(reg_family_mult_word("poisson"), "IRR")
  expect_identical(reg_family_mult_word("quasipoisson"), "IRR")
  expect_identical(reg_family_mult_word("rr"), "RR")
  expect_identical(reg_family_mult_word("mr"), "RoM")
  expect_identical(reg_family_mult_word("multinomial"), "OR")
  expect_identical(reg_family_mult_word("ordinal"), "OR")
  # no exponentiated coefficient estimand -> no word (the old switch answered "OR" for both)
  expect_true(is.na(reg_family_mult_word("gaussian")))
  expect_true(is.na(reg_family_mult_word("rd")))
  expect_true(is.na(reg_family_mult_word("wibble")))
})

# The rendered word per family, which is what actually stands between "one lookup" and an Obs_IRR
# column silently legended as an odds ratio.
test_that("19m-i G4: every family's rendered effect word is unchanged", {
  d <- forcats::gss_cat |>
    dplyr::mutate(married = as.integer(marital == "Married"), tv = tvhours)
  want <- list(
    list(a = list(dependent = "married", family = "binomial"),                  w = "OR"),
    list(a = list(dependent = "married", family = "binomial", measure = "ratio"), w = "RR"),
    list(a = list(dependent = "tv", family = "poisson"),                        w = "IRR"),
    list(a = list(dependent = "tv", family = "quasipoisson"),                   w = "IRR"),
    # ⚠ the case that shows the fit word must NOT win unconditionally: a logistic fit asked for a
    # MARGINAL ratio has a crude RISK-ratio column beside it, and both are legended RR.
    list(a = list(dependent = "married", family = "binomial", effect = "marginal",
                  measure = "ratio"),                                            w = "RR")
  )
  for (cs in want) {
    t <- suppressWarnings(suppressMessages(do.call(
      tab_reg, c(list(data = d, predictors = "race"), cs$a, list(empirical = TRUE)))))
    m    <- reg_call(t)
    cols <- names(t)[purrr::map_lgl(t, ~ is_fmt(.) && get_scale(.) == "odds_ratio")]
    expect_gt(length(cols), 0L)
    for (cn in cols)
      expect_identical(legend_reg_eff_word(t[[cn]], m), cs$w, info = paste(cs$w, cn))
  }
})

test_that("19m-i G4: the worded CI-method labels are one declared table", {
  nm <- function(method, word) legend_method_name(list(ci_method = method, eff_word = word))
  expect_identical(nm("katz", "IRR"), "Katz interval on the log rate-ratio")
  expect_identical(nm("katz", "RR"),  "Katz interval on the log risk-ratio")
  expect_identical(nm("wald_log", "IRR"), "Wald interval on the log rate-ratio")
  expect_identical(nm("wald_log", "OR"),  "Wald interval on the log odds-ratio")
  expect_identical(nm("wald_log", "RR"),  "Wald interval on the log risk-ratio")
  expect_identical(nm("wald_log", NA_character_), "Wald interval on the log scale")
  # the plain engines still resolve, and an unknown one still degrades to the generic phrase
  expect_identical(nm("wilson", NA_character_), "Wilson score interval")
  expect_identical(nm("wibble", NA_character_), "confidence interval")
  # `katz`'s duplicated msgid is gone from CI_METHOD_LABELS (it lives in CI_METHOD_WORDED)
  expect_false("katz" %in% names(CI_METHOD_LABELS))
})

test_that("19m-i G4: REG_OUTCOME_KINDS says how it names each kind", {
  expect_setequal(names(REG_OUTCOME_KINDS), c("binary", "ordered", "nominal", "numeric"))
  for (k in names(REG_OUTCOME_KINDS))
    expect_true(nzchar(REG_OUTCOME_KINDS[[k]]$said), info = k)
})

test_that("19m-i G3: `totcol`'s vocabulary is the declared one everywhere", {
  ok <- TAB_ARG_VALUES$totcol$values
  expect_setequal(ok, c("last", "each", "all_col_vars", "no", ""))
  # tab_many() had lost "" and the legacy step had lost "all_col_vars"
  expect_no_error(suppressWarnings(tab_deprecate_many(list(totcol = ""))))
  expect_no_error(suppressWarnings(tab_deprecate_many(list(totcol = "all_col_vars"))))
})
