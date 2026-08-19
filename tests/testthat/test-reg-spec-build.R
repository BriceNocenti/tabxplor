# Phase 20f-iii: the per-spec product and the predicate that decides whether the specs of a
# tab_reg() call can be built independently of one another.
#
# WHAT THESE PIN. Not the numbers (dev/verify_reg_specs.R and the other reg files do that) but the
# three CONTRACTS the restructure rests on:
#   1. the product's declared shape, and THE PAYLOAD RULE -- no fit, and no 60-100 MB crude frame,
#      unless the shape is one that is serial anyway;
#   2. reg_specs_independent()'s three refusals, each a fact about the statistics;
#   3. the two placeholders (footer `col`, tooltip row/column indices) resolving to the same table
#      the pre-20f-iii stages produced.

reg_fx <- local({
  d <- forcats::gss_cat[seq(1, nrow(forcats::gss_cat), 6), ]
  d$married <- factor(ifelse(d$marital == "Married", "Married", "Not married"))
  d$party3  <- forcats::fct_lump_n(d$partyid, 2)
  d
})

# --- the product ---------------------------------------------------------------------------------

test_that("new_reg_spec_product() declares every slot, and no dot-prefixed key", {
  p <- new_reg_spec_product()
  expect_true(all(c("cols", "emp", "gof_rows", "global_rows", "check_rows", "tips",
                    "positive_level", "fit", "skeleton", "degraded") %in% names(p)))
  # ⚠ as.list(environment()) defaults to all.names = FALSE: a `.key` would vanish in silence
  expect_false(any(startsWith(names(p), ".")))
})

test_that("reg_emp_slim() keeps the columns and nothing else", {
  e <- list(cols = list(a = 1), shape = list(word = "OR", scale = "odds_ratio"),
            effect = stats::setNames(list(1:3), ""),   # "" is the key of a single-column fit
            frame = data.frame(x = 1:100), fits = list(a = list(fit = 1)), grid = 1:9,
            fac_preds = "x", fit_preds = "x")
  s <- reg_emp_slim(e)
  expect_identical(names(s), "cols")                   # everything else is builder-local
  expect_null(reg_emp_slim(NULL))
})

# --- the payload rule ----------------------------------------------------------------------------

# a live ctx, through the two stages that precede the per-spec builder
mk_ctx <- function(..., crude = TRUE) {
  a   <- reg_resolve_args(reg_fx, ..., na_explicit = FALSE)
  ctx <- reg_stage_setup(new_reg_ctx(data = a$data, specs = a$specs, shared = a$shared,
                                     family = a$specs[[1]]$fit_family))
  if (crude) reg_stage_crude(ctx) else ctx
}

test_that("a product carries no fit, and never a crude frame", {
  # ONE model, `empirical`: the block is the OUTCOME's, so reg_stage_crude() built it and the
  # product carries only what the assembler splices.
  ctx <- mk_ctx("married", c("race", "age"), family = "binomial",
                empirical = TRUE, stats = FALSE)
  expect_false(is.null(ctx$crude$frame))               # the block itself keeps its heavy halves...
  expect_false(is.null(ctx$crude$fits))                # ...for the gap test, on the main process

  p <- reg_spec_build(1L, ctx)
  expect_null(p$fit)                                   # compare == "none"
  expect_null(p$emp)                                   # a one-outcome spec builds no block at all
  # the ONE fit-derived scalar the product still carries (reg_stage_rows -> reg_curves)
  expect_identical(p$positive_level, "Married")
})

test_that("with SEVERAL outcomes each spec builds its own block, slimmed on the way out", {
  ctx <- mk_ctx(c("married", "tvhours"), c("race", "age"), empirical = TRUE, stats = FALSE)
  expect_null(ctx$crude)                               # no shared block: each spec IS an outcome
  expect_equal(ctx$spec_plan$want_emp, c(TRUE, TRUE))
  p <- reg_spec_build(1L, ctx)
  expect_true(length(p$emp$cols) > 0L)                 # the columns survive...
  expect_null(p$emp$frame)                             # ...and nothing else does (the payload rule)
  expect_null(p$emp$fits)
})

test_that("the compared models of ONE outcome share the table's block, built before them", {
  ctx <- mk_ctx("married", list(m1 = "race", m2 = c("race", "age")),
                family = "binomial", empirical = TRUE, stats = FALSE)
  expect_true(ctx$spec_plan$want_crude)
  expect_false(any(ctx$spec_plan$want_emp))            # nobody builds one per spec
  expect_true(length(ctx$crude$cols) > 0L)
  # ...and every model reads it: `obs` is written on both models' columns
  p2 <- reg_spec_build(2L, ctx)
  expect_true(any(!is.na(get_obs(p2$cols[[1]]$col))))
})

# --- reg_specs_independent() ---------------------------------------------------------------------

test_that("reg_specs_independent() names its ONE reachable refusal and says nothing otherwise", {
  mk <- function(...) mk_ctx(..., crude = FALSE)
  # a single model: nothing to share
  expect_null(reg_specs_independent(mk("married", c("race", "age"), family = "binomial")))
  # several outcomes, no crude block: independent
  expect_null(reg_specs_independent(
    mk(c("married", "tvhours"), "race", stats = FALSE)))
  # a models list with the default stats: independent (compare is "none")
  expect_null(reg_specs_independent(
    mk("married", list(m1 = "race", m2 = c("race", "age")), family = "binomial", stats = FALSE)))
  # ...and STILL independent with a crude block: Phase 20f-iiii made it the outcome's, built by
  # reg_stage_crude() before any model, so nothing is handed from one spec to another
  expect_null(reg_specs_independent(
    mk("married", list(m1 = "race", m2 = c("race", "age")), family = "binomial",
       empirical = TRUE, stats = FALSE)))
  # the one refusal a user can reach: a comparison is a test BETWEEN the fits
  expect_match(reg_specs_independent(
    mk("married", list(m1 = "race", m2 = c("race", "age")), family = "binomial",
       stats = "compare_baseline")), "between the fits")
})

test_that("`parallel` reports the refusal only when it was asked for", {
  expect_silent(tab_reg(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
                        family = "binomial", stats = "compare_baseline"))
  expect_message(tab_reg(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
                         family = "binomial", stats = "compare_baseline", parallel = TRUE),
                 "one after another")
})

# --- the crude block is FIT-FREE (Phase 20f-iiii) -------------------------------------------------
# reg_stage_crude() runs before any model, so the two facts the block used to read off the fit are
# produced on their own. Both are equal BY CONSTRUCTION; these pin the construction.

test_that("reg_positive_level() is what the fit records as its positive level", {
  for (na_mode in c("drop_by_outcome", "drop_by_model")) {
    a   <- reg_resolve_args(reg_fx, "married", c("race", "age"), family = "binomial",
                            na = na_mode, na_explicit = TRUE)
    ctx <- reg_stage_setup(new_reg_ctx(data = a$data, specs = a$specs, shared = a$shared,
                                       family = a$specs[[1]]$fit_family))
    sp  <- ctx$specs[[1]]
    inv <- reg_outcome_level_of(sp$outcome_level) %||% ctx$shared$outcome_level
    f   <- reg_fit(ctx$data, sp$outcome, sp$predictors, sp$fit_family, ctx$shared$design_spec,
                   TRUE, inv, 0.95, ctx$shared$method,
                   drop_extra = ctx$shared$na_shared_vars)
    expect_identical(reg_positive_level(reg_emp_frame(sp$outcome, ctx), sp$outcome, inv),
                     f$positive_level)
  }
})

test_that("the outcome's reference CATEGORY collapses to the first crude level either way", {
  # reg_crude_yw() keeps a `ref_category` only when it is a level of the crude frame, so the fit's
  # own y_ref and the first level of that frame are the same value in both branches.
  d    <- data.frame(y = factor(c("a", "b", "c", "a", "b", "c")), x = 1:6)
  cats <- levels(d$y)
  for (rc in list(NULL, cats[[1]], "a level that is not there")) {
    expect_identical(reg_crude_yw(d, "y", "multinomial", ref_category = rc)$ref, cats[[1]])
  }
  # ...which is exactly what reg_stage_crude() passes
  expect_identical(levels(forcats::fct_drop(as.factor(d$y)))[[1L]], cats[[1]])
})

# --- the placeholders ----------------------------------------------------------------------------

test_that("the footer rows are re-keyed to each model's first column", {
  t <- tab_reg(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
               family = "binomial", stats = c("n", "aic"))
  tst <- get_test(t)
  fmt_cols <- names(t)[vapply(t, is_fmt, logical(1))]
  # every footer row keys a column that EXISTS (the placeholder was a pre-make.unique label)
  expect_true(all(tst$col[tst$test %in% c("n", "aic")] %in% fmt_cols))
  expect_length(unique(tst$col[tst$test == "aic"]), 2L)   # one per model
})

test_that("a numeric predictor's crude tooltip keys the crude column and the right row", {
  t <- tab_reg(reg_fx, "married", c("race", "age"), family = "binomial",
               empirical = TRUE, stats = FALSE)
  tips <- get_empirical_tips(t)
  skip_if(is.null(tips))
  expect_true(all(tips$col %in% names(t)))
  # the row key resolved to a DISPLAY level of the table, not a skeleton index
  expect_true(all(tips$level %in% as.character(t$levels)))
})

test_that("a multinomial crude tooltip keys a category column of the table", {
  t <- tab_reg(reg_fx, "party3", "race", family = "multinomial",
               empirical = TRUE, stats = FALSE, cleannames = FALSE)
  tips <- get_empirical_tips(t)
  skip_if(is.null(tips) || nrow(tips) == 0L)
  expect_true(all(tips$col %in% names(t)))
  expect_true(all(tips$level %in% as.character(t$levels)))
})

test_that("compared multinomial models each get tooltips, and they agree", {
  # Phase 20f-iiii deleted reg_spec_tips_mnl()'s second producer -- specs 2..S had no block of their
  # own, so each rebuilt the grid with reg_empirical(). They read the table's ONE block now, which
  # is only correct because every model of a one-outcome table resolves the same reference category.
  t <- tab_reg(reg_fx, "party3", list(m1 = "race", m2 = c("race", "age")),
               family = "multinomial", empirical = TRUE, stats = FALSE, cleannames = FALSE)
  tips <- get_empirical_tips(t)
  skip_if(is.null(tips) || nrow(tips) == 0L)
  expect_true(all(tips$col %in% names(t)))
  # both models' category columns carry tips...
  expect_true(length(unique(sub(".*\\.\\.\\.", "", tips$col))) > 1L ||
                length(unique(tips$col)) > 1L)
  # ...and, for a given (category, level), the two models say the same thing: it is ONE crude number
  by_cell <- split(tips$tip, paste(tips$var, tips$level, sub("^.*?_", "", tips$col)))
  expect_true(all(vapply(by_cell, function(v) length(unique(v)) == 1L, logical(1))))
})

# --- the plan ------------------------------------------------------------------------------------

test_that("every built column carries its level's own N, on that model's own frame", {
  t <- suppressMessages(tab_reg(reg_fx, c("married", "tvhours"), "race"))
  mods <- names(t)[vapply(t, function(x) is_fmt(x) && get_role(x) == "model", logical(1))]
  expect_length(mods, 2L)
  # different outcomes = different complete cases, and each column says so for itself
  expect_false(identical(get_n(t[[mods[[1]]]]), get_n(t[[mods[[2]]]])))
  expect_true(all(!is.na(get_n(t[[mods[[1]]]])[as.character(t$var) == "race"])))
})

test_that("a deferred skeleton survives the pooled branch (one compound spec)", {
  # ⚠ regression: `skeleton_deferred` means the skeleton is read back off the FIRST fit. With ONE
  # spec there is nothing to share, so reg_specs_independent() lets the call take the pooled branch
  # -- where the serial loop's ctx update never runs. The skeleton must be taken from the product
  # after BOTH branches, or every later stage sees NULL.
  skip_if_not_installed("mirai")
  t_ser <- tab_reg(reg_fx, married ~ race * age, family = "binomial", stats = FALSE,
                   parallel = FALSE)
  t_par <- tab_reg(reg_fx, married ~ race * age, family = "binomial", stats = FALSE,
                   parallel = TRUE)
  expect_identical(t_par, t_ser)
})
