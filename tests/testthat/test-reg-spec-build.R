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
  expect_true(all(c("cols", "emp", "n_col", "gof_rows", "global_rows", "check_rows", "tips",
                    "nobs", "positive_level", "y_ref", "fit", "skeleton", "degraded") %in% names(p)))
  # ⚠ as.list(environment()) defaults to all.names = FALSE: a `.key` would vanish in silence
  expect_false(any(startsWith(names(p), ".")))
})

test_that("reg_emp_slim() drops the heavy halves and keeps what the assembler reads", {
  e <- list(cols = list(a = 1), shape = list(nm = "Obs_%"),
            effect = stats::setNames(list(1:3), ""),   # "" is the key of a single-column fit
            frame = data.frame(x = 1:100), fits = list(a = list(fit = 1)), grid = 1:9,
            fac_preds = "x", fit_preds = "x", crude_key = "binomial")
  s <- reg_emp_slim(e)
  expect_identical(names(s), c("cols", "shape", "effect"))
  expect_null(reg_emp_slim(NULL))
})

# --- the payload rule ----------------------------------------------------------------------------

test_that("a product carries no fit, and no crude frame, unless the shape is serial anyway", {
  # ONE model, `empirical`: its crude block served nobody else, so it is slimmed on the way out
  t1 <- tab_reg(reg_fx, "married", c("race", "age"), family = "binomial",
                empirical = TRUE, stats = FALSE)
  expect_s3_class(t1, "tabxplor_tab")

  # the product's own shape, taken from a live build
  ctx <- NULL
  local({
    a <- reg_resolve_args(reg_fx, "married", c("race", "age"), family = "binomial",
                          empirical = TRUE, stats = FALSE, na_explicit = FALSE)
    c0 <- new_reg_ctx(data = a$data, specs = a$specs, shared = a$shared,
                      family = a$specs[[1]]$fit_family)
    ctx <<- reg_stage_setup(c0)
  })
  p <- reg_spec_build(1L, ctx)
  expect_null(p$fit)                                   # compare == "none"
  expect_null(p$emp$frame)                             # slimmed: nobody else needs it
  expect_null(p$emp$fits)
  expect_true(length(p$emp$cols) > 0L)                 # ...but the columns survive
  expect_true(is.numeric(p$nobs) && p$nobs > 0)
})

test_that("a compared model's crude block is kept WHOLE for the models that borrow it", {
  a <- reg_resolve_args(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
                        family = "binomial", empirical = TRUE, stats = FALSE, na_explicit = FALSE)
  ctx <- reg_stage_setup(new_reg_ctx(data = a$data, specs = a$specs, shared = a$shared,
                                     family = a$specs[[1]]$fit_family))
  expect_true(ctx$share_crude)
  expect_true(ctx$spec_plan$want_emp[[1]])
  expect_false(ctx$spec_plan$want_emp[[2]])            # 20f-ii: only spec 1 builds one
  p1 <- reg_spec_build(1L, ctx)
  expect_false(is.null(p1$emp$frame))                  # the shared block keeps its frame...
  expect_false(is.null(p1$emp$fits))                   # ...and its crude legs, for spec 2's gap SE
})

# --- reg_specs_independent() ---------------------------------------------------------------------

test_that("reg_specs_independent() names its three refusals and says nothing otherwise", {
  mk <- function(...) {
    a <- reg_resolve_args(reg_fx, ..., na_explicit = FALSE)
    reg_stage_setup(new_reg_ctx(data = a$data, specs = a$specs, shared = a$shared,
                                family = a$specs[[1]]$fit_family))
  }
  # a single model: nothing to share
  expect_null(reg_specs_independent(mk("married", c("race", "age"), family = "binomial")))
  # several outcomes, no crude block: independent
  expect_null(reg_specs_independent(
    mk(c("married", "tvhours"), "race", stats = FALSE)))
  # a models list with the default stats: independent (compare is "none")
  expect_null(reg_specs_independent(
    mk("married", list(m1 = "race", m2 = c("race", "age")), family = "binomial", stats = FALSE)))
  # ...but a comparison is a test BETWEEN the fits
  expect_match(reg_specs_independent(
    mk("married", list(m1 = "race", m2 = c("race", "age")), family = "binomial",
       stats = "compare_baseline")), "between the fits")
  # ...and compared models with a crude block share spec 1's
  expect_match(reg_specs_independent(
    mk("married", list(m1 = "race", m2 = c("race", "age")), family = "binomial",
       empirical = TRUE, stats = FALSE)), "observed")
})

test_that("`parallel` reports the refusal only when it was asked for", {
  expect_silent(tab_reg(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
                        family = "binomial", stats = "compare_baseline"))
  expect_message(tab_reg(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
                         family = "binomial", stats = "compare_baseline", parallel = TRUE),
                 "one after another")
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

# --- the plan ------------------------------------------------------------------------------------

test_that("add_n's declared plan is one count column per distinct outcome", {
  a <- reg_resolve_args(reg_fx, c("married", "tvhours"), "race", add_n = TRUE, na_explicit = FALSE)
  ctx <- reg_stage_setup(new_reg_ctx(data = a$data, specs = a$specs, shared = a$shared,
                                     family = a$specs[[1]]$fit_family))
  expect_equal(ctx$spec_plan$want_n, c(TRUE, TRUE))
  expect_equal(ctx$spec_plan$n_names, c("n [married]", "n [tvhours]"))

  b <- reg_resolve_args(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
                        family = "binomial", add_n = TRUE, na_explicit = FALSE)
  ctx2 <- reg_stage_setup(new_reg_ctx(data = b$data, specs = b$specs, shared = b$shared,
                                      family = b$specs[[1]]$fit_family))
  expect_equal(ctx2$spec_plan$want_n, c(TRUE, FALSE))   # the compared models share one population
  expect_equal(ctx2$spec_plan$n_names, c("n", "n"))
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
