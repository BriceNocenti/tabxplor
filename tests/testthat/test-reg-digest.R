# PURPOSE: the fit-free record of a fit, and the per-model product built from it.
# ROLE: the shipped CONTRACT for R/reg-digest.R, R/reg-spec-build.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the fit digest ======================================================================

dg_data <- function() fx_reg_fmt()
dg_ds   <- function(wt = NULL) list(design = NULL, wt = wt)
dg_fit  <- function(fam = "binomial", dep = "married", preds = c("race", "age"), ...)
  suppressWarnings(suppressMessages(
    reg_fit(dg_data(), dep, preds, fam, dg_ds(...), TRUE, NULL, .95, "wald")))


# --- 1. a digest answers the generics its fit answers ---------------------------------------
test_that("the S3 surface is the fit's own", {
  f <- dg_fit()
  d <- f$digest
  expect_s3_class(d, "tabxplor_fitdigest")
  expect_identical(stats::coef(d),        stats::coef(f$fit))
  expect_identical(stats::vcov(d),        stats::vcov(f$fit))
  expect_identical(stats::nobs(d),        as.integer(stats::nobs(f$fit)))
  expect_equal(stats::df.residual(d),     reg_df_residual(f$fit))
  # a family is stored as (name, link) and rebuilt: the same behaviour, not the same 10 MB object
  expect_identical(stats::family(d)$family, stats::family(f$fit)$family)
  expect_identical(stats::family(d)$link,   stats::family(f$fit)$link)
  eta <- c(-2, 0, 1.5)
  expect_equal(stats::family(d)$linkinv(eta), stats::family(f$fit)$linkinv(eta))
  expect_equal(stats::family(d)$mu.eta(eta),  stats::family(f$fit)$mu.eta(eta))
  # terms: the same formula, but NOT the environment it was fitted in
  expect_identical(attr(stats::terms(d), "term.labels"),
                   attr(stats::terms(f$fit), "term.labels"))
  expect_identical(attr(stats::terms(d), ".Environment"), baseenv())
})

test_that("a digest carries nothing length-n, and is kilobytes", {
  f  <- dg_fit()
  kb <- length(serialize(reg_fit_distil(f), connection = NULL)) / 1024
  expect_lt(kb, 100)                                  # a raw glm serialises at ~2400 KB
  n  <- f$nobs
  big <- purrr::keep(f$digest, ~ is.atomic(.x) && length(.x) >= n)
  expect_length(big, 0L)
})


# --- 2. the frame is rebuilt, never stored ----------------------------------------------------
test_that("reg_digest_frame() reproduces the frame the fit was built on", {
  for (case in list(list("binomial", "married", c("race", "age")),
                    list("gaussian", "tvhours", c("race", "relig")),
                    list("poisson",  "tvhours", c("race", "age")),
                    list("multinomial", "partyid", c("race", "age")))) {
    f  <- dg_fit(case[[1]], case[[2]], case[[3]])
    fr <- reg_digest_frame(f$digest, dg_data())
    expect_identical(fr, f$data, info = case[[1]])
  }
})

test_that("a frame that does not reproduce the fit is refused, not used", {
  f <- dg_fit()
  d <- dg_data()[-seq_len(50), , drop = FALSE]        # a different population
  expect_null(reg_digest_frame(f$digest, d))
})


# --- 3. the influence function and the g-computation, off the digest --------------------------
# ⚠ the tolerance is the phase's one measured cost: glm stores the IRLS weights of the PREVIOUS
# iteration, so a reconstruction at the converged coefficients differs by ~1e-8 relative.
test_that("reg_coef_if_maker() gives the fit's own answer from the digest", {
  f  <- dg_fit()
  mk <- reg_coef_if_maker(f$digest, f$data)
  expect_false(is.null(mk))
  for (tm in setdiff(names(stats::coef(f$fit)), "(Intercept)")) {
    v <- mk(stats::setNames(1, tm))
    expect_length(v, nrow(f$data))
    # the sandwich SE this implies is the model SE up to O(1/n) on a correctly specified binomial
    se <- sqrt(sum(v * v))
    expect_equal(se, unname(sqrt(diag(stats::vcov(f$fit)))[tm]), tolerance = 0.1, info = tm)
  }
})

test_that("g-computation off a digest == off the fit", {
  f  <- dg_fit()
  gf <- reg_gcomp_maker(f$fit,    f$data, NULL, "identity")
  gd <- reg_gcomp_maker(f$digest, f$data, NULL, "identity")
  a  <- gf("race", "Black", "White"); b <- gd("race", "Black", "White")
  expect_equal(a$est, b$est); expect_equal(a$G, b$G); expect_equal(a$mean1, b$mean1)

  m  <- dg_fit("multinomial", "partyid", c("race", "age"))
  ef <- reg_prob_engine(m$fit); ed <- reg_prob_engine(m$digest)
  expect_identical(ef$levels, ed$levels)
  expect_equal(ef$theta, ed$theta)
  X <- ed$mm(m$data)
  expect_equal(ef$probs(ef$theta, X), ed$probs(ed$theta, X))
})


# --- 4. distil -> rehydrate is the same table -------------------------------------------------
test_that("a distilled record rebuilds the same numbers, and keeps its footer rows", {
  f <- dg_fit()
  f$gof_rows <- tibble::tibble(x = 1)                 # stand-in for what the eager stage computes
  d <- reg_fit_distil(f)
  expect_null(d[["fit"]]); expect_null(d[["data"]]); expect_null(d[["tidy"]])
  expect_identical(d$gof_rows, f$gof_rows)            # the eager rows survive distillation

  r <- reg_fit_rehydrate(d, dg_data(), TRUE, 0.95)
  expect_identical(r$data, f$data)
  expect_equal(r$tidy, f$tidy)
  # ... and at another confidence level it is what a fresh fit gives there
  r90 <- reg_fit_rehydrate(d, dg_data(), TRUE, 0.90)
  f90 <- suppressWarnings(suppressMessages(
    reg_fit(dg_data(), "married", c("race", "age"), "binomial", dg_ds(), TRUE, NULL, .90, "wald")))
  expect_equal(r90$tidy, f90$tidy)
})

test_that("reg_digest_revive() goes back through the one fitter", {
  f <- dg_fit()
  r <- reg_digest_revive(reg_fit_distil(f), dg_data())
  expect_false(is.null(r$fit))
  expect_equal(stats::coef(r$fit), stats::coef(f$fit))
})


# --- 5. the extension rule the two tables encode ----------------------------------------------
test_that("every declared backend states one equation shape and one influence engine", {
  for (k in names(REG_FIT_KINDS)) {
    row <- REG_FIT_KINDS[[k]]
    expect_true(row$equations %in% c("single", "categorical"), info = k)
    expect_true(is.na(row$score) || row$score %in% REG_SCORE_ENGINES, info = k)
  }
  # the predicate every dispatch reads, rather than inherits(fit, "multinom")
  expect_true(reg_model_categorical(dg_fit("multinomial", "partyid", "race")$digest))
  expect_false(reg_model_categorical(dg_fit()$digest))
  # svy_vglm declares no score, so the gap test is refused there rather than approximated
  expect_true(is.na(REG_FIT_KINDS$svy_vglm$score))
})

test_that("a part a kind does not declare is simply absent", {
  f <- dg_fit()                                       # a glm: no zeta, no y_levels
  expect_false("zeta" %in% f$digest$parts)
  expect_null(f$digest$zeta)
  o <- dg_fit("ordinal", "partyid", c("race", "age"))
  expect_true(all(c("zeta", "y_levels") %in% o$digest$parts))
  expect_equal(o$digest$zeta, o$fit$zeta)
})


# --- 5. the one cache seam (Phase 22g-x) ------------------------------------------------------
# reg_fit_cached() is what the model path and the crude one now share; the gate is two functions
# because its two clauses are about two different things.
test_that("the cache gate splits: profile refuses any fit, a comparison only the model ones", {
  expect_false(reg_crude_cacheable("profile"))
  expect_true (reg_crude_cacheable("wald"))
  sp <- list(outcome = "married", predictors = "race")
  expect_false(reg_fit_cacheable(sp, "wald", compare = "seq"))   # a test BETWEEN fit objects
  expect_true (reg_fit_cacheable(sp, "wald", compare = "none"))
})

test_that("reg_fit_cached() is the bare thunk with no store, and distils with one", {
  data <- dg_data()
  n    <- 0L
  thunk <- function() { n <<- n + 1L; dg_fit() }
  # no cache env, or no key: computed, and never distilled
  f0 <- reg_fit_cached(NULL, "k", thunk, data, TRUE, .95)
  expect_false(is.null(f0$fit))
  expect_equal(n, 1L)
  env <- jmvreg_cache_env(NULL)
  f1  <- reg_fit_cached(env, NULL, thunk, data, TRUE, .95)       # not cacheable
  expect_false(is.null(f1$fit))
  expect_equal(env$hits, 0L)
  # with both, the record is stored distilled and the second call is served
  f2 <- reg_fit_cached(env, "k", thunk, data, TRUE, .95)
  f3 <- reg_fit_cached(env, "k", thunk, data, TRUE, .95)
  expect_equal(env$hits, 1L)
  expect_null(f3$fit)                       # served: a digest and the frame rebuilt around it
  expect_false(is.null(f3$data))
  expect_equal(stats::coef(reg_model_of(f3)), stats::coef(reg_model_of(f2)))
  expect_equal(f3$tidy$estimate, f2$tidy$estimate)
})


# === SECTION: the per-model product ===============================================================

reg_fx <- local({
  d <- fx_reg_df()[seq(1, nrow(fx_reg_df()), 6), ]
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

test_that("a parallel refusal is reported only when parallel was asked for", {
  expect_silent(tab_reg(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
                        family = "binomial", stats = "compare_baseline"))
  skip_if_not_installed("mirai")
  withr::local_options(tabxplor.parallel = 2L)
  expect_message(tab_reg(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
                         family = "binomial", stats = "compare_baseline"),
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
  t_ser <- tab_reg(reg_fx, married ~ race * age, family = "binomial", stats = FALSE)
  t_par <- withr::with_options(list(tabxplor.parallel = TRUE),
                               tab_reg(reg_fx, married ~ race * age, family = "binomial",
                                       stats = FALSE))
  expect_identical(t_par, t_ser)
})
