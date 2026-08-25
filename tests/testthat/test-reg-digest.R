# PURPOSE: Phase 22j -- the `tabxplor_fitdigest`. What it must answer exactly as the fitted object
#   it replaces, what it must NOT carry, and that a distilled record rebuilds the same table.
# The two fact tables' own consistency is asserted at load (R/reg-digest.R) and their cross-table
# edges by TAB_FOREIGN_KEYS, so nothing here re-checks those.

skip_if_not_installed("broom")

dg_data <- function() gss_cat_data_formatting()
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
