# Phase 8: parallel builds must be BYTE-IDENTICAL to the serial one. The build is dispatched per
# row_var to a named mirai daemon pool; everything cross-cutting (duplicated_levels rename, the join,
# tab_apply_tests / chi2 / ci) stays on the main process, so parity holds by construction. These
# tests exercise the real daemon path (skipped on CRAN / when mirai is absent).

skip_on_cran()
skip_if_not_installed("mirai")
skip_if_not_installed("pkgload")

# WARNING: must also skip under covr, or `covr::package_coverage()` DIES (not fails -- dies) with
# "Error in readRDS(f) : error reading from connection". Root-caused 2026-07-27 by reproducing it
# locally: covr instruments the INSTALLED package and injects
# `reg.finalizer(ns, covr:::save_trace, onexit = TRUE)`, so EVERY process that loads tabxplor writes
# a `covr_trace_*.Rds` when it exits -- including the mirai daemons these tests start. Those daemons
# are then KILLED by `mirai::daemons(0)` (tab_parallel_stop / the pool resize), which interrupts
# saveRDS mid-write. Measured: 10 healthy traces of ~1.19 MB alongside 4 truncated ones of exactly
# 688128 and 753664 bytes -- both exact multiples of 4096, i.e. only whole filesystem pages were
# flushed. covr's `merge_coverage.character()` then readRDS()es every trace with no guard against a
# corrupt one, so the whole coverage run aborts.
# This is invisible unless NOT_CRAN=true: without it `skip_on_cran()` above already skips this file.
# r-lib/actions sets NOT_CRAN=true job-wide, which is why the CI coverage job hit it and a plain
# local `covr::package_coverage()` did not. Nothing is lost by skipping: a killed grandchild process
# cannot contribute reliable coverage anyway. R-CMD-check still runs this file in full.
skip_if(identical(Sys.getenv("R_COVR"), "true"),
        "covr: killed mirai daemons truncate covr's trace files (see comment above)")

withr::defer(tab_parallel_stop())  # release the "tabxplor" pool at end of file

# The daemons bind the *installed* tabxplor namespace. Under devtools::load_all that predates the
# current source (no tab_build_one / tab_rowvar_ctxs), so the pool must be pre-warmed with the dev
# source; an installed/checked package needs no load_all. Detect dev by the SOURCE file (installed
# pkgs drop R/).
dev_pkg_path <- function() {
  p <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)
  if (is.null(p) || !file.exists(file.path(p, "R", "tab-parallel.R"))) return(NULL)
  normalizePath(p)
}

.pool <- new.env(parent = emptyenv())
warm_pool <- function(n = 2L) {
  if (isTRUE(.pool$n == n)) return(invisible())          # tab_pool_ensure() reuses a matching pool
  mirai::daemons(0, .compute = "tabxplor")
  mirai::daemons(n, .compute = "tabxplor")
  dev <- dev_pkg_path()
  if (!is.null(dev)) {
    mirai::everywhere(
      { suppressMessages(pkgload::load_all(dev, quiet = TRUE)) },
      dev = dev, .compute = "tabxplor"
    )
  }
  .pool$n <- n
  invisible()
}

# Phase 22b-vi: `parallel` is an OPTION, not an argument -- so every "serial vs parallel" pair here
# sets it around the call instead of passing it. `with_par(FALSE, ...)` is the serial branch.
with_par <- function(p, expr) withr::with_options(list(tabxplor.parallel = p), expr)

# Fixture: >=4 row_vars, 2 factor + 1 numeric col_var (exercises tab_transform's numeric + factor branches), a
# weight, a deliberate cross-col_var level collision ("No answer" in partyid AND denom -> the global
# duplicated_levels rename), and NAs (tvhours). Warnings from the arg cascade are pre-existing and fire
# identically in both paths, so parity is unaffected.
gss <- forcats::gss_cat
withr::with_seed(1, gss$w <- runif(nrow(gss), 0.5, 2))

tab_seq <- function(...) with_par(FALSE, suppressWarnings(
  tab(gss, c(race, relig, marital, rincome), c(partyid, denom, tvhours), wt = w, ...)))
tab_par <- function(..., workers = 2L) with_par(workers, suppressWarnings(
  tab(gss, c(race, relig, marital, rincome), c(partyid, denom, tvhours), wt = w, ...)))


test_that("parallel build is byte-identical to the serial build", {
  warm_pool(2L)
  expect_identical(tab_par(), tab_seq())
})

test_that("parity holds under na = 'drop' and na = 'drop_all'", {
  warm_pool(2L)
  expect_identical(tab_par(na = "drop"),     tab_seq(na = "drop"))
  expect_identical(tab_par(na = "drop_all"), tab_seq(na = "drop_all"))
})

test_that("option-sensitive leaf math is shipped: anova/stars overrides stay identical", {
  warm_pool(2L)
  withr::local_options(tabxplor.anova = "classic")
  expect_identical(tab_par(), tab_seq())

  withr::local_options(tabxplor.stars = FALSE)
  expect_identical(tab_par(), tab_seq())
})

test_that("below the parallel_min threshold, a parallel request stays serial (and identical)", {
  warm_pool(2L)
  one_seq <- with_par(FALSE, suppressWarnings(tab(gss, race, c(partyid, tvhours), wt = w)))
  one_par <- with_par(2, suppressWarnings(tab(gss, race, c(partyid, tvhours), wt = w)))
  expect_identical(one_par, one_seq)
  # the single row_var (< tabxplor.parallel_min = 2) never dispatched -> pool count untouched
  expect_identical(as.integer(mirai::status(.compute = "tabxplor")$connections), 2L)
})

test_that("the tabxplor pool does not touch the user's default daemon profile", {
  mirai::daemons(1)                                  # user's own default-compute pool
  withr::defer(mirai::daemons(0))
  warm_pool(2L)
  invisible(tab_par())
  expect_identical(as.integer(mirai::status()$connections), 1L)
})

test_that("tab_parallel_stop() shuts the tabxplor pool down", {
  warm_pool(2L)
  invisible(tab_par())
  tab_parallel_stop()
  .pool$n <- NULL                                    # force a re-warm for any later test
  left <- tryCatch(as.integer(mirai::status(.compute = "tabxplor")$connections),
                   error = function(e) 0L)
  expect_identical(left, 0L)
})

test_that("parallel works WITHOUT a manual pre-warm (tab_pool_ensure auto-load_all in dev)", {
  # Regression for the `object 'tab_build_one' not found` crash: with no manual warm_pool(), the pool
  # tab_pool_ensure() spawns on the first >= 2 row_var dispatch must load the current source itself.
  tab_parallel_stop()                                # clean slate: no daemons, no dev load
  .pool$n <- NULL
  expect_error(tab_par(), NA)                        # no crash on the fresh, self-warmed pool
  expect_identical(tab_par(), tab_seq())             # and byte-identical to serial
})

test_that("a worker's messages reach the user, in unit order (Phase 20f)", {
  # A daemon's console is not the user's, so before the tab_pmap() condition relay every
  # cli_inform() / cli_warn() raised inside tab_build_one() was silently DROPPED. Measured on this
  # very call: 2 messages serially, 0 in parallel -- a table that quietly stopped explaining itself.
  #
  # The emitter is tab_transform()'s "several numeric col_vars with different references" notice,
  # which needs a col% regime and two numeric col_vars given DIFFERENT references; it fires once per
  # row_var, so the count is also an order-independent check that no unit was skipped.
  warm_pool(2L)
  gss2 <- gss
  gss2$tvhours2 <- gss2$tvhours

  said <- function(parallel) {
    msgs <- character()
    withCallingHandlers(
      with_par(parallel, tab(gss2, c(race, marital), c(tvhours, tvhours2), pct = "col",
                             ref = c(tvhours = "first", tvhours2 = "tot"))),
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
      warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    grep("Several numeric col_vars", msgs, value = TRUE)
  }

  serial <- said(FALSE)
  expect_length(serial, 2L)                          # the fixture really does emit
  expect_identical(said(2L), serial)                 # text AND order survive the process boundary
})


# === Phase 20f-iii: the three tab_reg() axes ======================================================
# S = several models in ONE table (outcomes / a models list), G = the tab_vars groups, R = the
# outcomes x models-list recursion. All three go through the SAME tab_pmap(), so what is asserted
# here is the same contract as above -- byte-identity, and the worker's conditions reaching the
# user -- on the producer that dispatches whole model fits rather than aggregates.

reg_fx <- local({
  d <- forcats::gss_cat
  d$married <- factor(ifelse(d$marital == "Married", "Married", "Not married"))
  d$party3  <- forcats::fct_lump_n(d$partyid, 2)
  d$year_f  <- factor(d$year)
  d
})

test_that("tab_reg parallel: the S axis (several outcomes in one table) is byte-identical", {
  warm_pool(2L)
  args <- list(reg_fx, c("married", "tvhours"), c("race", "age"), stats = FALSE)
  expect_identical(
    suppressMessages(with_par(2L, do.call(tab_reg, args))),
    suppressMessages(with_par(FALSE, do.call(tab_reg, args))))
})

test_that("tab_reg parallel: the G axis (tab_vars groups) is byte-identical", {
  warm_pool(2L)
  args <- list(reg_fx, "married", "race", family = "binomial", tab_vars = "year_f", stats = FALSE)
  expect_identical(
    suppressMessages(with_par(2L, do.call(tab_reg, args))),
    suppressMessages(with_par(FALSE, do.call(tab_reg, args))))
})

test_that("tab_reg parallel: the R axis (outcomes x a models list) is byte-identical", {
  warm_pool(2L)
  args <- list(reg_fx, c("married", "tvhours"),
               list(m1 = "race", m2 = c("race", "age")), stats = FALSE)
  expect_identical(
    suppressMessages(with_par(2L, do.call(tab_reg, args))),
    suppressMessages(with_par(FALSE, do.call(tab_reg, args))))
})

test_that("tab_reg parallel: a worker's messages reach the user, in unit order", {
  # The family-detection notice fires once per outcome, in outcome order -- so it is both the
  # "did the relay work" check and an order-independent check that no unit was skipped.
  warm_pool(2L)
  said <- function(parallel) {
    msgs <- character()
    withCallingHandlers(
      with_par(parallel, tab_reg(reg_fx, c("married", "tvhours"), "race", stats = FALSE)),
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
    grep("outcome detected", msgs, value = TRUE)
  }
  serial <- said(FALSE)
  expect_length(serial, 2L)
  expect_identical(said(2L), serial)
})

test_that("tab_reg parallel: a shape that must stay serial says so, and is identical", {
  warm_pool(2L)
  args <- list(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
               family = "binomial", stats = "compare_baseline")
  expect_message(with_par(2L, do.call(tab_reg, args)), "one after another")
  expect_identical(
    suppressMessages(with_par(2L, do.call(tab_reg, args))),
    suppressMessages(with_par(FALSE, do.call(tab_reg, args))))
})

test_that("tab_reg parallel: compared models with a crude block DISPATCH (Phase 20f-iiii)", {
  # 20f-iii refused this shape -- spec 1 built the observed block and handed it down the loop. It is
  # the OUTCOME's block now, built by reg_stage_crude() before any model, so nothing is shared
  # between specs. `color = "adjustment"` is the everyday way in: it turns `empirical` on.
  warm_pool(2L)
  args <- list(reg_fx, "married", list(m1 = "race", m2 = c("race", "age")),
               family = "binomial", color = "adjustment", stats = FALSE)
  said <- character()
  withCallingHandlers(with_par(2L, do.call(tab_reg, args)),
                      message = function(m) { said <<- c(said, conditionMessage(m))
                                              invokeRestart("muffleMessage") })
  expect_false(any(grepl("one after another", said)))  # it is NOT refused any more
  expect_identical(
    suppressMessages(with_par(2L, do.call(tab_reg, args))),
    suppressMessages(with_par(FALSE, do.call(tab_reg, args))))
})


# === Phase 20f-iiii: a worker's ERROR ==============================================================
# [.stop] used to re-throw mirai's own wrapper BEFORE tab_pmap() replayed anything, so a failure
# discarded every message the successful units had already produced -- the diagnostics that explain
# it. The trampoline catches its unit's error and returns it, so collection completes.

test_that("a failing unit is NAMED, identically in both branches", {
  warm_pool(2L)
  bad <- reg_fx
  bad$constvar <- factor("only")                     # a one-level predictor: the fit cannot run
  fail <- function(parallel) tryCatch(
    with_par(parallel, suppressMessages(
      tab_reg(bad, "married", list(m1 = "race", m2 = c("race", "constvar")),
              family = "binomial", stats = FALSE))),
    error = conditionMessage)

  serial <- fail(FALSE)
  expect_match(serial, 'Model "m2"')                 # the model's LABEL, not an index
  expect_identical(fail(2L), serial)                 # ...and the process boundary changes nothing
})

test_that("a sibling unit's messages still reach the user when another unit fails", {
  warm_pool(2L)
  bad <- reg_fx
  bad$constvar <- factor("only")
  msgs <- character()
  expect_error(withCallingHandlers(
    with_par(2L, tab_reg(bad, c("married", "tvhours"),
                         list(m1 = "race", m2 = c("race", "constvar")), stats = FALSE)),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }))
  # the family-detection notice of the FIRST outcome was produced before the failure and survives
  expect_true(any(grepl("outcome detected", msgs)))
})
