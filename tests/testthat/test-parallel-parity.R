# Phase 8: tab(parallel=) must be BYTE-IDENTICAL to the serial build. The build is dispatched per
# row_var to a named mirai daemon pool; everything cross-cutting (duplicated_levels rename, the join,
# tab_apply_tests / chi2 / ci) stays on the main process, so parity holds by construction. These
# tests exercise the real daemon path (skipped on CRAN / when mirai is absent).

skip_on_cran()
skip_if_not_installed("mirai")
skip_if_not_installed("pkgload")

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

# Fixture: >=4 row_vars, 2 factor + 1 numeric col_var (exercises tab_transform's numeric + factor branches), a
# weight, a deliberate cross-col_var level collision ("No answer" in partyid AND denom -> the global
# duplicated_levels rename), and NAs (tvhours). Warnings from the arg cascade are pre-existing and fire
# identically in both paths, so parity is unaffected.
gss <- forcats::gss_cat
withr::with_seed(1, gss$w <- runif(nrow(gss), 0.5, 2))

tab_seq <- function(...) suppressWarnings(
  tab(gss, c(race, relig, marital, rincome), c(partyid, denom, tvhours), wt = w,
      parallel = FALSE, ...))
tab_par <- function(..., workers = 2L) suppressWarnings(
  tab(gss, c(race, relig, marital, rincome), c(partyid, denom, tvhours), wt = w,
      parallel = workers, ...))


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
  one_seq <- suppressWarnings(tab(gss, race, c(partyid, tvhours), wt = w, parallel = FALSE))
  one_par <- suppressWarnings(tab(gss, race, c(partyid, tvhours), wt = w, parallel = 2))
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
