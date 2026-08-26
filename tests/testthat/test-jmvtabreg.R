# PURPOSE: the regression fit-digest cache: what is in the key, and what is served.
# ROLE: the shipped CONTRACT for R/jmvtabreg-cache.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the regression fit cache ============================================================

reg_opts <- function(...) {
  o <- utils::modifyList(list(
    outcome = "married", predictors = c("race", "age"), wt = character(),
    tab_vars = NULL, ..family = "binomial",
    effect = "conditional", display = "auto",
    empirical = FALSE, ref = NULL, conf_level = 0.95,
    ci_method = "wald", stars = TRUE, color = NULL, color_signif = "grey_non_signif",
    na = "drop_by_outcome", cleannames = TRUE, n = "range", subtext = "",
    ..multiplier = NULL, ..trials = NULL, ..link = NULL
  ), list(...))
  # derive the Model-table arrays from the convenience fields
  o$family        <- if (identical(o$..family, "auto")) list()
                     else lapply(o$outcome, function(d) list(var = d, family = o$..family))
  o$link          <- if (is.null(o$..link)) list()
                     else lapply(o$outcome, function(d) list(var = d, link = o$..link))
  o$outcome_level <- list()
  o$trials        <- if (is.null(o$..trials)) list()
                     else lapply(o$outcome, function(d) list(var = d, n = as.character(o$..trials)))
  o$multiplier    <- if (is.null(o$..multiplier)) list()
                     else Map(function(v, k) list(var = v, k = as.character(k)),
                              names(o$..multiplier), unname(o$..multiplier))
  o
}


gss_reg <- function() fx_reg_fmt()


# poisson tvhours is (legitimately) over-dispersed -> reg_fit warns; that is expected here.
quiet <- function(expr) suppressWarnings(suppressMessages(expr))


# The user-visible content: the formatted display strings of every fmt column + the row labels. Two
# builds are "the same table" iff these match exactly.
reg_render <- function(tab) {
  tab      <- dplyr::ungroup(tab)
  fmt_cols <- names(tab)[vapply(tab, is_fmt, logical(1))]
  cells    <- lapply(fmt_cols, function(nm) format(tab[[nm]]))
  list(cells = cells, names = fmt_cols,
       levels = as.character(tab$levels), var = as.character(tab$var))
}


reg_field <- function(tab, field) {
  tab      <- dplyr::ungroup(tab)
  fmt_cols <- names(tab)[vapply(tab, is_fmt, logical(1))]
  unlist(lapply(fmt_cols, function(nm)
    tryCatch(as.numeric(vctrs::field(tab[[nm]], field)), error = function(e) NA_real_)))
}



# --- 1. jmvtab_reg_build parity with a direct tab_reg() -----------------------------------
test_that("jmvtab_reg_build == tab_reg(), each GLM family", {
  gss <- gss_reg()
  cases <- list(
    binomial = reg_opts(outcome = "married", predictors = c("race", "age"), ..family = "binomial"),
    gaussian = reg_opts(outcome = "tvhours", predictors = c("race", "relig"), ..family = "gaussian"),
    poisson  = reg_opts(outcome = "tvhours", predictors = c("race", "relig"), ..family = "poisson")
  )
  for (nm in names(cases)) {
    o      <- cases[[nm]]
    built  <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
    direct <- quiet(tab_reg(gss, o$outcome, o$predictors, family = o$..family,
                            empirical = o$empirical, cleannames = TRUE))
    expect_identical(reg_render(built), reg_render(direct), info = nm)
  }
})



# --- 2. a distilled record serves every ESTIMAND, and only the model is keyed ---------------
# Phase 22j retired the reference-reparametrization engine: a reference change relevels the data
# before the fit, so it moves jmv_col_fp()'s fingerprint and is an honest refit. What is cached
# instead is the FIT, distilled -- so the estimand arguments are the ones that must not move the key.
test_that("a reference change is a miss, and equals a direct tab_reg()", {
  gss <- gss_reg()
  grid <- list(
    list(dep = "married", preds = c("race", "age"),   fam = "binomial", ref = c(race = "Black")),
    list(dep = "tvhours", preds = c("race", "relig"), fam = "gaussian", ref = c(race = "Black")),
    list(dep = "tvhours", preds = c("race", "relig"), fam = "poisson",  ref = c(relig = "8-None")),
    list(dep = "married", preds = c("race", "age"), fam = "binomial",
         ref = c(race = "Black"), mult = c(age = 10))
  )
  for (g in grid) {
    o     <- reg_opts(outcome = g$dep, predictors = g$preds, ..family = g$fam, ref = g$ref,
                      ..multiplier = g$mult)
    built <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
    refit <- quiet(tab_reg(gss, g$dep, g$preds, family = g$fam, ref = g$ref,
                           multiplier = g$mult, empirical = o$empirical, cleannames = TRUE))
    expect_identical(reg_render(built), reg_render(refit),
                     info = paste(g$fam, paste(names(g$ref), collapse = "+")))
  }
})


test_that("the estimand is NOT in the key: measure / effect / display / colour are hits", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = "married", predictors = c("race", "age"), ..family = "binomial")
  b1  <- jmvtab_reg_build(gss, o, NULL)
  expect_equal(b1$hits, 0L)
  st  <- b1$store
  for (opts in list(reg_opts(display = "est_ci"), reg_opts(color = "no"),
                    reg_opts(conf_level = 0.90), reg_opts(stars = FALSE),
                    reg_opts(effect = "marginal", measure = "difference"))) {
    b  <- jmvtab_reg_build(gss, opts, st)
    expect_gte(b$hits, 1L)
    st <- b$store
  }
})


# THE POINT OF THE PHASE: kilobytes, not megabytes, and the footer survives the distillation.
test_that("the store holds a distilled record: KB, no fit, and the checks still there", {
  gss <- gss_reg()
  b   <- jmvtab_reg_build(gss, reg_opts(outcome = "married", predictors = c("race", "age"),
                                        ..family = "binomial"), NULL)
  expect_lt(as.numeric(utils::object.size(b$store)), 1024L * 1024L)   # < 1 MB, was 6-16 MB
  recs    <- purrr::map(b$store[["fit"]], "value")
  expect_gt(length(recs), 0L)
  for (r in recs) {
    expect_null(r[["fit"]]); expect_null(r[["data"]]); expect_null(r[["tidy"]])
    expect_s3_class(r$digest, "tabxplor_fitdigest")
  }
  # the model checks are computed while the fit lives, so a served record still carries them
  tst <- get_test(jmvtab_reg_build(gss, reg_opts(outcome = "married",
                                                 predictors = c("race", "age"),
                                                 ..family = "binomial"), b$store)$tabs)
  expect_true(all(c("dispersion", "collinearity") %in% tst$test))
})



# --- 2b. the CRUDE block's own records (Phase 22g-x) ---------------------------------------
# The observed companion's univariable refits ride the SAME store and the SAME tier as the model
# fit -- a crude fit IS a fit record, told apart by its key alone. Only a NON-SATURATED predictor
# is fitted at all (a numeric one, or any under a structured design / an ordinal shape), so an
# all-factor block on a flat design adds no record and costs nothing.
test_that("a crude fit is served from the store, and the served table is the cold one", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = "married", predictors = c("race", "age"), empirical = TRUE)
  b1  <- quiet(jmvtab_reg_build(gss, o, NULL))
  expect_equal(b1$hits, 0L)
  # one record for the model, one for `age` -- `race` is saturated, so it is a closed form
  expect_equal(length(b1$store[["fit"]]), 2L)
  b2 <- quiet(jmvtab_reg_build(gss, o, b1$store))
  expect_gte(b2$hits, 2L)
  expect_equal(reg_render(b2$tabs), reg_render(b1$tabs))
})


test_that("the store migrates on a NULL / schema mismatch", {
  expect_identical(jmvreg_cache_migrate(NULL)$schema, JMVREG_CACHE_SCHEMA)
  expect_identical(jmvreg_cache_migrate(list(schema = 999L))$schema, JMVREG_CACHE_SCHEMA)
  s <- jmvreg_cache_new()
  expect_identical(jmvreg_cache_migrate(s), s)
})


test_that("the store round-trips through serialization (jamovi $state)", {
  gss <- gss_reg()
  b1  <- jmvtab_reg_build(gss, reg_opts(), NULL)
  rt  <- unserialize(serialize(b1$store, NULL))
  b2  <- jmvtab_reg_build(gss, reg_opts(), rt)                # reuse across a serialize round-trip
  expect_gte(b2$hits, 1L)
})


test_that("Phase o: use_cache = FALSE builds the same table but persists NO store (the freeze fix)", {
  gss  <- gss_reg()
  mods <- list(demo = c("race", "age"), full = c("race", "age", "rincome"))
  o    <- reg_opts(outcome = "married", predictors = mods, ..family = "binomial")

  cached   <- quiet(jmvtab_reg_build(gss, o, NULL, use_cache = TRUE))
  uncached <- quiet(jmvtab_reg_build(gss, o, NULL, use_cache = FALSE))

  # identical user-visible table -- bypassing the cache changes nothing but what is stored
  expect_identical(reg_render(uncached$tabs), reg_render(cached$tabs))
  # the ~10 MB-per-model raw fits are NOT persisted -> nothing heavy re-serializes into $state
  expect_null(uncached$store)
  expect_false(is.null(cached$store))
})


# THE PRICE OF THE DEFAULT COMPARISON, locked so it cannot change unnoticed (Phase 22g-ii): several
# `predictors` sets now compare automatically, an LR / F / Wald test is a test BETWEEN two fitted
# objects, and reg_fit_cacheable() therefore refuses the store. So a comparison panel re-fits on every
# live edit -- which is what the staged Run button (jmvtab_reg_staged) already exists for -- while an
# ordinary single-model panel, the one the store was built for, keeps every hit (tests above).
test_that("a model comparison keeps its fits, so nothing is cached", {
  gss  <- gss_reg()
  mods <- list(demo = c("race", "age"), full = c("race", "age", "rincome"))
  o    <- reg_opts(outcome = "married", predictors = mods, ..family = "binomial")

  b1 <- quiet(jmvtab_reg_build(gss, o, NULL))
  expect_equal(b1$hits, 0L)
  b2 <- quiet(jmvtab_reg_build(gss, o, b1$store))
  expect_equal(b2$hits, 0L)
  # ...and the footer does carry the comparison it paid for
  expect_true(any(grepl("compare", get_test(b2$tabs)$test)))

  # adding a model still builds every one of them, and the table grows by one block
  mods3 <- c(mods, list(age_only = "age"))
  b3 <- quiet(jmvtab_reg_build(gss, reg_opts(outcome = "married", predictors = mods3,
                                             ..family = "binomial"), b2$store))
  expect_equal(b3$hits, 0L)
  expect_gt(sum(vapply(b3$tabs, is_fmt, logical(1))),
            sum(vapply(b2$tabs, is_fmt, logical(1))))

  # ...but its CRUDE block is served (Phase 22g-x): a univariable fit takes part in no comparison,
  # so only the profile clause can refuse it -- which is the whole reason the gate is two functions.
  oe <- reg_opts(outcome = "married", predictors = mods, ..family = "binomial", empirical = TRUE)
  e1 <- quiet(jmvtab_reg_build(gss, oe, NULL))
  e2 <- quiet(jmvtab_reg_build(gss, oe, e1$store))
  expect_gte(e2$hits, 1L)
  expect_equal(reg_render(e2$tabs), reg_render(e1$tabs))
})


# --- the module never dispatches (Phase 20f-iiii) ------------------------------------------------
# `options(tabxplor.parallel)` is the one switch, and the jamovi bridge must always turn it off: the
# UI repaints on every click, and a daemon pool inside jamovi's own R process is a cost with no payoff.

test_that("Phase 20f-iiii: jmvtab_reg_build() is serial in BOTH cache modes", {
  # (a) the live cache forces it: a cache_env is the tab_parallel_workers() escape hatch
  withr::local_options(tabxplor.parallel = 8L)
  expect_identical(tab_parallel_workers(cache_env = new.env()), 0L)

  # (b) ...but STAGED mode passes .fit_cache = NULL, so the bridge must say so itself -- otherwise
  # the build reads the option. Both modes must ignore a globally-set option.
  skip_if_not_installed("mirai")
  pool_n <- function() tryCatch(as.integer(mirai::status(.compute = "tabxplor")$connections),
                                error = function(e) 0L)
  gss <- gss_reg()
  o   <- reg_opts(outcome = "married", predictors = c("race", "age"), ..family = "binomial")
  withr::local_options(tabxplor.parallel = 4L)
  for (use_cache in c(TRUE, FALSE)) {
    before <- pool_n()
    invisible(quiet(jmvtab_reg_build(gss, o, NULL, use_cache = use_cache)))
    expect_identical(pool_n(), before,
                     info = paste("no daemon was spawned, use_cache =", use_cache))
  }
})
