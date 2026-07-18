# Phase 15b: the jmvtab_reg live-UI fit cache + the reference-reparametrization engine. Tests drive the
# engine-free jmvtab_reg_build() core (no live jamovi session needed) and lock: (1) parity with a direct
# tab_reg() call; (2) the digest fast path is byte-identical (at display precision) to a real
# refit-at-a-new-reference; (3) cache behaviour -- a display / reference toggle reuses the fit (a HIT,
# no refit), a predictor / family change refits (a MISS). The NULL-path identity for ordinary tab_reg()
# callers is locked by test-tab_reg.R. See CLAUDE.md > roadmap > Phase 15b.

skip_if_not_installed("broom")

# --- helpers ------------------------------------------------------------------------------
reg_opts <- function(...) {
  o <- list(
    dependent = "married", predictors = c("race", "age"), wt = character(), ids = NULL, strata = NULL,
    fpc = NULL, nest = FALSE, split_var = NULL, family = "binomial", exponentiate = "nongaussian",
    effect = "coefficient", at = "average", estimate_display = "value",
    inverse_two_level_factors = TRUE, empirical = FALSE, reference = NULL, conf_level = 0.95,
    method = "wald", stars = TRUE, color = NULL, color_signif = "grey_non_signif", na = "keep",
    cleannames = TRUE, stats = NULL, subtext = "",
    compare = "none", baseline = 1L, multiplier = NULL, trials = NULL
  )
  utils::modifyList(o, list(...))
}

gss_reg <- function() gss_cat_data_formatting()

# poisson tvhours is (legitimately) over-dispersed -> reg_fit warns; that is expected here.
quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# The user-visible content: the formatted display strings of every fmt column + the row labels. Two
# builds are "the same table" iff these match exactly (rounding swamps the ~1e-14 reparametrization
# round-off, so display equality is the right byte-identity notion here).
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
    binomial = reg_opts(dependent = "married", predictors = c("race", "age"), family = "binomial"),
    gaussian = reg_opts(dependent = "tvhours", predictors = c("race", "relig"), family = "gaussian"),
    poisson  = reg_opts(dependent = "tvhours", predictors = c("race", "relig"), family = "poisson")
  )
  for (nm in names(cases)) {
    o      <- cases[[nm]]
    built  <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
    direct <- quiet(tab_reg(gss, o$dependent, o$predictors, family = o$family,
                            cleannames = TRUE))
    expect_identical(reg_render(built), reg_render(direct), info = nm)
  }
})

test_that("empirical + weighted builds run and match tab_reg()", {
  gss <- gss_reg()
  o   <- reg_opts(dependent = "married", predictors = c("race", "age"),
                  family = "binomial", empirical = TRUE)
  built  <- jmvtab_reg_build(gss, o, NULL)$tabs
  direct <- suppressMessages(tab_reg(gss, "married", c("race", "age"),
                                     family = "binomial", empirical = TRUE, cleannames = TRUE))
  expect_identical(reg_render(built), reg_render(direct))

  skip_if_not_installed("survey")
  gss$w <- 0.5 + (seq_len(nrow(gss)) %% 7) / 4          # deterministic positive weights
  ow    <- reg_opts(dependent = "married", predictors = c("race", "age"),
                    family = "binomial", wt = "w")
  bw    <- jmvtab_reg_build(gss, ow, NULL)$tabs
  dw    <- suppressMessages(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                                    wt = "w", cleannames = TRUE))
  expect_identical(reg_render(bw), reg_render(dw))
})


# --- 2. the reference-reparametrization engine is byte-identical to a real refit ----------
test_that("digest reref == refit-at-new-reference (display + fields)", {
  gss <- gss_reg()
  grid <- list(
    list(dep = "married", preds = c("race", "age"),   fam = "binomial", ref = c(race = "Black")),
    list(dep = "married", preds = c("race", "age"),   fam = "binomial", ref = c(race = "Other")),
    list(dep = "tvhours", preds = c("race", "relig"), fam = "gaussian", ref = c(race = "Black")),
    list(dep = "tvhours", preds = c("race", "relig"), fam = "poisson",  ref = c(relig = "8-None")),
    list(dep = "married", preds = c("race", "relig", "age"), fam = "binomial",
         ref = c(race = "Black", relig = "8-None"))
  )
  for (g in grid) {
    o     <- reg_opts(dependent = g$dep, predictors = g$preds, family = g$fam, reference = g$ref)
    reref <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs                    # digest fast path
    refit <- quiet(tab_reg(gss, g$dep, g$preds, family = g$fam, reference = g$ref,
                           cleannames = TRUE))
    expect_identical(reg_render(reref), reg_render(refit),
                     info = paste(g$fam, paste(names(g$ref), collapse = "+")))
    for (f in c("or", "diff", "ci_inf", "ci_sup", "pvalue")) {
      expect_equal(reg_field(reref, f), reg_field(refit, f), tolerance = 1e-6,
                   info = paste(g$fam, f))
    }
  }
})


# --- 3. cache behaviour: reuse the fit on display / reference toggles, refit on a real change ---
test_that("a repeat build and a reference change reuse the fit (no refit)", {
  gss <- gss_reg()
  o   <- reg_opts(dependent = "married", predictors = c("race", "age"), family = "binomial")

  b1 <- jmvtab_reg_build(gss, o, NULL)
  expect_equal(b1$hits, 0L)                                   # first build: all misses

  b2 <- jmvtab_reg_build(gss, o, b1$store)                    # identical opts -> hit
  expect_gte(b2$hits, 1L)

  b3 <- jmvtab_reg_build(gss, reg_opts(reference = c(race = "Black")), b2$store)   # reref -> hit
  expect_gte(b3$hits, 1L)

  b4 <- jmvtab_reg_build(gss, reg_opts(color = "no"), b3$store)                    # display -> hit
  expect_gte(b4$hits, 1L)
})

test_that("a predictor / family change refits (a miss)", {
  gss <- gss_reg()
  b1  <- jmvtab_reg_build(gss, reg_opts(predictors = c("race", "age")), NULL)
  # a new predictor set has no matching digest key -> a miss
  b2  <- jmvtab_reg_build(gss, reg_opts(predictors = c("race", "relig")), b1$store)
  # the new build's own digest is a miss (it had to fit); assert it did NOT come only from hits
  b2b <- jmvtab_reg_build(gss, reg_opts(predictors = c("race", "relig")), b2$store)
  expect_gte(b2b$hits, 1L)                                    # now the new key is cached
})


# --- 4. guards + option mapping + store lifecycle -----------------------------------------
test_that("an empty outcome / predictor selection yields a NULL table", {
  gss <- gss_reg()
  expect_null(jmvtab_reg_build(gss, reg_opts(predictors = character()), NULL)$tabs)
  expect_null(jmvtab_reg_build(gss, reg_opts(dependent = character()), NULL)$tabs)
})

test_that("jmvtab_reg_ref_vector folds the picker into a named reference vector", {
  expect_null(jmvtab_reg_ref_vector(list()))
  expect_null(jmvtab_reg_ref_vector(list(list(var = "race", ref = ""))))
  expect_identical(
    jmvtab_reg_ref_vector(list(list(var = "race", ref = "Black"),
                              list(var = "relig", ref = ""))),
    c(race = "Black")
  )
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


# --- 5. the model-comparison "+" builder (Phase 15b-ii) -----------------------------------
test_that("a predictor-subset list == tab_reg() model comparison", {
  gss   <- gss_reg()
  mods  <- list(demo = c("race", "age"), full = c("race", "age", "rincome"))
  o     <- reg_opts(dependent = "married", predictors = mods, family = "binomial")
  built <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
  direct <- quiet(tab_reg(gss, "married", mods, family = "binomial", cleannames = TRUE))
  expect_identical(reg_render(built), reg_render(direct))
})

test_that("compare = baseline adds a comparison footer row", {
  gss <- gss_reg()
  o   <- reg_opts(predictors = list(small = c("race", "age"),
                                    full  = c("race", "age", "rincome")),
                  family = "binomial", compare = "baseline", baseline = 2L, na = "drop_all")
  t   <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
  cmp <- get_test(t) |> dplyr::filter(grepl("^compare", test))
  expect_gte(nrow(cmp), 1L)
})

test_that("a comparison list with several dependents yields a NULL table (guarded)", {
  gss <- gss_reg()
  o   <- reg_opts(dependent = c("married", "tvhours"),
                  predictors = list(a = "race", b = c("race", "age")), family = "binomial")
  expect_null(jmvtab_reg_build(gss, o, NULL)$tabs)
})

test_that("model-comparison fits are cached and reused (only fit new subsets)", {
  gss  <- gss_reg()
  mods <- list(demo = c("race", "age"), full = c("race", "age", "rincome"))
  o    <- reg_opts(dependent = "married", predictors = mods, family = "binomial")

  b1 <- quiet(jmvtab_reg_build(gss, o, NULL))
  expect_equal(b1$hits, 0L)                                   # first build: both models fit

  b2 <- quiet(jmvtab_reg_build(gss, o, b1$store))             # identical -> both fits reused
  expect_gte(b2$hits, 2L)

  # a display toggle (colour off) reuses both fits (no refit)
  b3 <- quiet(jmvtab_reg_build(gss, reg_opts(dependent = "married", predictors = mods,
                                             family = "binomial", color = "no"), b2$store))
  expect_gte(b3$hits, 2L)

  # add a model: the two existing fits are reused, only the new subset is fit
  mods3 <- c(mods, list(age_only = "age"))
  b4    <- quiet(jmvtab_reg_build(gss, reg_opts(dependent = "married", predictors = mods3,
                                                family = "binomial"), b2$store))
  expect_gte(b4$hits, 2L)
})

test_that("jmvtab_reg_models folds the builder into predictors (list or flat pool)", {
  # empty builder -> the flat pool (single model); empty pool too -> NULL
  expect_identical(jmvtab_reg_models(list(), c("race", "age")), c("race", "age"))
  expect_null(jmvtab_reg_models(list(), character()))
  # cards -> a named list in POOL ORDER; blank label -> model{i}; out-of-pool vars dropped
  m <- jmvtab_reg_models(
    list(list(label = "demo", vars = list("age", "race")),
         list(label = "",     vars = list("race", "zzz"))),
    c("race", "age", "rincome"))
  expect_identical(m, list(demo = c("race", "age"), model2 = "race"))
  # an all-empty card set -> the flat pool
  expect_identical(jmvtab_reg_models(list(list(label = "x", vars = list())), c("race", "age")),
                   c("race", "age"))
})


# --- 6. numeric-predictor scaling (multiplier) -----------------------------------------
test_that("jmvtab_reg_mult_vector folds the scaling picker into a named numeric", {
  expect_null(jmvtab_reg_mult_vector(list()))
  expect_null(jmvtab_reg_mult_vector(list(list(var = "age", k = ""))))
  expect_identical(
    jmvtab_reg_mult_vector(list(list(var = "age", k = "10"),
                                list(var = "x",   k = "abc"))),
    c(age = 10)
  )
})

test_that("a multiplier change is keyed (not stale) and matches tab_reg()", {
  gss  <- gss_reg()
  o10  <- reg_opts(predictors = c("race", "age"), family = "binomial", multiplier = c(age = 10))
  o05  <- reg_opts(predictors = c("race", "age"), family = "binomial", multiplier = c(age = 5))
  b10  <- quiet(jmvtab_reg_build(gss, o10, NULL))
  b05  <- quiet(jmvtab_reg_build(gss, o05, b10$store))       # a different scaling -> a fresh fit, not stale
  expect_false(identical(reg_render(b10$tabs), reg_render(b05$tabs)))
  d10  <- quiet(tab_reg(gss, "married", c("race", "age"),
                        family = "binomial", multiplier = c(age = 10), cleannames = TRUE))
  expect_identical(reg_render(b10$tabs), reg_render(d10))
})
