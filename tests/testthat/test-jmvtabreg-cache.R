# The jmvtab_reg live-UI fit cache. Tests drive the engine-free jmvtab_reg_build() core (no live
# jamovi session needed) and lock: (1) parity with a direct tab_reg() call; (2) what is and is not in
# the key -- every ESTIMAND argument is a HIT, the model's own arguments (predictors, family, the
# reference relevel) are a MISS; (3) what the store HOLDS -- a distilled record, kilobytes, with no
# fit and no frame, and its footer checks intact. The NULL-path identity for ordinary tab_reg()
# callers is locked by test-tab_reg.R. See CLAUDE.md > roadmap > Phase 22j.

skip_if_not_installed("broom")

# --- helpers ------------------------------------------------------------------------------
# Phase 15d: the per-outcome Model table (the family / outcome_level / trials ARRAYS) replaced the
# single family / trials / inverse options. Phase 20g-i named every jamovi option after the tab_reg()
# argument it drives, so an ARRAY option and this helper's scalar convenience field would now share a
# name -- the convenience ones (which the tests use to build the direct tab_reg() comparison call) are
# `..`-prefixed, which also marks them as "not a jamovi option".
# ⚠ `empirical = FALSE` here is the JAMOVI picker's default, which since 22g-ii is no longer
# tab_reg()'s (that is TRUE). Every parity check therefore passes `empirical = o$empirical` to the
# direct call too: what these tests lock is that the two BUILDS agree, never that the two DEFAULTS
# happen to coincide.
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

gss_reg <- function() gss_cat_data_formatting()

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

test_that("Phase 15e: mixed-family outcomes build ONE table (not a tabxplor_tabs list)", {
  gss <- gss_reg()
  o <- reg_opts(outcome = c("married", "tvhours"), predictors = c("race", "age"))
  o$family <- list(list(var = "married", family = "binomial"),
                      list(var = "tvhours", family = "gaussian"))
  built <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
  expect_false(inherits(built, "tabxplor_tabs"))       # merged, not stacked
  mf <- get_model_family(dplyr::ungroup(built))
  expect_true("binomial" %in% mf && "gaussian" %in% mf)
  # the cached build (.fit_cache present) matches a direct mixed tab_reg()
  direct <- quiet(tab_reg(gss, c("married", "tvhours"), c("race", "age"),
                          family = c("binomial", "gaussian"), empirical = o$empirical,
                          cleannames = TRUE))
  expect_identical(reg_render(built), reg_render(direct))
})

test_that("empirical + weighted builds run and match tab_reg()", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = "married", predictors = c("race", "age"),
                  ..family = "binomial", empirical = TRUE)
  built  <- jmvtab_reg_build(gss, o, NULL)$tabs
  direct <- suppressMessages(tab_reg(gss, "married", c("race", "age"),
                                     family = "binomial", empirical = TRUE, cleannames = TRUE))
  expect_identical(reg_render(built), reg_render(direct))

  skip_if_not_installed("survey")
  gss$w <- 0.5 + (seq_len(nrow(gss)) %% 7) / 4          # deterministic positive weights
  ow    <- reg_opts(outcome = "married", predictors = c("race", "age"),
                    ..family = "binomial", wt = "w")
  bw    <- jmvtab_reg_build(gss, ow, NULL)$tabs
  dw    <- suppressMessages(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                                    wt = "w", empirical = ow$empirical, cleannames = TRUE))
  expect_identical(reg_render(bw), reg_render(dw))
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


# --- 3. cache behaviour: reuse the fit on estimand toggles, refit on a real change ---
test_that("a repeat build reuses the fit, and a reference change does not", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = "married", predictors = c("race", "age"), ..family = "binomial")

  b1 <- jmvtab_reg_build(gss, o, NULL)
  expect_equal(b1$hits, 0L)                                   # first build: all misses

  b2 <- jmvtab_reg_build(gss, o, b1$store)                    # identical opts -> hit
  expect_gte(b2$hits, 1L)

  b3 <- jmvtab_reg_build(gss, reg_opts(ref = c(race = "Black")), b2$store)   # relevel -> miss
  expect_equal(b3$hits, 0L)

  b4 <- jmvtab_reg_build(gss, reg_opts(color = "no"), b3$store)              # estimand -> hit
  expect_gte(b4$hits, 1L)
})

test_that("a NUMERIC reference is a miss too, and its anchor really lands", {
  gss <- gss_reg()
  b1  <- jmvtab_reg_build(gss, reg_opts(predictors = c("race", "age")), NULL)
  b3  <- jmvtab_reg_build(gss, reg_opts(predictors = c("race", "age"),
                                        ref = c(age = "40")), b1$store)
  expect_equal(b3$hits, 0L)
  direct <- quiet(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                          ref = c(age = 40), empirical = FALSE, cleannames = TRUE))
  expect_equal(reg_field(b3$tabs, "or")[[1]], reg_field(direct, "or")[[1]], tolerance = 1e-8)
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
  expect_null(jmvtab_reg_build(gss, reg_opts(outcome = character()), NULL)$tabs)
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

test_that("jmvtab_reg_link_vector folds the Model table's link column", {
  expect_null(jmvtab_reg_link_vector(list()))
  # "auto" IS the default, so an entry for it must NOT reach the argument (it would only move the
  # fit key); a blank one is the same statement.
  expect_null(jmvtab_reg_link_vector(list(list(var = "y", link = "auto"),
                                          list(var = "z", link = ""))))
  expect_identical(
    jmvtab_reg_link_vector(list(list(var = "y", link = "ratio"),
                                list(var = "z", link = "auto"))),
    c(y = "ratio"))
})

test_that("the per-outcome link reaches the fit, and two outcomes may differ", {
  d <- gss_reg()
  # one outcome on the log link: the modified Poisson, so the column IS a risk ratio
  t1 <- quiet(jmvtab_reg_build(d, reg_opts(predictors = "race"))$tabs)
  t2 <- quiet(jmvtab_reg_build(d, reg_opts(predictors = "race", ..link = "ratio"))$tabs)
  expect_true(any(grepl("_OR", names(t1))))
  expect_true(any(grepl("_RR", names(t2))))
  expect_identical(reg_render(t2),
                   reg_render(quiet(tab_reg(d, outcome = "married", predictors = "race",
                                            family = c(married = "binomial"),
                                            link = c(married = "ratio"),
                                            effect = "conditional", ci_method = "wald",
                                            empirical = FALSE,
                                            color_signif = "grey_non_signif"))))
  # ...and the point of putting the link IN the per-outcome table: one table, two links.
  o <- reg_opts(outcome = c("married", "income25k"), predictors = "race")
  o$link <- list(list(var = "married", link = "ratio"))
  t3 <- quiet(jmvtab_reg_build(d, o)$tabs)
  expect_true(any(grepl("_RR", names(t3))) && any(grepl("_OR", names(t3))))
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
  o     <- reg_opts(outcome = "married", predictors = mods, ..family = "binomial")
  built <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
  direct <- quiet(tab_reg(gss, "married", mods, family = "binomial",
                          empirical = o$empirical, cleannames = TRUE))
  expect_identical(reg_render(built), reg_render(direct))
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

# Phase 22g-iii: the panel asks NOTHING about `stats`. Two predictor subsets are compared because
# that is tab_reg()'s own default since 22g-ii -- which is exactly why the picker had to go.
test_that("two predictor subsets are compared with no control at all", {
  gss <- gss_reg()
  o   <- reg_opts(predictors = list(small = c("race", "age"),
                                    full  = c("race", "age", "rincome")),
                  ..family = "binomial", na = "drop_all")
  t   <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
  cmp <- get_test(t) |> dplyr::filter(grepl("^compare", test))
  expect_gte(nrow(cmp), 1L)
})

# Phase 22j: the eager stage computes every fit-based footer row WHILE the fit lives, so a served
# digest carries them -- which is what let Phase 22g-iii delete the `stats_checks` tick-box (it only
# ever existed to turn the cache off). The rows must therefore survive a cache HIT.
test_that("the fit-based footer rows survive being served from the store", {
  gss <- gss_reg()
  b   <- quiet(jmvtab_reg_build(gss, reg_opts(predictors = c("race", "age")), NULL))
  expect_true(all(c("dispersion", "influence") %in% get_test(b$tabs)$test))
  again <- quiet(jmvtab_reg_build(gss, reg_opts(predictors = c("race", "age")), b$store))
  expect_gte(again$hits, 1L)
  expect_true(all(c("dispersion", "influence") %in% get_test(again$tabs)$test))
})

test_that("a comparison list with several dependents yields a NULL table (guarded)", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = c("married", "tvhours"),
                  predictors = list(a = "race", b = c("race", "age")), family = "binomial")
  expect_null(jmvtab_reg_build(gss, o, NULL)$tabs)
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
  # a card with no `crosses` field at all still folds (the shape every card had before 22g-viii)
  expect_identical(jmvtab_reg_models(list(list(label = "x", vars = list("race"))),
                                     c("race", "age"), "race*age"),
                   list(x = "race"))
  # ...and a card holding ONLY an interaction is not empty
  expect_identical(jmvtab_reg_models(list(list(label = "x", vars = list(),
                                               crosses = list("race*age"))),
                                     c("race", "age"), "race*age"),
                   list(x = "race*age"))
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
  o10  <- reg_opts(predictors = c("race", "age"), ..family = "binomial", ..multiplier = c(age = 10))
  o05  <- reg_opts(predictors = c("race", "age"), ..family = "binomial", ..multiplier = c(age = 5))
  b10  <- quiet(jmvtab_reg_build(gss, o10, NULL))
  b05  <- quiet(jmvtab_reg_build(gss, o05, b10$store))       # a different scaling -> a fresh fit, not stale
  expect_false(identical(reg_render(b10$tabs), reg_render(b05$tabs)))
  d10  <- quiet(tab_reg(gss, "married", c("race", "age"),
                        family = "binomial", multiplier = c(age = 10),
                        empirical = o10$empirical, cleannames = TRUE))
  expect_identical(reg_render(b10$tabs), reg_render(d10))
})

# --- Phase h: the staged-comparison gate helpers --------------------------------------------
test_that("Phase h: jmvtab_reg_staged() flags >=2-model comparisons only", {
  pool <- c("race", "age", "rincome")
  # no cards -> the flat pool (single model) -> not staged
  expect_false(jmvtab_reg_staged(list(), pool))
  # one card -> one model -> not staged (fast, stays live)
  expect_false(jmvtab_reg_staged(list(list(label = "a", vars = c("race", "age"))), pool))
  # two cards -> a comparison -> staged (Run-button gated)
  expect_true(jmvtab_reg_staged(
    list(list(label = "a", vars = "race"), list(label = "b", vars = c("race", "age"))), pool))
  # ⚠ a card holding ONLY an interaction has an empty `vars`: without the keys it reads as empty and
  # is dropped, so the predicate and .opts() must be given the SAME ones or a comparison runs live.
  cards <- list(list(label = "a", vars = c("race", "age")),
                list(label = "b", vars = list(), crosses = list("race*age")))
  expect_false(jmvtab_reg_staged(cards, pool))
  expect_true(jmvtab_reg_staged(cards, pool, "race*age"))
})

test_that("Phase h: jmvtab_reg_compare_sig() changes with the options, is stable otherwise", {
  o1 <- list(outcome = "married", predictors = list(a = "race", b = c("race", "age")),
             conf_level = 0.95, color = TRUE)
  o2 <- o1; o2$conf_level <- 0.90
  expect_identical(jmvtab_reg_compare_sig(o1), jmvtab_reg_compare_sig(o1))  # stable
  expect_false(identical(jmvtab_reg_compare_sig(o1), jmvtab_reg_compare_sig(o2)))  # option change -> new sig
})


# --- 8. the scaling picker passes the "sd" keywords through (Phase 18z9) ---------------
test_that("jmvtab_reg_mult_vector() keeps sd / 2sd as text and numbers as numbers", {
  mk <- function(...) list(...)
  expect_null(jmvtab_reg_mult_vector(list()))
  expect_identical(jmvtab_reg_mult_vector(list(mk(var = "age", k = "10"))), c(age = 10))
  expect_identical(jmvtab_reg_mult_vector(list(mk(var = "age", k = " SD "))), c(age = "sd"))
  expect_identical(jmvtab_reg_mult_vector(list(mk(var = "age", k = "2sd"),
                                               mk(var = "tv",  k = "5"))),
                   c(age = "2sd", tv = "5"))
  expect_null(jmvtab_reg_mult_vector(list(mk(var = "age", k = "nonsense"))))
})


# --- Phase 19k: the boundary speaks tab_reg()'s own vocabulary -----------------------------

# `trials`: ONE rule, R's. The module used to take the observed max() ITSELF for any integer
# outcome -- the same rule as `trials = TRUE`, but silently and on a different trigger, so the
# jamovi behaviour was not reproducible from the R API. Now it sends the typed count, or NA =
# "take the observed maximum", which tab_reg() resolves -- and only where there IS one.
test_that("Phase 19k: the trials picker == tab_reg(trials =), explicit and automatic", {
  gss <- gss_reg()
  # (a) a FACTOR binomial outcome: NA resolves to no trials -> an ordinary binary logit, and the
  #     digest fast path stays available (the raw NA used to look like a grouped binomial).
  o  <- reg_opts(outcome = "married", predictors = c("race", "age"), ..family = "binomial")
  expect_true(is.na(jmvtab_reg_dep_trials(list(), "married")))
  b  <- quiet(jmvtab_reg_build(gss, o, NULL))
  expect_equal(reg_render(b$tabs),
               reg_render(quiet(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                                        empirical = o$empirical, cleannames = TRUE,
                                        color_signif = "grey_non_signif"))))
  # (b) a numeric SCORE outcome: TRUE / NA / the observed max are the same table.
  d  <- gss
  d$score <- as.integer(pmin(pmax(round(d$tvhours), 0), 8))
  mx <- max(d$score, na.rm = TRUE)
  auto <- quiet(tab_reg(d, "score", "race", family = "binomial", trials = TRUE,
                        cleannames = TRUE, color_signif = "grey_non_signif"))
  na_v <- quiet(tab_reg(d, "score", "race", family = "binomial", trials = c(score = NA),
                        cleannames = TRUE, color_signif = "grey_non_signif"))
  expl <- quiet(tab_reg(d, "score", "race", family = "binomial", trials = mx,
                        cleannames = TRUE, color_signif = "grey_non_signif"))
  expect_equal(reg_render(auto), reg_render(expl))
  expect_equal(reg_render(na_v), reg_render(expl))
  # (c) `trials = TRUE` no longer errors when SOME outcome has no maximum to take (it used to run
  #     max() on a factor).
  expect_no_error(quiet(tab_reg(d, c("score", "married"), "race", family = "binomial",
                                trials = TRUE, cleannames = TRUE)))
})

# `shape`: the per-predictor functional-form picker folds into tab_reg(shape =).
test_that("Phase 19k: the shape picker folds into tab_reg(shape =)", {
  expect_null(jmvtab_shape_vector(list()))
  expect_null(jmvtab_shape_vector(list(list(var = "age", shape = "linear"))))  # the default
  expect_identical(jmvtab_shape_vector(list(list(var = "age", shape = "quadratic"),
                                                list(var = "tvhours", shape = ""),
                                                list(var = "educ", shape = "log"))),
                   c(age = "quadratic", educ = "log"))
  gss <- gss_reg()
  o <- reg_opts(outcome = "married", predictors = c("race", "age"), ..family = "binomial")
  o$shape <- list(list(var = "age", shape = "quadratic"))
  expect_equal(
    reg_render(quiet(jmvtab_reg_build(gss, o, NULL))$tabs),
    reg_render(quiet(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                             shape = c(age = "quadratic"), empirical = o$empirical,
                             cleannames = TRUE, color_signif = "grey_non_signif"))))
})

# the estimand pair reaches tab_reg() untranslated (jmv_reg_estimand_opts() is deleted)
test_that("Phase 19k: effect x measure x display pass straight through", {
  gss <- gss_reg()
  o <- reg_opts(outcome = "married", predictors = c("race", "age"), ..family = "binomial",
                effect = "conditional", measure = "log", display = "auto")
  expect_equal(
    reg_render(quiet(jmvtab_reg_build(gss, o, NULL))$tabs),
    reg_render(quiet(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                             effect = "conditional", measure = "log", empirical = o$empirical,
                             cleannames = TRUE, color_signif = "grey_non_signif"))))
  # a reg colour MEASURE (D25's surviving allow-list), not a checkbox
  o2 <- reg_opts(outcome = "married", predictors = c("race", "age"), ..family = "binomial",
                 empirical = TRUE, color = "adjustment")
  expect_no_error(quiet(jmvtab_reg_build(gss, o2, NULL)))
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


# --- Phase 20g-ii: level MERGING on a predictor --------------------------------------------------
# Same tab_collapse_levels() the crosstab uses, applied in reg_prepare_data()'s stage G0 -- before
# family detection, the reference relevel and the skeleton, so a merged level is simply a level.

test_that("Phase 20g-ii: a merged predictor is tab_reg() on a pre-collapsed frame", {
  gss <- gss_reg()
  sp  <- list(list(var = "marital", label = "Not married",
                   levels = list("Never married", "Divorced", "Separated")))
  pre <- dplyr::mutate(gss, marital = forcats::fct_collapse(
    marital, `Not married` = c("Never married", "Divorced", "Separated")))
  # ⚠ NOT `married`: the fixture derives it FROM marital, so a merged predictor would be collinear
  # with the outcome. `black` is independent of it.
  o   <- reg_opts(outcome = "black", predictors = c("marital", "age"), ..family = "binomial")
  a <- quiet(jmvtab_reg_build(gss, utils::modifyList(o, list(levels_collapse = sp)), NULL))$tabs
  b <- quiet(jmvtab_reg_build(pre, o, NULL))$tabs
  expect_equal(reg_render(a), reg_render(b))
  expect_true("Not married" %in% as.character(a$levels))
  expect_false("Divorced" %in% as.character(a$levels))
})

test_that("Phase 20g-ii: a merge changes the fit key, so the cached fit is not reused", {
  gss <- gss_reg()
  sp  <- list(list(var = "marital", label = "Not married",
                   levels = list("Never married", "Divorced", "Separated")))
  o   <- reg_opts(outcome = "black", predictors = c("marital", "age"), ..family = "binomial")
  s <- quiet(jmvtab_reg_build(gss, o, NULL))$store
  # a re-run of the SAME call hits (the control), while the merged one must not
  expect_gte(quiet(jmvtab_reg_build(gss, o, s))$hits, 1L)
  r <- quiet(jmvtab_reg_build(gss, utils::modifyList(o, list(levels_collapse = sp)), s))
  # jmvreg_fit_key() fingerprints the PREPARED frame's levels, and the merge runs before any fit --
  # so it invalidates by construction, with no cache code of its own.
  expect_equal(r$hits, 0L)
})


# --- Phase 22g-iii ------------------------------------------------------------------------------
# ONE predictor subset beside several outcomes is a per-outcome table, not a comparison -- and
# `is_comparison <- is.list(predictors)` would refuse the second outcome if the card still arrived
# as a list. TWO subsets and two outcomes is still refused: there is no such table.
test_that("one predictor subset builds with several outcomes; two subsets still refuse", {
  cards <- list(list(label = "m1", vars = c("race", "age")))
  expect_identical(jmvtab_reg_models(cards, c("race", "age"), flatten = TRUE), c("race", "age"))
  expect_true(is.list(jmvtab_reg_models(cards, c("race", "age"), flatten = FALSE)))
  two <- c(cards, list(list(label = "m2", vars = "race")))
  expect_true(is.list(jmvtab_reg_models(two, c("race", "age"), flatten = TRUE)))

  gss <- gss_reg()
  o   <- reg_opts(outcome = c("married", "black"), ..family = "binomial",
                  predictors = c("race", "age"))
  o$models <- cards
  o$predictors <- jmvtab_reg_models(cards, c("race", "age"), flatten = TRUE)
  t <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
  expect_false(is.null(t))
  expect_gte(sum(grepl("^Model", names(t))), 2L)     # one model column block per outcome
})

# `measure = "coefficient"` is TOTAL: it answers for a gaussian outcome too, where "log" was refused.
test_that("`measure = \"coefficient\"` builds on a mixed-family table", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = c("married", "tvhours"), ..family = "auto",
                  predictors = "race", measure = "coefficient", effect = "auto",
                  empirical = FALSE)
  t <- quiet(jmvtab_reg_build(gss, o, NULL))$tabs
  expect_false(is.null(t))
  expect_gte(sum(grepl("^Model", names(t))), 2L)
})
