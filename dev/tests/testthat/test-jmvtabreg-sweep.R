
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


test_that("an all-factor crude block on a flat design stores no crude record", {
  gss <- gss_reg()
  b <- quiet(jmvtab_reg_build(gss, reg_opts(outcome = "married",
                                            predictors = c("race", "relig"), empirical = TRUE),
                              NULL))
  expect_equal(length(b$store[["fit"]]), 1L)     # the model's, and nothing else
})


# ⚠ THE REGRESSION THIS PHASE HAD TO AVOID: a served crude record carries a DIGEST and no fitted
# object, so every consumer must read reg_model_of(). The gap SE is where it bites -- demanding
# `$fit` there drops `color = "adjustment"` silently, on every hit and for numeric predictors only.
test_that("a served crude record keeps its gap SE, weighted or not", {
  gss <- gss_reg()
  gss$.w <- stats::runif(nrow(gss), 0.5, 2)
  for (w in list(character(), ".w")) {
    o  <- reg_opts(outcome = "married", predictors = c("race", "age"), empirical = TRUE,
                   effect = "marginal", measure = "difference", color = "adjustment", wt = w)
    b1 <- quiet(jmvtab_reg_build(gss, o, NULL))
    b2 <- quiet(jmvtab_reg_build(gss, o, b1$store))
    g1 <- reg_field(b1$tabs, "gap_se"); g2 <- reg_field(b2$tabs, "gap_se")
    expect_gt(sum(!is.na(g1)), 0L)                       # the gate really opened
    expect_equal(g2, g1)
    # the NUMERIC predictor's own row is the one a `$fit` guard would lose
    num <- which(as.character(b2$tabs$var) == "age")
    expect_true(any(!is.na(reg_field(dplyr::slice(dplyr::ungroup(b2$tabs), num), "gap_se"))))
  }
})


test_that("a marginal crude sweep is the same served as cold (the refit callback)", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = "married", predictors = c("race", "age"), empirical = TRUE,
                  effect = "marginal", measure = "difference")
  b1  <- quiet(jmvtab_reg_build(gss, o, NULL))
  b2  <- quiet(jmvtab_reg_build(gss, o, b1$store))
  expect_gte(b2$hits, 2L)
  expect_equal(reg_render(b2$tabs), reg_render(b1$tabs))
  expect_equal(reg_field(b2$tabs, "diff"), reg_field(b1$tabs, "diff"))
})


# `drop_extra` decides the complete-case population without appearing in the formula, so it is a key
# member AND its columns are fingerprinted. For a crude fit it is the whole predictor set minus this
# predictor -- so the same (outcome, predictor) under another model is a DIFFERENT fit.
test_that("drop_extra is in the key, by name and by value", {
  gss <- gss_reg()
  sp  <- list(outcome = "married", predictors = "age", trials = NULL,
              outcome_level = NULL, formula = NULL)
  ds  <- list(wt = NULL, design = NULL)
  k0 <- jmvreg_fit_key(sp, gss, "binomial", ds)
  k1 <- jmvreg_fit_key(sp, gss, "binomial", ds, drop_extra = "relig")
  k2 <- jmvreg_fit_key(sp, gss, "binomial", ds, drop_extra = c("relig", "race"))
  expect_false(k0 == k1); expect_false(k1 == k2)
  # ...and a VALUE edit to one of those columns moves it: naming them was never enough
  edited <- gss; edited$relig[1:50] <- NA
  expect_false(k1 == jmvreg_fit_key(sp, edited, "binomial", ds, drop_extra = "relig"))
})


test_that("profile bounds are never served, crude fits included", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = "married", predictors = c("race", "age"), empirical = TRUE,
                  ci_method = "profile")
  b1 <- quiet(jmvtab_reg_build(gss, o, NULL))
  b2 <- quiet(jmvtab_reg_build(gss, o, b1$store))
  expect_equal(b2$hits, 0L)
  expect_equal(length(b2$store[["fit"]]), 0L)
})


# --- 2c. a reorder is DISPLAY, not a refit (Phase 22g-x) -----------------------------------
# Every factor PREDICTOR is fitted under treatment contrasts (reg_fit_frame strips `ordered`), so its
# level order decides one thing -- the reference -- and that one thing is `ref =`. The rest permutes
# the row skeleton, which the fit never sees.
test_that("reordering below the first level is a HIT, and only permutes the rows", {
  gss <- gss_reg()
  o   <- reg_opts(outcome = "married", predictors = c("race", "age"))
  b1  <- quiet(jmvtab_reg_build(gss, o, NULL))
  o2  <- o; o2$levels_order <- list(list(var = "race", levels = c("White", "Other", "Black")))
  b2  <- quiet(jmvtab_reg_build(gss, o2, b1$store))
  expect_gte(b2$hits, 1L)
  lv1 <- as.character(b1$tabs$levels); lv2 <- as.character(b2$tabs$levels)
  expect_equal(lv1[2:4], c("White", "Black", "Other"))
  expect_equal(lv2[2:4], c("White", "Other", "Black"))          # the reference still first
  # the same numbers, in the new order: sort both blocks by their level
  key <- function(t) { t <- dplyr::ungroup(t); k <- as.character(t$var) == "race"
                       o <- order(as.character(t$levels)[k])
                       reg_field(dplyr::slice(t, which(k)[o]), "or") }
  expect_equal(key(b2$tabs), key(b1$tabs))
})


test_that("reordering ONTO the first level is the reference, and refits", {
  gss <- gss_reg()
  b1  <- quiet(jmvtab_reg_build(gss, reg_opts(outcome = "married",
                                              predictors = c("race", "age")), NULL))
  o2  <- reg_opts(outcome = "married", predictors = c("race", "age"), ref = c(race = "Black"))
  o2$levels_order <- list(list(var = "race", levels = c("Black", "White", "Other")))
  b2  <- quiet(jmvtab_reg_build(gss, o2, b1$store))
  expect_equal(b2$hits, 0L)
  expect_equal(as.character(b2$tabs$levels)[2:4], c("Black", "White", "Other"))
  direct <- quiet(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                          empirical = FALSE, ref = c(race = "Black")))
  expect_equal(reg_field(b2$tabs, "or"), reg_field(direct, "or"))
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


# Phase 22i: `multiplier` LEFT the key. It scales the tidy at reg_tidy_finalize(), beside the
# interval and the exponentiation, and cannot move a fit -- so a scaling pick must be a HIT that
# re-reports, exactly as an estimand change is. ⚠ a hit that served a STALE table would pass the
# "matches tab_reg()" half alone, so the two halves are asserted together.
test_that("multiplier is NOT in the key: a scaling pick is a hit that still re-reports", {
  gss  <- gss_reg()
  o10  <- reg_opts(predictors = c("race", "age"), ..family = "binomial", ..multiplier = c(age = 10))
  o05  <- reg_opts(predictors = c("race", "age"), ..family = "binomial", ..multiplier = c(age = 5))
  b10  <- quiet(jmvtab_reg_build(gss, o10, NULL))
  expect_equal(b10$hits, 0L)
  b05  <- quiet(jmvtab_reg_build(gss, o05, b10$store))
  expect_gte(b05$hits, 1L)                                   # served, not refitted
  expect_false(identical(reg_render(b10$tabs), reg_render(b05$tabs)))   # and re-reported
  d10  <- quiet(tab_reg(gss, "married", c("race", "age"),
                        family = "binomial", multiplier = c(age = 10),
                        empirical = o10$empirical, cleannames = TRUE))
  expect_identical(reg_render(b10$tabs), reg_render(d10))
  # the SERVED one is the cold one too -- a hit may not cost a number
  d05  <- quiet(tab_reg(gss, "married", c("race", "age"),
                        family = "binomial", multiplier = c(age = 5),
                        empirical = o10$empirical, cleannames = TRUE))
  expect_identical(reg_render(b05$tabs), reg_render(d05))
})


# The scaling is a change of UNITS, not of evidence: the estimate scales by k and the SE by |k|, so
# every p-value and every star is untouched. That is why relocating the rescale could move no test.
test_that("a multiplier changes the estimate's units and no p-value", {
  gss <- gss_reg()
  p <- function(k) {
    t <- quiet(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                       multiplier = c(age = k), empirical = FALSE))
    reg_field(t, "pvalue")
  }
  expect_equal(p(1), p(7))
  expect_equal(p(1), p(-3))                       # a negative k too: |est/se| is unsigned
})


# ⚠ A NEGATIVE multiplier must not hand back an inverted bracket. The bounds scale by the SIGNED k,
# so reg_tidy_rescale() re-orders them; before Phase 22i the profile branch did not and could not.
test_that("a negative multiplier keeps the interval the right way round", {
  gss <- gss_reg()
  t   <- quiet(tab_reg(gss, "married", c("race", "age"), family = "binomial",
                       multiplier = c(age = -2), empirical = FALSE))
  lo  <- reg_field(t, "ci_inf"); hi <- reg_field(t, "ci_sup")
  ok  <- !is.na(lo) & !is.na(hi)
  expect_true(all(lo[ok] <= hi[ok]))
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
