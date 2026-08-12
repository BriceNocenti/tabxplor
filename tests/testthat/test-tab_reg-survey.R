# Phase 12g: survey design pass-through (ids/strata/fpc/nest + a prebuilt design as `data`), the reduced
# weighted glance, and weighted model comparison. Parity is checked against a hand-built survey::svyglm.

skip_if_not_installed("survey")
skip_if_not_installed("broom")

# A small deterministic clustered + stratified fixture (psu nested in strata).
reg_survey_data <- function() {
  set.seed(42)
  n <- 1200L
  strata <- sample(c("A", "B", "C"), n, replace = TRUE)
  psu    <- paste0(strata, "-", sample(1:5, n, replace = TRUE))   # psu nested in strata
  x1     <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE))
  x2     <- rnorm(n)
  eta    <- -0.3 + 0.8 * (x1 == "hi") - 0.5 * (x1 == "mid") + 0.4 * x2
  y      <- rbinom(n, 1, plogis(eta))
  w      <- runif(n, 0.4, 3)
  tibble::tibble(y = factor(y), yb = factor(dplyr::if_else(y == 1, "event", "no")),
                 x1 = x1, x2 = x2, w = w, psu = psu, strata = strata)
}

or_col <- function(tab) {
  nm <- grep("^Model_", names(tab), value = TRUE)[1]
  vapply(tab[[nm]], tabxplor::get_num, numeric(1))
}

test_that("a clustered, stratified design matches a hand svyglm", {
  # Last Phase z14-i: clustering / stratification are expressed by BUILDING the design and passing it
  # as `data` -- the ids/strata/fpc/nest arguments are gone (they reached only the omnibus p-value,
  # and svydesign() says all four better).
  d   <- reg_survey_data()
  # tab_reg models the FIRST level of the 2-level factor as the event; match that coding by hand.
  d01  <- dplyr::mutate(d, y01 = as.integer(y == levels(y)[1]))
  des2 <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d01, nest = TRUE)
  hand <- survey::svyglm(y01 ~ x1 + x2, design = des2, family = quasibinomial())

  des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d, nest = TRUE)
  # Last Phase z9: `multiplier = 1` pins the per-1-unit reading this parity assertion is ABOUT
  # (the default is now "sd", which would compare a per-SD OR to a per-unit coefficient).
  tab <- suppressMessages(tab_logit(des, "y", c("x1", "x2"), multiplier = 1))
  tv  <- or_col(tab)
  # skeleton = Constant, x1(ref), x1 mid, x1 hi, x2 -> drop the reference row (OR = 1) for the term match
  hand_or <- exp(stats::coef(hand))
  expect_equal(unname(tv[tv != 1]), unname(hand_or), tolerance = 1e-6)
})

test_that("a prebuilt survey design passed as `data` equals the hand svyglm", {
  d    <- reg_survey_data()
  d01  <- dplyr::mutate(d, y01 = as.integer(y == 1))
  des  <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d01, nest = TRUE)
  hand <- survey::svyglm(y01 ~ x1 + x2, design = des, family = quasibinomial())

  tab <- tab_logit(des, "y01", c("x1", "x2"), multiplier = 1)
  tv  <- or_col(tab)
  expect_equal(unname(tv[tv != 1]), unname(exp(stats::coef(hand))), tolerance = 1e-6)
})

test_that("passing wt alongside a design object ABORTS (Last Phase z16-i, W10)", {
  d   <- reg_survey_data()
  d01 <- dplyr::mutate(d, y01 = as.integer(y == 1))
  des <- survey::svydesign(ids = ~psu, weights = ~w, data = d01)
  # it used to be silently ignored with a console note nothing downstream could see; every other
  # variable-role collision in the package aborts, and now so does this one, in tab() too.
  expect_error(tab_logit(des, "y01", "x1", wt = "w"), "cannot be used when")
  expect_error(suppressMessages(tab(des, x1, y01, wt = w)), "cannot be used when")
})

test_that("weighted footer is the reduced survey set (n / wald_null / nagelkerke_r2 / aic)", {
  d   <- reg_survey_data()
  tab <- tab_logit(d, "y", c("x1", "x2"), wt = "w")
  tst <- tabxplor:::get_test(tab)
  # z13's overall-association rows and z15's model-check rows are in every default set; this asserts
  # the model-FIT statistics only.
  expect_setequal(setdiff(unique(tst$test),
                          c(tabxplor:::reg_global_types(), tabxplor:::reg_check_types())),
                  c("n", "wald_null", "nagelkerke_r2", "aic"))
  # no naive glm stats leak in under weights (`phi` is the exact Pearson dispersion, z15)
  expect_false(any(c("lr_null", "mcfadden_r2", "bic", "phi", "r2") %in% tst$test))
})

test_that("cox_snell_r2 is selectable via stats= for weighted models", {
  d   <- reg_survey_data()
  tab <- tab_logit(d, "y", "x1", wt = "w",
                   stats = c("n", "nagelkerke_r2", "cox_snell_r2"))
  tst <- tabxplor:::get_test(tab)
  expect_true("cox_snell_r2" %in% tst$test)
})

test_that("weighted model comparison emits a design-based Wald row", {
  d   <- reg_survey_data()
  des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d, nest = TRUE)
  tab <- suppressMessages(multi_logit(des, "y",
                     models = list(base = "x1", full = c("x1", "x2")),
                     compare = "baseline"))
  tst <- tabxplor:::get_test(tab)
  expect_true("compare_baseline_wald" %in% tst$test)
  wr <- tst[tst$test == "compare_baseline_wald", ]
  expect_true(is.finite(wr$pvalue) && wr$pvalue >= 0 && wr$pvalue <= 1)
})

test_that("unweighted tab_logit is unchanged by the design plumbing", {
  d  <- reg_survey_data()
  t0 <- tab_logit(d, "y", c("x1", "x2"), multiplier = 1)
  hand <- stats::glm(as.integer(y == levels(y)[1]) ~ x1 + x2, data = d, family = binomial())
  tv <- or_col(t0)
  expect_equal(unname(tv[tv != 1]), unname(exp(stats::coef(hand))), tolerance = 1e-6)
})

# --- Phase 12g-ii: weighted 3+ level outcomes ------------------------------------------------------
reg_survey_multi_data <- function() {
  set.seed(7); n <- 900L
  x1 <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE)); x2 <- rnorm(n)
  lp <- 0.6 * (x1 == "hi") - 0.4 * (x1 == "mid") + 0.3 * x2
  yo <- cut(lp + rnorm(n), breaks = c(-Inf, -0.5, 0.5, Inf),
            labels = c("low", "mid", "high"), ordered = TRUE)
  yn <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
  w  <- runif(n, 0.5, 3)
  tibble::tibble(yo = yo, yn = yn, x1 = x1, x2 = x2, w = w)
}

test_that("weighted ordinal (svyolr) matches a hand svyolr cumulative OR", {
  d   <- reg_survey_multi_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  hand <- survey::svyolr(yo ~ x1 + x2, design = des)

  tab <- tab_reg(d, "yo", c("x1", "x2"), family = "ordinal", wt = "w")
  oc  <- vapply(tab[[grep("^Model_", names(tab), value = TRUE)[1]]], tabxplor::get_num, numeric(1))
  # skeleton = Constant (NA), x1 ref (1), x1lo, x1mid, x2 -> drop NA + reference
  oc  <- oc[!is.na(oc) & oc != 1]
  expect_equal(unname(oc), unname(exp(hand$coefficients)), tolerance = 1e-5)
})

test_that("weighted multinomial (svyVGAM) matches a hand svy_vglm OR", {
  skip_if_not_installed("svyVGAM")
  skip_if_not_installed("VGAM")
  d   <- reg_survey_multi_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  hand <- svyVGAM::svy_vglm(yn ~ x1 + x2, design = des,
                            family = VGAM::multinomial(refLevel = 1))
  hand_or <- exp(stats::coef(hand))

  tab <- tab_reg(d, "yn", c("x1", "x2"), family = "multinomial", wt = "w")
  # one OR column per non-reference outcome category ("B", "C"); 14w strips the trailing ": OR"
  or_cols <- grep(" vs ", names(tab), value = TRUE)
  expect_length(or_cols, 2L)
  tv <- unlist(lapply(or_cols, function(nm) {
    v <- vapply(tab[[nm]], tabxplor::get_num, numeric(1)); v[!is.na(v) & v != 1]
  }))
  expect_equal(sort(unname(tv)), sort(unname(hand_or)), tolerance = 1e-4)
})

test_that("weighted multinomial errors clearly without svyVGAM", {
  skip_if("svyVGAM" %in% rownames(utils::installed.packages()))
  d <- reg_survey_multi_data()
  expect_error(tab_reg(d, "yn", c("x1", "x2"), family = "multinomial", wt = "w"), "svyVGAM")
})

test_that("effect='ame' is refused for weighted 3+ level outcomes", {
  d <- reg_survey_multi_data()
  expect_error(tab_reg(d, "yo", "x1", family = "ordinal", wt = "w", effect = "ame"),
               "not available for survey-weighted")
})

# --- Phase 12g-iii: split_var (stacked grouped subtables + tab_spread) ------------------------------
reg_split_data <- function() {
  set.seed(11); n <- 1500L
  g  <- factor(sample(c("north", "south"), n, replace = TRUE))
  x1 <- factor(sample(c("a", "b", "c"), n, replace = TRUE)); x2 <- rnorm(n)
  y  <- rbinom(n, 1, plogis(-0.2 + 0.5 * (x1 == "b") + 0.3 * x2 + 0.4 * (g == "south")))
  tibble::tibble(y = factor(y), g = g, x1 = x1, x2 = x2, w = runif(n, 0.5, 3))
}

test_that("split_var stacks one model per group (grouped by split_var + var)", {
  d <- reg_split_data()
  t <- tab_logit(d, "y", c("x1", "x2"), split_var = "g", spread_models = FALSE)
  expect_s3_class(t, "tabxplor_grouped_tab")
  expect_setequal(dplyr::group_vars(t), c("g", "var"))
  expect_true("g" %in% names(t))
  expect_setequal(levels(dplyr::pull(dplyr::ungroup(t), g)), c("north", "south"))
})

test_that("Phase g: split_var + a single model auto-spreads to side-by-side columns", {
  d <- reg_split_data()
  # default spread_models = TRUE: the sub-models sit side by side (no stacked `g` row-column)
  t <- tab_logit(d, "y", c("x1", "x2"), split_var = "g")
  expect_false("g" %in% names(t))
  # each split level's column carries a "{level}<br>{outcome}" col_var -> borders + a two-line span
  fc <- names(t)[vapply(t, is_fmt, logical(1))]
  cv <- vapply(fc, function(nm) tabxplor:::get_col_var(t[[nm]]), character(1))
  expect_true(all(grepl("<br>", cv)))
  expect_true(any(grepl("^north<br>", cv)) && any(grepl("^south<br>", cv)))
  # works with empirical = TRUE (crude companions spread too, level-suffixed)
  te <- suppressWarnings(tab_logit(d, "y", "x1", split_var = "g", empirical = TRUE))
  expect_true(any(grepl("^Obs_", names(te))))
  # opt-out keeps the stacked grouped form
  expect_s3_class(tab_logit(d, "y", "x1", split_var = "g", spread_models = FALSE),
                  "tabxplor_grouped_tab")
})

test_that("each split group equals a manual per-subset fit", {
  d <- reg_split_data()
  t <- dplyr::ungroup(tab_logit(d, "y", c("x1", "x2"), split_var = "g", spread_models = FALSE,
                                multiplier = 1))
  for (grp in c("north", "south")) {
    sub  <- dplyr::filter(d, g == grp)
    hand <- stats::glm(as.integer(y == levels(y)[1]) ~ x1 + x2, data = sub, family = binomial())
    tv   <- vapply(t[[grep("^Model_", names(t), value = TRUE)[1]]][t$g == grp], tabxplor::get_num, numeric(1))
    expect_equal(unname(tv[tv != 1]), unname(exp(stats::coef(hand))), tolerance = 1e-6)
  }
})

test_that("tab_spread pivots split groups into side-by-side columns", {
  d  <- reg_split_data()
  t  <- tab_logit(d, "y", c("x1", "x2"), split_var = "g", spread_models = FALSE)
  sp <- tab_spread(t, g)
  expect_s3_class(sp, "tabxplor_tab")
  # one OR column per split level (north / south), sharing the var/level stub
  expect_true(any(grepl("north", names(sp))) && any(grepl("south", names(sp))))
  expect_true(all(c("var", "levels") %in% names(sp)))
})

test_that("split_var footer carries per-group GOF", {
  d   <- reg_split_data()
  t   <- tab_logit(d, "y", "x1", split_var = "g", spread_models = FALSE)
  tst <- tabxplor:::get_test(t)
  expect_setequal(unique(tst$row_var), c("north", "south"))   # tagged per split group
  expect_true(all(c("n", "lr_null") %in% tst$test))
})

test_that("split_var works with survey weights (per-group svyglm)", {
  d <- reg_split_data()
  t <- tab_logit(d, "y", c("x1", "x2"), wt = "w", split_var = "g", spread_models = FALSE,
                 multiplier = 1)
  expect_s3_class(t, "tabxplor_grouped_tab")
  sub  <- dplyr::filter(d, g == "north")
  des  <- survey::svydesign(ids = ~1, weights = ~w,
                            data = dplyr::mutate(sub, y01 = as.integer(y == levels(y)[1])))
  hand <- survey::svyglm(y01 ~ x1 + x2, design = des, family = quasibinomial())
  tt   <- dplyr::ungroup(t)
  tv   <- vapply(tt[[grep("^Model_", names(tt), value = TRUE)[1]]][tt$g == "north"],
                 tabxplor::get_num, numeric(1))
  expect_equal(unname(tv[tv != 1]), unname(exp(stats::coef(hand))), tolerance = 1e-5)
})

test_that("split_var rejects an invalid grouping column", {
  d <- reg_split_data()
  expect_error(tab_logit(d, "y", "x1", split_var = "x1"), "cannot also be")   # a predictor
  expect_error(tab_logit(d, "y", "x1", split_var = "nope"), "not a column")
  expect_error(tab_logit(d, "y", "x1", split_var = "x2"), "factor or character")
})

# --- Phase 12g-iv: multiplier + empirical ----------------------------------------------------
test_that("multiplier scales a continuous predictor's OR to OR^k, p unchanged", {
  d <- reg_split_data()
  t0  <- suppressWarnings(tab_logit(d, "y", c("x1", "x2"), multiplier = 1))
  t10 <- suppressWarnings(tab_logit(d, "y", c("x1", "x2"), multiplier = c(x2 = 10)))
  oc  <- grep("^Model_", names(t0), value = TRUE)[1]
  or0  <- vapply(t0[[oc]],  tabxplor::get_num, numeric(1))
  or10 <- vapply(t10[[oc]], tabxplor::get_num, numeric(1))
  # last row = x2; other rows (Constant, x1 levels) unchanged
  expect_equal(or10[length(or10)], or0[length(or0)]^10, tolerance = 1e-8)
  expect_equal(or10[-length(or10)], or0[-length(or0)], tolerance = 1e-10)
  # stars (pvalue) unchanged by rescaling
  expect_equal(tabxplor:::get_pvalue(t0[[oc]]), tabxplor:::get_pvalue(t10[[oc]]))
})

test_that("multiplier rejects non-numeric predictors / wrong families", {
  d <- reg_split_data()
  expect_error(suppressWarnings(tab_logit(d, "y", c("x1", "x2"), multiplier = c(x1 = 2))),
               "numeric predictors")
})

test_that("empirical crude OR matches the weighted 2x2 odds ratio", {
  d   <- reg_split_data()
  t   <- suppressWarnings(tab_logit(d, "y", "x1", empirical = TRUE))
  expect_true(all(c("Obs_%", "Obs_OR") %in% names(t)))
  eo  <- vapply(dplyr::ungroup(t)[["Obs_OR"]], tabxplor::get_num, numeric(1))
  # hand crude OR of each x1 level vs the reference "a", positive outcome = first level of y
  pos <- levels(d$y)[1]; lv <- levels(d$x1); ref <- lv[1]
  hand <- vapply(lv, function(l) {
    a <- sum(d$x1 == l & d$y == pos); b <- sum(d$x1 == l & d$y != pos)
    c <- sum(d$x1 == ref & d$y == pos); e <- sum(d$x1 == ref & d$y != pos)
    (a / b) / (c / e)
  }, numeric(1))
  eo_fac <- eo[!is.na(eo)]                          # drop the Constant NA
  expect_equal(unname(eo_fac), unname(hand), tolerance = 1e-8)
})

test_that("empirical: gaussian now produces crude columns (Phase 14v)", {
  d <- reg_split_data()
  # Phase 14v: gaussian empirical is now wired (crude mean + mean-difference), no longer ignored.
  tg <- tab_reg(d, "x2", "x1", family = "gaussian", empirical = TRUE)
  expect_true(all(c("Obs_mean", "Obs_diff") %in% names(tg)))
})
