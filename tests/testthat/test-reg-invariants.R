# THREE INVARIANTS EVERY REGRESSION CELL MUST SATISFY, read off the column's own declared scale so
# one loop covers every family, contrast and measure:
#
#   1. an interval CONTAINS its estimate  -- the estimate and the bracket are one estimand
#   2. a reference cell IS the scale's neutral (0 additive, 1 multiplicative, 0 on a link scale)
#   3. a star AGREES with its interval    -- one CI-inclusion rule governs both
#
# They are cheap and family-agnostic, so they catch a whole CLASS of defect that per-case parity
# tests cannot: an interval built for one estimand beside an estimate computed for another. The grid
# below is deliberately LIGHT -- one case per family plus the two logged contrasts, on a reduced
# sample. The exhaustive family x effect x measure sweep is dev/verify_reg_invariants.R.

inv_data <- function(n = 2000) {
  d <- gss_cat_data_formatting()
  d <- d[!is.na(d$married) & !is.na(d$party3) & !is.na(d$rincome) &
           !is.na(d$race) & !is.na(d$age) & !is.na(d$tvhours), ]
  withr::with_seed(20260820, d[sample(nrow(d), min(n, nrow(d))), ])
}

inv_tea <- function() {
  e <- new.env()
  utils::data("tea", package = "FactoMineR", envir = e)
  tea <- e$tea
  items <- c("home", "tearoom", "work", "friends", "resto", "pub")
  tea$tea_where <- rowSums(vapply(items, function(v) as.integer(tea[[v]] == v),
                                  integer(nrow(tea))))
  tea$sex <- factor(tea$sex); tea$SPC <- factor(tea$SPC)
  tea
}

# every fmt column that carries an estimate: the model columns and their crude twins.
inv_cols <- function(t)
  names(t)[vapply(t, function(x) is_fmt(x) && get_role(x) %in% c("model", "emp"), logical(1))]

inv_check <- function(t, tag) {
  cols <- inv_cols(t)
  expect_true(length(cols) > 0L, info = tag)
  for (cn in cols) {
    col  <- t[[cn]]
    scr  <- EST_SCALES[[get_scale(col)]]
    est  <- fmt_est_of(col)
    lo   <- get_ci_inf(col); hi <- get_ci_sup(col); p <- get_pvalue(col)
    who  <- paste0(tag, " / ", cn, " [", get_scale(col), "]")
    # a cell with no interval says nothing; every invariant is about the cells that have one.
    ok <- is.finite(est) & is.finite(lo) & is.finite(hi)
    expect_true(all(est[ok] >= lo[ok] - 1e-9), info = paste(who, "-- estimate below its interval"))
    expect_true(all(est[ok] <= hi[ok] + 1e-9), info = paste(who, "-- estimate above its interval"))
    if (is.na(scr$neutral)) next
    # the Constant is a BASELINE, not a comparison: it is a reference row with no neutral to hold.
    ref <- is_refrow(col) & as.character(t$var) != "Constant" & is.finite(est)
    expect_true(all(abs(est[ref] - scr$neutral) < 1e-9),
                info = paste(who, "-- a reference cell is not the scale's neutral"))
    okp <- ok & is.finite(p)
    expect_identical(p[okp] < 0.05,
                     lo[okp] > scr$neutral + 1e-12 | hi[okp] < scr$neutral - 1e-12,
                     info = paste(who, "-- a star disagrees with its interval"))
  }
}

test_that("every family's cells hold one estimand: interval, neutral and star agree", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  skip_if_not_installed("FactoMineR")
  d <- inv_data()
  # family x LINK x contrast x measure -> the one case that exercises each producer. Since the
  # cascade, `link` and `measure` are separate axes, so both routes to a ratio are swept: the
  # model's own coefficient (`link`) and the same measure read off its predictions (`measure`).
  cases <- list(
    list(tag = "gaussian coef diff",   a = list(d, "age",     c("race", "tvhours"),
                                                family = "gaussian")),
    list(tag = "gaussian coef RoM",    a = list(d, "age",     c("race", "tvhours"),
                                                family = "gaussian", link = "ratio")),
    list(tag = "gaussian marg RoM",    a = list(d, "age",     c("race", "tvhours"),
                                                family = "gaussian", measure = "ratio")),
    list(tag = "binomial coef OR",     a = list(d, "married", c("race", "age"),
                                                family = "binomial")),
    list(tag = "binomial coef RR",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", link = "ratio")),
    list(tag = "binomial coef RD",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", link = "difference")),
    list(tag = "binomial marg RR",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", effect = "marginal")),
    list(tag = "binomial marg RD",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", effect = "marginal",
                                                measure = "difference")),
    # the estimand the generalised marginal engine added (Karlson & Jann 2023)
    list(tag = "binomial marg OR",     a = list(d, "married", c("race", "age"),
                                                family = "binomial", effect = "marginal",
                                                measure = "odds_ratio")),
    # fit on ONE scale, report on ANOTHER -- the capability only the cascade opens
    list(tag = "binomial rr -> mRD",   a = list(d, "married", c("race", "age"),
                                                family = "binomial", link = "ratio",
                                                measure = "difference")),
    list(tag = "poisson coef IRR",     a = list(d, "tvhours", c("race", "age"),
                                                family = "poisson")),
    list(tag = "multinomial coef OR",  a = list(d, "party3",  c("race", "age"),
                                                family = "multinomial")),
    list(tag = "ordinal coef cumOR",   a = list(d, "rincome", c("race", "age"),
                                                family = "ordinal")),
    # the two LOGGED contrasts: `log_coef` is one shared scale, so a wrong twin lands here
    list(tag = "binomial coef log(OR)", a = list(d, "married", c("race", "age"),
                                                 family = "binomial", measure = "log")),
    list(tag = "binomial marg log(RR)", a = list(d, "married", c("race", "age"),
                                                 family = "binomial", effect = "marginal",
                                                 measure = "log_risk")),
    # a SUMMED SCORE, whose crude effect sits on the mean score rather than a share
    list(tag = "grouped binomial RR",   a = list(inv_tea(), "tea_where", c("sex", "SPC"),
                                                 family = "binomial", trials = 6,
                                                 link = "ratio"))
  )
  for (cs in cases) {
    t <- suppressWarnings(suppressMessages(
      do.call(tab_reg, c(cs$a, list(empirical = "column", stats = FALSE)))))
    inv_check(t, cs$tag)
  }
})

test_that("a logged column is the log of its exponentiated twin, cell for cell", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d <- inv_data()
  # the marginal path derives its contrast from `comparison` but its SCALE from the estimand: the two
  # were one flag, which printed ratios on a column stamped `log_coef`.
  arg <- list(d, "married", c("race", "age"), family = "binomial", effect = "marginal",
              empirical = "column", stats = FALSE)
  lg <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "log_risk"))))
  rr <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "ratio"))))
  for (role in c("model", "emp")) {
    lc <- lg[[inv_cols(lg)[vapply(inv_cols(lg), function(n) get_role(lg[[n]]) == role,
                                  logical(1))][[1]]]]
    rc <- rr[[inv_cols(rr)[vapply(inv_cols(rr), function(n) get_role(rr[[n]]) == role,
                                  logical(1))][[1]]]]
    expect_identical(get_scale(lc), "log_coef")
    expect_identical(get_scale(rc), "pct_ratio")
    fin <- is.finite(get_ratio(rc)) & is.finite(get_diff(lc))
    expect_equal(get_diff(lc)[fin], log(get_ratio(rc))[fin], tolerance = 1e-12)
    expect_equal(get_ci_inf(lc), log(get_ci_inf(rc)), tolerance = 1e-12)
    expect_equal(get_pvalue(lc), get_pvalue(rc), tolerance = 1e-12)
  }
})

test_that("every reachable estimand pairs with its declared crude shape", {
  # reg_same_estimand() is the gate that withholds `obs` and the gap SE. It must refuse a mismatch
  # WITHOUT refusing anything the package can legitimately build, so the whole grid is swept.
  for (f in names(REG_ESTIMANDS)) for (r in REG_ESTIMANDS[[f]]$rows) {
    # `trials =` is the one caller-supplied fact that moves a block, and it applies to a BINARY
    # outcome only -- the argument boundary refuses it elsewhere.
    tris <- c(list(NA), if (reg_is_grouped_binomial(r$fit, 6)) list(6))
    for (tri in tris) {
      key <- reg_crude_key(r$fit, if (is.na(tri)) NULL else tri)
      if (is.na(key)) next
      sh <- reg_crude_shape(key, r)
      expect_false(is.null(sh),
                   info = paste(f, r$link, r$effect, r$measure, "-- no crude shape resolves"))
      expect_true(reg_same_estimand(sh, reg_scale_of(r, tri), r),
                  info = paste(f, r$link, r$effect, r$measure, "trials:", tri))
    }
  }
})
