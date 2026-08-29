# PURPOSE: the ordinal superiority measures.
# ROLE: the shipped CONTRACT for R/reg-rank -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the ordinal rank measures ===========================================================

testthat::test_that("the superiority pair's two readings are readings of ONE pair", {
  set.seed(1)
  p1 <- c(.25, .30, .20, .25); p0 <- c(.40, .25, .20, .15)
  pr <- tabxplor:::reg_rank_pair(p1, p0, "identity")
  # 2*gamma - 1 == win - loss is an identity, not an approximation
  testthat::expect_equal(2 * pr$gamma - 1, pr$win - pr$loss, tolerance = 1e-14)
  testthat::expect_equal(pr$est, pr$win - pr$loss, tolerance = 1e-14)
  testthat::expect_equal(pr$alt, pr$win / pr$loss, tolerance = 1e-14)
  # comparing a distribution with itself is a coin flip, exactly
  testthat::expect_equal(tabxplor:::reg_rank_pair(p1, p1, "identity")$gamma, 0.5, tolerance = 1e-14)
  testthat::expect_equal(tabxplor:::reg_rank_pair(p1, p1, "identity")$est,   0,   tolerance = 1e-14)
  testthat::expect_equal(tabxplor:::reg_rank_pair(p1, p1, "log")$est,        0,   tolerance = 1e-14)
  # the log reading is the log of the other one's ratio
  lg <- tabxplor:::reg_rank_pair(p1, p0, "log")
  testthat::expect_equal(exp(lg$est), pr$win / pr$loss, tolerance = 1e-12)
  testthat::expect_equal(lg$alt, pr$win - pr$loss, tolerance = 1e-14)
})

testthat::test_that("on two categories the pair IS the binomial family's own measures", {
  a <- 0.62; b <- 0.44
  pr <- tabxplor:::reg_rank_pair(c(1 - a, a), c(1 - b, b), "identity")
  testthat::expect_equal(pr$est, a - b, tolerance = 1e-14)                       # the risk difference
  testthat::expect_equal(pr$alt, (a / (1 - a)) / (b / (1 - b)), tolerance = 1e-12)  # the odds ratio
})

testthat::test_that("the analytic gradients match numeric differentiation, on both links", {
  testthat::skip_if_not_installed("MASS")
  set.seed(3)
  n <- 600
  d <- data.frame(x = factor(sample(c("a", "b"), n, TRUE)), z = stats::rnorm(n))
  eta <- 0.8 * (d$x == "b") + 0.6 * d$z
  cp  <- sapply(c(-1, 0, 1), function(k) stats::plogis(k - eta))
  P   <- cbind(cp[, 1], cp[, -1] - cp[, -3], 1 - cp[, 3])
  d$y <- factor(apply(P, 1, function(p) sample.int(4L, 1L, prob = p)), ordered = TRUE)
  m   <- MASS::polr(y ~ x + z, data = d, Hess = TRUE)
  eng <- tabxplor:::reg_prob_engine(m)

  for (lnk in c("identity", "log")) {
    p <- tabxplor:::reg_gcomp_rank_maker(m, d, NULL, lnk)("x", "b", "a")
    fn <- function(th) {
      X1 <- eng$mm(transform(d, x = factor("b", levels = levels(d$x))))
      X0 <- eng$mm(transform(d, x = factor("a", levels = levels(d$x))))
      tabxplor:::reg_rank_pair(colMeans(eng$probs(th, X1)),
                               colMeans(eng$probs(th, X0)), lnk)$est
    }
    th <- eng$theta; h <- 1e-6
    num <- vapply(seq_along(th), function(j) {
      u <- th; v <- th; u[j] <- u[j] + h; v[j] <- v[j] - h; (fn(u) - fn(v)) / (2 * h)
    }, numeric(1))
    testthat::expect_equal(p$est, fn(th), tolerance = 1e-12)
    testthat::expect_lt(max(abs(p$G - num)), 1e-6)
    testthat::expect_equal(p$mean0, 0.5, tolerance = 1e-14)   # `{base}` on the reference row
  }
})

testthat::test_that("the crude closed form's SE matches a multinomial bootstrap", {
  set.seed(5)
  y1 <- c(120, 200, 160, 90); y0 <- c(300, 340, 410, 500)
  p1 <- y1 / sum(y1); p0 <- y0 / sum(y0)
  pr <- tabxplor:::reg_rank_pair(p1, p0, "identity")
  se <- tabxplor:::reg_rank_se(pr, p1, p0, sum(y1), sum(y0))
  bs <- replicate(1500, tabxplor:::reg_rank_pair(
    stats::rmultinom(1, sum(y1), p1)[, 1] / sum(y1),
    stats::rmultinom(1, sum(y0), p0)[, 1] / sum(y0), "identity")$est)
  testthat::expect_equal(se, stats::sd(bs), tolerance = 0.08)
})

testthat::test_that("the marginal superiority measure is COLLAPSIBLE, where the cumOR is not", {
  # This is what makes `color = \"adjustment\"` a test here rather than a description: with the
  # covariate INDEPENDENT of the exposure there is no confounding, so an honest crude/adjusted
  # comparison must not move -- and the cumulative odds ratio does.
  testthat::skip_if_not_installed("MASS")
  set.seed(9)
  n <- 4000
  x <- stats::rbinom(n, 1, .5); z <- stats::rnorm(n)
  eta <- 0.8 * x + 1.5 * z
  cp  <- sapply(c(-1.5, -.5, .5, 1.5), function(k) stats::plogis(k - eta))
  P   <- cbind(cp[, 1], cp[, -1] - cp[, -4], 1 - cp[, 4])
  d   <- data.frame(y = factor(apply(P, 1, function(p) sample.int(5L, 1L, prob = p)), ordered = TRUE),
                    x = factor(x), z = z)
  m   <- MASS::polr(y ~ x + z, data = d, Hess = TRUE)
  adj <- tabxplor:::reg_gcomp_rank_maker(m, d, NULL, "identity")("x", "1", "0")$est
  tb  <- prop.table(table(d$x, d$y), 1)
  crd <- tabxplor:::reg_rank_pair(as.numeric(tb["1", ]), as.numeric(tb["0", ]), "identity")$est
  testthat::expect_lt(abs(adj - crd), 0.02)                        # the pair barely moves
  or_crude <- exp(stats::coef(MASS::polr(y ~ x, data = d, Hess = TRUE)))
  testthat::expect_gt(exp(stats::coef(m))[["x1"]] / or_crude, 1.2) # the cumOR moves a lot
})

testthat::test_that("tab_reg() draws ONE column, with its crude twin and a tested gap", {
  testthat::skip_if_not_installed("MASS")
  d <- fx_reg_fmt()
  t <- suppressWarnings(tab_reg(d, outcome = "rincome", predictors = c("race", "marital"),
                                family = "ordinal", effect = "marginal", empirical = TRUE,
                                color = "adjustment", cleannames = FALSE))
  testthat::expect_true(all(c("Model_mD", "Obs_D") %in% names(t)))
  mo <- t[["Model_mD"]]; ob <- t[["Obs_D"]]
  # the crude twin is a real column with a real interval, which a 3+ level outcome never had before
  testthat::expect_true(any(!is.na(get_ci_inf(ob))))
  # both columns read on the same scale, and both carry the probability of superiority as `{base}`
  testthat::expect_identical(get_scale(mo), get_scale(ob))
  testthat::expect_true(all(get_pct(mo)[!is.na(get_pct(mo))] >= 0 &
                            get_pct(mo)[!is.na(get_pct(mo))] <= 1))
  # collapsible, so the gap between them is TESTED rather than merely coloured
  testthat::expect_true(any(is.finite(vctrs::field(mo, "gap_se"))))
  # the footer says what "higher" means -- the only place a one-column table can
  testthat::expect_true(any(grepl("from low to high", tabxplor:::reg_model_lines(t), fixed = TRUE)))
})

testthat::test_that("`measure = \"ratio\"` on an ordered outcome builds one win-ratio column", {
  testthat::skip_if_not_installed("MASS")
  d <- fx_reg_fmt()
  t <- suppressWarnings(tab_reg(d, outcome = "rincome", predictors = "race",
                                family = "ordinal", measure = "ratio", cleannames = FALSE))
  testthat::expect_true("Model_mWR" %in% names(t))
  testthat::expect_identical(get_scale(t[["Model_mWR"]]), "pct_ratio")
  # the reference row is the neutral of a ratio, and its base a coin flip
  i <- which(as.character(t$levels) == "White")
  testthat::expect_equal(vctrs::field(t[["Model_mWR"]], "ratio")[i], 1, tolerance = 1e-12)
  testthat::expect_equal(get_pct(t[["Model_mWR"]])[i], 0.5, tolerance = 1e-12)
})

testthat::test_that("a WEIGHTED ordinal model can be read on its rank measures", {
  testthat::skip_if_not_installed("MASS")
  testthat::skip_if_not_installed("survey")
  d <- fx_reg_fmt()[c("rincome", "race", "marital")]
  d <- d[stats::complete.cases(d), ]
  set.seed(2); d$w <- stats::runif(nrow(d), .5, 2)
  # svyolr is NOT a polr subclass and its coef() carries the thresholds too: the engine must read
  # both parameterisations, and take its variance from svyolr's own design-based vcov().
  t <- suppressMessages(suppressWarnings(
    tab_reg(d, outcome = "rincome", predictors = c("race", "marital"), family = "ordinal",
            effect = "marginal", wt = "w", cleannames = FALSE)))
  testthat::expect_true("Model_mD" %in% names(t))
  testthat::expect_true(any(is.finite(get_ci_inf(t[["Model_mD"]]))))
  # ... while a weighted MULTINOMIAL marginal quantity still has no method at all
  testthat::expect_error(suppressMessages(
    tab_reg(d, outcome = "rincome", predictors = "race", family = "multinomial",
            effect = "marginal", wt = "w")), "coefficients")
})


# === SECTION: per-cut cumulative odds ratios ======================================================

ord_data <- function() {
  n <- c(a = 40, b = 30, c = 20, d = 10,      # group "ref"
         a = 10, b = 20, c = 30, d = 40)      # group "hi"
  data.frame(
    g = factor(rep(c("ref", "hi"), each = 100), levels = c("ref", "hi")),
    y = factor(rep(rep(c("a", "b", "c", "d"), 2), times = n),
               levels = c("a", "b", "c", "d"), ordered = TRUE)
  )
}

woolf_or <- function(a, b, cc, dd) (a * dd) / (b * cc)


# --- Step 1: the ordered class survives -------------------------------------------------------

test_that("an ordered tab_var no longer breaks the totals machinery, and keeps its class", {
  d <- ord_data()
  d$tv <- d$y                                   # an ORDERED tab_var: the case that used to abort

  t <- tab(d, g, y, tab_vars = tv)              # used to error in leaf_rename_totals()
  expect_s3_class(t, "tabxplor_tab")
  expect_true(is.ordered(t$tv))
  expect_true("Ensemble" %in% levels(t$tv))

  d$num <- as.numeric(d$y)
  tn <- tab(d, g, num, tab_vars = tv)           # used to error in num_rollup()'s vec_rbind
  expect_s3_class(tn, "tabxplor_tab")
  expect_true(is.ordered(tn$tv))
})

test_that("un-blocking `ordered` leaves an unordered table byte-identical", {
  d <- ord_data()
  d$tv <- d$y
  plain <- d
  plain$y  <- factor(plain$y,  levels = levels(plain$y),  ordered = FALSE)
  plain$tv <- factor(plain$tv, levels = levels(plain$tv), ordered = FALSE)

  a <- tab(d,     g, y, tab_vars = tv, pct = "row")
  b <- tab(plain, g, y, tab_vars = tv, pct = "row")
  # same values and same level ORDER; only the class of the grouping column differs
  expect_identical(levels(a$tv), levels(b$tv))
  expect_identical(lapply(a[-(1:2)], unclass), lapply(b[-(1:2)], unclass))
  expect_true(is.ordered(a$tv))
  expect_false(is.ordered(b$tv))
})


test_that("stacking an ordered row_var with a plain one drops the incomparable order", {
  d <- ord_data()
  d$g2 <- d$g
  # the merged `levels` column holds DIFFERENT variables' levels, so an order across them would be a
  # claim that does not exist -- and vctrs refuses to combine an ordered factor with a plain one.
  t <- tab(d, c(g2, y), g, pct = "row", na = "drop")
  expect_s3_class(t, "tabxplor_tab")
  expect_false(is.ordered(t$levels))
  expect_true(all(c(levels(d$g2), levels(d$y)) %in% levels(t$levels)))
  # two ordered row_vars have different level sets -- also incomparable
  d$y2 <- d$y
  expect_s3_class(tab(d, c(y, y2), g, pct = "row", na = "drop"), "tabxplor_tab")
  # a SINGLE ordered row_var keeps its class
  expect_true(is.ordered(tab(d, y, g, pct = "row", na = "drop")[[1]]))
})


# --- Step 2: ref2 = "cumulative", display = "{or}" ----------------------------------------------------------------------

test_that("cumOR is the per-cut Woolf odds ratio of the cumulated counts", {
  t <- tab(ord_data(), g, y, pct = "row", ref2 = "cumulative", display = "{or}", na = "drop")
  lv <- c("a", "b", "c", "d")

  cum_ref <- cumsum(c(40, 30, 20, 10))
  cum_hi  <- cumsum(c(10, 20, 30, 40))
  hand    <- vapply(seq_len(3), function(j)                       # k-1 = 3 real cut points
    woolf_or(cum_hi[j], 100 - cum_hi[j], cum_ref[j], 100 - cum_ref[j]), numeric(1))

  got <- vapply(lv[1:3], function(l) get_or(t[[l]])[t$g == "hi"], numeric(1))
  expect_equal(unname(got), hand, tolerance = 1e-10)
  expect_true(all(hand < 1))                                      # "hi" really is shifted upward

  # the reference row is 1 on every real cut
  expect_equal(unname(vapply(lv[1:3], function(l) get_or(t[[l]])[t$g == "ref"], numeric(1))),
               rep(1, 3))
})

test_that("the last cut is degenerate, so its column is empty and carries no reference '1'", {
  t <- tab(ord_data(), g, y, pct = "row", ref2 = "cumulative", display = "{or}", na = "drop")
  expect_true(all(is.na(get_or(t[["d"]]))))
  # and it must not print the raw "NA" beside the reference percentage
  expect_false(any(grepl("NA", format(t[["d"]]), fixed = TRUE)))
})

test_that("cumOR has no reference COLUMN (every column is its own cut point)", {
  t <- tab(ord_data(), g, y, pct = "row", ref2 = "cumulative", display = "{or}", na = "drop")
  expect_false(any(vapply(c("a", "b", "c", "d"), function(l) any(is_refcol(t[[l]])), logical(1))))
})

test_that("cumOR carries a Woolf interval and stars when a policy asks for one", {
  t <- tab(ord_data(), g, y, pct = "row", ref2 = "cumulative", display = "{or}", na = "drop", stars = TRUE)
  cum_ref <- cumsum(c(40, 30, 20, 10)); cum_hi <- cumsum(c(10, 20, 30, 40))
  j  <- 1
  ex <- ci_or(cum_hi[j], 100 - cum_hi[j], cum_ref[j], 100 - cum_ref[j], want_p = TRUE)
  hi <- t$g == "hi"
  expect_equal(get_ci_inf(t[["a"]])[hi], ex$inf, tolerance = 1e-10)
  expect_equal(get_ci_sup(t[["a"]])[hi], ex$sup, tolerance = 1e-10)
  expect_equal(get_pvalue(t[["a"]])[hi], ex$pvalue, tolerance = 1e-10)
  expect_identical(as.character(get_scale(t[["a"]])), "odds_ratio")
})

test_that("the `na = 'keep'` column never becomes a cut point", {
  d <- ord_data()
  d$y[1:10] <- NA
  t <- tab(d, g, y, pct = "row", ref2 = "cumulative", display = "{or}")            # na = "keep" -> an "NA" column
  expect_true("NA" %in% names(t))
  expect_true(all(is.na(get_or(t[["NA"]]))))
  # the last REAL level is then the degenerate cut, not the NA column
  expect_true(all(is.na(get_or(t[["d"]]))))
  expect_false(all(is.na(get_or(t[["c"]]))))
})

# Phase 19d: the odds ratio is computed on EVERY row/col-% column, so an ineligible col_var no
# longer degrades to "no odds ratio at all" -- only the CUMULATIVE cut needs an <ordered> factor,
# and what it degrades to is the plain 2x2 (ref2 = "first"). The message still names the fix.
test_that("an ineligible col_var degrades to the plain 2x2 OR, with one message naming the fix", {
  d <- ord_data()
  d$nominal <- factor(rep(c("p", "q", "r"), length.out = nrow(d)))

  expect_message(t <- tab(d, g, c(y, nominal), pct = "row", ref2 = "cumulative", display = "{or}", na = "drop"),
                 "ordered")
  expect_false(all(is.na(get_or(t[["a"]]))))               # the ordered col_var gets the cumulative cut
  expect_false(all(is.na(get_or(t[["p"]]))))               # the nominal one gets the plain 2x2
  expect_identical(as.character(get_display(t[["p"]]))[1], "or")
})

test_that("cumOR needs row percentages, and says so instead of computing nonsense", {
  expect_message(t <- tab(ord_data(), g, y, pct = "col", ref2 = "cumulative", display = "{or}", na = "drop"),
                 "pct")
  # the CUMULATIVE cut is skipped; a column percentage still carries its own plain 2x2 odds ratio.
  expect_false(all(is.na(get_or(t[["a"]]))))
})


# --- the recycle bug the per-pair OR resolution deleted ----------------------------------------

test_that("color = 'auto' resolves to the OR measure with several factor col_vars", {
  d <- ord_data()
  d$y2 <- factor(rep(c("no", "yes"), length.out = nrow(d)), levels = c("no", "yes"))
  # `auto_or` used to index the per-row_var SCALAR OR with a logical over col_vars, so with >= 2
  # factor col_vars it read c("OR", NA) -> FALSE -> the table silently coloured on the difference.
  t <- tab(d, g, c(y2, y), pct = "row", display = "{or}", ref = "first", color = TRUE, ref2 = 1, na = "drop")
  expect_identical(as.character(get_color(t[["yes"]])), "odds_ratio")
})
