# Phase 18z15-ii / z15-iii -- `shape =` (the cure for a non-linearity), the observed curves and the
# row sparkline (R/reg-assumptions.R + the wiring in R/tab_reg.R).
#
# The fixtures are the ones that would fail if a decision were quietly reverted, per the design's SS20:
# the crude twin's term names must be IDENTICAL to the model's, centring must keep the pair's VIF low,
# `shape = "linear"` must be byte-identical to no shape at all, and `spark = FALSE` must restore the
# old label byte-for-byte.
#
# CRAN time: several model fits per test. skip_on_cran() trims the CRAN check without weakening our own
# CI (devtools / covr / r-lib-actions all set NOT_CRAN=true).
skip_on_cran()

shp_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}

lv <- function(t, v) as.character(t$levels)[as.character(t$var) == v]

# ---- the vocabulary -----------------------------------------------------------------------------

test_that("`shape` refuses everything outside its closed vocabulary, naming the variable", {
  d <- shp_data()
  expect_error(tab_reg(d, "married", c("race", "age"), family = "binomial",
                       shape = c(age = "cubic")), "one of")
  expect_error(tab_reg(d, "married", c("race", "age"), family = "binomial",
                       shape = c(nope = "quadratic")), "predictor")
  # a factor has no functional form to mis-specify
  expect_error(tab_reg(d, "married", c("race", "age"), family = "binomial",
                       shape = c(race = "quadratic")), "continuous")
  # unnamed: there is no "apply to everything" form -- a shape is a statement about ONE variable
  expect_error(tab_reg(d, "married", c("race", "age"), family = "binomial",
                       shape = "quadratic"), "NAMED")
  # log needs strictly positive values (tvhours has zeros)
  expect_error(tab_reg(d, "married", c("race", "tvhours"), family = "binomial",
                       shape = c(tvhours = "log")), "positive")
})

test_that('shape = "linear" is byte-identical to no shape at all', {
  skip_if_not_installed("broom")
  d  <- shp_data()
  t0 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  t1 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 shape = c(age = "linear")))
  expect_identical(as.character(t0$levels), as.character(t1$levels))
  expect_equal(get_or(t0[["Model_OR"]]), get_or(t1[["Model_OR"]]))
})

# ---- quadratic ----------------------------------------------------------------------------------

test_that('shape = "quadratic" gives the predictor two rows, both fitted and both estimable', {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                shape = c(age = "quadratic"), stats = FALSE))
  labs <- lv(t, "age")
  expect_length(labs, 2L)
  expect_match(labs[[1]], "^age \\(per 1 SD")
  expect_match(labs[[2]], "^age\u00b2")               # the curvature row, "age" + SUPERSCRIPT TWO
  or <- get_or(t[["Model_OR"]])[as.character(t$var) == "age"]
  expect_true(all(is.finite(or)))
  # the squared term does NOT get the per-SD relabel: it is already per 1 SD^2 by construction
  expect_false(grepl("per", labs[[2]]))
})

test_that('the quadratic pair matches a hand-built glm, and centring keeps its VIF low', {
  skip_if_not_installed("broom")
  d  <- shp_data()
  t  <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 multiplier = 1, shape = c(age = "quadratic"), stats = FALSE))
  dm <- tidyr::drop_na(d, dplyr::all_of(c("married", "race", "age")))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  m  <- mean(dm$age); s <- stats::sd(dm$age)
  ref <- stats::glm(married ~ race + age + I(((age - m) / s)^2), data = dm,
                    family = stats::binomial())
  got <- get_or(t[["Model_OR"]])[as.character(t$var) == "age"]
  expect_equal(unname(got), unname(exp(stats::coef(ref)[4:5])), tolerance = 1e-6)
  # centring is not cosmetic: uncentred, the pair's own VIF is ~40 and the Collinearity check would
  # flag every curved model as broken
  skip_if_not_installed("car")
  expect_lt(max(car::vif(ref)), 5)
})

test_that("a curved predictor keeps its observed twin: the crude fit takes the SAME shape", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                shape = c(age = "quadratic"), empirical = TRUE, stats = FALSE))
  obs <- get_or(t[["Obs_OR"]])[as.character(t$var) == "age"]
  expect_length(obs, 2L)
  expect_true(all(is.finite(obs)))                  # BOTH rows, not just the slope
  # and it is the univariable fit of the same two terms
  dm <- tidyr::drop_na(d, dplyr::all_of(c("married", "race", "age")))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  m <- mean(dm$age); s <- stats::sd(dm$age)
  ref <- stats::glm(married ~ age + I(((age - m) / s)^2), data = dm, family = stats::binomial())
  expect_equal(unname(obs[[2]]), unname(exp(stats::coef(ref)[[3]])), tolerance = 1e-6)
})

test_that("the marginal path keeps ONE row per predictor (an AME already integrates the curvature)", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                shape = c(age = "quadratic"), effect = "marginal", stats = FALSE))
  expect_length(lv(t, "age"), 1L)
})

test_that("a cured predictor gets no Linearity row (its remedy is already in the model)", {
  skip_if_not_installed("broom")
  d  <- shp_data()
  # Phase 20f: Linearity refits, so it is opt-in (REG_CHECKS$cost == "refit")
  t0 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 stats = c("n", "linearity")))
  tt <- get_test(t0)
  expect_true(any(grepl("^linearity", tt$test) & tt$var == "age"))
  t1 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 stats = c("n", "linearity"), shape = c(age = "quadratic")))
  t1t <- get_test(t1)
  expect_false(any(grepl("^linearity", t1t$test) & t1t$var == "age"))
})

# ---- quantile groups + transforms ----------------------------------------------------------------

test_that("quantile groups turn the predictor into a factor, with the whole factor machinery", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                shape = c(age = "quintiles"), empirical = TRUE, stats = FALSE))
  labs <- lv(t, "age")
  expect_length(labs, 5L)                            # one row per group...
  expect_match(labs[[1]], "^\\[")                    # ...labelled by its own interval
  # a factor's crude twin is SATURATED, so the observed level is filled per group
  expect_true(all(is.finite(get_pct(t[["Obs_OR"]])[as.character(t$var) == "age"])))
  # and the predictor kind is STORED as what it now is
  expect_identical(reg_call(t)$predictor_types[["age"]], "factor")
  # an integer is the same request
  t4 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 shape = c(age = 4), stats = FALSE))
  expect_length(lv(t4, "age"), 4L)
})

test_that('shape = "sqrt" fits the transformed column and says so in the label', {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "tvhours"), family = "binomial",
                                multiplier = 1, shape = c(tvhours = "sqrt"), stats = FALSE))
  expect_match(lv(t, "tvhours"), "^sqrt\\(tvhours\\)")
  dm <- tidyr::drop_na(d, dplyr::all_of(c("married", "race", "tvhours")))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  ref <- stats::glm(married ~ race + sqrt(tvhours), data = dm, family = stats::binomial())
  expect_equal(unname(get_or(t[["Model_OR"]])[as.character(t$var) == "tvhours"]),
               unname(exp(stats::coef(ref)[["sqrt(tvhours)"]])), tolerance = 1e-6)
})

# ---- the primitives ------------------------------------------------------------------------------

test_that("rd_wquantile() weights, and reproduces stats::quantile() unweighted", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  expect_equal(tabxplor:::rd_wquantile(x, 0.5), stats::median(x), tolerance = 1e-8)
  # weighting the top half twice must push the median up
  w <- c(rep(1, 5), rep(2, 5))
  expect_gt(tabxplor:::rd_wquantile(x, 0.5, w), tabxplor:::rd_wquantile(x, 0.5))
})

test_that("rd_bin() is stats::weighted.mean() per bin, and its band is the theoretical one", {
  set.seed(1)
  x <- stats::runif(500); y <- stats::rbinom(500, 1, 0.4); w <- stats::runif(500, 0.5, 2)
  b <- tabxplor:::rd_bin(x, y, w, 5L, "identity")
  expect_equal(nrow(b), 5L)
  # bin 1 by hand
  br <- unique(tabxplor:::rd_wquantile(x, seq(0, 1, length.out = 6L), w))
  br[[1]] <- min(x) - 1e-9; br[[length(br)]] <- max(x) + 1e-9
  g  <- findInterval(x, br, rightmost.closed = TRUE)
  expect_equal(b$y[[1]], stats::weighted.mean(y[g == 1], w[g == 1]), tolerance = 1e-10)
  expect_equal(b$n[[1]], sum(w[g == 1]), tolerance = 1e-10)
  # the logit link uses the theoretical sqrt(1/(n p (1-p))), not arm's empirical sd(y)/sqrt(n)
  bl <- tabxplor:::rd_bin(x, y, w, 5L, "logit")
  # Phase 18z16-iv (W-G.4): the bin's effective base is the package's EXACT flat closed form on
  # the weights (svy_flat_neff_rows), not the hand-rolled Kish that stood here -- the last surviving
  # use of the formula z16 retired. Same device as every cell base: ne = p(1-p) / Var_flat(mean).
  k  <- g == 1
  my <- stats::weighted.mean(y[k], w[k])
  ne <- tabxplor:::svy_flat_neff_rows(w[k], y[k], rep(1, sum(k)), length(y),
                                      num = my * (1 - my))
  p  <- (my * ne + 0.5) / (ne + 1)
  expect_equal(bl$se[[1]], sqrt(1 / (ne * p * (1 - p))), tolerance = 1e-10)
  # non-vacuous: the exact form and Kish genuinely differ on these weights
  expect_false(isTRUE(all.equal(ne, sum(w[k])^2 / sum(w[k]^2), tolerance = 1e-6)))
  # UNWEIGHTED is byte-unchanged: Kish at equal weights IS n, so the bands do not move
  bu <- tabxplor:::rd_bin(x, y, NULL, 5L, "logit")
  gu <- findInterval(x, {
    b0 <- unique(tabxplor:::rd_wquantile(x, seq(0, 1, length.out = 6L)))
    b0[[1]] <- min(x) - 1e-9; b0[[length(b0)]] <- max(x) + 1e-9; b0
  }, rightmost.closed = TRUE)
  n1 <- sum(gu == 1); m1 <- mean(y[gu == 1]); p1 <- (m1 * n1 + 0.5) / (n1 + 1)
  expect_equal(bu$se[[1]], sqrt(1 / (n1 * p1 * (1 - p1))), tolerance = 1e-10)
})

test_that("rd_bin()'s band takes the DESIGN variance when a design is given (W-G.4)", {
  skip_if_not_installed("survey")
  set.seed(4)
  n  <- 900
  cl <- rep(seq_len(45), each = 20)
  d  <- data.frame(psu = factor(cl), x = stats::runif(n))
  d$w <- exp(stats::rnorm(n, 0, .4)); d$w <- d$w / mean(d$w)
  d$y <- stats::rbinom(n, 1, stats::plogis(-0.2 + stats::rnorm(45, 0, 1)[cl] + d$x))
  des <- survey::svydesign(ids = ~psu, weights = ~w, data = d)
  dr  <- seq_len(n)                                    # the frame IS the design's rows here
  flat <- tabxplor:::rd_bin(d$x, d$y, d$w, 5L, "identity")
  dsg  <- tabxplor:::rd_bin(d$x, d$y, d$w, 5L, "identity", design = des, des_rows = dr)
  expect_equal(dsg$y, flat$y)                          # the ESTIMATE is the same; only the band moves
  expect_false(isTRUE(all.equal(dsg$se, flat$se)))
  expect_true(mean(dsg$se) > mean(flat$se))            # clustering widens it
  # and it IS survey's own number: ne = Var_srs / Var_design, so se == SE(svymean) on the bin's domain
  br <- unique(tabxplor:::rd_wquantile(d$x, seq(0, 1, length.out = 6L), d$w))
  br[[1]] <- min(d$x) - 1e-9; br[[length(br)]] <- max(d$x) + 1e-9
  g  <- pmax(pmin(findInterval(d$x, br, rightmost.closed = TRUE), length(br) - 1L), 1L)
  sv <- survey::SE(survey::svymean(~y, subset(des, g == 1)))
  expect_equal(unname(as.numeric(dsg$se[[1]])), unname(as.numeric(sv)), tolerance = 1e-6)
})

test_that("rd_spark() reads as the shape, and distinguishes a line from a saturating curve", {
  line <- tabxplor:::rd_spark(seq(0, 1, length.out = 10))
  sat  <- tabxplor:::rd_spark(c(0, .6, .85, .95, .99, 1, 1, 1, 1, 1))
  expect_false(identical(line, sat))
  expect_equal(nchar(line), 10L)
  expect_match(line, "^\u2581")                       # lowest bin -> the lowest block
  expect_match(line, "\u2588$")                       # highest bin -> the full block
  # constant input must not divide by zero
  expect_equal(nchar(tabxplor:::rd_spark(rep(3, 5))), 5L)
  # the ASCII fallback keeps the length and the ordering, without block glyphs
  a <- tabxplor:::rd_spark(seq(0, 1, length.out = 10), "ascii")
  expect_equal(nchar(a), 10L)
  expect_false(grepl("[\u2581-\u2588]", a))
  expect_identical(tabxplor:::rd_spark(seq(0, 1, length.out = 10), FALSE), NA_character_)
})

test_that("rd_qq()'s analytic band brackets a correct model", {
  set.seed(2)
  r <- stats::rnorm(500)
  q <- tabxplor:::rd_qq(r)
  expect_true(all(q$lo <= q$hi))
  # POINTWISE coverage is ~95 % PER POSITION, i.e. over replicates -- not within one sample, where
  # consecutive order statistics are highly correlated and a single draw can sit outside for a long
  # stretch (measured 0.85 on one seed). So the property is tested across replicates, as it is defined.
  cov <- vapply(1:20, function(s) {
    set.seed(100 + s)
    qq <- tabxplor:::rd_qq(stats::rnorm(200))
    mean(qq$sample >= qq$lo & qq$sample <= qq$hi)
  }, numeric(1))
  expect_gt(mean(cov), 0.9)
  # the band IS the beta order-statistic one
  n <- length(r); i <- seq_len(n)
  expect_equal(tabxplor:::rd_qq(r, max_pts = n)$lo,
               stats::qnorm(stats::qbeta(0.025, i, n - i + 1)), tolerance = 1e-10)
})

test_that("rd_resid() is standard normal under a correct model, and refuses a multinomial", {
  skip_if_not_installed("broom")
  set.seed(3)
  n <- 800
  x <- stats::rnorm(n)
  y <- stats::rbinom(n, 1, stats::plogis(0.3 * x))
  f <- stats::glm(y ~ x, family = stats::binomial())
  r <- tabxplor:::rd_resid(f, "binomial", y)
  expect_length(r, n)
  expect_true(all(is.finite(r)))                     # qnorm(1) = Inf if u is not clamped
  expect_lt(stats::ks.test(r, "pnorm")$statistic, 0.06)
  expect_null(tabxplor:::rd_resid(f, "multinomial", y))
})

# ---- the sparkline in the row label ---------------------------------------------------------------

test_that("a continuous predictor's row carries its observed shape, and the option turns it off", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE))
  expect_match(lv(t, "age"), "[\u2581-\u2588]{3,}")
  # a factor row never gets one
  expect_false(any(grepl("[\u2581-\u2588]", lv(t, "race"))))
  withr::with_options(list(tabxplor.spark = FALSE), {
    t0 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                   stats = FALSE))
    expect_false(any(grepl("[\u2581-\u2588]", as.character(t0$levels))))
  })
})

test_that("the curve is the MODELLED level's, not the factor's first level", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", "age", family = "binomial", stats = FALSE))
  a <- get_assumptions(t)
  expect_identical(a$outcome, "married")
  expect_identical(a$link, "logit")
  # P(married) RISES with age over most of the range; reading the complement would invert it
  y <- a$curves$age$y
  expect_gt(y[[length(y)]], y[[1]])
  # ten bins, and the sparkline printed is this curve
  expect_equal(nrow(a$curves$age), 10L)
  expect_true(grepl(tabxplor:::rd_spark(y), lv(t, "age"), fixed = TRUE))
})

test_that("with several outcomes there is no single observed shape, so there is no sparkline", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, c("married", "tvhours"), "age",
                                family = c(married = "binomial", tvhours = "gaussian"),
                                stats = FALSE))
  expect_null(get_assumptions(t))
  expect_false(any(grepl("[\u2581-\u2588]", as.character(t$levels))))
})

test_that("the html engine upgrades the glyph run to an inline <svg>; the plot medium drops it", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                stats = FALSE))
  h <- paste(as.character(tab_html(t)), collapse = "\n")
  expect_true(grepl("<svg class=\"tx-spark\"", h, fixed = TRUE))
  expect_true(grepl("stroke=\"currentColor\"", h, fixed = TRUE))
  expect_false(grepl("[\u2581-\u2588]", h))          # not escaped into literal text either
  # a graphics device has no block glyphs: the plot medium strips them (mbcsToSbcs failures)
  expect_identical(tabxplor:::tx_spark_strip("age (per 1 SD)\u00a0\u2581\u2586\u2588"),
                   "age (per 1 SD)")
  # markdown and the console KEEP them
  expect_true(grepl("[\u2581-\u2588]", tab_md(t)))
})
