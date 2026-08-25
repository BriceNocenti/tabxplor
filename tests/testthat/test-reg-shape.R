# Phase 18z15-ii / z15-iii -- `shape =` (the cure for a non-linearity), the observed curves and the
# row sparkline (R/reg-assumptions.R + the wiring in R/tab_reg.R).
#
# The fixtures are the ones that would fail if a decision were quietly reverted, per the design's SS20:
# the crude twin's term names must be IDENTICAL to the model's, centring must keep the pair's VIF low,
# `shape = "linear"` must be byte-identical to no shape at all, and `shape_table = FALSE` must
# restore the old label byte-for-byte.
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
  # unnamed IS the "every continuous predictor" form, the shared per-predictor grammar's default
  t <- suppressWarnings(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                shape = "quadratic", stats = FALSE))
  expect_true(any(as.character(t$levels) == "age\u00b2"))
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
  expect_match(labs[[1]], "^per [0-9.]+ \\(2SD\\)")
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
                                shape = c(age = "quadratic"), effect = "marginal", measure = "difference", stats = FALSE))
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
  # ...labelled by its own values (`age` is whole-numbered: Phase 22g-v) or by its interval
  expect_match(labs[[1]], "^([0-9]+( (to|or) [0-9]+)?|\\[)")
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
  expect_match(lv(t, "tvhours"), "^\u221a\\(x\\)")   # the shape names itself, the var column names x
  dm <- tidyr::drop_na(d, dplyr::all_of(c("married", "race", "tvhours")))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  ref <- stats::glm(married ~ race + sqrt(tvhours), data = dm, family = stats::binomial())
  expect_equal(unname(get_or(t[["Model_OR"]])[as.character(t$var) == "tvhours"]),
               unname(exp(stats::coef(ref)[["sqrt(tvhours)"]])), tolerance = 1e-6)
})

# ---- the primitives ------------------------------------------------------------------------------

test_that("shape_wquantile() weights, and reproduces stats::quantile() unweighted", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  expect_equal(tabxplor:::shape_wquantile(x, 0.5), stats::median(x), tolerance = 1e-8)
  # weighting the top half twice must push the median up
  w <- c(rep(1, 5), rep(2, 5))
  expect_gt(tabxplor:::shape_wquantile(x, 0.5, w), tabxplor:::shape_wquantile(x, 0.5))
})

test_that("rd_bin() is stats::weighted.mean() per bin, and its band is the theoretical one", {
  set.seed(1)
  x <- stats::runif(500); y <- stats::rbinom(500, 1, 0.4); w <- stats::runif(500, 0.5, 2)
  b <- tabxplor:::rd_bin(x, y, w, 5L, "identity")
  expect_equal(nrow(b), 5L)
  # bin 1 by hand
  br <- unique(tabxplor:::shape_wquantile(x, seq(0, 1, length.out = 6L), w))
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
    b0 <- unique(tabxplor:::shape_wquantile(x, seq(0, 1, length.out = 6L)))
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
  br <- unique(tabxplor:::shape_wquantile(d$x, seq(0, 1, length.out = 6L), d$w))
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

# ---- the sparkline in the base-count cell ----------------------------------------------------------

# what the base-count column PRINTS, per row: where the sparkline lives since Phase 22b-v (a
# continuous predictor has no level population, so that cell is empty by construction).
nprint <- function(t, v) {
  m <- suppressMessages(tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE))
  nc <- names(m)[purrr::map_lgl(m, ~ is_fmt(.) && get_role(.) == "n")]
  vapply(nc, function(cl) paste(format(m[[cl]], na = "")[as.character(m$var) == v], collapse = " "),
         character(1))
}

test_that("a continuous predictor gets its observed shape, in a table of its own", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE))
  st <- tabxplor:::reg_shape_table(t)
  # ⚠ ONE row per CONTINUOUS predictor: a factor has a real count and a level of its own to read
  expect_identical(st$var, "age")
  expect_match(st$shape, "[\u2581-\u2588]{3,}")
  # ... and NOTHING in the table itself -- neither the row label nor the count cell (22b-xviii-ii)
  expect_false(grepl("[\u2581-\u2588]", lv(t, "age")))
  expect_false(any(grepl("[\u2581-\u2588]", nprint(t, "age"))))
  expect_false(any(grepl("[\u2581-\u2588]", nprint(t, "race"))))
})

test_that("`options(tabxplor.shape_table =)` chooses where the shape table is drawn", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE))
  want <- function(...) c(console = tab_wants_shape_table(t, "console"),
                          kable   = tab_wants_shape_table(t, "kable"))
  expect_identical(want(), c(console = TRUE, kable = TRUE))                       # the default
  withr::with_options(list(tabxplor.shape_table = "console"),
                      expect_identical(want(), c(console = TRUE,  kable = FALSE)))
  withr::with_options(list(tabxplor.shape_table = "no"),
                      expect_identical(want(), c(console = FALSE, kable = FALSE)))
  # TRUE / FALSE are the historical spelling of "all" / "no" and keep working
  withr::with_options(list(tabxplor.shape_table = TRUE),
                      expect_identical(want(), c(console = TRUE,  kable = TRUE)))
  withr::with_options(list(tabxplor.shape_table = FALSE),
                      expect_identical(want(), c(console = FALSE, kable = FALSE)))
  # ⚠ a mistyped display option must never silently remove content
  withr::with_options(list(tabxplor.shape_table = "yes please"),
                      expect_identical(want(), c(console = TRUE,  kable = TRUE)))
  # a plot never draws block glyphs, whatever the option says
  expect_false(tab_wants_shape_table(t, "plot"))
  # the development spelling is kept as an alias, and an alias is read FIRST
  withr::with_options(list(tabxplor.spark = "no"),
                      expect_identical(want(), c(console = FALSE, kable = FALSE)))
})

test_that("the curve is the MODELLED level's, not the factor's first level", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", "age", family = "binomial", stats = FALSE))
  # ⚠ ONE RECORD PER OUTCOME, keyed by it (22b-xviii)
  a <- get_assumptions(t)[["married"]]
  expect_identical(a$outcome, "married")
  expect_identical(a$link, "logit")
  # P(married) RISES with age over most of the range; reading the complement would invert it
  y <- a$curves$age$y
  expect_gt(y[[length(y)]], y[[1]])
  # ten bins, and the sparkline printed is this curve
  expect_equal(nrow(a$curves$age), 10L)
  # the drawn run is the curve RESAMPLED onto its own x axis, so it is read from the curve
  expect_identical(tabxplor:::reg_shape_table(t)$shape, tabxplor:::rd_spark(a$curves$age, n = 20L))
})

test_that("several outcomes get one curve EACH, and the shape table rather than a cell", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, c("married", "tvhours"), "age",
                                family = c(married = "binomial", tvhours = "gaussian"),
                                stats = FALSE))
  a <- get_assumptions(t)
  expect_identical(names(a), c("married", "tvhours"))
  expect_identical(a$married$kind,  "logit")
  expect_identical(a$tvhours$kind,  "mean")
  # the base-count column is shared by both outcomes, so no cell of it could carry either curve
  expect_true(tab_wants_shape_table(t, "html"))
  m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE, medium = "html")
  disp <- unlist(purrr::map(m[purrr::map_lgl(m, is_fmt)], get_display))
  expect_false(any(grepl("[\u2581-\u2588]", disp)))
  st <- tabxplor:::reg_shape_table(t)
  expect_identical(nrow(st), 2L)
  # ONE ROW PER OUTCOME, each naming its own -- on the scale its own family fits, so the two rows
  # are not even in the same units (a log-odds and a mean).
  expect_match(st$outcome[[1L]], "Married", fixed = TRUE)
  expect_match(st$outcome[[2L]], "tvhours", fixed = TRUE)
  expect_true(all(grepl("[\u2581-\u2588]{3,}", st$shape)))
  expect_false(any(grepl("[\u2581-\u2588]", as.character(t$levels))))
})

test_that("NO medium puts a glyph run in a cell -- the cell route is dormant", {
  skip_if_not_installed("broom")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE))
  expect_false(tabxplor:::SPARK_IN_CELL)
  for (md in c("console", "kable", "md", "xl")) {
    m <- tabxplor:::tab_materialize_extras(t, backend = if (md == "xl") "xl" else "text",
                                           pvalue = FALSE, medium = md)
    expect_false(any(grepl("[\u2581-\u2588]", get_display(m[["n"]]))), label = md)
  }
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

# ---- the composed unit label, and the sparkline that follows the shape ---------------------------

test_that("a continuous row's level is COMPOSED: shape, unit, anchor -- and none overwrites another", {
  skip_if_not_installed("broom")
  d <- shp_data()
  f <- function(...) suppressMessages(
    tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE, ...))
  expect_match(lv(f(), "age"), "^per [0-9.]+ \\(2SD\\), at [0-9.]+ \\(mean\\)$")
  expect_match(lv(f(multiplier = c(age = "sd")), "age"), "^per [0-9.]+ \\(SD\\),")
  expect_match(lv(f(multiplier = c(age = 10), ref = c(age = 0)), "age"), "^per 10, at 0$")
  # the shape used to be written first and then OVERWRITTEN by the unit, so it was invisible under
  # the default multiplier -- reachable only with multiplier = 1
  expect_match(lv(f(shape = c(age = "log")),  "age"), "^log\\(x\\), per ")
  expect_match(lv(f(shape = c(age = "sqrt")), "age"), "^\u221a\\(x\\), per ")
})

test_that("the sparkline is drawn on the model's own x axis: one width, and the shape moves it", {
  skip_if_not_installed("broom")
  d  <- shp_data()
  # ⚠ `gl()` looks the curve up BY ITS `var` CELL, which a transform now marks (`log(age)`). It
  # therefore asserts nothing unless the row is found: without the expect_length() below, a lookup
  # that silently returns character(0) makes every expect_false(identical(...)) here pass VACUOUSLY.
  gl <- function(t, v) {
    st <- tabxplor:::reg_shape_table(t)
    out <- st$shape[st$var == v]
    expect_length(out, 1L)
    out
  }
  t0 <- suppressMessages(tab_reg(d, "married", c("age", "tvhours"), family = "binomial",
                                 stats = FALSE))
  # EVERY predictor gets the same number of glyphs -- the run's length is the grid's, not the data's
  expect_identical(nchar(gl(t0, "age")), nchar(gl(t0, "tvhours")))
  expect_identical(nchar(gl(t0, "age")), 20L)   # the shape table's own width
  # ...and a monotone shape CHANGES the curve, which is the whole point of drawing it to scale
  tl <- suppressMessages(tab_reg(d, "married", c("age", "tvhours"), family = "binomial",
                                 shape = c(age = "log"), stats = FALSE))
  expect_false(identical(gl(t0, "age"), gl(tl, "log(age)")))
  expect_identical(gl(t0, "tvhours"), gl(tl, "tvhours"))   # the untouched predictor does not move
})

test_that("the shape table names the transform it DREW, and only that", {
  skip_if_not_installed("broom")
  d  <- shp_data()
  f  <- function(...) suppressMessages(
    tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE, ...))
  sv <- function(t) tabxplor:::reg_shape_table(t)$var
  gl <- function(t) tabxplor:::reg_shape_table(t)$shape

  expect_identical(sv(f()), "age")                              # nothing done, nothing said
  expect_identical(sv(f(shape = c(age = "log"))),  "log(age)")   # a RECODE is part of the curve
  expect_identical(sv(f(shape = c(age = "sqrt"))), "\u221a(age)")
  # ⚠ A QUADRATIC IS A MODEL TERM, NOT A RECODE: it must leave the column bare AND the curve
  # untouched. Marking it would promise a "is it straighter?" reading the drawing cannot support --
  # judge a quadratic with reg_check_plots(check = "linearity") instead. Do not "fix" this.
  expect_identical(sv(f(shape = c(age = "quadratic"))), "age")
  expect_identical(gl(f(shape = c(age = "quadratic"))), gl(f()))
  # a CUT leaves no numeric predictor at all, so it has no row here
  expect_null(tabxplor:::reg_shape_table(f(shape = c(age = "quartiles"))))
})

test_that("the mark survives the tab_vars merge, and the curve keys stay bare", {
  skip_if_not_installed("broom")
  d  <- shp_data()
  t  <- suppressMessages(tab_reg(d, "married", c("rincome", "age"), tab_vars = "race",
                                 family = "binomial", shape = c(age = "log"), stats = FALSE))
  st <- tabxplor:::reg_shape_table(t)
  expect_gt(nrow(st), 1L)                       # one row per group (reg_bind_assumptions path)
  expect_true(all(st$var == "log(age)"))
  # ⚠ the MARK is a display cell; names(curves) are the keys reg_bind_assumptions(), linear_level
  # and mat_reg_spark() all match on, so they must stay the raw column name.
  a <- get_assumptions(t)
  expect_identical(names(a[[1]]$curves), "age")
})

# ---- Phase 22b-xviii: the vertical window has a floor ------------------------------------------

test_that("a curve smaller than its own noise reads FLAT, and a real one uses the height", {
  skip_if_not_installed("broom")
  set.seed(20260822)
  n     <- 400
  noise <- data.frame(x = stats::rnorm(n), g = factor(sample(c("a", "b"), n, TRUE)))
  noise$y <- factor(sample(c("yes", "no"), n, TRUE))
  tn <- suppressMessages(tab_reg(noise, "y", c("g", "x"), family = "binomial", stats = FALSE))
  gn <- tabxplor:::reg_shape_table(tn)
  lv <- function(run) match(strsplit(run, "")[[1]], tabxplor:::rd_spark_glyphs())
  # pure noise stays in the middle of the run: it never reaches both ends
  expect_lt(diff(range(lv(gn$shape[[1]]))), 7L)
  # ... and the range column MARKS it: grey plus "ns", the package's own non-significant pair
  expect_match(gn$range[[1]], " ns$")
  expect_true(attr(gn, "noisy")[[1]])

  m <- 4000
  real <- data.frame(x = stats::rnorm(m), g = factor(sample(c("a", "b"), m, TRUE)))
  real$y <- factor(ifelse(stats::rbinom(m, 1, stats::plogis(real$x)) == 1, "yes", "no"))
  tr <- suppressMessages(tab_reg(real, "y", c("g", "x"), family = "binomial", stats = FALSE))
  gr <- tabxplor:::reg_shape_table(tr)
  expect_identical(diff(range(lv(gr$shape[[1]]))), 7L)   # a real effect spends every level
  expect_false(grepl(" ns$", gr$range[[1]]))
  expect_false(attr(gr, "noisy")[[1]])
})

test_that("the window's floor is the first colour rung, read on the curve's own scale", {
  # every ladder is the SAME ladder at a 50 % reference, so the rung converts exactly
  expect_equal(tabxplor:::rd_link_rung("logit",   c(0, 1), NULL), log(1.2))
  expect_equal(tabxplor:::rd_link_rung("logrisk", c(0, 1), NULL), log(1.1))
  expect_equal(tabxplor:::rd_link_rung("logmean", c(0, 1), NULL), log(1.1))
  # a probability and a mean share ONE rule -- 0.1 SD, which at p = 0.5 IS 5 points
  y <- rep(c(0, 1), 500)
  expect_equal(tabxplor:::rd_link_rung("risk", y, NULL), 0.05, tolerance = 1e-3)
  expect_equal(tabxplor:::rd_link_rung("mean", c(rep(0, 500), rep(10, 500)), NULL), 0.5,
               tolerance = 1e-3)
})

# ---- Phase 22b-xviii (ii): the observed range beside the picture --------------------------------

test_that("the observed range is the curve's own low and high, back on the outcome's scale", {
  skip_if_not_installed("broom")
  d <- suppressWarnings(gss_cat_data_formatting())
  rg <- function(...) tabxplor:::reg_shape_table(
    suppressMessages(suppressWarnings(tab_reg(d, ..., stats = FALSE))))$range
  # a LEVEL a reader can name, whatever measure the analyst asked for: the same curve, three links
  expect_match(rg("married", "age", family = "binomial"),                    "^13-57% ")
  expect_match(rg("married", "age", family = "binomial", link = "ratio"),    "^13-57% ")
  expect_match(rg("married", "age", family = "binomial", link = "difference"), "^13-57% ")
  # the unit is written ONCE, at the end -- it reads as a range, not as two numbers
  expect_false(any(grepl("%-", rg("married", "age", family = "binomial"))))
  # the effect in parentheses is the LINK's own measure, not the reported one
  expect_match(rg("married", "age", family = "binomial"),                     "\\(OR [0-9.]+\\)$")
  expect_match(rg("married", "age", family = "binomial", link = "ratio"),     "\\(\u00d7[0-9.]+\\)$")
  expect_match(rg("married", "age", family = "binomial", link = "difference"), "\\(\\+[0-9]+%\\)$")
  expect_match(rg("tvhours", "age", family = "poisson"),                      "\\(\u00d7[0-9.]+\\)$")
  expect_match(rg("age", "tvhours", family = "gaussian"),                     "\\(\\+[0-9.]+ SD\\)$")
  # ⚠ an ODDS RATIO is the one measure rendered with no glyph, so it is the only one NAMED
  expect_match(rg("rincome", "age"), "\\(cumOR [0-9.]+\\)$")
})

test_that("the range travels with the picture, in the same row of the shape table", {
  skip_if_not_installed("broom")
  d <- suppressWarnings(gss_cat_data_formatting())
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE))
  st <- tabxplor:::reg_shape_table(t)
  expect_match(st$range, "^13-57% \\(OR [0-9.]+\\)$")
  expect_match(st$shape, "^[\u2581-\u2588]{20}$")
  # the table itself carries neither, in any medium
  for (md in c("console", "kable")) {
    m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE, medium = md)
    expect_false(any(grepl("[\u2581-\u2588]|13-57%", get_display(m[["n"]]))), label = md)
  }
})

# 22g-ii retired the shape table's footer prose (the window is in the header, the units are in the
# `range` cell). The one caveat that is not verbosity -- an ordinal or multinomial outcome has one
# curve per cut and this draws the first -- lives in the OUTCOME cell, where it is read. 22g-xii made
# that cell the link-scale FORMULA, and `%x not 1st` now says it inside the formula itself.
test_that("an ordinal outcome says which curve it is drawing", {
  skip_if_not_installed("MASS")
  d  <- suppressWarnings(gss_cat_data_formatting())
  st <- tabxplor:::reg_shape_table(
    suppressMessages(suppressWarnings(tab_reg(d, "rincome", "age", stats = FALSE))))
  # ⚠ it must ALSO name the outcome: with several of them, two "not 1st" rows would be identical
  expect_true(any(grepl("not 1st", st$outcome, fixed = TRUE)))
  expect_true(any(grepl("rincome", st$outcome, fixed = TRUE)))
  # a binomial one does not: its single curve IS the whole reading, so it names its modelled level
  st2 <- tabxplor:::reg_shape_table(
    suppressMessages(tab_reg(d, "married", "age", family = "binomial", stats = FALSE)))
  expect_false(any(grepl("not 1st", st2$outcome, fixed = TRUE)))
  expect_true(any(grepl("Married", st2$outcome, fixed = TRUE)))
  # ...and the only note left is the "ns" one, on the tables that actually wear the mark
  expect_true(all(grepl("ns", attr(st, "note"), fixed = TRUE)))
})

test_that("the first column is the outcome on the model's own scale, one form per link", {
  skip_if_not_installed("broom")
  d  <- suppressWarnings(gss_cat_data_formatting())
  y  <- function(t) tabxplor:::reg_shape_table(t)$outcome[[1L]]
  b  <- function(...) suppressMessages(tab_reg(d, "married", "age", family = "binomial",
                                               stats = FALSE, ...))
  # the three readings of ONE binary outcome, each on the scale its own link fits
  expect_identical(y(b()),                     "log(%Married / (1 - %Married))")
  expect_identical(y(b(link = "ratio")),       "log(%Married)")
  expect_identical(y(b(link = "difference")),  "%Married")
  # a number is a mean, logged exactly where its link logs it
  expect_identical(y(suppressMessages(tab_reg(d, "age", "tvhours", stats = FALSE))),
                   "mean age")
  expect_identical(y(suppressWarnings(suppressMessages(
    tab_reg(d, "tvhours", "age", family = "poisson", stats = FALSE)))), "log(mean tvhours)")
  # ⚠ the same quantity the linearity panel puts on its y axis -- one fact, two renderings
  expect_identical(tabxplor:::rd_link_text("logit", "married", "01-Married"),
                   "log(%Married / (1 - %Married))")
})

test_that("the drawing floor and the noise mark are two different verdicts", {
  # a precisely measured but negligible curve is damped, and is NOT marked: it is not noise, and
  # its own range says it is nothing.
  cu <- tibble::tibble(x = 1:10, y = seq(-0.05, 0.05, length.out = 10), n = 4000,
                       se = rep(0.001, 10), xlo = 1, xhi = 10, rung = log(1.2))
  w  <- tabxplor:::rd_spark_window(cu)
  expect_true(w$flat)                       # under the first colour rung -> drawn damped
  expect_false(w$noisy)                     # but far outside its own standard errors
  # the same curve measured on very little data IS noise
  cu$se <- rep(0.05, 10)
  expect_true(tabxplor:::rd_spark_window(cu)$noisy)
})

# ---- Phase 22g-v ------------------------------------------------------------------------------

test_that("the shape table names EVERY group, not only the first variable's", {
  skip_if_not_installed("broom")
  d  <- shp_data()
  t  <- suppressMessages(tab_reg(d, "married", c("race", "age", "tvhours"), tab_vars = "relig",
                                 family = "binomial", empirical = FALSE, stats = "no"))
  st <- tabxplor:::reg_shape_table(t)
  testthat::skip_if(is.null(st) || !"group" %in% names(st))
  # ⚠ the rows arrive VARIABLE-major, so a group is not a run until they are sorted: blanking with
  # duplicated() over the whole column sent every row of the SECOND variable into the last group's
  # block, and both `age` rows came out naming no group at all. Read the table as a reader does --
  # a blank means "same as above" -- and every (group, variable) pair must appear exactly once.
  filled <- st$group
  for (i in seq_along(filled)) if (!nzchar(filled[[i]]) && i > 1L) filled[[i]] <- filled[[i - 1L]]
  expect_true(all(nzchar(filled)))
  expect_false(anyDuplicated(paste(filled, st$var)) > 0L)
  expect_setequal(unique(filled), unique(as.character(stats::na.omit(d$relig))))
  # ...and each group holds all of its numeric predictors
  expect_true(all(table(filled) == length(unique(st$var))))
})


# Phase 22g-vi: the shape table under a PUBLICATION palette. tab_css() gives `.tx-sec` a
# `display:inline-block` there (load-bearing: it is what takes an aside out of an ancestor's
# text-decoration), and that on a <td> destroys `display:table-cell` -- the cell drops out of the
# row and reflows, which is how a curve ended up under the "outcome" header.
testthat::test_that("the shape table's grey never lands on a <td>", {
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("age", "tvhours"), family = "binomial",
                                empirical = FALSE, stats = "no"))
  h <- tabxplor:::shape_html_table(t)
  testthat::skip_if(is.null(h))
  tds <- regmatches(h, gregexpr("<td[^>]*>", h))[[1]]
  expect_false(any(grepl("tx-sec", tds)))
  # ...and the print stylesheet is what makes it matter, so the rule is asserted where it lives
  expect_match(tab_css(theme = "print_marks"), "\\.tx-sec\\{[^}]*display:inline-block")
  # the curve is still in the shape column, and it is the only <svg> in the table
  expect_match(h, "tx-sparkcell")
})
