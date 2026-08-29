# PURPOSE: the model checks and the `shape =` cures that fix what they flag.
# ROLE: the shipped CONTRACT for R/reg-assumptions.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the model checks ====================================================================

chk_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}



# The model tab_reg() actually fits for chk_data(): complete cases, the binary outcome reversed so the
# FIRST level is modelled, factors dropped. Mirrors test-tab_reg-footer.R's own reference recipe.
chk_fit <- function(preds = c("race", "age", "rincome"), family = stats::binomial()) {
  d  <- chk_data()
  dm <- tidyr::drop_na(d, dplyr::all_of(c("married", preds)))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  for (p in preds) if (is.factor(dm[[p]])) dm[[p]] <- forcats::fct_drop(dm[[p]])
  list(fit = stats::glm(stats::reformulate(preds, "married"), data = dm, family = family), data = dm)
}



# The HC0 sandwich, written out: IF = X (W r) A^-1, SE = sqrt(colSums(IF^2)). This IS what
# reg_if_se(reg_coef_if_maker(fit)(e)) computes without a design, and it is what `sandwich` would give.
chk_sandwich_se <- function(fit) {
  X  <- stats::model.matrix(fit)
  W  <- fit$weights
  r  <- stats::residuals(fit, type = "working")
  Ai <- solve(crossprod(X * sqrt(W)))
  IF <- (X * (W * r)) %*% Ai
  sqrt(colSums(IF^2))
}



# ---- Dispersion (robust / model SE) ----------------------------------------------------------

test_that("Dispersion is max(robust/model SE), equal to a hand-written HC0 sandwich", {
  f  <- chk_fit()$fit
  se_mod <- sqrt(diag(stats::vcov(f)))
  ref    <- max(chk_sandwich_se(f) / se_mod)
  # 1e-6: the sandwich is rebuilt from the digest, and glm's stored IRLS weights lag by one step
  expect_equal(tabxplor:::reg_check_dispersion(f), ref, tolerance = 1e-6)
  # a correctly-specified binomial: the two variance estimators agree to O(1/n)
  expect_lt(abs(ref - 1), 0.15)
})



# ---- Influence (max dfbetas) -----------------------------------------------------------------

test_that("Influence equals max |stats::dfbetas()|", {
  # R 4.6.0 rebuilt the glm influence measures on PEARSON residuals, with no leave-one-out dispersion
  # where the dispersion is fixed. tabxplor's engine is the new definition; on an older R the
  # reference itself is the old one, so there is nothing to compare against.
  skip_if(getRversion() < "4.6.0", "stats::dfbetas() for glm changed in R 4.6.0")
  f   <- chk_fit()$fit
  ref <- max(abs(stats::dfbetas(f)))
  got <- tabxplor:::reg_check_influence(f)
  expect_equal(got, ref, tolerance = 0.05)               # the (1 - h_i) correction, ~1 % on real data
  expect_gt(stats::cor(as.vector(abs(stats::dfbetas(f))),
                       as.vector(abs(sweep(
                         (stats::model.matrix(f) * (f$weights * stats::residuals(f, "working"))) %*%
                           solve(crossprod(stats::model.matrix(f) * sqrt(f$weights))),
                         2, sqrt(diag(stats::vcov(f))), "/")))), 0.999)
})



# ---- Collinearity (max VIF) ------------------------------------------------------------------

test_that("tx_vif() IS 1/(1 - R2) of the auxiliary regression, on the model's own weights", {
  # The textbook definition, computed the long way round: regress each model-matrix column on all
  # the others and read the inflation off its R2. It never touches vcov(), which is the only thing
  # tx_vif() reads -- so this is a genuinely independent derivation, not a restatement.
  f  <- chk_fit(preds = c("age", "tvhours"))$fit          # only 1-df terms -> the bare-vector shape
  v  <- tabxplor:::tx_vif(f)
  expect_false(is.matrix(v))
  expect_named(v, c("age", "tvhours"))
  X  <- stats::model.matrix(f)[, -1, drop = FALSE]
  w  <- f$weights                                          # ⚠ the IRLS weights: a glm's VIF is on them
  aux <- vapply(seq_len(ncol(X)), function(j)
    1 / (1 - summary(stats::lm(X[, j] ~ X[, -j, drop = FALSE], weights = w))$r.squared), 0)
  expect_equal(unname(v), aux, tolerance = 1e-10)
  expect_equal(tabxplor:::reg_check_collinearity(f), max(aux), tolerance = 1e-10)

  # and the plain OLS arm, where no weight enters at all
  l  <- stats::lm(tvhours ~ age + I(age^2 / 100), data = chk_fit()$data)
  Xl <- stats::model.matrix(l)[, -1, drop = FALSE]
  expect_equal(unname(tabxplor:::tx_vif(l)),
               vapply(1:2, function(j)
                 1 / (1 - summary(stats::lm(Xl[, j] ~ Xl[, -j]))$r.squared), 0),
               tolerance = 1e-10)
})



# ---- Linearity -------------------------------------------------------------------------------

test_that("Linearity is drop1() on the model plus the predictor's centred squared term", {
  # Phase 20f: it costs a fit, so it is asked for by name (REG_CHECKS$cost == "refit").
  t  <- suppressMessages(tab_reg(chk_data(), "married", c("race", "age", "rincome"),
                                 family = "binomial", cleannames = FALSE,
                                 stats = c("n", "linearity")))
  tt <- get_test(t)
  li <- tt[tt$test %in% tabxplor:::reg_check_types() & startsWith(tt$test, "linearity"), ]
  expect_identical(nrow(li), 1L)                          # one numeric predictor
  expect_identical(li$var, "age")
  expect_identical(li$test, "linearity_lr")

  cf <- chk_fit()
  dm <- cf$data
  dm$z <- (dm$age - mean(dm$age)) / stats::sd(dm$age)
  aug <- stats::glm(married ~ race + age + rincome + I(z^2), data = dm, family = stats::binomial())
  d1  <- stats::drop1(aug, scope = "I(z^2)", test = "Chisq")
  expect_equal(li$pvalue, d1[["Pr(>Chi)"]][2], tolerance = 1e-6)
  expect_equal(li$statistic, d1[["LRT"]][2], tolerance = 1e-6)
})



test_that("the FREE checks are the default set, the costly ones are opt-in, and stats='all' is all", {
  d <- chk_data()
  full <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial")))
  # Phase 20f: the three that are arithmetic on the fit in hand ride the default footer...
  expect_true(all(c("dispersion", "influence", "collinearity") %in% full$test))
  # ...and the one that refits does not (it was 87 % of a default call at n = 200 000)
  expect_false(any(startsWith(full$test, "linearity")))

  # a `stats =` vector takes the check KEY; only what it names survives
  one <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                           stats = c("n", "influence"))))
  expect_true("influence" %in% one$test)
  expect_false(any(c("dispersion", "collinearity") %in% one$test))
  expect_false(any(startsWith(one$test, "linearity")))

  # naming the costly one brings it back -- opting in needs no new argument
  opt <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                           stats = c("n", "linearity"))))
  expect_true(any(startsWith(opt$test, "linearity")))

  # `stats = "all"` MEANS all: strictly more than the default, and every applicable check in it.
  # (It used to be a synonym of NULL, i.e. of the default set -- a name that already lied.)
  all_t <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                             stats = "all")))
  expect_true(all(setdiff(full$test, "") %in% all_t$test))
  expect_gt(length(unique(all_t$test)), length(unique(full$test)))
  expect_true(any(startsWith(all_t$test, "linearity")))

  none <- get_test(suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                            stats = FALSE)))
  expect_false(any(tabxplor:::reg_check_types() %in% none$test))
})



# ---- the fact table is the one source ---------------------------------------------------------

test_that("REG_CHECKS drives the footer labels and the `stats =` vocabulary from one row", {
  spec <- tabxplor:::reg_footer_spec()
  for (k in names(tabxplor:::REG_CHECKS)) {
    ck <- tabxplor:::REG_CHECKS[[k]]
    for (d in names(ck$types)) {
      expect_true(d %in% names(spec))
      expect_identical(spec[[d]]$label, paste0(ck$noun, " (", ck$types[[d]], ")"))
      expect_identical(spec[[d]]$kind, ck$kind)
    }
    # the KEY is what a user writes; the discriminators are what a `test` row carries. Phase 18z15:
    # a TAUGHT-BUT-NEVER-SCORED check (residuals / normality) carries none, so it expands to nothing --
    # which is exactly how it contributes a panel and no footer row.
    if (length(ck$types)) expect_setequal(tabxplor:::reg_check_expand(k), names(ck$types))
    else                  expect_length(tabxplor:::reg_check_expand(k), 0L)
  }
})




# === SECTION: shape =, the observed curve and the shape table =====================================

skip_on_cran()



shp_data <- function() {
  fx_reg_df() |>
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



# ---- the sparkline in the base-count cell ----------------------------------------------------------

# what the base-count column PRINTS, per row: where the sparkline lives since Phase 22b-v (a
# continuous predictor has no level population, so that cell is empty by construction).
nprint <- function(t, v) {
  m <- suppressMessages(tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE))
  nc <- names(m)[purrr::map_lgl(m, ~ is_fmt(.) && get_role(.) == "n")]
  vapply(nc, function(cl) paste(format(m[[cl]], na = "")[as.character(m$var) == v], collapse = " "),
         character(1))
}



# ---- Phase 22b-xviii: the vertical window has a floor ------------------------------------------

test_that("a curve smaller than its own noise reads FLAT, and a real one uses the height", {
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



# ---- Phase 22b-xviii (ii): the observed range beside the picture --------------------------------

test_that("the observed range is the curve's own low and high, back on the outcome's scale", {
  d <- suppressWarnings(fx_reg_fmt())
  rg <- function(...) tabxplor:::reg_shape_table(
    suppressMessages(suppressWarnings(tab_reg(d, ..., stats = FALSE))))$range
  # a LEVEL a reader can name, whatever measure the analyst asked for: the same curve, three links
  expect_match(rg("married", "age", family = "binomial"),                    "^[0-9]+-[0-9]+% ")
  expect_match(rg("married", "age", family = "binomial", link = "ratio"),    "^[0-9]+-[0-9]+% ")
  expect_match(rg("married", "age", family = "binomial", link = "difference"), "^[0-9]+-[0-9]+% ")
  # the unit is written ONCE, at the end -- it reads as a range, not as two numbers
  expect_false(any(grepl("%-", rg("married", "age", family = "binomial"))))
  # the effect in parentheses is the LINK's own measure, not the reported one
  expect_match(rg("married", "age", family = "binomial"),                     "\\(OR [0-9.]+\\)$")
  expect_match(rg("married", "age", family = "binomial", link = "ratio"),     "\\(\u00d7[0-9.]+\\)$")
  expect_match(rg("married", "age", family = "binomial", link = "difference"), "\\(\\+[0-9]+%\\)$")
  expect_match(rg("tvhours", "age", family = "poisson"),                      "\\(\u00d7[0-9.]+\\)$")
  expect_match(rg("age", "tvhours", family = "gaussian"),                     "\\(\\+[0-9.]+ SD\\)$")
  # ⚠ an ODDS RATIO is the one measure rendered with no glyph, so it is the only one NAMED
  expect_match(rg("married", "age", family = "binomial"), "\\(OR [0-9.]+\\)$")

  # AN ORDINAL / MULTINOMIAL OUTCOME DRAWS NO CURVE HERE: `Y != first` is one of its K-1 readings and
  # the least trustworthy (a tiny reference category prints "99-100%"), so the row names the outcome
  # and points at the panel that draws them all.
  st <- tabxplor:::reg_shape_table(
    suppressMessages(suppressWarnings(tab_reg(d, "rincome", "age", stats = FALSE))))
  expect_equal(unique(st$range), "")
  expect_match(st$shape, "reg_check_plots", fixed = TRUE)
  expect_equal(st$outcome[[1]], "rincome")
})


test_that("the shape table's outcome cell names the subject once, in two syntaxes", {
  d <- fx_reg_df(); d$m <- as.integer(d$marital == "Married")
  yc <- function(syntax) tabxplor:::reg_shape_table(
    suppressMessages(tab_reg(d, "m", c("age", "race"), family = "binomial", stats = FALSE)),
    syntax = syntax)$outcome[[1]]
  # `p` is named, then the formula is written on the letter -- never the subject twice
  expect_match(yc("text"), "^p = %.* ; log\\(p/\\(1-p\\)\\)$")
  # the html twin sets the qualifier as a real subscript, which is what lets it be qualified at all
  expect_match(yc("html"), "%<sub>.*</sub>", fixed = FALSE)
  expect_match(yc("html"), "log(p/(1-p))", fixed = TRUE)
})
