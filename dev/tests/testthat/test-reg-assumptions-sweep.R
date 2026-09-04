
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


test_that("Dispersion tracks sqrt(phi) on an over-dispersed count, and returns to ~1 under quasi", {
  # the WHOLE frame: the two variance estimators agree only to O(1/n), and this asserts how closely.
  # It fits glm() directly rather than tab_reg(), so reading every row costs nothing.
  d  <- dplyr::mutate(fx_gss(),
                      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
  dm <- tidyr::drop_na(d, tvhours, age, race)
  p  <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  q  <- stats::glm(tvhours ~ age + race, data = dm, family = stats::quasipoisson())
  phi <- sum(stats::residuals(p, "pearson")^2) / stats::df.residual(p)
  expect_gt(phi, 1.5)                                    # the fixture really is over-dispersed
  # a poisson vcov() assumes phi = 1, so the robust SEs are ~sqrt(phi) wider (SS9.1: within 1.5-8 %)
  r_pois <- tabxplor:::reg_check_dispersion(p)
  expect_gt(r_pois / sqrt(phi), 0.95)
  expect_lt(r_pois / sqrt(phi), 1.10)
  # a quasipoisson vcov() ALREADY carries the estimated dispersion, so the check returns to ~1: the
  # intervals have been fixed, and the row says so while the `phi` row still reports the dispersion.
  expect_lt(abs(tabxplor:::reg_check_dispersion(q) - 1), 0.10)
})


test_that("tx_vif() is Fox & Monette's GVIF: a determinant ratio of the predictors' correlations", {
  # The multi-df shape, again from the DATA side (cov.wt on the model matrix) rather than from
  # vcov() -- the identity that makes the generalised VIF a generalisation at all.
  f <- chk_fit()$fit                                       # race + rincome are multi-df -> matrix
  v <- tabxplor:::tx_vif(f)
  expect_true(is.matrix(v))
  expect_identical(colnames(v), c("GVIF", "Df", "GVIF^(1/(2*Df))"))
  expect_identical(rownames(v), labels(stats::terms(f)))
  X   <- stats::model.matrix(f)[, -1, drop = FALSE]
  ass <- attr(stats::model.matrix(f), "assign")[-1]
  R   <- stats::cov.wt(X, wt = f$weights, cor = TRUE)$cor
  gv  <- vapply(seq_len(max(ass)), function(k) { i <- which(ass == k)
    det(R[i, i, drop = FALSE]) * det(R[-i, -i, drop = FALSE]) / det(R) }, 0)
  expect_equal(unname(v[, 1]), gv, tolerance = 1e-8)
  expect_equal(unname(v[, 2]), as.numeric(table(ass)))     # Df comes from `assign`
  expect_equal(unname(v[, 3]), unname(v[, 1]^(1 / (2 * v[, 2]))), tolerance = 1e-12)
  # the footer reads ONE scale whatever the term width
  expect_equal(tabxplor:::reg_check_collinearity(f), max(v[, 3]^2), tolerance = 1e-12)
})


test_that("tx_vif() drops the ordinal cut-points, which vcov() still carries", {
  skip_if_not_installed("MASS")
  d <- chk_fit()$data
  d$ord <- factor(cut(d$tvhours, c(-1, 1, 3, 100), labels = c("lo", "mid", "hi")),
                  levels = c("lo", "mid", "hi"), ordered = TRUE)
  o <- MASS::polr(ord ~ age + race, data = d, Hess = TRUE, method = "logistic")
  expect_identical(nrow(stats::vcov(o)),                   # the premise: vcov CARRIES them
                   length(stats::coef(o)) + length(o$zeta))
  v <- tabxplor:::tx_vif(o)
  expect_identical(rownames(v), labels(stats::terms(o)))
  expect_false(any(names(o$zeta) %in% rownames(v)))
  expect_true(all(v[, 1] >= 1 - 1e-8))                     # a GVIF is never below 1
  # and dropping them CHANGES the answer, so the drop is load-bearing rather than cosmetic
  Rz <- stats::cov2cor(stats::vcov(o)); i <- 1L
  expect_false(isTRUE(all.equal(unname(v[1, 1]),
                                det(Rz[i, i, drop = FALSE]) * det(Rz[-i, -i, drop = FALSE]) / det(Rz),
                                tolerance = 1e-6)))
})


test_that("the curvature p is invariant to the centring, which exists for the Collinearity check", {
  cf <- chk_fit(preds = c("age", "race"))
  dm  <- cf$data
  dm$z <- (dm$age - mean(dm$age)) / stats::sd(dm$age)
  raw <- stats::glm(married ~ age + race + I(age^2), data = dm, family = stats::binomial())
  ctr <- stats::glm(married ~ age + race + I(z^2),   data = dm, family = stats::binomial())
  expect_equal(stats::drop1(raw, scope = "I(age^2)", test = "Chisq")[["Pr(>Chi)"]][2],
               stats::drop1(ctr, scope = "I(z^2)",   test = "Chisq")[["Pr(>Chi)"]][2],
               tolerance = 1e-8)
  # but the collinearity of the emitted pair is not invariant -- which is why the term is centred
  expect_gt(max(tabxplor:::tx_vif(raw)[, 3]^2), 20)
  expect_lt(max(tabxplor:::tx_vif(ctr)[, 3]^2), 5)
})


test_that("reg_nested_test() IS drop1's test, to the last bit, on both arms", {
  # Phase 20f: the Linearity check takes this route INSTEAD of drop1(), so "equal" is not enough --
  # if the two ever diverged the check would silently start answering a different question. Every
  # assertion here is `expect_identical()` on the double, not a tolerance.
  cf <- chk_fit(preds = c("race", "age"))
  dm <- cf$data
  dm$z <- (dm$age - mean(dm$age)) / stats::sd(dm$age)

  # -- the LR arm (binomial) ------------------------------------------------------------------------
  aug <- stats::glm(married ~ race + age + I(z^2), data = dm, family = stats::binomial())
  d1  <- stats::drop1(aug, scope = "I(z^2)", test = "Chisq")
  lr  <- tabxplor:::reg_nested_test(cf$fit, aug, use_f = FALSE)
  expect_identical(lr$stat, d1[["LRT"]][2])
  expect_identical(lr$p,    d1[["Pr(>Chi)"]][2])
  expect_true(is.na(lr$df2))

  # -- the F arm: lm, gaussian glm and quasipoisson ------------------------------------------------
  # ⚠ quasipoisson is the one that pins the DISPERSION: drop1.glm estimates it as deviance/df.residual
  # of the augmented fit, which is neither summary()'s Pearson dispersion nor what anova() uses (14.25
  # against 12.47 on this shape). This assertion is what stops a "tidier" substitution.
  # ⚠ the outcome must be genuinely UNEXPLAINED by the predictors: a numeric copy of `age` gives an
  # exact fit (RSS ~ 1e-25), and then both routes are comparing floating-point noise.
  dm$num <- as.numeric(dm$year)
  dm$cnt <- as.integer(dm$year - min(dm$year))
  cases <- list(
    lm         = list(stats::lm(num ~ race + age, dm),
                      stats::lm(num ~ race + age + I(z^2), dm)),
    gaussianglm = list(stats::glm(num ~ race + age, dm, family = stats::gaussian()),
                       stats::glm(num ~ race + age + I(z^2), dm, family = stats::gaussian())),
    quasipoisson = list(suppressWarnings(stats::glm(cnt ~ race + age, dm, family = stats::quasipoisson())),
                        suppressWarnings(stats::glm(cnt ~ race + age + I(z^2), dm,
                                                    family = stats::quasipoisson())))
  )
  for (nm in names(cases)) {
    b <- cases[[nm]][[1]]; a <- cases[[nm]][[2]]
    ref <- suppressWarnings(stats::drop1(a, scope = "I(z^2)", test = "F"))
    got <- tabxplor:::reg_nested_test(b, a, use_f = TRUE)
    expect_identical(got$stat, ref[["F value"]][2], info = nm)
    expect_identical(got$p,    ref[[grep("^Pr\\(", names(ref), value = TRUE)[1]]][2], info = nm)
    expect_identical(got$df2,  as.numeric(stats::df.residual(a)), info = nm)
  }

  # -- it refuses rather than guesses ---------------------------------------------------------------
  expect_null(tabxplor:::reg_nested_test(aug, cf$fit))       # not nested the right way round
  half <- stats::glm(married ~ race + age, data = dm[seq_len(nrow(dm) %/% 2L), ],
                     family = stats::binomial())
  expect_null(tabxplor:::reg_nested_test(half, aug))         # different rows
})


test_that("the nested test is what carries multinomial Linearity", {
  skip_if_not_installed("nnet")
  # nnet:::drop1.multinom returns only Df and AIC -- it has no `test` argument and no p-value at all,
  # so without this route the multinomial arm silently produced no row
  d  <- chk_data()
  dn <- tidyr::drop_na(d, marital, race, age)
  zz <- (dn$age - mean(dn$age)) / stats::sd(dn$age); dn$z <- zz
  b <- nnet::multinom(marital ~ race + age,          dn, trace = FALSE)
  a <- nnet::multinom(marital ~ race + age + I(z^2), dn, trace = FALSE)
  ref <- tabxplor:::reg_nested_test(b, a)
  expect_identical(ref$df, a$edf - b$edf)                 # one extra coefficient per category

  out <- utils::capture.output(t <- suppressMessages(suppressWarnings(
    tab_reg(d, "marital", c("race", "age"), family = "multinomial", cleannames = FALSE,
            stats = c("n", "linearity")))))
  tt <- get_test(t)
  li <- tt[startsWith(tt$test, "linearity"), , drop = FALSE]
  expect_identical(nrow(li), 1L)
  expect_equal(li$pvalue, ref$p, tolerance = 1e-6)
  # drop1.multinom prints its progress with cat(); none of it may reach the user
  expect_false(any(grepl("trying", out)))
})


# ---- the footer grain and the `stats =` vocabulary --------------------------------------------

test_that("a comparison table carries one check row per (model column x numeric predictor)", {
  t <- suppressMessages(tab_reg(
    chk_data(), "married",
    list(m1 = c("race", "age"), m2 = c("race", "age", "tvhours")),
    family = "binomial", cleannames = FALSE, stats = c("n", "linearity", "dispersion")))
  tt <- get_test(t)
  li <- tt[startsWith(tt$test, "linearity"), , drop = FALSE]
  # age is in both models, tvhours only in m2 -> 3 rows, and the plan lays out 2 labelled rows
  # Phase 19g re-keyed the `test` tibble: the column a row belongs to is `col` (was `col_var`).
  expect_setequal(paste(li$col, li$var), c("m1 age", "m2 age", "m2 tvhours"))
  plan <- tabxplor:::reg_footer_plan(tt)
  expect_setequal(plan$label[startsWith(plan$test, "linearity")],
                  c("Linearity (LR): age", "Linearity (LR): tvhours"))
  # the whole-model checks stay one row each
  expect_identical(sum(tt$test == "dispersion"), 2L)
  expect_true(all(tt$var[tt$test == "dispersion"] == ""))
  # and they render, per model column
  md <- gsub(intToUtf8(160L), " ", tab_md(t, print = FALSE), fixed = TRUE)
  expect_true(any(grepl("Linearity (LR): tvhours", md, fixed = TRUE)))
  expect_true(any(grepl("Dispersion (robust/model SE)", md, fixed = TRUE)))
})


test_that("a check absent for a family produces no row, never a wrong number", {
  skip_if_not_installed("nnet")
  # multinomial: one coefficient block per outcome category and a block vcov, so there is no single
  # correlation matrix to take a determinant of -- Collinearity is refused, while the others still
  # compute from the score-based influence
  expect_false("collinearity" %in% tabxplor:::reg_checks_for("multinomial"))
  t <- suppressMessages(suppressWarnings(
    tab_reg(chk_data(), "marital", "race", family = "multinomial", cleannames = FALSE)))
  tt <- get_test(t)
  expect_false("collinearity" %in% tt$test)
  expect_true("dispersion" %in% tt$test)
  # Proportionality is ordinal-only, and refused (absent, not approximated) on a weighted fit
  expect_false("proportionality" %in% tabxplor:::reg_checks_for("binomial"))
  expect_true( "proportionality" %in% tabxplor:::reg_checks_for("ordinal"))
  expect_false("proportionality" %in% tabxplor:::reg_checks_for("ordinal", weighted = TRUE))
  # the jamovi digest path keeps no model frame -> every check degrades to absent
  # every check is computed in the eager stage, while the fit lives, so a distilled record keeps
  # them: there is no fit-less degradation to assert any more (Phase 22j).
  expect_gt(length(tabxplor:::reg_checks_for("binomial")), 0L)
})


test_that("the Pearson dispersion row never reads df.residual, so a weighted count fit is honest", {
  skip_if_not_installed("survey")
  d  <- chk_data()
  dm <- tidyr::drop_na(d, tvhours, age, race)
  dm$w <- 1                                              # constant weights: phi must not change
  des <- survey::svydesign(ids = ~1, weights = ~w, data = dm)
  p   <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  sp  <- suppressWarnings(survey::svyglm(tvhours ~ age + race, design = des,
                                         family = stats::quasipoisson()))
  # df.residual(svyglm) is the DESIGN df, which used to inflate this by ~20x
  expect_equal(tabxplor:::reg_dispersion(sp), tabxplor:::reg_dispersion(p), tolerance = 1e-6)
})


# === SECTION: shape =, the observed curve and the shape table =====================================

skip_on_cran()


shp_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}


lv <- function(t, v) as.character(t$levels)[as.character(t$var) == v]


test_that('shape = "linear" is byte-identical to no shape at all', {
  d  <- shp_data()
  t0 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial"))
  t1 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 shape = c(age = "linear")))
  expect_identical(as.character(t0$levels), as.character(t1$levels))
  expect_equal(get_or(t0[["Model_OR"]]), get_or(t1[["Model_OR"]]))
})


# ---- quadratic ----------------------------------------------------------------------------------

test_that('shape = "quadratic" gives the predictor two rows, both fitted and both estimable', {
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
  expect_lt(max(tabxplor:::tx_vif(ref)[, 3]^2), 5)   # the scale the check reads, not the raw matrix
})


test_that("a curved predictor keeps its observed twin: the crude fit takes the SAME shape", {
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
  skip_if_not_installed("marginaleffects")
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                shape = c(age = "quadratic"), effect = "marginal", measure = "difference", stats = FALSE))
  expect_length(lv(t, "age"), 1L)
})


test_that("a cured predictor gets no Linearity row (its remedy is already in the model)", {
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


test_that('shape = "sqrt" fits the transformed column and says so in the label', {
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


test_that("the curve is the MODELLED level's, not the factor's first level", {
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


test_that("the range travels with the picture, in the same row of the shape table", {
  d <- suppressWarnings(fx_reg_fmt())
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial", stats = FALSE))
  st <- tabxplor:::reg_shape_table(t)
  expect_match(st$range, "^[0-9]+-[0-9]+% \\(OR [0-9.]+\\)$")
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
  d  <- suppressWarnings(fx_reg_fmt())
  st <- tabxplor:::reg_shape_table(
    suppressMessages(suppressWarnings(tab_reg(d, "rincome", "age", stats = FALSE))))
  # ⚠ it must ALSO name the outcome: with several of them, two "not 1st" rows would be identical
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
  d  <- suppressWarnings(fx_reg_fmt())
  y  <- function(t) tabxplor:::reg_shape_table(t)$outcome[[1L]]
  b  <- function(...) suppressMessages(tab_reg(d, "married", "age", family = "binomial",
                                               stats = FALSE, ...))
  # the three readings of ONE binary outcome, each on the scale its own link fits
  expect_identical(y(b(link = "difference")),  "%Married")
  # a number is a mean, logged exactly where its link logs it
  expect_identical(y(suppressMessages(tab_reg(d, "age", "tvhours", stats = FALSE))),
                   "mean age")
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


# Phase 22g-vi: the shape table under a PUBLICATION palette. tab_css() gives `.tx-sec` a
# `display:inline-block` there (load-bearing: it is what takes an aside out of an ancestor's
# text-decoration), and that on a <td> destroys `display:table-cell` -- the cell drops out of the
# row and reflows, which is how a curve ended up under the "outcome" header.
testthat::test_that("the shape table's grey never lands on a <td>", {
  d <- shp_data()
  t <- suppressMessages(tab_reg(d, "married", c("age", "tvhours"), family = "binomial",
                                empirical = FALSE, stats = "no"))
  # phase 7: the shape table is a NOTE like any other, and note_html() is everybody's emitter
  h <- tabxplor:::note_html(tabxplor:::reg_shape_table(t, syntax = "html"))
  testthat::skip_if(is.null(h))
  tds <- regmatches(h, gregexpr("<td[^>]*>", h))[[1]]
  expect_false(any(grepl("tx-sec", tds)))
  # ...and the print stylesheet is what makes it matter, so the rule is asserted where it lives
  expect_match(tab_css(theme = "print_marks"), "\\.tx-sec\\{[^}]*display:inline-block")
  # the curve is still in the shape column, and it is the only <svg> in the table
  expect_match(h, "tx-sparkcell")
})


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


test_that("tx_vif() refuses rather than approximates, and the footer then shows no row", {
  d   <- chk_data()
  one <- stats::glm(married ~ race, data = tidyr::drop_na(d, married, race),
                    family = stats::binomial())
  expect_null(tabxplor:::tx_vif(one))                      # fewer than 2 terms
  expect_true(is.na(tabxplor:::reg_check_collinearity(one)))

  dm <- chk_fit(preds = c("age", "tvhours"))$data; dm$age2 <- dm$age
  al <- stats::glm(married ~ age + age2 + tvhours, data = dm, family = stats::binomial())
  expect_true(anyNA(stats::coef(al)))                      # aliased
  expect_null(tabxplor:::tx_vif(al))
  expect_true(is.na(tabxplor:::reg_check_collinearity(al)))

  skip_if_not_installed("nnet")
  mn <- nnet::multinom(marital ~ age + race, data = tidyr::drop_na(d, marital, age, race),
                       trace = FALSE)
  expect_null(tabxplor:::tx_vif(mn))                       # a block vcov has no single R
})


test_that("REG_CHECKS declares its cost AND its default, and the readers agree with them", {
  # the declared facts, and the derived sets. ⚠ the default set is DECLARED (footer_default), not
  # "the applicable checks minus the costly ones": Proportionality is a refit AND a default, because
  # a cumulative odds ratio that fails it is not one number but a fiction.
  expect_setequal(tabxplor:::reg_checks_costly(), c("linearity", "proportionality"))
  dflt <- tabxplor:::reg_checks_default("ordinal")
  expect_true(all(dflt %in% tabxplor:::reg_checks_for("ordinal")))
  expect_setequal(dflt, c("proportionality", "dispersion", "influence", "collinearity"))
  # linearity is the costly check that is NOT a default -- `stats =` reaches it by name
  expect_false("linearity" %in% dflt)
  expect_true("linearity" %in% tabxplor:::reg_checks_for("ordinal"))
  # a panel is always free: reg_check_plots() keeps every panel whatever `cost` says
  expect_true(all(c("linearity", "proportionality") %in%
                    tabxplor:::reg_checks_for("ordinal", what = "panel")))
})


# === SECTION: shape =, the observed curve and the shape table =====================================

skip_on_cran()


shp_data <- function() {
  fx_reg_df() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}


lv <- function(t, v) as.character(t$levels)[as.character(t$var) == v]


# ---- quantile groups + transforms ----------------------------------------------------------------

test_that("quantile groups turn the predictor into a factor, with the whole factor machinery", {
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


# ---- the primitives ------------------------------------------------------------------------------

test_that("shape_wquantile() weights, and reproduces stats::quantile() unweighted", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  expect_equal(tabxplor:::shape_wquantile(x, 0.5), stats::median(x), tolerance = 1e-8)
  # weighting the top half twice must push the median up
  w <- c(rep(1, 5), rep(2, 5))
  expect_gt(tabxplor:::shape_wquantile(x, 0.5, w), tabxplor:::shape_wquantile(x, 0.5))
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


# ---- Phase 22g-v ------------------------------------------------------------------------------

test_that("the shape table names EVERY group, not only the first variable's", {
  d  <- shp_data()
  # relig is lumped first: the claim is that every group is named, and a group of a dozen people
  # cannot carry three predictors -- the rare levels would fail the fit, not the naming.
  d$relig <- forcats::fct_lump_n(d$relig, 4)
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
