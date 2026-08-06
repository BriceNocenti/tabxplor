# R/reg-influence.R -- Last Phase z8-B: influence functions, and the standard error of the GAP between
# two estimators fitted on the SAME rows.
#
# WHY THIS EXISTS. `color = "adjustment"` scores how far a model effect sits from its observed (crude)
# counterpart. Both are M-estimators on the same observations, so they are correlated (measured
# r = 0.52-0.90) and the naive `sqrt(se_model^2 + se_crude^2)` is 2-4x too large, while Hausman's
# `Var(crude) - Var(adj)` goes NEGATIVE for logistic. The only quantity that carries the covariance is
# the difference of their influence functions:
#
#     Var(theta_adj - theta_crude) = Var( sum_i ( IF_i^adj - IF_i^crude ) )
#
# This is seemingly-unrelated estimation (Stata's `suest`, Weesie 1999; Mize, Doan & Long 2019 is the
# sociological statement). Measurements, rejected alternatives and the calibration study:
# dev/model_vs_observed_gap_test.md SS2-SS4.
#
# WHAT IS HERE. Pure matrix math over `stats` + `survey` -- no fmt types, no tabxplor classes. The
# orchestration (which cells get a gap SE at all) lives in R/tab_reg.R's reg_gap_se_columns(), the only
# caller. This is the ONE place in the package that calls survey::svyrecvar().
#
#   reg_if_from_parts(X, W, r)  the ONE formula, as a closure over a contrast
#   reg_coef_if_maker(fit)      its fit adapter -- lm / glm / svyglm alike
#   reg_crude_if_maker(...)     the observed side of a FACTOR row, in closed form (no fit at all)
#   reg_ame_if_maker(...)       effect = "ame" / "ame_ratio" (the two-term marginal influence function)
#   reg_if_se(d, design)        design-based when a design exists, IID otherwise
#
# Last Phase z9 -- TWO crude paths, by predictor kind. A factor's crude effect is a saturated one-factor
# GLM, hence the closed form above; a CONTINUOUS predictor has no cells, so its crude leg is
# reg_coef_if_maker() on the row's own univariable fit (built in R/tab_reg.R). Both legs are then the
# same machinery over two fits solved on the same rows. reg_ame_if_maker()'s counterfactual therefore
# takes SHIFTS rather than levels for a numeric column -- see its own contract note.
#
# EVERY function here returns NULL rather than a wrong number when its inputs do not support the
# computation (singular information matrix, absent term, empty cell, unknown link). The caller reads a
# NULL as "no test here", writes no `gap_se`, and the colour engine's `force_policy` closure then makes
# the column read under `ignore` -- so a degraded path is descriptive, never falsely significant.


# reg_if_from_parts() -- the influence function of ANY M-estimator solving sum_i X_i W_i r_i = 0, as a
# closure over the CONTRAST rather than a matrix. Given the working parts of an IRLS fit,
#
#     A = X' W X        U_i = X_i W_i r_i        IF_i = U_i A^-1
#
# and the influence of one linear combination L of the coefficients is IF %*% L.
#
# DESIGN -- why a closure over `L` and not the n x p matrix. `U = X * (W*r)` is a pure ROW scaling, so
#     (U %*% c)_i == (W_i r_i) * (X %*% c)_i
# (verified to 1.7e-18). Holding X, the length-n vector W*r and the p x p A^-1 therefore gives every
# contrast for one length-n allocation each, and the second n x p matrix is never built.
# WARNING: peak memory is ONE n x p matrix -- the `model.matrix(fit)` the caller already holds. At
# n = 5M and p = 50 that is ~2 GB, so never materialise `U`, `IF`, or a per-term matrix of differences
# (dev/model_vs_observed_gap_test.md SS8).
#' @keywords internal
reg_if_from_parts <- function(X, W, r) {
  if (is.null(X) || !is.matrix(X) || nrow(X) == 0L || ncol(X) == 0L) return(NULL)
  W <- as.numeric(W); r <- as.numeric(r)
  if (length(W) != nrow(X) || length(r) != nrow(X)) return(NULL)
  W[!is.finite(W)] <- 0; r[!is.finite(r)] <- 0
  Ai <- tryCatch(solve(crossprod(X * sqrt(W))), error = function(e) NULL)   # singular -> no test
  if (is.null(Ai)) return(NULL)
  wr <- W * r
  cn <- colnames(X)
  # `L` may be named over the coefficients (a skeleton contrast) or bare of length p (a jacobian).
  function(L) {
    v <- if (is.null(names(L))) as.numeric(L) else unname(L[cn])
    if (length(v) != length(cn)) return(NULL)
    v[!is.finite(v)] <- 0
    wr * as.vector(X %*% (Ai %*% v))
  }
}

# reg_coef_if_maker() -- the fit adapter. ONE formula for stats::lm, stats::glm and survey::svyglm:
# `fit$weights` are the IRLS working weights (already carrying the prior / design weights) and
# `residuals(type = "working")` is (y - mu)/mu'(eta), so X'Wr is the score and X'WX the information.
# Verified bit-identical to `attr(survey::svyglm(..., influence = TRUE), "influence")` (max difference
# 5e-17) -- which is why nothing here ever passes `influence = TRUE`, and why the same code serves the
# weighted, the survey-design and the "rr" (modified Poisson) paths.
#
# NB the SE this implies is the HUBER-WHITE SANDWICH. That IS what svyglm prints, but a plain
# unweighted glm prints the model-based SE, so the two agree only up to O(1/n) there. Correct for a gap
# between two differently-specified estimators; see reg_gap_se_columns()'s docs for what it means for
# the printed intervals.
#
# Backticks are stripped from the column names exactly as reg_fit() strips them from `td$term`, so a
# contrast can be keyed by `skeleton$term` with no second naming rule. Aliased (rank-deficient)
# coefficients are dropped: no displayed contrast can load a column the fit could not estimate.
#' @keywords internal
reg_coef_if_maker <- function(fit) {
  X <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(X) || !nrow(X)) return(NULL)
  b <- stats::coef(fit)
  if (length(b) == ncol(X) && anyNA(b)) X <- X[, !is.na(b), drop = FALSE]
  colnames(X) <- stringi::stri_replace_all_regex(colnames(X), "`", "")
  W <- fit$weights
  if (is.null(W)) W <- rep(1, nrow(X))
  r <- tryCatch(stats::residuals(fit, type = "working"), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  reg_if_from_parts(X, W, r)
}

# reg_crude_if_maker() -- the OBSERVED side, in closed form. Every `Obs_*` effect tabxplor computes is
# exactly the coefficient of a saturated one-factor GLM at the matching link (verified to 1e-13 on all
# five families, weighted and unweighted), so its influence function is the two-cell expression
#
#   IF_i = 1(x_i = l) w_i (y_i - mu_l) / sum_{x=l} w * g'(mu_l)
#        - 1(x_i = r) w_i (y_i - mu_r) / sum_{x=r} w * g'(mu_r)
#
# with no fit at all: measured identical to the fitted equivalent to 8e-17 and ~21x cheaper. For the
# unweighted binomial case its SE is exactly the WOOLF interval the `Obs_OR` column already prints.
#
# WARNING: `link` describes the CRUDE estimator, not the model's family -- a binomial model shows a
# logit-scale OR by default but an IDENTITY-link risk difference under effect = "ame" and a LOG-link
# risk ratio under "ame_ratio". That is exactly why it is a fact of the REG_EMPIRICAL SHAPE row.
# The frame must be the model's own complete cases, in its order (the caller proves that).
#' @keywords internal
reg_crude_if_maker <- function(data, dependent, family, positive_level, wt, link) {
  gp <- switch(as.character(link)[1],
               "logit"    = function(m) 1 / (m * (1 - m)),
               "log"      = function(m) 1 / m,
               "identity" = function(m) 1,
               NULL)
  if (is.null(gp)) return(NULL)
  y <- reg_crude_y(data, dependent, family, positive_level)     # the shared outcome recode
  w <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (length(y) != nrow(data) || length(w) != nrow(data)) return(NULL)
  fin <- is.finite(y) & is.finite(w)
  function(var, level, ref) {
    x <- data[[var]]
    if (is.null(x)) return(NULL)
    x <- as.character(x)
    leg <- function(l) {
      m  <- fin & !is.na(x) & x == as.character(l)
      sw <- sum(w[m])
      if (!isTRUE(sw > 0)) return(NULL)
      mu <- sum(w[m] * y[m]) / sw
      g  <- gp(mu)
      if (!is.finite(g)) return(NULL)                            # a 0 % or 100 % cell has no log-odds
      out <- numeric(length(y))
      out[m] <- w[m] * (y[m] - mu) / sw * g
      out
    }
    a <- leg(level); b <- leg(ref)
    if (is.null(a) || is.null(b)) return(NULL)
    a - b
  }
}

# reg_ame_if_maker() -- the marginal (g-computation) side. An average marginal effect is not a
# coefficient: it depends on the empirical covariate distribution as well as on beta, so its influence
# function has TWO terms (dev/model_vs_observed_gap_test.md SS3.4):
#
#   additive (effect = "ame"):     IF_i = wt_i (g_i - AME)      + IF^beta_i %*% G
#   ratio    (effect = "ame_ratio"): IF_i = wt_i (mu1_i - M1)/M1 - wt_i (mu0_i - M0)/M0
#                                          + IF^beta_i %*% (G1/M1 - G0/M0)
#
# with g_i = mu1_i - mu0_i, wt_i = w_i / sum(w), and G the jacobian d(estimand)/d(beta). Verified: the
# delta term alone reproduces marginaleffects' own standard error (it reports the delta method with the
# covariates held fixed), and the full influence function adds the empirical-averaging term worth
# ~0.1 % -- a real difference in the right direction, not a discrepancy.
#
# The counterfactual design matrices are built and released ONE LEVEL AT A TIME: `G` is a p-vector and
# `g_i` a length-n vector, so peak memory stays X + one counterfactual.
#
# CONTRACT of the returned closure `(var, level, ref)`:
#   * `var` is a FACTOR  -- `level` / `ref` are level LABELS, and the counterfactual sets the whole
#     column to that level (the classic "everyone at level j vs everyone at the reference").
#   * `var` is NUMERIC   -- Last Phase z9: `level` / `ref` are SHIFTS added to the observed x, so the
#     caller passes (k, 0) for a k-unit contrast. That is marginaleffects' own forward difference
#     `variables = list(v = k)`, which is what the numeric AME column shows. The old code path assigned
#     `as.character(lv)` unconditionally, turning a numeric column into character -- model.matrix() then
#     either errored (caught -> NULL, i.e. no test) or built the wrong contrast width.
#' @keywords internal
reg_ame_if_maker <- function(fit, data, wt, ratio, coef_if) {
  if (is.null(coef_if)) return(NULL)
  tt  <- tryCatch(stats::delete.response(stats::terms(fit)), error = function(e) NULL)
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(tt) || is.null(fam) || is.null(fam$linkinv) || is.null(fam$mu.eta)) return(NULL)
  b    <- stats::coef(fit)
  keep <- !is.na(b)
  bk   <- b[keep]
  w    <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (length(w) != nrow(data) || !all(is.finite(w))) return(NULL)
  sw <- sum(w)
  cf <- function(lv, var) {                       # the counterfactual model matrix at one level/shift
    d <- data
    x <- data[[var]]
    d[[var]] <- if (is.factor(x)) factor(as.character(lv), levels = levels(x))
                else if (is.numeric(x)) x + as.numeric(lv)
                else as.character(lv)
    X <- tryCatch(stats::model.matrix(tt, d), error = function(e) NULL)
    if (is.null(X) || ncol(X) != length(b)) return(NULL)
    X[, keep, drop = FALSE]
  }
  function(var, level, ref) {
    X1 <- cf(level, var); X0 <- cf(ref, var)
    if (is.null(X1) || is.null(X0)) return(NULL)
    e1 <- as.vector(X1 %*% bk);  e0 <- as.vector(X0 %*% bk)
    m1 <- fam$linkinv(e1);       m0 <- fam$linkinv(e0)
    d1 <- fam$mu.eta(e1);        d0 <- fam$mu.eta(e0)
    if (!all(is.finite(m1)) || !all(is.finite(m0))) return(NULL)
    if (ratio) {
      M1 <- sum(w * m1) / sw; M0 <- sum(w * m0) / sw
      if (!isTRUE(M1 > 0) || !isTRUE(M0 > 0)) return(NULL)
      emp <- w * (m1 - M1) / (sw * M1) - w * (m0 - M0) / (sw * M0)
      G   <- colSums(w * X1 * d1) / (sw * M1) - colSums(w * X0 * d0) / (sw * M0)
    } else {
      A   <- sum(w * (m1 - m0)) / sw
      emp <- w * ((m1 - m0) - A) / sw
      G   <- colSums(w * (X1 * d1 - X0 * d0)) / sw
    }
    delta <- coef_if(unname(G))
    if (is.null(delta)) return(NULL)
    emp + delta
  }
}

# reg_if_se() -- the standard error of a quantity whose per-observation influence contributions are `d`.
# With a survey design that is survey::svyrecvar() -- the Binder (1983) linearization survey uses for
# its OWN variances, so strata, clusters and finite-population corrections come along for free
# (measured: it reproduces SE(svyglm) exactly, ratio 1.0000, while the IID version is 6 % too small on a
# mild stratified/clustered design). Without one it is the plain sum of squares.
#' @keywords internal
reg_if_se <- function(d, design = NULL) {
  if (is.null(d)) return(NA_real_)
  d <- as.numeric(d)
  if (!length(d) || anyNA(d)) return(NA_real_)
  v <- if (is.null(design)) sum(d * d) else tryCatch(
    as.numeric(survey::svyrecvar(as.matrix(d), design$cluster, design$strata, design$fpc,
                                 postStrata = design$postStrata)),
    error = function(e) NA_real_)
  if (!isTRUE(is.finite(v)) || v < 0) return(NA_real_)
  sqrt(v)
}
