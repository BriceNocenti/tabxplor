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
  # Last Phase z10: a 3+ level fit has no working residuals / IRLS weights, so it goes through the
  # score core instead. Same contract, different algebra (see reg_if_from_score's WARNING).
  if (inherits(fit, "multinom") || inherits(fit, "polr")) {
    sc <- if (inherits(fit, "multinom")) reg_score_multinom(fit) else reg_score_polr(fit)
    return(if (is.null(sc)) NULL else reg_if_from_score(sc$S, sc$bread))
  }
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
#
# Last Phase z10: the outcome + weights come from reg_crude_yw(), the ONE description of "what the crude
# estimator averages, and with what weights" -- so the influence function cannot be built around a
# different `y` than the estimate it is the standard error OF (the invariant reg_crude_y() was extracted
# for in z8-B, now covering two more shapes):
#   grouped binomial : each ROW is a cluster of `trials` draws, so y = succ/trials with weight w*trials.
#                      reg_if_se() then sums squares over ROWS, i.e. the cluster-robust variance -- the
#                      right one under over-dispersion, and the same grain as the model leg
#                      (reg_coef_if_maker on glm(cbind(s, f) ~ x) is also per row).
#   multinomial      : `category` picks the 0/1 indicator of that outcome category, which is the crude
#                      estimand of the column being tested.
#' @keywords internal
reg_crude_if_maker <- function(data, dependent, crude_key, positive_level, wt, link,
                               trials = NULL, category = "", ref_category = NULL) {
  gp <- switch(as.character(link)[1],
               "logit"    = function(m) 1 / (m * (1 - m)),
               "log"      = function(m) 1 / m,
               "identity" = function(m) 1,
               NULL)
  if (is.null(gp)) return(NULL)
  yw <- tryCatch(reg_crude_yw(data, dependent, crude_key, positive_level, wt, trials, ref_category),
                 error = function(e) NULL)
  if (is.null(yw)) return(NULL)
  # a categorical outcome averages the 0/1 indicator of the category this column shows
  y <- if (identical(yw$cats, "")) as.numeric(yw$y) else
    as.numeric(as.character(yw$y) == (if (nzchar(category)) category else "1"))
  w <- yw$w
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

# === 3+ LEVEL OUTCOMES (Last Phase z10) ============================================================
#
# reg_coef_if_maker() above reaches lm / glm / svyglm through model.matrix() + residuals(type =
# "working") + fit$weights, none of which nnet::multinom or MASS::polr provides -- so both correctly
# returned NULL, and `color = "adjustment"` had no test on a 3+ level outcome. The generalisation is
# NOT a branch per family: every one of these is an M-estimator, so
#
#     IF = (per-observation score) %*% (bread)
#
# and reg_if_from_parts() is already exactly that in GLM-specialised algebra -- X*(W*r) IS the score,
# solve(X'WX) IS the bread. What changes is only who supplies the score.
#
# WARNING -- the two cores are NOT merged, deliberately. reg_if_from_parts() exists to avoid ever
# materialising U = X*(W*r): it exploits the fact that U is a pure ROW SCALING of X, so peak memory is
# the ONE n x p model matrix the caller already holds. A multinomial / cumulative-logit score has no
# such structure and must be held as a real n x q matrix (n = 20 000, q = 40 is 6.4 MB -- fine, but say
# so). They share the CONTRACT (a closure over the contrast, NULL on failure), not the algebra.
#' @keywords internal
reg_if_from_score <- function(S, bread) {
  if (is.null(S) || !is.matrix(S) || !nrow(S) || !ncol(S)) return(NULL)
  if (is.null(bread) || !identical(dim(bread), c(ncol(S), ncol(S)))) return(NULL)
  cn <- colnames(S)
  function(L) {
    v <- if (is.null(names(L))) as.numeric(L) else unname(L[cn])
    if (length(v) != length(cn)) return(NULL)
    v[!is.finite(v)] <- 0
    as.vector(S %*% (bread %*% v))
  }
}

# The per-observation score of a MULTINOMIAL logit: U_i,(j) = x_i (1{y_i = j} - p_ij), the blocks
# stacked CATEGORY-MAJOR.
# WARNING -- trap measured, twice: coef(multinom) is a (K-1) x p MATRIX and vcov(multinom) is ordered
# category-major ("Dem:(Intercept)", "Dem:raceBlack", ...), while as.vector() on that matrix is
# category-MINOR. Getting it backwards produces a standard error ~2.7x too large with no warning. The
# defence here is structural, not a comment: the columns are NAMED and every lookup goes by name, so a
# mismatch is a NULL (the names test below), never a wrong number.
#' @keywords internal
reg_score_multinom <- function(fit, data = NULL) {
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  X <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  P <- tryCatch(stats::predict(fit, type = "probs"), error = function(e) NULL)
  if (is.null(V) || is.null(X) || is.null(P) || !is.matrix(P) || ncol(P) < 2L) return(NULL)
  y <- tryCatch(as.character(stats::model.frame(fit)[[1L]]), error = function(e) NULL)
  if (is.null(y) || length(y) != nrow(X)) return(NULL)
  lev <- colnames(P)
  S <- do.call(cbind, lapply(lev[-1], function(j) X * ((y == j) - P[, j])))
  colnames(S) <- as.vector(t(outer(lev[-1], colnames(X), function(a, b) paste0(a, ":", b))))
  if (!identical(colnames(S), rownames(V))) return(NULL)      # the ordering trap, closed structurally
  list(S = S, bread = V)
}

# The per-observation score of a CUMULATIVE logit (proportional odds), over (beta, zeta). With
# L = F(z_j - eta) - F(z_{j-1} - eta):
#   dlogL/dbeta  = -[f(z_j - eta) - f(z_{j-1} - eta)] / L * x
#   dlogL/dzeta_k = [1{k = j} f(z_j - eta) - 1{k = j-1} f(z_{j-1} - eta)] / L
# WARNING -- trap measured: the bread is vcov(fit), NEVER solve(fit$Hessian). MASS::polr optimises over
# (beta, zeta_1, log(diff zeta)), so its Hessian is in THAT parameterisation; substituting it gave
# standard errors up to 2x wrong here while looking entirely plausible. vcov() applies the transform's
# jacobian. (For svyolr, fit$var is the design-based SANDWICH, not the bread -- substituting it would
# double-count the design exactly as vcov(svyglm) would in the GLM path above. svyolr is unreachable
# anyway: tab_reg() refuses a weighted 3+ level outcome with effect = "ame".)
#' @keywords internal
reg_score_polr <- function(fit, data = NULL) {
  if (inherits(fit, "svyolr")) return(NULL)
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  b <- tryCatch(stats::coef(fit), error = function(e) NULL)
  z <- fit$zeta
  if (is.null(V) || is.null(b) || is.null(z) || !length(b)) return(NULL)
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  X  <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(mf) || is.null(X) || !all(names(b) %in% colnames(X))) return(NULL)
  X  <- X[, names(b), drop = FALSE]
  yv <- as.integer(mf[[1L]]); K <- length(z) + 1L
  if (anyNA(yv) || length(yv) != nrow(X)) return(NULL)
  eta <- as.vector(X %*% b)
  zhi <- ifelse(yv == K, Inf , z[pmin(yv    , K - 1L)])
  zlo <- ifelse(yv == 1L, -Inf, z[pmax(yv - 1L, 1L)])
  Fhi <- ifelse(is.infinite(zhi), 1, stats::plogis(zhi - eta))
  Flo <- ifelse(is.infinite(zlo), 0, stats::plogis(zlo - eta))
  L   <- Fhi - Flo
  if (!all(is.finite(L)) || any(L <= 0)) return(NULL)
  fhi <- ifelse(is.infinite(zhi), 0, stats::dlogis(zhi - eta))
  flo <- ifelse(is.infinite(zlo), 0, stats::dlogis(zlo - eta))
  Sz  <- vapply(seq_len(K - 1L),
                function(k) ((yv == k) * fhi - (yv == (k + 1L)) * flo) / L, numeric(nrow(X)))
  S   <- cbind(-((fhi - flo) / L) * X, Sz)
  colnames(S) <- c(names(b), names(z))
  if (!identical(colnames(S), rownames(V))) return(NULL)
  list(S = S, bread = V)
}

# The predicted-probability function of a 3+ level fit, as a PARAMETER-VECTOR closure -- the one piece
# a finite-difference jacobian needs and neither package exposes. Returns list(theta, probs(theta, X),
# mm(newdata)); NULL for anything else.
#
# DESIGN -- why a local predictor is not a duplicate implementation. marginaleffects computes its
# delta-method standard errors from an internal jacobian it does not expose as an attribute (checked),
# and perturbing the fit and re-calling it p+1 times costs ~4.6 s per table. The local softmax /
# cumulative-logit IS the same arithmetic the SCORE functions above already need, so there is ONE
# predictor with two consumers, not two predictors. It is policed the way reg_crude_if_maker() is: a
# test pins the local AME to marginaleffects::avg_comparisons() (which it reproduces to 8 significant
# digits) rather than to a hand-written expectation.
#' @keywords internal
reg_prob_engine <- function(fit) {
  tt <- tryCatch(stats::delete.response(stats::terms(fit)), error = function(e) NULL)
  if (is.null(tt)) return(NULL)
  if (inherits(fit, "multinom")) {
    B <- tryCatch(stats::coef(fit), error = function(e) NULL)
    if (is.null(B) || !is.matrix(B)) return(NULL)
    lev <- c(setdiff(colnames(tryCatch(stats::predict(fit, type = "probs"), error = function(e) NULL)),
                     rownames(B)), rownames(B))
    if (length(lev) != nrow(B) + 1L) return(NULL)
    return(list(
      theta = as.vector(t(B)),                     # CATEGORY-MAJOR, matching vcov / the score
      levels = lev,
      mm    = function(d) tryCatch(stats::model.matrix(tt, d), error = function(e) NULL),
      probs = function(th, X) {
        Bm <- matrix(th, nrow = nrow(B), byrow = TRUE)
        E  <- cbind(0, X %*% t(Bm))
        E  <- exp(E - apply(E, 1, max))
        p  <- E / rowSums(E)
        colnames(p) <- lev
        p
      }))
  }
  if (inherits(fit, "polr") && !inherits(fit, "svyolr")) {
    b <- tryCatch(stats::coef(fit), error = function(e) NULL)
    z <- fit$zeta
    if (is.null(b) || is.null(z) || !length(b)) return(NULL)
    lev <- levels(tryCatch(stats::model.frame(fit)[[1L]], error = function(e) NULL))
    if (length(lev) != length(z) + 1L) return(NULL)
    nb <- length(b)
    return(list(
      theta = c(unname(b), unname(z)),
      levels = lev,
      mm    = function(d) {
        X <- tryCatch(stats::model.matrix(tt, d), error = function(e) NULL)
        if (is.null(X) || !all(names(b) %in% colnames(X))) return(NULL)
        X[, names(b), drop = FALSE]
      },
      probs = function(th, X) {
        eta <- as.vector(X %*% th[seq_len(nb)])
        zz  <- th[(nb + 1L):length(th)]
        cum <- vapply(zz, function(k) stats::plogis(k - eta), numeric(length(eta)))
        p   <- cbind(cum, 1) - cbind(0, cum)
        colnames(p) <- lev
        p
      }))
  }
  NULL
}

# reg_ame_if_cat_maker() -- the marginal influence function for a 3+ level outcome, ONE outcome
# category at a time (each model column shows one). Same two-term shape as reg_ame_if_maker(); only the
# jacobian G is obtained by finite differences of the local AME instead of analytically, because a
# softmax / cumulative-logit derivative has no `mu.eta` to read it off.
#' @keywords internal
reg_ame_if_cat_maker <- function(fit, data, wt, ratio, category) {
  eng <- reg_prob_engine(fit)
  sc  <- if (inherits(fit, "multinom")) reg_score_multinom(fit) else reg_score_polr(fit)
  if (is.null(eng) || is.null(sc)) return(NULL)
  cif <- reg_if_from_score(sc$S, sc$bread)
  if (is.null(cif)) return(NULL)
  j <- match(as.character(category), eng$levels)
  if (is.na(j)) return(NULL)
  w <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (length(w) != nrow(data) || !all(is.finite(w))) return(NULL)
  sw <- sum(w)
  cf <- function(lv, var) {
    d <- data; x <- data[[var]]
    d[[var]] <- if (is.factor(x)) factor(as.character(lv), levels = levels(x))
                else if (is.numeric(x)) x + as.numeric(lv)
                else as.character(lv)
    eng$mm(d)
  }
  function(var, level, ref) {
    X1 <- cf(level, var); X0 <- cf(ref, var)
    if (is.null(X1) || is.null(X0)) return(NULL)
    est <- function(th) {
      p1 <- tryCatch(eng$probs(th, X1)[, j], error = function(e) NULL)
      p0 <- tryCatch(eng$probs(th, X0)[, j], error = function(e) NULL)
      if (is.null(p1) || is.null(p0)) return(NULL)
      list(p1 = p1, p0 = p0)
    }
    m <- est(eng$theta)
    if (is.null(m) || !all(is.finite(m$p1)) || !all(is.finite(m$p0))) return(NULL)
    val <- function(pp) if (ratio) log(sum(w * pp$p1) / sum(w * pp$p0))
                        else       sum(w * (pp$p1 - pp$p0)) / sw
    A  <- val(m)
    if (!is.finite(A)) return(NULL)
    # the empirical-averaging term (the same two shapes reg_ame_if_maker uses)
    if (ratio) {
      M1 <- sum(w * m$p1) / sw; M0 <- sum(w * m$p0) / sw
      if (!isTRUE(M1 > 0) || !isTRUE(M0 > 0)) return(NULL)
      emp <- w * (m$p1 - M1) / (sw * M1) - w * (m$p0 - M0) / (sw * M0)
    } else {
      emp <- w * ((m$p1 - m$p0) - A) / sw
    }
    # the delta term: G = d(estimand)/d(theta) by central differences (one perturbation per parameter
    # serves every contrast in this call; ~140 ms for 20 parameters, measured).
    th <- eng$theta
    h  <- pmax(1e-5, abs(th) * 1e-5)
    G  <- vapply(seq_along(th), function(r) {
      tp <- th; tp[r] <- tp[r] + h[r]; up <- est(tp)
      tm <- th; tm[r] <- tm[r] - h[r]; dn <- est(tm)
      if (is.null(up) || is.null(dn)) return(NA_real_)
      (val(up) - val(dn)) / (2 * h[r])
    }, numeric(1))
    if (anyNA(G)) return(NULL)
    delta <- cif(G)
    if (is.null(delta)) return(NULL)
    emp + delta
  }
}

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
