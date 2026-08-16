# R/reg-influence.R -- Phase 18z8-B: influence functions, and the standard error of the GAP between
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
# orchestration (which cells get a gap SE at all) lives in R/tab_reg.R's reg_gap_se_columns(); the
# printed marginal effects are R/tab_reg.R's reg_marginal_gcomp(). This is the ONE place in the package
# that calls survey::svyrecvar().
#
#   reg_if_from_parts(X, W, r)  the ONE formula, as a closure over a contrast
#   reg_coef_if_maker(fit)      its fit adapter -- lm / glm / svyglm alike
#   reg_crude_if_maker(...)     the observed side of a FACTOR row, in closed form (no fit at all)
#   reg_counterfactual(...)     "the sample with `var` set to this level", shared by the two below
#   reg_gcomp_maker(...)        THE marginal sweep: estimate + analytic jacobian + empirical term
#   reg_gcomp_cat_maker(...)    its 3+ level twin, answering for every outcome category at once
#   reg_ame_if_maker(...)       effect = "ame" / "ame_ratio" (the two-term marginal influence function)
#   reg_if_se(d, design)        design-based when a design exists, IID otherwise
#
# Phase 20d -- the g-computation makers are the PRODUCERS and the influence makers their consumers,
# because one sweep answers two questions: what does this marginal effect print (delta method, the
# printed interval) and is it different from its crude twin (influence function, the colour). See
# reg_gcomp_maker()'s own note for why those two variances differ and must not be swapped.
#
# Phase 18z9 -- TWO crude paths, by predictor kind. A factor's crude effect is a saturated one-factor
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
  # Phase 18z10: a 3+ level fit has no working residuals / IRLS weights, so it goes through the
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
# Phase 18z10: the outcome + weights come from reg_crude_yw(), the ONE description of "what the crude
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
reg_crude_if_maker <- function(data, outcome, crude_key, positive_level, wt, link,
                               trials = NULL, category = "", ref_category = NULL) {
  gp <- switch(as.character(link)[1],
               "logit"    = function(m) 1 / (m * (1 - m)),
               "log"      = function(m) 1 / m,
               "identity" = function(m) 1,
               NULL)
  if (is.null(gp)) return(NULL)
  yw <- tryCatch(reg_crude_yw(data, outcome, crude_key, positive_level, wt, trials, ref_category),
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

# === G-COMPUTATION: THE MARGINAL SIDE ===========================================================
#
# reg_counterfactual(data, var, lv) -- "what the sample would look like with `var` set to lv", the ONE
# rule the two g-computation makers share.
#
# CONTRACT, shared by every closure built on it, `(var, level, ref)`:
#   * `var` is a FACTOR  -- `level` / `ref` are level LABELS, and the counterfactual sets the whole
#     column to that level (the classic "everyone at level j vs everyone at the reference").
#   * `var` is NUMERIC   -- Phase 18z9: `level` / `ref` are SHIFTS added to the observed x, so the
#     caller passes (k, 0) for a k-unit contrast. That is marginaleffects' own forward difference
#     `variables = list(v = k)`, which is what the numeric AME column shows. Assigning
#     `as.character(lv)` unconditionally turned a numeric column into character -- model.matrix() then
#     either errored (caught -> NULL, i.e. no test) or built the wrong contrast width.
#
# WARNING -- assign through `[<-`, never through a fresh factor(). `factor(lv, levels = levels(x))`
# drops the `ordered` class, and an ordered predictor then gets TREATMENT contrasts where the fit used
# polynomial ones: measured on gss `rincome`, an AME of 0.1038 instead of 0.0302, silently. It cannot
# bite through tab_reg() -- Phase 14r's reg_fit() de-orders every factor predictor before fitting -- but
# this function's argument is "a level label", and it must be right for one.
#' @keywords internal
reg_counterfactual <- function(data, var, lv) {
  x <- data[[var]]
  if (is.factor(x)) {
    if (!as.character(lv) %in% levels(x)) return(NULL)   # an absent level is no answer, not an NA column
    data[[var]][] <- as.character(lv)
  } else if (is.numeric(x)) {
    data[[var]] <- x + as.numeric(lv)
  } else {
    data[[var]] <- as.character(lv)
  }
  data
}

# reg_gcomp_maker() -- G-COMPUTATION for a single-equation fit (lm / glm / svyglm). An average marginal
# effect and its two variances are the same sweep read three ways, so ONE producer computes all of it:
#
#   est   = mean_i w_i (mu1_i - mu0_i)   (or log(M1/M0))   the printed estimate
#   G     = d(est)/d(beta)                                 the delta method's jacobian, ANALYTIC
#   emp   = the empirical-averaging influence term
#   mean1 / mean0                                          the counterfactual adjusted means (`pct`)
#
# TWO CONSUMERS, TWO VARIANCES, AND THEY MUST NOT BE SWAPPED (Phase 20d):
#
#   * the PRINTED interval is  est +- crit * sqrt(G' vcov(fit) G)  -- exactly marginaleffects' own
#     quantity, which it reaches by a NUMERICAL derivative costing one full re-prediction per
#     coefficient (measured: 5.9 s, against 0.8 s with vcov = FALSE, on 13 000 rows x 14 coefficients).
#     Ours agrees with it to 1e-8 on estimate, standard error, both bounds and the p-value, on glm and
#     weighted svyglm alike.
#   * the GAP test needs the INFLUENCE FUNCTION, `emp + IF^beta %*% G` (reg_ame_if_maker below), because
#     only an influence function carries the covariance between the model effect and its crude twin:
#
#       additive (effect = "ame"):       IF_i = wt_i (g_i - AME)       + IF^beta_i %*% G
#       ratio    (effect = "ame_ratio"): IF_i = wt_i (mu1_i - M1)/M1 - wt_i (mu0_i - M0)/M0
#                                              + IF^beta_i %*% (G1/M1 - G0/M0)
#
#     with g_i = mu1_i - mu0_i and wt_i = w_i / sum(w) (dev/model_vs_observed_gap_test.md SS3.4).
#
# That influence-function standard error is a SANDWICH variance and adds the empirical-averaging term;
# measured against marginaleffects it differs by up to 3.6 % on a rare level. It is the better answer to
# "is this effect different from that one" and the wrong answer to "what interval does this AME print".
#
# The counterfactual design matrices are built and released ONE LEVEL AT A TIME: `G` is a p-vector and
# `g_i` a length-n vector, so peak memory stays X + one counterfactual.
#' @keywords internal
reg_gcomp_maker <- function(fit, data, wt, ratio) {
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
    d <- reg_counterfactual(data, var, lv)
    if (is.null(d)) return(NULL)
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
    M1 <- sum(w * m1) / sw; M0 <- sum(w * m0) / sw
    if (ratio) {
      if (!isTRUE(M1 > 0) || !isTRUE(M0 > 0)) return(NULL)
      est <- log(M1 / M0)
      emp <- w * (m1 - M1) / (sw * M1) - w * (m0 - M0) / (sw * M0)
      G   <- colSums(w * X1 * d1) / (sw * M1) - colSums(w * X0 * d0) / (sw * M0)
    } else {
      est <- sum(w * (m1 - m0)) / sw
      emp <- w * ((m1 - m0) - est) / sw
      G   <- colSums(w * (X1 * d1 - X0 * d0)) / sw
    }
    list(est = est, G = G, emp = emp, mean1 = M1, mean0 = M0)
  }
}

# reg_ame_if_maker() -- the g-computation above wearing its influence-function hat: the empirical term
# plus the coefficient influence carried along the jacobian.
#' @keywords internal
reg_ame_if_maker <- function(fit, data, wt, ratio, coef_if) {
  if (is.null(coef_if)) return(NULL)
  g <- reg_gcomp_maker(fit, data, wt, ratio)
  if (is.null(g)) return(NULL)
  function(var, level, ref) {
    p <- g(var, level, ref)
    if (is.null(p)) return(NULL)
    delta <- coef_if(unname(p$G))
    if (is.null(delta)) return(NULL)
    emp <- reg_if_align(p$emp, length(delta), data[[svy_row_col]])
    if (is.null(emp)) return(NULL)
    emp + delta
  }
}


# === 3+ LEVEL OUTCOMES (Phase 18z10) ============================================================
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
reg_score_multinom <- function(fit) {
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
reg_score_polr <- function(fit) {
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
# neither package exposes. Returns list(theta, levels, mm(newdata), probs(theta, X),
# dmean(X, P, j, w)); NULL for anything else.
#
# DESIGN -- why a local predictor is not a duplicate implementation. marginaleffects computes its
# delta-method standard errors from an internal jacobian it does not expose as an attribute (checked),
# and perturbing the fit and re-calling it p+1 times costs ~4.6 s per table. The local softmax /
# cumulative-logit IS the same arithmetic the SCORE functions above already need, so there is ONE
# predictor with three consumers, not three predictors. It is policed the way reg_crude_if_maker() is: a
# test pins the local AME to marginaleffects::avg_comparisons() (which it reproduces to 8 significant
# digits) rather than to a hand-written expectation.
#
# `dmean(X, P, j, w)` is Phase 20d's addition: d/d(theta) of `sum_i w_i P_ij`, ANALYTIC, in the SAME
# parameter order as `theta` and `vcov(fit)`. It is the derivative of `probs` above, so it belongs to
# the one predictor rather than beside a caller, and it is what lets both the printed interval and the
# gap test read one jacobian. It replaced a central-difference jacobian that cost 2.4 s PER CONTRAST
# (44 re-predictions on a 21 000-row frame); the analytic one is 6.6 ms and agrees with it, and with
# marginaleffects' standard error, to ~1e-9.
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
      },
      # softmax: d p_ij / d beta_{c,m} = p_ij (1{j == c} - p_ic) x_im, for the K-1 NON-reference
      # categories c (column 1 of P is the reference and has no coefficients). Column-major over
      # (c, m) == `as.vector(t(B))` == CATEGORY-MAJOR, the order `theta` and `vcov` already use.
      dmean = function(X, P, j, w) {
        as.vector(vapply(seq_len(ncol(P))[-1L], function(cc)
          colSums((w * P[, j] * ((j == cc) - P[, cc])) * X), numeric(ncol(X))))
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
      },
      # cumulative logit: P_ij = F(z_j - eta_i) - F(z_{j-1} - eta_i), with F(z_0 - eta) := 0 and
      # F(z_K - eta) := 1, so both densities vanish at the outer categories. Hence
      #   d P_ij / d b_m  = (f(z_{j-1} - eta) - f(z_j - eta)) x_im
      #   d P_ij / d z_k  =  f(z_j - eta) 1{k == j} - f(z_{j-1} - eta) 1{k == j-1}
      # in `theta`'s own order, c(beta, zeta).
      dmean = function(X, P, j, w) {
        eta <- as.vector(X %*% unname(b))
        n0  <- numeric(length(eta))
        fhi <- if (j <= length(z))  stats::dlogis(z[[j]]      - eta) else n0
        flo <- if (j >= 2L)         stats::dlogis(z[[j - 1L]] - eta) else n0
        c(colSums((w * (flo - fhi)) * X),
          vapply(seq_along(z), function(k) {
            v <- n0
            if (k == j)      v <- v + fhi
            if (k == j - 1L) v <- v - flo
            sum(w * v)
          }, numeric(1)))
      }))
  }
  NULL
}

# reg_gcomp_cat_maker() -- reg_gcomp_maker()'s twin for a 3+ level outcome. Same closure contract, one
# difference that is forced by the shape of the table: a multinomial / ordinal model shows ONE COLUMN
# PER OUTCOME CATEGORY, so the closure answers for ALL of them at once. The two counterfactual
# probability matrices are what costs anything, and they serve every category, so producing them once
# is also what makes this cheaper than asking per category.
#' @keywords internal
reg_gcomp_cat_maker <- function(fit, data, wt, ratio) {
  eng <- reg_prob_engine(fit)
  if (is.null(eng)) return(NULL)
  w <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (length(w) != nrow(data) || !all(is.finite(w))) return(NULL)
  sw <- sum(w)
  cf <- function(lv, var) {
    d <- reg_counterfactual(data, var, lv)
    if (is.null(d)) return(NULL)
    eng$mm(d)
  }
  function(var, level, ref) {
    X1 <- cf(level, var); X0 <- cf(ref, var)
    if (is.null(X1) || is.null(X0)) return(NULL)
    P1 <- tryCatch(eng$probs(eng$theta, X1), error = function(e) NULL)
    P0 <- tryCatch(eng$probs(eng$theta, X0), error = function(e) NULL)
    if (is.null(P1) || is.null(P0) || !all(is.finite(P1)) || !all(is.finite(P0))) return(NULL)
    K <- length(eng$levels)
    est <- M1 <- M0 <- numeric(K)
    G <- emp <- vector("list", K)
    for (j in seq_len(K)) {
      p1 <- P1[, j]; p0 <- P0[, j]
      M1[j] <- sum(w * p1) / sw; M0[j] <- sum(w * p0) / sw
      g1 <- eng$dmean(X1, P1, j, w); g0 <- eng$dmean(X0, P0, j, w)
      if (ratio) {
        if (!isTRUE(M1[j] > 0) || !isTRUE(M0[j] > 0)) return(NULL)
        est[j]   <- log(sum(w * p1) / sum(w * p0))
        emp[[j]] <- w * (p1 - M1[j]) / (sw * M1[j]) - w * (p0 - M0[j]) / (sw * M0[j])
        G[[j]]   <- g1 / (sw * M1[j]) - g0 / (sw * M0[j])
      } else {
        est[j]   <- sum(w * (p1 - p0)) / sw
        emp[[j]] <- w * ((p1 - p0) - est[j]) / sw
        G[[j]]   <- (g1 - g0) / sw
      }
      if (!is.finite(est[j])) return(NULL)
    }
    list(levels = eng$levels, est = est, G = G, emp = emp, mean1 = M1, mean0 = M0)
  }
}

# reg_ame_if_cat_maker() -- the marginal influence function for a 3+ level outcome, ONE outcome
# category at a time (each model column shows one). The g-computation above, plus the score-based
# coefficient influence: same two-term shape as reg_ame_if_maker().
#' @keywords internal
reg_ame_if_cat_maker <- function(fit, data, wt, ratio, category) {
  eng <- reg_prob_engine(fit)
  sc  <- if (inherits(fit, "multinom")) reg_score_multinom(fit) else reg_score_polr(fit)
  if (is.null(eng) || is.null(sc)) return(NULL)
  cif <- reg_if_from_score(sc$S, sc$bread)
  if (is.null(cif)) return(NULL)
  j <- match(as.character(category), eng$levels)
  if (is.na(j)) return(NULL)
  g <- reg_gcomp_cat_maker(fit, data, wt, ratio)
  if (is.null(g)) return(NULL)
  function(var, level, ref) {
    p <- g(var, level, ref)
    if (is.null(p)) return(NULL)
    delta <- cif(p$G[[j]])
    if (is.null(delta)) return(NULL)
    emp <- reg_if_align(p$emp[[j]], length(delta), data[[svy_row_col]])
    if (is.null(emp)) return(NULL)
    emp + delta
  }
}

# reg_if_align() -- Phase 18z14-iii: put an influence vector built on a FRAME into the row space
# the DESIGN uses, which is also the fit's. `[` does not drop rows on a CALIBRATED or PPS design --
# survey keeps all n and sets prob = Inf -- so svy_domain_design() pads the fit's design back to full
# length and svyglm keeps those zero-weight rows in model.matrix(). A leg built on the complete-case
# frame is then SHORTER than its counterpart, and the two could not be differenced: measured, the
# closed-form crude leg was 380 against a model leg of 400 (the length guard dropped the test), while
# reg_ame_if_maker()'s own `emp + delta` RECYCLED, i.e. returned a wrong number with only a warning.
# Scattering with zeros is exact, not an approximation: the padded rows carry design weight 0, so they
# contribute nothing to either term. NULL when no row rule applies (svy_row_at()).
#' @keywords internal
reg_if_align <- function(v, n, des_rows) {
  if (is.null(v) || length(v) == n) return(v)
  at <- svy_row_at(n, suppressWarnings(as.integer(des_rows)))
  if (is.null(at) || length(at) != length(v)) return(NULL)
  out <- numeric(n); out[at] <- v; out
}

# reg_delta_se() -- the standard error a g-computed quantity PRINTS: the delta method, sqrt(G' V G),
# with V the fit's own variance-covariance matrix. It is marginaleffects' quantity exactly (measured to
# 1e-8 on glm and weighted svyglm alike); the difference is that G comes from reg_gcomp_maker()
# analytically instead of from p+1 numerical re-predictions. On an svyglm, `vcov(fit)` is already the
# design-based sandwich, so a survey design is right by construction and needs no branch here.
# ⚠ NOT interchangeable with reg_if_se() below -- see reg_gcomp_maker()'s note on the two variances.
#' @keywords internal
reg_delta_se <- function(G, V) {
  if (is.null(G) || is.null(V) || !is.matrix(V)) return(NA_real_)
  g <- as.numeric(G)
  if (!length(g) || anyNA(g) || nrow(V) != ncol(V) || length(g) != nrow(V)) return(NA_real_)
  nm <- names(G)
  if (!is.null(nm) && !is.null(rownames(V)) && all(nm %in% rownames(V)))
    V <- V[nm, nm, drop = FALSE]                     # by NAME where both carry one, position otherwise
  v <- as.numeric(t(g) %*% V %*% g)
  if (!isTRUE(is.finite(v)) || v < 0) return(NA_real_)
  sqrt(v)
}

# reg_if_se() -- the standard error of a quantity whose per-observation influence contributions are `d`.
# With a survey design that is survey::svyrecvar() -- the Binder (1983) linearization survey uses for
# its OWN variances, so strata, clusters and finite-population corrections come along for free
# (measured: it reproduces SE(svyglm) exactly, ratio 1.0000, while the IID version is 6 % too small on a
# mild stratified/clustered design). Without one it is the plain sum of squares.
# Phase 18z16-iiiii (defect 5): the svyrecvar call goes through svy_var_recvar(), the ONE place the
# package answers the lonely-PSU question. It was inlined here WITHOUT that policy, so survey's default
# ("fail") made svyrecvar error on a design with a single-PSU stratum -- the tryCatch then returned NA
# and the gap test silently vanished, while tab()'s cell variances and the omnibus test, which both
# say "adjust", succeeded on the very same design.
#' @keywords internal
reg_if_se <- function(d, design = NULL) {
  if (is.null(d)) return(NA_real_)
  d <- as.numeric(d)
  if (!length(d) || anyNA(d)) return(NA_real_)
  v <- if (is.null(design)) sum(d * d) else
    as.numeric(svy_var_recvar(as.matrix(d), design) %||% NA_real_)
  if (!isTRUE(is.finite(v)) || v < 0) return(NA_real_)
  sqrt(v)
}
