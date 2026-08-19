# PURPOSE: INFLUENCE FUNCTIONS, and the standard error of the GAP between two estimators fitted on
#   the SAME rows.
# ROLE: what makes `color = "adjustment"` a test rather than a description. A model effect and its
#   observed (crude) counterpart are both M-estimators on the same observations, so they are
#   strongly correlated: the naive sqrt(se_adj^2 + se_crude^2) is two to four times too large, and
#   Hausman's Var(crude) - Var(adj) goes NEGATIVE for logistic. The only quantity that carries the
#   covariance is the difference of their influence functions,
#
#     Var(theta_adj - theta_crude) = Var( sum_i ( IF_i^adj - IF_i^crude ) )
#
#   which is seemingly-unrelated estimation (Stata's `suest`; Weesie 1999; Mize, Doan & Long 2019).
# KEY CONSTRAINTS:
#   - Pure matrix maths over `stats` + `survey`. No fmt types, no tabxplor classes: every function
#     takes vectors, matrices and fits, and returns a number or a closure.
#   - EVERY FUNCTION RETURNS NULL RATHER THAN A WRONG NUMBER when its inputs do not support the
#     computation (singular information matrix, absent term, empty cell, unknown link). The caller
#     reads NULL as "no test here" and writes no `gap_se`; MEASURES' force_policy closure then makes
#     the column read under `ignore`, so a degraded path is descriptive, never falsely significant.
#   - THE MEMORY CONTRACT: never materialise the per-observation influence matrix. reg_if_from_parts()
#     exploits the fact that the score is a pure ROW SCALING of the model matrix, so peak memory
#     stays the ONE n x p matrix the caller already holds -- and the callers (R/reg-empirical.R,
#     R/reg-assumptions.R) keep to one length-n difference vector at a time.
#   - ONE SWEEP, TWO VARIANCES, never swapped: a g-computation maker returns both the delta-method
#     variance (the interval the marginal effect PRINTS) and the influence-function one (the colour's
#     test). reg_gcomp_maker()'s own note says why they differ.
#   - TWO CRUDE PATHS, by predictor kind. A FACTOR's crude effect is a saturated one-factor GLM, so
#     it has a closed form and needs no fit; a CONTINUOUS predictor has no cells, so its crude leg is
#     reg_coef_if_maker() over the univariable fit R/reg-empirical.R built. Both legs are then the
#     same machinery over two fits solved on the same rows -- which is why the counterfactual takes
#     SHIFTS rather than levels for a numeric column.
#   - Design-based variance goes through svy_var_recvar() (R/survey-variance.R), the package's one
#     wrapper over survey::svyrecvar(); this file is its only regression-side caller.
#
# The orchestration -- which cells get a gap SE at all -- is reg_gap_se_columns() in
# R/reg-empirical.R; the printed marginal effects are reg_marginal_gcomp() in R/tab_reg.R.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem);
#      dev/model_vs_observed_gap_test.md (the derivation and the calibration study).


# reg_if_from_parts() -- the influence function of ANY M-estimator solving sum_i X_i W_i r_i = 0, as a
# closure over the CONTRAST rather than a matrix:
#
#     A = X' W X        U_i = X_i W_i r_i        IF_i = U_i A^-1
#
# and the influence of one linear combination L is IF %*% L. `U = X * (W*r)` is a pure ROW scaling, so
# holding X, W*r and A^-1 gives every contrast for one length-n allocation, without ever building U
# or IF as an n x p matrix.
#
# WARNING: peak memory is ONE n x p matrix -- `model.matrix(fit)`. At n = 5M, p = 50 that is ~2 GB.
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
# `fit$weights` are the IRLS working weights and `residuals(type = "working")` is (y-mu)/mu'(eta), so
# X'Wr is the score and X'WX the information -- verified bit-identical to
# `attr(svyglm(..., influence = TRUE), "influence")`.
#
# NB the SE this implies is the HUBER-WHITE SANDWICH: what svyglm prints, but a plain unweighted glm
# prints the model-based SE instead (they agree only up to O(1/n)).
#
# Backticks are stripped from column names as reg_fit() strips them from `td$term`, so a contrast can
# be keyed by `skeleton$term` with no second naming rule. Rank-deficient coefficients are dropped.
#' @keywords internal
reg_coef_if_maker <- function(fit, V = NULL) {
  # a 3+ level fit has no working residuals / IRLS weights, so it goes through the score core instead.
  # `V` is the fit's vcov when a caller already holds it; the GLM path below builds its own, unused.
  if (inherits(fit, "multinom") || inherits(fit, "polr")) {
    sc <- if (inherits(fit, "multinom")) reg_score_multinom(fit, V) else reg_score_polr(fit, V)
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

# reg_crude_if_maker() -- the OBSERVED side, in closed form: every `Obs_*` effect is exactly the
# coefficient of a saturated one-factor GLM at the matching link, so its influence function is
#
#   IF_i = 1(x_i = l) w_i (y_i - mu_l) / sum_{x=l} w * g'(mu_l)
#        - 1(x_i = r) w_i (y_i - mu_r) / sum_{x=r} w * g'(mu_r)
#
# with no fit at all; for the unweighted binomial case its SE is exactly the WOOLF interval `Obs_OR`
# already prints.
#
# WARNING: `link` describes the CRUDE estimator, not the model's family -- e.g. a binomial model
# shows a logit-scale OR by default but an identity-link risk difference or log-link risk ratio under
# `effect = "marginal"` (a fact of the REG_EMPIRICAL SHAPE row).
#
# `y` and its weights come from reg_crude_yw(), matching the estimate this is the SE of:
#   grouped binomial : y = succ/trials, weight w*trials; reg_if_se() sums over ROWS (cluster-robust).
#   multinomial      : `category` picks the 0/1 indicator of that outcome category.
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
# reg_counterfactual(data, var, lv) -- "what the sample would look like with `var` set to lv", shared
# by both g-computation makers.
#
# CONTRACT `(var, level, ref)`:
#   * `var` is a FACTOR  -- `level` / `ref` are level LABELS; the counterfactual sets the whole column
#     to that level.
#   * `var` is NUMERIC   -- `level` / `ref` are SHIFTS added to the observed x (marginaleffects' own
#     forward difference `variables = list(v = k)`).
#
# WARNING -- assign through `[<-`, never a fresh factor(): `factor(lv, levels = levels(x))` drops the
# `ordered` class, giving TREATMENT contrasts where the fit used polynomial ones (measured on gss
# `rincome`: an AME of 0.1038 instead of 0.0302, silently).
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

# reg_gcomp_maker() -- G-COMPUTATION for a single-equation fit (lm / glm / svyglm): an average
# marginal effect and its two variances are the same sweep read three ways:
#
#   est   = mean_i w_i (mu1_i - mu0_i)   (or log(M1/M0))   the printed estimate
#   G     = d(est)/d(beta)                                 the delta method's jacobian, ANALYTIC
#   emp   = the empirical-averaging influence term
#   mean1 / mean0                                          the counterfactual adjusted means (`pct`)
#
# TWO CONSUMERS, TWO VARIANCES, NEVER SWAPPED:
#
#   * the PRINTED interval is  est +- crit * sqrt(G' vcov(fit) G)  -- exactly marginaleffects' own
#     quantity, reached here analytically instead of by a numerical derivative.
#   * the GAP test needs the INFLUENCE FUNCTION, `emp + IF^beta %*% G` (reg_ame_if_maker below), the
#     only quantity that carries the covariance between the model effect and its crude twin:
#
#       additive (marginal difference):  IF_i = wt_i (g_i - AME)       + IF^beta_i %*% G
#       ratio    (marginal ratio):       IF_i = wt_i (mu1_i - M1)/M1 - wt_i (mu0_i - M0)/M0
#                                              + IF^beta_i %*% (G1/M1 - G0/M0)
#
#     with g_i = mu1_i - mu0_i and wt_i = w_i / sum(w) -- a SANDWICH variance, the right answer to
#     "is this different from that", the wrong one for "what interval does this AME print".
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

# reg_ame_if_maker() -- the g-computation above wearing its influence-function hat.
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


# === 3+ LEVEL OUTCOMES ===========================================================================
#
# reg_coef_if_maker() above needs model.matrix() + residuals(type = "working") + fit$weights, none of
# which nnet::multinom or MASS::polr provides. Every M-estimator's influence is still
#
#     IF = (per-observation score) %*% (bread)
#
# and reg_if_from_parts() is already exactly that (X*(W*r) the score, solve(X'WX) the bread) -- only
# who supplies the score changes.
#
# WARNING -- the two cores are NOT merged: a multinomial / cumulative-logit score has no row-scaling
# structure, unlike reg_if_from_parts()'s U, so it is held as a real n x q matrix. They share the
# CONTRACT (a closure, NULL on failure), not the algebra.
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

# The per-observation score of a MULTINOMIAL logit: U_i,(j) = x_i (1{y_i = j} - p_ij), stacked
# CATEGORY-MAJOR.
# WARNING: coef(multinom) is (K-1) x p; vcov(multinom) is ordered category-major while as.vector() on
# that matrix is category-MINOR -- backwards, the SE is ~2.7x too large with no warning. The defence
# is structural: columns are NAMED, so a mismatch is a NULL, never a wrong number.
#' @keywords internal
reg_score_multinom <- function(fit, V = NULL) {
  if (is.null(V)) V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
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
# WARNING: the bread is vcov(fit), NEVER solve(fit$Hessian) -- MASS::polr optimises over a different
# parametrisation, (beta, zeta_1, log(diff zeta)); vcov() applies the transform's jacobian. (svyolr's
# fit$var is the SANDWICH, not the bread, and is unreachable: tab_reg() refuses a weighted 3+ level
# outcome under `effect = "marginal"`.)
#' @keywords internal
reg_score_polr <- function(fit, V = NULL) {
  if (inherits(fit, "svyolr")) return(NULL)
  if (is.null(V)) V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
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
# DESIGN -- a local predictor, not a duplicate: the local softmax / cumulative-logit IS the same
# arithmetic the SCORE functions above already need, so ONE predictor serves three consumers. Tested
# against marginaleffects::avg_comparisons() rather than a hand-written expectation.
#
# `dmean(X, P, j, w)` is d/d(theta) of `sum_i w_i P_ij`, ANALYTIC, in the SAME order as `theta` /
# `vcov(fit)`, so both the printed interval and the gap test read one jacobian.
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

# reg_gcomp_cat_maker() -- reg_gcomp_maker()'s twin for a 3+ level outcome: a multinomial / ordinal
# model shows ONE COLUMN PER CATEGORY, so the closure answers for ALL of them at once from the same
# two counterfactual probability matrices.
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

# reg_ame_if_cat_maker() -- the marginal influence function for a 3+ level outcome, ONE category at a
# time: the g-computation above plus the score-based coefficient influence, same shape as
# reg_ame_if_maker().
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

# reg_if_align() -- put an influence vector built on a FRAME into the row space the DESIGN uses (also
# the fit's). `[` does not drop rows on a CALIBRATED or PPS design (survey keeps all n, prob = Inf), so
# a leg built on the complete-case frame is SHORTER than its counterpart: without this, `emp + delta`
# in reg_ame_if_maker() would RECYCLE, returning a wrong number with only a warning.
# Scattering with zeros is exact: the padded rows carry design weight 0, contributing nothing to
# either term. NULL when no row rule applies (svy_row_at()).
#' @keywords internal
reg_if_align <- function(v, n, des_rows) {
  if (is.null(v) || length(v) == n) return(v)
  at <- svy_row_at(n, suppressWarnings(as.integer(des_rows)))
  if (is.null(at) || length(at) != length(v)) return(NULL)
  out <- numeric(n); out[at] <- v; out
}

# reg_delta_se() -- the standard error a g-computed quantity PRINTS: sqrt(G' V G), the delta method,
# with V the fit's own vcov -- marginaleffects' quantity exactly, reached here analytically. On an
# svyglm, `vcov(fit)` is already the design-based sandwich, so no branch is needed here.
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

# reg_if_se() -- the SE of a quantity whose per-observation influence contributions are `d`. With a
# survey design that is survey::svyrecvar() (Binder 1983); without one, the plain sum of squares.
# The call goes through svy_var_recvar(), the ONE place the package answers the lonely-PSU question:
# survey's default ("fail") errors on a single-PSU stratum, which an un-policied tryCatch would
# silently turn into a vanished gap test.
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
