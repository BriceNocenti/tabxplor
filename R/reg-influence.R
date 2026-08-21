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
#   - ONE CLOSURE CONTRACT, THREE SWEEPS. reg_gcomp_maker() (single-equation), reg_gcomp_cat_maker()
#     (one answer per outcome category) and reg_gcomp_rank_maker() (the ordinal superiority pair: the
#     whole predicted distribution in, one number out) return the same list, so the callers pick a
#     sweep and branch on nothing else -- and the rank one, answering once, IS a drop-in for the
#     single-equation path from reg_marginal_gcomp() downwards. Its own arithmetic is shared further
#     still: reg_rank_pair() is the one comparison the model column and its crude twin both run.
#   - ONE LINK TABLE, THREE READERS. A marginal contrast is h(M1) - h(M0), so a link contributes the
#     transform and its derivative and nothing else (REG_LINK_FUNS). Both g-computation makers and
#     the crude leg read it, which is why the engines never knew which measure they were computing
#     and a new link costs no arm here. ⚠ it is the REPORTED measure's link, never the fit's: a
#     logistic model shows an additive AME by default, a marginal risk ratio or a marginal odds ratio
#     on request, all three from one sweep.
#   - TWO CRUDE PATHS, by predictor kind. A FACTOR's crude effect is a saturated one-factor GLM, so
#     it has a closed form and needs no fit; a CONTINUOUS predictor has no cells, so its crude leg is
#     reg_coef_if_maker() over the univariable fit R/reg-empirical.R built. Both legs are then the
#     same machinery over two fits solved on the same rows -- which is why the counterfactual takes
#     SHIFTS rather than levels for a numeric column.
#   - EVERY quantity the g-computation sweep produces is a WEIGHTED MEAN, which is why a subgroup
#     effect -- a crossed slope read within one level of its moderator -- is the same sweep under a
#     restricted weight vector (`mask`), and its influence function follows with no second engine.
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

# REG_LINK_FUNS -- a MARGINAL contrast is h(M1) - h(M0), so a link contributes exactly two things:
# the transform h and its derivative h'(M), the delta-method factor. ONE declaration, three readers
# (both g-computation makers and the crude leg below), which is why the engines are link-agnostic
# and a new link costs no arm here.
#
# `ok` is the DOMAIN, and it is not decoration: a log has no answer at a 0 % cell and a logit none at
# 0 % or 100 %, so the maker returns NULL -- "no test here" -- rather than an Inf.
#' @keywords internal
#' @noRd
REG_LINK_FUNS <- list(
  identity = list(h = function(m) m,                dh = function(m) 1,
                  ok = function(m) TRUE),
  log      = list(h = function(m) log(m),           dh = function(m) 1 / m,
                  ok = function(m) all(m > 0)),
  logit    = list(h = function(m) log(m / (1 - m)), dh = function(m) 1 / (m * (1 - m)),
                  ok = function(m) all(m > 0 & m < 1))
)

#' @keywords internal
#' @noRd
reg_link_funs <- function(link) REG_LINK_FUNS[[as.character(link)[1]]]

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
#
# ⚠ WHICH CONTRAST, read off the estimator rather than assumed. A categorical outcome offers two, and
# they are different quantities:
#   * category vs REST   -- what a marginal probability contrast estimates (identity / log link).
#   * category vs PIVOT  -- what a multinomial's own CONDITIONAL odds ratio estimates, and what the
#     crude column's interval computes since 22b-xiii-1. Restricting the weights to the {category,
#     pivot} pair makes `mu` the share WITHIN the pair, whose logit is that log-odds; unweighted, the
#     resulting SE is exactly Woolf on the restricted 2x2, as it is for the binary case below.
# The two coincide when the outcome has 2 categories ("the rest" IS the pivot), which is why a binary
# outcome needs no arm of its own.
#' @keywords internal
reg_crude_if_maker <- function(data, outcome, crude_key, positive_level, wt, link,
                               trials = NULL, category = "", ref_category = NULL) {
  lf <- reg_link_funs(link)
  if (is.null(lf)) return(NULL)
  gp <- lf$dh
  yw <- tryCatch(reg_crude_yw(data, outcome, crude_key, positive_level, wt, trials, ref_category),
                 error = function(e) NULL)
  if (is.null(yw)) return(NULL)
  # a categorical outcome averages the 0/1 indicator of the category this column shows
  cat_lv <- if (nzchar(category)) category else "1"
  y <- if (identical(yw$cats, "")) as.numeric(yw$y) else
    as.numeric(as.character(yw$y) == cat_lv)
  w <- yw$w
  # an ODDS ratio on a 3+ category outcome is conditional on the pivot: drop everything else.
  if (length(yw$cats) > 2L && identical(as.character(link)[1], "logit"))
    w <- w * as.numeric(as.character(yw$y) %in% c(cat_lv, yw$ref))
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
#       IF_i = wt_i (mu1_i - M1) h'(M1) - wt_i (mu0_i - M0) h'(M0) + IF^beta_i %*% G
#
#     with wt_i = w_i / sum(w) -- a SANDWICH variance, the right answer to "is this different from
#     that", the wrong one for "what interval does this AME print". At the identity link h' is 1 and
#     it collapses to the familiar wt_i (g_i - AME).
#
# `link` is the REPORTED comparison's, never the fit's: a logistic model shows an additive AME by
# default and a marginal risk ratio or odds ratio on request, all three from this one sweep.
#' @keywords internal
reg_gcomp_maker <- function(fit, data, wt, link = "identity") {
  lf  <- reg_link_funs(link)
  if (is.null(lf)) return(NULL)
  tt  <- tryCatch(stats::delete.response(stats::terms(fit)), error = function(e) NULL)
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(tt) || is.null(fam) || is.null(fam$linkinv) || is.null(fam$mu.eta)) return(NULL)
  b    <- stats::coef(fit)
  keep <- !is.na(b)
  bk   <- b[keep]
  w0   <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (length(w0) != nrow(data) || !all(is.finite(w0))) return(NULL)
  cf <- function(lv, var) {                       # the counterfactual model matrix at one level/shift
    d <- reg_counterfactual(data, var, lv)
    if (is.null(d)) return(NULL)
    X <- tryCatch(stats::model.matrix(tt, d), error = function(e) NULL)
    if (is.null(X) || ncol(X) != length(b)) return(NULL)
    X[, keep, drop = FALSE]
  }
  # `mask` restricts the AVERAGING to a subgroup -- a crossed slope's effect within one level of its
  # moderator. Every quantity below is a weighted mean, so a subgroup is the same sweep under a
  # restricted weight vector: no second engine, and the influence function follows for free.
  function(var, level, ref, mask = NULL) {
    w  <- if (is.null(mask)) w0 else w0 * as.numeric(mask)
    sw <- sum(w)
    if (!isTRUE(sw > 0)) return(NULL)
    X1 <- cf(level, var); X0 <- cf(ref, var)
    if (is.null(X1) || is.null(X0)) return(NULL)
    e1 <- as.vector(X1 %*% bk);  e0 <- as.vector(X0 %*% bk)
    m1 <- fam$linkinv(e1);       m0 <- fam$linkinv(e0)
    d1 <- fam$mu.eta(e1);        d0 <- fam$mu.eta(e0)
    if (!all(is.finite(m1)) || !all(is.finite(m0))) return(NULL)
    M1 <- sum(w * m1) / sw; M0 <- sum(w * m0) / sw
    if (!isTRUE(lf$ok(c(M1, M0)))) return(NULL)
    if (identical(link, "identity")) {
      # written out at the identity link so the AME and its influence keep the exact arithmetic the
      # parity tests pin; it IS the general form below with h' = 1.
      est <- sum(w * (m1 - m0)) / sw
      emp <- w * ((m1 - m0) - est) / sw
      G   <- colSums(w * (X1 * d1 - X0 * d0)) / sw
    } else {
      k1 <- lf$dh(M1); k0 <- lf$dh(M0)
      est <- lf$h(M1) - lf$h(M0)
      emp <- w * (m1 - M1) * k1 / sw - w * (m0 - M0) * k0 / sw
      G   <- colSums(w * X1 * d1) * k1 / sw - colSums(w * X0 * d0) * k0 / sw
    }
    if (!is.finite(est)) return(NULL)
    list(est = est, G = G, emp = emp, mean1 = M1, mean0 = M0)
  }
}

# reg_ame_if_maker() -- the g-computation above wearing its influence-function hat. `g` overrides
# WHICH sweep it wears: any maker with this closure contract (est / G / emp) gets its influence from
# here rather than from a copy of these six lines -- reg_gcomp_rank_maker() is the second one.
#' @keywords internal
reg_ame_if_maker <- function(fit, data, wt, link, coef_if, g = NULL) {
  if (is.null(coef_if)) return(NULL)
  g <- g %||% reg_gcomp_maker(fit, data, wt, link)
  if (is.null(g)) return(NULL)
  function(var, level, ref, mask = NULL) {
    p <- g(var, level, ref, mask)
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
  # ⚠ svyolr IS accepted, and it is NOT a polr subclass: it carries the same (beta, zeta)
  # parameterisation and its vcov() is already the DESIGN-BASED one, so reg_delta_se() needs no
  # branch -- but its coef() returns the THRESHOLDS TOO, which is the one difference. What it does
  # not carry is per-observation scores, so a gap SE is unavailable there, which makes
  # `color = "adjustment"` descriptive under a design rather than falsely significant.
  if (inherits(fit, "polr") || inherits(fit, "svyolr")) {
    b <- tryCatch(stats::coef(fit), error = function(e) NULL)
    z <- fit$zeta
    if (is.null(b) || is.null(z) || !length(z)) return(NULL)
    # ⚠ by CLASS and by NAME, never by length: polr's own coef() is already the betas alone, and a
    # length rule would silently truncate it.
    if (inherits(fit, "svyolr")) b <- b[!names(b) %in% names(z)]
    if (!length(b) || length(b) + length(z) != length(stats::coef(fit)) +
        (if (inherits(fit, "svyolr")) 0L else length(z))) return(NULL)
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

# reg_gcomp_baseline() -- THE model's own predicted outcome at a profile, with the analytic jacobian
# its interval needs. It is what the Constant row holds wherever there is no intercept in the tidy to
# read: `newdata = NULL` averages over the fitted sample (weighted), a one-row frame evaluates at
# that profile. One producer, both contrasts, single-equation and per-category alike.
#
# ⚠ an OFFSET term is refused rather than approximated: model.matrix() emits no column for it, so
# linkinv(X b) would silently drop it.
#' @keywords internal
reg_gcomp_baseline <- function(fit, data, wt = NULL, newdata = NULL) {
  d  <- newdata %||% data
  w  <- if (!is.null(newdata) || is.null(wt)) rep(1, nrow(d)) else as.numeric(data[[wt]])
  if (length(w) != nrow(d) || !all(is.finite(w)) || sum(w) <= 0) return(NULL)
  sw <- sum(w)
  if (inherits(fit, "multinom") || inherits(fit, "polr")) {
    eng <- reg_prob_engine(fit)
    if (is.null(eng)) return(NULL)
    X <- tryCatch(eng$mm(d), error = function(e) NULL)
    P <- if (is.null(X)) NULL else tryCatch(eng$probs(eng$theta, X), error = function(e) NULL)
    if (is.null(P) || !all(is.finite(P))) return(NULL)
    K <- length(eng$levels)
    return(list(levels = eng$levels,
                est = vapply(seq_len(K), function(j) sum(w * P[, j]) / sw, numeric(1)),
                G   = lapply(seq_len(K), function(j) eng$dmean(X, P, j, w) / sw)))
  }
  tt  <- tryCatch(stats::delete.response(stats::terms(fit)), error = function(e) NULL)
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(tt) || is.null(fam) || is.null(fam$linkinv) || is.null(fam$mu.eta)) return(NULL)
  if (length(attr(tt, "offset")) > 0L) return(NULL)
  b    <- stats::coef(fit)
  keep <- !is.na(b)
  X <- tryCatch(stats::model.matrix(tt, d), error = function(e) NULL)
  if (is.null(X) || ncol(X) != length(b)) return(NULL)
  X   <- X[, keep, drop = FALSE]
  eta <- as.vector(X %*% b[keep])
  mu  <- fam$linkinv(eta)
  dd  <- fam$mu.eta(eta)
  if (!all(is.finite(mu)) || !all(is.finite(dd))) return(NULL)
  list(levels = NA_character_, est = sum(w * mu) / sw,
       G = list(colSums(w * X * dd) / sw))
}

# reg_gcomp_cat_maker() -- reg_gcomp_maker()'s twin for a 3+ level outcome: a multinomial / ordinal
# model shows ONE COLUMN PER CATEGORY, so the closure answers for ALL of them at once from the same
# two counterfactual probability matrices.
#' @keywords internal
reg_gcomp_cat_maker <- function(fit, data, wt, link = "identity") {
  eng <- reg_prob_engine(fit)
  lf  <- reg_link_funs(link)
  if (is.null(eng) || is.null(lf)) return(NULL)
  w0 <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (length(w0) != nrow(data) || !all(is.finite(w0))) return(NULL)
  cf <- function(lv, var) {
    d <- reg_counterfactual(data, var, lv)
    if (is.null(d)) return(NULL)
    eng$mm(d)
  }
  function(var, level, ref, mask = NULL) {
    w  <- if (is.null(mask)) w0 else w0 * as.numeric(mask)
    sw <- sum(w)
    if (!isTRUE(sw > 0)) return(NULL)
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
      if (!isTRUE(lf$ok(c(M1[j], M0[j])))) return(NULL)
      if (identical(link, "identity")) {
        est[j]   <- sum(w * (p1 - p0)) / sw
        emp[[j]] <- w * ((p1 - p0) - est[j]) / sw
        G[[j]]   <- (g1 - g0) / sw
      } else {
        k1 <- lf$dh(M1[j]); k0 <- lf$dh(M0[j])
        est[j]   <- lf$h(M1[j]) - lf$h(M0[j])
        emp[[j]] <- w * (p1 - M1[j]) * k1 / sw - w * (p0 - M0[j]) * k0 / sw
        G[[j]]   <- g1 * k1 / sw - g0 * k0 / sw
      }
      if (!is.finite(est[j])) return(NULL)
    }
    list(levels = eng$levels, est = est, G = G, emp = emp, mean1 = M1, mean0 = M0)
  }
}

# reg_rank_pair() -- THE SUPERIORITY PAIR, and the ONE piece of arithmetic the model column and its
# crude twin share. Two distributions over the same ordered categories in, one comparison out:
#
#     win = sum_k p1_k P(Y0 < k)    loss = sum_k p0_k P(Y1 < k)    gamma = win + sum_k p1_k p0_k / 2
#
# It returns the two READINGS of that pair (`est` on the asked link, `alt` on the other) and, with
# them, the GRADIENT of `est` in each distribution -- which is all either side needs for a variance:
# the model side pushes a1/a0 through the fit's own jacobians, the crude side through the multinomial
# covariance of two independent samples. At `est = gamma` the gradients are the placement values, so
# the crude variance IS DeLong's, arrived at rather than special-cased.
#
#     a1 = d est / d p1     a0 = d est / d p0
#     dwin/dp1 = P(Y0 < k)  dwin/dp0 = P(Y1 > k)  dloss/dp0 = P(Y1 < k)  dloss/dp1 = P(Y0 > k)
#' @keywords internal
reg_rank_pair <- function(p1, p0, link = "identity") {
  K <- length(p1)
  if (K < 2L || length(p0) != K) return(NULL)
  c1 <- cumsum(p1);      c0 <- cumsum(p0)
  lt1 <- c(0, c1[-K]);   lt0 <- c(0, c0[-K])            # P(Y < k)
  win <- sum(p1 * lt0);  loss <- sum(p0 * lt1)
  if (!is.finite(win) || !is.finite(loss)) return(NULL)
  mult <- identical(link, "log")
  if (mult && !(win > 0 && loss > 0)) return(NULL)
  est <- if (mult) log(win) - log(loss) else win - loss
  if (!is.finite(est)) return(NULL)
  list(win = win, loss = loss, gamma = win + sum(p1 * p0) / 2, est = est,
       a1 = if (mult) lt0 / win - (1 - c0) / loss else lt0 - (1 - c0),
       a0 = if (mult) (1 - c1) / win - lt1 / loss else (1 - c1) - lt1,
       alt = if (mult) win - loss else win / loss)
}

# reg_rank_se() -- the crude side's variance of that comparison: two INDEPENDENT multinomial samples,
# so a1' (diag(p) - p p') a1 / n on each, which is just the sampling variance of the gradient read as
# a score over the categories. Verified against a 2 000-draw bootstrap.
#' @keywords internal
reg_rank_se <- function(pr, p1, p0, n1, n0) {
  v <- function(a, p, n) {
    if (!isTRUE(n > 0)) return(NA_real_)
    max(sum(a * a * p) - sum(a * p)^2, 0) / n
  }
  sqrt(v(pr$a1, p1, n1) + v(pr$a0, p0, n0))
}

# reg_crude_rank_if_maker() -- the CRUDE leg of a rank gap, and reg_crude_if_maker()'s twin: where
# that one averages a 0/1 indicator of one category, this one reads the gradient of the whole pair as
# a SCORE over the categories -- a_g[y_i] centred on its own mean, which is the same object whose
# sampling variance reg_rank_se() takes.
#
# ⚠ Both legs of the gap must be built from the same pair, so the link is the REPORTED one and the
# distributions are the observed cell shares of the two groups -- exactly what the crude column shows.
#' @keywords internal
reg_crude_rank_if_maker <- function(data, outcome, wt, link) {
  yw <- tryCatch(reg_crude_yw(data, outcome, "ordinal", NULL, wt), error = function(e) NULL)
  if (is.null(yw) || !identical(yw$kind, "labels")) return(NULL)
  cats <- yw$cats
  y    <- as.character(yw$y)
  w    <- yw$w
  if (length(y) != nrow(data) || length(w) != nrow(data)) return(NULL)
  idx  <- match(y, cats)
  fin  <- !is.na(idx) & is.finite(w)
  function(var, level, ref) {
    x <- data[[var]]
    if (is.null(x)) return(NULL)
    x <- as.character(x)
    leg <- function(l) {
      m  <- fin & !is.na(x) & x == as.character(l)
      sw <- sum(w[m])
      if (!isTRUE(sw > 0)) return(NULL)
      list(m = m, sw = sw,
           p = vapply(seq_along(cats), function(k) sum(w[m & idx == k]) / sw, numeric(1)))
    }
    a <- leg(level); b <- leg(ref)
    if (is.null(a) || is.null(b)) return(NULL)
    pr <- reg_rank_pair(a$p, b$p, link)
    if (is.null(pr)) return(NULL)
    score <- function(g, av) {
      v <- numeric(length(y))
      v[g$m] <- w[g$m] * (av[idx[g$m]] - sum(av * g$p)) / g$sw
      v
    }
    score(a, pr$a1) + score(b, pr$a0)
  }
}

# reg_gcomp_rank_maker() -- THE ORDINAL SUPERIORITY PAIR, and a DROP-IN for reg_gcomp_maker(): one
# value per (var, level) and the same closure contract, so reg_marginal_gcomp(), reg_marginal_column()
# and reg_wald_finalize() carry no arm for it.
#
# Of two people drawn independently from the sample, one forced to `level` and one to `ref`:
#
#     win = P(Y1 > Y0)     loss = P(Y1 < Y0)     gamma = win + P(Y1 == Y0) / 2
#
# DESIGN: the pairwise double sum over PEOPLE factorises exactly into the two standardised
# (g-computed) marginal distributions -- verified against the brute-force average -- so the whole
# measure is O(K) arithmetic on the very column means the per-category sweep already forms. That
# factorisation is also what makes it MARGINAL rather than matched, hence collapsible, hence worth
# testing against the crude twin (see REG_WORDS$D).
#
#     est   = win - loss (identity: Somers' D)  |  log(win / loss) (log: the win ratio)
#     mean1 = gamma, mean0 = 1/2 -- so `{base}` reads the probability of superiority and the
#             reference row's own base is a coin flip, exactly.
#     alt   = the OTHER reading, which is a primitive of the same pair rather than something
#             derivable from (mean1, mean0). reg_fill_base() prefers it over its own derivation.
#' @keywords internal
reg_gcomp_rank_maker <- function(fit, data, wt, link = "identity") {
  eng <- reg_prob_engine(fit)
  # only the two readings of the pair: a logit contrast of win/loss names no measure, and the
  # estimand library composes no such row.
  if (is.null(eng) || !link %in% c("identity", "log")) return(NULL)
  w0 <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (length(w0) != nrow(data) || !all(is.finite(w0))) return(NULL)
  cf <- function(lv, var) {
    d <- reg_counterfactual(data, var, lv)
    if (is.null(d)) return(NULL)
    eng$mm(d)
  }
  function(var, level, ref, mask = NULL) {
    w  <- if (is.null(mask)) w0 else w0 * as.numeric(mask)
    sw <- sum(w)
    if (!isTRUE(sw > 0)) return(NULL)
    X1 <- cf(level, var); X0 <- cf(ref, var)
    if (is.null(X1) || is.null(X0)) return(NULL)
    P1 <- tryCatch(eng$probs(eng$theta, X1), error = function(e) NULL)
    P0 <- tryCatch(eng$probs(eng$theta, X0), error = function(e) NULL)
    if (is.null(P1) || is.null(P0) || !all(is.finite(P1)) || !all(is.finite(P0))) return(NULL)
    K  <- length(eng$levels)
    p1 <- as.vector(crossprod(P1, w)) / sw           # the two STANDARDISED distributions
    p0 <- as.vector(crossprod(P0, w)) / sw
    pr <- reg_rank_pair(p1, p0, link)
    if (is.null(pr)) return(NULL)
    # the chain rule over the per-category jacobians the engine already computes: d est / d theta =
    # sum_k a1_k dp1_k/dtheta + a0_k dp0_k/dtheta.
    G <- 0
    for (j in seq_len(K))
      G <- G + pr$a1[[j]] * eng$dmean(X1, P1, j, w) + pr$a0[[j]] * eng$dmean(X0, P0, j, w)
    emp <- w * ((as.vector(P1 %*% pr$a1) - sum(p1 * pr$a1)) +
                (as.vector(P0 %*% pr$a0) - sum(p0 * pr$a0))) / sw
    list(est = pr$est, G = G / sw, emp = emp, mean1 = pr$gamma, mean0 = 0.5, alt = pr$alt)
  }
}

# reg_ame_if_cat_maker() -- the marginal influence function for a 3+ level outcome, ONE category at a
# time: the g-computation above plus the score-based coefficient influence, same shape as
# reg_ame_if_maker().
#' @keywords internal
reg_ame_if_cat_maker <- function(fit, data, wt, link, category) {
  eng <- reg_prob_engine(fit)
  sc  <- if (inherits(fit, "multinom")) reg_score_multinom(fit) else reg_score_polr(fit)
  if (is.null(eng) || is.null(sc)) return(NULL)
  cif <- reg_if_from_score(sc$S, sc$bread)
  if (is.null(cif)) return(NULL)
  j <- match(as.character(category), eng$levels)
  if (is.na(j)) return(NULL)
  g <- reg_gcomp_cat_maker(fit, data, wt, link)
  if (is.null(g)) return(NULL)
  function(var, level, ref, mask = NULL) {
    p <- g(var, level, ref, mask)
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
