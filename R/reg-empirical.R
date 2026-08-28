# PURPOSE: THE OBSERVED (crude) COMPANION of a model effect, and the standard error of the gap
#   between the two.
# ROLE: `tab_reg(empirical =)`'s producer. Beside every modelled effect it puts the SAME estimand
#   computed with one predictor instead of all of them, so "what did adjustment change" is read
#   across the table. The gap is what `color = "adjustment"` grades and what `{obs}` / `{gap}` print.
# KEY CONSTRAINTS:
#   - COMPUTED BY DEFAULT, DRAWN ON REQUEST. `empirical` is TRUE by default and resolves to ONE of
#     four modes (reg_emp_mode, R/reg-resolve.R): `no` / `tooltip` / `cell` / `column`. Only
#     `column` draws a column here -- `cell` is the `est_obs` display preset and `tooltip` prints
#     nothing at all -- but all three still compute `obs` and `gap_se`, which is what
#     `color = "adjustment"`, forest_plot() and the hover read. So the mode is a LAYOUT decision:
#     nothing in this file's arithmetic branches on it.
#   - ONE COLUMN SHAPE, BUILT TWICE. The crude and the modelled effect are the same estimand, so the
#     crude column is the model column's mirror: same stored scale, same colour measure (both
#     channels), same display, same digits, same reference. Only the estimation differs. It carries
#     exactly one interval -- its effect's -- and the level it sits on rides in the same cell, in the
#     field its scale names (`{base}`), with the additive and multiplicative readings of that level
#     PAIR beside it (with_base(), the twin of reg_fill_geometries()). It must never carry `obs` or
#     `gap_se`: it IS the observed value, and a column cannot be its own baseline.
#   - SAME ESTIMAND, SAME PEOPLE, or nothing. reg_same_estimand() (the scale AND the measure word --
#     every logged measure shares the one `log_coef` scale) / reg_same_frame() withhold the crude
#     value rather than let a "gap" mean listwise deletion instead of confounding.
#   - A SHAPE'S DECLARATION IS THE ARITHMETIC. Which closed form runs is read off the shape's own
#     scale geometry and `ci_method`, never off the family in hand: a shape is routinely BORROWED
#     across blocks (a binary marginal ratio's crude twin is REG_EMPIRICAL$rr$rr), so the two can
#     differ. The borrow is COMPOSED, not declared: REG_EMP_BY_LINK indexes this table by
#     (block, link, logged) and the estimand library picks each row's crude shape through it, which
#     is what makes "a model row and its observed twin state one estimand" true by construction.
#     And a shape a block does not declare means NO crude column, never another estimand's -- every
#     reachable key is a foreign key checked at load (R/zzz-fact-keys.R).
#   - TWO SOURCES, ONE SHAPE. A CLOSED FORM off reg_empirical()'s per-(var, level, category) grid
#     wherever the univariable model is saturated AND the closed form's own assumption holds
#     (reg_crude_saturated) -- there the crude odds ratio IS the Woolf 2x2 ratio. Otherwise a
#     univariable reg_fit() through the very fitter the table came from (a shape declaring `refit` --
#     the cumulative odds ratio, whose proportional-odds constraint means a one-factor polr is not
#     the cell table; every numeric predictor, every marginal shape, a NESTED interaction block whose
#     univariable model is `y ~ M/X`, and every factor predictor under a structured survey design),
#     so "same estimand, link, CI rule, multiplier" holds by construction. A COMBINED-factor
#     interaction needs neither route of its own: it is a factor, so it takes the closed form, and
#     there the closed form IS the observed cell table.
#   - The crude interval is the univariable MODEL's, under the table's own inference basis: pooled /
#     model-based unweighted, the sandwich weighted. See REG_EMPIRICAL's `ci_method_design`.
#     ⚠ EVERY closed form here assumes the two compared groups are INDEPENDENT (Woolf's
#     1/a+1/b+1/c+1/d, and its moment twins). Weights alone do not break that -- a flat design is a
#     weight vector -- but clusters, strata and calibration do: the groups share PSUs, so the formula
#     drops a covariance whose SIGN nothing in the number reveals. That is why a structured design
#     refits instead, and why a flat one does not.
#   - WEIGHTED means weighted estimates on unweighted counts, with the effective n carried apart --
#     the package's rule everywhere. Here the basis is FORCED weighted: these columns must be
#     measured like the model column beside them.
#
# THE FACT TABLE: `REG_EMPIRICAL` -- per family, the shape of each crude effect column. A family is a
# row, never a switch arm; its keys are foreign-key checked in R/zzz-fact-keys.R. Which shape a
# column takes is reg_crude_shape()'s answer alone.
#
# THE SPINE: reg_empirical() builds the grid -> reg_empirical_fit() supplies the rows no closed form
# covers -> reg_empirical_columns() emits one fmt column per crude effect -> reg_gap_se_columns()
# scores the distance to the model column (the maths is R/reg-influence.R's; this is the gate and
# the loop). The stage driving them is reg_stage_crude() (R/tab_reg.R).
#
# ⚠ A CRUDE FIT IS A CACHEABLE FIT. reg_empirical_fit() goes through reg_fit_cached()
# (R/reg-digest.R), the same seam the model path uses, on the same store and the same tier -- so a
# served record carries a DIGEST and no fitted object, and every consumer here reads reg_model_of(),
# never `$fit`. The one thing a digest cannot serve is the marginaleffects fallback, which buys its
# fit back through the `refit` callback. Its key is a synthetic one-predictor spec whose
# `drop_extra` -- the whole predictor set minus this one -- is a key member, because it is what
# lands each crude fit on the model's own complete cases.
#
# WARNING: this file sorts BEFORE R/tab_reg.R, so its top-level code -- the REG_EMPIRICAL grid, which
# tx_grid() folds at SOURCE time -- may not read anything defined there. Every cross-file call below
# is made at RUN time, which is why the split is free.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).

# === empirical: the descriptive crude companion beside the model effect =========================

# The outcome on the scale the crude estimator averages, mirroring reg_prep_binary()'s recode (the
# raw `data` has not been through it) so as.character(0/1) matches the label, not the raw code.
# Shared by reg_empirical()'s cell means and reg_crude_if_maker()'s residuals.
#' @keywords internal
reg_crude_y <- function(data, outcome, family, positive_level) {
  yv <- data[[outcome]]
  if (!reg_fam_binary(family)) return(as.numeric(yv))
  if (is.numeric(yv) && all(stats::na.omit(yv) %in% c(0, 1)))
    yv <- factor(yv, levels = c(0, 1), labels = c(paste0("Not ", outcome), outcome))
  as.numeric(as.character(yv) == positive_level)
}

# reg_crude_yw() -- generalises reg_crude_y(): what the crude estimator averages, and with what
# weights, so the crude GRID and the crude INFLUENCE FUNCTION are built around the same data.
#
#   $y    the per-observation outcome on the crude scale (a 0/1 indicator, a category label, a number)
#   $w    the weights the crude estimator averages with
#   $cats the outcome categories the grid produces a row for ("" = a numeric outcome, no categories)
#   $ref  the category the ODDS are conditional on
#   $num  the numeric outcome behind the mean/variance part (NULL = none), with $num_w its weights
#' @keywords internal
reg_crude_yw <- function(data, outcome, crude_key, positive_level = NULL, wt = NULL,
                         trials = NULL, ref_category = NULL) {
  w <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (identical(crude_key, "grouped_binomial")) {
    # `trials` is the resolved COUNT of Bernoulli draws per row (tab_reg turns TRUE / a named vector
    # into an integer before the specs are built), never a column name.
    s  <- as.numeric(data[[outcome]])
    tr <- rep_len(as.numeric(trials), length(s))
    return(list(y = s / tr, w = w * tr, kind = "share", cats = c("1", "0"), ref = "0",
                draws = tr, num = s, num_w = w))
  }
  if (identical(crude_key, "multinomial") || identical(crude_key, "ordinal")) {
    yv   <- forcats::fct_drop(as.factor(data[[outcome]]))
    cats <- levels(yv)
    return(list(y = as.character(yv), w = w, kind = "labels", cats = cats,
                ref = if (!is.null(ref_category) && ref_category %in% cats) ref_category else cats[1],
                draws = rep(1, nrow(data)), num = NULL, num_w = NULL))
  }
  if (identical(crude_key, "binomial") || identical(crude_key, "rr")) {
    # 0/1, so the "share" arithmetic below reduces to the indicator sums the binary arm always used.
    return(list(y = reg_crude_y(data, outcome, "binomial", positive_level), w = w, kind = "share",
                cats = c("1", "0"), ref = "0", draws = rep(1, nrow(data)), num = NULL, num_w = NULL))
  }
  yn <- as.numeric(data[[outcome]])
  list(y = yn, w = w, kind = "numeric", cats = "", ref = NA_character_,
       draws = rep(1, nrow(data)), num = yn, num_w = w)
}

# reg_level_counts() -- the N behind each predictor level, aligned to the skeleton; family-free,
# agreeing with reg_empirical()'s own `emp_n`. NA on a numeric predictor's row is deliberate: on a
# listwise-complete frame its count IS the model N. The BASELINE row is left to reg_constant_count(),
# which needs the contrast to know what its base even is.
#' @keywords internal
reg_level_counts <- function(frame, skeleton, wt = NULL, crosses = list()) {
  n  <- rep(NA_integer_, nrow(skeleton))
  wn <- rep(NA_real_,    nrow(skeleton))
  w  <- if (!is.null(wt) && wt %in% names(frame)) as.numeric(frame[[wt]]) else NULL
  for (v in setdiff(unique(skeleton$var), "Constant")) {
    # a nested cross block's rows are its MODERATOR's levels -- the count a continuous predictor
    # never had, and what a crossed slope is read with.
    rec <- reg_cross_of(crosses, v)
    cv  <- if (is.null(rec)) v else reg_cross_count_var(rec)
    if (!cv %in% names(frame) || !reg_is_factor_var(frame[[cv]])) next
    lv  <- as.character(frame[[cv]])
    idx <- which(skeleton$var == v)
    m   <- match(as.character(skeleton$level)[idx], lv)   # a level absent from the frame stays NA
    cnt <- tapply(rep(1L, length(lv)), lv, sum)
    n[idx] <- as.integer(cnt[as.character(skeleton$level)[idx]])
    n[idx][is.na(m)] <- NA_integer_
    if (!is.null(w)) {
      wcnt <- tapply(w, lv, sum, na.rm = TRUE)
      wn[idx] <- as.numeric(wcnt[as.character(skeleton$level)[idx]])
    }
  }
  list(n = n, wn = if (is.null(w)) rep(NA_real_, nrow(skeleton)) else wn)
}

# The zero-row shape of reg_empirical()'s long tibble, so the empty case cannot drift from the rest.
#' @keywords internal
reg_empirical_empty <- function()
  tibble::tibble(
    var = character(0), level = character(0), category = character(0),
    emp_prop = numeric(0), emp_prop_inf = numeric(0), emp_prop_sup = numeric(0),
    emp_diff = numeric(0), emp_diff_inf = numeric(0), emp_diff_sup = numeric(0),
    emp_ratio = numeric(0), emp_ratio_prop = numeric(0),
    emp_pivot_prop = numeric(0), emp_ref_pivot_prop = numeric(0),
    emp_mean = numeric(0), emp_var = numeric(0),
    emp_n = integer(0), emp_n_ci = numeric(0), emp_n_draw = numeric(0),
    emp_ref_n_draw = numeric(0),
    emp_ref_prop = numeric(0), emp_ref_mean = numeric(0), emp_ref_var = numeric(0),
    emp_ref_n = integer(0), emp_ref_n_ci = numeric(0)
  )

# reg_empirical() -- THE crude grid, keyed (var, level, category): `emp_prop` (+ Wilson interval)
# and its reference-level difference (+ Newcombe); the odds ratio `emp_ratio` and the risk ratio
# `emp_ratio_prop`; the weighted mean/variance (tab()'s own formula) for a NUMERIC predictor.
# `n_ci`/`n_draw` carry the SEPARATE effective n the basis computes; unweighted they equal `n`,
# byte-identical.
#
# THE PIVOT is the outcome category an odds is taken AGAINST -- the complement on a binary outcome,
# the reference category on a multinomial one. `emp_pivot_prop` is its share within the row's own
# level and `emp_ref_pivot_prop` its share in the reference level, so the odds-ratio 2x2 is four
# shares of the same kind, whatever the outcome has categories.
# WARNING: `emp_ratio` is the odds against the PIVOT, never "everything else" -- they coincide for a
# binary outcome but not a multinomial, where the {j, ref} form is nnet::multinom's own estimand.
reg_empirical <- function(data, fac_preds, outcome, crude_key, positive_level, wt,
                          trials = NULL, ref_category = NULL, conf_level = 0.95,
                          design_spec = NULL) {
  yw   <- reg_crude_yw(data, outcome, crude_key, positive_level, wt, trials, ref_category)
  cats <- yw$cats
  basis <- svy_inference_basis(design_spec, wt, force = TRUE)
  weighted <- identical(basis, "weights") || identical(basis, "design")   # the effective-n base
  # is the flat design's own closed form, never Kish; unweighted -> the raw count.
  n_obs    <- nrow(data)
  # the model columns already refer to t(degf), so the crude bracket must too; `Inf` is a no-op.
  degf     <- design_spec$degf %||% Inf
  w0       <- if (identical(yw$kind, "share")) yw$w / yw$draws else yw$w   # per-RESPONDENT weight
  flat_neff <- function(keep, u, v, raw, num = NULL) {
    if (!weighted) return(as.double(raw))
    ne <- svy_flat_neff_rows(w0[keep], u[keep], v[keep], n_obs, num = num)
    if (isTRUE(is.finite(ne) && ne > 0)) ne else as.double(raw)
  }
  has_num <- !is.null(yw$num)
  has_cat <- !identical(yw$kind, "numeric")
  share   <- identical(yw$kind, "share")
  emp_method_diff <- REG_EMPIRICAL[[crude_key]]$method_diff %||% "wald"   # the FAMILY's declared one
  want_var <- has_num
  # a TYPED zero-row return: purrr::map_dfr over character(0) yields a 0x0 tibble whose columns are
  # NULL, and reg_empirical_columns() then errors on the recycle.
  if (length(fac_preds) == 0L) return(reg_empirical_empty())

  # --- the design-based effective n: R/survey-variance.R's variance for a weighted-mean domain.
  # A LOCAL latch: the degrade reason travels OUT on the grid (attr "degrade").
  said <- FALSE
  degrade <- function(reason = NULL) {
    if (!said) { svy_var_degraded(reason); said <<- TRUE }
    NULL
  }
  # a FLAT svydesign(ids = ~1) has the closed form as its exact answer -- no influence matrix.
  need_svy <- svy_design_structured(design_spec$design)
  prep <- if (need_svy) svy_var_prep(design_spec$design, data[[svy_row_col]]) else NULL
  if (need_svy && is.null(prep)) degrade()
  if (!is.null(prep)) {
    # the grid's own weights must BE the design's, or estimate and variance describe two populations.
    wg <- prep$w[prep$at] * yw$draws
    if (length(wg) != length(yw$w) || anyNA(wg) ||
        !isTRUE(max(abs(wg - yw$w)) <= 1e-8 * max(1, max(abs(yw$w))))) { degrade(); prep <- NULL }
  }
  # `$p` an nl x nc matrix (share per outcome category), `$m` an nl x 1 (the numeric mean).
  design_var <- function(x, ok, lv) {
    if (is.null(prep) || !length(lv)) return(NULL)
    keys  <- list(as.character(seq_along(lv)))
    mkeys <- list(as.character(match(as.character(x), lv)))
    hide  <- function(v) ifelse(ok, as.numeric(v), NA_real_)
    xs_p  <- if (share) list(hide(yw$y))
             else lapply(stats::setNames(nm = cats), function(k) hide(as.character(yw$y) == k))
    rp <- if (has_cat) svy_var_mean(prep, keys, 0L, mkeys, xs_p, wmult = yw$draws) else NULL
    rm <- if (has_num) svy_var_mean(prep, keys, 0L, mkeys, list(hide(yw$num)))    else NULL
    if ((has_cat && is.null(rp$v)) || (has_num && is.null(rm$v)))
      return(degrade(rp$reason %||% rm$reason))
    list(p = rp$v, m = rm$v)
  }

  out <- purrr::map_dfr(fac_preds, function(p) {
    x  <- data[[p]]
    ok <- !is.na(x) & !is.na(yw$w) & !is.na(yw$y)
    if (has_num) ok <- ok & !is.na(yw$num)
    lv <- levels(forcats::fct_drop(as.factor(x[ok])))
    dv <- design_var(x, ok, lv)
    per <- purrr::map(seq_along(lv), function(i) {
      l  <- lv[[i]]
      m  <- ok & x == l
      wl <- sum(yw$w[m])
      # "share": Sum(w*y)/Sum(w*(1-y)). "labels": one indicator per outcome category.
      wc <- if (!has_cat) NA_real_
            else if (share) stats::setNames(c(sum(yw$w[m] * yw$y[m]), sum(yw$w[m] * (1 - yw$y[m]))),
                                            cats)
            else vapply(cats, function(k) sum(yw$w[m & yw$y == k]), numeric(1))
      # the CI base of a PROPORTION is its own flat-design effective n, per CATEGORY.
      draw_ne <- if (!has_cat) NA_real_ else vapply(cats, function(k) {
        u <- if (share) (if (identical(k, cats[[1]])) yw$y else 1 - yw$y) * yw$draws
             else as.numeric(yw$y == k)
        flat_neff(m, u, yw$draws, sum(m) * mean(yw$draws[m]))
      }, numeric(1))
      out <- list(
        n     = sum(m),
        n_ci  = flat_neff(m, yw$draws, yw$draws, sum(m)),
        n_draw = unname(draw_ne),
        prop  = if (has_cat) wc / wl else NA_real_,
        wpos  = if (has_cat) wc else NA_real_,
        wneg  = if (has_cat) rep(unname(wc[yw$ref]), length(cats)) else NA_real_,
        # the PIVOT's share, in the same unit as `prop`: what the odds is taken against.
        pivot = if (has_cat) rep(unname(wc[yw$ref]) / wl, length(cats)) else NA_real_,
        mean  = NA_real_, var = NA_real_
      )
      if (has_num) {
        nw <- yw$num_w; n1 <- sum(m); wn <- sum(nw[m])
        s1 <- sum(nw[m] * yw$num[m]); s2 <- sum(nw[m] * yw$num[m]^2)
        out$mean <- s1 / wn
        # matches tab(): unweighted -> stats::var (n-1), weighted -> ML (s2/wn - mean^2)
        out$var  <- if (want_var) {
          if (is.null(wt)) (s2 - s1^2 / n1) / (n1 - 1) else round(s2 / wn - (s1 / wn)^2, 10)
        } else NA_real_
        # the mean twin of the same closed form: s^2 / Var_design(x_bar).
        out$n_ci <- if (!weighted) as.double(n1) else {
          ne <- svy_flat_neff_rows(nw[m], yw$num[m], rep(1, sum(m)), n_obs, num = out$var)
          if (isTRUE(is.finite(ne) && ne > 0)) ne else as.double(n1)
        }
      }
      # the design supersedes the flat base with Korn & Graubard's device.
      if (!is.null(dv)) {
        if (has_cat && !is.null(dv$p)) {
          nd <- out$prop * (1 - out$prop) / dv$p[i, ]
          out$n_draw <- ifelse(is.finite(nd) & nd > 0, nd, out$n_draw)
        }
        if (has_num && !is.null(dv$m)) {
          nc <- out$var / dv$m[i, 1L]
          if (isTRUE(is.finite(nc) && nc > 0)) out$n_ci <- nc
        }
      }
      # a numeric outcome has one base; a categorical one without a mean column likewise.
      if (!has_cat)     out$n_draw <- rep(out$n_ci, length(cats))
      else if (!has_num) out$n_ci  <- out$n_draw[[1]]
      out
    })
    ref  <- per[[1]]                              # the reference LEVEL is always the first surviving one
    nc   <- length(cats)
    nl   <- length(lv)
    rep_lv <- function(f) rep(purrr::map_dbl(per, f), each = nc)
    flat   <- function(f) unname(unlist(purrr::map(per, f), use.names = FALSE))
    prop   <- flat("prop"); wpos <- flat("wpos"); wneg <- flat("wneg")
    pivot  <- flat("pivot")
    rprop  <- rep(unname(ref$prop),  times = nl)
    rpivot <- rep(unname(ref$pivot), times = nl)
    meanv  <- rep_lv("mean"); rmean <- rep(ref$mean, nl * nc)
    n_ci   <- rep_lv("n_ci"); r_n_ci <- rep(ref$n_ci, nl * nc)
    # n_draw is per (level, CATEGORY) -- flat(), not rep_lv() -- so a design variance is not averaged.
    n_draw <- flat("n_draw"); r_n_draw <- rep(ref$n_draw, times = nl)
    # WARNING: the divisor is the ref LEVEL's own wpos/wneg -- NOT `ref$prop / ref$prop[ref_cat]`.
    emp_ratio <- if (has_cat) {
      (wpos / wneg) / rep(unname(ref$wpos / ref$wneg), times = nl)
    } else meanv / rmean
    pw <- if (has_cat) ci_wilson(prop, n_draw, conf_level = conf_level, df = degf) else
      list(inf = rep(NA_real_, nl * nc), sup = rep(NA_real_, nl * nc))
    # the family's DECLARED difference method, not tab(ci = "diff")'s Newcombe.
    dd <- if (has_cat) ci_prop_diff(prop, n_draw, rprop, r_n_draw, conf_level = conf_level,
                                    method = emp_method_diff, want_p = FALSE, df = degf) else pw
    tibble::tibble(
      var = p, level = rep(lv, each = nc), category = rep(cats, times = nl),
      emp_prop = prop, emp_prop_inf = pw$inf, emp_prop_sup = pw$sup,
      emp_diff = if (has_cat) prop - rprop else meanv - rmean,
      emp_diff_inf = dd$inf, emp_diff_sup = dd$sup,
      emp_ratio = emp_ratio, emp_ratio_prop = if (has_cat) prop / rprop else NA_real_,
      emp_pivot_prop = pivot, emp_ref_pivot_prop = rpivot,
      emp_mean = meanv, emp_var = rep_lv("var"),
      emp_n    = as.integer(rep_lv("n")), emp_n_ci = n_ci, emp_n_draw = n_draw,
      emp_ref_n_draw = r_n_draw,
      emp_ref_prop = rprop, emp_ref_mean = rmean, emp_ref_var = rep(ref$var, nl * nc),
      emp_ref_n    = as.integer(rep(ref$n, nl * nc)), emp_ref_n_ci = r_n_ci
    )
  })
  # the degrade travels OUT with the grid, so a grid computed for one table cannot mislabel another.
  structure(out, degrade = said)
}

# reg_empirical_fit() -- the crude companion of every predictor whose univariable model is NOT
# saturated (the header's "otherwise" case): a NUMERIC predictor in any family, or ANY predictor
# under an ORDINAL outcome (reg_crude_saturated() states exactly that). `other_preds` become
# reg_fit()'s `drop_extra`, landing each crude fit on EXACTLY the model's complete-case population,
# always on the NATIVE (link) scale.
#
# Returns list(est = <named by outcome category, "" when none> of tibble(row, est, lo, hi, p),
#              fits = <named by predictor> of list(fit, digest, data),
#              degf = the WEAKEST reference these fits used) -- `row` is the SKELETON row index.
# ⚠ EACH FIT GOES THROUGH THE SHARED CACHE SEAM (reg_fit_cached), on the SAME store and the SAME
# tier as a model fit: a crude fit IS a fit record, told apart by its key alone -- a synthetic
# one-predictor spec whose `drop_extra` is a function of the WHOLE predictor set, hence a key member.
# So `$fits[[v]]$fit` may be NULL where the record was served; every consumer reads reg_model_of().
# ⚠ `degf` is one number for a column whose rows come from k different univariable fits, so it is the
# weakest claim among them (the smallest positive df, hence the widest critical value) -- which is
# exactly what `degf`'s own merge rule declares the attribute to mean.
# WARNING: messages are suppressed -- already emitted by the model fit on the same data/method.
# IS THE UNIVARIABLE MODEL'S INTERVAL AVAILABLE IN CLOSED FORM? Two things must hold, and the second
# is why a design object is an argument here.
#   * the univariable model must be SATURATED -- true of every factor predictor except where the
#     SHAPE declares `refit` (the cumulative odds ratio: proportional odds is a constraint, so a
#     one-factor polr is not the cell table). A numeric predictor is never saturated.
#   * the closed form's own assumption must be true. Woolf / Katz / the moment engines all read
#     `1/a + 1/b + 1/c + 1/d` style variances, which assume the two compared groups are INDEPENDENT.
#     Weights alone do not break that (a flat design is a weight vector); clusters, strata and
#     calibration do -- the groups share PSUs, so the closed form drops a covariance that can make it
#     28 % too narrow or twice too wide, with nothing in the number to say which.
# Where either fails the crude column is REFIT through reg_empirical_fit(), i.e. through the very
# fitter the table came from, which is what D22 asks for in the first place.
#' @keywords internal
reg_crude_saturated <- function(crude_key, is_factor, design = NULL, shape = NULL)
  isTRUE(is_factor) && !isTRUE((shape %||% list())$refit) && !svy_design_structured(design)

#' @keywords internal
reg_empirical_fit <- function(data, preds, outcome, family, design_spec, outcome_level,
                              conf_level, method, skeleton, multiplier = NULL,
                              other_preds = character(0), est = NULL, wt = NULL,
                              want_fit = FALSE, marginal = FALSE, trials = NULL,
                              shape_terms = NULL, crosses = list(), fit_cache = NULL) {
  if (length(preds) == 0L) return(list(est = list(), fits = list(), degf = NA_real_))
  mlink  <- if (is.null(est)) "identity" else est$measure_link %||% "identity"
  skey   <- reg_skel_key(skeleton$var, skeleton$level)
  rows   <- list()
  fits   <- list()
  dfs    <- numeric(0)
  cacheable <- reg_crude_cacheable(method)
  for (v in preds) {
    # a nested cross block's univariable model is `y ~ M/X` -- the moderator plus the crossed term,
    # through this same producer, so estimand, link and CI rule are shared by construction.
    rec <- reg_cross_of(crosses, v)
    fp  <- if (is.null(rec)) v else rec$moderator
    add <- if (is.null(rec)) reg_shape_add(shape_terms, v) else rec$term
    drop_v <- setdiff(other_preds, c(v, fp))
    # the crude fit takes the SAME shape as the model's (`add_terms`), so term names match.
    thunk <- function()
      suppressMessages(reg_fit(data, outcome, fp, family, design_spec, do_exp = FALSE,
                               outcome_level, conf_level, method,
                               trials = trials, formula = NULL, multiplier = multiplier,
                               drop_extra = drop_v, add_terms = add))
    # ⚠ `"crude"` leads `extra` so a univariable model can never collide with a one-predictor MODEL
    # spec's key; `est` / `marginal` / `multiplier` are absent from it, the whole point being that
    # the estimand AND the scaling are answered from the record.
    key <- if (cacheable)
      jmvreg_fit_key(list(outcome = outcome, predictors = fp, trials = trials,
                          outcome_level = outcome_level, formula = NULL),
                     data, family, design_spec,
                     extra = list("crude", method, add),
                     drop_extra = drop_v) else NULL
    f <- tryCatch(reg_fit_cached(fit_cache, key, thunk, data, FALSE, conf_level, multiplier),
                  error = function(e) NULL)
    if (is.null(f)) next
    # ⚠ the DIGEST rides along: it is what reg_coef_if_maker() reads, and only it carries the
    # recipe that names this fit's sampling weights.
    if (want_fit) fits[[v]] <- list(fit = f$fit, digest = f$digest, data = f$data)
    # a MARGINAL crude row is always Wald; a coefficient one follows the table's own `method`.
    dfs <- c(dfs, reg_wald_degf(if (marginal) "wald" else method, f$disp_known, f$df_residual))
    if (!marginal) {
      # align the univariable fit's terms to the skeleton exactly as the model column does.
      td <- f$tidy[!is.na(f$tidy$term) & f$tidy$term %in% skeleton$term[skeleton$var == v], ,
                   drop = FALSE]
      if (!nrow(td)) next
      idx <- match(td$term, skeleton$term)
      # ⚠ a 3+ level fit answers PER OUTCOME CATEGORY, and emit() looks these rows up under that key
      # -- so a multinomial tidy must carry its `y.level` here or every fitted row silently misses.
      rows[[length(rows) + 1L]] <- tibble::tibble(
        category = if ("y.level" %in% names(td)) as.character(td$y.level) else "",
        row = idx, est = td$estimate, lo = td$conf.low, hi = td$conf.high,
        p = td$p.value)
    } else {
      # `at = "average"` always: the crude effect is a whole-sample quantity, like the factor arm's.
      # `exponentiate = FALSE`: this function's contract is the NATIVE (link) scale, and
      # reg_fit_overlay() exp()s back where the shape's scale is multiplicative.
      # ⚠ reg_model_of() + `refit`, exactly as reg_cols_ame() does it: a SERVED record has no fitted
      # object, and marginaleffects -- the fallback engine -- needs one. Without the callback
      # reg_marginal() aborts, and this loop's tryCatch would turn that into a MISSING crude row.
      m <- tryCatch(suppressMessages(reg_marginal(
        reg_model_of(f), f$data, v, conf_level, wt, at = "average",
        link = mlink, comparison = if (is.null(est)) NULL else est$comparison, want_pred = FALSE,
        exponentiate = FALSE, multiplier = multiplier, crosses = crosses,
        # ⚠ the crude sweep must compute the SAME contrast as the model's: a rank estimand asks for
        # the superiority pair here too, or the fallback quietly returns a per-category AME.
        rank = identical((est %||% list())$level, "rank"),
        engine = if (is.null(est)) "marginaleffects" else reg_marginal_engine(est),
        disp_known = f$disp_known, df_residual = f$df_residual,
        refit = function() reg_digest_revive(f, data)$fit)),
        error = function(e) NULL)
      if (is.null(m) || !nrow(m$ame)) next
      a <- m$ame[m$ame$var == v, , drop = FALSE]
      if (!nrow(a)) next
      idx <- match(reg_skel_key(a$var, a$level), skey)
      ok  <- !is.na(idx)
      if (!any(ok)) next
      a <- a[ok, , drop = FALSE]; idx <- idx[ok]
      # ⚠ NOT `est`: that is this function's ESTIMAND-ROW argument, read again on the next predictor.
      e_v <- a$ame; lo <- a$ame_lo; hi <- a$ame_hi
      rows[[length(rows) + 1L]] <- tibble::tibble(
        category = ifelse(is.na(a$group), "", a$group), row = idx,
        est = e_v, lo = lo, hi = hi, p = a$ame_p)
    }
  }
  fit_degf <- { d <- dfs[is.finite(dfs) & dfs > 0]; if (length(d)) min(d) else NA_real_ }
  if (!length(rows)) return(list(est = list(), fits = fits, degf = fit_degf))
  all <- vctrs::vec_rbind(!!!rows)
  list(est = split(all[setdiff(names(all), "category")], all$category), fits = fits,
       degf = fit_degf)
}


# reg_fit_overlay() -- write fit-derived crude rows into the finished crude EFFECT column and
# VECTOR, at the ONE point both are in hand. The estimate lands in the field its `scale` declares,
# exp()d when multiplicative. `n` is left NA: a fit-derived row's base belongs in the footer.
#' @keywords internal
reg_fit_overlay <- function(col, eff, est, shape) {
  if (is.null(est) || !nrow(est)) return(list(col = col, eff = eff))
  idx <- est$row
  e <- est$est; lo <- est$lo; hi <- est$hi; p <- est$p
  scl <- EST_SCALES[[shape$scale]]
  if (isTRUE(scl$mult)) {
    e <- exp(e); lo <- exp(lo); hi <- exp(hi)
  }
  fld <- scl$est_field
  get_est <- switch(fld, "or" = get_or, "ratio" = get_ratio, get_diff)
  set_est <- switch(fld, "or" = set_or, "ratio" = set_ratio, set_diff)
  poke <- function(v, value) { v[idx] <- value; v }
  col <- set_est   (col, poke(get_est   (col), e ))
  col <- set_ci_inf(col, poke(get_ci_inf(col), lo))
  col <- set_ci_sup(col, poke(get_ci_sup(col), hi))
  col <- set_pvalue(col, poke(get_pvalue(col), p ))
  if (!is.null(eff)) eff <- poke(eff, e)
  list(col = col, eff = eff)
}

# REG_EMPIRICAL -- THE CRUDE COMPANION FACT TABLE: per family, the SHAPE of the crude effect column
# that mirrors each model estimand (see the header's "one shape, built twice"). It declares neither a
# colour, a display, nor a "base" row (that rides in `EST_SCALES$<scale>$base_display`).
#
# COLUMNS
#   block       the family, or the link pseudo-family (`rr` / `mr`), the shape belongs to.
#   shape       the shape's own name -- the key a model row's `crude_shape` points at.
#   word        the base measure acronym (a REG_WORDS key) reg_crude_word() composes the column name
#               from -- "Obs_RR", "Obs_log(OR)". The SHAPE's own acronym, never the model column's,
#               and it carries no contrast marker (see reg_crude_word()).
#   scale       the EST_SCALES row, hence the estimate's field, ladder, glyphs and level token.
#   ref         the reference marker ("1" multiplicative, "tot" additive, NA none).
#   ci_method   the interval engine, under the table's `n` basis...
#   ci_method_design  ...and under a weights / design basis: unweighted the fit is lm/glm and the
#               interval is MODEL-BASED (dispersion pooled over the predictor's levels); weighted it
#               is svyglm and the interval is the SANDWICH. NULL = the same interval either way.
#   link        the crude estimator's link, for the gap SE's influence function (g'(mu)).
#   per_category  one crude effect per OUTCOME category (multinomial): rides in the model cell
#               unless `empirical = "column"` asks for the columns.
#   refit       this shape has NO closed form, so its crude estimate comes from a univariable fit
#               through the table's own fitter. Only the cumulative odds ratio declares it:
#               proportional odds is a CONSTRAINT, so a one-factor polr is not the cell table.
#               reg_crude_saturated() reads it, which is why that predicate names no family.
#
# A block's own two scalars sit beside the grid: `coef` names the shape a row with no declared
# `crude_shape` falls back to; `method_diff` the engine of the GRID's own level-vs-reference
# difference -- a descriptive quantity belonging to no estimand, where a crude EFFECT's engine is its
# own shape's `ci_method`.
REG_EMPIRICAL <- local({
  grid <- tibble::tribble(
    ~block,             ~shape,          ~word,   ~scale,             ~ref,          ~ci_method,     ~ci_method_design, ~link,      ~per_category, ~refit,
    "binomial",         "ame",           "RD",    "points",           "tot",         "wald",         NULL,              "identity", NULL,          NULL,
    "binomial",         "or",            "OR",    "odds_ratio",       "1",           "woolf",        NULL,              "logit",    NULL,          NULL,
    "binomial",         "or_log",        "OR",    "log_coef",         NA_character_, "woolf",        NULL,              "logit",    NULL,          NULL,
    # the modified-Poisson companion: the crude RISK ratio, Katz log-RR interval (not the binomial
    # arm's Woolf log-OR).
    "rr",               "ame",           "RD",    "points",           "tot",         "wald",         NULL,              "identity", NULL,          NULL,
    "rr",               "rr",            "RR",    "pct_ratio",        "1",           "katz",         NULL,              "log",      NULL,          NULL,
    "rr",               "rr_log",        "RR",    "log_coef",         NA_character_, "katz",         NULL,              "log",      NULL,          NULL,
    # a RATIO OF MEANS (`measure = "ratio"` on a continuous outcome; the "mr" log-link pseudo-ML fit).
    "mr",               "mr",            "RoM",   "mean_ratio",       "1",           "quasipoisson", "robust",          "log",      NULL,          NULL,
    "mr",               "mr_log",        "RoM",   "log_coef",         NA_character_, "quasipoisson", "robust",          "log",      NULL,          NULL,
    "gaussian",         "diff",          "diff",  "raw_diff",         NA_character_, "ols",          "welch",           "identity", NULL,          NULL,
    # poisson also declares an ADDITIVE shape (a poisson marginal effect is a difference of expected
    # COUNTS): `welch`'s ROBUST interval is the target in both bases, unlike gaussian's `ols`.
    "poisson",          "irr",           "IRR",   "mean_ratio",       "1",           "quasipoisson", "robust",          "log",      NULL,          NULL,
    "poisson",          "irr_log",       "IRR",   "log_coef",         NA_character_, "quasipoisson", "robust",          "log",      NULL,          NULL,
    "poisson",          "diff",          "diff",  "raw_diff",         NA_character_, "welch",        "welch",           "identity", NULL,          NULL,
    # grouped_binomial (`trials =`): still saturated, Woolf 2x2 on the SUMMED counts; its LEVEL is the
    # mean SCORE, hence the two `score_*` scales. Its own `rr`/`rr_log` are the two groups' mean
    # SCORES, not the respondent-level `rr` block -- reg_crude_shape() enforces that precedence.
    "grouped_binomial", "ame",           "RD",    "raw_diff",         NA_character_, "welch",        "welch",           "identity", NULL,          NULL,
    "grouped_binomial", "or",            "OR",    "score_odds_ratio", "1",           "woolf",        NULL,              "logit",    NULL,          NULL,
    "grouped_binomial", "or_log",        "OR",    "log_coef",         NA_character_, "woolf",        NULL,              "logit",    NULL,          NULL,
    "grouped_binomial", "rr",            "RR",    "score_ratio",      "1",           "katz",         NULL,              "log",      NULL,          NULL,
    "grouped_binomial", "rr_log",        "RR",    "log_coef",         NA_character_, "katz",         NULL,              "log",      NULL,          NULL,
    # multinomial: one crude effect PER OUTCOME CATEGORY, the {j, ref} x {level, ref level} Woolf ratio.
    "multinomial",      "or",            "OR",    "odds_ratio",       "1",           "woolf",        NULL,              "logit",    TRUE,          NULL,
    "multinomial",      "or_log",        "OR",    "log_coef",         NA_character_, "woolf",        NULL,              "logit",    TRUE,          NULL,
    "multinomial",      "ame",           "RD",    "points",           "tot",         "wald",         NULL,              "identity", TRUE,          NULL,
    "multinomial",      "ame_ratio",     "RR",    "pct_ratio",        "1",           "katz",         NULL,              "log",      TRUE,          NULL,
    "multinomial",      "ame_ratio_log", "RR",    "log_coef",         NA_character_, "katz",         NULL,              "log",      TRUE,          NULL,
    # ordinal is the one block that MIXES THE TWO ROUTES: the cumulative odds ratio has no closed form
    # and refits, while the three rank shapes are exact arithmetic on two rows of the outcome x
    # predictor table -- which is why reg_crude_saturated() asks the SHAPE, not the family. A
    # cumulative odds ratio has no single share to sit on either, so `{base}` renders void.
    "ordinal",          "cumor",         "cumOR", "odds_ratio",       "1",           "wald_log",     NULL,              "logit",    NULL,          TRUE,
    "ordinal",          "cumor_log",     "cumOR", "log_coef",         NA_character_, "wald_log",     NULL,              "logit",    NULL,          TRUE,
    "ordinal",          "somers_d",      "D",     "points",           "tot",         "wald",         NULL,              "identity", NULL,          NULL,
    "ordinal",          "win_ratio",     "WR",    "pct_ratio",        "1",           "wald_log",     NULL,              "log",      NULL,          NULL,
    "ordinal",          "win_ratio_log", "WR",    "log_coef",         NA_character_, "wald_log",     NULL,              "log",      NULL,          NULL,
  )
  coef        <- c(binomial = "or",   rr = "rr",   mr = "mr", gaussian = "diff",
                   poisson  = "irr",  grouped_binomial = "or", multinomial = "or", ordinal = "cumor")
  method_diff <- c(binomial = "wald", rr = "wald", grouped_binomial = "wald", multinomial = "wald")

  blocks <- unique(grid$block)
  stats::setNames(lapply(blocks, function(b) c(
    if (b %in% names(method_diff)) list(method_diff = unname(method_diff[[b]])),
    list(coef = unname(coef[[b]])),
    tx_grid(grid[grid$block == b, -1L])
  )), blocks)
})

# REG_EMP_BY_LINK -- (block, link, logged) -> the shape name, read off the table's OWN `link` column
# and its `log_coef` scale. The estimand library composes every crude companion through it
# (reg_compose_crude(), R/reg-estimand.R), which is what makes "a model row and its observed twin
# state one estimand" true by construction rather than by two declarations agreeing.
#' @keywords internal
#' @noRd
REG_EMP_BY_LINK <- lapply(REG_EMPIRICAL, function(blk) {
  sh   <- Filter(is.list, blk)
  keys <- vapply(sh, function(s)
    paste0(s$link, if (identical(s$scale, "log_coef")) ".log" else ""), character(1))
  stopifnot("a block declares two shapes on one link" = !anyDuplicated(keys))
  stats::setNames(as.list(names(sh)), keys)
})

#' @keywords internal
#' @noRd
reg_emp_shape_on <- function(block, link, logged = FALSE) {
  if (is.null(block) || length(block) != 1L || is.na(block)) return(NULL)
  idx <- REG_EMP_BY_LINK[[block]]
  k   <- paste0(link, if (isTRUE(logged)) ".log" else "")
  if (is.null(idx) || !k %in% names(idx)) NULL else idx[[k]]
}

#' @keywords internal
shape_per_category <- function(shape) isTRUE(shape$per_category)

# reg_crude_word() / reg_crude_col_name() -- what a crude column is CALLED.
#
# THE CRUDE COLUMN IS NEVER MARKED: it names the measure, and the "Model:" footer says the rest,
# keeping `Obs_*` names stable across `effect =`. The log wrapper is composed from the shape's scale.
#' @keywords internal
reg_crude_word <- function(shape) {
  if (is.null(shape) || is.null(shape$word)) return(NA_character_)
  reg_word_logged(shape$word, if (identical(shape$scale, "log_coef")) "raw_coefficient" else "")
}

#' @keywords internal
reg_crude_col_name <- function(shape) {
  w <- reg_crude_word(shape)
  if (is.na(w)) NA_character_ else paste0("Obs_", w)
}

# reg_crude_shape() -- THE reader of "which REG_EMPIRICAL row describes the crude EFFECT of this
# estimand?". The SELECTION is the estimand row's own `crude_fam`/`crude_shape` (a cross-family
# borrow is DECLARED, not inferred here). ⚠ IT IS THE ONLY READER: re-deriving a shape from
# (marginal, do_exp) can draw a mean-DIFFERENCE crude column beside a ratio model column.
# A shape a block does not declare returns NULL -- no crude column -- never that block's coefficient
# shape, which would print another estimand under this one's name. Every reachable key is checked at
# load (R/zzz-fact-keys.R), so the NULL is a statement of intent rather than a reachable path.
#' @keywords internal
# WHICH BLOCK a run-time crude key and an estimand row land on. ⚠ the SUMMED-SCORE block wins over
# the estimand's borrow (load-bearing order): `rr` is an INDIVIDUAL-level block, while a score's
# crude effect sits on the mean SCORE. One rule, two readers -- reg_crude_shape() and the load-time
# reachability check (R/zzz-fact-keys.R).
#' @keywords internal
#' @noRd
reg_emp_block_of <- function(crude_key, est = NULL) {
  if (identical(crude_key, "grouped_binomial")) return(crude_key)
  fam <- (est %||% list())$crude_fam %||% "auto"
  if (!identical(fam, "auto")) fam else crude_key
}

#' @keywords internal
reg_crude_shape <- function(crude_key, est = NULL) {
  if (is.null(est)) est <- list(crude_fam = "auto", crude_shape = NA_character_)
  key <- reg_emp_block_of(crude_key, est)
  fam <- if (is.null(key) || is.na(key)) NULL else REG_EMPIRICAL[[key]]
  if (is.null(fam)) return(NULL)
  sh <- est$crude_shape
  if (is.null(sh) || is.na(sh)) sh <- fam$coef
  fam[[sh]]
}

# WARNING: `l[[""]]` is a subscript-out-of-bounds ERROR in R, not a miss -- and "" is exactly the key a
# single-column fit uses. Every lookup into a category-keyed list goes through this.
#' @keywords internal
cat_get <- function(l, key) {
  if (is.null(l) || !length(l)) return(NULL)
  i <- match(if (is.null(key)) "" else as.character(key), names(l))
  if (is.na(i)) NULL else l[[i]]
}

# THE CRUDE COLUMN (see the header's "one shape, built twice"): one fmt column per effect. The
# effect also travels back as a VECTOR keyed by OUTCOME CATEGORY; `emp_mode` decides whether those
# effects draw their own columns ("column") or ride in the model cell ("cell").
reg_empirical_columns <- function(skeleton, emp, fac_preds, crude_key, family, est, var_y,
                                  level_mag = NA_real_,
                                  conf_level = 0.95, color_signif = "grey_non_signif",
                                  color = NULL, fit_est = NULL, weighted = FALSE,
                                  degf = Inf, emp_mode = "column",
                                  saturated = TRUE, method = "wald") {
  if (is.null(REG_EMPIRICAL[[crude_key]]))
    return(list(cols = list(), cat_cols = list(), effect = NULL, shape = NULL))
  # THE crude shape, resolved ONCE (reg_crude_shape) and read by every arm below. ⚠ THE ARMS BRANCH
  # ON THE SHAPE, never on (effect, do_exp): a marginal crude effect IS what the shape's own
  # declarations compute, which is why no arm here asks whether the estimand was marginal.
  shape      <- reg_crude_shape(crude_key, est)
  if (is.null(shape)) return(list(cols = list(), cat_cols = list(), effect = NULL, shape = NULL))
  # THE COLOUR IS THE MODEL COLUMN'S. Under a GAP measure the crude column IS the baseline (`obs`
  # empty, uncoloured, marked `refcol`); ⚠ only "observed" (`adjustment`) does this.
  emp_color <- if (is.null(color)) "" else color
  gap_base  <- any(vapply(emp_color, function(k) {
    m <- measure_key(k)
    !is.na(m) && nzchar(m) && identical(MEASURES[[m]]$ref_kind, "observed")
  }, logical(1)))
  emp_signif <- if (any(nzchar(emp_color) & !is.na(emp_color))) color_signif else "ignore"
  n_rows  <- nrow(skeleton)
  is_fac  <- skeleton$var %in% fac_preds
  # the Constant is a reference row HERE TOO, or tab_bold_rows() would un-bold it.
  refrows <- (skeleton$is_ref & is_fac) | skeleton$var == "Constant"
  na_ref <- function(ci) { ci$inf[refrows] <- NA_real_; ci$sup[refrows] <- NA_real_
                           ci$pvalue[refrows] <- NA_real_; ci }
  na_v   <- function() rep(NA_real_, n_rows)
  # THE CRUDE INTERVAL a shape asks for, under THIS table's inference basis (see REG_EMPIRICAL) --
  # or, where no closed form is exact (`saturated = FALSE`), the fit's own, which is the very interval
  # its MODEL twin stamps. Stamping the same word is what keeps the pair in ONE legend block.
  emp_method <- function(shape)
    if (!isTRUE(saturated)) reg_wald_method_name(method, EST_SCALES[[shape$scale]]$mult)
    else (if (isTRUE(weighted)) shape$ci_method_design %||% shape$ci_method else shape$ci_method) %||% ""
  # the df the column's interval was referred to: the crude fits' weakest where they built it, and
  # the table's own (the design's) where a closed form did.
  emp_degf <- if (!isTRUE(saturated)) (fit_est$degf %||% NA_real_) else degf
  emp_col <- function(shape, fields, n_eff = NULL) {
    args <- c(fields, if (!is.null(n_eff)) list(n_eff = n_eff), list(
      scale = shape$scale, pct_type = reg_pct_type(shape$scale),
      digits = reg_cell_digits(shape$scale, level_mag),
      display = "est",
      ci_method = emp_method(shape), degf = emp_degf,
      color = emp_color, color_signif = emp_signif, refcol = gap_base,
      col_var = reg_crude_col_name(shape), comp_all = FALSE, in_refrow = refrows,
      model_family = family, role = "emp"))
    if (!is.na(shape$ref)) args$ref <- shape$ref
    do.call(fmt, args)
  }
  # `n_eff` IS "the effective sample size used for this cell's CI" -- so it is NA wherever the
  # interval did not come from a closed form on that base.
  neff_of <- function(v)
    if (isTRUE(weighted) && isTRUE(saturated)) as.double(v) else rep(NA_real_, n_rows)
  # emit() -- the finished column, plus the EFFECT VECTOR + SHAPE that become `obs`. A per-category
  # shape returns its column under `cat_cols`; only `emp_mode = "column"` draws a column at all --
  # `"cell"` folds the value into the model cell's own layout and `"tooltip"` prints it nowhere, but
  # BOTH still need the effect vector, which is what becomes `obs`.
  emit <- function(eff, cat = "") {
    if (is.null(eff)) return(list(cols = list(), cat_cols = list(), effect = NULL, shape = NULL))
    per_cat <- shape_per_category(eff$shape)
    key  <- if (per_cat) cat else ""
    o    <- reg_fit_overlay(eff$col, eff$vec, cat_get(fit_est$est, key), eff$shape)
    draw <- identical(emp_mode, "column")
    list(cols     = if (draw && !per_cat) stats::setNames(list(o$col), reg_crude_col_name(eff$shape)) else list(),
         cat_cols = if (draw &&  per_cat) stats::setNames(list(o$col), key) else list(),
         effect   = stats::setNames(list(o$eff), key), shape = eff$shape)
  }
  # per-category slice of the grid, aligned to the skeleton
  cat_of <- function(cat) {
    g  <- emp[emp$category == cat, , drop = FALSE]
    mi <- reg_skel_match(skeleton, g)
    lapply(stats::setNames(nm = setdiff(names(reg_empirical_empty()),
                                        c("var", "level", "category"))),
           function(nm) g[[nm]][mi])
  }

  # THE LEVEL a crude cell prints beside its estimate (EST_SCALES$base_display) -- NA on a link scale
  # and on a cumulative odds ratio, where `{base}` renders void -- and the two GEOMETRIES that level
  # pair implies. The pair rule is the model column's own (reg_fill_geometries(), R/tab_reg.R): one
  # comparison, read additively and multiplicatively, so `display` can show either on either column.
  with_base <- function(sh, fields, level, ref_level = NULL) {
    b <- EST_SCALES[[sh$scale]]$base_display
    if (!is.na(b)) fields <- c(fields, stats::setNames(list(level), b))
    if (is.null(ref_level)) return(fields)
    c(fields, reg_geometry_fields(EST_SCALES[[sh$scale]]$est_field, level, ref_level))
  }
  # the SD-standardized ladder's divisor (raw_diff): the model column carries var(Y) there.
  with_var <- function(sh, fields)
    if (identical(EST_SCALES[[sh$scale]]$sd_from %||% "", "var"))
      c(fields, list(var = rep(var_y, n_rows))) else fields

  # ---- the SUPERIORITY PAIR, read off the outcome x predictor table -------------------------------
  # The crude twin of a rank column is the SAME reg_rank_pair() its model column's sweep runs, given
  # the counted distributions instead of the two standardised predictions. One formula, two inputs --
  # which is what makes the distance between the two columns mean adjustment and nothing else.
  rank_effect <- function(sh) {
    sc   <- EST_SCALES[[sh$scale]]
    gl   <- lapply(unique(emp$category), cat_of)     # level-major grid: already in level ORDER
    P1   <- do.call(cbind, lapply(gl, `[[`, "emp_prop"))
    P0   <- do.call(cbind, lapply(gl, `[[`, "emp_ref_prop"))
    n1   <- gl[[1]]$emp_n_draw; n0 <- gl[[1]]$emp_ref_n_draw
    # a multiplicative or a link scale is the log of the win ratio; anything else is Somers' D.
    lnk  <- if (isTRUE(sc$mult) || identical(sc$geometry, "log")) "log" else "identity"
    zed  <- !is.finite(degf)
    est  <- gam <- alt <- se <- na_v()
    for (i in seq_len(n_rows)) {
      if (anyNA(P1[i, ]) || anyNA(P0[i, ])) next
      pr <- reg_rank_pair(P1[i, ], P0[i, ], lnk)
      if (is.null(pr)) next
      est[i] <- pr$est; gam[i] <- pr$gamma; alt[i] <- pr$alt
      se[i]  <- reg_rank_se(pr, P1[i, ], P0[i, ], n1[[i]], n0[[i]])
    }
    res <- reg_wald_finalize(est, do_exp = isTRUE(sc$mult), se = se,
                             crit = reg_wald_crit(zed, degf, conf_level),
                             disp_known = zed, df = degf)
    ci  <- na_ref(list(inf = res$conf.low, sup = res$conf.high, pvalue = res$p.value))
    fields <- c(stats::setNames(list(res$estimate), sc$est_field),
                list(n = gl[[1]]$emp_n, tot_n = gl[[1]]$emp_n,
                     ci_inf = ci$inf, ci_sup = ci$sup, pvalue = ci$pvalue))
    # the base is gamma, and the OTHER reading is a primitive of the same pair rather than something
    # derivable from (gamma, 1/2) -- so both are written here, not by with_base()'s geometry rule.
    if (!is.na(sc$base_display))
      fields <- c(fields, stats::setNames(list(gam), sc$base_display),
                  stats::setNames(list(alt),
                                  if (identical(sc$est_field, "diff")) "ratio" else "diff"))
    list(fields = fields, vec = res$estimate, n_eff = gl[[1]]$emp_n_draw)
  }

  if (identical(crude_key, "ordinal")) {
    # a CUMULATIVE odds ratio has no closed form (the estimate is spliced in by reg_fit_overlay) and
    # no single share to sit on, so `{base}` stays void by construction.
    # ⚠ `n` is what fmt() sizes the column from, so the void branch must still pass it.
    if (isTRUE(shape$refit))
      return(emit(list(col = emp_col(shape, list(n = rep(NA_integer_, n_rows))),
                       vec = na_v(), shape = shape)))
    pe <- rank_effect(shape)
    return(emit(list(col = emp_col(shape, pe$fields, n_eff = neff_of(pe$n_eff)),
                     vec = pe$vec, shape = shape)))
  }

  # ---- the probability families: one closed form per OUTCOME CATEGORY, dispatched on the SHAPE's
  # OWN declarations -- its scale's geometry, then its own `ci_method`. ⚠ never on the family's
  # coefficient shape: a shape is routinely BORROWED across blocks (a binary marginal ratio's crude
  # twin is REG_EMPIRICAL$rr$rr), so the family here and the shape's own family can differ.
  # WARNING: `emp_ratio` is an ODDS ratio, `emp_prop / emp_ref_prop` a RISK ratio.
  prob_effect <- function(sh, g) {
    prop <- g$emp_prop; rprop <- g$emp_ref_prop
    # a share for a probability scale, the mean SCORE for a summed-score one -- and its reference,
    # in the same unit, so the derived geometries stay on the unit the cell prints.
    score <- identical(EST_SCALES[[sh$scale]]$base_display, "mean")
    level <- if (score) g$emp_mean     else prop
    rlvl  <- if (score) g$emp_ref_mean else rprop
    ndr  <- g$emp_n_draw; rndr <- g$emp_ref_n_draw
    # a link scale is always the log of a RATIO: a log of a difference has no meaning, and
    # reg_estimand() refuses to compose one, so "log" never reaches the difference arm.
    geom   <- EST_SCALES[[sh$scale]]$geometry
    logged <- identical(geom, "log")
    if (identical(geom, "difference")) {
      v  <- g$emp_diff
      ci <- na_ref(ci_prop_diff(prop, ndr, rprop, rndr, conf_level = conf_level,
                                method = emp_method(sh), want_p = TRUE, df = degf))
    } else if (identical(emp_method(sh), "katz")) {
      v  <- prop / rprop
      ci <- na_ref(ci_katz_rr(prop, ndr, rprop, rndr, conf_level = conf_level,
                              want_p = TRUE, df = degf))
    } else {
      # Woolf's 2x2, on the shares the odds is actually taken over: this category against the PIVOT,
      # in this level and in the reference one. WEIGHTED share x UNWEIGHTED base, so the base cancels
      # out of the log-OR. On a binary outcome the pivot IS the complement, so this is the ordinary
      # 2x2; on a multinomial it is what keeps the interval on `emp_ratio`'s own estimand.
      v  <- g$emp_ratio
      ci <- na_ref(ci_or(prop * ndr, g$emp_pivot_prop * ndr,
                         rprop * rndr, g$emp_ref_pivot_prop * rndr,
                         conf_level = conf_level, want_p = TRUE, df = degf))
    }
    if (logged) { ci$inf <- log(ci$inf); ci$sup <- log(ci$sup); v <- log(v) }
    est_fld <- EST_SCALES[[sh$scale]]$est_field
    fields  <- c(stats::setNames(list(v), est_fld),
                 list(n = g$emp_n, tot_n = g$emp_n,
                      ci_inf = ci$inf, ci_sup = ci$sup, pvalue = ci$pvalue))
    list(fields = with_var(sh, with_base(sh, fields, level, rlvl)), vec = v, n_eff = g$emp_n_draw)
  }

  if (identical(crude_key, "multinomial")) {
    sh   <- shape
    cats <- unique(emp$category)
    if (!length(cats)) return(list(cols = list(), cat_cols = list(), effect = NULL, shape = sh))
    out <- purrr::map(stats::setNames(nm = cats), function(k) {
      # ⚠ NO marginal override: a marginal crude effect IS what prob_effect() computes for its own
      # shape (`ame` -> the share difference, `ame_ratio` -> the share ratio). Re-deriving it from
      # `comparison` here wrote a DIFFERENCE into the `or` field of the at-reference odds-ratio arm.
      pe <- prob_effect(sh, cat_of(k))
      emit(list(col = emp_col(sh, pe$fields, n_eff = neff_of(pe$n_eff)), vec = pe$vec, shape = sh), k)
    })
    return(list(cols = list(), cat_cols = purrr::flatten(purrr::map(out, "cat_cols")),
                shape = sh, effect = purrr::flatten(purrr::map(out, "effect"))))
  }

  # ---- the closed-form families: one category ("1" binary/grouped, "" numeric outcomes) -------------
  cat1 <- if (identical(emp$category[1], "1") || "1" %in% emp$category) "1" else ""
  g    <- cat_of(cat1)
  ratio <- g$emp_ratio
  meanv <- g$emp_mean; varv <- g$emp_var; nv <- g$emp_n
  rmean <- g$emp_ref_mean; rv <- g$emp_ref_var; rn <- g$emp_ref_n
  nv_ci <- g$emp_n_ci; rn_ci <- g$emp_ref_n_ci   # the effective n; unweighted it equals `nv`
  # the model-based dispersion the two MOMENT families need, pooled per predictor's level set.
  emp_pool <- function(shape, kind) {
    if (!identical(emp_method(shape), CI_POOLED[[kind]])) return(NULL)
    ci_pool_disp(n = nv_ci, mean = meanv, var = varv, by = skeleton$var, use = is_fac, kind = kind)
  }

  # ⚠ a SUMMED SCORE's difference is a difference of mean SCORES -- it takes the MOMENT arm below.
  binary_like <- (reg_fam_binary(crude_key) || identical(crude_key, "grouped_binomial")) &&
    !identical(shape$scale, "raw_diff")
  if (binary_like) {
    pe <- prob_effect(shape, g)
    return(emit(list(col = emp_col(shape, pe$fields, n_eff = neff_of(pe$n_eff)),
                     vec = pe$vec, shape = shape), cat1))
  }

  # ---- the moment families: a mean difference / a ratio of means / a rate ratio -------------------
  moment <- function(sh, v, ci) {
    fields <- c(stats::setNames(list(v), EST_SCALES[[sh$scale]]$est_field),
                list(n = nv, tot_n = nv, ci_inf = ci$inf, ci_sup = ci$sup, pvalue = ci$pvalue))
    emit(list(col = emp_col(sh, with_var(sh, with_base(sh, fields, meanv, rmean)),
                            n_eff = neff_of(nv_ci)),
              vec = v, shape = sh), cat1)
  }

  # ---- ONE moment arm: a mean difference, a ratio of means, a rate ratio, and their logged twins.
  # The declared shape's own scale picks the ENGINE, not the family.
  if (crude_key %in% c("gaussian", "mr", "poisson", "grouped_binomial")) {
    logged <- identical(shape$scale, "log_coef")   # a link-scale shape is always the log of a RATIO
    if (!logged && identical(shape$scale, "raw_diff")) {
      md <- na_ref(ci_mean_diff2(meanv, varv, nv_ci, rmean, rv, rn_ci,
                                 method = emp_method(shape), conf_level = conf_level,
                                 want_p = TRUE, df_design = degf,
                                 pool = emp_pool(shape, "mean_diff")))
      # ⚠ the estimate must be `meanv - rmean`, not the grid's `emp_diff` (a different unit).
      return(moment(shape, meanv - rmean, md))
    }
    rr <- na_ref(ci_mean_ratio(meanv, varv, nv_ci, rmean, rv, rn_ci,
                               method = emp_method(shape), conf_level = conf_level,
                               want_p = TRUE, df_design = degf,
                               pool = emp_pool(shape, "mean_ratio")))
    if (!logged) return(moment(shape, ratio, rr))
    return(moment(shape, log(ratio),
                  list(inf = log(rr$inf), sup = log(rr$sup), pvalue = rr$pvalue)))
  }

  list(cols = list(), cat_cols = list(), effect = NULL, shape = NULL)
}

# === the model-vs-observed GAP standard error =====================================================

# reg_same_estimand(): is the crude shape the SAME QUANTITY as the model column beside it? Two
# declared facts, because neither alone is enough: the SCALE (an additive count AME must never be
# compared to a crude rate ratio) and the declared measure WORD (every logged measure shares the one
# `log_coef` scale, so scale alone cannot tell log(OR) from log(RR)). Both are base words -- the
# contrast marker is composed at render, and a crude column is never marked.
# ⚠ `scale` is passed, not read off a column: the note in reg_color_notes() asks this question before
# any column exists, and the two must not be able to disagree.
#' @keywords internal
reg_same_estimand <- function(shape, scale, est)
  !is.null(shape) &&
  identical(as.character(shape$scale)[1], as.character(scale)[1]) &&
  identical(as.character(shape$word)[1],  as.character(est$word)[1])

# reg_same_frame(): the crude frame is a SUBSET of the model's, so equal row counts PROVE row
# identity. Fires under `na = "drop_by_model"` or a compound formula. `nobs` IS nrow(mdata) on
# every fit record and survives the jamovi digest path.
#' @keywords internal
reg_same_frame <- function(mdata, f) {
  n_fit <- if (!is.null(f$data)) nrow(f$data) else f$nobs
  !is.null(n_fit) && !is.na(n_fit) && identical(as.integer(nrow(mdata)), as.integer(n_fit))
}

# reg_gap_se_columns() -- the SE of the gap between ONE fit's effect and its observed counterpart
# (the maths is R/reg-influence.R's). The gate is FIVE correctness facts; NULL unless all five hold,
# regardless of whether `color = "adjustment"` was asked -- forest_plot()'s gap band reads the same.
#   * `shape`       the crude twin's REG_EMPIRICAL row: absent = no observed effect at all.
#   * a model       the fit or its digest, and the frame the digest rebuilds.
#   * same estimand reg_same_estimand() -- the scale AND the measure word.
#   * same frame    reg_same_frame().
#   * collapsible   a conditional odds ratio moves under adjustment even with zero confounding.
# `method = "profile"` is NOT a clause: adjustment COMPUTES its own SE regardless of the bracket.
#' @keywords internal
reg_gap_se_columns <- function(f, sp, model_col, skeleton, shape, mdata, fac_preds,
                               est, wt, fits_crude = NULL, fit_preds = character(0),
                               multiplier = NULL, category = "", crosses = list(),
                               saturated = TRUE) {
  # the estimand ROW carries both the profile axis (`at_reference`) and the marginal ratio.
  effect   <- est$effect
  marginal <- !identical(effect, "conditional")
  mlink    <- est$measure_link %||% "identity"
  if (is.null(shape) || is.null(reg_model_of(f)) || is.null(f$data)) return(NULL)
  if (isTRUE(sp$compound) || identical(effect, "at_reference")) return(NULL)
  if (!reg_same_estimand(shape, get_scale(model_col), est))     return(NULL)
  if (!reg_same_frame(mdata, f))                                return(NULL)
  # only a conditional (coefficient) odds ratio is non-collapsible.
  if (!reg_estimand_collapsible(sp$fit_family, effect))             return(NULL)
  # a REPLICATE-weights design needs withReplicates, not svyrecvar's linearization.
  des <- reg_model_design(f)
  if (inherits(des, "svyrep.design"))                           return(NULL)

  coef_if <- reg_coef_if_maker(reg_model_of(f), f$data)
  if (is.null(coef_if)) return(NULL)
  # a 3+ level outcome's marginal influence function is per CATEGORY too (family()$mu.eta lacks it) --
  # unless the estimand is a RANK, which reads the whole distribution and answers once.
  rank_est <- identical(est$level, "rank")
  per_cat  <- !rank_est && reg_model_categorical(reg_model_of(f))
  m  <- reg_model_of(f)
  model_if <- if (marginal && rank_est)
    reg_ame_if_maker(m, f$data, wt, link = mlink, coef_if = coef_if,
                     g = reg_gcomp_rank_maker(m, f$data, wt, mlink))
  else if (marginal && per_cat)
    reg_ame_if_cat_maker(m, f$data, wt, link = mlink, category = category)
  else if (marginal)
    reg_ame_if_maker(m, f$data, wt, link = mlink, coef_if = coef_if)
  else coef_if
  # the crude leg must be built around the SAME quantity the crude estimate was: one category's
  # indicator, or -- on a rank column -- the pair's own gradient read over every category.
  crude_if <- if (rank_est) reg_crude_rank_if_maker(mdata, sp$outcome, wt, mlink)
  else reg_crude_if_maker(mdata, sp$outcome, sp$crude_key, f$positive_level, wt, shape$link,
                          trials = sp$trials, category = category, ref_category = f$y_ref)
  if (is.null(model_if)) return(NULL)

  n_rows  <- nrow(skeleton)
  out     <- rep(NA_real_, n_rows)
  ref_of  <- function(v) { r <- skeleton$level[skeleton$var == v & skeleton$is_ref]
                           if (length(r)) as.character(r[[1]]) else NA_character_ }
  # the spec's ROW blocks, not its formula terms: a nested cross block's name is not a main effect.
  in_mod  <- skeleton$var %in% unique(c(sp$predictors, sp$row_vars))
  # WARNING: one length-n difference vector at a time -- never an n x p matrix of them.
  # ⚠ ONE fork, the block's own (reg_crude_block): where the crude column was refit rather than read
  # off the cells, its influence leg comes from that fit too -- the `fits_crude` loop below, which
  # covers factor predictors. The two loops stay disjoint because they read the same `saturated`.
  closed_form <- !is.null(crude_if) && isTRUE(saturated)
  for (k in if (closed_form) which(in_mod & skeleton$var %in% fac_preds & !skeleton$is_ref) else
              integer(0)) {
    v <- as.character(skeleton$var[k]); r <- ref_of(v)
    if (is.na(r)) next
    im <- if (marginal) model_if(v, as.character(skeleton$level[k]), r) else {
      tm <- skeleton$term[k]
      if (is.na(tm)) next
      L <- stats::setNames(1, tm)                       # the display data is already releveled
      coef_if(L)
    }
    if (is.null(im)) next
    # the crude leg lives on `mdata`; svy_domain_design() pads a calibrated/PPS design back to length.
    ic <- reg_if_align(crude_if(v, as.character(skeleton$level[k]), r), length(im),
                       mdata[[svy_row_col]])
    if (is.null(ic) || length(ic) != length(im)) next
    out[k] <- reg_if_se(im - ic, des)
  }

  # the NUMERIC arm: no cells, so the crude influence comes from its own univariable FIT.
  # `multiplier` scales gap_se by |k|; `between_groups` RECOVERS its SE from the printed interval.
  if (length(fits_crude) && length(fit_preds)) {
    for (k in which(in_mod & skeleton$var %in% fit_preds & !skeleton$is_ref)) {
      v  <- as.character(skeleton$var[k])
      nv <- fits_crude[[v]]
      # ⚠ reg_model_of(), NEVER `nv$fit`: a crude record SERVED from the store carries a digest and
      # no fitted object, and demanding the fit here would silently drop the gap SE -- i.e. the whole
      # of `color = "adjustment"` -- on every cache hit.
      if (is.null(nv) || is.null(reg_model_of(nv))) next
      # a crossed slope's unit is its MODIFIED variable's, so the |k| rescale is looked up there.
      mv <- reg_cross_of(crosses, v)$modified %||% v
      kk <- if (!is.null(multiplier) && mv %in% names(multiplier)) as.numeric(multiplier[[mv]]) else 1
      if (!is.finite(kk) || kk == 0) next
      cif_v <- reg_coef_if_maker(reg_model_of(nv), nv$data)
      if (is.null(cif_v)) next
      # covers FACTOR predictors too: (level, reference level) vs a numeric's k-unit difference.
      is_fac_k <- v %in% fac_preds
      cl <- if (is_fac_k) list(as.character(skeleton$level[k]), ref_of(v)) else list(kk, 0)
      if (is_fac_k && is.na(cl[[2]])) next
      if (marginal) {
        im <- model_if(v, cl[[1]], cl[[2]])
        ic <- if (reg_model_categorical(reg_model_of(nv)))
          reg_ame_if_cat_maker(reg_model_of(nv), nv$data, wt, link = mlink,
                               category = category)
        else
          reg_ame_if_maker(reg_model_of(nv), nv$data, wt, link = mlink,
                           coef_if = cif_v)
        ic <- if (is.null(ic)) NULL else ic(v, cl[[1]], cl[[2]])
        # the AME contrast already carries k, so no |k| rescale on this branch
        if (is.null(im) || is.null(ic) || length(ic) != length(im)) next
        out[k] <- reg_if_se(im - ic, des)
      } else {
        tm <- skeleton$term[k]
        if (is.na(tm)) next
        im <- coef_if(stats::setNames(1, tm))
        # the crude fit carries the SAME term name, so a factor level keys exactly as a numeric slope.
        ic <- cif_v(stats::setNames(1, tm))
        if (is.null(im) || is.null(ic) || length(ic) != length(im)) next
        out[k] <- abs(kk) * reg_if_se(im - ic, des)
      }
    }
  }
  if (all(is.na(out))) NULL else out
}
