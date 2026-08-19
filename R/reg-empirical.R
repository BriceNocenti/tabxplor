# PURPOSE: THE OBSERVED (crude) COMPANION of a model effect, and the standard error of the gap
#   between the two.
# ROLE: `tab_reg(empirical =)`'s producer. Beside every modelled effect it puts the SAME estimand
#   computed with one predictor instead of all of them, so "what did adjustment change" is read
#   across the table. The gap is what `color = "adjustment"` grades and what `{obs}` / `{gap}` print.
# KEY CONSTRAINTS:
#   - ONE COLUMN SHAPE, BUILT TWICE. The crude and the modelled effect are the same estimand, so the
#     crude column is the model column's mirror: same stored scale, same colour measure (both
#     channels), same display, same digits, same reference. Only the estimation differs. It carries
#     exactly one interval -- its effect's -- and the level it sits on rides in the same cell, in the
#     field its scale names (`{base}`). It must never carry `obs` or `gap_se`: it IS the observed
#     value, and a column cannot be its own baseline.
#   - SAME ESTIMAND, SAME PEOPLE, or nothing. reg_same_estimand() / reg_same_frame() withhold the
#     crude value rather than let a "gap" mean listwise deletion instead of confounding.
#   - TWO SOURCES, ONE SHAPE. A CLOSED FORM off reg_empirical()'s per-(var, level, category) grid
#     wherever the univariable model is saturated (every factor predictor except under ordinal) --
#     there the crude odds ratio IS the Woolf 2x2 ratio. Otherwise a univariable reg_fit() through
#     the very fitter the table came from (ordinal, every numeric predictor, every marginal shape),
#     so "same estimand, link, CI rule, multiplier" holds by construction.
#   - The crude interval is the univariable MODEL's, under the table's own inference basis: pooled /
#     model-based unweighted, the sandwich weighted. See REG_EMPIRICAL's `ci_method_design`.
#
# THE FACT TABLE: `REG_EMPIRICAL` -- per family, the shape of each crude effect column. A family is a
# row, never a switch arm; its keys are foreign-key checked in R/zzz-fact-keys.R.
#
# WHAT IS HERE
#   reg_crude_y / reg_crude_yw    the outcome on the scale the crude estimator averages
#   reg_level_counts              the N behind each predictor level (`add_n`'s column)
#   reg_empirical                 THE grid: emp_prop / emp_mean / emp_diff / emp_ratio / emp_n (+CIs)
#   reg_empirical_fit             the univariable fits the non-saturated shapes need
#   reg_fit_overlay               splicing those fitted rows into a grid-built column
#   REG_EMPIRICAL + reg_crude_shape / shape_per_category   the shape vocabulary
#   reg_empirical_columns         THE builder: one fmt column per crude effect
#   reg_same_estimand / _frame    the two predicates that withhold `obs` rather than lie
#   reg_gap_se_columns            the gap SE (R/reg-influence.R's math, orchestrated per column)
#
# The STAGE driving these producers is reg_stage_crude() in R/tab_reg.R (and reg_spec_build()'s
# per-spec step for a several-outcome table) -- the tab-leaf.R / tab.R relationship.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).
#
# WARNING: this file sorts BEFORE R/tab_reg.R, so its top-level code (the REG_EMPIRICAL literal) may
# not read anything defined there. Every cross-file call below is made at RUN time, which is why the
# split is free.

# === empirical : the descriptive crude companion beside the model effect (Phase 12g / 14v) =========

# The crude (unadjusted, single-predictor) companion of the model effect: the bivariate association
# between a FACTOR predictor and the outcome, which IS the modelised quantity when there is one
# predictor (standard "crude vs adjusted" comparison; a large gap signals confounding). Computed
# DIRECTLY (not via tab()) so the outcome direction / reference level match the skeleton, per family:
#   binomial : emp_base = P(positive | level), emp_ratio = crude OR (odds / ref odds).
#   gaussian : emp_base = weighted mean(Y | level), emp_var = weighted var (tab()'s formula, so the
#              "Emp. mean" sd matches tab() exactly), emp_ratio = mean / ref mean (unused for colour).
#   poisson  : emp_base = weighted mean(count | level) (crude rate), emp_ratio = crude rate-ratio.
# emp_diff is always emp_base - ref emp_base (risk- or mean-difference). Returns a tibble keyed by
# (var, level): emp_base, emp_diff, emp_ratio, emp_var, emp_n (unweighted cell count).
# reg_crude_y() -- Phase 18z8-B: the outcome ON THE SCALE THE CRUDE ESTIMATOR AVERAGES. For a binary
# outcome that is the 0/1 indicator of the positive level, which needs reg_prep_binary()'s own recode
# mirrored (the model frame has been through it; the raw `data` the crude block reads has not, so
# as.character(0/1) would never match the label and the crude base would silently be 0 -- the pre-14v-ii
# bug). Otherwise the numeric value. ONE definition, shared by reg_empirical()'s cell means and
# reg_crude_if_maker()'s residuals -- else the influence function could be built around a different `y`
# than the estimate it is the standard error OF.
#' @keywords internal
reg_crude_y <- function(data, outcome, family, positive_level) {
  yv <- data[[outcome]]
  if (!reg_fam_binary(family)) return(as.numeric(yv))
  if (is.numeric(yv) && all(stats::na.omit(yv) %in% c(0, 1)))
    yv <- factor(yv, levels = c(0, 1), labels = c(paste0("Not ", outcome), outcome))
  as.numeric(as.character(yv) == positive_level)
}

# reg_crude_yw() -- Phase 18z10: reg_crude_y()'s generalisation, the ONE description of "what the
# crude estimator averages, and with what weights", for every outcome kind. It returns the pieces the
# crude GRID and the crude INFLUENCE FUNCTION both read, so the two can never be built around different
# data (the invariant reg_crude_y() was extracted for in z8-B).
#
#   $y      the per-observation outcome on the crude scale (a 0/1 indicator, a category label, a number)
#   $w      the weights the crude estimator averages with
#   $cats   the outcome categories the grid produces a row for ("" = no categories, a numeric outcome)
#   $ref    the category the ODDS are conditional on
#   $num    the numeric outcome behind the mean/variance part (NULL = none), with $num_w its weights
#
# DESIGN -- the three kinds and why they are one function:
#   binary  (binomial / rr) : y = the 0/1 indicator, cats = c("1","0"), ref = "0". The category-
#                             conditional odds p1/p0 IS the plain odds, which is why the binary case
#                             looked like it needed no `category` key at all.
#   grouped_binomial        : each ROW is a cluster of `trials` Bernoulli draws, so y = succ/trials with
#                             weight w*trials -- and Sum(w*trials*y) = Sum(w*succ) is exactly the summed
#                             2x2 leg. The mean SCORE is a separate, per-RESPONDENT quantity, hence $num
#                             carrying its own weights.
#   categorical (multinomial / ordinal) : y = the outcome label, cats = every level, ref = the model's
#                             own baseline category -> the {j, ref} x {level, ref level} Woolf OR, i.e.
#                             the very number tab(pct = "row", OR = "OR") prints.
#   numeric (gaussian / poisson) : no categories; only the moment part.
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

# reg_level_counts() -- Phase 18z13 (SS7.1): the N behind each predictor level, on the model's own
# complete-case frame, aligned to the skeleton. STROBE asks for the unadjusted numbers behind an
# association and both comparable packages always print them; tabxplor already HAD the number (it is
# `emp_n` in reg_empirical()'s grid) but only under `empirical = TRUE`, only for families with a crude
# twin, and only in the html tooltip. This is its family-free generalisation: the two cannot disagree,
# both being counts of rows of the same frame.
#
# NA on a numeric predictor's row and on the Constant is deliberate: on a listwise-complete frame that
# count is nrow(frame) for EVERY numeric predictor, so a per-row cell would look specific and not be
# (the same reasoning z9 applied to the crude base cell). The Constant row shows the model N, which is
# the denominator every other cell is a part of.
#' @keywords internal
reg_level_counts <- function(frame, skeleton, wt = NULL) {
  n  <- rep(NA_integer_, nrow(skeleton))
  wn <- rep(NA_real_,    nrow(skeleton))
  w  <- if (!is.null(wt) && wt %in% names(frame)) as.numeric(frame[[wt]]) else NULL
  n[skeleton$var == "Constant"] <- nrow(frame)
  if (!is.null(w)) wn[skeleton$var == "Constant"] <- sum(w, na.rm = TRUE)
  for (v in setdiff(unique(skeleton$var), "Constant")) {
    if (!v %in% names(frame) || !reg_is_factor_var(frame[[v]])) next
    lv  <- as.character(frame[[v]])
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

# The zero-row shape of reg_empirical()'s long tibble -- ONE definition, so the empty case cannot drift
# from the populated one (Phase 18z9).
#' @keywords internal
reg_empirical_empty <- function()
  tibble::tibble(
    var = character(0), level = character(0), category = character(0),
    emp_prop = numeric(0), emp_prop_inf = numeric(0), emp_prop_sup = numeric(0),
    emp_diff = numeric(0), emp_diff_inf = numeric(0), emp_diff_sup = numeric(0),
    emp_ratio = numeric(0), emp_ratio_prop = numeric(0),
    emp_wpos = numeric(0), emp_wneg = numeric(0),
    emp_mean = numeric(0), emp_var = numeric(0),
    emp_n = integer(0), emp_n_ci = numeric(0), emp_n_draw = numeric(0),
    emp_ref_n_draw = numeric(0),
    emp_ref_prop = numeric(0), emp_ref_mean = numeric(0), emp_ref_var = numeric(0),
    emp_ref_n = integer(0), emp_ref_n_ci = numeric(0)
  )

# reg_empirical() -- Phase 18z10: THE crude grid, keyed (var, level, category).
#
# DESIGN -- this ONE producer replaces reg_empirical() + reg_empirical_tips(), which were the same
# computation at two key widths (measured line by line: the tips' `sum(w[m & y == cat]) / sum(w[m])`
# is bit-identical to the old binary branch's `wpos / (wpos + wneg)`). The tips version was simply the
# general K-category form; the old binary one was its K = 2, positive-level-only slice. Merging them is
# what lets multinomial and ordinal have a crude counterpart at all, at the cost of one extra key column.
#
# Two PARTS, because a family may need either or both (Phase 18z10 ruling: a grouped binomial shows a
# mean SCORE beside a summed-count OR, so it needs both at once -- which no single `emp_base` column
# could carry):
#   CATEGORICAL, per (var, level, category): the weighted share `emp_prop` (+ its Wilson interval), its
#     difference from the predictor's reference LEVEL (+ Newcombe), the two 2x2 legs `emp_wpos` /
#     `emp_wneg` (the category vs the reference CATEGORY), and the two ratios built from them --
#     `emp_ratio` = the Woolf ODDS ratio, `emp_ratio_prop` = the risk ratio.
#   NUMERIC, per (var, level): the weighted mean and variance (tab()'s own formula, so a crude mean's sd
#     matches tab() exactly).
#
# WARNING: `emp_ratio` is built from emp_wpos/emp_wneg, i.e. the odds of the category against the
# REFERENCE CATEGORY -- not against "everything else". For a binary outcome the two coincide (the
# reference category IS the complement), which is why one odds column looked sufficient before z10; for
# a multinomial they do not, and the {j, ref} form is the one nnet::multinom estimates and the one
# tab(pct = "row", OR = "OR") prints.
#
# Weighted rule (SS14): weighted proportions/means, unweighted `n`, and a SEPARATE effective n
# (`n_ci` / `n_draw`) for the intervals. Phase 18z14-iii makes that base come from the SAME
# producers tab()'s cells use (svy_inference_basis): a survey DESIGN passed as `data` ->
# Korn-Graubard's device on the design variance; else the EXACT flat closed form on the weights
# (svy_flat_neff_rows); else the raw count. The basis is FORCED weighted here (ruling 1) -- it is not
# the tab()-scoped option -- so a weighted crude column always matches the svyglm column beside it.
# Unweighted is byte-identical.
reg_empirical <- function(data, fac_preds, outcome, crude_key, positive_level, wt,
                          trials = NULL, ref_category = NULL, conf_level = 0.95,
                          design_spec = NULL) {
  yw   <- reg_crude_yw(data, outcome, crude_key, positive_level, wt, trials, ref_category)
  cats <- yw$cats
  # The basis comes from the ONE resolver, not a local option read (the drift z14-ii closed for
  # tab()). Phase 18z16-i, ruling 1: tab_reg() FORCES the weighted basis -- its crude Obs_* columns
  # must be comparable with the Model_* column beside them, which is always design/weight-based
  # (a weighted fit goes through svyglm, i.e. the Binder linearization). The tab()-scoped
  # tabxplor.design_effect option is therefore never read here.
  basis <- svy_inference_basis(design_spec, wt, force = TRUE)
  # Phase 18z16-ii: the WEIGHTED base is the flat design's own, in closed form -- the same
  # p(1-p)/Var_design device tab()'s cells use, evaluated at ids = ~1 (svy_flat_neff_rows). It replaces
  # Kish, which is that formula with the cell's own Sum(w^2) discarded (measured up to 17 % wrong in
  # either direction, and unable to move with the outcome at all). Unweighted -> the raw count,
  # byte-identical. `n_obs` is the crude frame's row count = survey's nPSU for its flat design.
  weighted <- identical(basis, "weights") || identical(basis, "design")
  n_obs    <- nrow(data)
  # Phase 18z16-iiiii (D4): a design's DEGREES OF FREEDOM. survey refers every interval to t(degf),
  # and the model columns of a design-weighted tab_reg() already are (an svyglm's df.residual IS the
  # design df) -- while the crude companions beside them were referred to z, so at degf = 8 the crude
  # bracket printed 15 % narrower than the model bracket it exists to be compared with. `Inf` (no
  # design) is a no-op: qt(p, Inf) is bit-identical to qnorm(p).
  degf     <- design_spec$degf %||% Inf
  # the per-RESPONDENT weight: yw$w already carries the grouped-binomial `trials` multiplier, and the
  # ratio form wants (weight, u, v) = (w, successes, trials) -- see svy_flat_neff_rows().
  w0       <- if (identical(yw$kind, "share")) yw$w / yw$draws else yw$w
  flat_neff <- function(keep, u, v, raw, num = NULL) {
    if (!weighted) return(as.double(raw))
    ne <- svy_flat_neff_rows(w0[keep], u[keep], v[keep], n_obs, num = num)
    if (isTRUE(is.finite(ne) && ne > 0)) ne else as.double(raw)
  }
  has_num <- !is.null(yw$num)
  has_cat <- !identical(yw$kind, "numeric")
  share   <- identical(yw$kind, "share")
  # Phase 18z16-iv (W-E): the difference-CI method is the FAMILY's declared one (REG_EMPIRICAL is
  # the single source), never a literal written here. "wald" is the fallback for a key that declares
  # none -- it is what tab_reg() uses throughout.
  emp_method_diff <- REG_EMPIRICAL[[crude_key]]$method_diff %||% "wald"
  # variance only where a mean column is actually built (gaussian / poisson / the grouped mean score)
  want_var <- has_num
  # Phase 18z9: a TYPED zero-row return. purrr::map_dfr over character(0) yields a 0x0 tibble, whose
  # columns are NULL -- reg_empirical_columns() then errors ("Can't recycle input of size 0").
  if (length(fac_preds) == 0L) return(reg_empirical_empty())

  # --- Phase 18z14-iii: the DESIGN-based effective n --------------------------------------------
  # A crude cell IS a weighted mean over a domain (the predictor level), so its design variance is the
  # producer R/survey-variance.R already owns -- the same influence vector reg_crude_if_maker() builds
  # for the gap test (its identity-link leg w(y-mu)/Sum(w) IS svy_var_mean()'s wf*d*(x-M)/B), but
  # batched one svyrecvar call per quantity and scattered through svy_var_prep()'s `at`, which is what
  # a CALIBRATED design needs. Every crude interval then follows for free: they all consume `n_ci` or
  # `n_draw`, and on an effective base the Woolf and Katz brackets ARE Var_design(logit p) and
  # Var_design(log p) by construction.
  # Phase 18z16-iiiii: a LOCAL latch, and the reason travels OUT on the returned grid
  # (attr "degrade"), which reg_stage_crude() harvests into the basis reg_stage_finalize() stamps -- the process-global
  # degrade environment is gone, so one degraded table can no longer mislabel every later one.
  said <- FALSE
  degrade <- function(reason = NULL) {
    if (!said) { svy_var_degraded(reason); said <<- TRUE }
    NULL
  }
  # z16-ii: a FLAT svydesign(ids = ~1) has the closed form as its exact answer (verified: identical
  # to svyrecvar here), so it takes the algebraic path -- no influence matrix, no ceiling.
  need_svy <- !is.null(design_spec$design) && !svy_design_is_flat(design_spec$design)
  prep <- if (need_svy) svy_var_prep(design_spec$design, data[[svy_row_col]]) else NULL
  if (need_svy && is.null(prep)) degrade()
  if (!is.null(prep)) {
    # the grid's own weights must BE the design's, or the printed estimate and the variance beside it
    # would describe two different populations.
    wg <- prep$w[prep$at] * yw$draws
    if (length(wg) != length(yw$w) || anyNA(wg) ||
        !isTRUE(max(abs(wg - yw$w)) <= 1e-8 * max(1, max(abs(yw$w))))) { degrade(); prep <- NULL }
  }
  # Var_design per level: `$p` an nl x nc matrix (the share of each outcome category), `$m` an nl x 1
  # (the numeric mean). The domain keys are the level INDEX, so the domain is `ok & x == l` by
  # construction and a predictor level literally named "Total" cannot trip svy_group_map()'s rule.
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
      # "share": y is the per-row SHARE of successes (0/1 for an ordinary binary outcome, succ/trials
      # for a grouped one), so the 2x2 legs are Sum(w*y) / Sum(w*(1-y)) -- which for 0/1 IS the indicator
      # sum the binary arm always computed. "labels": one indicator per outcome category.
      wc <- if (!has_cat) NA_real_
            else if (share) stats::setNames(c(sum(yw$w[m] * yw$y[m]), sum(yw$w[m] * (1 - yw$y[m]))),
                                            cats)
            else vapply(cats, function(k) sum(yw$w[m & yw$y == k]), numeric(1))
      # z16-ii: the CI base of a PROPORTION is now its own flat-design effective n, per CATEGORY --
      # the ratio p_k = Sum(w u_k) / Sum(w v) with (u, v) = (successes, trials) for a share and
      # (indicator, 1) for a label. For a grouped binomial that is the number of independent Bernoulli
      # DRAWS the level is worth, which is why it is not n x trials.
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
        mean  = NA_real_, var = NA_real_
      )
      if (has_num) {
        nw <- yw$num_w; n1 <- sum(m); wn <- sum(nw[m])
        s1 <- sum(nw[m] * yw$num[m]); s2 <- sum(nw[m] * yw$num[m]^2)
        out$mean <- s1 / wn
        # match tab()/num_derive_stats: unweighted -> stats::var (n-1), weighted -> ML (s2/wn - mean^2)
        out$var  <- if (want_var) {
          if (is.null(wt)) (s2 - s1^2 / n1) / (n1 - 1) else round(s2 / wn - (s1 / wn)^2, 10)
        } else NA_real_
        # the numeric part re-derives its own effective n from the per-respondent weights: the mean
        # twin of the same closed form, s^2 / Var_design(x_bar).
        out$n_ci <- if (!weighted) as.double(n1) else {
          ne <- svy_flat_neff_rows(nw[m], yw$num[m], rep(1, sum(m)), n_obs, num = out$var)
          if (isTRUE(is.finite(ne) && ne > 0)) ne else as.double(n1)
        }
      }
      # z14-iii: the design supersedes it, per level, with Korn & Graubard's device -- the very rule
      # z14-ii writes into tab()'s own n_eff field. A level whose variance came back non-finite or
      # <= 0 keeps the flat / raw base rather than losing its interval.
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
      # keep the two identities the pre-z14-iii code had by construction: a numeric outcome has one
      # base, a categorical one without a mean column likewise.
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
    rprop  <- rep(unname(ref$prop), times = nl)
    meanv  <- rep_lv("mean"); rmean <- rep(ref$mean, nl * nc)
    n_ci   <- rep_lv("n_ci"); r_n_ci <- rep(ref$n_ci, nl * nc)
    # z14-iii: n_draw is per (level, CATEGORY) -- flat(), not rep_lv() -- so a design variance is not
    # averaged away. The reference twin repeats the reference LEVEL's vector once per level, i.e. it
    # pairs each cell with its OWN category. `n_ci` stays per level: a mean has no category.
    n_draw <- flat("n_draw"); r_n_draw <- rep(ref$n_draw, times = nl)
    # the crude ODDS ratio (category vs the reference CATEGORY, level vs the reference LEVEL) where the
    # outcome has categories; the crude RATE ratio (mean / reference mean) where it does not.
    # WARNING: the divisor is the reference LEVEL's own wpos/wneg, i.e. the SAME expression as the
    # numerator -- not the algebraically-equal `ref$prop / ref$prop[ref_cat]`, whose last bit differs
    # and made the reference cell print "1/1" (an OR of 1 - 1e-16 renders as its own reciprocal).
    emp_ratio <- if (has_cat) {
      (wpos / wneg) / rep(unname(ref$wpos / ref$wneg), times = nl)
    } else meanv / rmean
    pw <- if (has_cat) ci_wilson(prop, n_draw, conf_level = conf_level, df = degf) else
      list(inf = rep(NA_real_, nl * nc), sup = rep(NA_real_, nl * nc))
    # the family's DECLARED difference method, not a second hard-coded one: this interval's only
    # consumer is the multinomial html tooltip, and one quantity may not carry two methods inside one
    # table. The difference from tab(ci = "diff")'s Newcombe is deliberate -- the crude companion
    # matches the model AME's Wald, so the merged legend can name ONE method.
    dd <- if (has_cat) ci_prop_diff(prop, n_draw, rprop, r_n_draw, conf_level = conf_level,
                                    method = emp_method_diff, want_p = FALSE, df = degf) else pw
    tibble::tibble(
      var = p, level = rep(lv, each = nc), category = rep(cats, times = nl),
      emp_prop = prop, emp_prop_inf = pw$inf, emp_prop_sup = pw$sup,
      emp_diff = if (has_cat) prop - rprop else meanv - rmean,
      emp_diff_inf = dd$inf, emp_diff_sup = dd$sup,
      emp_ratio = emp_ratio, emp_ratio_prop = if (has_cat) prop / rprop else NA_real_,
      emp_wpos = wpos, emp_wneg = wneg,
      emp_mean = meanv, emp_var = rep_lv("var"),
      emp_n    = as.integer(rep_lv("n")), emp_n_ci = n_ci, emp_n_draw = n_draw,
      emp_ref_n_draw = r_n_draw,
      emp_ref_prop = rprop, emp_ref_mean = rmean, emp_ref_var = rep(ref$var, nl * nc),
      emp_ref_n    = as.integer(rep(ref$n, nl * nc)), emp_ref_n_ci = r_n_ci
    )
  })
  # z16-iiiii: the degrade travels OUT with the grid it describes. reg_stage_crude() harvests it into the
  # basis it stamps on the columns ("design_partial"), so the fact reaches the footer without any
  # process-global state -- and a grid computed for one table cannot label another.
  structure(out, degrade = said)
}

# reg_empirical_fit() -- Phase 18z9 (numeric predictors) / z10 (ordinal outcomes): the crude
# companion of every predictor whose UNIVARIABLE model is NOT saturated, so no closed form exists.
#
# DESIGN -- the rule is the one the factor arm already applies, not a new one. "The observed effect is
# the UNIVARIABLE model's effect": when that model is saturated its coefficients ARE the weighted cell
# contrasts reg_empirical() computes in closed form; when it is not, we fit. Two cases are not:
#   * a NUMERIC predictor, in any family (one slope, not one contrast per level -- measured in
#     dev/numeric_predictors_crude_counterparts.md SS6: the closed-form substitutes are exact only for a
#     NORMAL predictor and degrade to 50-70 % error under skew);
#   * ANY predictor under an ORDINAL outcome, because proportional odds is a constraint (measured in
#     dev/model_vs_observed_gap_test.md SS13.2.3: the closed-form substitutes drift 2.4-5.4 %, and the
#     drift is the PO violation itself).
# reg_crude_saturated() below states exactly that, so the caller never re-derives it.
#
# Re-calling reg_fit() rather than hand-rolling is what makes the crude column structurally share the
# model's family, design, CI method, `outcome_level` and `multiplier` -- ruling Q6 by
# construction instead of by a rule someone must remember. `other_preds` become reg_fit()'s `drop_extra`,
# so each crude fit lands on EXACTLY the model's complete-case population -- the `empirical` contract,
# and the row identity the gap test needs.
#
# Always fitted on the NATIVE (link) scale: reg_wald_finalize() only exp()s estimate + bounds at the very
# end, so exp()ing here per requested shape is bit-identical to having passed do_exp = TRUE, and ONE fit
# then serves the exponentiated column, its log twin and the gap test.
#
# Returns list(est = <named by outcome category, "" when none> of tibble(row, est, lo, hi, p),
#              fits = <named by predictor> of list(fit, data)) -- `row` is the SKELETON row index, so
# the overlay needs no key arithmetic. A per-predictor failure drops that predictor, never the table.
#
# WARNING: messages are suppressed. Every message a crude fit can emit (the profile-method fallbacks, the
# binary recode) was already emitted by the model fit on the same data, family and method -- so p
# predictors would repeat it p times, saying nothing new.
#' @keywords internal
reg_crude_saturated <- function(crude_key, is_factor)
  isTRUE(is_factor) && !identical(crude_key, "ordinal")

#' @keywords internal
reg_empirical_fit <- function(data, preds, outcome, family, design_spec, outcome_level,
                              conf_level, method, skeleton, multiplier = NULL,
                              other_preds = character(0), est = NULL, wt = NULL,
                              want_fit = FALSE, marginal = FALSE, trials = NULL,
                              shape_terms = NULL) {
  if (length(preds) == 0L) return(list(est = list(), fits = list()))
  # Phase 19e: "is the marginal contrast a RATIO?" is the estimand row's own `comparison`.
  ratio  <- !is.null(est) && identical(est$comparison, "lnratioavg")
  skey   <- reg_skel_key(skeleton$var, skeleton$level)
  rows   <- list()
  fits   <- list()
  for (v in preds) {
    f <- tryCatch(
      # Phase 18z15: the crude fit takes the SAME shape as the model's (`add_terms`), so a curved
      # predictor's two rows both get an observed twin and its term names are IDENTICAL to the model's
      # -- which is the whole reason the alignment below needs no shape-aware branch.
      suppressMessages(reg_fit(data, outcome, v, family, design_spec, do_exp = FALSE,
                               outcome_level, conf_level, method,
                               trials = trials, formula = NULL, multiplier = multiplier,
                               drop_extra = setdiff(other_preds, v),
                               add_terms = reg_shape_add(shape_terms, v))),
      error = function(e) NULL)
    if (is.null(f)) next
    if (want_fit) fits[[v]] <- list(fit = f$fit, data = f$data)
    if (!marginal) {
      # coefficient scale: align the univariable fit's terms to the skeleton exactly as the model column
      # does (skeleton$term == the model-matrix column name, which broom::tidy() reproduces).
      td <- f$tidy[!is.na(f$tidy$term) & f$tidy$term %in% skeleton$term[skeleton$var == v], ,
                   drop = FALSE]
      if (!nrow(td)) next
      idx <- match(td$term, skeleton$term)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        category = "", row = idx, est = td$estimate, lo = td$conf.low, hi = td$conf.high,
        p = td$p.value)
    } else {
      # `at = "average"` always: the crude effect is a whole-sample quantity, exactly as the factor arm's
      # weighted cell contrasts are. (`at = "reference"` attaches no `obs` anyway -- set_obs_if's gate.)
      m <- tryCatch(suppressMessages(reg_marginal(
        f$fit, f$data, v, conf_level, wt, at = "average",
        comparison = if (ratio) "lnratioavg" else NULL, want_pred = FALSE,
        multiplier = multiplier,
        engine = if (is.null(est)) "marginaleffects" else reg_marginal_engine(est))),
        error = function(e) NULL)
      if (is.null(m) || !nrow(m$ame)) next
      a <- m$ame[m$ame$var == v, , drop = FALSE]
      if (!nrow(a)) next
      idx <- match(reg_skel_key(a$var, a$level), skey)
      ok  <- !is.na(idx)
      if (!any(ok)) next
      a <- a[ok, , drop = FALSE]; idx <- idx[ok]
      # ⚠ NOT `est`: that is this function's ESTIMAND-ROW argument, read again on the next predictor
      # (reg_marginal_engine(est)). Clobbering it made every predictor after the first abort inside the
      # tryCatch above and silently lose its crude column -- latent until something read `est` twice.
      e_v <- a$ame; lo <- a$ame_lo; hi <- a$ame_hi
      # reg_marginal() exp()s a log-ratio before returning, so log it back: this function's contract is
      # the NATIVE (link) scale, and reg_fit_overlay() re-exponentiates per the shape's own scale.
      if (ratio) { e_v <- log(e_v); lo <- log(lo); hi <- log(hi) }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        category = ifelse(is.na(a$group), "", a$group), row = idx,
        est = e_v, lo = lo, hi = hi, p = a$ame_p)
    }
  }
  if (!length(rows)) return(list(est = list(), fits = fits))
  all <- vctrs::vec_rbind(!!!rows)
  list(est = split(all[setdiff(names(all), "category")], all$category), fits = fits)
}


# reg_fit_overlay() -- write fit-derived crude rows into a
# finished crude EFFECT column and into the crude effect VECTOR, at the ONE point both are in hand.
#
# The estimate lands in the field its `scale` declares (fmt_center_field()'s rule), exp()d exactly
# when that scale is multiplicative -- which is also what tells this function whether the shape is an
# exponentiated effect or its log twin. `n` is deliberately left NA: like the model column's, a
# fit-derived row's base is the whole model N, which belongs in the footer, not in a per-cell "n:".
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
# that mirrors each model estimand.
#
# ONE COLUMN, BUILT TWICE. The crude and the modelled effect are the same estimand computed with one
# predictor and with all of them, so they are one column SHAPE: same stored scale, same colour
# measure, same display, same digits, same reference. Only the ESTIMATION differs. That is why this
# table declares neither a colour (the crude column takes the model column's, both channels) nor a
# display (the table's resolved one) nor a second "base" row for the level: the level a cell prints
# beside its estimate is `EST_SCALES$<scale>$base_display`, declared once on the scale, and it rides
# in the crude cell exactly as the adjusted prediction rides in the model cell.
#
# COLUMNS
#   word        the base measure acronym (a REG_WORDS key), from which reg_crude_word() composes the
#               column's name -- "Obs_RR", "Obs_log(OR)". It is the SHAPE's own acronym, never the
#               model column's: a poisson AME sits beside a crude rate RATIO, and "Obs_IRR" is what
#               that column holds. It carries no contrast marker either -- see reg_crude_word().
#               Per-category shapes disambiguate the name by outcome category at assembly, where the
#               model column's own label is in hand.
#   scale       the EST_SCALES row, hence the estimate's field, ladder, glyphs and level token.
#   ref         the reference marker ("1" multiplicative, "tot" additive, NA none).
#   ci_method   the interval engine, under the table's `n` basis...
#   ci_method_design  ...and under a weights / design basis. The crude column is the univariable
#               model's column, so its interval must be that model's: unweighted the fit is lm / glm
#               and the interval is MODEL-BASED (one dispersion pooled over the predictor's levels);
#               weighted it is svyglm and the interval is the SANDWICH, which the per-group forms
#               reproduce. Absent = the same interval either way.
#   link        the crude estimator's link, for the gap SE's influence function (g'(mu)). NA where
#               the shape is not an effect.
#   per_category  one crude effect per OUTCOME category (multinomial, ordinal-marginal): the crude
#               value rides in the model cell unless `empirical = "column"` asks for the columns.
#
# A family's own scalars: `coef` / `coef_log` name its coefficient-scale shape and the logged twin
# used when the model is not exponentiated, and `method_diff` the risk-difference engine.
REG_EMPIRICAL <- list(
  binomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    ame    = list(word = "RD",  scale = "points",     ref = "tot",         ci_method = "wald",  link = "identity"),
    or     = list(word = "OR",  scale = "odds_ratio", ref = "1",           ci_method = "woolf", link = "logit"),
    or_log = list(word = "OR",  scale = "log_coef",   ref = NA_character_, ci_method = "woolf", link = "logit")),
  # the modified-Poisson (binary outcome) companion: the crude RISK ratio with the KATZ log-RR
  # interval, not the Woolf log-OR the binomial arm uses -- the observed companion must be on the
  # same scale as the model column.
  rr = list(
    method_diff = "wald", coef = "rr", coef_log = "rr_log",
    ame    = list(word = "RD",  scale = "points",     ref = "tot",         ci_method = "wald", link = "identity"),
    rr     = list(word = "RR",  scale = "pct_ratio",  ref = "1",           ci_method = "katz", link = "log"),
    rr_log = list(word = "RR",  scale = "log_coef",   ref = NA_character_, ci_method = "katz", link = "log")),
  # a RATIO OF MEANS (`measure = "ratio"` on a continuous outcome, the "mr" log-link pseudo-ML fit).
  mr = list(
    coef = "mr", coef_log = "mr_log",
    mr     = list(word = "RoM", scale = "mean_ratio", ref = "1",           ci_method = "quasipoisson", ci_method_design = "robust", link = "log"),
    mr_log = list(word = "RoM", scale = "log_coef",   ref = NA_character_, ci_method = "quasipoisson", ci_method_design = "robust", link = "log")),
  gaussian = list(
    coef = "diff", coef_log = "diff",
    diff = list(word = "diff", scale = "raw_diff", ref = NA_character_, ci_method = "ols", ci_method_design = "welch", link = "identity")),
  # ⚠ it declares an ADDITIVE shape too: a poisson marginal effect is a difference of expected
  # COUNTS, and its crude counterpart is simply the observed difference of mean counts -- the same
  # estimand with one predictor. Measured on `tvhours ~ race`: the closed form reproduces the
  # univariable poisson AME to 2e-12, and `welch` its ROBUST interval to 1.5e-03 (the model-based one
  # is 49 % away -- that gap is the over-dispersion, and tab_reg dispersion-scales its poisson SEs,
  # so the sandwich is the target in BOTH bases; that is what makes it differ from gaussian's `ols`).
  poisson = list(
    coef = "irr", coef_log = "irr_log",
    irr     = list(word = "IRR",  scale = "mean_ratio", ref = "1",           ci_method = "quasipoisson", ci_method_design = "robust", link = "log"),
    irr_log = list(word = "IRR",  scale = "log_coef",   ref = NA_character_, ci_method = "quasipoisson", ci_method_design = "robust", link = "log"),
    diff    = list(word = "diff", scale = "raw_diff",   ref = NA_character_, ci_method = "welch", ci_method_design = "welch", link = "identity")),
  # grouped_binomial (`trials =`): the univariable model is still saturated for a factor predictor,
  # so the crude OR is the Woolf 2x2 on the SUMMED counts. Its level is the share of "yes" among the
  # draws. Its LEVEL is the mean SCORE -- the average number of "yes" out of `trials`, which is the
  # quantity a reader of a battery of items wants -- so its odds ratio takes `score_ratio`, the
  # one scale whose estimate is an odds ratio and whose level is a mean.
  # ⚠ it declares its own `rr` / `rr_log`: a summed score's RISK ratio is the ratio of the two
  # groups' mean SCORES (Katz on the draw counts), not the respondent-level one REG_EMPIRICAL$rr
  # holds -- so the estimand's `crude_fam = "rr"` borrow must land here, which reg_crude_shape()
  # enforces.
  grouped_binomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    ame    = list(word = "RD",  scale = "raw_diff",    ref = NA_character_, ci_method = "welch", ci_method_design = "welch", link = "identity"),
    or     = list(word = "OR",  scale = "score_ratio", ref = "1",           ci_method = "woolf", link = "logit"),
    or_log = list(word = "OR",  scale = "log_coef",    ref = NA_character_, ci_method = "woolf", link = "logit"),
    rr     = list(word = "RR",  scale = "score_ratio", ref = "1",           ci_method = "katz",  link = "log"),
    rr_log = list(word = "RR",  scale = "log_coef",    ref = NA_character_, ci_method = "katz",  link = "log")),
  # multinomial: one crude effect PER OUTCOME CATEGORY. The univariable multinomial is saturated, so
  # its OR is the {j, ref} x {level, ref level} Woolf ratio -- the number tab(pct = "row") prints.
  multinomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    or        = list(word = "OR", scale = "odds_ratio", ref = "1",           ci_method = "woolf", link = "logit",    per_category = TRUE),
    or_log    = list(word = "OR", scale = "log_coef",   ref = NA_character_, ci_method = "woolf", link = "logit",    per_category = TRUE),
    ame       = list(word = "RD", scale = "points",     ref = "tot",         ci_method = "wald",  link = "identity", per_category = TRUE),
    ame_ratio = list(word = "RR", scale = "pct_ratio",  ref = "1",           ci_method = "katz",  link = "log",      per_category = TRUE)),
  # ordinal: proportional odds is a CONSTRAINT, so the univariable model is NOT saturated and there
  # is no closed form -- the drift of the substitutes IS the PO violation, which would land in a
  # measure whose whole job is to say how far the model moved the effect. Hence a univariable
  # polr / svyolr through reg_fit(), where "same estimand, link, CI rule, multiplier" holds by
  # construction. A cumulative odds ratio has no single share to print beside it: the level field
  # stays empty and `{base}` renders void.
  ordinal = list(
    coef = "cumor", coef_log = "cumor_log",
    cumor     = list(word = "cumOR", scale = "odds_ratio", ref = "1",           ci_method = "wald_log", link = "logit"),
    cumor_log = list(word = "cumOR", scale = "log_coef",   ref = NA_character_, ci_method = "wald_log", link = "logit"),
    ame       = list(word = "RD",    scale = "points",     ref = "tot",         ci_method = "wald",     link = "identity", per_category = TRUE),
    ame_ratio = list(word = "RR",    scale = "pct_ratio",  ref = "1",           ci_method = "wald_log", link = "log",      per_category = TRUE))
)

#' @keywords internal
shape_per_category <- function(shape) isTRUE(shape$per_category)

# reg_crude_word() / reg_crude_col_name() -- what a crude column is CALLED.
#
# THE CRUDE COLUMN IS NEVER MARKED. It names the measure and nothing else, because one crude column
# holds two kinds of row: the levels the closed form covers (saturated, where a marginal and a
# conditional contrast are the same number, so a marker would assert a distinction that does not
# exist) and the rows a univariable refit fills (numeric predictors; every predictor under an ordinal
# outcome), which do carry one. One header cannot say both, so it says the measure and the "Model:"
# footer says the rest. It also keeps `Obs_*` names stable across `effect =`.
# The log wrapper IS composed, from the shape's own scale -- a link-scale shape is a logged estimand.
#' @keywords internal
reg_crude_word <- function(shape) {
  if (is.null(shape) || is.null(shape$word)) return(NA_character_)
  reg_word_logged(shape$word, if (identical(shape$scale, "log_coef")) "log" else "")
}

#' @keywords internal
reg_crude_col_name <- function(shape) {
  w <- reg_crude_word(shape)
  if (is.na(w)) NA_character_ else paste0("Obs_", w)
}

# reg_crude_shape() -- THE reader of "which REG_EMPIRICAL row describes the crude EFFECT of this
# estimand?". The SELECTION is the estimand row's own two columns (`crude_fam` / `crude_shape`), so
# the cross-family borrow (a binary marginal RATIO reuses REG_EMPIRICAL$rr$rr) and "a family with no
# marginal crude falls back to its coefficient row" are DECLARED per cell rather than inferred here.
# `crude_fam = "auto"` means "the outcome's own block", which is what carries `trials` ->
# grouped_binomial through reg_crude_key().
#
# ⚠ IT IS THE ONLY READER. reg_empirical_columns() used to re-derive the shape in three of its arms,
# from (marginal, do_exp) instead of from the declaration -- which is how a gaussian marginal RATIO
# drew a mean-DIFFERENCE crude column beside a ratio model column, and a poisson AME drew the LOGGED
# rate ratio. The arms call this function now; nothing else may pick a shape.
#' @keywords internal
reg_crude_shape <- function(crude_key, est = NULL) {
  if (is.null(est)) est <- list(crude_fam = "auto", crude_shape = NA_character_)
  # ⚠ the SUMMED-SCORE block wins over the estimand's borrow, and the order is load-bearing (the same
  # rule reg_crude_key() states): `rr` is an INDIVIDUAL-level block, while a score's crude effect
  # sits on the mean SCORE. Taking `crude_fam = "rr"` literally there would hand a battery of items
  # the respondent-level risk ratio.
  key <- if (identical(crude_key, "grouped_binomial")) crude_key
         else if (!identical(est$crude_fam %||% "auto", "auto")) est$crude_fam
         else crude_key
  fam <- if (is.null(key) || is.na(key)) NULL else REG_EMPIRICAL[[key]]
  if (is.null(fam)) return(NULL)
  sh <- est$crude_shape
  if (is.null(sh) || is.na(sh)) sh <- fam$coef
  fam[[sh]] %||% fam[[fam$coef]]     # a shape a block does not declare falls back to its own coefficient
}

# WARNING: `l[[""]]` is a subscript-out-of-bounds ERROR in R, not a miss -- and "" is exactly the key a
# single-column fit uses. Every lookup into a category-keyed list goes through this.
#' @keywords internal
cat_get <- function(l, key) {
  if (is.null(l) || !length(l)) return(NULL)
  i <- match(if (is.null(key)) "" else as.character(key), names(l))
  if (is.na(i)) NULL else l[[i]]
}

# THE CRUDE COLUMN: one fmt column per effect, aligned to the skeleton, for reg_stage_assemble() to
# splice ahead of the model columns it serves. The Constant -> empty cells; reference levels ->
# neutral + in_refrow, no CI. The p-value is always stored (stars are stripped post-build when
# `stars = FALSE`, like the model columns).
#
# ONE COLUMN, NOT TWO. The crude column carries the crude EFFECT and, in the same cell, the level it
# sits on -- the observed % or mean, in the field its own scale names (`{base}`). It is the model
# column's mirror, so it takes that column's colour measure and the table's display, and it carries
# exactly ONE interval: the effect's. What it must never carry is `obs` or `gap_se`: it IS the
# observed value, and a column cannot be its own baseline.
#
# The crude effect also travels back as a VECTOR keyed by OUTCOME CATEGORY ("" where the outcome has
# none), because a multinomial / ordinal-marginal model has one column per category and each needs
# its own `obs`; reg_set_obs() looks the column's stored `emp_key` up in it. `emp_mode` decides
# whether those per-category effects also draw columns of their own ("column") or ride in the model
# cell ("cell").
#
# `fit_est` (reg_empirical_fit()'s per-category estimates) fills the rows no closed form covers --
# numeric predictors in any family, and EVERY predictor under an ordinal outcome.
reg_empirical_columns <- function(skeleton, emp, fac_preds, crude_key, family, est, var_y,
                                  conf_level = 0.95, color_signif = "grey_non_signif",
                                  color = NULL, fit_est = NULL, weighted = FALSE,
                                  degf = Inf, emp_mode = "column") {
  fam <- REG_EMPIRICAL[[crude_key]]
  if (is.null(fam)) return(list(cols = list(), cat_cols = list(), effect = NULL, shape = NULL))
  # Phase 19e: the three facts the arms below branch on, read off the ESTIMAND row rather than from
  # the (effect, do_exp) pair -- so the crude companion cannot describe a different estimand from
  # the model column it sits beside.
  marginal   <- !identical(est$effect, "coefficient")
  ratio_marg <- marginal && identical(est$comparison, "lnratioavg")
  # THE crude shape, resolved ONCE from the declaration (reg_crude_shape) and read by every arm.
  # ⚠ never re-derived from (marginal, do_exp): the estimand row already says which crude effect
  # pairs with it, including its cross-family borrows and its logged twins.
  shape      <- reg_crude_shape(crude_key, est)
  if (is.null(shape)) return(list(cols = list(), cat_cols = list(), effect = NULL, shape = NULL))
  # THE COLOUR IS THE MODEL COLUMN'S, both channels. One comparison, one ladder, one legend block:
  # the crude column grades the same quantity, so it cannot be graded on another scale. `color = FALSE`
  # then turns both off with no special case. Under a GAP measure the crude column is the baseline --
  # its `obs` is empty, so the engine leaves it uncoloured by construction -- and it is marked
  # `refcol` so the reference subsystem bolds it and the legend names it as what the shades compare to.
  emp_color <- if (is.null(color)) "" else color
  # ⚠ only an "observed" baseline makes THIS column the reference: `adjustment` compares each model
  # cell to the crude one beside it, so the crude column is what the shades are measured from.
  # `between_groups` compares a cell to the same cell in another GROUP, where the crude column has a
  # real counterpart of its own and is graded like any other.
  gap_base  <- any(vapply(emp_color, function(k) {
    m <- measure_key(k)
    !is.na(m) && nzchar(m) && identical(MEASURES[[m]]$ref_kind, "observed")
  }, logical(1)))
  emp_signif <- if (any(nzchar(emp_color) & !is.na(emp_color))) color_signif else "ignore"
  n_rows  <- nrow(skeleton)
  is_fac  <- skeleton$var %in% fac_preds
  # Phase 18z9 (dev/numeric_predictors_crude_counterparts.md SS11.1): the Constant is a reference row
  # HERE TOO. reg_column() flags it (`... | var == "Constant"`) and tab_bold_rows() ANDs the flag across
  # every discriminating column, so leaving it out of the crude column silently un-bolded the Constant of
  # every `empirical = TRUE` table. Blanking its CI is a no-op: the Constant has no crude counterpart, so
  # all its crude fields are already NA.
  refrows <- (skeleton$is_ref & is_fac) | skeleton$var == "Constant"
  # a reference level has no CI/test against itself (like the model column's zeroed reference).
  na_ref <- function(ci) { ci$inf[refrows] <- NA_real_; ci$sup[refrows] <- NA_real_
                           ci$pvalue[refrows] <- NA_real_; ci }
  na_v   <- function() rep(NA_real_, n_rows)
  # THE crude interval a shape asks for, under THIS table's inference basis (see REG_EMPIRICAL).
  emp_method <- function(shape)
    (if (isTRUE(weighted)) shape$ci_method_design %||% shape$ci_method else shape$ci_method) %||% ""
  # one fmt column from a shape row + its varying fmt FIELD values. `display` is left to the table's
  # own resolution (reg_apply_display), `ref` omitted when the shape has none.
  emp_col <- function(shape, fields, n_eff = NULL) {
    args <- c(fields, if (!is.null(n_eff)) list(n_eff = n_eff), list(
      scale = shape$scale, pct_type = reg_pct_type(shape$scale),
      digits = reg_cell_digits(shape$scale),
      display = "est",
      ci_method = emp_method(shape),
      color = emp_color, color_signif = emp_signif, refcol = gap_base,
      col_var = reg_crude_col_name(shape), comp_all = FALSE, in_refrow = refrows,
      model_family = family, role = "emp"))
    if (!is.na(shape$ref)) args$ref <- shape$ref
    do.call(fmt, args)
  }
  # the effective base the interval was ACTUALLY computed on, stored in the `n_eff` field: the exact
  # flat closed form on the weights, or the design variance. NA when nothing corrected it, exactly as
  # an unweighted tab() cell carries NA. Each arm passes its OWN base -- the number of Bernoulli
  # DRAWS for a proportion / odds / risk ratio, the per-respondent n for a mean and its ratios.
  neff_of <- function(v) if (isTRUE(weighted)) as.double(v) else rep(NA_real_, n_rows)
  # emit() -- the finished column, plus the crude EFFECT VECTOR that travels back to reg_set_obs()
  # to become the model column's `obs` (what `color = "adjustment"` grades and `{obs}` prints). The
  # vector is taken from the local the column was built from, never re-read out of the fmt column,
  # and the effect SHAPE ROW travels with it so the gap test has both facts it needs: `link` (the
  # crude estimator's) and `scale` (proof the two columns are the same estimand).
  #
  # The fit-derived rows are spliced here, the one place the effect shape is known (reg_fit_overlay).
  # `cat` is the outcome category feeding this call; the returned lists are keyed by the MODEL
  # COLUMN's own category (`emp_key`), which is "" for every family producing one column -- including
  # the binary ones, whose grid slice is category "1".
  #
  # A per-category shape returns its column under `cat_cols` instead of `cols`: there is one per
  # outcome category, so the assembler names each from the model column it mirrors. Under
  # `emp_mode = "cell"` no column is drawn at all and only the effect travels.
  emit <- function(eff, cat = "") {
    if (is.null(eff)) return(list(cols = list(), cat_cols = list(), effect = NULL, shape = NULL))
    per_cat <- shape_per_category(eff$shape)
    key  <- if (per_cat) cat else ""
    o    <- reg_fit_overlay(eff$col, eff$vec, cat_get(fit_est$est, key), eff$shape)
    draw <- !identical(emp_mode, "cell")
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

  # THE LEVEL a crude cell prints beside its estimate: the field the column's own scale names
  # (EST_SCALES$base_display), declared once there rather than per family. NA on a link scale and on
  # a cumulative odds ratio, where no single level belongs beside the estimate -- `{base}` then
  # renders void.
  with_base <- function(sh, fields, level) {
    b <- EST_SCALES[[sh$scale]]$base_display
    if (is.na(b)) return(fields)
    c(fields, stats::setNames(list(level), b))
  }
  # the SD-standardized ladder's divisor, on the one scale that declares it (raw_diff): the model
  # column carries var(Y) there and so must its mirror, which is also why a crude column cannot carry
  # the LEVEL's own variance in the same field.
  with_var <- function(sh, fields)
    if (identical(EST_SCALES[[sh$scale]]$sd_from %||% "", "var"))
      c(fields, list(var = rep(var_y, n_rows))) else fields

  # ---- ordinal: proportional odds has no closed form, so every row comes from a univariable fit ----
  if (identical(crude_key, "ordinal")) {
    # ⚠ the ESTIMATE has no closed form here (proportional odds is a constraint, so the univariable
    # model is not saturated) and is spliced in by reg_fit_overlay. The LEVEL beside it has one: it
    # is the plain observed share of that outcome category, which the grid already holds -- so the
    # cell carries it, exactly as every other crude cell does.
    empty <- function(sh, g = NULL) {
      fields <- list(diff = na_v(), n = if (is.null(g)) rep(NA_integer_, n_rows) else g$emp_n)
      emp_col(sh, if (is.null(g)) fields else with_base(sh, fields, g$emp_prop))
    }
    if (marginal) {
      cats <- names(fit_est$est)
      if (!length(cats)) return(list(cols = list(), cat_cols = list(), effect = NULL, shape = shape))
      out  <- purrr::map(stats::setNames(nm = cats), function(k)
        emit(list(col = empty(shape, cat_of(k)), vec = na_v(), shape = shape), k))
      return(list(cols = list(), cat_cols = purrr::flatten(purrr::map(out, "cat_cols")),
                  shape = shape, effect = purrr::flatten(purrr::map(out, "effect"))))
    }
    # a CUMULATIVE odds ratio has no single share to sit on: `{base}` stays void by construction.
    return(emit(list(col = empty(shape), vec = na_v(), shape = shape)))
  }

  # ---- the probability families: one closed form, evaluated per OUTCOME CATEGORY ------------------
  #
  # binomial / rr / grouped_binomial have one category (the positive one); multinomial has several,
  # and its crude effect is the SAME arithmetic per category -- the {j, ref} x {level, ref level}
  # 2x2. So the arm below serves both, keyed on the SHAPE (its scale and its declared CI engine)
  # rather than on the family: a risk difference is a Wald difference of proportions, a Woolf odds
  # ratio the 2x2 legs, a risk ratio Katz's, and a logged twin the log of whichever it twins.
  #
  # WARNING: `emp_ratio` is an ODDS ratio and `emp_prop / emp_ref_prop` a RISK ratio. Feeding one to
  # the other's header is the whole point of dispatching on the shape.
  prob_effect <- function(sh, g) {
    prop <- g$emp_prop; rprop <- g$emp_ref_prop
    # the LEVEL beside the estimate is the field the scale names, and the grid holds both halves: a
    # share for a probability scale, the mean SCORE for a summed-score one.
    level <- if (identical(EST_SCALES[[sh$scale]]$base_display, "mean")) g$emp_mean else prop
    ndr  <- g$emp_n_draw; rndr <- g$emp_ref_n_draw
    logged <- identical(sh$scale, "log_coef")
    # the exponentiated twin a logged shape logs: same estimand, same interval, on the link scale.
    base_sh <- if (logged) fam[[fam$coef]] else sh
    # dispatch on the declared GEOMETRY rather than on the scale's name
    if (identical(EST_SCALES[[base_sh$scale]]$geometry, "difference")) {
      v  <- g$emp_diff
      ci <- na_ref(ci_prop_diff(prop, ndr, rprop, rndr, conf_level = conf_level,
                                method = fam$method_diff %||% "wald", want_p = TRUE, df = degf))
    } else if (identical(emp_method(base_sh), "katz") ||
               identical(base_sh$ci_method, "katz")) {
      v  <- prop / rprop
      ci <- na_ref(ci_katz_rr(prop, ndr, rprop, rndr, conf_level = conf_level,
                              want_p = TRUE, df = degf))
    } else {
      # the SS14 rule: WEIGHTED proportion x UNWEIGHTED base, so the base cancels out of the log-OR.
      # For a grouped binomial that base counts DRAWS (n x trials), which is what makes the crude OR
      # equal a univariable glm(cbind(s, q - s) ~ x) rather than an OR on respondent counts.
      v  <- g$emp_ratio
      ci <- na_ref(ci_or(prop * ndr, (1 - prop) * ndr, rprop * rndr, (1 - rprop) * rndr,
                         conf_level = conf_level, want_p = TRUE, df = degf))
    }
    if (logged) { ci$inf <- log(ci$inf); ci$sup <- log(ci$sup); v <- log(v) }
    est_fld <- EST_SCALES[[sh$scale]]$est_field
    fields  <- c(stats::setNames(list(v), est_fld),
                 list(n = g$emp_n, tot_n = g$emp_n,
                      ci_inf = ci$inf, ci_sup = ci$sup, pvalue = ci$pvalue))
    list(fields = with_var(sh, with_base(sh, fields, level)), vec = v, n_eff = g$emp_n_draw)
  }

  if (identical(crude_key, "multinomial")) {
    sh   <- shape
    cats <- unique(emp$category)
    if (!length(cats)) return(list(cols = list(), cat_cols = list(), effect = NULL, shape = sh))
    out <- purrr::map(stats::setNames(nm = cats), function(k) {
      pe <- prob_effect(sh, cat_of(k))
      # a MARGINAL contrast is not the closed form's: the ratio one is a ratio of the two observed
      # shares, the additive one their difference. Both are already in the grid.
      if (marginal) {
        g  <- cat_of(k)
        v  <- if (ratio_marg) g$emp_ratio_prop else g$emp_diff
        pe$fields[[EST_SCALES[[sh$scale]]$est_field]] <- v
        pe$vec <- v
      }
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
  # Phase 18s: the CI base is the effective n -- the exact flat closed form on the weights, or the
  # design variance; unweighted it equals the raw count, so those intervals are byte-identical. The
  # displayed n/tot_n fields always keep the raw count `nv`.
  nv_ci <- g$emp_n_ci; rn_ci <- g$emp_ref_n_ci
  # the model-based dispersion the two MOMENT families need, pooled over each predictor's own level
  # set -- exactly the univariable lm / glm's scope. The engines are elementwise, so it is computed
  # here, where the level set IS a group of skeleton rows. NULL where the chosen method is a
  # per-group (sandwich) one, which needs no pooling.
  emp_pool <- function(shape, kind) {
    if (!identical(emp_method(shape), CI_POOLED[[kind]])) return(NULL)
    ci_pool_disp(n = nv_ci, mean = meanv, var = varv, by = skeleton$var, use = is_fac, kind = kind)
  }

  # ⚠ a SUMMED SCORE's difference is a difference of mean SCORES, not of per-item proportions -- so it
  # takes the MOMENT arm below, on `emp_mean` / `emp_var`, which the grid already holds in score
  # units. Only its multiplicative shapes are probability arithmetic.
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
    emit(list(col = emp_col(sh, with_var(sh, with_base(sh, fields, meanv)),
                            n_eff = neff_of(nv_ci)),
              vec = v, shape = sh), cat1)
  }

  # ---- ONE moment arm: a mean difference, a ratio of means, a rate ratio, and their logged twins.
  #
  # The three outcome blocks (gaussian / mr / poisson) share the same sufficient statistics, so what
  # picks the ENGINE is the declared shape's own scale, not the family: a gaussian outcome asked for
  # a marginal RATIO takes REG_EMPIRICAL$mr's shape and the ratio engine, which is what puts the
  # crude column on the model column's scale instead of drawing a mean difference beside a ratio.
  if (crude_key %in% c("gaussian", "mr", "poisson", "grouped_binomial")) {
    # a link-scale shape in a moment family is always the log of a RATIO (a log needs something
    # multiplicative to take the log of), and it declares the same interval engine as the twin it
    # logs -- so the engine is chosen from this shape alone.
    logged <- identical(shape$scale, "log_coef")
    if (!logged && identical(shape$scale, "raw_diff")) {
      md <- na_ref(ci_mean_diff2(meanv, varv, nv_ci, rmean, rv, rn_ci,
                                 method = emp_method(shape), conf_level = conf_level,
                                 want_p = TRUE, df_design = degf,
                                 pool = emp_pool(shape, "mean_diff")))
      # ⚠ the estimate must come from the SAME statistics as its interval: `meanv - rmean`, not the
      # grid's `emp_diff`. They are the same number wherever the grid's y IS the outcome (gaussian,
      # poisson), and they differ on a summed score, where `emp_mean` is the mean SCORE while
      # `emp_diff` is the per-item proportion difference -- which is exactly the unit the interval
      # below is NOT in.
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

# === the model-vs-observed GAP standard error (Phase 18z8-B) =====================================

# reg_same_estimand() -- do the crude companion and the model column measure the SAME thing? Both
# declare their SCALE (the shape row's, the column attribute's), so this is one fact comparison rather
# than a family/effect table kept in sync. It gates BOTH `obs` and its gap SE, which closes a z5
# defect: reg_empirical_columns() ignores `effect` on the poisson branch, so effect = "ame" paired an
# ADDITIVE count AME with the crude rate RATIO and z5 scored the difference of two scales. Phase 19b
# made the comparison strictly sharper -- `points` vs `raw_diff` used to be one value ("diff"), and
# they are two genuinely different estimands (percentage points vs the outcome's own units).
#' @keywords internal
reg_same_estimand <- function(shape, col)
  !is.null(shape) &&
  identical(as.character(shape$scale)[1], as.character(get_scale(col))[1])

# reg_same_frame() -- were the model and its crude companion fitted on the SAME observations? The crude
# frame drops on `union_predictors` (+ whatever `na` shares), the model on its own predictors; the first
# is a SUBSET of the second, so equal row counts PROVE row identity -- both come from
# reg_complete_frame()'s drop_na, which preserves order.
#
# Phase 18z13 (D1): the TWIN of reg_same_estimand, and it gates the same two things. It used to be an
# inline clause in reg_gap_se_columns only, so a model fitted on more rows than the observed block got no
# TEST but kept its descriptive COLOUR -- the code knew the two numbers were not comparable and coloured
# their difference anyway. Under the default `na = "drop_by_outcome"` it is true by construction; it
# still fires under `na = "drop_by_model"` and on the compound-formula path, which is exactly where a
# colour would otherwise assert an adjustment that is really listwise deletion.
#
# `nobs` IS nrow(mdata) on every fit record (reg_fit and its 3+ level siblings all set it that way), and
# it survives the jamovi digest path where the fitted object -- and with it `$data` -- was distilled
# away. Reading it there keeps `obs` (hence the `{obs}` display and its tooltip) alive in the live UI,
# where the frames cannot differ anyway: `reref` requires a single non-comparison model, so its
# predictors ARE the union.
#' @keywords internal
reg_same_frame <- function(mdata, f) {
  n_fit <- if (!is.null(f$data)) nrow(f$data) else f$nobs
  !is.null(n_fit) && !is.na(n_fit) && identical(as.integer(nrow(mdata)), as.integer(n_fit))
}

# reg_gap_se_columns() -- the standard error of the gap between ONE fit's effect and its observed
# (crude) counterpart, per skeleton row, so `color = "adjustment"` reads `color_signif` like every other
# measure. The maths lives in R/reg-influence.R; this is the gate and the loop.
#
# DESIGN -- the gate is five CORRECTNESS facts, each already stored somewhere, and it returns NULL
# rather than a partial column: a gap SE without an honest premise is worse than none, because MEASURES'
# force_policy closure reads an all-NA `gap_se` as "no test here" and falls back to the descriptive
# reading.
# Phase 18z17 (ruling D2): the SIXTH clause -- `"adjustment" %in% sp$color` -- is GONE. It gated a
# fact on who asked to COLOUR it rather than on whether it is valid, which held while the colour engine
# was the only reader; forest_plot() is the second, and a user who built a table without
# `color = "adjustment"` then asked for the gap band got no band and no explanation. So `gap_se` is now
# written wherever `empirical = TRUE` produced a crude twin and the five clauses hold. The cost is small
# by construction: reg_empirical_fit() already FITS the univariable crude models when `empirical = TRUE`
# (`want_fit` only decided whether to keep them), so what is added is reg_coef_if_maker() +
# reg_if_se(), ~1/8 of a fit per column.
#   * `shape`             the crude twin's REG_EMPIRICAL row: absent = no observed effect at all
#                         (multinomial, ordinal, grouped binomial) -> `obs` is already NA.
#   * `f$fit`             NULL on the jamovi digest path, where the fitted object was distilled away.
#   * scale match         the crude and model columns must be the SAME estimand. This also closes a z5
#                         defect: reg_empirical_columns() ignores `effect` on the poisson branch, so
#                         effect = "ame" pairs an ADDITIVE count AME with a MULTIPLICATIVE crude rate
#                         ratio -- z5 wrote that ratio into `obs` and scored a gap between two scales.
#   * same frame          both estimators must solve their equations on the SAME observations
#                         (reg_same_frame, shared with the `obs` write itself since z13).
#   * collapsible         maintainer ruling Q1(b): a conditional odds ratio moves under adjustment even
#                         with zero confounding, so at survey sizes the test would be "significant"
#                         everywhere for a reason no reader would take it for (SS4.1-SS4.3).
# `method = "profile"` is deliberately NOT a clause: between_groups RECOVERS its SE from the printed
# interval and a profile bracket is not est +/- crit*se, but adjustment COMPUTES its own -- profile
# there only means the printed model CI and the gap test are different quantities (SS3.8, documented).
#' @keywords internal
reg_gap_se_columns <- function(f, sp, model_col, skeleton, shape, mdata, fac_preds,
                               est, wt, fits_crude = NULL, fit_preds = character(0),
                               multiplier = NULL, category = "") {
  # Phase 19e: the estimand ROW replaces the (effect, at) pair -- the profile axis is `at_reference`,
  # the marginal ratio is the row's own `comparison`.
  effect   <- est$effect
  marginal <- !identical(effect, "coefficient")
  ratio_m  <- marginal && identical(est$comparison, "lnratioavg")
  if (is.null(shape) || is.null(f$fit) || is.null(f$data))      return(NULL)
  if (isTRUE(sp$compound) || identical(effect, "at_reference")) return(NULL)
  if (!reg_same_estimand(shape, model_col))                     return(NULL)
  if (!reg_same_frame(mdata, f))                                return(NULL)
  # reg_estimand_collapsible() keeps its own vocabulary: "is the DISPLAYED estimand collapsible" is a
  # question about the contrast, and only a conditional (coefficient) odds ratio answers no.
  if (!reg_estimand_collapsible(sp$fit_family, effect))             return(NULL)
  # svyrecvar is the linearization estimator; a REPLICATE-weights design needs withReplicates instead,
  # so degrade rather than report a linearization variance for a design that did not ask for one.
  des <- if (inherits(f$fit, "svyglm")) f$fit$survey.design else NULL
  if (inherits(des, "svyrep.design"))                           return(NULL)

  coef_if <- reg_coef_if_maker(f$fit)
  if (is.null(coef_if)) return(NULL)
  # Phase 18z10: a 3+ level outcome shows ONE COLUMN PER CATEGORY, so its marginal influence function
  # is per category too (reg_ame_if_cat_maker); the single-equation one reads family()$mu.eta, which
  # multinom / polr do not have.
  per_cat  <- inherits(f$fit, "multinom") || inherits(f$fit, "polr")
  model_if <- if (marginal && per_cat)
    reg_ame_if_cat_maker(f$fit, f$data, wt, ratio = ratio_m, category = category)
  else if (marginal)
    reg_ame_if_maker(f$fit, f$data, wt, ratio = ratio_m, coef_if = coef_if)
  else coef_if
  # Phase 18z10: `category` is the outcome category THIS column shows (a multinomial / ordinal-marginal
  # fit owns one per category; "" elsewhere). The crude leg must be built around the SAME indicator the
  # crude estimate was -- reg_crude_yw() is the one description of that, so the closed form takes the
  # category rather than re-deriving a positive level.
  crude_if <- reg_crude_if_maker(mdata, sp$outcome, sp$crude_key, f$positive_level, wt, shape$link,
                                 trials = sp$trials, category = category, ref_category = f$y_ref)
  if (is.null(model_if)) return(NULL)

  n_rows  <- nrow(skeleton)
  out     <- rep(NA_real_, n_rows)
  ref_of  <- function(v) { r <- skeleton$level[skeleton$var == v & skeleton$is_ref]
                           if (length(r)) as.character(r[[1]]) else NA_character_ }
  in_mod  <- skeleton$var %in% sp$predictors
  # WARNING: one length-n difference vector at a time -- never an n x p matrix of them (SS8).
  # a predictor whose univariable model is not saturated has no closed-form crude leg -- its rows are
  # covered by the fit arm below instead (z10: every predictor under an ordinal outcome).
  closed_form <- !is.null(crude_if) && reg_crude_saturated(sp$crude_key, TRUE)
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
    # z14-iii: the crude leg lives on `mdata`, the model leg on the fit's row space -- the same thing
    # except on a calibrated / PPS design, which svy_domain_design() pads back to full length.
    ic <- reg_if_align(crude_if(v, as.character(skeleton$level[k]), r), length(im),
                       mdata[[svy_row_col]])
    if (is.null(ic) || length(ic) != length(im)) next
    out[k] <- reg_if_se(im - ic, des)
  }

  # Phase 18z9 -- the NUMERIC arm. Same two legs, different crude side: a numeric predictor has no
  # cells, so reg_crude_if_maker()'s indicator arithmetic cannot serve it and the crude influence
  # function comes from its own univariable FIT (`num[[v]]$fit`, kept only when a spec asked for
  # `color = "adjustment"`). Both legs are then the SAME machinery -- reg_coef_if_maker() on two fits
  # solved on the same rows, or reg_ame_if_maker() on both -- which is why this needs no new maths.
  #
  # `multiplier` scales gap_se by |k|. The influence functions are NATIVE-scale while the stored estimate
  # and `obs` are already scaled, and fmt_gap_raw() reads the STORED values: with both legs scaled by the
  # same k the gap is k*(b_model - b_crude) on either branch (log(exp(k*b)) = k*b), so its SE is |k| x the
  # native one -- and the resulting z is invariant, exactly as reg_fit()'s own p is.
  # (reg_gap_se_of()/`between_groups` needs no such handling: it RECOVERS the SE from the printed,
  # already-scaled interval.)
  if (length(fits_crude) && length(fit_preds)) {
    for (k in which(in_mod & skeleton$var %in% fit_preds & !skeleton$is_ref)) {
      v  <- as.character(skeleton$var[k])
      nv <- fits_crude[[v]]
      if (is.null(nv) || is.null(nv$fit)) next
      kk <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else 1
      if (!is.finite(kk) || kk == 0) next
      cif_v <- reg_coef_if_maker(nv$fit)
      if (is.null(cif_v)) next
      # Phase 18z10: the fit arm now covers FACTOR predictors too (every predictor under an ordinal
      # outcome). A factor's contrast is (level, reference level), a numeric's a k-unit forward
      # difference -- the same two shapes reg_ame_if_maker()'s own contract states.
      is_fac_k <- v %in% fac_preds
      cl <- if (is_fac_k) list(as.character(skeleton$level[k]), ref_of(v)) else list(kk, 0)
      if (is_fac_k && is.na(cl[[2]])) next
      if (marginal) {
        im <- model_if(v, cl[[1]], cl[[2]])
        ic <- if (inherits(nv$fit, "multinom") || inherits(nv$fit, "polr"))
          reg_ame_if_cat_maker(nv$fit, nv$data, wt, ratio = ratio_m,
                               category = category)
        else
          reg_ame_if_maker(nv$fit, nv$data, wt, ratio = ratio_m,
                           coef_if = cif_v)
        ic <- if (is.null(ic)) NULL else ic(v, cl[[1]], cl[[2]])
        # the AME contrast already carries k, so no |k| rescale on this branch
        if (is.null(im) || is.null(ic) || length(ic) != length(im)) next
        out[k] <- reg_if_se(im - ic, des)
      } else {
        tm <- skeleton$term[k]
        if (is.na(tm)) next
        im <- coef_if(stats::setNames(1, tm))
        # the crude fit carries the SAME term name (one predictor, same levels, same relevel), so a
        # factor level under an ordinal outcome keys exactly as a numeric slope does.
        ic <- cif_v(stats::setNames(1, tm))
        if (is.null(im) || is.null(ic) || length(ic) != length(im)) next
        out[k] <- abs(kk) * reg_if_se(im - ic, des)
      }
    }
  }
  if (all(is.na(out))) NULL else out
}

# Phase 18z10: reg_empirical_tips() is DELETED. It was reg_empirical() at a three-part key --
# measured bit-identical on the shared quantity -- so the merged (var, level, category) grid is now the
# single producer, read directly by reg_stage_tips().
