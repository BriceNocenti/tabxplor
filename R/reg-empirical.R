# R/reg-empirical.R -- THE OBSERVED (crude) COMPANION of a model effect, and the standard error of
# the gap between the two.
#
# WHY THIS EXISTS. `tab_reg(empirical = TRUE)` prints, beside every modelled effect, the same
# quantity computed WITHOUT adjustment -- a base descriptive column (`Obs_%` / `Obs_mean` /
# `Obs_rate`) and a crude effect column on the model column's own scale and colour ladder. The gap
# between the two IS the answer to "what did adjustment do", which is what `color = "adjustment"`
# scores and what the `{obs}` display token prints. So the two must be the SAME estimand on the SAME
# rows, or the difference means listwise deletion rather than confounding.
#
# THE ONE FACT TABLE. `REG_EMPIRICAL` -- per family, the SHAPE of each crude column (name, stored
# scale, display, digits, reference, pct base, CI method, colour measure, link) plus which of them is
# the effect twin of the model's coefficient. A family is added as one row, never as a switch arm,
# and its rows are foreign-key checked against COLOR_SCALES / CI_METHODS / DISPLAY_TOKENS in
# R/zzz-fact-keys.R.
#
# TWO SOURCES, ONE SHAPE. `from = "grid"` (the default) is a CLOSED FORM off reg_empirical()'s
# per-(var, level, category) grid -- the univariable model being saturated for a factor predictor,
# the crude OR is exactly the Woolf 2x2 ratio. `from = "fit"` is a univariable reg_fit() through the
# very fitter the table came from (ordinal, every numeric predictor, and any marginal shape), taken
# where proportional odds or a continuous predictor leaves no closed form; ruling Q6 -- same
# estimand, link, CI rule and multiplier -- then holds by construction rather than by care.
#
# WHAT IS HERE
#   reg_crude_y / reg_crude_yw    the outcome on the scale the crude estimator averages
#   reg_level_counts              the N behind each predictor level (`add_n`'s column)
#   reg_empirical                 THE grid: emp_prop / emp_mean / emp_diff / emp_ratio / emp_n (+CIs)
#   reg_empirical_fit             the univariable fits the `from = "fit"` shapes need
#   reg_fit_overlay               splicing those fitted rows into a grid-built column
#   REG_EMPIRICAL + reg_crude_shape / shape_visible / ...   the shape vocabulary
#   reg_empirical_columns         THE builder: one fmt column per declared shape
#   reg_same_estimand / _frame    the two predicates that withhold `obs` rather than lie
#   reg_gap_se_columns            the gap SE (R/reg-influence.R's math, orchestrated per column)
#
# Phase 19l carved four subsystems out of tab.R for exactly this reason; Phase 20e does the same for
# tab_reg.R's largest one. The STAGE that drives these producers is reg_stage_empirical() in
# R/tab_reg.R -- the tab-leaf.R / tab.R relationship.
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
#   NUMERIC, per (var, level): the weighted mean and variance (tab()'s own formula, so an Obs_mean sd
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
  # (attr "degrade"), which reg_stage_empirical() harvests into the basis reg_stage_finalize() stamps -- the process-global
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
    # Phase 18z16-iv (W-E): the family's DECLARED difference method, not a second hard-coded one.
    # This interval's only consumer is the multinomial html tooltip, which was Newcombe while the
    # Obs_% column of the same table was Wald -- one quantity, two methods, inside one table. The
    # cross-table difference from tab(ci = "diff")'s Newcombe is deliberate (Phase 16d: the crude
    # companion matches the model AME's Wald so the merged legend can name ONE method).
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
  # z16-iiiii: the degrade travels OUT with the grid it describes. reg_stage_empirical() harvests it into the
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


# reg_fit_overlay() -- Phase 18z9 (as reg_num_overlay) / z10: write fit-derived crude rows into a
# finished crude EFFECT column and into the crude effect VECTOR, at the ONE point both are in hand.
#
# DESIGN -- why here and not before emp_col(). On the binomial `ame` branch the base column and the
# effect column are built from the SAME `rd_fields` list, and REG_EMPIRICAL$binomial$base declares
# `color = "diff"` -- so overlaying the estimate into those shared locals would have written the AME into
# `Obs_%`'s `diff` field and COLOURED a cell that displays nothing. emit() is the one place the effect
# shape is known and only the effect column is touched.
#
# The estimate lands in the field its `scale` declares (fmt_center_field()'s rule), exp()d exactly when
# that scale is `odds_ratio` -- which is also what tells this function whether the shape is an
# exponentiated effect or its log twin. `n` is deliberately left NA: like the model column's, a fit-derived row's base is the
# whole model N, which belongs in the footer, not in a per-cell "n:".
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

# The empirical (crude) companion FACT TABLE: per family (binomial / gaussian / poisson), the SHAPE of
# the base descriptive column + the crude-effect column (fmt scale / pct_base / display / digits / ref /
# colour measure + the visible name), plus the CI METHOD literal the crude interval uses. The per-family
# CI MATH stays code below (ci_prop_diff / ci_or / ci_pivot / ci_mean_diff2 / ci_mean_ratio take
# different arguments), but the near-identical fmt() calls collapse into ONE builder (emp_col), and the
# `method_*` literals are the SAME the colour legend names -- each shape row also declares the engine
# its own column's bounds were built with (`ci_method`, Phase 19b), stamped by emp_col()
# (reg_build), so "the empirical CI matches the model CI" is data, not a hand-synced pair (Phase 17h).
#   binomial : Obs_% (risk-diff colour, WALD) + Obs_OR (ratio, Woolf log-OR) | ame: + Obs_diff (WALD).
#   gaussian : Obs_mean (mean+sd, UNCOLOURED, one-sample t) + Obs_diff (Student t = OLS, diff/SD(Y)).
#   poisson  : Obs_rate (rate-ratio colour) + Obs_IRR, one quasi-Poisson CI (the phi-scaled model's).
# Phase g: the crude columns are named "Obs_" (snake-case, "observed"; was "Emp." for "empirical"), on
# BOTH the exponentiate=TRUE and FALSE paths -- W6 adds the logged Obs_log(OR) / Obs_log(IRR) shapes.
# Phase g: each multiplicative effect shape (binomial `or`, poisson `irr`) has a LOGGED twin
# (`or_log` / `irr_log`) used when the model is NOT exponentiated -- a coef-shaped column carrying
# log(OR) / log(IRR) with a logged CI, so the crude companion matches the raw model coefficient (same
# link scale, same log_odds_scale colour). reg_empirical_columns picks the twin by `do_exp`.
# Phase 18z8-B: each EFFECT row also carries the `link` of the crude estimator it describes -- the
# one fact reg_crude_if_maker() needs to write its closed-form influence function (g'(mu) = 1/(mu(1-mu))
# logit | 1/mu log | 1 identity). It sits on the SHAPE row, not on the family, because the crude link
# follows the chosen ESTIMAND: a binomial model shows a logit-scale OR by default, an IDENTITY-link risk
# difference under effect = "ame", and a LOG-link risk ratio under "ame_ratio" (which reuses
# REG_EMPIRICAL$rr$rr verbatim -- the very reuse that makes a per-family link impossible). A `base` row
# is descriptive, never an effect, so its link is NA.
REG_EMPIRICAL <- list(
  binomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    base   = list(nm = "Obs_%",       scale = "points", display = "pct", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = NA_character_),
    ame    = list(nm = "Obs_diff",    scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = "identity"),
    or     = list(nm = "Obs_OR",      scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",    ci_method = "woolf", color = "OR",   link = "logit"),
    or_log = list(nm = "Obs_log(OR)", scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "woolf", color = "diff", link = "logit")),
  # Phase 18z3 -- the modified-Poisson (binary outcome) crude companion. SAME base column as binomial
  # (a risk, `Obs_%`, with the Wald risk-difference CI), but the effect is a crude RISK ratio with the
  # KATZ log-RR interval (ci_katz_rr) -- not the Woolf log-OR the binomial arm uses. That is the point
  # of the whole feature: the observed companion must be on the same scale as the model column.
  rr = list(
    method_diff = "wald", coef = "rr", coef_log = "rr_log",
    base   = list(nm = "Obs_%",       scale = "points", display = "pct", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = NA_character_),
    ame    = list(nm = "Obs_diff",    scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = "identity"),
    rr     = list(nm = "Obs_RR",      scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",    ci_method = "katz", color = "OR",   link = "log"),
    rr_log = list(nm = "Obs_log(RR)", scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "katz", color = "diff", link = "log")),
  # Phase 19e -- the crude companion of a RATIO OF MEANS (`measure = "ratio"` on a continuous
  # outcome, fitted by the "mr" log-link pseudo-likelihood). Its base is the group MEAN and its
  # effect the crude ratio of means, with the ci_mean_ratio engine tab() has used for years -- the
  # same "the observed companion must be on the same scale as the model column" rule that gave "rr"
  # its own block rather than borrowing binomial's.
  mr = list(
    method_mean_ratio = "quasipoisson", coef = "mr", coef_log = "mr_log",
    base   = list(nm = "Obs_mean",     scale = "level_mean", display = "mean", digits = 2L, ref = NA_character_, pct_base = "none", ci_method = "student",     color = "",      link = NA_character_),
    mr     = list(nm = "Obs_RoM",      scale = "mean_ratio", display = "ratio", digits = 2L, ref = "1", pct_base = "none", ci_method = "quasipoisson", color = "ratio", link = "log"),
    mr_log = list(nm = "Obs_log(RoM)", scale = "log_coef",   display = "coef", digits = 2L, ref = NA_character_, pct_base = "none", ci_method = "quasipoisson", color = "diff",  link = "log")),
  gaussian = list(
    method_mean_diff = "student", coef = "diff", coef_log = "diff",
    base = list(nm = "Obs_mean", scale = "level_mean", display = "mean", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "student", color = "",     link = NA_character_),
    diff = list(nm = "Obs_diff", scale = "raw_diff",  display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "student", color = "diff", link = "identity")),
  poisson = list(
    method_mean_ratio = "quasipoisson", coef = "irr", coef_log = "irr_log",
    base    = list(nm = "Obs_rate",     scale = "mean_ratio", display = "mean", digits = 2L, ref = "1", pct_base = "none", ci_method = "quasipoisson", color = "ratio", link = NA_character_),
    irr     = list(nm = "Obs_IRR",      scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",    ci_method = "katz", color = "OR",    link = "log"),
    irr_log = list(nm = "Obs_log(IRR)", scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "katz", color = "diff",  link = "log")),
  # Phase 18z10 -- the three families that had no crude twin at all.
  #
  # grouped_binomial (`trials =`): the univariable model is STILL saturated for a factor predictor, so
  # the crude OR is the existing Woolf 2x2 on the SUMMED counts (measured identical to a univariable glm
  # to 1.1e-8). Its BASE column is the mean SCORE (maintainer's ruling) -- a per-RESPONDENT quantity, so
  # it takes the gaussian base shape and reads `emp_mean`, while the effect reads the summed 2x2. That
  # one family needing both grid parts at once is why `emp_base` had to split into emp_prop / emp_mean.
  grouped_binomial = list(
    method_diff = "wald", method_mean_diff = "student", coef = "or", coef_log = "or_log",
    base   = list(nm = "Obs_mean",     scale = "level_mean", display = "mean", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "student", color = "",     link = NA_character_),
    ame    = list(nm = "Obs_diff",     scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = "identity"),
    or     = list(nm = "Obs_OR",       scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",    ci_method = "woolf", color = "OR",   link = "logit"),
    or_log = list(nm = "Obs_log(OR)",  scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "woolf", color = "diff", link = "logit")),
  # multinomial: one crude column PER OUTCOME CATEGORY would double an already wide table, so these
  # shapes are `visible = FALSE` -- the crude number rides IN-CELL in the model column's `obs` field
  # (maintainer's ruling Q4, rendered as "{or} ({obs})" / "{diff} ({obs})"). `obs` is defined as "the
  # value this cell is compared to, ON THE CELL'S OWN SCALE", so an invisible shape still has to declare
  # its scale and link exactly like a visible one. The crude effect is closed-form: the univariable
  # multinomial is saturated, and its OR is the {j, ref} x {level, ref level} Woolf ratio -- the very
  # number tab(pct = "row", OR = "OR") prints.
  multinomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    or        = list(nm = NA_character_, scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",   ci_method = "woolf", color = "OR",   link = "logit", visible = FALSE, per_category = TRUE),
    or_log    = list(nm = NA_character_, scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none", ci_method = "woolf", color = "diff", link = "logit", visible = FALSE, per_category = TRUE),
    ame       = list(nm = NA_character_, scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row", ci_method = "wald", color = "diff", link = "identity", visible = FALSE, per_category = TRUE),
    ame_ratio = list(nm = NA_character_, scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",   ci_method = "katz", color = "OR",   link = "log",   visible = FALSE, per_category = TRUE)),
  # ordinal: proportional odds is a CONSTRAINT, so the univariable model is NOT saturated and there is no
  # closed form (measured: the three closed-form substitutes drift by 2.4-5.4 %, of the same order as the
  # first colour break -- and the drift IS the PO violation, so it would inject a data-outcome offset
  # into a measure whose whole job is to say how far the model moved the effect). Hence `from = "fit"`:
  # a univariable polr / svyolr through reg_fit(), the same escape z9 took for numeric predictors and for
  # the same reason -- ruling Q6 (same estimand, link, CI rule, multiplier) holds by construction.
  ordinal = list(
    coef = "cumor", coef_log = "cumor_log",
    cumor     = list(nm = "Obs_cumOR",      scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",   ci_method = "wald_log", color = "OR",   link = "logit", from = "fit"),
    cumor_log = list(nm = "Obs_log(cumOR)", scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none", ci_method = "wald_log", color = "diff", link = "logit", from = "fit"),
    ame       = list(nm = NA_character_, scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row", ci_method = "wald", color = "diff", link = "identity", visible = FALSE, per_category = TRUE, from = "fit"),
    ame_ratio = list(nm = NA_character_, scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",   ci_method = "wald_log", color = "OR",   link = "log",   visible = FALSE, per_category = TRUE, from = "fit"))
)

# The three optional SHAPE facts z10 added, with their defaults in one place (a shape row states only
# what makes it unusual, so the 14 pre-existing rows stay untouched):
#   visible      does this shape draw an Obs_* COLUMN, or does its number ride in-cell via `obs`?
#   per_category is there one crude effect per OUTCOME category (multinomial / ordinal marginal)?
#   from         "grid" = a closed form from reg_empirical(); "fit" = a univariable reg_fit().
#' @keywords internal
shape_visible      <- function(shape) !isFALSE(shape$visible)
#' @keywords internal
shape_per_category <- function(shape) isTRUE(shape$per_category)
# (no shape_from_fit(): `from` is read where the numeric overlay is spliced in, and the accessor
# never acquired a caller -- deleted in 19l.)

# reg_crude_shape() -- WHICH REG_EMPIRICAL row describes the crude EFFECT of this estimand? Read by
# reg_empirical_columns()'s arms (which build the column) and by the footer wording -- two consumers,
# one fact, per Phase 17 rule 5.
# Phase 19e: it is a LOOKUP now, not a dispatch: the estimand row (R/reg-estimand.R) names its own
# `crude_fam` / `crude_shape`, so "a binary marginal RATIO reuses REG_EMPIRICAL$rr$rr" and "a family
# with no marginal crude falls back to its coefficient row" are both DECLARED per cell instead of
# being inferred from (effect, do_exp) here. `coef` / `coef_log` stay on each REG_EMPIRICAL family:
# they are that family's own facts (its coefficient-scale shape and its logged twin), read where BOTH
# twins are built at once.
#' @keywords internal
reg_crude_shape <- function(crude_key, est = NULL) {
  # Phase 19e: the SELECTION is the estimand row's own two columns (`crude_fam` / `crude_shape`), so
  # the dispatch above is gone -- including its cross-family borrow (a binary marginal RATIO reuses
  # REG_EMPIRICAL$rr$rr), which is a declared `crude_fam = "rr"` now. `crude_fam = "auto"` means "the
  # outcome's own block", which is what carries `trials` -> grouped_binomial through reg_crude_key().
  if (is.null(est)) est <- list(crude_fam = "auto", crude_shape = NA_character_)
  key <- if (!identical(est$crude_fam %||% "auto", "auto")) est$crude_fam else crude_key
  fam <- if (is.null(key) || is.na(key)) NULL else REG_EMPIRICAL[[key]]
  if (is.null(fam)) return(NULL)
  sh <- est$crude_shape
  if (is.null(sh) || is.na(sh)) sh <- fam$coef
  fam[[sh]]
}

# Does the crude effect ride IN-CELL (as `obs`) instead of drawing its own Obs_* column? One stored
# consequence of the shape, read by the footer wording and by set_obs_if()'s display fold.
#' @keywords internal
reg_crude_in_cell <- function(crude_key, est = NULL) {
  sh <- reg_crude_shape(crude_key, est)
  !is.null(sh) && !shape_visible(sh)
}
# WARNING: `l[[""]]` is a subscript-out-of-bounds ERROR in R, not a miss -- and "" is exactly the key a
# single-column fit uses. Every lookup into a category-keyed list goes through this.
#' @keywords internal
cat_get <- function(l, key) {
  if (is.null(l) || !length(l)) return(NULL)
  i <- match(if (is.null(key)) "" else as.character(key), names(l))
  if (is.na(i)) NULL else l[[i]]
}

# The base+effect fmt columns aligned to the skeleton, for reg_stage_assemble() to prepend before the model column.
# The Constant -> empty cells; reference levels -> neutral + in_refrow, no CI. want_p is TRUE (the pvalue
# is stored; stars are stripped post-build when stars = FALSE, like the model columns).
#
# Phase 18z10 -- three structural changes, all driven by shape FACTS rather than by family names:
#   * emit() replaces two(): a shape set may draw TWO columns (base + effect, every pre-z10 family), ONE
#     (ordinal: a cumulative OR has no base -- there is no single share to show beside it), or ZERO
#     (multinomial: the crude number rides in-cell via `obs`). The old two() could only ever do two.
#   * the crude EFFECT is returned as a list keyed by OUTCOME CATEGORY ("" when the outcome has none),
#     because a multinomial / ordinal-marginal model has one column per category and each needs its own
#     `obs`. reg_set_obs() looks the column's stored `emp_key` up in it.
#   * `fit_est` (reg_empirical_fit()'s per-category estimates) fills the rows no closed form covers --
#     numeric predictors in any family (z9), and EVERY predictor under an ordinal outcome (z10).
reg_empirical_columns <- function(skeleton, emp, fac_preds, crude_key, family, est, var_y,
                                  conf_level = 0.95, color_signif = "grey_non_signif",
                                  color = NULL, fit_est = NULL, weighted = FALSE,
                                  degf = Inf) {
  fam <- REG_EMPIRICAL[[crude_key]]
  if (is.null(fam)) return(list(cols = list(), effect = NULL, shape = NULL))
  # Phase 19e: the three facts the arms below branch on, read off the ESTIMAND row rather than from
  # the (effect, do_exp) pair -- so the crude companion cannot describe a different estimand from
  # the model column it sits beside.
  marginal   <- !identical(est$effect, "coefficient")
  ratio_marg <- marginal && identical(est$comparison, "lnratioavg")
  do_exp     <- isTRUE(est$exp)
  # WHICH crude shape this estimand declares -- the arms below dispatch on it rather than on
  # (marginal, do_exp), which is what lets a CONDITIONAL risk difference (`measure = "difference"`,
  # the identity-link "rd" fit) take the same crude risk-difference column as the marginal one.
  shape_key  <- est$crude_shape %||% fam$coef
  if (is.null(shape_key) || is.na(shape_key)) shape_key <- fam$coef
  # Phase 15d: when the model is uncoloured (`color = FALSE` -> "no"), the crude companions must be
  # uncoloured too (else the table shows coloured empirical columns beside plain model columns).
  # `color[1]`: the measure may be a length-2 (text, background) vector since Phase 18z5's
  # `color = c("OR", "adjustment")` -- `color %in% ...` would then return length 2 and the `if` below
  # would error. Only the text channel decides whether the crude companions are drawn at all.
  emp_off <- !is.null(color) && color[1] %in% c("no", "")
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
  # one fmt column from a shape row + its varying fmt FIELD values. Uncoloured when the model is off or
  # the shape declares no measure (Obs_mean); `ref` is omitted when the shape has none.
  emp_col <- function(shape, fields, n_eff = NULL) {
    measure <- if (emp_off || !nzchar(shape$color)) "" else shape$color
    args <- c(fields, if (!is.null(n_eff)) list(n_eff = n_eff), list(
      scale = shape$scale, pct_base = shape$pct_base, display = shape$display, digits = shape$digits,
      ci_method = shape$ci_method %||% "",
      color = measure, color_signif = if (nzchar(measure)) color_signif else "ignore",
      col_var = shape$nm, comp_all = FALSE, in_refrow = refrows, model_family = family, role = "emp"))
    if (!is.na(shape$ref)) args$ref <- shape$ref
    do.call(fmt, args)
  }
  # Phase 18z16-iv (W-D): the effective base a crude interval was ACTUALLY computed on, stored in
  # the `n_eff` field. reg_empirical() computes it (identically to tab()'s own cell base, to 9 s.f.),
  # feeds it to ci_wilson / ci_prop_diff / ci_or / ci_pivot / ci_mean_diff2 / ci_mean_ratio -- and then
  # threw it away, so ?fmt's "the effective sample size used for this cell's CI" was false on every
  # regression column and `$n_eff` returned NA where the correction demonstrably happened. NA when
  # nothing corrected it, exactly as an unweighted tab() cell carries NA. Which of the two bases a
  # column used is a property of ITS OWN interval, so each arm passes its own (`nv_dr` for a
  # proportion / odds / risk ratio, `nv_ci` for a mean, a rate and their ratios) -- it cannot be read
  # off `shape$type` (a poisson IRR is type "row" and takes `nv_ci`).
  neff_of <- function(v) if (isTRUE(weighted)) as.double(v) else rep(NA_real_, n_rows)
  # Phase 18z5: besides the columns, return the crude EFFECT vector -- the very value the effect
  # column stores in its own estimate field, so it is already on the model column's scale (an OR beside
  # an OR, log(OR) beside a raw coefficient, a risk difference beside an AME). reg_set_obs() writes it into
  # the model columns' `obs` field, which backs `color = "adjustment"` and the `{obs}` display token.
  # Taken from the local the shape was built from -- never re-read out of the fmt column by name.
  # Phase 18z8-B: the effect SHAPE ROW travels with it, giving the gap test both facts it needs --
  # `link` (the crude estimator's link) and `scale` (proof that the crude and model columns are the
  # SAME estimand) -- and any future shape fact for free, with no new element to thread.
  # Phase 18z9/z10: the fit-derived rows are spliced HERE -- the one place the effect shape is known,
  # so no return arm changes and the base column (which on the binomial `ame` branch shares its field
  # list with the effect column) cannot be touched. See reg_fit_overlay().
  # `cat` = which outcome category's grid rows / fit estimates feed this call. The returned effect list
  # is keyed by the MODEL COLUMN's own category (`emp_key`), which is "" for every family that produces
  # one column -- including the binary ones, whose grid slice is the positive category "1".
  emit <- function(base, eff, cat = "") {
    if (is.null(eff)) return(list(cols = if (is.null(base)) list() else
                                    stats::setNames(list(base$col), base$shape$nm),
                                  effect = NULL, shape = NULL))
    # `key` addresses BOTH the returned effect list and the fit estimates: reg_empirical_fit() keys its
    # coefficient rows "" and its marginal rows by outcome group, i.e. exactly the column's own category.
    key <- if (shape_per_category(eff$shape)) cat else ""
    o   <- reg_fit_overlay(eff$col, eff$vec, cat_get(fit_est$est, key), eff$shape)
    cols <- list()
    if (!is.null(base) && shape_visible(base$shape))
      cols <- c(cols, stats::setNames(list(base$col), base$shape$nm))
    if (shape_visible(eff$shape))
      cols <- c(cols, stats::setNames(list(o$col), eff$shape$nm))
    list(cols = cols, effect = stats::setNames(list(o$eff), key), shape = eff$shape)
  }
  # per-category slice of the grid, aligned to the skeleton
  cat_of <- function(cat) {
    g  <- emp[emp$category == cat, , drop = FALSE]
    mi <- reg_skel_match(skeleton, g)
    lapply(stats::setNames(nm = setdiff(names(reg_empirical_empty()),
                                        c("var", "level", "category"))),
           function(nm) g[[nm]][mi])
  }

  # ---- ordinal: no closed form, so both columns come from the univariable fits (see REG_EMPIRICAL) ----
  if (identical(crude_key, "ordinal")) {
    if (marginal) {
      sh   <- reg_crude_shape(crude_key, est)
      cats <- names(fit_est$est)
      if (!length(cats)) return(list(cols = list(), effect = NULL, shape = sh))
      out  <- purrr::map(stats::setNames(nm = cats), function(k)
        emit(NULL, list(col = emp_col(sh, list(diff = na_v(), n = rep(NA_integer_, n_rows))),
                        vec = na_v(), shape = sh), k))
      return(list(cols = list(), shape = sh,
                  effect = purrr::flatten(purrr::map(out, "effect"))))
    }
    sh  <- fam[[if (do_exp) fam$coef else fam$coef_log]]
    fld <- if (do_exp) list(or = na_v()) else list(diff = na_v())
    return(emit(NULL, list(col = emp_col(sh, c(fld, list(n = rep(NA_integer_, n_rows)))),
                           vec = na_v(), shape = sh)))
  }

  # ---- multinomial: closed form, one crude effect per outcome category, no visible column ------------
  if (identical(crude_key, "multinomial")) {
    sh   <- reg_crude_shape(crude_key, est)
    cats <- unique(emp$category)
    if (!length(cats)) return(list(cols = list(), effect = NULL, shape = sh))
    out <- purrr::map(stats::setNames(nm = cats), function(k) {
      g <- cat_of(k)
      v <- if (ratio_marg) g$emp_ratio_prop
           else if (marginal) g$emp_diff
           else if (do_exp)   g$emp_ratio else log(g$emp_ratio)
      emit(NULL, list(col = emp_col(sh, list(n = rep(NA_integer_, n_rows))), vec = v, shape = sh), k)
    })
    return(list(cols = list(), shape = sh, effect = purrr::flatten(purrr::map(out, "effect"))))
  }

  # ---- the closed-form families: one category ("1" binary/grouped, "" numeric outcomes) -------------
  cat1 <- if (identical(emp$category[1], "1") || "1" %in% emp$category) "1" else ""
  g    <- cat_of(cat1)
  prop <- g$emp_prop; diffv <- g$emp_diff; ratio <- g$emp_ratio
  meanv <- g$emp_mean; varv <- g$emp_var; nv <- g$emp_n
  rprop <- g$emp_ref_prop; rmean <- g$emp_ref_mean; rv <- g$emp_ref_var; rn <- g$emp_ref_n
  # Phase 18s: the CI base is the effective n -- the exact flat closed form on the weights, or the
  # design variance; unweighted it equals the raw count, so those intervals are byte-identical. The
  # displayed n/tot_n fields always keep the raw count `nv`.
  nv_ci <- g$emp_n_ci; rn_ci <- g$emp_ref_n_ci
  # the CI base of a PROPORTION is the number of Bernoulli DRAWS (n x trials for a grouped binomial,
  # n everywhere else -> byte-identical); the MEAN CIs keep the per-respondent n_ci.
  nv_dr <- g$emp_n_draw; rn_dr <- g$emp_ref_n_draw

  # binomial + "rr" (modified Poisson) share every BASE fact -- a crude risk and its Wald risk-difference
  # CI -- and differ only in the crude EFFECT, which must be the model's own estimand (Phase 18z3).
  # Phase 18z10: grouped_binomial shares the EFFECT facts (a Woolf OR on the summed 2x2 legs) but not
  # the base -- its base column is the mean SCORE, built below like the gaussian one.
  binary_like <- reg_fam_binary(crude_key) || identical(crude_key, "grouped_binomial")
  if (binary_like) {
    grouped <- identical(crude_key, "grouped_binomial")
    rd <- na_ref(ci_prop_diff(prop, nv_dr, rprop, rn_dr, conf_level = conf_level, # crude risk-difference
                              method = fam$method_diff, want_p = TRUE, df = degf))
    rd_fields <- list(pct = prop, diff = diffv, n = nv, tot_n = nv,
                      ci_inf = rd$inf, ci_sup = rd$sup, pvalue = rd$pvalue)
    base <- if (grouped) {
      # the mean SCORE and its one-sample t interval (the gaussian base shape, on the numeric part)
      cell <- ci_pivot(meanv, sqrt(varv / nv_ci), df = df_or_design(nv_ci - 1, degf),
                       conf_level = conf_level, want_p = FALSE)
      list(col = emp_col(fam$base, list(mean = meanv, var = varv, n = nv, tot_n = nv,
                                        ci_inf = cell$inf, ci_sup = cell$sup),
                         n_eff = neff_of(nv_ci)), shape = fam$base)
    } else list(col = emp_col(fam$base, rd_fields, n_eff = neff_of(nv_dr)), shape = fam$base)

    if (identical(shape_key, "ame")) { # a DIFFERENCE of risks (marginal AME or conditional "rd")
      sh <- reg_crude_shape(crude_key, est)
      return(emit(base, list(col = emp_col(sh, rd_fields, n_eff = neff_of(nv_dr)),
                             vec = diffv, shape = sh), cat1))
    }
    # Phase 18z3: a marginal RATIO's crude twin is the crude RISK ratio with the Katz log-RR interval
    # -- on the binomial model path as well as the "rr" one, since the estimand is what must match, not
    # the fitted family. Always exponentiated: `exponentiate` is ignored for marginal effects. The Obs_RR
    # shape is defined once, in REG_EMPIRICAL$rr, and reused here rather than duplicated per family.
    if (ratio_marg && !identical(crude_key, "rr")) {
      rr_ci <- na_ref(ci_katz_rr(prop, nv_dr, rprop, rn_dr, conf_level = conf_level,
                                 want_p = TRUE, df = degf))
      sh    <- reg_crude_shape(crude_key, est)
      return(emit(base, list(col = emp_col(sh, list(or = prop / rprop, n = nv, ci_inf = rr_ci$inf,
                                                    ci_sup = rr_ci$sup, pvalue = rr_ci$pvalue),
                                    n_eff = neff_of(nv_dr)),
                             vec = prop / rprop, shape = sh), cat1))
    }
    # binomial / grouped -> the crude ODDS ratio (the 2x2 legs vs the reference level's) with the Woolf
    # log-OR interval. "rr" -> the crude RISK ratio (prop/rprop) with the Katz log-RR interval. WARNING:
    # `ratio` (emp_ratio) is an ODDS ratio -- feeding it to an Obs_RR column would print an OR under an
    # RR header. Phase 18z10: the 2x2 legs come from the grid (emp_wpos / emp_wneg) instead of being
    # rebuilt as prop * n -- for a grouped binomial the base is Sum(w * trials), not the respondent
    # count, and only the legs know that.
    is_rr  <- identical(crude_key, "rr")
    eff_v  <- if (is_rr) prop / rprop else ratio
    eff_ci <- na_ref(if (is_rr)
      ci_katz_rr(prop, nv_dr, rprop, rn_dr, conf_level = conf_level, want_p = TRUE, df = degf)
    else
      # the SS14 rule, unchanged: WEIGHTED proportion x UNWEIGHTED base, so the base cancels out of the
      # log-OR. For a grouped binomial that base counts DRAWS (n x trials), which is what makes the crude
      # OR equal a univariable glm(cbind(s, q - s) ~ x) rather than an OR on respondent counts.
      ci_or(prop * nv_dr, (1 - prop) * nv_dr,
            rprop * rn_dr, (1 - rprop) * rn_dr, conf_level = conf_level, want_p = TRUE, df = degf))
    sh_exp <- fam[[fam$coef]]
    sh_log <- fam[[fam$coef_log]]
    if (do_exp) {
      eff_col <- emp_col(sh_exp, list(or = eff_v, n = nv, ci_inf = eff_ci$inf,
                                      ci_sup = eff_ci$sup, pvalue = eff_ci$pvalue),
                         n_eff = neff_of(nv_dr))
      return(emit(base, list(col = eff_col, vec = eff_v, shape = sh_exp), cat1))
    }
    # Phase g: exponentiate = FALSE -> the crude companion is the LOGGED effect (Obs_log(OR) /
    # Obs_log(RR)): the log ratio in the `diff` field with the logged CI, i.e. the exact Wald interval
    # on the log scale -- the same link scale as the raw model coefficient.
    eff_col <- emp_col(sh_log, list(diff = log(eff_v), n = nv, ci_inf = log(eff_ci$inf),
                                    ci_sup = log(eff_ci$sup), pvalue = eff_ci$pvalue),
                       n_eff = neff_of(nv_dr))
    return(emit(base, list(col = eff_col, vec = log(eff_v), shape = sh_log), cat1))
  }

  if (identical(crude_key, "gaussian")) {
    cell <- ci_pivot(meanv, sqrt(varv / nv_ci), df = df_or_design(nv_ci - 1, degf),
                     conf_level = conf_level, want_p = FALSE)
    base_col <- emp_col(fam$base, list(mean = meanv, var = varv, n = nv, tot_n = nv,
                                       ci_inf = cell$inf, ci_sup = cell$sup),
                        n_eff = neff_of(nv_ci))
    md <- na_ref(ci_mean_diff2(meanv, varv, nv_ci, rmean, rv, rn_ci, method = fam$method_mean_diff, # pooled t = OLS
                               conf_level = conf_level, want_p = TRUE, df_design = degf))
    eff_col <- emp_col(fam$diff, list(diff = diffv, var = rep(var_y, n_rows), n = nv,
                                      ci_inf = md$inf, ci_sup = md$sup, pvalue = md$pvalue),
                       n_eff = neff_of(nv_ci))
    return(emit(list(col = base_col, shape = fam$base),
                list(col = eff_col, vec = diffv,
                     shape = reg_crude_shape(crude_key, est)), cat1))
  }

  # Phase 19e -- the RATIO OF MEANS crude twin ("mr"): the gaussian base column (a group mean and its
  # one-sample t interval) beside the crude ratio of means, with the SAME ci_mean_ratio engine the
  # poisson arm uses -- which is what makes "the observed companion is on the model's scale" true
  # for this estimand too. The `ratio` field, never `or`: mean_ratio's declared est_field.
  if (identical(crude_key, "mr")) {
    cell <- ci_pivot(meanv, sqrt(varv / nv_ci), df = df_or_design(nv_ci - 1, degf),
                     conf_level = conf_level, want_p = FALSE)
    base_col <- emp_col(fam$base, list(mean = meanv, var = varv, n = nv, tot_n = nv,
                                       ci_inf = cell$inf, ci_sup = cell$sup),
                        n_eff = neff_of(nv_ci))
    mr <- na_ref(ci_mean_ratio(meanv, varv, nv_ci, rmean, rv, rn_ci, method = fam$method_mean_ratio,
                               conf_level = conf_level, want_p = TRUE, df_design = degf))
    if (do_exp) {
      eff_col <- emp_col(fam$mr, list(ratio = ratio, n = nv, ci_inf = mr$inf,
                                      ci_sup = mr$sup, pvalue = mr$pvalue),
                         n_eff = neff_of(nv_ci))
      return(emit(list(col = base_col, shape = fam$base),
                  list(col = eff_col, vec = ratio, shape = fam$mr), cat1))
    }
    eff_col <- emp_col(fam$mr_log, list(diff = log(ratio), n = nv, ci_inf = log(mr$inf),
                                        ci_sup = log(mr$sup), pvalue = mr$pvalue),
                       n_eff = neff_of(nv_ci))
    return(emit(list(col = base_col, shape = fam$base),
                list(col = eff_col, vec = log(ratio), shape = fam$mr_log), cat1))
  }

  if (identical(crude_key, "poisson")) {
    # one crude rate-ratio CI (quasi-Poisson, = the phi-scaled model's method) drives BOTH columns.
    rr <- na_ref(ci_mean_ratio(meanv, varv, nv_ci, rmean, rv, rn_ci, method = fam$method_mean_ratio,
                               conf_level = conf_level, want_p = TRUE, df_design = degf))
    base_col <- emp_col(fam$base, list(mean = meanv, ratio = ratio, n = nv, tot_n = nv,
                                       ci_inf = rr$inf, ci_sup = rr$sup, pvalue = rr$pvalue),
                        n_eff = neff_of(nv_ci))
    if (do_exp) {
      eff_col <- emp_col(fam$irr, list(or = ratio, n = nv, ci_inf = rr$inf,
                                       ci_sup = rr$sup, pvalue = rr$pvalue),
                          n_eff = neff_of(nv_ci))
      return(emit(list(col = base_col, shape = fam$base),
                  list(col = eff_col, vec = ratio, shape = fam$irr), cat1))
    }
    # Phase g: exponentiate = FALSE -> the crude companion is Obs_log(IRR): log(rate-ratio) in `diff`
    # with the logged rate-ratio CI (the same link scale as the raw Poisson coefficient).
    eff_col <- emp_col(fam$irr_log, list(diff = log(ratio), n = nv, ci_inf = log(rr$inf),
                                         ci_sup = log(rr$sup), pvalue = rr$pvalue),
                        n_eff = neff_of(nv_ci))
    return(emit(list(col = base_col, shape = fam$base),
                list(col = eff_col, vec = log(ratio), shape = fam$irr_log), cat1))
  }

  list(cols = list(), effect = NULL, shape = NULL)
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
