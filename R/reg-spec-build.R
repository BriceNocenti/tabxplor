# PURPOSE: WHAT ONE MODEL CONTRIBUTES TO A tab_reg() TABLE -- the per-spec half of reg_build(),
#   as one declared product (Phase 20f-iii, "20e one grain finer").
# ROLE: Phase 20e named the STAGES; six of them still carried their own `map(specs, ...)`, so
#   "which parts of the table are per-model and which are between-models" was answerable only by
#   reading four files. reg_spec_build() is that answer: ONE function, ONE record, and the stages
#   above it become cross-spec ASSEMBLERS. reg_stage_specs() (R/tab_reg.R) drives it -- the same
#   relationship R/reg-empirical.R has to reg_stage_empirical().
# KEY CONSTRAINTS:
#   - THE PAYLOAD RULE: the product carries no fit and nothing referencing one, so a unit can cross
#     a process boundary. Two DECLARED exceptions, each identical to a reason that shape is serial
#     anyway (reg_specs_independent) -- `fit` (a model comparison needs the fit objects) and the
#     crude block's heavy `$frame`/`$fits`, kept only for the block that is SHARED with other specs.
#   - ⚠ the ORDER inside the builder is today's relative order for one spec: fit -> columns ->
#     footer rows -> add_n -> the crude block -> obs/gap_se -> tooltips. Three of those can signal
#     (the fit, reg_marginal_basis_warn(), the Brant test), so the order IS part of the output.
#   - ⚠ NO dot-prefixed key on the record: as.list(environment()) defaults to all.names = FALSE and
#     drops them silently (Phase 20e's measured defect, on `.fit_cache`).
#   - ⚠ this file sorts BEFORE R/tab_reg.R, so it may hold no top-level code reading a tab_reg.R
#     object (function bodies are fine -- they run after the namespace is built).
# See: CLAUDE.md 2.0.0 roadmap Phase 20f-iii + dev/tabxplor_reg_performance.md 6.


# === THE PRODUCT ================================================================================
#
# new_reg_spec_product() -- the fourth record constructor after new_reg_ctx() / new_reg_shared() /
# new_reg_spec(), and their idiom: THE FORMALS ARE THE CONTRACT, the body is as.list(environment()),
# and a slot that is absent would be a missing binding rather than a NULL, so everything is declared.
#
# Two slots carry a PLACEHOLDER, because a worker cannot know facts that only exist once every spec
# has been built (Phase 20f-ii 6.5, constraint 4):
#   * the footer row tibbles' `col` is this spec's PRE-make.unique() first column label --
#     reg_stage_footer() overwrites the whole column with fit_first_col[[i]]. Total, not a match:
#     every row of one spec shares one `col`.
#   * a multinomial tooltip fragment carries `col_idx` (a within-spec column index) and every
#     fragment carries `row` (a skeleton row index) -- reg_stage_tips() maps them to labels[] and
#     disp_levels[]. That is also what frees the tooltips from needing reg_stage_rows() to run first.
#' @keywords internal
#' @noRd
new_reg_spec_product <- function(
    # --- the table's columns --------------------------------------------------------------------
    # `cols`: the {label, col, emp_key} records this spec contributes, `obs`/`gap_se` ALREADY set
    # (reg_set_obs runs here: doing it in the assembler would need the fits back, which is the whole
    # thing the payload rule forbids). `emp`: the observed/crude block -- reg_stage_assemble()
    # splices `emp$cols`.
    cols = list(), emp = NULL, n_col = list(),
    # --- the `test` tibble ----------------------------------------------------------------------
    gof_rows = NULL, global_rows = NULL, check_rows = NULL,
    # --- meta$empirical_tips --------------------------------------------------------------------
    tips = list(mnl = NULL, num = NULL),
    # --- the scalars the table-scalar stages read off a fit today --------------------------------
    # (reg_curves() takes positive_level; the assemblers take nothing else from a model object)
    nobs = NA_real_, positive_level = NULL, y_ref = NULL,
    # --- declared exceptions to the payload rule -------------------------------------------------
    # `fit`: NULL unless `compare != "none"`, whose reg_compare_rows() is the ONLY consumer -- and
    # which is also the first reason reg_specs_independent() forces the serial path.
    # `skeleton`: only where it could not be built without a fit (an all-coefficient table with a
    # compound formula); spec 1 derives it and reg_stage_specs() feeds it forward.
    fit = NULL, skeleton = NULL,
    # --- the crude grid's own degrade, folded into the basis at the build tail -------------------
    degraded = FALSE) {
  as.list(environment())
}


# reg_emp_slim() -- drop everything in a crude block that exists only to serve THIS spec's own
# reg_set_obs() and tooltips: the complete-case `$frame`, the fitted crude legs `$fits` and the
# per-(var, level, category) `$grid`. They are 60-100 MB at survey scale and have no reader once
# the builder returns -- reg_stage_assemble() wants `$cols`, and nothing else outside reg_build()
# ever sees the block at all.
#' @keywords internal
#' @noRd
reg_emp_slim <- function(e) {
  if (is.null(e)) return(NULL)
  e[intersect(c("cols", "shape", "effect"), names(e))]
}


# reg_specs_independent() -- CAN THE SPECS BE BUILT WITHOUT ONE ANOTHER? NULL = yes; otherwise the
# REASON, which reg_stage_specs() reports when `parallel` was explicitly asked for, so what was not
# parallelised is never silent (Phase 20f's own rule). The three are facts about the statistics, not
# limitations of the builder, and each is read in the code rather than assumed:
#   1. a model comparison is a test BETWEEN fits -- stats::anova(m_lo, m_hi), or survey's own
#      regTermTest Wald arm. Re-implementing that arithmetic would make tabxplor a second producer
#      of a survey quantity. `compare = "none"` is the default, so this excludes far less than it
#      sounds.
#   2. in comparison mode ONE crude block (spec 1's) is every model column's `obs` AND its gap
#      test's crude leg, and that block carries the heavy frame.
#   3. an all-coefficient table with a compound formula takes its shared skeleton from the first
#      fit. That is narrower than "any compound formula": the marginal builders key on the original
#      variables, so their skeleton is fit-free either way (reg_stage_setup).
#' @keywords internal
#' @noRd
reg_specs_independent <- function(ctx) {
  if (length(ctx$specs) < 2L) return(NULL)
  s <- ctx$shared
  if (!identical(s$compare, "none"))
    return("a model comparison is a test between the fits, so they are built together")
  if (isTRUE(ctx$is_comparison) && isTRUE(s$empirical))
    return("the compared models share one observed (crude) block, built by the first of them")
  if (isTRUE(ctx$skeleton_deferred))
    return("a compound formula takes the shared coefficient skeleton from the first fit")
  NULL
}


# === THE BUILDER ================================================================================
#
# reg_spec_build() -- everything ONE spec contributes, in the order it contributes it. `i` indexes
# ctx$specs; `emp_shared` is the crude block of spec 1 when it serves this one (comparison mode) and
# NULL otherwise -- the loop hands it down, which is exactly why that shape stays serial.
#' @keywords internal
#' @noRd
reg_spec_build <- function(i, ctx, emp_shared = NULL) {
  list2env(reg_ctx_locals(ctx), environment())
  sp <- specs[[i]]

  # --- 1. THE FIT --------------------------------------------------------------------------------
  # Phase 15d: the modelled-level choice is per-outcome (sp$outcome_level); fall back to the shared
  # scalar for any spec that predates it (e.g. a direct reg_build caller).
  inv_sp <- reg_outcome_level_of(sp$outcome_level) %||% outcome_level
  sp_fam <- sp$fit_family
  sp_dox <- isTRUE(sp$est$exp)      # 19m-ii: a one-token view of the estimand, not a field
  if (isTRUE(reref)) {
    # Phase 15b jamovi live reref: `data` arrived at the CANONICAL (natural-first) reference and was
    # releveled to the display one by reg_stage_setup(), which kept the canonical frame as
    # `data_canon`. Fit the digest once on THAT (cached, reference-independent) and reparametrize.
    # sp_fam in the digest key so a binomial vs gaussian outcome never share a digest (Phase 15e).
    digest <- jmvreg_cached(
      fit_cache, "digest", jmvreg_fit_key(sp, data_canon, sp_fam, design_spec),
      function() reg_build_digest(data_canon, sp, sp_fam, design_spec, sp_dox,
                                  inv_sp, conf_level, weighted))
    f <- reg_reref_fit_res(digest, ref, sp, skeleton, conf_level, multiplier = multiplier)
  } else {
    thunk <- function() reg_fit(data, sp$outcome, sp$predictors, sp_fam, design_spec, sp_dox,
                                inv_sp, conf_level, method,
                                trials = sp$trials, formula = sp$formula, multiplier = multiplier,
                                drop_extra = na_shared_vars,
                                add_terms = reg_shape_add(shape_terms, sp$predictors))
    # .fit_cache present but not on the reref path (ame / profile / mnl-vs-rest / compound): cache the
    # RAW fit keyed on the (already display-referenced) data -> a reference change refits.
    f <- if (is.null(fit_cache)) thunk()
         else jmvreg_cached(fit_cache, "fit",
                            jmvreg_fit_key(sp, data, sp_fam, design_spec,
                                           extra = list(method, sp_dox, conf_level, sp$est$effect,
                                                        sp$est$measure, display, multiplier,
                                                        shape_terms)),
                            thunk)
  }
  # The one skeleton that needs a fit (reg_stage_setup could not build it): derive it here and hand
  # it back on the product, so specs 2..S share THIS one rather than each deriving its own.
  skel_out <- NULL
  if (isTRUE(skeleton_deferred) && is.null(skeleton)) {
    skeleton <- reg_skeleton_from_fit(f$fit)
    skel_out <- skeleton
    ctx      <- ctx_update(ctx, list(skeleton = skeleton))
  }

  # --- 2. THE COLUMNS ----------------------------------------------------------------------------
  # Phase 19e: WHICH builder is the estimand row's own `builder`. ⚠ every arm is NAMED and an unknown
  # value aborts: this used to fall through to the coefficient builder, so a typo'd `builder`
  # silently built the wrong column. REG_BUILDERS is the vocabulary and a foreign key ties the two
  # together in both directions (R/zzz-fact-keys.R).
  cols <- switch(sp$est$builder %||% "coef",
                 coef   = reg_cols_coef(f, sp, ctx),
                 ame    = reg_cols_ame(f, sp, ctx),
                 vsrest = reg_cols_vsrest(f, sp, ctx),
                 cli::cli_abort("Internal: unknown estimand builder {.val {sp$est$builder}}."))
  cv0 <- cols[[1]]$label            # the placeholder; see new_reg_spec_product()

  # --- 3. THE FOOTER ROWS ------------------------------------------------------------------------
  # Phase 15e: the GOF stat SET is chosen from this fit's OWN family, so a mixed table shows each
  # outcome's stats (gaussian R2 next to a logit McFadden); test_grid_reg unions the rows + blanks.
  grouped <- sp_fam == "binomial" && !is.null(sp$trials) && !isTRUE(sp$compound)
  gof_rows <- reg_gof_rows(f, sp, cv0, weighted = weighted, grouped = grouped, stats = stats)
  # Phase 18z13 (SS7.2): the per-predictor global test, from the fit already in hand.
  global_rows <- if (isTRUE(want_global)) reg_global_rows(f, sp, shared, cv0) else NULL
  # Phase 18z15: the model checks. They need `data` (the Linearity refit), which is why they are a
  # sibling of the GOF rows rather than part of them.
  check_rows <- reg_check_rows(data, f, sp, shared, stats, cv0, grouped)

  # --- 4. add_n ----------------------------------------------------------------------------------
  # Phase 18z13 (SS7.1): the N behind each predictor level, right after the labels -- where STROBE
  # reads it. A BUILT column, not a `render_extras` display intent: the count needs the model's
  # complete-case frame, which exists only here. WHICH specs get one, and under which name, is
  # reg_stage_setup()'s declared plan (one column per distinct outcome), never a loop-carried skip.
  n_col <- list()
  if (isTRUE(spec_plan$want_n[[i]])) {
    cnt <- reg_level_counts(reg_complete_frame(data, c(sp$outcome, union_predictors,
                                                       reg_design_vars(design_spec))),
                            skeleton, wt = design_spec$wt)
    n_col <- stats::setNames(
      list(fmt(n = cnt$n, wn = cnt$wn, scale = "level_n", display = "n", digits = 0L,
               color = "", color_signif = "ignore", col_var = "n", comp_all = FALSE,
               # in_refrow is NOT decorative: tab_bold_rows() ANDs it across every discriminating
               # column, so omitting it would un-bold every reference row.
               in_refrow = skeleton$is_ref, model_family = sp_fam, role = "n")),
      spec_plan$n_names[[i]])
  }

  # --- 5. THE OBSERVED (CRUDE) BLOCK -------------------------------------------------------------
  own      <- NULL
  degraded <- FALSE
  if (isTRUE(spec_plan$want_emp[[i]])) {
    # ⚠ the two crude predictor sets are TABLE-scalar and come from the declared plan: `num_preds`
    # is emptied when ANY spec has a compound formula (one such spec strips the numeric crude
    # columns from every block, compound or not), so it is not derivable from `sp` alone.
    fac_preds_e <- factor_preds
    num_preds_e <- spec_plan$num_preds
    key_i   <- sp$crude_key
    pos_i   <- if (reg_fam_binary(sp_fam)) f$positive_level else NULL
    mdata_i <- reg_emp_frame(sp$outcome, ctx)          # the same complete-case frame as the model
    var_y_i <- if (sp_fam == "gaussian")
      suppressWarnings(stats::var(as.numeric(mdata_i[[sp$outcome]]), na.rm = TRUE)) else NA_real_
    emp_i   <- reg_empirical(mdata_i, fac_preds_e, sp$outcome, key_i, pos_i, design_spec$wt,
                             trials = sp$trials, ref_category = f$y_ref,
                             conf_level = conf_level, design_spec = design_spec)
    degraded <- isTRUE(attr(emp_i, "degrade"))
    # Which predictors have no closed form and must be fitted? z9: the numeric ones. z10: EVERY
    # predictor under an ordinal outcome (proportional odds is a constraint, so the univariable model
    # is not saturated). reg_crude_saturated() states the rule; nothing here re-derives it.
    fit_preds_e <- c(
      num_preds_e,
      if (!reg_crude_saturated(key_i, TRUE)) fac_preds_e else character(0))
    # The crude fits take the FULL `data` + `drop_extra`, never the pre-filtered frame: a prebuilt
    # survey design's keep_mask is computed from `data` itself (reg_resolve_design).
    # `marginal`: reg_empirical_columns() swaps the crude shape for a marginal one only where the
    # model's own estimand is marginal AND on a probability scale (a gaussian AME IS its coefficient;
    # a poisson AME is additive while its crude shape stays a rate RATIO, which reg_same_estimand()
    # then refuses), so the fit follows the shape it must fill.
    fit_i <- reg_empirical_fit(
      data, fit_preds_e, sp$outcome, sp_fam, design_spec,
      outcome_level = inv_sp,
      conf_level = conf_level, method = method, skeleton = skeleton, multiplier = multiplier,
      other_preds = union_predictors, est = sp$est, wt = design_spec$wt,
      # z17 (D2): always kept. `want_fit` does not decide whether the univariable crude models are
      # FITTED (they are, to fill the crude column) -- only whether the fitted object survives for
      # the gap test's crude leg. Build-time locals; they never reach the jamovi .fit_cache.
      want_fit = TRUE, trials = sp$trials,
      shape_terms = shape_terms,
      marginal = !identical(sp$est$effect, "coefficient") &&
        (reg_fam_binary(sp_fam) || reg_fam_prob(sp_fam)))
    own <- reg_empirical_columns(skeleton, emp_i, fac_preds_e, key_i, sp_fam, sp$est, var_y_i,
                                 conf_level = conf_level, color_signif = color_signif,
                                 color = sp$color, fit_est = fit_i,
                                 # W-D: `n_eff` is written only where something corrected it
                                 weighted = svy_weighted(design_spec, design_spec$wt),
                                 # z16-iiiii (D4): the design df the MODEL columns are already
                                 # referred to, so the crude bracket beside them matches
                                 degf = design_spec$degf %||% Inf)
    # Phase 14w (item 3): the crude companions share the model column's outcome col_var (one span,
    # no border). NOT in comparison mode (the crude block stays a distinct col_var beside the models).
    if (!is_comparison && length(own$cols)) {
      scv <- reg_shared_col_var(sp_fam, sp$outcome, pos_i, cleannames)
      own$cols <- purrr::map(own$cols, ~ set_col_var(.x, scv))
    }
    # Phase 18z8-B: the crude block also carries what the GAP TEST needs -- the frame it was computed
    # on, the factor predictors it covers and the fitted crude legs. All are locals here and nowhere
    # else; reg_emp_slim() drops them again on the way out unless this block is the SHARED one.
    own$frame     <- mdata_i
    own$fac_preds <- fac_preds_e
    own$crude_key <- key_i
    own$fit_preds <- fit_preds_e
    own$fits      <- fit_i$fits
    own$grid      <- emp_i
  }

  # --- 6. `obs` AND `gap_se` ---------------------------------------------------------------------
  # ⚠ `own %||% emp_shared`, and NOT the other way round: in comparison mode only spec 1 builds a
  # block, and reg_stage_assemble() used to hand emp_by_fit[[1]] to every column. The gap SE still
  # comes from THIS column's own fit -- the two estimators' covariance is per model though `obs` is
  # not, which is exactly what makes `color = "adjustment"` work in comparison mode.
  e    <- own %||% emp_shared
  cols <- purrr::map(cols, function(bi) { bi$col <- reg_set_obs(bi, e, f, sp, ctx); bi })

  # --- 7. THE TOOLTIPS ---------------------------------------------------------------------------
  # ⚠ these read `own`, never `e`: a tooltip describes THIS outcome's crude grid, and a compared
  # model that borrowed spec 1's block has none of its own (today's emp_by_fit[[si]], same).
  tips <- list(mnl = NULL, num = NULL)
  if (isTRUE(empirical)) {
    tips$mnl <- reg_spec_tips_mnl(sp, f, own, cols, ctx)
    tips$num <- reg_spec_tips_num(sp, f, own, ctx)
    degraded <- degraded || isTRUE(attr(tips$mnl, "degrade"))
  }

  new_reg_spec_product(
    cols = cols,
    # the payload rule: the block is slimmed unless it is about to be handed to another spec
    emp  = if (isTRUE(share_crude) && i == 1L) own else reg_emp_slim(own),
    n_col = n_col,
    gof_rows = gof_rows, global_rows = global_rows, check_rows = check_rows,
    tips = tips,
    nobs = f$nobs, positive_level = f$positive_level, y_ref = f$y_ref,
    # the other payload exception, and its only consumer
    fit = if (!identical(compare, "none")) f else NULL,
    skeleton = skel_out, degraded = degraded)
}


# === THE TWO TOOLTIP FRAGMENTS ==================================================================
#
# The two cases where a crude number cannot honestly take a column of its own. Both were blocks of
# reg_stage_tips(); they moved here because both read the crude block's HEAVY halves (`$grid`,
# `$frame`), which must not travel back. What they emit instead of a finished row is a fragment
# keyed by SKELETON ROW (and, for the multinomial one, by within-spec COLUMN) -- reg_stage_tips()
# resolves those to the final labels.

# reg_spec_tips_mnl() -- a multinomial outcome: one crude column per category would double the
# table, so the crude % + diff per (category column, predictor level) travel in `empirical_tips`
# and the render appends an "crude:" fragment.
#' @keywords internal
#' @noRd
reg_spec_tips_mnl <- function(sp, f, own, cols, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  if (!identical(sp$fit_family, "multinomial") || length(factor_preds) == 0L) return(NULL)
  idx <- which(!purrr::map_lgl(cols, ~ is.null(.$emp_key)))
  if (length(idx) == 0L) return(NULL)
  is_fac_t <- skeleton$var %in% factor_preds
  # Phase 18z10: read straight off the MERGED crude grid -- reg_empirical_tips() is gone, it was
  # reg_empirical() at a three-part key (measured bit-identical), and keeping two producers of one
  # quantity is exactly the sync-by-comment pair Phase 17 rule 5 forbids. Reuse the block already
  # built for this spec when there is one; otherwise build the grid here.
  tipsd <- if (!is.null(own$grid)) own$grid else
    reg_empirical(reg_emp_frame(sp$outcome, ctx), factor_preds, sp$outcome, "multinomial", NULL,
                  design_spec$wt, ref_category = f$y_ref, conf_level = conf_level,
                  design_spec = design_spec)
  tk  <- reg_skel_key(tipsd$var, tipsd$level, tipsd$category)
  out <- purrr::compact(purrr::map(idx, function(k) {
    b    <- cols[[k]]
    mi2  <- match(reg_skel_key(skeleton$var, skeleton$level, b$emp_key), tk)
    keep <- is_fac_t & !is.na(mi2) & !is.na(tipsd$emp_prop[mi2])
    if (!any(keep)) return(NULL)
    j  <- mi2[keep]
    pr <- tipsd$emp_prop[j]; df <- tipsd$emp_diff[j]
    # 14v-ii: the crude % carries its Wilson CI; a non-reference level also shows its crude
    # difference from the reference and that difference's Newcombe CI (percentage points).
    tibble::tibble(
      col_idx = k, row = which(keep),
      var   = as.character(skeleton$var[keep]),
      tip   = ifelse(skeleton$is_ref[keep],
                     sprintf("crude: %.0f%% [%.0f; %.0f]",
                             pr * 100, tipsd$emp_prop_inf[j] * 100, tipsd$emp_prop_sup[j] * 100),
                     sprintf("crude: %.0f%% (%+.0f pts [%+.0f; %+.0f])",
                             pr * 100, df * 100, tipsd$emp_diff_inf[j] * 100, tipsd$emp_diff_sup[j] * 100)))
  }))
  if (length(out) == 0L) return(NULL)
  structure(purrr::list_rbind(out), degrade = isTRUE(attr(tipsd, "degrade")))
}

# reg_spec_tips_num() -- a numeric predictor's DESCRIPTIVE, because nothing can honestly go in its
# base cell: measured (SS4.1), the univariable fit's only base-scale output is P(Y | X = mean X),
# the MARGINAL rate for every numeric predictor. What IS well defined is the predictor's own
# distribution and its mean within each outcome group, attached to the crude EFFECT column, which
# has visible content (a tooltip on the blank base cell would never be discovered).
#' @keywords internal
#' @noRd
reg_spec_tips_num <- function(sp, f, own, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  if (length(numeric_preds) == 0L) return(NULL)
  if (is.null(own) || is.null(own$shape) || !shape_visible(own$shape)) return(NULL)
  nm <- own$shape$nm                                   # the crude effect column's name
  if (is.na(nm)) return(NULL)
  # ⚠ this was `nm %in% names(tab)` -- "did the crude effect column reach the table?". The table does
  # not exist yet, and the question is local anyway: reg_stage_assemble() splices EVERY entry of
  # `own$cols`, under this same name plus the multi-outcome bracket.
  if (!nm %in% names(own$cols)) return(NULL)
  if (n_outcomes > 1L) nm <- paste0(nm, " [", sp$outcome, "]")
  vars <- intersect(intersect(own$fit_preds, numeric_preds), as.character(skeleton$var))
  if (!length(vars)) return(NULL)
  w  <- if (is.null(design_spec$wt)) NULL else own$frame[[design_spec$wt]]
  yb <- reg_crude_y(own$frame, sp$outcome, sp$fit_family,
                    if (reg_fam_binary(sp$fit_family)) f$positive_level else NULL)
  purrr::list_rbind(purrr::map(vars, function(v) {
    x <- as.numeric(own$frame[[v]])
    m <- reg_weighted_mean(x, w); s <- reg_predictor_sd(x, w)
    # mean(X | Y): for a binary outcome the two groups, else a single overall summary.
    by <- if (reg_fam_binary(sp$fit_family) && length(unique(stats::na.omit(yb))) == 2L)
      sprintf("; mean if yes %s, if no %s",
              format(signif(reg_weighted_mean(x[yb == 1], w[yb == 1]), 3)),
              format(signif(reg_weighted_mean(x[yb == 0], w[yb == 0]), 3)))
    else ""
    k <- which(as.character(skeleton$var) == v)
    tibble::tibble(col = nm, row = k, var = v,
                   tip = sprintf("%s: mean %s (SD %s)%s", v,
                                 format(signif(m, 3)), format(signif(s, 3)), by))
  }))
}


# === THE OTHER TWO PARALLEL AXES ================================================================
#
# reg_build_group() -- ONE `tab_vars` group (axis G). Its body is reg_stage_split()'s own map
# callback, lifted to a top-level namespaced function so tab_pmap() can dispatch it: a unit already
# returned finished, fit-free tibbles, so nothing else had to change.
#' @keywords internal
#' @noRd
reg_build_group <- function(g, sl, tab_vars, specs, fit_cache, shared, data) {
  gmask <- !is.na(data[[tab_vars]]) & data[[tab_vars]] == g
  sub   <- data[gmask, , drop = FALSE]
  # Phase 18z14-iii: the design is NOT subset here, and `shared` rides through untouched. `sub`
  # keeps its `.svy_row`, and reg_resolve_design() subsets the ORIGINAL design by those positions
  # -- ONE subset into ONE row space, R/survey-design.R's own discipline. Two measured defects go
  # with the two deleted lines:
  #   WARNING: utils::modifyList() RECURSES into list elements, and a survey.design IS a list whose
  #     $variables / $cluster / $strata are data.frames -- so handing it a per-group design merged
  #     the two designs COLUMN BY COLUMN ("replacement has 413 rows, data has 800" whenever the
  #     groups are unequal, i.e. normally; silent recycling when they happen to divide).
  #   `[` does not drop rows on a CALIBRATED or PPS design, so the group-local complete-case
  #     positions then landed on the wrong respondents (measured OR 1/2.17 and 1/3.13 against
  #     svyglm's 3.48 and 4.11 on the same groups, with no warning).
  # ⚠ `parallel = FALSE`: a worker never spawns nested daemons, and a group's own model axis is not
  # a second place to dispatch. `fit_cache` rides through -- it is non-NULL only under jamovi, which
  # forces the serial branch anyway (tab_parallel_workers(cache_env =)), so it never crosses a
  # process boundary.
  tg <- reg_build(sub, specs, shared, tab_vars = NULL, .fit_cache = fit_cache,
                  ref = NULL, reref = FALSE, skeleton_data = data, parallel = FALSE)
  # Phase 19g (KEY 6): the group level rides a column NAMED AFTER the split variable -- exactly
  # how a crosstab names its tab_var levels -- so both arms are read by one rule
  # (test_group_cols()) and a predictor name in `var` can no longer be mistaken for a group.
  tst <- get_test(tg); if (!is.null(tst) && nrow(tst) > 0) tst[[tab_vars]] <- as.character(g)
  list(data = tibble::add_column(tibble::as_tibble(dplyr::ungroup(tg)),
                                 "{tab_vars}" := new_lvl(factor(g, levels = sl),
                                                          "tab_var", tab_vars), .before = 1L),
       test = tst)
}


# reg_build_outcome() -- ONE outcome of a `outcome = c(...)` x `predictors = list(...)` call (axis
# R): the outer loop of tab_reg()'s own recursion, as a namespaced worker. `args` is the per-outcome
# argument list built on the main process; `data` rides separately because it is the big object
# tab_pmap() ships once.
#' @keywords internal
#' @noRd
reg_build_outcome <- function(args, data) do.call(tab_reg, c(list(data = data), args))
