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
    # --- the ONE scalar a table-scalar stage reads off a fit -------------------------------------
    # reg_stage_rows() hands products[[1]]$positive_level to reg_curves(); the assemblers take
    # nothing else from a model object.
    positive_level = NULL,
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


# reg_emp_slim() -- a crude block leaves the builder as its COLUMNS and nothing else. Everything
# else in it (the complete-case `$frame`, the fitted crude legs `$fits`, the per-(var, level,
# category) `$grid`, the two predictor sets, the shape and the effect vector) exists to serve
# reg_set_obs() and the tooltips, which both run here; `$frame` + `$fits` alone are 60-100 MB at
# survey scale. reg_stage_assemble() splices `$cols`, and nothing outside reg_build() ever sees a
# block at all.
#' @keywords internal
#' @noRd
reg_emp_slim <- function(e) {
  if (is.null(e)) return(NULL)
  e["cols"]
}


# reg_specs_independent() -- CAN THE SPECS BE BUILT WITHOUT ONE ANOTHER? NULL = yes; otherwise the
# REASON, which reg_stage_specs() reports when `parallel` was explicitly asked for, so what was not
# parallelised is never silent (Phase 20f's own rule). BOTH are facts about the statistics, not
# limitations of the builder, and each is MEASURED rather than assumed:
#   1. a model comparison is a test BETWEEN fits -- stats::anova(m_lo, m_hi), or survey's own
#      regTermTest Wald arm -- so the fit OBJECTS have to meet. Returning them instead was measured
#      (Phase 20f-iiii): one reg_fit() result serialises to 162 MB at n = 200 000 ($model 94 MB;
#      $family / $formula / $terms ~88 MB each, environment captures dragging the whole frame), so
#      shipping S fits back to run one anova() is the expensive route -- and a distilled
#      reg_compare_digest() would make tabxplor a second producer of a survey Wald statistic.
#      DECLARED KEEP. `compare = "none"` is the default, so it excludes far less than it sounds.
#   2. an all-coefficient table with a compound formula takes its shared skeleton from the first
#      fit (reg_skeleton_from_fit). A fit-free twin was measured and refused: it diverges per fitter
#      (names(coef()) is NULL for nnet::multinom and one short for MASS::polr) and would need a
#      second producer of reg_fit()'s own complete-case + fct_drop frame preparation.
#      ⚠ UNREACHABLE from tab_reg(): `compound` is only ever `formula_mode`, which refuses
#      `predictors` and takes one bare LHS, so such a table has exactly ONE spec and returns at the
#      guard above. It stays as the invariant for a direct reg_build() caller.
# The third refusal 20f-iii shipped -- "the compared models share one observed block" -- is GONE:
# the block belongs to the OUTCOME and reg_stage_crude() builds it before the models.
#' @keywords internal
#' @noRd
reg_specs_independent <- function(ctx) {
  if (length(ctx$specs) < 2L) return(NULL)
  s <- ctx$shared
  if (!identical(s$compare, "none"))
    return("a model comparison is a test between the fits, so they are built together")
  if (isTRUE(ctx$skeleton_deferred))
    return("a compound formula takes the shared coefficient skeleton from the first fit")
  NULL
}


# === THE BUILDER ================================================================================
#
# reg_spec_build() -- everything ONE spec contributes, in the order it contributes it. `i` indexes
# ctx$specs; everything else comes off the ctx, so a unit is a pure function of (i, ctx).
#
# The wrapper exists to NAME THE MODEL when one fails. With several models, "which one" is the first
# question, and the answer used to come from purrr::map()'s `i With name: m1.` -- which Phase 20f-iii
# lost when the fit loop stopped being a map. Wrapping here restores it with the model's own LABEL
# instead of an index, and in BOTH branches: `.f_name` is this function, so a daemon runs the
# wrapper too and the serial and parallel messages are the same string (hence `call = NULL`, which
# is also why the two cannot diverge through a call frame).
#' @keywords internal
#' @noRd
reg_spec_build <- function(i, ctx) {
  if (length(ctx$specs) < 2L) return(reg_spec_build_one(i, ctx))
  label <- ctx$specs[[i]]$label
  rlang::try_fetch(
    reg_spec_build_one(i, ctx),
    error = function(cnd) cli::cli_abort("Model {.val {label}} could not be built.",
                                         parent = cnd, call = NULL,
                                         class = "tabxplor_unit_named",
                                         tabxplor_unit = label))
}

#' @keywords internal
#' @noRd
reg_spec_build_one <- function(i, ctx) {
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
      fit_cache, "digest",
      # `multiplier` is in the KEY: the digest's adjusted-prediction sweep is not a linear rescaling
      # of the coefficients it also carries (see reg_build_digest).
      jmvreg_fit_key(sp, data_canon, sp_fam, design_spec, extra = list(multiplier)),
      function() reg_build_digest(data_canon, sp, sp_fam, design_spec, sp_dox,
                                  inv_sp, conf_level, weighted, multiplier = multiplier))
    f <- reg_reref_fit_res(digest, skeleton, conf_level, multiplier = multiplier)
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
  # ONLY where this spec IS an outcome of its own. A one-outcome table has ONE block, built before
  # any model by reg_stage_crude() and read below as `crude` -- see that stage for why the block
  # belongs to the outcome rather than to a model.
  own <- NULL
  if (isTRUE(spec_plan$want_emp[[i]])) {
    mdata_i <- reg_emp_frame(sp$outcome, ctx)          # the same complete-case frame as the model
    pos_i   <- if (reg_fam_binary(sp_fam)) f$positive_level else NULL
    var_y_i <- if (sp_fam == "gaussian")
      suppressWarnings(stats::var(as.numeric(mdata_i[[sp$outcome]]), na.rm = TRUE)) else NA_real_
    own <- reg_crude_block(sp, sp_fam, inv_sp, sp$crude_key, mdata_i, pos_i, f$y_ref, var_y_i, ctx)
    # Phase 14w (item 3): the crude companions share the model column's outcome col_var (one span,
    # no border). NOT in comparison mode (the crude block stays a distinct col_var beside the models).
    if (!is_comparison && length(own$cols)) {
      scv <- reg_shared_col_var(sp_fam, sp$outcome, pos_i, cleannames, sp$trials)
      own$cols <- purrr::map(own$cols, ~ set_col_var(.x, scv))
    }
    own$tips_num <- reg_spec_tips_num(sp, pos_i, own, ctx)
  }
  degraded <- isTRUE(own$degraded)

  # --- 6. `obs` AND `gap_se` ---------------------------------------------------------------------
  # ⚠ `own %||% crude`, and NOT the other way round: with several outcomes each spec has its own
  # block, and it wins. The gap SE still comes from THIS column's own fit -- the two estimators'
  # covariance is per model though `obs` is not, which is exactly what makes `color = "adjustment"`
  # work when several models share one outcome.
  e    <- own %||% crude
  cols <- purrr::map(cols, function(bi) { bi$col <- reg_set_obs(bi, e, f, sp, ctx); bi })

  # --- 7. THE TOOLTIPS ---------------------------------------------------------------------------
  # ⚠ the MULTINOMIAL fragment is the SPEC's: it keys this model's own category columns, so every
  # model of a comparison contributes one. The NUMERIC fragment is the BLOCK's -- it keys the crude
  # effect column, which the models of a one-outcome table share -- and is built with the block.
  tips <- list(mnl = NULL, num = own$tips_num)
  if (emp_on(empirical)) {
    tips$mnl <- reg_spec_tips_mnl(sp, e, cols, ctx)
    degraded <- degraded || isTRUE(attr(tips$mnl, "degrade"))
  }

  # --- 7b. THE PER-CATEGORY CRUDE COLUMNS --------------------------------------------------------
  # A 3+ level outcome has one crude effect per outcome CATEGORY, so `empirical = "column"` draws one
  # crude column per model column. It is spliced HERE, after `obs` and the tooltips: a crude column
  # is not a model column, so it must not be handed its own `obs` nor key the multinomial fragment.
  # Its name and col_var come from the model column it mirrors, which is the only place the two are
  # both in hand -- so the pair sits under one span, in order.
  if (length(e$cat_cols)) {
    cols <- purrr::flatten(purrr::map(cols, function(bi) {
      cc <- cat_get(e$cat_cols, bi$emp_key)
      if (is.null(cc)) return(list(bi))
      list(list(label = paste0("Obs_", bi$label), emp_key = bi$emp_key,
                col = set_col_var(cc, get_col_var(bi$col))),
           bi)
    }))
  }

  new_reg_spec_product(
    cols = cols,
    # THE PAYLOAD RULE: a block leaves as its columns and nothing else -- no frame, no crude legs.
    emp  = reg_emp_slim(own),
    n_col = n_col,
    gof_rows = gof_rows, global_rows = global_rows, check_rows = check_rows,
    tips = tips,
    positive_level = f$positive_level,
    # the other payload exception, and its only consumer
    fit = if (!identical(compare, "none")) f else NULL,
    skeleton = skel_out, degraded = degraded)
}


# === THE TWO TOOLTIP FRAGMENTS ==================================================================
#
# The two cases where a crude number cannot honestly take a column of its own. Both read a crude
# block's HEAVY halves (`$grid`, `$frame`), which never leave reg_build(). What they emit instead of
# a finished row is a fragment keyed by SKELETON ROW (and, for the multinomial one, by within-spec
# COLUMN) -- reg_stage_tips() resolves those to the final labels.
#
# ⚠ THEY BELONG TO DIFFERENT THINGS, and that is not a detail: the multinomial fragment keys ONE
# MODEL's category columns, so every model of a comparison contributes one; the numeric fragment
# keys the crude effect COLUMN, which the models of a one-outcome table share, so it is built once
# with the block (reg_stage_crude / reg_spec_build step 5). Letting each spec build the numeric one
# would re-emit identical rows for a single column -- the duplication Phase 20f-ii deleted.

# reg_spec_tips_mnl() -- a multinomial outcome: one crude column per category would double the
# table, so the crude % + diff per (category column, predictor level) travel in `empirical_tips`
# and the render appends an "crude:" fragment. `e` is the block serving this spec.
#' @keywords internal
#' @noRd
reg_spec_tips_mnl <- function(sp, e, cols, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  if (!identical(sp$fit_family, "multinomial") || length(factor_preds) == 0L) return(NULL)
  idx <- which(!purrr::map_lgl(cols, ~ is.null(.$emp_key)))
  if (length(idx) == 0L) return(NULL)
  is_fac_t <- skeleton$var %in% factor_preds
  # Phase 18z10: read straight off the MERGED crude grid -- reg_empirical_tips() is gone, it was
  # reg_empirical() at a three-part key (measured bit-identical), and keeping two producers of one
  # quantity is exactly the sync-by-comment pair Phase 17 rule 5 forbids.
  # ⚠ Phase 20f-iiii deleted the `else reg_empirical(...)` rebuild that stood here for the compared
  # models, which had no block of their own: a multinomial spec is either the outcome of a
  # several-outcome table (its own block, want_emp) or one of several models on ONE outcome (the
  # stage's block, `crude`), so the grid always exists and the second producer had become dead.
  tipsd <- e$grid
  if (is.null(tipsd)) return(NULL)
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
reg_spec_tips_num <- function(sp, positive_level, own, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  if (length(numeric_preds) == 0L) return(NULL)
  if (is.null(own) || is.null(own$shape)) return(NULL)
  nm <- reg_crude_col_name(own$shape)                  # the crude effect column's name
  if (is.na(nm)) return(NULL)
  # ⚠ this was `nm %in% names(tab)` -- "did the crude effect column reach the table?". The table does
  # not exist yet, and the question is local anyway: reg_stage_assemble() splices EVERY entry of
  # `own$cols`, under this same name plus the multi-outcome bracket. When the crude value rides in
  # the model cell instead, there is no such column and the fragment has nowhere to hang.
  if (!nm %in% names(own$cols)) return(NULL)
  if (n_outcomes > 1L) nm <- paste0(nm, " [", sp$outcome, "]")
  vars <- intersect(intersect(own$fit_preds, numeric_preds), as.character(skeleton$var))
  if (!length(vars)) return(NULL)
  w  <- if (is.null(design_spec$wt)) NULL else own$frame[[design_spec$wt]]
  yb <- reg_crude_y(own$frame, sp$outcome, sp$fit_family,
                    if (reg_fam_binary(sp$fit_family)) positive_level else NULL)
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
  # ⚠ `parallel = FALSE` is THE NESTING RULE, stated once in tab_pmap()'s everywhere() block: a
  # group's own model axis is not a second place to dispatch. `fit_cache` rides through -- it is
  # non-NULL only under jamovi, which forces the serial branch anyway
  # (tab_parallel_workers(cache_env =)), so it never crosses a process boundary.
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
