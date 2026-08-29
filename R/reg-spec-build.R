# PURPOSE: WHAT ONE MODEL CONTRIBUTES TO A tab_reg() TABLE -- the per-spec half of reg_build(), as
#   one declared product.
# ROLE: reg_build()'s stages name the parts of the table; this file answers "which of them are
#   per-model and which are between-models", once and in one place. reg_spec_build() returns
#   everything one model produces, and the stages above it become cross-spec ASSEMBLERS.
#   reg_stage_specs() (R/tab_reg.R) drives it -- the relationship R/reg-empirical.R has to
#   reg_stage_crude().
# KEY CONSTRAINTS:
#   - THE PAYLOAD RULE: the product carries no fit and nothing referencing one, so a unit can cross a
#     process boundary. Two DECLARED exceptions, each matching a shape that is serial anyway
#     (reg_specs_independent) -- `fit`, because a model comparison is a test BETWEEN the fit objects,
#     and the crude block's heavy frame / fits, kept only for the block SHARED with other specs.
#   - THE EAGER STAGE IS WHY A FIT MAY LEAVE: everything only a fitted object can compute -- the
#     model-fit statistics, the global tests, the assumption checks, each crossed pair's test -- is
#     computed while it lives, so the record the jamovi cache stores is fit-free AND complete
#     (reg_fit_distil / reg_fit_rehydrate, R/reg-digest.R). The fetch itself is reg_fit_cached(),
#     the ONE seam this file shares with the crude fits in R/reg-empirical.R.
#   - ⚠ THE ORDER INSIDE THE BUILDER IS PART OF THE OUTPUT: fit + its eager rows -> columns ->
#     footer rows -> the crude block -> obs/gap_se -> tooltips. Three of those can emit a message,
#     and the message stream is compared in order. ⚠ ON A CACHE HIT THE EAGER MESSAGES DO NOT
#     REPLAY, which is what a served table costs and why nothing downstream depends on them.
#   - THE LEVEL COUNTS ARE STAMPED ON THE COLUMNS, not given a column of their own: every column of
#     a fit rests on the same complete cases, so `n` is a property of the estimate. The base-count
#     COLUMN is then synthesised at display time (tab_base_n_cols, R/tab-display.R), which is what
#     lets `n = "range" / "min" / "no"` be chosen after the table is built, and what gives the
#     tooltips and forest_plot(size = "n") one place to read.
#   - ⚠ NO dot-prefixed key on the record: as.list(environment()) defaults to all.names = FALSE and
#     drops them silently.
#   - ⚠ this file sorts BEFORE R/tab_reg.R, so it may hold no top-level code reading a tab_reg.R
#     object (function bodies are fine -- they run after the namespace is built).
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).


# === SECTION: THE PRODUCT =======================================================================
#
# The fourth record constructor, in new_reg_ctx() / new_reg_shared() / new_reg_spec()'s idiom: THE
# FORMALS ARE THE CONTRACT, so a slot is never a missing binding. Two carry a PLACEHOLDER, a worker
# not knowing what exists only once every spec is built: the footer rows' `col` is this spec's
# PRE-make.unique() first column label, overwritten wholesale by the footer stage, and a tooltip
# fragment carries a column LABEL plus a `row` index reg_stage_tips() maps to the final names --
# which is what frees the tooltips from needing the rows stage first.
# ⚠ a placeholder is a NAME, never a position: step 6b below may prepend a crude column, so an index
# taken while the columns are being built no longer points at the same column afterwards.
#' @keywords internal
#' @noRd
new_reg_spec_product <- function(
    # --- the table's columns --------------------------------------------------------------------
    cols = list(), emp = NULL,
    # --- the `test` tibble ----------------------------------------------------------------------
    gof_rows = NULL, global_rows = NULL, check_rows = NULL, cross_rows = NULL,
    # --- meta$empirical_tips --------------------------------------------------------------------
    tips = list(mnl = NULL, num = NULL),
    # --- the ONE scalar a table-scalar stage reads off a fit --------------------------------------
    positive_level = NULL,
    # --- declared exceptions to the payload rule --------------------------------------------------
    # `fit`: NULL unless `compare != "none"`, its only consumer -- and reg_specs_independent()'s
    # first refusal. `skeleton`: only where it could not be built without a fit; spec 1 derives it.
    fit = NULL, skeleton = NULL,
    # --- the crude grid's own degrade, folded into the basis at the build tail --------------------
    degraded = FALSE) {
  as.list(environment())
}


# A crude block leaves as its COLUMNS and nothing else: its frame, its fitted crude legs and its grid
# serve reg_set_obs() and the tooltips, which both run here, and are the heavy part of the payload.
#' @keywords internal
#' @noRd
reg_emp_slim <- function(e) {
  if (is.null(e)) return(NULL)
  e["cols"]
}


# CAN THE SPECS BE BUILT WITHOUT ONE ANOTHER? NULL = yes; otherwise the REASON, reported when
# parallel was actually asked for. BOTH are facts about the STATISTICS, not limits of the
# builder:
#   1. a model comparison is a test BETWEEN fits, so the fit OBJECTS have to meet; a distilled
#      digest would make tabxplor a second producer of a survey Wald statistic. A DECLARED KEEP.
#   2. an all-coefficient table with a compound formula takes its skeleton from the first fit.
#      ⚠ UNREACHABLE from tab_reg(), kept as the invariant for a direct reg_build() caller.
#' @keywords internal
#' @noRd
reg_specs_independent <- function(ctx) {
  # WARNING: `compare` is a DEFAULT now ("auto", from an unnamed `stats`), so a table that asked for
  # no comparison must not land here with anything but "none" -- reg_resolve_args() degrades "auto"
  # before the shared record is built. Getting that wrong costs every table its parallelism AND makes
  # reg_spec_build() keep the fit objects instead of distilling them.
  if (length(ctx$specs) < 2L) return(NULL)
  s <- ctx$shared
  if (!identical(s$compare, "none"))
    return("a model comparison is a test between the fits, so they are built together")
  if (isTRUE(ctx$skeleton_deferred))
    return("a compound formula takes the shared coefficient skeleton from the first fit")
  NULL
}


# === SECTION: THE EAGER STAGE ===================================================================
#
# EVERYTHING THAT NEEDS THE FITTED OBJECT, COMPUTED WHILE IT LIVES. All four are facts about the
# FIT, not about the estimand -- the model-fit statistics, the per-predictor global tests, the
# assumption checks and each crossed pair's test -- so they survive a `measure` / `effect` change
# and the cached record can carry them with the fit thrown away, checks included.
#
# ⚠ THE COLUMN LABEL IS A PLACEHOLDER: it is not known yet (the columns are built after the fit),
# and reg_stage_footer() rewrites `col` wholesale for all four slots anyway. reg_rows_keyed() writes
# this spec's own label back in when the record is read.
#' @keywords internal
#' @noRd
REG_EAGER_COL <- "\r eager"

#' @keywords internal
#' @noRd
reg_fit_eager <- function(f, sp, ctx, grouped) {
  list2env(reg_ctx_locals(ctx), environment())
  cv <- REG_EAGER_COL
  f$gof_rows    <- reg_gof_rows(f, sp, cv, weighted = weighted, grouped = grouped, stats = stats)
  f$global_rows <- if (isTRUE(want_global)) reg_global_rows(f, sp, shared, cv) else NULL
  f$check_rows  <- reg_check_rows(data, f, sp, shared, stats, cv, grouped)
  # is each crossed pair real? one extra ADDITIVE fit per cross (R/reg-cross.R).
  f$cross_rows  <- reg_cross_rows(f, sp, ctx, cv)
  f
}

#' @keywords internal
#' @noRd
reg_rows_keyed <- function(rows, col) {
  if (is.null(rows) || nrow(rows) == 0L) return(NULL)
  rows$col <- col
  rows
}


# === SECTION: THE BUILDER =======================================================================
#
# Everything ONE spec contributes, in the order it contributes it. The wrapper exists to NAME THE
# MODEL when one fails, and `.f_name` is this function, so serial and parallel emit the SAME string.
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
  inv_sp <- reg_outcome_level_of(sp$outcome_level) %||% outcome_level
  sp_fam <- sp$fit_family
  sp_dox <- isTRUE(sp$est$exp)      # a one-token view of the estimand, not a field
  grouped <- reg_is_grouped_binomial(sp_fam, sp$trials, sp$compound)
  # ONE cached object, and it is fit-FREE: the fit and its frame are distilled away
  # (reg_fit_distil), the digest and everything only a live fit could compute stay. So the KEY holds
  # no estimand -- `measure` / `effect` / `display` / `color` / `conf_level` all change without a
  # refit, and reg_fit_rehydrate() rebuilds the frame and rewrites `tidy` on the way out.
  # ⚠ the FIT's own arguments must stay in it: `sp_fam` IS the link key (`rr` / `rd` / `mr` are
  # binomial fits under another link), so dropping `measure` is safe only because `family` is a key
  # member already. `stats` is in because it decides which of the eager rows below are computed.
  thunk <- function() {
    f0 <- reg_fit(data, sp$outcome, sp$predictors, sp_fam, design_spec, sp_dox,
                  inv_sp, conf_level, method,
                  trials = sp$trials, formula = sp$formula, multiplier = multiplier,
                  drop_extra = na_shared_vars,
                  add_terms = c(reg_shape_add(shape_terms, sp$predictors),
                                reg_cross_add(crosses, sp$cross)))
    reg_fit_eager(f0, sp, ctx, grouped)
  }
  # ⚠ `na_shared_vars` travels as `drop_extra =`, not inside `extra`: the key must FINGERPRINT those
  # columns, not merely name them (jmvreg_fit_key).
  key <- if (reg_fit_cacheable(sp, method, compare))
    jmvreg_fit_key(sp, data, sp_fam, design_spec,
                   extra = list(method, shape_terms, anchors, crosses, stats),
                   drop_extra = na_shared_vars %||% character(0)) else NULL
  # ⚠ `multiplier` is NOT a key member: it scales the tidy at finalize and cannot move the fit, so a
  # scaling pick is a HIT that re-reports rather than a refit. It is passed to the SEAM instead.
  f <- reg_fit_cached(fit_cache, key, thunk, data, sp_dox, conf_level, multiplier)
  skel_out <- NULL
  if (isTRUE(skeleton_deferred) && is.null(skeleton)) {
    skeleton <- reg_skeleton_reorder(reg_skeleton_from_fit(reg_digest_revive(f, data)$fit),
                                     levels_order)
    skel_out <- skeleton
    ctx      <- ctx_update(ctx, list(skeleton = skeleton))
  }

  # --- 2. THE COLUMNS ----------------------------------------------------------------------------
  # ⚠ every builder arm is NAMED and an unknown value aborts, never falls through to the default.
  cols <- switch(sp$est$builder %||% "coef",
                 coef   = reg_cols_coef(f, sp, ctx),
                 ame    = reg_cols_ame(f, sp, ctx),
                 vsrest = reg_cols_vsrest(f, sp, ctx),
                 cli::cli_abort("Unknown estimand builder {.val {sp$est$builder}}.", .internal = TRUE))
  cv0 <- cols[[1]]$label            # the placeholder; see new_reg_spec_product()
  # The N behind each level, on THIS model's complete cases, stamped on every column it belongs to:
  # a stored fact, not a column of its own, so the base-count column is synthesised at display time
  # (tab_base_n_cols()) and the tooltips, the footer and forest_plot(size = "n") all read one place.
  # A numeric predictor keeps NA -- on a listwise-complete frame its count IS the model N.
  frame <- reg_complete_frame(data, c(sp$outcome, union_predictors,
                                      reg_cross_parents(crosses),
                                      reg_design_vars(design_spec)))
  cnt  <- reg_level_counts(frame, skeleton, wt = design_spec$wt, crosses = crosses)
  cnt  <- reg_constant_count(cnt, frame, sp, skeleton, design_spec$wt, anchors)
  cols <- purrr::map(cols, function(cc) { cc$col <- set_n(set_wn(cc$col, cnt$wn), cnt$n); cc })

  # --- 3. THE FOOTER ROWS ------------------------------------------------------------------------
  # Computed by the EAGER STAGE while the fit was alive (reg_fit_eager, below); read back here and
  # keyed onto this spec's first column. Their `col` is a placeholder either way -- reg_stage_footer()
  # rewrites it wholesale for all four slots.
  gof_rows    <- reg_rows_keyed(f$gof_rows, cv0)
  global_rows <- reg_rows_keyed(f$global_rows, cv0)
  check_rows  <- reg_rows_keyed(f$check_rows, cv0)
  cross_rows  <- reg_rows_keyed(f$cross_rows, cv0)

  # --- 4. THE OBSERVED (CRUDE) BLOCK -------------------------------------------------------------
  # ONLY where this spec IS an outcome of its own; a one-outcome table's block was built before any
  # model by reg_stage_crude() and is read below as `crude`.
  own <- NULL
  if (isTRUE(spec_plan$want_emp[[i]])) {
    mdata_i <- reg_emp_frame(sp$outcome, ctx)          # the same complete-case frame as the model
    pos_i   <- if (reg_fam_binary(sp_fam)) f$positive_level else NULL
    var_y_i <- if (sp_fam == "gaussian")
      suppressWarnings(stats::var(as.numeric(mdata_i[[sp$outcome]]), na.rm = TRUE)) else NA_real_
    own <- reg_crude_block(sp, sp_fam, inv_sp, sp$crude_key, mdata_i, pos_i, f$y_ref, var_y_i, ctx)
    if (!is_comparison && length(own$cols)) {
      scv <- reg_shared_col_var(sp_fam, sp$outcome, pos_i, cleannames, sp$trials)
      own$cols <- purrr::map(own$cols, ~ set_col_var(.x, scv))
    }
    own$tips_num <- reg_spec_tips_num(sp, pos_i, own, ctx)
  }
  degraded <- isTRUE(own$degraded)

  # --- 5. `obs` AND `gap_se` ---------------------------------------------------------------------
  # ⚠ `own %||% crude`, NOT the other way round: with several outcomes each spec has its own block,
  # and it wins. The gap SE still comes from THIS column's own fit -- the covariance is per model
  # though `obs` is not, which is what makes `color = "adjustment"` work when models share one
  # outcome.
  e    <- own %||% crude
  cols <- purrr::map(cols, function(bi) { bi$col <- reg_set_obs(bi, e, f, sp, ctx); bi })
  # ⚠ AND THE DISPLAY IS WRITTEN AGAIN HERE, which is the first point every field a model cell can
  # print exists. display_write_col() drops a bracket group whose field is void on every row, so a
  # template naming `obs` or `gap` -- the `est_obs` preset, or a user's own "{est} ({obs})" -- was
  # silently pruned back to "{est}" when the column builders wrote it, before step 5 filled them.
  # The columns spliced in at 6b are crude ones, which keep the layout their own builder gave them.
  disp <- reg_display_of(display, empirical, is_comparison)
  if (!is.null(disp))
    cols <- purrr::map(cols, function(bi) { bi$col <- reg_apply_display(bi$col, disp); bi })

  # --- 6. THE TOOLTIPS ---------------------------------------------------------------------------
  # ⚠ the MULTINOMIAL fragment is the SPEC's, the NUMERIC one the BLOCK's -- see the section below.
  tips <- list(mnl = NULL, num = own$tips_num)
  if (emp_on(empirical)) {
    tips$mnl <- reg_spec_tips_mnl(sp, e, cols, ctx)
    degraded <- degraded || isTRUE(attr(tips$mnl, "degrade"))
  }

  # --- 6b. THE PER-CATEGORY CRUDE COLUMNS --------------------------------------------------------
  # `empirical = "column"` draws one crude column per model column. Spliced HERE, after `obs` and the
  # tooltips: a crude column is not a model column, so it takes no `obs` and keys no fragment. Its
  # name and col_var come from the model column it mirrors.
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
    emp  = reg_emp_slim(own),
    gof_rows = gof_rows, global_rows = global_rows, check_rows = check_rows,
    cross_rows = cross_rows,
    tips = tips,
    positive_level = f$positive_level,
    fit = if (!identical(compare, "none")) f else NULL,
    skeleton = skel_out, degraded = degraded)
}


# === SECTION: THE TWO TOOLTIP FRAGMENTS =========================================================
#
# The two cases where a crude number cannot honestly take a column of its own; both read a crude
# block's HEAVY halves, which never leave reg_build().
# ⚠ THEY BELONG TO DIFFERENT THINGS: the multinomial fragment keys ONE MODEL's category columns, so
# every model of a comparison contributes one; the numeric fragment keys the crude effect COLUMN,
# which the models of a one-outcome table SHARE, so it is built once with the block.

#' @keywords internal
#' @noRd
reg_spec_tips_mnl <- function(sp, e, cols, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  if (!identical(sp$fit_family, "multinomial") || length(factor_preds) == 0L) return(NULL)
  idx <- which(!purrr::map_lgl(cols, ~ is.null(.$emp_key)))
  if (length(idx) == 0L) return(NULL)
  is_fac_t <- skeleton$var %in% factor_preds
  tipsd <- e$grid
  if (is.null(tipsd)) return(NULL)
  tk  <- reg_skel_key(tipsd$var, tipsd$level, tipsd$category)
  out <- purrr::compact(purrr::map(idx, function(k) {
    b    <- cols[[k]]
    mi2  <- match(reg_skel_key(skeleton$var, skeleton$level, b$emp_key), tk)
    keep <- is_fac_t & !is.na(mi2) & !is.na(tipsd$emp_prop[mi2])
    if (!any(keep)) return(NULL)
    j  <- mi2[keep]
    pr <- tipsd$emp_prop[j]
    tibble::tibble(
      # ⚠ the column is named, not numbered: step 6b below may prepend a crude column, which would
      # shift any index taken here (reg_stage_tips() resolves it through `product_labels`).
      col_label = b$label, row = which(keep),
      var   = as.character(skeleton$var[keep]),
      # THE OBSERVED LEVEL AND ITS INTERVAL, and nothing else: the cell already folds in the crude
      # odds ratio, so a risk difference here would be a third estimand contradicting the one beside
      # it. Rendered as an ordinary fmt cell.
      tip   = tip_crude_level(pr, tipsd$emp_prop_inf[j], tipsd$emp_prop_sup[j]))
  }))
  if (length(out) == 0L) return(NULL)
  structure(purrr::list_rbind(out), degrade = isTRUE(attr(tipsd, "degrade")))
}

# A numeric predictor's DESCRIPTIVE, because nothing can honestly go in its base cell: a univariable
# fit's only base-scale output is the MARGINAL rate, the same for every numeric predictor. Its own
# distribution and its mean per outcome group ARE well defined, and hang on the crude EFFECT column,
# which has visible content where a blank base cell's tooltip would never be found.
#' @keywords internal
#' @noRd
reg_spec_tips_num <- function(sp, positive_level, own, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  if (length(numeric_preds) == 0L) return(NULL)
  if (is.null(own) || is.null(own$shape)) return(NULL)
  nm <- reg_crude_col_name(own$shape)
  if (is.na(nm)) return(NULL)
  # ⚠ the question is local: reg_stage_assemble() splices EVERY entry of `own$cols` under this name.
  # Where the crude value rides in the model cell instead, the fragment has nowhere to hang.
  if (!nm %in% names(own$cols)) return(NULL)
  if (n_outcomes > 1L) nm <- paste0(nm, " [", sp$outcome, "]")
  # WHICH numeric variable each row BLOCK is about: itself for an ordinary predictor, the MODIFIED
  # parent for a nested cross block, whose rows are that variable's slopes. ⚠ the block key of a
  # cross is not a column at all, and reading it printed "age*tvhours: mean NA (SD NA)".
  blocks <- intersect(unique(as.character(skeleton$var)), sp$row_vars)
  if (!length(blocks)) return(NULL)
  vars   <- stats::setNames(vapply(blocks, function(b) {
    rec <- reg_cross_of(crosses, b)
    if (is.null(rec)) b else if (identical(rec$arm, "nested")) rec$modified else NA_character_
  }, character(1)), blocks)
  # the test is on the COLUMN, not on `numeric_preds`: a crossed slope's parent is supplied by the
  # interaction term, so it is never listed among the model's own predictors.
  vars <- vars[!is.na(vars) & vars %in% names(own$frame)]
  if (length(vars))
    vars <- vars[!vapply(vars, function(v) reg_is_factor_var(own$frame[[v]]), logical(1))]
  if (!length(vars)) return(NULL)
  w  <- if (is.null(design_spec$wt)) NULL else own$frame[[design_spec$wt]]
  yb <- reg_crude_y(own$frame, sp$outcome, sp$fit_family,
                    if (reg_fam_binary(sp$fit_family)) positive_level else NULL)
  purrr::list_rbind(purrr::compact(purrr::map(names(vars), function(b) {
    v <- unname(vars[[b]])
    # ⚠ THE ONE PLACE a shifted column must be read back in the user's own units: `ref` anchors every
    # continuous predictor before the fit, so the stored column is x - anchor. Every ESTIMATE is
    # invariant under that shift; only a descriptive reading of the values is not, and this tooltip
    # and the linearity panel's x axis (R/plots.R) are the two.
    x <- as.numeric(own$frame[[v]]) + reg_anchor_of(anchors, v)
    m <- wtd_mean(x, w); s <- wtd_sd(x, w)
    # A TOOLTIP NEVER SHOWS A FIELD IT DOES NOT HAVE: no fragment at all rather than a line of NAs.
    if (!is.finite(m) || !is.finite(s)) return(NULL)
    m1 <- if (reg_fam_binary(sp$fit_family) && length(unique(stats::na.omit(yb))) == 2L)
      c(wtd_mean(x[yb == 1], w[yb == 1]), wtd_mean(x[yb == 0], w[yb == 0]))
    else c(NA_real_, NA_real_)
    # the tags are the hover's own (`mean`, `sd`), so this descriptive reads like every other line.
    by <- if (all(is.finite(m1)))
      sprintf(" ; yes %s / no %s", format(signif(m1[[1]], 3)), format(signif(m1[[2]], 3)))
    else ""
    k <- which(as.character(skeleton$var) == b)
    tibble::tibble(col = nm, row = k, var = b,
                   tip = sprintf("%s: mean %s (sd %s)%s", v,
                                 format(signif(m, 3)), format(signif(s, 3)), by))
  })))
}


# === SECTION: THE OTHER TWO PARALLEL AXES =======================================================
#
# ONE `tab_vars` group and ONE outcome, lifted to top-level namespaced functions so tab_pmap() can
# dispatch them BY NAME. Both already returned finished, fit-free objects.
#' @keywords internal
#' @noRd
reg_build_group <- function(g, sl, tab_vars, specs, fit_cache, shared, data) {
  gmask <- !is.na(data[[tab_vars]]) & data[[tab_vars]] == g
  sub   <- data[gmask, , drop = FALSE]
  # ⚠ the design is NOT subset here and `shared` rides through untouched: `sub` keeps its `.svy_row`
  # and reg_resolve_design() subsets the ORIGINAL design -- ONE subset into ONE row space. WARNING:
  # modifyList() RECURSES, and a survey.design IS a list of data.frames, so a per-group design merges
  # the two COLUMN BY COLUMN; and `[` does not drop rows on a CALIBRATED or PPS design.
  # THE NESTING RULE needs nothing here: tab_pmap() turns the option off around its whole map, so a
  # group's model axis cannot become a second place to dispatch.
  tg <- reg_build(sub, specs, shared, tab_vars = NULL, .fit_cache = fit_cache,
                  skeleton_data = data)
  tst <- get_test(tg); if (!is.null(tst) && nrow(tst) > 0) tst[[tab_vars]] <- as.character(g)
  # the group's OWN observed curves ride up beside its data: reg_stage_split() binds them into one
  # group-keyed `meta$assumptions`, so each group's base-count cell draws its own sparkline.
  list(data = tibble::add_column(tibble::as_tibble(dplyr::ungroup(tg)),
                                 "{tab_vars}" := new_lvl(factor(g, levels = sl),
                                                          "tab_var", tab_vars), .before = 1L),
       test = tst, assumptions = get_assumptions(tg))
}


#' @keywords internal
#' @noRd
reg_build_outcome <- function(args, data) do.call(tab_reg, c(list(data = data), args))
