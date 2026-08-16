# R/reg-resolve.R -- Phase 19m-ii: THE argument boundary of tab_reg().
#
# WHY THIS FILE EXISTS. Phase 19i gave the four CROSSTAB producers one boundary
# (tab_resolve_common_args(), R/tab-resolve.R). The regression producer never got one: 738 of
# tab_reg()'s 821 lines resolved 28 arguments before a single reg_build() call, and 30 of the
# package's ~190 user messages lived in that region -- 48 % of them in 13 % of the file. Inside it
# sat twelve ad-hoc local closures and two near-identical spec literals, all of them there for ONE
# reason: the per-outcome facts (family, estimand, trials, outcome level, crude key, colour, label,
# effect word) were never materialised, so each was recomputed on demand by a function closing over
# a frame that later blocks kept mutating.
#
# THE SHAPE. One entry point, reg_resolve_args(), composed of six private stages. tab_reg() calls it
# once and receives new_reg_args() -- the typed record, on new_reg_shared()'s idiom (the FORMALS are
# the contract, the body is as.list(environment()), the globalVariables mirror is derived beneath).
#
#   S1 reg_validate_args()      the checks that are PURE
#   S2 reg_prepare_data()       design unwrap / formula / predictors dispatch / labelled / shape /
#                               all_predictors / tab_vars -- and every rewrite of `data`
#   S3 reg_resolve_estimands()  the PER-DEPENDENT TABLE (family / estimand / trials / level / crude)
#   S4 reg_resolve_output()     display / colour / the empirical degrade -- and the notes, LAST
#   S5 reg_resolve_fit_plan()   na / reref / the reference relevel / multiplier / shape terms
#   S6 reg_resolve_specs()      the labels, the positive levels and the one new_reg_spec() call site
#
# ⚠ WHY `data` IS INSIDE THE BOUNDARY, NOT LIFTED OUT OF IT. tab_resolve_common_args() can be pure
# because tab()'s arguments are answerable without the data frame -- the data-outcome half is
# tab_setup()'s job, downstream. tab_reg() has no such split, and not incidentally: `family = "auto"`
# is ANSWERED by the data, `trials = TRUE` is, `multiplier = "sd"` IS a number measured from it,
# `shape` recodes it and `ref` relevels it -- and the relevel needs the multinomial outcomes
# the family stage resolves. A separate reg_prepare_data() that tab_reg() called itself would put
# the ORDERING in the caller: a second place it can be got wrong. The prepared `data` and
# `design_obj` are therefore DECLARED FIELDS of the returned record, exactly as `data` is a declared
# field of new_ctx() (R/tab.R) that pipeline stages rewrite.
#
# ⚠ WHY THERE IS NO `REG_ARG_VALUES` TABLE. TAB_ARG_VALUES exists because FIVE producers had each
# re-implemented one boundary and drifted (`tot`'s expansion written four times, `na`'s allow-list
# three times with three contents). tab_reg() is ONE producer with two forwarding wrappers, and its
# vocabularies are already declared once each -- REG_USER_FAMILIES / REG_EFFECTS_VALUES /
# REG_MEASURES_VALUES / REG_SHAPES / reg_stat_keys() / REG_MULTIPLIER_KEYWORDS /
# REG_DISPLAY_SHORTHANDS. And TAB_ARG_VALUES' own exclusion rule (R/tab-resolve.R: "validating it
# means RESOLVING it, and that is resolve_ci_value()'s job -- one validator, in the one place that
# can also rewrite the value") disqualifies eleven of the fifteen candidates: `family`, `effect`,
# `measure`, `shape`, `color`, `display`, `multiplier`, `trials`, `ref`, `cleannames` and `na`
# all REWRITE what they validate, so each is checked by its own resolver. `method` / `compare` / `na`
# are validated by match.arg AGAINST THEIR FORMALS, which is tighter single-sourcing than a table.
# What is left is reg_validate_args() below: five checks, each CALLING an existing single source.


# === S1: the pure checks =========================================================================
# Only what needs no data, no other resolution and no rewrite. Everything else is a `ci` (see above)
# and lives with its resolver.
#' @keywords internal
#' @noRd
reg_validate_args <- function(conf_level = NULL, stats = NULL, color_signif = NULL,
                              empirical = NULL, add_n = NULL, stars = NULL) {
  # 1. `conf_level` -- the reg producer had NEVER validated it, so `conf_level = 95` reached the
  # interval engine as a probability. tab_validate_args() has done this for the crosstab producers
  # since 19i and already carries the "did you mean 0.95?" hint: reuse, do not rewrite.
  tab_validate_args("tab_reg", conf_level = conf_level)

  # 2. `stats` -- reg_validate_stat_keys() has carried `arg = "stats"` as its DEFAULT since 19g and
  # was called from exactly one site, for `check`. The `stats` path silently FILTERED instead
  # (reg_footer_stats: `stats[stats %in% reg_stat_keys()]`), so a typo produced a missing footer row
  # with no message at all. `NULL` / `TRUE` / `FALSE` / "all" / "none" are the declared special
  # values and pass through untouched.
  # Phase 20c: it is the KEYS that are validated -- a named entry carries its key in the NAME
  # (`c(compare_baseline = "M1")`), so reg_stats_keys_of() is what the two readers share.
  if (is.character(stats) && !identical(stats, "all") && !identical(stats, "none"))
    reg_validate_stat_keys(reg_stats_keys_of(stats), arg = "stats")

  # 3. `color_signif` -- unvalidated on this path entirely. tab() goes through
  # normalize_color_spec(), which aborts; tab_reg() put the raw value into new_reg_shared() ->
  # reg_column() -> fmt(color_signif = ), and fmt() casts without validating -- so
  # `color_signif = "grey"` built a table whose stored policy was a value no consumer knows, and
  # which therefore silently painted as "ignore". ONE vocabulary (R/fmt_class.R), three readers.
  if (!is.null(color_signif)) {
    cs <- as.character(color_signif)[1]
    if (!identical(cs, "color_all_signif") && !cs %in% COLOR_SIGNIF_VALUES)
      cli::cli_abort(c("Unknown {.arg color_signif} value {.val {cs}}.",
                       "i" = "Valid: {.val {COLOR_SIGNIF_VALUES}}."), call = NULL)
  }

  # 4. THE BASELINE MODEL HAS NO CHECK HERE ANY MORE, and that is the point of Phase 20c's merge.
  # It used to be its own argument, so its SHAPE was a thing a user could get wrong (a vector, a
  # zero, a logical) and its RELATION to `compare` another ("you named a baseline but asked for no
  # comparison"). It is the VALUE of a `stats` key now, so the grammar guarantees a single string --
  # naming the baseline IS asking for the comparison, and both messages became unrepresentable.
  # What is left -- an empty or NA value, and "which model is that" -- belongs to the two places that
  # can act on it: reg_resolve_stats() rewrites, reg_compare_rows() matches the model labels.

  # 5. the scalar logicals. NULL passes (several of them mean "read the option" upstream).
  for (nm in c("empirical", "add_n", "stars")) {
    v <- get(nm)
    if (is.null(v)) next
    if (length(v) != 1L || !is.logical(v) || is.na(v))
      cli::cli_abort(c("{.arg {nm}} must be a single {.code TRUE} or {.code FALSE}.",
                       "x" = "Got {.val {v}}."), call = NULL)
  }
  invisible(TRUE)
}


# === S2: everything that touches `data` ==========================================================
# The design unwrap, the formula escape hatch, the predictors dispatch, the labelled conversion, the
# `shape` recode, the predictor union and the tab_vars validation -- i.e. every block that rewrites
# `data` or decides what the model's variables ARE, in the one order they may run in.
#
# ⚠ THE ORDER IS THE DESIGN. Each of these was written where it is for a reason that is now stated:
#   H2  the design unwrap BEFORE the formula: reg_parse_formula() calls terms(formula, data = data),
#       and on a prebuilt design `data` is not a data frame.
#   H3  the formula BEFORE everything else: it REWRITES `outcome` and `predictors`, so the
#       labelled scan, the shape validation and `all_predictors` would otherwise see the wrong names.
#   H4  labelled BEFORE shape: reg_resolve_shape() refuses a factor predictor, and
#       tab_apply_val_labels() is what turns a labelled numeric into one.
#   H5  labelled BEFORE the family stage: a coded 0/1 outcome must be a real factor when
#       reg_detect_family() looks at it, or it is detected as gaussian.
#   H6  labelled BEFORE the trials stage: reg_trials_observed_max() answers NA for a factor.
#   H18 shape BEFORE the frozen multiplier frame: a quantile-cut predictor's SD would otherwise be
#       measured on the raw numeric.
#   H23 tab_vars validation BEFORE the family/estimand stages. It used to run 500 lines later, so
#       "`tab_vars` is not a column of `data`" arrived after up to eight informs about families,
#       colours and forcings the call was never going to produce.
#' @keywords internal
#' @noRd
reg_prepare_data <- function(data, outcome, predictors, tab_vars = NULL, wt = NULL,
                             shape = NULL, family = "auto") {
  # --- C: a PREBUILT survey design as `data` (Phase 12g / 18z14-i) ------------------------------
  # THE shared boundary (R/survey-design.R) extracts its model frame for family-detect / reference /
  # skeleton and materialises the design's own weights as a column; the design itself still drives
  # every fit.
  # WARNING: `wt` MUST become that column, not NULL. It used to be nulled here, and since ~11 sites
  #   read `design_spec$wt`, a design-weighted Model_* column sat beside an UNWEIGHTED crude Obs_*
  #   column, a sample-average (not population-average) AME, an unweighted frozen SD for
  #   `multiplier = "sd"`, unweighted influence weights in the gap test, and no "Weighted by" footer
  #   at all (D1 / D2 / D8 of dev/full_survey_design_scope.md S2.3). The FIT is unaffected:
  #   reg_resolve_design() branches on design_spec$design first, so a non-NULL `wt` never rebuilds one.
  svy <- svy_unwrap_data(data, "tab_reg")
  design_obj <- svy$spec$design
  if (!is.null(svy)) {
    # Phase 18z16-i (W10): one rule across the package -- `wt` beside a design ABORTS (it used to
    # be silently ignored here with a note nothing downstream could see).
    svy_abort_wt_design(!is.null(wt))
    data <- svy$data
    wt   <- svy$spec$wt
  }
  stopifnot(is.data.frame(data))
  weighted <- svy_weighted(list(design = design_obj), wt)
  # the design's own degrees of freedom (#PSU - #strata), captured at THE boundary and carried to
  # every interval's critical value; NULL for a plain frame.
  degf <- svy$spec$degf

  # --- D: the formula escape hatch (D9) ---------------------------------------------------------
  # A SIMPLE formula (bare response ~ bare main-effect vars) reduces losslessly to the
  # outcome+predictors character path; a COMPOUND one (interactions / poly() / I() / calls) is fit
  # verbatim with a fit-read skeleton.
  formula_mode <- FALSE
  raw_formula  <- NULL
  if (rlang::is_formula(outcome)) {
    if (!is.null(predictors)) {
      cli::cli_abort("Provide either a formula in {.arg outcome} or {.arg predictors}, not both.",
                     call = NULL)
    }
    parsed <- reg_parse_formula(outcome, data)
    if (!parsed$lhs_is_name && identical(family, "auto")) {
      cli::cli_abort(c("Cannot auto-detect {.arg family} from a transformed formula response.",
                       "i" = "Set {.arg family} explicitly when the response is not a bare variable."),
                     call = NULL)
    }
    outcome <- parsed$outcome
    if (parsed$simple) {
      predictors <- parsed$labels                       # main-effect vars, in formula order
    } else {
      formula_mode <- TRUE
      raw_formula  <- parsed$formula
      predictors   <- parsed$predictors                 # RHS bare vars (ref= / drop_na)
    }
  } else if (is.null(predictors)) {
    cli::cli_abort(c("{.arg predictors} is required.",
                     "i" = "Or pass a model formula as {.arg outcome}, e.g. {.code y ~ x1 + x2}."),
                   call = NULL)
  }
  stopifnot(is.character(outcome), length(outcome) >= 1L)

  # --- E: predictors dispatch -------------------------------------------------------------------
  # a named list -> model-comparison ; a character vector -> one model per outcome
  is_comparison <- is.list(predictors)
  if (is_comparison && length(outcome) != 1L) {
    cli::cli_abort(c("With a list of models in {.arg predictors}, {.arg outcome} must be a single name.",
                     "i" = "A vector of outcomes is for the one-model-per-outcome mode."), call = NULL)
  }
  if (!is_comparison && !is.character(predictors)) {
    cli::cli_abort("{.arg predictors} must be a character vector or a named list of character vectors.",
                   call = NULL)
  }

  # --- F: labelled interop (Phase k) ------------------------------------------------------------
  # Capture variable labels (BEFORE conversion strips them) then convert labelled (haven/labelled)
  # predictors / outcome / split columns to value-label factors -- so family detection, the
  # skeleton and the fit all see real factors. Covers a prebuilt survey design's variables too.
  # `var_labels` rides `shared` into the reg table's meta$vars for the opt-in name display-swap.
  # Idempotent / no-op for non-labelled data.
  reg_lbl_vars   <- intersect(unique(c(as.character(outcome),
                                       unlist(predictors, use.names = FALSE),
                                       as.character(tab_vars))), names(data))
  var_labels <- capture_var_labels(data, reg_lbl_vars)
  data       <- tab_apply_val_labels(data, reg_lbl_vars)
  if (!is.null(design_obj)) design_obj$variables <- data

  # --- G: `shape` (Phase 18z15) -----------------------------------------------------------------
  # Fit a continuous predictor as something other than a line. THE boundary, and there is only one:
  # a shape either RECODES the column here (log / sqrt / quantile groups) or emits ONE extra model
  # term (quadratic). Before family detection, the reference relevel, the frozen multiplier SD and
  # the skeleton, so every one of them sees the predictor AS FITTED -- a quantile-cut `age` is a
  # factor from this line on, and inherits the entire factor machinery (one estimate per group, a
  # saturated crude twin, per-level N and colours) with no code of its own. The design's own
  # variables are recoded too, exactly as reg_relevel_design() does: a prebuilt survey design reads
  # its columns off `$variables`, not off `data`.
  reg_shapes   <- reg_resolve_shape(shape, data, unlist(predictors, use.names = FALSE))
  shape_labels <- character(0)
  if (length(reg_shapes) > 0L) {
    sh   <- reg_shape_apply(data, reg_shapes, w = wt)
    data <- sh$data
    shape_labels <- sh$labels
    if (!is.null(design_obj)) design_obj$variables <- data
  }

  # --- R: the predictor union -------------------------------------------------------------------
  all_predictors <- if (is_comparison) unique(purrr::flatten_chr(predictors)) else predictors

  # --- W: tab_vars (Phase 12g) -----------------------------------------------------------------
  # One grouping column, distinct from the outcome / predictors, that a model is fitted within each
  # level of. Must be a factor / character; reg_build recurses per level and stacks.
  reg_check_tab_vars(data, tab_vars, outcome, all_predictors, formula_mode)

  list(data = data, design_obj = design_obj, wt = wt, weighted = weighted,
       outcome = outcome, predictors = predictors, all_predictors = all_predictors,
       is_comparison = is_comparison, formula_mode = formula_mode, raw_formula = raw_formula,
       reg_shapes = reg_shapes, shape_labels = shape_labels, var_labels = var_labels, degf = degf)
}

# === S3: the per-outcome TABLE =================================================================
# THE fact table of a tab_reg() call: one row per outcome, in `outcome` order, carrying every
# per-outcome fact the rest of the boundary and the whole of reg_build() need.
#
#   dep         the outcome name
#   family      the USER-facing outcome family, "auto" resolved by reg_detect_family()
#   rr_promoted did `family = "poisson"` on a binary outcome mean `measure = "ratio"` (Phase 18z3)
#   est         the resolved ESTIMAND row (R/reg-estimand.R) -- the single answer to which model to
#               fit, whether to exponentiate, the header word, the stored `scale`, the crude
#               companion and the marginaleffects contrast
#   fit_family  est$fit -- the internal LINK key ("rr" / "rd" / "mr" included)
#   trials      the resolved grouped-binomial item count, NA = "not a grouped binomial"
#   outcome_level  the level of the outcome the user singled out: MODELLED on a binomial, the
#               BASELINE category on a multinomial (REG_FAMILIES declares which). NA = the family's
#               own default.
#   crude_key   which observed counterpart this outcome has, NA = none (the z10 stored fact)
#
# WHY A TABLE. Nine of tab_reg()'s twelve local closures existed because these facts were never
# materialised: `family_for`, `est_for` (with its own local() memo cache), `do_exp_for`,
# `effect_shape_for`, `eff_word_for`, `trials_for` (DEFINED TWICE -- an off default and an on-path
# redefinition nested two `if`s deep), `trials_auto`, `inverse_for` and `color_for` each re-derived
# one column on demand, from a frame later blocks kept mutating. Computing the rows once makes the
# memo cache unnecessary by construction, and turns the survivors into what they should always have
# been: one-line LOOKUPS, not resolutions.
#
# ⚠ ORDER: the family must be resolved before the estimand (reg_estimand() takes it), before the
# trials block (which asks "is any outcome binomial?" and gates the count on it), and before the
# survey-feasibility refusal. That is why they are one stage and not three.
#' @keywords internal
#' @noRd
reg_resolve_estimands <- function(data, outcome, family = "auto", effect = "coefficient",
                                  measure = "auto", trials = NULL,
                                  outcome_level = NULL,
                                  formula_mode = FALSE, weighted = FALSE) {
  n <- length(outcome)

  # --- H: `family`, per outcome ---------------------------------------------------------------
  # Phase 15e: one call can model several outcomes with DIFFERENT families (one column-group per
  # outcome). Accepts "auto" (detect each outcome), a scalar, a positional vector or a named vector
  # -- through reg_per_outcome(), the ONE declared slicer (R/reg-estimand.R). Auto-detection stays
  # honest and per-outcome (an ambiguous integer count aborts for THAT outcome, not the table).
  rr_promoted <- rep(FALSE, n)
  families    <- vapply(seq_len(n), function(i) {
    d <- outcome[[i]]
    f <- reg_per_outcome(family, d, i, "auto")
    if (identical(f, "auto")) f <- reg_detect_family(data, d)
    if (!f %in% REG_USER_FAMILIES)
      cli::cli_abort(c("{.arg family} for {.val {d}} must be one of {.or {.val {REG_USER_FAMILIES}}}.",
                       "x" = "Got {.val {f}}."), call = NULL)
    # DESIGN (Phase 18z3): THE resolution site for the modified-Poisson path. An explicit
    # poisson/quasipoisson on a BINARY outcome is not a count model -- it is Zou (2004)'s modified
    # Poisson, whose exp(coef) is a RISK RATIO, not an incidence-rate ratio. Resolving it here
    # (before the specs) means the tab_vars recursion, the multi-outcome recursion and the jamovi
    # bridge all inherit it, and every family switch downstream dispatches on ONE key. "rr" is
    # deliberately absent from REG_USER_FAMILIES: a user reaches it through family = "poisson".
    if (reg_fam_count(f) && reg_is_binary_outcome(data[[d]])) {
      # Phase 19e: this route still works exactly as documented, but it is no longer the ONLY one --
      # so the message names the front door. Asking for a risk ratio by naming the wrong distribution
      # was the clearest case in the study of a measure hidden inside another argument.
      cli::cli_inform(c("i" = paste0(
        "{.val {d}} is binary: fitting a modified Poisson regression (robust standard errors) -> ",
        "{.strong risk ratios}, not incidence-rate ratios."),
        "i" = paste0("The same table is {.code family = \"binomial\", measure = \"ratio\"}, ",
                     "which names the measure rather than the distribution.")))
      # Phase 19e: the promotion sets the MEASURE, not the family. "rr" was only ever a link chosen
      # to reach a risk ratio, so it is the estimand row's `fit` now, and the outcome stays what it
      # is -- which is what makes the two spellings one code path.
      rr_promoted[[i]] <<- TRUE
      f <- "binomial"
    }
    f
  }, character(1))

  # --- I: THE ESTIMAND (Phase 19e, KEY 8b) ------------------------------------------------------
  # `effect` (which contrast) x `measure` (which measure) resolve PER DEPENDENT, exactly where
  # `family` does, into ONE row of the declared library. It replaces `family` x `effect` x `at` x
  # `exponentiate` -- 36 combinations for 9 estimands, with three degrade blocks, two aborts and ~19
  # cells where an argument was silently ignored. Every one is now either a row (legal), a row with
  # status "impossible" (cannot be), or no row at all (not offered) -- and the message enumerates the
  # alternatives from the table itself.
  ests <- lapply(seq_len(n), function(i) {
    d   <- outcome[[i]]
    ekv <- reg_effect_key(reg_per_outcome(effect, d, i, "coefficient"))
    # a retired `effect` value could carry a measure inside it; an explicit `measure` still wins
    mv  <- reg_per_outcome(measure, d, i, "auto")
    if (identical(mv, "auto") && nzchar(ekv$measure))  mv <- ekv$measure
    # `family = "poisson"` on a binary outcome IS `measure = "ratio"` (see the promotion above)
    if (identical(mv, "auto") && isTRUE(rr_promoted[[i]])) mv <- "ratio"
    res <- reg_estimand(families[[i]], ekv$effect, mv)
    if (!identical(res$status, "ok")) reg_estimand_abort(res, outcome = d)
    res
  })

  # --- J: estimand x survey feasibility ---------------------------------------------------------
  # Phase 12g: survey-weighted 3+ level outcomes are supported -- ordinal via survey::svyolr, nominal
  # via svyVGAM::svy_vglm. The marginaleffects paths have no method for svyolr / svy_vglm -> error.
  # Asked of the resolved estimand (`builder`), not of an argument.
  if (weighted && any(reg_fam_percategory(families)) &&
      any(vapply(ests, function(e) !identical(e$builder, "coef"), logical(1)))) {
    cli::cli_abort(c(
      "Marginal-effects output is not available for survey-weighted {.val multinomial}/{.val ordinal} models.",
      "i" = "Use the default {.code effect = \"coefficient\"}, or drop the weights."), call = NULL)
  }

  # --- M: `trials` -> grouped binomial (D2) -----------------------------------------------------
  # A summed-score outcome fit as cbind(score, trials - score). NULL = off (binary logit).
  # TRUE = observed max per outcome. Numeric / named vector = the item count. Applied per BINOMIAL
  # outcome only (Phase 15e).
  # Phase 19k: `TRUE` and `NA` both mean "the observed maximum", and BOTH are outcome-aware -- an
  # outcome that is not a numeric score has no maximum to take, so it stays an ordinary binary logit.
  # `NA` inside a named vector is what lets a caller mix explicit counts with automatic ones, which
  # is the shape the jamovi Model table produces.
  tv <- rep(NA_integer_, n)
  if (isFALSE(trials)) trials <- NULL           # the natural off switch, symmetric with TRUE
  if (!is.null(trials)) tv <- reg_resolve_trials(trials, outcome, families, data, formula_mode)

  tibble::tibble(
    outcome     = outcome,
    family      = families,
    rr_promoted = rr_promoted,
    est         = ests,
    fit_family  = vapply(ests, function(e) e$fit, character(1)),
    # gated on the family HERE, so the column means "this outcome IS a grouped binomial" and every
    # reader is a lookup (tab_reg()'s local `trials_for` closure used to re-apply the gate on
    # every call).
    trials      = ifelse(families == "binomial", tv, NA_integer_),
    # Phase 20c: the per-outcome LEVEL to model (binomial) / to pivot on (multinomial), validated
    # against the family's declared role and against the column's own levels. NA = the family's own
    # default (binomial models the first level). A character column, not a logical: `outcome_level`
    # names what the user knows.
    outcome_level = vapply(seq_len(n), function(i)
      reg_resolve_outcome_level(outcome_level, outcome[[i]], families[[i]],
                                data[[outcome[[i]]]]) %||% NA_character_, character(1)),
    crude_key   = vapply(seq_len(n), function(i)
      reg_crude_key(ests[[i]]$fit,
                    if (is.na(tv[[i]]) || families[[i]] != "binomial") NULL else tv[[i]],
                    formula_mode), character(1))
  )
}

# === S4: what the table SHOWS ====================================================================
# `display`, `color`, `color_signif` and `empirical` -- the four arguments that describe the output
# rather than the model, and the one cluster whose ORDER was wrong.
#
# ⚠ THE ORDER IS THE FIX. `empirical` is written by TWO blocks -- the `adjustment` forcing turns it
# ON, the no-crude-companion degrade turns it OFF -- and three later things read it: the notes, the
# specs' effect word, and reg_call's own record. The old body ran forcing -> NOTES -> ... -> degrade
# -> specs, so:
#   H20  a spec and a label could carry " (adjusted %)" on a table whose `empirical` was then turned
#        off, and reg_call$eff_word (computed eagerly, pre-forcing) could disagree with the specs
#        beside it (computed lazily, post-forcing).
#   H21  reg_color_notes() tested `!is.null(color_signif)` while the default "grey_non_signif" was
#        applied 22 lines LATER -- so `tab_reg(color = "adjustment")` was silent and
#        `tab_reg(color = "adjustment", color_signif = "grey_non_signif")`, the identical effective
#        state, emitted the note.
# The order here is display -> colour -> forcings -> DEGRADE -> the color_signif default -> the
# NOTES, so reg_color_notes() describes the state the table is actually built in, and `empirical` is
# final before anything reads it.
#
# ⚠ the notes read the UNFILLED colour spec (`color_arg`), as they always have: `gap` asks which
# own-ref measures the USER named, and the auto fill answers a different question (which ladder an
# auto slot follows).
#' @keywords internal
#' @noRd
reg_resolve_output <- function(display = "value", color = TRUE, color_signif = NULL,
                               empirical = FALSE, deps = NULL, tab_vars = NULL, stats = NULL,
                               na = "drop_by_outcome", na_explicit = FALSE, formula_mode = FALSE) {
  families <- deps$family
  ests     <- deps$est

  # --- L: `display` (Phase 19e) -----------------------------------------------------------------
  # The estimate-cell layout, mirroring tab()'s grammar. "value" (plain) / "ci" (a visible interval,
  # any family) apply everywhere; a {} TEMPLATE naming `pct` / `diff` folds the model-adjusted
  # predicted probability / the average marginal effect into the effect cell -- which is exactly what
  # the retired `estimate_display = "prob" / "ame"` presets did, kept as documented shorthands.
  #
  # THE RULE (KEY 8's other half): a display template may ask for AUXILIARY quantities from the SAME
  # fit; it may never change the fit or the estimand. `measure` is the only estimand argument.
  display <- reg_resolve_display(display)
  # Marginal-effects output already IS a fold ("{diff} ({pct})") -> a second one is ignored.
  if (!identical(display, "value") &&
      any(vapply(ests, function(e) !identical(e$builder, "coef"), logical(1)))) {
    cli::cli_inform(c("i" = "{.arg display} is ignored with marginal-effects output."))
    display <- "value"
  }
  # Phase 15e: the folds are binomial-coefficient only; in a mixed table they apply to the binomial
  # outcomes and each non-binomial column degrades to the CI bracket (guarded per column in
  # reg_apply_display). Only degrade the whole call when NO outcome is a binomial coefficient.
  if (reg_display_folds(display) && !(any(families == "binomial") && !formula_mode)) {
    cli::cli_inform(c(
      "!" = paste0("{.arg display} = {.val {display}} folds a model-adjusted quantity into the ",
                   "effect cell, which needs a binomial coefficient model; showing the confidence ",
                   "interval instead.")))
    display <- "est_ci"
  }

  # --- O: `color` -- normalise, then validate through the storage boundary -----------------------
  # It is logical-primary: TRUE (default) auto-picks the per-column measure; FALSE turns every column
  # (model AND empirical companion) uncoloured; NULL == TRUE.
  # Phase 19e (D25): the GEOMETRY words are gone from this argument. `tab_reg(color = "difference")`
  # on an odds-ratio column used to be ACCEPTED and stored a measure contradicting what the column
  # estimates. Since KEY 2 the column states its own scale, so the ladder comes from the column and
  # what is left to choose is only the two measures whose baseline is ANOTHER COLUMN.
  color_arg  <- reg_normalize_color(color)
  # `color[1]`: since Phase 18z5 the measure may be a length-2 (text, background) vector. Only the
  # text channel carries the auto sentinel.
  # Phase 18z5: VALIDATE through the storage boundary itself rather than repeating its rules here --
  # fmt() casts `color` without validating, so tab_reg would otherwise accept an unknown measure, a
  # whole-cell measure on the background, or the two mutually exclusive `obs` measures together, and
  # only fail (or silently mis-colour) much later. The result is discarded.
  if (!is.na(color_arg[1])) invisible(resolve_color_channels(color_arg))

  # --- P: the forcings --------------------------------------------------------------------------
  # Phase 18z5: `adjustment` scores the model effect against its OBSERVED counterpart, which lives in
  # the `obs` field only when the crude companion was computed -- so asking for the colour asks for
  # `empirical`. Same shape as color = "contrib" forcing chi2 + totrow in the crosstab resolve
  # cascade: the user states an intent, the pipeline computes what it needs. Phase 19c: the forcing
  # is the measure's own declared `requires["empirical"]`, from the same table.
  if (any(vapply(color_arg, measure_forces, logical(1), "empirical")) && !isTRUE(empirical)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"adjustment\"} compares each model effect to its ",
                                   "observed one, so {.code empirical = TRUE} is turned on.")))
    empirical <- TRUE
  }
  # Phase 18z8: `between_groups` also gets the AGGREGATED companion of its per-cell colours -- one
  # pooled interaction test per predictor, in the footer. Automatic here for discoverability (and
  # because the two readings belong together); `stats = c(..., "interaction")` asks for it without
  # the colours. It costs one extra model fit per model, so say so.
  if (any(vapply(color_arg, measure_forces, logical(1), "interaction")) && !is.null(tab_vars) &&
      !(is.character(stats) && "interaction" %in% stats)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"between_groups\"} also adds the aggregated ",
                                   "interaction test to the footer (one extra model fit). Ask for it ",
                                   "without the colours with {.code stats = c(..., \"interaction\")}.")))
  }

  # --- Z: the `empirical` degrade ---------------------------------------------------------------
  # The descriptive crude companion beside the model effect -- the unadjusted bivariate association
  # (which IS the modelised quantity when there is a single predictor). Wired for binomial /
  # gaussian / poisson (explicit columns) and multinomial (tooltip only). Kept ON whenever ANY
  # outcome supports one (the per-fit loop skips the ineligible ones individually); dropped only when
  # NONE is eligible.
  # Phase 19m-ii: it asks the SPEC's own stored answer (`deps$crude_key`, the z10 fact) instead of
  # re-deriving the key from the OUTCOME family -- a third encoding, and one that read a different
  # family from the one the spec pairs its crude block with.
  if (isTRUE(empirical) && all(is.na(deps$crude_key))) {
    # Phase 18z15 (SS12.6 defect 1): name the REAL cause. A compound formula has no predictor
    # structure to be crude about, whatever the family -- the old message blamed the outcome family
    # and so told a binomial user their binomial outcome was unsupported.
    cli::cli_inform(if (formula_mode) c("i" = paste0(
      "{.arg empirical} (crude descriptive companion) needs one predictor per row; a compound formula ",
      "({.code poly()} / interactions / {.code I()}) has none, so it is ignored here."),
      "i" = 'Use {.arg predictors} with {.arg shape} for a curved term, e.g. {.code shape = c(age = "quadratic")}.')
      else c("i" = paste0(
      "{.arg empirical} (crude descriptive companion) is not available for any of these outcome ",
      "families; ignored here.")))
    empirical <- FALSE
  }

  # --- Q: the policy default, then THE NOTES ----------------------------------------------------
  if (is.null(color_signif)) color_signif <- "grey_non_signif"
  # Phase 18z13 (D6): every "the colour you asked for cannot be computed / cannot be tested here"
  # comes from ONE producer, so the rule is uniform -- before it, four cases said so in four blocks
  # and two said nothing at all. It runs LAST so that the `empirical` and `color_signif` it describes
  # are the ones the table is built with (H20 / H21).
  for (note in reg_color_notes(color_arg, color_signif, stats::setNames(ests, deps$outcome), tab_vars,
                               na, na_explicit, families = families, empirical = empirical)) {
    # `{note}` substitutes the already-interpolated string as a VALUE -- passing it as the template
    # would glue it a second time, and one of these notes legitimately prints a literal "{obs}".
    cli::cli_inform(c("i" = "{note}"))
  }

  list(display = display, color_arg = color_arg, color_signif = color_signif, empirical = empirical)
}


# === S5: the fit plan ============================================================================
# Which rows every model is fitted on, whether the cached-digest fast path can serve this call, the
# reference relevel, and the two things measured on ONE frozen frame (the multiplier's unit and the
# quadratic shape terms).
#
# ⚠ THE ORDER, again, is the design:
#   H11 the `reref` gate reads the RESOLVED trials, not the raw argument (19k's fix).
#   H12 a `shape` narrows the fast path -- a shaped model served from the linear fit's digest is a
#       WRONG NUMBER, silently. That clause is why this gate gets its own warning.
#   H14 `reref` BEFORE the relevel: on the digest path the body must NOT relevel (reg_build fits the
#       canonical digest and reparametrizes), so releveling first would fit at the wrong one.
#   H15 the family stage BEFORE the relevel: a multinomial OUTCOME's baseline is releveled too.
#   H16 the relevel BEFORE the specs: reg_positive_level() reads the factor's FIRST level.
#   H18 the `shape` recode (S2) BEFORE the frozen frame: a quantile-cut predictor's SD would
#       otherwise be measured on the raw numeric.
#   H19 the multiplier's SD and the quadratic terms share ONE frozen frame. They were two verbatim
#       reg_complete_frame() calls ten lines apart, under a comment demanding they be the same frame.
#' @keywords internal
#' @noRd
reg_resolve_fit_plan <- function(data, design_obj = NULL, deps = NULL, ref = NULL,
                                 .fit_cache = NULL, all_predictors = character(0),
                                 outcome = character(0), tab_vars = NULL, wt = NULL,
                                 multiplier = "sd", reg_shapes = list(), na = "drop_by_outcome",
                                 formula_mode = FALSE, is_comparison = FALSE, compare = "none",
                                 method = "wald", display = "value", color = NA_character_) {
  families <- deps$family

  # --- S: which rows every model is fitted on (Phase 18z13, D1) ---------------------------------
  # Resolved ONCE into the extra variables each fit must be complete on, and consumed by reg_build
  # through reg_fit(drop_extra=). That mechanism (Phase z9) exists for exactly this: variables
  # joining the complete-case drop without joining the formula, and it is the ONLY sound route --
  # pre-filtering `data` instead breaks a PREBUILT design's keep_mask (reg_resolve_design computes it
  # from `data` itself). So the old `drop_all_models` pre-pass, and its "ignored for a prebuilt
  # survey design" caveat, are both gone.
  #
  # "drop_by_outcome" (the default) makes every model OF ONE OUTCOME share a population, which is
  # what makes the crude companion comparable: reg_build's local `emp_frame_of` is then the model's own
  # frame, so "crude and adjusted are computed on the same people" is structural rather than checked.
  # It also equalises N across nested models, so the likelihood-ratio comparison fires instead of
  # degrading to an AIC difference. A second outcome keeps its own rows (comparing outcomes is not
  # what the call asked for); "drop_all" opts into one population for the whole call.
  # The design variables need no mention: reg_fit's own drop_vars already carries reg_design_vars(),
  # and tab_vars needs none either (the split filters its group before fitting).
  na_shared_vars <- if (formula_mode) character(0) else
    intersect(unique(switch(na,
                            "drop_by_model"   = character(0),
                            "drop_by_outcome" = all_predictors,
                            "drop_all"        = c(all_predictors, outcome))),
              names(data))

  # --- T: the cached-digest fast path (Phase 15b, jamovi live reref) ----------------------------
  # With a `.fit_cache`, a single-equation GLM coefficient table can be recomputed at any
  # factor-predictor reference from ONE canonical fit (reg_build_digest) -- no refit. On that path
  # the body does NOT relevel; reg_build fits the canonical digest + reparametrizes to `ref`.
  # Everything the reparametrization cannot handle keeps the refit path.
  #
  # ⚠ THIS IS THE ONE CLAUSE WHERE A WRONG `TRUE` IS A WRONG NUMBER, NOT AN ERROR. It reads thirteen
  # resolved values spanning eight of the boundary's blocks, which is why the stage order above is
  # the design and not a convenience. Each clause, and what it protects:
  #   builder / mnl_vsrest -- the digest reparametrizes COEFFICIENTS; a marginal effect or a
  #     multinomial-vs-rest column is a different estimator entirely.
  #   display / method     -- a fold needs the fitted object; a profile interval is not a Wald dual.
  #   families             -- Phase 15e: an all-glm mixed table keeps the fast path (each spec caches
  #     its own family's digest); any multinomial/ordinal outcome degrades the whole table.
  #   trials               -- Phase 19k: the RESOLVED trials, not the raw argument. `trials` may now
  #     carry NA = "take the observed maximum", which resolves to NULL on an outcome that has none --
  #     and the jamovi Model table sends exactly that for every binomial outcome. Read raw, a table
  #     of ordinary binary logits looked grouped-binomial and lost the fast path entirely.
  #   color "adjustment"   -- Phase 18z8-B: its gap test needs the FITTED object (influence
  #     functions), which the digest deliberately discards, so asking for it takes the refit path
  #     rather than getting a silently untested colour. Phase 19k made the jamovi reg `color` a
  #     MEASURE list, so a live-UI call CAN reach here with it. The recipe for a digest-based arm is
  #     in dev/model_vs_observed_gap_test.md SS6.
  #   reg_shapes           -- ⚠ Phase 18z15: a `shape` is a DIFFERENT MODEL, not a reparametrization
  #     of the canonical one (unlike `ref` / `multiplier`, which are exact transforms of it).
  #     Serving a shaped model from the linear fit's digest returns a plausible wrong number.
  # Phase 18z9: `multiplier` deliberately LEFT this list. The digest is fitted natively
  # (reg_build_digest passes multiplier = NULL), so it is multiplier-independent exactly as it is
  # reference-independent, and reg_reref_fit_res() applies the scaling itself -- a scaling change is
  # a cache HIT. Keeping the clause would kill the fast path for every table with a numeric
  # predictor once "sd" became the default, which is the regression Phase 15b exists to prevent.
  reref <- !is.null(.fit_cache) &&
    all(vapply(deps$est, function(e) identical(e$builder, "coef"), logical(1))) &&
    !any(vapply(deps$est, function(e) identical(e$builder, "vsrest"), logical(1))) &&
    display %in% c("value", "est_ci") && method == "wald" &&
    all(reg_fam_glm(families)) &&
    !formula_mode && is.null(tab_vars) && all(is.na(deps$trials)) &&
    compare == "none" && !is_comparison && !("adjustment" %in% color) &&
    length(reg_shapes) == 0L

  # --- U: the level relevels ---------------------------------------------------------------------
  # TWO arguments, ONE mechanism (a factor relevel), because they ask OPPOSITE questions:
  #   `ref`           names the level a predictor's other levels are compared AGAINST
  #   `outcome_level` names the level of the OUTCOME that is modelled (binomial) or pivoted on
  #                   (multinomial) -- REG_FAMILIES declares which, per family
  # Phase 20c moved the multinomial half OUT of `ref` (`reference` until this phase), where it was
  # outcome rather than a predictor: one argument carrying two meanings is the disease 19b cured
  # for `type`/`ci_type`, and it is why an outcome name in `ref` now aborts pointing here
  # (reg_apply_references()) instead of warning "matches no predictor".
  # A multinomial's baseline IS the outcome factor's first level, so singling one out is a relevel.
  # An ordinal outcome must keep its order and is refused at S3; a binomial's chosen level is applied
  # later, by reg_prep_binary(), because it also decides the column header.
  relevel <- ref
  mnl <- deps$outcome[families == "multinomial" & !is.na(deps$outcome_level)]
  if (length(mnl))
    relevel <- c(relevel, stats::setNames(deps$outcome_level[match(mnl, deps$outcome)], mnl))
  if (!is.null(relevel) && !reref) {
    # Phase 18z13 (D7): and the SPLIT variable. `color = "between_groups"` compares every effect to
    # the FIRST split level's, so which level that is is a reference choice like any other -- but
    # `ref = c(race = "Black")` was silently dropped for it (tab_vars is not a predictor), and
    # the only way to move the baseline was to relevel the data upstream. One name in this union.
    relevelable <- union(union(all_predictors, tab_vars), mnl)
    if (!is.null(design_obj)) {
      design_obj <- reg_relevel_design(design_obj, relevel, relevelable)  # relevel in the design
      data       <- design_obj$variables
    } else {
      data <- reg_apply_references(data, relevel, relevelable, outcomes = deps$outcome)
    }
  }

  # --- X + Y: the frozen frame, and the two things measured on it -------------------------------
  # Complete on the PREDICTORS + design variables, never on the outcome -- so one predictor keeps
  # one unit across outcomes, compared models and split groups. Phase 19m-ii: computed ONCE. It was
  # two verbatim reg_complete_frame() calls ten lines apart, under a comment (18z15) demanding that
  # the multiplier's SD and the quadratic terms' centre come from the SAME measurement of the SAME
  # column -- true by luck rather than by construction.
  need_mult  <- !formula_mode && any(reg_fam_glm(families))
  frozen     <- if (need_mult || length(reg_shapes) > 0L)
    reg_complete_frame(data, intersect(unique(c(all_predictors, wt)), names(data))) else NULL

  # multiplier (Phase 12g; grammar + resolution Phase 18z9): scale a CONTINUOUS predictor's effect to
  # per-k units (OR^k / beta*k). A SCALAR ("sd" / "2sd" / a number) applies to every numeric
  # predictor; a NAMED vector overrides per variable and the rest keep the scalar default.
  # `mult_default` = the argument's own default reaching us untouched. The multinomial/ordinal guards
  # answer "you asked for something that cannot happen", so they must stay silent for a default
  # nobody asked for -- otherwise every multinomial table would abort.
  mult_default <- identical(multiplier, "sd")
  if (!is.null(multiplier)) {
    if (!(is.numeric(multiplier) || is.character(multiplier)) || length(multiplier) == 0L)
      cli::cli_abort(c(
        "{.arg multiplier} must be a number, {.val sd}, {.val 2sd}, or a named vector of those.",
        "i" = 'e.g. {.code multiplier = "sd"}, {.code c(age = 10)}, {.code c(age = "2sd")}.'),
        call = NULL)
    # Phase 15e: multiplier scales glm-family coefficients; abort only when EVERY outcome is
    # multinomial/ordinal (nothing to scale). In a mixed table it applies to the glm outcomes.
    if (!mult_default && all(reg_fam_percategory(families)))
      cli::cli_abort("{.arg multiplier} is not supported for {.val multinomial}/{.val ordinal} models.",
                     call = NULL)
    if (!mult_default && any(reg_fam_percategory(families)))
      cli::cli_inform(c("i" = paste0(
        "{.arg multiplier} scales the glm-family outcomes only; the multinomial/ordinal ",
        "outcome{?s} are shown unscaled.")))
    if (!is.null(names(multiplier))) {
      bad <- setdiff(names(multiplier), reg_numeric_preds(data, all_predictors))
      if (length(bad) > 0L)
        cli::cli_abort(c("{.arg multiplier} names must be numeric predictors.",
                         "x" = "Not numeric predictor{?s}: {.val {bad}}."), call = NULL)
    }
  }
  # Phase 18z9: "sd" is the DEFAULT scalar. Per 1 unit a numeric predictor sits inside the first
  # colour break and reads as "no effect" beside the factor contrasts next to it; per 1 SD it lands
  # on the same visual scale. `multiplier = 1` restores the per-unit reading. Never applied in
  # compound-formula mode, where a bare RHS name may carry an interaction or a basis expansion and
  # reg_fit()'s `td$term == v` match would scale the main effect while leaving `age:raceWhite` alone.
  mult_res <- if (!need_mult) list(k = NULL, label = NULL) else
    reg_resolve_multiplier(multiplier, "sd", frozen,
                           reg_numeric_preds(data, all_predictors), wt = wt)

  # Phase 18z15: the quadratic terms, on the SAME frozen frame -- so the centre and the unit of a
  # curved predictor's two rows come from one measurement of one column, and a split group / compared
  # model cannot re-centre it. Empty unless a shape asked for one.
  shape_terms <- if (length(reg_shapes) > 0L) reg_shape_terms(frozen, reg_shapes, w = wt)
                 else stats::setNames(character(0), character(0))

  list(data = data, design_obj = design_obj, na_shared_vars = na_shared_vars, reref = reref,
       multiplier = mult_res$k, multiplier_label = mult_res$label, shape_terms = shape_terms)
}


# === S6: the specs ===============================================================================
# The per-model labels, the positive levels, and the ONE new_reg_spec() call site.
#
# The two branches produce only what genuinely differs -- which outcome each spec is about, its
# predictors, its label and the union of predictors -- and the record itself is built once, per
# outcome, from the per-outcome table.
#
# ⚠ A COMPARISON is single-outcome (guarded in S2), so `outcome[[1]]` is the only outcome and
# every table-scalar the old branch carried (`est`, `do_exp`, `eff_word`, `color`) was exactly its
# per-outcome value there. `compound = FALSE, formula = NULL` were hardcoded and are PROVABLY the
# general values: `formula_mode` is set only inside the `is_formula(outcome)` branch, which aborts
# if `predictors` is non-NULL and then assigns `predictors` a CHARACTER vector from
# reg_parse_formula() -- so `is_comparison <- is.list(predictors)` cannot be TRUE alongside it.
#
# `positive_levels` is built HERE and read twice (the label base, and reg_call's own record). It was
# two copies of one expression 200 lines apart, and both must see the POST-relevel data (H16/H17).
#' @keywords internal
#' @noRd
reg_resolve_specs <- function(data, deps, predictors, is_comparison = FALSE, formula_mode = FALSE,
                              raw_formula = NULL, color_arg = NA_character_, empirical = FALSE,
                              cleannames = TRUE) {
  outcome <- deps$outcome
  # a summed-score / compound-formula binomial has no single "positive level" -> label by name
  positive_levels <- vapply(seq_len(nrow(deps)), function(i) {
    if (!reg_fam_binary(deps$family[[i]]) || formula_mode || !is.na(deps$trials[[i]]))
      return(NA_character_)
    reg_cleanup(reg_positive_level(data, outcome[[i]],
                                   reg_outcome_level_of(deps$outcome_level[[i]])), cleannames)
  }, character(1))

  if (is_comparison) {
    models <- predictors
    if (is.null(names(models)) || any(names(models) == ""))
      names(models) <- paste0("model", seq_along(models))
    stopifnot(!formula_mode, is.null(raw_formula))       # the proof above, stated
    labels     <- make.unique(names(models))
    rows       <- rep(1L, length(models))
    preds      <- models
    spec_names <- names(models)                          # map2() over a named list carried these
    union_predictors <- reg_order_union(models)          # Phase 14u (L1): complete-model order if any
  } else {
    labels <- make.unique(vapply(seq_len(nrow(deps)), function(i)
      paste0(if (is.na(positive_levels[[i]])) outcome[[i]] else positive_levels[[i]], ": ",
             reg_eff_word(deps$est[[i]], empirical)), character(1)))
    rows       <- seq_len(nrow(deps))
    preds      <- rep(list(predictors), nrow(deps))
    spec_names <- NULL                                   # map2() over a bare vector carried none
    union_predictors <- predictors
  }
  # Phase 15e: each spec carries its OWN resolved family shape and its own resolved ESTIMAND row
  # (`est`), so reg_build builds a mixed-family table one column-group per outcome.
  specs <- purrr::pmap(list(rows, preds, labels), function(r, p, l)
    new_reg_spec(outcome = outcome[[r]], predictors = p, label = l,
                 fit_family = deps$fit_family[[r]],
                 # the table stores NA for "not a grouped binomial"; the spec field is NULL, which is
                 # what its ten `is.null(sp$trials)` readers speak.
                 trials = if (is.na(deps$trials[[r]])) NULL else deps$trials[[r]],
                 outcome_level = deps$outcome_level[[r]], compound = formula_mode, formula = raw_formula,
                 color = reg_color_for(color_arg, deps$est[[r]]), est = deps$est[[r]],
                 crude_key = deps$crude_key[[r]]))
  names(specs) <- spec_names

  list(specs = specs, union_predictors = union_predictors, positive_levels = positive_levels)
}


# === new_reg_args(): the record reg_resolve_args() returns =======================================
# new_reg_shared()'s idiom (Phase 19g): the FORMALS are the contract, the body is
# as.list(environment()), the globalVariables mirror is derived beneath. It declares MORE than
# reg_build() needs, because new_ctx()'s lesson applies (R/tab.R): tab_reg()'s post-build tail reads
# `positive_levels`, `families`, `eff_word`, `est`, `wt_disp` and `multiplier`, and an undeclared key
# would simply be ABSENT. as.list(environment()) guarantees presence, so a missing binding here is
# structurally impossible.
#' @keywords internal
new_reg_args <- function(data = NULL, specs = list(), shared = list(), reref = FALSE,
                         deps = NULL, outcome = character(0),
                         union_predictors = character(0), positive_levels = character(0),
                         families = character(0), ests = list(), est = NULL, eff_word = "",
                         is_comparison = FALSE, formula_mode = FALSE, empirical = FALSE,
                         display = "value", multiplier = NULL, shape_terms = NULL,
                         na_shared_vars = character(0), design_spec = list(),
                         wt_disp = NA_character_) {
  as.list(environment())
}
utils::globalVariables(names(formals(new_reg_args)))


# === reg_resolve_args(): THE boundary ============================================================
# One entry point, six stages, in the one order they may run in. tab_reg() calls it once.
#' @keywords internal
#' @noRd
reg_resolve_args <- function(data, outcome, predictors, tab_vars = NULL, wt = NULL,
                             family = "auto", effect = "coefficient", measure = "auto",
                             trials = NULL, empirical = FALSE, add_n = TRUE,
                             color = TRUE, color_signif = NULL, stars = TRUE,
                             conf_level = NULL, method = "wald",
                             ref = NULL, outcome_level = NULL,
                             multiplier = "sd", shape = NULL, stats = NULL,
                             na = "drop_by_outcome", na_explicit = FALSE,
                             display = "value", cleannames = TRUE, subtext = "",
                             .fit_cache = NULL) {
  # S1 -- the pure checks.
  # ⚠ H0 (Phase 20c): the `stats` SPLIT runs FIRST, before the validation that reads its parts and
  # before every stage that receives them. `stats` is one argument at the surface and the triple
  # (stats, compare, baseline) everywhere below, so exactly one place knows both readings.
  cmp      <- reg_resolve_stats(stats)
  stats    <- cmp$stats
  compare  <- cmp$compare
  baseline <- cmp$baseline
  reg_validate_args(conf_level = conf_level, stats = stats, color_signif = color_signif,
                    empirical = empirical, add_n = add_n, stars = stars)
  # Phase 20c: `conf_level` is NULL on every producer now (20b's idiom), each boundary resolving it
  # against options(tabxplor.conf_level) -- so the signature carries no call and no default twice.
  conf_level <- conf_level %||% conf_level_default()

  # S2 -- everything that touches `data`.
  prep <- reg_prepare_data(data, outcome, predictors, tab_vars = tab_vars, wt = wt,
                           shape = shape, family = family)

  # ⚠ H24 (Phase 20f-ii): a between-model test compares two fits OF THE SAME OUTCOME, and nothing
  # said so. A models LIST already refuses several outcomes (block E), but the one-model-per-outcome
  # shape did not: `outcome = c("a","b"), stats = "compare_baseline"` reached reg_compare_rows()
  # with two different responses, where `anova.glmlist`'s own `sameresp` filter silently dropped a
  # model and the surviving row was labelled with specs[[1]]$outcome -- a wrong outcome on a wrong
  # test, with no message. It must be refused HERE: `compare` is resolved in S1 and `prep$outcome`
  # is the resolved outcome vector, so this is the first point both are known.
  if (!identical(compare, "none") && length(prep$outcome) > 1L)
    cli::cli_abort(c("A model comparison needs the models to share one {.arg outcome}.",
                     "x" = paste0("{.arg stats} asks for {.val {paste0('compare_', compare)}}, but ",
                                  "{.arg outcome} names {length(prep$outcome)}: ",
                                  "{.val {prep$outcome}}."),
                     "i" = paste0("A comparison tests one model against another on the same ",
                                  "response. Compare within an outcome, or drop the comparison ",
                                  "key to get one column block per outcome.")), call = NULL)

  # S3 -- the per-outcome table.
  deps <- reg_resolve_estimands(prep$data, prep$outcome, family = family, effect = effect,
                                measure = measure, trials = trials,
                                outcome_level = outcome_level,
                                formula_mode = prep$formula_mode, weighted = prep$weighted)

  # S4 -- what the table shows (and the one cluster whose order was wrong).
  out <- reg_resolve_output(display = display, color = color, color_signif = color_signif,
                            empirical = empirical, deps = deps, tab_vars = tab_vars,
                            stats = stats, na = na, na_explicit = na_explicit,
                            formula_mode = prep$formula_mode)

  # S5 -- the fit plan. `color` here is the FILLED spec: `adjustment` is never an auto-fill answer,
  # so the reref gate reads the same thing either way, but the filled one is what the columns carry.
  color_filled <- reg_color_for(out$color_arg, deps$est[[1]])
  plan <- reg_resolve_fit_plan(prep$data, design_obj = prep$design_obj, deps = deps,
                               ref = ref, .fit_cache = .fit_cache,
                               all_predictors = prep$all_predictors, outcome = prep$outcome,
                               tab_vars = tab_vars, wt = prep$wt, multiplier = multiplier,
                               reg_shapes = prep$reg_shapes, na = na,
                               formula_mode = prep$formula_mode, is_comparison = prep$is_comparison,
                               compare = compare, method = method, display = out$display,
                               color = color_filled)

  # S6 -- the specs, on the POST-relevel data (H16).
  sp <- reg_resolve_specs(plan$data, deps, prep$predictors, is_comparison = prep$is_comparison,
                          formula_mode = prep$formula_mode, raw_formula = prep$raw_formula,
                          color_arg = out$color_arg, empirical = out$empirical,
                          cleannames = cleannames)

  # --- AA: what reg_build() is handed ------------------------------------------------------------
  # Phase 18z16-iiiii (defect 3): `degf` (#PSU - #strata) is captured ONCE at the boundary
  # (svy_unwrap_data -> svy$spec$degf) and this literal used to drop it, so tab_reg() was the only
  # consumer of a design that never saw its degrees of freedom. The model columns were on t(degf)
  # regardless -- stats::df.residual() of an svyglm IS the design df -- while the crude Obs_* columns
  # stayed on z: measured at degf = 8, the crude bracket came out 15 % NARROWER than the model
  # bracket beside it, in a table whose whole premise (ruling 1) is that the two are comparable.
  # WARNING: `design_obj` is re-assigned by S5 (its `$variables` are swapped, and
  #   reg_relevel_design() may relevel a factor inside it). Neither touches PSUs or strata, so
  #   `degf` is stable.
  design_spec <- list(design = plan$design_obj, wt = prep$wt, degf = prep$degf)
  # Phase 15e: check the Suggests deps of EVERY family present (nnet for multinomial, MASS for
  # ordinal...).
  for (fm in unique(deps$family))
    reg_check_deps(fm, prep$weighted,
                   needs_marginaleffects = any(vapply(deps$est, function(e) nzchar(e$needs),
                                                      logical(1))) || reg_display_folds(out$display))
  # Phase 17h: every per-call setting reg_build's leaves + assembler read, bundled once (the specs
  # carry the per-outcome family / estimand / colour, so those scalars are not threaded).
  shared <- new_reg_shared(
    union_predictors = sp$union_predictors, design_spec = design_spec, weighted = prep$weighted,
    outcome_level = outcome_level, conf_level = conf_level, method = method,
    color_signif = out$color_signif, cleannames = cleannames, subtext = subtext,
    stats = stats, compare = compare, baseline = baseline, multiplier = plan$multiplier,
    multiplier_label = plan$multiplier_label, shape_terms = plan$shape_terms,
    shape_labels = prep$shape_labels, empirical = out$empirical, display = out$display,
    var_labels = prep$var_labels, na_shared_vars = plan$na_shared_vars, add_n = add_n)

  # Phase 16d: the weight column NAME (or NA) drives the footer "Weighted by <wt>." line. `wt` is a
  # character column name or a formula (reg_design_formula accepts both); a prebuilt design carries
  # its own weights and cannot be named -> NA.
  wt_disp <- if (is.null(prep$wt) || (length(prep$wt) == 1L && is.na(prep$wt))) NA_character_
             else if (rlang::is_formula(prep$wt)) all.vars(prep$wt)[1]
             else as.character(prep$wt)[1]

  new_reg_args(
    data = plan$data, specs = sp$specs, shared = shared, reref = plan$reref, deps = deps,
    outcome = prep$outcome, union_predictors = sp$union_predictors,
    positive_levels = sp$positive_levels,
    families = stats::setNames(deps$family, deps$outcome),
    # NAMED by outcome: reg_call's `measures` / `effects` are per-outcome maps, and vapply over an
    # unnamed list silently drops those names.
    ests = stats::setNames(deps$est, deps$outcome), est = deps$est[[1]],
    eff_word = reg_eff_word(deps$est[[1]], out$empirical),
    is_comparison = prep$is_comparison, formula_mode = prep$formula_mode,
    empirical = out$empirical, display = out$display, multiplier = plan$multiplier,
    shape_terms = plan$shape_terms, na_shared_vars = plan$na_shared_vars,
    design_spec = design_spec, wt_disp = wt_disp)
}


# `trials`'s own validation + resolution: the six aborts and the warn that were block M's bulk.
# Returns one integer per outcome, NA = "not a grouped binomial".
#' @keywords internal
#' @noRd
reg_resolve_trials <- function(trials, outcome, families, data, formula_mode) {
  n <- length(outcome)
  # Phase 18z16-iv (S6): validate HERE. A column name -- the shape a reader naturally reaches for,
  # since a respondent may have answered a different number of items -- used to reach
  # as.numeric("q_count") -> NA -> `cbind(score, NA - score)`, and died deep inside glm() with
  # "contrasts can be applied only to factors with 2 or more levels", naming neither the argument
  # nor the reason. `trials` is one item COUNT per outcome, not a per-row column.
  if (is.character(trials) || is.factor(trials))
    cli::cli_abort(c(
      "{.arg trials} must be an item count, not a column name.",
      "x" = "Got {.val {as.character(trials)}}.",
      "i" = paste("Pass the number of ITEMS behind the summed score: an integer, a vector named by",
                  "outcome, or {.code TRUE} to use each outcome's observed maximum."),
      "i" = "Per-row item counts are not supported; write the model formula with {.code cbind()}."),
      call = NULL)
  # (an all-NA logical vector is the "take the observed maximum for these outcomes" spelling)
  if (!is.numeric(trials) && !isTRUE(trials) && !(is.logical(trials) && all(is.na(trials))))
    cli::cli_abort(c(
      "{.arg trials} must be a number, a vector named by outcome, or {.code TRUE}.",
      "x" = "Got {.cls {class(trials)[[1]]}}."), call = NULL)
  if (!any(families == "binomial"))
    cli::cli_abort("{.arg trials} applies only to {.val binomial} outcomes (grouped / summed-score).",
                   call = NULL)
  if (formula_mode) {
    cli::cli_warn("{.arg trials} is ignored with a compound formula; write {.code cbind()} in it instead.")
    return(rep(NA_integer_, n))
  }
  if (!isTRUE(trials) && !is.null(names(trials))) {
    # a name that matches no outcome is a typo, not a mixing request -- say so, rather than
    # silently auto-resolving the outcome the user meant to pin.
    unknown <- setdiff(names(trials), outcome)
    if (length(unknown))
      cli::cli_abort(c("{.arg trials} names {.val {unknown}}, which is not an outcome.",
                       "i" = "Outcomes: {.val {outcome}}."), call = NULL)
  }
  tv <- if (isTRUE(trials))               rep(NA_real_, n)
        else if (!is.null(names(trials))) unname(as.numeric(trials[outcome]))
        else                              rep_len(as.numeric(trials), n)
  # NA = "take this outcome's observed maximum" -- from `TRUE` (all of them), from an NA entry, or
  # from a named vector that simply does not name this outcome.
  auto <- is.na(tv)
  if (any(auto)) tv[auto] <- vapply(outcome[auto], function(d) reg_trials_observed_max(data[[d]]),
                                    double(1))
  tv <- as.integer(round(tv))
  # An outcome with no observed maximum (a factor, or a 0/1 numeric) keeps NA and is fit as an
  # ordinary binary logit -- there is nothing to abort about. Only an EXPLICIT bad count is an
  # error, and it names itself.
  bad <- outcome[!auto & (is.na(tv) | tv < 1L)]
  if (length(bad))
    cli::cli_abort(c(
      "{.arg trials} must be a positive item count.",
      "x" = "Missing or invalid for {.val {bad}}.",
      "i" = paste("Give an item count, or {.code NA} / {.code TRUE} to take each outcome's",
                  "observed maximum.")), call = NULL)
  tv
}


# The five `tab_vars` refusals, extracted so S2's own body stays a readable sequence of blocks.
#' @keywords internal
#' @noRd
reg_check_tab_vars <- function(data, tab_vars, outcome, all_predictors, formula_mode) {
  if (is.null(tab_vars)) return(invisible(NULL))
  if (!is.character(tab_vars) || length(tab_vars) != 1L) {
    cli::cli_abort("{.arg tab_vars} must be a single column name (character).", call = NULL)
  }
  if (!tab_vars %in% names(data)) {
    cli::cli_abort("{.arg tab_vars} {.val {tab_vars}} is not a column of {.arg data}.", call = NULL)
  }
  if (tab_vars %in% c(outcome, all_predictors)) {
    cli::cli_abort("{.arg tab_vars} {.val {tab_vars}} cannot also be the outcome or a predictor.",
                   call = NULL)
  }
  if (!is.factor(data[[tab_vars]]) && !is.character(data[[tab_vars]])) {
    cli::cli_abort("{.arg tab_vars} {.val {tab_vars}} must be a factor or character column.",
                   call = NULL)
  }
  # Phase 18z13 (D9): a group in which the outcome or a predictor has ONE value cannot be fitted --
  # `contrasts can only be applied to factors with 2 or more levels`, or "the outcome must be
  # binary", both wrapped in purrr's `In index: 1.` noise, naming neither the group nor the variable.
  # Splitting by a coarsening of a predictor (race / black) is a common first attempt, so check it
  # here, where both names are in scope, in the shape tab() uses for its own degenerate inputs.
  if (formula_mode) return(invisible(NULL))
  sl   <- levels(forcats::fct_drop(as.factor(data[[tab_vars]])))
  vars <- intersect(unique(c(outcome, all_predictors)), names(data))
  bad  <- purrr::map(sl, function(g) {
    sub <- data[!is.na(data[[tab_vars]]) & data[[tab_vars]] == g, vars, drop = FALSE]
    if (nrow(sub) == 0L) return(stats::setNames(list(character(0)), g))
    flat <- vars[vapply(sub, function(v) length(unique(stats::na.omit(v))) < 2L, logical(1))]
    stats::setNames(list(flat), g)
  })
  bad <- purrr::flatten(bad)
  bad <- bad[lengths(bad) > 0L | vapply(sl, function(g) sum(!is.na(data[[tab_vars]]) &
                                                           data[[tab_vars]] == g) == 0L,
                                        logical(1))]
  if (length(bad) == 0L) return(invisible(NULL))
  grp <- names(bad)[[1]]
  vb  <- bad[[1]]
  cli::cli_abort(c(
    "{.arg tab_vars} {.val {tab_vars}}: no model can be fitted within {.val {grp}}.",
    "x" = if (length(vb) == 0L) "That group has no rows left."
          else "{cli::qty(vb)}{.val {vb}} {?has/have} a single value there, so {?it/they} \\
                cannot be a model term.",
    "i" = "Drop or merge that group (e.g. with {.fn forcats::fct_lump} or a {.fn filter}), \\
           or split by a variable that varies within every group."
  ), call = NULL)
}
