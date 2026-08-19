# PURPOSE: THE argument boundary of tab_reg() -- every check, and every rewrite of `data`, once and
#   in one ordered place.
# ROLE: the regression twin of tab_resolve_common_args() (R/tab-resolve.R). One entry point,
#   reg_resolve_args(), composed of six private stages; tab_reg() calls it once and receives
#   new_reg_args(), the typed record the builder reads (the FORMALS are the contract, the body is
#   as.list(environment()), and the globalVariables mirror is derived beneath it).
#
#     S1 reg_validate_args()      the checks that are PURE
#     S2 reg_prepare_data()       design unwrap / formula / predictors dispatch / labelled / the
#                                 level merge / shape / all_predictors / tab_vars -- every rewrite
#                                 of `data`
#     S3 reg_resolve_estimands()  the PER-OUTCOME TABLE (family / estimand / trials / outcome level
#                                 / crude key)
#     S4 reg_resolve_output()     display / colour / the empirical mode -- and the notes, LAST
#     S5 reg_resolve_fit_plan()   na / reref / the reference relevel / multiplier / shape terms
#     S6 reg_resolve_specs()      the labels, the positive levels, the one new_reg_spec() call site
#
# KEY CONSTRAINTS:
#   - THE ORDER IS THE DESIGN, between the stages and inside each of them. A fact resolved after a
#     block that reads it is a wrong number, not untidiness, so each stage states what its own order
#     buys.
#   - `data` IS INSIDE THE BOUNDARY, not lifted out of it. tab()'s arguments are answerable without
#     the data frame; tab_reg()'s are not -- `family = "auto"` is ANSWERED by the data, `trials =
#     TRUE` is, `multiplier = "sd"` IS a number measured from it, `shape` recodes it, `ref` relevels
#     it, and that relevel needs the multinomial outcomes the family stage resolved. A preparation
#     the caller invoked separately would move the ordering into the caller: a second place to get
#     it wrong. The prepared `data` and `design_obj` are declared FIELDS of the returned record.
#   - There is deliberately no REG_ARG_VALUES table. TAB_ARG_VALUES exists because five crosstab
#     producers had each written one boundary and drifted; tab_reg() is one producer, its
#     vocabularies are already declared once each (REG_USER_FAMILIES / REG_EFFECTS_VALUES /
#     REG_MEASURES_VALUES / REG_SHAPES / reg_stat_keys() / REG_MULTIPLIER_KEYWORDS), and an argument
#     that REWRITES what it validates belongs to its own resolver -- one validator, in the one place
#     that can also rewrite the value.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).


# === S1: the pure checks =========================================================================
# Only what needs no data, no other resolution and no rewrite. A `ci`-style argument that REWRITES
# what it validates lives with its own resolver instead, not here.
#' @keywords internal
#' @noRd
reg_validate_args <- function(conf_level = NULL, stats = NULL, color_signif = NULL,
                              empirical = NULL, stars = NULL) {
  # `conf_level`: reuse tab_validate_args() (the "did you mean 0.95?" hint), not a second check.
  tab_validate_args("tab_reg", conf_level = conf_level)

  # `stats`: the KEYS are validated -- a named entry carries its key in the NAME (e.g. "M1").
  if (is.character(stats) && !identical(stats, "all") && !identical(stats, "none"))
    reg_validate_stat_keys(reg_stats_keys_of(stats), arg = "stats")

  # `color_signif`: ONE vocabulary (R/fmt_class.R), checked here -- fmt() casts without validating.
  if (!is.null(color_signif)) {
    cs <- as.character(color_signif)[1]
    if (!identical(cs, "color_all_signif") && !cs %in% COLOR_SIGNIF_VALUES)
      cli::cli_abort(c("Unknown {.arg color_signif} value {.val {cs}}.",
                       "i" = "Valid: {.val {COLOR_SIGNIF_VALUES}}."), call = NULL)
  }

  # the baseline model has no check here: it is the VALUE of a `stats` key.

  # the scalar logicals. NULL passes (several of them mean "read the option" upstream).
  for (nm in c("stars")) {
    v <- get(nm)
    if (is.null(v)) next
    if (length(v) != 1L || !is.logical(v) || is.na(v))
      cli::cli_abort(c("{.arg {nm}} must be a single {.code TRUE} or {.code FALSE}.",
                       "x" = "Got {.val {v}}."), call = NULL)
  }
  # `empirical` is logical-primary with two expert spellings that say WHERE the crude effect goes.
  if (!is.null(empirical) &&
      (length(empirical) != 1L || is.na(empirical) ||
       !(isTRUE(empirical) || isFALSE(empirical) ||
         (is.character(empirical) && empirical %in% c("cell", "column")))))
    cli::cli_abort(c(
      "{.arg empirical} must be {.code TRUE}, {.code FALSE}, {.val cell} or {.val column}.",
      "x" = "Got {.val {empirical}}.",
      "i" = paste("{.code TRUE} draws a crude column, except where one model column would need",
                  "several of them (a 3+ level outcome) -- there the crude value rides inside the",
                  "model cell. {.val cell} and {.val column} force one or the other.")), call = NULL)
  invisible(TRUE)
}


# === S2: everything that touches `data` ==========================================================
# The design unwrap, the formula escape hatch, the predictors dispatch, the labelled conversion,
# the `shape` recode, the predictor union and the tab_vars validation, in the order they may run.
#
# ⚠ THE ORDER IS THE DESIGN: design unwrap before the formula (not yet a data frame to parse), the
# formula before everything else (it REWRITES `outcome`/`predictors`), labelled conversion before
# `shape`/family-detection/`trials`, `shape` before the frozen multiplier frame, and tab_vars
# validation before the family/estimand stages.
#' @keywords internal
#' @noRd
reg_prepare_data <- function(data, outcome, predictors, tab_vars = NULL, wt = NULL,
                             shape = NULL, family = "auto", levels_collapse = NULL) {
  # --- C: a PREBUILT survey design as `data` --------------------------------------------------
  # THE shared boundary (R/survey-design.R) extracts its model frame and materialises the design's
  # weights as a column; the design itself still drives every fit.
  # WARNING: `wt` MUST become that column, not NULL -- many downstream sites read `design_spec$wt`.
  svy <- svy_unwrap_data(data, "tab_reg")
  design_obj <- svy$spec$design
  if (!is.null(svy)) {
    # `wt` beside a prebuilt design ABORTS, one rule across the package.
    svy_abort_wt_design(!is.null(wt))
    data <- svy$data
    wt   <- svy$spec$wt
  }
  stopifnot(is.data.frame(data))
  weighted <- svy_weighted(list(design = design_obj), wt)
  degf <- svy$spec$degf   # #PSU - #strata, carried to every interval's critical value

  # --- D: the formula escape hatch -----------------------------------------------------------
  # A SIMPLE formula reduces losslessly to the outcome+predictors character path; a COMPOUND one
  # (interactions / poly() / I() / calls) is fit verbatim with a fit-read skeleton.
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

  # --- F: labelled interop -------------------------------------------------------------------
  # Capture variable labels (BEFORE conversion strips them), then convert labelled columns to
  # value-label factors. `var_labels` rides `shared` into meta$vars for the opt-in display-swap.
  reg_lbl_vars   <- intersect(unique(c(as.character(outcome),
                                       unlist(predictors, use.names = FALSE),
                                       as.character(tab_vars))), names(data))
  var_labels <- capture_var_labels(data, reg_lbl_vars)
  data       <- tab_apply_val_labels(data, reg_lbl_vars)
  if (!is.null(design_obj)) design_obj$variables <- data

  # --- G0: the level MERGE -----------------------------------------------------------------------
  # tab()'s own pre-aggregate recode (R/tab.R), applied before family detection, the relevel, the
  # frozen multiplier SD and the skeleton. The design's own `$variables` must be recoded too.
  if (!is.null(levels_collapse)) {
    data <- tab_collapse_levels(data, levels_collapse)
    if (!is.null(design_obj)) design_obj$variables <- data
  }

  # --- G: `shape` ----------------------------------------------------------------------------
  # At the SAME boundary as G0: a shape either RECODES the column (log/sqrt/quantile groups) or
  # emits ONE extra model term (quadratic) -- a quantile-cut `age` is a factor from this line on.
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

  # --- W: tab_vars -----------------------------------------------------------------------------
  # One grouping column that a model is fitted within each level of; reg_build recurses and stacks.
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
#   outcome     the outcome name
#   family      the USER-facing outcome family, "auto" resolved by reg_detect_family()
#   rr_promoted did `family = "poisson"` on a binary outcome mean `measure = "ratio"`
#   est         the resolved ESTIMAND row (R/reg-estimand.R) -- the single answer to which model to
#               fit, whether to exponentiate, the header word, the stored `scale`, the crude
#               companion and the marginaleffects contrast
#   fit_family  est$fit -- the internal LINK key ("rr" / "rd" / "mr" included)
#   trials      the resolved grouped-binomial item count, NA = "not a grouped binomial"
#   outcome_level  the level of the outcome the user singled out: MODELLED on a binomial, the
#               BASELINE category on a multinomial (REG_FAMILIES declares which). NA = the family's
#               own default.
#   crude_key   which observed counterpart this outcome has, NA = none
#
# ⚠ ORDER: family before the estimand (reg_estimand() takes it), before the trials block (gates
# the count on "is any outcome binomial?"), and before the survey-feasibility refusal.
#' @keywords internal
#' @noRd
reg_resolve_estimands <- function(data, outcome, family = "auto", effect = "coefficient",
                                  measure = "auto", trials = NULL,
                                  outcome_level = NULL,
                                  formula_mode = FALSE, weighted = FALSE) {
  n <- length(outcome)

  # --- H: `family`, per outcome -- several outcomes may have DIFFERENT families, through
  # reg_per_outcome(); an ambiguous count aborts for THAT outcome only.
  rr_promoted <- rep(FALSE, n)
  families    <- vapply(seq_len(n), function(i) {
    d <- outcome[[i]]
    f <- reg_per_outcome(family, d, i, "auto")
    if (identical(f, "auto")) f <- reg_detect_family(data, d)
    if (!f %in% REG_USER_FAMILIES)
      cli::cli_abort(c("{.arg family} for {.val {d}} must be one of {.or {.val {REG_USER_FAMILIES}}}.",
                       "x" = "Got {.val {f}}."), call = NULL)
    # THE resolution site for the modified-Poisson path: poisson/quasipoisson on a BINARY outcome
    # is Zou (2004)'s modified Poisson (a RISK RATIO), reached only through family = "poisson".
    if (reg_fam_count(f) && reg_is_binary_outcome(data[[d]])) {
      cli::cli_inform(c("i" = paste0(
        "{.val {d}} is binary: fitting a modified Poisson regression (robust standard errors) -> ",
        "{.strong risk ratios}, not incidence-rate ratios."),
        "i" = paste0("The same table is {.code family = \"binomial\", measure = \"ratio\"}, ",
                     "which names the measure rather than the distribution.")))
      rr_promoted[[i]] <<- TRUE   # the promotion sets the MEASURE, not the family
      f <- "binomial"
    }
    f
  }, character(1))

  # --- I: THE ESTIMAND -----------------------------------------------------------------------
  # `effect` x `measure` resolve PER DEPENDENT into ONE row of the declared library: legal, status
  # "impossible", or no row at all (not offered) -- the abort enumerates the table's alternatives.
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

  # --- J: estimand x survey feasibility -- the marginaleffects paths have no method for
  # survey-weighted 3+ level outcomes. Asked of the resolved estimand (`builder`).
  if (weighted && any(reg_fam_percategory(families)) &&
      any(vapply(ests, function(e) !identical(e$builder, "coef"), logical(1)))) {
    cli::cli_abort(c(
      "Marginal-effects output is not available for survey-weighted {.val multinomial}/{.val ordinal} models.",
      "i" = "Use the default {.code effect = \"coefficient\"}, or drop the weights."), call = NULL)
  }

  # --- M: `trials` -> grouped binomial -----------------------------------------------------------
  # A summed-score outcome fit as cbind(score, trials - score). NULL = off. TRUE/NA = the observed
  # maximum, outcome-aware. NA inside a named vector mixes explicit counts with automatic ones.
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
    # reader is a plain lookup.
    trials      = ifelse(families == "binomial", tv, NA_integer_),
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
# `display`, `color`, `color_signif` and `empirical` -- the four arguments that describe the output.
#
# ⚠ `empirical` is written by TWO blocks (`adjustment` forcing ON, the degrade OFF); later readers
# must see it FINAL, so the order is display -> colour -> forcings -> degrade -> color_signif
# default -> the NOTES, run LAST, reading the UNFILLED colour spec (`color_arg`).
#' @keywords internal
#' @noRd
reg_resolve_output <- function(display = NULL, color = TRUE, color_signif = NULL,
                               empirical = FALSE, deps = NULL, tab_vars = NULL, stats = NULL,
                               na = "drop_by_outcome", na_explicit = FALSE, formula_mode = FALSE) {
  families <- deps$family
  ests     <- deps$est

  # --- L: `display` ------------------------------------------------------------------------------
  # THE RULE: a template may ask for AUXILIARY quantities from the SAME fit; it may never change the
  # fit or the estimand -- `measure` is the only estimand argument.
  display <- reg_resolve_display(display)

  # --- O: `color` -- normalise, then validate through the storage boundary -----------------------
  # Logical-primary: TRUE auto-picks the per-column measure; FALSE uncoloured. The GEOMETRY words
  # are gone: only the two measures whose baseline is ANOTHER COLUMN remain choosable.
  color_arg  <- reg_normalize_color(color)
  # VALIDATE through the storage boundary itself (fmt() casts without validating); discarded.
  if (!is.na(color_arg[1])) invisible(resolve_color_channels(color_arg))

  # --- P: the forcings --------------------------------------------------------------------------
  # `adjustment` scores the model effect against its OBSERVED counterpart, in `obs` -- so asking for
  # the colour asks for `empirical` (the measure's own declared `requires["empirical"]`).
  if (any(vapply(color_arg, measure_forces, logical(1), "empirical")) && !emp_on(empirical)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"adjustment\"} compares each model effect to its ",
                                   "observed one, so {.code empirical = TRUE} is turned on.")))
    empirical <- TRUE
  }
  # `between_groups` also gets the AGGREGATED interaction test in the footer, costing one extra fit.
  if (any(vapply(color_arg, measure_forces, logical(1), "interaction")) && !is.null(tab_vars) &&
      !(is.character(stats) && "interaction" %in% stats)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"between_groups\"} also adds the aggregated ",
                                   "interaction test to the footer (one extra model fit). Ask for it ",
                                   "without the colours with {.code stats = c(..., \"interaction\")}.")))
  }

  # --- Z: the `empirical` degrade ---------------------------------------------------------------
  # Kept ON whenever ANY outcome supports a crude companion; dropped only when NONE is eligible.
  # Reads the SPEC's own stored answer (`deps$crude_key`), never re-derived from the outcome family.
  if (emp_on(empirical) && all(is.na(deps$crude_key))) {
    # name the REAL cause: a compound formula has no predictor structure to be crude about.
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
  # every "cannot be computed / cannot be tested here" note comes from ONE producer, run LAST.
  for (note in reg_color_notes(color_arg, color_signif, stats::setNames(ests, deps$outcome), tab_vars,
                               na, na_explicit, empirical = empirical,
                               crude_keys = stats::setNames(deps$crude_key, deps$outcome),
                               trials     = stats::setNames(deps$trials,    deps$outcome))) {
    # `{note}` substitutes the interpolated string as a VALUE -- one note prints "{obs}" literally.
    cli::cli_inform(c("i" = "{note}"))
  }

  # `empirical` leaves the boundary RESOLVED to its mode -- "no"/"cell"/"column".
  list(display = display, color_arg = color_arg, color_signif = color_signif,
       empirical = reg_emp_mode(empirical, deps$crude_key, ests))
}


# emp_on() / reg_emp_mode() -- `empirical` asked, and WHERE the crude effect goes. The auto rule
# (`TRUE`): a crude COLUMN, except a 3+ level outcome (per-CATEGORY), which rides in the model cell
# instead. `"cell"`/`"column"` force one or the other.
#' @keywords internal
#' @noRd
emp_on <- function(empirical)
  !(is.null(empirical) || isFALSE(empirical) || identical(empirical, "no"))

#' @keywords internal
#' @noRd
reg_emp_mode <- function(empirical, crude_key, ests) {
  if (!emp_on(empirical)) return("no")
  if (is.character(empirical)) return(empirical)
  per_cat <- any(purrr::map_lgl(seq_along(ests), function(i) {
    k <- crude_key[[i]]
    if (is.null(k) || is.na(k)) return(FALSE)
    sh <- reg_crude_shape(k, ests[[i]])
    !is.null(sh) && shape_per_category(sh)
  }))
  if (per_cat) "cell" else "column"
}


# === S5: the fit plan ============================================================================
# Which rows every model is fitted on, the cached-digest fast path, the reference relevel, and the
# two things measured on ONE frozen frame (the multiplier's unit and the quadratic shape terms).
# ⚠ `reref` is computed BEFORE the relevel (see block T for why); the relevel comes after the
# family stage and before S6 (reg_positive_level() reads the factor's FIRST level).
#' @keywords internal
#' @noRd
reg_resolve_fit_plan <- function(data, design_obj = NULL, deps = NULL, ref = NULL,
                                 .fit_cache = NULL, all_predictors = character(0),
                                 outcome = character(0), tab_vars = NULL, wt = NULL,
                                 multiplier = "sd", reg_shapes = list(), na = "drop_by_outcome",
                                 formula_mode = FALSE, is_comparison = FALSE, compare = "none",
                                 method = "wald", display = NULL, color = NA_character_) {
  families <- deps$family

  # --- S: which rows every model is fitted on -----------------------------------------------------
  # Resolved ONCE via reg_fit(drop_extra=) -- pre-filtering `data` would break a PREBUILT design's
  # keep_mask. "drop_by_outcome" makes every model OF ONE OUTCOME share a population.
  na_shared_vars <- if (formula_mode) character(0) else
    intersect(unique(switch(na,
                            "drop_by_model"   = character(0),
                            "drop_by_outcome" = all_predictors,
                            "drop_all"        = c(all_predictors, outcome))),
              names(data))

  # --- T: the cached-digest fast path (jamovi live reref) ---------------------------------------
  # With a `.fit_cache`, a GLM coefficient table can be recomputed at any factor-predictor reference
  # from ONE canonical fit (reg_build_digest); the body does NOT relevel, reg_build reparametrizes.
  #
  # ⚠ THIS IS THE ONE CLAUSE WHERE A WRONG `TRUE` IS A WRONG NUMBER, NOT AN ERROR. Each condition
  # protects one fact the digest cannot serve: `builder`/`vsrest` (COEFFICIENTS only, not a marginal
  # effect); `display`/`method` (a fold needs the fitted object); `families` (multinomial/ordinal
  # degrades the table); `trials` (the RESOLVED value); `color "adjustment"` (the gap test needs the
  # FITTED object); `reg_shapes` (⚠ a DIFFERENT MODEL, not a reparametrization like `ref`).
  # `multiplier` stays OUT: the digest is fitted natively, so a scaling change is a cache HIT.
  reref <- !is.null(.fit_cache) &&
    all(vapply(deps$est, function(e) identical(e$builder, "coef"), logical(1))) &&
    !any(vapply(deps$est, function(e) identical(e$builder, "vsrest"), logical(1))) &&
    (is.null(display) ||
       identical(display_resolve(display), DISPLAY_PRESETS[["est_ci"]])) && method == "wald" &&
    all(reg_fam_glm(families)) &&
    !formula_mode && is.null(tab_vars) && all(is.na(deps$trials)) &&
    compare == "none" && !is_comparison && !("adjustment" %in% color) &&
    length(reg_shapes) == 0L

  # --- U: the level relevels: `ref` names the level a predictor is compared AGAINST;
  # `outcome_level` the OUTCOME level modelled (binomial) or pivoted on (multinomial).
  relevel <- ref
  mnl <- deps$outcome[families == "multinomial" & !is.na(deps$outcome_level)]
  if (length(mnl))
    relevel <- c(relevel, stats::setNames(deps$outcome_level[match(mnl, deps$outcome)], mnl))
  if (!is.null(relevel) && !reref) {
    # and the SPLIT variable: `between_groups` compares every effect to the FIRST split level's.
    relevelable <- union(union(all_predictors, tab_vars), mnl)
    if (!is.null(design_obj)) {
      design_obj <- reg_relevel_design(design_obj, relevel, relevelable)  # relevel in the design
      data       <- design_obj$variables
    } else {
      data <- reg_apply_references(data, relevel, relevelable, outcomes = deps$outcome)
    }
  }

  # --- X + Y: frozen frame, PREDICTORS + design variables, never the outcome. Computed ONCE: the
  # multiplier's SD and the quadratic terms' centre must come from the SAME measurement.
  need_mult  <- !formula_mode && any(reg_fam_glm(families))
  frozen     <- if (need_mult || length(reg_shapes) > 0L)
    reg_complete_frame(data, intersect(unique(c(all_predictors, wt)), names(data))) else NULL

  # multiplier: scale a CONTINUOUS predictor's effect to per-k units (OR^k / beta*k); a NAMED
  # vector overrides per variable. `mult_default` stays silent for the multinomial/ordinal guards.
  mult_default <- identical(multiplier, "sd")
  if (!is.null(multiplier)) {
    if (!(is.numeric(multiplier) || is.character(multiplier)) || length(multiplier) == 0L)
      cli::cli_abort(c(
        "{.arg multiplier} must be a number, {.val sd}, {.val 2sd}, or a named vector of those.",
        "i" = 'e.g. {.code multiplier = "sd"}, {.code c(age = 10)}, {.code c(age = "2sd")}.'),
        call = NULL)
    # abort only when EVERY outcome is multinomial/ordinal; a mixed table scales the glm outcomes.
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
  # "sd" is the DEFAULT scalar, landing a numeric predictor on the same visual scale as the factor
  # contrasts. Never applied in compound-formula mode (would scale only the main effect).
  mult_res <- if (!need_mult) list(k = NULL, label = NULL) else
    reg_resolve_multiplier(multiplier, "sd", frozen,
                           reg_numeric_preds(data, all_predictors), wt = wt)

  # the quadratic terms, on the SAME frozen frame. Empty unless a shape asked for one.
  shape_terms <- if (length(reg_shapes) > 0L) reg_shape_terms(frozen, reg_shapes, w = wt)
                 else stats::setNames(character(0), character(0))

  list(data = data, design_obj = design_obj, na_shared_vars = na_shared_vars, reref = reref,
       multiplier = mult_res$k, multiplier_label = mult_res$label, shape_terms = shape_terms)
}


# === S6: the specs ===============================================================================
# The per-model labels, the positive levels, and the ONE new_reg_spec() call site.
#
# ⚠ A COMPARISON is single-outcome (guarded in S2): `compound = FALSE, formula = NULL` generalize.
# `positive_levels` is built HERE and read twice, both needing the POST-relevel data.
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
    union_predictors <- reg_order_union(models)          # complete-model order if any
  } else {
    labels <- make.unique(vapply(seq_len(nrow(deps)), function(i)
      paste0(if (is.na(positive_levels[[i]])) outcome[[i]] else positive_levels[[i]], ": ",
             reg_word(deps$est[[i]])), character(1)))
    rows       <- seq_len(nrow(deps))
    preds      <- rep(list(predictors), nrow(deps))
    spec_names <- NULL                                   # map2() over a bare vector carried none
    union_predictors <- predictors
  }
  # each spec carries its OWN resolved family shape and ESTIMAND row (`est`).
  specs <- purrr::pmap(list(rows, preds, labels), function(r, p, l)
    new_reg_spec(outcome = outcome[[r]], predictors = p, label = l,
                 fit_family = deps$fit_family[[r]],
                 # NA in the table means "not a grouped binomial"; the spec field is NULL instead.
                 trials = if (is.na(deps$trials[[r]])) NULL else deps$trials[[r]],
                 outcome_level = deps$outcome_level[[r]], compound = formula_mode, formula = raw_formula,
                 color = reg_color_for(color_arg, deps$est[[r]]), est = deps$est[[r]],
                 crude_key = deps$crude_key[[r]]))
  names(specs) <- spec_names

  list(specs = specs, union_predictors = union_predictors, positive_levels = positive_levels)
}


# === new_reg_args(): the record reg_resolve_args() returns =======================================
# Over-declares beyond what reg_build() needs: as.list(environment()) guarantees every formal is
# PRESENT, so tab_reg()'s post-build tail cannot hit a silently absent binding.
#' @keywords internal
new_reg_args <- function(data = NULL, specs = list(), shared = list(), reref = FALSE,
                         deps = NULL, outcome = character(0),
                         union_predictors = character(0), positive_levels = character(0),
                         families = character(0), ests = list(), est = NULL, eff_word = "",
                         is_comparison = FALSE, formula_mode = FALSE, empirical = FALSE,
                         display = NULL, multiplier = NULL, shape_terms = NULL,
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
                             trials = NULL, empirical = FALSE, n = NULL,
                             color = TRUE, color_signif = NULL, stars = TRUE,
                             conf_level = NULL, method = "wald",
                             ref = NULL, outcome_level = NULL,
                             multiplier = "sd", shape = NULL, stats = NULL,
                             na = "drop_by_outcome", na_explicit = FALSE,
                             display = NULL, cleannames = TRUE, subtext = "",
                             .fit_cache = NULL, levels_collapse = NULL) {
  # S1 -- the pure checks.
  # ⚠ the `stats` SPLIT runs FIRST: `stats` is one argument at the surface, the triple (stats,
  # compare, baseline) everywhere below.
  cmp      <- reg_resolve_stats(stats)
  stats    <- cmp$stats
  compare  <- cmp$compare
  baseline <- cmp$baseline
  reg_validate_args(conf_level = conf_level, stats = stats, color_signif = color_signif,
                    empirical = empirical, stars = stars)
  # `conf_level` is NULL on every producer, each boundary resolving it against
  # options(tabxplor.conf_level) -- no default stated twice.
  conf_level <- conf_level %||% conf_level_default()
  # the base count is a DISPLAY mode, resolved once here and read back at print/export time.
  tab_validate_args("tab_reg", n = n)
  base_n <- if (is.null(n)) tx_option("n") else as.character(n)[[1]]

  # S2 -- everything that touches `data`.
  prep <- reg_prepare_data(data, outcome, predictors, tab_vars = tab_vars, wt = wt,
                           shape = shape, family = family,
                           levels_collapse = levels_collapse)

  # ⚠ a between-model test compares two fits OF THE SAME OUTCOME -- refused HERE, the first point
  # both `compare` and the resolved outcome vector are known.
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

  # S4 -- what the table shows.
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

  # S6 -- the specs, on the POST-relevel data.
  sp <- reg_resolve_specs(plan$data, deps, prep$predictors, is_comparison = prep$is_comparison,
                          formula_mode = prep$formula_mode, raw_formula = prep$raw_formula,
                          color_arg = out$color_arg, empirical = out$empirical,
                          cleannames = cleannames)

  # --- AA: what reg_build() is handed --------------------------------------------------------------
  # `degf` (#PSU - #strata) captured ONCE, so the model columns and the crude Obs_* columns refer to
  # the SAME design df. WARNING: `design_obj` is re-assigned by S5, but neither touches PSUs/strata.
  design_spec <- list(design = plan$design_obj, wt = prep$wt, degf = prep$degf)
  # check the Suggests deps of EVERY family present.
  for (fm in unique(deps$family))
    reg_check_deps(fm, prep$weighted,
                   needs_marginaleffects = any(vapply(
                     deps$est, function(e) identical(reg_marginal_engine(e), "marginaleffects"),
                     logical(1))))
  # every per-call setting reg_build's leaves + assembler read, bundled once.
  shared <- new_reg_shared(
    union_predictors = sp$union_predictors, design_spec = design_spec, weighted = prep$weighted,
    outcome_level = outcome_level, conf_level = conf_level, method = method,
    color_signif = out$color_signif, cleannames = cleannames, subtext = subtext,
    stats = stats, compare = compare, baseline = baseline, multiplier = plan$multiplier,
    multiplier_label = plan$multiplier_label, shape_terms = plan$shape_terms,
    shape_labels = prep$shape_labels, empirical = out$empirical, display = out$display,
    var_labels = prep$var_labels, na_shared_vars = plan$na_shared_vars, base_n = base_n)

  # the weight column NAME (or NA) drives the footer "Weighted by <wt>." line; a prebuilt design
  # cannot be named -> NA.
  wt_disp <- if (is.null(prep$wt) || (length(prep$wt) == 1L && is.na(prep$wt))) NA_character_
             else if (rlang::is_formula(prep$wt)) all.vars(prep$wt)[1]
             else as.character(prep$wt)[1]

  new_reg_args(
    data = plan$data, specs = sp$specs, shared = shared, reref = plan$reref, deps = deps,
    outcome = prep$outcome, union_predictors = sp$union_predictors,
    positive_levels = sp$positive_levels,
    families = stats::setNames(deps$family, deps$outcome),
    # `est`/`eff_word` are the table's REPRESENTATIVE estimand (first outcome), feeding the
    # reg_call SUMMARY; the per-outcome facts live in `ests` and the specs.
    ests = stats::setNames(deps$est, deps$outcome), est = deps$est[[1]],
    eff_word = reg_word(deps$est[[1]]),
    is_comparison = prep$is_comparison, formula_mode = prep$formula_mode,
    empirical = out$empirical, display = out$display, multiplier = plan$multiplier,
    shape_terms = plan$shape_terms, na_shared_vars = plan$na_shared_vars,
    design_spec = design_spec, wt_disp = wt_disp)
}


# `trials`'s own validation + resolution. Returns one integer per outcome, NA = "not grouped".
#' @keywords internal
#' @noRd
reg_resolve_trials <- function(trials, outcome, families, data, formula_mode) {
  n <- length(outcome)
  # `trials` is one item COUNT per outcome, not a per-row column.
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
    # a name that matches no outcome is a typo, not a mixing request -- say so.
    unknown <- setdiff(names(trials), outcome)
    if (length(unknown))
      cli::cli_abort(c("{.arg trials} names {.val {unknown}}, which is not an outcome.",
                       "i" = "Outcomes: {.val {outcome}}."), call = NULL)
  }
  tv <- if (isTRUE(trials))               rep(NA_real_, n)
        else if (!is.null(names(trials))) unname(as.numeric(trials[outcome]))
        else                              rep_len(as.numeric(trials), n)
  # NA = "take this outcome's observed maximum" (from `TRUE`, an NA entry, or an unnamed outcome).
  auto <- is.na(tv)
  if (any(auto)) tv[auto] <- vapply(outcome[auto], function(d) reg_trials_observed_max(data[[d]]),
                                    double(1))
  tv <- as.integer(round(tv))
  # an outcome with no observed maximum keeps NA; only an EXPLICIT bad count is an error.
  bad <- outcome[!auto & (is.na(tv) | tv < 1L)]
  if (length(bad))
    cli::cli_abort(c(
      "{.arg trials} must be a positive item count.",
      "x" = "Missing or invalid for {.val {bad}}.",
      "i" = paste("Give an item count, or {.code NA} / {.code TRUE} to take each outcome's",
                  "observed maximum.")), call = NULL)
  tv
}


# The `tab_vars` refusals, extracted so S2's own body stays a readable sequence of blocks.
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
  # a group with a ONE-value outcome or predictor cannot be fitted (an opaque glm error otherwise).
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
