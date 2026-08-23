# PURPOSE: THE argument boundary of tab_reg() -- every check, and every rewrite of `data`, once and
#   in one ordered place.
# ROLE: the regression twin of tab_resolve_common_args() (R/tab-resolve.R). One entry point,
#   reg_resolve_args(), composed of six private stages; tab_reg() calls it once and receives
#   new_reg_args(), the typed record the builder reads (the FORMALS are the contract, the body is
#   as.list(environment()), and the globalVariables mirror is derived beneath it). One stage sits
#   OUTSIDE that call, S0, because tab_reg() itself needs its answer first.
#
#     S0 reg_select_*()           the tidy-select variable roles, run BY tab_reg() -- the
#                                 multi-outcome recursion above the boundary reads values. It also
#                                 PEELS the `a:b` interactions, which tidyselect cannot see
#     S1 reg_validate_args()      the checks that are PURE
#     S2 reg_prepare_data()       design unwrap / formula / predictors dispatch / the interactions'
#                                 parents / labelled / the level merge / shape / all_predictors /
#                                 tab_vars -- every rewrite of `data`
#     S3 reg_resolve_estimands()  the PER-OUTCOME TABLE: THE CASCADE (family -> link -> measure ->
#                                 effect) resolved into one estimand row, plus trials / outcome
#                                 level / crude key
#     S4 reg_resolve_output()     display / colour / the empirical mode -- and the notes, LAST
#     S5 reg_resolve_fit_plan()   na / the reference relevel / multiplier / shape terms /
#                                 the interactions MATERIALISED, last of all
#     S6 reg_resolve_specs()      the labels, the positive levels, the one new_reg_spec() call site
#
# KEY CONSTRAINTS:
#   - THE ORDER IS THE DESIGN, between the stages and inside each of them. A fact resolved after a
#     block that reads it is a wrong number, not untidiness, so each stage states what its own order
#     buys.
#   - `data` IS INSIDE THE BOUNDARY, not lifted out of it. tab()'s arguments are answerable without
#     the data frame; tab_reg()'s are not -- `family = "auto"` is ANSWERED by the data, `trials =
#     TRUE` is, `multiplier = "sd"` IS a number measured from it, `shape` recodes it, `ref` relevels
#     a factor and SHIFTS a continuous predictor to its anchor, and that relevel needs the
#     multinomial outcomes the family stage resolved. A preparation
#     the caller invoked separately would move the ordering into the caller: a second place to get
#     it wrong. The prepared `data` and `design_obj` are declared FIELDS of the returned record.
#   - S0 RUNS IN tab_reg(), AND SELECTS AGAINST svy_select_frame(), never svy_unwrap_data(): the
#     unwrap informs, adds the reserved columns and computes degf, so S2 must stay its ONE caller.
#     An empty selection becomes NULL, not character(0) -- every guard below tests is.null().
#   - ONE PER-OUTCOME GRAMMAR, four arguments. `family` / `link` / `measure` / `effect` share
#     reg_per_outcome() and are resolved together, in cascade order, by reg_estimand(): the boundary
#     slices them and never interprets them, so "what does auto mean" has ONE home.
#   - A DEFAULT MUST NEVER REFUSE, AND NEVER RESTRICT. Two arguments are now on by default, so the
#     boundary answers for them silently: `empirical` leaves as one of four MODES (reg_emp_mode) and
#     explains its degrade only where a word asked for it; and `stats`' automatic comparison, which
#     arrives as `compare = "auto"`, is degraded to "none" wherever a between-model test has no
#     meaning -- because `compare != "none"` is also what costs a build its parallelism and its
#     dropped fits (reg_specs_independent, R/reg-spec-build.R). An EXPLICIT key keeps its refusal.
#   - ONE PER-PREDICTOR GRAMMAR, three arguments. `multiplier` / `shape` / `ref` share
#     per_variable() (below): an unnamed value -- or one named `default` -- is the fallback, a
#     named one overrides that variable. Each argument keeps its own VOCABULARY; only the parsing is
#     shared, which is why this is a resolver and not a fourth fact table.
#   - There is deliberately no REG_ARG_VALUES table. TAB_ARG_VALUES exists because five crosstab
#     producers had each written one boundary and drifted; tab_reg() is one producer, its
#     vocabularies are already declared once each (REG_USER_FAMILIES / REG_LINKS_VALUES /
#     REG_MEASURES_VALUES / REG_EFFECTS_VALUES / VAR_SHAPES / reg_stat_keys() /
#     REG_MULTIPLIER_KEYWORDS), and an argument
#     that REWRITES what it validates belongs to its own resolver -- one validator, in the one place
#     that can also rewrite the value.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).


# === S0: the variable roles ======================================================================
# The tidy-select boundary, run by tab_reg() itself (before the multi-outcome recursion, which reads
# VALUES). It shares tab()'s selector, tidy_select_chr() (R/tab.R), and adds the two escape hatches
# `outcome` and `predictors` carry -- a model formula, and a named list of models. Both are detected
# on the EXPRESSION where they are written inline, and on the peeked value where they arrive in a
# variable or spliced in by do.call(); tidyselect can see neither.
# tidy_select_chr(), or the quosure's VALUE when selection fails and the value is what this role
# also accepts. A CALL can build either escape hatch -- `as.formula(s)`, `specs[[i]]`, `f[[2]]` --
# and nothing in its expression tells it from a tidyselect helper like starts_with(), so it is
# recognised only once selection has failed. Any other failure re-raises tidyselect's own message.
#' @keywords internal
#' @noRd
reg_select_else_value <- function(quo, data, peeked, accept) {
  out <- tryCatch(list(sel = tidy_select_chr(quo, data, peeked = peeked)),
                  error = function(cnd) list(err = cnd))
  if (is.null(out$err)) return(list(sel = out$sel))
  val <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
  if (accept(val)) return(list(val = val))
  stop(out$err)
}

#' @keywords internal
#' @noRd
reg_select_outcome <- function(quo, data) {
  # is_formula() is TRUE for a quoted `~` call as well as for a formula object, so an inline
  # `y ~ a + b` and an injected one take the same branch.
  if (rlang::is_formula(rlang::quo_get_expr(quo))) return(rlang::eval_tidy(quo))
  pk <- quo_peek_extern(quo, data)
  if (rlang::is_formula(pk)) return(pk)
  r <- reg_select_else_value(quo, data, pk, rlang::is_formula)
  r$val %||% r$sel
}

# A character vector -> one model per outcome; a LIST -> one model per element, its name labelling
# the column. Each element is its own selection, so a comparison reads like the rest of the grammar:
# `list(m1 = c(race, age), m2 = starts_with("inc"))`.
#
# ⚠ `a*b` is an INTERACTION here: it is peeled off before selection and re-appended verbatim
# (R/reg-cross.R), so a model's crosses ride inside its own predictor vector and no parallel
# structure is threaded through the boundary. `a:b` is peeled too, only to be REFUSED with the
# message naming `*` -- it is a different model in R, and not one this package fits.
#' @keywords internal
#' @noRd
reg_select_predictors <- function(quo, data) {
  expr <- rlang::quo_get_expr(quo)
  one  <- function(q) {
    sl <- reg_cross_slots_quo(q, data)
    if (is.null(sl)) return(tidy_select_chr(q, data))
    reg_cross_slots_select(sl, data)
  }
  each <- function(l) {
    if (length(l) == 0L)
      cli::cli_abort(c("A {.arg predictors} list needs at least one model.",
                       "i" = "Name each one: {.code list(m1 = ..., m2 = ...)}."), call = NULL)
    purrr::map(l, function(v) {
      v <- as.character(v)
      if (!any(reg_cross_has_op(v)))
        return(tidy_select_chr(rlang::quo(tidyselect::all_of(!!v)), data))
      reg_cross_slots_select(reg_cross_slots_chr(v), data)
    })
  }
  # an INLINE list(): the elements are expressions, so each is selected in the caller's environment
  if (rlang::is_call(expr, "list")) {
    els <- rlang::call_args(expr)
    if (length(els) == 0L) return(each(list()))
    env <- rlang::quo_get_env(quo)
    return(purrr::map(els, function(e) one(rlang::new_quosure(e, env))))
  }
  if (is.list(expr) && !rlang::is_call(expr) && !rlang::is_symbol(expr)) return(each(expr))
  if (!is.null(reg_cross_slots_quo(quo, data))) return(one(quo))
  pk <- quo_peek_extern(quo, data)
  if (is.list(pk) && !rlang::is_formula(pk)) return(each(pk))
  r <- reg_select_else_value(quo, data, pk, function(v) is.list(v) && !rlang::is_formula(v))
  if (!is.null(r$val)) return(each(r$val))
  if (length(r$sel) == 0L) NULL else r$sel
}

# A role that takes ONE column: `tab_vars` (one grouping variable) and `wt` (one weight). NULL when
# empty -- 17 sites downstream test is.null(), and character(0) is not the same answer.
#' @keywords internal
#' @noRd
reg_select_one <- function(quo, data, arg) {
  v <- tidy_select_chr(quo, data)
  if (length(v) == 0L) return(NULL)
  if (length(v) > 1L)
    cli::cli_abort(c("{.arg {arg}} must select a single column.",
                     "x" = "Got {.val {v}}."), call = NULL)
  v
}


# === The per-predictor grammar ===================================================================
# ONE grammar for `multiplier`, `shape` and `ref` -- the predictor-axis sibling of reg_per_outcome()
# (R/reg-estimand.R), and, like it, the GRAMMAR only: each argument keeps its own vocabulary.
# It is per_variable() (R/var-shape.R), shared with tab(): an UNNAMED element -- or one named
# `default` -- is a FALLBACK whose VALUE names the kind of variable it applies to; a NAMED element
# overrides that one variable. Only `reg_check_continuous_names()` stays here, because its message
# is about PREDICTORS.

# The one message the generic abort cannot give: `multiplier` / `ref` on a factor predictor.
#' @keywords internal
#' @noRd
reg_check_continuous_names <- function(x, data, predictors, arg) {
  nm  <- setdiff(names(x) %||% character(0), c("", "default"))
  fac <- intersect(nm, reg_factor_preds(data, intersect(predictors, names(data))))
  if (length(fac) > 0L)
    cli::cli_abort(c("{.arg {arg}} applies to continuous predictors only.",
                     "x" = "Not {?a numeric predictor/numeric predictors}: {.val {fac}}.",
                     "i" = "{.val {fac}} {?is/are} already {?a factor/factors}."), call = NULL)
  invisible(TRUE)
}


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

  # `color_signif`: ONE vocabulary (R/fmt_class.R). Checked HERE so the abort names {.arg color_signif}
  # at the argument the user wrote, before any column exists -- fmt() validates the same value set.
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
  # `empirical` is logical-primary with four word spellings: "no" (the twin of FALSE, and the word
  # every other tabxplor argument uses for off) and the three that say WHERE the crude effect goes.
  # ⚠ the set is READ from TAB_ARGS, not written again: it already declared "no", `emp_on()` already
  # accepted it, and only this literal refused it -- which made the jamovi picker's own off value an
  # abort.
  emp_words <- tab_arg("empirical")$values
  if (!is.null(empirical) &&
      (length(empirical) != 1L || is.na(empirical) ||
       !(isTRUE(empirical) || isFALSE(empirical) ||
         (is.character(empirical) && empirical %in% emp_words))))
    cli::cli_abort(c(
      "{.arg empirical} must be {.code TRUE}, {.code FALSE}, or {.or {.val {emp_words}}}.",
      "x" = "Got {.val {empirical}}.",
      "i" = paste("{.code TRUE} (the default) draws a crude column, except where that would double a",
                  "table already wide ({.arg tab_vars} groups, a 3+ level outcome) -- there the crude",
                  "value is computed and read from the hover tooltip. {.val tooltip}, {.val cell}",
                  "(inside the model cell) and {.val column} force one outright.")), call = NULL)
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
    cli::cli_abort(c(
      "A model comparison is one {.arg outcome}, but {length(outcome)} were named.",
      "x" = "{.arg predictors} lists {length(predictors)} model{?s} to compare, and {.arg outcome} \
             names {.val {outcome}}.",
      "i" = "Keep ONE outcome to compare the models, or ONE list of predictors to get one model \
             per outcome."), call = NULL)
  }
  if (!is_comparison && !is.character(predictors)) {
    cli::cli_abort("{.arg predictors} must be a character vector or a named list of character vectors.",
                   call = NULL)
  }

  # --- E2: the interactions ----------------------------------------------------------------------
  # Validated here, and BOTH PARENTS named, so `shape`, `multiplier`, `ref` and the complete-case
  # frame see them as ordinary variables. The arm is decided in S5, on the final columns.
  cross <- reg_parse_crosses(predictors, data, outcome, tab_vars)

  # --- F: labelled interop -------------------------------------------------------------------
  # Capture variable labels (BEFORE conversion strips them), then convert labelled columns to
  # value-label factors. `var_labels` rides `shared` into meta$vars for the opt-in display-swap.
  reg_lbl_vars   <- intersect(unique(c(as.character(outcome),
                                       unlist(predictors, use.names = FALSE), cross$parents,
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
  reg_shapes   <- shape_resolve(shape, data,
                                c(unlist(predictors, use.names = FALSE), cross$parents),
                                producer = "tab_reg")
  # a crossed pair of CONTINUOUS variables has no cells, so its moderator is cut here -- decided
  # where a shape is resolved and before it is applied, the one point that can answer "will this
  # still be continuous?". Never silent (R/reg-cross.R).
  reg_shapes <- utils::modifyList(
    reg_shapes,
    purrr::imap(reg_cross_autocut(cross$keys, data, reg_shapes),
                function(v, nm) shape_value(v, nm, "tab_reg")))
  if (length(reg_shapes) > 0L) {
    sh   <- shape_apply(data, reg_shapes, w = wt)
    data <- sh$data
    reg_shapes   <- sh$shapes                    # with each quantile shape's breaks frozen
    if (!is.null(design_obj)) design_obj$variables <- data
  }

  # --- R: the predictor union -------------------------------------------------------------------
  # REAL columns only: an `a:b` key is a declaration, its PARENTS are the variables every later
  # stage measures (the frozen frame, the multiplier's SD, the anchor, the complete cases).
  all_predictors <- if (is_comparison) unique(purrr::flatten_chr(predictors)) else predictors
  all_predictors <- unique(c(setdiff(all_predictors, cross$keys), cross$parents))

  # --- W: tab_vars -----------------------------------------------------------------------------
  # One grouping column that a model is fitted within each level of; reg_build recurses and stacks.
  reg_check_tab_vars(data, tab_vars, outcome, all_predictors, formula_mode)

  list(data = data, design_obj = design_obj, wt = wt, weighted = weighted,
       outcome = outcome, predictors = predictors, all_predictors = all_predictors,
       is_comparison = is_comparison, formula_mode = formula_mode, raw_formula = raw_formula,
       reg_shapes = reg_shapes, var_labels = var_labels, degf = degf,
       cross_keys = cross$keys)
}

# === S3: the per-outcome TABLE =================================================================
# THE fact table of a tab_reg() call: one row per outcome, in `outcome` order, carrying every
# per-outcome fact the rest of the boundary and the whole of reg_build() need.
#
#   outcome     the outcome name
#   family      the USER-facing outcome family, "auto" resolved by reg_detect_family()
#   est         the resolved ESTIMAND row (R/reg-estimand.R) -- the single answer to which model to
#               fit, whether to exponentiate, the header word, the stored `scale`, the crude
#               companion and the marginaleffects contrast
#   fit_family  est$fit -- the internal LINK key ("rr" / "rd" / "mr" included), which is what
#               `link` selects
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
reg_resolve_estimands <- function(data, outcome, family = "auto", link = "auto",
                                  measure = "auto", effect = "auto", trials = NULL,
                                  outcome_level = NULL,
                                  formula_mode = FALSE, weighted = FALSE) {
  n <- length(outcome)

  # --- H: `family`, per outcome -- several outcomes may have DIFFERENT families, through
  # reg_per_outcome(); an ambiguous count aborts for THAT outcome only.
  families <- vapply(seq_len(n), function(i) {
    d <- outcome[[i]]
    f <- reg_per_outcome(family, d, i, "auto")
    if (identical(f, "auto")) f <- reg_detect_family(data, d)
    if (!f %in% REG_USER_FAMILIES)
      cli::cli_abort(c("{.arg family} for {.val {d}} must be one of {.or {.val {REG_USER_FAMILIES}}}.",
                       "x" = "Got {.val {f}}."), call = NULL)
    # `family` answers ONE question -- what kind of number the outcome is -- and never secretly
    # picks a link. The modified Poisson is a BINOMIAL fit under the log link, so it is `link`'s.
    if (reg_fam_count(f) && reg_is_binary_outcome(data[[d]]))
      cli::cli_abort(c(
        "{.val {d}} is binary, so {.code family = \"poisson\"} is not a count model.",
        "i" = paste0("For risk ratios use {.code link = \"ratio\"} (the modified Poisson, ",
                     "Zou 2004: a CONDITIONAL risk ratio), or {.code measure = \"ratio\"} ",
                     "(the MARGINAL one, from the logistic fit).")), call = NULL)
    f
  }, character(1))

  # --- I: THE ESTIMAND -----------------------------------------------------------------------
  # THE CASCADE, per outcome: `link` (which model) -> `measure` (which measure is reported) ->
  # `effect` (where the number comes from), each "auto" following from the left. It lands on ONE
  # composed row, or on a typed refusal whose abort names the cure (R/reg-estimand.R).
  ests <- lapply(seq_len(n), function(i) {
    d   <- outcome[[i]]
    res <- reg_estimand(families[[i]],
                        link    = reg_per_outcome(link,    d, i, "auto"),
                        measure = reg_per_outcome(measure, d, i, "auto"),
                        effect  = reg_per_outcome(effect,  d, i, "auto"))
    if (!identical(res$status, "ok")) reg_estimand_abort(res, outcome = d)
    res
  })

  # --- J: estimand x survey feasibility -- the marginaleffects paths have no method for
  # survey-weighted 3+ level outcomes. Asked of the resolved estimand (`builder`).
  # ⚠ EXCEPT a RANK estimand, which runs on tabxplor's own g-computation over svyolr's (beta, zeta)
  # and takes its variance from that fit's already design-based vcov(). It is the one marginal
  # quantity a weighted ordinal model can report, so it is exempted here rather than by a message.
  no_method <- vapply(ests, function(e) !identical(e$builder, "coef") &&
                        !identical(e$level, "rank"), logical(1))
  if (weighted && any(reg_fam_3plus(families)) && any(no_method)) {
    cli::cli_abort(c(
      "A survey-weighted {.val multinomial}/{.val ordinal} outcome can only be read on its coefficients.",
      "i" = paste0("Its marginal quantities have no method here, so a measure other than the ",
                   "model's own cannot be reported."),
      "i" = paste0("Use {.code effect = \"conditional\"} with the model's own measure, drop the ",
                   "weights, or -- on an ordered outcome -- ask for {.code measure = \"difference\"}.")),
      call = NULL)
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
  # ...then through the storage boundary, for the pair rules a single-value normaliser cannot see:
  # two measures whose baseline is another column cannot share a cell. Result discarded.
  if (!is.na(color_arg[1])) invisible(resolve_color_channels(color_arg))

  # --- P: the forcings --------------------------------------------------------------------------
  # `adjustment` scores the model effect against its OBSERVED counterpart, in `obs` -- so asking for
  # the colour asks for `empirical` (the measure's own declared `requires["empirical"]`).
  if (any(vapply(color_arg, measure_forces, logical(1), "empirical")) && !emp_on(empirical)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"adjustment\"} compares each model effect to its ",
                                   "observed one, so {.code empirical = TRUE} is turned on.")))
    empirical <- TRUE
  }
  # --- Z: the `empirical` degrade ---------------------------------------------------------------
  # Kept ON whenever ANY outcome supports a crude companion; dropped only when NONE is eligible.
  # Reads the SPEC's own stored answer (`deps$crude_key`), never re-derived from the outcome family.
  if (emp_on(empirical) && all(is.na(deps$crude_key))) {
    # ⚠ EXPLAINED ONLY WHERE IT WAS ASKED FOR BY NAME. `empirical` is on by default now, so a note
    # about it on every compound formula and every crude-less family would lecture a reader about an
    # argument they never typed. A word is a request; `TRUE` is "do the sensible thing".
    # name the REAL cause: a compound formula has no predictor structure to be crude about.
    if (is.character(empirical)) cli::cli_inform(if (formula_mode) c("i" = paste0(
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

  # `empirical` leaves the boundary RESOLVED to its mode -- "no"/"tooltip"/"cell"/"column".
  list(display = display, color_arg = color_arg, color_signif = color_signif,
       empirical = reg_emp_mode(empirical, deps$crude_key, ests, tab_vars))
}


# emp_on() / reg_emp_mode() -- `empirical` asked, and WHERE the crude effect goes. ONE value decides
# all three behaviours, and nothing downstream needs a second flag:
#
#   no       nothing computed
#   tooltip  computed and stored (`obs` / `gap_se`), printed nowhere -- it rides the hover tooltip
#            and `color = "adjustment"`, and is read with get_obs()
#   cell     ... plus the `est_obs` layout, "({obs}) {est}", in the model cell
#   column   ... plus a crude column of its own beside each model one
#
# THE AUTO RULE (`TRUE`, the default): a crude COLUMN, except where drawing one would double a table
# already wide -- `tab_vars` groups, and a 3+ level outcome (per-CATEGORY, one crude value per
# column) -- which take `tooltip`. `"tooltip"`/`"cell"`/`"column"` force one outright.
#' @keywords internal
#' @noRd
emp_on <- function(empirical)
  !(is.null(empirical) || isFALSE(empirical) || identical(empirical, "no"))

#' @keywords internal
#' @noRd
reg_emp_mode <- function(empirical, crude_key, ests, tab_vars = NULL) {
  if (!emp_on(empirical)) return("no")
  if (is.character(empirical)) return(empirical)
  per_cat <- any(purrr::map_lgl(seq_along(ests), function(i) {
    k <- crude_key[[i]]
    if (is.null(k) || is.na(k)) return(FALSE)
    sh <- reg_crude_shape(k, ests[[i]])
    !is.null(sh) && shape_per_category(sh)
  }))
  if (per_cat || length(tab_vars)) "tooltip" else "column"
}


# === S5: the fit plan ============================================================================
# Which rows every model is fitted on, and the ORIGIN of every predictor: a factor's reference
# level, a continuous predictor's anchor, its unit, its curvature.
#
# S2 defines the model's VARIABLES (labels -> merge -> shape); S5 fixes their ORIGIN. The order
# inside is the design: `ref` parsed once (U0) -> the frozen frame (X) -> the anchor SHIFT (Y) ->
# the multiplier's SD and the quadratic terms, on the shifted frame -> the factor relevel (U), last
# because it must come before S6 (reg_positive_level() reads the factor's FIRST level).
#' @keywords internal
#' @noRd
reg_resolve_fit_plan <- function(data, design_obj = NULL, deps = NULL, ref = NULL,
                                 all_predictors = character(0),
                                 outcome = character(0), tab_vars = NULL, wt = NULL,
                                 multiplier = "sd", reg_shapes = list(), na = "drop_by_outcome",
                                 cross_keys = character(0),
                                 formula_mode = FALSE, raw_formula = NULL) {
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

  # --- U0: `ref`, parsed ONCE for both kinds of variable: the LEVEL a factor is compared against,
  # and the raw anchor VALUE a continuous predictor is shifted to (turned into a number in block Y).
  # ⚠ under a COMPOUND formula only a predictor the formula uses AS ITSELF is anchorable: shifting a
  # column the user wraps in poly() / log() / I() would change what that call computes.
  num_preds  <- reg_numeric_preds(data, intersect(all_predictors, names(data)))
  anchorable <- if (!formula_mode) num_preds
                else intersect(num_preds, reg_formula_bare_vars(raw_formula, num_preds))
  refs <- reg_resolve_references(ref, data, all_predictors, tab_vars = tab_vars,
                                 outcomes = deps$outcome, num_eligible = anchorable)
  # `outcome_level` on a MULTINOMIAL is a relevel of the outcome factor, so it joins the level map
  # here -- appended, never eligible for a default.
  mnl <- deps$outcome[families == "multinomial" & !is.na(deps$outcome_level)]
  if (length(mnl))
    refs$levels <- c(refs$levels,
                     stats::setNames(deps$outcome_level[match(mnl, deps$outcome)], mnl))

  # --- X: the frozen frame, PREDICTORS + design variables, never the outcome. Computed ONCE: the
  # anchor, the multiplier's SD and the quadratic terms' centre are three readings of one
  # distribution and must come from the SAME measurement.
  # ⚠ built BEFORE the relevel, which is a no-op for it -- a relevel changes neither the row set nor
  # any numeric column -- and that is what lets the anchor be measured here.
  need_mult <- !formula_mode
  frozen    <- if (need_mult || length(reg_shapes) > 0L || length(anchorable) > 0L)
    reg_complete_frame(data, intersect(unique(c(all_predictors, wt)), names(data))) else NULL

  # --- Y: THE ANCHOR. Shifting the column here is what makes the fit's own coefficients anchored:
  # the intercept becomes the reference profile's baseline, and every lower-order term of an
  # interaction is read at the declared point instead of at zero. A slope is invariant, so nothing
  # that is an ESTIMATE moves. An anchor of 0 is dropped -- there is nothing to shift.
  anch    <- if (is.null(frozen))
    list(a = stats::setNames(numeric(0), character(0)),
         keyword = stats::setNames(character(0), character(0)))
    else reg_resolve_anchors(refs$anchors, frozen, anchorable, wt = wt, reg_shapes = reg_shapes)
  anchors <- anch$a
  if (any(anchors != 0)) {
    data   <- reg_anchor_apply(data, anchors)
    frozen <- reg_anchor_apply(frozen, anchors)
    if (!is.null(design_obj)) design_obj$variables <- data
  }

  # multiplier: scale a CONTINUOUS predictor's effect to per-k units (OR^k / beta*k). EVERY family,
  # multinomial and ordinal included: the rescale is arithmetic on the tidied coefficient, and a
  # cumulative or per-category log-odds is as linear in the predictor as a glm's. A per-1-unit
  # effect beside a factor contrast is unreadable. Never applied in compound-formula mode (it would
  # scale only the main effect). "sd" is the default, landing a numeric predictor on the same visual
  # scale as the factor contrasts.
  mult_res <- if (!need_mult) list(k = NULL, label = NULL) else
    reg_resolve_multiplier(multiplier, "sd", frozen, num_preds, wt = wt)

  # the quadratic terms, on the SAME frozen frame. Empty unless a shape asked for one.
  shape_terms <- if (length(reg_shapes) > 0L) reg_shape_terms(frozen, reg_shapes, w = wt)
                 else stats::setNames(character(0), character(0))

  # --- U: the FACTOR relevel, LAST. `between_groups` reads it too: it compares every effect to the
  # FIRST level of the split variable.
  # ⚠ IT IS ALSO WHAT PUTS THE REFERENCE IN THE CACHE KEY, for free: jmv_col_fp() fingerprints a
  # column's levels, so a relevel moves the key and a reference change is an honest refit.
  if (length(refs$levels) > 0L) {
    data <- reg_relevel_data(data, refs$levels)
    if (!is.null(design_obj)) design_obj$variables <- data
  }

  # --- Z: THE INTERACTIONS, last of all (R/reg-cross.R). A crossed pair is combined from variables
  # that are already final: cut by `shape`, shifted to their anchor, and releveled -- which is what
  # makes the combined factor's FIRST level the pair of the parents' own references, with no
  # cross-specific `ref` grammar.
  crosses <- reg_cross_resolve(cross_keys, data, reg_shapes)
  if (length(crosses) > 0L) {
    data <- reg_cross_apply(data, crosses)
    if (!is.null(design_obj)) design_obj$variables <- data
  }

  list(data = data, design_obj = design_obj, na_shared_vars = na_shared_vars,
       ref_levels = refs$levels, anchors = anchors, anchor_keyword = anch$keyword,
       multiplier = mult_res$k, multiplier_label = mult_res$label, shape_terms = shape_terms,
       crosses = crosses)
}


# `ref`'s own resolver: ONE parse, two products. The level map (factor predictors + tab_vars, the
# multinomial pivot appended by the caller) and the raw anchor VALUES, which block Y turns into
# numbers on the frozen frame -- the anchor cannot be measured here, before that frame exists.
#' @keywords internal
#' @noRd
reg_resolve_references <- function(ref, data, all_predictors, tab_vars = NULL,
                                   outcomes = character(0), num_eligible = character(0)) {
  preds <- intersect(all_predictors, names(data))
  fac   <- reg_factor_preds(data, preds)
  num   <- intersect(num_eligible, preds)
  elig  <- c(fac, num)
  kinds <- stats::setNames(c(rep("factor", length(fac)), rep("numeric", length(num))), elig)

  named <- setdiff(names(ref) %||% character(0), c("", "default"))
  # An OUTCOME named here is the other question: `ref` names the level compared AGAINST,
  # `outcome_level` the level MODELLED.
  wrong <- intersect(named, outcomes)
  if (length(wrong) > 0L)
    cli::cli_abort(c(
      "{.val {wrong[[1]]}} is an outcome, not a predictor, so {.arg ref} cannot set its level.",
      "i" = paste0("{.arg ref} names the level other levels are compared AGAINST; ",
                   "{.arg outcome_level} names the level that is MODELLED."),
      "i" = 'Did you mean {.code outcome_level = c({wrong[[1]]} = "{ref[[wrong[[1]]]]}")}?'),
      call = NULL)

  vals <- per_variable(
    ref, elig, "ref", kinds = kinds, fallback_kind = reg_ref_fallback_kind,
    also = intersect(as.character(tab_vars), names(data)),
    vocab = paste0("A default says which kind of predictor it is for: a number or ",
                   "{.or {.val {REG_ANCHOR_KEYWORDS}}} for the continuous ones, ",
                   "{.or {.val {REG_LEVEL_KEYWORDS}}} for the factors."),
    example = 'ref = c(race = "Black")', what = "predictor")
  lv <- intersect(names(vals), c(fac, as.character(tab_vars)))
  list(levels  = vapply(stats::setNames(lv, lv),
                        function(v) reg_ref_level(vals[[v]], data[[v]], v), character(1)),
       anchors = vals[intersect(names(vals), num)])
}

# The anchors as NUMBERS, measured on the frozen frame with the call's own weights. EVERY continuous
# predictor gets one -- "mean" is the package default, because zero is outside the data for an age
# or an income. The zeros are KEPT: `names(anchors)` is what says which columns are anchored, which
# the reference grid and the two descriptive readers each need; only reg_anchor_apply() skips them.
#' @keywords internal
#' @noRd
reg_resolve_anchors <- function(anchors, data, num_preds, wt = NULL, reg_shapes = list()) {
  none <- list(a = stats::setNames(numeric(0), character(0)),
               keyword = stats::setNames(character(0), character(0)))
  if (length(num_preds) == 0L) return(none)
  w <- if (!is.null(wt) && is.character(wt) && length(wt) == 1L && wt %in% names(data))
         data[[wt]] else NULL
  vals <- purrr::map(stats::setNames(num_preds, num_preds), function(v) {
    val  <- anchors[[v]] %||% "mean"
    kind <- reg_shapes[[v]]$kind %||% ""
    # ⚠ `shape` recodes FIRST, so a bare number here would be subtracted from log(x) / sqrt(x). The
    # keywords are measured on the transformed column and are always right; only a number is a trap.
    if (kind %in% c("log", "sqrt") && !is.na(suppressWarnings(as.numeric(val))))
      cli::cli_abort(c(
        '{.arg ref} for {.val {v}} is a value of {.code {kind}({v})}, not of {.val {v}}.',
        "i" = 'The shape is applied first, so write {.code ref = c({v} = {kind}({val}))}.',
        "i" = 'Or use {.or {.val {REG_ANCHOR_KEYWORDS}}}, which are read on the transformed column.'),
        call = NULL)
    # the KEYWORD that named it rides along, "" for a bare number: the row label says where the
    # anchor sits ("at mean/2.98"), and only the user's own word can name it.
    kw <- if (is.character(val) && is.na(suppressWarnings(as.numeric(val))))
            trimws(tolower(val[[1]])) else ""
    list(a = reg_anchor_value(val, data[[v]], w, var = v), keyword = kw)
  })
  a  <- vapply(vals, `[[`, numeric(1), "a")
  kw <- vapply(vals, `[[`, character(1), "keyword")
  ok <- is.finite(a)
  list(a = a[ok], keyword = kw[ok])
}


# Replay the PREPARATION on a fresh frame: the column recodes a `shape` applied, then the anchors.
# ⚠ reg_check_plots() refits from the USER's raw data, so without this the diagnostic would be a
# different model from the one the table shows -- silently, since the row count is unchanged. The
# quantile breaks are the frozen ones, never re-measured.
#' @keywords internal
#' @noRd
reg_prepare_replay <- function(data, prep) {
  if (is.null(prep)) return(data)
  for (v in intersect(names(prep$shapes %||% list()), names(data))) {
    sp <- prep$shapes[[v]]
    x  <- as.numeric(data[[v]])
    data[[v]] <- switch(sp$kind,
                        log       = log(x),
                        sqrt      = sqrt(x),
                        quantiles = shape_cut_quantiles(x, sp$k, var = v, breaks = sp$breaks),
                        sd_bands  = shape_cut_bands(x, var = v, breaks = sp$breaks,
                                                     labels = sp$labels),
                        data[[v]])
  }
  data <- reg_anchor_apply(data, prep$anchors %||% stats::setNames(numeric(0), character(0)))
  reg_cross_apply(data, prep$crosses %||% list())
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
                              cleannames = TRUE, crosses = list()) {
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
    union_predictors <- reg_order_union(purrr::map(models, reg_cross_row_vars, crosses))
  } else {
    labels <- make.unique(vapply(seq_len(nrow(deps)), function(i)
      paste0(if (is.na(positive_levels[[i]])) outcome[[i]] else positive_levels[[i]], ": ",
             reg_word(deps$est[[i]])), character(1)))
    rows       <- seq_len(nrow(deps))
    preds      <- rep(list(predictors), nrow(deps))
    spec_names <- NULL                                   # map2() over a bare vector carried none
    union_predictors <- reg_cross_row_vars(predictors, crosses)
  }
  # each spec carries its OWN resolved family shape and ESTIMAND row (`est`).
  specs <- purrr::pmap(list(rows, preds, labels), function(r, p, l)
    new_reg_spec(outcome = outcome[[r]], predictors = reg_cross_predictors(p, crosses), label = l,
                 cross = reg_cross_keys(p, crosses), row_vars = reg_cross_row_vars(p, crosses),
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
new_reg_args <- function(data = NULL, specs = list(), shared = list(),
                         deps = NULL, outcome = character(0),
                         union_predictors = character(0), positive_levels = character(0),
                         families = character(0), ests = list(), est = NULL, eff_word = "",
                         is_comparison = FALSE, formula_mode = FALSE, empirical = FALSE,
                         display = NULL, multiplier = NULL, shape_terms = NULL,
                         ref_levels = NULL, anchors = NULL, prep = NULL, crosses = list(),
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
                             family = "auto", link = "auto", measure = "auto", effect = "auto",
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

  # ⚠ THE DEFAULT COMPARISON DEGRADES, IT NEVER REFUSES. `"auto"` is what an unnamed `stats` asks
  # for, so it must not abort a table, and it must not turn on the two things `compare != "none"`
  # switches: reg_specs_independent() refuses parallelism on it, and reg_spec_build() then KEEPS the
  # fit object (which is exactly what Phase 22j stopped doing). Several `predictors` sets is the one
  # shape where a between-model test means anything -- and reg_prepare_data() has already refused
  # such a list with several outcomes, so after this line "auto" implies one outcome too.
  if (identical(compare, "auto") &&
      !(isTRUE(prep$is_comparison) && length(prep$predictors) >= 2L)) compare <- "none"

  # ⚠ a between-model test compares two fits OF THE SAME OUTCOME -- refused HERE, the first point
  # both `compare` and the resolved outcome vector are known. Only an EXPLICIT key reaches it.
  if (!identical(compare, "none") && length(prep$outcome) > 1L)
    cli::cli_abort(c("A model comparison needs the models to share one {.arg outcome}.",
                     "x" = paste0("{.arg stats} asks for {.val {paste0('compare_', compare)}}, but ",
                                  "{.arg outcome} names {length(prep$outcome)}: ",
                                  "{.val {prep$outcome}}."),
                     "i" = paste0("A comparison tests one model against another on the same ",
                                  "response. Compare within an outcome, or drop the comparison ",
                                  "key to get one column block per outcome.")), call = NULL)

  # S3 -- the per-outcome table.
  deps <- reg_resolve_estimands(prep$data, prep$outcome, family = family, link = link,
                                measure = measure, effect = effect, trials = trials,
                                outcome_level = outcome_level,
                                formula_mode = prep$formula_mode, weighted = prep$weighted)
  # ⚠ the one estimand refusal that must be raised HERE and not by the fitter: the crude block runs
  # before any model, and on a negative outcome a ratio of means makes it warn before it can abort.
  for (i in seq_along(deps$outcome))
    if (identical(deps$est[[i]]$fit %||% "", "mr") && deps$outcome[[i]] %in% names(prep$data))
      reg_check_ratio_outcome(prep$data[[deps$outcome[[i]]]], deps$outcome[[i]])

  # S4 -- what the table shows.
  out <- reg_resolve_output(display = display, color = color, color_signif = color_signif,
                            empirical = empirical, deps = deps, tab_vars = tab_vars,
                            stats = stats, na = na, na_explicit = na_explicit,
                            formula_mode = prep$formula_mode)

  # S5 -- the fit plan. `color` here is the FILLED spec: `adjustment` is never an auto-fill answer,
  # so both readings agree, but the filled one is what the columns carry.
  color_filled <- reg_color_for(out$color_arg, deps$est[[1]])
  plan <- reg_resolve_fit_plan(prep$data, design_obj = prep$design_obj, deps = deps,
                               ref = ref,
                               all_predictors = prep$all_predictors, outcome = prep$outcome,
                               tab_vars = tab_vars, wt = prep$wt, multiplier = multiplier,
                               reg_shapes = prep$reg_shapes, na = na,
                               cross_keys = prep$cross_keys,
                               formula_mode = prep$formula_mode, raw_formula = prep$raw_formula)

  # S6 -- the specs, on the POST-relevel data.
  sp <- reg_resolve_specs(plan$data, deps, prep$predictors, is_comparison = prep$is_comparison,
                          formula_mode = prep$formula_mode, raw_formula = prep$raw_formula,
                          color_arg = out$color_arg, empirical = out$empirical,
                          cleannames = cleannames, crosses = plan$crosses)

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
    multiplier_label = plan$multiplier_label, anchors = plan$anchors,
    anchor_keyword = plan$anchor_keyword, shape_terms = plan$shape_terms,
    crosses = plan$crosses,
    shape_kinds = vapply(prep$reg_shapes, function(z) z$kind, character(1)),
    empirical = out$empirical, display = out$display,
    var_labels = prep$var_labels, na_shared_vars = plan$na_shared_vars, base_n = base_n)

  # the weight column NAME (or NA) drives the footer "Weighted by <wt>." line; a prebuilt design
  # cannot be named -> NA.
  wt_disp <- if (is.null(prep$wt) || (length(prep$wt) == 1L && is.na(prep$wt))) NA_character_
             else if (rlang::is_formula(prep$wt)) all.vars(prep$wt)[1]
             else as.character(prep$wt)[1]

  new_reg_args(
    data = plan$data, specs = sp$specs, shared = shared, deps = deps,
    outcome = prep$outcome, union_predictors = sp$union_predictors,
    positive_levels = sp$positive_levels,
    families = stats::setNames(deps$family, deps$outcome),
    # `est`/`eff_word` are the table's REPRESENTATIVE estimand (first outcome), feeding the
    # reg_call SUMMARY; the per-outcome facts live in `ests` and the specs.
    prep = list(shapes = prep$reg_shapes, anchors = plan$anchors, crosses = plan$crosses),
    ests = stats::setNames(deps$est, deps$outcome), est = deps$est[[1]],
    eff_word = reg_word(deps$est[[1]]),
    is_comparison = prep$is_comparison, formula_mode = prep$formula_mode,
    empirical = out$empirical, display = out$display, multiplier = plan$multiplier,
    shape_terms = plan$shape_terms, ref_levels = plan$ref_levels, anchors = plan$anchors,
    crosses = plan$crosses, na_shared_vars = plan$na_shared_vars,
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
