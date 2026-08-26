# PURPOSE: tab_reg() -- one regression model per column, rendered as a native tabxplor_tab.
# ROLE: the package's second producer. It fits the models, turns each estimand into fmt cells and
#   returns the object tab() returns, so the colour engine, the accessors, the reshape operations
#   and every exporter treat a regression table and a crosstab identically. This file owns the
#   fitting, the column builders, the marginal-effects path, the footer and the staged build; the
#   estimand vocabulary is R/reg-estimand.R's, the argument boundary R/reg-resolve.R's, the crude
#   companion R/reg-empirical.R's, and the per-model product R/reg-spec-build.R's.
# KEY CONSTRAINTS:
#   - ONE ENGINE PER SHAPE, each tidied from its own summary(): stats::lm / stats::glm unweighted,
#     survey::svyglm as soon as there are weights or a design, nnet::multinom for a nominal 3+ level
#     outcome, MASS::polr for an ordered one. survey / MASS / nnet / brant / marginaleffects / car
#     are Suggests, and every entry point guards them.
#   - CI AND p ARE DUALS, so an interval and its stars can never disagree. `ci_method = "wald"`
#     (default) builds the interval as estimate +/- crit * se and recomputes p from those same two
#     numbers; the crit refers to z where the family FIXES the dispersion (unweighted binomial,
#     poisson) and to t(df.residual) where it is ESTIMATED (lm, quasi*, weighted svyglm), which is
#     what makes it match summary()'s own z / t p exactly. `"profile"` pairs confint() with the
#     likelihood-ratio p, its dual.
#   - ONE REFERENCE DISTRIBUTION PER FIT. That z-or-t choice is made ONCE, in reg_fit(), and travels
#     out with the tidy (`disp_known` / `df_residual`); the marginal sweep, the baseline row and a
#     crude column refit from the same fitter all read it back rather than assuming z. Each column
#     then STAMPS the df it used (reg_wald_degf), which is what lets the between_groups gap SE be
#     recovered with the very critical value that built the interval. ⚠ the df is therefore NOT a
#     table fact and reg_finalize() stamps none: a model column and its crude twin are fitted on
#     different numbers of parameters. The DESIGN's own df is a table fact and lives in the model
#     record, where the "Model:" footer line reads it. The 3+ level engines refer to z throughout,
#     deliberately -- they define no residual df (reg_tidy_native_z).
#   - THE CASCADE IS RESOLVED BEFORE THIS FILE RUNS. `link` picked the fit (`sp$fit_family`),
#     `measure` what is reported and `effect` where it comes from, all in R/reg-estimand.R; here a
#     COEFFICIENT route reads the tidy and a PREDICTION route sweeps the model -- on whichever fit
#     `link` chose, which is why reg_marginal() takes the REPORTED comparison's link rather than
#     inferring one from the family.
#   - THE ESTIMAND'S DECLARED SCALE decides a column's whole shape: which fmt field the estimate
#     lands in (multiplicative -> `or`, neutral 1; additive -> `diff`, neutral 0, with `var` = var(Y)
#     where the scale asks for it) and which ladder it is graded on. No builder names a
#     family-specific field, and every column is built displaying the scale-relative `est` token.
#   - EVERY MODEL COLUMN CARRIES ITS ADJUSTED PREDICTION and EVERY GEOMETRY of its own comparison,
#     printed or not (reg_fill_base -> reg_fill_geometries): one pair of levels, read additively
#     (`diff`) and multiplicatively (`ratio`), the crude column deriving the same two from the
#     observed pair. That is what makes `display` a purely post-hoc property: choosing a layout
#     triggers no computation and changes no number, so set_display() on a built table gives exactly
#     what asking for that layout at build time would have given.
#   - `trials` fits a summed score as a GROUPED binomial (cbind(score, trials - score)). A model
#     FORMULA in `outcome` is the escape hatch: a plain `y ~ a + b` reduces to the outcome+predictors
#     path, while interactions / poly() / I() are fit verbatim and rendered from the fitted terms.
#   - ONE FORMULA ASSEMBLY for every fitter (reg_fit_formula), so reg_formulas() reports what really
#     reached glm() / svyglm() / multinom() / polr() rather than a second reconstruction of it.
#   - EVERY PREDICTOR IS FITTED AT ITS DECLARED REFERENCE. A factor is releveled, a continuous
#     predictor SHIFTED to its `ref` anchor (its weighted mean by default) -- both at the argument
#     boundary, so the fit's own coefficients are already anchored and nothing downstream needs a
#     contrast engine. A slope is invariant under the shift, so no estimate moves; what moves is the
#     Constant row and every term the predictor interacts with. Exactly TWO readers must add the
#     offset back, because they describe the variable's own VALUES rather than an estimate:
#     reg_spec_tips_num()'s tooltip and reg_panel_linearity()'s x axis (R/plots.R).
#   - THE CONSTANT ROW IS A BASELINE, NOT AN EFFECT: the fit's intercept under `conditional`, the
#     predicted outcome at the reference profile under `at_reference`, the population average under
#     `marginal` -- one quantity, then converted to the column's own geometry (reg_constant_cell).
#     Only the tested intercept carries a p-value, so only it takes a star.
#   - A 3+ LEVEL OUTCOME becomes several COLUMNS, not several tables: one multinomial fit gives one
#     odds-ratio column per non-reference category, one proportional-odds fit one cumulative-OR
#     column (its cut-point rows are dropped, so the Constant cell is empty). Both reuse the ordinary
#     column shape and share reg_tidy_native_z(), so the duality above holds there too.
#   - THE FIT LEAVES, THE DIGEST STAYS. reg_fit() returns a RECORD (reg_fit_record) whose `tidy` is
#     the only REPORTING-dependent member: the estimate and its SE are stored on the model's NATIVE
#     scale -- per ONE unit, unexponentiated -- and reg_tidy_finalize() writes the interval, the
#     exponentiation, the p and the `multiplier` scaling per (do_exp, conf_level, multiplier).
#     ⚠ EVERY reporting choice is applied THERE and nowhere earlier: that is the whole reason the
#     jamovi cache can key on the MODEL alone (jmvreg_fit_key) and serve each of them as a hit.
#     Everything else a column needs comes from the `tabxplor_fitdigest` (R/reg-digest.R).
#     Only `method = "profile"` cannot be served: its bounds are a likelihood output, so they ride
#     natively on the record and are scaled with the estimate they belong to.
#   - THE MODEL COMPARISON IS A DEFAULT. Several `predictors` sets are tested against each other
#     without being asked (`stats = NULL` resolves `compare = "auto"`), sequential where every model
#     nests in the next and against the first otherwise -- decided in reg_compare_rows(), the first
#     place the fits exist. ⚠ `compare != "none"` is what makes reg_specs_independent() refuse
#     parallelism AND makes the product KEEP its fit, so the boundary degrades "auto" to "none"
#     wherever a comparison has no meaning (reg_resolve_args): an ordinary table must not pay for a
#     row it will not print.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).

# === SECTION: Internal engine ===================================================================

# DESIGN: marginaleffects is needed ONLY where an estimand's engine names it (`at_reference`). Every
# other marginal quantity runs on the dependency-free gcomp engine, which is what lets every model
# column populate them unconditionally.

#' @keywords internal
#' @noRd
reg_abort_marginaleffects <- function(what) {
  tx_need_pkg("marginaleffects", what)
}

reg_check_deps <- function(family, weighted, needs_marginaleffects = FALSE) {
  if (needs_marginaleffects && !requireNamespace("marginaleffects", quietly = TRUE))
    reg_abort_marginaleffects('effect = "at_reference"')
  # nnet / MASS need no guard: they are Imports. ⚠ VGAM is called directly on the weighted
  # multinomial path, so it is guarded explicitly -- an implicit guard is invisible to R CMD check.
  if (isTRUE(weighted) && family == "multinomial")
    tx_need_pkg(c("svyVGAM", "VGAM"), "A survey-weighted multinomial model")
}

reg_parse_formula <- function(formula, data) {
  lhs <- rlang::f_lhs(formula)
  if (is.null(lhs)) {
    cli::cli_abort("A regression {.arg formula} needs a response, e.g. {.code y ~ x1 + x2}.")
  }
  lhs_is_name <- rlang::is_symbol(lhs)
  outcome   <- if (lhs_is_name) rlang::as_string(lhs) else all.vars(lhs)[1]

  tt     <- stats::terms(formula, data = data)
  labels <- attr(tt, "term.labels")
  orders <- attr(tt, "order")
  rhs_vars <- all.vars(rlang::f_rhs(formula))

  simple <- lhs_is_name &&
    outcome %in% names(data) &&
    length(labels) > 0L &&
    all(orders == 1L) &&
    all(labels %in% names(data))

  list(outcome = outcome, predictors = rhs_vars, labels = labels,
       formula = formula, lhs_is_name = lhs_is_name, simple = simple)
}

# DESIGN: the family PREDICATES -- every "which families behave like X" question asked ONCE here.
reg_is_binary_outcome <- function(y) length(unique(stats::na.omit(y))) == 2L
reg_fam_binary   <- function(f)
  f %in% c("binomial", names(REG_FIT_FAMILY)[REG_FIT_FAMILY == "binomial"])
reg_fam_prob     <- function(f) f %in% c("binomial", "multinomial", "ordinal")
# fitted by one of the 3+ level machines (nnet::multinom / MASS::polr), which have no marginal-effects
# method under a survey design. ⚠ NOT "how many columns this estimand needs": that is the estimand's
# own `per_level`, derived in reg_compose_row().
reg_fam_3plus    <- function(f) reg_fam_prob(f) & !f %in% "binomial"
reg_fam_count    <- function(f) f %in% c("poisson", "quasipoisson")
# ⚠ the question is about the OUTCOME family, never the fit key: `rr` / `rd` are binomial FITS under
# another link, so a `family == "binomial"` test would drop `trials` on both. A compound formula owns
# its LHS, so `trials` does not apply to it -- from a spec that flag is `sp$compound`, the one spelling.
reg_is_grouped_binomial <- function(family, trials, compound = FALSE)
  reg_fam_binary(family) && !is.null(trials) && !isTRUE(compound)
# fitted by stats::glm -- not one of the 3+ level machines, which have no glm-shaped coefficient
# table, no anova() and no AIC path.
reg_fam_glm <- function(f) f %in% c("gaussian", "binomial", "poisson", "quasipoisson",
                                    "rr", "rd", "mr")
# over-dispersible, so the nominal variance cannot be trusted: poisson, and a grouped binomial for
# the same reason.
reg_fam_overdispersed <- function(f, grouped = FALSE) f == "poisson" || isTRUE(grouped)
# the dispersion is FIXED BY THE FAMILY (1), so the Wald critical value refers to z, not t.
reg_fam_disp_known <- function(f) f %in% c("binomial", "poisson")
# the dispersion is ESTIMATED from the residuals, so a term test refers to F rather than chi2.
reg_fam_disp_estimated <- function(f) f %in% c("gaussian", "quasipoisson")
# fitted by survey::svyglm -- ONE fact with two consequences: it picks the fitter, and an svyglm has
# no ordinary likelihood, so there is no LR test to run. "rr" / "rd" / "mr" go through it even
# unweighted: all three are deliberately misspecified likelihoods chosen to reach a MEASURE, so their
# honest variance is the Huber-White sandwich that svyglm's design-based variance IS.
reg_fam_svy_fitted <- function(f, weighted = FALSE)
  isTRUE(weighted) || f %in% REG_FIT_ONLY_FAMILIES
# Is the DISPLAYED estimand COLLAPSIBLE -- does a zero model-vs-observed gap mean "no confounding"?
# Everything tabxplor shows is, EXCEPT a CONDITIONAL ODDS RATIO (a probability-scale model's
# coefficient; exponentiating is irrelevant, a raw logit coefficient is the same estimand logged).
# ⚠ ask it of each resolved estimand's FIT, never of the outcome family.
reg_estimand_collapsible <- function(family, effect)
  !(identical(effect, "conditional") && reg_fam_prob(family))

# THE producer of "the colour you asked for cannot be computed here": one entry per reason, never a
# block at a call site, and run BEFORE the build, which would otherwise repeat itself per group.
#' @keywords internal
reg_color_notes <- function(color, color_signif, ests, tab_vars, na, na_explicit,
                            empirical = FALSE, crude_keys = NULL, trials = NULL) {
  notes <- character(0)
  at    <- if (any(vapply(ests, function(e) identical(e$effect, "at_reference"), logical(1))))
             "reference" else "average"
  effect <- ests[[1]]$effect
  # Interpolated HERE, where the locals the messages name are in scope: the caller only emits them.
  add   <- function(...) notes <<- c(notes, cli::format_inline(paste0(...)))
  gap   <- intersect(c("adjustment", "between_groups"), color)
  if (length(gap) == 0L && !emp_on(empirical)) return(notes)

  if ("between_groups" %in% gap && is.null(tab_vars)) {
    add('{.code color = "between_groups"} needs {.arg tab_vars} to say what the groups are; ',
        "nothing is coloured.")
  }
  if (identical(at, "reference")) {
    add('{.code effect = "at_reference"} reads the model at one profile while the observed columns ',
        "stay marginal, so the two are shown side by side but not compared.")
  }
  if ("adjustment" %in% gap) {
    # THE SAME PREDICATE the gate uses at build time (reg_same_estimand()), so the note and the gate
    # cannot disagree. ⚠ do NOT approximate it by "does the marginal row reuse the coefficient row's
    # crude shape": sharing that shape is the NORMAL case wherever the two contrasts are one estimand.
    if (!identical(effect, "conditional")) {
      bare <- unique(vapply(names(ests), function(d) {
        e  <- ests[[d]]
        tr <- if (is.null(trials)) NA else trials[[d]] %||% NA
        sh <- reg_crude_shape(if (is.null(crude_keys)) NA_character_ else crude_keys[[d]], e)
        if (!reg_same_estimand(sh, reg_scale_of(e, tr), e)) e$family else NA_character_
      }, character(1)))
      bare <- stats::na.omit(bare)
      if (length(bare)) {
        add("{.val {bare}}: {.code effect = {.val {effect}}} has no observed counterpart on the ",
            'same scale. {.code effect = "conditional"} compares them.')
      }
    }
    if (!is.null(color_signif) && !identical(color_signif, "ignore") &&
        !any(vapply(ests, function(e) reg_estimand_collapsible(e$fit, e$effect), logical(1)))) {
      add("An odds-ratio adjustment gap is not tested: part of it is non-collapsibility, not ",
          'confounding. {.code measure = "ratio"} gives a gap the test can read.')
    }
    if (na_explicit && identical(na, "drop_by_model")) {
      add('{.code na = "drop_by_model"} fits each model on its own complete cases, so a model the ',
          "observed columns do not cover gets no observed effect: the gap would be deletion, not ",
          "adjustment.")
    }
  }
  notes
}

# `crude_key` -- THE stored fact "which observed counterpart does this model have?": a REG_EMPIRICAL
# key, or NA (a compound formula has no predictor structure to be crude about). Computed ONCE at spec
# construction, where family, trials and the compound flag are all in scope.
reg_crude_key <- function(family, trials = NULL, compound = FALSE) {
  if (isTRUE(compound))                                 return(NA_character_)
  # ⚠ the grouped test comes FIRST and must stay there: `rd` and `rr` are binomial FITS, so a
  # summed-score outcome under either is a grouped binomial -- its crude base is the mean SCORE.
  if (reg_is_grouped_binomial(family, trials, compound)) return("grouped_binomial")
  # everything else is the declared block rule, shared with the estimand library's own composition
  # (reg_emp_block(), R/reg-estimand.R) so a run-time key and a composed shape cannot disagree.
  reg_emp_block(family)
}

# `trials = TRUE` means "the observed maximum"; NA where there is none (a factor outcome is an
# ordinary logit, a 0/1 numeric has no trial count).
#' @keywords internal
#' @noRd
reg_trials_observed_max <- function(x) {
  if (!is.numeric(x) || is.factor(x)) return(NA_real_)
  m <- suppressWarnings(max(x, na.rm = TRUE))
  if (is.finite(m) && m > 1) m else NA_real_
}

# Is a PREDICTOR a factor (contrasts against a reference) or a numeric (one slope per unit)?
# ⚠ a LOGICAL must take the FACTOR arm -- glm names its coefficient `<var>TRUE`; Date / POSIXct stay
# numeric. The answer is STORED in reg_meta$predictor_types: `cleannames` and the multiplier relabel
# both break the `level == var` convention.
#' @keywords internal
reg_is_factor_var <- function(x) is.factor(x) || is.character(x) || is.logical(x)

#' @keywords internal
reg_predictor_types <- function(data, predictors) {
  if (length(predictors) == 0L) return(stats::setNames(character(0), character(0)))
  stats::setNames(
    vapply(predictors, function(p) if (reg_is_factor_var(data[[p]])) "factor" else "numeric",
           character(1)),
    predictors)
}

#' @keywords internal
reg_factor_preds <- function(data, predictors)
  predictors[purrr::map_lgl(predictors, ~ reg_is_factor_var(data[[.x]]))]

#' @keywords internal
reg_numeric_preds <- function(data, predictors)
  predictors[!purrr::map_lgl(predictors, ~ reg_is_factor_var(data[[.x]]))]


# === SECTION: `multiplier` -- the per-unit scaling of a continuous predictor's effect ============
#
# GRAMMAR: the package's shared per-predictor one (per_variable(), R/var-shape.R) -- an
# unnamed value, or one named `default`, applies to every continuous predictor; a named element
# overrides that one.
#
# The SD is measured on the complete cases of the PREDICTORS + design variables -- not of the
# outcome -- and resolved ONCE, before `shared` is built, so the split recursion, the compared
# models, the crude companions and the jamovi cache key all see the SAME numbers; a per-group SD
# would make `color = "between_groups"` compare different quantities. ⚠ never passed downstream as
# a KEYWORD: marginaleffects reads "sd" as a CENTRED contrast on the SD of its own `newdata`.

#' @keywords internal
REG_MULTIPLIER_KEYWORDS <- c("sd", "1sd", "2sd")

# A value the unit vocabulary accepts. Refusing here rather than in reg_multiplier_value() is what
# stops a typo becoming a silent per-1-unit reading.
#' @keywords internal
reg_multiplier_ok <- function(v) {
  if (is.numeric(v)) return(TRUE)
  s <- trimws(tolower(as.character(v)[[1]]))
  s %in% REG_MULTIPLIER_KEYWORDS || !is.na(suppressWarnings(as.numeric(s)))
}

#' @keywords internal
reg_multiplier_value <- function(value, sd, digits = 3L) {
  v <- if (is.character(value)) trimws(tolower(value)) else value
  if (length(v) != 1L || is.na(v)) return(list(k = NA_real_, label = NA_character_))
  if (is.character(v) && v %in% REG_MULTIPLIER_KEYWORDS) {
    if (!is.finite(sd) || sd <= 0) return(list(k = NA_real_, label = NA_character_))
    mult <- if (identical(v, "2sd")) 2 else 1
    # "13.5 (SD)" -- the NUMBER first, because that is the unit the effect is per; the keyword is
    # what it was read from, and the count in it only when it is not 1.
    lab  <- if (mult == 1) "SD" else paste0(mult, "SD")
    return(list(k = mult * sd,
                label = paste0(format(signif(mult * sd, digits)), " (", lab, ")")))
  }
  k <- suppressWarnings(as.numeric(v))
  if (!is.finite(k)) return(list(k = NA_real_, label = NA_character_))
  # DESIGN: the LABEL is descriptive, the factor is arithmetic, and 1 is the value where the two
  # disagree -- scaling by 1 is a no-op, but "per 1" is exactly what a user who typed it asked to
  # read. So every finite k gets a label, and only the SCALING drops the ones that do nothing.
  list(k = k, label = format(k))
}

#' @keywords internal
reg_resolve_multiplier <- function(multiplier, default, data, num_preds, wt = NULL) {
  if (length(num_preds) == 0L) return(list(k = NULL, label = NULL))
  reg_check_continuous_names(multiplier, data, names(data), "multiplier")
  vals <- per_variable(multiplier, num_preds, "multiplier", what = "predictor")
  for (v in names(vals)) {
    if (!reg_multiplier_ok(vals[[v]]))
      cli::cli_abort(c(paste0("{.arg multiplier} for {.val {v}} must be a number or ",
                              "{.or {.val {REG_MULTIPLIER_KEYWORDS}}}."),
                       "x" = "Got {.val {as.character(vals[[v]])[[1]]}}."), call = NULL)
  }
  w   <- if (!is.null(wt) && is.character(wt) && length(wt) == 1L && wt %in% names(data))
           data[[wt]] else NULL
  sds <- vapply(num_preds, function(v) wtd_sd(data[[v]], w), numeric(1))
  res <- purrr::map(stats::setNames(num_preds, num_preds), function(v)
    reg_multiplier_value(vals[[v]] %||% default, sds[[v]]))
  k   <- vapply(res, function(z) z$k,     numeric(1))
  lab <- vapply(res, function(z) z$label, character(1))
  # the two vectors are filtered SEPARATELY, per the rule in reg_multiplier_value(): `k` carries the
  # scaling (a factor of 1 changes no number, so it is dropped), `label` carries what the level row
  # says (a scaling of 1 is still a statement about the unit).
  keep <- is.finite(k) & k != 1
  said <- is.finite(k) & !is.na(lab)
  if (!any(said)) return(list(k = NULL, label = NULL))
  list(k = if (any(keep)) k[keep] else NULL, label = lab[said])
}

# === SECTION: Naming -- the outcome kind, the family, the column headers and labels =============

# REG_OUTCOME_KINDS -- THE outcome-kind table, read here and GENERATED into the jamovi JavaScript, so
# the two sides cannot drift. One row per kind of outcome column, keyed by what BOTH sides compute
# from a column alone (has it levels, and how many):
#   <name> : the kind key.
#   detect : the family auto-detected for it.
#   offers : the families offered beside it, ordered, first = the detected default. A 2-level outcome
#            offers poisson because that is the opt-in modified-Poisson (risk-ratio) route, not a
#            count model.
#   said   : how reg_detect_family() names the kind (a bare string: not translated today).
#' @keywords internal
REG_OUTCOME_KINDS <- list(
  # ⚠ a binary outcome offers ONE family: the modified Poisson is a LINK of the binomial
  # (`link = "ratio"`), not a count model, and naming it twice is what the cascade deletes.
  binary   = list(detect = "binomial",    offers = "binomial",
                  said = "binary outcome detected"),
  # a 3+ level outcome also offers BINOMIAL: one level against all the others merged, which is the
  # ordinary way of asking "what predicts being X rather than anything else". Never the default --
  # collapsing categories is a choice, and `outcome_level` is where it is made.
  ordered  = list(detect = "ordinal",     offers = c("ordinal", "multinomial", "binomial"),
                  said = "ordered outcome detected"),
  nominal  = list(detect = "multinomial", offers = c("multinomial", "ordinal", "binomial"),
                  said = "nominal outcome detected"),
  numeric  = list(detect = "gaussian",    offers = c("gaussian", "binomial", "poisson"),
                  said = "continuous outcome detected")
)

#' @keywords internal
reg_outcome_kind <- function(y) {
  u <- unique(stats::na.omit(y))
  if (reg_is_binary_outcome(y))                                return("binary")
  if (is.ordered(y) && length(u) >= 3L)                        return("ordered")
  if ((is.factor(y) || is.character(y)) && length(u) >= 3L)    return("nominal")
  if (is.numeric(y))                                           return("numeric")
  ""
}

reg_detect_family <- function(data, outcome) {
  y    <- data[[outcome]]
  kind <- reg_outcome_kind(y)
  if (!nzchar(kind)) {
    cli::cli_abort(c(
      "Cannot auto-detect the model family for {.val {outcome}}.",
      "i" = paste0("Set {.arg family} explicitly: {.val gaussian} (linear), {.val poisson} (counts), ",
                   "{.val binomial} (logistic), {.val multinomial} / {.val ordinal} (3+ level).")
    ))
  }
  fam  <- REG_OUTCOME_KINDS[[kind]]$detect
  said <- REG_OUTCOME_KINDS[[kind]]$said
  tx_inform_once(paste0("family_auto_", outcome), c("i" = paste0(
    "{.val {outcome}}: ", said, " -> {.code family = \"", fam, "\"} (",
    reg_family_short(fam), ")",
    if (identical(kind, "numeric") && !any(y %% 1 != 0, na.rm = TRUE))
      "; it is integer-valued, so {.code family = \"poisson\"} if it is a count" else "",
    "."
  )))
  fam
}

# What a model cell's parenthetical holds. EVERY mode's aside is now a property of the table's own
# display -- the in-cell crude fold is the `est_obs` preset, not a per-cell rewrite -- so there is
# one reader and no mode to special-case.
#' @keywords internal
reg_meta_aside <- function(meta, est = NULL) {
  # ⚠ the comparison too: the default layout differs there (the model columns print no aside at
  # all), so a footer that skipped it would gloss a bracket the cells never write. It is the BUILD's
  # own test -- `predictors` given as a list AND more than one model -- because a one-model list
  # takes the ordinary layout.
  cmp     <- isTRUE(meta$comparison) && length(meta$fit_spec$specs) > 1L
  display <- meta$display %||% reg_display_of(NULL, meta$emp_mode %||% "no", cmp)
  reg_aside_token(display, est$scale)
}

#' @keywords internal
reg_meta_estimand <- function(meta, outcome = NULL, family = NULL) {
  d   <- if (is.null(outcome)) NULL else as.character(outcome)
  if (is.null(d) && !is.null(family) && nzchar(family)) {
    fk   <- unname(REG_FIT_FAMILY[family]); if (is.na(fk)) fk <- family
    fams <- meta$families %||% meta$family
    hit  <- names(fams)[fams %in% c(family, fk)]
    if (length(hit)) d <- hit[[1]] else return(reg_estimand(
      fk, link = meta$link %||% "auto", measure = meta$measure %||% "auto",
      effect = meta$effect %||% "auto"))
  }
  pick <- function(v, scalar) {
    if (is.null(v)) return(scalar)
    if (!is.null(d) && !is.null(names(v)) && d %in% names(v)) return(unname(v[[d]]))
    unname(v[[1]])
  }
  fam <- pick(meta$families, meta$family) %||% "gaussian"
  res <- reg_estimand(fam, link = pick(meta$links, meta$link %||% "auto") %||% "auto",
                      measure = pick(meta$measures, meta$measure %||% "auto") %||% "auto",
                      effect  = pick(meta$effects,  meta$effect  %||% "auto")  %||% "auto")
  if (identical(res$status, "ok")) res else reg_estimand(fam)
}

# THE REFERENCE-DISTRIBUTION CLAUSE of a "Model:" line, and only under a survey design -- elsewhere
# it names a t on thousands of df, which says nothing a reader can act on. The two numbers answer
# different questions: `t(17)` is what THIS model's intervals were referred to (survey's own
# `degf + 1 - p`, read back off the columns that used it), while `20 design df` is the design's own
# #PSU - #strata, which no interval uses directly. It lives here rather than in the colour legend
# because the df is per column there, and naming it would split the crude/adjusted block.
reg_model_df_clause <- function(x, meta, lang = NULL) {
  dg <- meta$design_degf
  if (is.null(dg) || length(dg) != 1L || !is.finite(dg) || dg <= 0) return("")
  cols <- purrr::keep(x, ~ is_fmt(.) && identical(get_role(.), "model"))
  df   <- unique(vapply(cols, fmt_degf_attr, numeric(1), USE.NAMES = FALSE))
  df   <- df[is.finite(df) & df > 0]
  # several models with different residual df: name only the design's, which they do share.
  if (length(df) == 1L) gettextf("t(%s) on %s design df", legend_num(df, lang), legend_num(dg, lang))
  else                  gettextf("%s design df", legend_num(dg, lang))
}

# ONE representative column per ROLE, for the family a "Model:" line speaks for -- all
# display_token_label() needs to name the cell's parts by the abbreviations the table prints
# ("obs%" / "adj%" / "sup%"). A role with no column simply gets no gloss.
#' @keywords internal
#' @noRd
reg_role_cols <- function(x, family = NULL) {
  cols <- purrr::keep(x, ~ is_fmt(.) && (get_role(.) %||% "") %in% c("emp", "model"))
  if (!is.null(family) && length(family) == 1L && nzchar(family)) {
    fk  <- unname(REG_FIT_FAMILY[family]); if (is.na(fk)) fk <- family
    hit <- purrr::keep(cols, ~ get_model_family(.) %in% c(family, fk))
    if (length(hit)) cols <- hit
  }
  # reading order: the observed column sits left of its model twin.
  out <- list()
  for (r in c("emp", "model")) {
    i <- purrr::detect(cols, ~ identical(get_role(.), r))
    if (!is.null(i)) out[[r]] <- i
  }
  out
}

# Does this model hold a NUMBER? The one fact the `at_reference` phrase needs and the estimand
# cascade cannot know: a reference PROFILE holds a factor at its reference level and a number at its
# mean, so with no number there is no mean to mention. `predictor_types` is stored per table.
#' @keywords internal
#' @noRd
reg_has_numeric_predictor <- function(meta) {
  pt <- meta$predictor_types
  if (is.null(pt) || !length(pt)) return(NA)
  any(as.character(pt) %in% c("numeric", "integer", "double"))
}

reg_model_line <- function(meta, df_clause = "", role_cols = list()) {
  if (is.null(meta)) return(NULL)
  fam <- reg_family_display_name(reg_meta_estimand(meta)$fit %||% meta$family)
  e   <- reg_meta_estimand(meta)
  est <- reg_estimand_note(e, aside = reg_meta_aside(meta, e), role_cols = role_cols,
                           has_num = reg_has_numeric_predictor(meta))
  if (nzchar(df_clause))
    est <- if (nzchar(est)) gettextf("%s; %s", est, df_clause) else df_clause
  # `who` carries no leading space: xgettext strips edge whitespace from a msgid, so the space and
  # the punctuation live in the outer template, which the translation then controls.
  who <- if (isTRUE(meta$comparison)) {
    pl <- meta$positive_level[[1]]
    w  <- if (!is.na(pl)) gettextf("of %s ('%s')", meta$outcome[[1]], pl)
          else            gettextf("of %s", meta$outcome[[1]])
    paste0(" ", w)
  } else ""
  line <- if (nzchar(est)) gettextf("Model: %s%s; %s.", fam, who, est)
          else            gettextf("Model: %s%s.", fam, who)
  enc2utf8(line)
}

# reg_outcome_scale() -- WHAT "HIGHER" MEANS, for the outcomes that report on their order. A rank
# column names its outcome and nothing else, so unlike a per-category table it never shows the
# categories: the footer has to. Stored on the record because the footer is composed from it long
# after the data is gone, and computed only where a family actually reads on a rank.
#' @keywords internal
#' @noRd
reg_outcome_scale <- function(data, outcome, families) {
  out <- list()
  for (d in unique(as.character(outcome))) {
    fm <- unname(families[[d]] %||% "")
    if (!identical(REG_FAMILIES[[fm]]$level %||% "", "rank")) next
    y <- data[[d]]
    if (is.null(y)) next
    y  <- forcats::fct_drop(as.factor(y))
    lv <- levels(y)
    if (length(lv) < 2L) next
    out[[d]] <- list(levels = lv, share = unname(as.numeric(table(y)) / sum(!is.na(y))))
  }
  out
}

# The footer's own rendering of it: the ordered levels, low to high, each with the share of the
# sample sitting on it -- so the reader sees both the direction and the shape of the scale.
#' @keywords internal
#' @noRd
reg_scale_lines <- function(meta) {
  sc <- meta$outcome_scale
  if (!length(sc)) return(character(0))
  vapply(names(sc), function(d) {
    r <- sc[[d]]
    enc2utf8(gettextf("%s, from low to high: %s.", d, paste0(
      r$levels, " (", round(r$share * 100), "%)", collapse = " < ")))
  }, character(1), USE.NAMES = FALSE)
}

reg_model_lines <- function(x, lang = NULL) {
  meta <- reg_call(x)
  if (is.null(meta)) return(character(0))
  with_legend_lang(lang, function(lg) {
    fams <- meta$families; if (is.null(fams)) fams <- meta$family
    uf   <- unique(fams)
    dfc  <- reg_model_df_clause(x, meta, lg)
    scl <- reg_scale_lines(meta)
    if (length(uf) <= 1L) { rl <- reg_model_line(meta, dfc, reg_role_cols(x))
                            return(c(if (is.null(rl)) character(0) else rl, scl)) }
    deps <- meta$outcome
    c(vapply(uf, function(fm) {
      grp   <- deps[fams == fm]
      e     <- reg_meta_estimand(meta, grp[[1]])
      fname <- reg_family_display_name(e$fit %||% fm)
      est   <- reg_estimand_note(e, aside = reg_meta_aside(meta, e),
                                 role_cols = reg_role_cols(x, fm),
                                 has_num = reg_has_numeric_predictor(meta))
      if (nzchar(dfc)) est <- if (nzchar(est)) gettextf("%s; %s", est, dfc) else dfc
      enc2utf8(if (nzchar(est)) gettextf("Model (%s): %s; %s.", legend_name_list(grp), fname, est)
               else            gettextf("Model (%s): %s.", legend_name_list(grp), fname))
    }, character(1), USE.NAMES = FALSE), scl)
  })
}

# The AGGREGATED effect-modification test, one footer LINE per model. A pooled test belongs to no
# single model column, which is the only thing the footer-ROW machinery can key on.
#' @keywords internal
reg_interaction_lines <- function(x, lang = NULL) {
  tt <- get_test(x)
  if (is.null(tt) || nrow(tt) == 0) return(character(0))
  it <- tt[tt$test %in% reg_interaction_types(), , drop = FALSE]
  if (nrow(it) == 0) return(character(0))
  meta <- reg_call(x)
  sv   <- if (is.null(meta)) NA_character_ else meta$tab_vars
  with_legend_lang(lang, function(lg) {
    tname <- vapply(reg_interaction_types(),
                    function(k) gettext(TEST_ROWS[[k]]$instrument), character(1))
    on_coef <- !is.null(meta) && !identical(meta$effect %||% "conditional", "conditional")
    vapply(split(seq_len(nrow(it)), factor(it$col, levels = unique(it$col))), function(idx) {
      d     <- it[idx, , drop = FALSE]
      items <- paste0(test_key_col(d, "var"), " p = ", test_fmt_pvalue(d$pvalue),
                      stars_from_pvalue(d$pvalue))
      kind  <- unname(tname[d$test[1]]); if (is.na(kind)) kind <- gettext("Wald test")
      what  <- if (on_coef) gettextf("%s on the coefficients", kind) else kind
      head  <- if (!is.na(sv) && nzchar(sv)) gettextf("Interaction with %s (%s):", sv, what)
               else                          gettextf("Interaction (%s):", what)
      enc2utf8(paste0(head, " ", paste(items, collapse = ", "), "."))
    }, character(1), USE.NAMES = FALSE)
  })
}

reg_title <- function(meta, max = 2, lang = NULL) {
  if (is.null(meta)) return(NA_character_)
  fams <- meta$families; if (is.null(fams)) fams <- meta$family
  mixed <- length(unique(fams)) > 1L
  with_legend_lang(lang, function(lg) {
    fam <- reg_family_display_name(meta$family)
    Fam <- if (mixed) gettext("Regression models")
           else paste0(toupper(substr(fam, 1, 1)), substr(fam, 2, nchar(fam)))
    dep <- tab_title_names(meta$outcome, max)
    tabbed <- if (!is.null(meta$tab_vars)) paste0(" ", gettextf("(tabbed by %s)", meta$tab_vars)) else ""
    by_of  <- function(preds) if (nzchar(preds)) paste0(" ", gettextf("by %s", preds)) else ""
    if (mixed) return(enc2utf8(paste0(Fam, ": ", dep, by_of(tab_title_names(meta$predictors, max)), tabbed)))
    if (isTRUE(meta$comparison)) {
      pl  <- meta$positive_level[[1]]
      dref <- if (!is.na(pl)) paste0(dep, ", '", pl, "' (", meta$eff_word, ")")
              else            paste0(dep, " (", meta$eff_word, ")")
      enc2utf8(gettextf("%s (models comparison): %s", paste0(Fam, "s"), paste0(dref, tabbed)))
    } else {
      preds <- tab_title_names(meta$predictors, max)
      enc2utf8(paste0(Fam, ": ", dep, by_of(preds), tabbed))
    }
  })
}

reg_sheet_name <- function(meta) {
  if (is.null(meta)) return(NA_character_)
  fams <- meta$families; if (is.null(fams)) fams <- meta$family
  tail <- if (isTRUE(meta$comparison)) c(meta$outcome[[1]], "compare")
          else                         c(meta$outcome, meta$predictors)
  short <- if (length(unique(fams)) > 1L) "reg" else reg_family_short(meta$family)
  paste(c(short, tail), collapse = "_")
}

# The shared col_var of a model column and its crude companion, so ONE span names the outcome and no
# border separates them. ⚠ a SUMMED SCORE (`trials =`) has no level to name -- its "positive level"
# is the internal binomial success code, and naming it split one comparison in two.
reg_shared_col_var <- function(family, outcome, positive_level, cleannames, trials = NULL) {
  named <- reg_fam_binary(family) && is.null(trials) &&
    !is.null(positive_level) && !is.na(positive_level)
  if (named) paste0(outcome, ": ", reg_cleanup(positive_level, cleannames)) else outcome
}

# On a PER-CATEGORY column set the outcome CATEGORY takes the header slot, so the measure has nowhere
# else to go and lives in the span. A comparison keeps each model's name.
reg_category_col_var <- function(sp, is_comparison, positive_level, cleannames) {
  if (isTRUE(is_comparison)) return(sp$label)
  paste0(reg_shared_col_var(sp$fit_family, sp$outcome, positive_level, cleannames, sp$trials), ": ",
         reg_word(sp$est))
}

reg_model_col_name <- function(eff_word, outcome, is_comparison, model_label, n_outcomes) {
  if (isTRUE(is_comparison)) return(model_label)
  if (n_outcomes > 1L) paste0("Model_", eff_word, " [", outcome, "]") else paste0("Model_", eff_word)
}

reg_prep_binary <- function(data, outcome, outcome_level = NULL) {
  y <- data[[outcome]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    y <- factor(y, levels = c(0, 1), labels = c(paste0("Not ", outcome), outcome))
    if (!is.null(outcome_level) && outcome_level %in% c(paste0("Not ", outcome), "0"))
      y <- forcats::fct_rev(y)
  } else {
    y <- forcats::fct_drop(as.factor(y))
    if (nlevels(y) != 2L) {
      cli::cli_abort(c(
        "The outcome variable {.val {outcome}} must be binary (2 levels).",
        "x" = "It has {nlevels(y)} level{?s}: {.val {levels(y)}}.",
        "i" = paste0('Use {.code family = "multinomial"} (unordered) or ',
                     '{.code family = "ordinal"} (ordered), or {.arg trials} for a summed score.')
      ))
    }
    # glm models levels(y)[2], so the chosen level goes LAST.
    if (identical(levels(y)[[1]], reg_positive_level(data, outcome, outcome_level)))
      y <- forcats::fct_rev(y)
  }
  data[[outcome]] <- y
  attr(data, "positive_level") <- levels(y)[[2L]]
  data
}

# ONE LEVEL AGAINST THE REST -- `family = "binomial"` on a 3+ level outcome. Done at the ARGUMENT
# BOUNDARY, with the anchors, the relevels and the crosses, and NOT in reg_fit_frame(): the crude
# block and reg_check_plots()' replay rebuild the model frame independently, and all three must be
# looking at the same column. After it, every engine sees an ordinary binary outcome.
# ⚠ said once, because it changes what the table IS: a reader must know which categories were merged.
#' @keywords internal
#' @noRd
reg_binarise_outcome <- function(data, outcome, outcome_level = NULL, announce = TRUE) {
  y <- data[[outcome]]
  if (!(is.factor(y) || is.character(y))) return(data)
  y  <- forcats::fct_drop(as.factor(y))
  if (nlevels(y) <= 2L) return(data)
  lv <- if (!is.null(outcome_level) && outcome_level %in% levels(y)) outcome_level
        else levels(y)[[1L]]
  rest <- gettextf("Not %s", lv)
  if (announce)
    tx_inform_once(paste0("binomial_collapse_", outcome), c("i" = paste0(
      "{.val {outcome}}: {.code family = \"binomial\"} models {.val {lv}} against the ",
      "{nlevels(y) - 1L} other categories, merged.")))
  data[[outcome]] <- factor(ifelse(as.character(y) == lv, lv, rest), levels = c(lv, rest))
  data
}

reg_positive_level <- function(data, outcome, outcome_level = NULL) {
  y <- data[[outcome]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    neg <- paste0("Not ", outcome)
    return(if (identical(outcome_level, "0") || identical(outcome_level, neg)) neg else outcome)
  }
  lv <- levels(forcats::fct_drop(as.factor(y)))
  if (!is.null(outcome_level) && outcome_level %in% lv) outcome_level else lv[[1L]]
}

# === SECTION: `ref` -- the reference every effect is measured from ===============================
#
# ONE argument, one meaning per kind of variable. A FACTOR's reference is the level the others are
# compared against. A CONTINUOUS predictor's is the value it is ANCHORED at, realised by SHIFTING
# the column at the argument boundary -- so the fit's own coefficients are already anchored and no
# consumer downstream needs a contrast engine. The predictor's own slope never moves; the Constant
# row, and every term the predictor interacts with, do.
# ⚠ the two vocabularies are DISJOINT, which is what lets a bare default name its own kind.

#' @keywords internal
REG_ANCHOR_KEYWORDS <- c("mean", "median", "min", "max")

#' @keywords internal
REG_LEVEL_KEYWORDS <- c("first", "last")

# Which kind of predictor a `ref` DEFAULT addresses. NA = neither vocabulary -> the shared resolver
# aborts (a bare level name cannot be portable across factors, so it must be named).
#' @keywords internal
reg_ref_fallback_kind <- function(v) {
  if (is.numeric(v)) return("numeric")
  s <- trimws(tolower(as.character(v)[[1]]))
  if (s %in% REG_ANCHOR_KEYWORDS) return("numeric")
  if (s %in% REG_LEVEL_KEYWORDS)  return("factor")
  if (!is.na(suppressWarnings(as.numeric(s)))) return("numeric")
  NA_character_
}

# A factor's reference: a LEVEL NAME first, the keyword only when no level matches -- so a variable
# owning a level called "last" keeps its own reading.
#' @keywords internal
reg_ref_level <- function(value, x, var) {
  # ⚠ EMPTY levels dropped first: reg_fit() drops them, so "last" must name the last level the model
  # will actually see -- otherwise the reference silently lands on the next one.
  lv <- levels(forcats::fct_drop(as.factor(x)))
  s  <- as.character(value)[[1]]
  if (s %in% lv) return(s)                # a NAMED level wins, the missing-value one included
  k <- trimws(tolower(s))
  # ⚠ A POSITIONAL keyword names a substantive GROUP, so it skips the level `na =
  # "keep_for_predictors"` gave the missing values -- which sorts LAST, and so was what "last"
  # selected. tab()'s two axes read the same rule (diff_index() / calculate_refrows()).
  grp <- setdiff(lv, TAB_NA_LEVEL)
  if (!length(grp)) grp <- lv             # a predictor that is nothing but missing
  if (identical(k, "first")) return(grp[[1L]])
  if (identical(k, "last"))  return(grp[[length(grp)]])
  cli::cli_abort(c("{.arg ref} level {.val {s}} not found in {.val {var}}.",
                   "i" = "Levels: {.val {lv}}."), call = NULL)
}

# A continuous predictor's anchor, as a number. WEIGHTED wherever the call is: the anchor, the
# multiplier's SD and a quantile shape's breaks are three readings of ONE distribution and must
# never disagree about which one they read.
#' @keywords internal
reg_anchor_value <- function(value, x, w = NULL, var = "x") {
  if (is.numeric(value)) return(as.numeric(value)[[1]])
  s <- trimws(tolower(as.character(value)[[1]]))
  k <- suppressWarnings(as.numeric(s))
  if (!is.na(k)) return(k)
  x <- as.numeric(x)
  switch(s,
         mean   = wtd_mean(x, w),
         median = shape_wquantile(x, 0.5, w),
         min    = suppressWarnings(min(x[is.finite(x)])),
         max    = suppressWarnings(max(x[is.finite(x)])),
         cli::cli_abort(c(paste0("{.arg ref} for the continuous predictor {.val {var}} must be a ",
                                 "number or {.or {.val {REG_ANCHOR_KEYWORDS}}}."),
                          "x" = "Got {.val {s}}."), call = NULL))
}

# The two rewriters. Pure now that the boundary validates: each takes the RESOLVED map and applies
# it, so a survey design is served by the caller re-assigning $variables, as every other data
# rewrite of R/reg-resolve.R's S2 already does.
#' @keywords internal
reg_relevel_data <- function(data, levels) {
  for (v in intersect(names(levels), names(data)))
    data[[v]] <- forcats::fct_relevel(as.factor(data[[v]]), levels[[v]])
  data
}

# WARNING: `as.numeric()` is load-bearing -- reg_is_factor_var() keeps a Date numeric, and
# `Date - <number>` shifts by DAYS.
# Which variables a COMPOUND formula uses AS THEMSELVES. A bare symbol may be anchored -- the fit is
# then the same model in different coordinates, which is exactly what an interacted `race * age`
# wants. A variable inside a CALL (`poly(age, 2)`, `log(age)`, `I(age^2)`, `offset()`) may not: the
# transform would be applied to shifted values, which is a different model, or undefined.
#' @keywords internal
reg_formula_bare_vars <- function(formula, vars) {
  if (is.null(formula) || length(vars) == 0L) return(character(0))
  wrapped <- character(0)
  walk <- function(e, inside) {
    if (is.symbol(e)) {
      if (inside) wrapped <<- c(wrapped, as.character(e))
      return(invisible(NULL))
    }
    if (!is.call(e)) return(invisible(NULL))
    op  <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
    nxt <- inside || !(op %in% c("+", "-", "*", "/", ":", "("))
    for (k in seq_along(e)[-1L]) walk(e[[k]], nxt)
    invisible(NULL)
  }
  walk(formula[[length(formula)]], FALSE)
  setdiff(vars, wrapped)
}

# The anchor keyword, translated. A literal per arm: gettext() on a bare variable is invisible to
# potools, so the extractor must see each word written out.
#' @keywords internal
reg_anchor_word <- function(k)
  switch(k, mean = gettext("mean"), median = gettext("median"),
         min = gettext("min"), max = gettext("max"), k)

# One predictor's anchor, 0 when it has none. ⚠ `anchors[[v]]` cannot be used: on a named NUMERIC an
# absent name is an error, not NULL.
#' @keywords internal
reg_anchor_of <- function(anchors, v) {
  a <- if (is.null(anchors)) NA_real_ else unname(anchors[v])
  if (length(a) != 1L || is.na(a)) 0 else a
}

#' @keywords internal
reg_anchor_apply <- function(data, anchors) {
  for (v in intersect(names(anchors), names(data))) {
    if (anchors[[v]] == 0) next                       # nothing to shift, and nothing to coerce
    data[[v]] <- as.numeric(data[[v]]) - anchors[[v]]
  }
  data
}

# === SECTION: The coefficient skeleton and the shared Wald assembly =============================

# The row skeleton, in display order; `term` matches lm / glm / svyglm coefficient names, so a fit
# aligns to it by term. `shape_terms` adds a non-linear `shape =`'s CURVATURE row, breaking the
# 1-to-1 by a rule: ONE ROW PER MODEL TERM on the coefficient path, ONE ROW PER PREDICTOR on the
# marginal one.
reg_skeleton <- function(data, predictors, shape_terms = NULL, crosses = list()) {
  parts <- purrr::map(predictors, function(p) {
    # a crossed pair's `cells` arm IS a factor column, so only the nested one needs its own rows.
    rec <- reg_cross_of(crosses, p)
    if (!is.null(rec) && identical(rec$arm, "nested")) return(reg_cross_skeleton(rec, data))
    v <- data[[p]]
    if (reg_is_factor_var(v)) {
      lv <- levels(forcats::fct_drop(as.factor(v)))
      tibble::tibble(
        var    = p,
        level  = lv,
        term   = c(NA_character_, paste0(p, lv[-1])),
        is_ref = c(TRUE, rep(FALSE, length(lv) - 1L))
      )
    } else {
      sq <- if (!is.null(shape_terms) && p %in% names(shape_terms)) shape_terms[[p]] else NULL
      tibble::tibble(
        var    = p,
        level  = c(p, if (!is.null(sq)) reg_shape_sq_level(p)),
        term   = c(p, if (!is.null(sq)) gsub("`", "", sq, fixed = TRUE)),
        is_ref = rep(FALSE, 1L + !is.null(sq))
      )
    }
  })
  dplyr::bind_rows(reg_constant_row(), parts)
}

# The skeleton's intercept row. `level` is a KEY, not a label: the displayed one says which baseline
# the contrast puts there ("Reference profile" / "Population average") and is written once, at row
# time, by reg_stage_rows(). ⚠ the `var` key "Constant" is read by forest_plot(intercept =),
# tab_constant_null() and reg_level_counts(), and never changes.
#' @keywords internal
reg_constant_row <- function()
  tibble::tibble(var = "Constant", level = "Constant", term = "(Intercept)", is_ref = TRUE)

# reg_skeleton_reorder() -- A PREDICTOR'S LEVEL ORDER IS DISPLAY, AND ONLY THE REFERENCE IS NOT.
# Every factor predictor is fitted under TREATMENT contrasts (reg_fit_frame() strips `ordered`
# precisely so that it is), so the order decides one thing -- which level the others are compared to
# -- and that one thing is `ref =`, which relevels the DATA and is an honest refit. Everything else
# is a permutation of rows, applied here, to the skeleton, where it costs nothing and moves no
# number: the fit never sees it, so a reorder is a cache HIT in every family, ordinal included.
#
# ⚠ THE REFERENCE ROW STAYS FIRST, and only the rest are permuted -- `is_ref` and `term` were built
# positionally by reg_skeleton() (`term = paste0(p, lv[-1])`), and every later stage reads them as
# flags. Unlisted levels keep their relative position, trailing, which is fct_relevel()'s own
# contract and therefore the panel's.
# ⚠ `levels_order` names MERGED levels: `.levels_collapse` has already run when the skeleton is
# built, so a caller holding raw names translates them first (jmvtab_reg_build).
#' @keywords internal
#' @noRd
reg_skeleton_reorder <- function(skeleton, levels_order) {
  if (is.null(skeleton) || !length(levels_order)) return(skeleton)
  vars <- intersect(names(levels_order), as.character(skeleton$var))
  for (v in vars) {
    k <- which(as.character(skeleton$var) == v & !skeleton$is_ref)
    if (length(k) < 2L) next
    pos <- match(as.character(skeleton$level[k]), levels_order[[v]])
    # an unlisted level sorts after every listed one, keeping its own relative position (stable sort)
    pos[is.na(pos)] <- length(levels_order[[v]]) + seq_len(sum(is.na(pos)))
    skeleton[k, ] <- skeleton[k[order(pos)], ]
  }
  skeleton
}

reg_skeleton_from_fit <- function(fit) {
  tt      <- stats::terms(fit)
  labels  <- attr(tt, "term.labels")
  mm      <- stats::model.matrix(fit)
  assign  <- attr(mm, "assign")                         # 0 = intercept, k = labels[k]
  # ⚠ the coefficient names come off the MODEL MATRIX, the vector `assign` indexes, NEVER off coef(),
  # whose shape is the FITTER's: nnet::multinom returns a MATRIX (names() NULL) and MASS::polr drops
  # the intercept from coef() but not from the model matrix.
  coefnms <- colnames(mm)
  xlev    <- fit$xlevels

  parts <- purrr::map(seq_along(labels), function(k) {
    lab  <- labels[k]
    cols <- coefnms[assign == k]
    if (lab %in% names(xlev)) {                          # pure factor main effect -> level rows
      lv <- xlev[[lab]]
      tibble::tibble(
        var    = lab,
        level  = lv,
        term   = c(NA_character_, paste0(lab, lv[-1])),
        is_ref = c(TRUE, rep(FALSE, length(lv) - 1L))
      )
    } else {                                             # numeric / interaction / poly / I() -> terms
      lvl <- sub(paste0("^", term_prefix(lab)), "", cols)
      lvl[!nzchar(lvl)] <- cols[!nzchar(lvl)]            # a single-column term keeps the full name
      tibble::tibble(var = lab, level = lvl, term = cols, is_ref = FALSE)
    }
  })
  dplyr::bind_rows(reg_constant_row(), parts)
}

term_prefix <- function(label) {
  gsub("([.\\\\+*?\\[^\\]$(){}=!<>|:#/-])", "\\\\$1", label, perl = TRUE)
}

reg_cleanup <- function(x, cleannames)
  if (isTRUE(cleannames)) gsub(cleannames_condition(), "", x, perl = TRUE) else x

# The (var, level [, extra]) join key aligning fitted results onto skeleton rows: the separator is a
# carriage return, which can never appear inside a variable name or a factor level.
reg_skel_key <- function(var, level, extra = NULL)
  if (is.null(extra)) paste(var, level, sep = "\r") else paste(var, level, extra, sep = "\r")

reg_skel_match <- function(skeleton, src) {
  if (is.null(src) || !nrow(src)) return(rep(NA_integer_, nrow(skeleton)))
  match(reg_skel_key(skeleton$var, skeleton$level), reg_skel_key(src$var, src$level))
}

# Per-coefficient LIKELIHOOD-RATIO p-values -- the dual of the profile-likelihood interval;
# unweighted glm only, and for a factor a test of one level against the reference.
reg_lr_pvalues <- function(fit) {
  X   <- stats::model.matrix(fit)
  y   <- fit$y
  w   <- fit$prior.weights
  off <- fit$offset
  if (is.null(off)) off <- rep(0, length(y))
  ic  <- which(colnames(X) == "(Intercept)")
  dev_full <- fit$deviance
  p <- vapply(seq_len(ncol(X)), function(j) {
    red <- suppressWarnings(stats::glm.fit(
      X[, -j, drop = FALSE], y, weights = w, offset = off, family = fit$family,
      intercept = length(ic) > 0L && j != ic
    ))
    stats::pchisq(red$deviance - dev_full, df = 1, lower.tail = FALSE)
  }, numeric(1))
  stats::setNames(p, gsub("`", "", colnames(X), perl = TRUE))
}

# === SECTION: ONE REFERENCE DISTRIBUTION PER FIT ================================================
#
# THE RULE: a fit decides z-or-t ONCE, and everything it goes on to produce reads that decision back
# -- the coefficient interval, the marginal sweep, the baseline row, and a crude column refit from
# it. Nothing downstream may assume z. The pair (`disp_known`, `df_residual`) travels out of
# reg_fit() beside the tidy, and is frozen into the jamovi digest for the same reason.
#
# ⚠ `df.residual()` on an svyglm is `degf(design) + 1 - p`, NOT n - p -- survey's own convention, and
# what confint.svyglm refers to. That is deliberate here and wrong for a dispersion denominator (see
# reg_dispersion(), which computes n - rank by hand).

# The fit's own arm of the package's one critical value. `conf_level_to_crit()` sanitises the df
# through df_clean(), so an absent one (NULL / NA -- a 3+ level engine, a distilled fit) degrades to
# z rather than propagating NA into every bound.
reg_wald_crit <- function(disp_known, df_residual, conf_level) {
  if (isTRUE(disp_known)) stats::qnorm(1 - (1 - conf_level) / 2)
  else                    conf_level_to_crit(conf_level, df_residual)
}

# NA rather than an error wherever the engine defines no residual df -- a 3+ level fitter, a fit that
# lost its frame. conf_level_to_crit() / df_clean() read NA as "refer to z", so the fallback is the
# same one every other df in the package takes.
reg_df_residual <- function(fit) {
  d <- suppressWarnings(tryCatch(as.numeric(stats::df.residual(fit)), error = function(e) NA_real_))
  if (length(d) != 1L || !is.finite(d) || d <= 0) NA_real_ else d
}

# WHAT A WALD-BUILT COLUMN STAMPS about its own interval -- written once because three builders do it
# (the coefficient column, a marginal one, and a crude column refit from the same fitter), and a crude
# column must stamp EXACTLY what its model twin does or the two stop folding into one legend block.
#   the METHOD word its legend renders; `mult` = the estimate is multiplicative, so Wald on the log.
reg_wald_method_name <- function(method, mult)
  if (identical(method, "profile")) "profile" else if (isTRUE(mult)) "wald_log" else "wald"
#   the DF it was referred to. NA where the reference is z, and NA under `profile`, whose
#   likelihood-ratio bounds refer to no distribution at all; get_degf() reads NA as "refer to z".
reg_wald_degf <- function(method, disp_known, df_residual) {
  if (identical(method, "profile") || isTRUE(disp_known)) return(NA_real_)
  as.double(df_residual %||% NA_real_)
}

# The shared Wald assembly: the interval is est +/- crit * se and the p is recomputed from those same
# two numbers, so bounds and stars are exact duals. `disp_known` picks z (dispersion fixed by the
# family) over t on `df` (dispersion estimated: lm, quasi*, weighted).
reg_wald_finalize <- function(est, do_exp, se = NULL, crit = NULL,
                              lo = NULL, hi = NULL, p = NULL, disp_known = TRUE, df = NULL) {
  if (is.null(lo)) lo <- est - crit * se
  if (is.null(hi)) hi <- est + crit * se
  if (is.null(p))
    p <- if (isTRUE(disp_known)) 2 * stats::pnorm(-abs(est / se))
         else                    2 * stats::pt(-abs(est / se), df = df)
  if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  list(estimate = est, conf.low = lo, conf.high = hi, p.value = p)
}

# A k-unit change multiplies the native-scale coefficient by k (se by |k|); the p is scale-invariant.
# ONE writer for every family, called at FINALIZE, beside the exponentiation and the interval --
# because `multiplier` is a REPORTING choice like them and nothing in the fit reads it. That is what
# keeps `tidy_native` genuinely native, and what lets the scaling leave the jamovi fit-cache key
# (jmvreg_fit_key): a scaling pick is then a cache hit rather than a refit.
# WARNING: it also carries the PROFILE bounds, which ride natively on the record. They are scaled by
#   the SIGNED k and so must be re-ordered -- a negative multiplier would otherwise hand back an
#   inverted bracket (lo > hi).
# `td$term == v` is an exact match, so a shape-generated squared term is never scaled -- and a
# multinomial tidy's `term x y.level` rows all scale together, which is what a per-category
# coefficient wants.
#' @keywords internal
#' @noRd
reg_tidy_rescale <- function(td, multiplier) {
  mult_vec <- rep(1, nrow(td))
  if (!is.null(multiplier)) {
    # the variable IS the term, or is one PART of an interaction: a crossed slope `raceBlack:age`
    # is still an effect per unit of `age`. A shape-generated squared term names no part, so it
    # stays unscaled, and a multinomial's `term x y.level` rows still scale together.
    parts <- strsplit(td$term, ":", fixed = TRUE)
    for (v in names(multiplier)) {
      mi <- vapply(parts, function(pp) any(pp == v), logical(1))
      if (any(mi)) mult_vec[mi] <- as.numeric(multiplier[[v]])
    }
    td$estimate  <- td$estimate  * mult_vec
    td$std.error <- td$std.error * abs(mult_vec)
    # ⚠ names(), not `$`: a tibble WARNS on an unknown column, and a Wald tidy has no bounds yet.
    if (all(c("conf.low", "conf.high") %in% names(td))) {
      lo <- td$conf.low * mult_vec; hi <- td$conf.high * mult_vec
      td$conf.low <- pmin(lo, hi);  td$conf.high <- pmax(lo, hi)
    }
  }
  td
}

# The 3+ level engines' NATIVE tidy: the fit's own estimate / std.error plus its p, everything
# reg_tidy_finalize() needs and nothing the estimand -- or the scaling -- decides.
# ⚠ the p is computed BEFORE any `multiplier`, and is the same number after it: the estimate scales
# by k and the SE by |k|, so |est/se| does not move. No test statistic depends on the scaling.
# ⚠ z, deliberately and for all four of them: multinom, polr, svy_vglm and svyolr define no
# residual-df convention (there is no single equation to count against), and their own summaries
# report z. So a multinomial or ordinal table is internally consistent on z -- its coefficient
# columns, its marginal columns and its crude twin all refer the same way. Their records therefore
# carry `disp_known = TRUE` and `df_residual = NA`, which is what makes reg_wald_crit() give qnorm.
reg_tidy_native_z <- function(td) {
  td$p.value <- 2 * stats::pnorm(-abs(td$estimate / td$std.error))
  td
}

# THE NATIVE TIDY OF EVERY FIT -- (term, estimate, std.error) on the model's own scale, read off its
# summary(). Three shapes, because three engines lay their
# coefficients out differently; the weighted twins above build the same columns by hand, so all five
# fitters hand reg_fit_record() one contract.

# stats::lm / stats::glm / survey::svyglm: one (estimate, se, statistic, p) matrix, terms as rownames.
# ⚠ the SPINE is names(coef(fit)), not the summary's rownames: an ALIASED coefficient (a collinear
# column the fitter dropped) is NA in coef() and simply ABSENT from the summary. Keeping its row
# means a rank-deficient fit still aligns term-for-term with the skeleton reg_column() matches on.
# The columns are taken by POSITION: the headers differ per family (t / z), the positions never do.
#' @keywords internal
reg_tidy_coefmat <- function(fit) {
  cf <- stats::coef(fit)
  cm <- stats::coef(summary(fit))
  i  <- match(names(cf), rownames(cm))
  tibble::tibble(term      = names(cf),        estimate  = unname(cf),
                 std.error = unname(cm[i, 2]), statistic = unname(cm[i, 3]),
                 p.value   = unname(cm[i, 4]))
}

# nnet::multinom: one block per NON-REFERENCE outcome category, terms in the fit's own column order.
# ⚠ TWO LEVELS IS A REAL CASE -- `family = "multinomial"` on a binary outcome, or a category emptied
# by the complete-case filter -- and nnet then returns its coefficients as a plain named VECTOR. The
# category name has to come from fit$lev[-1]: it is the key reg_columns_multinom() filters on, so a
# block labelled anything else yields a column of NA.
#' @keywords internal
reg_tidy_multinom <- function(fit) {
  s  <- summary(fit)
  co <- s$coefficients; se <- s$standard.errors
  if (is.null(dim(co))) {
    co <- matrix(co, nrow = 1L, dimnames = list(fit$lev[-1], names(co)))
    se <- matrix(se, nrow = 1L, dimnames = list(fit$lev[-1], names(se)))
  }
  trm <- colnames(co)
  dplyr::bind_rows(lapply(rownames(co), function(r) tibble::tibble(
    y.level = r, term = trm, estimate = unname(co[r, trm]), std.error = unname(se[r, trm]))))
}

# MASS::polr: the summary matrix is rbind(slopes, cut-points), and only the slopes are effects.
# names(coef()) is MASS's own answer to which rows those are -- its coef() drops the zeta
# thresholds, its vcov() does not.
#' @keywords internal
reg_tidy_polr <- function(fit) {
  cm <- stats::coef(summary(fit))
  i  <- match(names(stats::coef(fit)), rownames(cm))
  tibble::tibble(term = rownames(cm)[i], estimate = unname(cm[i, 1]), std.error = unname(cm[i, 2]))
}


# === SECTION: The 3+ level engines (multinomial / proportional-odds) ============================

# THE model formula of EVERY fitter, glm path included -- so what a table says it fitted
# (reg_formulas()) and what reg_fit() hands to glm() are one assembly. A compound `formula` is fitted
# VERBATIM (it owns its RHS, so the shape terms do not apply). ⚠ it must reach every fitter -- a 3+
# level engine building its own formula silently dropped the user's interactions from the MODEL, not
# merely from the table.
#   response   overrides the plain outcome: the grouped binomial's `cbind()` pair or `.gb_succ`.
#   cross      a tab_vars, making the POOLED interaction fit: `(x1 + x2) * g`.
#   offset     an offset term appended to the RHS (the grouped modified Poisson's log(trials)).
#' @keywords internal
#' @noRd
reg_fit_formula <- function(outcome, predictors, add_terms = NULL, formula = NULL,
                            response = NULL, cross = NULL, offset = NULL) {
  if (!is.null(formula)) return(formula)
  rhs <- paste0("`", predictors, "`", collapse = " + ")
  if (!is.null(cross))    rhs <- paste0("(", rhs, ") * `", cross, "`")
  if (length(add_terms))  rhs <- paste(c(rhs, add_terms), collapse = " + ")
  if (!is.null(offset))   rhs <- paste0(rhs, " + ", offset)
  stats::as.formula(paste0(response %||% paste0("`", outcome, "`"), " ~ ", rhs))
}

# The RESPONSE side of a grouped-binomial fit, from the estimand's link key: the modified Poisson
# models the success COUNT (with log(trials) as offset), every other link the two-column pair.
#' @keywords internal
#' @noRd
reg_grouped_response <- function(family)
  if (identical(family, "rr")) "`.gb_succ`" else "cbind(`.gb_succ`, `.gb_fail`)"

#' The model formulas a regression table fitted
#'
#' Shows the formula behind every column of a [tab_reg()] table --- exactly what reached
#' [stats::glm()], [survey::svyglm()], [nnet::multinom()] or [MASS::polr()]. Use it to check what a
#' `shape =`, a `trials =` or a model formula really built.
#'
#' @details
#' One row per model: several `outcome`s give one each, a `predictors` list one per model. Two things
#' the list does not repeat: under `tab_vars` the same formula is fitted **within each group**, and
#' `color = "between_groups"` (or `stats = "group_interaction"`) fits one extra pooled model for the footer
#' test only.
#'
#' A summed score (`trials =`) is fitted on a success / failure pair, so its formula names the two
#' internal columns tabxplor builds for it (`.gb_succ`, `.gb_fail`, and `.gb_trials` in the offset of
#' the risk-ratio link).
#'
#' The formula names the columns as the user wrote them, but a continuous predictor is fitted
#' **anchored** at its `ref` (its mean by default), and a `shape =` may have recoded it --- neither
#' changes any effect, only what the Constant row means.
#'
#' @param x A table built by [tab_reg()].
#' @return A tibble with one row per model: `model` (its name in the table), `outcome`, `family`
#'   (the outcome family), `fit` (the internal link the measure asked for --- `"rr"`, `"rd"`, `"mr"`
#'   --- or the family itself) and `formula`.
#' @seealso [tab_reg()], [reg_measures()] (what an outcome can be modelled as).
#' @export
#' @examples
#' \donttest{
#' d <- forcats::gss_cat
#' d$married <- as.integer(d$marital == "Married")
#' reg_formulas(tab_reg(d, "married", c("race", "age"), family = "binomial"))
#' }
reg_formulas <- function(x) {
  meta <- reg_call(x)
  if (is.null(meta) || is.null(meta$fit_spec))
    cli::cli_abort(c("{.arg x} must be a table built by {.fn tab_reg}.",
                     "i" = "A crosstab has no model to show a formula for."))
  fs <- meta$fit_spec
  dplyr::bind_rows(purrr::map(fs$specs, function(sp) {
    # the SAME assembly reg_fit() runs, on the SAME stored inputs -- so this cannot drift from what
    # was fitted, exactly as reg_check_plots() refits from this same recipe.
    grouped <- reg_is_grouped_binomial(sp$fit_family, sp$trials, sp$compound)
    fml <- reg_fit_formula(
      sp$outcome, sp$predictors,
      add_terms = c(reg_shape_add(fs$shape_terms, sp$predictors),
                    reg_cross_add(fs$crosses, sp$cross)), formula = sp$formula,
      response  = if (grouped) reg_grouped_response(sp$fit_family),
      offset    = if (grouped && identical(sp$fit_family, "rr")) "offset(log(`.gb_trials`))")
    tibble::tibble(
      model   = sp$label, outcome = sp$outcome,
      family  = if (sp$fit_family %in% names(REG_FIT_FAMILY))
                  unname(REG_FIT_FAMILY[[sp$fit_family]]) else sp$fit_family,
      fit     = sp$fit_family,
      formula = paste(deparse(fml, width.cutoff = 500L), collapse = " "))
  }))
}


reg_fit_multinom <- function(mdata, outcome, predictors, do_exp, conf_level, method,
                             weighted = FALSE, make_design = NULL, add_terms = NULL,
                             formula = NULL, multiplier = NULL, rec = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Multinomial models have no profile interval; using Wald."))
  }
  y_levels <- levels(mdata[[outcome]])            # reg_fit_frame() dropped the unused ones
  # ⚠ re-home the formula to THIS frame: nnet::multinom and MASS::polr store their call and
  # re-evaluate it, so a formula carrying the user's environment resolves `fml` nowhere.
  fml <- reg_fit_formula(outcome, predictors, add_terms, formula)
  environment(fml) <- environment()

  if (weighted) {
    # refLevel = 1 makes the FIRST level the baseline, matching nnet; VGAM names coefficients
    # "term:k" with k the k-th NON-reference category, so each name is parsed back.
    fit <- svyVGAM::svy_vglm(fml, design = make_design(mdata),
                             family = VGAM::multinomial(refLevel = 1))
    cf  <- if (!is.null(fit$coef)) fit$coef else stats::coef(fit)   # svy_vglm stores $coef / $var
    V   <- if (!is.null(fit$var))  fit$var  else stats::vcov(fit)
    se  <- sqrt(diag(V))
    nm  <- names(cf)
    k   <- suppressWarnings(as.integer(sub("^.*:(\\d+)$", "\\1", nm)))
    trm <- sub(":\\d+$", "", nm)
    if (any(is.na(k)) || max(k) > length(y_levels) - 1L) {
      cli::cli_abort(c("Could not map {.pkg svyVGAM} coefficients to outcome categories.",
                       "i" = "Unexpected coefficient names: {.val {nm[is.na(k)]}}."))
    }
    ylev <- y_levels[-1]                               # non-reference categories, in level order
    td   <- tibble::tibble(y.level = ylev[k], term = gsub("`", "", trm, perl = TRUE),
                           estimate = unname(cf), std.error = unname(se[nm]))
    return(reg_fit_record(tidy_native = reg_tidy_native_z(td), nobs = nrow(mdata),
                          fit = fit, data = mdata, digest = reg_digest(fit, rec),
                          y_ref = y_levels[1], y_levels = y_levels[-1],
                          do_exp = do_exp, conf_level = conf_level, multiplier = multiplier))
  }

  fit <- nnet::multinom(fml, data = mdata, trace = FALSE)
  td  <- reg_tidy_multinom(fit)
  td$term <- gsub("`", "", td$term, perl = TRUE)     # strip formula backticks -> match skeleton
  reg_fit_record(tidy_native = reg_tidy_native_z(td), nobs = nrow(mdata),
                 fit = fit, data = mdata, digest = reg_digest(fit, rec),
                 y_ref = y_levels[1], y_levels = y_levels[-1],
                 do_exp = do_exp, conf_level = conf_level, multiplier = multiplier)
}

# Ordered 3+ level outcome: proportional-odds cumulative logit -- MASS::polr unweighted,
# survey::svyolr weighted. ONE column of cumulative ORs; the cut-point rows are dropped, so
# "Constant" stays NA.
reg_fit_ordinal <- function(mdata, outcome, predictors, do_exp, conf_level, method,
                            weighted = FALSE, make_design = NULL, add_terms = NULL,
                            formula = NULL, multiplier = NULL, rec = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Proportional-odds models have no profile interval; using Wald."))
  }
  # reg_fit_frame() made the outcome an ordered factor and said so once.
  # ⚠ re-home the formula to THIS frame -- see reg_fit_multinom().
  fml <- reg_fit_formula(outcome, predictors, add_terms, formula)
  environment(fml) <- environment()

  if (weighted) {
    # svyolr's coef() also returns the cut-point thresholds, so the SLOPES come off fit$coefficients;
    # its start-value glm.fit step cannot take zero or negative weights.
    fit <- tryCatch(
      survey::svyolr(fml, design = make_design(mdata)),
      error = function(e) cli::cli_abort(c(
        "The survey-weighted ordinal model failed to fit.",
        "x" = conditionMessage(e),
        "i" = "Check for zero or negative weights."
      ))
    )
    cf  <- fit$coefficients
    se  <- sqrt(diag(stats::vcov(fit)))[names(cf)]
    td  <- tibble::tibble(term = gsub("`", "", names(cf), perl = TRUE),
                          estimate = unname(cf), std.error = unname(se))
    cli::cli_inform(c("i" = paste0("The proportional-odds assumption is not tested here: the Brant ",
                                   "test needs an unweighted fit.")))
    return(reg_fit_record(tidy_native = reg_tidy_native_z(td), nobs = nrow(mdata),
                          fit = fit, data = mdata, digest = reg_digest(fit, rec),
                          do_exp = do_exp, conf_level = conf_level, multiplier = multiplier))
  }

  fit <- MASS::polr(fml, data = mdata, Hess = TRUE, method = "logistic")
  td  <- reg_tidy_polr(fit)
  td$term <- gsub("`", "", td$term, perl = TRUE)
  # The Brant test is NOT run here: it is a footer ROW's statistic costing J-1 extra fits, so it is
  # built where that row is -- else every diagnostic and crude polr fit would pay for it.
  reg_fit_record(tidy_native = reg_tidy_native_z(td), nobs = nrow(mdata),
                 fit = fit, data = mdata, digest = reg_digest(fit, rec),
                 do_exp = do_exp, conf_level = conf_level, multiplier = multiplier)
}

# Make a fit SELF-CONTAINED: nnet::multinom / MASS::polr store `data = mdata`, a local of reg_fit(),
# so brant::brant() and stats::drop1() would fail with "object 'mdata' not found".
#' @keywords internal
reg_selfheal_call <- function(fit, data) {
  if (is.null(data) || is.null(fit$call)) return(fit)
  fml <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(fml)) return(fit)
  fit$call$data    <- data
  fit$call$formula <- fml
  fit
}

# The Brant test (`brant`, a Suggests): a missing package skips it with a hint and a failing test is
# swallowed -- a diagnostic must never break a table.
# `asked`: the check is now an ordinal DEFAULT, so "install brant" would greet every ordinal table of
# a user who never asked for it. The hint is for the user who NAMED the check in `stats =`.
# ⚠ brant's own sparsity warning is re-worded rather than passed through: a default check must speak
# about the READING ("take this p-value with care"), not about a contingency table the caller never
# built -- and R would repeat it verbatim on every table.
reg_ordinal_diagnostic <- function(fit, asked = FALSE) {
  if (!requireNamespace("brant", quietly = TRUE)) {
    if (isTRUE(asked)) tx_need_pkg("brant", "The proportional-odds test", severity = "inform")
    return(invisible(NA_real_))
  }
  fit <- reg_selfheal_call(fit, fit$model)
  sparse <- FALSE
  bt <- tryCatch(withCallingHandlers(
    { utils::capture.output(res <- brant::brant(fit)); res },
    warning = function(w) { sparse <<- TRUE; invokeRestart("muffleWarning") }),
    error = function(e) NULL)
  if (sparse)
    cli::cli_inform(c("i" = paste0("Too few cases in some outcome x predictor combinations: read ",
                                   "the proportional-odds p-value with care.")))
  if (is.null(bt) || !is.matrix(bt) ||
      !"Omnibus" %in% rownames(bt) || !"probability" %in% colnames(bt)) {
    return(invisible(NA_real_))                             # unexpected shape -> stay silent
  }
  p <- suppressWarnings(as.numeric(bt["Omnibus", "probability"]))
  if (!is.na(p) && p < 0.05) {
    cli::cli_warn(c(
      "!" = "The proportional-odds (parallel-lines) assumption is rejected (Brant omnibus p = {signif(p, 2)}).",
      "i" = paste0("Cumulative odds ratios may mislead; consider {.code family = \"multinomial\"} or a ",
                   "partial proportional-odds model."),
      "i" = "The Brant test over-rejects at large N; inspect the per-variable tests too."
    ))
  }
  invisible(p)
}

# === SECTION: Survey design construction =========================================================
# A weight column becomes a survey.design PER MODEL on the complete-case frame: ids = ~1 reproduces
# the flat weighted path exactly. A PREBUILT design is never rebuilt -- a calibrated one cannot be --
# only subset()'d to the model's complete cases (domain estimation).

reg_design_vars <- function(design_spec) svy_design_vars(design_spec)

# The model's complete-case frame -- the ONE definition of "the same population as the model". The
# empirical blocks recompute it from raw `data` rather than reading a fit's own frame.
reg_complete_frame <- function(data, vars)
  tidyr::drop_na(data, tidyselect::all_of(intersect(unique(vars), names(data))))

reg_resolve_design <- function(design_spec, mdata, data, drop_vars) {
  if (!is.null(design_spec$design)) {
    keep <- which(stats::complete.cases(data[, drop_vars, drop = FALSE]))
    # ⚠ index the ORIGINAL design, always. Under tab_vars `data` holds one group's rows, so its own
    # positions are group-local, while `.svy_row` is the position in the design the user passed.
    # Without it a CALIBRATED design -- which `[` does not shrink -- weights the wrong respondents.
    rows <- if (!is.null(data[[svy_row_col]])) as.integer(data[[svy_row_col]])[keep] else keep
    svy_domain_design(design_spec$design, rows, mdata)
  } else {
    svy_make_design(mdata, design_spec$wt)
  }
}
# AIC.svyglm / anova.svyglm refit sub-models with an UNQUALIFIED `svyglm()` call, which fails when
# survey is loaded via `::` but not attached: bind it into a child of the formula's environment.
reg_svyglm_env <- function(fit) {
  env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL)
  if (is.null(env)) env <- globalenv()
  if (!exists("svyglm", envir = env, inherits = TRUE)) {
    ne <- new.env(parent = env)
    assign("svyglm", survey::svyglm, envir = ne)
    try(environment(fit$formula) <- ne, silent = TRUE)
    try(environment(fit$terms)   <- ne, silent = TRUE)
  }
  fit
}
# === SECTION: reg_fit_frame() -- the model frame, and the one prep that builds it =================
# THE POPULATION AND THE CODING A FIT SEES, as a pure function of the data plus a few strings -- so
# reg_digest_frame() (R/reg-digest.R) rebuilds exactly what was fitted and no frame is ever cached.
# Three siblings cannot go through the `formula =` escape hatch, because they must inherit the binary
# prep, the grouped-binomial cbind, the "rr" route and the design resolution: `add_terms` adds RHS
# terms naming no new variable; `cross` is a tab_vars, making the POOLED interaction fit; and
# `drop_extra` joins drop_vars but NOT the formula -- variables the fit must be COMPLETE ON without
# modelling, which is how a crude univariable fit lands on exactly the model's population.
# ⚠ a pre-filtered frame passed as `data` is NOT equivalent: a PREBUILT design's keep mask is
# computed from `data` itself, and a shorter one recycles silently.
# ⚠ `positive_level` is a RETURN VALUE, not the attribute reg_prep_binary() leaves behind: any
# dplyr verb downstream would drop it.
# A RATIO OF MEANS NEEDS A NON-NEGATIVE OUTCOME, refused ONCE. ⚠ raised at the argument boundary,
# not only in the fitter: since the observed companion is on by default the crude block runs first,
# and it would take the log of a negative mean and warn "NaNs produced" before the honest abort.
#' @keywords internal
#' @noRd
reg_check_ratio_outcome <- function(y, outcome) {
  y <- suppressWarnings(as.numeric(y))
  if (any(is.finite(y) & y < 0)) cli::cli_abort(c(
    '{.code measure = "ratio"} needs a non-negative outcome: a ratio of means is not defined when {.val {outcome}} can be negative.',
    "i" = 'Model {.code log()} of a positive outcome instead, or use {.code measure = "difference"}.'),
    call = NULL)
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
reg_fit_frame <- function(data, outcome, predictors, family, design_spec,
                          outcome_level = NULL, trials = NULL, formula = NULL, cross = NULL,
                          drop_extra = NULL, add_terms = NULL, quiet = FALSE) {
  # `add_terms` may name a variable the main effects do not -- a crossed slope's modified predictor
  # -- and the model's population is what the formula uses, not what `predictors` lists.
  add_vars  <- if (length(add_terms))
    all.vars(stats::as.formula(paste("~", paste(add_terms, collapse = " + ")))) else NULL
  drop_vars <- unique(c(outcome, predictors, cross, add_vars, drop_extra,
                        reg_design_vars(design_spec)))
  mdata     <- reg_complete_frame(data, drop_vars)

  fac_preds <- reg_factor_preds(mdata, c(predictors, cross))
  if (length(fac_preds) > 0L) {
    # An ORDERED predictor makes glm / polr use polynomial contrasts, which no per-level skeleton can
    # align -- an all-NA column. Only PREDICTORS; an ordinal DEPENDENT keeps its order.
    mdata <- dplyr::mutate(mdata, dplyr::across(
      tidyselect::all_of(fac_preds),
      ~ { f <- forcats::fct_drop(as.factor(.)); factor(f, levels = levels(f), ordered = FALSE) }
    ))
  }

  grouped        <- reg_is_grouped_binomial(family, trials, !is.null(formula))
  positive_level <- NULL
  binary_prep    <- function(d) {
    d <- reg_prep_binary(d, outcome, outcome_level)
    positive_level <<- attr(d, "positive_level")
    attr(d, "positive_level") <- NULL
    d
  }

  if (grouped) {
    s <- mdata[[outcome]]
    if (!is.numeric(s) || any(s %% 1 != 0, na.rm = TRUE)) {
      cli::cli_abort(c("A summed-score outcome ({.arg trials}) must be integer-valued.",
                       "x" = "{.val {outcome}} is {.cls {class(s)}}."))
    }
    if (any(s < 0 | s > trials, na.rm = TRUE)) {
      cli::cli_abort(c("{.val {outcome}} scores must lie in {.val {0}}..{.val {trials}} (= {.arg trials}).",
                       "x" = "Observed range: {.val {range(s, na.rm = TRUE)}}."))
    }
    mdata[[".gb_succ"]] <- s
    mdata[[".gb_fail"]] <- trials - s
    # for the links that cannot take a two-column response: `.gb_trials` is the modified Poisson's
    # offset (so exp(coef) stays a PER-ITEM ratio), `.gb_prop` the risk the identity link is fitted
    # on.
    mdata[[".gb_trials"]] <- trials
    mdata[[".gb_prop"]]   <- s / trials
  }

  switch(
    family,
    "multinomial" = { mdata[[outcome]] <- forcats::fct_drop(as.factor(mdata[[outcome]])) },
    "ordinal" = {
      y <- mdata[[outcome]]
      if (!is.ordered(y)) {
        y <- as.ordered(forcats::fct_drop(as.factor(y)))
        lv_str <- paste(levels(y), collapse = " < ")
        if (!quiet) tx_inform_once(paste0("ordered_", outcome),
                                   c("i" = "{.val {outcome}}: read as ordered ({lv_str})."))
      } else {
        y <- forcats::fct_drop(y)
      }
      mdata[[outcome]] <- y
    },
    "binomial" = if (is.null(trials) && is.null(formula)) mdata <- binary_prep(mdata),
    # modified Poisson on a binary outcome (Zou 2004): the logistic arm's binary prep, then the 0/1
    # NUMERIC a log-link Poisson needs. The identity-link risk difference takes the same route.
    "rr" = , "rd" = if (!grouped) {
      mdata <- binary_prep(mdata)
      mdata[[outcome]] <- as.numeric(mdata[[outcome]] == positive_level)
    },
    "mr" = reg_check_ratio_outcome(mdata[[outcome]], outcome),
    "poisson" = , "quasipoisson" = , "gaussian" = NULL,
    cli::cli_abort("Unsupported {.arg family}: {.val {family}}.")
  )
  if (is.null(formula) && !grouped && !reg_fam_binary(family) &&
      !family %in% c("multinomial", "ordinal") && !is.numeric(mdata[[outcome]])) {
    cli::cli_abort(c(
      "A {.val {family}} outcome must be numeric.",
      "x" = "{.val {outcome}} is {.cls {class(mdata[[outcome]])}}."
    ))
  }
  y_levels <- if (is.factor(mdata[[outcome]])) levels(mdata[[outcome]]) else NULL
  list(frame = mdata, positive_level = positive_level, grouped = grouped,
       drop_vars = drop_vars, y_levels = y_levels)
}

# === SECTION: reg_fit() -- one model, one tidy ===================================================
# Fit ONE model on complete cases -> a tidy of the effect measure + CI + p + n. `do_exp` chooses the
# estimate scale: exp(coef) multiplicative, raw coef additive.
reg_fit <- function(data, outcome, predictors, family, design_spec, do_exp,
                    outcome_level, conf_level, method,
                    trials = NULL, formula = NULL, multiplier = NULL, cross = NULL,
                    drop_extra = NULL, add_terms = NULL) {
  prep      <- reg_fit_frame(data, outcome, predictors, family, design_spec,
                             outcome_level = outcome_level, trials = trials, formula = formula,
                             cross = cross, drop_extra = drop_extra, add_terms = add_terms)
  mdata     <- prep$frame
  drop_vars <- prep$drop_vars
  grouped   <- prep$grouped
  positive_level <- prep$positive_level
  # THE REFIT RECIPE, assembled once and carried by the digest: a few strings that rebuild the frame
  # (reg_digest_frame) and, where a digest cannot answer, the fit itself (reg_digest_revive).
  rec <- new_reg_recipe(outcome = outcome, predictors = predictors, family = family,
                        outcome_level = outcome_level, trials = trials, formula = formula,
                        cross = cross, drop_extra = drop_extra, add_terms = add_terms,
                        design_spec = design_spec, conf_level = conf_level, method = method,
                        multiplier = multiplier, y_levels = prep$y_levels,
                        positive_level = positive_level, grouped = grouped, drop_vars = drop_vars)

  weighted <- svy_weighted(design_spec, design_spec$wt)
  make_design <- function(recoded_mdata) reg_resolve_design(design_spec, recoded_mdata, data, drop_vars)

  if (family == "multinomial") {
    return(reg_fit_multinom(mdata, outcome, predictors, do_exp, conf_level, method,
                            weighted, make_design, add_terms = add_terms, formula = formula,
                            multiplier = multiplier, rec = rec))
  }
  if (family == "ordinal") {
    return(reg_fit_ordinal(mdata, outcome, predictors, do_exp, conf_level, method,
                           weighted, make_design, add_terms = add_terms, formula = formula,
                           multiplier = multiplier, rec = rec))
  }

  fam_obj <- switch(
    family,
    "binomial"     = if (weighted) stats::quasibinomial("logit") else stats::binomial("logit"),
    "poisson"      = if (weighted) stats::quasipoisson("log") else stats::poisson("log"),
    "quasipoisson" = stats::quasipoisson("log"),
    # quasipoisson in BOTH bases -- the "rr" fit goes through svyglm either way, and AIC / BIC then
    # return NA, the honest answer for a quasi-likelihood.
    "rr"           = stats::quasipoisson("log"),
    "rd"           = stats::binomial("identity"),
    # the RATIO OF MEANS: Poisson pseudo-maximum-likelihood with robust SEs -- the log link is the
    # point, not a claim about counts.
    "mr"           = stats::quasipoisson("log"),
    "gaussian"     = stats::gaussian()
  )

  # ONE assembly for every fitter (reg_fit_formula), so reg_formulas() reports what really ran.
  # A Poisson likelihood has no two-column response: the grouped modified Poisson models the success
  # count with log(trials) as OFFSET, which keeps exp(coef) a per-item risk ratio.
  resp <- if (grouped) reg_grouped_response(family) else NULL
  fml  <- reg_fit_formula(outcome, predictors, add_terms, formula, response = resp, cross = cross,
                          offset = if (grouped && identical(family, "rr"))
                            "offset(log(`.gb_trials`))")
  # the identity-link fallback fits the RISK itself, and never carries the count's offset
  fml_lpm <- if (!is.null(formula)) fml else
    reg_fit_formula(outcome, predictors, add_terms, NULL,
                    response = if (grouped) "`.gb_prop`", cross = cross)

  # ⚠ "rr" ALWAYS fits through svyglm, weighted or not: a Poisson likelihood on a 0/1 outcome is
  # deliberately misspecified, so its naive SEs must become the Huber-White SANDWICH -- which
  # svyglm's design-based variance IS, so the digest stores a vcov already sandwiched and
  # a distilled digest needs no special case. `weighted` stays FALSE here: it means "the USER
  # gave a design".
  use_svy <- reg_fam_svy_fitted(family, weighted)
  fit <- if (family == "gaussian" && !weighted) {
    stats::lm(fml, data = mdata)
  } else if (!use_svy) {
    stats::glm(fml, data = mdata, family = fam_obj)
  } else if (family == "rd") {
    # The identity link needs starting values and can still fail: start from the OLS fit, and on
    # failure BE it. WARNING: that fallback TARGETS the same risk difference but is a different
    # ESTIMATOR -- the two coincide only where the model holds, so the message must name which ran.
    des0  <- make_design(mdata)
    start <- tryCatch(stats::coef(stats::lm(fml_lpm, data = mdata)), error = function(e) NULL)
    fit   <- tryCatch(
      do.call(survey::svyglm, list(fml, design = des0, family = fam_obj, start = start)),
      error = function(e) NULL, warning = function(w) NULL)
    if (is.null(fit) || !isTRUE(fit$converged)) {
      cli::cli_inform(c("!" = paste0(
        "{.val {outcome}}: the risk-difference model did not converge; fitting a linear probability ",
        "model, which targets the same difference with a different estimator.")))
      fit <- do.call(survey::svyglm, list(fml_lpm, design = des0, family = stats::gaussian()))
    }
    fit
  } else {
    # WARNING: svyglm is called through do.call() with the family OBJECT spliced in -- some of its
    # methods re-evaluate their own call in the design's data enclosure, where `fam_obj` does not
    # exist.
    do.call(survey::svyglm, list(fml, design = make_design(mdata), family = fam_obj))
  }
  if (inherits(fit, "svyglm")) fit <- reg_svyglm_env(fit)

  td <- reg_tidy_coefmat(fit)                       # native scale: estimate, std.error, p.value
  td$term <- gsub("`", "", td$term, perl = TRUE)  # strip formula backticks -> match skeleton

  # `multiplier` is NOT applied here: it is a reporting choice, so it belongs to
  # reg_tidy_finalize() beside the interval and the exponentiation -- which is what keeps this tidy
  # NATIVE and keeps the scaling out of the fit-cache key.

  # An unweighted Poisson / grouped-binomial MLE reports naive SEs: scale them by sqrt(phi) so the
  # CI and stars match a quasi fit, while the MLE keeps its likelihood for the AIC / LR footer.
  # Bernoulli dispersion is not identifiable and gaussian has none, so both stay untouched.
  over_disp <- !weighted && reg_fam_overdispersed(family, grouped)
  phi       <- if (over_disp) reg_dispersion(fit) else NA_real_
  scaled    <- over_disp && !is.na(phi) && phi > 0
  if (scaled) {
    td$std.error <- td$std.error * sqrt(phi)
    if (phi > 1.5) cli::cli_warn(c(
      "!" = "Over-dispersion (dispersion = {signif(phi, 3)}): standard errors are scaled by its square root.",
      "i" = 'Use {.code family = "quasipoisson"} for the fully quasi fit.'
    ))
  }

  # "rr" is excluded by construction, but say so rather than downgrade silently.
  use_profile <- method == "profile" && !weighted && reg_fam_disp_known(family)
  if (method == "profile" && weighted) {
    cli::cli_inform(c("!" = "Survey-weighted models have no profile interval; using Wald."))
  } else if (method == "profile" && family == "rr") {
    cli::cli_inform(c("!" = paste0("A modified-Poisson fit is a quasi-likelihood and has no profile ",
                                   "interval; using the robust Wald one.")))
  }

  # THE fit's own reference distribution, decided ONCE and carried out with the fit: z where the
  # family FIXES the dispersion, else t on df.residual -- an ESTIMATED dispersion (lm, quasi*,
  # weighted, or a phi-scaled fit) moves the reference off z. Everything this fit goes on to produce
  # -- the marginal sweep, the baseline row, a crude column refit from it -- READS it back instead of
  # assuming z, which is what keeps one table on one reference distribution.
  disp_known <- !weighted && reg_fam_disp_known(family) && !scaled
  df_res     <- reg_df_residual(fit)

  # THE NATIVE TIDY: estimate and std.error on the model's own scale, `p.value` already the fit's
  # own (both are functions of the FIT alone). The interval and the exponentiation belong to the
  # ESTIMAND and are written by reg_tidy_finalize(), which is what lets `measure` / `effect` /
  # `conf_level` change without refitting.
  if (use_profile) {
    ci   <- suppressMessages(stats::confint(fit, level = conf_level))   # log/native scale
    idx  <- match(td$term, gsub("`", "", rownames(ci), perl = TRUE))
    # ⚠ the profile bounds are an OUTPUT of the likelihood at THIS conf_level, so they are the one
    # thing that cannot be rebuilt from (estimate, std.error) -- hence `method = "profile"` is not
    # cacheable, and its bounds ride NATIVELY on the record. reg_tidy_rescale() scales them at
    # finalize, with the estimate they belong to.
    td$conf.low  <- unname(ci[idx, 1])
    td$conf.high <- unname(ci[idx, 2])
    lrp  <- reg_lr_pvalues(fit)
    td$p.value <- unname(lrp[match(td$term, names(lrp))])
  } else if (scaled) {
    # with the SE scaled and the t reference, p is recomputed from est / se so p, CI and stars stay
    # duals (summary()'s own p belongs to the un-scaled model).
    td$p.value <- 2 * stats::pt(-abs(td$estimate / td$std.error), df = df_res)
  }

  # var(Y) is the standardised ladder's divisor. A summed score needs it too: its additive effect is
  # a difference of mean SCORES, graded against the score's own spread. ⚠ computed unconditionally:
  # it is a fact about the DATA, and gating it on `do_exp` would put the estimand back on the record.
  var_y <- if (family == "gaussian" || !is.na(trials %||% NA))
    stats::var(as.numeric(mdata[[outcome]])) else NA_real_

  reg_fit_record(tidy_native = td, nobs = nrow(mdata), var_y = var_y,
                 positive_level = positive_level, fit = fit, data = mdata,
                 digest = reg_digest(fit, rec), profile = use_profile,
                 disp_known = disp_known, df_residual = df_res,
                 do_exp = do_exp, conf_level = conf_level, multiplier = multiplier)
}


# THE FIT RECORD -- what one fit contributes, and the one object the jamovi cache stores (minus its
# `fit` and `data`, which reg_fit_distil() strips). The FORMALS ARE THE CONTRACT, as in
# new_reg_ctx() / new_reg_spec_product().
# ⚠ `tidy` is DERIVED, never stored by a cache: it is the only estimand-dependent member, and
# reg_tidy_finalize() rewrites it per (do_exp, conf_level) from `tidy_native`.
#' @keywords internal
#' @noRd
reg_fit_record <- function(tidy_native = NULL, nobs = NA_integer_, var_y = NA_real_,
                           positive_level = NULL, fit = NULL, data = NULL, digest = NULL,
                           profile = FALSE, disp_known = TRUE, df_residual = NA_real_,
                           y_ref = NULL, y_levels = NULL, glance = NULL,
                           do_exp = FALSE, conf_level = 0.95, multiplier = NULL) {
  f <- list(tidy = NULL, tidy_native = tidy_native, nobs = nobs, var_y = var_y,
            positive_level = positive_level, fit = fit, data = data, digest = digest,
            profile = profile, disp_known = disp_known, df_residual = df_residual,
            y_ref = y_ref, y_levels = y_levels, glance = glance)
  f$tidy <- reg_tidy_finalize(f, do_exp, conf_level, multiplier)
  f
}

# The tidy a COLUMN prints: the native estimate wearing this estimand's interval, exponentiation,
# reference distribution AND its `multiplier` scaling. One writer for every family and both interval
# methods -- and the one place every REPORTING choice is applied, which is what lets each of them
# change without refitting (reg_fit_cached serves the record; only these four arguments move).
#' @keywords internal
#' @noRd
reg_tidy_finalize <- function(f, do_exp, conf_level, multiplier = NULL) {
  td <- f$tidy_native
  if (is.null(td)) return(NULL)
  td  <- reg_tidy_rescale(td, multiplier)      # a k-unit change, before the interval is built
  res <- if (isTRUE(f$profile))
    reg_wald_finalize(td$estimate, do_exp, lo = td$conf.low, hi = td$conf.high, p = td$p.value)
  else
    reg_wald_finalize(td$estimate, do_exp, se = td$std.error,
                      crit = reg_wald_crit(f$disp_known, f$df_residual, conf_level),
                      p = td$p.value)
  td$estimate  <- res$estimate;  td$conf.low <- res$conf.low
  td$conf.high <- res$conf.high; td$p.value  <- res$p.value
  td
}

# === SECTION: The column builders ================================================================
# Align one fit to the union skeleton -> ONE fmt column: a reference LEVEL of a predictor present in
# this model takes the neutral value, a predictor ABSENT from it stays NA.
reg_column <- function(skeleton, fit_res, model_predictors, col_var, est,
                       color, color_signif, model_family = "", method = "wald", trials = NA) {
  effect_shape <- if (isTRUE(est$exp)) "ratio" else "additive"
  # The column's SHAPE is the estimand row's: the fmt field, the EST_SCALES key, the token, the
  # digits. No builder names a family-specific field.
  scale_key    <- reg_scale_of(est, trials)
  est_field    <- EST_SCALES[[scale_key]]$est_field
  base_field   <- EST_SCALES[[scale_key]]$base_display %||% NA_character_
  digits       <- reg_cell_digits(scale_key)
  td  <- fit_res$tidy
  m   <- match(skeleton$term, td$term)
  est_v <- td$estimate[m]
  lo  <- td$conf.low[m]
  hi  <- td$conf.high[m]
  p   <- td$p.value[m]

  in_model <- skeleton$var %in% c("Constant", model_predictors)
  # DESIGN: the Constant is a reference ROW but not a reference LEVEL, so it keeps the intercept
  # instead of the neutral: a baseline odds / mean IS a value. ⚠ its profile is the fitted equation's
  # -- factors at their reference, numerics at ZERO -- not the reference profile `at_reference` uses
  # (numerics at their mean).
  ref_lvl  <- skeleton$is_ref & skeleton$var != "Constant" & in_model
  neutral  <- if (effect_shape == "ratio") 1 else 0
  est_v[ref_lvl] <- neutral
  lo[ref_lvl]  <- NA_real_
  hi[ref_lvl]  <- NA_real_
  p[ref_lvl]   <- NA_real_

  n_rows   <- nrow(skeleton)
  # in_refrow is a UNION-skeleton row fact, NOT gated by in_model: a model that OMITS a predictor
  # must not blank its reference flag, or the shared cross-column bold loses it in a comparison.
  refrows  <- (skeleton$is_ref & skeleton$var != "Constant") | skeleton$var == "Constant"

  # ⚠ a SUMMED SCORE's additive effect is a difference of mean SCORES: the fit reports a per-item
  # probability difference, and E[score] = trials x p makes the conversion exact.
  if (identical(scale_key, "raw_diff") && !is.na(trials %||% NA)) {
    k <- as.numeric(trials); est_v <- est_v * k; lo <- lo * k; hi <- hi * k
  }
  # the baseline row leaves the estimate field wherever its scale says the effects act on the level
  cp     <- reg_constant_place(scale_key, trials, skeleton$var == "Constant",
                               est_v, NULL, p, rep("est", n_rows))
  est_v  <- cp$est; p <- cp$p; disp <- cp$display
  fields <- stats::setNames(list(est_v), est_field)
  if (!is.null(cp$base) && !is.na(base_field) && !identical(base_field, est_field))
    fields[[base_field]] <- cp$base
  args <- c(
    # NA here, overwritten in reg_spec_build_one() with each level's own count: the builders do
    # not know the model's complete-case frame, and the count is the same for every column of a fit.
    list(n = rep(NA_integer_, n_rows)),
    fields,
    list(ci_inf = lo, ci_sup = hi, pvalue = p,
         scale = scale_key, display = disp, digits = digits,
         ci_method = reg_wald_method_name(method, identical(effect_shape, "ratio")),
         degf = reg_wald_degf(method, fit_res$disp_known, fit_res$df_residual),
         color = color, color_signif = color_signif, col_var = col_var,
         comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"))
  if (identical(effect_shape, "ratio")) args <- c(args, list(ref = "1"))
  args <- c(args, list(pct_type = reg_pct_type(scale_key)))
  if (identical(EST_SCALES[[scale_key]]$sd_from %||% "", "var"))
    args <- c(args, list(var = rep(fit_res$var_y, n_rows)))
  do.call(fmt, args)
}

# `display` on a regression table IS tab()'s (R/tab-display.R). THE RULE its templates obey: a
# template may ask for an AUXILIARY quantity of the SAME fit, never for a different fit or estimand.
# ⚠ the boundary VALIDATES and normalises, but does not collapse a preset to one template: a preset
# may declare one arm per column ROLE, and only reg_apply_display() knows which column it is writing.
#' @keywords internal
#' @noRd
reg_resolve_display <- function(display) {
  if (is.null(display_resolve(display))) return(NULL)
  as.character(display)[[1]]
}

# WHY THE ADJUSTED PREDICTION AND EVERY GEOMETRY OF THE COMPARISON ARE ALWAYS STORED: `display` is a
# post-hoc property -- choosing what a cell shows may never trigger a computation nor change a
# number, or set_display() on a built table would be a lie and jamovi's repaint would need a refit.
# All of it comes from ONE point-estimate g-computation sweep. The prediction lands in the field the
# column's scale names for a LEVEL (`EST_SCALES$base_display`, what `{base}` renders), the additive
# and multiplicative readings of the same comparison in `diff` and `ratio` (reg_fill_geometries()).
# WARNING: none of them may write into the column's OWN estimate field.
# `add` supplies the additive fallback where the column's own sweep reports another contrast (a
# ratio); the PREDICTIONS always come from `marg`, the sweep at this column's own profile.
#' @keywords internal
#' @noRd
reg_fill_base <- function(col, marg, skeleton, model_predictors, group = NULL, add = NULL,
                          crosses = NULL) {
  if (is.null(marg)) return(col)
  add <- add %||% marg
  n_rows   <- nrow(skeleton)
  est_fld  <- fmt_center_field(col)
  base_fld <- fmt_scale_row(col)$base_display %||% NA_character_
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  # WARNING: where the sweep returns one value per outcome CATEGORY and the column belongs to none --
  # an ordinal cumulative odds ratio, spanning every cut -- the fill is refused rather than guessed.
  slice <- function(d) {
    if (!"group" %in% names(d)) return(d)
    if (all(is.na(d$group)))    return(d)
    if (!is.null(group))  return(d[!is.na(d$group) & d$group == group, , drop = FALSE])
    if (length(unique(stats::na.omit(d$group))) > 1L) return(d[0, , drop = FALSE])
    d
  }
  take  <- function(d, col_nm) {
    d <- slice(d)
    if (is.null(d) || !nrow(d)) return(rep(NA_real_, n_rows))
    d[[col_nm]][reg_skel_match(skeleton, d)]
  }
  pred_v <- if (is.null(marg$pred)) rep(NA_real_, n_rows) else take(marg$pred, "pred")
  # ROWS THE SWEEP MUST NOT WRITE A LEVEL INTO, so they keep whatever is already stored: the
  # Constant, whose baseline reg_constant_place() has placed on the scale's own terms; and a NESTED
  # cross block's slopes, which have no level pair -- exactly like a plain numeric predictor's row,
  # and the moderator's own block already shows each group's adjusted level.
  no_lvl <- as.character(skeleton$var) %in% c("Constant", reg_cross_nested_vars(crosses))
  if (!is.na(base_fld) && !identical(base_fld, est_fld)) {
    pred_v[no_lvl] <- as.double(vctrs::field(col, base_fld))[no_lvl]
    col <- vctrs::`field<-`(col, base_fld, pred_v)
  }
  # DESIGN: a factor level's ADDITIVE effect is derived from the two adjusted predictions rather than
  # from the sweep's own contrast. The two are the same number (averaging commutes with an additive
  # contrast), but the derived form is reference-INVARIANT, which is what lets jamovi's digest
  # re-reference a cached fit without refitting. A numeric slope has no level pair: it comes from the
  # sweep, and has no multiplicative reading at all.
  refi   <- which(skeleton$is_ref & in_model)
  ref_of <- pred_v[refi][match(as.character(skeleton$var), as.character(skeleton$var)[refi])]
  ame_v  <- if (is.null(add$ame)) rep(NA_real_, n_rows) else take(add$ame, "ame")
  col    <- reg_fill_geometries(col, pred_v, ref_of, fallback_diff = ame_v)
  # DESIGN: where the sweep carries the OTHER geometry itself, it wins over the derivation above. A
  # rank pair's two readings are both primitives of (win, loss) -- neither follows from the level and
  # its reference, which here are the probability of superiority and a coin flip.
  alt_v <- if (is.null(add$ame) || !"alt" %in% names(add$ame)) rep(NA_real_, n_rows)
           else take(add$ame, "alt")
  if (any(!is.na(alt_v))) {
    fld <- if (identical(est_fld, "diff")) "ratio" else "diff"
    v   <- as.double(vctrs::field(col, fld))
    v[!is.na(alt_v)] <- alt_v[!is.na(alt_v)]
    col <- vctrs::`field<-`(col, fld, v)
  }
  col
}

# THE THREE GEOMETRIES OF ONE COMPARISON, from ONE pair of levels -- the adjusted predictions on a
# model column, the observed levels on a crude one. A level and its reference define an additive
# reading (`diff`) and a multiplicative one (`ratio`) of the very same comparison, so both are stored
# on both columns and `display` can show either without computing anything.
# WARNING: never into the column's OWN estimate field -- that one holds what was FITTED (or closed-
# formed), which on a non-collapsible scale is not the contrast of the two levels. `or` is NOT
# derived here for the same reason: an odds ratio built from two adjusted predictions is MARGINAL,
# a different estimand from the conditional one it would sit beside, so `or` stays the fit's own.
#' @keywords internal
#' @noRd
reg_geometry_fields <- function(est_fld, lvl, ref_lvl) {
  # a ratio needs two POSITIVE levels: a mean straddling zero has no multiplicative reading, and a
  # zero baseline none at all -- the colour ladder is logarithmic, so 0 / Inf is not a value there.
  r <- lvl / ref_lvl
  r[!is.na(r) & (lvl <= 0 | ref_lvl <= 0)] <- NA_real_
  c(if (!identical(est_fld, "diff"))  list(diff  = lvl - ref_lvl),
    if (!identical(est_fld, "ratio")) list(ratio = r))
}

# The same rule applied to a BUILT column (the model side): the crude side composes the fields
# instead, at reg-empirical.R's with_base().
#' @keywords internal
#' @noRd
reg_fill_geometries <- function(col, lvl, ref_lvl, fallback_diff = NULL) {
  f <- reg_geometry_fields(fmt_center_field(col), lvl, ref_lvl)
  if (!is.null(f$diff) && !is.null(fallback_diff))
    f$diff <- ifelse(is.na(f$diff), fallback_diff, f$diff)
  for (nm in names(f)) col <- vctrs::`field<-`(col, nm, f[[nm]])
  col
}

# WARNING: this calls the ANALYTIC engine directly, never reg_marginal(). These quantities are
# AUXILIARY -- what a cell MAY show -- so they are computed where they are free and absent where they
# are not. reg_marginal() would fall back to `marginaleffects` when g-computation refuses, turning an
# optional annotation into a hard dependency and, worse, an abort.
#' @keywords internal
#' @noRd
reg_fill_sweep <- function(fit, data, predictors, conf_level, wt = NULL, multiplier = NULL,
                           crosses = list())
  tryCatch(reg_marginal_gcomp(fit, data, predictors, conf_level, wt, want_pred = TRUE,
                              want_se = FALSE, multiplier = multiplier, crosses = crosses),
           error = function(e) NULL)

# A pure template writer: every field it can name is already stored, and the per-cell rule is the
# crosstab's own -- a cell takes the template wherever its PRIMARY field exists, a void aside being
# padded rather than dropped. The template is resolved HERE, per column, because a preset's arm
# depends on the column's `role`.
#' @keywords internal
#' @noRd
reg_apply_display <- function(col, display) {
  if (is.null(display)) return(col)
  tmpl <- display_resolve(display, get_role(col))
  if (is.null(tmpl)) return(col)
  display_write_col(col, tmpl)$col
}

# The display every cell of the table takes: the user's, else the default LAYOUT the `empirical` mode
# implies. `est_base`'s `emp` arm mirrors it -- "({base}) {est}" against "{est} ({base})" -- so the
# two ESTIMATES end up adjacent, each with its level on the outside, the order of the modelling
# itself. `"cell"` has no pair to mirror and takes `est_obs`, which puts the crude effect where every
# other observed-then-modelled layout puts it: before the estimate.
# DESIGN: the in-cell fold is a PRESET, not a per-cell rewrite -- so the layout can report its own
# aside (reg_meta_aside), and one boundary still decides what every cell shows.
# DESIGN: COMPARING PREDICTOR SUBSETS, the level is stated ONCE -- there is one observed column for
# the whole set, and it carries it (`est_base_once`). Repeating it in every model column puts a
# bracket between the very numbers the spelling exists to set side by side.
#' @keywords internal
#' @noRd
reg_display_of <- function(display, empirical, comparison = FALSE) {
  if (!is.null(display)) return(display)
  if (!emp_on(empirical)) return(NULL)
  if (identical(empirical, "cell")) return("est_obs")
  if (isTRUE(comparison)) "est_base_once" else "est_base"
}

# === SECTION: Marginal effects and adjusted predictions (the `at` profile axis) ==================

# THE reference profile, and the one producer of it: every predictor at its declared reference -- a
# factor at its first level (the model's treatment-contrast baseline), a continuous predictor at its
# ANCHOR, which the boundary already shifted to 0. The fallback is the WEIGHTED mean, and it is
# reached only where nothing was anchored (a compound formula).
reg_reference_grid_values <- function(data, predictors, anchors = NULL, w = NULL) {
  vals <- lapply(predictors, function(v) {
    x <- data[[v]]
    if (reg_is_factor_var(x))       levels(as.factor(x))[1]
    else if (v %in% names(anchors)) 0
    else                            wtd_mean(x, w)
  })
  stats::setNames(vals, predictors)
}

# ⚠ THE one place a marginal effect can be silently WRONG: `marginaleffects` re-evaluates a poly() /
# ns() / bs() basis on perturbed data, and an orthogonal basis absorbs a location shift exactly, so
# it returns AME = 0 with no warning. Whether it happens depends on whether the data can be
# recovered, so the answer is to CHECK, not to refuse: the comparator is stats::predict(newdata =),
# which carries the basis's frozen `predvars`. `shape = "quadratic"` is correct through every route.
#' @keywords internal
reg_basis_vars <- function(fit, predictors) {
  lab <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
  hit <- grepl("\\b(poly|ns|bs|rcs)\\s*\\(", lab)
  if (!any(hit)) return(character(0))
  predictors[vapply(predictors, function(v)
    any(grepl(paste0("\\b", tolower(v), "\\b"), tolower(lab[hit]))), logical(1))]
}

#' @keywords internal
reg_marginal_basis_ok <- function(fit, data, v, k, est, ratio, do_exp = ratio) {
  truth <- tryCatch({
    p0 <- stats::predict(fit, newdata = data, type = "response")
    d2 <- data; d2[[v]] <- as.numeric(d2[[v]]) + (if (is.finite(k) && k != 0) k else 1)
    mean(as.numeric(stats::predict(fit, newdata = d2, type = "response")) - as.numeric(p0),
         na.rm = TRUE)
  }, error = function(e) NA_real_)
  if (!is.finite(truth) || abs(truth) < 1e-10) return(TRUE)          # nothing to disagree about
  # a ratio has no `truth` to compare against, so the tell is "did it come back NEUTRAL?" -- and the
  # neutral is the one the column PRINTS: 1 exponentiated, 0 on a kept log.
  if (isTRUE(ratio))
    return(!isTRUE(all.equal(unname(est[[1]]), if (isTRUE(do_exp)) 1 else 0, tolerance = 1e-8)))
  isTRUE(abs(unname(est[[1]]) - truth) <= 0.02 * abs(truth) + 1e-10)
}

# THE dispatcher between the two engines: the fast route returns NULL rather than a wrong number, and
# the fallback then runs for the WHOLE call, so one column carries one convention.
#
# ⚠ TWO DECISIONS, NEVER ONE FLAG. `comparison` says what the ENGINE computes -- "lnratioavg" /
# "lnor" make it work on the log of a ratio, which is where the interval is Wald. `exponentiate` says
# what the COLUMN prints, and it comes from the estimand's own `exp`: a `measure = "log_*"` column
# keeps the log, and folding the two together printed ratios on a column stamped `log_coef`.
reg_marginal <- function(fit, data, predictors, conf_level, wt = NULL,
                         at = "average", link = "identity", comparison = NULL, want_pred = TRUE,
                         exponentiate = TRUE,
                         multiplier = NULL, engine = "marginaleffects", want_se = TRUE,
                         anchors = NULL, crosses = list(), rank = FALSE,
                         disp_known = TRUE, df_residual = NA_real_, refit = NULL) {
  # `link` is the REPORTED comparison's, and it is what decides both questions: the contrast the
  # sweep computes, and whether the result comes back on a log scale that `exponentiate` may undo.
  # `comparison` is the marginaleffects spelling of that same contrast, so it FOLLOWS the link unless
  # the caller names another -- which only the multinomial vs-rest arm ("lnor") does.
  if (is.null(comparison) || is.na(comparison))
    comparison <- unname(REG_MARGINAL_COMPARISON[[link]])
  log_ratio <- !identical(link, "identity")
  do_exp    <- log_ratio && isTRUE(exponentiate)
  out <- NULL
  # "lnor" is the MNL j-vs-rest contrast, which only ever comes with at = "reference".
  if (identical(engine, "gcomp") && identical(at, "average") && !identical(comparison, "lnor"))
    out <- reg_marginal_gcomp(fit, data, predictors, conf_level, wt, link = link,
                              do_exp = do_exp,
                              want_pred = want_pred, want_se = want_se, multiplier = multiplier,
                              crosses = crosses, rank = rank,
                              disp_known = disp_known, df_residual = df_residual)
  # THE fallback, and the only place `marginaleffects` is genuinely required: the estimand's engine
  # named it, or gcomp refused this fit -- which the argument boundary cannot know.
  if (is.null(out)) {
    # WARNING: a superiority pair has NO marginaleffects contrast -- falling through would silently
    # print a per-category average marginal effect under a Somers' D header. The one fit that gets
    # here is survey::svyolr(), which reg_prob_engine() refuses.
    if (isTRUE(rank)) cli::cli_abort(c(
      "A weighted ordinal model cannot be read as a probability of superiority.",
      "i" = 'Use {.code effect = "conditional"} (the cumulative odds ratio), or drop the weights.'),
      call = NULL)
    if (!requireNamespace("marginaleffects", quietly = TRUE))
      reg_abort_marginaleffects("this contrast, which has no closed form on this model")
    # ⚠ marginaleffects works on a FITTED object and knows nothing of a digest, so this is where a
    # distilled record buys its fit back (R/reg-digest.R). The refusal below is the honest one: no
    # fit, no fallback engine, rather than a wrong number.
    if (is_reg_digest(fit)) {
      fit <- if (is.null(refit)) NULL else refit()
      if (is.null(fit)) cli::cli_abort(c(
        "This contrast needs the fitted model, which could not be rebuilt.",
        "i" = "It has no closed form here, so {.pkg marginaleffects} must read the fit itself."),
        call = NULL)
    }
    out <- reg_marginal_me(fit, data, predictors, conf_level, wt, at = at, link = link,
                           comparison = comparison,
                           want_pred = want_pred, exponentiate = exponentiate,
                           multiplier = multiplier, want_se = want_se,
                           anchors = anchors, crosses = crosses,
                           disp_known = disp_known, df_residual = df_residual)
  }
  if (identical(at, "average")) reg_marginal_basis_warn(fit, data, predictors, multiplier,
                                                        out$ame, log_ratio, do_exp)
  out
}

#' @keywords internal
reg_marginal_gcomp <- function(fit, data, predictors, conf_level, wt = NULL, link = "identity",
                               do_exp = !identical(link, "identity"),
                               want_pred = TRUE, want_se = TRUE, multiplier = NULL,
                               crosses = list(), rank = FALSE,
                               disp_known = TRUE, df_residual = NA_real_) {
  # ⚠ a poly() / ns() basis is the one shape whose marginal effect can be silently 0, and the check
  # for it (reg_marginal_basis_ok) needs predict(): refuse here so the fallback engine runs on a
  # revived fit instead.
  if (is_reg_digest(fit) && length(reg_basis_vars(fit, predictors))) return(NULL)
  tvars <- tryCatch(all.vars(stats::delete.response(stats::terms(fit))), error = function(e) NULL)
  # a nested cross block is named by its BLOCK, whose parents are the formula's own variables.
  need  <- unlist(lapply(predictors, function(v) {
    r <- reg_cross_of(crosses, v); if (is.null(r)) v else c(r$modified, r$moderator) }),
    use.names = FALSE)
  if (is.null(tvars) || !all(need %in% tvars)) return(NULL)
  V <- if (want_se) tryCatch(stats::vcov(fit), error = function(e) NULL) else NULL
  if (want_se && (is.null(V) || !is.matrix(V))) return(NULL)
  # THE THREE SWEEPS, chosen once: a rank contrast reads the whole predicted distribution and answers
  # with ONE number, so it takes the single-equation path from here on -- `per_cat` is what fans a
  # sweep out over the outcome's categories, and a rank has none to fan out over.
  per_cat <- !isTRUE(rank) && reg_model_categorical(fit)
  g <- if (isTRUE(rank)) reg_gcomp_rank_maker(fit, data, wt, link)
       else if (per_cat) reg_gcomp_cat_maker(fit, data, wt, link)
       else              reg_gcomp_maker(fit, data, wt, link)
  if (is.null(g)) return(NULL)
  crit <- reg_wald_crit(disp_known, df_residual, conf_level)   # the FIT's reference, never z by default
  amel <- list(); predl <- list()
  for (v in predictors) {
    rec    <- reg_cross_of(crosses, v)
    nested <- !is.null(rec) && identical(rec$arm, "nested")
    is_fac <- !nested && reg_is_factor_var(data[[v]])
    kof <- function(x) {
      k <- if (!is.null(multiplier) && x %in% names(multiplier)) as.numeric(multiplier[[x]]) else 1
      if (!is.finite(k) || k == 0) 1 else k
    }
    if (nested) {
      # ONE k-unit forward difference per moderator level, each averaged over that level's rows --
      # a subgroup AME, which the sweep already is once its weights are masked.
      lv  <- levels(forcats::fct_drop(as.factor(data[[rec$moderator]])))
      k   <- kof(rec$modified)
      cls <- lapply(lv, function(l) list(level = l, at = k, ref = 0, on = rec$modified,
                                         mask = as.numeric(!is.na(data[[rec$moderator]]) &
                                                             data[[rec$moderator]] == l)))
    } else if (is_fac) {
      lv <- levels(forcats::fct_drop(as.factor(data[[v]])))
      if (length(lv) < 2L) return(NULL)
      cls <- lapply(lv[-1], function(l) list(level = l, at = l, ref = lv[[1]]))
    } else {
      cls <- list(list(level = v, at = kof(v), ref = 0))   # a k-unit FORWARD DIFFERENCE
    }
    for (ct in cls) {
      p <- g(ct$on %||% v, ct$at, ct$ref, ct$mask)
      if (is.null(p)) return(NULL)
      # The 3+ level producer answers for every category at once (K-long, `group` naming them) where
      # a single-equation one is scalar. Hence one loop.
      grp <- if (per_cat) as.character(p$levels) else NA_character_
      se  <- if (per_cat) vapply(p$G, function(gj) reg_delta_se(gj, V), numeric(1))
             else         reg_delta_se(p$G, V)
      res <- reg_wald_finalize(p$est, do_exp, se = se, crit = crit,
                               disp_known = disp_known, df = df_residual)
      amel[[length(amel) + 1L]] <- tibble::tibble(
        var = v, level = as.character(ct$level), group = grp,
        ame = res$estimate, ame_lo = res$conf.low, ame_hi = res$conf.high, ame_p = res$p.value,
        alt = p$alt %||% NA_real_)
      add_pred <- function(l, val) predl[[length(predl) + 1L]] <<-
        tibble::tibble(var = v, level = l, group = grp, pred = val)
      if (want_pred && is_fac) {
        add_pred(as.character(ct$level), p$mean1)
        if (identical(ct$level, lv[[2]])) add_pred(lv[[1]], p$mean0)  # the reference's own, once
      }
      # a crossed slope has no level of its own, so the adjusted level it sits on is its GROUP's:
      # `mean0` is the prediction with the predictor left where the data has it.
      if (want_pred && nested) add_pred(as.character(ct$level), p$mean0)
    }
  }
  list(ame = dplyr::bind_rows(amel), pred = dplyr::bind_rows(predl))
}

#' @keywords internal
reg_marginal_basis_warn <- function(fit, data, predictors, multiplier, ame, ratio,
                                    do_exp = ratio) {
  bv <- reg_basis_vars(fit, predictors)
  # a digest has no predict() to compare against, and `reg_marginal_gcomp()` refused it upstream, so
  # whatever ran here was the fitted object's own answer.
  if (!length(bv) || is.null(ame) || !nrow(ame) || is_reg_digest(fit)) return(invisible(NULL))
  for (v in bv) {
    if (reg_is_factor_var(data[[v]])) next
    est <- ame$ame[ame$var == v]
    if (length(est) != 1L) next
    k <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else 1
    if (reg_marginal_basis_ok(fit, data, v, k, est, ratio, do_exp)) next
    cli::cli_warn(c(
      "!" = paste0("The marginal effect of {.val {v}} is not trustworthy: it is fitted through a ",
                   "basis expansion ({.code poly()} / {.code ns()}), which the marginal-effects ",
                   "engine re-evaluates on perturbed data."),
      "i" = 'Fit it with {.code shape = c({v} = "quadratic")} instead of a formula basis.'))
  }
  invisible(NULL)
}

# Marginal effects + adjusted predictions on the RESPONSE scale, through `marginaleffects`. `newdata`
# -- the complete-case fitted frame -- is REQUIRED: the package's own data recovery fails past the
# fitting scope and on dropped levels. A single-equation fit has `group = NA`; multinom / polr carry
# the outcome category there.
#   at = "average"   -> averaged over `data`, weighted by `wt`: a population quantity.
#   at = "reference" -> at the reference profile, a single datagrid row: no averaging, no weights.
# `comparison = "lnor"` is the multinomial j-vs-rest contrast (profile only) and `"lnratioavg"` its
# ratio twin; both return a log, so the interval stays a Wald one on the log scale. It is exp()'d
# back here only where the ESTIMAND asks for it -- see reg_marginal()'s two decisions.
reg_marginal_me <- function(fit, data, predictors, conf_level, wt = NULL,
                            at = "average", link = "identity", comparison = NULL, want_pred = TRUE,
                            exponentiate = TRUE,
                            multiplier = NULL, want_se = TRUE, anchors = NULL,
                            crosses = list(),
                            disp_known = TRUE, df_residual = NA_real_) {
  ref_vals <- if (at == "reference")
    reg_reference_grid_values(data, predictors, anchors,
                              if (is.null(wt) || !wt %in% names(data)) NULL else data[[wt]]) else NULL
  ref_grid <- if (at == "reference")
    do.call(marginaleffects::datagrid, c(list(model = fit), ref_vals)) else NULL
  # weights only at the AVERAGING step; a single-row profile takes none, and `wts = NULL` is
  # rejected.
  wts_arg <- if (at == "reference" || is.null(wt)) list() else list(wts = wt)
  cmp_arg <- if (is.null(comparison) || is.na(comparison)) list() else list(comparison = comparison)
  # the REPORTED link decides both: the engine works on the log of a ratio wherever it is not the
  # identity, and `exponentiate` says whether the column prints that log or undoes it.
  log_ratio <- !identical(link, "identity")
  do_exp    <- log_ratio && isTRUE(exponentiate)

  # `variables = list(v = k)` is a k-unit FORWARD DIFFERENCE, not k x the 1-unit AME. ⚠ the KEYWORD
  # is never passed through (see the `multiplier` section).
  var_arg <- function(v) {
    k <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else NA_real_
    if (is.finite(k) && k != 1 && !reg_is_factor_var(data[[v]])) stats::setNames(list(k), v) else v
  }
  # A crossed slope is `variables = list(x = k)` READ WITHIN each level of its moderator: `by` at
  # the sample average, one datagrid row per level at the reference profile. The block's rows ARE
  # the moderator's levels, so the level comes off that column rather than off a contrast label.
  x_ref_grid <- function(rec) do.call(marginaleffects::datagrid, c(
    list(model = fit),
    utils::modifyList(ref_vals %||% list(),
                      stats::setNames(list(levels(forcats::fct_drop(
                        as.factor(data[[rec$moderator]])))), rec$moderator))))
  # the delta-method jacobian costs one re-prediction PER COEFFICIENT, unpaid where the caller
  # discards the interval.
  se_arg <- if (want_se) list() else list(vcov = FALSE)
  # ⚠ marginaleffects always refers to z; the FIT may refer to t. Rebuild the bounds and the p from
  # the estimate and SE it reports, so this engine and the analytic one cannot hand one table two
  # reference distributions. The exp() fold rides here too, once for both arms. Where no SE comes
  # back (`want_se = FALSE`, or a comparison that reports none) the engine's own numbers stand.
  crit  <- reg_wald_crit(disp_known, df_residual, conf_level)
  refer <- function(ac) {
    n  <- length(ac$estimate)
    se <- ac[["std.error"]] %||% rep(NA_real_, n)   # [[: `$` partial-matches on a data frame
    r  <- if (all(is.na(se)))
      list(estimate  = ac$estimate,
           conf.low  = ac$conf.low  %||% rep(NA_real_, n),
           conf.high = ac$conf.high %||% rep(NA_real_, n),
           p.value   = ac$p.value   %||% rep(NA_real_, n))
    else reg_wald_finalize(ac$estimate, do_exp = FALSE, se = se, crit = crit,
                           disp_known = disp_known, df = df_residual)
    if (do_exp) r[c("estimate", "conf.low", "conf.high")] <-
      lapply(r[c("estimate", "conf.low", "conf.high")], exp)
    r
  }
  amelist <- purrr::map(predictors, function(v) {
    rec <- reg_cross_of(crosses, v)
    if (!is.null(rec) && identical(rec$arm, "nested")) {
      va <- var_arg(rec$modified)
      ac <- as.data.frame(if (at == "reference")
        do.call(marginaleffects::comparisons, c(
          list(fit, variables = va, newdata = x_ref_grid(rec), conf_level = conf_level),
          cmp_arg, se_arg))
        else do.call(marginaleffects::avg_comparisons, c(
          list(fit, variables = va, by = rec$moderator, newdata = data, conf_level = conf_level),
          wts_arg, cmp_arg, se_arg)))
      r <- refer(ac)
      return(tibble::tibble(
        var = v, level = as.character(ac[[rec$moderator]]),
        group = if ("group" %in% names(ac)) as.character(ac$group) else NA_character_,
        ame = r$estimate, ame_lo = r$conf.low, ame_hi = r$conf.high,
        ame_p = r$p.value))
    }
    ac <- if (at == "reference")
      as.data.frame(do.call(marginaleffects::comparisons, c(
        list(fit, variables = var_arg(v), newdata = ref_grid, conf_level = conf_level),
        cmp_arg, se_arg)))
    else
      as.data.frame(do.call(marginaleffects::avg_comparisons, c(
        list(fit, variables = var_arg(v), newdata = data, conf_level = conf_level),
        wts_arg, cmp_arg, se_arg)))
    is_fac <- reg_is_factor_var(data[[v]])
    # ⚠ strip the KNOWN prefix and reference suffix off the contrast label rather than splitting on
    # the first " - " or ")": a level containing either ("$20000 - 24999") was truncated and left an
    # NA cell.
    ref_lv <- if (is_fac) levels(forcats::fct_drop(as.factor(data[[v]])))[1] else NA_character_
    level  <- if (!is_fac) v else {
      # the label is `marginaleffects`' own: it names what the engine was ASKED to take the log OF,
      # which is the REPORTED link -- odds on a logit, the mean on a log -- never what this column
      # prints (`exponentiate` may undo it).
      inner <- if (identical(link, "logit")) "odds" else "mean"
      pre <- if (log_ratio) paste0("ln(", inner, "(") else ""
      suf <- if (log_ratio) paste0(") / ", inner, "(", ref_lv, "))") else paste0(" - ", ref_lv)
      substr(ac$contrast, nchar(pre) + 1L, nchar(ac$contrast) - nchar(suf))
    }
    grp    <- if ("group" %in% names(ac)) as.character(ac$group) else NA_character_
    r <- refer(ac)                                     # log-ratio -> OR / RR (and its CI) rides here
    tibble::tibble(var = v, level = as.character(level), group = grp,
                   ame = r$estimate, ame_lo = r$conf.low, ame_hi = r$conf.high, ame_p = r$p.value)
  })
  ame <- dplyr::bind_rows(amelist)

  predlist <- if (want_pred) purrr::map(predictors, function(v) {
    rec <- reg_cross_of(crosses, v)
    if (!is.null(rec) && identical(rec$arm, "nested")) {
      # a slope has no level of its own; the level it sits on is its GROUP's adjusted prediction.
      ap <- as.data.frame(if (at == "reference")
        marginaleffects::predictions(fit, newdata = x_ref_grid(rec), vcov = FALSE)
        else do.call(marginaleffects::avg_predictions, c(
          list(fit, variables = rec$moderator, newdata = data, vcov = FALSE), wts_arg)))
      return(tibble::tibble(
        var = v, level = as.character(ap[[rec$moderator]]),
        group = if ("group" %in% names(ap)) as.character(ap$group) else NA_character_,
        pred = ap$estimate))
    }
    if (!reg_is_factor_var(data[[v]])) return(NULL)      # no per-level prediction for numerics
    ap <- if (at == "reference") {
      grid_v <- do.call(marginaleffects::datagrid, c(list(model = fit),
        utils::modifyList(ref_vals, stats::setNames(list(levels(as.factor(data[[v]]))), v))))
      as.data.frame(marginaleffects::predictions(fit, newdata = grid_v, vcov = FALSE))
    } else {
      # the adjusted prediction is the marginal-STANDARDIZED one, which is what COHERES with the AME
      # (adjusted(ref) + AME(level) == adjusted(level)); `by = v` would give the OBSERVED rate.
      as.data.frame(do.call(marginaleffects::avg_predictions, c(
        list(fit, variables = v, newdata = data, vcov = FALSE), wts_arg)))
    }
    grp <- if ("group" %in% names(ap)) as.character(ap$group) else NA_character_
    tibble::tibble(var = v, level = as.character(ap[[v]]), group = grp, pred = ap$estimate)
  }) else list()
  pred <- dplyr::bind_rows(purrr::compact(predlist))

  list(ame = ame, pred = pred)
}

# === SECTION: the Constant row -- the baseline the column is read against ========================
#
# WHERE ITS VALUE COMES FROM. Under `effect = "conditional"` it IS the fit's own intercept, anchored
# by `ref`, and reg_column() reads it straight from the tidy. The other two contrasts have no
# intercept in their tidy, so the row holds the same thing computed the way THAT contrast computes
# everything else: the model's predicted outcome, averaged over the sample (`marginal`) or evaluated
# at the reference profile (`at_reference`).
#
# WHERE IT LANDS is reg_constant_place()'s one rule, shared by both producers: the row holds the
# quantity the column's effects OPERATE ON (EST_SCALES$const_display), so it leaves the estimate
# field wherever that is the level. Nothing downstream needs a branch -- the cell then carries a
# LEVEL token, which the signing and multiplicative-glyph rules of format() simply do not match.
#
# ⚠ it carries its interval, and a p-value only where what it prints has a null: a predicted 48.7 %
# is trivially "different from 0", so a star there would mean something else than everywhere else in
# the table (tab_constant_null(), R/fmt_class.R, reads exactly that).

# THE BASELINE ROW'S OWN BASE -- which is a property of the CONTRAST, so it is decided where the
# spec is known rather than in the family-free counter. Under `marginal` the row IS the population
# and rests on the whole model N. Otherwise it is a PROFILE, and a profile is a countable subgroup
# only when every predictor is categorical: with a continuous one nobody is at the mean, by
# definition, so the cell stays empty and the model N is read from the "N" footer row instead.
#' @keywords internal
reg_constant_count <- function(cnt, frame, sp, skeleton, wt = NULL, anchors = NULL) {
  i <- which(as.character(skeleton$var) == "Constant")
  if (!length(i)) return(cnt)
  w <- if (!is.null(wt) && length(wt) == 1L && !is.na(wt) && wt %in% names(frame))
    as.numeric(frame[[wt]]) else NULL
  keep <- if (identical(sp$est$effect %||% "", "marginal")) {
    rep(TRUE, nrow(frame))
  } else {
    vars <- sp$row_vars
    if (!length(vars) || !all(vars %in% names(frame))) return(cnt)
    if (!all(vapply(frame[vars], reg_is_factor_var, logical(1)))) return(cnt)
    vals <- reg_reference_grid_values(frame, vars, anchors, w)
    Reduce(`&`, lapply(names(vals), function(v)
      as.character(frame[[v]]) == as.character(vals[[v]])), rep(TRUE, nrow(frame)))
  }
  cnt$n[i] <- sum(keep, na.rm = TRUE)
  if (!is.null(w)) cnt$wn[i] <- sum(w[keep], na.rm = TRUE)
  cnt
}

# The one-row frame the reference profile is evaluated on. Built from `data` so every factor keeps
# its full level set -- a one-row grid assembled from scratch would drop levels and shorten the model
# matrix.
#' @keywords internal
reg_profile_row <- function(data, predictors, anchors = NULL, w = NULL) {
  d    <- data[1L, , drop = FALSE]
  vals <- reg_reference_grid_values(data, intersect(predictors, names(data)), anchors, w)
  for (v in names(vals)) {
    d[[v]] <- if (is.factor(data[[v]])) factor(vals[[v]], levels = levels(data[[v]]))
              else vals[[v]]
  }
  d
}

# WHERE THE BASELINE ROW'S VALUE BELONGS, for both contrasts at once. `EST_SCALES$const_display`
# names the quantity this column's effects OPERATE ON: an odds ratio multiplies odds, so an odds
# column keeps the baseline odds (with its level as the cell's aside); a risk / rate ratio multiplies
# the level and a difference adds to it, so those show the LEVEL itself; a coefficient adds on the
# link scale. The number never changes -- only the field it sits in and the token that renders it --
# which is what stops the row wearing a comparison sign or a "x" glyph it has no reference for.
# ⚠ a baseline shown as a LEVEL carries no p-value: a predicted 43 % is trivially "different from 0",
# and a star there would mean something else than everywhere else in the table.
# `base_v` may be NULL where the caller writes no level; it is returned filled if one is due.
#' @keywords internal
reg_constant_place <- function(scale_key, trials, is_cst, est_v, base_v, p_v, display) {
  sc  <- EST_SCALES[[scale_key]]
  tok <- sc$const_display %||% NA_character_
  out <- function() list(est = est_v, base = base_v, p = p_v, display = display)
  # ⚠ only where a baseline was actually computed: a cumulative logit has thresholds and no single
  # intercept, so stamping the token there would print the token's own NA where the row is empty.
  is_cst <- is_cst & !is.na(est_v)
  if (is.na(tok) || !any(is_cst)) return(out())
  # a summed score's LEVEL is the mean SCORE, the per-item probability x `trials` (exact). An
  # additive summed-score effect is already in score units (reg_column() scales the whole tidy).
  k  <- if (!is.na(trials %||% NA) && identical(sc$var_kind, "mean")) as.numeric(trials) else 1
  display[is_cst] <- tok
  if (identical(tok, sc$base_display)) {
    if (is.null(base_v)) base_v <- rep(NA_real_, length(est_v))
    base_v[is_cst] <- est_v[is_cst] * k
    est_v[is_cst]  <- NA_real_
    p_v[is_cst]    <- NA_real_
  } else if (identical(tok, "or") && !is.na(sc$base_display %||% NA)) {
    if (is.null(base_v)) base_v <- rep(NA_real_, length(est_v))
    o <- est_v[is_cst]
    base_v[is_cst] <- o / (1 + o) * k
  }
  out()
}

# A predicted outcome read on the column's own geometry, with the interval taken where that geometry
# is linear: an ODDS RATIO column shows the baseline odds and brackets it on the log-odds, a ratio
# column the baseline risk/rate on the log, an additive column the probability/mean as it is.
#' @keywords internal
reg_constant_cell <- function(P, se, scale_key, crit) {
  if (!is.finite(P) || !is.finite(se)) return(list(est = NA_real_, lo = NA_real_, hi = NA_real_))
  sc <- EST_SCALES[[scale_key]]
  if (identical(sc$est_field, "or")) {
    if (!isTRUE(P > 0 && P < 1)) return(list(est = NA_real_, lo = NA_real_, hi = NA_real_))
    o  <- P / (1 - P)
    sl <- se / (P * (1 - P))                                   # d log-odds / dP
    return(list(est = o, lo = o * exp(-crit * sl), hi = o * exp(crit * sl)))
  }
  if (isTRUE(sc$mult)) {
    if (!isTRUE(P > 0)) return(list(est = NA_real_, lo = NA_real_, hi = NA_real_))
    sl <- se / P
    return(list(est = P, lo = P * exp(-crit * sl), hi = P * exp(crit * sl)))
  }
  list(est = P, lo = P - crit * se, hi = P + crit * se)
}

# The whole row, per outcome category: NULL wherever the baseline cannot be computed (an offset, a
# polr under `coefficient`, a fit the digest path did not keep).
# `log = TRUE` is the link-scale column's route: the cell is built on the scale the estimand is the
# log OF (so the odds / level geometry above still applies), then logged -- which is why the interval
# stays exact rather than needing an arm of its own.
#' @keywords internal
reg_constant_baseline <- function(fit, data, predictors, at, wt, conf_level, scale_key,
                                  log = FALSE, anchors = NULL,
                                  disp_known = TRUE, df_residual = NA_real_) {
  if (is.null(fit) || is.null(scale_key)) return(NULL)
  w  <- if (!is.null(wt) && wt %in% names(data)) data[[wt]] else NULL
  nd <- if (identical(at, "reference")) reg_profile_row(data, predictors, anchors, w) else NULL
  b  <- reg_gcomp_baseline(fit, data, wt, newdata = nd)
  if (is.null(b)) return(NULL)
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(V) || !is.matrix(V)) return(NULL)
  crit <- reg_wald_crit(disp_known, df_residual, conf_level)   # the FIT's reference, never z by default
  cells <- purrr::map(seq_along(b$est), function(j)
    reg_constant_cell(b$est[[j]], reg_delta_se(b$G[[j]], V), scale_key, crit))
  out <- tibble::tibble(group = as.character(b$levels),
                        est = vapply(cells, `[[`, numeric(1), "est"),
                        lo  = vapply(cells, `[[`, numeric(1), "lo"),
                        hi  = vapply(cells, `[[`, numeric(1), "hi"))
  if (isTRUE(log)) out[c("est", "lo", "hi")] <- lapply(out[c("est", "lo", "hi")], base::log)
  out
}

# ONE build, every stamp read off the estimand's own EST_SCALES row -- the field the estimate goes
# in, the neutral a reference cell carries, whether the interval is Wald on the log, whether there is
# a multiplicative reference to mark, and where the SD ladder's divisor comes from. There is no
# per-family arm, which is what lets a `measure = "log_*"` column print logs with a 0 neutral and a
# symmetric interval without a sixth branch.
reg_marginal_column <- function(skeleton, marg, model_predictors, scale, var_y,
                                group, color, color_signif, col_var, or_tip = NULL,
                                model_family = "", trials = NULL, const = NULL,
                                degf = NA_real_) {
  amt <- marg$ame; prd <- marg$pred
  if (!is.na(group)) {
    amt <- amt[!is.na(amt$group) & amt$group == group, , drop = FALSE]
    if (nrow(prd)) prd <- prd[!is.na(prd$group) & prd$group == group, , drop = FALSE]
  }
  m     <- reg_skel_match(skeleton, amt)
  ame_v <- amt$ame[m]; lo_v <- amt$ame_lo[m]; hi_v <- amt$ame_hi[m]; p_v <- amt$ame_p[m]
  pred_v <- if (nrow(prd)) prd$pred[reg_skel_match(skeleton, prd)] else rep(NA_real_, nrow(skeleton))

  n_rows   <- nrow(skeleton)
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_const <- skeleton$var == "Constant"
  is_ref   <- skeleton$is_ref & !is_const & in_model
  # in_refrow is the UNION-skeleton row fact (see reg_column); `is_ref` stays in_model-gated below.
  refrows  <- (skeleton$is_ref & !is_const) | is_const

  # THE SCALE the estimand declares, and everything the cell is stamped with follows from it.
  sc  <- scale
  scr <- EST_SCALES[[sc]]

  display <- rep("blank", n_rows)
  show    <- in_model & (!is.na(ame_v) | is_ref)
  # the baseline this contrast is read against (see reg_constant_baseline): no p-value, so the row
  # takes no star and `keep <- !is.na(get_pvalue(col))` still means "an estimated effect".
  if (!is.null(const) && any(is_const)) {
    cst <- if (is.na(group)) const[1L, ] else const[const$group == group, , drop = FALSE]
    if (nrow(cst) == 1L && is.finite(cst$est)) {
      # ⚠ on a summed score the baseline arrives PER ITEM, like everything the sweep returns. The
      # additive branch below scales the whole vector by `trials`, but the placer takes the baseline
      # out of that vector first, so it is converted here.
      k <- if (identical(sc, "raw_diff") && !is.na(trials %||% NA)) as.numeric(trials) else 1
      ame_v[is_const] <- cst$est * k; lo_v[is_const] <- cst$lo * k; hi_v[is_const] <- cst$hi * k
      show <- show | is_const
    }
  }
  display[show] <- "est"
  # ...then the baseline row leaves the estimate field wherever its scale says the effects act on the
  # level. One rule, shared with the coefficient arm (reg_constant_place).
  base_fld <- scr$base_display %||% NA_character_
  cp     <- reg_constant_place(sc, trials, is_const, ame_v, pred_v, p_v, display)
  ame_v  <- cp$est; pred_v <- cp$base; p_v <- cp$p; display <- cp$display
  # every branch offers the level its scale names, so a baseline placed there is not dropped; where
  # the sweep has none the field is simply all-NA and reg_fill_base() fills it later.
  base_args <- if (!is.na(base_fld) && !identical(base_fld, scr$est_field))
    stats::setNames(list(pred_v), base_fld) else list()

  # EVERY reference cell carries its measure's own neutral -- 0 additive, 1 multiplicative, 0 again
  # on a link scale -- exactly as the coefficient twin does (reg_column()). It is what makes a
  # marginal effect read like its conditional counterpart instead of leaving a hole.
  ame_v[is_ref] <- scr$neutral
  # a SUMMED SCORE's additive effect is a difference of mean SCORES: the per-item contrast the sweep
  # returns, x `trials` (exact, since the interval scales by the same constant).
  if (identical(sc, "raw_diff") && !is.na(trials %||% NA)) {
    k <- as.numeric(trials); ame_v <- ame_v * k; lo_v <- lo_v * k; hi_v <- hi_v * k
  }
  do.call(fmt, c(
    stats::setNames(list(ame_v), scr$est_field),
    base_args,
    list(
      n = rep(NA_integer_, n_rows),   # the level's own count is stamped by the spec builder
      ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      scale = sc, pct_type = reg_pct_type(sc), display = display,
      digits = reg_cell_digits(sc),
      # a multiplicative estimate's interval is Wald on the LOG, and its baseline is the neutral 1.
      # A marginal effect is ALWAYS Wald -- neither engine produces profile bounds.
      ci_method = reg_wald_method_name("wald", scr$mult), degf = degf,
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"),
    if (isTRUE(scr$mult)) list(ref = "1"),
    # the model OR rides in `or` so the tooltip can surface it although the cell DISPLAYS the AME.
    if (!is.null(or_tip) && !identical(scr$est_field, "or")) list(or = or_tip),
    # var(Y): the divisor of the SD-standardized colour ladder, where the scale declares one.
    if (identical(scr$sd_from %||% "", "var")) list(var = rep(var_y, n_rows))))
}

reg_columns_multinom <- function(skeleton, f, sp, est, color, color_signif,
                                 cleannames, prefix_dep, col_var, model_family = "multinomial",
                                 method = "wald") {
  y_ref <- reg_cleanup(f$y_ref, cleannames)
  purrr::map(f$y_levels, function(j) {
    sub      <- f
    sub$tidy <- f$tidy[f$tidy$y.level == j,
                       setdiff(names(f$tidy), "y.level"), drop = FALSE]
    jc  <- reg_cleanup(j, cleannames)
    lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "",
                  jc, " vs ", y_ref)
    list(label = lab, emp_key = j,   # emp_key: raw category, for the empirical tooltip
         col   = reg_column(skeleton, sub, sp$predictors, col_var, est, color, color_signif,
                            model_family = model_family, method = method))
  })
}

# === SECTION: Model-summary footer -- GOF stats stored in the `test` attribute ===================
# The regression GOF lives in the SAME whole-table `test` tibble crosstabs use, adding ROWS whose
# discriminators never collide with the crosstab's -- so each renderer auto-no-ops on the other's
# table. A value stat carries its number in `statistic` (pvalue NA), a test stat statistic + df + p;
# `col_var` is the model's FIRST output column. The footer is DISPLAY-ONLY.

reg_null_loglik <- function(fit, family) {
  # the deviance-based LR null needs a KNOWN dispersion (the deviance is then the likelihood).
  if (reg_fam_disp_known(family) &&
      !is.null(fit$null.deviance) && !is.null(fit$deviance)) {
    ll_f <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
    lr   <- fit$null.deviance - fit$deviance
    df   <- fit$df.null - fit$df.residual
    return(list(ll_f = ll_f, ll_0 = ll_f - lr / 2, df = df))
  }
  null <- tryCatch({
    mf   <- stats::model.frame(fit)
    fla  <- stats::reformulate("1", response = names(mf)[1])
    if (inherits(fit, "multinom")) nnet::multinom(fla, data = mf, trace = FALSE)
    else if (inherits(fit, "polr")) MASS::polr(fla, data = mf, Hess = TRUE)
    else NULL
  }, error = function(e) NULL)
  if (is.null(null)) return(NULL)
  llf <- tryCatch(stats::logLik(fit),  error = function(e) NULL)
  ll0 <- tryCatch(stats::logLik(null), error = function(e) NULL)
  if (is.null(llf) || is.null(ll0)) return(NULL)
  list(ll_f = as.numeric(llf), ll_0 = as.numeric(ll0),
       df = attr(llf, "df") - attr(ll0, "df"))
}

# Pearson dispersion: poisson / grouped binomial only -- not identifiable for ungrouped Bernoulli
# data. PURE: the over-dispersion warning belongs to reg_fit(), where the SEs are actually scaled.
# ⚠ the denominator is n - rank, computed HERE, NEVER stats::df.residual(fit): for an svyglm that is
# the DESIGN degrees of freedom, which puts a weighted dispersion off by an order of magnitude.
reg_dispersion <- function(fit) {
  rp  <- tryCatch(stats::residuals(fit, type = "pearson"), error = function(e) NULL)
  if (is.null(rp)) return(NA_real_)
  n    <- sum(is.finite(rp))
  rank <- tryCatch(sum(!is.na(stats::coef(fit))), error = function(e) NA_integer_)
  if (is.na(rank)) return(NA_real_)
  dfr <- n - rank
  if (dfr <= 0) return(NA_real_)
  sum(rp^2, na.rm = TRUE) / dfr
}

reg_aic_value <- function(fit) {
  a <- tryCatch(suppressWarnings(stats::AIC(fit)), error = function(e) NA_real_)
  if (length(a) > 1L && !is.null(names(a)) && "AIC" %in% names(a)) return(as.numeric(a[["AIC"]]))
  as.numeric(a)[1]
}

# The gaussian model-fit statistics: summary.lm()'s own numbers.
# `df` is the F test's NUMERATOR df, `df.residual` the fit's own.
# ⚠ NULL for anything that is not an lm. A glm summary carries no r.squared, and a footer row built
# on a NULL statistic would be a wrong number rather than a missing one.
#' @keywords internal
reg_glance_lm <- function(fit) {
  s <- summary(fit)
  if (is.null(s$r.squared)) return(NULL)
  fs <- s$fstatistic                                   # NULL on an intercept-only fit
  tibble::tibble(
    r.squared = s$r.squared, adj.r.squared = s$adj.r.squared, sigma = s$sigma,
    statistic   = if (is.null(fs)) NA_real_ else unname(fs[["value"]]),
    df          = if (is.null(fs)) NA_real_ else unname(fs[["numdf"]]),
    df.residual = as.numeric(stats::df.residual(fit)),
    p.value     = if (is.null(fs)) NA_real_ else
      unname(stats::pf(fs[["value"]], fs[["numdf"]], fs[["dendf"]], lower.tail = FALSE)),
    AIC = stats::AIC(fit), BIC = stats::BIC(fit))
}

# GOF stats for ONE fit. quasi* / svyglm have no true likelihood, so those stats stay NA or become a
# relabelled Rao-Scott Wald -- never a false LR.
reg_glance <- function(fit, family, grouped, weighted, nobs) {
  row <- function(test, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_, pvalue = NA_real_)
    tibble::tibble(test = test, statistic = statistic, df1 = df1, df2 = df2, pvalue = pvalue)
  out <- row("n", statistic = as.numeric(nobs))
  wald_null_row <- function(fit) {
    terms_all <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
    wt <- if (length(terms_all) > 0)
      tryCatch(suppressWarnings(survey::regTermTest(fit, stats::reformulate(terms_all))),
               error = function(e) NULL)
    else NULL
    if (is.null(wt)) return(NULL)
    row("wald_null", statistic = as.numeric(wt$Ftest), df1 = as.numeric(wt$df),
        df2 = as.numeric(wt$ddf), pvalue = as.numeric(wt$p))
  }

  # a fit that reaches a MEASURE through a misspecified likelihood has no AIC / BIC / McFadden, and a
  # 0/1 Pearson dispersion is a constant, not a diagnostic. Placed FIRST so it holds weighted or not.
  if (family %in% REG_FIT_ONLY_FAMILIES) {
    out <- dplyr::bind_rows(out, wald_null_row(fit))
    return(out)
  }

  if (weighted) {
    out <- dplyr::bind_rows(out, wald_null_row(fit))
    nk <- tryCatch(suppressWarnings(as.numeric(survey::psrsq(fit, method = "Nagelkerke"))),
                   error = function(e) NA_real_)
    if (!is.na(nk)) out <- dplyr::bind_rows(out, row("nagelkerke_r2", statistic = nk))
    cs <- tryCatch(suppressWarnings(as.numeric(survey::psrsq(fit, method = "Cox-Snell"))),
                   error = function(e) NA_real_)                # selectable via stats=; not in the default set
    if (!is.na(cs)) out <- dplyr::bind_rows(out, row("cox_snell_r2", statistic = cs))
    aic <- reg_aic_value(fit)
    if (!is.na(aic)) out <- dplyr::bind_rows(out, row("aic", statistic = aic))
    return(out)
  }

  if (family == "gaussian") {
    g <- tryCatch(reg_glance_lm(fit), error = function(e) NULL)
    if (!is.null(g)) out <- dplyr::bind_rows(out,
      row("r2",      statistic = g$r.squared),
      row("r2_adj",  statistic = g$adj.r.squared),
      row("f_model", statistic = g$statistic, df1 = g$df, df2 = g$df.residual, pvalue = g$p.value),
      row("sigma",   statistic = g$sigma),
      row("aic",     statistic = g$AIC),
      row("bic",     statistic = g$BIC))
    return(out)
  }

  nl <- reg_null_loglik(fit, family)
  if (!is.null(nl) && !is.na(nl$ll_f) && !is.na(nl$ll_0) && !is.na(nl$df) && nl$df > 0) {
    lr <- 2 * (nl$ll_f - nl$ll_0)
    out <- dplyr::bind_rows(out,
      row("lr_null", statistic = lr, df1 = nl$df, pvalue = stats::pchisq(lr, nl$df, lower.tail = FALSE)),
      row("mcfadden_r2", statistic = 1 - nl$ll_f / nl$ll_0))
  }
  aic <- reg_aic_value(fit)
  bic <- tryCatch(as.numeric(stats::BIC(fit)), error = function(e) NA_real_)
  if (!is.na(aic)) out <- dplyr::bind_rows(out, row("aic", statistic = aic))
  if (!is.na(bic)) out <- dplyr::bind_rows(out, row("bic", statistic = bic))
  if (reg_fam_overdispersed(family, grouped)) {
    phi <- reg_dispersion(fit)
    if (!is.na(phi)) out <- dplyr::bind_rows(out, row("phi", statistic = phi))
  }
  out
}

# THE `stats =` / `check =` vocabulary in one place, DERIVED from TEST_ROWS' `glance` block, i.e. the
# rows reg_glance() emits. ⚠ its ORDER is TEST_ROWS' display order.
#' @keywords internal
REG_GOF_KEYS <- .trow_keys(.trow_chr("block") == "glance")

# ⚠ THE UNION IS MANDATORY: `unique(TEST_ROWS$stat)` alone silently DROPS `residuals` and
# `normality`, both legal `check =` values with a panel and, deliberately, no test row -- so they
# have no `stat` here to be derived from.
#' @keywords internal
reg_stat_keys <- function() unique(c(.trow_chr("stat")[.trow_chr("producer") == "reg"],
                                     names(REG_CHECKS)))

# === SECTION: `stats =` is ONE argument ==========================================================
# `stats` says WHAT RIDES THE MODEL-SUMMARY FOOTER, the model comparison included. An element is
# always a KEY -- carried in the NAME when it has a parameter, in the VALUE when it does not, which
# is `ref = c(var = "level")`'s grammar one subsystem over:
#
#   stats = c("n", "aic", "compare_sequential")     each model vs the previous one
#   stats = "compare_baseline"                      each model vs the FIRST
#   stats = c("n", compare_baseline = "M1")         ... vs the model labelled "M1"
#   stats = c("n", compare_baseline = 2)            ... vs the 2nd column
#
# The boundary validates KEYS; the resolver splits them into the plain triple every producer speaks.

#' @keywords internal
#' @noRd
reg_stats_keys_of <- function(stats) {
  if (!is.character(stats) || !length(stats)) return(character(0))
  nm <- names(stats)
  if (is.null(nm)) unname(stats) else ifelse(nzchar(nm), nm, unname(stats))
}

#' @keywords internal
#' @noRd
reg_resolve_stats <- function(stats) {
  # `none` = keep what was ASKED FOR, drop only the comparison (the named-footer-set branch below);
  # `nothing` = no footer at all.
  none    <- list(stats = stats, compare = "none", baseline = NULL)
  nothing <- list(stats = FALSE, compare = "none", baseline = NULL)
  # NOTHING IS NOTHING. `NULL` / `FALSE` / "no" / "none" all hide the WHOLE footer, the comparison
  # included: one argument means one list of what the footer shows, and a user who writes NULL is
  # asking for no statistics, not for the default set. That is why the signature's default is the
  # word "auto" -- R cannot tell a missing argument from an explicit NULL.
  if (is.null(stats) || isFALSE(stats) ||
      (is.character(stats) && length(stats) == 1L && stats %in% c("no", "none"))) return(nothing)
  # THE DEFAULT COMPARES. Writing several `predictors` sets is something a reader does on purpose,
  # and the row that says whether the added variables bought anything is the point of writing them
  # -- so "auto" is what the unnamed footer asks for, and reg_compare_rows() then picks BETWEEN the
  # two comparisons from the models themselves (nested chain -> sequential, else vs the first).
  # ⚠ it must NEVER abort or restrict, being a default: reg_resolve_args() degrades it to "none"
  # wherever a between-model test has no meaning, before anything reads it.
  # ⚠ "all" travels on, because reg_footer_stats() reads that word itself (every statistic AND every
  # check); "auto" / TRUE ask for the per-family default set, which it spells NULL.
  auto <- function(keep) list(stats = keep, compare = "auto", baseline = NULL)
  if (identical(stats, "all")) return(auto("all"))
  if (isTRUE(stats) || identical(stats, "auto")) return(auto(NULL))
  if (!is.character(stats) || !length(stats)) return(nothing)

  keys  <- reg_stats_keys_of(stats)
  is_cmp <- keys %in% c("compare_baseline", "compare_sequential")
  # a NAMED footer set that does not name a comparison has dropped it on purpose.
  if (!any(is_cmp)) return(none)
  cmp <- keys[is_cmp]
  if (length(cmp) > 1L)
    cli::cli_abort(c("{.arg stats} names more than one model comparison: {.val {cmp}}.",
                     "i" = "A footer row compares each model to one other, so pick one.",
                     call = NULL))

  val <- unname(stats[is_cmp])
  bl  <- if (nzchar(names(stats)[is_cmp] %||% "") && !is.na(val) && nzchar(val)) val else NULL
  if (identical(cmp, "compare_sequential") && !is.null(bl))
    cli::cli_abort(c('{.code stats = c(compare_sequential = {.val {bl}})} names a baseline model.',
                     "x" = "A sequential comparison has none: each model is tested against the previous one.",
                     "i" = 'Did you mean {.code stats = c(compare_baseline = "{bl}")}?'), call = NULL)
  if (!is.null(bl) && grepl("^[0-9]+$", bl)) bl <- as.numeric(bl)

  # ⚠ A comparison key RESTRICTS NOTHING: `stats = "compare_baseline"` asks for a comparison, not
  # for a footer holding only that -- so when the comparison keys are all that was named, what is
  # left is NULL, i.e. the per-family default set. Hiding the whole footer is still `stats = FALSE`
  # / `"none"`.
  rest <- unname(stats[!is_cmp])
  list(stats    = if (!length(rest)) NULL else rest,
       compare  = if (identical(cmp, "compare_sequential")) "sequential" else "baseline",
       baseline = bl)
}

# === SECTION: `digits =` -- a FLOOR, and a per-token override ====================================
# DIGITS ARE A DISPLAY PROPERTY, so `digits` writes where the display lives and nowhere else:
#   digits = 2                  a floor on every cell -- the stored per-column `digits` field
#   digits = c(base = 1)        one token, at that precision -- the "{base:1}" template suffix
#   digits = c(2, ratio = 3)    both
# The two halves are not interchangeable: the FIELD is one number for a whole cell (so it can only
# raise the estimate and the tokens that declare a minimum), while only the TEMPLATE can say that an
# aside reads at one decimal beside an estimate reading at three. Neither adds a field or an
# attribute -- the record already carries both.
#' @keywords internal
#' @noRd
reg_resolve_digits <- function(digits) {
  none <- list(floor = 0L, tokens = integer(0))
  if (is.null(digits) || !length(digits)) return(none)
  if (!is.numeric(digits) && !is.character(digits))
    cli::cli_abort("{.arg digits} must be a number, or a named vector of them.", call = NULL)
  d  <- suppressWarnings(as.integer(digits))
  nm <- names(digits) %||% rep("", length(d))
  if (anyNA(d) || any(d < 0L | d > 6L))
    cli::cli_abort(c("{.arg digits} must be whole numbers between 0 and 6.",
                     "x" = "Got {.val {as.character(digits)}}."), call = NULL)
  known <- c(DISPLAY_USER_FIELDS, names(DISPLAY_ALIASES))
  bad   <- setdiff(nm[nzchar(nm)], known)
  if (length(bad))
    cli::cli_abort(c("{.arg digits} names {?a field/fields} no cell can print: {.val {bad}}.",
                     "i" = "Valid: {.or {.val {DISPLAY_USER_FIELDS}}}.",
                     "i" = "An unnamed value is the floor for the whole table."), call = NULL)
  if (sum(!nzchar(nm)) > 1L)
    cli::cli_abort(c("{.arg digits} takes one unnamed value, the floor for the whole table.",
                     "i" = 'Name the others: {.code digits = c(2, ratio = 3)}.'), call = NULL)
  list(floor  = if (any(!nzchar(nm))) d[[which(!nzchar(nm))[[1]]]] else 0L,
       tokens = stats::setNames(d[nzchar(nm)], nm[nzchar(nm)]))
}

# The per-token half, applied POST-HOC over the finished table -- one pass, the way set_display() is
# post-hoc: it rewrites each fmt column's template, naming the precision on the tokens the user
# named. ⚠ `est` / `base` are scale-relative, so a column is matched on the token its scale resolves
# them to as well as on the written word -- `digits = c(ratio = 3)` finds a column whose template
# says `{est}` and whose estimate IS a ratio.
#' @keywords internal
#' @noRd
reg_digits_write <- function(tab, floor = 0L, tokens = integer(0)) {
  if (floor <= 0L && !length(tokens)) return(tab)
  for (j in which(vapply(tab, is_fmt, logical(1)))) {
    col <- tab[[j]]
    # THE FLOOR, on the stored field. A count is not raised: format() pins the `n` tokens at 0
    # decimals before any floor is read, which is where that rule belongs and already lives.
    if (floor > 0L) col <- set_digits(col, pmax(get_digits(col), floor))
    tab[[j]] <- col
    if (!length(tokens)) next
    scl <- fmt_scale_row(col)
    d   <- get_display(col)
    # ⚠ `est` / `base` are scale-relative, so a token is matched BOTH as written and as this column's
    # scale resolves it: `digits = c(ratio = 3)` must find a column whose template says `{est}`.
    named <- function(tok) {
      h <- match(tok, names(tokens))
      ifelse(is.na(h), match(fmt_resolve_scale_tokens(tok, scl), names(tokens)), h)
    }
    for (tmpl in unique(d[!is.na(d) & nzchar(d)])) {
      # a BARE token is a one-token template; writing it braced is what gives it a precision
      if (!grepl("{", tmpl, fixed = TRUE)) {
        h <- named(display_primary(tmpl))
        if (is.na(h)) next
        d[!is.na(d) & d == tmpl] <- paste0("{", tmpl, ":", tokens[[h]], "}")
        next
      }
      seg <- parse_display_template(tmpl)
      hit <- named(seg$fields)
      if (all(is.na(hit))) next
      pieces <- seg$pieces
      ti     <- which(seg$is_tok)
      for (i in which(!is.na(hit)))
        pieces[[ti[[i]]]] <- paste0("{", seg$fields[[i]], ":", tokens[[hit[[i]]]], "}")
      d[!is.na(d) & d == tmpl] <- paste0(pieces, collapse = "")
    }
    tab[[j]] <- set_display(col, d)
  }
  tab
}

#' @keywords internal
reg_validate_stat_keys <- function(x, arg = "stats", allowed = reg_stat_keys()) {
  bad <- setdiff(x, allowed)
  if (length(bad))
    cli::cli_abort(c("{.arg {arg}} must name model-fit statistics or checks.",
                     "x" = "Unknown: {.val {bad}}.",
                     "i" = "Available: {.val {allowed}}."))
  x
}

# Resolve `stats =` -> the ordered footer discriminators. NULL / TRUE = the per-context default set;
# "all" = every statistic AND check, fit-based ones included; FALSE / "none" = no footer.
reg_footer_stats <- function(family, weighted, grouped, stats) {
  default <- if (family %in% REG_FIT_ONLY_FAMILIES) c("n", "wald_null")
    else if (weighted) c("n", "wald_null", "nagelkerke_r2", "aic")
    else if (family == "gaussian") c("n", "r2", "r2_adj", "f_model", "sigma")
    else { s <- c("n", "lr_null", "mcfadden_r2", "aic", "bic")
           # `phi` is the EXACT Pearson dispersion; the key `dispersion` names the CHECK.
           if (reg_fam_overdispersed(family, grouped)) s <- c(s, "phi"); s }
  # ⚠ the per-predictor global test is NOT in the default set. It answers a real question the stars
  # cannot -- does this predictor matter as a whole -- but it is one row per multi-level factor, it
  # costs a drop1() refit each on the unweighted path, and on a table where every block is strongly
  # associated it repeats what the reader has already read. `stats = "global"` asks for it.
  checks  <- reg_checks_for(family, weighted)
  # the crossed-pair interaction test costs ONE extra fit, and only where a cross was asked for. On
  # a glm that is ~20 ms, so it joins the default set; on multinomial / ordinal it roughly DOUBLES
  # the fitting time, so it is opt-in -- the free/refit rule REG_CHECKS already states for a check.
  default <- c(default, if (reg_fam_glm(family)) "interaction",
               reg_checks_default(family, weighted))
  # "all" = every statistic AND check, the per-predictor joint test included (want_global reads the
  # same word, and the two must not disagree about what "all" means).
  if (identical(stats, "all")) return(reg_check_expand(unique(c(default, "global", checks))))
  if (is.null(stats) || isTRUE(stats)) return(reg_check_expand(default))
  if (isFALSE(stats)) return(character(0))
  reg_check_expand(stats[stats %in% reg_stat_keys()])
}

reg_gof_rows <- function(f, sp, col_var, weighted, grouped, stats) {
  keep <- reg_footer_stats(sp$fit_family, weighted, isTRUE(grouped), stats)
  if (length(keep) == 0) return(NULL)                        # stats = FALSE -> no glance, no warnings
  g <- if (!is.null(f$glance)) f$glance
       else reg_glance(f$fit, sp$fit_family, isTRUE(grouped), weighted, f$nobs)
  g <- g[g$test %in% keep, , drop = FALSE]
  g <- g[order(match(g$test, keep)), , drop = FALSE]           # spec order
  if (nrow(g) == 0) return(NULL)
  reg_test_row(g$test, col_var, statistic = g$statistic, df1 = g$df1, df2 = g$df2,
               pvalue = g$pvalue, nobs = as.numeric(f$nobs), outcome = sp$outcome)
}

# === SECTION: Multi-model comparison -- each model column vs a baseline / the previous one ========
# An LR / F test between two models is only valid on the SAME complete-case set and when one nests in
# the other; a guard failure falls back to Delta-AIC. Nesting is checked in BOTH directions, a
# baseline being allowed to be the SUPERSET.
reg_compare_guard <- function(m_ref, m_full, crosses = list()) {
  ok_n   <- tryCatch(stats::nobs(m_ref) == stats::nobs(m_full), error = function(e) FALSE)
  t_ref  <- tryCatch(attr(stats::terms(m_ref),  "term.labels"), error = function(e) NULL)
  t_full <- tryCatch(attr(stats::terms(m_full), "term.labels"), error = function(e) NULL)
  if (is.null(t_ref) || is.null(t_full) || !isTRUE(ok_n)) return(0L)
  t_ref  <- reg_cross_expand_terms(t_ref,  crosses)
  t_full <- reg_cross_expand_terms(t_full, crosses)
  if (all(t_ref %in% t_full)) return(1L)                  # ref nested in full
  if (all(t_full %in% t_ref)) return(-1L)                 # full nested in ref (superset baseline)
  0L
}

# Is this a SEQUENCE -- every model's terms a subset of the next one's? The question the automatic
# comparison asks, and the one a reader answers by writing the `predictors` list they wrote. Term
# sets only, so the answer is the same in every tab_vars group (see reg_compare_rows).
reg_compare_chained <- function(fits, crosses = list()) {
  terms_of <- function(m) tryCatch(
    reg_cross_expand_terms(attr(stats::terms(m), "term.labels"), crosses), error = function(e) NULL)
  tl <- lapply(purrr::map(fits, "fit"), terms_of)
  if (length(tl) < 2L || any(vapply(tl, is.null, logical(1)))) return(FALSE)
  all(vapply(seq_along(tl)[-1L], function(i) all(tl[[i - 1L]] %in% tl[[i]]), logical(1)))
}

reg_order_union <- function(models) {
  sets  <- purrr::map(models, unique)
  all_u <- unique(purrr::flatten_chr(sets))
  complete_i <- which(purrr::map_lgl(sets, function(s) all(all_u %in% s)))
  if (length(complete_i) > 0L) unique(sets[[complete_i[length(complete_i)]]]) else all_u
}

reg_compare_extract <- function(an, use_f) {
  k     <- nrow(an)
  p_col <- grep("^Pr\\(", names(an), value = TRUE)
  p     <- if (length(p_col)) suppressWarnings(as.numeric(an[[p_col[1]]][k])) else NA_real_
  df1   <- suppressWarnings(abs(as.numeric(an[["Df"]][k])))
  if (use_f) list(stat = suppressWarnings(as.numeric(an[["F"]][k])), df1 = df1,
                  df2 = suppressWarnings(as.numeric(an[["Res.Df"]][k])), p = p)
  else       list(stat = suppressWarnings(as.numeric(an[["Deviance"]][k])), df1 = df1,
                  df2 = NA_real_, p = p)
}

# One comparison row per model column: LR for binomial / poisson / multinomial / ordinal, F for
# gaussian / quasi, a design-based Wald for a WEIGHTED (or "rr") model so no false-LR claim is made.
# Distinct discriminators per test kind keep each row homogeneous, so its label alone names the test.
reg_compare_rows <- function(reg_gof, fits, specs, family, weighted, fit_first_col,
                             compare = "none", baseline = NULL, crosses = list()) {
  if (identical(compare, "none")) return(reg_gof)
  n <- length(fits)
  if (n < 2L) {
    # ⚠ silent under "auto": that is the DEFAULT, and a one-model table is not a mistake.
    if (!identical(compare, "auto"))
      cli::cli_inform(c("i" = paste0("{.arg compare} needs at least two models: a {.arg predictors} ",
                                     "list, or several outcomes.")))
    return(reg_gof)
  }
  # WHICH comparison, decided from the models themselves: a chain where each one nests in the next is
  # something a reader built on purpose, and the honest test there is each vs the PREVIOUS. Anything
  # else is read against the first.
  # ⚠ TERM SETS ONLY, not reg_compare_guard(): the guard also requires an equal N, which is a
  # property of one tab_vars GROUP's missing data -- so it would pick sequential in one group and
  # baseline in the next, and one table would carry two kinds of comparison row. Nesting is
  # structural and group-invariant; a differing N is still caught per pair below, where it falls
  # back to Delta-AIC on that column alone.
  auto <- identical(compare, "auto")
  if (auto) {
    compare  <- if (reg_compare_chained(fits, crosses)) "sequential" else "baseline"
    baseline <- NULL
  }
  use_f  <- reg_fam_disp_estimated(family)
  # an "rr" fit is an svyglm, so it takes the Wald branch either way: an LR between two
  # quasi-likelihood fits would be a false LR.
  use_wald <- reg_fam_svy_fitted(family, weighted)
  base_i <- if (compare == "baseline") {
    if (is.null(baseline))          1L
    else if (is.numeric(baseline))  as.integer(baseline)
    else                            match(baseline, purrr::map_chr(specs, "label"))
  } else NA_integer_
  if (compare == "baseline" && (is.na(base_i) || base_i < 1L || base_i > n)) {
    cli::cli_warn("{.arg baseline} {.val {baseline}} matches no model; using the first.")
    base_i <- 1L
  }

  cmp_outcome <- specs[[1]]$outcome
  row <- function(test, col_var, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_,
                  pvalue = NA_real_, nobs = NA_real_)
    reg_test_row(test, col_var, statistic = statistic, df1 = df1, df2 = df2,
                 pvalue = pvalue, nobs = nobs, outcome = cmp_outcome)

  stat_key <- if (compare == "sequential") "compare_sequential" else "compare_baseline"
  rows <- purrr::map(seq_len(n), function(i) {
    ref_i <- if (compare == "sequential") i - 1L else base_i
    if (is.na(ref_i) || ref_i < 1L || ref_i == i) return(NULL)
    m_full <- fits[[i]]$fit; m_ref <- fits[[ref_i]]$fit
    col    <- fit_first_col[[i]]
    dir  <- reg_compare_guard(m_ref, m_full, crosses)
    m_lo <- if (dir >= 0L) m_ref  else m_full
    m_hi <- if (dir >= 0L) m_full else m_ref
    if (dir != 0L) {
      if (use_wald) {
        e <- tryCatch({
          an <- stats::anova(m_lo, m_hi, method = "Wald", test = "F")
          list(stat = as.numeric(an$Ftest), df1 = as.numeric(an$df),
               df2 = as.numeric(an$ddf), p = as.numeric(an$p))
        }, error = function(e) NULL)
        if (!is.null(e) && !is.na(e$p)) {
          return(row(test_row_key(stat_key, "wald"), col, statistic = e$stat, df1 = e$df1,
                     df2 = e$df2, pvalue = e$p, nobs = fits[[i]]$nobs))
        }
      } else {
        an <- tryCatch(stats::anova(m_lo, m_hi, test = if (use_f) "F" else "Chisq"),
                       error = function(e) NULL)
        if (!is.null(an)) {
          e <- reg_compare_extract(an, use_f)
          if (!is.na(e$p)) {
            disc <- test_row_key(stat_key, if (use_f) "f" else "lr")
            return(row(disc, col, statistic = e$stat, df1 = e$df1, df2 = e$df2, pvalue = e$p,
                       nobs = fits[[i]]$nobs))
          }
        }
      }
    }
    daic <- tryCatch(reg_aic_value(m_full) - reg_aic_value(m_ref), error = function(e) NA_real_)
    # ⚠ the ROW still appears -- Delta-AIC names itself and is a legitimate answer -- but a table
    # that never asked for a comparison must not explain the one it got.
    if (!auto) cli::cli_inform(c(
      "i" = paste0("{.val {col}}: the models are not nested, or their N differ, so the AIC ",
                   "difference is shown instead of a likelihood-ratio test."),
      "i" = 'Differing N is usually the missing values: {.code na = "drop_all"} fits them on the same rows.'))
    row(test_row_key(stat_key, "aic"), col, statistic = daic, nobs = fits[[i]]$nobs)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}


# === SECTION: The aggregated effect-modification test (predictor x tab_vars) =====================
# The per-cell `between_groups` colour says how big each group difference is, one cell at a time;
# this says ONCE per predictor whether its effect differs between groups at all -- aggregated, so no
# multiplicity inflation. ONE extra pooled fit `y ~ (predictors) * g`, then drop1() per predictor or
# survey::regTermTest(), the same LR-F-vs-Wald split the model comparison uses.
# DESIGN: these rows are deliberately ABSENT from reg_footer_spec(). A footer ROW is keyed to exactly
# one model column and a pooled test belongs to none, so they stay pure data, rendered as a
# table-wide footer STREAM.

#' @keywords internal
reg_test_row <- function(test, col, var = "", statistic = NA_real_, df1 = NA_real_, df2 = NA_real_,
                         pvalue = NA_real_, nobs = NA_real_, outcome = NA_character_)
  tibble::tibble(var = var, col = col, test = test, statistic = statistic,
                 df1 = df1, df2 = df2, pvalue = pvalue, n = nobs, min_e = NA_real_, outcome = outcome)

#' @keywords internal
reg_interaction_types <- function() unname(test_row_types("group_interaction"))

# ⚠ THE FOURTH FITTING SITE, and the one that cannot join a per-spec product: it fits the POOLED
# model -- every tab_vars group at once, with the group interacted -- so it lives AFTER the split
# barrier. A per-spec builder runs inside ONE group and can never see the others: not a missed
# parallel axis.
#' @keywords internal
reg_interaction_rows <- function(reg_gof, data, specs, shared, tab_vars, fit_first_col) {
  weighted <- shared$weighted
  rows <- purrr::map(seq_along(specs), function(i) {
    sp <- specs[[i]]
    # No pooled interaction for the engines that are not one glm equation, nor for a compound
    # formula: degrade to no row, never a wrong one.
    if (!reg_fam_glm(sp$fit_family) || isTRUE(sp$compound)) return(NULL)
    preds <- sp$predictors
    if (length(preds) == 0L) return(NULL)
    f <- tryCatch(reg_fit(data, sp$outcome, preds, sp$fit_family, shared$design_spec, isTRUE(sp$est$exp),
                          reg_outcome_level_of(sp$outcome_level) %||% shared$outcome_level,
                          shared$conf_level, "wald", trials = sp$trials, formula = NULL,
                          multiplier = NULL, cross = tab_vars),
                  error = function(e) NULL)
    if (is.null(f) || is.null(f$fit)) return(NULL)
    fit      <- f$fit
    use_f    <- reg_fam_disp_estimated(sp$fit_family)
    use_wald <- reg_fam_svy_fitted(sp$fit_family, weighted)
    # WARNING: take the interaction terms from the FIT's own term.labels, verbatim -- terms() orders
    # the parts of an interaction by formula position, so a hand-built "age:party3" comes back as
    # "party3:age" and drop1() then rejects the scope.
    have  <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
    inter <- have[grepl(":", have, fixed = TRUE)]
    keyed <- vapply(inter, function(tl) {
      parts <- gsub("`", "", strsplit(tl, ":", fixed = TRUE)[[1]], fixed = TRUE)
      if (length(parts) == 2L && tab_vars %in% parts) setdiff(parts, tab_vars)[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
    ok      <- !is.na(keyed) & keyed %in% preds
    terms_i <- inter[ok]
    keep    <- keyed[ok]

    reg_term_tests(fit, keep, terms_i, use_f, use_wald,
                   types = test_row_types("group_interaction"),
                   col_var = fit_first_col[[i]], nobs = f$nobs, outcome = sp$outcome)
  })
  rows <- purrr::compact(purrr::flatten(purrr::compact(rows)))
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}

#' @keywords internal
reg_term_tests <- function(fit, preds, terms, use_f, use_wald, types, col_var, nobs,
                           outcome = NA_character_) {
  if (length(terms) == 0L) return(NULL)
  if (use_wald) {
    return(purrr::map2(preds, terms, function(pv, tm) {
      e <- tryCatch({
        rt <- suppressWarnings(survey::regTermTest(fit, tm))
        list(stat = as.numeric(rt$Ftest), df1 = as.numeric(rt$df),
             df2 = as.numeric(rt$ddf), p = as.numeric(rt$p))
      }, error = function(e) NULL)
      if (is.null(e) || is.na(e$p)) return(NULL)
      reg_test_row(types[["wald"]], col_var, pv, e$stat, e$df1, e$df2, e$p, nobs, outcome = outcome)
    }))
  }
  # WARNING: capture.output, not just suppressMessages -- nnet's drop1.multinom PRINTS its progress
  # with cat(), which no condition handler catches.
  d1 <- tryCatch({
    utils::capture.output(
      res <- suppressWarnings(stats::drop1(fit, scope = terms, test = if (use_f) "F" else "Chisq")))
    res
  }, error = function(e) NULL)
  if (is.null(d1)) return(NULL)
  p_col <- grep("^Pr\\(", names(d1), value = TRUE)
  if (!length(p_col)) return(NULL)
  m <- match(terms, rownames(d1))
  purrr::map(seq_along(preds), function(k) {
    j <- m[[k]]
    if (is.na(j)) return(NULL)
    p <- suppressWarnings(as.numeric(d1[[p_col[1]]][j]))
    if (is.na(p)) return(NULL)
    stat <- suppressWarnings(as.numeric(d1[[if (use_f) "F value" else "LRT"]][j]))
    reg_test_row(types[[if (use_f) "f" else "lr"]], col_var, preds[[k]],
        stat, suppressWarnings(as.numeric(d1[["Df"]][j])),
        if (use_f) suppressWarnings(as.numeric(stats::df.residual(fit))) else NA_real_,
        p, nobs, outcome = outcome)
  })
}

# The per-predictor GLOBAL test -- "is this variable associated with the outcome at all?", the
# answer a block of stars against a reference category cannot give. Emitted only for terms carrying
# 2+ coefficients: a 1-df term's global p IS the single cell's p. ⚠ IT DOES REFIT (the unweighted
# drop1() arm), and that is a DECLARED KEEP: the only cheaper route is a Wald test, which is a
# DIFFERENT NUMBER, and this is a test a reader will quote.
#' @keywords internal
reg_global_types <- function() unname(test_row_types("global"))

#' @keywords internal
reg_global_rows <- function(f, sp, shared, col_var) {
  if (!reg_fam_glm(sp$fit_family) || isTRUE(sp$compound)) return(NULL)
  if (is.null(f) || is.null(f$fit)) return(NULL)    # the eager stage always has one; a caller may not
  fit  <- f$fit
  have <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
  asg  <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(asg)) return(NULL)
  df_of <- tryCatch({
    a <- attr(stats::model.matrix(fit), "assign")
    vapply(seq_along(have), function(k) sum(a == k), integer(1))
  }, error = function(e) rep(NA_integer_, length(have)))
  # ⚠ compare on the BARE name and keep `terms_i` verbatim: reg_fit_formula() backticks every
  # predictor, so `terms(fit)$term.labels` reads `` `race x age4` `` for any non-syntactic name and
  # the row silently disappeared -- while drop1()'s scope needs the label exactly as terms() spells
  # it.
  bare    <- reg_cross_term_var(gsub("`", "", have, fixed = TRUE), shared$crosses)
  keep    <- bare %in% c(sp$predictors, sp$row_vars) & !is.na(df_of) & df_of >= 2L
  terms_i <- have[keep]
  if (length(terms_i) == 0L) return(NULL)
  rows <- purrr::compact(reg_term_tests(fit, bare[keep], terms_i,
                                        use_f = reg_fam_disp_estimated(sp$fit_family),
                                        use_wald = reg_fam_svy_fitted(sp$fit_family, shared$weighted),
                                        types = test_row_types("global"),
                                        col_var = col_var, nobs = f$nobs, outcome = sp$outcome))
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}



# Recover a column's per-cell SE, on the estimate's own TEST scale, from the Wald interval it
# stores. ⚠ on a MULTIPLICATIVE scale the SE lives on the LOG, where the gap is measured too.
#
# DESIGN -- TWO decisions, and they are independent. Divide by the critical value that BUILT the
# interval (the column's own `conf_level` / `degf`, stamped by the builder), or the SE comes back
# inflated by t/z on every gaussian, quasi or svyglm column -- +31 % at 5 df. Then TEST with z,
# which is the convention fmt_gap_bounds() / fmt_gap_p() share and the only one open to the
# `adjustment` gap, whose SE is an influence-function sandwich referring to no df at all.
#' @keywords internal
reg_gap_se_of <- function(col) {
  crit <- conf_level_to_crit(get_conf_level(col), get_degf(col))
  lo <- get_ci_inf(col); hi <- get_ci_sup(col)
  if (isTRUE(fmt_scale_row(col)$mult)) {          # a multiplicative scale -> the SE lives on the log
    ok <- is.finite(lo) & is.finite(hi) & lo > 0 & hi > 0
    ifelse(ok, (log(hi) - log(lo)) / (2 * crit), NA_real_)
  } else {
    ifelse(is.finite(lo) & is.finite(hi), (hi - lo) / (2 * crit), NA_real_)
  }
}

# Fill each group's `obs` with the REFERENCE GROUP's estimate for the same row, so `color =
# "between_groups"` reads the per-row effect-modification contrast. ⚠ rows are matched BY KEY (var,
# level), never by position: the compound-formula path builds each GROUP's skeleton from its own
# fit, so a group can have fewer rows in a different order. A key match degrades to NA instead of
# pairing the wrong rows, and the reference group's own cells are NA. The same pass writes `gap_se`:
# the two groups are DISJOINT samples, hence a gap variance of sqrt(SE_i^2 + SE_ref^2) (Altman &
# Bland 2003), recovered from the intervals the table already prints, so test and intervals cannot
# disagree. ⚠ a profile interval is asymmetric -> no SE is written.
#' @keywords internal
reg_write_group_gap <- function(parts, color, method = "wald") {
  if (!"between_groups" %in% color || length(parts) < 2L) return(parts)
  key_of <- function(d) reg_skel_key(as.character(d$var), as.character(d$levels))
  ref_d  <- parts[[1L]]$data                                  # the FIRST split level is the baseline
  ref_k  <- key_of(ref_d)
  fmt_nm <- names(ref_d)[purrr::map_lgl(ref_d, is_fmt)]
  wald   <- !identical(method, "profile")       # asymmetric bounds yield no SE
  est_of <- fmt_est_of
  for (i in seq_along(parts)) {
    d <- parts[[i]]$data
    m <- if (i == 1L) rep(NA_integer_, nrow(d)) else match(key_of(d), ref_k)
    for (nm in intersect(fmt_nm, names(d))) {
      if (!is_fmt(d[[nm]])) next
      # only where a gap measure can READ them: an `Obs_*` companion colours on its own measure.
      # fmt_color_attr, not get_color: a gap usually rides the BACKGROUND channel.
      if (!any(c("adjustment", "between_groups") %in% fmt_color_attr(d[[nm]]))) next
      d[[nm]] <- set_obs(d[[nm]], est_of(ref_d[[nm]])[m])
      if (wald) {
        se_ref <- reg_gap_se_of(ref_d[[nm]])[m]
        d[[nm]] <- set_gap_se(d[[nm]], sqrt(reg_gap_se_of(d[[nm]])^2 + se_ref^2))
      }
    }
    parts[[i]]$data <- d
  }
  parts
}


# The reference GROUP's columns, marked as the reading anchor of `between_groups` -- the twin of the
# crude column `adjustment` marks (reg-empirical.R). Uncoloured by construction (their own `obs` is
# empty), so `refcol` is what says which block the shades beside them are measured from, and
# get_reference()'s gap arm then bolds them whole.
# ⚠ ONLY after the spread: `refcol` merges "same" and falls to its neutral on disagreement, so a
# per-group stamp cannot survive the vec_rbind() -- and in the stacked shape the reference group is a
# block of ROWS, where a reference COLUMN would be a lie. The synthesised `n` column is left alone:
# it carries no estimate to be a baseline for.
#' @keywords internal
#' @noRd
reg_mark_ref_group <- function(tab, ref_level, color) {
  if (!any(vapply(color, function(k) {
    mk <- measure_key(k)
    !is.na(mk) && nzchar(mk) && identical(MEASURES[[mk]]$ref_kind, "group")
  }, logical(1)))) return(tab)
  for (nm in names(tab)) {
    cl <- tab[[nm]]
    if (!is_fmt(cl) || !get_role(cl) %in% c("model", "emp")) next
    if (!identical(get_col_group(cl), as.character(ref_level))) next
    tab[[nm]] <- as_refcol(cl, TRUE)
  }
  tab
}


# THE assembly tail, shared by BOTH branches of reg_build(). A weighted tab_reg() is ALWAYS on the
# weighted basis, so tab()'s design_effect option is never read. ⚠ `basis` is NULL on the split branch
# BY DESIGN: each group stamped its own, and the vec_rbind() reconcile took the weakest.
#
# ⚠ NO `degf` IS STAMPED HERE. On a regression the df is not a table fact: each column carries the df
# ITS OWN interval was referred to (reg_wald_degf()), which is what lets the gap SE be recovered with
# the critical value that built the interval. Stamping a table-wide one would overwrite all of them
# with the design's df -- a number no column used.
#' @keywords internal
reg_finalize <- function(tab, tests, conf_level, var_labels, group_vars, outcomes = character(0),
                         basis = NULL, meta_extra = list()) {
  tab |>
    tab_stamp_inference(conf_level, degf = NULL, basis) |>
    new_tab(subtext = meta_extra$subtext, test = tests,
            meta = c(meta_extra[setdiff(names(meta_extra), "subtext")],
                     list(spec = reg_spec(var_labels, outcomes)))) |>
    dplyr::group_by(!!!rlang::syms(group_vars))
}

# === SECTION: The typed records =================================================================

# THE typed record of every per-call setting reg_build() reads. The idiom, shared with
# new_reg_spec(): the FORMALS are the contract and the body is as.list(environment()), so a direct
# caller omitting a slot gets the declared default, not a missing binding. `tab_vars` stays a formal
# of reg_build() instead: it flips to NULL in the split recursion, and NULL cannot round-trip
# through modifyList().
#' @keywords internal
new_reg_shared <- function(union_predictors = character(0), design_spec = list(), weighted = FALSE,
                           outcome_level = NULL, conf_level = conf_level_default(),
                           method = "wald", color_signif = "grey_non_signif", cleannames = TRUE,
                           subtext = "",
                           stats = NULL, compare = "none", baseline = NULL,
                           multiplier = NULL, multiplier_label = NULL,
                           anchors = NULL, anchor_keyword = NULL,
                           shape_terms = NULL, shape_kinds = character(0), crosses = list(),
                           empirical = FALSE, display = NULL, digits = NULL,
                           var_labels = character(0), na_shared_vars = character(0),
                           levels_order = NULL, base_n = "range") {
  as.list(environment())
}
# ...and THE globalVariables mirror, DERIVED from those formals: reg_build() binds them with
# list2env(), which codetools cannot see, so a hand-kept copy could only fall behind.
utils::globalVariables(names(formals(new_reg_shared)))

# THE typed record of ONE fitted model -- new_reg_shared()'s per-model sibling, same idiom. ⚠
# `fit_family` is the internal LINK key `est$fit` carries ("rr" / "rd" / "mr" included), NOT the
# outcome family, which is `est$family`. `crude_key` is STORED rather than derived: it is a
# five-branch cascade over (fit family, trials, compound) read in six places.
#' @keywords internal
new_reg_spec <- function(outcome = character(0), predictors = character(0), label = "",
                         fit_family = "", trials = NULL, outcome_level = NA_character_,
                         compound = FALSE, formula = NULL, cross = character(0),
                         row_vars = character(0),
                         color = NA_character_, est = NULL, crude_key = NA_character_) {
  # `outcome` arrives NAMED on the comparison branch and unnamed on the per-outcome one, and every
  # downstream map_chr(specs, "outcome") compares it to a bare column name.
  outcome <- unname(outcome)
  as.list(environment())
}
utils::globalVariables(names(formals(new_reg_spec)))

reg_inference <- function(shared, degraded = FALSE) {
  ds <- shared$design_spec
  leaf_inference(new_inference(ds$wt, ds, force = TRUE), degraded = degraded)
}


# === SECTION: reg_build() -- THE STAGED BUILD ====================================================
#
# WHICH STAGE PRODUCED WHICH PART OF THE TABLE. Each stage is named after the part it produces and
# runs over ONE typed context; the per-MODEL half of them is ONE declared product (reg_spec_build(),
# R/reg-spec-build.R), so the stages around the loop are cross-spec ASSEMBLERS and the loop itself is
# dispatchable (see R/tab-parallel.R).
# ⚠ THE STAGE ORDER IS THE SOURCE ORDER, and load-bearing: every fit happens inside reg_stage_specs()
# and may inform or warn, so the message stream is part of the output. It is SPEC-major.

# THE typed context of one build, in new_ctx()'s idiom (R/tab.R): a stage product is DECLARED, never
# left to appear -- an undeclared key is ABSENT, list2env() creates no binding for it, and its own
# is.null() guard therefore ERRORS instead of returning TRUE. DESIGN: `shared` stays ONE nested
# element and is PROJECTED into bare names at each stage head, never flattened -- three consumers
# need the record whole. A build-time assert (R/zzz-fact-keys.R) keeps the two name sets DISJOINT,
# so a projection cannot shadow a product.
#' @keywords internal
#' @noRd
new_reg_ctx <- function(
    # --- INPUTS: reg_build()'s own formals ------------------------------------------------------
    # ⚠ `skeleton_data` is FORCED here: it means the FULL data, so every split group shares one
    # skeleton.
    # ⚠ `fit_cache` is NOT `.fit_cache`: `as.list(environment())` defaults to all.names = FALSE, so a
    # dot-prefixed key is SILENTLY DROPPED. No ctx key may start with a dot.
    data = NULL, specs = list(), shared = list(), tab_vars = NULL, fit_cache = NULL,
    skeleton_data = NULL,
    # --- reg_stage_setup: the skeleton, the table's SHAPE facts and the per-spec PLAN ------------
    family = NA_character_, skeleton = NULL, skeleton_deferred = FALSE,
    compound = logical(0), builders = character(0),
    prefix_dep = FALSE, n_outcomes = 0L, is_comparison = FALSE,
    numeric_preds = character(0), factor_preds = character(0),
    # `spec_plan` is what the builder must be TOLD rather than work out: the de-duplications a worker
    # cannot reproduce, and the one predictor set whose rule is table-scalar.
    spec_plan = list(), want_global = TRUE,
    # --- reg_stage_crude: the ONE observed block of a one-outcome table ---------------------------
    # NULL when the table has several outcomes (each spec builds its own) or no crude companion.
    crude = NULL,
    # --- reg_stage_specs: one new_reg_spec_product() per spec, and the column LAYOUT -------------
    # `built` is the flattened VIEW of the products' `cols`, in order; `labels` their uniquified
    # names; `product_labels` those same names split back per product, which is how every later
    # stage names one of a product's columns without recomputing a position.
    products = list(),
    built = list(), labels = character(0), product_labels = list(),
    fit_first_idx = integer(0), fit_first_col = character(0),
    emp_degraded = FALSE,
    # --- reg_stage_footer: the `test` tibble -----------------------------------------------------
    test = NULL,
    # --- reg_stage_rows: the row axis ------------------------------------------------------------
    tab = NULL, disp_levels = character(0), assumptions = NULL,
    # --- reg_stage_tips --------------------------------------------------------------------------
    empirical_tips = NULL) {
  as.list(environment())
}
# ...and THE globalVariables mirror, DERIVED from those formals: list2env() is invisible to
# codetools.
utils::globalVariables(names(formals(new_reg_ctx)))

#' @keywords internal
#' @noRd
reg_ctx_locals <- function(ctx) c(ctx, ctx$shared)

reg_build <- function(data, specs, shared, tab_vars = NULL, .fit_cache = NULL,
                      skeleton_data = data) {
  shared <- do.call(new_reg_shared, shared[intersect(names(shared), names(formals(new_reg_shared)))])
  ctx <- new_reg_ctx(
    data = data, specs = specs, shared = shared, tab_vars = tab_vars, fit_cache = .fit_cache,
    skeleton_data = skeleton_data,
    family = specs[[1]]$fit_family)
  list2env(reg_ctx_locals(ctx), environment())

  # THE STAGES. Each takes and returns the ctx; only the split recursion returns a finished table.
  # ⚠ the ORDER is the source order and is load-bearing (three of them fit models).
  if (!is.null(tab_vars)) return(reg_stage_split(ctx))
  ctx <- reg_stage_setup(ctx)      # the skeleton, the table's shape facts, the per-spec plan
  ctx <- reg_stage_crude(ctx)      # the observed (crude) block of a ONE-outcome table, built once
  ctx <- reg_stage_specs(ctx)      # ONE reg_spec_build() per model (serial or pooled) + the layout
  ctx <- reg_stage_footer(ctx)     # the products' rows + the between-model comparison -> `test`
  ctx <- reg_stage_rows(ctx)       # the row axis: labels, relabels, sparklines -> `tab`
  ctx <- reg_stage_assemble(ctx)   # the crude blocks and the model columns into `tab`
  ctx <- reg_stage_tips(ctx)       # the crude tooltips (multinomial + numeric)
  reg_stage_finalize(ctx)          # the inference basis, then the shared assembly tail
}


# THE tab_vars RECURSION: fit the SAME model(s) within each level of a grouping variable and STACK
# the per-group tables into one grouped_tab, so tab_spread(tab_vars) can pivot them side by side.
# The groups share ONE skeleton (the full data), so each has identical rows and columns. tab_vars is
# placed FIRST because the index columns DECLARE their roles, so the spread machinery needs no
# change. ⚠ it RETURNS A FINISHED TABLE, not a ctx -- the one early return.
# ONE group-keyed `assumptions` out of the per-group parts: the scalars come from the first (same
# specs, same outcome, so they are identical by construction), the curves are stacked with the group
# they were measured on. `linear_level` is group-independent -- the groups share one skeleton.
#' @keywords internal
#' @noRd
reg_bind_assumptions <- function(parts, sl) {
  aa <- stats::setNames(purrr::map(parts, "assumptions"), as.character(sl))
  aa <- purrr::compact(aa)
  if (length(aa) == 0L) return(NULL)
  # OUTCOME first, group second: `assumptions` is one record per outcome, and each of them stacks
  # its groups' curves.
  deps <- unique(unlist(purrr::map(aa, names)))
  out  <- purrr::compact(stats::setNames(purrr::map(deps, function(dep) {
    ad   <- purrr::compact(purrr::map(aa, dep))
    if (length(ad) == 0L) return(NULL)
    vars <- unique(unlist(purrr::map(ad, ~ names(.x$curves))))
    rec  <- ad[[1]]
    rec$curves <- purrr::map(stats::setNames(nm = vars), function(v)
      purrr::list_rbind(purrr::imap(ad, function(a, g)
        if (is.null(a$curves[[v]])) NULL else dplyr::mutate(a$curves[[v]], group = g))))
    rec
  }), deps))
  if (length(out) == 0L) NULL else out
}

#' @keywords internal
#' @noRd
reg_stage_split <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  sl <- levels(forcats::fct_drop(as.factor(data[[tab_vars]])))
  # ⚠ this axis clears 2x only when the groups are EVEN and the frame is survey-size.
  parts <- tab_pmap(list(g = sl), "reg_build_group",
                    .const = list(sl = sl, tab_vars = tab_vars, specs = specs,
                                  fit_cache = fit_cache),
                    .ship  = list(shared = shared, data = data),
                    .names = as.character(sl),
                    workers = tab_parallel_workers(fit_cache))
  # `color = "between_groups"` scores each group's estimate against the REFERENCE GROUP's, and THIS
  # is the only point where the groups are parallel, separately addressable tibbles: vec_rbind()
  # then stacks them, and after the spread a group survives only in a name suffix. ⚠ the existing
  # reference machinery cannot do it -- fmt_broadcast_last() groups by runs of `in_refrow`, which
  # cross the split boundary.
  color_ms <- unique(unlist(purrr::map(specs, "color")))
  parts <- reg_write_group_gap(parts, color_ms, method = method)
  combined <- vctrs::vec_rbind(!!!purrr::map(parts, "data"))
  tests    <- purrr::list_rbind(purrr::compact(purrr::map(parts, "test")))
  if (is.null(tests) || nrow(tests) == 0) tests <- new_test_tibble()
  # the AGGREGATED companion of the per-cell gap colour, automatic under `color = "between_groups"`.
  # It costs one fit per spec, and this is the ONE place with the full data.
  if ("between_groups" %in% color_ms ||
      (is.character(shared$stats) && "group_interaction" %in% shared$stats)) {
    fit_cols <- unique(tests$col[tests$test %in% reg_footer_test_types()])
    if (length(fit_cols) != length(specs)) fit_cols <- make.unique(purrr::map_chr(specs, "label"))
    tests <- reg_interaction_rows(tests, data, specs, shared, tab_vars, fit_cols)
  }
  # `empirical_tips` is deliberately NOT carried up: a per-GROUP fact with no per-group slot in
  # `meta`, so merging would attach the FIRST group's numbers to every cell. The observed CURVES do
  # carry a group of their own, so they merge honestly -- one row block per group, read back by the
  # base-count cell that belongs to it.
  grouped <- reg_finalize(combined, tests, conf_level, var_labels,
                          group_vars = c(tab_vars, "var"),
                          outcomes = unique(purrr::map_chr(specs, "outcome")),
                          meta_extra = list(subtext = subtext,
                                            assumptions = reg_bind_assumptions(parts, sl)))
  # the groups go side by side whenever that is unambiguous -- ONE model, not multinomial. An
  # internal rule: tab_spread() is the public way to set the layout.
  if (length(specs) == 1L && !identical(family, "multinomial")) {
    return(reg_mark_ref_group(tab_spread(grouped, tidyselect::all_of(tab_vars)), sl[[1L]], color_ms))
  }
  return(grouped)
}


# THE TABLE'S SHAPE, before any model exists: the SKELETON every column is aligned to, the
# whole-table facts, and the PER-SPEC PLAN reg_spec_build() reads.
# The fits could leave this stage because the skeleton is fit-FREE in every shape but one --
# the cascade below, whose ORDER is the contract: only an all-coefficient table with a compound
# formula must read it back off the first fit, which is what `skeleton_deferred` names.
#' @keywords internal
#' @noRd
reg_stage_setup <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  skeleton   <- NULL
  compound   <- purrr::map_lgl(specs, ~ isTRUE(.$compound))
  builders   <- purrr::map_chr(specs, ~ .$est$builder %||% "coef")
  skeleton_deferred <- FALSE
  if (is.null(skeleton)) {
    if (any(builders != "coef"))  skeleton <- reg_skeleton(skeleton_data, union_predictors,
                                                            crosses = crosses)
    else if (any(compound))       skeleton_deferred <- TRUE          # only here: reg_skeleton_from_fit()
    else                          skeleton <- reg_skeleton(skeleton_data, union_predictors,
                                                            shape_terms, crosses)
  }
  # the DISPLAY order, applied to the rows and never to the data (reg_skeleton_reorder). The deferred
  # branch is reordered where its skeleton is born instead, in reg_spec_build_one().
  skeleton <- reg_skeleton_reorder(skeleton, levels_order)

  prefix_dep    <- length(specs) > 1L
  n_outcomes    <- length(unique(purrr::map_chr(specs, "outcome")))
  is_comparison <- length(specs) > 1L && n_outcomes == 1L
  numeric_preds <- reg_numeric_preds(skeleton_data, union_predictors)
  factor_preds  <- reg_factor_preds(skeleton_data, union_predictors)

  # by NAME or by "all", never by default -- the same rule reg_footer_stats() states, and the reason
  # this gate exists at all is that the drop1() sweep must be skipped before it is paid for.
  want_global <- identical(stats, "all") || (is.character(stats) && "global" %in% stats)

  outcomes <- purrr::map_chr(specs, "outcome")
  # THE CRUDE BLOCK BELONGS TO THE OUTCOME: every input is table-wide or per-OUTCOME, so ONE outcome
  # means ONE block built before any model (`want_crude`), while SEVERAL build one per spec
  # (`want_emp`) -- also an outcome, so the work stays on the parallel axis. A numeric predictor
  # gets a crude column from its univariable fit, EXCEPT under a compound formula, where a bare RHS
  # name's term may be an interaction or a basis expansion. ⚠ `any(compound)` is deliberately
  # TABLE-scalar: one compound spec empties this for every block.
  num_e    <- if (any(compound)) character(0) else numeric_preds
  has_pred <- length(factor_preds) > 0L || length(num_e) > 0L
  crude_ok <- !is.na(purrr::map_chr(specs, ~ .$crude_key %||% NA_character_))
  want_emp   <- emp_on(empirical) & has_pred & (n_outcomes > 1L) & crude_ok
  want_crude <- emp_on(empirical) && has_pred && n_outcomes <= 1L && crude_ok[[1L]] &&
    # a deferred skeleton is read off the FIRST fit, so a stage running before them has nothing to
    # align to. Unreachable (see the assert), kept as the stage's statement of what it needs.
    !skeleton_deferred

  # ⚠ THE TWO FACTS reg_stage_crude() RESTS ON, asserted rather than remembered:
  #  (1) with one outcome every spec is built from the same row, so the estimand, family, trials,
  #      crude_key and colour read off specs[[1]] are every spec's;
  #  (2) a deferred skeleton and a crude block cannot co-exist -- `compound` is only ever formula
  #      mode, and reg_crude_key(compound = TRUE) is NA, which turns `empirical` off at the argument
  #      boundary. Without it, `want_emp` not covering spec 1 would silently drop such a block.
  if (length(specs) > 1L && n_outcomes <= 1L) {
    one <- function(f) length(unique(purrr::map(specs, f))) == 1L
    stopifnot(one("outcome"), one("fit_family"), one("trials"), one("crude_key"),
              one(~ .$est$effect), one(~ .$est$measure), one("color"))
  }
  stopifnot(!skeleton_deferred || !emp_on(empirical))

  ctx_update(ctx, list(data = data, skeleton = skeleton,
                        skeleton_deferred = skeleton_deferred,
                        compound = compound, builders = builders,
                        prefix_dep = prefix_dep, n_outcomes = n_outcomes,
                        is_comparison = is_comparison,
                        numeric_preds = numeric_preds, factor_preds = factor_preds,
                        want_global = want_global,
                        spec_plan = list(want_emp = want_emp, want_crude = want_crude,
                                         num_preds = num_e)))
}


# THE OBSERVED (CRUDE) BLOCK OF A ONE-OUTCOME TABLE, built once, before any model: the descriptive
# companion every model column is compared to -- the crude level and effect, their intervals, the
# complete-case frame and the univariable legs the gap test needs. WHY IT IS A STAGE: the block is a
# function of the OUTCOME, not of a model (hence the assert above). ⚠ IT IS FIT-FREE, which is what
# makes it liftable: the two things it once read off the model object have exact producers of their
# own (reg_positive_level(), and reg_crude_yw()'s collapse of an unknown reference category to the
# crude frame's first level).
#' @keywords internal
#' @noRd
reg_stage_crude <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  if (!isTRUE(spec_plan$want_crude)) return(ctx)

  sp     <- specs[[1L]]                     # every spec's, with one outcome (reg_stage_setup assert)
  sp_fam <- sp$fit_family
  inv_sp <- reg_outcome_level_of(sp$outcome_level) %||% outcome_level
  key    <- sp$crude_key
  mdata  <- reg_emp_frame(sp$outcome, ctx)  # the same complete-case frame as the model
  pos    <- if (reg_fam_binary(sp_fam)) reg_positive_level(mdata, sp$outcome, inv_sp) else NULL
  y_ref  <- levels(forcats::fct_drop(as.factor(mdata[[sp$outcome]])))[[1L]]
  var_y  <- if (sp_fam == "gaussian")
    suppressWarnings(stats::var(as.numeric(mdata[[sp$outcome]]), na.rm = TRUE)) else NA_real_

  block <- reg_crude_block(sp, sp_fam, inv_sp, key, mdata, pos, y_ref, var_y, ctx)
  # the crude companions share the model column's outcome col_var -- not in comparison mode.
  if (!is_comparison && length(block$cols)) {
    scv <- reg_shared_col_var(sp_fam, sp$outcome, pos, cleannames, sp$trials)
    block$cols <- purrr::map(block$cols, ~ set_col_var(.x, scv))
  }
  block$tips_num <- reg_spec_tips_num(sp, pos, block, ctx)

  ctx_update(ctx, list(crude = block))
}


#' @keywords internal
#' @noRd
reg_crude_block <- function(sp, sp_fam, inv_sp, key, mdata, pos, y_ref, var_y, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  # ⚠ the two crude predictor sets are TABLE-scalar and come from the declared plan: `num_preds` is
  # emptied when ANY spec has a compound formula, so it is not derivable from `sp` alone.
  fac_preds_e <- factor_preds
  num_preds_e <- spec_plan$num_preds
  emp <- reg_empirical(mdata, fac_preds_e, sp$outcome, key, pos, design_spec$wt,
                       trials = sp$trials, ref_category = y_ref,
                       conf_level = conf_level, design_spec = design_spec)
  # THE FORK, answered once for the whole block: is the closed form the univariable model's own
  # interval? A numeric predictor is never saturated; a factor one stops being so where the SHAPE
  # declares `refit`, or under a design carrying structure the closed form cannot see.
  # Everything that is not saturated is REFIT, through the very fitter the table came from.
  saturated   <- reg_crude_saturated(key, TRUE, design_spec$design,
                                     reg_crude_shape(key, sp$est))
  fit_preds_e <- c(
    num_preds_e, reg_cross_nested_vars(crosses),
    if (!saturated) fac_preds_e else character(0))
  # The crude fits take the FULL `data` + `drop_extra`, never the pre-filtered frame: a prebuilt
  # design's keep mask is computed from `data` itself. `marginal` swaps the crude shape for a
  # marginal one only where the model's estimand is marginal AND on a probability scale.
  fit_e <- reg_empirical_fit(
    data, fit_preds_e, sp$outcome, sp_fam, design_spec,
    outcome_level = inv_sp,
    conf_level = conf_level, method = method, skeleton = skeleton, multiplier = multiplier,
    other_preds = c(union_predictors, reg_cross_parents(crosses)), est = sp$est,
    wt = design_spec$wt,
    want_fit = TRUE, trials = sp$trials,
    shape_terms = shape_terms, crosses = crosses, fit_cache = fit_cache,
    marginal = !identical(sp$est$effect, "conditional") &&
      (reg_fam_binary(sp_fam) || reg_fam_prob(sp_fam)))
  out <- reg_empirical_columns(skeleton, emp, fac_preds_e, key, sp_fam, sp$est, var_y,
                               conf_level = conf_level, color_signif = color_signif,
                               color = sp$color, fit_est = fit_e,
                               weighted = svy_weighted(design_spec, design_spec$wt),
                               degf = design_spec$degf %||% Inf,
                               emp_mode = empirical,
                               saturated = saturated, method = method)
  # the crude columns take the table's own display -- one grammar, and by default the MIRROR layout.
  disp  <- reg_display_of(display, empirical, is_comparison)
  dress <- function(cl) purrr::map(cl, function(col) reg_apply_display(col, disp))
  out$cols     <- dress(out$cols)
  out$cat_cols <- dress(out$cat_cols)
  # the block also carries what the GAP TEST needs; none of it leaves reg_build() (reg_emp_slim()).
  out$frame     <- mdata
  out$fac_preds <- fac_preds_e          # ⚠ live: reg_set_obs() -> reg_gap_se_columns(fac_preds =)
  out$fit_preds <- fit_preds_e
  out$saturated <- saturated
  # ⚠ a served record here has a DIGEST and no `$fit` (reg_empirical_fit -> reg_fit_cached), so
  # reg_gap_se_columns() reads every one of them through reg_model_of().
  out$fits      <- fit_e$fits
  out$grid      <- emp
  out$degraded  <- isTRUE(attr(emp, "degrade"))
  out
}


# A SUMMED-SCORE outcome's level is the mean SCORE, not the per-item share the fit predicts.
# ⚠ EVERY builder needs this, not just the coefficient one.
#' @keywords internal
#' @noRd
reg_scale_pred <- function(marg, trials) {
  if (is.null(marg$pred) || !nrow(marg$pred) || is.na(trials %||% NA)) return(marg)
  marg$pred$pred <- marg$pred$pred * as.numeric(trials)
  marg
}

#' @keywords internal
#' @noRd
reg_cols_ame <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  sp_fam       <- sp$fit_family
  # the header word is COMPOSED here from the resolved estimand, so it cannot disagree with its
  # column.
  sp_eff       <- reg_word(sp$est)
  sp_col       <- sp$color
  prob_scale   <- reg_fam_prob(sp_fam)
  per_category <- isTRUE(sp$est$per_level)
  rank_est     <- identical(sp$est$level, "rank")
  # the contrast asked of the ENGINE and the scale the COLUMN prints both come from the estimand row,
  # and they are two decisions: a `measure = "log_*"` row still wants the log-ratio contrast, and
  # still keeps the log (`exp` FALSE). See reg_marginal().
  sp_est       <- sp$est
  sp_link      <- sp_est$measure_link                  # the REPORTED comparison's, not the fit's
  ratio_ame    <- !identical(sp_link, "identity")
  sp_scale     <- reg_scale_of(sp_est, sp$trials)
  m      <- reg_model_of(f)
  # the way back to a fitted object, for the one engine that needs one (see reg_marginal()).
  refit  <- function() reg_digest_revive(f, data)$fit
  marg  <- reg_marginal(m, f$data, sp$row_vars, conf_level, design_spec$wt,
                        at = if (identical(sp_est$effect, "at_reference")) "reference" else "average",
                        link = sp_link, want_pred = TRUE,
                        comparison = sp_est$comparison,
                        exponentiate = isTRUE(sp_est$exp),
                        multiplier = multiplier, engine = reg_marginal_engine(sp_est),
                        anchors = anchors, crosses = crosses, rank = rank_est,
                        disp_known = f$disp_known, df_residual = f$df_residual, refit = refit)
  marg     <- reg_scale_pred(marg, sp$trials)
  marg_degf <- reg_wald_degf("wald", f$disp_known, f$df_residual)
  # the Constant row: this contrast has no intercept in its tidy, so the baseline is the model's own
  # predicted outcome, at the very profile the column's effects are read at.
  # ⚠ a LOGGED column's baseline is computed on the scale it is the log OF -- the baseline odds under
  # a logged odds ratio, the baseline level under a logged risk / rate ratio -- and logged after, so
  # `Constant + effect` stays coherent on the link scale.
  exp_sc <- reg_exp_scale_of(sp_est, sp$trials)
  # DESIGN: a RANK column has no baseline to place. The model's predicted outcome distribution is a
  # fact about the OUTCOME, not the level this column's effects move away from -- that one is a coin
  # flip, and the reference row already prints it. The distribution goes to the footer instead
  # (reg_model_lines()), where it names a scale rather than pretending to be an intercept.
  const <- if (rank_est) NULL else reg_constant_baseline(
    m, f$data, sp$predictors,
    at = if (identical(sp_est$effect, "at_reference")) "reference" else "average",
    wt = design_spec$wt, conf_level = conf_level,
    scale_key = if (is.na(exp_sc)) sp_scale else exp_sc, log = !is.na(exp_sc),
    anchors = anchors, disp_known = f$disp_known, df_residual = f$df_residual)
  marg_add <- if (!ratio_ame) marg
    else if (is.null(m)) NULL
    else reg_scale_pred(reg_fill_sweep(m, f$data, sp$row_vars, conf_level,
                                       design_spec$wt, multiplier, crosses = crosses), sp$trials)
  disp  <- reg_display_of(display, empirical, is_comparison)
  # ⚠ the LEVELS come from this column's own sweep (`marg`, at its own profile); `marg_add` only
  # supplies the additive fallback where the column reports a ratio and a numeric predictor has no
  # level pair. Reading the levels off `marg_add` would put the sample-averaged prediction beside an
  # at-reference estimate.
  dress <- function(col, group = NULL)
    reg_apply_display(reg_fill_base(col, marg, skeleton, sp$row_vars, group = group,
                                    add = marg_add, crosses = crosses), disp)
  # a summed score is prob-scale by family, but its additive effect is a difference of mean SCORES.
  var_y <- if (!prob_scale || !is.na(sp$trials %||% NA))
    suppressWarnings(stats::var(as.numeric(f$data[[sp$outcome]]))) else NA_real_
  if (per_category) {                            # one AME column per OUTCOME category (all levels)
    groups <- levels(as.factor(f$data[[sp$outcome]]))
    cv_cat <- reg_category_col_var(sp, is_comparison, f$positive_level, cleannames)
    purrr::map(groups, function(g) {
      jc  <- reg_cleanup(g, cleannames)
      lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "", jc)
      list(label = lab, emp_key = g,   # emp_key: raw category, for the empirical tooltip
           col   = dress(reg_marginal_column(skeleton, marg, sp$row_vars, sp_scale,
                                             var_y, g, sp_col, color_signif, cv_cat,
                                             model_family = sp_fam,
                                             trials = sp$trials, const = const,
                                             degf = marg_degf), g))
    })
  } else {
    or_tip <- if (sp_fam == "binomial" && !ratio_ame) {
      # The fit's own coefficients, exponentiated. ⚠ `tidy_native` is NATIVE -- per ONE unit -- so
      # the scaling is applied here: without it this hover would read a per-unit odds ratio beside a
      # row labelled "per 3.08 (2SD)". reg_tidy_finalize() does the same for every printed column.
      td <- reg_tidy_rescale(f$tidy_native, multiplier)
      exp(td$estimate[match(skeleton$term, td$term)])
    } else NULL
    cv <- if (is_comparison) sp$label
          else reg_shared_col_var(sp_fam, sp$outcome, f$positive_level, cleannames, sp$trials)
    list(list(
      label = reg_model_col_name(sp_eff, sp$outcome, is_comparison, sp$label, n_outcomes),
      col   = dress(reg_marginal_column(skeleton, marg, sp$row_vars, sp_scale,
                                        var_y, NA_character_, sp_col, color_signif,
                                        cv, or_tip = or_tip, model_family = sp_fam,
                                        trials = sp$trials, const = const,
                                        degf = marg_degf))))
  }
}


#' @keywords internal
#' @noRd
reg_cols_vsrest <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  sp_fam <- sp$fit_family
  sp_col <- sp$color
  sp_scale <- reg_scale_of(sp$est, sp$trials)
  exp_sc   <- reg_exp_scale_of(sp$est, sp$trials)
  m      <- reg_model_of(f)
  refit  <- function() reg_digest_revive(f, data)$fit
  marg   <- reg_marginal(m, f$data, sp$row_vars, conf_level, design_spec$wt,
                         at = "reference", link = sp$est$measure_link,
                         comparison = sp$est$comparison, want_pred = FALSE,
                         exponentiate = isTRUE(sp$est$exp),
                         engine = reg_marginal_engine(sp$est), anchors = anchors,
                         crosses = crosses,
                         disp_known = f$disp_known, df_residual = f$df_residual, refit = refit)
  marg_add <- if (is.null(m)) NULL else
    reg_scale_pred(reg_fill_sweep(m, f$data, sp$row_vars, conf_level, design_spec$wt,
                                  crosses = crosses), sp$trials)
  const  <- reg_constant_baseline(m, f$data, sp$predictors, at = "reference",
                                  wt = design_spec$wt, conf_level = conf_level,
                                  scale_key = if (is.na(exp_sc)) sp_scale else exp_sc,
                                  log = !is.na(exp_sc), anchors = anchors,
                                  disp_known = f$disp_known, df_residual = f$df_residual)
  groups <- levels(as.factor(f$data[[sp$outcome]]))
  cv_cat <- reg_category_col_var(sp, is_comparison, f$positive_level, cleannames)
  purrr::map(groups, function(g) {
    jc  <- reg_cleanup(g, cleannames)
    lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "", jc, " vs rest")
    col <- reg_marginal_column(skeleton, marg, sp$row_vars, sp_scale,
                               NA_real_, g, sp_col, color_signif, cv_cat,
                               model_family = sp_fam, const = const,
                               degf = reg_wald_degf("wald", f$disp_known, f$df_residual))
    col <- reg_fill_base(col, marg_add, skeleton, sp$row_vars, group = g, crosses = crosses)
    list(label = lab, col = reg_apply_display(col, reg_display_of(display, empirical, is_comparison)))
  })
}


#' @keywords internal
#' @noRd
reg_cols_coef <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  sp_fam   <- sp$fit_family
  sp_eff   <- reg_word(sp$est)                  # composed from the estimand, see cols_ame above
  sp_col   <- sp$color
  # the coefficient path runs no marginal sweep of its own, so it runs the one reg_fill_base() needs.
  model_predictors <- if (isTRUE(sp$compound)) unique(skeleton$var)
                     else unique(c(sp$predictors, sp$row_vars))
  marg <- reg_fill_sweep(reg_model_of(f), f$data, sp$row_vars, conf_level, design_spec$wt,
                         multiplier, crosses = crosses)
  marg <- reg_scale_pred(marg, sp$trials)
  disp  <- reg_display_of(display, empirical, is_comparison)
  dress <- function(col, group = NULL)
    reg_apply_display(reg_fill_base(col, marg, skeleton, model_predictors, group = group,
                                    crosses = crosses), disp)
  if (sp_fam == "multinomial") {
    cols <- reg_columns_multinom(skeleton, f, sp, sp$est, sp_col, color_signif,
                                 cleannames, prefix_dep,
                                 col_var = reg_category_col_var(sp, is_comparison,
                                                                f$positive_level, cleannames),
                                 model_family = sp_fam, method = method)
    return(purrr::map(cols, function(lc) { lc$col <- dress(lc$col, lc$emp_key); lc }))
  }
  cv  <- if (is_comparison) sp$label
         else reg_shared_col_var(sp_fam, sp$outcome, f$positive_level, cleannames, sp$trials)
  col <- reg_column(skeleton, f, model_predictors, cv, sp$est, sp_col, color_signif,
                    model_family = sp_fam, method = method, trials = sp$trials)
  list(list(label = reg_model_col_name(sp_eff, sp$outcome, is_comparison, sp$label, n_outcomes),
            col = dress(col)))
}


# ONE reg_spec_build() PER MODEL, and the column LAYOUT their products imply. SERIAL OR POOLED:
# reg_specs_independent() is the ONE predicate -- NULL when a spec needs nothing from another, else
# the reason, reported only when parallel was actually asked for, so what was not parallelised
# is never silent. Its two reasons are exactly what rides the serial branch: the crude block spec 1
# shares with the compared models, and the skeleton read back off the first fit.
#' @keywords internal
#' @noRd
reg_stage_specs <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  why     <- reg_specs_independent(ctx)
  wanted  <- tab_parallel_workers(fit_cache)
  workers <- if (is.null(why)) wanted else 0L
  # ⚠ only when parallel was actually asked for: it must not nag on every comparison.
  if (!is.null(why) && wanted > 1L)
    cli::cli_inform(c("i" = "The models are built one after another: {why}."))

  if (workers > 1L) {
    # ⚠ the whole ctx is the shipped object -- data, skeleton, design and crude block -- sent ONCE.
    products <- tab_pmap(list(i = seq_along(specs)), "reg_spec_build",
                         .ship = list(ctx = ctx), .names = purrr::map_chr(specs, "label"),
                         workers = workers)
  } else {
    # ⚠ the ONE serial unit-loop that does not go through tab_pmap(), so it needs the BLAS pin here
    # or a serial specs build would not match the parallel one bit for bit (see local_blas_threads).
    local_blas_threads(1L)
    products <- vector("list", length(specs))
    for (k in seq_along(specs)) {
      products[[k]] <- reg_spec_build(k, ctx)
      if (k == 1L && isTRUE(skeleton_deferred))
        ctx <- ctx_update(ctx, list(skeleton = products[[1]]$skeleton))
    }
  }
  # ⚠ AFTER both branches, not inside the serial one: a table with ONE compound spec defers its
  # skeleton but has nothing to share, so it takes the pooled branch, where the loop-carried update
  # above never runs.
  if (isTRUE(skeleton_deferred)) skeleton <- products[[1]]$skeleton

  built  <- purrr::flatten(purrr::map(products, "cols"))
  labels <- make.unique(purrr::map_chr(built, "label"))

  # `fit_first_idx` = each product's OFFSET into `built`, which the assembler and the tooltips index
  # from. ⚠ every model owns at least one column and the LAYOUT depends on it: two models sharing an
  # offset would collide in the assembler's match(), silently dropping the second's crude block.
  fit_ncol      <- purrr::map_int(products, ~ length(.x$cols))
  if (any(fit_ncol == 0L)) cli::cli_abort("A model produced no column.", .internal = TRUE)
  fit_first_idx <- cumsum(c(1L, utils::head(fit_ncol, -1L)))
  # `product_labels[[k]]` = the UNIQUIFIED names product k's own columns ended up with, in order.
  # DESIGN: everything downstream keys a column BY LABEL through this, never by a position computed
  # before the product was assembled -- step 6b of reg_spec_build_one() PREPENDS a crude column, so an
  # index taken earlier points one column too far left. That silently keyed the whole footer, and the
  # multinomial crude tooltips, onto the `Obs_*` column.
  product_labels <- purrr::map2(fit_first_idx, fit_ncol, ~ labels[.x + seq_len(.y) - 1L])
  # the footer keys each fit's GOF to its MODEL column -- the column the numbers belong to, whatever
  # crude companions were spliced in beside it.
  fit_first_col <- purrr::map2_chr(products, product_labels, function(p, lb) {
    m <- which(purrr::map_chr(p$cols, ~ get_role(.x$col)) == "model")
    lb[[if (length(m)) m[[1L]] else 1L]]
  })

  ctx_update(ctx, list(products = products, skeleton = skeleton,
                        built = built, labels = labels, product_labels = product_labels,
                        fit_first_idx = fit_first_idx, fit_first_col = fit_first_col,
                        emp_degraded = any(purrr::map_lgl(products, ~ isTRUE(.x$degraded))) ||
                          isTRUE(crude$degraded)))
}


# THE `test` TIBBLE, from the products plus the one footer producer that is BETWEEN models. ⚠
# SLOT-MAJOR, not product-major: GOF rows, comparison, global rows, checks -- the order a
# verification script compares. ⚠ reg_compare_rows() STAYS here: a test BETWEEN two fitted models
# needs the fit OBJECTS, which is why `compare != "none"` is reg_specs_independent()'s first
# refusal.
#' @keywords internal
#' @noRd
reg_stage_footer <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  # every per-fit row is rekeyed onto that fit's MODEL column: the spec wrote a placeholder before
  # make.unique() ran, and the crude columns spliced in beside it are not what the gof describes.
  rekey <- function(slot) {
    rows <- purrr::compact(purrr::map(seq_along(products), function(k) {
      r <- products[[k]][[slot]]
      if (is.null(r) || nrow(r) == 0L) return(NULL)
      r$col <- fit_first_col[[k]]
      r
    }))
    if (length(rows) == 0L) NULL else dplyr::bind_rows(rows)
  }

  reg_gof <- rekey("gof_rows") %||% new_test_tibble()
  reg_gof <- reg_compare_rows(reg_gof, purrr::map(products, "fit"), specs, family, weighted = weighted,
                              fit_first_col = fit_first_col, compare = compare, baseline = baseline,
                              crosses = crosses)
  gl <- rekey("global_rows"); if (!is.null(gl)) reg_gof <- dplyr::bind_rows(reg_gof, gl)
  ck <- rekey("check_rows");  if (!is.null(ck)) reg_gof <- dplyr::bind_rows(reg_gof, ck)
  cx <- rekey("cross_rows");  if (!is.null(cx)) reg_gof <- dplyr::bind_rows(reg_gof, cx)

  ctx_update(ctx, list(test = reg_gof))
}


#' @keywords internal
#' @noRd
reg_stage_rows <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  disp_levels <- reg_cleanup(skeleton$level, cleannames)
  # The Constant row says WHICH baseline it holds, which is a property of the CONTRAST: a
  # sample-averaged table's is the population, every other one's is the reference profile. A row
  # label cannot be per column, so a table mixing contrasts takes the profile reading -- the one
  # that is true of the anchored intercept the other columns show.
  cst <- skeleton$var == "Constant"
  if (any(cst)) {
    effs <- vapply(specs, function(sp) sp$est$effect %||% "conditional", character(1))
    disp_levels[cst] <- if (all(effs == "marginal")) gettext("Population average")
                        else gettext("Reference profile")
  }

  # A CONTINUOUS PREDICTOR'S LEVEL IS ITS UNIT, composed once so no clause can overwrite another:
  # "log(x), per 10 (SD), at 42.4 (mean)". The `var` column already names the variable, so the row
  # states only what a reader needs to place the effect -- the transform it was fitted through, the
  # step it is per, and the origin the Constant row and every interacted term sit at.
  # Keyed on the LINEAR term, so a curved predictor's squared row claims neither unit nor anchor.
  lin      <- !is.na(skeleton$term) & skeleton$term == skeleton$var
  num_rows <- skeleton$var %in% numeric_preds & lin
  # ⚠ every lookup is `[v]`, never `[[v]]`: each of the four is a NAMED vector holding only the
  # predictors it applies to, and `[[` on an absent name is an error rather than NA.
  one <- function(x, v) { y <- unname(x[v]); if (length(y) && !is.na(y)) y else NULL }
  unit_of  <- function(v, anchor = TRUE) {
    mult <- one(multiplier_label, v)
    anch <- if (anchor) one(anchors, v) else NULL
    parts <- c(one(SHAPE_MARKS[unname(shape_kinds[v])], 1L),
               if (!is.null(mult)) gettextf("per %s", mult),
               if (!is.null(anch)) {
                 kw  <- one(unlist(anchor_keyword), v) %||% ""
                 num <- format(signif(anch, 3), scientific = FALSE)
                 gettextf("at %s", if (nzchar(kw)) paste0(num, " (", reg_anchor_word(kw), ")")
                                   else num)
               })
    paste(parts, collapse = ", ")
  }
  for (v in intersect(numeric_preds, as.character(skeleton$var))) {
    hit <- num_rows & skeleton$var == v
    if (!any(hit)) next
    u <- unit_of(v)
    if (nzchar(u)) disp_levels[hit] <- u
  }
  # A CROSSED slope's rows are the moderator's LEVELS, so the modified variable and its unit open
  # each of them: the block names the pair, the row says whose slope this is and within which group.
  # No anchor clause -- a nested slope does not depend on it, and the row is long enough.
  for (rec in crosses) {
    if (!identical(rec$arm, "nested")) next
    hit <- skeleton$var == rec$var
    if (!any(hit)) next
    u <- unit_of(rec$modified, anchor = FALSE)
    disp_levels[hit] <- paste0(rec$modified, if (nzchar(u)) paste0(" ", u),
                               reg_cross_row_sep(), disp_levels[hit])
  }

  # THE OBSERVED SHAPE of each continuous predictor, stored as a CURVE and drawn at display time
  # into the row's empty base-count cell (materialize_specs()$reg_spark). Fit-free.
  # ⚠ drawn on the GROUP's own `data`, not on the shared `skeleton_data`: a per-group curve used to
  # be impossible because the glyphs lived in the row LABEL, and vec_rbind(), tab_spread()'s pivot
  # and reg_write_group_gap()'s reg_skel_key() all key on (var, levels) -- two groups whose labels
  # differed would double the pivot's rows and break the gap match. Out of the label, out of the key.
  # `linear_level` is the row each curve belongs to: `shape = "quadratic"` gives a predictor TWO rows
  # and `skeleton$term` does not exist at display time, so the linear one must be named here.
  # the modelled level PER OUTCOME (each product carries its own), so a two-outcome table draws each
  # curve of the level its own column reports.
  pos_lv <- stats::setNames(purrr::map(products, "positive_level"),
                            purrr::map_chr(specs, "outcome"))
  pos_lv <- pos_lv[!duplicated(names(pos_lv))]
  assumptions <- reg_curves(data, specs, numeric_preds, design_spec$wt,
                            positive_level = pos_lv, design = design_spec$design)
  assumptions <- purrr::map(assumptions %||% list(), function(a) {
    a$curves <- purrr::map(a$curves, ~ dplyr::mutate(.x, group = ""))
    a$linear_level <- purrr::map_chr(
      stats::setNames(nm = names(a$curves)),
      function(v) { hit <- lin & skeleton$var == v
                    if (any(hit)) disp_levels[which(hit)[[1]]] else NA_character_ })
    # HOW THE SHAPE TABLE NAMES THE CURVE. A `log`/`sqrt` shape recoded the column IN PLACE, so the
    # curve drawn below is a curve of log(age) labelled `age` unless the mark travels with it. NA for
    # every other kind, straight from VAR_SHAPES$mark -- which is what makes `quadratic` unmarked a
    # DERIVED fact and not a clause: it adds a term, so the observed curve is genuinely unchanged.
    # Stored beside the curves, never ON them: names(a$curves) are keys (see reg_shape_table()).
    a$mark <- purrr::map_chr(stats::setNames(nm = names(a$curves)),
                             function(v) shape_mark(unname(shape_kinds[v]), v))
    a
  })
  if (length(assumptions) == 0L) assumptions <- NULL

  tab <- tibble::tibble(
    var    = new_lvl(forcats::fct_inorder(skeleton$var), "var"),
    levels = new_lvl(forcats::fct_inorder(disp_levels) , "level")
  )

  ctx_update(ctx, list(tab = tab, disp_levels = disp_levels, assumptions = assumptions))
}


# The per-outcome complete-case frame the crude companions and the tooltips share with the model,
# RECOMPUTED from `data` rather than read off a fit's own frame. `na_shared_vars`
# is the same extra-completeness set reg_fit() receives, so under the default it IS the model's own.
#' @keywords internal
#' @noRd
reg_emp_frame <- function(dep, ctx) {
  s <- ctx$shared
  reg_complete_frame(ctx$data, c(dep, s$union_predictors, s$na_shared_vars,
                                 reg_design_vars(s$design_spec)))
}


# ONE model column's `obs` (what it is compared to, on its own scale) and the `gap_se` between them
# -- or NEITHER, when the two estimators are not the same estimand on the same people. `f` / `sp`
# are the fit and spec this COLUMN came from, NOT the crude block's: in comparison mode one block
# serves several models. `key` is the column's OWN outcome category -- a multinomial needs one
# counterpart per column.
#' @keywords internal
#' @noRd
reg_set_obs <- function(bi, e, f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  col <- bi$col
  # the "may a crude value be attached?" decision is PER SPEC (the estimand's declared `obs`).
  if (is.null(e) || !reg_estimand_obs(sp$est)) return(col)
  if (!reg_same_estimand(e$shape, get_scale(col), sp$est)) return(col)  # same estimand, or nothing
  # ...and the same PEOPLE, or nothing: otherwise the "gap" is listwise deletion, not adjustment.
  if (!reg_same_frame(e$frame, f)) return(col)
  key <- if (is.null(bi$emp_key)) "" else as.character(bi$emp_key)
  ev  <- cat_get(e$effect, key)
  if (is.null(ev)) return(col)
  # `obs` is on the CELL'S OWN SCALE, so it is the same kind of quantity as the estimate and IS what
  # `color = "adjustment"` scores. WHERE it is shown is the display's business, never this function's
  # (`empirical = "cell"` resolves to the `est_obs` preset in reg_display_of).
  col <- set_obs(col, ev)
  g <- reg_gap_se_columns(f, sp, col, skeleton, e$shape, e$frame,
                          e$fac_preds, sp$est, design_spec$wt,
                          fits_crude = e$fits, fit_preds = e$fit_preds, multiplier = multiplier,
                          category = key, crosses = crosses, saturated = e$saturated)
  if (is.null(g)) col else set_gap_se(col, g)
}


#' @keywords internal
#' @noRd
reg_add_emp_cols <- function(tab, cols, suffix) {
  for (nm in names(cols)) {
    out_nm <- if (nzchar(suffix)) paste0(nm, " [", suffix, "]") else nm
    tab[[out_nm]] <- cols[[nm]]
  }
  tab
}


#' @keywords internal
#' @noRd
reg_stage_assemble <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  if (n_outcomes <= 1L) {
    if (!is.null(crude)) tab <- reg_add_emp_cols(tab, crude$cols, "")
  } else {
    for (i in seq_along(built)) {
      # ⚠ match(), not fit_of_col: it is non-NA exactly at a fit's FIRST column, which is where that
      # fit's crude block belongs.
      fi <- match(i, fit_first_idx)
      if (!is.na(fi) && !is.null(products[[fi]]$emp))
        tab <- reg_add_emp_cols(tab, products[[fi]]$emp$cols, specs[[fi]]$outcome)
      tab[[labels[i]]] <- built[[i]]$col
    }
    return(ctx_update(ctx, list(tab = tab)))
  }
  for (i in seq_along(built)) tab[[labels[i]]] <- built[[i]]$col

  ctx_update(ctx, list(tab = tab))
}


# `meta$empirical_tips`, from the products' fragments. The two blocks that produce them live in
# reg_spec_build(), because both read the crude block's HEAVY halves -- the grid and the
# complete-case frame -- which must not travel back from a worker. What arrives is keyed by SKELETON
# ROW and, for the multinomial fragment, by the within-spec column's own LABEL, resolved here
# through `product_labels`. ⚠ SLOT-MAJOR, the order the two blocks ran in.
#' @keywords internal
#' @noRd
reg_stage_tips <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  mnl <- purrr::compact(purrr::map(seq_along(products), function(k) {
    fr <- products[[k]]$tips$mnl
    if (is.null(fr) || nrow(fr) == 0L) return(NULL)
    tibble::tibble(col   = product_labels[[k]][match(fr$col_label,
                                                    purrr::map_chr(products[[k]]$cols, "label"))],
                   var   = fr$var,
                   level = disp_levels[fr$row],
                   tip   = fr$tip)
  }))
  # ⚠ the numeric fragments come from the BLOCKS, not the specs: one per outcome -- with a single
  # outcome reg_stage_crude()'s one block, with several each spec's own.
  num <- purrr::compact(purrr::map(c(list(crude$tips_num), purrr::map(products, ~ .x$tips$num)),
                                   function(fr) {
    if (is.null(fr) || nrow(fr) == 0L) return(NULL)
    tibble::tibble(col = fr$col, var = fr$var, level = disp_levels[fr$row], tip = fr$tip)
  }))

  rows <- c(mnl, num)
  ctx_update(ctx, list(empirical_tips = if (length(rows)) purrr::list_rbind(rows) else NULL))
}


#' @keywords internal
#' @noRd
reg_stage_finalize <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  # The confidence level and the basis are stamped on EACH fmt column -- the colour engine is per
  # column and cannot read a table attribute. The df is NOT: every column already carries its own.
  reg_inf <- reg_inference(shared, emp_degraded)
  # the per-token half of `digits`, post-hoc over every fmt column at once (see reg_resolve_digits)
  dg  <- digits %||% list(floor = 0L, tokens = integer(0))
  tab <- reg_digits_write(tab, dg$floor %||% 0L, dg$tokens %||% integer(0))
  out <- reg_finalize(tab, test, conf_level, var_labels, group_vars = "var",
                      outcomes = unique(purrr::map_chr(specs, "outcome")),
                      basis = reg_inf$basis,
                      meta_extra = list(subtext = subtext, empirical_tips = empirical_tips,
                                        assumptions = assumptions))
  # the base count is a DISPLAY intent here exactly as in tab(): the column is synthesised at
  # print/export time from the `n` every model column carries.
  set_render_extras(out, list(n = shared$base_n))
}



# === Public API =====================================================================

#' Regression table (effect measures) as a tabxplor table
#'
#' Fits one regression model per column and returns a `tabxplor` table of the per-family effect
#' measure --- a linear **mean difference** (gaussian), **odds ratios** (binomial), **incidence-rate
#' ratios** (poisson), one **odds-ratio column per outcome category** (nominal 3+ level), a
#' **cumulative odds ratio** (ordinal) --- one row per predictor level, grouped by predictor, with
#' the **observed (crude)** effect beside each adjusted one. Each cell stores its estimate, interval
#' and p-value, so the table prints with stars, greys what is not significant, and exports like any
#' `tabxplor` crosstab.
#'
#' To **learn** what such a table says, read
#' \href{https://bricenocenti.github.io/tabxplor/articles/tabxplor-all-else-equal.html}{All else
#' equal} (`vignette("tabxplor-all-else-equal")`); to **look something up**, the
#' \href{https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html}{regression vignette}
#' (`vignette("tabxplor-reg")`).
#'
#' @details
#' New to regressions with tabxplor? A first model needs three arguments: `data`, `outcome` and
#' `predictors`. The model follows the outcome's type --- a two-level factor gives logistic **odds
#' ratios**, a numeric a linear **mean difference**, a count Poisson **rate ratios**, a 3+ level
#' factor multinomial or ordinal odds ratios --- so you rarely set `family` by hand.
#'
#' **The estimand is a cascade**: `family` -> `link` -> `measure` -> `effect`, where `"auto"` means
#' *follow from the left*. `family` says what kind of number the outcome is; `link` **which measure
#' the model estimates** (a link *is* a measure, so it takes the same words); `measure` **which one
#' is reported**; `effect` where that number comes from. Set any one and everything to its right
#' re-derives, so most tables set none of them --- and the one most people ever set is `measure`.
#'
#' A **coefficient** exists only where the reported measure IS the model's own. Ask for another and
#' it is worked out from the model's predictions instead, averaged over the sample. So
#' `measure = "ratio"` on a binary outcome gives a *marginal* risk ratio from the logistic fit, while
#' `link = "ratio"` fits the modified Poisson and gives its *conditional* one --- two different
#' quantities, and now two different arguments.
#'
#' @param data A data frame, **or a prebuilt survey design** ([survey::svydesign()]). A design's
#'   weights, clustering, stratification and calibration drive the estimation, and `wt` is ignored.
#' @param outcome <[`tidy-select`][tidyr::tidyr_tidy_select]> The outcome variable(s) --- bare
#'   names, quoted names, or any selection helper, exactly as in [tab()] --- **or a model formula**
#'   (the escape hatch; leave `predictors` unset). Several names give one effect column per outcome;
#'   with a `predictors` list, a single name is required. [reg_formulas()] shows what was fitted.
#' @param predictors <[`tidy-select`][tidyr::tidyr_tidy_select]> The predictors of one model --- or a
#'   **named list**, one model per element, its name labelling the column, each element selected on
#'   its own (`list(m1 = c(race, age), m2 = starts_with("inc"))`), which is how models are compared.
#'   Leave `NULL` when `outcome` is a formula. A bare name is a column of `data` first, then an
#'   object, so a variable holding names works without `all_of()`.
#'
#'   **`a*b` is an interaction**, R's own spelling, bare or quoted --- *"a's effect, allowed to vary
#'   with b"*. Two categorical variables give one row per **cell** of the pair; a continuous `a`
#'   gives its **slope within each level** of `b`. An interaction supplies both its variables, so do
#'   not list them beside it, which is what makes "with and without" an ordinary model comparison.
#'   `a:b`, which drops the main effects, is refused. See
#'   \href{https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html}{the regression
#'   vignette}.
#' @param family The model family, **resolved per outcome** so several outcomes with different
#'   families can share one table. `"auto"` (default) detects each one and says so: a binary outcome
#'   gives `"binomial"`, an ordered 3+ level `"ordinal"`, a nominal 3+ level `"multinomial"`, any
#'   other numeric `"gaussian"` --- a genuine count is yours to name. Or set it: `"gaussian"`
#'   (linear), `"binomial"` (logistic), `"poisson"` / `"quasipoisson"` (counts),
#'   `"multinomial"`, `"ordinal"`. A **scalar** applies to every outcome; a **vector** aligned to
#'   `outcome`, or a **named** vector keyed by outcome (e.g. `c(income = "poisson", satisfied =
#'   "binomial")`), sets one family per outcome. Mixed families need a character `predictors`.
#'
#'   It answers **one** question --- what kind of number the outcome is --- and never picks a link
#'   behind your back: on a binary outcome `family = "poisson"` is refused, naming the two things it
#'   could have meant, `link = "ratio"` and `measure = "ratio"`.
#' @param link **Which measure the model estimates** --- the only argument that changes the model.
#'   A link *is* a measure (the one a model gives you directly), so it takes `measure`'s own words:
#'
#'   * `"auto"` (default) --- the family's own: logistic for a binary outcome, linear for a
#'     quantity, Poisson for a count.
#'   * `"odds_ratio"` --- the logit fit (binomial, multinomial, ordinal).
#'   * `"ratio"` --- the log link: the **modified Poisson** on a binary outcome (a conditional risk
#'     ratio), Poisson on a count, Poisson pseudo-likelihood on a continuous one.
#'   * `"difference"` --- the identity link; on a binary outcome the **risk difference**. It can
#'     fail to converge, and the linear probability model then takes over, with a message.
#'
#'   Reach for it when you want the model's *coefficient* to be that measure; to report a measure
#'   without changing the model, set `measure` instead. ⚠ `"log"` is the one word the two arguments
#'   do not share: here the **log link**, on `measure` a spelling of `"raw_coefficient"`.
#' @param measure **Which measure of deviation is reported** --- a deviation being how far a group
#'   sits from the reference, the measure which of the ways of expressing it you read. The one
#'   argument most readers ever set, and the one that never changes the model. `"auto"` (default) is
#'   the model's own. The full word is canonical, the discipline's acronym a synonym:
#'
#'   * `"odds_ratio"` (`"OR"`) --- the odds of the outcome, times what.
#'   * `"ratio"` (`"RR"`, `"IRR"`, `"RoM"`) --- how many times as likely, as frequent, as large.
#'     Reach for it when the outcome is **common**, where an odds ratio is far from the risk ratio
#'     people hear in it, and because a risk ratio stays comparable across nested models.
#'   * `"difference"` (`"RD"`, `"diff"`) --- how much more, in the outcome's own units.
#'   * `"raw_coefficient"` (`"coef"`, `"log"`, ...) --- the model's own coefficient, un-transformed.
#'
#'   On an **ordered** outcome the first three read the whole predicted distribution rather than one
#'   category, so they stay in one column: Somers' `D` and the win ratio. Where the measure IS the
#'   model's own it is read off the coefficients, otherwise from its predictions --- so it is
#'   available whichever model you fit. ⚠ `"auto"` never lands on a **predicted odds ratio**, a
#'   specialist quantity to be asked for by name. Call [reg_measures()] to see what an outcome
#'   offers.
#' @param effect **Where the reported number comes from**, once the model and the measure are fixed.
#'   `"auto"` (default) needs no thought: the model's own coefficients when the reported measure is
#'   the model's, its predictions otherwise. The other values name a reading:
#'
#'   * `"conditional"` --- read off the coefficients ("holding the other predictors constant").
#'     Only where `measure` is the model's own; otherwise the abort names the two cures.
#'   * `"marginal"` --- the **average marginal effect**: the model's prediction for every observed
#'     person, averaged. Comparable across models (Mood 2010), and always available.
#'   * `"at_reference"` --- the same at **one profile**, every other predictor at its reference.
#'
#'   The contrast is a **marker on the measure** in the header, so the acronym stays the one thing
#'   to look up: `Model_OR`, `Model_mRR`, `Model_refRD`. The observed companion carries the measure
#'   alone (`Obs_RR`), a univariable effect having no adjustment to be marginal over.
#' @param wt <[`tidy-select`][tidyr::tidyr_tidy_select]> Optional. One weight column, switching to
#'   design-based survey estimation ([survey::svyglm()]). For clustering, stratification, a
#'   finite-population correction or calibration, build the design with [survey::svydesign()] and
#'   pass it as `data`. See `vignette("tabxplor-weights")`.
#' @param trials Grouped-binomial (summed-score) outcomes only. The number of items behind the score,
#'   fitting `cbind(score, trials - score)` as a binomial. `NULL` (default) fits an ordinary binary
#'   logit; an integer (or a vector named by outcome) sets the item count; `TRUE` uses each
#'   outcome's observed maximum. Requires `family = "binomial"`.
#' @param conf_level Confidence level for the intervals. `NULL` (default) reads
#'   `options(tabxplor.conf_level)` --- 0.95. It drives every interval, the significance stars, the
#'   greying under `color_signif` and the model-versus-observed gap interval, and each column
#'   records the level it was built at.
#' @param ref The reference every effect is measured **from** --- one argument, one meaning per kind
#'   of predictor.
#'   For a **factor** it is the level the others are compared against (a level name, or `"first"`
#'   (default) / `"last"`); for a **continuous** predictor the value it is **anchored** at (a number,
#'   or `"mean"` (default), `"median"`, `"min"`, `"max"`).
#'
#'   Anchoring a continuous predictor **does not change its own effect** --- a slope is the same
#'   wherever you start reading it from --- but it does move the **Constant** row; its own row says
#'   where the anchor sits, `per SD/13.5 (at mean/42.4)`. The default is the mean because zero is
#'   usually outside the data: nobody is 0 years old.
#'
#'   `ref`, `multiplier` and `shape` share one grammar: a value **on its own** is the default for
#'   every predictor it can apply to, a **named** one overrides that variable ---
#'   `ref = c("median", "last", race = "Black")`.
#'
#'   For the level of the **outcome**, see `outcome_level`: `ref` names the level you compare
#'   AGAINST, `outcome_level` the one you MODEL.
#' @param outcome_level Which level of the **outcome** to single out, as a named vector keyed by
#'   outcome name --- `outcome_level = c(married = "Married")`. It is the twin of `ref`: **`ref`
#'   names the level you compare AGAINST, `outcome_level` the one you MODEL.**
#'   \itemize{
#'     \item **binomial**: the level whose probability is estimated; it becomes the column header.
#'       Defaults to the outcome's **first** level. A 0/1 numeric outcome may be named either way.
#'     \item **multinomial**: the baseline category the other categories' columns are compared to.
#'     \item **ordinal and numeric outcomes**: refused, with the reason.
#'   }
#' @param tab_vars <[`tidy-select`][tidyr::tidyr_tidy_select]> Optional. One grouping variable ---
#'   the same argument as [tab()]'s: one sub-table per group, the same model(s) fitted **within each
#'   level**. Two readings of "does this effect hold in every subgroup?" come with it:
#'   `color = "between_groups"` colours and tests each effect against the first group's, row by row,
#'   and `stats = c(..., "group_interaction")` adds the aggregated test, once per predictor. For an
#'   interaction between two PREDICTORS of one model, write it in `predictors` as `a*b` instead.
#' @param multiplier How a **continuous** predictor's effect is scaled --- the unit its row reports.
#'   One unit is rarely a readable amount (a one-year change in `age` barely moves the odds), so the
#'   default is **two standard deviations**: roughly the span a binary predictor's own contrast
#'   covers, which is what makes a continuous row and a factor row comparable at a glance
#'   (Gelman 2008). Values: `"2sd"` (default), `"sd"`, or a number of units (`10` = per decade).
#'   Same grammar as `ref`: `multiplier = c("sd", age = 10)`.
#'
#'   The estimate, its interval and the observed companion all scale together; the p-value does not
#'   move. ⚠ **because the default is not 1, a continuous predictor's `Model_*` cell does not equal
#'   `exp(coef(glm(...)))` unless you pass `multiplier = 1`.**
#' @param shape How a **continuous** predictor enters the model, when one straight line is not
#'   enough. The `Linearity` footer row and the little curve drawn in the predictor's `n` cell tell
#'   you *whether* a line is enough; this argument is how you fix it without leaving the framework.
#'   Same grammar as `multiplier` and `ref` --- `shape = "quintiles"` cuts every continuous
#'   predictor, `shape = c(age = "quadratic")` only that one, and anything unnamed stays linear:
#'   \describe{
#'     \item{`"linear"`}{one slope (the default).}
#'     \item{`"quintiles"` / `"quartiles"` / an integer `k`}{cut into `k` quantile groups, so the
#'       predictor becomes an ordinary **factor**: one estimate per group, its own observed
#'       companion, counts and colours per group --- the non-linearity becomes visible in the printed
#'       numbers. Start here; it is the most readable answer.}
#'     \item{`"sd_bands"`}{cut at the **mean and one standard deviation either side** --- the
#'       classic low / average / high reading, whose cut points mean the same thing across
#'       sub-samples, where quantiles move with each one. The bands are not balanced: prefer
#'       quantiles when the group sizes matter.}
#'     \item{`"quadratic"`}{adds a curvature term, so the predictor takes **two rows** --- the slope
#'       at the mean, and whether it flattens or accelerates away from it.}
#'     \item{`"log"` / `"sqrt"`}{fit `log(x)` / `sqrt(x)` instead of `x` --- diminishing returns.}
#'   }
#'   Example: `shape = c(age = "quadratic", income = "log")`. The observed companion is fitted with
#'   the same shape, so the comparison stays like with like. It is the vocabulary [tab()] takes
#'   ([shape_numeric_var()]) plus `"quadratic"`, which is a model term.
#' @param empirical Show the **observed, unadjusted (crude)** effect beside each modelled one ---
#'   the same quantity fitted with a single predictor, on exactly the same people. The distance
#'   between the two is what adjustment changed, read left to right; it is the feature the package
#'   exists for. `TRUE` (**the default**) or `FALSE`; three spellings say *where* it goes, and in
#'   every one but `"no"` it is stored in the `obs` field and read by `$obs`,
#'   `color = "adjustment"`, [forest_plot()] and the hover tooltip:
#'   \itemize{
#'     \item `TRUE` --- a crude **column** beside the model one, except where that would double a
#'       table already wide (`tab_vars` groups, a 3+ level outcome), which take `"tooltip"`.
#'     \item `"column"` --- always the column, per outcome category if that is what it takes.
#'     \item `"tooltip"` --- computed, printed nowhere. The narrowest table.
#'     \item `"cell"` --- **inside** the model cell, `(1/1.69) 1/1.63***`. `display` overrides it.
#'   }
#'   The two columns are the same column twice: same estimand, same colour ladder, one legend block.
#'   Each cell prints the effect with the level it sits on --- the observed percentage or mean on the
#'   crude side, the **adjusted** prediction on the model side. ⚠ a **continuous** predictor has no
#'   levels, so its crude cell is the univariable slope, which assumes linearity: check that with
#'   `shape` first.
#' @param n How many people the table is about. `NULL` (default) reads `options(tabxplor.n)` ---
#'   `"range"`, which adds an `n` column holding the **unadjusted count** behind each predictor
#'   level, on the model's own complete cases. Where several models rest on different people it
#'   prints the whole range (`5 139-9 862`), so an unequal base cannot pass unnoticed; `"min"` shows
#'   the smallest count only, `"no"` none. Continuous predictors are left blank: their count is the
#'   model N, the first footer row.
#' @param stats The statistics shown in the model-summary **footer** (one block per model).
#'   `"auto"` (default) uses the per-family set --- R square, adjusted R square, the overall F-test
#'   and the residual SD for a linear model, the likelihood-ratio test against the null model,
#'   McFadden's pseudo-R square, AIC and BIC otherwise --- plus the default **model checks** (see
#'   below). A weighted model shows the survey-appropriate set. Pass a character vector to pick
#'   them: `"n"`, `"lr_null"`, `"mcfadden_r2"`, `"aic"`, `"bic"`, `"phi"`, `"r2"`, `"r2_adj"`,
#'   `"f_model"`, `"sigma"`, `"global"`, `"interaction"`, `"group_interaction"`, `"linearity"`,
#'   `"proportionality"`, `"dispersion"`, `"influence"`, `"collinearity"`; `"all"` for everything,
#'   or `NULL` / `FALSE` / `"no"` to hide the footer.
#'
#'   **Model comparison happens by default** wherever it means anything --- when `predictors` is a
#'   list of several models. Where each nests in the next, every model is tested against the
#'   previous one; otherwise each against the first. `"compare_sequential"` and `"compare_baseline"`
#'   (optionally naming the model) override that, and naming any footer statistic drops it.
#'
#'   Three tests are asked for by name: `"global"`, one **overall test per predictor** --- "is this
#'   variable associated with the outcome at all?", which a block of stars against a reference
#'   category cannot answer; `"interaction"`, whether each **crossed pair** in `predictors` is real
#'   or the additive model is enough; and `"group_interaction"`, one aggregated
#'   **effect-modification** test per predictor across `tab_vars` groups.
#'
#' @section Model checks:
#'
#' Beside the fit statistics the footer carries five **model checks**, each naming an assumption and
#' the instrument that measured it: **Linearity** and **Proportionality** (p-values) say whether the
#' estimate means what it claims, **Dispersion** whether the intervals are wide enough,
#' **Influence** whether one respondent carries the result, **Collinearity** why the intervals are
#' wide. Four are shown by default; `stats = "linearity"` adds the fifth, and `shape` is the cure
#' for what it flags. [reg_check_plots()] draws them all. What each one asks, and how to read it:
#' \href{https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html}{the regression
#' vignette}.
#'
#' @param display What each effect cell shows --- [tab()]'s display grammar, same names, same
#'   meaning, on every family and on the crude column as well as the model one. `NULL` (default)
#'   shows the plain estimate, or, with `empirical`, the estimate with the level it sits on beside
#'   it. The whole vocabulary --- the named layouts, the `{}` templates and the per-token precision
#'   `"{est:3} ({base:1})"` --- is in [tabxplor-display]; the ones this table uses most are
#'   `"est_ci"` (`1/2.22*** [1/2.47;1/1.99]`), `"est_base"` (`1/2.22 (32.8%)`), `"est_coef"` and
#'   `"base_est_mdiff"` / `"base_est_mratio"`, which read the same comparison the other way. The
#'   **Constant** row holds the quantity the column's effects operate on, so it is read in one
#'   step: a baseline *odds* on an odds-ratio column, the level itself on an additive one.
#'
#'   `display` is **post-hoc**: every quantity it can name is already stored, so choosing a layout
#'   never triggers a computation and never changes a number --- [set_display()] on a built table
#'   gives the same result. It never changes the estimand, which is `measure`'s job alone.
#' @param color,color_signif Colouring of the effect cells. `color = "measure"` (default, `TRUE`
#'   equivalently) grades each cell on **its own measure**, so the ladder follows what the column
#'   estimates; `color = FALSE` turns colouring off. `color_signif` is the significance policy ---
#'   `NULL` (default) is `"grey_non_signif"` here, where [tab()] defaults to `"ignore"`.
#'
#'   What is left to choose is what each effect is compared **to**. `color` is positional,
#'   `c(text, background)`, so `color = c("measure", "adjustment")` answers "how strong is this
#'   effect?" and "how much did the model change it?" in one glance:
#'
#'   * `"adjustment"` --- how far each **modelled** effect sits from its **observed** (crude)
#'     counterpart: what adjusting for the other predictors did to it. It turns `empirical = TRUE`
#'     on. Set its thresholds with [set_color_breaks()] (`adj_ratio`, `adj_diff`, `adj_diff_std`).
#'   * `"between_groups"` --- with `tab_vars`, how far each group's effect sits from the **first**
#'     group's: a per-predictor reading of effect modification.
#'
#'   The two are mutually exclusive, and each always tests its own gap: a gap whose interval covers
#'   zero is greyed whatever `color_signif` says --- so a cell can be filled while neither estimate
#'   carries a star, which is correct rather than odd. ⚠ a conditional **odds ratio** is not
#'   collapsible, so there the colours stay descriptive and are not tested. Read a coloured cell as
#'   "adjustment moved this effect", not as "this variable is a confounder". See
#'   \href{https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.html}{the regression
#'   vignette}.
#' @param stars Logical (default `TRUE` for regression tables, where significance stars are
#'   standard). When `FALSE`, the per-cell p-value is dropped and no stars are shown (colours still
#'   read the interval).
#' @param na Which rows each model is fitted on --- the grain at which missing values are dropped.
#'   `"drop_by_outcome"` (default) gives every model **of one outcome** the same complete-case
#'   population, which is what makes the comparisons honest: the observed columns are computed on
#'   exactly the model's rows, and nested models get equal N. `"drop_by_model"` lets each model use
#'   its own complete cases --- more rows, at the price of comparability. `"drop_all"` shares one
#'   population across the whole call.
#'
#'   `"keep_for_predictors"` drops nothing but a missing **outcome**: every predictor keeps its
#'   missing values as an ordinary `NA` level, with its own row, count and effect --- often the
#'   fastest way to find out whether non-response is itself patterned. A number has no level to put
#'   them in, so a numeric predictor that has any is cut into bands.
#' @param digits The number of decimals. A single integer sets every cell (`0`, the default, means
#'   "each measure's own"), and a measure finer than the level it sits on keeps its own precision.
#'   Name a display field to set just that one, an aside included --- `digits = c(ratio = 3)`,
#'   `digits = c(1, or = 3)`; a template may carry its own, `display = "{est:3} ({base:1})"`.
#' @param cleannames Logical. If `TRUE`, strips numeric prefixes from factor levels for display.
#'   Uses `getOption("tabxplor.cleannames")` when `NULL`.
#' @param subtext Optional character. A note shown below the table.
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one effect column per model / outcome.
#'
#' @seealso [reg_formulas()] shows the formula each column was fitted with, and [reg_measures()]
#'   what an outcome can be modelled as.
#'   [forest_plot()] draws the finished table --- every effect with its interval, its stars
#'   and its colour, and (with `empirical = TRUE`) the observed effect beside it with the margin of
#'   error of the gap. [reg_check_plots()] draws the model checks. [tabxplor-display] says what a
#'   cell can show, [tab()] builds cross-tables.
#'
#' @examples
#' # The shape table a continuous predictor draws under the footer is noise in an example:
#' .opt <- options(tabxplor.shape_table = "no")
#'
#' # Logistic: the odds of being released, adjusted, beside the observed (crude) odds ratio.
#' tab_reg(car_arrests, "released", c("colour", "checks"))
#'
#' \donttest{
#' # Linear: a mean difference in dollars.
#' tab_reg(car_salaries, "salary", c("sex", "discipline", "rank"))
#'
#' # A count outcome: incidence-rate ratios.
#' tab_reg(car_arrests, "checks", c("colour", "employed"), family = "poisson")
#'
#' # `measure` reports another measure WITHOUT changing the model: a MARGINAL risk ratio,
#' # averaged over the sample, still from the logistic fit.
#' tab_reg(car_arrests, "released", c("colour", "checks"), measure = "ratio")
#'
#' # `link` changes the model: the CONDITIONAL risk ratio of a modified-Poisson fit.
#' tab_reg(car_arrests, "released", c("colour", "checks"), link = "ratio")
#'
#' # A named list of predictor sets: one column per model, compared in the footer.
#' tab_reg(car_salaries, "salary",
#'         list("sex alone" = "sex",
#'              "+ field"   = c("sex", "discipline"),
#'              "+ rank"    = c("sex", "discipline", "rank")),
#'         empirical = FALSE)
#'
#' # A continuous predictor cut into groups, on French survey data:
#' tab_reg(questionr_hdv, "cinema", c("qualif", "age"), shape = c(age = "quartiles"))
#' }
#' options(.opt)
#'
#' @section Out of scope:
#' `tab_reg()` covers linear, logistic, Poisson, multinomial and ordinal models, with survey designs.
#' Three families of models are deliberately **not** supported, and are unlikely to be: **survival /
#' Cox** models, **mixed / multilevel** models, and pooling over **multiply-imputed** datasets. Fit
#' those with their own packages.
#'
#' @references
#' Clogg, C. C., Petkova, E. & Haritou, A. (1995). Statistical Methods for Comparing Regression
#' Coefficients between Models. *American Journal of Sociology*, 100(5), 1261-1293 --- the
#' comparison `color = "adjustment"` implements.
#'
#' Zou, G. (2004). A Modified Poisson Regression Approach to Prospective Studies with Binary Data.
#' *American Journal of Epidemiology*, 159(7), 702-706 --- `link = "ratio"`.
#'
#' Altman, D. G. & Bland, J. M. (2003). Interaction revisited: the difference between two estimates.
#' *BMJ*, 326, 219 --- the `color = "between_groups"` test.
#'
#' @param ... One rarely-typed argument, plus internal plumbing.
#'
#'   `ci_method` --- how the interval and p-value are computed: the same argument, and the same
#'   named-vector grammar, as in [tab()], whose fifth slot is this producer's. `"wald"` (default)
#'   matches standard software output and is the only option for weighted models; `"profile"` uses
#'   the profile-likelihood interval and the likelihood-ratio test --- more accurate near
#'   separation, unweighted binomial / poisson only.
#'
#'   Every argument removed or renamed while `tab_reg()` was in development is still accepted here,
#'   and gives an error naming its replacement rather than R's bare "unused argument". The
#'   dot-prefixed names are the jamovi live-cache plumbing, not user arguments.
#' @eval reg_words_rd()
#' @export
tab_reg <- function(data, outcome, predictors = NULL, tab_vars = NULL, wt = NULL,
                    family = "auto", link = "auto", measure = "auto", effect = "auto",
                    outcome_level = NULL, trials = NULL, empirical = TRUE, n = NULL,
                    color = "measure", color_signif = NULL, stars = TRUE,
                    ref = NULL, multiplier = "2sd", shape = NULL, stats = "auto",
                    conf_level = NULL,
                    na = c("drop_by_outcome", "drop_by_model", "drop_all", "keep_for_predictors"),
                    display = NULL, digits = 0, cleannames = NULL, subtext = "", ...) {
  # ⚠ FIRST: capture the four variable roles before anything can force their promises -- and the
  # EXPRESSION `data` was written as, which is how reg_check_plots() finds the microdata again
  # without the user naming it twice (only a bare name is ever re-resolved; see reg_plot_fits()).
  data_expr      <- paste(deparse(substitute(data), width.cutoff = 500L), collapse = "")
  outcome_quo    <- rlang::enquo(outcome)
  predictors_quo <- rlang::enquo(predictors)
  tab_vars_quo   <- rlang::enquo(tab_vars)
  wt_quo         <- rlang::enquo(wt)
  # `.fit_cache` (the jamovi live-UI cache env), `.levels_collapse` (the level-merge spec) and
  # `.levels_order` (the per-predictor DISPLAY order) are jamovi-internal plumbing riding `...`; none
  # is a user argument. ⚠ `.levels_order` reorders the ROWS, never the data: see
  # reg_skeleton_reorder().
  .dots      <- list(...)
  .fit_cache <- .dots[[".fit_cache"]]
  .levels_collapse <- new_lvl_collapse(.dots[[".levels_collapse"]])
  .levels_order    <- .dots[[".levels_order"]]
  tab_check_dots(.dots, "tab_reg")
  # the declared arguments that ride `...` rather than the signature, refilled from their own
  # TAB_ARGS default when absent (`dots = "tab_reg"` is where that is stated).
  ci_method <- tab_dots_expand(.dots, "tab_reg")[["ci_method"]]
  # ONE `ci_method` grammar for both producers: the named vector tab() takes, whose fifth slot is
  # this one's. A bare "profile" means that slot -- a regression has only one interval to choose for.
  if (is.character(ci_method) && is.null(names(ci_method)) && length(ci_method) == 1L)
    ci_method <- stats::setNames(ci_method, "model")
  method <- resolve_ci_method(ci_method, fn = "tab_reg")[["model"]]
  # ⚠ the un-supplied default is the whole vector, so its LENGTH is "the user did not choose" --
  # read before match.arg() collapses it, so the `na` advice fires on a choice and not on the
  # default it would advise.
  na_explicit <- length(na) == 1L
  na      <- match.arg(na)
  cleannames <- resolve_cleannames(cleannames)

  # --- THE VARIABLE ROLES: one tidy-select grammar, tab()'s (R/tab.R, tidy_select_chr) ------------
  # Resolved HERE -- after tab_check_dots(), so a mistyped argument still says "unknown argument"
  # rather than dying in tidyselect, and BEFORE the multi-outcome recursion below, which reads
  # values. ⚠ svy_select_frame() and NOT svy_unwrap_data(): the unwrap informs, adds the reserved
  # columns and computes degf, and it must run exactly once -- inside the boundary (R/reg-resolve.R).
  sel_data   <- svy_select_frame(data, "tab_reg")
  outcome    <- reg_select_outcome(outcome_quo, sel_data)
  predictors <- reg_select_predictors(predictors_quo, sel_data)
  tab_vars   <- reg_select_one(tab_vars_quo, sel_data, "tab_vars")
  wt         <- reg_select_one(wt_quo, sel_data, "wt")
  if (!rlang::is_formula(outcome) && length(outcome) == 0L)
    cli::cli_abort(c("{.arg outcome} is required.",
                     "i" = "Name one or more columns, or pass a model formula, e.g. {.code y ~ x1 + x2}."),
                   call = NULL)


  # A models LIST and SEVERAL outcomes -> one model-comparison table per outcome, returned as a
  # `tabxplor_tabs` list (so tab_export("xl") writes one sheet each). The outcomes loop on the
  # OUTSIDE and each iteration recurses into the ordinary single-outcome comparison, so every
  # argument, message and family detection is reused. It sits BEFORE the design extraction, so a
  # survey design recurses intact.
  #
  # ⚠ `!rlang::is_formula(outcome)` is load-bearing: a two-sided formula is a call of length 3, so
  # `length(outcome) > 1L` is TRUE for every one of them, and without this guard a formula recurses
  # over `~`, its lhs and its rhs instead of reaching the teachable error below.
  if (!rlang::is_formula(outcome) && is.list(predictors) &&
      !inherits(predictors, "formula") && length(outcome) > 1L) {
    if (!is.null(trials) && !isTRUE(trials) && is.null(names(trials)) &&
        length(trials) > 1L && length(trials) != length(outcome)) {
      cli::cli_abort(c("{.arg trials} must be length 1, one per outcome, or a named vector.",
                       "x" = "Got {length(trials)} for {length(outcome)} outcomes."))
    }
    # Every per-outcome argument is SLICED the way `trials` is, through the one slicer
    # (reg_per_outcome()); every whole-call one is forwarded. The recursion itself is the namespaced
    # reg_build_outcome(), so this map IS tab_pmap() -- the cleanest of the three parallel axes,
    # each unit returning a FINISHED table with no cross-unit step at all.
    # THE NESTING RULE needs nothing here: tab_pmap() turns the option off around its whole map,
    # serial branch included, so a recursed unit cannot dispatch again.
    args <- purrr::map(seq_along(outcome), function(i) {
      d   <- outcome[[i]]
      tri <- if (is.null(trials) || isTRUE(trials)) trials
             else if (!is.null(names(trials)))      unname(trials[d])
             else if (length(trials) == 1L)         as.numeric(trials)
             else                                   trials[[i]]
      list(outcome = d, predictors = predictors, wt = wt,
           family  = reg_per_outcome(family,  d, i, "auto"),
           link    = reg_per_outcome(link,    d, i, "auto"),
           measure = reg_per_outcome(measure, d, i, "auto"),
           effect  = reg_per_outcome(effect,  d, i, "auto"),
           trials = tri, conf_level = conf_level, ci_method = ci_method,
           ref = ref, outcome_level = outcome_level,
           tab_vars = tab_vars, multiplier = multiplier, shape = shape,
           empirical = empirical, n = n,
           stats = stats,
           display = display, color = color, color_signif = color_signif,
           stars = stars, na = na, cleannames = cleannames, subtext = subtext,
           .fit_cache = .fit_cache,
           .levels_collapse = .levels_collapse, .levels_order = .levels_order)
    })
    tabs <- tab_pmap(list(args = args), "reg_build_outcome", .ship = list(data = data),
                     .names = outcome,
                     workers = tab_parallel_workers(.fit_cache))
    names(tabs) <- outcome
    return(new_tabxplor_tabs(tabs))
  }

  # THE argument boundary, in one call (R/reg-resolve.R): six stages in the one order they may run
  # in, every check and every rewrite of `data` among them.
  a <- reg_resolve_args(
    data, outcome, predictors, tab_vars = tab_vars, wt = wt,
    family = family, link = link, measure = measure, effect = effect, trials = trials,
    empirical = empirical, n = n, color = color, color_signif = color_signif,
    stars = stars, conf_level = conf_level, method = method, ref = ref,
    outcome_level = outcome_level, multiplier = multiplier,
    shape = shape, stats = stats,
    na = na, na_explicit = na_explicit, display = display, digits = digits, cleannames = cleannames,
    subtext = subtext, levels_collapse = .levels_collapse, levels_order = .levels_order)

  res <- reg_build(a$data, a$specs, a$shared, tab_vars = tab_vars,
                   .fit_cache = .fit_cache)

  # The p-value is stars-only -- colours read the CI bounds -- so `stars = FALSE` just drops it.
  if (!isTRUE(stars)) {
    for (nm in names(res)[vapply(res, is_fmt, logical(1))]) {
      res[[nm]] <- set_pvalue(res[[nm]], NA_real_)
    }
  }

  # THE TABLE'S OWN MODEL RECORD: what drives the title / caption, the "Model:" footer lines and the
  # colour legend -- the table-level narrative only, since the legend reads each column's own
  # `model_family` attribute for the per-column word. It stores the ESTIMAND (`measures` and
  # `effects` per outcome) because a table must remember what it estimated, or a refit silently
  # changes it. Every field is READ OFF the boundary's record rather than recomputed here.
  reg_call_record <- list(
    family = a$families[[1]], families = a$families,
    link = a$est$link, effect = a$est$effect, measure = a$est$measure, eff_word = a$eff_word,
    links    = vapply(a$ests, function(e) e$link,    character(1)),
    measures = vapply(a$ests, function(e) e$measure, character(1)),
    effects  = vapply(a$ests, function(e) e$effect,  character(1)),
    outcome = a$outcome, positive_level = a$positive_levels, predictors = a$union_predictors,
    # ⚠ the predictor-kind map is STORED, never re-derived from the rendered table: the only implicit
    # marker (a numeric row's `level == var`) is already broken by `cleannames` and by the multiplier
    # relabel. `multiplier` records the RESOLVED scaling used, frozen SDs included, so the footer and
    # legend can name the unit.
    predictor_types = reg_predictor_types(a$data, a$union_predictors), multiplier = a$multiplier,
    outcome_scale = reg_outcome_scale(a$data, a$outcome, a$families),
    # THE RECIPE reg_check_plots() refits from: the specs plus the few scalars reg_fit() takes, a few
    # KB of strings. Deliberately NOT the fits -- they are megabytes each, and a refit through the
    # very fitter the table came from is both cheaper and impossible to drift from.
    # ⚠ the RESOLVED conf_level, read back off the boundary record: the formal is NULL on every
    # producer, so tab_reg()'s own local is still NULL here.
    fit_spec = list(specs = a$specs, method = method, conf_level = a$shared$conf_level,
                    outcome_level = outcome_level,
                    na_shared_vars = a$na_shared_vars, shape_terms = a$shape_terms,
                    crosses = a$crosses,
                    # THE preparation recipe (the column recodes + the anchors), so a refit from the
                    # user's raw data reproduces the very model the table shows.
                    prep = a$prep, data_expr = data_expr,
                    multiplier = a$multiplier, link = a$est$link,
                    effect = a$est$effect, measure = a$est$measure,
                    wt = a$wt_disp, design_vars = reg_design_vars(a$design_spec)),
    # which observed counterpart each outcome has (NA = none), and where it went -- stored, with the
    # LAYOUT, so the footer can word whatever the cell's bracket actually holds.
    emp_mode = a$empirical, display = a$display,
    crude_keys = if (emp_on(a$empirical))
      stats::setNames(purrr::map_chr(a$specs, ~ .$crude_key), purrr::map_chr(a$specs, "outcome"))
      else stats::setNames(rep(NA_character_, length(a$specs)),
                           purrr::map_chr(a$specs, "outcome")),
    # THE DESIGN's own df (#PSU - #strata). A table-level fact, so it lives here and not on a column:
    # every column now carries the df ITS OWN interval was referred to, which is a different number.
    design_degf = a$design_spec$degf %||% NA_real_,
    tab_vars = tab_vars, comparison = a$is_comparison, wt = a$wt_disp
  )
  # The model record IS this table's `spec$call` -- "how was this table made", the slot every
  # producer has. ⚠ `conf_level` is deliberately absent from it: the level lives on every COLUMN
  # (get_conf_level() is what consumers read), so a table-wide copy could only ever disagree.
  set_reg_call(res, reg_call_record)
}
