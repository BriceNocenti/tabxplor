# PURPOSE: Regression tables (effect measures) as native tabxplor_tab objects.
# ROLE: tab_reg() fits one model per column across families and renders the per-family effect measure
#   -- gaussian beta (additive), binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR
#   (multiplicative) -- through the tabxplor_fmt diff|or / ci_inf|ci_sup / pvalue / var fields, so a
#   regression table prints, colours and exports (kable / md / Excel) exactly like a crosstab.
#   tab_logit()/multi_logit() are thin binomial-family wrappers with the curated binary-outcome UX.
# KEY CONSTRAINTS:
#   - Direct engine: stats::lm/glm (unweighted) / survey::svyglm (weighted) + nnet::multinom (nominal
#     3+ level) + MASS::polr (ordinal 3+ level), all tidied with broom::tidy. No parsnip.
#   - broom (always), survey (wt path), MASS (ordinal + method="profile"), nnet (multinomial), brant
#     (ordinal PO diagnostic) are Suggests -> guarded.
#   - CI <-> p are DUALS (CI <-> stars can never disagree). method="wald" (default): in-house Wald
#     CI (coef +/- crit*se, exp()'d for ratio measures) + the model's own Wald p; crit is z for
#     fixed-dispersion glm (binomial/poisson), t(df.residual) for lm / quasi* / weighted svyglm --
#     matching broom's z/t p exactly. method="profile" (unweighted glm): confint + LR-test p.
#   - Effect shape is driven by `exponentiate` (default "nongaussian"): MULTIPLICATIVE (OR/IRR) ->
#     the `or` field, type="row", display="or", ci_type="or", color="OR" (neutral 1, 1/x reciprocal);
#     ADDITIVE (gaussian beta / log-odds) -> the `diff` field, type="coef", display="coef",
#     ci_type="diff", color="diff" (neutral 0), with `var`=var(Y) so the colour is the effect-size
#     beta/SD(Y) against the mean_diff (Cohen) breaks -- the additive twin of OR-coloured-by-ratio.
#   - 12c-ii: `trials` fits a summed-score outcome as GROUPED binomial (cbind(score, trials-score));
#     a model FORMULA in `dependent` is the escape hatch -- a simple `y ~ a + b` reduces to the
#     dependent+predictors path, a compound one (interactions / poly() / I()) is fit verbatim with a
#     best-effort skeleton read from the fitted terms (reg_skeleton_from_fit).
#   - 12d: nominal 3+ level -> ONE multinom -> reg_build splits its `y.level` tidy into one OR column
#     per non-reference category ("<j> vs <ref>: OR"); the outcome baseline is set by `reference`
#     keyed on the dependent. Ordered 3+ level -> polr -> one cumulative-OR column (cut-point rows
#     dropped -> "Constant" NA), with a Brant PO diagnostic (reg_ordinal_diagnostic, self-heals the
#     fit's $call so brant works out of the fitting scope). Both reuse the OR fmt shape unchanged;
#     both share reg_wald_from_tidy so CI <-> p <-> stars stay exact duals. Weighted MNL/ordinal
#     deferred (guarded error).
#   - 12e-i: effect="ame" (marginaleffects Suggests, guarded) is the orthogonal interpretation axis --
#     sample-average marginal effects + adjusted predictions on the RESPONSE scale. reg_marginal()
#     wraps avg_comparisons()/avg_predictions() (newdata = the fitted frame is REQUIRED); a factor AME
#     is keyed by (var, level) from the "Level - Reference" contrast label. reg_marginal_column()
#     composes them AME-first via the {} display grammar: prob-scale families (binomial/MNL/ordinal)
#     get type="row" + "{diff} ({pct})" (reference level -> "({pct})", numeric -> "diff"); gaussian/
#     poisson get the raw type="coef". MNL/ordinal -> one AME column per outcome CATEGORY (all levels).
#     No new fmt fields/attributes/tokens; effect="coefficient" byte-identical.
#   - 12e-ii: the `at` profile axis. at="reference" evaluates at the REFERENCE PROFILE (other predictors
#     at their reference = factor first level / numeric mean) via marginaleffects::datagrid ->
#     comparisons()/predictions() (a single row, no averaging/weights): for effect="ame" the
#     marginal effect at reference (MER) + the adjusted prediction there; for a MULTINOMIAL
#     effect="coefficient" the "j vs rest" OR at the profile (comparison="lnor" -> exp, the `or` shape,
#     one column per outcome category). reg_marginal_column() gained the "or" shape. `at` no-ops on
#     ordinary coefficients (profile-independent -> message).
#   - 12f: the model-summary FOOTER (GOF stats) + multi-model comparison. reg_glance() computes N /
#     LR-vs-null / McFadden R2 / AIC / BIC (glm+MNL+ordinal), N / R2 / adjR2 / F / sigma (lm), a
#     Pearson-dispersion flag (poisson / grouped binomial), and a reduced survey set (svyglm: Wald /
#     Nagelkerke / AIC). reg_gof_tibble() stores them in the whole-table `test` attribute keyed by the
#     model column, with reg-specific discriminators DISJOINT from the crosstab "chi2"/"F_*" (so the
#     footer is invisible to the crosstab renderers and vice versa). reg_compare_rows() adds a
#     model-comparison row (compare = baseline / sequential; anova LR, F for lm/quasi; Delta-AIC + a
#     message on non-nesting / N-mismatch). The footer is DISPLAY-ONLY (R/tab_classes.R:
#     summary console block, reg_footer_lines export rows); the built object stays the
#     coefficient skeleton. `stats=` picks the set (FALSE hides it). No new fmt fields; ONE new display
#     token "gof" (a plain model-fit number, forced uncoloured).
#   - 12g: SURVEY designs + companion features. `wt` (+ optional ids/strata/fpc/nest) builds a
#     survey::svydesign per model (reg_make_design); a PREBUILT survey.design / svyrep.design passed as
#     `data` is subset()'d per model (reg_subset_design / reg_resolve_design) -- design-based, no weight
#     normalisation. reg_svyglm_env() binds survey::svyglm into the fit's formula env so AIC.svyglm /
#     anova.svyglm work when survey is loaded but not attached. Weighted 3+ level lifted: ordinal ->
#     survey::svyolr, nominal -> svyVGAM::svy_vglm (Suggests). Weighted glance = the reduced survey set
#     (n / Wald-vs-null / Nagelkerke [+ selectable Cox-Snell] / Rao-Scott AIC); weighted comparison =
#     anova.svyglm Wald (compare_*_wald). `split_var` = the tab_vars analogue: reg_build recurses per
#     group on a SHARED skeleton (skeleton_data) and stacks into a grouped_tab (split_var + var), so
#     tab_spread(split_var) pivots groups to columns (no tab_spread change: split_var placed first so
#     `levels` stays the row_var; console footer is group-aware, export footer skipped for splits).
#     `multiplier` (c(var=k)) scales a continuous predictor's native coef by k before CI/exp (OR^k),
#     p unchanged.
# See: CLAUDE.md Phase 12c-12g ; dev/tabxplor_1.4.0_decisions.md S37.

# === Internal engine ================================================================

# broom is an Import (every fit is tidied with it), so it needs no guard here; survey only for the
# weighted (wt) path; nnet / MASS for the nominal (multinomial) / ordinal (proportional-odds) families
# (both R Recommended -> normally present); marginaleffects only for effect="ame" (the AME /
# adjusted-prediction engine, Phase 12e).
reg_check_deps <- function(family, weighted, needs_marginaleffects = FALSE) {
  if (needs_marginaleffects && !requireNamespace("marginaleffects", quietly = TRUE)) {
    cli::cli_abort(c(
      '{.pkg marginaleffects} is required for {.code effect = "ame"} and the {.code at = "reference"} ',
      "i" = 'profile axis. Install it with {.code install.packages("marginaleffects")}, or use the ',
      "i" = 'default {.code effect = "coefficient"}, {.code at = "average"}.'
    ))
  }
  if (isTRUE(weighted) && !requireNamespace("survey", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg survey} is required for weighted / survey-design regression.",
      "i" = 'Install it with {.code install.packages("survey")}.'
    ))
  }
  if (family == "multinomial" && !isTRUE(weighted) && !requireNamespace("nnet", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg nnet} is required for multinomial (nominal 3+ level) outcomes.",
      "i" = 'Install it with {.code install.packages("nnet")}.'
    ))
  }
  if (family == "ordinal" && !isTRUE(weighted) && !requireNamespace("MASS", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg MASS} is required for ordinal (proportional-odds) outcomes.",
      "i" = 'Install it with {.code install.packages("MASS")}.'
    ))
  }
  # Weighted 3+ level (Phase 12g): ordinal -> survey::svyolr (already covered by the survey check);
  # multinomial -> svyVGAM::svy_vglm, whose family argument is VGAM::multinomial(). VGAM is a hard
  # dependency of svyVGAM, so it is present whenever svyVGAM is -- but reg_fit_multinom() calls
  # VGAM:: directly, so guard it explicitly: an implicit guard is invisible to R CMD check.
  if (isTRUE(weighted) && family == "multinomial") {
    missing_pkgs <- c("svyVGAM", "VGAM")[
      !vapply(c("svyVGAM", "VGAM"), requireNamespace, logical(1), quietly = TRUE)
    ]
    if (length(missing_pkgs) > 0) {
      cli::cli_abort(c(
        "{.pkg {missing_pkgs}} {?is/are} required for survey-weighted multinomial (nominal 3+ level) models.",
        "i" = 'Install {?it/them} with {.code install.packages("svyVGAM")}, or drop the weights / design.'
      ))
    }
  }
}

# Parse a formula escape-hatch (D9). Returns the LHS outcome name, the bare RHS variables (for
# reference= / family-detect / labels), the verbatim formula, and `simple`: TRUE iff LHS is a bare
# column name AND every RHS term is a bare main-effect column of `data` (no `:`, poly(), I(), calls).
# A simple formula reduces losslessly to the dependent+predictors character path; a compound one is
# fit verbatim with a skeleton read from the fitted terms (reg_skeleton_from_fit).
reg_parse_formula <- function(formula, data) {
  lhs <- rlang::f_lhs(formula)
  if (is.null(lhs)) {
    cli::cli_abort("A regression {.arg formula} needs a response, e.g. {.code y ~ x1 + x2}.")
  }
  lhs_is_name <- rlang::is_symbol(lhs)
  dependent   <- if (lhs_is_name) rlang::as_string(lhs) else all.vars(lhs)[1]

  tt     <- stats::terms(formula, data = data)
  labels <- attr(tt, "term.labels")
  orders <- attr(tt, "order")
  rhs_vars <- all.vars(rlang::f_rhs(formula))

  simple <- lhs_is_name &&
    dependent %in% names(data) &&
    length(labels) > 0L &&
    all(orders == 1L) &&
    all(labels %in% names(data))

  list(dependent = dependent, predictors = rhs_vars, labels = labels,
       formula = formula, lhs_is_name = lhs_is_name, simple = simple)
}

# Auto-detect the model family from the outcome (a message is emitted). The safe data-driven rules
# (S37 D2): 0/1 or any 2-level outcome -> binomial; an ORDERED factor with 3+ levels -> ordinal
# (proportional-odds); an UNORDERED factor / character with 3+ levels -> multinomial; a non-integer
# numeric -> gaussian. An integer/count with 3+ values stays ambiguous (poisson vs grouped-binomial
# vs gaussian) and must be named explicitly.
reg_detect_family <- function(data, dependent) {
  y <- data[[dependent]]
  u <- unique(stats::na.omit(y))
  if (length(u) == 2L) {
    cli::cli_inform(c("i" = paste0(
      "{.val {dependent}}: binary outcome detected -> {.code family = \"binomial\"} (logistic)."
    )))
    return("binomial")
  }
  if (is.ordered(y) && length(u) >= 3L) {
    cli::cli_inform(c("i" = paste0(
      "{.val {dependent}}: ordered outcome detected -> {.code family = \"ordinal\"} ",
      "(proportional-odds)."
    )))
    return("ordinal")
  }
  if ((is.factor(y) || is.character(y)) && length(u) >= 3L) {
    cli::cli_inform(c("i" = paste0(
      "{.val {dependent}}: nominal outcome detected -> {.code family = \"multinomial\"} (multinomial ",
      "logistic)."
    )))
    return("multinomial")
  }
  if (is.numeric(y) && any(y %% 1 != 0, na.rm = TRUE)) {
    cli::cli_inform(c("i" = paste0(
      "{.val {dependent}}: continuous outcome detected -> {.code family = \"gaussian\"} (linear)."
    )))
    return("gaussian")
  }
  cli::cli_abort(c(
    "Cannot auto-detect the model family for {.val {dependent}}.",
    "i" = paste0("Set {.arg family} explicitly: {.val gaussian} (linear), {.val poisson} (counts), ",
                 "{.val binomial} (logistic), {.val multinomial} / {.val ordinal} (3+ level).")
  ))
}

# The effect-measure word shown per column (S37 D1 -- auto-labelled per column, never one global
# header). effect="ame" -> "AME" (average marginal effect, response scale). Otherwise: additive (raw)
# -> beta ; multiplicative -> OR (binomial) / IRR (poisson) / exp(beta).
reg_effect_word <- function(family, do_exp, effect = "coefficient", at = "average") {
  if (effect == "ame") return(if (at == "reference") "MER" else "AME")   # marginal effect at reference
  if (!do_exp) return("\u03b2")                  # beta (raw / log-odds coefficient)
  switch(family,
         "binomial" = , "multinomial" = , "ordinal" = "OR",
         "poisson" = , "quasipoisson" = "IRR",
         "exp(\u03b2)")
}

# Phase 14w: the human name of the model family, shared by the reg title/caption and the "Model:" footer
# line (reg_model_line). do_exp/effect do not change the NAME (the estimand phrase carries that detail).
reg_family_display_name <- function(family) {
  switch(family,
    "gaussian"     = "linear regression",
    "binomial"     = "logistic regression",
    "poisson"      = "Poisson regression",
    "quasipoisson" = "quasi-Poisson regression",
    "multinomial"  = "multinomial logistic regression",
    "ordinal"      = "ordinal logistic regression",
    "regression")
}

# Phase 14w: the short model tag used for Excel sheet names ("logit_<dep>_<pred>...").
reg_family_short <- function(family) {
  switch(family,
    "gaussian"     = "linear",
    "binomial"     = "logit",
    "poisson"      = "poisson",
    "quasipoisson" = "qpoisson",
    "multinomial"  = "mlogit",
    "ordinal"      = "ologit",
    "reg")
}

# The ESTIMAND phrase (lower-case fragment, never NULL) -- WHAT the numbers are. Composed into the
# "Model: <family>. <estimand>." footer line by reg_model_line(). effect="ame" and the multinomial
# "at reference" profile get their own phrasing; otherwise it is the coefficient/exp scale per family.
reg_model_note <- function(family, do_exp, effect = "coefficient", at = "average") {
  if (effect == "ame") {
    prob  <- family %in% c("binomial", "multinomial", "ordinal")
    where <- if (at == "reference")
      " at the reference profile (other predictors held at their reference level / mean)"
    else " (sample-averaged)"
    return(if (prob)
      paste0("marginal effects on the probability scale (percentage points)", where,
             "; each cell shows the effect vs the reference level and, in parentheses, the adjusted ",
             "predicted probability")
    else
      paste0("marginal effects on the response scale", where))
  }
  if (at == "reference" && family == "multinomial") {
    return(paste0("odds ratios of each outcome category versus the rest, at the reference profile ",
                  "(other predictors held at their reference level / mean); profile-conditional"))
  }
  switch(family,
    "gaussian"     = "coefficients (mean difference vs the reference category)",
    "binomial"     = if (do_exp) "odds ratios (vs the reference category)"
                     else        "log-odds coefficients (vs the reference category)",
    "poisson"      = ,
    "quasipoisson" = if (do_exp) "incidence-rate ratios (vs the reference category)"
                     else        "log-rate coefficients (vs the reference category)",
    "multinomial"  = if (do_exp) "odds ratios (each category vs the reference)"
                     else        "log-odds coefficients (each category vs the reference)",
    "ordinal"      = if (do_exp) "cumulative odds ratios (proportional-odds model)"
                     else        "proportional-odds model (log-odds coefficients)",
    "")
}

# Phase 14w: the "Model: <family>. <estimand>." legend line, generated fresh from `reg_meta` at render
# so it can be ordered BEFORE the colour legend (item 2). For a model comparison the caption is not shown
# in the console, so the dependent + (binomial) reference level are named here too (item 4). NULL when the
# table is not a regression (get_reg_meta -> NULL).
reg_model_line <- function(meta) {
  if (is.null(meta)) return(NULL)
  fam <- reg_family_display_name(meta$family)
  est <- reg_model_note(meta$family, meta$do_exp, meta$effect, meta$at)
  who <- if (isTRUE(meta$comparison)) {
    pl <- meta$positive_level[[1]]
    if (!is.na(pl)) paste0(" of ", meta$dependent[[1]], " ('", pl, "')")
    else            paste0(" of ", meta$dependent[[1]])
  } else ""
  paste0("Model: ", fam, who, if (nzchar(est)) paste0("; ", est) else "", ".")
}

# Phase 14w: the reg table's TITLE / caption (Excel title + sheet, md/kable caption). Single model:
# "<Family>: <dep> by <p1>, <p2> +N more". Comparison: "<Family>s (models comparison): <dep>, '<ref>'
# (<effect>)" -- the reference level + effect that would otherwise be written nowhere (item 4).
reg_title <- function(meta, max = 2) {
  if (is.null(meta)) return(NA_character_)
  fam <- reg_family_display_name(meta$family)
  Fam <- paste0(toupper(substr(fam, 1, 1)), substr(fam, 2, nchar(fam)))
  dep <- tab_title_names(meta$dependent, max)
  tabbed <- if (!is.null(meta$split_var)) paste0(" (tabbed by ", meta$split_var, ")") else ""
  if (isTRUE(meta$comparison)) {
    pl  <- meta$positive_level[[1]]
    dref <- if (!is.na(pl)) paste0(dep, ", '", pl, "' (", meta$eff_word, ")")
            else            paste0(dep, " (", meta$eff_word, ")")
    paste0(Fam, "s (models comparison): ", dref, tabbed)
  } else {
    preds <- tab_title_names(meta$predictors, max)
    paste0(Fam, ": ", dep, if (nzchar(preds)) paste0(" by ", preds) else "", tabbed)
  }
}

# Phase 14w (item 1): a compact Excel sheet name for a reg table -- "<short>_<dep>_<pred>..." (e.g.
# "logit_married_race_rincome"), truncated to 25 chars by the caller. A comparison collapses the
# predictors to "compare" (they differ per model).
reg_sheet_name <- function(meta) {
  if (is.null(meta)) return(NA_character_)
  tail <- if (isTRUE(meta$comparison)) c(meta$dependent[[1]], "compare")
          else                         c(meta$dependent, meta$predictors)
  paste(c(reg_family_short(meta$family), tail), collapse = "_")
}

# Phase 14w (item 3): the shared col_var for a SINGLE-outcome model column + its empirical companions,
# so ONE span header names the outcome and no border separates them (they share a col_var). Binomial ->
# "<dep>: <positive_level>"; a numeric outcome (gaussian/poisson) -> the dependent name alone. NOT used in
# comparison mode (each model keeps its own col_var = model name, so borders separate the models, and the
# outcome / reference / effect go in the title instead).
reg_shared_col_var <- function(family, dependent, positive_level, cleannames) {
  if (family == "binomial" && !is.null(positive_level) && !is.na(positive_level)) {
    pl <- positive_level
    if (isTRUE(cleannames)) pl <- stringr::str_remove_all(pl, cleannames_condition())
    paste0(dependent, ": ", pl)
  } else dependent
}

# Phase 14w (item 3): the single-model column NAME ("Model OR" / "Model IRR" / "Model AME (adjusted %)"),
# so the effect word lives in the column, not repeated in the span. Comparison mode keeps the model name;
# a multi-dependent (several outcomes, one predictor set) suffixes the dependent so the names stay unique.
reg_model_col_name <- function(eff_word, dependent, is_comparison, model_label, n_dep) {
  if (isTRUE(is_comparison)) return(model_label)
  if (n_dep > 1L) paste0("Model ", eff_word, " (", dependent, ")") else paste0("Model ", eff_word)
}

# Prepare a binary dependent: a 0/1 numeric becomes a 2-level factor ("Not <dep>" / "<dep>"); any
# other input must have exactly 2 levels, optionally reversed so glm models the FIRST level
# (inverse_two_level_factors -- the maintainer's convention, e.g. "1-Married" first = modelled). The
# modelled ("positive") level is returned as an attribute for the column label.
reg_prep_binary <- function(data, dependent, inverse_two_level_factors) {
  y <- data[[dependent]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    y <- factor(y, levels = c(0, 1), labels = c(paste0("Not ", dependent), dependent))
    positive <- dependent
  } else {
    y <- forcats::fct_drop(as.factor(y))
    if (nlevels(y) != 2L) {
      cli::cli_abort(c(
        "The dependent variable {.val {dependent}} must be binary (2 levels).",
        "x" = "It has {nlevels(y)} level{?s}: {.val {levels(y)}}.",
        "i" = paste0("For a summed-score outcome (0..q items), pass {.arg trials} to fit a grouped ",
                     "binomial."),
        "i" = "Multinomial / 3+ level outcomes are planned for a later phase (12d)."
      ))
    }
    if (inverse_two_level_factors) y <- forcats::fct_rev(y)
    positive <- levels(y)[2L]
  }
  data[[dependent]] <- y
  attr(data, "positive_level") <- positive
  data
}

# The modelled ("positive") level of a binary dependent, computed from the raw data (for the column
# label, before fitting). Mirrors reg_prep_binary()'s choice.
reg_positive_level <- function(data, dependent, inverse_two_level_factors) {
  y <- data[[dependent]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) return(dependent)
  lv <- levels(forcats::fct_drop(as.factor(y)))
  lv[if (inverse_two_level_factors) 1L else 2L]
}

# Relevel factor predictors to user-chosen treatment-contrast baselines. `reference` is a named
# vector c(var = "baseline level"). Regression references are model contrasts (fct_relevel), NOT the
# crosstab comparison-row semantics of resolve_ref_vector().
reg_apply_references <- function(data, reference, predictors) {
  nm <- names(reference)
  if (is.null(nm) || any(!nzchar(nm))) {
    cli::cli_abort(c("{.arg reference} must be a named vector, e.g. {.code c(race = \"White\")}."))
  }
  extra <- setdiff(nm, predictors)
  if (length(extra) > 0L) {
    cli::cli_warn("{.arg reference} name{?s} {.val {extra}} match no predictor; ignored.")
  }
  for (v in intersect(nm, predictors)) {
    f <- data[[v]]
    if (!is.factor(f) && !is.character(f)) {
      cli::cli_warn("{.arg reference} ignored for {.val {v}}: not a factor/character predictor.")
      next
    }
    f   <- as.factor(f)
    lev <- reference[[v]]
    if (!lev %in% levels(f)) {
      cli::cli_abort(c("{.arg reference} level {.val {lev}} not found in {.val {v}}.",
                       "i" = "Levels: {.val {levels(f)}}."))
    }
    data[[v]] <- forcats::fct_relevel(f, lev)
  }
  data
}

# The (var, level, term, is_ref) row skeleton for a set of predictors, in display order: the
# intercept ("Constant") first, then each predictor's levels (factor / character) -- first level =
# reference, no model term -- or a single row for a numeric predictor. `term` matches lm/glm/svyglm
# coefficient names so a fit aligns to the skeleton by term.
reg_skeleton <- function(data, predictors) {
  parts <- purrr::map(predictors, function(p) {
    v <- data[[p]]
    if (is.factor(v) || is.character(v)) {
      lv <- levels(forcats::fct_drop(as.factor(v)))
      tibble::tibble(
        var    = p,
        level  = lv,
        term   = c(NA_character_, paste0(p, lv[-1])),
        is_ref = c(TRUE, rep(FALSE, length(lv) - 1L))
      )
    } else {
      tibble::tibble(var = p, level = p, term = p, is_ref = FALSE)
    }
  })
  dplyr::bind_rows(
    tibble::tibble(var = "Constant", level = "Reference population",
                   term = "(Intercept)", is_ref = TRUE),
    parts
  )
}

# The skeleton for the COMPOUND-formula path (D9): built from the FITTED model, not the data, so the
# rows match whatever the user's formula produced. Intercept first, then each fitted term: a pure
# factor main effect (its label is one of fit$xlevels) expands to level rows (first = reference, no
# term); every other term (numeric main effect, interaction, poly(), I(), fn call) emits one row per
# coefficient column assigned to it, labelled by the coefficient name -- best-effort, no reference row.
# `term` values equal the model-matrix column names, which broom::tidy() reproduces, so reg_column()
# aligns by term exactly as on the data-skeleton path.
reg_skeleton_from_fit <- function(fit) {
  tt      <- stats::terms(fit)
  labels  <- attr(tt, "term.labels")
  assign  <- attr(stats::model.matrix(fit), "assign")   # 0 = intercept, k = labels[k]
  coefnms <- names(stats::coef(fit))
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
  dplyr::bind_rows(
    tibble::tibble(var = "Constant", level = "Reference population",
                   term = "(Intercept)", is_ref = TRUE),
    parts
  )
}

# Strip a term's own name off its coefficient names for a shorter `level` label (best-effort):
# "poly(age, 2)1" -> "1"; a name we can't shorten is left whole. Regex-escaped so poly()/I() are safe.
term_prefix <- function(label) {
  stringr::str_replace_all(label, "([.\\\\+*?\\[^\\]$(){}=!<>|:#/-])", "\\\\\\1")
}

# Per-coefficient LIKELIHOOD-RATIO p-values (the dual of the profile-likelihood CI). Each coefficient
# is dropped from the model matrix in turn and the deviance change is a 1-df chi-square. Unweighted
# glm only (binomial/poisson); for a factor it tests one level vs the reference, matching the
# per-level effect the table shows.
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
  stats::setNames(p, stringr::str_remove_all(colnames(X), "`"))
}

# Wald CI + p from a tidy carrying `estimate` + `std.error` on the log scale. multinom / polr are ML
# with fixed dispersion, so the quantile is z (qnorm) -- the same branch the fixed-dispersion glm path
# uses. Both CI and p come from estimate/se, so they are exact duals (CI <-> stars can never disagree),
# and both survive an NaN se (a rank-deficient / empty cell -> NaN, matching the base model). `do_exp`
# exponentiates the estimate and the bounds (OR/IRR). Fills conf.low/conf.high/p.value in place.
reg_wald_from_tidy <- function(td, conf_level, do_exp) {
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  lo   <- td$estimate - crit * td$std.error
  hi   <- td$estimate + crit * td$std.error
  p    <- 2 * stats::pnorm(-abs(td$estimate / td$std.error))
  est  <- td$estimate
  if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  td$estimate <- est; td$conf.low <- lo; td$conf.high <- hi; td$p.value <- p
  td
}

# Nominal 3+ level outcome: ONE multinomial logit -- unweighted nnet::multinom, weighted
# svyVGAM::svy_vglm (Phase 12g). exp(coef) is the "OR (j vs the reference outcome level)" -- the
# Begg-Gray estimand, one set of coefficients per non-reference category (the tidy carries a `y.level`
# column that reg_build splits into one OR column per category). The reference category is the outcome
# factor's FIRST level (set via `reference` upstream, MNL only).
reg_fit_multinom <- function(mdata, dependent, predictors, do_exp, conf_level, method,
                             weighted = FALSE, make_design = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for multinomial models; using Wald."))
  }
  mdata[[dependent]] <- forcats::fct_drop(as.factor(mdata[[dependent]]))
  y_levels <- levels(mdata[[dependent]])
  fml <- stats::as.formula(paste0(
    "`", dependent, "` ~ ", paste0("`", predictors, "`", collapse = " + ")
  ))

  if (weighted) {
    # svyVGAM::svy_vglm with VGAM's multinomial: refLevel = 1 makes the FIRST (reference) level the
    # baseline, matching nnet. VGAM names coefficients "term:k", k = the k-th NON-reference category
    # (in level order) -> parse each name into (term, y.level) so reg_build's per-category split works.
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
    td   <- tibble::tibble(y.level = ylev[k], term = stringr::str_remove_all(trm, "`"),
                           estimate = unname(cf), std.error = unname(se[nm]))
    td   <- reg_wald_from_tidy(td, conf_level, do_exp)
    return(list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL,
                fit = fit, data = mdata, y_ref = y_levels[1], y_levels = y_levels[-1]))
  }

  fit <- nnet::multinom(fml, data = mdata, trace = FALSE)
  td  <- broom::tidy(fit)                              # y.level, term, estimate, std.error, ...
  td$term <- stringr::str_remove_all(td$term, "`")     # strip formula backticks -> match skeleton
  td  <- reg_wald_from_tidy(td, conf_level, do_exp)
  list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL,
       fit = fit, data = mdata, y_ref = y_levels[1], y_levels = y_levels[-1])
}

# Ordered 3+ level outcome: proportional-odds cumulative logit -- unweighted MASS::polr, weighted
# survey::svyolr (Phase 12g). exp(coef) is one cumulative OR per predictor level -> ONE column (the
# cut-point "scale" rows are dropped, so the skeleton "Constant" cell stays NA). The parallel-lines
# assumption is diagnosed (Brant test) for the unweighted fit; the design-based fit degrades that.
reg_fit_ordinal <- function(mdata, dependent, predictors, do_exp, conf_level, method,
                            weighted = FALSE, make_design = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for proportional-odds models; using Wald."))
  }
  y <- mdata[[dependent]]
  if (!is.ordered(y)) {
    y <- as.ordered(forcats::fct_drop(as.factor(y)))
    lv_str <- paste(levels(y), collapse = " < ")
    cli::cli_inform(c("i" = "{.val {dependent}}: treated as ordered ({lv_str})."))
  } else {
    y <- forcats::fct_drop(y)
  }
  mdata[[dependent]] <- y
  fml <- stats::as.formula(paste0(
    "`", dependent, "` ~ ", paste0("`", predictors, "`", collapse = " + ")
  ))

  if (weighted) {
    # survey::svyolr: coef() also returns the cut-point ("zeta") thresholds -> read the SLOPES from
    # fit$coefficients and their SEs from the matching vcov() rows (the cumulative-OR terms only).
    # svyolr's start-value glm.fit step cannot take zero/negative weights -> a clear hint on failure.
    fit <- tryCatch(
      survey::svyolr(fml, design = make_design(mdata)),
      error = function(e) cli::cli_abort(c(
        "The survey-weighted ordinal model ({.fn survey::svyolr}) failed to fit.",
        "x" = conditionMessage(e),
        "i" = "{.fn svyolr} needs strictly positive weights - check for zero / negative weights."
      ))
    )
    cf  <- fit$coefficients
    se  <- sqrt(diag(stats::vcov(fit)))[names(cf)]
    td  <- tibble::tibble(term = stringr::str_remove_all(names(cf), "`"),
                          estimate = unname(cf), std.error = unname(se))
    td  <- reg_wald_from_tidy(td, conf_level, do_exp)
    cli::cli_inform(c("i" = paste0("The proportional-odds (parallel-lines) assumption is not tested for ",
                                   "survey-weighted ordinal models (the Brant test needs an unweighted fit).")))
    return(list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL, fit = fit,
                data = mdata))
  }

  fit <- MASS::polr(fml, data = mdata, Hess = TRUE, method = "logistic")
  td  <- broom::tidy(fit)
  td  <- td[td$coef.type == "coefficient", , drop = FALSE]   # drop cut-point ("scale") intercepts
  td$term <- stringr::str_remove_all(td$term, "`")
  td  <- reg_wald_from_tidy(td, conf_level, do_exp)
  # Brant PO test -> warn (gated on brant); stash the omnibus p on the fit so reg_glance() can add the
  # "Brant PO test" footer row without recomputing (Phase 14q Item I).
  attr(fit, "brant_po") <- reg_ordinal_diagnostic(fit)
  list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL, fit = fit,
       data = mdata)
}

# Diagnose the proportional-odds (parallel-lines) assumption with the Brant test (the `brant` package,
# a Suggests). Warn when the omnibus test rejects; a missing `brant` skips it with a hint; a failing
# test (sparse data) is swallowed -- a diagnostic must never break the table.
reg_ordinal_diagnostic <- function(fit) {
  if (!requireNamespace("brant", quietly = TRUE)) {
    cli::cli_inform(c("i" = paste0(
      "Proportional-odds (parallel-lines) assumption not tested: install {.pkg brant} to run the ",
      "Brant test."
    )))
    return(invisible(NA_real_))
  }
  # brant rebuilds the model frame via eval.parent(fit$call), needing the fit's `data`/`formula`
  # SYMBOLS resolvable in brant's caller frame -- which fails once we are past the fitting scope. Make
  # this (copy-on-modify) fit self-contained from its own stored model frame so brant works anywhere.
  fit$call$data    <- fit$model
  fit$call$formula <- stats::formula(fit)
  bt <- tryCatch({ utils::capture.output(res <- brant::brant(fit)); res },
                 error = function(e) NULL)
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
  # Phase 14q (Item I): return the omnibus p so reg_glance() can surface it as a footer row.
  invisible(p)
}

# --- Survey design construction (Phase 12g) --------------------------------------------------------
# A weight column (+ optional ids/strata/fpc/nest) is turned into a survey.design *per model*, on the
# complete-case model frame -- ids = ~1 (no clustering) by default reproduces the flat weighted path
# exactly. A PREBUILT survey.design / svyrep.design (passed as `data`) is NOT rebuilt (replicate /
# calibrated designs cannot be) -- it is subset()'d to the model's complete cases (domain estimation)
# with its model-frame variables replaced by the recoded `mdata` (same rows, same order).
# `design_spec` = list(design = <prebuilt or NULL>, wt, ids, strata, fpc, nest).

# Coerce a design argument (NULL / a column name or char vector / a formula) to a survey formula.
reg_design_formula <- function(x) {
  if (is.null(x)) return(NULL)
  if (rlang::is_formula(x)) return(x)
  stats::reformulate(x)
}
# The data columns a design spec references (for drop_na, so svydesign never sees NA weights/strata/fpc).
reg_design_vars <- function(design_spec) {
  if (!is.null(design_spec$design)) return(character(0))   # a prebuilt design carries its own metadata
  parts <- list(design_spec$wt, design_spec$ids, design_spec$strata, design_spec$fpc)
  unique(unlist(purrr::map(parts, function(x) {
    if (is.null(x)) character(0) else if (rlang::is_formula(x)) all.vars(x) else as.character(x)
  })))
}
reg_make_design <- function(data, wt, ids, strata, fpc, nest) {
  survey::svydesign(
    ids     = if (is.null(ids)) stats::as.formula("~1") else reg_design_formula(ids),
    strata  = reg_design_formula(strata),
    fpc     = reg_design_formula(fpc),
    weights = reg_design_formula(wt),
    data    = data,
    nest    = nest
  )
}
# Subset a prebuilt design to the model's complete cases, then swap its model frame for the recoded
# `mdata` (drop_na + fct_drop + reg_prep_binary + grouped-binomial cols already applied). The design
# metadata slots (strata / cluster / fpc / prob) are subset by `[` and stay row-aligned with mdata.
reg_subset_design <- function(design, keep_mask, mdata) {
  dd <- design[keep_mask, ]
  dd$variables <- mdata
  dd
}
# The survey design for a model's (recoded) complete-case frame: a prebuilt design is subset()'d and
# has its model frame swapped for `mdata`; a weight column (+ ids/strata/fpc/nest) is built into a fresh
# design on `mdata`. `data` + `drop_vars` give the complete-case mask for the subset path. Shared by the
# glm (svyglm) and the 3+ level (svyolr / svy_vglm) weighted branches -- one design constructor.
reg_resolve_design <- function(design_spec, mdata, data, drop_vars) {
  if (!is.null(design_spec$design)) {
    keep_mask <- stats::complete.cases(data[, drop_vars, drop = FALSE])
    reg_subset_design(design_spec$design, keep_mask, mdata)
  } else {
    reg_make_design(mdata, design_spec$wt, design_spec$ids,
                    design_spec$strata, design_spec$fpc, design_spec$nest)
  }
}
# AIC.svyglm / anova.svyglm refit sub-models with an UNQUALIFIED `svyglm()` call evaluated in the model
# formula's environment. When `survey` is loaded via `::` but NOT attached (the normal tabxplor case),
# that lookup fails ("could not find function svyglm"). Bind `survey::svyglm` into a child of the
# formula/terms environment so the survey glance (AIC) and model comparison (anova) work regardless.
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
# Relevel predictor / outcome reference levels INSIDE a prebuilt design (factor releveling touches only
# $variables, never the weights / strata / fpc / row set), so a design-based fit honours `reference=`.
reg_relevel_design <- function(design, reference, relevelable) {
  design$variables <- reg_apply_references(design$variables, reference, relevelable)
  design
}

# Fit ONE model on complete cases -> a tidy of the (per-family) effect measure + CI + p + the model n
# (+ var(Y) for the additive gaussian effect-size colour). `do_exp` chooses the estimate scale:
# TRUE -> exp(coef) (OR/IRR, multiplicative); FALSE -> raw coef (beta, additive). Wald CI uses z for
# fixed-dispersion glm (binomial/poisson), else t(df.residual); this matches broom's own z/t p, so
# the CI and the stars are exact duals. method="profile" (unweighted glm) swaps to confint + LR p.
reg_fit <- function(data, dependent, predictors, family, design_spec, do_exp,
                    inverse_two_level_factors, conf_level, method,
                    trials = NULL, formula = NULL, multiplier = NULL) {
  drop_vars <- unique(c(dependent, predictors, reg_design_vars(design_spec)))
  mdata     <- tidyr::drop_na(data, tidyselect::all_of(drop_vars))

  fac_preds <- predictors[purrr::map_lgl(
    predictors, ~ is.factor(mdata[[.]]) || is.character(mdata[[.]])
  )]
  if (length(fac_preds) > 0L) {
    # Phase 14r: coerce factor/character predictors to UNORDERED factors. An ORDERED predictor makes
    # glm / polr use polynomial contrasts (terms `x.L`/`x.Q`/...), which the coefficient path cannot
    # align to the per-level skeleton -> an all-NA effect column (the "remove ordered to not break the
    # model" the maintainer had to do by hand). Only PREDICTORS are de-ordered; an ordinal DEPENDENT
    # keeps its order (reg_fit_ordinal re-imposes it). Level ORDER is preserved, so the reference (first
    # level) and the display order are unchanged.
    mdata <- dplyr::mutate(mdata, dplyr::across(
      tidyselect::all_of(fac_preds),
      ~ { f <- forcats::fct_drop(as.factor(.)); factor(f, levels = levels(f), ordered = FALSE) }
    ))
  }

  weighted <- !is.null(design_spec$design) || !is.null(design_spec$wt)
  # A closure the fit branches call with their OWN recoded model frame -> the matching survey design
  # (build the weight-column design / subset the prebuilt one). Lets the MNL / ordinal engines, which
  # recode the outcome themselves, get a row-aligned design without re-deriving the mask.
  make_design <- function(recoded_mdata) reg_resolve_design(design_spec, recoded_mdata, data, drop_vars)

  # 3+ level categorical outcomes have their own engines: unweighted -> nnet::multinom / MASS::polr;
  # weighted -> svyVGAM::svy_vglm / survey::svyolr. All share the Wald machinery (reg_wald_from_tidy) so
  # the CI <-> p <-> stars duality holds, but not the glm path.
  if (family == "multinomial") {
    return(reg_fit_multinom(mdata, dependent, predictors, do_exp, conf_level, method,
                            weighted, make_design))
  }
  if (family == "ordinal") {
    return(reg_fit_ordinal(mdata, dependent, predictors, do_exp, conf_level, method,
                           weighted, make_design))
  }

  positive_level <- NULL
  # grouped binomial: a summed-score outcome (0..trials) fit as cbind(score, trials-score) (D2). Only
  # on the non-formula path (a compound formula controls its own LHS, so `trials` does not apply).
  grouped <- family == "binomial" && !is.null(trials) && is.null(formula)
  if (grouped) {
    s <- mdata[[dependent]]
    if (!is.numeric(s) || any(s %% 1 != 0, na.rm = TRUE)) {
      cli::cli_abort(c("A summed-score outcome ({.arg trials}) must be integer-valued.",
                       "x" = "{.val {dependent}} is {.cls {class(s)}}."))
    }
    if (any(s < 0 | s > trials, na.rm = TRUE)) {
      cli::cli_abort(c("{.val {dependent}} scores must lie in {.val {0}}..{.val {trials}} (= {.arg trials}).",
                       "x" = "Observed range: {.val {range(s, na.rm = TRUE)}}."))
    }
    mdata[[".gb_succ"]] <- s
    mdata[[".gb_fail"]] <- trials - s
  }

  fam_obj <- switch(
    family,
    "binomial" = {
      if (is.null(trials) && is.null(formula)) {
        mdata <- reg_prep_binary(mdata, dependent, inverse_two_level_factors)
        positive_level <- attr(mdata, "positive_level")
      }
      if (weighted) stats::quasibinomial("logit") else stats::binomial("logit")
    },
    "poisson" = if (weighted) stats::quasipoisson("log") else stats::poisson("log"),
    "quasipoisson" = stats::quasipoisson("log"),
    "gaussian" = stats::gaussian(),
    cli::cli_abort("Unsupported {.arg family}: {.val {family}}.")
  )
  if (is.null(formula) && !grouped && family != "binomial" && !is.numeric(mdata[[dependent]])) {
    cli::cli_abort(c(
      "A {.val {family}} outcome must be numeric.",
      "x" = "{.val {dependent}} is {.cls {class(mdata[[dependent]])}}."
    ))
  }

  fml <- if (!is.null(formula)) {
    formula                                            # compound escape-hatch: fit verbatim
  } else {
    resp <- if (grouped) "cbind(`.gb_succ`, `.gb_fail`)" else paste0("`", dependent, "`")
    stats::as.formula(paste0(
      resp, " ~ ", paste0("`", predictors, "`", collapse = " + ")
    ))
  }

  fit <- if (family == "gaussian" && !weighted) {
    stats::lm(fml, data = mdata)
  } else if (!weighted) {
    stats::glm(fml, data = mdata, family = fam_obj)
  } else {
    # weighted: svyglm on the design for this model's complete cases (built or subset via make_design)
    survey::svyglm(fml, design = make_design(mdata), family = fam_obj)
  }
  if (weighted) fit <- reg_svyglm_env(fit)   # make survey::svyglm visible to AIC / anova null-refits

  td <- broom::tidy(fit)                            # native scale: estimate, std.error, p.value
  td$term <- stringr::str_remove_all(td$term, "`")  # strip formula backticks -> match skeleton

  # multiplier (Phase 12g): a k-unit change of a continuous predictor multiplies its native-scale
  # coefficient by k (beta -> beta*k, se -> se*|k|; exp() then gives OR^k). Applied on the native scale
  # BEFORE the CI so the Wald interval scales automatically; the profile CI (monotone reparametrisation)
  # scales linearly too. The z / LR p is scale-invariant (testing beta=0 <=> k*beta=0) -> unchanged.
  mult_vec <- rep(1, nrow(td))
  if (!is.null(multiplier)) {
    for (v in names(multiplier)) {
      mi <- td$term == v
      if (any(mi)) mult_vec[mi] <- as.numeric(multiplier[[v]])
    }
    td$estimate  <- td$estimate  * mult_vec
    td$std.error <- td$std.error * abs(mult_vec)
  }

  # 14v-ii over-dispersion (decisions §48): an unweighted Poisson / grouped-binomial MLE fit reports
  # naive (fixed-dispersion) SEs. Scale them by sqrt(phi) (phi = Pearson dispersion) so the Wald CI +
  # stars match a quasi-Poisson / quasi-binomial fit, while the MLE fit keeps its likelihood for the
  # AIC / McFadden / LR / BIC footer. Auto-degrades to naive when phi ~= 1. Bernoulli-binary dispersion
  # is not identifiable (reg_dispersion -> NA) and gaussian has no dispersion, so both are untouched.
  over_disp <- !weighted && (family == "poisson" || grouped)
  phi       <- if (over_disp) reg_dispersion(fit) else NA_real_
  scaled    <- over_disp && !is.na(phi) && phi > 0
  if (scaled) {
    td$std.error <- td$std.error * sqrt(phi)
    if (phi > 1.5) cli::cli_warn(c(
      "!" = paste0("Over-dispersion (Pearson dispersion = {signif(phi, 3)}",
                   "{if (phi > 2) ', strong' else ''}); standard errors are scaled by sqrt(dispersion) ",
                   "(quasi-{family}-like)."),
      "i" = "The footer reports the dispersion; use {.code family = \"quasipoisson\"} for the fully quasi fit."
    ))
  }

  use_profile <- method == "profile" && !weighted && family %in% c("binomial", "poisson")
  if (method == "profile" && weighted) {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for survey-weighted ",
                                   "models; using Wald.")))
  }

  if (use_profile) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      cli::cli_abort(c('{.pkg MASS} is required for {.code method = "profile"}.',
                       "i" = '- Install it, or use {.code method = "wald"} (the default).'))
    }
    ci  <- suppressMessages(stats::confint(fit, level = conf_level))   # log/native scale
    idx <- match(td$term, stringr::str_remove_all(rownames(ci), "`"))
    lo  <- unname(ci[idx, 1]) * mult_vec; hi <- unname(ci[idx, 2]) * mult_vec  # scale profile bounds
    lrp <- reg_lr_pvalues(fit)
    td$p.value <- unname(lrp[match(td$term, names(lrp))])
  } else {
    # z for fixed-dispersion glm (binomial/poisson, unweighted); else t on df.residual (lm, quasi*,
    # weighted svyglm, OR a 14v-ii phi-scaled poisson/grouped-binomial -- an estimated dispersion moves
    # the reference off z onto t, matching a quasi fit).
    disp_known <- !weighted && family %in% c("binomial", "poisson") && !scaled
    crit <- reg_wald_crit(disp_known, stats::df.residual(fit), conf_level)   # shared with reg_reref (15b)
    lo <- td$estimate - crit * td$std.error
    hi <- td$estimate + crit * td$std.error
    # 14v-ii: with the SE scaled and the t reference, recompute the Wald p from est/se so p <-> CI <->
    # stars stay duals (broom's td$p.value was the un-scaled fixed-dispersion model p).
    if (scaled)
      td$p.value <- 2 * stats::pt(-abs(td$estimate / td$std.error), df = stats::df.residual(fit))
  }

  est <- td$estimate
  if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  td$estimate <- est; td$conf.low <- lo; td$conf.high <- hi

  # var(Y) drives the additive gaussian effect-size colour (beta/SD(Y)); NA otherwise (no std colour)
  var_y <- if (!do_exp && family == "gaussian") stats::var(mdata[[dependent]]) else NA_real_

  list(tidy = td, nobs = nrow(mdata), var_y = var_y, positive_level = positive_level, fit = fit,
       data = mdata)
}

# Align one fit to the union skeleton -> a single fmt column (length = nrow(skeleton)), in the
# additive (beta) or multiplicative (OR/IRR) shape. Reference LEVELS of predictors present in this
# model get the neutral value (0 / 1, no CI/p); predictors ABSENT from this model stay NA (empty
# cells); the Constant carries the intercept (baseline) estimate.
reg_column <- function(skeleton, fit_res, model_predictors, col_var, effect_shape,
                       color, color_signif) {
  td  <- fit_res$tidy
  m   <- match(skeleton$term, td$term)
  est <- td$estimate[m]
  lo  <- td$conf.low[m]
  hi  <- td$conf.high[m]
  p   <- td$p.value[m]

  in_model <- skeleton$var %in% c("Constant", model_predictors)
  ref_lvl  <- skeleton$is_ref & skeleton$var != "Constant" & in_model
  neutral  <- if (effect_shape == "ratio") 1 else 0
  est[ref_lvl] <- neutral
  lo[ref_lvl]  <- NA_real_
  hi[ref_lvl]  <- NA_real_
  p[ref_lvl]   <- NA_real_

  n_rows   <- nrow(skeleton)
  refrows  <- ref_lvl | skeleton$var == "Constant"

  if (effect_shape == "ratio") {
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): whole-model N is in the footer, not a per-cell "n:"
      or = est, ci_inf = lo, ci_sup = hi, pvalue = p,
      type = "row", display = "or", digits = 2L, ref = "1", ci_type = "or",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows
    )
  } else {
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): whole-model N is in the footer, not a per-cell "n:"
      diff = est, ci_inf = lo, ci_sup = hi, pvalue = p,
      var = rep(fit_res$var_y, n_rows),                 # var(Y): standardizes beta/SD(Y) for colour
      type = "coef", display = "coef", digits = 2L, ci_type = "diff",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows
    )
  }
}

# Phase 12h: apply the `estimate_display` layout to ONE coefficient column. "value" = unchanged (plain
# OR / IRR / beta); "ci" = the `est_ci` token (estimate + a VISIBLE [ci_inf; ci_sup] bracket, dispatching
# OR vs beta on ci_type); "prob" / "ame" = FOLD the model-adjusted predicted probability / average
# marginal effect into the OR cell via the {} grammar ("{or} ({pct})" / "{or} ({diff})"), reusing
# reg_marginal(). The fold is binomial-coefficient-only (guaranteed by the tab_reg() degrade): stars ride
# the OR (the primary token) and its CI drives the colour; the (annotation) is a descriptive companion.
reg_apply_estimate_display <- function(col, mode, skeleton, f, sp, family, design_spec, conf_level,
                                       numeric_preds, model_predictors) {
  if (mode == "value") return(col)
  if (mode == "ci")    return(set_display(col, "est_ci"))

  marg     <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                           at = "average", want_pred = mode == "prob")
  key      <- paste(skeleton$var, skeleton$level, sep = "\r")
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_const <- skeleton$var == "Constant"
  is_ref   <- skeleton$is_ref & !is_const & in_model
  disp     <- get_display(col)
  if (mode == "prob") {
    prd    <- marg$pred
    pred_v <- if (nrow(prd)) prd$pred[match(key, paste(prd$var, prd$level, sep = "\r"))]
              else           rep(NA_real_, length(key))
    col    <- vctrs::`field<-`(col, "pct", pred_v)
    disp[in_model & !is_const & !is.na(pred_v)] <- "{or} ({pct})"
  } else {                                                   # "ame"
    amt    <- marg$ame
    ame_v  <- amt$ame[match(key, paste(amt$var, amt$level, sep = "\r"))]
    ame_v[is_ref] <- NA_real_                                # reference level has no marginal effect
    col    <- vctrs::`field<-`(col, "diff", ame_v)
    disp[in_model & !is_const & !is_ref & !is.na(ame_v)] <- "{or} ({diff})"
  }
  set_display(col, disp)
}


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
reg_empirical <- function(data, fac_preds, dependent, family, positive_level, wt) {
  w  <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  yv <- data[[dependent]]
  bin <- family == "binomial"
  # A 0/1 numeric outcome is fit as the labelled factor c("Not <dep>", "<dep>") with positive_level =
  # "<dep>" (reg_prep_binary). reg_empirical sees the RAW data, so mirror that recode -- else
  # as.character(0/1) never matches the label and the crude base is silently 0 (pre-14v-ii bug).
  if (bin && is.numeric(yv) && all(stats::na.omit(yv) %in% c(0, 1)))
    yv <- factor(yv, levels = c(0, 1), labels = c(paste0("Not ", dependent), dependent))
  if (bin) pos  <- as.character(yv) == positive_level else ynum <- as.numeric(yv)
  purrr::map_dfr(fac_preds, function(p) {
    x  <- data[[p]]
    ok <- !is.na(x) & !is.na(w) & (if (bin) !is.na(pos) else !is.na(ynum))
    lv <- levels(forcats::fct_drop(as.factor(x[ok])))
    per <- purrr::map(lv, function(l) {
      m <- ok & x == l
      if (bin) {
        wpos <- sum(w[m & pos]); wneg <- sum(w[m & !pos])
        list(base = wpos / (wpos + wneg), ratio_raw = wpos / wneg, var = NA_real_, n = sum(m))
      } else {
        n1 <- sum(m); wn <- sum(w[m]); s1 <- sum(w[m] * ynum[m]); s2 <- sum(w[m] * ynum[m]^2)
        mean_l <- s1 / wn
        # match tab()/num_derive_stats: unweighted -> stats::var (n-1), weighted -> ML (s2/wn - mean^2).
        # 14v-ii: poisson gets the count variance too (drives its crude rate-ratio CI, ci_mean_ratio).
        var_l  <- if (family %in% c("gaussian", "poisson")) {
          if (is.null(wt)) (s2 - s1^2 / n1) / (n1 - 1) else round(s2 / wn - (s1 / wn)^2, 10)
        } else NA_real_
        list(base = mean_l, ratio_raw = mean_l, var = var_l, n = n1)
      }
    })
    ref <- per[[1]]
    # 14v-ii: the reference level's stats (constant within a var), so reg_empirical_columns can build the
    # crude CI of each level vs its reference with the same engines tab()/the model use.
    tibble::tibble(
      var = p, level = lv,
      emp_base  = purrr::map_dbl(per, "base"),
      emp_diff  = purrr::map_dbl(per, ~ .$base - ref$base),
      emp_ratio = purrr::map_dbl(per, ~ .$ratio_raw / ref$ratio_raw),
      emp_var   = purrr::map_dbl(per, "var"),
      emp_n     = purrr::map_int(per, ~ as.integer(.$n)),
      emp_ref_base = ref$base, emp_ref_var = ref$var, emp_ref_n = as.integer(ref$n)
    )
  })
}

# Two fmt columns aligned to the skeleton (base descriptive + crude effect mirroring the model's
# measure & colour scale), for reg_build to prepend before the model column. Numeric predictors / the
# Constant -> empty cells; reference levels -> neutral (diff 0 / OR 1) + in_refrow. 14v-ii: each column
# now carries a crude CI + pvalue computed with the SAME method as the model (so crude and adjusted are
# directly comparable) and is coloured by significance (the caller's `color_signif`, like the model
# columns) -- except the descriptive Emp. mean, which stays uncoloured (its CI feeds only stars/tooltip).
# Per (family, effect):
#   binomial coef : Emp. % (risk-diff colour, Newcombe CI) + Emp. OR (ratio colour, Woolf log-OR CI).
#   binomial ame  : Emp. % + Emp. diff (crude risk-diff; the AME shows a difference, not an OR).
#   gaussian      : Emp. mean (mean+sd, UNCOLOURED, one-sample t cell CI) + Emp. diff (crude mean-diff,
#                   Student t = OLS, type="coef" var=var(Y) -> diff/SD(Y), the model beta's scale).
#   poisson       : Emp. rate (rate-ratio colour) + Emp. IRR (crude rate-ratio, quasi-Poisson CI = the
#                   phi-scaled model's method). want_p = TRUE always: the pvalue is stored (stars are
#                   stripped post-build when stars = FALSE, exactly like the model columns).
reg_empirical_columns <- function(skeleton, emp, fac_preds, family, effect, var_y,
                                  conf_level = 0.95, color_signif = "grey_non_signif") {
  mi      <- match(paste(skeleton$var, skeleton$level, sep = "\r"),
                   paste(emp$var, emp$level, sep = "\r"))
  n_rows  <- nrow(skeleton)
  is_fac  <- skeleton$var %in% fac_preds
  refrows <- skeleton$is_ref & is_fac
  base <- emp$emp_base[mi]; diffv <- emp$emp_diff[mi]; ratio <- emp$emp_ratio[mi]
  varv <- emp$emp_var[mi];  nv    <- emp$emp_n[mi]
  rb   <- emp$emp_ref_base[mi]; rv <- emp$emp_ref_var[mi]; rn <- emp$emp_ref_n[mi]
  # a reference level has no CI/test against itself (like the model column's zeroed reference).
  na_ref <- function(ci) { ci$inf[refrows] <- NA_real_; ci$sup[refrows] <- NA_real_
                           ci$pvalue[refrows] <- NA_real_; ci }

  if (family == "binomial") {
    # Phase 16d: the crude risk-difference companion uses the two-proportion WALD interval, matching the
    # reg's `method_diff = "wald"` (and the model AME's Wald delta interval) so the shared colour legend
    # names ONE honest method -- for a saturated single-predictor logit the AME IS this crude Wald diff.
    rd <- na_ref(ci_prop_diff(base, nv, rb, rn, conf_level = conf_level,
                              method = "wald", want_p = TRUE))              # crude risk-difference
    base_col <- fmt(pct = base, diff = diffv, n = nv, tot_n = nv,
                    ci_inf = rd$inf, ci_sup = rd$sup, pvalue = rd$pvalue,
                    type = "row", display = "pct", digits = 0L, ref = "tot", ci_type = "diff",
                    color = "diff", color_signif = color_signif, col_var = "Emp. %",
                    comp_all = FALSE, in_refrow = refrows)
    if (effect == "ame") {
      # the AME models the risk-DIFFERENCE from the reference, so the crude companion is the crude
      # risk-difference (not the OR, which the AME table does not display).
      eff_col <- fmt(pct = base, diff = diffv, n = nv, tot_n = nv,
                     ci_inf = rd$inf, ci_sup = rd$sup, pvalue = rd$pvalue,
                     type = "row", display = "diff", digits = 0L, ref = "tot",
                     ci_type = "diff", color = "diff", color_signif = color_signif,
                     col_var = "Emp. diff", comp_all = FALSE, in_refrow = refrows)
      return(list("Emp. %" = base_col, "Emp. diff" = eff_col))
    }
    or_ci <- na_ref(ci_or(base * nv, (1 - base) * nv, rb * rn, (1 - rb) * rn,   # Woolf log-OR Wald
                          conf_level = conf_level, want_p = TRUE))
    eff_col <- fmt(or = ratio, n = nv, ci_inf = or_ci$inf, ci_sup = or_ci$sup, pvalue = or_ci$pvalue,
                   type = "row", display = "or", digits = 2L,
                   ref = "1", ci_type = "or", color = "OR", color_signif = color_signif,
                   col_var = "Emp. OR", comp_all = FALSE, in_refrow = refrows)
    return(list("Emp. %" = base_col, "Emp. OR" = eff_col))
  }

  if (family == "gaussian") {
    cell <- ci_pivot(base, sqrt(varv / nv), df = nv - 1, conf_level = conf_level, want_p = FALSE)
    base_col <- fmt(mean = base, var = varv, n = nv, tot_n = nv,
                    ci_inf = cell$inf, ci_sup = cell$sup,
                    type = "mean", display = "mean", digits = 2L, ci_type = "cell",
                    color = "", color_signif = "ignore", col_var = "Emp. mean",
                    comp_all = FALSE, in_refrow = refrows)
    md <- na_ref(ci_mean_diff2(base, varv, nv, rb, rv, rn, method = "student",   # pooled t = OLS coef CI
                               conf_level = conf_level, want_p = TRUE))
    eff_col  <- fmt(diff = diffv, var = rep(var_y, n_rows), n = nv,
                    ci_inf = md$inf, ci_sup = md$sup, pvalue = md$pvalue,
                    type = "coef", display = "coef", digits = 2L, ci_type = "diff",
                    color = "diff", color_signif = color_signif, col_var = "Emp. diff",
                    comp_all = FALSE, in_refrow = refrows)
    return(list("Emp. mean" = base_col, "Emp. diff" = eff_col))
  }

  if (family == "poisson") {
    # one crude rate-ratio CI (quasi-Poisson, = the phi-scaled model's method) drives both columns:
    # Emp. rate (ratio colour, ci_type "ratio") and Emp. IRR (OR colour, ci_type "or"). Same bounds.
    rr <- na_ref(ci_mean_ratio(base, varv, nv, rb, rv, rn, method = "quasipoisson",
                               conf_level = conf_level, want_p = TRUE))
    base_col <- fmt(mean = base, ratio = ratio, n = nv, tot_n = nv,
                    ci_inf = rr$inf, ci_sup = rr$sup, pvalue = rr$pvalue,
                    type = "mean", display = "mean", digits = 2L, ref = "1", ci_type = "ratio",
                    color = "ratio", color_signif = color_signif, col_var = "Emp. rate",
                    comp_all = FALSE, in_refrow = refrows)
    eff_col  <- fmt(or = ratio, n = nv, ci_inf = rr$inf, ci_sup = rr$sup, pvalue = rr$pvalue,
                    type = "row", display = "or", digits = 2L,
                    ref = "1", ci_type = "or", color = "OR", color_signif = color_signif,
                    col_var = "Emp. IRR", comp_all = FALSE, in_refrow = refrows)
    return(list("Emp. rate" = base_col, "Emp. IRR" = eff_col))
  }
  list()
}

# Multinomial crude tooltip data (Phase 14v / 14v-ii): one column per outcome CATEGORY would explode the
# layout, so the crude companion for multinomial is TOOLTIP-only. For each FACTOR predictor level and
# each outcome category, the weighted observed proportion of that category, its difference from the
# predictor's reference level, and 14v-ii each with a crude CI (Wilson on the %, Newcombe on the diff --
# the same engines the model / tab() use). Weighted rule (§14): weighted proportions, unweighted n.
# Returns a long tibble keyed by (var, level [raw], category): prop, diff + the four CI bounds. reg_build
# turns it into the `empirical_tips` table attribute (col, var, level [displayed], tip); the render appends it.
reg_empirical_tips <- function(data, fac_preds, dependent, wt, conf_level = 0.95) {
  w    <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  yv   <- as.factor(data[[dependent]])
  cats <- levels(forcats::fct_drop(yv))
  purrr::map_dfr(fac_preds, function(p) {
    x  <- data[[p]]
    ok <- !is.na(x) & !is.na(yv) & !is.na(w)
    lv <- levels(forcats::fct_drop(as.factor(x[ok])))
    grid <- purrr::map_dfr(lv, function(l) {
      m  <- ok & x == l; wl <- sum(w[m])
      tibble::tibble(level = l, category = cats, n = sum(m),
                     prop = purrr::map_dbl(cats, ~ sum(w[m & yv == .x]) / wl))
    })
    ref_p <- stats::setNames(grid$prop[grid$level == lv[1]], grid$category[grid$level == lv[1]])
    ref_n <- grid$n[grid$level == lv[1]][1]
    grid$diff <- grid$prop - ref_p[grid$category]
    grid$var  <- p
    pw <- ci_wilson(grid$prop, grid$n, conf_level = conf_level)                  # crude % cell CI
    dd <- ci_prop_diff(grid$prop, grid$n, ref_p[grid$category], ref_n,           # crude diff CI
                       conf_level = conf_level, method = "newcombe", want_p = FALSE)
    grid$prop_inf <- pw$inf; grid$prop_sup <- pw$sup
    grid$diff_inf <- dd$inf; grid$diff_sup <- dd$sup
    grid
  })
}


# === effect = "ame" + the `at` profile axis: marginal effects + adjusted predictions (Phase 12e) ==

# The REFERENCE PROFILE (Phase 12e-ii): each predictor held at its reference -- factor / character at
# its first level (the model's treatment-contrast baseline), numeric at its mean. A named list keyed by
# predictor, fed to marginaleffects::datagrid() to evaluate effects/predictions at one representative
# row instead of sample-averaging. Caveat: a factor's first level can be an unusual baseline (e.g.
# rincome = "No answer") -- documented; `at = "average"` (the default) avoids it.
reg_reference_grid_values <- function(data, predictors) {
  vals <- lapply(predictors, function(v) {
    x <- data[[v]]
    if (is.factor(x))        levels(x)[1]
    else if (is.character(x)) sort(unique(x))[1]
    else                      mean(x, na.rm = TRUE)
  })
  stats::setNames(vals, predictors)
}

# Per-predictor marginal effects + adjusted predictions, on the RESPONSE scale, for ONE fitted model.
# Returns a keyed tidy `ame` (one row per (var, level[, group]) with `ame`/`ame_lo`/`ame_hi`/`ame_p`) and
# `pred` (each factor level's adjusted prediction; numerics have none). Alignment is by (var, level) --
# the factor contrast label is "Level - Reference" (marginaleffects), so `level` is the part before
# " - ". `newdata` (the complete-case fitted frame) is REQUIRED (marginaleffects' own data recovery
# fails past the fitting scope / on dropped levels, probed). A single-outcome glm/lm has `group = NA`;
# multinom/polr carry the outcome category in `group`.
#   at = "average"   -> avg_comparisons/avg_predictions over `data` (weighted by `wt` -> a population
#                       quantity, S14).
#   at = "reference" -> comparisons/predictions at the reference profile (a single datagrid row, so no
#                       averaging / no weights). `comparison = "lnor"` (MNL "j vs rest" OR at the
#                       profile) returns log-odds-ratios, exp()'d here into odds ratios.
reg_marginal <- function(fit, data, predictors, conf_level, wt = NULL,
                         at = "average", comparison = NULL, want_pred = TRUE,
                         want_unadj = FALSE) {
  ref_vals <- if (at == "reference") reg_reference_grid_values(data, predictors) else NULL
  ref_grid <- if (at == "reference")
    do.call(marginaleffects::datagrid, c(list(model = fit), ref_vals)) else NULL
  # weights only at the AVERAGING step; a single-row profile takes none. `wts = NULL` is rejected by
  # marginaleffects (default FALSE), so the arg is omitted when unweighted or at a profile.
  wts_arg <- if (at == "reference" || is.null(wt)) list() else list(wts = wt)
  cmp_arg <- if (is.null(comparison)) list() else list(comparison = comparison)
  do_exp  <- identical(comparison, "lnor")

  amelist <- purrr::map(predictors, function(v) {
    ac <- if (at == "reference")
      as.data.frame(do.call(marginaleffects::comparisons, c(
        list(fit, variables = v, newdata = ref_grid, conf_level = conf_level), cmp_arg)))
    else
      as.data.frame(do.call(marginaleffects::avg_comparisons, c(
        list(fit, variables = v, newdata = data, conf_level = conf_level), wts_arg, cmp_arg)))
    is_fac <- is.factor(data[[v]]) || is.character(data[[v]])
    # The factor contrast label is "<Level> - <Reference>" (difference) or
    # "ln(odds(<Level>) / odds(<Reference>))" (comparison = "lnor"). Phase 14r: strip the KNOWN prefix +
    # reference suffix instead of splitting on the FIRST " - " / first ")" -- a Level that itself
    # contains " - " (e.g. "$20000 - 24999") or ")" was otherwise truncated and failed to key the AME to
    # the skeleton, leaving an NA cell. The reference is the factor's first level (after de-ordering in
    # reg_fit). A numeric predictor keys on the variable name.
    ref_lv <- if (is_fac) levels(forcats::fct_drop(as.factor(data[[v]])))[1] else NA_character_
    level  <- if (!is_fac) v else {
      pre <- if (do_exp) "ln(odds(" else ""
      # lnor contrast = "ln(odds(<Level>) / odds(<Ref>))" -- note the DOUBLE closing paren.
      suf <- if (do_exp) paste0(") / odds(", ref_lv, "))") else paste0(" - ", ref_lv)
      substr(ac$contrast, nchar(pre) + 1L, nchar(ac$contrast) - nchar(suf))
    }
    grp    <- if ("group" %in% names(ac)) as.character(ac$group) else NA_character_
    est <- ac$estimate; lo <- ac$conf.low; hi <- ac$conf.high
    if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }   # lnor -> OR (and its CI)
    tibble::tibble(var = v, level = as.character(level), group = grp,
                   ame = est, ame_lo = lo, ame_hi = hi, ame_p = ac$p.value)
  })
  ame <- dplyr::bind_rows(amelist)

  predlist <- if (want_pred) purrr::map(predictors, function(v) {
    if (!(is.factor(data[[v]]) || is.character(data[[v]]))) return(NULL)  # no per-level pred for numerics
    ap <- if (at == "reference") {
      grid_v <- do.call(marginaleffects::datagrid, c(list(model = fit),
        utils::modifyList(ref_vals, stats::setNames(list(levels(as.factor(data[[v]]))), v))))
      as.data.frame(marginaleffects::predictions(fit, newdata = grid_v, conf_level = conf_level))
    } else {
      # Change A (decisions doc S50): the adjusted % is the marginal-STANDARDIZED prediction --
      # `variables = v` sets v to each level for the WHOLE sample (keeping every other covariate as
      # observed) and averages = g-computation / direct standardization. This is the covariate-adjusted
      # quantity that COHERES with the AME (adjusted%(ref) + AME(level) == adjusted%(level)); `by = v`
      # would instead reproduce the estimation-sample OBSERVED rate (score-equation identity) and is not
      # adjusted. `want_unadj` additionally returns that observed-group prediction as a control (change D).
      as.data.frame(do.call(marginaleffects::avg_predictions, c(
        list(fit, variables = v, newdata = data, conf_level = conf_level), wts_arg)))
    }
    grp <- if ("group" %in% names(ap)) as.character(ap$group) else NA_character_
    tibble::tibble(var = v, level = as.character(ap[[v]]), group = grp, pred = ap$estimate)
  }) else list()
  pred <- dplyr::bind_rows(purrr::compact(predlist))

  # Change D (decisions doc S50): the model's UNADJUSTED predicted % = avg_predictions(by = v), the
  # observed-group average (each observation keeps its own covariates, grouped by its actual level). By the
  # logit score-equation identity this reproduces the estimation-sample observed rate, so it equals the
  # same-frame `Emp. %` exactly -- a control shown beside the adjusted (standardized) prediction. Only for
  # at = "average" factor predictors (a reference profile has no observed-group average).
  pred_unadj <- if (want_unadj && at != "reference") {
    ul <- purrr::map(predictors, function(v) {
      if (!(is.factor(data[[v]]) || is.character(data[[v]]))) return(NULL)
      ap <- as.data.frame(do.call(marginaleffects::avg_predictions, c(
        list(fit, by = v, newdata = data, conf_level = conf_level), wts_arg)))
      grp <- if ("group" %in% names(ap)) as.character(ap$group) else NA_character_
      tibble::tibble(var = v, level = as.character(ap[[v]]), group = grp, pred = ap$estimate)
    })
    dplyr::bind_rows(purrr::compact(ul))
  } else NULL

  list(ame = ame, pred = pred, pred_unadj = pred_unadj)
}

# Build ONE fmt column from a reg_marginal() result, for a given outcome `group` (NA for a single-outcome
# glm/lm/poisson). Aligns the effect (+ adjusted prediction) to the shared (var, level) skeleton. `shape`
# picks the fmt shape: "prob" (probability-scale AME) composes AME-first "{diff} ({pct})" (reference
# level -> "({pct})", numeric -> "diff"); "raw" (gaussian/poisson) a plain "coef"; "or" (MNL j-vs-rest
# OR at the profile) the multiplicative "or" shape (reference -> 1, no prediction). Reference levels +
# the Constant carry no effect; predictors ABSENT from this model stay NA (empty cells).
reg_marginal_column <- function(skeleton, marg, model_predictors, numeric_preds, shape, var_y,
                                nobs, group, color, color_signif, col_var, or_tip = NULL) {
  amt <- marg$ame; prd <- marg$pred
  if (!is.na(group)) {
    amt <- amt[!is.na(amt$group) & amt$group == group, , drop = FALSE]
    if (nrow(prd)) prd <- prd[!is.na(prd$group) & prd$group == group, , drop = FALSE]
  }
  key   <- paste(skeleton$var, skeleton$level, sep = "\r")
  a_key <- paste(amt$var, amt$level, sep = "\r")
  m     <- match(key, a_key)
  ame_v <- amt$ame[m]; lo_v <- amt$ame_lo[m]; hi_v <- amt$ame_hi[m]; p_v <- amt$ame_p[m]
  pred_v <- if (nrow(prd)) prd$pred[match(key, paste(prd$var, prd$level, sep = "\r"))]
            else            rep(NA_real_, nrow(skeleton))

  n_rows   <- nrow(skeleton)
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_const <- skeleton$var == "Constant"
  is_ref   <- skeleton$is_ref & !is_const & in_model
  is_num   <- skeleton$var %in% numeric_preds & in_model
  refrows  <- is_ref | is_const

  # "blank" (not NA) for the Constant / out-of-model cells: an NA display falls back to get_n() in
  # get_num(), so it must be an explicit blank-token (renders "") rather than left unset.
  display <- rep("blank", n_rows)
  if (shape == "prob") {
    compos <- in_model & !is_const & !is_ref & !is_num & !is.na(ame_v) & !is.na(pred_v)
    display[compos]                    <- "{diff} ({pct})"     # non-ref factor level: AME (prediction)
    display[in_model & is_ref & !is.na(pred_v)] <- "({pct})"   # reference level: prediction only
    display[in_model & is_num & !is.na(ame_v)]  <- "diff"      # numeric predictor: bare AME
    ame_v[is_ref] <- NA_real_                                  # reference has no marginal effect
    # Phase 14r (E): carry the model OR (coefficient path) in the `or` field so cond_or surfaces it on
    # hover though the cell DISPLAYS the AME. Read-only: the AME display / colour never read `or`, so it
    # is inert everywhere but the tooltip. NA on the reference (which shows "ref").
    or_v <- if (is.null(or_tip)) NA_real_ else or_tip
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): no misleading whole-model N (see the empirical cols)
      pct = pred_v, diff = ame_v, or = or_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      type = "row", display = display, digits = 1L, ci_type = "diff",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows
    )
  } else if (shape == "or") {                                  # MNL "j vs rest" OR at the profile
    display[in_model & !is_const & !is.na(ame_v)] <- "or"
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    display[in_model & is_ref] <- "or"
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): no misleading whole-model N (see the empirical cols)
      or = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      type = "row", display = display, digits = 2L, ref = "1", ci_type = "or",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows
    )
  } else {                                                     # "raw" (gaussian / poisson)
    display[in_model & !is_const & !is.na(ame_v)] <- "coef"    # raw AME (gaussian == coef; poisson count)
    ame_v[is_ref] <- 0                                         # additive neutral at the reference
    display[in_model & is_ref]      <- "coef"
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): no misleading whole-model N (see the empirical cols)
      diff = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      var = rep(var_y, n_rows),                               # var(Y): standardizes the effect-size colour
      type = "coef", display = display, digits = 2L, ci_type = "diff",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows
    )
  }
}

# Change D (decisions doc S50): the model's UNADJUSTED predicted % as a plain (uncoloured) `pct` column,
# aligned to the skeleton. Value = avg_predictions(by = level) (marg$pred_unadj) = the observed-group rate,
# which equals the same-frame `Emp. %` exactly (score-equation identity) -- a verification control beside
# the AME cell's adjusted %. Factor levels IN the model only; numeric / Constant / out-of-model -> blank.
reg_unadj_column <- function(skeleton, marg, model_predictors, col_var) {
  prd <- marg$pred_unadj
  key <- paste(skeleton$var, skeleton$level, sep = "\r")
  pv  <- if (!is.null(prd) && nrow(prd))
           prd$pred[match(key, paste(prd$var, prd$level, sep = "\r"))]
         else rep(NA_real_, nrow(skeleton))
  in_model <- skeleton$var %in% model_predictors
  is_const <- skeleton$var == "Constant"
  display  <- rep("blank", nrow(skeleton))
  display[in_model & !is_const & !is.na(pv)] <- "pct"
  fmt(n = rep(NA_integer_, nrow(skeleton)),
      pct = pv, type = "row", display = display, digits = 0L,
      color = "", color_signif = "ignore", col_var = col_var,
      comp_all = FALSE, in_refrow = skeleton$is_ref & in_model & !is_const)
}

# Split ONE multinomial fit into one OR column per non-reference outcome category. Each category's
# tidy rows (`y.level == j`, y.level dropped) look like a standard glm tidy, so reg_column() aligns
# them to the shared predictor skeleton unchanged. Label = "<j> vs <ref>: OR" (prefixed by the
# dependent when several dependents / models coexist, to disambiguate). Returns a list of {label, col}.
reg_columns_multinom <- function(skeleton, f, sp, effect_shape, color, color_signif,
                                 eff_word, cleannames, prefix_dep) {
  y_ref <- if (cleannames) stringr::str_remove_all(f$y_ref, cleannames_condition()) else f$y_ref
  purrr::map(f$y_levels, function(j) {
    sub      <- f
    sub$tidy <- f$tidy[f$tidy$y.level == j,
                       setdiff(names(f$tidy), "y.level"), drop = FALSE]
    jc  <- if (cleannames) stringr::str_remove_all(j, cleannames_condition()) else j
    lab <- paste0(if (prefix_dep) paste0(sp$dependent, " - ") else "",
                  jc, " vs ", y_ref)
    # Phase 14s (G) + 14w (item 3): every category column of ONE model shares `sp$label` ("<dep>: OR")
    # as its col_var, so no border is drawn between them (borders separate DIFFERENT col_vars) and the
    # model name + effect span them once. The repeated ": OR" is stripped from the per-category NAME.
    list(label = lab, emp_key = j,   # emp_key: raw category, for the empirical tooltip (Phase 14v)
         col   = reg_column(skeleton, sub, sp$predictors, sp$label, effect_shape, color, color_signif))
  })
}

# === Model-summary footer (Phase 12f): GOF stats stored in the `test` attribute ==================
# The regression GOF is stored in the SAME whole-table `test` tibble crosstabs use (schema
# new_test_tibble(): row_var/col_var/test/statistic/df1/df2/pvalue/n/min_e), adding ROWS with
# NEW `test` discriminators that never collide with the crosstab "chi2"/"F_welch"/"F_classic" -- so
# test_display_rows() (chi2/F only) makes the summary block / tab_pvalue_lines() auto-no-op on a reg table,
# and the reg renderers (R/tab_classes.R) auto-no-op on a crosstab. Value-stats (n/r2/aic/...) carry
# the number in `statistic` (pvalue NA); test-stats (lr_null/f_model/wald_null/compare_*) carry
# statistic + df + pvalue. `col_var` = the model's FIRST output column label (MNL/ordinal place the
# footer under the first category column). The footer is DISPLAY-ONLY (never baked into the fmt
# columns), materialised by R/tab_classes.R at print / export.

# The null-model log-likelihood, for LR-vs-null + McFadden -- robust to the fitting scope. glm
# (binomial/poisson) is ANALYTIC from the stored null.deviance (no refit, no `update()` env fragility:
# ll_0 = ll_full - LR/2 where LR = null.deviance - deviance); multinom/polr refit the intercept-only
# model on the stored model frame. Returns NULL when the null can't be recovered.
reg_null_loglik <- function(fit, family) {
  if (family %in% c("binomial", "poisson") &&
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

# Pearson dispersion (over/under-dispersion): poisson / grouped binomial only -- the dispersion
# parameter is not identifiable for ungrouped Bernoulli data. phi = Sum(pearson resid^2) / df.residual
# (better-behaved than deviance/df). PURE (14v-ii): the over-dispersion warning moved to reg_fit(),
# where the SEs are now actually scaled by sqrt(phi) -- so it is emitted ONCE per fit (this helper is
# also called by reg_glance for the footer, which must stay silent).
reg_dispersion <- function(fit) {
  rp  <- tryCatch(stats::residuals(fit, type = "pearson"), error = function(e) NULL)
  dfr <- tryCatch(stats::df.residual(fit), error = function(e) NA_real_)
  if (is.null(rp) || is.na(dfr) || dfr <= 0) return(NA_real_)
  sum(rp^2, na.rm = TRUE) / dfr
}

# AIC as a single number. AIC.svyglm (Rao-Scott survey AIC) returns a NAMED vector
# c(eff.p, AIC, deltabar) -> take the "AIC" element; a plain glm/lm AIC is already scalar.
reg_aic_value <- function(fit) {
  a <- tryCatch(suppressWarnings(stats::AIC(fit)), error = function(e) NA_real_)
  if (length(a) > 1L && !is.null(names(a)) && "AIC" %in% names(a)) return(as.numeric(a[["AIC"]]))
  as.numeric(a)[1]
}

# GOF stats for ONE fit -> a tidy tibble (test, statistic, df1, df2, pvalue) in the reg-footer
# vocabulary. Dependency-light: broom::glance (lm) + base logLik/AIC/BIC + the analytic/refit null.
# quasi* / svyglm have no true likelihood -> those stats stay NA / a relabelled Rao-Scott Wald (survey),
# never a false LR. `nobs` comes from the fit_res (multinom has no stats::nobs()).
reg_glance <- function(fit, family, grouped, weighted, nobs) {
  row <- function(test, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_, pvalue = NA_real_)
    tibble::tibble(test = test, statistic = statistic, df1 = df1, df2 = df2, pvalue = pvalue)
  out <- row("n", statistic = as.numeric(nobs))

  if (weighted) {
    # svyglm: no true likelihood -> Rao-Scott Wald-vs-null (relabelled) + Nagelkerke pseudo-R2 + AIC.
    # survey's psrsq / AIC emit "rsquared may be wrong" / "zero weight" notes under scaled weights; these
    # are inherent approximations of a survey summary, not user-actionable -> suppressed (the footer is a
    # descriptive summary, not the primary design-based inference).
    # svy_vglm (weighted MNL) has no terms component -> the Wald-vs-null degrades away (footer = n only).
    terms_all <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
    wt <- if (length(terms_all) > 0)
      tryCatch(suppressWarnings(survey::regTermTest(fit, stats::reformulate(terms_all))),
               error = function(e) NULL)
    else NULL
    if (!is.null(wt)) out <- dplyr::bind_rows(out, row("wald_null",
      statistic = as.numeric(wt$Ftest), df1 = as.numeric(wt$df), df2 = as.numeric(wt$ddf),
      pvalue = as.numeric(wt$p)))
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
    g <- tryCatch(broom::glance(fit), error = function(e) NULL)
    if (!is.null(g)) out <- dplyr::bind_rows(out,
      row("r2",      statistic = g$r.squared),
      row("r2_adj",  statistic = g$adj.r.squared),
      row("f_model", statistic = g$statistic, df1 = g$df, df2 = g$df.residual, pvalue = g$p.value),
      row("sigma",   statistic = g$sigma),
      row("aic",     statistic = g$AIC),
      row("bic",     statistic = g$BIC))
    return(out)
  }

  # glm binomial/poisson + multinom/polr: LR-vs-null + McFadden from the null log-likelihood; AIC/BIC.
  # quasi* (no logLik) -> those stay NA (footer shows N + dispersion).
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
  if (family == "poisson" || grouped) {
    phi <- reg_dispersion(fit)
    if (!is.na(phi)) out <- dplyr::bind_rows(out, row("dispersion", statistic = phi))
  }
  # Phase 14q (Item I): the Brant proportional-odds omnibus p, computed once at fit time and stashed on
  # the fit (reg_fit_ordinal). Weighted ordinal (svyolr) has no Brant fit -> the attr is absent -> skip.
  if (family == "ordinal" && !weighted) {
    bp <- attr(fit, "brant_po")
    if (!is.null(bp) && !is.na(bp)) out <- dplyr::bind_rows(out, row("brant_po", pvalue = bp))
  }
  out
}

# Resolve the `stats=` argument -> the ordered set of footer discriminators. Per-context defaults:
# glm -> n/lr_null/mcfadden_r2/aic/bic (+dispersion for poisson/grouped); lm -> n/r2/r2_adj/f_model/
# sigma; weighted -> n/wald_null/nagelkerke_r2/aic. A character vector overrides (keeping its order,
# valid names only); FALSE / "none" suppresses the footer; NULL / "all" / TRUE = the default set.
reg_footer_stats <- function(family, weighted, grouped, stats) {
  default <- if (weighted) c("n", "wald_null", "nagelkerke_r2", "aic")
    else if (family == "gaussian") c("n", "r2", "r2_adj", "f_model", "sigma")
    else { s <- c("n", "lr_null", "mcfadden_r2", "aic", "bic")
           if (family == "poisson" || grouped) s <- c(s, "dispersion")
           if (family == "ordinal") s <- c(s, "brant_po"); s }  # Phase 14q Item I
  if (is.null(stats) || identical(stats, "all") || isTRUE(stats)) return(default)
  if (isFALSE(stats) || identical(stats, "none")) return(character(0))
  valid <- c("n", "lr_null", "wald_null", "mcfadden_r2", "nagelkerke_r2", "cox_snell_r2",
             "r2", "r2_adj", "f_model", "sigma", "aic", "bic", "dispersion", "brant_po")
  stats[stats %in% valid]
}

# Assemble the whole-table `test` tibble for a regression table: one row per (fit's first column x
# footer stat), in new_test_tibble() schema. `fit_first_col` = the fmt column each fit is keyed under
# (MNL/ordinal -> the first category column). `grouped_by_fit` marks grouped-binomial fits (dispersion).
reg_gof_tibble <- function(fits, fit_first_col, family, weighted, grouped_by_fit, stats, nobs_by_fit) {
  rows <- purrr::map(seq_along(fits), function(i) {           # integer index (fits may be NAMED)
    f    <- fits[[i]]
    keep <- reg_footer_stats(family, weighted, isTRUE(grouped_by_fit[[i]]), stats)
    if (length(keep) == 0) return(NULL)                        # stats = FALSE -> no glance, no warnings
    # Phase 15b: the reref fast path carries the reference-invariant glance in `f$glance` (the raw fit
    # was discarded); a real reg_fit() result has no `$glance` -> compute from `f$fit` as before.
    g    <- if (!is.null(f$glance)) f$glance
            else reg_glance(f$fit, family, isTRUE(grouped_by_fit[[i]]), weighted, nobs_by_fit[[i]])
    g    <- g[g$test %in% keep, , drop = FALSE]
    g    <- g[order(match(g$test, keep)), , drop = FALSE]        # spec order
    if (nrow(g) == 0) return(NULL)
    tibble::tibble(row_var = "", col_var = fit_first_col[[i]], test = g$test,
                   statistic = g$statistic, df1 = g$df1, df2 = g$df2, pvalue = g$pvalue,
                   n = as.numeric(nobs_by_fit[[i]]), min_e = NA_real_)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(new_test_tibble())
  dplyr::bind_rows(rows)
}

# --- Multi-model comparison (Phase 12f-ii): each model column vs a baseline / the previous model ----
# The nesting / same-N guard mirrors anova()'s own error: an LR / F test between two models is only
# valid on the SAME complete-case set (differing predictor missingness silently changes N) and when
# one model nests in the other. On a guard failure the comparison falls back to Delta-AIC + a message.
# Phase 14u (L2): nesting is checked in BOTH directions -- `1L` = m_ref is the sub-model
# (anova(m_ref, m_full)), `-1L` = m_full is the sub-model (baseline is the SUPERSET, e.g. a "complete"
# model tested against each smaller one), `0L` = not nested / N differs. Testing only `t_ref %in%
# t_full` (as before) wrongly rejected a superset baseline as non-nested -> the AIC fallback.
reg_compare_guard <- function(m_ref, m_full) {
  ok_n   <- tryCatch(stats::nobs(m_ref) == stats::nobs(m_full), error = function(e) FALSE)
  t_ref  <- tryCatch(attr(stats::terms(m_ref),  "term.labels"), error = function(e) NULL)
  t_full <- tryCatch(attr(stats::terms(m_full), "term.labels"), error = function(e) NULL)
  if (is.null(t_ref) || is.null(t_full) || !isTRUE(ok_n)) return(0L)
  if (all(t_ref %in% t_full)) return(1L)                  # ref nested in full
  if (all(t_full %in% t_ref)) return(-1L)                 # full nested in ref (superset baseline)
  0L
}

# Phase 14u (L1): the predictor row order for a model-COMPARISON. If one model's predictor set is a
# superset of every other model's (a "complete" model, hence of the whole union), keep THAT model's own
# predictor order; otherwise first-appearance order (the historical behaviour). Downstream keys by
# (var, level)/term and follows the skeleton's fct_inorder, so reordering the union is the whole change.
reg_order_union <- function(models) {
  sets  <- purrr::map(models, unique)
  all_u <- unique(purrr::flatten_chr(sets))
  complete_i <- which(purrr::map_lgl(sets, function(s) all(all_u %in% s)))
  if (length(complete_i) > 0L) unique(sets[[complete_i[length(complete_i)]]]) else all_u
}

# Pull statistic / df / p from an anova() comparison table (last row): glm "Chisq" (Deviance + Df +
# Pr(>Chi)) vs lm/quasi "F" (F + Df + Res.Df + Pr(>F)).
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

# Append one comparison row per model column to the GOF `test` tibble. `compare = "baseline"` tests
# each column vs the `baseline=` column (default the first); `"sequential"` vs the previous column.
# LR (Chisq) for binomial/poisson/multinomial/ordinal, F for gaussian/quasi; for WEIGHTED models a
# design-based Wald test (anova.svyglm, method="Wald") -> the "compare_*_wald" discriminator, so no
# false-LR claim is made under survey sampling. Guard failure -> a Delta-AIC row (test "compare_*_aic",
# a value stat -- Rao-Scott AIC.svyglm under weights) + a one-time message. Single-column tables no-op.
# Distinct discriminators per test kind keep each footer row homogeneous (all LR / F / Wald / Delta-AIC)
# so the row label alone names the test -- no in-cell label needed.
reg_compare_rows <- function(reg_gof, fits, specs, family, weighted, fit_first_col,
                             compare = "none", baseline = NULL, conf_level = 0.95) {
  if (identical(compare, "none")) return(reg_gof)
  n <- length(fits)
  if (n < 2L) {
    cli::cli_inform(c("i" = paste0("{.arg compare} needs at least two models (a {.arg predictors} list ",
                                   "or several dependents); ignored.")))
    return(reg_gof)
  }
  use_f  <- family %in% c("gaussian", "quasipoisson")
  base_i <- if (compare == "baseline") {
    if (is.null(baseline))          1L
    else if (is.numeric(baseline))  as.integer(baseline)
    else                            match(baseline, purrr::map_chr(specs, "label"))
  } else NA_integer_
  if (compare == "baseline" && (is.na(base_i) || base_i < 1L || base_i > n)) {
    cli::cli_warn("{.arg baseline} {.val {baseline}} matches no model; using the first.")
    base_i <- 1L
  }

  row <- function(test, col_var, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_,
                  pvalue = NA_real_, nobs = NA_real_)
    tibble::tibble(row_var = "", col_var = col_var, test = test, statistic = statistic,
                   df1 = df1, df2 = df2, pvalue = pvalue, n = nobs, min_e = NA_real_)

  tag  <- if (compare == "sequential") "seq" else "baseline"
  rows <- purrr::map(seq_len(n), function(i) {
    ref_i <- if (compare == "sequential") i - 1L else base_i
    if (is.na(ref_i) || ref_i < 1L || ref_i == i) return(NULL)
    m_full <- fits[[i]]$fit; m_ref <- fits[[ref_i]]$fit
    col    <- fit_first_col[[i]]
    # Phase 14u (L2): anova() needs the SUB-model first; reg_compare_guard tells us which of the two it
    # is (a superset baseline flips the order). The LR/F statistic + p test the extra term(s) either way.
    dir  <- reg_compare_guard(m_ref, m_full)
    m_lo <- if (dir >= 0L) m_ref  else m_full
    m_hi <- if (dir >= 0L) m_full else m_ref
    if (dir != 0L) {
      if (weighted) {
        # design-based Wald test on the extra term(s): anova.svyglm(method="Wald") -> a regTermTest
        # ($Ftest/$df/$ddf/$p), the same object reg_glance's Wald-vs-null uses.
        e <- tryCatch({
          an <- stats::anova(m_lo, m_hi, method = "Wald", test = "F")
          list(stat = as.numeric(an$Ftest), df1 = as.numeric(an$df),
               df2 = as.numeric(an$ddf), p = as.numeric(an$p))
        }, error = function(e) NULL)
        if (!is.null(e) && !is.na(e$p)) {
          return(row(paste0("compare_", tag, "_wald"), col, statistic = e$stat, df1 = e$df1,
                     df2 = e$df2, pvalue = e$p, nobs = fits[[i]]$nobs))
        }
      } else {
        an <- tryCatch(stats::anova(m_lo, m_hi, test = if (use_f) "F" else "Chisq"),
                       error = function(e) NULL)
        if (!is.null(an)) {
          e <- reg_compare_extract(an, use_f)
          if (!is.na(e$p)) {
            disc <- if (use_f) paste0("compare_", tag, "_f") else paste0("compare_", tag)
            return(row(disc, col, statistic = e$stat, df1 = e$df1, df2 = e$df2, pvalue = e$p,
                       nobs = fits[[i]]$nobs))
          }
        }
      }
    }
    daic <- tryCatch(reg_aic_value(m_full) - reg_aic_value(m_ref), error = function(e) NA_real_)
    cli::cli_inform(c("i" = paste0(
      "Column {.val {col}}: models are not nested or N differs -> showing the AIC difference vs the ",
      "{if (compare == 'sequential') 'previous' else 'baseline'} model instead of a likelihood-ratio test.")))
    row(paste0("compare_", tag, "_aic"), col, statistic = daic, nobs = fits[[i]]$nobs)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}


# === Phase 15b: jamovi live-UI fit cache -- digest + reference reparametrization =================
# A factor-predictor reference change is a LINEAR reparametrization of the SAME fit (likelihood,
# fitted values and dispersion are invariant), so the whole table at any reference is recomputable
# from the canonical fit's coefficients + covariance -- NO refit (the reg analogue of jmvtab's
# jmv_tab3_reref). reg_build_digest() fits ONCE at the natural-first-level (canonical) reference and
# returns a small, reference-INDEPENDENT digest (coef + vcov + the reference-invariant glance +
# scalars); reg_reref_fit_res() reparametrizes it to any display reference, producing a fit_res
# drop-in for reg_column() / reg_gof_tibble(). Reached ONLY with .fit_cache present, on the
# single-equation GLM coefficient path (method="wald", value/ci display, no split/multiplier/trials
# /compound/ame/mnl-vs-rest). Locked byte-identical to a real refit by test-jmvtab-reg-cache.R.

# Critical value for a Wald interval: z for a fixed-dispersion glm (binomial/poisson, unweighted,
# unscaled), else t on the residual df -- the same rule reg_fit()'s Wald else-branch uses.
reg_wald_crit <- function(disp_known, df_residual, conf_level) {
  if (disp_known) stats::qnorm(1 - (1 - conf_level) / 2)
  else            stats::qt(1 - (1 - conf_level) / 2, df = df_residual)
}

# Fit ONCE at the canonical (natural-first-level) reference and distil the reference-independent
# quantities. reg_fit() de-orders factor predictors + drops NA rows deterministically, so the
# canonical coefficient basis does not depend on `reference`. The raw fit is DISCARDED (only coef /
# vcov / scalars / glance are kept -- kilobytes, not the megabyte model object).
reg_build_digest <- function(data, sp, family, design_spec, do_exp, inverse_two_level_factors,
                             conf_level, weighted) {
  f   <- reg_fit(data, sp$dependent, sp$predictors, family, design_spec, do_exp,
                 inverse_two_level_factors, conf_level, method = "wald",
                 trials = sp$trials, formula = sp$formula, multiplier = NULL)
  fit <- f$fit
  coef_v <- stats::coef(fit)
  V      <- stats::vcov(fit)
  names(coef_v) <- stringr::str_remove_all(names(coef_v), "`")   # match skeleton terms (as reg_fit does)
  dn <- stringr::str_remove_all(rownames(V), "`")
  dimnames(V) <- list(dn, dn)
  grouped    <- family == "binomial" && !is.null(sp$trials) && is.null(sp$formula)
  over_disp  <- !weighted && (family == "poisson" || grouped)
  phi        <- if (over_disp) reg_dispersion(fit) else NA_real_
  scaled     <- over_disp && !is.na(phi) && phi > 0
  disp_known <- !weighted && family %in% c("binomial", "poisson") && !scaled
  list(coef = coef_v, vcov = V, df_residual = stats::df.residual(fit),
       phi = phi, scaled = scaled, disp_known = disp_known, do_exp = do_exp,
       var_y = f$var_y, positive_level = f$positive_level, nobs = f$nobs,
       glance = reg_glance(fit, family, grouped, weighted, f$nobs), family = family)
}

# Reparametrize a canonical digest to the DISPLAY reference encoded in `skeleton` (built on the
# releveled data). Each display term is a linear contrast L over the canonical coefficients: a factor
# level j vs the display reference r is L = e_{p j} - e_{p r} (a canonical term absent = the canonical
# first level = a 0 column); the intercept at the display profile is e_0 + sum_p e_{p r_p}; a numeric
# predictor is the identity. estimate = L'b, se = sqrt(L' V L); then the SAME Wald finalize reg_fit()
# uses (phi scaling, z/t crit, p as the CI's dual, exp) -> byte-identical to a real refit-at-r.
reg_reref_fit_res <- function(digest, reference, sp, skeleton, conf_level) {
  coef_v <- digest$coef
  V      <- digest$vcov
  cn     <- names(coef_v)
  preds  <- setdiff(unique(skeleton$var), "Constant")
  ref_of <- stats::setNames(vapply(preds, function(p) {
    r <- skeleton$level[skeleton$var == p & skeleton$is_ref]
    if (length(r)) as.character(r[[1]]) else NA_character_
  }, character(1)), preds)

  rows <- skeleton[!is.na(skeleton$term), , drop = FALSE]        # (Intercept) + non-reference terms
  n    <- nrow(rows)
  est  <- numeric(n); se <- numeric(n)
  for (i in seq_len(n)) {
    p <- rows$var[i]; t <- rows$term[i]
    L <- stats::setNames(numeric(length(cn)), cn)
    if (identical(t, "(Intercept)")) {
      if ("(Intercept)" %in% cn) L["(Intercept)"] <- 1
      for (pp in preds) {
        if (is.na(ref_of[[pp]])) next
        rn <- paste0(pp, ref_of[[pp]])
        if (rn %in% cn) L[rn] <- L[rn] + 1
      }
    } else if (p %in% preds && !is.na(ref_of[[p]])) {            # factor level j vs display ref r_p
      rn <- paste0(p, ref_of[[p]])
      if (t  %in% cn) L[t]  <- L[t]  + 1
      if (rn %in% cn) L[rn] <- L[rn] - 1
    } else if (t %in% cn) {                                      # numeric predictor: identity
      L[t] <- 1
    }
    est[i] <- sum(L * coef_v)
    se[i]  <- sqrt(as.numeric(t(L) %*% V %*% L))
  }
  if (isTRUE(digest$scaled)) se <- se * sqrt(digest$phi)
  crit <- reg_wald_crit(digest$disp_known, digest$df_residual, conf_level)
  lo   <- est - crit * se
  hi   <- est + crit * se
  p    <- if (digest$disp_known) 2 * stats::pnorm(-abs(est / se))
          else                   2 * stats::pt(-abs(est / se), df = digest$df_residual)
  est_o <- est
  if (isTRUE(digest$do_exp)) { est_o <- exp(est_o); lo <- exp(lo); hi <- exp(hi) }
  list(tidy = tibble::tibble(term = rows$term, estimate = est_o,
                             conf.low = lo, conf.high = hi, p.value = p),
       nobs = digest$nobs, var_y = digest$var_y, positive_level = digest$positive_level,
       glance = digest$glance, fit = NULL, data = NULL)
}


# The shared builder: fit every column spec, align to one skeleton, assemble a grouped_tab. specs =
# list of list(dependent, predictors, label, trials, formula, compound). The data-skeleton (union of
# the specs' predictors) is used unless a spec is a compound formula (single model), in which case the
# skeleton is read from its fitted terms (reg_skeleton_from_fit). Fit-all first so the skeleton can
# come from the fit before the columns are aligned. A multinomial fit contributes SEVERAL columns
# (one per outcome category), so the per-spec columns are flattened into one (label, col) list.
reg_build <- function(data, specs, union_predictors, family, design_spec, weighted, do_exp, effect_shape,
                      inverse_two_level_factors, conf_level, method, color, color_signif,
                      cleannames, subtext, eff_word, effect = "coefficient", at = "average",
                      stats = NULL, compare = "none", baseline = NULL, split_var = NULL,
                      multiplier = NULL, empirical = FALSE, predicted_unadjusted = FALSE,
                      estimate_display = "value",
                      .fit_cache = NULL, reference = NULL, reref = FALSE,
                      skeleton_data = data) {
  # split_var (Phase 12g): the regression analogue of tab()'s tab_vars -- fit the SAME model(s) within
  # each level of a grouping variable and STACK the per-group tables into one grouped_tab (grouped by
  # split_var + var), so tab_spread(split_var) can pivot the groups into side-by-side columns. Each group
  # is a recursive reg_build on its data subset, sharing ONE skeleton (skeleton_data = the full data) so
  # every group has the same rows/columns (a level absent in a group -> empty cells). split_var is placed
  # FIRST so `levels` stays the last factor column -> tab_get_vars()/tab_spread() see row_var = "levels",
  # tab_vars = c(split_var, "var") with no change to the crosstab spread machinery.
  if (!is.null(split_var)) {
    sl <- levels(forcats::fct_drop(as.factor(data[[split_var]])))
    parts <- purrr::map(sl, function(g) {
      gmask <- !is.na(data[[split_var]]) & data[[split_var]] == g
      sub   <- data[gmask, , drop = FALSE]
      ds_g  <- design_spec
      if (!is.null(design_spec$design)) ds_g$design <- design_spec$design[gmask, ]  # subset the design
      tg  <- reg_build(sub, specs, union_predictors, family, ds_g, weighted, do_exp, effect_shape,
                       inverse_two_level_factors, conf_level, method, color, color_signif,
                       cleannames, subtext, eff_word, effect, at, stats, compare, baseline,
                       split_var = NULL, multiplier = multiplier, empirical = empirical,
                       predicted_unadjusted = predicted_unadjusted,
                       estimate_display = estimate_display, .fit_cache = .fit_cache,
                       reference = NULL, reref = FALSE, skeleton_data = data)
      tst <- get_test(tg); if (!is.null(tst) && nrow(tst) > 0) tst$row_var <- as.character(g)
      list(data = tibble::add_column(tibble::as_tibble(dplyr::ungroup(tg)),
                                     "{split_var}" := factor(g, levels = sl), .before = 1L),
           test = tst)
    })
    combined <- vctrs::vec_rbind(!!!purrr::map(parts, "data"))
    tests    <- purrr::list_rbind(purrr::compact(purrr::map(parts, "test")))
    if (is.null(tests) || nrow(tests) == 0) tests <- new_test_tibble()
    return(
      combined |>
        new_tab(subtext = subtext, test = tests,
                ci_settings = list(conf_level = conf_level, method_cell = NA_character_,
                                   method_diff = method)) |>
        dplyr::group_by(!!rlang::sym(split_var), var)
    )
  }

  # Phase 15b jamovi live reref: `data` arrives at the CANONICAL (natural-first) reference; fit the
  # digest once on it (cached, reference-independent) and reparametrize to the display `reference`,
  # which is baked into the skeleton (built on the releveled data). `data` is releveled here so the
  # skeleton + empirical companions use the display reference, while `data_canon` fits the digest.
  data_canon <- data
  skeleton <- NULL
  if (isTRUE(reref)) {
    if (!is.null(reference)) data <- reg_apply_references(data, reference, union_predictors)
    skeleton <- reg_skeleton(data, union_predictors)
    fits <- purrr::map(specs, function(sp) {
      digest <- jmvreg_cached(
        .fit_cache, "digest", jmvreg_fit_key(sp, data_canon, family, design_spec),
        function() reg_build_digest(data_canon, sp, family, design_spec, do_exp,
                                    inverse_two_level_factors, conf_level, weighted))
      reg_reref_fit_res(digest, reference, sp, skeleton, conf_level)
    })
  } else {
    fits <- purrr::map(specs, function(sp) {
      thunk <- function() reg_fit(data, sp$dependent, sp$predictors, family, design_spec, do_exp,
                                  inverse_two_level_factors, conf_level, method,
                                  trials = sp$trials, formula = sp$formula, multiplier = multiplier)
      # .fit_cache present but not on the reref path (ame / profile / mnl-vs-rest / compound): cache the
      # RAW reg_fit result keyed on the (already display-referenced) data -> a reference change refits.
      if (is.null(.fit_cache)) thunk()
      else jmvreg_cached(.fit_cache, "fit",
                         jmvreg_fit_key(sp, data, family, design_spec,
                                        extra = list(method, do_exp, conf_level, effect, at,
                                                     estimate_display, multiplier)),
                         thunk)
    })
  }

  # marginaleffects paths (effect="ame", and the MNL "j vs rest" OR at the reference profile) always key
  # by the ORIGINAL variables, so a compound formula still gets a clean bare-variable skeleton; the plain
  # coefficient path keeps its fit-read skeleton for compound terms. `skeleton_data` (Phase 12g split_var)
  # is the FULL data so every split group shares one skeleton (missing group levels -> empty cells); it
  # defaults to `data`, so non-split builds are unchanged.
  compound   <- purrr::map_lgl(specs, ~ isTRUE(.$compound))
  mnl_vsrest <- effect == "coefficient" && at == "reference" && family == "multinomial"
  if (is.null(skeleton))
    skeleton <- if (effect == "ame" || mnl_vsrest) reg_skeleton(skeleton_data, union_predictors)
                else if (any(compound))            reg_skeleton_from_fit(fits[[1]]$fit)
                else                               reg_skeleton(skeleton_data, union_predictors)

  multi_col     <- family == "multinomial"
  prefix_dep    <- length(specs) > 1L
  # Phase 14w: a model COMPARISON (several models, one dependent) keeps each model's col_var = its own
  # name (borders separate the models; the outcome/reference/effect go in the title). A single or
  # multi-dependent table shares one outcome col_var per model column + its empirical companions.
  n_dep         <- length(unique(purrr::map_chr(specs, "dependent")))
  is_comparison <- length(specs) > 1L && n_dep == 1L
  numeric_preds <- union_predictors[!purrr::map_lgl(
    union_predictors, ~ is.factor(skeleton_data[[.x]]) || is.character(skeleton_data[[.x]]))]

  # built_per_fit: a list PER FIT of {label, col} lists (a multinomial / MNL-vs-rest / AME-per-category
  # fit contributes SEVERAL columns). Kept un-flattened so reg_gof_tibble() can key the model-summary
  # footer to each fit's FIRST output column (Phase 12f).
  if (effect == "ame") {
    prob_scale   <- family %in% c("binomial", "multinomial", "ordinal")
    per_category <- family %in% c("multinomial", "ordinal")
    shape        <- if (prob_scale) "prob" else "raw"
    built_per_fit <- purrr::map2(fits, specs, function(f, sp) {
      marg  <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                            at = at, want_pred = prob_scale,
                            want_unadj = predicted_unadjusted && family == "binomial")
      var_y <- if (!prob_scale) suppressWarnings(stats::var(as.numeric(f$data[[sp$dependent]])))
               else NA_real_
      if (per_category) {                            # one AME column per OUTCOME category (all levels)
        groups <- levels(as.factor(f$data[[sp$dependent]]))
        purrr::map(groups, function(g) {
          jc  <- if (cleannames) stringr::str_remove_all(g, cleannames_condition()) else g
          # Phase 14s (G) + 14w (item 3): the per-category AME columns of one model share `sp$label`
          # ("<dep>: AME (adjusted %)") as col_var (no inter-category border, one span names the effect
          # once); the visible NAME is just the category (the repeated ": AME" is stripped).
          lab <- paste0(if (prefix_dep) paste0(sp$dependent, " - ") else "", jc)
          list(label = lab, emp_key = g,   # emp_key: raw category, for the empirical tooltip (Phase 14v)
               col   = reg_marginal_column(skeleton, marg, sp$predictors, numeric_preds, shape,
                                           var_y, f$nobs, g, color, color_signif, sp$label))
        })
      } else {
        # Phase 14r (E): the model OR (exp of the fit's coefficient, aligned to the skeleton by term)
        # carried in the AME column's `or` field for the tooltip. Binomial single-outcome only -- for
        # gaussian/poisson the coefficient is not an OR. NA on reference / out-of-model rows (term NA).
        or_tip <- if (family == "binomial") {
          td <- broom::tidy(f$fit); td$term <- stringr::str_remove_all(td$term, "`")
          exp(td$estimate[match(skeleton$term, td$term)])
        } else NULL
        # Phase 14w (item 3): the single AME column shares the outcome col_var with its empirical
        # companions; its NAME carries the effect ("Model AME (adjusted %)").
        cv <- if (is_comparison) sp$label
              else reg_shared_col_var(family, sp$dependent, f$positive_level, cleannames)
        entries <- list(list(
          label = reg_model_col_name(eff_word, sp$dependent, is_comparison, sp$label, n_dep),
          col   = reg_marginal_column(skeleton, marg, sp$predictors, numeric_preds, shape,
                                      var_y, f$nobs, NA_character_, color, color_signif,
                                      cv, or_tip = or_tip)))
        # Change D: the unadjusted control column is appended AFTER the model column (so fit_first_idx --
        # and thus the GOF footer -- still keys on the model column). Shares the outcome col_var.
        if (predicted_unadjusted && family == "binomial") {
          entries <- c(entries, list(list(
            label = paste0("Model % (unadj.)",
                           if (n_dep > 1L) paste0(" (", sp$dependent, ")") else ""),
            col   = reg_unadj_column(skeleton, marg, sp$predictors, cv))))
        }
        entries
      }
    })
  } else if (mnl_vsrest) {
    # MNL "j vs rest" OR at the reference profile (D3-flavour-2): exp of the profile log-odds-ratio of
    # "category j vs the rest" for each predictor level; one OR column per outcome category.
    built_per_fit <- purrr::map2(fits, specs, function(f, sp) {
      marg   <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                             at = "reference", comparison = "lnor", want_pred = FALSE)
      groups <- levels(as.factor(f$data[[sp$dependent]]))
      purrr::map(groups, function(g) {
        jc  <- if (cleannames) stringr::str_remove_all(g, cleannames_condition()) else g
        # Phase 14s (G) + 14w (item 3): shared col_var (`sp$label`) across the "vs rest" category columns
        # of one model; the repeated ": OR" is stripped from the visible NAME (the span carries it).
        lab <- paste0(if (prefix_dep) paste0(sp$dependent, " - ") else "", jc, " vs rest")
        list(label = lab,
             col   = reg_marginal_column(skeleton, marg, sp$predictors, numeric_preds, "or",
                                         NA_real_, f$nobs, g, color, color_signif, sp$label))
      })
    })
  } else {
    built_per_fit <- purrr::map2(fits, specs, function(f, sp) {
      if (multi_col) {
        cols <- reg_columns_multinom(skeleton, f, sp, effect_shape, color, color_signif,
                                     eff_word, cleannames, prefix_dep)
        # Phase 12h: estimate_display="ci" adds the visible interval to each category's OR column
        # (the prob/ame folds are degraded to "ci" for MNL in tab_reg()).
        if (estimate_display != "value") {
          cols <- purrr::map(cols, function(lc) { lc$col <- set_display(lc$col, "est_ci"); lc })
        }
        cols
      } else {
        # a compound formula is one model: every skeleton row belongs to it (else compound rows go NA)
        model_predictors <- if (isTRUE(sp$compound)) unique(skeleton$var) else sp$predictors
        # Phase 14w (item 3): outcome col_var + "Model <effect>" name (comparison keeps the model name).
        cv  <- if (is_comparison) sp$label
               else reg_shared_col_var(family, sp$dependent, f$positive_level, cleannames)
        col <- reg_column(skeleton, f, model_predictors, cv, effect_shape, color, color_signif)
        col <- reg_apply_estimate_display(col, estimate_display, skeleton, f, sp, family,
                                          design_spec, conf_level, numeric_preds, model_predictors)
        list(list(label = reg_model_col_name(eff_word, sp$dependent, is_comparison, sp$label, n_dep),
                  col = col))
      }
    })
  }
  built  <- purrr::flatten(built_per_fit)
  labels <- make.unique(purrr::map_chr(built, "label"))

  # Phase 12f: the model-summary footer -- key each fit's GOF to its FIRST output column (make.unique'd).
  fit_ncol      <- purrr::map_int(built_per_fit, length)
  fit_first_idx <- cumsum(c(1L, utils::head(fit_ncol, -1L)))
  fit_first_col <- labels[fit_first_idx]
  grouped_by_fit <- purrr::map_lgl(specs, ~ family == "binomial" && !is.null(.$trials) &&
                                     !isTRUE(.$compound))
  nobs_by_fit    <- purrr::map_dbl(fits, "nobs")
  reg_gof <- reg_gof_tibble(fits, fit_first_col, family, weighted = weighted,
                            grouped_by_fit = grouped_by_fit, stats = stats,
                            nobs_by_fit = nobs_by_fit)
  reg_gof <- reg_compare_rows(reg_gof, fits, specs, family, weighted = weighted,
                              fit_first_col = fit_first_col, compare = compare, baseline = baseline,
                              conf_level = conf_level)

  disp_levels <- skeleton$level
  if (cleannames) {
    disp_levels <- stringr::str_remove_all(disp_levels, cleannames_condition())
  }
  # multiplier (Phase 12g): relabel the display level of each scaled numeric predictor so the row
  # reads "<var> | per <k>" (the effect is now per k units). Numeric predictors have level == var.
  if (!is.null(multiplier)) {
    for (v in names(multiplier)) {
      hit <- skeleton$var == v & skeleton$level == v
      if (any(hit)) disp_levels[hit] <- paste0("per ", multiplier[[v]])
    }
  }

  tab <- tibble::tibble(
    var    = forcats::fct_inorder(skeleton$var),
    levels = forcats::fct_inorder(disp_levels)
  )
  # empirical (Phase 12g / 14v): the descriptive crude (unadjusted, single-predictor) companion of the
  # model effect -- a base descriptive column + a crude-effect column mirroring the model's measure &
  # colour scale (reg_empirical_columns). Built PER FIT (per dependent), so it works with a vector of
  # dependents. Column families only (binomial / gaussian / poisson); multinomial is tooltip-only
  # (empirical_tips, below), ordinal is unsupported (dropped upstream). A grouped-binomial fit (trials)
  # has no `positive_level` -> no crude 2x2 -> skipped (as before). Aligned to the shared skeleton.
  emp_by_fit <- vector("list", length(specs))
  if (isTRUE(empirical) && family %in% c("binomial", "gaussian", "poisson")) {
    fac_preds_e <- union_predictors[!purrr::map_lgl(
      union_predictors, ~ is.numeric(skeleton_data[[.x]]))]
    if (length(fac_preds_e) > 0L) {
      for (i in seq_along(specs)) {
        dep_i   <- specs[[i]]$dependent
        pos_i   <- if (family == "binomial") fits[[i]]$positive_level else NULL
        if (family == "binomial" && is.null(pos_i)) next   # grouped-binomial / compound: no crude 2x2
        # Change B (decisions doc S50): every crude companion is computed on the SAME complete-case
        # population as the model fit -- mirror reg_fit()'s `mdata` (drop rows missing the dependent, ANY
        # model predictor, or a design var). `union_predictors` == the model's predictors when not
        # comparing; in comparison mode it is the shared population where all compared models overlap
        # (na = "drop_all" makes the model columns share it too). Recomputed from `data` (NOT fits[[i]]$data,
        # which is NULL on the reref/digest path). On this listwise-complete frame reg_empirical()'s
        # per-predictor NA filter is a no-op, so the crude reference level / n exactly match the model.
        emp_vars <- intersect(unique(c(dep_i, union_predictors, reg_design_vars(design_spec))), names(data))
        mdata_i  <- tidyr::drop_na(data, tidyselect::all_of(emp_vars))
        var_y_i <- if (family == "gaussian")
          suppressWarnings(stats::var(as.numeric(mdata_i[[dep_i]]), na.rm = TRUE)) else NA_real_
        emp_i   <- reg_empirical(mdata_i, fac_preds_e, dep_i, family, pos_i, design_spec$wt)
        cols_i  <- reg_empirical_columns(skeleton, emp_i, fac_preds_e, family, effect, var_y_i,
                                         conf_level = conf_level, color_signif = color_signif)
        # Phase 14w (item 3): the crude companions share the model column's outcome col_var (one span,
        # no border). NOT in comparison mode (the crude block stays a distinct col_var beside the models).
        if (!is_comparison) {
          scv    <- reg_shared_col_var(family, dep_i, pos_i, cleannames)
          cols_i <- purrr::map(cols_i, ~ set_col_var(.x, scv))
        }
        emp_by_fit[[i]] <- cols_i
      }
    }
  }
  # one crude companion before all model columns when there is a single dependent (byte-identical
  # layout, incl. a model-comparison list -- all its models share the dependent); per-fit before each
  # fit's first model column when several dependents (names suffixed so they do not collide).
  add_emp_cols <- function(tab, cols, suffix) {
    for (nm in names(cols)) {
      out_nm <- if (nzchar(suffix)) paste0(nm, " (", suffix, ")") else nm
      tab[[out_nm]] <- cols[[nm]]
    }
    tab
  }
  if (n_dep <= 1L) {
    if (!is.null(emp_by_fit[[1]])) tab <- add_emp_cols(tab, emp_by_fit[[1]], "")
    for (i in seq_along(built)) tab[[labels[i]]] <- built[[i]]$col
  } else {
    for (i in seq_along(built)) {
      fi <- match(i, fit_first_idx)                        # non-NA at a fit's first column
      if (!is.na(fi) && !is.null(emp_by_fit[[fi]]))
        tab <- add_emp_cols(tab, emp_by_fit[[fi]], specs[[fi]]$dependent)
      tab[[labels[i]]] <- built[[i]]$col
    }
  }

  # multinomial empirical (Phase 14v): TOOLTIP-only (one column per category would explode the layout).
  # The crude % + diff per (category column, predictor level) travel in the `empirical_tips` table
  # attribute (carried through dplyr like `test`); the render appends an "crude:" fragment. Keyed by the
  # FINAL make.unique'd column label (each category column carries its raw category in `emp_key`).
  empirical_tips <- NULL
  if (isTRUE(empirical) && family == "multinomial") {
    fac_preds_t <- union_predictors[!purrr::map_lgl(
      union_predictors, ~ is.numeric(skeleton_data[[.x]]))]
    has_cat <- any(!purrr::map_lgl(built, ~ is.null(.$emp_key)))
    if (length(fac_preds_t) > 0L && has_cat) {
      # Change B: multinomial crude tooltips on the model's complete-case frame (union-predictor frame).
      tip_vars <- intersect(unique(c(specs[[1]]$dependent, union_predictors,
                                     reg_design_vars(design_spec))), names(data))
      mdata_t  <- tidyr::drop_na(data, tidyselect::all_of(tip_vars))
      tipsd <- reg_empirical_tips(mdata_t, fac_preds_t, specs[[1]]$dependent, design_spec$wt,
                                  conf_level = conf_level)
      tk    <- paste(tipsd$var, tipsd$level, tipsd$category, sep = "\r")
      is_fac_t <- skeleton$var %in% fac_preds_t
      tip_rows <- purrr::compact(purrr::imap(built, function(b, i) {
        if (is.null(b$emp_key)) return(NULL)
        mi2  <- match(paste(skeleton$var, skeleton$level, b$emp_key, sep = "\r"), tk)
        keep <- is_fac_t & !is.na(mi2) & !is.na(tipsd$prop[mi2])
        if (!any(keep)) return(NULL)
        k  <- mi2[keep]
        pr <- tipsd$prop[k]; df <- tipsd$diff[k]
        # 14v-ii: the crude % carries its Wilson CI; a non-reference level also shows its crude
        # difference from the reference and that difference's Newcombe CI (percentage points).
        tibble::tibble(
          col   = labels[i],
          var   = as.character(skeleton$var[keep]),
          level = disp_levels[keep],
          tip   = ifelse(skeleton$is_ref[keep],
                         sprintf("crude: %.0f%% [%.0f; %.0f]",
                                 pr * 100, tipsd$prop_inf[k] * 100, tipsd$prop_sup[k] * 100),
                         sprintf("crude: %.0f%% (%+.0f pts [%+.0f; %+.0f])",
                                 pr * 100, df * 100, tipsd$diff_inf[k] * 100, tipsd$diff_sup[k] * 100)))
      }))
      if (length(tip_rows)) empirical_tips <- purrr::list_rbind(tip_rows)
    }
  }

  # Change D (decisions doc S50): the unadjusted-% control ALSO rides the html tooltip of the adjusted AME
  # cell -- reusing the `empirical_tips` pipeline (no new attribute / fmt field). Read straight from the
  # built unadjusted column, keyed to each fit's AME column (fit_first_idx points at the model column, the
  # unadjusted column is the entry right after it).
  if (predicted_unadjusted && family == "binomial" && effect == "ame") {
    utip <- purrr::compact(purrr::map(seq_along(fit_first_idx), function(fi) {
      ame_lab   <- labels[fit_first_idx[fi]]
      unadj_lab <- labels[fit_first_idx[fi] + 1L]
      if (is.na(unadj_lab) || is.null(tab[[unadj_lab]])) return(NULL)
      upct <- as.numeric(vctrs::field(tab[[unadj_lab]], "pct"))
      keep <- !is.na(upct)
      if (!any(keep)) return(NULL)
      tibble::tibble(col = ame_lab, var = as.character(skeleton$var[keep]),
                     level = disp_levels[keep],
                     tip = sprintf("model unadjusted: %.0f%%", upct[keep] * 100))
    }))
    if (length(utip)) {
      ut <- purrr::list_rbind(utip)
      empirical_tips <- if (is.null(empirical_tips)) ut else vctrs::vec_rbind(empirical_tips, ut)
    }
  }

  # Phase 12f: the GOF footer travels in the whole-table `test` attribute (disjoint discriminators, so
  # the crosstab renderers ignore it); it is materialised as a console block / export rows at display,
  # never baked into the fmt columns (the coefficient skeleton stays intact for downstream reads).
  tab |>
    new_tab(subtext = subtext, test = reg_gof, empirical_tips = empirical_tips,
            # 14v-ii: the numeric/ratio methods the empirical columns use (Student mean-diff = OLS,
            # quasi-Poisson rate-ratio = the phi-scaled model), so the legend can name them (14w).
            ci_settings = list(conf_level = conf_level, method_cell = NA_character_,
                               method_diff = method, method_ratio = "katz",
                               method_mean_diff = "student", method_mean_ratio = "quasipoisson")) |>
    dplyr::group_by(var)
}


# === Public API =====================================================================

#' Regression table (effect measures) as a tabxplor table
#'
#' Fits one regression model per column and returns a `tabxplor` table of the per-family effect
#' measure -- linear **beta** (gaussian), **odds ratios** (binomial / logistic), **incidence-rate
#' ratios** (poisson), **multinomial odds ratios** (one column per outcome category vs the reference,
#' nominal 3+ level), **cumulative odds ratios** (ordinal / proportional-odds) -- one row per predictor
#' level (the reference level shown as the neutral value `0` or `1`), grouped by predictor. Each cell
#' stores the estimate, its confidence interval and p-value, so the table prints with significance
#' stars, greys out non-significant effects, and exports (kable / Markdown / Excel) like any `tabxplor`
#' crosstab.
#'
#' @details
#' New to regressions with tabxplor? A first model needs only three arguments: `data`, `dependent`
#' (the outcome) and `predictors`. tabxplor picks the right model from the outcome's type --- a
#' two-level factor gives logistic **odds ratios**, a numeric gives linear **betas**, a count gives
#' Poisson **rate ratios**, and a 3+ level factor gives multinomial or ordinal odds ratios --- so you
#' rarely set `family` by hand. Add `empirical = TRUE` to show the crude (unadjusted) effect beside the
#' model's adjusted one. See `vignette("tabxplor-reg")` for a guided tour.
#'
#' The arguments fall into groups:
#' \itemize{
#'   \item **The model**: `data`, `dependent`, `predictors` (a character vector = one model; a named
#'     list = several models to compare), `family` (usually detected), `wt` (survey weights).
#'   \item **What each cell shows**: `exponentiate`, `effect` (`"coefficient"` or average marginal
#'     effect `"ame"`), `estimate_display`, `empirical` (crude vs adjusted effect).
#'   \item **Colors & significance**: `color`, `color_signif`, `stars`, `conf_level` --- as in [tab()].
#'   \item **Comparisons & structure**: `reference` (baseline levels), `compare` / `baseline` (model
#'     comparison test), `split_var` (one table per group), `multiplier` (effect per *k* units).
#'   \item **Survey design**: `wt`, `ids`, `strata`, `fpc`, `nest`, or pass a prebuilt design as `data`.
#'   \item **Diagnostics**: `stats` (footer statistics), and the plots [or_plot()] / [lm_plots()].
#' }
#'
#' `predictors` selects the mode: a **character vector** fits one model, and `dependent` may itself
#' be a vector -> one column per dependent; a **named list** of predictor sets fits one model each ->
#' one column per model (predictors absent from a model are left blank), for comparing specifications.
#'
#' `effect = "ame"` switches from the native coefficient to **average marginal effects** with the
#' adjusted **predicted probability** shown in parentheses (e.g. `-8%*** (16%)`) -- a probability-scale,
#' cross-model-comparable interpretation (Mood 2010), computed with the `marginaleffects` package.
#' `at = "reference"` instead evaluates at a **reference profile** (other predictors held at their
#' reference level / mean): the marginal effect *at reference*, or -- for a multinomial
#' `effect = "coefficient"` -- the odds ratio of each outcome category *versus the rest* at that profile.
#'
#' Unweighted models use [stats::lm()] / [stats::glm()]; a `wt` weight column switches to a survey
#' design ([survey::svyglm()]), which gives correct design-based standard errors rather than the
#' frequency-inflated ones of `glm(weights=)`. `broom` (always) and `survey` (only with `wt`) are
#' optional dependencies. `tab_logit()` / `multi_logit()` are convenience wrappers for the binomial
#' family.
#'
#' A **nominal** outcome with 3+ unordered levels is fit as one multinomial logit ([nnet::multinom()]),
#' giving **one odds-ratio column per non-reference outcome category** ("`<category>` vs `<reference>`:
#' OR"). An **ordered** outcome with 3+ levels is fit as a proportional-odds cumulative logit
#' ([MASS::polr()]), giving one cumulative-odds-ratio column; the parallel-lines assumption is tested
#' with the Brant test (install the `brant` package) and a warning is issued if it is violated.
#' (Weighted 3+ level models are planned for a later release.)
#'
#' A **summed-score** outcome (a count of "yes" answers out of a fixed number of items) is fit as a
#' grouped binomial when you pass `trials` (the number of items). Power users can pass a **model
#' formula** as `dependent` -- `tab_reg(data, y ~ x1 + poly(x2, 2) + x1:x3)` -- driving the model
#' directly; simple `y ~ a + b` formulas behave exactly like `dependent = "y"`, `predictors = c("a",
#' "b")`, while interactions / `poly()` / `I()` terms render as best-effort term rows.
#'
#' @param data A data frame, **or a prebuilt survey design** ([survey::svydesign()] /
#'   [survey::svrepdesign()]). When a design is passed, its weights (and clustering / stratification /
#'   calibration) drive the estimation and `wt` / `ids` / `strata` / `fpc` are ignored.
#' @param dependent Character outcome variable name(s), **or a model formula** (the escape hatch).
#'   With a `predictors` character vector, several names give one effect column per outcome; with a
#'   `predictors` list, a single name is required. A formula supplies its own model (leave
#'   `predictors` unset).
#' @param predictors Either a character vector of predictor names (one model), or a **named list**
#'   of character vectors (one model per element, its name labelling the column). Leave `NULL` when
#'   `dependent` is a formula.
#' @param family The model family. `"auto"` (default) detects a binary (-> `"binomial"`), an ordered
#'   3+ level (-> `"ordinal"`), a nominal 3+ level (-> `"multinomial"`), or a continuous
#'   (-> `"gaussian"`) outcome and emits a message; an integer count stays ambiguous and must be named.
#'   Set it explicitly with `"gaussian"` (linear), `"binomial"` (logistic), `"poisson"` /
#'   `"quasipoisson"` (counts), `"multinomial"` (nominal 3+ level), `"ordinal"` (ordered 3+ level).
#' @param wt Optional. Name of a weight column (character). Switches to design-based survey estimation
#'   ([survey::svyglm()]): the sandwich standard errors are scale-invariant, so raw population weights
#'   are handled correctly (no normalisation) and the point estimates match the weighted crosstabs.
#' @param ids,strata,fpc Optional survey-design specification for the `wt` path (each a column name /
#'   character vector, or a formula such as `~psu` / `~region`). `ids` gives the cluster identifier(s)
#'   from largest to smallest stage (default no clustering); `strata` the stratifying variable(s);
#'   `fpc` the finite-population correction. Give correct clustering/stratification for honest
#'   design-based variances (a flat `ids = ~1` can understate them). Ignored when `data` is a design.
#' @param nest Logical. Passed to [survey::svydesign()]: set `TRUE` when cluster ids are reused across
#'   strata. Default `FALSE`.
#' @param exponentiate Whether to exponentiate coefficients into ratios. `"nongaussian"` (default)
#'   exponentiates every family except gaussian (odds ratios / incidence-rate ratios, leaving linear
#'   betas raw); `TRUE` / `FALSE` force it on / off for all columns. Ignored when `effect = "ame"`
#'   (marginal effects are always on the response scale).
#' @param effect The interpretation scale, orthogonal to `family`. `"coefficient"` (default) shows the
#'   native per-family effect (beta / OR / IRR / cumulative-OR). `"ame"` shows **average marginal
#'   effects** with the **adjusted predicted probability** in parentheses (e.g. `-8%*** (16%)`): a
#'   probability-scale, cross-model-comparable summary (Mood 2010) for logistic / multinomial / ordinal
#'   outcomes (percentage points), the expected-count change for poisson, and the coefficient itself for
#'   gaussian. The parenthetical is a *marginal-standardized* prediction (`avg_predictions(variables=)`:
#'   the predictor set to each level for the whole sample, other covariates kept as observed, then
#'   averaged), so it is genuinely covariate-adjusted and coheres with the effect --- adjusted-%(reference)
#'   + AME(level) equals adjusted-%(level). Read it as a standardized comparison ("holding the measured
#'   covariates' distribution fixed"), not a manipulation. Requires the `marginaleffects` package. A
#'   multinomial / ordinal outcome gets one AME column per outcome category.
#' @param at Where the profile-conditional quantities are evaluated (needs `marginaleffects`).
#'   `"average"` (default) is the sample average (the AME / adjusted prediction over the data).
#'   `"reference"` evaluates at the **reference profile** --- every other predictor held at its
#'   reference (factor first level, numeric mean): for `effect = "ame"` this gives the marginal effect
#'   *at reference* (MER) with the adjusted prediction there; for a **multinomial** `effect =
#'   "coefficient"` it gives the odds ratio of each outcome category *versus the rest* at that profile
#'   (one column per category). It has no effect on ordinary coefficients (they are profile-independent).
#'   Note the reference profile can be an unusual baseline (e.g. a factor's first level = `"No answer"`).
#' @param trials Grouped-binomial (summed-score) outcomes only. The number of items behind the score,
#'   fitting `cbind(score, trials - score)` as a binomial. `NULL` (default) fits an ordinary binary
#'   logit; a single integer (or a vector named by dependent) sets the item count; `TRUE` uses each
#'   dependent's observed maximum score. Requires `family = "binomial"`.
#' @param conf_level Confidence level for the intervals. Default `0.95`.
#' @param method How the interval and p-value are computed. `"wald"` (default) uses the Wald interval
#'   and the Wald z / t test: fast, matches standard software output, and the only option for weighted
#'   models. `"profile"` uses the profile-likelihood interval ([stats::confint()], needs `MASS`) and
#'   the likelihood-ratio test: more accurate near separation, unweighted binomial/poisson models only
#'   (else it falls back to Wald with a message; gaussian always uses the exact-t interval).
#' @param reference Optional named vector `c(var = "baseline level")` choosing the treatment-contrast
#'   reference level of one or more factor predictors (the effect of every other level is measured
#'   against it). For a **multinomial** outcome, keying the vector by the outcome name (e.g.
#'   `c(partyid = "Independent")`) also sets the baseline outcome category all the OR columns are
#'   compared against. This is how factor contrasts are set; other contrast codings can be applied by
#'   passing a formula in `dependent` with the terms already coded.
#' @param inverse_two_level_factors Logical, binomial only. If `TRUE` (default), models the FIRST
#'   level of a 2-level factor dependent (e.g. `"1-Married"` before `"2-Not married"`).
#' @param split_var Optional. Name of a grouping variable (character): the regression analogue of
#'   [tab()]'s `tab_vars`. The same model(s) are fitted **within each level** of this variable and the
#'   per-group tables are stacked into one grouped table (grouped by `split_var`), sharing the
#'   variable/level stub. Use [tab_spread()] on `split_var` to pivot the groups into side-by-side
#'   columns for an easy across-group comparison. A level absent from a group shows empty cells.
#' @param multiplier Optional named numeric vector `c(var = k)` rescaling a **continuous**
#'   predictor's effect to a k-unit change (e.g. `c(age = 10)` shows the odds ratio / beta per decade
#'   of age = OR^10 / beta*10). The confidence interval scales with it; the p-value is unchanged. Names
#'   must be numeric predictors; not available for multinomial / ordinal outcomes.
#' @param empirical Logical. If `TRUE`, adds the descriptive **crude** (unadjusted, single-predictor)
#'   companion of the model effect for each factor-predictor level -- the unadjusted bivariate
#'   association, which IS the modelised quantity when there is a single predictor (the standard "crude
#'   vs adjusted" comparison; a large gap signals confounding). Per family: **binomial** adds `Emp. %`
#'   + `Emp. OR` (coefficient) or `Emp. %` + `Emp. diff` (AME); **gaussian** adds `Emp. mean` +
#'   `Emp. diff`; **poisson** adds `Emp. rate` + `Emp. IRR`; **multinomial** shows the crude % +
#'   difference per category in the HTML tooltip (columns would explode). By design every crude quantity
#'   is computed on **exactly the same complete-case population as the model** (listwise-complete on the
#'   dependent, all predictors and any design variable), so crude and adjusted are directly comparable
#'   and not confounded by differing missingness (reproduce it with [dplyr::filter()] + [tab()] on the
#'   same rows). Also works with a vector of dependents. Ordinal has no clean crude analogue and is
#'   ignored (with a message). Default `FALSE`.
#' @param predicted_unadjusted Logical, binomial `effect = "ame"` only. If `TRUE`, adds a
#'   `Model % (unadj.)` control column (and an HTML tooltip on the adjusted-% cell) showing the model's
#'   *unadjusted* predicted probability (`avg_predictions(by=)`, the observed-group average). This equals
#'   the same-frame `Emp. %` exactly (a logistic-model identity), so it is only a cross-check that the
#'   crude companion is computed on the model's population; the adjusted `(...)` prediction in the AME
#'   cell is the substantive quantity. Ignored (with a message) for any other family/effect. Default
#'   `FALSE`.
#' @param empirical_OR `r lifecycle::badge("deprecated")` Renamed `empirical` (now cross-family, not
#'   OR-only). Passing it now errors; use `empirical`.
#' @param stats The goodness-of-fit statistics shown in the model-summary **footer** (one block per
#'   model). `NULL` (default) uses the per-family set: linear models show N, R square, adjusted R
#'   square, the overall F-test and the residual SD; other models show N, the likelihood-ratio test
#'   versus the null model, McFadden's pseudo-R square, AIC and BIC (poisson / grouped-binomial models
#'   also show the Pearson dispersion). Pass a character vector to pick and order the statistics
#'   (`"n"`, `"lr_null"`, `"mcfadden_r2"`, `"aic"`, `"bic"`, `"dispersion"`, `"r2"`, `"r2_adj"`,
#'   `"f_model"`, `"sigma"`), or `FALSE` / `"none"` to hide the footer. Weighted models show a reduced,
#'   survey-appropriate set (design-based Wald test, Nagelkerke pseudo-R square, AIC).
#' @param compare Add a **model-comparison** footer row (only with several models / dependents).
#'   `"none"` (default) adds nothing; `"baseline"` tests each model against the `baseline` column;
#'   `"sequential"` tests each model against the previous one. Uses a likelihood-ratio test (F for
#'   linear / quasi models, a design-based Wald test for weighted / survey models); when the models are
#'   not nested or fit on different numbers of observations it falls back to the AIC difference with a
#'   message.
#' @param baseline For `compare = "baseline"`: which column is the reference model (its label, or a
#'   position). Defaults to the first model.
#' @param estimate_display What each effect cell shows beside the estimate. `"value"` (default) the plain
#'   estimate (e.g. `2.34`); `"ci"` adds a visible confidence-interval bracket (`2.34 [1.20; 4.50]`, any
#'   family); `"prob"` folds the model-adjusted predicted probability into the odds-ratio cell
#'   (`2.34 (16%)`); `"ame"` folds the average marginal effect (`2.34 (+8%)`). `"prob"`/`"ame"` need the
#'   `marginaleffects` package and apply to binomial (logistic) coefficient models only (they degrade to
#'   `"ci"` otherwise, with a message). Note `estimate_display = "ame"` *adds* an AME beside the odds
#'   ratio, whereas `effect = "ame"` makes the whole column an AME (probability scale); the two are
#'   different and, when both are set, `effect = "ame"` wins and `estimate_display` is reset to `"value"`.
#' @param color,color_signif How the effect measure is coloured (`NULL` uses the per-family default:
#'   `"OR"` magnitude for ratios, standardized `"diff"` for betas; significance policy
#'   `"grey_non_signif"`). See [tab()].
#' @param stars Logical (default `TRUE` for regression tables, where significance stars are standard).
#'   When `FALSE`, the per-cell p-value is dropped and no stars are shown (colours still read the CI).
#' @param na How missing values are handled across a model comparison. `"keep"` (default) drops
#'   `NA` rows per model (each model uses its own complete cases). `"drop_all"` fits every model on ONE
#'   shared complete-case population (rows with no `NA` on any predictor / dependent / design variable),
#'   so nested models get equal N and the likelihood-ratio comparison can run; note this **changes all
#'   estimates** (shared population), hence opt-in. Ignored for a prebuilt survey design.
#' @param cleannames Logical. If `TRUE`, strips numeric prefixes from factor levels for display.
#'   Uses `getOption("tabxplor.cleannames")` when `NULL`.
#' @param subtext Optional character. A note shown below the table.
#' @param .fit_cache Internal, for the jamovi live UI (Phase 15b): a mutable cache environment
#'   (see `jmvreg_cache_env()`) that memoizes fitted models so display / colour / reference toggles
#'   avoid a refit. On the single-equation GLM coefficient path a factor-predictor reference change is
#'   reparametrized from the cached fit (no refit). `NULL` (the default) leaves ordinary calls unchanged.
#'
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one effect column per model / dependent.
#'
#' @examples
#' data <- forcats::gss_cat |>
#'   dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
#'                                                 "Married", "Not married")))
#' # Every regression table needs broom; the heavier families need their own engine. Guarding each
#' # keeps the examples runnable where Suggests are absent (CRAN checks such a flavour).
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   # logistic (odds ratios):
#'   print(tab_reg(data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "binomial"))
#'   # linear (betas):
#'   print(tab_reg(data, dependent = "tvhours", predictors = c("race", "age"),
#'                 family = "gaussian"))
#'   # formula escape-hatch (same model, terser):
#'   print(tab_reg(data, married ~ race + rincome, family = "binomial"))
#' }
#'
#' \donttest{
#' # average marginal effects + adjusted predictions (needs the marginaleffects package):
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("marginaleffects", quietly = TRUE)) {
#'   print(tab_reg(data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "ame"))
#'   # marginal effects at the reference profile (others at their reference level / mean):
#'   print(tab_reg(data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "ame", at = "reference"))
#' }
#' # multinomial (nominal 3+ level): one OR column per outcome category vs the reference
#' if (requireNamespace("broom", quietly = TRUE) && requireNamespace("nnet", quietly = TRUE)) {
#'   print(tab_reg(forcats::gss_cat, dependent = "partyid", predictors = c("race", "age"),
#'                 family = "multinomial", reference = c(partyid = "Independent")))
#' }
#' # ordinal (proportional-odds): one cumulative-OR column
#' if (requireNamespace("broom", quietly = TRUE) && requireNamespace("MASS", quietly = TRUE)) {
#'   income3 <- forcats::gss_cat |>
#'     dplyr::mutate(income = factor(rincome, ordered = TRUE))
#'   print(tab_reg(income3, dependent = "income", predictors = "race", family = "ordinal"))
#' }
#' }
#'
#' @export
tab_reg <- function(data, dependent, predictors = NULL,
                    family = "auto", wt = NULL, ids = NULL, strata = NULL, fpc = NULL, nest = FALSE,
                    exponentiate = "nongaussian",
                    effect = c("coefficient", "ame"), at = c("average", "reference"),
                    trials = NULL, conf_level = getOption("tabxplor.conf_level", 0.95), method = c("wald", "profile"),
                    reference = NULL, inverse_two_level_factors = TRUE, split_var = NULL,
                    multiplier = NULL, empirical = FALSE, predicted_unadjusted = FALSE,
                    stats = NULL, compare = c("none", "baseline", "sequential"), baseline = NULL,
                    estimate_display = c("value", "ci", "prob", "ame"),
                    color = NULL, color_signif = NULL, stars = TRUE,
                    na = c("keep", "drop_all"),
                    cleannames = NULL, subtext = "",
                    empirical_OR = lifecycle::deprecated(), .fit_cache = NULL) {
  method  <- match.arg(method)
  effect  <- match.arg(effect)
  at      <- match.arg(at)
  compare <- match.arg(compare)
  estimate_display <- match.arg(estimate_display)
  na      <- match.arg(na)
  # Fallback FALSE matches .onLoad's default and tab()'s read sites (the option is always set to FALSE
  # on load, so this only bites if someone unsets it; TRUE here was an inconsistency, not an intent).
  cleannames <- if (is.null(cleannames)) getOption("tabxplor.cleannames", FALSE) else cleannames
  # Phase 14v: `empirical_OR` renamed to `empirical` (now cross-family, not OR-only). Phase 14x hard-
  # deprecates the alias (it never shipped in a CRAN release, so there is no retro-compat debt): calling
  # it now errors with a pointer to `empirical`, rather than silently forwarding.
  if (lifecycle::is_present(empirical_OR)) {
    lifecycle::deprecate_stop("1.4.0", "tab_reg(empirical_OR)", "tab_reg(empirical)")
  }


  # Phase 14u (K): a LIST of models AND SEVERAL dependents -> one model-comparison table per dependent,
  # returned as a `tabxplor_tabs` list (so tab_export("xl") writes one sheet per dependent). Loop the
  # dependents on the outside; each iteration is the ordinary single-dependent comparison (recursion,
  # so every arg / message / family-detect is reused). `trials` is per-dependent (a vector or a named
  # vector), split here. Placed BEFORE the design extraction so a survey design recurses intact.
  if (is.list(predictors) && !inherits(predictors, "formula") && length(dependent) > 1L) {
    if (!is.null(trials) && !isTRUE(trials) && is.null(names(trials)) &&
        length(trials) > 1L && length(trials) != length(dependent)) {
      cli::cli_abort(c("{.arg trials} must be length 1, one per dependent, or a named vector.",
                       "x" = "Got {length(trials)} for {length(dependent)} dependents."))
    }
    tabs <- purrr::map(seq_along(dependent), function(i) {
      d   <- dependent[[i]]
      tri <- if (is.null(trials) || isTRUE(trials)) trials
             else if (!is.null(names(trials)))      unname(trials[d])
             else if (length(trials) == 1L)         as.numeric(trials)
             else                                   trials[[i]]
      tab_reg(data, dependent = d, predictors = predictors, family = family, wt = wt,
              ids = ids, strata = strata, fpc = fpc, nest = nest, exponentiate = exponentiate,
              effect = effect, at = at, trials = tri, conf_level = conf_level, method = method,
              reference = reference, inverse_two_level_factors = inverse_two_level_factors,
              split_var = split_var, multiplier = multiplier, empirical = empirical,
              predicted_unadjusted = predicted_unadjusted,
              stats = stats, compare = compare, baseline = baseline,
              estimate_display = estimate_display, color = color, color_signif = color_signif,
              stars = stars, na = na, cleannames = cleannames, subtext = subtext)
    })
    names(tabs) <- dependent
    return(new_tabxplor_tabs(tabs))
  }

  # Phase 12g: `data` may be a PREBUILT survey design (survey.design / svyrep.design), gtsummary-style.
  # Extract its model frame for family-detect / reference / skeleton; keep the design for the fits.
  design_obj <- NULL
  if (inherits(data, c("survey.design", "survey.design2", "svyrep.design"))) {
    if (!requireNamespace("survey", quietly = TRUE)) {
      cli::cli_abort(c("{.pkg survey} is required to pass a survey design as {.arg data}.",
                       "i" = 'Install it with {.code install.packages("survey")}.'))
    }
    design_obj <- data
    data       <- design_obj$variables
    if (!is.null(wt) || !is.null(ids) || !is.null(strata) || !is.null(fpc)) {
      cli::cli_inform(c("i" = paste0("{.arg data} is already a survey design; {.arg wt} / {.arg ids} / ",
                                     "{.arg strata} / {.arg fpc} are ignored.")))
      wt <- NULL; ids <- NULL; strata <- NULL; fpc <- NULL
    }
  }
  stopifnot(is.data.frame(data))
  weighted <- !is.null(design_obj) || !is.null(wt)

  # formula escape-hatch (D9): a formula in `dependent` supplies the model. A SIMPLE formula (bare
  # response ~ bare main-effect vars) reduces losslessly to the dependent+predictors character path;
  # a COMPOUND one (interactions / poly() / I() / calls) is fit verbatim with a fit-read skeleton.
  formula_mode <- FALSE
  raw_formula  <- NULL
  if (rlang::is_formula(dependent)) {
    if (!is.null(predictors)) {
      cli::cli_abort("Provide either a formula in {.arg dependent} or {.arg predictors}, not both.")
    }
    parsed <- reg_parse_formula(dependent, data)
    if (!parsed$lhs_is_name && identical(family, "auto")) {
      cli::cli_abort(c("Cannot auto-detect {.arg family} from a transformed formula response.",
                       "i" = "Set {.arg family} explicitly when the response is not a bare variable."))
    }
    dependent <- parsed$dependent
    if (parsed$simple) {
      predictors <- parsed$labels                       # main-effect vars, in formula order
    } else {
      formula_mode <- TRUE
      raw_formula  <- parsed$formula
      predictors   <- parsed$predictors                 # RHS bare vars (reference= / drop_na)
    }
  } else if (is.null(predictors)) {
    cli::cli_abort(c("{.arg predictors} is required.",
                     "i" = "Or pass a model formula as {.arg dependent}, e.g. {.code y ~ x1 + x2}."))
  }
  stopifnot(is.character(dependent), length(dependent) >= 1L)

  # predictors dispatch: named list -> model-comparison ; character vector -> one model per dependent
  is_comparison <- is.list(predictors)
  if (is_comparison && length(dependent) != 1L) {
    cli::cli_abort(c("With a list of models in {.arg predictors}, {.arg dependent} must be a single name.",
                     "i" = "A vector of dependents is for the one-model-per-outcome mode."))
  }
  if (!is_comparison && !is.character(predictors)) {
    cli::cli_abort("{.arg predictors} must be a character vector or a named list of character vectors.")
  }

  if (identical(family, "auto")) family <- reg_detect_family(data, dependent[[1]])
  family <- rlang::arg_match(family, c("gaussian", "binomial", "poisson", "quasipoisson",
                                       "multinomial", "ordinal"))

  # Phase 12g: survey-weighted 3+ level outcomes are supported -- ordinal via survey::svyolr, nominal
  # via svyVGAM::svy_vglm (checked in reg_check_deps). The marginaleffects paths (effect="ame", and the
  # multinomial "j vs rest" OR at the reference profile) have no method for svyolr / svy_vglm -> error.
  if (weighted && family %in% c("multinomial", "ordinal") &&
      (effect == "ame" || (family == "multinomial" && at == "reference"))) {
    cli::cli_abort(c(
      paste0("Marginal-effects output ({.code effect = \"ame\"}", if (family == "multinomial")
             ' or {.code at = "reference"}' else "", ") is not available for survey-weighted ",
             "{.val {family}} models."),
      "i" = "Use the default {.code effect = \"coefficient\"} (at = \"average\"), or drop the weights."
    ))
  }

  # `at = "reference"` (the profile axis) only bites on effect="ame" and on a multinomial coefficient
  # (the "j vs rest" OR at the profile); ordinary coefficients are profile-independent -> message + drop.
  mnl_vsrest <- effect == "coefficient" && at == "reference" && family == "multinomial"
  if (at == "reference" && effect == "coefficient" && family != "multinomial") {
    cli::cli_inform(c("i" = paste0(
      "{.code at = \"reference\"} has no effect on {.val {family}} coefficients (they are ",
      "profile-independent); returning the usual coefficients. Use {.code effect = \"ame\"} for ",
      "profile-specific marginal effects.")))
    at <- "average"
  }

  do_exp       <- isTRUE(exponentiate) ||
    (identical(exponentiate, "nongaussian") && family != "gaussian")
  effect_shape <- if (do_exp) "ratio" else "additive"
  eff_word     <- reg_effect_word(family, do_exp, effect, at)
  # Phase 14v: with an empirical companion, a prob-scale AME/MER cell folds in the model-adjusted
  # predicted % as "{diff} ({pct})"; name it in the header ("... AME (adjusted %)") so the parenthetical is
  # unambiguous next to the crude "Emp. %". The parenthetical is the marginal-STANDARDISED predicted
  # probability (decisions doc S50, change A/C), hence "adjusted %" not "model %". Gated on `empirical`
  # (the maintainer's disambiguation case), prob-scale families only (gaussian/poisson AME is a bare effect).
  if (effect == "ame" && isTRUE(empirical) && family %in% c("binomial", "multinomial", "ordinal")) {
    eff_word <- paste0(eff_word, " (adjusted %)")
  }

  # Phase 12h: `estimate_display` = the estimate-cell layout. "value" (plain) / "ci" (a visible interval,
  # any family) apply everywhere; the "prob"/"ame" folds (OR + adjusted probability / OR + marginal
  # effect, via reg_marginal) are probability-scale -> binomial coefficient models only. Marginal-effects
  # output (effect="ame" / the MNL "j vs rest" OR at reference) already has its own layout -> ignored.
  if (estimate_display != "value" && (effect == "ame" || mnl_vsrest)) {
    cli::cli_inform(c("i" = "{.arg estimate_display} is ignored with marginal-effects output."))
    estimate_display <- "value"
  }
  if (estimate_display %in% c("prob", "ame") && !(family == "binomial" && !formula_mode)) {
    cli::cli_inform(c(
      "!" = paste0("{.arg estimate_display = \"{estimate_display}\"} needs a binomial coefficient ",
                   "model; showing the confidence interval instead.")))
    estimate_display <- "ci"
  }

  # trials -> grouped binomial (D2): a summed-score outcome fit as cbind(score, trials-score). NULL =
  # off (binary logit). TRUE = observed max per dependent. Numeric / named vector = the item count.
  trials_for <- function(d) NULL
  if (!is.null(trials)) {
    if (family != "binomial") {
      cli::cli_abort("{.arg trials} applies only to the {.val binomial} family (grouped / summed-score).")
    }
    if (formula_mode) {
      cli::cli_warn("{.arg trials} is ignored with a compound formula; write {.code cbind()} in it instead.")
    } else {
      tv <- if (isTRUE(trials))              purrr::map_dbl(dependent, ~ max(data[[.x]], na.rm = TRUE))
            else if (!is.null(names(trials))) unname(trials[dependent])
            else                              rep_len(as.numeric(trials), length(dependent))
      tv <- stats::setNames(as.integer(round(tv)), dependent)
      trials_for <- function(d) tv[[d]]
    }
  }

  # base `%||%` is R >= 4.4 only; the package supports R >= 4.1, so use explicit is.null().
  # effect="ame" always colours the marginal effect as a difference (neutral 0), never as a ratio.
  if (is.null(color))        color        <- if (effect_shape == "ratio" && effect != "ame") "OR" else "diff"
  if (is.null(color_signif)) color_signif <- "grey_non_signif"

  all_predictors <- if (is_comparison) unique(purrr::flatten_chr(predictors)) else predictors

  # Phase 14u (L2): na = "drop_all" fits every model on ONE shared complete-case population -- the union
  # of all predictors + the dependent + design vars -- so genuinely-nested models get EQUAL N and the LR
  # comparison fires (the default per-model NA drop can make N differ -> the AIC fallback). It CHANGES all
  # estimates (shared population), hence opt-in. Not applied to a prebuilt survey design (subsetting is
  # the design's own concern).
  if (identical(na, "drop_all") && !formula_mode) {
    if (!is.null(design_obj)) {
      cli::cli_inform(c("i" = '{.code na = "drop_all"} is ignored for a prebuilt survey design.'))
    } else {
      keep_vars <- intersect(unique(c(dependent, all_predictors, wt, ids, strata, fpc, split_var)),
                             names(data))
      data <- tidyr::drop_na(data, tidyselect::all_of(keep_vars))
    }
  }

  # Phase 15b (jamovi live reref): with a `.fit_cache`, a single-equation GLM coefficient table can be
  # recomputed at any factor-predictor reference from ONE canonical fit (reg_build_digest) -- no refit.
  # On that path the body does NOT relevel; reg_build fits the canonical digest + reparametrizes to
  # `reference`. Everything the reparametrization can't handle (ame / profile / mnl-vs-rest / compound /
  # multinomial / ordinal / split / multiplier / trials / model comparison) keeps the refit path.
  reref <- !is.null(.fit_cache) && effect == "coefficient" && !mnl_vsrest &&
    estimate_display %in% c("value", "ci") && method == "wald" &&
    family %in% c("gaussian", "binomial", "poisson", "quasipoisson") &&
    !formula_mode && is.null(split_var) && is.null(multiplier) && is.null(trials) &&
    compare == "none" && !is_comparison

  if (!is.null(reference) && !reref) {
    # A multinomial's baseline is the OUTCOME factor's first level, so `reference` keyed by the
    # dependent relevels it too (unified "reference level of any variable"). An ordinal outcome must
    # keep its order -> never releveled; predictor contrasts are releveled for every family.
    relevelable <- if (family == "multinomial") c(all_predictors, dependent) else all_predictors
    if (!is.null(design_obj)) {
      design_obj <- reg_relevel_design(design_obj, reference, relevelable)  # relevel inside the design
      data       <- design_obj$variables
    } else {
      data <- reg_apply_references(data, reference, relevelable)
    }
  }

  if (is_comparison) {
    models <- predictors
    if (is.null(names(models)) || any(names(models) == "")) {
      names(models) <- paste0("model", seq_along(models))
    }
    labels <- make.unique(names(models))
    specs  <- purrr::map2(models, labels,
                          ~ list(dependent = dependent, predictors = .x, label = .y,
                                 trials = trials_for(dependent), compound = FALSE, formula = NULL))
    union_predictors <- reg_order_union(models)          # Phase 14u (L1): complete-model order if any
  } else {
    labels <- purrr::map_chr(dependent, function(d) {
      # a summed-score / compound-formula binomial has no single "positive level" -> label by name
      base <- if (family == "binomial" && !formula_mode && is.null(trials_for(d))) {
        pl <- reg_positive_level(data, d, inverse_two_level_factors)
        if (cleannames) pl <- stringr::str_remove_all(pl, cleannames_condition())
        pl
      } else d
      paste0(base, ": ", eff_word)
    })
    labels <- make.unique(labels)
    specs  <- purrr::map2(dependent, labels,
                          ~ list(dependent = .x, predictors = predictors, label = .y,
                                 trials = trials_for(.x), compound = formula_mode,
                                 formula = raw_formula))
    union_predictors <- predictors
  }

  # Phase 14w: the model note is NO LONGER baked into `subtext` here. It is generated fresh from `reg_meta`
  # (reg_model_line) at render time, so it can be ordered BEFORE the colour legend. `subtext` now holds
  # only user-supplied text.

  # split_var (Phase 12g): one grouping column, distinct from the outcome / predictors, that a model is
  # fitted within each level of. Must be a factor / character; reg_build recurses per level and stacks.
  if (!is.null(split_var)) {
    if (!is.character(split_var) || length(split_var) != 1L) {
      cli::cli_abort("{.arg split_var} must be a single column name (character).")
    }
    if (!split_var %in% names(data)) {
      cli::cli_abort("{.arg split_var} {.val {split_var}} is not a column of {.arg data}.")
    }
    if (split_var %in% c(dependent, all_predictors)) {
      cli::cli_abort("{.arg split_var} {.val {split_var}} cannot also be the outcome or a predictor.")
    }
    if (!is.factor(data[[split_var]]) && !is.character(data[[split_var]])) {
      cli::cli_abort("{.arg split_var} {.val {split_var}} must be a factor or character column.")
    }
  }

  # multiplier (Phase 12g): a named numeric vector c(var = k) scaling a CONTINUOUS predictor's effect
  # to per-k units (OR^k / beta*k). Names must be numeric predictors of the glm-family models.
  if (!is.null(multiplier)) {
    if (!is.numeric(multiplier) || is.null(names(multiplier))) {
      cli::cli_abort("{.arg multiplier} must be a named numeric vector, e.g. {.code c(age = 10)}.")
    }
    if (family %in% c("multinomial", "ordinal")) {
      cli::cli_abort("{.arg multiplier} is not supported for {.val {family}} models.")
    }
    num_preds <- all_predictors[!purrr::map_lgl(
      all_predictors, ~ is.factor(data[[.x]]) || is.character(data[[.x]]))]
    bad <- setdiff(names(multiplier), num_preds)
    if (length(bad) > 0L) {
      cli::cli_abort(c("{.arg multiplier} names must be numeric predictors.",
                       "x" = "Not numeric predictor{?s}: {.val {bad}}."))
    }
  }

  # empirical (Phase 12g / 14v): the descriptive crude companion beside the model effect -- the
  # unadjusted bivariate association (which IS the modelised quantity when there is a single predictor).
  # Wired for binomial / gaussian / poisson (explicit columns) and multinomial (tooltip only). A vector
  # of dependents is supported (crude companion per dependent). Ordinal (cumulative OR) has no clean
  # crude analogue -> a message, not an error, and `empirical` is dropped for this call.
  if (isTRUE(empirical) &&
      !family %in% c("binomial", "gaussian", "poisson", "multinomial")) {
    cli::cli_inform(c("i" = paste0(
      "{.arg empirical} (crude descriptive companion) is not available for {.val {family}} ",
      "models; ignored here.")))
    empirical <- FALSE
  }

  # predicted_unadjusted (decisions doc S50, change D): an opt-in control column + html tooltip showing the
  # model's UNADJUSTED predicted % (avg_predictions(by = level) = the observed-group average, which equals
  # the same-frame `Emp. %` exactly by the score-equation identity). Only meaningful for a binomial AME
  # (the adjusted-% fold); ignored otherwise.
  predicted_unadjusted <- isTRUE(predicted_unadjusted)
  if (predicted_unadjusted && !(effect == "ame" && family == "binomial")) {
    cli::cli_inform(c("i" = paste0(
      "{.arg predicted_unadjusted} applies to a binomial {.code effect = \"ame\"} table only; ",
      "ignored here.")))
    predicted_unadjusted <- FALSE
  }

  design_spec <- list(design = design_obj, wt = wt, ids = ids, strata = strata, fpc = fpc, nest = nest)
  reg_check_deps(family, weighted, needs_marginaleffects = effect == "ame" || mnl_vsrest ||
                   estimate_display %in% c("prob", "ame"))
  res <- reg_build(data, specs, union_predictors, family, design_spec, weighted, do_exp, effect_shape,
                   inverse_two_level_factors, conf_level, method, color, color_signif,
                   cleannames, subtext, eff_word, effect, at,
                   stats = stats, compare = compare, baseline = baseline, split_var = split_var,
                   multiplier = multiplier, empirical = empirical,
                   predicted_unadjusted = predicted_unadjusted,
                   estimate_display = estimate_display,
                   .fit_cache = .fit_cache, reference = reference, reref = reref)

  # stars = TRUE (default) for regression tables -- the per-cell pvalue is stored by reg_build so the
  # main display shows significance stars. stars = FALSE strips it (pvalue is stars-only; colours read
  # the CI bounds), so the table renders without stars.
  if (!isTRUE(stars)) {
    for (nm in names(res)[vapply(res, is_fmt, logical(1))]) {
      res[[nm]] <- set_pvalue(res[[nm]], NA_real_)
    }
  }

  # Phase 14w: the table's own model record (drives the reg title / caption, the "Model:" footer line, and
  # the colour legend). Table-level: family/effect are one per table; the per-column effect word is derived
  # from this + the column's ci_type (legend_specs), so no column-level attribute / fmt field is needed.
  positive_levels <-
    if (family == "binomial" && !formula_mode) {
      purrr::map_chr(dependent, function(d) {
        if (!is.null(trials_for(d))) return(NA_character_)
        pl <- reg_positive_level(data, d, inverse_two_level_factors)
        if (isTRUE(cleannames)) pl <- stringr::str_remove_all(pl, cleannames_condition())
        pl
      })
    } else rep(NA_character_, length(dependent))
  # Phase 16d: the weight column NAME (or NA) drives the footer "Weighted by <wt>." line. `wt` is a
  # character column name or a formula (reg_design_formula accepts both); a prebuilt design carries its
  # own weights and cannot be named -> NA.
  wt_disp <- if (is.null(wt) || (length(wt) == 1L && is.na(wt))) NA_character_
             else if (rlang::is_formula(wt)) all.vars(wt)[1]
             else as.character(wt)[1]
  reg_meta <- list(
    family = family, effect = effect, at = at, do_exp = do_exp, eff_word = eff_word,
    dependent = dependent, positive_level = positive_levels, predictors = union_predictors,
    split_var = split_var, comparison = is_comparison, wt = wt_disp,
    model_labels = if (is_comparison) labels else NULL, conf_level = conf_level
  )
  set_reg_meta(res, reg_meta)
}


#' Logistic-regression table (odds ratios)
#'
#' Convenience wrapper of [tab_reg()] for the binomial family: fits one binary logistic regression
#' per `dependent` on a shared set of `predictors` and returns a `tabxplor` table of odds ratios
#' (one column per dependent, the reference level shown as `1`, grouped by predictor). See [tab_reg()]
#' for the engine, weighting and interval details.
#'
#' @inheritParams tab_reg
#' @param dependent Character vector of binary dependent variable name(s). Each must be a 2-level
#'   factor/character or a 0/1 numeric.
#' @param predictors Character vector of predictor variable name(s).
#' @param color_signif How significance drives the colours. `"grey_non_signif"` (default) colours
#'   only odds ratios whose confidence interval excludes 1 and greys the rest.
#'
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one odds-ratio column per `dependent`.
#'
#' @examples
#' data <- forcats::gss_cat |>
#'   dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
#'                                                 "Married", "Not married")))
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   tab_logit(data, dependent = "married", predictors = c("race", "rincome"))
#' }
#'
#' @export
tab_logit <- function(data, dependent, predictors, wt = NULL,
                      ids = NULL, strata = NULL, fpc = NULL, nest = FALSE,
                      inverse_two_level_factors = TRUE, split_var = NULL, multiplier = NULL,
                      empirical = FALSE,
                      conf_level = getOption("tabxplor.conf_level", 0.95),
                      method = c("wald", "profile"),
                      stats = NULL, estimate_display = c("value", "ci", "prob", "ame"),
                      color_signif = c("grey_non_signif", "ignore", "guaranteed_effect"),
                      stars = TRUE, na = c("keep", "drop_all"),
                      cleannames = NULL, subtext = "") {
  method       <- match.arg(method)
  color_signif <- match.arg(color_signif)
  estimate_display <- match.arg(estimate_display)
  na           <- match.arg(na)
  stopifnot(is.character(predictors), length(predictors) >= 1L)
  tab_reg(data, dependent = dependent, predictors = predictors, family = "binomial", wt = wt,
          ids = ids, strata = strata, fpc = fpc, nest = nest, split_var = split_var,
          multiplier = multiplier, empirical = empirical,
          conf_level = conf_level, method = method, stats = stats,
          estimate_display = estimate_display,
          inverse_two_level_factors = inverse_two_level_factors,
          color_signif = color_signif, stars = stars, na = na,
          cleannames = cleannames, subtext = subtext)
}


#' Compare several logistic-regression models (odds ratios side by side)
#'
#' Convenience wrapper of [tab_reg()] for the binomial family in model-comparison mode: fits several
#' models for ONE binary `dependent`, one per named predictor set in `models`, and returns a
#' `tabxplor` table with one odds-ratio column per model (predictors absent from a model left blank).
#'
#' @inheritParams tab_logit
#' @inheritParams tab_reg
#' @param dependent Character. Name of the binary dependent variable. May be a **vector** of several
#'   binary dependents: the model comparison is then run once per dependent and the per-dependent
#'   tables are returned as a list (one sheet each when exported to Excel).
#' @param models A named list of character vectors; each element is one model's predictor set and its
#'   name labels the column. Unnamed elements are labelled `model1`, `model2`, ...
#'
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one odds-ratio column per model; or, for
#'   several `dependent`s, a `tabxplor_tabs` list of such tables (one per dependent).
#'
#' @examples
#' data <- forcats::gss_cat |>
#'   dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
#'                                                 "Married", "Not married")))
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   multi_logit(
#'     data, dependent = "married",
#'     models = list(demographic = c("race", "age"),
#'                   full        = c("race", "age", "rincome"))
#'   )
#' }
#'
#' @export
multi_logit <- function(data, dependent, models, wt = NULL,
                        ids = NULL, strata = NULL, fpc = NULL, nest = FALSE,
                        inverse_two_level_factors = TRUE, split_var = NULL, multiplier = NULL,
                        empirical = FALSE,
                        conf_level = getOption("tabxplor.conf_level", 0.95),
                        method = c("wald", "profile"),
                        stats = NULL, compare = c("none", "baseline", "sequential"), baseline = NULL,
                        estimate_display = c("value", "ci", "prob", "ame"),
                        color_signif = c("grey_non_signif", "ignore", "guaranteed_effect"),
                        stars = TRUE, na = c("keep", "drop_all"),
                        cleannames = NULL, subtext = "") {
  method       <- match.arg(method)
  compare      <- match.arg(compare)
  color_signif <- match.arg(color_signif)
  estimate_display <- match.arg(estimate_display)
  na           <- match.arg(na)
  # Phase 14x: `dependent` may be a VECTOR -> the model comparison runs once per dependent (tab_reg's K
  # mode: a models list + several dependents -> one table each, returned as a tabxplor_tabs list).
  stopifnot(is.character(dependent), length(dependent) >= 1L, is.list(models), length(models) >= 1L)
  tab_reg(data, dependent = dependent, predictors = models, family = "binomial", wt = wt,
          ids = ids, strata = strata, fpc = fpc, nest = nest, split_var = split_var,
          multiplier = multiplier, empirical = empirical,
          conf_level = conf_level, method = method,
          stats = stats, compare = compare, baseline = baseline,
          estimate_display = estimate_display,
          inverse_two_level_factors = inverse_two_level_factors,
          color_signif = color_signif, stars = stars, na = na,
          cleannames = cleannames, subtext = subtext)
}
