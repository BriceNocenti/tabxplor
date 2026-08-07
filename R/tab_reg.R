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
#     `multiplier` scales a continuous predictor's native coef by k before CI/exp (OR^k), p unchanged.
#   - Last Phase z9: `multiplier` is the UNIT such a predictor's effect is reported per, and its DEFAULT
#     is "sd" (per one standard deviation) -- per 1 unit the row sits inside the first colour break and
#     reads as "no effect". A scalar ("sd"/"2sd"/a number) applies to all, a named vector overrides per
#     variable, 1 = per unit. Resolved ONCE in tab_reg() on the PREDICTOR complete-case frame, so one
#     predictor keeps one unit across outcomes, compared models and split groups. Same phase:
#     `empirical = TRUE` fills a continuous predictor's crude EFFECT column from its univariable
#     reg_fit() (base cell stays empty, distribution -> tooltip), and reg_gap_se_columns() tests that
#     gap like any other. reg_is_factor_var() is the ONE predictor-kind predicate.
# See: CLAUDE.md Phase 12c-12g + Last Phase z9 ; dev/tabxplor_2.0.0_decisions.md S37 ;
#      dev/numeric_predictors_crude_counterparts.md.

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

# DESIGN (Last Phase z3): the family PREDICATES. Every "which families behave like X" question is asked
# here ONCE instead of by a hand-written whitelist at each call site (there were 11 bare `== "binomial"`
# tests, 4 probability-scale lists, and the log-scale list written TWICE verbatim in fmt_class.R). The
# internal family key "rr" (modified Poisson on a binary outcome, see the families_vec resolver in
# tab_reg()) joins the binary + log-scale sets here and nowhere else.
# WARNING: `reg_fam_logscale()` is read by fmt_class.R's colour engine AND its legend -- it is the single
# source that replaced their sync-by-comment pair. Keep it set-identical for the pre-existing families.
reg_is_binary_outcome <- function(y) length(unique(stats::na.omit(y))) == 2L
# the binary-outcome machinery: reg_prep_binary / positive_level / the crude 2x2 companion.
reg_fam_binary   <- function(f) f %in% c("binomial", "rr")
# probability-scale outcomes: a marginal effect is a probability (percentage points / a risk ratio).
reg_fam_prob     <- function(f) f %in% c("binomial", "multinomial", "ordinal")
# coefficients that live on a LOG scale, so an un-exponentiated coef colours on the logged OR breaks
# (log_odds_scale) rather than the SD-standardized mean_diff scale. gaussian is the only family out.
reg_fam_logscale <- function(f)
  f %in% c("binomial", "poisson", "quasipoisson", "ordinal", "multinomial", "rr")
# Last Phase z8-B (SS4.2, maintainer ruling Q1(b)): is the DISPLAYED estimand COLLAPSIBLE -- i.e. does a
# zero model-vs-observed gap mean "no confounding"? Everything tabxplor shows is, EXCEPT a CONDITIONAL
# ODDS RATIO: the coefficient of a probability-scale model (the binomial logit; the multinomial /
# ordinal cumulative logits, which have no crude twin anyway). `exponentiate` is irrelevant -- a raw
# logit coefficient is the same estimand, logged. Marginalising (effect = "ame" / "ame_ratio") or
# changing the link (family = "poisson" -> the "rr" modified Poisson) makes it collapsible. Measured
# (dev/model_vs_observed_gap_test.md SS4.1): with the covariate INDEPENDENT of the exposure -- so
# strictly no confounding -- the conditional-OR gap test rejects at 1.000 for n = 32000 while every
# collapsible scale holds its nominal 0.05. This is the FIRST place the collapsibility ladder z3 and z5
# built is USED rather than merely documented.
reg_estimand_collapsible <- function(family, effect)
  !(identical(effect, "coefficient") && reg_fam_prob(family))

# Last Phase z10: `crude_key` -- THE stored fact "which observed counterpart does this model have?".
# It is the REG_EMPIRICAL key, or NA when there is none. Before z10 the same question was asked by
# inference in six places, in three different shapes: a missing REG_EMPIRICAL key, a hand-written
# `quasipoisson -> poisson` alias, two duplicated family whitelists, and -- worst -- `positive_level`
# being NULL, which is a SIDE EFFECT of reg_fit() skipping reg_prep_binary() on the grouped path
# (Phase 17 rule 2: roles are stored, never guessed). Computed ONCE at spec construction, where
# `family`, `trials` and the compound-formula flag are all in scope, and read everywhere else.
#   grouped_binomial : `trials =` -- the outcome is a success COUNT, so the crude 2x2 sums over trials
#                      and the base column is the mean SCORE, not a share of respondents.
#   compound formula : no predictor structure to be crude about -> NA.
reg_crude_key <- function(family, trials = NULL, compound = FALSE) {
  if (isTRUE(compound))                                 return(NA_character_)
  if (identical(family, "quasipoisson"))                return("poisson")
  if (identical(family, "binomial") && !is.null(trials)) return("grouped_binomial")
  if (is.null(REG_EMPIRICAL[[family]]))                 return(NA_character_)
  family
}

# Last Phase z9: is a PREDICTOR a factor (contrasts vs a reference level) or a numeric (one slope per
# unit)? ONE definition, replacing five sites that disagreed -- `is.factor || is.character` at
# reg_fit()'s de-ordering and at tab_reg()'s multiplier check, `!is.numeric` at the empirical-columns
# and empirical-tips gates. They diverged for exactly two column kinds, both measured:
#   * logical  -- glm names its coefficient `<var>TRUE`, so it MUST take the factor arm. Under the old
#                 split reg_skeleton() sent it down the NUMERIC arm (term = `<var>`, no tidy match ->
#                 a silent all-NA row) while the empirical gate simultaneously gave it crude FACTOR
#                 rows keyed "<var>\rTRUE" against a skeleton key of "<var>\r<var>" -- two misses.
#   * Date / POSIXct -- glm names the coefficient `<var>` (bare), so the NUMERIC arm is right and they
#                 work today; `!is.numeric(Date)` is TRUE, so the old empirical gate wrongly classed
#                 them as factors. Keeping them numeric here preserves the working behaviour.
# Everything else non-numeric (character, factor, haven_labelled after tab_apply_val_labels) is a
# factor. The result is STORED in reg_meta$predictor_types (Phase 17 rule 2: roles are stored, never
# guessed -- the `level == var` convention that implicitly marks a numeric row is already broken by
# `cleannames` and by the multiplier relabel).
#' @keywords internal
reg_is_factor_var <- function(x) is.factor(x) || is.character(x) || is.logical(x)

# The predictor-kind map for a set of predictors: a named "factor"/"numeric" character vector.
#' @keywords internal
reg_predictor_types <- function(data, predictors) {
  if (length(predictors) == 0L) return(stats::setNames(character(0), character(0)))
  stats::setNames(
    vapply(predictors, function(p) if (reg_is_factor_var(data[[p]])) "factor" else "numeric",
           character(1)),
    predictors)
}

# The two halves of that map, as name vectors (the shape every call site actually wants).
#' @keywords internal
reg_factor_preds <- function(data, predictors)
  predictors[purrr::map_lgl(predictors, ~ reg_is_factor_var(data[[.x]]))]

#' @keywords internal
reg_numeric_preds <- function(data, predictors)
  predictors[!purrr::map_lgl(predictors, ~ reg_is_factor_var(data[[.x]]))]


# === `multiplier`: the per-unit scaling of a continuous predictor's effect (Last Phase z9) ===========
#
# GRAMMAR. A SCALAR ("sd", "2sd" or a number) applies to every numeric predictor; a NAMED vector
# overrides per variable and any predictor it does not name keeps the scalar default. Values may mix
# numbers and keywords. `multiplier = 1` therefore means "per 1 unit everywhere".
#
# WHY A KEYWORD AT ALL. Per 1 unit a numeric predictor is unreadable next to the factor contrasts beside
# it: measured on gss_simple, age's crude OR is 0.969 per year -- inside the FIRST colour break, so the
# row never colours and reads as "no effect" -- against 0.657 per SD, which sits squarely in the range
# of that table's factor contrasts (2.23 / 0.93 / 0.66 / 0.86 / 1.02).
#
# WHY THE SD IS FROZEN ONCE, ON THE PREDICTOR FRAME. It is measured on the complete cases of the
# PREDICTORS + design variables, deliberately NOT of the dependent -- so one predictor keeps ONE unit
# across several outcomes, across compared models and across split_var groups. A per-group SD would make
# `color = "between_groups"` compare different quantities: measured 15.91 vs 12.22 across a 2-group
# income split (30 % apart), 13.59 / 12.66 / 12.39 by race.
#
# WHERE IT IS RESOLVED. Once, in tab_reg(), before `shared` is built -- so the split recursion, the
# compared models, the crude companions and the jamovi cache key all see the SAME numbers. Never a
# keyword downstream: marginaleffects understands "sd" too, but as a CENTRED contrast on the SD of its
# own `newdata`, i.e. a per-group SD -- exactly what this rule forbids.

# The (optionally weighted) mean of one predictor -- the twin of reg_predictor_sd(), shared by the
# numeric crude tooltip.
#' @keywords internal
reg_weighted_mean <- function(x, w = NULL) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (!is.null(w)) { w <- as.numeric(w); ok <- ok & is.finite(w) & w > 0 }
  if (!any(ok)) return(NA_real_)
  if (is.null(w)) mean(x[ok]) else sum(w[ok] * x[ok]) / sum(w[ok])
}

# The SD of one predictor on the frozen frame (weighted when the design supplies weights).
#' @keywords internal
reg_predictor_sd <- function(x, w = NULL) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (!is.null(w)) { w <- as.numeric(w); ok <- ok & is.finite(w) & w > 0 }
  if (sum(ok) < 2L) return(NA_real_)
  if (is.null(w)) return(stats::sd(x[ok]))
  xw <- x[ok]; ww <- w[ok]
  m  <- sum(ww * xw) / sum(ww)
  sqrt(sum(ww * (xw - m)^2) / sum(ww))          # the ML weighted variance, as tab()'s numeric side uses
}

# Parse ONE multiplier value ("sd" / "2sd" / a number) against a predictor's frozen SD.
# Returns list(k = <numeric>, label = <character or NA>); k = NA drops the entry.
#' @keywords internal
reg_multiplier_value <- function(value, sd, digits = 3L) {
  v <- if (is.character(value)) trimws(tolower(value)) else value
  if (length(v) != 1L || is.na(v)) return(list(k = NA_real_, label = NA_character_))
  if (is.character(v) && v %in% c("sd", "1sd", "2sd")) {
    if (!is.finite(sd) || sd <= 0) return(list(k = NA_real_, label = NA_character_))
    mult <- if (identical(v, "2sd")) 2 else 1
    lab  <- if (mult == 1) "1 SD" else "2 SD"
    return(list(k = mult * sd, label = paste0(lab, " (", format(signif(mult * sd, digits)), ")")))
  }
  k <- suppressWarnings(as.numeric(v))
  if (!is.finite(k)) return(list(k = NA_real_, label = NA_character_))
  list(k = k, label = if (k == 1) NA_character_ else paste0(format(k), " units"))
}

# Resolve the whole `multiplier` argument into the frozen per-variable numbers + their unit labels.
# `default` is the scalar fallback applied to every numeric predictor the user did not name.
#' @keywords internal
reg_resolve_multiplier <- function(multiplier, default, data, num_preds, wt = NULL) {
  if (length(num_preds) == 0L) return(list(k = NULL, label = NULL))
  named  <- if (!is.null(multiplier) && !is.null(names(multiplier))) multiplier else NULL
  scalar <- if (is.null(multiplier)) default
            else if (is.null(names(multiplier))) multiplier[[1]]
            else default
  w   <- if (!is.null(wt) && is.character(wt) && length(wt) == 1L && wt %in% names(data))
           data[[wt]] else NULL
  sds <- vapply(num_preds, function(v) reg_predictor_sd(data[[v]], w), numeric(1))
  res <- purrr::map(stats::setNames(num_preds, num_preds), function(v) {
    val <- if (!is.null(named) && v %in% names(named)) named[[v]] else scalar
    reg_multiplier_value(val, sds[[v]])
  })
  k   <- vapply(res, function(z) z$k,     numeric(1))
  lab <- vapply(res, function(z) z$label, character(1))
  keep <- is.finite(k) & k != 1
  if (!any(keep)) return(list(k = NULL, label = NULL))
  list(k = k[keep], label = lab[keep])
}

# Auto-detect the model family from the outcome (a message is emitted). The safe data-driven rules
# (S37 D2): 0/1 or any 2-level outcome -> binomial; an ORDERED factor with 3+ levels -> ordinal
# (proportional-odds); an UNORDERED factor / character with 3+ levels -> multinomial; a non-integer
# numeric -> gaussian. An integer/count with 3+ values stays ambiguous (poisson vs grouped-binomial
# vs gaussian) and must be named explicitly. A binary outcome is ALWAYS binomial here: the modified
# Poisson (risk-ratio) path is opt-in via an explicit family = "poisson" (Last Phase z3).
reg_detect_family <- function(data, dependent) {
  y <- data[[dependent]]
  u <- unique(stats::na.omit(y))
  if (reg_is_binary_outcome(y)) {
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
  if (effect == "ame_ratio") return("RR")   # marginal RISK RATIO (ratio of adjusted probabilities)
  if (effect == "ame") return(if (at == "reference") "MER" else "AME")   # marginal effect at reference
  if (!do_exp) return("\u03b2")                  # beta (raw / log-odds coefficient)
  switch(family,
         "binomial" = , "multinomial" = , "ordinal" = "OR",
         "poisson" = , "quasipoisson" = "IRR",
         "rr" = "RR",                                # modified Poisson on a binary outcome: risk ratio
         "exp(\u03b2)")
}

# Phase 14w: the human name of the model family, shared by the reg title/caption and the "Model:" footer
# line (reg_model_line). do_exp/effect do not change the NAME (the estimand phrase carries that detail).
# Last Phase w: translatable (gettext). Every caller runs it inside a with_legend_lang() context
# (reg_model_lines / reg_title), so the LANGUAGE env is already set when these gettext() lookups fire.
reg_family_display_name <- function(family) {
  switch(family,
    "gaussian"     = gettext("linear regression"),
    "binomial"     = gettext("logistic regression"),
    "poisson"      = gettext("Poisson regression"),
    "quasipoisson" = gettext("quasi-Poisson regression"),
    "rr"           = gettext("modified Poisson regression"),
    "multinomial"  = gettext("multinomial logistic regression"),
    "ordinal"      = gettext("ordinal logistic regression"),
    gettext("regression"))
}

# Phase 14w: the short model tag used for Excel sheet names ("logit_<dep>_<pred>...").
reg_family_short <- function(family) {
  switch(family,
    "gaussian"     = "linear",
    "binomial"     = "logit",
    "poisson"      = "poisson",
    "quasipoisson" = "qpoisson",
    "rr"           = "rr",
    "multinomial"  = "mlogit",
    "ordinal"      = "ologit",
    "reg")
}

# The ESTIMAND phrase (lower-case fragment, never NULL) -- WHAT the numbers are. Composed into the
# "Model: <family>. <estimand>." footer line by reg_model_line(). effect="ame" and the multinomial
# "at reference" profile get their own phrasing; otherwise it is the coefficient/exp scale per family.
# Last Phase w: translatable (gettext); runs inside a with_legend_lang() context (see reg_model_lines /
# reg_title). Notation (OR/IRR/beta) lives in reg_effect_word, kept English; only this prose translates.
reg_model_note <- function(family, do_exp, effect = "coefficient", at = "average",
                           obs_in_cell = FALSE) {
  # Last Phase z10: where the crude effect has no column of its own it is FOLDED into the model cell as
  # "{or} ({obs})" / "{diff} ({obs})" -- so the bracket the footer names is the OBSERVED effect, not the
  # adjusted probability. One stored fact (reg_crude_in_cell), one wording swap.
  paren <- if (obs_in_cell)
    gettext("; each cell shows the modelled effect vs the reference level and, in parentheses, the observed (crude) one")
    else NULL
  # Last Phase z3: the ratio twin of the AME phrase. Guarded to prob-scale families upstream, so the
  # "adjusted predicted probability" wording always applies. Name the quantity a RATIO OF PROBABILITIES,
  # never a "log-linear model" -- in sociology that phrase means Goodman's contingency-table models.
  if (effect == "ame_ratio") {
    where <- if (at == "reference")
      gettext(" at the reference profile (other predictors held at their reference level / mean)")
    else gettext(" (sample-averaged)")
    return(paste0(gettext("marginal risk ratios (the ratio of adjusted predicted probabilities)"), where,
                  if (!is.null(paren)) paren else
                    gettext("; each cell shows the ratio vs the reference level and, in parentheses, the adjusted predicted probability")))
  }
  if (effect == "ame") {
    prob  <- reg_fam_prob(family)
    where <- if (at == "reference")
      gettext(" at the reference profile (other predictors held at their reference level / mean)")
    else gettext(" (sample-averaged)")
    return(if (prob)
      paste0(gettext("marginal effects on the probability scale (percentage points)"), where,
             if (!is.null(paren)) paren else
               gettext("; each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability"))
    else
      paste0(gettext("marginal effects on the response scale"), where))
  }
  if (at == "reference" && family == "multinomial") {
    return(gettext("odds ratios of each outcome category versus the rest, at the reference profile (other predictors held at their reference level / mean); profile-conditional"))
  }
  if (!is.null(paren)) return(paste0(reg_model_note(family, do_exp, effect, at, FALSE), paren))
  switch(family,
    "gaussian"     = gettext("coefficients (mean difference vs the reference category)"),
    "binomial"     = if (do_exp) gettext("odds ratios (vs the reference category)")
                     else        gettext("log-odds coefficients (vs the reference category)"),
    "poisson"      = ,
    "quasipoisson" = if (do_exp) gettext("incidence-rate ratios (vs the reference category)")
                     else        gettext("log-rate coefficients (vs the reference category)"),
    "rr"           = if (do_exp) gettext("risk ratios (vs the reference category)")
                     else        gettext("log-risk coefficients (vs the reference category)"),
    "multinomial"  = if (do_exp) gettext("odds ratios (each category vs the reference)")
                     else        gettext("log-odds coefficients (each category vs the reference)"),
    "ordinal"      = if (do_exp) gettext("cumulative odds ratios (proportional-odds model)")
                     else        gettext("proportional-odds model (log-odds coefficients)"),
    "")
}

# Phase 14w: the "Model: <family>. <estimand>." legend line, generated fresh from `reg_meta` at render
# so it can be ordered BEFORE the colour legend (item 2). For a model comparison the caption is not shown
# in the console, so the dependent + (binomial) reference level are named here too (item 4). NULL when the
# table is not a regression (get_reg_meta -> NULL).
# Last Phase w: the prose is translatable (gettext); called only from reg_model_lines(), which sets the
# LANGUAGE env via with_legend_lang(). enc2utf8 for the French accents (matches tab_weight_line et al.).
# Does ANY of these outcomes fold its observed effect into the model cell? Reads the STORED crude keys.
#' @keywords internal
reg_meta_obs_in_cell <- function(meta, deps = NULL) {
  ck <- meta$crude_keys
  if (is.null(ck)) return(FALSE)
  if (!is.null(deps)) ck <- ck[intersect(names(ck), deps)]
  any(purrr::map_lgl(ck, ~ !is.na(.x) &&
                       reg_crude_in_cell(.x, if (is.null(meta$effect)) "coefficient" else meta$effect,
                                         isTRUE(meta$do_exp))))
}

reg_model_line <- function(meta) {
  if (is.null(meta)) return(NULL)
  fam <- reg_family_display_name(meta$family)
  est <- reg_model_note(meta$family, meta$do_exp, meta$effect, meta$at,
                        obs_in_cell = reg_meta_obs_in_cell(meta))
  # `who` carries no leading space (gettext msgids must not have edge whitespace -- xgettext strips it);
  # the space + full punctuation live in the outer gettextf template, so French controls "Modele : ... ; ."
  who <- if (isTRUE(meta$comparison)) {
    pl <- meta$positive_level[[1]]
    w  <- if (!is.na(pl)) gettextf("of %s ('%s')", meta$dependent[[1]], pl)
          else            gettextf("of %s", meta$dependent[[1]])
    paste0(" ", w)
  } else ""
  line <- if (nzchar(est)) gettextf("Model: %s%s; %s.", fam, who, est)
          else            gettextf("Model: %s%s.", fam, who)
  enc2utf8(line)
}

# Phase 15e: the "Model:" footer line(s). A homogeneous table returns the single reg_model_line (byte-
# identical). A mixed-family table returns ONE line per distinct outcome family present, each prefixed by
# the outcomes it covers (legend_name_list), so every estimand is described without inventing a single
# false family for the whole table. `x` is the table (reads its `reg_meta`). Returns a character vector.
# Last Phase w: `lang` selects the footer language (NULL -> options(tabxplor.lang)/locale). The whole
# composition runs under with_legend_lang() so every nested gettext() (family name, estimand, "Model:")
# resolves to that language; English is byte-identical (gettext returns the msgid under the en locale).
reg_model_lines <- function(x, lang = NULL) {
  meta <- get_reg_meta(x)
  if (is.null(meta)) return(character(0))
  with_legend_lang(lang, function(lg) {
    fams <- meta$families; if (is.null(fams)) fams <- meta$family
    uf   <- unique(fams)
    if (length(uf) <= 1L) { rl <- reg_model_line(meta); return(if (is.null(rl)) character(0) else rl) }
    deps <- meta$dependent
    vapply(uf, function(fm) {
      grp   <- deps[fams == fm]
      fname <- reg_family_display_name(fm)
      dox   <- isTRUE(meta$exponentiate) && fm != "gaussian"
      est   <- reg_model_note(fm, dox, meta$effect, meta$at,
                              obs_in_cell = reg_meta_obs_in_cell(meta, grp))
      enc2utf8(if (nzchar(est)) gettextf("Model (%s): %s; %s.", legend_name_list(grp), fname, est)
               else            gettextf("Model (%s): %s.", legend_name_list(grp), fname))
    }, character(1), USE.NAMES = FALSE)
  })
}

# Last Phase z8: the AGGREGATED effect-modification test, as one footer line per model -- the
# table-wide companion of the per-cell `between_groups` colour ("does this predictor act differently
# between groups?", once per predictor, for all its levels together, with no per-cell multiplicity).
# It is a LINE, not footer rows: a pooled test belongs to no single model column, which is the only
# thing the footer-row machinery can key on (see reg_interaction_rows). Rendered by
# tab_footer_streams() beside the weight / "Model:" lines, so every backend gets it from one producer.
#
# The mention "on the coefficients" is added ONLY when the cells are not coefficients (effect = "ame" /
# "ame_ratio"): there the footer tests whether the model COEFFICIENTS differ between groups while the
# colours score the difference in marginal effects -- related, but not the same null. On a coefficient
# table the words would be noise.
#' @keywords internal
reg_interaction_lines <- function(x, lang = NULL) {
  tt <- get_test(x)
  if (is.null(tt) || nrow(tt) == 0) return(character(0))
  it <- tt[tt$test %in% reg_interaction_types(), , drop = FALSE]
  if (nrow(it) == 0) return(character(0))
  meta <- get_reg_meta(x)
  sv   <- if (is.null(meta)) NA_character_ else meta$split_var
  with_legend_lang(lang, function(lg) {
    tname <- c(interact_lr = gettext("likelihood ratio"), interact_f = gettext("F test"),
               interact_wald = gettext("Wald test"))
    on_coef <- !is.null(meta) && isTRUE(meta$effect %in% c("ame", "ame_ratio"))
    # split() by a FACTOR of first-appearance order, so several models keep their column order.
    vapply(split(seq_len(nrow(it)), factor(it$col_var, levels = unique(it$col_var))), function(idx) {
      d     <- it[idx, , drop = FALSE]
      items <- paste0(d$row_var, " p = ", test_fmt_pvalue(d$pvalue), stars_from_pvalue(d$pvalue))
      kind  <- unname(tname[d$test[1]]); if (is.na(kind)) kind <- gettext("Wald test")
      what  <- if (on_coef) gettextf("%s on the coefficients", kind) else kind
      head  <- if (!is.na(sv) && nzchar(sv)) gettextf("Interaction with %s (%s):", sv, what)
               else                          gettextf("Interaction (%s):", what)
      enc2utf8(paste0(head, " ", paste(items, collapse = ", "), "."))
    }, character(1), USE.NAMES = FALSE)
  })
}

# Phase 14w: the reg table's TITLE / caption (Excel title + sheet, md/kable caption). Single model:
# "<Family>: <dep> by <p1>, <p2> +N more". Comparison: "<Family>s (models comparison): <dep>, '<ref>'
# (<effect>)" -- the reference level + effect that would otherwise be written nowhere (item 4).
# Last Phase w: the caption prose is translatable (gettext), resolved under with_legend_lang(). `lang`
# NULL follows options(tabxplor.lang)/locale (so a French-locale user gets a French caption with no arg
# threading through the export prep); English is byte-identical. The comparison "s ..." plural suffix is
# gettext'd as a whole fragment so English keeps its exact "regressions" wording -- French refines it in
# the catalogue if desired. Notation (eff_word: OR/IRR/beta) stays English.
reg_title <- function(meta, max = 2, lang = NULL) {
  if (is.null(meta)) return(NA_character_)
  # Phase 15e: a mixed-family table has no single family -> a generic caption ("Regression models").
  fams <- meta$families; if (is.null(fams)) fams <- meta$family
  mixed <- length(unique(fams)) > 1L
  with_legend_lang(lang, function(lg) {
    fam <- reg_family_display_name(meta$family)
    Fam <- if (mixed) gettext("Regression models")
           else paste0(toupper(substr(fam, 1, 1)), substr(fam, 2, nchar(fam)))
    dep <- tab_title_names(meta$dependent, max)
    # edge whitespace stays OUT of gettext msgids (xgettext strips it); the leading space is added here.
    tabbed <- if (!is.null(meta$split_var)) paste0(" ", gettextf("(tabbed by %s)", meta$split_var)) else ""
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

# Phase 14w (item 1): a compact Excel sheet name for a reg table -- "<short>_<dep>_<pred>..." (e.g.
# "logit_married_race_rincome"), truncated to 25 chars by the caller. A comparison collapses the
# predictors to "compare" (they differ per model).
reg_sheet_name <- function(meta) {
  if (is.null(meta)) return(NA_character_)
  fams <- meta$families; if (is.null(fams)) fams <- meta$family
  tail <- if (isTRUE(meta$comparison)) c(meta$dependent[[1]], "compare")
          else                         c(meta$dependent, meta$predictors)
  # Phase 15e: a mixed-family table gets a generic short tag ("reg") instead of one family's.
  short <- if (length(unique(fams)) > 1L) "reg" else reg_family_short(meta$family)
  paste(c(short, tail), collapse = "_")
}

# Phase 14w (item 3): the shared col_var for a SINGLE-outcome model column + its empirical companions,
# so ONE span header names the outcome and no border separates them (they share a col_var). Binomial ->
# "<dep>: <positive_level>"; a numeric outcome (gaussian/poisson) -> the dependent name alone. NOT used in
# comparison mode (each model keeps its own col_var = model name, so borders separate the models, and the
# outcome / reference / effect go in the title instead).
reg_shared_col_var <- function(family, dependent, positive_level, cleannames) {
  if (reg_fam_binary(family) && !is.null(positive_level) && !is.na(positive_level)) {
    pl <- reg_cleanup(positive_level, cleannames)
    paste0(dependent, ": ", pl)
  } else dependent
}

# Phase 14w (item 3): the single-model column NAME ("Model_OR" / "Model_IRR" / "Model_AME (adjusted %)"),
# so the effect word lives in the column, not repeated in the span. Comparison mode keeps the model name;
# a multi-dependent (several outcomes, one predictor set) suffixes the dependent so the names stay unique.
# Phase g: "Model_" (snake-case) prefix; the multi-dependent disambiguator is a "[dep]" BRACKET, which the
# console shows and every exporter STRIPS (tab_col_var_header) -- the col_var span row already names the
# outcome, so repeating it per column wasted export width.
reg_model_col_name <- function(eff_word, dependent, is_comparison, model_label, n_dep) {
  if (isTRUE(is_comparison)) return(model_label)
  if (n_dep > 1L) paste0("Model_", eff_word, " [", dependent, "]") else paste0("Model_", eff_word)
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
    if (reg_is_factor_var(v)) {
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
  stringi::stri_replace_all_regex(label, "([.\\\\+*?\\[^\\]$(){}=!<>|:#/-])", "\\\\$1")
}

# Strip factor code prefixes + trailing parentheticals off a label when `cleannames` is on (the ONE
# wrapper over cleannames_condition(), previously inlined at 8 sites). Vectorised; a no-op when off.
reg_cleanup <- function(x, cleannames)
  if (isTRUE(cleannames)) stringi::stri_replace_all_regex(x, cleannames_condition(), "") else x

# The (var, level [, extra]) join key that aligns fitted results back onto skeleton rows. A carriage
# return separates the parts (it never appears in a variable name / factor level). `extra` adds a third
# component (the multinomial outcome category, for the crude tooltips).
reg_skel_key <- function(var, level, extra = NULL)
  if (is.null(extra)) paste(var, level, sep = "\r") else paste(var, level, extra, sep = "\r")

# Match a skeleton's rows into a source tibble that carries $var/$level (returning the match index);
# NA for every row when `src` is empty (the fallback several callers wrote by hand).
reg_skel_match <- function(skeleton, src) {
  if (is.null(src) || !nrow(src)) return(rep(NA_integer_, nrow(skeleton)))
  match(reg_skel_key(skeleton$var, skeleton$level), reg_skel_key(src$var, src$level))
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
  stats::setNames(p, stringi::stri_replace_all_regex(colnames(X), "`", ""))
}

# The shared Wald assembly (the est +/- crit.se -> p-dual -> exp block, previously open-coded 3x). From
# an estimate + its standard error, form the CI (est +/- crit*se) and the two-sided p (the z/t dual
# 2*pnorm/pt(-|est/se|)), then exponentiate estimate + bounds when `do_exp` (OR/IRR). Any of `lo`/`hi`/`p`
# supplied pre-computed (a profile CI, an LR / scaled-Wald p) is used as-is; the `do_exp` step still
# applies. `disp_known` picks z (fixed dispersion: unweighted binomial/poisson, ML) vs t on `df` (estimated
# dispersion: lm, quasi*, weighted). Returns a list estimate/conf.low/conf.high/p.value.
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

# Wald CI + p from a tidy carrying `estimate` + `std.error` on the log scale. multinom / polr are ML
# with fixed dispersion, so the quantile is z (qnorm) -- the same branch the fixed-dispersion glm path
# uses. Both CI and p come from estimate/se, so they are exact duals (CI <-> stars can never disagree),
# and both survive an NaN se (a rank-deficient / empty cell -> NaN, matching the base model). `do_exp`
# exponentiates the estimate and the bounds (OR/IRR). Fills conf.low/conf.high/p.value in place.
reg_wald_from_tidy <- function(td, conf_level, do_exp) {
  res <- reg_wald_finalize(td$estimate, do_exp, se = td$std.error,
                           crit = stats::qnorm(1 - (1 - conf_level) / 2))
  td$estimate <- res$estimate; td$conf.low <- res$conf.low
  td$conf.high <- res$conf.high; td$p.value <- res$p.value
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
    td   <- tibble::tibble(y.level = ylev[k], term = stringi::stri_replace_all_regex(trm, "`", ""),
                           estimate = unname(cf), std.error = unname(se[nm]))
    td   <- reg_wald_from_tidy(td, conf_level, do_exp)
    return(list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL,
                fit = fit, data = mdata, y_ref = y_levels[1], y_levels = y_levels[-1]))
  }

  fit <- nnet::multinom(fml, data = mdata, trace = FALSE)
  td  <- broom::tidy(fit)                              # y.level, term, estimate, std.error, ...
  td$term <- stringi::stri_replace_all_regex(td$term, "`", "")     # strip formula backticks -> match skeleton
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
    td  <- tibble::tibble(term = stringi::stri_replace_all_regex(names(cf), "`", ""),
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
  td$term <- stringi::stri_replace_all_regex(td$term, "`", "")
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

# Last Phase j: the design constructors are single-sourced in R/survey-design.R (svy_*) and shared with
# tab()'s survey-design tests. These thin wrappers keep the reg_* call sites + byte-identical behaviour.
reg_design_formula <- function(x) svy_design_formula(x)
reg_design_vars    <- function(design_spec) svy_design_vars(design_spec)
reg_make_design    <- function(data, wt, ids, strata, fpc, nest)
  svy_make_design(data, wt, ids, strata, fpc, nest)
# Subset a prebuilt design to the model's complete cases, then swap its model frame for the recoded
# `mdata` (drop_na + fct_drop + reg_prep_binary + grouped-binomial cols already applied). The design
# metadata slots (strata / cluster / fpc / prob) are subset by `[` and stay row-aligned with mdata.
reg_subset_design <- function(design, keep_mask, mdata) {
  dd <- design[keep_mask, ]
  dd$variables <- mdata
  dd
}
# The model's complete-case frame: drop rows missing the dependent, ANY predictor, or a design var --
# the ONE definition of "the same population as the model". reg_fit uses it for the fit; the empirical /
# multinomial-tip blocks recompute it from raw `data` (the fitted `f$data` is NULL on the reref/digest
# path, so it cannot be read back there). `intersect(., names(data))` guards vars absent from the frame.
reg_complete_frame <- function(data, vars)
  tidyr::drop_na(data, tidyselect::all_of(intersect(unique(vars), names(data))))

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
                    trials = NULL, formula = NULL, multiplier = NULL, cross = NULL,
                    drop_extra = NULL) {
  # Last Phase z8: `cross` (a split_var) makes the POOLED interaction fit `y ~ (x1 + x2) * g`, used
  # only by reg_interaction_rows(). It goes through this whole function rather than the `formula =`
  # escape hatch precisely so it inherits the binary prep, the grouped-binomial cbind, the family
  # objects, the "rr" -> svyglm route and the design resolution; `formula =` deliberately disables the
  # first two. `cross` joins drop_vars so the pooled complete-case frame matches the per-group ones.
  #
  # Last Phase z9: `drop_extra` joins drop_vars but NOT the formula -- variables the fit must be
  # COMPLETE ON without modelling. The crude univariable fit behind a numeric predictor's `Obs_*` column
  # uses it to land on exactly the model's population (`empirical`'s documented contract, and the row
  # identity the gap test's influence functions need). Passing the pre-filtered frame as `data` instead
  # is NOT equivalent: reg_resolve_design() computes a PREBUILT design's keep_mask from `data` itself,
  # and a shorter mask recycles silently against the design's rows -- wrong numbers, no error.
  drop_vars <- unique(c(dependent, predictors, cross, drop_extra, reg_design_vars(design_spec)))
  mdata     <- reg_complete_frame(data, drop_vars)

  fac_preds <- reg_factor_preds(mdata, c(predictors, cross))
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
    # Last Phase z3 -- modified Poisson on a binary outcome (Zou 2004). Same binary prep as the logistic
    # arm (so `inverse_two_level_factors` and the positive-level label behave identically), then the
    # factor is coerced to the 0/1 NUMERIC a log-link Poisson needs: glm(poisson) / svyglm both error on
    # a factor response. quasipoisson (not poisson) in BOTH the weighted and unweighted case -- the fit
    # goes through svyglm either way (see the dispatch below), and it also makes AIC/BIC return NA, which
    # is the honest answer for a quasi-likelihood.
    "rr" = {
      mdata <- reg_prep_binary(mdata, dependent, inverse_two_level_factors)
      positive_level <- attr(mdata, "positive_level")
      mdata[[dependent]] <- as.numeric(mdata[[dependent]] == positive_level)
      stats::quasipoisson("log")
    },
    "gaussian" = stats::gaussian(),
    cli::cli_abort("Unsupported {.arg family}: {.val {family}}.")
  )
  if (is.null(formula) && !grouped && !reg_fam_binary(family) && !is.numeric(mdata[[dependent]])) {
    cli::cli_abort(c(
      "A {.val {family}} outcome must be numeric.",
      "x" = "{.val {dependent}} is {.cls {class(mdata[[dependent]])}}."
    ))
  }

  fml <- if (!is.null(formula)) {
    formula                                            # compound escape-hatch: fit verbatim
  } else {
    resp <- if (grouped) "cbind(`.gb_succ`, `.gb_fail`)" else paste0("`", dependent, "`")
    rhs  <- paste0("`", predictors, "`", collapse = " + ")
    if (!is.null(cross)) rhs <- paste0("(", rhs, ") * `", cross, "`")   # z8: the pooled interaction fit
    stats::as.formula(paste0(resp, " ~ ", rhs))
  }

  # Last Phase z3: "rr" ALWAYS fits through svyglm, weighted or not. A Poisson likelihood on a 0/1
  # outcome is deliberately misspecified (Var = mu, truth = mu(1-mu)), so the naive SEs are too large
  # and must be replaced by the Huber-White SANDWICH. svyglm's design-based variance IS that sandwich
  # (measured: exactly HC0 * sqrt(n/(n-1)) on a constant-weight ids=~1 design, coefficients identical to
  # glm). Reusing it rather than hand-rolling the matrix keeps ONE encoding of the variance rule -- and
  # crucially reg_build_digest() stores vcov(fit), which for an svyglm is already the sandwich, so the
  # jamovi reref byte-identity contract needs no special case. `weighted` stays FALSE for an unweighted
  # "rr": it is a whole-call scalar that a mixed table shares, so it must keep meaning "the USER gave a
  # design". The "rr" branches in reg_glance / reg_footer_stats / reg_compare_rows key on the family.
  use_svy <- weighted || family == "rr"
  fit <- if (family == "gaussian" && !weighted) {
    stats::lm(fml, data = mdata)
  } else if (!use_svy) {
    stats::glm(fml, data = mdata, family = fam_obj)
  } else {
    # svyglm on the design for this model's complete cases (built or subset via make_design; an
    # unweighted "rr" gets reg_make_design's ids = ~1, weights = NULL constant-weight design).
    survey::svyglm(fml, design = make_design(mdata), family = fam_obj)
  }
  # make survey::svyglm visible to AIC / anova null-refits. Fit-driven, not flag-driven, so it covers
  # the unweighted "rr" too (svyolr / svy_vglm return earlier and are not svyglm -- unchanged).
  if (inherits(fit, "svyglm")) fit <- reg_svyglm_env(fit)

  td <- broom::tidy(fit)                            # native scale: estimate, std.error, p.value
  td$term <- stringi::stri_replace_all_regex(td$term, "`", "")  # strip formula backticks -> match skeleton

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

  # "rr" is excluded by construction (the test names binomial/poisson, never the "rr" key) -- but say so,
  # rather than silently downgrading: a profile likelihood on a deliberately misspecified quasi-likelihood
  # is not a meaningful interval, and the robust Wald IS the method the modified Poisson is defined with.
  use_profile <- method == "profile" && !weighted && family %in% c("binomial", "poisson")
  if (method == "profile" && weighted) {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for survey-weighted ",
                                   "models; using Wald.")))
  } else if (method == "profile" && family == "rr") {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for a modified Poisson ",
                                   "fit (a quasi-likelihood); using the robust Wald interval.")))
  }

  if (use_profile) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      cli::cli_abort(c('{.pkg MASS} is required for {.code method = "profile"}.',
                       "i" = '- Install it, or use {.code method = "wald"} (the default).'))
    }
    ci   <- suppressMessages(stats::confint(fit, level = conf_level))   # log/native scale
    idx  <- match(td$term, stringi::stri_replace_all_regex(rownames(ci), "`", ""))
    lo   <- unname(ci[idx, 1]) * mult_vec; hi <- unname(ci[idx, 2]) * mult_vec  # scale profile bounds
    lrp  <- reg_lr_pvalues(fit)
    p_in <- unname(lrp[match(td$term, names(lrp))])
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
    p_in <- if (scaled) 2 * stats::pt(-abs(td$estimate / td$std.error), df = stats::df.residual(fit))
            else        td$p.value
  }
  res <- reg_wald_finalize(td$estimate, do_exp, lo = lo, hi = hi, p = p_in)   # shared exp assembly
  td$estimate <- res$estimate; td$conf.low <- res$conf.low
  td$conf.high <- res$conf.high; td$p.value <- res$p.value

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
                       color, color_signif, model_family = "") {
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
  # in_refrow is a UNION-skeleton row fact (any predictor's reference level + the Constant), NOT gated
  # by in_model: a model that OMITS a predictor must not blank that predictor's reference-row flag, else
  # the shared cross-column bold (tab_bold_rows ANDs in_refrow) drops its bold in a comparison. The
  # absent cell stays NA-valued (ref_lvl above zeroes only present predictors) -- only the flag changes.
  refrows  <- (skeleton$is_ref & skeleton$var != "Constant") | skeleton$var == "Constant"

  if (effect_shape == "ratio") {
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): whole-model N is in the footer, not a per-cell "n:"
      or = est, ci_inf = lo, ci_sup = hi, pvalue = p,
      type = "row", display = "or", digits = 2L, ref = "1", ci_type = "or",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  } else {
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): whole-model N is in the footer, not a per-cell "n:"
      diff = est, ci_inf = lo, ci_sup = hi, pvalue = p,
      var = rep(fit_res$var_y, n_rows),                 # var(Y): standardizes beta/SD(Y) for colour
      type = "coef", display = "coef", digits = 2L, ci_type = "diff",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
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
                                       numeric_preds, model_predictors, multiplier = NULL) {
  if (mode == "value") return(col)
  if (mode == "ci")    return(set_display(col, "est_ci"))
  # Phase 15e: the prob/ame folds need a binomial coefficient model; a non-binomial column of a mixed
  # table shows the CI bracket instead (the whole-call degrade only fires when NO outcome is binomial).
  if (mode %in% c("prob", "ame") && !identical(family, "binomial")) return(set_display(col, "est_ci"))

  marg     <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                           at = "average", want_pred = mode == "prob", multiplier = multiplier)
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_const <- skeleton$var == "Constant"
  is_ref   <- skeleton$is_ref & !is_const & in_model
  disp     <- get_display(col)
  if (mode == "prob") {
    prd    <- marg$pred
    pred_v <- if (nrow(prd)) prd$pred[reg_skel_match(skeleton, prd)] else rep(NA_real_, nrow(skeleton))
    col    <- vctrs::`field<-`(col, "pct", pred_v)
    disp[in_model & !is_const & !is.na(pred_v)] <- "{or} ({pct})"
  } else {                                                   # "ame"
    amt    <- marg$ame
    ame_v  <- amt$ame[reg_skel_match(skeleton, amt)]
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
# reg_crude_y() -- Last Phase z8-B: the outcome ON THE SCALE THE CRUDE ESTIMATOR AVERAGES. For a binary
# outcome that is the 0/1 indicator of the positive level, which needs reg_prep_binary()'s own recode
# mirrored (the model frame has been through it; the raw `data` the crude block reads has not, so
# as.character(0/1) would never match the label and the crude base would silently be 0 -- the pre-14v-ii
# bug). Otherwise the numeric value. ONE definition, shared by reg_empirical()'s cell means and
# reg_crude_if_maker()'s residuals -- else the influence function could be built around a different `y`
# than the estimate it is the standard error OF.
#' @keywords internal
reg_crude_y <- function(data, dependent, family, positive_level) {
  yv <- data[[dependent]]
  if (!reg_fam_binary(family)) return(as.numeric(yv))
  if (is.numeric(yv) && all(stats::na.omit(yv) %in% c(0, 1)))
    yv <- factor(yv, levels = c(0, 1), labels = c(paste0("Not ", dependent), dependent))
  as.numeric(as.character(yv) == positive_level)
}

# reg_crude_yw() -- Last Phase z10: reg_crude_y()'s generalisation, the ONE description of "what the
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
reg_crude_yw <- function(data, dependent, crude_key, positive_level = NULL, wt = NULL,
                         trials = NULL, ref_category = NULL) {
  w <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  if (identical(crude_key, "grouped_binomial")) {
    # `trials` is the resolved COUNT of Bernoulli draws per row (tab_reg turns TRUE / a named vector
    # into an integer before the specs are built), never a column name.
    s  <- as.numeric(data[[dependent]])
    tr <- rep_len(as.numeric(trials), length(s))
    return(list(y = s / tr, w = w * tr, kind = "share", cats = c("1", "0"), ref = "0",
                draws = tr, num = s, num_w = w))
  }
  if (identical(crude_key, "multinomial") || identical(crude_key, "ordinal")) {
    yv   <- forcats::fct_drop(as.factor(data[[dependent]]))
    cats <- levels(yv)
    return(list(y = as.character(yv), w = w, kind = "labels", cats = cats,
                ref = if (!is.null(ref_category) && ref_category %in% cats) ref_category else cats[1],
                draws = rep(1, nrow(data)), num = NULL, num_w = NULL))
  }
  if (identical(crude_key, "binomial") || identical(crude_key, "rr")) {
    # 0/1, so the "share" arithmetic below reduces to the indicator sums the binary arm always used.
    return(list(y = reg_crude_y(data, dependent, "binomial", positive_level), w = w, kind = "share",
                cats = c("1", "0"), ref = "0", draws = rep(1, nrow(data)), num = NULL, num_w = NULL))
  }
  yn <- as.numeric(data[[dependent]])
  list(y = yn, w = w, kind = "numeric", cats = "", ref = NA_character_,
       draws = rep(1, nrow(data)), num = yn, num_w = w)
}

# The zero-row shape of reg_empirical()'s long tibble -- ONE definition, so the empty case cannot drift
# from the populated one (Last Phase z9).
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

# reg_empirical() -- Last Phase z10: THE crude grid, keyed (var, level, category).
#
# DESIGN -- this ONE producer replaces reg_empirical() + reg_empirical_tips(), which were the same
# computation at two key widths (measured line by line: the tips' `sum(w[m & y == cat]) / sum(w[m])`
# is bit-identical to the old binary branch's `wpos / (wpos + wneg)`). The tips version was simply the
# general K-category form; the old binary one was its K = 2, positive-level-only slice. Merging them is
# what lets multinomial and ordinal have a crude counterpart at all, at the cost of one extra key column.
#
# Two PARTS, because a family may need either or both (Last Phase z10 ruling: a grouped binomial shows a
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
# Weighted rule (SS14, unchanged): weighted proportions/means, unweighted `n`, and a SEPARATE effective
# n (`n_ci`) for the intervals -- the Kish n_eff when opted in, else the raw count, so off-kish is
# byte-identical.
reg_empirical <- function(data, fac_preds, dependent, crude_key, positive_level, wt,
                          trials = NULL, ref_category = NULL, conf_level = 0.95) {
  yw   <- reg_crude_yw(data, dependent, crude_key, positive_level, wt, trials, ref_category)
  cats <- yw$cats
  kish <- !is.null(wt) && isTRUE(getOption("tabxplor.kish_neff", FALSE))
  neff_or_n <- function(wsum, w2, raw) {
    if (!kish) return(as.double(raw))
    ne <- wsum^2 / w2
    if (is.finite(ne)) ne else as.double(raw)
  }
  has_num <- !is.null(yw$num)
  has_cat <- !identical(yw$kind, "numeric")
  share   <- identical(yw$kind, "share")
  # variance only where a mean column is actually built (gaussian / poisson / the grouped mean score)
  want_var <- has_num
  # Last Phase z9: a TYPED zero-row return. purrr::map_dfr over character(0) yields a 0x0 tibble, whose
  # columns are NULL -- reg_empirical_columns() then errors ("Can't recycle input of size 0").
  if (length(fac_preds) == 0L) return(reg_empirical_empty())

  purrr::map_dfr(fac_preds, function(p) {
    x  <- data[[p]]
    ok <- !is.na(x) & !is.na(yw$w) & !is.na(yw$y)
    if (has_num) ok <- ok & !is.na(yw$num)
    lv <- levels(forcats::fct_drop(as.factor(x[ok])))
    per <- purrr::map(lv, function(l) {
      m  <- ok & x == l
      wl <- sum(yw$w[m])
      # "share": y is the per-row SHARE of successes (0/1 for an ordinary binary outcome, succ/trials
      # for a grouped one), so the 2x2 legs are Sum(w*y) / Sum(w*(1-y)) -- which for 0/1 IS the indicator
      # sum the binary arm always computed. "labels": one indicator per outcome category.
      wc <- if (!has_cat) NA_real_
            else if (share) stats::setNames(c(sum(yw$w[m] * yw$y[m]), sum(yw$w[m] * (1 - yw$y[m]))),
                                            cats)
            else vapply(cats, function(k) sum(yw$w[m & yw$y == k]), numeric(1))
      out <- list(
        n     = sum(m),
        n_ci  = neff_or_n(wl, sum(yw$w[m]^2), sum(m)),
        # the CI base of a PROPORTION is the number of Bernoulli DRAWS, which for a grouped binomial is
        # `trials` per respondent. Equal to n_ci for every other outcome -> byte-identical.
        n_draw = neff_or_n(wl, sum(yw$w[m]^2), sum(m)) * mean(yw$draws[m]),
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
        # the numeric part re-derives its own effective n from the per-respondent weights
        out$n_ci <- neff_or_n(wn, sum(nw[m]^2), n1)
      }
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
    n_draw <- rep_lv("n_draw"); r_n_draw <- rep(ref$n_draw, nl * nc)
    # the crude ODDS ratio (category vs the reference CATEGORY, level vs the reference LEVEL) where the
    # outcome has categories; the crude RATE ratio (mean / reference mean) where it does not.
    # WARNING: the divisor is the reference LEVEL's own wpos/wneg, i.e. the SAME expression as the
    # numerator -- not the algebraically-equal `ref$prop / ref$prop[ref_cat]`, whose last bit differs
    # and made the reference cell print "1/1" (an OR of 1 - 1e-16 renders as its own reciprocal).
    emp_ratio <- if (has_cat) {
      (wpos / wneg) / rep(unname(ref$wpos / ref$wneg), times = nl)
    } else meanv / rmean
    pw <- if (has_cat) ci_wilson(prop, n_draw, conf_level = conf_level) else
      list(inf = rep(NA_real_, nl * nc), sup = rep(NA_real_, nl * nc))
    dd <- if (has_cat) ci_prop_diff(prop, n_draw, rprop, r_n_draw, conf_level = conf_level,
                                    method = "newcombe", want_p = FALSE) else pw
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
}

# reg_empirical_fit() -- Last Phase z9 (numeric predictors) / z10 (ordinal outcomes): the crude
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
# model's family, design, CI method, `inverse_two_level_factors` and `multiplier` -- ruling Q6 by
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
reg_empirical_fit <- function(data, preds, dependent, family, design_spec, inverse,
                              conf_level, method, skeleton, multiplier = NULL,
                              other_preds = character(0), effect = "coefficient", wt = NULL,
                              want_fit = FALSE, marginal = FALSE, trials = NULL) {
  if (length(preds) == 0L) return(list(est = list(), fits = list()))
  ratio  <- identical(effect, "ame_ratio")
  skey   <- reg_skel_key(skeleton$var, skeleton$level)
  rows   <- list()
  fits   <- list()
  for (v in preds) {
    f <- tryCatch(
      suppressMessages(reg_fit(data, dependent, v, family, design_spec, do_exp = FALSE,
                               inverse, conf_level, method,
                               trials = trials, formula = NULL, multiplier = multiplier,
                               drop_extra = setdiff(other_preds, v))),
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
        multiplier = multiplier)), error = function(e) NULL)
      if (is.null(m) || !nrow(m$ame)) next
      a <- m$ame[m$ame$var == v, , drop = FALSE]
      if (!nrow(a)) next
      idx <- match(reg_skel_key(a$var, a$level), skey)
      ok  <- !is.na(idx)
      if (!any(ok)) next
      a <- a[ok, , drop = FALSE]; idx <- idx[ok]
      est <- a$ame; lo <- a$ame_lo; hi <- a$ame_hi
      # reg_marginal() exp()s a log-ratio before returning, so log it back: this function's contract is
      # the NATIVE (link) scale, and reg_fit_overlay() re-exponentiates per the shape's own ci_type.
      if (ratio) { est <- log(est); lo <- log(lo); hi <- log(hi) }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        category = ifelse(is.na(a$group), "", a$group), row = idx,
        est = est, lo = lo, hi = hi, p = a$ame_p)
    }
  }
  if (!length(rows)) return(list(est = list(), fits = fits))
  all <- vctrs::vec_rbind(!!!rows)
  list(est = split(all[setdiff(names(all), "category")], all$category), fits = fits)
}


# reg_fit_overlay() -- Last Phase z9 (as reg_num_overlay) / z10: write fit-derived crude rows into a
# finished crude EFFECT column and into the crude effect VECTOR, at the ONE point both are in hand.
#
# DESIGN -- why here and not before emp_col(). On the binomial `ame` branch the base column and the
# effect column are built from the SAME `rd_fields` list, and REG_EMPIRICAL$binomial$base declares
# `color = "diff"` -- so overlaying the estimate into those shared locals would have written the AME into
# `Obs_%`'s `diff` field and COLOURED a cell that displays nothing. emit() is the one place the effect
# shape is known and only the effect column is touched.
#
# The estimate lands in the field its `ci_type` declares (fmt_est_of()'s rule), exp()d exactly when that
# ci_type is "or" -- which is also what tells this function whether the shape is an exponentiated effect
# or its log twin. `n` is deliberately left NA: like the model column's, a fit-derived row's base is the
# whole model N, which belongs in the footer, not in a per-cell "n:".
#' @keywords internal
reg_fit_overlay <- function(col, eff, est, shape) {
  if (is.null(est) || !nrow(est)) return(list(col = col, eff = eff))
  idx <- est$row
  e <- est$est; lo <- est$lo; hi <- est$hi; p <- est$p
  if (identical(as.character(shape$ci_type)[1], "or")) {
    e <- exp(e); lo <- exp(lo); hi <- exp(hi)
  }
  fld <- fmt_est_field(shape$ci_type)
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
# the base descriptive column + the crude-effect column (fmt type / display / digits / ref / ci_type /
# colour measure + the visible name), plus the CI METHOD literal the crude interval uses. The per-family
# CI MATH stays code below (ci_prop_diff / ci_or / ci_pivot / ci_mean_diff2 / ci_mean_ratio take
# different arguments), but the near-identical fmt() calls collapse into ONE builder (emp_col), and the
# `method_*` literals are the SAME the colour legend names -- ci_settings reads them straight from here
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
# Last Phase z8-B: each EFFECT row also carries the `link` of the crude estimator it describes -- the
# one fact reg_crude_if_maker() needs to write its closed-form influence function (g'(mu) = 1/(mu(1-mu))
# logit | 1/mu log | 1 identity). It sits on the SHAPE row, not on the family, because the crude link
# follows the chosen ESTIMAND: a binomial model shows a logit-scale OR by default, an IDENTITY-link risk
# difference under effect = "ame", and a LOG-link risk ratio under "ame_ratio" (which reuses
# REG_EMPIRICAL$rr$rr verbatim -- the very reuse that makes a per-family link impossible). A `base` row
# is descriptive, never an effect, so its link is NA.
REG_EMPIRICAL <- list(
  binomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    base   = list(nm = "Obs_%",       type = "row",  display = "pct",  digits = 0L, ref = "tot",           ci_type = "diff",  color = "diff", link = NA_character_),
    ame    = list(nm = "Obs_diff",    type = "row",  display = "diff", digits = 0L, ref = "tot",           ci_type = "diff",  color = "diff", link = "identity"),
    or     = list(nm = "Obs_OR",      type = "row",  display = "or",   digits = 2L, ref = "1",             ci_type = "or",    color = "OR",   link = "logit"),
    or_log = list(nm = "Obs_log(OR)", type = "coef", display = "coef", digits = 2L, ref = NA_character_,   ci_type = "diff",  color = "diff", link = "logit")),
  # Last Phase z3 -- the modified-Poisson (binary outcome) crude companion. SAME base column as binomial
  # (a risk, `Obs_%`, with the Wald risk-difference CI), but the effect is a crude RISK ratio with the
  # KATZ log-RR interval (ci_katz_rr) -- not the Woolf log-OR the binomial arm uses. That is the point
  # of the whole feature: the observed companion must be on the same scale as the model column.
  rr = list(
    method_diff = "wald", coef = "rr", coef_log = "rr_log",
    base   = list(nm = "Obs_%",       type = "row",  display = "pct",  digits = 0L, ref = "tot",           ci_type = "diff",  color = "diff", link = NA_character_),
    ame    = list(nm = "Obs_diff",    type = "row",  display = "diff", digits = 0L, ref = "tot",           ci_type = "diff",  color = "diff", link = "identity"),
    rr     = list(nm = "Obs_RR",      type = "row",  display = "or",   digits = 2L, ref = "1",             ci_type = "or",    color = "OR",   link = "log"),
    rr_log = list(nm = "Obs_log(RR)", type = "coef", display = "coef", digits = 2L, ref = NA_character_,   ci_type = "diff",  color = "diff", link = "log")),
  gaussian = list(
    method_mean_diff = "student", coef = "diff", coef_log = "diff",
    base = list(nm = "Obs_mean", type = "mean", display = "mean", digits = 2L, ref = NA_character_,  ci_type = "cell",  color = "",     link = NA_character_),
    diff = list(nm = "Obs_diff", type = "coef", display = "coef", digits = 2L, ref = NA_character_,  ci_type = "diff",  color = "diff", link = "identity")),
  poisson = list(
    method_mean_ratio = "quasipoisson", coef = "irr", coef_log = "irr_log",
    base    = list(nm = "Obs_rate",     type = "mean", display = "mean", digits = 2L, ref = "1",           ci_type = "ratio", color = "ratio", link = NA_character_),
    irr     = list(nm = "Obs_IRR",      type = "row",  display = "or",   digits = 2L, ref = "1",           ci_type = "or",    color = "OR",    link = "log"),
    irr_log = list(nm = "Obs_log(IRR)", type = "coef", display = "coef", digits = 2L, ref = NA_character_, ci_type = "diff",  color = "diff",  link = "log")),
  # Last Phase z10 -- the three families that had no crude twin at all.
  #
  # grouped_binomial (`trials =`): the univariable model is STILL saturated for a factor predictor, so
  # the crude OR is the existing Woolf 2x2 on the SUMMED counts (measured identical to a univariable glm
  # to 1.1e-8). Its BASE column is the mean SCORE (maintainer's ruling) -- a per-RESPONDENT quantity, so
  # it takes the gaussian base shape and reads `emp_mean`, while the effect reads the summed 2x2. That
  # one family needing both grid parts at once is why `emp_base` had to split into emp_prop / emp_mean.
  grouped_binomial = list(
    method_diff = "wald", method_mean_diff = "student", coef = "or", coef_log = "or_log",
    base   = list(nm = "Obs_mean",     type = "mean", display = "mean", digits = 2L, ref = NA_character_, ci_type = "cell",  color = "",     link = NA_character_),
    ame    = list(nm = "Obs_diff",     type = "row",  display = "diff", digits = 0L, ref = "tot",         ci_type = "diff",  color = "diff", link = "identity"),
    or     = list(nm = "Obs_OR",       type = "row",  display = "or",   digits = 2L, ref = "1",           ci_type = "or",    color = "OR",   link = "logit"),
    or_log = list(nm = "Obs_log(OR)",  type = "coef", display = "coef", digits = 2L, ref = NA_character_, ci_type = "diff",  color = "diff", link = "logit")),
  # multinomial: one crude column PER OUTCOME CATEGORY would double an already wide table, so these
  # shapes are `visible = FALSE` -- the crude number rides IN-CELL in the model column's `obs` field
  # (maintainer's ruling Q4, rendered as "{or} ({obs})" / "{diff} ({obs})"). `obs` is defined as "the
  # value this cell is compared to, ON THE CELL'S OWN SCALE", so an invisible shape still has to declare
  # its ci_type and link exactly like a visible one. The crude effect is closed-form: the univariable
  # multinomial is saturated, and its OR is the {j, ref} x {level, ref level} Woolf ratio -- the very
  # number tab(pct = "row", OR = "OR") prints.
  multinomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    or        = list(nm = NA_character_, type = "row", display = "or",   digits = 2L, ref = "1",   ci_type = "or",   color = "OR",   link = "logit", visible = FALSE, per_category = TRUE),
    or_log    = list(nm = NA_character_, type = "coef", display = "coef", digits = 2L, ref = NA_character_, ci_type = "diff", color = "diff", link = "logit", visible = FALSE, per_category = TRUE),
    ame       = list(nm = NA_character_, type = "row", display = "diff", digits = 0L, ref = "tot", ci_type = "diff", color = "diff", link = "identity", visible = FALSE, per_category = TRUE),
    ame_ratio = list(nm = NA_character_, type = "row", display = "or",   digits = 2L, ref = "1",   ci_type = "or",   color = "OR",   link = "log",   visible = FALSE, per_category = TRUE)),
  # ordinal: proportional odds is a CONSTRAINT, so the univariable model is NOT saturated and there is no
  # closed form (measured: the three closed-form substitutes drift by 2.4-5.4 %, of the same order as the
  # first colour break -- and the drift IS the PO violation, so it would inject a data-dependent offset
  # into a measure whose whole job is to say how far the model moved the effect). Hence `from = "fit"`:
  # a univariable polr / svyolr through reg_fit(), the same escape z9 took for numeric predictors and for
  # the same reason -- ruling Q6 (same estimand, link, CI rule, multiplier) holds by construction.
  ordinal = list(
    coef = "cumor", coef_log = "cumor_log",
    cumor     = list(nm = "Obs_cumOR",      type = "row",  display = "or",   digits = 2L, ref = "1",           ci_type = "or",   color = "OR",   link = "logit", from = "fit"),
    cumor_log = list(nm = "Obs_log(cumOR)", type = "coef", display = "coef", digits = 2L, ref = NA_character_, ci_type = "diff", color = "diff", link = "logit", from = "fit"),
    ame       = list(nm = NA_character_, type = "row", display = "diff", digits = 0L, ref = "tot", ci_type = "diff", color = "diff", link = "identity", visible = FALSE, per_category = TRUE, from = "fit"),
    ame_ratio = list(nm = NA_character_, type = "row", display = "or",   digits = 2L, ref = "1",   ci_type = "or",   color = "OR",   link = "log",   visible = FALSE, per_category = TRUE, from = "fit"))
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
#' @keywords internal
shape_from_fit     <- function(shape) identical(shape$from, "fit")

# reg_crude_shape() -- WHICH REG_EMPIRICAL row describes the crude EFFECT of this (crude_key, effect,
# do_exp)? The ONE selection rule, read by reg_empirical_columns()'s arms (which build the column) and
# by reg_model_note() (which words the footer for it) -- two consumers, one fact, per Phase 17 rule 5.
# `coef` / `coef_log` on each family name its coefficient-scale row and that row's logged twin; a family
# declares `ame` / `ame_ratio` rows only where a MARGINAL crude exists (a gaussian AME IS its
# coefficient, and a poisson AME is additive while its crude stays a rate RATIO, which
# reg_same_estimand() then refuses -- so those two families fall through to the coefficient row exactly
# as they always did). The binary families borrow REG_EMPIRICAL$rr$rr for "ame_ratio": a marginal risk
# ratio is the same crude quantity whichever way the model was fitted.
#' @keywords internal
reg_crude_shape <- function(crude_key, effect = "coefficient", do_exp = TRUE) {
  fam <- if (is.null(crude_key) || is.na(crude_key)) NULL else REG_EMPIRICAL[[crude_key]]
  if (is.null(fam)) return(NULL)
  if (effect %in% c("ame", "ame_ratio")) {
    if (!is.null(fam[[effect]]))                     return(fam[[effect]])
    if (identical(effect, "ame_ratio") && !is.null(fam$ame)) return(REG_EMPIRICAL$rr$rr)
  }
  fam[[if (isTRUE(do_exp)) fam$coef else fam$coef_log]]
}

# Does the crude effect ride IN-CELL (as `obs`) instead of drawing its own Obs_* column? One stored
# consequence of the shape, read by the footer wording and by set_obs_if()'s display fold.
#' @keywords internal
reg_crude_in_cell <- function(crude_key, effect = "coefficient", do_exp = TRUE) {
  sh <- reg_crude_shape(crude_key, effect, do_exp)
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

# The base+effect fmt columns aligned to the skeleton, for reg_build to prepend before the model column.
# The Constant -> empty cells; reference levels -> neutral + in_refrow, no CI. want_p is TRUE (the pvalue
# is stored; stars are stripped post-build when stars = FALSE, like the model columns).
#
# Last Phase z10 -- three structural changes, all driven by shape FACTS rather than by family names:
#   * emit() replaces two(): a shape set may draw TWO columns (base + effect, every pre-z10 family), ONE
#     (ordinal: a cumulative OR has no base -- there is no single share to show beside it), or ZERO
#     (multinomial: the crude number rides in-cell via `obs`). The old two() could only ever do two.
#   * the crude EFFECT is returned as a list keyed by OUTCOME CATEGORY ("" when the outcome has none),
#     because a multinomial / ordinal-marginal model has one column per category and each needs its own
#     `obs`. reg_build looks the column's stored `emp_key` up in it.
#   * `fit_est` (reg_empirical_fit()'s per-category estimates) fills the rows no closed form covers --
#     numeric predictors in any family (z9), and EVERY predictor under an ordinal outcome (z10).
reg_empirical_columns <- function(skeleton, emp, fac_preds, crude_key, family, effect, var_y,
                                  conf_level = 0.95, color_signif = "grey_non_signif",
                                  color = NULL, do_exp = TRUE, fit_est = NULL) {
  fam <- REG_EMPIRICAL[[crude_key]]
  if (is.null(fam)) return(list(cols = list(), effect = NULL, shape = NULL))
  # Phase 15d: when the model is uncoloured (`color = FALSE` -> "no"), the crude companions must be
  # uncoloured too (else the table shows coloured empirical columns beside plain model columns).
  # `color[1]`: the measure may be a length-2 (text, background) vector since Last Phase z5's
  # `color = c("OR", "adjustment")` -- `color %in% ...` would then return length 2 and the `if` below
  # would error. Only the text channel decides whether the crude companions are drawn at all.
  emp_off <- !is.null(color) && color[1] %in% c("no", "")
  n_rows  <- nrow(skeleton)
  is_fac  <- skeleton$var %in% fac_preds
  # Last Phase z9 (dev/numeric_predictors_crude_counterparts.md SS11.1): the Constant is a reference row
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
  emp_col <- function(shape, fields) {
    measure <- if (emp_off || !nzchar(shape$color)) "" else shape$color
    args <- c(fields, list(
      type = shape$type, display = shape$display, digits = shape$digits, ci_type = shape$ci_type,
      color = measure, color_signif = if (nzchar(measure)) color_signif else "ignore",
      col_var = shape$nm, comp_all = FALSE, in_refrow = refrows, model_family = family, role = "emp"))
    if (!is.na(shape$ref)) args$ref <- shape$ref
    do.call(fmt, args)
  }
  # Last Phase z5: besides the columns, return the crude EFFECT vector -- the very value the effect
  # column stores in its own estimate field, so it is already on the model column's scale (an OR beside
  # an OR, log(OR) beside a raw coefficient, a risk difference beside an AME). reg_build writes it into
  # the model columns' `obs` field, which backs `color = "adjustment"` and the `{obs}` display token.
  # Taken from the local the shape was built from -- never re-read out of the fmt column by name.
  # Last Phase z8-B: the effect SHAPE ROW travels with it, giving the gap test both facts it needs --
  # `link` (the crude estimator's link) and `ci_type` (proof that the crude and model columns are the
  # SAME estimand) -- and any future shape fact for free, with no new element to thread.
  # Last Phase z9/z10: the fit-derived rows are spliced HERE -- the one place the effect shape is known,
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
    if (effect %in% c("ame", "ame_ratio")) {
      sh   <- reg_crude_shape(crude_key, effect, do_exp)
      cats <- names(fit_est$est)
      if (!length(cats)) return(list(cols = list(), effect = NULL, shape = sh))
      out  <- purrr::map(stats::setNames(nm = cats), function(k)
        emit(NULL, list(col = emp_col(sh, list(diff = na_v(), n = rep(NA_integer_, n_rows))),
                        vec = na_v(), shape = sh), k))
      return(list(cols = list(), shape = sh,
                  effect = purrr::flatten(purrr::map(out, "effect"))))
    }
    sh  <- reg_crude_shape(crude_key, "coefficient", do_exp)
    fld <- if (do_exp) list(or = na_v()) else list(diff = na_v())
    return(emit(NULL, list(col = emp_col(sh, c(fld, list(n = rep(NA_integer_, n_rows)))),
                           vec = na_v(), shape = sh)))
  }

  # ---- multinomial: closed form, one crude effect per outcome category, no visible column ------------
  if (identical(crude_key, "multinomial")) {
    sh   <- reg_crude_shape(crude_key, effect, do_exp)
    cats <- unique(emp$category)
    if (!length(cats)) return(list(cols = list(), effect = NULL, shape = sh))
    out <- purrr::map(stats::setNames(nm = cats), function(k) {
      g <- cat_of(k)
      v <- switch(effect,
                  "ame"       = g$emp_diff,
                  "ame_ratio" = g$emp_ratio_prop,
                  if (do_exp) g$emp_ratio else log(g$emp_ratio))
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
  # Last Phase s: the CI base is the effective n (Kish n_eff, opt-in) -- off-kish it equals the raw
  # count, so the intervals are byte-identical. The displayed n/tot_n fields keep the raw count `nv`.
  nv_ci <- g$emp_n_ci; rn_ci <- g$emp_ref_n_ci
  # the CI base of a PROPORTION is the number of Bernoulli DRAWS (n x trials for a grouped binomial,
  # n everywhere else -> byte-identical); the MEAN CIs keep the per-respondent n_ci.
  nv_dr <- g$emp_n_draw; rn_dr <- g$emp_ref_n_draw

  # binomial + "rr" (modified Poisson) share every BASE fact -- a crude risk and its Wald risk-difference
  # CI -- and differ only in the crude EFFECT, which must be the model's own estimand (Last Phase z3).
  # Last Phase z10: grouped_binomial shares the EFFECT facts (a Woolf OR on the summed 2x2 legs) but not
  # the base -- its base column is the mean SCORE, built below like the gaussian one.
  binary_like <- reg_fam_binary(crude_key) || identical(crude_key, "grouped_binomial")
  if (binary_like) {
    grouped <- identical(crude_key, "grouped_binomial")
    rd <- na_ref(ci_prop_diff(prop, nv_dr, rprop, rn_dr, conf_level = conf_level, # crude risk-difference
                              method = fam$method_diff, want_p = TRUE))
    rd_fields <- list(pct = prop, diff = diffv, n = nv, tot_n = nv,
                      ci_inf = rd$inf, ci_sup = rd$sup, pvalue = rd$pvalue)
    base <- if (grouped) {
      # the mean SCORE and its one-sample t interval (the gaussian base shape, on the numeric part)
      cell <- ci_pivot(meanv, sqrt(varv / nv_ci), df = nv_ci - 1, conf_level = conf_level,
                       want_p = FALSE)
      list(col = emp_col(fam$base, list(mean = meanv, var = varv, n = nv, tot_n = nv,
                                        ci_inf = cell$inf, ci_sup = cell$sup)), shape = fam$base)
    } else list(col = emp_col(fam$base, rd_fields), shape = fam$base)

    if (effect == "ame") {             # the AME shows a difference, not an OR -> crude risk-difference
      sh <- reg_crude_shape(crude_key, "ame", do_exp)
      return(emit(base, list(col = emp_col(sh, rd_fields), vec = diffv, shape = sh), cat1))
    }
    # Last Phase z3: a marginal RATIO's crude twin is the crude RISK ratio with the Katz log-RR interval
    # -- on the binomial model path as well as the "rr" one, since the estimand is what must match, not
    # the fitted family. Always exponentiated: `exponentiate` is ignored for marginal effects. The Obs_RR
    # shape is defined once, in REG_EMPIRICAL$rr, and reused here rather than duplicated per family.
    if (effect == "ame_ratio") {
      rr_ci <- na_ref(ci_katz_rr(prop, nv_dr, rprop, rn_dr, conf_level = conf_level, want_p = TRUE))
      sh    <- reg_crude_shape(crude_key, "ame_ratio", do_exp)
      return(emit(base, list(col = emp_col(sh, list(or = prop / rprop, n = nv, ci_inf = rr_ci$inf,
                                                    ci_sup = rr_ci$sup, pvalue = rr_ci$pvalue)),
                             vec = prop / rprop, shape = sh), cat1))
    }
    # binomial / grouped -> the crude ODDS ratio (the 2x2 legs vs the reference level's) with the Woolf
    # log-OR interval. "rr" -> the crude RISK ratio (prop/rprop) with the Katz log-RR interval. WARNING:
    # `ratio` (emp_ratio) is an ODDS ratio -- feeding it to an Obs_RR column would print an OR under an
    # RR header. Last Phase z10: the 2x2 legs come from the grid (emp_wpos / emp_wneg) instead of being
    # rebuilt as prop * n -- for a grouped binomial the base is Sum(w * trials), not the respondent
    # count, and only the legs know that.
    is_rr  <- identical(crude_key, "rr")
    eff_v  <- if (is_rr) prop / rprop else ratio
    eff_ci <- na_ref(if (is_rr)
      ci_katz_rr(prop, nv_dr, rprop, rn_dr, conf_level = conf_level, want_p = TRUE)
    else
      # the SS14 rule, unchanged: WEIGHTED proportion x UNWEIGHTED base, so the base cancels out of the
      # log-OR. For a grouped binomial that base counts DRAWS (n x trials), which is what makes the crude
      # OR equal a univariable glm(cbind(s, q - s) ~ x) rather than an OR on respondent counts.
      ci_or(prop * nv_dr, (1 - prop) * nv_dr,
            rprop * rn_dr, (1 - rprop) * rn_dr, conf_level = conf_level, want_p = TRUE))
    sh_exp <- reg_crude_shape(crude_key, "coefficient", TRUE)
    sh_log <- reg_crude_shape(crude_key, "coefficient", FALSE)
    if (do_exp) {
      eff_col <- emp_col(sh_exp, list(or = eff_v, n = nv, ci_inf = eff_ci$inf,
                                      ci_sup = eff_ci$sup, pvalue = eff_ci$pvalue))
      return(emit(base, list(col = eff_col, vec = eff_v, shape = sh_exp), cat1))
    }
    # Phase g: exponentiate = FALSE -> the crude companion is the LOGGED effect (Obs_log(OR) /
    # Obs_log(RR)): the log ratio in the `diff` field with the logged CI, i.e. the exact Wald interval
    # on the log scale -- the same link scale as the raw model coefficient.
    eff_col <- emp_col(sh_log, list(diff = log(eff_v), n = nv, ci_inf = log(eff_ci$inf),
                                    ci_sup = log(eff_ci$sup), pvalue = eff_ci$pvalue))
    return(emit(base, list(col = eff_col, vec = log(eff_v), shape = sh_log), cat1))
  }

  if (identical(crude_key, "gaussian")) {
    cell <- ci_pivot(meanv, sqrt(varv / nv_ci), df = nv_ci - 1, conf_level = conf_level, want_p = FALSE)
    base_col <- emp_col(fam$base, list(mean = meanv, var = varv, n = nv, tot_n = nv,
                                       ci_inf = cell$inf, ci_sup = cell$sup))
    md <- na_ref(ci_mean_diff2(meanv, varv, nv_ci, rmean, rv, rn_ci, method = fam$method_mean_diff, # pooled t = OLS
                               conf_level = conf_level, want_p = TRUE))
    eff_col <- emp_col(fam$diff, list(diff = diffv, var = rep(var_y, n_rows), n = nv,
                                      ci_inf = md$inf, ci_sup = md$sup, pvalue = md$pvalue))
    return(emit(list(col = base_col, shape = fam$base),
                list(col = eff_col, vec = diffv,
                     shape = reg_crude_shape(crude_key, effect, do_exp)), cat1))
  }

  if (identical(crude_key, "poisson")) {
    # one crude rate-ratio CI (quasi-Poisson, = the phi-scaled model's method) drives BOTH columns.
    rr <- na_ref(ci_mean_ratio(meanv, varv, nv_ci, rmean, rv, rn_ci, method = fam$method_mean_ratio,
                               conf_level = conf_level, want_p = TRUE))
    base_col <- emp_col(fam$base, list(mean = meanv, ratio = ratio, n = nv, tot_n = nv,
                                       ci_inf = rr$inf, ci_sup = rr$sup, pvalue = rr$pvalue))
    if (do_exp) {
      eff_col <- emp_col(fam$irr, list(or = ratio, n = nv, ci_inf = rr$inf,
                                       ci_sup = rr$sup, pvalue = rr$pvalue))
      return(emit(list(col = base_col, shape = fam$base),
                  list(col = eff_col, vec = ratio, shape = fam$irr), cat1))
    }
    # Phase g: exponentiate = FALSE -> the crude companion is Obs_log(IRR): log(rate-ratio) in `diff`
    # with the logged rate-ratio CI (the same link scale as the raw Poisson coefficient).
    eff_col <- emp_col(fam$irr_log, list(diff = log(ratio), n = nv, ci_inf = log(rr$inf),
                                         ci_sup = log(rr$sup), pvalue = rr$pvalue))
    return(emit(list(col = base_col, shape = fam$base),
                list(col = eff_col, vec = log(ratio), shape = fam$irr_log), cat1))
  }

  list(cols = list(), effect = NULL, shape = NULL)
}

# === the model-vs-observed GAP standard error (Last Phase z8-B) =====================================

# reg_same_estimand() -- do the crude companion and the model column measure the SAME thing? Both
# declare their scale as a `ci_type` (the shape row's, the column attribute's), so this is one fact
# comparison rather than a family/effect table kept in sync. It gates BOTH `obs` and its gap SE, which
# closes a z5 defect: reg_empirical_columns() ignores `effect` on the poisson branch, so
# effect = "ame" paired an ADDITIVE count AME ("diff") with the crude rate RATIO ("or") and z5 scored
# the difference of two scales. Checked against all nine live family x effect combinations: it fires on
# that one and on nothing else.
#' @keywords internal
reg_same_estimand <- function(shape, col)
  !is.null(shape) &&
  identical(as.character(shape$ci_type)[1], as.character(get_ci_type(col))[1])

# reg_gap_se_columns() -- the standard error of the gap between ONE fit's effect and its observed
# (crude) counterpart, per skeleton row, so `color = "adjustment"` reads `color_signif` like every other
# measure. The maths lives in R/reg-influence.R; this is the gate and the loop.
#
# DESIGN -- the gate is six facts, each already stored somewhere, and it returns NULL rather than a
# partial column: a gap SE without an honest premise is worse than none, because MEASURES' force_policy
# closure reads an all-NA `gap_se` as "no test here" and falls back to the descriptive reading.
#   * `sp$color`          nobody reads it otherwise, and it costs ~1/8 of a fit (SS8).
#   * `shape`             the crude twin's REG_EMPIRICAL row: absent = no observed effect at all
#                         (multinomial, ordinal, grouped binomial) -> `obs` is already NA.
#   * `f$fit`             NULL on the jamovi digest path, where the fitted object was distilled away.
#   * ci_type match       the crude and model columns must be the SAME estimand. This also closes a z5
#                         defect: reg_empirical_columns() ignores `effect` on the poisson branch, so
#                         effect = "ame" pairs an ADDITIVE count AME with a MULTIPLICATIVE crude rate
#                         ratio -- z5 wrote that ratio into `obs` and scored a gap between two scales.
#   * nrow match          both estimators must solve their equations on the SAME observations. The
#                         crude frame drops on `union_predictors`, the model on its own; the first is a
#                         SUBSET of the second, so equal row counts PROVE row identity (and both come
#                         from reg_complete_frame()'s drop_na, which preserves order).
#   * collapsible         maintainer ruling Q1(b): a conditional odds ratio moves under adjustment even
#                         with zero confounding, so at survey sizes the test would be "significant"
#                         everywhere for a reason no reader would take it for (SS4.1-SS4.3).
# `method = "profile"` is deliberately NOT a clause: between_groups RECOVERS its SE from the printed
# interval and a profile bracket is not est +/- crit*se, but adjustment COMPUTES its own -- profile
# there only means the printed model CI and the gap test are different quantities (SS3.8, documented).
#' @keywords internal
reg_gap_se_columns <- function(f, sp, model_col, skeleton, shape, mdata, fac_preds,
                               effect, at, wt, fits_crude = NULL, fit_preds = character(0),
                               multiplier = NULL, category = "") {
  if (!"adjustment" %in% sp$color)                              return(NULL)
  if (is.null(shape) || is.null(f$fit) || is.null(f$data))      return(NULL)
  if (isTRUE(sp$compound) || identical(at, "reference"))        return(NULL)
  if (!reg_same_estimand(shape, model_col))                     return(NULL)
  if (!identical(nrow(mdata), nrow(f$data)))                    return(NULL)
  if (!reg_estimand_collapsible(sp$family, effect))             return(NULL)
  # svyrecvar is the linearization estimator; a REPLICATE-weights design needs withReplicates instead,
  # so degrade rather than report a linearization variance for a design that did not ask for one.
  des <- if (inherits(f$fit, "svyglm")) f$fit$survey.design else NULL
  if (inherits(des, "svyrep.design"))                           return(NULL)

  coef_if <- reg_coef_if_maker(f$fit)
  if (is.null(coef_if)) return(NULL)
  marginal <- effect %in% c("ame", "ame_ratio")
  # Last Phase z10: a 3+ level outcome shows ONE COLUMN PER CATEGORY, so its marginal influence function
  # is per category too (reg_ame_if_cat_maker); the single-equation one reads family()$mu.eta, which
  # multinom / polr do not have.
  per_cat  <- inherits(f$fit, "multinom") || inherits(f$fit, "polr")
  model_if <- if (marginal && per_cat)
    reg_ame_if_cat_maker(f$fit, f$data, wt, ratio = identical(effect, "ame_ratio"),
                         category = category)
  else if (marginal)
    reg_ame_if_maker(f$fit, f$data, wt, ratio = identical(effect, "ame_ratio"), coef_if = coef_if)
  else coef_if
  # Last Phase z10: `category` is the outcome category THIS column shows (a multinomial / ordinal-marginal
  # fit owns one per category; "" elsewhere). The crude leg must be built around the SAME indicator the
  # crude estimate was -- reg_crude_yw() is the one description of that, so the closed form takes the
  # category rather than re-deriving a positive level.
  crude_if <- reg_crude_if_maker(mdata, sp$dependent, sp$crude_key, f$positive_level, wt, shape$link,
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
    ic <- crude_if(v, as.character(skeleton$level[k]), r)
    if (is.null(ic) || length(ic) != length(im)) next
    out[k] <- reg_if_se(im - ic, des)
  }

  # Last Phase z9 -- the NUMERIC arm. Same two legs, different crude side: a numeric predictor has no
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
      # Last Phase z10: the fit arm now covers FACTOR predictors too (every predictor under an ordinal
      # outcome). A factor's contrast is (level, reference level), a numeric's a k-unit forward
      # difference -- the same two shapes reg_ame_if_maker()'s own contract states.
      is_fac_k <- v %in% fac_preds
      cl <- if (is_fac_k) list(as.character(skeleton$level[k]), ref_of(v)) else list(kk, 0)
      if (is_fac_k && is.na(cl[[2]])) next
      if (marginal) {
        im <- model_if(v, cl[[1]], cl[[2]])
        ic <- if (inherits(nv$fit, "multinom") || inherits(nv$fit, "polr"))
          reg_ame_if_cat_maker(nv$fit, nv$data, wt, ratio = identical(effect, "ame_ratio"),
                               category = category)
        else
          reg_ame_if_maker(nv$fit, nv$data, wt, ratio = identical(effect, "ame_ratio"),
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

# Last Phase z10: reg_empirical_tips() is DELETED. It was reg_empirical() at a three-part key --
# measured bit-identical on the shared quantity -- so the merged (var, level, category) grid is now the
# single producer, read directly by reg_build's tooltip block.
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
# Last Phase z3: `comparison = "lnratioavg"` is the RATIO twin of the default additive contrast -- the
# log of the ratio of adjusted predicted probabilities (marginal standardization / g-computation), exp()'d
# here into a risk ratio. It shares the whole multiplicative path with "lnor": same double-paren label
# shape, same exp() of the estimate and BOTH bounds (so the interval stays a Wald interval on the log
# scale, asymmetric and strictly positive once exponentiated).
reg_marginal <- function(fit, data, predictors, conf_level, wt = NULL,
                         at = "average", comparison = NULL, want_pred = TRUE,
                         multiplier = NULL) {
  ref_vals <- if (at == "reference") reg_reference_grid_values(data, predictors) else NULL
  ref_grid <- if (at == "reference")
    do.call(marginaleffects::datagrid, c(list(model = fit), ref_vals)) else NULL
  # weights only at the AVERAGING step; a single-row profile takes none. `wts = NULL` is rejected by
  # marginaleffects (default FALSE), so the arg is omitted when unweighted or at a profile.
  wts_arg <- if (at == "reference" || is.null(wt)) list() else list(wts = wt)
  cmp_arg <- if (is.null(comparison)) list() else list(comparison = comparison)
  # WARNING: `comparison` is NULL on the additive default, and `NULL %in% x` is logical(0), not FALSE --
  # which would make every `if (do_exp)` below error with "argument is of length zero".
  do_exp  <- !is.null(comparison) && comparison %in% c("lnor", "lnratioavg")

  # Last Phase z9: `multiplier` reaches the MARGINAL path too. Before, a scaled numeric predictor kept a
  # per-1-unit AME while its row label already read "(per 10)" -- a live mislabel, and one that a
  # non-unit default would make universal. `variables = list(v = k)` is a k-unit FORWARD DIFFERENCE, not
  # k x the 1-unit AME (measured: 0.020322 vs 0.020297 for a 10-year contrast) -- the honest quantity for
  # a nonlinear model, and the one whose adjusted-% companion stays coherent. The keyword is NEVER passed
  # through: marginaleffects' own "sd" is a CENTRED contrast on the SD of its `newdata`, i.e. a per-group
  # SD, which is exactly what freezing the multiplier upstream exists to prevent.
  var_arg <- function(v) {
    k <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else NA_real_
    if (is.finite(k) && k != 1 && !reg_is_factor_var(data[[v]])) stats::setNames(list(k), v) else v
  }
  amelist <- purrr::map(predictors, function(v) {
    ac <- if (at == "reference")
      as.data.frame(do.call(marginaleffects::comparisons, c(
        list(fit, variables = var_arg(v), newdata = ref_grid, conf_level = conf_level), cmp_arg)))
    else
      as.data.frame(do.call(marginaleffects::avg_comparisons, c(
        list(fit, variables = var_arg(v), newdata = data, conf_level = conf_level), wts_arg, cmp_arg)))
    is_fac <- reg_is_factor_var(data[[v]])
    # The factor contrast label is "<Level> - <Reference>" (difference) or
    # "ln(odds(<Level>) / odds(<Reference>))" (comparison = "lnor"). Phase 14r: strip the KNOWN prefix +
    # reference suffix instead of splitting on the FIRST " - " / first ")" -- a Level that itself
    # contains " - " (e.g. "$20000 - 24999") or ")" was otherwise truncated and failed to key the AME to
    # the skeleton, leaving an NA cell. The reference is the factor's first level (after de-ordering in
    # reg_fit). A numeric predictor keys on the variable name.
    ref_lv <- if (is_fac) levels(forcats::fct_drop(as.factor(data[[v]])))[1] else NA_character_
    level  <- if (!is_fac) v else {
      # "lnor"       -> "ln(odds(<Level>) / odds(<Ref>))"
      # "lnratioavg"  -> "ln(mean(<Level>) / mean(<Ref>))"   -- same shape, DOUBLE closing paren.
      inner <- if (identical(comparison, "lnor")) "odds" else "mean"
      pre <- if (do_exp) paste0("ln(", inner, "(") else ""
      suf <- if (do_exp) paste0(") / ", inner, "(", ref_lv, "))") else paste0(" - ", ref_lv)
      substr(ac$contrast, nchar(pre) + 1L, nchar(ac$contrast) - nchar(suf))
    }
    grp    <- if ("group" %in% names(ac)) as.character(ac$group) else NA_character_
    est <- ac$estimate; lo <- ac$conf.low; hi <- ac$conf.high
    if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }   # log-ratio -> OR / RR (and its CI)
    tibble::tibble(var = v, level = as.character(level), group = grp,
                   ame = est, ame_lo = lo, ame_hi = hi, ame_p = ac$p.value)
  })
  ame <- dplyr::bind_rows(amelist)

  predlist <- if (want_pred) purrr::map(predictors, function(v) {
    if (!reg_is_factor_var(data[[v]])) return(NULL)      # no per-level prediction for numerics
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
      # adjusted. `by = v` would instead reproduce the estimation-sample OBSERVED rate.
      as.data.frame(do.call(marginaleffects::avg_predictions, c(
        list(fit, variables = v, newdata = data, conf_level = conf_level), wts_arg)))
    }
    grp <- if ("group" %in% names(ap)) as.character(ap$group) else NA_character_
    tibble::tibble(var = v, level = as.character(ap[[v]]), group = grp, pred = ap$estimate)
  }) else list()
  pred <- dplyr::bind_rows(purrr::compact(predlist))

  list(ame = ame, pred = pred)
}

# Build ONE fmt column from a reg_marginal() result, for a given outcome `group` (NA for a single-outcome
# glm/lm/poisson). Aligns the effect (+ adjusted prediction) to the shared (var, level) skeleton. `shape`
# picks the fmt shape: "prob" (probability-scale AME) composes AME-first "{diff} ({pct})" (reference
# level -> "({pct})", numeric -> "diff"); "raw" (gaussian/poisson) a plain "coef"; "or" (MNL j-vs-rest
# OR at the profile) the multiplicative "or" shape (reference -> 1, no prediction). Reference levels +
# the Constant carry no effect; predictors ABSENT from this model stay NA (empty cells).
reg_marginal_column <- function(skeleton, marg, model_predictors, numeric_preds, shape, var_y,
                                nobs, group, color, color_signif, col_var, or_tip = NULL,
                                model_family = "") {
  amt <- marg$ame; prd <- marg$pred
  if (!is.na(group)) {
    amt <- amt[!is.na(amt$group) & amt$group == group, , drop = FALSE]
    if (nrow(prd)) prd <- prd[!is.na(prd$group) & prd$group == group, , drop = FALSE]
  }
  m     <- reg_skel_match(skeleton, amt)
  ame_v <- amt$ame[m]; lo_v <- amt$ame_lo[m]; hi_v <- amt$ame_hi[m]; p_v <- amt$ame_p[m]
  # guard the $pred access: an empty `prd` (numeric-only preds / want_pred = FALSE) has no `pred` column.
  pred_v <- if (nrow(prd)) prd$pred[reg_skel_match(skeleton, prd)] else rep(NA_real_, nrow(skeleton))

  n_rows   <- nrow(skeleton)
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_const <- skeleton$var == "Constant"
  is_ref   <- skeleton$is_ref & !is_const & in_model
  is_num   <- skeleton$var %in% numeric_preds & in_model
  # in_refrow: the UNION-skeleton row fact (see reg_column) so an absent predictor keeps its bold in a
  # comparison; is_ref above stays in_model-gated for the value/display blanking below.
  refrows  <- (skeleton$is_ref & !is_const) | is_const

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
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  } else if (shape == "prob_ratio") {
    # Last Phase z3: the RATIO twin of "prob" -- a marginal RISK RATIO with the adjusted predicted
    # probability in parentheses. The composite is coherent BY CONSTRUCTION: marginal standardization
    # gives adjusted%(ref) * RR(level) == adjusted%(level) exactly, the multiplicative mirror of the
    # additive identity "prob" relies on. WARNING: the reference cell keeps the FULL "{or} ({pct})"
    # template with or = 1, not "({pct})" -- a template that does not mention `or` leaves the tooltip
    # gate (fmt_display_shows, tab_classes.R) free to attach a stray "OR: 1.00" hover to a risk-ratio
    # column. Still load-bearing after z10 replaced display_primary() there: the gate now reads the
    # WHOLE template instead of its first token, which is a different bug, not this one.
    compos <- in_model & !is_const & !is_ref & !is.na(ame_v) & !is.na(pred_v)
    display[compos]                            <- "{or} ({pct})"
    display[in_model & is_ref & !is.na(pred_v)] <- "{or} ({pct})"
    display[in_model & is_num & !is.na(ame_v)]  <- "or"        # numeric predictor: bare RR
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): no misleading whole-model N (see the empirical cols)
      pct = pred_v, or = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      type = "row", display = display, digits = 1L, ref = "1", ci_type = "or",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
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
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
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
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  }
}

# Split ONE multinomial fit into one OR column per non-reference outcome category. Each category's
# tidy rows (`y.level == j`, y.level dropped) look like a standard glm tidy, so reg_column() aligns
# them to the shared predictor skeleton unchanged. Label = "<j> vs <ref>: OR" (prefixed by the
# dependent when several dependents / models coexist, to disambiguate). Returns a list of {label, col}.
reg_columns_multinom <- function(skeleton, f, sp, effect_shape, color, color_signif,
                                 eff_word, cleannames, prefix_dep, model_family = "multinomial") {
  y_ref <- reg_cleanup(f$y_ref, cleannames)
  purrr::map(f$y_levels, function(j) {
    sub      <- f
    sub$tidy <- f$tidy[f$tidy$y.level == j,
                       setdiff(names(f$tidy), "y.level"), drop = FALSE]
    jc  <- reg_cleanup(j, cleannames)
    lab <- paste0(if (prefix_dep) paste0(sp$dependent, " - ") else "",
                  jc, " vs ", y_ref)
    # Phase 14s (G) + 14w (item 3): every category column of ONE model shares `sp$label` ("<dep>: OR")
    # as its col_var, so no border is drawn between them (borders separate DIFFERENT col_vars) and the
    # model name + effect span them once. The repeated ": OR" is stripped from the per-category NAME.
    list(label = lab, emp_key = j,   # emp_key: raw category, for the empirical tooltip (Phase 14v)
         col   = reg_column(skeleton, sub, sp$predictors, sp$label, effect_shape, color, color_signif,
                            model_family = model_family))
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

  # Last Phase z3: a modified-Poisson ("rr") fit is a QUASI-likelihood on a deliberately misspecified
  # variance, so AIC / BIC / McFadden are not defined, and the Pearson dispersion of a 0/1 outcome is
  # just mean(1-mu) -- a constant of the fitted values, never a diagnostic. Report the honest pair:
  # n + the design-based Wald-vs-null. Placed FIRST so it holds weighted or not (the fit is an svyglm
  # either way); the weighted branch below keeps its Nagelkerke/AIC set for genuine survey models.
  if (family == "rr") {
    terms_all <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
    wt <- if (length(terms_all) > 0)
      tryCatch(suppressWarnings(survey::regTermTest(fit, stats::reformulate(terms_all))),
               error = function(e) NULL)
    else NULL
    if (!is.null(wt)) out <- dplyr::bind_rows(out, row("wald_null",
      statistic = as.numeric(wt$Ftest), df1 = as.numeric(wt$df), df2 = as.numeric(wt$ddf),
      pvalue = as.numeric(wt$p)))
    return(out)
  }

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
  # Last Phase z3: "rr" FIRST -- a quasi-likelihood has no AIC/BIC/McFadden, and binary-outcome Pearson
  # dispersion is meaningless (see reg_glance). Matches the pair reg_glance actually emits.
  default <- if (family == "rr") c("n", "wald_null")
    else if (weighted) c("n", "wald_null", "nagelkerke_r2", "aic")
    else if (family == "gaussian") c("n", "r2", "r2_adj", "f_model", "sigma")
    else { s <- c("n", "lr_null", "mcfadden_r2", "aic", "bic")
           if (family == "poisson" || grouped) s <- c(s, "dispersion")
           if (family == "ordinal") s <- c(s, "brant_po"); s }  # Phase 14q Item I
  if (is.null(stats) || identical(stats, "all") || isTRUE(stats)) return(default)
  if (isFALSE(stats) || identical(stats, "none")) return(character(0))
  # "interaction" (Last Phase z8) is not produced by reg_glance -- it is read straight off `stats` by
  # reg_build's split block -- but it belongs to this vocabulary so a user vector does not drop it.
  valid <- c("n", "lr_null", "wald_null", "mcfadden_r2", "nagelkerke_r2", "cox_snell_r2",
             "r2", "r2_adj", "f_model", "sigma", "aic", "bic", "dispersion", "brant_po",
             "interaction")
  stats[stats %in% valid]
}

# Assemble the whole-table `test` tibble for a regression table: one row per (fit's first column x
# footer stat), in new_test_tibble() schema. `fit_first_col` = the fmt column each fit is keyed under
# (MNL/ordinal -> the first category column). `grouped_by_fit` marks grouped-binomial fits (dispersion).
# Phase 15e: `families_by_fit` is per fit (aligned to `fits`) -- a scalar is recycled for a direct
# caller -- so a mixed-family table gets each outcome's own stat set (gaussian R2 / logit McFadden).
reg_gof_tibble <- function(fits, fit_first_col, families_by_fit, weighted, grouped_by_fit, stats,
                           nobs_by_fit) {
  if (length(families_by_fit) == 1L) families_by_fit <- rep(families_by_fit, length(fits))
  rows <- purrr::map(seq_along(fits), function(i) {           # integer index (fits may be NAMED)
    f    <- fits[[i]]
    fam_i <- families_by_fit[[i]]
    keep <- reg_footer_stats(fam_i, weighted, isTRUE(grouped_by_fit[[i]]), stats)
    if (length(keep) == 0) return(NULL)                        # stats = FALSE -> no glance, no warnings
    # Phase 15b: the reref fast path carries the reference-invariant glance in `f$glance` (the raw fit
    # was discarded); a real reg_fit() result has no `$glance` -> compute from `f$fit` as before.
    g    <- if (!is.null(f$glance)) f$glance
            else reg_glance(f$fit, fam_i, isTRUE(grouped_by_fit[[i]]), weighted, nobs_by_fit[[i]])
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
  # Last Phase z3: an "rr" fit is an svyglm (see reg_fit), so its comparison takes the DESIGN-BASED Wald
  # branch below whether or not the user gave a design -- a likelihood-ratio test between two
  # quasi-likelihood fits would be a false LR.
  use_wald <- weighted || family == "rr"
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
      if (use_wald) {
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
    cli::cli_inform(c(
      "i" = paste0(
        "Column {.val {col}}: models are not nested or N differs -> showing the AIC difference vs the ",
        "{if (compare == 'sequential') 'previous' else 'baseline'} model instead of a likelihood-ratio test."),
      "i" = 'A different N is usually the per-model missing-value drop; set {.code na = "drop_all_models"} to fit every model on the same complete cases so the likelihood-ratio test can run.'))
    row(paste0("compare_", tag, "_aic"), col, statistic = daic, nobs = fits[[i]]$nobs)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}


# --- Last Phase z8: the aggregated effect-modification test (predictor x split_var) -----------------
# The per-cell `between_groups` colour says how big each group difference is, one cell at a time; this
# says ONCE per predictor whether its effect differs between groups at all -- the textbook test, and
# aggregated, so it carries no multiplicity inflation. ONE extra pooled fit `y ~ (predictors) * g`
# through reg_fit(cross =), then per predictor:
#   * unweighted -> drop1(scope = the interaction terms), LR (Chisq) or F for gaussian/quasi -- one
#     call, per-term and order-independent, which anova(fit)'s Type-I sequence is not;
#   * weighted / "rr" -> survey::regTermTest() per predictor, no refit.
# The LR/F-vs-Wald split is reg_compare_rows()'s own rule (use_f / use_wald), so the two extra-fit
# footer tests never disagree about what a weighted model may claim.
#
# DESIGN -- these rows are deliberately ABSENT from reg_footer_spec(). A footer ROW is keyed to exactly
# one model column and reg_spread_models() re-keys per split group; a POOLED test belongs to neither,
# and one row per predictor cannot be expressed by a fixed discriminator->label list anyway. So the
# rows stay pure data (read by reg_interaction_line, rendered as a table-wide footer STREAM like the
# weight / "Model:" lines), and both row consumers, which filter on names(reg_footer_spec()), ignore
# them -- the existing GOF footer is untouched. `row_var` carries the predictor (its canonical meaning
# in the crosstab arm); `col_var` the fit's first column, so several models each get their own line.
#' @keywords internal
reg_interaction_types <- function() c("interact_lr", "interact_f", "interact_wald")

#' @keywords internal
reg_interaction_rows <- function(reg_gof, data, specs, shared, split_var, fit_first_col) {
  weighted <- shared$weighted
  row <- function(test, col_var, predictor, statistic, df1, df2, pvalue, nobs)
    tibble::tibble(row_var = predictor, col_var = col_var, test = test, statistic = statistic,
                   df1 = df1, df2 = df2, pvalue = pvalue, n = nobs, min_e = NA_real_)

  rows <- purrr::map(seq_along(specs), function(i) {
    sp <- specs[[i]]
    # No pooled interaction for the engines that are not a single glm/svyglm equation (multinomial /
    # ordinal have their own fitters), nor for the compound-formula escape hatch (the interaction of an
    # arbitrary formula is ill-defined). Degrade to no row, never to a wrong one.
    if (sp$family %in% c("multinomial", "ordinal") || isTRUE(sp$compound)) return(NULL)
    preds <- sp$predictors
    if (length(preds) == 0L) return(NULL)
    f <- tryCatch(reg_fit(data, sp$dependent, preds, sp$family, shared$design_spec, sp$do_exp,
                          if (is.null(sp$inverse)) shared$inverse_two_level_factors else sp$inverse,
                          shared$conf_level, "wald", trials = sp$trials, formula = NULL,
                          multiplier = NULL, cross = split_var),
                  error = function(e) NULL)
    if (is.null(f) || is.null(f$fit)) return(NULL)
    fit      <- f$fit
    use_f    <- sp$family %in% c("gaussian", "quasipoisson")
    use_wald <- weighted || sp$family == "rr"
    # WARNING: take the interaction terms from the FIT's own term.labels, verbatim -- never rebuild
    # them. terms() orders the parts of an interaction by the variable's position in the formula, so a
    # hand-built "age:party3" comes back as "party3:age" and drop1() then rejects the scope. Both
    # drop1() and regTermTest() accept the labels as a CHARACTER vector, which skips the re-parse.
    have  <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
    inter <- have[grepl(":", have, fixed = TRUE)]
    keyed <- vapply(inter, function(tl) {
      parts <- gsub("`", "", strsplit(tl, ":", fixed = TRUE)[[1]], fixed = TRUE)
      if (length(parts) == 2L && split_var %in% parts) setdiff(parts, split_var)[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
    ok      <- !is.na(keyed) & keyed %in% preds
    terms_i <- inter[ok]
    keep    <- keyed[ok]
    if (length(terms_i) == 0L) return(NULL)

    if (use_wald) {
      purrr::map2(keep, terms_i, function(pv, tm) {
        e <- tryCatch({
          rt <- suppressWarnings(survey::regTermTest(fit, tm))
          list(stat = as.numeric(rt$Ftest), df1 = as.numeric(rt$df),
               df2 = as.numeric(rt$ddf), p = as.numeric(rt$p))
        }, error = function(e) NULL)
        if (is.null(e) || is.na(e$p)) return(NULL)
        row("interact_wald", fit_first_col[[i]], pv, e$stat, e$df1, e$df2, e$p, f$nobs)
      })
    } else {
      d1 <- tryCatch(suppressWarnings(
        stats::drop1(fit, scope = terms_i, test = if (use_f) "F" else "Chisq")),
        error = function(e) NULL)
      if (is.null(d1)) return(NULL)
      p_col <- grep("^Pr\\(", names(d1), value = TRUE)
      if (!length(p_col)) return(NULL)
      m <- match(terms_i, rownames(d1))
      purrr::map(seq_along(keep), function(k) {
        j <- m[[k]]
        if (is.na(j)) return(NULL)
        p <- suppressWarnings(as.numeric(d1[[p_col[1]]][j]))
        if (is.na(p)) return(NULL)
        stat <- suppressWarnings(as.numeric(d1[[if (use_f) "F value" else "LRT"]][j]))
        row(if (use_f) "interact_f" else "interact_lr", fit_first_col[[i]], keep[[k]],
            stat, suppressWarnings(as.numeric(d1[["Df"]][j])),
            if (use_f) suppressWarnings(as.numeric(stats::df.residual(fit))) else NA_real_,
            p, f$nobs)
      })
    }
  })
  rows <- purrr::compact(purrr::flatten(purrr::compact(rows)))
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
  names(coef_v) <- stringi::stri_replace_all_regex(names(coef_v), "`", "")   # match skeleton terms (as reg_fit does)
  dn <- stringi::stri_replace_all_regex(rownames(V), "`", "")
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
reg_reref_fit_res <- function(digest, reference, sp, skeleton, conf_level, multiplier = NULL) {
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
  # Last Phase z9: the multiplier, applied with reg_fit()'s OWN two expressions in reg_fit()'s OWN order
  # (scale, then phi, then the Wald finalize) -- so the reref stays byte-identical to a real refit by
  # construction, not merely to a rounding. Folding k into the contrast (L[t] <- k) would compute
  # sqrt(k^2 V) where reg_fit computes |k| sqrt(V): equal in exact arithmetic, not in IEEE754.
  # The DIGEST itself stays native-scale (reg_build_digest passes multiplier = NULL), so it is both
  # reference- AND multiplier-independent: changing the scaling is a cache HIT plus this loop.
  if (!is.null(multiplier)) {
    mult_vec <- rep(1, n)
    for (v in names(multiplier)) {
      mi <- !is.na(rows$term) & rows$term == v
      if (any(mi)) mult_vec[mi] <- as.numeric(multiplier[[v]])
    }
    est <- est * mult_vec
    se  <- se  * abs(mult_vec)
  }
  if (isTRUE(digest$scaled)) se <- se * sqrt(digest$phi)                 # caller pre-scales the SE
  crit <- reg_wald_crit(digest$disp_known, digest$df_residual, conf_level)
  res  <- reg_wald_finalize(est, isTRUE(digest$do_exp), se = se, crit = crit,
                            disp_known = digest$disp_known, df = digest$df_residual)
  list(tidy = tibble::tibble(term = rows$term, estimate = res$estimate,
                             conf.low = res$conf.low, conf.high = res$conf.high, p.value = res$p.value),
       nobs = digest$nobs, var_y = digest$var_y, positive_level = digest$positive_level,
       glance = digest$glance, fit = NULL, data = NULL)
}


# Phase g: with a split_var + a SINGLE model (one dependent, one predictor set, not multinomial), spread
# the stacked grouped_tab so the per-subpopulation models sit SIDE BY SIDE (spread_models = TRUE). The
# split level is folded into each spread column's col_var as "{level}<br>{model outcome}", so a border
# separates the sub-models and the span header reads on two lines (e.g. "White" over "married: Married").
# The spread column NAME ends with the split level (single col_level -> the name IS the level; several
# empirical/model columns -> "{col_level}_{level}"); the base col_var (the shared outcome) is read off
# the pivoted column and prefixed. Console tells the models apart by that name suffix (col_var is not
# shown there); html / Excel get the two-line span + borders.
# reg_gap_se_of() -- Last Phase z8: recover a column's per-cell standard error, on the estimate's own
# TEST scale, from the Wald interval it already stores. `reg_wald_finalize()` exponentiates before
# storing, so a multiplicative interval must be logged back first -- the SE of an OR / RR / IRR lives on
# the log scale, which is also the scale the gap and `gap_se` are measured on.
#
# DESIGN -- z, not the interval's own critical value. `reg_wald_crit()` uses z only when the dispersion
# is fixed (unweighted binomial / poisson, unscaled) and t on df.residual otherwise, and `df.residual`
# is not recoverable at the one point where the split groups are still parallel tibbles. Dividing by z
# is therefore EXACT on the fixed-dispersion path and inflates the recovered SE by t/z elsewhere --
# 0.09 % at n = 1500 with 5 parameters, i.e. conservative and negligible. dev/model_vs_observed_gap_
# test.md SS4.5 measured that a t reference changes the gap test by nothing at any n, so the gap test
# is a z test throughout.
#' @keywords internal
reg_gap_se_of <- function(col, crit) {
  lo <- get_ci_inf(col); hi <- get_ci_sup(col)
  if (as.character(get_ci_type(col))[1] %in% c("or", "ratio")) {
    ok <- is.finite(lo) & is.finite(hi) & lo > 0 & hi > 0
    ifelse(ok, (log(hi) - log(lo)) / (2 * crit), NA_real_)
  } else {
    ifelse(is.finite(lo) & is.finite(hi), (hi - lo) / (2 * crit), NA_real_)
  }
}

#' @keywords internal
# Last Phase z5: fill each group's `obs` field with the REFERENCE GROUP's estimate for the same row, so
# `color = "between_groups"` reads the per-row effect-modification contrast. `parts` is the list of
# per-group tibbles built by reg_build()'s split recursion, all sharing ONE skeleton (skeleton_data =
# the full data), hence the same rows in the same order.
#
# Rows are matched by KEY (var, level), not by position: the compound-formula path builds its skeleton
# from each GROUP's own fit (reg_skeleton_from_fit), so a group missing an interaction level has fewer
# rows in a different order -- measured. A key match degrades to NA there (uncoloured) instead of
# silently pairing the wrong rows. The reference group's own cells get NA: a group is not compared to
# itself. Non-fmt columns and groups with no counterpart are left untouched.
#
# Last Phase z8: the same pass writes `gap_se`, so `color_signif` applies. The two groups are DISJOINT
# samples, so the gap's variance is the plain sum -- sqrt(SE_i^2 + SE_ref^2), the standard test for a
# difference between two independent estimates (Altman & Bland 2003). Both SEs come from the intervals
# the table already prints, which is what makes the test and the printed intervals impossible to
# disagree. A profile-likelihood interval is asymmetric and is NOT est +/- crit*se, so `method =
# "profile"` writes no SE (the gap keeps its descriptive colour, and the policies stay inert).
reg_write_group_gap <- function(parts, color, conf_level = 0.95, method = "wald") {
  if (!"between_groups" %in% color || length(parts) < 2L) return(parts)
  key_of <- function(d) reg_skel_key(as.character(d$var), as.character(d$levels))
  ref_d  <- parts[[1L]]$data                                  # the FIRST split level is the baseline
  ref_k  <- key_of(ref_d)
  fmt_nm <- names(ref_d)[purrr::map_lgl(ref_d, is_fmt)]
  crit   <- if (identical(method, "profile")) NA_real_ else zscore_formula(conf_level)
  # the estimate a column stores, dispatched on its ci_type -- fmt_est_of() is the ONE such rule, shared
  # with fmt_gap_parts() and the crude numeric overlay (an `Obs_rate` column is ci_type "ratio" and keeps
  # its estimate in `ratio`, not `diff`).
  est_of <- fmt_est_of
  for (i in seq_along(parts)) {
    d <- parts[[i]]$data
    m <- if (i == 1L) rep(NA_integer_, nrow(d)) else match(key_of(d), ref_k)
    for (nm in intersect(fmt_nm, names(d))) {
      if (!is_fmt(d[[nm]])) next
      d[[nm]] <- set_obs(d[[nm]], est_of(ref_d[[nm]])[m])
      if (!is.na(crit)) {
        se_ref <- reg_gap_se_of(ref_d[[nm]], crit)[m]
        d[[nm]] <- set_gap_se(d[[nm]], sqrt(reg_gap_se_of(d[[nm]], crit)^2 + se_ref^2))
      }
    }
    parts[[i]]$data <- d
  }
  parts
}

reg_spread_models <- function(t, split_var, sl) {
  s    <- tab_spread(t, !!rlang::sym(split_var))
  test <- get_test(s)                                  # carried through tab_spread untouched
  # First spread fmt column of each split level (= the column its GOF footer keys under, mirroring the
  # single-column non-split placement); also rewrite every spread column's col_var for legend/borders.
  col_of_group <- stats::setNames(rep(NA_character_, length(sl)), sl)
  for (nm in names(s)[vapply(s, is_fmt, logical(1))]) {
    matches <- sl[vapply(sl, function(g) nm == g || endsWith(nm, paste0("_", g)), logical(1))]
    if (!length(matches)) next
    g <- matches[which.max(nchar(matches))]            # longest match disambiguates nested levels
    s[[nm]] <- set_col_var(s[[nm]], paste0(g, "<br>", get_col_var(s[[nm]])))
    if (is.na(col_of_group[[g]])) col_of_group[[g]] <- nm
  }
  # Last Phase m: the split build stacked one GOF block PER split level (each keyed to the SAME pre-spread
  # column via `row_var = level`); tab_spread pivots only the data, so the footer materialisers saw
  # is_split = TRUE (tripled) and matched cells by a col_var that no longer exists (empty). Re-key each
  # group's GOF rows onto that group's spread column NAME and clear `row_var` -> ONE block, each cell
  # placed under its subpopulation's column (like the single-column non-split footer).
  # Last Phase z8: re-key ONLY the per-group GOF block. The interaction rows (row_var = a PREDICTOR,
  # not a split level) are a pooled, table-wide test read by reg_interaction_line() -- keying them to a
  # group's column would be wrong, and the `col_of_group[row_var]` lookup would silently drop them.
  if (!is.null(test) && nrow(test) > 0 && !is.null(test$row_var) && any(nzchar(test$row_var))) {
    gof   <- test$test %in% reg_footer_test_types()
    part  <- test[gof, , drop = FALSE]
    g_col <- col_of_group[part$row_var]
    part  <- part[!is.na(g_col), , drop = FALSE]
    part$col_var <- unname(g_col[!is.na(g_col)])
    part$row_var <- ""
    test <- dplyr::bind_rows(part, test[!gof, , drop = FALSE])
    s <- set_test(s, test)
  }
  s
}

# The shared builder: fit every column spec, align to one skeleton, assemble a grouped_tab. specs =
# list of list(dependent, predictors, label, trials, formula, compound). The data-skeleton (union of
# the specs' predictors) is used unless a spec is a compound formula (single model), in which case the
# skeleton is read from its fitted terms (reg_skeleton_from_fit). Fit-all first so the skeleton can
# come from the fit before the columns are aligned. A multinomial fit contributes SEVERAL columns
# (one per outcome category), so the per-spec columns are flattened into one (label, col) list.
reg_build <- function(data, specs, shared, split_var = NULL, .fit_cache = NULL, reference = NULL,
                      reref = FALSE, skeleton_data = data) {
  # `shared` bundles every per-call setting the leaves + assembler read (built once in tab_reg), replacing
  # the 30-formal signature and its fragile positional re-listing at the split recursion. Contract (every
  # name always present): union_predictors, design_spec, weighted, inverse_two_level_factors, conf_level,
  # method, color_signif, cleannames, subtext, effect, at, stats, compare, baseline, multiplier,
  # multiplier_label, empirical,
  # estimate_display, spread_models, var_labels. (`split_var` stays a formal -- it flips to NULL in the
  # recursion, and a NULL value cannot live in a modifyList()-mergeable list.)
  list2env(shared, environment())
  # Phase 15e: each spec carries its OWN resolved family / do_exp / effect_shape / eff_word / color (set by
  # tab_reg), read as sp$<key>. The homogeneous-context scalar `family` (first outcome) is still needed by
  # mnl_vsrest + reg_compare_rows; derive it FROM the specs so it can never drift from them.
  family <- specs[[1]]$family

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
      tg  <- reg_build(sub, specs, utils::modifyList(shared, list(design_spec = ds_g)),
                       split_var = NULL, .fit_cache = .fit_cache, reference = NULL, reref = FALSE,
                       skeleton_data = data)
      tst <- get_test(tg); if (!is.null(tst) && nrow(tst) > 0) tst$row_var <- as.character(g)
      list(data = tibble::add_column(tibble::as_tibble(dplyr::ungroup(tg)),
                                     "{split_var}" := factor(g, levels = sl), .before = 1L),
           test = tst)
    })
    # Last Phase z5: `color = "between_groups"` scores each group's estimate against the REFERENCE
    # GROUP's on the same row. THIS is the only point where the groups exist as parallel, separately
    # addressable tibbles: one line later vec_rbind() stacks them into rows, and after
    # reg_spread_models() each is a column whose group could only be recovered from a name suffix.
    # Writing the counterpart into the per-cell `obs` field here makes BOTH output shapes work with one
    # pass, and it rides vec_rbind / group_by / tab_spread untouched (fields survive the pivot).
    # It cannot be done with the existing reference machinery: fmt_broadcast_last() groups by runs of
    # in_refrow, which cross the split boundary (measured: north's rows get south's intercept).
    # the measure lives on the SPECS (Phase 17h: specs are the truth), not on a scalar formal.
    # Last Phase z8: the same pass writes `gap_se` (the groups are disjoint -> quadrature is exact),
    # which is what lets `color_signif` apply to the gap.
    color_ms <- unique(unlist(purrr::map(specs, "color")))
    parts <- reg_write_group_gap(parts, color_ms, conf_level = conf_level, method = method)
    combined <- vctrs::vec_rbind(!!!purrr::map(parts, "data"))
    tests    <- purrr::list_rbind(purrr::compact(purrr::map(parts, "test")))
    if (is.null(tests) || nrow(tests) == 0) tests <- new_test_tibble()
    # Last Phase z8: the AGGREGATED companion of the per-cell gap colour -- one pooled interaction test
    # per predictor. Opt-in via stats = c(..., "interaction"), and automatic under
    # `color = "between_groups"` (the same "state an intent, the pipeline computes what it needs" rule
    # that makes `color = "adjustment"` turn on `empirical`). Costs one extra fit per spec, which is why
    # it is not on by default. This is the ONE place with the full data, every spec and `shared`.
    if ("between_groups" %in% color_ms ||
        (is.character(shared$stats) && "interaction" %in% shared$stats)) {
      fit_cols <- unique(tests$col_var[tests$test %in% reg_footer_test_types()])
      if (length(fit_cols) != length(specs)) fit_cols <- make.unique(purrr::map_chr(specs, "label"))
      tests <- reg_interaction_rows(tests, data, specs, shared, split_var, fit_cols)
    }
    grouped <- combined |>
      new_tab(subtext = subtext, test = tests,
              meta = list(ci_settings = list(conf_level = conf_level, method_cell = NA_character_,
                                             method_diff = method),
                          vars = if (length(var_labels)) new_vars_attr(var_labels = var_labels) else NULL)) |>
      dplyr::group_by(!!rlang::sym(split_var), var)
    # Phase g: auto tab_spread() when there is ONE model (single dependent + single predictor set) that
    # is not multinomial (a multinomial has several columns for one model, so side-by-side is ambiguous).
    # spread_models = FALSE keeps the stacked grouped_tab.
    if (isTRUE(spread_models) && length(specs) == 1L && !identical(family, "multinomial")) {
      return(reg_spread_models(grouped, split_var, sl))
    }
    return(grouped)
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
      # Phase 15d: the modelled-level choice is per-dependent (sp$inverse); fall back to the shared
      # scalar for any spec that predates it (e.g. a direct reg_build caller).
      inv_sp   <- if (is.null(sp$inverse)) inverse_two_level_factors else sp$inverse
      sp_fam   <- sp$family
      sp_dox   <- sp$do_exp
      # sp_fam in the digest key so a binomial vs gaussian outcome never share a digest (Phase 15e).
      digest <- jmvreg_cached(
        .fit_cache, "digest", jmvreg_fit_key(sp, data_canon, sp_fam, design_spec),
        function() reg_build_digest(data_canon, sp, sp_fam, design_spec, sp_dox,
                                    inv_sp, conf_level, weighted))
      reg_reref_fit_res(digest, reference, sp, skeleton, conf_level, multiplier = multiplier)
    })
  } else {
    fits <- purrr::map(specs, function(sp) {
      inv_sp   <- if (is.null(sp$inverse)) inverse_two_level_factors else sp$inverse
      sp_fam   <- sp$family
      sp_dox   <- sp$do_exp
      thunk <- function() reg_fit(data, sp$dependent, sp$predictors, sp_fam, design_spec, sp_dox,
                                  inv_sp, conf_level, method,
                                  trials = sp$trials, formula = sp$formula, multiplier = multiplier)
      # .fit_cache present but not on the reref path (ame / profile / mnl-vs-rest / compound): cache the
      # RAW reg_fit result keyed on the (already display-referenced) data -> a reference change refits.
      if (is.null(.fit_cache)) thunk()
      else jmvreg_cached(.fit_cache, "fit",
                         jmvreg_fit_key(sp, data, sp_fam, design_spec,
                                        extra = list(method, sp_dox, conf_level, effect, at,
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
    skeleton <- if (effect %in% c("ame", "ame_ratio") || mnl_vsrest)
                  reg_skeleton(skeleton_data, union_predictors)
                else if (any(compound))            reg_skeleton_from_fit(fits[[1]]$fit)
                else                               reg_skeleton(skeleton_data, union_predictors)

  prefix_dep    <- length(specs) > 1L
  # Phase 14w: a model COMPARISON (several models, one dependent) keeps each model's col_var = its own
  # name (borders separate the models; the outcome/reference/effect go in the title). A single or
  # multi-dependent table shares one outcome col_var per model column + its empirical companions.
  n_dep         <- length(unique(purrr::map_chr(specs, "dependent")))
  is_comparison <- length(specs) > 1L && n_dep == 1L
  # Last Phase z9: ONE predictor-kind split for the whole builder (reg_is_factor_var) -- the AME
  # column's numeric cells, the crude companions and the crude tips all read the same two vectors.
  numeric_preds <- reg_numeric_preds(skeleton_data, union_predictors)
  factor_preds  <- reg_factor_preds(skeleton_data, union_predictors)

  # built_per_fit: a list PER FIT of {label, col} lists (a multinomial / MNL-vs-rest / AME-per-category
  # fit contributes SEVERAL columns). Kept un-flattened so reg_gof_tibble() can key the model-summary
  # footer to each fit's FIRST output column (Phase 12f).
  if (effect %in% c("ame", "ame_ratio")) {
    built_per_fit <- purrr::map2(fits, specs, function(f, sp) {
      # Phase 15e: prob-scale / per-category / colour shape are per OUTCOME family (a mixed AME table
      # mixes binomial prob-points with a gaussian coef in one grid).
      sp_fam       <- sp$family
      sp_eff       <- sp$eff_word
      sp_col       <- sp$color
      prob_scale   <- reg_fam_prob(sp_fam)
      per_category <- sp_fam %in% c("multinomial", "ordinal")
      # Last Phase z3: effect = "ame_ratio" swaps the ADDITIVE contrast for the log-ratio one, i.e. the
      # ratio of adjusted predicted probabilities (a marginal risk ratio). Guarded to prob-scale families
      # at the tab_reg() boundary, so `shape` can only become "prob_ratio" where a probability exists.
      ratio_ame    <- effect == "ame_ratio"
      shape        <- if (!prob_scale) "raw" else if (ratio_ame) "prob_ratio" else "prob"
      marg  <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                            at = at, want_pred = prob_scale,
                            comparison = if (ratio_ame) "lnratioavg" else NULL,
                            multiplier = multiplier)
      var_y <- if (!prob_scale) suppressWarnings(stats::var(as.numeric(f$data[[sp$dependent]])))
               else NA_real_
      if (per_category) {                            # one AME column per OUTCOME category (all levels)
        groups <- levels(as.factor(f$data[[sp$dependent]]))
        purrr::map(groups, function(g) {
          jc  <- reg_cleanup(g, cleannames)
          # Phase 14s (G) + 14w (item 3): the per-category AME columns of one model share `sp$label`
          # ("<dep>: AME (adjusted %)") as col_var (no inter-category border, one span names the effect
          # once); the visible NAME is just the category (the repeated ": AME" is stripped).
          lab <- paste0(if (prefix_dep) paste0(sp$dependent, " - ") else "", jc)
          list(label = lab, emp_key = g,   # emp_key: raw category, for the empirical tooltip (Phase 14v)
               col   = reg_marginal_column(skeleton, marg, sp$predictors, numeric_preds, shape,
                                           var_y, f$nobs, g, sp_col, color_signif, sp$label,
                                           model_family = sp_fam))
        })
      } else {
        # Phase 14r (E): the model OR (exp of the fit's coefficient, aligned to the skeleton by term)
        # carried in the AME column's `or` field for the tooltip. Binomial single-outcome only -- for
        # gaussian/poisson the coefficient is not an OR. NA on reference / out-of-model rows (term NA).
        # the OR hover is a "prob"-shape rider only: a "prob_ratio" cell already SHOWS a ratio in `or`.
        or_tip <- if (sp_fam == "binomial" && !ratio_ame) {
          td <- broom::tidy(f$fit); td$term <- stringi::stri_replace_all_regex(td$term, "`", "")
          exp(td$estimate[match(skeleton$term, td$term)])
        } else NULL
        # Phase 14w (item 3): the single AME column shares the outcome col_var with its empirical
        # companions; its NAME carries the effect ("Model AME (adjusted %)").
        cv <- if (is_comparison) sp$label
              else reg_shared_col_var(sp_fam, sp$dependent, f$positive_level, cleannames)
        list(list(
          label = reg_model_col_name(sp_eff, sp$dependent, is_comparison, sp$label, n_dep),
          col   = reg_marginal_column(skeleton, marg, sp$predictors, numeric_preds, shape,
                                      var_y, f$nobs, NA_character_, sp_col, color_signif,
                                      cv, or_tip = or_tip, model_family = sp_fam)))
      }
    })
  } else if (mnl_vsrest) {
    # MNL "j vs rest" OR at the reference profile (D3-flavour-2): exp of the profile log-odds-ratio of
    # "category j vs the rest" for each predictor level; one OR column per outcome category. Reached only
    # for a HOMOGENEOUS multinomial table (a mixed table degrades at="reference" -> "average" upstream).
    built_per_fit <- purrr::map2(fits, specs, function(f, sp) {
      sp_fam <- sp$family
      sp_col <- sp$color
      marg   <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                             at = "reference", comparison = "lnor", want_pred = FALSE)
      groups <- levels(as.factor(f$data[[sp$dependent]]))
      purrr::map(groups, function(g) {
        jc  <- reg_cleanup(g, cleannames)
        # Phase 14s (G) + 14w (item 3): shared col_var (`sp$label`) across the "vs rest" category columns
        # of one model; the repeated ": OR" is stripped from the visible NAME (the span carries it).
        lab <- paste0(if (prefix_dep) paste0(sp$dependent, " - ") else "", jc, " vs rest")
        list(label = lab,
             col   = reg_marginal_column(skeleton, marg, sp$predictors, numeric_preds, "or",
                                         NA_real_, f$nobs, g, sp_col, color_signif, sp$label,
                                         model_family = sp_fam))
      })
    })
  } else {
    built_per_fit <- purrr::map2(fits, specs, function(f, sp) {
      # Phase 15e: each column takes its own family shape (multinomial fans out; glm/gaussian are one col).
      sp_fam   <- sp$family
      sp_shape <- sp$effect_shape
      sp_eff   <- sp$eff_word
      sp_col   <- sp$color
      if (sp_fam == "multinomial") {
        cols <- reg_columns_multinom(skeleton, f, sp, sp_shape, sp_col, color_signif,
                                     sp_eff, cleannames, prefix_dep, model_family = sp_fam)
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
               else reg_shared_col_var(sp_fam, sp$dependent, f$positive_level, cleannames)
        col <- reg_column(skeleton, f, model_predictors, cv, sp_shape, sp_col, color_signif,
                          model_family = sp_fam)
        col <- reg_apply_estimate_display(col, estimate_display, skeleton, f, sp, sp_fam,
                                          design_spec, conf_level, numeric_preds, model_predictors,
                                          multiplier = multiplier)
        list(list(label = reg_model_col_name(sp_eff, sp$dependent, is_comparison, sp$label, n_dep),
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
  # Phase 15e: the GOF stat SET is chosen per fit from its OWN family, so a mixed table shows each
  # outcome's stats (gaussian R2 next to a logit McFadden); test_grid_reg unions the rows + blanks.
  families_by_fit <- purrr::map_chr(specs, ~ .$family)
  grouped_by_fit  <- purrr::map_lgl(specs, ~ (.$family) == "binomial" &&
                                      !is.null(.$trials) && !isTRUE(.$compound))
  nobs_by_fit    <- purrr::map_dbl(fits, "nobs")
  reg_gof <- reg_gof_tibble(fits, fit_first_col, families_by_fit, weighted = weighted,
                            grouped_by_fit = grouped_by_fit, stats = stats,
                            nobs_by_fit = nobs_by_fit)
  reg_gof <- reg_compare_rows(reg_gof, fits, specs, family, weighted = weighted,
                              fit_first_col = fit_first_col, compare = compare, baseline = baseline,
                              conf_level = conf_level)

  disp_levels <- reg_cleanup(skeleton$level, cleannames)
  # multiplier (Phase 12g / 15d): relabel the display level of each scaled numeric predictor so the row
  # reads "<var> (per <unit>)" -- KEEP the predictor name (dropping it left a bare "per 2" the user could
  # not read). Last Phase z9: the unit text comes from `multiplier_label` ("1 SD (13.5)" / "10 units"),
  # resolved with the number itself so the two can never disagree, and the row is found through the
  # STORED predictor kind rather than the `level == var` convention (which `cleannames` and this very
  # relabel already break -- Phase 17 rule 2).
  if (length(multiplier_label)) {
    num_rows <- skeleton$var %in% numeric_preds
    for (v in names(multiplier_label)) {
      lab <- multiplier_label[[v]]
      if (is.na(lab)) next
      hit <- num_rows & skeleton$var == v
      if (any(hit)) disp_levels[hit] <- paste0(disp_levels[hit], " (per ", lab, ")")
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
  # Phase 15e: each fit's crude companion uses its OWN family (a mixed table pairs each model column with
  # the matching Emp. % / Emp. mean / Emp. rate + effect column). Ineligible outcomes (ordinal, or a
  # grouped/compound binomial with no positive level) are skipped individually; multinomial is tooltip-only.
  emp_by_fit <- vector("list", length(specs))
  # The per-dependent complete-case frame the crude companions + multinomial tips share with the model
  # (reg_complete_frame = reg_fit's own frame). `union_predictors` == the model's predictors when not
  # comparing; in comparison mode it is the shared population. Recomputed from `data` (fits[[i]]$data is
  # NULL on the reref/digest path). On this listwise-complete frame reg_empirical()'s per-predictor NA
  # filter is a no-op, so the crude reference level / n exactly match the model.
  emp_frame_of <- function(dep)
    reg_complete_frame(data, c(dep, union_predictors, reg_design_vars(design_spec)))
  if (isTRUE(empirical)) {
    fac_preds_e <- factor_preds
    # Last Phase z9: numeric predictors get a crude column too, from the univariable fit. Excluded in
    # compound-formula mode: there `var` is a bare RHS name whose model term may be an interaction or a
    # basis expansion (`age*race`, `poly(age, 2)`), so a univariable slope is not that row's estimand.
    num_preds_e <- if (any(purrr::map_lgl(specs, ~ isTRUE(.$compound)))) character(0) else numeric_preds
    if (length(fac_preds_e) > 0L || length(num_preds_e) > 0L) {
      for (i in seq_along(specs)) {
        # Last Phase z10: ONE stored fact decides eligibility -- reg_crude_key(), computed once at spec
        # construction. It replaced a duplicated family whitelist here, a `quasipoisson -> poisson` alias
        # in reg_empirical_columns(), a lookup-miss return, and (worst) `positive_level`-is-NULL as a
        # proxy for "grouped binomial or compound formula" -- which was a SIDE EFFECT of reg_fit()
        # skipping reg_prep_binary() on that path, not a statement about crude twins.
        key_i   <- specs[[i]]$crude_key
        if (is.na(key_i)) next
        fam_i   <- specs[[i]]$family
        col_i   <- specs[[i]]$color               # on/off follows the model column
        dep_i   <- specs[[i]]$dependent
        pos_i   <- if (reg_fam_binary(fam_i)) fits[[i]]$positive_level else NULL
        mdata_i <- emp_frame_of(dep_i)                    # Change B: same complete-case frame as the model
        var_y_i <- if (fam_i == "gaussian")
          suppressWarnings(stats::var(as.numeric(mdata_i[[dep_i]]), na.rm = TRUE)) else NA_real_
        emp_i   <- reg_empirical(mdata_i, fac_preds_e, dep_i, key_i, pos_i, design_spec$wt,
                                 trials = specs[[i]]$trials, ref_category = fits[[i]]$y_ref,
                                 conf_level = conf_level)
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
        fit_i   <- reg_empirical_fit(
          data, fit_preds_e, dep_i, fam_i, design_spec,
          inverse = if (is.null(specs[[i]]$inverse)) inverse_two_level_factors else specs[[i]]$inverse,
          conf_level = conf_level, method = method, skeleton = skeleton, multiplier = multiplier,
          other_preds = union_predictors, effect = effect, wt = design_spec$wt,
          want_fit = "adjustment" %in% specs[[i]]$color, trials = specs[[i]]$trials,
          marginal = effect %in% c("ame", "ame_ratio") &&
            (reg_fam_binary(fam_i) || reg_fam_prob(fam_i)))
        cols_i  <- reg_empirical_columns(skeleton, emp_i, fac_preds_e, key_i, fam_i, effect, var_y_i,
                                         conf_level = conf_level, color_signif = color_signif,
                                         color = col_i, do_exp = specs[[i]]$do_exp, fit_est = fit_i)
        # Phase 14w (item 3): the crude companions share the model column's outcome col_var (one span,
        # no border). NOT in comparison mode (the crude block stays a distinct col_var beside the models).
        if (!is_comparison && length(cols_i$cols)) {
          scv    <- reg_shared_col_var(fam_i, dep_i, pos_i, cleannames)
          cols_i$cols <- purrr::map(cols_i$cols, ~ set_col_var(.x, scv))
        }
        # Last Phase z8-B: the crude block also carries what the GAP TEST needs -- the frame it was
        # computed on and the factor predictors it covers. Both are locals here and nowhere else, and
        # the test is per (model column, crude block): in comparison mode ONE observed block serves
        # several models, each with its own fit, so the covariance differs per model though `obs` does
        # not. Keeping them on the record is what lets the column loop below stay one call.
        cols_i$frame     <- mdata_i
        cols_i$fac_preds <- fac_preds_e
        cols_i$crude_key <- key_i
        # Last Phase z9/z10: the fit-derived crude legs travel too. A row with no closed form has no
        # closed-form crude influence function either (reg_crude_if_maker() is cell-indicator
        # arithmetic), so it comes from this fit -- the second IF path SS13 forecast. The fits are kept
        # ONLY when some spec asked for `color = "adjustment"`; they are build-time locals and never
        # reach the jamovi .fit_cache, whose persisted raw fits were Phase o's freeze.
        cols_i$fit_preds <- fit_preds_e
        cols_i$fits      <- fit_i$fits
        cols_i$grid      <- emp_i
        emp_by_fit[[i]] <- cols_i
      }
    }
  }
  # Last Phase z5: the crude EFFECT vector, per fit, on the model column's own scale -- written into
  # each model column's `obs` field below. It is what `color = "adjustment"` scores and what the
  # `{obs}` display token / the html tooltip print. NULL for a fit with no crude companion
  # (multinomial, ordinal, grouped-binomial) -> `obs` stays NA -> those cells stay uncoloured.
  # Last Phase z8-B: the whole crude RECORD travels (effect + shape + frame), because its gap SE needs
  # the last two; `obs` and `gap_se` are then written together or not at all.
  emp_of <- function(fi) if (is.na(fi) || is.null(emp_by_fit[[fi]])) NULL else emp_by_fit[[fi]]
  # Last Phase z8 (a z5 defect): `at = "reference"` makes the model cell a marginal effect AT THE
  # REFERENCE PROFILE, while the crude companion stays a MARGINAL effect over the whole sample -- two
  # different estimands, so their difference is not "what adjustment did". The stratum-restricted crude
  # effect would match the estimand but answers a different question (model FIT at one profile, not
  # confounding) on a few percent of the rows, so no `obs` is attached at all: the cells stay
  # uncoloured, `{obs}` blanks, and tab_reg() says why once.
  at_profile <- identical(at, "reference")
  # `fi` = the fit this COLUMN came from (not the crude block's -- they differ in comparison mode).
  # Last Phase z10: `key` = the column's OWN outcome category, stored on it at build time as `emp_key`
  # (reg_columns_multinom / the per-category AME loop already stamp it). A multinomial or ordinal-marginal
  # fit owns one column per category and each needs its OWN crude counterpart, so the crude record's
  # `effect` is a list keyed the same way; "" is the key of a single-column fit. A missing key means no
  # crude counterpart for that column -- the honest degrade, leaving `obs` NA and the cell uncoloured.
  set_obs_if <- function(bi, e, fi) {
    col <- bi$col
    if (is.null(e) || at_profile) return(col)
    if (!reg_same_estimand(e$shape, col)) return(col)     # z5 defect: same scale, or nothing
    key <- if (is.null(bi$emp_key)) "" else as.character(bi$emp_key)
    ev  <- cat_get(e$effect, key)
    if (is.null(ev)) return(col)
    col <- set_obs(col, ev)
    # Last Phase z10 (maintainer's ruling Q4): when the crude effect draws NO column of its own, fold it
    # into the model cell -- "{or} ({obs})" / "{diff} ({obs})" -- so it is visible at all. Driven by the
    # shape's `visible` fact, never by `family == "multinomial"`. Three reasons this is the right shape:
    # `obs` is defined ON THE CELL'S OWN SCALE, so the bracket is the same kind of quantity as the
    # estimate; the printed bracket then IS what `color = "adjustment"` scores, so the number and the
    # colour can never tell different stories; and the crude PERCENTAGE is not lost -- it stays in the
    # `empirical_tips` tooltip, which already fires for exactly these columns.
    if (!shape_visible(e$shape)) {
      d    <- get_display(col)
      prim <- display_primary(d)
      hit  <- is.finite(ev) & prim %in% c("or", "diff")
      if (any(hit)) col <- set_display(col, dplyr::if_else(hit, paste0("{", prim, "} ({obs})"), d))
    }
    g <- reg_gap_se_columns(fits[[fi]], specs[[fi]], col, skeleton, e$shape, e$frame,
                            e$fac_preds, effect, at, design_spec$wt,
                            fits_crude = e$fits, fit_preds = e$fit_preds, multiplier = multiplier,
                            category = key)
    if (is.null(g)) col else set_gap_se(col, g)
  }
  # a fit's columns (a multinomial / per-category fit owns several) -- hoisted above both branches so
  # ONE set_obs_if() serves them.
  fit_of_col <- rep(seq_along(fit_ncol), times = fit_ncol)
  # one crude companion before all model columns when there is a single dependent (byte-identical
  # layout, incl. a model-comparison list -- all its models share the dependent); per-fit before each
  # fit's first model column when several dependents (names suffixed so they do not collide).
  # Phase g: the multi-dependent disambiguator is a "[dep]" BRACKET (console shows it; every exporter
  # STRIPS it via tab_col_var_header, the col_var span already naming the outcome).
  add_emp_cols <- function(tab, cols, suffix) {
    for (nm in names(cols)) {
      out_nm <- if (nzchar(suffix)) paste0(nm, " [", suffix, "]") else nm
      tab[[out_nm]] <- cols[[nm]]
    }
    tab
  }
  if (n_dep <= 1L) {
    if (!is.null(emp_by_fit[[1]])) tab <- add_emp_cols(tab, emp_by_fit[[1]]$cols, "")
    # ONE crude block serves every model column here -- which is exactly what makes `adjustment` work
    # in model-comparison mode: each model is scored against the same observed effect. Its gap SE
    # still comes from each column's OWN fit (the two estimators' covariance is per model).
    emp1 <- emp_of(1L)
    for (i in seq_along(built))
      tab[[labels[i]]] <- set_obs_if(built[[i]], emp1, fit_of_col[i])
  } else {
    # several dependents: each fit has its OWN crude block, so map every column back to its fit
    # (fit_first_idx marks a fit's FIRST column; a multinomial fit owns several).
    for (i in seq_along(built)) {
      fi <- match(i, fit_first_idx)                        # non-NA at a fit's first column
      if (!is.na(fi) && !is.null(emp_by_fit[[fi]]))
        tab <- add_emp_cols(tab, emp_by_fit[[fi]]$cols, specs[[fi]]$dependent)
      tab[[labels[i]]] <- set_obs_if(built[[i]], emp_of(fit_of_col[i]), fit_of_col[i])
    }
  }

  # multinomial empirical (Phase 14v): TOOLTIP-only (one column per category would explode the layout).
  # The crude % + diff per (category column, predictor level) travel in the `empirical_tips` table
  # attribute (carried through dplyr like `test`); the render appends an "crude:" fragment. Keyed by the
  # FINAL make.unique'd column label (each category column carries its raw category in `emp_key`).
  # Phase 15e: crude tooltips are built PER multinomial fit (a mixed table can hold several multinomial
  # outcomes, each with its own dependent / complete-case frame), keyed to that fit's category columns.
  empirical_tips <- NULL
  mnl_specs <- which(purrr::map_chr(specs, ~ .$family) == "multinomial")
  if (isTRUE(empirical) && length(mnl_specs) > 0L) {
    fac_preds_t <- factor_preds
    is_fac_t <- skeleton$var %in% fac_preds_t
    if (length(fac_preds_t) > 0L) {
      tip_rows <- purrr::flatten(purrr::map(mnl_specs, function(si) {
        dep_i    <- specs[[si]]$dependent
        cols_idx <- fit_first_idx[[si]]:(fit_first_idx[[si]] + fit_ncol[[si]] - 1L)
        cols_idx <- cols_idx[!purrr::map_lgl(built[cols_idx], ~ is.null(.$emp_key))]
        if (length(cols_idx) == 0L) return(list())
        # Change B: multinomial crude tooltips on the model's complete-case frame (shared with the model).
        # Last Phase z10: read straight off the MERGED crude grid -- reg_empirical_tips() is gone, it was
        # reg_empirical() at a three-part key (measured bit-identical), and keeping two producers of one
        # quantity is exactly the sync-by-comment pair Phase 17 rule 5 forbids. Reuse the block already
        # built for this fit when there is one; otherwise build the grid here.
        tipsd <- if (!is.null(emp_by_fit[[si]]$grid)) emp_by_fit[[si]]$grid else
          reg_empirical(emp_frame_of(dep_i), fac_preds_t, dep_i, "multinomial", NULL, design_spec$wt,
                        ref_category = fits[[si]]$y_ref, conf_level = conf_level)
        tk    <- reg_skel_key(tipsd$var, tipsd$level, tipsd$category)
        purrr::compact(purrr::map(cols_idx, function(i) {
          b    <- built[[i]]
          mi2  <- match(reg_skel_key(skeleton$var, skeleton$level, b$emp_key), tk)
          keep <- is_fac_t & !is.na(mi2) & !is.na(tipsd$emp_prop[mi2])
          if (!any(keep)) return(NULL)
          k  <- mi2[keep]
          pr <- tipsd$emp_prop[k]; df <- tipsd$emp_diff[k]
          # 14v-ii: the crude % carries its Wilson CI; a non-reference level also shows its crude
          # difference from the reference and that difference's Newcombe CI (percentage points).
          tibble::tibble(
            col   = labels[i],
            var   = as.character(skeleton$var[keep]),
            level = disp_levels[keep],
            tip   = ifelse(skeleton$is_ref[keep],
                           sprintf("crude: %.0f%% [%.0f; %.0f]",
                                   pr * 100, tipsd$emp_prop_inf[k] * 100, tipsd$emp_prop_sup[k] * 100),
                           sprintf("crude: %.0f%% (%+.0f pts [%+.0f; %+.0f])",
                                   pr * 100, df * 100, tipsd$emp_diff_inf[k] * 100, tipsd$emp_diff_sup[k] * 100)))
        }))
      }))
      if (length(tip_rows)) empirical_tips <- purrr::list_rbind(tip_rows)
    }
  }
  # Last Phase z9: a numeric predictor's DESCRIPTIVE goes in the tooltip, because nothing can honestly
  # go in its base cell -- measured (SS4.1), the univariable fit's only base-scale output is
  # P(Y | X = mean X), which is the MARGINAL rate for every numeric predictor (0.4738 for both `age` and
  # `tvhours` against an overall 0.4744), i.e. a cell that looks per-predictor and is not. What IS well
  # defined is the predictor's own distribution and its mean within each outcome group, so those ride the
  # existing `empirical_tips` mechanism -- attached to the crude EFFECT column, which has visible content
  # (a tooltip on the blank base cell would never be discovered).
  if (isTRUE(empirical) && length(numeric_preds) > 0L) {
    num_tips <- purrr::compact(purrr::map(seq_along(specs), function(i) {
      e <- emp_by_fit[[i]]
      if (is.null(e) || is.null(e$shape) || !shape_visible(e$shape)) return(NULL)
      nm <- e$shape$nm                                     # the crude effect column's name
      if (is.na(nm)) return(NULL)
      if (n_dep > 1L) nm <- paste0(nm, " [", specs[[i]]$dependent, "]")
      if (!nm %in% names(tab)) return(NULL)
      vars <- intersect(intersect(e$fit_preds, numeric_preds), as.character(skeleton$var))
      if (!length(vars)) return(NULL)
      w  <- if (is.null(design_spec$wt)) NULL else e$frame[[design_spec$wt]]
      yb <- reg_crude_y(e$frame, specs[[i]]$dependent, specs[[i]]$family,
                        if (reg_fam_binary(specs[[i]]$family)) fits[[i]]$positive_level else NULL)
      purrr::list_rbind(purrr::map(vars, function(v) {
        x  <- as.numeric(e$frame[[v]])
        m  <- reg_weighted_mean(x, w); s <- reg_predictor_sd(x, w)
        # mean(X | Y): for a binary outcome the two groups, else a single overall summary.
        by <- if (reg_fam_binary(specs[[i]]$family) && length(unique(stats::na.omit(yb))) == 2L)
          sprintf("; mean if yes %s, if no %s",
                  format(signif(reg_weighted_mean(x[yb == 1], w[yb == 1]), 3)),
                  format(signif(reg_weighted_mean(x[yb == 0], w[yb == 0]), 3)))
        else ""
        k <- which(as.character(skeleton$var) == v)
        tibble::tibble(col = nm, var = v, level = disp_levels[k],
                       tip = sprintf("%s: mean %s (SD %s)%s", v,
                                     format(signif(m, 3)), format(signif(s, 3)), by))
      }))
    }))
    if (length(num_tips)) {
      nt <- purrr::list_rbind(num_tips)
      empirical_tips <- if (is.null(empirical_tips)) nt else vctrs::vec_rbind(empirical_tips, nt)
    }
  }

  # Phase 12f: the GOF footer travels in the whole-table `test` attribute (disjoint discriminators, so
  # the crosstab renderers ignore it); it is materialised as a console block / export rows at display,
  # never baked into the fmt columns (the coefficient skeleton stays intact for downstream reads).
  tab |>
    new_tab(subtext = subtext, test = reg_gof,
            meta = list(empirical_tips = empirical_tips,
                        # Phase k: variable labels for the opt-in name display-swap (absent when none).
                        vars = if (length(var_labels)) new_vars_attr(var_labels = var_labels) else NULL,
                        # 14v-ii / 17h: the numeric/ratio methods the empirical columns use, read STRAIGHT
                        # from the REG_EMPIRICAL fact table (Student mean-diff = OLS, quasi-Poisson rate-
                        # ratio = the phi-scaled model), so the legend names exactly what the crude CI used.
                        ci_settings = list(conf_level = conf_level, method_cell = NA_character_,
                                           method_diff = method, method_ratio = "katz",
                                           method_mean_diff  = REG_EMPIRICAL$gaussian$method_mean_diff,
                                           method_mean_ratio = REG_EMPIRICAL$poisson$method_mean_ratio))) |>
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
#'     comparison test), `split_var` (one table per group), `multiplier` (the unit a continuous
#'     predictor's effect is reported per — one standard deviation by default).
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
#' @param family The model family, **resolved per dependent** so several outcomes with different
#'   families can share one table (one effect column-group each). `"auto"` (default) detects each
#'   outcome: a binary (-> `"binomial"`), an ordered 3+ level (-> `"ordinal"`), a nominal 3+ level
#'   (-> `"multinomial"`), or a continuous (-> `"gaussian"`) outcome, emitting a message; an integer
#'   count stays ambiguous and must be named (for that outcome only). Set it explicitly with
#'   `"gaussian"` (linear), `"binomial"` (logistic), `"poisson"` / `"quasipoisson"` (counts),
#'   `"multinomial"` (nominal 3+ level), `"ordinal"` (ordered 3+ level). A **scalar** applies to every
#'   dependent; a **vector** aligned to `dependent`, or a **named** vector keyed by dependent
#'   (e.g. `c(income = "poisson", satisfied = "binomial")`), sets one family per outcome. Mixed
#'   families work only with a character `predictors` (one model per outcome); a `predictors` list
#'   (model comparison) is single-outcome, hence single-family.
#'
#'   \strong{A binary outcome with `family = "poisson"` (modified Poisson).} Asking for `"poisson"` on a
#'   binary outcome is not a mistake and not a count model: it fits the **modified Poisson regression**
#'   (Zou 2004), whose exponentiated coefficient is a **risk ratio** (RR), not an odds ratio. It is
#'   strictly opt-in --- a binary outcome still auto-detects as `"binomial"`; you must name `"poisson"`.
#'   Reach for it when the outcome is **common** (above roughly 10%), where an odds ratio is much further
#'   from 1 than the risk ratio and is almost always narrated as if it were one ("twice as likely"), and
#'   when you compare a coefficient **across nested models**, which an odds ratio does not support (it is
#'   non-collapsible: it moves when you add a covariate even if that covariate is not a confounder).
#'   \strong{Standard errors are handled consistently}: the Poisson likelihood is deliberately
#'   misspecified for a 0/1 outcome, so the naive standard errors are too wide and are replaced by the
#'   robust **Huber--White sandwich** --- via `survey::svyglm()` in both cases, which means the
#'   design-based variance when you supply `wt`/`ids`/`strata`, and the equivalent of `HC0` on a
#'   constant-weight design when you do not. The observed companion (`empirical = TRUE`) follows the same
#'   estimand: `Obs_RR` is the crude **risk** ratio with a Katz interval, not the crude odds ratio.
#'   Two caveats: the log link is unbounded above, so predicted probabilities can exceed 1 --- the model
#'   is for **effect estimation, not prediction**; and the sandwich needs a decent sample (n of at least
#'   about 100). `method = "profile"` is not available (a quasi-likelihood has no profile), and the
#'   footer reports N and a Wald-vs-null test rather than AIC/BIC/pseudo-R2, which a quasi-likelihood
#'   does not define.
#'
#'   \strong{Over-dispersed counts.} An unweighted `"poisson"` fit auto-scales its standard errors by
#'   the square root of the Pearson dispersion, so with an over-dispersed outcome (dispersion clearly
#'   above 1) its CIs and p-values are **identical to `"quasipoisson"`**, and it **warns** to say so
#'   (the footer reports the dispersion). At equidispersion (\eqn{\approx}1) the scaling is a no-op and
#'   the result matches a standard `glm(family = poisson)` z-interval — so a user comparing to a
#'   hand-fit Poisson `glm` is not surprised by wider intervals.
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
#' @param exponentiate Logical. `TRUE` (default) exponentiates coefficients into ratios (odds ratios
#'   for logistic, incidence-rate ratios for poisson, cumulative odds ratios for ordinal),
#'   automatically leaving gaussian linear betas on their raw scale. `FALSE` keeps every coefficient on
#'   the coefficient (log / linear) scale. Ignored when `effect = "ame"` (marginal effects are always
#'   on the response scale).
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
#'
#'   `"ame_ratio"` is the same quantity as a **ratio** instead of a difference: the **marginal risk
#'   ratio** (RR), i.e. the ratio of those adjusted predicted probabilities, with the adjusted
#'   probability again in parentheses (e.g. `0.62*** (32%)`). Its cell is coherent by construction, the
#'   multiplicative twin of the AME identity --- adjusted-%(reference) * RR(level) equals
#'   adjusted-%(level). Reach for it for the same reason as the modified Poisson (see `family`): with a
#'   **common** outcome an odds ratio is not a risk ratio and should not be narrated as one, and unlike
#'   the odds ratio a risk ratio stays comparable **across nested models**. The two routes answer
#'   slightly different questions --- `ame_ratio` gives a **marginal** (population-averaged) risk ratio
#'   from the familiar logistic fit and can never predict a probability above 1, while
#'   `family = "poisson"` gives a **conditional** one; on real data they agree closely. Standard errors
#'   are the delta-method interval on the log ratio (design-based when the model is weighted), and the
#'   observed companion is the crude risk ratio `Obs_RR` with a Katz interval --- the same estimand.
#'   Requires `marginaleffects`; **probability-scale outcomes only** (binomial / multinomial / ordinal),
#'   since a ratio of adjusted means or counts has no such interpretation. One caveat worth knowing:
#'   the estimate is standardized to the covariate distribution of the sample at hand, so under
#'   `split_var` each group standardizes to **its own** subpopulation.
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
#'   Two readings of "does this effect hold in every subgroup?" come with it:
#'   `color = "between_groups"` colours (and tests) each effect against the first group's, row by row,
#'   and `stats = c(..., "interaction")` adds the aggregated test, once per predictor.
#' @param multiplier How a **continuous** predictor's effect is scaled — the unit its row reports.
#'   One unit of a continuous variable is rarely a readable amount (a one-year change in `age` barely
#'   moves the odds, so its odds ratio sits inside the first colour break and the row reads as "no
#'   effect"), so the default is **one standard deviation**.
#'
#'   Give either a **single value**, applied to every continuous predictor, or a **named vector**
#'   overriding chosen ones (predictors it does not name keep the single-value default). Accepted
#'   values: `"sd"` (the default — per one standard deviation), `"2sd"` (per two, i.e. roughly bottom
#'   to top of the distribution), or a number of units (`10` = per decade of age). `multiplier = 1`
#'   restores the per-one-unit reading. The row label names the unit it used, e.g.
#'   `age (per 1 SD (13.5))`.
#'
#'   Everything scales together: the estimate (`OR^k` / `beta*k`), its confidence interval, the crude
#'   `Obs_*` companion and the model-versus-observed comparison; the p-value is unchanged. **Because
#'   the default is not 1, a continuous predictor's `Model_*` cell does not equal `exp(coef(glm(...)))`
#'   unless you pass `multiplier = 1`.** The standard deviation is measured **once**, on the complete
#'   cases of the predictors (not of the outcome), so one predictor keeps one unit across several
#'   outcomes, across compared models and across `split_var` groups. Names must be continuous
#'   predictors. Not applied to multinomial / ordinal outcomes, nor to a `formula` model (where a
#'   variable may enter through an interaction or a `poly()` basis). A 0/1-coded **numeric** predictor
#'   gets a "per 1 SD (0.5)" reading — pass such a variable as a factor instead.
#' @param empirical Logical. If `TRUE`, adds the **observed, unadjusted (univariable)** companion of
#'   each model effect: with a categorical predictor that is exactly the observed contrast between
#'   levels; with a continuous predictor it is the univariable slope, which assumes the effect is
#'   linear on the model's scale — check that (`cut()`, splines) before trusting it. It IS the
#'   modelised quantity when there is a single predictor, so a large crude-versus-adjusted gap signals
#'   confounding.
#'
#'   Per family: **binomial** adds `Obs_%` + `Obs_OR` (coefficient) or `Obs_%` + `Obs_diff` (AME);
#'   **gaussian** adds `Obs_mean` + `Obs_diff`; **poisson** adds `Obs_rate` + `Obs_IRR`; a
#'   **grouped binomial** (`trials =`) adds `Obs_mean` — the mean *score* — plus `Obs_OR`, the odds
#'   ratio of the summed counts; an **ordinal** outcome adds a single `Obs_cumOR`, the cumulative odds
#'   ratio of a univariable proportional-odds fit. A **multinomial** outcome would need one crude
#'   column per outcome category, so its crude effect is folded into the model cell instead —
#'   `2.31 (obs 2.05)` — with the crude % + difference per category in the HTML tooltip. A continuous
#'   predictor fills the **effect** column only: it has no levels, so its base cell stays empty and its
#'   distribution — mean, standard deviation, and the mean in each outcome group — goes to the tooltip.
#'
#'   The rule behind all of that is one sentence: *the observed effect is the model's own effect,
#'   fitted with a single predictor*. Where that univariable model happens to be **saturated** (a
#'   categorical predictor under every family except ordinal) it has a closed form and is computed
#'   directly; otherwise it is a real fit, so the crude column shares the model's family, link,
#'   confidence-interval method and `multiplier` by construction. The only outcomes left with no
#'   observed counterpart at all are the compound-`formula` escape hatch (there is no predictor
#'   structure to be crude about) and, for `effect = "ame"`, a *weighted* 3+ level outcome (no
#'   `marginaleffects` method).
#'
#'   By design every crude quantity is computed on **exactly the same complete-case population as the
#'   model** (listwise-complete on the dependent, all predictors and any design variable), so crude and
#'   adjusted are directly comparable and not confounded by differing missingness (reproduce it with
#'   [dplyr::filter()] + [tab()] on the same rows). Also works with a vector of dependents. Ordinal has
#'   no clean crude analogue and is ignored (with a message).
#'
#'   The crude companions of a **categorical** predictor are descriptive, so on weighted data they
#'   honour `options(tabxplor.kish_neff = TRUE)` (Kish's effective sample size) exactly like [tab()];
#'   a **continuous** predictor's companion comes from a univariable fit and therefore uses the model's
#'   own design-based variance, like the `Model_*` column beside it. On weighted data the two rules
#'   will not agree to the last digit — they answer the same question under different variance
#'   assumptions. Default `FALSE`.
#' @param stats The goodness-of-fit statistics shown in the model-summary **footer** (one block per
#'   model). `NULL` (default) uses the per-family set: linear models show N, R square, adjusted R
#'   square, the overall F-test and the residual SD; other models show N, the likelihood-ratio test
#'   versus the null model, McFadden's pseudo-R square, AIC and BIC (poisson / grouped-binomial models
#'   also show the Pearson dispersion). Pass a character vector to pick and order the statistics
#'   (`"n"`, `"lr_null"`, `"mcfadden_r2"`, `"aic"`, `"bic"`, `"dispersion"`, `"r2"`, `"r2_adj"`,
#'   `"f_model"`, `"sigma"`, `"interaction"`), or `FALSE` / `"none"` to hide the footer.
#'
#'   `"interaction"` is different from the others: with `split_var`, it adds one **aggregated
#'   effect-modification test per predictor** — "does this predictor act differently between the
#'   groups?", asked once for all its levels together, so it carries none of the multiplicity of the
#'   per-cell `color = "between_groups"` colours. It is printed as a footer line rather than a footer
#'   row (a pooled test belongs to no single model column), and it costs one extra model fit.
#'   `color = "between_groups"` turns it on for you. It is a likelihood-ratio test (an F test for
#'   linear and quasi models, a design-based Wald test for weighted / survey models, exactly like
#'   `compare`) on the model **coefficients** — so under `effect = "ame"` / `"ame_ratio"` the footer
#'   and the colours answer related but distinct questions, and the line says so. Multinomial and
#'   ordinal outcomes get no such test.
#'
#'   Weighted models show a reduced, survey-appropriate set of goodness-of-fit statistics
#'   (design-based Wald test, Nagelkerke pseudo-R square, AIC).
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
#' @param color,color_signif Colouring of the effect cells. `color = TRUE` (default) auto-picks the
#'   sensible per-family measure (`"OR"` magnitude for ratios, standardized `"diff"` for betas);
#'   `color = FALSE` turns colouring off for every column (model and empirical). Power users may pass a
#'   measure string (`"OR"`, `"diff"`, `"ratio"`, `"no"`) to override. `color_signif` is the
#'   significance policy (default `"grey_non_signif"`). See [tab()].
#'
#'   Two measures are **specific to regression tables**, and both are meant for the *background*
#'   channel so the text keeps showing the effect size — `color = c("OR", "adjustment")` answers
#'   "how strong is this effect?" and "how much did the model change it?" in one glance:
#'
#'   * `"adjustment"` — how far each **modelled** effect sits from its **observed** (crude,
#'     unadjusted) counterpart, i.e. what adjusting for the other predictors did to it. It turns
#'     `empirical = TRUE` on (that is where the observed effect comes from). Thresholds are
#'     `×1.1 / ×1.25 / ×1.5 / ×2` for ratios and `±2 / ±5 / ±10 / ±20` points for differences, the
#'     first one being the classic 10% "change in estimate" rule; set them with
#'     [set_color_breaks()] (`adj_ratio`, `adj_diff`). One pole means the model **strengthened** the
#'     effect (suppression), the other that it **attenuated** it — measured from the null, so a
#'     protective effect (OR < 1) and a risky one read the same way.
#'   * `"between_groups"` — with `split_var`, how far each group's effect sits from the **first**
#'     group's, on the same row: a per-predictor reading of effect modification, beside the global
#'     comparison a likelihood-ratio test gives. Reorder the split variable's levels
#'     (`forcats::fct_relevel()`) to change the baseline group. It also adds the **aggregated**
#'     interaction test to the footer (see `stats`).
#'
#'   The two are mutually exclusive (they share one per-cell slot). The gap itself is readable as a
#'   number, with `display = "\{or\} (obs \{obs\})"`, and the html tooltip adds its confidence interval
#'   and p-value.
#'
#'   **Significance.** Both measures test their own gap, and the usual policies then follow —
#'   `"grey_non_signif"` greys a gap whose interval covers "no change"; `"guaranteed_effect"` colours
#'   its floor, i.e. "the two differ by at least ×1.1". They use different standard errors, because
#'   they compare different things:
#'
#'   * `"between_groups"` compares two `split_var` groups, which are **different people**, so the gap's
#'     standard error is `sqrt(SE_A² + SE_B²)` — recovered from the two Wald intervals the table
#'     already prints, which is why the colours can never contradict them.
#'   * `"adjustment"` compares two estimates fitted on the **same rows**, which are correlated, so its
#'     standard error is the sampling variance of the difference of their influence functions (the
#'     "seemingly unrelated estimation" of Weesie 1999 / Mize, Doan & Long 2019). With survey weights
#'     or a design it is design-based (strata, clusters and FPC respected).
#'
#'   The `"adjustment"` test is only computed where a zero gap really means "no confounding" — that is,
#'   on a **collapsible** effect measure: an average marginal effect (`effect = "ame"`), a marginal risk
#'   ratio (`effect = "ame_ratio"`), a risk ratio (`family = "poisson"` on a binary outcome), an
#'   incidence-rate ratio, or a linear β. A **conditional odds ratio** is not collapsible: adjusting it
#'   moves it away from 1 even when the extra variable is independent of the exposure, so at survey
#'   sizes every row would test "significant" for a reason that is not confounding. There the colours
#'   stay descriptive, `color_signif` is ignored, and `tab_reg()` says so once. The test is also skipped
#'   when the two estimates cannot be compared cell by cell: at `at = "reference"`, and when a compared
#'   model's complete cases differ from the observed columns' (use `na = "drop_all_models"`).
#'
#'   That ruling covers the **3+ level outcomes** too, and it is the reason they behave as they do: a
#'   multinomial or ordinal *coefficient* is a conditional odds ratio, so those columns show the
#'   observed effect but carry no test. Their **marginal** paths (`effect = "ame"` / `"ame_ratio"`) are
#'   collapsible and do get one, per outcome category.
#'
#'   Two things to keep in mind. A difference between two groups, or before and after adjustment, is a
#'   difference in *that effect measure*: groups with different base rates or more variable outcomes can
#'   show different effects on every scale without the underlying cause differing. And each cell is
#'   tested on its own, with no multiple-comparison correction — with seven comparisons about one table
#'   in five shows a spurious coloured cell, so read the pattern rather than the single cell (for
#'   `"between_groups"`, the footer's aggregated test is the multiplicity-free reading).
#'
#'   The gap test uses the **robust (sandwich)** variance on both sides, which is what a comparison of
#'   two differently-specified estimators needs. For an unweighted binomial odds ratio that is exactly
#'   the Woolf interval the `Obs_OR` column prints; elsewhere it can differ slightly from the printed
#'   descriptive intervals, which follow `tab()`'s own conventions (pooled Student *t* for a mean
#'   difference, unweighted *n* under `wt`).
#'
#'   **Caveat on odds ratios.** The odds ratio is *non-collapsible*: adjusting for a covariate that
#'   predicts the outcome moves it away from 1 even with no confounding at all (about +8% in a
#'   simulation where the covariate is independent of the exposure — the same size as the first
#'   colour step). So part of an OR gap is arithmetic, not confounding. The collapsible comparisons
#'   are `effect = "ame"` / `"ame_ratio"` (marginal effects), `family = "poisson"` on a binary
#'   outcome (risk ratios) and a gaussian beta; on those the gap is confounding by the adjustment
#'   set. The legend says so on the odds-ratio path, and it is why the significance test above is
#'   computed only on the collapsible scales.
#' @param stars Logical (default `TRUE` for regression tables, where significance stars are standard).
#'   When `FALSE`, the per-cell p-value is dropped and no stars are shown (colours still read the CI).
#' @param na How missing values are handled across models. `"drop_by_model"` (default) drops `NA`
#'   rows per model (each model / dependent uses its own complete cases). `"drop_all_models"` fits every
#'   model on ONE shared complete-case population (rows with no `NA` on any predictor / dependent /
#'   design variable), so nested models get equal N and the likelihood-ratio comparison can run; note
#'   this **changes all estimates** (shared population), hence opt-in. Ignored for a prebuilt survey
#'   design.
#' @param cleannames Logical. If `TRUE`, strips numeric prefixes from factor levels for display.
#'   Uses `getOption("tabxplor.cleannames")` when `NULL`.
#' @param subtext Optional character. A note shown below the table.
#' @param spread_models Logical, only used with `split_var`. If `TRUE` (default), a single
#'   non-multinomial model fitted within each `split_var` level is automatically pivoted with
#'   [tab_spread()] so the per-group models sit side by side as columns. `FALSE` keeps the
#'   stacked grouped table (one block of rows per group).
#' @param .fit_cache Internal, for the jamovi live UI (Phase 15b): a mutable cache environment
#'   (see `jmvreg_cache_env()`) that memoizes fitted models so display / colour / reference toggles
#'   avoid a refit. On the single-equation GLM coefficient path a factor-predictor reference change is
#'   reparametrized from the cached fit (no refit). `NULL` (the default) leaves ordinary calls unchanged.
#'
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one effect column per model / dependent.
#'
#' @examples
#'   data <- gss_cat_data_formatting()
#'   # a subset keeps the examples fast: fitting these models on all 21,483 rows costs well over
#'   # CRAN's 5-second-per-topic budget. Use the full `data` in real analyses.
#'   reg_data <- head(data, 3000)
#'
#'   # logistic (odds ratios):
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'           family = "binomial")
#'
#' \donttest{
#'   # linear (betas):
#'   tab_reg(reg_data, dependent = "tvhours", predictors = c("rincome", "age"),
#'           family = "gaussian")
#'   # to use normal R model formulas instead (same model, terser):
#'   tab_reg(reg_data, married ~ race + rincome, family = "binomial")
#'
#'  # logistic : comparison between observed odds-ratio and modelised odds-ratio
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'           family = "binomial", empirical = TRUE
#'   )
#' # average marginal effects + adjusted predictions (needs the marginaleffects package):
#' if (requireNamespace("marginaleffects", quietly = TRUE)) {
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "ame")
#'   # marginal effects at the reference profile (others at their reference level / mean):
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "ame", at = "reference")
#'   # the same quantity as a RATIO: marginal risk ratios beside the crude ones. With a common
#'   # outcome this is what a reader means by "x times more likely" -- an odds ratio is not.
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "ame_ratio", empirical = TRUE)
#' }
#' # modified Poisson (Zou 2004): a binary outcome + family = "poisson" gives conditional RISK
#' # ratios with robust standard errors. Opt-in -- a binary outcome still defaults to logistic.
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "poisson", empirical = TRUE)
#' # multinomial (nominal 3+ level): one OR column per outcome category vs the reference
#'   tab_reg(reg_data, dependent = "party3", predictors = c("race", "age"),
#'                 family = "multinomial", reference = c(party3 = "3-Republican"))
#' # ordinal (proportional-odds): one cumulative-OR column
#'   tab_reg(reg_data, dependent = "rincome", predictors = c("race", "age"), family = "ordinal")
#' }
#'
#' @export
tab_reg <- function(data, dependent, predictors = NULL, split_var = NULL, wt = NULL, 
                    family = "auto", 
                    effect = c("coefficient", "ame", "ame_ratio"), at = c("average", "reference"),
                    exponentiate = TRUE, 
                    trials = NULL, empirical = FALSE,
                    color = TRUE, color_signif = NULL, stars = TRUE, 
                    conf_level = getOption("tabxplor.conf_level", 0.95), method = c("wald", "profile"),
                    reference = NULL, inverse_two_level_factors = TRUE, multiplier = "sd",
                    stats = NULL, compare = c("none", "baseline", "sequential"), baseline = NULL,
                    na = c("drop_by_model", "drop_all_models"),
                    estimate_display = c("value", "ci", "prob", "ame"),
                    cleannames = NULL, subtext = "", spread_models = TRUE,
                    ids = NULL, strata = NULL, fpc = NULL, nest = FALSE,
                    .fit_cache = NULL) {
  method  <- match.arg(method)
  effect  <- match.arg(effect)
  at      <- match.arg(at)
  compare <- match.arg(compare)
  estimate_display <- match.arg(estimate_display)
  na      <- match.arg(na)
  # Fallback FALSE matches .onLoad's default and tab()'s read sites (the option is always set to FALSE
  # on load, so this only bites if someone unsets it; TRUE here was an inconsistency, not an intent).
  cleannames <- if (is.null(cleannames)) getOption("tabxplor.cleannames", FALSE) else cleannames


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

  # Phase k (labelled interop): capture variable labels (BEFORE conversion strips them) then convert
  # labelled (haven/labelled) predictors / dependent / split columns to value-label factors -- so family
  # detection below, the skeleton and the fit all see real factors (a coded outcome becomes a proper
  # binomial/multinomial; complete-labelled predictors show their value labels). Covers a prebuilt
  # survey design's variables too. `var_labels` rides `shared` into the reg table's meta$vars for the
  # opt-in name display-swap. Idempotent / no-op for non-labelled data.
  reg_lbl_vars   <- intersect(unique(c(as.character(dependent),
                                       unlist(predictors, use.names = FALSE),
                                       as.character(split_var))), names(data))
  reg_var_labels <- capture_var_labels(data, reg_lbl_vars)
  data           <- tab_apply_val_labels(data, reg_lbl_vars)
  if (!is.null(design_obj)) design_obj$variables <- data

  # Phase 15e: `family` is resolved PER DEPENDENT, so one call can model several outcomes with
  # DIFFERENT families (one column-group per outcome). Accepts "auto" (detect each outcome), a scalar
  # (recycled to every dependent), a positional length-N vector, or a named vector keyed by dependent
  # -- mirroring `trials` / `inverse_two_level_factors`. Auto-detection stays honest and per-dependent
  # (an ambiguous integer count aborts for THAT outcome, not the whole table). The scalar `family`
  # below (= the first outcome's) is the recycled default for reg_meta / direct reg_build callers; each
  # spec carries its own family and the per-column `model_family` fmt attribute (Step D) drives the legend.
  valid_families <- c("gaussian", "binomial", "poisson", "quasipoisson", "multinomial", "ordinal")
  fam_named    <- !is.null(names(family))
  families_vec <- vapply(seq_along(dependent), function(i) {
    d <- dependent[[i]]
    f <- if (fam_named)                 family[[d]]
         else if (length(family) == 1L) family[[1]]
         else                           family[[i]]
    if (is.null(f) || (length(f) == 1L && is.na(f))) f <- "auto"
    if (identical(f, "auto")) f <- reg_detect_family(data, d)
    if (!f %in% valid_families) {
      cli::cli_abort(c("{.arg family} for {.val {d}} must be one of {.or {.val {valid_families}}}.",
                       "x" = "Got {.val {f}}."))
    }
    # DESIGN (Last Phase z3): THE resolution site for the modified-Poisson path. An explicit
    # poisson/quasipoisson on a BINARY outcome is not a count model -- it is Zou (2004)'s modified
    # Poisson, whose exp(coef) is a RISK RATIO, not an incidence-rate ratio. Resolving it to the
    # internal family key "rr" here (before `specs` are built) means the split_var recursion, the
    # multi-dependent recursion and the jamovi bridge all inherit it, and every family switch
    # downstream dispatches on ONE key. "rr" is deliberately absent from `valid_families`: a user
    # reaches it only through family = "poisson", never by typing it.
    if (f %in% c("poisson", "quasipoisson") && reg_is_binary_outcome(data[[d]])) {
      cli::cli_inform(c("i" = paste0(
        "{.val {d}} is binary: fitting a modified Poisson regression (robust standard errors) -> ",
        "{.strong risk ratios}, not incidence-rate ratios."
      )))
      f <- "rr"
    }
    f
  }, character(1))
  names(families_vec) <- dependent
  family_for   <- function(d) families_vec[[d]]
  family       <- families_vec[[1]]                          # scalar fallback (homogeneous default)
  mixed_family <- length(unique(families_vec)) > 1L

  # Phase 15e: `at = "reference"` (the MNL "j vs rest" / MER profile axis) keys on the scalar first
  # family and does not generalise across a mixed table -> degrade to "average" with a message.
  if (identical(at, "reference") && mixed_family) {
    cli::cli_inform(c("i" = paste0(
      "With several outcome families, {.code at = \"reference\"} is not supported; ",
      "using {.code at = \"average\"}.")))
    at <- "average"
  }

  # Phase 12g: survey-weighted 3+ level outcomes are supported -- ordinal via survey::svyolr, nominal
  # via svyVGAM::svy_vglm (checked in reg_check_deps). The marginaleffects paths (effect="ame", and the
  # multinomial "j vs rest" OR at the reference profile) have no method for svyolr / svy_vglm -> error.
  if (weighted && any(families_vec %in% c("multinomial", "ordinal")) &&
      (effect %in% c("ame", "ame_ratio") || at == "reference")) {
    cli::cli_abort(c(
      paste0("Marginal-effects output ({.code effect = \"ame\"}",
             if (any(families_vec == "multinomial")) ' or {.code at = "reference"}' else "",
             ") is not available for survey-weighted {.val multinomial}/{.val ordinal} models."),
      "i" = "Use the default {.code effect = \"coefficient\"} (at = \"average\"), or drop the weights."
    ))
  }

  # Last Phase z3: a marginal RATIO needs a probability to take a ratio OF -- so it is defined only for
  # probability-scale outcomes. A ratio of adjusted MEANS (gaussian) or of predicted COUNTS (poisson) is
  # unstable near zero and the "{or} ({pct})" cell would have no percentage to show. Abort naming the
  # offending outcome(s) rather than silently degrading.
  if (effect == "ame_ratio" && !all(reg_fam_prob(families_vec))) {
    bad <- dependent[!reg_fam_prob(families_vec)]
    cli::cli_abort(c(
      '{.code effect = "ame_ratio"} needs a probability-scale outcome ({.val binomial}, {.val multinomial} or {.val ordinal}).',
      "x" = "{.val {bad}}: {.val {families_vec[!reg_fam_prob(families_vec)]}}.",
      "i" = 'Use {.code effect = "ame"} (marginal effects on the response scale) for {.val gaussian} / {.val poisson} outcomes.'
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

  # `exponentiate` is logical: TRUE (default) exponentiates every family EXCEPT gaussian to ratios
  # (OR / IRR); FALSE keeps the coefficient (log / linear) scale everywhere. Gaussian is never
  # exponentiated. Legacy strings ("nongaussian"/"yes" -> TRUE, "no" -> FALSE) are still accepted so the
  # jamovi bridge and old scripts keep working.
  exp_on <- isTRUE(exponentiate) ||
    (is.character(exponentiate) && exponentiate %in% c("nongaussian", "yes"))
  do_exp       <- exp_on && family != "gaussian"
  effect_shape <- if (do_exp) "ratio" else "additive"
  eff_word     <- reg_effect_word(family, do_exp, effect, at)
  # Phase 14v: with an empirical companion, a prob-scale AME/MER cell folds in the model-adjusted
  # predicted % as "{diff} ({pct})"; name it in the header ("... AME (adjusted %)") so the parenthetical is
  # unambiguous next to the crude "Emp. %". The parenthetical is the marginal-STANDARDISED predicted
  # probability (decisions doc S50, change A/C), hence "adjusted %" not "model %". Gated on `empirical`
  # (the maintainer's disambiguation case), prob-scale families only (gaussian/poisson AME is a bare effect).
  if (effect %in% c("ame", "ame_ratio") && isTRUE(empirical) && reg_fam_prob(family)) {
    eff_word <- paste0(eff_word, " (adjusted %)")
  }
  # Phase 15e: the per-dependent versions of the above (each outcome keeps its own family shape). The
  # scalar do_exp/effect_shape/eff_word stay as the reg_build recycled defaults + the reg_meta fallback.
  do_exp_for       <- function(d) exp_on && family_for(d) != "gaussian"
  effect_shape_for <- function(d) if (do_exp_for(d)) "ratio" else "additive"
  eff_word_for     <- function(d) {
    w <- reg_effect_word(family_for(d), do_exp_for(d), effect, at)
    if (effect %in% c("ame", "ame_ratio") && isTRUE(empirical) &&
        reg_fam_prob(family_for(d)))
      w <- paste0(w, " (adjusted %)")
    w
  }

  # Phase 12h: `estimate_display` = the estimate-cell layout. "value" (plain) / "ci" (a visible interval,
  # any family) apply everywhere; the "prob"/"ame" folds (OR + adjusted probability / OR + marginal
  # effect, via reg_marginal) are probability-scale -> binomial coefficient models only. Marginal-effects
  # output (effect="ame" / the MNL "j vs rest" OR at reference) already has its own layout -> ignored.
  if (estimate_display != "value" && (effect %in% c("ame", "ame_ratio") || mnl_vsrest)) {
    cli::cli_inform(c("i" = "{.arg estimate_display} is ignored with marginal-effects output."))
    estimate_display <- "value"
  }
  # Phase 15e: the prob/ame folds are binomial-coefficient only; in a mixed table they apply to the
  # binomial outcomes and each non-binomial column degrades to the CI bracket (guarded per column in
  # reg_apply_estimate_display). Only degrade the whole call when NO outcome is a binomial coefficient.
  if (estimate_display %in% c("prob", "ame") && !(any(families_vec == "binomial") && !formula_mode)) {
    cli::cli_inform(c(
      "!" = paste0("{.arg estimate_display = \"{estimate_display}\"} needs a binomial coefficient ",
                   "model; showing the confidence interval instead.")))
    estimate_display <- "ci"
  }

  # trials -> grouped binomial (D2): a summed-score outcome fit as cbind(score, trials-score). NULL =
  # off (binary logit). TRUE = observed max per dependent. Numeric / named vector = the item count.
  # Phase 15e: applied per BINOMIAL outcome only (a non-binomial dependent ignores it).
  trials_for <- function(d) NULL
  if (!is.null(trials)) {
    if (!any(families_vec == "binomial")) {
      cli::cli_abort("{.arg trials} applies only to {.val binomial} outcomes (grouped / summed-score).")
    }
    if (formula_mode) {
      cli::cli_warn("{.arg trials} is ignored with a compound formula; write {.code cbind()} in it instead.")
    } else {
      tv <- if (isTRUE(trials))              purrr::map_dbl(dependent, ~ max(data[[.x]], na.rm = TRUE))
            else if (!is.null(names(trials))) unname(trials[dependent])
            else                              rep_len(as.numeric(trials), length(dependent))
      tv <- stats::setNames(as.integer(round(tv)), dependent)
      trials_for <- function(d) if (identical(family_for(d), "binomial")) tv[[d]] else NULL
    }
  }

  # Phase 15d: `inverse_two_level_factors` may be a NAMED logical vector (one choice per dependent) so
  # several binomial-factor outcomes can each pick which level is modelled as "success" (first level =
  # TRUE, the default). A plain scalar keeps every existing call byte-identical. inverse_for(d) resolves
  # to a scalar for each spec (unknown dependent -> the default TRUE).
  inverse_for <- if (length(inverse_two_level_factors) > 1L ||
                     !is.null(names(inverse_two_level_factors))) {
    function(d) { v <- inverse_two_level_factors[[d]]; if (is.null(v) || is.na(v)) TRUE else isTRUE(v) }
  } else {
    function(d) isTRUE(inverse_two_level_factors)
  }

  # `color` is logical-primary: TRUE (default) auto-picks the per-family measure below; FALSE turns
  # every column (model AND empirical companion) uncoloured. The string measures ("OR"/"diff"/"ratio"/
  # "no") stay accepted as a power-user escape hatch (documented as such). NULL == TRUE (auto).
  if (isTRUE(color) || is.null(color)) color <- NA_character_   # sentinel: auto-derive just below
  else if (isFALSE(color))             color <- "no"
  # base `%||%` is R >= 4.4 only; the package supports R >= 4.1, so use explicit is.null()/is.na().
  # effect="ame" always colours the marginal effect as a difference (neutral 0), never as a ratio.
  # `color[1]`: since Last Phase z5 the measure may be a length-2 (text, background) vector, e.g. the
  # headline `color = c("OR", "adjustment")`. Only the text channel carries the auto sentinel.
  color_auto <- is.na(color[1])                                 # Phase 15e: remember the auto sentinel

  # Last Phase z5: VALIDATE the measure(s) through the storage boundary itself rather than repeating
  # its rules here -- fmt() casts `color` without validating, so tab_reg would otherwise accept an
  # unknown measure, a whole-cell measure on the background, or the two mutually exclusive `obs`
  # measures together, and only fail (or silently mis-colour) much later. The result is discarded: the
  # canonical form is applied per column by fmt()/set_color as before.
  if (!color_auto) invisible(resolve_color_channels(color))

  # Last Phase z5: `adjustment` scores the model effect against its OBSERVED counterpart, which lives in
  # the `obs` field only when the crude companion was computed -- so asking for the colour asks for
  # `empirical`. Same shape as color = "contrib" forcing chi2 + totrow in the resolve cascade
  # (R/tab-resolve.R): the user states an intent, the pipeline computes what it needs.
  if ("adjustment" %in% color && !isTRUE(empirical)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"adjustment\"} compares each model effect to its ",
                                   "observed one, so {.code empirical = TRUE} is turned on.")))
    empirical <- TRUE
  }
  # Last Phase z8: at the reference profile the model cell is a marginal effect AT that profile while
  # the observed columns stay marginal over the whole sample -- comparable side by side as description,
  # but not cell by cell, so no `obs` is written and the gap colours stay off (see reg_build).
  if (isTRUE(empirical) && identical(at, "reference")) {
    cli::cli_inform(c("i" = paste0("{.code at = \"reference\"} evaluates the model at the reference ",
                                   "profile, while the observed columns stay marginal over the whole ",
                                   "sample: the two are shown side by side, but not compared cell by ",
                                   "cell ({.code color = \"adjustment\"} and {.code \"{{obs}}\"} stay empty).")))
  }
  # Last Phase z8: `between_groups` now HAS a test of its own (the two split groups are disjoint, so the
  # gap SE is exact by quadrature -- reg_write_group_gap), and reads `color_signif` normally.
  # `adjustment` compares two estimates fitted on the SAME rows, whose joint variance needs influence
  # functions (dev/model_vs_observed_gap_test.md SS3): still neutralised by MEASURES$force_policy, and
  # said once rather than letting a `color_signif` look effective.
  # Last Phase z8: `between_groups` also gets the AGGREGATED companion of its per-cell colours -- one
  # pooled interaction test per predictor, in the footer. Automatic here for discoverability (and
  # because the two readings belong together); `stats = c(..., "interaction")` asks for it without the
  # colours. It costs one extra model fit per model, so say so.
  if ("between_groups" %in% color && !is.null(split_var) &&
      !(is.character(stats) && "interaction" %in% stats)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"between_groups\"} also adds the aggregated ",
                                   "interaction test to the footer (one extra model fit). Ask for it ",
                                   "without the colours with {.code stats = c(..., \"interaction\")}.")))
  }
  # Last Phase z8-B: `adjustment` now HAS a test (the difference of the two estimators' influence
  # functions -- R/reg-influence.R), on every COLLAPSIBLE estimand. On a conditional odds ratio it is
  # deliberately not computed: adjusting an OR moves it away from 1 even with zero confounding, so the
  # test is a valid test of a statement no reader makes (measured rejection 1.000 at n = 32000 with the
  # covariate independent of the exposure -- dev/model_vs_observed_gap_test.md SS4.1). Say it once, and
  # name the three routes that make the same comparison collapsible, rather than let a `color_signif`
  # look effective. The colours stay descriptive there (MEASURES' force_policy predicate).
  if ("adjustment" %in% color && !is.null(color_signif) && !identical(color_signif, "ignore") &&
      !any(vapply(families_vec, reg_estimand_collapsible, logical(1), effect = effect))) {
    cli::cli_inform(c("i" = paste0("{.arg color_signif} does not apply to an odds-ratio {.val adjustment} ",
                                   "gap: part of it is non-collapsibility, not confounding. Use ",
                                   "{.code effect = \"ame\"} or {.code \"ame_ratio\"}, or ",
                                   "{.code family = \"poisson\"} (risk ratios), for a gap the test can ",
                                   "read.")))
  }
  # Last Phase z8-B: the gap test needs BOTH estimates computed on the same observations. With the
  # default per-model NA drop a compared model can be fitted on more rows than the shared observed
  # block, so those columns keep the descriptive colour -- said once here (the build cannot, it recurses
  # per split group), on the cheap pre-condition rather than after the fact.
  if ("adjustment" %in% color && identical(na, "drop_by_model") &&
      (is_comparison || length(dependent) > 1L)) {
    cli::cli_inform(c("i" = paste0("Each model uses its own complete cases, so a model fitted on more ",
                                   "rows than the observed columns keeps the descriptive gap colour. ",
                                   "Use {.code na = \"drop_all_models\"} to compare them on one ",
                                   "population.")))
  }
  # Last Phase z3: an explicit ladder, not `effect != "ame"`. A marginal RATIO is multiplicative
  # whatever `exponentiate` says (which is ignored for marginal effects), so keying off `effect_shape`
  # alone would colour an ame_ratio column on the 0-centred difference scale under exponentiate = FALSE.
  color_auto_measure <- function(shape) if (effect == "ame") "diff"
    else if (effect == "ame_ratio") "OR"
    else if (shape == "ratio") "OR" else "diff"
  if (color_auto)            color        <- color_auto_measure(effect_shape)
  if (is.null(color_signif)) color_signif <- "grey_non_signif"
  # Phase 15e: the per-dependent auto colour measure (each family its own default). An explicit user
  # `color=` (string / c(text, bg)) is not auto -> applied to every column unchanged.
  color_for <- function(d) if (color_auto)
    color_auto_measure(effect_shape_for(d)) else color

  all_predictors <- if (is_comparison) unique(purrr::flatten_chr(predictors)) else predictors

  # Phase 14u (L2): na = "drop_all_models" fits every model on ONE shared complete-case population -- the
  # union of all predictors + the dependent + design vars -- so genuinely-nested models get EQUAL N and
  # the LR comparison fires (the default per-model "drop_by_model" NA drop can make N differ -> the AIC
  # fallback). It CHANGES all estimates (shared population), hence opt-in. Not applied to a prebuilt
  # survey design (subsetting is the design's own concern).
  if (identical(na, "drop_all_models") && !formula_mode) {
    if (!is.null(design_obj)) {
      cli::cli_inform(c("i" = '{.code na = "drop_all_models"} is ignored for a prebuilt survey design.'))
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
  # multinomial / ordinal / split / trials / model comparison) keeps the refit path.
  # Last Phase z9: `multiplier` LEFT that list. The digest is fitted natively (reg_build_digest passes
  # multiplier = NULL), so it is multiplier-independent just as it is reference-independent, and
  # reg_reref_fit_res() applies the scaling itself -- a scaling change is now a cache HIT. Keeping the
  # clause would silently kill the fast path for every table with a numeric predictor once "sd" becomes
  # the default, which is the regression Phase 15b exists to prevent.
  # Phase 15e: an all-glm mixed table keeps the digest fast-path (each spec caches its own family's
  # digest); any multinomial/ordinal outcome degrades the whole table to the cached raw-fit path.
  # Last Phase z8-B: `color = "adjustment"`'s gap test needs the FITTED object (influence functions),
  # which the digest deliberately discards -- so asking for it takes the refit path rather than getting
  # a silently untested colour. One clause, not a rebuild-from-coef arm: jamovi's reg `color` is a
  # checkbox (jamovi/jmvtabreg.a.yaml), so no live-UI call can reach here with the measure today, and
  # building the arm would mean a second encoding of reg_fit()'s model frame for no caller. The recipe
  # is in dev/model_vs_observed_gap_test.md SS6 if the option ever becomes a list.
  reref <- !is.null(.fit_cache) && effect == "coefficient" && !mnl_vsrest &&
    estimate_display %in% c("value", "ci") && method == "wald" &&
    all(families_vec %in% c("gaussian", "binomial", "poisson", "quasipoisson", "rr")) &&
    !formula_mode && is.null(split_var) && is.null(trials) &&
    compare == "none" && !is_comparison && !("adjustment" %in% color)

  if (!is.null(reference) && !reref) {
    # A multinomial's baseline is the OUTCOME factor's first level, so `reference` keyed by the
    # dependent relevels it too (unified "reference level of any variable"). An ordinal outcome must
    # keep its order -> never releveled; predictor contrasts are releveled for every family. Phase 15e:
    # relevel every predictor + the MULTINOMIAL outcomes (per-dependent family).
    relevelable <- union(all_predictors, dependent[families_vec == "multinomial"])
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
    # Model comparison is single-dependent -> single family; carry the scalar family shape on each spec.
    specs  <- purrr::map2(models, labels,
                          ~ list(dependent = dependent, predictors = .x, label = .y,
                                 trials = trials_for(dependent), inverse = inverse_for(dependent),
                                 compound = FALSE, formula = NULL,
                                 family = family, do_exp = do_exp, effect_shape = effect_shape,
                                 eff_word = eff_word, color = color,
                                 crude_key = reg_crude_key(family, trials_for(dependent), FALSE)))
    union_predictors <- reg_order_union(models)          # Phase 14u (L1): complete-model order if any
  } else {
    labels <- purrr::map_chr(dependent, function(d) {
      # a summed-score / compound-formula binomial has no single "positive level" -> label by name
      base <- if (reg_fam_binary(family_for(d)) && !formula_mode && is.null(trials_for(d))) {
        pl <- reg_cleanup(reg_positive_level(data, d, inverse_for(d)), cleannames)
        pl
      } else d
      paste0(base, ": ", eff_word_for(d))
    })
    labels <- make.unique(labels)
    # Phase 15e: each spec carries its OWN resolved family shape (family / do_exp / effect_shape /
    # eff_word / color), so reg_build builds a mixed-family table one column-group per outcome.
    specs  <- purrr::map2(dependent, labels,
                          ~ list(dependent = .x, predictors = predictors, label = .y,
                                 trials = trials_for(.x), inverse = inverse_for(.x),
                                 compound = formula_mode, formula = raw_formula,
                                 family = family_for(.x), do_exp = do_exp_for(.x),
                                 effect_shape = effect_shape_for(.x), eff_word = eff_word_for(.x),
                                 color = color_for(.x),
                                 crude_key = reg_crude_key(family_for(.x), trials_for(.x),
                                                           formula_mode)))
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

  # multiplier (Phase 12g; grammar + resolution Last Phase z9): scale a CONTINUOUS predictor's effect to
  # per-k units (OR^k / beta*k). A SCALAR ("sd" / "2sd" / a number) applies to every numeric predictor; a
  # NAMED vector overrides per variable and the rest keep the scalar default. Resolved ONCE here, into
  # frozen numbers -- see reg_resolve_multiplier() for why the frame excludes the dependent.
  # `mult_default` = the argument's own default reaching us untouched. The multinomial/ordinal guards
  # answer "you asked for something that cannot happen", so they must stay silent for a default nobody
  # asked for -- otherwise every multinomial table would abort. (A user who types the default value
  # explicitly on an all-multinomial table simply gets no scaling, which is the only thing there is.)
  mult_default <- identical(multiplier, "sd")
  if (!is.null(multiplier)) {
    if (!(is.numeric(multiplier) || is.character(multiplier)) || length(multiplier) == 0L) {
      cli::cli_abort(c(
        "{.arg multiplier} must be a number, {.val sd}, {.val 2sd}, or a named vector of those.",
        "i" = 'e.g. {.code multiplier = "sd"}, {.code c(age = 10)}, {.code c(age = "2sd")}.'))
    }
    # Phase 15e: multiplier scales glm-family coefficients; abort only when EVERY outcome is
    # multinomial/ordinal (nothing to scale). In a mixed table it applies to the glm outcomes.
    if (!mult_default && all(families_vec %in% c("multinomial", "ordinal"))) {
      cli::cli_abort("{.arg multiplier} is not supported for {.val multinomial}/{.val ordinal} models.")
    }
    if (!mult_default && any(families_vec %in% c("multinomial", "ordinal"))) {
      cli::cli_inform(c("i" = paste0(
        "{.arg multiplier} scales the glm-family outcomes only; the multinomial/ordinal ",
        "outcome{?s} are shown unscaled.")))
    }
    if (!is.null(names(multiplier))) {
      bad <- setdiff(names(multiplier), reg_numeric_preds(data, all_predictors))
      if (length(bad) > 0L) {
        cli::cli_abort(c("{.arg multiplier} names must be numeric predictors.",
                         "x" = "Not numeric predictor{?s}: {.val {bad}}."))
      }
    }
  }
  # The frozen frame: complete on the PREDICTORS + design variables, never on the dependent -- so one
  # predictor keeps one unit across outcomes, compared models and split groups. Never applied in
  # compound-formula mode, where a bare RHS name may carry an interaction or a basis expansion and
  # reg_fit()'s `td$term == v` match would scale the main effect while leaving `age:raceWhite` alone.
  # Last Phase z9: "sd" is the DEFAULT scalar. Per 1 unit a numeric predictor sits inside the first
  # colour break and reads as "no effect" beside the factor contrasts next to it; per 1 SD it lands on
  # the same visual scale. `multiplier = 1` restores the per-unit reading.
  mult_scalar_default <- "sd"
  mult_res <- if (formula_mode || !any(families_vec %in% c("gaussian", "binomial", "poisson",
                                                           "quasipoisson", "rr"))) {
    list(k = NULL, label = NULL)
  } else {
    num_preds_all <- reg_numeric_preds(data, all_predictors)
    sd_frame <- reg_complete_frame(
      data, intersect(unique(c(all_predictors, wt, ids, strata, fpc)), names(data)))
    reg_resolve_multiplier(multiplier, mult_scalar_default, sd_frame, num_preds_all, wt = wt)
  }
  multiplier       <- mult_res$k
  multiplier_label <- mult_res$label

  # empirical (Phase 12g / 14v): the descriptive crude companion beside the model effect -- the
  # unadjusted bivariate association (which IS the modelised quantity when there is a single predictor).
  # Wired for binomial / gaussian / poisson (explicit columns) and multinomial (tooltip only). A vector
  # of dependents is supported (crude companion per dependent). Ordinal (cumulative OR) has no clean
  # crude analogue -> a message, not an error, and `empirical` is dropped for this call.
  # Phase 15e: kept ON whenever ANY outcome supports a crude companion (the per-fit loop skips the
  # ineligible outcomes -- ordinal -- individually). Only dropped when NO outcome is eligible.
  # Last Phase z10: one stored fact, not a third hand-written family list. What is left with no crude
  # counterpart at all is the compound-formula escape hatch (no predictor structure to be crude about).
  if (isTRUE(empirical) &&
      !any(purrr::map_lgl(dependent, ~ !is.na(reg_crude_key(family_for(.x), trials_for(.x),
                                                            formula_mode))))) {
    cli::cli_inform(c("i" = paste0(
      "{.arg empirical} (crude descriptive companion) is not available for any of these outcome ",
      "families; ignored here.")))
    empirical <- FALSE
  }

  design_spec <- list(design = design_obj, wt = wt, ids = ids, strata = strata, fpc = fpc, nest = nest)
  # Phase 15e: check the Suggests deps of EVERY family present (nnet for multinomial, MASS for ordinal...).
  for (fm in unique(families_vec))
    reg_check_deps(fm, weighted, needs_marginaleffects = effect %in% c("ame", "ame_ratio") || mnl_vsrest ||
                     estimate_display %in% c("prob", "ame"))
  # Phase 17h: every per-call setting reg_build's leaves + assembler read, bundled once (the specs carry
  # the per-dependent family/do_exp/effect_shape/eff_word/color, so those scalars are no longer threaded).
  shared <- list(
    union_predictors = union_predictors, design_spec = design_spec, weighted = weighted,
    inverse_two_level_factors = inverse_two_level_factors, conf_level = conf_level, method = method,
    color_signif = color_signif, cleannames = cleannames, subtext = subtext, effect = effect, at = at,
    stats = stats, compare = compare, baseline = baseline, multiplier = multiplier,
    multiplier_label = multiplier_label,
    empirical = empirical, estimate_display = estimate_display, spread_models = spread_models,
    var_labels = reg_var_labels)
  res <- reg_build(data, specs, shared, split_var = split_var,
                   .fit_cache = .fit_cache, reference = reference, reref = reref)

  # stars = TRUE (default) for regression tables -- the per-cell pvalue is stored by reg_build so the
  # main display shows significance stars. stars = FALSE strips it (pvalue is stars-only; colours read
  # the CI bounds), so the table renders without stars.
  if (!isTRUE(stars)) {
    for (nm in names(res)[vapply(res, is_fmt, logical(1))]) {
      res[[nm]] <- set_pvalue(res[[nm]], NA_real_)
    }
  }

  # Phase 14w / 15e: the table's own model record (drives the reg title / caption, the "Model:" footer
  # lines, and the colour legend). `families` is per dependent (the mixed-family case); the per-column
  # effect word is read from the column's own `model_family` fmt attribute (Step D) in the legend, so this
  # record is only the table-level narrative. `family`/`do_exp`/`eff_word` stay scalar = the first outcome.
  positive_levels <- purrr::map_chr(dependent, function(d) {
    if (!reg_fam_binary(family_for(d)) || formula_mode || !is.null(trials_for(d))) return(NA_character_)
    pl <- reg_positive_level(data, d, inverse_for(d))
    pl <- reg_cleanup(pl, cleannames)
    pl
  })
  # Phase 16d: the weight column NAME (or NA) drives the footer "Weighted by <wt>." line. `wt` is a
  # character column name or a formula (reg_design_formula accepts both); a prebuilt design carries its
  # own weights and cannot be named -> NA.
  wt_disp <- if (is.null(wt) || (length(wt) == 1L && is.na(wt))) NA_character_
             else if (rlang::is_formula(wt)) all.vars(wt)[1]
             else as.character(wt)[1]
  reg_meta <- list(
    family = family, families = families_vec, exponentiate = exp_on,
    effect = effect, at = at, do_exp = do_exp, eff_word = eff_word,
    dependent = dependent, positive_level = positive_levels, predictors = union_predictors,
    # Last Phase z9: the predictor-kind map is STORED, not re-derived from the rendered table. Nothing
    # recorded it before, and the only implicit marker (a numeric row's `level == var`) is already
    # broken by `cleannames` and by the multiplier relabel. `multiplier` records the RESOLVED per-unit
    # scaling actually used (the frozen SDs included), so the footer/legend can name the unit.
    predictor_types = reg_predictor_types(data, union_predictors), multiplier = multiplier,
    # Last Phase z10: which observed counterpart each outcome has (NA = none). Stored, so the footer can
    # word the in-cell "{or} ({obs})" bracket and ?tab_reg can state the scope honestly.
    crude_keys = if (isTRUE(empirical))
      stats::setNames(purrr::map_chr(specs, ~ .$crude_key), purrr::map_chr(specs, "dependent"))
      else stats::setNames(rep(NA_character_, length(specs)), purrr::map_chr(specs, "dependent")),
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
                      inverse_two_level_factors = TRUE, split_var = NULL, multiplier = "sd",
                      empirical = FALSE,
                      conf_level = getOption("tabxplor.conf_level", 0.95),
                      method = c("wald", "profile"),
                      stats = NULL, estimate_display = c("value", "ci", "prob", "ame"),
                      color_signif = c("grey_non_signif", "ignore", "guaranteed_effect"),
                      stars = TRUE, na = c("drop_by_model", "drop_all_models"),
                      cleannames = NULL, subtext = "", spread_models = TRUE) {
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
          cleannames = cleannames, subtext = subtext, spread_models = spread_models)
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
                        inverse_two_level_factors = TRUE, split_var = NULL, multiplier = "sd",
                        empirical = FALSE,
                        conf_level = getOption("tabxplor.conf_level", 0.95),
                        method = c("wald", "profile"),
                        stats = NULL, compare = c("none", "baseline", "sequential"), baseline = NULL,
                        estimate_display = c("value", "ci", "prob", "ame"),
                        color_signif = c("grey_non_signif", "ignore", "guaranteed_effect"),
                        stars = TRUE, na = c("drop_by_model", "drop_all_models"),
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
