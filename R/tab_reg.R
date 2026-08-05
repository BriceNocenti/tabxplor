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
# See: CLAUDE.md Phase 12c-12g ; dev/tabxplor_2.0.0_decisions.md S37.

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
reg_model_note <- function(family, do_exp, effect = "coefficient", at = "average") {
  # Last Phase z3: the ratio twin of the AME phrase. Guarded to prob-scale families upstream, so the
  # "adjusted predicted probability" wording always applies. Name the quantity a RATIO OF PROBABILITIES,
  # never a "log-linear model" -- in sociology that phrase means Goodman's contingency-table models.
  if (effect == "ame_ratio") {
    where <- if (at == "reference")
      gettext(" at the reference profile (other predictors held at their reference level / mean)")
    else gettext(" (sample-averaged)")
    return(paste0(gettext("marginal risk ratios (the ratio of adjusted predicted probabilities)"), where,
                  gettext("; each cell shows the ratio vs the reference level and, in parentheses, the adjusted predicted probability")))
  }
  if (effect == "ame") {
    prob  <- reg_fam_prob(family)
    where <- if (at == "reference")
      gettext(" at the reference profile (other predictors held at their reference level / mean)")
    else gettext(" (sample-averaged)")
    return(if (prob)
      paste0(gettext("marginal effects on the probability scale (percentage points)"), where,
             gettext("; each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability"))
    else
      paste0(gettext("marginal effects on the response scale"), where))
  }
  if (at == "reference" && family == "multinomial") {
    return(gettext("odds ratios of each outcome category versus the rest, at the reference profile (other predictors held at their reference level / mean); profile-conditional"))
  }
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
reg_model_line <- function(meta) {
  if (is.null(meta)) return(NULL)
  fam <- reg_family_display_name(meta$family)
  est <- reg_model_note(meta$family, meta$do_exp, meta$effect, meta$at)
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
      est   <- reg_model_note(fm, dox, meta$effect, meta$at)
      enc2utf8(if (nzchar(est)) gettextf("Model (%s): %s; %s.", legend_name_list(grp), fname, est)
               else            gettextf("Model (%s): %s.", legend_name_list(grp), fname))
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
                    trials = NULL, formula = NULL, multiplier = NULL) {
  drop_vars <- unique(c(dependent, predictors, reg_design_vars(design_spec)))
  mdata     <- reg_complete_frame(data, drop_vars)

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
    stats::as.formula(paste0(
      resp, " ~ ", paste0("`", predictors, "`", collapse = " + ")
    ))
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
                                       numeric_preds, model_predictors) {
  if (mode == "value") return(col)
  if (mode == "ci")    return(set_display(col, "est_ci"))
  # Phase 15e: the prob/ame folds need a binomial coefficient model; a non-binomial column of a mixed
  # table shows the CI bracket instead (the whole-call degrade only fires when NO outcome is binomial).
  if (mode %in% c("prob", "ame") && !identical(family, "binomial")) return(set_display(col, "est_ci"))

  marg     <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                           at = "average", want_pred = mode == "prob")
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
reg_empirical <- function(data, fac_preds, dependent, family, positive_level, wt) {
  w  <- if (is.null(wt)) rep(1, nrow(data)) else as.numeric(data[[wt]])
  yv <- data[[dependent]]
  bin <- reg_fam_binary(family)
  # Last Phase s: the crude companion CIs honour Kish n_eff like tab()'s descriptive CIs (opt-in +
  # weighted). We carry a SEPARATE effective-n (`n_ci`) for the interval; the displayed `n` stays the
  # raw unweighted count. Off-kish/unweighted n_ci == the raw count -> byte-identical.
  kish <- !is.null(wt) && isTRUE(getOption("tabxplor.kish_neff", FALSE))
  neff_or_n <- function(wsum, w2, raw) {
    if (!kish) return(as.double(raw))
    ne <- wsum^2 / w2
    if (is.finite(ne)) ne else as.double(raw)
  }
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
        n_ci <- neff_or_n(wpos + wneg, sum(w[m]^2), sum(m))
        list(base = wpos / (wpos + wneg), ratio_raw = wpos / wneg, var = NA_real_,
             n = sum(m), n_ci = n_ci)
      } else {
        n1 <- sum(m); wn <- sum(w[m]); s1 <- sum(w[m] * ynum[m]); s2 <- sum(w[m] * ynum[m]^2)
        mean_l <- s1 / wn
        # match tab()/num_derive_stats: unweighted -> stats::var (n-1), weighted -> ML (s2/wn - mean^2).
        # 14v-ii: poisson gets the count variance too (drives its crude rate-ratio CI, ci_mean_ratio).
        var_l  <- if (family %in% c("gaussian", "poisson", "quasipoisson")) {
          if (is.null(wt)) (s2 - s1^2 / n1) / (n1 - 1) else round(s2 / wn - (s1 / wn)^2, 10)
        } else NA_real_
        list(base = mean_l, ratio_raw = mean_l, var = var_l,
             n = n1, n_ci = neff_or_n(wn, sum(w[m]^2), n1))
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
      emp_n_ci  = purrr::map_dbl(per, "n_ci"),
      emp_ref_base = ref$base, emp_ref_var = ref$var, emp_ref_n = as.integer(ref$n),
      emp_ref_n_ci = ref$n_ci
    )
  })
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
REG_EMPIRICAL <- list(
  binomial = list(
    method_diff = "wald",
    base   = list(nm = "Obs_%",       type = "row",  display = "pct",  digits = 0L, ref = "tot",           ci_type = "diff",  color = "diff"),
    ame    = list(nm = "Obs_diff",    type = "row",  display = "diff", digits = 0L, ref = "tot",           ci_type = "diff",  color = "diff"),
    or     = list(nm = "Obs_OR",      type = "row",  display = "or",   digits = 2L, ref = "1",             ci_type = "or",    color = "OR"),
    or_log = list(nm = "Obs_log(OR)", type = "coef", display = "coef", digits = 2L, ref = NA_character_,   ci_type = "diff",  color = "diff")),
  # Last Phase z3 -- the modified-Poisson (binary outcome) crude companion. SAME base column as binomial
  # (a risk, `Obs_%`, with the Wald risk-difference CI), but the effect is a crude RISK ratio with the
  # KATZ log-RR interval (ci_katz_rr) -- not the Woolf log-OR the binomial arm uses. That is the point
  # of the whole feature: the observed companion must be on the same scale as the model column.
  rr = list(
    method_diff = "wald",
    base   = list(nm = "Obs_%",       type = "row",  display = "pct",  digits = 0L, ref = "tot",           ci_type = "diff",  color = "diff"),
    ame    = list(nm = "Obs_diff",    type = "row",  display = "diff", digits = 0L, ref = "tot",           ci_type = "diff",  color = "diff"),
    rr     = list(nm = "Obs_RR",      type = "row",  display = "or",   digits = 2L, ref = "1",             ci_type = "or",    color = "OR"),
    rr_log = list(nm = "Obs_log(RR)", type = "coef", display = "coef", digits = 2L, ref = NA_character_,   ci_type = "diff",  color = "diff")),
  gaussian = list(
    method_mean_diff = "student",
    base = list(nm = "Obs_mean", type = "mean", display = "mean", digits = 2L, ref = NA_character_,  ci_type = "cell",  color = ""),
    diff = list(nm = "Obs_diff", type = "coef", display = "coef", digits = 2L, ref = NA_character_,  ci_type = "diff",  color = "diff")),
  poisson = list(
    method_mean_ratio = "quasipoisson",
    base    = list(nm = "Obs_rate",     type = "mean", display = "mean", digits = 2L, ref = "1",           ci_type = "ratio", color = "ratio"),
    irr     = list(nm = "Obs_IRR",      type = "row",  display = "or",   digits = 2L, ref = "1",           ci_type = "or",    color = "OR"),
    irr_log = list(nm = "Obs_log(IRR)", type = "coef", display = "coef", digits = 2L, ref = NA_character_, ci_type = "diff",  color = "diff"))
)

# The base+effect fmt columns aligned to the skeleton, for reg_build to prepend before the model column.
# Numeric predictors / the Constant -> empty cells; reference levels -> neutral + in_refrow, no CI. want_p
# is TRUE (the pvalue is stored; stars are stripped post-build when stars = FALSE, like the model columns).
reg_empirical_columns <- function(skeleton, emp, fac_preds, family, effect, var_y,
                                  conf_level = 0.95, color_signif = "grey_non_signif",
                                  color = NULL, do_exp = TRUE) {
  # quasipoisson shares the poisson crude shapes/CI (rate + IRR, quasi-Poisson interval); model_family
  # below stays the real family so the legend still words it as the model's own.
  fam_key <- if (family == "quasipoisson") "poisson" else family
  fam <- REG_EMPIRICAL[[fam_key]]
  if (is.null(fam)) return(list())            # multinomial is tooltip-only; ordinal unsupported
  # Phase 15d: when the model is uncoloured (`color = FALSE` -> "no"), the crude companions must be
  # uncoloured too (else the table shows coloured empirical columns beside plain model columns).
  emp_off <- !is.null(color) && color %in% c("no", "")
  mi      <- reg_skel_match(skeleton, emp)
  n_rows  <- nrow(skeleton)
  is_fac  <- skeleton$var %in% fac_preds
  refrows <- skeleton$is_ref & is_fac
  base <- emp$emp_base[mi]; diffv <- emp$emp_diff[mi]; ratio <- emp$emp_ratio[mi]
  varv <- emp$emp_var[mi];  nv    <- emp$emp_n[mi]
  rb   <- emp$emp_ref_base[mi]; rv <- emp$emp_ref_var[mi]; rn <- emp$emp_ref_n[mi]
  # Last Phase s: the CI base is the effective n (Kish n_eff, opt-in) -- off-kish it equals the raw
  # count, so the intervals are byte-identical. The displayed n/tot_n fields keep the raw count `nv`.
  nv_ci <- emp$emp_n_ci[mi]; rn_ci <- emp$emp_ref_n_ci[mi]
  # a reference level has no CI/test against itself (like the model column's zeroed reference).
  na_ref <- function(ci) { ci$inf[refrows] <- NA_real_; ci$sup[refrows] <- NA_real_
                           ci$pvalue[refrows] <- NA_real_; ci }
  # one fmt column from a shape row + its varying fmt FIELD values. Uncoloured when the model is off or
  # the shape declares no measure (Emp. mean); `ref` is omitted when the shape has none.
  emp_col <- function(shape, fields) {
    measure <- if (emp_off || !nzchar(shape$color)) "" else shape$color
    args <- c(fields, list(
      type = shape$type, display = shape$display, digits = shape$digits, ci_type = shape$ci_type,
      color = measure, color_signif = if (nzchar(measure)) color_signif else "ignore",
      col_var = shape$nm, comp_all = FALSE, in_refrow = refrows, model_family = family, role = "emp"))
    if (!is.na(shape$ref)) args$ref <- shape$ref
    do.call(fmt, args)
  }
  two <- function(a, b) stats::setNames(list(a$col, b$col), c(a$shape$nm, b$shape$nm))

  # binomial + "rr" (modified Poisson) share every BASE fact -- a crude risk and its Wald risk-difference
  # CI -- and differ only in the crude EFFECT, which must be the model's own estimand (Last Phase z3).
  if (reg_fam_binary(family)) {
    rd <- na_ref(ci_prop_diff(base, nv_ci, rb, rn_ci, conf_level = conf_level,    # crude risk-difference
                              method = fam$method_diff, want_p = TRUE))
    rd_fields <- list(pct = base, diff = diffv, n = nv, tot_n = nv,
                      ci_inf = rd$inf, ci_sup = rd$sup, pvalue = rd$pvalue)
    base_col <- emp_col(fam$base, rd_fields)
    if (effect == "ame")               # the AME shows a difference, not an OR -> crude risk-difference
      return(two(list(col = base_col, shape = fam$base),
                 list(col = emp_col(fam$ame, rd_fields), shape = fam$ame)))
    # Last Phase z3: a marginal RATIO's crude twin is the crude RISK ratio with the Katz log-RR interval
    # -- on the binomial model path as well as the "rr" one, since the estimand is what must match, not
    # the fitted family. Always exponentiated: `exponentiate` is ignored for marginal effects. The Obs_RR
    # shape is defined once, in REG_EMPIRICAL$rr, and reused here rather than duplicated per family.
    if (effect == "ame_ratio") {
      rr_ci <- na_ref(ci_katz_rr(base, nv_ci, rb, rn_ci, conf_level = conf_level, want_p = TRUE))
      sh    <- REG_EMPIRICAL$rr$rr
      return(two(list(col = base_col, shape = fam$base),
                 list(col = emp_col(sh, list(or = base / rb, n = nv, ci_inf = rr_ci$inf,
                                             ci_sup = rr_ci$sup, pvalue = rr_ci$pvalue)),
                      shape = sh)))
    }
    # binomial -> the crude ODDS ratio (emp_ratio = wpos/wneg vs the reference's) with the Woolf log-OR
    # interval. "rr" -> the crude RISK ratio (base/rb) with the Katz log-RR interval. WARNING: `ratio`
    # (emp_ratio) is an ODDS ratio -- feeding it to an Obs_RR column would print an OR under an RR header.
    is_rr  <- family == "rr"
    eff_v  <- if (is_rr) base / rb else ratio
    eff_ci <- na_ref(if (is_rr)
      ci_katz_rr(base, nv_ci, rb, rn_ci, conf_level = conf_level, want_p = TRUE)
    else
      ci_or(base * nv_ci, (1 - base) * nv_ci,
            rb * rn_ci, (1 - rb) * rn_ci, conf_level = conf_level, want_p = TRUE))
    sh_exp <- if (is_rr) fam$rr     else fam$or
    sh_log <- if (is_rr) fam$rr_log else fam$or_log
    if (do_exp) {
      eff_col <- emp_col(sh_exp, list(or = eff_v, n = nv, ci_inf = eff_ci$inf,
                                      ci_sup = eff_ci$sup, pvalue = eff_ci$pvalue))
      return(two(list(col = base_col, shape = fam$base), list(col = eff_col, shape = sh_exp)))
    }
    # Phase g: exponentiate = FALSE -> the crude companion is the LOGGED effect (Obs_log(OR) /
    # Obs_log(RR)): the log ratio in the `diff` field with the logged CI, i.e. the exact Wald interval
    # on the log scale -- the same link scale as the raw model coefficient.
    eff_col <- emp_col(sh_log, list(diff = log(eff_v), n = nv, ci_inf = log(eff_ci$inf),
                                    ci_sup = log(eff_ci$sup), pvalue = eff_ci$pvalue))
    return(two(list(col = base_col, shape = fam$base), list(col = eff_col, shape = sh_log)))
  }

  if (family == "gaussian") {
    cell <- ci_pivot(base, sqrt(varv / nv_ci), df = nv_ci - 1, conf_level = conf_level, want_p = FALSE)
    base_col <- emp_col(fam$base, list(mean = base, var = varv, n = nv, tot_n = nv,
                                       ci_inf = cell$inf, ci_sup = cell$sup))
    md <- na_ref(ci_mean_diff2(base, varv, nv_ci, rb, rv, rn_ci, method = fam$method_mean_diff,  # pooled t = OLS
                               conf_level = conf_level, want_p = TRUE))
    eff_col <- emp_col(fam$diff, list(diff = diffv, var = rep(var_y, n_rows), n = nv,
                                      ci_inf = md$inf, ci_sup = md$sup, pvalue = md$pvalue))
    return(two(list(col = base_col, shape = fam$base), list(col = eff_col, shape = fam$diff)))
  }

  if (fam_key == "poisson") {
    # one crude rate-ratio CI (quasi-Poisson, = the phi-scaled model's method) drives BOTH columns.
    rr <- na_ref(ci_mean_ratio(base, varv, nv_ci, rb, rv, rn_ci, method = fam$method_mean_ratio,
                               conf_level = conf_level, want_p = TRUE))
    base_col <- emp_col(fam$base, list(mean = base, ratio = ratio, n = nv, tot_n = nv,
                                       ci_inf = rr$inf, ci_sup = rr$sup, pvalue = rr$pvalue))
    if (do_exp) {
      eff_col <- emp_col(fam$irr, list(or = ratio, n = nv, ci_inf = rr$inf,
                                       ci_sup = rr$sup, pvalue = rr$pvalue))
      return(two(list(col = base_col, shape = fam$base), list(col = eff_col, shape = fam$irr)))
    }
    # Phase g: exponentiate = FALSE -> the crude companion is Obs_log(IRR): log(rate-ratio) in `diff`
    # with the logged rate-ratio CI (the same link scale as the raw Poisson coefficient).
    eff_col <- emp_col(fam$irr_log, list(diff = log(ratio), n = nv, ci_inf = log(rr$inf),
                                         ci_sup = log(rr$sup), pvalue = rr$pvalue))
    return(two(list(col = base_col, shape = fam$base), list(col = eff_col, shape = fam$irr_log)))
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
  # Last Phase s: the crude tooltip CIs honour Kish n_eff too (opt-in); `n` stays the raw count.
  kish <- !is.null(wt) && isTRUE(getOption("tabxplor.kish_neff", FALSE))
  purrr::map_dfr(fac_preds, function(p) {
    x  <- data[[p]]
    ok <- !is.na(x) & !is.na(yv) & !is.na(w)
    lv <- levels(forcats::fct_drop(as.factor(x[ok])))
    grid <- purrr::map_dfr(lv, function(l) {
      m  <- ok & x == l; wl <- sum(w[m])
      ne <- if (kish) wl^2 / sum(w[m]^2) else NA_real_
      tibble::tibble(level = l, category = cats, n = sum(m),
                     n_ci = if (is.finite(ne)) ne else as.double(sum(m)),
                     prop = purrr::map_dbl(cats, ~ sum(w[m & yv == .x]) / wl))
    })
    ref_p <- stats::setNames(grid$prop[grid$level == lv[1]], grid$category[grid$level == lv[1]])
    ref_n <- grid$n[grid$level == lv[1]][1]
    ref_n_ci <- grid$n_ci[grid$level == lv[1]][1]
    grid$diff <- grid$prop - ref_p[grid$category]
    grid$var  <- p
    pw <- ci_wilson(grid$prop, grid$n_ci, conf_level = conf_level)               # crude % cell CI
    dd <- ci_prop_diff(grid$prop, grid$n_ci, ref_p[grid$category], ref_n_ci,     # crude diff CI
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
# Last Phase z3: `comparison = "lnratioavg"` is the RATIO twin of the default additive contrast -- the
# log of the ratio of adjusted predicted probabilities (marginal standardization / g-computation), exp()'d
# here into a risk ratio. It shares the whole multiplicative path with "lnor": same double-paren label
# shape, same exp() of the estimate and BOTH bounds (so the interval stays a Wald interval on the log
# scale, asymmetric and strictly positive once exponentiated).
reg_marginal <- function(fit, data, predictors, conf_level, wt = NULL,
                         at = "average", comparison = NULL, want_pred = TRUE) {
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
    # template with or = 1, not "({pct})" -- display_primary("({pct})") is `pct`, which would make
    # cond_or (tab_classes.R) attach a stray "OR: 1.00" hover to a risk-ratio column.
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
  valid <- c("n", "lr_null", "wald_null", "mcfadden_r2", "nagelkerke_r2", "cox_snell_r2",
             "r2", "r2_adj", "f_model", "sigma", "aic", "bic", "dispersion", "brant_po")
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
#' @keywords internal
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
  if (!is.null(test) && nrow(test) > 0 && !is.null(test$row_var) && any(nzchar(test$row_var))) {
    g_col <- col_of_group[test$row_var]
    test  <- test[!is.na(g_col), , drop = FALSE]
    test$col_var <- unname(g_col[!is.na(g_col)])
    test$row_var <- ""
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
  # method, color_signif, cleannames, subtext, effect, at, stats, compare, baseline, multiplier, empirical,
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
    combined <- vctrs::vec_rbind(!!!purrr::map(parts, "data"))
    tests    <- purrr::list_rbind(purrr::compact(purrr::map(parts, "test")))
    if (is.null(tests) || nrow(tests) == 0) tests <- new_test_tibble()
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
      reg_reref_fit_res(digest, reference, sp, skeleton, conf_level)
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
  numeric_preds <- union_predictors[!purrr::map_lgl(
    union_predictors, ~ is.factor(skeleton_data[[.x]]) || is.character(skeleton_data[[.x]]))]

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
                            comparison = if (ratio_ame) "lnratioavg" else NULL)
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
                                          design_spec, conf_level, numeric_preds, model_predictors)
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
  # reads "<var> (per <k>)" (the effect is now per k units) -- KEEP the predictor name (dropping it left
  # a bare "per 2" the user could not read). Numeric predictors have level == var; k == 1 is a no-op
  # (no scaling), so leave the plain name.
  if (!is.null(multiplier)) {
    for (v in names(multiplier)) {
      k <- as.numeric(multiplier[[v]])
      if (is.na(k) || k == 1) next
      hit <- skeleton$var == v & skeleton$level == v
      if (any(hit)) disp_levels[hit] <- paste0(disp_levels[hit], " (per ", multiplier[[v]], ")")
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
    fac_preds_e <- union_predictors[!purrr::map_lgl(
      union_predictors, ~ is.numeric(skeleton_data[[.x]]))]
    if (length(fac_preds_e) > 0L) {
      for (i in seq_along(specs)) {
        fam_i   <- specs[[i]]$family
        # quasipoisson rides the poisson crude path (rate + IRR); ordinal none; multinomial tooltip.
        if (!fam_i %in% c("binomial", "gaussian", "poisson", "quasipoisson", "rr")) next
        col_i   <- specs[[i]]$color               # on/off follows the model column
        dep_i   <- specs[[i]]$dependent
        pos_i   <- if (reg_fam_binary(fam_i)) fits[[i]]$positive_level else NULL
        if (reg_fam_binary(fam_i) && is.null(pos_i)) next # grouped-binomial / compound: no crude 2x2
        mdata_i  <- emp_frame_of(dep_i)                    # Change B: same complete-case frame as the model
        var_y_i <- if (fam_i == "gaussian")
          suppressWarnings(stats::var(as.numeric(mdata_i[[dep_i]]), na.rm = TRUE)) else NA_real_
        emp_i   <- reg_empirical(mdata_i, fac_preds_e, dep_i, fam_i, pos_i, design_spec$wt)
        cols_i  <- reg_empirical_columns(skeleton, emp_i, fac_preds_e, fam_i, effect, var_y_i,
                                         conf_level = conf_level, color_signif = color_signif,
                                         color = col_i, do_exp = specs[[i]]$do_exp)
        # Phase 14w (item 3): the crude companions share the model column's outcome col_var (one span,
        # no border). NOT in comparison mode (the crude block stays a distinct col_var beside the models).
        if (!is_comparison) {
          scv    <- reg_shared_col_var(fam_i, dep_i, pos_i, cleannames)
          cols_i <- purrr::map(cols_i, ~ set_col_var(.x, scv))
        }
        emp_by_fit[[i]] <- cols_i
      }
    }
  }
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
  # Phase 15e: crude tooltips are built PER multinomial fit (a mixed table can hold several multinomial
  # outcomes, each with its own dependent / complete-case frame), keyed to that fit's category columns.
  empirical_tips <- NULL
  mnl_specs <- which(purrr::map_chr(specs, ~ .$family) == "multinomial")
  if (isTRUE(empirical) && length(mnl_specs) > 0L) {
    fac_preds_t <- union_predictors[!purrr::map_lgl(
      union_predictors, ~ is.numeric(skeleton_data[[.x]]))]
    is_fac_t <- skeleton$var %in% fac_preds_t
    if (length(fac_preds_t) > 0L) {
      tip_rows <- purrr::flatten(purrr::map(mnl_specs, function(si) {
        dep_i    <- specs[[si]]$dependent
        cols_idx <- fit_first_idx[[si]]:(fit_first_idx[[si]] + fit_ncol[[si]] - 1L)
        cols_idx <- cols_idx[!purrr::map_lgl(built[cols_idx], ~ is.null(.$emp_key))]
        if (length(cols_idx) == 0L) return(list())
        # Change B: multinomial crude tooltips on the model's complete-case frame (shared with the model).
        mdata_t  <- emp_frame_of(dep_i)
        tipsd <- reg_empirical_tips(mdata_t, fac_preds_t, dep_i, design_spec$wt,
                                    conf_level = conf_level)
        tk    <- reg_skel_key(tipsd$var, tipsd$level, tipsd$category)
        purrr::compact(purrr::map(cols_idx, function(i) {
          b    <- built[[i]]
          mi2  <- match(reg_skel_key(skeleton$var, skeleton$level, b$emp_key), tk)
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
      }))
      if (length(tip_rows)) empirical_tips <- purrr::list_rbind(tip_rows)
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
#' @param multiplier Optional named numeric vector `c(var = k)` rescaling a **continuous**
#'   predictor's effect to a k-unit change (e.g. `c(age = 10)` shows the odds ratio / beta per decade
#'   of age = OR^10 / beta*10). The confidence interval scales with it; the p-value is unchanged. Names
#'   must be numeric predictors; not available for multinomial / ordinal outcomes.
#' @param empirical Logical. If `TRUE`, adds the descriptive **crude** (unadjusted, single-predictor)
#'   companion of the model effect for each factor-predictor level -- the unadjusted bivariate
#'   association, which IS the modelised quantity when there is a single predictor (the standard "crude
#'   vs adjusted" comparison; a large gap signals confounding). Per family: **binomial** adds `Obs_%`
#'   + `Obs_OR` (coefficient) or `Obs_%` + `Obs_diff` (AME); **gaussian** adds `Obs_mean` +
#'   `Obs_diff`; **poisson** adds `Obs_rate` + `Obs_IRR`; **multinomial** shows the crude % +
#'   difference per category in the HTML tooltip (columns would explode). By design every crude quantity
#'   is computed on **exactly the same complete-case population as the model** (listwise-complete on the
#'   dependent, all predictors and any design variable), so crude and adjusted are directly comparable
#'   and not confounded by differing missingness (reproduce it with [dplyr::filter()] + [tab()] on the
#'   same rows). Also works with a vector of dependents. Ordinal has no clean crude analogue and is
#'   ignored (with a message). These crude companion CIs are descriptive, so on weighted data they
#'   honour `options(tabxplor.kish_neff = TRUE)` (Kish's effective sample size) exactly like [tab()];
#'   the model column's own CI is always design-based (`survey::svyglm`) and unaffected. Default
#'   `FALSE`.
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
#' @param color,color_signif Colouring of the effect cells. `color = TRUE` (default) auto-picks the
#'   sensible per-family measure (`"OR"` magnitude for ratios, standardized `"diff"` for betas);
#'   `color = FALSE` turns colouring off for every column (model and empirical). Power users may pass a
#'   measure string (`"OR"`, `"diff"`, `"ratio"`, `"no"`) to override. `color_signif` is the
#'   significance policy (default `"grey_non_signif"`). See [tab()].
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
                    reference = NULL, inverse_two_level_factors = TRUE, multiplier = NULL, 
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
  color_auto <- is.na(color)                                    # Phase 15e: remember the auto sentinel
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
  # multinomial / ordinal / split / multiplier / trials / model comparison) keeps the refit path.
  # Phase 15e: an all-glm mixed table keeps the digest fast-path (each spec caches its own family's
  # digest); any multinomial/ordinal outcome degrades the whole table to the cached raw-fit path.
  reref <- !is.null(.fit_cache) && effect == "coefficient" && !mnl_vsrest &&
    estimate_display %in% c("value", "ci") && method == "wald" &&
    all(families_vec %in% c("gaussian", "binomial", "poisson", "quasipoisson", "rr")) &&
    !formula_mode && is.null(split_var) && is.null(multiplier) && is.null(trials) &&
    compare == "none" && !is_comparison

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
                                 eff_word = eff_word, color = color))
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
                                 color = color_for(.x)))
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
    # Phase 15e: multiplier scales glm-family coefficients; abort only when EVERY outcome is
    # multinomial/ordinal (nothing to scale). In a mixed table it applies to the glm outcomes.
    if (all(families_vec %in% c("multinomial", "ordinal"))) {
      cli::cli_abort("{.arg multiplier} is not supported for {.val multinomial}/{.val ordinal} models.")
    }
    if (any(families_vec %in% c("multinomial", "ordinal"))) {
      cli::cli_inform(c("i" = paste0(
        "{.arg multiplier} scales the glm-family outcomes only; the multinomial/ordinal ",
        "outcome{?s} are shown unscaled.")))
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
  # Phase 15e: kept ON whenever ANY outcome supports a crude companion (the per-fit loop skips the
  # ineligible outcomes -- ordinal -- individually). Only dropped when NO outcome is eligible.
  if (isTRUE(empirical) &&
      !any(families_vec %in% c("binomial", "gaussian", "poisson", "quasipoisson", "multinomial",
                               "rr"))) {
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
                        inverse_two_level_factors = TRUE, split_var = NULL, multiplier = NULL,
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
