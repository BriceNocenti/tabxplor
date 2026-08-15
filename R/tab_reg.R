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
#     the `or` field, scale="odds_ratio", display="or", color="OR" (neutral 1, 1/x reciprocal);
#     ADDITIVE (gaussian beta / log-odds) -> the `diff` field, type="coef", display="coef",
#     scale="raw_diff", color="diff" (neutral 0), with `var`=var(Y) so the colour is the effect-size
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
#   - 12g: SURVEY designs + companion features. `wt` builds a flat (ids = ~1)
#     survey::svydesign per model (svy_make_design); a PREBUILT survey.design passed as
#     `data` is subset()'d per model (svy_domain_design / reg_resolve_design) -- design-based, no weight
#     normalisation. reg_svyglm_env() binds survey::svyglm into the fit's formula env so AIC.svyglm /
#     anova.svyglm work when survey is loaded but not attached. Weighted 3+ level lifted: ordinal ->
#     survey::svyolr, nominal -> svyVGAM::svy_vglm (Suggests). Weighted glance = the reduced survey set
#     (n / Wald-vs-null / Nagelkerke [+ selectable Cox-Snell] / Rao-Scott AIC); weighted comparison =
#     anova.svyglm Wald (compare_*_wald). `split_var` = the tab_vars analogue: reg_build recurses per
#     group on a SHARED skeleton (skeleton_data) and stacks into a grouped_tab (split_var + var), so
#     tab_spread(split_var) pivots groups to columns (no tab_spread change: split_var placed first so
#     `levels` stays the row_var; console footer is group-aware, export footer skipped for splits).
#     `multiplier` scales a continuous predictor's native coef by k before CI/exp (OR^k), p unchanged.
#   - Phase 18z9: `multiplier` is the UNIT such a predictor's effect is reported per, and its DEFAULT
#     is "sd" (per one standard deviation) -- per 1 unit the row sits inside the first colour break and
#     reads as "no effect". A scalar ("sd"/"2sd"/a number) applies to all, a named vector overrides per
#     variable, 1 = per unit. Resolved ONCE in tab_reg() on the PREDICTOR complete-case frame, so one
#     predictor keeps one unit across outcomes, compared models and split groups. Same phase:
#     `empirical = TRUE` fills a continuous predictor's crude EFFECT column from its univariable
#     reg_fit() (base cell stays empty, distribution -> tooltip), and reg_gap_se_columns() tests that
#     gap like any other. reg_is_factor_var() is the ONE predictor-kind predicate.
# See: CLAUDE.md Phase 12c-12g + Phase 18z9 ; dev/tabxplor_2.0.0_decisions.md S37 ;
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
  # Phase 19l: the unweighted 3+ level engines need NO guard -- `nnet` and `MASS` are in Imports (see
  # DESCRIPTION), so requireNamespace() could never be FALSE and the two aborts behind it were
  # unreachable. Only Suggests-level packages are guarded here.
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

# DESIGN (Phase 18z3): the family PREDICATES. Every "which families behave like X" question is asked
# here ONCE instead of by a hand-written whitelist at each call site (there were 11 bare `== "binomial"`
# tests, 4 probability-scale lists, and the log-scale list written TWICE verbatim in fmt_class.R). The
# internal family key "rr" (modified Poisson on a binary outcome, see the families_vec resolver in
# tab_reg()) joins the binary + log-scale sets here and nowhere else.
reg_is_binary_outcome <- function(y) length(unique(stats::na.omit(y))) == 2L
# the binary-outcome machinery: reg_prep_binary / positive_level / the crude 2x2 companion. Phase 19l
# states the REASON the three agree instead of repeating the list: `rr` and `rd` are the internal
# LINKS whose fit is a binomial one, which REG_FIT_FAMILY already declares.
reg_fam_binary   <- function(f)
  f %in% c("binomial", names(REG_FIT_FAMILY)[REG_FIT_FAMILY == "binomial"])
# probability-scale outcomes: a marginal effect is a probability (percentage points / a risk ratio).
reg_fam_prob     <- function(f) f %in% c("binomial", "multinomial", "ordinal")
# per-CATEGORY outcomes: one column (and one crude effect) per outcome category, not one per model.
reg_fam_percategory <- function(f) reg_fam_prob(f) & !f %in% "binomial"
# the count families: a Poisson likelihood, with or without an estimated dispersion.
reg_fam_count    <- function(f) f %in% c("poisson", "quasipoisson")
# Is this fit a GROUPED binomial -- a summed-score outcome fit as cbind(score, trials - score)?
# Phase 19l: THREE sites asked it. Two were the same three-clause test written twice; the third
# (reg_crude_key) also named "rd", which was DEAD -- that function returns on `rd` one line earlier.
# The compound-formula clause is part of the fact: a compound formula controls its own LHS, so
# `trials` does not apply to it.
reg_is_grouped_binomial <- function(family, trials, compound = FALSE)
  identical(family, "binomial") && !is.null(trials) && !isTRUE(compound)
# (no reg_fam_logscale(): Phase 19l deleted it. Its WARNING claimed fmt_class.R's colour engine and
#  legend read it -- they do not, and had not since 19b: both reach the fact through the column's
#  STORED `scale`. Its one live caller picked "log_coef", which REG_ESTIMANDS declares per row.)

# Phase 19a: five more, absorbing 21 hard-coded whitelists across tab_reg.R and reg-assumptions.R --
# the census that Phase 19e needs done before it can move the estimand surface. Same rule as above:
# ask the question once, here, not at each call site. Each names a FACT about the family, so the
# reason two call sites agree is stated rather than coincidental.
#
# fitted by stats::glm -- i.e. NOT one of the 3+-level machines (nnet::multinom / MASS::polr /
# survey::svyolr / svyVGAM), which have no glm-shaped coefficient table, no anova() and no AIC path.
reg_fam_glm <- function(f) f %in% c("gaussian", "binomial", "poisson", "quasipoisson",
                                    "rr", "rd", "mr")
# the count model can be over-dispersed, so a Pearson dispersion (phi) is worth reporting and the
# nominal variance cannot be trusted. `grouped` = a grouped-binomial (successes/trials) fit, which is
# over-dispersible for the same reason a Poisson is.
reg_fam_overdispersed <- function(f, grouped = FALSE) f == "poisson" || isTRUE(grouped)
# the dispersion is FIXED BY THE FAMILY (1), so the Wald critical value refers to z, not t.
reg_fam_disp_known <- function(f) f %in% c("binomial", "poisson")
# the dispersion is ESTIMATED from the residuals, so a term test refers to F rather than chi2.
reg_fam_disp_estimated <- function(f) f %in% c("gaussian", "quasipoisson")
# the fit is produced by survey::svyglm -- ONE fact with two consequences, which is why the same
# expression used to appear under two different names (`use_svy`, picking the fitter, and `use_wald`,
# picking Wald over LR). An svyglm has no ordinary likelihood, so there is no LR test to run. "rr"
# (modified Poisson with robust SE on a binary outcome) goes through svyglm even unweighted.
# Phase 19e: "rd" (identity link on a binary outcome) and "mr" (log link on a continuous one) join
# "rr" for the same reason: all three are deliberately misspecified likelihoods chosen to reach a
# MEASURE, so their honest variance is the Huber-White sandwich, which svyglm's design-based variance
# IS. One fact, three links.
reg_fam_svy_fitted <- function(f, weighted = FALSE)
  isTRUE(weighted) || f %in% REG_FIT_ONLY_FAMILIES
# Phase 18z8-B (SS4.2, maintainer ruling Q1(b)): is the DISPLAYED estimand COLLAPSIBLE -- i.e. does a
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

# Phase 18z13 (D6): THE producer of "the colour you asked for cannot be computed here". Before it,
# four refusals said so in four hand-written blocks in tab_reg()'s body and two said nothing at all --
# a table that looked as if `color =` had simply been ignored. The rule is now uniform and testable in
# isolation: a requested colour measure that cannot be scored, or cannot be TESTED, names its reason
# once. Each reason is one entry here, not a new block at a call site.
#
# It runs BEFORE the build on purpose: the build recurses per split group and per spec, so it would say
# the same thing many times, and every fact needed is a pre-condition (the family, the effect, the crude
# key, the axis) rather than a result.
#
# `no_colour` = no `obs` will be attached, so the measure paints nothing; `no_test` = `obs` is there and
# the descriptive colour works, but `color_signif` cannot apply. The distinction matters to the reader:
# the first is an empty feature, the second a deliberate one.
#' @keywords internal
reg_color_notes <- function(color, color_signif, ests, split_var, na, na_explicit,
                            families, crude_keys, empirical = FALSE) {
  notes <- character(0)
  # Phase 19e: the four facts these notes need are read off the resolved ESTIMAND rows, not
  # re-derived from (effect, at, do_exp) -- so a note cannot describe a different estimand from the
  # one that was built.
  at    <- if (any(vapply(ests, function(e) identical(e$effect, "at_reference"), logical(1))))
             "reference" else "average"
  effect <- ests[[1]]$effect
  # Interpolated HERE, where the locals the messages name are in scope: the caller only emits them, so
  # a `{.val {bare}}` resolved in its frame would be an error, not a message.
  add   <- function(...) notes <<- c(notes, cli::format_inline(paste0(...)))
  gap   <- intersect(c("adjustment", "between_groups"), color)
  if (length(gap) == 0L && !isTRUE(empirical)) return(notes)

  if ("between_groups" %in% gap && is.null(split_var)) {
    add("{.code color = \"between_groups\"} compares each effect to the first group's, so it needs ",
        "{.arg split_var} to say what the groups are. Without it nothing is coloured.")
  }
  if (identical(at, "reference")) {
    add("{.code at = \"reference\"} evaluates the model at the reference profile, while the ",
        "observed columns stay marginal over the whole sample: the two are shown side by side, ",
        "but not compared cell by cell ({.code color = \"adjustment\"} and {.code \"{{obs}}\"} ",
        "stay empty).")
  }
  if ("adjustment" %in% gap) {
    # A MARGINAL model effect needs a marginal crude twin. REG_EMPIRICAL declares which families have
    # one, so the question is asked of the fact table -- not of a family list kept in sync with it:
    # if asking for the marginal shape returns the coefficient shape, there is none, and
    # reg_same_estimand() will (rightly) refuse to pair an additive AME with a crude ratio.
    if (!identical(effect, "coefficient")) {
      # Phase 19e: "is there a marginal crude twin?" is now a comparison of two DECLARED shape names
      # -- the marginal row's against the family's coefficient row's. Where a family declares no
      # marginal crude shape it names its coefficient one, and reg_same_estimand() then (rightly)
      # refuses to pair an additive AME with a crude ratio.
      bare <- unique(vapply(names(ests), function(d) {
        e  <- ests[[d]]
        cf <- reg_estimand(e$family, "coefficient", "auto")
        if (identical(e$crude_shape, cf$crude_shape)) e$family else NA_character_
      }, character(1)))
      bare <- stats::na.omit(bare)
      if (length(bare)) {
        add("{.code effect = {.val {effect}}} has no observed counterpart on the same scale for ",
            "{.val {bare}} (its crude effect is a ratio, the marginal effect a difference), so ",
            "{.code color = \"adjustment\"} stays empty there. Use {.code effect = \"coefficient\"} ",
            "to compare them.")
      }
    }
    if (!is.null(color_signif) && !identical(color_signif, "ignore") &&
        !any(vapply(families, reg_estimand_collapsible, logical(1), effect = effect))) {
      add("{.arg color_signif} does not apply to an odds-ratio {.val adjustment} gap: part of it is ",
          "non-collapsibility, not confounding. Use {.code effect = \"marginal\"} or ",
          "{.code measure = \"ratio\"} (risk ratios), for a gap the test can read.")
    }
    # Phase 18z13 (D1): only on an explicit choice -- the default already shares the population.
    if (na_explicit && identical(na, "drop_by_model")) {
      add("{.code na = \"drop_by_model\"} lets each model use its own complete cases, so a model ",
          "fitted on rows the observed columns do not cover gets no observed effect at all (no ",
          "colour, no test): their distance would be listwise deletion, not adjustment.")
    }
  }
  notes
}

# Phase 18z10: `crude_key` -- THE stored fact "which observed counterpart does this model have?".
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
  # Phase 19e: the identity-link risk-difference fit shares the binomial's crude block outright --
  # its base is the same risk and its effect the same Wald risk difference (REG_EMPIRICAL$binomial's
  # `base` / `ame` rows), which is why "rd" needed no block of its own.
  if (identical(family, "rd"))                          return("binomial")
  if (reg_is_grouped_binomial(family, trials, compound)) return("grouped_binomial")
  if (is.null(REG_EMPIRICAL[[family]]))                 return(NA_character_)
  family
}

# Phase 18z9: is a PREDICTOR a factor (contrasts vs a reference level) or a numeric (one slope per
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


# === `multiplier`: the per-unit scaling of a continuous predictor's effect (Phase 18z9) ===========
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

# (`reg_shape_term()` -- the extra TERM a non-linear `shape` emits -- lives in R/reg-assumptions.R,
# beside the Linearity check that emits the same term: the check and its cure are one object.)

# THE multiplier keywords: the per-SD spellings `multiplier` accepts beside a plain number. Read by
# the parser below AND by the jamovi picker (jmvtab_reg_mult_vector), which used to copy the set.
#' @keywords internal
REG_MULTIPLIER_KEYWORDS <- c("sd", "1sd", "2sd")

# Parse ONE multiplier value ("sd" / "2sd" / a number) against a predictor's frozen SD.
# Returns list(k = <numeric>, label = <character or NA>); k = NA drops the entry.
#' @keywords internal
reg_multiplier_value <- function(value, sd, digits = 3L) {
  v <- if (is.character(value)) trimws(tolower(value)) else value
  if (length(v) != 1L || is.na(v)) return(list(k = NA_real_, label = NA_character_))
  if (is.character(v) && v %in% REG_MULTIPLIER_KEYWORDS) {
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
# (proportional-odds); an UNORDERED factor / character with 3+ levels -> multinomial; a numeric ->
# gaussian. A binary outcome is ALWAYS binomial here: the modified Poisson (risk-ratio) path is opt-in
# via an explicit family = "poisson" (Phase 18z3).
#
# Phase 18z13 (D10): the gaussian branch used to require a NON-INTEGER numeric, so every
# integer-stored continuous outcome -- age in years, years of schooling, income in whole units, a
# Likert sum -- fell through to the abort. That is one of the two commonest outcome kinds, and it made
# the R side disagree with the jamovi selector, which had to pick something. Both now answer gaussian
# and name `family = "poisson"` for a genuine count: gaussian always fits (poisson refuses negatives),
# and reading an integer outcome as a count is the rarer intent, not the safer default.
# REG_OUTCOME_KINDS -- Phase 19k: THE outcome-kind table. One row per kind of dependent variable the
# module and the R side can see, carrying the family DETECTED for it and the families OFFERED beside
# that one. It exists because this rule was written THREE times: here, in the jamovi R fallback, and
# in `detectFamily()` / `familyOptionsFor()` in JavaScript, whose own comment claimed it "matches the
# R side exactly" (it had not, since 18z13). The JS block is GENERATED from this table now
# (dev/generate_jamovi_js.R), so the claim is checked by a test instead of by a comment.
#
# `kind` is what BOTH sides can compute from a column alone: whether it has levels, and how many.
# `offers` is ordered, first = the detected default; a 2-level outcome offers poisson because that is
# the opt-in modified-Poisson (risk-ratio) route (18z3), not a count model.
#' @keywords internal
REG_OUTCOME_KINDS <- list(
  binary   = list(detect = "binomial",    offers = c("binomial", "poisson")),
  ordered  = list(detect = "ordinal",     offers = c("ordinal", "multinomial")),
  nominal  = list(detect = "multinomial", offers = c("multinomial", "ordinal")),
  # Phase 18z13 (D10): ANY numeric is gaussian, integer-valued included -- age in years, a summed
  # score and income in whole units are all integers, and a linear model always fits. poisson stays
  # one click away in `offers`.
  numeric  = list(detect = "gaussian",    offers = c("gaussian", "binomial", "poisson"))
)

# The KIND of one outcome column ("" = none of them -> no family can be detected).
#' @keywords internal
reg_outcome_kind <- function(y) {
  u <- unique(stats::na.omit(y))
  if (reg_is_binary_outcome(y))                                return("binary")
  if (is.ordered(y) && length(u) >= 3L)                        return("ordered")
  if ((is.factor(y) || is.character(y)) && length(u) >= 3L)    return("nominal")
  if (is.numeric(y))                                           return("numeric")
  ""
}

reg_detect_family <- function(data, dependent) {
  y    <- data[[dependent]]
  kind <- reg_outcome_kind(y)
  if (!nzchar(kind)) {
    cli::cli_abort(c(
      "Cannot auto-detect the model family for {.val {dependent}}.",
      "i" = paste0("Set {.arg family} explicitly: {.val gaussian} (linear), {.val poisson} (counts), ",
                   "{.val binomial} (logistic), {.val multinomial} / {.val ordinal} (3+ level).")
    ))
  }
  fam  <- REG_OUTCOME_KINDS[[kind]]$detect
  said <- switch(kind,
    binary  = "binary outcome detected",
    ordered = "ordered outcome detected",
    nominal = "nominal outcome detected",
    numeric = "continuous outcome detected")
  cli::cli_inform(c("i" = paste0(
    "{.val {dependent}}: ", said, " -> {.code family = \"", fam, "\"} (",
    reg_family_short(fam), ")",
    if (identical(kind, "numeric") && !any(y %% 1 != 0, na.rm = TRUE))
      "; it is integer-valued, so {.code family = \"poisson\"} if it is a count" else "",
    "."
  )))
  fam
}

# Phase 14w: the human name of the model family, shared by the reg title/caption and the "Model:" footer
# line (reg_model_line). do_exp/effect do not change the NAME (the estimand phrase carries that detail).
# Phase 18w: translatable (gettext). Every caller runs it inside a with_legend_lang() context
# (reg_model_lines / reg_title), so the LANGUAGE env is already set when these gettext() lookups fire.
reg_family_display_name <- function(family) {
  switch(family,
    "gaussian"     = gettext("linear regression"),
    "binomial"     = gettext("logistic regression"),
    "poisson"      = gettext("Poisson regression"),
    "quasipoisson" = gettext("quasi-Poisson regression"),
    "rr"           = gettext("modified Poisson regression"),
    "rd"           = gettext("additive-risk regression (identity link, robust standard errors)"),
    "mr"           = gettext("log-link mean regression (Poisson pseudo-likelihood, robust standard errors)"),
    "multinomial"  = gettext("multinomial logistic regression"),
    "ordinal"      = gettext("ordinal logistic regression"),
    gettext("regression"))
}

# REG_FAMILY_UI_LABEL -- Phase 19k: the short family label a PICKER shows ("what kind of model is
# this", in three words). Distinct from reg_family_display_name() (a full sentence, for the footer)
# and from reg_family_short() (a filename tag). Generated into the jamovi model-table dropdown by
# dev/generate_jamovi_js.R, where it used to be typed a second time.
# `_BINARY` overrides the label on a 2-LEVEL outcome, where family = "poisson" is not a count model:
# R resolves it to the modified Poisson (Zou 2004), whose exp(coef) is a RISK ratio (18z3). Same
# stored value, different words -- so the dropdown never says "counts" next to a yes/no variable.
#' @keywords internal
REG_FAMILY_UI_LABEL <- c(
  gaussian    = "gaussian (linear)",
  binomial    = "binomial (logistic)",
  poisson     = "poisson (counts)",
  multinomial = "multinomial (nominal)",
  ordinal     = "ordinal (ordered)"
)
#' @keywords internal
REG_FAMILY_UI_LABEL_BINARY <- c(
  binomial = "binomial (logistic)",
  poisson  = "poisson (risk ratio)"
)

# Phase 14w: the short model tag used for Excel sheet names ("logit_<dep>_<pred>...").
reg_family_short <- function(family) {
  switch(family,
    "gaussian"     = "linear",
    "binomial"     = "logit",
    "poisson"      = "poisson",
    "quasipoisson" = "qpoisson",
    "rr"           = "rr",
    "rd"           = "rd",
    "mr"           = "mr",
    "multinomial"  = "mlogit",
    "ordinal"      = "ologit",
    "reg")
}


# Phase 14w: the "Model: <family>. <estimand>." legend line, generated fresh from the table's stored
# recipe (`meta$spec$call`, Phase 19g) at render
# so it can be ordered BEFORE the colour legend (item 2). For a model comparison the caption is not shown
# in the console, so the dependent + (binomial) reference level are named here too (item 4). NULL when the
# table is not a regression (reg_call -> NULL).
# Phase 18w: the prose is translatable (gettext); called only from reg_model_lines(), which sets the
# LANGUAGE env via with_legend_lang(). enc2utf8 for the French accents (matches tab_weight_line et al.).
# Does ANY of these outcomes fold its observed effect into the model cell? Reads the STORED crude keys.
#' @keywords internal
reg_meta_obs_in_cell <- function(meta, deps = NULL) {
  ck <- meta$crude_keys
  if (is.null(ck)) return(FALSE)
  if (!is.null(deps)) ck <- ck[intersect(names(ck), deps)]
  any(purrr::map_lgl(names(ck), function(d)
    !is.na(ck[[d]]) && reg_crude_in_cell(ck[[d]], reg_meta_estimand(meta, d))))
}

# reg_meta_estimand() -- the stored ESTIMAND of one dependent, re-resolved from the recipe (Phase
# 19e). The record keeps the words (`families` / `effects` / `measures`); the row is looked up rather
# than stored, so a table cannot carry a row that a later version's library disagrees with.
#' @keywords internal
reg_meta_estimand <- function(meta, dependent = NULL, family = NULL) {
  d   <- if (is.null(dependent)) NULL else as.character(dependent)
  # a consumer may hold only the COLUMN's stored family (the fit): find the outcome it belongs to, so
  # a mixed table names each column from its own row rather than from the table's first outcome.
  if (is.null(d) && !is.null(family) && nzchar(family)) {
    fk   <- unname(REG_FIT_FAMILY[family]); if (is.na(fk)) fk <- family
    fams <- meta$families %||% meta$family
    hit  <- names(fams)[fams %in% c(family, fk)]
    if (length(hit)) d <- hit[[1]] else return(reg_estimand(fk, meta$effect %||% "coefficient",
                                                            meta$measure %||% "auto"))
  }
  pick <- function(v, scalar) {
    if (is.null(v)) return(scalar)
    if (!is.null(d) && !is.null(names(v)) && d %in% names(v)) return(unname(v[[d]]))
    unname(v[[1]])
  }
  fam <- pick(meta$families, meta$family)
  eff <- pick(meta$effects,  meta$effect  %||% "coefficient")
  mea <- pick(meta$measures, meta$measure %||% "auto")
  res <- reg_estimand(fam %||% "gaussian", eff %||% "coefficient", mea %||% "auto")
  if (identical(res$status, "ok")) res else reg_estimand(fam %||% "gaussian", "coefficient", "auto")
}

reg_model_line <- function(meta) {
  if (is.null(meta)) return(NULL)
  fam <- reg_family_display_name(reg_meta_estimand(meta)$fit %||% meta$family)
  est <- reg_estimand_note(reg_meta_estimand(meta), obs_in_cell = reg_meta_obs_in_cell(meta))
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
# Phase 18w: `lang` selects the footer language (NULL -> options(tabxplor.lang)/locale). The whole
# composition runs under with_legend_lang() so every nested gettext() (family name, estimand, "Model:")
# resolves to that language; English is byte-identical (gettext returns the msgid under the en locale).
reg_model_lines <- function(x, lang = NULL) {
  meta <- reg_call(x)
  if (is.null(meta)) return(character(0))
  with_legend_lang(lang, function(lg) {
    fams <- meta$families; if (is.null(fams)) fams <- meta$family
    uf   <- unique(fams)
    if (length(uf) <= 1L) { rl <- reg_model_line(meta); return(if (is.null(rl)) character(0) else rl) }
    deps <- meta$dependent
    vapply(uf, function(fm) {
      grp   <- deps[fams == fm]
      e     <- reg_meta_estimand(meta, grp[[1]])
      fname <- reg_family_display_name(e$fit %||% fm)
      est   <- reg_estimand_note(e, obs_in_cell = reg_meta_obs_in_cell(meta, grp))
      enc2utf8(if (nzchar(est)) gettextf("Model (%s): %s; %s.", legend_name_list(grp), fname, est)
               else            gettextf("Model (%s): %s.", legend_name_list(grp), fname))
    }, character(1), USE.NAMES = FALSE)
  })
}

# Phase 18z8: the AGGREGATED effect-modification test, as one footer line per model -- the
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
# Phase 18z15: the per-predictor GLOBAL test (z13) is no longer a footer LINE -- it became footer
# ROWS, one per (model column x predictor), so reg_global_lines() and the shared reg_term_test_line()
# it was extracted for are gone. Measured on the vignette's own data: in a 3-model comparison the line
# rendered as three sentences with nothing naming which model each described, and on a split table it
# printed the split level, repeated, instead of the predictors. A verdict that belongs to one model
# column belongs in the GOF block, which already has one column per model.
reg_interaction_lines <- function(x, lang = NULL) {
  tt <- get_test(x)
  if (is.null(tt) || nrow(tt) == 0) return(character(0))
  it <- tt[tt$test %in% reg_interaction_types(), , drop = FALSE]
  if (nrow(it) == 0) return(character(0))
  meta <- reg_call(x)
  sv   <- if (is.null(meta)) NA_character_ else meta$split_var
  with_legend_lang(lang, function(lg) {
    tname <- c(interact_lr = gettext("likelihood ratio"), interact_f = gettext("F test"),
               interact_wald = gettext("Wald test"))
    on_coef <- !is.null(meta) && !identical(meta$effect %||% "coefficient", "coefficient")
    # split() by a FACTOR of first-appearance order, so several models keep their column order.
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

# Phase 14w: the reg table's TITLE / caption (Excel title + sheet, md/kable caption). Single model:
# "<Family>: <dep> by <p1>, <p2> +N more". Comparison: "<Family>s (models comparison): <dep>, '<ref>'
# (<effect>)" -- the reference level + effect that would otherwise be written nowhere (item 4).
# Phase 18w: the caption prose is translatable (gettext), resolved under with_legend_lang(). `lang`
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
#
# Phase 18z15: `shape_terms` (named by variable) adds the CURVATURE row of a `shape = "quadratic"`
# predictor -- two coefficient rows for one predictor, which is exactly what R8 asks for. The 1-to-1
# it breaks is stated as a rule, not patched with an `if`: the skeleton emits ONE ROW PER MODEL TERM on
# the coefficient path and ONE ROW PER PREDICTOR on the marginal path (an AME already integrates the
# curvature), so reg_build passes `shape_terms` only on the former.
reg_skeleton <- function(data, predictors, shape_terms = NULL) {
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
      sq <- if (!is.null(shape_terms) && p %in% names(shape_terms)) shape_terms[[p]] else NULL
      tibble::tibble(
        var    = p,
        level  = c(p, if (!is.null(sq)) reg_shape_sq_level(p)),
        # the formula carries backticks; broom::tidy()'s term does not (reg_fit strips them)
        term   = c(p, if (!is.null(sq)) gsub("`", "", sq, fixed = TRUE)),
        is_ref = rep(FALSE, 1L + !is.null(sq))
      )
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
                             weighted = FALSE, make_design = NULL, add_terms = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for multinomial models; using Wald."))
  }
  mdata[[dependent]] <- forcats::fct_drop(as.factor(mdata[[dependent]]))
  y_levels <- levels(mdata[[dependent]])
  fml <- stats::as.formula(paste0(
    "`", dependent, "` ~ ",
    paste(c(paste0("`", predictors, "`"), add_terms), collapse = " + ")
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
                            weighted = FALSE, make_design = NULL, add_terms = NULL) {
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
    "`", dependent, "` ~ ",
    paste(c(paste0("`", predictors, "`"), add_terms), collapse = " + ")
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
# Make a fit SELF-CONTAINED, so anything that rebuilds it from `fit$call` works outside the fitting
# scope. `nnet::multinom` / `MASS::polr` store `data = mdata` -- a local of reg_fit() -- so every
# consumer that calls update() or eval.parent(fit$call) fails with "object 'mdata' not found":
# brant::brant() (Phase 12d) and, since z15, stats::drop1() on the Linearity refit. Copy-on-modify, so
# the caller's fit is untouched; the returned one carries its own frame.
#' @keywords internal
reg_selfheal_call <- function(fit, data) {
  if (is.null(data) || is.null(fit$call)) return(fit)
  # Not every engine has a formula() method (svyVGAM::svy_vglm has none) -- leave such a fit as it is
  # rather than erroring: the caller degrades to no test, which is this module's contract.
  fml <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(fml)) return(fit)
  fit$call$data    <- data
  fit$call$formula <- fml
  fit
}

reg_ordinal_diagnostic <- function(fit) {
  if (!requireNamespace("brant", quietly = TRUE)) {
    cli::cli_inform(c("i" = paste0(
      "Proportional-odds (parallel-lines) assumption not tested: install {.pkg brant} to run the ",
      "Brant test."
    )))
    return(invisible(NA_real_))
  }
  fit <- reg_selfheal_call(fit, fit$model)
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
# A weight column is turned into a survey.design *per model*, on the complete-case model frame --
# svy_make_design()'s ids = ~1 (no clustering) reproduces the flat weighted path exactly. A PREBUILT
# survey.design (passed as `data`) is NOT rebuilt (a calibrated design cannot be) -- it is subset()'d
# to the model's complete cases (domain estimation) with its model-frame variables replaced by the
# recoded `mdata`. `design_spec` = list(design = <prebuilt or NULL>, wt).

reg_design_vars <- function(design_spec) svy_design_vars(design_spec)

# Subsetting a prebuilt design to a model's complete cases and swapping in the recoded `mdata` is the
# SAME operation tab()'s robust overlay needs, so it lives once in R/survey-design.R as
# svy_domain_design() -- including the calibrated-design padding rule (D10).
# The model's complete-case frame: drop rows missing the dependent, ANY predictor, or a design var --
# the ONE definition of "the same population as the model". reg_fit uses it for the fit; the empirical /
# multinomial-tip blocks recompute it from raw `data` (the fitted `f$data` is NULL on the reref/digest
# path, so it cannot be read back there). `intersect(., names(data))` guards vars absent from the frame.
reg_complete_frame <- function(data, vars)
  tidyr::drop_na(data, tidyselect::all_of(intersect(unique(vars), names(data))))

# The survey design for a model's (recoded) complete-case frame: a prebuilt design is subset()'d and
# has its model frame swapped for `mdata`; a weight column is built into a fresh design on `mdata`.
# `data` + `drop_vars` give the complete-case mask for the subset path. Shared by the glm (svyglm) and
# the 3+ level (svyolr / svy_vglm) weighted branches -- one design constructor.
reg_resolve_design <- function(design_spec, mdata, data, drop_vars) {
  if (!is.null(design_spec$design)) {
    keep <- which(stats::complete.cases(data[, drop_vars, drop = FALSE]))
    # Phase 18z14-iii: index the ORIGINAL design, always. Under split_var `data` holds one group's
    # rows, so its own positions are group-local; `.svy_row` (written at the boundary, R/survey-design.R)
    # is the position in the design the user passed. At top level .svy_row == seq_len(n), so `rows` is
    # `keep` and this is byte-identical. Without it a CALIBRATED design -- which `[` does not shrink --
    # took a group-local position as a full-sample one and weighted the wrong respondents.
    rows <- if (!is.null(data[[svy_row_col]])) as.integer(data[[svy_row_col]])[keep] else keep
    svy_domain_design(design_spec$design, rows, mdata)
  } else {
    svy_make_design(mdata, design_spec$wt)
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
                    drop_extra = NULL, add_terms = NULL) {
  # Phase 18z15: `add_terms` is the third sibling of `cross` / `drop_extra` -- extra RHS terms,
  # verbatim, appended to the formula and to nothing else (they name no new VARIABLE, so they never
  # join drop_vars: `I(((age - 44.2)/13.5)^2)` is complete exactly where `age` is). It is how the
  # Linearity check refits "the model plus this predictor's centred squared term" through the very
  # fitter the table came from, inheriting the binary prep, the grouped-binomial cbind, the "rr" route
  # and the design resolution -- which the `formula =` escape hatch would not.
  #
  # Phase 18z8: `cross` (a split_var) makes the POOLED interaction fit `y ~ (x1 + x2) * g`, used
  # only by reg_interaction_rows(). It goes through this whole function rather than the `formula =`
  # escape hatch precisely so it inherits the binary prep, the grouped-binomial cbind, the family
  # objects, the "rr" -> svyglm route and the design resolution; `formula =` deliberately disables the
  # first two. `cross` joins drop_vars so the pooled complete-case frame matches the per-group ones.
  #
  # Phase 18z9: `drop_extra` joins drop_vars but NOT the formula -- variables the fit must be
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

  weighted <- svy_weighted(design_spec, design_spec$wt)
  # A closure the fit branches call with their OWN recoded model frame -> the matching survey design
  # (build the weight-column design / subset the prebuilt one). Lets the MNL / ordinal engines, which
  # recode the outcome themselves, get a row-aligned design without re-deriving the mask.
  make_design <- function(recoded_mdata) reg_resolve_design(design_spec, recoded_mdata, data, drop_vars)

  # 3+ level categorical outcomes have their own engines: unweighted -> nnet::multinom / MASS::polr;
  # weighted -> svyVGAM::svy_vglm / survey::svyolr. All share the Wald machinery (reg_wald_from_tidy) so
  # the CI <-> p <-> stars duality holds, but not the glm path.
  if (family == "multinomial") {
    return(reg_fit_multinom(mdata, dependent, predictors, do_exp, conf_level, method,
                            weighted, make_design, add_terms = add_terms))
  }
  if (family == "ordinal") {
    return(reg_fit_ordinal(mdata, dependent, predictors, do_exp, conf_level, method,
                           weighted, make_design, add_terms = add_terms))
  }

  positive_level <- NULL
  # grouped binomial: a summed-score outcome (0..trials) fit as cbind(score, trials-score) (D2). Only
  # on the non-formula path (a compound formula controls its own LHS, so `trials` does not apply).
  grouped <- reg_is_grouped_binomial(family, trials, !is.null(formula))
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
    # Phase 18z3 -- modified Poisson on a binary outcome (Zou 2004). Same binary prep as the logistic
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
    # Phase 19e -- the ADDITIVE-RISK model: the same binary prep, an IDENTITY link, and the same
    # sandwich variance. `measure = "difference"` on a binary outcome asks for a risk DIFFERENCE, and
    # a difference of risks is what an identity link estimates directly (the marginal path estimates
    # the same quantity by g-computation over a logit fit -- two routes, one estimand, which is
    # exactly the conditional/marginal distinction the two `effect` values exist to make).
    # The identity link is not bounded, so the MLE can fail to converge; reg_fit falls back to the
    # linear probability model (gaussian identity, same sandwich) with a message -- the runtime third
    # state of the capability table.
    "rd" = {
      mdata <- reg_prep_binary(mdata, dependent, inverse_two_level_factors)
      positive_level <- attr(mdata, "positive_level")
      mdata[[dependent]] <- as.numeric(mdata[[dependent]] == positive_level)
      stats::binomial("identity")
    },
    # Phase 19e -- the RATIO OF MEANS: Poisson pseudo-maximum-likelihood with robust standard errors
    # (Santos Silva & Tenreyro 2006), i.e. the "rr" recipe on a continuous outcome. exp(coef) is the
    # ratio of adjusted means; the Poisson likelihood is a device for the log link, not a claim about
    # counts, and the sandwich is what makes it honest. The outcome must be non-negative.
    "mr" = {
      y <- suppressWarnings(as.numeric(mdata[[dependent]]))
      if (any(is.finite(y) & y < 0)) cli::cli_abort(c(
        '{.code measure = "ratio"} needs a non-negative outcome: a ratio of means is not defined when {.val {dependent}} can be negative.',
        "i" = 'Model {.code log()} of a positive outcome instead, or use {.code measure = "difference"}.'))
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
    # z15: extra terms LAST, so the fit's own term.labels end with them (the Linearity scope).
    if (length(add_terms)) rhs <- paste(c(rhs, add_terms), collapse = " + ")
    stats::as.formula(paste0(resp, " ~ ", rhs))
  }

  # Phase 18z3: "rr" ALWAYS fits through svyglm, weighted or not. A Poisson likelihood on a 0/1
  # outcome is deliberately misspecified (Var = mu, truth = mu(1-mu)), so the naive SEs are too large
  # and must be replaced by the Huber-White SANDWICH. svyglm's design-based variance IS that sandwich
  # (measured: exactly HC0 * sqrt(n/(n-1)) on a constant-weight ids=~1 design, coefficients identical to
  # glm). Reusing it rather than hand-rolling the matrix keeps ONE encoding of the variance rule -- and
  # crucially reg_build_digest() stores vcov(fit), which for an svyglm is already the sandwich, so the
  # jamovi reref byte-identity contract needs no special case. `weighted` stays FALSE for an unweighted
  # "rr": it is a whole-call scalar that a mixed table shares, so it must keep meaning "the USER gave a
  # design". The "rr" branches in reg_glance / reg_footer_stats / reg_compare_rows key on the family.
  use_svy <- reg_fam_svy_fitted(family, weighted)
  fit <- if (family == "gaussian" && !weighted) {
    stats::lm(fml, data = mdata)
  } else if (!use_svy) {
    stats::glm(fml, data = mdata, family = fam_obj)
  } else if (family == "rd") {
    # Phase 19e: the identity link needs sensible starting values (the default eta = 0 puts a fitted
    # probability outside the parameter space at once), and can still fail. Start from the OLS fit,
    # and on failure BE the OLS fit -- the linear probability model, whose sandwich SEs are the same
    # estimator of the same risk difference. The message names which of the two ran, because the
    # footer must not claim an identity-link GLM when a fallback produced the numbers.
    des0  <- make_design(mdata)
    start <- tryCatch(stats::coef(stats::lm(fml, data = mdata)), error = function(e) NULL)
    fit   <- tryCatch(
      do.call(survey::svyglm, list(fml, design = des0, family = fam_obj, start = start)),
      error = function(e) NULL, warning = function(w) NULL)
    if (is.null(fit) || !isTRUE(fit$converged)) {
      cli::cli_inform(c("!" = paste0(
        "The identity-link risk-difference model did not converge for {.val {dependent}}; ",
        "fitting the {.strong linear probability model} instead (same estimand, robust ",
        "standard errors).")))
      fit <- do.call(survey::svyglm, list(fml, design = des0, family = stats::gaussian()))
    }
    fit
  } else {
    # svyglm on the design for this model's complete cases (built or subset via make_design; an
    # unweighted "rr" gets svy_make_design's ids = ~1, weights = NULL constant-weight design).
    # WARNING (D3): call it through do.call() with the family OBJECT spliced in. Some svyglm methods
    #   rebuild their own call and re-evaluate it in the design's data enclosure, where a local named
    #   `fam_obj` does not exist -- an "object 'fam_obj' not found" error from inside survey.
    do.call(survey::svyglm, list(fml, design = make_design(mdata), family = fam_obj))
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
  over_disp <- !weighted && reg_fam_overdispersed(family, grouped)
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
  use_profile <- method == "profile" && !weighted && reg_fam_disp_known(family)
  if (method == "profile" && weighted) {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for survey-weighted ",
                                   "models; using Wald.")))
  } else if (method == "profile" && family == "rr") {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for a modified Poisson ",
                                   "fit (a quasi-likelihood); using the robust Wald interval.")))
  }

  if (use_profile) {
    # (no MASS guard: it is an Imports dependency -- see DESCRIPTION -- so it is always available.)
    ci   <- suppressMessages(stats::confint(fit, level = conf_level))   # log/native scale
    idx  <- match(td$term, stringi::stri_replace_all_regex(rownames(ci), "`", ""))
    lo   <- unname(ci[idx, 1]) * mult_vec; hi <- unname(ci[idx, 2]) * mult_vec  # scale profile bounds
    lrp  <- reg_lr_pvalues(fit)
    p_in <- unname(lrp[match(td$term, names(lrp))])
  } else {
    # z for fixed-dispersion glm (binomial/poisson, unweighted); else t on df.residual (lm, quasi*,
    # weighted svyglm, OR a 14v-ii phi-scaled poisson/grouped-binomial -- an estimated dispersion moves
    # the reference off z onto t, matching a quasi fit).
    disp_known <- !weighted && reg_fam_disp_known(family) && !scaled
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
reg_column <- function(skeleton, fit_res, model_predictors, col_var, est,
                       color, color_signif, model_family = "", method = "wald") {
  # Phase 19e: the column's SHAPE is the estimand row's -- which fmt field the estimate goes in (the
  # scale's own `est_field`), which EST_SCALES key is stamped, which display token and how many
  # digits. `effect_shape` (a two-valued "ratio"/"additive" derived from `exponentiate`) could not
  # express a THIRD shape, which is what a conditional risk difference (identity link, percentage
  # points) and a ratio of means (the `ratio` field, not `or`) both are.
  effect_shape <- if (isTRUE(est$exp)) "ratio" else "additive"
  # Phase 19l: the scale IS the estimand row's -- `est_row()` takes `scale` as a required formal, so
  # all 36 declared rows carry one and reg_estimand() aborts before returning anything else. The
  # family-sniffing fallback that stood here (reg_fam_logscale) was therefore unreachable, and it is
  # deleted with the predicate.
  scale_key    <- est$scale
  est_field    <- EST_SCALES[[scale_key]]$est_field
  disp         <- est$display %||% (if (identical(effect_shape, "ratio")) "or" else "coef")
  digits       <- if (identical(scale_key, "points")) 0L else 2L
  td  <- fit_res$tidy
  m   <- match(skeleton$term, td$term)
  est_v <- td$estimate[m]
  lo  <- td$conf.low[m]
  hi  <- td$conf.high[m]
  p   <- td$p.value[m]

  in_model <- skeleton$var %in% c("Constant", model_predictors)
  ref_lvl  <- skeleton$is_ref & skeleton$var != "Constant" & in_model
  neutral  <- if (effect_shape == "ratio") 1 else 0
  est_v[ref_lvl] <- neutral
  lo[ref_lvl]  <- NA_real_
  hi[ref_lvl]  <- NA_real_
  p[ref_lvl]   <- NA_real_

  n_rows   <- nrow(skeleton)
  # in_refrow is a UNION-skeleton row fact (any predictor's reference level + the Constant), NOT gated
  # by in_model: a model that OMITS a predictor must not blank that predictor's reference-row flag, else
  # the shared cross-column bold (tab_bold_rows ANDs in_refrow) drops its bold in a comparison. The
  # absent cell stays NA-valued (ref_lvl above zeroes only present predictors) -- only the flag changes.
  refrows  <- (skeleton$is_ref & skeleton$var != "Constant") | skeleton$var == "Constant"

  # the estimate goes in the field its SCALE declares (`or` / `ratio` / `diff`) -- one line instead
  # of a per-shape fmt() call, which is what made a third shape unrepresentable.
  fields <- stats::setNames(list(est_v), est_field)
  args <- c(
    list(n = rep(NA_integer_, n_rows)),   # Phase 14r (D): whole-model N is in the footer, not "n:"
    fields,
    list(ci_inf = lo, ci_sup = hi, pvalue = p,
         scale = scale_key, display = disp, digits = digits,
         # the model's own interval: a Wald one on the estimate's own scale (or the profile one)
         ci_method = if (identical(method, "profile")) "profile"
                     else if (identical(effect_shape, "ratio")) "wald_log" else "wald",
         color = color, color_signif = color_signif, col_var = col_var,
         comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"))
  if (identical(effect_shape, "ratio")) args <- c(args, list(ref = "1", pct_base = "row"))
  # var(Y) standardizes beta/SD(Y) for colour -- only the scales that declare `sd_from = "var"`
  if (identical(EST_SCALES[[scale_key]]$sd_from %||% "", "var"))
    args <- c(args, list(var = rep(fit_res$var_y, n_rows)))
  do.call(fmt, args)
}

# Phase 19e: `display` on a regression table, mirroring tab()'s grammar. The four values Phase 12h's
# `estimate_display` offered are kept as documented SHORTHANDS over that grammar, which is what they
# already were -- so this deletes a preset layer rather than adding machinery.
#
#   "value"          the plain estimate (unchanged)
#   "ci"             the `est_ci` token: estimate + a VISIBLE [ci_inf; ci_sup] bracket, dispatching
#                    OR vs beta on the stored scale
#   "prob"           == "{or} ({pct})"   the model-adjusted predicted probability, folded in
#   "ame"            == "{or} ({diff})"  the average marginal effect, folded in
#   any {} template  written as asked
#
# THE RULE the templates obey (KEY 8): a template may ask for an AUXILIARY quantity of the SAME fit
# (an adjusted prediction, an AME beside an odds ratio -- which is what reg_marginal() supplies
# here); it must never change the fit or the estimand. That is what keeps `measure` the only
# estimand argument while `display` stays free.
#' @keywords internal
#' @noRd
REG_DISPLAY_SHORTHANDS <- c(value = "value", ci = "est_ci",
                            prob = "{or} ({pct})", ame = "{or} ({diff})")

#' @keywords internal
#' @noRd
reg_resolve_display <- function(display) {
  if (is.null(display) || length(display) != 1L || is.na(display)) return("value")
  d <- as.character(display)
  if (d %in% names(REG_DISPLAY_SHORTHANDS)) return(unname(REG_DISPLAY_SHORTHANDS[[d]]))
  if (d %in% c("value", "est_ci")) return(d)
  # a real template: validated by tab()'s own grammar, so the two producers refuse the same things
  validate_display_template(d)
}

# Does this display FOLD a marginal quantity into the effect cell (i.e. name a field the coefficient
# path does not fill)? The one predicate behind the binomial-only guard and the reg_marginal() call.
#' @keywords internal
#' @noRd
reg_display_folds <- function(display) {
  if (display %in% c("value", "est_ci")) return(FALSE)
  fl <- tryCatch(parse_display_template(display)$fields, error = function(e) character(0))
  any(c("pct", "diff") %in% fl)
}

# Apply the resolved `display` to ONE coefficient column. Stars ride the primary token and its CI
# drives the colour; the (annotation) is a descriptive companion.
reg_apply_display <- function(col, display, skeleton, f, sp, family, design_spec, conf_level,
                              numeric_preds, model_predictors, multiplier = NULL) {
  if (identical(display, "value")) return(col)
  if (identical(display, "est_ci")) return(set_display(col, "est_ci"))
  if (!reg_display_folds(display)) return(set_display(col, display))
  # Phase 15e: the folds need a binomial coefficient model; a non-binomial column of a mixed table
  # shows the CI bracket instead (the whole-call degrade only fires when NO outcome is binomial).
  if (!identical(family, "binomial")) return(set_display(col, "est_ci"))

  fields   <- parse_display_template(display)$fields
  want_pct <- "pct" %in% fields
  marg     <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                           at = "average", want_pred = want_pct, multiplier = multiplier)
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_const <- skeleton$var == "Constant"
  is_ref   <- skeleton$is_ref & !is_const & in_model
  disp     <- get_display(col)
  ok       <- in_model & !is_const
  if (want_pct) {
    prd    <- marg$pred
    pred_v <- if (nrow(prd)) prd$pred[reg_skel_match(skeleton, prd)] else rep(NA_real_, nrow(skeleton))
    col    <- vctrs::`field<-`(col, "pct", pred_v)
    ok     <- ok & !is.na(pred_v)
  }
  if ("diff" %in% fields) {
    amt    <- marg$ame
    ame_v  <- amt$ame[reg_skel_match(skeleton, amt)]
    ame_v[is_ref] <- NA_real_                                # reference level has no marginal effect
    col    <- vctrs::`field<-`(col, "diff", ame_v)
    ok     <- ok & !is_ref & !is.na(ame_v)
  }
  # D22: a template is written only where every field it names exists -- elsewhere the cell keeps
  # its plain estimate rather than silently printing a substitute quantity.
  disp[ok] <- display
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
# reg_crude_y() -- Phase 18z8-B: the outcome ON THE SCALE THE CRUDE ESTIMATOR AVERAGES. For a binary
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

# reg_crude_yw() -- Phase 18z10: reg_crude_y()'s generalisation, the ONE description of "what the
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

# reg_level_counts() -- Phase 18z13 (SS7.1): the N behind each predictor level, on the model's own
# complete-case frame, aligned to the skeleton. STROBE asks for the unadjusted numbers behind an
# association and both comparable packages always print them; tabxplor already HAD the number (it is
# `emp_n` in reg_empirical()'s grid) but only under `empirical = TRUE`, only for families with a crude
# twin, and only in the html tooltip. This is its family-free generalisation: the two cannot disagree,
# both being counts of rows of the same frame.
#
# NA on a numeric predictor's row and on the Constant is deliberate: on a listwise-complete frame that
# count is nrow(frame) for EVERY numeric predictor, so a per-row cell would look specific and not be
# (the same reasoning z9 applied to the crude base cell). The Constant row shows the model N, which is
# the denominator every other cell is a part of.
#' @keywords internal
reg_level_counts <- function(frame, skeleton, wt = NULL) {
  n  <- rep(NA_integer_, nrow(skeleton))
  wn <- rep(NA_real_,    nrow(skeleton))
  w  <- if (!is.null(wt) && wt %in% names(frame)) as.numeric(frame[[wt]]) else NULL
  n[skeleton$var == "Constant"] <- nrow(frame)
  if (!is.null(w)) wn[skeleton$var == "Constant"] <- sum(w, na.rm = TRUE)
  for (v in setdiff(unique(skeleton$var), "Constant")) {
    if (!v %in% names(frame) || !reg_is_factor_var(frame[[v]])) next
    lv  <- as.character(frame[[v]])
    idx <- which(skeleton$var == v)
    m   <- match(as.character(skeleton$level)[idx], lv)   # a level absent from the frame stays NA
    cnt <- tapply(rep(1L, length(lv)), lv, sum)
    n[idx] <- as.integer(cnt[as.character(skeleton$level)[idx]])
    n[idx][is.na(m)] <- NA_integer_
    if (!is.null(w)) {
      wcnt <- tapply(w, lv, sum, na.rm = TRUE)
      wn[idx] <- as.numeric(wcnt[as.character(skeleton$level)[idx]])
    }
  }
  list(n = n, wn = if (is.null(w)) rep(NA_real_, nrow(skeleton)) else wn)
}

# The zero-row shape of reg_empirical()'s long tibble -- ONE definition, so the empty case cannot drift
# from the populated one (Phase 18z9).
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

# reg_empirical() -- Phase 18z10: THE crude grid, keyed (var, level, category).
#
# DESIGN -- this ONE producer replaces reg_empirical() + reg_empirical_tips(), which were the same
# computation at two key widths (measured line by line: the tips' `sum(w[m & y == cat]) / sum(w[m])`
# is bit-identical to the old binary branch's `wpos / (wpos + wneg)`). The tips version was simply the
# general K-category form; the old binary one was its K = 2, positive-level-only slice. Merging them is
# what lets multinomial and ordinal have a crude counterpart at all, at the cost of one extra key column.
#
# Two PARTS, because a family may need either or both (Phase 18z10 ruling: a grouped binomial shows a
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
# Weighted rule (SS14): weighted proportions/means, unweighted `n`, and a SEPARATE effective n
# (`n_ci` / `n_draw`) for the intervals. Phase 18z14-iii makes that base come from the SAME
# producers tab()'s cells use (svy_inference_basis): a survey DESIGN passed as `data` ->
# Korn-Graubard's device on the design variance; else the EXACT flat closed form on the weights
# (svy_flat_neff_rows); else the raw count. The basis is FORCED weighted here (ruling 1) -- it is not
# the tab()-scoped option -- so a weighted crude column always matches the svyglm column beside it.
# Unweighted is byte-identical.
reg_empirical <- function(data, fac_preds, dependent, crude_key, positive_level, wt,
                          trials = NULL, ref_category = NULL, conf_level = 0.95,
                          design_spec = NULL) {
  yw   <- reg_crude_yw(data, dependent, crude_key, positive_level, wt, trials, ref_category)
  cats <- yw$cats
  # The basis comes from the ONE resolver, not a local option read (the drift z14-ii closed for
  # tab()). Phase 18z16-i, ruling 1: tab_reg() FORCES the weighted basis -- its crude Obs_* columns
  # must be comparable with the Model_* column beside them, which is always design/weight-based
  # (a weighted fit goes through svyglm, i.e. the Binder linearization). The tab()-scoped
  # tabxplor.design_effect option is therefore never read here.
  basis <- svy_inference_basis(design_spec, wt, force = TRUE)
  # Phase 18z16-ii: the WEIGHTED base is the flat design's own, in closed form -- the same
  # p(1-p)/Var_design device tab()'s cells use, evaluated at ids = ~1 (svy_flat_neff_rows). It replaces
  # Kish, which is that formula with the cell's own Sum(w^2) discarded (measured up to 17 % wrong in
  # either direction, and unable to move with the outcome at all). Unweighted -> the raw count,
  # byte-identical. `n_obs` is the crude frame's row count = survey's nPSU for its flat design.
  weighted <- identical(basis, "weights") || identical(basis, "design")
  n_obs    <- nrow(data)
  # Phase 18z16-iiiii (D4): a design's DEGREES OF FREEDOM. survey refers every interval to t(degf),
  # and the model columns of a design-weighted tab_reg() already are (an svyglm's df.residual IS the
  # design df) -- while the crude companions beside them were referred to z, so at degf = 8 the crude
  # bracket printed 15 % narrower than the model bracket it exists to be compared with. `Inf` (no
  # design) is a no-op: qt(p, Inf) is bit-identical to qnorm(p).
  degf     <- design_spec$degf %||% Inf
  # the per-RESPONDENT weight: yw$w already carries the grouped-binomial `trials` multiplier, and the
  # ratio form wants (weight, u, v) = (w, successes, trials) -- see svy_flat_neff_rows().
  w0       <- if (identical(yw$kind, "share")) yw$w / yw$draws else yw$w
  flat_neff <- function(keep, u, v, raw, num = NULL) {
    if (!weighted) return(as.double(raw))
    ne <- svy_flat_neff_rows(w0[keep], u[keep], v[keep], n_obs, num = num)
    if (isTRUE(is.finite(ne) && ne > 0)) ne else as.double(raw)
  }
  has_num <- !is.null(yw$num)
  has_cat <- !identical(yw$kind, "numeric")
  share   <- identical(yw$kind, "share")
  # Phase 18z16-iv (W-E): the difference-CI method is the FAMILY's declared one (REG_EMPIRICAL is
  # the single source), never a literal written here. "wald" is the fallback for a key that declares
  # none -- it is what tab_reg() uses throughout.
  emp_method_diff <- REG_EMPIRICAL[[crude_key]]$method_diff %||% "wald"
  # variance only where a mean column is actually built (gaussian / poisson / the grouped mean score)
  want_var <- has_num
  # Phase 18z9: a TYPED zero-row return. purrr::map_dfr over character(0) yields a 0x0 tibble, whose
  # columns are NULL -- reg_empirical_columns() then errors ("Can't recycle input of size 0").
  if (length(fac_preds) == 0L) return(reg_empirical_empty())

  # --- Phase 18z14-iii: the DESIGN-based effective n --------------------------------------------
  # A crude cell IS a weighted mean over a domain (the predictor level), so its design variance is the
  # producer R/survey-variance.R already owns -- the same influence vector reg_crude_if_maker() builds
  # for the gap test (its identity-link leg w(y-mu)/Sum(w) IS svy_var_mean()'s wf*d*(x-M)/B), but
  # batched one svyrecvar call per quantity and scattered through svy_var_prep()'s `at`, which is what
  # a CALIBRATED design needs. Every crude interval then follows for free: they all consume `n_ci` or
  # `n_draw`, and on an effective base the Woolf and Katz brackets ARE Var_design(logit p) and
  # Var_design(log p) by construction.
  # Phase 18z16-iiiii: a LOCAL latch, and the reason travels OUT on the returned grid
  # (attr "degrade"), which reg_build() harvests into the basis it stamps -- the process-global
  # degrade environment is gone, so one degraded table can no longer mislabel every later one.
  said <- FALSE
  degrade <- function(reason = NULL) {
    if (!said) { svy_var_degraded(reason); said <<- TRUE }
    NULL
  }
  # z16-ii: a FLAT svydesign(ids = ~1) has the closed form as its exact answer (verified: identical
  # to svyrecvar here), so it takes the algebraic path -- no influence matrix, no ceiling.
  need_svy <- !is.null(design_spec$design) && !svy_design_is_flat(design_spec$design)
  prep <- if (need_svy) svy_var_prep(design_spec$design, data[[svy_row_col]]) else NULL
  if (need_svy && is.null(prep)) degrade()
  if (!is.null(prep)) {
    # the grid's own weights must BE the design's, or the printed estimate and the variance beside it
    # would describe two different populations.
    wg <- prep$w[prep$at] * yw$draws
    if (length(wg) != length(yw$w) || anyNA(wg) ||
        !isTRUE(max(abs(wg - yw$w)) <= 1e-8 * max(1, max(abs(yw$w))))) { degrade(); prep <- NULL }
  }
  # Var_design per level: `$p` an nl x nc matrix (the share of each outcome category), `$m` an nl x 1
  # (the numeric mean). The domain keys are the level INDEX, so the domain is `ok & x == l` by
  # construction and a predictor level literally named "Total" cannot trip svy_group_map()'s rule.
  design_var <- function(x, ok, lv) {
    if (is.null(prep) || !length(lv)) return(NULL)
    keys  <- list(as.character(seq_along(lv)))
    mkeys <- list(as.character(match(as.character(x), lv)))
    hide  <- function(v) ifelse(ok, as.numeric(v), NA_real_)
    xs_p  <- if (share) list(hide(yw$y))
             else lapply(stats::setNames(nm = cats), function(k) hide(as.character(yw$y) == k))
    rp <- if (has_cat) svy_var_mean(prep, keys, 0L, mkeys, xs_p, wmult = yw$draws) else NULL
    rm <- if (has_num) svy_var_mean(prep, keys, 0L, mkeys, list(hide(yw$num)))    else NULL
    if ((has_cat && is.null(rp$v)) || (has_num && is.null(rm$v)))
      return(degrade(rp$reason %||% rm$reason))
    list(p = rp$v, m = rm$v)
  }

  out <- purrr::map_dfr(fac_preds, function(p) {
    x  <- data[[p]]
    ok <- !is.na(x) & !is.na(yw$w) & !is.na(yw$y)
    if (has_num) ok <- ok & !is.na(yw$num)
    lv <- levels(forcats::fct_drop(as.factor(x[ok])))
    dv <- design_var(x, ok, lv)
    per <- purrr::map(seq_along(lv), function(i) {
      l  <- lv[[i]]
      m  <- ok & x == l
      wl <- sum(yw$w[m])
      # "share": y is the per-row SHARE of successes (0/1 for an ordinary binary outcome, succ/trials
      # for a grouped one), so the 2x2 legs are Sum(w*y) / Sum(w*(1-y)) -- which for 0/1 IS the indicator
      # sum the binary arm always computed. "labels": one indicator per outcome category.
      wc <- if (!has_cat) NA_real_
            else if (share) stats::setNames(c(sum(yw$w[m] * yw$y[m]), sum(yw$w[m] * (1 - yw$y[m]))),
                                            cats)
            else vapply(cats, function(k) sum(yw$w[m & yw$y == k]), numeric(1))
      # z16-ii: the CI base of a PROPORTION is now its own flat-design effective n, per CATEGORY --
      # the ratio p_k = Sum(w u_k) / Sum(w v) with (u, v) = (successes, trials) for a share and
      # (indicator, 1) for a label. For a grouped binomial that is the number of independent Bernoulli
      # DRAWS the level is worth, which is why it is not n x trials.
      draw_ne <- if (!has_cat) NA_real_ else vapply(cats, function(k) {
        u <- if (share) (if (identical(k, cats[[1]])) yw$y else 1 - yw$y) * yw$draws
             else as.numeric(yw$y == k)
        flat_neff(m, u, yw$draws, sum(m) * mean(yw$draws[m]))
      }, numeric(1))
      out <- list(
        n     = sum(m),
        n_ci  = flat_neff(m, yw$draws, yw$draws, sum(m)),
        n_draw = unname(draw_ne),
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
        # the numeric part re-derives its own effective n from the per-respondent weights: the mean
        # twin of the same closed form, s^2 / Var_design(x_bar).
        out$n_ci <- if (!weighted) as.double(n1) else {
          ne <- svy_flat_neff_rows(nw[m], yw$num[m], rep(1, sum(m)), n_obs, num = out$var)
          if (isTRUE(is.finite(ne) && ne > 0)) ne else as.double(n1)
        }
      }
      # z14-iii: the design supersedes it, per level, with Korn & Graubard's device -- the very rule
      # z14-ii writes into tab()'s own n_eff field. A level whose variance came back non-finite or
      # <= 0 keeps the flat / raw base rather than losing its interval.
      if (!is.null(dv)) {
        if (has_cat && !is.null(dv$p)) {
          nd <- out$prop * (1 - out$prop) / dv$p[i, ]
          out$n_draw <- ifelse(is.finite(nd) & nd > 0, nd, out$n_draw)
        }
        if (has_num && !is.null(dv$m)) {
          nc <- out$var / dv$m[i, 1L]
          if (isTRUE(is.finite(nc) && nc > 0)) out$n_ci <- nc
        }
      }
      # keep the two identities the pre-z14-iii code had by construction: a numeric outcome has one
      # base, a categorical one without a mean column likewise.
      if (!has_cat)     out$n_draw <- rep(out$n_ci, length(cats))
      else if (!has_num) out$n_ci  <- out$n_draw[[1]]
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
    # z14-iii: n_draw is per (level, CATEGORY) -- flat(), not rep_lv() -- so a design variance is not
    # averaged away. The reference twin repeats the reference LEVEL's vector once per level, i.e. it
    # pairs each cell with its OWN category. `n_ci` stays per level: a mean has no category.
    n_draw <- flat("n_draw"); r_n_draw <- rep(ref$n_draw, times = nl)
    # the crude ODDS ratio (category vs the reference CATEGORY, level vs the reference LEVEL) where the
    # outcome has categories; the crude RATE ratio (mean / reference mean) where it does not.
    # WARNING: the divisor is the reference LEVEL's own wpos/wneg, i.e. the SAME expression as the
    # numerator -- not the algebraically-equal `ref$prop / ref$prop[ref_cat]`, whose last bit differs
    # and made the reference cell print "1/1" (an OR of 1 - 1e-16 renders as its own reciprocal).
    emp_ratio <- if (has_cat) {
      (wpos / wneg) / rep(unname(ref$wpos / ref$wneg), times = nl)
    } else meanv / rmean
    pw <- if (has_cat) ci_wilson(prop, n_draw, conf_level = conf_level, df = degf) else
      list(inf = rep(NA_real_, nl * nc), sup = rep(NA_real_, nl * nc))
    # Phase 18z16-iv (W-E): the family's DECLARED difference method, not a second hard-coded one.
    # This interval's only consumer is the multinomial html tooltip, which was Newcombe while the
    # Obs_% column of the same table was Wald -- one quantity, two methods, inside one table. The
    # cross-table difference from tab(ci = "diff")'s Newcombe is deliberate (Phase 16d: the crude
    # companion matches the model AME's Wald so the merged legend can name ONE method).
    dd <- if (has_cat) ci_prop_diff(prop, n_draw, rprop, r_n_draw, conf_level = conf_level,
                                    method = emp_method_diff, want_p = FALSE, df = degf) else pw
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
  # z16-iiiii: the degrade travels OUT with the grid it describes. reg_build() harvests it into the
  # basis it stamps on the columns ("design_partial"), so the fact reaches the footer without any
  # process-global state -- and a grid computed for one table cannot label another.
  structure(out, degrade = said)
}

# reg_empirical_fit() -- Phase 18z9 (numeric predictors) / z10 (ordinal outcomes): the crude
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
                              other_preds = character(0), est = NULL, wt = NULL,
                              want_fit = FALSE, marginal = FALSE, trials = NULL,
                              shape_terms = NULL) {
  if (length(preds) == 0L) return(list(est = list(), fits = list()))
  # Phase 19e: "is the marginal contrast a RATIO?" is the estimand row's own `comparison`.
  ratio  <- !is.null(est) && identical(est$comparison, "lnratioavg")
  skey   <- reg_skel_key(skeleton$var, skeleton$level)
  rows   <- list()
  fits   <- list()
  for (v in preds) {
    f <- tryCatch(
      # Phase 18z15: the crude fit takes the SAME shape as the model's (`add_terms`), so a curved
      # predictor's two rows both get an observed twin and its term names are IDENTICAL to the model's
      # -- which is the whole reason the alignment below needs no shape-aware branch.
      suppressMessages(reg_fit(data, dependent, v, family, design_spec, do_exp = FALSE,
                               inverse, conf_level, method,
                               trials = trials, formula = NULL, multiplier = multiplier,
                               drop_extra = setdiff(other_preds, v),
                               add_terms = reg_shape_add(shape_terms, v))),
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
      # the NATIVE (link) scale, and reg_fit_overlay() re-exponentiates per the shape's own scale.
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


# reg_fit_overlay() -- Phase 18z9 (as reg_num_overlay) / z10: write fit-derived crude rows into a
# finished crude EFFECT column and into the crude effect VECTOR, at the ONE point both are in hand.
#
# DESIGN -- why here and not before emp_col(). On the binomial `ame` branch the base column and the
# effect column are built from the SAME `rd_fields` list, and REG_EMPIRICAL$binomial$base declares
# `color = "diff"` -- so overlaying the estimate into those shared locals would have written the AME into
# `Obs_%`'s `diff` field and COLOURED a cell that displays nothing. emit() is the one place the effect
# shape is known and only the effect column is touched.
#
# The estimate lands in the field its `scale` declares (fmt_center_field()'s rule), exp()d exactly when
# that scale is `odds_ratio` -- which is also what tells this function whether the shape is an
# exponentiated effect or its log twin. `n` is deliberately left NA: like the model column's, a fit-derived row's base is the
# whole model N, which belongs in the footer, not in a per-cell "n:".
#' @keywords internal
reg_fit_overlay <- function(col, eff, est, shape) {
  if (is.null(est) || !nrow(est)) return(list(col = col, eff = eff))
  idx <- est$row
  e <- est$est; lo <- est$lo; hi <- est$hi; p <- est$p
  scl <- EST_SCALES[[shape$scale]]
  if (isTRUE(scl$mult)) {
    e <- exp(e); lo <- exp(lo); hi <- exp(hi)
  }
  fld <- scl$est_field
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
# the base descriptive column + the crude-effect column (fmt scale / pct_base / display / digits / ref /
# colour measure + the visible name), plus the CI METHOD literal the crude interval uses. The per-family
# CI MATH stays code below (ci_prop_diff / ci_or / ci_pivot / ci_mean_diff2 / ci_mean_ratio take
# different arguments), but the near-identical fmt() calls collapse into ONE builder (emp_col), and the
# `method_*` literals are the SAME the colour legend names -- each shape row also declares the engine
# its own column's bounds were built with (`ci_method`, Phase 19b), stamped by emp_col()
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
# Phase 18z8-B: each EFFECT row also carries the `link` of the crude estimator it describes -- the
# one fact reg_crude_if_maker() needs to write its closed-form influence function (g'(mu) = 1/(mu(1-mu))
# logit | 1/mu log | 1 identity). It sits on the SHAPE row, not on the family, because the crude link
# follows the chosen ESTIMAND: a binomial model shows a logit-scale OR by default, an IDENTITY-link risk
# difference under effect = "ame", and a LOG-link risk ratio under "ame_ratio" (which reuses
# REG_EMPIRICAL$rr$rr verbatim -- the very reuse that makes a per-family link impossible). A `base` row
# is descriptive, never an effect, so its link is NA.
REG_EMPIRICAL <- list(
  binomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    base   = list(nm = "Obs_%",       scale = "points", display = "pct", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = NA_character_),
    ame    = list(nm = "Obs_diff",    scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = "identity"),
    or     = list(nm = "Obs_OR",      scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",    ci_method = "woolf", color = "OR",   link = "logit"),
    or_log = list(nm = "Obs_log(OR)", scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "woolf", color = "diff", link = "logit")),
  # Phase 18z3 -- the modified-Poisson (binary outcome) crude companion. SAME base column as binomial
  # (a risk, `Obs_%`, with the Wald risk-difference CI), but the effect is a crude RISK ratio with the
  # KATZ log-RR interval (ci_katz_rr) -- not the Woolf log-OR the binomial arm uses. That is the point
  # of the whole feature: the observed companion must be on the same scale as the model column.
  rr = list(
    method_diff = "wald", coef = "rr", coef_log = "rr_log",
    base   = list(nm = "Obs_%",       scale = "points", display = "pct", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = NA_character_),
    ame    = list(nm = "Obs_diff",    scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = "identity"),
    rr     = list(nm = "Obs_RR",      scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",    ci_method = "katz", color = "OR",   link = "log"),
    rr_log = list(nm = "Obs_log(RR)", scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "katz", color = "diff", link = "log")),
  # Phase 19e -- the crude companion of a RATIO OF MEANS (`measure = "ratio"` on a continuous
  # outcome, fitted by the "mr" log-link pseudo-likelihood). Its base is the group MEAN and its
  # effect the crude ratio of means, with the ci_mean_ratio engine tab() has used for years -- the
  # same "the observed companion must be on the same scale as the model column" rule that gave "rr"
  # its own block rather than borrowing binomial's.
  mr = list(
    method_mean_ratio = "quasipoisson", coef = "mr", coef_log = "mr_log",
    base   = list(nm = "Obs_mean",     scale = "level_mean", display = "mean", digits = 2L, ref = NA_character_, pct_base = "none", ci_method = "student",     color = "",      link = NA_character_),
    mr     = list(nm = "Obs_MR",       scale = "mean_ratio", display = "ratio", digits = 2L, ref = "1", pct_base = "none", ci_method = "quasipoisson", color = "ratio", link = "log"),
    mr_log = list(nm = "Obs_log(MR)",  scale = "log_coef",   display = "coef", digits = 2L, ref = NA_character_, pct_base = "none", ci_method = "quasipoisson", color = "diff",  link = "log")),
  gaussian = list(
    method_mean_diff = "student", coef = "diff", coef_log = "diff",
    base = list(nm = "Obs_mean", scale = "level_mean", display = "mean", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "student", color = "",     link = NA_character_),
    diff = list(nm = "Obs_diff", scale = "raw_diff",  display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "student", color = "diff", link = "identity")),
  poisson = list(
    method_mean_ratio = "quasipoisson", coef = "irr", coef_log = "irr_log",
    base    = list(nm = "Obs_rate",     scale = "mean_ratio", display = "mean", digits = 2L, ref = "1", pct_base = "none", ci_method = "quasipoisson", color = "ratio", link = NA_character_),
    irr     = list(nm = "Obs_IRR",      scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",    ci_method = "katz", color = "OR",    link = "log"),
    irr_log = list(nm = "Obs_log(IRR)", scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "katz", color = "diff",  link = "log")),
  # Phase 18z10 -- the three families that had no crude twin at all.
  #
  # grouped_binomial (`trials =`): the univariable model is STILL saturated for a factor predictor, so
  # the crude OR is the existing Woolf 2x2 on the SUMMED counts (measured identical to a univariable glm
  # to 1.1e-8). Its BASE column is the mean SCORE (maintainer's ruling) -- a per-RESPONDENT quantity, so
  # it takes the gaussian base shape and reads `emp_mean`, while the effect reads the summed 2x2. That
  # one family needing both grid parts at once is why `emp_base` had to split into emp_prop / emp_mean.
  grouped_binomial = list(
    method_diff = "wald", method_mean_diff = "student", coef = "or", coef_log = "or_log",
    base   = list(nm = "Obs_mean",     scale = "level_mean", display = "mean", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "student", color = "",     link = NA_character_),
    ame    = list(nm = "Obs_diff",     scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row",  ci_method = "wald", color = "diff", link = "identity"),
    or     = list(nm = "Obs_OR",       scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",    ci_method = "woolf", color = "OR",   link = "logit"),
    or_log = list(nm = "Obs_log(OR)",  scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none",  ci_method = "woolf", color = "diff", link = "logit")),
  # multinomial: one crude column PER OUTCOME CATEGORY would double an already wide table, so these
  # shapes are `visible = FALSE` -- the crude number rides IN-CELL in the model column's `obs` field
  # (maintainer's ruling Q4, rendered as "{or} ({obs})" / "{diff} ({obs})"). `obs` is defined as "the
  # value this cell is compared to, ON THE CELL'S OWN SCALE", so an invisible shape still has to declare
  # its scale and link exactly like a visible one. The crude effect is closed-form: the univariable
  # multinomial is saturated, and its OR is the {j, ref} x {level, ref level} Woolf ratio -- the very
  # number tab(pct = "row", OR = "OR") prints.
  multinomial = list(
    method_diff = "wald", coef = "or", coef_log = "or_log",
    or        = list(nm = NA_character_, scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",   ci_method = "woolf", color = "OR",   link = "logit", visible = FALSE, per_category = TRUE),
    or_log    = list(nm = NA_character_, scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none", ci_method = "woolf", color = "diff", link = "logit", visible = FALSE, per_category = TRUE),
    ame       = list(nm = NA_character_, scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row", ci_method = "wald", color = "diff", link = "identity", visible = FALSE, per_category = TRUE),
    ame_ratio = list(nm = NA_character_, scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",   ci_method = "katz", color = "OR",   link = "log",   visible = FALSE, per_category = TRUE)),
  # ordinal: proportional odds is a CONSTRAINT, so the univariable model is NOT saturated and there is no
  # closed form (measured: the three closed-form substitutes drift by 2.4-5.4 %, of the same order as the
  # first colour break -- and the drift IS the PO violation, so it would inject a data-dependent offset
  # into a measure whose whole job is to say how far the model moved the effect). Hence `from = "fit"`:
  # a univariable polr / svyolr through reg_fit(), the same escape z9 took for numeric predictors and for
  # the same reason -- ruling Q6 (same estimand, link, CI rule, multiplier) holds by construction.
  ordinal = list(
    coef = "cumor", coef_log = "cumor_log",
    cumor     = list(nm = "Obs_cumOR",      scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",   ci_method = "wald_log", color = "OR",   link = "logit", from = "fit"),
    cumor_log = list(nm = "Obs_log(cumOR)", scale = "log_coef", display = "coef", digits = 2L, ref = NA_character_, pct_base = "none", ci_method = "wald_log", color = "diff", link = "logit", from = "fit"),
    ame       = list(nm = NA_character_, scale = "points", display = "diff", digits = 0L, ref = "tot", pct_base = "row", ci_method = "wald", color = "diff", link = "identity", visible = FALSE, per_category = TRUE, from = "fit"),
    ame_ratio = list(nm = NA_character_, scale = "odds_ratio", display = "or", digits = 2L, ref = "1", pct_base = "row",   ci_method = "wald_log", color = "OR",   link = "log",   visible = FALSE, per_category = TRUE, from = "fit"))
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
# (no shape_from_fit(): `from` is read where the numeric overlay is spliced in, and the accessor
# never acquired a caller -- deleted in 19l.)

# reg_crude_shape() -- WHICH REG_EMPIRICAL row describes the crude EFFECT of this estimand? Read by
# reg_empirical_columns()'s arms (which build the column) and by the footer wording -- two consumers,
# one fact, per Phase 17 rule 5.
# Phase 19e: it is a LOOKUP now, not a dispatch: the estimand row (R/reg-estimand.R) names its own
# `crude_fam` / `crude_shape`, so "a binary marginal RATIO reuses REG_EMPIRICAL$rr$rr" and "a family
# with no marginal crude falls back to its coefficient row" are both DECLARED per cell instead of
# being inferred from (effect, do_exp) here. `coef` / `coef_log` stay on each REG_EMPIRICAL family:
# they are that family's own facts (its coefficient-scale shape and its logged twin), read where BOTH
# twins are built at once.
#' @keywords internal
reg_crude_shape <- function(crude_key, est = NULL) {
  # Phase 19e: the SELECTION is the estimand row's own two columns (`crude_fam` / `crude_shape`), so
  # the dispatch above is gone -- including its cross-family borrow (a binary marginal RATIO reuses
  # REG_EMPIRICAL$rr$rr), which is a declared `crude_fam = "rr"` now. `crude_fam = "auto"` means "the
  # outcome's own block", which is what carries `trials` -> grouped_binomial through reg_crude_key().
  if (is.null(est)) est <- list(crude_fam = "auto", crude_shape = NA_character_)
  key <- if (!identical(est$crude_fam %||% "auto", "auto")) est$crude_fam else crude_key
  fam <- if (is.null(key) || is.na(key)) NULL else REG_EMPIRICAL[[key]]
  if (is.null(fam)) return(NULL)
  sh <- est$crude_shape
  if (is.null(sh) || is.na(sh)) sh <- fam$coef
  fam[[sh]]
}

# Does the crude effect ride IN-CELL (as `obs`) instead of drawing its own Obs_* column? One stored
# consequence of the shape, read by the footer wording and by set_obs_if()'s display fold.
#' @keywords internal
reg_crude_in_cell <- function(crude_key, est = NULL) {
  sh <- reg_crude_shape(crude_key, est)
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
# Phase 18z10 -- three structural changes, all driven by shape FACTS rather than by family names:
#   * emit() replaces two(): a shape set may draw TWO columns (base + effect, every pre-z10 family), ONE
#     (ordinal: a cumulative OR has no base -- there is no single share to show beside it), or ZERO
#     (multinomial: the crude number rides in-cell via `obs`). The old two() could only ever do two.
#   * the crude EFFECT is returned as a list keyed by OUTCOME CATEGORY ("" when the outcome has none),
#     because a multinomial / ordinal-marginal model has one column per category and each needs its own
#     `obs`. reg_build looks the column's stored `emp_key` up in it.
#   * `fit_est` (reg_empirical_fit()'s per-category estimates) fills the rows no closed form covers --
#     numeric predictors in any family (z9), and EVERY predictor under an ordinal outcome (z10).
reg_empirical_columns <- function(skeleton, emp, fac_preds, crude_key, family, est, var_y,
                                  conf_level = 0.95, color_signif = "grey_non_signif",
                                  color = NULL, fit_est = NULL, weighted = FALSE,
                                  degf = Inf) {
  fam <- REG_EMPIRICAL[[crude_key]]
  if (is.null(fam)) return(list(cols = list(), effect = NULL, shape = NULL))
  # Phase 19e: the three facts the arms below branch on, read off the ESTIMAND row rather than from
  # the (effect, do_exp) pair -- so the crude companion cannot describe a different estimand from
  # the model column it sits beside.
  marginal   <- !identical(est$effect, "coefficient")
  ratio_marg <- marginal && identical(est$comparison, "lnratioavg")
  do_exp     <- isTRUE(est$exp)
  # WHICH crude shape this estimand declares -- the arms below dispatch on it rather than on
  # (marginal, do_exp), which is what lets a CONDITIONAL risk difference (`measure = "difference"`,
  # the identity-link "rd" fit) take the same crude risk-difference column as the marginal one.
  shape_key  <- est$crude_shape %||% fam$coef
  if (is.null(shape_key) || is.na(shape_key)) shape_key <- fam$coef
  # Phase 15d: when the model is uncoloured (`color = FALSE` -> "no"), the crude companions must be
  # uncoloured too (else the table shows coloured empirical columns beside plain model columns).
  # `color[1]`: the measure may be a length-2 (text, background) vector since Phase 18z5's
  # `color = c("OR", "adjustment")` -- `color %in% ...` would then return length 2 and the `if` below
  # would error. Only the text channel decides whether the crude companions are drawn at all.
  emp_off <- !is.null(color) && color[1] %in% c("no", "")
  n_rows  <- nrow(skeleton)
  is_fac  <- skeleton$var %in% fac_preds
  # Phase 18z9 (dev/numeric_predictors_crude_counterparts.md SS11.1): the Constant is a reference row
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
  emp_col <- function(shape, fields, n_eff = NULL) {
    measure <- if (emp_off || !nzchar(shape$color)) "" else shape$color
    args <- c(fields, if (!is.null(n_eff)) list(n_eff = n_eff), list(
      scale = shape$scale, pct_base = shape$pct_base, display = shape$display, digits = shape$digits,
      ci_method = shape$ci_method %||% "",
      color = measure, color_signif = if (nzchar(measure)) color_signif else "ignore",
      col_var = shape$nm, comp_all = FALSE, in_refrow = refrows, model_family = family, role = "emp"))
    if (!is.na(shape$ref)) args$ref <- shape$ref
    do.call(fmt, args)
  }
  # Phase 18z16-iv (W-D): the effective base a crude interval was ACTUALLY computed on, stored in
  # the `n_eff` field. reg_empirical() computes it (identically to tab()'s own cell base, to 9 s.f.),
  # feeds it to ci_wilson / ci_prop_diff / ci_or / ci_pivot / ci_mean_diff2 / ci_mean_ratio -- and then
  # threw it away, so ?fmt's "the effective sample size used for this cell's CI" was false on every
  # regression column and `$n_eff` returned NA where the correction demonstrably happened. NA when
  # nothing corrected it, exactly as an unweighted tab() cell carries NA. Which of the two bases a
  # column used is a property of ITS OWN interval, so each arm passes its own (`nv_dr` for a
  # proportion / odds / risk ratio, `nv_ci` for a mean, a rate and their ratios) -- it cannot be read
  # off `shape$type` (a poisson IRR is type "row" and takes `nv_ci`).
  neff_of <- function(v) if (isTRUE(weighted)) as.double(v) else rep(NA_real_, n_rows)
  # Phase 18z5: besides the columns, return the crude EFFECT vector -- the very value the effect
  # column stores in its own estimate field, so it is already on the model column's scale (an OR beside
  # an OR, log(OR) beside a raw coefficient, a risk difference beside an AME). reg_build writes it into
  # the model columns' `obs` field, which backs `color = "adjustment"` and the `{obs}` display token.
  # Taken from the local the shape was built from -- never re-read out of the fmt column by name.
  # Phase 18z8-B: the effect SHAPE ROW travels with it, giving the gap test both facts it needs --
  # `link` (the crude estimator's link) and `scale` (proof that the crude and model columns are the
  # SAME estimand) -- and any future shape fact for free, with no new element to thread.
  # Phase 18z9/z10: the fit-derived rows are spliced HERE -- the one place the effect shape is known,
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
    if (marginal) {
      sh   <- reg_crude_shape(crude_key, est)
      cats <- names(fit_est$est)
      if (!length(cats)) return(list(cols = list(), effect = NULL, shape = sh))
      out  <- purrr::map(stats::setNames(nm = cats), function(k)
        emit(NULL, list(col = emp_col(sh, list(diff = na_v(), n = rep(NA_integer_, n_rows))),
                        vec = na_v(), shape = sh), k))
      return(list(cols = list(), shape = sh,
                  effect = purrr::flatten(purrr::map(out, "effect"))))
    }
    sh  <- fam[[if (do_exp) fam$coef else fam$coef_log]]
    fld <- if (do_exp) list(or = na_v()) else list(diff = na_v())
    return(emit(NULL, list(col = emp_col(sh, c(fld, list(n = rep(NA_integer_, n_rows)))),
                           vec = na_v(), shape = sh)))
  }

  # ---- multinomial: closed form, one crude effect per outcome category, no visible column ------------
  if (identical(crude_key, "multinomial")) {
    sh   <- reg_crude_shape(crude_key, est)
    cats <- unique(emp$category)
    if (!length(cats)) return(list(cols = list(), effect = NULL, shape = sh))
    out <- purrr::map(stats::setNames(nm = cats), function(k) {
      g <- cat_of(k)
      v <- if (ratio_marg) g$emp_ratio_prop
           else if (marginal) g$emp_diff
           else if (do_exp)   g$emp_ratio else log(g$emp_ratio)
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
  # Phase 18s: the CI base is the effective n -- the exact flat closed form on the weights, or the
  # design variance; unweighted it equals the raw count, so those intervals are byte-identical. The
  # displayed n/tot_n fields always keep the raw count `nv`.
  nv_ci <- g$emp_n_ci; rn_ci <- g$emp_ref_n_ci
  # the CI base of a PROPORTION is the number of Bernoulli DRAWS (n x trials for a grouped binomial,
  # n everywhere else -> byte-identical); the MEAN CIs keep the per-respondent n_ci.
  nv_dr <- g$emp_n_draw; rn_dr <- g$emp_ref_n_draw

  # binomial + "rr" (modified Poisson) share every BASE fact -- a crude risk and its Wald risk-difference
  # CI -- and differ only in the crude EFFECT, which must be the model's own estimand (Phase 18z3).
  # Phase 18z10: grouped_binomial shares the EFFECT facts (a Woolf OR on the summed 2x2 legs) but not
  # the base -- its base column is the mean SCORE, built below like the gaussian one.
  binary_like <- reg_fam_binary(crude_key) || identical(crude_key, "grouped_binomial")
  if (binary_like) {
    grouped <- identical(crude_key, "grouped_binomial")
    rd <- na_ref(ci_prop_diff(prop, nv_dr, rprop, rn_dr, conf_level = conf_level, # crude risk-difference
                              method = fam$method_diff, want_p = TRUE, df = degf))
    rd_fields <- list(pct = prop, diff = diffv, n = nv, tot_n = nv,
                      ci_inf = rd$inf, ci_sup = rd$sup, pvalue = rd$pvalue)
    base <- if (grouped) {
      # the mean SCORE and its one-sample t interval (the gaussian base shape, on the numeric part)
      cell <- ci_pivot(meanv, sqrt(varv / nv_ci), df = df_or_design(nv_ci - 1, degf),
                       conf_level = conf_level, want_p = FALSE)
      list(col = emp_col(fam$base, list(mean = meanv, var = varv, n = nv, tot_n = nv,
                                        ci_inf = cell$inf, ci_sup = cell$sup),
                         n_eff = neff_of(nv_ci)), shape = fam$base)
    } else list(col = emp_col(fam$base, rd_fields, n_eff = neff_of(nv_dr)), shape = fam$base)

    if (identical(shape_key, "ame")) { # a DIFFERENCE of risks (marginal AME or conditional "rd")
      sh <- reg_crude_shape(crude_key, est)
      return(emit(base, list(col = emp_col(sh, rd_fields, n_eff = neff_of(nv_dr)),
                             vec = diffv, shape = sh), cat1))
    }
    # Phase 18z3: a marginal RATIO's crude twin is the crude RISK ratio with the Katz log-RR interval
    # -- on the binomial model path as well as the "rr" one, since the estimand is what must match, not
    # the fitted family. Always exponentiated: `exponentiate` is ignored for marginal effects. The Obs_RR
    # shape is defined once, in REG_EMPIRICAL$rr, and reused here rather than duplicated per family.
    if (ratio_marg && !identical(crude_key, "rr")) {
      rr_ci <- na_ref(ci_katz_rr(prop, nv_dr, rprop, rn_dr, conf_level = conf_level,
                                 want_p = TRUE, df = degf))
      sh    <- reg_crude_shape(crude_key, est)
      return(emit(base, list(col = emp_col(sh, list(or = prop / rprop, n = nv, ci_inf = rr_ci$inf,
                                                    ci_sup = rr_ci$sup, pvalue = rr_ci$pvalue),
                                    n_eff = neff_of(nv_dr)),
                             vec = prop / rprop, shape = sh), cat1))
    }
    # binomial / grouped -> the crude ODDS ratio (the 2x2 legs vs the reference level's) with the Woolf
    # log-OR interval. "rr" -> the crude RISK ratio (prop/rprop) with the Katz log-RR interval. WARNING:
    # `ratio` (emp_ratio) is an ODDS ratio -- feeding it to an Obs_RR column would print an OR under an
    # RR header. Phase 18z10: the 2x2 legs come from the grid (emp_wpos / emp_wneg) instead of being
    # rebuilt as prop * n -- for a grouped binomial the base is Sum(w * trials), not the respondent
    # count, and only the legs know that.
    is_rr  <- identical(crude_key, "rr")
    eff_v  <- if (is_rr) prop / rprop else ratio
    eff_ci <- na_ref(if (is_rr)
      ci_katz_rr(prop, nv_dr, rprop, rn_dr, conf_level = conf_level, want_p = TRUE, df = degf)
    else
      # the SS14 rule, unchanged: WEIGHTED proportion x UNWEIGHTED base, so the base cancels out of the
      # log-OR. For a grouped binomial that base counts DRAWS (n x trials), which is what makes the crude
      # OR equal a univariable glm(cbind(s, q - s) ~ x) rather than an OR on respondent counts.
      ci_or(prop * nv_dr, (1 - prop) * nv_dr,
            rprop * rn_dr, (1 - rprop) * rn_dr, conf_level = conf_level, want_p = TRUE, df = degf))
    sh_exp <- fam[[fam$coef]]
    sh_log <- fam[[fam$coef_log]]
    if (do_exp) {
      eff_col <- emp_col(sh_exp, list(or = eff_v, n = nv, ci_inf = eff_ci$inf,
                                      ci_sup = eff_ci$sup, pvalue = eff_ci$pvalue),
                         n_eff = neff_of(nv_dr))
      return(emit(base, list(col = eff_col, vec = eff_v, shape = sh_exp), cat1))
    }
    # Phase g: exponentiate = FALSE -> the crude companion is the LOGGED effect (Obs_log(OR) /
    # Obs_log(RR)): the log ratio in the `diff` field with the logged CI, i.e. the exact Wald interval
    # on the log scale -- the same link scale as the raw model coefficient.
    eff_col <- emp_col(sh_log, list(diff = log(eff_v), n = nv, ci_inf = log(eff_ci$inf),
                                    ci_sup = log(eff_ci$sup), pvalue = eff_ci$pvalue),
                       n_eff = neff_of(nv_dr))
    return(emit(base, list(col = eff_col, vec = log(eff_v), shape = sh_log), cat1))
  }

  if (identical(crude_key, "gaussian")) {
    cell <- ci_pivot(meanv, sqrt(varv / nv_ci), df = df_or_design(nv_ci - 1, degf),
                     conf_level = conf_level, want_p = FALSE)
    base_col <- emp_col(fam$base, list(mean = meanv, var = varv, n = nv, tot_n = nv,
                                       ci_inf = cell$inf, ci_sup = cell$sup),
                        n_eff = neff_of(nv_ci))
    md <- na_ref(ci_mean_diff2(meanv, varv, nv_ci, rmean, rv, rn_ci, method = fam$method_mean_diff, # pooled t = OLS
                               conf_level = conf_level, want_p = TRUE, df_design = degf))
    eff_col <- emp_col(fam$diff, list(diff = diffv, var = rep(var_y, n_rows), n = nv,
                                      ci_inf = md$inf, ci_sup = md$sup, pvalue = md$pvalue),
                       n_eff = neff_of(nv_ci))
    return(emit(list(col = base_col, shape = fam$base),
                list(col = eff_col, vec = diffv,
                     shape = reg_crude_shape(crude_key, est)), cat1))
  }

  # Phase 19e -- the RATIO OF MEANS crude twin ("mr"): the gaussian base column (a group mean and its
  # one-sample t interval) beside the crude ratio of means, with the SAME ci_mean_ratio engine the
  # poisson arm uses -- which is what makes "the observed companion is on the model's scale" true
  # for this estimand too. The `ratio` field, never `or`: mean_ratio's declared est_field.
  if (identical(crude_key, "mr")) {
    cell <- ci_pivot(meanv, sqrt(varv / nv_ci), df = df_or_design(nv_ci - 1, degf),
                     conf_level = conf_level, want_p = FALSE)
    base_col <- emp_col(fam$base, list(mean = meanv, var = varv, n = nv, tot_n = nv,
                                       ci_inf = cell$inf, ci_sup = cell$sup),
                        n_eff = neff_of(nv_ci))
    mr <- na_ref(ci_mean_ratio(meanv, varv, nv_ci, rmean, rv, rn_ci, method = fam$method_mean_ratio,
                               conf_level = conf_level, want_p = TRUE, df_design = degf))
    if (do_exp) {
      eff_col <- emp_col(fam$mr, list(ratio = ratio, n = nv, ci_inf = mr$inf,
                                      ci_sup = mr$sup, pvalue = mr$pvalue),
                         n_eff = neff_of(nv_ci))
      return(emit(list(col = base_col, shape = fam$base),
                  list(col = eff_col, vec = ratio, shape = fam$mr), cat1))
    }
    eff_col <- emp_col(fam$mr_log, list(diff = log(ratio), n = nv, ci_inf = log(mr$inf),
                                        ci_sup = log(mr$sup), pvalue = mr$pvalue),
                       n_eff = neff_of(nv_ci))
    return(emit(list(col = base_col, shape = fam$base),
                list(col = eff_col, vec = log(ratio), shape = fam$mr_log), cat1))
  }

  if (identical(crude_key, "poisson")) {
    # one crude rate-ratio CI (quasi-Poisson, = the phi-scaled model's method) drives BOTH columns.
    rr <- na_ref(ci_mean_ratio(meanv, varv, nv_ci, rmean, rv, rn_ci, method = fam$method_mean_ratio,
                               conf_level = conf_level, want_p = TRUE, df_design = degf))
    base_col <- emp_col(fam$base, list(mean = meanv, ratio = ratio, n = nv, tot_n = nv,
                                       ci_inf = rr$inf, ci_sup = rr$sup, pvalue = rr$pvalue),
                        n_eff = neff_of(nv_ci))
    if (do_exp) {
      eff_col <- emp_col(fam$irr, list(or = ratio, n = nv, ci_inf = rr$inf,
                                       ci_sup = rr$sup, pvalue = rr$pvalue),
                          n_eff = neff_of(nv_ci))
      return(emit(list(col = base_col, shape = fam$base),
                  list(col = eff_col, vec = ratio, shape = fam$irr), cat1))
    }
    # Phase g: exponentiate = FALSE -> the crude companion is Obs_log(IRR): log(rate-ratio) in `diff`
    # with the logged rate-ratio CI (the same link scale as the raw Poisson coefficient).
    eff_col <- emp_col(fam$irr_log, list(diff = log(ratio), n = nv, ci_inf = log(rr$inf),
                                         ci_sup = log(rr$sup), pvalue = rr$pvalue),
                        n_eff = neff_of(nv_ci))
    return(emit(list(col = base_col, shape = fam$base),
                list(col = eff_col, vec = log(ratio), shape = fam$irr_log), cat1))
  }

  list(cols = list(), effect = NULL, shape = NULL)
}

# === the model-vs-observed GAP standard error (Phase 18z8-B) =====================================

# reg_same_estimand() -- do the crude companion and the model column measure the SAME thing? Both
# declare their SCALE (the shape row's, the column attribute's), so this is one fact comparison rather
# than a family/effect table kept in sync. It gates BOTH `obs` and its gap SE, which closes a z5
# defect: reg_empirical_columns() ignores `effect` on the poisson branch, so effect = "ame" paired an
# ADDITIVE count AME with the crude rate RATIO and z5 scored the difference of two scales. Phase 19b
# made the comparison strictly sharper -- `points` vs `raw_diff` used to be one value ("diff"), and
# they are two genuinely different estimands (percentage points vs the outcome's own units).
#' @keywords internal
reg_same_estimand <- function(shape, col)
  !is.null(shape) &&
  identical(as.character(shape$scale)[1], as.character(get_scale(col))[1])

# reg_same_frame() -- were the model and its crude companion fitted on the SAME observations? The crude
# frame drops on `union_predictors` (+ whatever `na` shares), the model on its own predictors; the first
# is a SUBSET of the second, so equal row counts PROVE row identity -- both come from
# reg_complete_frame()'s drop_na, which preserves order.
#
# Phase 18z13 (D1): the TWIN of reg_same_estimand, and it gates the same two things. It used to be an
# inline clause in reg_gap_se_columns only, so a model fitted on more rows than the observed block got no
# TEST but kept its descriptive COLOUR -- the code knew the two numbers were not comparable and coloured
# their difference anyway. Under the default `na = "drop_by_outcome"` it is true by construction; it
# still fires under `na = "drop_by_model"` and on the compound-formula path, which is exactly where a
# colour would otherwise assert an adjustment that is really listwise deletion.
#
# `nobs` IS nrow(mdata) on every fit record (reg_fit and its 3+ level siblings all set it that way), and
# it survives the jamovi digest path where the fitted object -- and with it `$data` -- was distilled
# away. Reading it there keeps `obs` (hence the `{obs}` display and its tooltip) alive in the live UI,
# where the frames cannot differ anyway: `reref` requires a single non-comparison model, so its
# predictors ARE the union.
#' @keywords internal
reg_same_frame <- function(mdata, f) {
  n_fit <- if (!is.null(f$data)) nrow(f$data) else f$nobs
  !is.null(n_fit) && !is.na(n_fit) && identical(as.integer(nrow(mdata)), as.integer(n_fit))
}

# reg_gap_se_columns() -- the standard error of the gap between ONE fit's effect and its observed
# (crude) counterpart, per skeleton row, so `color = "adjustment"` reads `color_signif` like every other
# measure. The maths lives in R/reg-influence.R; this is the gate and the loop.
#
# DESIGN -- the gate is five CORRECTNESS facts, each already stored somewhere, and it returns NULL
# rather than a partial column: a gap SE without an honest premise is worse than none, because MEASURES'
# force_policy closure reads an all-NA `gap_se` as "no test here" and falls back to the descriptive
# reading.
# Phase 18z17 (ruling D2): the SIXTH clause -- `"adjustment" %in% sp$color` -- is GONE. It gated a
# fact on who asked to COLOUR it rather than on whether it is valid, which held while the colour engine
# was the only reader; forest_plot() is the second, and a user who built a table without
# `color = "adjustment"` then asked for the gap band got no band and no explanation. So `gap_se` is now
# written wherever `empirical = TRUE` produced a crude twin and the five clauses hold. The cost is small
# by construction: reg_empirical_fit() already FITS the univariable crude models when `empirical = TRUE`
# (`want_fit` only decided whether to keep them), so what is added is reg_coef_if_maker() +
# reg_if_se(), ~1/8 of a fit per column.
#   * `shape`             the crude twin's REG_EMPIRICAL row: absent = no observed effect at all
#                         (multinomial, ordinal, grouped binomial) -> `obs` is already NA.
#   * `f$fit`             NULL on the jamovi digest path, where the fitted object was distilled away.
#   * scale match         the crude and model columns must be the SAME estimand. This also closes a z5
#                         defect: reg_empirical_columns() ignores `effect` on the poisson branch, so
#                         effect = "ame" pairs an ADDITIVE count AME with a MULTIPLICATIVE crude rate
#                         ratio -- z5 wrote that ratio into `obs` and scored a gap between two scales.
#   * same frame          both estimators must solve their equations on the SAME observations
#                         (reg_same_frame, shared with the `obs` write itself since z13).
#   * collapsible         maintainer ruling Q1(b): a conditional odds ratio moves under adjustment even
#                         with zero confounding, so at survey sizes the test would be "significant"
#                         everywhere for a reason no reader would take it for (SS4.1-SS4.3).
# `method = "profile"` is deliberately NOT a clause: between_groups RECOVERS its SE from the printed
# interval and a profile bracket is not est +/- crit*se, but adjustment COMPUTES its own -- profile
# there only means the printed model CI and the gap test are different quantities (SS3.8, documented).
#' @keywords internal
reg_gap_se_columns <- function(f, sp, model_col, skeleton, shape, mdata, fac_preds,
                               est, wt, fits_crude = NULL, fit_preds = character(0),
                               multiplier = NULL, category = "") {
  # Phase 19e: the estimand ROW replaces the (effect, at) pair -- the profile axis is `at_reference`,
  # the marginal ratio is the row's own `comparison`.
  effect   <- est$effect
  marginal <- !identical(effect, "coefficient")
  ratio_m  <- marginal && identical(est$comparison, "lnratioavg")
  if (is.null(shape) || is.null(f$fit) || is.null(f$data))      return(NULL)
  if (isTRUE(sp$compound) || identical(effect, "at_reference")) return(NULL)
  if (!reg_same_estimand(shape, model_col))                     return(NULL)
  if (!reg_same_frame(mdata, f))                                return(NULL)
  # reg_estimand_collapsible() keeps its own vocabulary: "is the DISPLAYED estimand collapsible" is a
  # question about the contrast, and only a conditional (coefficient) odds ratio answers no.
  if (!reg_estimand_collapsible(sp$family, effect))             return(NULL)
  # svyrecvar is the linearization estimator; a REPLICATE-weights design needs withReplicates instead,
  # so degrade rather than report a linearization variance for a design that did not ask for one.
  des <- if (inherits(f$fit, "svyglm")) f$fit$survey.design else NULL
  if (inherits(des, "svyrep.design"))                           return(NULL)

  coef_if <- reg_coef_if_maker(f$fit)
  if (is.null(coef_if)) return(NULL)
  # Phase 18z10: a 3+ level outcome shows ONE COLUMN PER CATEGORY, so its marginal influence function
  # is per category too (reg_ame_if_cat_maker); the single-equation one reads family()$mu.eta, which
  # multinom / polr do not have.
  per_cat  <- inherits(f$fit, "multinom") || inherits(f$fit, "polr")
  model_if <- if (marginal && per_cat)
    reg_ame_if_cat_maker(f$fit, f$data, wt, ratio = ratio_m, category = category)
  else if (marginal)
    reg_ame_if_maker(f$fit, f$data, wt, ratio = ratio_m, coef_if = coef_if)
  else coef_if
  # Phase 18z10: `category` is the outcome category THIS column shows (a multinomial / ordinal-marginal
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
    # z14-iii: the crude leg lives on `mdata`, the model leg on the fit's row space -- the same thing
    # except on a calibrated / PPS design, which svy_domain_design() pads back to full length.
    ic <- reg_if_align(crude_if(v, as.character(skeleton$level[k]), r), length(im),
                       mdata[[svy_row_col]])
    if (is.null(ic) || length(ic) != length(im)) next
    out[k] <- reg_if_se(im - ic, des)
  }

  # Phase 18z9 -- the NUMERIC arm. Same two legs, different crude side: a numeric predictor has no
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
      # Phase 18z10: the fit arm now covers FACTOR predictors too (every predictor under an ordinal
      # outcome). A factor's contrast is (level, reference level), a numeric's a k-unit forward
      # difference -- the same two shapes reg_ame_if_maker()'s own contract states.
      is_fac_k <- v %in% fac_preds
      cl <- if (is_fac_k) list(as.character(skeleton$level[k]), ref_of(v)) else list(kk, 0)
      if (is_fac_k && is.na(cl[[2]])) next
      if (marginal) {
        im <- model_if(v, cl[[1]], cl[[2]])
        ic <- if (inherits(nv$fit, "multinom") || inherits(nv$fit, "polr"))
          reg_ame_if_cat_maker(nv$fit, nv$data, wt, ratio = ratio_m,
                               category = category)
        else
          reg_ame_if_maker(nv$fit, nv$data, wt, ratio = ratio_m,
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

# Phase 18z10: reg_empirical_tips() is DELETED. It was reg_empirical() at a three-part key --
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
# Phase 18z3: `comparison = "lnratioavg"` is the RATIO twin of the default additive contrast -- the
# log of the ratio of adjusted predicted probabilities (marginal standardization / g-computation), exp()'d
# here into a risk ratio. It shares the whole multiplicative path with "lnor": same double-paren label
# shape, same exp() of the estimate and BOTH bounds (so the interval stays a Wald interval on the log
# scale, asymmetric and strictly positive once exponentiated).
# Phase 18z15 (SS12.6 defect 2) -- the ONE place a marginal effect can be silently WRONG.
# `marginaleffects` re-evaluates a poly() / ns() / bs() basis on the perturbed data, and an orthogonal
# basis absorbs a location shift exactly, so it returns AME = 0.000000 with no warning. Whether it
# happens depends on whether `insight` can recover the data, i.e. it is a coin flip, not a property of
# the model -- so the answer is to CHECK, not to refuse.
#
# The comparator is stats::predict(newdata =), which is correct here: predict() carries the basis's
# frozen `predvars` (makepredictcall), which is precisely what the perturbed-frame route loses.
# `shape = "quadratic"` emits I(((x - m)/s)^2) instead, which is correct through every route.
#' @keywords internal
reg_basis_vars <- function(fit, predictors) {
  lab <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
  hit <- grepl("\\b(poly|ns|bs|rcs)\\s*\\(", lab)
  if (!any(hit)) return(character(0))
  predictors[vapply(predictors, function(v)
    any(grepl(paste0("\\b", tolower(v), "\\b"), tolower(lab[hit]))), logical(1))]
}

#' @keywords internal
reg_marginal_basis_ok <- function(fit, data, v, k, est, ratio) {
  truth <- tryCatch({
    p0 <- stats::predict(fit, newdata = data, type = "response")
    d2 <- data; d2[[v]] <- as.numeric(d2[[v]]) + (if (is.finite(k) && k != 0) k else 1)
    mean(as.numeric(stats::predict(fit, newdata = d2, type = "response")) - as.numeric(p0),
         na.rm = TRUE)
  }, error = function(e) NA_real_)
  if (!is.finite(truth) || abs(truth) < 1e-10) return(TRUE)          # nothing to disagree about
  if (isTRUE(ratio)) return(!isTRUE(all.equal(unname(est[[1]]), 1, tolerance = 1e-8)))
  isTRUE(abs(unname(est[[1]]) - truth) <= 0.02 * abs(truth) + 1e-10)
}

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

  # Phase 18z9: `multiplier` reaches the MARGINAL path too. Before, a scaled numeric predictor kept a
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
  basis_vars <- reg_basis_vars(fit, predictors)
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
    # the basis-expansion guard, paid only where a basis exists (see reg_marginal_basis_ok)
    if (!is_fac && at == "average" && length(est) == 1L && v %in% basis_vars &&
        !reg_marginal_basis_ok(fit, data, v,
                               if (!is.null(multiplier) && v %in% names(multiplier))
                                 as.numeric(multiplier[[v]]) else 1, est, do_exp)) {
      cli::cli_warn(c(
        "!" = paste0("The marginal effect of {.val {v}} is not trustworthy: it is fitted through a ",
                     "basis expansion ({.code poly()} / {.code ns()}), which the marginal-effects ",
                     "engine re-evaluates on perturbed data."),
        "i" = 'Fit it with {.code shape = c({v} = "quadratic")} instead of a formula basis.'))
    }
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
      scale = "points", pct_base = "row", display = display, digits = 1L, ci_method = "wald",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  } else if (shape == "prob_ratio") {
    # Phase 18z3: the RATIO twin of "prob" -- a marginal RISK RATIO with the adjusted predicted
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
      scale = "odds_ratio", pct_base = "row", display = display, digits = 1L, ref = "1",
      ci_method = "wald_log",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  } else if (shape == "raw_ratio") {
    # Phase 19e (capability gap): the ratio twin of "raw" -- a marginal RATIO OF ADJUSTED MEANS (or of
    # predicted counts), which used to be refused ("needs a probability-scale outcome") although
    # tab() has given a ratio of means for years and tabxplor already owned the `mean_ratio` scale,
    # its ladder and three ci_mean_ratio engines. No adjusted PROBABILITY exists here, so there is no
    # parenthetical: the cell is the ratio alone.
    display[in_model & !is_const & !is.na(ame_v)] <- "ratio"
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    display[in_model & is_ref] <- "ratio"
    fmt(
      n = rep(NA_integer_, n_rows),
      ratio = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      scale = "mean_ratio", display = display, digits = 2L, ref = "1", ci_method = "wald_log",
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
      scale = "odds_ratio", pct_base = "row", display = display, digits = 2L, ref = "1",
      ci_method = "wald_log",
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
      # a marginal effect on the OUTCOME's scale (a gaussian AME, a poisson COUNT AME) -- never a
      # link-scale coefficient, whatever the family, which is exactly what used to make this column
      # and the raw coefficient beside it indistinguishable except through the `var` field.
      scale = "raw_diff", display = display, digits = 2L, ci_method = "wald",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  }
}

# Split ONE multinomial fit into one OR column per non-reference outcome category. Each category's
# tidy rows (`y.level == j`, y.level dropped) look like a standard glm tidy, so reg_column() aligns
# them to the shared predictor skeleton unchanged. Label = "<j> vs <ref>: OR" (prefixed by the
# dependent when several dependents / models coexist, to disambiguate). Returns a list of {label, col}.
reg_columns_multinom <- function(skeleton, f, sp, est, color, color_signif,
                                 eff_word, cleannames, prefix_dep, model_family = "multinomial",
                                 method = "wald") {
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
         col   = reg_column(skeleton, sub, sp$predictors, sp$label, est, color, color_signif,
                            model_family = model_family, method = method))
  })
}

# === Model-summary footer (Phase 12f): GOF stats stored in the `test` attribute ==================
# The regression GOF is stored in the SAME whole-table `test` tibble crosstabs use (schema
# new_test_tibble(): var/col/test/statistic/df1/df2/pvalue/n/min_e), adding ROWS with
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

# Pearson dispersion (over/under-dispersion): poisson / grouped binomial only -- the dispersion
# parameter is not identifiable for ungrouped Bernoulli data. phi = Sum(pearson resid^2) / (n - rank)
# (better-behaved than deviance/df). PURE (14v-ii): the over-dispersion warning moved to reg_fit(),
# where the SEs are now actually scaled by sqrt(phi) -- so it is emitted ONCE per fit (this helper is
# also called by reg_glance for the footer, which must stay silent).
#
# WARNING (Phase 18z15): the denominator is n - rank, computed here, NEVER stats::df.residual(fit).
# For an svyglm df.residual() is the DESIGN degrees of freedom (PSUs - strata), so the footer row read
# ~22 instead of ~1 on a weighted Poisson -- the reason the design doc believed phi could not be
# computed honestly under a design. It can; it was reading the wrong denominator. The SE-scaling caller
# is gated `!weighted`, where the two denominators agree, so only the weighted row moves.
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

  # Phase 18z3: a modified-Poisson ("rr") fit is a QUASI-likelihood on a deliberately misspecified
  # variance, so AIC / BIC / McFadden are not defined, and the Pearson dispersion of a 0/1 outcome is
  # just mean(1-mu) -- a constant of the fitted values, never a diagnostic. Report the honest pair:
  # n + the design-based Wald-vs-null. Placed FIRST so it holds weighted or not (the fit is an svyglm
  # either way); the weighted branch below keeps its Nagelkerke/AIC set for genuine survey models.
  if (family %in% REG_FIT_ONLY_FAMILIES) {
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
  if (reg_fam_overdispersed(family, grouped)) {
    phi <- reg_dispersion(fit)
    if (!is.na(phi)) out <- dplyr::bind_rows(out, row("phi", statistic = phi))
  }
  # (Phase 14q's Brant row moved to REG_CHECKS as the "Proportionality (Brant)" check, z15 -- same
  # stashed attr, same p, one vocabulary.)
  out
}

# Phase 19g (KEY 6): THE `stats =` / `check =` vocabulary, in one place. `tab_reg(stats =)` and
# `reg_check_plots(check =)` name the same things with two argument names, and each used to carry its
# own hand-written list and its own validator -- so a check could be addable in one and not the other.
# "interaction" (Phase 18z8) is not produced by reg_glance (reg_build's split block reads it straight
# off `stats`) but it belongs to the vocabulary, so a user vector does not drop it.
#' @keywords internal
REG_GOF_KEYS <- c("n", "lr_null", "wald_null", "mcfadden_r2", "nagelkerke_r2", "cox_snell_r2",
                  "r2", "r2_adj", "f_model", "sigma", "aic", "bic", "phi")

#' @keywords internal
reg_stat_keys <- function() c(REG_GOF_KEYS, "interaction", "global", names(REG_CHECKS))

# THE shared validator. `arg` names the user's argument in the message; `allowed` narrows the
# vocabulary (reg_check_plots() takes model CHECKS only). Returns the accepted keys.
#' @keywords internal
reg_validate_stat_keys <- function(x, arg = "stats", allowed = reg_stat_keys()) {
  bad <- setdiff(x, allowed)
  if (length(bad))
    cli::cli_abort(c("{.arg {arg}} must name model-fit statistics or checks.",
                     "x" = "Unknown: {.val {bad}}.",
                     "i" = "Available: {.val {allowed}}."))
  x
}

# Resolve the `stats=` argument -> the ordered set of footer discriminators. Per-context defaults:
# glm -> n/lr_null/mcfadden_r2/aic/bic (+dispersion for poisson/grouped); lm -> n/r2/r2_adj/f_model/
# sigma; weighted -> n/wald_null/nagelkerke_r2/aic. A character vector overrides (keeping its order,
# valid names only); FALSE / "none" suppresses the footer; NULL / "all" / TRUE = the default set.
reg_footer_stats <- function(family, weighted, grouped, stats) {
  # Phase 18z3: "rr" FIRST -- a quasi-likelihood has no AIC/BIC/McFadden, and binary-outcome Pearson
  # dispersion is meaningless (see reg_glance). Matches the pair reg_glance actually emits.
  default <- if (family %in% REG_FIT_ONLY_FAMILIES) c("n", "wald_null")
    else if (weighted) c("n", "wald_null", "nagelkerke_r2", "aic")
    else if (family == "gaussian") c("n", "r2", "r2_adj", "f_model", "sigma")
    else { s <- c("n", "lr_null", "mcfadden_r2", "aic", "bic")
           # Phase 18z15: `phi` is the EXACT Pearson dispersion this row has always held; the key
           # `dispersion` now names the CHECK (max robust/model SE), which every family gets below.
           if (reg_fam_overdispersed(family, grouped)) s <- c(s, "phi"); s }
  # Phase 18z13: the per-predictor global test is in the DEFAULT set -- "is this variable associated
  # at all?" is the question a multi-level factor block leaves unanswered, and it costs no extra fit.
  # Phase 18z15: so are the five model CHECKS (ruling R7 -- always, no opt-in gate). They need no new
  # argument: `stats` already IS the footer vocabulary, so each is individually removable and
  # `stats = FALSE` still hides everything. The applicable set is REG_CHECKS' own rule.
  default <- c(default, "global", reg_checks_for(family, weighted, grouped))
  if (is.null(stats) || identical(stats, "all") || isTRUE(stats)) return(reg_check_expand(default))
  if (isFALSE(stats) || identical(stats, "none")) return(character(0))
  # A user writes a check KEY ("linearity"); a `test` row carries a discriminator ("linearity_lr").
  reg_check_expand(stats[stats %in% reg_stat_keys()])
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
    reg_test_row(g$test, fit_first_col[[i]], statistic = g$statistic, df1 = g$df1, df2 = g$df2,
                 pvalue = g$pvalue, nobs = as.numeric(nobs_by_fit[[i]]))
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
  use_f  <- reg_fam_disp_estimated(family)
  # Phase 18z3: an "rr" fit is an svyglm (see reg_fit), so its comparison takes the DESIGN-BASED Wald
  # branch below whether or not the user gave a design -- a likelihood-ratio test between two
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

  row <- function(test, col_var, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_,
                  pvalue = NA_real_, nobs = NA_real_)
    reg_test_row(test, col_var, statistic = statistic, df1 = df1, df2 = df2,
                 pvalue = pvalue, nobs = nobs)

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
      # Phase 19a (D5): the advice named `na = "drop_all_models"`, a value REMOVED in z13 -- so a user
      # following it hit match.arg()'s "should be one of" error. The `na` family is
      # drop_by_outcome / drop_by_model / drop_all.
      "i" = 'A different N is usually the per-model missing-value drop; set {.code na = "drop_all"} to fit every model on the same complete cases so the likelihood-ratio test can run.'))
    row(paste0("compare_", tag, "_aic"), col, statistic = daic, nobs = fits[[i]]$nobs)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}


# --- Phase 18z8: the aggregated effect-modification test (predictor x split_var) -----------------
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
# one model column and the spread re-keys per split group; a POOLED test belongs to neither,
# and one row per predictor cannot be expressed by a fixed discriminator->label list anyway. So the
# rows stay pure data (read by reg_interaction_line, rendered as a table-wide footer STREAM like the
# weight / "Model:" lines), and both row consumers, which filter on names(reg_footer_spec()), ignore
# them -- the existing GOF footer is untouched. `col_var` is the fit's first column, so several models
# each get their own line.
# Phase 19g: the predictor rides `var` -- the same dimension a crosstab's test row uses. The split
# level rides a column named after the split variable, so the two facts can no longer collide (they
# did: the split branch overwrote `row_var` wholesale with the group level, and the line printed
# that level, repeated, instead of the predictors).
# Phase 19g (KEY 6): ONE builder for every regression `test` row. It was written out as an identical
# tibble literal in four places (GOF, model comparison, interaction, global, checks), which is how
# three of them still spelled the pre-19g key. `var` = the predictor the row is about ("" = the whole
# model); `col` = the fmt column it keys under.
#' @keywords internal
reg_test_row <- function(test, col, var = "", statistic = NA_real_, df1 = NA_real_, df2 = NA_real_,
                         pvalue = NA_real_, nobs = NA_real_)
  tibble::tibble(var = var, col = col, test = test, statistic = statistic,
                 df1 = df1, df2 = df2, pvalue = pvalue, n = nobs, min_e = NA_real_)

#' @keywords internal
reg_interaction_types <- function() c("interact_lr", "interact_f", "interact_wald")

#' @keywords internal
reg_interaction_rows <- function(reg_gof, data, specs, shared, split_var, fit_first_col) {
  weighted <- shared$weighted
  rows <- purrr::map(seq_along(specs), function(i) {
    sp <- specs[[i]]
    # No pooled interaction for the engines that are not a single glm/svyglm equation (multinomial /
    # ordinal have their own fitters), nor for the compound-formula escape hatch (the interaction of an
    # arbitrary formula is ill-defined). Degrade to no row, never to a wrong one.
    if (!reg_fam_glm(sp$family) || isTRUE(sp$compound)) return(NULL)
    preds <- sp$predictors
    if (length(preds) == 0L) return(NULL)
    f <- tryCatch(reg_fit(data, sp$dependent, preds, sp$family, shared$design_spec, sp$do_exp,
                          if (is.null(sp$inverse)) shared$inverse_two_level_factors else sp$inverse,
                          shared$conf_level, "wald", trials = sp$trials, formula = NULL,
                          multiplier = NULL, cross = split_var),
                  error = function(e) NULL)
    if (is.null(f) || is.null(f$fit)) return(NULL)
    fit      <- f$fit
    use_f    <- reg_fam_disp_estimated(sp$family)
    use_wald <- reg_fam_svy_fitted(sp$family, weighted)
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

    reg_term_tests(fit, keep, terms_i, use_f, use_wald,
                   types = c(wald = "interact_wald", f = "interact_f", lr = "interact_lr"),
                   col_var = fit_first_col[[i]], nobs = f$nobs)
  })
  rows <- purrr::compact(purrr::flatten(purrr::compact(rows)))
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}

# THE per-predictor term test (Phase 18z13). Two callers ask different QUESTIONS of the same
# computation, so it lives once: the aggregated interaction test (predictor x split_var, on a pooled
# fit) and the per-predictor global test (the predictor's own term, on the model's own fit). They
# differ only in which fit and which terms are dropped -- everything below (the Wald / F / LR ladder,
# the drop1 column-name handling, the row shape) was identical.
#
# WARNING: `terms` must come from the FIT's own term.labels, verbatim. terms() orders the parts of an
# interaction by the variable's position in the formula, so a hand-built "age:party3" comes back as
# "party3:age" and drop1() then rejects the scope. Both drop1() and regTermTest() take the labels as a
# CHARACTER vector, which skips the re-parse.
#' @keywords internal
reg_term_tests <- function(fit, preds, terms, use_f, use_wald, types, col_var, nobs) {
  if (length(terms) == 0L) return(NULL)
  if (use_wald) {
    return(purrr::map2(preds, terms, function(pv, tm) {
      e <- tryCatch({
        rt <- suppressWarnings(survey::regTermTest(fit, tm))
        list(stat = as.numeric(rt$Ftest), df1 = as.numeric(rt$df),
             df2 = as.numeric(rt$ddf), p = as.numeric(rt$p))
      }, error = function(e) NULL)
      if (is.null(e) || is.na(e$p)) return(NULL)
      reg_test_row(types[["wald"]], col_var, pv, e$stat, e$df1, e$df2, e$p, nobs)
    }))
  }
  # WARNING: capture.output, not just suppressMessages -- nnet's drop1.multinom PRINTS its progress
  # ("trying - <term>") with cat(), which no condition handler catches, and it would land in the user's
  # console on every multinomial table with a numeric predictor. Harmless for the other engines.
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
        p, nobs)
  })
}

# Phase 18z13 (SS7.2): the per-predictor GLOBAL test -- "is this variable associated with the
# outcome at all?", the one answer a block of 7 stars against a reference category cannot give, and
# the item a gtsummary user reaches for first (`add_global_p`). tabxplor's audience is almost entirely
# multi-level categorical predictors, so it is in the DEFAULT stats set.
#
# It costs NO extra fit: the model is already in hand (unlike the interaction test, which needs a
# pooled one), so this is regTermTest() on the stored vcov where a design exists and drop1() on the
# existing fit otherwise. Emitted only for terms carrying 2+ coefficients: a 1-df term's global p IS
# the single cell's p, already starred, so a line for it would be noise.
#' @keywords internal
reg_global_types <- function() c("global_lr", "global_f", "global_wald")

#' @keywords internal
reg_global_rows <- function(reg_gof, fits, specs, shared, fit_first_col) {
  weighted <- shared$weighted
  rows <- purrr::map(seq_along(specs), function(i) {
    sp <- specs[[i]]
    if (!reg_fam_glm(sp$family) || isTRUE(sp$compound)) return(NULL)
    f <- fits[[i]]
    if (is.null(f) || is.null(f$fit)) return(NULL)            # the jamovi digest path keeps no fit
    fit  <- f$fit
    have <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
    asg  <- tryCatch(stats::coef(fit), error = function(e) NULL)
    if (is.null(asg)) return(NULL)
    df_of <- tryCatch({
      a <- attr(stats::model.matrix(fit), "assign")
      vapply(seq_along(have), function(k) sum(a == k), integer(1))
    }, error = function(e) rep(NA_integer_, length(have)))
    keep    <- have %in% sp$predictors & !is.na(df_of) & df_of >= 2L
    terms_i <- have[keep]
    if (length(terms_i) == 0L) return(NULL)
    reg_term_tests(fit, terms_i, terms_i,
                   use_f = reg_fam_disp_estimated(sp$family),
                   use_wald = reg_fam_svy_fitted(sp$family, weighted),
                   types = c(wald = "global_wald", f = "global_f", lr = "global_lr"),
                   col_var = fit_first_col[[i]], nobs = f$nobs)
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
  grouped    <- reg_is_grouped_binomial(family, sp$trials, !is.null(sp$formula))
  over_disp  <- !weighted && reg_fam_overdispersed(family, grouped)
  phi        <- if (over_disp) reg_dispersion(fit) else NA_real_
  scaled     <- over_disp && !is.na(phi) && phi > 0
  disp_known <- !weighted && reg_fam_disp_known(family) && !scaled
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
  # Phase 18z9: the multiplier, applied with reg_fit()'s OWN two expressions in reg_fit()'s OWN order
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


# reg_gap_se_of() -- Phase 18z8: recover a column's per-cell standard error, on the estimate's own
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
  if (isTRUE(fmt_scale_row(col)$mult)) {          # a multiplicative scale -> the SE lives on the log
    ok <- is.finite(lo) & is.finite(hi) & lo > 0 & hi > 0
    ifelse(ok, (log(hi) - log(lo)) / (2 * crit), NA_real_)
  } else {
    ifelse(is.finite(lo) & is.finite(hi), (hi - lo) / (2 * crit), NA_real_)
  }
}

#' @keywords internal
# Phase 18z5: fill each group's `obs` field with the REFERENCE GROUP's estimate for the same row, so
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
# Phase 18z8: the same pass writes `gap_se`, so `color_signif` applies. The two groups are DISJOINT
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
  # the estimate a column stores, dispatched on its stored scale -- fmt_est_of() is the ONE such rule,
  # shared with fmt_gap_parts() and the crude numeric overlay (an `Obs_rate` column is `mean_ratio` and keeps
  # its estimate in `ratio`, not `diff`).
  est_of <- fmt_est_of
  for (i in seq_along(parts)) {
    d <- parts[[i]]$data
    m <- if (i == 1L) rep(NA_integer_, nrow(d)) else match(key_of(d), ref_k)
    for (nm in intersect(fmt_nm, names(d))) {
      if (!is_fmt(d[[nm]])) next
      # Phase 18z13 (D11): only where a gap measure can READ them. This wrote `obs`/`gap_se` on every
      # fmt column, including the `Obs_*` companions, which colour on their own `diff`/`or` measure and
      # never consult either -- a stored value with no consumer (and an "obs:" tooltip line on a column
      # whose whole point is that it IS the observed effect).
      # fmt_color_attr, not get_color: the measure is length <= 2 and a gap almost always rides the
      # BACKGROUND channel (`color = c("OR", "between_groups")`), which get_color() does not see.
      if (!any(c("adjustment", "between_groups") %in% fmt_color_attr(d[[nm]]))) next
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


# The shared builder: fit every column spec, align to one skeleton, assemble a grouped_tab. specs =
# list of list(dependent, predictors, label, trials, formula, compound). The data-skeleton (union of
# the specs' predictors) is used unless a spec is a compound formula (single model), in which case the
# skeleton is read from its fitted terms (reg_skeleton_from_fit). Fit-all first so the skeleton can
# come from the fit before the columns are aligned. A multinomial fit contributes SEVERAL columns
# (one per outcome category), so the per-spec columns are flattened into one (label, col) list.
# Phase 18z16-i: the inference basis of a regression table. Ruling 1 -- a weighted tab_reg() is
# ALWAYS on the weighted basis (its models fit through svyglm, i.e. the Binder linearization, and
# since z16-ii its crude Obs_* companions use the same closed form), so the tab()-scoped
# tabxplor.design_effect option is never read. Feeds the footer sentence, nothing else.
# `degraded` is harvested from the crude grids this build produced (attr "degrade", set by
# reg_empirical()) -- z16-iiiii, in place of the process-global degrade flag it used to read.
# Phase 19g (KEY 6): THE assembly tail, shared by BOTH branches of reg_build(). The split branch
# carried a COMPLETE duplicate of it -- its own tab_stamp_inference / new_tab / meta literal / group_by
# -- and had already drifted once (z16-iiiii found it reducing six inference keys to three, so a split
# table's legend could not name its own interval method). One function, two callers, no drift:
#   `basis`/`degf` are NULL on the split branch by design -- each group was built by the recursion and
#   stamped its OWN design df and basis on its own columns, and vec_rbind()'s fmt reconcile already
#   took the weakest of them. Re-stamping one table-wide basis there would overwrite a group whose
#   design variance succeeded with the verdict of a group that had to fall back.
#' @keywords internal
reg_finalize <- function(tab, tests, conf_level, var_labels, group_vars,
                         degf = NULL, basis = NULL, meta_extra = list()) {
  tab |>
    tab_stamp_inference(conf_level, degf, basis) |>
    new_tab(subtext = meta_extra$subtext, test = tests,
            meta = c(meta_extra[setdiff(names(meta_extra), "subtext")],
                     list(spec = reg_spec(var_labels)))) |>
    dplyr::group_by(!!!rlang::syms(group_vars))
}

# Phase 19g (KEY 6): THE typed record of every per-call setting reg_build()'s leaves and assembler
# read. It was a bare `list(...)` of 24 keys, documented as 20 in reg_build()'s header, partially
# re-listed twice more, mirrored by hand in fmt_class.R's globalVariables() to silence R CMD check,
# and with two fields declared nowhere -- so "what does reg_build receive" had four answers. The
# constructor's FORMALS are the contract now: every name is always present (a direct caller that
# omits one gets the declared default, never a missing binding), the globalVariables mirror is
# DERIVED from names(formals(new_reg_shared)), and adding a setting is one line.
# `split_var` stays a formal of reg_build(): it flips to NULL in the split recursion, and a NULL
# value cannot live in a list that must round-trip through modifyList().
# Phase 19e: `effect` and `at` LEFT the record. The estimand is a per-SPEC fact (`sp$est`, the
# R/reg-estimand.R row), so a table-scalar copy of half of it could only ever disagree with the
# columns it described -- and it did: the marginal builder was chosen by that scalar even after
# Phase 15e made the family per spec.
#' @keywords internal
new_reg_shared <- function(union_predictors = character(0), design_spec = list(), weighted = FALSE,
                           inverse_two_level_factors = TRUE, conf_level = conf_level_default(),
                           method = "wald", color_signif = "grey_non_signif", cleannames = TRUE,
                           subtext = "",
                           stats = NULL, compare = "none", baseline = NULL,
                           multiplier = NULL, multiplier_label = NULL,
                           shape_terms = NULL, shape_labels = NULL,
                           empirical = FALSE, display = "value",
                           var_labels = character(0), na_shared_vars = character(0),
                           add_n = FALSE) {
  as.list(environment())
}
# ...and THE globalVariables mirror, derived from those formals: reg_build() binds them with
# list2env(), which codetools cannot see. It lived in R/fmt_class.R as a hand-kept copy and had
# fallen behind twice.
utils::globalVariables(names(formals(new_reg_shared)))

reg_inference <- function(shared, degraded = FALSE) {
  ds <- shared$design_spec
  leaf_inference(new_inference(ds$wt, ds, force = TRUE), degraded = degraded)
}

# THE `meta$ci_settings` of a regression table -- what the colour legend names as the interval method.
reg_build <- function(data, specs, shared, split_var = NULL, .fit_cache = NULL, reference = NULL,
                      reref = FALSE, skeleton_data = data) {
  # `shared` is the TYPED record new_reg_shared() builds (Phase 19g) -- its formals ARE the contract,
  # so every field is present and a direct caller cannot be missing one. list2env() binds them as
  # locals; nothing below re-reads `shared$<field>` except where the whole record is forwarded.
  shared <- do.call(new_reg_shared, shared[intersect(names(shared), names(formals(new_reg_shared)))])
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
  # FIRST because the index columns DECLARE their roles (19f): tab_declared_vars() reads row_var =
  # "levels", tab_vars = c(split_var, "var") off the stamped columns, so the crosstab spread machinery
  # needs no change. (The old reason -- "so `levels` stays the LAST factor column" for the
  # last-factor heuristic -- is stale: that heuristic is the degraded fallback now, not the rule.)
  if (!is.null(split_var)) {
    sl <- levels(forcats::fct_drop(as.factor(data[[split_var]])))
    parts <- purrr::map(sl, function(g) {
      gmask <- !is.na(data[[split_var]]) & data[[split_var]] == g
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
      tg  <- reg_build(sub, specs, shared,
                       split_var = NULL, .fit_cache = .fit_cache, reference = NULL, reref = FALSE,
                       skeleton_data = data)
      # Phase 19g (KEY 6): the group level rides a column NAMED AFTER the split variable -- exactly
      # how a crosstab names its tab_var levels -- so both arms are read by one rule
      # (test_group_cols()) and a predictor name in `var` can no longer be mistaken for a group.
      tst <- get_test(tg); if (!is.null(tst) && nrow(tst) > 0) tst[[split_var]] <- as.character(g)
      list(data = tibble::add_column(tibble::as_tibble(dplyr::ungroup(tg)),
                                     "{split_var}" := new_lvl(factor(g, levels = sl),
                                                              "tab_var", split_var), .before = 1L),
           test = tst)
    })
    # Phase 18z5: `color = "between_groups"` scores each group's estimate against the REFERENCE
    # GROUP's on the same row. THIS is the only point where the groups exist as parallel, separately
    # addressable tibbles: one line later vec_rbind() stacks them into rows, and after
    # the spread makes each a column whose group could only be recovered from a name suffix.
    # Writing the counterpart into the per-cell `obs` field here makes BOTH output shapes work with one
    # pass, and it rides vec_rbind / group_by / tab_spread untouched (fields survive the pivot).
    # It cannot be done with the existing reference machinery: fmt_broadcast_last() groups by runs of
    # in_refrow, which cross the split boundary (measured: north's rows get south's intercept).
    # the measure lives on the SPECS (Phase 17h: specs are the truth), not on a scalar formal.
    # Phase 18z8: the same pass writes `gap_se` (the groups are disjoint -> quadrature is exact),
    # which is what lets `color_signif` apply to the gap.
    color_ms <- unique(unlist(purrr::map(specs, "color")))
    parts <- reg_write_group_gap(parts, color_ms, conf_level = conf_level, method = method)
    combined <- vctrs::vec_rbind(!!!purrr::map(parts, "data"))
    tests    <- purrr::list_rbind(purrr::compact(purrr::map(parts, "test")))
    if (is.null(tests) || nrow(tests) == 0) tests <- new_test_tibble()
    # Phase 18z8: the AGGREGATED companion of the per-cell gap colour -- one pooled interaction test
    # per predictor. Opt-in via stats = c(..., "interaction"), and automatic under
    # `color = "between_groups"` (the same "state an intent, the pipeline computes what it needs" rule
    # that makes `color = "adjustment"` turn on `empirical`). Costs one extra fit per spec, which is why
    # it is not on by default. This is the ONE place with the full data, every spec and `shared`.
    if ("between_groups" %in% color_ms ||
        (is.character(shared$stats) && "interaction" %in% shared$stats)) {
      fit_cols <- unique(tests$col[tests$test %in% reg_footer_test_types()])
      if (length(fit_cols) != length(specs)) fit_cols <- make.unique(purrr::map_chr(specs, "label"))
      tests <- reg_interaction_rows(tests, data, specs, shared, split_var, fit_cols)
    }
    # `empirical_tips` and `assumptions` are deliberately NOT carried from the groups: they are
    # per-GROUP facts (crude tooltips keyed var\rlevel\rcategory; the observed curve of each
    # predictor) and `meta` has no per-group slot, so merging them would attach the FIRST group's
    # numbers to every other group's cells. Absent is honest; wrong is not. (A split table's
    # sparklines are already baked into its row labels, and reg_check_plots() refits from
    # spec$call$fit_spec.)
    grouped <- reg_finalize(combined, tests, conf_level, var_labels,
                            group_vars = c(split_var, "var"),
                            meta_extra = list(subtext = subtext))
    # Phase 19h (KEY 7): the split groups go side by side whenever that is unambiguous -- ONE model
    # (a single dependent and a single predictor set) that is not multinomial (a multinomial has
    # several columns for one model, so a side-by-side layout has no one column per group). It is an
    # internal rule, not an argument: tab_spread() is the public way to control the layout, and
    # reg_spread_models() -- whose two post-spread repairs were generic all along -- is deleted.
    if (length(specs) == 1L && !identical(family, "multinomial")) {
      return(tab_spread(grouped, tidyselect::all_of(split_var)))
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
                                  trials = sp$trials, formula = sp$formula, multiplier = multiplier,
                                  drop_extra = na_shared_vars,
                                  add_terms = reg_shape_add(shape_terms, sp$predictors))
      # .fit_cache present but not on the reref path (ame / profile / mnl-vs-rest / compound): cache the
      # RAW reg_fit result keyed on the (already display-referenced) data -> a reference change refits.
      if (is.null(.fit_cache)) thunk()
      else jmvreg_cached(.fit_cache, "fit",
                         jmvreg_fit_key(sp, data, sp_fam, design_spec,
                                        extra = list(method, sp_dox, conf_level, sp$est$effect,
                                                     sp$est$measure, display, multiplier,
                                                     shape_terms)),
                         thunk)
    })
  }

  # marginaleffects paths (effect="ame", and the MNL "j vs rest" OR at the reference profile) always key
  # by the ORIGINAL variables, so a compound formula still gets a clean bare-variable skeleton; the plain
  # coefficient path keeps its fit-read skeleton for compound terms. `skeleton_data` (Phase 12g split_var)
  # is the FULL data so every split group shares one skeleton (missing group levels -> empty cells); it
  # defaults to `data`, so non-split builds are unchanged.
  compound   <- purrr::map_lgl(specs, ~ isTRUE(.$compound))
  # Phase 19e: WHICH builder each spec takes is the estimand row's own `builder` -- the choice that
  # used to be a table-scalar `if` over (effect, at, family).
  builders   <- purrr::map_chr(specs, ~ .$est$builder %||% "coef")
  mnl_vsrest <- any(builders == "vsrest")
  if (is.null(skeleton))
    skeleton <- if (any(builders != "coef"))
                  reg_skeleton(skeleton_data, union_predictors)     # one row per PREDICTOR (z15)
                else if (any(compound))            reg_skeleton_from_fit(fits[[1]]$fit)
                else            reg_skeleton(skeleton_data, union_predictors, shape_terms)

  prefix_dep    <- length(specs) > 1L
  # Phase 14w: a model COMPARISON (several models, one dependent) keeps each model's col_var = its own
  # name (borders separate the models; the outcome/reference/effect go in the title). A single or
  # multi-dependent table shares one outcome col_var per model column + its empirical companions.
  n_dep         <- length(unique(purrr::map_chr(specs, "dependent")))
  is_comparison <- length(specs) > 1L && n_dep == 1L
  # Phase 18z9: ONE predictor-kind split for the whole builder (reg_is_factor_var) -- the AME
  # column's numeric cells, the crude companions and the crude tips all read the same two vectors.
  numeric_preds <- reg_numeric_preds(skeleton_data, union_predictors)
  factor_preds  <- reg_factor_preds(skeleton_data, union_predictors)

  # built_per_fit: a list PER FIT of {label, col} lists (a multinomial / MNL-vs-rest / AME-per-category
  # fit contributes SEVERAL columns). Kept un-flattened so reg_gof_tibble() can key the model-summary
  # footer to each fit's FIRST output column (Phase 12f).
  # Phase 19g (KEY 6): ONE assembler over three named column builders. They were three parallel
  # `purrr::map2(fits, specs, ...)` blocks chosen by a TABLE-scalar `if` -- even though Phase 15e made
  # the family per SPEC, so a mixed table had to be degraded upstream before the scalar could be
  # trusted. The choice is per spec now, where the fact lives; on a homogeneous table it picks
  # exactly what the scalar picked.
  cols_ame <- function(f, sp) {
    # Phase 15e: prob-scale / per-category / colour shape are per OUTCOME family (a mixed AME table
    # mixes binomial prob-points with a gaussian coef in one grid).
    sp_fam       <- sp$family
    sp_eff       <- sp$eff_word
    sp_col       <- sp$color
    prob_scale   <- reg_fam_prob(sp_fam)
    per_category <- reg_fam_percategory(sp_fam)
    # Phase 18z3: effect = "ame_ratio" swaps the ADDITIVE contrast for the log-ratio one, i.e. the
    # ratio of adjusted predicted probabilities (a marginal risk ratio). Guarded to prob-scale families
    # at the tab_reg() boundary, so `shape` can only become "prob_ratio" where a probability exists.
    # Phase 19e: the contrast asked of marginaleffects and the cell SHAPE both come from the
    # estimand row -- `comparison` (NA = the additive default) and its stored `scale`.
    sp_est       <- sp$est
    ratio_ame    <- !is.na(sp_est$comparison) && identical(sp_est$comparison, "lnratioavg")
    shape        <- if (!prob_scale) (if (ratio_ame) "raw_ratio" else "raw")
                    else if (ratio_ame) "prob_ratio" else "prob"
    marg  <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                          at = if (identical(sp_est$effect, "at_reference")) "reference" else "average",
                          want_pred = prob_scale,
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
  }
  cols_vsrest <- function(f, sp) {
    # MNL "j vs rest" OR at the reference profile (D3-flavour-2): exp of the profile log-odds-ratio of
    # "category j vs the rest" for each predictor level; one OR column per outcome category. Reached only
    # for a HOMOGENEOUS multinomial table (a mixed table degrades at="reference" -> "average" upstream).
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
  }
  cols_coef <- function(f, sp) {
    # Phase 15e: each column takes its own family shape (multinomial fans out; glm/gaussian are one col).
    sp_fam   <- sp$family
    sp_eff   <- sp$eff_word
    sp_col   <- sp$color
    if (sp_fam == "multinomial") {
      cols <- reg_columns_multinom(skeleton, f, sp, sp$est, sp_col, color_signif,
                                   sp_eff, cleannames, prefix_dep, model_family = sp_fam,
                                   method = method)
      # Phase 12h: display = "ci" adds the visible interval to each category's OR column
      # (the folds are degraded to "ci" for MNL in tab_reg()).
      if (!identical(display, "value")) {
        cols <- purrr::map(cols, function(lc) { lc$col <- set_display(lc$col, "est_ci"); lc })
      }
      cols
    } else {
      # a compound formula is one model: every skeleton row belongs to it (else compound rows go NA)
      model_predictors <- if (isTRUE(sp$compound)) unique(skeleton$var) else sp$predictors
      # Phase 14w (item 3): outcome col_var + "Model <effect>" name (comparison keeps the model name).
      cv  <- if (is_comparison) sp$label
             else reg_shared_col_var(sp_fam, sp$dependent, f$positive_level, cleannames)
      col <- reg_column(skeleton, f, model_predictors, cv, sp$est, sp_col, color_signif,
                        model_family = sp_fam, method = method)
      col <- reg_apply_display(col, display, skeleton, f, sp, sp_fam,
                               design_spec, conf_level, numeric_preds, model_predictors,
                               multiplier = multiplier)
      list(list(label = reg_model_col_name(sp_eff, sp$dependent, is_comparison, sp$label, n_dep),
                col = col))
    }
  }
  built_per_fit <- purrr::map2(fits, specs, function(f, sp)
    switch(sp$est$builder %||% "coef",
           ame    = cols_ame(f, sp),
           vsrest = cols_vsrest(f, sp),
           cols_coef(f, sp)))

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
  # Phase 18z13 (SS7.2): the per-predictor global test, from the fits already in hand. In the
  # default stats set (reg_footer_stats), so a table of multi-level factor predictors answers "is this
  # variable associated at all?" without being asked. Rows, then a footer LINE (reg_global_lines).
  # In the DEFAULT set, so NULL / "all" / TRUE ask for it; FALSE / "none" and an explicit vector that
  # omits it do not. (The interaction test is opt-in only, hence its narrower gate.)
  want_global <- is.null(stats) || identical(stats, "all") || isTRUE(stats) ||
    (is.character(stats) && "global" %in% stats)
  if (want_global)
    reg_gof <- reg_global_rows(reg_gof, fits, specs,
                               list(weighted = weighted, design_spec = design_spec),
                               fit_first_col)
  # Phase 18z15: the five model checks. A sibling of reg_compare_rows / reg_global_rows and placed
  # with them because it needs `data` (the Linearity refit) and `specs`, which reg_gof_tibble() has
  # neither of. Its own per-fit gate is reg_footer_stats(), so `stats` governs it like every other
  # footer row and no second sentinel logic exists.
  reg_gof <- reg_check_rows(reg_gof, data, fits, specs,
                            list(weighted = weighted, design_spec = design_spec,
                                 conf_level = conf_level, shape_terms = shape_terms,
                                 inverse_two_level_factors = inverse_two_level_factors),
                            stats, fit_first_col, grouped_by_fit)

  disp_levels <- reg_cleanup(skeleton$level, cleannames)
  # Phase 18z15: a `shape = "log"` / `"sqrt"` predictor was RECODED, so its row must say which
  # column was fitted ("log(age)"). The variable NAME is unchanged everywhere else (select(), the
  # references, the crude twin), exactly as the multiplier relabel below leaves it -- only the display
  # level moves. A quantile-cut predictor needs nothing: its levels ARE the groups.
  if (length(shape_labels)) {
    for (v in names(shape_labels)) {
      hit <- skeleton$var == v & !is.na(skeleton$term)
      if (any(hit)) disp_levels[hit] <- sub(v, shape_labels[[v]], disp_levels[hit], fixed = TRUE)
    }
  }
  # multiplier (Phase 12g / 15d): relabel the display level of each scaled numeric predictor so the row
  # reads "<var> (per <unit>)" -- KEEP the predictor name (dropping it left a bare "per 2" the user could
  # not read). Phase 18z9: the unit text comes from `multiplier_label` ("1 SD (13.5)" / "10 units"),
  # resolved with the number itself so the two can never disagree, and the row is found through the
  # STORED predictor kind rather than the `level == var` convention (which `cleannames` and this very
  # relabel already break -- Phase 17 rule 2).
  # Phase 18z15: keyed on the LINEAR term (`term == var`), so a curved predictor's `age²` row does
  # not claim a per-SD unit it does not carry -- reg_fit()'s multiplier matches `td$term == v` and
  # leaves the squared term alone (it is already per 1 SD², by construction of reg_shape_term()).
  if (length(multiplier_label)) {
    num_rows <- skeleton$var %in% numeric_preds &
      !is.na(skeleton$term) & skeleton$term == skeleton$var
    for (v in names(multiplier_label)) {
      lab <- multiplier_label[[v]]
      if (is.na(lab)) next
      hit <- num_rows & skeleton$var == v
      if (any(hit)) disp_levels[hit] <- paste0(disp_levels[hit], " (per ", lab, ")")
    }
  }

  # Phase 18z15 -- the OBSERVED shape of each continuous predictor, and its miniature in the row's
  # own label. It is the `Obs_*` half of the Linearity check (SS7.1): the sparkline is what the data
  # does, the footer p is whether the model's straight line was good enough. So it is fit-free, and
  # deliberately drawn on `skeleton_data`, not `data` -- under `split_var` the groups share one
  # skeleton and are pivoted into columns by row, so a per-group curve would give the same row two
  # different labels and break the alignment. Ten bins fixed, so two predictors are comparable.
  assumptions <- reg_curves(skeleton_data, specs, numeric_preds, design_spec$wt,
                            positive_level = fits[[1]]$positive_level,
                            design = design_spec$design)
  if (!is.null(assumptions)) {
    spark <- getOption("tabxplor.spark", TRUE)
    lin   <- !is.na(skeleton$term) & skeleton$term == skeleton$var
    for (v in names(assumptions$curves)) {
      gl <- rd_spark(assumptions$curves[[v]]$y, spark)
      if (is.na(gl)) next
      hit <- lin & skeleton$var == v
      # a NON-BREAKING space (U+00A0): the glyph run belongs to the label and must not wrap off it
      if (any(hit)) disp_levels[hit] <- paste0(disp_levels[hit], "\u00a0", gl)
    }
  }

  # Phase 19f (KEY 1): a regression DECLARES its index like every other producer -- `var` is the
  # column naming each row's variable (role "var"), `levels` holds the levels (role "level"). It used
  # to be a pun: `tab_render_vars()` reported the predictor as `tab_vars = "var"`, a fake sub-table
  # variable, because the grouped-tab machinery offered no other slot.
  tab <- tibble::tibble(
    var    = new_lvl(forcats::fct_inorder(skeleton$var), "var"),
    levels = new_lvl(forcats::fct_inorder(disp_levels) , "level")
  )
  # Phase 18z13 (SS7.1): the N behind each predictor level, right after the labels -- where STROBE
  # reads it and where the comparable packages put it. It is a BUILT column, not a `render_extras`
  # display intent like tab()'s own `add_n`: the count needs the model's complete-case frame, which
  # exists only here, and tab_add_n_pct() folds into a Total cell that a reg table does not have.
  # `n_frame_of()` is the same frame the crude companion uses (emp_frame_of, hoisted below), so the
  # column and the `Obs_*` block can never count different people. One column when there is a single
  # outcome -- every compared model shares it, since z13's default puts them on one population.
  if (isTRUE(add_n)) {
    n_dep_all <- length(unique(purrr::map_chr(specs, "dependent")))
    for (i in seq_along(specs)) {
      dep_i <- specs[[i]]$dependent
      if (i > 1L && n_dep_all <= 1L) break
      if (dep_i %in% purrr::map_chr(specs[seq_len(i - 1L)], "dependent")) next
      cnt <- reg_level_counts(reg_complete_frame(data, c(dep_i, union_predictors,
                                                         reg_design_vars(design_spec))),
                              skeleton, wt = design_spec$wt)
      nm  <- if (n_dep_all > 1L) paste0("n [", dep_i, "]") else "n"
      tab[[nm]] <- fmt(n = cnt$n, wn = cnt$wn, scale = "level_n", display = "n", digits = 0L,
                       color = "", color_signif = "ignore", col_var = "n", comp_all = FALSE,
                       # in_refrow is NOT decorative: tab_bold_rows() ANDs it across every
                       # discriminating column, so omitting it would un-bold every reference row.
                       in_refrow = skeleton$is_ref, model_family = specs[[i]]$family, role = "n")
    }
  }
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
  # z16-iiiii: the crude grids' own degrade, harvested from the grid each reg_empirical() returns
  # (attr "degrade") and folded into the basis stamped at the tail -- see reg_inference().
  emp_degraded <- FALSE
  # The per-dependent complete-case frame the crude companions + multinomial tips share with the model
  # (reg_complete_frame = reg_fit's own frame). `union_predictors` == the model's predictors when not
  # comparing; in comparison mode it is the shared population. Recomputed from `data` (fits[[i]]$data is
  # NULL on the reref/digest path). On this listwise-complete frame reg_empirical()'s per-predictor NA
  # filter is a no-op, so the crude reference level / n exactly match the model.
  # Phase 18z13 (D1): `na_shared_vars` is the same extra-completeness set reg_fit() receives above,
  # so under the default this frame IS the model's own frame -- crude and adjusted on the same rows by
  # construction, not by coincidence. Under `na = "drop_by_model"` it is empty and the crude block keeps
  # its union population, which is why `obs` is then gated (reg_same_frame).
  emp_frame_of <- function(dep)
    reg_complete_frame(data, c(dep, union_predictors, na_shared_vars,
                               reg_design_vars(design_spec)))
  if (isTRUE(empirical)) {
    fac_preds_e <- factor_preds
    # Phase 18z9: numeric predictors get a crude column too, from the univariable fit. Excluded in
    # compound-formula mode: there `var` is a bare RHS name whose model term may be an interaction or a
    # basis expansion (`age*race`, `poly(age, 2)`), so a univariable slope is not that row's estimand.
    num_preds_e <- if (any(purrr::map_lgl(specs, ~ isTRUE(.$compound)))) character(0) else numeric_preds
    if (length(fac_preds_e) > 0L || length(num_preds_e) > 0L) {
      for (i in seq_along(specs)) {
        # Phase 18z10: ONE stored fact decides eligibility -- reg_crude_key(), computed once at spec
        # construction. It replaced a duplicated family whitelist here, a `quasipoisson -> poisson` alias
        # in reg_empirical_columns(), a lookup-miss return, and (worst) `positive_level`-is-NULL as a
        # proxy for "grouped binomial or compound formula" -- which was a SIDE EFFECT of reg_fit()
        # skipping reg_prep_binary() on that path, not a statement about crude twins.
        key_i   <- specs[[i]]$crude_key
        if (is.na(key_i)) next
        fam_i   <- specs[[i]]$family
        est_i   <- specs[[i]]$est
        col_i   <- specs[[i]]$color               # on/off follows the model column
        dep_i   <- specs[[i]]$dependent
        pos_i   <- if (reg_fam_binary(fam_i)) fits[[i]]$positive_level else NULL
        mdata_i <- emp_frame_of(dep_i)                    # Change B: same complete-case frame as the model
        var_y_i <- if (fam_i == "gaussian")
          suppressWarnings(stats::var(as.numeric(mdata_i[[dep_i]]), na.rm = TRUE)) else NA_real_
        emp_i   <- reg_empirical(mdata_i, fac_preds_e, dep_i, key_i, pos_i, design_spec$wt,
                                 trials = specs[[i]]$trials, ref_category = fits[[i]]$y_ref,
                                 conf_level = conf_level, design_spec = design_spec)
        emp_degraded <- emp_degraded || isTRUE(attr(emp_i, "degrade"))
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
          other_preds = union_predictors, est = est_i, wt = design_spec$wt,
          # z17 (D2): always kept. `want_fit` does not decide whether the univariable crude models are
          # FITTED (they are, to fill the crude column) -- only whether the fitted object survives for
          # the gap test's crude leg. Since the test no longer waits to be asked for by `color`, it
          # does. Build-time locals; they never reach the jamovi .fit_cache.
          want_fit = TRUE, trials = specs[[i]]$trials,
          shape_terms = shape_terms,
          marginal = !identical(est_i$effect, "coefficient") &&
            (reg_fam_binary(fam_i) || reg_fam_prob(fam_i)))
        cols_i  <- reg_empirical_columns(skeleton, emp_i, fac_preds_e, key_i, fam_i, est_i, var_y_i,
                                         conf_level = conf_level, color_signif = color_signif,
                                         color = col_i, fit_est = fit_i,
                                         # W-D: `n_eff` is written only where something corrected it
                                         weighted = svy_weighted(design_spec, design_spec$wt),
                                         # z16-iiiii (D4): the design df the MODEL columns are already
                                         # referred to, so the crude bracket beside them matches
                                         degf = design_spec$degf %||% Inf)
        # Phase 14w (item 3): the crude companions share the model column's outcome col_var (one span,
        # no border). NOT in comparison mode (the crude block stays a distinct col_var beside the models).
        if (!is_comparison && length(cols_i$cols)) {
          scv    <- reg_shared_col_var(fam_i, dep_i, pos_i, cleannames)
          cols_i$cols <- purrr::map(cols_i$cols, ~ set_col_var(.x, scv))
        }
        # Phase 18z8-B: the crude block also carries what the GAP TEST needs -- the frame it was
        # computed on and the factor predictors it covers. Both are locals here and nowhere else, and
        # the test is per (model column, crude block): in comparison mode ONE observed block serves
        # several models, each with its own fit, so the covariance differs per model though `obs` does
        # not. Keeping them on the record is what lets the column loop below stay one call.
        cols_i$frame     <- mdata_i
        cols_i$fac_preds <- fac_preds_e
        cols_i$crude_key <- key_i
        # Phase 18z9/z10: the fit-derived crude legs travel too. A row with no closed form has no
        # closed-form crude influence function either (reg_crude_if_maker() is cell-indicator
        # arithmetic), so it comes from this fit -- the second IF path SS13 forecast. They are
        # build-time locals and never reach the jamovi .fit_cache, whose persisted raw fits were
        # Phase o's freeze.
        cols_i$fit_preds <- fit_preds_e
        cols_i$fits      <- fit_i$fits
        cols_i$grid      <- emp_i
        emp_by_fit[[i]] <- cols_i
      }
    }
  }
  # Phase 18z5: the crude EFFECT vector, per fit, on the model column's own scale -- written into
  # each model column's `obs` field below. It is what `color = "adjustment"` scores and what the
  # `{obs}` display token / the html tooltip print. NULL for a fit with no crude companion
  # (multinomial, ordinal, grouped-binomial) -> `obs` stays NA -> those cells stay uncoloured.
  # Phase 18z8-B: the whole crude RECORD travels (effect + shape + frame), because its gap SE needs
  # the last two; `obs` and `gap_se` are then written together or not at all.
  emp_of <- function(fi) if (is.na(fi) || is.null(emp_by_fit[[fi]])) NULL else emp_by_fit[[fi]]
  # Phase 18z8 (a z5 defect): `at = "reference"` makes the model cell a marginal effect AT THE
  # REFERENCE PROFILE, while the crude companion stays a MARGINAL effect over the whole sample -- two
  # different estimands, so their difference is not "what adjustment did". The stratum-restricted crude
  # effect would match the estimand but answers a different question (model FIT at one profile, not
  # confounding) on a few percent of the rows, so no `obs` is attached at all: the cells stay
  # uncoloured, `{obs}` blanks, and tab_reg() says why once.
  at_profile <- any(purrr::map_lgl(specs, ~ identical(.$est$effect, "at_reference")))
  # `fi` = the fit this COLUMN came from (not the crude block's -- they differ in comparison mode).
  # Phase 18z10: `key` = the column's OWN outcome category, stored on it at build time as `emp_key`
  # (reg_columns_multinom / the per-category AME loop already stamp it). A multinomial or ordinal-marginal
  # fit owns one column per category and each needs its OWN crude counterpart, so the crude record's
  # `effect` is a list keyed the same way; "" is the key of a single-column fit. A missing key means no
  # crude counterpart for that column -- the honest degrade, leaving `obs` NA and the cell uncoloured.
  set_obs_if <- function(bi, e, fi) {
    col <- bi$col
    if (is.null(e) || at_profile) return(col)
    if (!reg_same_estimand(e$shape, col)) return(col)     # z5 defect: same scale, or nothing
    # Phase 18z13 (D1): ...and the same PEOPLE, or nothing. A model fitted on rows the crude block
    # does not cover has a "gap" that is listwise deletion, not adjustment -- the same predicate that
    # withholds its test therefore withholds the value the colour would score.
    if (!reg_same_frame(e$frame, fits[[fi]])) return(col)
    key <- if (is.null(bi$emp_key)) "" else as.character(bi$emp_key)
    ev  <- cat_get(e$effect, key)
    if (is.null(ev)) return(col)
    col <- set_obs(col, ev)
    # Phase 18z10 (maintainer's ruling Q4): when the crude effect draws NO column of its own, fold it
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
                            e$fac_preds, specs[[fi]]$est, design_spec$wt,
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
        # Phase 18z10: read straight off the MERGED crude grid -- reg_empirical_tips() is gone, it was
        # reg_empirical() at a three-part key (measured bit-identical), and keeping two producers of one
        # quantity is exactly the sync-by-comment pair Phase 17 rule 5 forbids. Reuse the block already
        # built for this fit when there is one; otherwise build the grid here.
        tipsd <- if (!is.null(emp_by_fit[[si]]$grid)) emp_by_fit[[si]]$grid else
          reg_empirical(emp_frame_of(dep_i), fac_preds_t, dep_i, "multinomial", NULL, design_spec$wt,
                        ref_category = fits[[si]]$y_ref, conf_level = conf_level,
                        design_spec = design_spec)
        emp_degraded <<- emp_degraded || isTRUE(attr(tipsd, "degrade"))
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
  # Phase 18z9: a numeric predictor's DESCRIPTIVE goes in the tooltip, because nothing can honestly
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
  # Phase 18z13 (D3) + z16-iiiii: the level every interval in this table was built at, the design df
  # it is referred to and the basis it was computed on, all on each fmt column -- the colour engine
  # is per column and cannot read a table attribute. It is what makes the gap interval
  # (fmt_gap_bounds), manufactured at print time, follow `conf_level`; and it is where the footer
  # reads the basis from (ruling 1: a weighted tab_reg() is ALWAYS on the weighted basis, model
  # column and crude companions alike).
  reg_inf <- reg_inference(shared, emp_degraded)
  reg_finalize(tab, reg_gof, conf_level, var_labels, group_vars = "var",
               degf = reg_inf$degf, basis = reg_inf$basis,
               meta_extra = list(subtext = subtext, empirical_tips = empirical_tips,
                                 # Phase 18z15: the observed curves the sparklines were drawn from,
                                 # and the only thing reg_check_plots() needs a refit cannot give back.
                                 assumptions = assumptions))
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
#'   \item **What each cell shows**: `effect` (which contrast) x `measure` (which effect measure),
#'     `display` (the cell layout), `empirical` (crude vs adjusted effect).
#'   \item **Colors & significance**: `color`, `color_signif`, `stars`, `conf_level` --- as in [tab()].
#'   \item **Comparisons & structure**: `reference` (baseline levels), `compare` / `baseline` (model
#'     comparison test), `split_var` (one table per group), `multiplier` (the unit a continuous
#'     predictor's effect is reported per — one standard deviation by default).
#'   \item **Survey design**: `wt` for a simple weight, or a prebuilt [survey::svydesign()] as `data`.
#'   \item **Model checks**: `stats` (the footer rows --- linearity, dispersion, influence,
#'   collinearity, proportionality), `shape` (the cure for a non-linearity), and the plot
#'   [reg_check_plots()]. \item **Chart**: [forest_plot()] draws the finished table.
#' }
#'
#' `predictors` selects the mode: a **character vector** fits one model, and `dependent` may itself
#' be a vector -> one column per dependent; a **named list** of predictor sets fits one model each ->
#' one column per model (predictors absent from a model are left blank), for comparing specifications.
#'
#' `effect = "marginal"` switches from the native coefficient to **average marginal effects** with the
#' adjusted **predicted probability** shown in parentheses (e.g. `-8%*** (16%)`) -- a probability-scale,
#' cross-model-comparable interpretation (Mood 2010), computed with the `marginaleffects` package.
#' `effect = "at_reference"` instead evaluates at a **reference profile** (other predictors held at their
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
#' @param data A data frame, **or a prebuilt survey design** ([survey::svydesign()]). When a design
#'   is passed, its weights (and clustering / stratification / calibration) drive the estimation and
#'   `wt` is ignored. Replicate-weight ([survey::svrepdesign()]) and two-phase designs are refused at
#'   the boundary rather than approximated.
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
#'   (-> `"multinomial"`), or any other numeric (-> `"gaussian"`) outcome, emitting a message. An
#'   integer-valued numeric reads as `"gaussian"` too --- age in years, years of schooling, a summed
#'   score and income in whole units are all integers, and a linear model always fits --- with the
#'   message naming `"poisson"` for a genuine count. Set it explicitly with
#'   `"gaussian"` (linear), `"binomial"` (logistic), `"poisson"` / `"quasipoisson"` (counts),
#'   `"multinomial"` (nominal 3+ level), `"ordinal"` (ordered 3+ level). A **scalar** applies to every
#'   dependent; a **vector** aligned to `dependent`, or a **named** vector keyed by dependent
#'   (e.g. `c(income = "poisson", satisfied = "binomial")`), sets one family per outcome. Mixed
#'   families work only with a character `predictors` (one model per outcome); a `predictors` list
#'   (model comparison) is single-outcome, hence single-family.
#'
#'   \strong{A binary outcome with `family = "poisson"` (modified Poisson).} Asking for `"poisson"` on a
#'   binary outcome is not a mistake and not a count model: it fits the **modified Poisson regression**
#'   (Zou 2004), whose exponentiated coefficient is a **risk ratio** (RR), not an odds ratio --- the same
#'   table as `measure = "ratio"`, which names the measure rather than the distribution. It is
#'   strictly opt-in --- a binary outcome still auto-detects as `"binomial"`; you must name `"poisson"`.
#'   Reach for it when the outcome is **common** (above roughly 10%), where an odds ratio is much further
#'   from 1 than the risk ratio and is almost always narrated as if it were one ("twice as likely"), and
#'   when you compare a coefficient **across nested models**, which an odds ratio does not support (it is
#'   non-collapsible: it moves when you add a covariate even if that covariate is not a confounder).
#'   \strong{Standard errors are handled consistently}: the Poisson likelihood is deliberately
#'   misspecified for a 0/1 outcome, so the naive standard errors are too wide and are replaced by the
#'   robust **Huber--White sandwich** --- via `survey::svyglm()` in both cases, which means the
#'   design-based variance when you supply `wt` or a design, and the equivalent of `HC0` on a
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
#'   For clustering, stratification, a finite-population correction or calibration, build the design
#'   yourself with [survey::svydesign()] and pass it as `data` (see there); `wt` alone is a flat
#'   `ids = ~1` design, which can understate the variance of a clustered sample.
#' @param effect **Which contrast** the table shows --- one of the two questions an estimand asks.
#'   `"coefficient"` (default) is the model's own conditional effect (the beta / odds ratio /
#'   incidence-rate ratio / cumulative odds ratio, "holding the other predictors constant").
#'   `"marginal"` is the **average marginal effect**: the model's effect averaged over the observed
#'   covariate distribution --- a probability-scale, cross-model-comparable summary (Mood 2010) for
#'   logistic / multinomial / ordinal outcomes, the expected-count change for poisson, the
#'   coefficient itself for gaussian. `"at_reference"` evaluates the same quantity **at the reference
#'   profile** (every other predictor at its reference level / mean): the marginal effect at
#'   reference (MER), and for a **multinomial** outcome the odds ratio of each outcome category
#'   *versus the rest* at that profile. `"marginal"` / `"at_reference"` need the `marginaleffects`
#'   package. Resolved **per dependent** like `family` (scalar / vector / named vector).
#'
#'   The parenthetical of a probability-scale marginal cell is a *marginal-standardized* prediction
#'   (`avg_predictions(variables=)`: the predictor set to each level for the whole sample, other
#'   covariates kept as observed, then averaged), so it is genuinely covariate-adjusted and coheres
#'   with the effect --- adjusted-%(reference) + AME(level) equals adjusted-%(level). Read it as a
#'   standardized comparison ("holding the measured covariates' distribution fixed"), not a
#'   manipulation. Note the reference profile can be an unusual baseline (e.g. a factor's first
#'   level = `"No answer"`).
#' @param measure **Which effect measure** --- the other question. `"auto"` (default) takes the
#'   family's usual one (odds ratios for a logit, incidence-rate ratios for a count, a coefficient
#'   for a linear model; a difference for a marginal contrast). The full word is the canonical
#'   spelling and the discipline's acronym is an accepted synonym, so the argument teaches the
#'   concept while the column header keeps the acronym:
#'
#'   * `"odds_ratio"` (`"OR"`) --- the odds ratio of a logit / multinomial / ordinal fit.
#'   * `"ratio"` (`"RR"`, `"IRR"`, `"RoM"`) --- a **risk** ratio on a binary outcome (the modified
#'     Poisson, Zou 2004: a log link with robust standard errors), an **incidence-rate** ratio on a
#'     count, a **ratio of adjusted means** on a continuous one (Poisson pseudo-maximum-likelihood,
#'     Santos Silva & Tenreyro 2006). Reach for it when the outcome is **common**: an odds ratio is
#'     then much further from 1 than the risk ratio and is almost always narrated as if it were one
#'     ("twice as likely"), and unlike an odds ratio a risk ratio stays comparable **across nested
#'     models** (an odds ratio is non-collapsible --- it moves when you add a covariate even if that
#'     covariate is not a confounder).
#'   * `"difference"` (`"RD"`, `"diff"`) --- a linear coefficient on a continuous outcome; on a
#'     **binary** one the **risk difference** in percentage points, from an identity-link
#'     (additive-risk) fit with robust standard errors. The identity link is unbounded, so it can
#'     fail to converge: the linear probability model then takes over, with a message, and the footer
#'     says which one ran.
#'   * `"log"` (`"log_odds"`, `"log_risk"`, `"log_rate"`) --- the same fit, **un-exponentiated**:
#'     coefficients on the model's own link scale. Bare `"log"` logs the family's default measure;
#'     the precise spellings pin which (so `"log_risk"` is the modified-Poisson fit, logged).
#'
#'   Resolved **per dependent** like `family`. `effect` and `measure` are orthogonal: a *conditional*
#'   ratio is a different **fit**, a *marginal* one a different **estimator**, and both land on the
#'   same stored scale. Call [reg_measures()] on your outcome to see what it offers, with the reason
#'   whenever something is not available.
#'
#'   Two caveats worth knowing. A conditional risk ratio and a marginal one answer slightly different
#'   questions --- the marginal one is population-averaged and can never predict a probability above
#'   1 --- though on real data they agree closely; and a marginal estimate is standardized to the
#'   covariate distribution at hand, so under `split_var` each group standardizes to **its own**
#'   subpopulation.
#'
#'   \strong{How this is called elsewhere.} `measure = "ratio"` on a binary outcome is Stata's
#'   `binreg y x, rr`, `glm(family = binomial("log"))` or the modified Poisson, and
#'   `marginaleffects`' `comparison = "ratio"` on the marginal path; `measure = "log"` is
#'   `exponentiate = FALSE` in broom / parameters / gtsummary; `effect = "marginal"` is
#'   `avg_comparisons()`.
#' @param trials Grouped-binomial (summed-score) outcomes only. The number of items behind the score,
#'   fitting `cbind(score, trials - score)` as a binomial. `NULL` (default) fits an ordinary binary
#'   logit; a single integer (or a vector named by dependent) sets the item count; `TRUE`, or an `NA`
#'   entry in a named vector, uses that outcome's **observed maximum** score --- so explicit and
#'   automatic counts can be mixed, and an outcome with no score to take a maximum of (a factor, a
#'   0/1 numeric) simply stays an ordinary binary logit. Requires `family = "binomial"`. It is one
#'   count per *dependent*, never a column name --- a per-row item count is not supported; write the
#'   model with `cbind()` in a compound `formula` instead.
#' @param conf_level Confidence level for the intervals. Default `0.95`. It drives every interval in
#'   the table, the significance stars, and the greying under `color_signif` --- including the
#'   model-vs-observed gap interval, which is computed at print time from the stored standard error and
#'   is stored on each column so it follows this argument rather than
#'   `options("tabxplor.conf_level")`.
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
#'   [tab()]'s `tab_vars`. The same model(s) are fitted **within each level** of this variable.
#'   When that leaves one column per group — a single outcome, a single set of predictors, and not a
#'   multinomial — the groups are pivoted into **side-by-side columns** for an easy across-group
#'   comparison; otherwise the per-group tables are stacked into one grouped table (grouped by
#'   `split_var`), sharing the variable/level stub. Call [tab_spread()] yourself for full control of
#'   the layout in that case. A level absent from a group shows empty cells.
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
#' @param shape How a **continuous** predictor enters the model, when one straight line is not enough.
#'   The `Linearity` footer row and the little curve drawn in the predictor's own row label tell you
#'   *whether* a line is enough; this argument is how you fix it without leaving the framework.
#'
#'   A **named vector** over continuous predictors — everything it does not name stays linear:
#'   \describe{
#'     \item{`"linear"`}{one slope (the default).}
#'     \item{`"quintiles"` / `"quartiles"` / an integer `k`}{cut into `k` quantile groups, so the
#'       predictor becomes an ordinary **factor**: one estimate per group, its own observed companion,
#'       counts and colours per group — the non-linearity becomes visible in the printed numbers. Start
#'       here; it is the most readable answer.}
#'     \item{`"quadratic"`}{adds a curvature term, so the predictor takes **two rows** — the slope at
#'       the mean, and `age²`, which says whether the slope flattens (< 1) or accelerates (> 1) as you
#'       move away from it. Parsimonious, but two coefficients to read.}
#'     \item{`"log"` / `"sqrt"`}{fit `log(x)` / `sqrt(x)` instead of `x` — diminishing returns. The row
#'       label says which (`log(age)`). `"log"` needs strictly positive values.}
#'   }
#'   Example: `shape = c(age = "quadratic", income = "log")`.
#'
#'   Everything else keeps working: the observed `Obs_*` companion is fitted with the same shape, the
#'   model-versus-observed colour and test compare like with like, and `multiplier` still names the
#'   unit. A `poly()` / `ns()` basis is deliberately never emitted — the marginal-effects engine
#'   silently returns zero for those (a warning fires if you reach one through a `formula`).
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
#'   structure to be crude about) and, for a marginal contrast, a *weighted* 3+ level outcome (no
#'   `marginaleffects` method).
#'
#'   By design every crude quantity is computed on **exactly the same complete-case population as the
#'   model** (listwise-complete on the dependent, all predictors and any design variable), so crude and
#'   adjusted are directly comparable and not confounded by differing missingness (reproduce it with
#'   [dplyr::filter()] + [tab()] on the same rows). That is what the default `na = "drop_by_outcome"`
#'   guarantees, including when several models are compared. Under `na = "drop_by_model"` a model
#'   fitted on rows the observed block does not cover gets **no** observed value at all --- no `obs`,
#'   no `color = "adjustment"`, no gap test --- because the distance between two such estimates would
#'   be listwise deletion rather than adjustment. Also works with a vector of dependents. Ordinal has
#'   no clean crude analogue and is ignored (with a message).
#'
#'   The crude companions are **always** on the same inference basis as the `Model_*` column beside
#'   them, which is the whole point of putting them side by side: when the data is weighted, their
#'   intervals account for the weighting exactly (the flat survey design), and under a
#'   `survey::svydesign` for the full design. Each column stores the base its own interval used, in
#'   the `n_eff` [fmt()] field, while the displayed `n` stays the raw count.
#'   A **continuous** predictor's companion comes from a univariable fit; a categorical one
#'   from the closed-form cell sums, which for a saturated model is the same estimator. Default
#'   `FALSE`.
#'
#'   **Two consequences worth knowing**, both deliberate. First, a weighted `tab_reg()` is *always*
#'   design-corrected while a weighted [tab()] is *not* unless you ask (`design_effect = TRUE`).
#'   `tab_reg()` has no choice --- its observed columns must be measured like the model column beside
#'   them, and that one is design-based by construction (\code{survey::svyglm}); [tab()] does, and
#'   keeps the descriptive convention (a weighted estimate on the raw sample size) as its default.
#'   Each table's footer says which it used --- so the same weight can legitimately give a slightly
#'   wider interval here than in a crosstab beside it, and `tab(design_effect = TRUE)` is what brings
#'   the two into line. Second, the crude percentage *difference* here uses the
#'   **Wald** interval, matching the model AME so one legend can name one method, where
#'   `tab(ci = "diff")` uses Newcombe; on a real table they differ by a few tenths of a percent.
#'
#'   **Under a `survey::svydesign`** every column is design-based. The `Model_*` ones through
#'   `survey::svyglm`; the crude ones through an effective sample size derived from the design
#'   variance of each cell (Korn & Graubard's device, the same one [tab()]'s cells use). That base is
#'   **exact** for a single cell, and for each leg of a ratio: the odds-ratio and risk-ratio brackets
#'   built on it *are* the design variance of the log odds and of the log risk. A **difference or a
#'   ratio between two cells** additionally ignores the design covariance between them, so it lands a
#'   few percent either side of the exact answer — against the 15–25 % it was out by before. Two
#'   things stay outside: a 3+ level outcome (multinomial, ordinal) has no crude *column*, only a
#'   crude value folded into the model cell, and replicate-weight designs (`svrepdesign`) are refused
#'   at the boundary. If the design variance cannot be computed the intervals fall back to the raw
#'   count and say so.
#' @param add_n Logical, default `TRUE`. Add an `n` column, right after the level labels, holding the
#'   **unadjusted count** behind each predictor level on the model's own complete cases --- the numbers
#'   a reader needs to judge the estimates beside them (and which reporting guidelines ask for). The
#'   Constant row shows the model N. Continuous predictors are left blank: on a listwise-complete frame
#'   their count is the model N for every one of them. Unlike [tab()]'s `add_n`, this is a real column
#'   rather than a display option, because only the model frame knows the counts.
#' @param stats The statistics shown in the model-summary **footer** (one block per model). `NULL`
#'   (default) uses the per-family set: linear models show N, R square, adjusted R square, the overall
#'   F-test and the residual SD; other models show N, the likelihood-ratio test versus the null model,
#'   McFadden's pseudo-R square, AIC and BIC (poisson / grouped-binomial models also show the Pearson
#'   dispersion, `"phi"`). Every default set also carries the overall-association test `"global"` and
#'   the five **model checks** below. Pass a character vector to pick the statistics (`"n"`,
#'   `"lr_null"`, `"mcfadden_r2"`, `"aic"`, `"bic"`, `"phi"`, `"r2"`, `"r2_adj"`, `"f_model"`,
#'   `"sigma"`, `"global"`, `"interaction"`, `"linearity"`, `"proportionality"`, `"dispersion"`,
#'   `"influence"`, `"collinearity"`), or `FALSE` / `"none"` to hide the footer entirely.
#'
#'   `"global"` (in the default set) adds one **overall test per predictor** --- "is this variable
#'   associated with the outcome at all?", the question a block of stars against a reference category
#'   cannot answer, and the reason a multi-level factor needs more than its cells. It is shown for
#'   predictors carrying two or more coefficients (for a single-coefficient term it would simply repeat
#'   that cell's own p-value), and it costs **no extra model fit**: it is a likelihood-ratio test on
#'   the fitted model (an F test for linear and quasi models, a design-based Wald test for weighted /
#'   survey models). Multinomial and ordinal outcomes get no such test.
#'
#' @section Model checks:
#'
#' Five checks are computed for every model, in the order of what each one threatens --- the estimate,
#' what the estimate means, its interval, whether it is real at all, and why it is wide. Each is a
#' footer row, so it travels into every export; none needs an argument, and any of them can be dropped
#' through `stats`.
#'
#' \describe{
#'   \item{**Linearity** (p-value, per numeric predictor)}{Is this predictor's effect really one
#'     straight line? The model is refitted with that predictor's centred squared term and the two
#'     compared. A small p says one slope is the wrong summary --- and the damage is **not confined to
#'     that row**: on the model used throughout `vignette("tabxplor-reg")`, letting `age` curve moves
#'     the top income category's odds ratio by 24 % and flips another income level's conclusion at the
#'     5 % threshold.}
#'   \item{**Proportionality (Brant)** (p-value, ordinal outcomes)}{Is one cumulative odds ratio enough
#'     for every cut of the outcome? Read it beside the size of the departure: at survey sample sizes
#'     this test rejects on differences the eye calls mild. Weighted ordinal models (`svyolr`) have no
#'     Brant fit, so the row is absent rather than approximated.}
#'   \item{**Dispersion (robust/model SE)** (a ratio)}{Are the standard errors wide enough? The largest
#'     ratio, over the coefficients, of a robust (sandwich) standard error to the model-based one. About
#'     1 means the family's variance assumption holds; above 1 it does not --- over-dispersion,
#'     heteroscedasticity or clustering, by roughly that factor. For a count model it is close to the
#'     square root of the Pearson dispersion, and it correctly returns to about 1 once
#'     `family = "quasipoisson"` has widened the intervals (while `"phi"` still reports the
#'     dispersion). Under a survey design it measures what the design did to the standard errors, which
#'     are already the design's --- nothing to act on.}
#'   \item{**Influence (max dfbetas)** (a ratio)}{Does one respondent carry the result? The largest
#'     change, over coefficients and observations, that dropping a single observation makes to a
#'     coefficient, in units of that coefficient's own standard error. It is printed as a
#'     *reassurance*: with thousands of respondents no single one should move anything, and a near-zero
#'     value is the finding. Note that influence is not outlyingness --- a surprising outcome with an
#'     ordinary covariate profile moves nothing.}
#'   \item{**Collinearity (max VIF)** (a ratio)}{Can the data tell these predictors apart? The largest
#'     variance inflation factor (`car::vif()`, on one scale whatever a term's degrees of freedom).
#'     It is the one check that is not a comparison with the data: collinearity biases nothing, it only
#'     widens intervals, and a wide interval already shows that. It is here because it is what every
#'     textbook teaches first. Needs the `car` package; refused for multinomial outcomes.}
#' }
#'
#' Two cautions. A curvature test on one predictor can pick up **another** predictor's
#' mis-specification when the two are near-collinear --- one more reason the collinearity row sits in
#' the same block. And at survey sample sizes a diagnostic p-value rejects almost anything, which is
#' why three of the five report a *magnitude* rather than a p-value.
#'
#'   `"interaction"` is different from the others: with `split_var`, it adds one **aggregated
#'   effect-modification test per predictor** — "does this predictor act differently between the
#'   groups?", asked once for all its levels together, so it carries none of the multiplicity of the
#'   per-cell `color = "between_groups"` colours. It is printed as a footer line rather than a footer
#'   row (a pooled test belongs to no single model column), and it costs one extra model fit.
#'   `color = "between_groups"` turns it on for you. It is a likelihood-ratio test (an F test for
#'   linear and quasi models, a design-based Wald test for weighted / survey models, exactly like
#'   `compare`) on the model **coefficients** — so under a marginal contrast the footer
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
#' @param display What each effect cell shows, mirroring [tab()]'s display grammar. `"value"`
#'   (default) the plain estimate (e.g. `2.34`); `"ci"` adds a visible confidence-interval bracket
#'   (`2.34 [1.20; 4.50]`, any family); `"prob"` folds the model-adjusted predicted probability into
#'   the cell (`2.34 (16%)`); `"ame"` folds the average marginal effect (`2.34 (+8%)`). The last two
#'   are shorthands for the templates `"{or} ({pct})"` and `"{or} ({diff})"`, which you may write out
#'   instead; they need the `marginaleffects` package and apply to binomial coefficient models only
#'   (they degrade to `"ci"` otherwise, with a message).
#'
#'   A template may ask for an **auxiliary** quantity of the same fit; it never changes the fit or
#'   the estimand --- that is `measure`'s job alone. Note `display = "ame"` *adds* an AME beside the
#'   odds ratio, whereas `effect = "marginal"` makes the whole column an AME; when both are set the
#'   column wins and `display` is ignored, with a message.
#' @param color,color_signif Colouring of the effect cells. `color = TRUE` (default) grades each cell
#'   on **its own scale** --- the ladder follows what the column estimates (`measure`), so it is never
#'   asked for separately; `color = FALSE` turns colouring off for every column (model and empirical).
#'   `color_signif` is the significance policy (default `"grey_non_signif"`). See [tab()].
#'
#'   What is left to choose is what each effect is compared **to**, and both such measures are meant
#'   for the *background* channel so the text keeps showing the effect size. `color` is positional,
#'   `c(text, background)`, and `TRUE` in the text slot means "the column's own scale" --- so
#'   `color = c(TRUE, "adjustment")` answers "how strong is this effect?" and "how much did the model
#'   change it?" in one glance:
#'
#'   * `"adjustment"` — how far each **modelled** effect sits from its **observed** (crude,
#'     unadjusted) counterpart, i.e. what adjusting for the other predictors did to it. It turns
#'     `empirical = TRUE` on (that is where the observed effect comes from). The ladder follows the
#'     estimate's own scale, so a threshold means the same thing in every table: `×1.1 / ×1.25 /
#'     ×1.5 / ×2` for a ratio (odds, risk or rate), `±2 / ±5 / ±10 / ±20` **points** for a
#'     probability-scale marginal effect, and `±0.05 / ±0.1 / ±0.2 / ±0.4` **standard deviations of
#'     the outcome** for an effect in the outcome's own units (a linear beta, a count marginal
#'     effect) --- otherwise the same model on an outcome recorded in hours, minutes or days would
#'     read three different ways. The first ratio threshold is the classic 10% "change in estimate"
#'     rule, a convention rather than a decision rule; set them all with
#'     [set_color_breaks()] (`adj_ratio`, `adj_diff`, `adj_diff_std`). One pole means the model **strengthened** the
#'     effect (suppression), the other that it **attenuated** it — measured from the null, so a
#'     protective effect (OR < 1) and a risky one read the same way.
#'   * `"between_groups"` — with `split_var`, how far each group's effect sits from the **first**
#'     group's, on the same row: a per-predictor reading of effect modification, beside the global
#'     comparison a likelihood-ratio test gives. Pick the baseline group with `reference` keyed by the
#'     split variable (e.g. `reference = c(race = "Black")`). It also adds the **aggregated**
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
#'   on a **collapsible** effect measure: an average marginal effect (`effect = "marginal"`), a marginal risk
#'   ratio (`effect = "marginal", measure = "ratio"`), a conditional risk ratio (`measure = "ratio"`), an
#'   incidence-rate ratio, or a linear β. A **conditional odds ratio** is not collapsible: adjusting it
#'   moves it away from 1 even when the extra variable is independent of the exposure, so at survey
#'   sizes every row would test "significant" for a reason that is not confounding. There the colours
#'   stay descriptive, `color_signif` is ignored, and `tab_reg()` says so once. The test is also skipped
#'   when the two estimates cannot be compared cell by cell: at `effect = "at_reference"`, and under
#'   `na = "drop_by_model"` when a compared model's complete cases differ from the observed columns'
#'   (there the observed value is withheld altogether, so nothing is coloured either).
#'
#'   That ruling covers the **3+ level outcomes** too, and it is the reason they behave as they do: a
#'   multinomial or ordinal *coefficient* is a conditional odds ratio, so those columns show the
#'   observed effect but carry no test. Their **marginal** paths (`effect = "marginal"`) are
#'   collapsible and do get one, per outcome category.
#'
#'   Two things to keep in mind. A difference between two groups, or before and after adjustment, is a
#'   difference in *that effect measure*: groups with different base rates or more variable outcomes can
#'   show different effects on every scale without the underlying cause differing. And each cell is
#'   tested on its own, with no multiple-comparison correction — with seven comparisons about one table
#'   in five shows a spurious coloured cell, so read the pattern rather than the single cell (for
#'   `"between_groups"`, the footer's aggregated test is the multiplicity-free reading).
#'
#'   Where a row carries no test at all — a group with an empty cell leaves no recoverable standard
#'   error, and `method = "profile"` bounds are not `estimate ± critical value × SE` — the cell stays
#'   uncoloured under `"grey_non_signif"` and the legend says that some rows are untested (the HTML
#'   tooltip shows which: an untested cell has no gap line).
#'
#'   **On what the comparison is.** This reports how much two numbers differ and whether that
#'   difference exceeds noise; it does not detect confounding. The change-in-estimate criterion it
#'   operationalises is contested as a *confounder-selection rule* — the 10% cut-off is a convention,
#'   not a threshold with a decision-theoretic basis — so read a coloured cell as "adjustment moved
#'   this effect", not as "this variable is a confounder". For a formal treatment of the crude-versus-
#'   adjusted difference see Clogg, Petkova & Haritou (1995) and Allison's (1995) comment; for nested
#'   logit models, where part of the change is rescaling rather than confounding, see the KHB
#'   decomposition (Karlson, Holm & Breen 2012; Kohler, Karlson & Holm 2011). The collapsible routes
#'   this function offers (`effect = "marginal"`, `measure = "ratio"`) are what the
#'   literature calls **marginal standardization** (g-computation).
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
#'   are `effect = "marginal"` (marginal effects), `measure = "ratio"` on a binary
#'   outcome (risk ratios) and a gaussian beta; on those the gap is confounding by the adjustment
#'   set. The legend says so on the odds-ratio path, and it is why the significance test above is
#'   computed only on the collapsible scales.
#' @param stars Logical (default `TRUE` for regression tables, where significance stars are standard).
#'   When `FALSE`, the per-cell p-value is dropped and no stars are shown (colours still read the CI).
#' @param na Which rows each model is fitted on — the grain at which missing values are dropped.
#'   `"drop_by_outcome"` (default) gives every model **of one outcome** the same complete-case
#'   population (no `NA` on the outcome, on any predictor of any model in the call, or on a design
#'   variable); a second outcome keeps its own rows. That is what makes the comparisons in the table
#'   honest: the observed columns (`empirical`) are then computed on exactly the model's rows, and
#'   nested models get equal N so the likelihood-ratio comparison can run instead of degrading to an
#'   AIC difference. `"drop_by_model"` lets each model use its own complete cases — every model then
#'   keeps as many rows as it can, at the price of comparability: models fitted on different people
#'   get no observed effect at all (no `obs`, no `color = "adjustment"`, no gap test), because the
#'   distance between two such estimates is listwise deletion rather than adjustment.
#'   `"drop_all"` shares one population across the whole call, all outcomes included.
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
#' @seealso [forest_plot()] draws the finished table -- every effect with its interval, its stars and
#'   its colour, and (with `empirical = TRUE`) the observed effect beside it with the margin of error
#'   of the gap. [reg_check_plots()] draws the model checks. [tab()] for cross-tables.
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
#'                 family = "binomial", effect = "marginal")
#'   # the same contrast at the reference profile (others at their reference level / mean):
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "at_reference")
#'   # the same quantity as a RATIO: marginal risk ratios beside the crude ones. With a common
#'   # outcome this is what a reader means by "x times more likely" -- an odds ratio is not.
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "marginal", measure = "ratio", empirical = TRUE)
#' }
#' # the CONDITIONAL risk ratio: measure = "ratio" on a binary outcome fits the modified Poisson
#' # (Zou 2004), a log link with robust standard errors. Ask for the measure, not the distribution.
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'                 measure = "ratio", empirical = TRUE)
#' # ... and the risk DIFFERENCE, from an identity-link (additive-risk) fit:
#'   tab_reg(reg_data, dependent = "married", predictors = c("race", "rincome"),
#'                 measure = "difference")
#' # what this outcome can be modelled as, with the reason wherever it cannot:
#'   reg_measures(reg_data, "married")
#' # multinomial (nominal 3+ level): one OR column per outcome category vs the reference
#'   tab_reg(reg_data, dependent = "party3", predictors = c("race", "age"),
#'                 family = "multinomial", reference = c(party3 = "3-Republican"))
#' # ordinal (proportional-odds): one cumulative-OR column
#'   tab_reg(reg_data, dependent = "rincome", predictors = c("race", "age"), family = "ordinal")
#' }
#'
#' @section Out of scope:
#' `tab_reg()` covers linear, logistic, Poisson, multinomial and ordinal models, with survey designs.
#' Three families of models are deliberately **not** supported, and are unlikely to be: **survival /
#' Cox** models, **mixed / multilevel** models, and pooling over **multiply-imputed** datasets. Fit
#' those with their own packages.
#'
#' @references
#' Clogg, C. C., Petkova, E. & Haritou, A. (1995). Statistical Methods for Comparing Regression
#' Coefficients between Models. *American Journal of Sociology*, 100(5), 1261-1293. (With
#' Allison, P. D. (1995), *ibid.* 1294-1305.) --- the crude-versus-adjusted comparison
#' `color = "adjustment"` implements, generalised here to GLMs, survey designs and marginal effects.
#'
#' Karlson, K. B., Holm, A. & Breen, R. (2012). Comparing Regression Coefficients Between Same-sample
#' Nested Models Using Logit and Probit. *Sociological Methodology*, 42(1), 286-313. --- the KHB
#' decomposition, which separates confounding from rescaling in nested logit models.
#'
#' Zou, G. (2004). A Modified Poisson Regression Approach to Prospective Studies with Binary Data.
#' *American Journal of Epidemiology*, 159(7), 702-706.
#'
#' Altman, D. G. & Bland, J. M. (2003). Interaction revisited: the difference between two estimates.
#' *BMJ*, 326, 219. --- the `color = "between_groups"` test.
#'
#' Santos Silva, J. M. C. & Tenreyro, S. (2006). The log of gravity. *The Review of Economics and
#' Statistics*, 88(4), 641-658. --- `measure = "ratio"` on a continuous outcome.
#'
#' @eval reg_measures_rd()
#' @export
tab_reg <- function(data, dependent, predictors = NULL, split_var = NULL, wt = NULL,
                    family = "auto",
                    effect = "coefficient", measure = "auto",
                    trials = NULL, empirical = FALSE, add_n = TRUE,
                    color = TRUE, color_signif = NULL, stars = TRUE,
                    conf_level = conf_level_default(), method = c("wald", "profile"),
                    reference = NULL, inverse_two_level_factors = TRUE, multiplier = "sd",
                    shape = NULL,
                    stats = NULL, compare = c("none", "baseline", "sequential"), baseline = NULL,
                    na = c("drop_by_outcome", "drop_by_model", "drop_all"),
                    display = "value",
                    cleannames = NULL, subtext = "",
                    .fit_cache = NULL, ...) {
  # Phase 19e: the retired estimand arguments (`exponentiate` / `at` / `estimate_display`) are caught
  # here rather than by R's "unused argument", so the error names the spelling that replaced them.
  reg_retired_args(list(...))
  method  <- match.arg(method)
  compare <- match.arg(compare)
  # Phase 18z13: the un-supplied default is the whole vector, so its length IS "the user did not
  # choose" -- read before match.arg collapses it. The `na` advice messages fire on a CHOICE, never on
  # the default (which is the very thing they would advise).
  na_explicit <- length(na) == 1L
  na      <- match.arg(na)
  # Fallback FALSE matches .onLoad's default and tab()'s read sites (the option is always set to FALSE
  # on load, so this only bites if someone unsets it; TRUE here was an inconsistency, not an intent).
  cleannames <- resolve_cleannames(cleannames)


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
    # Phase 19e (D6): every per-dependent argument is SLICED the way `trials` is, and every
    # whole-call one is forwarded. Before, the per-call options and `.fit_cache` were silently
    # dropped (so the jamovi cache never filled), and
    # a POSITIONAL `family` vector was passed whole to each recursion -- where its first entry then
    # became every outcome's family. `reg_per_dep()` is the one slicer, shared by the four.
    tabs <- purrr::map(seq_along(dependent), function(i) {
      d   <- dependent[[i]]
      tri <- if (is.null(trials) || isTRUE(trials)) trials
             else if (!is.null(names(trials)))      unname(trials[d])
             else if (length(trials) == 1L)         as.numeric(trials)
             else                                   trials[[i]]
      tab_reg(data, dependent = d, predictors = predictors, wt = wt,
              family  = reg_per_dep(family,  d, i, "auto"),
              effect  = reg_per_dep(effect,  d, i, "coefficient"),
              measure = reg_per_dep(measure, d, i, "auto"),
              trials = tri, conf_level = conf_level, method = method,
              reference = reference, inverse_two_level_factors = inverse_two_level_factors,
              split_var = split_var, multiplier = multiplier, shape = shape,
              empirical = empirical, add_n = add_n,
              stats = stats, compare = compare, baseline = baseline,
              display = display, color = color, color_signif = color_signif,
              stars = stars, na = na, cleannames = cleannames, subtext = subtext,
              .fit_cache = .fit_cache)
    })
    names(tabs) <- dependent
    return(new_tabxplor_tabs(tabs))
  }

  # Phase 12g / Phase 18z14-i: `data` may be a PREBUILT survey design, gtsummary-style. THE shared
  # boundary (R/survey-design.R) extracts its model frame for family-detect / reference / skeleton and
  # materialises the design's own weights as a column; the design itself still drives every fit.
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
    svy_abort_wt_design(!is.null(wt), "tab_reg")
    data <- svy$data
    wt   <- svy$spec$wt
  }
  stopifnot(is.data.frame(data))
  weighted <- svy_weighted(list(design = design_obj), wt)

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

  # Phase 18z15 -- `shape`: fit a continuous predictor as something other than a line. THE boundary,
  # and there is only one: a shape either RECODES the column here (log / sqrt / quantile groups) or
  # emits ONE extra model term (quadratic). Placed before family detection, the reference relevel, the
  # frozen multiplier SD and the skeleton, so every one of them sees the predictor AS FITTED -- a
  # quantile-cut `age` is a factor from this line on, and inherits the entire factor machinery (one
  # estimate per group, a saturated crude twin, per-level N and colours) with no code of its own.
  # The design's own variables are recoded too, exactly as reg_relevel_design() does: a prebuilt
  # survey design reads its columns off `$variables`, not off `data`.
  reg_shapes   <- reg_resolve_shape(shape, data, unlist(predictors, use.names = FALSE))
  shape_labels <- character(0)
  if (length(reg_shapes) > 0L) {
    sh   <- reg_shape_apply(data, reg_shapes, w = wt)
    data <- sh$data
    shape_labels <- sh$labels
    if (!is.null(design_obj)) design_obj$variables <- data
  }

  # Phase 15e: `family` is resolved PER DEPENDENT, so one call can model several outcomes with
  # DIFFERENT families (one column-group per outcome). Accepts "auto" (detect each outcome), a scalar
  # (recycled to every dependent), a positional length-N vector, or a named vector keyed by dependent
  # -- mirroring `trials` / `inverse_two_level_factors`. Auto-detection stays honest and per-dependent
  # (an ambiguous integer count aborts for THAT outcome, not the whole table). The scalar `family`
  # below (= the first outcome's) is the recycled default for reg_meta / direct reg_build callers; each
  # spec carries its own family and the per-column `model_family` fmt attribute (Step D) drives the legend.
  fam_named    <- !is.null(names(family))
  rr_promoted  <- stats::setNames(as.list(rep(FALSE, length(dependent))), dependent)
  families_vec <- vapply(seq_along(dependent), function(i) {
    d <- dependent[[i]]
    f <- if (fam_named)                 family[[d]]
         else if (length(family) == 1L) family[[1]]
         else                           family[[i]]
    if (is.null(f) || (length(f) == 1L && is.na(f))) f <- "auto"
    if (identical(f, "auto")) f <- reg_detect_family(data, d)
    if (!f %in% REG_USER_FAMILIES) {
      cli::cli_abort(c("{.arg family} for {.val {d}} must be one of {.or {.val {REG_USER_FAMILIES}}}.",
                       "x" = "Got {.val {f}}."))
    }
    # DESIGN (Phase 18z3): THE resolution site for the modified-Poisson path. An explicit
    # poisson/quasipoisson on a BINARY outcome is not a count model -- it is Zou (2004)'s modified
    # Poisson, whose exp(coef) is a RISK RATIO, not an incidence-rate ratio. Resolving it to the
    # internal family key "rr" here (before `specs` are built) means the split_var recursion, the
    # multi-dependent recursion and the jamovi bridge all inherit it, and every family switch
    # downstream dispatches on ONE key. "rr" is deliberately absent from REG_USER_FAMILIES: a user
    # reaches it only through family = "poisson", never by typing it.
    if (reg_fam_count(f) && reg_is_binary_outcome(data[[d]])) {
      # Phase 19e: this route still works exactly as documented, but it is no longer the ONLY one --
      # so the message names the front door. Asking for a risk ratio by naming the wrong distribution
      # was the clearest case in the study of a measure hidden inside another argument.
      cli::cli_inform(c("i" = paste0(
        "{.val {d}} is binary: fitting a modified Poisson regression (robust standard errors) -> ",
        "{.strong risk ratios}, not incidence-rate ratios."),
        "i" = paste0("The same table is {.code family = \"binomial\", measure = \"ratio\"}, ",
                     "which names the measure rather than the distribution.")
      ))
      # Phase 19e: the promotion sets the MEASURE, not the family. "rr" was only ever a link chosen
      # to reach a risk ratio, so it is the estimand row's `fit` now (R/reg-estimand.R) and the
      # outcome stays what it is -- which is what makes the two spellings one code path.
      rr_promoted[[d]] <<- TRUE
      f <- "binomial"
    }
    f
  }, character(1))
  names(families_vec) <- dependent
  family_for   <- function(d) families_vec[[d]]
  family       <- families_vec[[1]]                          # scalar fallback (homogeneous default)
  mixed_family <- length(unique(families_vec)) > 1L

  # === THE ESTIMAND (Phase 19e, KEY 8b) ============================================================
  # `effect` (which contrast) x `measure` (which measure) resolve PER DEPENDENT, exactly where
  # `family` does, into ONE row of the declared library (R/reg-estimand.R). That row is the single
  # answer to: which model to fit, whether to exponentiate, the column's header word, its stored
  # `scale`, which crude companion pairs with it, and which marginaleffects contrast to ask for.
  #
  # It replaces `family` x `effect` x `at` x `exponentiate` -- 36 combinations for 9 estimands, with
  # three degrade blocks, two aborts and ~19 cells where an argument was silently ignored. Every one
  # of those is now either a row (legal), a row with status "impossible" (cannot be), or no row at
  # all (not offered) -- and the message enumerates the alternatives from the table itself.
  est_for <- local({
    cache <- list()
    function(d) {
      key <- as.character(d)
      if (!is.null(cache[[key]])) return(cache[[key]])
      i   <- match(key, dependent)
      ekv <- reg_effect_key(reg_per_dep(effect, key, i, "coefficient"))
      # a retired `effect` value could carry a measure inside it; an explicit `measure` still wins
      mv  <- reg_per_dep(measure, key, i, "auto")
      if (identical(mv, "auto") && nzchar(ekv$measure)) mv <- ekv$measure
      # `family = "poisson"` on a binary outcome IS `measure = "ratio"` (see the resolver above)
      if (identical(mv, "auto") && isTRUE(rr_promoted[[key]])) mv <- "ratio"
      res <- reg_estimand(family_for(key), ekv$effect, mv)
      if (!identical(res$status, "ok")) reg_estimand_abort(res, dependent = key)
      cache[[key]] <<- res
      res
    }
  })
  est_vec <- purrr::map(stats::setNames(dependent, dependent), est_for)
  est     <- est_vec[[1]]                                    # the recycled table-scalar default
  # The two facts that stay table-scalar because they describe the CALL, not a column: which builder
  # runs (a mixed table may still mix them -- the choice is per spec in reg_build) and whether the
  # profile axis is in force (the crude companions stay marginal over the whole sample there).
  mnl_vsrest <- any(vapply(est_vec, function(e) identical(e$builder, "vsrest"), logical(1)))
  at_ref     <- any(vapply(est_vec, function(e) identical(e$effect, "at_reference"), logical(1)))

  # Phase 12g: survey-weighted 3+ level outcomes are supported -- ordinal via survey::svyolr, nominal
  # via svyVGAM::svy_vglm (checked in reg_check_deps). The marginaleffects paths have no method for
  # svyolr / svy_vglm -> error. Asked of the resolved estimand (`builder`), not of an argument.
  if (weighted && any(reg_fam_percategory(families_vec)) &&
      any(vapply(est_vec, function(e) !identical(e$builder, "coef"), logical(1)))) {
    cli::cli_abort(c(
      "Marginal-effects output is not available for survey-weighted {.val multinomial}/{.val ordinal} models.",
      "i" = "Use the default {.code effect = \"coefficient\"}, or drop the weights."
    ))
  }

  # Phase 19e: `do_exp` / `effect_shape` / `eff_word` are READ OFF the estimand row -- they were the
  # three things `exponentiate` was derived into, each with its own per-dependent closure.
  # Phase 14v: with an empirical companion, a prob-scale AME/MER cell folds in the model-adjusted
  # predicted % as "{diff} ({pct})"; name it in the header ("... AME (adjusted %)") so the parenthetical is
  # unambiguous next to the crude "Emp. %". The parenthetical is the marginal-STANDARDISED predicted
  # probability (decisions doc S50, change A/C), hence "adjusted %" not "model %". Gated on `empirical`
  # (the maintainer's disambiguation case), prob-scale families only (gaussian/poisson AME is a bare effect).
  eff_word_of <- function(e) {
    w <- e$word
    if (!identical(e$builder, "coef") && isTRUE(empirical) && reg_fam_prob(e$family))
      w <- paste0(w, " (adjusted %)")
    w
  }
  do_exp       <- isTRUE(est$exp)
  effect_shape <- if (do_exp) "ratio" else "additive"
  eff_word     <- eff_word_of(est)
  do_exp_for       <- function(d) isTRUE(est_for(d)$exp)
  effect_shape_for <- function(d) if (do_exp_for(d)) "ratio" else "additive"
  eff_word_for     <- function(d) eff_word_of(est_for(d))

  # Phase 19e: `display` = the estimate-cell layout, mirroring tab()'s grammar. "value" (plain) /
  # "ci" (a visible interval, any family) apply everywhere; a {} TEMPLATE naming `pct` / `diff`
  # folds the model-adjusted predicted probability / the average marginal effect into the effect
  # cell -- which is exactly what the retired `estimate_display = "prob" / "ame"` presets did, kept
  # as documented shorthands for them.
  #
  # THE RULE (KEY 8's other half): a display template may ask for AUXILIARY quantities from the SAME
  # fit; it may never change the fit or the estimand. `measure` is the only estimand argument.
  display <- reg_resolve_display(display)
  # Marginal-effects output already IS a fold ("{diff} ({pct})") -> a second one is ignored.
  if (!identical(display, "value") && (mnl_vsrest ||
      any(vapply(est_vec, function(e) !identical(e$builder, "coef"), logical(1))))) {
    cli::cli_inform(c("i" = "{.arg display} is ignored with marginal-effects output."))
    display <- "value"
  }
  # Phase 15e: the folds are binomial-coefficient only; in a mixed table they apply to the binomial
  # outcomes and each non-binomial column degrades to the CI bracket (guarded per column in
  # reg_apply_display). Only degrade the whole call when NO outcome is a binomial coefficient.
  if (reg_display_folds(display) && !(any(families_vec == "binomial") && !formula_mode)) {
    cli::cli_inform(c(
      "!" = paste0("{.arg display} = {.val {display}} folds a model-adjusted quantity into the ",
                   "effect cell, which needs a binomial coefficient model; showing the confidence ",
                   "interval instead.")))
    display <- "est_ci"
  }

  # trials -> grouped binomial (D2): a summed-score outcome fit as cbind(score, trials-score). NULL =
  # off (binary logit). TRUE = observed max per dependent. Numeric / named vector = the item count.
  # Phase 15e: applied per BINOMIAL outcome only (a non-binomial dependent ignores it).
  # Phase 19k: `TRUE` and `NA` both mean "the observed maximum", and BOTH are outcome-aware -- an
  # outcome that is not a numeric score has no maximum to take, so it stays an ordinary binary logit
  # (max() on a factor is an error, which is why `trials = TRUE` used to be unusable as soon as one
  # dependent was a factor). `NA` inside a named vector is what lets a caller mix explicit counts
  # with automatic ones, which is the shape the jamovi Model table produces -- it used to apply the
  # rule ITSELF, silently, for any integer outcome: one rule written twice, with a semantic shift.
  trials_for <- function(d) NULL
  trials_auto <- function(d) {                    # the observed max, or NA where there is none
    x <- data[[d]]
    if (!is.numeric(x) || is.factor(x)) return(NA_real_)
    m <- suppressWarnings(max(x, na.rm = TRUE))
    if (is.finite(m) && m > 1) m else NA_real_
  }
  if (isFALSE(trials)) trials <- NULL            # the natural off switch, symmetric with TRUE
  if (!is.null(trials)) {
    # Phase 18z16-iv (S6): validate HERE. A column name -- the shape a reader naturally reaches for,
    # since a respondent may have answered a different number of items -- used to reach
    # as.numeric("q_count") -> NA -> `cbind(score, NA - score)`, and died deep inside glm() with
    # "contrasts can be applied only to factors with 2 or more levels", naming neither the argument
    # nor the reason. `trials` is one item COUNT per dependent, not a per-row column.
    if (is.character(trials) || is.factor(trials))
      cli::cli_abort(c(
        "{.arg trials} must be an item count, not a column name.",
        "x" = "Got {.val {as.character(trials)}}.",
        "i" = paste("Pass the number of ITEMS behind the summed score: an integer, a vector named by",
                    "dependent, or {.code TRUE} to use each dependent's observed maximum."),
        "i" = "Per-row item counts are not supported; write the model formula with {.code cbind()}."))
    # (an all-NA logical vector is the "take the observed maximum for these outcomes" spelling)
    if (!is.numeric(trials) && !isTRUE(trials) && !(is.logical(trials) && all(is.na(trials))))
      cli::cli_abort(c(
        "{.arg trials} must be a number, a vector named by dependent, or {.code TRUE}.",
        "x" = "Got {.cls {class(trials)[[1]]}}."))
    if (!any(families_vec == "binomial")) {
      cli::cli_abort("{.arg trials} applies only to {.val binomial} outcomes (grouped / summed-score).")
    }
    if (formula_mode) {
      cli::cli_warn("{.arg trials} is ignored with a compound formula; write {.code cbind()} in it instead.")
    } else {
      if (!isTRUE(trials) && !is.null(names(trials))) {
        # a name that matches no dependent is a typo, not a mixing request -- say so, rather than
        # silently auto-resolving the outcome the user meant to pin.
        unknown <- setdiff(names(trials), dependent)
        if (length(unknown))
          cli::cli_abort(c("{.arg trials} names {.val {unknown}}, which is not a dependent.",
                           "i" = "Dependents: {.val {dependent}}."))
      }
      tv <- if (isTRUE(trials))               rep(NA_real_, length(dependent))
            else if (!is.null(names(trials))) unname(as.numeric(trials[dependent]))
            else                              rep_len(as.numeric(trials), length(dependent))
      tv <- stats::setNames(tv, dependent)
      # NA = "take this outcome's observed maximum" -- from `TRUE` (all of them), from an NA entry, or
      # from a named vector that simply does not name this dependent.
      auto <- is.na(tv)
      if (any(auto)) tv[auto] <- vapply(dependent[auto], trials_auto, double(1))
      tv <- stats::setNames(as.integer(round(tv)), dependent)
      # An outcome with no observed maximum (a factor, or a 0/1 numeric) keeps NA and is fit as an
      # ordinary binary logit -- there is nothing to abort about. Only an EXPLICIT bad count is an
      # error, and it names itself.
      bad <- names(tv)[!auto & (is.na(tv) | tv < 1L)]
      if (length(bad))
        cli::cli_abort(c(
          "{.arg trials} must be a positive item count.",
          "x" = "Missing or invalid for {.val {bad}}.",
          "i" = paste("Give an item count, or {.code NA} / {.code TRUE} to take each outcome's",
                      "observed maximum.")))
      trials_for <- function(d)
        if (identical(family_for(d), "binomial") && !is.na(tv[[d]])) tv[[d]] else NULL
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

  # `color` is logical-primary: TRUE (default) auto-picks the per-column measure below; FALSE turns
  # every column (model AND empirical companion) uncoloured. NULL == TRUE (auto).
  #
  # Phase 19e (D25): the GEOMETRY words are gone from this argument. `tab_reg(color = "difference")`
  # on an odds-ratio column used to be ACCEPTED and stored a measure contradicting what the column
  # estimates (measured: a `+/-` additive ladder on cells whose neutral is 1). Since KEY 2 the column
  # states its own scale, so the ladder comes from the column and what is left to choose is only the
  # two measures whose baseline is ANOTHER COLUMN -- `measure_own_ref()`, a DERIVED allow-list, not a
  # new one. `TRUE` in the text slot means "the column's own geometry", so the headline
  # `c("OR", "adjustment")` is now `c(TRUE, "adjustment")`: same two channels, no contradiction
  # representable.
  color <- reg_normalize_color(color)
  # `color[1]`: since Phase 18z5 the measure may be a length-2 (text, background) vector. Only the
  # text channel carries the auto sentinel.
  color_auto <- is.na(color[1])                                 # Phase 15e: remember the auto sentinel

  # Phase 18z5: VALIDATE the measure(s) through the storage boundary itself rather than repeating
  # its rules here -- fmt() casts `color` without validating, so tab_reg would otherwise accept an
  # unknown measure, a whole-cell measure on the background, or the two mutually exclusive `obs`
  # measures together, and only fail (or silently mis-colour) much later. The result is discarded: the
  # canonical form is applied per column by fmt()/set_color as before.
  if (!color_auto) invisible(resolve_color_channels(color))

  # Phase 18z5: `adjustment` scores the model effect against its OBSERVED counterpart, which lives in
  # the `obs` field only when the crude companion was computed -- so asking for the colour asks for
  # `empirical`. Same shape as color = "contrib" forcing chi2 + totrow in the resolve cascade
  # (R/tab-resolve.R): the user states an intent, the pipeline computes what it needs.
  # Phase 19c: the forcing is the measure's own declared `requires["empirical"]`, so it fires from
  # the same table the crosstab forcings read -- the last hand-written "this measure needs that step".
  if (any(vapply(color, measure_forces, logical(1), "empirical")) && !isTRUE(empirical)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"adjustment\"} compares each model effect to its ",
                                   "observed one, so {.code empirical = TRUE} is turned on.")))
    empirical <- TRUE
  }
  # Phase 18z8: `between_groups` now HAS a test of its own (the two split groups are disjoint, so the
  # gap SE is exact by quadrature -- reg_write_group_gap), and reads `color_signif` normally.
  # `adjustment` compares two estimates fitted on the SAME rows, whose joint variance needs influence
  # functions (dev/model_vs_observed_gap_test.md SS3): still neutralised by MEASURES$force_policy, and
  # said once rather than letting a `color_signif` look effective.
  # Phase 18z8: `between_groups` also gets the AGGREGATED companion of its per-cell colours -- one
  # pooled interaction test per predictor, in the footer. Automatic here for discoverability (and
  # because the two readings belong together); `stats = c(..., "interaction")` asks for it without the
  # colours. It costs one extra model fit per model, so say so.
  if (any(vapply(color, measure_forces, logical(1), "interaction")) && !is.null(split_var) &&
      !(is.character(stats) && "interaction" %in% stats)) {
    cli::cli_inform(c("i" = paste0("{.code color = \"between_groups\"} also adds the aggregated ",
                                   "interaction test to the footer (one extra model fit). Ask for it ",
                                   "without the colours with {.code stats = c(..., \"interaction\")}.")))
  }
  # Phase 18z13 (D6): every "the colour you asked for cannot be computed / cannot be tested here"
  # comes from ONE producer, so the rule is uniform -- before it, four cases said so in four blocks here
  # and two said nothing at all. `crude_keys` is the stored per-outcome fact z10 introduced.
  for (note in reg_color_notes(
    color, color_signif, est_vec, split_var, na, na_explicit,
    families = families_vec,
    crude_keys = vapply(dependent, function(d)
      reg_crude_key(family_for(d), trials_for(d), formula_mode), character(1)),
    empirical = empirical)) {
    # `{note}` substitutes the already-interpolated string as a VALUE -- passing it as the template
    # would glue it a second time, and one of these notes legitimately prints a literal "{obs}".
    cli::cli_inform(c("i" = "{note}"))
  }
  # Phase 19c: the ladder decides the CONTEXT ("reg_diff" / "reg_ratio"); WHICH measure answers it is
  # MEASURES' own `auto_for`, the same table tab()'s two auto passes read. So the three `color = TRUE`
  # cascades that could once disagree are one lookup with three call sites.
  # Phase 19e: the context comes from the column's own stored SCALE (its declared geometry), not from
  # a re-reading of `effect` + `exponentiate` -- which is what made the ladder and the estimand two
  # facts that could disagree (a marginal RATIO is multiplicative whatever `exponentiate` said).
  color_auto_measure <- function(e) {
    ctx <- if (identical(EST_SCALES[[e$scale]]$geometry, "ratio")) "reg_ratio" else "reg_diff"
    measure_auto(ctx, "text")
  }
  # a TRUE in the text slot of an explicit two-channel spec is the same "the column's own geometry"
  # sentinel as a bare TRUE -- resolved per dependent, so a mixed table keeps one ladder per family.
  color_slot_auto <- is.na(color)
  if (is.null(color_signif)) color_signif <- "grey_non_signif"
  color_fill <- function(spec, e) { spec[color_slot_auto] <- color_auto_measure(e); spec }
  if (color_auto) color <- color_fill(color, est)
  # Phase 15e: the per-dependent auto colour measure (each family its own default). An explicit user
  # `color=` (a measure / c(text, bg)) keeps its own slots; only the auto ones follow the column.
  color_spec_arg <- color
  color_for <- function(d) if (any(color_slot_auto))
    color_fill(color_spec_arg, est_for(d)) else color_spec_arg

  all_predictors <- if (is_comparison) unique(purrr::flatten_chr(predictors)) else predictors

  # Phase 18z13 (D1): WHICH ROWS every model of the call is fitted on -- resolved ONCE here into the
  # extra variables each fit must be complete on, and consumed by reg_build through reg_fit(drop_extra=).
  # That mechanism (Phase z9) exists for exactly this: variables joining the complete-case drop without
  # joining the formula, and it is the ONLY sound route -- pre-filtering `data` instead breaks a PREBUILT
  # design's keep_mask (reg_resolve_design computes it from `data` itself). So the old `drop_all_models`
  # pre-pass, and its "ignored for a prebuilt survey design" caveat, are both gone.
  #
  # "drop_by_outcome" (the default) makes every model OF ONE OUTCOME share a population, which is what
  # makes the crude companion comparable: reg_build's emp_frame_of() is then the model's own frame, so
  # "crude and adjusted are computed on the same people" is structural rather than checked. It also
  # equalises N across nested models, so the likelihood-ratio comparison fires instead of degrading to
  # an AIC difference. A second outcome keeps its own rows (comparing outcomes is not what the call
  # asked for); "drop_all" opts into one population for the whole call.
  # The design variables need no mention: reg_fit's own drop_vars already carries reg_design_vars(),
  # and split_var needs none either (the split filters its group before fitting).
  na_shared_vars <- if (formula_mode) character(0) else
    intersect(unique(switch(na,
                            "drop_by_model"   = character(0),
                            "drop_by_outcome" = all_predictors,
                            "drop_all"        = c(all_predictors, dependent))),
              names(data))

  # Phase 15b (jamovi live reref): with a `.fit_cache`, a single-equation GLM coefficient table can be
  # recomputed at any factor-predictor reference from ONE canonical fit (reg_build_digest) -- no refit.
  # On that path the body does NOT relevel; reg_build fits the canonical digest + reparametrizes to
  # `reference`. Everything the reparametrization can't handle (ame / profile / mnl-vs-rest / compound /
  # multinomial / ordinal / split / trials / model comparison) keeps the refit path.
  # Phase 18z9: `multiplier` LEFT that list. The digest is fitted natively (reg_build_digest passes
  # multiplier = NULL), so it is multiplier-independent just as it is reference-independent, and
  # reg_reref_fit_res() applies the scaling itself -- a scaling change is now a cache HIT. Keeping the
  # clause would silently kill the fast path for every table with a numeric predictor once "sd" becomes
  # the default, which is the regression Phase 15b exists to prevent.
  # Phase 15e: an all-glm mixed table keeps the digest fast-path (each spec caches its own family's
  # digest); any multinomial/ordinal outcome degrades the whole table to the cached raw-fit path.
  # Phase 18z8-B: `color = "adjustment"`'s gap test needs the FITTED object (influence functions),
  # which the digest deliberately discards -- so asking for it takes the refit path rather than getting
  # a silently untested colour. One clause, not a rebuild-from-coef arm: building that arm would mean
  # a second encoding of reg_fit()'s model frame. Phase 19k made the jamovi reg `color` a MEASURE
  # list, so a live-UI call CAN now reach here with "adjustment" -- it takes the (correct, heavier)
  # refit path. The recipe for a digest-based arm is in dev/model_vs_observed_gap_test.md SS6.
  reref <- !is.null(.fit_cache) &&
    all(vapply(est_vec, function(e) identical(e$builder, "coef"), logical(1))) && !mnl_vsrest &&
    display %in% c("value", "est_ci") && method == "wald" &&
    all(reg_fam_glm(families_vec)) &&
    # Phase 19k: the gate is the RESOLVED trials, not the raw argument. `trials` may now carry NA =
    # "take the observed maximum", which resolves to NULL on an outcome that has none (a factor, a
    # 0/1 numeric) -- and the jamovi Model table sends exactly that for every binomial outcome. Read
    # raw, a table of ordinary binary logits looked like a grouped-binomial one and lost the digest
    # fast path entirely (measured: every reference toggle refitted).
    !formula_mode && is.null(split_var) &&
    all(vapply(dependent, function(d) is.null(trials_for(d)), logical(1))) &&
    compare == "none" && !is_comparison && !("adjustment" %in% color) &&
    # Phase 18z15: a `shape` is a DIFFERENT MODEL, not a reparametrization of the canonical one, so
    # the digest cannot serve it (unlike `reference` / `multiplier`, which are exact transforms of it).
    # Phase 19k: `shape` IS reachable from the jamovi UI now (the per-predictor picker), so this is a
    # live narrowing -- a shaped model takes the raw-fit tier, where its `shape_terms` are part of
    # the key. A quadratic shape adds a TERM without changing the data, so that key entry is what
    # keeps it from colliding with the linear fit of the same predictors.
    length(reg_shapes) == 0L

  if (!is.null(reference) && !reref) {
    # A multinomial's baseline is the OUTCOME factor's first level, so `reference` keyed by the
    # dependent relevels it too (unified "reference level of any variable"). An ordinal outcome must
    # keep its order -> never releveled; predictor contrasts are releveled for every family. Phase 15e:
    # relevel every predictor + the MULTINOMIAL outcomes (per-dependent family).
    # Phase 18z13 (D7): and the SPLIT variable. `color = "between_groups"` compares every effect to
    # the FIRST split level's, so which level that is is a reference choice like any other -- but
    # `reference = c(race = "Black")` was silently dropped for it (split_var is not a predictor), and
    # the only way to move the baseline was to relevel the data upstream. One name in this union.
    relevelable <- union(union(all_predictors, split_var),
                         dependent[families_vec == "multinomial"])
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
                                 family = est$fit, do_exp = do_exp, effect_shape = effect_shape,
                                 eff_word = eff_word, color = color, est = est,
                                 crude_key = reg_crude_key(est$fit, trials_for(dependent), FALSE)))
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
    # Phase 19e: and its own resolved ESTIMAND row (`est`), of which do_exp / effect_shape / eff_word
    # are now views -- kept as fields because ~15 build sites read them by those names.
    specs  <- purrr::map2(dependent, labels,
                          ~ list(dependent = .x, predictors = predictors, label = .y,
                                 trials = trials_for(.x), inverse = inverse_for(.x),
                                 compound = formula_mode, formula = raw_formula,
                                 family = est_for(.x)$fit, do_exp = do_exp_for(.x),
                                 effect_shape = effect_shape_for(.x), eff_word = eff_word_for(.x),
                                 color = color_for(.x), est = est_for(.x),
                                 crude_key = reg_crude_key(est_for(.x)$fit, trials_for(.x),
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
    # Phase 18z13 (D9): a group in which the outcome or a predictor has ONE value cannot be fitted --
    # `contrasts can only be applied to factors with 2 or more levels`, or "the dependent must be
    # binary", both wrapped in purrr's `In index: 1.` noise, naming neither the group nor the variable.
    # Splitting by a coarsening of a predictor (race / black) is a common first attempt, so check it
    # here, where both names are in scope, in the shape tab() uses for its own degenerate inputs.
    if (!formula_mode) {
      sl   <- levels(forcats::fct_drop(as.factor(data[[split_var]])))
      vars <- intersect(unique(c(dependent, all_predictors)), names(data))
      bad  <- purrr::map(sl, function(g) {
        sub <- data[!is.na(data[[split_var]]) & data[[split_var]] == g, vars, drop = FALSE]
        if (nrow(sub) == 0L) return(stats::setNames(list(character(0)), g))
        flat <- vars[vapply(sub, function(v) length(unique(stats::na.omit(v))) < 2L, logical(1))]
        stats::setNames(list(flat), g)
      })
      bad <- purrr::flatten(bad)
      bad <- bad[lengths(bad) > 0L | vapply(sl, function(g) sum(!is.na(data[[split_var]]) &
                                                               data[[split_var]] == g) == 0L,
                                            logical(1))]
      if (length(bad) > 0L) {
        grp <- names(bad)[[1]]
        vb  <- bad[[1]]
        cli::cli_abort(c(
          "{.arg split_var} {.val {split_var}}: no model can be fitted within {.val {grp}}.",
          "x" = if (length(vb) == 0L) "That group has no rows left."
                else "{cli::qty(vb)}{.val {vb}} {?has/have} a single value there, so {?it/they} \\
                      cannot be a model term.",
          "i" = "Drop or merge that group (e.g. with {.fn forcats::fct_lump} or a {.fn filter}), \\
                 or split by a variable that varies within every group."
        ))
      }
    }
  }

  # multiplier (Phase 12g; grammar + resolution Phase 18z9): scale a CONTINUOUS predictor's effect to
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
    if (!mult_default && all(reg_fam_percategory(families_vec))) {
      cli::cli_abort("{.arg multiplier} is not supported for {.val multinomial}/{.val ordinal} models.")
    }
    if (!mult_default && any(reg_fam_percategory(families_vec))) {
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
  # Phase 18z9: "sd" is the DEFAULT scalar. Per 1 unit a numeric predictor sits inside the first
  # colour break and reads as "no effect" beside the factor contrasts next to it; per 1 SD it lands on
  # the same visual scale. `multiplier = 1` restores the per-unit reading.
  mult_scalar_default <- "sd"
  mult_res <- if (formula_mode || !any(reg_fam_glm(families_vec))) {
    list(k = NULL, label = NULL)
  } else {
    num_preds_all <- reg_numeric_preds(data, all_predictors)
    sd_frame <- reg_complete_frame(
      data, intersect(unique(c(all_predictors, wt)), names(data)))
    reg_resolve_multiplier(multiplier, mult_scalar_default, sd_frame, num_preds_all, wt = wt)
  }
  multiplier       <- mult_res$k
  multiplier_label <- mult_res$label

  # Phase 18z15: the quadratic terms, built on the SAME frozen frame as the multiplier's SD -- so
  # the centre and the unit of a curved predictor's two rows come from one measurement of one column,
  # and a split group / compared model cannot re-centre it. Empty unless a shape asked for one.
  shape_terms <- if (length(reg_shapes) > 0L)
    reg_shape_terms(reg_complete_frame(data, intersect(unique(c(all_predictors, wt)), names(data))),
                    reg_shapes, w = wt)
  else stats::setNames(character(0), character(0))

  # empirical (Phase 12g / 14v): the descriptive crude companion beside the model effect -- the
  # unadjusted bivariate association (which IS the modelised quantity when there is a single predictor).
  # Wired for binomial / gaussian / poisson (explicit columns) and multinomial (tooltip only). A vector
  # of dependents is supported (crude companion per dependent). Ordinal (cumulative OR) has no clean
  # crude analogue -> a message, not an error, and `empirical` is dropped for this call.
  # Phase 15e: kept ON whenever ANY outcome supports a crude companion (the per-fit loop skips the
  # ineligible outcomes -- ordinal -- individually). Only dropped when NO outcome is eligible.
  # Phase 18z10: one stored fact, not a third hand-written family list. What is left with no crude
  # counterpart at all is the compound-formula escape hatch (no predictor structure to be crude about).
  if (isTRUE(empirical) &&
      !any(purrr::map_lgl(dependent, ~ !is.na(reg_crude_key(family_for(.x), trials_for(.x),
                                                            formula_mode))))) {
    # Phase 18z15 (SS12.6 defect 1): name the REAL cause. A compound formula has no predictor
    # structure to be crude about, whatever the family -- the old message blamed the outcome family and
    # so told a binomial user their binomial outcome was unsupported.
    cli::cli_inform(if (formula_mode) c("i" = paste0(
      "{.arg empirical} (crude descriptive companion) needs one predictor per row; a compound formula ",
      "({.code poly()} / interactions / {.code I()}) has none, so it is ignored here."),
      "i" = 'Use {.arg predictors} with {.arg shape} for a curved term, e.g. {.code shape = c(age = "quadratic")}.')
      else c("i" = paste0(
      "{.arg empirical} (crude descriptive companion) is not available for any of these outcome ",
      "families; ignored here.")))
    empirical <- FALSE
  }

  # Phase 18z16-iiiii (defect 3): `degf` (#PSU - #strata) is captured ONCE at the boundary
  # (svy_unwrap_data -> svy$spec$degf) and this literal used to drop it, so tab_reg() was the only
  # consumer of a design that never saw its degrees of freedom. The model columns were on t(degf)
  # regardless -- stats::df.residual() of an svyglm IS the design df (see reg_glance()) -- while the
  # crude Obs_* columns stayed on z: measured at degf = 8, the crude bracket came out 15 % NARROWER
  # than the model bracket beside it, in a table whose whole premise (ruling 1) is that the two are
  # comparable. NULL for a plain data frame, exactly as in tab()'s spec.
  # WARNING: `design_obj` is re-assigned above (its `$variables` are swapped, and reg_relevel_design()
  #   may relevel a factor inside it). Neither touches PSUs or strata, so `degf` is stable.
  design_spec <- list(design = design_obj, wt = wt, degf = svy$spec$degf)
  # Phase 15e: check the Suggests deps of EVERY family present (nnet for multinomial, MASS for ordinal...).
  for (fm in unique(families_vec))
    reg_check_deps(fm, weighted,
                   needs_marginaleffects = any(vapply(est_vec, function(e) nzchar(e$needs),
                                                      logical(1))) || reg_display_folds(display))
  # Phase 17h: every per-call setting reg_build's leaves + assembler read, bundled once (the specs carry
  # the per-dependent family/do_exp/effect_shape/eff_word/color, so those scalars are no longer threaded).
  shared <- new_reg_shared(
    union_predictors = union_predictors, design_spec = design_spec, weighted = weighted,
    inverse_two_level_factors = inverse_two_level_factors, conf_level = conf_level, method = method,
    color_signif = color_signif, cleannames = cleannames, subtext = subtext,
    stats = stats, compare = compare, baseline = baseline, multiplier = multiplier,
    multiplier_label = multiplier_label, shape_terms = shape_terms, shape_labels = shape_labels,
    empirical = empirical, display = display,
    var_labels = reg_var_labels, na_shared_vars = na_shared_vars, add_n = add_n)
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
  # Phase 19e: the record stores the ESTIMAND -- `measures` beside `families`, per dependent, and the
  # resolved `effect` -- because a table must remember what it estimated or a refit silently changes
  # it. `exponentiate` / `at` / `do_exp` left with the arguments they mirrored; `eff_word` stays as
  # the table-level narrative's scalar (the per-column word comes from the column's own attributes).
  reg_call_record <- list(
    family = family, families = families_vec,
    effect = est$effect, measure = est$measure, eff_word = eff_word,
    measures = vapply(est_vec, function(e) e$measure, character(1)),
    effects  = vapply(est_vec, function(e) e$effect,  character(1)),
    dependent = dependent, positive_level = positive_levels, predictors = union_predictors,
    # Phase 18z9: the predictor-kind map is STORED, not re-derived from the rendered table. Nothing
    # recorded it before, and the only implicit marker (a numeric row's `level == var`) is already
    # broken by `cleannames` and by the multiplier relabel. `multiplier` records the RESOLVED per-unit
    # scaling actually used (the frozen SDs included), so the footer/legend can name the unit.
    predictor_types = reg_predictor_types(data, union_predictors), multiplier = multiplier,
    # Phase 18z15: THE recipe reg_check_plots() refits from -- the specs plus the handful of scalars
    # reg_fit() takes, ~4 KB of strings. Deliberately NOT the fits themselves: ~10 MB each was the
    # measured cause of the Phase-o jamovi freeze, and a 60 ms refit through the very fitter the table
    # came from is both cheaper and impossible to drift from.
    fit_spec = list(specs = specs, method = method, conf_level = conf_level,
                    inverse_two_level_factors = inverse_two_level_factors,
                    na_shared_vars = na_shared_vars, shape_terms = shape_terms,
                    multiplier = multiplier, effect = est$effect, measure = est$measure,
                    wt = wt_disp, design_vars = reg_design_vars(design_spec)),
    # Phase 19a: `shape` and `model_labels` are DELETED -- write-only across the whole repo (verified:
    # the seven readers of reg_meta take family / fit_spec / split_var / wt / dependent, and every
    # other `$shape` hit in R/ is the effect-emission spec, a different object). `conf_level` stays
    # for now: study §5 rules it "use it to unlock further simplification", which is 19g's item.
    # Phase 18z10: which observed counterpart each outcome has (NA = none). Stored, so the footer can
    # word the in-cell "{or} ({obs})" bracket and ?tab_reg can state the scope honestly.
    crude_keys = if (isTRUE(empirical))
      stats::setNames(purrr::map_chr(specs, ~ .$crude_key), purrr::map_chr(specs, "dependent"))
      else stats::setNames(rep(NA_character_, length(specs)), purrr::map_chr(specs, "dependent")),
    split_var = split_var, comparison = is_comparison, wt = wt_disp
  )
  # Phase 19g (KEY 6): the model record IS this table's `spec$call` -- "how was this table made",
  # the slot every producer has, rather than a regression-only sibling of `meta$vars`. `conf_level`
  # left it here: it was a stale duplicate of a per-COLUMN attribute (tab_stamp_inference stamps the
  # level on every column, and get_conf_level() is what every consumer reads), so keeping a
  # table-wide copy could only ever disagree with the columns it described.
  set_reg_call(res, reg_call_record)
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
                      inverse_two_level_factors = TRUE, split_var = NULL, multiplier = "sd",
                      shape = NULL, empirical = FALSE, add_n = TRUE,
                      conf_level = conf_level_default(),
                      method = c("wald", "profile"),
                      stats = NULL, display = "value",
                      color_signif = c("grey_non_signif", "ignore", "guaranteed_effect"),
                      stars = TRUE, na = c("drop_by_outcome", "drop_by_model", "drop_all"),
                      cleannames = NULL, subtext = "") {
  method       <- match.arg(method)
  color_signif <- match.arg(color_signif)
  na           <- match.arg(na)
  stopifnot(is.character(predictors), length(predictors) >= 1L)
  tab_reg(data, dependent = dependent, predictors = predictors, family = "binomial", wt = wt,
          split_var = split_var,
          multiplier = multiplier, shape = shape, empirical = empirical, add_n = add_n,
          conf_level = conf_level, method = method, stats = stats,
          display = display,
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
                        inverse_two_level_factors = TRUE, split_var = NULL, multiplier = "sd",
                        empirical = FALSE, add_n = TRUE,
                        conf_level = conf_level_default(),
                        method = c("wald", "profile"),
                        stats = NULL, compare = c("none", "baseline", "sequential"), baseline = NULL,
                        display = "value",
                        color_signif = c("grey_non_signif", "ignore", "guaranteed_effect"),
                        stars = TRUE, na = c("drop_by_outcome", "drop_by_model", "drop_all"),
                        cleannames = NULL, subtext = "") {
  method       <- match.arg(method)
  compare      <- match.arg(compare)
  color_signif <- match.arg(color_signif)
  na           <- match.arg(na)
  # Phase 14x: `dependent` may be a VECTOR -> the model comparison runs once per dependent (tab_reg's K
  # mode: a models list + several dependents -> one table each, returned as a tabxplor_tabs list).
  stopifnot(is.character(dependent), length(dependent) >= 1L, is.list(models), length(models) >= 1L)
  tab_reg(data, dependent = dependent, predictors = models, family = "binomial", wt = wt,
          split_var = split_var,
          multiplier = multiplier, empirical = empirical, add_n = add_n,
          conf_level = conf_level, method = method,
          stats = stats, compare = compare, baseline = baseline,
          display = display,
          inverse_two_level_factors = inverse_two_level_factors,
          color_signif = color_signif, stars = stars, na = na,
          cleannames = cleannames, subtext = subtext)
}
