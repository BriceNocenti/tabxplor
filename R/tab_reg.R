# PURPOSE: Regression tables (effect measures) as native tabxplor_tab objects.
# ROLE: tab_reg() fits one model per column across families and renders the per-family effect measure
#   -- gaussian beta (additive), binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR
#   (multiplicative) -- through the tabxplor_fmt diff|or / ci_inf|ci_sup / pvalue / var fields, so a
#   regression table prints, colours and exports (kable / md / Excel) exactly like a crosstab.
# KEY CONSTRAINTS:
#   - Direct engine: stats::lm/glm (unweighted) / survey::svyglm (weighted) + nnet::multinom (nominal
#     3+ level) + MASS::polr (ordinal 3+ level), all tidied with broom::tidy. No parsnip.
#   - broom (always), survey (wt path), MASS (ordinal + method="profile"), nnet (multinomial), brant
#     (ordinal PO diagnostic) are Suggests -> guarded.
#   - CI <-> p are DUALS (CI <-> stars can never disagree). method="wald" (default): in-house Wald
#     CI (coef +/- crit*se, exp()'d for ratio measures) + the model's own Wald p; crit is z for
#     fixed-dispersion glm (binomial/poisson), t(df.residual) for lm / quasi* / weighted svyglm --
#     matching broom's z/t p exactly. method="profile" (unweighted glm): confint + LR-test p.
#   - Effect shape follows the ESTIMAND's declared `scale` (REG_ESTIMANDS -> EST_SCALES), which names
#     the field the estimate lands in and the ladder it is graded on: MULTIPLICATIVE (odds_ratio /
#     rate_ratio) -> the `or` field, neutral 1, the 1/x reciprocal rendering; ADDITIVE (raw_diff /
#     points / log_coef) -> the `diff` field, neutral 0, with `var` = var(Y) where the scale declares
#     it, so the colour is the effect size beta/SD(Y) against the Cohen breaks. Every column is built
#     displaying `est`, the scale-relative token -- no builder names a family-specific one.
#   - EVERY MODEL COLUMN CARRIES ITS ADJUSTED PREDICTION and its additive marginal effect, whether or
#     not the cell prints them (reg_fill_base). That is what makes `display` a pure post-hoc
#     property: choosing a layout never triggers a computation and never changes a number, so
#     set_display() on a built table gives the same table as asking for it at build time.
#   - 12c-ii: `trials` fits a summed-score outcome as GROUPED binomial (cbind(score, trials-score));
#     a model FORMULA in `outcome` is the escape hatch -- a simple `y ~ a + b` reduces to the
#     outcome+predictors path, a compound one (interactions / poly() / I()) is fit verbatim with a
#     best-effort skeleton read from the fitted terms (reg_skeleton_from_fit).
#   - 12d: nominal 3+ level -> ONE multinom -> reg_build splits its `y.level` tidy into one OR column
#     per non-reference category ("<j> vs <ref>: OR"); the outcome baseline is set by `reference`
#     keyed on the outcome. Ordered 3+ level -> polr -> one cumulative-OR column (cut-point rows
#     dropped -> "Constant" NA), with a Brant PO diagnostic (reg_ordinal_diagnostic, self-heals the
#     fit's $call so brant works out of the fitting scope). Both reuse the OR fmt shape unchanged;
#     both share reg_wald_from_tidy so CI <-> p <-> stars stay exact duals. Weighted MNL/ordinal
#     deferred (guarded error).
#   - 12e-i: effect="ame" (marginaleffects Suggests, guarded) is the orthogonal interpretation axis --
#     sample-average marginal effects + adjusted predictions on the RESPONSE scale. reg_marginal()
#     wraps avg_comparisons()/avg_predictions() (newdata = the fitted frame is REQUIRED); a factor AME
#     is keyed by (var, level) from the "Level - Reference" contrast label. reg_marginal_column()
#     builds them like any other column -- the estimate in the field its scale names, displaying
#     `est` -- so the AME and the coefficient differ in what they ESTIMATE, never in how they print.
#     MNL/ordinal -> one AME column per outcome CATEGORY (all levels).
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
#     anova.svyglm Wald (compare_*_wald). `tab_vars` = the tab_vars analogue: reg_build recurses per
#     group on a SHARED skeleton (skeleton_data) and stacks into a grouped_tab (tab_vars + var), so
#     tab_spread(tab_vars) pivots groups to columns (no tab_spread change: tab_vars placed first so
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
# (both R Recommended -> normally present); marginaleffects only where an estimand's engine resolves
# to it -- `effect = "at_reference"`, whose one-row profile grid g-computation does not build.
# The other marginal quantities run on the dependency-free gcomp engine, which is what lets every
# model column populate them unconditionally.

# THE ONE marginaleffects abort, so the upfront guard and reg_marginal()'s fallback say one thing.
#' @keywords internal
#' @noRd
reg_abort_marginaleffects <- function(what) {
  cli::cli_abort(c(
    "{.pkg marginaleffects} is required for {.code {what}}.",
    "i" = 'Install it with {.code install.packages("marginaleffects")}, or use
           {.code effect = "coefficient"} / {.code effect = "marginal"}, which need no extra package.'
  ))
}

reg_check_deps <- function(family, weighted, needs_marginaleffects = FALSE) {
  if (needs_marginaleffects && !requireNamespace("marginaleffects", quietly = TRUE))
    reg_abort_marginaleffects('effect = "at_reference"')
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
# A simple formula reduces losslessly to the outcome+predictors character path; a compound one is
# fit verbatim with a skeleton read from the fitted terms (reg_skeleton_from_fit).
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

# DESIGN (Phase 18z3): the family PREDICATES. Every "which families behave like X" question is asked
# here ONCE instead of by a hand-written whitelist at each call site (there were 11 bare `== "binomial"`
# tests, 4 probability-scale lists, and the log-scale list written TWICE verbatim in fmt_class.R). The
# internal family key "rr" (modified Poisson on a binary outcome, resolved in
# reg_resolve_estimands(), R/reg-resolve.R) joins the binary + log-scale sets here and nowhere else.
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
# Is this fit a GROUPED binomial -- a summed-score outcome fit from (successes, trials)?
# WARNING: the question is about the OUTCOME family, never the fit key. `measure = "ratio"` and
# `"difference"` resolve a binary outcome to the internal links `rr` / `rd`, which are binomial fits
# under another link; testing `family == "binomial"` therefore dropped `trials` on both, and their
# arms then met the raw 0..q score instead of a 2-level factor. reg_fam_binary() is the declared
# reader of that fact (REG_FIT_FAMILY), so the three call sites keep one answer.
# The compound-formula clause is part of the fact: a compound formula controls its own LHS, so
# `trials` does not apply to it.
reg_is_grouped_binomial <- function(family, trials, compound = FALSE)
  reg_fam_binary(family) && !is.null(trials) && !isTRUE(compound)
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
reg_color_notes <- function(color, color_signif, ests, tab_vars, na, na_explicit,
                            empirical = FALSE) {
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
  if (length(gap) == 0L && !emp_on(empirical)) return(notes)

  if ("between_groups" %in% gap && is.null(tab_vars)) {
    add("{.code color = \"between_groups\"} compares each effect to the first group's, so it needs ",
        "{.arg tab_vars} to say what the groups are. Without it nothing is coloured.")
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
    # WARNING: asked of each resolved estimand's FIT, never of the outcome family -- the same key
    # reg_gap_se_columns() gates the real test on (R/reg-empirical.R). Only a conditional odds ratio
    # is non-collapsible; `measure = "ratio"` / `"difference"` change the link (to `rr` / `rd`) and
    # with it the estimand, so reading the outcome family made this note fire on a risk ratio and a
    # risk difference -- advising the very `measure = "ratio"` that had just been asked for, and
    # contradicting the gap SE the build had already computed.
    if (!is.null(color_signif) && !identical(color_signif, "ignore") &&
        !any(vapply(ests, function(e) reg_estimand_collapsible(e$fit, e$effect), logical(1)))) {
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
  # WARNING: the grouped test comes FIRST, and must stay there. `rd` (and `rr`) are binomial fits, so
  # a summed-score outcome under either is a grouped binomial like any other -- its crude base is the
  # mean SCORE, not a share of respondents. Short-circuiting on the link key ahead of it would hand
  # them the individual-level block.
  if (reg_is_grouped_binomial(family, trials, compound)) return("grouped_binomial")
  # The identity-link risk-difference fit shares the binomial's crude block outright -- its base is
  # the same risk and its effect the same Wald risk difference (REG_EMPIRICAL$binomial's `base` /
  # `ame` rows), which is why "rd" needs no block of its own.
  if (identical(family, "rd"))                          return("binomial")
  if (is.null(REG_EMPIRICAL[[family]]))                 return(NA_character_)
  family
}

# Phase 19m-ii: `trials = TRUE` / `trials = c(outcome = NA)` means "take the observed maximum" -- THE rule
# tab_reg() has always applied and the jamovi bridge open-coded until 19k. NA where there is none: a
# factor outcome is an ordinary binary logit, not a grouped binomial, and a 0/1 numeric has no
# trial count to read. A pure function of one column, so it is testable without building a table.
#' @keywords internal
#' @noRd
reg_trials_observed_max <- function(x) {
  if (!is.numeric(x) || is.factor(x)) return(NA_real_)
  m <- suppressWarnings(max(x, na.rm = TRUE))
  if (is.finite(m) && m > 1) m else NA_real_
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
# PREDICTORS + design variables, deliberately NOT of the outcome -- so one predictor keeps ONE unit
# across several outcomes, across compared models and across tab_vars groups. A per-group SD would make
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
# REG_OUTCOME_KINDS -- Phase 19k: THE outcome-kind table. One row per kind of outcome variable the
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
# Phase 19m-i: `said` -- how reg_detect_family() NAMES the kind it detected. It was a `switch(kind,)`
# re-spelling this table's own four keys twenty lines below, with no default arm (an unlisted kind
# produced NULL, which paste0() silently dropped). Kept a BARE string, not a gettext(): it is not
# translated today, and wrapping it would add four msgids to po/R-fr.po as a side effect of a
# refactor.
#' @keywords internal
REG_OUTCOME_KINDS <- list(
  binary   = list(detect = "binomial",    offers = c("binomial", "poisson"),
                  said = "binary outcome detected"),
  ordered  = list(detect = "ordinal",     offers = c("ordinal", "multinomial"),
                  said = "ordered outcome detected"),
  nominal  = list(detect = "multinomial", offers = c("multinomial", "ordinal"),
                  said = "nominal outcome detected"),
  # Phase 18z13 (D10): ANY numeric is gaussian, integer-valued included -- age in years, a summed
  # score and income in whole units are all integers, and a linear model always fits. poisson stays
  # one click away in `offers`.
  numeric  = list(detect = "gaussian",    offers = c("gaussian", "binomial", "poisson"),
                  said = "continuous outcome detected")
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
  cli::cli_inform(c("i" = paste0(
    "{.val {outcome}}: ", said, " -> {.code family = \"", fam, "\"} (",
    reg_family_short(fam), ")",
    if (identical(kind, "numeric") && !any(y %% 1 != 0, na.rm = TRUE))
      "; it is integer-valued, so {.code family = \"poisson\"} if it is a count" else "",
    "."
  )))
  fam
}

# Phase 19m-i: the family NAMES (the footer sentence, the Excel filename tag, the two picker labels)
# are ONE declared table, REG_FAMILIES (R/reg-estimand.R) -- four tables and a switch before, which
# had already drifted. reg_family_display_name() / reg_family_short() / reg_family_ui_labels() are
# its readers and live there; every call site here is unchanged.


# Phase 14w: the "Model: <family>. <estimand>." legend line, generated fresh from the table's stored
# recipe (`meta$spec$call`, Phase 19g) at render
# so it can be ordered BEFORE the colour legend (item 2). For a model comparison the caption is not shown
# in the console, so the outcome + (binomial) reference level are named here too (item 4). NULL when the
# table is not a regression (reg_call -> NULL).
# Phase 18w: the prose is translatable (gettext); called only from reg_model_lines(), which sets the
# LANGUAGE env via with_legend_lang(). enc2utf8 for the French accents (matches tab_weight_line et al.).
# Does the observed effect ride INSIDE the model cell? One stored fact, resolved at the argument
# boundary (reg_emp_mode()); `deps` narrows to a group's outcomes, which only matters for the crude
# keys that say whether there is an observed effect at all.
#' @keywords internal
reg_meta_obs_in_cell <- function(meta, deps = NULL) {
  if (!identical(meta$emp_mode, "cell")) return(FALSE)
  ck <- meta$crude_keys
  if (is.null(ck)) return(FALSE)
  if (!is.null(deps)) ck <- ck[intersect(names(ck), deps)]
  any(!is.na(unlist(ck)))
}

# reg_meta_estimand() -- the stored ESTIMAND of one outcome, re-resolved from the recipe (Phase
# 19e). The record keeps the words (`families` / `effects` / `measures`); the row is looked up rather
# than stored, so a table cannot carry a row that a later version's library disagrees with.
#' @keywords internal
reg_meta_estimand <- function(meta, outcome = NULL, family = NULL) {
  d   <- if (is.null(outcome)) NULL else as.character(outcome)
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
    w  <- if (!is.na(pl)) gettextf("of %s ('%s')", meta$outcome[[1]], pl)
          else            gettextf("of %s", meta$outcome[[1]])
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
  # Phase 19m-i: this guard genuinely asks "is there a RECIPE to describe" -- NOT "is this a
  # regression" (that is tab_is_reg(), the stored kind). The two diverge on a meta-stripped reg
  # table, which keeps its kind and its columns but has no model left to name: there is no line to
  # write, and returning none is right. Kept as it is, deliberately.
  meta <- reg_call(x)
  if (is.null(meta)) return(character(0))
  with_legend_lang(lang, function(lg) {
    fams <- meta$families; if (is.null(fams)) fams <- meta$family
    uf   <- unique(fams)
    if (length(uf) <= 1L) { rl <- reg_model_line(meta); return(if (is.null(rl)) character(0) else rl) }
    deps <- meta$outcome
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
  sv   <- if (is.null(meta)) NA_character_ else meta$tab_vars
  with_legend_lang(lang, function(lg) {
    # Phase 20c: the phrase is the row's own declared `instrument` (TEST_ROWS) -- it was the third
    # copy of the interaction discriminators, and the only one carrying their names.
    tname <- vapply(reg_interaction_types(),
                    function(k) gettext(TEST_ROWS[[k]]$instrument), character(1))
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
    dep <- tab_title_names(meta$outcome, max)
    # edge whitespace stays OUT of gettext msgids (xgettext strips it); the leading space is added here.
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

# Phase 14w (item 1): a compact Excel sheet name for a reg table -- "<short>_<dep>_<pred>..." (e.g.
# "logit_married_race_rincome"), truncated to 25 chars by the caller. A comparison collapses the
# predictors to "compare" (they differ per model).
reg_sheet_name <- function(meta) {
  if (is.null(meta)) return(NA_character_)
  fams <- meta$families; if (is.null(fams)) fams <- meta$family
  tail <- if (isTRUE(meta$comparison)) c(meta$outcome[[1]], "compare")
          else                         c(meta$outcome, meta$predictors)
  # Phase 15e: a mixed-family table gets a generic short tag ("reg") instead of one family's.
  short <- if (length(unique(fams)) > 1L) "reg" else reg_family_short(meta$family)
  paste(c(short, tail), collapse = "_")
}

# Phase 14w (item 3): the shared col_var for a SINGLE-outcome model column + its empirical companions,
# so ONE span header names the outcome and no border separates them (they share a col_var). Binomial ->
# "<dep>: <positive_level>"; a numeric outcome (gaussian/poisson) -> the outcome name alone. NOT used in
# comparison mode (each model keeps its own col_var = model name, so borders separate the models, and the
# outcome / reference / effect go in the title instead).
reg_shared_col_var <- function(family, outcome, positive_level, cleannames) {
  if (reg_fam_binary(family) && !is.null(positive_level) && !is.na(positive_level)) {
    pl <- reg_cleanup(positive_level, cleannames)
    paste0(outcome, ": ", pl)
  } else outcome
}

# Phase 14w (item 3): the single-model column NAME ("Model_OR" / "Model_IRR" / "Model_AME (adjusted %)"),
# so the effect word lives in the column, not repeated in the span. Comparison mode keeps the model name;
# a multi-outcome (several outcomes, one predictor set) suffixes the outcome so the names stay unique.
# Phase g: "Model_" (snake-case) prefix; the multi-outcome disambiguator is a "[dep]" BRACKET, which the
# console shows and every exporter STRIPS (tab_col_var_header) -- the col_var span row already names the
# outcome, so repeating it per column wasted export width.
reg_model_col_name <- function(eff_word, outcome, is_comparison, model_label, n_outcomes) {
  if (isTRUE(is_comparison)) return(model_label)
  if (n_outcomes > 1L) paste0("Model_", eff_word, " [", outcome, "]") else paste0("Model_", eff_word)
}

# Prepare a binary outcome: a 0/1 numeric becomes a 2-level factor ("Not <dep>" / "<dep>"); any
# other input must have exactly 2 levels, ordered so glm models the one the user asked for.
#
# Phase 20c: `outcome_level` NAMES that level, where `outcome_level` toggled the level
# ORDER to imply it. Three things improve, and the third is what forced the change: the argument says
# what the user knows (a level, not a direction), it is checkable against the column, and it WORKS on
# the 0/1 numeric path -- where the logical was a silent no-op, because that branch returns before
# ever reaching the reversal.
# NULL keeps the historical default exactly: the FIRST level is modelled ("1-Married" before
# "2-Not married"), which is the maintainer's own coding convention.
reg_prep_binary <- function(data, outcome, outcome_level = NULL) {
  y <- data[[outcome]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    y <- factor(y, levels = c(0, 1), labels = c(paste0("Not ", outcome), outcome))
    # "0" / "1" are accepted spellings of the synthesised labels (reg_outcome_levels()).
    if (!is.null(outcome_level) && outcome_level %in% c(paste0("Not ", outcome), "0"))
      y <- forcats::fct_rev(y)
  } else {
    y <- forcats::fct_drop(as.factor(y))
    if (nlevels(y) != 2L) {
      cli::cli_abort(c(
        "The outcome variable {.val {outcome}} must be binary (2 levels).",
        "x" = "It has {nlevels(y)} level{?s}: {.val {levels(y)}}.",
        "i" = paste0("For a summed score -- how many of q yes/no items each person chose -- pass ",
                     "{.arg trials} to fit a grouped binomial."),
        "i" = paste0("For an outcome with 3 or more categories, use {.code family = \"multinomial\"} ",
                     "(unordered) or {.code family = \"ordinal\"} (ordered).")
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

# The modelled ("positive") level of a binary outcome, computed from the raw data (for the column
# label, before fitting). THE one rule reg_prep_binary() also applies: what the user named, else the
# first level.
reg_positive_level <- function(data, outcome, outcome_level = NULL) {
  y <- data[[outcome]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    # the synthesised pair, and the raw codes as accepted spellings of it
    neg <- paste0("Not ", outcome)
    return(if (identical(outcome_level, "0") || identical(outcome_level, neg)) neg else outcome)
  }
  lv <- levels(forcats::fct_drop(as.factor(y)))
  if (!is.null(outcome_level) && outcome_level %in% lv) outcome_level else lv[[1L]]
}

# Relevel factor predictors to user-chosen treatment-contrast baselines. `reference` is a named
# vector c(var = "baseline level"). Regression references are model contrasts (fct_relevel), NOT the
# crosstab comparison-row semantics of resolve_ref_vector().
reg_apply_references <- function(data, ref, predictors, outcomes = character(0)) {
  nm <- names(ref)
  if (is.null(nm) || any(!nzchar(nm))) {
    cli::cli_abort(c("{.arg ref} must be a named vector, e.g. {.code c(race = \"White\")}."))
  }
  extra <- setdiff(nm, predictors)
  # Phase 20c: an OUTCOME named here is not a typo, it is the other question -- `reference` names the
  # level you compare against, `outcome_level` the level you model. It used to fall through to the
  # generic "matches no predictor" warning, which said nothing about the argument that does work.
  wrong <- intersect(extra, outcomes)
  if (length(wrong))
    cli::cli_abort(c("{.val {wrong[[1]]}} is an outcome, not a predictor, so {.arg ref} cannot set its level.",
                     "i" = paste0("{.arg ref} names the level other levels are compared AGAINST; ",
                                  "{.arg outcome_level} names the level that is MODELLED."),
                     "i" = 'Did you mean {.code outcome_level = c({wrong[[1]]} = "{ref[[wrong[[1]]]]}")}?'),
                   call = NULL)
  extra <- setdiff(extra, outcomes)
  if (length(extra) > 0L) {
    cli::cli_warn("{.arg ref} name{?s} {.val {extra}} match no predictor; ignored.")
  }
  for (v in intersect(nm, predictors)) {
    f <- data[[v]]
    if (!is.factor(f) && !is.character(f)) {
      cli::cli_warn("{.arg ref} ignored for {.val {v}}: not a factor/character predictor.")
      next
    }
    f   <- as.factor(f)
    lev <- ref[[v]]
    if (!lev %in% levels(f)) {
      cli::cli_abort(c("{.arg ref} level {.val {lev}} not found in {.val {v}}.",
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
  mm      <- stats::model.matrix(fit)
  assign  <- attr(mm, "assign")                         # 0 = intercept, k = labels[k]
  # ⚠ the coefficient names come off the MODEL MATRIX, which is the vector `assign` indexes -- NOT
  # off coef(), whose shape is the FITTER's convention (Phase 20f-iiii, two measured defects):
  # nnet::multinom returns a MATRIX for k > 2 outcome categories, so names() is NULL and every
  # non-pure-factor term produced ZERO rows (`party3 ~ race*age` built 4 rows instead of 7 -- the
  # numeric main effect and both interactions vanished in silence); MASS::polr drops the intercept
  # from coef() but not from the model matrix, so the two vectors were off by one and every term
  # after the first was mislabelled. For lm / glm / svyglm the two are identical, which is why this
  # went unseen. broom::tidy() names its terms after the same model-matrix columns, so reg_column()
  # still aligns exactly.
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
# reg_fit_formula() -- THE model formula of the two 3+ level engines, and the one rule the glm arm
# already applied inline: a compound `formula` is fitted VERBATIM (it controls its own RHS, so the
# shape terms do not apply to it); otherwise the bare predictors plus any shape term, backticked.
# ⚠ Phase 20f-iiii: both engines used to BUILD the formula and never see the user's, so
# `tab_reg(d, party3 ~ race * age, family = "multinomial")` silently fitted `race + age` -- the
# interaction was dropped from the model, not merely from the table. The escape hatch is documented
# for every family, so it must reach every fitter.
#' @keywords internal
#' @noRd
reg_fit_formula <- function(outcome, predictors, add_terms = NULL, formula = NULL) {
  if (!is.null(formula)) return(formula)
  stats::as.formula(paste0(
    "`", outcome, "` ~ ",
    paste(c(paste0("`", predictors, "`"), add_terms), collapse = " + ")))
}

reg_fit_multinom <- function(mdata, outcome, predictors, do_exp, conf_level, method,
                             weighted = FALSE, make_design = NULL, add_terms = NULL,
                             formula = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for multinomial models; using Wald."))
  }
  mdata[[outcome]] <- forcats::fct_drop(as.factor(mdata[[outcome]]))
  y_levels <- levels(mdata[[outcome]])
  # ⚠ re-home the formula to THIS frame, which is where `fml` and `mdata` are: nnet::multinom and
  # MASS::polr store their call and re-evaluate it (model.frame.multinom / model.matrix.polr, which
  # reg_skeleton_from_fit() then reads), so a formula carrying the user's environment resolves
  # `fml` nowhere. The built formula needs it too -- as.formula()'s environment is its caller's.
  fml <- reg_fit_formula(outcome, predictors, add_terms, formula)
  environment(fml) <- environment()

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
reg_fit_ordinal <- function(mdata, outcome, predictors, do_exp, conf_level, method,
                            weighted = FALSE, make_design = NULL, add_terms = NULL,
                            formula = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for proportional-odds models; using Wald."))
  }
  y <- mdata[[outcome]]
  if (!is.ordered(y)) {
    y <- as.ordered(forcats::fct_drop(as.factor(y)))
    lv_str <- paste(levels(y), collapse = " < ")
    cli::cli_inform(c("i" = "{.val {outcome}}: treated as ordered ({lv_str})."))
  } else {
    y <- forcats::fct_drop(y)
  }
  mdata[[outcome]] <- y
  # ⚠ re-home the formula to THIS frame, which is where `fml` and `mdata` are: nnet::multinom and
  # MASS::polr store their call and re-evaluate it (model.frame.multinom / model.matrix.polr, which
  # reg_skeleton_from_fit() then reads), so a formula carrying the user's environment resolves
  # `fml` nowhere. The built formula needs it too -- as.formula()'s environment is its caller's.
  fml <- reg_fit_formula(outcome, predictors, add_terms, formula)
  environment(fml) <- environment()

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
  # Phase 20f: the Brant PO test is NOT run here. It is a footer ROW's statistic and it costs a fit
  # (J-1 binary logits, ~1.1 s at n = 21 483), so it is computed where that row is built --
  # reg_check_rows()'s `proportionality` branch. Running it at fit time meant paying for it on every
  # diagnostic and crude univariable polr fit and reading exactly one of them.
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
# The model's complete-case frame: drop rows missing the outcome, ANY predictor, or a design var --
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
    # Phase 18z14-iii: index the ORIGINAL design, always. Under tab_vars `data` holds one group's
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
reg_relevel_design <- function(design, ref, relevelable) {
  design$variables <- reg_apply_references(design$variables, ref, relevelable)
  design
}

# Fit ONE model on complete cases -> a tidy of the (per-family) effect measure + CI + p + the model n
# (+ var(Y) for the additive gaussian effect-size colour). `do_exp` chooses the estimate scale:
# TRUE -> exp(coef) (OR/IRR, multiplicative); FALSE -> raw coef (beta, additive). Wald CI uses z for
# fixed-dispersion glm (binomial/poisson), else t(df.residual); this matches broom's own z/t p, so
# the CI and the stars are exact duals. method="profile" (unweighted glm) swaps to confint + LR p.
reg_fit <- function(data, outcome, predictors, family, design_spec, do_exp,
                    outcome_level, conf_level, method,
                    trials = NULL, formula = NULL, multiplier = NULL, cross = NULL,
                    drop_extra = NULL, add_terms = NULL) {
  # Phase 18z15: `add_terms` is the third sibling of `cross` / `drop_extra` -- extra RHS terms,
  # verbatim, appended to the formula and to nothing else (they name no new VARIABLE, so they never
  # join drop_vars: `I(((age - 44.2)/13.5)^2)` is complete exactly where `age` is). It is how the
  # Linearity check refits "the model plus this predictor's centred squared term" through the very
  # fitter the table came from, inheriting the binary prep, the grouped-binomial cbind, the "rr" route
  # and the design resolution -- which the `formula =` escape hatch would not.
  #
  # Phase 18z8: `cross` (a tab_vars) makes the POOLED interaction fit `y ~ (x1 + x2) * g`, used
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
  drop_vars <- unique(c(outcome, predictors, cross, drop_extra, reg_design_vars(design_spec)))
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
    return(reg_fit_multinom(mdata, outcome, predictors, do_exp, conf_level, method,
                            weighted, make_design, add_terms = add_terms, formula = formula))
  }
  if (family == "ordinal") {
    return(reg_fit_ordinal(mdata, outcome, predictors, do_exp, conf_level, method,
                           weighted, make_design, add_terms = add_terms, formula = formula))
  }

  positive_level <- NULL
  # grouped binomial: a summed-score outcome (0..trials) fit as cbind(score, trials-score) (D2). Only
  # on the non-formula path (a compound formula controls its own LHS, so `trials` does not apply).
  grouped <- reg_is_grouped_binomial(family, trials, !is.null(formula))
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
    # Two derived columns the non-logit links need, because neither can take a two-column response:
    # `.gb_trials` is the modified Poisson's offset (so exp(coef) stays a PER-ITEM ratio), `.gb_prop`
    # the observed per-item risk the identity link's start values and its LPM fallback are fitted on.
    mdata[[".gb_trials"]] <- trials
    mdata[[".gb_prop"]]   <- s / trials
  }

  fam_obj <- switch(
    family,
    "binomial" = {
      if (is.null(trials) && is.null(formula)) {
        mdata <- reg_prep_binary(mdata, outcome, outcome_level)
        positive_level <- attr(mdata, "positive_level")
      }
      if (weighted) stats::quasibinomial("logit") else stats::binomial("logit")
    },
    "poisson" = if (weighted) stats::quasipoisson("log") else stats::poisson("log"),
    "quasipoisson" = stats::quasipoisson("log"),
    # Phase 18z3 -- modified Poisson on a binary outcome (Zou 2004). Same binary prep as the logistic
    # arm (so `outcome_level` and the positive-level label behave identically), then the
    # factor is coerced to the 0/1 NUMERIC a log-link Poisson needs: glm(poisson) / svyglm both error on
    # a factor response. quasipoisson (not poisson) in BOTH the weighted and unweighted case -- the fit
    # goes through svyglm either way (see the dispatch below), and it also makes AIC/BIC return NA, which
    # is the honest answer for a quasi-likelihood.
    "rr" = {
      # On the grouped path the outcome is already a success COUNT, so there is nothing to recode:
      # the response is that count with log(trials) as offset (built with the formula below).
      if (!grouped) {
        mdata <- reg_prep_binary(mdata, outcome, outcome_level)
        positive_level <- attr(mdata, "positive_level")
        mdata[[outcome]] <- as.numeric(mdata[[outcome]] == positive_level)
      }
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
      # Grouped: the two-column response is already the per-item risk the identity link estimates, so
      # the recode is skipped and cbind(successes, failures) is fitted as it is.
      if (!grouped) {
        mdata <- reg_prep_binary(mdata, outcome, outcome_level)
        positive_level <- attr(mdata, "positive_level")
        mdata[[outcome]] <- as.numeric(mdata[[outcome]] == positive_level)
      }
      stats::binomial("identity")
    },
    # Phase 19e -- the RATIO OF MEANS: Poisson pseudo-maximum-likelihood with robust standard errors
    # (Santos Silva & Tenreyro 2006), i.e. the "rr" recipe on a continuous outcome. exp(coef) is the
    # ratio of adjusted means; the Poisson likelihood is a device for the log link, not a claim about
    # counts, and the sandwich is what makes it honest. The outcome must be non-negative.
    "mr" = {
      y <- suppressWarnings(as.numeric(mdata[[outcome]]))
      if (any(is.finite(y) & y < 0)) cli::cli_abort(c(
        '{.code measure = "ratio"} needs a non-negative outcome: a ratio of means is not defined when {.val {outcome}} can be negative.',
        "i" = 'Model {.code log()} of a positive outcome instead, or use {.code measure = "difference"}.'))
      stats::quasipoisson("log")
    },
    "gaussian" = stats::gaussian(),
    cli::cli_abort("Unsupported {.arg family}: {.val {family}}.")
  )
  if (is.null(formula) && !grouped && !reg_fam_binary(family) && !is.numeric(mdata[[outcome]])) {
    cli::cli_abort(c(
      "A {.val {family}} outcome must be numeric.",
      "x" = "{.val {outcome}} is {.cls {class(mdata[[outcome]])}}."
    ))
  }

  # `fml_lpm` is fml's linear-probability twin: the same right-hand side over a SINGLE-column
  # response. Only the identity link uses it (start values, and the fallback fit), and only on the
  # grouped path does it differ from fml -- cbind(successes, failures) is not a linear model, while
  # the observed proportion estimates the very same per-item risk difference.
  fml_lpm <- NULL
  fml <- if (!is.null(formula)) {
    formula                                            # compound escape-hatch: fit verbatim
  } else {
    resp <- if (!grouped) paste0("`", outcome, "`") else if (identical(family, "rr"))
      "`.gb_succ`" else "cbind(`.gb_succ`, `.gb_fail`)"
    rhs  <- paste0("`", predictors, "`", collapse = " + ")
    if (!is.null(cross)) rhs <- paste0("(", rhs, ") * `", cross, "`")   # z8: the pooled interaction fit
    # z15: extra terms LAST, so the fit's own term.labels end with them (the Linearity scope).
    if (length(add_terms)) rhs <- paste(c(rhs, add_terms), collapse = " + ")
    fml_lpm <- stats::as.formula(
      paste0(if (grouped) "`.gb_prop`" else resp, " ~ ", rhs))
    # A Poisson likelihood has no two-column response: the grouped modified Poisson models the
    # success count with log(trials) as OFFSET, which is what keeps exp(coef) a per-item risk ratio
    # and the intercept a per-item risk rather than an expected count. An offset() is not a
    # term.label, so it does not disturb the ordering the line above protects.
    if (grouped && identical(family, "rr")) rhs <- paste0(rhs, " + offset(log(`.gb_trials`))")
    stats::as.formula(paste0(resp, " ~ ", rhs))
  }
  if (is.null(fml_lpm)) fml_lpm <- fml

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
    # The identity link needs sensible starting values (the default eta = 0 puts a fitted probability
    # outside the parameter space at once), and can still fail. Start from the OLS fit, and on
    # failure BE the OLS fit -- the linear probability model.
    # WARNING: the fallback TARGETS the same risk difference; it is not the same ESTIMATOR. The two
    # coincide only where the model holds, so the message must name which one ran -- the footer would
    # otherwise credit an identity-link GLM for numbers OLS produced.
    des0  <- make_design(mdata)
    start <- tryCatch(stats::coef(stats::lm(fml_lpm, data = mdata)), error = function(e) NULL)
    fit   <- tryCatch(
      do.call(survey::svyglm, list(fml, design = des0, family = fam_obj, start = start)),
      error = function(e) NULL, warning = function(w) NULL)
    if (is.null(fit) || !isTRUE(fit$converged)) {
      cli::cli_inform(c("!" = paste0(
        "The identity-link risk-difference model did not converge for {.val {outcome}}; ",
        "fitting the {.strong linear probability model} instead. It estimates the same risk ",
        "difference, but is a different estimator: the two agree only where the model holds.")))
      fit <- do.call(survey::svyglm, list(fml_lpm, design = des0, family = stats::gaussian()))
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
  var_y <- if (!do_exp && family == "gaussian") stats::var(mdata[[outcome]]) else NA_real_

  list(tidy = td, nobs = nrow(mdata), var_y = var_y, positive_level = positive_level, fit = fit,
       data = mdata)
}

# Align one fit to the union skeleton -> a single fmt column (length = nrow(skeleton)), in the
# additive (beta) or multiplicative (OR/IRR) shape. Reference LEVELS of predictors present in this
# model get the neutral value (0 / 1, no CI/p); predictors ABSENT from this model stay NA (empty
# cells); the Constant carries the intercept (baseline) estimate.
reg_column <- function(skeleton, fit_res, model_predictors, col_var, est,
                       color, color_signif, model_family = "", method = "wald", trials = NA) {
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
  scale_key    <- reg_scale_of(est, trials)
  est_field    <- EST_SCALES[[scale_key]]$est_field
  # every value cell shows THE ESTIMATE: `est` is the scale-relative token, so the builder names no
  # family-specific one and the table's `display` (or its default) decides what joins it.
  disp         <- "est"
  digits       <- reg_cell_digits(scale_key)
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
  if (identical(effect_shape, "ratio")) args <- c(args, list(ref = "1"))
  args <- c(args, list(pct_type = reg_pct_type(scale_key)))
  # var(Y) standardizes beta/SD(Y) for colour -- only the scales that declare `sd_from = "var"`
  if (identical(EST_SCALES[[scale_key]]$sd_from %||% "", "var"))
    args <- c(args, list(var = rep(fit_res$var_y, n_rows)))
  do.call(fmt, args)
}

# `display` on a regression table IS tab()'s display: one preset table, one resolver, one grammar
# (R/tab-display.R). NULL means "each cell keeps the token the builder gave it", which is the default.
#
# THE RULE the templates obey: a template may ask for an AUXILIARY quantity of the SAME fit -- an
# adjusted prediction, a marginal effect beside an odds ratio, which is what reg_marginal() supplies
# here -- but never for a different fit or a different estimand. That is what keeps `measure` the one
# estimand argument while `display` stays free.
#' @keywords internal
#' @noRd
reg_resolve_display <- function(display) display_resolve(display)

# reg_fill_base() -- THE ADJUSTED PREDICTION and the ADDITIVE MARGINAL EFFECT, on every model
# column, whether or not the cell prints them.
#
# WHY ALWAYS. `display` is a post-hoc property: choosing what a cell shows may never trigger a
# computation nor change a number, or `set_display()` on a built table would be a lie and jamovi's
# repaint would need a refit. Both quantities come from ONE point-estimate g-computation sweep
# (`want_se = FALSE`: no delta method, no jacobian, no dependency), measured at ~0.1 s -- inside the
# noise of the fit itself. The marginal builder has usually run that sweep already and passes it in.
#
# WHERE THEY LAND. The prediction goes in the field the column's own scale names for a LEVEL
# (`EST_SCALES$base_display`, what `{base}` renders); the marginal effect in `diff`.
# WARNING: neither may write into the column's OWN estimate field -- that would replace the number
# the interval, the stars and the colour belong to. A risk-difference column's estimate IS in `diff`,
# a percentage-level scale's IS `pct`: those two arms simply do not fire.
#' @keywords internal
#' @noRd
reg_fill_base <- function(col, marg, skeleton, model_predictors, group = NULL) {
  if (is.null(marg)) return(col)
  n_rows   <- nrow(skeleton)
  est_fld  <- fmt_center_field(col)
  base_fld <- fmt_scale_row(col)$base_display %||% NA_character_
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_ref   <- skeleton$is_ref & skeleton$var != "Constant" & in_model
  # one outcome category's rows, for a per-category (multinomial / ordinal-marginal) column.
  # WARNING: where the sweep returns one value per outcome CATEGORY and the column belongs to none of
  # them -- an ordinal cumulative odds ratio, which spans every cut -- there is no single level to
  # print, so the fill is refused rather than picking a category at random.
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
  if (!is.na(base_fld) && !identical(base_fld, est_fld))
    col <- vctrs::`field<-`(col, base_fld, pred_v)
  if (!identical(est_fld, "diff")) {
    # DESIGN: a factor level's ADDITIVE marginal effect is derived from the two adjusted predictions
    # rather than taken from the sweep's own contrast. The two are the same number -- averaging
    # commutes with an additive contrast, so the average marginal effect IS the difference of the
    # standardised means -- but the derived form is reference-INVARIANT, which is what lets jamovi's
    # digest path re-reference a cached fit without refitting it. A numeric predictor has no
    # prediction to difference, so its slope comes from the sweep.
    v <- if (is.null(marg$ame)) rep(NA_real_, n_rows) else take(marg$ame, "ame")
    refi   <- which(skeleton$is_ref & in_model)
    ref_of <- pred_v[refi][match(as.character(skeleton$var),
                                 as.character(skeleton$var)[refi])]
    v <- ifelse(is.na(pred_v) | is.na(ref_of), v, pred_v - ref_of)
    v[is_ref] <- NA_real_                                # a reference level has no marginal effect
    col <- vctrs::`field<-`(col, "diff", v)
  }
  col
}

# reg_fill_sweep() -- the point-estimate g-computation sweep reg_fill_base() reads.
#
# WARNING: it calls the ANALYTIC engine directly, never reg_marginal(). These quantities are
# AUXILIARY -- what a cell may show beside its estimate -- so they are computed where they are free
# and simply absent where they are not. Going through reg_marginal() would fall back to
# `marginaleffects` when g-computation refuses (a survey multinomial, a compound formula), turning an
# optional annotation into a hard dependency and, worse, an abort on a model that package refuses.
#' @keywords internal
#' @noRd
reg_fill_sweep <- function(fit, data, predictors, conf_level, wt = NULL, multiplier = NULL)
  tryCatch(reg_marginal_gcomp(fit, data, predictors, conf_level, wt, want_pred = TRUE,
                              want_se = FALSE, multiplier = multiplier),
           error = function(e) NULL)

# reg_apply_display() -- write the table's resolved `display` template into ONE model column.
#
# A pure template writer: every field it can name is already stored (reg_fill_base), so this never
# computes and never changes a number. The per-cell rule is the crosstab's own (display_write_col):
# a cell takes the template only where every field it names exists, so the Constant row, an
# out-of-model predictor and a numeric predictor with no adjusted prediction keep their plain
# estimate instead of printing a void.
#' @keywords internal
#' @noRd
reg_apply_display <- function(col, display) {
  if (is.null(display)) return(col)
  display_write_col(col, display)$col
}

# reg_default_display() -- the cell layout a regression column takes when the user named none.
#
# With a crude COLUMN the two mirror each other -- the crude cell prints "({base}) {est}", the model
# cell "{est} ({base})" -- so the two ESTIMATES end up side by side, adjacent across the table, with
# the level each sits on on the outside. That order also mirrors the modelling itself: the observed
# level is what everything starts from, a crude measure is computed on it, the model re-estimates
# that measure all things being equal, and the adjusted level is inferred back from the model.
#
# Where the crude effect rides IN the model cell there is no column to mirror, so the cell keeps its
# bare estimate here and reg_set_obs() folds "{est} ({obs})" once `obs` exists. Without a companion
# at all there is nothing to compare across, and a column prints its estimate alone.
#' @keywords internal
#' @noRd
reg_default_display <- function(col, empirical) {
  if (!emp_on(empirical) || identical(empirical, "cell")) return(col)
  display_write_col(col, DISPLAY_PRESETS[[if (identical(get_role(col), "emp")) "base_est"
                                          else "est_base"]])$col
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

# reg_marginal() -- THE dispatcher between the two engines (Phase 20d). `engine` is the estimand row's
# own declaration, resolved by reg_marginal_engine(); the fast route returns NULL rather than a wrong
# number, and the fallback then runs for the WHOLE call, so one column always carries one convention.
# The basis-expansion guard is applied to whichever engine answered: both build the counterfactual by
# re-evaluating the formula, so both can be silently wrong on a poly() / ns() term.
reg_marginal <- function(fit, data, predictors, conf_level, wt = NULL,
                         at = "average", comparison = NULL, want_pred = TRUE,
                         multiplier = NULL, engine = "marginaleffects", want_se = TRUE) {
  do_exp <- !is.null(comparison) && comparison %in% c("lnor", "lnratioavg")
  out <- NULL
  # "lnor" is the MNL j-vs-rest contrast, which only ever comes with at = "reference".
  if (identical(engine, "gcomp") && identical(at, "average") && !identical(comparison, "lnor"))
    out <- reg_marginal_gcomp(fit, data, predictors, conf_level, wt, ratio = do_exp,
                              want_pred = want_pred, want_se = want_se, multiplier = multiplier)
  # THE fallback, and the only place `marginaleffects` is genuinely required: either the estimand's
  # engine named it (at_reference -- the upfront guard already checked), or gcomp refused this
  # particular fit. The second case is not knowable at the argument boundary, so it is checked here.
  if (is.null(out)) {
    if (!requireNamespace("marginaleffects", quietly = TRUE))
      reg_abort_marginaleffects("this contrast, which has no closed form on this model")
    out <- reg_marginal_me(fit, data, predictors, conf_level, wt, at = at, comparison = comparison,
                           want_pred = want_pred, multiplier = multiplier, want_se = want_se)
  }
  if (identical(at, "average")) reg_marginal_basis_warn(fit, data, predictors, multiplier,
                                                        out$ame, do_exp)
  out
}

# reg_marginal_gcomp() -- the ANALYTIC engine: one counterfactual sweep per (predictor, level) giving
# the estimate, the adjusted prediction and the delta-method interval, from R/reg-influence.R's
# reg_gcomp_maker() / reg_gcomp_cat_maker(). Returns reg_marginal_me()'s exact shape, or NULL if any
# piece refuses -- measured 0.40 s against 9.92 s for the marginaleffects route on 13 000 rows.
#' @keywords internal
reg_marginal_gcomp <- function(fit, data, predictors, conf_level, wt = NULL, ratio = FALSE,
                               want_pred = TRUE, want_se = TRUE, multiplier = NULL) {
  # A predictor absent from the model has no counterfactual to build (the compound-formula path):
  # refuse the whole call rather than return a zero effect the other engine would have errored on.
  tvars <- tryCatch(all.vars(stats::delete.response(stats::terms(fit))), error = function(e) NULL)
  if (is.null(tvars) || !all(predictors %in% tvars)) return(NULL)
  V <- if (want_se) tryCatch(stats::vcov(fit), error = function(e) NULL) else NULL
  if (want_se && (is.null(V) || !is.matrix(V))) return(NULL)
  per_cat <- inherits(fit, "multinom") || inherits(fit, "polr")
  g <- if (per_cat) reg_gcomp_cat_maker(fit, data, wt, ratio)
       else         reg_gcomp_maker(fit, data, wt, ratio)
  if (is.null(g)) return(NULL)
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  amel <- list(); predl <- list()
  for (v in predictors) {
    is_fac <- reg_is_factor_var(data[[v]])
    if (is_fac) {
      lv <- levels(forcats::fct_drop(as.factor(data[[v]])))
      if (length(lv) < 2L) return(NULL)
      # marginaleffects' own factor contrast set: every non-reference level against the first.
      cls <- lapply(lv[-1], function(l) list(level = l, at = l, ref = lv[[1]]))
    } else {
      k <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else 1
      if (!is.finite(k) || k == 0) k <- 1
      cls <- list(list(level = v, at = k, ref = 0))   # a k-unit FORWARD DIFFERENCE, as `variables=list(v=k)`
    }
    for (ct in cls) {
      p <- g(v, ct$at, ct$ref)
      if (is.null(p)) return(NULL)
      # The 3+ level producer answers for every outcome category at once, so its `est` / `G` / means
      # are K-long and `group` names them; a single-equation one is scalar with no group. That is the
      # ONLY difference between the two, hence one loop.
      grp <- if (per_cat) as.character(p$levels) else NA_character_
      se  <- if (per_cat) vapply(p$G, function(gj) reg_delta_se(gj, V), numeric(1))
             else         reg_delta_se(p$G, V)
      res <- reg_wald_finalize(p$est, ratio, se = se, crit = crit)
      amel[[length(amel) + 1L]] <- tibble::tibble(
        var = v, level = as.character(ct$level), group = grp,
        ame = res$estimate, ame_lo = res$conf.low, ame_hi = res$conf.high, ame_p = res$p.value)
      if (want_pred && is_fac) {
        add_pred <- function(l, val) predl[[length(predl) + 1L]] <<-
          tibble::tibble(var = v, level = l, group = grp, pred = val)
        add_pred(as.character(ct$level), p$mean1)
        if (identical(ct$level, lv[[2]])) add_pred(lv[[1]], p$mean0)  # the reference's own, once
      }
    }
  }
  list(ame = dplyr::bind_rows(amel), pred = dplyr::bind_rows(predl))
}

# reg_marginal_basis_warn() -- Phase 18z15's guard, hoisted out of the per-predictor loop so it runs
# once per call whichever engine answered.
#' @keywords internal
reg_marginal_basis_warn <- function(fit, data, predictors, multiplier, ame, ratio) {
  bv <- reg_basis_vars(fit, predictors)
  if (!length(bv) || is.null(ame) || !nrow(ame)) return(invisible(NULL))
  for (v in bv) {
    if (reg_is_factor_var(data[[v]])) next
    est <- ame$ame[ame$var == v]
    if (length(est) != 1L) next
    k <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else 1
    if (reg_marginal_basis_ok(fit, data, v, k, est, ratio)) next
    cli::cli_warn(c(
      "!" = paste0("The marginal effect of {.val {v}} is not trustworthy: it is fitted through a ",
                   "basis expansion ({.code poly()} / {.code ns()}), which the marginal-effects ",
                   "engine re-evaluates on perturbed data."),
      "i" = 'Fit it with {.code shape = c({v} = "quadratic")} instead of a formula basis.'))
  }
  invisible(NULL)
}

reg_marginal_me <- function(fit, data, predictors, conf_level, wt = NULL,
                            at = "average", comparison = NULL, want_pred = TRUE,
                            multiplier = NULL, want_se = TRUE) {
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
  # Phase 20d: the delta-method jacobian is one full re-prediction PER COEFFICIENT (measured 7x the
  # whole estimate), so it is not paid where the caller discards the interval -- reg_apply_display()'s
  # fold pokes `pct`/`diff` into a column that keeps its own CI.
  se_arg <- if (want_se) list() else list(vcov = FALSE)
  amelist <- purrr::map(predictors, function(v) {
    ac <- if (at == "reference")
      as.data.frame(do.call(marginaleffects::comparisons, c(
        list(fit, variables = var_arg(v), newdata = ref_grid, conf_level = conf_level),
        cmp_arg, se_arg)))
    else
      as.data.frame(do.call(marginaleffects::avg_comparisons, c(
        list(fit, variables = var_arg(v), newdata = data, conf_level = conf_level),
        wts_arg, cmp_arg, se_arg)))
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
    est <- ac$estimate
    # `vcov = FALSE` drops conf.low / conf.high / p.value from the result entirely.
    lo <- ac$conf.low %||% rep(NA_real_, length(est))
    hi <- ac$conf.high %||% rep(NA_real_, length(est))
    pv <- ac$p.value %||% rep(NA_real_, length(est))
    if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }   # log-ratio -> OR / RR (and its CI)
    tibble::tibble(var = v, level = as.character(level), group = grp,
                   ame = est, ame_lo = lo, ame_hi = hi, ame_p = pv)
  })
  ame <- dplyr::bind_rows(amelist)

  predlist <- if (want_pred) purrr::map(predictors, function(v) {
    if (!reg_is_factor_var(data[[v]])) return(NULL)      # no per-level prediction for numerics
    # Phase 20d: `vcov = FALSE` throughout -- only `$estimate` is ever read below, and the interval
    # marginaleffects would build for it costs one re-prediction per coefficient (measured 4x).
    ap <- if (at == "reference") {
      grid_v <- do.call(marginaleffects::datagrid, c(list(model = fit),
        utils::modifyList(ref_vals, stats::setNames(list(levels(as.factor(data[[v]]))), v))))
      as.data.frame(marginaleffects::predictions(fit, newdata = grid_v, vcov = FALSE))
    } else {
      # Change A (decisions doc S50): the adjusted % is the marginal-STANDARDIZED prediction --
      # `variables = v` sets v to each level for the WHOLE sample (keeping every other covariate as
      # observed) and averages = g-computation / direct standardization. This is the covariate-adjusted
      # quantity that COHERES with the AME (adjusted%(ref) + AME(level) == adjusted%(level)); `by = v`
      # would instead reproduce the estimation-sample OBSERVED rate (score-equation identity) and is not
      # adjusted. `by = v` would instead reproduce the estimation-sample OBSERVED rate.
      as.data.frame(do.call(marginaleffects::avg_predictions, c(
        list(fit, variables = v, newdata = data, vcov = FALSE), wts_arg)))
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
reg_marginal_column <- function(skeleton, marg, model_predictors, shape, var_y,
                                group, color, color_signif, col_var, or_tip = NULL,
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
  # in_refrow: the UNION-skeleton row fact (see reg_column) so an absent predictor keeps its bold in a
  # comparison; is_ref above stays in_model-gated for the value/display blanking below.
  refrows  <- (skeleton$is_ref & !is_const) | is_const

  # "blank" (not NA) for the Constant / out-of-model cells: an NA display falls back to get_n() in
  # get_num(), so it must be an explicit blank-token (renders "") rather than left unset.
  # every value cell shows THE ESTIMATE; `est` is the scale-relative token, so one line serves every
  # shape and the table's `display` (or its default) then decides what joins it. "blank" -- not NA --
  # for the Constant / out-of-model cells: an NA display falls back to get_n() in get_num().
  display <- rep("blank", n_rows)
  show    <- in_model & (!is.na(ame_v) | is_ref)
  if (shape == "prob") {
    display[show] <- "est"
    ame_v[is_ref] <- NA_real_                                  # reference has no marginal effect
    # Phase 14r (E): carry the model OR (coefficient path) in the `or` field so cond_or surfaces it on
    # hover though the cell DISPLAYS the AME. Read-only: the AME display / colour never read `or`, so it
    # is inert everywhere but the tooltip. NA on the reference (which shows "ref").
    or_v <- if (is.null(or_tip)) NA_real_ else or_tip
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): no misleading whole-model N (see the empirical cols)
      pct = pred_v, diff = ame_v, or = or_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      scale = "points", pct_type = reg_pct_type("points"), display = display, digits = reg_cell_digits("points"), ci_method = "wald",
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
    display[show] <- "est"
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): no misleading whole-model N (see the empirical cols)
      pct = pred_v, or = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      scale = "odds_ratio", pct_type = reg_pct_type("odds_ratio"), display = display, digits = reg_cell_digits("odds_ratio"), ref = "1",
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
    display[show] <- "est"
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    fmt(
      n = rep(NA_integer_, n_rows),
      ratio = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      scale = "mean_ratio", display = display, digits = reg_cell_digits("mean_ratio"), ref = "1", ci_method = "wald_log",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  } else if (shape == "or") {                                  # MNL "j vs rest" OR at the profile
    display[show] <- "est"
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): no misleading whole-model N (see the empirical cols)
      or = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      scale = "odds_ratio", pct_type = reg_pct_type("odds_ratio"), display = display, digits = reg_cell_digits("odds_ratio"), ref = "1",
      ci_method = "wald_log",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  } else {                                                     # "raw" (gaussian / poisson)
    display[show] <- "est"
    ame_v[is_ref] <- 0                                         # additive neutral at the reference
    fmt(
      n = rep(NA_integer_, n_rows),   # Phase 14r (D): no misleading whole-model N (see the empirical cols)
      diff = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      var = rep(var_y, n_rows),                               # var(Y): standardizes the effect-size colour
      # a marginal effect on the OUTCOME's scale (a gaussian AME, a poisson COUNT AME) -- never a
      # link-scale coefficient, whatever the family, which is exactly what used to make this column
      # and the raw coefficient beside it indistinguishable except through the `var` field.
      scale = "raw_diff", display = display, digits = reg_cell_digits("raw_diff"), ci_method = "wald",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  }
}

# Split ONE multinomial fit into one OR column per non-reference outcome category. Each category's
# tidy rows (`y.level == j`, y.level dropped) look like a standard glm tidy, so reg_column() aligns
# them to the shared predictor skeleton unchanged. Label = "<j> vs <ref>: OR" (prefixed by the
# outcome when several outcomes / models coexist, to disambiguate). Returns a list of {label, col}.
reg_columns_multinom <- function(skeleton, f, sp, est, color, color_signif,
                                 cleannames, prefix_dep, model_family = "multinomial",
                                 method = "wald") {
  y_ref <- reg_cleanup(f$y_ref, cleannames)
  purrr::map(f$y_levels, function(j) {
    sub      <- f
    sub$tidy <- f$tidy[f$tidy$y.level == j,
                       setdiff(names(f$tidy), "y.level"), drop = FALSE]
    jc  <- reg_cleanup(j, cleannames)
    lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "",
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
  # Phase 19m-i: the design-based Wald-vs-null, written ONCE. It was byte-identical in both branches
  # below, ten lines apart inside this same function. A fit with no `terms` component (svy_vglm, the
  # weighted MNL) yields no row at all -- the footer degrades to `n`, which is the intended answer.
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

  # Phase 18z3: a modified-Poisson ("rr") fit is a QUASI-likelihood on a deliberately misspecified
  # variance, so AIC / BIC / McFadden are not defined, and the Pearson dispersion of a 0/1 outcome is
  # just mean(1-mu) -- a constant of the fitted values, never a diagnostic. Report the honest pair:
  # n + the design-based Wald-vs-null. Placed FIRST so it holds weighted or not (the fit is an svyglm
  # either way); the weighted branch below keeps its Nagelkerke/AIC set for genuine survey models.
  if (family %in% REG_FIT_ONLY_FAMILIES) {
    out <- dplyr::bind_rows(out, wald_null_row(fit))
    return(out)
  }

  if (weighted) {
    # svyglm: no true likelihood -> Rao-Scott Wald-vs-null (relabelled) + Nagelkerke pseudo-R2 + AIC.
    # survey's psrsq / AIC emit "rsquared may be wrong" / "zero weight" notes under scaled weights; these
    # are inherent approximations of a survey summary, not user-actionable -> suppressed (the footer is a
    # descriptive summary, not the primary design-based inference).
    # svy_vglm (weighted MNL) has no terms component -> the Wald-vs-null degrades away (footer = n only).
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
# Phase 20c: DERIVED from TEST_ROWS -- the `glance` block, i.e. the rows reg_glance() emits, one per
# model column. ⚠ its ORDER is TEST_ROWS' (the display order); the old hand-written order differed,
# and its only reader is the "Available: ..." message below.
#' @keywords internal
REG_GOF_KEYS <- .trow_keys(.trow_chr("block") == "glance")

# ⚠ THE UNION IS MANDATORY. `unique(TEST_ROWS$stat)` alone would silently DROP `residuals` and
# `normality`: both are legal `check =` values with a panel and, deliberately, no test row at all
# (REG_CHECKS' "taught, never scored" block), so they have no `stat` here to be derived from.
#' @keywords internal
reg_stat_keys <- function() unique(c(.trow_chr("stat")[.trow_chr("producer") == "reg"],
                                     names(REG_CHECKS)))

# --- Phase 20c: `stats =` is ONE argument -------------------------------------------------------
# `stats` was three arguments for one concept -- WHAT RIDES THE MODEL-SUMMARY FOOTER. `compare` said
# which comparison, `baseline` said against which model, and neither could be expressed in the
# vocabulary `stats` already had. They are two more TEST_ROWS keys now, and the baseline model is the
# ONE parameter its key carries, written as a named entry:
#
#   stats = c("n", "aic", "compare_sequential")     each model vs the previous one
#   stats = "compare_baseline"                      each model vs the FIRST
#   stats = c("n", compare_baseline = "M1")         ... vs the model labelled "M1"
#   stats = c("n", compare_baseline = 2)            ... vs the 2nd column
#
# so a `stats` element is always a KEY -- carried in the name when it has a parameter, in the value
# when it does not. That is `ref = c(var = "level")`'s grammar, one subsystem over.
#
# reg_stats_keys_of() is what makes the two readings one: the boundary validates KEYS, the resolver
# splits them. Everything downstream (reg_footer_stats, reg_compare_rows) still receives the plain
# triple, so no producer changed.

# The KEYS a `stats` vector names: the name where there is one, the value otherwise.
#' @keywords internal
#' @noRd
reg_stats_keys_of <- function(stats) {
  if (!is.character(stats) || !length(stats)) return(character(0))
  nm <- names(stats)
  if (is.null(nm)) unname(stats) else ifelse(nzchar(nm), nm, unname(stats))
}

# THE resolver: a user's `stats` -> list(stats, compare, baseline), the triple every producer below
# already speaks. Returns `stats` with the comparison keys REMOVED (reg_footer_stats() would
# otherwise read one as a goodness-of-fit discriminator and emit an empty row).
#' @keywords internal
#' @noRd
reg_resolve_stats <- function(stats) {
  none <- list(stats = stats, compare = "none", baseline = NULL)
  # ⚠ FALSE / "none" now hides the comparison too, which `compare =` (applied unconditionally) did
  # not. One argument means one list of what the footer shows; NEWS.md records the change.
  if (!is.character(stats) || !length(stats) ||
      identical(stats, "all") || identical(stats, "none")) return(none)

  keys  <- reg_stats_keys_of(stats)
  is_cmp <- keys %in% c("compare_baseline", "compare_sequential")
  if (!any(is_cmp)) return(none)
  cmp <- keys[is_cmp]
  if (length(cmp) > 1L)
    cli::cli_abort(c("{.arg stats} names more than one model comparison: {.val {cmp}}.",
                     "i" = "A footer row compares each model to ONE other, so pick one.",
                     call = NULL))

  # The baseline model, if the entry was written as a named one. An empty or NA value means the same
  # as an unnamed key ("the first model") -- this is the one place that can rewrite it, so the shape
  # is settled here rather than refused at a boundary that could not fix it.
  val <- unname(stats[is_cmp])
  bl  <- if (nzchar(names(stats)[is_cmp] %||% "") && !is.na(val) && nzchar(val)) val else NULL
  if (identical(cmp, "compare_sequential") && !is.null(bl))
    cli::cli_abort(c('{.code stats = c(compare_sequential = {.val {bl}})} names a baseline model.',
                     "x" = "A sequential comparison has none: each model is tested against the previous one.",
                     "i" = 'Did you mean {.code stats = c(compare_baseline = "{bl}")}?'), call = NULL)
  # a numeric-looking baseline is a COLUMN POSITION: c(compare_baseline = 2) coerces to "2" in a
  # character vector, and reg_compare_rows() matches a label first, a position second.
  if (!is.null(bl) && grepl("^[0-9]+$", bl)) bl <- as.numeric(bl)

  # ⚠ A comparison key RESTRICTS NOTHING. `stats = "compare_baseline"` asks for a comparison, not for
  # a footer with only that in it -- so when the comparison keys are all that was named, what is left
  # is NULL = the per-family default set, exactly as if `stats` had not been given. Restricting the
  # goodness-of-fit rows is still `stats = c("n", "aic", "compare_baseline")`, and hiding the whole
  # footer (the comparison included) is still `stats = FALSE` / `"none"`.
  rest <- unname(stats[!is_cmp])
  list(stats    = if (!length(rest)) NULL else rest,
       compare  = if (identical(cmp, "compare_sequential")) "sequential" else "baseline",
       baseline = bl)
}

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
# sigma; weighted -> n/wald_null/nagelkerke_r2/aic; plus `global` and the free checks.
# NULL / TRUE = that default set; "all" = every statistic AND every check, fit-based ones included;
# FALSE / "none" = no footer; a character vector overrides (keeping its order, valid names only).
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
  # Phase 18z15 put the five model CHECKS there too; Phase 20f keeps the three that cost nothing and
  # makes the two that fit a model opt-in (REG_CHECKS$cost). `stats` already IS the footer
  # vocabulary, so each is individually addable and `stats = FALSE` still hides everything.
  checks  <- reg_checks_for(family, weighted)
  default <- c(default, "global", reg_checks_default(family, weighted))
  # "all" MEANS ALL (Phase 20f). It used to be a synonym of NULL, i.e. of the default set -- already
  # a misnomer, and one that D4 would have made worse: it is now the one value a user has to
  # remember to see every statistic and every check this family allows, fit-based ones included.
  if (identical(stats, "all")) return(reg_check_expand(unique(c(default, checks))))
  if (is.null(stats) || isTRUE(stats)) return(reg_check_expand(default))
  if (isFALSE(stats) || identical(stats, "none")) return(character(0))
  # A user writes a check KEY ("linearity"); a `test` row carries a discriminator ("linearity_lr").
  reg_check_expand(stats[stats %in% reg_stat_keys()])
}

# The model-summary rows of ONE fit, in new_test_tibble() schema. `col_var` is the fmt column this
# fit is keyed under (MNL/ordinal -> its first category column); `grouped` marks a grouped-binomial
# fit (dispersion). Phase 15e: the stat SET comes from THIS fit's own family, so a mixed-family
# table gets each outcome's (gaussian R2 / logit McFadden) and test_grid_reg unions them.
# Phase 20f-iii: per SPEC. It was vectorised over `fits` with four parallel per-fit vectors, and its
# one caller now builds a model at a time -- reg_spec_build() holds every one of those facts as a
# scalar, and reg_stage_footer() concatenates. NULL = this fit contributes no row.
reg_gof_rows <- function(f, sp, col_var, weighted, grouped, stats) {
  keep <- reg_footer_stats(sp$fit_family, weighted, isTRUE(grouped), stats)
  if (length(keep) == 0) return(NULL)                        # stats = FALSE -> no glance, no warnings
  # Phase 15b: the reref fast path carries the reference-invariant glance in `f$glance` (the raw fit
  # was discarded); a real reg_fit() result has no `$glance` -> compute from `f$fit` as before.
  g <- if (!is.null(f$glance)) f$glance
       else reg_glance(f$fit, sp$fit_family, isTRUE(grouped), weighted, f$nobs)
  g <- g[g$test %in% keep, , drop = FALSE]
  g <- g[order(match(g$test, keep)), , drop = FALSE]           # spec order
  if (nrow(g) == 0) return(NULL)
  reg_test_row(g$test, col_var, statistic = g$statistic, df1 = g$df1, df2 = g$df2,
               pvalue = g$pvalue, nobs = as.numeric(f$nobs), outcome = sp$outcome)
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
                             compare = "none", baseline = NULL) {
  if (identical(compare, "none")) return(reg_gof)
  n <- length(fits)
  if (n < 2L) {
    cli::cli_inform(c("i" = paste0("{.arg compare} needs at least two models (a {.arg predictors} list ",
                                   "or several outcomes); ignored.")))
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

  # a model COMPARISON is single-outcome (guarded at the boundary), so every row is about it
  cmp_outcome <- specs[[1]]$outcome
  row <- function(test, col_var, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_,
                  pvalue = NA_real_, nobs = NA_real_)
    reg_test_row(test, col_var, statistic = statistic, df1 = df1, df2 = df2,
                 pvalue = pvalue, nobs = nobs, outcome = cmp_outcome)

  # Phase 20c: the discriminator is LOOKED UP, not built. `stat_key` is the word the user typed in
  # `stats =` ("compare_baseline" / "compare_sequential") and `method` is the instrument that fired;
  # TEST_ROWS asserts that pair unique, so test_row_key() is total. The old `tag` + four paste0()s
  # were the package's last generated discriminators, hand-enumerated a second time in the footer
  # spec -- adding a fifth instrument meant editing two files that could not check each other.
  stat_key <- if (compare == "sequential") "compare_sequential" else "compare_baseline"
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
    cli::cli_inform(c(
      "i" = paste0(
        "Column {.val {col}}: models are not nested or N differs -> showing the AIC difference vs the ",
        "{if (compare == 'sequential') 'previous' else 'baseline'} model instead of a likelihood-ratio test."),
      # Phase 19a (D5): the advice named `na = "drop_all_models"`, a value REMOVED in z13 -- so a user
      # following it hit match.arg()'s "should be one of" error. The `na` family is
      # drop_by_outcome / drop_by_model / drop_all.
      "i" = 'A different N is usually the per-model missing-value drop; set {.code na = "drop_all"} to fit every model on the same complete cases so the likelihood-ratio test can run.'))
    row(test_row_key(stat_key, "aic"), col, statistic = daic, nobs = fits[[i]]$nobs)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}


# --- Phase 18z8: the aggregated effect-modification test (predictor x tab_vars) -----------------
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
                         pvalue = NA_real_, nobs = NA_real_, outcome = NA_character_)
  # Phase 19m-ii: `dep` -- which outcome this row is about. Every reg row is per-fit or
  # per-(fit, predictor), and a fit IS a spec, so the caller always has it. It replaces the length
  # coincidence test_grid_reg() used to pair `meta$outcome` against `unique(test$col)` with.
  tibble::tibble(var = var, col = col, test = test, statistic = statistic,
                 df1 = df1, df2 = df2, pvalue = pvalue, n = nobs, min_e = NA_real_, outcome = outcome)

# Phase 20c: DERIVED from TEST_ROWS (`stat == "interaction"`), like every other discriminator block.
#' @keywords internal
reg_interaction_types <- function() unname(test_row_types("interaction"))

# ⚠ THE FOURTH FITTING SITE, and the one that cannot join a per-spec product (Phase 20f-iiii, stated
# here so the next reader does not re-derive it). It fits the POOLED model -- every tab_vars group at
# once, with the group interacted -- so it lives AFTER the split barrier in reg_stage_split(), where
# the groups' own reg_build() calls have already returned. A per-spec builder runs inside ONE group
# and can never see the others, which is why this is not a missed parallel axis but a different
# question: reg_specs_independent() is about models of ONE table, this is about a test across tables.
#' @keywords internal
reg_interaction_rows <- function(reg_gof, data, specs, shared, tab_vars, fit_first_col) {
  weighted <- shared$weighted
  rows <- purrr::map(seq_along(specs), function(i) {
    sp <- specs[[i]]
    # No pooled interaction for the engines that are not a single glm/svyglm equation (multinomial /
    # ordinal have their own fitters), nor for the compound-formula escape hatch (the interaction of an
    # arbitrary formula is ill-defined). Degrade to no row, never to a wrong one.
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
    # WARNING: take the interaction terms from the FIT's own term.labels, verbatim -- never rebuild
    # them. terms() orders the parts of an interaction by the variable's position in the formula, so a
    # hand-built "age:party3" comes back as "party3:age" and drop1() then rejects the scope. Both
    # drop1() and regTermTest() accept the labels as a CHARACTER vector, which skips the re-parse.
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
                   types = test_row_types("interaction"),
                   col_var = fit_first_col[[i]], nobs = f$nobs, outcome = sp$outcome)
  })
  rows <- purrr::compact(purrr::flatten(purrr::compact(rows)))
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}

# THE per-predictor term test (Phase 18z13). Two callers ask different QUESTIONS of the same
# computation, so it lives once: the aggregated interaction test (predictor x tab_vars, on a pooled
# fit) and the per-predictor global test (the predictor's own term, on the model's own fit). They
# differ only in which fit and which terms are dropped -- everything below (the Wald / F / LR ladder,
# the drop1 column-name handling, the row shape) was identical.
#
# WARNING: `terms` must come from the FIT's own term.labels, verbatim. terms() orders the parts of an
# interaction by the variable's position in the formula, so a hand-built "age:party3" comes back as
# "party3:age" and drop1() then rejects the scope. Both drop1() and regTermTest() take the labels as a
# CHARACTER vector, which skips the re-parse.
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
        p, nobs, outcome = outcome)
  })
}

# Phase 18z13 (SS7.2): the per-predictor GLOBAL test -- "is this variable associated with the
# outcome at all?", the one answer a block of 7 stars against a reference category cannot give, and
# the item a gtsummary user reaches for first (`add_global_p`). tabxplor's audience is almost entirely
# multi-level categorical predictors, so it is in the DEFAULT stats set.
#
# Emitted only for terms carrying 2+ coefficients: a 1-df term's global p IS the single cell's p,
# already starred, so a line for it would be noise.
#
# ⚠ IT DOES REFIT, and that is a DECLARED KEEP (Phase 20f-iiii, from 20f's routed item). The design
# arm is regTermTest() on the stored vcov and refits nothing; the unweighted arm is drop1(), which
# fits one reduced model per multi-coefficient term (~2 s at n = 200 000). The only cheaper route is
# a Wald test -- which is a DIFFERENT NUMBER, not a faster way to the same one, and this is a TEST a
# reader will quote, not a diagnostic. 20f's own precedent stands behind that: substituting anova()
# for drop1() moved a quasipoisson F from 14.25 to 12.47. Unlike the model CHECKS (REG_CHECKS$cost),
# it therefore stays in the default `stats` set and pays the fit.
# Phase 20c: DERIVED from TEST_ROWS (`stat == "global"`).
#' @keywords internal
reg_global_types <- function() unname(test_row_types("global"))

# `shared` is new_reg_shared()'s record (Phase 20e: it used to be a hand-written subset of it).
# Phase 20f-iii: per SPEC, like its two siblings -- reg_spec_build() calls it with the fit in hand
# and reg_stage_footer() concatenates. NULL = this model contributes no global test.
#' @keywords internal
reg_global_rows <- function(f, sp, shared, col_var) {
  if (!reg_fam_glm(sp$fit_family) || isTRUE(sp$compound)) return(NULL)
  if (is.null(f) || is.null(f$fit)) return(NULL)              # the jamovi digest path keeps no fit
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
  rows <- purrr::compact(reg_term_tests(fit, terms_i, terms_i,
                                        use_f = reg_fam_disp_estimated(sp$fit_family),
                                        use_wald = reg_fam_svy_fitted(sp$fit_family, shared$weighted),
                                        types = test_row_types("global"),
                                        col_var = col_var, nobs = f$nobs, outcome = sp$outcome))
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
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
reg_build_digest <- function(data, sp, family, design_spec, do_exp, outcome_level,
                             conf_level, weighted, multiplier = NULL) {
  f   <- reg_fit(data, sp$outcome, sp$predictors, family, design_spec, do_exp,
                 outcome_level, conf_level, method = "wald",
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
  # DESIGN: the adjusted predictions and the numeric slopes travel WITH the digest, computed here
  # while the fitted object exists. They are a counterfactual sweep over the model, not a
  # reparametrization of its coefficients, so they cannot be recovered from `coef` later -- and the
  # fit itself is far too big to cache. They ARE reference-invariant (relevelling a factor leaves
  # every fitted value identical to machine precision, measured), which is what keeps a reference
  # change a cache HIT.
  # WARNING: they are NOT multiplier-invariant -- a k-unit contrast on a non-identity link is not k
  # times the one-unit one -- so `multiplier` is part of the digest KEY (unlike the coefficients,
  # which reg_reref_fit_res() rescales exactly). A scaling edit therefore refits; a reference or
  # display toggle, the live-UI cases this path exists for, does not.
  marg <- reg_fill_sweep(fit, f$data, sp$predictors, conf_level, design_spec$wt, multiplier)
  list(coef = coef_v, vcov = V, df_residual = stats::df.residual(fit),
       phi = phi, scaled = scaled, disp_known = disp_known, do_exp = do_exp,
       var_y = f$var_y, positive_level = f$positive_level, nobs = f$nobs, marg = marg,
       glance = reg_glance(fit, family, grouped, weighted, f$nobs), family = family)
}

# Reparametrize a canonical digest to the DISPLAY reference encoded in `skeleton` (built on the
# releveled data). Each display term is a linear contrast L over the canonical coefficients: a factor
# level j vs the display reference r is L = e_{p j} - e_{p r} (a canonical term absent = the canonical
# first level = a 0 column); the intercept at the display profile is e_0 + sum_p e_{p r_p}; a numeric
# predictor is the identity. estimate = L'b, se = sqrt(L' V L); then the SAME Wald finalize reg_fit()
# uses (phi scaling, z/t crit, p as the CI's dual, exp) -> byte-identical to a real refit-at-r.
reg_reref_fit_res <- function(digest, skeleton, conf_level, multiplier = NULL) {
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
       # the adjusted-prediction sweep travels on; the fitted object itself does not exist here
       glance = digest$glance, fit = NULL, data = NULL, marg = digest$marg)
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
  # shared with fmt_gap_parts() and the crude numeric overlay (a rate-ratio column keeps
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
# list of list(outcome, predictors, label, trials, formula, compound). The data-skeleton (union of
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
# `tab_vars` stays a formal of reg_build(): it flips to NULL in the split recursion, and a NULL
# value cannot live in a list that must round-trip through modifyList().
# Phase 19e: `effect` and `at` LEFT the record. The estimand is a per-SPEC fact (`sp$est`, the
# R/reg-estimand.R row), so a table-scalar copy of half of it could only ever disagree with the
# columns it described -- and it did: the marginal builder was chosen by that scalar even after
# Phase 15e made the family per spec.
#' @keywords internal
new_reg_shared <- function(union_predictors = character(0), design_spec = list(), weighted = FALSE,
                           outcome_level = NULL, conf_level = conf_level_default(),
                           method = "wald", color_signif = "grey_non_signif", cleannames = TRUE,
                           subtext = "",
                           stats = NULL, compare = "none", baseline = NULL,
                           multiplier = NULL, multiplier_label = NULL,
                           shape_terms = NULL, shape_labels = NULL,
                           empirical = FALSE, display = NULL,
                           var_labels = character(0), na_shared_vars = character(0),
                           add_n = FALSE) {
  as.list(environment())
}
# ...and THE globalVariables mirror, derived from those formals: reg_build() binds them with
# list2env(), which codetools cannot see. It lived in R/fmt_class.R as a hand-kept copy and had
# fallen behind twice.
utils::globalVariables(names(formals(new_reg_shared)))

# Phase 19m-ii: THE typed record of ONE fitted model of a tab_reg() call -- `new_reg_shared()`'s
# per-model sibling, and its same idiom (the FORMALS are the contract, the body is
# as.list(environment()), the globalVariables mirror is derived beneath). It was two hand-written
# 14-field `list()` literals inside tab_reg(), one per branch, that differed in exactly THREE things
# -- which model or which outcome varies, the label, and the union of predictors -- and were
# otherwise the same record with every `*_for(d)` closure replaced by its `outcome[[1]]` scalar.
#
# `fit_family` is exactly that: the internal LINK key `est$fit` carries ("rr" / "rd" / "mr"
# included), NOT the outcome family, which is `est$family`. It was called `family`, one word away
# from reg_call's `families` (the OUTCOME families) and from `sp$est$family` (the same) -- a name
# that invited a guess about which of the two it was, in a phase whose rule 2 is "never guess". `crude_key` STAYS a stored field: it is a 5-branch cascade over
# (fit family, trials, compound) read in six places, computed once at construction (the z10 ruling
# above reg_crude_key()). What LEFT are the three that were only other names for `est`:
#   effect_shape -- ZERO readers; reg_column() recomputes the identical expression from est$exp.
#   do_exp       -- exactly `isTRUE(est$exp)`, and its 5 readers all forward it straight to reg_fit().
#   eff_word     -- `reg_eff_word(est, empirical)`, and its 2 readers are inside reg_build(), where
#                   `empirical` is FINAL. Deriving it there is strictly better than storing it: the
#                   header word can no longer disagree with the table's own shared$empirical, which
#                   is what the eager-vs-lazy pair in tab_reg() could do.
#' @keywords internal
new_reg_spec <- function(outcome = character(0), predictors = character(0), label = "",
                         fit_family = "", trials = NULL, outcome_level = NA_character_,
                         compound = FALSE, formula = NULL,
                         color = NA_character_, est = NULL, crude_key = NA_character_) {
  # `outcome` arrives NAMED on the comparison branch (it is the call's own vector, length 1) and
  # unnamed on the per-outcome one; every downstream map_chr(specs, "outcome") compares it to a
  # bare column name.
  outcome <- unname(outcome)
  as.list(environment())
}
utils::globalVariables(names(formals(new_reg_spec)))

reg_inference <- function(shared, degraded = FALSE) {
  ds <- shared$design_spec
  leaf_inference(new_inference(ds$wt, ds, force = TRUE), degraded = degraded)
}


# === reg_build(): THE STAGED BUILD (Phase 20e, KEY 6) ===========================================
#
# WHICH STAGE PRODUCED WHICH PART OF THE TABLE. reg_build() was 726 lines and eleven unnamed phases,
# where tab_build() has had a typed ctx and six named stages since 17e/19i. It is now seven stages,
# each named after the part of the table it produces, over ONE typed context.
#
# Phase 20f-iii: the per-MODEL half of those stages is ONE declared product now
# (reg_spec_build(), R/reg-spec-build.R). Six stages used to carry their own `map(specs, ...)`, so
# "which parts of the table are per-model and which are between-models" could only be answered by
# reading four files; the stages above the loop are cross-spec ASSEMBLERS, and the loop itself is
# dispatchable (`parallel`).
#
# ⚠ THE STAGE ORDER IS THE SOURCE ORDER, and it is load-bearing. Every model fit -- the reported
# ones, the linearity refits, the crude univariable ones -- happens inside reg_stage_specs(), and a
# fit may inform or warn, so the message stream is part of the output. It is SPEC-major since
# 20f-iii (one model's diagnostics arrive together); dev/verify_reg_specs.R compares it in order,
# and that reordering is the phase's one declared delta.
#
# `new_reg_ctx()`'s formals are the contract, `reg_ctx_locals()` is its projection into a stage's
# scope. See both, immediately below.

# new_reg_ctx() -- THE typed context of one regression build (Phase 20e). The idiom is new_ctx()'s
# (R/tab.R), and its lesson: a stage product is DECLARED, never left to appear. An undeclared key is
# simply ABSENT, and list2env() creates no binding for an absent key -- so its own is.null() guard
# does not return TRUE, it ERRORS. Declaring costs one line each and makes the ctx self-describing:
# a reader can see what a stage may find without running the build.
#
# DESIGN: `shared` (new_reg_shared()'s record) stays ONE nested element and is PROJECTED into bare
# names at each stage head -- it is never flattened into the ctx. Flattening would give every
# per-call setting two carriers, which is exactly what 19i had to undo for tab()'s settings spine,
# and three consumers (reg_inference(), reg_interaction_rows(), the split recursion) need the record
# whole anyway. A build-time assert in R/zzz-fact-keys.R keeps the two name sets DISJOINT, so the
# projection can never shadow a stage product.
#' @keywords internal
#' @noRd
new_reg_ctx <- function(
    # --- INPUTS: reg_build()'s own formals ------------------------------------------------------
    # ⚠ `skeleton_data` is FORCED here, i.e. before reg_stage_fit() may relevel `data` on the jamovi
    # reref path. That is what the argument means (the FULL data, so every split group shares one
    # skeleton); its lazy default used to be forced later, so on that one path it silently became
    # the POST-relevel frame. The two coincide for both of its consumers -- a factor relevel moves
    # no predictor between reg_numeric_preds()/reg_factor_preds(), and reg_curves() reads only the
    # numeric predictors and the outcome -- so this is a contract fix, not a number change.
    # ⚠ `fit_cache` is NOT `.fit_cache`: new_reg_shared()'s `as.list(environment())` idiom defaults to
    # all.names = FALSE, so a dot-prefixed key would be SILENTLY DROPPED from the record -- the jamovi
    # cache would simply stop being threaded, with no error until a stage read it. No ctx key may
    # start with a dot; reg_build()'s own formal keeps its `.` (it is an internal argument of the
    # entry point, tab()'s `.cache` convention).
    data = NULL, specs = list(), shared = list(), tab_vars = NULL, fit_cache = NULL,
    ref = NULL, reref = FALSE, skeleton_data = NULL, parallel = NULL,
    # --- reg_stage_setup: the skeleton, the table's SHAPE facts and the per-spec PLAN ------------
    # ⚠ `data` is REWRITTEN by this stage on the reref path (the reference relevel) and is read
    # afterwards by add_n, reg_emp_frame(), reg_empirical_fit() and reg_check_rows(): it is a
    # declared product of reg_stage_setup() as well as an input. `data_canon` is the PRE-relevel
    # frame the jamovi digest is fitted on (NULL off that path -- never a second copy of `data`).
    family = NA_character_, skeleton = NULL, skeleton_deferred = FALSE, data_canon = NULL,
    compound = logical(0), builders = character(0),
    prefix_dep = FALSE, n_outcomes = 0L, is_comparison = FALSE,
    numeric_preds = character(0), factor_preds = character(0),
    # `spec_plan` (want_n / n_names / want_emp / want_crude / num_preds) is what the builder must be
    # TOLD rather than work out: the de-duplications a worker cannot reproduce (it does not know
    # what another unit built) and the one predictor set whose rule is table-scalar. `want_global`
    # is the other table-scalar gate it reads.
    spec_plan = list(), want_global = TRUE,
    # --- reg_stage_crude: the ONE observed block of a one-outcome table ---------------------------
    # NULL when the table has several outcomes (each spec builds its own) or no crude companion.
    crude = NULL,
    # --- reg_stage_specs: one new_reg_spec_product() per spec, and the column LAYOUT -------------
    # `built` is the flattened VIEW of the products' `cols`, in order (`built_per_fit`'s old role).
    products = list(),
    built = list(), labels = character(0),
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
# ...and THE globalVariables mirror, derived from those formals: every stage binds them with
# list2env(), which codetools cannot see (new_reg_shared()'s own line, one record up).
utils::globalVariables(names(formals(new_reg_ctx)))

# reg_ctx_locals() -- ctx_settings_locals()'s twin (R/tab.R): project the context INTO ONE STAGE'S
# SCOPE, rebuilt at each stage head and never written back. `shared` is the only nested carrier, so
# the projection is the ctx plus its fields; the disjointness assert is what makes `c()` safe here.
#' @keywords internal
#' @noRd
reg_ctx_locals <- function(ctx) c(ctx, ctx$shared)

reg_build <- function(data, specs, shared, tab_vars = NULL, .fit_cache = NULL, ref = NULL,
                      reref = FALSE, skeleton_data = data, parallel = NULL) {
  # `shared` is the TYPED record new_reg_shared() builds (Phase 19g) -- its formals ARE the contract,
  # so every field is present and a direct caller cannot be missing one. Normalised ONCE here; every
  # stage below reads it through reg_ctx_locals(), which binds its fields as locals.
  shared <- do.call(new_reg_shared, shared[intersect(names(shared), names(formals(new_reg_shared)))])
  ctx <- new_reg_ctx(
    data = data, specs = specs, shared = shared, tab_vars = tab_vars, fit_cache = .fit_cache,
    ref = ref, reref = reref, skeleton_data = skeleton_data, parallel = parallel,
    # Phase 15e: each spec carries its OWN resolved family / estimand / colour (set at the
    # boundary), read as sp$<key>. The homogeneous-context scalar `family` (first outcome) is still
    # needed by the split branch and reg_compare_rows(); derive it FROM the specs so it can never
    # drift from them.
    family = specs[[1]]$fit_family)
  list2env(reg_ctx_locals(ctx), environment())

  # THE STAGES. Each takes and returns the ctx; only the split recursion returns a finished table.
  # ⚠ the ORDER is the source order and is load-bearing -- three of them fit models, and every fit
  # may inform or warn (dev/verify_reg_specs.R compares the message stream IN ORDER).
  if (!is.null(tab_vars)) return(reg_stage_split(ctx))
  ctx <- reg_stage_setup(ctx)      # the skeleton, the table's shape facts, the per-spec plan
  ctx <- reg_stage_crude(ctx)      # the observed (crude) block of a ONE-outcome table, built once
  ctx <- reg_stage_specs(ctx)      # ONE reg_spec_build() per model (serial or pooled) + the layout
  ctx <- reg_stage_footer(ctx)     # the products' rows + the between-model comparison -> `test`
  ctx <- reg_stage_rows(ctx)       # the row axis: labels, relabels, sparklines, add_n -> `tab`
  ctx <- reg_stage_assemble(ctx)   # the crude blocks and the model columns into `tab`
  ctx <- reg_stage_tips(ctx)       # the crude tooltips (multinomial + numeric)
  reg_stage_finalize(ctx)          # the inference basis, then the shared assembly tail
}


# reg_stage_split() -- THE tab_vars RECURSION (Phase 12g): the regression analogue of tab()'s
# tab_vars -- fit the SAME model(s) within each level of a grouping variable and STACK the per-group
# tables into one grouped_tab (grouped by tab_vars + var), so tab_spread(tab_vars) can pivot the
# groups into side-by-side columns. Each group is a recursive reg_build() on its data subset, sharing
# ONE skeleton (skeleton_data = the full data) so every group has the same rows/columns (a level
# absent in a group -> empty cells). tab_vars is placed FIRST because the index columns DECLARE their
# roles (19f): tab_declared_vars() reads row_var = "levels", tab_vars = c(tab_vars, "var") off the
# stamped columns, so the crosstab spread machinery needs no change.
#
# ⚠ It RETURNS A FINISHED TABLE, not a ctx -- an early return from reg_build(), the shape
# tab_build_tables() has on the crosstab side. Every other stage is ctx-in, ctx-out.
#' @keywords internal
#' @noRd
reg_stage_split <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  sl <- levels(forcats::fct_drop(as.factor(data[[tab_vars]])))
  # Phase 20f-iii (axis G): the per-group body is reg_build_group() (R/reg-spec-build.R), a
  # namespaced worker, so this map IS tab_pmap() -- serial by default, a daemon pool under
  # `parallel`. A group already returned finished, fit-free tibbles and the one cross-group step
  # below is a post-loop barrier matching by KEY, so nothing else had to change -- and the message
  # stream was already group-major, so the 20f condition relay preserves its order exactly.
  # ⚠ measured (dev/tabxplor_reg_performance.md 6.2): this axis clears 2x only when the groups are
  # EVEN and the frame is survey-size (8 even survey waves 2.11x; 4 uneven race groups 1.22x).
  parts <- tab_pmap(list(g = sl), "reg_build_group",
                    .const = list(sl = sl, tab_vars = tab_vars, specs = specs,
                                  fit_cache = fit_cache),
                    .ship  = list(shared = shared, data = data),
                    .names = as.character(sl),
                    workers = tab_parallel_workers(parallel, fit_cache))
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
    tests <- reg_interaction_rows(tests, data, specs, shared, tab_vars, fit_cols)
  }
  # `empirical_tips` and `assumptions` are deliberately NOT carried from the groups: they are
  # per-GROUP facts (crude tooltips keyed var\rlevel\rcategory; the observed curve of each
  # predictor) and `meta` has no per-group slot, so merging them would attach the FIRST group's
  # numbers to every other group's cells. Absent is honest; wrong is not. (A split table's
  # sparklines are already baked into its row labels, and reg_check_plots() refits from
  # spec$call$fit_spec.)
  grouped <- reg_finalize(combined, tests, conf_level, var_labels,
                          group_vars = c(tab_vars, "var"),
                          meta_extra = list(subtext = subtext))
  # Phase 19h (KEY 7): the split groups go side by side whenever that is unambiguous -- ONE model
  # (a single outcome and a single predictor set) that is not multinomial (a multinomial has
  # several columns for one model, so a side-by-side layout has no one column per group). It is an
  # internal rule, not an argument: tab_spread() is the public way to control the layout, and
  # reg_spread_models() -- whose two post-spread repairs were generic all along -- is deleted.
  if (length(specs) == 1L && !identical(family, "multinomial")) {
    return(tab_spread(grouped, tidyselect::all_of(tab_vars)))
  }
  return(grouped)
}


# reg_stage_setup() -- THE TABLE'S SHAPE, before any model exists. The coefficient SKELETON every
# column is aligned to, the handful of whole-table facts derived from the specs, and the PER-SPEC
# PLAN reg_spec_build() reads. ⚠ it REWRITES `data` on the reref path (the reference relevel) and
# four later readers depend on that, hence its place among the declared products.
#
# Phase 20f-iii: the fits left this stage for reg_spec_build(). What made that possible is that the
# skeleton is fit-FREE in every shape but one -- see the cascade below, whose ORDER is the contract:
# the marginal builders key by the ORIGINAL variables, so even a compound formula gets a clean
# bare-variable skeleton there; only an all-coefficient table with a compound formula must read it
# back off the first fit, and that single case is what `skeleton_deferred` names (and one of the
# three reasons reg_specs_independent() forces the serial path).
#' @keywords internal
#' @noRd
reg_stage_setup <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  # Phase 15b jamovi live reref: `data` arrives at the CANONICAL (natural-first) reference; the
  # digest is fitted once on THAT (cached, reference-independent) and reparametrized to the display
  # `reference`, which is baked into the skeleton (built on the releveled data). So `data` is
  # releveled here -- the skeleton + empirical companions use the display reference -- and the
  # canonical frame travels as `data_canon`. ⚠ off that path `data_canon` stays NULL rather than a
  # second name for `data`: it would otherwise look like a frame worth shipping to a worker.
  data_canon <- NULL
  skeleton   <- NULL
  if (isTRUE(reref)) {
    data_canon <- data
    if (!is.null(ref)) data <- reg_apply_references(data, ref, union_predictors)
    skeleton <- reg_skeleton(data, union_predictors)   # an INPUT to reg_reref_fit_res(), not an output
  }

  compound   <- purrr::map_lgl(specs, ~ isTRUE(.$compound))
  # Phase 19e: WHICH builder each spec takes is the estimand row's own `builder` -- the choice that
  # used to be a table-scalar `if` over (effect, at, family).
  builders   <- purrr::map_chr(specs, ~ .$est$builder %||% "coef")
  # `skeleton_data` (Phase 12g tab_vars) is the FULL data so every split group shares one skeleton
  # (missing group levels -> empty cells); it defaults to `data`, so non-split builds are unchanged.
  skeleton_deferred <- FALSE
  if (is.null(skeleton)) {
    if (any(builders != "coef"))  skeleton <- reg_skeleton(skeleton_data, union_predictors)  # one row per PREDICTOR (z15)
    else if (any(compound))       skeleton_deferred <- TRUE          # only here: reg_skeleton_from_fit()
    else                          skeleton <- reg_skeleton(skeleton_data, union_predictors, shape_terms)
  }

  prefix_dep    <- length(specs) > 1L
  # Phase 14w: a model COMPARISON (several models, one outcome) keeps each model's col_var = its own
  # name (borders separate the models; the outcome/reference/effect go in the title). A single or
  # multi-outcome table shares one outcome col_var per model column + its empirical companions.
  n_outcomes    <- length(unique(purrr::map_chr(specs, "outcome")))
  is_comparison <- length(specs) > 1L && n_outcomes == 1L
  # Phase 18z9: ONE predictor-kind split for the whole builder (reg_is_factor_var) -- the AME
  # column's numeric cells, the crude companions and the crude tips all read the same two vectors.
  numeric_preds <- reg_numeric_preds(skeleton_data, union_predictors)
  factor_preds  <- reg_factor_preds(skeleton_data, union_predictors)
  # Phase 18z8 (a z5 defect): `effect = "at_reference"` makes the model cell a marginal effect AT THE
  # REFERENCE PROFILE, while the crude companion stays a MARGINAL effect over the whole sample -- two
  # different estimands, so their difference is not "what adjustment did", and no `obs` is attached:
  # the cells stay uncoloured, `{obs}` blanks, and reg_color_notes() says why. Phase 20i: that
  # decision reads the estimand's declared `obs` PER SPEC, in reg_set_obs() (`sp$est$obs`), so a mixed
  # table withholds it only on the at_reference columns -- the table-scalar `any()` gate used to blank
  # the whole table.

  # Phase 18z13 (SS7.2): the per-predictor global test is in the DEFAULT stats set, so NULL / "all" /
  # TRUE ask for it; FALSE / "none" and an explicit vector that omits it do not. (The interaction
  # test is opt-in only, hence its narrower gate.) A table-scalar gate, read by every spec.
  want_global <- is.null(stats) || identical(stats, "all") || isTRUE(stats) ||
    (is.character(stats) && "global" %in% stats)

  # THE PER-SPEC PLAN (Phase 20f-iii). Two de-duplications used to be loop-carried -- an `add_n`
  # `break`/`next` pair and the crude block's `break` (20f-ii) -- which a worker cannot reproduce
  # and a reader had to simulate. Both are declared here instead, as one vector per question.
  outcomes <- purrr::map_chr(specs, "outcome")
  # add_n: ONE count column per distinct outcome -- every compared model shares it, since z13's
  # default puts them on one population. The NAME is table-scalar too.
  want_n   <- isTRUE(add_n) & (n_outcomes > 1L | seq_along(specs) == 1L) & !duplicated(outcomes)
  n_names  <- if (n_outcomes > 1L) paste0("n [", outcomes, "]") else rep("n", length(specs))
  # THE CRUDE BLOCK BELONGS TO THE OUTCOME (Phase 20f-iiii). Every input to it is table-wide or
  # per-OUTCOME, so a table with ONE outcome has ONE block: reg_stage_crude() builds it before any
  # model (`want_crude`), and a table with SEVERAL builds each one with its own spec (`want_emp`) --
  # which is also its outcome, so it stays on the parallel axis instead of serialising into a
  # pre-pass. Before this, spec 1 built the single-outcome block and handed it down the loop, which
  # is what made "compared models share an observed block" a reason to refuse the pooled branch.
  # Phase 18z9: numeric predictors get a crude column too, from their univariable fit -- EXCEPT in
  # compound-formula mode, where `var` is a bare RHS name whose model term may be an interaction or a
  # basis expansion (`age*race`, `poly(age, 2)`), so a univariable slope is not that row's estimand.
  # ⚠ `any(compound)` is deliberately table-scalar: one compound spec empties this for every block.
  num_e    <- if (any(compound)) character(0) else numeric_preds
  has_pred <- length(factor_preds) > 0L || length(num_e) > 0L
  crude_ok <- !is.na(purrr::map_chr(specs, ~ .$crude_key %||% NA_character_))
  want_emp   <- emp_on(empirical) & has_pred & (n_outcomes > 1L) & crude_ok
  want_crude <- emp_on(empirical) && has_pred && n_outcomes <= 1L && crude_ok[[1L]] &&
    # a deferred skeleton is read back off the FIRST fit, so a stage that runs BEFORE the fits has
    # nothing to align the crude columns to. Unreachable (the assert below says why), kept as the
    # statement of what the stage needs.
    !skeleton_deferred

  # ⚠ THE TWO FACTS reg_stage_crude() RESTS ON, asserted rather than remembered:
  #  (1) with one outcome every spec is built from deps[1, ] (reg_resolve_specs), so the estimand,
  #      family, trials, crude_key and colour it reads off specs[[1]] are every spec's. Phase 20f-ii
  #      relied on this silently ("true today, stated nowhere") to stop rebuilding the block.
  #  (2) a deferred skeleton and a crude block cannot co-exist: `compound` is only ever
  #      `formula_mode`, and reg_crude_key(compound = TRUE) is NA, which turns `empirical` off at
  #      the argument boundary (R/reg-resolve.R). Without this, `want_emp` no longer covering spec 1
  #      would silently drop a deferred table's crude block.
  if (length(specs) > 1L && n_outcomes <= 1L) {
    one <- function(f) length(unique(purrr::map(specs, f))) == 1L
    stopifnot(one("outcome"), one("fit_family"), one("trials"), one("crude_key"),
              one(~ .$est$effect), one(~ .$est$measure), one("color"))
  }
  stopifnot(!skeleton_deferred || !emp_on(empirical))

  ctx_update(ctx, list(data = data, data_canon = data_canon, skeleton = skeleton,
                        skeleton_deferred = skeleton_deferred,
                        compound = compound, builders = builders,
                        prefix_dep = prefix_dep, n_outcomes = n_outcomes,
                        is_comparison = is_comparison,
                        numeric_preds = numeric_preds, factor_preds = factor_preds,
                        want_global = want_global,
                        spec_plan = list(want_n = want_n, n_names = n_names,
                                         want_emp = want_emp, want_crude = want_crude,
                                         num_preds = num_e)))
}


# reg_stage_crude() -- THE OBSERVED (CRUDE) BLOCK OF A ONE-OUTCOME TABLE, built once, before any
# model. It is the descriptive companion every model column is compared to: the crude % / mean /
# rate and the crude effect, their intervals, the complete-case frame and the fitted univariable
# legs the adjustment gap test needs.
#
# WHY IT IS A STAGE (Phase 20f-iiii). The block is a function of the OUTCOME, not of a model: every
# input is table-wide or per-outcome, and reg_stage_setup()'s assert says so. Building it inside the
# first spec and handing it down the loop made it look per-model, cost a `break`, and was one of the
# three reasons the models could not be dispatched. A table with SEVERAL outcomes keeps its blocks
# per spec (`want_emp`) -- there each spec IS an outcome, so nothing is shared and the work stays on
# the parallel axis rather than serialising into this pre-pass.
#
# ⚠ IT IS FIT-FREE, and that is what makes it liftable. The block used to read two things off the
# model object; both have exact producers of their own:
#   * `positive_level` -- reg_positive_level() is the function reg_prep_binary() itself calls to
#     order the levels, and `f$positive_level` is the attribute it writes. Same frame, same answer.
#   * the outcome's reference CATEGORY -- reg_crude_yw() collapses any `ref_category` that is not a
#     level of the crude frame to `cats[1]`, and the crude frame is a row-subset of the model frame,
#     so `f$y_ref` and the first level here are the same value in both branches. (`f$y_ref` is
#     written only by reg_fit_multinom(); it is NULL for every other family, and absent on the
#     jamovi reref path.)
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
  # Phase 14w (item 3): the crude companions share the model column's outcome col_var (one span, no
  # border). NOT in comparison mode, where the crude block stays a distinct col_var beside the models.
  if (!is_comparison && length(block$cols)) {
    scv <- reg_shared_col_var(sp_fam, sp$outcome, pos, cleannames)
    block$cols <- purrr::map(block$cols, ~ set_col_var(.x, scv))
  }
  # the numeric predictors' descriptive tooltip belongs to the BLOCK, not to a model: it keys the
  # crude effect column, which every spec of a one-outcome table shares. Built here exactly once --
  # letting each spec build it would re-emit the same rows, which is what Phase 20f-ii deleted.
  block$tips_num <- reg_spec_tips_num(sp, pos, block, ctx)

  ctx_update(ctx, list(crude = block))
}


# reg_crude_block() -- the arithmetic of one observed block: the crude grid, the univariable legs of
# the predictors that have no closed form, and the columns they become. Shared by reg_stage_crude()
# (one outcome) and reg_spec_build() (one block per spec, several outcomes), so the two cannot fork.
#' @keywords internal
#' @noRd
reg_crude_block <- function(sp, sp_fam, inv_sp, key, mdata, pos, y_ref, var_y, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  # ⚠ the two crude predictor sets are TABLE-scalar and come from the declared plan: `num_preds` is
  # emptied when ANY spec has a compound formula (one such spec strips the numeric crude columns from
  # every block, compound or not), so it is not derivable from `sp` alone.
  fac_preds_e <- factor_preds
  num_preds_e <- spec_plan$num_preds
  emp <- reg_empirical(mdata, fac_preds_e, sp$outcome, key, pos, design_spec$wt,
                       trials = sp$trials, ref_category = y_ref,
                       conf_level = conf_level, design_spec = design_spec)
  # Which predictors have no closed form and must be fitted? z9: the numeric ones. z10: EVERY
  # predictor under an ordinal outcome (proportional odds is a constraint, so the univariable model
  # is not saturated). reg_crude_saturated() states the rule; nothing here re-derives it.
  fit_preds_e <- c(
    num_preds_e,
    if (!reg_crude_saturated(key, TRUE)) fac_preds_e else character(0))
  # The crude fits take the FULL `data` + `drop_extra`, never the pre-filtered frame: a prebuilt
  # survey design's keep_mask is computed from `data` itself (reg_resolve_design).
  # `marginal`: reg_empirical_columns() swaps the crude shape for a marginal one only where the
  # model's own estimand is marginal AND on a probability scale (a gaussian AME IS its coefficient;
  # a poisson AME is additive while its crude shape stays a rate RATIO, which reg_same_estimand()
  # then refuses), so the fit follows the shape it must fill.
  fit_e <- reg_empirical_fit(
    data, fit_preds_e, sp$outcome, sp_fam, design_spec,
    outcome_level = inv_sp,
    conf_level = conf_level, method = method, skeleton = skeleton, multiplier = multiplier,
    other_preds = union_predictors, est = sp$est, wt = design_spec$wt,
    # z17 (D2): always kept. `want_fit` does not decide whether the univariable crude models are
    # FITTED (they are, to fill the crude column) -- only whether the fitted object survives for
    # the gap test's crude leg. Build-time locals; they never reach the jamovi .fit_cache.
    want_fit = TRUE, trials = sp$trials,
    shape_terms = shape_terms,
    marginal = !identical(sp$est$effect, "coefficient") &&
      (reg_fam_binary(sp_fam) || reg_fam_prob(sp_fam)))
  out <- reg_empirical_columns(skeleton, emp, fac_preds_e, key, sp_fam, sp$est, var_y,
                               conf_level = conf_level, color_signif = color_signif,
                               color = sp$color, fit_est = fit_e,
                               # W-D: `n_eff` is written only where something corrected it
                               weighted = svy_weighted(design_spec, design_spec$wt),
                               # z16-iiiii (D4): the design df the MODEL columns are already
                               # referred to, so the crude bracket beside them matches
                               degf = design_spec$degf %||% Inf,
                               emp_mode = empirical)
  # the crude columns take the table's own display, exactly like the model columns: one grammar, and
  # by default the MIRROR layout, so the two estimates end up side by side (reg_default_display).
  dress <- function(cl) purrr::map(cl, function(col)
    if (is.null(display)) reg_default_display(col, empirical) else reg_apply_display(col, display))
  out$cols     <- dress(out$cols)
  out$cat_cols <- dress(out$cat_cols)
  # Phase 18z8-B: the block also carries what the GAP TEST needs -- the frame it was computed on,
  # the factor predictors it covers and the fitted crude legs. None of them leaves reg_build():
  # reg_emp_slim() drops everything but `$cols` on the way out of reg_spec_build().
  out$frame     <- mdata
  out$fac_preds <- fac_preds_e          # ⚠ live: reg_set_obs() -> reg_gap_se_columns(fac_preds =)
  out$fit_preds <- fit_preds_e
  out$fits      <- fit_e$fits
  out$grid      <- emp
  out$degraded  <- isTRUE(attr(emp, "degrade"))
  out
}


# reg_cols_ame() -- REG_ESTIMANDS builder "ame": the average marginal effect column(s) of one fit.
#' @keywords internal
#' @noRd
reg_cols_ame <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  # Phase 15e: prob-scale / per-category / colour shape are per OUTCOME family (a mixed AME table
  # mixes binomial prob-points with a gaussian coef in one grid).
  sp_fam       <- sp$fit_family
  # Phase 19m-ii: DERIVED here rather than stored on the spec. `empirical` is bound by
  # reg_ctx_locals() and is FINAL at this point, so the header word cannot disagree
  # with the table's own shared$empirical -- which the eager/lazy pair in tab_reg() could.
  sp_eff       <- reg_eff_word(sp$est, empirical)
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
                        want_pred = TRUE,
                        comparison = if (ratio_ame) "lnratioavg" else NULL,
                        multiplier = multiplier, engine = reg_marginal_engine(sp_est))
  # the ADDITIVE twin reg_fill_base() stores beside a multiplicative estimate. A ratio contrast is
  # not an average of differences, so it cannot be re-derived from `marg`; a second point-estimate
  # sweep is the honest way to get it, and it costs no fit. The additive paths reuse `marg` itself.
  marg_add <- if (!ratio_ame) marg
    else if (is.null(f$fit)) NULL
    else reg_fill_sweep(f$fit, f$data, sp$predictors, conf_level, design_spec$wt, multiplier)
  dress <- function(col, group = NULL) {
    col <- reg_fill_base(col, marg_add, skeleton, sp$predictors, group = group)
    if (is.null(display)) reg_default_display(col, empirical)
    else reg_apply_display(col, display)
  }
  var_y <- if (!prob_scale) suppressWarnings(stats::var(as.numeric(f$data[[sp$outcome]])))
           else NA_real_
  if (per_category) {                            # one AME column per OUTCOME category (all levels)
    groups <- levels(as.factor(f$data[[sp$outcome]]))
    purrr::map(groups, function(g) {
      jc  <- reg_cleanup(g, cleannames)
      # Phase 14s (G) + 14w (item 3): the per-category AME columns of one model share `sp$label`
      # ("<dep>: AME (adjusted %)") as col_var (no inter-category border, one span names the effect
      # once); the visible NAME is just the category (the repeated ": AME" is stripped).
      lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "", jc)
      list(label = lab, emp_key = g,   # emp_key: raw category, for the empirical tooltip (Phase 14v)
           col   = dress(reg_marginal_column(skeleton, marg, sp$predictors, shape,
                                             var_y, g, sp_col, color_signif, sp$label,
                                             model_family = sp_fam), g))
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
          else reg_shared_col_var(sp_fam, sp$outcome, f$positive_level, cleannames)
    list(list(
      label = reg_model_col_name(sp_eff, sp$outcome, is_comparison, sp$label, n_outcomes),
      col   = dress(reg_marginal_column(skeleton, marg, sp$predictors, shape,
                                        var_y, NA_character_, sp_col, color_signif,
                                        cv, or_tip = or_tip, model_family = sp_fam))))
  }
}


# reg_cols_vsrest() -- REG_ESTIMANDS builder "vsrest": one "category j vs the rest" OR column per
# outcome category, at the reference profile.
#' @keywords internal
#' @noRd
reg_cols_vsrest <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  # MNL "j vs rest" OR at the reference profile (D3-flavour-2): exp of the profile log-odds-ratio of
  # "category j vs the rest" for each predictor level; one OR column per outcome category. Reached only
  # for a HOMOGENEOUS multinomial table (a mixed table degrades at="reference" -> "average" upstream).
  sp_fam <- sp$fit_family
  sp_col <- sp$color
  marg   <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                         at = "reference", comparison = "lnor", want_pred = FALSE,
                         engine = reg_marginal_engine(sp$est))
  # the adjusted predictions of the same fit, sample-averaged: the "vs rest" contrast is evaluated at
  # a profile, but the LEVEL a reader compares it to is the population's.
  marg_add <- if (is.null(f$fit)) NULL else
    reg_fill_sweep(f$fit, f$data, sp$predictors, conf_level, design_spec$wt)
  groups <- levels(as.factor(f$data[[sp$outcome]]))
  purrr::map(groups, function(g) {
    jc  <- reg_cleanup(g, cleannames)
    # Phase 14s (G) + 14w (item 3): shared col_var (`sp$label`) across the "vs rest" category columns
    # of one model; the repeated ": OR" is stripped from the visible NAME (the span carries it).
    lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "", jc, " vs rest")
    col <- reg_marginal_column(skeleton, marg, sp$predictors, "or",
                               NA_real_, g, sp_col, color_signif, sp$label,
                               model_family = sp_fam)
    col <- reg_fill_base(col, marg_add, skeleton, sp$predictors, group = g)
    list(label = lab,
         col   = if (is.null(display)) reg_default_display(col, empirical)
                 else reg_apply_display(col, display))
  })
}


# reg_cols_coef() -- REG_ESTIMANDS builder "coef": the plain coefficient column of one fit (a
# multinomial fans out into one column per outcome category).
#' @keywords internal
#' @noRd
reg_cols_coef <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  # Phase 15e: each column takes its own family shape (multinomial fans out; glm/gaussian are one col).
  sp_fam   <- sp$fit_family
  sp_eff   <- reg_eff_word(sp$est, empirical)   # 19m-ii: derived, see cols_ame above
  sp_col   <- sp$color
  # the coefficient path never runs a marginal sweep of its own, so it runs the one reg_fill_base()
  # needs: point estimates only, on the dependency-free engine.
  # WARNING: a cached fit may have been dropped (`want_fit = FALSE`, jamovi's repaint path). Without
  # the model there is no adjusted prediction to compute, and no reason to reach for an engine.
  model_predictors <- if (isTRUE(sp$compound)) unique(skeleton$var) else sp$predictors
  marg <- if (!is.null(f$marg)) f$marg               # the digest path already ran the sweep
          else if (is.null(f$fit)) NULL
          else reg_fill_sweep(f$fit, f$data, sp$predictors, conf_level, design_spec$wt, multiplier)
  # A SUMMED-SCORE outcome's level is the mean SCORE, not the share it is built from -- that is the
  # quantity a reader of a battery of items wants, and it is what the crude column beside it shows.
  # The fit predicts the share, so the adjusted score is that share times the number of items.
  if (!is.null(marg$pred) && nrow(marg$pred) && !is.na(sp$trials %||% NA))
    marg$pred$pred <- marg$pred$pred * as.numeric(sp$trials)
  dress <- function(col, group = NULL) {
    col <- reg_fill_base(col, marg, skeleton, model_predictors, group = group)
    if (is.null(display)) reg_default_display(col, empirical)
    else reg_apply_display(col, display)
  }
  if (sp_fam == "multinomial") {
    cols <- reg_columns_multinom(skeleton, f, sp, sp$est, sp_col, color_signif,
                                 cleannames, prefix_dep, model_family = sp_fam,
                                 method = method)
    return(purrr::map(cols, function(lc) { lc$col <- dress(lc$col, lc$emp_key); lc }))
  }
  # Phase 14w (item 3): outcome col_var + "Model <effect>" name (comparison keeps the model name).
  cv  <- if (is_comparison) sp$label
         else reg_shared_col_var(sp_fam, sp$outcome, f$positive_level, cleannames)
  col <- reg_column(skeleton, f, model_predictors, cv, sp$est, sp_col, color_signif,
                    model_family = sp_fam, method = method, trials = sp$trials)
  list(list(label = reg_model_col_name(sp_eff, sp$outcome, is_comparison, sp$label, n_outcomes),
            col = dress(col)))
}


# reg_stage_specs() -- ONE reg_spec_build() PER MODEL, and the column LAYOUT their products imply.
# THE per-model half of the build (R/reg-spec-build.R): the fit, the columns, this model's footer
# rows, its `add_n` count, its observed (crude) block, its `obs`/`gap_se` and its tooltips.
#
# SERIAL OR POOLED. reg_specs_independent() is the ONE predicate -- NULL when a spec needs nothing
# from another spec, else the reason, which is reported when `parallel` was explicitly asked for so
# that what was not parallelised is never silent (Phase 20f's rule). Two things ride the serial
# branch and cannot ride the other, which is why they ARE reasons: the crude block spec 1 shares
# with the compared models, and the skeleton read back off the first fit.
#' @keywords internal
#' @noRd
reg_stage_specs <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  why     <- reg_specs_independent(ctx)
  workers <- if (is.null(why)) tab_parallel_workers(parallel, fit_cache) else 0L
  # ⚠ only when the ARGUMENT was passed. A user who set the option once would otherwise be told, on
  # every model comparison they ever build, about a dispatch they did not ask for by name.
  if (!is.null(why) && !is.null(parallel) && !isFALSE(parallel))
    cli::cli_inform(c("i" = "{.arg parallel}: the models are built one after another here -- {why}."))

  if (workers > 1L) {
    # ⚠ the whole ctx is the shipped object: `data`, `skeleton_data`, a prebuilt survey design and
    # (on a one-outcome table) the crude block all live in it, and everywhere() sends it ONCE per
    # dispatch (measured 0.05 s for a 200 000-row frame, dev/tabxplor_reg_performance.md 6.3).
    products <- tab_pmap(list(i = seq_along(specs)), "reg_spec_build",
                         .ship = list(ctx = ctx), .names = purrr::map_chr(specs, "label"),
                         workers = workers)
  } else {
    products <- vector("list", length(specs))
    for (k in seq_along(specs)) {
      products[[k]] <- reg_spec_build(k, ctx)
      # the ONE fact a later spec still takes from the first one, and the surviving refusal above
      if (k == 1L && isTRUE(skeleton_deferred))
        ctx <- ctx_update(ctx, list(skeleton = products[[1]]$skeleton))
    }
  }
  # ⚠ AFTER both branches, not inside the serial one: a table with ONE compound spec defers its
  # skeleton but has nothing to share, so reg_specs_independent() lets it take the pooled branch --
  # where the loop-carried update above never runs, and the ctx would keep its NULL skeleton.
  if (isTRUE(skeleton_deferred)) skeleton <- products[[1]]$skeleton

  # THE LAYOUT. `built` is the flattened VIEW of the products' columns, in order (a multinomial /
  # MNL-vs-rest / AME-per-category model contributes several).
  built  <- purrr::flatten(purrr::map(products, "cols"))
  labels <- make.unique(purrr::map_chr(built, "label"))

  # Phase 12f: the model-summary footer keys each fit's GOF to its FIRST output column (make.unique'd).
  # ⚠ every model owns at least one column, and the layout DEPENDS on it: two models with the same
  # fit_first_idx would collide in reg_stage_assemble()'s match(), silently dropping the second's
  # crude block. Every declared builder returns >= 1 column, so this is an assert, not a branch.
  # `fit_ncol` stays a LOCAL: it feeds the assert and the offsets, and nothing downstream asks how
  # many columns a model has. `fit_first_idx` is the key both consumers use -- reg_stage_assemble()
  # (match(), so it is non-NA exactly at a fit's first column) and reg_stage_tips() (the within-spec
  # column offset).
  fit_ncol      <- purrr::map_int(products, ~ length(.x$cols))
  if (any(fit_ncol == 0L)) cli::cli_abort("Internal: a model produced no column.")
  fit_first_idx <- cumsum(c(1L, utils::head(fit_ncol, -1L)))
  fit_first_col <- labels[fit_first_idx]

  ctx_update(ctx, list(products = products, skeleton = skeleton,
                        built = built, labels = labels,
                        fit_first_idx = fit_first_idx, fit_first_col = fit_first_col,
                        emp_degraded = any(purrr::map_lgl(products, ~ isTRUE(.x$degraded))) ||
                          isTRUE(crude$degraded)))
}


# reg_stage_footer() -- THE `test` TIBBLE, assembled from the products plus the one footer producer
# that is BETWEEN models and could not join them.
#
# ⚠ SLOT-MAJOR, not product-major: all the GOF rows, then the comparison, then all the global rows,
# then all the checks -- today's order, and what dev/verify_reg_specs.R compares. (The rendered
# footer re-sorts by TEST_ROWS anyway, so this is cheap fidelity rather than a load-bearing fact.)
#
# ⚠ reg_compare_rows() STAYS. It is a test BETWEEN two fitted models -- stats::anova(m_lo, m_hi), or
# survey's own regTermTest Wald arm -- so it needs the fit OBJECTS, which is why `compare != "none"`
# is reg_specs_independent()'s first refusal and why the products carry a `fit` in that one case.
# `family` is deliberately the TABLE scalar here (use_f / use_wald describe the whole comparison).
#' @keywords internal
#' @noRd
reg_stage_footer <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  # each product's rows carry a PRE-make.unique() placeholder in `col`; the real key is this fit's
  # first output column, which only exists once every model has been built. One overwrite per
  # product, not a match: every row of one model shares one `col`.
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
                              fit_first_col = fit_first_col, compare = compare, baseline = baseline)
  gl <- rekey("global_rows"); if (!is.null(gl)) reg_gof <- dplyr::bind_rows(reg_gof, gl)
  ck <- rekey("check_rows");  if (!is.null(ck)) reg_gof <- dplyr::bind_rows(reg_gof, ck)

  ctx_update(ctx, list(test = reg_gof))
}


# reg_stage_rows() -- THE ROW AXIS: the display level of every skeleton row (cleannames, the
# `shape` recode, the per-unit multiplier, the observed sparkline), the two declared index columns
# the whole table is built on, and `add_n`'s count column.
#' @keywords internal
#' @noRd
reg_stage_rows <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

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
  # deliberately drawn on `skeleton_data`, not `data` -- under `tab_vars` the groups share one
  # skeleton and are pivoted into columns by row, so a per-group curve would give the same row two
  # different labels and break the alignment. Ten bins fixed, so two predictors are comparable.
  assumptions <- reg_curves(skeleton_data, specs, numeric_preds, design_spec$wt,
                            positive_level = products[[1]]$positive_level,
                            design = design_spec$design)
  if (!is.null(assumptions)) {
    spark <- tx_option("spark")
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
  # display intent like tab()'s own `add_n`: the count needs the model's complete-case frame, and
  # tab_add_n_pct() folds into a Total cell that a reg table does not have. ⚠ so it is built by
  # reg_spec_build(), and WHICH specs contribute one, under which name, is reg_stage_setup()'s
  # declared plan (one column per distinct outcome; every compared model shares it, since z13's
  # default puts them on one population). Here they are only spliced, in spec order.
  for (pr in products) for (nm in names(pr$n_col)) tab[[nm]] <- pr$n_col[[nm]]

  ctx_update(ctx, list(tab = tab, disp_levels = disp_levels, assumptions = assumptions))
}


# reg_emp_frame() -- the per-outcome complete-case frame the crude companions and the multinomial
# tooltips share with the model (reg_complete_frame = reg_fit's own frame). `union_predictors` ==
# the model's predictors when not comparing; in comparison mode it is the shared population.
# Recomputed from `data` (fits[[i]]$data is NULL on the reref/digest path). On this listwise-complete
# frame reg_empirical()'s per-predictor NA filter is a no-op, so the crude reference level / n
# exactly match the model.
# Phase 18z13 (D1): `na_shared_vars` is the same extra-completeness set reg_fit() receives, so under
# the default this frame IS the model's own frame -- crude and adjusted on the same rows by
# construction, not by coincidence. Under `na = "drop_by_model"` it is empty and the crude block
# keeps its union population, which is why `obs` is then gated (reg_same_frame).
# Phase 20e: a top-level function because TWO stages need it -- _empirical and _tips.
#' @keywords internal
#' @noRd
reg_emp_frame <- function(dep, ctx) {
  s <- ctx$shared
  reg_complete_frame(ctx$data, c(dep, s$union_predictors, s$na_shared_vars,
                                 reg_design_vars(s$design_spec)))
}


# reg_set_obs() -- ONE model column's `obs` (the value it is compared to, on its own scale) and the
# `gap_se` of the distance between them -- or NEITHER, which is the honest answer whenever the two
# estimators are not the same estimand on the same people.
# `bi` = the built column record, `e` = the crude block it is scored against, `f`/`sp` = the fit and
# spec this COLUMN came from -- NOT the crude block's, which differ in comparison mode (there one
# observed block serves several models, so `obs` is shared while the covariance behind `gap_se` is
# per model). Phase 20f-iii passes them rather than indexing `fits[[fi]]`: the caller is now that
# model's own reg_spec_build(), which holds them and must not hand a fit back.
# Phase 18z10: `key` = the column's OWN outcome category, stored on it at build time as `emp_key`
# (reg_columns_multinom / the per-category AME loop already stamp it). A multinomial or
# ordinal-marginal fit owns one column per category and each needs its OWN crude counterpart, so the
# crude record's `effect` is a list keyed the same way; "" is the key of a single-column fit. A
# missing key means no crude counterpart for that column -- `obs` stays NA, the cell uncoloured.
#' @keywords internal
#' @noRd
reg_set_obs <- function(bi, e, f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  col <- bi$col
  # Phase 20i: the "may a crude value be attached?" decision is PER SPEC (the estimand's declared
  # `obs`, withheld exactly at the reference profile -- R/reg-estimand.R). `sp` is this column's own
  # spec, so a mixed table -- effect = c(a = "at_reference", b = "coefficient") -- withholds `obs`
  # only on a's columns and keeps it on b's. A table-scalar `any(!obs)` used to blank the whole table.
  if (is.null(e) || !isTRUE(sp$est$obs)) return(col)
  if (!reg_same_estimand(e$shape, col)) return(col)     # z5 defect: same scale, or nothing
  # Phase 18z13 (D1): ...and the same PEOPLE, or nothing. A model fitted on rows the crude block
  # does not cover has a "gap" that is listwise deletion, not adjustment -- the same predicate that
  # withholds its test therefore withholds the value the colour would score.
  if (!reg_same_frame(e$frame, f)) return(col)
  key <- if (is.null(bi$emp_key)) "" else as.character(bi$emp_key)
  ev  <- cat_get(e$effect, key)
  if (is.null(ev)) return(col)
  col <- set_obs(col, ev)
  # When the crude effect draws NO column of its own it is folded into the model cell instead, as
  # "{est} ({obs})", so it is visible at all. One template for every family: `obs` is defined ON THE
  # CELL'S OWN SCALE, so the bracket is the same kind of quantity as the estimate, and the printed
  # bracket IS what `color = "adjustment"` scores -- number and colour cannot tell different stories.
  # An explicit `display` wins outright (it was already written, and is not a value cell's default).
  if (identical(empirical, "cell") && is.null(display)) {
    d   <- get_display(col)
    hit <- is.finite(ev) & d %in% DISPLAY_VALUE_CELLS
    if (any(hit)) col <- set_display(col, dplyr::if_else(hit, "{est} ({obs})", d))
  }
  g <- reg_gap_se_columns(f, sp, col, skeleton, e$shape, e$frame,
                          e$fac_preds, sp$est, design_spec$wt,
                          fits_crude = e$fits, fit_preds = e$fit_preds, multiplier = multiplier,
                          category = key)
  if (is.null(g)) col else set_gap_se(col, g)
}


# reg_add_emp_cols() -- splice a crude block's columns into the table. The multi-outcome
# disambiguator is a "[outcome]" BRACKET (the console shows it; every exporter STRIPS it via
# tab_col_var_header(), the col_var span already naming the outcome).
#' @keywords internal
#' @noRd
reg_add_emp_cols <- function(tab, cols, suffix) {
  for (nm in names(cols)) {
    out_nm <- if (nzchar(suffix)) paste0(nm, " [", suffix, "]") else nm
    tab[[out_nm]] <- cols[[nm]]
  }
  tab
}


# reg_stage_assemble() -- THE COLUMNS INTO THE TABLE: each crude block spliced in ahead of the model
# columns it serves, then those columns. They already carry their `obs` and `gap_se` -- reg_set_obs()
# runs inside reg_spec_build(), because doing it here would need the fits back.
#' @keywords internal
#' @noRd
reg_stage_assemble <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  # one crude companion before all model columns when there is a single outcome (incl. a
  # model-comparison list -- all its models share the outcome, so reg_stage_crude() built ONE block
  # for the table); per-fit before each fit's first model column when several outcomes (names
  # suffixed so they do not collide).
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


# reg_stage_tips() -- `meta$empirical_tips`, assembled from the products' fragments.
#
# The two blocks that produce them live in reg_spec_build() (R/reg-spec-build.R), because both read
# the crude block's HEAVY halves -- the per-(var, level, category) grid and the complete-case frame
# -- which must not travel back from a worker. What arrives here is keyed by SKELETON ROW and, for
# the multinomial fragment, by WITHIN-SPEC COLUMN: neither the final `make.unique()`d labels nor the
# display levels exist while a model is being built. Resolving them is this stage's whole job.
#
# ⚠ SLOT-MAJOR (every multinomial fragment, then every numeric one), which is the order the two
# blocks ran in. The consumer splits by `col` and takes first-match, and the two namespaces are
# disjoint (model labels vs `Obs_*` names), so this is fidelity rather than a load-bearing fact.
#' @keywords internal
#' @noRd
reg_stage_tips <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  mnl <- purrr::compact(purrr::map(seq_along(products), function(k) {
    fr <- products[[k]]$tips$mnl
    if (is.null(fr) || nrow(fr) == 0L) return(NULL)
    tibble::tibble(col   = labels[fit_first_idx[[k]] + fr$col_idx - 1L],
                   var   = fr$var,
                   level = disp_levels[fr$row],
                   tip   = fr$tip)
  }))
  # ⚠ the numeric fragments come from the BLOCKS, not the specs: one per outcome. With a single
  # outcome that is reg_stage_crude()'s one block; with several it is each spec's own, which is
  # why the products still carry theirs.
  num <- purrr::compact(purrr::map(c(list(crude$tips_num), purrr::map(products, ~ .x$tips$num)),
                                   function(fr) {
    if (is.null(fr) || nrow(fr) == 0L) return(NULL)
    tibble::tibble(col = fr$col, var = fr$var, level = disp_levels[fr$row], tip = fr$tip)
  }))

  rows <- c(mnl, num)
  ctx_update(ctx, list(empirical_tips = if (length(rows)) purrr::list_rbind(rows) else NULL))
}


# reg_stage_finalize() -- the inference basis this build ended on, then the assembly tail shared
# with the split branch (reg_finalize()). Returns the finished table, not a ctx.
#' @keywords internal
#' @noRd
reg_stage_finalize <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

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
  reg_finalize(tab, test, conf_level, var_labels, group_vars = "var",
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
#' New to regressions with tabxplor? A first model needs only three arguments: `data`, `outcome`
#' (the outcome) and `predictors`. tabxplor picks the right model from the outcome's type --- a
#' two-level factor gives logistic **odds ratios**, a numeric gives linear **betas**, a count gives
#' Poisson **rate ratios**, and a 3+ level factor gives multinomial or ordinal odds ratios --- so you
#' rarely set `family` by hand. Add `empirical = TRUE` to show the crude (unadjusted) effect beside the
#' model's adjusted one. See `vignette("tabxplor-reg")` for a guided tour.
#'
#' The arguments fall into groups:
#' \itemize{
#'   \item **The model**: `data`, `outcome`, `predictors` (a character vector = one model; a named
#'     list = several models to compare), `family` (usually detected), `wt` (survey weights).
#'   \item **What each cell shows**: `effect` (which contrast) x `measure` (which effect measure),
#'     `display` (the cell layout), `empirical` (crude vs adjusted effect).
#'   \item **Colors & significance**: `color`, `color_signif`, `stars`, `conf_level` --- as in [tab()].
#'   \item **Comparisons & structure**: `reference` (baseline levels), `tab_vars` (one table per
#'     group), `multiplier` (the unit a continuous predictor's effect is reported per — one standard
#'     deviation by default).
#'   \item **Survey design**: `wt` for a simple weight, or a prebuilt [survey::svydesign()] as `data`.
#'   \item **The footer**: `stats` --- one argument for everything the model-summary block shows:
#'     the goodness-of-fit rows, the model checks (linearity, dispersion, influence, collinearity,
#'     proportionality) and the model-comparison test (`"compare_baseline"` / `"compare_sequential"`).
#'   \item **Model checks**: `shape` (the cure for a non-linearity), and the plot
#'   [reg_check_plots()]. \item **Chart**: [forest_plot()] draws the finished table.
#' }
#'
#' `predictors` selects the mode: a **character vector** fits one model, and `outcome` may itself
#' be a vector -> one column per outcome; a **named list** of predictor sets fits one model each ->
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
#' optional dependencies.
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
#' formula** as `outcome` -- `tab_reg(data, y ~ x1 + poly(x2, 2) + x1:x3)` -- driving the model
#' directly; simple `y ~ a + b` formulas behave exactly like `outcome = "y"`, `predictors = c("a",
#' "b")`, while interactions / `poly()` / `I()` terms render as best-effort term rows.
#'
#' @param data A data frame, **or a prebuilt survey design** ([survey::svydesign()]). When a design
#'   is passed, its weights (and clustering / stratification / calibration) drive the estimation and
#'   `wt` is ignored. Replicate-weight ([survey::svrepdesign()]) and two-phase designs are refused at
#'   the boundary rather than approximated.
#' @param outcome Character outcome variable name(s), **or a model formula** (the escape hatch).
#'   With a `predictors` character vector, several names give one effect column per outcome; with a
#'   `predictors` list, a single name is required. A formula supplies its own model (leave
#'   `predictors` unset).
#' @param predictors Either a character vector of predictor names (one model), or a **named list**
#'   of character vectors (one model per element, its name labelling the column). Leave `NULL` when
#'   `outcome` is a formula.
#' @param family The model family, **resolved per outcome** so several outcomes with different
#'   families can share one table (one effect column-group each). `"auto"` (default) detects each
#'   outcome: a binary (-> `"binomial"`), an ordered 3+ level (-> `"ordinal"`), a nominal 3+ level
#'   (-> `"multinomial"`), or any other numeric (-> `"gaussian"`) outcome, emitting a message. An
#'   integer-valued numeric reads as `"gaussian"` too --- age in years, years of schooling, a summed
#'   score and income in whole units are all integers, and a linear model always fits --- with the
#'   message naming `"poisson"` for a genuine count. Set it explicitly with
#'   `"gaussian"` (linear), `"binomial"` (logistic), `"poisson"` / `"quasipoisson"` (counts),
#'   `"multinomial"` (nominal 3+ level), `"ordinal"` (ordered 3+ level). A **scalar** applies to every
#'   outcome; a **vector** aligned to `outcome`, or a **named** vector keyed by outcome
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
#'   package. Resolved **per outcome** like `family` (scalar / vector / named vector).
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
#'   Resolved **per outcome** like `family`. `effect` and `measure` are orthogonal: a *conditional*
#'   ratio is a different **fit**, a *marginal* one a different **estimator**, and both land on the
#'   same stored scale. Call [reg_measures()] on your outcome to see what it offers, with the reason
#'   whenever something is not available.
#'
#'   Two caveats worth knowing. A conditional risk ratio and a marginal one answer slightly different
#'   questions --- the marginal one is population-averaged and can never predict a probability above
#'   1 --- though on real data they agree closely; and a marginal estimate is standardized to the
#'   covariate distribution at hand, so under `tab_vars` each group standardizes to **its own**
#'   subpopulation.
#'
#'   \strong{How this is called elsewhere.} `measure = "ratio"` on a binary outcome is Stata's
#'   `binreg y x, rr`, `glm(family = binomial("log"))` or the modified Poisson, and
#'   `marginaleffects`' `comparison = "ratio"` on the marginal path; `measure = "log"` is
#'   `exponentiate = FALSE` in broom / parameters / gtsummary; `effect = "marginal"` is
#'   `avg_comparisons()`.
#' @param trials Grouped-binomial (summed-score) outcomes only. The number of items behind the score,
#'   fitting `cbind(score, trials - score)` as a binomial. `NULL` (default) fits an ordinary binary
#'   logit; a single integer (or a vector named by outcome) sets the item count; `TRUE`, or an `NA`
#'   entry in a named vector, uses that outcome's **observed maximum** score --- so explicit and
#'   automatic counts can be mixed, and an outcome with no score to take a maximum of (a factor, a
#'   0/1 numeric) simply stays an ordinary binary logit. Requires `family = "binomial"`. It is one
#'   count per *outcome*, never a column name --- a per-row item count is not supported; write the
#'   model with `cbind()` in a compound `formula` instead.
#' @param conf_level Confidence level for the intervals. Default `0.95`. It drives every interval in
#'   the table, the significance stars, and the greying under `color_signif` --- including the
#'   model-vs-observed gap interval, which is computed at print time from the stored standard error and
#'   is stored on each column so it follows this argument rather than
#'   `options("tabxplor.conf_level")`.
#' @param ci_method How the interval and p-value are computed --- the same argument, and the same
#'   named-vector grammar, as in [tab()], whose fifth slot is this producer's:
#'   `ci_method = c(model = "profile")`. On a regression there is only one interval to choose a
#'   method for, so a bare `"profile"` means that slot.
#'   `"wald"` (default) uses the Wald interval and the Wald z / t test: fast, matches standard
#'   software output, and the only option for weighted models. `"profile"` uses the
#'   profile-likelihood interval ([stats::confint()], needs `MASS`) and the likelihood-ratio test:
#'   more accurate near separation, unweighted binomial/poisson models only (else it falls back to
#'   Wald with a message; gaussian always uses the exact-t interval).
#' @param ref Optional named vector `c(var = "baseline level")` --- the same grammar as [tab()]'s
#'   `ref` --- choosing the treatment-contrast reference level of one or more factor **predictors**
#'   (the effect of every other level is measured against it), and of `tab_vars` (which group
#'   `color = "between_groups"` compares to). This is how factor contrasts are set; other contrast
#'   codings can be applied by passing a formula in `outcome` with the terms already coded.
#'   For the level of the **outcome**, see `outcome_level`: `ref` names the level you compare
#'   AGAINST, `outcome_level` the level you MODEL.
#' @param outcome_level Which level of the **outcome** to single out, as a named vector keyed by
#'   outcome name --- `outcome_level = c(married = "Married")` --- so several outcomes each get their
#'   own. It is the twin of `reference`, and the pair asks opposite questions:
#'   **`reference` names the level you compare AGAINST, `outcome_level` the level you MODEL.**
#'   \itemize{
#'     \item **binomial**: the level whose probability is estimated. It becomes the column header, and
#'       every odds ratio is the odds of *that* level. Defaults to the outcome's **first** level
#'       (so a coded factor like `"1-Married"` / `"2-Not married"` models "1-Married"). A 0/1 numeric
#'       outcome is labelled `"Not <outcome>"` / `"<outcome>"`, and either spelling --- or the raw
#'       `"0"` / `"1"` --- may be named.
#'     \item **multinomial**: the baseline category every other category's column is compared to.
#'       With more than two levels you cannot choose what is modelled (all of them are), only the
#'       pivot --- which is why the same argument means the opposite thing here. The rule is declared
#'       once, per family, in `REG_FAMILIES`.
#'     \item **ordinal, and any numeric outcome**: refused, with the reason. An ordinal outcome must
#'       keep the order of its levels, so none of them can be singled out.
#'   }
#' @param tab_vars Optional. Name of a grouping variable (character) --- the same argument as
#'   [tab()]'s `tab_vars`, and the same idea: one sub-table per group. The same model(s) are fitted
#'   **within each level** of this variable.
#'   When that leaves one column per group — a single outcome, a single set of predictors, and not a
#'   multinomial — the groups are pivoted into **side-by-side columns** for an easy across-group
#'   comparison; otherwise the per-group tables are stacked into one grouped table (grouped by
#'   `tab_vars`), sharing the variable/level stub. Call [tab_spread()] yourself for full control of
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
#'   outcomes, across compared models and across `tab_vars` groups. Names must be continuous
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
#' @param empirical Show the **observed, unadjusted (crude)** effect beside each modelled one ---
#'   the same quantity fitted with a single predictor. It IS the modelised quantity when there is
#'   only one predictor, so the distance between the two is exactly what adjustment changed, read
#'   left to right. `FALSE` (default) or `TRUE`; the two expert spellings say *where* the crude
#'   effect goes:
#'   \itemize{
#'     \item `TRUE` --- a crude **column** beside the model one, except on a 3+ level outcome
#'       (multinomial, or an ordinal marginal effect), where one model column would need one crude
#'       column per outcome category: there the crude value rides **inside** the model cell instead,
#'       as `1/1.63*** (1/1.69)`.
#'     \item `"column"` --- always the column, per outcome category if that is what it takes.
#'     \item `"cell"` --- always in the cell, however few columns it would have taken.
#'   }
#'   The two columns are the same column twice: same estimand, same colour ladder, same layout, one
#'   legend block. Each cell prints the effect with the level it sits on beside it --- the observed
#'   percentage or mean on the crude side, the **adjusted** prediction on the model side --- and the
#'   two effects end up adjacent, in the middle. Ask for a different layout with `display`.
#'
#'   The rule behind it is one sentence: *the observed effect is the model's own effect, fitted with
#'   a single predictor*. Where that univariable model is **saturated** (a categorical predictor
#'   under every family except ordinal) it has a closed form and is computed directly; otherwise it
#'   is a real fit, so the crude column shares the model's family, link, confidence-interval method
#'   and `multiplier` by construction. A **continuous** predictor has no levels, so its cell shows
#'   the effect alone --- the univariable slope, which assumes linearity on the model's scale (check
#'   that with `shape =` before trusting it) --- and its distribution goes to the html tooltip.
#'   The only outcomes with no crude counterpart at all are the compound-`formula` escape hatch
#'   (there is no predictor structure to be crude about) and, for a marginal contrast, a *weighted*
#'   3+ level outcome (no `marginaleffects` method).
#'
#'   Every crude quantity is computed on **exactly the same complete-case population as the model**
#'   (listwise-complete on the outcome, all predictors and any design variable), so crude and
#'   adjusted are comparable and not confounded by differing missingness --- reproduce it with
#'   [dplyr::filter()] + [tab()] on the same rows. Under `na = "drop_by_model"` a model fitted on
#'   rows the observed block does not cover gets **no** observed value at all, because the distance
#'   between two such estimates would be listwise deletion rather than adjustment.
#'
#'   Both columns are always on the **same inference basis**, which is the whole point of putting
#'   them side by side: weighted data gives both intervals that account for the weighting, and a
#'   `survey::svydesign` gives both the full design. Each column stores the base its own interval
#'   used in the `n_eff` [fmt()] field, while the displayed `n` stays the raw count.
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
#'   `tab(ci = "ref")` uses Newcombe; on a real table they differ by a few tenths of a percent.
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
#'   the **model checks that cost nothing** (see below). Pass a character vector to pick the statistics
#'   (`"n"`, `"lr_null"`, `"mcfadden_r2"`, `"aic"`, `"bic"`, `"phi"`, `"r2"`, `"r2_adj"`,
#'   `"f_model"`, `"sigma"`, `"global"`, `"interaction"`, `"linearity"`, `"proportionality"`,
#'   `"dispersion"`, `"influence"`, `"collinearity"`), `"all"` for **everything this model can
#'   report**, or `FALSE` / `"none"` to hide the footer entirely.
#'
#'   **Model comparison** (several models / outcomes only) is two more keys, so it needs no separate
#'   argument: `"compare_sequential"` tests each model against the previous one, and
#'   `"compare_baseline"` tests each against one reference model --- the first by default, or the one
#'   you name as the key's value, `stats = c("n", "aic", compare_baseline = "Model 1")` (an index
#'   works too, `compare_baseline = 2`). Both use a likelihood-ratio test (F for linear / quasi
#'   models, a design-based Wald test for weighted / survey models); when the models are not nested or
#'   are fitted on different numbers of observations, they fall back to the AIC difference with a
#'   message. A comparison key **adds** a row and restricts nothing, so
#'   `stats = "compare_sequential"` keeps the default statistics beside it; name the others too
#'   (`c("n", "aic", "compare_sequential")`) to narrow the block.
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
#' Five checks, in the order of what each one threatens --- the estimate, what the estimate means, its
#' interval, whether it is real at all, and why it is wide. Each is a footer row, so it travels into
#' every export, and each is named in `stats`.
#'
#' Three of them --- **Dispersion**, **Influence** and **Collinearity** --- are arithmetic on the model
#' already fitted, so they ride the default footer and cost nothing. The other two fit a model:
#' **Linearity** refits once per numeric predictor and **Proportionality** fits the Brant test's
#' auxiliary logits. Those two are therefore **asked for by name** ---
#' `stats = c("n", "aic", "linearity")`, or `stats = "all"` for every check this model allows.
#' The cheap answer to the same question is already on screen either way: the observed shape of each
#' numeric predictor is binned with no fit at all and drawn as the row's sparkline, and
#' [reg_check_plots()] draws the full diagnostic panel for **every** check, free of the footer.
#'
#' \describe{
#'   \item{**Linearity** (p-value, per numeric predictor; costs one model fit)}{Is this predictor's
#'     effect really one straight line? The model is refitted with that predictor's centred squared
#'     term and the two compared. A small p says one slope is the wrong summary --- and the damage is **not confined to
#'     that row**: on the model used throughout `vignette("tabxplor-reg")`, letting `age` curve moves
#'     the top income category's odds ratio by 24 % and flips another income level's conclusion at the
#'     5 % threshold.}
#'   \item{**Proportionality (Brant)** (p-value, ordinal outcomes; costs one model fit)}{Is one cumulative odds ratio enough
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
#'   `"interaction"` is different from the others: with `tab_vars`, it adds one **aggregated
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
#' @param display What each effect cell shows --- [tab()]'s display grammar, same names, same
#'   meaning, on every family and on the crude column as well as the model one. `NULL` (default)
#'   shows the plain estimate, or, with `empirical`, the estimate with the level it sits on beside
#'   it. The named layouts:
#'   * `"est"` --- the effect alone.
#'   * `"est_ci"` --- with a visible interval: `1/2.22 [1/2.47; 1/1.99]`.
#'   * `"est_base"` --- the effect, with the level beside it: `1/2.22 (32.8%)` on a logistic model,
#'     `-0.89 (2.25)` on a linear one. On a model column that level is the **adjusted** prediction;
#'     on a crude column it is the observed percentage or mean.
#'   * `"base_est"` --- the mirror, level first: `(32.8%) 1/2.22`. The effect stays the number the
#'     cell is about (it carries the stars and the colour); the bracket is the aside.
#'   * `"base"` --- the levels alone, still coloured and starred by the effect.
#'
#'   Or write a `{}` template: `"{est} (obs {obs})"` prints each adjusted effect next to the
#'   unadjusted one it is compared to, `"{est} ({gap})"` next to how far adjustment moved it (see
#'   `color = "adjustment"` below).
#'
#'   `display` is a **post-hoc** property: every quantity it can name is already stored, so choosing
#'   a layout never triggers a computation and never changes a number --- and [set_display()] on a
#'   built table gives the same result as asking for it here. It may show an **auxiliary** quantity
#'   of the same fit; it never changes the fit or the estimand, which is `measure`'s job alone.
#'   So `display = "est_base"` *adds* the adjusted prediction beside the odds ratio, while
#'   `effect = "marginal"` makes the whole column a marginal effect.
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
#'   * `"between_groups"` — with `tab_vars`, how far each group's effect sits from the **first**
#'     group's, on the same row: a per-predictor reading of effect modification, beside the global
#'     comparison a likelihood-ratio test gives. Pick the baseline group with `ref` keyed by the
#'     split variable (e.g. `ref = c(race = "Black")`). It also adds the **aggregated**
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
#'   * `"between_groups"` compares two `tab_vars` groups, which are **different people**, so the gap's
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
#' @param parallel Opt-in parallel build of the models of one call, using the (Suggests-only)
#'   \pkg{mirai} package: several `outcome`s, a `predictors` list, or the `tab_vars` groups.
#'   `NULL` (default) reads `getOption("tabxplor.parallel")` (off); `FALSE` forces serial; `TRUE`
#'   uses an auto worker count; an integer sets the number of worker processes. Byte-identical to
#'   the serial result. It pays off for **many, evenly sized** models against a survey-size data
#'   frame, and is a loss otherwise (the pool costs about a second to start, and two uneven models
#'   cannot gain much). One shape is always serial and says so when asked: a model comparison
#'   (`stats = "compare_*"`) is a test *between* the fits, so they are built together. The worker
#'   pool persists for the session; release it with [tab_parallel_stop()].
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one effect column per model / outcome.
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
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'           family = "binomial")
#'
#' \donttest{
#'   # linear (betas):
#'   tab_reg(reg_data, outcome = "tvhours", predictors = c("rincome", "age"),
#'           family = "gaussian")
#'   # to use normal R model formulas instead (same model, terser):
#'   tab_reg(reg_data, married ~ race + rincome, family = "binomial")
#'
#'  # logistic : comparison between observed odds-ratio and modelised odds-ratio
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'           family = "binomial", empirical = TRUE
#'   )
#' # average marginal effects + adjusted predictions (needs the marginaleffects package):
#' if (requireNamespace("marginaleffects", quietly = TRUE)) {
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "marginal")
#'   # the same contrast at the reference profile (others at their reference level / mean):
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "at_reference")
#'   # the same quantity as a RATIO: marginal risk ratios beside the crude ones. With a common
#'   # outcome this is what a reader means by "x times more likely" -- an odds ratio is not.
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "marginal", measure = "ratio", empirical = TRUE)
#' }
#' # the CONDITIONAL risk ratio: measure = "ratio" on a binary outcome fits the modified Poisson
#' # (Zou 2004), a log link with robust standard errors. Ask for the measure, not the distribution.
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'                 measure = "ratio", empirical = TRUE)
#' # ... and the risk DIFFERENCE, from an identity-link (additive-risk) fit:
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'                 measure = "difference")
#' # what this outcome can be modelled as, with the reason wherever it cannot:
#'   reg_measures(reg_data, "married")
#' # multinomial (nominal 3+ level): one OR column per outcome category vs the baseline
#'   tab_reg(reg_data, outcome = "party3", predictors = c("race", "age"),
#'                 family = "multinomial", outcome_level = c(party3 = "3-Republican"))
#' # ordinal (proportional-odds): one cumulative-OR column
#'   tab_reg(reg_data, outcome = "rincome", predictors = c("race", "age"), family = "ordinal")
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
#' @param ... Not a user argument. It carries the internal `.fit_cache` (the jamovi live UI's fit
#'   cache environment), and it is what makes every argument removed or renamed while `tab_reg()`
#'   was in development --- `exponentiate`, `at`, `estimate_display`, `dependent`, `split_var`,
#'   `reference`, `method`, `compare`, `baseline`, `inverse_two_level_factors`, and the `effect`
#'   values `"ame"` / `"ame_ratio"` --- give an error naming its replacement at the moment of the
#'   mistake, rather than R's bare "unused argument".
#' @eval reg_measures_rd()
#' @export
tab_reg <- function(data, outcome, predictors = NULL, tab_vars = NULL, wt = NULL,
                    family = "auto", effect = "coefficient", measure = "auto",
                    trials = NULL, empirical = FALSE, add_n = TRUE,
                    color = TRUE, color_signif = NULL, stars = TRUE,
                    conf_level = NULL, ci_method = NULL,
                    outcome_level = NULL, ref = NULL,
                    multiplier = "sd", shape = NULL, stats = NULL,
                    na = c("drop_by_outcome", "drop_by_model", "drop_all"),
                    display = NULL, cleannames = NULL, subtext = "", parallel = NULL, ...) {
  # `.fit_cache` (the jamovi live-UI cache env) and `.levels_collapse` (the level-merge spec, shared
  # with tab() -- R/row-model.R declares it, tab_collapse_levels() applies it in reg_prepare_data()'s
  # stage G beside `shape`) are jamovi-internal and ride `...`; neither is a user argument.
  .dots      <- list(...)
  .fit_cache <- .dots[[".fit_cache"]]
  .levels_collapse <- new_lvl_collapse(.dots[[".levels_collapse"]])
  # One dots-validator for both producers (Phase 20j): every declared formal is a known name, a
  # dot-prefixed one is internal plumbing, and anything else aborts with a suggestion. tab_reg() is
  # unreleased, so a removed spelling is simply an unknown argument -- no retired-name table to carry.
  tab_check_dots(.dots, "tab_reg")
  # Phase 20c (KEY 4): ONE `ci_method` grammar for both producers -- the named vector `tab()` takes,
  # whose fifth slot is the regression's own. resolve_ci_method() validates every slot against
  # CI_METHODS and returns the full vector; only `model` is read here. A bare "profile" is accepted
  # as that slot, because on this producer there is only one interval to choose a method for.
  if (is.character(ci_method) && is.null(names(ci_method)) && length(ci_method) == 1L)
    ci_method <- stats::setNames(ci_method, "model")
  method <- resolve_ci_method(ci_method, fn = "tab_reg")[["model"]]
  # Phase 18z13: the un-supplied default is the whole vector, so its length IS "the user did not
  # choose" -- read before match.arg collapses it. The `na` advice messages fire on a CHOICE, never on
  # the default (which is the very thing they would advise).
  na_explicit <- length(na) == 1L
  na      <- match.arg(na)
  # Fallback FALSE matches .onLoad's default and tab()'s read sites (the option is always set to FALSE
  # on load, so this only bites if someone unsets it; TRUE here was an inconsistency, not an intent).
  cleannames <- resolve_cleannames(cleannames)


  # Phase 14u (K): a LIST of models AND SEVERAL outcomes -> one model-comparison table per outcome,
  # returned as a `tabxplor_tabs` list (so tab_export("xl") writes one sheet per outcome). Loop the
  # outcomes on the outside; each iteration is the ordinary single-outcome comparison (recursion,
  # so every arg / message / family-detect is reused). `trials` is per-outcome (a vector or a named
  # vector), split here. Placed BEFORE the design extraction so a survey design recurses intact.
  #
  # ⚠ Phase 19m-ii: `!rlang::is_formula(outcome)` is load-bearing. A two-sided formula is a CALL of
  # length 3 (`~`, lhs, rhs), so `length(outcome) > 1L` was TRUE for every one of them: a
  # `tab_reg(d, y ~ x, list(m1 = "a"))` recursed three times over `` `~` ``, `y` and `x`, and each
  # child died on the internal `stopifnot(is.character(outcome))` below -- while the teachable
  # message written for exactly that mistake ("Provide either a formula in `outcome` or
  # `predictors`, not both") sat unreachable in the formula block.
  if (!rlang::is_formula(outcome) && is.list(predictors) &&
      !inherits(predictors, "formula") && length(outcome) > 1L) {
    if (!is.null(trials) && !isTRUE(trials) && is.null(names(trials)) &&
        length(trials) > 1L && length(trials) != length(outcome)) {
      cli::cli_abort(c("{.arg trials} must be length 1, one per outcome, or a named vector.",
                       "x" = "Got {length(trials)} for {length(outcome)} outcomes."))
    }
    # Phase 19e (D6): every per-outcome argument is SLICED the way `trials` is, and every
    # whole-call one is forwarded. Before, the per-call options and `.fit_cache` were silently
    # dropped (so the jamovi cache never filled), and
    # a POSITIONAL `family` vector was passed whole to each recursion -- where its first entry then
    # became every outcome's family. `reg_per_outcome()` is the one slicer, shared by the four.
    # Phase 20f-iii (axis R): the per-outcome argument list is built here and the recursion itself is
    # reg_build_outcome(), a namespaced worker, so this map IS tab_pmap(). A unit returns a FINISHED
    # table (its fit_spec is ~4 KB of strings) and there is no cross-unit step at all, which makes
    # this the cleanest of the three axes -- and, measured, a 1.82x ceiling on two outcomes x a
    # models list at survey scale (dev/tabxplor_reg_performance.md 6.2).
    # ⚠ `parallel = FALSE` inside the unit: THE NESTING RULE, stated once in tab_pmap()'s
    # everywhere() block, where it is also enforced for any site that forgets it.
    args <- purrr::map(seq_along(outcome), function(i) {
      d   <- outcome[[i]]
      tri <- if (is.null(trials) || isTRUE(trials)) trials
             else if (!is.null(names(trials)))      unname(trials[d])
             else if (length(trials) == 1L)         as.numeric(trials)
             else                                   trials[[i]]
      list(outcome = d, predictors = predictors, wt = wt,
           family  = reg_per_outcome(family,  d, i, "auto"),
           effect  = reg_per_outcome(effect,  d, i, "coefficient"),
           measure = reg_per_outcome(measure, d, i, "auto"),
           trials = tri, conf_level = conf_level, ci_method = ci_method,
           ref = ref, outcome_level = outcome_level,
           tab_vars = tab_vars, multiplier = multiplier, shape = shape,
           empirical = empirical, add_n = add_n,
           stats = stats,
           display = display, color = color, color_signif = color_signif,
           stars = stars, na = na, cleannames = cleannames, subtext = subtext,
           parallel = FALSE, .fit_cache = .fit_cache,
           .levels_collapse = .levels_collapse)
    })
    tabs <- tab_pmap(list(args = args), "reg_build_outcome", .ship = list(data = data),
                     .names = outcome,
                     workers = tab_parallel_workers(parallel, .fit_cache))
    names(tabs) <- outcome
    return(new_tabxplor_tabs(tabs))
  }

  # Phase 19m-ii: THE argument boundary, in one call (R/reg-resolve.R). Six stages -- the pure
  # checks, everything that touches `data`, the per-outcome estimand table, what the table shows,
  # the fit plan, the specs -- in the one order they may run in, with every ordering constraint
  # stated there rather than implied by 738 lines of sequence here.
  a <- reg_resolve_args(
    data, outcome, predictors, tab_vars = tab_vars, wt = wt,
    family = family, effect = effect, measure = measure, trials = trials,
    empirical = empirical, add_n = add_n, color = color, color_signif = color_signif,
    stars = stars, conf_level = conf_level, method = method, ref = ref,
    outcome_level = outcome_level, multiplier = multiplier,
    shape = shape, stats = stats,
    na = na, na_explicit = na_explicit, display = display, cleannames = cleannames,
    subtext = subtext, .fit_cache = .fit_cache, levels_collapse = .levels_collapse)

  res <- reg_build(a$data, a$specs, a$shared, tab_vars = tab_vars,
                   .fit_cache = .fit_cache, ref = ref, reref = a$reref, parallel = parallel)

  # stars = TRUE (default) for regression tables -- the per-cell pvalue is stored by reg_build so the
  # main display shows significance stars. stars = FALSE strips it (pvalue is stars-only; colours read
  # the CI bounds), so the table renders without stars.
  if (!isTRUE(stars)) {
    for (nm in names(res)[vapply(res, is_fmt, logical(1))]) {
      res[[nm]] <- set_pvalue(res[[nm]], NA_real_)
    }
  }

  # Phase 14w / 15e: the table's own model record (drives the reg title / caption, the "Model:" footer
  # lines, and the colour legend). `families` is per outcome (the mixed-family case); the per-column
  # effect word is read from the column's own `model_family` fmt attribute (Step D) in the legend, so
  # this record is only the table-level narrative.
  # Phase 19e: the record stores the ESTIMAND -- `measures` beside `families`, per outcome, and the
  # resolved `effect` -- because a table must remember what it estimated or a refit silently changes
  # it. `exponentiate` / `at` / `do_exp` left with the arguments they mirrored; `eff_word` stays as
  # the table-level narrative's scalar.
  # Phase 19m-ii: every field is READ OFF the boundary's record. `positive_level` and `wt_disp` were
  # recomputed here from four closures and the raw `wt` -- the positive level twice in one function,
  # 200 lines apart, both of which had to see the post-relevel data.
  reg_call_record <- list(
    family = a$families[[1]], families = a$families,
    effect = a$est$effect, measure = a$est$measure, eff_word = a$eff_word,
    measures = vapply(a$ests, function(e) e$measure, character(1)),
    effects  = vapply(a$ests, function(e) e$effect,  character(1)),
    outcome = a$outcome, positive_level = a$positive_levels, predictors = a$union_predictors,
    # Phase 18z9: the predictor-kind map is STORED, not re-derived from the rendered table. Nothing
    # recorded it before, and the only implicit marker (a numeric row's `level == var`) is already
    # broken by `cleannames` and by the multiplier relabel. `multiplier` records the RESOLVED per-unit
    # scaling actually used (the frozen SDs included), so the footer/legend can name the unit.
    predictor_types = reg_predictor_types(a$data, a$union_predictors), multiplier = a$multiplier,
    # Phase 18z15: THE recipe reg_check_plots() refits from -- the specs plus the handful of scalars
    # reg_fit() takes, ~4 KB of strings. Deliberately NOT the fits themselves: ~10 MB each was the
    # measured cause of the Phase-o jamovi freeze, and a 60 ms refit through the very fitter the table
    # came from is both cheaper and impossible to drift from.
    # ⚠ the RESOLVED conf_level, read back off the boundary record: since 20c the formal is NULL on
    # every producer and reg_resolve_args() resolves it, so tab_reg()'s own local is still NULL here.
    fit_spec = list(specs = a$specs, method = method, conf_level = a$shared$conf_level,
                    outcome_level = outcome_level,
                    na_shared_vars = a$na_shared_vars, shape_terms = a$shape_terms,
                    multiplier = a$multiplier, effect = a$est$effect, measure = a$est$measure,
                    wt = a$wt_disp, design_vars = reg_design_vars(a$design_spec)),
    # which observed counterpart each outcome has (NA = none), and WHERE it went. Stored, so the
    # footer can word the in-cell bracket and ?tab_reg can state the scope honestly.
    emp_mode = a$empirical,
    crude_keys = if (emp_on(a$empirical))
      stats::setNames(purrr::map_chr(a$specs, ~ .$crude_key), purrr::map_chr(a$specs, "outcome"))
      else stats::setNames(rep(NA_character_, length(a$specs)),
                           purrr::map_chr(a$specs, "outcome")),
    tab_vars = tab_vars, comparison = a$is_comparison, wt = a$wt_disp
  )
  # Phase 19g (KEY 6): the model record IS this table's `spec$call` -- "how was this table made",
  # the slot every producer has, rather than a regression-only sibling of `meta$vars`. `conf_level`
  # left it here: it was a stale duplicate of a per-COLUMN attribute (tab_stamp_inference stamps the
  # level on every column, and get_conf_level() is what every consumer reads), so keeping a
  # table-wide copy could only ever disagree with the columns it described.
  set_reg_call(res, reg_call_record)
}


# Phase 20a: `tab_logit()` and `multi_logit()` are DELETED. They were thin forwarders to
# `tab_reg(family = "binomial")` that mirrored only ~20 of its formals, so a user who found them
# could not reach `effect = "marginal"`, `measure = "ratio"`, `compare =`, `baseline =`,
# `reference =` or `color =` -- a capability hole created purely by the mirror, plus 523 lines of
# `man/`. Neither was ever released (absent from CRAN 1.3.1), so there is nothing to deprecate.
#   tab_logit(data, outcome, predictors, ...)  ->  tab_reg(..., family = "binomial")
#   multi_logit(data, outcome, models, ...)    ->  tab_reg(predictors = <named list>,
#                                                            family = "binomial")
