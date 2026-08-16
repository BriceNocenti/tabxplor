# =====================================================================================================
# R/reg-estimand.R -- WHAT A REGRESSION COLUMN ESTIMATES (Phase 19e, KEY 8b + KEY 3a)
# =====================================================================================================
# PURPOSE: one declared library mapping the user's TWO questions --
#
#     effect  = which CONTRAST   ("coefficient" | "marginal" | "at_reference")
#     measure = which MEASURE    ("odds_ratio" | "ratio" | "difference" | "log")
#
# -- onto everything downstream needs: which model to FIT, whether to exponentiate, the column
# header, the `EST_SCALES` row stamped on the column, which crude companion pairs with it, which
# `marginaleffects` contrast to ask for, and the estimand phrase of the "Model:" footer line.
#
# WHY IT EXISTS. Before it, that one decision was spread over FOUR arguments
# (`family` x `effect` x `at` x `exponentiate`) = 36 combinations for 9 distinct estimands, with
# ~19 cells in which an argument was silently ignored: `exponentiate` was a no-op on the whole
# marginal path, `at = "reference"` was degraded away in three separate blocks, and a RISK RATIO
# could only be obtained by naming the wrong distribution (`family = "poisson"` on a binary
# outcome). The knowledge was real but it lived in two nested switches (`reg_effect_word`,
# `reg_model_note`), a dispatch (`reg_crude_shape`), three degrade blocks and two aborts.
#
# THE DIVERGENCE THIS FILE ENCODES (KEY 8, and it must never be re-collapsed). On a crosstab every
# geometry is a function of the SAME sufficient statistics, so `tab(color =)` asking for one is a
# SELECTION. On a regression a geometry is a different FIT or a different ESTIMATOR -- an odds ratio
# is a logit fit, a conditional risk ratio a log-link one, a risk difference an identity-link one, a
# marginal risk ratio a g-computation over the logit fit. So on `tab_reg()` it is a MODELLING
# DECISION and lives in an argument: *changing `display` must never change the model.*
#
# THE VOCABULARY IS SHARED WITH `tab()`. `measure`'s values ARE `EST_SCALES$geometry`
# ("ratio" / "difference" / "log"), which is what `tab(color =)` resolves into as well -- so the
# argument that asks, the attribute that stores, the legend that names and the forest-plot axis that
# draws are one vocabulary end to end (R/fmt_class.R, SECTION "the ESTIMATE's scale").
#
#   the argument names the GEOMETRY; the attribute names the ROW.
#
# THE THREE STATES. A user must be able to tell "we don't offer that" from "that cannot be done"
# from "it did not converge on your data" -- before this file the first two produced the same abort:
#   * a row with status "ok"          -> build it;
#   * a row with status "impossible"  -> abort with its own `why` (an odds ratio of a continuous
#                                        outcome is not a thing, whatever we implement);
#   * NO ROW                          -> "not offered", the message ENUMERATING what this outcome
#                                        does offer, generated from the table itself;
#   * and at runtime, the fit that did not converge (reg_fit()), which names the alternatives.
#
# FOUR CONSUMERS, ONE TABLE (the standing rule: never a second hand-written list):
#   1. the boundary resolver in tab_reg()          -> reg_estimand()
#   2. the error message                            -> reg_estimand_abort()
#   3. the user-callable lister                     -> reg_measures()
#   4. the generated `?tab_reg` section (and, in Phase 19k, the jamovi eligibility rule)
#                                                   -> reg_estimands_for()
#
# ALSO HERE (Phase 19m-i): **REG_FAMILIES** -- what each model family is CALLED, and where it may be
# named. Four name tables and a switch before, in two files, already disagreeing: the footer sentence
# (`reg_family_display_name`), the Excel filename tag (`reg_family_short`) and the two picker labels
# (`REG_FAMILY_UI_LABEL` / `_BINARY`, which silently omitted quasipoisson / rr / rd / mr). `ui = NA`
# IS the fact "not offered in the picker" -- which dev/generate_jamovi_js.R used to write a second
# time as a hardcoded setdiff(). `REG_FIT_FAMILY` and `REG_FAMILY_MULT_WORD` are DERIVED from it and
# from REG_ESTIMANDS, the latter with a build-time singleton assert.
# =====================================================================================================


# --- the measure vocabulary --------------------------------------------------------------------------
#
# THREE base geometries + `log`, which is NOT a peer: it is the same fit, un-exponentiated (exactly
# what `exponentiate = FALSE` meant). `measure = "log"` therefore resolves to the family's DEFAULT
# estimand on its link scale; the precise spellings `log_odds` / `log_risk` / `log_rate` additionally
# PIN which base, so a modified-Poisson fit can be shown logged without a second argument.
# The acronyms are permanent aliases, never deprecated: the argument teaches the concept word
# ("ratio"), the column header keeps the discipline's ("RR" / "IRR" / "RoM"), so the table prints the
# mapping between the two every time it renders.
#' @keywords internal
#' @noRd
REG_MEASURE_ALIASES <- c(
  odds_ratio = "odds_ratio", or = "odds_ratio", OR = "odds_ratio",
  ratio = "ratio", rr = "ratio", RR = "ratio", irr = "ratio", IRR = "ratio",
  mr = "ratio", MR = "ratio", RoM = "ratio", risk_ratio = "ratio", rate_ratio = "ratio",
  difference = "difference", diff = "difference", rd = "difference", RD = "difference",
  log = "log", log_odds = "log", log_risk = "log", log_rate = "log", log_ratio = "log",
  auto = "auto"
)

# The BASE a `log_*` spelling pins ("" = the family's default estimand, i.e. bare "log").
#' @keywords internal
#' @noRd
REG_LOG_BASE <- c(log = "", log_odds = "odds_ratio", log_risk = "ratio",
                  log_rate = "ratio", log_ratio = "ratio")

# reg_measure_key() -- one spelling -> (measure, log_base). The twin of measure_key() on the colour
# side. Returns NULL for an unknown spelling, so the caller aborts naming the argument.
#' @keywords internal
#' @noRd
reg_measure_key <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(list(measure = "auto", log_base = ""))
  x   <- as.character(x)
  key <- unname(REG_MEASURE_ALIASES[x])
  if (is.na(key)) key <- unname(REG_MEASURE_ALIASES[tolower(x)])
  if (is.na(key)) return(NULL)
  base <- if (identical(key, "log")) unname(REG_LOG_BASE[tolower(x)]) else ""
  list(measure = key, log_base = if (is.na(base)) "" else base)
}

#' @keywords internal
#' @noRd
REG_MEASURES_VALUES <- c("auto", "odds_ratio", "ratio", "difference", "log")
#' @keywords internal
#' @noRd
REG_EFFECTS_VALUES  <- c("coefficient", "marginal", "at_reference")


# --- the library -------------------------------------------------------------------------------------
#
# ONE ROW per (family, effect, measure) the package can answer, plus the rows that state why a
# combination CANNOT be answered. Columns:
#
#   builder      which of reg_build()'s three column builders runs: "coef" | "ame" | "vsrest".
#                It replaces the table-scalar `if` that used to choose them, so a mixed-family table
#                picks per spec (Phase 19g made the builders per-spec; this makes the CHOICE so too).
#   fit          the internal family key handed to reg_fit(). It is where a geometry becomes a
#                different MODEL: "rr" = modified Poisson (a conditional risk ratio), "rd" =
#                identity link (a risk difference), "mr" = log-link pseudo-ML (a ratio of means).
#   exp          exponentiate the tidy estimate (the old `exponentiate`, now derived).
#   word         the column header's effect word -- "OR" / "IRR" / "RR" / "RoM" / "RD" / beta /
#                "AME" / "MER". `reg_effect_word()`'s four-argument nested switch IS this column.
#   scale        the EST_SCALES key stamped on the column (KEY 2). Its `est_field` says which fmt
#                field the estimate is written into, so a scale change needs no builder change.
#   display      the per-cell display token the column is built with.
#   crude_fam    which REG_EMPIRICAL block the observed companion comes from; "auto" =
#                reg_crude_key(fit, trials), which is what carries `trials` -> grouped_binomial.
#   crude_shape  which shape row inside it. `reg_crude_shape()`'s dispatch -- including its
#                cross-family borrow (a binary marginal RATIO reuses REG_EMPIRICAL$rr$rr) -- IS
#                these two columns.
#   comparison   the marginaleffects `comparison =` value (NA = the additive default).
#   engine       WHICH ENGINE computes this row's marginal quantities (Phase 20d):
#                "gcomp"          -- tabxplor's own g-computation: one counterfactual sweep giving the
#                                    estimate, the adjusted predictions and an ANALYTIC jacobian, whose
#                                    delta-method interval reproduces marginaleffects to 1e-8 (measured,
#                                    glm and weighted svyglm alike) at ~25x the speed;
#                "marginaleffects"-- the numerical-jacobian route;
#                "auto"           -- resolve by the rule below (the `crude_fam = "auto"` idiom).
#                THE RULE, stated once in reg_marginal_engine(): everything but `at_reference`, whose
#                contrast lives on a one-row profile grid that g-computation does not build (and which
#                costs 2.4 s, not 45). It is a PERMISSION, not a promise: the producer returns NULL
#                rather than a wrong number, and reg_marginal() then falls back for the WHOLE call --
#                never a per-contrast mix, so one column always carries one convention.
#   needs        a Suggests package this cell requires ("" = none).
#   obs          may an `obs` (crude) value be attached cell by cell? FALSE at the reference profile,
#                where the model is conditional and the observed columns stay marginal.
#   status       "ok" | "impossible"; ABSENT from the table = "not offered".
#   why          for "impossible": a closure returning the reason (gettext at render, statically
#                extractable -- the MEASURES$word pattern; a top-level gettext() would freeze the
#                build locale).
#   note         a closure returning the estimand phrase of the "Model:" footer line.
#                `reg_model_note()`'s six arms x `do_exp` ARE this column.
#
# WARNING: the msgids in `why` / `note` are the ones `po/R-fr.po` already carries wherever the
# phrase existed before -- do not re-word them in passing, or the French legend silently reverts to
# English.
# The two vocabularies the columns above are keyed on. `builder` names one of reg_build()'s three
# column builders -- declared here, beside the column that chooses it, and policed BOTH ways by a
# foreign key (R/zzz-fact-keys.R): tab_reg.R's dispatch had a silent fall-through arm, so a typo'd
# builder quietly built a coefficient column.
#' @keywords internal
#' @noRd
REG_BUILDERS <- c("coef", "ame", "vsrest")

#' @keywords internal
#' @noRd
REG_MARGINAL_ENGINES <- c("gcomp", "marginaleffects")

# ⚠ a new column goes in the DEFAULTED TAIL: the first eight arguments are passed positionally at
# every one of the 36 call sites below, so a column inserted earlier shifts all of them in silence.
#' @keywords internal
#' @noRd
est_row <- function(effect, measure, builder, fit, exp, word, scale, display,
                    crude_fam = "auto", crude_shape = NA_character_, comparison = NA_character_,
                    needs = "", obs = TRUE, engine = "auto", status = "ok", why = NULL, note = NULL) {
  list(effect = effect, measure = measure, builder = builder, fit = fit, exp = exp, word = word,
       scale = scale, display = display, crude_fam = crude_fam, crude_shape = crude_shape,
       comparison = comparison, needs = needs, obs = obs, engine = engine,
       status = status, why = why, note = note)
}

# reg_marginal_engine() -- THE rule `engine = "auto"` resolves to, and the ONE reader of the column.
#' @keywords internal
#' @noRd
reg_marginal_engine <- function(est) {
  e <- est$engine %||% "auto"
  if (!identical(e, "auto")) return(e)
  if (identical(est$effect, "at_reference")) "marginaleffects" else "gcomp"
}

# The three phrases the MARGINAL rows share, keyed by what the response scale is. Written once here
# rather than per row: they differ only in the quantity's name, and the "where" clause is the
# effect's, not the family's.
#' @keywords internal
#' @noRd
est_note_marginal <- function(kind, at_ref = FALSE, ratio = FALSE) {
  function() {
    where <- if (at_ref)
      gettext(" at the reference profile (other predictors held at their reference level / mean)")
    else gettext(" (sample-averaged)")
    head <- if (ratio && identical(kind, "prob"))
      gettext("marginal risk ratios (the ratio of adjusted predicted probabilities)")
    else if (ratio)
      gettext("marginal ratios (the ratio of adjusted predicted values)")
    else if (identical(kind, "prob"))
      gettext("marginal effects on the probability scale (percentage points)")
    else gettext("marginal effects on the response scale")
    paste0(head, where)
  }
}

# The per-family estimand library. Read it as: "for THIS outcome family, what can each
# (contrast x measure) pair be?". `default` names the measure `measure = "auto"` resolves to, and is
# also the base `measure = "log"` logs.
#' @keywords internal
#' @noRd
REG_ESTIMANDS <- list(

  # ---- gaussian ---------------------------------------------------------------------------------
  gaussian = list(
    default = c(coefficient = "difference", marginal = "difference", at_reference = "difference"),
    rows = list(
      est_row("coefficient", "difference", "coef", "gaussian", FALSE, "\u03b2", "raw_diff", "coef",
              crude_shape = "diff",
              note = function() gettext("coefficients (mean difference vs the reference category)")),
      # Phase 19e -- the capability gap `tab()` never had: a RATIO OF MEANS. Poisson pseudo-ML with
      # robust standard errors (Santos Silva & Tenreyro 2006), i.e. exactly the "rr" route one
      # family over: a deliberately misspecified log-link likelihood whose sandwich variance is the
      # honest one. tabxplor already owned the mean_ratio scale, its ladder and three ci_mean_ratio
      # engines -- only tab_reg() refused.
      est_row("coefficient", "ratio", "coef", "mr", TRUE, "RoM", "mean_ratio", "ratio",
              crude_fam = "mr", crude_shape = "mr",
              note = function() gettext("ratios of adjusted means (vs the reference category)")),
      est_row("coefficient", "log", "coef", "mr", FALSE, "\u03b2", "log_coef", "coef",
              crude_fam = "mr", crude_shape = "mr_log",
              note = function() gettext("log-mean coefficients (vs the reference category)")),
      est_row("coefficient", "odds_ratio", "coef", "gaussian", TRUE, "OR", "odds_ratio", "or",
              status = "impossible",
              why = function() gettext("an odds ratio needs a probability to take the odds of; this outcome is continuous")),
      est_row("marginal", "difference", "ame", "gaussian", FALSE, "AME", "raw_diff", "coef",
              crude_shape = "diff", needs = "marginaleffects",
              note = est_note_marginal("raw")),
      est_row("marginal", "ratio", "ame", "gaussian", TRUE, "RoM", "mean_ratio", "ratio",
              crude_fam = "mr", crude_shape = "mr", comparison = "lnratioavg",
              needs = "marginaleffects", note = est_note_marginal("raw", ratio = TRUE)),
      est_row("at_reference", "difference", "ame", "gaussian", FALSE, "MER", "raw_diff", "coef",
              crude_shape = "diff", needs = "marginaleffects", obs = FALSE,
              note = est_note_marginal("raw", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "gaussian", TRUE, "RoM", "mean_ratio", "ratio",
              crude_fam = "mr", crude_shape = "mr", comparison = "lnratioavg",
              needs = "marginaleffects", obs = FALSE,
              note = est_note_marginal("raw", at_ref = TRUE, ratio = TRUE))
    )),

  # ---- binomial ---------------------------------------------------------------------------------
  binomial = list(
    default = c(coefficient = "odds_ratio", marginal = "difference", at_reference = "difference"),
    rows = list(
      est_row("coefficient", "odds_ratio", "coef", "binomial", TRUE, "OR", "odds_ratio", "or",
              crude_shape = "or",
              note = function() gettext("odds ratios (vs the reference category)")),
      est_row("coefficient", "log", "coef", "binomial", FALSE, "\u03b2", "log_coef", "coef",
              crude_shape = "or_log",
              note = function() gettext("log-odds coefficients (vs the reference category)")),
      # the modified Poisson (Zou 2004) -- reachable by NAME at last. It used to require typing
      # `family = "poisson"` on a binary outcome, which is the wrong distribution said out loud.
      est_row("coefficient", "ratio", "coef", "rr", TRUE, "RR", "odds_ratio", "or",
              crude_fam = "rr", crude_shape = "rr",
              note = function() gettext("risk ratios (vs the reference category)")),
      # Phase 19e -- the second capability gap: the additive-risk (identity-link) model.
      est_row("coefficient", "difference", "coef", "rd", FALSE, "RD", "points", "diff",
              crude_shape = "ame",
              note = function() gettext("risk differences (percentage points vs the reference category)")),
      est_row("marginal", "difference", "ame", "binomial", FALSE, "AME", "points", "diff",
              crude_shape = "ame", needs = "marginaleffects",
              note = est_note_marginal("prob")),
      est_row("marginal", "ratio", "ame", "binomial", TRUE, "RR", "odds_ratio", "or",
              crude_fam = "rr", crude_shape = "rr", comparison = "lnratioavg",
              needs = "marginaleffects", note = est_note_marginal("prob", ratio = TRUE)),
      est_row("at_reference", "difference", "ame", "binomial", FALSE, "MER", "points", "diff",
              crude_shape = "ame", needs = "marginaleffects", obs = FALSE,
              note = est_note_marginal("prob", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "binomial", TRUE, "RR", "odds_ratio", "or",
              crude_fam = "rr", crude_shape = "rr", comparison = "lnratioavg",
              needs = "marginaleffects", obs = FALSE,
              note = est_note_marginal("prob", at_ref = TRUE, ratio = TRUE))
    )),

  # ---- poisson / quasipoisson (counts) ----------------------------------------------------------
  poisson = list(
    default = c(coefficient = "ratio", marginal = "difference", at_reference = "difference"),
    rows = list(
      est_row("coefficient", "ratio", "coef", "poisson", TRUE, "IRR", "odds_ratio", "or",
              crude_shape = "irr",
              note = function() gettext("incidence-rate ratios (vs the reference category)")),
      est_row("coefficient", "log", "coef", "poisson", FALSE, "\u03b2", "log_coef", "coef",
              crude_shape = "irr_log",
              note = function() gettext("log-rate coefficients (vs the reference category)")),
      est_row("coefficient", "odds_ratio", "coef", "poisson", TRUE, "OR", "odds_ratio", "or",
              status = "impossible",
              why = function() gettext("an odds ratio needs a probability to take the odds of; this outcome is a count")),
      # a poisson AME is ADDITIVE while its crude companion stays a rate RATIO: the crude shape is
      # deliberately the coefficient one, and reg_same_estimand() then (rightly) refuses to pair
      # them. That fall-through used to live inside reg_crude_shape(); it is data now.
      est_row("marginal", "difference", "ame", "poisson", FALSE, "AME", "raw_diff", "coef",
              crude_shape = "irr", needs = "marginaleffects", note = est_note_marginal("raw")),
      est_row("marginal", "ratio", "ame", "poisson", TRUE, "RoM", "mean_ratio", "ratio",
              crude_shape = "irr", comparison = "lnratioavg", needs = "marginaleffects",
              note = est_note_marginal("raw", ratio = TRUE)),
      est_row("at_reference", "difference", "ame", "poisson", FALSE, "MER", "raw_diff", "coef",
              crude_shape = "irr", needs = "marginaleffects", obs = FALSE,
              note = est_note_marginal("raw", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "poisson", TRUE, "RoM", "mean_ratio", "ratio",
              crude_shape = "irr", comparison = "lnratioavg", needs = "marginaleffects", obs = FALSE,
              note = est_note_marginal("raw", at_ref = TRUE, ratio = TRUE))
    )),

  # ---- multinomial ------------------------------------------------------------------------------
  multinomial = list(
    default = c(coefficient = "odds_ratio", marginal = "difference", at_reference = "odds_ratio"),
    rows = list(
      est_row("coefficient", "odds_ratio", "coef", "multinomial", TRUE, "OR", "odds_ratio", "or",
              crude_shape = "or",
              note = function() gettext("odds ratios (each category vs the reference)")),
      est_row("coefficient", "log", "coef", "multinomial", FALSE, "\u03b2", "log_coef", "coef",
              crude_shape = "or_log",
              note = function() gettext("log-odds coefficients (each category vs the reference)")),
      est_row("marginal", "difference", "ame", "multinomial", FALSE, "AME", "points", "diff",
              crude_shape = "ame", needs = "marginaleffects", note = est_note_marginal("prob")),
      est_row("marginal", "ratio", "ame", "multinomial", TRUE, "RR", "odds_ratio", "or",
              crude_shape = "ame_ratio", comparison = "lnratioavg", needs = "marginaleffects",
              note = est_note_marginal("prob", ratio = TRUE)),
      # the one cell whose BUILDER is neither coefficient nor marginal: at the reference profile a
      # multinomial coefficient becomes the odds ratio of each category VERSUS THE REST.
      est_row("at_reference", "odds_ratio", "vsrest", "multinomial", TRUE, "OR", "odds_ratio", "or",
              crude_shape = "or", comparison = "lnor", needs = "marginaleffects", obs = FALSE,
              note = function() gettext("odds ratios of each outcome category versus the rest, at the reference profile (other predictors held at their reference level / mean); profile-conditional")),
      est_row("at_reference", "difference", "ame", "multinomial", FALSE, "MER", "points", "diff",
              crude_shape = "ame", needs = "marginaleffects", obs = FALSE,
              note = est_note_marginal("prob", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "multinomial", TRUE, "RR", "odds_ratio", "or",
              crude_shape = "ame_ratio", comparison = "lnratioavg", needs = "marginaleffects",
              obs = FALSE, note = est_note_marginal("prob", at_ref = TRUE, ratio = TRUE))
    )),

  # ---- ordinal ----------------------------------------------------------------------------------
  ordinal = list(
    default = c(coefficient = "odds_ratio", marginal = "difference", at_reference = "difference"),
    rows = list(
      est_row("coefficient", "odds_ratio", "coef", "ordinal", TRUE, "OR", "odds_ratio", "or",
              crude_shape = "cumor",
              note = function() gettext("cumulative odds ratios (proportional-odds model)")),
      est_row("coefficient", "log", "coef", "ordinal", FALSE, "\u03b2", "log_coef", "coef",
              crude_shape = "cumor_log",
              note = function() gettext("proportional-odds model (log-odds coefficients)")),
      est_row("marginal", "difference", "ame", "ordinal", FALSE, "AME", "points", "diff",
              crude_shape = "ame", needs = "marginaleffects", note = est_note_marginal("prob")),
      est_row("marginal", "ratio", "ame", "ordinal", TRUE, "RR", "odds_ratio", "or",
              crude_shape = "ame_ratio", comparison = "lnratioavg", needs = "marginaleffects",
              note = est_note_marginal("prob", ratio = TRUE)),
      est_row("at_reference", "difference", "ame", "ordinal", FALSE, "MER", "points", "diff",
              crude_shape = "ame", needs = "marginaleffects", obs = FALSE,
              note = est_note_marginal("prob", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "ordinal", TRUE, "RR", "odds_ratio", "or",
              crude_shape = "ame_ratio", comparison = "lnratioavg", needs = "marginaleffects",
              obs = FALSE, note = est_note_marginal("prob", at_ref = TRUE, ratio = TRUE))
    ))
)

# `quasipoisson` is the same estimand grid as `poisson` -- it differs in the VARIANCE assumption, not
# in what it estimates -- so it inherits the rows and only swaps the fitter. Declared, not copied.
REG_ESTIMANDS$quasipoisson <- list(
  default = REG_ESTIMANDS$poisson$default,
  rows = lapply(REG_ESTIMANDS$poisson$rows, function(r) {
    if (identical(r$fit, "poisson")) r$fit <- "quasipoisson"
    r
  }))

# REG_FAMILIES -- WHAT EACH MODEL FAMILY IS CALLED, and where it may be named. Phase 19m-i: FOUR
# name tables and a fifth switch, in two files, which already disagreed -- `reg_family_display_name()`
# (a 9-arm switch of full sentences), `reg_family_short()` (a 9-arm switch of filename tags),
# `REG_FAMILY_UI_LABEL` (a 5-entry vector, silently omitting quasipoisson / rr / rd / mr) and
# `REG_FAMILY_UI_LABEL_BINARY` (2 entries). All four are DERIVED from this one table now, and adding
# a family is one row.
#
# The columns:
#   display    a CLOSURE -- gettext() at render (so the footer follows options(tabxplor.lang)) while
#              staying statically extractable by potools. The CI_METHOD_LABELS precedent.
#   short      the filename tag (Excel sheet names).
#   ui         the PICKER label, or NA. ⚠ `NA` IS THE FACT "not offered in the picker": it is what
#              dev/generate_jamovi_js.R used to write a second time as a hardcoded
#              `setdiff(names(REG_ESTIMANDS), "quasipoisson")`.
#   ui_binary  the picker label OVERRIDE on a 2-level outcome, where family = "poisson" is not a
#              count model: R resolves it to the modified Poisson (Zou 2004), whose exp(coef) is a
#              RISK ratio (18z3). Same stored value, different words -- so the dropdown never says
#              "counts" beside a yes/no variable.
#   outcome    NA on a user family; on an internal LINK key (rr / rd / mr), the OUTCOME family it
#              belongs to. REG_FIT_FAMILY is derived from this column.
#   outcome_level  WHAT `outcome_level = c(<outcome> = "<level>")` MEANS FOR THIS FAMILY, and the one
#              non-uniformity in the argument -- forced by arithmetic, not by taste, so it is
#              declared once here rather than written twice in prose:
#                "modelled"  with TWO levels, singling one out IS choosing what is estimated (the
#                            other becomes the baseline automatically), and the chosen level is the
#                            column header. binomial.
#                "baseline"  with k > 2 you can only choose the PIVOT, so the named level is the
#                            category every other column is compared to -- the opposite role.
#                            multinomial.
#                NA          the outcome has no level to choose. `why` says which kind of "no":
#                            an ordinal outcome HAS levels but must keep their order, and a numeric
#                            one has none at all. NA + a `why` closure IS the refusal.
#   why        the reason `outcome_level` is refused, a gettext closure, or NULL for the generic
#              "this family models no level" message.
#
# ORDER IS LOAD-BEARING: `ui` is emitted into the generated jamovi JS in declaration order, and the
# reader defaults ("regression" / "reg") replace the switches' own fall-through arms.
#' @keywords internal
#' @noRd
REG_FAMILIES <- list(
  gaussian     = list(display = function() gettext("linear regression"),
                      short = "linear",   ui = "gaussian (linear)",    ui_binary = NA_character_,
                      outcome = NA_character_, outcome_level = NA_character_),
  binomial     = list(display = function() gettext("logistic regression"),
                      short = "logit",    ui = "binomial (logistic)",  ui_binary = "binomial (logistic)",
                      outcome = NA_character_, outcome_level = "modelled"),
  poisson      = list(display = function() gettext("Poisson regression"),
                      short = "poisson",  ui = "poisson (counts)",     ui_binary = "poisson (risk ratio)",
                      outcome = NA_character_, outcome_level = NA_character_),
  multinomial  = list(display = function() gettext("multinomial logistic regression"),
                      short = "mlogit",   ui = "multinomial (nominal)", ui_binary = NA_character_,
                      outcome = NA_character_, outcome_level = "baseline"),
  ordinal      = list(display = function() gettext("ordinal logistic regression"),
                      short = "ologit",   ui = "ordinal (ordered)",    ui_binary = NA_character_,
                      outcome = NA_character_, outcome_level = NA_character_,
                      why = function() gettext(
                        "an ordinal outcome must keep the order of its levels, so none can be singled out")),
  # a USER family the picker does not offer (the checkbox route is `family = "poisson"` + a
  # dispersion warning): `ui = NA` says so once, where the label lives.
  quasipoisson = list(display = function() gettext("quasi-Poisson regression"),
                      short = "qpoisson", ui = NA_character_,          ui_binary = NA_character_,
                      outcome = NA_character_),
  # the three internal LINK keys: never named by a user, never offered by a picker.
  rr           = list(display = function() gettext("modified Poisson regression"),
                      short = "rr",       ui = NA_character_,          ui_binary = NA_character_,
                      outcome = "binomial"),
  rd           = list(display = function()
                        gettext("additive-risk regression (identity link, robust standard errors)"),
                      short = "rd",       ui = NA_character_,          ui_binary = NA_character_,
                      outcome = "binomial"),
  mr           = list(display = function()
                        gettext("log-link mean regression (Poisson pseudo-likelihood, robust standard errors)"),
                      short = "mr",       ui = NA_character_,          ui_binary = NA_character_,
                      outcome = "gaussian")
)

# The internal family keys a `fit` may name, beside the user-facing ones, and the OUTCOME family each
# belongs to. Every one of them is a LINK chosen to reach a measure, never a distribution the user
# should have to name -- which is why the map exists at all: a consumer holding a column's stored
# `model_family` (the FIT) must be able to ask the library, which is keyed by the outcome.
# Phase 19m-i: DERIVED from REG_FAMILIES$outcome.
#' @keywords internal
#' @noRd
REG_FIT_FAMILY <- {
  o <- vapply(REG_FAMILIES, function(r) r$outcome, character(1))
  o[!is.na(o)]
}
#' @keywords internal
#' @noRd
REG_FIT_ONLY_FAMILIES <- names(REG_FIT_FAMILY)

# The four readers. Each keeps its own name and its own default, so no call site moved.
#' @keywords internal
#' @noRd
reg_family_display_name <- function(family) {
  r <- REG_FAMILIES[[family]]
  if (is.null(r)) gettext("regression") else r$display()
}
#' @keywords internal
#' @noRd
reg_family_short <- function(family) REG_FAMILIES[[family]]$short %||% "reg"
# The picker labels, in declaration order -- `ui`/`ui_binary` non-NA IS "offered".
#' @keywords internal
#' @noRd
reg_family_ui_labels <- function(binary = FALSE) {
  f <- if (binary) "ui_binary" else "ui"
  v <- vapply(REG_FAMILIES, function(r) r[[f]], character(1))
  v[!is.na(v)]
}

# THE reader of a STORED outcome level. The per-outcome table and the spec carry it as a character
# with NA for "the family's own default", because a tibble column and a typed record field cannot
# hold NULL; every consumer wants that NA back as NULL, which is what reg_prep_binary() and
# reg_positive_level() take. One name, so the NA <-> NULL boundary is written once.
#' @keywords internal
#' @noRd
reg_outcome_level_of <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) NULL else as.character(x)[[1]]
}

# What `outcome_level =` means for this family: "modelled" | "baseline" | NA (refused). Phase 20c.
#' @keywords internal
#' @noRd
reg_outcome_level_role <- function(family)
  REG_FAMILIES[[family]][["outcome_level"]] %||% NA_character_

# THE refusal, from the declaration: which families do offer it, and why this one does not. One
# message, so the resolver, the abort and the generated `@param` cannot say three different things.
#' @keywords internal
#' @noRd
reg_outcome_level_abort <- function(outcome, family) {
  why  <- REG_FAMILIES[[family]][["why"]]
  offers <- names(REG_FAMILIES)[
    !is.na(vapply(names(REG_FAMILIES), reg_outcome_level_role, character(1)))]
  cli::cli_abort(c(
    "{.arg outcome_level} does not apply to {.val {outcome}} ({reg_family_display_name(family)}).",
    "x" = if (is.null(why)) gettext("this family models no single level of the outcome") else why(),
    "i" = "It is offered for: {.val {offers}}."), call = NULL)
}

# THE per-outcome resolution: `outcome_level = c(<outcome> = "<level>")` -> the level for THIS
# outcome, validated against the family's role and the levels the column actually has. NULL = the
# user said nothing, which is every family's own default (binomial models the FIRST level, a
# multinomial pivots on it).
#' @keywords internal
#' @noRd
reg_resolve_outcome_level <- function(outcome_level, outcome, family, y) {
  lv <- reg_per_outcome(outcome_level, outcome, NULL, NULL)
  if (is.null(lv) || !length(lv) || is.na(lv) || !nzchar(lv)) return(NULL)
  if (is.na(reg_outcome_level_role(family))) reg_outcome_level_abort(outcome, family)
  lv   <- as.character(lv)[[1]]
  have <- reg_outcome_levels(y, outcome)
  if (!lv %in% have)
    cli::cli_abort(c("{.arg outcome_level} {.val {lv}} is not a level of {.val {outcome}}.",
                     "i" = "Levels: {.val {have}}."), call = NULL)
  lv
}

# The levels `outcome_level` may name. A 0/1 numeric outcome has none of its own, so reg_prep_binary()
# labels it "Not <outcome>" / "<outcome>" -- BOTH spellings are accepted here, which is what makes
# the argument work on that path instead of being the silent no-op the old logical was.
#' @keywords internal
#' @noRd
reg_outcome_levels <- function(y, outcome) {
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1)))
    return(c(paste0("Not ", outcome), outcome, "0", "1"))
  levels(forcats::fct_drop(as.factor(y)))
}

# REG_FAMILY_MULT_WORD -- the MULTIPLICATIVE effect word of a fit key: what exp(coef) is CALLED for
# this link. OR for a logit, RR for the modified Poisson, IRR for a (quasi-)Poisson rate, RoM for the
# log-link mean model; NA where the family has no exponentiated coefficient estimand at all
# (gaussian, and `rd`, whose coefficients are risk DIFFERENCES).
#
# Phase 19m-i: DERIVED, not declared -- it is the `word` of the family's own exponentiated
# coefficient row, which REG_ESTIMANDS already states. It replaces the residual `switch(fam, ...)`
# inside legend_reg_eff_word() (R/fmt_class.R), whose default silently answered "OR" for any family
# it did not list -- including `rd` and `mr`, added one phase after it was written. The build-time
# assert is what makes the derivation safe: if a family ever grows two exponentiated coefficient
# rows with different words, this fails to LOAD rather than picking one at random.
#' @keywords internal
#' @noRd
REG_FAMILY_MULT_WORD <- local({
  # ⚠ keyed on the row's `fit`, NOT on the family bucket it is declared under. A binomial outcome
  # holds BOTH the logit row (fit "binomial", word "OR") and the modified-Poisson one (fit "rr",
  # word "RR"); asking "the binomial family's exponentiated coefficient word" is therefore ambiguous,
  # while asking "the fit key's" is not. The consumer holds a `model_family` attribute, which is the
  # FIT -- so the fit is also the right key.
  all_rows <- unlist(lapply(REG_ESTIMANDS, function(fr) fr$rows), recursive = FALSE)
  keys <- unique(c(names(REG_ESTIMANDS), names(REG_FIT_FAMILY)))
  vapply(keys, function(k) {
    w <- unique(vapply(Filter(function(r)
      identical(r$fit, k) && identical(r$effect, "coefficient") && isTRUE(r$exp) &&
        identical(r$status, "ok"), all_rows), function(r) r$word, character(1)))
    stopifnot(length(w) <= 1L)
    if (length(w)) w else NA_character_
  }, character(1))
})
#' @keywords internal
#' @noRd
reg_family_mult_word <- function(family) {
  w <- REG_FAMILY_MULT_WORD[family]                # `[` not `[[`: an unknown key is NA, not an error
  if (length(w) != 1L) NA_character_ else unname(w)
}

# The PUBLIC family vocabulary -- what `tab_reg(family =)` accepts and what auto-detection may
# return. Phase 19l promoted it out of a local in tab_reg(): it is the complement of
# REG_FIT_ONLY_FAMILIES over the library, and stating that here is what keeps the two in step. The
# internal link keys (rr / rd / mr) are deliberately absent: a user reaches them by naming a MEASURE.
#' @keywords internal
#' @noRd
REG_USER_FAMILIES <- setdiff(names(REG_ESTIMANDS), REG_FIT_ONLY_FAMILIES)

# Build-time integrity: the library can only be wrong at load time, so it is checked there.
# Phase 20a: `scale` LEFT this block. Its target (EST_SCALES) lives in fmt_class.R, so it is a
# cross-table foreign key like every other, and it is declared with them in R/zzz-fact-keys.R --
# together with `fit`, `display`, `crude_fam` and `crude_shape`, which were never checked at all.
# What stays here is this table's SELF-consistency: does it cover its own key set.
local({
  for (fam in names(REG_ESTIMANDS)) {
    fr <- REG_ESTIMANDS[[fam]]
    keys <- vapply(fr$rows, function(r) paste(r$effect, r$measure), character(1))
    stopifnot(
      "every family declares a default measure per contrast" =
        setequal(names(fr$default), REG_EFFECTS_VALUES),
      "every declared default has a row"                     =
        all(paste(names(fr$default), fr$default) %in% keys),
      "no (effect, measure) cell is declared twice" = !anyDuplicated(keys),
      "every row's effect is a declared value"      =
        all(vapply(fr$rows, function(r) r$effect %in% REG_EFFECTS_VALUES, logical(1))),
      "every row's engine is a declared value or auto" =
        all(vapply(fr$rows, function(r)
          (r$engine %||% "auto") %in% c(REG_MARGINAL_ENGINES, "auto"), logical(1))),
      "every impossible row says why"               =
        all(vapply(fr$rows, function(r) r$status == "ok" || is.function(r$why), logical(1))),
      "every buildable row has an estimand phrase"  =
        all(vapply(fr$rows, function(r) r$status != "ok" || is.function(r$note), logical(1)))
    )
  }
})


# --- the resolvers (the ONLY readers) ----------------------------------------------------------------

# Every (effect, measure) cell declared for one family, as a data frame -- the shape the lister, the
# generated help section and the enumerated error message all want.
#' @keywords internal
#' @noRd
reg_estimands_for <- function(family) {
  fr <- REG_ESTIMANDS[[family]]
  if (is.null(fr)) return(NULL)
  fr$rows
}

# The default measure of an outcome family (what `measure = "auto"` means, and what `measure = "log"`
# logs when it is not given a precise spelling).
#' @keywords internal
#' @noRd
reg_default_measure <- function(family, effect = "coefficient") {
  d <- REG_ESTIMANDS[[family]]$default
  if (is.null(d)) return("difference")
  v <- unname(d[effect])
  if (is.na(v)) unname(d[["coefficient"]]) else v
}

# reg_estimand() -- THE row for one (family, effect, measure), or a typed refusal.
# Returns the row with `status = "ok"`, or a list carrying `status` in
# c("impossible", "not_offered", "unknown_family") plus everything the message needs.
#' @keywords internal
#' @noRd
reg_estimand <- function(family, effect = "coefficient", measure = "auto") {
  fr <- REG_ESTIMANDS[[family]]
  if (is.null(fr)) return(list(status = "unknown_family", family = family))
  mk <- if (is.list(measure)) measure else reg_measure_key(measure)
  if (is.null(mk)) return(list(status = "unknown_measure", family = family, measure = measure))
  meas <- mk$measure
  logged <- identical(meas, "log")
  # "log" is the family's default estimand un-exponentiated; a `log_*` spelling pins another base.
  # The default is per CONTRAST: a coefficient's is the family's ratio, a marginal effect's is a
  # difference (the AME everyone means), a multinomial profile's the "vs rest" odds ratio.
  if (logged) meas <- if (nzchar(mk$log_base)) mk$log_base else reg_default_measure(family, effect)
  if (identical(meas, "auto")) meas <- reg_default_measure(family, effect)

  hit <- Filter(function(r) identical(r$effect, effect) && identical(r$measure, meas), fr$rows)
  if (!length(hit)) {
    # a logged spelling of a measure this family cannot fit is a miss on the BASE, said as such
    return(list(status = "not_offered", family = family, effect = effect,
                measure = if (logged) mk$measure else meas, base = meas))
  }
  row <- hit[[1L]]
  if (identical(row$status, "impossible"))
    return(c(row, list(family = family, asked = meas)))
  if (logged) {
    # A LOG is only meaningful over a multiplicative estimand: an additive coefficient already lives
    # on the scale a log would take it to, which is why `exponentiate = FALSE` was a silent no-op on
    # a gaussian outcome. Said, rather than silently answering the difference.
    if (!isTRUE(row$exp))
      return(list(status = "impossible", family = family, effect = effect, measure = "log",
                  why = function() gettext(
                    "this outcome's coefficient is already additive, so there is no ratio to take the log of")))
    # the log twin of a multiplicative row: same fit, same contrast, no exp(), the link-scale ladder.
    lrow <- Filter(function(r) identical(r$effect, effect) && identical(r$measure, "log") &&
                     identical(r$fit, row$fit), fr$rows)
    if (length(lrow)) row <- lrow[[1L]]
    else {
      row$exp     <- FALSE
      row$word    <- "\u03b2"
      row$scale   <- "log_coef"
      row$display <- "coef"
      row$crude_shape <- paste0(row$crude_shape, "_log")
      row$note    <- function() gettext("coefficients on the model's own link scale")
    }
    row$measure <- "log"
  }
  c(row, list(family = family))
}

# The enumerated refusal. It is generated from the table, so it cannot go stale, and it says which of
# the three states it is -- the distinction the user needs and did not have. Every branch ends with
# the line that WOULD work, the standard `reg_detect_family()` and `ref2 = "cumulative"` already set.
#' @keywords internal
#' @noRd
reg_estimand_abort <- function(res, outcome = NULL, arg = "measure") {
  who <- if (is.null(outcome)) "" else cli::format_inline(" for {.val {outcome}}")
  fam <- res$family
  if (identical(res$status, "unknown_family"))
    cli::cli_abort("Unknown {.arg family} {.val {fam}}.")
  if (identical(res$status, "unknown_measure"))
    cli::cli_abort(c("Unknown {.arg measure} {.val {res$measure}}.",
                     "i" = "Valid: {.or {.val {REG_MEASURES_VALUES}}}."))
  offered <- reg_estimand_offer_lines(fam, res$effect)
  if (identical(res$status, "impossible")) {
    cli::cli_abort(c(
      "{.code measure = {.val {res$measure}}} is not defined{who}: {res$why()}.",
      stats::setNames(offered, rep("i", length(offered)))))
  }
  cli::cli_abort(c(
    "tabxplor does not offer {.code effect = {.val {res$effect}}, measure = {.val {res$measure}}}{who}.",
    stats::setNames(offered, rep("i", length(offered)))))
}

# The arguments Phase 19e retired, and the (effect, measure) spelling each becomes. `tab_reg()` was
# never released, so there is nothing to deprecate -- but "unused argument (exponentiate = FALSE)" is
# not a teachable error, so the `...` catches them and the mapping IS the message. This is the idiom
# Phase 19b established for fmt(type =): the error is the documentation, delivered where the mistake
# is made. NOTE the entries are phrases, not code to run: nothing here changes behaviour.
#' @keywords internal
#' @noRd
REG_RETIRED_ARGS <- list(
  exponentiate = function()
    c("i" = "{.code exponentiate = FALSE} is now {.code measure = \"log\"}; {.code TRUE} is the default estimand.",
      "i" = "The measure is now named: {.code measure = \"odds_ratio\" / \"ratio\" / \"difference\" / \"log\"}."),
  at = function()
    c("i" = "{.code at = \"reference\"} is now {.code effect = \"at_reference\"}.",
      "i" = "{.code at = \"average\"} is the default {.code effect = \"marginal\"}."),
  estimate_display = function()
    c("i" = "{.arg estimate_display} is now {.arg display}, which also takes a {.code {\"{or} ({pct})\"}} template."),
  # Phase 20c (KEY 5): three arguments for one concept -- WHAT RIDES THE MODEL-SUMMARY FOOTER --
  # became one. The comparison is a footer key like any other, and the baseline model is the one
  # parameter that key carries.
  compare = function()
    c("i" = "{.code compare = \"baseline\"} is now {.code stats = c(..., \"compare_baseline\")}.",
      "i" = "{.code compare = \"sequential\"} is now {.code stats = c(..., \"compare_sequential\")}.",
      "i" = "The comparison is a footer row like any other, so it is named in {.arg stats}."),
  baseline = function()
    c("i" = "{.arg baseline} is now the VALUE of the comparison key: {.code stats = c(compare_baseline = \"Model 1\")}.",
      "i" = "An index works too: {.code stats = c(compare_baseline = 2)}. Omit it for the first model."),
  # Phase 20c: a 25-character LOGICAL that toggled the outcome's level ORDER became an argument that
  # NAMES the level -- checkable, readable in a sentence, and working on the 0/1 numeric path where
  # the logical was a silent no-op.
  inverse_two_level_factors = function()
    c("i" = '{.arg inverse_two_level_factors} is now {.code outcome_level = c(<outcome> = "<level>")}, which NAMES the level modelled.',
      "i" = "{.code TRUE} was \"model the first level\", which is still the default -- so drop the argument.",
      "i" = '{.code FALSE} was "model the other one": name it, e.g. {.code outcome_level = c(married = "Not married")}.'),
  # Phase 20c (KEY 4): both producers ask "how is this interval computed" with the same word now.
  method = function()
    c("i" = '{.arg method} is now {.arg ci_method}, the named vector {.fn tab} already takes.',
      "i" = '{.code method = "profile"} is {.code ci_method = c(model = "profile")}, or just {.code ci_method = "profile"}.'),
  # Phase 20c (KEY 4): four questions the two producers asked with two words each. `tab_reg()` is
  # unreleased, so these are RENAMES -- the old spelling aborts naming the new one rather than
  # living on as a permanent second vocabulary.
  dependent = function()
    c("i" = "{.arg dependent} is now {.arg outcome}, the word the rest of the package uses.",
      "i" = 'It pairs with {.arg outcome_level}: {.code tab_reg(d, outcome = "married", outcome_level = c(married = "Married"))}.'),
  split_var = function()
    c("i" = "{.arg split_var} is now {.arg tab_vars}, as in {.fn tab} --- one table per group.",
      "i" = "The STORAGE has said so since 19f: a split group is stamped as a `tab_var` on the index column."),
  reference = function()
    c("i" = "{.arg reference} is now {.arg ref}, as in {.fn tab}: {.code ref = c(race = \"White\")}.",
      "i" = "For the level of the OUTCOME, that is {.arg outcome_level} --- `ref` names what you compare AGAINST."),
  exponentiate_ = function() character(0)
)

# reg_retired_args() -- the `...` guard. A retired name gets its mapping; anything else gets the
# ordinary "unknown argument", which R's own message would not have named either.
#' @keywords internal
#' @noRd
reg_retired_args <- function(dots, fn = "tab_reg") {
  if (!length(dots)) return(invisible(NULL))
  nms <- names(dots) %||% rep("", length(dots))
  old <- intersect(nms, names(REG_RETIRED_ARGS))
  if (length(old)) {
    cli::cli_abort(c(
      "{.arg {old[[1]]}} was removed from {.fn {fn}} in tabxplor 2.0.0.",
      REG_RETIRED_ARGS[[old[[1]]]]()))
  }
  # Phase 20c: a DOT-PREFIXED name is internal, never a user argument -- `.fit_cache` (the jamovi
  # live UI's cache environment) rides `...` since it stopped being a documented formal. Same
  # convention as tab()'s `.cache` / `.return_armed` and test_group_cols()' scratch keys.
  nms <- nms[!startsWith(nms, ".")]
  if (!length(nms)) return(invisible(NULL))
  bad <- nms[!nzchar(nms)]
  cli::cli_abort(c("Unknown argument{?s} passed to {.fn {fn}}.",
                   "x" = "{.val {if (length(bad)) 'unnamed' else nms}}."))
}

# The retired `effect` VALUES, mapped to their (effect, measure) pair. Same contract as above: the
# concept behind `ame_ratio` -- a marginal risk ratio -- is fully kept; only its spelling moved.
#' @keywords internal
#' @noRd
REG_RETIRED_EFFECTS <- list(
  ame       = c(effect = "marginal",     measure = ""),
  ame_ratio = c(effect = "marginal",     measure = "ratio"),
  mer       = c(effect = "at_reference", measure = "")
)

# reg_estimand_note() -- the estimand phrase of the "Model:" footer line, plus the one clause that
# depends on the CELL's layout rather than on the estimand: what the parenthetical in the cell is.
# `reg_model_note()`'s six family arms x `do_exp` are the rows' own `note` closures; only this
# suffix, which is genuinely about the rendered cell, stayed code.
#' @keywords internal
#' @noRd
reg_estimand_note <- function(est, obs_in_cell = FALSE) {
  if (is.null(est) || !is.function(est$note)) return("")
  paren <- if (isTRUE(obs_in_cell))
    gettext("; each cell shows the modelled effect vs the reference level and, in parentheses, the observed (crude) one")
  else if (identical(est$builder, "ame") && reg_fam_prob(est$family %||% ""))
    if (identical(est$comparison, "lnratioavg"))
      gettext("; each cell shows the ratio vs the reference level and, in parentheses, the adjusted predicted probability")
    else
      gettext("; each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability")
  else NULL
  paste0(est$note(), paren)
}

# reg_normalize_color() -- Phase 19e (D25). THE `tab_reg(color =)` boundary.
#
# What a regression table's colour can still CHOOSE, now that KEY 2 stores what every column
# estimates, is only "compared to what": `adjustment` (the same effect, unadjusted) and
# `between_groups` (the same effect in another group). Both are exactly the measures whose baseline
# is another column -- `measure_own_ref()` -- so the allow-list is DERIVED from MEASURES rather than
# written here, and a measure added there needs no edit.
#
# Grammar (unchanged, positional c(text, background)):
#   TRUE / NULL / "auto"  the column's own geometry           (the sentinel NA_character_)
#   FALSE / "no"          no colour anywhere
#   "adjustment" / "between_groups" (either channel)
#   c(TRUE, "adjustment") the headline: effect size in the text, adjustment behind it
#' @keywords internal
#' @noRd
reg_normalize_color <- function(color) {
  if (is.null(color) || isTRUE(color))  return(NA_character_)
  if (isFALSE(color))                   return("no")
  out <- vapply(seq_along(color), function(i) {
    v <- color[[i]]
    # WARNING: `c(TRUE, "adjustment")` is COERCED by c() to `c("TRUE", "adjustment")` -- the sentinel
    # arrives as a STRING, so the string spellings are the ones that must be accepted. `"auto"` and
    # `NA` mean the same thing and are equally documented.
    # "the column's own geometry" is a TEXT-channel answer: a background slot has no geometry of its
    # own to fall back on, so an auto there means "no background colour".
    if (isTRUE(v)  || identical(v, "auto") || identical(v, "TRUE") || is.na(v))
      return(if (i == 1L) NA_character_ else "no")
    if (isFALSE(v) || identical(v, "no")   || identical(v, "FALSE") || identical(v, "")) return("no")
    v <- as.character(v)
    key <- measure_key(v)
    if (!is.na(key) && nzchar(key) && measure_own_ref(key)) return(key)
    own <- names(MEASURES)[vapply(names(MEASURES), measure_own_ref, logical(1))]
    cli::cli_abort(c(
      "{.arg color} = {.val {v}} is not a {.fn tab_reg} colour.",
      "i" = paste0("A regression column states what it estimates, so its colour LADDER comes from ",
                   "the column -- change the estimand with {.arg measure} instead."),
      "i" = "What is left to choose is what to compare it TO: {.or {.val {own}}}.",
      "i" = "{.code color = TRUE} colours by the effect itself; {.code c(TRUE, \"adjustment\")} adds the background."))
  }, character(1))
  # a bare "no" in the text slot with a real background measure is the "background only" spelling
  if (length(out) > 1L && all(out == "no")) return("no")
  out
}

# Phase 19m-ii: reg_color_auto_measure() / reg_color_for() -- the auto-colour sentinel, resolved.
# They were two closures inside tab_reg() (`color_auto_measure` / `color_fill` + `color_for`) that had
# to remember, in three extra locals, which slots WERE auto: the body filled `color` in place, so
# `is.na()` stopped being the sentinel one line after it started being it. Keep the normalised spec
# un-mutated and `is.na()` IS the sentinel, always -- which is also what makes them pure functions of
# (spec, estimand row) instead of closures over a mutating frame.
#
# The ladder decides the CONTEXT ("reg_diff" / "reg_ratio"); WHICH measure answers it is MEASURES' own
# `auto_for`, the same table tab()'s two auto passes read (19c). The context comes from the column's
# own stored SCALE -- its declared geometry -- not from a re-reading of `effect` + `exponentiate`,
# which is what made the ladder and the estimand two facts that could disagree (19e).
#' @keywords internal
#' @noRd
reg_color_auto_measure <- function(est) {
  ctx <- if (identical(EST_SCALES[[est$scale]]$geometry, "ratio")) "reg_ratio" else "reg_diff"
  measure_auto(ctx, "text")
}

# A TRUE in the text slot of an explicit two-channel spec is the same "the column's own geometry"
# sentinel as a bare TRUE -- resolved PER DEPENDENT, so a mixed-family table keeps one ladder per
# family. An explicit user measure keeps its own slots; only the auto ones follow the column.
#' @keywords internal
#' @noRd
reg_color_for <- function(color, est) {
  auto <- is.na(color)
  if (!any(auto)) return(color)
  color[auto] <- reg_color_auto_measure(est)
  color
}

# Phase 19m-ii: THE header word of an estimand, given the cell layout it will be rendered in.
# Phase 14v: with an empirical companion, a prob-scale AME/MER cell folds in the model-adjusted
# predicted % as "{diff} ({pct})"; name it in the header ("... AME (adjusted %)") so the parenthetical
# is unambiguous next to the crude "Emp. %". It is the marginal-STANDARDISED predicted probability
# (decisions doc S50, change A/C), hence "adjusted %" not "model %". Prob-scale families only
# (a gaussian/poisson AME is a bare effect).
#
# ⚠ `empirical` is an EXPLICIT formal, and that is the point. It was a closure reading `empirical`
# lazily from tab_reg()'s frame while two later blocks still mutated it (the `adjustment` forcing
# turns it ON, the no-crude-companion degrade turns it OFF), so the eager `eff_word` recorded in
# reg_call could disagree with the lazy one the specs and labels carried. A function of its arguments
# can only be called once the caller has decided.
#' @keywords internal
#' @noRd
reg_eff_word <- function(est, empirical = FALSE) {
  w <- est$word
  if (!identical(est$builder, "coef") && isTRUE(empirical) && reg_fam_prob(est$family))
    w <- paste0(w, " (adjusted %)")
  w
}

# reg_per_outcome() -- THE per-outcome slicer, shared by `family`, `effect` and `measure` (and by the
# multi-outcome recursion, which used to slice `trials` by hand and forward `family` whole -- D6).
# A scalar applies to every outcome; a NAMED vector is keyed by outcome; a positional one is
# aligned to `outcome`. NULL / NA anywhere means "the default".
#' @keywords internal
#' @noRd
reg_per_outcome <- function(x, d, i, default) {
  if (is.null(x)) return(default)
  v <- if (!is.null(names(x)))  { if (d %in% names(x)) x[[d]] else default }
       else if (length(x) == 1L) x[[1L]]
       else if (i <= length(x))  x[[i]]
       else                      default
  if (is.null(v) || (length(v) == 1L && is.na(v))) default else v
}

# reg_effect_key() -- validate ONE `effect` value, naming the new spelling of a retired one. Returns
# list(effect =, measure =) because `ame_ratio` carried a measure inside the contrast: that
# conflation is the disease, so unpicking it here is the whole point.
#' @keywords internal
#' @noRd
reg_effect_key <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(list(effect = "coefficient", measure = ""))
  x <- as.character(x)
  if (x %in% REG_EFFECTS_VALUES) return(list(effect = x, measure = ""))
  old <- REG_RETIRED_EFFECTS[[tolower(x)]]
  if (!is.null(old)) {
    cli::cli_abort(c(
      "{.code effect = {.val {x}}} was removed in tabxplor 2.0.0.",
      "i" = if (nzchar(old[["measure"]]))
        "It is now {.code effect = \"{old[['effect']]}\", measure = \"{old[['measure']]}\"} -- the marginal risk ratio, unchanged."
        else "It is now {.code effect = \"{old[['effect']]}\"}.",
      "i" = "{.arg effect} says WHICH CONTRAST, {.arg measure} says WHICH MEASURE: {.or {.val {REG_EFFECTS_VALUES}}} x {.or {.val {REG_MEASURES_VALUES}}}."))
  }
  cli::cli_abort(c("Unknown {.arg effect} value {.val {x}}.",
                   "i" = "Valid: {.or {.val {REG_EFFECTS_VALUES}}}."))
}

# "here is what this outcome DOES offer" -- one line per legal measure of the asked contrast, plus a
# pointer to the lister. Shared by the abort and by reg_measures().
#' @keywords internal
#' @noRd
reg_estimand_offer_lines <- function(family, effect = NULL) {
  rows <- reg_estimands_for(family)
  if (is.null(rows)) return(character(0))
  ok <- Filter(function(r) identical(r$status, "ok") &&
                 (is.null(effect) || identical(r$effect, effect)), rows)
  if (!length(ok)) ok <- Filter(function(r) identical(r$status, "ok"), rows)
  lines <- vapply(ok, function(r) cli::format_inline(
    "{.code effect = \"{r$effect}\", measure = \"{r$measure}\"} -> {.val {r$word}}"), character(1))
  c(cli::format_inline("A {.val {family}} outcome offers:"), unique(lines),
    cli::format_inline("Call {.fn reg_measures} on your outcome to see this table with its status."))
}


# --- consumer 3: the lister the user can call on their own outcome -----------------------------------

#' What can this outcome be modelled as?
#'
#' Lists every `effect` × `measure` combination [tab_reg()] can build for one outcome, with its
#' status and the column header it would produce. It is the same runtime table the argument
#' validator, the error messages and `?tab_reg`'s own section read, so what it prints is what the
#' function does.
#'
#' Three statuses, and the distinction matters:
#' * **available** — a call would build it;
#' * **not defined** — the quantity is not a thing for this outcome (an odds ratio needs a
#'   probability to take the odds of), whatever anyone implements;
#' * **not offered** — tabxplor does not build it (yet).
#'
#' A fourth state exists only at run time: a link that does not converge on your data. `tab_reg()`
#' says so and, for the risk difference, falls back to the linear probability model.
#'
#' @param data A data frame (or a `survey` design), as for [tab_reg()].
#' @param outcome The outcome column name.
#' @param family The model family. `"auto"` (default) detects it and says so, exactly as
#'   [tab_reg()] does.
#'
#' @return A tibble of `effect`, `measure`, `status`, `header` (the column name it would produce)
#'   and `note` (why, when it is not available), invisibly when printed.
#' @export
#' @examples
#' d <- forcats::gss_cat
#' d$married <- as.integer(d$marital == "Married")
#' reg_measures(d, "married")
reg_measures <- function(data, outcome, family = "auto") {
  svy <- svy_unwrap_data(data, "reg_measures")
  if (!is.null(svy)) data <- svy$data
  fam <- if (identical(family, "auto")) reg_detect_family(data, outcome) else family
  rows <- reg_estimands_for(fam)
  if (is.null(rows)) cli::cli_abort("Unknown {.arg family} {.val {fam}}.")
  grid <- expand.grid(effect = REG_EFFECTS_VALUES, measure = c("odds_ratio", "ratio", "difference"),
                      stringsAsFactors = FALSE)
  out <- purrr::map(seq_len(nrow(grid)), function(i) {
    r <- reg_estimand(fam, grid$effect[[i]], grid$measure[[i]])
    tibble::tibble(
      effect  = grid$effect[[i]],
      measure = grid$measure[[i]],
      status  = switch(r$status, ok = "available", impossible = "not defined", "not offered"),
      header  = if (identical(r$status, "ok")) paste0("Model_", r$word) else NA_character_,
      note    = if (is.function(r$why)) r$why() else NA_character_)
  })
  out <- dplyr::bind_rows(out)
  # `log` is not a peer measure -- it is any multiplicative row un-exponentiated, so it gets one line
  lg <- reg_estimand(fam, "coefficient", "log")
  out <- dplyr::bind_rows(out, tibble::tibble(
    effect = "coefficient", measure = "log",
    status = if (identical(lg$status, "ok")) "available" else "not defined",
    header = if (identical(lg$status, "ok")) paste0("Model_", lg$word) else NA_character_,
    note   = if (is.function(lg$why)) lg$why() else NA_character_))
  cli::cli_inform(c("i" = "{.val {outcome}}: {.code family = \"{fam}\"}.",
                    "i" = 'The default is {.code measure = "{reg_default_measure(fam)}"}.'))
  out
}

# --- consumer 4: the generated `?tab_reg` section ----------------------------------------------------
# Called from a roxygen `@eval` block, so the documentation is rendered FROM the resolver at
# document() time and cannot drift from it. (The jamovi eligibility rule is the same table's fifth
# reader; Phase 19k generates the JS from reg_estimands_for().)
#' @keywords internal
#' @noRd
reg_measures_rd <- function() {
  fams <- setdiff(names(REG_ESTIMANDS), "quasipoisson")
  line <- function(fam) {
    ok <- Filter(function(r) identical(r$status, "ok"), reg_estimands_for(fam))
    it <- vapply(ok, function(r) sprintf("\\code{effect = \"%s\", measure = \"%s\"} (\\code{Model_%s})",
                                         r$effect, r$measure, r$word), character(1))
    paste0("  \\item \\strong{", fam, "} --- ", paste(unique(it), collapse = "; "))
  }
  c("@section Which estimands each outcome offers:",
    "Generated from the package's own resolution table, so it cannot drift from what",
    "\\code{tab_reg()} builds. Call \\code{\\link{reg_measures}()} on your outcome for the same",
    "table with its per-cell status.",
    "\\itemize{", vapply(fams, line, character(1)), "}")
}


# Phase 20a: the "every link key is answerable by the assumption checks" assertion moved to
# R/zzz-fact-keys.R, where every cross-table edge is declared -- and it moved UP a level while it
# went: it is stated on REG_ESTIMANDS' own `fit` rows now, not on the three-entry REG_FIT_FAMILY
# subset, so an estimand whose fit has no diagnostics fails to load whichever family it belongs to
# (the Phase 19l defect, checked at its real grain).

