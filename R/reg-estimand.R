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
#' @keywords internal
#' @noRd
est_row <- function(effect, measure, builder, fit, exp, word, scale, display,
                    crude_fam = "auto", crude_shape = NA_character_, comparison = NA_character_,
                    needs = "", obs = TRUE, status = "ok", why = NULL, note = NULL) {
  list(effect = effect, measure = measure, builder = builder, fit = fit, exp = exp, word = word,
       scale = scale, display = display, crude_fam = crude_fam, crude_shape = crude_shape,
       comparison = comparison, needs = needs, obs = obs, status = status, why = why, note = note)
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

# The internal family keys a `fit` may name, beside the user-facing ones, and the OUTCOME family each
# belongs to. Every one of them is a LINK chosen to reach a measure, never a distribution the user
# should have to name -- which is why the map exists at all: a consumer holding a column's stored
# `model_family` (the FIT) must be able to ask the library, which is keyed by the outcome.
#' @keywords internal
#' @noRd
REG_FIT_FAMILY <- c(rr = "binomial", rd = "binomial", mr = "gaussian")
#' @keywords internal
#' @noRd
REG_FIT_ONLY_FAMILIES <- names(REG_FIT_FAMILY)

# The PUBLIC family vocabulary -- what `tab_reg(family =)` accepts and what auto-detection may
# return. Phase 19l promoted it out of a local in tab_reg(): it is the complement of
# REG_FIT_ONLY_FAMILIES over the library, and stating that here is what keeps the two in step. The
# internal link keys (rr / rd / mr) are deliberately absent: a user reaches them by naming a MEASURE.
#' @keywords internal
#' @noRd
REG_USER_FAMILIES <- setdiff(names(REG_ESTIMANDS), REG_FIT_ONLY_FAMILIES)

# Build-time integrity: the library can only be wrong at load time, so it is checked there.
local({
  scales <- EST_SCALE_KEYS
  for (fam in names(REG_ESTIMANDS)) {
    fr <- REG_ESTIMANDS[[fam]]
    keys <- vapply(fr$rows, function(r) paste(r$effect, r$measure), character(1))
    stopifnot(
      "every family declares a default measure per contrast" =
        setequal(names(fr$default), REG_EFFECTS_VALUES),
      "every declared default has a row"                     =
        all(paste(names(fr$default), fr$default) %in% keys),
      "no (effect, measure) cell is declared twice" = !anyDuplicated(keys),
      "every row's scale is an EST_SCALES key"      =
        all(vapply(fr$rows, function(r) r$scale %in% scales, logical(1))),
      "every row's effect is a declared value"      =
        all(vapply(fr$rows, function(r) r$effect %in% REG_EFFECTS_VALUES, logical(1))),
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
reg_estimand_abort <- function(res, dependent = NULL, arg = "measure") {
  who <- if (is.null(dependent)) "" else cli::format_inline(" for {.val {dependent}}")
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

# reg_per_dep() -- THE per-dependent slicer, shared by `family`, `effect` and `measure` (and by the
# multi-dependent recursion, which used to slice `trials` by hand and forward `family` whole -- D6).
# A scalar applies to every outcome; a NAMED vector is keyed by dependent; a positional one is
# aligned to `dependent`. NULL / NA anywhere means "the default".
#' @keywords internal
#' @noRd
reg_per_dep <- function(x, d, i, default) {
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
#' @param dependent The outcome column name.
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
reg_measures <- function(data, dependent, family = "auto") {
  svy <- svy_unwrap_data(data, "reg_measures")
  if (!is.null(svy)) data <- svy$data
  fam <- if (identical(family, "auto")) reg_detect_family(data, dependent) else family
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
  cli::cli_inform(c("i" = "{.val {dependent}}: {.code family = \"{fam}\"}.",
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
