# PURPOSE: WHAT A REGRESSION COLUMN ESTIMATES -- one declared library answering the user's two
#   questions at once.
#
#     effect  = which CONTRAST   ("coefficient" | "marginal" | "at_reference")
#     measure = which MEASURE    ("odds_ratio" | "ratio" | "difference" | "log")
#
#   One row per (family, effect, measure) names the model to FIT, whether to exponentiate, the
#   header word, the EST_SCALES row stamped on the column, the crude companion that pairs with it,
#   the marginaleffects contrast, and the estimand phrase of the "Model:" footer line.
#
# THE DIVERGENCE THIS FILE ENCODES, never to be re-collapsed. On a crosstab every geometry is a
# function of the SAME sufficient statistics, so tab(color =) asking for one is a SELECTION. On a
# regression a geometry is a different FIT or a different ESTIMATOR -- an odds ratio is a logit fit,
# a conditional risk ratio a log-link one, a risk difference an identity-link one, a marginal risk
# ratio a g-computation over the logit fit. So here it is a MODELLING DECISION and lives in an
# argument: changing `display` must never change the model.
#
# THE VOCABULARY IS SHARED WITH tab(). `measure`'s values ARE EST_SCALES$geometry ("ratio" /
# "difference" / "log"), which is what tab(color =) resolves into as well, so the argument that
# asks, the attribute that stores, the legend that names and the forest-plot axis that draws are one
# vocabulary end to end (R/fmt_class.R, SECTION "the ESTIMATE's scale"):
#
#     the argument names the GEOMETRY; the attribute names the ROW.
#
# THE FOUR STATES, which a user must be able to tell apart. Status "ok" -> build it. Status
# "impossible" -> abort with the row's own `why` (an odds ratio of a continuous outcome is not a
# thing, whatever anyone implements). Status "redundant" -> abort naming the coefficient: where the
# link is collapsible, a marginal contrast IS the coefficient, and one quantity may not have two
# names (stamped by reg_mark_redundant(), DERIVED so a new family cannot forget it). NO ROW -> "not
# offered", and the message ENUMERATES what this outcome does offer, generated from the table
# itself. A fifth exists only at run time: the fit that did not converge (reg_fit()), which names
# the alternatives.
#
# THE HEADER VOCABULARY is here too (REG_WORDS + REG_CONTRASTS). One name per quantity: a header
# names the MEASURE and the CONTRAST is a marker on it, so the varying part of a column name is one
# acronym a reader can look up. The word is COMPOSED (marker o log-wrap o base acronym), never
# declared -- which is what makes it impossible for two estimands to share a header, or for one
# estimand to be named two ways.
#
# FIVE CONSUMERS, ONE TABLE, and never a second hand-written list: the boundary resolver
# (reg_estimand()), the abort (reg_estimand_abort()), the user-callable lister (reg_measures()), the
# generated ?tab_reg sections and jamovi's eligibility rule (reg_estimands_for() / reg_words_rd()),
# and the "Model:" footer line (reg_estimand_note()).
#
# ALSO HERE: REG_FAMILIES -- what each model family is CALLED and where it may be named, so the
# footer sentence, the Excel filename tag and the two jamovi picker labels cannot disagree; `ui = NA`
# IS the fact "not offered in the picker". REG_FIT_FAMILY and REG_FAMILY_MULT_WORD are DERIVED from
# it and from REG_ESTIMANDS, the latter with a build-time singleton assert.
#
# WARNING -- i18n: every user-visible string in these tables (`long`, `why`, `note`, `display`) is a
# BARE MSGID, gettext()'d by its reader at render. A top-level gettext() would evaluate once at load
# and freeze the build locale, making the language switch a no-op.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).


# --- the measure vocabulary --------------------------------------------------------------------------
#
# THREE base geometries + `log`, which is NOT a peer: it is the same fit, un-exponentiated. A precise
# spelling (`log_odds` / `log_risk` / `log_rate`) additionally PINS which base.
# The acronyms are permanent aliases, never deprecated: the argument teaches the concept word
# ("ratio"), the header keeps the discipline's ("RR" / "IRR" / "RoM").
#' @keywords internal
#' @noRd
REG_MEASURE_ALIASES <- c(
  odds_ratio = "odds_ratio", or = "odds_ratio", OR = "odds_ratio", cumOR = "odds_ratio",
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

# REG_CELL_DIGITS -- how many decimals a REGRESSION cell prints, per estimate scale. ONE declaration,
# read by every builder, because the crude and model columns of one comparison must print the same
# quantity to the same precision.
#
# It is the LEVEL's precision, not the estimate's: a token too coarse at 0 raises itself through
# DISPLAY_TOKENS$min_digits.
# WARNING: a crosstab's digits are its own (tab(digits =)); this is the regression side only.
#' @keywords internal
#' @noRd
REG_CELL_DIGITS <- c(odds_ratio = 0L, score_odds_ratio = 2L, pct_ratio = 0L, score_ratio = 1L,
                     mean_ratio = 1L, raw_diff = 1L, mean_diff = 1L, log_coef = 2L, points = 1L)

#' @keywords internal
#' @noRd
reg_cell_digits <- function(scale) {
  d <- REG_CELL_DIGITS[[scale[[1]]]]
  if (is.null(d)) 2L else d
}

# THE SCALE A COLUMN IS STAMPED WITH, the estimand's own -- except for a SUMMED-SCORE outcome
# (`tab_reg(trials =)`), whose multiplicative effect sits on a mean score, not a probability.
# Declared as one map so the model column and its crude twin cannot disagree (reg_same_estimand()
# compares exactly this, and a mismatch withholds `obs`).
#' @keywords internal
#' @noRd
REG_SCALE_GROUPED <- c(odds_ratio = "score_odds_ratio", pct_ratio = "score_ratio",
                       points = "raw_diff")

#' @keywords internal
#' @noRd
reg_scale_of <- function(est, trials = NA) {
  sc <- est$scale
  if (is.null(trials) || length(trials) != 1L || is.na(trials)) return(sc)
  g <- unname(REG_SCALE_GROUPED[sc])
  if (is.na(g)) sc else g
}

# The scale a LOGGED estimand is the log OF, `NA` on any other column. `log_coef` is one row shared
# by every logged measure, so a link-scale column cannot say on its own whether its exponential is an
# odds or a level -- and its baseline row differs by exactly that. Set by reg_estimand()'s logged
# branch; the declared `measure = "log"` COEFFICIENT rows carry no `log_of` and need none (their
# baseline is the fit's own intercept, already on the link scale).
#' @keywords internal
#' @noRd
reg_exp_scale_of <- function(est, trials = NA) {
  if (is.null(est$log_of)) return(NA_character_)
  reg_scale_of(list(scale = est$log_of), trials)
}

# WHICH KIND OF PERCENTAGE a regression cell holds -- derived, never declared: a column's `pct` field
# exists exactly where the scale names `pct` as its level, and is always a ROW percentage there.
#' @keywords internal
#' @noRd
reg_pct_type <- function(scale)
  if (identical(EST_SCALES[[scale[[1]]]]$base_display, "pct")) "row" else "none"

#' @keywords internal
#' @noRd
REG_MEASURES_VALUES <- c("auto", "odds_ratio", "ratio", "difference", "log")
#' @keywords internal
#' @noRd
REG_EFFECTS_VALUES  <- c("coefficient", "marginal", "at_reference")


# --- the header vocabulary ---------------------------------------------------------------------------
#
# The composition rule is in the file header above. REG_WORDS -- the acronyms themselves. Columns:
#   long            the expansion, a CLOSURE so gettext() runs at render (the MEASURES$word pattern:
#                   a top-level gettext() would freeze the build locale) -- read by the footer, by
#                   reg_measures(), by the "what this outcome offers" abort and by ?tab_reg.
#   noncollapsible  adjusting for a covariate moves this measure away from its neutral even with zero
#                   confounding. Read by the `adjustment` legend caveat; set-identical to
#                   reg_estimand_collapsible(), which states the same fact from the build side.
#
# WARNING: every acronym here must also be an accepted `measure` spelling (REG_MEASURE_ALIASES), so
# what a header prints can always be typed back into the argument. A foreign key checks it at load.
#' @keywords internal
#' @noRd
REG_WORDS <- list(
  OR    = list(long = function() gettext("odds ratio"),            noncollapsible = TRUE),
  cumOR = list(long = function() gettext("cumulative odds ratio"), noncollapsible = TRUE),
  RR    = list(long = function() gettext("risk ratio"),            noncollapsible = FALSE),
  RD    = list(long = function() gettext("risk difference"),       noncollapsible = FALSE),
  IRR   = list(long = function() gettext("incidence-rate ratio"),  noncollapsible = FALSE),
  RoM   = list(long = function() gettext("ratio of means"),        noncollapsible = FALSE),
  diff  = list(long = function() gettext("mean difference"),       noncollapsible = FALSE)
)

# REG_CONTRASTS -- how each `effect` marks the measure it rides on. `mark` is a PREFIX on the acronym
# (unmarked = conditional, as the literature reads an unqualified odds ratio), `long` wraps the
# expansion the same way. One row per REG_EFFECTS_VALUES entry, asserted at load.
#
# The markers are prefixes rather than suffixes so the measure stays the last token of every header,
# and they are plain letters so the name remains a syntactic R name -- `t$Model_refRR` works without
# backticks, where an `@` would parse as an S4 slot access and fail with an unrelated message.
#' @keywords internal
#' @noRd
REG_CONTRASTS <- list(
  coefficient  = list(mark = "",    long = function(l) l),
  marginal     = list(mark = "m",   long = function(l) gettextf("marginal %s", l)),
  at_reference = list(mark = "ref", long = function(l) gettextf("%s at the reference profile", l))
)

stopifnot("every contrast declares its marker" = setequal(names(REG_CONTRASTS), REG_EFFECTS_VALUES))

# reg_word() -- THE column header word.
#' @keywords internal
#' @noRd
reg_word <- function(est) {
  if (is.null(est) || is.null(est$word)) return("")
  # the log wraps the whole marked token ("log(refOR)"), while the expansion below logs the measure
  # and marks the contrast around it ("marginal log odds ratio") -- each reads as it is spoken.
  reg_word_logged(paste0(REG_CONTRASTS[[est$effect]]$mark, est$word), est$measure)
}

# reg_word_long() -- THE expansion of that word, in one sentence fragment.
#' @keywords internal
#' @noRd
reg_word_long <- function(est) {
  if (is.null(est) || is.null(est$word)) return("")
  base <- REG_WORDS[[est$word]]$long()
  if (identical(est$measure, "log")) base <- gettextf("log %s", base)
  REG_CONTRASTS[[est$effect]]$long(base)
}

# The log wrapper, shared by the header and by the crude column: `measure = "log"` shows the SAME
# estimand un-exponentiated, so it names what it logs rather than collapsing to one greek letter.
#' @keywords internal
#' @noRd
reg_word_logged <- function(word, measure)
  if (identical(measure, "log")) paste0("log(", word, ")") else word

# The acronym a composed header was built from -- "log(OR)" -> "OR", "mRR" -> "RR".
#' @keywords internal
#' @noRd
reg_word_base <- function(word) {
  if (is.null(word) || length(word) != 1L || is.na(word)) return(NA_character_)
  w <- sub("^log\\((.*)\\)$", "\\1", word)
  for (m in setdiff(vapply(REG_CONTRASTS, function(r) r$mark, character(1)), ""))
    if (startsWith(w, m) && substring(w, nchar(m) + 1L) %in% names(REG_WORDS))
      return(substring(w, nchar(m) + 1L))
  if (w %in% names(REG_WORDS)) w else NA_character_
}

# reg_legend_word() -- the word the COLOUR legend names a column by: the measure, never the contrast.
# ⚠ the marker is deliberately dropped: legend_group_by_body() groups columns by their rendered
# sentence, so a crude "RR" beside a model "mRR" would split the single legend block the
# crude/adjusted merge exists to produce.
#' @keywords internal
#' @noRd
reg_legend_word <- function(est) {
  if (is.null(est) || is.null(est$word) || is.null(REG_WORDS[[est$word]])) return(NA_character_)
  reg_word_logged(est$word, est$measure)
}

# Is this rendered word a non-collapsible measure? reg_estimand_collapsible() states the same fact
# from the build side.
#' @keywords internal
#' @noRd
reg_word_noncollapsible <- function(word) {
  b <- reg_word_base(word)
  !is.na(b) && isTRUE(REG_WORDS[[b]]$noncollapsible)
}


# --- the library -------------------------------------------------------------------------------------
#
# ONE ROW per (family, effect, measure) the package can answer, plus the rows that state why a
# combination CANNOT be answered. Columns:
#
#   builder      which of reg_build()'s three column builders runs: "coef" | "ame" | "vsrest".
#   fit          the internal family key handed to reg_fit(). It is where a geometry becomes a
#                different MODEL: "rr" = modified Poisson (a conditional risk ratio), "rd" =
#                identity link (a risk difference), "mr" = log-link pseudo-ML (a ratio of means).
#   exp          exponentiate the tidy estimate.
#   word         the BASE measure acronym, a key into REG_WORDS. The contrast marker and the log
#                wrapper are composed onto it by reg_word(), never declared -- see "the header
#                vocabulary" above.
#   scale        the EST_SCALES key stamped on the column. Its `est_field` says which fmt
#                field the estimate is written into, so a scale change needs no builder change.
#   crude_fam    which REG_EMPIRICAL block the observed companion comes from; "auto" =
#                reg_crude_key(fit, trials), which is what carries `trials` -> grouped_binomial.
#   crude_shape  which shape row inside it. `reg_crude_shape()`'s dispatch -- including its
#                cross-family borrow (a binary marginal RATIO reuses REG_EMPIRICAL$rr$rr) -- IS
#                these two columns.
#   comparison   the marginaleffects `comparison =` value (NA = the additive default).
#   engine       WHICH ENGINE computes this row's marginal quantities:
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
#                It is ALSO the dependency rule: `marginaleffects` is a hard requirement exactly where
#                this resolves to it, which is why no row declares that package separately. Every
#                other row runs dependency-free, so a table may always populate its marginal
#                quantities (the fallback checks for the package where it actually falls back).
#   obs          may an `obs` (crude) value be attached cell by cell? FALSE at the reference profile,
#                where the model is conditional and the observed columns stay marginal.
#   status       "ok" | "impossible"; ABSENT from the table = "not offered". A fourth value,
#                "redundant", is STAMPED after the table is built (reg_mark_redundant(), below).
#   why          for "impossible" / "redundant": a closure returning the reason (gettext at render,
#                statically
#                extractable -- the MEASURES$word pattern; a top-level gettext() would freeze the
#                build locale).
#   note         a closure returning the QUALIFIER clause of the "Model:" footer line -- what the
#                estimand is measured against, and any assumption worth one phrase. The measure
#                itself is not repeated here: the footer composes "<word> = <long> (<note>)" from
#                REG_WORDS, so the acronym in the header and its expansion are one fact.
#
# Two members exist only on a SYNTHESIZED logged row (reg_estimand()'s `measure = "log_*"` branch,
# where a family declares no explicit log twin): `display`, the token the cell renders, and `log_of`,
# the scale the column is the log OF -- read by reg_exp_scale_of() for the baseline row.
#
# WARNING: the msgids in `why` / `note` are the ones `po/R-fr.po` carries -- do not re-word them in
# passing, or the French footer silently reverts to English.
# `builder` and `engine` are the two closed vocabularies the columns above are keyed on, each
# checked at load via a foreign key (R/zzz-fact-keys.R).
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
est_row <- function(effect, measure, builder, fit, exp, word, scale,
                    crude_fam = "auto", crude_shape = NA_character_, comparison = NA_character_,
                    obs = TRUE, engine = "auto", status = "ok", why = NULL, note = NULL) {
  list(effect = effect, measure = measure, builder = builder, fit = fit, exp = exp, word = word,
       scale = scale, crude_fam = crude_fam, crude_shape = crude_shape,
       comparison = comparison, obs = obs, engine = engine,
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

# The three MARGINAL-row phrases, generated once rather than written per row.
#' @keywords internal
#' @noRd
est_note_marginal <- function(kind, at_ref = FALSE, ratio = FALSE) {
  function() {
    where <- if (at_ref)
      gettext("other predictors held at their reference level / mean")
    else gettext("sample-averaged")
    what <- if (ratio && identical(kind, "prob"))
      gettext("the ratio of adjusted predicted probabilities")
    else if (ratio)
      gettext("the ratio of adjusted predicted values")
    else if (identical(kind, "prob"))
      gettext("on the probability scale, in percentage points")
    else gettext("on the response scale")
    gettextf("%s; %s", what, where)   # the separator is the TRANSLATION's (French: " ; ")
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
      est_row("coefficient", "difference", "coef", "gaussian", FALSE, "diff", "raw_diff",
              crude_shape = "diff",
              note = function() gettext("vs the reference category")),
      # ratio of means: Poisson pseudo-ML, robust SEs (Santos Silva & Tenreyro 2006).
      est_row("coefficient", "ratio", "coef", "mr", TRUE, "RoM", "mean_ratio",
              crude_fam = "mr", crude_shape = "mr",
              note = function() gettext("vs the reference category")),
      est_row("coefficient", "log", "coef", "mr", FALSE, "RoM", "log_coef",
              crude_fam = "mr", crude_shape = "mr_log",
              note = function() gettext("vs the reference category")),
      est_row("coefficient", "odds_ratio", "coef", "gaussian", TRUE, "OR", "odds_ratio",
              status = "impossible",
              why = function() gettext("an odds ratio needs a probability to take the odds of; this outcome is continuous")),
      est_row("marginal", "difference", "ame", "gaussian", FALSE, "diff", "raw_diff",
              crude_shape = "diff",
              note = est_note_marginal("raw")),
      est_row("marginal", "ratio", "ame", "gaussian", TRUE, "RoM", "mean_ratio",
              crude_fam = "mr", crude_shape = "mr", comparison = "lnratioavg",
              note = est_note_marginal("raw", ratio = TRUE)),
      est_row("at_reference", "difference", "ame", "gaussian", FALSE, "diff", "raw_diff",
              crude_shape = "diff", obs = FALSE,
              note = est_note_marginal("raw", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "gaussian", TRUE, "RoM", "mean_ratio",
              crude_fam = "mr", crude_shape = "mr", comparison = "lnratioavg",
              obs = FALSE,
              note = est_note_marginal("raw", at_ref = TRUE, ratio = TRUE))
    )),

  # ---- binomial ---------------------------------------------------------------------------------
  binomial = list(
    default = c(coefficient = "odds_ratio", marginal = "difference", at_reference = "difference"),
    rows = list(
      est_row("coefficient", "odds_ratio", "coef", "binomial", TRUE, "OR", "odds_ratio",
              crude_shape = "or",
              note = function() gettext("vs the reference category")),
      est_row("coefficient", "log", "coef", "binomial", FALSE, "OR", "log_coef",
              crude_shape = "or_log",
              note = function() gettext("vs the reference category")),
      # modified Poisson (Zou 2004): a genuine conditional risk ratio, not derived from the odds ratio.
      est_row("coefficient", "ratio", "coef", "rr", TRUE, "RR", "pct_ratio",
              crude_fam = "rr", crude_shape = "rr",
              note = function() gettext("vs the reference category")),
      est_row("coefficient", "difference", "coef", "rd", FALSE, "RD", "points",
              crude_shape = "ame",
              note = function() gettext("in percentage points, vs the reference category")),
      est_row("marginal", "difference", "ame", "binomial", FALSE, "RD", "points",
              crude_shape = "ame",
              note = est_note_marginal("prob")),
      est_row("marginal", "ratio", "ame", "binomial", TRUE, "RR", "pct_ratio",
              crude_fam = "rr", crude_shape = "rr", comparison = "lnratioavg",
              note = est_note_marginal("prob", ratio = TRUE)),
      est_row("at_reference", "difference", "ame", "binomial", FALSE, "RD", "points",
              crude_shape = "ame", obs = FALSE,
              note = est_note_marginal("prob", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "binomial", TRUE, "RR", "pct_ratio",
              crude_fam = "rr", crude_shape = "rr", comparison = "lnratioavg",
              obs = FALSE,
              note = est_note_marginal("prob", at_ref = TRUE, ratio = TRUE))
    )),

  # ---- poisson / quasipoisson (counts) ----------------------------------------------------------
  poisson = list(
    default = c(coefficient = "ratio", marginal = "difference", at_reference = "difference"),
    rows = list(
      # an incidence-rate ratio IS a ratio of means, so it takes `mean_ratio`, printing like any
      # other ratio of means, in a regression and a crosstab alike.
      est_row("coefficient", "ratio", "coef", "poisson", TRUE, "IRR", "mean_ratio",
              crude_shape = "irr",
              note = function() gettext("vs the reference category")),
      est_row("coefficient", "log", "coef", "poisson", FALSE, "IRR", "log_coef",
              crude_shape = "irr_log",
              note = function() gettext("vs the reference category")),
      est_row("coefficient", "odds_ratio", "coef", "poisson", TRUE, "OR", "odds_ratio",
              status = "impossible",
              why = function() gettext("an odds ratio needs a probability to take the odds of; this outcome is a count")),
      est_row("marginal", "difference", "ame", "poisson", FALSE, "diff", "raw_diff",
              crude_shape = "diff", note = est_note_marginal("raw")),
      # word "IRR" (not "RoM"): a count outcome has ONE ratio acronym.
      est_row("marginal", "ratio", "ame", "poisson", TRUE, "IRR", "mean_ratio",
              crude_shape = "irr", comparison = "lnratioavg",
              note = est_note_marginal("raw", ratio = TRUE)),
      est_row("at_reference", "difference", "ame", "poisson", FALSE, "diff", "raw_diff",
              crude_shape = "diff", obs = FALSE,
              note = est_note_marginal("raw", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "poisson", TRUE, "IRR", "mean_ratio",
              crude_shape = "irr", comparison = "lnratioavg", obs = FALSE,
              note = est_note_marginal("raw", at_ref = TRUE, ratio = TRUE))
    )),

  # ---- multinomial ------------------------------------------------------------------------------
  multinomial = list(
    default = c(coefficient = "odds_ratio", marginal = "difference", at_reference = "odds_ratio"),
    rows = list(
      est_row("coefficient", "odds_ratio", "coef", "multinomial", TRUE, "OR", "odds_ratio",
              crude_shape = "or",
              note = function() gettext("each category vs the reference")),
      est_row("coefficient", "log", "coef", "multinomial", FALSE, "OR", "log_coef",
              crude_shape = "or_log",
              note = function() gettext("each category vs the reference")),
      est_row("marginal", "difference", "ame", "multinomial", FALSE, "RD", "points",
              crude_shape = "ame", note = est_note_marginal("prob")),
      est_row("marginal", "ratio", "ame", "multinomial", TRUE, "RR", "pct_ratio",
              crude_shape = "ame_ratio", comparison = "lnratioavg",
              note = est_note_marginal("prob", ratio = TRUE)),
      # the one row whose BUILDER is "vsrest": at the reference profile the coefficient becomes the
      # odds ratio of each category versus the rest.
      est_row("at_reference", "odds_ratio", "vsrest", "multinomial", TRUE, "OR", "odds_ratio",
              crude_shape = "or", comparison = "lnor", obs = FALSE,
              note = function() gettext("each outcome category versus the rest; other predictors held at their reference level / mean; profile-conditional")),
      est_row("at_reference", "difference", "ame", "multinomial", FALSE, "RD", "points",
              crude_shape = "ame", obs = FALSE,
              note = est_note_marginal("prob", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "multinomial", TRUE, "RR", "pct_ratio",
              crude_shape = "ame_ratio", comparison = "lnratioavg",
              obs = FALSE, note = est_note_marginal("prob", at_ref = TRUE, ratio = TRUE))
    )),

  # ---- ordinal ----------------------------------------------------------------------------------
  ordinal = list(
    default = c(coefficient = "odds_ratio", marginal = "difference", at_reference = "difference"),
    rows = list(
      est_row("coefficient", "odds_ratio", "coef", "ordinal", TRUE, "cumOR", "odds_ratio",
              crude_shape = "cumor",
              note = function() gettext("proportional-odds model")),
      est_row("coefficient", "log", "coef", "ordinal", FALSE, "cumOR", "log_coef",
              crude_shape = "cumor_log",
              note = function() gettext("proportional-odds model")),
      est_row("marginal", "difference", "ame", "ordinal", FALSE, "RD", "points",
              crude_shape = "ame", note = est_note_marginal("prob")),
      est_row("marginal", "ratio", "ame", "ordinal", TRUE, "RR", "pct_ratio",
              crude_shape = "ame_ratio", comparison = "lnratioavg",
              note = est_note_marginal("prob", ratio = TRUE)),
      est_row("at_reference", "difference", "ame", "ordinal", FALSE, "RD", "points",
              crude_shape = "ame", obs = FALSE,
              note = est_note_marginal("prob", at_ref = TRUE)),
      est_row("at_reference", "ratio", "ame", "ordinal", TRUE, "RR", "pct_ratio",
              crude_shape = "ame_ratio", comparison = "lnratioavg",
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

# --- the REDUNDANT cells, derived ---------------------------------------------------------------
# A marginal contrast IS the coefficient wherever the two run on the same fit and target the same
# measure and the link is collapsible: averaging exp(xb) over the sample divides the exposure factor
# out (log link), averaging a constant slope changes nothing (identity). So the sweep would return
# the coefficient under a second header and a second name -- which is what the one-name-per-quantity
# rule forbids. Those cells are REFUSED, with reg_estimand_abort() naming the coefficient call.
#
# DERIVED rather than declared row by row, so a family added later cannot forget the rule; the test
# suite pins the resulting set. It reads only this file's own facts, all defined above.
# ⚠ `at_reference` is NOT redundant: the estimate is the same, but the adjusted prediction beside it
# comes from the column's OWN reference-profile sweep, not the sample-averaged one -- a different
# table. And an odds ratio never qualifies (REG_WORDS' declared `noncollapsible`).
#' @keywords internal
#' @noRd
reg_mark_redundant <- function(tbl) {
  lapply(tbl, function(fr) {
    coefs <- Filter(function(r) identical(r$effect, "coefficient") && identical(r$status, "ok"),
                    fr$rows)
    fr$rows <- lapply(fr$rows, function(r) {
      if (!identical(r$effect, "marginal") || !identical(r$builder, "ame") ||
          !identical(r$status, "ok") || reg_word_noncollapsible(r$word)) return(r)
      same <- any(vapply(coefs, function(c)
        identical(c$fit, r$fit) && identical(c$measure, r$measure), logical(1)))
      if (!same) return(r)
      r$status <- "redundant"
      # the measure names the link, exactly because the pair only matches on the link's own contrast
      r$why    <- if (identical(r$measure, "difference"))
        function() gettext("the identity link is collapsible, so averaging changes nothing")
      else
        function() gettext("the log link is collapsible, so averaging changes nothing")
      r
    })
    fr
  })
}
REG_ESTIMANDS <- reg_mark_redundant(REG_ESTIMANDS)

# REG_FAMILIES -- WHAT EACH MODEL FAMILY IS CALLED, and where it may be named. Every other name
# table (reg_family_display_name(), reg_family_short(), the UI labels) is DERIVED from this one, so
# adding a family is one row.
#
# The columns:
#   display    a CLOSURE -- gettext() at render (so the footer follows options(tabxplor.lang)) while
#              staying statically extractable by potools. The CI_METHOD_LABELS precedent.
#   short      the filename tag (Excel sheet names).
#   ui         the PICKER label, or NA. ⚠ `NA` IS THE FACT "not offered in the picker".
#   ui_binary  the picker label OVERRIDE on a 2-level outcome, where family = "poisson" is not a
#              count model: R resolves it to the modified Poisson (Zou 2004), whose exp(coef) is a
#              RISK ratio. Same stored value, different words -- so the dropdown never says "counts"
#              beside a yes/no variable.
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
# ORDER IS LOAD-BEARING: `ui` is emitted into the generated jamovi JS in declaration order.
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

# REG_FIT_FAMILY -- the internal LINK keys, DERIVED from REG_FAMILIES$outcome (see `outcome` above).
#' @keywords internal
#' @noRd
REG_FIT_FAMILY <- {
  o <- vapply(REG_FAMILIES, function(r) r$outcome, character(1))
  o[!is.na(o)]
}
#' @keywords internal
#' @noRd
REG_FIT_ONLY_FAMILIES <- names(REG_FIT_FAMILY)

#' @keywords internal
#' @noRd
reg_family_display_name <- function(family) {
  r <- REG_FAMILIES[[family]]
  if (is.null(r)) gettext("regression") else r$display()
}
#' @keywords internal
#' @noRd
reg_family_short <- function(family) REG_FAMILIES[[family]]$short %||% "reg"
#' @keywords internal
#' @noRd
reg_family_ui_labels <- function(binary = FALSE) {
  f <- if (binary) "ui_binary" else "ui"
  v <- vapply(REG_FAMILIES, function(r) r[[f]], character(1))
  v[!is.na(v)]
}

# THE reader of a STORED outcome level: NA (not NULL -- a tibble column can't hold NULL) means "the
# family's own default"; reg_prep_binary() and reg_positive_level() want that NA back as NULL.
#' @keywords internal
#' @noRd
reg_outcome_level_of <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) NULL else as.character(x)[[1]]
}

# What `outcome_level =` means for this family: "modelled" | "baseline" | NA (refused).
#' @keywords internal
#' @noRd
reg_outcome_level_role <- function(family)
  REG_FAMILIES[[family]][["outcome_level"]] %||% NA_character_

# THE refusal, generated from the declaration, so the resolver / abort / @param cannot disagree.
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
# outcome. NULL = the user said nothing, which is every family's own default.
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
# labels it "Not <outcome>" / "<outcome>" -- BOTH spellings are accepted here.
#' @keywords internal
#' @noRd
reg_outcome_levels <- function(y, outcome) {
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1)))
    return(c(paste0("Not ", outcome), outcome, "0", "1"))
  levels(forcats::fct_drop(as.factor(y)))
}

# REG_FAMILY_MULT_WORD -- the MULTIPLICATIVE effect word of a fit key: what exp(coef) is CALLED for
# this link (OR / RR / IRR / RoM; NA where there is no exponentiated coefficient estimand).
#
# DERIVED from REG_ESTIMANDS' own exponentiated coefficient row: an ambiguous or missing row fails to
# LOAD (a build-time assert) rather than silently defaulting to "OR".
#' @keywords internal
#' @noRd
REG_FAMILY_MULT_WORD <- local({
  # ⚠ keyed on the row's `fit`, NOT on the family bucket it is declared under: a binomial outcome
  # holds BOTH the logit row (word "OR") and the modified-Poisson one (word "RR"), so "the binomial
  # family's word" is ambiguous where "the fit key's" is not.
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

# The PUBLIC family vocabulary -- what `tab_reg(family =)` accepts: the complement of
# REG_FIT_ONLY_FAMILIES. The internal link keys (rr / rd / mr) are reached by naming a MEASURE.
#' @keywords internal
#' @noRd
REG_USER_FAMILIES <- setdiff(names(REG_ESTIMANDS), REG_FIT_ONLY_FAMILIES)

# Build-time integrity: checked here is this table's own SELF-consistency (does it cover its own
# key set). Cross-table foreign keys against EST_SCALES / reg_fit() / etc. are declared separately,
# in R/zzz-fact-keys.R.
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
        all(vapply(fr$rows, function(r) r$status != "ok" || is.function(r$note), logical(1))),
      # `obs` is withheld exactly at the reference profile -- asserted here so the day an estimand
      # needs `obs = FALSE` for another reason, this line is what must be relaxed, deliberately.
      "obs is withheld exactly at the reference profile" =
        all(vapply(fr$rows, function(r)
          isTRUE(r$obs) == !identical(r$effect, "at_reference"), logical(1)))
    )
  }
})


# --- the resolvers (the ONLY readers) ----------------------------------------------------------------

# One family's rows, in the shape the lister / help / error message all read.
#' @keywords internal
#' @noRd
reg_estimands_for <- function(family) {
  fr <- REG_ESTIMANDS[[family]]
  if (is.null(fr)) return(NULL)
  fr$rows
}

# The default measure of an outcome family: what `measure = "auto"` (or a bare `"log"`) resolves to.
#' @keywords internal
#' @noRd
reg_default_measure <- function(family, effect = "coefficient") {
  d <- REG_ESTIMANDS[[family]]$default
  if (is.null(d)) return("difference")
  v <- unname(d[effect])
  if (is.na(v)) unname(d[["coefficient"]]) else v
}

# reg_estimand() -- THE row for one (family, effect, measure), or a typed refusal (`status` in
# c("impossible", "not_offered", "unknown_family")).
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
  # The default is PER CONTRAST: a coefficient's is the family's ratio, a marginal effect's a
  # difference (the usual AME).
  if (logged) meas <- if (nzchar(mk$log_base)) mk$log_base else reg_default_measure(family, effect)
  if (identical(meas, "auto")) meas <- reg_default_measure(family, effect)

  hit <- Filter(function(r) identical(r$effect, effect) && identical(r$measure, meas), fr$rows)
  if (!length(hit)) {
    # a logged spelling of a measure this family cannot fit is a miss on the BASE, said as such
    return(list(status = "not_offered", family = family, effect = effect,
                measure = if (logged) mk$measure else meas, base = meas))
  }
  row <- hit[[1L]]
  if (row$status %in% c("impossible", "redundant"))
    return(c(row, list(family = family, asked = meas)))
  if (logged) {
    # A LOG is only meaningful over a multiplicative estimand: an additive coefficient already lives
    # on the scale a log would take it to, so this is refused explicitly rather than silently
    # returning the difference.
    if (!isTRUE(row$exp))
      return(list(status = "impossible", family = family, effect = effect, measure = "log",
                  why = function() gettext(
                    "this outcome's coefficient is already additive, so there is no ratio to take the log of")))
    # the log twin of a multiplicative row: same fit, same contrast, no exp(), the link-scale ladder.
    lrow <- Filter(function(r) identical(r$effect, effect) && identical(r$measure, "log") &&
                     identical(r$fit, row$fit), fr$rows)
    if (length(lrow)) row <- lrow[[1L]]
    else {
      # `word` is deliberately KEPT: reg_word() composes the log wrapper onto it, so a pinned
      # `log_risk` on a binomial outcome reads "log(RR)" with nothing to declare here.
      # `log_of` records WHAT is being logged: `log_coef` is one shared row, so it cannot say whether
      # the exponential is an odds (baseline = odds) or a ratio (baseline = level). Read only by
      # reg_exp_scale_of(), for the marginal builders' baseline row.
      row$log_of  <- row$scale
      row$exp     <- FALSE
      row$scale   <- "log_coef"
      row$display <- "coef"
      row$crude_shape <- paste0(row$crude_shape, "_log")
      row$note    <- function() gettext("on the model's own link scale")
    }
    row$measure <- "log"
  }
  c(row, list(family = family))
}

# The enumerated refusal, generated from the table so it cannot go stale: it names which state
# applies, then lists what IS available.
#' @keywords internal
#' @noRd
reg_estimand_abort <- function(res, outcome = NULL) {
  who <- if (is.null(outcome)) "" else cli::format_inline(" for {.val {outcome}}")
  fam <- res$family
  if (identical(res$status, "unknown_family"))
    cli::cli_abort("Unknown {.arg family} {.val {fam}}.")
  if (identical(res$status, "unknown_measure"))
    cli::cli_abort(c("Unknown {.arg measure} {.val {res$measure}}.",
                     "i" = "Valid: {.or {.val {REG_MEASURES_VALUES}}}."))
  # A REDUNDANT cell has exactly one right call, so it gets the pointer rather than a menu.
  if (identical(res$status, "redundant"))
    cli::cli_abort(c(
      "{.code effect = {.val {res$effect}}, measure = {.val {res$measure}}} returns the coefficient itself{who}: {res$why()}.",
      "i" = 'Use {.code effect = "coefficient"} (the default).'))
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

# REG_ASIDE_NOTE -- what a model cell's PARENTHETICAL holds, keyed by the token the display resolves
# to. This is the one clause of the "Model:" line that depends on the LAYOUT rather than on the
# estimand, so it is read off the display itself and cannot claim a prediction where the cell prints
# a difference. `{ci}`, `{n}` and `{n_range}` name themselves and get no clause.
# ⚠ the sentence describes the MODEL cell: on the mirrored crude column the same slot holds the
# OBSERVED level, which its `Obs_` header already says. (Naming an aside in every backend and in the
# legend is Phase 22c-ii's general problem; this is the existing clause made true.)
#' @keywords internal
REG_ASIDE_NOTE <- list(
  obs   = function() gettext("the observed (crude) one"),
  pct   = function() gettext("the adjusted predicted probability"),
  mean  = function() gettext("the adjusted predicted mean"),
  diff  = function() gettext("the same effect as a difference"),
  ratio = function() gettext("the same effect as a ratio"),
  or    = function() gettext("the same effect as an odds ratio"),
  coef  = function() gettext("the coefficient on the model's own link scale"),
  gap   = function() gettext("its distance to the observed effect")
)

# The aside a model cell prints, as a RESOLVED token ("" = none). The scale-relative tokens are
# resolved through the estimand's own scale, which is what makes one map serve every family.
#' @keywords internal
#' @noRd
reg_aside_token <- function(display, scale = NULL) {
  tmpl <- tryCatch(display_resolve(display), error = function(e) NULL)
  if (is.null(tmpl) || !grepl("{", tmpl, fixed = TRUE)) return("")
  seg <- parse_display_template(tmpl)
  if (length(seg$fields) < 2L) return("")
  tok <- fmt_resolve_scale_tokens(seg$fields, EST_SCALES[[scale %||% ""]] %||% list())
  # the same two rules display_write_col() prunes by: an aside already printed as the primary, or one
  # the scale cannot render, is not in the cell -- so the footer must not name it either.
  tok <- setdiff(tok[-seg$primary], c(tok[[seg$primary]], "blank"))
  if (!length(tok)) "" else tok[[1]]
}

# reg_estimand_note() -- the estimand phrase of the "Model:" footer line, plus that layout clause.
#' @keywords internal
#' @noRd
reg_estimand_note <- function(est, aside = "") {
  if (is.null(est) || !is.function(est$note)) return("")
  note  <- if (aside %in% names(REG_ASIDE_NOTE)) REG_ASIDE_NOTE[[aside]] else NULL
  paren <- if (is.null(note)) NULL else
    paste0(gettext("; each cell shows the effect vs the reference level and, in parentheses, "),
           note())
  # "OR = odds ratio (vs the reference category)": the acronym the header prints, its expansion and
  # the qualifier, composed from one declaration each so the three cannot drift apart.
  paste0(reg_word(est), " = ", reg_word_long(est), " (", est$note(), ")", paren)
}

# reg_normalize_color() -- THE `tab_reg(color =)` boundary. What is left to CHOOSE is "compared to
# what": `adjustment` / `between_groups`, the measures whose baseline is another column, so the
# allow-list is DERIVED from MEASURES.
#
# Grammar (positional c(text, background)):
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
    # WARNING: `c(TRUE, "adjustment")` is COERCED by c() to strings, so string spellings must be
    # accepted too; `is.na()` is the sentinel throughout. A background slot has no geometry of its
    # own, so an auto/TRUE there means "no background colour".
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

# reg_color_auto_measure() / reg_color_for() -- the auto-colour sentinel, resolved from the column's
# own stored SCALE, never from re-reading `effect` + `exponentiate`.
#' @keywords internal
#' @noRd
reg_color_auto_measure <- function(est) {
  # THE measure the column's own scale declares (`label_meas`): a coarser "is it multiplicative?"
  # reading would hand a rate-ratio column the odds-ratio measure and an empty `or` field.
  EST_SCALES[[est$scale]]$label_meas
}

# A TRUE in the text slot is the same "column's own geometry" sentinel, resolved PER DEPENDENT so a
# mixed-family table keeps one ladder per family.
#' @keywords internal
#' @noRd
reg_color_for <- function(color, est) {
  auto <- is.na(color)
  if (!any(auto)) return(color)
  color[auto] <- reg_color_auto_measure(est)
  color
}


# reg_per_outcome() -- THE per-outcome slicer, shared by `family`, `effect`, `measure`. Scalar =
# every outcome; named = keyed by outcome; positional = aligned to `outcome`; NULL/NA = default.
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

# reg_effect_key() -- validate ONE `effect` value: WHICH CONTRAST, orthogonal to `measure` (WHICH
# MEASURE). The measure slot stays empty here -- unpicking the two is the whole point.
#' @keywords internal
#' @noRd
reg_effect_key <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(list(effect = "coefficient", measure = ""))
  x <- as.character(x)
  if (x %in% REG_EFFECTS_VALUES) return(list(effect = x, measure = ""))
  cli::cli_abort(c("Unknown {.arg effect} value {.val {x}}.",
                   "i" = "Valid: {.or {.val {REG_EFFECTS_VALUES}}}.",
                   "i" = "{.arg effect} says WHICH CONTRAST, {.arg measure} says WHICH MEASURE: {.or {.val {REG_EFFECTS_VALUES}}} x {.or {.val {REG_MEASURES_VALUES}}}."))
}

# "here is what this outcome DOES offer" -- shared by the abort and by reg_measures().
#' @keywords internal
#' @noRd
reg_estimand_offer_lines <- function(family, effect = NULL) {
  rows <- reg_estimands_for(family)
  if (is.null(rows)) return(character(0))
  ok <- Filter(function(r) identical(r$status, "ok") &&
                 (is.null(effect) || identical(r$effect, effect)), rows)
  if (!length(ok)) ok <- Filter(function(r) identical(r$status, "ok"), rows)
  lines <- vapply(ok, function(r) cli::format_inline(
    "{.code measure = \"{r$measure}\"} -> {.val {reg_word(r)}}, the {reg_word_long(r)}"), character(1))
  head <- if (is.null(effect) || !length(ok) || !identical(ok[[1]]$effect, effect))
    cli::format_inline("A {.val {family}} outcome offers:")
  else cli::format_inline(
    "A {.val {family}} outcome offers, with {.code effect = \"{effect}\"}:")
  c(head, unique(lines),
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
#' A combination that would return the coefficient under another name — a marginal effect where the
#' link is collapsible, as in a linear model — is **not listed**: it is refused, with the coefficient
#' call named. And one state exists only at run time: a link that does not converge on your data.
#' `tab_reg()` says so and, for the risk difference, falls back to the linear probability model.
#'
#' @param data A data frame (or a `survey` design), as for [tab_reg()].
#' @param outcome The outcome column name.
#' @param family The model family. `"auto"` (default) detects it and says so, exactly as
#'   [tab_reg()] does.
#'
#' @return A tibble of `effect`, `measure`, `status`, `header` (the column name it would produce),
#'   `long` (what that header's acronym means) and `note` (why, when it is not available).
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
  # every declared measure, not a hand-picked pair -- "log" is not a peer (it is any multiplicative
  # row un-exponentiated), so it gets its own line below.
  # a REDUNDANT cell is dropped, not shown as a status: it is the coefficient under another name, so
  # listing it would offer two names for one quantity -- which is what the refusal exists to stop.
  row_of <- function(effect, measure) {
    r <- reg_estimand(fam, effect, measure)
    if (identical(r$status, "redundant")) return(NULL)
    tibble::tibble(
      effect = effect, measure = measure,
      status = switch(r$status, ok = "available", impossible = "not defined", "not offered"),
      header = if (identical(r$status, "ok")) paste0("Model_", reg_word(r)) else NA_character_,
      long   = if (identical(r$status, "ok")) reg_word_long(r) else NA_character_,
      note   = if (is.function(r$why)) r$why() else NA_character_)
  }
  grid <- expand.grid(effect = REG_EFFECTS_VALUES,
                      measure = setdiff(REG_MEASURES_VALUES, c("auto", "log")),
                      stringsAsFactors = FALSE)
  out <- dplyr::bind_rows(purrr::map(seq_len(nrow(grid)),
                                     function(i) row_of(grid$effect[[i]], grid$measure[[i]])))
  out <- dplyr::bind_rows(out, row_of("coefficient", "log"))
  cli::cli_inform(c("i" = "{.val {outcome}}: {.code family = \"{fam}\"}.",
                    "i" = 'The default is {.code measure = "{reg_default_measure(fam)}"}.'))
  out
}

# --- consumer 4: the generated `?tab_reg` section ----------------------------------------------------
# Called from a roxygen `@eval` block, so the documentation renders FROM the resolver at document()
# time and cannot drift. (jamovi's eligibility rule is the same table's fifth reader.)
#' @keywords internal
#' @noRd
reg_measures_rd <- function() {
  fams <- setdiff(names(REG_ESTIMANDS), "quasipoisson")
  line <- function(fam) {
    ok <- Filter(function(r) identical(r$status, "ok"), reg_estimands_for(fam))
    it <- vapply(ok, function(r) sprintf("\\code{effect = \"%s\", measure = \"%s\"} (\\code{Model_%s})",
                                         r$effect, r$measure, reg_word(r)), character(1))
    paste0("  \\item \\strong{", fam, "} --- ", paste(unique(it), collapse = "; "))
  }
  c("@section Which estimands each outcome offers:",
    "Generated from the package's own resolution table, so it cannot drift from what",
    "\\code{tab_reg()} builds. Call \\code{\\link{reg_measures}()} on your outcome for the same",
    "table with its per-cell status.",
    "\\itemize{", vapply(fams, line, character(1)), "}")
}

# --- consumer 5: the generated acronym grid ----------------------------------------------------
# Acronym | what it means | which outcome families print it. Generated from REG_WORDS x
# REG_ESTIMANDS so the taught vocabulary cannot drift from the headers the package builds.
#' @keywords internal
#' @noRd
reg_words_rd <- function() {
  rows <- unlist(lapply(setdiff(names(REG_ESTIMANDS), "quasipoisson"),
                        function(f) lapply(reg_estimands_for(f),
                                           function(r) c(f = f, w = r$word, s = r$status))),
                 recursive = FALSE)
  fams_of <- function(w) unique(vapply(Filter(function(r)
    identical(r[["w"]], w) && identical(r[["s"]], "ok"), rows), function(r) r[["f"]], character(1)))
  used <- Filter(function(w) length(fams_of(w)) > 0L, names(REG_WORDS))
  item <- function(w) sprintf("  \\item \\code{%s} --- %s (%s)", w, REG_WORDS[[w]]$long(),
                              paste(fams_of(w), collapse = ", "))
  c("@section The header acronyms:",
    "A column header names the \\strong{measure}; the \\strong{contrast} is a marker on it ---",
    "no marker for a coefficient, \\code{m} for a marginal effect, \\code{ref} at the reference",
    "profile --- and \\code{measure = \"log\"} wraps it (\\code{Model_mRR}, \\code{Model_refRD},",
    "\\code{Model_log(OR)}). The observed companion carries the measure alone (\\code{Obs_RR}).",
    "\\itemize{", vapply(used, item, character(1)), "}")
}


# The "every link key is answerable by the assumption checks" assertion lives in
# R/zzz-fact-keys.R, checked at REG_ESTIMANDS$fit grain so no family's fit is missed.

